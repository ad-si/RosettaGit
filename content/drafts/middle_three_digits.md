+++
title = "Middle three digits"
description = ""
date = 2019-10-18T11:43:46Z
aliases = []
[extra]
id = 12884
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Write a function/procedure/subroutine that is called with an integer value and returns the middle three digits of the integer if possible or a clear indication of an error if this is not possible.

Note: The order of the middle digits should be preserved.

Your function should be tested with the following values; the first line should return valid answers, those of the second line should return clear indications of an error:

```txt

123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345
1, 2, -1, -10, 2002, -2002, 0

```

Show your output on this page.





## Ada



```Ada
with Ada.Text_IO;

procedure Middle_Three_Digits is

   Impossible: exception;

   function Middle_String(I: Integer; Middle_Size: Positive) return String is
      S: constant String := Integer'Image(I);
      First: Natural := S'First;
      Full_Size, Border: Natural;
   begin
      while S(First) not in '0' .. '9' loop -- skip leading blanks and minus
         First := First + 1;
      end loop;
      Full_Size := S'Last-First+1;
      if (Full_Size < Middle_Size) or (Full_Size mod 2 = 0) then
         raise Impossible;
      else
         Border := (Full_Size - Middle_Size)/2;
         return S(First+Border .. First+Border+Middle_Size-1);
      end if;
   end Middle_String;

   Inputs: array(Positive range <>) of Integer :=
     (123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345,
      1, 2, -1, -10, 2002, -2002, 0);
   Error_Message: constant String := "number of digits must be >= 3 and odd";

   package IIO is new Ada.Text_IO.Integer_IO(Integer);

begin
   for I in Inputs'Range loop
      IIO.Put(Inputs(I), Width => 9);
      Ada.Text_IO.Put(": ");
      begin
         Ada.Text_IO.Put(Middle_String(Inputs(I), 3));
      exception
         when Impossible => Ada.Text_IO.Put("****" & Error_Message & "****");
      end;
      Ada.Text_IO.New_Line;
   end loop;

end Middle_Three_Digits;
```


{{out}}

```txt
      123: 123
    12345: 234
  1234567: 345
987654321: 654
    10001: 000
   -10001: 000
     -123: 123
     -100: 100
      100: 100
   -12345: 234
        1: ****number of digits must be >= 3 and odd****
        2: ****number of digits must be >= 3 and odd****
       -1: ****number of digits must be >= 3 and odd****
      -10: ****number of digits must be >= 3 and odd****
     2002: ****number of digits must be >= 3 and odd****
    -2002: ****number of digits must be >= 3 and odd****
        0: ****number of digits must be >= 3 and odd****
```



## Aime


```aime
void
m3(integer i)
{
    text s;

    s = itoa(i);
    if (s[0] == '-') {
        s = delete(s, 0);
    }

    if (~s < 3) {
        v_integer(i);
        v_text(" has not enough digits\n");
    } elif (~s & 1) {
        o_form("/w9/: ~\n", i, cut(s, ~s - 3 >> 1, 3));
    } else {
        v_integer(i);
        v_text(" has an even number of digits\n");
    }
}

void
middle_3(...)
{
    integer i;

    i = 0;
    while (i < count()) {
        m3($i);
        i += 1;
    }
}

integer
main(void)
{
    middle_3(123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100,
             -12345, 1, 2, -1, -10, 2002, -2002, 0);

    return 0;
}
```

{{out}}

```txt
      123: 123
    12345: 234
  1234567: 345
987654321: 654
    10001: 000
   -10001: 000
     -123: 123
     -100: 100
      100: 100
   -12345: 234
1 has not enough digits
2 has not enough digits
-1 has not enough digits
-10 has not enough digits
2002 has an even number of digits
-2002 has an even number of digits
0 has not enough digits
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.6.win32}}

```algol68
# we define a UNION MODE so that our middle 3 digits PROC can #
# return either an integer on success or a error message if   #
# the middle 3 digits couldn't be extracted                   #
MODE EINT = UNION( INT # success value #, STRING # error message # );
# PROC to return the middle 3 digits of an integer.           #
# if this is not possible, an error message is returned       #
# instead                                                     #
PROC middle 3 digits = ( INT number ) EINT:
BEGIN
    # convert the absolute value of the number to a string with the  #
    # minumum possible number characters                             #
    STRING digits     = whole( ABS number, 0 );
    INT    len        = UPB digits;
    IF   len < 3
    THEN
        # number has less than 3 digits #
        # return an error message       #
        "number must have at least three digits"
    ELIF ( len MOD 2 ) = 0
    THEN
        # the number has an even number of digits #
        # return an error message                 #
        "number must have an odd number of digits"
    ELSE
        # the number is suitable for extraction of the middle 3 digits #
        INT first digit pos = 1 + ( ( len - 3 ) OVER 2 );
        # the result is the integer value of the three digits          #
        ( ( ( ABS digits[ first digit pos     ] - ABS "0" ) * 100 )
        + ( ( ABS digits[ first digit pos + 1 ] - ABS "0" ) *  10 )
        + ( ( ABS digits[ first digit pos + 2 ] - ABS "0" )       )
        )
    FI
END; # middle 3 digits #

main: (
    # test the middle 3 digits PROC #
    []INT test values = (    123,  12345, 1234567, 987654321
                        ,  10001, -10001,    -123,      -100
                        ,    100, -12345
                          # the following values should fail #
                        , 1, 2, -1, -10, 2002, -2002, 0
                        );
    FOR test number FROM LWB test values TO UPB test values
    DO
        CASE middle 3 digits( test values[ test number ] )
        IN
        ( INT     success value ):    # got the middle 3 digits #
            printf( ( $ 11z-d, " : ", 3d $
                    , test values[ test number ]
                    , success value
                    )
                  )
            ,
        ( STRING  error message  ):    # got an error message   #
            printf( ( $ 11z-d, " : ", n( UPB error message )a $
                    , test values[ test number ]
                    , error message
                    )
                  )
        ESAC;
        print( ( newline ) )
    OD
)
```

{{out}}

```txt

          123 : 123
        12345 : 234
      1234567 : 345
    987654321 : 654
        10001 : 000
       -10001 : 000
         -123 : 123
         -100 : 100
          100 : 100
       -12345 : 234
            1 : number must have at least three digits
            2 : number must have at least three digits
           -1 : number must have at least three digits
          -10 : number must have at least three digits
         2002 : number must have an odd number of digits
        -2002 : number must have an odd number of digits
            0 : number must have at least three digits

```



## ALGOL W


```algolw
begin
    % record structure that will be used to return the middle 3 digits of a number   %
    % if the middle three digits can't be determined, isOk will be false and message %
    % will contain an error message                                                  %
    % if the middle 3 digts can be determined, middle3 will contain the middle 3     %
    % digits and isOk will be true                                                   %
    record MiddleThreeDigits ( integer middle3; logical isOk; string(80) message );

    % finds the middle3 digits of a number or describes why they can't be found      %
    reference(MiddleThreeDigits) procedure findMiddleThreeDigits ( integer value number ) ;
        begin
            integer n, digitCount, d;

            n          := abs( number );
            % count the number of digits the number has %
            digitCount := if n = 0 then 1 else 0;
            d          := n;
            while d > 0 do begin
                digitCount := digitCount + 1;
                d          := d div 10
            end while_d_gt_0 ;
            if      digitCount < 3        then MiddleThreeDigits( 0, false, "Number must have at least 3 digits"       )
            else if not odd( digitCount ) then MiddleThreeDigits( 0, false, "Number must have an odd number of digits" )
            else begin
                % can find the middle three digits %
                integer m3;
                m3 := n;
                for d := 1 until ( digitCount - 3 ) div 2 do m3 := m3 div 10;
                MiddleThreeDigits( m3 rem 1000, true, "" )
            end
        end findMiddleThreeDigits ;

    % test the findMiddleThreeDigits procedure %
    for n := 123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0 do begin
        reference(MiddleThreeDigits) m3;
        i_w := 10; s_w := 0; % set output formating %
        m3 := findMiddleThreeDigits( n );
        write( n, ": " );
        if not isOk(m3) then writeon( message(m3) )
        else begin
            % as we return the middle three digits as an integer, we must manually 0 pad %
            if middle3(m3) < 100 then writeon( "0" );
            if middle3(m3) <  10 then writeon( "0" );
            writeon( i_w := 1, middle3(m3) )
        end
    end for_n

end.
```

{{out}}

```txt

       123: 123
     12345: 234
   1234567: 345
 987654321: 654
     10001: 000
    -10001: 000
      -123: 123
      -100: 100
       100: 100
    -12345: 234
         1: Number must have at least 3 digits
         2: Number must have at least 3 digits
        -1: Number must have at least 3 digits
       -10: Number must have at least 3 digits
      2002: Number must have an odd number of digits
     -2002: Number must have an odd number of digits
         0: Number must have at least 3 digits

```



## ATS


```ATS

(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
#include
"share/HATS/atspre_staload_libats_ML.hats"
//
(* ****** ****** *)
//
extern
fun
int2digits(x: int): list0(int)
//
(* ****** ****** *)

implement
int2digits(x) =
loop(x, list0_nil) where
{
//
fun
loop
(
 x: int, res: list0(int)
) : list0(int) =
  if x > 0 then loop(x/10, list0_cons(x%10, res)) else res
//
} (* end of [int2digits] *)

(* ****** ****** *)

extern
fun
Middle_three_digits(x: int): void

(* ****** ****** *)

implement
Middle_three_digits
  (x0) = let
//
val x1 =
(
if x0 >= 0 then x0 else ~x0
) : int
//
fun
skip
(
ds: list0(int), k: int
) : list0(int) =
  if k > 0 then skip(ds.tail(), k-1) else ds
//
val ds =
  int2digits(x1)
//
val n0 = length(ds)
//
in
//
if
(n0 <= 2)
then
(
println! ("Middle-three-digits(", x0, "): Too small!")
)
else
(
if
(n0 % 2 = 0)
then
(
println!
(
"Middle-three-digits(", x0, "): Even number of digits!"
)
)
else let
  val ds =
    skip(ds, (n0-3)/2)
  val-list0_cons(d1, ds) = ds
  val-list0_cons(d2, ds) = ds
  val-list0_cons(d3, ds) = ds
in
  println! ("Middle-three-digits(", x0, "): ", d1, d2, d3) 
end // end of [else]
)
//
end // end of [Middle_three_digits]

(* ****** ****** *)

implement
main0() =
{
//
val
thePassing =
g0ofg1
(
$list{int}
(
  123
, 12345
, 1234567
, 987654321
, 10001, ~10001
, ~123, ~100, 100, ~12345
)
)
val
theFailing =
g0ofg1($list{int}(1, 2, ~1, ~10, 2002, ~2002, 0))
//
val () = thePassing.foreach()(lam x => Middle_three_digits(x))
val () = theFailing.foreach()(lam x => Middle_three_digits(x))
//
} (* end of [main0] *)

(* ****** ****** *)

```

{{out}}

```txt
Middle-three-digits(123): 123
Middle-three-digits(12345): 234
Middle-three-digits(1234567): 345
Middle-three-digits(987654321): 654
Middle-three-digits(10001): 000
Middle-three-digits(-10001): 000
Middle-three-digits(-123): 123
Middle-three-digits(-100): 100
Middle-three-digits(100): 100
Middle-three-digits(-12345): 234
Middle-three-digits(1): Too small!
Middle-three-digits(2): Too small!
Middle-three-digits(-1): Too small!
Middle-three-digits(-10): Too small!
Middle-three-digits(2002): Even number of digits!
Middle-three-digits(-2002): Even number of digits!
Middle-three-digits(0): Too small!
```



## AutoHotkey


```AutoHotkey
Numbers:="123,12345,1234567,987654321,10001,-10001,-123,-100,100,-12345,1,2,-1,-10,2002,-2002,0"
Loop, parse, Numbers, `,
{
	if A_LoopField is not number
		log := log . A_LoopField . "`t: Not a valid number`n"
	else if((d:=StrLen(n:=RegExReplace(A_LoopField,"\D")))<3)
		log := log . A_LoopField . "`t: Too short`n"
	else if(!Mod(d,2))
		log := log . A_LoopField . "`t: Not an odd number of digits`n"
	else
		log := log . A_LoopField . "`t: " . SubStr(n,((d-3)//2)+1,3) . "`n"
}
MsgBox % log
```

{{out}}

```txt
123	: 123
12345	: 234
1234567	: 345
987654321	: 654
10001	: 000
-10001	: 000
-123	: 123
-100	: 100
100	: 100
-12345	: 234
1	: Too short
2	: Too short
-1	: Too short
-10	: Too short
2002	: Not an odd number of digits
-2002	: Not an odd number of digits
0	: Too short
```



## AWK


```AWK
#!/bin/awk -f
# use as: awk -f middle_three_digits.awk

BEGIN {
	n = split("123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345 1 2 -1 -10 2002 -2002 0", arr)

	for (i=1; i<=n; i++) {
		if (arr[i] !~ /^-?[0-9]+$/) {
			printf("%10s : invalid input: not a number\n", arr[i])
			continue
		}

		num = arr[i]<0 ? -arr[i]:arr[i]
		len = length(num)

		if (len < 3) {
			printf("%10s : invalid input: too few digits\n", arr[i])
			continue
		}

		if (len % 2 == 0) {
			printf("%10s : invalid input: even number of digits\n", arr[i])
			continue
		}

		printf("%10s : %s\n", arr[i], substr(num, len/2, 3))
	}
}

```

{{out}}

```txt

       123 : 123
     12345 : 234
   1234567 : 345
 987654321 : 654
     10001 : 000
    -10001 : 000
      -123 : 123
      -100 : 100
       100 : 100
    -12345 : 234
         1 : invalid input: too few digits
         2 : invalid input: too few digits
        -1 : invalid input: too few digits
       -10 : invalid input: too few digits
      2002 : invalid input: even number of digits
     -2002 : invalid input: even number of digits
         0 : invalid input: too few digits

```



## BASIC

=
## Applesoft BASIC
=

```ApplesoftBasic
100 DEF FN L(N) = LEN(STR$(INT(ABS(N))))
110 DEF FN N(N) = VAL(MID$(STR$(INT(ABS(N))),(FN L(N)-1)/2,3))
120 DEF FN EVEN(N) = INT(N/2) = N/2
130 FOR I = 1 TO 20
140     READ N
150     PRINT N":",
160     GOSUB 100"MIDDLE THREE DIGITS
170     PRINT R$
180 NEXT
190 END
200 R$ = ""
210 IF FN EVEN(FN L(N)) THEN R$ = "?EVEN,"
220 IF FN L(N) < 3 THEN R$ = R$ + "ONLY " + STR$(FN L(N)) + " DIGIT" + MID$("S",FN L(N) - 1, 1)
230 IF RIGHT$(R$, 1) = "," THEN R$ = LEFT$(R$, LEN(R$) - 1) : RETURN
240 IF LEFT$(R$, 1) = "?" THEN RETURN
250 IF R$ <> "" THEN R$ = "?" + R$ : RETURN
260 R$ = STR$(FN N(N))
270 IF LEN(R$) = 1 THEN R$ = "00" + R$
280 IF LEN(R$) = 2 THEN R$ = "0" + R$
290 RETURN
300 DATA123,12345,1234567,987654321,10001,-10001,-123,-100,100,-12345
310 DATA1,2,-1,-10,2002,-2002,0

```

{{out}}

```txt

123:            123
12345:          234
1234567:        345
987654321:      654
10001:          000
-10001:         000
-123:           123
-100:           100
100:            100
-12345:         234
1:              ?ONLY 1 DIGIT
2:              ?ONLY 1 DIGIT
-1:             ?ONLY 1 DIGIT
-10:            ?EVEN,ONLY 2 DIGITS
2002:           ?EVEN
-2002:          ?EVEN
0:              ?ONLY 1 DIGIT

```


=
## BBC BASIC
=

```bbcbasic>REM 
midthree
FOR i% = 1 TO 17
  READ test%
  PRINT test%; " -> "; FN_middle_three(test%)
NEXT
END
:
DATA 123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345
DATA 1, 2, -1, -10, 2002, -2002, 0
:
DEF FN_middle_three(n%)
LOCAL n$
n$ = STR$ ABS n%
CASE TRUE OF
WHEN LEN n$ < 3
  = "Not enough digits"
WHEN LEN n$ MOD 2 = 0
  = "Even number of digits"
OTHERWISE
  = MID$(n$, LEN n$ / 2, 3)
ENDCASE
```

{{out}}

```txt
       123 -> 123
     12345 -> 234
   1234567 -> 345
 987654321 -> 654
     10001 -> 000
    -10001 -> 000
      -123 -> 123
      -100 -> 100
       100 -> 100
    -12345 -> 234
         1 -> Not enough digits
         2 -> Not enough digits
        -1 -> Not enough digits
       -10 -> Not enough digits
      2002 -> Even number of digits
     -2002 -> Even number of digits
         0 -> Not enough digits
```


=
## FBSL
=

```qbasic
#APPTYPE CONSOLE

DIM numbers AS STRING = "123,12345,1234567,987654321,10001,-10001,-123,-100,100,-12345,1,2,-1,-10,2002,-2002,0"
DIM dict[] = Split(numbers, ",")
DIM num AS INTEGER
DIM num2 AS INTEGER
DIM powered AS INTEGER

FOR DIM i = 0 TO COUNT(dict) - 1
    num2 = dict[i]
    num = ABS(num2)
    IF num < 100 THEN
        display(num2, "is too small")
    ELSE
        FOR DIM j = 9 DOWNTO 1
            powered = 10 ^ j
            IF num >= powered THEN
                IF j MOD 2 = 1 THEN
                    display(num2, "has even number of digits")
                ELSE
                    display(num2, middle3(num, j))
                END IF
                EXIT FOR
            END IF
        NEXT
    END IF
NEXT

PAUSE

FUNCTION display(num, msg)
    PRINT LPAD(num, 11, " "), " --> ", msg
END FUNCTION

FUNCTION middle3(n, pwr)
    DIM power AS INTEGER = (pwr \ 2) - 1
    DIM m AS INTEGER = n
    m = m \ (10 ^ power)
    m = m MOD 1000
    IF m = 0 THEN
        RETURN "000"
    ELSE
        RETURN m
    END IF
END FUNCTION
```

Output

```txt
        123 --> 123
      12345 --> 234
    1234567 --> 345
  987654321 --> 654
      10001 --> 000
     -10001 --> 000
       -123 --> 123
       -100 --> 100
        100 --> 100
     -12345 --> 234
          1 --> is too small
          2 --> is too small
         -1 --> is too small
        -10 --> is too small
       2002 --> has even number of digits
      -2002 --> has even number of digits
          0 --> is too small

Press any key to continue...
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 INPUT PROMPT "Number: ":N
120 PRINT MIDDLE$(N)
130 DEF MIDDLE$(N)
140   LET N$=STR$(ABS(N))
150   IF LEN(N$)<3 THEN LET MIDDLE$="Not enough digits":EXIT DEF
160   IF MOD(LEN(N$),2)=0 THEN LET MIDDLE$="Even number of digits":EXIT DEF
170   LET P=(LEN(N$)-3)/2
180   LET MIDDLE$=N$(P+1:P+3)
190 END DEF
```



=
## PureBasic
=

```purebasic
Procedure.s middleThreeDigits(x.q)
  Protected x$, digitCount

  If x < 0: x = -x: EndIf

  x$ = Str(x)
  digitCount = Len(x$)
  If digitCount < 3
    ProcedureReturn "invalid input: too few digits"
  ElseIf digitCount % 2 = 0
    ProcedureReturn "invalid input: even number of digits"
  EndIf

  ProcedureReturn Mid(x$,digitCount / 2, 3)
EndProcedure

If OpenConsole()
  Define testValues$ = "123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345 1 2 -1 -10 2002 -2002 0"
  
  Define i, value.q, numTests = CountString(testValues$, " ") + 1
  For i = 1 To numTests
    value = Val(StringField(testValues$, i, " "))
    PrintN(RSet(Str(value), 12, " ") + " : " + middleThreeDigits(value))
  Next
  
  Print(#crlf$ + #crlf$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
         123 : 123
       12345 : 234
     1234567 : 345
   987654321 : 654
       10001 : 000
      -10001 : 000
        -123 : 123
        -100 : 100
         100 : 100
      -12345 : 234
           1 : invalid input: too few digits
           2 : invalid input: too few digits
          -1 : invalid input: too few digits
         -10 : invalid input: too few digits
        2002 : invalid input: even number of digits
       -2002 : invalid input: even number of digits
           0 : invalid input: too few digits
```


=
## Run BASIC
=

```runbasic
x$ = "123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0"

while word$(x$,i+1,",") <> ""
 i	= i + 1
 a1$	= trim$(word$(x$,i,","))
 if left$(a1$,1) = "-" then a$ = mid$(a1$,2) else a$ = a1$
 if (len(a$) and 1) = 0 or len(a$) < 3 then 
   print a1$;chr$(9);" length < 3 or is even"
  else
   print mid$(a$,((len(a$)-3)/2)+1,3);" ";a1$
 end if
wend
end
```


```txt
123 123
234 12345
345 1234567
654 987654321
000 10001
000 -10001
123 -123
100 -100
100 100
234 -12345
1	 length < 3 or is even
2	 length < 3 or is even
-1	 length < 3 or is even
-10	 length < 3 or is even
2002	 length < 3 or is even
-2002	 length < 3 or is even
0	 length < 3 or is even
```



## Batch File


```dos
@echo off
setlocal enabledelayedexpansion

	%== Initialization ==%
set "numbers=123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0"

	%== The Main Thing ==%
for %%N in (%numbers%) do (
	call :middle3 %%N
)
echo.
pause
exit /b 0
	%==/The Main Thing ==%

	%== The Procedure ==%
:middle3

	set str=%1
	%== Making sure that str is positive ==%
	if !str! lss 0 set /a str*=-1

	%== Alternative for finding the string length ==%
	%== But this has a limit of 1000 characters ==%
	set leng=0&if not "!str!"=="" for /l %%. in (0,1,1000) do if not "!str:~%%.,1!"=="" set /a leng+=1

	if !leng! lss 3 (
		echo.%~1:		[ERROR] Input too small.
		goto :EOF
	)

	set /a "test2=leng %% 2,trimmer=(leng - 3) / 2"

	if !test2! equ 0 (
		echo.%~1:		[ERROR] Even number of digits.
		goto :EOF
	)

	%== Passed the tests. Now, really find the middle 3 digits... ==%
	if !trimmer! equ 0 (
		echo.%~1:		!str!
	) else (
		echo.%~1:		!str:~%trimmer%,-%trimmer%!
	)
	goto :EOF
	%==/The Procedure ==%
```

{{Out}}

```txt
123:            123
12345:          234
1234567:                345
987654321:              654
10001:          000
-10001:         000
-123:           123
-100:           100
100:            100
-12345:         234
1:              [ERROR] Input too small.
2:              [ERROR] Input too small.
-1:             [ERROR] Input too small.
-10:            [ERROR] Input too small.
2002:           [ERROR] Even number of digits.
-2002:          [ERROR] Even number of digits.
0:              [ERROR] Input too small.

Press any key to continue . . .
```




## Befunge

Reads the integer value from stdin and writes the middle three digits (or an error message) to stdout.


```befunge>
&>:0`2*1-*0>v
v+*86%+55:p00<
>\55+/:00g1+\|
v3_v#*%2\`2::<
->@>0".rorrE"v
2^,+55_,#!>#:<
>/>\#<$#-:#1_v
>_@#<,+55,,,$<
```


{{out}} (multiple runs)

```txt
123
123
12345
234
1234567
345
987654321
654
10001
000
-10001
000
-123
123
-100
100
100
100
-12345
234
1
Error.
2
Error.
-1
Error.
-10
Error.
2002
Error.
-2002
Error.
0
Error.
```



## Bracmat


```bracmat
( ( middle3
  =   x p
    .     @(!arg:? [?p:? [(1/2*!p+-3/2) %?x [(1/2*!p+3/2) ?)
        & !x
      |   !arg
          ( !p:<3&"is too small"
          | "has even number of digits"
          )
  )
&     123 12345 1234567 987654321 10001 -10001 -123 -100 100
      -12345 1 2 -1 -10 2002 -2002 0
  : ?L
& whl'(!L:%?e ?L&out$(middle3$!e))
& 
);

```

Output:

```txt
123
234
345
654
000
000
123
100
100
234
1 is too small
2 is too small
-1 is too small
-10 is too small
2002 has even number of digits
-2002 has even number of digits
0 is too small
```



## Burlesque



```blsq

blsq ) {123 12345 1234567 987654321 -10001 -123}{XX{~-}{L[3.>}w!m]\[}m[uN
123
234
345
654
000
123

```


<tt>m]\[</tt> and <tt>uN</tt> are just for displaying it nicely.


## C

This code is followed by its output.

```c

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// we return a static buffer; caller wants it, caller copies it
char * mid3(int n)
{
	static char buf[32];
	int l;
	sprintf(buf, "%d", n > 0 ? n : -n);
	l = strlen(buf);
	if (l < 3 || !(l & 1)) return 0;
	l = l / 2 - 1;
	buf[l + 3] = 0;
	return buf + l;
}

int main(void)
{
	int x[] = {123, 12345, 1234567, 987654321, 10001, -10001,
		-123, -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0,
		1234567890};

	int i;
	char *m;
	for (i = 0; i < sizeof(x)/sizeof(x[0]); i++) {
		if (!(m = mid3(x[i])))
			m = "error";
		printf("%d: %s\n", x[i], m);
	}
	return 0;
}
```



```txt

123: 123
12345: 234
1234567: 345
987654321: 654
10001: 000
-10001: 000
-123: 123
-100: 100
100: 100
-12345: 234
1: error
2: error
-1: error
-10: error
2002: error
-2002: error
0: error
1234567890: error

```


### Alternative Version
 
This code has been extensively rewritten. The original was purely interactive and had to be invoked each time from the console. It also did not produce the correct answer.

```c

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
 
void midThree(char arg[])
{
    char output[4];
    int midPoint;
    arg[0]=='-'?midPoint = ((strlen(arg) + 1) / 2):midPoint = ((strlen(arg) + 1) / 2) - 1;
    
    if(strlen(arg) < 3)
        {
            printf("Error, %d is too short.\n",atoi(arg));
            return ;
        }
        else if(strlen(arg)  == 4 || strlen(arg) == 3)
        {
            printf("Error, %d has %d digits, no 3 middle digits.\n",atoi(arg),arg[0]=='-'?strlen(arg)-1:strlen(arg));
            return ;
        }
    else{
    for(int i=0; i<3; i++)
    {
        output[i] = arg[(midPoint-1) + i];
    }
    output[3] = '\0';
    printf("The middle three digits of %s are %s.\n",arg,output);
}}
 
int main(int argc, char * argv[])
{
    char input[50];
    int x[] = {123, 12345, 1234567, 987654321, 10001, -10001,
		-123, -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0,
		1234567890},i;
		
    if(argc < 2)
    {
        printf("Usage: %s <integer>\n",argv[0]);
        printf("Examples with preloaded data shown below :\n");
        
        for(i=0;i<18;i++)
        {
	        midThree(itoa(x[i],argv[0],10));
	     }
        return 1;
    }
    else
    {
        sprintf(input,"%d",atoi(argv[1]));
            midThree(argv[1]);
            return 0;
        
 
    }
}
```


{{out}}

```txt

Usage: C:\TurboC++\Disk\TurboC3\BIN\rosmid3_2.exe <integer>
Examples with preloaded data shown below :
Error, 123 has 3 digits, no 3 middle digits.
The middle three digits of 12345 are 234.
The middle three digits of 1234567 are 345.
The middle three digits of 987654321 are 654.
The middle three digits of 10001 are 000.
The middle three digits of -10001 are 000.
Error, -123 has 3 digits, no 3 middle digits.
Error, -100 has 3 digits, no 3 middle digits.
Error, 100 has 3 digits, no 3 middle digits.
The middle three digits of -12345 are 234.
Error, 1 is too short.
Error, 2 is too short.
Error, -1 is too short.
Error, -10 has 2 digits, no 3 middle digits.
Error, 2002 has 4 digits, no 3 middle digits.
Error, -2002 has 4 digits, no 3 middle digits.
Error, 0 is too short.
The middle three digits of 1234567890 are 456.

```


=={{header|C_sharp|C#}}==

```csharp
using System;

namespace RosettaCode
{
    class Program
    {
        static void Main(string[] args)
        {
            string text = Math.Abs(int.Parse(Console.ReadLine())).ToString();
            Console.WriteLine(text.Length < 2 || text.Length % 2 == 0 ? "Error" : text.Substring((text.Length - 3) / 2, 3));
        }
    }
}
```

{{out}}

```txt
123:123
12345:234
1234567:345
987654321:654
10001:000
-10001:000
-123:123
-100:100
100:100
-12345:234

1:Error
2:Error
-1:Error
-10:Error
2002:Error
-2002:Error
0:Error

```



## C++


```cpp>#include <iostream


std::string middleThreeDigits(int n)
{
    auto number = std::to_string(std::abs(n));
    auto length = number.size();

    if (length < 3) {
        return "less than three digits";
    } else if (length % 2 == 0) {
        return "even number of digits";
    } else {
        return number.substr(length / 2 - 1, 3);
    }
}

int main()
{
    auto values {123, 12345, 1234567, 987654321, 10001,
                 -10001, -123, -100, 100, -12345,
                 1, 2, -1, -10, 2002, -2002, 0};

    for (auto&& v : values) {
        std::cout << "middleThreeDigits(" << v << "): " <<
                     middleThreeDigits(v) << "\n";
    }
}

```

{{out}}

```txt
middleThreeDigits(123): 123
middleThreeDigits(12345): 234
middleThreeDigits(1234567): 345
middleThreeDigits(987654321): 654
middleThreeDigits(10001): 000
middleThreeDigits(-10001): 000
middleThreeDigits(-123): 123
middleThreeDigits(-100): 100
middleThreeDigits(100): 100
middleThreeDigits(-12345): 234
middleThreeDigits(1): less than three digits
middleThreeDigits(2): less than three digits
middleThreeDigits(-1): less than three digits
middleThreeDigits(-10): less than three digits
middleThreeDigits(2002): even number of digits
middleThreeDigits(-2002): even number of digits
middleThreeDigits(0): less than three digits
```



## Clojure


```Clojure

(defn middle3 [v]
  (let [no (Math/abs v)
        digits (str no)
        len (count digits)]
    (cond
      (< len 3) :too_short
      (even? len) :no_middle_in_even_no_of_digits
      :else (let [mid (/ len 2)
                  start (- mid 2)]
              (apply str
                     (take 3
                           (nthnext digits start)))))))

(def passes '(123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345))
(def fails '(1 2 -1 -10 2002 -2002 0))


```


{{out}}

```txt

user=> (map middle3 passes)
("123" "234" "345" "654" "000" "000" "123" "100" "100" "234")
user=> (map middle3 fails)
(:too_short :too_short :too_short :too_short :no_middle_in_even_no_of_digits :no_middle_in_even_no_of_digits :too_short)

```



## COBOL


```COBOL
identification division.
program-id. middle3.
environment division.
data division.
working-storage section.
01  num                 pic 9(9). 
    88 num-too-small    values are -99 thru 99.
01  num-disp            pic ---------9.

01  div                 pic 9(9).
01  mod                 pic 9(9).
01  mod-disp            pic 9(3).

01  digit-counter       pic 999.
01  digit-div           pic 9(9).
    88  no-more-digits  value 0.
01  digit-mod           pic 9(9).
    88  is-even         value 0.

01  multiplier          pic 9(9).

01  value-items.
    05  filler  pic s9(9) value 123.
    05  filler  pic s9(9) value 12345.
    05  filler  pic s9(9) value 1234567.
    05  filler  pic s9(9) value 987654321.
    05  filler  pic s9(9) value 10001.
    05  filler  pic s9(9) value -10001.
    05  filler  pic s9(9) value -123.
    05  filler  pic s9(9) value -100.
    05  filler  pic s9(9) value 100.
    05  filler  pic s9(9) value -12345.
    05  filler  pic s9(9) value 1.
    05  filler  pic s9(9) value 2.
    05  filler  pic s9(9) value -1.
    05  filler  pic s9(9) value -10.
    05  filler  pic s9(9) value 2002.
    05  filler  pic s9(9) value -2002.
    05  filler  pic s9(9) value 0.
    
01  value-array redefines value-items.
    05  items   pic s9(9)  occurs 17 times indexed by item.

01  result  pic x(20).

procedure division.
10-main.
    perform with test after varying item from 1 by 1 until items(item) = 0
        move items(item) to num
        move items(item) to num-disp
        perform 20-check
        display num-disp " --> " result
    end-perform.
    stop run.
    
20-check.
    if num-too-small
        move "Number too small" to result
        exit paragraph
    end-if.

    perform 30-count-digits.
    divide digit-counter by 2 giving digit-div remainder digit-mod.
    if is-even
        move "Even number of digits" to result
        exit paragraph
    end-if.
    
    *> if digit-counter is 5, mul by 10
    *> if digit-counter is 7, mul by 100
    *> if digit-counter is 9, mul by 1000
        
    if digit-counter > 3
        compute multiplier rounded = 10 ** (((digit-counter - 5) / 2) + 1) 
        divide num by multiplier giving num
        divide num by 1000 giving div remainder mod
        move mod to mod-disp
    else
        move num to mod-disp
    end-if.
    move mod-disp to result.
    exit paragraph.
    
30-count-digits.
    move zeroes to digit-counter.
    move num to digit-div.
    perform with test before until no-more-digits
        divide digit-div by 10 giving digit-div remainder digit-mod
        add 1 to digit-counter
    end-perform.
    exit paragraph.
```

Output

```txt
       123 --> 123
     12345 --> 234
   1234567 --> 345
 987654321 --> 654
     10001 --> 000
    -10001 --> 000
      -123 --> 123
      -100 --> 100
       100 --> 100
    -12345 --> 234
         1 --> Number too small
         2 --> Number too small
        -1 --> Number too small
       -10 --> Number too small
      2002 --> Even number of digit
     -2002 --> Even number of digit
         0 --> Number too small
```

Optimised version

```cobol
identification division.
program-id. middle3.
environment division.
data division.
working-storage section.
01  value-items.
        05  filler  pic s9(9) value 123.
        05  filler  pic s9(9) value 12345.
        05  filler  pic s9(9) value 1234567.
        05  filler  pic s9(9) value 987654321.
        05  filler  pic s9(9) value 10001.
        05  filler  pic s9(9) value -10001.
        05  filler  pic s9(9) value -123.
        05  filler  pic s9(9) value -100.
        05  filler  pic s9(9) value 100.
        05  filler  pic s9(9) value -12345.
        05  filler  pic s9(9) value 1.
        05  filler  pic s9(9) value 2.
        05  filler  pic s9(9) value -1.
        05  filler  pic s9(9) value -10.
        05  filler  pic s9(9) value 2002.
        05  filler  pic s9(9) value -2002.
        05  filler  pic s9(9) value 0.
 
01 value-array redefines value-items.
        05  items   pic s9(9)  occurs 17 times indexed by item.

01 num      pic 9(9).
01 num-disp pic ---------9.
01 num2     pic 9(9).

01 power    pic 9.
01 power10  pic 9(16).

01 three-digits pic 999.

01 result   pic X(20).

01 flag     pic 9.
    88 done value 1.

procedure division.
01-setup.
    perform 02-outer with test after varying item from 1 by 1 until items(item) = 0.
    stop run.

02-outer.
    move items(item) to num.
    move items(item) to num-disp.
    if num less than 100
        move "too small" to result
    else
        perform 03-inner with test after varying power from 9 by -1 until power = 1 or done
    end-if.
    display num-disp " --> " result.
    exit paragraph.

03-inner.
    move 0 to flag.
    compute power10 = 10 ** power.
    if num >= power10
        move 1 to flag
        if function mod(power,2) = 1
            move "even number digits" to result
        else 
            move num to num2
            compute num2 = num2 / ( 10 ** (( power / 2 ) - 1 ))
            move function mod(num2,1000) to three-digits
            move three-digits to result
        end-if
    end-if.


```

Output

```txt
       123 --> 123
     12345 --> 234
   1234567 --> 345
 987654321 --> 654
     10001 --> 000
    -10001 --> 000
      -123 --> 123
      -100 --> 100
       100 --> 100
    -12345 --> 234
         1 --> too small
         2 --> too small
        -1 --> too small
       -10 --> too small
      2002 --> even number digits
     -2002 --> even number digits
         0 --> too small

```



## Common Lisp


```lisp

(defun mid3 (n)
  (let ((a (abs n))
        (hmd)) ; how many digits
    (labels ((give (fmt &optional x y) (return-from mid3 (format nil fmt x y)))
             (need (x) (give "Need ~a digits, not ~d." x hmd))
             (nbr (n) (give "~3,'0d" n)))
      (when (zerop n) (give "Zero is 1 digit"))
      (setq hmd (truncate (1+ (log a 10))))
      (cond ((< hmd 3) (need "3+"))
            ((= hmd 3) (nbr a))
            ((evenp hmd) (need "odd number of"))
            (t (nbr (mod (truncate a (expt 10 (/ (- hmd 3) 2))) 1000)))))))

```


Test code:


```lisp

(loop as n in '(123 12345 1234567 987654321
                    10001 -10001 -123 -100 100 -12345
                    1 2 -1 -10 2002 -2002 0)
      do (format t "~d:~12t~a~%" n (mid3 n)))

```

{{out}}

```txt
123:        123
12345:      234
1234567:    345
987654321:  654
10001:      000
-10001:     000
-123:       123
-100:       100
100:        100
-12345:     234
1:          Need 3+ digits, not 1.
2:          Need 3+ digits, not 1.
-1:         Need 3+ digits, not 1.
-10:        Need 3+ digits, not 2.
2002:       Need odd number of digits, not 4.
-2002:      Need odd number of digits, not 4.
0:          Zero is 1 digit
```



## D


```d
import std.stdio, std.traits, std.conv;

string middleThreeDigits(T)(in T n) pure nothrow if (isIntegral!T) {
    auto s = n < 0 ? n.text[1 .. $] : n.text;
    auto len = s.length;
    if (len < 3 || len % 2 == 0)
        return "Need odd and >= 3 digits";
    auto mid = len / 2;
    return s[mid - 1 .. mid + 2];
}

void main() {
    immutable passing = [123, 12345, 1234567, 987654321, 10001, -10001,
            -123, -100, 100, -12345, long.min, long.max];
    foreach (immutable n; passing)
        writefln("middleThreeDigits(%s): %s", n, middleThreeDigits(n));

    immutable failing = [1, 2, -1, -10, 2002, -2002, 0,int.min,int.max];
    foreach (immutable n; failing)
        writefln("middleThreeDigits(%s): %s", n, middleThreeDigits(n));
}
```

{{out}}

```txt
middleThreeDigits(123): 123
middleThreeDigits(12345): 234
middleThreeDigits(1234567): 345
middleThreeDigits(987654321): 654
middleThreeDigits(10001): 000
middleThreeDigits(-10001): 000
middleThreeDigits(-123): 123
middleThreeDigits(-100): 100
middleThreeDigits(100): 100
middleThreeDigits(-12345): 234
middleThreeDigits(-9223372036854775808): 368
middleThreeDigits(9223372036854775807): 368
middleThreeDigits(1): Need odd and >= 3 digits
middleThreeDigits(2): Need odd and >= 3 digits
middleThreeDigits(-1): Need odd and >= 3 digits
middleThreeDigits(-10): Need odd and >= 3 digits
middleThreeDigits(2002): Need odd and >= 3 digits
middleThreeDigits(-2002): Need odd and >= 3 digits
middleThreeDigits(0): Need odd and >= 3 digits
middleThreeDigits(-2147483648): Need odd and >= 3 digits
middleThreeDigits(2147483647): Need odd and >= 3 digits
```



### Alternative Version

This longer version gives a stronger typed output, and it tries to be faster avoiding conversions to string.

```d
import std.stdio, std.traits, std.math, std.variant;

/// Returns a string with the error, or the three digits.
Algebraic!(string, char[3]) middleThreeDigits(T)(in T n)
if (isIntegral!T) {
    // Awkward code to face abs(T.min) when T is signed.
    ulong ln;
    static if (isSigned!T) {
        if (n >= 0) {
            ln = n;
        } else {
            if (n == T.min) {
                ln = -(n + 1);
                ln++;
            } else {
                ln = -n;
            }
        }
    } else {
        ln = n;
    }

    if (ln < 100)
        return typeof(return)("n is too short.");
    immutable uint digits = 1 + cast(uint)log10(ln);
    if (digits % 2 == 0)
        return typeof(return)("n must have an odd number of digits.");

    // From the Reddit answer by "millstone".
    int drop = (digits - 3) / 2;
    while (drop-- > 0)
        ln /= 10;

    char[3] result = void;
    result[2] = ln % 10 + '0';
    ln /= 10;
    result[1] = ln % 10 + '0';
    ln /= 10;
    result[0] = ln % 10 + '0';
    return typeof(return)(result);
}

void main() {
    immutable passing = [123, 12345, 1234567, 987654321, 10001,
                         -10001, -123, -100, 100, -12345, -8765432];
    foreach (n; passing) {
        auto mtd = middleThreeDigits(n);
        // A string result means it didn't pass.
        assert(!mtd.peek!string);
        writefln("middleThreeDigits(%d): %s", n, mtd);
    }
    writeln();

    immutable failing = [1, 2, -1, -10, 2002, -2002, 0,
                         15, int.min, int.max];
    foreach (n; failing) {
        auto mtd = middleThreeDigits(n);
        assert(mtd.peek!string);
        writefln("middleThreeDigits(%d): %s", n, mtd);
    }
    writeln();

    immutable long[] passingL = [123, 12345, 1234567, 987654321, 10001,
                                 -10001, -123, -100, 100, -12345,
                                 -8765432, long.min, long.max];
    foreach (n; passingL) {
        auto mtd = middleThreeDigits(n);
        assert(!mtd.peek!string);
        writefln("middleThreeDigits(%d): %s", n, mtd);
    }
    writeln();

    immutable long[] failingL = [1, 2, -1, -10, 2002, -2002, 0, 15];
    foreach (n; failingL) {
        auto mtd = middleThreeDigits(n);
        assert(mtd.peek!string);
        writefln("middleThreeDigits(%d): %s", n, mtd);
    }
    writeln();

    {
        immutable n = short.min;
        auto mtd = middleThreeDigits(n);
        assert(!mtd.peek!string);
        writefln("middleThreeDigits(cast(short)%d): %s", n, mtd);
    }
}
```

{{out}}

```txt
middleThreeDigits(123): 123
middleThreeDigits(12345): 234
middleThreeDigits(1234567): 345
middleThreeDigits(987654321): 654
middleThreeDigits(10001): 000
middleThreeDigits(-10001): 000
middleThreeDigits(-123): 123
middleThreeDigits(-100): 100
middleThreeDigits(100): 100
middleThreeDigits(-12345): 234
middleThreeDigits(-8765432): 654

middleThreeDigits(1): n is too short.
middleThreeDigits(2): n is too short.
middleThreeDigits(-1): n is too short.
middleThreeDigits(-10): n is too short.
middleThreeDigits(2002): n must have an odd number of digits.
middleThreeDigits(-2002): n must have an odd number of digits.
middleThreeDigits(0): n is too short.
middleThreeDigits(15): n is too short.
middleThreeDigits(-2147483648): n must have an odd number of digits.
middleThreeDigits(2147483647): n must have an odd number of digits.

middleThreeDigits(123): 123
middleThreeDigits(12345): 234
middleThreeDigits(1234567): 345
middleThreeDigits(987654321): 654
middleThreeDigits(10001): 000
middleThreeDigits(-10001): 000
middleThreeDigits(-123): 123
middleThreeDigits(-100): 100
middleThreeDigits(100): 100
middleThreeDigits(-12345): 234
middleThreeDigits(-8765432): 654
middleThreeDigits(-9223372036854775808): 368
middleThreeDigits(9223372036854775807): 368

middleThreeDigits(1): n is too short.
middleThreeDigits(2): n is too short.
middleThreeDigits(-1): n is too short.
middleThreeDigits(-10): n is too short.
middleThreeDigits(2002): n must have an odd number of digits.
middleThreeDigits(-2002): n must have an odd number of digits.
middleThreeDigits(0): n is too short.
middleThreeDigits(15): n is too short.

middleThreeDigits(cast(short)-32768): 276
```



## Dart

<lang>
import'dart:math';
  int length(int x)
  {
    int i,y;
  for(i=0;;i++)
  {
    y=pow(10,i);
    if(x%y==x)
      break;
  }
    return i;
  }
 int middle(int x,int l)
  {
 int a=(x/10)-((x%10)/10);
  int b=a%(pow(10,l-2));
  int l2=length(b);
      if(l2==3)
      {
        return b;
      }
    if(l2!=3)
      {
        return middle(b,l2);
      }
  return 0;
  }
   

main()
{
  int x=-100,y;
  if(x<0)
 x=-x;
  int l=length(x);
  if(l.isEven||x<100)
  {print('error');}
    if(l==3)
  {print('$x');}
 
  if(l.isOdd&& x>100)
  {
   y=middle(x,l);
  print('$y');
  }
}

```


## DCL

<lang>$ list = "123,12345,1234567,987654321,10001,-10001,-123,-100,100,-12345,1,2,-1,-10,2002,-2002,0"
$ i = 0
$ loop:
$  number = f$element( i, ",", list )
$  if number .eqs. "," then $ exit
$  abs_number = number - "-"
$  len = f$length( abs_number )
$  if len .lt. 3 .or. .not. len
$  then
$   write sys$output f$fao( "!9SL: ", f$integer( number )), "has no middle three"
$  else
$   write sys$output f$fao( "!9SL: ", f$integer( number )), f$extract( ( len - 3 ) / 2, 3, abs_number )
$  endif
$  i = i + 1
$  goto loop
```

{{out}}

```txt
$ @middle_three_digits
      123: 123
    12345: 234
  1234567: 345
987654321: 654
    10001: 000
   -10001: 000
     -123: 123
     -100: 100
      100: 100
   -12345: 234
        1: has no middle three
        2: has no middle three
       -1: has no middle three
      -10: has no middle three
     2002: has no middle three
    -2002: has no middle three
        0: has no middle three
```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
			-- Test of middle_three_digits.
		local
			test_1, test_2: ARRAY [INTEGER]
		do
			test_1 := <<123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345>>
			test_2 := <<1, 2, -1, -10, 2002, -2002, 0>>
			across
				test_1 as t
			loop
				io.put_string ("The middle three digits of " + t.item.out + " are: %T ")
				io.put_string (middle_three_digits (t.item) + "%N")
			end
			across
				test_2 as t
			loop
				io.put_string ("The middle three digits of " + t.item.out + " are: %T")
				io.put_string (middle_three_digits (t.item) + "%N")
			end
		end

	middle_three_digits (n: INTEGER): STRING
			-- The middle three digits of 'n'.
		local
			k, i: INTEGER
			in: STRING
		do
			create in.make_empty
			in := n.out
			if n < 0 then
				in.prune ('-')
			end
			create Result.make_empty
			if in.count < 3 then
				io.put_string (" Not enough digits. ")
			elseif in.count \\ 2 = 0 then
				io.put_string (" Even number of digits. ")
			else
				i := (in.count - 3) // 2
				from
					k := i + 1
				until
					k > i + 3
				loop
					Result.extend (in.at (k))
					k := k + 1
				end
			end
		ensure
			length_is_three: Result.count = 3 or Result.count = 0
		end

end

```

{{out}}

```txt

The middle three digits of 123 are:    123
The middle three digits of 12345 are:  234
The middle three digits of 1234567 are:    345
The middle three digits of 987654321 are:  654
The middle three digits of 10001 are:  000
The middle three digits of -10001 are: 000
The middle three digits of -123 are:   123
The middle three digits of -100 are:   100
The middle three digits of 100 are:    100
The middle three digits of -12345 are: 234
The middle three digits of 1 are:      Not enough digits.
The middle three digits of 2 are:      Not enough digits.
The middle three digits of -1 are:     Not enough digits.
The middle three digits of -10 are:    Not enough digits.
The middle three digits of 2002 are:   Even number of digits.
The middle three digits of -2002 are:  Even number of digits.
The middle three digits of 0 are:      Not enough digits.


```


## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;
 
middleThreeDigits(int n)
{
    string s := n.Absolute.toString();
    int len := s.Length;
    if(len<3)
    {
        InvalidArgumentException.new:"n must have 3 digits or more".raise()
    }
    else if(len.isEven())
    {
        InvalidArgumentException.new:"n must have an odd number of digits".raise()
    };
 
    int mid := len / 2;
 
    ^ s.Substring(mid-1,3)
}
 
public program()
{
    new int[]::(123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0)
        .forEach:(n)
        {
            console.printLine("middleThreeDigits(",n,"):",middleThreeDigits(n) | on:(e => e.Message))
        }
}
```

{{out}}

```txt

middleThreeDigits(123):123
middleThreeDigits(12345):234
middleThreeDigits(1234567):345
middleThreeDigits(987654321):654
middleThreeDigits(10001):000
middleThreeDigits(-10001):000
middleThreeDigits(-123):123
middleThreeDigits(-100):100
middleThreeDigits(100):100
middleThreeDigits(-12345):234
middleThreeDigits(1):n must have 3 digits or more
middleThreeDigits(2):n must have 3 digits or more
middleThreeDigits(-1):n must have 3 digits or more
middleThreeDigits(-10):n must have 3 digits or more
middleThreeDigits(2002):n must have an odd number of digits
middleThreeDigits(-2002):n must have an odd number of digits
middleThreeDigits(0):n must have 3 digits or more

```



## Elixir


```Elixir
defmodule Middle do
  def three(num) do
    n = num |> abs |> to_string
    
    case {n,String.length(n) > 2,even?(n)} do
      {n, true, false} ->
        cut(n)
      {_, false, _} ->
        raise "Number must have at least three digits"
      {_, _, true} ->
        raise "Number must have an odd number of digits"
    end
  end
  
  defp even?(n), do: rem(String.length(n),2) == 0
  defp cut(n), do: String.slice(n,(div(String.length(n),2) - 1),3)
end

valids = [123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345]
Enum.each(valids, fn n -> :io.format "~10w : ~s~n", [n, Middle.three(n)] end)

errors = [1, 2, -1, -10, 2002, -2002, 0]
Enum.each(errors, fn n ->
  :io.format "~10w : ", [n]
  try do
    IO.puts Middle.three(n)
  rescue
    e -> IO.puts e.message
  end
end)
```


{{out}}

```txt

       123 : 123
     12345 : 234
   1234567 : 345
 987654321 : 654
     10001 : 000
    -10001 : 000
      -123 : 123
      -100 : 100
       100 : 100
    -12345 : 234
         1 : Number must have at least three digits
         2 : Number must have at least three digits
        -1 : Number must have at least three digits
       -10 : Number must have at least three digits
      2002 : Number must have an odd number of digits
     -2002 : Number must have an odd number of digits
         0 : Number must have at least three digits

```



## Erlang


```erlang
% 
-module(middle_three_digits).
-export([main/0]).

main() ->
	digits(123),  
	digits(12345),
	digits(1234567),
	digits(987654321),	
	digits(10001),
	digits(-10001),
	digits(-123),	
	digits(-100),
	digits(100),
	digits(-12345),
	digits(1),
	digits(2),
	digits(-1),
	digits(-10),
	digits(2002),
	digits(-2002),
	digits(0).

digits(N) when N < 0 ->
	digits(-N); 
digits(N) when (N div 100) =:= 0  ->
	io:format("too small\n");
digits(N) ->
	K=length(integer_to_list(N)),
	if (K rem 2) =:= 0 ->
		io:format("even number of digits\n");
	true ->	 
		loop((K-3) div 2 , N)
	end.

loop(0, N) ->
	io:format("~3..0B~n",[N rem 1000]);	
loop(X,N)  when X>0 ->
	loop(X-1, N div 10).

```

{{out}}

```txt
123
234
345
654
000
000
123
100
100
234
too small
too small
too small
too small
even number of digits
even number of digits
too small
ok

```



## ERRE


```ERRE

PROGRAM MIDDLE

!$DOUBLE

FUNCTION LUNG(N)
   LUNG=LEN(STR$(INT(ABS(N))))+1
END FUNCTION

FUNCTION NCNT(N)
    NCNT=VAL(MID$(STR$(INT(ABS(N))),(LUNG(N)-1)/2,3))
END FUNCTION

FUNCTION EVEN(N)
    EVEN=INT(N/2)=N/2
END FUNCTION

PROCEDURE NUMBER_EXAM(N->R$)
    R$=""   LG%=LUNG(N)-2
    IF EVEN(LG%) THEN R$="?EVEN,"  END IF
    IF LG%<3 THEN
             R$=R$+"ONLY"+STR$(LG%)+" DIGIT"
             IF LG%=1 THEN
                 R$=R$+"S"
             END IF
    END IF
    IF RIGHT$(R$,1)="," THEN R$=LEFT$(R$,LEN(R$)-1)   EXIT PROCEDURE END IF
    IF LEFT$(R$,1)="?" THEN EXIT PROCEDURE END IF
    IF R$<>"" THEN R$="?"+R$   EXIT PROCEDURE END IF
    R$=STR$(NCNT(N))
    IF LEFT$(R$,1)=" " THEN R$=MID$(R$,2) END IF
    IF LEN(R$)=1 THEN R$="00"+R$  END IF
    IF LEN(R$)=2 THEN R$="0"+R$   END IF
END PROCEDURE

BEGIN
    DATA(123,12345,1234567,987654321,10001,-10001,-123,-100,100,-12345)
    DATA(1,2,-1,-10,2002,-2002,0)
    FOR I%=1 TO 17 DO
        READ(N)
        PRINT(N;"  ",)
        NUMBER_EXAM(N->R$)
        PRINT(R$)
    END FOR
END PROGRAM

```

{{out}}

```txt

 123          123
 12345        234
 1234567      345
 987654321    654
 10001        000
-10001        000
-123          123
-100          100
 100          100
-12345        234
 1            ?ONLY 1 DIGITS
 2            ?ONLY 1 DIGITS
-1            ?ONLY 1 DIGITS
-10           ?EVEN,ONLY 2 DIGIT
 2002         ?EVEN
-2002         ?EVEN
 0            ?ONLY 1 DIGITS

```



## Factor

<lang>USING: combinators formatting io kernel math math.parser
sequences ;
IN: rosetta-code.middle-three-digits

CONSTANT: test-values {
    123 12345 1234567 987654321 10001 -10001
    -123 -100 100 -12345 1 2 -1 -10 2002 -2002 0
}

: (middle-three) ( str -- str' )
    [ midpoint@ [ 1 - ] [ 2 + ] bi ] [ subseq ] bi ;
    
: too-short ( -- )
    "Number must have at least three digits." print ;
    
: number-even ( -- )
    "Number must have an odd number of digits." print ;

: middle-three ( n -- )
    abs number>string {
        { [ dup length 3 < ] [ drop too-short ] }
        { [ dup length even? ] [ drop number-even ] }
        [ (middle-three) print ]
    } cond ;
    
: main ( -- )
    test-values [ dup "%9d : " printf middle-three ] each ;

MAIN: main
```

{{out}}

```txt

      123 : 123
    12345 : 234
  1234567 : 345
987654321 : 654
    10001 : 000
   -10001 : 000
     -123 : 123
     -100 : 100
      100 : 100
   -12345 : 234
        1 : Number must have at least three digits.
        2 : Number must have at least three digits.
       -1 : Number must have at least three digits.
      -10 : Number must have at least three digits.
     2002 : Number must have an odd number of digits.
    -2002 : Number must have an odd number of digits.
        0 : Number must have at least three digits.

```



## Forth

This is a problem, which is easily solved in Forth. It converts the number to a string and checks it length. If the number does not represent a printable string, it returns an empty string.

```forth
: middle3          ( n1 -- a n2)
  abs s>d <# #s #> dup 2/ 0<> over 1 and 0<> and
  if 2/ 1- chars + 3 else drop 0 then
;
```


## Fortran

Please find compilation instructions along with the output for the examples in the comments at the beginning of the file.  This program was produced in an Ubuntu distribution of the GNU/linux system.

```FORTRAN

!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Sat Jun  1 14:48:41
!
!a=./f && make $a && OMP_NUM_THREADS=2 $a < unixdict.txt # some of the compilation options and redirection from unixdict.txt are vestigial.
!gfortran -std=f2008 -Wall -fopenmp -ffree-form -fall-intrinsics -fimplicit-none f.f08 -o f
!                 123  123                 
!               12345  234                 
!             1234567  345                 
!           987654321  654                 
!               10001  000                 
!              -10001  000                 
!                -123  123                 
!                -100  100                 
!                 100  100                 
!              -12345  234                 
!                   1  Too short           
!                   2  Too short           
!                  -1  Too short           
!                 -10  Too short           
!                2002  Digit count too even
!               -2002  Digit count too even
!                   0  Too short           
!
!Compilation finished at Sat Jun  1 14:48:41


program MiddleMuddle
  integer, dimension(17) :: itest, idigits
  integer :: i, n
  data itest/123,12345,1234567,987654321,10001,-10001,-123,-100,100,-12345,1,2,-1,-10,2002,-2002,0/
  do i = 1, size(itest)
    call antibase(10, abs(itest(i)), idigits, n)
    write(6,'(i20,2x,a20)') itest(i), classifym3(idigits, n)
    if (0 .eq. itest(i)) exit
  end do

contains

  logical function even(n)
    integer, intent(in) :: n
    even = 0 .eq. iand(n,1)
  end function even

  function classifym3(iarray, n) result(s)
    integer, dimension(:), intent(in) :: iarray
    integer, intent(in) :: n
    character(len=20) :: s
    integer :: i,m
    if (n < 3) then
      s = 'Too short'
    else if (even(n)) then
      s = 'Digit count too even'
    else
      m = (n+1)/2
      write(s,'(3i1)')(iarray(i), i=m+1,m-1,-1)
    end if
  end function classifym3

  subroutine antibase(base, m, digits, n) ! digits ordered by increasing significance
    integer, intent(in) :: base, m
    integer, intent(out) :: n  ! the number of digits
    integer, dimension(:), intent(out) :: digits
    integer :: em
    em = m
    do n=1, size(digits)
      digits(n) = mod(em, base)
      em = em / base
      if (0 .eq. em) return
    end do
    stop 'antibase ran out of space to store result'
  end subroutine antibase

end program MiddleMuddle

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function middleThreeDigits (n As Integer) As String
  If n < 0 Then n = -n
  If n < 100 Then Return ""  '' error code
  If n < 1000 Then Return Str(n)
  If n < 10000 Then Return "" 
  Dim ns As String = Str(n)
  If Len(ns) Mod 2 = 0 Then Return ""  '' need to have an odd number of digits for there to be 3 middle
  Return Mid(ns, Len(ns) \ 2, 3) 
End Function

Dim a(1 To 16) As Integer => _
{123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -123451, 2, -1, -10, 2002, -2002, 0}

Dim As Integer i
Dim As String result

Print "The 3 middle digits of the following numbers are : "
Print
For i = 1 To 16
  result = middleThreeDigits(a(i))
  Print a(i), " => "; 
  If result <> "" Then
    Print result
  Else
    Print "Error: does not have 3 middle digits"
  End If
Next
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

The 3 middle digits of the following numbers are :

 123           => 123
 12345         => 234
 1234567       => 345
 987654321     => 654
 10001         => 000
-10001         => 000
-123           => 123
-100           => 100
 100           => 100
-123451        => Error: does not have 3 middle digits
 2             => Error: does not have 3 middle digits
-1             => Error: does not have 3 middle digits
-10            => Error: does not have 3 middle digits
 2002          => Error: does not have 3 middle digits
-2002          => Error: does not have 3 middle digits
 0             => Error: does not have 3 middle digits

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=ed25e84978446d65895ccd909fc092fa Click this link to run this code]'''

```gambas
Public Sub Main()
Dim iList As Integer[] = [123, 12345, 1234567, 987654321, 10001,
 -10001, -123, -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0]    'Array of integers to process
Dim sTemp As String                                                 'Temp string
Dim siCount As Short                                                'Counter
Dim sAnswer As New String[]                                         'Array, resons for failure or 'middle three digits'

For siCount = 0 To iList.Max                                        'Loop through the integers
  sTemp = Str(Abs(iList[siCount]))                                  'Convert integer to positive and place in sTemp as a string
  If Len(sTemp) < 3 Then                                            'If sTemp has less than 3 characters then..
    sAnswer.Add("This integer has less than 3 characters")          'Place text in sAnswers
  Else If Even(Len(sTemp)) Then                                     'Else If sTemp is of even length then
    sAnswer.Add("This integer has an even length")                  'Place text in sAnswers
  Else                                                              'Else..
    sAnswer.Add(Mid(sTemp, Int(Len(sTemp) / 2), 3))                 'Place the middle 3 digits in sAnswer
  Endif
Next

For siCount = 0 To iList.Max                                        'Loop through the integers
  Print Space$(10 - Len(Str(iList[siCount]))) & 
    iList[siCount] & " : " & sAnswer[siCount]                       'Print out results
Next

End
```

Output:

```txt

     12345 : 234
   1234567 : 345
 987654321 : 654
     10001 : 000
    -10001 : 000
      -123 : 123
      -100 : 100
       100 : 100
    -12345 : 234
         1 : This integer has less than 3 characters
         2 : This integer has less than 3 characters
        -1 : This integer has less than 3 characters
       -10 : This integer has less than 3 characters
      2002 : This integer has an even length
     -2002 : This integer has an even length
         0 : This integer has less than 3 characters

```



## Go

Unlike most Go examples on RC, this is written as a package to show operation of the go test tool.

File m3.go:

```go
package m3

import (
    "errors"
    "strconv"
)

var (
    ErrorLT3  = errors.New("N of at least three digits required.")
    ErrorEven = errors.New("N with odd number of digits required.")
)

func Digits(i int) (string, error) {
    if i < 0 {
        i = -i
    }
    if i < 100 {
        return "", ErrorLT3
    }
    s := strconv.Itoa(i)
    if len(s)%2 == 0 {
        return "", ErrorEven
    }
    m := len(s) / 2
    return s[m-1 : m+2], nil
}
```

File m3_test.go:

```go
package m3_test

import (
    "testing"

    "m3"
)

func TestPassing(t *testing.T) {
    type s struct {
        i int
        m string
    }
    tcs := []s{
        {123, "123"},
        {12345, "234"},
        {1234567, "345"},
        {987654321, "654"},
        {10001, "000"},
        {-10001, "000"},
    }
    for _, tc := range tcs {
        m, err := m3.Digits(tc.i)
        if err != nil {
            t.Fatalf("d(%d) returned %q.", tc.i, err)
        }
        if m != tc.m {
            t.Fatalf("d(%d) expected %q, got %q.", tc.i, tc.m, m)
        }
        t.Logf("d(%d) = %q.", tc.i, m)
    }
}

func TestFailing(t *testing.T) {
    type s struct {
        i   int
        err error
    }
    tcs := []s{
        {1, m3.ErrorLT3},
        {2, m3.ErrorLT3},
        {-1, m3.ErrorLT3},
        {-10, m3.ErrorLT3},
        {2002, m3.ErrorEven},
        {-2002, m3.ErrorEven},
        {0, m3.ErrorLT3},
    }
    for _, tc := range tcs {
        m, err := m3.Digits(tc.i)
        if err == nil {
            t.Fatal("d(%d) expected error %q, got non-error %q.",
                tc.i, tc.err, m)
        }
        if err != tc.err {
            t.Fatal("d(d) expected error %q, got %q", tc.i, tc.err, err)
        }
        t.Logf("d(%d) returns %q", tc.i, err)
    }
}
```

{{out}}
Output of go test is normally terse:

```txt

> go test
PASS
ok      m3      0.008s

```

With -v (for verbose):

```txt

> go test -v
=== RUN TestPassing-4
--- PASS: TestPassing-4 (0.00 seconds)
        m3_test.go:30: d(123) = "123".
        m3_test.go:30: d(12345) = "234".
        m3_test.go:30: d(1234567) = "345".
        m3_test.go:30: d(987654321) = "654".
        m3_test.go:30: d(10001) = "000".
        m3_test.go:30: d(-10001) = "000".
=== RUN TestFailing-4
--- PASS: TestFailing-4 (0.00 seconds)
        m3_test.go:57: d(1) returns "N of at least three digits required."
        m3_test.go:57: d(2) returns "N of at least three digits required."
        m3_test.go:57: d(-1) returns "N of at least three digits required."
        m3_test.go:57: d(-10) returns "N of at least three digits required."
        m3_test.go:57: d(2002) returns "N with odd number of digits required."
        m3_test.go:57: d(-2002) returns "N with odd number of digits required."
        m3_test.go:57: d(0) returns "N of at least three digits required."
PASS
ok      m3      0.008s

```



## Gosu


```gosu
var valid = {123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345}
valid.each(\ num ->print(middleThree(num)))

var errant = {1, 2, -1, -10, 2002, -2002, 0}
errant.each(\ num ->print(middleThree(num)))

function middleThree(x: int) : String {
  var s = Math.abs(x) as String
  if(s.length < 3) return "Error: ${x} has less than 3 digits"
  if(s.length % 2 == 0) return "Error: ${x} has an even number of digits"
  var start = (s.length / 2) - 1
  return s.substring(start, start + 3)
}
```


{{Output}}

```txt
123
234
345
654
000
000
123
100
100
234
Error: 1 has less than 3 digits
Error: 2 has less than 3 digits
Error: -1 has less than 3 digits
Error: -10 has less than 3 digits
Error: 2002 has an even number of digits
Error: -2002 has an even number of digits
Error: 0 has less than 3 digits

```



## Groovy


```groovy
def middleThree(Number number) {
    def text = Math.abs(number) as String
    assert text.size() >= 3 : "'$number' must be more than 3 numeric digits"
    assert text.size() % 2 == 1 : "'$number' must have an odd number of digits"

    int start = text.size() / 2 - 1
    text[start..(start+2)]
}
```

Test Code:

```groovy
[123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0].each { number ->
    def text = (number as String).padLeft(10)
    try {
        println "$text: ${middleThree(number)}"
    } catch(AssertionError error) {
        println "$text cannot be converted: $error.message"
    }
}
```

Output:

```txt
       123: 123
     12345: 234
   1234567: 345
 987654321: 654
     10001: 000
    -10001: 000
      -123: 123
      -100: 100
       100: 100
    -12345: 234
         1 cannot be converted: '1' must be more than 3 numeric digits. Expression: (text.size() >= 3)
         2 cannot be converted: '2' must be more than 3 numeric digits. Expression: (text.size() >= 3)
        -1 cannot be converted: '-1' must be more than 3 numeric digits. Expression: (text.size() >= 3)
       -10 cannot be converted: '-10' must be more than 3 numeric digits. Expression: (text.size() >= 3)
      2002 cannot be converted: '2002' must have an odd number of digits. Expression: ((text.size() % 2) == 1)
     -2002 cannot be converted: '-2002' must have an odd number of digits. Expression: ((text.size() % 2) == 1)
         0 cannot be converted: '0' must be more than 3 numeric digits. Expression: (text.size() >= 3)
```



## Haskell


```haskell
mid3 :: Int -> Either String String
mid3 n
  | m < 100 = Left "is too small"
  | even lng = Left "has an even number of digits"
  | otherwise = Right . take 3 $ drop ((lng - 3) `div` 2) s
  where
    m = abs n
    s = show m
    lng = length s

-- TEST --------------------------------------------------------
main :: IO ()
main = do
  let xs =
        [ 123
        , 12345
        , 1234567
        , 987654321
        , 10001
        , -10001
        , -123
        , -100
        , 100
        , -12345
        , 1
        , 2
        , -1
        , -10
        , 2002
        , -2002
        , 0
        ]
      w = maximum $ (length . show) <$> xs
  (putStrLn . unlines) $
    (\n ->
        justifyRight w ' ' (show n) ++
        " -> " ++ either ((>>= id) . ("(" :) . (: [")"])) id (mid3 n)) <$>
    xs
  where
    justifyRight :: Int -> Char -> String -> String
    justifyRight n c s = drop (length s) (replicate n c ++ s)
```

Output:

```txt
      123 -> 123
    12345 -> 234
  1234567 -> 345
987654321 -> 654
    10001 -> 000
   -10001 -> 000
     -123 -> 123
     -100 -> 100
      100 -> 100
   -12345 -> 234
        1 -> (is too small)
        2 -> (is too small)
       -1 -> (is too small)
      -10 -> (is too small)
     2002 -> (has an even number of digits)
    -2002 -> (has an even number of digits)
        0 -> (is too small)
```


=={{header|Icon}} and {{header|Unicon}}==

The following solution works in both languages.


```unicon
procedure main(a)
   every n := !a do write(right(n,15)," -> ",midM(n))
end

procedure midM(n,m)
   /m := 3
   n := abs(n)
   return n ? if (*n >= m) then
                 if (((*n-m) % 2) = 0) then (move((*n - m)/2),move(m))
                 else "wrong number of digits"
              else "too short"
end
```


with output:


```txt
->m3d 123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345 1 2 -1 -10 2002 -2002 0
            123 -> 123
          12345 -> 234
        1234567 -> 345
      987654321 -> 654
          10001 -> 000
         -10001 -> 000
           -123 -> 123
           -100 -> 100
            100 -> 100
         -12345 -> 234
              1 -> too short
              2 -> too short
             -1 -> too short
            -10 -> too short
           2002 -> wrong number of digits
          -2002 -> wrong number of digits
              0 -> too short
->
```



## J

'''Solution:'''

```j
asString=: ":"0                NB. convert vals to strings
getPfxSize=: [: -:@| 3 -~ #    NB. get size of prefix to drop before the 3 middle digits
getMid3=: (3 {. getPfxSize }. ,&'err') :: ('err'"_)  NB. get 3 middle digits or return 'err'
getMiddle3=: getMid3@asString@:|
```

'''Example:'''

```j
   vals=: 123 12345 1234567 987654321 10001 _10001 _123 _100 100 _12345  1 2 _1 _10 2002 _2002 0
   getMiddle3 vals
123
234
345
654
000
000
123
100
100
234
err
err
err
err
err
err
err
```



## Java


```Java
public class MiddleThreeDigits {

    public static void main(String[] args) {
        final long[] passing = {123, 12345, 1234567, 987654321, 10001, -10001,
            -123, -100, 100, -12345, Long.MIN_VALUE, Long.MAX_VALUE};

        final int[] failing = {1, 2, -1, -10, 2002, -2002, 0, Integer.MIN_VALUE,
            Integer.MAX_VALUE};

        for (long n : passing)
            System.out.printf("middleThreeDigits(%s): %s\n", n, middleThreeDigits(n));

        for (int n : failing)
            System.out.printf("middleThreeDigits(%s): %s\n", n, middleThreeDigits(n));
    }

    public static <T> String middleThreeDigits(T n) {
        String s = String.valueOf(n);
        if (s.charAt(0) == '-')
            s = s.substring(1);
        int len = s.length();
        if (len < 3 || len % 2 == 0)
            return "Need odd and >= 3 digits";
        int mid = len / 2;
        return s.substring(mid - 1, mid + 2);
    }
}
```


```txt
middleThreeDigits(123): 123
middleThreeDigits(12345): 234
middleThreeDigits(1234567): 345
middleThreeDigits(987654321): 654
middleThreeDigits(10001): 000
middleThreeDigits(-10001): 000
middleThreeDigits(-123): 123
middleThreeDigits(-100): 100
middleThreeDigits(100): 100
middleThreeDigits(-12345): 234
middleThreeDigits(-9223372036854775808): 368
middleThreeDigits(9223372036854775807): 368
middleThreeDigits(1): Need odd and >= 3 digits
middleThreeDigits(2): Need odd and >= 3 digits
middleThreeDigits(-1): Need odd and >= 3 digits
middleThreeDigits(-10): Need odd and >= 3 digits
middleThreeDigits(2002): Need odd and >= 3 digits
middleThreeDigits(-2002): Need odd and >= 3 digits
middleThreeDigits(0): Need odd and >= 3 digits
middleThreeDigits(-2147483648): Need odd and >= 3 digits
middleThreeDigits(2147483647): Need odd and >= 3 digits
```



## JavaScript


```JavaScript
function middleThree(x){
  var n=''+Math.abs(x); var l=n.length-1;
  if(l<2||l%2) throw new Error(x+': Invalid length '+(l+1));
  return n.slice(l/2-1,l/2+2);
}

[123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345,
1, 2, -1, -10, 2002, -2002, 0].forEach(function(n){
  try{console.log(n,middleThree(n))}catch(e){console.error(e.message)}
});
```



```txt
123 "123"
12345 "234"
1234567 "345"
987654321 "654"
10001 "000"
-10001 "000"
-123 "123"
-100 "100"
100 "100"
-12345 "234"
1: Invalid length 1
2: Invalid length 1
-1: Invalid length 1
-10: Invalid length 2
2002: Invalid length 4
-2002: Invalid length 4
0: Invalid length 1
```



Or, using an option type, composing a solution from existing generic primitives, and formatting the output a little:
{{Trans|Python}}

```javascript
(() => {
    'use strict';

    // mid3digits :: Int -> Either String String
    const mid3digits = n => {
        const
            m = abs(n),
            s = m.toString();
        return 100 > m ? (
            Left('Less than 3 digits')
        ) : even(length(s)) ? (
            Left('Even digit count')
        ) : Right(take(3, drop(quot(length(s) - 3, 2), s)));
    };

    // TEST -----------------------------------------------
    const main = () => {
        const
            xs = [
                123, 12345, 1234567, 987654321, 10001, -10001, -123,
                -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0
            ],
            w = maximum(map(x => x.toString().length, xs));
        return (
            unlines(map(
                n => justifyRight(w, ' ', n.toString()) + ' -> ' +
                either(
                    s => '(' + s + ')',
                    id,
                    mid3digits(n)
                ),
                xs
            ))
        );
    };

    // GENERIC FUNCTIONS ----------------------------------

    // Left :: a -> Either a b
    const Left = x => ({
        type: 'Either',
        Left: x
    });

    // Right :: b -> Either a b
    const Right = x => ({
        type: 'Either',
        Right: x
    });

    // abs :: Num -> Num
    const abs = Math.abs;

    // drop :: Int -> [a] -> [a]
    // drop :: Int -> Generator [a] -> Generator [a]
    // drop :: Int -> String -> String
    const drop = (n, xs) =>
        Infinity > length(xs) ? (
            xs.slice(n)
        ) : (take(n, xs), xs);

    // either :: (a -> c) -> (b -> c) -> Either a b -> c
    const either = (fl, fr, e) =>
        'Either' === e.type ? (
            undefined !== e.Left ? (
                fl(e.Left)
            ) : fr(e.Right)
        ) : undefined;

    // even :: Int -> Bool
    const even = n => 0 === n % 2;

    // foldl1 :: (a -> a -> a) -> [a] -> a
    const foldl1 = (f, xs) =>
        1 < xs.length ? xs.slice(1)
        .reduce(f, xs[0]) : xs[0];

    // id :: a -> a
    const id = x => x;

    // justifyRight :: Int -> Char -> String -> String
    const justifyRight = (n, cFiller, s) =>
        n > s.length ? (
            s.padStart(n, cFiller)
        ) : s;

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // maximum :: Ord a => [a] -> a
    const maximum = xs =>
        0 < xs.length ? (
            foldl1((a, x) => x > a ? x : a, xs)
        ) : undefined;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // quot :: Int -> Int -> Int
    const quot = (n, m) => Math.floor(n / m);

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt

      123 -> 123
    12345 -> 234
  1234567 -> 345
987654321 -> 654
    10001 -> 000
   -10001 -> 000
     -123 -> 123
     -100 -> 100
      100 -> 100
   -12345 -> 234
        1 -> (Less than 3 digits)
        2 -> (Less than 3 digits)
       -1 -> (Less than 3 digits)
      -10 -> (Less than 3 digits)
     2002 -> (Even digit count)
    -2002 -> (Even digit count)
        0 -> (Less than 3 digits)
```



## jq


```jq
def middle3:
  if . <  0 then -. else . end
  | tostring as $s
  | ($s | length) as $n
  | if $n<3 or ($n % 2) == 0 then "invalid length: \($n)"
    else (($n - 1) / 2) as $n |  $s[$n - 1 : $n + 2]
    end ;

(123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0)
  | "\(.) => \( .|middle3 )"
```

Typescript:
```sh
 $ jq -r -n -f Middle_three_digits.jq
123 => 123
12345 => 234
1234567 => 345
987654321 => 654
10001 => 000
-10001 => 000
-123 => 123
-100 => 100
100 => 100
-12345 => 234
1 => invalid length: 1
2 => invalid length: 1
-1 => invalid length: 1
-10 => invalid length: 2
2002 => invalid length: 4
-2002 => invalid length: 4
0 => invalid length: 1 
```


## Julia

{{works with|Julia|1.0.3}}


```julia
using Printf

function middle3(n::Integer)
    l = ndigits(n)
    iseven(l) && error("n must have an odd number of digits")
    l < 3 && error("n must have 3 digits or more")
    mid = (l + 1) ÷ 2
    abs(n) ÷ 10^(mid-2) % 10^3
end

for n = [123, 12345, 1234567, 987654321, 10001, -10001, -123,
         -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0]
    @printf("%10d -> %s\n", n, try middle3(n) catch e e.msg end)
end
```


{{out}}

```txt
       123 -> 123
     12345 -> 234
   1234567 -> 345
 987654321 -> 654
     10001 -> 000
    -10001 -> 000
      -123 -> 123
      -100 -> 100
       100 -> 100
    -12345 -> 234
         1 -> n must have 3 digits or more
         2 -> n must have 3 digits or more
        -1 -> n must have 3 digits or more
       -10 -> n must have an odd number of digits
      2002 -> n must have an odd number of digits
     -2002 -> n must have an odd number of digits
         0 -> n must have 3 digits or more
```



## K


```K

/ Rosetta code - Middle three digits
/ mid3.k
mid3: {qs:$x;:[qs[0]="-";qs: 1 _ qs];:[(#qs)<3;:"small";(1+#qs)!2;:"even"];p:(-3+#qs)%2;:(|p _|p _ qs)}

```

The output of the session:
{{out}}

```txt

K Console - Enter \ for help

  \l mid3

  mid3' 123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345 1 2 -1 -10
 2002 -2002 0
("123"
 "234"
 "345"
 "654"
 "000"
 "000"
 "123"
 "100"
 "100"
 "234"
 "small"
 "small"
 "small"
 "small"
 "even"
 "even"
 "small")

```



## Klong


```k
items::[123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345 1 2 -1 -10 2002 -2002 0]

mid3::{[d k];:[3>k::#$#x;"small":|0=k!2;"even";(-d)_(d::_(k%2)-1)_$#x]}
.p(mid3'items)
```

Output:

```txt
[123 234 345 654 000 000 123 100 100 234 small small small small even even small]
```



## Kotlin


```scala
fun middleThree(x: Int): Int? {
    val s = Math.abs(x).toString()
    return when {
        s.length < 3 -> null // throw Exception("too short!")
        s.length % 2 == 0 -> null // throw Exception("even number of digits")
        else -> ((s.length / 2) - 1).let { s.substring(it, it + 3) }.toInt()
    }
}

fun main(args: Array<String>) {
    println(middleThree(12345)) // 234
    println(middleThree(1234)) // null
    println(middleThree(1234567)) // 345
    println(middleThree(123))// 123
    println(middleThree(123555)) //null
}
```



## Lasso


```Lasso
define middlethree(value::integer) => {
	local(
		pos_value	= math_abs(#value),
		stringvalue	= #pos_value -> asstring,
		intlength	= #stringvalue -> size,
		middle		= integer((#intlength + 1) / 2),
		prefix		= string(#value) -> padleading(15)& + ': '
	)

	#intlength < 3 ? return #prefix + 'Error: too few digits'
	not(#intlength % 2) ? return #prefix + 'Error: even number of digits'

	return #prefix + #stringvalue -> sub(#middle -1, 3)

}
'
```txt
'
with number in array(123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0) do {^

	middlethree(#number)
	'<br />'
^}
'
```
'
```

'''Output'''

```txt
            123: 123
          12345: 234
        1234567: 345
      987654321: 654
          10001: 000
         -10001: 000
           -123: 123
           -100: 100
            100: 100
         -12345: 234
              1: Error: too few digits
              2: Error: too few digits
             -1: Error: too few digits
            -10: Error: too few digits
           2002: Error: even number of digits
          -2002: Error: even number of digits
              0: Error: too few digits
```



## Logo


```logo
to middle3digits :n
  if [less? :n 0] [make "n minus :n]
  local "len make "len count :n
  if [less? :len 3] [(throw "error [Number must have at least 3 digits])]
  if [equal? 0 modulo :len 2] [(throw "error [Number must have odd number of digits])]
  while [greater? count :n 3] [
    make "n butlast butfirst :n
  ]
  output :n
end

foreach [123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345
         1 2 -1 -10 2002 -2002 0] [
    type sentence (word ? ": char 9) runresult [if [less? count ? 7] [char 9]]
    make "mid runresult [catch "error [middle3digits ?]]
    print ifelse [empty? :mid] [item 2 error] [:mid]
]

bye
```


{{Output}}

```txt
123:	 	123
12345:	 	234
1234567:	345
987654321:	654
10001:	 	000
-10001:	 	000
-123:	 	123
-100:	 	100
100:	 	100
-12345:	 	234
1:	 	Number must have at least 3 digits
2:	 	Number must have at least 3 digits
-1:	 	Number must have at least 3 digits
-10:	 	Number must have at least 3 digits
2002:	 	Number must have odd number of digits
-2002:	 	Number must have odd number of digits
0:	 	Number must have at least 3 digits
```



## Lua


```lua
function middle_three(n)
	if n < 0 then
		n = -n
	end
		
	n = tostring(n)
	if #n % 2 == 0 then
		return "Error: the number of digits is even."
	elseif #n < 3 then
		return "Error: the number has less than 3 digits."
	end

	local l = math.floor(#n/2)
	return n:sub(l, l+2)
end

-- test
do
	local t = {123, 12345, 1234567, 987654321,
	10001, -10001, -123, -100, 100, -12345, 1,
	2, -1, -10, 2002, -2002, 0}

	for _,n in pairs(t) do
		print(n, middle_three(n))	
	end
end
```


{{out}}

```txt
123	123
12345	234
1234567	345
987654321	654
10001	000
-10001	000
-123	123
-100	100
100	100
-12345	234
1	Error: the number has less than 3 digits.
2	Error: the number has less than 3 digits.
-1	Error: the number has less than 3 digits.
-10	Error: the number is even.
2002	Error: the number is even.
-2002	Error: the number is even.
0	Error: the number has less than 3 digits.
```



## Maple


```maple
middleDigits := proc(n)
	local nList, start;
	nList := [seq(parse(i), i in convert (abs(n), string))];
	if numelems(nList) < 3 then
		printf ("%9a: Error: Not enough digits.", n);
	elif numelems(nList) mod 2 = 0 then
		printf ("%9a: Error: Even number of digits.", n);
	else
		start := (numelems(nList)-1)/2;
		printf("%9a: %a%a%a", n, op(nList[start..start+2]));
	end if;
end proc:

a := [123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345,
	 1, 2, -1, -10, 2002, -2002, 0]:
for i in a do
	middleDigits(i);
	printf("\n");
end do;
```

{{out}}

```txt

      123: 123
    12345: 234
  1234567: 345
987654321: 654
    10001: 000
   -10001: 000
     -123: 123
     -100: 100
      100: 100
   -12345: 234
        1: Error: Not enough digits.
        2: Error: Not enough digits.
       -1: Error: Not enough digits.
      -10: Error: Not enough digits.
     2002: Error: Even number of digits.
    -2002: Error: Even number of digits.
        0: Error: Not enough digits.

```



## Mathematica


```Mathematica
middleThree[n_Integer] :=
 Block[{digits = IntegerDigits[n], len},
  len = Length[digits];
  If[len < 3 || EvenQ[len], "number digits odd or less than 3", 
   len = Ceiling[len/2]; 
   StringJoin @@ (ToString /@ digits[[len - 1 ;; len + 1]])]]

testData = {123, 12345, 1234567, 987654321, 10001, -10001, -123, -100,
    100, -12345, 1, 2, -1, -10, 2002, -2002, 0};

Column[middleThree /@ testData]
```

{{out}}
```txt

123
234
345
654
000
000
123
100
100
234
err: n too small
err: n too small
err: n too small
err: n too small
err: even number of digits
err: even number of digits
err: n too small

```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function s=middle_three_digits(a)
% http://rosettacode.org/wiki/Middle_three_digits

s=num2str(abs(a));

if ~mod(length(s),2)
	s='*** error: number of digits must be odd ***';
	return;
end; 
if length(s)<3,
	s='*** error: number of digits must not be smaller than 3 ***';
	return;
end;

s = s((length(s)+1)/2+[-1:1]);
```

Test with 

```MATLAB
 x=[123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0];
for k=1:length(x); fprintf(1,'%9i:%s\n',x(k),middle_three_digits(x(k)));end;
```

Result

```txt

      123:123
    12345:234
  1234567:457
987654321:654
    10001:000
   -10001:000
     -123:123
     -100:100
      100:100
   -12345:234
        1:*** error: number of digits must not be smaller than 3 ***
        2:*** error: number of digits must not be smaller than 3 ***
       -1:*** error: number of digits must not be smaller than 3 ***
      -10:*** error: number of digits must be odd ***
     2002:*** error: number of digits must be odd ***
    -2002:*** error: number of digits must be odd ***
        0:*** error: number of digits must not be smaller than 3 ***

```



## MiniScript


```MiniScript
middle3 = function(num)
    if num < 0 then num = -num
    s = str(num)
    if s.len < 3 then return "Input too short"
    if s.len % 2 == 0 then return "Input length not odd"
    mid = (s.len + 1) / 2 - 1
    return s[mid-1:mid+2]
end function

for test in [123, 12345, 1234567, 987654321, 10001, -10001, -123, -100,
 100, -12345, 1, 2, -1, -10, 2002, -2002, 0]
    print test + " --> " + middle3(test)
end for
```

{{out}}

```txt
123 --> 123
12345 --> 234
1234567 --> 345
987654321 --> 654
10001 --> 000
-10001 --> 000
-123 --> 123
-100 --> 100
100 --> 100
-12345 --> 234
1 --> Input too short
2 --> Input too short
-1 --> Input too short
-10 --> Input too short
2002 --> Input length not odd
-2002 --> Input length not odd
0 --> Input too short
```


=={{header|МК-61/52}}==
<lang>П0	lg	[x]	3	-	x>=0	23	ИП0	1	0
/	[x]	^	lg	[x]	10^x	П1	/	{x}	ИП1
*	БП	00	1	+	x=0	29	ИП0	С/П	0
/
```


''Instruction:'' enter the number in the РX (on display), the result after the execution of the same. In the case of an even or less than 3 number of digits the indicator displays an error message.


## ML

=
## mLite
=

```ocaml

	val test_array = ["123","12345","1234567","987654321","10001","~10001","~123","~100","100","~12345","1","2","~1","~10","2002","~2002","0"];
 
	fun even (x rem 2 = 0) = true | _ = false;
 
	fun middleThreeDigits
			(h :: t, s, 1 ) = s @ " --> too small" 
		|	(h :: t, s, 2 ) = s @ " --> has even digits"
		|	(h :: t, s, 3 ) where (len (h :: t) = 3) = s @ " --> " @ (implode (h :: t))
		|	(h :: t, s, 3 ) = (middleThreeDigits ( sub (t, 0, (len t)-1), s, 3)) 
		|	(h :: t, s, m) = if len (h :: t) < 3 then
							middleThreeDigits (h :: t, s, 1)
						  else
						    if even ` len (h :: t) then
							  middleThreeDigits (h :: t, s, 2)
							else
							  middleThreeDigits (h :: t, s, 3)
 
 
		|	s	= if sub (s, 0, 1) = "~" then
				middleThreeDigits (sub (explode s, 1, len s), s, 0)
			else
				middleThreeDigits (explode s, s, 0)
	;
 
	map (println o middleThreeDigits) test_array;
```

Output:

```txt
123 --> 123
12345 --> 234
1234567 --> 345
987654321 --> 654
10001 --> 000
~10001 --> 000
~123 --> 123
~100 --> 100
100 --> 100
~12345 --> 234
1 --> too small
2 --> too small
~1 --> too small
~10 --> too small
2002 --> has even digits
~2002 --> has even digits
0 --> too small
```



## MUMPS

This sample shows the MUMPS code required to pass the specification.

```MUMPS
/* MUMPS */
MID3(N)  ;                                                                       
        N LEN,N2                                                                
        S N2=$S(N<0:-N,1:N)                                                     
        I N2<100 Q "NUMBER TOO SMALL"                                           
        S LEN=$L(N2)                                                            
        I LEN#2=0 Q "EVEN NUMBER OF DIGITS"                                     
        Q $E(N2,LEN\2,LEN\2+2)

F I=123,12345,1234567,987654321,10001,-10001,-123,-100,100,-12345,1,2,-1,-10,2002,-2002,0 W !,$J(I,10),": ",$$MID3^MID3(I)                 

```

'''Output:'''

```txt

       123: 123                                                                 
     12345: 234                                                                 
   1234567: 345                                                                 
 987654321: 654                                                                 
     10001: 000                                                                 
    -10001: 000                                                                 
      -123: 123                                                                 
      -100: 100                                                                 
       100: 100                                                                 
    -12345: 234                                                                 
         1: NUMBER TOO SMALL                                                    
         2: NUMBER TOO SMALL                                                    
        -1: NUMBER TOO SMALL                                                    
       -10: NUMBER TOO SMALL                                                    
      2002: EVEN NUMBER OF DIGITS                                               
     -2002: EVEN NUMBER OF DIGITS                                               
         0: NUMBER TOO SMALL      

```



## NetRexx

This sample goes the extra mile and provides a method that can display the middle N digits from the input value.  To satisfy the requirements of this task,  a static invocation of this general method is also provided with the value '''<tt>3</tt>''' hard coded as the digit count.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

sl = '123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345' -
     '1 2 -1 -10 2002 -2002 0' -
     'abc 1e3 -17e-3 4004.5 12345678 9876543210' -- extras

parse arg digsL digsR .
if \digsL.datatype('w') then digsL = 3
if \digsR.datatype('w') then digsR = digsL
if digsL > digsR        then digsR = digsL

loop dc = digsL to digsR
  say 'Middle' dc 'characters'
  loop nn = 1 to sl.words()
    val = sl.word(nn)
    say middleDigits(dc, val)
    end nn
  say
  end dc
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method middle3Digits(val) constant
  return middleDigits(3, val)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method middleDigits(digitCt, val) constant
  text = val.right(15)':'
  even = digitCt // 2 == 0 -- odd or even?
  select
    when digitCt <= 0                       then text = 'digit selection size must be >= 1'
    when \val.datatype('w')                 then text = text 'is not a whole number'
    when val.abs().length < digitCt         then text = text 'has less than' digitCt 'digits'
    when \even & val.abs().length // 2 == 0 then text = text 'does not have an odd number of digits'
    when even  & val.abs().length // 2 \= 0 then text = text 'does not have an even number of digits'
    otherwise do
      val = val.abs()
      valL = val.length
      cutP = (valL - digitCt) % 2
      text = text val.substr(cutP + 1, digitCt)
      end
    catch NumberFormatException
      text = val 'is not numeric'
    end
  return text

```

'''Output:'''

```txt

Middle 3 characters
            123: 123
          12345: 234
        1234567: 345
      987654321: 654
          10001: 000
         -10001: 000
           -123: 123
           -100: 100
            100: 100
         -12345: 234
              1: has less than 3 digits
              2: has less than 3 digits
             -1: has less than 3 digits
            -10: has less than 3 digits
           2002: does not have an odd number of digits
          -2002: does not have an odd number of digits
              0: has less than 3 digits
            abc: is not a whole number
            1e3: is not a whole number
         -17e-3: is not a whole number
         4004.5: is not a whole number
       12345678: does not have an odd number of digits
     9876543210: does not have an odd number of digits

```



## NewLISP


```NewLISP

(define (middle3 x)
    (if (even? (length x))
        (println "You entered " x ". I need an odd number of digits, not " (length x) ".")
        (if (< (length x) 3)
            (println "You entered " x ". Sorry, but I need 3 or more digits.")
            (println "The middle 3 digits of " x " are " ((- (/ (- (length x) 1) 2) 1) 3 (string (abs x))) ".")
        )
    )
)

(map middle3 lst)

```

'''Output:'''

```txt

The middle 3 digits of 123 are 123.
The middle 3 digits of 12345 are 234.
The middle 3 digits of 1234567 are 345.
The middle 3 digits of 987654321 are 654.
The middle 3 digits of 10001 are 000.
The middle 3 digits of -10001 are 000.
The middle 3 digits of -123 are 123.
The middle 3 digits of -100 are 100.
The middle 3 digits of 100 are 100.
The middle 3 digits of -12345 are 234.
You entered 1. Sorry, but I need 3 or more digits.
You entered 2. Sorry, but I need 3 or more digits.
You entered -1. Sorry, but I need 3 or more digits.
You entered 10. I need an odd number of digits, not 2.
You entered 2002. I need an odd number of digits, not 4.
You entered -2002. I need an odd number of digits, not 4.
You entered 0. Sorry, but I need 3 or more digits.

```



## Nim


```nim
import math

proc middleThreeDigits(i): auto =
  var s = $abs(i)
  if s.len < 3 or s.len mod 2 == 0:
    raise newException(ValueError, "Need odd and >= 3 digits")
  let mid = s.len div 2
  return s[mid-1..mid+1]

const passing = @[123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345]
const failing = @[1, 2, -1, -10, 2002, -2002, 0]

for i in passing & failing:
  var answer = try: middleThreeDigits(i)
    except ValueError: getCurrentExceptionMsg()
  echo "middleThreeDigits(",i,") returned: ",answer
```

Output:

```txt
middleThreeDigits(123) returned: 123
middleThreeDigits(12345) returned: 234
middleThreeDigits(1234567) returned: 345
middleThreeDigits(987654321) returned: 654
middleThreeDigits(10001) returned: 000
middleThreeDigits(-10001) returned: 000
middleThreeDigits(-123) returned: 123
middleThreeDigits(-100) returned: 100
middleThreeDigits(100) returned: 100
middleThreeDigits(-12345) returned: 234
middleThreeDigits(1) returned: Need odd and >= 3 digits
middleThreeDigits(2) returned: Need odd and >= 3 digits
middleThreeDigits(-1) returned: Need odd and >= 3 digits
middleThreeDigits(-10) returned: Need odd and >= 3 digits
middleThreeDigits(2002) returned: Need odd and >= 3 digits
middleThreeDigits(-2002) returned: Need odd and >= 3 digits
middleThreeDigits(0) returned: Need odd and >= 3 digits
```



## Objeck


```objeck

class Test {
  function : Main(args : String[]) ~ Nil {
    text := "123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0";
    ins := text->Split(",");
    each(i : ins) {
      in := ins[i]->Trim();
      IO.Console->Print(in)->Print(": ");
      in := (in->Size() > 0 & in->Get(0) = '-') ? in->SubString(1, in->Size() - 1) : in;
      IO.Console->PrintLine((in->Size() < 2 | in->Size() % 2 = 0) ? "Error" : in->SubString((in->Size() - 3) / 2, 3));
    };
  }

}
```



```txt

123: 123
12345: 234
1234567: 345
987654321: 654
10001: 000
-10001: 000
-123: 123
-100: 100
100: 100
-12345: 234
1: Error
2: Error
-1: Error
-10: Error
2002: Error
-2002: Error
0: Error

```



## OCaml



```ocaml
let even x = (x land 1) <> 1

let middle_three_digits x =
  let s = string_of_int (abs x) in
  let n = String.length s in
  if n < 3 then failwith "need >= 3 digits" else
  if even n then failwith "need odd number of digits" else
  String.sub s (n / 2 - 1) 3

let passing = [123; 12345; 1234567; 987654321; 10001; -10001; -123; -100; 100; -12345]
let failing = [1; 2; -1; -10; 2002; -2002; 0]

let print x =
  let res =
    try (middle_three_digits x)
    with Failure e -> "failure: " ^ e
  in
  Printf.printf "%d: %s\n" x res

let () =
  print_endline "Should pass:";
  List.iter print passing;
  print_endline "Should fail:";
  List.iter print failing;
;;
```


{{out}}

```txt

Should pass:
123: 123
12345: 234
1234567: 345
987654321: 654
10001: 000
-10001: 000
-123: 123
-100: 100
100: 100
-12345: 234

Should fail:
1: failure: need >= 3 digits
2: failure: need >= 3 digits
-1: failure: need >= 3 digits
-10: failure: need >= 3 digits
2002: failure: need odd number of digits
-2002: failure: need odd number of digits
0: failure: need >= 3 digits

```



## Oforth



```Oforth
: middle3
| s sz |
   abs asString dup ->s size ->sz
   sz 3 <    ifTrue: [ "Too short" println return ]
   sz isEven ifTrue: [ "Not odd number of digits" println return ]
   sz 3 - 2 / 1+  dup 2 + s extract ;
```


{{out}}

```txt

[ 123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345] map(#middle3) println
[123, 234, 345, 654, 000, 000, 123, 100, 100, 234]

[ 1, 2, -1, -10, 2002, -2002, 0 ] apply(#middle3)
Too short
Too short
Too short
Too short
Not odd number of digits
Not odd number of digits
Too short

```



## PARI/GP

{{works with|PARI/GP|2.6.0}}

```parigp
middle(n)=my(v=digits(n));if(#v>2&&#v%2,100*v[#v\2]+10*v[#v\2+1]+v[#v\2+2],"no middle 3 digits");
apply(middle,[123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0])
```

Output:

```txt
%1 = [123, 234, 345, 654, 0, 0, 123, 100, 100, 234, "no middle 3 digits", "no middle 3 digits", "no middle 3 digits", "no middle 3 digits", "no middle 3 digits", "no middle 3 digits", "no middle 3 digits"]
```


If for some reason you want to see the leading digits, you can run

```parigp
apply(n-> Strprintf("%03d", n), %)
```


For earlier versions <code>digits</code> can be defined as

```parigp
digits(n)=eval(Vec(Str(n)))
```

or more efficiently as

```parigp
digits(n)=Vec(apply(n->n-48,Vectorsmall(Str(n))))
```


## Pascal

{{works with|Free Pascal}}

```pascal
program Midl3dig;
{$IFDEF FPC}
  {$MODE Delphi} //result /integer => Int32 aka longInt etc..
{$ELSE}
  {$APPTYPE console} // Delphi
{$ENDIF}
uses
  sysutils;   //IntToStr
function GetMid3dig(i:NativeInt):Ansistring;
var
  n,l: NativeInt;
Begin
  setlength(result,0);
  //n = |i| jumpless abs
  n := i-((ORD(i>0)-1)AND (2*i));
  //calculate digitcount
  IF n > 0 then
    l := trunc(ln(n)/ln(10))+1
  else
    l := 1;
  if l<3 then Begin  write('got too few digits');  EXIT; end;
  If Not(ODD(l)) then Begin write('got even number of digits'); EXIT; end;
  result:= copy(IntToStr(n),l DIV 2,3);
end;
const
  Test : array [0..16] of NativeInt =
    ( 123,12345,1234567,987654321,10001,-10001,
    -123,-100,100,-12345,1,2,-1,-10,2002,-2002,0);
var
  i,n : NativeInt;
Begin
  For i := low(Test) to High(Test) do
  Begin
    n := Test[i];
    writeln(n:9,': ',GetMid3dig(Test[i]));
  end;
end.
```

{{out}}

```txt
      123: 123
    12345: 234
  1234567: 345
987654321: 654
    10001: 000
   -10001: 000
     -123: 123
     -100: 100
      100: 100
   -12345: 234
        1: got too few digits
        2: got too few digits
       -1: got too few digits
      -10: got too few digits
     2002: got even number of digits
    -2002: got even number of digits
        0: got too few digits

```


## Perl


```Perl
#!/usr/bin/perl
use strict ;
use warnings ;

sub middlethree {
   my $number = shift ;
   my $testnumber = abs $number ;
   my $error = "Middle 3 digits can't be shown" ;
   my $numberlength = length $testnumber ;
   if ( $numberlength < 3 ) {
      print "$error : $number too short!\n" ;
      return ;
   }
   if ( $numberlength % 2 == 0 ) {
      print "$error : even number of digits in $number!\n" ;
      return ;
   }
   my $middle = int ( $numberlength  / 2 ) ;
   print "Middle 3 digits of $number : " . substr( $testnumber , $middle - 1 , 3 ) . " !\n" ;
   return ;
}

my @numbers = ( 123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345 ,
      1, 2, -1, -10, 2002, -2002, 0 ) ;
map { middlethree( $_ ) } @numbers ;

```

{{out}}

```txt
Middle 3 digits of 123 : 123 !
Middle 3 digits of 12345 : 234 !
Middle 3 digits of 1234567 : 345 !
Middle 3 digits of 987654321 : 654 !
Middle 3 digits of 10001 : 000 !
Middle 3 digits of -10001 : 000 !
Middle 3 digits of -123 : 123 !
Middle 3 digits of -100 : 100 !
Middle 3 digits of 100 : 100 !
Middle 3 digits of -12345 : 234 !
Middle 3 digits can't be shown : 1 too short!
Middle 3 digits can't be shown : 2 too short!
Middle 3 digits can't be shown : -1 too short!
Middle 3 digits can't be shown : -10 too short!
Middle 3 digits can't be shown : even number of digits in 2002!
Middle 3 digits can't be shown : even number of digits in -2002!
Middle 3 digits can't be shown : 0 too short!

```



## Perl 6


```Perl6
sub middle-three($n) {
    given $n.abs {
        when .chars < 3  { "$n is too short" }
        when .chars %% 2 { "$n has an even number of digits" }
        default          { "The three middle digits of $n are: ", .substr: (.chars - 3)/2, 3 }
    }
}

say middle-three($_) for <
    123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345
    1 2 -1 -10 2002 -2002 0
>;
```

{{out}}

```txt
The three middle digits of 123 are:  123
The three middle digits of 12345 are:  234
The three middle digits of 1234567 are:  345
The three middle digits of 987654321 are:  654
The three middle digits of 10001 are:  000
The three middle digits of -10001 are:  000
The three middle digits of -123 are:  123
The three middle digits of -100 are:  100
The three middle digits of 100 are:  100
The three middle digits of -12345 are:  234
1 is too short
2 is too short
-1 is too short
-10 is too short
2002 has an even number of digits
-2002 has an even number of digits
0 is too short
```


It is also possible to write a regular expression with a code assertion:

```perl6
for [\~] ^10 { say "$_ => $()" if m/^^(\d+) <(\d**3)> (\d+) $$ <?{ $0.chars == $1.chars}> / }
```

{{out}}

```txt
01234 => 123
0123456 => 234
012345678 => 345
```



## Phix


```Phix
procedure mid3(integer i)
    string s = sprintf("%d",abs(i))
    integer k = length(s)-3
    printf(1,"%10d: %s\n",{i,iff(k<0 or and_bits(k,1)?"error":s[k/2+1..k/2+3])})
end procedure

constant tests = {123,12345,1234567,987654321,10001,-10001,-123,-100,100,-12345,
                  1,2,-1,-10,2002,-2002,0}
for i=1 to length(tests) do
    mid3(tests[i])
end for
```

{{out}}

```txt

       123: 123
     12345: 234
   1234567: 345
 987654321: 654
     10001: 000
    -10001: 000
      -123: 123
      -100: 100
       100: 100
    -12345: 234
         1: error
         2: error
        -1: error
       -10: error
      2002: error
     -2002: error
         0: error

```



## PHP


```PHP
// 32-bit builds of PHP: Integers can be from -2147483648 to 2147483647
// 64-bit builds of PHP: Integers can be from -9223372036854775808 to 9223372036854775807

function middlethree($integer)
{
	$int 	= (int)str_replace('-','',$integer);
	$length = strlen($int);

	if(is_int($int))
	{
		if($length >= 3)
		{
			if($length % 2 == 1)
			{
				$middle = floor($length / 2) - 1;
				return substr($int,$middle, 3);
			}
			else
			{
				return 'The value must contain an odd amount of digits...';	
			}
		}
		else
		{
			return 'The value must contain at least three digits...';	
		}
	}
	else
	{
		return 'The value does not appear to be an integer...';
	}
}

$numbers = array(123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0);

foreach($numbers as $nums)
{
	echo $nums.' : '.middlethree($nums). '
';
}
```

Output:

```txt
123 : 123
12345 : 234
1234567 : 345
987654321 : 654
10001 : 000
-10001 : 000
-123 : 123
-100 : 100
100 : 100
-12345 : 234
1 : The value must contain at least three digits...
2 : The value must contain at least three digits...
-1 : The value must contain at least three digits...
-10 : The value must contain at least three digits...
2002 : The value must contain an odd amount of digits...
-2002 : The value must contain an odd amount of digits...
0 : The value must contain at least three digits...
```



## PicoLisp


```PicoLisp
(de middle3digits (N)
   (let (Lst (chop (abs N))  Len (length Lst))
      (tab (10 -2 -30)
         N
         ":"
         (cond
            ((> 3 Len) "not enough digits")
            ((bit? 1 Len)
               (head 3 (nth Lst (/ Len 2))) )
            (T "even number of digits") ) ) ) )
```

Test:

```PicoLisp
(mapc middle3digits
   (123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345
      1 2 -1 -10 2002 -2002 0 ) )
```

Output:

```txt
       123: 123
     12345: 234
   1234567: 345
 987654321: 654
     10001: 000
    -10001: 000
      -123: 123
      -100: 100
       100: 100
    -12345: 234
         1: not enough digits
         2: not enough digits
        -1: not enough digits
       -10: not enough digits
      2002: even number of digits
     -2002: even number of digits
         0: not enough digits
```



## PL/I


```PL/I

middle: procedure options (main);      /* 29 October 2013 */
   declare n fixed (15);
   declare s character (25) varying;
   declare in file input;

   open file (in) title ('/MIDDLE.DAT,type(text),recsize(100)');
   on endfile (in) stop;

   do forever;
      get file (in) list (n); put skip list (n);
      n = abs(n);
      s = trim(n);
      if length(s) < 3 then put list ('not possible');
      else  if mod(length(s), 2) = 0 then put list ('not possible');
      else
         do;
            do while (length(s) > 3);
               s = substr(s, 2, length(s)-2);
            end;
            put list ('The middle three digits are: ' || s);
         end;
   end;

end middle;

```

Output:

```txt

               123      The middle three digits are: 123 
             12345      The middle three digits are: 234 
           1234567      The middle three digits are: 345 
         987654321      The middle three digits are: 654 
             10001      The middle three digits are: 000 
            -10001      The middle three digits are: 000 
              -123      The middle three digits are: 123 
              -100      The middle three digits are: 100 
               100      The middle three digits are: 100 
           -123451      not possible 
                 2      not possible 
                -1      not possible 
               -10      not possible 
              2002      not possible 
             -2002      not possible 
                 0      not possible

```



## PowerShell


```powershell
function middle3($inp){

	$str = [Math]::abs($inp)

	$leng = "$str".length

	if ($leng -lt 3){
		Write-host $inp":	[ERROR] too short."
		Return
	}
	if (($leng % 2) -eq 0){
		Write-host $inp":	[ERROR] even number of digits."
	} else {
		$trimmer = ($leng - 3) / 2
		$ans = "$str".subString($trimmer,3)

		Write-host $inp":	$ans"
	}
	Return
}

$sample = 123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0

foreach ($x in $sample){middle3 $x}
```

{{Out}}

```txt
123:    123
12345:  234
1234567:        345
987654321:      654
10001:  000
-10001: 000
-123:   123
-100:   100
100:    100
-12345: 234
1:      [ERROR] too short.
2:      [ERROR] too short.
-1:     [ERROR] too short.
-10:    [ERROR] too short.
2002:   [ERROR] even number of digits.
-2002:  [ERROR] even number of digits.
0:      [ERROR] too short.
```




## Prolog

{{works with|SWI-Prolog|6}}


```prolog

middle_3_digits(Number, [D1,D2,D3]) :-
    verify_middle_3_able(Number, Digits),    
    append(FrontDigits, [D1,D2,D3| BackDigits], Digits),
    same_length(FrontDigits, BackDigits).

verify_middle_3_able(Number, Digits) :-
    must_be(number, Number),
    AbsNumber is abs(Number),
    number_chars(AbsNumber, Digits),
    length(Digits, NumDigits),
    ( 3 > NumDigits         ->  domain_error('at least 3 digits',    Number)
    ; 0 is NumDigits mod 2  ->  domain_error('odd number of digits', Number)
    ; true
    ).

```



Test code:


```prolog

test_correct :-
    TestCases = [123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345],
    foreach( ( member(TestCase, TestCases),
               middle_3_digits(TestCase, Result) ),
            format('Middle 3 digits of ~w ~30|: ~w~n', [TestCase, Result])
          ).

```



Testing correct inputs:

```txt

?- test_correct.
Middle 3 digits of 123        : [1,2,3]
Middle 3 digits of 12345      : [2,3,4]
Middle 3 digits of 1234567    : [3,4,5]
Middle 3 digits of 987654321  : [6,5,4]
Middle 3 digits of 10001      : [0,0,0]
Middle 3 digits of -10001     : [0,0,0]
Middle 3 digits of -123       : [1,2,3]
Middle 3 digits of -100       : [1,0,0]
Middle 3 digits of 100        : [1,0,0]
Middle 3 digits of -12345     : [2,3,4]
true.

```


Errors


```txt

?- middle_3_digits(1, S).
ERROR: Domain error: `at least 3 digits' expected, found `1'
?- middle_3_digits(2, S).
ERROR: Domain error: `at least 3 digits' expected, found `2'
?- middle_3_digits(-1, S).
ERROR: Domain error: `at least 3 digits' expected, found `-1'
?- middle_3_digits(-10, S).
ERROR: Domain error: `at least 3 digits' expected, found `-10'
?- middle_3_digits(2002, S).
ERROR: Domain error: `odd number of digits' expected, found `2002'
?- middle_3_digits(-2002, S).
ERROR: Domain error: `odd number of digits' expected, found `-2002'
?- middle_3_digits(0, S).
ERROR: Domain error: `at least 3 digits' expected, found `0'

```



## Python


### Procedural


```python>>>
 def middle_three_digits(i):
	s = str(abs(i))
	length = len(s)
	assert length >= 3 and length % 2 == 1, "Need odd and >= 3 digits"
	mid = length // 2
	return s[mid-1:mid+2]

>>> passing = [123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345]
>>> failing = [1, 2, -1, -10, 2002, -2002, 0]
>>> for x in passing + failing:
	try:
		answer = middle_three_digits(x)
	except AssertionError as error:
		answer = error
	print("middle_three_digits(%s) returned: %r" % (x, answer))

	
middle_three_digits(123) returned: '123'
middle_three_digits(12345) returned: '234'
middle_three_digits(1234567) returned: '345'
middle_three_digits(987654321) returned: '654'
middle_three_digits(10001) returned: '000'
middle_three_digits(-10001) returned: '000'
middle_three_digits(-123) returned: '123'
middle_three_digits(-100) returned: '100'
middle_three_digits(100) returned: '100'
middle_three_digits(-12345) returned: '234'
middle_three_digits(1) returned: AssertionError('Need odd and >= 3 digits',)
middle_three_digits(2) returned: AssertionError('Need odd and >= 3 digits',)
middle_three_digits(-1) returned: AssertionError('Need odd and >= 3 digits',)
middle_three_digits(-10) returned: AssertionError('Need odd and >= 3 digits',)
middle_three_digits(2002) returned: AssertionError('Need odd and >= 3 digits',)
middle_three_digits(-2002) returned: AssertionError('Need odd and >= 3 digits',)
middle_three_digits(0) returned: AssertionError('Need odd and >= 3 digits',)
>>> 
```



### Composition of pure functions

Using a composable option type as an alternative to error handling:
{{Trans|Haskell}}

```python
'''Middle 3 digits'''


# mid3digits :: Int -> Either String String
def mid3digits(n):
    '''Either the middle three digits,
       or an explanatory string.'''
    m = abs(n)
    s = str(m)
    return Left('Less than 3 digits') if (100 > m) else (
        Left('Even digit count') if even(len(s)) else Right(
            s[(len(s) - 3) // 2:][0:3]
        )
    )


# TEST ----------------------------------------------------
def main():
    '''Test'''

    def bracketed(x):
        return '(' + str(x) + ')'

    print(
        tabulated('Middle three digits, where defined:\n')(str)(
            either(bracketed)(str)
        )(mid3digits)([
            123, 12345, 1234567, 987654321, 10001, -10001, -123,
            -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0
        ])
    )


# GENERIC -------------------------------------------------

# Left :: a -> Either a b
def Left(x):
    '''Constructor for an empty Either (option type) value
       with an associated string.'''
    return {'type': 'Either', 'Right': None, 'Left': x}


# Right :: b -> Either a b
def Right(x):
    '''Constructor for a populated Either (option type) value'''
    return {'type': 'Either', 'Left': None, 'Right': x}


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Function composition.'''
    return lambda f: lambda x: g(f(x))


# either :: (a -> c) -> (b -> c) -> Either a b -> c
def either(fl):
    '''The application of fl to e if e is a Left value,
       or the application of fr to e if e is a Right value.'''
    return lambda fr: lambda e: fl(e['Left']) if (
        None is e['Right']
    ) else fr(e['Right'])


# even :: Int -> Bool
def even(x):
    '''Is x even ?'''
    return 0 == x % 2


# tabulated :: String -> (b -> String) -> (a -> b) -> [a] -> String
def tabulated(s):
    '''Heading -> x display function -> fx display function ->
                f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(str), xs))
        return s + '\n' + '\n'.join(
            xShow(x).rjust(w, ' ') + ' -> ' + fxShow(f(x)) for x in xs
        )
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Middle three digits, where defined:

      123 -> 123
    12345 -> 234
  1234567 -> 345
987654321 -> 654
    10001 -> 000
   -10001 -> 000
     -123 -> 123
     -100 -> 100
      100 -> 100
   -12345 -> 234
        1 -> (Less than 3 digits)
        2 -> (Less than 3 digits)
       -1 -> (Less than 3 digits)
      -10 -> (Less than 3 digits)
     2002 -> (Even digit count)
    -2002 -> (Even digit count)
        0 -> (Less than 3 digits)
```



## Racket


```scheme
#lang racket
(define (middle x)
  (cond
    [(negative? x) (middle (- x))]
    [(< x 100)     "error: number too small"]
    [else 
     (define s (number->string x))
     (define l (string-length s))
     (cond [(even? l) "error: number has even length"]
           [else (define i (quotient l 2)) 
                 (substring s (- i 1) (+ i 2))])]))

(map middle (list 123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345))
(map middle (list 1 2 -1 -10 2002 -2002 0))
```

The output:

```scheme
'("123" "234" "345" "654" "000" "000" "123" "100" "100" "234")
'("error: number too small" "error: number too small" "error: number too small" "error: number too small"
  "error: number has even length" "error: number has even length" "error: number too small")
```



## REXX


### version 1


```rexx
/* REXX ***************************************************************
* 03.02.2013 Walter Pachl
* 19.04.2013 mid 3 is now a function returning the middle 3 digits
*            or an error indication
**********************************************************************/
sl='123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345',
   '2 -1 -10 2002 -2002 0 abc 1e3 -17e-3'
Do While sl<>''                        /* loop through test values   */
  Parse Var sl s sl                    /* pick next value            */
  Say left(s,12) '->' mid3(s)          /* test it                    */
  End
Exit

mid3: Procedure
Parse arg d                            /* take the argument          */
Select                                 /* first test for valid input */
  When datatype(d)<>'NUM'   Then  Return 'not a number'
  When pos('E',translate(d))>0 Then  Return 'not just digits'
  When length(abs(d))<3     Then  Return 'less than 3 digits'
  When length(abs(d))//2<>1 Then  Return 'not an odd number of digits'
  Otherwise Do                         /* input is ok                */
    dx=abs(d)                          /* get rid of optional sign   */
    ld=length(dx)                      /* length of digit string     */
    z=(ld-3)/2                         /* number of digits to cut    */
    res=substr(dx,z+1,3)               /* get middle 3 digits        */
    End
  End
  Return res
```

Output:

```txt

123          -> 123
12345        -> 234
1234567      -> 345
987654321    -> 654
10001        -> 000
-10001       -> 000
-123         -> 123
-100         -> 100
100          -> 100
-12345       -> 234
2            -> less than 3 digits
-1           -> less than 3 digits
-10          -> less than 3 digits
2002         -> not an odd number of digits
-2002        -> not an odd number of digits
0            -> less than 3 digits
abc          -> not a number
1e3          -> not just digits
-17e-3       -> not just digits

```



### version 2

A premise:   '''12.3e2'''   is an integer   (regardless of how it's displayed). 

So is the value of a   '''googol'''   and a  '''googolplex'''. 


This REXX version is limited to numbers whose length is   <big>≤</big>   100,000   decimal digits.

(The decimal digits limit is defined via the   '''numeric digits'''   statement in the first line of the subroutine/procedure.)

```rexx
/*REXX program returns the  three middle digits  of a decimal number  (or an error msg).*/
n= '123    12345   1234567    987654321   10001    -10001    -123   -100    100   -12345',
   '+123   0123    2     -1    -10    2002     -2002   0   abc   1e3   -17e-3   1234567.',
    1237654.00     1234567890123456789012345678901234567890123456789012345678901234567

       do j=1  for words(n);   #=word(n,j)       /* [↓]  format number for pretty output*/
       say 'middle 3 digits of'    right(#, max(15, length(#) ) )     "──►"     middle3(#)
       end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
middle3: procedure;  parse arg x;      numeric digits 1e6;           er='    ***error*** '
         if  datatype(x,'N')  then x=abs(x)/1;  L=length(x)  /*use abs value; get length*/
         if \datatype(x,'W')  then return  er  "argument isn't an integer."
         if L<3               then return  er  "argument is less than three digits."
         if L//2==0           then return  er  "argument isn't an odd number of digits."
                                   return  substr(x, (L-3)%2+1, 3)
```

'''output''' 

```txt

middle 3 digits of             123 ──► 123
middle 3 digits of           12345 ──► 234
middle 3 digits of         1234567 ──► 345
middle 3 digits of       987654321 ──► 654
middle 3 digits of           10001 ──► 000
middle 3 digits of          -10001 ──► 000
middle 3 digits of            -123 ──► 123
middle 3 digits of            -100 ──► 100
middle 3 digits of             100 ──► 100
middle 3 digits of          -12345 ──► 234
middle 3 digits of            +123 ──► 123
middle 3 digits of            0123 ──► 123
middle 3 digits of               2 ──►     ***error***  argument is less than three digits.
middle 3 digits of              -1 ──►     ***error***  argument is less than three digits.
middle 3 digits of             -10 ──►     ***error***  argument is less than three digits.
middle 3 digits of            2002 ──►     ***error***  argument isn't an odd number of digits.
middle 3 digits of           -2002 ──►     ***error***  argument isn't an odd number of digits.
middle 3 digits of               0 ──►     ***error***  argument is less than three digits.
middle 3 digits of             abc ──►     ***error***  argument isn't an integer.
middle 3 digits of             1e3 ──►     ***error***  argument isn't an odd number of digits.
middle 3 digits of          -17e-3 ──►     ***error***  argument isn't an integer.
middle 3 digits of        1234567. ──► 345
middle 3 digits of      1237654.00 ──► 376
middle 3 digits of 1234567890123456789012345678901234567890123456789012345678901234567 ──► 345

```



## Ring


```ring

n = 1234567
middle(n)

func middle nr
     mnr = floor(len(string(nr))/2)
     lennr = len(string(nr))
     if lennr = 3 see "" + nr + nl
     but lennr < 3 see "Number must have at least three digits"
     but lennr%2=0 see "Number must have an odd number of digits"
     else cnr = substr(string(nr),mnr,3) see cnr + nl ok 

```



## Ruby



```ruby
def middle_three_digits(num)
  # minus sign doesn't factor into digit count,
  # and calling #abs acts as a duck-type assertion
  num = num.abs
  
  # convert to string and find length
  length = (str = num.to_s).length
  
  # check validity
  raise ArgumentError, "Number must have at least three digits" if length < 3
  raise ArgumentError, "Number must have an odd number of digits" if length.even?
  
  return str[length/2 - 1, 3].to_i
end
```


Testing program:


```ruby
samples = [ 
  123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345,
  1, 2, -1, -10, 2002, -2002, 0 
]

left_column_width = samples.map { |n| n.to_s.length }.max
samples.each do |n|
  print "%#{left_column_width}d: " % n
  begin
    puts "%03d" % middle_three_digits(n)
  rescue ArgumentError => e
    puts e
  end
end
```

{{out}}

```txt

      123: 123
    12345: 234
  1234567: 345
987654321: 654
    10001: 000
   -10001: 000
     -123: 123
     -100: 100
      100: 100
   -12345: 234
        1: Number must have at least three digits
        2: Number must have at least three digits
       -1: Number must have at least three digits
      -10: Number must have at least three digits
     2002: Number must have an odd number of digits
    -2002: Number must have an odd number of digits
        0: Number must have at least three digits

```



## Rust


```rust
fn middle_three_digits(x: i32) -> Result<String, String> {
    let s: String = x.abs().to_string();
    let len = s.len();
    if len < 3 {
        Err("Too short".into())
    } else if len % 2 == 0 {
        Err("Even number of digits".into())
    } else {
        Ok(s[len/2 - 1 .. len/2 + 2].to_owned())
    }
}

fn print_result(x: i32) {
    print!("middle_three_digits({}) returned: ", x);
    match middle_three_digits(x) {
        Ok(s) => println!("Success, {}", s),
        Err(s) => println!("Failure, {}", s)
    }
}

fn main() {
    let passing = [123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345];
    let failing = [1, 2, -1, -10, 2002, -2002, 0];
    for i in passing.iter() {
        print_result(*i);
    }
    for i in failing.iter() {
        print_result(*i);
    }
}
```


{{Out}}


```txt
middle_three_digits(123) returned: Success, 123
middle_three_digits(12345) returned: Success, 234
middle_three_digits(1234567) returned: Success, 345
middle_three_digits(987654321) returned: Success, 654
middle_three_digits(10001) returned: Success, 000
middle_three_digits(-10001) returned: Success, 000
middle_three_digits(-123) returned: Success, 123
middle_three_digits(-100) returned: Success, 100
middle_three_digits(100) returned: Success, 100
middle_three_digits(-12345) returned: Success, 234
middle_three_digits(1) returned: Failure, Too short
middle_three_digits(2) returned: Failure, Too short
middle_three_digits(-1) returned: Failure, Too short
middle_three_digits(-10) returned: Failure, Too short
middle_three_digits(2002) returned: Failure, Even number of digits
middle_three_digits(-2002) returned: Failure, Even number of digits
middle_three_digits(0) returned: Failure, Too short
```


=={{header|S-lang}}==

```txt
Educational Notes:
1) Array-style string manipulations (e.g. as = as[[1:]] ) always use bytes,
   not UTF-8 characters.  As digits and '-' are all ASCII, all is copacetic.
2) The task doesn't require 64-bit integer support, but I added it, dependant
   on whether the S-Lang library compile supports them, which it generally
   does.  [Main exception would be the current downloadable/installable MS-Win
   version of the Jed editor.]   
3) S-Lang functions peel arguments off right-to-left, opposite of many
   languages.  To align the order with other solutions, I enclosed the
   parameters in a list.  One alternative would have been for m3() to call
   _stk_reverse(_NARGS), then pop each arg directly off the stack.  Or of
   course just list the numbers backwards.

```

<lang S-lang>
define m3(i)
{
    variable s = string(i), sabs = s, l;
    
    if (sabs[0] == '-')
        sabs = sabs[[1:]];
    l = strlen(sabs);
 
    if (l < 3)
        print(sprintf("%s doesn't have enough digits", s));
    else if (l & 1) {
        variable st = (l-3) >> 1;
        print(sprintf("%13s: %s", s, sabs[[st:st+2]]));
    }
    else
        print(sprintf("%s has an even number of digits", s));
}
 
define middle_3(lst)
{
    foreach (lst)
        m3( () );
}

middle_3( { 123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345,
#ifexists Int64_Type
         6644221335577ll, -11122588999ll,
#endif
         1, 2, -1, -10, 2002, -2002, 0 } );

```

{{out}}
```txt

"          123: 123"
"        12345: 234"
"      1234567: 345"
"    987654321: 654"
"        10001: 000"
"       -10001: 000"
"         -123: 123"
"         -100: 100"
"          100: 100"
"       -12345: 234"
"6644221335577: 213"
" -11122588999: 258"
"1 doesn't have enough digits"
"2 doesn't have enough digits"
"-1 doesn't have enough digits"
"-10 doesn't have enough digits"
"2002 has an even number of digits"
"-2002 has an even number of digits"
"0 doesn't have enough digits"

```



## Scala


```scala
/** 
 * Optionally return the middle three digits of an integer.
 *
 * @example List(123,12345,-789,1234,12) flatMap (middleThree(_)), returns: List(123, 234, 789)
 */
def middleThree( s:Int ) : Option[Int] = s.abs.toString match {
  case v if v.length % 2 == 0   => None   // Middle three is undefined for even lengths
  case v if v.length < 3        => None
  case v                        => 			
    val i = (v.length / 2) - 1
    Some( v.substring(i,i+3).toInt )
}


// A little test...
val intVals = List(123,12345,1234567,987654321,10001,-10001,-123,-100,100,-12345,1,2,-1,-10,2002,-2002,0)

intVals map (middleThree(_)) map { 
  case None => "No middle three" 
  case Some(v) => "%03d".format(v)  // Format the value, force leading zeroes 
} mkString("\n")

```

{{out}}

```txt
123
234
345
654
000
000
123
100
100
234
No middle three
No middle three
No middle three
No middle three
No middle three
No middle three
No middle three

```



## Seed7


```seed7
$include "seed7_05.s7i";

const func string: middle3 (in integer: number) is func
  result
    var string: middle3 is "";
  begin
    middle3 := str(abs(number));
    if not odd(length(middle3)) or length(middle3) < 3 then
      middle3 := "error";
    else
      middle3 := middle3[succ((length(middle3) - 3) div 2) len 3];
    end if;
  end func;

const proc: main is func
  local
    var integer: number is 0;
    var string: mid3 is "";
  begin
    for number range [] (123, 12345, 1234567, 987654321, 10001, -10001, -123,
                         -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0) do
      writeln(number <& ": " <& middle3(number));
    end for;
  end func;
```


{{out}}

```txt

123: 123
12345: 234
1234567: 345
987654321: 654
10001: 000
-10001: 000
-123: 123
-100: 100
100: 100
-12345: 234
1: error
2: error
-1: error
-10: error
2002: error
-2002: error
0: error

```



## Sidef

{{trans|Perl 6}}

```ruby
func middle_three(n) {
  var l = n.len;
  if (l < 3) {
    "#{n} is too short"
  } elsif (l.is_even) {
    "#{n} has an even number of digits"
  } else {
    "The three middle digits of #{n} are: " + n.digits.ft(l-3 / 2, l/2 + 1).join
  }
}

var nums = %n(
    123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345
    1 2 -1 -10 2002 -2002 0
);
nums.each { say middle_three(_) };
```

{{out}}

```txt

The three middle digits of 123 are: 123
The three middle digits of 12345 are: 234
The three middle digits of 1234567 are: 345
The three middle digits of 987654321 are: 654
The three middle digits of 10001 are: 000
The three middle digits of -10001 are: 000
The three middle digits of -123 are: 123
The three middle digits of -100 are: 100
The three middle digits of 100 are: 100
The three middle digits of -12345 are: 234
1 is too short
2 is too short
-1 is too short
-10 is too short
2002 has an even number of digits
-2002 has an even number of digits
0 is too short

```



## SQL


```sql
;WITH DATA 
     AS (SELECT CAST(ABS(NUMBER) AS NVARCHAR(MAX))      charNum, 
                NUMBER, 
                LEN(CAST(ABS(NUMBER) AS NVARCHAR(MAX))) LcharNum 
         FROM   TABLE1) 
SELECT CASE 
         WHEN ( LCHARNUM >= 3 
                AND LCHARNUM % 2 = 1 ) THEN SUBSTRING(CHARNUM, LCHARNUM / 2, 3) 
         ELSE 'Error!' 
       END    Output, 
       NUMBER Input 
FROM   DATA 
```



```txt

| OUTPUT |     INPUT |
|--------|-----------|
|    123 |       123 |
|    234 |     12345 |
|    345 |   1234567 |
|    654 | 987654321 |
|    000 |     10001 |
|    000 |    -10001 |
|    123 |      -123 |
|    100 |      -100 |
|    100 |       100 |
|    234 |    -12345 |

```



```txt

| OUTPUT | INPUT |
|--------|-------|
| Error! |     1 |
| Error! |     2 |
| Error! |    -1 |
| Error! |   -10 |
| Error! |  2002 |
| Error! | -2002 |
| Error! |     0 |
		

```


## Swift


```swift
var num:Int = \\enter your number here
if num<0{num = -num}
var numArray:[Int]=[]
while num>0{
	var temp:Int=num%10
	numArray.append(temp)
	num=num/10
}
var i:Int=numArray.count
if i<3||i%2==0{
	print("Invalid Input")
}
else{
	i=i/2
	print("\(numArray[i+1]),\(numArray[i]),\(numArray[i-1])")
}
```


```txt

      123: 123
    12345: 234
  1234567: 345
987654321: 654
    10001: 000
   -10001: 000
     -123: 123
     -100: 100
      100: 100
   -12345: 234
        1: Invalid Input
        2: Invalid Input
       -1: Invalid Input
      -10: Invalid Input
     2002: Invalid Input
    -2002: Invalid Input
        0: Invalid Input

```




## Tcl


```tcl
proc middleThree n {
    if {$n < 0} {
	set n [expr {-$n}]
    }
    set idx [expr {[string length $n] - 2}]
    if {$idx % 2 == 0} {
	error "no middle three digits: input is of even length"
    }
    if {$idx < 1} {
	error "no middle three digits: insufficient digits"
    }
    set idx [expr {$idx / 2}]
    string range $n $idx [expr {$idx+2}]
}
```

Demonstrating:

```tcl
foreach n {
    123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345
    1 2 -1 -10 2002 -2002 0
} {
    if {[catch {
	set mid [middleThree $n]
    } msg]} then {
	puts "error for ${n}: $msg"
    } else {
	puts "found for ${n}: $mid"
    }
}
```

{{out}}

```txt

found for 123: 123
found for 12345: 234
found for 1234567: 345
found for 987654321: 654
found for 10001: 000
found for -10001: 000
found for -123: 123
found for -100: 100
found for 100: 100
found for -12345: 234
error for 1: no middle three digits: insufficient digits
error for 2: no middle three digits: insufficient digits
error for -1: no middle three digits: insufficient digits
error for -10: no middle three digits: input is of even length
error for 2002: no middle three digits: input is of even length
error for -2002: no middle three digits: input is of even length
error for 0: no middle three digits: insufficient digits

```



## UNIX Shell

{{works with|Bourne Again Shell}}
{{works with|Korn Shell|93}}
{{works with|Z Shell}}


```bash
function middle3digits 
{ 
  typeset -i n="${1#-}"
  typeset -i l=${#n}
  if (( l < 3 )); then
    echo >&2 "$1 has less than 3 digits"
    return 1
  elif (( l % 2 == 0 )); then
    echo >&2 "$1 has an even number of digits"
    return 1
  else
    echo ${n:$((l/2-1)):3}
    return 0
  fi
}

# test
testdata=(123 12345 1234567 987654321 10001 -10001 -123 -100 100 -12345 1 2 -1
          -10 2002 -2002 0)
for n in ${testdata[@]}; do
  printf "%10d: " $n
  middle3digits "$n"
done
```


Output: 
```txt
       123: 123
     12345: 234
   1234567: 345
 987654321: 654
     10001: 000
    -10001: 000
      -123: 123
      -100: 100
       100: 100
    -12345: 234
         1: 1 has less than 3 digits
         2: 2 has less than 3 digits
        -1: -1 has less than 3 digits
       -10: -10 has less than 3 digits
      2002: 2002 has an even number of digits
     -2002: -2002 has an even number of digits
         0: 0 has less than 3 digits
```



## VBA



```vb

Option Explicit

Sub Main_Middle_three_digits()
Dim Numbers, i&
    Numbers = Array(123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, _
    100, -12345, 1, 2, -1, -10, 2002, -2002, 0)
    For i = 0 To 16
        Debug.Print Numbers(i) & " Return : " & Middle3digits(CStr(Numbers(i)))
    Next
End Sub

Function Middle3digits(strNb As String) As String
    If Left(strNb, 1) = "-" Then strNb = Right(strNb, Len(strNb) - 1)
    If Len(strNb) < 3 Then
        Middle3digits = "Error ! Number of digits must be >= 3"
    ElseIf Len(strNb) Mod 2 = 0 Then
        Middle3digits = "Error ! Number of digits must be odd"
    Else
        Middle3digits = Mid(strNb, 1 + (Len(strNb) - 3) / 2, 3)
    End If
End Function

```

{{out}}

```txt
123 Return : 123
12345 Return : 234
1234567 Return : 345
987654321 Return : 654
10001 Return : 000
-10001 Return : 000
-123 Return : 123
-100 Return : 100
100 Return : 100
-12345 Return : 234
1 Return : Error ! Number of digits must be >= 3
2 Return : Error ! Number of digits must be >= 3
-1 Return : Error ! Number of digits must be >= 3
-10 Return : Error ! Number of digits must be >= 3
2002 Return : Error ! Number of digits must be odd
-2002 Return : Error ! Number of digits must be odd
0 Return : Error ! Number of digits must be >= 3
```



## VBScript


```vb
'http://rosettacode.org/wiki/Middle_three_digits

Function mid3n(n)
	'Remove the number's sign.
	n = CStr(Abs(n))
	If Len(n) < 3 Or Len(n) Mod 2 = 0 Then
		mid3n = "Invalid: Either the length of n < 3 or an even number."
	ElseIf Round(Len(n)/2) > Len(n)/2 Then
		mid3n = Mid(n,Round(Len(n)/2)-1,3)
	Else
		mid3n = Mid(n,Round(Len(n)/2),3)
	End If
End Function

'Calling the function.
arrn = Array(123,12345,1234567,987654321,10001,-10001,-123,-100,100,-12345,_
			1,2,-1,-10,2002,-2002,0)
For Each n In arrn
	WScript.StdOut.Write n & ": " & mid3n(n)
	WScript.StdOut.WriteLine
Next
```

{{Out}}

```txt

123: 123
12345: 234
1234567: 345
987654321: 654
10001: 000
-10001: 000
-123: 123
-100: 100
100: 100
-12345: 234
1: Invalid: Either the length of n < 3 or an even number.
2: Invalid: Either the length of n < 3 or an even number.
-1: Invalid: Either the length of n < 3 or an even number.
-10: Invalid: Either the length of n < 3 or an even number.
2002: Invalid: Either the length of n < 3 or an even number.
-2002: Invalid: Either the length of n < 3 or an even number.
0: Invalid: Either the length of n < 3 or an even number.

```



## Vedit macro language


```vedit
do {
    #1 = Get_Num("Enter a number, or 0 to stop: ", STATLINE)
    Ins_Text("Input: ") Num_Ins(#1, COUNT, 10)
    Call("MIDDLE_3_DIGITS")
    Ins_Text("  Result: ") Reg_Ins(10) Ins_Newline
    Update()
} while (#1);
Return

// Find middle 3 digits of a number
//  in: #1 = numeric value
// out: @10 = the result, or error text
//
:MIDDLE_3_DIGITS:
Buf_Switch(Buf_Free)
Num_Ins(abs(#1), LEFT+NOCR)	// the input value as string
#2 = Cur_Col-1			// #2 = number of digits
if (#2 < 3) {
    Reg_Set(10, "Too few digits!")
} else {
    if ((#2 & 1) == 0) {
	Reg_Set(10, "Not odd number of digits!")
    } else {
	Goto_Pos((#2-3)/2)
	Reg_Copy_Block(10, Cur_Pos, Cur_Pos+3)
    }
}
Buf_Quit(OK)
Return 
```


Output:

```txt
Input:        123  Result: 123
Input:      12345  Result: 234
Input:    1234567  Result: 345
Input:  987654321  Result: 654
Input:      10001  Result: 000
Input:     -10001  Result: 000
Input:       -123  Result: 123
Input:       -100  Result: 100
Input:        100  Result: 100
Input:     -12345  Result: 234
Input:          1  Result: Too few digits!
Input:          2  Result: Too few digits!
Input:         -1  Result: Too few digits!
Input:        -10  Result: Too few digits!
Input:       2002  Result: Not odd number of digits!
Input:      -2002  Result: Not odd number of digits!
Input:          0  Result: Too few digits! 
```



## Wart



```wart
def (mid3 n)
  withs (digits  (with outstring  # itoa
                   (pr abs.n))
         max  len.digits
         mid  (int max/2))
    if (and odd?.max (max >= 3))
      (digits mid-1 mid+2)
```


'''Output'''

```txt
mid3.123
=> 123
mid3.12345
=> 234
mid3.1234567
=> 345
mid3.987654321
=> 654
mid3.10001
=> 000
mid3 -10001
=> 000
(mid3 -123)
=> 123
(mid3 -100)
=> 100
(mid3 100)
=> 100
(mid3 -12345)
=> 234

# Errors
mid3.1
=> nil
mid3.2
=> nil
mid3 -1
=> nil
mid3 -10
=> nil
mid3 2002
=> nil
mid3 -2002
=> nil
(mid3 0)
=> nil
```



## XPL0


```XPL0
include c:\cxpl\stdlib;

func Mid3Digits(I);     \Return the middle three digits of I
int  I;
int  Len, Mid;
char S(10);
[ItoA(abs(I), S);
Len:= StrLen(S);
if Len<3 or (Len&1)=0 then return "Must be 3, 5, 7 or 9 digits";
Mid:= Len/2;
S:= S + Mid - 1;
S(2):= S(2) ! $80;      \terminate string
return S;               \WARNING: very temporary
];

int  Passing, Failing, X;
[Passing:= [123, 12345, 1234567, 987654321, 10001, -10001, -123, -100, 100, -12345];
 Failing:= [1, 2, -1, -10, 2002, -2002, 0];     \WARNING: nasty trick
for X:= 0 to 16 do
    [Text(0, "Middle three digits of ");  IntOut(0, Passing(X));
     Text(0, " returned: ");
     Text(0, Mid3Digits(Passing(X)));  CrLf(0);
    ];
]
```


{{out}}

```txt

Middle three digits of 123 returned: 123
Middle three digits of 12345 returned: 234
Middle three digits of 1234567 returned: 345
Middle three digits of 987654321 returned: 654
Middle three digits of 10001 returned: 000
Middle three digits of -10001 returned: 000
Middle three digits of -123 returned: 123
Middle three digits of -100 returned: 100
Middle three digits of 100 returned: 100
Middle three digits of -12345 returned: 234
Middle three digits of 1 returned: Must be 3, 5, 7 or 9 digits
Middle three digits of 2 returned: Must be 3, 5, 7 or 9 digits
Middle three digits of -1 returned: Must be 3, 5, 7 or 9 digits
Middle three digits of -10 returned: Must be 3, 5, 7 or 9 digits
Middle three digits of 2002 returned: Must be 3, 5, 7 or 9 digits
Middle three digits of -2002 returned: Must be 3, 5, 7 or 9 digits
Middle three digits of 0 returned: Must be 3, 5, 7 or 9 digits

```



## zkl


```zkl
fcn middle(ns){
   ns.apply("toString").apply('-("-"))
   .apply(fcn(n){nl:=n.len(); 
      if(nl<3 or nl.isEven) return(False);
      n[(nl-3)/2,3] : "%03d".fmt(_)
   })
}
middle(T(123,12345,1234567,987654321,10001,-10001,-123,-100,100,-12345)).println()
middle(T(1, 2, -1, -10, 2002, -2002, 0)).println();
```

{{out}}

```txt

L("123","234","345","654","000","000","123","100","100","234")
L(False,False,False,False,False,False,False)

```

Algorithm is convert each number into a string, remove any "-", reject strings less than 3 characters or are even in length, return middle three characters of remainder.


{{omit from|GUISS}}
