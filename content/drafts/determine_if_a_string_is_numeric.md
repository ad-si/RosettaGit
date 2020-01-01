+++
title = "Determine if a string is numeric"
description = ""
date = 2019-10-08T18:25:49Z
aliases = []
[extra]
id = 1624
[taxonomies]
categories = []
tags = []
+++

{{task|Text processing}}
[[Category:Simple]]

;Task:
Create a boolean function which takes in a string and tells whether it is a numeric string (floating point and negative numbers included) in the syntax the language uses for numeric literals or numbers converted from strings.





## 8th


```Forth>: number? >n >kind ns:n n:= ;</lang


## ActionScript


```actionscript
public function isNumeric(num:String):Boolean
{
    return !isNaN(parseInt(num));
}
```



## Ada

The first file is the package interface containing the declaration of the Is_Numeric function.

```ada
package Numeric_Tests is
   function Is_Numeric (Item : in String) return Boolean;
end Numeric_Tests;
```

The second file is the package body containing the implementation of the Is_Numeric function.

```ada
package body Numeric_Tests is
   function Is_Numeric (Item : in String) return Boolean is
      Dummy : Float;
   begin
      Dummy := Float'Value (Item);
      return True;
   exception
      when others =>
         return False;
   end Is_Numeric;
end Numeric_Tests;
```

The last file shows how the Is_Numeric function can be called.

```ada
with Ada.Text_Io; use Ada.Text_Io;
with Numeric_Tests; use Numeric_Tests;

procedure Is_Numeric_Test is
   S1 : String := "152";
   S2 : String := "-3.1415926";
   S3 : String := "Foo123";
begin
   Put_Line(S1 & " results in " & Boolean'Image(Is_Numeric(S1)));
   Put_Line(S2 & " results in " & Boolean'Image(Is_Numeric(S2)));
   Put_Line(S3 & " results in " & Boolean'Image(Is_Numeric(S3)));
end Is_Numeric_Test;
```

{{out}}

```txt

 152 results in TRUE
 -3.1415926 results in TRUE
 Foo123 results in FALSE

```



## Aime


```aime
integer
is_numeric(text s)
{
    return !trap_q(alpha, s, 0);
}

integer
main(void)
{
    if (!is_numeric("8192&*")) {
        o_text("Not numeric.\n");
    }
    if (is_numeric("8192")) {
        o_text("Numeric.\n");
    }

    return 0;
}
```



## ALGOL 68

{{trans|Ada}}

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}

```algol68
PROC is numeric = (REF STRING string) BOOL: (
  BOOL out := TRUE;
  PROC call back false = (REF FILE f)BOOL: (out:= FALSE; TRUE);

  FILE memory;
  associate(memory, string);
  on value error(memory, call back false);
  on logical file end(memory, call back false);

  UNION (INT, REAL, COMPL) numeric:=0.0;
  # use a FORMAT pattern instead of a regular expression #
  getf(memory, ($gl$, numeric));
  out
);

test:(
   STRING
     s1 := "152",
     s2 := "-3.1415926",
     s3 := "Foo123";
   print((
     s1, " results in ", is numeric(s1), new line,
     s2, " results in ", is numeric(s2), new line,
     s3, " results in ", is numeric(s3), new line
   ))
)

```

{{out}}

```txt

152 results in T
-3.1415926 results in T
Foo123 results in F

```



## ALGOL W


```algolw
begin

    % determnines whether the string contains an integer, real or imaginary  %
    % number. Returns true if it does, false otherwise                       %
    logical procedure isNumeric( string(32) value text ) ;
        begin

            logical ok;
            % the "number" cannot be blank                                   %
            ok := ( text not = " " );
            if ok then begin

                % there is at least one non-blank character                  %
                % must have either an integer or real/immaginary number      %
                % integer: [+|-]digit-sequence                               %
                % real:    [+|-][digit-sequence].digit-sequence['integer][L] %
                % or:      [+|-]digit-sequence[.[digit-sequence]]'integer[L] %
                % imaginary:                                                 %
                %          [+|-][digit-sequence].digit-sequence['integer][L]I%
                % or:      [+|-]digit-sequence[.[digit-sequence]]'integer[L]I%
                % The "I" at the end of an imaginary number can appear       %
                % before or after the "L" (which indicates a long number)    %
                % the "I" and "L" can be in either case                      %

                procedure nextChar ; charPos := charPos + 1;
                logical procedure have( string(1) value ch ) ;
                    ( charPos <= maxChar and text(charPos//1) = ch ) ;

                logical procedure haveDigit ;
                    ( charPos <= maxChar and text(charPos//1) >= "0" and text(charPos//1) <= "9" ) ;


                integer charPos, maxChar;
                logical hadDigits, isReal;
                charPos   :=  0;
                maxChar   := 31;
                hadDigits := false;
                isReal    := false;

                % skip trailing spaces                                       %
                while maxChar > 0 and text(maxChar//1) = " " do maxChar := maxChar - 1;
                % skip leading spacesx                                       %
                while have( " " ) do nextChar;

                % skip optional sign                                         %
                if have( "+" ) or have( "-" ) then nextChar;

                if      haveDigit then begin
                    % have a digit sequence                                  %
                    hadDigits := true;
                    while haveDigit do nextChar
                end if_have_sign ;

                if have( "." ) then begin
                    % real or imaginary number                               %
                    nextChar;
                    isReal    := true;
                    hadDigits := hadDigits or haveDigit;
                    while haveDigit do nextChar
                end if_have_point ;

                % should have had some digits                                %
                ok := hadDigits;

                if ok and have( "'" ) then begin
                    % the number has an exponent                             %
                    isReal := true;
                    nextChar;
                    % skip optional sign                                     %
                    if have( "+" ) or have( "-" ) then nextChar;
                    % must have a digit sequence                             %
                    ok := haveDigit;
                    while haveDigit do nextChar;
                end if_ok_and_have_exponent ;

                % if it is a real number, there could be L/I suffixes        %
                if ok and isReal then begin
                    integer LCount, ICount;
                    LCount := 0;
                    ICount := 0;
                    while have( "L" ) or have( "l" ) or have( "I" ) or have( "i" ) do begin
                        if have( "L" ) or have( "l" )
                        then LCount := LCount + 1
                        else ICount := ICount + 1;
                        nextChar
                    end while_have_L_or_I ;
                    % there can be at most one L and at most 1 I             %
                    ok := ( LCount < 2 and ICount < 2 )
                end if_ok_and_isReal ;

                % must now be at the end if the number                       %
                ok := ok and charPos >= maxChar

            end if_ok ;

            ok
        end isNumeric ;


    % test the isNumeric procedure                                           %
    procedure testIsNumeric( string(32) value n
                           ; logical    value expectedResult
                           ) ;
        begin
            logical actualResult;
            actualResult := isNumeric( n );
            write( s_w := 0
                 , """", n, """ is "
                 , if actualResult then "" else "not "
                 , "numeric "
                 , if actualResult = expectedResult then "" else " NOT "
                 , "as expected"
                 )
        end testIsNumeric ;


    testIsNumeric(          "", false );
    testIsNumeric(         "b", false );
    testIsNumeric(         ".", false );
    testIsNumeric(       ".'3", false );
    testIsNumeric(       "3.'", false );
    testIsNumeric(    "0.0z44", false );
    testIsNumeric(      "-1IL", false );
    testIsNumeric( "4.5'23ILL", false );

    write( "---------" );

    testIsNumeric(        "-1", true  );
    testIsNumeric(    " +.345", true  );
    testIsNumeric(   "4.5'23I", true  );
    testIsNumeric(    "-5'+3i", true  );
    testIsNumeric(    "-5'-3l", true  );
    testIsNumeric(  " -.345LI", true  );

end.
```

{{out}}

```txt

"                                " is not numeric as expected
"b                               " is not numeric as expected
".                               " is not numeric as expected
".'3                             " is not numeric as expected
"3.'                             " is not numeric as expected
"0.0z44                          " is not numeric as expected
"-1IL                            " is not numeric as expected
"4.5'23ILL                       " is not numeric as expected
---------
"-1                              " is numeric as expected
" +.345                          " is numeric as expected
"4.5'23I                         " is numeric as expected
"-5'+3i                          " is numeric as expected
"-5'-3l                          " is numeric as expected
" -.345LI                        " is numeric as expected

```



## Apex

The isNumeric() method is part of the Apex String Class. Note that it will return false if applied to a decimal, because the '.' character is not a Unicode digit.

```Apex

String numericString = '123456';
String partlyNumericString = '123DMS';
String decimalString = '123.456';

System.debug(numericString.isNumeric()); // this will be true
System.debug(partlyNumericString.isNumeric()); // this will be false
System.debug(decimalString.isNumeric()); // this will be false
System.debug(decimalString.remove('.').isNumeric()); // this will be true

```



## APL

{{works with|Dyalog APL}}

```apl
      ⊃⎕VFI{w←⍵⋄((w='-')/w)←'¯'⋄w}'152 -3.1415926 Foo123'
1 1 0
```




## AppleScript


```AppleScript

-- isNumString :: String -> Bool
on isNumString(s)
    try
        if class of s is string then
            set c to class of (s as number)
            c is real or c is integer
        else
            false
        end if
    on error
        false
    end try
end isNumString



-- TEST
on run

    map(isNumString, {3, 3.0, 3.5, "3.5", "3E8", "-3.5", "30", "three", three, four})

    --> {false, false, false, true, true, true, true, false, false, false}

end run

-- three :: () -> Int
script three
    3
end script

-- four :: () -> Int
on four()
    4
end four


-- GENERIC FUNCTIONS FOR TEST

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to lambda(item i of xs, i, xs)
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
            property lambda : f
        end script
    end if
end mReturn
```


{{Out}}

```AppleScript
{false, false, false, true, true, true, true, false, false, false}
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program strNumber.s   */

/* Constantes    */
.equ STDIN,  0                           @ Linux input console
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ READ,   3                           @ Linux syscall
.equ WRITE,  4                           @ Linux syscall

.equ BUFFERSIZE,          100


/* Initialized data */
.data
szMessNum: .asciz "Enter number : \n"

szMessError:            .asciz "String is not a number !!!\n"
szMessInteger:          .asciz "String is a integer.\n"
szMessFloat:            .asciz "String is a float.\n"
szMessFloatExp:         .asciz "String is a float with exposant.\n"
szCarriageReturn:       .asciz "\n"

/* UnInitialized data */
.bss
sBuffer:              .skip BUFFERSIZE

/*  code section */
.text
.global main
main:

loop:
    ldr r0,iAdrszMessNum
    bl affichageMess
    mov r0,#STDIN                               @ Linux input console
    ldr r1,iAdrsBuffer                          @ buffer address
    mov r2,#BUFFERSIZE                          @ buffer size
    mov r7, #READ                               @ request to read datas
    swi 0                                       @ call system
    ldr r1,iAdrsBuffer                          @ buffer address
    mov r2,#0                                   @ end of string
    sub r0,#1                                   @ replace character 0xA
    strb r2,[r1,r0]                             @ store byte at the end of input string (r0 contains number of characters)
    ldr r0,iAdrsBuffer
    bl controlNumber                            @ call routine
    cmp r0,#0
    bne 1f
    ldr r0,iAdrszMessError                      @ not a number
    bl affichageMess
    b 5f
1:
    cmp r0,#1
    bne 2f
    ldr r0,iAdrszMessInteger                    @ integer
    bl affichageMess
    b 5f
2:
    cmp r0,#2
    bne 3f
    ldr r0,iAdrszMessFloat                      @ float
    bl affichageMess
    b 5f
3:
    cmp r0,#3
    bne 5f
    ldr r0,iAdrszMessFloatExp                   @ float with exposant
    bl affichageMess
5:
    b loop

100:                                            @ standard end of the program
    mov r0, #0                                  @ return code
    mov r7, #EXIT                               @ request to exit program
    svc 0                                       @ perform system call
iAdrszMessNum:            .int szMessNum
iAdrszMessError:          .int szMessError
iAdrszMessInteger:        .int szMessInteger
iAdrszMessFloat:          .int szMessFloat
iAdrszMessFloatExp:       .int szMessFloatExp
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrsBuffer:              .int sBuffer
/******************************************************************/
/*     control if string is number                          */
/******************************************************************/
/* r0 contains the address of the string */
/* r0 return 0 if not a number       */
/* r0 return 1 if integer   eq 12345 or -12345      */
/* r0 return 2 if float     eq 123.45 or 123,45  or -123,45     */
/* r0 return 3 if float with exposant  eq 123.45E30 or -123,45E-30        */
controlNumber:
    push {r1-r4,lr}                       @ save  registers
    mov r1,#0
    mov r3,#0          @ point counter
1:
    ldrb r2,[r0,r1]
    cmp r2,#0
    beq 5f
    cmp r2,#' '
    addeq r1,#1
    beq   1b
    cmp r2,#'-'                    @ negative ?
    addeq r1,#1
    beq 2f
    cmp r2,#'+'                    @ positive ?
    addeq r1,#1
2:
    ldrb r2,[r0,r1]                @ control space
    cmp r2,#0                      @ end ?
    beq 5f
    cmp r2,#' '
    addeq r1,#1
    beq 2b
3:
    ldrb r2,[r0,r1]
    cmp r2,#0                 @ end ?
    beq 10f
    cmp r2,#'E'               @ exposant ?
    beq 6f
    cmp r2,#'e'               @ exposant ?
    beq 6f
    cmp r2,#'.'               @ point ?
    addeq r3,#1               @ yes increment counter
    addeq r1,#1
    beq 3b
    cmp r2,#','               @ comma ?
    addeq r3,#1               @ yes increment counter
    addeq r1,#1
    beq 3b
    cmp r2,#'0'               @ control digit < 0
    blt 5f
    cmp r2,#'9'               @ control digit > 0
    bgt 5f
    add r1,#1                 @ no error loop digit
    b 3b
5:                            @ error detected
    mov r0,#0
    b 100f
6:    @ float with exposant
    add r1,#1
    ldrb r2,[r0,r1]
    cmp r2,#0             @ end ?
    moveq r0,#0           @ error
    beq 100f
    cmp r2,#'-'           @ negative exposant ?
    addeq r1,#1
    mov r4,#0             @ nombre de chiffres
7:
    ldrb r2,[r0,r1]
    cmp r2,#0             @ end ?
    beq 9f
    cmp r2,#'0'           @ control digit < 0
    blt 8f
    cmp r2,#'9'           @ control digit > 0
    bgt 8f
    add r1,#1
    add r4,#1             @ counter digit
    b 7b
8:
    mov r0,#0
    b 100f
9:
    cmp r4,#0             @ number digit exposant = 0 -> error
    moveq r0,#0           @ erreur
    beq 100f
    cmp r4,#2             @ number digit exposant > 2 -> error
    movgt r0,#0           @ error
    bgt 100f
    mov r0,#3             @ valid float with exposant
    b 100f
10:
    cmp r3,#0
    moveq r0,#1           @ valid integer
    beq 100f
    cmp r3,#1             @ number of point or comma = 1 ?
    moveq r0,#2           @ valid float
    movgt r0,#0           @ error
100:
    pop {r1-r4,lr}                         @ restaur des  2 registres
    bx lr                                        @ return
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                       @ save  registers
    mov r2,#0                                   @ counter length */
1:                                              @ loop length calculation
    ldrb r1,[r0,r2]                             @ read octet start position + index
    cmp r1,#0                                   @ if 0 its over
    addne r2,r2,#1                              @ else add 1 in the length
    bne 1b                                      @ and loop
                                                @ so here r2 contains the length of the message
    mov r1,r0                                   @ address message in r1
    mov r0,#STDOUT                              @ code to write to the standard output Linux
    mov r7, #WRITE                              @ code call system "write"
    svc #0                                      @ call system
    pop {r0,r1,r2,r7,lr}                        @ restaur registers
    bx lr                                       @ return



```



## AutoHotkey

AutoHotkey has no explicitly defined variable types. A variable containing only digits (with an optional decimal point) is automatically interpreted as a number when a math operation or comparison requires it.

```autohotkey
list = 0 .14 -5.2 ten 0xf
Loop, Parse, list, %A_Space%
  MsgBox,% IsNumeric(A_LoopField)
Return

IsNumeric(x) {
  If x is number
    Return, 1
  Else Return, 0
}

;Output: 1  1  1  0  1
```



## AWK

The following function uses the fact that non-numeric strings in AWK are treated as having the value 0 when used in arithmetics, but not in comparison:

```AWK

$ awk 'function isnum(x){return(x==x+0)} BEGIN{print isnum("hello"),isnum("-42")}'

```

{{out}}

```txt

0 1

```



## BaCon


```qbasic
INPUT "Your string: ", s$

IF VAL(s$) = 0 AND s$ <> "0" THEN
    PRINT "Not a number"
ELSE
    PRINT "This is a number"
END IF
```

{{out}}

```txt

user@host $ bacon isnumber.bac
Converting 'isnumber.bac'... done, 7 lines were processed in 0.006 seconds.
Compiling 'isnumber.bac'... cc  -c isnumber.bac.c
cc -o isnumber isnumber.bac.o -lbacon -lm
Done, program 'isnumber' ready.
user@host $ ./isnumber
Your string: 12.3
This is a number
user@host $ ./isnumber
Your string: 12E3
This is a number
user@host $ ./isnumber
Your string: PI
Not a number
user@host $ ./isnumber
Your string: Hello
Not a number

```



## BASIC


```qbasic
10 INPUT "Enter a string";S$:GOSUB 1000
20 IF R THEN PRINT "Is num" ELSE PRINT"Not num"
99 END
1000 T1=VAL(S$):T1$=STR$(T1)
1010 R=T1$=S$ OR T1$=" "+S$
1099 RETURN
```



## Batch File


```dos
set /a a=%arg%+0 >nul
if %a% == 0 (
	if not "%arg%"=="0" (
		echo Non Numeric.
	) else (
		echo Numeric.
	)
) else (
	echo Numeric.
)
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      REPEAT
        READ N$
        IF FN_isanumber(N$) THEN
          PRINT "'" N$ "' is a number"
        ELSE
          PRINT "'" N$ "' is NOT a number"
        ENDIF
      UNTIL N$ = "end"
      END

      DATA "PI", "0123", "-0123", "12.30", "-12.30", "123!", "0"
      DATA "0.0", ".123", "-.123", "12E3", "12E-3", "12+3", "end"

      DEF FN_isanumber(A$)
      ON ERROR LOCAL = FALSE
      IF EVAL("(" + A$ + ")") <> VAL(A$) THEN = FALSE
      IF VAL(A$) <> 0 THEN = TRUE
      IF LEFT$(A$,1) = "0" THEN = TRUE
      = FALSE

```

{{out}}

```txt
'PI' is NOT a number
'0123' is a number
'-0123' is a number
'12.30' is a number
'-12.30' is a number
'123!' is NOT a number
'0' is a number
'0.0' is a number
'.123' is a number
'-.123' is a number
'12E3' is a number
'12E-3' is a number
'12+3' is NOT a number
'end' is NOT a number
```



## Befunge


```befunge
 ~:0\`#v_:"+"-!#v_:"-"-!#v_::"E"-\"e"-*#v_                               v
v       _v#     <        <             0<
>~:0\`#v_>::"0"-0\`\"9"-0`+!#v_:"."-!#v_::"E"-\"e"-*!#v_                 v
^                           $<        >                 > $               v
>~:0\`#v_>::"0"-0\`\"9"-0`+!#v_:"."-!#v_::"E"-\"e"-*!#v_>                v>
^                           $<                        >$~:0\`#v_:"+"-#v_v
v      $_v#                                                     < <   :#<
>~:0\`#v_>::"0"-0\`\"9"-0`+!#v_:"."-!#v_::"E"-\"e"-*!v          0     > v
^                           $<        v                 <    << ^_^#-"-"<
       > "ciremuN">:#,_@             >>#$_"ciremun toN">:#,_@^           <
```



Although only integer inputs are strictly allowed in Befunge, the code tries to adhere to the floating point conventions in other languages when recognising valid numbers.

{{out}}

```txt

'PI' Not numeric
'0123' Numeric
'-0123' Numeric
'12.30' Numeric
'-12.30' Numeric
'123!' Not numeric
'0' Numeric
'0.0' Numeric
'.123' Numeric
'-.123' Numeric
'12E3' Numeric
'12E-3' Numeric
'12+3' Not numeric
'end' Not numeric
'12..34' Not numeric
'12e3.4' Not numeric
'192.168.0.1' Not numeric

```



## Bracmat

To check whether a string is a number, a fraction or an integer, use the patterns <code>#</code>, <code>/</code> and <code>~/#</code> ("not a fraction and yet a number"). In the pattern matching examples below (which can be typed in at the Bracmat prompt) <code>F</code> denotes 'failure' and <code>S</code> denotes 'success'.


```bracmat
43257349578692:/
    F

260780243875083/35587980:/
    S

247/30:~/#
    F

80000000000:~/#
    S
```

Bracmat doesn't do floating point computations (historical reason: the Acorn Risc Machine a.k.a. ARM processor in the Archimedes computer did not have an FPU), but the pattern <code>~/# (|"." (|? 0|`) (|~/#:>0)) (|(E|e) ~/#)</code> recognises string representations of floating point numbers.

```bracmat
@("1.000-4E-10":~/# (|"." (|? 0|`) (|~/#:>0)) (|(E|e) ~/#))
 F

@("1.0004E-54328":~/# (|"." (|? 0|`) (|~/#:>0)) (|(E|e) ~/#))
 S

@("-464641.0004E-54328":~/# (|"." (|? 0|`) (|~/#:>0)) (|(E|e) ~/#))
 S

@("1/2.0004E-10":~/# (|"." (|? 0|`) (|~/#:>0)) (|(E|e) ~/#))
 F

@("1357E-10":~/# (|"." (|? 0|`) (|~/#:>0)) (|(E|e) ~/#))
 S

@("1357e0":~/# (|"." (|? 0|`) (|~/#:>0)) (|(E|e) ~/#))
 S

@("13579":~/# (|"." (|? 0|`) (|~/#:>0)) (|(E|e) ~/#))
 S

@("1.246":~/# (|"." (|? 0|`) (|~/#:>0)) (|(E|e) ~/#))
 S

@("0.0":~/# (|"." (|? 0|`) (|~/#:>0)) (|(E|e) ~/#))
 S

@("0.0000":~/# (|"." (|? 0|`) (|~/#:>0)) (|(E|e) ~/#))
 S
```


To do computations with such "floating point strings" you would have to convert such strings to fractional representations first.

```bracmat
(float2fraction=
  integerPart decimalPart d1 dn exp sign
.   @( !arg
     :   ~/#?integerPart
         ( &0:?decimalPart:?d1:?dn
         |   "."
             [?d1
             (|? 0|`)
             ( &0:?decimalPart
             | ~/#?decimalPart:>0
             )
             [?dn
         )
         ( &0:?exp
         | (E|e) ~/#?exp
         )
     )
  & ( !integerPart*(-1:?sign):>0:?integerPart
    | 1:?sign
    )
  & !sign*(!integerPart+!decimalPart*10^(!d1+-1*!dn))*10^!exp
);

( out$float2fraction$"1.2"
& out$float2fraction$"1.02"
& out$float2fraction$"1.01"
& out$float2fraction$"10.01"
& out$float2fraction$"10.01e10"
& out$float2fraction$"10.01e1"
& out$float2fraction$"10.01e2"
& out$float2fraction$"10.01e-2"
& out$float2fraction$"-10.01e-2"
& out$float2fraction$"-10e-2"
& out$float2fraction$"0.000"
);

```

{{out}}

```txt
6/5
51/50
101/100
1001/100
100100000000
1001/10
1001
1001/10000
-1001/10000
-1/10
0
```



## Burlesque


```burlesque

ps^^-]to{"Int""Double"}\/~[\/L[1==?*

```


Assumes string is not empty.


## C

Returns true (non-zero) if character-string parameter represents a signed or unsigned floating-point number. Otherwise returns false (zero).


```c
#include <ctype.h>
#include <stdlib.h>
int isNumeric (const char * s)
{
    if (s == NULL || *s == '\0' || isspace(*s))
      return 0;
    char * p;
    strtod (s, &p);
    return *p == '\0';
}
```



## C++

Using stringstream:

```cpp
#include <sstream>
// for istringstream
using namespace std;

bool isNumeric( const char* pszInput, int nNumberBase )
{
	istringstream iss( pszInput );

	if ( nNumberBase == 10 )
	{
		double dTestSink;
		iss >> dTestSink;
	}
	else if ( nNumberBase == 8 || nNumberBase == 16 )
	{
		int nTestSink;
		iss >> ( ( nNumberBase == 8 ) ? oct : hex ) >> nTestSink;
	}
	else
		return false;

	// was any input successfully consumed/converted?
	if ( ! iss )
		return false;

	// was all the input successfully consumed/converted?
	return ( iss.rdbuf()->in_avail() == 0 );
}

```


Using find:

```cpp

bool isNumeric( const char* pszInput, int nNumberBase )
{
	string base = "0123456789ABCDEF";
	string input = pszInput;

	return (input.find_first_not_of(base.substr(0, nNumberBase)) == string::npos);
}

```


Using all_of (requires C++11)

```cpp

bool isNumeric(const std::string& input) {
    return std::all_of(input.begin(), input.end(), ::isdigit);
}

```


=={{header|C sharp|C#}}==
'''Framework:''' [[.NET]] 2.0+


```csharp
public static bool IsNumeric(string s)
{
    double Result;
    return double.TryParse(s, out Result);  // TryParse routines were added in Framework version 2.0.
}

string value = "123";
if (IsNumeric(value))
{
  // do something
}
```


'''Framework:''' [[.NET]] 1.0+


```csharp
public static bool IsNumeric(string s)
{
  try
  {
    Double.Parse(s);
    return true;
  }
  catch
  {
    return false;
  }
}
```



## Clojure


```clojure
(defn numeric? [s]
  (if-let [s (seq s)]
    (let [s (if (= (first s) \-) (next s) s)
          s (drop-while #(Character/isDigit %) s)
          s (if (= (first s) \.) (next s) s)
          s (drop-while #(Character/isDigit %) s)]
      (empty? s))))
```

This works with any sequence of characters, not just Strings, e.g.:

```clojure
(numeric? [\1 \2 \3])  ;; yields logical true
```



## COBOL


### Intrinsic Functions

COBOL has the intrinsic functions <code>TEST-NUMVAL</code> and <code>TEST-NUMVAL-C</code> to check if a string is numeric (<code>TEST-NUMVAL-C</code> is used to check if it is also a monetary string). Implementations supporting the 20XX draft standard can also use <code>TEST-NUMVAL-F</code> for floating-point numbers. They return 0 if the string is valid, or the position of the first incorrect character.


```cobol
        program-id. is-numeric.
        procedure division.
        display function test-numval-f("abc") end-display
        display function test-numval-f("-123.01E+3") end-display
        if function test-numval-f("+123.123") equal zero then
            display "is numeric" end-display
        else
            display "failed numval-f test" end-display
        end-if
        goback.
```



### Implementation

{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Is-Numeric.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Numeric-Chars      PIC X(10) VALUE "0123456789".

       01  Success            CONSTANT 0.
       01  Failure            CONSTANT 128.

       LOCAL-STORAGE SECTION.
       01  I                  PIC 99.

       01  Num-Decimal-Points PIC 99.
       01  Num-Valid-Chars    PIC 99.

       LINKAGE SECTION.
       01  Str                PIC X(30).

       PROCEDURE DIVISION USING Str.
           IF Str = SPACES
               MOVE Failure TO Return-Code
               GOBACK
           END-IF

           MOVE FUNCTION TRIM(Str) TO Str

           INSPECT Str TALLYING Num-Decimal-Points FOR ALL "."
           IF Num-Decimal-Points > 1
               MOVE Failure TO Return-Code
               GOBACK
           ELSE
               ADD Num-Decimal-Points TO Num-Valid-Chars
           END-IF

           IF Str (1:1) = "-" OR "+"
               ADD 1 TO Num-Valid-Chars
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
               INSPECT Str TALLYING Num-Valid-Chars
                   FOR ALL Numeric-Chars (I:1) BEFORE SPACE
           END-PERFORM

           INSPECT Str TALLYING Num-Valid-Chars FOR TRAILING SPACES

           IF Num-Valid-Chars = FUNCTION LENGTH(Str)
               MOVE Success TO Return-Code
           ELSE
               MOVE Failure TO Return-Code
           END-IF

           GOBACK
           .
```



## CoffeeScript

The isFinite function is built into JavaScript, so we don't need to create our own function in CoffeeScript.

```coffeescript

console.log (isFinite(s) for s in [5, "5", "-5", "5", "5e5", 0]) # all true
console.log (isFinite(s) for s in [NaN, "fred", "###"]) # all false

```




## ColdFusion

Adobe's ColdFusion


```cfm><cfset TestValue=34

  TestValue: <cfoutput>#TestValue#</cfoutput>

<cfif isNumeric(TestValue)>
  is Numeric.
<cfelse>
  is NOT Numeric.
</cfif>

<cfset TestValue="NAS">
  TestValue: <cfoutput>#TestValue#</cfoutput>

<cfif isNumeric(TestValue)>
  is Numeric.
<cfelse>
  is NOT Numeric.
</cfif>
```


### Alternative solution

<lang><cfoutput>#isNumeric(42)#</cfoutput>
```



## CFScript

ColdFusion Script (CfScript)

```cfm
isNumeric(42)
```



## Common Lisp

If the input may be relied upon to not be especially malicious, then it may be ''read'' and the result checked for being a number.

```lisp
(defun numeric-string-p (string)
  (let ((*read-eval* nil))
    (ignore-errors (numberp (read-from-string string)))))
```

<code>ignore-errors</code> here handles returning nil in case the input is invalid rather than simply non-numeric.

However, <code>read</code>[<code>-from-string</code>] has the side effect of interning any symbols encountered, and can have memory allocation larger than the input size (due to read syntax such as <code>#*</code>, which takes a length). The [http://www.cliki.net/PARSE-NUMBER <code>parse-number</code>] library provides a numbers-only equivalent of <code>read</code>.

```lisp
(defun numeric-string-p (string)
      (ignore-errors (parse-number:parse-number string)))  ; parse failed, return false (nil)
```



## D


### Standard Version

Using the standard Phobos function (currently binary and hex literals are not recognized):


```d
import std.stdio, std.string, std.array;

void main() {
    foreach (const s; ["12", " 12\t", "hello12", "-12", "02",
                 "0-12", "+12", "1.5", "1,000", "1_000",
                 "0x10", "0b10101111_11110000_11110000_00110011",
                 "-0b10101", "0x10.5"])
        writefln(`isNumeric("%s"): %s`, s, s.strip().isNumeric(true));
}
```

{{out}}

```txt
isNumeric("12"): true
isNumeric(" 12  "): true
isNumeric("hello12"): false
isNumeric("-12"): true
isNumeric("02"): true
isNumeric("0-12"): false
isNumeric("+12"): true
isNumeric("1.5"): true
isNumeric("1,000"): true
isNumeric("1_000"): true
isNumeric("0x10"): false
isNumeric("0b10101111_11110000_11110000_00110011"): false
isNumeric("-0b10101"): false
isNumeric("0x10.5"): false
```



### An Implementation


```d
import std.stdio, std.string, std.conv, std.array, std.exception;

bool isNumeric(in string s) pure {
    immutable s2 = s.strip.toLower.replace("_", "").replace(",", "");
    try {
        s2.to!real;
    } catch (ConvException e) {
        if (s2.startsWith("0x"))
            return !s2[2 .. $].to!ulong(16)
                    .collectException!ConvException;
        else if (s2.startsWith("0b"))
            return !s2[2 .. $].to!ulong(2)
                    .collectException!ConvException;
        else
            return false;
    }

    return true;
}

void main() {
    foreach (immutable s; ["12", " 12\t", "hello12", "-12", "02",
                 "0-12", "+12", "1.5", "1,000", "1_000",
                 "0x10", "0b10101111_11110000_11110000_00110011",
                 "-0b10101", "0x10.5"])
        writefln(`isNumeric("%s"): %s`, s, s.isNumeric);
}
```

{{out}}

```txt
isNumeric("12"): true
isNumeric(" 12  "): true
isNumeric("hello12"): false
isNumeric("-12"): true
isNumeric("02"): true
isNumeric("0-12"): false
isNumeric("+12"): true
isNumeric("1.5"): true
isNumeric("1,000"): true
isNumeric("1_000"): true
isNumeric("0x10"): true
isNumeric("0b10101111_11110000_11110000_00110011"): true
isNumeric("-0b10101"): false
isNumeric("0x10.5"): false
```


=={{header|Déjà Vu}}==

```dejavu
is-numeric s:
	true
	try:
		drop to-num s
	catch value-error:
		not

for v in [ "1" "0" "3.14" "hello" "12e3" "12ef" "-3" ]:
	!.( v is-numeric v )
```

{{out}}

```txt
"-3" true
"12ef" false
"12e3" true
"hello" false
"3.14" true
"0" true
"1" true
```



## Delphi

This simple function is a wrapper around a built-in Delphi function


```Delphi

function IsNumericString(const inStr: string): Boolean;
var
  i: extended;
begin
  Result := TryStrToFloat(inStr,i);
end;

```


This console application tests the function:


```Delphi

program isNumeric;

{$APPTYPE CONSOLE}

uses
  Classes,
  SysUtils;

function IsNumericString(const inStr: string): Boolean;
var
  i: extended;
begin
  Result := TryStrToFloat(inStr,i);
end;


{ Test function }
var
  s: string;
  c: Integer;

const
  MAX_TRIES = 10;
  sPROMPT   = 'Enter a string (or type "quit" to exit):';
  sIS       = ' is numeric';
  sISNOT    = ' is NOT numeric';

begin
  c := 0;
  s := '';
  repeat
    Inc(c);
    Writeln(sPROMPT);
    Readln(s);
    if (s <> '') then
      begin
        tmp.Add(s);
        if IsNumericString(s) then
          begin
            Writeln(s+sIS);
          end
          else
          begin
            Writeln(s+sISNOT);
          end;
        Writeln('');
      end;
  until
    (c >= MAX_TRIES) or (LowerCase(s) = 'quit');

end.


```


{{out}} Example summarised:

```txt

123 is numeric
-123.456 is numeric
-123.-456 is NOT numeric
.345 is numeric
m1k3 is NOT numeric

```



## Dyalect


```dyalect
func String.isNumeric() {
    try {
        var o = parse(this)
        o.getType() == Integer || o.getType() == Float
    } catch _ {
        false
    }
}

var str = "1234567"
print(str.isNumeric())
```



## E


```e
def isNumeric(specimen :String) {
    try {
        <import:java.lang.makeDouble>.valueOf(specimen)
        return true
    } catch _ {
       return false
    }
}
```



## EasyLang

<lang>func is_numeric a$ . r .
  r = 0
  h = number a$
  if error = 0
    r = 1
  .
  h# = numberf a$
  if error = 0
    r = 1
  .
  # because every variable must be used
  h# = h# + h
.

test$[] = [ "abc" "21a" "1234" "-13" "7.65" ]
for i range len test$[]
  call is_numeric test$[i] r
  if r = 1
    print test$[i] & " is numeric"
  else
    print test$[i] & " is not numeric"
  .
.
```



## EchoLisp

The conversion function '''string->number''' returns #f - false - in the string is not a number, else returns a number, which is #t - true - as far as logical operations are concerned

```scheme

(string->number "albert")
   → #f
(string->number -666)
    → -666
(if (string->number 666) 'YES 'NO)
    → YES

```



## Elixir


```elixir
defmodule RC do
  def is_numeric(str) do
    case Float.parse(str) do
      {_num, ""} -> true
      _          -> false
    end
  end
end

["123", "-12.3", "123.", ".05", "-12e5", "+123", " 123", "abc", "123a", "12.3e", "1 2"] |> Enum.filter(&RC.is_numeric/1)
```


{{out}}

```txt

["123", "-12.3", "-12e5", "+123"]

```



## Erlang

Erlang doesn't come with a way to say if a string represents a numeric value or not, but does come with the built-in function <tt>is_number/1</tt>, which will return true if the argument passed is either an integer or a float. Erlang also has two functions to transform a string to either a floating number or an integer, which will be used in conjunction with <tt>is_number/1</tt>.


```erlang
is_numeric(L) ->
    Float = (catch erlang:list_to_float(L)),
    Int = (catch erlang:list_to_integer(L)),
    is_number(Float) orelse is_number(Int).
```



## ERRE

Short form using predeclared ERRE functions VAL and STR$.

```erre

PROGRAM NUMERIC

PROCEDURE IS_NUMERIC(S$->ANS%)
LOCAL T1,T1$
    T1=VAL(S$)
    T1$=STR$(T1)
    ANS%=(T1$=S$) OR T1$=" "+S$
END PROCEDURE

BEGIN
    PRINT(CHR$(12);)
    INPUT("Enter a string",S$)
    IS_NUMERIC(S$->ANS%)
    IF ANS% THEN PRINT("is num") ELSE PRINT("not num")
END PROGRAM

```

{{out}}

```txt
Enter a string? 12.30
is num

```



## Euphoria


```Euphoria
include get.e

function is_numeric(sequence s)
    sequence val
    val = value(s)
    return val[1]=GET_SUCCESS and atom(val[2])
end function
```


=={{header|F_Sharp|F#}}==

```fsharp
let is_numeric a = fst (System.Double.TryParse(a))
```



## Factor


```factor
: numeric? ( string -- ? ) string>number >boolean ;
```



## Fantom

The 'fromStr' methods return a parsed number or given an error.  The 'false' tells each method to return null if the string does not parse as a number of given type, otherwise, the 'fromStr' method throws an exception.


```fantom

class Main
{
  // function to see if str contains a number of any of built-in types
  static Bool readNum (Str str)
  {
    int := Int.fromStr (str, 10, false)  // use base 10
    if (int != null) return true
    float := Float.fromStr (str, false)
    if (float != null) return true
    decimal := Decimal.fromStr (str, false)
    if (decimal != null) return true

    return false
  }

  public static Void main ()
  {
    echo ("For '2': " + readNum ("2"))
    echo ("For '-2': " + readNum ("-2"))
    echo ("For '2.5': " + readNum ("2.5"))
    echo ("For '2a5': " + readNum ("2a5"))
    echo ("For '-2.1e5': " + readNum ("-2.1e5"))
  }
}

```


{{out}}

```txt

For '2': true
For '-2': true
For '2.5': true
For '2a5': false
For '-2.1e5': true

```




## Forth

{{works with|gforth|0.6.2}}

```forth
: is-numeric ( addr len -- )
  2dup snumber? ?dup if      \ not standard, but >number is more cumbersome to use
   0< if
     -rot type ."  as integer = " .
   else
     2swap type ."  as double = " <# #s #> type
   then
  else 2dup >float if
    type ."  as float = " f.
  else
    type ."  isn't numeric in base " base @ dec.
  then then ;

s" 1234" is-numeric    \ 1234 as integer = 1234
s" 1234." is-numeric    \ 1234. as double = 1234
s" 1234e" is-numeric    \ 1234e as float = 1234.
s" $1234" is-numeric    \ $1234 as integer = 4660  ( hex literal )
s" %1010" is-numeric    \ %1010 as integer = 10  ( binary literal )
s" beef" is-numeric    \ beef isn't numeric in base 10
hex
s" beef" is-numeric    \ beef as integer = BEEF
s" &1234" is-numeric    \ &1234 as integer = 4D2 ( decimal literal )
```



## Fortran


```fortran
FUNCTION is_numeric(string)
  IMPLICIT NONE
  CHARACTER(len=*), INTENT(IN) :: string
  LOGICAL :: is_numeric
  REAL :: x
  INTEGER :: e
  READ(string,*,IOSTAT=e) x
  is_numeric = e == 0
END FUNCTION is_numeric
```



## FreeBASIC

FreeBASIC has a built-in Val() function which converts numeric strings to doubles. However, it is not ideal for the present task
since it will try to convert as much of the string as it can (so "123xyz" would convert to 123) and return 0 if a conversion on
this basis is not possible (i.e. "xyz" would return 0).

I've therefore written a custom function which recognizes signed numbers in bases from 2 to 16 (but only integral numbers for bases other than 10). For base 10, it will treat "123." or ".123" as numbers but not ".". It doesn't recognize scientific notation but does recognize the integral prefixes &H, &O and &B if bases 16, 8 or 2 respectively are specified.


```freebasic
' FB 1.05.0 Win64

Dim Shared symbols(0 To 15) As UByte
For i As Integer = 48 to 57
  symbols(i - 48) = i
Next
For i As Integer = 97 to 102
  symbols(i - 87) = i
Next

Const plus  As UByte = 43
Const minus As Ubyte = 45
Const dot   As UByte = 46

Function isNumeric(s As Const String, base_ As Integer = 10) As Boolean
  If s = "" OrElse s = "."  OrElse s = "+" OrElse s = "-" Then Return False
  Err = 0

  If base_ < 2 OrElse base_ > 16 Then
    Err = 1000
    Return False
  End If

  Dim t As String = LCase(s)

  If (t[0] = plus) OrElse (t[0] = minus) Then
     t = Mid(t, 2)
  End If

  If Left(t, 2) = "&h" Then
    If base_ <> 16 Then Return False
    t = Mid(t, 3)
  End if

  If Left(t, 2) = "&o" Then
    If base_ <> 8 Then Return False
    t = Mid(t, 3)
  End if

  If Left(t, 2) = "&b" Then
    If base_ <> 2 Then Return False
    t = Mid(t, 3)
  End if

  If Len(t) = 0 Then Return False
  Dim As Boolean isValid, hasDot = false

  For i As Integer = 0 To Len(t) - 1
    isValid = False

    For j As Integer = 0 To base_ - 1
      If t[i] = symbols(j) Then
        isValid = True
        Exit For
      End If
      If t[i] = dot Then
        If CInt(Not hasDot) AndAlso (base_ = 10) Then
          hasDot = True
          IsValid = True
          Exit For
        End If
        Return False ' either more than one dot or not base 10
      End If
    Next j

    If Not isValid Then Return False
  Next i

  Return True
End Function

Dim s As String
s = "1234.056789"
Print s, " (base 10) => "; isNumeric(s)
s = "1234.56"
Print s, " (base 7)  => "; isNumeric(s, 7)
s = "021101"
Print s, " (base 2)  => "; isNumeric(s, 2)
s = "Dog"
Print s, " (base 16) => "; isNumeric(s, 16)
s = "Bad125"
Print s, " (base 16) => "; isNumeric(s, 16)
s = "-0177"
Print s, " (base 8)  => "; isNumeric(s, 8)
s = "+123abcd.ef"
Print s, " (base 16) => "; isNumeric(s, 8)
s = "54321"
Print s, " (base 6)  => "; isNumeric(s, 6)
s = "123xyz"
Print s, " (base 10) => "; isNumeric(s)
s = "xyz"
Print s, " (base 10) => "; isNumeric(s)
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

1234.056789    (base 10) => true
1234.56        (base 7)  => false
021101         (base 2)  => false
Dog            (base 16) => false
Bad125         (base 16) => true
-0177          (base 8)  => true
+123abcd.ef    (base 16) => false
54321          (base 6)  => true
123xyz         (base 10) => false
xyz            (base 10) => false

```



## Free Pascal


```pascal
function isNumeric(const potentialNumeric: string): boolean;
var
	potentialInteger: integer;
	potentialReal: real;
	integerError: integer;
	realError: integer;
begin
	integerError := 0;
	realError := 0;

	// system.val attempts to convert numerical value representations.
	// It accepts all notations as they are accepted by the language,
	// as well as the '0x' (or '0X') prefix for hexadecimal values.
	val(potentialNumeric, potentialInteger, integerError);
	val(potentialNumeric, potentialReal, realError);

	isNumeric := (integerError = 0) or (realError = 0);
end;
```



## Gambas


```gambas
Public Sub Form_Open()
Dim sAnswer, sString As String

sString = Trim(InputBox("Enter as string", "String or Numeric"))

If IsNumber(sString) Then sAnswer = "'" & sString & "' is numeric" Else sAnswer = "'" & sString & "' is a string"
Print sAnswer

End
```

Output:

```txt

'Charlie' is a string
'-00.256666' is numeric

```



## Go


```go
import "strconv"

func IsNumeric(s string) bool {
    _, err := strconv.ParseFloat(s, 64)
    return err == nil
}
```



## Groovy

Use the positional parser in java.text.NumberFormat. If, after parsing, the parse position is at the end of the string, we can deduce that the entire string was a valid number.

```groovy
def isNumeric = {
    def formatter = java.text.NumberFormat.instance
    def pos = [0] as java.text.ParsePosition
    formatter.parse(it, pos)

    // if parse position index has moved to end of string
    // them the whole string was numeric
    pos.index == it.size()
}
```


Test Program:

```groovy
println isNumeric('1')
println isNumeric('-.555')
println isNumeric('1,000,000')
println isNumeric(' 1 1 1 1 ')
println isNumeric('abcdef')
```


{{out}}

```txt
true
true
true
false
false
```



## Haskell

This function is not particularly useful in a statically typed language. Instead, one would just attempt to convert the string
to the desired type with ''read'' or ''reads'', and handle parsing failure appropriately.

The task doesn't define which strings are considered "numeric", so we do Integers and Doubles, which should catch the most common cases (including hexadecimal 0x notation):


```haskell
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False

isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s
```


One can easily add ''isRational'', ''isComplex'' etc. following the same pattern.

Another way would be to use the Data.Char module, allowing code such as:


```haskell
areDigits = all isDigit
isDigit  selects ASCII digits i.e. '0'..'9'
isOctDigit selects '0'..'7'
isHexDigit selects '0'..'9','A'..'F','a'..'f'
```


so read s::Int (for instance) could be reliably used if string s passed these tests.


## Haxe

Haxe has a built-in function that will convert a string to an integer, so we can use that to determine if the string is numeric or not.
```ActionScript

static function isNumeric(n:String):Bool
{
	if (Std.parseInt(n) != null) //Std.parseInt converts a string to an int
        {
		return true; //as long as it results in an integer, the function will return true
	}
        else
        {
		return false;
	}
}

```



## HicEst


```hicest
                              !    = bin + 2*int + 4*flt + 8*oct +16*hex + 32*sci
   isNumeric("1001")          ! 27 =  1       1       0       1       1        0
   isNumeric("123")           ! 26 =  0       1       0       1       1        0
   isNumeric("1E78")          ! 48 =  0       0       0       0       1        1
   isNumeric("-0.123")        !  4 =  0       0       1       0       0        1
   isNumeric("-123.456e-78")  ! 32 =  0       0       0       0       0        1
   isNumeric(" 123")          !  0: leading blank
   isNumeric("-123.456f-78")  !  0: illegal character f


FUNCTION isNumeric(string)    ! true ( > 0 ), no leading/trailing blanks
   CHARACTER string
   b = INDEX(string, "[01]+", 128, Lbin)                ! Lbin returns length found
   i = INDEX(string, "-?\d+", 128, Lint)                ! regular expression: 128
   f = INDEX(string, "-?\d+\.\d*", 128, Lflt)
   o = INDEX(string, "[0-7]+",    128, Loct)
   h = INDEX(string, "[0-9A-F]+", 128, Lhex)            ! case sensitive: 1+128
   s = INDEX(string, "-?\d+\.*\d*E[+-]*\d*", 128, Lsci)
   IF(anywhere) THEN     ! 0 (false) by default
     isNumeric = ( b > 0 ) + 2*( i > 0 ) + 4*( f > 0 ) + 8*( o > 0 ) + 16*( h > 0 ) + 32*( s > 0 )
   ELSEIF(boolean) THEN  ! 0 (false) by default
     isNumeric = ( b + i + f + o + h + s ) > 0 ! this would return 0 or 1
   ELSE
     L = LEN(string)
     isNumeric = (Lbin==L) + 2*(Lint==L) + 4*(Lflt==L) + 8*(Loct==L) + 16*(Lhex==L) + 32*(Lsci==L)
   ENDIF
 END
```



## i


```i
concept numeric(n) {
	number(n)
	errors {
		print(n, "  is not numeric!")
		return
	}
	print(n, "  is numeric :)")
}

software {
	numeric("1200")
	numeric("3.14")
	numeric("3/4")
	numeric("abcdefg")
	numeric("1234test")
}
```


=={{header|Icon}} and {{header|Unicon}}==
The code writes a printable image of x whatever type it is and a statement about whether it is numeric or not.  Icon and Unicon use success and failure instead of boolean functions, numeric(x) is built-in and returns x or fails.

```Icon

write(image(x), if numeric(x) then " is numeric." else " is not numeric")

```



## IDL


```idl
function isnumeric,input
  on_ioerror, false
  test = double(input)
  return, 1
  false: return, 0
end
```


Could be called like this:


```idl
if isnumeric('-123.45e-2') then print, 'yes' else print, 'no'
; ==> yes
if isnumeric('picklejuice') then print, 'yes' else print, 'no'
; ==> no
```



## J


```j
isNumeric=: _ ~: _ ". ]
isNumericScalar=: 1 -: isNumeric
TXT=: ,&' a scalar numeric value.' &.> ' is not';' represents'
sayIsNumericScalar=: , TXT {::~ isNumericScalar
```

Examples of use:

```j
   isNumeric '152'
1
   isNumeric '152 -3.1415926 Foo123'
1 1 0
   isNumeric '42 foo42 4.2e1 4200e-2 126r3 16b2a 42foo'
1 0 1 1 1 1 0
   isNumericScalar '152 -3.1415926 Foo123'
0
   sayIsNumericScalar '-3.1415926'
-3.1415926 represents a scalar numeric value.
```



## Java

It's generally bad practice in Java to rely on an exception being thrown since exception handling is relatively expensive. If non-numeric strings are common, you're going to see a huge performance hit.

```java
public boolean isNumeric(String input) {
  try {
    Integer.parseInt(input);
    return true;
  }
  catch (NumberFormatException e) {
    // s is not numeric
    return false;
  }
}
```


Alternative 1 : Check that each character in the string is number. Note that this will only works for integers.


```java
private static final boolean isNumeric(final String s) {
  if (s == null || s.isEmpty()) return false;
  for (int x = 0; x < s.length(); x++) {
    final char c = s.charAt(x);
    if (x == 0 && (c == '-')) continue;  // negative
    if ((c >= '0') && (c <= '9')) continue;  // 0 - 9
    return false; // invalid
  }
  return true; // valid
}
```


Alternative 2 : use a regular expression (a more elegant solution).


```java
public static boolean isNumeric(String inputData) {
  return inputData.matches("[-+]?\\d+(\\.\\d+)?");
}
```


Alternative 3 : use the positional parser in the java.text.NumberFormat object (a more robust solution). If, after parsing, the parse position is at the end of the string, we can deduce that the entire string was a valid number.


```java
public static boolean isNumeric(String inputData) {
  NumberFormat formatter = NumberFormat.getInstance();
  ParsePosition pos = new ParsePosition(0);
  formatter.parse(inputData, pos);
  return inputData.length() == pos.getIndex();
}
```


Alternative 4 : use the java.util.Scanner object. Very useful if you have to scan multiple entries.


```java
public static boolean isNumeric(String inputData) {
  Scanner sc = new Scanner(inputData);
  return sc.hasNextInt();
}
```

Scanner also has similar methods for longs, shorts, bytes, doubles, floats, BigIntegers, and BigDecimals as well as methods for integral types where you may input a base/radix other than 10 (10 is the default, which can be changed using the useRadix method).


## JavaScript

A far better validator can be found on StackOverflow[http://stackoverflow.com/questions/18082/validate-numbers-in-javascript-isnumeric]

```javascript
function isNumeric(n) {
  return !isNaN(parseFloat(n)) && isFinite(n);
}
var value = "123.45e7"; // Assign string literal to value
if (isNumeric(value)) {
  // value is a number
}
//Or, in web browser in address field:
//  javascript:function isNumeric(n) {return !isNaN(parseFloat(n)) && isFinite(n);}; value="123.45e4"; if(isNumeric(value)) {alert('numeric')} else {alert('non-numeric')}

```


## jq

In versions of jq that support try/catch, the simplest way to test if a string can be parsed as a number is:
```jq>try tonumber catch false</lang

The above expression will emit the corresponding number, or false if there is none.  Here then is a boolean filter which will also emit true for each input that is a number:

```jq
def is_numeric: true and try tonumber catch false;
```



## Julia

The function <tt>isnumeric</tt> tests for strings that parse directly to numbers.  This test excludes symbols, such as &pi; and <tt>1 + 1</tt>, that evaluate to numbers as well as certain elaborate numbers (large integers, rationals and complex numbers) whose literals parse to expressions that must be evaluated to yield numbers.


```julia
using Printf

isnumeric(s::AbstractString) = tryparse(Float64, s) isa Number

tests = ["1", "-121", "one", "pi", "1 + 1", "NaN", "1234567890123456789", "1234567890123456789123456789",
        "1234567890123456789123456789.0", "1.3", "1.4e10", "Inf", "1//2", "1.0 + 1.0im"]

for t in tests
    fl = isnumeric(t) ? "is" : "is not"
    @printf("%35s %s a direct numeric literal.\n", t, fl)
end
```


{{out}}

```txt

                                  1 is a direct numeric literal.
                               -121 is a direct numeric literal.
                                one is not a direct numeric literal.
                                 pi is not a direct numeric literal.
                              1 + 1 is not a direct numeric literal.
                                NaN is a direct numeric literal.
                1234567890123456789 is a direct numeric literal.
       1234567890123456789123456789 is a direct numeric literal.
     1234567890123456789123456789.0 is a direct numeric literal.
                                1.3 is a direct numeric literal.
                             1.4e10 is a direct numeric literal.
                                Inf is a direct numeric literal.
                               1//2 is not a direct numeric literal.
                        1.0 + 1.0im is not a direct numeric literal.


```



## Kotlin


```scala
// version 1.1

fun isNumeric(input: String): Boolean =
    try {
        input.toDouble()
        true
    } catch(e: NumberFormatException) {
        false
    }

fun main(args: Array<String>) {
    val inputs = arrayOf("152", "-3.1415926", "Foo123", "-0", "456bar", "1.0E10")
    for (input in inputs) println("$input is ${if (isNumeric(input)) "numeric" else "not numeric"}")
}
```


{{out}}

```txt

152 is numeric
-3.1415926 is numeric
Foo123 is not numeric
-0 is numeric
456bar is not numeric
1.0E10 is numeric

```



## LabVIEW

{{VI solution|LabVIEW_Determine_if_a_string_is_numeric.png}}


## Lasso


{{works with|Lasso|8 & 9 }}


```Lasso
local(str='12345')
string_isNumeric(#str) // true
```


{{works with|Lasso|9}}


```Lasso
'12345'->isdigit // true
'1X34Q'->isdigit // false
```



## Liberty BASIC


```lb

DATA "PI", "0123", "-0123", "12.30", "-12.30", "123!", "0"
DATA "0.0", ".123", "-.123", "12E3", "12E-3", "12+3", "end"


while n$ <> "end"
    read n$
    print n$, IsNumber(n$)
wend
end

function IsNumber(string$)
    on error goto [NotNumber]
    string$ = trim$(string$)
    'check for float overflow
    n = val(string$)

    'assume it is number and try to prove wrong
    IsNumber = 1
    for i = 1 to len(string$)
        select case mid$(string$, i, 1)
            case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
                HasNumeric = 1 'to check if there are any digits
            case "e", "E"
                '"e" must not occur more than once
                'must not occur before digits

                if HasE > 0 or HasNumeric = 0 then
                    IsNumber = 0
                    exit for
                end if
                HasE = i 'store position of "e"
                HasNumeric = 0 'needs numbers after "e"
            case "-", "+"
                'must be either first character or immediately after "e"
                '(HasE = 0 if no occurrences yet)
                if HasE <> i-1 then
                    IsNumber = 0
                    exit for
                end if
            case "."
                'must not have previous points and must not come after "e"
                if HasE <> 0 or HasPoint <> 0 then
                    IsNumber = 0
                    exit for
                end if
                HasPoint = 1
            case else
                'no other characters allowed
                IsNumber = 0
                exit for
        end select
    next i
    'must have digits
    if HasNumeric = 0 then IsNumber = 0
    [NotNumber]
end function

```



## Lisaac


```Lisaac

"123457".is_integer.println;
// write TRUE on stdin

```



## Logo


```logo
show number? "-1.23    ; true
```



## Lua

This will also accept strings like "0xFF" or "314.16e-2" as numbers.

```lua
if tonumber(a) ~= nil then
   --it's a number
end;

```



## M2000 Interpreter

We have to define our IsNumber()
Version 2 handle decimal point character. For code M2000 use dot, but for input and output use the user specified decimal point, from OS. Function Str$(1.2) return a string with a dot always, but if we place a second parameter this change. Print str$(1.2, "") maybe return 1,2 and nor 1.2. Print str$(1.2, "#.00") maybe return 1.20 or 1,20. The reverse function is Val() which can take more characters so A Val("121mm") is 121, and with a second parameter we can interpret properly the decimal dot: Print Val("1.2", ".") always return 1.2 double. Print Val("1,2", ",")=1.2 return true, 1.2 is a m2000 number literal and always has a dot.

```M2000 Interpreter

\\ version 2
Module Checkit {
      function global isNumber(a$, de$=".") {
            =false=true  ' return boolean
            if de$="" then de$=str$(.1,".")  ' get current decimal point character
            a$=trim$(ucase$(a$))
            m=len(a$)
            if m=0 then exit
            c$=filter$(a$,"0123456789")
            if c$ = "" then {
                  =true
            } else.if m>1 then {
                  \ may have -+ and ,
                  if m=2 then {
                        if not c$~"[-+\"+de$+"]" then break
                  } else {
                        if left$(c$,1 ) ~"[+-]" then c$=mid$(c$, 2)
                        if not (c$=de$ or c$=de$+"E" or c$ ~ de$+"E[+-]") then break
                        if c$ ~ de$+"E[+-]" then if not (instr(a$,"E+")>0 or instr(a$,"E-")>0) then break
                  }
                 if de$<>"." then  a$=replace$(de$, ".", a$, 1,1)
                 try {inline "a="+a$+"=="+a$}
                 if valid(a) then =a = true=true  ' return boolean
             }
      }
      Print isNumber("+1"), isnumber("-1"), isNumber("1+")=false, isnumber("1-")=false
      Print isNumber(",1",","), isnumber("1,",","), isNumber(",0",","), isnumber("0,", ",")
      Print isNumber(".1"), isnumber("1."), isNumber(".0"), isnumber("0.")
      Print isNumber("+.1"), isnumber("-1."), isNumber(".12e+232"), isnumber("0.122e10")
      Print isNumber("+.1a")=false, isnumber("asasa1212")=false, isNumber("1.2e43+23")=false, isnumber("0.122e10")
      Print isNumber("1221.211.1221")=false, isnumber("1221e1212")=false, isNumber("1.2e4323")=false, isnumber("-.122e-10")
}
Checkit


```


From rev.31 Version 9.3 Val function update, to include a more quick version of above. We have to specify the dot char or write any two or more chars for dot to get integer part. Val function return number and in third argument (passing by reference by default) return first position in string after number. If string is empty or have no number then position is -1. If a number found position is >1. Leading spaces trimmed.


```M2000 Interpreter

Function IsNumeric(a$) {
      def m
      =val(false->boolean)
      Try {
            if islet then {
                  z=val(a$,letter$, m)
            } else.if isnum then {
                  z=val(a$,number, m)
            } else z=val(a$,"", m)
            =m>len(a$)
      }
}
Function IsIntegerNumeric(a$) {
      def m
      =val(false->boolean)
      Try {
            z=val(a$,"Int", m)
            =m>len(a$)
      }
}

Locale 1033 ' to use . as decimal, else we have to place 1033 or "." as second parameter

Print isNumeric("+1"), isNumeric("-1"), isNumeric("1+")=false, isNumeric("1-")=false
Print isNumeric(",1",","), isNumeric("1,",","), isNumeric(",0",","), isNumeric("0,", ",")
Print isNumeric(".1"), isNumeric("1."), isNumeric(".0"), isNumeric("0.")
Print isNumeric("+.1"), isNumeric("-1."), isNumeric(".12e+232"), isNumeric("0.122e10")
Print isNumeric("+.1a")=false, isNumeric("asasa1212")=false, isNumeric("1.2e43+23")=false, isNumeric("0.122e10")
Print isNumeric("1221.211.1221")=false, isNumeric("1221e1212")=false, isNumeric("1.2e4323")=false, isNumeric("-.122e-10")


```



## Maple


```maple
isNumeric := proc(s)
	try
		if type(parse(s), numeric) then
			printf("The string is numeric."):
		else
			printf("The string is not numeric."):
		end if:
	catch:
		printf("The string is not numeric."):
	end try:
end proc:
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
NumberQ[ToExpression["02553352000242"]]
```



## MATLAB


```MATLAB

    % copy from Octave version on this page
    function r = isnum(a)
     if ( isnumeric(a) )
       r = 1;
     else
       o = str2num(a);
       r = !isempty(o);
     endif
   end

% tests
disp(isnum(123)) % 1
disp(isnum("123")) % 1
disp(isnum("foo123")) % 0
disp(isnum("123bar")) % 0
disp(isnum("3.1415")) % 1


```



## Maxima


```maxima
numberp(parse_string("170141183460469231731687303715884105727"));
```



## MAXScript


```maxscript
fn isNumeric str =
(
    try
    (
        (str as integer) != undefined
    )
    catch(false)
)

isNumeric "123"
```



## min

{{works with|min|0.19.3}}

```min
(
  dup (((int integer?) (pop false)) try) dip
  ((float float?) (pop false)) try or
) :numeric?
```



## MiniScript

We rely on conversion to number returning a nonzero number, plus special checks for zero strings.  Note that the <code>val</code> function is forgiving about additional characters ''after'' the number, so our function is too.

```MiniScript
isNumeric = function(s)
    return s == "0" or s == "-0" or val(s) != 0
end function

print isNumeric("0")
print isNumeric("42")
print isNumeric("-3.14157")
print isNumeric("5@*#!")
print isNumeric("spam")
```


{{out}}

```txt
1
1
1
1
0
```



## MIPS Assembly


```mips

# $a0 char val
# $a1 address pointer
# $a2 PERIOD_HIT_FLAG
# $a3 HAS_DIGITS_FLAG

.data
			### CHANGE THIS STRING TO TEST DIFFERENT ONES... ###
	string: .asciiz "-.1236"
	s_false: .asciiz "False"
	s_true: .asciiz "True"
.text
main:
	set_up: #test for 0th char == 45 or 46 or 48...57
		la $a1,string
		lb $a0,($a1)
		beq $a0,45,loop # == '-'
		beq $a0,46,loop # == '.'
		blt $a0,48,exit_false # isn't below the ascii range for chars '0'...'9'
		bgt $a0,57,exit_false # isn't above the ascii range for chars '0'...'9'
	loop:
		addi $a1,$a1,1
		lb $a0,($a1)
		beqz $a0,exit_true # test for \0 null char
		beq $a0,46,period_test #test for a duplicate period
		blt $a0,48,exit_false  #test for
		bgt $a0,57,exit_false
		la $a3,1 #set the HAS_DIGITS flag. This line is only reached because the
			 #    tests for period and - both jump back to start.
		j loop

exit_true:
	beqz $a3,exit_false
	la $a0,s_true
	la $v0,4
	syscall

	li $v0,10
	syscall

exit_false:
	la $a0,s_false
	la $v0,4
	syscall

	li $v0,10
	syscall

period_test:
	beq $a2,1,exit_false
	li $a2,1
	j loop

```



## Mirah


```mirah
import java.text.NumberFormat
import java.text.ParsePosition
import java.util.Scanner

# this first example relies on catching an exception,
# which is bad style and poorly performing in Java
def is_numeric?(s:string)
    begin
        Double.parseDouble(s)
        return true
    rescue
        return false
    end
end

puts '123   is numeric' if is_numeric?('123')
puts '-123  is numeric' if is_numeric?('-123')
puts '123.1 is numeric' if is_numeric?('123.1')

puts 'nil   is not numeric' unless is_numeric?(nil)
puts "''    is not numeric" unless is_numeric?('')
puts 'abc   is not numeric' unless is_numeric?('abc')
puts '123-  is not numeric' unless is_numeric?('123-')
puts '1.2.3 is not numeric' unless is_numeric?('1.2.3')


# check every element of the string
def is_numeric2?(s: string)
    if (s == nil || s.isEmpty())
        return false
    end
    if (!s.startsWith('-'))
        if s.contains('-')
            return false
        end
    end

    0.upto(s.length()-1) do |x|
        c = s.charAt(x)
        if ((x == 0) && (c == '-'.charAt(0)))
            # negative number
        elsif (c == '.'.charAt(0))
            if (s.indexOf('.', x) > -1)
                return false # more than one period
            end
        elsif (!Character.isDigit(c))
            return false
        end
    end
    true
end


puts '123   is numeric' if is_numeric2?('123')
puts '-123  is numeric' if is_numeric2?('-123')
puts '123.1 is numeric' if is_numeric2?('123.1')

puts 'nil   is not numeric' unless is_numeric2?(nil)
puts "''    is not numeric" unless is_numeric2?('')
puts 'abc   is not numeric' unless is_numeric2?('abc')
puts '123-  is not numeric' unless is_numeric2?('123-')
puts '1.2.3 is not numeric' unless is_numeric2?('1.2.3')



# use a regular  expression
def is_numeric3?(s:string)
  s == nil || s.matches("[-+]?\\d+(\\.\\d+)?")
end

puts '123   is numeric' if is_numeric3?('123')
puts '-123  is numeric' if is_numeric3?('-123')
puts '123.1 is numeric' if is_numeric3?('123.1')

puts 'nil   is not numeric' unless is_numeric3?(nil)
puts "''    is not numeric" unless is_numeric3?('')
puts 'abc   is not numeric' unless is_numeric3?('abc')
puts '123-  is not numeric' unless is_numeric3?('123-')
puts '1.2.3 is not numeric' unless is_numeric3?('1.2.3')


#  use the positional parser in the java.text.NumberFormat object
# (a more robust solution). If, after parsing, the parse position is at
# the end of the string, we can deduce that the entire string was a
# valid number.
def is_numeric4?(s:string)
    return false if s == nil
    formatter = NumberFormat.getInstance()
    pos = ParsePosition.new(0)
    formatter.parse(s, pos)
    s.length() == pos.getIndex()
end


puts '123   is numeric' if is_numeric4?('123')
puts '-123  is numeric' if is_numeric4?('-123')
puts '123.1 is numeric' if is_numeric4?('123.1')

puts 'nil   is not numeric' unless is_numeric4?(nil)
puts "''    is not numeric" unless is_numeric4?('')
puts 'abc   is not numeric' unless is_numeric4?('abc')
puts '123-  is not numeric' unless is_numeric4?('123-')
puts '1.2.3 is not numeric' unless is_numeric4?('1.2.3')


# use the java.util.Scanner object. Very useful if you have to
# scan multiple entries. Scanner also has similar methods for longs,
# shorts, bytes, doubles, floats, BigIntegers, and BigDecimals as well
# as methods for integral types where you may input a base/radix other than
# 10 (10 is the default, which can be changed using the useRadix method).
def is_numeric5?(s:string)
    return false if s == nil
    Scanner sc = Scanner.new(s)
    sc.hasNextDouble()
end

puts '123   is numeric' if is_numeric5?('123')
puts '-123  is numeric' if is_numeric5?('-123')
puts '123.1 is numeric' if is_numeric5?('123.1')

puts 'nil   is not numeric' unless is_numeric5?(nil)
puts "''    is not numeric" unless is_numeric5?('')
puts 'abc   is not numeric' unless is_numeric5?('abc')
puts '123-  is not numeric' unless is_numeric5?('123-')
puts '1.2.3 is not numeric' unless is_numeric5?('1.2.3')
```



## mIRC Scripting Language

{{works with|mIRC}}

```mirc
var %value = 3
if (%value isnum) {
  echo -s %value is numeric.
}
```


=={{header|Modula-3}}==

```modula3
MODULE Numeric EXPORTS Main;

IMPORT IO, Fmt, Text;

PROCEDURE isNumeric(s: TEXT): BOOLEAN =
  BEGIN
    FOR i := 0 TO Text.Length(s) DO
      WITH char = Text.GetChar(s, i) DO
        IF i = 0 AND char = '-' THEN
          EXIT;
        END;
        IF char >= '0' AND char <= '9' THEN
          EXIT;
        END;
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END isNumeric;

BEGIN
  IO.Put("isNumeric(152) = " & Fmt.Bool(isNumeric("152")) & "\n");
  IO.Put("isNumeric(-3.1415926) = " & Fmt.Bool(isNumeric("-3.1415926")) & "\n");
  IO.Put("isNumeric(Foo123) = " & Fmt.Bool(isNumeric("Foo123")) & "\n");
END Numeric.
```


{{out}}

```txt

isNumeric(152) = TRUE
isNumeric(-3.1415926) = TRUE
isNumeric(Foo123) = FALSE

```



## MUMPS

<p>In MUMPS, strings are automatically converted to numbers when a unary or binary arithmetic operator works upon them. If there are no leading digits, a string converts to zero. If there a string of digits followed by an "e" or an "E" followed in turn by more digits, the numbers after the letter are treated as an exponent.</p>
<p>Examples from command line:
```txt

USER>WRITE +"1"
1
USER>WRITE +"1A"
1
USER>WRITE +"A1"
0
USER>WRITE +"1E"
1
USER>WRITE +"1E2"
100
USER>WRITE +"1EA24"
1
USER>WRITE +"1E3A"
1000
USER>WRITE +"1E-3"
.001

```
</p>
<p>There is a function, $ISVALIDNUM, to do the testing.
```txt

USER>WRITE $SELECT($ISVALIDNUM("123"):"Valid",1:"Invalid"),!
Valid

USER>WRITE $SELECT($ISVALIDNUM("a123"):"Valid",1:"Invalid"),!
Invalid

USER>WRITE $SELECT($ISVALIDNUM("123a"):"Valid",1:"Invalid"),!
Invalid

USER>WRITE $SELECT($ISVALIDNUM("123e4"):"Valid",1:"Invalid"),!
Valid
```



## Nemerle


```Nemerle
using System;
using System.Console;

module IsNumeric
{
    IsNumeric( input : string) : bool
    {
        mutable meh = 0.0;  // I don't want it, not going to use it, why force me to declare it?
        double.TryParse(input, out meh)
    }

    Main() : void
    {
        def num = "-1.2345E6";
        def not = "abc45";
        WriteLine($"$num is numeric: $(IsNumeric(num))");
        WriteLine($"$not is numeric: $(IsNumeric(not))");
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

numeric digits 20

loop n_ over getTestData()
  -- could have used n_.datatype('N') directly here...
  if isNumeric(n_) then msg = 'numeric'
                   else msg = 'not numeric'
  say ('"'n_'"').right(25)':' msg
  end n_

return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Pointless in NetRexx; the DATATYPE built-in-function is more powerful!
method isNumeric(testString) public static returns boolean
  return testString.datatype('N')

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method getTestData() private static returns Rexx[]

  -- Coercing numbers into the Rexx type has the effect of converting them to strings.
  -- NetRexx will still perform arithmetic on Rexx strings if those strings represent numbers.
  -- Notice that whitespace between the sign and the number are ignored even when inside a string constant
  testData = [ Rexx -
    ' one and a half',   1,   1.5,    1.5e+27, '   1 ', '  1.5 ', '  1.5e+27 ', -
    '-one and a half', - 1, - 1.5,  - 1.5e-27, ' - 1 ', '- 1.5 ', '- 1.5e-27 ', -
    '+one and a half', + 1, + 1.5,  + 1.5e+27, ' + 1 ', '+ 1.5 ', '+ 1.5e+27 ', -
    'Math Constants', -
     Math.PI,  Math.E, -
    -Math.PI, -Math.E, -
    +Math.PI, +Math.E, -
    'Numeric Constants', -
    Double.NaN, Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY -
    ]
  return testData

```

{{out}}

```txt

        " one and a half": not numeric
                      "1": numeric
                    "1.5": numeric
                "1.5e+27": numeric
                  "   1 ": numeric
                 "  1.5 ": numeric
             "  1.5e+27 ": numeric
        "-one and a half": not numeric
                     "-1": numeric
                   "-1.5": numeric
               "-1.5E-27": numeric
                  " - 1 ": numeric
                 "- 1.5 ": numeric
             "- 1.5e-27 ": numeric
        "+one and a half": not numeric
                      "1": numeric
                    "1.5": numeric
                "1.5E+27": numeric
                  " + 1 ": numeric
                 "+ 1.5 ": numeric
             "+ 1.5e+27 ": numeric
         "Math Constants": not numeric
      "3.141592653589793": numeric
      "2.718281828459045": numeric
     "-3.141592653589793": numeric
     "-2.718281828459045": numeric
      "3.141592653589793": numeric
      "2.718281828459045": numeric
      "Numeric Constants": not numeric
                    "NaN": not numeric
               "Infinity": not numeric
               "Infinity": not numeric

```



## Nim


```nim
import strutils
let s = "123"
var f: float
try:
  f = parseFloat s
except EInvalidValue:
  echo "not numeric"

if s.contains AllChars - Digits:
  echo "not a positive integer"
```


=={{header|Objective-C}}==
{{works with|GCC}}
{{works with|OpenStep}}
{{works with|GNUstep}}

The ''NSScanner'' class supports scanning of strings for various types. The ''scanFloat'' method will return YES if the string is numeric, even if the number is actually too long to be contained by the precision of a ''float''.


```objc
if( [[NSScanner scannerWithString:@"-123.4e5"] scanFloat:NULL] )
	NSLog( @"\"-123.4e5\" is numeric" );
else
	NSLog( @"\"-123.4e5\" is not numeric" );
if( [[NSScanner scannerWithString:@"Not a number"] scanFloat:NULL] )
	NSLog( @"\"Not a number\" is numeric" );
else
	NSLog( @"\"Not a number\" is not numeric" );
// prints: "-123.4e5" is numeric
// prints: "Not a number" is not numeric
```


The following function can be used to check if a string is numeric "totally"; this is achieved by checking if the scanner reached the end of the string after the float is parsed.


```objc
BOOL isNumeric(NSString *s)
{
   NSScanner *sc = [NSScanner scannerWithString: s];
   if ( [sc scanFloat:NULL] )
   {
      return [sc isAtEnd];
   }
   return NO;
}
```


If we want to scan ''by hand'', we could use a function like the following, that checks if a number is an integer positive or negative number; spaces can appear at the beginning, but not after the number, and
the '+' or '-' can appear only ''attached'' to the number ("+123" returns YES, but "+ 123" returns NO).


```objc
BOOL isNumericI(NSString *s)
{
   NSUInteger len = [s length];
   NSUInteger i;
   BOOL status = NO;

   for(i=0; i < len; i++)
   {
       unichar singlechar = [s characterAtIndex: i];
       if ( (singlechar == ' ') && (!status) )
       {
         continue;
       }
       if ( ( singlechar == '+' ||
              singlechar == '-' ) && (!status) ) { status=YES; continue; }
       if ( ( singlechar >= '0' ) &&
            ( singlechar <= '9' ) )
       {
          status = YES;
       } else {
          return NO;
       }
   }
   return (i == len) && status;
}
```


Here we assumed that in the internal encoding of a string (that should be Unicode), 1 comes after 0, 2 after 1 and so on until 9. Another way could be to get the C String from the NSString object, and then the parsing would be the same of the one we could do in standard C, so this path is not given.


## OCaml

This function is not particularly useful in a statically typed language. Instead, one would just attempt to convert the string
to the desired type and handle parsing failure appropriately.

The task doesn't define which strings are considered "numeric", so we do ints and floats, which should catch the most common cases:


```ocaml
let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

let is_float s =
  try ignore (float_of_string s); true
  with _ -> false

let is_numeric s = is_int s || is_float s
```



## Octave

The builtin function <tt>isnumeric</tt> return true (1) if the argument is a data of type ''number''; the provided function <tt>isnum</tt> works the same for numeric datatype, while if another type is passed as argument, it tries to convert it to a number; if the conversion fails, it means it is not a string representing a number.


```octave
function r = isnum(a)
  if ( isnumeric(a) )
    r = 1;
  else
    o = str2num(a);
    r = !isempty(o);
  endif
endfunction

% tests
disp(isnum(123))      % 1
disp(isnum("123"))    % 1
disp(isnum("foo123")) % 0
disp(isnum("123bar")) % 0
disp(isnum("3.1415")) % 1
```


STR2NUM uses internally the function eval(), therefore it should not be used for unsecured data (e.g. user input). Use instead str2double() or scanf().


```octave
function r = isnum(a)
  if ( isnumeric(a) )
    r = 1;
  else
    o = str2double(a);
    r = !isnan(o);
  endif
endfunction
```



## Oz


```oz
fun {IsNumeric S}
   {String.isInt S} orelse {String.isFloat S}
end
```



## PARI/GP


```parigp
isNumeric(s)={
  my(t=type(eval(s)));
  t == "t_INT" || t == "T_REAL"
};
```



## Pascal

See [[#Delphi|Delphi]] or [[#Free Pascal|Free Pascal]].


## PeopleCode


```PeopleCode

Built-In Function
Syntax

IsNumber(Value)
Description

Use the IsNumber function to determine if Value contains a valid numeric value. Numeric characters include sign indicators and comma and period decimal points.
To determine if a value is a number and if it's in the user's local format, use the IsUserNumber function.

Parameters

Value
Specify a string you want to search to determine if it is a valid number.

Returns
A Boolean value: True if Value contains a valid numeric value, False otherwise.

Example

&Value = Get Field().Value;
If IsNumber(&Value) Then
   /* do numeric processing */
Else
   /* do non-numeric processing */
End-if;

```



## Perl

{{works with|Perl|5.8}}

```perl
use Scalar::Util qw(looks_like_number);
print looks_like_number($str) ? "numeric" : "not numeric\n";
```


{{works with|Perl|5.8}}
Quoting from [http://perldoc.perl.org/perlfaq4.html#How-do-I-determine-whether-a-scalar-is-a-number%2fwhole%2finteger%2ffloat%3f perlfaq4]:

''How do I determine whether a scalar is a number/whole/integer/float?''

Assuming that you don't care about [[IEEE]] notations like "NaN" or "Infinity", you probably just want to use a [[Regular expression matching|regular expression]].


```perl
if (/\D/)            { print "has nondigits\n" }
if (/^\d+\z/)         { print "is a whole number\n" }
if (/^-?\d+\z/)       { print "is an integer\n" }
if (/^[+-]?\d+\z/)    { print "is a +/- integer\n" }
if (/^-?\d+\.?\d*\z/) { print "is a real number\n" }
if (/^-?(?:\d+(?:\.\d*)?&\.\d+)\z/) { print "is a decimal number\n" }
if (/^([+-]?)(?=\d&\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?\z/)
                     { print "a C float\n" }
```


There are also some commonly used modules for the task. Scalar::Util (distributed with 5.8) provides access to Perl's internal function "looks_like_number" for determining whether a variable looks like a number. Data::Types exports functions that validate data types using both the above and other regular expressions. Thirdly, there is "Regexp::Common" which has regular expressions to match various types of numbers. Those three modules are available from the CPAN.

If you're on a [[POSIX]] system, Perl supports the "POSIX::strtod" function. Its semantics are somewhat cumbersome, so here's a "getnum" wrapper function for more convenient access. This function takes a string and returns the number it found, or "undef" for input that isn't a C float. The "is_numeric" function is a front end to "getnum" if you just want to say, ''Is this a float?''


```perl
sub getnum {
    use POSIX;
    my $str = shift;
    $str =~ s/^\s+//;
    $str =~ s/\s+$//;
    $! = 0;
    my($num, $unparsed) = strtod($str);
    if (($str eq '') && ($unparsed != 0) && $!) {
        return undef;
    } else {
        return $num;
    }
}

sub is_numeric { defined getnum($_[0]) }
```


Or you could check out the String::Scanf module on the CPAN instead. The POSIX module (part of the standard Perl distribution) provides the "strtod" and "strtol" for converting strings to double and longs, respectively.


## Perl 6

{{works with|Rakudo|2017.11}}
Perl 6 tries very hard to DWIM (do what I mean). As part of that, numeric strings are by default stored as allomorphic types which can be used as numbers or strings without any conversion. If we truly want to operate on strings, we have to explicitly coerce the allomorphs to strings. A subtlety that may not be immediately apparent, whitespace, empty strings and null strings may be treated as (False) boolean values in Perl 6, however booleans are allomorphic to numeric, so empty strings will coerce to a numeric value (0), and return as numeric unless specifically checked for.

Note: These routines are usable for most cases but won't detect unicode non-digit numeric forms; E.G. vulgar fractions, Roman numerals, circled numbers, etc. If it is necessary to detect those as numeric, a full fledged grammar may be necessary.


```perl6
sub is-number-w-ws( Str $term --> Bool ) { # treat Falsey strings as numeric
    $term.Numeric !~~ Failure;
}

sub is-number-wo-ws( Str $term --> Bool ) { # treat Falsey strings as non-numeric
    ?($term ~~ / \S /) && $term.Numeric !~~ Failure;
}

say "               Coerce     Don't coerce";
say '    String   whitespace    whitespace';
printf "%10s  %8s  %11s\n",
"<$_>", .&is-number-w-ws, .&is-number-wo-ws for
(|<1 1.2 1.2.3 -6 1/2 12e B17 1.3e+12 1.3e12 -2.6e-3 zero
0x 0xA10 0b1001 0o16 0o18 2+5i>, '1 1 1', '', ' ').map: *.Str;
```



```txt
               Coerce     Don't coerce
    String   whitespace    whitespace
       <1>      True         True
     <1.2>      True         True
   <1.2.3>     False        False
      <-6>      True         True
     <1/2>      True         True
     <12e>     False        False
     <B17>     False        False
 <1.3e+12>      True         True
  <1.3e12>      True         True
 <-2.6e-3>      True         True
    <zero>     False        False
      <0x>     False        False
   <0xA10>      True         True
  <0b1001>      True         True
    <0o16>      True         True
    <0o18>     False        False
    <2+5i>      True         True
   <1 1 1>     False        False
        <>      True        False
       < >      True        False
```




## Phix


```Phix
function isNumber(string s)
    return scanf(s,"%f")!={}
end function
```

test code

```Phix
function testset(sequence s)
    for i=1 to length(s) do
        s[i] = isNumber(s[i])
    end for
    return s
end function
?testset({"#a","#A","0xA","0(16)A","#FF","255","0",".14",".05","-5.2","0xf","ten","1B"})
?testset({" 12  ",trim(" 12  ")})
?testset({"0o16","0o18"})
?testset({"1_000","0b10101111_11110000_11110000_00110011","-0b10101","0x10.5",""," ","1.","50e"})
```

{{out}}

```txt

{1,1,1,1,1,1,1,1,1,1,1,0,0}
{0,1}
{1,0}
{1,1,1,0,0,0,0,0}

```



## PHP


```php
<?php
$string = '123';
if(is_numeric(trim($string))) {
}
?>
```



## PicoLisp

The '[http://software-lab.de/doc/refF.html#format format]' function can
be used for that. It returns NIL if the given string is not a legal number

```PicoLisp
: (format "123")
-> 123

: (format "123a45")
-> NIL

: (format "-123.45" 4)
-> 1234500
```



## Pike

the sscanf format %f will find any kind of number.
the %s before and after make sure the number is not surrounded by other text.


```Pike

int(0..1) is_number(string s)
{
    array test = array_sscanf(s, "%s%f%s");
    if (sizeof(test) == 3 && test[1] && !sizeof(test[0]) && !sizeof(test[2]) )
        return true;
    else
        return false;
}

string num = "-1.234"
is_number(num);
-> true

```



## PL/I


```PL/I

is_numeric: procedure (text) returns (bit (1));
   declare text character (*);
   declare x float;

   on conversion go to done;

   get string(text) edit (x) (E(length(text),0));
   return ('1'b);

done:
   return ('0'b);
end is_numeric;
```


```txt

5                       '1'B
6.7                     '1'B
-8.9                    '1'B
-4e3                    '1'B
4A37                    '0'B

```



## PL/SQL


```plsql
FUNCTION IsNumeric( value IN VARCHAR2 )
RETURN BOOLEAN
IS
  help NUMBER;
BEGIN
  help := to_number( value );
  return( TRUE );
EXCEPTION
  WHEN others THEN
    return( FALSE );
END;
```



```plsql
Value VARCHAR2( 10 ) := '123';
IF( IsNumeric( Value ) )
  THEN
    NULL;
END  IF;
```



## PowerShell

Note: PowerShell 1.0 does not support 'try'
THis simply tries arithmetic with the argument and if that fails, ''false'' is returned.

```powershell
function isNumeric ($x) {
    try {
        0 + $x | Out-Null
        return $true
    } catch {
        return $false
    }
}
```


But this one doesn't work for strings like "8." though a . is appended it returns true!

Alternatively, you can use the static System.Int32.TryParse() method in the .NET framework.


```powershell
function isNumeric ($x) {
    $x2 = 0
    $isNum = [System.Int32]::TryParse($x, [ref]$x2)
    return $isNum
}
```



## Prolog

{{works with|SWI-Prolog|7}}

The code:


```prolog
numeric_string(String) :-
    atom_string(Atom, String),
    atom_number(Atom, _).
```


A predicate to test the code:


```prolog
test_strings(Strings) :-
    forall( member(String, Strings),
            ( ( numeric_string(String)
              ->  Result = a
              ;   Result = 'not a' ),
              format('~w is ~w number.~n', [String, Result])
            )
          ).
```


Example of using the test predicate:


```prolog
?- test_strings(["123", "0.123", "-123.1", "NotNum", "1."]).
123 is a number.
0.123 is a number.
-123.1 is a number.
NotNum is not a number.
1. is not a number.
true.
```



## PureBasic

This routine parses the string to verify it's a number. It returns 1 if string is numeric, 0 if it is not. The character used as the decimal separator may be specified if desired.

```PureBasic
Procedure IsNumeric(InString.s, DecimalCharacter.c = '.')
  #NotNumeric = #False
  #IsNumeric = #True

  InString = Trim(InString)
  Protected IsDecimal, CaughtDecimal, CaughtE
  Protected IsSignPresent, IsSignAllowed = #True, CountNumeric
  Protected *CurrentChar.Character = @InString

  While *CurrentChar\c
    Select *CurrentChar\c
      Case '0' To '9'
        CountNumeric + 1
        IsSignAllowed = #False
      Case DecimalCharacter
        If CaughtDecimal Or CaughtE Or CountNumeric = 0
          ProcedureReturn #NotNumeric
        EndIf

        CountNumeric = 0
        CaughtDecimal = #True
        IsDecimal = #True
      Case  '-', '+'
        If IsSignPresent Or Not IsSignAllowed: ProcedureReturn #NotNumeric: EndIf
        IsSignPresent = #True
      Case 'E', 'e'
        If CaughtE Or CountNumeric = 0
          ProcedureReturn #NotNumeric
        EndIf

        CaughtE = #True
        CountNumeric = 0
        CaughtDecimal = #False
        IsSignPresent = #False
        IsSignAllowed = #True
      Default
        ProcedureReturn #NotNumeric
    EndSelect
    *CurrentChar + SizeOf(Character)
  Wend

  If CountNumeric = 0: ProcedureReturn #NotNumeric: EndIf
  ProcedureReturn #IsNumeric
EndProcedure

If OpenConsole()
  PrintN("'+3183.31151E+321' = " + Str(IsNumeric("+3183.31151E+321")))
  PrintN("'-123456789' = " + Str(IsNumeric("-123456789")))
  PrintN("'123.45.6789+' = " + Str(IsNumeric("123.45.6789+")))
  PrintN("'-e' = " + Str(IsNumeric("-e")))
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
'+3183.31151E+321' = 1
'-123456789' = 1
'123.45.6789+' = 0
'-e' = 0
```



## Python


### Python: Simple int/float


```python
def is_numeric(s):
    try:
        float(s)
        return True
    except (ValueError, TypeError):
        return False

is_numeric('123.0')
```


Or for positive integers only:


```python
'123'.isdigit()
```



### Python: Most numeric literals

Including complex, hex, binary, and octal numeric literals we get:

```python
def is_numeric(literal):
    """Return whether a literal can be parsed as a numeric value"""
    castings = [int, float, complex,
        lambda s: int(s,2),  #binary
        lambda s: int(s,8),  #octal
        lambda s: int(s,16)] #hex
    for cast in castings:
        try:
            cast(literal)
            return True
        except ValueError:
            pass
    return False
```


Sample use, including value parsed, its type, and standard method str.isnumeric():

```python
def numeric(literal):
    """Return value of numeric literal or None if can't parse a value"""
    castings = [int, float, complex,
        lambda s: int(s,2),  #binary
        lambda s: int(s,8),  #octal
        lambda s: int(s,16)] #hex
    for cast in castings:
        try:
            return cast(literal)
        except ValueError:
            pass
    return None


tests = [
    '0', '0.', '00', '123', '0123', '+123', '-123', '-123.', '-123e-4', '-.8E-04',
    '0.123', '(5)', '-123+4.5j', '0b0101', ' +0B101 ', '0o123', '-0xABC', '0x1a1',
    '12.5%', '1/2', '½', '3¼', 'π', 'Ⅻ', '1,000,000', '1 000', '- 001.20e+02',
    'NaN', 'inf', '-Infinity']

for s in tests:
    print("%14s -> %-14s %-20s is_numeric: %-5s  str.isnumeric: %s" % (
        '"'+s+'"', numeric(s), type(numeric(s)), is_numeric(s), s.isnumeric() ))
```

{{out}}

```txt

           "0" -> 0              <class 'int'>        is_numeric: True   str.isnumeric: True
          "0." -> 0.0            <class 'float'>      is_numeric: True   str.isnumeric: False
          "00" -> 0              <class 'int'>        is_numeric: True   str.isnumeric: True
         "123" -> 123            <class 'int'>        is_numeric: True   str.isnumeric: True
        "0123" -> 123            <class 'int'>        is_numeric: True   str.isnumeric: True
        "+123" -> 123            <class 'int'>        is_numeric: True   str.isnumeric: False
        "-123" -> -123           <class 'int'>        is_numeric: True   str.isnumeric: False
       "-123." -> -123.0         <class 'float'>      is_numeric: True   str.isnumeric: False
     "-123e-4" -> -0.0123        <class 'float'>      is_numeric: True   str.isnumeric: False
     "-.8E-04" -> -8e-05         <class 'float'>      is_numeric: True   str.isnumeric: False
       "0.123" -> 0.123          <class 'float'>      is_numeric: True   str.isnumeric: False
         "(5)" -> (5+0j)         <class 'complex'>    is_numeric: True   str.isnumeric: False
   "-123+4.5j" -> (-123+4.5j)    <class 'complex'>    is_numeric: True   str.isnumeric: False
      "0b0101" -> 5              <class 'int'>        is_numeric: True   str.isnumeric: False
    " +0B101 " -> 5              <class 'int'>        is_numeric: True   str.isnumeric: False
       "0o123" -> 83             <class 'int'>        is_numeric: True   str.isnumeric: False
      "-0xABC" -> -2748          <class 'int'>        is_numeric: True   str.isnumeric: False
       "0x1a1" -> 417            <class 'int'>        is_numeric: True   str.isnumeric: False
       "12.5%" -> None           <class 'NoneType'>   is_numeric: False  str.isnumeric: False
         "1/2" -> None           <class 'NoneType'>   is_numeric: False  str.isnumeric: False
           "½" -> None           <class 'NoneType'>   is_numeric: False  str.isnumeric: True
          "3¼" -> None           <class 'NoneType'>   is_numeric: False  str.isnumeric: True
           "π" -> None           <class 'NoneType'>   is_numeric: False  str.isnumeric: False
           "Ⅻ" -> None           <class 'NoneType'>   is_numeric: False  str.isnumeric: True
   "1,000,000" -> None           <class 'NoneType'>   is_numeric: False  str.isnumeric: False
       "1 000" -> None           <class 'NoneType'>   is_numeric: False  str.isnumeric: False
"- 001.20e+02" -> None           <class 'NoneType'>   is_numeric: False  str.isnumeric: False
         "NaN" -> nan            <class 'float'>      is_numeric: True   str.isnumeric: False
         "inf" -> inf            <class 'float'>      is_numeric: True   str.isnumeric: False
   "-Infinity" -> -inf           <class 'float'>      is_numeric: True   str.isnumeric: False
```



## R



```R>
 strings <- c("152", "-3.1415926", "Foo123")
> suppressWarnings(!is.na(as.numeric(strings)))
[1]  TRUE  TRUE FALSE

```



## RapidQ


```RapidQ
isnumeric
$Typecheck on

Defint FALSE, TRUE

FALSE = 0
TRUE = NOT FALSE

Function isNumeric(s as string, optchar as string) as integer
    If len(s) = 0 then
        Result = FALSE
        Exit Function
    End If
    if instr(s,"+") > 1 then
        Result = FALSE
        exit function
    end if
    if instr(s,"-") > 1 then
        Result = FALSE
        exit function
    end if
    Defint i, ndex = 0
    For i = 1 to len(s)
        select case asc(mid$(s,i,1))
        case 43   '+
        case 45   '-
        case 46 '.
            if ndex = 1 then
                Result = FALSE
                Exit function
            end if
            ndex = 1
        case 48 to 57  '0 to 9
        case else
            if instr(optchar,(mid$(s,i,1))) = 0 then
                Result = FALSE
                exit function
            end if
        end select
    next
    Result = TRUE
End Function

'
### ======================================================

'Begin
'
### ======================================================


showmessage (str$(isNumeric("-152.34","")))
end

```



## REBOL


```REBOL

REBOL [
	Title: "Is Numeric?"
	URL: http://rosettacode.org/wiki/IsNumeric
]

; Built-in.

numeric?: func [x][not error? try [to-decimal x]]

; Parse dialect for numbers.

sign:   [0 1 "-"]
digit:  charset "0123456789"
int:    [some digit]
float:  [int "." int]
number: [
	sign float ["e" | "E"] sign int |
	sign int ["e" | "E"] sign int |
	sign float |
	sign int
]

pnumeric?: func [x][parse x number]

; Test cases.

cases: parse {
   10 -99
   10.43 -12.04
   1e99 1.0e10 -10e3 -9.12e7 2e-4 -3.4E-5
   3phase  Garkenhammer  e  n3v3r  phase3
} none
foreach x cases [print [x  numeric? x  pnumeric? x]]

```



## Retro

Retro does not have floating point numbers. For others, it provides '''isNumber?''':


```Retro
"123" isNumber?
```



## REXX


```rexx
/*REXX program to determine if a string is numeric. */
yyy=' -123.78'               /*or some such.*/

                             /*strings below are all numeric (REXX).*/
zzz='  -123.78 '
zzz='-123.78'
zzz='2'
zzz="2"
zzz=2
zzz='000000000004'
zzz='+5'
zzz='  +6  '
zzz='  +  7  '
zzz='  -  8  '
zzz='  -  .9 '
zzz='-  19.'
zzz='.7'
zzz='2e3'
zzz=47e567
zzz='2e-3'
zzz='1.2e1'
zzz='  .2E6'
zzz='  2.e5 '
zzz='       +1.2E0002  '
zzz='       +1.2e+002  '
zzz=' +0000001.200e+002  '
zzz='  -  000001.200e+002  '
zzz='  -  000008.201e-00000000000000002  '
ifx=i

/*Note:  some REXX interpreters allow use of tab chars as blanks.  */

                             /*all statements below are equivalent.*/

if \datatype(yyy,'n')       then say 'oops, not numeric:' yyy
if \datatype(yyy,'N')       then say 'oops, not numeric:' yyy
if ¬datatype(yyy,'N')       then say 'oops, not numeric:' yyy
if ¬datatype(yyy,'numeric') then say 'oops, not numeric:' yyy
if ¬datatype(yyy,'nim.') then say 'oops, not numeric:' yyy
if  datatype(yyy)\=='NUM'   then say 'oops, not numeric:' yyy
if  datatype(yyy)/=='NUM'   then say 'oops, not numeric:' yyy
if  datatype(yyy)¬=='NUM'   then say 'oops, not numeric:' yyy
if  datatype(yyy)¬= 'NUM'   then say 'oops, not numeric:' yyy

/*note: REXX only looks at the first char for DATATYPE's  2nd arg. */

/*note: some REXX interpreters don't support the ¬ (not) character.*/
/*note:   "    "       "         "      "    the /  as  negation.  */
```



## Ring


```ring

see isdigit("0123456789") + nl +        # print 1
    isdigit("0123a")                    # print 0

```



## Ruby


```ruby
def is_numeric?(s)
  begin
    Float(s)
  rescue
    false # not numeric
  else
    true # numeric
  end
end
```


or more compact:


```ruby
def is_numeric?(s)
    !!Float(s) rescue false
end
```

'''sample'''

```ruby
strings = %w(0 0.0 -123 abc 0x10 0xABC 123a -123e3 0.1E-5 50e)
strings.each do |str|
  puts "%9p => %s" % [str, is_numeric?(str)]
end
```

{{out}}

```txt

      "0" => true
    "0.0" => true
   "-123" => true
    "abc" => false
   "0x10" => true
  "0xABC" => true
   "123a" => false
 "-123e3" => true
 "0.1E-5" => true
    "50e" => false

```



## Run BASIC


```runbasic
print isNumeric("123")
print isNumeric("1ab")

' ------------------------
' Numeric Check
' 0 = bad
' 1 = good
' ------------------------
FUNCTION isNumeric(f$)
isNumeric = 1
f$	= trim$(f$)
if left$(f$,1) = "-" or left$(f$,1) = "+" then f$ = mid$(f$,2)
for i = 1 to len(f$)
        d$ = mid$(f$,i,1)
	if d$ = "," then goto [nxtDigit]
	if d$ = "." then
                if dot$ = "." then isNumeric = 0
		dot$	= "."
		goto [nxtDigit]
	end if
       if (d$ < "0") or (d$ > "9") then isNumeric = 0
[nxtDigit]
next i
END FUNCTION
```

```txt
123 1
1ab 0
```



## Rust


```rust
// This function is not limited to just numeric types but rather anything that implements the FromStr trait.
fn parsable<T: FromStr>(s: &str) -> bool {
    s.parse::<T>().is_ok()
}
```



## Scala


```scala

import scala.util.control.Exception.allCatch

def isNumber(s: String): Boolean = (allCatch opt s.toDouble).isDefined

```



```scala

def isNumeric(input: String): Boolean = input.forall(_.isDigit)

```


Or a more complete version, using a complex regular expression:

```scala

def isNumeric2(str: String): Boolean = {
  str.matches(s"""[+-]?((\d+(e\d+)?[lL]?)|(((\d+(\.\d*)?)|(\.\d+))(e\d+)?[fF]?))""")
}

```


Or using the built-in number parsing and catching exceptions:

```scala

def isNumeric(str: String): Boolean = {
  !throwsNumberFormatException(str.toLong) || !throwsNumberFormatException(str.toDouble)
}

def throwsNumberFormatException(f: => Any): Boolean = {
  try { f; false } catch { case e: NumberFormatException => true }
}

```



## Scheme

string->number returns #f when the string is not numeric and otherwise the number, which is non-#f and therefore true.

```scheme
(define (numeric? s) (string->number s))
```



## SQL PL

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:

```sql pl

--#SET TERMINATOR @

CREATE OR REPLACE FUNCTION IS_NUMERIC (
  IN STRING VARCHAR(10)
 ) RETURNS SMALLINT
 -- ) RETURNS BOOLEAN
 BEGIN
  DECLARE RET SMALLINT;
  -- DECLARE RET BOOLEAN;
  DECLARE TMP INTEGER;
  DECLARE CONTINUE HANDLER FOR SQLSTATE '22018'
    SET RET = 1;
    -- SET RET = FALSE;

  SET RET = 0;
  --SET RET = TRUE;
  SET TMP = INTEGER(STRING);
  RETURN RET;
 END @

VALUES IS_NUMERIC('5')@
VALUES IS_NUMERIC('0')@
VALUES IS_NUMERIC('-1')@
VALUES IS_NUMERIC('A')@
VALUES IS_NUMERIC('-')@
VALUES IS_NUMERIC('z')@
VALUES IS_NUMERIC('')@
VALUES IS_NUMERIC(' ')@

```

Output:

```txt

db2 -td@
db2 => BEGIN
...
db2 (cont.) => END @
DB20000I  The SQL command completed successfully.

VALUES IS_NUMERIC('5')

1
------
     0

  1 record(s) selected.


VALUES IS_NUMERIC('0')

1
------
     0

  1 record(s) selected.


VALUES IS_NUMERIC('-1')

1
------
     0

  1 record(s) selected.


VALUES IS_NUMERIC('A')

1
------
     1

  1 record(s) selected.


VALUES IS_NUMERIC('-')

1
------
     1

  1 record(s) selected.


VALUES IS_NUMERIC('z')

1
------
     1

  1 record(s) selected.


VALUES IS_NUMERIC('')

1
------
     1

  1 record(s) selected.


VALUES IS_NUMERIC(' ')

1
------
     1

  1 record(s) selected.

```



## Racket


```racket
(define (string-numeric? s) (number? (string->number s)))
```

Or, since all non-<tt>#f</tt> are true:

```racket
(define string-numeric? string->number)
```



## Seed7

The function isNumeric uses the function [http://seed7.sourceforge.net/libraries/scanstri.htm#getNumber%28inout_string%29 getNumber] from the library [http://seed7.sourceforge.net/libraries/scanstri.htm scanstri.s7i].
GetNumber reads a numeric literal from a string. The numeric literal is removed from the input string.


```seed7
$ include "seed7_05.s7i";
  include "scanstri.s7i";

const func boolean: isNumeric (in var string: stri) is func
  result
    var boolean: isNumeric is FALSE;
  local
    var string: numberStri is "";
  begin
    numberStri := getNumber(stri);
    isNumeric := stri = "";
  end func;
```



## Sidef

There is the the ''String.looks_like_number'' method, which returns true when a given strings looks like a number:

```ruby
say "0.1E-5".looks_like_number;       #=> true
```


Alternatively, we can use regular expressions to determine this:


```ruby
func is_numeric(s) {
    (s ~~ /^[+-]?+(?=\.?[0-9])[0-9_]*+(?:\.[0-9_]++)?(?:[Ee](?:[+-]?+[0-9_]+))?\z/) ||
    (s ~~ /^0(?:b[10_]*|x[0-9A-Fa-f_]*|[0-9_]+\b)\z/)
}
```


Sample:

```ruby
var strings = %w(0 0.0 -123 abc 0x10 0xABC 123a -123e3 0.1E-5 50e);
for str in strings {
    say ("%9s => %s" % (str, is_numeric(str)))
}
```

{{out}}

```txt

        0 => true
      0.0 => true
     -123 => true
      abc => false
     0x10 => true
    0xABC => true
     123a => false
   -123e3 => true
   0.1E-5 => true
      50e => false

```



## Simula

Simula uses the '&' instead of 'e' or 'E' for the exponent part of a floating point decimal number.


```simula
BEGIN

    BOOLEAN PROCEDURE ISNUMERIC(W); TEXT W;
    BEGIN
        BOOLEAN PROCEDURE MORE;
            MORE := W.MORE;
        CHARACTER PROCEDURE NEXT;
            NEXT := IF MORE THEN W.GETCHAR ELSE CHAR(0);
        CHARACTER PROCEDURE LAST;
            LAST := IF W.LENGTH = 0
                    THEN CHAR(0)
                    ELSE W.SUB(W.LENGTH,1).GETCHAR;
        CHARACTER CH;
        W.SETPOS(1);
        IF MORE THEN
        BEGIN
            CH := NEXT;
            IF CH = '-' OR CH = '+' THEN CH := NEXT;
            WHILE DIGIT(CH) DO CH := NEXT;
            IF CH = '.' THEN
            BEGIN
                CH := NEXT;
                IF NOT DIGIT(CH) THEN GOTO L;
                WHILE DIGIT(CH) DO CH := NEXT;
            END;
            IF CH = '&' THEN
            BEGIN
                CH := NEXT;
                IF CH = '-' OR CH = '+' THEN CH := NEXT;
                WHILE DIGIT(CH) DO CH := NEXT;
            END;
        END;
    L:  ISNUMERIC := (W.LENGTH > 0) AND THEN (NOT MORE) AND THEN DIGIT(LAST);
    END;

    REAL X;
    TEXT T;
    FOR X := 0, -3.1415, 2.768&+31, 5&10, .5, 5.&10 DO
    BEGIN
        OUTREAL(X, 10, 20);
        OUTIMAGE;
    END;

    OUTIMAGE;

    FOR T :- "0", "-3.1415", "2.768&+31", ".5", "5&22" DO
    BEGIN
        OUTTEXT(IF ISNUMERIC(T) THEN "    NUMERIC " ELSE "NOT NUMERIC ");
        OUTCHAR('"');
        OUTTEXT(T);
        OUTCHAR('"');
        IF T = "0" THEN OUTCHAR(CHAR(9)); OUTCHAR(CHAR(9));
        COMMENT PROBE ;
        X := T.GETREAL;
        OUTREAL(X, 10, 20);
        OUTIMAGE;
    END;

    OUTIMAGE;

    X := 5.&10;
   !X := 5&;
   !X := 5.;
    X := .5;
    FOR T :- "", "5.", "5&", "5&+", "5.&", "5.&-", "5.&10" DO
    BEGIN
        OUTTEXT(IF ISNUMERIC(T) THEN "    NUMERIC " ELSE "NOT NUMERIC ");
        OUTCHAR('"');
        OUTTEXT(T);
        OUTCHAR('"');
        OUTIMAGE;
    END;

END

```

{{out}}

```txt

    0.000000000&+000
   -3.141500000&+000
    2.768000000&+031
    5.000000000&+010
    5.000000000&-001
    5.100000000&+000

    NUMERIC "0"             0.000000000&+000
    NUMERIC "-3.1415"      -3.141500000&+000
    NUMERIC "2.768&+31"     2.768000000&+031
    NUMERIC ".5"            5.000000000&-001
    NUMERIC "5&22"          5.000000000&+022

NOT NUMERIC ""
NOT NUMERIC "5."
NOT NUMERIC "5&"
NOT NUMERIC "5&+"
NOT NUMERIC "5.&"
NOT NUMERIC "5.&-"
NOT NUMERIC "5.&10"

```



## Smalltalk

{{works with|GNU Smalltalk}}
The String class has the method <code>isNumeric</code>; this method (at least on version 3.0.4) does not recognize as number strings like '-123'! So I've written an extension...


```smalltalk
String extend [
  realIsNumeric [
     (self first = $+) |
     (self first = $-)
        ifTrue: [
           ^ (self allButFirst) isNumeric
        ]
        ifFalse: [
           ^ self isNumeric
        ]
  ]
]

{ '1234'.            "true"
  '3.14'. '+3.8111'. "true"
  '+45'.             "true"
  '-3.78'.           "true"
  '-3.78.23'. "false"
  '123e3'     "false: the notation is not recognized"
} do: [ :a | a realIsNumeric printNl ]
```


{{works with|Smalltalk/X}}
{{works with|GNU Smalltalk}}
(should work with all)

```smalltalk
(Number readFrom:(aString readStream) onError:[nil]) notNil
```

to handle radix numbers (such as 2r10111), use:

```smalltalk
(Scanner scanNumberFrom:(aString readStream)) notNil
```



## SNOBOL4

This task is easy in Snobol. Use the convert( ) function as a predicate returning success (T) or failure (F) for string to real conversion.


```Snobol4
        define('nchk(str)') :(nchk_end)
nchk    convert(str,'real') :s(return)f(freturn)
nchk_end

*       # Wrapper for testing
        define('isnum(str)') :(isnum_end)
isnum   isnum = 'F'; isnum = nchk(str) 'T'
        isnum = isnum ': ' str :(return)
isnum_end

*       # Test and display
        output = isnum('123')
        output = isnum('123.0')
        output = isnum('123.')
        output = isnum('-123')
        output = isnum('3.14159')
        output = isnum('1.2.3')
        output = isnum('abc')
        output = isnum('A440')
end
```


{{out}}

```txt
T: 123
T: 123.0
T: 123.
T: -123
T: 3.14159
F: 1.2.3
F: abc
F: A440
```



## SQL

{{works with|MS SQL|Server 2005}}

```sql
declare @s varchar(10)
set @s = '1234.56'

print isnumeric(@s) --prints 1 if numeric, 0 if not.

if isnumeric(@s)=1 begin print 'Numeric' end
else print 'Non-numeric'
```



## Standard ML


```sml
(* this function only recognizes integers in decimal format *)
fun isInteger s = case Int.scan StringCvt.DEC Substring.getc (Substring.full s) of
   SOME (_,subs) => Substring.isEmpty subs
 | NONE          => false

fun isReal s = case Real.scan Substring.getc (Substring.full s) of
   SOME (_,subs) => Substring.isEmpty subs
 | NONE          => false

fun isNumeric s = isInteger s orelse isReal s
```



## Swift

{{works with|Swift|2.x+}}

```swift
func isNumeric(a: String) -> Bool {
  return Double(a) != nil
}
```


{{works with|Swift|1.x}}
This one only checks whether it is an integer:

```swift
func isNumeric(a: String) -> Bool {
  return a.toInt() != nil
}
```



## Tcl


```tcl
if {
    [string is double -strict $varname]
} then {  ... }
```


Also <tt>string is integer</tt> (, <tt>string is alnum</tt> etc etc)


## Toka

Returns a flag of TRUE if character-string parameter represents a signed or unsigned integer. Otherwise returns a flag of FALSE. The success or failure is dependent on the source is valid in the current numeric base. The '''>number''' function also recognizes several optional prefixes for overriding the current base during conversion.


```toka
[ ( string -- flag )
  >number nip ] is isNumeric

( Some tests )
decimal
" 100" isNumeric .     ( succeeds, 100 is a valid decimal integer )
" 100.21" isNumeric .  ( fails, 100.21 is not an integer)
" a" isNumeric .       ( fails, 'a' is not a valid integer in the decimal base )
" $a" isNumeric .      ( succeeds, because $ is a valid override prefix )
                       ( denoting that the following character is a hexadecimal number )
```



## UNIX Shell


```bash

#!/bin/bash
isnum() {
  printf "%f" $1 >/dev/null 2>&1
}


check() {
  if isnum $1
  then
     echo "$1 is numeric"
  else
     echo "$1 is NOT numeric"
  fi
}

check 2
check -3
check +45.44
check -33.332
check 33.aa
check 3.3.3

```


{{out}}

```txt

2 is numeric
-3 is numeric
+45.44 is numeric
-33.332 is numeric
33.aa is NOT numeric
3.3.3 is NOT numeric

```



## Ursa


```ursa
def isnum (string str)
	try
		double str
		return true
	catch valueerror
		return false
	end try
end isnum
```



## VBA

In France, IsNumeric("123.45") return False.
So we have to check the "." or "," and replace it by the Application.DecimalSeparator.


```vb
Sub Main()
    Debug.Print Is_Numeric("")
    Debug.Print Is_Numeric("-5.32")
    Debug.Print Is_Numeric("-51,321 32")
    Debug.Print Is_Numeric("123.4")
    Debug.Print Is_Numeric("123,4")
    Debug.Print Is_Numeric("123;4")
    Debug.Print Is_Numeric("123.4x")
End Sub

Private Function Is_Numeric(s As String) As Boolean
Dim Separat As String, Other As String
    Separat = Application.DecimalSeparator
    Other = IIf(Separat = ",", ".", ",")
    Is_Numeric = IsNumeric(Replace(s, Other, Separat))
End Function
```



## VBScript


```vb
IsNumeric(Expr)
```


Returns a True if numeric and a false if not.


## Vedit macro language

This routine returns TRUE if there is numeric value at current cursor location.
Only signed and unsigned integers are recognized, in decimal, hex (preceded with 0x) or octal (preceded with 0o).
Remove the SUPPRESS option to evaluate an expression instead of single numeric value.

```vedit
:IS_NUMERIC:
if (Num_Eval(SUPPRESS)==0 && Cur_Char != '0') {
    Return(FALSE)
} else {
    Return(TRUE)
}
```



## Visual Basic .NET

{{works with|Visual Basic .NET|2005}}

```vbnet
Dim Value As String = "+123"

If IsNumeric(Value) Then
    PRINT "It is numeric."
End If
```



## XLISP

The inbuilt function <tt>STRING->NUMBER</tt> returns the numeric value of a string if it can. We discard this value and return the Boolean value 'true'; otherwise, the <tt>IF</tt> conditional will not be satisfied and will return 'false'.

```xlisp
(DEFUN NUMERICP (X)
    (IF (STRING->NUMBER X) T))
```



## zkl


```zkl
fcn isNum(text){
   try{ text.toInt(); True }
   catch{ try{ text.toFloat(); True }
          catch{ False }
   }
}
```


```txt

isNum("123.4")  //-->True
isNum("123")    //-->True
isNum("-123.4") //-->True
isNum("123.4x") //-->False
isNum("hoho")   //-->False
isNum(123.4)    //-->True
isNum(123)      //-->True

```



{{omit from|GUISS}}

[[Category: String manipulation]]
