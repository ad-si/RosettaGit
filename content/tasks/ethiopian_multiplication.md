+++
title = "Ethiopian multiplication"
description = ""
date = 2019-06-21T16:32:49Z
aliases = []
[extra]
id = 4576
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
languages = [
  "11l",
  "acl2",
  "actionscript",
  "ada",
  "aime",
  "algol_68",
  "algol_w",
  "applescript",
  "autohotkey",
  "autoit",
  "awk",
  "basic",
  "batch_file",
  "bbc_basic",
  "bracmat",
  "c",
  "c_shell",
  "clojure",
  "cobol",
  "coffeescript",
  "coldfusion",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dc",
  "e",
  "eiffel",
  "ela",
  "elixir",
  "emacs_lisp",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "false",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "hicest",
  "j",
  "java",
  "jsish",
  "julia",
  "kotlin",
  "liberty_basic",
  "limbo",
  "locomotive_basic",
  "logo",
  "lolcode",
  "lua",
  "matlab",
  "metafont",
  "microsoft_small_basic",
  "mmix",
  "mumps",
  "nemerle",
  "netrexx",
  "nim",
  "objeck",
  "object_pascal",
  "ocaml",
  "octave",
  "oforth",
  "ol",
  "oorexx",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pike",
  "pl_i",
  "pl_sql",
  "powerbuilder",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rascal",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "sinclair_zx81_basic",
  "smalltalk",
  "snobol4",
  "snusp",
  "soar",
  "swift",
  "tcl",
  "true_basic",
  "tuscript",
  "unix_shell",
  "ursala",
  "vba",
  "vbscript",
  "x86_assembly",
  "xbasic",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
+++

Ethiopian multiplication is a method of multiplying integers using only addition, doubling, and halving.


'''Method:'''

# Take two numbers to be multiplied and write them down at the top of two columns.
# In the left-hand column repeatedly halve the last number, discarding any remainders, and write the result below the last in the same column, until you write a value of 1.
# In the right-hand column repeatedly double the last number and write the result below. stop when you add a result in the same row as where the left hand column shows 1.
# Examine the table produced and discard any row where the value in the left column is even.
# Sum the values in the right-hand column that remain to produce the result of multiplying the original two numbers together



'''For example:'''   17 &times; 34
        17    34
Halving the first column:
        17    34
         8
         4
         2
         1
Doubling the second column:
        17    34
         8    68
         4   136
         2   272
         1   544
Strike-out rows whose first cell is even:
        17    34
         8    <strike>68</strike>
         4   <strike>136</strike>
         2   <strike>272</strike>
         1   544
Sum the remaining numbers in the right-hand column:
        17    34
         8    --
         4   ---
         2   ---
         1   544
            ====
             578
So 17 multiplied by 34, by the Ethiopian method is 578.


## Task

The task is to '''define three named functions'''/methods/procedures/subroutines:
# one to '''halve an integer''',
# one to '''double an integer''', and
# one to '''state if an integer is even'''.



Use these functions to '''create a function that does Ethiopian multiplication'''.


## References

*[http://www.bbc.co.uk/learningzone/clips/ethiopian-multiplication-explained/11232.html Ethiopian multiplication explained] (Video)
*[http://www.youtube.com/watch?v=Nc4yrFXw20Q A Night Of Numbers - Go Forth And Multiply] (Video)
*[http://www.ncetm.org.uk/blogs/3064 Ethiopian multiplication]
*[http://www.bbc.co.uk/dna/h2g2/A22808126 Russian Peasant Multiplication]
*[http://thedailywtf.com/Articles/Programming-Praxis-Russian-Peasant-Multiplication.aspx Programming Praxis: Russian Peasant Multiplication]





## 11l

```11l
F halve(x)
   R x I/ 2

F double_(x)
   R x * 2

F even(x)
   R !(x % 2)

F ethiopian(=multiplier, =multiplicand)
   V result = 0

   L multiplier >= 1
      I !even(multiplier)
         result += multiplicand
      multiplier = halve(multiplier)
      multiplicand = double_(multiplicand)

   R result

print(ethiopian(17, 34))
```


```txt
578
```



## ACL2


```Lisp
(include-book "arithmetic-3/top" :dir :system)

(defun halve (x)
   (floor x 2))

(defun double (x)
   (* x 2))

(defun is-even (x)
   (evenp x))

(defun multiply (x y)
   (if (zp (1- x))
       y
       (+ (if (is-even x)
              0
              y)
          (multiply (halve x) (double y)))))
```



## ActionScript

```ActionScript
function Divide(a:Number):Number {
	return ((a-(a%2))/2);
}
function Multiply(a:Number):Number {
	return (a *= 2);
}
function isEven(a:Number):Boolean {
	if (a%2 == 0) {
		return (true);
	} else {
		return (false);
	}
}
function Ethiopian(left:Number, right:Number) {
	var r:Number = 0;
	trace(left+"     "+right);
	while (left != 1) {
		var State:String = "Keep";
		if (isEven(Divide(left))) {
			State = "Strike";
		}
		trace(Divide(left)+"     "+Multiply(right)+"  "+State);
		left = Divide(left);
		right = Multiply(right);
		if (State == "Keep") {
			r += right;
		}
	}
	trace("="+"      "+r);
}
}
```
ex. Ethiopian(17,34);
 17     34
 8     68  Strike
 4     136  Strike
 2     272  Strike
 1     544  Keep


## Ada


```Ada

with ada.text_io;use ada.text_io;

procedure ethiopian is
  function double  (n : Natural) return Natural is (2*n);
  function halve   (n : Natural) return Natural is (n/2);
  function is_even (n : Natural) return Boolean is (n mod 2 = 0);

  function mul (l, r : Natural) return Natural is
  (if l = 0 then 0 elsif l = 1 then r elsif is_even (l) then mul (halve (l),double (r))
   else r + double (mul (halve (l), r)));

begin
  put_line (mul (17,34)'img);
end ethiopian;
```



## Aime

```aime
void
halve(integer &x)
{
    x >>= 1;
}

void
double(integer &x)
{
    x <<= 1;
}

integer
iseven(integer x)
{
    return (x & 1) == 0;
}

integer
ethiopian(integer plier, integer plicand, integer tutor)
{
    integer result;

    result = 0;

    if (tutor) {
        o_form("ethiopian multiplication of ~ by ~\n", plier, plicand);
    }

    while (plier >= 1) {
        if (iseven(plier)) {
            if (tutor) {
                o_form("/w4/ /w6/ struck\n", plier, plicand);
            }
        } else {
            if (tutor) {
                o_form("/w4/ /w6/ kept\n", plier, plicand);
            }

            result += plicand;
        }

        halve(plier);
        double(plicand);
    }

    return result;
}

integer
main(void)
{
    o_integer(ethiopian(17, 34, 1));
    o_byte('\n');

    return 0;
}
```

  17     34 kept
   8     68 struck
   4    136 struck
   2    272 struck
   1    544 kept
 578


## ALGOL 68

<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 - missing printf and FORMAT}} -->

```algol68
PROC halve = (REF INT x)VOID: x := ABS(BIN x SHR 1);
PROC doublit = (REF INT x)VOID: x := ABS(BIN x SHL 1);
PROC iseven = (#CONST# INT x)BOOL: NOT ODD x;

PROC ethiopian = (INT in plier,
              INT in plicand, #CONST# BOOL tutor)INT:
(
  INT plier := in plier, plicand := in plicand;
  INT result:=0;

  IF tutor THEN
    printf(($"ethiopian multiplication of "g(0)," by "g(0)l$, plier, plicand)) FI;

  WHILE plier >= 1 DO
    IF iseven(plier) THEN
      IF tutor THEN printf(($" "4d,"  "6d" struck"l$, plier, plicand)) FI
    ELSE
      IF tutor THEN printf(($" "4d,"  "6d" kept"l$, plier, plicand)) FI;
      result +:= plicand
    FI;
    halve(plier); doublit(plicand)
  OD;
  result
);

main:
(
  printf(($g(0)l$, ethiopian(17, 34, TRUE)))
)
```
 ethiopian multiplication of 17 by 34
 0017  000034 kept
 0008  000068 struck
 0004  000136 struck
 0002  000272 struck
 0001  000544 kept
 578


## ALGOL W


```algolw
begin
    % returns half of a %
    integer procedure halve  ( integer value a ) ; a div 2;
    % returns a doubled %
    integer procedure double ( integer value a ) ; a * 2;
    % returns true if a is even, false otherwise %
    logical procedure even   ( integer value a ) ; not odd( a );
    % returns the product of a and b using ethopian multiplication %
    % rather than keep a table of the intermediate results,        %
    % we examine then as they are generated                        %
    integer procedure ethopianMultiplication ( integer value a, b ) ;
    begin
        integer v, r, accumulator;
        v           := a;
        r           := b;
        accumulator := 0;
        i_w := 4; s_w := 0; % set output formatting %
        while begin
            write( v );
            if even( v ) then writeon( "    ---" )
            else begin
                accumulator := accumulator + r;
                writeon( "   ", r );
            end;
            v := halve( v );
            r := double( r );
            v > 0
        end do begin end;
        write( "      =====" );
        accumulator
    end ethopianMultiplication ;
    % task test case %
    begin
        integer m;
        m := ethopianMultiplication( 17, 34 );
        write( "       ", m )
    end
end.
```

```txt

  17     34
   8    ---
   4    ---
   2    ---
   1    544
      =====
        578

```



## AppleScript


Note that this algorithm, already described in the Rhind Papyrus (c. BCE 1650), can be used to multiply strings as well as integers, if we change the identity element from 0 to the empty string, and replace integer addition with string concatenation.

See also: [[Repeat_a_string#AppleScript]]


```AppleScript
on run
    {ethMult(17, 34), ethMult("Rhind", 9)}

    --> {578, "RhindRhindRhindRhindRhindRhindRhindRhind"}
end run


-- Int -> Int -> Int
-- or
-- Int -> String -> String
on ethMult(m, n)
    script fns
        property identity : missing value
        property plus : missing value

        on half(n) -- 1. half an integer (div 2)
            n div 2
        end half

        on double(n) -- 2. double (add to self)
            plus(n, n)
        end double

        on isEven(n) -- 3. is n even ? (mod 2 > 0)
            (n mod 2) > 0
        end isEven

        on chooseFns(c)
            if c is string then
                set identity of fns to ""
                set plus of fns to plusString of fns
            else
                set identity of fns to 0
                set plus of fns to plusInteger of fns
            end if
        end chooseFns

        on plusInteger(a, b)
            a + b
        end plusInteger

        on plusString(a, b)
            a & b
        end plusString
    end script

    chooseFns(class of m) of fns


    -- MAIN PROCESS OF CALCULATION

    set o to identity of fns
    if n < 1 then return o

    repeat while (n > 1)
        if isEven(n) of fns then -- 3. is n even ? (mod 2 > 0)
            set o to plus(o, m) of fns
        end if
        set n to half(n) of fns -- 1. half an integer (div 2)
        set m to double(m) of fns -- 2. double  (add to self)
    end repeat
    return plus(o, m) of fns
end ethMult
```


```txt
{578, "RhindRhindRhindRhindRhindRhindRhindRhindRhind"}
```



## AutoHotkey


```AutoHotkey
MsgBox % Ethiopian(17, 34) "`n" Ethiopian2(17, 34)

; func definitions:
half( x ) {
	return x >> 1
}

double( x ) {
	return x << 1
}

isEven( x ) {
	return x & 1 == 0
}

Ethiopian( a, b ) {
	r := 0
	While (a >= 1) {
		if !isEven(a)
			r += b
		a := half(a)
		b := double(b)
	}
	return r
}

; or a recursive function:
Ethiopian2( a, b, r = 0 ) { ;omit r param on initial call
	return a==1 ? r+b : Ethiopian2( half(a), double(b), !isEven(a) ? r+b : r )
}
```



## AutoIt


```AutoIt

Func Halve($x)
	Return Int($x/2)
EndFunc

Func Double($x)
	Return ($x*2)
EndFunc

Func IsEven($x)
	Return (Mod($x,2) == 0)
EndFunc

; this version also supports negative parameters
Func Ethiopian($nPlier, $nPlicand, $bTutor = True)
	Local $nResult = 0
	If ($nPlier < 0) Then
		$nPlier =- $nPlier
		$nPlicand =- $nPlicand
	ElseIf ($nPlicand > 0) And ($nPlier > $nPlicand) Then
		$nPlier = $nPlicand
		$nPlicand = $nPlier
	EndIf
	If $bTutor Then _
    ConsoleWrite(StringFormat("Ethiopian multiplication of %d by %d...\n", $nPlier, $nPlicand))
	While ($nPlier >= 1)
		If Not IsEven($nPlier) Then
			$nResult += $nPlicand
			If $bTutor Then ConsoleWrite(StringFormat("%d\t%d\tKeep\n", $nPlier, $nPlicand))
		Else
			If $bTutor Then ConsoleWrite(StringFormat("%d\t%d\tStrike\n", $nPlier, $nPlicand))
		EndIf
		$nPlier = Halve($nPlier)
		$nPlicand = Double($nPlicand)
	WEnd
	If $bTutor Then ConsoleWrite(StringFormat("Answer = %d\n", $nResult))
	Return $nResult
EndFunc

MsgBox(0, "Ethiopian multiplication of 17 by 34", Ethiopian(17, 34) )

```



## AWK

Implemented without the tutor.

```awk
function halve(x)
{
  return int(x/2)
}

function double(x)
{
  return x*2
}

function iseven(x)
{
  return x%2 == 0
}

function ethiopian(plier, plicand)
{
  r = 0
  while(plier >= 1) {
    if ( !iseven(plier) ) {
      r += plicand
    }
    plier = halve(plier)
    plicand = double(plicand)
  }
  return r
}

BEGIN {
  print ethiopian(17, 34)
}
```



## BASIC


=
## BASIC
=
Works with QBasic. While building the table, it's easier to simply not print unused values, rather than have to go back and strike them out afterward. (Both that and the actual adding happen in the "IF NOT (isEven(x))" block.)


```qbasic
DECLARE FUNCTION half% (a AS INTEGER)
DECLARE FUNCTION doub% (a AS INTEGER)
DECLARE FUNCTION isEven% (a AS INTEGER)

DIM x AS INTEGER, y AS INTEGER, outP AS INTEGER

x = 17
y = 34

DO
    PRINT x,
    IF NOT (isEven(x)) THEN
        outP = outP + y
        PRINT y
    ELSE
        PRINT
    END IF
    IF x < 2 THEN EXIT DO
    x = half(x)
    y = doub(y)
LOOP

PRINT " =", outP

FUNCTION doub% (a AS INTEGER)
    doub% = a * 2
END FUNCTION

FUNCTION half% (a AS INTEGER)
    half% = a \ 2
END FUNCTION

FUNCTION isEven% (a AS INTEGER)
    isEven% = (a MOD 2) - 1
END FUNCTION
```
 17            34
 8
 4
 2
 1             544
 =             578

=
## BBC BASIC
=

```bbcbasic
      x% = 17
      y% = 34

      REPEAT
        IF NOT FNeven(x%) THEN
          p% += y%
          PRINT x%, y%
        ELSE
          PRINT x%, "       ---"
        ENDIF
        x% = FNhalve(x%)
        y% = FNdouble(y%)
      UNTIL x% = 0
      PRINT " " , "       ==="
      PRINT " " , p%
      END

      DEF FNdouble(A%) = A% * 2

      DEF FNhalve(A%) = A% DIV 2

      DEF FNeven(A%) = ((A% AND 1) = 0)
```
        17        34
         8       ---
         4       ---
         2       ---
         1       544
                 ===
                 578

=
## FreeBASIC
=

```FreeBASIC
Function double_(y As String) As String
    Var answer="0"+y
    Var addcarry=0
    For n_ As Integer=Len(y)-1 To 0 Step -1
        Var addup=y[n_]+y[n_]-96
        answer[n_+1]=(addup+addcarry) Mod 10+48
        addcarry=(-(10<=(addup+addcarry)))
    Next n_
    answer[0]=addcarry+48
    Return Ltrim(answer,"0")
End Function

Function Accumulate(NUM1 As String,NUM2 As String) As String
    Var three="0"+NUM1
    Var two=String(len(NUM1)-len(NUM2),"0")+NUM2
    Var addcarry=0
    For n2 As Integer=len(NUM1)-1 To 0 Step -1
        Var addup=two[n2]+NUM1[n2]-96
        three[n2+1]=(addup+addcarry) Mod 10+48
        addcarry=(-(10<=(addup+addcarry)))
    Next n2
    three[0]=addcarry+48
    three=Ltrim(three,"0")
    If three="" Then Return "0"
    Return three
End Function

Function Half(Byref x As String) As String
    Var carry=0
    For z As Integer=0 To Len(x)-1
        Var temp=(x[z]-48+carry)
        Var main=temp Shr 1
        carry=(temp And 1) Shl 3 +(temp And 1) Shl 1
        x[z]=main+48
    Next z
    x= Ltrim(x,"0")
    Return x
End Function

Function IsEven(x As String) As Integer
    If x[Len(x)-1] And 1  Then Return 0
    return -1
End Function

Function EthiopianMultiply(n1 As String,n2 As String) As String
    Dim As String x=n1,y=n2
    If Len(y)>Len(x) Then Swap y,x
    'set the largest one to be halfed
    If Len(y)=Len(x) Then
        If x<y Then Swap y,x
    End If
    Dim As String ans
    Dim As String temprint,odd
    While x<>""
        temprint=""
        odd=""
        If  not IsEven(x) Then
            temprint=" *"
            odd=" <-- odd"
            ans=Accumulate(y,ans)
        End If
        Print x;odd;tab(30);y;temprint
        x=Half(x)
        y= Double_(y)
    Wend
    Return ans
End Function
'
### ==============  Example =================

Print
Dim As String s1="17"
Dim As String s2="34"
Print "Half";tab(30);"Double     * marks those accumulated"
print "Biggest";tab(30);"Smallest"


Print

Var ans= EthiopianMultiply(s1,s2)

Print
Print
Print "Final answer"
Print " ";ans
print "Float check"
Print Val(s1)*Val(s2)

Sleep

```
note: algorithm uses strings instead of integers
```txt
Half                         Double     * marks those accumulated
Biggest                      Smallest

34                           17
17 <-- odd                   34 *
8                            68
4                            136
2                            272
1 <-- odd                    544 *

Final answer
 578
Float check
 578
```


==={{header|GW-BASIC}}===

```qbasic
10 DEF FNE(A)=(A+1) MOD 2
20 DEF FNH(A)=INT(A/2)
30 DEF FND(A)=2*A
40 X=17:Y=34:TOT=0
50 WHILE X>=1
60 PRINT X,
70 IF FNE(X)=0 THEN TOT=TOT+Y:PRINT Y ELSE PRINT
80 X=FNH(X):Y=FND(Y)
90 WEND
100 PRINT "=", TOT
```


=
## Liberty BASIC
=

```lb
x = 17
y = 34
msg$ = str$(x) + " * " + str$(y) + " = "
Print str$(x) + "    " + str$(y)
'In this routine we will not worry about discarding the right hand value whos left hand partner is even;
'we will just not add it to our product.
Do Until x < 2
    If Not(isEven(x)) Then
        product = (product + y)
    End If
    x = halveInt(x)
    y = doubleInt(y)
    Print str$(x) + "    " + str$(y)
Loop
product = (product + y)
If (x < 0) Then product = (product * -1)
Print msg$ + str$(product)

Function isEven(num)
    isEven = Abs(Not(num Mod 2))
End Function

Function halveInt(num)
    halveInt = Int(num/ 2)
End Function

Function doubleInt(num)
    doubleInt = Int(num * 2)
End Function
```


=
## Microsoft Small Basic
=

```microsoftsmallbasic

x = 17
y = 34
tot = 0
While x >= 1
  TextWindow.Write(x)
  TextWindow.CursorLeft = 10
  If Math.Remainder(x + 1, 2) = 0 Then
    tot = tot + y
    TextWindow.WriteLine(y)
  Else
    TextWindow.WriteLine("")
  EndIf
  x = Math.Floor(x / 2)
  y = 2 * y
EndWhile
TextWindow.Write("=")
TextWindow.CursorLeft = 10
TextWindow.WriteLine(tot)

```


=
## PureBasic
=

```PureBasic
Procedure isEven(x)
  ProcedureReturn (x & 1) ! 1
EndProcedure

Procedure halveValue(x)
  ProcedureReturn x / 2
EndProcedure

Procedure doubleValue(x)
  ProcedureReturn x << 1
EndProcedure

Procedure EthiopianMultiply(x, y)
  Protected sum
  Print("Ethiopian multiplication of " + Str(x) + " and " + Str(y) + " ... ")
  Repeat
    If Not isEven(x)
      sum + y
    EndIf
    x = halveValue(x)
    y = doubleValue(y)
  Until x < 1
  PrintN(" equals " + Str(sum))
  ProcedureReturn sum
EndProcedure

If OpenConsole()
  EthiopianMultiply(17,34)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

 Ethiopian multiplication of 17 and 34 ...  equals 578
It became apparent that according to the way the Ethiopian method is described above it can't produce a correct result if the first multiplicand (the one being repeatedly halved) is negative. I've addressed that in this variation. If the first multiplicand is negative then the resulting sum (which may already be positive or negative) is negated.

```PureBasic
Procedure isEven(x)
  ProcedureReturn (x & 1) ! 1
EndProcedure

Procedure halveValue(x)
  ProcedureReturn x / 2
EndProcedure

Procedure doubleValue(x)
  ProcedureReturn x << 1
EndProcedure

Procedure EthiopianMultiply(x, y)
  Protected sum, sign = x

  Print("Ethiopian multiplication of " + Str(x) + " and " + Str(y) + " ...")
  Repeat
    If Not isEven(x)
      sum + y
    EndIf
    x = halveValue(x)
    y = doubleValue(y)
  Until x = 0
  If sign < 0 : sum * -1: EndIf

  PrintN(" equals " + Str(sum))
  ProcedureReturn sum
EndProcedure

If OpenConsole()
  EthiopianMultiply(17,34)
  EthiopianMultiply(-17,34)
  EthiopianMultiply(-17,-34)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

 Ethiopian multiplication of 17 and 34 ... equals 578
 Ethiopian multiplication of -17 and 34 ... equals -578
 Ethiopian multiplication of -17 and -34 ... equals 578

=
## Sinclair ZX81 BASIC
=
Requires at least 2k of RAM. The specification is emphatic about wanting named functions: in a language where user-defined functions do not exist, the best we can do is to use subroutines and assign their line numbers to variables. This allows us to <code>GOSUB HALVE</code> instead of having to <code>GOSUB 320</code>. (It would however be more idiomatic to avoid using subroutines at all, for simple operations like these, and to refer to them by line number if they were used.)

```basic
 10 LET HALVE=320
 20 LET DOUBLE=340
 30 LET EVEN=360
 40 DIM L(20)
 50 DIM R(20)
 60 INPUT L(1)
 70 INPUT R(1)
 80 LET I=1
 90 PRINT L(1),R(1)
100 IF L(I)=1 THEN GOTO 200
110 LET I=I+1
120 IF I>20 THEN STOP
130 LET X=L(I-1)
140 GOSUB HALVE
150 LET L(I)=Y
160 LET X=R(I-1)
170 GOSUB DOUBLE
180 LET R(I)=Y
190 GOTO 90
200 FOR K=1 TO I
210 LET X=L(K)
220 GOSUB EVEN
230 IF NOT Y THEN GOTO 260
240 LET R(K)=0
250 PRINT AT K-1,16;"          "
260 NEXT K
270 LET A=0
280 FOR K=1 TO I
290 LET A=A+R(K)
300 NEXT K
310 GOTO 380
320 LET Y=INT (X/2)
330 RETURN
340 LET Y=X*2
350 RETURN
360 LET Y=X/2=INT (X/2)
370 RETURN
380 PRINT AT I+1,16;A
```

```txt
17
34
```

```txt
17              34
8
4
2
1               544

                578
```


=
## True BASIC
=
A translation of BBC BASIC. True BASIC does not have Boolean operations built-in.

```basic

!RosettaCode: Ethiopian Multiplication
! True BASIC v6.007
PROGRAM EthiopianMultiplication
	DECLARE DEF FNdouble
	DECLARE DEF FNhalve
	DECLARE DEF FNeven

	LET x = 17
	LET y = 34

	DO
		IF FNeven(x) = 0 THEN
			LET p = p + y
			PRINT x,y
		ELSE
			PRINT x," ---"
		END IF

		LET x = FNhalve(x)
		LET y = FNdouble(y)
	LOOP UNTIL x = 0
	PRINT " ", " ==="
	PRINT " ", p
	GET KEY done

	DEF FNdouble(A) = A * 2
	DEF FNhalve(A) = INT(A / 2)
	DEF FNeven(A) = MOD(A+1,2)
END


```


=
## XBasic
=
```xbasic

PROGRAM "ethmult"
VERSION "0.0000"

DECLARE FUNCTION Entry()
INTERNAL FUNCTION Double(@a&&)
INTERNAL FUNCTION Halve(@a&&)
INTERNAL FUNCTION IsEven(a&&)

FUNCTION Entry()
  x&& = 17
  y&& = 34
  tot&& = 0
  DO WHILE x&& >= 1
    PRINT FORMAT$("#########", x&&);
    PRINT " ";
    IFF IsEven(x&&) THEN
      tot&& = tot&& + y&&
      PRINT FORMAT$("#########", y&&);
    END IF
    PRINT
    Halve(@x&&)
    Double(@y&&)
  LOOP
  PRINT "=         ";
  PRINT FORMAT$("#########", tot&&);
  PRINT
END FUNCTION

FUNCTION Double(a&&)
  a&& = 2 * a&&
END FUNCTION

FUNCTION Halve(a&&)
  a&& = a&& / 2
END FUNCTION

FUNCTION IsEven(a&&)
  RETURN a&& MOD 2 = 0
END FUNCTION
END PROGRAM

```

```txt

       17        34
        8
        4
        2
        1       544
=               578

```



## Batch File


```dos

@echo off
:: Pick 2 random, non-zero, 2-digit numbers to send to :_main
set /a param1=%random% %% 98 + 1
set /a param2=%random% %% 98 + 1
call:_main %param1% %param2%
pause>nul
exit /b

:: This is the main function that outputs the answer in the form of "%1 * %2 = %answer%"
:_main
setlocal enabledelayedexpansion
set l0=%1
set r0=%2
set leftcount=1
set lefttempcount=0
set rightcount=1
set righttempcount=0

:: Creates an array ("l[]") with the :_halve function. %l0% is the initial left number parsed
:: This section will loop until the most recent member of "l[]" is equal to 0
:left
set /a lefttempcount=%leftcount%-1
if !l%lefttempcount%!==1 goto right
call:_halve !l%lefttempcount%!
set l%leftcount%=%errorlevel%
set /a leftcount+=1
goto left

:: Creates an array ("r[]") with the :_double function, %r0% is the initial right number parsed
:: This section will loop until it has the same amount of entries as "l[]"
:right
set /a righttempcount=%rightcount%-1
if %rightcount%==%leftcount% goto both
call:_double !r%righttempcount%!
set r%rightcount%=%errorlevel%
set /a rightcount+=1
goto right

:both
:: Creates an boolean array ("e[]") corresponding with whether or not the respective "l[]" entry is even
for /l %%i in (0,1,%lefttempcount%) do (
  call:_even !l%%i!
  set e%%i=!errorlevel!
)

:: Adds up all entries of "r[]" based on the value of "e[]", respectively
set answer=0
for /l %%i in (0,1,%lefttempcount%) do (
  if !e%%i!==1 (
    set /a answer+=!r%%i!
  :: Everything from this-----------------------------
    set iseven%%i=KEEP
  ) else (
    set iseven%%i=STRIKE
  )
  echo L: !l%%i! R: !r%%i! - !iseven%%i!
  :: To this, is for cosmetics and is optional--------

)
echo %l0% * %r0% = %answer%
exit /b

:: These are the three functions being used. The output of these functions are expressed in the errorlevel that they return
:_halve
setlocal
set /a temp=%1/2
exit /b %temp%

:_double
setlocal
set /a temp=%1*2
exit /b %temp%

:_even
setlocal
set int=%1
set /a modint=%int% %% 2
exit /b %modint%

```


```txt

L: 17 R: 34 - KEEP
L: 8 R: 68 - STRIKE
L: 4 R: 136 - STRIKE
L: 2 R: 272 - STRIKE
L: 1 R: 544 - KEEP
17 * 34 = 578

```



## Bracmat


```bracmat
( (halve=.div$(!arg.2))
& (double=.2*!arg)
& (isEven=.mod$(!arg.2):0)
& ( mul
  =   a b as bs newbs result
    .   !arg:(?as.?bs)
      &   whl
        ' ( !as:? (%@:~1:?a)
          & !as halve$!a:?as
          & !bs:? %@?b
          & !bs double$!b:?bs
          )
      & :?newbs
      &   whl
        ' ( !as:%@?a ?as
          & !bs:%@?b ?bs
          & (isEven$!a|!newbs !b:?newbs)
          )
      & 0:?result
      &   whl
        ' (!newbs:%@?b ?newbs&!b+!result:?result)
      & !result
  )
& out$(mul$(17.34))
);
```

Output

```txt
578
```



## C


```c
#include <stdio.h>
#include <stdbool.h>

void halve(int *x) { *x >>= 1; }
void doublit(int *x)  { *x <<= 1; }
bool iseven(const int x) { return (x & 1) ==  0; }

int ethiopian(int plier,
	      int plicand, const bool tutor)
{
  int result=0;

  if (tutor)
    printf("ethiopian multiplication of %d by %d\n", plier, plicand);

  while(plier >= 1) {
    if ( iseven(plier) ) {
      if (tutor) printf("%4d %6d struck\n", plier, plicand);
    } else {
      if (tutor) printf("%4d %6d kept\n", plier, plicand);
      result += plicand;
    }
    halve(&plier); doublit(&plicand);
  }
  return result;
}

int main()
{
  printf("%d\n", ethiopian(17, 34, true));
  return 0;
}
```


## C#

```c#

using System;
using System.Linq;

namespace RosettaCode.Tasks
{
	public static class EthiopianMultiplication_Task
	{
		public static void Test ( )
		{
			Console.WriteLine ( "Ethiopian Multiplication" );
			int A = 17, B = 34;
			Console.WriteLine ( "Recursion: {0}*{1}={2}", A, B, EM_Recursion ( A, B ) );
			Console.WriteLine ( "Linq: {0}*{1}={2}", A, B, EM_Linq ( A, B ) );
			Console.WriteLine ( "Loop: {0}*{1}={2}", A, B, EM_Loop ( A, B ) );
			Console.WriteLine ( );
		}

		public static int Halve ( this int p_Number )
		{
			return p_Number >> 1;
		}
		public static int Double ( this int p_Number )
		{
			return p_Number << 1;
		}
		public static bool IsEven ( this int p_Number )
		{
			return ( p_Number % 2 ) == 0;
		}

		public static int EM_Recursion ( int p_NumberA, int p_NumberB )
		{
			//     Anchor Point,                Recurse to find the next row                                 Sum it with the second number according to the rules
			return p_NumberA == 1 ? p_NumberB : EM_Recursion ( p_NumberA.Halve ( ), p_NumberB.Double ( ) ) + ( p_NumberA.IsEven ( ) ? 0 : p_NumberB );
		}
		public static int EM_Linq ( int p_NumberA, int p_NumberB )
		{
			// Creating a range from 1 to x where x the number of times p_NumberA can be halved.
			// This will be 2^x where 2^x <= p_NumberA. Basically, ln(p_NumberA)/ln(2).
			return Enumerable.Range ( 1, Convert.ToInt32 ( Math.Log ( p_NumberA, Math.E ) / Math.Log ( 2, Math.E ) ) + 1 )
				// For every item (Y) in that range, create a new list, comprising the pair (p_NumberA,p_NumberB) Y times.
				.Select ( ( item ) => Enumerable.Repeat ( new { Col1 = p_NumberA, Col2 = p_NumberB }, item )
					// The aggregate method iterates over every value in the target list, passing the accumulated value and the current item's value.
					.Aggregate ( ( agg_pair, orig_pair ) => new { Col1 = agg_pair.Col1.Halve ( ), Col2 = agg_pair.Col2.Double ( ) } ) )
				// Remove all even items
				.Where ( pair => !pair.Col1.IsEven ( ) )
				// And sum!
				.Sum ( pair => pair.Col2 );
		}
		public static int EM_Loop ( int p_NumberA, int p_NumberB )
		{
			int RetVal = 0;
			while ( p_NumberA >= 1 )
			{
				RetVal += p_NumberA.IsEven ( ) ? 0 : p_NumberB;
				p_NumberA = p_NumberA.Halve ( );
				p_NumberB = p_NumberB.Double ( );
			}
			return RetVal;
		}
	}
}
```



## C++

Using C++ templates, these kind of tasks can be implemented as meta-programs.
The program runs at compile time, and the result is statically
saved into regularly compiled code.
Here is such an implementation without tutor, since there is no mechanism in C++ to output
messages during program compilation.

```cpp
template<int N>

struct Half
{
        enum { Result = N >> 1 };
};

template<int N>
struct Double
{
        enum { Result = N << 1 };
};

template<int N>
struct IsEven
{
        static const bool Result = (N & 1) == 0;
};

template<int Multiplier, int Multiplicand>
struct EthiopianMultiplication
{
        template<bool Cond, int Plier, int RunningTotal>
        struct AddIfNot
        {
                enum { Result = Plier + RunningTotal };
        };
        template<int Plier, int RunningTotal>
        struct AddIfNot <true, Plier, RunningTotal>
        {
                enum { Result = RunningTotal };
        };

        template<int Plier, int Plicand, int RunningTotal>
        struct Loop
        {
                enum { Result = Loop<Half<Plier>::Result, Double<Plicand>::Result,
                       AddIfNot<IsEven<Plier>::Result, Plicand, RunningTotal >::Result >::Result };
        };
        template<int Plicand, int RunningTotal>
        struct Loop <0, Plicand, RunningTotal>
        {
                enum { Result = RunningTotal };
        };

        enum { Result = Loop<Multiplier, Multiplicand, 0>::Result };
};

#include <iostream>

int main(int, char **)
{
        std::cout << EthiopianMultiplication<17, 54>::Result << std::endl;
        return 0;
}
```



## Clojure


```lisp
(defn halve [n]
  (bit-shift-right n 1))

(defn twice [n]          ; 'double' is taken
  (bit-shift-left n 1))

(defn even [n]           ; 'even?' is the standard fn
  (zero? (bit-and n 1)))

(defn emult [x y]
  (reduce +
    (map second
      (filter #(not (even (first %))) ; a.k.a. 'odd?'
        (take-while #(pos? (first %))
          (map vector
            (iterate halve x)
            (iterate twice y)))))))

(defn emult2 [x y]
  (loop [a x, b y, r 0]
    (if (= a 1)
      (+ r b)
      (if (even a)
        (recur (halve a) (twice b) r)
        (recur (halve a) (twice b) (+ r b))))))
```



## COBOL

In COBOL, ''double'' is a reserved word, so the doubling functions is named ''twice'', instead.

```COBOL
       *>* Ethiopian multiplication

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ethiopian-multiplication.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  l                  PICTURE 9(10) VALUE 17.
       01  r                  PICTURE 9(10) VALUE 34.
       01  ethiopian-multiply PICTURE 9(20).
       01  product            PICTURE 9(20).
       PROCEDURE DIVISION.
         CALL "ethiopian-multiply" USING
           BY CONTENT l, BY CONTENT r,
           BY REFERENCE ethiopian-multiply
         END-CALL
         DISPLAY ethiopian-multiply END-DISPLAY
         MULTIPLY l BY r GIVING product END-MULTIPLY
         DISPLAY product END-DISPLAY
         STOP RUN.
       END PROGRAM ethiopian-multiplication.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ethiopian-multiply.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  evenp   PICTURE 9.
         88 even   VALUE 1.
         88 odd    VALUE 0.
       LINKAGE SECTION.
       01  l       PICTURE 9(10).
       01  r       PICTURE 9(10).
       01  product PICTURE 9(20) VALUE ZERO.
       PROCEDURE DIVISION using l, r, product.
         MOVE ZEROES TO product
         PERFORM UNTIL l EQUAL ZERO
           CALL "evenp" USING
             BY CONTENT l,
             BY REFERENCE evenp
           END-CALL
           IF odd
             ADD r TO product GIVING product END-ADD
           END-IF
           CALL "halve" USING
             BY CONTENT l,
             BY REFERENCE l
           END-CALL
           CALL "twice" USING
             BY CONTENT r,
             BY REFERENCE r
           END-CALL
         END-PERFORM
         GOBACK.
       END PROGRAM ethiopian-multiply.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. halve.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01  n   PICTURE 9(10).
       01  m   PICTURE 9(10).
       PROCEDURE DIVISION USING n, m.
         DIVIDE n BY 2 GIVING m END-DIVIDE
         GOBACK.
       END PROGRAM halve.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. twice.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       01  n   PICTURE 9(10).
       01  m   PICTURE 9(10).
       PROCEDURE DIVISION USING n, m.
         MULTIPLY n by 2 GIVING m END-MULTIPLY
         GOBACK.
       END PROGRAM twice.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. evenp.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  q   PICTURE 9(10).
       LINKAGE SECTION.
       01  n   PICTURE 9(10).
       01  m   PICTURE 9(1).
         88 even   VALUE 1.
         88 odd    VALUE 0.
       PROCEDURE DIVISION USING n, m.
         DIVIDE n BY 2 GIVING q REMAINDER m END-DIVIDE
         SUBTRACT m FROM 1 GIVING m END-SUBTRACT
         GOBACK.
       END PROGRAM evenp.
```



## CoffeeScript


```coffeescript

halve = (n) -> Math.floor n / 2
double = (n) -> n * 2
is_even = (n) -> n % 2 == 0

multiply = (a, b) ->
  prod = 0
  while a > 0
    prod += b if !is_even a
    a = halve a
    b = double b
  prod

# tests
do ->
  for i in [0..100]
    for j in [0..100]
      throw Error("broken for #{i} * #{j}") if multiply(i,j) != i * j

```



## ColdFusion

Version with as a function of functions:


```cfm
<cffunction name="double">
    <cfargument name="number" type="numeric" required="true">
	<cfset answer = number * 2>
    <cfreturn answer>
</cffunction>

<cffunction name="halve">
    <cfargument name="number" type="numeric" required="true">
	<cfset answer = int(number / 2)>
    <cfreturn answer>
</cffunction>

<cffunction name="even">
    <cfargument name="number" type="numeric" required="true">
	<cfset answer = number mod 2>
    <cfreturn answer>
</cffunction>

<cffunction name="ethiopian">
    <cfargument name="Number_A" type="numeric" required="true">
    <cfargument name="Number_B" type="numeric" required="true">
    <cfset Result = 0>

    <cfloop condition = "Number_A GTE 1">
        <cfif even(Number_A) EQ 1>
            <cfset Result = Result + Number_B>
        </cfif>
        <cfset Number_A = halve(Number_A)>
        <cfset Number_B = double(Number_B)>
    </cfloop>
    <cfreturn Result>
</cffunction>


<cfoutput>#ethiopian(17,34)#</cfoutput>
```
Version with display pizza:
```cfm
<cfset Number_A = 17>
<cfset Number_B = 34>
<cfset Result = 0>

<cffunction name="double">
    <cfargument name="number" type="numeric" required="true">
	<cfset answer = number * 2>
    <cfreturn answer>
</cffunction>

<cffunction name="halve">
    <cfargument name="number" type="numeric" required="true">
	<cfset answer = int(number / 2)>
    <cfreturn answer>
</cffunction>

<cffunction name="even">
    <cfargument name="number" type="numeric" required="true">
	<cfset answer = number mod 2>
    <cfreturn answer>
</cffunction>


<cfoutput>

Ethiopian multiplication of #Number_A# and #Number_B#...




<table width="512" border="0" cellspacing="20" cellpadding="0">

<cfloop condition = "Number_A GTE 1">


   <cfif even(Number_A) EQ 1>
   	<cfset Result = Result + Number_B>
        <cfset Action = "Keep">
   <cfelse>
	<cfset Action = "Strike">
   </cfif>

  <tr>
    <td align="right">#Number_A#</td>
    <td align="right">#Number_B#</td>
    <td align="center">#Action#</td>
  </tr>

  <cfset Number_A = halve(Number_A)>
  <cfset Number_B = double(Number_B)>

</cfloop>

</table>

...equals #Result#

</cfoutput>
```

Sample output:
```txt

Ethiopian multiplication of 17 and 34...
17 	34 	Keep
8 	68 	Strike
4 	136 	Strike
2 	272 	Strike
1 	544 	Keep
...equals 578

```



## Common Lisp


Common Lisp already has <code>evenp</code>, but all three of <code>halve</code>, <code>double</code>, and <code>even-p</code> are locally defined within <code>ethiopian-multiply</code>.  (Note that the termination condition is <code>(zerop l)</code> because we terminate 'after' the iteration wherein the left column contains 1, and <code>(halve 1)</code> is 0.)
```lisp
(defun ethiopian-multiply (l r)
  (flet ((halve (n) (floor n 2))
         (double (n) (* n 2))
         (even-p (n) (zerop (mod n 2))))
    (do ((product 0 (if (even-p l) product (+ product r)))
         (l l (halve l))
         (r r (double r)))
        ((zerop l) product))))
```



## D


```d
int ethiopian(int n1, int n2) pure nothrow @nogc
in {
    assert(n1 >= 0, "Multiplier can't be negative");
} body {
    static enum doubleNum = (in int n) pure nothrow @nogc => n * 2;
    static enum halveNum = (in int n) pure nothrow @nogc => n / 2;
    static enum isEven = (in int n) pure nothrow @nogc => !(n & 1);

    int result;
    while (n1 >= 1) {
        if (!isEven(n1))
            result += n2;
        n1 = halveNum(n1);
        n2 = doubleNum(n2);
    }

    return result;
} unittest {
    assert(ethiopian(77, 54) == 77 * 54);
    assert(ethiopian(8, 923) == 8 * 923);
    assert(ethiopian(64, -4) == 64 * -4);
}

void main() {
    import std.stdio;

    writeln("17 ethiopian 34 is ", ethiopian(17, 34));
}
```
 17 ethiopian 34 is 578


## dc


```dc
0k                    [ Make sure we're doing integer division  ]sx
[ 2 / ] sH            [ Define "halve" function in register H   ]sx
[ 2 * ] sD            [ Define "double" function in register D  ]sx
[ 2 % 1 r - ] sE      [ Define "even?" function in register E   ]sx

[ Entry into the main Ethiopian multiplication function is register M ]sx
[ Keeps running value for the product in register p ]sx
[ 0 sp lLx lp ] sM

[ The body of the main loop is in register L ]sx

[
  sb sa             [ First thing we do is cheat and store the parameters in
                      registers, which is safe because the only recursion is of
                      the tail variety.  This avoids tricky stack
                      manipulations, which dc doesn't have good support for
                      (unlike, say, Forth). ]sx

  la lEx sr         [ r = even?(a)  ]sx
  lr 0 =S           [ if r = 0 then call s]sx
  la lHx d          [ a = halve(a)]sx
  lb lDx            [ b = double(b)]sx
  r 0 !=L           [ if a !=0 then recurse ]
] sL

[ Utility macro that just adds the current value of b to the total in p ]sx
[ lp lb + sp ]sS

[ Demo by multiplying 17 and 34 ]sx
17 34 lMx p
```

 578


## E


```e
def halve(&x)  { x //= 2 }
def double(&x) { x *= 2 }
def even(x)    { return x %% 2 <=> 0 }

def multiply(var a, var b) {
    var ab := 0
    while (a > 0) {
        if (!even(a)) { ab += b }
        halve(&a)
        double(&b)
    }
    return ab
}
```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature {NONE}

	make
		do
			io.put_integer (ethiopian_multiplication (17, 34))
		end

	ethiopian_multiplication (a, b: INTEGER): INTEGER
			-- Product of 'a' and 'b'.
		require
			a_positive: a > 0
			b_positive: b > 0
		local
			x, y: INTEGER
		do
			x := a
			y := b
			from
			until
				x <= 0
			loop
				if not is_even_int (x) then
					Result := Result + y
				end
				x := halve_int (x)
				y := double_int (y)
			end
		ensure
			Result_correct: Result = a * b
		end

feature {NONE}

	double_int (n: INTEGER): INTEGER
                        --Two times 'n'.
		do
			Result := n * 2
		end

	halve_int (n: INTEGER): INTEGER
                        --'n' divided by two.
		do
			Result := n // 2
		end

	is_even_int (n: INTEGER): BOOLEAN
                        --Is 'n' an even integer?
		do
			Result := n \\ 2 = 0
		end

end


```

```txt

578

```



## Ela

Translation of Haskell:

```ela
open list number

halve x = x `div` 2
double = (2*)

ethiopicmult a b = sum <| map snd <| filter (odd << fst) <| zip
  (takeWhile (>=1) <| iterate halve a)
  (iterate double b)

ethiopicmult 17 34
```
 578


## Elixir

```elixir
defmodule Ethiopian do
  def halve(n), do: div(n, 2)

  def double(n), do: n * 2

  def even(n), do: rem(n, 2) == 0

  def multiply(lhs, rhs) when is_integer(lhs) and lhs > 0 and is_integer(rhs) and rhs > 0 do
    multiply(lhs, rhs, 0)
  end

  def multiply(1, rhs, acc), do: rhs + acc
  def multiply(lhs, rhs, acc) do
    if even(lhs), do:   multiply(halve(lhs), double(rhs), acc),
                  else: multiply(halve(lhs), double(rhs), acc+rhs)
  end
end

IO.inspect Ethiopian.multiply(17, 34)
```


```txt

578

```



## Emacs Lisp

Emacs Lisp has <code>cl-evenp</code> in cl-lib.el (its Common Lisp library), but for the sake of completeness the desired effect is achieved here via <code>mod</code>.

```lisp

(defun even-p (n)
  (= (mod n 2) 0))
(defun halve (n)
  (floor n 2))
(defun double (n)
  (* n 2))
(defun ethiopian-multiplication (l r)
  (let ((sum 0))
    (while (>= l 1)
      (unless (even-p l)
	(setq sum (+ r sum)))
      (setq l (halve l))
      (setq r (double r)))
    sum))

```



## Erlang


```erlang
-module(ethopian).
-export([multiply/2]).

halve(N) ->
    N div 2.

double(N) ->
    N * 2.

even(N) ->
    (N rem 2) == 0.

multiply(LHS,RHS) when is_integer(Lhs) and Lhs > 0 and
			is_integer(Rhs) and Rhs > 0 ->
    multiply(LHS,RHS,0).

multiply(1,RHS,Acc) ->
    RHS+Acc;
multiply(LHS,RHS,Acc) ->
    case even(LHS) of
        true ->
            multiply(halve(LHS),double(RHS),Acc);
        false ->
            multiply(halve(LHS),double(RHS),Acc+RHS)
    end.
```



## ERRE


```ERRE
PROGRAM ETHIOPIAN_MULT

FUNCTION EVEN(A)
   EVEN=(A+1) MOD 2
END FUNCTION

FUNCTION HALF(A)
   HALF=INT(A/2)
END FUNCTION

FUNCTION DOUBLE(A)
   DOUBLE=2*A
END FUNCTION

BEGIN
   X=17 Y=34 TOT=0
   WHILE X>=1 DO
     PRINT(X,)
     IF EVEN(X)=0 THEN TOT=TOT+Y PRINT(Y) ELSE PRINT END IF
     X=HALF(X) Y=DOUBLE(Y)
   END WHILE
   PRINT("=",TOT)
END PROGRAM

```

 17            34
 8
 4
 2
 1             544
 =             578


## Euphoria


```euphoria
function emHalf(integer n)
  return floor(n/2)
end function

function emDouble(integer n)
  return n*2
end function

function emIsEven(integer n)
  return (remainder(n,2) = 0)
end function

function emMultiply(integer a, integer b)
 integer sum
  sum = 0
  while (a) do
    if (not emIsEven(a)) then sum += b end if
    a = emHalf(a)
    b = emDouble(b)
  end while

  return sum
end function

----------------------------------------------------------------
-- runtime

printf(1,"emMultiply(%d,%d) = %d\n",{17,34,emMultiply(17,34)})

printf(1,"\nPress Any Key\n",{})
while (get_key() = -1) do end while
```


=={{header|F Sharp|F#}}==

```fsharp
let ethopian n m =
    let halve n = n / 2
    let double n = n * 2
    let even n = n % 2 = 0
    let rec loop n m result =
        if n <= 1 then result + m
        else if even n then loop (halve n) (double m) result
        else loop (halve n) (double m) (result + m)
    loop n m 0
```



## Factor


```factor
USING: arrays kernel math multiline sequences ;
IN: ethiopian-multiplication

/*
This function is built-in
: odd? ( n -- ? ) 1 bitand 1 number= ;
*/

: double ( n -- 2*n ) 2 * ;
: halve ( n -- n/2 ) 2 /i ;

: ethiopian-mult ( a b -- a*b )
    [ 0 ] 2dip
    [ dup 0 > ] [
        [ odd? [ + ] [ drop ] if ] 2keep
        [ double ] [ halve ] bi*
    ] while 2drop ;
```



## FALSE


```false
[2/]h:
[2*]d:
[$2/2*-]o:
[0[@$][$o;![@@\$@+@]?h;!@d;!@]#%\%]m:
17 34m;!.  {578}
```



## Forth

Halve and double are standard words, spelled '''2/''' and '''2*''' respectively.

```forth
: even? ( n -- ? ) 1 and 0= ;
: e* ( x y -- x*y )
  dup 0= if nip exit then
  over 2* over 2/ recurse
  swap even? if nip else + then ;
```
The author of Forth, Chuck Moore, designed a similar primitive into his MISC Forth microprocessors.  The '''+*''' instruction is a multiply step: it adds S to T if A is odd, then shifts both A and T right one. The idea is that you only need to perform as many of these multiply steps as you have significant bits in the operand.(See his [http://www.colorforth.com/inst.htm core instruction set] for details.)


## Fortran

```fortran
program EthiopicMult
  implicit none

  print *, ethiopic(17, 34, .true.)

contains

  subroutine halve(v)
    integer, intent(inout) :: v
    v = int(v / 2)
  end subroutine halve

  subroutine doublit(v)
    integer, intent(inout) :: v
    v = v * 2
  end subroutine doublit

  function iseven(x)
    logical :: iseven
    integer, intent(in) :: x
    iseven = mod(x, 2) == 0
  end function iseven

  function ethiopic(multiplier, multiplicand, tutorialized) result(r)
    integer :: r
    integer, intent(in) :: multiplier, multiplicand
    logical, intent(in), optional :: tutorialized

    integer :: plier, plicand
    logical :: tutor

    plier = multiplier
    plicand = multiplicand

    if ( .not. present(tutorialized) ) then
       tutor = .false.
    else
       tutor = tutorialized
    endif

    r = 0

    if ( tutor ) write(*, '(A, I0, A, I0)') "ethiopian multiplication of ", plier, " by ", plicand

    do while(plier >= 1)
       if ( iseven(plier) ) then
          if (tutor) write(*, '(I4, " ", I6, A)') plier, plicand, " struck"
       else
          if (tutor) write(*, '(I4, " ", I6, A)') plier, plicand, " kept"
          r = r + plicand
       endif
       call halve(plier)
       call doublit(plicand)
    end do

  end function ethiopic

end program EthiopicMult
```



## Go


```go
package main

import "fmt"

func halve(i int) int { return i/2 }

func double(i int) int { return i*2 }

func isEven(i int) bool { return i%2 == 0 }

func ethMulti(i, j int) (r int) {
    for ; i > 0; i, j = halve(i), double(j) {
        if !isEven(i) {
            r += j
        }
    }
    return
}

func main() {
    fmt.Printf("17 ethiopian 34 = %d\n", ethMulti(17, 34))
}
```



## Haskell

===Using integer (+)===

```haskell
import Prelude hiding (odd)
import Control.Monad (join)

halve :: Int -> Int
halve = (`div` 2)

double :: Int -> Int
double = join (+)

odd :: Int -> Bool
odd = (== 1) . (`mod` 2)

ethiopicmult :: Int -> Int -> Int
ethiopicmult a b =
  sum $
  map snd $
  filter (odd . fst) $
  zip (takeWhile (>= 1) $ iterate halve a) (iterate double b)

main :: IO ()
main = print $ ethiopicmult 17 34 == 17 * 34
```

```txt
*Main> ethiopicmult 17 34
 578
```



Or, as an unfold followed by a refold:


```haskell
import Data.List (intercalate, unfoldr)
import Debug.Trace (trace)
import Data.Tuple (swap)
import Data.Bool (bool)

-- ETHIOPIAN MULTIPLICATION -------------------------------

ethMult :: Int -> Int -> Int
ethMult n m =
  let addedWhereOdd (d, x) a
        | 0 < d = (+) a x
        | otherwise = a
      halved h
        | 0 < h = Just $ trace (showHalf h) (swap $ quotRem h 2)
        | otherwise = Nothing
      doubled x = x + x
      pairs = zip (unfoldr halved n) (iterate doubled m)
  in (let x = foldr addedWhereOdd 0 pairs
      in trace (showDoubles pairs ++ " = " ++ show x ++ "\n") x)

-- TRACE DISPLAY -------------------------------------------

showHalf :: Int -> String
showHalf x = "halve: " ++ rjust 6 ' ' (show (quotRem x 2))

showDoubles :: [(Int, Int)] -> String
showDoubles xs =
  "double:\n" ++
  unlines
    (fmap
       (\x ->
           bool
             (rjust 6 ' ' $ show $ snd x)
             ("-> " ++ rjust 3 ' ' (show $ snd x))
             (0 < fst x))
       xs) ++
  intercalate " + " (xs >>= (\(r, q) -> bool [] [show q] (0 < r)))

rjust :: Int -> Char -> String -> String
rjust n c s = drop (length s) (replicate n c ++ s)

-- TEST ---------------------------------------------------
main :: IO ()
main = do
  print $ ethMult 17 34
  print $ ethMult 34 17
```

```txt
halve:  (8,1)
halve:  (4,0)
halve:  (2,0)
halve:  (1,0)
halve:  (0,1)
double:
->  34
    68
   136
   272
-> 544
34 + 544 = 578

halve: (17,0)
halve:  (8,1)
halve:  (4,0)
halve:  (2,0)
halve:  (1,0)
halve:  (0,1)
double:
    17
->  34
    68
   136
   272
-> 544
34 + 544 = 578

578
578
```



### Using monoid mappend

Alternatively, we can express Ethiopian multiplication in terms of mappend and mempty, in place of '''(+)''' and '''0'''.

This additional generality means that our '''ethMult''' function can now replicate a string n times as readily as it multiplies an integer n times, or raises an integer to the nth power.


```haskell
import Data.Monoid (mempty, (<>), getSum, getProduct)
import Control.Monad (join)
import Data.List (unfoldr)
import Data.Tuple (swap)

-- ETHIOPIAN MULTIPLICATION -------------------------------
ethMult
  :: (Monoid m)
  => Int -> m -> m
ethMult n m =
  let half n
        | 0 /= n = Just . swap $ quotRem n 2
        | otherwise = Nothing
      addedWhereOdd (d, x) a
        | 0 /= d = a <> x
        | otherwise = a
  in foldr addedWhereOdd mempty $ zip (unfoldr half n) (iterate (join (<>)) m)

-- TEST ---------------------------------------------------
main :: IO ()
main = do
  mapM_ print $
    [ getSum $ ethMult 17 34 -- 34 * 17
    , getProduct $ ethMult 3 34 -- 34 ^ 3
    ] <>
    (getProduct <$> ([ethMult 17] <*> [3, 4])) -- [3 ^ 17, 4 ^ 17]
  print $ ethMult 17 "34"
  print $ ethMult 17 [3, 4]
```

```txt
578
39304
129140163
17179869184
"3434343434343434343434343434343434"
[3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4,3,4]
```



## HicEst


```hicest
   WRITE(Messagebox) ethiopian( 17, 34 )
END ! of "main"

FUNCTION ethiopian(x, y)
    ethiopian = 0
    left = x
    right = y
    DO i = x, 1, -1
      IF( isEven(left) == 0 ) ethiopian = ethiopian + right
      IF( left == 1 ) RETURN
      left = halve(left)
      right = double(right)
    ENDDO
 END

FUNCTION halve( x )
    halve = INT( x/2 )
 END

FUNCTION double( x )
    double = 2 * x
 END

FUNCTION isEven( x )
    isEven = MOD(x, 2) == 0
 END
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)
while ethiopian(integer(get(arglist)),integer(get(arglist)))  # multiply successive pairs of command line arguments
end

procedure ethiopian(i,j)                                      # recursive Ethiopian multiplication
return ( if not even(i) then j                                # this exploits that icon control expressions return values
         else 0 ) +
       ( if i ~= 0 then ethiopian(halve(i),double(j))
         else 0 )
end

procedure double(i)
return i * 2
end

procedure halve(i)
return i / 2
end

procedure even(i)
return ( i % 2 = 0, i )
end
```
While not it seems a task requirement, most implementations have a tutorial version.  This seemed easiest in an iterative version.
```Icon
procedure ethiopian(i,j)  # iterative tutor
local p,w
w := *j+3
write("Ethiopian Multiplication of ",i," * ",j)

p := 0
until i = 0 do {
   writes(right(i,w),right(j,w))
   if not even(i) then {
      p +:= j
      write(" add")
      }
   else write(" discard")
   i := halve(i)
   j := double(j)
   }
write(right("=",w),right(p,w))
return p
end
```



## J

'''Solution''':
```j
double =:  2&*
halve  =:  %&2           NB.  or the primitive  -:
odd    =:  2&|

ethiop =:  +/@(odd@] # (double~ <@#)) (1>.<.@halve)^:a:
```


'''Example''':
   17 ethiop 34
 578

Note that <code>double</code> will repeatedly double its right argument if given a repetition count for its left argument:
   (<5) double 17
 17 34 68 136 272

Note: this implementation assumes that the number on the right is a positive integer. In contexts where it can be negative, its absolute value should be used and you should multiply the result of ethiop by its sign.
```j
ethio=: *@] * (ethiop |)
```


Alternatively, if multiplying by negative 1 is prohibited, you can use a conditional function which optionally negates its argument.
```j
ethio=: *@] -@]^:(0 > [) (ethiop |)
```

Examples:
```j
   7 ethio 11
77
   7 ethio _11
_77
   _7 ethio 11
_77
   _7 ethio _11
77
```



## Java

```java5
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
public class Mult{
  public static void main(String[] args){
    Scanner sc = new Scanner(System.in);
    int first = sc.nextInt();
    int second = sc.nextInt();

    if(first < 0){
        first = -first;
        second = -second;
    }

    Map<Integer, Integer> columns = new HashMap<Integer, Integer>();
        columns.put(first, second);
    int sum = isEven(first)? 0 : second;
    do{
      first = halveInt(first);
      second = doubleInt(second);
      columns.put(first, second);
      if(!isEven(first)){
          sum += second;
      }
    }while(first > 1);

    System.out.println(sum);
  }

  public static int doubleInt(int doubleMe){
    return doubleMe << 1; //shift left
  }

  public static int halveInt(int halveMe){
    return halveMe >>> 1; //shift right
  }

  public static boolean isEven(int num){
    return (num & 1) == 0;
  }
}
```
An optimised variant using the three helper functions from the other example.
```java5
/**
 * This method will use ethiopian styled multiplication.
 * @param a Any non-negative integer.
 * @param b Any integer.
 * @result a multiplied by b
 */
public static int ethiopianMultiply(int a, int b) {
  if(a==0 || b==0) {
    return 0;
  }
  int result = 0;
  while(a>=1) {
    if(!isEven(a)) {
      result+=b;
    }
    b = doubleInt(b);
    a = halveInt(a);
  }
  return result;
}

/**
 * This method is an improved version that will use
 * ethiopian styled multiplication, and also
 * supports negative parameters.
 * @param a Any integer.
 * @param b Any integer.
 * @result a multiplied by b
 */
public static int ethiopianMultiplyWithImprovement(int a, int b) {
  if(a==0 || b==0) {
    return 0;
  }
  if(a<0) {
    a=-a;
    b=-b;
  } else if(b>0 && a>b) {
    int tmp = a;
    a = b;
    b = tmp;
  }
  int result = 0;
  while(a>=1) {
    if(!isEven(a)) {
      result+=b;
    }
    b = doubleInt(b);
    a = halveInt(a);
  }
  return result;
}
```


== {{header|JavaScript}} ==

```javascript
var eth = {

	halve : function ( n ){  return Math.floor(n/2);  },
	double: function ( n ){  return 2*n;              },
	isEven: function ( n ){  return n%2 === 0);       },

	mult: function ( a , b ){
		var sum = 0, a = [a], b = [b];

		while ( a[0] !== 1 ){
			a.unshift( eth.halve( a[0] ) );
			b.unshift( eth.double( b[0] ) );
		}

		for( var i = a.length - 1; i > 0 ; i -= 1 ){

			if( !eth.isEven( a[i] ) ){
				sum += b[i];
			}
		}
		return sum + b[0];
	}
}
// eth.mult(17,34) returns 578
```



Or, avoiding the use of a multiplication operator in the version above, we can alternatively:

# Halve an integer, in this sense, with a right-shift (n >>= 1)
# Double an integer by addition to self (m += m)
# Test if an integer is odd by bitwise '''and''' (n & 1)



```javascript
function ethMult(m, n) {
  var o = !isNaN(m) ? 0 : ''; // same technique works with strings
  if (n < 1) return o;
  while (n > 1) {
    if (n & 1) o += m;  // 3. integer odd/even? (bit-wise and 1)
    n >>= 1;            // 1. integer halved (by right-shift)
    m += m;             // 2. integer doubled (addition to self)
  }
  return o + m;
}

ethMult(17, 34)
```


```txt
578
```



Note that the same function will also multiply strings with some efficiency, particularly where n is larger. See [[Repeat_a_string]]


```javascript
ethMult('Ethiopian', 34)
```


```txt
"EthiopianEthiopianEthiopianEthiopianEthiopianEthiopian
EthiopianEthiopianEthiopianEthiopianEthiopianEthiopianEthiopian
EthiopianEthiopianEthiopianEthiopianEthiopianEthiopianEthiopian
EthiopianEthiopianEthiopianEthiopianEthiopianEthiopianEthiopian
EthiopianEthiopianEthiopianEthiopianEthiopianEthiopianEthiopian"
```


== {{header|jq}} ==
The following implementation is intended for jq 1.4 and later.

If your jq has <tt>while/2</tt>, then the implementation of the inner function, <tt>pairs</tt>, can be simplified to:
```jq
def pairs: while( .[0] > 0; [ (.[0] | halve), (.[1] | double) ]);
```

```jq
def halve: (./2) | floor;

def double: 2 * .;

def isEven: . % 2 == 0;

def ethiopian_multiply(a;b):
  def pairs: recurse( if .[0] > 0
                      then [ (.[0] | halve), (.[1] | double) ]
                      else empty
                      end );
  reduce ([a,b] | pairs
          | select( .[0] | isEven | not)
          | .[1] ) as $i
    (0; . + $i) ;
```
Example:
```jq
ethiopian_multiply(17;34) # => 578
```



## Jsish

From Javascript entry.

```javascript
/* Ethiopian multiplication in Jsish */
var eth = {
    halve : function(n) { return Math.floor(n / 2); },
    double: function(n) { return n << 1;            },
    isEven: function(n) { return n % 2 === 0;       },

    mult: function(a, b){
        var sum = 0;
        a = [a], b = [b];

        while (a[0] !== 1) {
            a.unshift(eth.halve(a[0]));
            b.unshift(eth.double(b[0]));
        }

        for (var i = a.length - 1; i > 0; i -= 1) {
            if(!eth.isEven(a[i])) sum += b[i];
        }
        return sum + b[0];
    }
};

;eth.mult(17,34);

/*
=!EXPECTSTART!=
eth.mult(17,34) ==> 578
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish -u ethiopianMultiplication.jsi
[PASS] ethiopianMultiplication.jsi
```



## Julia

'''Helper functions''' (type stable):

```julia
halve(x::Integer) = x >> one(x)
double(x::Integer) = Int8(2) * x
even(x::Integer) = x & 1 != 1
```


'''Main function''':

```julia
function ethmult(a::Integer, b::Integer)
    r = 0
    while a > 0
        r += b * !even(a)
        a = halve(a)
        b = double(b)
    end
    return r
end

@show ethmult(17, 34)
```


'''Array version''' (more similar algorithm to the one from the task description):

```julia
function ethmult2(a::Integer, b::Integer)
    A = [a]
    B = [b]
    while A[end] > 1
        push!(A, halve(A[end]))
        push!(B, double(B[end]))
    end
    return sum(B[map(!even, A)])
end

@show ethmult2(17, 34)
```


```txt
ethmult(17, 34) = 578
ethmult2(17, 34) = 578
```


'''Benchmark test''':

```txt
julia> @time ethmult(17, 34)
  0.000003 seconds (5 allocations: 176 bytes)
578

julia> @time ethmult2(17, 34)
  0.000007 seconds (18 allocations: 944 bytes)
578

```



## Kotlin


```scala
// version 1.1.2

fun halve(n: Int) = n / 2

fun double(n: Int) = n * 2

fun isEven(n: Int) = n % 2 == 0

fun ethiopianMultiply(x: Int, y: Int): Int {
    var xx = x
    var yy = y
    var sum = 0
    while (xx >= 1) {
       if (!isEven(xx)) sum += yy
       xx = halve(xx)
       yy = double(yy)
    }
    return sum
}

fun main(args: Array<String>) {
    println("17 x 34 = ${ethiopianMultiply(17, 34)}")
    println("99 x 99 = ${ethiopianMultiply(99, 99)}")
}
```


```txt

17 x 34 = 578
99 x 99 = 9801

```



## Limbo


```Limbo
implement Ethiopian;

include "sys.m";
	sys: Sys;
	print: import sys;
include "draw.m";
	draw: Draw;

Ethiopian : module
{
	init : fn(ctxt : ref Draw->Context, args : list of string);
};

init (ctxt: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;

	print("\n%d\n", ethiopian(17, 34, 0));
	print("\n%d\n", ethiopian(99, 99, 1));
}

halve(n: int): int
{
	return (n /2);
}

double(n: int): int
{
	return (n * 2);
}

iseven(n: int): int
{
	return ((n%2) == 0);
}

ethiopian(a: int, b: int, tutor: int): int
{
	product := 0;
	if (tutor)
		print("\nmultiplying %d x %d", a, b);
	while (a >= 1) {
		if (!(iseven(a))) {
			if (tutor)
				print("\n%3d   %d", a, b);
			product += b;
		} else
			if (tutor)
				print("\n%3d   ----", a);
		a = halve(a);
		b = double(b);
	}
	return product;
}

```


## Locomotive Basic



```locobasic
10 DEF FNiseven(a)=(a+1) MOD 2
20 DEF FNhalf(a)=INT(a/2)
30 DEF FNdouble(a)=2*a
40 x=17:y=34:tot=0
50 WHILE x>=1
60 PRINT x,
70 IF FNiseven(x)=0 THEN tot=tot+y:PRINT y ELSE PRINT
80 x=FNhalf(x):y=FNdouble(y)
90 WEND
100 PRINT "=", tot
```


Output:

```txt

 17           34
 8
 4
 2
 1            544
=             578

```



## Logo


```logo
to double :x
  output ashift :x  1
end
to halve :x
  output ashift :x -1
end
to even? :x
  output equal? 0 bitand 1 :x
end
to eproduct :x :y
  if :x = 0 [output 0]
  ifelse even? :x ~
    [output      eproduct halve :x double :y] ~
    [output :y + eproduct halve :x double :y]
end
```



## LOLCODE


```lolcode
HAI 1.3

HOW IZ I Halve YR Integer
  FOUND YR QUOSHUNT OF Integer AN 2
IF U SAY SO

HOW IZ I Dubble YR Integer
  FOUND YR PRODUKT OF Integer AN 2
IF U SAY SO

HOW IZ I IzEven YR Integer
  FOUND YR BOTH SAEM 0 AN MOD OF Integer AN 2
IF U SAY SO

HOW IZ I EthiopianProdukt YR a AN YR b
  I HAS A Result ITZ 0
  IM IN YR Loop UPPIN YR x WILE DIFFRINT a AN 0
    NOT I IZ IzEven YR a MKAY
    O RLY?
      YA RLY
        Result R SUM OF Result AN b
    OIC
    a R I IZ Halve YR a MKAY
    b R I IZ Dubble YR b MKAY
  IM OUTTA YR Loop
  FOUND YR Result
IF U SAY SO

VISIBLE I IZ EthiopianProdukt YR 17 AN YR 34 MKAY
KTHXBYE
```


Output:
```txt
578
```



## Lua


```lua
function halve(a)
    return a/2
end

function double(a)
    return a*2
end

function isEven(a)
    return a%2 == 0
end

function ethiopian(x, y)
    local result = 0

    while (x >= 1) do
        if not isEven(x) then
            result = result + y
        end

        x = math.floor(halve(x))
        y = double(y)
    end

    return result;
end

print(ethiopian(17, 34))
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
IntegerHalving[x_]:=Floor[x/2]
IntegerDoubling[x_]:=x*2;
OddInteger           OddQ
Ethiopian[x_, y_] :=
Total[Select[NestWhileList[{IntegerHalving[#[[1]]],IntegerDoubling[#[[2]]]}&, {x,y}, (#[[1]]>1&)], OddQ[#[[1]]]&]][[2]]

Ethiopian[17, 34]
```


Output:

```txt
578
```



## MATLAB

First we define the three subroutines needed for this task. These must be saved in their own individual ".m" files. The file names must be the same as the function name stored in that file. Also, they must be saved in the same directory as the script that performs the Ethiopian Multiplication.

In addition, with the exception of the "isEven" and "doubleInt" functions, the inputs of the functions have to be an integer data type. This means that the input to these functions must be coerced from the default IEEE754 double precision floating point data type that all numbers and variables are represented as, to integer data types. As of MATLAB 2007a, 64-bit integer arithmetic is not supported. So, at best, these will work for 32-bit integer data types.

halveInt.m:

```MATLAB
function result = halveInt(number)

    result = idivide(number,2,'floor');

end
```


doubleInt.m:

```MATLAB
function result = doubleInt(number)

    result = times(2,number);

end
```


isEven.m:

```MATLAB
%Returns a logical 1 if the number is even, 0 otherwise.
function trueFalse = isEven(number)

    trueFalse = logical( mod(number,2)==0 );

end
```


ethiopianMultiplication.m:

```MATLAB
function answer = ethiopianMultiplication(multiplicand,multiplier)

    %Generate columns
    while multiplicand(end)>1
        multiplicand(end+1,1) = halveInt( multiplicand(end) );
        multiplier(end+1,1) = doubleInt( multiplier(end) );
    end

    %Strike out appropriate rows
    multiplier( isEven(multiplicand) ) = [];

    %Generate answer
    answer = sum(multiplier);

end
```


Sample input: (with data type coercion)

```MATLAB
ethiopianMultiplication( int32(17),int32(34) )

ans =

   578

```



## Metafont

Implemented without the ''tutor''.

```metafont
vardef halve(expr x) = floor(x/2) enddef;
vardef double(expr x) = x*2 enddef;
vardef iseven(expr x) = if (x mod 2) = 0: true else: false fi enddef;

primarydef a ethiopicmult b =
  begingroup
    save r_, plier_, plicand_;
    plier_ := a; plicand_ := b;
    r_ := 0;
    forever: exitif plier_ < 1;
      if not iseven(plier_): r_ := r_ + plicand_; fi
      plier_ := halve(plier_);
      plicand_ := double(plicand_);
    endfor
    r_
  endgroup
enddef;

show( (17 ethiopicmult 34) );
end
```


=={{header|MK-61/52}}==
<lang>П1	П2	<->	П0
ИП0	1	-	x#0	29
	ИП1	2	*	П1
	ИП0	2	/	[x]	П0
	2	/	{x}	x#0	04	ИП2	ИП1	+	П2
БП	04
ИП2	С/П
```



## MMIX


In order to assemble and run this program you'll have to install MMIXware from [http://www-cs-faculty.stanford.edu/~knuth/mmix-news.html]. This provides you with a simple assembler, a simulator, example programs and full documentation.


```mmix
A	IS	17
B	IS	34

pliar	IS 	$255		% designating main registers
pliand	GREG
acc	GREG
str	IS	pliar		% reuse reg $255 for printing

	LOC	Data_Segment
	GREG	@
BUF	OCTA	#3030303030303030 % reserve a buffer that is big enough to hold
	OCTA	#3030303030303030 % a max (signed) 64 bit integer:
	OCTA	#3030300a00000000 %   2^63 - 1 = 9223372036854775807
				  % string is terminated with NL, 0

	LOC	#1000		% locate program at address
	GREG	@
halve	SR	pliar,pliar,1
	GO	$127,$127,0

double	SL	pliand,pliand,1
	GO	$127,$127,0

odd	DIV	$77,pliar,2
	GET	$78,rR
	GO	$127,$127,0

				% Main is the entry point of the program
Main 	SET	pliar,A		% initialize registers for calculation
	SET	pliand,B
	SET	acc,0
1H	GO	$127,odd
	BZ	$78,2F		% if pliar is even skip incr. acc with pliand
	ADD	acc,acc,pliand	%
2H	GO	$127,halve	% halve pliar
	GO	$127,double	% and double pliand
	PBNZ	pliar,1B	% repeat from 1H while pliar > 0
// result: acc = 17 x 34
// next: print result --> stdout
// $0 is a temp register
	LDA	str,BUF+19	% points after the end of the string
2H	SUB	str,str,1	% update buffer pointer
	DIV	acc,acc,10	% do a divide and mod
	GET	$0,rR		% get digit from special purpose reg. rR
				% containing the remainder of the division
	INCL	$0,'0'		% convert to ascii
	STBU	$0,str		% place digit in buffer
	PBNZ	acc,2B		% next
				% 'str' points to the start of the result
	TRAP	0,Fputs,StdOut	% output answer to stdout
	TRAP	0,Halt,0	% exit
```

Assembling:

```txt
~/MIX/MMIX/Progs> mmixal ethiopianmult.mms
```

Running:

```txt
~/MIX/MMIX/Progs> mmix ethiopianmult
578
```


=={{header|Modula-2}}==
```modula2

MODULE EthiopianMultiplication;

FROM SWholeIO IMPORT
  WriteCard;
FROM STextIO IMPORT
  WriteString, WriteLn;

PROCEDURE Halve(VAR A: CARDINAL);
BEGIN
  A := A / 2;
END Halve;

PROCEDURE Double(VAR A: CARDINAL);
BEGIN
  A := 2 * A;
END Double;

PROCEDURE IsEven(A: CARDINAL): BOOLEAN;
BEGIN
  RETURN A REM 2 = 0;
END IsEven;

VAR
  X, Y, Tot: CARDINAL;

BEGIN
  X := 17;
  Y := 34;
  Tot := 0;
  WHILE X >= 1 DO
    WriteCard(X, 9);
    WriteString(" ");
    IF NOT(IsEven(X)) THEN
      INC(Tot, Y);
      WriteCard(Y, 9)
    END;
    WriteLn;
    Halve(X);
    Double(Y);
  END;
  WriteString("=         ");
  WriteCard(Tot, 9);
  WriteLn;
END EthiopianMultiplication.

```

```txt

       17        34
        8
        4
        2
        1       544
=               578

```


=={{header|Modula-3}}==
```modula3
MODULE Ethiopian EXPORTS Main;

IMPORT IO, Fmt;

PROCEDURE IsEven(n: INTEGER): BOOLEAN =
  BEGIN
    RETURN n MOD 2 = 0;
  END IsEven;

PROCEDURE Double(n: INTEGER): INTEGER =
  BEGIN
    RETURN n * 2;
  END Double;

PROCEDURE Half(n: INTEGER): INTEGER =
  BEGIN
    RETURN n DIV 2;
  END Half;

PROCEDURE Multiply(a, b: INTEGER): INTEGER =
  VAR
    temp := 0;
    plier := a;
    plicand := b;
  BEGIN
    WHILE plier >= 1 DO
      IF NOT IsEven(plier) THEN
        temp := temp + plicand;
      END;
      plier := Half(plier);
      plicand := Double(plicand);
    END;
    RETURN temp;
  END Multiply;

BEGIN
  IO.Put("17 times 34 = " & Fmt.Int(Multiply(17, 34)) & "\n");
END Ethiopian.
```



## MUMPS

```MUMPS

HALVE(I)
 ;I should be an integer
 QUIT I\2
DOUBLE(I)
 ;I should be an integer
 QUIT I*2
ISEVEN(I)
 ;I should be an integer
 QUIT '(I#2)
E2(M,N)
 New W,A,E,L Set W=$Select($Length(M)>=$Length(N):$Length(M)+2,1:$L(N)+2),A=0,L=0,A(L,1)=M,A(L,2)=N
 Write "Multiplying two numbers:"
 For  Write !,$Justify(A(L,1),W),?W,$Justify(A(L,2),W) Write:$$ISEVEN(A(L,1)) ?(2*W)," Struck" Set:'$$ISEVEN(A(L,1)) A=A+A(L,2) Set L=L+1,A(L,1)=$$HALVE(A(L-1,1)),A(L,2)=$$DOUBLE(A(L-1,2)) Quit:A(L,1)<1
 Write ! For E=W:1:(2*W) Write ?E,"="
 Write !,?W,$Justify(A,W),!
 Kill W,A,E,L
 Q
```
 USER>D E2^ROSETTA(1439,7)
 Multiplying two numbers:
  1439     7
   719    14
   359    28
   179    56
    89   112
    44   224 Struck
    22   448 Struck
    11   896
     5  1792
     2  3584 Struck
     1  7168

### =

       10073


## Nemerle


```Nemerle
using System;
using System.Console;

module Ethiopian
{
    Multiply(x : int, y : int) : int
    {
        def halve(a)  {a / 2}
        def doble(a)  {a * 2}
        def isEven(a) {a % 2 == 0}
        def multiply(p, q)
        {
            match(p)
            {
                |p when (p < 1) => 0
                |p when (isEven(p)) => 0 + multiply(halve(p), doble(q))
                |_ => q + multiply(halve(p), doble(q))
            }
        }
        multiply(x, y)
    }

    Main() : void
    {
        WriteLine("By Ethiopian multiplication, 17 * 34 = {0}", Multiply(17, 34));
    }
}
```



## NetRexx

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

/*REXX program multiplies 2 integers by Ethiopian/Russian peasant method*/
numeric digits 1000              /*handle extremely large integers.     */
                                 /*handles zeroes and negative integers.*/
                                 /*A & B  should be checked if integers.*/
parse arg a b .
say 'a=' a
say 'b=' b
say 'product=' emult(a,b)
return

method emult(x,y) private static
  parse x x 1 ox
  prod=0
  loop while x\==0
    if \iseven(x) then prod=prod+y
    x=halve(x)
    y=dubble(y)
    end
  return prod*ox.sign

method halve(x) private static
  return x % 2

method dubble(x) private static
  return x + x

method iseven(x) private static
  return x//2 == 0
```



## Nim


```nim
proc halve(x): int = x div 2
proc double(x): int = x * 2
proc even(x): bool = x mod 2 == 0

proc ethiopian(x, y): int =
  var x = x
  var y = y

  while x >= 1:
    if not even x:
      result += y
    x = halve x
    y = double y

echo ethiopian(17, 34)
```



## Objeck

```objeck

use Collection;

class EthiopianMultiplication {
  function : Main(args : String[]) ~ Nil {
    first := IO.Console->ReadString()->ToInt();
    second := IO.Console->ReadString()->ToInt();
    "----"->PrintLine();
    Mul(first, second)->PrintLine();
  }

  function : native : Mul(first : Int, second : Int) ~ Int {
    if(first < 0){
      first := -1 * first;
      second := -1 * second;
    };

    sum := isEven(first)? 0 : second;
    do {
      first := halveInt(first);
      second := doubleInt(second);
      if(isEven(first) = false){
        sum += second;
      };
    }
    while(first > 1);

    return sum;
  }

  function : halveInt(num : Int) ~ Bool {
    return num >> 1;
  }

  function : doubleInt(num : Int) ~ Bool {
    return num << 1;
  }

  function : isEven(num : Int) ~ Bool {
    return (num and 1) = 0;
  }
}
```



## Object Pascal

multiplication.pas:
```pascal
unit Multiplication;
interface

function Double(Number: Integer): Integer;
function Halve(Number: Integer): Integer;
function Even(Number: Integer): Boolean;
function Ethiopian(NumberA, NumberB: Integer): Integer;

implementation
  function Double(Number: Integer): Integer;
  begin
    result := Number * 2
  end;

  function Halve(Number: Integer): Integer;
  begin
    result := Number div 2
  end;

  function Even(Number: Integer): Boolean;
  begin
    result := Number mod 2 = 0
  end;

  function Ethiopian(NumberA, NumberB: Integer): Integer;
  begin
    result := 0;
    while NumberA >= 1 do
    begin
      if not Even(NumberA) then
        result := result + NumberB;
      NumberA := Halve(NumberA);
      NumberB := Double(NumberB)
    end
  end;
begin
end.
```
ethiopianmultiplication.pas:
```pascal
program EthiopianMultiplication;

uses
  Multiplication;

begin
  WriteLn('17 * 34 = ', Ethiopian(17, 34))
end.
```
 17 * 34 = 578

=={{header|Objective-C}}==
Using class methods except for the generic useful function <tt>iseven</tt>.

```objc
#import <stdio.h


BOOL iseven(int x)
{
  return (x&1) == 0;
}

@interface EthiopicMult : NSObject
+ (int)mult: (int)plier by: (int)plicand;
+ (int)halve: (int)a;
+ (int)double: (int)a;
@end

@implementation EthiopicMult
+ (int)mult: (int)plier by: (int)plicand
{
  int r = 0;
  while(plier >= 1) {
    if ( !iseven(plier) ) r += plicand;
    plier = [EthiopicMult halve: plier];
    plicand = [EthiopicMult double: plicand];
  }
  return r;
}

+ (int)halve: (int)a
{
  return (a>>1);
}

+ (int)double: (int)a
{
  return (a<<1);
}
@end

int main()
{
  @autoreleasepool {
    printf("%d\n", [EthiopicMult mult: 17 by: 34]);
  }
  return 0;
}
```



## OCaml


```ocaml
(* We optimize a bit by not keeping the intermediate lists, and summing
   the right column on-the-fly, like in the C version.
   The function takes "halve" and "double" operators and "is_even" predicate as arguments,
   but also "is_zero", "zero" and "add". This allows for more general uses of the
   ethiopian multiplication. *)
let ethiopian is_zero is_even halve zero double add b a =
  let rec g a b r =
    if is_zero a
    then (r)
    else g (halve a) (double b) (if not (is_even a) then (add b r) else (r))
  in
  g a b zero
;;

let imul =
  ethiopian (( = ) 0) (fun x -> x mod 2 = 0) (fun x -> x / 2) 0 (( * ) 2) ( + );;

imul 17 34;;
(* - : int = 578 *)

(* Now, we have implemented the same algorithm as "rapid exponentiation",
   merely changing operator names *)
let ipow =
  ethiopian (( = ) 0) (fun x -> x mod 2 = 0) (fun x -> x / 2) 1 (fun x -> x*x) ( * )
;;

ipow 2 16;;
(* - : int = 65536 *)

(* still renaming operators, if "halving" is just subtracting one,
   and "doubling", adding one, then we get an addition *)
let iadd a b =
  ethiopian (( = ) 0) (fun x -> false) (pred) b (function x -> x) (fun x y -> succ y) 0 a
;;

iadd 421 1000;;
(* - : int = 1421 *)

(* One can do much more with "ethiopian multiplication",
   since the two "multiplicands" and the result may be of three different types,
   as shown by the typing system of ocaml *)

ethiopian;;
- : ('a -> bool) ->          (* is_zero *)
    ('a -> bool) ->          (* is_even *)
    ('a -> 'a) ->            (* halve *)
    'b ->                    (* zero *)
    ('c -> 'c) ->            (* double *)
    ('c -> 'b -> 'b) ->      (* add *)
    'c ->                    (* b *)
    'a ->                    (* a *)
    'b                       (* result *)
= <fun>

(* Here zero is the starting value for the accumulator of the sums
   of values in the right column in the original algorithm. But the "add"
   me do something else, see for example the RosettaCode page on
   "Exponentiation operator". *)
```



## Octave


```octave
function r = halve(a)
  r = floor(a/2);
endfunction

function r = doublit(a)
  r = a*2;
endfunction

function r = iseven(a)
  r = mod(a,2) == 0;
endfunction

function r = ethiopicmult(plier, plicand, tutor=false)
  r = 0;
  if (tutor)
    printf("ethiopic multiplication of %d and %d\n", plier, plicand);
  endif
  while(plier >= 1)
    if ( iseven(plier) )
      if (tutor)
	printf("%4d %6d struck\n", plier, plicand);
      endif
    else
      r = r + plicand;
      if (tutor)
	printf("%4d %6d kept\n", plier, plicand);
      endif
    endif
    plier = halve(plier);
    plicand = doublit(plicand);
  endwhile
endfunction

disp(ethiopicmult(17, 34, true))
```




## Oforth


Based on Forth version.

isEven is already defined for Integers.


```Oforth
: halve   2 / ;
: double  2 * ;

: ethiopian
   dup ifZero: [ nip return ]
   over double over halve ethiopian
   swap isEven ifTrue: [ nip ] else: [ + ] ;
```


```txt

17 34 ethiopian .
578

```



## Ol



```ol

(define (ethiopian-multiplication l r)
   (let ((even? (lambda (n)
                  (eq? (mod n 2) 0))))

   (let loop ((sum 0) (l l) (r r))
      (print "sum: " sum ", l: " l ", r: " r)
      (if (eq? l 0)
         sum
         (loop
            (if (even? l) (+ sum r) sum)
            (floor (/ l 2)) (* r 2))))))

(print (ethiopian-multiplication 17 34))

```


```txt

sum: 0, l: 17, r: 34
sum: 0, l: 8, r: 68
sum: 68, l: 4, r: 136
sum: 204, l: 2, r: 272
sum: 476, l: 1, r: 544
sum: 476, l: 0, r: 1088
476

```



## ooRexx

The [[#REXX|Rexx]] solution shown herein applies equally to [[ooRexx]].


## Oz


```oz
declare
  fun {Halve X}   X div 2             end
  fun {Double X}  X * 2               end
  fun {Even X}    {Abs X mod 2} == 0  end  %% standard function: Int.isEven

  fun {EthiopicMult X Y}
     X >= 0 = true %% assert: X must not be negative

     Rows = for
               L in X; L>0;  {Halve L}  %% C-like iterator: "Init; While; Next"
               R in Y; true; {Double R}
               collect:Collect
	    do
	       {Collect L#R}
	    end

     OddRows = {Filter Rows LeftIsOdd}
     RightColumn = {Map OddRows SelectRight}
  in
     {Sum RightColumn}
  end

  %% Helpers
  fun {LeftIsOdd L#_}   {Not {Even L}}          end
  fun {SelectRight _#R} R                       end
  fun {Sum Xs}          {FoldL Xs Number.'+' 0} end
in
  {Show {EthiopicMult 17 34}}
```



## PARI/GP


```parigp
halve(n)=n\2;
double(n)=2*n;
even(n)=!(n%2);
multE(a,b)={ my(d=0);
     while(a,
          if(!even(a),
               d+=b);
          a=halve(a);
          b=double(b));
     d
};
```



## Pascal


```pascal
program EthiopianMultiplication;

  function Double(Number: Integer): Integer;
  begin
    Double := Number * 2
  end;

  function Halve(Number: Integer): Integer;
  begin
    Halve := Number div 2
  end;

  function Even(Number: Integer): Boolean;
  begin
    Even := Number mod 2 = 0
  end;

  function Ethiopian(NumberA, NumberB: Integer): Integer;
  begin
    Ethiopian := 0;
    while NumberA >= 1 do
	begin
	  if not Even(NumberA) then
	    Ethiopian := Ethiopian + NumberB;
	  NumberA := Halve(NumberA);
	  NumberB := Double(NumberB)
	end
  end;

begin
  Write(Ethiopian(17, 34))
end.
```



## Perl


```perl
use strict;

sub halve { int((shift) / 2); }
sub double { (shift) * 2; }
sub iseven { ((shift) & 1) == 0; }

sub ethiopicmult
{
    my ($plier, $plicand, $tutor) = @_;
    print "ethiopic multiplication of $plier and $plicand\n" if $tutor;
    my $r = 0;
    while ($plier >= 1)
    {
	$r += $plicand unless iseven($plier);
	if ($tutor) {
	    print "$plier, $plicand ", (iseven($plier) ? " struck" : " kept"), "\n";
	}
	$plier = halve($plier);
	$plicand = double($plicand);
    }
    return $r;
}

print ethiopicmult(17,34, 1), "\n";
```



## Perl 6


```perl6
sub halve  (Int $n is rw)    { $n div= 2 }
sub double (Int $n is rw)    { $n *= 2 }
sub even   (Int $n --> Bool) { $n %% 2 }

sub ethiopic-mult (Int $a is copy, Int $b is copy --> Int) {
    my Int $r = 0;
    while $a {
	even $a or $r += $b;
	halve $a;
	double $b;
    }
    return $r;
}

say ethiopic-mult(17,34);
```

 578
More succinctly using implicit typing, primed lambdas, and an infinite loop:

```perl6
sub ethiopic-mult {
    my &halve  = * div= 2;
    my &double = * *= 2;
    my &even   = * %% 2;

    my ($a,$b) = @_;
    my $r;
    loop {
        even  $a or $r += $b;
        halve $a or return $r;
        double $b;
    }
}

say ethiopic-mult(17,34);
```

More succinctly still, using a pure functional approach (reductions, mappings, lazy infinite sequences):

```perl6
sub halve  { $^n div 2 }
sub double { $^n * 2   }
sub even   { $^n %% 2  }

sub ethiopic-mult ($a, $b) {
    [+] ($b, &double ... *)
        Z*
        ($a, &halve ... 0).map: { not even $^n }
}

say ethiopic-mult(17,34);
```
(same output)


## Phix

```Phix
function emHalf(integer n)
    return floor(n/2)
end function

function emDouble(integer n)
    return n*2
end function

function emIsEven(integer n)
    return (remainder(n,2)=0)
end function

function emMultiply(integer a, integer b)
integer sum = 0
    while a!=0 do
        if not emIsEven(a) then sum += b end if
        a = emHalf(a)
        b = emDouble(b)
    end while
    return sum
end function

printf(1,"emMultiply(%d,%d) = %d\n",{17,34,emMultiply(17,34)})
```



## PHP

Not object oriented version:
```php
<?php
function halve($x)
{
  return floor($x/2);
}

function double($x)
{
  return $x*2;
}

function iseven($x)
{
  return !($x & 0x1);
}

function ethiopicmult($plier, $plicand, $tutor)
{
  if ($tutor) echo "ethiopic multiplication of $plier and $plicand\n";
  $r = 0;
  while($plier >= 1) {
    if ( !iseven($plier) ) $r += $plicand;
    if ($tutor)
      echo "$plier, $plicand ", (iseven($plier) ? "struck" : "kept"), "\n";
    $plier = halve($plier);
    $plicand = double($plicand);
  }
  return $r;
}

echo ethiopicmult(17, 34, true), "\n";

?>
```
 ethiopic multiplication of 17 and 34
 17, 34 kept
 8, 68 struck
 4, 136 struck
 2, 272 struck
 1, 544 kept
 578
Object Oriented version:
```php
<?php

class ethiopian_multiply {

   protected $result = 0;

   protected function __construct($x, $y){
      while($x >= 1){
         $this->sum_result($x, $y);
         $x = $this->half_num($x);
         $y = $this->double_num($y);
      }
   }

   protected function half_num($x){
      return floor($x/2);
   }

   protected function double_num($y){
      return $y*2;
   }

   protected function not_even($n){
      return $n%2 != 0 ? true : false;
   }

   protected function sum_result($x, $y){
      if($this->not_even($x)){
         $this->result += $y;
      }
   }

   protected function get_result(){
      return $this->result;
   }

   static public function init($x, $y){
      $init = new ethiopian_multiply($x, $y);
      return $init->get_result();
   }

}

echo ethiopian_multiply::init(17, 34);
?>
```



## PicoLisp


```PicoLisp
(de halve (N)
   (/ N 2) )

(de double (N)
   (* N 2) )

(de even? (N)
   (not (bit? 1 N)) )

(de ethiopian (X Y)
   (let R 0
      (while (>= X 1)
         (or (even? X) (inc 'R Y))
         (setq
            X (halve X)
            Y (double Y) ) )
      R ) )
```



## Pike


```Pike
int ethopian_multiply(int l, int r)
{
    int halve(int n) { return n/2; };
    int double(int n) { return n*2; };
    int(0..1) evenp(int n) { return !(n%2); };

    int product = 0;
    do
    {
        write("%5d %5d\n", l, r);
        if (!evenp(l))
            product += r;
        l = halve(l);
        r = double(r);
    }
    while(l);
    return product;
}
```



## PL/I


```PL/I

   declare (L(30), R(30)) fixed binary;
   declare (i, s) fixed binary;

   L, R = 0;
   put skip list
      ('Hello, please type two values and I will print their product:');
   get list (L(1), R(1));
   put edit ('The product of ', trim(L(1)), ' and ', trim(R(1)), ' is ') (a);
   do i = 1 by 1 while (L(i) ^= 0);
      L(i+1) = halve(L(i));
      R(i+1) = double(R(i));
   end;
   s = 0;
   do i = 1 by 1 while (L(i) > 0);
      if odd(L(i)) then s = s + R(i);
   end;
   put edit (trim(s)) (a);

halve: procedure (k) returns (fixed binary);
   declare k fixed binary;
   return (k/2);
end halve;
double: procedure (k) returns (fixed binary);
   declare k fixed binary;
   return (2*k);
end;
odd: procedure (k) returns (bit (1));
   return (iand(k, 1) ^= 0);
end odd;
```



## PL/SQL

This code was taken from the ADA example above - very minor differences.

```plsql
create or replace package ethiopian is

  function multiply
    ( left    in  integer,
      right   in  integer)
  return integer;

end ethiopian;
/

create or replace package body ethiopian is

  function is_even(item  in integer) return boolean is
  begin
    return item mod 2 = 0;
  end is_even;

  function double(item  in integer) return integer is
  begin
    return item * 2;
  end double;

  function half(item  in integer) return integer is
  begin
    return trunc(item / 2);
  end half;

  function multiply
    ( left   in integer,
      right  in integer)
    return Integer
  is
    temp     integer := 0;
    plier    integer := left;
    plicand  integer := right;
  begin

    loop
      if not is_even(plier) then
        temp := temp + plicand;
      end if;
      exit when plier <= 1;
      plier := half(plier);
      plicand := double(plicand);
    end loop;

    return temp;

  end multiply;

end ethiopian;
/

/* example call */
begin
  dbms_output.put_line(ethiopian.multiply(17, 34));
end;
/
```



## Powerbuilder


```powerbuilder
public function boolean wf_iseven (long al_arg);return mod(al_arg, 2 ) = 0
end function

public function long wf_halve (long al_arg);RETURN int(al_arg / 2)
end function

public function long wf_double (long al_arg);RETURN al_arg * 2
end function

public function long wf_ethiopianmultiplication (long al_multiplicand, long al_multiplier);// calculate result
long ll_product

DO WHILE al_multiplicand >= 1
	IF wf_iseven(al_multiplicand) THEN
		// do nothing
	ELSE
		ll_product += al_multiplier
	END IF
	al_multiplicand = wf_halve(al_multiplicand)
	al_multiplier = wf_double(al_multiplier)
LOOP

return ll_product
end function

// example call
long ll_answer
ll_answer = wf_ethiopianmultiplication(17,34)
```



## PowerShell


### Traditional


```PowerShell
function isEven {
	param ([int]$value)
	return [bool]($value % 2 -eq 0)
}

function doubleValue {
	param ([int]$value)
	return [int]($value * 2)
}

function halveValue {
	param ([int]$value)
	return [int]($value / 2)
}

function multiplyValues {
	param (
		[int]$plier,
		[int]$plicand,
		[int]$temp = 0
	)

	while ($plier -ge 1)
	{
		if (!(isEven $plier)) {
			$temp += $plicand
		}
		$plier = halveValue $plier
		$plicand = doubleValue $plicand
	}

return $temp
}

multiplyValues 17 34
```


### Pipes with Busywork

This uses several PowerShell specific features, in functions everything is returned automatically, so explicitly stating return is unnecessary.  type conversion happens automatically for certain types, [int] into [boolean] maps 0 to false and everything else to true.  A hash is used to store the values as they are being written, then a pipeline is used to iterate over the keys of the hash, determine which are odd, and only sum those.  The three-valued ForEach-Object is used to set a start expression, an iterative expression, and a return expression.

```PowerShell
function halveInt( [int] $rhs )
{
	[math]::floor( $rhs / 2 )
}

function doubleInt( [int] $rhs )
{
	$rhs*2
}

function isEven( [int] $rhs )
{
	-not ( $_ % 2 )
}

function Ethiopian( [int] $lhs , [int] $rhs )
{
	$scratch = @{}
	1..[math]::floor( [math]::log( $lhs , 2 ) + 1 ) |
	ForEach-Object {
		$scratch[$lhs] = $rhs
		$lhs
		$lhs = halveInt( $lhs )
		$rhs = doubleInt( $rhs ) } |
	Where-Object { -not ( isEven $_ ) } |
	ForEach-Object { $sum = 0 } { $sum += $scratch[$_] } { $sum }
}

Ethiopian 17 34
```



## Prolog



###  Traditional



```prolog
halve(X,Y) :- Y is X // 2.
double(X,Y) :- Y is 2*X.
is_even(X) :- 0 is X mod 2.

% columns(First,Second,Left,Right) is true if integers First and Second
% expand into the columns Left and Right, respectively
columns(1,Second,[1],[Second]).
columns(First,Second,[First|Left],[Second|Right]) :-
    halve(First,Halved),
    double(Second,Doubled),
    columns(Halved,Doubled,Left,Right).

% contribution(Left,Right,Amount) is true if integers Left and Right,
% from their respective columns contribute Amount to the final sum.
contribution(Left,_Right,0) :-
    is_even(Left).
contribution(Left,Right,Right) :-
    \+ is_even(Left).

ethiopian(First,Second,Product) :-
    columns(First,Second,Left,Right),
    maplist(contribution,Left,Right,Contributions),
    sumlist(Contributions,Product).
```




###  Functional Style


Using the same definitions as above for "halve/2", "double/2" and "is_even/2" along with an SWI-Prolog [http://www.swi-prolog.org/pack/list?p=func pack for function notation], one might write the following solution


```prolog
:- use_module(library(func)).

% halve/2, double/2, is_even/2 definitions go here

ethiopian(First,Second,Product) :-
    ethiopian(First,Second,0,Product).

ethiopian(1,Second,Sum0,Sum) :-
    Sum is Sum0 + Second.
ethiopian(First,Second,Sum0,Sum) :-
    Sum1 is Sum0 + Second*(First mod 2),
    ethiopian(halve $ First, double $ Second, Sum1, Sum).
```




###  Constraint Handling Rules


This is a CHR solution for this problem using Prolog as the host language.  Code will work in SWI-Prolog and YAP (and possibly in others with or without some minor tweaking).

```prolog
:- module(ethiopia, [test/0, mul/3]).

:- use_module(library(chr)).

:- chr_constraint mul/3, halve/2, double/2, even/1, add_odd/4.

mul(1, Y, S) <=>          S = Y.
mul(X, Y, S) <=> X \= 1 | halve(X, X1),
                          double(Y, Y1),
                          mul(X1, Y1, S1),
                          add_odd(X, Y, S1, S).

halve(X, Y) <=> Y is X // 2.

double(X, Y) <=> Y is X * 2.

even(X) <=> 0 is X mod 2 | true.
even(X) <=> 1 is X mod 2 | false.

add_odd(X, _, A, S) <=> even(X)    | S is A.
add_odd(X, Y, A, S) <=> \+ even(X) | S is A + Y.

test :-
    mul(17, 34, Z), !,
    writeln(Z).
```
Note that the task statement is what makes the halve and double constraints required.  Their use is highly artificial and a more realistic implementation would look like this:


```prolog
:- module(ethiopia, [test/0, mul/3]).

:- use_module(library(chr)).

:- chr_constraint mul/3, even/1, add_if_odd/4.

mul(1, Y, S) <=>          S = Y.
mul(X, Y, S) <=> X \= 1 | X1 is X // 2,
                          Y1 is Y * 2,
                          mul(X1, Y1, S1),
                          add_if_odd(X, Y, S1, S).

even(X) <=> 0 is X mod 2 | true.
even(X) <=> 1 is X mod 2 | false.

add_if_odd(X, _, A, S) <=> even(X)    | S is A.
add_if_odd(X, Y, A, S) <=> \+ even(X) | S is A + Y.

test :-
    mul(17, 34, Z),
    writeln(Z).
```
Even this is more verbose than what a more native solution would look like.


## Python


### Python: With tutor


```python
tutor = True

def halve(x):
    return x // 2

def double(x):
    return x * 2

def even(x):
    return not x % 2

def ethiopian(multiplier, multiplicand):
    if tutor:
        print("Ethiopian multiplication of %i and %i" %
              (multiplier, multiplicand))
    result = 0
    while multiplier >= 1:
        if even(multiplier):
            if tutor:
                print("%4i %6i STRUCK" %
                      (multiplier, multiplicand))
        else:
            if tutor:
                print("%4i %6i KEPT" %
                      (multiplier, multiplicand))
            result += multiplicand
        multiplier   = halve(multiplier)
        multiplicand = double(multiplicand)
    if tutor:
        print()
    return result
```


Sample output

```txt
Python 3.1 (r31:73574, Jun 26 2009, 20:21:35) [MSC v.1500 32 bit (Intel)] on win32
Type "copyright", "credits" or "license()" for more information.
>>> ethiopian(17, 34)
Ethiopian multiplication of 17 and 34
  17     34 KEPT
   8     68 STRUCK
   4    136 STRUCK
   2    272 STRUCK
   1    544 KEPT

578
>>>
```



### Python: Without tutor

Without the tutorial code, and taking advantage of Python's lambda:


```python
halve  = lambda x: x // 2
double = lambda x: x*2
even   = lambda x: not x % 2

def ethiopian(multiplier, multiplicand):
    result = 0

    while multiplier >= 1:
        if not even(multiplier):
            result += multiplicand
        multiplier   = halve(multiplier)
        multiplicand = double(multiplicand)

    return result
```



### Python: With tutor. More Functional

Using some features which Python has for use in functional programming. The example also tries to show how to mix different programming styles while keeping close to the task specification, a kind of "executable pseudocode". Note: While column2 could theoretically generate a sequence of infinite length, izip will stop requesting values from it (and so provide the necessary stop condition) when column1 has no more values. When not using the tutor, table will generate the table on the fly in an efficient way, not keeping any intermediate values.
```python
tutor = True

from itertools import izip, takewhile

def iterate(function, arg):
    while 1:
        yield arg
        arg = function(arg)

def halve(x): return x // 2
def double(x): return x * 2
def even(x): return x % 2 == 0

def show_heading(multiplier, multiplicand):
    print "Multiplying %d by %d" % (multiplier, multiplicand),
    print "using Ethiopian multiplication:"
    print

TABLE_FORMAT = "%8s %8s %8s %8s %8s"

def show_table(table):
    for p, q in table:
        print TABLE_FORMAT % (p, q, "->",
                              p, q if not even(p) else "-" * len(str(q)))

def show_result(result):
    print TABLE_FORMAT % ('', '', '', '', "=" * (len(str(result)) + 1))
    print TABLE_FORMAT % ('', '', '', '', result)

def ethiopian(multiplier, multiplicand):
    def column1(x): return takewhile(lambda v: v >= 1, iterate(halve, x))
    def column2(x): return iterate(double, x)
    def rows(x, y): return izip(column1(x), column2(y))
    table = rows(multiplier, multiplicand)
    if tutor:
        table = list(table)
        show_heading(multiplier, multiplicand)
        show_table(table)
    result = sum(q for p, q in table if not even(p))
    if tutor:
        show_result(result)
    return result
```
 >>> ethiopian(17, 34)
 Multiplying 17 by 34 using Ethiopian multiplication:

      17       34       ->       17       34
       8       68       ->        8       --
       4      136       ->        4      ---
       2      272       ->        2      ---
       1      544       ->        1      544
                                        ====
                                         578
 578


### Python: as an unfold followed by a fold

```python
'''Ethiopian multiplication'''

from functools import reduce


# ethMult :: Int -> Int -> Int
def ethMult(n):
    '''Ethiopian multiplication of n by m.'''

    def doubled(x):
        return x + x

    def halved(h):
        qr = divmod(h, 2)
        if 0 < h:
            print('halve:', str(qr).rjust(8, ' '))
        return Just(qr) if 0 < h else Nothing()

    def addedWhereOdd(a, remx):
        odd, x = remx
        if odd:
            print(
                str(a).rjust(2, ' '), '+',
                str(x).rjust(3, ' '), '->',
                str(a + x).rjust(3, ' ')
            )
            return a + x
        else:
            print(str(x).rjust(8, ' '))
            return a

    return lambda m: reduce(
        addedWhereOdd,
        zip(
            unfoldr(halved)(n),
            iterate(doubled)(m)
        ),
        0
    )


# TEST -------------------------------------------------
def main():
    '''Tests of multiplication.'''

    print(
        '\nProduct:    ' + str(
            ethMult(17)(34)
        ),
        '\n_______________\n'
    )
    print(
        '\nProduct:    ' + str(
            ethMult(34)(17)
        )
    )


# GENERIC -------------------------------------------------

# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': True}


# iterate :: (a -> a) -> a -> Gen [a]
def iterate(f):
    '''An infinite list of repeated
       applications of f to x.
    '''
    def go(x):
        v = x
        while True:
            yield v
            v = f(v)
    return lambda x: go(x)


# showLog :: a -> IO String
def showLog(*s):
    '''Arguments printed with
       intercalated arrows.'''
    print(
        ' -> '.join(map(str, s))
    )


# unfoldr(lambda x: Just((x, x - 1)) if 0 != x else Nothing())(10)
# -> [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

# unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
def unfoldr(f):
    '''Dual to reduce or foldr.
       Where catamorphism reduces a list to a summary value,
       the anamorphic unfoldr builds a list from a seed value.
       As long as f returns Just(a, b), a is prepended to the list,
       and the residual b is used as the argument for the next
       application of f.
       When f returns Nothing, the completed list is returned.'''
    def go(v):
        xr = v, v
        xs = []
        while True:
            mb = f(xr[0])
            if mb.get('Nothing'):
                return xs
            else:
                xr = mb.get('Just')
                xs.append(xr[1])
        return xs
    return lambda x: go(x)


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
halve:   (8, 1)
halve:   (4, 0)
halve:   (2, 0)
halve:   (1, 0)
halve:   (0, 1)
 0 +  34 ->  34
      68
     136
     272
34 + 544 -> 578

Product:    578
_______________

halve:  (17, 0)
halve:   (8, 1)
halve:   (4, 0)
halve:   (2, 0)
halve:   (1, 0)
halve:   (0, 1)
      17
 0 +  34 ->  34
      68
     136
     272
34 + 544 -> 578

Product:    578
```



## R


### R: With tutor


```R
halve <- function(a) floor(a/2)
double <- function(a) a*2
iseven <- function(a) (a%%2)==0

ethiopicmult <- function(plier, plicand, tutor=FALSE) {
  if (tutor) { cat("ethiopic multiplication of", plier, "and", plicand, "\n") }
  result <- 0
  while(plier >= 1) {
    if (!iseven(plier)) { result <- result + plicand }
    if (tutor) {
      cat(plier, ", ", plicand, " ", ifelse(iseven(plier), "struck", "kept"), "\n", sep="")
    }
    plier <- halve(plier)
    plicand <- double(plicand)
  }
  result
}

print(ethiopicmult(17, 34, TRUE))
```



### R: Without tutor

Simplified version.

```R

halve <- function(a) floor(a/2)
double <- function(a) a*2
iseven <- function(a) (a%%2)==0

ethiopicmult<-function(x,y){
	res<-ifelse(iseven(y),0,x)
	while(!y==1){
		x<-double(x)
		y<-halve(y)
		if(!iseven(y)) res<-res+x
	}
	return(res)
}

print(ethiopicmult(17,34))

```



## Racket


```Racket
#lang racket

(define (halve  i) (quotient i 2))
(define (double i) (* i 2))
;; `even?' is built-in

(define (ethiopian-multiply x y)
  (cond [(zero? x) 0]
        [(even? x) (ethiopian-multiply (halve x) (double y))]
        [else (+ y (ethiopian-multiply (halve x) (double y)))]))

(ethiopian-multiply 17 34) ; -> 578
```



## Rascal


```Rascal
import IO;

public int halve(int n) = n/2;

public int double(int n) = n*2;

public bool uneven(int n) = (n % 2) != 0);

public int ethiopianMul(int n, int m) {
	result = 0;
	while(n >= 1) {
		if(uneven(n))
			result += m;
		n = halve(n);
		m = double(m);
	}
	return result;
}
```



## REXX

These two REXX versions properly handle negative integers.

### sans error checking


```rexx
/*REXX program multiplies two integers by the  Ethiopian  (or Russian peasant)  method. */
numeric digits 3000                              /*handle some gihugeic integers.       */
parse arg a b .                                  /*get two numbers from the command line*/
say  'a=' a                                      /*display a formatted value of  A.     */
say  'b='   b                                    /*   "    "     "       "    "  B.     */
say  'product='    eMult(a, b)                   /*invoke eMult & multiple two integers.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
eMult:   procedure;  parse arg x,y;  s=sign(x)   /*obtain the two arguments; sign for X.*/
         $=0                                     /*product of the two integers (so far).*/
                      do  while x\==0            /*keep processing while   X   not zero.*/
                      if \isEven(x)  then $=$+y  /*if odd,  then add   Y   to product.  */
                      x= halve(x)                /*invoke the  HALVE   function.        */
                      y=double(y)                /*   "    "   DOUBLE      "            */
                      end   /*while*/            /* [↑]  Ethiopian multiplication method*/
         return $*s/1                            /*maintain the correct sign for product*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
double:  return  arg(1)  * 2                     /*   *   is REXX's  multiplication.    */
halve:   return  arg(1)  % 2                     /*   %    "   "     integer division.  */
isEven:  return  arg(1) // 2 == 0                /*   //   "   "     division remainder.*/
```

'''output'''   when the following input is used:   <tt> 30   -7 </tt>

```txt

a= 30
b= -7
product= -210

```



### with error checking

This REXX version also aligns the "input" messages and also performs some basic error checking.

Note that the 2<sup>nd</sup> number needn't be an integer, any valid number will work.

```rexx
/*REXX program multiplies two integers by the  Ethiopian  (or Russian peasant)  method. */
numeric digits 3000                              /*handle some gihugeic integers.       */
parse arg a b _ .                                /*get two numbers from the command line*/
if a==''              then call error  "1st argument wasn't specified."
if b==''              then call error  "2nd argument wasn't specified."
if _\==''             then call error  "too many arguments were specified: "  _
if \datatype(a, 'W')  then call error  "1st argument isn't an integer: "      a
if \datatype(b, 'N')  then call error  "2nd argument isn't a valid number: "  b
p=eMult(a, b)                                    /*Ethiopian or Russian peasant method. */
w=max(length(a), length(b), length(p))           /*find the maximum width of 3 numbers. */
say  '      a='  right(a, w)                     /*use right justification to display A.*/
say  '      b='  right(b, w)                     /* "    "         "        "    "    B.*/
say  'product='  right(p, w)                     /* "    "         "        "    "    P.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
eMult:   procedure;  parse arg x,y;  s=sign(x)   /*obtain the two arguments; sign for X.*/
         $=0                                     /*product of the two integers (so far).*/
                      do  while x\==0            /*keep processing while   X   not zero.*/
                      if \isEven(x)  then $=$+y  /*if odd,  then add   Y   to product.  */
                      x= halve(x)                /*invoke the  HALVE   function.        */
                      y=double(y)                /*   "    "   DOUBLE      "            */
                      end   /*while*/            /* [↑]  Ethiopian multiplication method*/
         return $*s/1                            /*maintain the correct sign for product*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
double:  return  arg(1)  * 2                     /*   *   is REXX's  multiplication.    */
halve:   return  arg(1)  % 2                     /*   %    "   "     integer division.  */
isEven:  return  arg(1) // 2 == 0                /*   //   "   "     division remainder.*/
error:   say '***error!***' arg(1);    exit 13   /*display an error message to terminal.*/
```

'''output'''   when the following input is used:   <tt> 200   0.333 </tt>

```txt

      a=   200
      b= 0.333
product=  66.6

```



## Ring


```ring

x = 17
y = 34
p = 0
while x != 0
      if not even(x)
         p += y
         see "" + x + " " + " " + y + nl
      else
         see "" + x + "  ---" + nl ok
         x = halve(x)
         y = double(y)
end
see " " + "  ===" + nl
see "   " + p

func double n return (n * 2)
func halve n return floor(n / 2)
func even n return ((n & 1) = 0)

```

Output:

```txt

17  34
8  ---
4  ---
2  ---
1  544
   ===
   578

```



## Ruby

Iterative and recursive implementations here.
I've chosen to highlight the example 20*5 which I think is more illustrative.

```ruby
def halve(x)   x/2  end
def double(x)  x*2  end

# iterative
def ethiopian_multiply(a, b)
  product = 0
  while a >= 1
    p [a, b, a.even? ? "STRIKE" : "KEEP"] if $DEBUG
    product += b unless a.even?
    a = halve(a)
    b = double(b)
  end
  product
end

# recursive
def rec_ethiopian_multiply(a, b)
  return 0 if a < 1
  p [a, b, a.even? ? "STRIKE" : "KEEP"] if $DEBUG
  (a.even? ? 0 : b) + rec_ethiopian_multiply(halve(a), double(b))
end

$DEBUG = true   # $DEBUG also set to true if "-d" option given
a, b = 20, 5
puts "#{a} * #{b} = #{ethiopian_multiply(a,b)}"; puts
```


```txt

[20, 5, "STRIKE"]
[10, 10, "STRIKE"]
[5, 20, "KEEP"]
[2, 40, "STRIKE"]
[1, 80, "KEEP"]
20 * 5 = 100
```


A test suite:

```ruby
require 'test/unit'
class EthiopianTests < Test::Unit::TestCase
  def test_iter1; assert_equal(578, ethopian_multiply(17,34)); end
  def test_iter2; assert_equal(100, ethopian_multiply(20,5));  end
  def test_iter3; assert_equal(5,   ethopian_multiply(5,1));   end
  def test_iter4; assert_equal(5,   ethopian_multiply(1,5));   end
  def test_iter5; assert_equal(0,   ethopian_multiply(5,0));   end
  def test_iter6; assert_equal(0,   ethopian_multiply(0,5));   end
  def test_rec1;  assert_equal(578, rec_ethopian_multiply(17,34)); end
  def test_rec2;  assert_equal(100, rec_ethopian_multiply(20,5));  end
  def test_rec3;  assert_equal(5,   rec_ethopian_multiply(5,1));   end
  def test_rec4;  assert_equal(5,   rec_ethopian_multiply(1,5));   end
  def test_rec5;  assert_equal(0,   rec_ethopian_multiply(5,0));   end
  def test_rec6;  assert_equal(0,   rec_ethopian_multiply(0,5));   end
end
```


```txt
Run options:

# Running tests:

............
Finished tests in 0.014001s, 857.0816 tests/s, 857.0816 assertions/s.

12 tests, 12 assertions, 0 failures, 0 errors, 0 skips

ruby -v: ruby 2.0.0p247 (2013-06-27) [i386-mingw32]

```



## Rust


```rust
fn double(a: i32) -> i32 {
    2*a
}

fn halve(a: i32) -> i32 {
    a/2
}

fn is_even(a: i32) -> bool {
    a % 2 == 0
}

fn ethiopian_multiplication(mut x: i32, mut y: i32) -> i32 {
    let mut sum = 0;

    while x >= 1 {
        print!("{} \t {}", x, y);
        match is_even(x) {
            true  => println!("\t Not Kept"),
            false => {
                println!("\t Kept");
                sum += y;
            }
        }
        x = halve(x);
        y = double(y);
    }
    sum
}

fn main() {
    let output = ethiopian_multiplication(17, 34);
    println!("---------------------------------");
    println!("\t {}", output);
}
```


```txt
17       34      Kept
8        68      Not Kept
4        136     Not Kept
2        272     Not Kept
1        544     Kept
---------------------------------
         578
```



## Scala

The first and second are only slightly different and use functional style.
The third uses a for loop to yield the result.
The fourth uses recursion.


```scala

def ethiopian(i:Int, j:Int):Int=
   pairIterator(i,j).filter(x=> !isEven(x._1)).map(x=>x._2).foldLeft(0){(x,y)=>x+y}

def ethiopian2(i:Int, j:Int):Int=
   pairIterator(i,j).map(x=>if(isEven(x._1)) 0 else x._2).foldLeft(0){(x,y)=>x+y}

def ethiopian3(i:Int, j:Int):Int=
{
   var res=0;
   for((h,d) <- pairIterator(i,j) if !isEven(h)) res+=d;
   res
}

def ethiopian4(i: Int, j: Int): Int = if (i == 1) j else ethiopian(halve(i), double(j)) + (if (isEven(i)) 0 else j)

def isEven(x:Int)=(x&1)==0
def halve(x:Int)=x>>>1
def double(x:Int)=x<<1

// generates pairs of values (halve,double)
def pairIterator(x:Int, y:Int)=new Iterator[(Int, Int)]
{
   var i=(x, y)
   def hasNext=i._1>0
   def next={val r=i; i=(halve(i._1), double(i._2)); r}
}

```



## Scheme

In Scheme, <code>even?</code> is a standard procedure.

```scheme
(define (halve num)
  (quotient num 2))

(define (double num)
  (* num 2))

(define (*mul-eth plier plicand acc)
  (cond ((zero? plier) acc)
        ((even? plier) (*mul-eth (halve plier) (double plicand) acc))
        (else (*mul-eth (halve plier) (double plicand) (+ acc plicand)))))

(define (mul-eth plier plicand)
  (*mul-eth plier plicand 0))

(display (mul-eth 17 34))
(newline)
```

Output:
 578


## Seed7

Ethiopian Multiplication is another name for the peasant multiplication:


```seed7
const proc: double (inout integer: a) is func
  begin
    a *:= 2;
  end func;

const proc: halve (inout integer: a) is func
  begin
    a := a div 2;
  end func;

const func boolean: even (in integer: a) is
  return not odd(a);

const func integer: peasantMult (in var integer: a, in var integer: b) is func
  result
    var integer: result is 0;
  begin
    while a <> 0 do
      if not even(a) then
        result +:= b;
      end if;
      halve(a);
      double(b);
    end while;
  end func;
```


Original source (without separate functions for doubling, halving, and checking if a number is even): [http://seed7.sourceforge.net/algorith/math.htm#peasantMult]


## Sidef


```ruby
func double (n) { n << 1 }
func halve  (n) { n >> 1 }
func isEven (n) { n&1 == 0 }

func ethiopian_mult(a, b) {
    var r = 0
    while (a > 0) {
        r += b if !isEven(a)
        a = halve(a)
        b = double(b)
    }
    return r
}

say ethiopian_mult(17, 34)
```

```txt

578

```



## Smalltalk

```smalltalk
Number extend [
  double [ ^ self * 2 ]
  halve  [ ^ self // 2 ]
  ethiopianMultiplyBy: aNumber withTutor: tutor [
    |result multiplier multiplicand|
    multiplier := self.
    multiplicand := aNumber.
    tutor ifTrue: [ ('ethiopian multiplication of %1 and %2' %
                      { multiplier. multiplicand }) displayNl ].
    result := 0.
    [ multiplier >= 1 ]
      whileTrue: [
        multiplier even ifFalse: [
                           result := result + multiplicand.
                           tutor ifTrue: [
                              ('%1, %2 kept' % { multiplier. multiplicand })
                                displayNl
                           ]
                        ]
                        ifTrue: [
                           tutor ifTrue: [
                             ('%1, %2 struck' % { multiplier. multiplicand })
			       displayNl
                           ]
                        ].
        multiplier := multiplier halve.
        multiplicand := multiplicand double.
      ].
    ^result
  ]
  ethiopianMultiplyBy: aNumber [ ^ self ethiopianMultiplyBy: aNumber withTutor: false ]
].
```



```smalltalk
(17 ethiopianMultiplyBy: 34 withTutor: true) displayNl.
```



## SNOBOL4


```snobol4

	define('halve(num)')	:(halve_end)
halve	eq(num,1)	:s(freturn)
	halve = num / 2	:(return)
halve_end

	define('double(num)')	:(double_end)
double	double = num * 2	:(return)
double_end

	define('odd(num)')	:(odd_end)
odd	eq(num,1)	:s(return)
	eq(num,double(halve(num)))	:s(freturn)f(return)

odd_end	l = trim(input)
	r = trim(input)
	s = 0
next	s = odd(l) s + r
	r = double(r)
	l = halve(l)	:s(next)
stop  	output = s
end
```



## SNUSP



```snusp
    /==!/==atoi==@@@-@-----#
    |   |          /-\          /recurse\    #/?\ zero
$>,@/>,@/?\<=zero=!\?/<=print==!\@\>?!\@/<@\.!\-/
        < @     #                 |   \=/  \=itoa=@@@+@+++++#
     /==\ \===?!/===-?\>>+# halve !     /+ !/+ !/+ !/+   \    mod10
#    !  @ |  #>>\?-<+>/           /<+> -\!?-\!?-\!?-\!?-\!
/-<+>\  > ?     />+<<++>-\        \?!\-?!\-?!\-?!\-?!\-?/\    div10
?down?  | \-<<<!\
### =
?/\ add &    #  +/! +/! +/! +/! +/
\>+<-/  | \=<<<!/====?\=\ | double
!    #  |       \<++>-/ | |
\
### =
\!@>
### ======
/!/
```


This is possibly the smallest multiply routine so far discovered for SNUSP.


## Soar


```soar
##########################################
# multiply takes ^left and ^right numbers
# and a ^return-to
sp {multiply*elaborate*initialize
   (state <s> ^superstate.operator <o>)
   (<o> ^name multiply
        ^left <x>
        ^right <y>
        ^return-to <r>)
-->
   (<s> ^name multiply
        ^left <x>
        ^right <y>
        ^return-to <r>)}

sp {multiply*propose*recurse
   (state <s> ^name multiply
              ^left <x> > 0
              ^right <y>
              ^return-to <r>
             -^multiply-done)
-->
   (<s> ^operator <o> +)
   (<o> ^name multiply
        ^left (div <x> 2)
        ^right (* <y> 2)
        ^return-to <s>)}

sp {multiply*elaborate*mod
   (state <s> ^name multiply
              ^left <x>)
-->
   (<s> ^left-mod-2 (mod <x> 2))}

sp {multiply*elaborate*recursion-done-even
   (state <s> ^name multiply
              ^left <x>
              ^right <y>
              ^multiply-done <temp>
              ^left-mod-2 0)
-->
   (<s> ^answer <temp>)}

sp {multiply*elaborate*recursion-done-odd
   (state <s> ^name multiply
              ^left <x>
              ^right <y>
              ^multiply-done <temp>
              ^left-mod-2 1)
-->
   (<s> ^answer (+ <temp> <y>))}

sp {multiply*elaborate*zero
   (state <s> ^name multiply
              ^left 0)
-->
   (<s> ^answer 0)}

sp {multiply*elaborate*done
   (state <s> ^name multiply
              ^return-to <r>
              ^answer <a>)
-->
   (<r> ^multiply-done <a>)}
```



## Swift


```swift
import Darwin

func ethiopian(var #int1:Int, var #int2:Int) -> Int {
  var lhs = [int1], rhs = [int2]

  func isEven(#n:Int) -> Bool {return n % 2 == 0}
  func double(#n:Int) -> Int {return n * 2}
  func halve(#n:Int) -> Int {return n / 2}

  while int1 != 1 {
    lhs.append(halve(n: int1))
    rhs.append(double(n: int2))
    int1 = halve(n: int1)
    int2 = double(n: int2)
  }

  var returnInt = 0
  for (a,b) in zip(lhs, rhs) {
    if (!isEven(n: a)) {
      returnInt += b
    }
  }
  return returnInt
}

println(ethiopian(int1: 17, int2: 34))
```

```txt
578
```



## Tcl


```tcl
# This is how to declare functions - the mathematical entities - as opposed to procedures
proc function {name arguments body} {
    uplevel 1 [list proc tcl::mathfunc::$name $arguments [list expr $body]]
}

function double n {$n * 2}
function halve n {$n / 2}
function even n {($n & 1) == 0}
function mult {a b} {
    $a < 1 ? 0 :
    even($a) ? [logmult STRUCK] + mult(halve($a), double($b))
	     : [logmult KEPT]   + mult(halve($a), double($b)) + $b
}

# Wrapper to set up the logging
proc ethiopianMultiply {a b {tutor false}} {
    if {$tutor} {
	set wa [expr {[string length $a]+1}]
	set wb [expr {$wa+[string length $b]-1}]
	puts stderr "Ethiopian multiplication of $a and $b"
	interp alias {} logmult {} apply {{wa wb msg} {
	    upvar 1 a a b b
	    puts stderr [format "%*d %*d %s" $wa $a $wb $b $msg]
	    return 0
	}} $wa $wb
    } else {
	proc logmult args {return 0}
    }
    return [expr {mult($a,$b)}]
}
```
Demo code:
```tcl
puts "17 * 34 = [ethiopianMultiply 17 34 true]"
```
 Ethiopian multiplication of 17 and 34
 17   34 KEPT
  8   68 STRUCK
  4  136 STRUCK
  2  272 STRUCK
  1  544 KEPT
 17 * 34 = 578


## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
ASK "insert number1", nr1=""
ASK "insert number2", nr2=""

SET nrs=APPEND(nr1,nr2),size_nrs=SIZE(nrs)
IF (size_nrs!=2) ERROR/STOP "insert two numbers"
LOOP n=nrs
IF (n!='digits') ERROR/STOP n, " is not a digit"
ENDLOOP

PRINT "ethopian multiplication of ",nr1," and ",nr2

SET sum=0
SECTION checkifeven
SET even=MOD(nr1,2)
 IF (even==0) THEN
   SET action="struck"
 ELSE
   SET action="kept"
   SET sum=APPEND (sum,nr2)
 ENDIF
SET nr1=CENTER (nr1,+6),nr2=CENTER (nr2,+6),action=CENTER (action,8)
PRINT nr1,nr2,action
ENDSECTION

SECTION halve_i
SET nr1=nr1/2
ENDSECTION

SECTION double_i
nr2=nr2*2
ENDSECTION

DO checkifeven

LOOP
DO halve_i
DO double_i
DO checkifeven
IF (nr1==1) EXIT
ENDLOOP

SET line=REPEAT ("=",20), sum = sum(sum),sum=CENTER (sum,+12)
PRINT line
PRINT sum

```
 ethopian multiplication of 17 and 34
    17    34  kept
     8    68 struck
     4   136 struck
     2   272 struck
     1   544  kept

### ==============

         578


## UNIX Shell

Tried with ''bash --posix'', and also with Heirloom's ''sh''. Beware that ''bash --posix'' has more features than ''sh''; this script uses only ''sh'' features.

```bash
halve()
{
    expr "$1" / 2
}

double()
{
    expr "$1" \* 2
}

is_even()
{
    expr "$1" % 2 = 0 >/dev/null
}

ethiopicmult()
{
    plier=$1
    plicand=$2
    r=0
    while [ "$plier" -ge 1 ]; do
	is_even "$plier" || r=`expr $r + "$plicand"`
	plier=`halve "$plier"`
	plicand=`double "$plicand"`
    done
    echo $r
}

ethiopicmult 17 34
# => 578
```


While breaking if the --posix flag is passed to bash, the following alternative script avoids the *, /, and % operators. It also uses local variables and built-in arithmetic.

```bash
halve() {
  (( $1 >>= 1 ))
}

double() {
  (( $1 <<= 1 ))
}

is_even() {
  (( ($1 & 1) == 0 ))
}

multiply() {
  local plier=$1
  local plicand=$2
  local result=0

  while (( plier > 0 ))
  do
    is_even plier || (( result += plicand ))
    halve plier
    double plicand
  done
  echo $result
}

multiply 17 34
# => 578
```


=
## C Shell
=

```csh
alias halve '@ \!:1 /= 2'
alias double '@ \!:1 *= 2'
alias is_even '@ \!:1 = ! ( \!:2 % 2 )'

alias multiply eval \''set multiply_args=( \!*:q )		\\
	@ multiply_plier = $multiply_args[2]			\\
	@ multiply_plicand = $multiply_args[3]			\\
	@ multiply_result = 0					\\
	while ( $multiply_plier > 0 )				\\
		is_even multiply_is_even $multiply_plier	\\
		if ( ! $multiply_is_even ) then			\\
			@ multiply_result += $multiply_plicand	\\
		endif						\\
		halve multiply_plier				\\
		double multiply_plicand				\\
	end							\\
	@ $multiply_args[1] = $multiply_result			\\
'\'

multiply p 17 34
echo $p
# => 578
```



## Ursala

This solution makes use of the functions odd, double, and half, which respectively
check the parity, double a given natural number, or perform truncating division by two. These
functions are normally imported from the nat library but defined here explicitly for
the sake of completeness.
```Ursala
odd    = ~&ihB
double = ~&iNiCB
half   = ~&itB
```
The functions above are defined in terms of bit manipulations exploiting the concrete representations of natural numbers. The remaining code treats natural numbers instead as abstract types by way of the library API, and uses the operators for distribution (*-), triangular iteration (|\),
and filtering (*~) among others.
```Ursala
#import nat

emul = sum:-0@rS+ odd@l*~+ ^|(~&,double)|\+ *-^|\~& @iNC ~&h~=0->tx :^/half@h ~&
```
test program:
```Ursala
#cast %n

test = emul(34,17)
```
 578


## VBA

Define three named functions :
# one to '''halve an integer''',
# one to '''double an integer''', and
# one to '''state if an integer is even'''.

```vb
Private Function lngHalve(Nb As Long) As Long
    lngHalve = Nb / 2
End Function

Private Function lngDouble(Nb As Long) As Long
    lngDouble = Nb * 2
End Function

Private Function IsEven(Nb As Long) As Boolean
    IsEven = (Nb Mod 2 = 0)
End Function
```

Use these functions to create a function that does Ethiopian multiplication.
The first function below is a non optimized function :

```vb
Private Function Ethiopian_Multiplication_Non_Optimized(First As Long, Second As Long) As Long
Dim Left_Hand_Column As New Collection, Right_Hand_Column As New Collection, i As Long, temp As Long

'Take two numbers to be multiplied and write them down at the top of two columns.
    Left_Hand_Column.Add First, CStr(First)
    Right_Hand_Column.Add Second, CStr(Second)
'In the left-hand column repeatedly halve the last number, discarding any remainders,
    'and write the result below the last in the same column, until you write a value of 1.
    Do
        First = lngHalve(First)
        Left_Hand_Column.Add First, CStr(First)
    Loop While First > 1
'In the right-hand column repeatedly double the last number and write the result below.
    'stop when you add a result in the same row as where the left hand column shows 1.
    For i = 2 To Left_Hand_Column.Count
        Second = lngDouble(Second)
        Right_Hand_Column.Add Second, CStr(Second)
    Next

'Examine the table produced and discard any row where the value in the left column is even.
    For i = Left_Hand_Column.Count To 1 Step -1
        If IsEven(Left_Hand_Column(i)) Then Right_Hand_Column.Remove CStr(Right_Hand_Column(i))
    Next
'Sum the values in the right-hand column that remain to produce the result of multiplying
    'the original two numbers together
    For i = 1 To Right_Hand_Column.Count
        temp = temp + Right_Hand_Column(i)
    Next
    Ethiopian_Multiplication_Non_Optimized = temp
End Function
```

This one is better :

```vb
Private Function Ethiopian_Multiplication(First As Long, Second As Long) As Long
    Do
        If Not IsEven(First) Then Mult_Eth = Mult_Eth + Second
        First = lngHalve(First)
        Second = lngDouble(Second)
    Loop While First >= 1
    Ethiopian_Multiplication = Mult_Eth
End Function
```

Then you can call one of these functions like this :

```vb
Sub Main_Ethiopian()
Dim result As Long
    result = Ethiopian_Multiplication(17, 34)
    ' or :
    'result = Ethiopian_Multiplication_Non_Optimized(17, 34)
    Debug.Print result
End Sub
```



## VBScript

Nowhere near as optimal a solution as the Ada. Yes, it could have made as optimal, but the long way seemed more interesting.

Demonstrates a List class. The .recall and .replace methods have bounds checking but the code does not test for the exception that would be raised. List class extends the storage allocated for the list when the occupation of the list goes beyond the original allocation.

<code>option explicit</code> makes sure that all variables are declared.

'''Implementation'''
```vb
option explicit

class List
	private theList
	private nOccupiable
	private nTop

	sub class_initialize
		nTop = 0
		nOccupiable = 100
		redim theList( nOccupiable )
	end sub

	public sub store( x )
		if nTop >= nOccupiable then
			nOccupiable = nOccupiable + 100
			redim preserve theList( nOccupiable )
		end if
		theList( nTop ) = x
		nTop = nTop + 1
	end sub

	public function recall( n )
		if n >= 0 and n <= nOccupiable then
			recall = theList( n )
		else
			err.raise vbObjectError + 1000,,"Recall bounds error"
		end if
	end function

	public sub replace( n, x )
		if n >= 0 and n <= nOccupiable then
			theList( n )  = x
		else
			err.raise vbObjectError + 1001,,"Replace bounds error"
		end if
	end sub

	public property get listCount
		listCount = nTop
	end property

end class

function halve( n )
	halve = int( n / 2 )
end function

function twice( n )
	twice = int( n * 2 )
end function

function iseven( n )
	iseven = ( ( n mod 2 ) = 0 )
end function


function multiply( n1, n2 )
	dim LL
	set LL = new List

	dim RR
	set RR = new List

	LL.store n1
	RR.store n2

	do while n1 <> 1
		n1 = halve( n1 )
		LL.store n1
		n2 = twice( n2 )
		RR.store n2
	loop

	dim i
	for i = 0 to LL.listCount
		if iseven( LL.recall( i ) ) then
			RR.replace i, 0
		end if
	next

	dim total
	total = 0
	for i = 0 to RR.listCount
		total = total + RR.recall( i )
	next

	multiply = total
end function

```
'''Invocation'''
```vb

wscript.echo multiply(17,34)

```
 578


## x86 Assembly

{{works with|nasm}}, linking with the C standard library and start code.

```asm
	extern 	printf
	global	main

	section	.text

halve
	shr	ebx, 1
	ret

double
	shl	ebx, 1
	ret

iseven
	and	ebx, 1
	cmp	ebx, 0
	ret			; ret preserves flags

main
	push	1		; tutor = true
	push	34		; 2nd operand
	push	17		; 1st operand
	call	ethiopicmult
	add	esp, 12

	push	eax		; result of 17*34
	push	fmt
	call	printf
	add	esp, 8

	ret


%define plier 8
%define plicand 12
%define tutor 16

ethiopicmult
	enter	0, 0
	cmp	dword [ebp + tutor], 0
	je	.notut0
	push	dword [ebp + plicand]
	push	dword [ebp + plier]
	push	preamblefmt
	call	printf
	add	esp, 12
.notut0

	xor	eax, eax		; eax -> result
	mov	ecx, [ebp + plier] 	; ecx -> plier
	mov	edx, [ebp + plicand]    ; edx -> plicand

.whileloop
	cmp	ecx, 1
	jl	.multend
	cmp	dword [ebp + tutor], 0
	je	.notut1
	call	tutorme
.notut1
	mov	ebx, ecx
	call	iseven
	je	.iseven
	add	eax, edx	; result += plicand
.iseven
	mov	ebx, ecx	; plier >>= 1
	call	halve
	mov	ecx, ebx

	mov	ebx, edx	; plicand <<= 1
	call	double
	mov	edx, ebx

	jmp	.whileloop
.multend
	leave
	ret


tutorme
	push	eax
	push	strucktxt
	mov	ebx, ecx
	call	iseven
	je	.nostruck
	mov	dword [esp], kepttxt
.nostruck
	push	edx
	push	ecx
	push	tutorfmt
	call	printf
	add	esp, 4
	pop	ecx
	pop	edx
	add	esp, 4
	pop	eax
	ret

	section .data

fmt
	db	"%d", 10, 0
preamblefmt
	db	"ethiopic multiplication of %d and %d", 10, 0
tutorfmt
	db	"%4d %6d %s", 10, 0
strucktxt
	db	"struck", 0
kepttxt
	db	"kept", 0
```


### Smaller version

Using old style 16 bit registers created in debug

The functions to halve double and even are coded inline.
To half a value
   shr,1

to double a value
   shl,1

to test if the value is even


```asm
test,01
jz   Even
Odd:
Even:
```

```asm
;calling program

 1BDC:0100 6A11           PUSH   11  ;17  Put operands on the stack
 1BDC:0102 6A22           PUSH   22  ;34
 1BDC:0104 E80900         CALL   0110  ; call the mulitplcation routine
;putting some space in, (not needed)
 1BDC:0107 90             NOP
 1BDC:0108 90             NOP
 1BDC:0109 90             NOP
 1BDC:010A 90             NOP
 1BDC:010B 90             NOP
 1BDC:010C 90             NOP
 1BDC:010D 90             NOP
 1BDC:010E 90             NOP
 1BDC:010F 90             NOP
;mulitplication routine starts here
 1BDC:0110 89E5           MOV    BP,SP      ; prepare to get operands off stack
 1BDC:0112 8B4E02         MOV    CX,[BP+02] ; Get the first operand
 1BDC:0115 8B5E04         MOV    BX,[BP+04] ; get the second oerand
 1BDC:0118 31C0           XOR    AX,AX      ; zero out the result
 1BDC:011A F7C10100       TEST   CX,0001     ; are we odd
 1BDC:011E 7402           JZ     0122       ; no skip the next instruction
 1BDC:0120 01D8           ADD    AX,BX     ; we are odd so add to the result
 1BDC:0122 D1E3           SHL    BX,1      ; multiply by 2
 1BDC:0124 D1E9           SHR    CX,1      ; divide by 2 (if zr flag is set, we are done)
 1BDC:0126 75F2           JNZ    011A      ; cx not 0, go back and do it again
 1BDC:0128 C3             RET              ; return with the result in AX

;pretty small, just 24 bytes
```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations

func Halve(N);          \Return half of N
int  N;
return N>>1;

func Double(N);         \Return N doubled
int  N;
return N<<1;

func IsEven(N);         \Return 'true' if N is an even number
int  N;
return (N&1)=0;

func EthiopianMul(A, B); \Multiply A times B using Ethiopian method
int  A, B;
int  I, J, S, Left(100), Right(100);
[Left(0):= A;  Right(0):= B;            \1. write numbers to be multiplied
I:= 1;                                  \2. repeatedly halve number on left
repeat  A:= Halve(A);
        Left(I):= A;  I:= I+1;
until   A=1;
J:= 1;                                  \3. repeatedly double number on right
repeat  B:= Double(B);
        Right(J):= B;  J:= J+1;
until   J=I;                            \stop where left column = 1
for J:= 0 to I-1 do                     \4. discard right value if left is even
        if IsEven(Left(J)) then Right(J):= 0;
S:= 0;                                  \5. sum remaining values on right
for J:= 0 to I-1 do
        S:= S + Right(J);
for J:= 0 to I-1 do                     \show this insanity
        [IntOut(0, Left(J));  ChOut(0, 9\tab\);  IntOut(0, Right(J));  CrLf(0)];
Text(0, "       --------
");
return S;                               \sum = product
];

int Product;
[Product:= EthiopianMul(17, 34);
ChOut(0, 9);  IntOut(0, Product);  CrLf(0);  CrLf(0);
Product:= EthiopianMul(1234, 5678);
ChOut(0, 9);  IntOut(0, Product);  CrLf(0);
]
```
```txt
17      34
8       0
4       0
2       0
1       544
        --------
        578

1234    0
617     11356
308     0
154     0
77      90848
38      0
19      363392
9       726784
4       0
2       0
1       5814272
        --------
        7006652
```



## zkl

Trying to duplicate the task as stated, using columns.
isEven is a integer method.

```zkl
fcn ethiopianMultiply(l,r){ // l is a non-negative integer
   halve  :=fcn(n){ n/2 };
   double :=fcn(n){ n+n };
   lr:=List(T(l,r)); // ( (l,r) .. (1,r*n) )
   while(l>1){ lr.write( T(l=halve(l),r=double(r)) ) }
   lr.filter(fcn([(l,r)]){ (not l.isEven) }); // strike out even left rows
   .reduce(fcn(sum,[(l,r)]){ sum + r },0);  // sum right column
}
```


```zkl
foreach l,r in ( T(T(17,34),T(34,1),T(34,2),T(34,0)) ){
   println(ethiopianMultiply(l,r)," ",ethiopianMultiply(r,l));
}
```

```txt

578 578
34 34
68 68
0 0

```



## ZX Spectrum Basic

```zxbasic
10 DEF FN e(a)=a-INT (a/2)*2-1
20 DEF FN h(a)=INT (a/2)
30 DEF FN d(a)=2*a
40 LET x=17: LET y=34: LET tot=0
50 IF x<1 THEN GO TO 100
60 PRINT x;TAB (4);
70 IF FN e(x)=0 THEN LET tot=tot+y: PRINT y: GO TO 90
80 PRINT "---"
90 LET x=FN h(x): LET y=FN d(y): GO TO 50
100 PRINT TAB (4);"===",TAB (4);tot
```


