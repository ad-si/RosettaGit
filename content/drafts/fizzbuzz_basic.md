+++
title = "FizzBuzz/Basic"
description = ""
date = 2019-04-21T17:54:08Z
aliases = []
[extra]
id = 19592
[taxonomies]
categories = []
tags = []
+++

{{collection|FizzBuzz}}


## Applesoft BASIC


```applesoftbasic
10  DEF  FN M(N) = ((A / N) -  INT (A / N)) * N
20  FOR A = 1 TO 100
30  LET O$ = ""
40  IF  FN M(3) = 0 THEN O$ = "FIZZ"
50  IF  FN M(5) = 0 THEN O$ = O$ + "BUZZ"
60  IF O$ = "" THEN O$ =  STR$ (A)
70  PRINT O$
80  NEXT A

```



## BaCon


```freebasic
' FizzBuzz
FOR i = 1 TO 100
    msg$ = IIF$(MOD(i, 3), "", "Fizz")
    msg$ = msg$ & IIF$(MOD(i, 5), "", "Buzz")
    PRINT IIF$(msg$ = "", STR$(i), msg$), " ";
NEXT
PRINT
```


{{out}}

```txt
prompt$ bacon fizzbuzz.bac
Converting 'fizzbuzz.bac'... done, 7 lines were processed in 0.003 seconds.
Compiling 'fizzbuzz.bac'... cc  -c fizzbuzz.bac.c
cc -o fizzbuzz fizzbuzz.bac.o -lbacon -lm
Done, program 'fizzbuzz' ready.
prompt$ ./fizzbuzz | fold -w 72 -s
1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17 Fizz 19
Buzz Fizz 22 23 Fizz Buzz 26 Fizz 28 29 FizzBuzz 31 32 Fizz 34 Buzz
Fizz 37 38 Fizz Buzz 41 Fizz 43 44 FizzBuzz 46 47 Fizz 49 Buzz Fizz 52
53 Fizz Buzz 56 Fizz 58 59 FizzBuzz 61 62 Fizz 64 Buzz Fizz 67 68 Fizz
Buzz 71 Fizz 73 74 FizzBuzz 76 77 Fizz 79 Buzz Fizz 82 83 Fizz Buzz 86
Fizz 88 89 FizzBuzz 91 92 Fizz 94 Buzz Fizz 97 98 Fizz Buzz
```



## BASIC

{{works with|QuickBasic|4.5}}

### If/else ladder approach


```qbasic
FOR A = 1 TO 100
   IF A MOD 15 = 0 THEN
      PRINT "FizzBuzz"
   ELSE IF A MOD 3 = 0 THEN
      PRINT "Fizz"
   ELSE IF A MOD 5 = 0 THEN
      PRINT "Buzz"
   ELSE
      PRINT A
   END IF
NEXT A
```



### Concatenation approach


```qbasic
FOR A = 1 TO 100
   OUT$ = ""

   IF A MOD 3 = 0 THEN 
      OUT$ = "Fizz"
   END IF

   IF A MOD 5 = 0 THEN
      OUT$ = OUT$ + "Buzz"
   END IF
   
   IF OUT$ = "" THEN
      OUT$ = STR$(A)
   END IF

   PRINT OUT$
NEXT A
```

See also: [[#RapidQ|RapidQ]]


## BBC BASIC


```bbcbasic
      FOR number% = 1 TO 100
        CASE TRUE OF
          WHEN number% MOD 15 = 0: PRINT "FizzBuzz"
          WHEN number% MOD 3 = 0:  PRINT "Fizz"
          WHEN number% MOD 5 = 0:  PRINT "Buzz"
          OTHERWISE: PRINT ; number%
        ENDCASE
      NEXT number%
```



## Basic Casio

<lang>For 1→A To 100 Step 1↵
MOD(A,15)→B↵
MOD(A,3)→C↵
MOD(A,5)→D↵
↵
B=0⇒"FIZZBUZZ"↵
B=0⇒Goto 1↵
C=0⇒"BUZZ"↵
C=0⇒Goto 1↵
D=0⇒"FIZZ"↵
D=0⇒Goto 1↵
↵
A◢
Goto 1
↵
Lbl 1↵
Next
```



## FreeBASIC


```freebasic
'FreeBASIC
Dim As ULong n

For n = 1 To 100

    If n Mod 15 = 0  Then
        Print "FizzBuzz"
    ElseIf n Mod 3 = 0 Then
        Print "Fizz"
    ElseIf n Mod 5 = 0 Then
        Print "Buzz"
    Else
        Print n
    End If

Next n

sleep
```


=={{header|GW-BASIC}}==

```qbasic
10 FOR N = 1 TO 100
20     OP$ = ""
30     IF N MOD 3 = 0 THEN OP$ = "Fizz"
40     IF N MOD 5 = 0 THEN OP$ = OP$ + "Buzz"
50     IF OP$ = "" THEN PRINT N ELSE PRINT OP$
60 NEXT N

```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Fizzbuzz.bas"
110 FOR I=1 TO 100
120   IF MOD(I,15)=0 THEN
130     PRINT "FizzBuzz"
140   ELSE IF MOD(I,3)=0 THEN
150     PRINT "Fizz"
160   ELSE IF MOD(I,5)=0 THEN
170     PRINT "Buzz"
180   ELSE
190     PRINT I
200   END IF
210 NEXT
```



## Liberty BASIC


```lb
for i = 1 to 100
    select case
        case i mod 15 = 0
            print "FizzBuzz"
        case i mod 3 = 0
            print "Fizz"
        case i mod 5 = 0
            print "Buzz"
        case else
            print i
    end select
next i
```




## PureBasic


```purebasic
OpenConsole()
For x = 1 To 100
  If x%15 = 0
    PrintN("FizzBuzz")
  ElseIf x%3 = 0
    PrintN("Fizz")
  ElseIf x%5 = 0
    PrintN("Buzz")
  Else
    PrintN(Str(x))
  EndIf
Next
Input()
```



## REALbasic


```vb
  For i As Integer = 1 To 100
    If i mod 3 = 0 And i mod 5 = 0 Then
      Print("FizzBuzz")
    ElseIf i mod 3 = 0 Then
      Print("Fizz")
    ElseIf i mod 5 = 0 Then
      Print("Buzz")
    Else
      Print(Str(i))
    End If
  Next
```

An alternative syntax:

```vb

  For i As Integer = 1 To 100
    Select Case True
      Case i mod 3 = 0 And i mod 5 = 0
      Print("FizzBuzz")
    Case i mod 3 = 0
      Print("Fizz")
    Case i mod 5 = 0
      Print("Buzz")
    Else
      Print(Str(i))
    End Select
  Next
```



## Ring


```ring

for n = 1 to 20
    if n % 15 = 0 see "" + n + " = " + "FizzBuzz" + nl loop
    but n % 3 = 0 see "" + n + " = " + "Fizz"+ nl
    but n % 5 = 0 see "" + n + " = " + "Buzz" + nl
    else see "" + n + " = " + n + nl ok
next

```

Output:

```txt

1 = 1
2 = 2
3 = Fizz
4 = 4
5 = Buzz
6 = Fizz
7 = 7
8 = 8
9 = Fizz
10 = Buzz
11 = 11
12 = Fizz
13 = 13
14 = 14
15 = FizzBuzz
16 = 16
17 = 17
18 = Fizz
19 = 19
20 = Buzz

```



## Run BASIC


```runbasic
for i = 1 to 100
 print i;
 if (i mod 15) = 0 then print " FizzBuzz";
 if (i mod  3) = 0 then print " Fizz";
 if (i mod  5) = 0 then print " Buzz";
 print
next i
```



## tbas


```qbasic
FOR i = 1 TO 100
	LET SHOW$ = ""
	IF MOD(i,3) = 0 THEN
		LET SHOW$ = SHOW$ + "Fizz"
	END IF
	IF MOD(i,5) = 0 THEN
		LET SHOW$ = SHOW$ + "Buzz"
	END IF
	IF SHOW$ = "" THEN
		LET SHOW$ = STR$(i)
	END IF
		
	PRINT SHOW$;" ";
NEXT
```


=={{header|TI-83 BASIC}}==

```ti83b
PROGRAM:FIZZBUZZ
For(I,1,100
I
If not(fPart(I/3
"FIZZ
If not(fPart(I/5
"BUZZ
If not(fPart(I/15
"FIZZBUZZ
Disp Ans
:End
```



## Visual Basic .NET

'''Platform:''' [[.NET]]
{{works with|Visual Basic .NET|9.0+}}

```vbnet
Sub Main()
 
    For i = 1 To 100
        If i Mod 15 = 0 Then
            Console.WriteLine("FizzBuzz")
        ElseIf i Mod 5 = 0 Then
            Console.WriteLine("Buzz")
        ElseIf i Mod 3 = 0 Then
            Console.WriteLine("Fizz")
        Else
            Console.WriteLine(i)
        End If
    Next
 
End Sub
```


== {{header|XBasic}} ==
{{works with|Windows XBasic}}

```xbasic

PROGRAM "fizzbuzz"
VERSION "0.0001"

DECLARE FUNCTION Entry()

FUNCTION Entry()
  FOR i% = 1 TO 100
    SELECT CASE TRUE
      CASE i% MOD 15 = 0:
        PRINT "FizzBuzz"
      CASE i% MOD 3 = 0:
        PRINT "Fizz"
      CASE i% MOD 5 = 0:
        PRINT "Buzz"
      CASE ELSE:
        PRINT i%
    END SELECT
  NEXT i%
END FUNCTION

END PROGRAM

```

