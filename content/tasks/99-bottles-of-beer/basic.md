+++
title = "Basic"
description = ""
date = 2019-10-01T22:14:14Z
aliases = []
[extra]
id = 18268
[taxonomies]
categories = []
tags = []
+++

<!--
=Task in Basic(s)=
-->
{{collection|99 Bottles of Beer}} [[implementation of task::99 Bottles of Beer| ]]
[[99 Bottles of Beer]] done in any of the BASIC-languages.
__toc__


## BASIC

=
## QuickBASIC
=
{{works with|QuickBASIC|4.5}}

### =Sound=

This version plays the tune 100 times while printing out the lyrics (not synchronized).

```qbasic
PLAY "<"
FOR x = 99 TO 0 STEP -1
  PRINT x; "bottles of beer on the wall"
  PRINT x; "bottles of beer"
  PRINT "Take one down, pass it around"
  PRINT x-1; "bottles of beer on the wall"
  PRINT
  PLAY "e-8e-8e-8<b-8b-8b-8>e-8e-8e-8e-4"'X bottles of beer on the wall
  PLAY "f8f8f8c8c8c8f4"'X bottles of beer
  PLAY "d4d8d8 N0 d8d8d8d4"'take one down, pass it around
  PLAY "<a+8a+8a+8>c8c8d8d+8d+8d+8d+4"'X-1 bottles of beer on the wall
NEXT x
```



### =Text=


```qbasic
FOR x = 99 TO 1 STEP -1
  PRINT x; "bottles of beer on the wall"
  PRINT x; "bottles of beer"
  PRINT "Take one down, pass it around"
  PRINT x-1; "bottles of beer on the wall"
  PRINT
NEXT x
```


=
## Applesoft BASIC
=

```Applesoft BASIC
H$ = "HELLO, WORLD!":B$ = " BOTTLES OF BEER":N$ =  CHR$ (13):W$ = " ON THE WALL" + N$: FOR B = 99 TO 1 STEP  - 1: PRINT B;B$W$B" "B$N$"TAKE ONE DOWN, PASS IT AROUND"N$B - 1;B$W$: NEXT
```



### BaCon

With lyrics and punctuation taken from the 99-bottles-of-beer.net site.

```freebasic
' 99 bottles of beer on the wall
DECLARE counter$
DECLARE bottle$
DECLARE ofbeer$ = "of beer"
DECLARE onthewall$ = "on the wall"

SUB howmany(b)
    LOCAL plural$
    plural$ = IIF$(b != 1, "s", "")
    bottle$ = "bottle" & plural$
    counter$ = IIF$(b >= 1, STR$(b), "no more")
END SUB

FOR bottles = 99 DOWNTO 0
    howmany(bottles)
    IF counter$ = "no more" THEN counter$ = "No more"
    PRINT counter$, bottle$, ofbeer$, onthewall$ FORMAT "%s %s %s %s, "
    IF counter$ = "No more" THEN counter$ = "no more"
    PRINT counter$, bottle$, ofbeer$ FORMAT "%s %s %s.\n"
    IF bottles > 0 THEN
        PRINT "Take one down and pass it around" FORMAT "%s, "
        howmany(bottles - 1)
    ELSE
        PRINT "Go to the store and buy some more" FORMAT "%s, "
        howmany(99)
    ENDIF
    PRINT counter$, bottle$, ofbeer$, onthewall$ FORMAT "%s %s %s %s.\n"
    IF bottles > 0 THEN PRINT
NEXT
```


=
## BASIC256
=

```BASIC256
#length of querter and eight note in ms
n4 = 1000 * 60 / 80 / 4
n8 = n4 / 2

#frequency of musical notes in hz
e = 330
ef = 311
b = 247
bf = 233
f = 349
c = 262
d = 294
ds = 311
a = 220

dim notes(1)
dim lengs(1)

# redim is automatic when using a {} list to assign an array
notes = {ef, ef, ef, bf, bf, bf, ef, ef, ef, ef, f , f , f , c , c , c , f , d , d , d , d , d , d , d , bf, bf, bf, c , c , ef, ef, ef, ef, ef}
lengs = {n8, n8, n8, n8, n8, n8, n8, n8, n8, n4, n8, n8, n8, n8, n8, n8, n4, n4, n8, n8, n8, n8, n8, n4, n8, n8, n8, n8, n8, n8, n8, n8, n8, n4 }

for x = 99 to 1 step -1
   for t = 0 to notes[?]-1
      if t = 0 then print x + " bottles of beer on the wall"
      if t = 11 then print x + " bottles of beer"
      if t = 18 then print "Take one down, pass it around"
      if t = 25 then print(x-1) + " bottles of beer on the wall"
      sound notes[t], lengs[t]
      pause .002
   next t
   print
next x

```


=
## BBC BASIC
=


```bbcbasic

      N_Bottles = 99

      beer$ = " of beer"
      wall$ = " on the wall"
      unit$ = "99 bottles"

      WHILE N_Bottles >= 0

        IF N_Bottles=0 THEN
          PRINT '"No more bottles" beer$ wall$ ", " unit$ beer$ "."
          PRINT "Go to the store and buy some more, ";
        ELSE
          PRINT 'unit$ beer$ wall$ ", " unit$ beer$ "."
          PRINT "Take one down and pass it around, ";
        ENDIF

        N_Bottles -= 1

        CASE N_Bottles OF
          WHEN 0:
            unit$ = "no more bottles"
          WHEN 1:
            unit$ = "1 bottle"
          OTHERWISE:
            unit$ = STR$((N_Bottles + 100) MOD 100) + " bottles"
        ENDCASE

        PRINT unit$ beer$ wall$ "."

      ENDWHILE

      END

```


=
## Commodore BASIC
=

You may need to remove the semicolon on line 20 when executing on the VIC-20. Also, this looks best on the machines with 40 or more columns.


```FreeBasic

10 dim bo$(2):bo$(0)="bottle":bo$(1)="bottles"
15 print chr$(147);chr$(14);"99 Bottles of Beer Song"
20 print:print "Start with how many bottles";
25 input nb
30 if nb<1 then print "Sorry, must start with at least one. Try again.":goto 20
40 print
50 for c=nb to 1 step -1
60 c$=str$(c):if c=0 then c$=" No more"
65 br$=str$(c-1): if c-1=0 then br$=" No more"
70 print chr$(147)
80 print c$;" ";bo$((c=1)+1);" of beer on the wall,"
90 print c$;" ";bo$((c=1)+1);" of beer."
100 print:print "    Take one down, pass it around...":print
110 print br$;" ";bo$((c-1=1)+1);" of beer on the wall!"
120 for t=1 to 1200:get k$:if k$="" then next t
130 if k$="q" then end
135 rem move to next verse
140 print:next c

```


=
## Creative Basic
=

### =window version=


```Creative Basic
DEF Win:WINDOW
DEF Close:CHAR
DEF ScreenSizeX,ScreenSizeY:INT

DECLARE VSpace(Number:UINT)
DECLARE CLR()

DEF TheLine$[4],Number$,Erase:STRING
DEF TheLine,TextHeight,TextWidth:INT
DEF TextX,TextY:UINT

TheLine$[0]="bottles"
TheLine$[1]="of beer on the wall."
TheLine$[2]="of beer."
TheLine$[3]="Take one down, pass it around."

BottlesOfBeer=99
TheLine=1

GETSCREENSIZE(ScreenSizeX,ScreenSizeY)

WINDOW Win,0,0,ScreenSizeX,ScreenSizeY,@MINBOX|@MAXBOX|@SIZE,0,"99 Bottles Of Beer",MainHandler

GETTEXTSIZE(Win,TheLine$[3],TextWidth,TextHeight)

Erase$=STRING$(TextWidth," ")

PRINT Win,"Let's sing a song."

VSpace(2)

'1.2 seconds.
STARTTIMER Win,1200

GOSUB Sing

WAITUNTIL Close=1

CLOSEWINDOW Win

END

SUB MainHandler

     SELECT @CLASS

     CASE @IDCLOSEWINDOW

     Close=1

     CASE @IDTIMER

     GOSUB Sing

     ENDSELECT

RETURN

SUB Sing

     DEF Sing:INT

     Sing=TheLine

     MOVE Win,TextX,TextY

     Number$=STR$(BottlesOfBeer)

     IF BottlesOfBeer=0

          Number$="No more"
	  TheLine$[0]="bottles"
	  TheLine$[3]="Go to the store and buy some more."

     ENDIF

     IF BottlesOfBeer=1

	  TheLine$[0]="bottle"
	  TheLine$[3]="Take it down, pass it around."

     ENDIF

     IF TheLine=4 THEN Sing=1

     IF (TheLine=1)|(TheLine=2)|(TheLine=4)

	  IF BottlesOfBeer>-1 THEN PRINT Win,Number$+" "+TheLine$[0]+" "+TheLine$[Sing] ELSE GOSUB TheEnd

     ELSE

	  PRINT Win,TheLine$[3]

	  BottlesOfBeer=BottlesOfBeer-1

     ENDIF

     TheLine=TheLine+1

     VSpace(1)

     IF TheLine>4

           TheLine=1

	   VSpace(1)

     ENDIF

RETURN

SUB TheEnd

     PRINT Win,"What's the problem, offishur?"

     STOPTIMER Win

     VSpace(2)

     MOVE Win,TextX,TextY:PRINT Win,"That's all."

RETURN

SUB VSpace(Number:UINT)

     TextY=TextY+(TextHeight*Number)

     IF TextY+(TextHeight*8)>ScreenSizeY THEN CLR()

RETURN

SUB CLR()

     FOR X=0 TO ScreenSizeY

          MOVE Win,0,X:PRINT Win,Erase$

          TextY=8

     NEXT X

RETURN

```


### =console only version=

Rather quickly written and dirty.

```Creative Basic
REM Using the ESC key to exit will not work in console programs under Windows 95/98 or ME.

DECLARE SingWallLn()
DECLARE Delay1()
DECLARE Delay2()
'To use ESC Key to exit.
DECLARE Quit()
DECLARE TheEnd()

DEF Bottles:UINT
DEF Number$,Again$:STRING

OPENCONSOLE

PRINT"I'm going to sing a song.":PRINT

Delay1()

LABEL StartSong

Bottles=99

DO
     Quit()

     SingWallLn():Delay1()

     PRINT LTRIM$(STR$(Bottles))+Number$+" of beer.":Delay1()

     IF Bottles>0 THEN PRINT"Take one down, pass it around." ELSE PRINT"Take it down, pass it around.":Delay1()

     Bottles=Bottles-1

     SingWallLn()

     Delay2()

PRINT:PRINT

UNTIL Bottles=0

Delay2()

ClS

LABEL Question

INPUT"Sing it again (y or n)?",Again$

SELECT Again$

	CASE("y")
	CASE("Y")

	CLS

	GOTO StartSong

	CASE "n"
	CASE "N"

	CLS

	PRINT"Fine, be that way.":Delay2()

	TheEnd()

ENDSELECT

PRINT"Sorry, I didn't understand.":PRINT

GOTO Question

'Keep from running into subroutines.
END

SUB SingWallLn()

     IF Bottles=1 THEN Number$=" bottle" ELSE Number$=" bottles"

     PRINT LTRIM$(STR$(Bottles))+Number$+" of beer on the wall."

RETURN

SUB Delay1()

     FOR X=1 TO 7000:NEXT X

RETURN

SUB Delay2()

     FOR X=1 TO 1750000:NEXT X

RETURN

SUB Quit()

     'Close program by pressing the ESC key.
     'Will not work in console under Windows 95/98 or ME.
     IF GETKEYSTATE(0x1B) THEN TheEnd()

RETURN

SUB TheEnd()

     CLOSECONSOLE

     END

RETURN

```


=
## FBSL
=
This is a OO version, using FBSL v3.5 Beta

```qbasic
#AppType Console

Class Wall
	bottles

	Sub Initialize(%n = 99)
		bottles = n
	End Sub

	Method Denom()
		if bottles+1 > 1 then
			return "one"
		elseif bottles+1 = 1 then
			return "it"
		end if
	End Method

	Method StockUp( %n = 99 )
		bottles = n
	End Method

	Method TakeOneDown()
		bottles = bottles - 1
	end Method

	Method Pluraliser()
		if bottles > 1 then
			return "s"
		else
			return ""
		end if
	end method

	Method Sing()
		print bottles, " bottle", Pluraliser(), " of beer on the wall"
		print bottles, " bottle", Pluraliser(), " of beer"
		TakeOneDown()
		print "take ", Denom(), " down and pass it round"
		if bottles > 0 then
			print bottles, " bottle", Pluraliser(), " of beer on the wall"
			print
		else
			print "no more bottles of beer on the wall"
			print
			print "no more bottles of beer on the wall"
			print "no more bottles of beer on the wall"
			print "go to the store and buy some more"
			StockUp(99)
			print bottles, " bottle", Pluraliser(), " of beer on the wall"
			print
		end if
		return bottles
	End Method

End Class

Dim BeerSong as new Wall(99)

while BeerSong.Sing() <> 99
end while

```


=
## FreeBASIC
=

```freebasic
' FB 1.05.0 Win64

Dim As Integer bottles = 99
Dim As String b = " bottles "

Do
  Print Str(bottles); b; "of beer hanging on the wall"
  Print Str(bottles); b; "of beer hanging on the wall"
  Print "And if one bottle of beer should accidently be drunk"
  bottles -= 1
  If bottles = 1 Then
    b = " bottle "
  ElseIf bottles = 0 Then
    b = " bottles "
  End If
  Print "There'll be"; bottles; b; "of beer hanging on the wall"
  Print
Loop Until bottles = 0

Print "Press any key to sleep it off"
Sleep
```


=
## FUZE BASIC
=

```qbasic
CLS
DIM s$(1)
READ b,s$(0),s$(1),a$,b$,c$,d$,e$,f$
g$ = chr$(10)
WHILE b > 0 CYCLE
    PRINT b;a$;s$(b>1);b$;c$;b;a$;s$(b>1);b$;".";g$;d$;b-1;a$;s$(b-1<>1);b$;c$;g$
    b = b - 1
REPEAT
PRINT e$;a$;"s";b$;c$;e$;a$;"s";b$;".";g$;f$;a$;"s";b$;c$
DATA 99,"","s"," bottle"," of beer"," on the wall. ","Take one down, pass it around. "
DATA "No more","Go to the store and buy some more. 99"
END
```


=
## Integer BASIC
=

IMPORTANT NOTE: Integer BASIC was written (and hand-assembled by Woz himself)
for the Apple 1 and original Apple 2.
The Apple 1 has NO support for lower-case letters, and it was an expensive (and later) option on the Apple 2.
The UPPER-CASE output of this example accurately represents the only reasonable solution for those target devices, and therefore cannot be "fixed" for mixed case, only deleted.


```Integer BASIC
E000G (APPLE II)
E000R (APPLE I)
10 REM -------------------------
11 REM BEERSONG IN APPLE INTEGER
12 REM BASIC BY BARRYM 2011-8-21
13 REM THANKS : APPLEWIN1.17.2.0
14 REM THANKS ALSO : POM1 0.7B
15 REM -------------------------
16 REM PRINTS THE COMPLETE UPPER
17 REM CASE LYRICS ON AN APPLE I
18 REM OR AN 'ORIGINAL' APPLE II
19 REM WITH WOZ'S INTEGER BASIC.
20 REM -------------------------
21 REM THIS BASIC HAS AN UNUSUAL
22 REM 'THEN', WHICH EXECUTES OR
23 REM SKIPS ONE (AND ONLY ONE!)
24 REM STATEMENT.  THIS CONFUSED
25 REM US KIDS REGULARLY WHEN WE
26 REM TRIED TRANSLATING INTEGER
27 REM BASIC GAMES TO APPLE$OFT!
30 REM -------------------------
40 FOR B=99 TO 98 STEP 0: PRINT : FOR W=0 TO 2: IF W<2 THEN 70
50 IF B THEN PRINT "TAKE ONE DOWN AND PASS IT AROUND";:B=B-1
60 IF B+1 THEN 70:B=99: PRINT "GO TO THE STORE AND BUY SOME MORE";
70 IF W THEN PRINT ",": IF B THEN PRINT B;: IF B=0 THEN PRINT "NO MORE";
80 PRINT " BOTTLE";: IF B#1 THEN PRINT "S";: PRINT " OF BEER";
90 IF W#1 THEN PRINT " ON THE WALL";: IF W THEN PRINT ".": NEXT W,B: END
RUN
```


=
## Liberty BASIC
=

```lb
For bottles = 99 To 1 Step -1
    song$ = song$ + str$(bottles) + " bottle"
    If (bottles > 1) Then song$ = song$ + "s"
    song$ = song$ + " of beer on the wall, " + str$(bottles) + " bottle"
    If (bottles > 1) Then song$ = song$ + "s"
    song$ = song$ + " of beer,"  + chr$(13) + chr$(10) + "Take one down, pass it around, " + str$(bottles - 1) + " bottle"
    If (bottles > 2) Or (bottles = 1) Then song$ = song$ + "s"
    song$ = song$ + " of beer on the wall." + chr$(13) + chr$(10)
Next bottles
song$ = song$ + "No more bottles of beer on the wall, no more bottles of beer." _
        + chr$(13) + chr$(10) + "Go to the store and buy some more, 99 bottles of beer on the wall."

Print song$
```


=
## Microsoft Small Basic
=


```microsoftsmallbasic

For n = 99 To 1 Step -1
  If n = 1 Then
    bottleText1 = "1 bottle"
    bottleText2 = "No more bottles"
  ElseIf n = 2 then
    bottleText1 = "2 bottles"
    bottleText2 = "1 bottle"
  Else
    bottleText1 = n + " bottles"
    bottleText2 = n - 1 + " bottles"
  EndIf
  TextWindow.WriteLine(bottleText1 + " of beer on the wall")
  TextWindow.WriteLine(bottleText1 + " of beer")
  TextWindow.WriteLine("Take one down, pass it around")
  TextWindow.WriteLine(bottleText2 + " of beer on the wall")
  TextWindow.WriteLine("")
EndFor

```


=
## OxygenBasic
=

```qbasic

int    x=99
string cr,tab,pr,bottles,bottlem,remain
cr=chr(13) chr(10)
tab=chr(9)
pr="99 BOTTLES" cr cr
bottles=" bottles "
bottlem=" bottles "
'
for x=99 to 1 step -1
  if x=1
    bottles=" bottle "
    bottlem=" bottles "
    remain="No"
  elseif x=2
    bottlem=" bottle "
    remain=x-1
  else
    remain=x-1
  end if
  pr+=
  x bottles      "of beer on the wall" cr +
  x bottles      "of beer" cr +
                 "Take one down, pass it around" cr +
  remain bottlem "of beer on the wall" cr +
  cr
next
'
putfile "t.txt",pr

```


=
## PowerBASIC
=

```PowerBasic
#COMPILE EXE
#DIM ALL

FUNCTION PBMAIN () AS LONG

    DIM bottles%
    DIM b$
    DIM done$
    bottles% = 99
    b$ = " bottles "
    DO
        PRINT bottles%; b$; "of beer hanging on the wall"
        PRINT bottles%; b$; "of beer..."
        PRINT "And if one bottle of beer should accidentally be drunk"
        bottles% = bottles% - 1
        IF bottles% = 1 THEN
            b$ = " bottle "
        ELSEIF bottles% = 0 THEN
            b$ = " bottles "
        ELSE
            ' Press on!
        END IF
        PRINT "There'll be"; bottles; b$; "of beer hanging on the wall"
        PRINT
    LOOP UNTIL bottles% = 0


    CON.INPUT("Press any key to sleep it off",done$)

END FUNCTION
```


=
## PureBasic
=

### =Normal version=


```PureBasic
If OpenConsole()
  Define Bottles=99
  While Bottles
    PrintN(Str(Bottles)+" bottles of beer on the wall")
    PrintN(Str(Bottles)+" bottles of beer")
    PrintN("Take one down, pass it around")
    Bottles-1
    PrintN(Str(Bottles)+" bottles of beer on the wall"+#CRLF$)
  Wend

  PrintN(#CRLF$+#CRLF$+"Press ENTER to exit"):Input()
  CloseConsole()
EndIf
```


====An object-oriented solution====

```PureBasic
Prototype Wall_Action(*Self, Number.i)

Structure WallClass
  Inventory.i
  AddBottle.Wall_Action
  DrinkAndSing.Wall_Action
EndStructure

Procedure.s _B(n, Short=#False)
  Select n
    Case 0 : result$="No more bottles "
    Case 1 : result$=Str(n)+" bottle of beer"
    Default: result$=Str(n)+" bottles of beer"
  EndSelect
  If Not Short: result$+" on the wall": EndIf
  ProcedureReturn result$+#CRLF$
EndProcedure

Procedure PrintBottles(*Self.WallClass, n)
  Bottles$=" bottles of beer "
  Bottle$ =" bottle of beer "
  txt$ = _B(*Self\Inventory)
  txt$ + _B(*Self\Inventory, #True)
  txt$ + "Take one down, pass it around"+#CRLF$
  *Self\AddBottle(*Self, -1)
  txt$ + _B(*self\Inventory)
  PrintN(txt$)
  ProcedureReturn *Self\Inventory
EndProcedure

Procedure AddBottle(*Self.WallClass, n)
  i=*Self\Inventory+n
  If i>=0
    *Self\Inventory=i
  EndIf
EndProcedure

Procedure InitClass()
  *class.WallClass=AllocateMemory(SizeOf(WallClass))
  If *class
    InitializeStructure(*class, WallClass)
    With *class
      \AddBottle    =@AddBottle()
      \DrinkAndSing =@PrintBottles()
    EndWith
  EndIf
  ProcedureReturn *class
EndProcedure

If OpenConsole()
  *MyWall.WallClass=InitClass()
  If *MyWall
    *MyWall\AddBottle(*MyWall, 99)
    While *MyWall\DrinkAndSing(*MyWall, #True): Wend
    ;
    PrintN(#CRLF$+#CRLF$+"Press ENTER to exit"):Input()
    CloseConsole()
  EndIf
EndIf
```


=
## REALbasic
=
Place the following in the "open" event of a console application.

```vb
dim bottles as Integer = 99
While bottles > 0
   Print(str(bottles) + " bottles of beer on the wall")
   Print(str(bottles) + " bottles of beer")
   Print("Take one down, pass it around")
   bottles = bottles - 1
   Print(str(bottles) + " bottles of beer on the wall")
Wend
```


=
## Run BASIC
=

```Runbasic
b$ = " bottles"
for bottles = 99 To 1 Step -1
   If (bottles = 1) then b$ = " bottle"
   print  bottles;b$;" of beer on the wall, "
   print bottles ;b$;" of beer"
   print "Take one down, pass it around, "
   if bottles = 1 then
      print "No bottles of beer on the wall"
   else
      print bottles - 1;b$;" of beer on the wall.";chr$(10)
   end if
next bottles
```


=
## smart BASIC
=

```qbasic
READ b,s$(0),s$(1),a$,b$,c$,d$,e$,f$
g$ = CHR$(10)
WHILE b > 0
    IF b>1 THEN
        x=1
    ELSE
        x=0
    ENDIF
    IF b-1 <> 1 THEN
        y=1
    ELSE
        y=0
    ENDIF
PRINT b;a$;s$(x);b$;c$;b;a$;s$(x);b$;".";g$;d$;b-1;a$;s$(y);b$;c$;g$
b = b - 1
END WHILE
PRINT e$;a$;"s";b$;c$;e$;a$;"s";b$;".";g$;f$;a$;"s";b$;c$
DATA 99,"","s"," bottle"," of beer"," on the wall. ","Take one down, pass it around. "
DATA "No more","Go to the store and buy some more. 99"
END
```


==={{header|TI-83 BASIC}}===

```ti83b
PROGRAM:BEER
:For(I,99,1,-1)
:Disp I
:Disp "BOTTLES OF BEER"
:Disp "ON THE WALL,"
:Disp I
:Pause "BOTTLES OF BEER,"
:Disp "TAKE ONE DOWN,"
:Disp "PASS IT AROUND,"
:Disp I-1
:Disp "BOTTLES OF BEER"
:Disp "ON THE WALL."
:End

```



==={{header|TI-89 BASIC}}===

```ti89b
Prgm
  Local i,plural,clockWas,t,k,wait
  "s" → plural
  0 → k
  isClkOn() → clockWas

  Define wait() = Prgm
  EndPrgm

  ClockOn

  For i,99,0,–1
    Disp ""
    Disp string(i) & " bottle" & plural & " of beer on the"
    Disp "wall, " & string(i) & " bottle" & plural & " of beer."

    getTime()[3]→t
    While getTime()[3] = t and k = 0 : getKey() → k : EndWhile
    If k ≠ 0 Then : Exit : EndIf

    Disp "Take one down, pass it"
    Disp "around."

    getTime()[3]→t
    While getTime()[3] = t and k = 0 : getKey() → k : EndWhile
    If k ≠ 0 Then : Exit : EndIf

    If i - 1 = 1 Then
      "" → plural
    EndIf
    If i > 1 Then
        Disp string(i-1) & " bottle" & plural & " of beer on the"
        Disp "wall."
    Else
        Disp "No more bottles of beer on"
        Disp "the wall."
    EndIf

    getTime()[3]→t
    While abs(getTime()[3] - t)<2 and k = 0 : getKey() → k : EndWhile
    If k ≠ 0 Then : Exit : EndIf

  EndFor
  If not clockWas Then
    ClockOff
  ENdIf
EndPrgm
```


=
## True BASIC
=

```basic

! TrueBASIC v6.007

LET bottles = 99
LET b$ = " bottles "

DO
	PRINT bottles; b$; "of beer hanging on the wall"
	PRINT bottles; b$; "f beer hanging on the wall"
	PRINT "And if one bottle of beer should accidentally be drunk"
	LET bottles = bottles -1
	IF bottles = 1 THEN
		LET b$ = " bottle "
	ELSEIF bottles = 0 THEN
		LET b$ = " bottles "
	ELSE
		! Press on
	END IF
	PRINT "There'll be"; bottles; b$; "of beer hanging on the wall"
	PRINT
LOOP UNTIL bottles = 0

PRINT "Press any key to sleep it off"
GET KEY done
END

```


=
## Visual Basic
=

```vb
Sub Main()
    Const bottlesofbeer As String = " bottles of beer"
    Const onthewall As String = " on the wall"
    Const takeonedown As String = "Take one down, pass it around"
    Const onebeer As String = "1 bottle of beer"

    Dim bottles As Long

    For bottles = 99 To 3 Step -1
        Debug.Print CStr(bottles) & bottlesofbeer & onthewall
        Debug.Print CStr(bottles) & bottlesofbeer
        Debug.Print takeonedown
        Debug.Print CStr(bottles - 1) & bottlesofbeer & onthewall
        Debug.Print
    Next

    Debug.Print "2" & bottlesofbeer & onthewall
    Debug.Print "2" & bottlesofbeer
    Debug.Print takeonedown
    Debug.Print onebeer & onthewall
    Debug.Print

    Debug.Print onebeer & onthewall
    Debug.Print onebeer
    Debug.Print takeonedown
    Debug.Print "No more" & bottlesofbeer & onthewall
    Debug.Print

    Debug.Print "No" & bottlesofbeer & onthewall
    Debug.Print "No" & bottlesofbeer
    Debug.Print "Go to the store, buy some more"
    Debug.Print "99" & bottlesofbeer & onthewall
End Sub
```


=
## Visual Basic .NET
=
'''Platform:''' [[.NET]]


### =Classic=


```vbnet
Module Module1
   Sub Main()
       Dim Bottles As Integer
       For Bottles = 99 To 0 Step -1
           If Bottles = 0 Then
               Console.WriteLine("No more bottles of beer on the wall, no more bottles of beer.")
               Console.WriteLine("Go to the store and buy some more, 99 bottles of beer on the wall.")
               Console.ReadLine()
           ElseIf Bottles = 1 Then
               Console.WriteLine(Bottles & " bottle of beer on the wall, " & Bottles & " bottle of beer.")
               Console.WriteLine("Take one down and pass it around, no more bottles of beer on the wall.")
               Console.ReadLine()
           Else
               Console.WriteLine(Bottles & " bottles of beer on the wall, " & Bottles & " bottles of beer.")
               Console.WriteLine("Take one down and pass it around, " & (Bottles - 1) & " bottles of beer on the wall.")
               Console.ReadLine()
           End If
       Next
   End Sub
End Module
```



### =String interpolation=


```vbnet
Module Program
    Function Plural(count As Integer) As String
        Return If(count = 1, "", "s")
    End Function

    Sub Main()
        For i = 99 To 1 Step -1
            Console.WriteLine($"{i} bottle{Plural(i)} of beer on the wall")
            Console.WriteLine($"{i} bottle{Plural(i)} of beer")
            Console.WriteLine($"Take one down, pass it around")
            Console.WriteLine($"{i - 1} bottle{Plural(i - 1)} of beer on the wall")
            Console.WriteLine()
        Next
    End Sub
End Module
```



### =Mental mutilation beyond hope of regeneration=


```vbnet
Option Explicit Off
Option Strict Off

Module Program
    Sub Main
        i = 99
START:
        Console.Write(i)
        Console.Write(" bottle")
        If i > 1 Then Console.Write("s")
        Console.Write(" of beer on the wall")
        Console.WriteLine()
        Console.Write(i)
        Console.Write(" bottle")
        If i > 1 Then Console.Write("s")
        Console.Write(" of beer")
        Console.WriteLine()
        Console.Write($"Take one down, pass it around")
        Console.WriteLine()
        Console.Write(i - 1)
        Console.Write(" bottle")
        If i - 1 > 1 Then Console.Write("s")
        Console.Write(" of beer on the wall")
        Console.WriteLine()
        Console.WriteLine()
        i -= 1
        If i > 0 Then GoTo START
    End Sub
End Module
```



### =Python=


```vbnet
Option Strict Off

Module Program
    Sub Main()
        Dim a=" bottles of beer",b=" on the wall"&vbLf,c="Take one down, pass it around"&vbLf,s=Function(o)o.ToString
        Console.Write(String.Join(vbLf,Enumerable.Range(1,99).Reverse.Select(Function(x)s(x)+a+b+s(x)+a+vbLf+c+s(x-1)+a+b)))
    End Sub
End Module
```



### =With sound=

'''Compiler:''' Roslyn Visual Basic (language version >= 15.3)

'''Platform:''' Windows, versions other than 64-bit Vista and XP, and either with a 8254 chip or sound card installed.

Plays tones synchronized with output syllable-by-syllable. Uses word forms of numbers, but can be made to display the numbers (while still playing the appropriate number of notes as per the word form).

Uses the Windows beep function, which on modern PCs should just use the speakers. Volume depends on system volume.

Parses command-line arguments (no validation) (syntax: <name>:<value>, space-separated and optionally enclosed in quotes):
* bpm (default 120): beats-per-minute. Beep seems to lag when called frequently, resulting in desynchronized sound and dropped notes
* oct (default 3): (MIDI) octave to play notes in.
* words (default true): Whether to display bottle count as words; set to false to get task-compliant output

Could use some tidying-up of magic numbers.

Options and imports statements (all parts must be in one file):

```vbnet
Option Explicit On
Option Infer On
Option Strict On

Imports System.Globalization
Imports System.Runtime.InteropServices
Imports System.Threading
```


Helper to parse command-line arguments:

```vbnet

Module ArgHelper
    ReadOnly _ArgDict As New Dictionary(Of String, String)()

    Delegate Function TryParse(Of T, TResult)(value As T, <Out> ByRef result As TResult) As Boolean

    Sub InitializeArguments(args As String())
        For Each item In args
            item = item.ToUpperInvariant()

            If item.Length > 0 AndAlso item(0) <> """"c Then
                Dim colonPos = item.IndexOf(":"c, StringComparison.Ordinal)

                If colonPos <> -1 Then
                    ' Split arguments with colons into key(part before colon) / value(part after colon) pairs.
                    _ArgDict.Add(item.Substring(0, colonPos), item.Substring(colonPos + 1, item.Length - colonPos - 1))
                End If
            End If
        Next
    End Sub

    Sub FromArgument(Of T)(
            key As String,
            <Out> ByRef var As T,
            getDefault As Func(Of T),
            tryParse As TryParse(Of String, T),
            Optional validate As Predicate(Of T) = Nothing)

        Dim value As String = Nothing
        If _ArgDict.TryGetValue(key.ToUpperInvariant(), value) Then
            If Not (tryParse(value, var) AndAlso (validate Is Nothing OrElse validate(var))) Then
                Console.WriteLine($"Invalid value for {key}: {value}")
                Environment.Exit(-1)
            End If
        Else
            var = getDefault()
        End If
    End Sub
End Module
```


Program:

```vbnet
Module Program
    Function GetNumberSyllables(n As Integer) As IEnumerable(Of String)
        Static getTensPrefix As Func(Of Integer, IEnumerable(Of String)) =
           Iterator Function(_n As Integer) As IEnumerable(Of String)
               Select Case _n
                   Case 20 : Yield "twen"
                   Case 30 : Yield "thir"
                   Case 40 : Yield "for"
                   Case 50 : Yield "fif"
                   Case 60 : Yield "six"
                   Case 70 : Yield "se" : Yield "ven"
                   Case 80 : Yield "eigh"
                   Case 90 : Yield "nine"
                   Case Else
                       Throw New ArgumentOutOfRangeException(NameOf(_n), _n, "")
               End Select
           End Function

        Static getSmallNumber As Func(Of Integer, IEnumerable(Of String)) =
            Iterator Function(_n As Integer) As IEnumerable(Of String)
                Select Case _n
                    Case 0 : Yield "ze" : Yield "ro"
                    Case 1 : Yield "one"
                    Case 2 : Yield "two"
                    Case 3 : Yield "three"
                    Case 4 : Yield "four"
                    Case 5 : Yield "five"
                    Case 6 : Yield "six"
                    Case 7 : Yield "se" : Yield "ven"
                    Case 8 : Yield "eight"
                    Case 9 : Yield "nine"
                    Case 10 : Yield "ten"
                    Case 11 : Yield "e" : Yield "le" : Yield "ven"
                    Case 12 : Yield "twelve"
                    Case 13 : Yield "thir" : Yield "teen"
                    Case 14 : Yield "four" : Yield "teen"
                    Case 15 To 19
                        For Each s In getTensPrefix((_n - 10) * 10)
                            Yield s
                        Next
                        Yield "teen"
                    Case Else
                        Throw New ArgumentOutOfRangeException(NameOf(_n), _n, "")
                End Select
            End Function

        Select Case n
            Case 0 To 19
                Return getSmallNumber(n)
            Case 20 To 99
                Dim tens = (n \ 10) * 10
                Dim ones = n Mod 10

                Dim tensSyllables = getTensPrefix(tens).Append("ty")
                If ones = 0 Then Return tensSyllables

                Dim onesSyllables = getSmallNumber(ones)
                Return tensSyllables.
                    Append("-" & onesSyllables.First()).
                    Concat(onesSyllables.Skip(1))
            Case Else
                Throw New ArgumentOutOfRangeException(NameOf(n), n, "")
        End Select
    End Function

    Iterator Function GetLyrics(numbersToWords As Boolean) As IEnumerable(Of (text As String, midiNote As Integer, noteType As Double))
        Dim getSyllablesWordsOrNumber =
            Function(n As Integer) As IEnumerable(Of String)
                Dim syllables = GetNumberSyllables(n)
                If numbersToWords Then
                    Dim firstWordTitleCase = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(syllables.First())
                    Return syllables.Skip(1).Prepend(firstWordTitleCase)
                Else
                    Return syllables.Skip(1).Select(Function(s) "").Prepend(n.ToString(CultureInfo.CurrentCulture))
                End If
            End Function

        Dim number, nextNumber As IEnumerable(Of String)
        nextNumber = getSyllablesWordsOrNumber(99)
        For i = 99 To 1 Step -1
            number = nextNumber
            nextNumber = getSyllablesWordsOrNumber(i - 1)

            For Each syl In number
                Yield (syl, 67, 4)
            Next
            Yield (" bo", 62, 4)
            Yield ("ttle" & Plural(i), 62, 4)
            Yield (" of", 62, 4)
            Yield (" beer", 67, 4)
            Yield (" on", 67, 4)
            Yield (" the", 67, 4)
            Yield (" wall", 67, 4 / 3)

            Yield (vbLf, -1, 0)
            For Each syl In number
                Yield (syl, 69, 4)
            Next
            Yield (" bo", 64, 4)
            Yield ("ttle" & Plural(i), 64, 4)
            Yield (" of", 64, 4)
            Yield (" beer", 69, 1)

            Yield (vbLf, -1, 0)
            Yield ("Take", 66, 2)
            Yield (" one", 66, 4)
            Yield (" down,", 66, 4 / 3)
            Yield (" pass", 66, 4)
            Yield (" it", 66, 4)
            Yield (" a", 66, 4)
            Yield ("round", 66, 4 / 3)

            Yield (vbLf, -1, 0)
            For Each syl In nextNumber
                Yield (syl, 62, 4)
            Next
            Yield (" bo", 64, 4)
            Yield ("ttle" & Plural(i - 1), 64, 4)
            Yield (" of", 66, 4)
            Yield (" beer", 67, 4)
            Yield (" on", 67, 4)
            Yield (" the", 67, 4)
            Yield (" wall" & vbLf, 67, 4 / 3)

            Yield (vbLf, -1, 0)
        Next
    End Function

    ' Note 69 = A440
    Function MidiNoteToFreq(midiNote As Integer) As Integer
        Const REF_NOTE = 69
        Const REF_FREQ = 440
        Return CInt(REF_FREQ * (2 ^ ((midiNote - REF_NOTE) / 12)))
    End Function

    'e.g. whole=1, half=2, quarter=4
    Function NoteToMilliseconds(noteType As Double, bpm As Integer) As Integer
        If noteType = 0 Then Return 0
        Const SECS_PER_MIN As Integer = 60
        Const MS_PER_SEC As Integer = 1000
        Const QT_NOTE_VAL As Integer = 4
        Return CInt(SECS_PER_MIN * MS_PER_SEC * QT_NOTE_VAL / (bpm * noteType))
    End Function

    Sub PlayNote(midiNote As Integer, millisecondsDuration As Integer)
        If millisecondsDuration < 1 Then Return

        If midiNote > 0 Then
            Console.Beep(MidiNoteToFreq(midiNote), millisecondsDuration)
        Else
            Thread.Sleep(millisecondsDuration)
        End If
    End Sub

    Sub Play(Optional bpm As Integer = 120, Optional octave As Integer = 3, Optional numbersToWords As Boolean = True, Optional cancellationToken As CancellationToken = Nothing)
        For Each syl In GetLyrics(numbersToWords)
            cancellationToken.ThrowIfCancellationRequested()
            Console.Write(syl.text)
            PlayNote(syl.midiNote + (12 * (octave - 3)), NoteToMilliseconds(syl.noteType, bpm))
        Next
    End Sub

    Sub Main(args As String())
        Dim bpm As Integer
        Dim octave As Integer
        Dim numbersToWords As Boolean

        InitializeArguments(args)
        FromArgument("bpm", bpm, Function() 120, AddressOf Integer.TryParse)
        FromArgument("oct", octave, Function() 3, AddressOf Integer.TryParse)
        FromArgument("words", numbersToWords, Function() True, AddressOf Boolean.TryParse)

        Using cts As New CancellationTokenSource()
            Dim t = Task.Run(Sub() Play(cancellationToken:=cts.Token,
                                        bpm:=bpm,
                                        octave:=octave,
                                        numbersToWords:=numbersToWords), cts.Token)

            Do
                Thread.Yield()
            Loop Until t.IsCompleted OrElse Console.KeyAvailable

            If Not t.IsCompleted Then
                Console.WriteLine("STOPPING")
                Try
                    cts.Cancel()
                    t.Wait()
                Catch ex As Exception
                End Try
                Console.WriteLine("STOPPED")
            End If
        End Using
    End Sub
End Module

```


{{out}}

```txt
Ninety-nine bottles of beer on the wall
Ninety-nine bottles of beer
Take one down, pass it around
Ninety-eight bottles of beer on the wall

Ninety-eight bottles of beer on the wall
Ninety-eight bottles of beer
Take one down, pass it around
Ninety-seven bottles of beer on the wall

...

One bottle of beer on the wall
One bottle of beer
Take one down, pass it around
Zero bottles of beer on the wall
```


{{out|input=words:false}}

```txt
99 bottles of beer on the wall
99 bottles of beer
Take one down, pass it around
98 bottles of beer on the wall

98 bottles of beer on the wall
98 bottles of beer
Take one down, pass it around
97 bottles of beer on the wall

...

1 bottle of beer on the wall
1 bottle of beer
Take one down, pass it around
0 bottles of beer on the wall
```



### =Concurrent=


```vbnet
Module Program
    Function Plural(count As Integer) As String
        Return If(count = 1, "", "s")
    End Function

    Sub Main()
        Parallel.For(0, 99,
            Sub(i)
                i = 99 - i
                Console.WriteLine($"{i} bottle{Plural(i)} of beer on the wall")
                Console.WriteLine($"{i} bottle{Plural(i)} of beer")
                Console.WriteLine($"Take one down, pass it around")
                Console.WriteLine($"{i - 1} bottle{Plural(i - 1)} of beer on the wall")
                Console.WriteLine()
            End Sub)
    End Sub
End Module
```


{{out}}

```txt
99 bottles of beer on the wall
27 bottles of beer on the wall
51 bottles of beer on the wall
51 bottles of beer
Take one down, pass it around
50 bottles of beer on the wall
27 bottles of beer
Take one down, pass it around
26 bottles of beer on the wall
99 bottles of beer
Take one down, pass it around
98 bottles of beer on the wall


26 bottles of beer on the wall
26 bottles of beer
Take one down, pass it around
25 bottles of beer on the wall
98 bottles of beer on the wall
98 bottles of beer
Take one down, pass it around
97 bottles of beer on the wall

...
```



### =Less concurrent=


```vbnet
Module Program
    Function Plural(count As Integer) As String
        Return If(count = 1, "", "s")
    End Function

    Sub Main()
        Dim lockobj As New Object()
        Parallel.For(0, 99,
            Sub(i)
                i = 99 - i
                SyncLock lockobj
                    Console.WriteLine($"{i} bottle{Plural(i)} of beer on the wall")
                    Console.WriteLine($"{i} bottle{Plural(i)} of beer")
                    Console.WriteLine($"Take one down, pass it around")
                    Console.WriteLine($"{i - 1} bottle{Plural(i - 1)} of beer on the wall")
                    Console.WriteLine()
                End SyncLock
            End Sub)
    End Sub
End Module
```


{{out}}

```txt
99 bottles of beer on the wall
99 bottles of beer
Take one down, pass it around
98 bottles of beer on the wall

51 bottles of beer on the wall
51 bottles of beer
Take one down, pass it around
50 bottles of beer on the wall

50 bottles of beer on the wall
50 bottles of beer
Take one down, pass it around
49 bottles of beer on the wall

49 bottles of beer on the wall
49 bottles of beer
Take one down, pass it around
48 bottles of beer on the wall

...
```



### =<s>More</s>D<s> </s>R<s>con</s>U<s>current</s>NK=


```vbnet
Module Program
    Function Plural(count As Integer) As String
        Return If(count = 1, "", "s")
    End Function

    Sub Main()
        Dim WriteLine =
        Sub(s As String)
            For Each c In s
                Threading.Thread.Sleep(0)
                Console.Write(c)
            Next
            Console.WriteLine()
        End Sub

        Parallel.For(0, 99,
            Sub(i)
                i = 99 - i
                WriteLine($"{i} bottle{Plural(i)} of beer on the wall")
                WriteLine($"{i} bottle{Plural(i)} of beer")
                WriteLine($"Take one down, pass it around")
                WriteLine($"{i - 1} bottle{Plural(i - 1)} of beer on the wall")
                WriteLine("")
            End Sub)
    End Sub
End Module
```


{{out}}

```txt
9235797 b5  1  bbobbootoottttltttles oftles of beer on the wall beer on the waes of beer on the wtlesallles of beer
 of beer on 3 bottles of beon the wall
er
Take one down, pass it around
2 bottles of beer on the wall

99 bottles of beer
Take one down, pass it arotl
2uhl
 ne72bd
57 o 9 btw8botalotl l
tebttlso 5leot1esft s  lb obeoofestf e t brolbe feeeo ser
b onTer
 akfTtee ahr bke oeeer
ne oT one down, pass it around
wall
n t72a he bottles odown, pass it around
4 bo26 bottles of beerkft we t a ololbnenl

s te 9eh o8eedf r
o beTwn,er on the wall

 pass it arouake one dow74 bottles of beer on the wall
 wall

bottles of beer on the wall
98 bottles of beer
Takend
n, pass it around
26 bottles of beer on 50 bottles of beer on the wall

t 175ho 40enb   we down, pass it all
aobotround
b26 bottles of beer
Tottles of beer on the tles of beer
t97 botwTtataalkllkeeel
  oe osn 5oneo0fe f   d bbdoboeowetewnntr,e,le spr   a poosoan the wall

sf beer
1s it arn the wall

ound
T bottle of beer on the wall
as it around
k97 bottles of beer on the wall
125 bottles of beer on th bottle of beer
ee one down, pass it around
T wall

4ake one down, pass it around
797 bottles of beer
T325 bottles of beer a 9bottl0 bottles of beeron thk es of beer on the wall

b on the wall

...
```


=
## XBasic
=
{{trans|Microsoft Small Basic}}
{{works with|Windows XBasic}}

```xbasic

PROGRAM "99bottles"
VERSION "0.0001"

DECLARE FUNCTION Entry()

FUNCTION Entry()
  FOR n@@ = 99 TO 1 STEP -1
    SELECT CASE TRUE
      CASE n@@ = 1:
        bottleText1$ = "1 bottle"
        bottleText2$ = "No more bottles"
      CASE n@@ = 2:
        bottleText1$ = "2 bottles"
        bottleText2$ = "1 bottle"
      CASE ELSE:
        bottleText1$ = LTRIM$(STR$(n@@)) + " bottles"
        bottleText2$ = LTRIM$(STR$(n@@ - 1)) + " bottles"
    END SELECT
    PRINT bottleText1$; " of beer on the wall"
    PRINT bottleText1$; " of beer"
    PRINT "Take one down, pass it around"
    PRINT bottleText2$; " of beer on the wall"
    PRINT
  NEXT n@@
END FUNCTION
END PROGRAM

```


=
## ZX Spectrum Basic
=

```zxbasic
10 DEF FN n$(b)="no more" AND NOT b
20 DEF FN m$(b)=(STR$ b) AND b
30 DEF FN o$(b)="s" AND (b>1)
40 DEF FN b$(b)=FN m$(b)+FN n$(b)
50 DEF FN p$(b)=FN o$(b)+("s" AND NOT b)
60 FOR i=99 TO 0 STEP -1
70 LET x$=FN b$(i)
80 IF x$(1)="n" THEN LET x$(1)="N"
90 PRINT x$;
100 PRINT " bottle";FN o$(i);" of beer on the wall, "'FN b$(i);" bottle";FN p$(i);" of beer."
110 IF NOT i THEN GO TO 160
120 PRINT "Take one down"'"and pass it around, ";
130 PRINT FN b$(i-1);" bottle";FN p$(i-1);" of beer on the wall."
140 PRINT
150 NEXT i
160 PRINT "Go to the store and buy some more, 99 bottles of beer on the wall."

```



## VBA

This version uses tail recursion and inline if-statements,
plus a Static variable to count the number of bottles emptied.


```vb
Public Function countbottles(n As Integer, liquid As String) As String
  countbottles = IIf(n > 1, Format$(n), IIf(n = 0, "no more", "one")) & " bottle" & IIf(n = 1, "", "s") & " of " & liquid
End Function

Public Sub drink(fullbottles As Integer, Optional liquid As String = "beer")
Static emptybottles As Integer

  Debug.Print countbottles(fullbottles, liquid) & " on the wall"
  Debug.Print countbottles(fullbottles, liquid)

  If fullbottles > 0 Then
    Debug.Print "take " & IIf(fullbottles > 1, "one", "it") & " down, pass it around"
    Debug.Print countbottles(fullbottles - 1, liquid) & " on the wall"
    Debug.Print
    emptybottles = emptybottles + 1
    drink fullbottles - 1, liquid
  Else
    Debug.Print "go to the store and buy some more"
    Debug.Print countbottles(emptybottles, liquid) & " on the wall"
  End If

End Sub
```


Usage: type "drink 99" in the Immediate window of the VBA editor. If you're not a beer drinker, you can specify your own favourite drink as the second argument; for example:


```txt

drink 3, "Johnnie Walker"

3 bottles of Johnnie Walker on the wall
3 bottles of Johnnie Walker
take one down, pass it around
2 bottles of Johnnie Walker on the wall

2 bottles of Johnnie Walker on the wall
2 bottles of Johnnie Walker
take one down, pass it around
one bottle of Johnnie Walker on the wall

one bottle of Johnnie Walker on the wall
one bottle of Johnnie Walker
take it down, pass it around
no more bottles of Johnnie Walker on the wall

no more bottles of Johnnie Walker on the wall
no more bottles of Johnnie Walker
go to the store and buy some more
3 bottles of Johnnie Walker on the wall


```



## VBScript


### Simple Method


```vb
sub song( numBottles )
	dim i
	for i = numBottles to 0 step -1
		if i > 0 then
			wscript.echo pluralBottles(i) & " of beer on the wall"
			wscript.echo pluralBottles(i) & " of beer"
			if i = 1 then
				wscript.echo "take it down"
			else
				wscript.echo "take one down"
			end if
			wscript.echo "and pass it round"
			wscript.echo pluralBottles(i-1) & " of beer on the wall"
			wscript.echo
		else
			wscript.echo "no more bottles of beer on the wall"
			wscript.echo "no more bottles of beer"
			wscript.echo "go to the store"
			wscript.echo "and buy some more"
			wscript.echo pluralBottles(numBottles) & " of beer on the wall"
			wscript.echo
		end if
	next
end sub

function pluralBottles( n )
	select case n
	case 1
		pluralBottles = "one bottle"
	case 0
		pluralBottles = "no more bottles"
	case else
		pluralBottles = n & " bottles"
	end select
end function

song 3
```

{{Out}}

```txt
3 bottles of beer on the wall
3 bottles of beer
take one down
and pass it round
2 bottles of beer on the wall

2 bottles of beer on the wall
2 bottles of beer
take one down
and pass it round
one bottle of beer on the wall

one bottle of beer on the wall
one bottle of beer
take it down
and pass it round
no more bottles of beer on the wall

no more bottles of beer on the wall
no more bottles of beer
go to the store
and buy some more
3 bottles of beer on the wall
```



### Regular Expressions and Embedded Scripting

Another way of doing it, using Regular Expressions
to locate executable code inside {}
and replacing the code with the result of its evaluation.


```vb
function pluralBottles( n )
	select case n
	case 1
		pluralBottles = "one bottle"
	case 0
		pluralBottles = "no more bottles"
	case else
		pluralBottles = n & " bottles"
	end select
end function

function eef( b, r1, r2 )
	if b then
		eef = r1
	else
		eef = r2
	end if
end function

Function evalEmbedded(sInput, sP1)
	dim oRe, oMatch, oMatches
	dim sExpr, sResult
	Set oRe = New RegExp
	'Look for expressions as enclosed in braces
	oRe.Pattern = "{(.+?)}"
	sResult = sInput
	do
		Set oMatches = oRe.Execute(sResult)
		if oMatches.count = 0 then exit do
		for each oMatch in oMatches
			'~ wscript.echo oMatch.Value
			for j = 0 to omatch.submatches.count - 1
				sExpr = omatch.submatches(j)
				sResult  = Replace( sResult, "{" & sExpr & "}", eval(sExpr) )
			next
		next
	loop
	evalEmbedded = sResult
End Function

sub sing( numBottles )
	dim i
	for i = numBottles to 0 step -1
		if i = 0 then
			wscript.echo evalEmbedded("no more bottles of beer on the wall" & vbNewline & _
						"no more bottles of beer" & vbNewline & _
						"go to the store and buy some more" & vbNewline & _
						"{pluralBottles(sP1)} of beer on the wall" & vbNewline, numBottles)
		else
			wscript.echo evalEmbedded("{pluralBottles(sP1)} of beer on the wall" & vbNewline & _
					      "{pluralBottles(sP1)} of beer" & vbNewline & _
					      "take {eef(sP1=1,""it"",""one"")} down and pass it round" & vbNewline & _
					      "{pluralBottles(sP1-1)} of beer on the wall" & vbNewline, i)
		end if
	next
end sub

sing 3
```

