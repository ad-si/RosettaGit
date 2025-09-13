+++
title = "Multiplication tables"
description = ""
date = 2019-10-21T06:08:44Z
aliases = []
[extra]
id = 5177
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
+++

## Task

Produce a formatted   12&times;12   multiplication table of the kind memorized by rote when in primary (or elementary) school.


Only print the top half triangle of products.




## 360 Assembly


```360asm
*        12*12 multiplication table    14/08/2015
MULTTABL CSECT
         USING  MULTTABL,R12
         LR     R12,R15
         LA     R10,0              buffer pointer
         LA     R3,BUFFER
         MVC    0(4,R3),=C'  | '
         LA     R10,4(R10)
         LA     R5,12
         LA     R4,1               i=1
LOOPN    LA     R3,BUFFER          do i=1 to 12
         AR     R3,R10
         XDECO  R4,XDEC            i
         MVC    0(4,R3),XDEC+8     output i
         LA     R10,4(R10)
         LA     R4,1(R4)
         BCT    R5,LOOPN           end i
         XPRNT  BUFFER,52
         XPRNT  PORT,52            border
         LA     R5,12
         LA     R4,1               i=1 (R4)
LOOPI    LA     R10,0              do i=1 to 12
         MVC    BUFFER,=CL52' '
         LA     R3,BUFFER
         AR     R3,R10
         XDECO  R4,XDEC
         MVC    0(2,R3),XDEC+10
         LA     R10,2(R10)
         LA     R3,BUFFER
         AR     R3,R10
         MVC    0(2,R3),=C'| '
         LA     R10,2(R10)
         LA     R7,12
         LA     R6,1               j=1 (R6)
LOOPJ    CR     R6,R4              do j=1 to 12
         BNL    MULT
         LA     R3,BUFFER
         AR     R3,R10
         MVC    0(4,R3),=C'    '
         LA     R10,4(R10)
         B      NEXTJ
MULT     LR     R9,R4              i
         MR     R8,R6              i*j in R8R9
         LA     R3,BUFFER
         AR     R3,R10
         XDECO  R9,XDEC
         MVC    0(4,R3),XDEC+8
         LA     R10,4(R10)
NEXTJ    LA     R6,1(R6)
         BCT    R7,LOOPJ           end j
ELOOPJ   XPRNT  BUFFER,52
         LA     R4,1(R4)
         BCT    R5,LOOPI           end i
ELOOPI   XR     R15,R15
         BR     R14
BUFFER   DC     CL52' '
XDEC     DS     CL12
PORT     DC     C'--+-------------------------------------------------'
         YREGS
         END    MULTTABL
```

```txt
  |    1   2   3   4   5   6   7   8   9  10  11  12
--+-------------------------------------------------
 1|    1   2   3   4   5   6   7   8   9  10  11  12
 2|        4   6   8  10  12  14  16  18  20  22  24
 3|            9  12  15  18  21  24  27  30  33  36
 4|               16  20  24  28  32  36  40  44  48
 5|                   25  30  35  40  45  50  55  60
 6|                       36  42  48  54  60  66  72
 7|                           49  56  63  70  77  84
 8|                               64  72  80  88  96
 9|                                   81  90  99 108
10|                                      100 110 120
11|                                          121 132
12|                                              144
```



## ActionScript


```ActionScript

package {

    import flash.display.Sprite;
    import flash.events.Event;
    import flash.text.TextField;
    import flash.text.TextFieldAutoSize;
    import flash.text.TextFormat;

    [SWF (width = 550, height = 550)]
    public class MultiplicationTable extends Sprite {

        public function MultiplicationTable() {
            if ( stage ) _init();
            else addEventListener(Event.ADDED_TO_STAGE, _init);
        }

        private function _init(e:Event = null):void {

            removeEventListener(Event.ADDED_TO_STAGE, _init);

            var format:TextFormat = new TextFormat();
            format.size = 15;
            var blockSize:uint = 40;
            var max:uint = 12;

            var i:uint, j:uint;
            var tf:TextField;

            for ( i = 1; i <= max; i++ ) {
                tf = new TextField();
                tf.defaultTextFormat = format;
                tf.x = blockSize * i;
                tf.y = 0;
                tf.width = tf.height = blockSize;
                tf.autoSize = TextFieldAutoSize.CENTER;
                tf.text = String(i);
                addChild(tf);

                tf = new TextField();
                tf.defaultTextFormat = format;
                tf.x = 0;
                tf.y = blockSize * i;
                tf.width = tf.height = blockSize;
                tf.autoSize = TextFieldAutoSize.CENTER;
                tf.text = String(i);
                addChild(tf);
            }

            var yOffset:Number = tf.textHeight / 2;
            y += yOffset;

            graphics.lineStyle(1, 0x000000);
            graphics.moveTo(blockSize, -yOffset);
            graphics.lineTo(blockSize, (blockSize * (max + 1)) - yOffset);
            graphics.moveTo(0, blockSize - yOffset);
            graphics.lineTo(blockSize * (max + 1), blockSize - yOffset);


            for ( i = 1; i <= max; i++ ) {
                for ( j = 1; j <= max; j++ ) {
                    if ( j > i )
                        continue;

                    tf = new TextField();
                    tf.defaultTextFormat = format;
                    tf.x = blockSize * i;
                    tf.y = blockSize * j;
                    tf.width = tf.height = blockSize;
                    tf.autoSize = TextFieldAutoSize.CENTER;
                    tf.text = String(i * j);
                    addChild(tf);
                }
            }

        }

    }

}

```



## Ada


```Ada

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;  use Ada.Strings.Fixed;
procedure Multiplication_Table is
   package IO is new Integer_IO (Integer);
   use IO;
begin
   Put ("  | ");
   for Row in 1..12 loop
      Put (Row, Width => 4);
   end loop;
   New_Line;
   Put_Line ("--+-" & 12 * 4 * '-');
   for Row in 1..12 loop
      Put (Row, Width => 2);
      Put ("| ");
      for Column in 1..12 loop
         if Column < Row then
            Put ("    ");
         else
            Put (Row * Column, Width => 4);
         end if;
      end loop;
      New_Line;
   end loop;
end Multiplication_Table;

```


```txt

  |    1   2   3   4   5   6   7   8   9  10  11  12
--+-------------------------------------------------
 1|    1   2   3   4   5   6   7   8   9  10  11  12
 2|        4   6   8  10  12  14  16  18  20  22  24
 3|            9  12  15  18  21  24  27  30  33  36
 4|               16  20  24  28  32  36  40  44  48
 5|                   25  30  35  40  45  50  55  60
 6|                       36  42  48  54  60  66  72
 7|                           49  56  63  70  77  84
 8|                               64  72  80  88  96
 9|                                   81  90  99 108
10|                                      100 110 120
11|                                          121 132
12|                                              144

```



## Agena

```agena
scope
    # print a school style multiplication table
    # NB: print outputs a newline at the end, write and printf do not
    write( "    " );
    for i to 12 do printf( " %3d", i ) od;
    printf( "\n   +" );
    for i to 12 do write( "----" ) od;
    for i to 12 do
        printf( "\n%3d|", i );
        for j        to i - 1 do write(  "    "        ) od;
        for j from i to 12    do printf( " %3d", i * j ) od;
    od;
    print()
epocs
```

```txt

       1   2   3   4   5   6   7   8   9  10  11  12
   +------------------------------------------------
  1|   1   2   3   4   5   6   7   8   9  10  11  12
  2|       4   6   8  10  12  14  16  18  20  22  24
  3|           9  12  15  18  21  24  27  30  33  36
  4|              16  20  24  28  32  36  40  44  48
  5|                  25  30  35  40  45  50  55  60
  6|                      36  42  48  54  60  66  72
  7|                          49  56  63  70  77  84
  8|                              64  72  80  88  96
  9|                                  81  90  99 108
 10|                                     100 110 120
 11|                                         121 132
 12|                                             144

```



## ALGOL 68

<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 - missing printf and FORMAT}} -->

```Algol68
main:(
  INT max = 12;
  INT width = ENTIER(log(max)*2)+1;
  STRING empty = " "*width, sep="|", hr = "+" + (max+1)*(width*"-"+"+");
  FORMAT ifmt = $g(-width)"|"$; # remove leading zeros #

  printf(($gl$, hr));
  print(sep + IF width<2 THEN "x" ELSE " "*(width-2)+"x " FI + sep);
  FOR col TO max DO printf((ifmt, col)) OD;
  printf(($lgl$, hr));

  FOR row TO max DO
    [row:max]INT product;
    FOR col FROM row TO max DO product[col]:=row*col OD;
    STRING prefix=(empty+sep)*(row-1);
    printf(($g$, sep, ifmt, row, $g$, prefix, ifmt, product, $l$))
  OD;
  printf(($gl$, hr))
)
```

```txt

+---+---+---+---+---+---+---+---+---+---+---+---+---+
| x |  1|  2|  3|  4|  5|  6|  7|  8|  9| 10| 11| 12|
+---+---+---+---+---+---+---+---+---+---+---+---+---+
|  1|  1|  2|  3|  4|  5|  6|  7|  8|  9| 10| 11| 12|
|  2|   |  4|  6|  8| 10| 12| 14| 16| 18| 20| 22| 24|
|  3|   |   |  9| 12| 15| 18| 21| 24| 27| 30| 33| 36|
|  4|   |   |   | 16| 20| 24| 28| 32| 36| 40| 44| 48|
|  5|   |   |   |   | 25| 30| 35| 40| 45| 50| 55| 60|
|  6|   |   |   |   |   | 36| 42| 48| 54| 60| 66| 72|
|  7|   |   |   |   |   |   | 49| 56| 63| 70| 77| 84|
|  8|   |   |   |   |   |   |   | 64| 72| 80| 88| 96|
|  9|   |   |   |   |   |   |   |   | 81| 90| 99|108|
| 10|   |   |   |   |   |   |   |   |   |100|110|120|
| 11|   |   |   |   |   |   |   |   |   |   |121|132|
| 12|   |   |   |   |   |   |   |   |   |   |   |144|
+---+---+---+---+---+---+---+---+---+---+---+---+---+

```



## ALGOL W


```algolw
begin
    % print a school style multiplication table                              %
    i_w := 3; s_w := 0; % set output formating                               %
    write( "    " );
    for i := 1 until 12 do writeon( " ", i );
    write( "   +" );
    for i := 1 until 12 do writeon( "----" );
    for i := 1 until 12 do begin
        write( i, "|" );
        for j := 1 until i - 1 do writeon( "    " );
        for j := i until 12    do writeon( " ", i * j );
    end;

end.
```

```txt

       1   2   3   4   5   6   7   8   9  10  11  12
   +------------------------------------------------
  1|   1   2   3   4   5   6   7   8   9  10  11  12
  2|       4   6   8  10  12  14  16  18  20  22  24
  3|           9  12  15  18  21  24  27  30  33  36
  4|              16  20  24  28  32  36  40  44  48
  5|                  25  30  35  40  45  50  55  60
  6|                      36  42  48  54  60  66  72
  7|                          49  56  63  70  77  84
  8|                              64  72  80  88  96
  9|                                  81  90  99 108
 10|                                     100 110 120
 11|                                         121 132
 12|                                             144

```



## AppleScript


### Iteration


```AppleScript
set n to 12 -- Size of table.
repeat with x from 0 to n
	if x = 0 then set {table, x} to {{return}, -1}
	repeat with y from 0 to n
		if y's contents = 0 then
			if x > 0 then set row to {f(x)}
			if x = -1 then set {row, x} to {{f("x")}, 1}
		else
			if y ≥ x then set end of row to f(x * y)
			if y < x then set end of row to f("")
		end if
	end repeat
	set end of table to row & return
end repeat
return table as string

-- Handler/Function for formatting fixed width integer string.
on f(x)
	set text item delimiters to ""
	return (characters -4 thru -1 of ("    " & x)) as string
end f
```

```txt
"
   x   1   2   3   4   5   6   7   8   9  10  11  12
   1   1   2   3   4   5   6   7   8   9  10  11  12
   2       4   6   8  10  12  14  16  18  20  22  24
   3           9  12  15  18  21  24  27  30  33  36
   4              16  20  24  28  32  36  40  44  48
   5                  25  30  35  40  45  50  55  60
   6                      36  42  48  54  60  66  72
   7                          49  56  63  70  77  84
   8                              64  72  80  88  96
   9                                  81  90  99 108
  10                                     100 110 120
  11                                         121 132
  12                                             144
"
```



### Functional composition

As an alternative to iteration, we could also write the top level more declaratively, composing a solution from a set of generic functions.

{{trans|JavaScript}}  (ES5 functional version)

```AppleScript
tableText(multTable(1, 12))

-- multTable :: Int -> [[String]]
on multTable(m, n)

    set axis to enumFromTo(m, n)

    script column
        on |λ|(x)
            script row
                on |λ|(y)
                    if y < x then
                        ""
                    else
                        (x * y) as string
                    end if
                end |λ|
            end script

            {x & map(row, axis)}
        end |λ|
    end script

    {{"x"} & axis} & concatMap(column, axis)
end multTable

-- TABLE DISPLAY --------------------------------------------------------------

-- tableText :: [[Int]] -> String
on tableText(lstTable)
    script tableLine
        on |λ|(lstLine)
            script tableCell
                on |λ|(int)
                    (characters -4 thru -1 of ("    " & int)) as string
                end |λ|
            end script

            intercalate(" ", map(tableCell, lstLine))
        end |λ|
    end script

    intercalate(linefeed, map(tableLine, lstTable))
end tableText


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lst to {}
    set lng to length of xs
    tell mReturn(f)
        repeat with i from 1 to lng
            set lst to (lst & |λ|(item i of xs, i, xs))
        end repeat
    end tell
    return lst
end concatMap

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

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- justifyRight :: Int -> Char -> Text -> Text
on justifyRight(n, cFiller, strText)
    if n > length of strText then
        text -n thru -1 of ((replicate(n, cFiller) as text) & strText)
    else
        strText
    end if
end justifyRight

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
```

```txt
   x    1    2    3    4    5    6    7    8    9   10   11   12
   1    1    2    3    4    5    6    7    8    9   10   11   12
   2         4    6    8   10   12   14   16   18   20   22   24
   3              9   12   15   18   21   24   27   30   33   36
   4                  16   20   24   28   32   36   40   44   48
   5                       25   30   35   40   45   50   55   60
   6                            36   42   48   54   60   66   72
   7                                 49   56   63   70   77   84
   8                                      64   72   80   88   96
   9                                           81   90   99  108
  10                                               100  110  120
  11                                                    121  132
  12                                                         144
```


## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program multtable.s   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
.equ MAXI,   12
/*********************************/
/* Initialized data              */
/*********************************/
.data
sMessValeur:       .fill 11, 1, ' '            @ size => 11
szCarriageReturn: .asciz "\n"
sBlanc1:            .asciz " "
sBlanc2:            .asciz "  "
sBlanc3:            .asciz "   "
/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                @ entry of program
    push {fp,lr}      @ saves 2 registers
    @ display first line
    mov r4,#0
1:    @ begin loop
    mov r0,r4
    ldr r1,iAdrsMessValeur     @ display value
    bl conversion10             @ call function
    mov r2,#0                      @ final zéro
    strb r2,[r1,r0]               @ on display value
    ldr r0,iAdrsMessValeur
    bl affichageMess            @ display message
    cmp r4,#10                     @ one or two digit in résult
    ldrgt r0,iAdrsBlanc2       @ two  display two spaces
    ldrle r0,iAdrsBlanc3       @ one  display 3 spaces
    bl affichageMess            @ display message
    add r4,#1                      @ increment counter
    cmp r4,#MAXI
    ble 1b                       @ loop
    ldr r0,iAdrszCarriageReturn
    bl affichageMess            @ display carriage return

    mov r5,#1                   @ line counter
2:    @ begin loop lines
    mov r0,r5                      @ display column 1 with N° line
    ldr r1,iAdrsMessValeur     @ display value
    bl conversion10             @ call function
    mov r2,#0                      @ final zéro
    strb r2,[r1,r0]
    ldr r0,iAdrsMessValeur
    bl affichageMess            @ display message
    cmp r5,#10                      @ one or two digit in N° line
    ldrge r0,iAdrsBlanc2
    ldrlt r0,iAdrsBlanc3
    bl affichageMess
    mov r4,#1                     @ counter column
3:  @ begin loop columns
    mul r0,r4,r5                   @ multiplication
    mov r3,r0                      @ save résult
    ldr r1,iAdrsMessValeur     @ display value
    bl conversion10             @ call function
    mov r2,#0
    strb r2,[r1,r0]
    ldr r0,iAdrsMessValeur
    bl affichageMess            @ display message
    cmp r3,#100                    @ 3 digits in résult ?
    ldrge r0,iAdrsBlanc1       @ yes, display one space
    bge 4f
    cmp r3,#10                     @ 2 digits in result
    ldrge r0,iAdrsBlanc2       @ yes display 2 spaces
    ldrlt r0,iAdrsBlanc3       @ no  display 3 spaces
4:
    bl affichageMess            @ display message
    add r4,#1                      @ increment counter column
    cmp r4,r5                      @ < counter lines
    ble 3b                        @ loop
    ldr r0,iAdrszCarriageReturn
    bl affichageMess            @ display carriage return
    add r5,#1                      @ increment line counter
    cmp r5,#MAXI                  @ MAXI ?
    ble 2b                        @ loop

100:   @ standard end of the program
    mov r0, #0                  @ return code
    pop {fp,lr}                 @restaur 2 registers
    mov r7, #EXIT              @ request to exit program
    svc #0                       @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:	.int szCarriageReturn
iAdrsBlanc1:		.int sBlanc1
iAdrsBlanc2:		.int sBlanc2
iAdrsBlanc3:		.int sBlanc3
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}      @ save  registres
    mov r2,#0                  @ counter length
1:      @ loop length calculation
    ldrb r1,[r0,r2]           @ read octet start position + index
    cmp r1,#0                  @ if 0 its over
    addne r2,r2,#1            @ else add 1 in the length
    bne 1b                    @ and loop
                                @ so here r2 contains the length of the message
    mov r1,r0        			@ address message in r1
    mov r0,#STDOUT      		@ code to write to the standard output Linux
    mov r7, #WRITE             @ code call system "write"
    svc #0                      @ call systeme
    pop {r0,r1,r2,r7,lr}        @ restaur des  2 registres */
    bx lr                       @ return
/******************************************************************/
/*     Converting a register to a decimal unsigned                */
/******************************************************************/
/* r0 contains value and r1 address area   */
/* r0 return size of result (no zero final in area) */
/* area size => 11 bytes          */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}    @ save registers
    mov r3,r1
    mov r2,#LGZONECAL

1:	   @ start loop
    bl divisionpar10U   @unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48        @ digit
    strb r1,[r3,r2]  @ store digit on area
    cmp r0,#0         @ stop if quotient = 0 */
    subne r2,#1      @ else previous position
    bne 1b	          @ and loop
    @ and move digit from left of area
    mov r4,#0
2:
    ldrb r1,[r3,r2]
    strb r1,[r3,r4]
    add r2,#1
    add r4,#1
    cmp r2,#LGZONECAL
    ble 2b
    @ and move spaces in end on area
    mov r0,r4     @ result length
    mov r1,#' '   @ space
3:
    strb r1,[r3,r4]  @ store space in area
    add r4,#1         @ next position
    cmp r4,#LGZONECAL
    ble 3b           @ loop if r4 <= area size

100:
    pop {r1-r4,lr}    @ restaur registres
    bx lr             @return

/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient */
/* r1 remainder  */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0         @ save value
    mov r3,#0xCCCD   @ r3 <- magic_number  lower
    movt r3,#0xCCCC  @ r3 <- magic_number  upper
    umull r1, r2, r3, r0      @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0)
    mov r0, r2, LSR #3      @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2     @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1     @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                  @ leave function




```



## AutoHotkey


```autohotkey
Gui, -MinimizeBox
Gui, Margin, 0, 0
Gui, Font, s9, Fixedsys
Gui, Add, Edit, h0 w0
Gui, Add, Edit, w432 r14 -VScroll
Gosub, Table
Gui, Show,, Multiplication Table
Return

GuiClose:
GuiEscape:
    ExitApp
Return

Table:
    ; top row
    Table := "  x |"
    Loop, 12
        Table .= SubStr("   " A_Index, -3)
    Table .= "`n"

    ; underlines
    Table .= "----+"
    Loop, 48
        Table .= "-"
    Table .= "`n"

    ; table
    Loop, 12 { ; rows
        Table .= SubStr("  " Row := A_Index, -2) " |"
        Loop, 12 ; columns
            Table .= SubStr("    " (A_Index >= Row ? A_Index * Row : ""), -3)
        Table .= "`n"
    }
    GuiControl,, Edit2, %Table%
Return
```

Message box shows:

```txt
  x |   1   2   3   4   5   6   7   8   9  10  11  12
----+------------------------------------------------
  1 |   1   2   3   4   5   6   7   8   9  10  11  12
  2 |       4   6   8  10  12  14  16  18  20  22  24
  3 |           9  12  15  18  21  24  27  30  33  36
  4 |              16  20  24  28  32  36  40  44  48
  5 |                  25  30  35  40  45  50  55  60
  6 |                      36  42  48  54  60  66  72
  7 |                          49  56  63  70  77  84
  8 |                              64  72  80  88  96
  9 |                                  81  90  99 108
 10 |                                     100 110 120
 11 |                                         121 132
 12 |                                             144

```



## AutoIt


```AutoIt
#AutoIt Version: 3.2.10.0
$tableupto=12
$table=""
for $i = 1 To $tableupto
   for $j = $i to $tableupto
      $prod=string($i*$j)
      if StringLen($prod) == 1  then
	 $prod = "    "& $prod
      EndIf
      if StringLen($prod) == 2  then
	 $prod = "  "& $prod
      EndIf
      $table = $table&" "&$prod
   Next
   $table = $table&"  - "&$i&@CRLF
   for  $k = 1 to $i
      $table = $table&"       "
   Next
Next
msgbox(0,"Multiplication Tables",$table)
```



## AWK


```AWK

BEGIN {
  for(i=1;i<=12;i++){
    for(j=1;j<=12;j++){
      if(j>=i||j==1){printf "%4d",i*j}
      else          {printf "    "}
  }
  print
 }
}
```

```txt

   1   2   3   4   5   6   7   8   9  10  11  12
   2   4   6   8  10  12  14  16  18  20  22  24
   3       9  12  15  18  21  24  27  30  33  36
   4          16  20  24  28  32  36  40  44  48
   5              25  30  35  40  45  50  55  60
   6                  36  42  48  54  60  66  72
   7                      49  56  63  70  77  84
   8                          64  72  80  88  96
   9                              81  90  99 108
  10                                 100 110 120
  11                                     121 132
  12                                         144

```



## Axe

Since the standard text output is poorly suited to this kind of formatted data, this example is implemented by writing to the screen buffer using the small font. Also, the limits were adjusted to 10x8 to make the table fit the screen.

```axe
Fix 5
ClrDraw
For(I,1,10)
 Text(I-1*9,0,I▶Dec)
 Text(91,I*7+1,I▶Dec)
End

For(J,1,8)
 For(I,J,10)
  Text(I-1*9,J*7+1,I*J▶Dec)
 End
End

HLine(7)
VLine(89)
DispGraph
getKeyʳ
Fix 4
```


Approximate output:

```txt

1  2  3  4  5  6  7  8  9  10 |
---------------------------------
1  2  3  4  5  6  7  8  9  10 | 1
   4  6  8  10 12 14 16 18 20 | 2
      9  12 15 18 21 24 27 30 | 3
         16 20 24 28 32 36 40 | 4
            25 30 35 40 45 50 | 5
               36 42 48 54 60 | 6
                  49 56 63 70 | 7
                     64 72 80 | 8

```



## BASIC

```qbasic
CLS

'header row
PRINT "     ";
FOR n = 1 TO 12
    'do it this way for alignment purposes
    o$ = "    "
    MID$(o$, LEN(o$) - LEN(STR$(n)) + 1) = STR$(n)
    PRINT o$;
NEXT
PRINT : PRINT "    "; STRING$(49, "-");

FOR n = 1 TO 12
    PRINT
    IF n < 10 THEN PRINT " ";
    PRINT n; "|";   'row labels
    FOR m = 1 TO n - 1
        PRINT "    ";
    NEXT
    FOR m = n TO 12
        'alignment again
        o$ = "    "
        MID$(o$, LEN(o$) - LEN(STR$(m * n)) + 1) = STR$(m * n)
        PRINT o$;
    NEXT
NEXT
```


```txt

        1   2   3   4   5   6   7   8   9  10  11  12
    -------------------------------------------------
  1 |   1   2   3   4   5   6   7   8   9  10  11  12
  2 |       4   6   8  10  12  14  16  18  20  22  24
  3 |           9  12  15  18  21  24  27  30  33  36
  4 |              16  20  24  28  32  36  40  44  48
  5 |                  25  30  35  40  45  50  55  60
  6 |                      36  42  48  54  60  66  72
  7 |                          49  56  63  70  77  84
  8 |                              64  72  80  88  96
  9 |                                  81  90  99 108
 10 |                                     100 110 120
 11 |                                         121 132
 12 |                                             144

```


'''See also:''' [[#BBC BASIC|BBC BASIC]], [[#Liberty BASIC|Liberty BASIC]], [[#PureBasic|PureBasic]]

=
## Applesoft BASIC
=

```ApplesoftBasic
100 M = 12
110 DEF FN T(X) = X * 3 + (X < 4) * (4 - X) + (X > 10) * (X - 10) - 1
120 FOR N = -1 TO M
130     IF NOT N THEN PRINT CHR$(13) TAB(5); : FOR J = 5 TO FN T(M + 1) - 2 : PRINT "-"; : NEXT J, N
140     I = ABS(N)
150     IF N > 0 THEN PRINT CHR$(13) MID$("  ", 1, I < 10) I" !";
160     FOR J = I TO M
170         V$ = STR$(I * J)
180         PRINT TAB(FN T(J)) MID$("  ", 1, 3 - LEN(V$) - (J < 4)) V$;
190 NEXT J, N
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "Multipli.bas"
110 TEXT 80
120 PRINT TAB(7);
130 FOR I=1 TO 12
140   PRINT USING " ###":I;
150 NEXT
160 PRINT AT 2,5:"----------------------------------------------------"
170 FOR I=1 TO 12
180   PRINT USING "### |":I;:PRINT TAB(I*4+3);
190   FOR J=I TO 12
200     PRINT USING " ###":I*J;
210   NEXT
220   PRINT
230 NEXT
```



## Batch File


```dos
@echo off
setlocal enabledelayedexpansion

::The Main Thing...
cls
set colum=12&set row=12
call :multable
echo.
pause
exit /b 0
::/The Main Thing.

::The Functions...
:multable
	echo.
	for /l %%. in (1,1,%colum%) do (
	call :numstr %%.
	set firstline=!firstline!!space!%%.
	set seconline=!seconline!-----
	)
	echo !firstline!
	echo !seconline!

	::The next lines here until the "goto :EOF" prints the products...

	for /l %%X in (1,1,%row%) do (
		for /l %%Y in (1,1,%colum%) do (
			if %%Y lss %%X (set "line%%X=!line%%X!     ") else (
				set /a ans=%%X*%%Y
				call :numstr !ans!
				set "line%%X=!line%%X!!space!!ans!"
			)
		)
		echo.!line%%X! ^| %%X
	)
	goto :EOF

:numstr
	::This function returns the number of whitespaces to be applied on each numbers.
	set cnt=0&set proc=%1&set space=
	:loop
	set currchar=!proc:~%cnt%,1!
	if not "!currchar!"=="" set /a cnt+=1&goto loop
	set /a numspaces=5-!cnt!
	for /l %%A in (1,1,%numspaces%) do set "space=!space! "
goto :EOF
::/The Functions.
```

```txt

    1    2    3    4    5    6    7    8    9   10   11   12
------------------------------------------------------------
    1    2    3    4    5    6    7    8    9   10   11   12 | 1
         4    6    8   10   12   14   16   18   20   22   24 | 2
              9   12   15   18   21   24   27   30   33   36 | 3
                  16   20   24   28   32   36   40   44   48 | 4
                       25   30   35   40   45   50   55   60 | 5
                            36   42   48   54   60   66   72 | 6
                                 49   56   63   70   77   84 | 7
                                      64   72   80   88   96 | 8
                                           81   90   99  108 | 9
                                               100  110  120 | 10
                                                    121  132 | 11
                                                         144 | 12

Press any key to continue . . .
```



## BBC BASIC

BBC BASIC automatically right-justifies numeric output.

```bbcbasic
      @% = 5 : REM Set column width
      FOR row% = 1 TO 12
        PRINT row% TAB(row% * @%) ;
        FOR col% = row% TO 12
          PRINT row% * col% ;
        NEXT col%
        PRINT
      NEXT row%
```

```txt
    1    1    2    3    4    5    6    7    8    9   10   11   12
    2         4    6    8   10   12   14   16   18   20   22   24
    3              9   12   15   18   21   24   27   30   33   36
    4                  16   20   24   28   32   36   40   44   48
    5                       25   30   35   40   45   50   55   60
    6                            36   42   48   54   60   66   72
    7                                 49   56   63   70   77   84
    8                                      64   72   80   88   96
    9                                           81   90   99  108
   10                                               100  110  120
   11                                                    121  132
   12                                                         144
```



## Befunge


```befunge>0>51p0
52p51g52g*:51g52g`!*\!51g52g+*+0\3>01p::55+%68*+\!28v
w^p2<y|!`+66:+1,+*84*"\"!:g25$_,#!>#:<$$_^#!:-1g10/+55\-**<<
"$9"^x>$55+,51g1+:66+`#@_055+68*\>\#<1#*-#9:#5_$"+---">:#,_$
```


```txt
   |  1   2   3   4   5   6   7   8   9  10  11  12
---+-----------------------------------------------
  1|  1   2   3   4   5   6   7   8   9  10  11  12
  2|      4   6   8  10  12  14  16  18  20  22  24
  3|          9  12  15  18  21  24  27  30  33  36
  4|             16  20  24  28  32  36  40  44  48
  5|                 25  30  35  40  45  50  55  60
  6|                     36  42  48  54  60  66  72
  7|                         49  56  63  70  77  84
  8|                             64  72  80  88  96
  9|                                 81  90  99 108
 10|                                    100 110 120
 11|                                        121 132
 12|                                            144
```



## Bracmat


```Bracmat
  ( multiplicationTable
  =     high i j row row2 matrix padFnc tmp
      , celPad leftCelPad padFnc celDashes leftDashes
    .   !arg:?high
      & ( padFnc
        =   L i w d
          .   @(!arg:? [?L)
            & 1+(!L:?i):?L
            & " ":?w
            & "-":?d
            &   whl
              ' ( !i+-1:~<0:?i
                & " " !w:?w
                & "-" !d:?d
                )
            & str$!w:?w
            & (
                ' (
                  .   @(str$(rev$!arg ()$w):?arg [($L) ?)
                    & rev$!arg
                  )
              . str$!d
              )
        )
      & padFnc$(!high^2):((=?celPad).?celDashes)
      & @(!high:?tmp [-2 ?)
      & padFnc$!tmp:((=?leftCelPad).?leftDashes)
      & 0:?i
      & :?row:?row2
      &   whl
        ' ( 1+!i:~>!high:?i
          & !row celPad$!i:?row
          & !celDashes !row2:?row2
          )
      &   str$(leftCelPad$X "|" !row \n !leftDashes "+" !row2 \n)
        : ?matrix
      & 0:?j
      &   whl
        ' ( 1+!j:~>!high:?j
          & 0:?i
          & :?row
          &   whl
            ' ( 1+!i:<!j:?i
              & celPad$() !row:?row
              )
          & leftCelPad$!j "|" !row:?row
          &   whl
            ' ( 1+!i:~>!high:?i
              & !row celPad$(!i*!j):?row
              )
          & !matrix str$(!row \n):?matrix
          )
      & str$!matrix
  )
& out$(multiplicationTable$12)
& done;
```

```txt
 X|   1   2   3   4   5   6   7   8   9  10  11  12
--+------------------------------------------------
 1|   1   2   3   4   5   6   7   8   9  10  11  12
 2|       4   6   8  10  12  14  16  18  20  22  24
 3|           9  12  15  18  21  24  27  30  33  36
 4|              16  20  24  28  32  36  40  44  48
 5|                  25  30  35  40  45  50  55  60
 6|                      36  42  48  54  60  66  72
 7|                          49  56  63  70  77  84
 8|                              64  72  80  88  96
 9|                                  81  90  99 108
10|                                     100 110 120
11|                                         121 132
12|                                             144
```



## C


```c
#include <stdio.h>

int main(void)
{
	int i, j, n = 12;

	for (j = 1; j <= n; j++) printf("%3d%c", j, j != n ? ' ' : '\n');
	for (j = 0; j <= n; j++) printf(j != n ? "----" : "+\n");

	for (i = 1; i <= n; i++) {
		for (j = 1; j <= n; j++)
			printf(j < i ? "    " : "%3d ", i * j);
                printf("| %d\n", i);
        }

	return 0;
}
```

```txt
  1   2   3   4   5   6   7   8   9  10  11  12
------------------------------------------------+
  1   2   3   4   5   6   7   8   9  10  11  12 | 1
      4   6   8  10  12  14  16  18  20  22  24 | 2
          9  12  15  18  21  24  27  30  33  36 | 3
             16  20  24  28  32  36  40  44  48 | 4
                 25  30  35  40  45  50  55  60 | 5
                     36  42  48  54  60  66  72 | 6
                         49  56  63  70  77  84 | 7
                             64  72  80  88  96 | 8
                                 81  90  99 108 | 9
                                    100 110 120 | 10
                                        121 132 | 11
                                            144 | 12
```



## C++

This is a slightly more-generalized version
that takes any minimum and maximum table value,
and formats the table columns.


```cpp
#include <iostream>
#include <iomanip>
#include <cmath> // for log10()
#include <algorithm> // for max()

size_t table_column_width(const int min, const int max)
{
    unsigned int abs_max = std::max(max*max, min*min);

    // abs_max is the largest absolute value we might see.
    // If we take the log10 and add one, we get the string width
    // of the largest possible absolute value.
    // Add one more for a little whitespace guarantee.
    size_t colwidth = 2 + std::log10(abs_max);

    // If only one of them is less than 0, then some will
    // be negative. If some values may be negative, then we need to add some space
    // for a sign indicator (-)
    if (min < 0 && max > 0)
	++colwidth;
    return colwidth;
}

struct Writer_
{
    decltype(std::setw(1)) fmt_;
    Writer_(size_t w) : fmt_(std::setw(w)) {}
    template<class T_> Writer_& operator()(const T_& info) { std::cout << fmt_ << info; return *this; }
};

void print_table_header(const int min, const int max)
{
    Writer_ write(table_column_width(min, max));

    // table corner
    write(" ");
    for(int col = min; col <= max; ++col)
        write(col);

    // End header with a newline and blank line.
    std::cout << std::endl << std::endl;
}

void print_table_row(const int num, const int min, const int max)
{
    Writer_ write(table_column_width(min, max));

    // Header column
    write(num);

    // Spacing to ensure only the top half is printed
    for(int multiplicand = min; multiplicand < num; ++multiplicand)
        write(" ");

    // Remaining multiplicands for the row.
    for(int multiplicand = num; multiplicand <= max; ++multiplicand)
        write(num * multiplicand);

    // End row with a newline and blank line.
    std::cout << std::endl << std::endl;
}

void print_table(const int min, const int max)
{
    // Header row
    print_table_header(min, max);

    // Table body
    for(int row = min; row <= max; ++row)
        print_table_row(row, min, max);
}

int main()
{
    print_table(1, 12);
    return 0;
}

```


```txt

       1   2   3   4   5   6   7   8   9  10  11  12

   1   1   2   3   4   5   6   7   8   9  10  11  12

   2       4   6   8  10  12  14  16  18  20  22  24

   3           9  12  15  18  21  24  27  30  33  36

   4              16  20  24  28  32  36  40  44  48

   5                  25  30  35  40  45  50  55  60

   6                      36  42  48  54  60  66  72

   7                          49  56  63  70  77  84

   8                              64  72  80  88  96

   9                                  81  90  99 108

  10                                     100 110 120

  11                                         121 132

  12                                             144


```



## C#


```c#
using System;

namespace multtbl
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.Write(" X".PadRight(4));
            for (int i = 1; i <= 12; i++)
                Console.Write(i.ToString("####").PadLeft(4));

            Console.WriteLine();
            Console.Write(" ___");

            for (int i = 1; i <= 12; i++)
                Console.Write(" ___");

            Console.WriteLine();
            for (int row = 1; row <= 12; row++)
            {
                Console.Write(row.ToString("###").PadLeft(3).PadRight(4));
                for (int col = 1; col <= 12; col++)
                {
                    if (row <= col)
                        Console.Write((row * col).ToString("###").PadLeft(4));
                    else
                        Console.Write("".PadLeft(4));
                }

                Console.WriteLine();
            }

            Console.WriteLine();
            Console.ReadLine();
        }
    }
}

```


```txt
 X     1   2   3   4   5   6   7   8   9  10  11  12
 ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___
  1    1   2   3   4   5   6   7   8   9  10  11  12
  2        4   6   8  10  12  14  16  18  20  22  24
  3            9  12  15  18  21  24  27  30  33  36
  4               16  20  24  28  32  36  40  44  48
  5                   25  30  35  40  45  50  55  60
  6                       36  42  48  54  60  66  72
  7                           49  56  63  70  77  84
  8                               64  72  80  88  96
  9                                   81  90  99 108
 10                                      100 110 120
 11                                          121 132
 12                                              144

```



## Chef



```chef
Multigrain Bread.

Prints out a multiplication table.

Ingredients.
12 cups flour
12 cups grains
12 cups seeds
1 cup water
9 dashes yeast
1 cup nuts
40 ml honey
1 cup sugar

Method.
Sift the flour.
 Put flour into the 1st mixing bowl.
 Put yeast into the 1st mixing bowl.
Shake the flour until sifted.
Put grains into the 2nd mixing bowl.
Fold flour into the 2nd mixing bowl.
Put water into the 2nd mixing bowl.
Add yeast into the 2nd mixing bowl.
Combine flour into the 2nd mixing bowl.
Fold nuts into the 2nd mixing bowl.
Liquify nuts.
Put nuts into the 1st mixing bowl.
Pour contents of the 1st mixing bowl into the baking dish.
Sieve the flour.
 Put yeast into the 2nd mixing bowl.
 Add water into the 2nd mixing bowl.
 Sprinkle the seeds.
   Put flour into the 2nd mixing bowl.
   Combine seeds into the 2nd mixing bowl.
   Put yeast into the 2nd mixing bowl.
   Put seeds into the 2nd mixing bowl.
   Remove flour from the 2nd mixing bowl.
   Fold honey into the 2nd mixing bowl.
   Put water into the 2nd mixing bowl.
   Fold sugar into the 2nd mixing bowl.
   Squeeze the honey.
     Put water into the 2nd mixing bowl.
     Remove water from the 2nd mixing bowl.
     Fold sugar into the 2nd mixing bowl.
     Set aside.
   Drip until squeezed.
   Scoop the sugar.
     Crush the seeds.
       Put yeast into the 2nd mixing bowl.
     Grind the seeds until crushed.
     Put water into the 2nd mixing bowl.
     Fold seeds into the 2nd mixing bowl.
     Set aside.
   Drop until scooped.
 Randomize the seeds until sprinkled.
 Fold honey into the 2nd mixing bowl.
 Put flour into the 2nd mixing bowl.
 Put grains into the 2nd mixing bowl.
 Fold seeds into the 2nd mixing bowl.
Shake the flour until sieved.
Put yeast into the 2nd mixing bowl.
Add water into the 2nd mixing bowl.
Pour contents of the 2nd mixing bowl into the 2nd baking dish.

Serves 2.
```


```txt
  x    1   2   3   4   5   6   7   8   9  10  11  12
   1   1   2   3   4   5   6   7   8   9  10  11  12
   2       4   6   8  10  12  14  16  18  20  22  24
   3           9  12  15  18  21  24  27  30  33  36
   4              16  20  24  28  32  36  40  44  48
   5                  25  30  35  40  45  50  55  60
   6                      36  42  48  54  60  66  72
   7                          49  56  63  70  77  84
   8                              64  72  80  88  96
   9                                  81  90  99 108
  10                                     100 110 120
  11                                         121 132
  12                                             144

```



## Clojure

This is more generalized.
Any size can be used and the table will be formatted appropriately.

```lisp
(let [size 12
      trange (range 1 (inc size))
      fmt-width (+ (.length (str (* size size))) 1)
      fmt-str (partial format (str "%" fmt-width "s"))
      fmt-dec (partial format (str "% " fmt-width "d"))]

  (doseq [s (cons
             (apply str (fmt-str " ") (map #(fmt-dec %) trange))
             (for [i trange]
               (apply str (fmt-dec i) (map #(fmt-str (str %))
                                           (map #(if (>= % i) (* i %) " ")
                                                (for [j trange] j))))))]
    (println s)))

```


```txt
       1   2   3   4   5   6   7   8   9  10  11  12
   1   1   2   3   4   5   6   7   8   9  10  11  12
   2       4   6   8  10  12  14  16  18  20  22  24
   3           9  12  15  18  21  24  27  30  33  36
   4              16  20  24  28  32  36  40  44  48
   5                  25  30  35  40  45  50  55  60
   6                      36  42  48  54  60  66  72
   7                          49  56  63  70  77  84
   8                              64  72  80  88  96
   9                                  81  90  99 108
  10                                     100 110 120
  11                                         121 132
  12                                             144

```



## COBOL


```COBOL
       identification division.
       program-id. multiplication-table.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       data division.
       working-storage section.
       01 multiplication.
          05 rows occurs 12 times.
             10 colm occurs 12 times.
                15 num    pic 999.
       77 cand pic 99.
       77 ier  pic 99.
       77 ind  pic z9.
       77 show pic zz9.

       procedure division.
       sample-main.
       perform varying cand from 1 by 1 until cand greater than 12
                  after ier from 1 by 1 until ier greater than 12
           multiply cand by ier giving num(cand, ier)
       end-perform

       perform varying cand from 1 by 1 until cand greater than 12
           move cand to ind
           display "x " ind "| " with no advancing
           perform varying ier from 1 by 1 until ier greater than 12
               if ier greater than or equal to cand then
                   move num(cand, ier) to show
                   display show with no advancing
                   if ier equal to 12 then
                       display "|"
                   else
                       display space with no advancing
                   end-if
               else
                   display "    " with no advancing
               end-if
           end-perform
       end-perform

       goback.
       end program multiplication-table.

```

```txt
prompt$ cobc -xj multiplication-table.cob
x  1|   1   2   3   4   5   6   7   8   9  10  11  12|
x  2|       4   6   8  10  12  14  16  18  20  22  24|
x  3|           9  12  15  18  21  24  27  30  33  36|
x  4|              16  20  24  28  32  36  40  44  48|
x  5|                  25  30  35  40  45  50  55  60|
x  6|                      36  42  48  54  60  66  72|
x  7|                          49  56  63  70  77  84|
x  8|                              64  72  80  88  96|
x  9|                                  81  90  99 108|
x 10|                                     100 110 120|
x 11|                                         121 132|
x 12|                                             144|

```



## CoffeeScript


```coffeescript

print_multiplication_tables = (n) ->
  width = 4

  pad = (s, n=width, c=' ') ->
    s = s.toString()
    result = ''
    padding = n - s.length
    while result.length < padding
      result += c
    result + s

  s = pad('') + '|'
  for i in [1..n]
    s += pad i
  console.log s

  s = pad('', width, '-') + '+'
  for i in [1..n]
    s += pad '', width, '-'
  console.log s


  for i in [1..n]
    s = pad i
    s += '|'
    s += pad '', width*(i - 1)
    for j in [i..n]
       s += pad i*j
    console.log s

print_multiplication_tables 12

```


```txt

> coffee multiply.coffee
    |   1   2   3   4   5   6   7   8   9  10  11  12
----+------------------------------------------------
   1|   1   2   3   4   5   6   7   8   9  10  11  12
   2|       4   6   8  10  12  14  16  18  20  22  24
   3|           9  12  15  18  21  24  27  30  33  36
   4|              16  20  24  28  32  36  40  44  48
   5|                  25  30  35  40  45  50  55  60
   6|                      36  42  48  54  60  66  72
   7|                          49  56  63  70  77  84
   8|                              64  72  80  88  96
   9|                                  81  90  99 108
  10|                                     100 110 120
  11|                                         121 132
  12|                                             144

```



## Common Lisp


```lisp

(do ((m 0 (if (= 12 m) 0 (1+ m)))
     (n 0 (if (= 12 m) (1+ n) n)))
    ((= n 13))
  (if (zerop n)
      (case m
        (0 (format t "  *|"))
        (12 (format t "  12~&---+------------------------------------------------~&"))
        (otherwise
         (format t "~4,D" m)))
      (case m
        (0 (format t "~3,D|" n))
        (12 (format t "~4,D~&" (* n m)))
        (otherwise
         (if (>= m n)
             (format t "~4,D" (* m n))
             (format t "    "))))))

```

Output:

```txt


  *|   1   2   3   4   5   6   7   8   9  10  11  12
---+------------------------------------------------
  1|   1   2   3   4   5   6   7   8   9  10  11  12
  2|       4   6   8  10  12  14  16  18  20  22  24
  3|           9  12  15  18  21  24  27  30  33  36
  4|              16  20  24  28  32  36  40  44  48
  5|                  25  30  35  40  45  50  55  60
  6|                      36  42  48  54  60  66  72
  7|                          49  56  63  70  77  84
  8|                              64  72  80  88  96
  9|                                  81  90  99 108
 10|                                     100 110 120
 11|                                         121 132
 12|                                             144

```



## D

```d
void main() {
    import std.stdio, std.array, std.range, std.algorithm;

    enum n = 12;
    writefln("    %(%4d%)\n%s", iota(1, n+1), "-".replicate(4*n + 4));
    foreach (immutable y; 1 .. n + 1)
        writefln("%4d" ~ " ".replicate(4 * (y - 1)) ~ "%(%4d%)", y,
                 iota(y, n + 1).map!(x => x * y));
}
```

```txt
       1   2   3   4   5   6   7   8   9  10  11  12
----------------------------------------------------
   1   1   2   3   4   5   6   7   8   9  10  11  12
   2       4   6   8  10  12  14  16  18  20  22  24
   3           9  12  15  18  21  24  27  30  33  36
   4              16  20  24  28  32  36  40  44  48
   5                  25  30  35  40  45  50  55  60
   6                      36  42  48  54  60  66  72
   7                          49  56  63  70  77  84
   8                              64  72  80  88  96
   9                                  81  90  99 108
  10                                     100 110 120
  11                                         121 132
  12                                             144
```


## DCL


```DCL
$ max = 12
$ h = f$fao( "!4* " )
$ r = 0
$ loop1:
$  o = ""
$  c = 0
$  loop2:
$   if r .eq. 0 then $ h = h + f$fao( "!4SL", c )
$   p = r * c
$   if c .ge. r
$   then
$    o = o + f$fao( "!4SL", p )
$   else
$    o = o + f$fao( "!4* " )
$   endif
$   c = c + 1
$   if c .le. max then $ goto loop2
$  if r .eq. 0
$  then
$   write sys$output h
$   n = 4 * ( max + 2 )
$   write sys$output f$fao( "!''n*-" )
$  endif
$  write sys$output f$fao( "!4SL", r ) + o
$  r = r + 1
$  if r .le. max then $ goto loop1
```

```txt
$ @multiplication_tables
       0   1   2   3   4   5   6   7   8   9  10  11  12
--------------------------------------------------------
   0   0   0   0   0   0   0   0   0   0   0   0   0   0
   1       1   2   3   4   5   6   7   8   9  10  11  12
   2           4   6   8  10  12  14  16  18  20  22  24
   3               9  12  15  18  21  24  27  30  33  36
   4                  16  20  24  28  32  36  40  44  48
   5                      25  30  35  40  45  50  55  60
   6                          36  42  48  54  60  66  72
   7                              49  56  63  70  77  84
   8                                  64  72  80  88  96
   9                                      81  90  99 108
  10                                         100 110 120
  11                                             121 132
  12                                                 144

```



## Delphi

```delphi
program MultiplicationTables;

{$APPTYPE CONSOLE}

uses SysUtils;

const
  MAX_COUNT = 12;
var
  lRow, lCol: Integer;
begin
  Write('  | ');
  for lRow := 1 to MAX_COUNT do
    Write(Format('%4d', [lRow]));
  Writeln('');
  Writeln('--+-' + StringOfChar('-', MAX_COUNT * 4));
  for lRow := 1 to MAX_COUNT do
  begin
    Write(Format('%2d', [lRow]));
    Write('| ');
    for lCol := 1 to MAX_COUNT do
    begin
      if lCol < lRow then
        Write('    ')
      else
        Write(Format('%4d', [lRow * lCol]));
    end;
    Writeln;
  end;
end.
```



## DWScript



```delphi
const size = 12;
var row, col : Integer;

Print('  | ');
for row:=1 to size do
   Print(Format('%4d', [row]));
PrintLn('');
PrintLn('--+-'+StringOfChar('-', size*4));
for row:=1 to size do begin
   Print(Format('%2d', [row]));
   Print('| ');
   for col:=1 to size do begin
      if col<row then
         Print('    ')
      else Print(Format('%4d', [row*col]));
   end;
   PrintLn('');
end;

```



## E



```e
  def size := 12
  println(`{|style="border-collapse: collapse; text-align: right;"`)
  println(`|`)
  for x in 1..size {
    println(`|style="border-bottom: 1px solid black; " | $x`)
  }
  for y in 1..size {
    println(`|-`)
      println(`|style="border-right: 1px solid black;" | $y`)
    for x in 1..size {
      println(`|  ${if (x >= y) { x*y } else {""}}`)
    }
  }
  println("|}")
```


Targets MediaWiki markup.
<blockquote>
{|style="border-collapse: collapse; text-align: right;"
|
|style="border-bottom: 1px solid black; " | 1
|style="border-bottom: 1px solid black; " | 2
|style="border-bottom: 1px solid black; " | 3
|style="border-bottom: 1px solid black; " | 4
|style="border-bottom: 1px solid black; " | 5
|style="border-bottom: 1px solid black; " | 6
|style="border-bottom: 1px solid black; " | 7
|style="border-bottom: 1px solid black; " | 8
|style="border-bottom: 1px solid black; " | 9
|style="border-bottom: 1px solid black; " | 10
|style="border-bottom: 1px solid black; " | 11
|style="border-bottom: 1px solid black; " | 12
|-
|style="border-right: 1px solid black;" | 1
|  1
|  2
|  3
|  4
|  5
|  6
|  7
|  8
|  9
|  10
|  11
|  12
|-
|style="border-right: 1px solid black;" | 2
|
|  4
|  6
|  8
|  10
|  12
|  14
|  16
|  18
|  20
|  22
|  24
|-
|style="border-right: 1px solid black;" | 3
|
|
|  9
|  12
|  15
|  18
|  21
|  24
|  27
|  30
|  33
|  36
|-
|style="border-right: 1px solid black;" | 4
|
|
|
|  16
|  20
|  24
|  28
|  32
|  36
|  40
|  44
|  48
|-
|style="border-right: 1px solid black;" | 5
|
|
|
|
|  25
|  30
|  35
|  40
|  45
|  50
|  55
|  60
|-
|style="border-right: 1px solid black;" | 6
|
|
|
|
|
|  36
|  42
|  48
|  54
|  60
|  66
|  72
|-
|style="border-right: 1px solid black;" | 7
|
|
|
|
|
|
|  49
|  56
|  63
|  70
|  77
|  84
|-
|style="border-right: 1px solid black;" | 8
|
|
|
|
|
|
|
|  64
|  72
|  80
|  88
|  96
|-
|style="border-right: 1px solid black;" | 9
|
|
|
|
|
|
|
|
|  81
|  90
|  99
|  108
|-
|style="border-right: 1px solid black;" | 10
|
|
|
|
|
|
|
|
|
|  100
|  110
|  120
|-
|style="border-right: 1px solid black;" | 11
|
|
|
|
|
|
|
|
|
|
|  121
|  132
|-
|style="border-right: 1px solid black;" | 12
|
|
|
|
|
|
|
|
|
|
|
|  144
|}
</blockquote>


## EasyLang


<lang>n = 12
func out h . .
  if h < 10
    write "  "
  elif h < 100
    write " "
  .
  write " "
  write h
.
write "     "
for i = 1 to n
  call out i
.
pr ""
write "     "
for i = 1 to n
  write "----"
.
pr ""
for i = 1 to n
  call out i
  write "|"
  for j = 1 to n
    if j < i
      write "    "
    else
      call out i * j
    .
  .
  pr ""
.
```



## EchoLisp


```scheme

(lib 'matrix)

(define (mtable i j)
    (cond
    ((and (zero? i) (zero? j)) "😅")
    ((= i 0) j)
    ((= j 0) i)
    ((>= j i ) (* i j ))
    (else " ")))

(array-print (build-array 13 13 mtable))


```

```txt

  😅   1   2   3   4    5    6    7    8    9    10    11    12
  1    1   2   3   4    5    6    7    8    9    10    11    12
  2        4   6   8    10   12   14   16   18   20    22    24
  3            9   12   15   18   21   24   27   30    33    36
  4                16   20   24   28   32   36   40    44    48
  5                     25   30   35   40   45   50    55    60
  6                          36   42   48   54   60    66    72
  7                               49   56   63   70    77    84
  8                                    64   72   80    88    96
  9                                         81   90    99    108
  10                                             100   110   120
  11                                                   121   132
  12                                                         144

```



## Elixir


```elixir
defmodule RC do
  def multiplication_tables(n) do
    IO.write " X |"
    Enum.each(1..n, fn i -> :io.fwrite("~4B", [i]) end)
    IO.puts "\n---+" <> String.duplicate("----", n)
    Enum.each(1..n, fn j ->
      :io.fwrite("~2B |", [j])
      Enum.each(1..n, fn i ->
        if i<j, do: (IO.write "    "), else: :io.fwrite("~4B", [i*j])
      end)
      IO.puts ""
    end)
  end
end

RC.multiplication_tables(12)
```


```txt

 X |   1   2   3   4   5   6   7   8   9  10  11  12
---+------------------------------------------------
 1 |   1   2   3   4   5   6   7   8   9  10  11  12
 2 |       4   6   8  10  12  14  16  18  20  22  24
 3 |           9  12  15  18  21  24  27  30  33  36
 4 |              16  20  24  28  32  36  40  44  48
 5 |                  25  30  35  40  45  50  55  60
 6 |                      36  42  48  54  60  66  72
 7 |                          49  56  63  70  77  84
 8 |                              64  72  80  88  96
 9 |                                  81  90  99 108
10 |                                     100 110 120
11 |                                         121 132
12 |                                             144

```



## Erlang


```Erlang

-module( multiplication_tables ).

-export( [print_upto/1, task/0, upto/1] ).

print_upto( N ) ->
	Upto_tuples = [{X, {Y, Sum}} || {X, Y, Sum} <- upto(N)],
	io:fwrite( "  " ),
	[io:fwrite( "~5B", [X]) || X <- lists:seq(1, N)],
	io:nl(),
	io:nl(),
	[print_upto(X, proplists:get_all_values(X, Upto_tuples)) || X <- lists:seq(1, N)].


task() -> print_upto( 12 ).

upto( N ) -> [{X, Y, X*Y} || X <- lists:seq(1, N), Y <- lists:seq(1, N), Y >= X].



print_upto( N, Uptos ) ->
	io:fwrite( "~2B", [N] ),
	io:fwrite( "~*s", [5*(N - 1), " "] ),
	[io:fwrite("~5B", [Sum]) || {_Y, Sum} <- Uptos],
	io:nl().

```

```txt

25> multiplication_tables:task().
      1    2    3    4    5    6    7    8    9   10   11   12

 1    1    2    3    4    5    6    7    8    9   10   11   12
 2         4    6    8   10   12   14   16   18   20   22   24
 3              9   12   15   18   21   24   27   30   33   36
 4                  16   20   24   28   32   36   40   44   48
 5                       25   30   35   40   45   50   55   60
 6                            36   42   48   54   60   66   72
 7                                 49   56   63   70   77   84
 8                                      64   72   80   88   96
 9                                           81   90   99  108
10                                               100  110  120
11                                                    121  132
12                                                         144

```




## Euphoria


```Euphoria
puts(1," x")
for i = 1 to 12 do
    printf(1," %3d",i)
end for

puts(1,'\n')

for i = 1 to 12 do
  printf(1,"%2d",i)
  for j = 1 to 12 do
    if j<i then
      puts(1,"    ")
    else
      printf(1," %3d",i*j)
    end if
  end for
  puts(1,'\n')
end for
```


```txt

  x   1   2   3   4   5   6   7   8   9  10  11  12
  1   1   2   3   4   5   6   7   8   9  10  11  12
  2       4   6   8  10  12  14  16  18  20  22  24
  3           9  12  15  18  21  24  27  30  33  36
  4              16  20  24  28  32  36  40  44  48
  5                  25  30  35  40  45  50  55  60
  6                      36  42  48  54  60  66  72
  7                          49  56  63  70  77  84
  8                              64  72  80  88  96
  9                                  81  90  99 108
 10                                     100 110 120
 11                                         121 132
 12                                             144

```


=={{header|F Sharp|F#}}==
Translation of C#

```FSharp

open System

let multTable () =
    Console.Write (" X".PadRight (4))
    for i = 1 to 12 do Console.Write ((i.ToString "####").PadLeft 4)
    Console.Write "\n ___"
    for i = 1 to 12 do Console.Write " ___"
    Console.WriteLine ()
    for row = 1 to 12 do
        Console.Write (row.ToString("###").PadLeft(3).PadRight(4))
        for col = 1 to 12 do
            if row <= col then Console.Write ((row * col).ToString("###").PadLeft(4))
            else
                Console.Write ("".PadLeft 4)
        Console.WriteLine ()
    Console.WriteLine ()
    Console.ReadKey () |> ignore

multTable ()

```

```txt

 X     1   2   3   4   5   6   7   8   9  10  11  12
 ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___
  1    1   2   3   4   5   6   7   8   9  10  11  12
  2        4   6   8  10  12  14  16  18  20  22  24
  3            9  12  15  18  21  24  27  30  33  36
  4               16  20  24  28  32  36  40  44  48
  5                   25  30  35  40  45  50  55  60
  6                       36  42  48  54  60  66  72
  7                           49  56  63  70  77  84
  8                               64  72  80  88  96
  9                                   81  90  99 108
 10                                      100 110 120
 11                                          121 132
 12                                              144

```



## Factor


```factor
USING: io kernel math math.parser math.ranges sequences ;
IN: multiplication-table

: print-row ( n -- )
    [ number>string 2 CHAR: space pad-head write " |" write ]
    [ 1 - [ "    " write ] times ]
    [
        dup 12 [a,b]
        [ * number>string 4 CHAR: space pad-head write ] with each
    ] tri nl ;

: print-table ( -- )
    "    " write
    1 12 [a,b] [ number>string 4 CHAR: space pad-head write ] each nl
    "   +" write
    12 [ "----" write ] times nl
    1 12 [a,b] [ print-row ] each ;
```



```txt

       1   2   3   4   5   6   7   8   9  10  11  12
   +------------------------------------------------
 1 |   1   2   3   4   5   6   7   8   9  10  11  12
 2 |       4   6   8  10  12  14  16  18  20  22  24
 3 |           9  12  15  18  21  24  27  30  33  36
 4 |              16  20  24  28  32  36  40  44  48
 5 |                  25  30  35  40  45  50  55  60
 6 |                      36  42  48  54  60  66  72
 7 |                          49  56  63  70  77  84
 8 |                              64  72  80  88  96
 9 |                                  81  90  99 108
10 |                                     100 110 120
11 |                                         121 132
12 |                                             144

```



## FALSE


```false
[$100\>[" "]?$10\>[" "]?." "]p:
[$p;! m: 2[$m;\>]["    "1+]# [$13\>][$m;*p;!1+]#%"
"]l:
1[$13\>][$l;!1+]#%
```



## Fantom



```fantom

class Main
{
  static Void multiplicationTable (Int n)
  {
    // print column headings
    echo ("    |" + (1..n).map |Int a -> Str| { a.toStr.padl(4)}.join("") )
    echo ("-----" + (1..n).map { "----" }.join("") )
    // work through each row
    (1..n).each |i|
    {
      echo ( i.toStr.padl(4) + "|" +
             Str.spaces(4*(i-1)) +
             (i..n).map |Int j -> Str| { (i*j).toStr.padl(4)}.join("") )
    }
  }

  public static Void main ()
  {
    multiplicationTable (12)
  }
}

```



## Forth


```forth

: multiplication-table
  cr 2 spaces  13 2 do i 4 u.r loop
  cr
  13 2 do
    cr i 2 u.r
    13 2 do
      i j < if 4 spaces else i j * 4 u.r then
    loop
  loop ;

```



## Fortran

```fortran
program multtable
implicit none

  integer :: i, j, k

    write(*, "(a)") " x|   1   2   3   4   5   6   7   8   9  10  11  12"
    write(*, "(a)") "--+------------------------------------------------"
    do i = 1, 12
      write(*, "(i2, a)", advance="no") i, "|"
	do k = 2, i
    	  write(*, "(a4)", advance="no") ""
        end do
    	do j = i, 12
          write(*, "(i4)", advance="no") i*j
        end do
        write(*, *)
    end do

end program multtable
```



### Traditional approach

The usual style is to write whole lines at a go, traditionally to fast lineprinters. Producing a tabular layout is easy (four characters per field to allow room to print 144 with a space separator), the difficulty lies in having blank parts at the start of the line followed by results. Having results followed by blanks is normal. The simplest way to achieve this would be to have a CHARACTER*4 function IFMT4(n) that returns four spaces for n <= 0, otherwise the digits, similar to the above example. But the plan is to write a line of such function calls at a go (with n = 0 for unwanted results), and alas, very few Fortran implementations allow recursive use of the formatted I/O system - here one level would be inside the function to produce the result for N > 0, and the other is the original WRITE statement that invokes the function.

So instead, write the table by first writing a line to a CHARACTER variable then blanking out the unwanted part.

```Fortran

Cast forth a twelve times table, suitable for chanting at school.
      INTEGER I,J	!Steppers.
      CHARACTER*52 ALINE	!Scratchpad.
      WRITE(6,1) (I,I = 1,12)	!Present the heading.
    1 FORMAT ("  ×|",12I4,/," --+",12("----"))	!Alas, can't do overprinting with underlines now.
      DO 3 I = 1,12		!Step down the lines.
        WRITE (ALINE,2) I,(I*J, J = 1,12)	!Prepare one line.
    2   FORMAT (I3,"|",12I4)		!Aligned with the heading.
        ALINE(5:1 + 4*I) = ""		!Scrub the unwanted part.
    3   WRITE (6,"(A)") ALINE		!Print the text.
      END	!"One one is one! One two is two! One three is three!...

```

Output in the same style as above, with underlining unavailable: those who have used a lineprinter's overprint facility to properly underline find the flabby modern requirement of a second line vexing, but, few output devices support underlining in so easy a way.
  ×|   1   2   3   4   5   6   7   8   9  10  11  12
 --+------------------------------------------------
  1|   1   2   3   4   5   6   7   8   9  10  11  12
  2|       4   6   8  10  12  14  16  18  20  22  24
  3|           9  12  15  18  21  24  27  30  33  36
  4|              16  20  24  28  32  36  40  44  48
  5|                  25  30  35  40  45  50  55  60
  6|                      36  42  48  54  60  66  72
  7|                          49  56  63  70  77  84
  8|                              64  72  80  88  96
  9|                                  81  90  99 108
 10|                                     100 110 120
 11|                                         121 132
 12|                                             144
Going to the trouble of preparing results, and then blanking some might seem a little too crude. An alternative would be to use a different FORMAT statement for each line of output. But, a collection of a dozen output statements hardly represents a programming solution. Instead, create and then use the text of FORMAT statements, as follows. Notice that there are ''no reserved words'' in Fortran.

```Fortran

Cast forth a twelve times table, suitable for chanting at school.
      INTEGER I,J	!Steppers.
      CHARACTER*16 FORMAT	!Scratchpad.
      WRITE(6,1) (I,I = 1,12)	!Present the heading.
    1 FORMAT ("  ×|",12I4,/," --+",12("----"))	!Alas, can't do overprinting with underlines now.
      DO 3 I = 1,12		!Step down the lines.
        WRITE (FORMAT,2) (I - 1)*4,13 - I	!Spacing for omitted fields, count of wanted fields.
    2   FORMAT ("(I3,'|',",I0,"X,",I0,"I4)")	!The format of the FORMAT statement.
    3   WRITE (6,FORMAT) I,(I*J, J = I,12)	!Use it.
      END	!"One one is one! One two is two! One three is three!...

```

The output is the same, so instead, here are the generated FORMAT texts:
 (I3,'|',0X,12I4)
 (I3,'|',4X,11I4)
 (I3,'|',8X,10I4)
 (I3,'|',12X,9I4)
 (I3,'|',16X,8I4)
 (I3,'|',20X,7I4)
 (I3,'|',24X,6I4)
 (I3,'|',28X,5I4)
 (I3,'|',32X,4I4)
 (I3,'|',36X,3I4)
 (I3,'|',40X,2I4)
 (I3,'|',44X,1I4)
A zero count for spacing (the 0X, due to there being no omitted results on the first line) was possibly a weak point, but if not handled, the fallback position would have been to arrange that instead of 12I4 format, the first would be 1X,I3.

Some fortrans offer an extension to FORMAT statements, whereby a variable can appear in place of an integer constant, thus instead of say FORMAT (12I4) there could be FORMAT (<n>I4) for example. Then, during the interpretation of the FORMAT text, the current value of variable ''n'' would be accessed. Note that this is on-the-fly:
 READ(in,"(I2,<N>I4)") N,(A(I),I = 1,N)
would read N as a two-digit integer, and, as the READ statement executes further, use that value of N both in the FORMAT text's interpretation and in the further processing of the READ statement.


### VAX FORTRAN


```Fortran

      PROGRAM TABLES
      IMPLICIT NONE
C
C     Produce a formatted multiplication table of the kind memorised by rote
C     when in primary school. Only print the top half triangle of products.
C
C     23 Nov 15 - 0.1   - Adapted from original for VAX FORTRAN - MEJT
C
      INTEGER I,J,K                                             ! Counters.
      CHARACTER*32 S                                            ! Buffer for format specifier.
C
      K=12
C
      WRITE(S,1) K,K
    1 FORMAT(8H(4H0  |,,I2.2,11HI4,/,4H --+,I2.2,9H(4H----)))
      WRITE(6,S) (I,I = 1,K)                                    ! Print heading.
C
      DO 3 I=1,K		                                ! Step down the lines.
        WRITE(S,2) (I-1)*4+1,K                                  ! Update format string.
    2   FORMAT(12H(1H ,I2,1H|,,I2.2,5HX,I3,,I2.2,3HI4),8X)      ! Format string includes an explicit carridge control character.
        WRITE(6,S) I,(I*J, J = I,K)                             ! Use format to print row with leading blanks, unused fields are ignored.
    3 CONTINUE
C
      END

```
Based on the above code but with a slight modification as VAX FORTRAN doesn't allow zero width fields in a format statement. The number of rows and columns can also be altered by modifying the value of K which must be in the range 1 - 25.
===FORTRAN-IV===

```Fortran
      PROGRAM TABLES
C
C     Produce a formatted multiplication table of the kind memorised by rote
C     when in primary school. Only print the top half triangle of products.
C
C     23 Nov 15 - 0.1   - Adapted from original for VAX FORTRAN - MEJT
C     24 Nov 15 - 0.2   - FORTRAN IV version adapted from VAX FORTRAN and
C                         compiled using Microsoft FORTRAN-80 - MEJT
C
      DIMENSION K(12)
      DIMENSION A(6)
      DIMENSION L(12)
C
      COMMON //A
      EQUIVALENCE (A(1),L(1))
C
      DATA A/'(1H ',',I2,','1H|,','01X,','I3,1','2I4)'/
C
      WRITE(1,1) (I,I=1,12)
    1 FORMAT(4H0  |,12I4,/,4H --+12(4H----))
C
C     Overlaying the format specifier with an integer array makes it possibe
C     to modify the number of blank spaces.  The number of blank spaces is
C     stored as two consecuitive ASCII characters that overlay on the
C     integer value in L(7) in the ordr low byte, high byte.
C
      DO 3 I=1,12
        L(7)=(48+(I*4-3)-((I*4-3)/10)*10)*256+48+((I*4-3)/10)
        DO 2 J=1,12
          K(J)=I*J
    2   CONTINUE
        WRITE(1,A)I,(K(J), J = I,12)
    3 CONTINUE
C
      END
```
Rather more changes are needed to produce the same result, in particular we cannot modify the format specifier directly and have to rely on overlaying it with an integer array and calculating the ASCII values needed for each byte we need to modify. Nested implicit DO loops are allowed, but not used as it isn't possible to compute K on the fly so we have to calculate (and store) the results for each row before printing it. Note also that the unit numbers for the output devices are different and when using Hollerith strings to define values in a DATA statement the size of each string must match the size of the data type.

===Microsoft FORTRAN-80===
The use of a non standard(?) BYTE data type available in Microsoft FORTRAN-80 makes it easier to understand what is going on.

```Fortran
      PROGRAM TABLES
C
C     Produce a formatted multiplication table of the kind memorised by rote
C     when in primary school. Only print the top half triangle of products.
C
C     23 Nov 15 - 0.1   - Adapted from original for VAX FORTRAN - MEJT
C     24 Nov 15 - 0.2   - FORTRAN IV version adapted from VAX FORTRAN and
C                         compiled using Microsoft FORTRAN-80 - MEJT
C     25 Nov 15 - 0.3   - Microsoft FORTRAN-80 version using a BYTE array
C                         which makes it easier to understand what is going
C                         on. - MEJT
C
      BYTE A
      DIMENSION A(24)
      DIMENSION K(12)
C
      DATA A/'(','1','H',' ',',','I','2',',','1','H','|',',',
     +       '0','1','X',',','I','3',',','1','1','I','4',')'/
C
C     Print a heading and (try to) underline it.
C
      WRITE(1,1) (I,I=1,12)
    1 FORMAT(4H   |,12I4,/,4H --+12(4H----))
      DO 3 I=1,12
        A(13)=48+((I*4-3)/10)
        A(14)=48+(I*4-3)-((I*4-3)/10)*10
        DO 2 J=1,12
          K(J)=I*J
    2   CONTINUE
        WRITE(1,A)I,(K(J), J = I,12)
    3 CONTINUE
C
      END
```
Inserting the following two lines before the inner DO loop will print the format specifier used to print each row of the table.
```Fortran
        WRITE(1,4) (A(J), J = 1,24)
    4   FORMAT(1x,24A1)
```
Running the program produces the following output<lang>
  |   1   2   3   4   5   6   7   8   9  10  11  12
--+------------------------------------------------
 1|   1   2   3   4   5   6   7   8   9  10  11  12
 2|       4   6   8  10  12  14  16  18  20  22  24
 3|           9  12  15  18  21  24  27  30  33  36
 4|              16  20  24  28  32  36  40  44  48
 5|                  25  30  35  40  45  50  55  60
 6|                      36  42  48  54  60  66  72
 7|                          49  56  63  70  77  84
 8|                              64  72  80  88  96
 9|                                  81  90  99 108
10|                                     100 110 120
11|                                         121 132
12|                                             144
```



## FreeBASIC


```freebasic

' FB 1.05.0 Win64

Print "  X|";
For i As Integer = 1 To 12
  Print Using "####"; i;
Next

Print
Print "---+"; String(48, "-")

For i As Integer = 1 To 12
  Print Using "###"; i;
  Print"|"; Spc(4 * (i - 1));
  For j As Integer = i To 12
    Print Using "####"; i * j;
  Next j
  Print
Next i

Print
Print "Press any key to quit"
Sleep
```


```txt

  X|   1   2   3   4   5   6   7   8   9  10  11  12
---+------------------------------------------------
  1|   1   2   3   4   5   6   7   8   9  10  11  12
  2|       4   6   8  10  12  14  16  18  20  22  24
  3|           9  12  15  18  21  24  27  30  33  36
  4|              16  20  24  28  32  36  40  44  48
  5|                  25  30  35  40  45  50  55  60
  6|                      36  42  48  54  60  66  72
  7|                          49  56  63  70  77  84
  8|                              64  72  80  88  96
  9|                                  81  90  99 108
 10|                                     100 110 120
 11|                                         121 132
 12|                                             144

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=3a3a987766a9a9a383b3e0e8a65d9ea2 Click this link to run this code]'''

```gambas
'Code 'stolen' from Free Basic and altered to work in Gambas

Public Sub Main()
Dim i, j As Integer

Print "  X|";
For i = 1 To 12
  Print Format(i, "####");
Next

Print
Print "---+"; String(48, "-")

For i = 1 To 12
  Print Format(i, "###");
  Print "|"; Space(4 * (i - 1));
  For j = i To 12
    Print Format(i * j, "####");
  Next
  Print
Next

End
```

Output:

```txt

  X|   1   2   3   4   5   6   7   8   9  10  11  12
---+------------------------------------------------
  1|   1   2   3   4   5   6   7   8   9  10  11  12
  2|       4   6   8  10  12  14  16  18  20  22  24
  3|           9  12  15  18  21  24  27  30  33  36
  4|              16  20  24  28  32  36  40  44  48
  5|                  25  30  35  40  45  50  55  60
  6|                      36  42  48  54  60  66  72
  7|                          49  56  63  70  77  84
  8|                              64  72  80  88  96
  9|                                  81  90  99 108
 10|                                     100 110 120
 11|                                         121 132
 12|                                             144

```



## Go


```go

package main

import (
    "fmt"
)

func main() {
    fmt.Print(" x |")
    for i := 1; i <= 12; i++ {
        fmt.Printf("%4d", i)
    }
    fmt.Print("\n---+")
    for i := 1; i <= 12; i++ {
        fmt.Print("----")
    }
    for j := 1; j <= 12; j++ {
        fmt.Printf("\n%2d |", j)
        for i := 1; i <= 12; i++ {
            if i >= j {
                fmt.Printf("%4d", i*j)
            } else {
                fmt.Print("    ")
            }
        }
    }
    fmt.Println("")
}

```



## Groovy

Solution:

```groovy
def printMultTable = { size = 12 ->
    assert size > 1

    // factor1 line
    print '  |'; (1..size).each { f1 -> printf('%4d', f1) }; println ''

    // dividing line
    print '--+'; (1..size).each { printf('----', it) }; println ''

    // factor2 result lines
    (1..size).each { f2 ->
        printf('%2d|', f2)
        (1..<f2).each{ print '    ' }
        (f2..size).each{ f1 -> printf('%4d', f1*f2) }
        println ''
    }
}

printMultTable()
```


```txt
  |   1   2   3   4   5   6   7   8   9  10  11  12
--+------------------------------------------------
 1|   1   2   3   4   5   6   7   8   9  10  11  12
 2|       4   6   8  10  12  14  16  18  20  22  24
 3|           9  12  15  18  21  24  27  30  33  36
 4|              16  20  24  28  32  36  40  44  48
 5|                  25  30  35  40  45  50  55  60
 6|                      36  42  48  54  60  66  72
 7|                          49  56  63  70  77  84
 8|                              64  72  80  88  96
 9|                                  81  90  99 108
10|                                     100 110 120
11|                                         121 132
12|                                             144
```


=={{header|GW-BASIC}}==
```qbasic

10  ' Multiplication Tables
20  LET N% = 12
30  FOR J% = 1 TO N% - 1
40   PRINT USING "###"; J%;
50   PRINT " ";
60  NEXT J%
70  PRINT USING "###"; N%
80  FOR J% = 0 TO N% - 1
90   PRINT "----";
100 NEXT J%
110 PRINT "+"
120 FOR I% = 1 TO N%
130  FOR J% = 1 TO N%
140   IF J% < I% THEN PRINT "    "; ELSE PRINT USING "###"; I% * J%;: PRINT " ";
150  NEXT J%
160  PRINT "| "; USING "##"; I%
170 NEXT I%

```

```txt

  1   2   3   4   5   6   7   8   9  10  11  12
------------------------------------------------+
  1   2   3   4   5   6   7   8   9  10  11  12 |  1
      4   6   8  10  12  14  16  18  20  22  24 |  2
          9  12  15  18  21  24  27  30  33  36 |  3
             16  20  24  28  32  36  40  44  48 |  4
                 25  30  35  40  45  50  55  60 |  5
                     36  42  48  54  60  66  72 |  6
                         49  56  63  70  77  84 |  7
                             64  72  80  88  96 |  8
                                 81  90  99 108 |  9
                                    100 110 120 | 10
                                        121 132 | 11
                                            144 | 12

```



## Haskell


```haskell
import Data.Maybe (fromMaybe, maybe)
import Data.Bool (bool)

table :: [Int] -> [[Maybe Int]]
table xs =
  let axis = Just <$> xs
  in (Nothing : axis) :
     zipWith
       (:)
       axis
       [ [ bool (Just (x * y)) Nothing (x > y)
         | y <- xs ]
       | x <- xs ]

-- TEST ---------------------------------------------------
main :: IO ()
main =
  (putStrLn . unlines) $
  (showTable . table) <$> [[13 .. 20], [1 .. 12], [95 .. 100]]

-- FORMATTING ---------------------------------------------
showTable :: [[Maybe Int]] -> String
showTable xs =
  let w = 1 + (length . show) (fromMaybe 0 $ (last . last) xs)
      gap = replicate w ' '
      rows = (maybe gap (rjust w ' ' . show) =<<) <$> xs
  in unlines $ head rows : [] : tail rows

rjust :: Int -> Char -> String -> String
rjust n c = (drop . length) <*> (replicate n c ++)

```

```txt
      13  14  15  16  17  18  19  20

  13 169 182 195 208 221 234 247 260
  14     196 210 224 238 252 266 280
  15         225 240 255 270 285 300
  16             256 272 288 304 320
  17                 289 306 323 340
  18                     324 342 360
  19                         361 380
  20                             400

       1   2   3   4   5   6   7   8   9  10  11  12

   1   1   2   3   4   5   6   7   8   9  10  11  12
   2       4   6   8  10  12  14  16  18  20  22  24
   3           9  12  15  18  21  24  27  30  33  36
   4              16  20  24  28  32  36  40  44  48
   5                  25  30  35  40  45  50  55  60
   6                      36  42  48  54  60  66  72
   7                          49  56  63  70  77  84
   8                              64  72  80  88  96
   9                                  81  90  99 108
  10                                     100 110 120
  11                                         121 132
  12                                             144

          95    96    97    98    99   100

    95  9025  9120  9215  9310  9405  9500
    96        9216  9312  9408  9504  9600
    97              9409  9506  9603  9700
    98                    9604  9702  9800
    99                          9801  9900
   100                               10000
```


Or, more roughly and directly:

```haskell
import Data.List (groupBy)
import Data.Function (on)
import Control.Monad (join)

main :: IO ()
main =
  mapM_ print $
  fmap (uncurry (*)) <$>
  groupBy
    (on (==) fst)
    (filter (uncurry (>=)) $ join ((<*>) . fmap (,)) [1 .. 12])
```

```txt
[1]
[2,4]
[3,6,9]
[4,8,12,16]
[5,10,15,20,25]
[6,12,18,24,30,36]
[7,14,21,28,35,42,49]
[8,16,24,32,40,48,56,64]
[9,18,27,36,45,54,63,72,81]
[10,20,30,40,50,60,70,80,90,100]
[11,22,33,44,55,66,77,88,99,110,121]
[12,24,36,48,60,72,84,96,108,120,132,144]
```



## hexiscript


```hexiscript
fun format n l
  let n tostr n
  while len n < l; let n (" " + n); endwhile
  return n
endfun

print   "   |"
for let i 1; i <= 12; i++; print format i 4; endfor
print "\n --+"
for let i 1; i <= 12; i++; print "----"; endfor
println ""
for let i 1; i <= 12; i++
  print format i 3 + "|"
  for let j 1; j <= 12; j++
    if j < i; print "    "
    else print format (i * j) 4; endif
  endfor
  println ""
endfor
```



## HicEst


```HicEst
WRITE(Row=1) " x   1   2   3   4   5   6   7   8   9  10  11  12"
DO line = 1, 12
  WRITE(Row=line+2, Format='i2') line
  DO col = line, 12
    WRITE(Row=line+2, Column=4*col, Format='i3') line*col
  ENDDO
ENDDO
```



## HolyC

```holyc
U8 i, j, n = 12;
for (j = 1; j <= n; j++)
  if (j != n)
    Print("%3d%c", j, ' ');
  else
    Print("%3d%c", j, '\n');

for (j = 0; j <= n; j++)
  if (j != n)
    Print("----");
  else
    Print("+\n");

for (i = 1; i <= n; i++) {
  for (j = 1; j <= n; j++)
    if (j < i)
      Print("    ");
    else
      Print("%3d ", i * j);
  Print("| %d\n", i);
}
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
lim := 13
wid :=  5
every writes(right("* |" | (1 to lim) | "\n",wid)|right("\n",wid*(lim+1),"_"))         # header row and separator
every (i := 1 to lim) &
   writes(right( i||" |" | (j := 1 to lim, if j < i then "" else i*j) | "\n",wid))     # table content and triangle
end
```


The above example is a somewhat exaggerated example of contractions.
In both cases 'every' is used to force all alternatives including row labels, column headings, content, line terminators.
The upper triangle is produced by embedding an 'if' expression inside the object of an 'every' (normally an error prone construct which would malfunction if not carefully separated from the generators for 'i' and 'j' - an all too tempting possibility once you get into this mind set.)

```txt
 * |    1    2    3    4    5    6    7    8    9   10   11   12   13
_____________________________________________________________________
  1 |    1    2    3    4    5    6    7    8    9   10   11   12   13
  2 |         4    6    8   10   12   14   16   18   20   22   24   26
  3 |              9   12   15   18   21   24   27   30   33   36   39
  4 |                  16   20   24   28   32   36   40   44   48   52
  5 |                       25   30   35   40   45   50   55   60   65
  6 |                            36   42   48   54   60   66   72   78
  7 |                                 49   56   63   70   77   84   91
  8 |                                      64   72   80   88   96  104
  9 |                                           81   90   99  108  117
 10 |                                               100  110  120  130
 11 |                                                    121  132  143
 12 |                                                         144  156
 13 |                                                              169
```



## J


```j
   multtable=: <:/~ * */~
   format=: 'b4.0' 8!:2 ]
   (('*' ; ,.) ,. ({. ; ])@format@multtable) >:i.12
┌──┬────────────────────────────────────────────────┐
│* │   1   2   3   4   5   6   7   8   9  10  11  12│
├──┼────────────────────────────────────────────────┤
│ 1│   1   2   3   4   5   6   7   8   9  10  11  12│
│ 2│       4   6   8  10  12  14  16  18  20  22  24│
│ 3│           9  12  15  18  21  24  27  30  33  36│
│ 4│              16  20  24  28  32  36  40  44  48│
│ 5│                  25  30  35  40  45  50  55  60│
│ 6│                      36  42  48  54  60  66  72│
│ 7│                          49  56  63  70  77  84│
│ 8│                              64  72  80  88  96│
│ 9│                                  81  90  99 108│
│10│                                     100 110 120│
│11│                                         121 132│
│12│                                             144│
└──┴────────────────────────────────────────────────┘
```


That said, note that <code>*/~</code> is the core primitive used to construct a multiplication table and this is a general technique so that, for example, <code>+/~</code> would make an addition table.  The rest is just to make it look pretty (and to blank out the lower triangle -- we use a less than or equal table (<code><:/~</code>) to control that, and format zeros as spaces to blank them out).


## Java


```Java
public class MultiplicationTable {
    public static void main(String[] args) {
        for (int i = 1; i <= 12; i++)
            System.out.print("\t" + i);

        System.out.println();
        for (int i = 0; i < 100; i++)
            System.out.print("-");
        System.out.println();
        for (int i = 1; i <= 12; i++) {
            System.out.print(i + "|");
            for(int j = 1; j <= 12; j++) {
                System.out.print("\t");
                if (j >= i)
                    System.out.print("\t" + i * j);
            }
            System.out.println();
        }
    }
}
```

```txt

        1       2       3       4       5       6       7       8       9       10      11      12
----------------------------------------------------------------------------------------------------
1|      1       2       3       4       5       6       7       8       9       10      11      12
2|              4       6       8       10      12      14      16      18      20      22      24
3|                      9       12      15      18      21      24      27      30      33      36
4|                              16      20      24      28      32      36      40      44      48
5|                                      25      30      35      40      45      50      55      60
6|                                              36      42      48      54      60      66      72
7|                                                      49      56      63      70      77      84
8|                                                              64      72      80      88      96
9|                                                                      81      90      99      108
10|                                                                             100     110     120
11|                                                                                     121     132
12|                                                                                             144

```



## JavaScript



### Imperative



```html4strict
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
<title>12 times table</title>
<script type='text/javascript'>

    function multiplication_table(n, target) {
        var table = document.createElement('table');

        var row = document.createElement('tr');
        var cell = document.createElement('th');
        cell.appendChild(document.createTextNode('x'));
        row.appendChild(cell);
        for (var x = 1; x <=n; x++) {
            cell = document.createElement('th');
            cell.appendChild(document.createTextNode(x));
            row.appendChild(cell);
        }
        table.appendChild(row);

        for (var x = 1; x <=n; x++) {
            row = document.createElement('tr');
            cell = document.createElement('th');
            cell.appendChild(document.createTextNode(x));
            row.appendChild(cell);
            var y;
            for (y = 1; y < x; y++) {
                cell = document.createElement('td');
                cell.appendChild(document.createTextNode('\u00a0'));
                row.appendChild(cell);
            }
            for (; y <= n; y++) {
                cell = document.createElement('td');
                cell.appendChild(document.createTextNode(x*y));
                row.appendChild(cell);
            }
            table.appendChild(row);
        }
        target.appendChild(table);
    }

</script>
<style type='text/css'>
    body {font-family: sans-serif;}
    table {border-collapse: collapse;}
    th, td {border: 1px solid black; text-align: right; width: 4ex;}
</style>
</head>
<body onload="multiplication_table(12, document.getElementById('target'));">
<div id='target'></div>
</body>
</html>
```


{{out}} (minus the style):
<div><table><tr><th>x</th><th>1</th><th>2</th><th>3</th><th>4</th><th>5</th><th>6</th><th>7</th><th>8</th><th>9</th><th>10</th><th>11</th><th>12</th></tr><tr><th>1</th><td>1</td><td>2</td><td>3</td><td>4</td><td>5</td><td>6</td><td>7</td><td>8</td><td>9</td><td>10</td><td>11</td><td>12</td></tr><tr><th>2</th><td> </td><td>4</td><td>6</td><td>8</td><td>10</td><td>12</td><td>14</td><td>16</td><td>18</td><td>20</td><td>22</td><td>24</td></tr><tr><th>3</th><td> </td><td> </td><td>9</td><td>12</td><td>15</td><td>18</td><td>21</td><td>24</td><td>27</td><td>30</td><td>33</td><td>36</td></tr><tr><th>4</th><td> </td><td> </td><td> </td><td>16</td><td>20</td><td>24</td><td>28</td><td>32</td><td>36</td><td>40</td><td>44</td><td>48</td></tr><tr><th>5</th><td> </td><td> </td><td> </td><td> </td><td>25</td><td>30</td><td>35</td><td>40</td><td>45</td><td>50</td><td>55</td><td>60</td></tr><tr><th>6</th><td> </td><td> </td><td> </td><td> </td><td> </td><td>36</td><td>42</td><td>48</td><td>54</td><td>60</td><td>66</td><td>72</td></tr><tr><th>7</th><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td>49</td><td>56</td><td>63</td><td>70</td><td>77</td><td>84</td></tr><tr><th>8</th><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td>64</td><td>72</td><td>80</td><td>88</td><td>96</td></tr><tr><th>9</th><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td>81</td><td>90</td><td>99</td><td>108</td></tr><tr><th>10</th><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td>100</td><td>110</td><td>120</td></tr><tr><th>11</th><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td>121</td><td>132</td></tr><tr><th>12</th><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td>144</td></tr></table></div>


### Functional


### =ES5=


```JavaScript
(function (m, n) {

    // [m..n]
    function range(m, n) {
        return Array.apply(null, Array(n - m + 1)).map(function (x, i) {
            return m + i;
        });
    }

    // Monadic bind (chain) for lists
    function mb(xs, f) {
        return [].concat.apply([], xs.map(f));
    }

    var rng = range(m, n),

        lstTable = [['x'].concat(   rng )]
                         .concat(mb(rng,   function (x) {
        return       [[x].concat(mb(rng,   function (y) {

            return y < x ? [''] : [x * y];               // triangle only

    }))]}));

    /*                        FORMATTING OUTPUT                             */

    // [[a]] -> bool -> s -> s
    function wikiTable(lstRows, blnHeaderRow, strStyle) {
        return '{| class="wikitable" ' + (
            strStyle ? 'style="' + strStyle + '"' : ''
        ) + lstRows.map(function (lstRow, iRow) {
            var strDelim = ((blnHeaderRow && !iRow) ? '!' : '|');

            return '\n|-\n' + strDelim + ' ' + lstRow.map(function (v) {
                return typeof v === 'undefined' ? ' ' : v;
            }).join(' ' + strDelim + strDelim + ' ');
        }).join('') + '\n|}';
    }

    // Formatted as WikiTable
    return wikiTable(
        lstTable, true,
        'text-align:center;width:33em;height:33em;table-layout:fixed;'
    ) + '\n\n' +

    // or simply stringified as JSON
    JSON.stringify(lstTable);

})(1, 12);
```


{| class="wikitable" style="text-align:center;width:33em;height:33em;table-layout:fixed;"
|-
! x !! 1 !! 2 !! 3 !! 4 !! 5 !! 6 !! 7 !! 8 !! 9 !! 10 !! 11 !! 12
|-
| 1 || 1 || 2 || 3 || 4 || 5 || 6 || 7 || 8 || 9 || 10 || 11 || 12
|-
| 2 ||  || 4 || 6 || 8 || 10 || 12 || 14 || 16 || 18 || 20 || 22 || 24
|-
| 3 ||  ||  || 9 || 12 || 15 || 18 || 21 || 24 || 27 || 30 || 33 || 36
|-
| 4 ||  ||  ||  || 16 || 20 || 24 || 28 || 32 || 36 || 40 || 44 || 48
|-
| 5 ||  ||  ||  ||  || 25 || 30 || 35 || 40 || 45 || 50 || 55 || 60
|-
| 6 ||  ||  ||  ||  ||  || 36 || 42 || 48 || 54 || 60 || 66 || 72
|-
| 7 ||  ||  ||  ||  ||  ||  || 49 || 56 || 63 || 70 || 77 || 84
|-
| 8 ||  ||  ||  ||  ||  ||  ||  || 64 || 72 || 80 || 88 || 96
|-
| 9 ||  ||  ||  ||  ||  ||  ||  ||  || 81 || 90 || 99 || 108
|-
| 10 ||  ||  ||  ||  ||  ||  ||  ||  ||  || 100 || 110 || 120
|-
| 11 ||  ||  ||  ||  ||  ||  ||  ||  ||  ||  || 121 || 132
|-
| 12 ||  ||  ||  ||  ||  ||  ||  ||  ||  ||  ||  || 144
|}


```JavaScript
[["x",1,2,3,4,5,6,7,8,9,10,11,12],
 [1,1,2,3,4,5,6,7,8,9,10,11,12],
 [2,"",4,6,8,10,12,14,16,18,20,22,24],
 [3,"","",9,12,15,18,21,24,27,30,33,36],
 [4,"","","",16,20,24,28,32,36,40,44,48],
 [5,"","","","",25,30,35,40,45,50,55,60],
 [6,"","","","","",36,42,48,54,60,66,72],
 [7,"","","","","","",49,56,63,70,77,84],
 [8,"","","","","","","",64,72,80,88,96],
 [9,"","","","","","","","",81,90,99,108],
 [10,"","","","","","","","","",100,110,120],
 [11,"","","","","","","","","","",121,132],
 [12,"","","","","","","","","","","",144]]
```



### =ES6=


```JavaScript
(() => {

    // main :: () -> IO String
    const main = () =>
        wikiTable(
            multTable(1, 12),
            true,
            'text-align:center;width:33em;height:33em;table-layout:fixed;'
        );

    // multTable :: Int -> Int -> [[String]]
    const multTable = (m, n) => {
        const xs = enumFromToInt(m, n);
        return [
            ['x', ...xs],
            ...concatMap(
                x => [
                    [x, ...concatMap(
                        y => y < x ? [''] : [x * y],
                        xs
                    )]
                ],
                xs
            )
        ];
    };

    // GENERIC FUNCTIONS -----------------------------------------------------

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b
    });

    // Size of space -> filler Char -> String -> Centered String
    // center :: Int -> Char -> String -> String
    const center = (n, c, s) => {
        const
            qr = quotRem(n - s.length, 2),
            q = qr[0];
        return concat(concat([replicate(q, c), s, replicate(q + qr[1], c)]));
    };

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        xs.length > 0 ? (() => {
            const unit = typeof xs[0] === 'string' ? '' : [];
            return unit.concat.apply(unit, xs);
        })() : [];

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) => [].concat.apply([], xs.map(f));

    // enumFromToInt :: Int -> Int -> [Int]
    const enumFromToInt = (m, n) =>
        n >= m ? Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i) : [];

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // quotRem :: Int -> Int -> (Int, Int)
    const quotRem = (m, n) => Tuple(Math.floor(m / n), m % n);

    // replicate :: Int -> a -> [a]
    const replicate = (n, x) =>
        Array.from({
            length: n
        }, () => x);

    // FORMATTING ------------------------------------------------------------

    // wikiTable :: [[a]] -> Bool -> String -> String
    const wikiTable = (rows, blnHeader, style) =>
        '{| class="wikitable" ' + (
            style ? 'style="' + style + '"' : ''
        ) + rows.map((row, i) => {
            const dlm = ((blnHeader && !i) ? '!' : '|');
            return '\n|-\n' + dlm + ' ' + row.map(v =>
                    typeof v !== 'undefined' ? v : ' '
                )
                .join(' ' + dlm + dlm + ' ');
        })
        .join('') + '\n|}';

    // MAIN ------------------------------------------------------------------
    return main();
})();
```

{| class="wikitable" style="text-align:center;width:33em;height:33em;table-layout:fixed;"
|-
! x !! 1 !! 2 !! 3 !! 4 !! 5 !! 6 !! 7 !! 8 !! 9 !! 10 !! 11 !! 12
|-
| 1 || 1 || 2 || 3 || 4 || 5 || 6 || 7 || 8 || 9 || 10 || 11 || 12
|-
| 2 ||  || 4 || 6 || 8 || 10 || 12 || 14 || 16 || 18 || 20 || 22 || 24
|-
| 3 ||  ||  || 9 || 12 || 15 || 18 || 21 || 24 || 27 || 30 || 33 || 36
|-
| 4 ||  ||  ||  || 16 || 20 || 24 || 28 || 32 || 36 || 40 || 44 || 48
|-
| 5 ||  ||  ||  ||  || 25 || 30 || 35 || 40 || 45 || 50 || 55 || 60
|-
| 6 ||  ||  ||  ||  ||  || 36 || 42 || 48 || 54 || 60 || 66 || 72
|-
| 7 ||  ||  ||  ||  ||  ||  || 49 || 56 || 63 || 70 || 77 || 84
|-
| 8 ||  ||  ||  ||  ||  ||  ||  || 64 || 72 || 80 || 88 || 96
|-
| 9 ||  ||  ||  ||  ||  ||  ||  ||  || 81 || 90 || 99 || 108
|-
| 10 ||  ||  ||  ||  ||  ||  ||  ||  ||  || 100 || 110 || 120
|-
| 11 ||  ||  ||  ||  ||  ||  ||  ||  ||  ||  || 121 || 132
|-
| 12 ||  ||  ||  ||  ||  ||  ||  ||  ||  ||  ||  || 144
|}


## Jsish


```javascript
/* Multiplication tables, is Jsish */
var m, n, tableSize = 12;

if (console.args.length > 0) tableSize = parseInt(console.args[0]);
if (tableSize < 1 || tableSize > 20) tableSize = 12;

var width = String(tableSize * tableSize).length;
var spaces = ' '.repeat(width+1);

printf(spaces);
for (m = 1; m <= tableSize; m++) printf(' %*d', width, m);
printf('\n' + ' '.repeat(width) + '+');
printf('-'.repeat((width+1) * tableSize));
for (m = 1; m <= tableSize; m++) {
    printf('\n%*d|', width, m);
    for (n = m; n < m; n++) printf(spaces);
    for (n = 1; n <= tableSize; n++) {
        if (m <= n) printf(' %*d', width, m * n); else printf(spaces);
    }
}
printf('\n');
```


```txt
prompt$ jsish multiplication-tables.jsi
       1   2   3   4   5   6   7   8   9  10  11  12
   +------------------------------------------------
  1|   1   2   3   4   5   6   7   8   9  10  11  12
  2|       4   6   8  10  12  14  16  18  20  22  24
  3|           9  12  15  18  21  24  27  30  33  36
  4|              16  20  24  28  32  36  40  44  48
  5|                  25  30  35  40  45  50  55  60
  6|                      36  42  48  54  60  66  72
  7|                          49  56  63  70  77  84
  8|                              64  72  80  88  96
  9|                                  81  90  99 108
 10|                                     100 110 120
 11|                                         121 132
 12|                                             144

prompt$ jsish multiplication-tables.jsi 4
     1  2  3  4
  +------------
 1|  1  2  3  4
 2|     4  6  8
 3|        9 12
 4|          16
```



## Julia


```Julia
println(" X |   1   2   3   4   5   6   7   8   9  10  11  12")
println("---+------------------------------------------------")

for i=1:12, j=0:12
    if j == 0
        @printf("%2d | ", i)
    elseif i <= j
        @printf("%3d%c", i * j, j == 12 ? '\n' : ' ')
    else
        print("    ")
    end
end
```


```txt
 X |   1   2   3   4   5   6   7   8   9  10  11  12
---+------------------------------------------------
 1 |   1   2   3   4   5   6   7   8   9  10  11  12
 2 |       4   6   8  10  12  14  16  18  20  22  24
 3 |           9  12  15  18  21  24  27  30  33  36
 4 |              16  20  24  28  32  36  40  44  48
 5 |                  25  30  35  40  45  50  55  60
 6 |                      36  42  48  54  60  66  72
 7 |                          49  56  63  70  77  84
 8 |                              64  72  80  88  96
 9 |                                  81  90  99 108
10 |                                     100 110 120
11 |                                         121 132
12 |                                             144
```



## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    print("  x|")
    for (i in 1..12) print("%4d".format(i))
    println("\n---+${"-".repeat(48)}")
    for (i in 1..12) {
        print("%3d".format(i) +"|${" ".repeat(4 * i - 4)}")
        for (j in i..12) print("%4d".format(i * j))
        println()
    }
}
```


```txt

  x|   1   2   3   4   5   6   7   8   9  10  11  12
---+------------------------------------------------
  1|   1   2   3   4   5   6   7   8   9  10  11  12
  2|       4   6   8  10  12  14  16  18  20  22  24
  3|           9  12  15  18  21  24  27  30  33  36
  4|              16  20  24  28  32  36  40  44  48
  5|                  25  30  35  40  45  50  55  60
  6|                      36  42  48  54  60  66  72
  7|                          49  56  63  70  77  84
  8|                              64  72  80  88  96
  9|                                  81  90  99 108
 10|                                     100 110 120
 11|                                         121 132
 12|                                             144

```



## Lasso


```lasso
define printTimesTables(max::integer) => {
    local(result)  = ``
    local(padSize) = string(#max*#max)->size + 1

    // Print header row
    #result->append((' ' * #padSize) + '|')
    loop(#max) => {
        #result->append(loop_count->asString(-padding=#padSize))
    }
    #result->append("\n" + (`-` * #padSize) + '+' + (`-` * (#padSize * #max)))

    with left in 1 to #max do {
        // left column
        #result->append("\n" + #left->asString(-padding=#padSize) + '|')

        // Table results
        with right in 1 to #max do {
            #result->append(
                #right < #left
                    ? ' ' * #padSize
                    | (#left * #right)->asString(-padding=#padSize)
            )
        }
    }

    return #result
}

printTimesTables(12)
```


```txt
----+------------------------------------------------
   1|   1   2   3   4   5   6   7   8   9  10  11  12
   2|       4   6   8  10  12  14  16  18  20  22  24
   3|           9  12  15  18  21  24  27  30  33  36
   4|              16  20  24  28  32  36  40  44  48
   5|                  25  30  35  40  45  50  55  60
   6|                      36  42  48  54  60  66  72
   7|                          49  56  63  70  77  84
   8|                              64  72  80  88  96
   9|                                  81  90  99 108
  10|                                     100 110 120
  11|                                         121 132
  12|                                             144
```




## Liberty BASIC


```lb
Print "  |    1    2    3    4    5    6    7    8    9   10   11   12"
Print "--+------------------------------------------------------------"

For i = 1 To 12
    nums$ = Right$(" " + str$(i), 2) + "|"
    For ii = 1 To 12
        If i <= ii Then
            If ii >= 1 Then
                nums$ = nums$ + Left$("     ", (5 - Len(str$(i * ii))))
            End If
            nums$ = nums$ + str$(i * ii)
        Else
            nums$ = nums$ + "     "
        End If
    Next ii
    Print nums$
Next i
```

```txt

  |    1    2    3    4    5    6    7    8    9   10   11   12
--+------------------------------------------------------------
 1|    1    2    3    4    5    6    7    8    9   10   11   12
 2|         4    6    8   10   12   14   16   18   20   22   24
 3|              9   12   15   18   21   24   27   30   33   36
 4|                  16   20   24   28   32   36   40   44   48
 5|                       25   30   35   40   45   50   55   60
 6|                            36   42   48   54   60   66   72
 7|                                 49   56   63   70   77   84
 8|                                      64   72   80   88   96
 9|                                           81   90   99  108
10|                                               100  110  120
11|                                                    121  132
12|                                                         144

```



## Logo

```logo
to mult.table :n
  type "|  | for [i 2 :n] [type form :i 4 0] (print)
  (print)
  for [i 2 :n] [
    type form :i 2 0
    for [j 2 :n] [
      type ifelse :i > :j ["|    |] [form :i*:j 4 0]
    ]
    (print)
  ]
end

mult.table 12

```


## Lua


```lua
io.write( "   |" )
for i = 1, 12 do
    io.write( string.format( "%#5d", i ) )
end
io.write( "\n", string.rep( "-", 12*5+4 ), "\n" )

for i = 1, 12 do
    io.write( string.format( "%#2d |", i ) )

    for j = 1, 12 do
        if j < i then
            io.write( "     " )
        else
            io.write( string.format( "%#5d", i*j ) )
        end
    end
    io.write( "\n" )
end
```


```txt
   |    1    2    3    4    5    6    7    8    9   10   11   12
----------------------------------------------------------------
 1 |    1    2    3    4    5    6    7    8    9   10   11   12
 2 |         4    6    8   10   12   14   16   18   20   22   24
 3 |              9   12   15   18   21   24   27   30   33   36
 4 |                  16   20   24   28   32   36   40   44   48
 5 |                       25   30   35   40   45   50   55   60
 6 |                            36   42   48   54   60   66   72
 7 |                                 49   56   63   70   77   84
 8 |                                      64   72   80   88   96
 9 |                                           81   90   99  108
10 |                                               100  110  120
11 |                                                    121  132
12 |                                                         144
```



## M2000 Interpreter

Using jagged array (arrays of arrays)

```M2000 Interpreter

Module CheckIt {
      Dim Base 1, A(12)
      Mult=lambda (n)-> {
            Flush  ' empty stack
            For i=1 to n : Data i*n : Next i
            =Array([])   ' copy stack in an array, and return a pointer
      }
      i=Each(A())
      Print "  |";
      while i {
            Print Format$("{0:0:-4}",i^+1);
            A(i^+1)=Mult(i^+1)
      }
      Print
      Print "--+"+string$("-",4*12)
      For i=1 to 12 {
            Print Format$("{0:0:-2}|",i);
            For j=1 to 12 {
                  If len(A(j)())>=i then {
                        Print Format$("{0:0:-4}",A(j)(i-1));
                  } Else Print "    ";
            }
            Print
      }
}
CheckIt

```


Final loop can be this, using Each() and r1 as pointer to array.

```txt

For i=1 to 12 {
      j=Each(A())
      Print Format$("{0:0:-2}|",i);
      While j {
            r1=A(j^+1)
            If len(r1)>=i then {
                  Print Format$("{0:0:-4}",Array(r1,i-1));
            } Else Print "    ";
      }
      Print
}

```

```txt

   |   1   2   3   4   5   6   7   8   9  10  11  12
 --+------------------------------------------------
  1|   1   2   3   4   5   6   7   8   9  10  11  12
  2|       4   6   8  10  12  14  16  18  20  22  24
  3|           9  12  15  18  21  24  27  30  33  36
  4|              16  20  24  28  32  36  40  44  48
  5|                  25  30  35  40  45  50  55  60
  6|                      36  42  48  54  60  66  72
  7|                          49  56  63  70  77  84
  8|                              64  72  80  88  96
  9|                                  81  90  99 108
 10|                                     100 110 120
 11|                                         121 132
 12|                                             144

```



## Maple


```maple
printf("    ");
for i to 12 do
	printf("%-3d   ", i);
end do;
printf("\n");
for i to 75 do
	printf("-");
end do;
for i to 12 do
	printf("\n%2d| ", i);
	for j to 12 do
		if j<i then
			printf("      ");
		else
			printf("%-3d   ", i * j);
		end if
	end do
end do
```

```txt

    1     2     3     4     5     6     7     8     9     10    11    12
---------------------------------------------------------------------------
 1| 1     2     3     4     5     6     7     8     9     10    11    12
 2|       4     6     8     10    12    14    16    18    20    22    24
 3|             9     12    15    18    21    24    27    30    33    36
 4|                   16    20    24    28    32    36    40    44    48
 5|                         25    30    35    40    45    50    55    60
 6|                               36    42    48    54    60    66    72
 7|                                     49    56    63    70    77    84
 8|                                           64    72    80    88    96
 9|                                                 81    90    99    108
10|                                                       100   110   120
11|                                                             121   132
12|                                                                   144

```



## Mathematica


```Mathematica
Grid[{{Range[12]//Column,Grid[UpperTriangularize[KroneckerProduct[Range[12],Range[12]]]/.{0->""}]}}]
```

```txt

1 1	2	3	4	5	6	7	8	9	10	11	12
2	4	6	8	10	12	14	16	18	20	22	24
3		9	12	15	18	21	24	27	30	33	36
4			16	20	24	28	32	36	40	44	48
5				25	30	35	40	45	50	55	60
6					36	42	48	54	60	66	72
7						49	56	63	70	77	84
8							64	72	80	88	96
9								81	90	99	108
10									100	110	120
11										121	132
12											144

```


=={{header|MATLAB}} / {{header|Octave}}==
timesTable.m: (creates Times Table of N degree)


```MATLAB
function table = timesTable(N)
    table = [(0:N); (1:N)' triu( kron((1:N),(1:N)') )];
end
```


A minimally vectorized version of the above code:


```MATLAB
function table = timesTable(N)

    %Generates a column vector with integers from 1 to N
    rowLabels = (1:N)';

    %Generate a row vector with integers from 0 to N
    columnLabels = (0:N);

    %Generate the multiplication table using the kronecker tensor product
    %of two vectors one a column vector and the other a row vector
    table = kron((1:N),(1:N)');

    %Make it upper triangular and concatenate the rowLabels and
    %columnLabels to the table
    table = [columnLabels; rowLabels triu(table)];

end
```


For N=12:

```txt
timesTable(12)

ans =

     0     1     2     3     4     5     6     7     8     9    10    11    12
     1     1     2     3     4     5     6     7     8     9    10    11    12
     2     0     4     6     8    10    12    14    16    18    20    22    24
     3     0     0     9    12    15    18    21    24    27    30    33    36
     4     0     0     0    16    20    24    28    32    36    40    44    48
     5     0     0     0     0    25    30    35    40    45    50    55    60
     6     0     0     0     0     0    36    42    48    54    60    66    72
     7     0     0     0     0     0     0    49    56    63    70    77    84
     8     0     0     0     0     0     0     0    64    72    80    88    96
     9     0     0     0     0     0     0     0     0    81    90    99   108
    10     0     0     0     0     0     0     0     0     0   100   110   120
    11     0     0     0     0     0     0     0     0     0     0   121   132
    12     0     0     0     0     0     0     0     0     0     0     0   144

```



## Maxima


```Maxima
for i: 1 thru 12 do (
  for j: 1 thru 12 do (
    if j>=i or j=1 then printf(true, "~4d", i*j) else printf(true, "    ")
    ),
  printf(true, "~%")
  );
```


=={{header|MK-61/52}}==
<lang>П0	КИП0	КИП4	КИП5	ИП4	ИП5	*	С/П
ИП5	ИП0	-	x=0	03
ИП4	ИП0	-	x#0	22	ИП4	П5	БП	02
С/П
```


''Input'': 12 С/П ...

{{out}} (compiled):

```txt

    1   2   3   4   5   6   7   8   9  10  11  12
        4   6   8  10  12  14  16  18  20  22  24
            9  12  15  18  21  24  27  30  33  36
               16  20  24  28  32  36  40  44  48
                   25  30  35  40  45  50  55  60
                       36  42  48  54  60  66  72
                           49  56  63  70  77  84
                               64  72  80  88  96
                                   81  90  99 108
                                      100 110 120
                                          121 132
                                              144

```



## Microsoft Small Basic

```microsoftsmallbasic

n = 12
For j = 1 To n - 1
  TextWindow.CursorLeft = (j - 1) * 4 + (3 - Text.GetLength(j))
  TextWindow.Write(j)
  TextWindow.Write(" ")
EndFor
TextWindow.CursorLeft = (n - 1) * 4 + (3 - Text.GetLength(n))
TextWindow.Write(n)
TextWindow.WriteLine("")
For j = 0 To n - 1
  TextWindow.Write("----")
EndFor
TextWindow.WriteLine("+")
For i = 1 To n
  For j = 1 To n
    If j < i Then
      TextWindow.Write("    ")
    Else
      TextWindow.CursorLeft = (j - 1) * 4 + (3 - Text.GetLength(i * j))
      TextWindow.Write(i * j)
      TextWindow.Write(" ")
    EndIf
  EndFor
  TextWindow.Write("| ")
  TextWindow.CursorLeft = n * 4 + (4 - Text.GetLength(i))
  TextWindow.Write(i)
  TextWindow.WriteLine("")
EndFor

```

```txt

  1   2   3   4   5   6   7   8   9  10  11  12
------------------------------------------------+
  1   2   3   4   5   6   7   8   9  10  11  12 |  1
      4   6   8  10  12  14  16  18  20  22  24 |  2
          9  12  15  18  21  24  27  30  33  36 |  3
             16  20  24  28  32  36  40  44  48 |  4
                 25  30  35  40  45  50  55  60 |  5
                     36  42  48  54  60  66  72 |  6
                         49  56  63  70  77  84 |  7
                             64  72  80  88  96 |  8
                                 81  90  99 108 |  9
                                    100 110 120 | 10
                                        121 132 | 11
                                            144 | 12

```


=={{header|Modula-2}}==
```modula2

MODULE MultiplicationTables;

FROM SWholeIO IMPORT
  WriteInt;
FROM STextIO IMPORT
  WriteString, WriteLn;

CONST
  N = 12;

VAR
  I, J: INTEGER;

BEGIN
  FOR J := 1 TO N - 1 DO
    WriteInt(J, 3);
    WriteString(" ");
  END;
  WriteInt(N, 3);
  WriteLn;
  FOR J := 0 TO N - 1 DO
    WriteString("----");
  END;
  WriteString("+");
  WriteLn;
  FOR I := 1 TO N DO
    FOR J := 1 TO N DO
      IF J < I THEN
        WriteString("    ");
      ELSE
        WriteInt(I * J, 3);
        WriteString(" ");
      END;
    END;
    WriteString("| ");
    WriteInt(I, 2);
    WriteLn;
  END;
END MultiplicationTables.

```

```txt

  1   2   3   4   5   6   7   8   9  10  11  12
------------------------------------------------+
  1   2   3   4   5   6   7   8   9  10  11  12 |  1
      4   6   8  10  12  14  16  18  20  22  24 |  2
          9  12  15  18  21  24  27  30  33  36 |  3
             16  20  24  28  32  36  40  44  48 |  4
                 25  30  35  40  45  50  55  60 |  5
                     36  42  48  54  60  66  72 |  6
                         49  56  63  70  77  84 |  7
                             64  72  80  88  96 |  8
                                 81  90  99 108 |  9
                                    100 110 120 | 10
                                        121 132 | 11
                                            144 | 12

```



## MUMPS


```MUMPS
MULTTABLE(SIZE)
 ;Print out a multiplication table
 ;SIZE is the size of the multiplication table to make
 ;MW is the maximum width of the numbers
 ;D is the down axis
 ;A is the across axis
 ;BAR is the horizontal bar under the operands
 NEW MW,D,A,BAR
 IF $DATA(SIZE)<1 SET SIZE=12
 SET MW=$LENGTH(SIZE*SIZE)
 SET BAR="" FOR I=1:1:(MW+2) SET BAR=BAR_"-"
 FOR D=1:1:(SIZE+2) DO
 .FOR A=1:1:(SIZE+1) DO
 ..WRITE:(D=1)&(A=1) !,$JUSTIFY("",MW-1)," X|"
 ..WRITE:(D=1)&(A>1) ?((A-1)*5),$JUSTIFY((A-1),MW)
 ..WRITE:(D=2)&(A=1) !,BAR
 ..WRITE:(D=2)&(A'=1) BAR
 ..WRITE:(D>2)&(A=1) !,$JUSTIFY((D-2),MW)," |"
 ..WRITE:((A-1)>=(D-2))&((D-2)>=1) ?((A-1)*5),$JUSTIFY((D-2)*(A-1),MW)
 KILL MW,D,A,BAR
 QUIT
```


```txt
USER>D MULTTABLE^ROSETTA

   X|  1    2    3    4    5    6    7    8    9   10   11   12
-----------------------------------------------------------------
  1 |  1    2    3    4    5    6    7    8    9   10   11   12
  2 |       4    6    8   10   12   14   16   18   20   22   24
  3 |            9   12   15   18   21   24   27   30   33   36
  4 |                16   20   24   28   32   36   40   44   48
  5 |                     25   30   35   40   45   50   55   60
  6 |                          36   42   48   54   60   66   72
  7 |                               49   56   63   70   77   84
  8 |                                    64   72   80   88   96
  9 |                                         81   90   99  108
 10 |                                             100  110  120
 11 |                                                  121  132
 12 |                                                       144
```



## MOO

This quick example is designed to demonstrate raw MOO.  In other words it does not use any of the helper functions available in popular DBs such as LambdaMOO.

```moo

@verb me:@tables none none none rxd
@program me:@tables
player:tell("    |      1    2    3    4    5    6    7    8    9   10   11   12");
player:tell("-------------------------------------------------------------------");
for i in [1..12]
  line = ((i < 10) ? "  " | " ") + tostr(i) + " |  ";
  for j in [1..12]
    if (j >= i)
      product = i * j;
      "calculate spacing for right justification of values";
      if (product >= 100)
        spacer = "";
      elseif (product >= 10)
        spacer = " ";
      else
        spacer = "  ";
      endif
      line = line + "  " + spacer + tostr(product);
    else
      line = line + "     ";
    endif
  endfor
  player:tell(line);
endfor
.

```


LambdaMOO string utilities version:

```moo

@program me:@tables
player:tell("    |      1    2    3    4    5    6    7    8    9   10   11   12");
player:tell($string_utils:space(67, "-"));
for i in [1..12]
  line = " " + $string_utils:right(i, 2) + " |  ";
  for j in [1..12]
    line = line + "  " + ((i > j) ? "   " | $string_utils:right(j*i, 3));
  endfor
  player:tell(line);
endfor
.

```

```txt

@tables
    |      1    2    3    4    5    6    7    8    9   10   11   12
-------------------------------------------------------------------
  1 |      1    2    3    4    5    6    7    8    9   10   11   12
  2 |           4    6    8   10   12   14   16   18   20   22   24
  3 |                9   12   15   18   21   24   27   30   33   36
  4 |                    16   20   24   28   32   36   40   44   48
  5 |                         25   30   35   40   45   50   55   60
  6 |                              36   42   48   54   60   66   72
  7 |                                   49   56   63   70   77   84
  8 |                                        64   72   80   88   96
  9 |                                             81   90   99  108
 10 |                                                 100  110  120
 11 |                                                      121  132
 12 |                                                           144

```



## Neko


```ActionScript
/**
 Multiplication table, in Neko
 Tectonics:
   nekoc multiplication-table.neko
   neko multiplication-table
*/

var sprintf = $loader.loadprim("std@sprintf", 2);

var i, j;

i = 1;
$print("  X |");
while i < 13 {
  $print(sprintf("%4d", i));
  i += 1;
}
$print("\n");
$print(" ---+");
i = 1;
while i < 13 {
  $print("----");
  i += 1;
}
$print("\n");

j = 1;
while j < 13 {
  $print(sprintf("%3d", j));
  $print(" |");
  i = 1;
  while i < 13 {
    if j > i {
      $print("    ");
    } else {
      $print(sprintf("%4d", i*j));
    }
    i += 1;
  }
  $print("\n");
  j += 1;
}
```


```txt
prompt$ nekoc multiplication-table.neko
prompt$ neko multiplication-table
  X |   1   2   3   4   5   6   7   8   9  10  11  12
 ---+------------------------------------------------
  1 |   1   2   3   4   5   6   7   8   9  10  11  12
  2 |       4   6   8  10  12  14  16  18  20  22  24
  3 |           9  12  15  18  21  24  27  30  33  36
  4 |              16  20  24  28  32  36  40  44  48
  5 |                  25  30  35  40  45  50  55  60
  6 |                      36  42  48  54  60  66  72
  7 |                          49  56  63  70  77  84
  8 |                              64  72  80  88  96
  9 |                                  81  90  99 108
 10 |                                     100 110 120
 11 |                                         121 132
 12 |                                             144

```



## Nim

```nim
import strfmt

const n = 12

for j in 1..n:
  stdout.write "{:3d}{:s}".fmt(j, if n-j>0: " " else: "\n")
for j in 0..n:
  stdout.write if n-j>0: "----" else: "+\n"
for i in 1..n:
  for j in 1..n:
    stdout.write if j<i: "    " else: "{:3d} ".fmt(i*j)
  echo "| {:2d}".fmt(i)
```

```txt
  1   2   3   4   5   6   7   8   9  10  11  12
------------------------------------------------+
  1   2   3   4   5   6   7   8   9  10  11  12 |  1
      4   6   8  10  12  14  16  18  20  22  24 |  2
          9  12  15  18  21  24  27  30  33  36 |  3
             16  20  24  28  32  36  40  44  48 |  4
                 25  30  35  40  45  50  55  60 |  5
                     36  42  48  54  60  66  72 |  6
                         49  56  63  70  77  84 |  7
                             64  72  80  88  96 |  8
                                 81  90  99 108 |  9
                                    100 110 120 | 10
                                        121 132 | 11
                                            144 | 12
```



## OCaml

```ocaml
let () =
  let max = 12 in
  let fmax = float_of_int max in

  let dgts = int_of_float (ceil (log10 (fmax *. fmax))) in
  let fmt = Printf.printf " %*d" dgts in
  let fmt2 = Printf.printf "%*s%c" dgts in

  fmt2 "" 'x';
  for i = 1 to max do fmt i done;
  print_string "\n\n";

  for j = 1 to max do
    fmt j;
    for i = 1 to pred j do fmt2 "" ' '; done;
    for i = j to max do fmt (i*j); done;
    print_newline()
  done;
  print_newline()
```



## PARI/GP

Quick and dirty one-liner:

```parigp
for(y=1,12,printf("%2Ps| ",y);for(x=1,12,print1(if(y>x,"",x*y)"\t"));print)
```



## Pascal

See [[Multiplication_tables#Delphi | Delphi]]


## Perl


```perl
our $max = 12;
our $width = length($max**2) + 1;

printf "%*s", $width, $_ foreach 'x|', 1..$max;
print "\n", '-' x ($width - 1), '+', '-' x ($max*$width), "\n";
foreach my $i (1..$max) {
	printf "%*s", $width, $_
            foreach "$i|", map { $_ >= $i and $_*$i } 1..$max;
	print "\n";
}
```


```txt

  x|   1   2   3   4   5   6   7   8   9  10  11  12
---+------------------------------------------------
  1|   1   2   3   4   5   6   7   8   9  10  11  12
  2|       4   6   8  10  12  14  16  18  20  22  24
  3|           9  12  15  18  21  24  27  30  33  36
  4|              16  20  24  28  32  36  40  44  48
  5|                  25  30  35  40  45  50  55  60
  6|                      36  42  48  54  60  66  72
  7|                          49  56  63  70  77  84
  8|                              64  72  80  88  96
  9|                                  81  90  99 108
 10|                                     100 110 120
 11|                                         121 132
 12|                                             144

```



## Perl 6

```perl6
my $max = 12;
my $width = chars $max**2;
my $f = "%{$width}s";

say 'x'.fmt($f), '│ ', (1..$max).fmt($f);
say '─' x $width, '┼', '─' x $max*$width + $max;
for 1..$max -> $i {
    say $i.fmt($f), '│ ', (
        for 1..$max -> $j {
            $i <= $j ?? $i*$j !! '';
        }
    ).fmt($f);
}
```


```txt

  x│   1   2   3   4   5   6   7   8   9  10  11  12
───┼────────────────────────────────────────────────
  1│   1   2   3   4   5   6   7   8   9  10  11  12
  2│       4   6   8  10  12  14  16  18  20  22  24
  3│           9  12  15  18  21  24  27  30  33  36
  4│              16  20  24  28  32  36  40  44  48
  5│                  25  30  35  40  45  50  55  60
  6│                      36  42  48  54  60  66  72
  7│                          49  56  63  70  77  84
  8│                              64  72  80  88  96
  9│                                  81  90  99 108
 10│                                     100 110 120
 11│                                         121 132
 12│                                             144

```



## Phix

```Phix
printf(1,"  | ")
for col=1 to 12 do
    printf(1,"%4d",col)
end for
printf(1,"\n--+-"&repeat('-',12*4))
for row=1 to 12 do
    printf(1,"\n%2d| ",row)
    for col=1 to 12 do
        printf(1,iff(col<row?"    ":sprintf("%4d",row*col)))
    end for
end for
```

<pre style="font-size: 8px">
  |    1   2   3   4   5   6   7   8   9  10  11  12
--+-------------------------------------------------
 1|    1   2   3   4   5   6   7   8   9  10  11  12
 2|        4   6   8  10  12  14  16  18  20  22  24
 3|            9  12  15  18  21  24  27  30  33  36
 4|               16  20  24  28  32  36  40  44  48
 5|                   25  30  35  40  45  50  55  60
 6|                       36  42  48  54  60  66  72
 7|                           49  56  63  70  77  84
 8|                               64  72  80  88  96
 9|                                   81  90  99 108
10|                                      100 110 120
11|                                          121 132
12|                                              144

```


## PL/I


```PL/I

/* 12 x 12 multiplication table. */

multiplication_table: procedure options (main);
   declare (i, j) fixed decimal (2);

   put skip edit ((i do i = 1 to 12)) (X(4), 12 F(4));
   put skip edit ( (49)'_') (X(3), A);

   do i = 1 to 12;
      put skip edit (i, ' |', (i*j do j = i to 12))
         (F(2), a, col(i*4+1), 12 F(4));
   end;

end multiplication_table;

```


Result:

<lang>
       1   2   3   4   5   6   7   8   9  10  11  12
   _________________________________________________
 1 |   1   2   3   4   5   6   7   8   9  10  11  12
 2 |       4   6   8  10  12  14  16  18  20  22  24
 3 |           9  12  15  18  21  24  27  30  33  36
 4 |              16  20  24  28  32  36  40  44  48
 5 |                  25  30  35  40  45  50  55  60
 6 |                      36  42  48  54  60  66  72
 7 |                          49  56  63  70  77  84
 8 |                              64  72  80  88  96
 9 |                                  81  90  99 108
10 |                                     100 110 120
11 |                                         121 132
12 |                                             144

```



## PicoLisp


```PicoLi/th>sp
(de mulTable (N)
   (space 4)
   (for X N
      (prin (align 4 X)) )
   (prinl)
   (prinl)
   (for Y N
      (prin (align 4 Y))
      (space (* (dec Y) 4))
      (for (X Y (>= N X) (inc X))
         (prin (align 4 (* X Y))) )
      (prinl) ) )

(mulTable 12)
```

```txt
       1   2   3   4   5   6   7   8   9  10  11  12

   1   1   2   3   4   5   6   7   8   9  10  11  12
   2       4   6   8  10  12  14  16  18  20  22  24
   3           9  12  15  18  21  24  27  30  33  36
   4              16  20  24  28  32  36  40  44  48
   5                  25  30  35  40  45  50  55  60
   6                      36  42  48  54  60  66  72
   7                          49  56  63  70  77  84
   8                              64  72  80  88  96
   9                                  81  90  99 108
  10                                     100 110 120
  11                                         121 132
  12                                             144
```



## PowerShell


```powershell
#  For clarity
$Tab = "`t"

#  Create top row
$Tab + ( 1..12 -join $Tab )

#  For each row
ForEach ( $i in 1..12 )
    {
    $(  #  The number in the left column
        $i

        #  An empty slot for the bottom triangle
        @( "" ) * ( $i - 1 )

        #  Calculate the top triangle
        $i..12 | ForEach { $i * $_ }

        #  Combine them all together
        ) -join $Tab
    }
```

```txt
	1	2	3	4	5	6	7	8	9	10	11	12
1	1	2	3	4	5	6	7	8	9	10	11	12
2		4	6	8	10	12	14	16	18	20	22	24
3			9	12	15	18	21	24	27	30	33	36
4				16	20	24	28	32	36	40	44	48
5					25	30	35	40	45	50	55	60
6						36	42	48	54	60	66	72
7							49	56	63	70	77	84
8								64	72	80	88	96
9									81	90	99	108
10										100	110	120
11											121	132
12												144
```

<b>A more general solution</b>

```powershell
function Get-TimesTable ( [int]$Size )
    {
    #  For clarity
    $Tab = "`t"

    #  Create top row
    $Tab + ( 1..$Size -join $Tab )

    #  For each row
    ForEach ( $i in 1..$Size )
        {
        $(  #  The number in the left column
            $i

            #  An empty slot for the bottom triangle
            @( "" ) * ( $i - 1 )

            #  Calculate the top triangle
            $i..$Size | ForEach { $i * $_ }

         #  Combine them all together (and send them to the out put stream, which in PowerShell implicityly returns them)
         ) -join $Tab
        }
    }

Get-TimesTable 18
```

```txt
	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18
1	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	17	18
2		4	6	8	10	12	14	16	18	20	22	24	26	28	30	32	34	36
3			9	12	15	18	21	24	27	30	33	36	39	42	45	48	51	54
4				16	20	24	28	32	36	40	44	48	52	56	60	64	68	72
5					25	30	35	40	45	50	55	60	65	70	75	80	85	90
6						36	42	48	54	60	66	72	78	84	90	96	102	108
7							49	56	63	70	77	84	91	98	105	112	119	126
8								64	72	80	88	96	104	112	120	128	136	144
9									81	90	99	108	117	126	135	144	153	162
10										100	110	120	130	140	150	160	170	180
11											121	132	143	154	165	176	187	198
12												144	156	168	180	192	204	216
13													169	182	195	208	221	234
14														196	210	224	238	252
15															225	240	255	270
16																256	272	288
17																	289	306
18																		324
```



## PureBasic


```PureBasic
Procedure PrintMultiplicationTable(maxx, maxy)
  sp       = Len(Str(maxx*maxy)) + 1
  trenner$ =  "+"
  For l1 = 1 To maxx + 1
    For l2 = 1 To sp
      trenner$ + "-"
    Next
    trenner$ + "+"
  Next
  header$ = "|" + RSet("x", sp) + "|"
  For a = 1 To maxx
    header$ + RSet(Str(a), sp)
    header$ + "|"
  Next
  PrintN(trenner$)
  PrintN(header$)
  PrintN(trenner$)
  For y = 1 To maxy
    line$ = "|" + RSet(Str(y), sp) + "|"
    For x = 1 To maxx
      If x >= y
        line$ + RSet(Str(x*y), sp)
      Else
        line$ + Space(sp)
      EndIf
      line$ + "|"
    Next
    PrintN(line$)
  Next
  PrintN(trenner$)
EndProcedure

OpenConsole()
PrintMultiplicationTable(12, 12)
Input()
```


Ouput similar to ALGOL 68


## Python


### Procedural


```python>>>
 size = 12
>>> width = len(str(size**2))
>>> for row in range(-1,size+1):
	if row==0:
		print("─"*width + "┼"+"─"*((width+1)*size-1))
	else:
		print("".join("%*s%1s" % ((width,) + (("x","│")      if row==-1 and col==0
					              else (row,"│") if row>0   and col==0
					              else (col,"")  if row==-1
					              else ("","")   if row>col
					              else (row*col,"")))
			       for col in range(size+1)))


  x│  1   2   3   4   5   6   7   8   9  10  11  12
───┼───────────────────────────────────────────────
  1│  1   2   3   4   5   6   7   8   9  10  11  12
  2│      4   6   8  10  12  14  16  18  20  22  24
  3│          9  12  15  18  21  24  27  30  33  36
  4│             16  20  24  28  32  36  40  44  48
  5│                 25  30  35  40  45  50  55  60
  6│                     36  42  48  54  60  66  72
  7│                         49  56  63  70  77  84
  8│                             64  72  80  88  96
  9│                                 81  90  99 108
 10│                                    100 110 120
 11│                                        121 132
 12│                                            144
>>>
```


The above works with Python 3.X, which uses Unicode strings by default.

Declaring a file type of UTF-8 and adding a u to all string literals to transform them into Unicode literals would make the above work in Python 2.X.
<small>(As would using ASCII minus, plus, and pipe characters: "-", "+", "|"; instead of the non-ASCII chars used to draw a frame)</small>.


### Functional


We can define a multiplication table string first in terms of a '''list comprehension''' (''mulTable'' function),

and then again, for comparison, as an equivalent '''list monad''' expression (''mulTable2'' function):


```python
'''Multiplication table

   1. by list comprehension (mulTable ),
   2. by list monad.        (mulTable2)'''

from itertools import chain


# mulTable :: Int -> String
def mulTable(n):
    '''A multiplication table of dimension n,
       without redundant entries beneath
       the diagonal of squares.'''

    # colWidth :: Int
    colWidth = len(str(n * n))

    # pad :: String -> String
    def pad(s):
        return s.rjust(colWidth, ' ')

    xs = enumFromTo(1)(n)
    return unlines([
        pad(str(y) + ':') + unwords([
            pad(str(x * y) if x >= y else '')
            for x in xs
        ]) for y in xs
    ])


# mulTable2 :: Int -> String
def mulTable2(n):
    '''Identical to mulTable above,
       but the list comprehension is directly
       desugared to an equivalent list monad expression.'''

    # colWidth :: Int
    colWidth = len(str(n * n))

    # pad :: String -> String
    def pad(s):
        return s.rjust(colWidth, ' ')

    xs = enumFromTo(1)(n)
    return unlines(
        bind(xs)(lambda y: [
            pad(str(y) + ':') + unwords(
                bind(xs)(lambda x: [
                    pad(str(x * y) if x >= y else '')
                ])
            )
        ])
    )


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Test'''

    for s, f in [
            ('list comprehension', mulTable),
            ('list monad', mulTable2)
    ]:
        print(
            'By ' + s + ' (' + f.__name__ + '):\n\n',
            f(12).strip() + '\n'
        )


# GENERIC -------------------------------------------------

# bind (>>=) :: [a] -> (a -> [b]) -> [b]
def bind(xs):
    '''The injection operator for the list monad.
       Equivalent to concatMap with its arguments flipped.'''
    return lambda f: list(
        chain.from_iterable(
            map(f, xs)
        )
    )


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# unlines :: [String] -> String
def unlines(xs):
    '''A newline-delimited string derived from a list of lines.'''
    return '\n'.join(xs)


# unwords :: [String] -> String
def unwords(xs):
    '''A space-delimited string derived from a list of words.'''
    return ' '.join(xs)


if __name__ == '__main__':
    main()
```

```txt
By list comprehension (mulTable):

 1:  1   2   3   4   5   6   7   8   9  10  11  12
 2:      4   6   8  10  12  14  16  18  20  22  24
 3:          9  12  15  18  21  24  27  30  33  36
 4:             16  20  24  28  32  36  40  44  48
 5:                 25  30  35  40  45  50  55  60
 6:                     36  42  48  54  60  66  72
 7:                         49  56  63  70  77  84
 8:                             64  72  80  88  96
 9:                                 81  90  99 108
10:                                    100 110 120
11:                                        121 132
12:                                            144

By list monad (mulTable2):

 1:  1   2   3   4   5   6   7   8   9  10  11  12
 2:      4   6   8  10  12  14  16  18  20  22  24
 3:          9  12  15  18  21  24  27  30  33  36
 4:             16  20  24  28  32  36  40  44  48
 5:                 25  30  35  40  45  50  55  60
 6:                     36  42  48  54  60  66  72
 7:                         49  56  63  70  77  84
 8:                             64  72  80  88  96
 9:                                 81  90  99 108
10:                                    100 110 120
11:                                        121 132
12:                                            144
```



Or, with a little more abstraction, and a complete separation of model from view:
```python
'''Generalised multiplication tables'''

import collections
import itertools
import inspect


# table :: Int -> [[Maybe Int]]
def table(xs):
    '''An option-type model of a multiplication table:
       a tabulation of Just(x * y) values for all
       pairings (x, y) of integers in xs where x > y,
       and Nothing values where y <= x.
    '''
    axis = fmap(Just)(xs)
    return list(cons(
        cons(Nothing())(axis)
    )(zipWith(cons)(axis)([
        [
            Nothing() if y > x else Just(x * y)
            for x in xs
        ]
        for y in xs
    ])))


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Test'''
    print('\n\n'.join(
        fmap(fmap(fmap(showTable)(table))(
            liftA2(enumFromTo)(fst)(snd)
        ))(
            [(13, 20), (1, 12), (95, 100)]
        )
    ))


# DISPLAY -------------------------------------------------

# showTable :: [[Maybe Int]] -> String
def showTable(xs):
    '''A stringification of an abstract model
       of a multiplication table.
    '''
    w = 1 + len(str(last(last(xs))['Just']))
    gap = ' ' * w
    rows = fmap(fmap(concat)(
        fmap(maybe(gap)(
            fmap(justifyRight(w)(' '))(str)
        ))
    ))(xs)
    return unlines([rows[0]] + [''] + rows[1:])


# GENERIC -------------------------------------------------

# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': True}


# concat :: [[a]] -> [a]
# concat :: [String] -> String
def concat(xs):
    '''The concatenation of all the elements
       in a list or iterable.'''
    chain = itertools.chain

    def f(ys):
        zs = list(chain(*ys))
        return ''.join(zs) if isinstance(ys[0], str) else zs

    return (
        f(xs) if isinstance(xs, list) else (
            chain.from_iterable(xs)
        )
    ) if xs else []


# cons :: a -> [a] -> [a]
def cons(x):
    '''Construction of a list from x as head,
       and xs as tail.'''
    chain = itertools.chain
    return lambda xs: [x] + xs if (
        isinstance(xs, list)
    ) else chain([x], xs)


# curry :: ((a, b) -> c) -> a -> b -> c
def curry(f):
    '''A curried function derived
       from an uncurried function.'''
    signature = inspect.signature
    if 1 < len(signature(f).parameters):
        return lambda x: lambda y: f(x, y)
    else:
        return f


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# fmap :: Functor f => (a -> b) -> f a -> f b
def fmap(f):
    '''A function f mapped over a functor.'''
    def go(x):
        defaultdict = collections.defaultdict
        return defaultdict(list, [
            ('list', fmapList),
            # ('iter', fmapNext),
            # ('Either', fmapLR),
            # ('Maybe', fmapMay),
            # ('Tree', fmapTree),
            # ('tuple', fmapTuple),
            ('function', fmapFn),
            ('type', fmapFn)
        ])[
            typeName(x)
        ](f)(x)
    return lambda v: go(v)


# fmapFn :: (a -> b) -> (r -> a) -> r -> b
def fmapFn(f):
    '''fmap over a function.
       The composition of f and g.
    '''
    return lambda g: lambda x: f(g(x))


# fmapList :: (a -> b) -> [a] -> [b]
def fmapList(f):
    '''fmap over a list.
       f lifted to a function over a list.
    '''
    return lambda xs: list(map(f, xs))


# fst :: (a, b) -> a
def fst(tpl):
    '''First member of a pair.'''
    return tpl[0]


# justifyRight :: Int -> Char -> String -> String
def justifyRight(n):
    '''A string padded at left to length n,
       using the padding character c.
    '''
    return lambda c: lambda s: s.rjust(n, c)


# last :: [a] -> a
def last(xs):
    '''The last element of a non-empty list.'''
    return xs[-1]


# liftA2 :: (a -> b -> c) -> f a -> f b -> f c
def liftA2(f):
    '''Lift a binary function to the type of a.'''
    def go(a, b):
        defaultdict = collections.defaultdict
        return defaultdict(list, [
            # ('list', liftA2List),
            # ('Either', liftA2LR),
            # ('Maybe', liftA2May),
            # ('Tree', liftA2Tree),
            # ('tuple', liftA2Tuple),
            ('function', liftA2Fn)
        ])[
            typeName(a)
        ](f)(a)(b)
    return lambda a: lambda b: go(a, b)


# liftA2Fn :: (a0 -> b -> c) -> (a -> a0) -> (a -> b) -> a -> c
def liftA2Fn(op):
    '''Lift a binary function to a composition
       over two other functions.
       liftA2 (*) (+ 2) (+ 3) 7 == 90
    '''
    def go(f, g):
        return lambda x: curry(op)(
            f(x)
        )(g(x))
    return lambda f: lambda g: go(f, g)


# maybe :: b -> (a -> b) -> Maybe a -> b
def maybe(v):
    '''Either the default value v, if m is Nothing,
       or the application of f to x,
       where m is Just(x).
    '''
    return lambda f: lambda m: v if m.get('Nothing') else (
        f(m.get('Just'))
    )


# typeName :: a -> String
def typeName(x):
    '''Name string for a built-in or user-defined type.
       Selector for type-specific instances
       of polymorphic functions.
    '''
    if isinstance(x, dict):
        return x.get('type') if 'type' in x else 'dict'
    else:
        return 'iter' if hasattr(x, '__next__') else (
            type(x).__name__
        )


# snd :: (a, b) -> b
def snd(tpl):
    '''Second member of a pair.'''
    return tpl[1]


# uncurry :: (a -> b -> c) -> ((a, b) -> c)
def uncurry(f):
    '''A function over a pair of arguments,
       derived from a vanilla or curried function.
    '''
    signature = inspect.signature
    if 1 < len(signature(f).parameters):
        return lambda xy: f(*xy)
    else:
        return lambda x, y: f(x)(y)


# unlines :: [String] -> String
def unlines(xs):
    '''A single string derived by the intercalation
       of a list of strings with the newline character.
    '''
    return '\n'.join(xs)


# zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
def zipWith(f):
    '''A list constructed by zipping with a
       custom function, rather than with the
       default tuple constructor.
    '''
    return lambda xs: lambda ys: (
        map(uncurry(f), xs, ys)
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
      13  14  15  16  17  18  19  20

  13 169 182 195 208 221 234 247 260
  14     196 210 224 238 252 266 280
  15         225 240 255 270 285 300
  16             256 272 288 304 320
  17                 289 306 323 340
  18                     324 342 360
  19                         361 380
  20                             400

       1   2   3   4   5   6   7   8   9  10  11  12

   1   1   2   3   4   5   6   7   8   9  10  11  12
   2       4   6   8  10  12  14  16  18  20  22  24
   3           9  12  15  18  21  24  27  30  33  36
   4              16  20  24  28  32  36  40  44  48
   5                  25  30  35  40  45  50  55  60
   6                      36  42  48  54  60  66  72
   7                          49  56  63  70  77  84
   8                              64  72  80  88  96
   9                                  81  90  99 108
  10                                     100 110 120
  11                                         121 132
  12                                             144

          95    96    97    98    99   100

    95  9025  9120  9215  9310  9405  9500
    96        9216  9312  9408  9504  9600
    97              9409  9506  9603  9700
    98                    9604  9702  9800
    99                          9801  9900
   100                               10000
```



## R


```r

multiplication_table <- function(n=12)
{
   one_to_n <- 1:n
   x <- matrix(one_to_n) %*% t(one_to_n)
   x[lower.tri(x)] <- 0
   rownames(x) <- colnames(x) <- one_to_n
   print(as.table(x), zero.print="")
   invisible(x)
}
multiplication_table()

```



## Racket



```Racket

#lang racket

(define (show-line xs)
  (for ([x xs]) (display (~a x #:width 4 #:align 'right)))
  (newline))

(show-line (cons "" (range 1 13)))
(for ([y (in-range 1 13)])
  (show-line (cons y (for/list ([x (in-range 1 13)])
                       (if (<= y x) (* x y) "")))))

```


```txt

       1   2   3   4   5   6   7   8   9  10  11  12
   1   1   2   3   4   5   6   7   8   9  10  11  12
   2       4   6   8  10  12  14  16  18  20  22  24
   3           9  12  15  18  21  24  27  30  33  36
   4              16  20  24  28  32  36  40  44  48
   5                  25  30  35  40  45  50  55  60
   6                      36  42  48  54  60  66  72
   7                          49  56  63  70  77  84
   8                              64  72  80  88  96
   9                                  81  90  99 108
  10                                     100 110 120
  11                                         121 132
  12                                             144

```



## REBOL


```REBOL
REBOL [
	Title: "12x12 Multiplication Table"
	URL: http://rosettacode.org/wiki/Print_a_Multiplication_Table
]

size: 12

; Because of REBOL's GUI focus, it doesn't really do pictured output,
; so I roll my own. See Formatted_Numeric_Output for more
; comprehensive version:

pad: func [pad n][
    n: to-string n
    insert/dup n " " (pad - length? n)
    n
]
p3: func [v][pad 3 v]  ; A shortcut, I hate to type...

--: has [x][repeat x size + 1 [prin "+---"]  print "+"]  ; Special chars OK.

.row: func [label y /local row x][
	row: reduce ["|" label "|"]
	repeat x size [append row reduce [either x < y ["   "][p3 x * y] "|"]]
	print rejoin row
]

--  .row " x " 1  --  repeat y size [.row  p3 y  y]  --

print rejoin [ crlf  "What about "  size: 5  "?"  crlf ]
--  .row " x " 1  --  repeat y size [.row  p3 y  y]  --

print rejoin [ crlf  "How about "  size: 20  "?"  crlf ]
--  .row " x " 1  --  repeat y size [.row  p3 y  y]  --
```


{{out}} (only 12x12 shown):

```txt
+---+---+---+---+---+---+---+---+---+---+---+---+---+
| x |  1|  2|  3|  4|  5|  6|  7|  8|  9| 10| 11| 12|
+---+---+---+---+---+---+---+---+---+---+---+---+---+
|  1|  1|  2|  3|  4|  5|  6|  7|  8|  9| 10| 11| 12|
|  2|   |  4|  6|  8| 10| 12| 14| 16| 18| 20| 22| 24|
|  3|   |   |  9| 12| 15| 18| 21| 24| 27| 30| 33| 36|
|  4|   |   |   | 16| 20| 24| 28| 32| 36| 40| 44| 48|
|  5|   |   |   |   | 25| 30| 35| 40| 45| 50| 55| 60|
|  6|   |   |   |   |   | 36| 42| 48| 54| 60| 66| 72|
|  7|   |   |   |   |   |   | 49| 56| 63| 70| 77| 84|
|  8|   |   |   |   |   |   |   | 64| 72| 80| 88| 96|
|  9|   |   |   |   |   |   |   |   | 81| 90| 99|108|
| 10|   |   |   |   |   |   |   |   |   |100|110|120|
| 11|   |   |   |   |   |   |   |   |   |   |121|132|
| 12|   |   |   |   |   |   |   |   |   |   |   |144|
+---+---+---+---+---+---+---+---+---+---+---+---+---+

```



## REXX

A lot of the program code deals with the presentation/display of the grid for the multiplication table.

```REXX
/*REXX program displays a  NxN  multiplication table  (in a boxed grid) to the terminal.*/
parse arg high .                                 /*obtain optional grid size from the CL*/
if high=='' | high==","  then high=12            /*Not specified?  Then use the default.*/
            bar  = '│'   ;    dash = "─"         /*(vertical) bar; horizontal bar (dash)*/
            bj   = '┴'   ;    tj   = "┬"         /*bottom and top junctions  (or  tees).*/
            cj   = '┼'                           /*center junction  (or cross).         */
            lj   = '├'   ;    rj   = "┤"         /*left and right junctions  (or  tees).*/
            tlc  = '┌'   ;    trc  = "┐"         /* top     left and right corners.     */
            blc  = '└'   ;    brc  = "┘"         /*bottom     "   "    "      "         */
cell = cj || copies(dash,max(5,length(high) +1)) /*define the  top  of the cell.        */
sep  = copies(cell, high+1)rj                    /*construct the table separator.       */
size = length(cell) - 1                          /*width for the products in the table. */
box. = left('', size)                            /*initialize all the cells in the table*/
            do j=0  to  high                     /*step through  zero  ───►  high.      */
            _=right(j, size - 2)'x '             /*build the   "label" (border) number. */
            box.0.j=center(_,        size)       /*  "    "     top label cell.         */
            box.j.0=center(_, max(5, size) )     /*  "    "    left label cell.         */
            end   /*j*/
box.0.0=center('times', max(5, size))            /*redefine    box.0.0   with  "times". */
            do   r=1   for high                  /*step through row      one ───► high. */
              do c=r    to high                  /*step through column   row ───► high. */
              box.r.c=right(r*c, size)           /*build a single multiplication cell.  */
              end   /*c*/
            end     /*r*/                        /*only build the top right-half of grid*/

         do r=0  to high;  @=sep;  L=length(sep) /*step through all lines; use a mod sep*/
         if r==0  then do; @=overlay(tlc, @ , 1) /*use a better tlc (top  left corner). */
                           @=overlay(trc, @ , L) /* "  "    "   trc ( "  right    "  ). */
                           @=translate(@, tj,cj) /* "  "    "   tj  (top  junction/tee).*/
                       end
                  else @=overlay(lj, @, 1)       /* "  "    "   lj  (left junction/tee).*/
         say @                                   /*display a single table grid line.    */
         if r==0  then call buildLine 00         /*   "    "    "   blank grid   "      */
                       call buildLine r          /*build a single line of the grid.     */
         if r==0  then call buildLine 00         /*display a single blank grid line.    */
         end   /*r*/
@=sep                                            /*allow use of a modified separator.   */
@=overlay(blc, @ ,  1)                           /*use a better  bottom   left corner.  */
@=overlay(brc, @ , length(sep) )                 /* "  "    "       "    right corner.  */
@=translate(@, bj, cj)                           /* "  "    "       "      junction.    */
say @                                            /*display a (single)  table grid line. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
buildLine: parse arg row,,$;    do col=0  to high       /*step through  zero ───► high. */
                                $=$ ||bar ||box.row.col /*build one cell at a time.     */
                                end   /*col*/           /* [↑]  build (row) line by cols*/
           say $ || bar; return                         /*finish building the last cell.*/
```

'''output'''   when using the default input:

```txt

┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
│     │     │     │     │     │     │     │     │     │     │     │     │     │
│times│  1x │  2x │  3x │  4x │  5x │  6x │  7x │  8x │  9x │ 10x │ 11x │ 12x │
│     │     │     │     │     │     │     │     │     │     │     │     │     │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  1x │   1 │   2 │   3 │   4 │   5 │   6 │   7 │   8 │   9 │  10 │  11 │  12 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  2x │     │   4 │   6 │   8 │  10 │  12 │  14 │  16 │  18 │  20 │  22 │  24 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  3x │     │     │   9 │  12 │  15 │  18 │  21 │  24 │  27 │  30 │  33 │  36 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  4x │     │     │     │  16 │  20 │  24 │  28 │  32 │  36 │  40 │  44 │  48 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  5x │     │     │     │     │  25 │  30 │  35 │  40 │  45 │  50 │  55 │  60 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  6x │     │     │     │     │     │  36 │  42 │  48 │  54 │  60 │  66 │  72 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  7x │     │     │     │     │     │     │  49 │  56 │  63 │  70 │  77 │  84 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  8x │     │     │     │     │     │     │     │  64 │  72 │  80 │  88 │  96 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  9x │     │     │     │     │     │     │     │     │  81 │  90 │  99 │ 108 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│ 10x │     │     │     │     │     │     │     │     │     │ 100 │ 110 │ 120 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│ 11x │     │     │     │     │     │     │     │     │     │     │ 121 │ 132 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│ 12x │     │     │     │     │     │     │     │     │     │     │     │ 144 │
└─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┘

```

'''output'''   when the following is used for input:   <tt> 16 </tt>

```txt

┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
│     │     │     │     │     │     │     │     │     │     │     │     │     │     │     │     │     │
│times│  1x │  2x │  3x │  4x │  5x │  6x │  7x │  8x │  9x │ 10x │ 11x │ 12x │ 13x │ 14x │ 15x │ 16x │
│     │     │     │     │     │     │     │     │     │     │     │     │     │     │     │     │     │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  1x │   1 │   2 │   3 │   4 │   5 │   6 │   7 │   8 │   9 │  10 │  11 │  12 │  13 │  14 │  15 │  16 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  2x │     │   4 │   6 │   8 │  10 │  12 │  14 │  16 │  18 │  20 │  22 │  24 │  26 │  28 │  30 │  32 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  3x │     │     │   9 │  12 │  15 │  18 │  21 │  24 │  27 │  30 │  33 │  36 │  39 │  42 │  45 │  48 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  4x │     │     │     │  16 │  20 │  24 │  28 │  32 │  36 │  40 │  44 │  48 │  52 │  56 │  60 │  64 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  5x │     │     │     │     │  25 │  30 │  35 │  40 │  45 │  50 │  55 │  60 │  65 │  70 │  75 │  80 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  6x │     │     │     │     │     │  36 │  42 │  48 │  54 │  60 │  66 │  72 │  78 │  84 │  90 │  96 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  7x │     │     │     │     │     │     │  49 │  56 │  63 │  70 │  77 │  84 │  91 │  98 │ 105 │ 112 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  8x │     │     │     │     │     │     │     │  64 │  72 │  80 │  88 │  96 │ 104 │ 112 │ 120 │ 128 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│  9x │     │     │     │     │     │     │     │     │  81 │  90 │  99 │ 108 │ 117 │ 126 │ 135 │ 144 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│ 10x │     │     │     │     │     │     │     │     │     │ 100 │ 110 │ 120 │ 130 │ 140 │ 150 │ 160 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│ 11x │     │     │     │     │     │     │     │     │     │     │ 121 │ 132 │ 143 │ 154 │ 165 │ 176 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│ 12x │     │     │     │     │     │     │     │     │     │     │     │ 144 │ 156 │ 168 │ 180 │ 192 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│ 13x │     │     │     │     │     │     │     │     │     │     │     │     │ 169 │ 182 │ 195 │ 208 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│ 14x │     │     │     │     │     │     │     │     │     │     │     │     │     │ 196 │ 210 │ 224 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│ 15x │     │     │     │     │     │     │     │     │     │     │     │     │     │     │ 225 │ 240 │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
│ 16x │     │     │     │     │     │     │     │     │     │     │     │     │     │     │     │ 256 │
└─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┘

```



## Ring


```ring

multiplication_table(12)
func multiplication_table n
  nSize = 4   See "    |   "
  for t = 1 to n see  fsize(t, nSize) next
  see nl + "----+-" + copy("-", nSize*n) + nl
  for t1 = 1 to n
     see fsize(t1, nSize) + "|   "
     for t2 = 1 to n if t2 >= t1 see  fsize(t1*t2,nSize) else see copy(" ", nSize) ok next
     see nl
  next
func fsize x,n return string(x) + copy(" ",n-len(string(x)))

```


Output

```ring

    |   1   2   3   4   5   6   7   8   9   10  11  12
----+-------------------------------------------------
1   |   1   2   3   4   5   6   7   8   9   10  11  12
2   |       4   6   8   10  12  14  16  18  20  22  24
3   |           9   12  15  18  21  24  27  30  33  36
4   |               16  20  24  28  32  36  40  44  48
5   |                   25  30  35  40  45  50  55  60
6   |                       36  42  48  54  60  66  72
7   |                           49  56  63  70  77  84
8   |                               64  72  80  88  96
9   |                                   81  90  99  108
10  |                                       100 110 120
11  |                                           121 132
12  |                                               144

```



## Ruby


```ruby
def multiplication_table(n)
  puts "    |" + (" %3d" * n) % [*1..n]
  puts "----+" + "----" * n
  1.upto(n) do |x|
    print "%3d |" % x
    1.upto(x-1) {|y| print "    "}
    x.upto(n)   {|y| print " %3d" % (x*y)}
    puts
  end
end

multiplication_table 12
```


```txt

    |   1   2   3   4   5   6   7   8   9  10  11  12
----+------------------------------------------------
  1 |   1   2   3   4   5   6   7   8   9  10  11  12
  2 |       4   6   8  10  12  14  16  18  20  22  24
  3 |           9  12  15  18  21  24  27  30  33  36
  4 |              16  20  24  28  32  36  40  44  48
  5 |                  25  30  35  40  45  50  55  60
  6 |                      36  42  48  54  60  66  72
  7 |                          49  56  63  70  77  84
  8 |                              64  72  80  88  96
  9 |                                  81  90  99 108
 10 |                                     100 110 120
 11 |                                         121 132
 12 |                                             144

```


## Run BASIC


```Runbasic
html "<TABLE border=1 ><TR bgcolor=silver align=center><TD><TD>1<TD>2<TD>3<TD>4<TD>5<TD>6<TD>7<TD>8<TD>9<TD>10<TD>11<TD>12</td></TR>"
For i = 1 To 12
	html "<TR align=right><TD>";i;"</td>"
	For ii = 1 To 12
		html "<td width=25>"
		If ii >= i Then   html i * ii
		html "</td>"
	Next ii
next i
html "</table>"

```
Output:
<TABLE border=1 ><TR bgcolor=silver align=center><TD><TD>1<TD>2<TD>3<TD>4<TD>5<TD>6<TD>7<TD>8<TD>9<TD>10<TD>11<TD>12</td></TR><TR align=right><TD>1</td><td width=25>1</td><td width=25>2</td><td width=25>3</td><td width=25>4</td><td width=25>5</td><td width=25>6</td><td width=25>7</td><td width=25>8</td><td width=25>9</td><td width=25>10</td><td width=25>11</td><td width=25>12</td><TR align=right><TD>2</td><td width=25></td><td width=25>4</td><td width=25>6</td><td width=25>8</td><td width=25>10</td><td width=25>12</td><td width=25>14</td><td width=25>16</td><td width=25>18</td><td width=25>20</td><td width=25>22</td><td width=25>24</td><TR align=right><TD>3</td><td width=25></td><td width=25></td><td width=25>9</td><td width=25>12</td><td width=25>15</td><td width=25>18</td><td width=25>21</td><td width=25>24</td><td width=25>27</td><td width=25>30</td><td width=25>33</td><td width=25>36</td><TR align=right><TD>4</td><td width=25></td><td width=25></td><td width=25></td><td width=25>16</td><td width=25>20</td><td width=25>24</td><td width=25>28</td><td width=25>32</td><td width=25>36</td><td width=25>40</td><td width=25>44</td><td width=25>48</td><TR align=right><TD>5</td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25>25</td><td width=25>30</td><td width=25>35</td><td width=25>40</td><td width=25>45</td><td width=25>50</td><td width=25>55</td><td width=25>60</td><TR align=right><TD>6</td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25>36</td><td width=25>42</td><td width=25>48</td><td width=25>54</td><td width=25>60</td><td width=25>66</td><td width=25>72</td><TR align=right><TD>7</td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25>49</td><td width=25>56</td><td width=25>63</td><td width=25>70</td><td width=25>77</td><td width=25>84</td><TR align=right><TD>8</td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25>64</td><td width=25>72</td><td width=25>80</td><td width=25>88</td><td width=25>96</td><TR align=right><TD>9</td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25>81</td><td width=25>90</td><td width=25>99</td><td width=25>108</td><TR align=right><TD>10</td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25>100</td><td width=25>110</td><td width=25>120</td><TR align=right><TD>11</td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25>121</td><td width=25>132</td><TR align=right><TD>12</td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25></td><td width=25>144</td></table>


## Rust


```rust
const LIMIT: i32 = 12;

fn main() {
    for i in 1..LIMIT+1 {
        print!("{:3}{}", i, if LIMIT - i == 0 {'\n'} else {' '})
    }
    for i in 0..LIMIT+1 {
        print!("{}", if LIMIT - i == 0 {"+\n"} else {"----"});
    }

    for i in 1..LIMIT+1 {
        for j in 1..LIMIT+1 {
            if j < i {
                print!("    ")
            } else {
                print!("{:3} ", j * i)
            }
        }
        println!("| {}", i);
    }


}
```



## Scala


```scala

//Multiplication Table
print("%5s".format("|"))
for (i <- 1 to 12) print("%5d".format(i))
println()
println("-----" * 13)

for (i <- 1 to 12) {
  print("%4d|".format(i))

  for (j <- 1 to 12) {
    if (i <= j)
      print("%5d".format(i * j))
    else
      print("%5s".format(""))
  }

  println("")
}

```



###  case


```scala

implicit def intToString(i: Int) = i.toString
val cell = (x:String) => print("%5s".format(x))

for {
  i <- 1 to 14
  j <- 1 to 14
}
yield {
  (i, j) match {
    case (i, 13) => cell("|")
    case (i, 14) if i > 12 => cell("\n")
    case (13, j) => cell("-----")
    case (i, 14) => cell(i + "\n")
    case (14, j) => cell(j)
    case (i, j) if i <= j => cell(i*j)
    case (i, j) => cell("-")
  }
}

```



## Scheme


A better implementation of <tt>iota</tt> is provided by SRFI-1 [http://srfi.schemers.org/srfi-1/srfi-1.html].


```scheme

(define iota
  (lambda (count start step)
    (let loop ((result (list (+ start (* (- count 1) step)))))
      (let ((acc (car result)))
        (if (= acc start)
            result
            (loop (cons (- acc step) result)))))))


(define table
  (lambda (x)
    (let loop ((count 1)
               (numbers (iota x 1 1)))
      (if (not (null? numbers))
          (begin
            (display (make-string (* 6 (- count 1)) #\space))
            (for-each
             (lambda (n)
               (let ((number (number->string (* n count))))
                 (display (string-append
                           (make-string (- 6 (string-length number)) #\space)
                           number))))
             numbers)
            (newline)
            (loop (+ count 1)
                  (cdr numbers)))))))

```



```txt

(table 12)
     1     2     3     4     5     6     7     8     9    10    11    12
           4     6     8    10    12    14    16    18    20    22    24
                 9    12    15    18    21    24    27    30    33    36
                      16    20    24    28    32    36    40    44    48
                            25    30    35    40    45    50    55    60
                                  36    42    48    54    60    66    72
                                        49    56    63    70    77    84
                                              64    72    80    88    96
                                                    81    90    99   108
                                                         100   110   120
                                                               121   132
                                                                     144

```



## Scilab

<lang>    nmax=12, xx=3
    s= blanks(xx)+" |"
    for j=1:nmax
        s=s+part(blanks(xx)+string(j),$-xx:$)
    end
    printf("%s\n",s)
    s=strncpy("-----",xx)+" +"
    for j=1:nmax
        s=s+" "+strncpy("-----",xx)
    end
    printf("%s\n",s)
    for i=1:nmax
        s=part(blanks(xx)+string(i),$-xx+1:$)+" |"
        for j = 1:nmax
            if j >= i then
                s=s+part(blanks(xx)+string(i*j),$-xx:$)
            else
                s=s+blanks(xx+1)
	    end
        end
        printf("%s\n",s)
    end
```

```txt
    |   1   2   3   4   5   6   7   8   9  10  11  12
--- + --- --- --- --- --- --- --- --- --- --- --- ---
  1 |   1   2   3   4   5   6   7   8   9  10  11  12
  2 |       4   6   8  10  12  14  16  18  20  22  24
  3 |           9  12  15  18  21  24  27  30  33  36
  4 |              16  20  24  28  32  36  40  44  48
  5 |                  25  30  35  40  45  50  55  60
  6 |                      36  42  48  54  60  66  72
  7 |                          49  56  63  70  77  84
  8 |                              64  72  80  88  96
  9 |                                  81  90  99 108
 10 |                                     100 110 120
 11 |                                         121 132
 12 |                                             144
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    const integer: n is 12;
    var integer: i is 0;
    var integer: j is 0;
  begin
    for j range 1 to n do
      write(j lpad 3 <& " ");
    end for;
    writeln;
    writeln("-" mult 4 * n);
    for i range 1 to n do
      for j range 1 to n do
        if j < i then
          write("    ");
        else
          write(i * j lpad 3 <& " ");
        end if;
      end for;
      writeln("|" <& i lpad 3);
    end for;
  end func;
```


```txt

  1   2   3   4   5   6   7   8   9  10  11  12
------------------------------------------------
  1   2   3   4   5   6   7   8   9  10  11  12 |  1
      4   6   8  10  12  14  16  18  20  22  24 |  2
          9  12  15  18  21  24  27  30  33  36 |  3
             16  20  24  28  32  36  40  44  48 |  4
                 25  30  35  40  45  50  55  60 |  5
                     36  42  48  54  60  66  72 |  6
                         49  56  63  70  77  84 |  7
                             64  72  80  88  96 |  8
                                 81  90  99 108 |  9
                                    100 110 120 | 10
                                        121 132 | 11
                                            144 | 12

```



## Sidef


```ruby
var max = 12
var width = (max**2 -> len+1)
 
func fmt_row(*items) {
    items.map {|s| "%*s" % (width, s) }.join
}
 
say fmt_row('x┃', (1..max)...)
say "#{'━' * (width - 1)}╋#{'━' * (max * width)}"

{ |i| 
    say fmt_row("#{i}┃", {|j| i <= j ? i*j : ''}.map(1..max)...)
} << 1..max
```

```txt

  x┃   1   2   3   4   5   6   7   8   9  10  11  12
━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  1┃   1   2   3   4   5   6   7   8   9  10  11  12
  2┃       4   6   8  10  12  14  16  18  20  22  24
  3┃           9  12  15  18  21  24  27  30  33  36
  4┃              16  20  24  28  32  36  40  44  48
  5┃                  25  30  35  40  45  50  55  60
  6┃                      36  42  48  54  60  66  72
  7┃                          49  56  63  70  77  84
  8┃                              64  72  80  88  96
  9┃                                  81  90  99 108
 10┃                                     100 110 120
 11┃                                         121 132
 12┃                                             144

```



## Simula

```simula
begin
    integer i, j;
    outtext( "    " );
    for i := 1 step 1 until 12 do outint( i, 4 );
    outimage;
    outtext( "   +" );
    for i := 1 step 1 until 12 do outtext( "----" );
    outimage;
    for i := 1 step 1 until 12 do
    begin
        outint( i, 3 );
        outtext( "|" );
        for j := 1 step 1 until i - 1 do outtext( "    " );
        for j := i step 1 until 12 do outint( i * j, 4 );
        outimage
    end;
end
```

```txt
       1   2   3   4   5   6   7   8   9  10  11  12
   +------------------------------------------------
  1|   1   2   3   4   5   6   7   8   9  10  11  12
  2|       4   6   8  10  12  14  16  18  20  22  24
  3|           9  12  15  18  21  24  27  30  33  36
  4|              16  20  24  28  32  36  40  44  48
  5|                  25  30  35  40  45  50  55  60
  6|                      36  42  48  54  60  66  72
  7|                          49  56  63  70  77  84
  8|                              64  72  80  88  96
  9|                                  81  90  99 108
 10|                                     100 110 120
 11|                                         121 132
 12|                                             144
```



## Tcl


```tcl
puts "  x\u2502   1   2   3   4   5   6   7   8   9  10  11  12"
puts \u0020\u2500\u2500\u253c[string repeat \u2500 48]
for {set i 1} {$i <= 12} {incr i} {
    puts -nonewline [format "%3d" $i]\u2502[string repeat " " [expr {$i*4-4}]]
    for {set j 1} {$j <= 12} {incr j} {
	if {$j >= $i} {
	    puts -nonewline [format "%4d" [expr {$i*$j}]]
	}
    }
    puts ""
}
```

```txt

  x│   1   2   3   4   5   6   7   8   9  10  11  12
 ──┼────────────────────────────────────────────────
  1│   1   2   3   4   5   6   7   8   9  10  11  12
  2│       4   6   8  10  12  14  16  18  20  22  24
  3│           9  12  15  18  21  24  27  30  33  36
  4│              16  20  24  28  32  36  40  44  48
  5│                  25  30  35  40  45  50  55  60
  6│                      36  42  48  54  60  66  72
  7│                          49  56  63  70  77  84
  8│                              64  72  80  88  96
  9│                                  81  90  99 108
 10│                                     100 110 120
 11│                                         121 132
 12│                                             144

```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
x=y="1'2'3'4'5'6'7'8'9'10'11'12"
LOOP n,col=x,cnt=""
 skip=n-1
 LOOP m,row=y
  IF (m==skip) THEN
   td=""
  ELSE
   td=col*row
   coleqrow=col*n
   IF (td.lt.#coleqrow) td=""
  ENDIF
 td=CENTER (td,+3," ")
 cnt=APPEND (cnt,td," ")
 ENDLOOP
 col=CENTER (col,+3," ")
 PRINT col,cnt
ENDLOOP

```

<pre style='height:30ex;overflow:scroll'>
  1   2   3   4   5   6   7   8   9  10  11  12
  2   4   6   8  10  12  14  16  18  20  22  24
  3       9  12  15  18  21  24  27  30  33  36
  4          16  20  24  28  32  36  40  44  48
  5              25  30  35  40  45  50  55  60
  6                  36  42  48  54  60  66  72
  7                      49  56  63  70  77  84
  8                          64  72  80  88  96
  9                              81  90  99 108
 10                                 100 110 120
 11                                     121 132
 12                                         144

```



## uBasic/4tH

<lang>For R = 1 To 12
  Print R;Tab(R * 5);
  For C = R To 12
    Print Using "_____";R * C;
  Next
  Print
Next
```

```txt
1        1    2    3    4    5    6    7    8    9   10   11   12
2             4    6    8   10   12   14   16   18   20   22   24
3                  9   12   15   18   21   24   27   30   33   36
4                      16   20   24   28   32   36   40   44   48
5                           25   30   35   40   45   50   55   60
6                                36   42   48   54   60   66   72
7                                     49   56   63   70   77   84
8                                          64   72   80   88   96
9                                               81   90   99  108
10                                                  100  110  120
11                                                       121  132
12                                                            144

0 OK, 0:105
```


## Ursala

It's no more difficult to express the general case than the size 12 case, so
a table generating function parameterized by the size is used.

```Ursala

#import std
#import nat

table "n" =

~&plrTS(
   ~&xS pad` @xS <'x  ','--'>-- --' | '*hS %nP* nrange/1 "n",
   ^CthPiC(`-!*h,~&) mat` *xSSK7 pad` *K7ihxPBSS (~&i&& %nP)** nleq&&product**iiK0lK2x nrange/1 "n")

#show+

main = table 12

```

A better way of using Ursala to make tables would be with the <code>tbl</code> library included with
the standard package, which can generate LaTeX code for arbitrary heading hierarchies and typesetting options, but here it is in ASCII art.

```txt

  x  1 2 3  4  5  6  7  8  9  10  11  12
   -------------------------------------
 1 | 1 2 3  4  5  6  7  8  9  10  11  12
 2 |   4 6  8 10 12 14 16 18  20  22  24
 3 |     9 12 15 18 21 24 27  30  33  36
 4 |       16 20 24 28 32 36  40  44  48
 5 |          25 30 35 40 45  50  55  60
 6 |             36 42 48 54  60  66  72
 7 |                49 56 63  70  77  84
 8 |                   64 72  80  88  96
 9 |                      81  90  99 108
10 |                         100 110 120
11 |                             121 132
12 |                                 144

```



## Visual Basic

```vb
Sub Main()
    Const nmax = 12, xx = 3
    Const x = xx + 1
    Dim i As Integer, j As Integer, s As String
    s = String(xx, " ") & " |"
    For j = 1 To nmax
        s = s & Right(String(x, " ") & j, x)
    Next j
    Debug.Print s
    s = String(xx, "-") & " +"
    For j = 1 To nmax
        s = s & " " & String(xx, "-")
    Next j
    Debug.Print s
    For i = 1 To nmax
        s = Right(String(xx, " ") & i, xx) & " |"
        For j = 1 To nmax
            If j >= i _
            Then s = s & Right(String(x, " ") & i * j, x) _
            Else s = s & String(x, " ")
        Next j
        Debug.Print s
    Next i
End Sub 'Main
```

```txt

    |   1   2   3   4   5   6   7   8   9  10  11  12
--- + --- --- --- --- --- --- --- --- --- --- --- ---
  1 |   1   2   3   4   5   6   7   8   9  10  11  12
  2 |       4   6   8  10  12  14  16  18  20  22  24
  3 |           9  12  15  18  21  24  27  30  33  36
  4 |              16  20  24  28  32  36  40  44  48
  5 |                  25  30  35  40  45  50  55  60
  6 |                      36  42  48  54  60  66  72
  7 |                          49  56  63  70  77  84
  8 |                              64  72  80  88  96
  9 |                                  81  90  99 108
 10 |                                     100 110 120
 11 |                                         121 132
 12 |                                             144

```



## VBA



```vb

Option Explicit

Sub Multiplication_Tables()
Dim strTemp As String, strBuff As String
Dim i&, j&, NbDigits As Byte

'You can adapt the following const :
Const NB_END As Byte = 12

    Select Case NB_END
        Case Is < 10: NbDigits = 3
        Case 10 To 31: NbDigits = 4
        Case 31 To 100: NbDigits = 5
        Case Else: MsgBox "Number too large": Exit Sub
    End Select
    strBuff = String(NbDigits, " ")

    For i = 1 To NB_END
        strTemp = Right(strBuff & i, NbDigits)
        For j = 2 To NB_END
            If j < i Then
                strTemp = strTemp & strBuff
            Else
                strTemp = strTemp & Right(strBuff & j * i, NbDigits)
            End If
        Next j
        Debug.Print strTemp
    Next i
End Sub

```

```txt
   1   2   3   4   5   6   7   8   9  10  11  12
   2   4   6   8  10  12  14  16  18  20  22  24
   3       9  12  15  18  21  24  27  30  33  36
   4          16  20  24  28  32  36  40  44  48
   5              25  30  35  40  45  50  55  60
   6                  36  42  48  54  60  66  72
   7                      49  56  63  70  77  84
   8                          64  72  80  88  96
   9                              81  90  99 108
  10                                 100 110 120
  11                                     121 132
  12                                         144
```



## XBasic

```xbasic

PROGRAM "multiplicationtables"
VERSION "0.0001"

DECLARE FUNCTION Entry()

FUNCTION Entry()
  $N = 12
  FOR j@@ = 1 TO $N - 1
    PRINT FORMAT$("### ", j@@);
  NEXT j@@
  PRINT FORMAT$("###", $N)
  FOR j@@ = 0 TO $N - 1
    PRINT "----";
  NEXT j@@
  PRINT "+"
  FOR i@@ = 1 TO $N
    FOR j@@ = 1 TO $N
      IF j@@ < i@@ THEN
        PRINT "    ";
      ELSE
        PRINT FORMAT$("### ", i@@ * j@@);
      END IF
    NEXT j@@
    PRINT "|"; FORMAT$(" ##", i@@)
  NEXT i@@
END FUNCTION
END PROGRAM

```

```txt

  1   2   3   4   5   6   7   8   9  10  11  12
------------------------------------------------+
  1   2   3   4   5   6   7   8   9  10  11  12 |  1
      4   6   8  10  12  14  16  18  20  22  24 |  2
          9  12  15  18  21  24  27  30  33  36 |  3
             16  20  24  28  32  36  40  44  48 |  4
                 25  30  35  40  45  50  55  60 |  5
                     36  42  48  54  60  66  72 |  6
                         49  56  63  70  77  84 |  7
                             64  72  80  88  96 |  8
                                 81  90  99 108 |  9
                                    100 110 120 | 10
                                        121 132 | 11
                                            144 | 12

```



## XPL0


```XPL0
include c:\cxpl\codes;
int X, Y;
[Format(4, 0);
Text(0, "    |");  for X:= 1 to 12 do RlOut(0, float(X));
CrLf(0);
Text(0, "  --+");  for X:= 1 to 12 do Text(0, "----");
CrLf(0);
for Y:= 1 to 12 do
    [RlOut(0, float(Y));  ChOut(0, ^|);
    for X:= 1 to 12 do
        if X>=Y then RlOut(0, float(X*Y)) else Text(0, " . .");
    CrLf(0);
    ];
]
```


```txt

    |   1   2   3   4   5   6   7   8   9  10  11  12
  --+------------------------------------------------
   1|   1   2   3   4   5   6   7   8   9  10  11  12
   2| . .   4   6   8  10  12  14  16  18  20  22  24
   3| . . . .   9  12  15  18  21  24  27  30  33  36
   4| . . . . . .  16  20  24  28  32  36  40  44  48
   5| . . . . . . . .  25  30  35  40  45  50  55  60
   6| . . . . . . . . . .  36  42  48  54  60  66  72
   7| . . . . . . . . . . . .  49  56  63  70  77  84
   8| . . . . . . . . . . . . . .  64  72  80  88  96
   9| . . . . . . . . . . . . . . . .  81  90  99 108
  10| . . . . . . . . . . . . . . . . . . 100 110 120
  11| . . . . . . . . . . . . . . . . . . . . 121 132
  12| . . . . . . . . . . . . . . . . . . . . . . 144

```



## zkl


```zkl
fcn multiplicationTable(n){
   w,fmt := (n*n).numDigits, " %%%dd".fmt(w).fmt;  // eg " %3".fmt
   header:=[1..n].apply(fmt).concat();	   // 1  2  3  4 ...
   println(" x ", header, "\n   ", "-"*header.len());
   dash:=String(" "*w,"-");	// eg "   -"
   foreach a in ([1..n]){
      print("%2d|".fmt(a),dash*(a-1));
      [a..n].pump(String,'*(a),fmt).println();
   }
}(12);
```

```txt

 x   1   2   3   4   5   6   7   8   9  10  11  12
   -----------------------------------------------
 1|  1   2   3   4   5   6   7   8   9  10  11  12
 2|  -   4   6   8  10  12  14  16  18  20  22  24
 3|  -   -   9  12  15  18  21  24  27  30  33  36
 4|  -   -   -  16  20  24  28  32  36  40  44  48
 5|  -   -   -   -  25  30  35  40  45  50  55  60
 6|  -   -   -   -   -  36  42  48  54  60  66  72
 7|  -   -   -   -   -   -  49  56  63  70  77  84
 8|  -   -   -   -   -   -   -  64  72  80  88  96
 9|  -   -   -   -   -   -   -   -  81  90  99 108
10|  -   -   -   -   -   -   -   -   - 100 110 120
11|  -   -   -   -   -   -   -   -   -   - 121 132
12|  -   -   -   -   -   -   -   -   -   -   - 144

```

