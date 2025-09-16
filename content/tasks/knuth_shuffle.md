+++
title = "Knuth shuffle"
description = ""
date = 2019-10-20T02:49:03Z
aliases = []
[extra]
id = 4216
[taxonomies]
categories = ["task", "Classic CS problems and programs"]
tags = []
languages = [
  "360_assembly",
  "acl2",
  "ada",
  "aime",
  "algol_68",
  "applescript",
  "applesoft_basic",
  "arm_assembly",
  "autohotkey",
  "autoit",
  "awk",
  "basic",
  "bbc_basic",
  "brat",
  "c",
  "clojure",
  "cmake",
  "cobol",
  "coffeescript",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "delphi",
  "dwscript",
  "e",
  "echolisp",
  "egel",
  "eiffel",
  "elena",
  "elixir",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "fantom",
  "forth",
  "fortran",
  "freebasic",
  "frink",
  "funl",
  "gambas",
  "gap",
  "go",
  "groovy",
  "haskell",
  "inform_6",
  "j",
  "java",
  "javascript",
  "joy",
  "julia",
  "kotlin",
  "labview",
  "lasso",
  "liberty_basic",
  "logo",
  "lua",
  "m2000_interpreter",
  "m4",
  "mathematica",
  "matlab",
  "maxima",
  "mumps",
  "nemerle",
  "netrexx",
  "nim",
  "ocaml",
  "oforth",
  "ol",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "rebol",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "scratch",
  "seed7",
  "sidef",
  "sinclair_zx81_basic",
  "smalltalk",
  "snobol4",
  "stata",
  "swift",
  "tcl",
  "tuscript",
  "ubasic_4th",
  "unix_shell",
  "ursala",
  "vba",
  "vbscript",
  "vedit_macro_language",
  "zkl",
]
+++

## Task

The   [[wp:Knuth shuffle|Knuth shuffle]]   (a.k.a. the Fisher-Yates shuffle)   is an algorithm for randomly shuffling the elements of an array.

Implement the Knuth shuffle for an integer array (or, if possible, an array of any type).

Given an array '''''items''''' with indices ranging from ''0'' to '''''last''''', the algorithm can be defined as follows (pseudo-code):

 '''for''' ''i'' '''from''' ''last'' '''downto''' 1 '''do''':
     '''let''' ''j'' = random integer in range ''0'' <math>\leq</math> ''j'' <math>\leq</math> ''i''
     '''swap''' ''items''[''i''] '''with''' ''items''[''j'']

Notes:
* It modifies the input array in-place. If that is unreasonable in your programming language, you may amend the algorithm to return the shuffled items as a new array instead.
* The algorithm can also be amended to iterate from left to right, if that is more convenient.

{| class="wikitable"
|-
! Input array
! Possible output arrays
|-
| <tt>[]</tt>
| <tt>[]</tt>
|-
| <tt>[10]</tt>
| <tt>[10]</tt>
|-
| <tt>[10, 20]</tt>
| <tt>[10, 20]</tt>
<tt>[20, 10]</tt>
|-
| <tt>[10, 20, 30]</tt>
| <tt>[10, 20, 30]</tt>
<tt>[10, 30, 20]</tt>
<tt>[20, 10, 30]</tt>
<tt>[20, 30, 10]</tt>
<tt>[30, 10, 20]</tt>
<tt>[30, 20, 10]</tt>
|}

(These are listed here just for your convenience; no need to demonstrate them on the page.)

* [[Sattolo cycle]]

<hr>


## 360 Assembly

```360asm

*        Knuth shuffle             02/11/2015
KNUTHSH  CSECT
         USING  KNUTHSH,R15
         LA     R6,1               i=1
LOOPI1   C      R6,=A(CARDS)       do i=1 to cards
         BH     ELOOPI1
         STC    R6,PACK(R6)        pack(i)=i
         LA     R6,1(R6)           i=i+1
         B      LOOPI1
ELOOPI1  LA     R7,CARDS           n=cards
LOOPN    C      R7,=F'2'           do n=cards to 2 by -1
         BL     ELOOPN
         L      R5,RANDSEED        r5=seed
         M      R4,=F'397204094'   r4r5=seed*const
         D      R4,=X'7FFFFFFF'    r5=r5 div (2^31-1)
         ST     R4,RANDSEED        r4=r5 mod (2^31-1); seed=r4
         LR     R5,R4              r5=seed
         LA     R4,0               r4=0
         DR     R4,R7              r5=seed div n; r4=seed mod n
         LA     R9,1(R4)           r2=randint(n)+1 [1:n]
         LA     R4,PACK(R7)        @pack(n)
         LA     R5,PACK(R9)        @pack(nw)
         MVC    TMP,0(R4)          tmp=pack(n)
         MVC    0(1,R4),0(R5)      pack(n)=pack(nw)
         MVC    0(1,R5),TMP        pack(nw)=tmp
         BCTR   R7,0               n=n-1
         B      LOOPN
ELOOPN   LA     R6,1               i=1
         LA     R8,PG              pgi=@pg
LOOPI2   C      R6,=A(CARDS)       do i=1 to cards
         BH     ELOOPI2
         XR     R2,R2              r2=0
         IC     R2,PACK(R6)        pack(i)
         XDECO  R2,XD              edit pack(i)
         MVC    0(3,R8),XD+9       output pack(i)
         LA     R8,3(R8)           pgi=pgi+3
         LA     R6,1(R6)           i=i+1
         B      LOOPI2
ELOOPI2  XPRNT  PG,80              print buffer
         XR     R15,R15            set return code
         BR     R14                return to caller
CARDS    EQU    20                 number of cards
PACK     DS     (CARDS+1)C         pack of cards
TMP      DS     C                  temp for swap
PG       DC     CL80' '            buffer
XD       DS     CL12               to decimal
RANDSEED DC     F'16807'           running seed
         YREGS
         END    KNUTHSH

```

```txt

 13 16 10 18 19 14  6 17  2  5  1 15  7 11 12  9  8 20  4  3

```




## ACL2


```Lisp
:set-state-ok t

(defun array-swap (name array i j)
   (let ((ai (aref1 name array i))
         (aj (aref1 name array j)))
      (aset1 name
             (aset1 name array j ai)
             i aj)))

(defun shuffle-r (name array m state)
   (if (zp m)
       (mv array state)
       (mv-let (i state)
               (random$ m state)
          (shuffle-r name
                     (array-swap name array i m)
                     (1- m)
                     state))))

(defun shuffle (name array state)
   (shuffle-r name
              array
              (1- (first (dimensions name array)))
              state))
```



## Ada

This implementation is a generic shuffle routine, able to shuffle an array of any type.

```Ada
generic
   type Element_Type is private;
   type Array_Type is array (Positive range <>) of Element_Type;

procedure Generic_Shuffle (List : in out Array_Type);
```


```Ada
with Ada.Numerics.Discrete_Random;

procedure Generic_Shuffle (List : in out Array_Type) is
   package Discrete_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Integer);
   use Discrete_Random;
   K : Integer;
   G : Generator;
   T : Element_Type;
begin
   Reset (G);
   for I in reverse List'Range loop
      K := (Random(G) mod I) + 1;
      T := List(I);
      List(I) := List(K);
      List(K) := T;
   end loop;
end Generic_Shuffle;
```

An example using Generic_Shuffle.

```Ada
with Ada.Text_IO;
with Generic_Shuffle;

procedure Test_Shuffle is

   type Integer_Array is array (Positive range <>) of Integer;

   Integer_List : Integer_Array
     := (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18);
   procedure Integer_Shuffle is new Generic_Shuffle(Element_Type => Integer,
                                                    Array_Type => Integer_Array);
begin

   for I in Integer_List'Range loop
      Ada.Text_IO.Put(Integer'Image(Integer_List(I)));
   end loop;
   Integer_Shuffle(List => Integer_List);
   Ada.Text_IO.New_Line;
   for I in Integer_List'Range loop
      Ada.Text_IO.Put(Integer'Image(Integer_List(I)));
   end loop;
end Test_Shuffle;
```



## Aime

The shuffle function works on any type (the lists are heterogenous).

```aime
void
shuffle(list l)
{
    integer i;

    i = ~l;
    if (i) {
        i -= 1;
        while (i) {
            l.spin(i, drand(i));
            i -= 1;
        }
    }
}
```



## ALGOL 68

```algol68
PROC between = (INT a, b)INT :
(
  ENTIER (random * ABS (b-a+1) + (a<b|a|b))
);

PROC knuth shuffle = (REF[]INT a)VOID:
(
  FOR i FROM LWB a TO UPB a DO
    INT j = between(LWB a, UPB a);
    INT t = a[i];
    a[i] := a[j];
    a[j] := t
  OD
);
```


```algol68
main:(
  [20]INT a;
  FOR i FROM 1 TO 20 DO a[i] := i OD;
  knuth shuffle(a);
  print(a)
)
```



## AppleScript



### Iteration



```AppleScript
set n to 25

set array to {}
repeat with i from 1 to n
	set end of array to i
end repeat
copy {array, array} to {unshuffled, shuffled}
repeat with i from n to 1 by -1
	set j to (((random number) * (i - 1)) as integer) + 1
	set shuffled's item i to array's item j
	if j ≠ i's contents then set array's item j to array's item i
end repeat

return {unshuffled, shuffled}
```

Example:

```AppleScript
{{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25},
{14, 25, 3, 1, 12, 18, 11, 20, 16, 15, 21, 5, 22, 19, 2, 24, 8, 10, 13, 6, 17, 23, 9, 7, 4}}
```



### Functional composition



```AppleScript
-- KNUTH SHUFFLE -------------------------------------------------------------

-- knuthShuffle :: [a] -> [a]
on knuthShuffle(xs)

    -- randomSwap :: [Int] -> Int -> [Int]
    script randomSwap
        on |λ|(a, i)
            if i > 1 then
                set iRand to random number from 1 to i
                tell a
                    set tmp to item iRand
                    set item iRand to item i
                    set item i to tmp
                    it
                end tell
            else
                a
            end if
        end |λ|
    end script

    foldr(randomSwap, xs, enumFromTo(1, length of xs))
end knuthShuffle


-- TEST ----------------------------------------------------------------------
on run
    knuthShuffle(["alpha", "beta", "gamma", "delta", "epsilon", ¬
        "zeta", "eta", "theta", "iota", "kappa", "lambda", "mu"])
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

-- foldr :: (a -> b -> a) -> a -> [b] -> a
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldr

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

e.g.

```AppleScript
{"mu", "theta", "alpha", "delta", "zeta", "gamma",
"iota", "kappa", "lambda", "epsilon", "beta", "eta"}
```


## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program knuthShuffle.s   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/*********************************/
/* Initialized data              */
/*********************************/
.data
sMessResult:      .ascii "Value  : "
sMessValeur:       .fill 11, 1, ' '            @ size => 11
szCarriageReturn: .asciz "\n"

.align 4
iGraine:  .int 123456
.equ NBELEMENTS,      10
TableNumber:	     .int   1,2,3,4,5,6,7,8,9,10

/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                           @ entry of program
    ldr r0,iAdrTableNumber                      @ address number table
    mov r1,#NBELEMENTS                          @ number of élements
    bl knuthShuffle
    ldr r2,iAdrTableNumber
    mov r3,#0
1:                                              @ loop display table
    ldr r0,[r2,r3,lsl #2]
    ldr r1,iAdrsMessValeur                      @ display value
    bl conversion10                             @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                            @ display message
    add r3,#1
    cmp r3,#NBELEMENTS - 1
    ble 1b

    ldr r0,iAdrszCarriageReturn
    bl affichageMess
    /*    2e shuffle             */
    ldr r0,iAdrTableNumber                     @ address number table
    mov r1,#NBELEMENTS                         @ number of élements
    bl knuthShuffle
    ldr r2,iAdrTableNumber
    mov r3,#0
2:                                             @ loop display table
    ldr r0,[r2,r3,lsl #2]
    ldr r1,iAdrsMessValeur                     @ display value
    bl conversion10                            @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                           @ display message
    add r3,#1
    cmp r3,#NBELEMENTS - 1
    ble 2b

100:                                            @ standard end of the program
    mov r0, #0                                  @ return code
    mov r7, #EXIT                               @ request to exit program
    svc #0                                      @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrsMessResult:          .int sMessResult
iAdrTableNumber:          .int TableNumber

/******************************************************************/
/*     Knuth Shuffle                                             */
/******************************************************************/
/* r0 contains the address of table */
/* r1 contains the number of elements */
knuthShuffle:
    push {r2-r5,lr}                                    @ save registers
    mov r5,r0                                          @ save table address
    mov r2,#0                                          @ start index
1:
    mov r0,r2                                          @ generate aleas
    bl genereraleas
    ldr r3,[r5,r2,lsl #2]                              @ swap number on the table
    ldr r4,[r5,r0,lsl #2]
    str r4,[r5,r2,lsl #2]
    str r3,[r5,r0,lsl #2]
    add r2,#1                                           @ next number
    cmp r2,r1                                           @ end ?
    blt 1b                                              @ no -> loop

100:
    pop {r2-r5,lr}
    bx lr                                               @ return

/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                          @ save  registres
    mov r2,#0                                      @ counter length
1:                                                 @ loop length calculation
    ldrb r1,[r0,r2]                                @ read octet start position + index
    cmp r1,#0                                      @ if 0 its over
    addne r2,r2,#1                                 @ else add 1 in the length
    bne 1b                                         @ and loop
                                                   @ so here r2 contains the length of the message
    mov r1,r0                                      @ address message in r1
    mov r0,#STDOUT                                 @ code to write to the standard output Linux
    mov r7, #WRITE                                 @ code call system "write"
    svc #0                                         @ call systeme
    pop {r0,r1,r2,r7,lr}                           @ restaur des  2 registres */
    bx lr                                          @ return
/******************************************************************/
/*     Converting a register to a decimal unsigned                */
/******************************************************************/
/* r0 contains value and r1 address area   */
/* r0 return size of result (no zero final in area) */
/* area size => 11 bytes          */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}                                @ save registers
    mov r3,r1
    mov r2,#LGZONECAL

1:	                                           @ start loop
    bl divisionpar10U                              @unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                                     @ digit
    strb r1,[r3,r2]                                @ store digit on area
    cmp r0,#0                                      @ stop if quotient = 0
    subne r2,#1                                    @ else previous position
    bne 1b	                                   @ and loop
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
    mov r0,r4                                         @ result length
    mov r1,#' '                                       @ space
3:
    strb r1,[r3,r4]                                   @ store space in area
    add r4,#1                                         @ next position
    cmp r4,#LGZONECAL
    ble 3b                                            @ loop if r4 <= area size

100:
    pop {r1-r4,lr}                                    @ restaur registres
    bx lr                                             @return

/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient */
/* r1 remainder  */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                          @ save value
    //mov r3,#0xCCCD                                   @ r3 <- magic_number lower  raspberry 3
    //movt r3,#0xCCCC                                  @ r3 <- magic_number higter raspberry 3
    ldr r3,iMagicNumber                                @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                               @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0)
    mov r0, r2, LSR #3                                 @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                               @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                               @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                              @ leave function
iMagicNumber:  	.int 0xCCCCCCCD
/***************************************************/
/*   Generation random number                  */
/***************************************************/
/* r0 contains limit  */
genereraleas:
    push {r1-r4,lr}                                    @ save registers
    ldr r4,iAdriGraine
    ldr r2,[r4]
    ldr r3,iNbDep1
    mul r2,r3,r2
    ldr r3,iNbDep1
    add r2,r2,r3
    str r2,[r4]                                        @ maj de la graine pour l appel suivant
    cmp r0,#0
    beq 100f
    mov r1,r0                                          @ divisor
    mov r0,r2                                          @ dividende
    bl division
    mov r0,r3                                          @ résult = remainder

100:                                                   @ end function
    pop {r1-r4,lr}                                     @ restaur registers
    bx lr                                              @ return
/*****************************************************/
iAdriGraine: .int iGraine
iNbDep1: .int 0x343FD
iNbDep2: .int 0x269EC3
/***************************************************/
/* integer division unsigned                       */
/***************************************************/
division:
    /* r0 contains dividend */
    /* r1 contains divisor */
    /* r2 returns quotient */
    /* r3 returns remainder */
    push {r4, lr}
    mov r2, #0                                         @ init quotient
    mov r3, #0                                         @ init remainder
    mov r4, #32                                        @ init counter bits
    b 2f
1:                                                     @ loop
    movs r0, r0, LSL #1                                @ r0 <- r0 << 1 updating cpsr (sets C if 31st bit of r0 was 1)
    adc r3, r3, r3                                     @ r3 <- r3 + r3 + C. This is equivalent to r3 ? (r3 << 1) + C
    cmp r3, r1                                         @ compute r3 - r1 and update cpsr
    subhs r3, r3, r1                                   @ if r3 >= r1 (C=1) then r3 <- r3 - r1
    adc r2, r2, r2                                     @ r2 <- r2 + r2 + C. This is equivalent to r2 <- (r2 << 1) + C
2:
    subs r4, r4, #1                                    @ r4 <- r4 - 1
    bpl 1b                                             @ if r4 >= 0 (N=0) then loop
    pop {r4, lr}
    bx lr



```



## AutoHotkey

ahk forum: [http://www.autohotkey.com/forum/viewtopic.php?t=44657&postdays=0&postorder=asc&start=133 discussion]

```AutoHotkey
MsgBox % shuffle("1,2,3,4,5,6,7,8,9")
MsgBox % shuffle("1,2,3,4,5,6,7,8,9")

shuffle(list) {                          ; shuffle comma separated list, converted to array
   StringSplit a, list, `,               ; make array (length = a0)
   Loop % a0-1 {
      Random i, A_Index, a0              ; swap item 1,2... with a random item to the right of it
      t := a%i%, a%i% := a%A_Index%, a%A_Index% := t
   }
   Loop % a0                             ; construct string from sorted array
      s .= "," . a%A_Index%
   Return SubStr(s,2)                    ; drop leading comma
}
```



## AutoIt


```AutoIt

Dim $a[10]
ConsoleWrite('array before permutation:' & @CRLF)
For $i = 0 To 9
	$a[$i] = Random(20,100,1)
	ConsoleWrite($a[$i] & ' ')
Next
ConsoleWrite(@CRLF)

_Permute($a)
ConsoleWrite('array after permutation:' & @CRLF)
For $i = 0 To UBound($a) -1
	ConsoleWrite($a[$i] & ' ')
Next
ConsoleWrite(@CRLF)


Func _Permute(ByRef $array)
	Local $random, $tmp
	For $i = UBound($array) -1 To 0 Step -1
		$random = Random(0,$i,1)
		$tmp = $array[$random]
		$array[$random] = $array[$i]
		$array[$i] = $tmp
	Next
EndFunc

```


```txt

 array before permutation:
 43 57 37 20 97 98 69 76 97 70
 array after permutation:
 57 69 97 70 37 97 20 76 43 98

```



## AWK

Many [[Arrays#AWK|arrays in AWK]] have the first index at 1.
This example shows how to shuffle such arrays.
The elements can be integers, floating-point numbers, or strings.

```awk
# Shuffle an _array_ with indexes from 1 to _len_.
function shuffle(array, len,    i, j, t) {
	for (i = len; i > 1; i--) {
		# j = random integer from 1 to i
		j = int(i * rand()) + 1

		# swap array[i], array[j]
		t = array[i]
		array[i] = array[j]
		array[j] = t
	}
}

# Test program.
BEGIN {
	len = split("11 22 33 44 55 66 77 88 99 110", array)
	shuffle(array, len)

	for (i = 1; i < len; i++) printf "%s ", array[i]
	printf "%s\n", array[len]
}
```



## BASIC


```qbasic
RANDOMIZE TIMER

DIM cards(51) AS INTEGER
DIM L0 AS LONG, card AS LONG

PRINT "before:"
FOR L0 = 0 TO 51
    cards(L0) = L0
    PRINT LTRIM$(STR$(cards(L0))); " ";
NEXT

FOR L0 = 51 TO 0 STEP -1
    card = INT(RND * (L0 + 1))
    IF card <> L0 THEN SWAP cards(card), cards(L0)
NEXT

PRINT : PRINT "after:"
FOR L0 = 0 TO 51
    PRINT LTRIM$(STR$(cards(L0))); " ";
NEXT
PRINT
```


```txt

 before:
 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51
 after:
 27 14 37 35 3 44 25 38 46 1 22 49 2 51 16 32 20 30 4 33 36 6 31 21 41 34 9 13 0
 50 47 48 40 39 7 18 19 26 24 10 29 5 12 28 11 17 43 45 8 23 42 15

```


=
## Applesoft BASIC
=
As mentioned in the Sinclair ZX81 BASIC solution, for very small positive integer values, a string is a much more memory-efficient array, but here is an example of an array with numbers. Line <code>150</code> initializes and prints each element in the array. Line <code>190</code> performs the swap of the elements.

```basic
 100 :
 110  REM  KNUTH SHUFFLE
 120 :
 130  DIM A(25)
 140  FOR I = 1 TO 25
 150 A(I) = I: PRINT A(I);" ";: NEXT I
 160  PRINT : PRINT
 170  FOR I = 25 TO 2 STEP  - 1
 180 J =  INT ( RND (1) * I + 1)
 190 T = A(I):A(I) = A(J):A(J) = T: NEXT I
 200  FOR I = 1 TO 25
 210  PRINT A(I);" ";: NEXT I
 220  END

```

```txt
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 1
7 18 19 20 21 22 23 24 25
```

When it has finished, the screen will show (for example):

```txt
20 5 6 9 15 23 22 8 4 24 7 11 16 21 2 17
 14 10 19 13 12 18 1 3 25
```


=
## Sinclair ZX81 BASIC
=
For very small positive integer values, a string (which can be treated as an array of bytes) is much more memory-efficient than an array of numbers. In this program we shuffle a string consisting of the letters 'A' to 'Z'. The ZX81 is slow enough that we can watch the shuffle happening in real time, with items switching to inverse video display as they are shuffled. (This can be done, in the ZX81 character set, by setting the high bit in the character code.) Line <code>10</code> seeds the pseudo-random number generator. Note that strings (and arrays) are indexed from 1.

The program works with the unexpanded (1k RAM) ZX81.

```basic
 10 RAND
 20 LET A$=""
 30 FOR I=1 TO 26
 40 LET A$=A$+CHR$ (37+I)
 50 NEXT I
 60 PRINT A$
 70 FOR I=26 TO 2 STEP -1
 80 LET J=1+INT (RND*I)
 90 LET T$=A$(I)
100 LET A$(I)=A$(J)
110 LET A$(J)=T$
120 PRINT AT 0,I-1;CHR$ (CODE A$(I)+128)
130 PRINT AT 0,J-1;CHR$ (CODE A$(J)+128)
140 NEXT I
```

While the program is running, we will see something like this (using lower case as a stand-in for inverse video):

```txt
ABCuEFGzwJKLMNOPxySvdtiqrh
```

When it has finished, the screen will show (for example):

```txt
lcjbpxekzsaygumwnovfdtiqrh
```



## BBC BASIC


```bbcbasic
      cards% = 52
      DIM pack%(cards%)
      FOR I% = 1 TO cards%
        pack%(I%) = I%
      NEXT I%
      FOR N% = cards% TO 2 STEP -1
        SWAP pack%(N%),pack%(RND(N%))
      NEXT N%
      FOR I% = 1 TO cards%
        PRINT pack%(I%);
      NEXT I%
      PRINT
```


== {{header|bc}} ==
I provide a <tt>shuffle()</tt> function. It can only shuffle an array of numbers. It fails if the array has more than 32768 elements. It always shuffles the array named <tt>shuffle[]</tt>; the array is not a function parameter because <tt>bc</tt> passes arrays by copying.

This code requires a <tt>bc</tt> with long names; the test program also requires a <tt>bc</tt> with the <tt>print</tt> statement.
```bc
seed = 1   /* seed of the random number generator */
scale = 0

/* Random number from 0 to 32767. */
define rand() {
	/* Formula (from POSIX) for random numbers of low quality. */
	seed = (seed * 1103515245 + 12345) % 4294967296
	return ((seed / 65536) % 32768)
}

/* Shuffle the first _count_ elements of shuffle[]. */
define shuffle(count) {
	auto b, i, j, t

	i = count
	while (i > 0) {
		/* j = random number in [0, i) */
		b = 32768 % i  /* want rand() >= b */
		while (1) {
			j = rand()
			if (j >= b) break
		}
		j = j % i

		/* decrement i, swap shuffle[i] and shuffle[j] */
		t = shuffle[--i]
		shuffle[i] = shuffle[j]
		shuffle[j] = t
	}
}

/* Test program. */
define print_array(count) {
	auto i
	for (i = 0; i < count - 1; i++) print shuffle[i], ", "
	print shuffle[i], "\n"
}

for (i = 0; i < 10; i++) shuffle[i] = 11 * (i + 1)
"Original array: "; trash = print_array(10)

trash = shuffle(10)
"Shuffled array: "; trash = print_array(10)
quit
```

```txt
Original array: 11, 22, 33, 44, 55, 66, 77, 88, 99, 110
Shuffled array: 66, 44, 11, 55, 33, 77, 110, 22, 88, 99
```



## Brat


```brat
shuffle = { a |
  (a.length - 1).to 1 { i |
    random_index = random(0, i)
    temp = a[i]
    a[i] = a[random_index]
    a[random_index] = temp
  }

  a
}

p shuffle [1 2 3 4 5 6 7]
```



## C

This shuffles any "object"; it imitates <tt>qsort</tt> in the syntax.

```cpp
#include <iostream>
#include <string.h>

int rrand(int m)
{
  return (int)((double)m * ( rand() / (RAND_MAX+1.0) ));
}

#define BYTE(X) ((unsigned char *)(X))
void shuffle(void *obj, size_t nmemb, size_t size)
{
  void *temp = malloc(size);
  size_t n = nmemb;
  while ( n > 1 ) {
    size_t k = rrand(n--);
    memcpy(temp, BYTE(obj) + n*size, size);
    memcpy(BYTE(obj) + n*size, BYTE(obj) + k*size, size);
    memcpy(BYTE(obj) + k*size, temp, size);
  }
  free(temp);
}
```

Alternatively, using Durstenfeld's method (swapping selected item and last item in each iteration instead of literally shifting everything), and macro'd function declaration/definition:

```c
#include <stdio.h>
#include <stdlib.h>

/* define a shuffle function. e.g. decl_shuffle(double).
 * advantage: compiler is free to optimize the swap operation without
 *            indirection with pointers, which could be much faster.
 * disadvantage: each datatype needs a separate instance of the function.
 *            for a small funciton like this, it's not very big a deal.
 */
#define decl_shuffle(type)				\
void shuffle_##type(type *list, size_t len) {		\
	int j;						\
	type tmp;					\
	while(len) {					\
		j = irand(len);				\
		if (j != len - 1) {			\
			tmp = list[j];			\
			list[j] = list[len - 1];	\
			list[len - 1] = tmp;		\
		}					\
		len--;					\
	}						\
}							\

/* random integer from 0 to n-1 */
int irand(int n)
{
	int r, rand_max = RAND_MAX - (RAND_MAX % n);
	/* reroll until r falls in a range that can be evenly
	 * distributed in n bins.  Unless n is comparable to
	 * to RAND_MAX, it's not *that* important really. */
	while ((r = rand()) >= rand_max);
	return r / (rand_max / n);
}

/* declare and define int type shuffle function from macro */
decl_shuffle(int);

int main()
{
	int i, x[20];

	for (i = 0; i < 20; i++) x[i] = i;
	for (printf("before:"), i = 0; i < 20 || !printf("\n"); i++)
		printf(" %d", x[i]);

	shuffle_int(x, 20);

	for (printf("after: "), i = 0; i < 20 || !printf("\n"); i++)
		printf(" %d", x[i]);
	return 0;
}
```



## C++

'''Compiler:''' [[g++]] (version 4.3.2 20081105 (Red Hat 4.3.2-7))

```cpp
#include <cstdlib>
#include <algorithm>
#include <iterator>

template<typename RandomAccessIterator>
void knuthShuffle(RandomAccessIterator begin, RandomAccessIterator end) {
  for(unsigned int n = end - begin - 1; n >= 1; --n) {
    unsigned int k = rand() % (n + 1);
    if(k != n) {
      std::iter_swap(begin + k, begin + n);
    }
  }
}
```

The standard library provides this in the form of <code>std::random_shuffle</code>.

```cpp
#include <algorithm>
#include <vector>

int main()
{
    int array[] = { 1,2,3,4,5,6,7,8,9 }; // C-style array of integers
    std::vector<int> vec(array, array + 9); // build STL container from int array

    std::random_shuffle(array, array + 9); // shuffle C-style array
    std::random_shuffle(vec.begin(), vec.end()); // shuffle STL container
}
```


## C#

```csharp
public static void KnuthShuffle<T>
(T[] array)
{
    System.Random random = new System.Random();
    for (int i = 0; i < array.Length; i++)
    {
        int j = random.Next(i, array.Length); // Don't select from the entire array on subsequent loops
        T temp = array[i]; array[i] = array[j]; array[j] = temp;
    }
}
```



## Clojure


```lisp
(defn shuffle [vect]
  (reduce (fn [v i] (let [r (rand-int i)]
                      (assoc v i (v r) r (v i))))
          vect (range (dec (count vect)) 1 -1)))
```

This works by generating a sequence of end-indices from n-1 to 1, then reducing that sequence (starting with the original vector) through a function that, given a vector and end-index, performs a swap between the end-index and some random index less than the end-index.


## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. knuth-shuffle.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  i                       PIC 9(8).
       01  j                       PIC 9(8).

       01  temp                    PIC 9(8).

       LINKAGE SECTION.
       78  Table-Len               VALUE 10.
       01  ttable-area.
           03  ttable              PIC 9(8) OCCURS Table-Len TIMES.

       PROCEDURE DIVISION USING ttable-area.
           MOVE FUNCTION RANDOM(FUNCTION CURRENT-DATE (11:6)) TO i

           PERFORM VARYING i FROM Table-Len BY -1 UNTIL i = 0
               COMPUTE j =
                   FUNCTION MOD(FUNCTION RANDOM * 10000, Table-Len) + 1

               MOVE ttable (i) TO temp
               MOVE ttable (j) TO ttable (i)
               MOVE temp TO ttable (j)
           END-PERFORM

           GOBACK
           .
```



## CMake


```cmake
# shuffle(<output variable> [<value>...]) shuffles the values, and
# stores the result in a list.
function(shuffle var)
  set(forever 1)

  # Receive ARGV1, ARGV2, ..., ARGV${last} as an array of values.
  math(EXPR last "${ARGC} - 1")

  # Shuffle the array with Knuth shuffle (Fisher-Yates shuffle).
  foreach(i RANGE ${last} 1)
    # Roll j = a random number from 1 to i.
    math(EXPR min "100000000 % ${i}")
    while(forever)
      string(RANDOM LENGTH 8 ALPHABET 0123456789 j)
      if(NOT j LESS min)        # Prevent modulo bias when j < min.
        break()                 # Break loop when j >= min.
      endif()
    endwhile()
    math(EXPR j "${j} % ${i} + 1")

    # Swap ARGV${i} with ARGV${j}.
    set(t ${ARGV${i}})
    set(ARGV${i} ${ARGV${j}})
    set(ARGV${j} ${t})
  endforeach(i)

  # Convert array to list.
  set(answer)
  foreach(i RANGE 1 ${last})
    list(APPEND answer ${ARGV${i}})
  endforeach(i)
  set("${var}" ${answer} PARENT_SCOPE)
endfunction(shuffle)
```



```cmake
shuffle(result 11 22 33 44 55 66)
message(STATUS "${result}")
# One possible output:
# -- 66;33;22;55;44;11
```



## CoffeeScript

```coffeescript
knuth_shuffle = (a) ->
  n = a.length
  while n > 1
    r = Math.floor(n * Math.random())
    n -= 1
    [a[n], a[r]] = [a[r], a[n]]
  a

counts =
  "1,2,3": 0
  "1,3,2": 0
  "2,1,3": 0
  "2,3,1": 0
  "3,1,2": 0
  "3,2,1": 0

for i in [1..100000]
  counts[knuth_shuffle([ 1, 2, 3 ]).join(",")] += 1

for key, val of counts
  console.log "#{key}: #{val}"
```

```txt

> coffee knuth_shuffle.coffee
1,2,3: 16714
1,3,2: 16566
2,1,3: 16460
2,3,1: 16715
3,1,2: 16750
3,2,1: 16795

```



## Common Lisp


```lisp
(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)
```

This operates on arbitrary sequences, but will be inefficient applied to a list as opposed to a vector.  Dispatching on type, and using an intermediate vector to hold the contents of list can make both cases more efficient (since the array specific case can use <code>aref</code> rather than <code>elt</code>):

```lisp
(defun nshuffle (sequence)
  (etypecase sequence
    (list  (nshuffle-list sequence))
    (array (nshuffle-array sequence))))

(defun nshuffle-list (list)
  "Shuffle the list using an intermediate vector."
  (let ((array (nshuffle-array (coerce list 'vector))))
    (declare (dynamic-extent array))
    (map-into list 'identity array)))

(defun nshuffle-array (array)
  (loop for i from (length array) downto 2
        do (rotatef (aref array (random i))
                    (aref array (1- i)))
        finally (return array)))
```



## Crystal


```crystal
def knuthShuffle(items : Array)
    i = items.size-1
    while i > 1
        j = Random.rand(0..i)
        items.swap(i, j)

        i -= 1
    end
end
```



## D


### Standard Version

A variant of the Knuth shuffle is in the D standard library Phobos:

```d
void main() {
    import std.stdio, std.random;

    auto a = [1, 2, 3, 4, 5, 6, 7, 8, 9];
    a.randomShuffle;
    a.writeln;
}
```

```txt
[8, 9, 3, 1, 7, 5, 4, 6, 2]
```



### One Implementation

This shuffles any collection that supports random access, length and swapping of items:

```d
import std.stdio, std.algorithm, std.random, std.range;

void knuthShuffle(Range)(Range r)
if (isRandomAccessRange!Range && hasLength!Range &&
    hasSwappableElements!Range) {
    foreach_reverse (immutable i, ref ri; r[1 .. $ - 1])
        ri.swap(r[uniform(0, i + 1)]);
}

void main() {
    auto a = [1, 2, 3, 4, 5, 6, 7, 8, 9];
    a.knuthShuffle;
    a.writeln;
}
```



## Delphi

''See [[Knuth_shuffle#Pascal|Pascal]] or [[Knuth_shuffle#DWScript|DWScript]]''


## DWScript


```delphi
procedure KnuthShuffle(a : array of Integer);
var
   i, j, tmp : Integer;
begin
   for i:=a.High downto 1 do begin
      j:=RandomInt(a.Length);
      tmp:=a[i]; a[i]:=a[j]; a[j]:=tmp;
   end;
end;
```



## E


```e
def shuffle(array, random) {
    for bound in (2..(array.size())).descending() {
        def i := random.nextInt(bound)
        def swapTo := bound - 1
        def t := array[swapTo]
        array[swapTo] := array[i]
        array[i] := t
    }
}
```


```e
? def arr := [1,2,3,4,5,6,7,8,9,10].diverge()
# value: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].diverge()

? shuffle(arr, entropy)
? arr
# value: [4, 5, 2, 9, 7, 8, 1, 3, 6, 10].diverge()
```



## EchoLisp


```scheme

Remark- The native '''shuffle''' function implementation  in EchoLisp has been replaced by this one.
 Thx Rosetta Code.
(lib 'list) ;; for list-permute

;; use "inside-out" algorithm, no swapping needed.
;; returns a random permutation vector of [0 .. n-1]
(define (rpv n (j))
(define v (make-vector n))
	(for [(i n)]
		(set! j (random (1+ i)))
		(when (!= i j) (vector-set! v i [v j]))
		(vector-set! v j i))
	v)

;; apply to any kind of list
(define (k-shuffle list)
	(list-permute list (vector->list (rpv (length list)))))

;; out
(k-shuffle (iota 17))
    → (16 7 11 10 0 9 15 12 13 8 4 2 14 3 6 5 1)

(k-shuffle
'(adrien 🎸 alexandre 🚂  antoine  🍼 ben 📚   georges 📷   julie 🎥 marine 🐼 nathalie 🍕 ))
    → (marine alexandre 🎥 julie 🎸 ben 🍼 nathalie 📚 georges 🚂 antoine adrien 🐼 📷 🍕)

(shuffle ;; native
'(adrien 🎸 alexandre 🚂 antoine 🍼 ben 📚 georges 📷 julie 🎥 marine 🐼 nathalie 🍕 ))
    → (antoine 🎥 🚂 marine adrien nathalie 🍼 🍕 ben 🐼 julie 📷 📚 🎸 alexandre georges)

```



## Egel


```Egel

import "prelude.eg"
import "random.ego"

using System
using List
using Math

def swap =
    [ I J XX -> insert I (nth J XX) (insert J (nth I XX) XX) ]

def shuffle =
    [ XX ->
        let INDICES = reverse (fromto 0 ((length XX) - 1)) in
        let SWAPS = map [ I -> I (between 0 I) ] INDICES in
            foldr [I J -> swap I J] XX SWAPS ]

def main = shuffle (fromto 1 9)

```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
		do
			test := <<1, 2>>
			io.put_string ("Initial: ")
			across
				test as t
			loop
				io.put_string (t.item.out + " ")
			end
			test := shuffle (test)
			io.new_line
			io.put_string ("Shuffled: ")
			across
				test as t
			loop
				io.put_string (t.item.out + " ")
			end
		end

	test: ARRAY [INTEGER]

	shuffle (ar: ARRAY [INTEGER]): ARRAY [INTEGER]
			-- Array containing the same elements as 'ar' in a shuffled order.
		require
			more_than_one_element: ar.count > 1
		local
			count, j, ith: INTEGER
			random: V_RANDOM
		do
			create random
			create Result.make_empty
			Result.deep_copy (ar)
			count := ar.count
			across
				1 |..| count as c
			loop
				j := random.bounded_item (c.item, count)
				ith := Result [c.item]
				Result [c.item] := Result [j]
				Result [j] := ith
				random.forth
			end
		ensure
			same_elements: across ar as a all Result.has (a.item) end
		end

end


``` >
```txt

Initial: 1 2 3 4 5 6 7
Shuffeld: 1 5 3 4 7 6 2

```




## Elena

ELENA 4.x:

```elena
import system'routines;
import extensions;

const int MAX = 10;

extension randomOp
{
    randomize()
    {
        var max := self.Length;

        for(int i := 0, i < max, i += 1)
        {
            var j := randomGenerator.eval(i,max);

            self.exchange(i,j)
        };

        ^ self
    }
}

public program()
{
    var a := Array.allocate:MAX.populate:(i => i );

    console.printLine(a.randomize())
}
```

```txt

7,3,6,8,4,9,0,1,2,5

```



## Elixir

```elixir
defmodule Knuth do
  def shuffle( inputs ) do
    n = length( inputs )
    {[], acc} = Enum.reduce( n..1, {inputs, []}, &random_move/2 )
    acc
  end

  defp random_move( n, {inputs, acc} ) do
    item = Enum.at( inputs, :rand.uniform(n)-1 )
    {List.delete( inputs, item ), [item | acc]}
  end
end

seq = Enum.to_list( 0..19 )
IO.inspect Knuth.shuffle( seq )

seq = [1,2,3]
Enum.reduce(1..100000, Map.new, fn _,acc ->
  k = Knuth.shuffle(seq)
  Map.update(acc, k, 1, &(&1+1))
end)
|> Enum.each(fn {k,v} -> IO.inspect {k,v} end)
```


```txt

[17, 13, 4, 2, 16, 1, 8, 19, 9, 12, 14, 5, 0, 11, 6, 10, 18, 3, 15, 7]
{[1, 2, 3], 16702}
{[1, 3, 2], 16635}
{[2, 1, 3], 16518}
{[2, 3, 1], 16935}
{[3, 1, 2], 16500}
{[3, 2, 1], 16710}

```



## Erlang


```Erlang

-module( knuth_shuffle ).

-export( [list/1] ).

list( Inputs ) ->
	N = erlang:length( Inputs ),
	{[], Acc} = lists:foldl( fun random_move/2, {Inputs, []}, lists:reverse(lists:seq(1, N)) ),
	Acc.



random_move( N, {Inputs, Acc} ) ->
	Item = lists:nth( random:uniform(N), Inputs ),
	{lists:delete(Item, Inputs), [Item | Acc]}.

```

```txt

21> knuth_shuffle:list(lists:seq(1,9)).
[5,7,8,1,4,2,3,9,6]

```



## ERRE


```ERRE
PROGRAM KNUTH_SHUFFLE

CONST CARDS%=52

DIM PACK%[CARDS%]

BEGIN
   RANDOMIZE(TIMER)
   FOR I%=1 TO CARDS% DO
      PACK%[I%]=I%
   END FOR
   FOR N%=CARDS% TO 2 STEP -1 DO
      SWAP(PACK%[N%],PACK%[1+INT(N%*RND(1))])
   END FOR
   FOR I%=1 TO CARDS% DO
      PRINT(PACK%[I%];)
   END FOR
   PRINT
END PROGRAM

```



## Euphoria

```Euphoria
sequence cards
cards = repeat(0,52)
integer card,temp

puts(1,"Before:\n")
for i = 1 to 52 do
    cards[i] = i
    printf(1,"%d ",cards[i])
end for

for i = 52 to 1 by -1 do
    card = rand(i)
    if card != i then
        temp = cards[card]
        cards[card] = cards[i]
        cards[i] = temp
    end if
end for

puts(1,"\nAfter:\n")
for i = 1 to 52 do
    printf(1,"%d ",cards[i])
end for
```



## Factor

There is a <code>randomize</code> word already in the standard library. Implementation:

```factor
: randomize ( seq -- seq )
    dup length [ dup 1 > ]
    [ [ iota random ] [ 1 - ] bi [ pick exchange ] keep ]
    while drop ;
```



## Fantom


```fantom
class Main
{
  static Void knuthShuffle (List array)
  {
    ((array.size-1)..1).each |Int i|
    {
      r := Int.random(0..i)
      array.swap (i, r)
    }
  }

  public static Void main ()
  {
    List a := [1,2,3,4,5]
    knuthShuffle (a)
    echo (a)

    List b := ["apples", "oranges", "pears", "bananas"]
    knuthShuffle (b)
    echo (b)
  }
}
```



## Forth


```forth
include random.fs

: shuffle ( deck size -- )
  2 swap do
    dup i random cells +
    over @ over @  swap
    rot  ! over !
    cell+
  -1 +loop drop ;

: .array   0 do dup @ . cell+ loop drop ;

create deck 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 ,

deck 10 2dup shuffle .array
```



## Fortran

```fortran
program Knuth_Shuffle
  implicit none

  integer, parameter :: reps = 1000000
  integer :: i, n
  integer, dimension(10) :: a, bins = 0, initial = (/ (n, n=1,10) /)

  do i = 1, reps
    a = initial
 	call Shuffle(a)
    where (a == initial) bins = bins + 1  ! skew tester
  end do
  write(*, "(10(i8))") bins
! prints  100382  100007   99783  100231  100507   99921   99941  100270  100290  100442

contains

subroutine Shuffle(a)
  integer, intent(inout) :: a(:)
  integer :: i, randpos, temp
  real :: r

  do i = size(a), 2, -1
    call random_number(r)
    randpos = int(r * i) + 1
    temp = a(randpos)
    a(randpos) = a(i)
    a(i) = temp
  end do

end subroutine Shuffle

end program Knuth_Shuffle
```


## FreeBASIC


```freebasic
' version 22-10-2016
' compile with: fbc -s console
' for boundry checks on array's compile with: fbc -s console -exx

' sort from lower bound to the highter bound
' array's can have subscript range from -2147483648 to +2147483647

Sub knuth_down(a() As Long)

    Dim As Long lb = LBound(a)
    Dim As ULong n = UBound(a) - lb +1
    Dim As ULong i, j

    Randomize Timer

    For i = n -1 To 1 Step -1
        j =Fix(Rnd * (i +1))       ' 0 <= j <= i
        Swap a(lb + i), a(lb + j)
    Next

End Sub

Sub knuth_up(a() As Long)

    Dim As Long lb = LBound(a)
    Dim As ULong n = UBound(a) - lb +1
    Dim As ULong i, j

    Randomize Timer

    For i = 0 To n -2
        j = Fix(Rnd * (n - i) + i)   '  0 <= j < n-i, + i ==> i <= j < n
        Swap a(lb + i), a(lb + j)
    Next

End Sub

' ------=< MAIN >=------

Dim As Long i
Dim As Long array(1 To 52), array2(-7 To 7)

For i = 1 To 52 : array(i) = i : Next

Print "Starting array"
For i = 1 To 52
    Print Using " ###";array(i);
Next : Print : Print

knuth_down(array())

Print "After Knuth shuffle downwards"
For i = 1 To 52
    Print Using " ###";array(i);
Next : Print : Print

For i = LBound(array2) To UBound(array2)
    array2(i) = i - LBound(array2) + 1
Next

Print "Starting array, first index <> 0 "
For i = LBound(array2) To UBound(array2)
    Print Using " ##";array2(i);
Next : Print : Print

knuth_up(array2())
Print "After Knuth shuffle upwards"
For i = LBound(array2) To UBound(array2)
    Print Using " ##";array2(i);
Next : Print : Print


' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
Starting array
   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25
  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50
  51  52

After Knuth shuffle downwards
   2  17  46   4  40  36  51  24  19  29  13   9   8  16  44  43  47  34  14  52  39  35  23  31  48
  42   7  12  21  33  18  32  22  49  38   6  27   1  41   5  20  15  37   3  28  30  26  45  50  25
  10  11

Starting array, first index <> 0
  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15

After Knuth shuffle upwards
  4  1  9 10 15 11 12  7  3  5  8 13  6 14  2
```



## Frink

The built-in method <CODE><I>array</I>.shuffle[]</CODE> implements the Fisher-Yates-Knuth shuffle algorithm:

```frink

a = [1,2,3]
a.shuffle[]

```


=={{header|F_Sharp|F#}}==
Allows a shuffle of arrays of arbitrary items.  Requires 2010 beta of F#.  Lazily returns a sequence.

This is the original Fisher-Yates shuffle as described by the link:

```fsharp
open System

let FisherYatesShuffle (initialList : array<'a>) =                  // '
    let availableFlags = Array.init initialList.Length (fun i -> (i, true))
                                                                    // Which items are available and their indices
    let rnd = new Random()
    let nextItem nLeft =
        let nItem = rnd.Next(0, nLeft)                              // Index out of available items
        let index =                                                 // Index in original deck
            availableFlags                                          // Go through available array
            |> Seq.filter (fun (ndx,f) -> f)                        // and pick out only the available tuples
            |> Seq.nth nItem                                        // Get the one at our chosen index
            |> fst                                                  // and retrieve it's index into the original array
        availableFlags.[index] <- (index, false)                    // Mark that index as unavailable
        initialList.[index]                                         // and return the original item
    seq {(initialList.Length) .. -1 .. 1}                           // Going from the length of the list down to 1
    |> Seq.map (fun i -> nextItem i)                                // yield the next item
```

Here's the modified Knuth shuffle which shuffles the original array in place

```fsharp
let KnuthShuffle (lst : array<'a>) =                   // '
    let Swap i j =                                                  // Standard swap
        let item = lst.[i]
        lst.[i] <- lst.[j]
        lst.[j] <- item
    let rnd = new Random()
    let ln = lst.Length
    [0..(ln - 2)]                                                   // For all indices except the last
    |> Seq.iter (fun i -> Swap i (rnd.Next(i, ln)))                 // swap th item at the index with a random one following it (or itself)
    lst                                                             // Return the list shuffled in place
```

Example:

```fsharp>
 KnuthShuffle [| "Darrell"; "Marvin"; "Doug"; "Greg"; "Sam"; "Ken" |];;
val it : string array = [|"Marvin"; "Doug"; "Sam"; "Darrell"; "Ken"; "Greg"|]
```



## FunL


```funl
def shuffle( a ) =
  res = array( a )
  n = a.length()

  for i <- 0:n
    r = rnd( i:n )
    res(i), res(r) = res(r), res(i)

  res.toList()
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=58402023fbdc617ce10f6a85db721105 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim iTotal As Integer = 40
Dim iCount, iRand1, iRand2 As Integer
Dim iArray As New Integer[]

For iCount = 0 To iTotal
  iArray.add(iCount)
Next

Print "Original = ";
For iCount = 0 To iArray.Max
  If iCount = iArray.max Then Print iArray[iCount]; Else Print iArray[iCount] & ",";
Next

For iCount = iTotal DownTo 0
  iRand1 = Rand(iTotal)
  iRand2 = Rand(iTotal)
  Swap iArray[iRand1], iArray[iRand2]
Next

Print gb.NewLine & "Shuffled = ";
For iCount = 0 To iArray.Max
  If iCount = iArray.max Then Print iArray[iCount]; Else Print iArray[iCount] & ",";
Next

End
```

Output:

```txt

Original = 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40
Shuffled = 8,23,12,31,4,38,39,40,37,34,14,0,21,22,3,10,27,26,17,15,6,7,19,2,24,35,25,16,18,36,1,13,32,33,20,5,9,11,29,28,30

```



## GAP


```gap
# Return the list L after applying Knuth shuffle. GAP also has the function Shuffle, which does the same.
ShuffleAlt := function(a)
    local i, j, n, t;
    n := Length(a);
    for i in [n, n - 1 .. 2] do
        j := Random(1, i);
        t := a[i];
        a[i] := a[j];
        a[j] := t;
    od;
    return a;
end;

# Return a "Permutation" object (a permutation of 1 .. n).
# They are printed in GAP, in cycle decomposition form.
PermShuffle := n -> PermList(ShuffleAlt([1 .. n]));

ShuffleAlt([1 .. 10]);
# [ 4, 7, 1, 5, 8, 2, 6, 9, 10, 3 ]

PermShuffle(10);
# (1,9)(2,3,6,4,5,10,8,7)

# One may also call the built-in random generator on the symmetric group :
Random(SymmetricGroup(10));
(1,8,2,5,9,6)(3,4,10,7)
```



## Go

(Note, in addition to these examples,
<code>[https://golang.org/pkg/math/rand/#Shuffle rand.Shuffle]</code>
was added in [https://golang.org/doc/go1.10#math/rand Go1.10]
implementing a Fisher–Yates shuffle.)


```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    var a [20]int
    for i := range a {
        a[i] = i
    }
    fmt.Println(a)

    rand.Seed(time.Now().UnixNano())
    for i := len(a) - 1; i >= 1; i-- {
        j := rand.Intn(i + 1)
        a[i], a[j] = a[j], a[i]
    }
    fmt.Println(a)
}
```

To shuffle any type:

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

// Generic Knuth Shuffle algorithm.  In Go, this is done with interface
// types.  The parameter s of function shuffle is an interface type.
// Any type satisfying the interface "shuffler" can be shuffled with
// this function.  Since the shuffle function uses the random number
// generator, it's nice to seed the generator at program load time.
func init() {
    rand.Seed(time.Now().UnixNano())
}
func shuffle(s shuffler) {
    for i := s.Len() - 1; i >= 1; i-- {
        j := rand.Intn(i + 1)
        s.Swap(i, j)
    }
}

// Conceptually, a shuffler is an indexed collection of things.
// It requires just two simple methods.
type shuffler interface {
    Len() int      // number of things in the collection
    Swap(i, j int) // swap the two things indexed by i and j
}

// ints is an example of a concrete type implementing the shuffler
// interface.
type ints []int

func (s ints) Len() int      { return len(s) }
func (s ints) Swap(i, j int) { s[i], s[j] = s[j], s[i] }

// Example program.  Make an ints collection, fill with sequential numbers,
// print, shuffle, print.
func main() {
    a := make(ints, 20)
    for i := range a {
        a[i] = i
    }
    fmt.Println(a)
    shuffle(a)
    fmt.Println(a)
}
```

{{out|Example output}} (of either program)

```txt

[0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19]
[11 10 12 19 4 13 15 17 14 2 5 18 8 0 6 9 7 3 1 16]

```



## Groovy

Solution:

```groovy
def shuffle = { list ->
    if (list == null || list.empty) return list
    def r = new Random()
    def n = list.size()
    (n..1).each { i ->
        def j = r.nextInt(i)
        list[[i-1, j]] = list[[j, i-1]]
    }
    list
}
```

Test:

```groovy
def list = [] + (0..20)
println list
println shuffle(list)
println shuffle(list)
println shuffle(list)
```

```txt
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
[12, 16, 7, 13, 1, 9, 17, 20, 15, 3, 5, 6, 8, 0, 18, 10, 14, 4, 2, 11, 19]
[17, 6, 10, 1, 18, 5, 7, 13, 2, 11, 16, 3, 14, 0, 4, 20, 19, 12, 8, 9, 15]
[6, 20, 11, 4, 7, 12, 5, 14, 19, 18, 13, 15, 1, 2, 8, 16, 17, 10, 0, 9, 3]
```



## Haskell


```Haskell
import System.Random (randomRIO)

mkRands :: Int -> IO [Int]
mkRands = mapM (randomRIO . (,) 0) . enumFromTo 1 . pred

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i c l =
  let (a, b) = splitAt i l
  in a ++ c : drop 1 b

swapElems :: (Int, Int) -> [a] -> [a]
swapElems (i, j) xs
  | i == j = xs
  | otherwise = replaceAt j (xs !! i) $ replaceAt i (xs !! j) xs

knuthShuffle :: [a] -> IO [a]
knuthShuffle xs = (foldr swapElems xs . zip [1 ..]) <$> mkRands (length xs)
```


or, as an alternative to making two indexed references into the list with '''(!!)''':

```haskell
import System.Random (randomRIO)
import Data.Bool (bool)

knuthShuffle :: [a] -> IO [a]
knuthShuffle xs = (foldr swapped xs . zip [1 ..]) <$> randoms (length xs)

swapped :: (Int, Int) -> [a] -> [a]
swapped (i, j) xs =
  let go (a, b)
        | a == b = xs
        | otherwise =
          let (m, n) = bool (b, a) (a, b) (b > a)
              (l, hi:t) = splitAt m xs
              (ys, lo:zs) = splitAt (pred (n - m)) t
          in concat [l, lo : ys, hi : zs]
  in bool xs (go (i, j)) $ ((&&) . (i <) <*> (j <)) $ length xs

randoms :: Int -> IO [Int]
randoms x = mapM (randomRIO . (,) 0) [1 .. pred x]

main :: IO ()
main = knuthShuffle ['a' .. 'k'] >>= print
```


Examples of use of either of the two versions above:

```txt
*Main> knuthShuffle  ['a'..'k']
"bhjdgfciake"

*Main> knuthShuffle $ map(ap (,)(+10)) [0..9]
[(0,10),(8,18),(2,12),(3,13),(9,19),(4,14),(7,17),(1,11),(6,16),(5,15)]
```

Function for showing intermediate results:

```Haskell
knuthShuffleProcess :: (Show a) => [a] -> IO ()
knuthShuffleProcess =
   (mapM_ print. reverse =<<). ap (fmap. (. zip [1..]). scanr swapElems) (mkRands. length)
```

{{out}} Detailed example:

```txt
*Main> knuthShuffleProcess  ['a'..'k']
"abcdefghijk"
"abckefghijd"
"jbckefghiad"
"jbckeighfad"
"jbckeihgfad"
"jbhkeicgfad"
"jbhiekcgfad"
"jbeihkcgfad"
"ibejhkcgfad"
"iebjhkcgfad"
"iebjhkcgfad"
```

An imperative implementation using arrays and the <code>ST</code> monad:

```haskell
import Data.Array.ST
import Data.STRef
import Control.Monad
import Control.Monad.ST
import Control.Arrow
import System.Random

shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle list g = runST $ do
    r <- newSTRef g
    let rand range = liftM (randomR range) (readSTRef r) >>=
            runKleisli (second (Kleisli $ writeSTRef r) >>> arr fst)
    a <- newAry (1, len) list
    forM_ [len, len - 1 .. 2] $ \n -> do
        k <- rand (1, n)
        liftM2 (,) (readArray a k) (readArray a n) >>=
           runKleisli (Kleisli (writeArray a n) *** Kleisli (writeArray a k))
    liftM2 (,) (getElems a) (readSTRef r)
  where len = length list
        newAry :: (Int, Int) -> [a] -> ST s (STArray s Int a)
        newAry = newListArray
```


=={{header|Icon}} and {{header|Unicon}}==
The <tt>shuffle</tt> method used here can shuffle lists, record fields, and strings:

```icon
procedure main()
    show(shuffle([3,1,4,1,5,9,2,6,3]))
    show(shuffle("this is a string"))
end

procedure shuffle(A)
    every A[i := *A to 1 by -1] :=: A[?i]
    return A
end

procedure show(A)
    every writes(!A," ")
    write()
end
```

```txt
->ks
9 6 1 4 3 1 3 5 2
i n   t i s   r t g   h s a i s
->
```

Note that the gloriously succinct 'standard' Icon shuffle:

```icon
procedure shuffle(A)
    every !A :=: ?A
end
```

is subtly biased.


## Inform 6


```Inform 6
[ shuffle a n i j tmp;
  for(i = n - 1: i > 0: i--)
  {
    j = random(i + 1) - 1;

    tmp = a->j;
    a->j = a->i;
    a->i = tmp;
  }
];
```



## J


```j
KS=:{~ (2&{.@[ {`(|.@[)`]} ])/@(,~(,.?@>:))@i.@#
```

The input array is transformed to a rectangular array of indexes. By doing this all kinds of arrays can serve as input (see examples below). The process is imitated by using using a fold, swapping elements in a restricted part of this index-array in each fold step.

```j
process                         J

 fold swap transform array   <==>  f / g y
```

Example of a transformed input:

```j
(,~(,.?@>:))@i.@# 1+i.6
0 0 0 0 0 0
1 1 0 0 0 0
2 0 0 0 0 0
3 2 0 0 0 0
4 3 0 0 0 0
5 0 0 0 0 0
0 1 2 3 4 5
```

The last row is the index-array that has to be shuffled. The other rows have valid indexes in the first two columns. The second column has a randomized value <= value first column.

The index-swapping is done by the part:

```j
2&{.@[ {`(|.@[)`]} ]
```

Finally, the shuffled indexes select elements from the original array.

```j
input { ~ shuffled indexes
```

Alternatively, instead of creating a rectangular array, the swapping indices and the original data can be individually boxed.

In other words, <code>(,~ (,. ?@>:))@i.@#</code> can be replaced with <code>|.@; ;&~./@(,. ?@>:)@i.@#</code>, and the swapping can be achieved using <code>(<@C. >)/</code> instead of <code>(2&{.@[ {`(|.@[)`]} ])/</code>.

With this approach, the data structure with the swapping indices and the original data could look like this:

```j
    (|.@; ;&~./@(,. ?@>:)@i.@#)'abcde'
+---+-+---+---+-+-----+
|4 2|3|2 1|1 0|0|abcde|
+---+-+---+---+-+-----+
```

Note that we have the original data here, instead of indices to select all of its items.  Note also that we have only a single value in a box where an item is being "swapped" with itself (this is required by J's cycle operation (<code>C.</code>)).

The resulting definition looks like this:

```j
KS=: [:>
 (<@C. >)/@(|.@; ;&~./@(,. ?@>:)@i.@#)
```

Note that here we did not wind up with a list of indices which we used to permute the original data set.  That data set is permuted directly.  However, it is in a box and we do have to remove it from that box.

Permuting the data directly, instead of permuting indices, has performance implications when the items being swapped are large, but see the note at the end of this entry for J for how you would do this operation in a "real" J program.

Examples:
```j
]A=: 5+i.9
5 6 7 8 9 10 11 12 13
```
 Shuffle:

```j
KS A
13 10 7 5 11 9 8 6 12
```
Input

```j
]M=: /:~(1 2 3,:2 3 4),(11 2 3,: 0 11 2),(1 1 1,:1 0),:1 1 1,:1 0 1
 1  1 1
 1  0 0

 1  1 1
 1  0 1

 1  2 3
 2  3 4

11  2 3
 0 11 2
```
Shuffle

```j
KS M
11  2 3
 0 11 2

 1  1 1
 1  0 1

 1  1 1
 1  0 0

 1  2 3
 2  3 4
```
Input

```j
]L=:'aA';'bbB';'cC%$';'dD@'
+--+---+----+---+
|aA|bbB|cC%$|dD@|
+--+---+----+---+
```
Shuffle

```j
KS L
+--+----+---+---+
|aA|cC%$|dD@|bbB|
+--+----+---+---+
```

In J the shuffling of an arbitrary array can easily be implemented by the phrase
( ref http://www.jsoftware.com/jwiki/JPhrases/RandomNumbers ):

```j
({~?~@#)
```

Applied on the former examples:

```j
({~?~@#) A
8 7 13 6 10 11 5 9 12

   ({~?~@#) M
 1  1 1
 1  0 1

 1  2 3
 2  3 4

11  2 3
 0 11 2

 1  1 1
 1  0 0

   ({~?~@#) L
+----+---+--+---+
|cC%$|bbB|aA|dD@|
+----+---+--+---+
```



## Java


```java
import java.util.Random;

public static final Random gen = new Random();

// version for array of ints
public static void shuffle (int[] array) {
    int n = array.length;
    while (n > 1) {
        int k = gen.nextInt(n--); //decrements after using the value
        int temp = array[n];
        array[n] = array[k];
        array[k] = temp;
    }
}
// version for array of references
public static void shuffle (Object[] array) {
    int n = array.length;
    while (n > 1) {
        int k = gen.nextInt(n--); //decrements after using the value
        Object temp = array[n];
        array[n] = array[k];
        array[k] = temp;
    }
}
```



## JavaScript



### ES5



```javascript
function knuthShuffle(arr) {
    var rand, temp, i;

    for (i = arr.length - 1; i > 0; i -= 1) {
        rand = Math.floor((i + 1) * Math.random());//get random between zero and i (inclusive)
        temp = arr[rand];//swap i and the zero-indexed number
        arr[rand] = arr[i];
        arr[i] = temp;
    }
    return arr;
}

var res = {
    '1,2,3': 0, '1,3,2': 0,
    '2,1,3': 0, '2,3,1': 0,
    '3,1,2': 0, '3,2,1': 0
};

for (var i = 0; i < 100000; i++) {
    res[knuthShuffle([1,2,3]).join(',')] += 1;
}

for (var key in res) {
    print(key + "\t" + res[key]);
}
```

Results in:

```txt
1,2,3   16619
1,3,2   16614
2,1,3   16752
2,3,1   16959
3,1,2   16460
3,2,1   16596
```




### ES6


====Mutating in-place swap====

```JavaScript
(() => {

    // knuthShuffle :: [a] -> [a]
    const knuthShuffle = xs =>
        enumFromTo(0, xs.length - 1)
        .reduceRight((a, i) => {
            const
                iRand =  randomRInt(0, i),
                tmp = a[iRand];
            return iRand !== i ? (
                a[iRand] = a[i],
                a[i] = tmp,
                a
            ) : a;
        }, xs);

    const test = () => knuthShuffle(
        (`alpha beta gamma delta epsilon zeta
              eta theta iota kappa lambda mu`)
        .split(/\s+/)
    );

    // GENERIC FUNCTIONS ----------------------------------

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        n >= m ? (
            iterateUntil(x => x >= n, x => 1 + x, m)
        ) : [];

    // iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
    const iterateUntil = (p, f, x) => {
        let vs = [x],
            h = x;
        while (!p(h))(h = f(h), vs.push(h));
        return vs;
    };

    // randomRInt :: Int -> Int -> Int
    const randomRInt = (low, high) =>
        low + Math.floor(
            (Math.random() * ((high - low) + 1))
        );

    return test();
})();

```


e.g.

```JavaScript
["iota", "epsilon", "kappa", "theta", "gamma", "delta",
"lambda", "eta", "zeta", "beta", "mu", "alpha"]
```


====Non-mutating swap====

```JavaScript
(() => {

    // knuthShuffle :: [a] -> [a]
    const knuthShuffle = xs =>
        enumFromTo(0, xs.length - 1)
        .reduceRight((a, i) => {
            const iRand = randomRInt(0, i);
            return i !== iRand ? (
                swapped(i, iRand, a)
            ) : a;
        }, xs);

    const test = () => knuthShuffle(
        (`alpha beta gamma delta epsilon zeta
          eta theta iota kappa lambda mu`)
        .split(/\s+/)
    );

    // Non mutating version of swapped

    // swapped :: Int -> Int -> [a] -> [a]
    const swapped = (iFrom, iTo, xs) =>
        xs.map(
            (x, i) => iFrom !== i ? (
                iTo !== i ? (
                    x
                ) : xs[iFrom]
            ) : xs[iTo]
        );

    // GENERIC FUNCTIONS ----------------------------------

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        n >= m ? (
            iterateUntil(x => x >= n, x => 1 + x, m)
        ) : [];

    // iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
    const iterateUntil = (p, f, x) => {
        let vs = [x],
            h = x;
        while (!p(h))(h = f(h), vs.push(h));
        return vs;
    };

    // randomRInt :: Int -> Int -> Int
    const randomRInt = (low, high) =>
        low + Math.floor(
            (Math.random() * ((high - low) + 1))
        );

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) =>
        Array.from({
            length: Math.min(xs.length, ys.length)
        }, (_, i) => f(xs[i], ys[i], i));

    // MAIN ---
    return test();
})();
```

e.g.

```JavaScript
["mu", "theta", "beta", "eta", "delta", "epsilon",
"kappa", "alpha", "gamma", "lambda", "zeta", "iota"]
```



## Joy


```Joy
DEFINE knuth-shuffle ==

(* Take the size of the array (without destroying it) *)
dup dup size

(* Generate a list of as many random numbers *)
[rand] [rem] enconcat map

(* Zip the two lists *)
swap zip

(* Sort according to the new index number *)
[small] [] [uncons unswonsd [first >] split [swons] dip2]
[enconcat] binrec

(* Delete the new index number *)
[second] map.
```

Using knuth-shuffle (file shuffle.joy):

```Joy
(* Sorted array of 21 integers *)
[ 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20]
knuth-shuffle.
```

Command line:
: <tt>joy shuffle.joy</tt>
```txt

usrlib  is loaded
inilib  is loaded
agglib  is loaded
[12 6 8 4 14 18 7 15 1 0 11 13 5 10 16 2 19 17 9 20 3]

```



## Julia

```julia
function knuthshuffle!(r::AbstractRNG, v::AbstractVector)
    for i in length(v):-1:2
        j = rand(r, 1:i)
        v[i], v[j] = v[j], v[i]
    end
    return v
end
knuthshuffle!(v::AbstractVector) = knuthshuffle!(Base.Random.GLOBAL_RNG, v)

v = collect(1:20)
println("# v = $v\n   -> ", knuthshuffle!(v))
```


```txt
# v = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
   -> [16, 5, 17, 10, 2, 7, 20, 14, 4, 8, 19, 15, 18, 12, 11, 1, 9, 13, 3, 6]
```



## Kotlin


```scala
object Knuth {
    internal val gen = java.util.Random()
}

fun <T> Array<T>.shuffle(): Array<T> {
    val a = clone()
    var n = a.size
    while (n > 1) {
        val k = Knuth.gen.nextInt(n--)
        val t = a[n]
        a[n] = a[k]
        a[k] = t
    }
    return a
}

fun main(args: Array<String>) {
    val str = "abcdefghijklmnopqrstuvwxyz".toCharArray()
    (1..10).forEach {
        val s = str.toTypedArray().shuffle().toCharArray()
        println(s)
        require(s.toSortedSet() == str.toSortedSet())
    }

    val ia = arrayOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    (1..10).forEach {
        val s = ia.shuffle()
        println(s.distinct())
        require(s.toSortedSet() == ia.toSet())
    }
}
```

```txt
xdhsvtnumjgbywiqoapcelkrfz
pjnegbiyzuhsrclodftwkmaqvx
bkmqwhzregifyanvsltxjupodc
ewhxrlybnjqpvdsozaimkucgft
pdqgoaymbzefnjrwuvilsckxht
kcpagyuehjswdtvnzfrlbxqomi
iztsmaygkblephcjfnwvxurdoq
pltdyjwivsehckzfaxruqogmbn
nytfbpmjicgkaueoxwrhlsqvdz
epucijbvrhwyzdlsqftagxmkon
[7, 4, 5, 9, 2, 1, 3, 8, 10, 6]
[8, 10, 5, 4, 3, 6, 1, 2, 7, 9]
[7, 9, 2, 1, 10, 4, 6, 5, 8, 3]
[9, 6, 1, 8, 2, 5, 10, 3, 4, 7]
[7, 3, 6, 9, 10, 2, 5, 4, 1, 8]
[2, 9, 1, 7, 5, 10, 8, 4, 6, 3]
[4, 2, 7, 3, 8, 5, 6, 10, 1, 9]
[4, 8, 7, 6, 10, 5, 2, 1, 3, 9]
[6, 3, 9, 4, 5, 2, 10, 8, 1, 7]
[3, 6, 9, 2, 10, 8, 7, 5, 1, 4]
```



## LabVIEW

{{works with|LabVIEW|8.0 Full Development System}}<br/><br/>
[[File:Knuth_shuffle_panel.png|200px]]
[[File:Knuth_shuffle_diagram.png|200px]]



## Lasso


```lasso
define staticarray->swap(p1::integer,p2::integer) => {
    fail_if(
        #p1 < 1 or #p2 < 1 or
        #p1 > .size or #p2 > .size,
        'invalid parameters'
    )
    #p1 == #p2
        ? return

    local(tmp) = .get(#p2)
    .get(#p2)  = .get(#p1)
    .get(#p1)  = #tmp
}
define staticarray->knuthShuffle => {
    loop(-from=.size, -to=2, -by=-1) => {
        .swap(math_random(1, loop_count), loop_count)
    }
}

(1 to 10)->asStaticArray->knuthShuffle&asString
```

```txt
staticarray(9, 5, 6, 1, 10, 8, 3, 4, 2, 7)
```




## Liberty BASIC


```lb
'Declared the UpperBound to prevent confusion with lots of 9's floating around....
UpperBound = 9
Dim array(UpperBound)

For i = 0 To UpperBound
    array(i) = Int(Rnd(1) * 10)
    Print array(i)
Next i

For i = 0 To UpperBound
    'set a random value because we will need to use the same value twice
    randval = Int(Rnd(1) * (UpperBound - i))
    temp1 = array(randval)
    temp2 = array((UpperBound - i))
    array(randval) = temp2
    array((UpperBound - i)) = temp1
Next i

Print
For i = 0 To UpperBound
    Print array(i)
Next i
```



## Logo


```logo
to swap :i :j :a
  localmake "t item :i :a
  setitem :i :a item :j :a
  setitem :j :a :t
end
to shuffle :a
  for [i [count :a] 2] [swap 1 + random :i :i :a]
end

make "a {1 2 3 4 5 6 7 8 9 10}
shuffle :a
show :a
```

Lhogho does not have a setitem, and also does things more 'function'ally.

```logo
to slice :lst :start :finish
	local "res
	make "res []
	for "i [:start :finish 1] [
		make "j item :i :lst
		make "res se :res :j
	]
	op :res
end

to setitem :n :lst :val
	local "lhs
	local "rhs
	make "lhs slice :lst 1 :n-1
	make "rhs slice :lst :n+1 count :lst
	op (se :lhs :val :rhs)
end

to swap :i :j :a
	local "t
	make "t item :i :a
	make "a setitem :i :a item :j :a
	make "a setitem :j :a :t
	op :a
end

to shuffle :a
	for "i [count :a 2]
	[
		make "a swap 1 + random :i :i :a
	]
	op :a
end

make "a ( list 1 2 3 4 5 6 7 8 9 10 )
make "a shuffle :a
show :a
```



## Lua


```lua
function table.shuffle(t)
  for n = #t, 1, -1 do
    local k = math.random(n)
    t[n], t[k] = t[k], t[n]
  end

  return t
end

math.randomseed( os.time() )
a = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
table.shuffle(a)
for i,v in ipairs(a) do print(i,v) end
```




## M2000 Interpreter


```M2000 Interpreter

Dim Base 0, A(3)
For k=1 to 6 {
      A(0):=10,20, 30
      For i=len(A())-1 to 0 {
            let j=random(0,i)
            Swap a(i), a(j)
      }
 Print A()
}


```



## M4


```M4
divert(-1)
define(`randSeed',141592653)
define(`rand_t',`eval(randSeed^(randSeed>>13))')
define(`random',
   `define(`randSeed',eval((rand_t^(rand_t<<18))&0x7fffffff))randSeed')
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',incr($2),$3,`$4')')')')
define(`set',`define(`$1[$2]',`$3')')
define(`get',`defn($1[$2])')
define(`new',`set($1,size,0)')
define(`deck',
   `new($1)for(`x',1,$2,
         `set(`$1',x,x)')`'set(`$1',size,$2)')
define(`show',
   `for(`x',1,get($1,size),`get($1,x)`'ifelse(x,get($1,size),`',`, ')')')
define(`swap',`set($1,$2,get($1,$4))`'set($1,$4,$3)')
define(`shuffle',
   `define(`s',get($1,size))`'for(`x',1,decr(s),
      `swap($1,x,get($1,x),eval(x+random%(s-x+1)))')')
divert

deck(`b',52)
show(`b')
shuffle(`b')
show(`b')
```

```txt

1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43,
44, 45, 46, 47, 48, 49, 50, 51, 52

6, 22, 33, 51, 35, 45, 16, 32, 7, 34, 10, 44, 5, 38, 43, 25, 29, 9, 37, 20, 21,
48, 24, 46, 8, 26, 41, 47, 49, 36, 14, 31, 15, 39, 12, 17, 13, 1, 3, 4, 27, 11,
28, 2, 19, 30, 42, 50, 18, 52, 40, 23

```



## Mathematica

Usage of built-in function:

```Mathematica
RandomSample[{1, 2, 3, 4, 5, 6}]
```

Custom function:

```Mathematica
Shuffle[input_List /; Length[input] >= 1] :=
 Module[{indices = {}, allindices = Range[Length[input]]},
  Do[
   AppendTo[indices,
     Complement[allindices, indices][[RandomInteger[{1, i}]]]];
   ,
   {i, Length[input], 1, -1}
   ];
  input[[indices]]
  ]
```

Example:

```Mathematica
Shuffle[{1, 2, 3, 4, 5, 6}]
```



## MATLAB

Because this shuffle is done using rounds of operations on subsets of decreasing size, this is not an algorithm that can be vectorized using built-in MATLAB functions. So, we have to go old-school, no fancy MATLAB trickery.

```MATLAB
function list = knuthShuffle(list)

    for i = (numel(list):-1:2)

        j = floor(i*rand(1) + 1); %Generate random int between 1 and i

        %Swap element i with element j.
        list([j i]) = list([i j]);
    end
end
```

There is an alternate way to do this that is not a true Knuth Shuffle, but operates with the same spirit.
This alternate version produces the same output, saves some space,
and can be implemented in-line without the need to encapsulate it
in a function call like the Knuth Shuffle.

```MATLAB
function list = randSort(list)

    list = list( randperm(numel(list)) );

end
```



## Maxima


```maxima
/* Maxima has an implementation of Knuth shuffle */
random_permutation([a, b, c]);
```


=={{header|Modula-3}}==

```modula3
MODULE Shuffle EXPORTS Main;

IMPORT IO, Fmt, Random;

VAR a := ARRAY [0..9] OF INTEGER {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

PROCEDURE Shuffle(VAR a: ARRAY OF INTEGER) =
  VAR temp: INTEGER;
      n: INTEGER := NUMBER(a);
BEGIN
  WITH rand = NEW(Random.Default).init() DO
    WHILE n > 1 DO
      WITH k = rand.integer(0, n - 1) DO
        DEC(n);
        temp := a[n];
        a[n] := a[k];
        a[k] := temp;
      END;
    END;
  END;
END Shuffle;

BEGIN
  Shuffle(a);
  FOR i := FIRST(a) TO LAST(a) DO
    IO.Put(Fmt.Int(a[i]) & " ");
  END;
  IO.Put("\n");
END Shuffle.
```

```txt

martin@thinkpad:~$ ./shuffle
9 2 7 3 6 8 4 5 1 10
martin@thinkpad:~$ ./shuffle
1 7 8 10 5 4 6 3 9 2

```



## MUMPS


```MUMPS
Shuffle(items,separator)	New ii,item,list,n
	Set list="",n=0
	Set ii="" For  Set ii=$Order(items(ii)) Quit:ii=""  Do
	. Set n=n+1,list(n)=items(ii),list=list_$Char(n)
	. Quit
	For  Quit:list=""  Do
	. Set n=$Random($Length(list))+1
	. Set item=list($ASCII(list,n))
	. Set $Extract(list,n)=""
	. Write item,separator
	. Quit
	Quit
CardDeck	New card,ii,suite
	Set ii=0
	For suite="Spades","Hearts","Clubs","Diamonds" Do
	. For card=2:1:10,"Jack","Queen","King","Ace" Do
	. . Set ii=ii+1,items(ii)=card_" of "_suite
	. . Quit
	. Quit
	Quit

Kill items
Set items(91)="Red"
Set items(82)="White"
Set items(73)="Blue"
Set items(64)="Yellow"
Set items(55)="Green"
Do Shuffle(.items,"  ") ; Red  Yellow  White  Green  Blue
Do Shuffle(.items,"  ") ; Red  Blue  Yellow  White  Green
Do Shuffle(.items,"  ") ; Green  Blue  Yellow  White  Red

Kill items Do CardDeck,Shuffle(.items,$Char(13,10))
Queen of Hearts
9 of Diamonds
10 of Hearts
King of Hearts
7 of Diamonds
9 of Clubs
6 of Diamonds
8 of Diamonds
Jack of Spades
Ace of Hearts
Queen of Diamonds
9 of Hearts
2 of Hearts
King of Clubs
10 of Spades
7 of Clubs
6 of Clubs
3 of Diamonds
3 of Spades
Queen of Clubs
Ace of Spades
4 of Hearts
Ace of Diamonds
7 of Spades
Ace of Clubs
King of Spades
10 of Diamonds
Jack of Diamonds
8 of Clubs
4 of Spades
Jack of Hearts
10 of Clubs
4 of Diamonds
3 of Hearts
2 of Diamonds
5 of Hearts
Jack of Clubs
2 of Clubs
5 of Diamonds
6 of Hearts
4 of Clubs
9 of Spades
3 of Clubs
5 of Spades
6 of Spades
7 of Hearts
8 of Spades
8 of Hearts
2 of Spades
Queen of Spades
King of Diamonds
5 of Clubs
```



## Nemerle


```Nemerle
Shuffle[T] (arr : array[T]) : array[T]
{
    def rnd = Random();

    foreach (i in [0 .. (arr.Length - 2)])
        arr[i] <-> arr[(rnd.Next(i, arr.Length))];
    arr
}
```



## NetRexx


### version 1


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary

import java.util.List

cards = [String -
    'hA', 'h2', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8', 'h9', 'h10', 'hJ', 'hQ', 'hK' -
  , 'cA', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9', 'c10', 'cJ', 'cQ', 'cK' -
  , 'dA', 'd2', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8', 'd9', 'd10', 'dJ', 'dQ', 'dK' -
  , 'sA', 's2', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 's10', 'sJ', 'sQ', 'sK' -
]
cardsLen = cards.length
deck = ArrayList(cardsLen)
loop c_ = 0 to cardsLen - 1
  deck.add(String(cards[c_]))
  end c_

showHand(deck)
deck = ArrayList shuffle(deck)
showHand(deck)

return

method shuffle(deck = List) public static binary returns List

  rn = Random()
  dl = deck.size

  loop i_ = dl - 1 to 1 by -1
    j_ = rn.nextInt(i_)
    __ = deck.get(i_)
    deck.set(i_, deck.get(j_))
    deck.set(j_, __)
    end i_

  return deck

method showHand(deck = ArrayList) public static binary

  dl = deck.size
  hl = dl % 4
  loop c_ = 0 to dl - 1 by hl
    d_ = c_ + hl
    if d_ >= dl then d_ = dl
    say ArrayList(deck.subList(c_, d_)).toString
    end c_
    say

  return
```

```txt

[hA, h2, h3, h4, h5, h6, h7, h8, h9, h10, hJ, hQ, hK]
[cA, c2, c3, c4, c5, c6, c7, c8, c9, c10, cJ, cQ, cK]
[dA, d2, d3, d4, d5, d6, d7, d8, d9, d10, dJ, dQ, dK]
[sA, s2, s3, s4, s5, s6, s7, s8, s9, s10, sJ, sQ, sK]

[s8, c10, sJ, c8, h10, h3, s3, d6, hJ, d3, c7, h5, s5]
[h8, d10, cK, s6, dQ, d9, d4, c4, c6, h6, cA, sA, dK]
[dJ, dA, d7, c2, d2, s10, sK, h2, c5, s7, cJ, d5, h9]
[c9, d8, c3, s9, cQ, sQ, h4, s4, hQ, h7, hK, hA, s2]

```



### version 2


```NetRexx
/* NetRexx ------------------------------------------------------------
* 08.01.2014 Walter Pachl modified to show state development a la Rexx
*--------------------------------------------------------------------*/
options replace format comments java crossref savelog symbols nobinary

import java.util.List

cards = [String '1','2','3','4','5','6','7','8','9','10']
cardsLen = cards.length
deck = ArrayList(cardsLen)
loop c_ = 0 to cardsLen - 1
  deck.add(String(cards[c_]))
  end c_

showHand(deck,'In ')
deck = ArrayList shuffle(deck)
showHand(deck,'Out')
return

method shuffle(deck = List) public static binary returns List
  rn = Random()
  dl = deck.size
  loop i_ = dl - 1 to 1 by -1
    j_ = rn.nextInt(i_)
    __ = deck.get(i_)
    deck.set(i_, deck.get(j_))
    deck.set(j_, __)
    say i_ j_ ArrayList(deck.subList(0,i_+1)).toString
    end i_
  return deck

method showHand(deck = ArrayList,tag=REXX) public static binary
  say tag ArrayList(deck.subList(0,deck.size)).toString
  return
```

```txt
In  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
9 5 [1, 2, 3, 4, 5, 10, 7, 8, 9, 6]
8 4 [1, 2, 3, 4, 9, 10, 7, 8, 5]
7 2 [1, 2, 8, 4, 9, 10, 7, 3]
6 0 [7, 2, 8, 4, 9, 10, 1]
5 4 [7, 2, 8, 4, 10, 9]
4 1 [7, 10, 8, 4, 2]
3 2 [7, 10, 4, 8]
2 0 [4, 10, 7]
1 0 [10, 4]
Out [10, 4, 7, 8, 2, 9, 1, 3, 5, 6]
```



## Nim


```nim
import math
randomize()

proc shuffle[T](x: var seq[T]) =
  for i in countdown(x.high, 0):
    let j = random(i + 1)
    swap(x[i], x[j])

var x = @[0,1,2,3,4,5,6,7,8,9]
shuffle(x)
echo x
```


=={{header|Objective-C}}==

```objc
#import <Foundation/Foundation.h>


@interface NSMutableArray (KnuthShuffle)
- (void)knuthShuffle;
@end
@implementation NSMutableArray (KnuthShuffle)
- (void)knuthShuffle {
  for (NSUInteger i = self.count-1; i > 0; i--) {
    NSUInteger j = arc4random_uniform(i+1);
    [self exchangeObjectAtIndex:i withObjectAtIndex:j];
  }
}
@end

int main() {
  @autoreleasepool {
    NSMutableArray *x = [NSMutableArray arrayWithObjects:@0, @1, @2, @3, @4, @5, @6, @7, @8, @9, nil];
    [x knuthShuffle];
    NSLog(@"%@", x);
  }
  return 0;
}
```

```txt

(
    9,
    4,
    0,
    8,
    5,
    3,
    2,
    1,
    7,
    6
)

```



## OCaml


```ocaml
let shuffle arr =
  for n = Array.length arr - 1 downto 1 do
    let k = Random.int (n + 1) in
    let temp = arr.(n) in
    arr.(n) <- arr.(k);
    arr.(k) <- temp
  done
```



## Oforth


Works with any object that has the property to be Indexable (Lists, Intervals, ...)
Returns a new list


```Oforth
Indexable method: shuffle
| s i l |
   self asListBuffer ->l
   self size dup ->s 1- loop: i [ s i - rand i +  i  l swapValues ]
   l dup freeze ;
```



## Ol

There are two functions - one for tuples (that speedy) and second for lists (that uses previous one).

Ol is functional language, so we should make a copy of shuffling tuple and return this shuffled copy.


```scheme

(define (shuffle tp)
   (let ((items (vm:cast tp (type tp)))) ; make a copy
      (for-each (lambda (i)
            (let ((a (ref items i))
                  (j (+ 1 (rand! i))))
               (set-ref! items i (ref items j))
               (set-ref! items j a)))
         (reverse (iota (size items) 1)))
      items))

(define (list-shuffle tp)
   (map (lambda (i)
         (list-ref tp i))
      (tuple->list
         (shuffle (list->tuple (iota (length tp)))))))

```


Testing:

```scheme

(define items (tuple 1 2 3 4 5 6 7 8 9))
(print "tuple before: " items)
(print "tuple after: " (shuffle items))

(define items (list 1 2 3 4 5 6 7 8 9))
(print "list before: " items)
(print "list after: " (list-shuffle items))

```

Output:

```txt

tuple before: #[1 2 3 4 5 6 7 8 9]
tuple after: #[9 4 1 3 7 2 5 6 8]
list before: (1 2 3 4 5 6 7 8 9)
list after: (8 2 4 9 5 3 6 1 7)

```




## Oz


```oz
declare
  proc {Shuffle Arr}
     Low = {Array.low Arr}
     High = {Array.high Arr}
  in
     for I in High..Low;~1 do
	J = Low + {OS.rand} mod (I - Low + 1)
        OldI = Arr.I
     in
	Arr.I := Arr.J
        Arr.J := OldI
     end
  end

  X = {Tuple.toArray unit(0 1 2 3 4 5 6 7 8 9)}
in
  {Show {Array.toRecord unit X}}
  {Shuffle X}
  {Show {Array.toRecord unit X}}
```



## PARI/GP


```parigp
FY(v)={
  forstep(n=#v,2,-1,
    my(i=random(n)+1,t=v[i]);
    v[i]=v[n];
    v[n]=t
  );
  v
};

FY(vector(52,i,i))
```



## Pascal


```Pascal
program Knuth;

const
  startIdx = -5;
  max = 11;
type
  tmyData = string[9];
  tmylist = array [startIdx..startIdx+max-1] of tmyData;

procedure InitList(var a: tmylist);
var
  i: integer;
Begin
  for i := Low(a) to High(a) do
    str(i:3,a[i])
end;

procedure shuffleList(var a: tmylist);
var
  i,k : integer;
  tmp: tmyData;
begin
  for i := High(a)-low(a) downto 1 do begin
    k := random(i+1) + low(a);
    tmp := a[i+low(a)]; a[i+low(a)] := a[k]; a[k] := tmp
  end
end;

procedure DisplayList(const a: tmylist);
var
  i : integer;
Begin
  for i := Low(a) to High(a) do
    write(a[i]);
  writeln
end;

{ Test and display }
var
 a: tmylist;
 i: integer;
begin
  randomize;
  InitList(a);
  DisplayList(a);
  writeln;
  For i := 0 to 4 do
  Begin
    shuffleList(a);
    DisplayList(a);
  end;
end.
```

```txt
 -5 -4 -3 -2 -1  0  1  2  3  4  5

 -5  4  0 -4  3 -1 -3  1 -2  5  2
  2  0  1 -5 -1  5 -3  4 -2  3 -4
  3 -1 -2  5 -4  1  2 -5 -3  4  0
 -4  1 -1 -5  5  2  0  3 -2 -3  4
 -3 -5  4  2 -4  0  5  3  1 -1 -2
```



## Perl


```perl
sub shuffle {
  my @a = @_;
  foreach my $n (1 .. $#a) {
    my $k = int rand $n + 1;
    $k == $n or @a[$k, $n] = @a[$n, $k];
  }
  return @a;
}
```



## Perl 6

```perl6
sub shuffle (@a is copy) {
    for 1 ..^ @a -> $n {
        my $k = (0 .. $n).pick;
        $k == $n or @a[$k, $n] = @a[$n, $k];
    }
    return @a;
}
```

The shuffle is also built into the pick method on lists when you pass it a "whatever" for the number to pick:

```perl6
my @deck = @cards.pick(*);
```



## Phix


```Phix
sequence cards = tagset(52)
puts(1,"Before: ")      ?cards
for i=52 to 1 by -1 do
    integer r = rand(i)
    {cards[r],cards[i]} = {cards[i],cards[r]}
end for
puts(1,"After:  ")      ?cards
puts(1,"Sorted: ")      ?sort(cards)
```

<pre style="font-size: 12px">
Before: {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52}
After:  {42,4,48,28,11,3,52,51,22,2,49,38,25,33,27,35,18,44,5,7,21,13,36,29,43,6,9,31,10,30,20,16,46,34,8,17,14,45,37,24,32,41,50,15,39,40,47,23,1,12,26,19}
Sorted: {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52}

```



## PHP


```php
//The Fisher-Yates original Method
function yates_shuffle($arr){
	$shuffled = Array();
	while($arr){
		$rnd = array_rand($arr);
		$shuffled[] = $arr[$rnd];
		array_splice($arr, $rnd, 1);
	}
	return $shuffled;
}

//The modern Durstenfeld-Knuth algorithm
function knuth_shuffle(&$arr){
	for($i=count($arr)-1;$i>0;$i--){
		$rnd = mt_rand(0,$i);
		list($arr[$i], $arr[$rnd]) = array($arr[$rnd], $arr[$i]);
	}
}
```



## PicoLisp


```PicoLisp
(seed (in "/dev/urandom" (rd 8)))

(de knuth (Lst)
   (for (N (length Lst) (>= N 2) (dec N))
      (let I (rand 1 N)
         (xchg (nth Lst N) (nth Lst I)) ) ) )

(let L (range 1 15)
   (println 'before L)
   (knuth L)
   (println 'after L) )
```

```txt

before (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
after (12 15 4 13 11 9 7 1 2 14 5 6 8 3 10)

```



## PL/I


### version 1


```pli
declare T(0:10) fixed binary initial (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);
declare (i, j, temp) fixed binary;
do i = lbound(T,1) to hbound(T,1);
   j = min(random() * 12, 11);
   temp = T(j);   T(j) = T(i);   T(i) = temp;
end;
```



### version 2


```pli
 kn: Proc Options(main);
 /*--------------------------------------------------------------------
 * 07.01.2014 Walter Pachl  translated from REXX version 2
 * Iteration i: only the first i elements are candidates for swapping
 *-------------------------------------------------------------------*/
 Dcl T(10) Bin Fixed(15) Init(1,2,3,4,5,6,7,8,9,10);
 Dcl (i,j,temp) Bin Fixed(15) init(0);
 Dcl h Char(6);
 Call show('In',10);                   /* show start                 */
 do i = 10 To 2 By -1;                 /* shuffle                    */
   j=random()*i+1;
   Put string(h)Edit(i,j)(f(2),f(3));
   temp=t(i); t(i)=t(j); t(j)=temp;    /* t(i) <-> t(j)              */
   Call show(h,i);                     /* show intermediate states   */
   end;
 Call show('Out',10);                  /* show final state           */

 show: Proc(txt,n);
 Dcl txt Char(*);
 Dcl n   Bin Fixed(15);
 Put Edit(txt,(t(k) do k=1 To n))(Skip,a(7),10(f(3)));
 End;
 end;
```

```txt
In       1  2  3  4  5  6  7  8  9 10
10  5    1  2  3  4 10  6  7  8  9  5
 9  1    9  2  3  4 10  6  7  8  1
 8  7    9  2  3  4 10  6  8  7
 7  2    9  8  3  4 10  6  2
 6  6    9  8  3  4 10  6
 5  3    9  8 10  4  3
 4  2    9  4 10  8
 3  3    9  4 10
 2  1    4  9
Out      4  9 10  8  3  6  2  7  1  5
```



## PowerShell

```powershell
$A = 1, 2, 3, 4, 5
Get-Random $A -Count $A.Count
```

{{works with|PowerShell|2}} <!-- Get-Random didn't exist in PowerShell 1 -->

```powershell
function shuffle ($a) {
    $c = $a.Clone()  # make copy to avoid clobbering $a
    1..($c.Length - 1) | ForEach-Object {
        $i = Get-Random -Minimum $_ -Maximum $c.Length
        $c[$_-1],$c[$i] = $c[$i],$c[$_-1]
        $c[$_-1]  # return newly-shuffled value
    }
    $c[-1]  # last value
}
```

This yields the values one by one instead of returning the array as a whole, so the rest of the pipeline can work on the values while shuffling is still in progress.


## PureBasic


```PureBasic
EnableExplicit

Procedure KnuthShuffle(Array a(1))
   Protected i, last = ArraySize(a())

   For i = last To 1 Step -1
      Swap a(i), a(Random(i))
   Next
EndProcedure

Procedure.s ArrayToString(Array a(1))
   Protected ret$, i, last = ArraySize(a())

   ret$ = Str(a(0))
   For i = 1 To last
      ret$ + "," + Str(a(i))
   Next
   ProcedureReturn ret$
EndProcedure


#NumElements = 10

Dim a(#NumElements-1)
Define i

For i = 0 To #NumElements-1
   a(i) = i
Next

KnuthShuffle(a())
Debug "shuffled: " + ArrayToString(a())
```

```txt
shuffled: 1,8,6,0,5,9,2,4,7,3
```



## Python

Python's standard library function <code>[http://docs.python.org/library/random.html#random.shuffle random.shuffle]</code> uses this algorithm and so should normally be used.
The function below is very similar:

```python
from random import randrange

def knuth_shuffle(x):
    for i in range(len(x)-1, 0, -1):
        j = randrange(i + 1)
        x[i], x[j] = x[j], x[i]

x = list(range(10))
knuth_shuffle(x)
print("shuffled:", x)
```

```txt

shuffled: [5, 1, 6, 0, 8, 4, 2, 3, 9, 7]

```



We could also write our own Knuth shuffle function as a fold, with a non-mutating swap function:
```python
'''Knuth shuffle as a fold'''

from functools import reduce
from random import randint


# knuthShuffle :: [a] -> IO [a]
def knuthShuffle(xs):
    '''A pseudo-random shuffle of the elements in xs.'''
    return reduce(
        swapped,
        enumerate(randoms(len(xs))), xs
    )


# swapped :: (Int, Int) -> [a] -> [a]
def swapped(xs, ij):
    '''New list in which the elements at indices
       i and j of xs are swapped.
    '''
    def go(a, b):
        if a != b:
            m, n = (a, b) if b > a else (b, a)
            l, ht = splitAt(m)(xs)
            ys, zs = splitAt((n - m) - 1)(ht[1:])
            return l + [zs[0]] + ys + [ht[0]] + zs[1:]
        else:
            return xs
    i, j = ij
    z = len(xs) - 1
    return xs if i > z or j > z else go(i, j)


# randoms :: Int -> IO [Int]
def randoms(n):
    '''Pseudo-random list of n - 1 indices.
    '''
    return list(map(randomRInt(0)(n - 1), range(1, n)))


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Repeated Knuth shuffles of ['a' .. 'k']'''

    print(
        fTable(main.__doc__ + ':\n')(str)(lambda x: ''.join(x))(
            lambda _: knuthShuffle(list('abcdefghijk'))
        )(range(1, 11))
    )


# GENERIC -------------------------------------------------

# randomRInt :: Int -> Int -> IO () -> Int
def randomRInt(m):
    '''The return value of randomRInt is itself
       a function. The returned function, whenever
       called, yields a a new pseudo-random integer
       in the range [m..n].
    '''
    return lambda n: lambda _: randint(m, n)


# splitAt :: Int -> [a] -> ([a], [a])
def splitAt(n):
    '''A tuple pairing the prefix of length n
       with the rest of xs.
    '''
    return lambda xs: (xs[0:n], xs[n:])


# FORMATTING -----------------------------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Repeated Knuth shuffles of ['a' .. 'k']:

 1 -> kdafbhigejc
 2 -> jhdkgeicabf
 3 -> aciebghdfkj
 4 -> fjahegibckd
 5 -> cabejfidkgh
 6 -> gbecahfkijd
 7 -> jegchkdifba
 8 -> fcjkghiadeb
 9 -> ihfebdajgkc
10 -> hjkigbadcfe
```



## R

See also, the built-in function 'sample'.

Original Fisher-Yates version

```r
fisheryatesshuffle <- function(n)
{
  pool <- seq_len(n)
  a <- c()
  while(length(pool) > 0)
  {
     k <- sample.int(length(pool), 1)
     a <- c(a, pool[k])
     pool <- pool[-k]
  }
  a
}
```

Knuth variation:

```r
fisheryatesknuthshuffle <- function(n)
{
   a <- seq_len(n)
   while(n >=2)
   {
      k <- sample.int(n, 1)
      if(k != n)
      {
         temp <- a[k]
         a[k] <- a[n]
         a[n] <- temp
      }
      n <- n - 1
   }
   a
}

#Example usage:
fisheryatesshuffle(6)                # e.g. 1 3 6 2 4 5
x <- c("foo", "bar", "baz", "quux")
x[fisheryatesknuthshuffle(4)]        # e.g. "bar"  "baz"  "quux" "foo"
```



## Racket



```scheme
#lang racket

(define (swap! vec i j)
  (let ([tmp (vector-ref vec i)])
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j tmp)))

(define (knuth-shuffle x)
  (if (list? x)
    (vector->list (knuth-shuffle (list->vector x)))
    (begin (for ([i (in-range (sub1 (vector-length x)) 0 -1)])
             (define r (random (+ i 1)))
             (swap! x i r))
           x)))

(knuth-shuffle '(1 2 3 4))
```



## REBOL


```rebol
REBOL [
    Title: "Fisher-Yates"
    Purpose: {Fisher-Yates shuffling algorithm}
]

fisher-yates: func [b [block!] /local n i j k] [
    n: length? b: copy b
    i: n
    while [i > 1] [
        if i <> j: random i [
            error? set/any 'k pick b j
            change/only at b j pick b i
            change/only at b i get/any 'k
        ]
        i: i - 1
    ]
    b
]
```



## REXX

===version 0, card pips===

```rexx
/*REXX program shuffles a deck of playing cards (with jokers)  using the  Knuth shuffle.*/
rank= 'A 2 3 4 5 6 7 8 9 10 J Q K'               /*pips  of the various playing cards.  */
suit= '♣♠♦♥'                                     /*suit   "  "     "       "      "     */
parse arg seed .                                 /*obtain optional argument from the CL.*/
if datatype(seed,'W')  then call random ,,seed   /*maybe use for  RANDOM  repeatability.*/
say '══════════════════ getting a new deck out of the box ···'
@.1= 'highJoker'                                 /*good decks have a color joker, and a */
@.2= 'lowJoker'                                  /*            ··· black & white joker. */
cards=2                                          /*now, there're 2 cards are in the deck*/
               do j     =1  for length(suit)
                    do k=1  for  words(rank);      cards=cards + 1
                    @.cards=substr(suit, j, 1)word(rank, k)
                    end  /*k*/
               end       /*j*/
call show
say;      say '══════════════════ shuffling' cards "cards ···"
     do s=cards  by -1  to 2;  ?=random(1,s);  parse value  @.?  @.s   with   @.s  @.?
                                                 /*  [↑]  swap two cards in the deck.   */
     end   /*s*/
call show
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: _=;      do m=1  for cards;   _=_ @.m;   end  /*m*/;         say _;           return
```

'''output'''

```txt

══════════════════ getting a new deck out of the box ···
 highJoker lowJoker ♣A ♣2 ♣3 ♣4 ♣5 ♣6 ♣7 ♣8 ♣9 ♣10 ♣J ♣Q ♣K ♠A ♠2 ♠3 ♠4 ♠5 ♠6 ♠7 ♠8 ♠9 ♠10 ♠J ♠Q ♠K ♦A ♦2 ♦3 ♦4 ♦5 ♦6 ♦7 ♦8 ♦9 ♦10 ♦J ♦Q ♦K ♥A ♥2 ♥3 ♥4 ♥5 ♥6 ♥7 ♥8 ♥9 ♥10 ♥J ♥Q ♥K

══════════════════ shuffling 54 cards ···
 ♣J ♦3 ♥5 ♣10 ♥2 ♥J ♣6 ♦4 ♠2 ♥8 ♥A ♠A ♣9 ♣5 ♠7 ♦6 ♥6 ♠10 ♥9 ♦2 lowJoker ♥3 ♠5 ♠K ♣K ♣8 ♣Q ♠Q ♣2 ♦8 ♠4 ♣7 ♦5 ♥K ♣A ♠6 ♠J ♦Q ♦7 ♠9 ♦10 ♦K ♣4 ♥7 ♣3 ♠3 highJoker ♦A ♥4 ♦J ♠8 ♦9 ♥Q ♥10

```


===version 1, card names===
This version handles items with (leading/trailing/embedded) blanks in them, so   '''parse'''   isn't an option for shuffling.

```rexx
/*REXX program shuffles a deck of playing cards (with jokers)  using the  Knuth shuffle.*/
rank = 'ace deuce trey 4 5 6 7 8 9 10 jack queen king'         /*use pip names for cards*/
suit = 'club spade diamond heart'                              /* "  suit  "    "    "  */
say '══════════════════ getting a new deck out of the box ···'
@.1= '  color joker'                             /*good decks have a color joker, and a */
@.2= '    b&w joker'                             /*            ··· black & white joker. */
cards=2                                          /*now, there're 2 cards are in the deck*/
          do j     =1  for words(suit)
               do k=1  for words(rank);       cards=cards+1    /*bump the card counter. */
               @.cards=right(word(suit,j),7)  word(rank,k)     /*assign a card name.    */
               end  /*k*/
          end       /*j*/

call show 'ace'                                  /*inserts blank when an  ACE  is found.*/
say;  say '══════════════════ shuffling' cards "cards ···"

          do s=cards  by -1  to 2;   ?=random(1,s);   _=@.?;   @.?=@.s;    @.s=_
          end   /*s*/                            /* [↑]  swap two cards in the deck.    */
call show
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: parse arg break;    say                    /*get separator card, show blank line. */
        do m=1  for cards                        /* [↓]  traipse through the card deck. */
        if pos(break,@.m)\==0  then say          /*show a blank to read cards easier.   */
        say 'card'  right(m, 2)    '───►'   @.m  /*display a particular card from deck. */
        end   /*m*/
return
```

'''output'''
<pre style="height:50ex">
══════════════════ getting a new deck out of the box ···

card  1 ───►   color joker
card  2 ───►     b&w joker

card  3 ───►    club ace
card  4 ───►    club deuce
card  5 ───►    club trey
card  6 ───►    club 4
card  7 ───►    club 5
card  8 ───►    club 6
card  9 ───►    club 7
card 10 ───►    club 8
card 11 ───►    club 9
card 12 ───►    club 10
card 13 ───►    club jack
card 14 ───►    club queen
card 15 ───►    club king

card 16 ───►   spade ace
card 17 ───►   spade deuce
card 18 ───►   spade trey
card 19 ───►   spade 4
card 20 ───►   spade 5
card 21 ───►   spade 6
card 22 ───►   spade 7
card 23 ───►   spade 8
card 24 ───►   spade 9
card 25 ───►   spade 10
card 26 ───►   spade jack
card 27 ───►   spade queen
card 28 ───►   spade king

card 29 ───► diamond ace
card 30 ───► diamond deuce
card 31 ───► diamond trey
card 32 ───► diamond 4
card 33 ───► diamond 5
card 34 ───► diamond 6
card 35 ───► diamond 7
card 36 ───► diamond 8
card 37 ───► diamond 9
card 38 ───► diamond 10
card 39 ───► diamond jack
card 40 ───► diamond queen
card 41 ───► diamond king

card 42 ───►   heart ace
card 43 ───►   heart deuce
card 44 ───►   heart trey
card 45 ───►   heart 4
card 46 ───►   heart 5
card 47 ───►   heart 6
card 48 ───►   heart 7
card 49 ───►   heart 8
card 50 ───►   heart 9
card 51 ───►   heart 10
card 52 ───►   heart jack
card 53 ───►   heart queen
card 54 ───►   heart king

══════════════════ shuffling 54 cards ···

card  1 ───►   spade ace
card  2 ───►   heart jack
card  3 ───►   heart ace
card  4 ───► diamond 10
card  5 ───►   spade 7
card  6 ───►    club 10
card  7 ───►    club trey
card  8 ───► diamond deuce
card  9 ───► diamond 7
card 10 ───►   spade queen
card 11 ───►   heart queen
card 12 ───►   spade deuce
card 13 ───►   spade 9
card 14 ───► diamond 4
card 15 ───► diamond ace
card 16 ───►   heart 6
card 17 ───►    club king
card 18 ───►   color joker
card 19 ───►   spade 6
card 20 ───►   heart 5
card 21 ───► diamond 8
card 22 ───►   heart 8
card 23 ───►    club 7
card 24 ───►   heart king
card 25 ───►    club jack
card 26 ───► diamond jack
card 27 ───►   heart 9
card 28 ───►   spade trey
card 29 ───►   spade jack
card 30 ───►   spade king
card 31 ───►   heart 10
card 32 ───► diamond king
card 33 ───► diamond trey
card 34 ───►   heart deuce
card 35 ───►   heart 4
card 36 ───► diamond 5
card 37 ───► diamond 9
card 38 ───►   spade 4
card 39 ───►    club 4
card 40 ───►    club 5
card 41 ───►   spade 5
card 42 ───►    club 9
card 43 ───►     b&w joker
card 44 ───►    club 6
card 45 ───►   heart 7
card 46 ───►   spade 8
card 47 ───► diamond 6
card 48 ───►    club deuce
card 49 ───► diamond queen
card 50 ───►    club queen
card 51 ───►    club ace
card 52 ───►   heart trey
card 53 ───►   spade 10
card 54 ───►    club 8

```



### version 2


```rexx
/* REXX ---------------------------------------------------------------
* 05.01.2014 Walter Pachl
*            borrow one improvement from version 1
* 06.01.2014 removed    -"-  (many tests cost more than few "swaps")
*--------------------------------------------------------------------*/
Call random ,,123456                   /* seed for random            */
Do i=1 To 10; a.i=i; End;              /* fill array                 */
Call show 'In',10                      /* show start                 */
do i = 10 To 2 By -1                   /* shuffle                    */
  j=random(i-1)+1;
  h=right(i,2) right(j,2)
  Parse Value a.i a.j With a.j a.i     /* a.i <-> a.j                */
  Call show h,i                        /* show intermediate states   */
  end;
Call show 'Out',10                     /* show fomaö state           */
Exit

show: Procedure Expose a.
Parse Arg txt,n
ol=left(txt,6);
Do k=1 To n; ol=ol right(a.k,2); End
Say ol
Return
```

```txt
In      1  2  3  4  5  6  7  8  9 10
10  2   1 10  3  4  5  6  7  8  9  2
 9  6   1 10  3  4  5  9  7  8  6
 8  6   1 10  3  4  5  8  7  9
 7  3   1 10  7  4  5  8  3
 6  5   1 10  7  4  8  5
 5  1   8 10  7  4  1
 4  1   4 10  7  8
 3  1   7 10  4
 2  1  10  7
Out    10  7  4  8  1  5  3  9  6  2
```



## Ring


```ring

# Project : Knuth shuffle

items = list(52)
for n = 1 to len(items)
      items[n] = n
next
knuth(items)
showarray(items)

func knuth(items)
       for i = len(items) to 1 step -1
            j = random(i-1) + 1
            if i != j
               temp = items[i]
               items[i] = items[j]
               items[j] = temp
            ok
       next

func showarray(vect)
       see "["
       svect = ""
       for n = 1 to len(vect)
           svect = svect + vect[n] + " "
       next
       svect = left(svect, len(svect) - 1)
       see svect
       see "]" + nl

```


```txt

[15 1 51 20 45 29 43 8 13 3 41 35 11 7 37 9 38 17 32 48 40 25 44 18 14 50 42 34 2 21 12 4 26 19 23 24 28 46 36 10 5 16 6 49 22 33 39 47 31 52 30 27]

```



## Ruby

```ruby
class Array
  def knuth_shuffle!
    j = length
    i = 0
    while j > 1
      r = i + rand(j)
      self[i], self[r] = self[r], self[i]
      i += 1
      j -= 1
    end
    self
  end
end

r = Hash.new(0)
100_000.times do |i|
  a = [1,2,3].knuth_shuffle!
  r[a] += 1
end

r.keys.sort.each {|a| puts "#{a.inspect} => #{r[a]}"}
```

results in

```txt
[1, 2, 3] => 16572
[1, 3, 2] => 16610
[2, 1, 3] => 16633
[2, 3, 1] => 16714
[3, 1, 2] => 16838
[3, 2, 1] => 16633
```

'''More idomatic:'''

```ruby
class Array
  def knuth_shuffle!
    (length - 1).downto(1) do |i|
      j = rand(i + 1)
      self[i], self[j] = self[j], self[i]
    end
    self
  end
end
```




## Run BASIC


```runbasic
dim cards(52)
for i = 1 to 52                ' make deck
  cards(i) = i
next

for i = 52 to 1 step -1        ' shuffle deck
   r = int((rnd(1)*i) + 1)
   if r <> i then
     hold     = cards(r)
     cards(r) = cards(i)
     cards(i) = hold
   end if
next

print "== Shuffled Cards =="  ' print shuffled cards
for i = 1 to 52
    print cards(i);" ";
    if i mod 18 = 0 then print
next
print
```



## Rust

```rust
use rand::Rng;

extern crate rand;

fn knuth_shuffle<T>(v: &mut [T]) {
    let mut rng = rand::thread_rng();
    let l = v.len();

    for n in 0..l {
        let i = rng.gen_range(0, l - n);
        v.swap(i, l - n - 1);
    }
}

fn main() {
    let mut v: Vec<_> = (0..10).collect();

    println!("before: {:?}", v);
    knuth_shuffle(&mut v);
    println!("after:  {:?}", v);
}
```



## Scala


```Scala
def shuffle[T](a: Array[T]) = {
  for (i <- 1 until a.size reverse) {
    val j = util.Random nextInt (i + 1)
    val t = a(i)
    a(i) = a(j)
    a(j) = t
  }
  a
}
```



## Scheme

A functional version, using lists (inefficient), somewhat unusual in reversing the entire initial sublist on each pass instead of just swapping:

```Scheme
#!r6rs
(import (rnrs base (6))
        (srfi :27 random-bits))

(define (semireverse li n)
  (define (continue front back n)
    (cond
      ((null? back) front)
      ((zero? n) (cons (car back) (append front (cdr back))))
      (else (continue (cons (car back) front) (cdr back) (- n 1)))))
  (continue '() li n))

(define (shuffle li)
  (if (null? li)
      ()
      (let
          ((li-prime (semireverse li (random-integer (length li)))))
        (cons (car li-prime) (shuffle (cdr li-prime))))))
```


A mutable version, using vectors (efficient):

```Scheme
#!r6rs
(import (rnrs base (6))
        (srfi :27 random-bits))

(define (vector-swap! vec i j)
  (let
      ((temp (vector-ref vec i)))
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j temp)))

(define (countdown n)
  (if (zero? n)
      ()
      (cons n (countdown (- n 1)))))

(define (vector-shuffle! vec)
  (for-each
   (lambda (i)
     (let
         ((j (random-integer i)))
       (vector-swap! vec (- i 1) j)))
   (countdown (vector-length vec))))
```



## Scratch

See Knuth's shuffle in action.  Visit [https://scratch.mit.edu/projects/65352234/ this Scratch implementation] to see a demo and inspect its source.


## Seed7


```seed7
$ include "seed7_05.s7i";

const type: intArray is array integer;

const proc: shuffle (inout intArray: a) is func
  local
    var integer: i is 0;
    var integer: k is 0;
    var integer: tmp is 0;
  begin
    for i range maxIdx(a) downto 2 do
      k := rand(1, i);
      tmp := a[i];
      a[i] := a[k];
      a[k] := tmp;
    end for;
  end func;

const proc: main is func
  local
    var intArray: a is 10 times 0;
    var integer: i is 0;
  begin
    for key i range a do
      a[i] := i;
    end for;
    shuffle(a);
    for i range a do
      write(i <& " ");
    end for;
    writeln;
  end func;
```


```txt

7 5 6 8 3 10 9 4 2 1

```



## Sidef


```ruby
func knuth_shuffle(a) {
    for i (a.len ^.. 1) {
        var j = i.irand
        a[i, j] = a[j, i]
    }
    return a
}

say knuth_shuffle(@(1..10))
```

```txt

[7, 4, 3, 8, 9, 6, 10, 2, 1, 5]

```



## Smalltalk

```smalltalk
"The selector swap:with: is documented, but it seems not
 implemented (GNU Smalltalk version 3.0.4); so here it is an implementation"
SequenceableCollection extend [
  swap: i with: j [
    |t|
    t := self at: i.
    self at: i put: (self at: j).
    self at: j put: t.
  ]
].

Object subclass: Shuffler [
  Shuffler class >> Knuth: aSequenceableCollection [
    |n k|
    n := aSequenceableCollection size.
    [ n > 1 ] whileTrue: [
      k := Random between: 1 and: n.
      aSequenceableCollection swap: n with: k.
      n := n - 1
    ]
  ]
].
```

Testing

```smalltalk
"Test"
|c|
c := OrderedCollection new.
c addAll: #( 1 2 3 4 5 6 7 8 9 ).
Shuffler Knuth: c.
c display.
```



## SNOBOL4


```SNOBOL4
* Library for random()
-include 'Random.sno'

*       # String -> array
        define('s2a(str,n)i') :(s2a_end)
s2a     s2a = array(n); str = str ' '
sa1     str break(' ') . s2a<i = i + 1> span(' ') = :s(sa1)f(return)
s2a_end

*       # Array -> string
        define('a2s(a)i') :(a2s_end)
a2s     a2s = a2s a<i = i + 1> ' ' :s(a2s)f(return)
a2s_end

*       # Knuth shuffle in-place
        define('shuffle(a)alen,n,k,tmp') :(shuffle_end)
shuffle n = alen = prototype(a);
sh1     k = convert(random() * alen,'integer') + 1
        eq(a<n>,a<k>) :s(sh2)
        tmp = a<n>; a<n> = a<k>; a<k> = tmp
sh2     n = gt(n,1) n - 1 :s(sh1)
        shuffle = a :(return)
shuffle_end

*       # Test and display
        a = s2a('1 2 3 4 5 6 7 8 9 10',10)
        output = a2s(a) '->'
        shuffle(a)
        output = a2s(a)
end
```

```txt
1 2 3 4 5 6 7 8 9 10 ->
2 10 4 9 1 5 6 8 7 3
```



## Stata



```stata
mata
function shuffle(a) {
	n = length(a)
	r = runiformint(1,1,1,1..n)
	for (i=n; i>=2; i--) {
		j = r[i]
		x = a[i]
		a[i] = a[j]
		a[j] = x
	}
	return(a)
}

shuffle(1..10)
end
```


'''Output'''


```txt
        1    2    3    4    5    6    7    8    9   10
    +---------------------------------------------------+
  1 |   8   10    9    1    7    2    6    4    3    5  |
    +---------------------------------------------------+
```



## Swift


'''Simple version (any Swift version):''' Extend Array with shuffle methods; using arc4random_uniform from C stdlib:


```swift
import func Darwin.arc4random_uniform

extension Array {

    func shuffle() -> Array {

        var result = self; result.shuffleInPlace(); return result
    }

    mutating func shuffleInPlace() {

        for i in 1 ..< count { swap(&self[i], &self[Int(arc4random_uniform(UInt32(i+1)))]) }
    }

}

// Swift 2.0:
print([1, 2, 3, 4, 5, 6, 7, 8, 9, 10].shuffle())
// Swift 1.x:
//println([1, 2, 3, 4, 5, 6, 7, 8, 9, 10].shuffle())
```


```txt
[8, 7, 2, 1, 6, 10, 5, 3, 4, 9]
```


'''Generic version (any Swift version):''' While the above code is generic in that it works with arrays of any element type, we can use generic global functions to define shuffling for any mutable collection with random-access index type which is far more generic than the above code:


```swift
import func Darwin.arc4random_uniform

func shuffleInPlace<T: MutableCollectionType where T.Index: RandomAccessIndexType>(inout collection: T) {

    let i0 = collection.startIndex

    for i in i0.successor() ..< collection.endIndex {

        let j = i0.advancedBy(numericCast(
                    arc4random_uniform(numericCast(
                        i0.distanceTo()
                    )+1)
                ))

        swap(&collection[i], &collection[j])
    }
}

func shuffle<T: MutableCollectionType where T.Index: RandomAccessIndexType>(collection: T) -> T {

    var result = collection

    shuffleInPlace(&result)

    return result
}

// Swift 2.0:
print(shuffle([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
// Swift 1.x:
//println(shuffle([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
```


```txt
[2, 5, 7, 1, 6, 10, 4, 3, 8, 9]
```


{{works with|Swift | 2.0 }} While the above solutions work with Swift 2.0 as they are, we can use Swift 2.0's Protocol Oriented Programming features to add shuffling methods to any mutable collection that has a random-access index:


```swift
import func Darwin.arc4random_uniform

// Define a protocol for shuffling:

protocol Shufflable {

    @warn_unused_result (mutable_variant="shuffleInPlace")
    func shuffle() -> Self

    mutating func shuffleInPlace()

}

// Provide a generalized implementation of the shuffling protocol for any mutable collection with random-access index:

extension Shufflable where Self: MutableCollectionType, Self.Index: RandomAccessIndexType {

    func shuffle() -> Self {

        var result = self

        result.shuffleInPlace()

        return result
    }

    mutating func shuffleInPlace() {

        let i0 = startIndex

        for i in i0+1 ..< endIndex {

            let j = i0.advancedBy(numericCast(
                        arc4random_uniform(numericCast(
                            i0.distanceTo(i)
                        )+1)
                    ))

            swap(&self[i], &self[j])
        }
    }

}

// Declare Array's conformance to Shufflable:

extension Array: Shufflable
    { /* Implementation provided by Shufflable protocol extension */ }

print([1, 2, 3, 4, 5, 6, 7, 8, 9, 10].shuffle())
```


```txt
[3, 1, 5, 6, 7, 8, 10, 2, 4, 9]
```



## Tcl


```tcl
proc knuth_shuffle lst {
   set j [llength $lst]
   for {set i 0} {$j > 1} {incr i;incr j -1} {
       set r [expr {$i+int(rand()*$j)}]
       set t [lindex $lst $i]
       lset lst $i [lindex $lst $r]
       lset lst $r $t
   }
   return $lst
}

% knuth_shuffle {1 2 3 4 5}
2 1 3 5 4
% knuth_shuffle {1 2 3 4 5}
5 2 1 4 3
% knuth_shuffle {tom dick harry peter paul mary}
tom paul mary harry peter dick
```

As a test of skewing (an indicator of a poor implementation) this code was used:

```tcl
% for {set i 0} {$i<100000} {incr i} {
    foreach val [knuth_shuffle {1 2 3 4 5}] pos {pos0 pos1 pos2 pos3 pos4} {
        incr tots($pos) $val
    }
}
% parray tots
tots(pos0) = 300006
tots(pos1) = 300223
tots(pos2) = 299701
tots(pos3) = 299830
tots(pos4) = 300240
```


=={{header|TI-83 BASIC}}==
Input L<sub>1</sub>, output L<sub>2</sub>.
 :"SHUFFLE"
 :L<sub>1</sub>→L<sub>2</sub>
 :dim(L<sub>2</sub>)→A
 :For(B,1,dim(L<sub>2</sub>)-1)
 :randInt(1,A)→C
 :L<sub>2</sub>(C)→D
 :L<sub>2</sub>(A)→L<sub>2</sub>(C)
 :D→L<sub>2</sub>(A)
 :A-1→A
 :End
 :DelVar A
 :DelVar B
 :DelVar C
 :DelVar D
 :Return


## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
oldnumbers=newnumbers="",range=20
LOOP nr=1,#range
 oldnumbers=APPEND(oldnumbers,nr)
ENDLOOP

PRINT "before ",oldnumbers

LOOP r=#range,1,-1
 RANDNR=RANDOM_NUMBERS (1,#r,1)
 shuffle=SELECT (oldnumbers,#randnr,oldnumbers)
 newnumbers=APPEND(newnumbers,shuffle)
ENDLOOP

PRINT "after  ",newnumbers
```

```txt

before 1'2'3'4'5'6'7'8'9'10'11'12'13'14'15'16'17'18'19'20
after  7'16'13'11'1'9'15'4'18'14'3'12'17'8'19'20'6'5'2'10

```



## uBasic/4tH

<lang>PRINT "before:"
FOR L = 0 TO 51
    @(L) = L
    PRINT @(L); " ";
NEXT

FOR L = 51 TO 0 STEP -1
    C = RND(L + 1)
    IF C # L THEN
      PUSH @(C), L, @(L), C
      GOSUB 100
    ENDIF
NEXT

PRINT : PRINT "after:"
FOR L = 0 TO 51
    PRINT @(L); " ";
NEXT
PRINT
END

100 @(POP()) = POP() : @(POP()) = POP() : RETURN
```

```txt
before:
0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51
after:
19 4 49 9 27 35 50 11 2 29 22 48 33 15 17 42 47 28 41 18 34 21 30 39 3 8 23 12 36 26 0 46 7 44 13 14 16 40 10 25 31 32 51 24 20 38 45 6 43 1 5 37
```


## UNIX Shell

```bash
# Shuffle array[@].
function shuffle {
	integer i j t

	((i = ${#array[@]}))
	while ((i > 1)); do
		((j = RANDOM))                 # 0 <= j < 32768
		((j < 32768 % i)) && continue  # no modulo bias
		((j %= i))                     # 0 <= j < i

		((i -= 1))
		((t = array[i]))
		((array[i] = array[j]))
		((array[j] = t))
	done
}

# Test program.
set -A array 11 22 33 44 55 66 77 88 99 110
shuffle
echo "${array[@]}"
```



## Ursala

This function works on lists of any type and length, including character strings.

```Ursala
shuffle = @iNX ~&l->r ^jrX/~&l ~&lK8PrC
```

test program:

```Ursala
#cast %s

example = shuffle 'abcdefghijkl'
```

```txt
'keacfjlbdigh'
```



## VBA


```vb
Private Sub Knuth(Optional ByRef a As Variant)
    Dim t As Variant, i As Integer
    If Not IsMissing(a) Then
        For i = UBound(a) To LBound(a) + 1 Step -1
            j = Int((UBound(a) - LBound(a) + 1) * Rnd + LBound(a))
            t = a(i)
            a(i) = a(j)
            a(j) = t
        Next i
    End If
End Sub
Public Sub program()
    Dim b As Variant, c As Variant, d As Variant, e As Variant
    Randomize
    'imagine an empty array on this line
    b = [{10}]
    c = [{10, 20}]
    d = [{10, 20, 30}]
    e = [{11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22}]
    f = [{"This ", "is ", "a ", "test"}]
    Debug.Print "Before:"
    Knuth 'feeding an empty array ;)
    Debug.Print "After: "
    Debug.Print "Before:";
    For Each i In b: Debug.Print i;: Next i: Debug.Print
    Knuth b
    Debug.Print "After: ";
    For Each i In b: Debug.Print i;: Next i: Debug.Print
    Debug.Print "Before:";
    For Each i In c: Debug.Print i;: Next i: Debug.Print
    Knuth c
    Debug.Print "After: ";
    For Each i In c: Debug.Print i;: Next i: Debug.Print
    Debug.Print "Before:";
    For Each i In d: Debug.Print i;: Next i: Debug.Print
    Knuth d
    Debug.Print "After: ";
    For Each i In d: Debug.Print i;: Next i: Debug.Print
    Debug.Print "Before:";
    For Each i In e: Debug.Print i;: Next i: Debug.Print
    Knuth e
    Debug.Print "After: ";
    For Each i In e: Debug.Print i;: Next i: Debug.Print
    Debug.Print "Before:";
    For Each i In f: Debug.Print i;: Next i: Debug.Print
    Knuth f
    Debug.Print "After: ";
    For Each i In f: Debug.Print i;: Next i: Debug.Print
End Sub
```
```txt
Before:
After:
Before: 10
After:  10
Before: 10  20
After:  10  20
Before: 10  20  30
After:  20  10  30
Before: 11  12  13  14  15  16  17  18  19  20  21  22
After:  22  12  15  20  19  11  13  21  16  17  14  18
Before:This is a test
After: a This testis

```


## VBScript

;Implementation

```vb

function shuffle( a )
	dim i
	dim r
	randomize timer
	for i = lbound( a ) to ubound( a )
		r = int( rnd * ( ubound( a ) + 1 )  )
		if r <> i then
			swap a(i), a(r)
		end if
	next
	shuffle = a
end function

sub swap( byref a, byref b )
	dim tmp
	tmp = a
	a = b
	b = tmp
end sub
```

;Invocation

```vb
dim a
a = array( 1,2,3,4,5,6,7,8,9)
wscript.echo "before: ", join( a, ", " )
shuffle a
wscript.echo "after: ", join( a, ", " )
shuffle a
wscript.echo "after: ", join( a, ", " )
wscript.echo "--"
a = array( now(), "cow", 123, true, sin(1), 16.4 )
wscript.echo "before: ", join( a, ", " )
shuffle a
wscript.echo "after: ", join( a, ", " )
shuffle a
wscript.echo "after: ", join( a, ", " )
```

```txt

before:  1, 2, 3, 4, 5, 6, 7, 8, 9
after:  6, 4, 1, 2, 7, 3, 5, 8, 9
after:  8, 7, 3, 2, 6, 5, 9, 1, 4
--
before:  16/02/2010 5:46:58 PM, cow, 123, True, 0.841470984807897, 16.4
after:  True, 16.4, 16/02/2010 5:46:58 PM, 123, cow, 0.841470984807897
after:  16.4, 16/02/2010 5:46:58 PM, 123, 0.841470984807897, True, cow

```



## Vedit macro language

The shuffle routine in [[Playing Cards]] shuffles text lines in edit buffer.
This example shuffles numeric registers #0 to #19.

The output will be inserted in current edit buffer.

```vedit
// Test main
#90 = Time_Tick                   // seed for random number generator
#99 = 20                          // number of items in the array

IT("Before:") IN
for (#100 = 0; #100 < #99; #100++) {
    #@100 = #100
    Num_Ins(#@100, LEFT+NOCR) IT(" ")
}
IN

Call("SHUFFLE_NUMBERS")

IT("After:") IN
for (#100 = 0; #100 < #99; #100++) {
    Num_Ins(#@100, LEFT+NOCR) IT(" ")
}
IN
Return

//--------------------------------------------------------------
// Shuffle numeric registers #0 to #nn
//  #99 = number of registers to shuffle (nn-1)
//
:SHUFFLE_NUMBERS:
for (#91 = #99-1; #91 > 0; #91--) {
    Call("RANDOM")
    #101 = Return_Value
    #102 = #@101; #@101 = #@91; #@91 = #102
}
Return

//--------------------------------------------------------------
// Generate random numbers in range 0 <= Return_Value < #91
//  #90 = Seed    (0 to 0x7fffffff)
//  #91 = Scaling (0 to 0x10000)
//
:RANDOM:
#92 = 0x7fffffff / 48271
#93 = 0x7fffffff % 48271
#90 = (48271 * (#90 % #92) - #93 * (#90 / #92)) & 0x7fffffff
Return ((#90 & 0xffff) * #91 / 0x10000)
```

```txt
Before:
0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
After:
9 13 8 18 10 1 17 15 0 16 14 19 3 2 7 11 6 4 5 12
```



## zkl

Two versions, imperative and functional, same results.
xs has to be a mutable list.

```zkl
fcn kshuffle(xs){
   foreach i in ([xs.len()-1..1,-1]){ xs.swap(i,(0).random(0,i+1)) }
   xs
}
fcn kshufflep(xs){
   [xs.len()-1..1,-1].pump(Void,'wrap(i){ xs.swap(i,(0).random(0,i+1)) })
   xs
}
```


```txt

var ns=(1).pump(10,List).copy() // [1..10] made mutable
kshuffle(ns)  //-->L(6,3,8,2,4,5,10,9,1,7)

ns="this is a test foo bar hoho".split(" ").copy();
kshufflep(ns)  //-->L("a","bar","hoho","foo","test","is","this")

```

