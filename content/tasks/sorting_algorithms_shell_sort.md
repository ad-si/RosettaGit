+++
title = "Sorting algorithms/Shell sort"
description = ""
date = 2019-04-09T19:16:41Z
aliases = []
[extra]
id = 2861
[taxonomies]
categories = ["task", "Sorting Algorithms"]
tags = []
languages = [
  "360_assembly",
  "actionscript",
  "ada",
  "algol_68",
  "arm_assembly",
  "autohotkey",
  "awk",
  "bbc_basic",
  "bcpl",
  "c",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "e",
  "eiffel",
  "elixir",
  "euphoria",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "io",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "liberty_basic",
  "lisaac",
  "lua",
  "m2000_interpreter",
  "maple",
  "mathematica",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oorexx",
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
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "scala",
  "seed7",
  "sidef",
  "swift",
  "tcl",
  "ubasic_4th",
  "whitespace",
  "xpl0",
  "yabasic",
  "zkl",
]
+++

## Task

Sort an array of elements using the [[wp:Shell sort|Shell sort]] algorithm, a diminishing increment sort.

The Shell sort   (also known as Shellsort or Shell's method)   is named after its inventor, Donald Shell, who published the algorithm in 1959.

Shell sort is a sequence of interleaved insertion sorts based on an increment sequence.
The increment size is reduced after each pass until the increment size is 1.

With an increment size of 1, the sort is a basic insertion sort, but by this time the data is guaranteed to be almost sorted, which is insertion sort's "best case".

Any sequence will sort the data as long as it ends in 1, but some work better than others.

Empirical studies have shown a geometric increment sequence with a ratio of about 2.2 work well in practice.
[http://www.cs.princeton.edu/~rs/shell/]

Other good sequences are found at the [https://oeis.org/search?q=shell+sort On-Line Encyclopedia of Integer Sequences].





## 360 Assembly

The program uses ASM structured macros and two ASSIST macros to keep the code as short as possible.

```360asm
*        Shell sort                24/06/2016
SHELLSRT CSECT
         USING  SHELLSRT,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         L      RK,N               incr=n
         SRA    RK,1               incr=n/2
         DO WHILE=(LTR,RK,P,RK)    do while(incr>0)
         LA     RI,1(RK)             i=1+incr
         DO WHILE=(C,RI,LE,N)        do i=1+incr to n
         LR     RJ,RI                  j=i
         LR     R1,RI                  i
         SLA    R1,2                   .
         L      RT,A-4(R1)             temp=a(i)
         LR     R2,RK                  incr
         LA     R2,1(R2)               r2=incr+1
         LR     R3,RJ                  j
         SR     R3,RK                  j-incr
         SLA    R3,2                   *.
         LA     R3,A-4(R3)             r3=@a(j-incr)
         LR     R4,RK                  incr
         SLA    R4,2                   r4=incr*4
         LR     R5,RJ                  j
         SLA    R5,2                   .
         LA     R5,A-4(R5)             @a(j)
*        do while j-incr>=1 and a(j-incr)>temp
         DO WHILE=(CR,RJ,GE,R2,AND,C,RT,LT,0(R3))
         L      R0,0(R3)                 a(j-incr)
         ST     R0,0(R5)                 a(j)=a(j-incr)
         SR     RJ,RK                    j=j-incr
         LR     R5,R3                    @a(j)
         SR     R3,R4                    @a(j-incr)=@a(j-incr)-incr*4
         ENDDO  ,                      end do
         ST     RT,0(R5)               a(j)=temp
         LA     RI,1(RI)               i=i+1
         ENDDO  ,                    end do
         IF     C,RK,EQ,=F'2'        if incr=2
         LA     RK,1                   incr=1
         ELSE   ,                    else
         LR     R5,RK                  incr
         M      R4,=F'5'               *5
         D      R4,=F'11'              /11
         LR     RK,R5                  incr=incr*5/11
         ENDIF  ,                    end if
         ENDDO  ,                  end do
         LA     R3,PG              pgi=0
         LA     RI,1               i=1
         DO     WHILE=(C,RI,LE,N)  do i=1 to n
         LR     R1,RI                i
         SLA    R1,2                 .
         L      R2,A-4(R1)           a(i)
         XDECO  R2,XDEC              edit a(i)
         MVC    0(4,R3),XDEC+8       output a(i)
         LA     R3,4(R3)             pgi=pgi+4
         LA     RI,1(RI)             i=i+1
         ENDDO  ,                  end do
         XPRNT  PG,L'PG            print buffer
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
A     DC F'4',F'65',F'2',F'-31',F'0',F'99',F'2',F'83',F'782',F'1'
      DC F'45',F'82',F'69',F'82',F'104',F'58',F'88',F'112',F'89',F'74'
N        DC     A((N-A)/L'A)       number of items of a
PG       DC     CL80' '            buffer
XDEC     DS     CL12               temp for xdeco
         YREGS
RI       EQU    6                  i
RJ       EQU    7                  j
RK       EQU    8                  incr
RT       EQU    9                  temp
         END    SHELLSRT
```

```txt

 -31   0   1   2   2   4  45  58  65  69  74  82  82  83  88  89  99 104 112 782

```



## ActionScript


```ActionScript
function shellSort(data:Array):Array
{
	var inc:uint = data.length/2;
	while(inc > 0)
	{
		for(var i:uint = inc; i< data.length; i++)
		{
			var tmp:Object = data[i];
			for(var j:uint = i; j >= inc && data[j-inc] > tmp; j -=inc)
			{
				data[j] = data[j-inc];
			}
			data[j] = tmp;
		}
		inc = Math.round(inc/2.2);
	}
	return data;
}

```



## Ada

This is a generic implementation of the shell sort. Ada allows arrays to be indexed by integer or enumeration types starting at any value. This version deals with any kind or value of valid index type.

```ada
generic
   type Element_Type is digits <>;
   type Index_Type is (<>);
   type Array_Type is array(Index_Type range <>) of Element_Type;
package Shell_Sort is
   procedure Sort(Item : in out Array_Type);
end Shell_Sort;
```


```ada
package body Shell_Sort is

   ----------
   -- Sort --
   ----------

   procedure Sort (Item : in out Array_Type) is
      Increment : Natural := Index_Type'Pos(Item'Last) / 2;
      J : Index_Type;
      Temp : Element_Type;
   begin
      while Increment > 0 loop
         for I in Index_Type'Val(Increment) .. Item'Last loop
            J := I;
            Temp := Item(I);
            while J > Index_Type'val(Increment) and then Item (Index_Type'Val(Index_Type'Pos(J) - Increment)) > Temp loop
               Item(J) := Item (Index_Type'Val(Index_Type'Pos(J) - Increment));
               J := Index_Type'Val(Index_Type'Pos(J) - Increment);
            end loop;
            Item(J) := Temp;
         end loop;
         if Increment = 2 then
            Increment := 1;
         else
            Increment := Increment * 5 / 11;
         end if;
      end loop;
   end Sort;

end Shell_Sort;
```



## ALGOL 68

'''File: prelude/sort/shell.a68'''
```algol68
# -*- coding: utf-8 -*- #

COMMENT
  REQUIRES(
    MODE SORTELEMENT = mode of element of array to be sorted...
    OP < = (SORTELEMENT a, b)BOOL: a < b;
  )
END COMMENT

MODE SORTELEMENTCMP = PROC(SORTELEMENT,SORTELEMENT)BOOL;

# create a global sort procedure for convenience #
PROC(SORTELEMENT,SORTELEMENT)BOOL sort cmp default := (SORTELEMENT a, b)BOOL: a < b;
PROC sort cmp rev = (SORTELEMENT a, b)BOOL: NOT sort cmp default(a,b);

# Alternative gap calculations: #
#     ⌊n/2**k⌋; ⌊n/2⌋; Θ(n**2) [when n=2**p]; Donald Shell 1959 #
PROC sort gap shell = (INT k, n)INT: n OVER 2;
#     2 ⌊n/2**(k+1)⌋+1; 2 ⌊n/4⌋+1, ..., 3, 1; Θ(n**(3/2)); Frank & Lazarus, 1960 #
#     2**k-1; 1, 3, 7, 15, 31, 63, ...; Θ(n**(3/2)); Hibbard, 1963 #
#     2**k+1, prefixed with 1; 1, 3, 5, 9, 17, 33, 65, ...; Θ(n**(3/2)); Papernov & Stasevich, 1965 #
#     successive numbers of the form 2**p 3**q; 1, 2, 3, 4, 6, 8, 9, 12, ...; Θ(n log**2 n); Pratt 1971 #
#     (3**k-1)/2, not greater than ⌈n/3⌉; 1, 4, 13, 40, 121, ...; Θ(n**(3/2)); Knuth 1973 #
#     ∏a[q], where r=⌊√(2k+√(2k))⌋ and a[q]=min(n∈𝒩:n≥(5/2)**(q+1) and ∀ p:0≤ p<q → gcd(a[p],n)=1);
      limit where 0≤q<r and q≠(r**2+r)/2-k
         1, 3, 7, 21, 48, 112, ...; O(n e**√(8ln(5/2)ln n)); Incerpi & Sedgewick, 1985 #
#     4**k+3×2**(k-1)+1, prefixed with 1; 1, 8, 23, 77, 281, ...; Θ(n**(4/3)); Sedgewick, 1986 #
#     9(4**(k-1)-2**(k-1))+1, 4**(k+1)-6×2**k+1; 1, 5, 19, 41, 109, ...; Θ(n**(4/3)); Sedgewick, 1986 #
#     h[k]=max(⌊5h[k-1]/11⌋, 1), h[0]=n; ⌊5N/11⌋, ⌊5/11 ⌊5N/11⌋⌋, ..., 1; Θ(?); Gonnet & Baeza-Yates, 1991 #
PROC sort gap gonnet and baeza yates = (INT k, n)INT: IF n=2 THEN 1 ELSE n*5 OVER 11 FI;
#     ⌈(9**k-4**k)/(5×4**(k-1))⌉; 1, 4, 9, 20, 46, 103, ...; Θ(?); Tokuda, 1992 #
#     unknown; 1, 4, 10, 23, 57, 132, 301, 701; Θ(?); Ciura, 2001 #

# set default gap calculation #
PROC (INT #k#, INT #n#)INT sort gap := sort gap gonnet and baeza yates;

PROC shell sort in place = (REF []SORTELEMENT array, UNION(VOID, SORTELEMENTCMP) opt cmp)REF[]SORTELEMENT:(
  SORTELEMENTCMP cmp := (opt cmp|(SORTELEMENTCMP cmp): cmp | sort cmp default);
  INT n := ( UPB array + LWB array + 1 ) OVER 2; # initial gap #
  FOR k WHILE n NE 0 DO
    FOR index FROM LWB array TO UPB array DO
      INT i := index;
      SORTELEMENT element = array[i];
      WHILE ( i - LWB array >= n | cmp(element, array[i-n]) | FALSE ) DO
        array[i] := array[i-n];
        i -:= n
      OD;
      array[i] := element
    OD;
    n := sort gap(k,n)
  OD;
  array
);

PROC shell sort = ([]SORTELEMENT seq)[]SORTELEMENT:
  shell sort in place(LOC[LWB seq: UPB seq]SORTELEMENT:=seq, EMPTY);

PROC shell sort rev = ([]SORTELEMENT seq)[]SORTELEMENT:
  shell sort in place(LOC[LWB seq: UPB seq]SORTELEMENT:=seq, sort cmp rev);

SKIP
```
'''File: test/sort/shell.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

MODE SORTELEMENT = CHAR;
PR read "prelude/sort/shell.a68" PR;

[]SORTELEMENT char array data = "big fjords vex quick waltz nymph";
print((shell sort(char array data), new line))
```

```txt

     abcdefghiijklmnopqrstuvwxyz

```


## ARM Assembly

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program shellSort.s   */

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
szMessSortOk:       .asciz "Table sorted.\n"
szMessSortNok:      .asciz "Table not sorted !!!!!.\n"
sMessResult:        .ascii "Value  : "
sMessValeur:        .fill 11, 1, ' '            @ size => 11
szCarriageReturn:   .asciz "\n"

.align 4
iGraine:  .int 123456
.equ NBELEMENTS,      10
#TableNumber:      .int   1,3,6,2,5,9,10,8,4,7
TableNumber:     .int   10,9,8,7,6,5,4,3,2,1
/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                             @ entry of program

1:
    ldr r0,iAdrTableNumber                        @ address number table
    mov r1,#0                                     @ not use in routine
    mov r2,#NBELEMENTS                            @ number of élements
    bl shellSort
    ldr r0,iAdrTableNumber                        @ address number table
    bl displayTable

    ldr r0,iAdrTableNumber                        @ address number table
    mov r1,#NBELEMENTS                            @ number of élements
    bl isSorted                                   @ control sort
    cmp r0,#1                                     @ sorted ?
    beq 2f
    ldr r0,iAdrszMessSortNok                      @ no !! error sort
    bl affichageMess
    b 100f
2:                                                @ yes
    ldr r0,iAdrszMessSortOk
    bl affichageMess
100:                                              @ standard end of the program
    mov r0, #0                                    @ return code
    mov r7, #EXIT                                 @ request to exit program
    svc #0                                        @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:     .int szCarriageReturn
iAdrsMessResult:          .int sMessResult
iAdrTableNumber:          .int TableNumber
iAdrszMessSortOk:         .int szMessSortOk
iAdrszMessSortNok:        .int szMessSortNok
/******************************************************************/
/*     control sorted table                                   */
/******************************************************************/
/* r0 contains the address of table */
/* r1 contains the number of elements  > 0  */
/* r0 return 0  if not sorted   1  if sorted */
isSorted:
    push {r2-r4,lr}                                    @ save registers
    mov r2,#0
    ldr r4,[r0,r2,lsl #2]
1:
    add r2,#1
    cmp r2,r1
    movge r0,#1
    bge 100f
    ldr r3,[r0,r2, lsl #2]
    cmp r3,r4
    movlt r0,#0
    blt 100f
    mov r4,r3
    b 1b
100:
    pop {r2-r4,lr}
    bx lr                                              @ return
/***************************************************/
/*   shell Sort                                    */
/***************************************************/

/* r0 contains the address of table */
/* r1 contains the first element but not use !!   */
/*   this routine use first element at index zero !!!  */
/* r2 contains the number of element */
shellSort:
    push {r0-r7,lr}              @save registers

    sub r2,#1                    @ index last item
    mov r1,r2                    @ init gap = last item
1:                               @ start loop 1
    lsrs r1,#1                   @ gap = gap / 2
    beq 100f                     @ if gap = 0 -> end
    mov r3,r1                    @ init loop indice 1
2:                               @ start loop 2
    ldr r4,[r0,r3,lsl #2]        @ load first value
    mov r5,r3                    @ init loop indice 2
3:                               @ start loop 3
    cmp r5,r1                    @ indice < gap
    blt 4f                       @ yes -> end loop 2
    sub r6,r5,r1                 @ index = indice - gap
    ldr r7,[r0,r6,lsl #2]        @ load second value
    cmp r4,r7                    @ compare values
    strlt r7,[r0,r5,lsl #2]      @ store if <
    sublt r5,r1                  @ indice = indice - gap
    blt 3b                       @ and loop
4:                               @ end loop 3
    str r4,[r0,r5,lsl #2]        @ store value 1 at indice 2
    add r3,#1                    @ increment indice 1
    cmp r3,r2                    @ end ?
    ble 2b                       @ no -> loop 2
    b 1b                         @ yes loop for new gap

100:                             @ end function
    pop {r0-r7,lr}               @ restaur registers
    bx lr                        @ return


/******************************************************************/
/*      Display table elements                                */
/******************************************************************/
/* r0 contains the address of table */
displayTable:
    push {r0-r3,lr}                                    @ save registers
    mov r2,r0                                          @ table address
    mov r3,#0
1:                                                     @ loop display table
    ldr r0,[r2,r3,lsl #2]
    ldr r1,iAdrsMessValeur                             @ display value
    bl conversion10                                    @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                                   @ display message
    add r3,#1
    cmp r3,#NBELEMENTS - 1
    ble 1b
    ldr r0,iAdrszCarriageReturn
    bl affichageMess
100:
    pop {r0-r3,lr}
    bx lr
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
    push {r1-r4,lr}                                 @ save registers
    mov r3,r1
    mov r2,#LGZONECAL

1:                                                  @ start loop
    bl divisionpar10U                               @ unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                                      @ digit
    strb r1,[r3,r2]                                 @ store digit on area
    cmp r0,#0                                       @ stop if quotient = 0
    subne r2,#1                                     @ else previous position
    bne 1b                                          @ and loop
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

```



## AutoHotkey

ahk forum: [http://www.autohotkey.com/forum/viewtopic.php?t=44657&postdays=0&postorder=asc&start=131 discussion]

```AutoHotkey
MsgBox % ShellSort("")
MsgBox % ShellSort("xxx")
MsgBox % ShellSort("3,2,1")
MsgBox % ShellSort("dog,000000,xx,cat,pile,abcde,1,cat,zz,xx,z")
MsgBox % ShellSort("12,11,10,9,8,4,5,6,7,3,2,1,10,13,14,15,19,17,18,16,20,10")

ShellSort(var) {                         ; SORT COMMA SEPARATED LIST
   StringSplit a, var, `,                ; make array (length = a0)
   inc := a0
   While inc:=round(inc/2.2)             ; geometric gap sequence
      Loop % a0-inc {                    ; insertion sort:
         i := A_Index+inc, t := a%i%, j := i, k := j-inc
         While j > inc && a%k% > t
            a%j% := a%k%, j := k, k -= inc
         a%j% := t
      }
   Loop % a0                             ; construct string from sorted array
      s .= "," . a%A_Index%
   Return SubStr(s,2)                    ; drop leading comma
}
```



## AWK

```awk
{
  line[NR] = $0
}
END { # sort it with shell sort
  increment = int(NR / 2)
  while ( increment > 0 ) {
    for(i=increment+1; i <= NR; i++) {
      j = i
      temp = line[i]
      while ( (j >= increment+1) && (line[j-increment] > temp) ) {
	line[j] = line[j-increment]
	j -= increment
      }
      line[j] = temp
    }
    if ( increment == 2 )
      increment = 1
    else
      increment = int(increment*5/11)
  }
  #print it
  for(i=1; i <= NR; i++) {
    print line[i]
  }
}
```



## BBC BASIC

Note that the array index is assumed to start at zero.

```bbcbasic
      DIM test(9)
      test() = 4, 65, 2, -31, 0, 99, 2, 83, 782, 1
      PROCshellsort(test(), 10)
      FOR i% = 0 TO 9
        PRINT test(i%) ;
      NEXT
      PRINT
      END

      DEF PROCshellsort(a(), n%)
      LOCAL h%, i%, j%, k
      h% = n%
      WHILE h%
        IF h% = 2 h% = 1 ELSE h% DIV= 2.2
        FOR i% = h% TO n% - 1
          k = a(i%)
          j% = i%
          WHILE j% >= h% AND k < a(ABS(j% - h%))
            a(j%) = a(j% - h%)
            j% -= h%
          ENDWHILE
          a(j%) = k
        NEXT
      ENDWHILE
      ENDPROC
```

```txt

       -31         0         1         2         2         4        65        83        99       782

```



## BCPL


```BCPL
GET "libhdr"

LET shellsort(v, upb) BE
{ LET m = 1
  UNTIL m>upb DO m := m*3 + 1  // Find first suitable value in the
                                // series:  1, 4, 13, 40, 121, 364, ...
  { m := m/3
    FOR i = m+1 TO upb DO
    { LET vi = v!i
      LET j = i
      { LET k = j - m
        IF k<=0 | v!k < vi BREAK
        v!j := v!k
        j := k
      } REPEAT
      v!j := vi
    }
 } REPEATUNTIL m=1
}

MANIFEST { upb = 10000  }

LET start() = VALOF
{ LET v = getvec(upb)

  try("shell", shellsort, v, upb)

  writes("*nEnd of test*n")
  freevec(v)
  RESULTIS 0
}

AND try(name, sortroutine, v, upb) BE
{ // delay, referencing the first and last elements of v
   FOR i = 1 TO 50000 DO v!upb := v!1
   writef("*nSetting %n words of data for %s sort*n", upb, name)
   FOR i = 1 TO upb DO v!i := randno(10000)
   writef("Entering %s sort routine*n", name)
   sortroutine(v, upb)
   writes("Sorting complete*n")
   TEST sorted(v, upb)
   THEN writes("The data is now sorted*n")
   ELSE writef("### ERROR: %s sort does not work*n", name)
}

AND sorted(v, n) = VALOF
{ //FOR i = 1 TO n-1 UNLESS v!i<=v!(i+1) RESULTIS FALSE
   RESULTIS TRUE
}
```



## C


```c
#include <stdio.h>

void shell_sort (int *a, int n) {
    int h, i, j, t;
    for (h = n; h /= 2;) {
        for (i = h; i < n; i++) {
            t = a[i];
            for (j = i; j >= h && t < a[j - h]; j -= h) {
                a[j] = a[j - h];
            }
            a[j] = t;
        }
    }
}

int main (int ac, char **av) {
    int a[] = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1};
    int n = sizeof a / sizeof a[0];
    int i;
    for (i = 0; i < n; i++)
        printf("%d%s", a[i], i == n - 1 ? "\n" : " ");
    shell_sort(a, n);
    for (i = 0; i < n; i++)
        printf("%d%s", a[i], i == n - 1 ? "\n" : " ");
    return 0;
}

```

```txt

4 65 2 -31 0 99 2 83 782 1
-31 0 1 2 2 4 65 83 99 782

```



## C++


```cpp

#include <time.h>
#include <iostream>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const int MAX = 126;
class shell
{
public:
    shell()
    { _gap[0] = 1750; _gap[1] = 701; _gap[2] = 301; _gap[3] = 132; _gap[4] = 57; _gap[5] = 23; _gap[6] = 10; _gap[7] = 4; _gap[8] = 1; }

    void sort( int* a, int count )
    {
	_cnt = count;
	for( int x = 0; x < 9; x++ )
	    if( count > _gap[x] )
	    { _idx = x; break; }

	sortIt( a );
    }

private:
    void sortIt( int* arr )
    {
	bool sorted = false;
	while( true )
	{
	    sorted = true;
	    int st = 0;
	    for( int x = _gap[_idx]; x < _cnt; x += _gap[_idx] )
	    {
		if( arr[st] > arr[x] )
		{ swap( arr[st], arr[x] ); sorted = false; }
		st = x;
	    }
	    if( ++_idx >= 8 ) _idx = 8;
	    if( sorted && _idx == 8 ) break;
	}
    }

    void swap( int& a, int& b ) { int t = a; a = b; b = t; }

    int _gap[9], _idx, _cnt;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( static_cast<unsigned int>( time( NULL ) ) ); int arr[MAX];
    for( int x = 0; x < MAX; x++ )
	arr[x] = rand() % MAX - rand() % MAX;

    cout << " Before: \n
### ===
\n";
    for( int x = 0; x < 7; x++ )
    {
	for( int a = 0; a < 18; a++ )
	{ cout << arr[x * 18 + a] << " "; }
	cout << endl;
    }
    cout << endl; shell s; s.sort( arr, MAX );

    cout << " After: \n
### ==
\n";
    for( int x = 0; x < 7; x++ )
    {
	for( int a = 0; a < 18; a++ )
	{ cout << arr[x * 18 + a] << " "; }
	cout << endl;
    }
    cout << endl << endl; return system( "pause" );
}
//--------------------------------------------------------------------------------------------------

```

```txt

Before:

### =

-28 64 51 96 24 -51 15 4 51 37 -28 64 -18 -45 63 -64 -75 16
32 -44 -26 -50 -30 94 -55 -60 51 -30 14 -16 -42 22 91 -85 100 -14
-35 20 -73 11 -65 53 -25 -21 -65 16 -36 35 -69 -16 -13 -21 -103 80
-51 40 2 -7 11 29 65 -28 63 -108 -45 -8 -11 73 -8 -34 41 -20
-55 -64 4 41 5 -13 37 -39 -11 20 -24 -62 30 -19 30 -17 -11 -15
104 -14 -35 14 5 20 58 -38 6 -41 -23 88 49 -7 -54 -40 10 6
-57 -77 -6 -72 122 23 -39 67 121 63 28 31 43 -33 -1 59 -5 -91

After:
======
-108 -103 -91 -85 -77 -75 -73 -72 -69 -65 -65 -64 -64 -62 -60 -57 -55 -55
-54 -51 -51 -50 -45 -45 -44 -42 -41 -40 -39 -39 -38 -36 -35 -35 -34 -33
-30 -30 -28 -28 -28 -26 -25 -24 -23 -21 -21 -20 -19 -18 -17 -16 -16 -15
-14 -14 -13 -13 -11 -11 -11 -8 -8 -7 -7 -6 -5 -1 2 4 4 5
5 6 6 10 11 11 14 14 15 16 16 20 20 20 22 23 24 28
29 30 30 31 32 35 37 37 40 41 41 43 49 51 51 51 53 58
59 63 63 63 64 64 65 67 73 80 88 91 94 96 100 104 121 122

```


## C#
<lang C sharp|C#>
public static class ShellSorter
{
    public static void Sort<T>(IList<T> list) where T : IComparable
    {
        int n = list.Count;
        int h = 1;

        while (h < (n >> 1))
        {
            h = (h << 1) + 1;
        }

        while (h >= 1)
        {
            for (int i = h; i < n; i++)
            {
                int k = i - h;
                for (int j = i; j >= h && list[j].CompareTo(list[k]) < 0; k -= h)
                {
                    T temp = list[j];
                    list[j] = list[k];
                    list[k] = temp;
                    j = k;
                }
            }
            h >>= 1;
        }
    }
}

```

```txt

Before:

### =

-28 64 51 96 24 -51 15 4 51 37 -28 64 -18 -45 63 -64 -75 16
32 -44 -26 -50 -30 94 -55 -60 51 -30 14 -16 -42 22 91 -85 100 -14
-35 20 -73 11 -65 53 -25 -21 -65 16 -36 35 -69 -16 -13 -21 -103 80
-51 40 2 -7 11 29 65 -28 63 -108 -45 -8 -11 73 -8 -34 41 -20
-55 -64 4 41 5 -13 37 -39 -11 20 -24 -62 30 -19 30 -17 -11 -15
104 -14 -35 14 5 20 58 -38 6 -41 -23 88 49 -7 -54 -40 10 6
-57 -77 -6 -72 122 23 -39 67 121 63 28 31 43 -33 -1 59 -5 -91

After:
======
-108 -103 -91 -85 -77 -75 -73 -72 -69 -65 -65 -64 -64 -62 -60 -57 -55 -55
-54 -51 -51 -50 -45 -45 -44 -42 -41 -40 -39 -39 -38 -36 -35 -35 -34 -33
-30 -30 -28 -28 -28 -26 -25 -24 -23 -21 -21 -20 -19 -18 -17 -16 -16 -15
-14 -14 -13 -13 -11 -11 -11 -8 -8 -7 -7 -6 -5 -1 2 4 4 5
5 6 6 10 11 11 14 14 15 16 16 20 20 20 22 23 24 28
29 30 30 31 32 35 37 37 40 41 41 43 49 51 51 51 53 58
59 63 63 63 64 64 65 67 73 80 88 91 94 96 100 104 121 122

```



## COBOL


### Complete Program

Should work in Cobol/2 too. Picture for array to be sorted is purely fictional, there's no boundary check - hence: be careful!
Program will sort any array using standard EBCDIC sequence (won't work properly with signed packed variables). In addition to the "usual" array and array lenght parameters, you need to supply an area (initialized to low-values) to detail row-length and up to 10 sort keys defined as follows: start position (1 based), length and sequence (Ascending/Descending).


```cobol
      *******************************************************
       IDENTIFICATION DIVISION.
      *******************************************************
       PROGRAM-ID.      SHELLSRT.
      ************************************************************
      *** SHELLSORT                                           ****
      ************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 II                        PIC S9(008) COMP-5.
       01 IJ                        PIC S9(008) COMP-5.
       01 IZ                        PIC S9(008) COMP-5.
       01 IA                        PIC S9(008) COMP-5.
       01 STRT1                     PIC S9(008) COMP-5.
       01 STRT2                     PIC S9(008) COMP-5.
       01 LGT                       PIC S9(008) COMP-5.
       01 ORG                       PIC S9(008) COMP-5.
       01 DST                       PIC S9(008) COMP-5.
      *
       01 GAP                       PIC S9(008) COMP-5.
       01 NEGAP                     PIC S9(008) COMP-5.
       01 TEMP                      PIC X(32768).
       77 KEY-RESULT                PIC X.
      *
       LINKAGE SECTION.
       01 SRT-ARRAY                 PIC  X(1000000).
       01 NUM-ITEM                  PIC  9(008) COMP-5.
       01 SRT-DATA.
          03 LGT-ITEM               PIC  9(004) COMP-5.
          03 SRT-KEYS.
             05 SRT-KEY OCCURS 10.
                07 K-START         PIC S9(004) COMP-5.
                07 K-LENGTH        PIC S9(004) COMP-5.
                07 K-ASC           PIC X.
      *
      *    P R O C E D U R E      D I V I S I O N
      *
       PROCEDURE DIVISION USING SRT-ARRAY NUM-ITEM SRT-DATA.

           COMPUTE GAP = NUM-ITEM / 2.
           PERFORM UNTIL GAP < 1
              COMPUTE NEGAP = GAP * -1
              PERFORM VARYING II FROM GAP BY 1
                        UNTIL II GREATER  NUM-ITEM
                 MOVE ' ' TO KEY-RESULT
                 COMPUTE ORG = (II - 1) * LGT-ITEM + 1
                 MOVE SRT-ARRAY(ORG:LGT-ITEM) TO TEMP(1:LGT-ITEM)
                 PERFORM VARYING IJ FROM II BY NEGAP
                           UNTIL IJ NOT GREATER  GAP
                              OR (KEY-RESULT NOT EQUAL '<' AND ' ')
                    COMPUTE IA = IJ - GAP
                    IF IA < 1
                       MOVE 1 TO IA
                    END-IF
                    PERFORM COMPARE-KEYS
                    IF KEY-RESULT = '<'
                       COMPUTE ORG = (IA - 1) * LGT-ITEM + 1
                       COMPUTE DST = (IJ - 1) * LGT-ITEM + 1
                       MOVE SRT-ARRAY(ORG:LGT-ITEM)
                         TO SRT-ARRAY(DST:LGT-ITEM)
                       COMPUTE DST = (IA - 1) * LGT-ITEM + 1
                       MOVE TEMP(1:LGT-ITEM) TO SRT-ARRAY(DST:LGT-ITEM)
                    END-IF
                 END-PERFORM
              END-PERFORM
              IF GAP = 2
                 MOVE 1 TO GAP
              ELSE
                 COMPUTE GAP = GAP / 2.2
              END-IF
           END-PERFORM.
           GOBACK.
      *
       COMPARE-KEYS.
           MOVE ' ' TO KEY-RESULT
           PERFORM VARYING IZ FROM 1 BY 1
                     UNTIL IZ GREATER 10
                        OR (KEY-RESULT NOT EQUAL '=' AND ' ')
              IF SRT-KEY(IZ) GREATER LOW-VALUES
                 COMPUTE STRT1 = (IJ - 1) * LGT-ITEM + K-START(IZ)
                 COMPUTE STRT2 = (IA - 1) * LGT-ITEM + K-START(IZ)
                 MOVE K-LENGTH(IZ) TO LGT
                 IF SRT-ARRAY(STRT1:LGT) > SRT-ARRAY(STRT2:LGT) AND
                    K-ASC(IZ) EQUAL 'A'
                 OR SRT-ARRAY(STRT1:LGT) < SRT-ARRAY(STRT2:LGT) AND
                    K-ASC(IZ) EQUAL 'D'
                    MOVE '>' TO KEY-RESULT
                 END-IF
                 IF SRT-ARRAY(STRT1:LGT) < SRT-ARRAY(STRT2:LGT) AND
                    K-ASC(IZ) EQUAL 'A'
                 OR SRT-ARRAY(STRT1:LGT) > SRT-ARRAY(STRT2:LGT) AND
                    K-ASC(IZ) EQUAL 'D'
                    MOVE '<' TO KEY-RESULT
                 END-IF
              END-IF
           END-PERFORM.
           IF KEY-RESULT = ' '
              MOVE '=' TO KEY-RESULT
           END-IF.
```



### Sorting Process

This excerpt contains just enough of the procedure division to show the workings. See the example for the bubble sort for a more complete program.

```COBOL
       C-PROCESS SECTION.
       C-000.
           DISPLAY "SORT STARTING".

           DIVIDE WC-SIZE BY 2 GIVING WC-GAP.

           PERFORM E-PROCESS-GAP UNTIL WC-GAP = 0.

           DISPLAY "SORT FINISHED".

       C-999.
           EXIT.


       E-PROCESS-GAP SECTION.
       E-000.
           PERFORM F-SELECTION VARYING WB-IX-1 FROM WC-GAP BY 1
                               UNTIL WB-IX-1 > WC-SIZE.

           DIVIDE WC-GAP BY 2.2 GIVING WC-GAP.

       E-999.
           EXIT.

       F-SELECTION SECTION.
       F-000.
           SET WB-IX-2            TO WB-IX-1.
           MOVE WB-ENTRY(WB-IX-1) TO WC-TEMP.

           SET WB-IX-3 TO WB-IX-2.
           SET WB-IX-3 DOWN BY WC-GAP.
           PERFORM G-PASS UNTIL WB-IX-2 NOT > WC-GAP
      * The next line logically reads :
      *                   or wb-entry(wb-ix-2 - wc-gap) not > wc-temp.
                          OR WB-ENTRY(WB-IX-3) NOT > WC-TEMP.

           IF WB-IX-1 NOT = WB-IX-2
              MOVE WC-TEMP TO WB-ENTRY(WB-IX-2).

       F-999.
           EXIT.

       G-PASS SECTION.
      * Note that WB-IX-3 is WC-GAP less than WB-IX-2.
      * Logically this should be :
      *    move wb-entry(wb-ix-2 - wc-gap) to wb-entry(wb-ix-2).
      *    set wb-ix-2 down by wc-gap.
      * Unfortunately wb-entry(wb-ix-2 - wc-gap) is not legal in C2 cobol
       G-000.
           MOVE WB-ENTRY(WB-IX-3) TO WB-ENTRY(WB-IX-2).
           SET WB-IX-2            DOWN BY WC-GAP.
           SET WB-IX-3            DOWN BY WC-GAP.

       G-999.
           EXIT.
```



## Common Lisp



```lisp
(defun gap-insertion-sort (array predicate gap)
  (let ((length (length array)))
    (if (< length 2) array
      (do ((i 1 (1+ i))) ((eql i length) array)
        (do ((x (aref array i))
             (j i (- j gap)))
            ((or (< (- j gap) 0)
                 (not (funcall predicate x (aref array (1- j)))))
             (setf (aref array j) x))
          (setf (aref array j) (aref array (- j gap))))))))

(defconstant +gaps+
  '(1750 701 301 132 57 23 10 4 1)
  "The best sequence of gaps, according to Marcin Ciura.")

(defun shell-sort (array predicate &optional (gaps +gaps+))
  (assert (eql 1 (car (last gaps))) (gaps)
    "Last gap of ~w is not 1." gaps)
  (dolist (gap gaps array)
    (gap-insertion-sort array predicate gap)))
```



## D


```d
import std.stdio: writeln;

void shellSort(T)(T[] seq) pure nothrow {
    int inc = seq.length / 2;
    while (inc) {
        foreach (ref i, el; seq) {
            while (i >= inc && seq[i - inc] > el) {
                seq[i] = seq[i - inc];
                i -= inc;
            }
            seq[i] = el;
        }
        inc = (inc == 2) ? 1 : cast(int)(inc * 5.0 / 11);
    }
}

void main() {
    auto data = [22, 7, 2, -5, 8, 4];
    shellSort(data);
    writeln(data);
}
```

```txt
[-5, 2, 4, 7, 8, 22]
```



## Delphi


```delphi
Procedure ShellSort(var buf:Array of Integer);
const
  gaps:array[0..7] of Integer = (701, 301, 132, 57, 23, 10, 4, 1);

var
  whichGap, i, j, n, gap, temp : Integer;

begin
  n := high(buf);
  for whichGap := 0 to high(gaps) do begin
    gap := gaps[whichGap];
    for i := gap to n do begin
      temp := buf[i];

      j := i;
      while ( (j >= gap ) and ( (buf[j-gap] > dt) ) do begin
        buf[j] := buf[j-gap];
        dec(j, gap);
      end;
      buf[j] := temp;
    end;
  end;
end;
```



## E


```e
/** Shell sort (in-place) */
def shellSort(array) {
    var inc := array.size() // 2
    while (inc.aboveZero()) {
        for var i => a in array {
            while (i >= inc && (def b := array[i - inc]) > a) {
                array[i] := b
                i -= inc
            }
            array[i] := a
        }
        inc := if (inc <=> 2) { 1 } else { (inc * 5.0 / 11).floor() }
    }
}
```



## Eiffel

Translated from pseudocode at [http://en.wikipedia.org/wiki/Shell_sort#Shell_sort_algorithm_in_pseudocode Wikipedia]

This solution is shown in the routine <code lang="eiffel">sort</code> of the class <code lang="eiffel">MY_SORTED_SET</code>.

For a more complete explanation of the Eiffel sort examples, see [[Sorting algorithms/Bubble sort#Eiffel|Bubble sort]].


```eiffel
class
    MY_SORTED_SET [G -> COMPARABLE]
inherit
    TWO_WAY_SORTED_SET [G]
        redefine
            sort
        end
create
    make

feature

    sort
            -- Shell sort
        local
            inc: INTEGER
            j: INTEGER
            l_value: like item
        do
            from
                inc := (count.to_double / 2.0).rounded
            until
                inc <= 0
            loop
                across inc |..| (count - 1) as ii
                loop
                    l_value := Current [ii.item + 1]
                    from
                        j := ii.item
                    until
                        j < inc or Current [j - inc + 1] <= l_value
                    loop
                        Current [j + 1] := Current [j - inc + 1]
                        j := j - inc
                    end
                    Current [j + 1] := l_value
                end
                inc := (inc.to_double / 2.2).rounded
            end
        end

end
```



## Elixir


```elixir
defmodule Sort do
  def shell_sort(list) when length(list)<=1, do: list
  def shell_sort(list), do: shell_sort(list, div(length(list),2))

  defp shell_sort(list, inc) do
    gb = Enum.with_index(list) |> Enum.group_by(fn {_,i} -> rem(i,inc) end)
    wk = Enum.map(0..inc-1, fn i ->
           Enum.map(gb[i], fn {x,_} -> x end) |> insert_sort([])
         end)
         |> merge
    if sorted?(wk), do: wk, else: shell_sort( wk, max(trunc(inc / 2.2), 1) )
  end

  defp merge(lists) do
    len = length(hd(lists))
    Enum.map(lists, fn list -> if length(list)<len, do: list++[nil], else: list end)
    |> List.zip
    |> Enum.flat_map(fn tuple -> Tuple.to_list(tuple) end)
    |> Enum.filter(&(&1))               # remove nil
  end

  defp sorted?(list) do
    Enum.chunk(list,2,1) |> Enum.all?(fn [a,b] -> a <= b end)
  end

  defp insert_sort(list), do: insert_sort(list, [])

  defp insert_sort([], sorted), do: sorted
  defp insert_sort([h | t], sorted), do: insert_sort(t, insert(h, sorted))

  defp insert(x, []), do: [x]
  defp insert(x, sorted) when x < hd(sorted), do: [x | sorted]
  defp insert(x, [h | t]), do: [h | insert(x, t)]
end

list = [0, 14, 11, 8, 13, 15, 5, 7, 16, 17, 1, 6, 12, 2, 10, 4, 19, 9, 18, 3]
IO.inspect Sort.shell_sort(list)
```


```txt

[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]

```



## Euphoria


```euphoria
function shell_sort(sequence s)
    integer gap,j
    object temp
    gap = floor(length(s)/2)
    while gap > 0 do
        for i = gap to length(s) do
            temp = s[i]
            j=i-gap
            while j >= 1 and compare(temp, s[j]) <= 0 do
                s[j+gap]=s[j]
                j -= gap
            end while
            s[j+gap] = temp
        end for
        gap = floor(gap/2)
    end while
    return s
end function

constant s = rand(repeat(1000,10))
puts(1,"Before: ")
? s
puts(1,"After:  ")
? shell_sort(s)
```


```txt
Before: {501,49,558,719,991,246,500,98,925,398}
After:  {49,98,246,398,500,501,558,719,925,991}

```



## Forth

```forth
defer less?   ' < is less?

: shell { array len -- }
  1 begin dup len u<= while 2* 1+ repeat { gap }
  begin gap 2 = if 1 else gap 5 11 */ then dup to gap while
    len gap do
      array i cells +
      dup @ swap         ( temp last )
      begin gap cells -
            array over u<=
      while 2dup @ less?
      while dup gap cells + over @ swap !
      repeat then
      gap cells + !
    loop
  repeat ;

create array 8 , 1 , 4 , 2 , 10 , 3 , 7 , 9 , 6 , 5 ,

array 10 shell
array 10 cells dump
```


A version without local variables:

```forth
defer precedes ' < is precedes

: (shell)                              ( a n h -- a n h)
  over >r tuck                         ( a h n h)
  ?do                                  ( a h)
    i swap >r                          ( a j        R: h)
    2dup cells + @ -rot                ( k a j      R: h)
    begin                              ( k a j      R: h)
      dup r@ - dup >r 0< 0=            ( k a j f    R: h j-h)
    while                              ( k a j      R: h j-h)
      -rot over over r@ cells + @      ( j k a k v  R: h j-h)
      precedes >r rot r>               ( k a j f    R: h j-h)
    while                              ( k a j      R: h j-h)
      over r@ cells + @ >r             ( k a j      R: h j-h a[j-h])
      2dup cells + r> swap !           ( k a j      R: h j-h)
      drop r>                          ( k a j'     R: h)
    repeat then                        ( k a j      R: h j-h)
    rot >r 2dup cells +                ( a j a[j]   R: h j-h k)
    r> swap ! r> drop drop r>          ( a h)
  loop r> swap
;
                                       ( a n --)
: shell dup begin dup 2 = if 2/ else 5 11 */ then dup while (shell) repeat drop 2drop ;

create array 8 , 1 , 4 , 2 , 10 , 3 , 7 , 9 , 6 , 5 ,

array 10 shell
array 10 cells dump
```



## Fortran

```fortran
MODULE sort

CONTAINS

SUBROUTINE Shell_Sort(a)

  IMPLICIT NONE
  INTEGER :: i, j, increment
  REAL :: temp
  REAL, INTENT(in out) :: a(:)

  increment = SIZE(a) / 2
  DO WHILE (increment > 0)
      DO i = increment+1, SIZE(a)
         j = i
         temp = a(i)
         DO WHILE (j >= increment+1 .AND. a(j-increment) > temp)
            a(j) = a(j-increment)
            j = j - increment
         END DO
         a(j) = temp
      END DO
      IF (increment == 2) THEN
   	  increment = 1
      ELSE
         increment = increment * 5 / 11
      END IF
  END DO

END SUBROUTINE Shell_Sort

END MODULE sort

PROGRAM Shellsort

USE sort

  IMPLICIT NONE
  REAL :: array(1000)

  CALL RANDOM_SEED
  CALL RANDOM_NUMBER(array)

  WRITE (*,*) "Unsorted array"
  WRITE (*,*) array
  WRITE (*,*)
  CALL Shell_Sort(array)
  WRITE (*,*) "Sorted array"
  WRITE (*,*) array

END PROGRAM Shellsort
```



## FreeBASIC

modified bubble sort code

```freebasic
' version 21-10-2016
' compile with: fbc -s console
' for boundry checks on array's compile with: fbc -s console -exx

Sub shellsort(s() As Long)
    ' sort from lower bound to the highter bound
    ' array's can have subscript range from -2147483648 to +2147483647
    Dim As Long lb = LBound(s)
    Dim As Long ub = UBound(s)
    Dim As Long done, i, inc = ub - lb

    Do
        inc = Int(inc / 2.2)
        If inc < 1 Then inc = 1

        Do
            done = 0
            For i = lb To ub - inc
                ' replace "<" with ">" for downwards sort
                If s(i) > s(i + inc) Then
                    Swap s(i), s(i + inc)
                    done = 1
                End If
            Next
        Loop Until done = 0

    Loop Until inc = 1

End Sub

' ------=< MAIN >=------

Dim As Long i, array(-7 To 7)

Dim As Long a = LBound(array), b = UBound(array)

Randomize Timer
For i = a To b : array(i) = i  : Next
For i = a To b ' little shuffle
    Swap array(i), array(Int(Rnd * (b - a +1)) + a)
Next

Print "unsorted ";
For i = a To b : Print Using "####"; array(i); : Next : Print
shellsort(array())  ' sort the array
Print "  sorted ";
For i = a To b : Print Using "####"; array(i); : Next : Print

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
unsorted    1  -4  -1   7  -6   3   6  -7  -5   2  -2   0   5   4  -3
  sorted   -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7
```



## Go

Following WP pseudocode:

```go
package main

import "fmt"

var a = []int{170, 45, 75, -90, -802, 24, 2, 66}

func main() {
    fmt.Println("before:", a)
    for inc := len(a) / 2; inc > 0; inc = (inc + 1) * 5 / 11 {
        for i := inc; i < len(a); i++ {
            j, temp := i, a[i]
            for ; j >= inc && a[j-inc] > temp; j -= inc {
                a[j] = a[j-inc]
            }
            a[j] = temp
        }
    }
    fmt.Println("after: ", a)
}
```

```txt

before: [170 45 75 -90 -802 24 2 66]
after:  [-802 -90 2 24 45 66 75 170]

```



## Haskell

Adapted version from [http://en.wikibooks.org/wiki/Algorithm_Implementation/Sorting/Shell_sort#Haskell]


```haskell
import Data.List

shellSort xs = foldr (invColumnize (map (foldr insert []))) xs gaps
  where gaps = takeWhile (< length xs) $ iterate (succ.(3*)) 1
        invColumnize f k = concat. transpose. f. transpose
                           . takeWhile (not.null). unfoldr (Just. splitAt k)
```



## Io

Translated from pseudocode at [[wp:Shell_sort#Shell_sort_algorithm_in_pseudocode|Wikipedia]]

```io
List do(
    shellSortInPlace := method(
        gap := (size / 2) round
        while(gap > 0,
            for(i, gap, size - 1,
                key := at(i)
                j := i

                while(j >= gap and at(j - gap) > key,
                    atPut(j, at(j - gap))
                    j = j - gap
                )
                atPut(j, key)
            )
            gap = (gap / 2.2) round
        )
    self)
)

l := list(2, 3, 4, 5, 1)
l shellSortInPlace println # ==> list(1, 2, 3, 4, 5)
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()                     #: demonstrate various ways to sort a list and string
   demosort(shellsort,[3, 14, 1, 5, 9, 2, 6, 3],"qwerty")
end

procedure shellsort(X,op)             #: return sorted X
local i,j,inc,temp

   op := sortop(op,X)                 # select how and what we sort

   inc := *X/2
   while inc > 0 do {
      every i := inc to *X do {
         temp := X[j := i]
         while op(temp,X[j - (j >= inc)]) do
            X[j] := X[j -:= inc]
         X[j] := temp
         }
      inc := if inc = 2 then 1 else inc*5/11 # switch to insertion near the end
      }
   return X
end
```


Note: This example relies on [[Sorting_algorithms/Bubble_sort#Icon| the supporting procedures 'sortop', and 'demosort' in Bubble Sort]]. The full demosort exercises the named sort of a list with op = "numeric", "string", ">>" (lexically gt, descending),">" (numerically gt, descending), a custom comparator, and also a string.

{{out}} Abbreviated sample

```txt
Sorting Demo using procedure shellsort
  on list : [ 3 14 1 5 9 2 6 3 ]
    with op = &null:         [ 1 2 3 3 5 6 9 14 ]   (0 ms)
  ...
  on string : "qwerty"
    with op = &null:         "eqrtwy"   (0 ms)
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "ShellSrt.bas"
110 RANDOMIZE
120 LET N=20 ! Number of elements
130 NUMERIC ARRAY(1 TO N)
140 CALL INIT(ARRAY)
150 CALL WRITE(ARRAY)
160 CALL SHELLSORT(ARRAY)
170 CALL WRITE(ARRAY)
180 DEF INIT(REF A)
190   FOR I=LBOUND(A) TO UBOUND(A)
200     LET A(I)=RND(N)+1
210   NEXT
220 END DEF
230 DEF WRITE(REF A)
240   FOR I=LBOUND(A) TO UBOUND(A)
250     PRINT A(I);
260   NEXT
270   PRINT
280 END DEF
290 DEF SHELLSORT(REF A)
300   LET D=2^INT(LOG(N)/LOG(2))-1
310   DO
320     LET I=1
330     DO WHILE I<=D AND I+D<=N
340       FOR J=I+D TO N STEP D
350         LET AH=A(J):LET BH=J-D
360         DO WHILE BH>0 AND AH<A(BH)
370           LET A(BH+D)=A(BH):LET BH=BH-D
380         LOOP
390         LET A(BH+D)=AH
400       NEXT
410       LET I=I+1
420     LOOP
430     LET D=INT(D/2)
440   LOOP WHILE D>0
450 END DEF
```



## J

'''Solution'''

```j
gaps      =: [: }: 1 (1+3*])^:(> {:)^:a:~ #
insert    =: (I.~ {. ]) , [ , ] }.~ I.~
gapinss   =: #@] {. ,@|:@(] insert//.~ #@] $ i.@[)
shellSort =: [: ; gapinss &.>/@(< ,~ ]&.>@gaps)
```


Example:


```J
   shellSort 8 6 4 2 1 3 5 7 9
1 2 3 4 5 6 7 8 9
```



## Java

This method will sort in place. If you want to preserve your unsorted array, use a copy of the array as an argument to this method.

```java
public static void shell(int[] a) {
	int increment = a.length / 2;
	while (increment > 0) {
		for (int i = increment; i < a.length; i++) {
			int j = i;
			int temp = a[i];
			while (j >= increment && a[j - increment] > temp) {
				a[j] = a[j - increment];
				j = j - increment;
			}
			a[j] = temp;
		}
		if (increment == 2) {
			increment = 1;
		} else {
			increment *= (5.0 / 11);
		}
	}
}
```



## JavaScript


```Javascript
function shellSort (a) {
    for (var h = a.length; h > 0; h = parseInt(h / 2)) {
        for (var i = h; i < a.length; i++) {
            var k = a[i];
            for (var j = i; j >= h && k < a[j - h]; j -= h)
                a[j] = a[j - h];
            a[j] = k;
        }
    }
    return a;
}

var a = [];
var n = location.href.match(/\?(\d+)|$/)[1] || 10;
for (var i = 0; i < n; i++)
    a.push(parseInt(Math.random() * 100));
shellSort(a);
document.write(a.join(" "));
```



## jq

Adapted from the [[#Go]] entry on this page.

shellSort as defined here can be used to sort an array of arbitrary JSON entities.

```jq
# The "while" loops are implemented using the following jq function:

# As soon as "condition" is true, then emit . and stop:
def do_until(condition; next):
  def u: if condition then . else (next|u) end;
  u;

# sort the input array
def shellSort:
  length as $length
  | [ ($length/2|floor), .]                          # L1: state: [h, array]
  | do_until( .[0] == 0;
              .[0] as $h
              | reduce range($h; $length) as $i      # L2: state: array
                  ( .[1];
                   .[$i] as $k
                   | [ $i, . ]                       # L3: state: [j, array]
                   | do_until( .[0] < $h or ($k >= .[1][.[0] - $h]);
                               .[0] as $j
                               | [ ($j - $h), (.[1]|setpath([$j]; .[$j - $h])) ] )
                   | .[0] as $j | (.[1]|setpath([$j]; $k))  # i.e. a[j] = $k
                  )
              | [(($h+1)*5/11 | floor), .] )
       | .[1] ;
```

'''Example''':

```jq
([],
 [5,null,3,1,2,0,4.4,5]
) | shellSort
```

```sh
$ jq -M -c -n -f Shell_sort.jq
[]
[null,0,1,2,3,4.4,5,5]
```



## Julia

```julia
# v0.6

function shellsort!(a::Array{Int})::Array{Int}
    incr = div(length(a), 2)
    while incr > 0
        for i in incr+1:length(a)
            j = i
            tmp = a[i]
            while j > incr && a[j - incr] > tmp
                a[j] = a[j-incr]
                j -= incr
            end
            a[j] = tmp
        end
        if incr == 2
            incr = 1
        else
            incr = floor(Int, incr * 5.0 / 11)
        end
    end
    return a
end

x = rand(1:10, 10)
@show x shellsort!(x)
@assert issorted(x)
```


```txt
x = [2, 6, 9, 2, 3, 9, 5, 2, 5, 9]
shellsort!(x) = [2, 2, 2, 3, 5, 5, 6, 9, 9, 9]
```



## Kotlin


```scala
// version 1.1.0

val gaps = listOf(701, 301, 132, 57, 23, 10, 4, 1)  // Marcin Ciura's gap sequence

fun shellSort(a: IntArray) {
    for (gap in gaps) {
        for (i in gap until a.size) {
            val temp = a[i]
            var j = i
            while (j >= gap && a[j - gap] > temp) {
                a[j] = a[j - gap]
                j -= gap
            }
            a[j] = temp
        }
    }
}

fun main(args: Array<String>) {
    val aa = arrayOf(
        intArrayOf(100, 2, 56, 200, -52, 3, 99, 33, 177, -199),
        intArrayOf(4, 65, 2, -31, 0, 99, 2, 83, 782, 1),
        intArrayOf(62, 83, 18, 53, 7, 17, 95, 86, 47, 69, 25, 28)
    )
    for (a in aa) {
        shellSort(a)
        println(a.joinToString(", "))
    }
}
```


```txt

-199, -52, 2, 3, 33, 56, 99, 100, 177, 200
-31, 0, 1, 2, 2, 4, 65, 83, 99, 782
7, 17, 18, 25, 28, 47, 53, 62, 69, 83, 86, 95

```



## Liberty BASIC


```lb

siz = 100
dim a(siz)
for i = 1 to siz
 a(i) = int(rnd(1) * 1000)
next

' -------------------------------
' Shell Sort
' -------------------------------
   incr = int(siz / 2)
   WHILE incr > 0
         for i = 1 to siz
            j    = i
            temp = a(i)
               WHILE (j >= incr+1 and a(abs(j-incr)) > temp)
               a(j) = a(j-incr)
               j    = j - incr
               wend
            a(j) = temp
         next
         IF incr = 2 THEN
            incr = 1
         ELSE
            incr = int(incr * (5 / 11))
        end if
    WEND

for i = 1 to siz
print a(i)
next

```



## Lisaac


```Lisaac
Section Header

+ name := SHELL_SORT;

- external := `#include <time.h>`;

Section Public

- main <- (
  + a : ARRAY[INTEGER];

  a := ARRAY[INTEGER].create 0 to 100;
  `srand(time(NULL))`;
  0.to 100 do { i : INTEGER;
    a.put `rand()`:INTEGER to i;
  };

  shell a;

  a.foreach { item : INTEGER;
    item.print;
    '\n'.print;
  };
);

- shell a : ARRAY[INTEGER] <- (
  + lower, length, increment, temp : INTEGER;

  lower := a.lower;
  length := a.upper - lower + 1;
  increment := length;
  {
    increment := increment / 2;
    increment > 0
  }.while_do {
    increment.to (length - 1) do { i : INTEGER; + j : INTEGER;
      temp := a.item(lower + i);
      j := i - increment;
      { (j >= 0) && { a.item(lower + j) > temp } }.while_do {
        a.put (a.item(lower + j)) to (lower + j + increment);
        j := j - increment;
      };
      a.put temp to (lower + j + increment);
    };
  };
);
```



## Lua


```lua
function shellsort( a )
    local inc = math.ceil( #a / 2 )
    while inc > 0 do
        for i = inc, #a do
            local tmp = a[i]
            local j = i
            while j > inc and a[j-inc] > tmp do
                a[j] = a[j-inc]
                j = j - inc
            end
            a[j] = tmp
        end
        inc = math.floor( 0.5 + inc / 2.2 )
    end

    return a
end

a = { -12, 3, 0, 4, 7, 4, 8, -5, 9 }
a = shellsort( a )

for _, i in pairs(a) do
    print(i)
end
```



## M2000 Interpreter

We use & for passing by reference. Variables with % are integers, and can be any type, a double by default with no decimals, or Decimal, Currency, Long, Integer, Float. When we change value, using operators ++ -- += -= /= *= the final value round to integer using 0.5 where 1.5 give 2. So A%=1/2 give A%=1 and A%=-1/2 give A%=-1. A%=Int(1/2) give A%=0, A%=Int(-1/2) give A%=-1 (int is same as floor() and there is ceil() too, and there is a Bank() for bank type rounding)

For Next in M2000 always execute at least one time the code inside (we can change it using a switch, in M2000 environment, to act as in BASIC). From step get the absolute value, and direction get from starting and ending value. So For i=1 to 0 { } execute two times the block with standard switch "-For" or no execute if switch is "+For".
A For statement can be as in this example or the faster For  { } without Next


```M2000 Interpreter

Module ShellSortExample {
      Module shellsort(&a()) {
            DEf h%, i%, j%, k, n%
            n%=LEN(a())
            h% = n%
            WHILE h% {
                    IF h% = 2  THEN {h% = 1 }ELSE h%= h% DIV 2.2
                    FOR i% = h% TO n% - 1
                      k = a(i%)
                      j% = i%
                      WHILE j% >= h% AND k < a(ABS(j% - h%)) {
                              a(j%) = a(j% - h%)
                              j% -= h%
                        }
                      a(j%) = k
                    NEXT i%
            }
      }

      Dim numbers(10)
      numbers(0)=4, 65, 2, -31, 0, 99, 2, 83, 782, 1
      shellsort &numbers()
      Print numbers()
}
ShellSortExample

```



## Maple

<lang>shellsort := proc(arr)
	local n, gap, i, val, j;
	n := numelems(arr):
	gap := trunc(n/2):
	while (gap > 0) do #notice by 1 error
		for i from gap to n by 1 do
			val := arr[i];
			j := i;
			while (j > gap and arr[j-gap] > val) do
				arr[j] := arr[j-gap];
				j -= gap;
			end do;
			arr[j] := val;
		end do;
		gap := trunc(gap/2);
	end do;
end proc;
arr := Array([17,3,72,0,36,2,3,8,40,0]);
shellsort(arr);
arr;
```

```txt
[0,0,2,3,3,8,17,36,40,72]
```



## Mathematica


```Mathematica
shellSort[ lst_ ] := Module[ {list = lst, incr, temp, i, j},
 incr = Round[Length[list]/2];
 While[incr > 0,

  For[i = incr + 1, i <= Length[list], i++,

   temp = list[[i]]; j = i;

   While[(j >= (incr + 1)) && (list[[j - incr]] > temp) ,
    list[[j]] = list[[j - incr]];  j = j-incr;
   ];

   list[[j]] = temp;];
   If[incr == 2, incr = 1, incr = Round[incr/2.2]]
];  list
]
```



```txt
shellSort[{2,1,4,6,8}]
{1,2,4,6,8}
```


=={{header|MATLAB}} / {{header|Octave}}==
This is a translation of the FORTRAN solution into MATLAB.

```MATLAB
function list = shellSort(list)

    N = numel(list);
    increment = round(N/2);

    while increment > 0

        for i = (increment+1:N)
            temp = list(i);
            j = i;
            while (j >= increment+1) && (list(j-increment) > temp)
                list(j) = list(j-increment);
                j = j - increment;
            end

            list(j) = temp;

        end %for

        if increment == 2 %This case causes shell sort to become insertion sort
            increment = 1;
        else
            increment = round(increment/2.2);
        end
    end %while
end %shellSort
```


Sample Usage:

```MATLAB>>
 shellSort([4 3 1 5 6 2])

ans =

     1     2     3     4     5     6
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

placesList = [String -
    "UK  London",     "US  New York",   "US  Boston",     "US  Washington" -
  , "UK  Washington", "US  Birmingham", "UK  Birmingham", "UK  Boston"     -
]
sortedList = shellSort(String[] Arrays.copyOf(placesList, placesList.length))

lists = [placesList, sortedList]
loop ln = 0 to lists.length - 1
  cl = lists[ln]
  loop ct = 0 to cl.length - 1
    say cl[ct]
    end ct
    say
  end ln

return

method shellSort(a = String[]) public constant binary returns String[]

  n = a.length
  inc = int Math.round(double n / 2.0)
  loop label inc while inc > 0
    loop i_ = inc to n - 1
      temp = a[i_]
      j_ = i_
      loop label j_ while j_ >= inc
        if \(a[j_ - inc].compareTo(temp) > 0) then leave j_
        a[j_] = a[j_ - inc]
        j_ = j_ - inc
        end j_
      a[j_] = temp
      end i_
    inc = int Math.round(double inc / 2.2)
    end inc

  return a

method isTrue public constant binary returns boolean
  return 1 == 1

method isFalse public constant binary returns boolean
  return \isTrue

```

```txt

UK  London
US  New York
US  Boston
US  Washington
UK  Washington
US  Birmingham
UK  Birmingham
UK  Boston

UK  Birmingham
UK  Boston
UK  London
UK  Washington
US  Birmingham
US  Boston
US  New York
US  Washington

```



## Nim


```nim
proc shellSort[T](a: var openarray[T]) =
  var h = a.len
  while h > 0:
    h = h div 2
    for i in h .. < a.len:
      let k = a[i]
      var j = i
      while j >= h and k < a[j-h]:
        a[j] = a[j-h]
        j -= h
      a[j] = k

var a = @[4, 65, 2, -31, 0, 99, 2, 83, 782]
shellSort a
echo a
```



## Objeck

```objeck

bundle Default {
  class ShellSort {
    function : Main(args : String[]) ~ Nil {
      a := [1, 3, 7, 21, 48, 112,336, 861, 1968, 4592, 13776,33936, 86961, 198768, 463792, 1391376,3402672, 8382192, 21479367, 49095696, 114556624,343669872, 52913488, 2085837936];
      Shell(a);
      each(i : a) {
        IO.Console->Print(a[i])->Print(", ");
      };
      IO.Console->PrintLine();
    }

    function : native : Shell(a : Int[]) ~ Nil {
      increment := a->Size() / 2;
      while(increment > 0) {
        for(i := increment; i < a->Size(); i += 1;) {
          j := i;
          temp := a[i];
          while(j >= increment & a[j - increment] > temp) {
            a[j] := a[j - increment];
            j -= increment;
          };
          a[j] := temp;
        };

        if(increment = 2) {
          increment := 1;
        }
        else {
          increment *= (5.0 / 11);
        };
      };
    }
  }
}

```



## OCaml

```ocaml
let shellsort a =
  let len = Array.length a in
  let incSequence = [| 412771; 165103; 66041; 26417; 10567;
                       4231; 1693; 673; 269; 107; 43; 17; 7; 3; 1 |] in

  Array.iter (fun increment ->
    if (increment * 2) <= len then
      for i = increment to pred len do
        let temp = a.(i) in
        let rec loop j =
          if j < 0 || a.(j) <= temp then (j)
          else begin
            a.(j + increment) <- a.(j);
            loop (j - increment)
          end
        in
        let j = loop (i - increment) in
        a.(j + increment) <- temp;
      done;
  ) incSequence;
;;
```

and the main:

```ocaml
let () =
  let arraysize = 1000 in  (* or whatever *)
  Random.self_init();
  let intArray =
    Array.init arraysize (fun _ -> Random.int 4000)
  in
  shellsort intArray;
  Array.iter (Printf.printf " %d") intArray;
  print_newline();
;;
```



## ooRexx

```ooRexx
/* Rexx */
-- --- Main --------------------------------------------------------------------
call demo
return
exit

-- -----------------------------------------------------------------------------
--  Shell sort implementation
-- -----------------------------------------------------------------------------
::routine shellSort
  use arg ra

  n = ra~items()
  inc = format(n / 2.0,, 0) -- rounding
  loop label inc while inc > 0
    loop i_ = inc to n - 1
      temp = ra~get(i_)
      j_ = i_
      loop label j_ while j_ >= inc
        if \(ra~get(j_ - inc) > temp) then leave j_
        ra~set(j_, ra~get(j_ - inc))
        j_ = j_ - inc
        end j_
      ra~set(j_, temp)
      end i_
    inc = format(inc / 2.2,, 0) -- rounding
    end inc

  return ra

-- -----------------------------------------------------------------------------
-- Demonstrate the implementation
-- -----------------------------------------------------------------------------
::routine demo

placesList = .nlist~of( -
    "UK  London",     "US  New York",   "US  Boston",     "US  Washington" -
  , "UK  Washington", "US  Birmingham", "UK  Birmingham", "UK  Boston"     -
)

lists = .array~of( -
    placesList -
  , shellSort(placesList~copy()) -
  )

loop ln = 1 to lists~items()
  cl = lists[ln]
  loop ct = 0 to cl~items() - 1
    say right(ct + 1, 4)':' cl[ct]
    end ct
    say
  end ln
  return

-- -----------------------------------------------------------------------------
::routine isTrue
  return 1 == 1

-- -----------------------------------------------------------------------------
::routine isFalse
  return \isTrue()

-- -----------------------------------------------------------------------------
-- Helper class.  Map get and set methods for easier conversion from java.util.List
-- -----------------------------------------------------------------------------
::class NList mixinclass List public

-- Map get() to at()
::method get
  use arg ix
  return self~at(ix)

-- Map set() to put()
::method set
  use arg ix, item
  self~put(item, ix)
  return

```

```txt

   1: UK  London
   2: US  New York
   3: US  Boston
   4: US  Washington
   5: UK  Washington
   6: US  Birmingham
   7: UK  Birmingham
   8: UK  Boston

   1: UK  Birmingham
   2: UK  Boston
   3: UK  London
   4: UK  Washington
   5: US  Birmingham
   6: US  Boston
   7: US  New York
   8: US  Washington

```



## PARI/GP


```parigp
shellSort(v)={
  my(inc=#v\2);
  while(inc,
    for(i=inc+1,#v,
      my(t=v[i],j=i);
      while(j>inc && v[j-inc]>t,
        v[j]=v[j-=inc]
      );
      v[j]=t
    );
    inc \= 2.2
  );
  v
};
```



## Pascal



```pascal
Const
  MaxN = 100; { number of elements (my example is 100) }
Type
  TArray = Array [0..MaxN] of Integer;

Procedure ShellSort ( var A : TArray; N : Integer );
Var
  i, j, step, tmp : Integer;
Begin
  step:=N div 2;  // step:=step shr 1
  While step>0 Do Begin
    For i:=step to N Do Begin
      tmp:=A[i];
      j:=i;
      While (j>=step) and (A[j-step]>tmp) Do Begin
        A[j]:=A[j-step];
        dec(j,step);
      End;
      A[j]:=tmp;
    End;
    step:=step div 2;  // step:=step shr 1
  End;
End;

```



## Perl



```perl
sub shell_sort {
    my (@a, $h, $i, $j, $k) = @_;
    for ($h = @a; $h = int $h / 2;) {
        for $i ($h .. $#a) {
            $k = $a[$i];
            for ($j = $i; $j >= $h && $k < $a[$j - $h]; $j -= $h) {
                $a[$j] = $a[$j - $h];
            }
            $a[$j] = $k;
        }
    }
    @a;
}

my @a = map int rand 100, 1 .. $ARGV[0] || 10;
say "@a";
@a = shell_sort @a;
say "@a";

```



## Perl 6


```perl6
sub shell_sort ( @a is copy ) {
    loop ( my $gap = (@a/2).round; $gap > 0; $gap = ( $gap * 5 / 11 ).round ) {
        for $gap .. @a.end -> $i {
            my $temp = @a[$i];

            my $j;
            loop ( $j = $i; $j >= $gap; $j -= $gap ) {
                my $v = @a[$j - $gap];
                last if $v <= $temp;
                @a[$j] = $v;
            }

            @a[$j] = $temp;
        }
    }
    return @a;
}
my @data = 22, 7, 2, -5, 8, 4;
say 'input  = ' ~ @data;
say 'output = ' ~ @data.&shell_sort;

```


```txt

input  = 22 7 2 -5 8 4
output = -5 2 4 7 8 22

```



## Phix

Copy of [[Sorting_algorithms/Shell_sort#Euphoria|Euphoria]]

```Phix
function shell_sort(sequence s)
integer gap = floor(length(s)/2), j
object temp
    while gap>0 do
        for i=gap to length(s) do
            temp = s[i]
            j = i-gap
            while j>=1 and temp<=s[j] do
                s[j+gap] = s[j]
                j -= gap
            end while
            s[j+gap] = temp
        end for
        gap = floor(gap/2)
    end while
    return s
end function
```



## PHP


```php

function shellSort($arr)
{
	$inc = round(count($arr)/2);
	while($inc > 0)
	{
		for($i = $inc; $i < count($arr);$i++){
			$temp = $arr[$i];
			$j = $i;
			while($j >= $inc && $arr[$j-$inc] > $temp)
			{
				$arr[$j] = $arr[$j - $inc];
				$j -= $inc;
			}
			$arr[$j] = $temp;
		}
		$inc = round($inc/2.2);
	}
	return $arr;
}

```



## PL/I


```PL/I

/* Based on Rosetta Fortran */
Shell_Sort: PROCEDURE (A);
   DECLARE A(*) FIXED;
   DECLARE ( i, j, increment) FIXED BINARY (31);
   DECLARE temp FIXED;

   increment = DIMENSION(a) / 2;
   DO WHILE (increment > 0);
         DO i = lbound(A,1)+increment TO hbound(a,1);
            j = i;
            temp = a(i);
            DO WHILE (j >= increment+1 & a(j-increment) > temp);
               a(j) = a(j-increment);
               j = j - increment;
            END;
            a(j) = temp;
         END;
         IF increment = 2 THEN
            increment = 1;
         ELSE
            increment = increment * 5 / 11;
      END;
END SHELL_SORT;

```



## PicoLisp


```PicoLisp
(de shellSort (A)
   (for (Inc (*/ (length A) 2)  (gt0 Inc)  (*/ Inc 10 22))
      (for (I Inc  (get A I)  (inc I))
         (let (Tmp @  J I)
            (while (and (>= J Inc) (> (get A (- J Inc)) Tmp))
               (set (nth A J) (get A (- J Inc)))
               (dec 'J Inc) )
            (set (nth A J) Tmp) ) ) )
   A )
```

```txt
: (shellSort (make (do 9 (link (rand 1 999)))))
-> (1 167 183 282 524 556 638 891 902)

: (shellSort (make (do 9 (link (rand 1 999)))))
-> (82 120 160 168 205 226 408 708 719)

: (shellSort (make (do 9 (link (rand 1 999)))))
-> (108 212 330 471 667 716 739 769 938)
```



## PowerShell


```PowerShell
Function ShellSort( [Array] $data )
{
	# http://oeis.org/A108870
	$A108870 = [Int64[]] ( 1, 4, 9, 20, 46, 103, 233, 525, 1182, 2660, 5985, 13467, 30301, 68178, 153401, 345152, 776591, 1747331, 3931496, 8845866, 19903198, 44782196, 100759940, 226709866, 510097200, 1147718700, 2582367076, 5810325920, 13073233321, 29414774973 )
	$datal = $data.length - 1
	$inci = [Array]::BinarySearch( $A108870, [Int64] ( [Math]::Floor( $datal / 2 ) ) )
	if( $inci -lt 0 )
	{
		$inci = ( $inci -bxor -1 ) - 1
	}
	$A108870[ $inci..0 ] | ForEach-Object {
		$inc = $_
		$_..$datal | ForEach-Object {
			$temp = $data[ $_ ]
			$j = $_
			for( ; ( $j -ge $inc ) -and ( $data[ $j - $inc ] -gt $temp ); $j -= $inc )
			{
				$data[ $j ] = $data[ $j - $inc ]
			}
			$data[ $j ] = $temp
		}
	}
	$data
}

$l = 10000; ShellSort( ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( 0, $l - 1 ) } ) )
```



## PureBasic

```PureBasic
#STEP=2.2

Procedure Shell_sort(Array A(1))
  Protected l=ArraySize(A()), increment=Int(l/#STEP)
  Protected i, j, temp
  While increment
    For i= increment To l
      j=i
      temp=A(i)
      While j>=increment And A(j-increment)>temp
        A(j)=A(j-increment)
        j-increment
      Wend
      A(j)=temp
    Next i
    If increment=2
      increment=1
    Else
      increment*(5.0/11)
    EndIf
  Wend
EndProcedure
```



## Python

This method sorts in place.
If you want to preserve your unsorted list, copy it first.

```python


def shell(seq):
    inc = len(seq) // 2
    while inc:
        for i, el in enumerate(seq):
            while i >= inc and seq[i - inc] > el:
                seq[i] = seq[i - inc]
                i -= inc
            seq[i] = el
        inc = 1 if inc == 2 else int(inc * 5.0 / 11)

data = [22, 7, 2, -5, 8, 4]
shell(data)
print data # [-5, 2, 4, 7, 8, 22]
```



## Racket


```racket

#lang racket
(define (shell-sort! xs)
  (define ref (curry vector-ref xs))
  (define (new Δ) (if (= Δ 2) 1 (quotient (* Δ 5) 11)))
  (let loop ([Δ (quotient (vector-length xs) 2)])
    (unless (= Δ 0)
      (for ([xᵢ (in-vector xs)] [i (in-naturals)])
        (let while ([i i])
          (cond [(and (>= i Δ) (> (ref (- i Δ)) xᵢ))
                 (vector-set! xs i (ref (- i Δ)))
                 (while (- i Δ))]
                [else (vector-set! xs i xᵢ)])))
      (loop (new Δ))))
  xs)

```



## REXX

Historical data note:   the three-character abbreviations were (and probably still are) the

official three-character abbreviations for the states of the USA before the advent of ZIP codes.

'''ZIP''' = '''Z'''one '''I'''mprovement '''P'''lan.     Now-a-days, the USA uses two-character abbreviations.

```rexx
/*REXX program  sorts  a  stemmed array  using the  shell sort  (shellsort) algorithm.  */
call gen                                         /*generate the array elements.         */
call show           'before sort'                /*display the  before  array elements. */
say copies('▒', 75)                              /*displat a separator line  (a fence). */
call shellSort       #                           /*invoke the  shell  sort.             */
call show           ' after sort'                /*display the   after  array elements. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen:    @.=                                      /*assign a default value to stem array.*/
        @.1='3 character abbreviations for states of the USA'       /*predates ZIP code.*/
        @.2='
### =========================================
'
        @.3='RHO  Rhode Island and Providence Plantations'    ;  @.36='NMX  New Mexico'
        @.4='CAL  California'    ;   @.20='NEV  Nevada'       ;  @.37='IND  Indiana'
        @.5='KAN  Kansas'        ;   @.21='TEX  Texas'        ;  @.38='MOE  Missouri'
        @.6='MAS  Massachusetts' ;   @.22='VGI  Virginia'     ;  @.39='COL  Colorado'
        @.7='WAS  Washington'    ;   @.23='OHI  Ohio'         ;  @.40='CON  Connecticut'
        @.8='HAW  Hawaii'        ;   @.24='NHM  New Hampshire';  @.41='MON  Montana'
        @.9='NCR  North Carolina';   @.25='MAE  Maine'        ;  @.42='LOU  Louisiana'
       @.10='SCR  South Carolina';   @.26='MIC  Michigan'     ;  @.43='IOW  Iowa'
       @.11='IDA  Idaho'         ;   @.27='MIN  Minnesota'    ;  @.44='ORE  Oregon'
       @.12='NDK  North Dakota'  ;   @.28='MIS  Mississippi'  ;  @.45='ARK  Arkansas'
       @.13='SDK  South Dakota'  ;   @.29='WIS  Wisconsin'    ;  @.46='ARZ  Arizona'
       @.14='NEB  Nebraska'      ;   @.30='OKA  Oklahoma'     ;  @.47='UTH  Utah'
       @.15='DEL  Delaware'      ;   @.31='ALA  Alabama'      ;  @.48='KTY  Kentucky'
       @.16='PEN  Pennsylvania'  ;   @.32='FLA  Florida'      ;  @.49='WVG  West Virginia'
       @.17='TEN  Tennessee'     ;   @.33='MLD  Maryland'     ;  @.50='NWJ  New Jersey'
       @.18='GEO  Georgia'       ;   @.34='ALK  Alaska'       ;  @.51='NYK  New York'
       @.19='VER  Vermont'       ;   @.35='ILL  Illinois'     ;  @.52='WYO  Wyoming'
            do #=1  while @.#\==''; end;  #=#-1  /*determine number of entries in array.*/
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
shellSort: procedure expose @.;   parse arg N    /*obtain the  N  from the argument list*/
           i=N%2                                 /*%   is integer division in REXX.     */
                     do  while i\==0
                              do j=i+1  to N;    k=j;       p=k-i     /*P: previous item*/
                              _=@.j
                                    do  while k>=i+1 & @.p>_;    @.k=@.p;   k=k-i;   p=k-i
                                    end   /*while k≥i+1*/
                              @.k=_
                              end         /*j*/
                     if i==2  then i=1
                              else i=i * 5 % 11
                     end                  /*while i¬==0*/
           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:      do j=1  for #;  say 'element'  right(j,length(#)) arg(1)": "  @.j; end;  return
```

<pre style="height:85ex">
element  1 before sort:  3 character abbreviations for states of the USA
element  2 before sort:
### =========================================

element  3 before sort:  RHO  Rhode Island and Providence Plantations
element  4 before sort:  CAL  California
element  5 before sort:  KAN  Kansas
element  6 before sort:  MAS  Massachusetts
element  7 before sort:  WAS  Washington
element  8 before sort:  HAW  Hawaii
element  9 before sort:  NCR  North Carolina
element 10 before sort:  SCR  South Carolina
element 11 before sort:  IDA  Idaho
element 12 before sort:  NDK  North Dakota
element 13 before sort:  SDK  South Dakota
element 14 before sort:  NEB  Nebraska
element 15 before sort:  DEL  Delaware
element 16 before sort:  PEN  Pennsylvania
element 17 before sort:  TEN  Tennessee
element 18 before sort:  GEO  Georgia
element 19 before sort:  VER  Vermont
element 20 before sort:  NEV  Nevada
element 21 before sort:  TEX  Texas
element 22 before sort:  VGI  Virginia
element 23 before sort:  OHI  Ohio
element 24 before sort:  NHM  New Hampshire
element 25 before sort:  MAE  Maine
element 26 before sort:  MIC  Michigan
element 27 before sort:  MIN  Minnesota
element 28 before sort:  MIS  Mississippi
element 29 before sort:  WIS  Wisconsin
element 30 before sort:  OKA  Oklahoma
element 31 before sort:  ALA  Alabama
element 32 before sort:  FLA  Florida
element 33 before sort:  MLD  Maryland
element 34 before sort:  ALK  Alaska
element 35 before sort:  ILL  Illinois
element 36 before sort:  NMX  New Mexico
element 37 before sort:  IND  Indiana
element 38 before sort:  MOE  Missouri
element 39 before sort:  COL  Colorado
element 40 before sort:  CON  Connecticut
element 41 before sort:  MON  Montana
element 42 before sort:  LOU  Louisiana
element 43 before sort:  IOW  Iowa
element 44 before sort:  ORE  Oregon
element 45 before sort:  ARK  Arkansas
element 46 before sort:  ARZ  Arizona
element 47 before sort:  UTH  Utah
element 48 before sort:  KTY  Kentucky
element 49 before sort:  WVG  West Virginia
element 50 before sort:  NWJ  New Jersey
element 51 before sort:  NYK  New York
element 52 before sort:  WYO  Wyoming
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
element  1  after sort:  3 character abbreviations for states of the USA
element  2  after sort:
### =========================================

element  3  after sort:  ALA  Alabama
element  4  after sort:  ALK  Alaska
element  5  after sort:  ARK  Arkansas
element  6  after sort:  ARZ  Arizona
element  7  after sort:  CAL  California
element  8  after sort:  COL  Colorado
element  9  after sort:  CON  Connecticut
element 10  after sort:  DEL  Delaware
element 11  after sort:  FLA  Florida
element 12  after sort:  GEO  Georgia
element 13  after sort:  HAW  Hawaii
element 14  after sort:  IDA  Idaho
element 15  after sort:  ILL  Illinois
element 16  after sort:  IND  Indiana
element 17  after sort:  IOW  Iowa
element 18  after sort:  KAN  Kansas
element 19  after sort:  KTY  Kentucky
element 20  after sort:  LOU  Louisiana
element 21  after sort:  MAE  Maine
element 22  after sort:  MAS  Massachusetts
element 23  after sort:  MIC  Michigan
element 24  after sort:  MIN  Minnesota
element 25  after sort:  MIS  Mississippi
element 26  after sort:  MLD  Maryland
element 27  after sort:  MOE  Missouri
element 28  after sort:  MON  Montana
element 29  after sort:  NCR  North Carolina
element 30  after sort:  NDK  North Dakota
element 31  after sort:  NEB  Nebraska
element 32  after sort:  NEV  Nevada
element 33  after sort:  NHM  New Hampshire
element 34  after sort:  NMX  New Mexico
element 35  after sort:  NWJ  New Jersey
element 36  after sort:  NYK  New York
element 37  after sort:  OHI  Ohio
element 38  after sort:  OKA  Oklahoma
element 39  after sort:  ORE  Oregon
element 40  after sort:  PEN  Pennsylvania
element 41  after sort:  RHO  Rhode Island and Providence Plantations
element 42  after sort:  SCR  South Carolina
element 43  after sort:  SDK  South Dakota
element 44  after sort:  TEN  Tennessee
element 45  after sort:  TEX  Texas
element 46  after sort:  UTH  Utah
element 47  after sort:  VER  Vermont
element 48  after sort:  VGI  Virginia
element 49  after sort:  WAS  Washington
element 50  after sort:  WIS  Wisconsin
element 51  after sort:  WVG  West Virginia
element 52  after sort:  WYO  Wyoming
───────────────────────────────────────────────────────────────────────────────

```



## Ring


```ring

aList = [-12, 3, 0, 4, 7, 4, 8, -5, 9]
shellSort(aList)
for i=1 to len(aList)
    see "" + aList[i] + " "
next

func shellSort a
     inc = ceil( len(a) / 2 )
     while inc > 0
           for i = inc to len(a)
               tmp = a[i]
               j = i
               while j > inc and a[j-inc] > tmp
                     a[j] = a[j-inc]
                     j = j - inc
               end
               a[j] = tmp
           next
           inc = floor( 0.5 + inc / 2.2 )
     end
     return a

```



## Ruby

This method sorts in place. If you want to preserve your unsorted list, copy it first.

```ruby
class Array
  def shellsort!
    inc = length / 2
    while inc != 0
      inc.step(length-1) do |i|
        el = self[i]
        while i >= inc and self[i - inc] > el
          self[i] = self[i - inc]
          i -= inc
        end
        self[i] = el
      end
      inc = (inc == 2 ? 1 : (inc * 5.0 / 11).to_i)
    end
    self
  end
end

data = [22, 7, 2, -5, 8, 4]
data.shellsort!
p data # [-5, 2, 4, 7, 8, 22]
```



## Run BASIC


```runbasic
siz = 100
dim a(siz)
for i = 1 to siz
 a(i) = rnd(1) * 1000
next i

' -------------------------------
' Shell Sort
' -------------------------------
incr = int(siz / 2)
WHILE incr > 0
  for i = 1 to siz
    j    = i
    temp = a(i)
    WHILE (j >= incr and a(abs(j-incr)) > temp)
      a(j) = a(j-incr)
      j    = j - incr
    WEND
    a(j) = temp
  next i
  incr = int(incr / 2.2)
WEND
```



## Scala


```scala
object ShellSort {
  def incSeq(len:Int)=new Iterator[Int]{
    private[this] var x:Int=len/2
    def hasNext=x>0
    def next()={x=if (x==2) 1 else x*5/11; x}
  }

  def InsertionSort(a:Array[Int], inc:Int)={
    for (i <- inc until a.length; temp=a(i)){
      var j=i;
      while (j>=inc && a(j-inc)>temp){
        a(j)=a(j-inc)
        j=j-inc
      }
      a(j)=temp
    }
  }

  def shellSort(a:Array[Int])=for(inc<-incSeq(a.length)) InsertionSort(a, inc)

  def main(args: Array[String]): Unit = {
    var a=Array(2, 5, 3, 4, 3, 9, 3, 2, 5, 4, 1, 3, 22, 7, 2, -5, 8, 4)
    println(a.mkString(","))
    shellSort(a)
    println(a.mkString(","))
  }
}
```

```txt
2,5,3,4,3,9,3,2,5,4,1,3,22,7,2,-5,8,4
-5,1,2,2,2,3,3,3,3,4,4,4,5,5,7,8,9,22
```



## Seed7


```seed7
const proc: shellSort (inout array elemType: arr) is func
  local
    var integer: i is 0;
    var integer: j is 0;
    var integer: increment is 0;
    var elemType: help is elemType.value;
  begin
    increment := length(arr) div 2;
    while increment > 0 do
      for i range 1 to length(arr) do
        j := i;
        help := arr[i];
        while j > increment and arr[j - increment] > help do
          arr[j] := arr[j - increment];
          j -:= increment;
        end while;
        arr[j] := help;
      end for;
      increment := increment div 2;
    end while;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/sorting.htm#shellSort]


## Sidef

```ruby
func shell_sort(a) {
    var h = a.len;
    while (h >>= 1) {
        for i in (h .. a.end) {
            var k = a[i];
            for (var j = i; (j >= h) && (k < a[j - h]); j -= h) {
                a[j] = a[j - h];
            }
            a[j] = k;
        }
    }
    return a;
}

var a = 10.of {100.irand};
say a;
shell_sort(a);
say a;
```

```txt
[54, 67, 65, 8, 56, 83, 64, 42, 20, 17]
[8, 17, 20, 42, 54, 56, 64, 65, 67, 83]
```



## Swift

```swift>func shellsort<T where T : Comparable
(inout seq: [T]) {
    var inc = seq.count / 2
    while inc > 0 {
        for (var i, el) in EnumerateSequence(seq) {
            while i >= inc && seq[i - inc] > el {
                seq[i] = seq[i - inc]
                i -= inc
            }
            seq[i] = el
        }
        if inc == 2 {
            inc = 1
        } else {
            inc = inc * 5 / 11
        }
    }
}
```


```txt
var data = [22, 7, 2, -5, 8, 4]
```


```txt
shellsort(&data) // [-5, 2, 4, 7, 8, 22]
```



## Tcl


```tcl
package require Tcl 8.5

proc shellsort {m} {
    set len [llength $m]
    set inc [expr {$len / 2}]
    while {$inc > 0} {
        for {set i $inc} {$i < $len} {incr i} {
            set j $i
            set temp [lindex $m $i]
            while {$j >= $inc && [set val [lindex $m [expr {$j - $inc}]]] > $temp} {
                lset m $j $val
                incr j -$inc
            }
            lset m $j $temp
        }
        set inc [expr {$inc == 2 ? 1 : $inc * 5 / 11}]
    }
    return $m
}

puts [shellsort {8 6 4 2 1 3 5 7 9}] ;# => 1 2 3 4 5 6 7 8 9
```



## uBasic/4tH

<lang>PRINT "Shell sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Shellsort (n)
  PROC _ShowArray (n)
PRINT

END


_Shellsort PARAM (1)                   ' Shellsort subroutine
  LOCAL (4)
  b@ = a@

  DO WHILE b@
    b@ = b@ / 2
    FOR c@ = b@ TO a@ - 1
      e@ = @(c@)
      d@ = c@
      DO WHILE (d@ > b@-1) * (e@ < @(ABS(d@ - b@)))
        @(d@) = @(d@ - b@)
        d@ = d@ - b@
      LOOP
      @(d@) = e@
    NEXT
  LOOP
RETURN


_Swap PARAM(2)                         ' Swap two array elements
  PUSH @(a@)
  @(a@) = @(b@)
  @(b@) = POP()
RETURN


_InitArray                             ' Init example array
  PUSH 4, 65, 2, -31, 0, 99, 2, 83, 782, 1

  FOR i = 0 TO 9
    @(i) = POP()
  NEXT

RETURN (i)


_ShowArray PARAM (1)                   ' Show array subroutine
  FOR i = 0 TO a@-1
    PRINT @(i),
  NEXT

  PRINT
RETURN
```



## Whitespace


[[/Whitespace|Implementation in Whitespace]].


## XPL0


```XPL0

include c:\cxpl\codes;          \intrinsic 'code' declarations
string 0;                       \use zero-terminated strings


proc    SSort(A, N);            \Shell sort array in ascending order
char    A;                      \address of array
int     N;                      \number of elements in array (size)
int     I, J, Gap, JG, T;
[Gap:= N>>1;
while Gap > 0 do
        [for I:= Gap to N-1 do
                [J:= I - Gap;
                loop    [JG:= J + Gap;
                        if A(J) <= A(JG) then quit;
                        T:= A(J);   A(J):= A(JG);   A(JG):= T;  \swap elements
                        J:= J - Gap;
                        if J < 0 then quit;
                        ];
                ];
        Gap:= Gap>>1;
        ];
];      \SSort


func    StrLen(Str);            \Return number of characters in an ASCIIZ string
char    Str;
int     I;
for I:= 0 to -1>>1-1 do
        if Str(I) = 0 then return I;


char    Str;
[Str:= "Pack my box with five dozen liquor jugs.";
SSort(Str, StrLen(Str));
Text(0, Str);  CrLf(0);
]
```


```txt

"       .Pabcdeefghiiijklmnoooqrstuuvwxyz"

```



## Yabasic


```Yabasic
export sub shell_sort(x())
// Shell sort based on insertion sort

   local gap, i, j, first, last, tempi, tempj

   last = arraysize(x(),1)
   gap = int(last / 10) + 1
   while(TRUE)
	first = gap + 1
	for i = first to last
	   	tempi = x(i)
	    	j = i - gap
	    	while(TRUE)
			tempj = x(j)
			if tempi >= tempj then
		    		j = j + gap
		    		break
			end if
			x(j+gap) = tempj
			if j <= gap then
		    		break
			end if
			j = j - gap
	    	wend
	    	x(j) = tempi
	next i
	if gap = 1 then
	   	return
	else
	   	gap = int(gap / 3.5) + 1
	end if
   wend
end sub

if peek$("library") = "main" then

	clear screen

	ITEMS = 100
	dim numeros(ITEMS)

	for n = 1 to ITEMS
		numeros(n) = ran(ITEMS + 1)
	next n

	print time$
	shell_sort(numeros())
	print time$
	print "Press a key to see ordered numbers."
	inkey$

	for n = 1 to ITEMS
		print numeros(n),", ";
	next n

end if
```



## zkl


```zkl
    // Shell sort a sequence of objects in place
    // Requires mutiable list
fcn shellSort(sequence){
   n := sequence.len();
   gap := n / 2;
   while (0 < gap){
      i := gap;
      while (i < n){
	 j := i - gap;
	 while ((j >= 0) and (sequence[j] > sequence[j + gap])){
	    sequence.swap(j,j + gap);
	    j -= gap;
	 }
	 i += 1;
      }
      gap /= 2;
   }
   return(sequence);
}
```


