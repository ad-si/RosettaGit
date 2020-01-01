+++
title = "Sorting algorithms/Insertion sort"
description = ""
date = 2019-10-18T20:12:40Z
aliases = []
[extra]
id = 2219
[taxonomies]
categories = []
tags = []
+++

{{task|Sorting Algorithms}}
{{Sorting Algorithm}}
{{wikipedia|Insertion sort}}
{{omit from|GUISS}}



An `[[O]](''n''<sup>2</sup>)` sorting algorithm which moves elements one at a time into the correct position.
The algorithm consists of inserting one element at a time into the previously sorted part of the array, moving higher ranked elements up as necessary.
To start off, the first (or smallest, or any arbitrary) element of the unsorted array is considered to be the sorted part.

Although insertion sort is an `[[O]](''n''<sup>2</sup>)` algorithm, its simplicity, low overhead, good locality of reference and efficiency make it a good choice in two cases:

(i) small `''n''`,

(ii) as the final finishing-off algorithm for `[[O]](''n'' log''n'')` algorithms such as [[Merge sort|mergesort]] and [[quicksort]].

The algorithm is as follows (from [[wp:Insertion_sort#Algorithm|wikipedia]]):
 '''function''' ''insertionSort''(array A)
     '''for''' i '''from''' 1 '''to''' length[A]-1 '''do'''
         value := A[i]
         j := i-1
         '''while''' j >= 0 '''and''' A[j] > value '''do'''
             A[j+1] := A[j]
             j := j-1
         '''done'''
         A[j+1] = value
     '''done'''

Writing the algorithm for integers will suffice.





## 360 Assembly

{{trans|PL/I}}
These programs use two ASSIST macros (XDECO, XPRNT) to keep the code as short as possible.

### Basic


```360asm
*        Insertion sort            16/06/2016
INSSORT  CSECT
         USING  INSSORT,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         LA     R6,2               i=2
         LA     R9,A+L'A           @a(2)
LOOPI    C      R6,N               do i=2 to n
         BH     ELOOPI             leave i
         L      R2,0(R9)           a(i)
         ST     R2,V               v=a(i)
         LR     R7,R6              j=i
         BCTR   R7,0               j=i-1
         LR     R8,R9              @a(i)
         S      R8,=A(L'A)         @a(j)
LOOPJ    LTR    R7,R7              do j=i-1 to 1 by -1 while j>0
         BNH    ELOOPJ             leave j
         L      R2,0(R8)           a(j)
         C      R2,V               a(j)>v
         BNH    ELOOPJ             leave j
         MVC    L'A(L'A,R8),0(R8)  a(j+1)=a(j)
         BCTR   R7,0               j=j-1
         S      R8,=A(L'A)         @a(j)
         B      LOOPJ              next j
ELOOPJ   MVC    L'A(L'A,R8),V      a(j+1)=v;
         LA     R6,1(R6)           i=i+1
         LA     R9,L'A(R9)         @a(i)
         B      LOOPI              next i
ELOOPI   LA     R9,PG              pgi=0
         LA     R6,1               i=1
         LA     R8,A               @a(1)
LOOPXI   C      R6,N               do i=1 to n
         BH     ELOOPXI            leave i
         L      R1,0(R8)           a(i)
         XDECO  R1,XDEC            edit a(i)
         MVC    0(4,R9),XDEC+8     output a(i)
         LA     R9,4(R9)           pgi=pgi+1
         LA     R6,1(R6)           i=i+1
         LA     R8,L'A(R8)         @a(i)
         B      LOOPXI             next i
ELOOPXI  XPRNT  PG,L'PG            print buffer
         L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
A  DC F'4',F'65',F'2',F'-31',F'0',F'99',F'2',F'83',F'782',F'1'
   DC F'45',F'82',F'69',F'82',F'104',F'58',F'88',F'112',F'89',F'74'
V        DS     F                  variable
N        DC     A((V-A)/L'A)       n=hbound(a)
PG       DC     CL80' '            buffer
XDEC     DS     CL12               for xdeco
         YREGS                     symbolics for registers
         END    INSSORT
```

{{out}}

```txt

 -31   0   1   2   2   4  45  58  65  69  74  82  82  83  88  89  99 104 112 782

```



### Assembler Structured Macros

No harmful gotos [:)Dijkstra], no labels. It's cleaner, but is it clearer?

```360asm
*        Insertion sort        16/06/2016
INSSORTS CSECT
     USING  INSSORTS,R13       base register
     B      72(R15)            skip savearea
     DC     17F'0'             savearea
     STM    R14,R12,12(R13)    prolog
     ST     R13,4(R15)         "
     ST     R15,8(R13)         "
     LR     R13,R15            "
     LA     R6,2               i=2
     LA     R9,A+L'A           @a(2)
     DO     WHILE=(C,R6,LE,N)  do while i<=n
       L      R2,0(R9)           a(i)
       ST     R2,V               v=a(i)
       LR     R7,R6              j=i
       BCTR   R7,0               j=i-1
       LR     R8,R9              @a(i)
       S      R8,=A(L'A)         @a(j)
       L      R2,0(R8)           a(j)
       DO     WHILE=(C,R7,GT,0,AND,C,R2,GT,V)  do while j>0 & a(j)>v
         MVC    L'A(L'A,R8),0(R8)  a(j+1)=a(j)
         BCTR   R7,0               j=j-1
         S      R8,=A(L'A)         @a(j)
         L      R2,0(R8)           a(j)
       ENDDO  ,                  next j
       MVC    L'A(L'A,R8),V      a(j+1)=v;
       LA     R6,1(R6)           i=i+1
       LA     R9,L'A(R9)         @a(i)
     ENDDO  ,                  next i
     LA     R9,PG              pgi=0
     LA     R6,1               i=1
     LA     R8,A               @a(1)
     DO     WHILE=(C,R6,LE,N)  do while i<=n
       L      R1,0(R8)           a(i)
       XDECO  R1,XDEC            edit a(i)
       MVC    0(4,R9),XDEC+8     output a(i)
       LA     R9,4(R9)           pgi=pgi+1
       LA     R6,1(R6)           i=i+1
       LA     R8,L'A(R8)         @a(i)
     ENDDO  ,                  next i
     XPRNT  PG,L'PG            print buffer
     L      R13,4(0,R13)       epilog
     LM     R14,R12,12(R13)    "
     XR     R15,R15            "
     BR     R14                exit
A  DC F'4',F'65',F'2',F'-31',F'0',F'99',F'2',F'83',F'782',F'1'
   DC F'45',F'82',F'69',F'82',F'104',F'58',F'88',F'112',F'89',F'74'
V    DS     F                  variable
N    DC     A((V-A)/L'A)       n=hbound(a)
PG   DC     CL80' '            buffer
XDEC DS     CL12               for xdeco
     YREGS                     symbolics for registers
     END    INSSORTS
```

{{out}}
Same as previous


## ACL2


```Lisp
(defun insert (x xs)
   (cond ((endp xs) (list x))
         ((< x (first xs))
          (cons x xs))
         (t (cons (first xs)
                  (insert x (rest xs))))))

(defun isort (xs)
   (if (endp xs)
       nil
       (insert (first xs)
               (isort (rest xs)))))
```



## ActionScript


```ActionScript
function insertionSort(array:Array)
{
	for(var i:int = 1; i < array.length;i++)
	{
		var value = array[i];
		var j:int = i-1;
		while(j >= 0 && array[j] > value)
		{
			array[j+1] = array[j];
			j--;
		}
		array[j+1] = value;
	}
	return array;
}
```



## Ada


```ada
type Data_Array is array(Natural range <>) of Integer;

procedure Insertion_Sort(Item : in out Data_Array) is
   First : Natural := Item'First;
   Last  : Natural := Item'Last;
   Value : Integer;
   J     : Integer;
begin
   for I in (First + 1)..Last loop
      Value := Item(I);
      J := I - 1;
      while J in Item'range and then Item(J) > Value loop
         Item(J + 1) := Item(J);
         J := J - 1;
      end loop;
      Item(J + 1) := Value;
   end loop;
end Insertion_Sort;
```



## ALGOL 68

{{trans|Ada}}

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
MODE DATA = REF CHAR;

PROC in place insertion sort = (REF[]DATA item)VOID:
BEGIN
   INT first := LWB item;
   INT last  := UPB item;
   INT j;
   DATA value;
   FOR i FROM first + 1 TO last DO
      value := item[i];
      j := i - 1;
   #  WHILE j >= LWB item AND j <= UPB item ANDF item[j] > value DO // example of ANDF extension #
      WHILE ( j >= LWB item AND j <= UPB item | item[j]>value | FALSE ) DO # no extension! #
         item[j + 1] := item[j];
         j -:=  1
      OD;
      item[j + 1] := value
   OD
END # in place insertion sort #;

[32]CHAR data := "big fjords vex quick waltz nymph";
[UPB data]DATA ref data;  FOR i TO UPB data DO ref data[i] := data[i] OD;
in place insertion sort(ref data);
FOR i TO UPB ref data DO print(ref data[i]) OD; print(new line);
print((data))
```

{{out}}

```txt

abcdefghiijklmnopqrstuvwxyz
big fjords vex quick waltz nymph

```



## ALGOL W

External in-place insertion sort routine for integers. From the pseudo code but with variable bounds.

```algolw
% insertion sorts in-place the array A. As Algol W procedures can't find the bounds %
% of an array parameter, the lower and upper bounds must be specified in lb and ub  %
procedure insertionSortI ( integer array A ( * ); integer value lb, ub ) ;
    for i := lb + 1 until ub do begin
        integer v, j;
        v := A( i );
        j := i - 1;
        while j >= lb and A( j ) > v do begin
            A( j + 1 ) := A( j );
            j := j - 1
        end while_j_ge_0_and_Aj_gt_v ;
        A( j + 1 ) := v
    end insertionSortI ;
```

Test the insertionSortI procedure.

```algolw
begin
    % external in-place insertion sort procedure %
    procedure insertionSortI ( integer array A( * ); integer value lb, ub ) ;
        algol "ISORTI" ;

    integer array d ( 1 :: 8 );
    integer p;
    p := 1;
    for i := 34, 2, -1, 0, 0, 9, -56, 3 do begin
        d( p ) := i;
        p := p + 1
    end for_i ;
    insertionSortI( d, 1, 8 );
    write( i_w := 1, d( 1 ) );
    for i := 2 until 8 do writeon( i_w := 1, d( i ) )
end.
```

{{out}}

```txt

-56  -1  0  0  2  3  9  34

```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program insertionSort.s   */
/* look Pseudocode begin this task  */

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
szCarriageReturn:  .asciz "\n"

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
main:                                              @ entry of program

1:
    ldr r0,iAdrTableNumber                         @ address number table
    mov r1,#0
    mov r2,#NBELEMENTS                             @ number of élements
    bl insertionSort
    ldr r0,iAdrTableNumber                         @ address number table
    bl displayTable

    ldr r0,iAdrTableNumber                         @ address number table
    mov r1,#NBELEMENTS                             @ number of élements
    bl isSorted                                    @ control sort
    cmp r0,#1                                      @ sorted ?
    beq 2f
    ldr r0,iAdrszMessSortNok                       @ no !! error sort
    bl affichageMess
    b 100f
2:                                                 @ yes
    ldr r0,iAdrszMessSortOk
    bl affichageMess
100:                                               @ standard end of the program
    mov r0, #0                                     @ return code
    mov r7, #EXIT                                  @ request to exit program
    svc #0                                         @ perform the system call

iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:    .int szCarriageReturn
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
/******************************************************************/
/*         insertion sort                                              */
/******************************************************************/
/* r0 contains the address of table */
/* r1 contains the first element    */
/* r2 contains the number of element */
insertionSort:
    push {r2,r3,r4,lr}                                     @ save registers
    add r3,r1,#1                                           @ start index i
1:                                                         @ start loop
    ldr r4,[r0,r3,lsl #2]                                  @ load value A[i]
    sub r5,r3,#1                                           @ index j
2:
    ldr r6,[r0,r5,lsl #2]                                  @ load value A[j]
    cmp r6,r4                                              @ compare value
    ble 3f
    add r5,#1                                              @ increment index j
    str r6,[r0,r5,lsl #2]                                  @ store value A[j+1]
    sub r5,#2                                              @ j = j - 1
    cmp r5,r1
    bge 2b                                                 @ loop if j >= first item
3:
    add r5,#1                                              @ increment index j
    str r4,[r0,r5,lsl #2]                                  @ store value A[i] in A[j+1]
    add r3,#1                                              @ increment index i
    cmp r3,r2                                              @ end ?
    blt 1b                                                 @ no -> loop

100:
    pop {r2,r3,r4,lr}
    bx lr                                                  @ return

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

1:	                                            @ start loop
    bl divisionpar10U                               @ unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                                      @ digit
    strb r1,[r3,r2]                                 @ store digit on area
    cmp r0,#0                                       @ stop if quotient = 0
    subne r2,#1                                     @ else previous position
    bne 1b	                                    @ and loop
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

contributed by Laszlo on the ahk [http://www.autohotkey.com/forum/post-276481.html#276481 forum]

```AutoHotkey
MsgBox % InsertionSort("")
MsgBox % InsertionSort("xxx")
MsgBox % InsertionSort("3,2,1")
MsgBox % InsertionSort("dog,000000,xx,cat,pile,abcde,1,cat,zz,xx,z")

InsertionSort(var) {                     ; SORT COMMA SEPARATED LIST
   StringSplit a, var, `,                ; make array, size = a0
   Loop % a0-1 {
      i := A_Index+1, v := a%i%, j := i-1
      While j>0 and a%j%>v
         u := j+1, a%u% := a%j%, j--
      u := j+1, a%u% := v
   }
   Loop % a0                             ; construct string from sorted array
      sorted .= "," . a%A_Index%
   Return SubStr(sorted,2)               ; drop leading comma
}
```



## AWK

Sort standard input (storing lines into an array) and output to standard output

```awk
{
  line[NR] = $0
}
END { # sort it with insertion sort
  for(i=1; i <= NR; i++) {
    value = line[i]
    j = i - 1
    while( ( j > 0) && ( line[j] > value ) ) {
      line[j+1] = line[j]
      j--
    }
    line[j+1] = value
  }
  #print it
  for(i=1; i <= NR; i++) {
    print line[i]
  }
}
```



## BASIC

{{trans|REALbasic}}
{{works with|QBasic}}

This version should work on any BASIC that can accept arrays as function arguments.

```qbasic
DECLARE SUB InsertionSort (theList() AS INTEGER)

DIM n(10) AS INTEGER, L AS INTEGER, o AS STRING
FOR L = 0 TO 10
    n(L) = INT(RND * 32768)
NEXT
InsertionSort n()
FOR L = 1 TO 10
    PRINT n(L); ";";
NEXT

SUB InsertionSort (theList() AS INTEGER)
    DIM insertionElementIndex AS INTEGER
    FOR insertionElementIndex = 1 TO UBOUND(theList)
        DIM insertionElement AS INTEGER
        insertionElement = theList(insertionElementIndex)
        DIM j AS INTEGER
        j = insertionElementIndex - 1
        DO WHILE (j >= 0)
            'necessary for BASICs without short-circuit evaluation
            IF (insertionElement < theList(j)) THEN
                theList(j + 1) = theList(j)
                j = j - 1
            ELSE
                EXIT DO
            END IF
        LOOP
        theList(j + 1) = insertionElement
    NEXT
END SUB
```


{{out}}

```txt

 1486 ; 9488 ; 9894 ; 17479 ; 18989 ; 23119 ; 23233 ; 24927 ; 25386 ; 26689 ;

```


=
## BBC BASIC
=
Note that the array index is assumed to start at zero.

```bbcbasic
      DIM test(9)
      test() = 4, 65, 2, -31, 0, 99, 2, 83, 782, 1
      PROCinsertionsort(test(), 10)
      FOR i% = 0 TO 9
        PRINT test(i%) ;
      NEXT
      PRINT
      END

      DEF PROCinsertionsort(a(), n%)
      LOCAL i%, j%, t
      FOR i% = 1 TO n%-1
        t = a(i%)
        j% = i%
        WHILE j%>0 AND t<a(ABS(j%-1))
          a(j%) = a(j%-1)
          j% -= 1
        ENDWHILE
        a(j%) = t
      NEXT
      ENDPROC
```

{{out}}

```txt

       -31         0         1         2         2         4        65        83        99       782

```


=
## Commodore BASIC
=

```basic

10 DIM A(10): N=9
11 REM GENERATE SOME RANDOM NUMBERS AND PRINT THEM
12 FOR I=0 TO N: A(I)=INT(RND(1)*10)+1: NEXT: GOSUB 50
20 FOR J=1 TO N:KEY=A(J): I=J-1: GOSUB 30: A(I+1)=KEY: NEXT: GOSUB 50: END
30 IFI=-1 THEN RETURN
31 IFA(I)>KEY THEN A(I+1)=A(I):I=I-1: GOTO 30
32 RETURN
50 PRINT: FOR I=0 TO N: PRINTA(I): NEXT: RETURN

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>  100 PROGRAM "InserSrt.bas"
110 RANDOMIZE
120 NUMERIC ARRAY(5 TO 21)
130 CALL INIT(ARRAY)
140 CALL WRITE(ARRAY)
150 CALL INSERTSORT(ARRAY)
160 CALL WRITE(ARRAY)
170 DEF INIT(REF A)
180   FOR I=LBOUND(A) TO UBOUND(A)
190     LET A(I)=RND(98)+1
200   NEXT
210 END DEF
220 DEF WRITE(REF A)
230   FOR I=LBOUND(A) TO UBOUND(A)
240     PRINT A(I);
250   NEXT
260   PRINT
270 END DEF
280 DEF INSERTSORT(REF A)
290   FOR J=LBOUND(A)+1 TO UBOUND(A)
300     LET I=J-1:LET SW=A(J)
310     DO WHILE I>=LBOUND(A) AND SW<A(I)
320       LET A(I+1)=A(I):LET I=I-1
330     LOOP
340     LET A(I+1)=SW
350   NEXT
360 END DEF
```



## C


```c
#include <stdio.h>

void insertion_sort(int *a, int n) {
	for(size_t i = 1; i < n; ++i) {
		int tmp = a[i];
		size_t j = i;
		while(j > 0 && tmp < a[j - 1]) {
			a[j] = a[j - 1];
			--j;
		}
		a[j] = tmp;
	}
}

int main () {
    int a[] = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1};
    int n = sizeof a / sizeof a[0];
    int i;
    for (i = 0; i < n; i++)
        printf("%d%s", a[i], i == n - 1 ? "\n" : " ");
    insertion_sort(a, n);
    for (i = 0; i < n; i++)
        printf("%d%s", a[i], i == n - 1 ? "\n" : " ");
    return 0;
}

```

{{out}}

```txt

4 65 2 -31 0 99 2 83 782 1
-31 0 1 2 2 4 65 83 99 782

```



## C++

Uses C++11. Compile with
 g++ -std=c++11 insertion.cpp
Uses binary search via std::upper_bound() to find the insertion position in logarithmic time and then performs the insertion via std::rotate() in linear time.

```cpp
#include <algorithm>
#include <iostream>
#include <iterator>

template <typename RandomAccessIterator, typename Predicate>
void insertion_sort(RandomAccessIterator begin, RandomAccessIterator end,
                    Predicate p) {
  for (auto i = begin; i != end; ++i) {
    std::rotate(std::upper_bound(begin, i, *i, p), i, i + 1);
  }
}

template <typename RandomAccessIterator>
void insertion_sort(RandomAccessIterator begin, RandomAccessIterator end) {
  insertion_sort(
      begin, end,
      std::less<
          typename std::iterator_traits<RandomAccessIterator>::value_type>());
}

int main() {
  int a[] = { 100, 2, 56, 200, -52, 3, 99, 33, 177, -199 };
  insertion_sort(std::begin(a), std::end(a));
  copy(std::begin(a), std::end(a), std::ostream_iterator<int>(std::cout, " "));
  std::cout << "\n";
}
```

{{out}}

```txt

-199 -52 2 3 33 56 99 100 177 200

```


## C#

```c#
namespace Sort {
  using System;

  static class InsertionSort<T> where T : IComparable {
    public static void Sort(T[] entries) {
      Sort(entries, 0, entries.Length - 1);
    }

    public static void Sort(T[] entries, Int32 first, Int32 last) {
      for (var i = first + 1; i <= last; i++) {
        var entry = entries[i];
        var j = i;

        while (j > first && entries[j - 1].CompareTo(entry) > 0)
          entries[j] = entries[--j];

        entries[j] = entry;
      }
    }
  }
}
```

'''Example''':

```c#
  using Sort;
  using System;

  class Program {
    static void Main(String[] args) {
      var entries = new Int32[] { 3, 9, 4, 6, 8, 1, 7, 2, 5 };
      InsertionSort<Int32>.Sort(entries);
      Console.WriteLine(String.Join(" ", entries));
    }
  }
```



## Clojure


```clojure

(defn insertion-sort [coll]
   (reduce (fn [result input]
             (let [[less more] (split-with #(< % input) result)]
               (concat less [input] more)))
           []
           coll))

```


Translated from the Haskell example:

```clojure

(defn in-sort! [data]
  (letfn [(insert ([raw x](insert [] raw x))
		  ([sorted [y & raw] x]
		     (if (nil? y) (conj sorted x)
			 (if (<= x y ) (concat sorted [x,y] raw)
			     (recur (conj sorted y)  raw x )))))]
    (reduce insert [] data)))
;Usage:(in-sort! [6,8,5,9,3,2,1,4,7])
;Returns: [1 2 3 4 5 6 7 8 9]
```



## CMake


```cmake
# insertion_sort(var [value1 value2...]) sorts a list of integers.
function(insertion_sort var)
  math(EXPR last "${ARGC} - 1")         # Sort ARGV[1..last].
  foreach(i RANGE 1 ${last})
    # Extend the sorted area to ARGV[1..i].
    set(b ${i})
    set(v ${ARGV${b}})
    # Insert v == ARGV[b] in sorted order. While b > 1, check if b is
    # too high, then decrement b. After loop, set ARGV[b] = v.
    while(b GREATER 1)
      math(EXPR a "${b} - 1")
      set(u ${ARGV${a}})
      # Now u == ARGV[a]. Pretend v == ARGV[b]. Compare.
      if(u GREATER ${v})
        # ARGV[a] and ARGV[b] are in wrong order. Fix by moving ARGV[a]
        # to ARGV[b], making room for later insertion of v.
        set(ARGV${b} ${u})
      else()
        break()
      endif()
      math(EXPR b "${b} - 1")
    endwhile()
    set(ARGV${b} ${v})
  endforeach(i)

  set(answer)
  foreach(i RANGE 1 ${last})
    list(APPEND answer ${ARGV${i}})
  endforeach(i)
  set("${var}" "${answer}" PARENT_SCOPE)
endfunction(insertion_sort)
```



```cmake
insertion_sort(result 33 11 44 22 66 55)
message(STATUS "${result}") # -- 11;22;33;44;55;66
```



## COBOL

This exerpt contains just enough of the procedure division to show the sort itself. The appropriate data division entries can be inferred. See also the entry for the Bubble sort for a full program.

```COBOL
       C-PROCESS SECTION.
           PERFORM E-INSERTION VARYING WB-IX-1 FROM 1 BY 1
                               UNTIL WB-IX-1 > WC-SIZE.

...

       E-INSERTION SECTION.
       E-000.
           MOVE WB-ENTRY(WB-IX-1) TO WC-TEMP.
           SET WB-IX-2 TO WB-IX-1.

           PERFORM F-PASS UNTIL WB-IX-2 NOT > 1 OR
                                WC-TEMP NOT < WB-ENTRY(WB-IX-2 - 1).

           IF WB-IX-1 NOT = WB-IX-2
              MOVE WC-TEMP TO WB-ENTRY(WB-IX-2).

       E-999.
           EXIT.

       F-PASS SECTION.
       F-000.
           MOVE WB-ENTRY(WB-IX-2 - 1) TO WB-ENTRY(WB-IX-2).
           SET WB-IX-2                DOWN BY 1.

       F-999.
           EXIT.
```


And a fully runnable version, by Steve Williams
{{works with|GnuCOBOL}}

```COBOL

        >>SOURCE FORMAT FREE
*> This code is dedicated to the public domain
*> This is GNUCOBOL 2.0
identification division.
program-id. insertionsort.
environment division.
configuration section.
repository. function all intrinsic.
data division.
working-storage section.
01  filler.
    03  a pic 99.
    03  a-lim pic 99 value 10.
    03  array occurs 10 pic 99.

01  filler.
    03  s pic 99.
    03  o pic 99.
    03  o1 pic 99.
    03  sorted-len pic 99.
    03  sorted-lim pic 99 value 10.
    03  sorted-array occurs 10 pic 99.

procedure division.
start-insertionsort.

    *> fill the array
    compute a = random(seconds-past-midnight)
    perform varying a from 1 by 1 until a > a-lim
        compute array(a) = random() * 100
    end-perform

    *> display the array
    perform varying a from 1 by 1 until a > a-lim
        display space array(a) with no advancing
    end-perform
    display  space 'initial array'

    *> sort the array
    move 0 to sorted-len
    perform varying a from 1 by 1 until a > a-lim
        *> find the insertion point
        perform varying s from 1 by 1
        until s > sorted-len
        or array(a) <= sorted-array(s)
            continue
        end-perform

        *>open the insertion point
        perform varying o from sorted-len by -1
        until o < s
            compute o1 = o + 1
            move sorted-array(o) to sorted-array(o1)
        end-perform

        *> move the array-entry to the insertion point
        move array(a) to sorted-array(s)

        add 1 to sorted-len
    end-perform

    *> display the sorted array
    perform varying s from 1 by 1 until s > sorted-lim
        display space sorted-array(s) with no advancing
    end-perform
    display space 'sorted array'

    stop run
    .
end program insertionsort.
```


{{out}}

```txt

prompt$ cobc -xj insertionsort.cob
 89 04 86 32 65 62 83 75 24 69 initial array
 04 24 32 62 65 69 75 83 86 89 sorted array
```



## Common Lisp


```lisp
(defun span (predicate list)
  (let ((tail (member-if-not predicate list)))
    (values (ldiff list tail) tail)))

(defun less-than (x)
  (lambda (y) (< y x)))

(defun insert (list elt)
  (multiple-value-bind (left right) (span (less-than elt) list)
    (append left (list elt) right)))

(defun insertion-sort (list)
  (reduce #'insert list :initial-value nil))
```



```lisp
(defun insertion-sort (sequence &optional (predicate #'<))
  (if (cdr sequence)
      (insert (car sequence)                 ;; insert the current item into
              (insertion-sort (cdr sequence) ;; the already-sorted
                              predicate)     ;; remainder of the list
              predicate)
      sequence)) ; a list of one element is already sorted

(defun insert (item sequence predicate)
  (cond ((null sequence) (list item))
        ((funcall (complement predicate)      ;; if the first element of the list
                              (car sequence)  ;; isn't better than the item,
                              item)           ;; cons the item onto
         (cons item sequence))                ;; the front of the list
        (t (cons (car sequence) ;; otherwise cons the first element onto the front of
                 (insert item   ;; the list of the item sorted with the rest of the list
                         (cdr sequence)
                         predicate)))))
```



## D


```d
void insertionSort(T)(T[] data) pure nothrow @safe @nogc {
    foreach (immutable i, value; data[1 .. $]) {
        auto j = i + 1;
        for ( ; j > 0 && value < data[j - 1]; j--)
            data[j] = data[j - 1];
        data[j] = value;
    }
}

void main() {
    import std.stdio;
    auto items = [28, 44, 46, 24, 19, 2, 17, 11, 25, 4];
    items.insertionSort;
    items.writeln;
}
```

{{out}}

```txt
[2, 4, 11, 17, 19, 24, 25, 28, 44, 46]
```



### Higher Level Version

{{trans|C++}}

```d
import std.stdio, std.range, std.algorithm, std.traits;

void insertionSort(R)(R arr)
if (hasLength!R && isRandomAccessRange!R && hasSlicing!R) {
    foreach (immutable i; 1 .. arr.length)
        bringToFront(arr[0 .. i].assumeSorted.upperBound(arr[i]), arr[i .. i + 1]);
}

void main() {
    import std.random, std.container;

    auto arr1 = [28, 44, 46, 24, 19, 2, 17, 11, 25, 4];
    arr1.insertionSort;
    assert(arr1.isSorted);
    writeln("arr1 sorted: ", arr1);

    auto arr2 = Array!int([28, 44, 46, 24, 19, 2, 17, 11, 25, 4]);
    arr2[].insertionSort;
    assert(arr2[].isSorted);
    writeln("arr2 sorted: ", arr2[]);

    // Random data test.
    int[10] buf;
    foreach (immutable _; 0 .. 100_000) {
        auto arr3 = buf[0 .. uniform(0, $)];
        foreach (ref x; arr3)
            x = uniform(-6, 6);
        arr3.insertionSort;
        assert(arr3.isSorted);
    }
}
```

{{out}}

```txt
arr1 sorted: [2, 4, 11, 17, 19, 24, 25, 28, 44, 46]
arr2 sorted: [2, 4, 11, 17, 19, 24, 25, 28, 44, 46]
```



## Delphi



### Array sort

Dynamic array is a 0-based array of variable length

Static array is an arbitrary-based array of fixed length

```Delphi
program TestInsertionSort;

{$APPTYPE CONSOLE}

{.$DEFINE DYNARRAY}  // remove '.' to compile with dynamic array

type
  TItem = Integer;   // declare ordinal type for array item
{$IFDEF DYNARRAY}
  TArray = array of TItem;          // dynamic array
{$ELSE}
  TArray = array[0..15] of TItem;   // static array
{$ENDIF}

procedure InsertionSort(var A: TArray);
var
  I, J: Integer;
  Item: TItem;

begin
  for I:= 1 + Low(A) to High(A) do begin
    Item:= A[I];
    J:= I - 1;
    while (J >= Low(A)) and (A[J] > Item) do begin
      A[J + 1]:= A[J];
      Dec(J);
    end;
    A[J + 1]:= Item;
  end;
end;

var
  A: TArray;
  I: Integer;

begin
{$IFDEF DYNARRAY}
  SetLength(A, 16);
{$ENDIF}
  for I:= Low(A) to High(A) do
    A[I]:= Random(100);
  for I:= Low(A) to High(A) do
    Write(A[I]:3);
  Writeln;
  InsertionSort(A);
  for I:= Low(A) to High(A) do
    Write(A[I]:3);
  Writeln;
  Readln;
end.
```

{{out}}

```txt

  0  3 86 20 27 67 31 16 37 42  8 47  7 84  5 29
  0  3  5  7  8 16 20 27 29 31 37 42 47 67 84 86

```



### String sort

// string is 1-based variable-length array of Char

```Delphi
procedure InsertionSort(var S: string);
var
  I, J, L: Integer;
  Ch: Char;

begin
  L:= Length(S);
  for I:= 2 to L do begin
    Ch:= S[I];
    J:= I - 1;
    while (J > 0) and (S[J] > Ch) do begin
      S[J + 1]:= S[J];
      Dec(J);
    end;
    S[J + 1]:= Ch;
  end;
end;
```


```txt

// in : S = 'the quick brown fox jumps over the lazy dog'
// out: S = '        abcdeeefghhijklmnoooopqrrsttuuvwxyz'

```



## E

{{lines too long|E}}
A direct conversion of the pseudocode.


```e
def insertionSort(array) {
    for i in 1..!(array.size()) {
        def value := array[i]
        var j := i-1
        while (j >= 0 && array[j] > value) {
            array[j + 1] := array[j]
            j -= 1
        }
        array[j+1] := value
   }
}
```


Test case:


```e
? def a := [71, 53, 22, 24, 83, 54, 39, 78, 65, 26, 60, 75, 67, 27, 52, 59, 93, 62, 85, 99, 88, 10, 91, 85, 13, 17, 14, 96, 55, 10, 61, 94, 27, 50, 75, 40, 47, 63, 10, 23].diverge()
> insertionSort(a)
> a
# value: [10, 10, 10, 13, 14, 17, 22, 23, 24, 26, 27, 27, 39, 40, 47, 50, 52, 53, 54, 55, 59, 60, 61, 62, 63, 65, 67, 71, 75, 75, 78, 83, 85, 85, 88, 91, 93, 94, 96, 99].diverge()
```



## EasyLang


<lang>subr sort
  for i = 1 to len data[] - 1
    h = data[i]
    j = i - 1
    while j >= 0 and h < data[j]
      data[j + 1] = data[j]
      j -= 1
    .
    data[j + 1] = h
  .
.
data[] = [ 29 4 72 44 55 26 27 77 92 5 ]
call sort
print data[]
```



## Eiffel

{{works with|EiffelStudio|6.6 (with provisional loop syntax)}}

This solution is shown in the routine <code lang="eiffel">sort</code> of the class <code lang="eiffel">MY_SORTED_SET</code>.

For a more complete explanation of the Eiffel sort examples, see the [[Sorting algorithms/Bubble sort#Eiffel|Bubble sort]].


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
            -- Insertion sort
        local
            l_j: INTEGER
            l_value: like item
        do
            across 2 |..| count as ii loop
                from
                    l_j := ii.item - 1
                    l_value := Current.i_th (ii.item)
                until
                    l_j < 1 or Current.i_th (l_j) <= l_value
                loop
                    Current.i_th (l_j + 1) := Current.i_th (l_j)
                    l_j := l_j - 1
                end
                Current.i_th (l_j + 1) := l_value
            end
        end

end
```


## Elena

ELENA 4.1 :

```elena
import extensions;

extension op
{
    insertionSort()
        = self.clone().insertionSort(0, self.Length - 1);

    insertionSort(int first, int last)
    {
        for(int i := first + 1, i <= last, i += 1)
        {
            var entry := self[i];
            int j := i;

            while (j > first && self[j - 1] > entry)
            {
                self[j] := self[j - 1];

                j -= 1
            };

            self[j] := entry
        }
    }
}

public program()
{
    var list := new int[]::(3, 9, 4, 6, 8, 1, 7, 2, 5);

    console.printLine("before:", list.asEnumerable());
    console.printLine("after :", list.insertionSort().asEnumerable());
}
```

{{out}}

```txt

before:3,9,4,6,8,1,7,2,5
after :1,2,3,4,5,6,7,8,9

```



## Elixir


```elixir
defmodule Sort do
  def insert_sort(list) when is_list(list), do: insert_sort(list, [])

  def insert_sort([], sorted), do: sorted
  def insert_sort([h | t], sorted), do: insert_sort(t, insert(h, sorted))

  defp insert(x, []), do: [x]
  defp insert(x, sorted) when x < hd(sorted), do: [x | sorted]
  defp insert(x, [h | t]), do: [h | insert(x, t)]
end
```


Example:

```txt

iex(10)> Sort.insert_sort([5,3,9,4,1,6,8,2,7])
[1, 2, 3, 4, 5, 6, 7, 8, 9]

```



## Emacs Lisp


```lisp


(defun min-or-max-of-2-numbers (n1 n2 rel)
  "n1 and n2 are two numbers, rel can be '< or '> according to
what sort of sorting is wanted, this function returns the greater
or smaller number n1 or n2"
  (cond
   ((eval (list rel n1 n2)) n1)
   (t n2)))

(defun min-or-max-of-a-list (lon rel)
  "lon is a list of numbers, rel is '< or '>, this fonction
returns the higher or lower number of the list"
  (if (cdr lon)
      (min-or-max-of-2-numbers (car lon)
			       (min-or-max-of-a-list (cdr lon) rel)
			       rel)
    (car lon)))

(defun remove-number-from-list (n lon)
  "lon is a list of numbers, n is a number belonging to the list,
this function returns the same list but the number n. If n is
present twice or more, it will be removed only once"
  (if lon
      (cond
       ((= (car lon) n) (cdr lon))
       (t (cons (car lon) (remove-number-from-list n (cdr lon)))))
    nil))


(defun sort-insertion (lon rel)
  "lon is a list of numbers, rel can be '< or '>, this function
returns a list containing the same elements but which is sorted
according to rel"
  (if lon
      (cons (min-or-max-of-a-list lon rel)
	    (sort-insertion
	     (remove-number-from-list
	      (min-or-max-of-a-list lon rel)
	      lon)
	     rel))
    nil))

;;; let's try it :

(sort-insertion (list 1 2 3 9 8 7 25 12 3 2 1) '>)


```



## Erlang


```Erlang
-module(sort).
-export([insertion/1]).

insertion(L) -> lists:foldl(fun insert/2, [], L).

insert(X,[]) -> [X];
insert(X,L=[H|_]) when X =< H -> [X|L];
insert(X,[H|T]) -> [H|insert(X, T)].
```


And the calls:

```erlang>1
 c(sort).
{ok,sort}
2> sort:insertion([5,3,9,4,1,6,8,2,7]).
[1,2,3,4,5,6,7,8,9]
```



## ERRE

Note: array index is assumed to start at zero.

```ERRE

PROGRAM INSERTION_SORT

DIM A[9]

PROCEDURE INSERTION_SORT(A[])
    LOCAL I,J
    FOR I=0 TO UBOUND(A,1) DO
        V=A[I]
        J=I-1
        WHILE J>=0 DO
          IF A[J]>V THEN
            A[J+1]=A[J]
            J=J-1
           ELSE
            EXIT
          END IF
        END WHILE
        A[J+1]=V
    END FOR
END PROCEDURE

BEGIN
  A[]=(4,65,2,-31,0,99,2,83,782,1)
  FOR I%=0 TO UBOUND(A,1) DO
     PRINT(A[I%];)
  END FOR
  PRINT
  INSERTION_SORT(A[])
  FOR I%=0 TO UBOUND(A,1) DO
     PRINT(A[I%];)
  END FOR
  PRINT
END PROGRAM

```

{{out}}

```txt

 4  65  2 -31  0  99  2  83  782  1
-31  0  1  2  2  4  65  83  99  782

```



## Euphoria


```euphoria
function insertion_sort(sequence s)
    object temp
    integer j
    for i = 2 to length(s) do
        temp = s[i]
        j = i-1
        while j >= 1 and compare(s[j],temp) > 0 do
            s[j+1] = s[j]
            j -= 1
        end while
        s[j+1] = temp
    end for
    return s
end function

include misc.e
constant s = {4, 15, "delta", 2, -31, 0, "alfa", 19, "gamma", 2, 13, "beta", 782, 1}

puts(1,"Before: ")
pretty_print(1,s,{2})
puts(1,"\nAfter: ")
pretty_print(1,insertion_sort(s),{2})
```


{{out}}

```txt
Before: {
  4,
  15,
  "delta",
  2,
  -31,
  0,
  "alfa",
  19,
  "gamma",
  2,
  13,
  "beta",
  782,
  1
}
After: {
  -31,
  0,
  1,
  2,
  2,
  4,
  13,
  15,
  19,
  782,
  "alfa",
  "beta",
  "delta",
  "gamma"
}
```


=={{header|F Sharp|F#}}==
Procedural Version

```fsharp

// This function performs an insertion sort with an array.
// The input parameter is a generic array (any type that can perform comparison).
// As is typical of functional programming style the input array is not modified;
// a copy of the input array is made and modified and returned.
let insertionSort (A: _ array) =
    let B = Array.copy A
    for i = 1 to B.Length - 1 do
        let mutable value = B.[i]
        let mutable j = i - 1
        while (j >= 0 && B.[j] > value) do
            B.[j+1] <- B.[j]
            j <- j - 1
        B.[j+1] <- value
    B  // the array B is returned

```


Functional Version

```fsharp

let insertionSort collection =

    // Inserts an element into its correct place in a sorted collection
    let rec sinsert element collection =
        match element, collection with
        | x, [] -> [x]
        | x, y::ys when x < y -> x::y::ys
        | x, y::ys -> y :: (ys |> sinsert x)

    // Performs Insertion Sort
    let rec isort acc collection =
        match collection, acc with
        | [], _ -> acc
        | x::xs, ys -> xs |> isort (sinsert x ys)
    collection |> isort []

```



## Forth


```forth
: insert ( start end -- start )
  dup @ >r ( r: v )	\ v = a[i]
  begin
    2dup <			\ j>0
  while
    r@ over cell- @ <		\ a[j-1] > v
  while
    cell-			\ j--
    dup @ over cell+ !		\ a[j] = a[j-1]
  repeat then
  r> swap ! ;		\ a[j] = v

: sort ( array len -- )
  1 ?do dup i cells + insert loop drop ;

create test 7 , 3 , 0 , 2 , 9 , 1 , 6 , 8 , 4 , 5 ,
test 10 sort
test 10 cells dump
```



## Fortran

{{works with|Fortran|90 and later}}


```fortran
subroutine sort(n, a)
    implicit none
    integer :: n, i, j
    real :: a(n), x

    do i = 2, n
        x = a(i)
        j = i - 1
        do while (j >= 1)
            if (a(j) <= x) exit
            a(j + 1) = a(j)
            j = j - 1
        end do
        a(j + 1) = x
    end do
end subroutine
```



###  Alternate Fortran 77 version

This also could have a problem with the compound test always being fully evaluated, so...
```fortran
      SUBROUTINE SORT(N,A)
      IMPLICIT NONE
      INTEGER N,I,J
      DOUBLE PRECISION A(N),X
      DO 30 I = 2,N
        X = A(I)
        J = I
   10   J = J - 1
Can't   IF (J.EQ.0 .OR. A(J).LE.X) GO TO 20 in case both sides are ALWAYS evaluated.
        IF (J.EQ.0) GO TO 20
        IF (A(J).LE.X) GO TO 20
        A(J + 1) = A(J)
        GO TO 10
   20   A(J + 1) = X
   30 CONTINUE
      END
```



## FreeBASIC


```freebasic
' version 20-10-2016
' compile with: fbc -s console
' for boundry checks on array's compile with: fbc -s console -exx

Sub insertionSort( arr() As Long )

  ' sort from lower bound to the highter bound
  ' array's can have subscript range from -2147483648 to +2147483647

  Dim As Long lb = LBound(arr)
  Dim As Long i, j, value

  For i = lb +1 To UBound(arr)

    value = arr(i)
    j = i -1
    While j >= lb  And arr(j) > value
      arr(j +1) = arr(j)
      j = j -1
    Wend

    arr(j +1) = value

  Next

End Sub

' ------=< MAIN >=------

Dim As Long i, array(-7 To 7)
Dim As Long a = LBound(array), b = UBound(array)

Randomize Timer
For i = a To b : array(i) = i  : Next
For i = a To b ' little shuffle
  Swap array(i), array(Int(Rnd * (b - a +1)) + a)
Next

Print "unsort ";
For i = a To b : Print Using "####"; array(i); : Next : Print
insertionSort(array())  ' sort the array
Print "  sort ";
For i = a To b : Print Using "####"; array(i); : Next : Print


' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
unsort   -7  -1   4  -6   5   2   1  -2   0  -5  -4   6  -3   7   3
  sort   -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7
```


## GAP


```gap
InsertionSort := function(L)
  local n, i, j, x;
  n := Length(L);
  for i in [ 2 .. n ] do
    x := L[i];
    j := i - 1;
    while j >= 1 and L[j] > x do
      L[j + 1] := L[j];
      j := j - 1;
    od;
    L[j + 1] := x;
  od;
end;

s := "BFKRIMPOQACNESWUTXDGLVZHYJ";
InsertionSort(s);
s;
# "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
```



## Go


```go
package main

import "fmt"

func insertionSort(a []int) {
    for i := 1; i < len(a); i++ {
        value := a[i]
        j := i - 1
        for j >= 0 && a[j] > value {
            a[j+1] = a[j]
            j = j - 1
        }
        a[j+1] = value
    }
}

func main() {
    list := []int{31, 41, 59, 26, 53, 58, 97, 93, 23, 84}
    fmt.Println("unsorted:", list)

    insertionSort(list)
    fmt.Println("sorted!  ", list)
}
```

{{out}}

```txt

unsorted: [31 41 59 26 53 58 97 93 23 84]
sorted!   [23 26 31 41 53 58 59 84 93 97]

```


A generic version that takes any container that conforms to <code>sort.Interface</code>:

```go
package main

import (
  "fmt"
  "sort"
)

func insertionSort(a sort.Interface) {
    for i := 1; i < a.Len(); i++ {
        for j := i; j > 0 && a.Less(j, j-1); j-- {
            a.Swap(j-1, j)
        }
    }
}

func main() {
    list := []int{31, 41, 59, 26, 53, 58, 97, 93, 23, 84}
    fmt.Println("unsorted:", list)

    insertionSort(sort.IntSlice(list))
    fmt.Println("sorted!  ", list)
}
```

{{out}}

```txt

unsorted: [31 41 59 26 53 58 97 93 23 84]
sorted!   [23 26 31 41 53 58 59 84 93 97]

```


Using binary search to locate the place to insert:

```go
package main

import (
  "fmt"
  "sort"
)

func insertionSort(a []int) {
    for i := 1; i < len(a); i++ {
        value := a[i]
        j := sort.Search(i, func(k int) bool { return a[k] > value })
        copy(a[j+1:i+1], a[j:i])
        a[j] = value
    }
}

func main() {
    list := []int{31, 41, 59, 26, 53, 58, 97, 93, 23, 84}
    fmt.Println("unsorted:", list)

    insertionSort(list)
    fmt.Println("sorted!  ", list)
}
```

{{out}}

```txt

unsorted: [31 41 59 26 53 58 97 93 23 84]
sorted!   [23 26 31 41 53 58 59 84 93 97]

```



## Groovy

Solution:

```groovy
def insertionSort = { list ->

    def size = list.size()
    (1..<size).each { i ->
        def value = list[i]
        def j = i - 1
        for (; j >= 0 && list[j] > value; j--) {
            print "."; list[j+1] = list[j]
        }
        print "."; list[j+1] = value
    }
    list
}
```


Test:

```groovy
println (insertionSort([23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4]))
println (insertionSort([88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1]))
```


{{out}}

```txt
..................................................................................................................................................................[4, 12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]
...............................................................................................................................................................[0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]
```



## Haskell


```haskell
import Data.List (insert)

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

-- Example use:
-- *Main> insertionSort [6,8,5,9,3,2,1,4,7]
-- [1,2,3,4,5,6,7,8,9]
```



## HicEst


```hicest
DO i = 2, LEN(A)
   value = A(i)
   j = i - 1
 1 IF( j > 0 ) THEN
     IF( A(j) > value ) THEN
       A(j+1) = A(j)
       j = j - 1
       GOTO 1 ! no WHILE in HicEst
     ENDIF
   ENDIF
   A(j+1) = value
ENDDO
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()                     #: demonstrate various ways to sort a list and string
   demosort(insertionsort,[3, 14, 1, 5, 9, 2, 6, 3],"qwerty")
end

procedure insertionsort(X,op)        #: return sorted X
local i,temp

   op := sortop(op,X)                # select how and what we sort

   every i := 2 to *X do {
      temp := X[j := i]
      while op(temp,X[1 <= (j -:= 1)]) do
         X[j+1] := X[j]
      X[j+1] := temp
      }
   return X
end
```


Note: This example relies on [[Sorting_algorithms/Bubble_sort#Icon| the supporting procedures 'sortop', and 'demosort' in Bubble Sort]]. The full demosort exercises the named sort of a list with op = "numeric", "string", ">>" (lexically gt, descending),">" (numerically gt, descending), a custom comparator, and also a string.

{{out|abbreviated}}

```txt
Sorting Demo using procedure insertionsort
  on list : [ 3 14 1 5 9 2 6 3 ]
    with op = &null:         [ 1 2 3 3 5 6 9 14 ]   (0 ms)
  ...
  on string : "qwerty"
    with op = &null:         "eqrtwy"   (0 ms)
```



## Io



```io

List do(
  insertionSortInPlace := method(
    for(j, 1, size - 1,
      key := at(j)
      i := j - 1

      while(i >= 0 and at(i) > key,
        atPut(i + 1, at(i))
        i = i - 1
      )
      atPut(i + 1, key)
    )
  )
)

lst := list(7, 6, 5, 9, 8, 4, 3, 1, 2, 0)
lst insertionSortInPlace println # ==> list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
```


A shorter, but slightly less efficient, version:

```io
List do(
    insertionSortInPlace := method(
        # In fact, we could've done slice(1, size - 1) foreach(...)
        # but creating a new list in memory can only make it worse.
        foreach(idx, key,
            newidx := slice(0, idx) map(x, x > key) indexOf(true)
            if(newidx, insertAt(removeAt(idx), newidx))
        )
    self)
)

lst := list(7, 6, 5, 9, 8, 4, 3, 1, 2, 0)
lst insertionSortInPlace println # ==> list(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

```



## J

{{eff note|J|/:~}}
Solution inspired by the Common LISP solution:

```J
isort=:((>: # ]) , [ , < #])/
```

Example of use:

```J
   isort 32 4 1 34 95 3 2 120 _38
_38 1 2 3 4 32 34 95 120
```



## Java


```java5
public static void insertSort(int[] A){
  for(int i = 1; i < A.length; i++){
    int value = A[i];
    int j = i - 1;
    while(j >= 0 && A[j] > value){
      A[j + 1] = A[j];
      j = j - 1;
    }
    A[j + 1] = value;
  }
}
```


Using some built-in algorithms (warning: not stable, due to the lack of an "upper bound" binary search function)
{{trans|C++}}

```java5>public static <E extends Comparable<? super E>
 void insertionSort(List<E> a) {
  for (int i = 1; i < a.size(); i++) {
    int j = Math.abs(Collections.binarySearch(a.subList(0, i), a.get(i)) + 1);
    Collections.rotate(a.subList(j, i+1), j - i);
  }
}
public static <E extends Comparable<? super E>> void insertionSort(E[] a) {
  for (int i = 1; i < a.length; i++) {
    E x = a[i];
    int j = Math.abs(Arrays.binarySearch(a, 0, i, x) + 1);
    System.arraycopy(a, j, a, j+1, i-j);
    a[j] = x;
  }
}
```



## JavaScript


```javascript

function insertionSort (a) {
    for (var i = 0; i < a.length; i++) {
        var k = a[i];
        for (var j = i; j > 0 && k < a[j - 1]; j--)
            a[j] = a[j - 1];
        a[j] = k;
    }
    return a;
}

var a = [4, 65, 2, -31, 0, 99, 83, 782, 1];
insertionSort(a);
document.write(a.join(" "));
```



## jq

{{works with|jq|1.4}}
The insertion sort can be expressed directly in jq as follows:

```jq
def insertion_sort:
  reduce .[] as $x ([]; insert($x));
```
where insert/1 inserts its argument into its input, which can, by construction, be assumed here to be sorted.  This algorithm will work in jq for any JSON array.

The following solution uses an "industrial strength" implementation of bsearch (binary search) that requires the following control structure:

```jq
# As soon as "condition" is true, then emit . and stop:
def do_until(condition; next):
  def u: if condition then . else (next|u) end;
  u;
```


bsearch is the only non-trivial part of this solution, and so we include
its complete specification:

Assuming the input array is sorted, bsearch/1 returns
the index of the target if the target is in the input array; and otherwise
(-1 - ix), where ix is the insertion point that would leave the array sorted.

If the input is not sorted, bsearch will terminate but with irrelevant results.
```jq
def bsearch(target):
  if length == 0 then -1
  elif length == 1 then
     if target == .[0] then 0 elif target < .[0] then -1 else -2 end
  else . as $in
    # state variable: [start, end, answer]
    # where start and end are the upper and lower offsets to use.
      | [0, length-1, null]
      | do_until( .[0] > .[1] ;
                (if .[2] != null then (.[1] = -1) # i.e. break
                 else
                   ( ( (.[1] + .[0]) / 2 ) | floor ) as $mid
                 | $in[$mid] as $monkey
                 | if $monkey == target  then (.[2] = $mid)     # success
                   elif .[0] == .[1]     then (.[1] = -1)       # failure
                   elif $monkey < target then (.[0] = ($mid + 1))
                   else (.[1] = ($mid - 1))
                   end
                 end ))
    | if .[2] == null then # compute the insertion point
         if $in[ .[0] ] < target then (-2 -.[0])
         else (-1 -.[0])
         end
      else .[2]
      end
  end;

# insert x assuming input is sorted
def insert(x):
  if length == 0 then [x]
  else
    bsearch(x) as $i
    | ( if $i < 0 then -(1+$i) else $i end ) as $i
    | .[0:$i] + [x] + .[$i:]
  end ;

def insertion_sort:
   reduce .[] as $x ([]; insert($x));
```

Example:
```jq
[1, 2, 1, 1.1, -1.1, null, [null], {"null":null}] | insertion_sort
```

{{Out}}
 [null,-1.1,1,1,1.1,2,[null],{"null":null}]



## Julia


```julia
# v0.6

function insertionsort!(A::Array{T}) where T <: Number
    for i in 1:length(A)-1
        value = A[i+1]
        j = i
        while j > 0 && A[j] > value
            A[j+1] = A[j]
            j -= 1
        end
        A[j+1] = value
    end
    return A
end

x = randn(5)
@show x insertionsort!(x)
```


{{out}}

```txt
x = [-1.24011, -1.23848, 0.176698, -1.01986, 0.830544]
insertionsort!(x) = [-1.24011, -1.23848, -1.01986, 0.176698, 0.830544]
```



## Kotlin


```kotlin
fun insertionSort(array: IntArray) {
    for (index in 1 until array.size) {
        val value = array[index]
        var subIndex = index - 1
        while (subIndex >= 0 && array[subIndex] > value) {
            array[subIndex + 1] = array[subIndex]
            subIndex--
        }
        array[subIndex + 1] = value
    }
}

fun main(args: Array<String>) {
    val numbers = intArrayOf(5, 2, 3, 17, 12, 1, 8, 3, 4, 9, 7)

    fun printArray(message: String, array: IntArray) = with(array) {
        print("$message [")
        forEachIndexed { index, number ->
            print(if (index == lastIndex) number else "$number, ")
        }
        println("]")
    }

    printArray("Unsorted:", numbers)
    insertionSort(numbers)
    printArray("Sorted:", numbers)
}
```


{{out}}

```txt
Unsorted: [5, 2, 3, 17, 12, 1, 8, 3, 4, 9, 7]
Sorted:   [1, 2, 3, 3, 4, 5, 7, 8, 9, 12, 17]
```



## Liberty BASIC


```lb
   itemCount = 20
    dim A(itemCount)
    for i = 1 to itemCount
        A(i) = int(rnd(1) * 100)
    next i

    print "Before Sort"
    gosub [printArray]

'--- Insertion sort algorithm
    for i = 2 to itemCount
        value = A(i)
        j = i-1
        while j >= 0 and A(j) > value
            A(j+1) = A(j)
            j = j-1
        wend
        A(j+1) = value
    next
'--- end of (Insertion sort algorithm)

    print "After Sort"
    gosub [printArray]
end

[printArray]
    for i = 1 to itemCount
        print using("###", A(i));
    next i
    print
return
```



## Lua



```lua
function bins(tb, val, st, en)
  local st, en = st or 1, en or #tb
  local mid = math.floor((st + en)/2)
  if en == st then return tb[st] > val and st or st+1
  else return tb[mid] > val and bins(tb, val, st, mid) or bins(tb, val, mid+1, en)
  end
end
function isort(t)
  local ret = {t[1], t[2]}
  for i = 3, #t do
    table.insert(ret, bins(ret, t[i]), t[i])
  end
  return ret
end

print(unpack(isort{4,5,2,7,8,3}))
```



## Maple


```Maple
arr := Array([17,3,72,0,36,2,3,8,40,0]):
len := numelems(arr):
for i from 2 to len do
	val := arr[i]:
	j := i-1:
	while(j > 0 and arr[j] > val) do
		arr[j+1] := arr[j]:
		j--:
	end do:
	arr[j+1] := val:
end do:
arr;
```

{{Out|Output}}

```txt
[0,0,2,3,3,8,17,36,40,72]
```



## Mathematica


```Mathematica
insertionSort[a_List] := Module[{A = a},
  For[i = 2, i <= Length[A], i++,
   value = A[[i]];    j = i - 1;
   While[j >= 1 && A[[j]] > value, A[[j + 1]] = A[[j]]; j--;];
   A[[j + 1]] = value;];
A
]
```



```txt
insertionSort@{ 2, 1, 3, 5}
{1, 2, 3, 5}
```


=={{header|MATLAB}} / {{header|Octave}}==
This is a direct translation of the pseudo-code above, except that it has been modified to compensate for MATLAB's 1 based arrays.

```MATLAB
function list = insertionSort(list)

    for i = (2:numel(list))

        value = list(i);
        j = i - 1;

        while (j >= 1) && (list(j) > value)
            list(j+1) = list(j);
            j = j-1;
        end

        list(j+1) = value;

    end %for
end %insertionSort
```


Sample Usage:

```MATLAB>>
 insertionSort([4 3 1 5 6 2])

ans =

     1     2     3     4     5     6
```



## Maxima


```maxima
insertion_sort(u) := block(
   [n: length(u), x, j],
   for i from 2 thru n do (
      x: u[i],
      j: i - 1,
      while j >= 1 and u[j] > x do (
         u[j + 1]: u[j],
         j: j - 1
      ),
      u[j + 1]: x
   )
)$
```



## MAXScript


```MAXScript

fn inSort arr =
(
	arr = deepcopy arr
	for i = 1 to arr.count do
	(
		j = i
		while j > 1 and arr[j-1] > arr[j] do
		(
			swap arr[j] arr[j-1]
			j -= 1
		)
	)
	return arr
)

```

Output:

```MAXScript

b = for i in 1 to 20 collect random 1 40
#(2, 28, 35, 31, 27, 24, 2, 22, 15, 34, 9, 10, 22, 40, 26, 5, 23, 6, 18, 33)
a = insort b
#(2, 2, 5, 6, 9, 10, 15, 18, 22, 22, 23, 24, 26, 27, 28, 31, 33, 34, 35, 40)

```



## ML

=
## mLite
=
{{trans|OCaml}}

```ocaml
fun insertion_sort L =
	let
		fun insert
				(x,[]) = [x]
			|	(x, y :: ys) =
					if x <= y then
						x :: y :: ys
					else
						y :: insert (x, ys)
	in
		foldr (insert,[]) L
	end;

println ` insertion_sort [6,8,5,9,3,2,1,4,7];

```

Output

```txt

[1, 2, 3, 4, 5, 6, 7, 8, 9]

```


=
## Standard ML
=

```sml
fun insertion_sort cmp = let
  fun insert (x, []) = [x]
    | insert (x, y::ys) =
       case cmp (x, y) of GREATER => y :: insert (x, ys)
                        | _       => x :: y :: ys
in
 foldl insert []
end;

insertion_sort Int.compare [6,8,5,9,3,2,1,4,7];
```


=={{header|Modula-3}}==
{{trans|Ada}}

```modula3
MODULE InsertSort;

PROCEDURE IntSort(VAR item: ARRAY OF INTEGER) =
  VAR j, value: INTEGER;
  BEGIN
    FOR i := FIRST(item) + 1 TO LAST(item) DO
      value := item[i];
      j := i - 1;
      WHILE j >= FIRST(item) AND item[j] > value DO
        item[j + 1] := item[j];
        DEC(j);
      END;
      item[j + 1] := value;
    END;
  END IntSort;
END InsertSort.
```



## N/t/roff


{{works with|GNU Troff|1.22.2}}


### Sliding method


```N/t/roff
.de end
..
.de array
.	nr \\$1.c 0 1
.	de \\$1.push end
.		nr \\$1..\\\\n+[\\$1.c] \\\\$1
.	end
.	de \\$1.pushln end
.		if \\\\n(.$>0 .\\$1.push \\\\$1
.		if \\\\n(.$>1 \{ \
.			shift
.			\\$1.pushln \\\\$@
.		\}
.	end
.	de \\$1.dump end
.		nr i 0 1
.		ds out "
.		while \\\\n+i<=\\\\n[\\$1.c] .as out "\\\\n[\\$1..\\\\ni]
.		tm \\\\*[out]
.		rm out
.		rr i
.	end
.	de \\$1.slideright end
.		nr i \\\\$1
.		nr i+1 \\\\ni+1
.		nr \\$1..\\\\n[i+1] \\\\n[\\$1..\\\\ni]
.		rr i
.		rr i+1
.	end
..
.de insertionsort
.	nr keyidx 1 1
.	while \\n+[keyidx]<=\\n[\\$1.c] \{ \
.		nr key \\n[\\$1..\\n[keyidx]]
.		nr compidx \\n[keyidx] 1
.		while \\n-[compidx]>=0 \{ \
.			if \\n[compidx]=0 \{ \
.				nr \\$1..1 \\n[key]
.				break
.			\}
.			ie \\n[\\$1..\\n[compidx]]>\\n[key] \{ \
.				\\$1.slideright \\n[compidx]
.			\}
.			el \{ \
.				nr compidx+1 \\n[compidx]+1
.				nr \\$1..\\n[compidx+1] \\n[key]
.				break
.			\}
.		\}
.	\}
..
.array a
.a.pushln 13 64 22 87 54 87 23 92 11 64 5 9 3 3 0
.insertionsort a
.a.dump
```



### Swapping method


```N/t/roff
.de end
..
.de array
.	nr \\$1.c 0 1
.	de \\$1.push end
.		nr \\$1..\\\\n+[\\$1.c] \\\\$1
.	end
.	de \\$1.pushln end
.		if \\\\n(.$>0 .\\$1.push \\\\$1
.		if \\\\n(.$>1 \{ \
.			shift
.			\\$1.pushln \\\\$@
.		\}
.	end
.	de \\$1.dump end
.		nr i 0 1
.		ds out "
.		while \\\\n+i<=\\\\n[\\$1.c] .as out "\\\\n[\\$1..\\\\ni]
.		tm \\\\*[out]
.		rm out
.		rr i
.	end
.	de \\$1.swap end
.		if (\\\\$1<=\\\\n[\\$1.c])&(\\\\$1<=\\\\n[\\$1.c]) \{ \
.			nr tmp \\\\n[\\$1..\\\\$2]
.			nr \\$1..\\\\$2 \\\\n[\\$1..\\\\$1]
.			nr \\$1..\\\\$1 \\\\n[tmp]
.			rr tmp
.		\}
.	end
..
.de insertionsort
.	nr keyidx 1 1
.	while \\n+[keyidx]<=\\n[\\$1.c] \{ \
.		nr compidx \\n[keyidx]+1 1
.		nr compidx-1 \\n[keyidx] 1
.		while (\\n-[compidx]>0)&(\\n[\\$1..\\n-[compidx-1]]>\\n[\\$1..\\n[compidx]]) \{ \
.			\\$1.swap \\n[compidx] \\n[compidx-1]
.		\}
.	\}
..
.array a
.a.pushln 13 64 22 87 54 87 23 92 11 64 5 9 3 3 0
.insertionsort a
.a.dump
```



## Nemerle

From the psuedocode.

```Nemerle
using System.Console;
using Nemerle.English;

module InsertSort
{
    public static Sort(this a : array[int]) : void
    {
        mutable value = 0; mutable j = 0;
        foreach (i in [1 .. (a.Length - 1)])
        {
            value = a[i]; j = i - 1;
            while (j >= 0 and a[j] > value)
            {
                a[j + 1] = a[j];
                j = j - 1;
            }
            a[j + 1] = value;
        }
    }

    Main() : void
    {
        def arr = array[1, 4, 8, 3, 8, 3, 5, 2, 6];
        arr.Sort();
        foreach (i in arr) Write($"$i  ");
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

import java.util.List

placesList = [String -
    "UK  London",     "US  New York",   "US  Boston",     "US  Washington" -
  , "UK  Washington", "US  Birmingham", "UK  Birmingham", "UK  Boston"     -
]

lists = [ -
    placesList -
  , insertionSort(String[] Arrays.copyOf(placesList, placesList.length)) -
]

loop ln = 0 to lists.length - 1
  cl = lists[ln]
  loop ct = 0 to cl.length - 1
    say cl[ct]
    end ct
    say
  end ln

return

method insertionSort(A = String[]) public constant binary returns String[]

  rl = String[A.length]
  al = List insertionSort(Arrays.asList(A))
  al.toArray(rl)

  return rl

method insertionSort(A = List) public constant binary returns ArrayList

  loop i_ = 1 to A.size - 1
    value = A.get(i_)
    j_ = i_ - 1
    loop label j_ while j_ >= 0
      if (Comparable A.get(j_)).compareTo(Comparable value) <= 0 then leave j_
      A.set(j_ + 1, A.get(j_))
      j_ = j_ - 1
      end j_
      A.set(j_ + 1, value)
    end i_

  return ArrayList(A)

```

{{out}}

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
proc insertSort[T](a: var openarray[T]) =
  for i in 1 .. <a.len:
    let value = a[i]
    var j = i
    while j > 0 and value < a[j-1]:
      a[j] = a[j-1]
      dec j
    a[j] = value

var a = @[4, 65, 2, -31, 0, 99, 2, 83, 782]
insertSort a
echo a
```

{{out}}

```txt
@[-31, 0, 2, 2, 4, 65, 83, 99, 782]
```



## Objeck


```objeck

bundle Default {
  class Insert {
    function : Main(args : String[]) ~ Nil {
      values := [9, 7, 10, 2, 9, 7, 4, 3, 10, 2, 7, 10];
      InsertionSort(values);
      each(i : values) {
        values[i]->PrintLine();
      };
    }

    function : InsertionSort (a : Int[]) ~ Nil {
      each(i : a) {
        value := a[i];
        j := i - 1;
        while(j >= 0 & a[j] > value) {
          a[j + 1] := a[j];
          j -= 1;
        };
        a[j + 1] := value;
      };
    }
  }
}

```



## OCaml


```ocaml
let rec insert lst x =
  match lst with
    [] -> [x]
  | y :: ys  when x <= y -> x :: y :: ys
  | y :: ys -> y :: insert ys x

;;
let insertion_sort = List.fold_left insert [];;

insertion_sort [6;8;5;9;3;2;1;4;7];;
```



## Oforth


Returns a new sorted list.


```Oforth
: insertionSort(a)
| l i j v |
   a asListBuffer ->l
   2 l size for: i [
      l at(i) ->v
      i 1- ->j
      while(j) [
         l at(j) dup v <= ifTrue: [ drop break ]
         j 1+ swap l put
         j 1- ->j
         ]
      l put(j 1 +, v)
      ]
   l ;
```


{{out}}

```txt

>[ 4, 65, 2, -31, 0, 99, 2, 83, 782, 1 ] insertionSort .
[-31, 0, 1, 2, 2, 4, 65, 83, 99, 782] ok
>

```



## ooRexx

{{trans|REXX}}

```oorexx
/* REXX program sorts a stemmed array (has characters)                */
/* using the insertion sort algorithm                                 */
  Call gen                          /* fill the array with test data  */
  Call show 'before sort'           /* display the elements           */
  Say copies('-',79)                /* display a separator line       */
  Call insertionSort x.0            /* invoke the insertion sort.     */
  Call show ' after sort'           /* display the elements after sort*/
  Exit
/*--------------------------------------------------------------------*/
gen: Procedure Expose x.
  x.1="---Monday's Child Is Fair of Face  (by Mother Goose)---"
  x.2="
### =================================================
"
  x.3="Monday's child is fair of face;"
  x.4="Tuesday's child is full of grace;"
  x.5="Wednesday's child is full of woe;"
  x.6="Thursday's child has far to go;"
  x.7="Friday's child is loving and giving;"
  x.8="Saturday's child works hard for a living;"
  x.9="But the child that is born on the Sabbath day"
  x.10="Is blithe and bonny, good and gay."
  x.0=10                            /* number of elements             */
  Return
/*--------------------------------------------------------------------*/
insertionsort: Procedure Expose x.
  Parse Arg n
  Do i=2 To n
    y=x.i
    Do j=i-1 By -1 To 1 While x.j>y
      z=j+1
      x.z=x.j
      /* Say 'set x.'z 'to x.'j '('||x.j||')' */
      End
    z=j+1
    x.z=y
    /* Say 'set x.'z 'to' y                   */
    End
  Return
/*--------------------------------------------------------------------*/
show:
  Do j=1 To x.0
    Say 'Element' right(j,length(x.0)) arg(1)":" x.j
    End
  Return
```

{{out}}

```txt
Element  1 before sort: ---Monday's Child Is Fair of Face  (by Mother Goose)---
Element  2 before sort:
### =================================================

Element  3 before sort: Monday's child is fair of face;
Element  4 before sort: Tuesday's child is full of grace;
Element  5 before sort: Wednesday's child is full of woe;
Element  6 before sort: Thursday's child has far to go;
Element  7 before sort: Friday's child is loving and giving;
Element  8 before sort: Saturday's child works hard for a living;
Element  9 before sort: But the child that is born on the Sabbath day
Element 10 before sort: Is blithe and bonny, good and gay.
-------------------------------------------------------------------------------
Element  1  after sort: ---Monday's Child Is Fair of Face  (by Mother Goose)---
Element  2  after sort:
### =================================================

Element  3  after sort: But the child that is born on the Sabbath day
Element  4  after sort: Friday's child is loving and giving;
Element  5  after sort: Is blithe and bonny, good and gay.
Element  6  after sort: Monday's child is fair of face;
Element  7  after sort: Saturday's child works hard for a living;
Element  8  after sort: Thursday's child has far to go;
Element  9  after sort: Tuesday's child is full of grace;
Element 10  after sort: Wednesday's child is full of woe;
```



## Oz

Direct translation of pseudocode. In-place sorting of mutable arrays.

```oz
declare
  proc {InsertionSort A}
     Low = {Array.low A}
     High = {Array.high A}
  in
     for I in Low+1..High do
        Value = A.I
        J = {NewCell I-1}
     in
        for while:@J >= Low andthen A.@J > Value do
           A.(@J+1) := A.@J
           J := @J - 1
        end
        A.(@J+1) := Value
     end
  end

  Arr = {Tuple.toArray unit(3 1 4 1 5 9 2 6 5)}
in
  {InsertionSort Arr}
  {Show {Array.toRecord unit Arr}}
```



## Qi

Based on the scheme version.

```qi
(define insert
  X []     -> [X]
  X [Y|Ys] -> [X Y|Ys] where (<= X Y)
  X [Y|Ys] -> [Y|(insert X Ys)])

(define insertion-sort
  []     -> []
  [X|Xs] -> (insert X (insertion-sort Xs)))

(insertion-sort [6 8 5 9 3 2 1 4 7])

```



## PARI/GP


```parigp
insertionSort(v)={
  for(i=1,#v-1,
    my(j=i-1,x=v[i]);
    while(j && v[j]>x,
      v[j+1]=v[j];
      j--
    );
    v[j+1]=x
  );
  v
};
```



## Pascal

See [[Sorting_algorithms/Insertion_sort#Delphi | Delphi]]


## Perl


```perl

sub insertion_sort {
    my (@list) = @_;
    foreach my $i (1 .. $#list) {
        my $j = $i;
        my $k = $list[$i];
        while ( $j > 0 && $k < $list[$j - 1]) {
            $list[$j] = $list[$j - 1];
            $j--;
        }
        $list[$j] = $k;
    }
    return @list;
}

my @a = insertion_sort(4, 65, 2, -31, 0, 99, 83, 782, 1);
print "@a\n";

```

{{out}}
 -31 0 1 2 4 65 83 99 782


## Perl 6


```perl6
sub insertion_sort ( @a is copy ) {
    for 1 .. @a.end -> $i {
        my $value = @a[$i];
        my $j;
        loop ( $j = $i-1; $j >= 0 and @a[$j] > $value; $j-- ) {
            @a[$j+1] = @a[$j];
        }
        @a[$j+1] = $value;
    }
    return @a;
}

my @data = 22, 7, 2, -5, 8, 4;
say 'input  = ' ~ @data;
say 'output = ' ~ @data.&insertion_sort;

```


{{out}}

```txt
input  = 22 7 2 -5 8 4
output = -5 2 4 7 8 22

```



## Phix

Copy of [[Sorting_algorithms/Insertion_sort#Euphoria|Euphoria]]

```Phix
function insertion_sort(sequence s)
object temp
integer j
    for i=2 to length(s) do
        temp = s[i]
        j = i-1
        while j>=1 and s[j]>temp do
            s[j+1] = s[j]
            j -= 1
        end while
        s[j+1] = temp
    end for
    return s
end function

constant s = {4, 15, "delta", 2, -31, 0, "alpha", 19, "gamma", 2, 13, "beta", 782, 1}

puts(1,"Before: ")    ?s
puts(1,"After: ")     ?insertion_sort(s)
```

{{out}}

```txt

Before: {4,15,"delta",2,-31,0,"alpha",19,"gamma",2,13,"beta",782,1}
After: {-31,0,1,2,2,4,13,15,19,782,"alpha","beta","delta","gamma"}

```



## PHP


```php
function insertionSort(&$arr){
	for($i=0;$i<count($arr);$i++){
		$val = $arr[$i];
		$j = $i-1;
		while($j>=0 && $arr[$j] > $val){
			$arr[$j+1] = $arr[$j];
			$j--;
		}
		$arr[$j+1] = $val;
	}
}

$arr = array(4,2,1,6,9,3,8,7);
insertionSort($arr);
echo implode(',',$arr);
```


```txt
1,2,3,4,6,7,8,9
```



## PicoLisp


```PicoLisp
(de insertionSort (Lst)
   (for (I (cdr Lst)  I  (cdr I))
      (for (J Lst  (n== J I)  (cdr J))
         (T (> (car J) (car I))
            (rot J (offset I J)) ) ) )
   Lst )
```

{{out}}

```txt
: (insertionSort (5 3 1 7 4 1 1 20))
-> (1 1 1 3 4 5 7 20)
```



## PL/I


```pli

INSSORT: PROC(A);
   DCL A(*)        FIXED BIN(31);
   DCL (I,J,V,N,M) FIXED BIN(31);

   N = HBOUND(A,1); M = LBOUND(A,1);
   DO I=M+1 TO N;
      V=A(I);
      DO J=I-1 BY -1 WHILE (J>M-1 & A(J)>V);
         A(J+1)=A(J);
      END;
      A(J+1)=V;
   END;
   RETURN;
END INSSORT;

```



## PowerShell

Very similar to the PHP code.

```powershell
function insertionSort($arr){
	for($i=0;$i -lt $arr.length;$i++){
		$val = $arr[$i]
		$j = $i-1
		while($j -ge 0 -and $arr[$j] -gt $val){
			$arr[$j+1] = $arr[$j]
			$j--
		}
		$arr[$j+1] = $val
	}
}

$arr = @(4,2,1,6,9,3,8,7)
insertionSort($arr)
$arr -join ","
```

{{Out}}

```txt
1,2,3,4,6,7,8,9
```



## Prolog


```prolog
insert_sort(L1,L2) :-
  insert_sort_intern(L1,[],L2).

insert_sort_intern([],L,L).
insert_sort_intern([H|T],L1,L) :-
  insert(L1,H,L2),
  insert_sort_intern(T,L2,L).

insert([],X,[X]).
insert([H|T],X,[X,H|T]) :-
  X =< H,
  !.
insert([H|T],X,[H|T2]) :-
  insert(T,X,T2).
```


 % Example use:
 %    ?- insert_sort([2,23,42,3,10,1,34,5],L).
 %    L = [1,2,3,5,10,23,34,42] ?
 %    yes


### Functional approach

Works with SWI-Prolog.

Insertion sort inserts elements of a list in a sorted list. So we can use foldl to sort a list.

```Prolog
% insertion sort
isort(L, LS) :-
	foldl(insert, [], L, LS).


% foldl(Pred, Init, List, R).
foldl(_Pred, Val, [], Val).
foldl(Pred, Val, [H | T], Res) :-
	call(Pred, Val, H, Val1),
	foldl(Pred, Val1, T, Res).

% insertion in a sorted list
insert([], N, [N]).

insert([H | T], N, [N, H|T]) :-
	N =< H, !.

insert([H | T], N, [H|L1]) :-
	insert(T, N, L1).

```

Example use:

```txt
 ?- isort([2,23,42,3,10,1,34,5],L).
L = [1,2,3,5,10,23,34,42]

```



## PureBasic


```PureBasic
Procedure insertionSort(Array a(1))
  Protected low, high
  Protected firstIndex, lastIndex = ArraySize(a())

  If lastIndex > firstIndex + 1
    low = firstIndex + 1
    While low <= lastIndex
      high = low
      While high > firstIndex
        If a(high) < a(high - 1)
          Swap a(high), a(high - 1)
        Else
          Break
        EndIf
        high - 1
      Wend
      low + 1
    Wend
  EndIf
EndProcedure
```



## Python


```python
def insertion_sort(L):
    for i in xrange(1, len(L)):
        j = i-1
        key = L[i]
        while (L[j] > key) and (j >= 0):
           L[j+1] = L[j]
           j -= 1
        L[j+1] = key
```


Using pythonic iterators:


```python
def insertion_sort(L):
    for i, value in enumerate(L):
        for j in range(i - 1, -1, -1):
            if L[j] > value:
                L[j + 1] = L[j]
                L[j] = value
```


### Insertion sort with binary search


```python
def insertion_sort_bin(seq):
    for i in range(1, len(seq)):
        key = seq[i]
        # invariant: ``seq[:i]`` is sorted
        # find the least `low' such that ``seq[low]`` is not less then `key'.
        #   Binary search in sorted sequence ``seq[low:up]``:
        low, up = 0, i
        while up > low:
            middle = (low + up) // 2
            if seq[middle] < key:
                low = middle + 1
            else:
                up = middle
        # insert key at position ``low``
        seq[:] = seq[:low] + [key] + seq[low:i] + seq[i + 1:]
```


This is also built-in to the standard library:


```python
import bisect
def insertion_sort_bin(seq):
    for i in range(1, len(seq)):
        bisect.insort(seq, seq.pop(i), 0, i)
```



## R

Direct translation of pseudocode.

```r
insertionsort <- function(x)
{
   for(i in 2:(length(x)))
   {
      value <- x[i]
      j <- i - 1
      while(j >= 1 && x[j] > value)
      {
         x[j+1] <- x[j]
         j <- j-1
      }
      x[j+1] <- value
   }
   x
}
insertionsort(c(4, 65, 2, -31, 0, 99, 83, 782, 1)) # -31   0   1   2   4  65  83  99 782
```


R has native vectorized operations which allow the following, more efficient implementation.


```r

insertion_sort <- function(x) {
  for (j in 2:length(x)) {
    key <- x[j]
    bp <- which.max(x[1:j] > key)
    # 'bp' stands for breakpoint
    if (bp == 1) {
      if (key < ar[1]){
          x <- c(key, ar[-j])
      }
    }
    else {
      x <- x[-j]
      x <- c(ar[1:bp - 1], key, x[bp : (s-1)])
    }
  return(x)
  }
}

```



## Racket

This implementation makes use of the pattern matching facilities in the Racket distribution.


```racket

#lang racket

(define (sort < l)
  (define (insert x ys)
    (match ys
      [(list) (list x)]
      [(cons y rst) (cond [(< x y) (cons x ys)]
                          [else (cons y (insert x rst))])]))
  (foldl insert '() l))
```



## Rascal


```rascal
import List;

public list[int] insertionSort(a){
	for(i <- [0..size(a)-1]){
		v = a[i];
		j = i-1;
		while(j >= 0 && a[j] > v){
			a[j+1] = a[j];
			j -= 1;
                }
		a[j+1] = v;
        }
	return a;
}
```

{{out}}

```rascal>rascal>rascal
insertionSort([4, 65, 2, -31, 0, 99, 83, 782, 1])
list[int]: [-31,0,1,2,4,65,83,99,782]
```



## REALbasic


```vb
Sub InsertionSort(theList() as Integer)
  for insertionElementIndex as Integer = 1 to UBound(theList)
    dim insertionElement as Integer = theList(insertionElementIndex)
    dim j as Integer = insertionElementIndex - 1
    while (j >= 0) and (insertionElement < theList(j))
      theList(j + 1) = theList(j)
      j = j - 1
    wend
    theList(j + 1) = insertionElement
  next
End Sub
```



## REBOL


```rebol

; This program works with REBOL version R2 and R3, to make it work with Red
; change the word func to function
insertion-sort: func [
	a [block!]
	/local i [integer!] j [integer!] n [integer!]
	value [integer! string! date!]
][
	i: 2
	n: length? a

	while [i <= n][
        	value: a/:i
		j: i
		while [ all [ 	1 < j
				value < a/(j - 1) ]][

	       		a/:j: a/(j - 1)
			j: j - 1
        	]
        	a/:j: value
		i: i + 1
	]
	a
]

probe insertion-sort [4 2 1 6 9 3 8 7]

probe insertion-sort [ "---Monday's Child Is Fair of Face (by Mother Goose)---"
  "Monday's child is fair of face;"
  "Tuesday's child is full of grace;"
  "Wednesday's child is full of woe;"
 "Thursday's child has far to go;"
  "Friday's child is loving and giving;"
  "Saturday's child works hard for a living;"
  "But the child that is born on the Sabbath day"
  "Is blithe and bonny, good and gay."]

; just by adding the date! type to the local variable value the same function can sort dates.
probe insertion-sort [12-Jan-2015 11-Jan-2015 11-Jan-2016 12-Jan-2014]

```


{{out}}

```txt

[1 2 3 4 6 7 8 9]
[{---Monday's Child Is Fair of Face (by Mother Goose)---}
    "But the child that is born on the Sabbath day"
    "Friday's child is loving and giving;"
    "Is blithe and bonny, good and gay."
    "Monday's child is fair of face;"
    "Saturday's child works hard for a living;"
    "Thursday's child has far to go;"
    "Tuesday's child is full of grace;"
    "Wednesday's child is full of woe;"
]
[12-Jan-2014 11-Jan-2015 12-Jan-2015 11-Jan-2016]

```



## REXX


```rexx
/*REXX program sorts a stemmed array (has characters) using the insertion sort algorithm*/
call gen                                         /*generate the array's (data) elements.*/
call show           'before sort'                /*display the  before  array elements. */
say copies('▒', 85)                              /*display a separator line  (a fence). */
call insertionSort  #                            /*invoke the  insertion  sort.         */
call show           ' after sort'                /*display the   after  array elements. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen: @.=;                 @.1  = "---Monday's Child Is Fair of Face  (by Mother Goose)---"
                          @.2  = "
### =================================================
"
                          @.3  = "Monday's child is fair of face;"
                          @.4  = "Tuesday's child is full of grace;"
                          @.5  = "Wednesday's child is full of woe;"
                          @.6  = "Thursday's child has far to go;"
                          @.7  = "Friday's child is loving and giving;"
                          @.8  = "Saturday's child works hard for a living;"
                          @.9  = "But the child that is born on the Sabbath day"
                          @.10 = "Is blithe and bonny, good and gay."
            do #=1  while @.#\==''; end;  #=#-1  /*determine how many entries in @ array*/
     return
/*──────────────────────────────────────────────────────────────────────────────────────*/
insertionSort:  procedure expose @.;    parse arg #
                          do i=2  to #;  $=@.i;         do j=i-1  by -1  to 1  while @.j>$
                                                        _=j+1;    @._=@.j
                                                        end   /*j*/
                          _=j+1;         @._=$
                          end   /*i*/
                return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:  do j=1  for #;  say '   element'  right(j,length(#))  arg(1)": "  @.j; end;  return
```

'''output'''   when using the internal data:

```txt

   element  1 before sort:  ---Monday's Child Is Fair of Face  (by Mother Goose)---
   element  2 before sort:
### =================================================

   element  3 before sort:  Monday's child is fair of face;
   element  4 before sort:  Tuesday's child is full of grace;
   element  5 before sort:  Wednesday's child is full of woe;
   element  6 before sort:  Thursday's child has far to go;
   element  7 before sort:  Friday's child is loving and giving;
   element  8 before sort:  Saturday's child works hard for a living;
   element  9 before sort:  But the child that is born on the Sabbath day
   element 10 before sort:  Is blithe and bonny, good and gay.
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
   element  1  after sort:  ---Monday's Child Is Fair of Face  (by Mother Goose)---
   element  2  after sort:
### =================================================

   element  3  after sort:  But the child that is born on the Sabbath day
   element  4  after sort:  Friday's child is loving and giving;
   element  5  after sort:  Is blithe and bonny, good and gay.
   element  6  after sort:  Monday's child is fair of face;
   element  7  after sort:  Saturday's child works hard for a living;
   element  8  after sort:  Thursday's child has far to go;
   element  9  after sort:  Tuesday's child is full of grace;
   element 10  after sort:  Wednesday's child is full of woe;

```



## Ring


```ring

alist = [7,6,5,9,8,4,3,1,2,0]
see insertionsort(alist)

func insertionsort blist
     for i = 1 to len(blist)
         value = blist[i]
         j = i - 1
         while j >= 1 and blist[j] > value
               blist[j+1] = blist[j]
               j = j - 1
         end
         blist[j+1] = value
      next
      return blist

```



## Ruby


```ruby
class Array
  def insertionsort!
    1.upto(length - 1) do |i|
      value = self[i]
      j = i - 1
      while j >= 0 and self[j] > value
        self[j+1] = self[j]
        j -= 1
      end
      self[j+1] = value
    end
    self
  end
end
ary = [7,6,5,9,8,4,3,1,2,0]
p ary.insertionsort!
# => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```


Alternative version which doesn't swap elements but rather removes and inserts the value at the correct place:

```ruby
class Array
  def insertionsort!
    1.upto(length - 1) do |i|
      value = delete_at i
      j = i - 1
      j -= 1 while j >= 0 && value < self[j]
      insert(j + 1, value)
    end
    self
  end
end

ary = [7,6,5,9,8,4,3,1,2,0]
p ary.insertionsort!
# => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Run BASIC


```runbasic
dim insSort(100)
sortEnd = 0
global inSort
global sortEnd

' -- insert some random numbers --

for i = 1 to 20
  a = int(1000 * rnd(1))
  x = insertSort(a)
next i

' --- Print the Sorted Data -----

print "End Sort:";sortEnd                ' number sorted
for i = 1 to sortEnd
 print i;" ";insSort(i)                  ' location and sorted data
next i
wait

function insertSort(x)                   ' Insert Sort Function
i = 1
while x > insSort(i) and i <= sortEnd
  i = i + 1
wend
for j = sortEnd to i step -1
   insSort(j + 1) = insSort(j)
next j
insSort(i) = x
sortEnd    = sortEnd + 1
end function
```


```txt
End Sort:20
1 124
2 248
3 263
4 279
5 390
6 431
7 458
8 480
9 543
10 556
11 567
12 619
13 625
........
```



## Rust


```rust
fn insertion_sort<T: std::cmp::Ord>(arr: &mut [T]) {
    for i in 1..arr.len() {
        let mut j = i;
        while j > 0 && arr[j] < arr[j-1] {
            arr.swap(j, j-1);
            j = j-1;
        }
    }
}
```



## Scala


### version 1


```scala
def insertSort[X](list: List[X])(implicit ord: Ordering[X]) = {
  def insert(list: List[X], value: X) = list.span(x => ord.lt(x, value)) match {
    case (lower, upper) => lower ::: value :: upper
  }
  list.foldLeft(List.empty[X])(insert)
}
```



### version 2

Copied from SASL manual, Appendix II, answer (2)(a)

```SASL

DEF
sort () = ()
sort (a : x) = insert a (sort x)
insert a () = a,
insert a (b : x) = a < b -> a : b : x
          b : insert a x
?
```



## Scheme


```scheme
(define (insert x lst)
  (if (null? lst)
      (list x)
      (let ((y (car lst))
            (ys (cdr lst)))
        (if (<= x y)
            (cons x lst)
            (cons y (insert x ys))))))

(define (insertion-sort lst)
  (if (null? lst)
      '()
      (insert (car lst)
              (insertion-sort (cdr lst)))))

(insertion-sort '(6 8 5 9 3 2 1 4 7))
```



## Seed7


```seed7
const proc: insertionSort (inout array elemType: arr) is func
  local
    var integer: i is 0;
    var integer: j is 0;
    var elemType: help is elemType.value;
  begin
    for i range 2 to length(arr) do
      j := i;
      help := arr[i];
      while j > 1 and arr[pred(j)] > help do
        arr[j] := arr[pred(j)];
        decr(j);
      end while;
      arr[j] := help;
    end for;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/sorting.htm#insertionSort]


## Sidef


```ruby
class Array {
    method insertion_sort {
        { |i|
            var j = i-1
            var k = self[i]
            while ((j >= 0) && (k < self[j])) {
                self[j+1] = self[j]
                j--
            }
            self[j+1] = k
        } << 1..self.end
        return self
    }
}

var a = 10.of { 100.irand }
say a.insertion_sort
```



## SNOBOL4


```snobol
* read data into an array
	A = table()
	i = 0
readln	A<i = i + 1> = trim(input)	:s(readln)
	aSize = i - 1

* sort array
	i = 1
loop1	value = A<i>
	j = i - 1
loop2	gt(j,0) gt(A<j>,value)	:f(done2)
	A<j + 1> = A<j>
	j = j - 1	:(loop2)
done2	A<j + 1> = value
	i = ?lt(i,aSize) i + 1	:s(loop1)
	i = 1

* output sorted data
while	output = A<i>; i = ?lt(i,aSize) i + 1	:s(while)
end
```


## Stata


```stata
mata
void insertion_sort(real vector a) {
	real scalar i, j, n, x

	n = length(a)
	for (i=2; i<=n; i++) {
		x = a[i]
		for (j=i-1; j>=1; j--) {
			if (a[j] <= x) break
			a[j+1] = a[j]
		}
		a[j+1] = x
	}
}
end
```



## Swift

Using generics.

```Swift>func insertionSort<T:Comparable
(inout list:[T]) {
    for i in 1..<list.count {
        var j = i

        while j > 0 && list[j - 1] > list[j] {
           swap(&list[j], &list[j - 1])
            j--
        }
    }
}
```


=={{header|TI-83 BASIC}}==
Store input in L<sub>1</sub>, run prgmSORTINS, get output in L<sub>2</sub>.
 :L<sub>1</sub>→L<sub>2</sub>
 :0→A
 :Lbl L
 :A+1→A
 :A→B
 :While B>0
 :If L<sub>2</sub>(B)<L<sub>2</sub>(B+1)
 :Goto B
 :L<sub>2</sub>(B)→C
 :L<sub>2</sub>(B+1)→L<sub>2</sub>(B)
 :C→L<sub>2</sub>(B+1)
 :B-1→B
 :End
 :Lbl B
 :If A<(dim(L<sub>2</sub>)-1)
 :Goto L
 :DelVar A
 :DelVar B
 :DelVar C
 :Stop


## Tcl


```tcl
package require Tcl 8.5

proc insertionsort {m} {
    for {set i 1} {$i < [llength $m]} {incr i} {
        set val [lindex $m $i]
        set j [expr {$i - 1}]
        while {$j >= 0 && [lindex $m $j] > $val} {
            lset m [expr {$j + 1}] [lindex $m $j]
            incr j -1
        }
        lset m [expr {$j + 1}] $val
    }
    return $m
}

puts [insertionsort {8 6 4 2 1 3 5 7 9}] ;# => 1 2 3 4 5 6 7 8 9
```


=={{header|TI-83 BASIC}}==
Input into L<sub>1</sub>, run prgmSORTINS, output in L<sub>2</sub>.
 :"INSERTION"
 :L<sub>1</sub>→L<sub>2</sub>
 :0→A
 :Lbl L
 :A+1→A
 :A→B
 :While B>0
 :If L<sub>2</sub>(B)&leq;L<sub>2</sub>(B+1)
 :Goto B
 :L<sub>2</sub>(B)→C
 :L<sub>2</sub>(B+1)→L<sub>2</sub>(B)
 :C→L<sub>2</sub>(B+1)
 :B-1→B
 :End
 :Lbl B
 :If A<(dim(L<sub>2</sub>)-1)
 :Goto L
 :DelVar A
 :DelVar B
 :DelVar C
 :Return


## uBasic/4tH

<lang>PRINT "Insertion sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Insertionsort (n)
  PROC _ShowArray (n)
PRINT

END


_Insertionsort PARAM (1)               ' Insertion sort
  LOCAL (3)

  FOR b@ = 1 TO a@-1
    c@ = @(b@)
    d@ = b@
    DO WHILE (d@>0) * (c@ < @(ABS(d@-1)))
        @(d@) = @(d@-1)
        d@ = d@ - 1
    LOOP
    @(d@) = c@
  NEXT
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


## UnixPipes


```bash
selectionsort() {
   read a
   test -n "$a" && ( selectionsort | sort -nm <(echo $a) -)
}
```


```bash
cat to.sort | selectionsort
```



## Ursala


```Ursala
#import nat

insort = ~&i&& @hNCtX ~&r->lx ^\~&rt nleq-~rlrSPrhlPrSCPTlrShlPNCTPQ@rhPlD
```

test program:

```Ursala
#cast %nL

example = insort <45,82,69,82,104,58,88,112,89,74>
```

{{out}}

```txt

<45,58,69,74,82,82,88,89,104,112>

```


## VBA

{{trans|Phix}}
```vb
Option Base 1
Private Function insertion_sort(s As Variant) As Variant
    Dim temp As Variant
    Dim j As Integer
    For i = 2 To UBound(s)
        temp = s(i)
        j = i - 1
        Do While s(j) > temp
            s(j + 1) = s(j)
            j = j - 1
            If j = 0 Then Exit Do
        Loop
        s(j + 1) = temp
    Next i
    insertion_sort = s
End Function

Public Sub main()
    s = [{4, 15, "delta", 2, -31, 0, "alpha", 19, "gamma", 2, 13, "beta", 782, 1}]
    Debug.Print "Before: ", Join(s, ", ")
    Debug.Print "After: ", Join(insertion_sort(s), "' ")
End Sub
```
{{out}}

```txt
Before:       4, 15, delta, 2, -31, 0, alpha, 19, gamma, 2, 13, beta, 782, 1
After:        -31' 0' 1' 2' 2' 4' 13' 15' 19' 782' alpha' beta' delta' gamma
```


## VBScript

{{trans|REALbasic}}

```vb
Randomize
Dim n(9) 'nine is the upperbound.
         'since VBS arrays are 0-based, it will have 10 elements.
For L = 0 to 9
   n(L) = Int(Rnd * 32768)
Next

WScript.StdOut.Write "ORIGINAL : "
For L = 0 to 9
   WScript.StdOut.Write n(L) & ";"
Next

InsertionSort n

WScript.StdOut.Write vbCrLf & "  SORTED : "
For L = 0 to 9
   WScript.StdOut.Write n(L) & ";"
Next

'the function
Sub InsertionSort(theList)
   For insertionElementIndex = 1 To UBound(theList)
      insertionElement = theList(insertionElementIndex)
      j = insertionElementIndex - 1
      Do While j >= 0
         'necessary for BASICs without short-circuit evaluation
         If insertionElement < theList(j) Then
            theList(j + 1) = theList(j)
            j = j - 1
         Else
            Exit Do
         End If
      Loop
      theList(j + 1) = insertionElement
   Next
End Sub

```

{{Out}}

```txt
ORIGINAL : 26699;2643;10249;31612;21346;19702;29799;31115;20413;5197;
  SORTED : 2643;5197;10249;19702;20413;21346;26699;29799;31115;31612;
```



## XPL0


```XPL0
code ChOut=8, IntOut=11;

proc InsertionSort(A, L);       \Sort array A of length L
int  A, L;
int  I, J, V;
[for I:= 1 to L-1 do
    [V:= A(I);
    J:= I-1;
    while J>=0 and A(J)>V do
        [A(J+1):= A(J);
        J:= J-1;
        ];
    A(J+1):= V;
    ];
];

int A, I;
[A:= [3, 1, 4, 1, -5, 9, 2, 6, 5, 4];
InsertionSort(A, 10);
for I:= 0 to 10-1 do [IntOut(0, A(I));  ChOut(0, ^ )];
]
```


{{out}}

```txt

-5 1 1 2 3 4 4 5 6 9

```



## Yorick

Based on pseudocode, except using 1-based arrays.

```yorick
func insertionSort(&A) {
  for(i = 2; i <= numberof(A); i++) {
    value = A(i);
    j = i - 1;
    while(j >= 1 && A(j) > value) {
      A(j+1) = A(j);
      j--;
    }
    A(j+1) = value;
  }
}
```



## zkl


```zkl
fcn insertionSort(list){
   sink:=List();
   foreach x in (list){
      if(False==(n:=sink.filter1n('>(x)))) sink.append(x); // x>all items in sink
      else sink.insert(n,x);
   }
   sink.close();
}
```


```zkl
insertionSort(T(4,65,2,-31,0,99,2,83,782,1)).println();
insertionSort("big fjords vex quick waltz nymph".split()).println();
```

{{out}}

```txt

L(-31,0,1,2,2,4,65,83,99,782)
L("big","fjords","nymph","quick","vex","waltz")

```

