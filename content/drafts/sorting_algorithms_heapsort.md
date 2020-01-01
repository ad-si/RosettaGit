+++
title = "Sorting algorithms/Heapsort"
description = ""
date = 2019-06-28T09:04:41Z
aliases = []
[extra]
id = 4498
[taxonomies]
categories = []
tags = []
+++

{{task|Sorting Algorithms}} {{Sorting Algorithm}} {{wikipedia|Heapsort}}
{{omit from|GUISS}}



[[wp:Heapsort|Heapsort]] is an in-place sorting algorithm with worst case and average complexity of   <span style="font-family: serif">O(''n'' log''n'')</span>.

The basic idea is to turn the array into a binary heap structure, which has the property that it allows efficient retrieval and removal of the maximal element.

We repeatedly "remove" the maximal element from the heap, thus building the sorted list from back to front.

Heapsort requires random access, so can only be used on an array-like data structure.

Pseudocode:
 '''function''' heapSort(a, count) '''is'''
    '''input:''' an unordered array ''a'' of length ''count''

    <span style="color: grey">''(first place a in max-heap order)''</span>
    heapify(a, count)

    end := count - 1
    '''while''' end > 0 '''do'''
       <span style="color: grey">''(swap the root(maximum value) of the heap with the''
        ''last element of the heap)''</span>
       swap(a[end], a[0])
       <span style="color: grey">''(decrement the size of the heap so that the previous''
        ''max value will stay in its proper place)''</span>
       end := end - 1
       <span style="color: grey">''(put the heap back in max-heap order)''</span>
       siftDown(a, 0, end)


 '''function''' heapify(a,count) '''is'''
    <span style="color: grey">''(start is assigned the index in ''a'' of the last parent node)''</span>
    start := (count - 2) / 2

    '''while''' start ≥ 0 '''do'''
       <span style="color: grey">''(sift down the node at index start to the proper place''
        ''such that all nodes below the start index are in heap''
        ''order)''</span>
       siftDown(a, start, count-1)
       start := start - 1
    <span style="color: grey">''(after sifting down the root all nodes/elements are in heap order)''</span>

 '''function''' siftDown(a, start, end) '''is'''
    <span style="color: grey">''(''end'' represents the limit of how far down the heap to sift)''</span>
    root := start

    '''while''' root * 2 + 1 ≤ end '''do'''       <span style="color: grey">''(While the root has at least one child)''</span>
       child := root * 2 + 1           <span style="color: grey">''(root*2+1 points to the left child)''</span>
       <span style="color: grey">''(If the child has a sibling and the child's value is less than its sibling's...)''</span>
       '''if''' child + 1 ≤ end '''and''' a[child] < a[child + 1] '''then'''
          child := child + 1           <span style="color: grey">''(... then point to the right child instead)''</span>
       '''if''' a[root] < a[child] '''then'''     <span style="color: grey">''(out of max-heap order)''</span>
          swap(a[root], a[child])
          root := child                <span style="color: grey">''(repeat to continue sifting down the child now)''</span>
       '''else'''
          '''return'''



Write a function to sort a collection of integers using heapsort.





## 360 Assembly

{{trans|PL/I}}
The program uses ASM structured macros and two ASSIST macros (XDECO, XPRNT) to keep the code as short as possible.

```360asm
*        Heap sort                     22/06/2016
HEAPS    CSECT
         USING  HEAPS,R13              base register
         B      72(R15)                skip savearea
         DC     17F'0'                 savearea
         STM    R14,R12,12(R13)        prolog
         ST     R13,4(R15)             "
         ST     R15,8(R13)             "
         LR     R13,R15                "
         L      R1,N                   n
         BAL    R14,HEAPSORT           call heapsort(n)
         LA     R3,PG                  pgi=0
         LA     R6,1                   i=1
         DO WHILE=(C,R6,LE,N)          for i=1 to n
         LR     R1,R6                    i
         SLA    R1,2                     .
         L      R2,A-4(R1)               a(i)
         XDECO  R2,XDEC                  edit a(i)
         MVC    0(4,R3),XDEC+8           output a(i)
         LA     R3,4(R3)                 pgi=pgi+4
         LA     R6,1(R6)                 i=i+1
         ENDDO  ,                      end for
         XPRNT  PG,80                  print buffer
         L      R13,4(0,R13)           epilog
         LM     R14,R12,12(R13)        "
         XR     R15,R15                "
         BR     R14                    exit
PG       DC     CL80' '                local data
XDEC     DS     CL12                   "
*------- heapsort(icount)----------------------------------------------
HEAPSORT ST     R14,SAVEHPSR           save return addr
         ST     R1,ICOUNT              icount
         BAL    R14,HEAPIFY            call heapify(icount)
         MVC    IEND,ICOUNT            iend=icount
         DO WHILE=(CLC,IEND,GT,=F'1')  while iend>1
         L      R1,IEND                  iend
         LA     R2,1                     1
         BAL    R14,SWAP                 call swap(iend,1)
         LA     R1,1                     1
         L      R2,IEND                  iend
         BCTR   R2,0                     -1
         ST     R2,IEND                  iend=iend-1
         BAL    R14,SIFTDOWN             call siftdown(1,iend)
         ENDDO  ,                      end while
         L      R14,SAVEHPSR           restore return addr
         BR     R14                    return to caller
SAVEHPSR DS     A                      local data
ICOUNT   DS     F                      "
IEND     DS     F                      "
*------- heapify(count)------------------------------------------------
HEAPIFY  ST     R14,SAVEHPFY           save return addr
         ST     R1,COUNT               count
         SRA    R1,1                   /2
         ST     R1,ISTART              istart=count/2
         DO WHILE=(C,R1,GE,=F'1')      while istart>=1
         L      R1,ISTART                istart
         L      R2,COUNT                 count
         BAL    R14,SIFTDOWN             call siftdown(istart,count)
         L      R1,ISTART                istart
         BCTR   R1,0                     -1
         ST     R1,ISTART                istart=istart-1
         ENDDO  ,                      end while
         L      R14,SAVEHPFY           restore return addr
         BR     R14                    return to caller
SAVEHPFY DS     A                      local data
COUNT    DS     F                      "
ISTART   DS     F                      "
*------- siftdown(jstart,jend)-----------------------------------------
SIFTDOWN ST     R14,SAVESFDW           save return addr
         ST     R1,JSTART              jstart
         ST     R2,JEND                jend
         ST     R1,ROOT                root=jstart
         LR     R3,R1                  root
         SLA    R3,1                   root*2
         DO WHILE=(C,R3,LE,JEND)       while root*2<=jend
         ST     R3,CHILD                 child=root*2
         MVC    SW,ROOT                  sw=root
         L      R1,SW                    sw
         SLA    R1,2                     .
         L      R2,A-4(R1)               a(sw)
         L      R1,CHILD                 child
         SLA    R1,2                     .
         L      R3,A-4(R1)               a(child)
         IF     CR,R2,LT,R3 THEN         if a(sw)<a(child) then
         MVC    SW,CHILD                   sw=child
         ENDIF  ,                        end if
         L      R2,CHILD                 child
         LA     R2,1(R2)                 +1
         L      R1,SW                    sw
         SLA    R1,2                     .
         L      R3,A-4(R1)               a(sw)
         L      R1,CHILD                 child
         LA     R1,1(R1)                 +1
         SLA    R1,2                     .
         L      R4,A-4(R1)               a(child+1)
         IF    C,R2,LE,JEND,AND,         if child+1<=jend and          X
               CR,R3,LT,R4 THEN             a(sw)<a(child+1) then
         L      R2,CHILD                   child
         LA     R2,1(R2)                   +1
         ST     R2,SW                      sw=child+1
         ENDIF  ,                        end if
         IF     CLC,SW,NE,ROOT THEN      if sw^=root then
         L      R1,ROOT                    root
         L      R2,SW                      sw
         BAL    R14,SWAP                   call swap(root,sw)
         MVC    ROOT,SW                    root=sw
         ELSE   ,                        else
         B      RETSFDW                    return
         ENDIF  ,                        end if
         L      R3,ROOT                  root
         SLA    R3,1                     root*2
         ENDDO  ,                      end while
RETSFDW  L      R14,SAVESFDW           restore return addr
         BR     R14                    return to caller
SAVESFDW DS     A                      local data
JSTART   DS     F                      "
ROOT     DS     F                      "
JEND     DS     F                      "
CHILD    DS     F                      "
SW       DS     F                      "
*------- swap(x,y)-----------------------------------------------------
SWAP     SLA    R1,2                   x
         LA     R1,A-4(R1)             @a(x)
         SLA    R2,2                   y
         LA     R2,A-4(R2)             @a(y)
         L      R3,0(R1)               temp=a(x)
         MVC    0(4,R1),0(R2)          a(x)=a(y)
         ST     R3,0(R2)               a(y)=temp
         BR     R14                    return to caller
*------- ------ -------------------------------------------------------
A     DC F'4',F'65',F'2',F'-31',F'0',F'99',F'2',F'83',F'782',F'1'
      DC F'45',F'82',F'69',F'82',F'104',F'58',F'88',F'112',F'89',F'74'
N        DC     A((N-A)/L'A)           number of items
         YREGS
         END    HEAPS
```

{{out}}

```txt

 -31   0   1   2   2   4  45  58  65  69  74  82  82  83  88  89  99 104 112 782

```



## ActionScript


```ActionScript
function heapSort(data:Vector.<int>):Vector.<int> {
	for (var start:int = (data.length-2)/2; start >= 0; start--) {
		siftDown(data, start, data.length);
	}
	for (var end:int = data.length - 1; end > 0; end--) {
		var tmp:int=data[0];
		data[0]=data[end];
		data[end]=tmp;
		siftDown(data, 0, end);
	}
	return data;
}
function siftDown(data:Vector.<int>, start:int, end:int):void {
	var heapRoot:int=start;
	while (heapRoot * 2+1 < end) {
		var child:int=heapRoot*2+1;
		if (child+1<end&&data[child]<data[child+1]) {
			child++;
		}
		if (data[heapRoot]<data[child]) {
			var tmp:int=data[heapRoot];
			data[heapRoot]=data[child];
			data[child]=tmp;
			heapRoot=child;
		} else {
			return;
		}
	}
}
```



## Ada

This implementation is a generic heapsort for unconstrained arrays.

```Ada
generic
   type Element_Type is private;
   type Index_Type is (<>);
   type Collection is array(Index_Type range <>) of Element_Type;
   with function "<" (Left, right : element_type) return boolean is <>;
procedure Generic_Heapsort(Item : in out Collection);
```


```Ada
procedure Generic_Heapsort(Item : in out Collection) is
   procedure Swap(Left : in out Element_Type; Right : in out Element_Type) is
      Temp : Element_Type := Left;
   begin
      Left := Right;
      Right := Temp;
   end Swap;
   procedure Sift_Down(Item : in out Collection) is
      Root : Integer := Index_Type'Pos(Item'First);
      Child : Integer := Index_Type'Pos(Item'Last);
      Last : Integer := Index_Type'Pos(Item'Last);
   begin
      while Root * 2 + 1 <= Last loop
         Child := Root * 2 + 1;
         if Child + 1 <= Last and then Item(index_Type'Val(Child)) < Item(Index_Type'Val(Child + 1)) then
            Child := Child + 1;
         end if;
         if Item(Index_Type'Val(Root)) < Item(Index_Type'Val(Child)) then
            Swap(Item(Index_Type'Val(Root)), Item(Index_Type'Val(Child)));
            Root := Child;
         else
            exit;
         end if;
      end loop;
   end Sift_Down;

   procedure Heapify(Item : in out Collection) is
      First_Pos : Integer := Index_Type'Pos(Index_Type'First);
      Last_Pos  : Integer := Index_Type'Pos(Index_type'Last);
      Start : Index_type := Index_Type'Val((Last_Pos - First_Pos + 1) / 2);
   begin
      loop
         Sift_Down(Item(Start..Item'Last));
         if Start > Index_Type'First then
            Start := Index_Type'Pred(Start);
         else
            exit;
         end if;
      end loop;
   end Heapify;
   Last_Index : Index_Type := Index_Type'Last;
begin
   Heapify(Item);
   while Last_Index > Index_Type'First loop
      Swap(Item(Last_Index), Item(Item'First));
      Last_Index := Index_Type'Pred(Last_Index);
      Sift_Down(Item(Item'First..Last_Index));
   end loop;

end Generic_Heapsort;
```

Demo code:

```Ada
with Generic_Heapsort;
with Ada.Text_Io; use Ada.Text_Io;

procedure Test_Generic_Heapsort is
   type Days is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   type Days_Col is array(Days range <>) of Natural;
   procedure Sort is new Generic_Heapsort(Natural, Days, Days_Col);
   Week : Days_Col := (5, 2, 7, 3, 4, 9, 1);
begin
   for I in Week'range loop
      Put(Days'Image(I) & ":" & Natural'Image(Week(I)) & " ");
   end loop;
   New_Line;
   Sort(Week);
   for I in Week'range loop
      Put(Days'Image(I) & ":" & Natural'Image(Week(I))& " ");
   end loop;
   New_Line;
end Test_Generic_Heapsort;
```



## ALGOL 68


```algol68
#--- Swap function ---#
PROC swap = (REF []INT array, INT first, INT second) VOID:
(
    INT temp := array[first];
    array[first] := array[second];
    array[second]:= temp
);

#--- Heap sort Move Down ---#
PROC heapmove = (REF []INT array, INT i, INT last) VOID:
(
    INT index := i;
    INT larger := (index*2);

    WHILE larger <= last DO
        IF larger < last THEN IF array[larger] < array[larger+1] THEN
            larger +:= 1
        FI FI;
        IF array[index] < array[larger] THEN
            swap(array, index, larger)
        FI;
        index := larger;
        larger := (index*2)
    OD
);

#--- Heap sort ---#
PROC heapsort = (REF []INT array) VOID:
(
    FOR i FROM ENTIER((UPB array) / 2) BY -1 WHILE
        heapmove(array, i, UPB array);
    i > 1 DO SKIP OD;

    FOR i FROM UPB array BY -1 WHILE
        swap(array, 1, i);
        heapmove(array, 1, i-1);
    i > 1 DO SKIP OD
);
#***************************************************************#
main:
(
    [10]INT a;
    FOR i FROM 1 TO UPB a DO
        a[i] := ROUND(random*100)
    OD;

    print(("Before:", a));
    print((newline, newline));
    heapsort(a);
    print(("After: ", a))

)
```

{{out}}

```txt

Before:       +633       +972       +136       +494       +720       +326       +813       +980       +784       +760

After:        +136       +326       +494       +633       +720       +760       +784       +813       +972       +980

```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program heapSort.s   */
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
TableNumber:	     .int   1,3,6,2,5,9,10,8,4,7
#TableNumber:	     .int   10,9,8,7,6,5,4,3,2,1
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
    ldr r0,iAdrTableNumber                      @ address number table
    mov r1,#NBELEMENTS                           @ number of élements
    bl heapSort
    ldr r0,iAdrTableNumber                      @ address number table
    bl displayTable

    ldr r0,iAdrTableNumber                      @ address number table
    mov r1,#NBELEMENTS                           @ number of élements
    bl isSorted                                   @ control sort
    cmp r0,#1                                       @ sorted ?
    beq 2f
    ldr r0,iAdrszMessSortNok                    @ no !! error sort
    bl affichageMess
    b 100f
2:                                                  @ yes
    ldr r0,iAdrszMessSortOk
    bl affichageMess
100:                                               @ standard end of the program
    mov r0, #0                                      @ return code
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
/*         heap sort                                              */
/******************************************************************/
/* r0 contains the address of table */
/* r1 contains the number of element */
heapSort:
    push {r2,r3,r4,lr}                                    @ save registers
    bl heapify                                          @ first place table in max-heap order
    sub r3,r1,#1
1:
    cmp r3,#0
    ble 100f
    mov r1,#0                                             @ swap the root(maximum value) of the heap with the last element of the heap)
    mov r2,r3
    bl swapElement
    sub r3,#1
    mov r1,#0
    mov r2,r3                                             @ put the heap back in max-heap order
    bl siftDown
    b 1b

100:
    pop {r2,r3,r4,lr}
    bx lr                                              @ return
/******************************************************************/
/*      place table in max-heap order                             */
/******************************************************************/
/* r0 contains the address of table */
/* r1 contains the number of element */
heapify:
    push {r1,r2,r3,r4,lr}                                    @ save registers
    mov r4,r1
    sub r3,r1,#2
    lsr r3,#1
1:
    cmp r3,#0
    blt 100f
    mov r1,r3
    sub r2,r4,#1
    bl siftDown
    sub r3,#1
    b 1b
100:
    pop {r1,r2,r3,r4,lr}
    bx lr                                              @ return
/******************************************************************/
/*     swap two elements of table                                  */
/******************************************************************/
/* r0 contains the address of table */
/* r1 contains the first index */
/* r2 contains the second index */
swapElement:
    push {r3,r4,lr}                                    @ save registers
    ldr r3,[r0,r1,lsl #2]                              @ swap number on the table
    ldr r4,[r0,r2,lsl #2]
    str r4,[r0,r1,lsl #2]
    str r3,[r0,r2,lsl #2]

100:
    pop {r3,r4,lr}
    bx lr                                              @ return

/******************************************************************/
/*     put the heap back in max-heap order                        */
/******************************************************************/
/* r0 contains the address of table */
/* r1 contains the first index */
/* r2 contains the last index */
siftDown:
    push {r1-r7,lr}                                    @ save registers
                                                       @ r1 = root = start
    mov r3,r2                                          @ save last index
1:
    lsl r4,r1,#1
    add r4,#1
    cmp r4,r3
    bgt 100f
    add r5,r4,#1
    cmp r5,r3
    bgt 2f
    ldr r6,[r0,r4,lsl #2]                              @ compare elements on the table
    ldr r7,[r0,r5,lsl #2]
    cmp r6,r7
    movlt r4,r5
2:
    ldr r7,[r0,r4,lsl #2]                              @ compare elements on the table
    ldr r6,[r0,r1,lsl #2]                              @ root
    cmp r6,r7
    bge 100f
    mov r2,r4                                          @ and r1 is root
    bl swapElement
    mov r1,r4                                          @ root = child
    b 1b

100:
    pop {r1-r7,lr}
    bx lr                                              @ return

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


```AutoHotkey
heapSort(a) {
    Local end
    end := %a%0
    heapify(a,end)
    While end > 1
        %a%%end% := (%a%1 "", %a%1 := %a%%end%)
       ,siftDown(a, 1, --end)
}

heapify(a, count) {
    Local start
    start := count // 2
    While start
       siftDown(a, start--, count)
}

siftDown(a, start, end) {
    Local child, c1
    While start*2 <= end {
        c1 := 1 + child := start*2
        If (c1 <= end && %a%%child% < %a%%c1%)
            child := c1
        If (%a%%start% < %a%%child%)
            %a%%start% := (%a%%child% "", %a%%child% := %a%%start%)
           ,start := child
        Else Return
    }
}

a = 1,5,2,7,3,4,6,8,1 ; ----- test -----
StringSplit a, a, `,
heapSort("a")
ListVars
MsgBox
```



## BBC BASIC


```bbcbasic
      DIM test(9)
      test() = 4, 65, 2, -31, 0, 99, 2, 83, 782, 1
      PROCheapsort(test())
      FOR i% = 0 TO 9
        PRINT test(i%) ;
      NEXT
      PRINT
      END

      DEF PROCheapsort(a())
      LOCAL e%
      PROCheapify(a())
      FOR e% = DIM(a(),1) TO 1 STEP -1
        SWAP a(e%), a(0)
        PROCsiftdown(a(), 0, e%-1)
      NEXT
      ENDPROC

      DEF PROCheapify(a())
      LOCAL s%, m%
      m% = DIM(a(),1)
      FOR s% = (m% - 1) / 2 TO 0 STEP -1
        PROCsiftdown(a(), s%, m%)
      NEXT
      ENDPROC

      DEF PROCsiftdown(a(), s%, e%)
      LOCAL c%, r%
      r% = s%
      WHILE r% * 2 + 1 <= e%
        c% = r% * 2 + 1
        IF c% + 1 <= e% IF a(c%) < a(c% + 1) c% += 1
        IF a(r%) < a(c%) SWAP a(r%), a(c%) : r% = c% ELSE ENDPROC
      ENDWHILE
      ENDPROC
```

{{out}}

```txt

       -31         0         1         2         2         4        65        83        99       782

```



## BCPL


```BCPL
// This can be run using Cintcode BCPL freely available from www.cl.cam.ac.uk/users/mr10.

GET "libhdr.h"

LET heapify(v, k, i, last) BE
{ LET j = i+i  // If there is a son (or two), j = subscript of first.
  AND x = k    // x will hold the larger of the sons if any.

  IF j<=last DO x := v!j      // j, x = subscript and key of first son.
  IF j< last DO
  { LET y = v!(j+1)           // y = key of the other son.
    IF x<y DO x,j := y, j+1   // j, x = subscript and key of larger son.
  }

  IF k>=x DO
  { v!i := k                  // k is not lower than larger son if any.
    RETURN
  }
  v!i := x
  i := j
} REPEAT

AND heapsort(v, upb) BE
{ FOR i = upb/2 TO 1 BY -1 DO heapify(v, v!i, i, upb)

  FOR i = upb TO 2 BY -1 DO
  { LET k = v!i
    v!i := v!1
    heapify(v, k, 1, i-1)
  }
}

LET start() = VALOF {
  LET v = VEC 1000
  FOR i = 1 TO 1000 DO v!i := randno(1_000_000)
  heapsort(v, 1000)
  FOR i = 1 TO 1000 DO
  { IF i MOD 10 = 0 DO newline()
    writef(" %i6", v!i)
  }
  newline()
}
```



## C


```c
#include <stdio.h>

int max (int *a, int n, int i, int j, int k) {
    int m = i;
    if (j < n && a[j] > a[m]) {
        m = j;
    }
    if (k < n && a[k] > a[m]) {
        m = k;
    }
    return m;
}

void downheap (int *a, int n, int i) {
    while (1) {
        int j = max(a, n, i, 2 * i + 1, 2 * i + 2);
        if (j == i) {
            break;
        }
        int t = a[i];
        a[i] = a[j];
        a[j] = t;
        i = j;
    }
}

void heapsort (int *a, int n) {
    int i;
    for (i = (n - 2) / 2; i >= 0; i--) {
        downheap(a, n, i);
    }
    for (i = 0; i < n; i++) {
        int t = a[n - i - 1];
        a[n - i - 1] = a[0];
        a[0] = t;
        downheap(a, n - i - 1, 0);
    }
}

int main () {
    int a[] = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1};
    int n = sizeof a / sizeof a[0];
    int i;
    for (i = 0; i < n; i++)
        printf("%d%s", a[i], i == n - 1 ? "\n" : " ");
    heapsort(a, n);
    for (i = 0; i < n; i++)
        printf("%d%s", a[i], i == n - 1 ? "\n" : " ");
    return 0;
}

```



## C++

Uses C++11. Compile with
 g++ -std=c++11 heap.cpp

```cpp
#include <algorithm>
#include <iterator>
#include <iostream>

template<typename RandomAccessIterator>
void heap_sort(RandomAccessIterator begin, RandomAccessIterator end) {
  std::make_heap(begin, end);
  std::sort_heap(begin, end);
}

int main() {
  int a[] = {100, 2, 56, 200, -52, 3, 99, 33, 177, -199};
  heap_sort(std::begin(a), std::end(a));
  copy(std::begin(a), std::end(a), std::ostream_iterator<int>(std::cout, " "));
  std::cout << "\n";
}
```

{{out}}

```txt

-199 -52 2 3 33 56 99 100 177 200

```


{{trans|CoffeeScript}}
Uses C++11. Compile with
  g++ -std=c++11

```cpp

#include <iostream>
#include <vector>

using namespace std;

void shift_down(vector<int>& heap,int i, int max) {
    int i_big, c1, c2;
    while(i < max) {
        i_big = i;
        c1 = (2*i) + 1;
        c2 = c1 + 1;
        if( c1<max && heap[c1]>heap[i_big] )
            i_big = c1;
        if( c2<max && heap[c2]>heap[i_big] )
            i_big = c2;
        if(i_big == i) return;
        swap(heap[i],heap[i_big]);
        i = i_big;
    }
}

void to_heap(vector<int>& arr) {
    int i = (arr.size()/2) - 1;
    while(i >= 0) {
        shift_down(arr, i, arr.size());
        --i;
    }
}

void heap_sort(vector<int>& arr) {
    to_heap(arr);
    int end = arr.size() - 1;
    while (end > 0) {
        swap(arr[0], arr[end]);
        shift_down(arr, 0, end);
        --end;
    }
}

int main() {
    vector<int> data = {
        12, 11, 15, 10, 9, 1, 2,
        3, 13, 14, 4, 5, 6, 7, 8
    };
    heap_sort(data);
    for(int i : data) cout << i << " ";
}
```

{{out}}

```txt

1 2 3 4 5 6 7 8 9 10 11 12 13 14 15

```


## C#

```c#
using System;
using System.Collections.Generic;
using System.Text;

public class HeapSortClass
{
    public static void HeapSort<T>(T[] array)
    {
        HeapSort<T>(array, 0, array.Length, Comparer<T>.Default);
    }

    public static void HeapSort<T>(T[] array, int offset, int length, IComparer<T> comparer)
    {
        HeapSort<T>(array, offset, length, comparer.Compare);
    }

    public static void HeapSort<T>(T[] array, int offset, int length, Comparison<T> comparison)
    {
        // build binary heap from all items
        for (int i = 0; i < length; i++)
        {
            int index = i;
            T item = array[offset + i]; // use next item

            // and move it on top, if greater than parent
            while (index > 0 &&
                comparison(array[offset + (index - 1) / 2], item) < 0)
            {
                int top = (index - 1) / 2;
                array[offset + index] = array[offset + top];
                index = top;
            }
            array[offset + index] = item;
        }

        for (int i = length - 1; i > 0; i--)
        {
            // delete max and place it as last
            T last = array[offset + i];
            array[offset + i] = array[offset];

            int index = 0;
            // the last one positioned in the heap
            while (index * 2 + 1 < i)
            {
                int left = index * 2 + 1, right = left + 1;

                if (right < i && comparison(array[offset + left], array[offset + right]) < 0)
                {
                    if (comparison(last, array[offset + right]) > 0) break;

                    array[offset + index] = array[offset + right];
                    index = right;
                }
                else
                {
                    if (comparison(last, array[offset + left]) > 0) break;

                    array[offset + index] = array[offset + left];
                    index = left;
                }
            }
            array[offset + index] = last;
        }
    }

    static void Main()
    {
        // usage
        byte[] r = {5, 4, 1, 2};
        HeapSort(r);

        string[] s = { "-", "D", "a", "33" };
        HeapSort(s, 0, s.Length, StringComparer.CurrentCultureIgnoreCase);
    }
}
```



## Clojure


```lisp
(defn- swap [a i j]
  (assoc a i (nth a j) j (nth a i)))

(defn- sift [a pred k l]
  (loop [a a x k y (inc (* 2 k))]
    (if (< (inc (* 2 x)) l)
      (let [ch (if (and (< y (dec l)) (pred (nth a y) (nth a (inc y))))
                 (inc y)
                 y)]
        (if (pred (nth a x) (nth a ch))
          (recur (swap a x ch) ch (inc (* 2 ch)))
          a))
      a)))

(defn- heapify[pred a len]
  (reduce (fn [c term] (sift (swap c term 0) pred 0 term))
          (reduce (fn [c i] (sift c pred i len))
                  (vec a)
                  (range (dec (int (/ len 2))) -1 -1))
          (range (dec len) 0 -1)))

(defn heap-sort
  ([a pred]
   (let [len (count a)]
     (heapify pred a len)))
  ([a]
     (heap-sort a <)))

```

Example usage:

```lisp>user
 (heapsort [1 2 4 6 2 3 6])
[1 2 2 3 4 6 6]
user> (heapsort [1 2 4 6 2 3 6] >)
[6 6 4 3 2 2 1]
user> (heapsort (list 1 2 4 6 2 3 6))
[1 2 2 3 4 6 6]
```



## COBOL

{{works with|GnuCOBOL}}

```cobol>        >
SOURCE FORMAT FREE
*> This code is dedicated to the public domain
*> This is GNUCOBOL 2.0
identification division.
program-id. heapsort.
environment division.
configuration section.
repository. function all intrinsic.
data division.
working-storage section.
01  filler.
    03  a pic 99.
    03  a-start pic 99.
    03  a-end pic 99.
    03  a-parent pic 99.
    03  a-child pic 99.
    03  a-sibling pic 99.
    03  a-lim pic 99 value 10.
    03  array-swap pic 99.
    03  array occurs 10 pic 99.
procedure division.
start-heapsort.

    *> fill the array
    compute a = random(seconds-past-midnight)
    perform varying a from 1 by 1 until a > a-lim
        compute array(a) = random() * 100
    end-perform

    perform display-array
    display  space 'initial array'

    *>heapify the array
    move a-lim to a-end
    compute a-start = (a-lim + 1) / 2
    perform sift-down varying a-start from a-start by -1 until a-start = 0

    perform display-array
    display space 'heapified'

    *> sort the array
    move 1 to a-start
    move a-lim to a-end
    perform until a-end = a-start
        move array(a-end) to array-swap
        move array(a-start) to array(a-end)
        move array-swap to array(a-start)
        subtract 1 from a-end
        perform sift-down
    end-perform

    perform display-array
    display space 'sorted'

    stop run
    .
sift-down.
    move a-start to a-parent
    perform until a-parent * 2 > a-end
        compute a-child = a-parent * 2
        compute a-sibling = a-child + 1
        if a-sibling <= a-end and array(a-child) < array(a-sibling)
            *> take the greater of the two
            move a-sibling to a-child
        end-if
        if a-child <= a-end and array(a-parent) < array(a-child)
           *> the child is greater than the parent
           move array(a-child) to array-swap
           move array(a-parent) to array(a-child)
           move array-swap to array(a-parent)
        end-if
        *> continue down the tree
        move a-child to a-parent
    end-perform
    .
display-array.
    perform varying a from 1 by 1 until a > a-lim
        display space array(a) with no advancing
    end-perform
    .
end program heapsort.
```

{{out}}

```txt
prompt$ cobc -xj heapsort.cob
 20 26 47 88 97 39 07 77 35 98 initial array
 98 97 47 88 26 39 07 77 35 20 heapified
 07 20 26 35 39 47 77 88 97 98 sorted
```




## CoffeeScript


```coffeescript
# Do an in-place heap sort.
heap_sort = (arr) ->
  put_array_in_heap_order(arr)
  end = arr.length - 1
  while end > 0
    [arr[0], arr[end]] = [arr[end], arr[0]]
    sift_element_down_heap arr, 0, end
    end -= 1

put_array_in_heap_order = (arr) ->
  i = arr.length / 2 - 1
  i = Math.floor i
  while i >= 0
    sift_element_down_heap arr, i, arr.length
    i -= 1

sift_element_down_heap = (heap, i, max) ->
  while i < max
    i_big = i
    c1 = 2*i + 1
    c2 = c1 + 1
    if c1 < max and heap[c1] > heap[i_big]
      i_big = c1
    if c2 < max and heap[c2] > heap[i_big]
      i_big = c2
    return if i_big is i
    [heap[i], heap[i_big]] = [heap[i_big], heap[i]]
    i = i_big

do ->
  arr = [12, 11, 15, 10, 9, 1, 2, 3, 13, 14, 4, 5, 6, 7, 8]
  heap_sort arr
  console.log arr
```

{{out}}

```txt

> coffee heap.coffee
[ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ]

```



## Common Lisp


```lisp
(defun make-heap (&optional (length 7))
  (make-array length :adjustable t :fill-pointer 0))

(defun left-index (index)
  (1- (* 2 (1+ index))))

(defun right-index (index)
  (* 2 (1+ index)))

(defun parent-index (index)
  (floor (1- index) 2))

(defun percolate-up (heap index predicate)
  (if (zerop index) heap
    (do* ((element (aref heap index))
          (index index pindex)
          (pindex (parent-index index)
                  (parent-index index)))
         ((zerop index) heap)
      (if (funcall predicate element (aref heap pindex))
        (rotatef (aref heap index) (aref heap pindex))
        (return-from percolate-up heap)))))

(defun heap-insert (heap element predicate)
  (let ((index (vector-push-extend element heap 2)))
    (percolate-up heap index predicate)))

(defun percolate-down (heap index predicate)
  (let ((length (length heap))
        (element (aref heap index)))
    (flet ((maybe-element (index)
             "return the element at index or nil, and a boolean
              indicating whether there was an element."
             (if (< index length)
               (values (aref heap index) t)
               (values nil nil))))
      (do ((index index swap-index)
           (lindex (left-index index) (left-index index))
           (rindex (right-index index) (right-index index))
           (swap-index nil) (swap-child nil))
          (nil)
        ;; Extact the left child if there is one. If there is not,
        ;; return the heap.  Set the left child as the swap-child.
        (multiple-value-bind (lchild lp) (maybe-element lindex)
          (if (not lp) (return-from percolate-down heap)
            (setf swap-child lchild
                  swap-index lindex))
          ;; Extract the right child, if any, and when better than the
          ;; current swap-child, update the swap-child.
          (multiple-value-bind (rchild rp) (maybe-element rindex)
            (when (and rp (funcall predicate rchild lchild))
              (setf swap-child rchild
                    swap-index rindex))
            ;; If the swap-child is better than element, rotate them,
            ;; and continue percolating down, else return heap.
            (if (not (funcall predicate swap-child element))
              (return-from percolate-down heap)
              (rotatef (aref heap index) (aref heap swap-index)))))))))

(defun heap-empty-p (heap)
  (eql (length heap) 0))

(defun heap-delete-min (heap predicate)
  (assert (not (heap-empty-p heap)) () "Can't pop from empty heap.")
  (prog1 (aref heap 0)
    (setf (aref heap 0) (vector-pop heap))
    (unless (heap-empty-p heap)
      (percolate-down heap 0 predicate))))

(defun heapsort (sequence predicate)
  (let ((h (make-heap (length sequence))))
    (map nil #'(lambda (e) (heap-insert h e predicate)) sequence)
    (map-into sequence #'(lambda () (heap-delete-min h predicate)))))
```

Example usage:

```txt
(heapsort (vector 1 9 2 8 3 7 4 6 5) '<) ; #(1 2 3 4 5 6 7 8 9)
(heapsort (list 9 8 1 2 7 6 3 4 5) '<)   ; (1 2 3 4 5 6 7 8 9)
```



## D


```d
import std.stdio, std.container;

void heapSort(T)(T[] data) /*pure nothrow @safe @nogc*/ {
    for (auto h = data.heapify; !h.empty; h.removeFront) {}
}

void main() {
   auto items = [7, 6, 5, 9, 8, 4, 3, 1, 2, 0];
   items.heapSort;
   items.writeln;
}
```


A lower level implementation:

```d
import std.stdio, std.algorithm;

void heapSort(R)(R seq) pure nothrow @safe @nogc {
   static void siftDown(R seq, in size_t start,
                        in size_t end) pure nothrow @safe @nogc {
      for (size_t root = start; root * 2 + 1 <= end; ) {
         auto child = root * 2 + 1;
         if (child + 1 <= end && seq[child] < seq[child + 1])
            child++;
         if (seq[root] < seq[child]) {
            swap(seq[root], seq[child]);
            root = child;
         } else
            break;
      }
   }

   if (seq.length > 1)
      foreach_reverse (immutable start; 1 .. (seq.length - 2) / 2 + 2)
         siftDown(seq, start - 1, seq.length - 1);

   foreach_reverse (immutable end; 1 .. seq.length) {
      swap(seq[end], seq[0]);
      siftDown(seq, 0, end - 1);
   }
}

void main() {
   auto data = [7, 6, 5, 9, 8, 4, 3, 1, 2, 0];
   data.heapSort;
   data.writeln;
}
```



## Dart


```dart

void heapSort(List a) {
  int count = a.length;

  // first place 'a' in max-heap order
  heapify(a, count);

  int end = count - 1;
  while (end > 0) {
    // swap the root (maximum value) of the heap with the
    // last element of the heap
    int tmp = a[end];
    a[end] = a[0];
    a[0] = tmp;

    // put the heap back in max-heap order
    siftDown(a, 0, end - 1);

    // decrement the size of the heap so that the previous
    // max value will stay in its proper place
    end--;
  }
}



void heapify(List a, int count) {
  // start is assigned the index in 'a' of the last parent node
  int start = ((count - 2)/2).toInt(); // binary heap

  while (start >= 0) {
    // sift down the node at index 'start' to the proper place
    // such that all nodes below the 'start' index are in heap
    // order
    siftDown(a, start, count - 1);
    start--;
  }
}

void siftDown(List a, int start, int end) {
  // end represents the limit of how far down the heap to shift
  int root = start;

  while ((root*2 + 1) <= end) { // While the root has at least one child
    int child = root*2 + 1; // root*2+1 points to the left child
    // if the child has a sibling and the child's value is less than its sibling's...
    if (child + 1 <= end && a[child] < a[child + 1]) {
      child = child+1; // .. then point to the right child instead
    }

    if (a[root] < a[child]) { // out of max-heap order
      int tmp = a[root];
      a[root] = a[child];
      a[child] = tmp;
      root = child; // repeat to continue shifting down the child now
    } else {
      return;
    }
  }

}

void main() {
  var arr=[1,5,2,7,3,9,4,6,8];
  print("Before sort");
  arr.forEach((var i)=>print("$i"));
  heapSort(arr);
  print("After sort");
  arr.forEach((var i)=>print("$i"));
}


```



## E

{{trans|Python}}

```e
def heapsort := {
  def cswap(c, a, b) {
    def t := c[a]
    c[a]  := c[b]
    c[b]  := t
    # println(c)
  }

  def siftDown(array, start, finish) {
    var root := start
    while (var child := root * 2 + 1
           child <= finish) {
      if (child + 1 <= finish && array[child] < array[child + 1]) {
        child += 1
      }
      if (array[root] < array[child]) {
        cswap(array, root, child)
        root := child
      } else {
        break
      }
    }
  }

  /** Heapsort (in-place). */
  def heapsort(array) {
    # in pseudo-code, heapify only called once, so inline it here
    for start in (0..((array.size()-2)//2)).descending() {
      siftDown(array, start, array.size()-1)
    }

    for finish in (0..(array.size()-1)).descending() {
      cswap(array, 0, finish)
      siftDown(array, 0, finish - 1)
    }
  }
}
```


## EasyLang


<lang>subr make_heap
  for i = 1 to n - 1
    if data[i] > data[(i - 1) / 2]
      j = i
      while data[j] > data[(j - 1) / 2]
        swap data[j] data[(j - 1) / 2]
        j = (j - 1) / 2
      .
    .
  .
.
subr sort
  n = len data[]
  call make_heap
  for i = n - 1 downto 1
    swap data[0] data[i]
    j = 0
    ind = 1
    while ind < i
      if ind + 1 < i and data[ind + 1] > data[ind]
        ind += 1
      .
      if data[j] < data[ind]
        swap data[j] data[ind]
      .
      j = ind
      ind = 2 * j + 1
    .
  .
.
data[] = [ 29 4 72 44 55 26 27 77 92 5 ]
call sort
print data[]
```



## EchoLisp

We use the heap library and the '''heap-pop''' primitive to implement heap-sort.

```scheme

(lib 'heap)

(define (heap-sort list)
    (define heap (make-heap < )) ;; make a min heap
    (list->heap list heap)
    (while (not (heap-empty? heap))
          (push 'stack (heap-pop heap)))
    (stack->list 'stack))

(define L (shuffle (iota 15)))
    → (9 4 0 12 8 3 10 7 11 2 5 6 14 13 1)
(heap-sort L)
    → (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)


```



## Eiffel


```Eiffel

class
	HEAPSORT

feature

	sort_array (ar: ARRAY [INTEGER])
			-- Sorts array 'ar' in ascending order.
		require
			not_empty: ar.count > 0
		local
			i, j, r, l, m, n: INTEGER
			sorted: BOOLEAN
		do
			n := ar.count
			j := 0
			i := 0
			m := 0
			r := n
			l := (n // 2)+1
			from
			until
				sorted
			loop
				if l > 1 then
					l := l - 1
					m := ar[l]
				else
					m := ar[r]
					ar[r] := ar[1]
					r := r - 1
					if r = 1 then
						ar[1]:=m
						sorted := True
					end
				end
				if not sorted then
					i := l
					j := l * 2
					from
					until
						j > r
					loop
						if (j < r) and (ar[j] < ar[j + 1]) then
							j := j + 1
						end
						if m < ar[j] then
							ar[i]:= ar[j]
							i := j
							j := j + i
						else
							j := r + 1
						end
					end
					ar[i]:= m
				end
			end
			ensure
				sorted: is_sorted(ar)
		end

feature{NONE}

	is_sorted (ar: ARRAY [INTEGER]): BOOLEAN
			--- Is 'ar' sorted in ascending order?
		local
			i: INTEGER
		do
			Result := True
			from
				i := ar.lower
			until
				i >= ar.upper
			loop
				if ar [i] > ar [i + 1] then
					Result := False
				end
				i := i + 1
			end
		end

end

```

Test:

```Eiffel

class
	APPLICATION

create
	make

feature

	make
		local
			test: ARRAY [INTEGER]
		do
			create test.make_empty
			test := <<5, 91, 13, 99,7, 35>>
			io.put_string ("Unsorted: ")
			across
				test as t
			loop
				io.put_string (t.item.out + " ")
			end
			io.new_line
			create heap_sort
			heap_sort.sort_array (test)
			io.put_string ("Sorted: ")
			across
				test as t
			loop
				io.put_string (t.item.out + " ")
			end
		end

	heap_sort: HEAPSORT

end

```

{{out}}

```txt

Unsorted: 5 91 13 99 7 35
Sorted: 5 7 13 35 91 99

```



## Elixir


```elixir
defmodule Sort do
  def heapSort(list) do
    len = length(list)
    heapify(List.to_tuple(list), div(len - 2, 2))
    |> heapSort(len-1)
    |> Tuple.to_list
  end

  defp heapSort(a, finish) when finish > 0 do
    swap(a, 0, finish)
    |> siftDown(0, finish-1)
    |> heapSort(finish-1)
  end
  defp heapSort(a, _), do: a

  defp heapify(a, start) when start >= 0 do
    siftDown(a, start, tuple_size(a)-1)
    |> heapify(start-1)
  end
  defp heapify(a, _), do: a

  defp siftDown(a, root, finish) when root * 2 + 1 <= finish do
    child = root * 2 + 1
    if child + 1 <= finish and elem(a,child) < elem(a,child + 1), do: child = child + 1
    if elem(a,root) < elem(a,child),
      do:   swap(a, root, child) |> siftDown(child, finish),
      else: a
  end
  defp siftDown(a, _root, _finish), do: a

  defp swap(a, i, j) do
    {vi, vj} = {elem(a,i), elem(a,j)}
    a |> put_elem(i, vj) |> put_elem(j, vi)
  end
end

(for _ <- 1..20, do: :rand.uniform(20)) |> IO.inspect |> Sort.heapSort |> IO.inspect
```


{{out}}

```txt

[6, 1, 12, 3, 7, 7, 9, 20, 8, 15, 2, 10, 14, 5, 19, 7, 20, 9, 14, 19]
[1, 2, 3, 5, 6, 7, 7, 7, 8, 9, 9, 10, 12, 14, 14, 15, 19, 19, 20, 20]

```


=={{header|F Sharp|F#}}==

```fsharp
let inline swap (a: _ []) i j =
  let temp = a.[i]
  a.[i] <- a.[j]
  a.[j] <- temp

let inline sift cmp (a: _ []) start count =
  let rec loop root child =
    if root * 2 + 1 < count then
      let p = child < count - 1 && cmp a.[child] a.[child + 1] < 0
      let child = if p then child + 1 else child
      if cmp a.[root] a.[child] < 0 then
        swap a root child
        loop child (child * 2 + 1)
  loop start (start * 2 + 1)

let inline heapsort cmp (a: _ []) =
  let n = a.Length
  for start = n/2 - 1 downto 0 do
    sift cmp a start n
  for term = n - 1 downto 1 do
    swap a term 0
    sift cmp a 0 term
```



## Forth

This program assumes that return addresses simply reside as a single cell on the Return Stack. Most Forth compilers fulfill this requirement.

```forth
create example
  70 , 61 , 63 , 37 , 63 , 25 , 46 , 92 , 38 , 87 ,

[UNDEFINED] r'@ [IF]
: r'@ r> r> r@ swap >r swap >r ;
[THEN]

defer precedes                         ( n1 n2 a -- f)
defer exchange                         ( n1 n2 a --)

: siftDown                             ( a e s -- a e s)
  swap >r swap >r dup                  ( s r)
  begin                                ( s r)
    dup 2* 1+ dup r'@ <                ( s r c f)
  while                                ( s r c)
    dup 1+ dup r'@ <                   ( s r c c+1 f)
    if                                 ( s r c c+1)
      over over r@ precedes if swap then
    then drop                          ( s r c)
    over over r@ precedes              ( s r c f)
  while                                ( s r c)
    tuck r@ exchange                   ( s r)
  repeat then                          ( s r)
  drop drop r> swap r> swap            ( a e s)
;

: heapsort                             ( a n --)
  over >r                              ( a n)
  dup 1- 1- 2/                         ( a c s)
  begin                                ( a c s)
    dup 0< 0=                          ( a c s f)
  while                                ( a c s)
    siftDown 1-                        ( a c s)
  repeat drop                          ( a c)

  1- 0                                 ( a e 0)
  begin                                ( a e 0)
    over 0>                            ( a e 0 f)
  while                                ( a e 0)
    over over r@ exchange              ( a e 0)
    siftDown swap 1- swap              ( a e 0)
  repeat                               ( a e 0)
  drop drop drop r> drop
;

:noname >r cells r@ + @ swap cells r> + @ swap < ; is precedes
:noname >r cells r@ + swap cells r> + over @ over @ swap rot ! swap ! ; is exchange

: .array 10 0 do example i cells + ? loop cr ;

.array example 10 heapsort .array
```




```forth

\ Written in ANS-Forth; tested under VFX.
\ Requires the novice package: http://www.forth.org/novice.html
\ The following should already be done:
\ include novice.4th

\ This is already in the novice package, so it is not really necessary to compile the code provided here.

\ ******
\ ****** This is our array sort. We are using the heap-sort because it provides consistent times and it is not recursive.
\ ****** This code was ported from C++ at: http://www.snippets.24bytes.com/2010/06/heap-sort.html
\ ****** Our array record size must be a multiple of W. This is assured if FIELD is used for creating the record.
\ ****** The easiest way to speed this up is to rewrite EXCHANGE in assembly language.
\ ******

marker HeapSort.4th

macro: exchange ( adrX adrY size -- )   \ the size of the record must be a multiple of W
    begin  dup while                    \ -- adrX adrY remaining
        over @  fourth @                \ -- adrX adrY remaining Y X
        fourth !  fourth !              \ -- adrX adrY remaining
        rot w +  rot w +  rot w -
        repeat
    3drop ;

\ All of these macros use the locals from SORT, and can only be called from SORT.

macro: adr ( index -- adr )
    recsiz *  array + ;

macro: left ( x -- y )      2*  1+ ;

macro: right ( x -- y )     2*  2 + ;

macro: heapify ( x -- )
    dup >r  begin   \ r: -- great
        dup left    dup limit < if      dup adr  rover adr  'comparer execute if    rdrop  dup >r   then then  drop
        dup right   dup limit < if      dup adr  r@ adr     'comparer execute if    rdrop  dup >r   then then  drop
        dup r@ <> while
            adr  r@ adr  recsiz exchange
            r@ repeat
    drop rdrop ;

macro: build-max-heap ( -- )
    limit 1- 2/  begin  dup 0>= while  dup heapify  1- repeat drop ;

: sort { array limit recsiz 'comparer -- }
    recsiz  [ w 1- ] literal  and  abort" *** SORT: record size must be a multiple of the cell size ***"
    build-max-heap
    begin  limit while  -1 +to limit
        0 adr  limit adr  recsiz exchange
        0 heapify  repeat ;

\ The SORT locals:
\ array             \ the address of the 0th element
\ limit             \ the number of records in the array
\ recsiz            \ the size of a record in the array     \ this must be a multiple of W (FIELD assures this)
\ 'comparer         \ adrX adrY -- X>Y?

\ Note for the novice:
\ This code was originally written with colon words rather than macros, and using items rather than local variables.
\ After it was debugged, it was changed to use macros and locals so that it would be fast and reentrant.
\ One of the reasons why the heap-sort was chosen is because it is not recursive, which allows macros to be used.
\ Using macros allows the data (array, limit, recsiz, 'comparer) to be held in locals rather than items, which is reentrant.


\ ******
\ ****** This tests SORT.
\ ******

create aaa  2 , 9 , 3 , 6 , 1 , 4 , 5 , 7 , 0 , 8 ,

: print-aaa ( limit -- )
    cells aaa +  aaa do  I @ .  w +loop ;

: int> ( adrX adrY -- X>Y? )
    swap @  swap @  > ;

: test-sort ( limit -- )
    cr  dup print-aaa
    aaa  over  w  ['] int>  sort
    cr  print-aaa ;

10 test-sort

```

{{out}}
<pre style="height:8ex;overflow:scroll">
2 9 3 6 1 4 5 7 0 8
0 1 2 3 4 5 6 7 8 9

```



## Fortran

{{works with|Fortran|90 and later}}
Translation of the pseudocode

```fortran
program Heapsort_Demo
  implicit none

  integer, parameter :: num = 20
  real :: array(num)

  call random_seed
  call random_number(array)
  write(*,*) "Unsorted array:-"
  write(*,*) array
  write(*,*)
  call heapsort(array)
  write(*,*) "Sorted array:-"
  write(*,*) array

contains

subroutine heapsort(a)

   real, intent(in out) :: a(0:)
   integer :: start, n, bottom
   real :: temp

   n = size(a)
   do start = (n - 2) / 2, 0, -1
     call siftdown(a, start, n);
   end do

   do bottom = n - 1, 1, -1
     temp = a(0)
     a(0) = a(bottom)
     a(bottom) = temp;
     call siftdown(a, 0, bottom)
   end do

end subroutine heapsort

subroutine siftdown(a, start, bottom)

  real, intent(in out) :: a(0:)
  integer, intent(in) :: start, bottom
  integer :: child, root
  real :: temp

  root = start
  do while(root*2 + 1 < bottom)
    child = root * 2 + 1

    if (child + 1 < bottom) then
      if (a(child) < a(child+1)) child = child + 1
    end if

    if (a(root) < a(child)) then
      temp = a(child)
      a(child) = a (root)
      a(root) = temp
      root = child
    else
      return
    end if
  end do

end subroutine siftdown

end program Heapsort_Demo
```


## FreeBASIC


```freebasic
' version 22-10-2016
' compile with: fbc -s console
' for boundary checks on array's compile with: fbc -s console -exx

' sort from lower bound to the higher bound
' array's can have subscript range from -2147483648 to +2147483647

Sub siftdown(hs() As Long, start As ULong, end_ As ULong)
    Dim As ULong root = start
    Dim As Long lb = LBound(hs)

    While root * 2 + 1 <= end_
        Dim As ULong child = root * 2 + 1
        If (child + 1 <= end_) AndAlso (hs(lb + child) < hs(lb + child + 1)) Then
            child = child + 1
        End If
        If hs(lb + root) < hs(lb + child) Then
            Swap hs(lb + root), hs(lb + child)
            root = child
        Else
            Return
        End If
    Wend
End Sub

Sub heapsort(hs() As Long)
    Dim As Long lb = LBound(hs)
    Dim As ULong count = UBound(hs) - lb + 1
    Dim As Long start = (count - 2) \ 2
    Dim As ULong end_ = count - 1

    While start >= 0
        siftdown(hs(), start, end_)
        start = start - 1
    Wend

    While end_ > 0
        Swap hs(lb + end_), hs(lb)
        end_ = end_ - 1
        siftdown(hs(), 0, end_)
    Wend
End Sub

' ------=< MAIN >=------

Dim As Long array(-7 To 7)
Dim As Long i, lb = LBound(array), ub = UBound(array)

Randomize Timer
For i = lb To ub : array(i) = i : Next
For i = lb To ub
    Swap array(i), array(Int(Rnd * (ub - lb + 1)) + lb)
Next

Print "Unsorted"
For i = lb To ub
    Print Using " ###"; array(i);
Next : Print : Print

heapsort(array())

Print "After heapsort"
For i = lb To ub
    Print Using " ###"; array(i);
Next : Print

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Unsorted
   0   3  -6   2   1  -4   7   5   6  -3   4  -7  -1  -5  -2

After heapsort
  -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7
```



## FunL

Direct translation of the pseudocode.  The array object (using Scala's <code>ArraySeq</code> class) has built-in method <code>length</code>, so the <code>count</code> parameter is not needed.


```funl
def heapSort( a ) =
  heapify( a )
  end = a.length() - 1

  while end > 0
    a(end), a(0) = a(0), a(end)
    siftDown( a, 0, --end )

def heapify( a ) =
  for i <- (a.length() - 2)\2..0 by -1
    siftDown( a, i, a.length() - 1 )

def siftDown( a, start, end ) =
  root = start

  while root*2 + 1 <= end
    child = root*2 + 1

    if child + 1 <= end and a(child) < a(child + 1)
      child++

    if a(root) < a(child)
      a(root), a(child) = a(child), a(root)
      root = child
    else
      break

a = array( [7, 2, 6, 1, 9, 5, 0, 3, 8, 4] )
heapSort( a )
println( a )
```


{{out}}


```txt

ArraySeq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

```



## Go

Here's an ingenious solution that makes use of the heap module. Although the heap module usually implements an independent heap with push/pop operations, we use a helper type where the "pop" operation does not actually change the size of the underlying container, but changes a "heap length" variable indicating the length of the prefix of the underlying container that is considered "the heap".

Since we want to implement a generic algorithm, we accept an argument of type <code>sort.Interface</code>, and thus do not have access to the actual elements of the container we're sorting. We can only swap elements. This causes a problem for us when implementing the <code>Pop</code> method, as we can't actually return an element. The ingenious step is realizing that <code>heap.Pop()</code> must move the value to pop to the "end" of the heap area, because its interface only has access to a "Swap" function, and a "Pop" function that pops from the end. (It does not have the ability to pop a value at the beginning.) This is perfect because we precisely want to move the thing popped to the end and shrink the "heap area" by 1. Our "Pop" function returns nothing since we can't get the value, but don't actually need it. (We only need the swapping that it does for us.)

```go
package main

import (
  "sort"
  "container/heap"
  "fmt"
)

type HeapHelper struct {
    container sort.Interface
    length    int
}

func (self HeapHelper) Len() int { return self.length }
// We want a max-heap, hence reverse the comparison
func (self HeapHelper) Less(i, j int) bool { return self.container.Less(j, i) }
func (self HeapHelper) Swap(i, j int) { self.container.Swap(i, j) }
// this should not be called
func (self *HeapHelper) Push(x interface{}) { panic("impossible") }
func (self *HeapHelper) Pop() interface{} {
    self.length--
    return nil // return value not used
}

func heapSort(a sort.Interface) {
    helper := HeapHelper{ a, a.Len() }
    heap.Init(&helper)
    for helper.length > 0 {
        heap.Pop(&helper)
    }
}

func main() {
    a := []int{170, 45, 75, -90, -802, 24, 2, 66}
    fmt.Println("before:", a)
    heapSort(sort.IntSlice(a))
    fmt.Println("after: ", a)
}
```

{{out}}

```txt

before: [170 45 75 -90 -802 24 2 66]
after:  [-802 -90 2 24 45 66 75 170]

```

If you want to implement it manually:

```go
package main

import (
  "sort"
  "fmt"
)

func main() {
    a := []int{170, 45, 75, -90, -802, 24, 2, 66}
    fmt.Println("before:", a)
    heapSort(sort.IntSlice(a))
    fmt.Println("after: ", a)
}

func heapSort(a sort.Interface) {
    for start := (a.Len() - 2) / 2; start >= 0; start-- {
        siftDown(a, start, a.Len()-1)
    }
    for end := a.Len() - 1; end > 0; end-- {
        a.Swap(0, end)
        siftDown(a, 0, end-1)
    }
}


func siftDown(a sort.Interface, start, end int) {
    for root := start; root*2+1 <= end; {
        child := root*2 + 1
        if child+1 <= end && a.Less(child, child+1) {
            child++
        }
        if !a.Less(root, child) {
            return
        }
        a.Swap(root, child)
        root = child
    }
}
```



## Groovy

Loose translation of the pseudocode:

```groovy
def makeSwap = { a, i, j = i+1 -> print "."; a[[j,i]] = a[[i,j]] }

def checkSwap = { list, i, j = i+1 -> [(list[i] > list[j])].find{ it }.each { makeSwap(list, i, j) } }

def siftDown = { a, start, end ->
    def p = start
    while (p*2 < end) {
        def c = p*2 + ((p*2 + 1 < end && a[p*2 + 2] > a[p*2 + 1]) ? 2 : 1)
        if (checkSwap(a, c, p)) { p = c }
        else                    { return }
    }
}

def heapify = {
    (((it.size()-2).intdiv(2))..0).each { start -> siftDown(it, start, it.size()-1) }
}

def heapSort = { list ->
    heapify(list)
    (0..<(list.size())).reverse().each { end ->
        makeSwap(list, 0, end)
        siftDown(list, 0, end-1)
    }
    list
}
```

Test:

```groovy
println (heapSort([23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4]))
println (heapSort([88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1]))
```

{{out}}

```txt
.......................................................................[4, 12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]
..........................................................................................[0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]
```



## Haskell

Using package [http://hackage.haskell.org/package/fgl fgl] from HackageDB

```haskell
import Data.Graph.Inductive.Internal.Heap(
  Heap(..),insert,findMin,deleteMin)

-- heapsort is added in this module as an example application

build :: Ord a => [(a,b)] -> Heap a b
build = foldr insert Empty

toList :: Ord a => Heap a b -> [(a,b)]
toList Empty = []
toList h = x:toList r
           where (x,r) = (findMin h,deleteMin h)

heapsort :: Ord a => [a] -> [a]
heapsort = (map fst) . toList . build . map (\x->(x,x))
```

e.g.

```haskell
*Main> heapsort [[6,9],[2,13],[6,8,14,9],[10,7],[5]]
[[2,13],[5],[6,8,14,9],[6,9],[10,7]]
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()                     #: demonstrate various ways to sort a list and string
   demosort(heapsort,[3, 14, 1, 5, 9, 2, 6, 3],"qwerty")
end

procedure heapsort(X,op)                            #: return sorted list ascending(or descending)
local head,tail

   op := sortop(op,X)                               # select how and what we sort

   every head := (tail := *X) / 2  to 1 by -1 do    # work back from from last parent node
      X := siftdown(X,op,head,tail)                 # sift down from head to make the heap

   every tail := *X to 2 by -1 do {                 # work between the beginning and the tail to final positions
      X[1] :=: X[tail]
      X := siftdown(X,op,1,tail-1)                  # re-sift next (previous) branch after shortening the heap
      }

   return X
end

procedure siftdown(X,op,root,tail)                  #: the value @root is moved "down" the path of max(min) value to its level
local child

   while (child :=  root * 2) <= tail do {          # move down the branch from root to tail

      if op(X[child],X[tail >= child + 1]) then     # choose the larger(smaller)
         child +:= 1                                # ... child

      if op(X[root],X[child]) then  {               # root out of order?
         X[child] :=: X[root]
         root := child                              # follow max(min) branch
         }
      else
         return X
      }
   return X
end
```

Algorithm notes:
* This is a fairly straight forward implementation of the pseudo-code with 'heapify' coded in-line.
Implementation notes:
* Since this transparently sorts both string and list arguments the result must 'return' to bypass call by value (strings)
* Beware missing trailing 'returns' when translating pseudo-code.  For amusement try comment out the return at the end of 'shiftdown'

Note: This example relies on [[Sorting_algorithms/Bubble_sort#Icon| the supporting procedures 'sortop', and 'demosort' in Bubble Sort]].
The full demosort exercises the named sort of a list with op = "numeric", "string", ">>" (lexically gt, descending),">" (numerically gt, descending), a custom comparator, and also a string.
{{out|Abbreviated sample output}}

```txt
Sorting Demo using procedure heapsort
  on list : [ 3 14 1 5 9 2 6 3 ]
    with op = &null:         [ 1 2 3 3 5 6 9 14 ]   (0 ms)
  ...
  on string : "qwerty"
    with op = &null:         "eqrtwy"   (0 ms)
```



## J

{{eff note|J|/:~}}
'''Translation of the pseudocode'''

```j
swap=: C.~ <

siftDown=: 4 : 0
  'c e'=. x
  while. e > c=.1+2*s=.c do.
    before=. <&({&y)
    if. e > 1+c do. c=.c+ c before c+1 end.
    if. s before c do. y=. y swap c,s else. break. end.
  end.
  y
)

heapSort=: 3 : 0
  if. 1>: c=. # y do. y return. end.
  z=. siftDown&.>/ (c,~each i.<.c%2),<y        NB. heapify
  > ([ siftDown swap~)&.>/ (0,each}.i.c),z
)
```

'''Examples'''

```j
   heapSort 1 5 2 7 3 9 4 6 8 1
1 1 2 3 4 5 6 7 8 9

   heapSort &. (a.&i.) 'aqwcdhkij'
acdhijkqw
```



## Java

Direct translation of the pseudocode.

```java
public static void heapSort(int[] a){
	int count = a.length;

	//first place a in max-heap order
	heapify(a, count);

	int end = count - 1;
	while(end > 0){
		//swap the root(maximum value) of the heap with the
		//last element of the heap
		int tmp = a[end];
		a[end] = a[0];
		a[0] = tmp;
		//put the heap back in max-heap order
		siftDown(a, 0, end - 1);
		//decrement the size of the heap so that the previous
		//max value will stay in its proper place
		end--;
	}
}

public static void heapify(int[] a, int count){
	//start is assigned the index in a of the last parent node
	int start = (count - 2) / 2; //binary heap

	while(start >= 0){
		//sift down the node at index start to the proper place
		//such that all nodes below the start index are in heap
		//order
		siftDown(a, start, count - 1);
		start--;
	}
	//after sifting down the root all nodes/elements are in heap order
}

public static void siftDown(int[] a, int start, int end){
	//end represents the limit of how far down the heap to sift
	int root = start;

	while((root * 2 + 1) <= end){      //While the root has at least one child
		int child = root * 2 + 1;           //root*2+1 points to the left child
		//if the child has a sibling and the child's value is less than its sibling's...
		if(child + 1 <= end && a[child] < a[child + 1])
			child = child + 1;           //... then point to the right child instead
		if(a[root] < a[child]){     //out of max-heap order
			int tmp = a[root];
			a[root] = a[child];
			a[child] = tmp;
			root = child;                //repeat to continue sifting down the child now
		}else
			return;
	}
}
```



## Javascript

{{trans|CoffeeScript}}

```Javascript

function swap(data, i, j) {
    var tmp = data[i];
    data[i] = data[j];
    data[j] = tmp;
}

 function heap_sort(arr) {
    put_array_in_heap_order(arr);
    var end = arr.length - 1;
    while(end > 0) {
        swap(arr, 0, end);
        sift_element_down_heap(arr, 0, end);
        end -= 1
    }
}

function put_array_in_heap_order(arr) {
    var i;
    i = arr.length / 2 - 1;
    i = Math.floor(i);
    while (i >= 0) {
        sift_element_down_heap(arr, i, arr.length);
        i -= 1;
    }
}

function sift_element_down_heap(heap, i, max) {
    var i_big, c1, c2;
    while(i < max) {
        i_big = i;
        c1 = 2*i + 1;
        c2 = c1 + 1;
        if (c1 < max && heap[c1] > heap[i_big])
            i_big = c1;
        if (c2 < max && heap[c2] > heap[i_big])
            i_big = c2;
        if (i_big == i) return;
        swap(heap,i, i_big);
        i = i_big;
    }
}

arr = [12, 11, 15, 10, 9, 1, 2, 3, 13, 14, 4, 5, 6, 7, 8,];
heap_sort(arr);
alert(arr);
```

{{out}}

```txt

    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

```




## Julia


```julia
swapa(a, i, j) = begin a[i], a[j] = a[j], a[i] end

function pd!(a, first, last)
    while (c = 2 * first - 1) < last
        if c < last && a[c] < a[c + 1]
            c += 1
        end
        if a[first] < a[c]
            swapa(a, c, first)
            first = c
        else
            break
        end
    end
end

hfy!(a, n) = (f = div(n, 2); while f >= 1 pd!(a, f, n); f -= 1 end)

heapsort!(a) = (n = length(a); hfy!(a, n); l = n; while l > 1 swapa(a, 1, l); l -= 1; pd!(a, 1, l) end; a)

a = shuffle(collect(1:12))
println("Unsorted: $a")
println("Heap sorted: ", heapsort!(a))

```
{{output}}
```txt

 Unsorted: [3, 12, 11, 4, 2, 7, 5, 8, 9, 1, 10, 6]
 Heap sorted: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]

```



## Kotlin


```scala
// version 1.1.0

fun heapSort(a: IntArray) {
    heapify(a)
    var end = a.size - 1
    while (end > 0) {
        val temp = a[end]
        a[end] = a[0]
        a[0] = temp
        end--
        siftDown(a, 0, end)
    }
}

fun heapify(a: IntArray) {
    var start = (a.size - 2) / 2
    while (start >= 0) {
        siftDown(a, start, a.size - 1)
        start--
    }
}

fun siftDown(a: IntArray, start: Int, end: Int) {
    var root = start
    while (root * 2 + 1 <= end) {
        var child = root * 2 + 1
        if (child + 1 <= end && a[child] < a[child + 1]) child++
        if (a[root] < a[child]) {
            val temp = a[root]
            a[root] = a[child]
            a[child] = temp
            root = child
        }
        else return
    }
}

fun main(args: Array<String>) {
    val aa = arrayOf(
        intArrayOf(100, 2, 56, 200, -52, 3, 99, 33, 177, -199),
        intArrayOf(4, 65, 2, -31, 0, 99, 2, 83, 782, 1),
        intArrayOf(12, 11, 15, 10, 9, 1, 2, 3, 13, 14, 4, 5, 6, 7, 8)
    )
    for (a in aa) {
        heapSort(a)
        println(a.joinToString(", "))
    }
}
```


{{out}}

```txt

-199, -52, 2, 3, 33, 56, 99, 100, 177, 200
-31, 0, 1, 2, 2, 4, 65, 83, 99, 782
1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15

```



## Liberty BASIC


```lb
wikiSample=1    'comment out for random array

data 6, 5, 3, 1, 8, 7, 2, 4
    itemCount = 20
if wikiSample then itemCount = 8
    dim A(itemCount)
    for i = 1 to itemCount
        A(i) = int(rnd(1) * 100)
        if wikiSample then read tmp: A(i)=tmp
    next i

    print "Before Sort"
    call printArray itemCount

    call heapSort itemCount

    print "After Sort"
    call printArray itemCount
end

'------------------------------------------
sub heapSort count
    call heapify count

    print "the heap"
    call printArray  count

    theEnd = count
    while theEnd > 1
        call swap theEnd, 1
        call siftDown 1, theEnd-1
        theEnd = theEnd - 1
    wend
end sub

sub heapify count
    start = int(count / 2)
    while start >= 1
         call siftDown start, count
         start = start - 1
    wend
end sub

sub siftDown start, theEnd
    root = start
    while root * 2 <= theEnd
        child = root * 2
        swap = root
        if A(swap) < A(child) then
            swap = child
        end if
        if child+1 <= theEnd  then
            if A(swap) < A(child+1) then
                swap = child + 1
            end if
        end if
        if swap <> root then
            call swap root, swap
            root = swap
        else
            exit sub
        end if
    wend
end sub

sub swap a,b
    tmp = A(a)
    A(a) = A(b)
    A(b) = tmp
end sub

'
### =====================================

sub printArray itemCount
    for i = 1 to itemCount
        print using("###", A(i));
    next i
    print
end sub
```




## LotusScript



```LotusScript

Public Sub heapsort(pavIn As Variant)
  Dim liCount As Integer, liEnd As Integer
  Dim lvTemp As Variant
  liCount = UBound(pavIn) + 1

  heapify pavIn, liCount

  liEnd = liCount - 1
  While liEnd > 0
    lvTemp = pavIn(liEnd)
    pavIn(liEnd) = pavIn(0)
    pavIn(0) = lvTemp
    liEnd = liEnd -1
    siftDown pavIn,0, liEnd
  Wend
End Sub

Private Sub heapify(pavIn As Variant,piCount As Integer)
  Dim liStart As Integer
  liStart = (piCount - 2) / 2
  While liStart >=0
    siftDown pavIn, liStart, piCount -1
    liStart = liStart - 1
  Wend
End Sub

Private Sub siftDown(pavIn As Variant, piStart As Integer, piEnd As Integer)
  Dim liRoot As Integer, liChild As Integer
  Dim lvTemp As Variant
  liRoot = piStart
  While liRoot *2 + 1 <= piEnd
    liChild = liRoot *2 + 1
    If liChild +1 <= piEnd And pavIn(liChild) < pavIn(liChild + 1) Then
      liChild = liChild + 1
    End If
    If pavIn(liRoot) < pavIn(liChild) Then
      lvTemp = pavIn(liRoot)
      pavIn(liRoot) = pavIn(liChild)
      pavIn(liChild) = lvTemp
      liRoot = liChild
    Else
      Exit sub
    End if
  wend
End Sub

```



## M4


```M4
divert(-1)

define(`randSeed',141592653)
define(`setRand',
   `define(`randSeed',ifelse(eval($1<10000),1,`eval(20000-$1)',`$1'))')
define(`rand_t',`eval(randSeed^(randSeed>>13))')
define(`random',
   `define(`randSeed',eval((rand_t^(rand_t<<18))&0x7fffffff))randSeed')

define(`set',`define(`$1[$2]',`$3')')
define(`get',`defn(`$1[$2]')')
define(`new',`set($1,size,0)')
dnl  for the heap calculations, it's easier if origin is 0, so set value first
define(`append',
   `set($1,get($1,size),$2)`'set($1,size,incr(get($1,size)))')

dnl  swap(<name>,<j>,<name>[<j>],<k>)  using arg stack for the temporary
define(`swap',`set($1,$2,get($1,$4))`'set($1,$4,$3)')

define(`deck',
   `new($1)for(`x',1,$2,
         `append(`$1',eval(random%100))')')
define(`show',
   `for(`x',0,decr(get($1,size)),`get($1,x) ')')
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',incr($2),$3,`$4')')')')

define(`ifywork',
   `ifelse(eval($2>=0),1,
      `siftdown($1,$2,$3)`'ifywork($1,decr($2),$3)')')
define(`heapify',
   `define(`start',eval((get($1,size)-2)/2))`'ifywork($1,start,
      decr(get($1,size)))')
define(`siftdown',
   `define(`child',eval($2*2+1))`'ifelse(eval(child<=$3),1,
       `ifelse(eval(child+1<=$3),1,
       `ifelse(eval(get($1,child)<get($1,incr(child))),1,
       `define(`child',
           incr(child))')')`'ifelse(eval(get($1,$2)<get($1,child)),1,
       `swap($1,$2,get($1,$2),child)`'siftdown($1,child,$3)')')')
define(`sortwork',
   `ifelse($2,0,
      `',
      `swap($1,0,get($1,0),$2)`'siftdown($1,0,decr($2))`'sortwork($1,
            decr($2))')')

define(`heapsort',
   `heapify($1)`'sortwork($1,decr(get($1,size)))')

divert
deck(`a',10)
show(`a')
heapsort(`a')
show(`a')
```


## Maple

<lang>swap := proc(arr, a, b)
	local temp:
	temp := arr[a]:
	arr[a] := arr[b]:
	arr[b] := temp:
end proc:
heapify := proc(toSort, n, i)
	local largest, l, r, holder:
	largest := i:
	l := 2*i:
	r := 2*i+1:
	if (l <= n and toSort[l] > toSort[largest]) then
		largest := l:
	end if:
	if (r <= n and toSort[r] > toSort[largest]) then
		largest := r:
	end if:
	if (not largest = i) then
		swap(toSort, i, largest);
		heapify(toSort, n, largest):
	end if:
end proc:
heapsort := proc(arr)
	local n,i:
	n := numelems(arr):
	for i from trunc(n/2) to 1 by -1 do
		heapify(arr, n, i):
	end do:
	for i from n to 2 by -1 do
		swap(arr, 1, i):
		heapify(arr, i-1, 1):
	end do:
end proc:
arr := Array([17,3,72,0,36,2,3,8,40,0]);
heapsort(arr);
arr;
```

{{Out|Output}}

```txt
[0,0,2,3,3,8,17,36,40,72]
```



## Mathematica


```Mathematica
siftDown[list_,root_,theEnd_]:=
 While[(root*2) <= theEnd,
  child = root*2;
  If[(child+1 <= theEnd)&&(list[[child]] < list[[child+1]]), child++;];
  If[list[[root]] < list[[child]],
   list[[{root,child}]] = list[[{child,root}]]; root = child;,
   Break[];
  ]
 ]

heapSort[list_] := Module[{ count, start},
 count = Length[list]; start = Floor[count/2];
 While[start >= 1,list = siftDown[list,start,count];
  start--;
 ]
 While[count > 1, list[[{count,1}]] = list[[{1,count}]];
  count--; list = siftDown[list,1,count];
 ]
]
```

{{out}}

```txt
heapSort@{2,3,1,5,7,6}
{1,2,3,5,6,7}
```


=={{header|MATLAB}} / {{header|Octave}}==
This function definition is an almost exact translation of the pseudo-code into MATLAB, but I have chosen to make the heapify function inline because it is only called once in the pseudo-code. Also, MATLAB uses 1 based array indecies, therefore all of the pseudo-code has been translated to reflect that difference.

```MATLAB
function list = heapSort(list)

    function list = siftDown(list,root,theEnd)
        while (root * 2) <= theEnd

            child = root * 2;
            if (child + 1 <= theEnd) && (list(child) < list(child+1))
                child = child + 1;
            end

            if list(root) < list(child)
                list([root child]) = list([child root]); %Swap
                root = child;
            else
                return
            end

        end %while
    end %siftDown

    count = numel(list);

    %Because heapify is called once in pseudo-code, it is inline here
    start = floor(count/2);

    while start >= 1
        list = siftDown(list, start, count);
        start = start - 1;
    end
    %End Heapify

    while count > 1

        list([count 1]) = list([1 count]); %Swap
        count = count - 1;
        list = siftDown(list,1,count);

    end

end
```

Sample Usage:

```MATLAB>>
 heapSort([4 3 1 5 6 2])

ans =

     1     2     3     4     5     6
```



## MAXScript


```MAXScript
fn heapify arr count =
(
	local s = count /2
	while s > 0 do
	(
		arr = siftDown arr s count
		s -= 1
	)
	return arr
)
fn siftDown arr s end =
(
	local root = s
	while root * 2 <= end do
	(
		local child = root * 2
		if child < end and arr[child] < arr[child+1] do
		(
			child += 1
		)
		if arr[root] < arr[child] then
		(
			swap arr[root] arr[child]
			root = child
		)
		else return arr
	)
	return arr
)
fn heapSort arr =
(
	local count = arr.count
	arr = heapify arr count
	local end = count
	while end >= 1 do
	(
		swap arr[1] arr[end]

		end -= 1
		arr = siftDown arr 1 end
	)

)
```

Output:

```MAXScript

a = for i in 1 to 10 collect random 0 9
#(7, 2, 5, 6, 1, 5, 4, 0, 1, 6)
heapSort a
#(0, 1, 1, 2, 4, 5, 5, 6, 6, 7)

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
  , heapSort(String[] Arrays.copyOf(placesList, placesList.length)) -
]

loop ln = 0 to lists.length - 1
  cl = lists[ln]
  loop ct = 0 to cl.length - 1
    say cl[ct]
    end ct
    say
  end ln

return

method heapSort(a = String[], count = a.length) public constant binary returns String[]


  rl = String[a.length]
  al = List heapSort(Arrays.asList(a), count)
  al.toArray(rl)

  return rl

method heapSort(a = List, count = a.size) public constant binary returns ArrayList

  a = heapify(a, count)

  iend = count - 1
  loop label iend while iend > 0
    swap = a.get(0)
    a.set(0, a.get(iend))
    a.set(iend, swap)
    a = siftDown(a, 0, iend - 1)
    iend = iend - 1
    end iend

  return ArrayList(a)

method heapify(a = List, count = int) public constant binary returns List

  start = (count - 2) % 2

  loop label strt while start >= 0
    a = siftDown(a, start, count - 1)
    start = start - 1
    end strt

  return a

method siftDown(a = List, istart = int, iend = int) public constant binary returns List

  root = istart

  loop label root while root * 2 + 1 <= iend
    child = root * 2 + 1
    if child + 1 <= iend then do
      if (Comparable a.get(child)).compareTo(Comparable a.get(child + 1)) < 0 then do
        child = child + 1
        end
      end
    if (Comparable a.get(root)).compareTo(Comparable a.get(child)) < 0 then do
      swap = a.get(root)
      a.set(root, a.get(child))
      a.set(child, swap)
      root = child
      end
    else do
      leave root
      end
    end root

  return a
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
proc siftDown[T](a: var openarray[T]; start, ending: int) =
  var root = start
  while root * 2 + 1 < ending:
    var child = 2 * root + 1
    if child + 1 < ending and a[child] < a[child+1]:
      inc child
    if a[root] < a[child]:
      swap a[child], a[root]
      root = child
    else:
      return

proc heapSort[T](a: var openarray[T]) =
  let count = a.len
  for start in countdown((count - 2) div 2, 0):
    siftDown(a, start, count)
  for ending in countdown(count - 1, 1):
    swap a[ending], a[0]
    siftDown(a, 0, ending)

var a = @[4, 65, 2, -31, 0, 99, 2, 83, 782]
heapSort a
echo a
```

{{out}}

```txt
@[-31, 0, 2, 2, 4, 65, 83, 99, 782]
```



## Objeck

{{trans|Java}}

```objeck
bundle Default {
  class HeapSort {
    function : Main(args : String[]) ~ Nil {
      values := [4, 3, 1, 5, 6, 2];
      HeapSort(values);
      each(i : values) {
        values[i]->PrintLine();
      };
    }

    function : HeapSort(a : Int[]) ~ Nil {
      count := a->Size();
      Heapify(a, count);

      end := count - 1;
      while(end > 0) {
        tmp := a[end];
        a[end] := a[0];
        a[0] := tmp;
        SiftDown(a, 0, end - 1);
        end -= 1;
      };
    }

    function : Heapify(a : Int[], count : Int) ~ Nil {
      start := (count - 2) / 2;
      while(start >= 0) {
        SiftDown(a, start, count - 1);
        start -= 1;
      };
    }

    function : SiftDown(a : Int[], start : Int, end : Int) ~ Nil {
      root := start;
      while((root * 2 + 1) <= end) {
        child := root * 2 + 1;
        if(child + 1 <= end & a[child] < a[child + 1]) {
          child := child + 1;
        };

        if(a[root] < a[child]) {
          tmp := a[root];
          a[root] := a[child];
          a[child] := tmp;
          root := child;
        }
        else {
          return;
        };
      };
    }
  }
}
```



## OCaml


```ocaml
let heapsort a =

  let swap i j =
    let t = a.(i) in a.(i) <- a.(j); a.(j) <- t in

  let sift k l =
    let rec check x y =
      if 2*x+1 < l then
        let ch =
          if y < l-1 && a.(y) < a.(y+1) then y+1 else y in
        if a.(x) < a.(ch) then (swap x ch; check ch (2*ch+1)) in
    check k (2*k+1) in

  let len = Array.length a in

  for start = (len/2)-1 downto 0 do
    sift start len;
  done;

  for term = len-1 downto 1 do
    swap term 0;
    sift 0 term;
  done;;
```

Usage:

```ocaml
let a = [|3;1;4;1;5;9;2;6;5;3;5;8;97;93;23;84;62;64;33;83;27;95|] in
  heapsort a;
  Array.iter (Printf.printf "%d ") a;;
print_newline ();;

let s = "Just to show this is a type-checked polymorphic function" in
let b = Array.init (String.length s) (String.get s) in
  heapsort b;
  Array.iter print_char b;;
print_newline ();;
```

{{out}}

```txt

1 1 2 3 3 4 5 5 5 6 8 9 23 27 33 62 64 83 84 93 95 97
        -Jaccccdeeefhhhhiiiiklmnnoooooppprsssstttttuuwyy

```



## Oz

A faithful translation of the pseudocode, adjusted to the fact that Oz arrays can start with an arbitrary index, not just 0 or 1.

```oz
declare
  proc {HeapSort A}
     Low = {Array.low A}
     High = {Array.high A}
     Count = High-Low+1

     %% heapify
     LastParent = Low + (Count-2) div 2
  in
     for Start in LastParent..Low;~1 do
        {Siftdown A Start High}
     end

     %% repeatedly put the maximum element to the end
     %% and re-heapify the rest
     for End in High..Low+1;~1 do
        {Swap A End Low}
        {Siftdown A Low End-1}
     end
  end

  proc {Siftdown A Start End}
     Low = {Array.low A}
     fun {FirstChildOf I} Low+(I-Low)*2+1 end

     Root = {NewCell Start}
  in
     for while:{FirstChildOf @Root} =< End
        break:Break
     do
        Child = {NewCell {FirstChildOf @Root}}
     in
        if @Child + 1 =< End andthen A.@Child < A.(@Child + 1) then
           Child := @Child + 1
        end
        if A.@Root < A.@Child then
           {Swap A @Root @Child}
           Root := @Child
        else
           {Break}
        end
     end
  end

  proc {Swap A I J}
     A.J := (A.I := A.J)
  end

  %% create array with indices ~1..7 and fill it
  Arr = {Array.new ~1 7 0}
  {Record.forAllInd unit(~1:3 0:1 4 1 5 9 2 6 5)
   proc {$ I V}
      Arr.I := V
   end}
in
  {HeapSort Arr}
  {Show {Array.toRecord unit Arr}}
```



## Pascal

An example, which works on arrays with arbitrary bounds :-)

```pascal
program HeapSortDemo;

type
  TIntArray = array[4..15] of integer;

var
  data: TIntArray;
  i: integer;

procedure siftDown(var a: TIntArray; start, ende: integer);
  var
    root, child, swap: integer;
  begin
    root := start;
    while root * 2 - start + 1 <= ende do
    begin
      child := root * 2 - start + 1;
      if (child + 1 <= ende) and (a[child] < a[child + 1]) then
        inc(child);
      if a[root] < a[child] then
      begin
	swap     := a[root];
        a[root]  := a[child];
        a[child] := swap;
        root := child;
      end
      else
        exit;
    end;
  end;

procedure heapify(var a: TIntArray);
  var
    start, count: integer;
  begin
    count := length(a);
    start := low(a) + count div 2 - 1;
    while start >= low(a) do
    begin
      siftdown(a, start, high(a));
      dec(start);
    end;
  end;

procedure heapSort(var a: TIntArray);
  var
    ende, swap: integer;
  begin
    heapify(a);
    ende := high(a);
    while ende > low(a) do
    begin
      swap := a[low(a)];
      a[low(a)] := a[ende];
      a[ende] := swap;
      dec(ende);
      siftdown(a, low(a), ende);
    end;
  end;

begin
  Randomize;
  writeln('The data before sorting:');
  for i := low(data) to high(data) do
  begin
    data[i] := Random(high(data));
    write(data[i]:4);
  end;
  writeln;
  heapSort(data);
  writeln('The data after sorting:');
  for i := low(data) to high(data) do
  begin
    write(data[i]:4);
  end;
  writeln;
end.
```

{{out}}

```txt

The data before sorting:
  12  13   0   1   0  14  13  10   1  10   9   2
The data after sorting:
   0   0   1   1   2   9  10  10  12  13  13  14

```



## Perl


```perl
#!/usr/bin/perl

my @a = (4, 65, 2, -31, 0, 99, 2, 83, 782, 1);
print "@a\n";
heap_sort(\@a);
print "@a\n";

sub heap_sort {
    my ($a) = @_;
    my $n = @$a;
    for (my $i = ($n - 2) / 2; $i >= 0; $i--) {
        down_heap($a, $n, $i);
    }
    for (my $i = 0; $i < $n; $i++) {
        my $t = $a->[$n - $i - 1];
        $a->[$n - $i - 1] = $a->[0];
        $a->[0] = $t;
        down_heap($a, $n - $i - 1, 0);
    }
}

sub down_heap {
    my ($a, $n, $i) = @_;
    while (1) {
        my $j = max($a, $n, $i, 2 * $i + 1, 2 * $i + 2);
        last if $j == $i;
        my $t = $a->[$i];
        $a->[$i] = $a->[$j];
        $a->[$j] = $t;
        $i = $j;
    }
}

sub max {
    my ($a, $n, $i, $j, $k) = @_;
    my $m = $i;
    $m = $j if $j < $n && $a->[$j] > $a->[$m];
    $m = $k if $k < $n && $a->[$k] > $a->[$m];
    return $m;
}

```



## Perl 6


```perl6
sub heap_sort ( @list ) {
    for ( 0 ..^ +@list div 2 ).reverse -> $start {
        _sift_down $start, @list.end, @list;
    }

    for ( 1 ..^ +@list ).reverse -> $end {
        @list[ 0, $end ] .= reverse;
        _sift_down 0, $end-1, @list;
    }
}

sub _sift_down ( $start, $end, @list ) {
    my $root = $start;
    while ( my $child = $root * 2 + 1 ) <= $end {
        $child++ if $child + 1 <= $end and [<] @list[ $child, $child+1 ];
        return if @list[$root] >= @list[$child];
        @list[ $root, $child ] .= reverse;
        $root = $child;
    }
}

my @data = 6, 7, 2, 1, 8, 9, 5, 3, 4;
say 'Input  = ' ~ @data;
@data.&heap_sort;
say 'Output = ' ~ @data;
```

{{out}}

```txt

Input  = 6 7 2 1 8 9 5 3 4
Output = 1 2 3 4 5 6 7 8 9

```



## Phix


```Phix
function siftDown(sequence arr, integer s, integer last)
integer root = s
    while root*2<=last do
        integer child = root*2
        if child<last and arr[child]<arr[child+1] then
            child += 1
        end if
        if arr[root]>=arr[child] then exit end if
        object tmp = arr[root]
        arr[root] = arr[child]
        arr[child] = tmp
        root = child
    end while
    return arr
end function

function heapify(sequence arr, integer count)
integer s = floor(count/2)
    while s>0 do
        arr = siftDown(arr,s,count)
        s -= 1
    end while
    return arr
end function

function heap_sort(sequence arr)
integer last = length(arr)
    arr = heapify(arr,last)
    while last>1 do
        object tmp = arr[1]
        arr[1] = arr[last]
        arr[last] = tmp
        last -= 1
        arr = siftDown(arr,1,last)
    end while
    return arr
end function

?heap_sort({5,"oranges","and",3,"apples"})
```

{{out}}

```txt

{3,5,"and","apples","oranges"}

```



## PicoLisp


```PicoLisp
(de heapSort (A Cnt)
   (let Cnt (length A)
      (for (Start (/ Cnt 2) (gt0 Start) (dec Start))
         (siftDown A Start (inc Cnt)) )
      (for (End Cnt (> End 1) (dec End))
         (xchg (nth A End) A)
         (siftDown A 1 End) ) )
   A )

(de siftDown (A Start End)
   (use Child
      (for (Root Start  (> End (setq Child (* 2 Root))))
         (and
            (> End (inc Child))
            (> (get A (inc Child)) (get A Child))
            (inc 'Child) )
         (NIL (> (get A Child) (get A Root)))
         (xchg (nth A Root) (nth A Child))
         (setq Root Child) ) ) )
```

{{out}}

```txt
: (heapSort (make (do 9 (link (rand 1 999)))))
-> (1 167 183 282 524 556 638 891 902)
```



## PL/I


```pli
*process source xref attributes or(!);
 /*********************************************************************
 * Pseudocode found here:
 *   http://en.wikipedia.org/wiki/Heapsort#Pseudocode
 * Sample data from REXX
 * 27.07.2013 Walter Pachl
 *********************************************************************/
 heaps: Proc Options(main);
 Dcl a(0:25) Char(50) Var Init(
      '---letters of the modern Greek Alphabet---',
      '
### ====================================
',
      'alpha','beta','gamma','delta','epsilon','zeta','eta','theta',
      'iota','kappa','lambda','mu','nu','xi','omicron','pi',
      'rho','sigma','tau','upsilon','phi','chi','psi','omega');
 Dcl n Bin Fixed(31) Init((hbound(a)+1));

 Call showa('before sort');
 Call heapsort((n));
 Call showa(' after sort');

 heapSort: Proc(count);
   Dcl (count,end) Bin Fixed(31);
   Call heapify((count));
   end=count-1;
   do while(end>0);
     Call swap(end,0);
     end=end-1;
     Call siftDown(0,(end));
     End;
   End;

 heapify: Proc(count);
   Dcl (count,start) Bin Fixed(31);
   start=(count-2)/2;
   Do while (start>=0);
     Call siftDown((start),count-1);
     start=start-1;
     End;
   End;

 siftDown: Proc(start,end);
   Dcl (count,start,root,end,child,sw) Bin Fixed(31);
   root=start;
   Do while(root*2+1<= end);
     child=root*2+1;
     sw=root;
     if a(sw)<a(child) Then
       sw=child;
     if child+1<=end & a(sw)<a(child+1) Then
       sw=child+1;
     if sw^=root Then Do;
       Call swap(root,sw);
       root=sw;
       End;
     else
       return;
     End;
   End;

 swap: Proc(x,y);
 Dcl (x,y) Bin Fixed(31);
 Dcl temp Char(50) Var;
   temp=a(x);
   a(x)=a(y);
   a(y)=temp;
   End;

 showa: Proc(txt);
 Dcl txt Char(*);
 Dcl j Bin Fixed(31);
 Do j=0 To hbound(a);
   Put Edit('element',j,txt,a(j))(skip,a,f(3),x(1),a,x(1),a);
   End;
 End;

 End;
```

{{out}}

```txt

element  0 before sort ---letters of the modern Greek Alphabet---
element  1 before sort
### ====================================

element  2 before sort alpha
element  3 before sort beta
element  4 before sort gamma
element  5 before sort delta
element  6 before sort epsilon
element  7 before sort zeta
element  8 before sort eta
element  9 before sort theta
element 10 before sort iota
element 11 before sort kappa
element 12 before sort lambda
element 13 before sort mu
element 14 before sort nu
element 15 before sort xi
element 16 before sort omicron
element 17 before sort pi
element 18 before sort rho
element 19 before sort sigma
element 20 before sort tau
element 21 before sort upsilon
element 22 before sort phi
element 23 before sort chi
element 24 before sort psi
element 25 before sort omega
element  0  after sort ---letters of the modern Greek Alphabet---
element  1  after sort
### ====================================

element  2  after sort alpha
element  3  after sort beta
element  4  after sort chi
element  5  after sort delta
element  6  after sort epsilon
element  7  after sort eta
element  8  after sort gamma
element  9  after sort iota
element 10  after sort kappa
element 11  after sort lambda
element 12  after sort mu
element 13  after sort nu
element 14  after sort omega
element 15  after sort omicron
element 16  after sort phi
element 17  after sort pi
element 18  after sort psi
element 19  after sort rho
element 20  after sort sigma
element 21  after sort tau
element 22  after sort theta
element 23  after sort upsilon
element 24  after sort xi
element 25  after sort zeta

```



## PowerShell


```PowerShell

function heapsort($a, $count) {
   $a = heapify $a $count
   $end = $count - 1
   while( $end -gt 0) {
      $a[$end], $a[0] = $a[0], $a[$end]
      $end--
      $a = siftDown $a 0 $end
    }
    $a
}
function heapify($a, $count) {
   $start = [Math]::Floor(($count - 2) / 2)
   while($start -ge 0) {
      $a = siftDown $a $start ($count-1)
      $start--
   }
   $a
}
function siftdown($a, $start, $end) {
   $b, $root = $true, $start
   while(( ($root * 2 + 1) -le $end) -and $b) {
      $child = $root * 2 + 1
      if( ($child + 1 -le $end) -and ($a[$child] -lt $a[$child + 1]) ) {
         $child++
      }
      if($a[$root] -lt $a[$child]) {
        $a[$root], $a[$child] = $a[$child], $a[$root]
        $root = $child
      }
      else { $b = $false}
    }
    $a
}
$array = @(60, 21, 19, 36, 63, 8, 100, 80, 3, 87, 11)
"$(heapsort $array $array.Count)"

```

<b>Output:</b>

```txt

3 8 11 19 21 36 60 63 80 87 100

```



## PureBasic


```PureBasic
Declare heapify(Array a(1), count)
Declare siftDown(Array a(1), start, ending)

Procedure heapSort(Array a(1), count)
  Protected ending=count-1
  heapify(a(), count)
  While ending>0
    Swap a(ending),a(0)
    siftDown(a(), 0, ending-1)
    ending-1
  Wend
EndProcedure

Procedure heapify(Array a(1), count)
  Protected start=(count-2)/2
  While start>=0
    siftDown(a(),start,count-1)
    start-1
  Wend
EndProcedure

Procedure siftDown(Array a(1), start, ending)
  Protected root=start, child
  While (root*2+1)<=ending
    child=root*2+1
    If child+1<=ending And a(child)<a(child+1)
      child+1
    EndIf
    If a(root)<a(child)
      Swap a(root), a(child)
      root=child
    Else
      Break
    EndIf
  Wend
EndProcedure
```



## Python


```python
def heapsort(lst):
  ''' Heapsort. Note: this function sorts in-place (it mutates the list). '''

  # in pseudo-code, heapify only called once, so inline it here
  for start in range((len(lst)-2)/2, -1, -1):
    siftdown(lst, start, len(lst)-1)

  for end in range(len(lst)-1, 0, -1):
    lst[end], lst[0] = lst[0], lst[end]
    siftdown(lst, 0, end - 1)
  return lst

def siftdown(lst, start, end):
  root = start
  while True:
    child = root * 2 + 1
    if child > end: break
    if child + 1 <= end and lst[child] < lst[child + 1]:
      child += 1
    if lst[root] < lst[child]:
      lst[root], lst[child] = lst[child], lst[root]
      root = child
    else:
      break
```

Testing:

```txt
>>> ary = [7, 6, 5, 9, 8, 4, 3, 1, 2, 0]
>>> heapsort(ary)
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Racket


```racket

#lang racket
(require (only-in srfi/43 vector-swap!))

(define (heap-sort! xs)
  (define (ref i) (vector-ref xs i))
  (define (swap! i j) (vector-swap! xs i j))
  (define size (vector-length xs))

  (define (sift-down! r end)
    (define c (+ (* 2 r) 1))
    (define c+1 (+ c 1))
    (when (<= c end)
      (define child
        (if (and (<= c+1 end) (< (ref c) (ref c+1)))
            c+1 c))
      (when (< (ref r) (ref child))
        (swap! r child))
      (sift-down! child end)))

  (for ([i (in-range (quotient (- size 2) 2) -1 -1)])
    (sift-down! i (- size 1)))

  (for ([end (in-range (- size 1) 0 -1)])
    (swap! 0 end)
    (sift-down! 0 (- end 1)))
  xs)

```



## REXX

===version 1, elements of an array===
This REXX version uses a heapsort to sort elements of an array, the elements can be numbers or character strings   (or a mixture of both).

Indexing of the array (for this version) starts with   '''1'''   (one),   but can be programmed to start with zero.

```rexx
/*REXX pgm sorts an array (names of epichoric Greek letters) using a heapsort algorithm.*/
@.=; @.1= 'alpha'  ;   @.7 = "zeta"  ;   @.13= 'mu'     ;  @.19= "qoppa"  ;  @.25= 'chi'
     @.2= 'beta'   ;   @.8 = "eta"   ;   @.14= 'nu'     ;  @.20= "rho"    ;  @.26= 'psi'
     @.3= 'gamma'  ;   @.9 = "theta" ;   @.15= 'xi'     ;  @.21= "sigma"  ;  @.27= 'omega'
     @.4= 'delta'  ;   @.10= "iota"  ;   @.16= 'omicron';  @.22= "tau"
     @.5= 'digamma';   @.11= "kappa" ;   @.17= 'pi'     ;  @.23= "upsilon"
     @.6= 'epsilon';   @.12= "lambda";   @.18= 'san'    ;  @.24= "phi"
                            do #=1  until @.#=='';  end;      #=# - 1  /*find # entries.*/
call show      "before sort:"
call heapSort        #;          say copies('▒', 40)                   /*sort; show sep.*/
call show      " after sort:"
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
heapSort: procedure expose @.; arg n;   do j=n%2 by -1 to 1;  call shuffle  j,n; end /*j*/
              do n=n  by -1  to 2;       _=@.1;   @.1=@.n;   @.n=_;   call shuffle  1, n-1
              end   /*n*/                        /* [↑]  swap two elements; and shuffle.*/
          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
shuffle:  procedure expose @.;  parse arg i,n;    $=@.i                /*obtain parent. */
                                        do  while i+i<=n;        j=i+i;    k=j+1
                                        if k<=n    then  if  @.k>@.j  then j=k
                                        if $>=@.j  then leave;   @.i=@.j;  i=j
                                        end   /*while*/
          @.i=$;    return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:     do s=1  for #;  say '    element' right(s, length(#)) arg(1) @.s;  end;   return
```

{{out|output|text=  when using the default   (epichoric Greek alphabet)   for input:}}

```txt

    element  1 before sort: alpha
    element  2 before sort: beta
    element  3 before sort: gamma
    element  4 before sort: delta
    element  5 before sort: digamma
    element  6 before sort: epsilon
    element  7 before sort: zeta
    element  8 before sort: eta
    element  9 before sort: theta
    element 10 before sort: iota
    element 11 before sort: kappa
    element 12 before sort: lambda
    element 13 before sort: mu
    element 14 before sort: nu
    element 15 before sort: xi
    element 16 before sort: omicron
    element 17 before sort: pi
    element 18 before sort: san
    element 19 before sort: qoppa
    element 20 before sort: rho
    element 21 before sort: sigma
    element 22 before sort: tau
    element 23 before sort: upsilon
    element 24 before sort: phi
    element 25 before sort: chi
    element 26 before sort: psi
    element 27 before sort: omega
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
    element  1  after sort: alpha
    element  2  after sort: beta
    element  3  after sort: chi
    element  4  after sort: delta
    element  5  after sort: digamma
    element  6  after sort: epsilon
    element  7  after sort: eta
    element  8  after sort: gamma
    element  9  after sort: iota
    element 10  after sort: kappa
    element 11  after sort: lambda
    element 12  after sort: mu
    element 13  after sort: nu
    element 14  after sort: omega
    element 15  after sort: omicron
    element 16  after sort: phi
    element 17  after sort: pi
    element 18  after sort: psi
    element 19  after sort: qoppa
    element 20  after sort: rho
    element 21  after sort: san
    element 22  after sort: sigma
    element 23  after sort: tau
    element 24  after sort: theta
    element 25  after sort: upsilon
    element 26  after sort: xi
    element 27  after sort: zeta

```


===version 2, elements of a list===
This REXX version creates a stemmed array from a list   (it can be numbers or characters, or a mixture of both).

```rexx
/*REXX pgm sorts an array (names of epichoric Greek letters) using a heapsort algorithm.*/
parse arg g                                      /*obtain optional arguments from the CL*/
if g=''  then g='alpha beta gamma delta digamma epsilon zeta eta theta iota kappa lambda',
                "mu nu xi omicron pi san qoppa  rho sigma tau upsilon phi chi psi omega"
#=words(g);                     do i=1  for #;   @.i=word(g,i);   end   /*assign to @.  */
call show      "before sort:"
call heapSort     #;            say copies('▒', 40)                     /*sort; show sep*/
call show      " after sort:"
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
heapSort: procedure expose @.; arg n;   do j=n%2 by -1 to 1;  call shuffle  j,n; end /*j*/
              do n=n  by -1  to 2;      _=@.1;   @.1=@.n;   @.n=_;    call shuffle  1, n-1
              end   /*n*/                        /* [↑]  swap two elements; and shuffle.*/
          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
shuffle:  procedure expose @.;  parse arg i,n;    $=@.i                 /*obtain parent.*/
                                        do  while i+i<=n;        j=i+i;    k=j+1
                                        if k<=n    then  if  @.k>@.j  then j=k
                                        if $>=@.j  then leave;   @.i=@.j;  i=j
                                        end   /*while*/
          @.i=$;    return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show:     do s=1  for #;  say '    element' right(s, length(#)) arg(1) @.s;  end;   return
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version when using the default input   (an epichoric Greek alphabet).}}


{{out|output|text=  when using the following for input:     <tt> 19  0  -.2  .1  1e5  19  17  -6  789  11  37 </tt>}}

```txt

    element  1 before sort: 19
    element  2 before sort: 0
    element  3 before sort: -.2
    element  4 before sort: .1
    element  5 before sort: 1e5
    element  6 before sort: 19
    element  7 before sort: 17
    element  8 before sort: -6
    element  9 before sort: 789
    element 10 before sort: 11
    element 11 before sort: 37
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
    element  1  after sort: -6
    element  2  after sort: -.2
    element  3  after sort: 0
    element  4  after sort: .1
    element  5  after sort: 11
    element  6  after sort: 17
    element  7  after sort: 19
    element  8  after sort: 19
    element  9  after sort: 37
    element 10  after sort: 789
    element 11  after sort: 1e5

```



### version 3


```rexx
/* REXX ***************************************************************
* Translated from PL/I
* 27.07.2013 Walter Pachl
**********************************************************************/
 list='---letters of the modern Greek Alphabet---|'||,
      '
### ====================================
|'||,
      'alpha|beta|gamma|delta|epsilon|zeta|eta|theta|'||,
      'iota|kappa|lambda|mu|nu|xi|omicron|pi|'||,
      'rho|sigma|tau|upsilon|phi|chi|psi|omega'
 Do i=0 By 1 While list<>''
   Parse Var list a.i '|' list
   End
 n=i-1

 Call showa 'before sort'
 Call heapsort n
 Call showa ' after sort'
 Exit

 heapSort: Procedure Expose a.
 Parse Arg count
 Call heapify count
 end=count-1
 do while end>0
   Call swap end,0
   end=end-1
   Call siftDown 0,end
   End
 Return

 heapify: Procedure Expose a.
 Parse Arg count
 start=(count-2)%2
 Do while start>=0
   Call siftDown start,count-1
   start=start-1
   End
 Return

 siftDown: Procedure Expose a.
 Parse Arg start,end
 root=start
 Do while root*2+1<= end
   child=root*2+1
   sw=root
   if a.sw<a.child Then
     sw=child
   child_1=child+1
   if child+1<=end & a.sw<a.child_1 Then
     sw=child+1
   if sw<>root Then Do
     Call swap root,sw
     root=sw
     End
   else
     return
   End
 Return

 swap: Procedure Expose a.
 Parse arg x,y
 temp=a.x
 a.x=a.y
 a.y=temp
 Return

 showa: Procedure Expose a. n
 Parse Arg txt
 Do j=0 To n-1
   Say 'element' format(j,2) txt a.j
   End
 Return
```

Output: see PL/I


## Ring


```ring

# Project : Sorting algorithms/Heapsort

test = [4, 65, 2, -31, 0, 99, 2, 83, 782, 1]
see "before sort:" + nl
showarray(test)
heapsort(test)
see "after sort:" + nl
showarray(test)

func heapsort(a)
cheapify(a)
for e = len(a) to 1 step -1
     temp = a[e]
     a[e] = a[1]
     a[1] = temp
     siftdown(a, 1, e-1)
next

func cheapify(a)
m = len(a)
for s = floor((m - 1) / 2) to 1 step -1
     siftdown(a,s,m)
next

func siftdown(a,s,e)
r = s
while r * 2 + 1 <= e
         c = r * 2
         if c + 1 <= e
            if a[c] < a[c + 1]
               c = c + 1
            ok
         ok
         if a[r] < a[c]
            temp = a[r]
            a[r] = a[c]
            a[c] = temp
            r = c
         else
            exit
         ok
end

func showarray(vect)
        svect = ""
        for n = 1 to len(vect)
              svect = svect + vect[n] + " "
        next
        svect = left(svect, len(svect) - 1)
        see svect + nl

```

Output:

```txt

before sort:
4 65 2 -31 0 99 2 83 782 1
after sort:
-31 0 1 2 2 4 65 83 99 782

```



## Ruby


```ruby
class Array
  def heapsort
    self.dup.heapsort!
  end

  def heapsort!
    # in pseudo-code, heapify only called once, so inline it here
    ((length - 2) / 2).downto(0) {|start| siftdown(start, length - 1)}

    # "end" is a ruby keyword
    (length - 1).downto(1) do |end_|
      self[end_], self[0] = self[0], self[end_]
      siftdown(0, end_ - 1)
    end
    self
  end

  def siftdown(start, end_)
    root = start
    loop do
      child = root * 2 + 1
      break if child > end_
      if child + 1 <= end_ and self[child] < self[child + 1]
        child += 1
      end
      if self[root] < self[child]
        self[root], self[child] = self[child], self[root]
        root = child
      else
        break
      end
    end
  end
end
```

Testing:

```txt
irb(main):035:0> ary = [7, 6, 5, 9, 8, 4, 3, 1, 2, 0]
=> [7, 6, 5, 9, 8, 4, 3, 1, 2, 0]
irb(main):036:0> ary.heapsort
=> [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Rust

{{trans|Python}}
This program allows the caller to specify an arbitrary function by which an order is determined.

```rust
fn main() {
    let mut v = [4, 6, 8, 1, 0, 3, 2, 2, 9, 5];
    heap_sort(&mut v, |x, y| x < y);
    println!("{:?}", v);
}

fn heap_sort<T, F>(array: &mut [T], order: F)
where
    F: Fn(&T, &T) -> bool,
{
    let len = array.len();
    // Create heap
    for start in (0..len / 2).rev() {
        shift_down(array, &order, start, len - 1)
    }

    for end in (1..len).rev() {
        array.swap(0, end);
        shift_down(array, &order, 0, end - 1)
    }
}

fn shift_down<T, F>(array: &mut [T], order: &F, start: usize, end: usize)
where
    F: Fn(&T, &T) -> bool,
{
    let mut root = start;
    loop {
        let mut child = root * 2 + 1;
        if child > end {
            break;
        }
        if child + 1 <= end && order(&array[child], &array[child + 1]) {
            child += 1;
        }
        if order(&array[root], &array[child]) {
            array.swap(root, child);
            root = child
        } else {
            break;
        }
    }
}
```


Of course, you could also simply use <code>BinaryHeap</code> in the standard library.


```rust
use std::collections::BinaryHeap;

fn main() {
    let src = vec![6, 2, 3, 6, 1, 2, 7, 8, 3, 2];
    let sorted = BinaryHeap::from(src).into_sorted_vec();
    println!("{:?}", sorted);
}
```



## Scala

{{works with|Scala|2.8}}
This code is not written for maximum performance, though, of course, it preserves the O(n log n) characteristic of heap sort.

```scala
def heapSort[T](a: Array[T])(implicit ord: Ordering[T]) {
  import scala.annotation.tailrec // Ensure functions are tail-recursive
  import ord._

  val indexOrdering = Ordering by a.apply

  def numberOfLeaves(heapSize: Int) = (heapSize + 1) / 2

  def children(i: Int, heapSize: Int) = {
    val leftChild = i * 2 + 1
    leftChild to leftChild + 1 takeWhile (_ < heapSize)
  }

  def swap(i: Int, j: Int) = {
    val tmp = a(i)
    a(i) = a(j)
    a(j) = tmp
  }

  // Maintain partial ordering by bubbling down elements
  @tailrec
  def siftDown(i: Int, heapSize: Int) {
    val childrenOfI = children(i, heapSize)
    if (childrenOfI nonEmpty) {
      val biggestChild = childrenOfI max indexOrdering
      if (a(i) < a(biggestChild)) {
        swap(i, biggestChild)
        siftDown(biggestChild, heapSize)
      }
    }
  }

  // Prepare heap by sifting down all non-leaf elements
  for (i <- a.indices.reverse drop numberOfLeaves(a.size)) siftDown(i, a.size)

  // Sort from the end of the array forward, by swapping the highest element,
  // which is always the top of the heap, to the end of the unsorted array
  for (i <- a.indices.reverse) {
    swap(0, i)
    siftDown(0, i)
  }
}
```



## Scheme

{{works with|Scheme|R<math>^5</math>RS}}

```scheme
; swap two elements of a vector
(define (swap! v i j)
  (define temp (vector-ref v i))
  (vector-set! v i (vector-ref v j))
  (vector-set! v j temp))

; sift element at node start into place
(define (sift-down! v start end)
  (let ((child (+ (* start 2) 1)))
    (cond
      ((> child end) 'done) ; start has no children
      (else
       (begin
         ; if child has a sibling node whose value is greater ...
         (and (and (<= (+ child 1) end)
                   (< (vector-ref v child) (vector-ref v (+ child 1))))
              ; ... then we'll look at the sibling instead
              (set! child (+ child 1)))
         (if (< (vector-ref v start) (vector-ref v child))
             (begin
               (swap! v start child)
               (sift-down! v child end))
             'done))))))

; transform v into a binary max-heap
(define (heapify v)
  (define (iter v start)
    (if (>= start 0)
        (begin (sift-down! v start (- (vector-length v) 1))
               (iter v (- start 1)))
        'done))
  ; start sifting with final parent node of v
  (iter v (quotient (- (vector-length v) 2) 2)))

(define (heapsort v)
  ; swap root and end node values,
  ; sift the first element into place
  ; and recurse with new root and next-to-end node
  (define (iter v end)
    (if (zero? end)
        'done
        (begin
          (swap! v 0 end)
          (sift-down! v 0 (- end 1))
          (iter v (- end 1)))))
  (begin
    (heapify v)
    ; start swapping with root and final node
    (iter v (- (vector-length v) 1))))

; testing
(define uriah (list->vector '(3 5 7 9 0 8 1 4 2 6)))
(heapsort uriah)
uriah
```

{{out}}

```txt
done
#(0 1 2 3 4 5 6 7 8 9)
```



## Seed7


```seed7
const proc: downheap (inout array elemType: arr, in var integer: k, in integer: n) is func
  local
    var elemType: help is elemType.value;
    var integer: j is 0;
  begin
    if k <= n div 2 then
      help := arr[k];
      repeat
        j := 2 * k;
        if j < n and arr[j] < arr[succ(j)] then
          incr(j);
        end if;
        if help < arr[j] then
          arr[k] := arr[j];
          k := j;
        end if;
      until help >= arr[j] or k > n div 2;
      arr[k] := help;
    end if;
  end func;

const proc: heapSort (inout array elemType: arr) is func
  local
    var integer: n is 0;
    var integer: k is 0;
    var elemType: help is elemType.value;
  begin
    n := length(arr);
    for k range n div 2 downto 1 do
      downheap(arr, k, n);
    end for;
    repeat
      help := arr[1];
      arr[1] := arr[n];
      arr[n] := help;
      decr(n);
      downheap(arr, 1, n);
    until n <= 1;
  end func;
```

Original source: [http://seed7.sourceforge.net/algorith/sorting.htm#heapSort]


## SequenceL


```sequenceL

import <Utilities/Sequence.sl>;

TUPLE<T> ::= (A: T, B: T);

heapSort(x(1)) :=
	let
		heapified := heapify(x, (size(x) - 2) / 2 + 1);
	in
		sortLoop(heapified, size(heapified));

heapify(x(1), i) :=
	x when i <= 0 else
	heapify(siftDown(x, i, size(x)), i - 1);

sortLoop(x(1), i) :=
	x when i <= 2 else
	sortLoop( siftDown(swap(x, 1, i), 1, i - 1), i - 1);

siftDown(x(1), start, end) :=
	let
		child := start * 2;
		child1 := child + 1 when child + 1 <= end and x[child] < x[child + 1] else child;
	in
		x when child >= end else
		x when x[start] >= x[child1] else
		siftDown(swap(x, child1, start), child1, end);

swap(list(1), i, j) :=
	let
		vals := (A: list[i], B: list[j]);
	in
		setElementAt(setElementAt(list, i, vals.B), j, vals.A);

```


## Sidef


```ruby
func sift_down(a, start, end) {
    var root = start;
    while ((2*root + 1) <= end) {
        var child = (2*root + 1);
        if ((child+1 <= end) && (a[child] < a[child + 1])) {
            child += 1;
        }
        if (a[root] < a[child]) {
            a[child, root] = a[root, child];
            root = child;
        } else {
            return;
        }
    }
}

func heapify(a, count) {
    var start = ((count - 2) / 2);
    while (start >= 0) {
        sift_down(a, start, count-1);
        start -= 1;
    }
}

func heap_sort(a, count) {
    heapify(a, count);
    var end = (count - 1);
    while (end > 0) {
        a[0, end] = a[end, 0];
        end -= 1;
        sift_down(a, 0, end)
    }
    return a
}

var arr = (1..10 -> shuffle);  # creates a shuffled array
say arr;                       # prints the unsorted array
heap_sort(arr, arr.len);       # sorts the array in-place
say arr;                       # prints the sorted array
```

{{out}}

```txt
[10, 5, 2, 1, 7, 6, 4, 8, 3, 9]
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```



## Standard ML


Since Standard ML is a functional language, a [http://en.wikipedia.org/wiki/Pairing_heap pairing heap] is used instead of a standard binary heap.


```sml
(* Pairing heap - http://en.wikipedia.org/wiki/Pairing_heap *)
functor PairingHeap(type t
                    val cmp : t * t -> order) =
struct
  datatype 'a heap = Empty
                   | Heap of 'a * 'a heap list;

  (* merge, O(1)
   * Merges two heaps *)
  fun merge (Empty, h) = h
    | merge (h, Empty) = h
    | merge (h1 as Heap(e1, s1), h2 as Heap(e2, s2)) =
        case cmp (e1, e2) of LESS => Heap(e1, h2 :: s1)
                            |   _ => Heap(e2, h1 :: s2)

  (* insert, O(1)
   * Inserts an element into the heap *)
  fun insert (e, h) = merge (Heap (e, []), h)

  (* findMin, O(1)
   * Returns the smallest element of the heap *)
  fun findMin Empty = raise Domain
    | findMin (Heap(e, _)) = e

  (* deleteMin, O(lg n) amortized
   * Deletes the smallest element of the heap *)
  local
    fun mergePairs [] = Empty
      | mergePairs [h] = h
      | mergePairs (h1::h2::hs) = merge (merge(h1, h2), mergePairs hs)
  in
    fun deleteMin Empty = raise Domain
      | deleteMin (Heap(_, s)) = mergePairs s
  end

  (* build, O(n)
   * Builds a heap from a list *)
  fun build es = foldl insert Empty es;
end

local
  structure IntHeap = PairingHeap(type t = int; val cmp = Int.compare);
  open IntHeap

  fun heapsort' Empty = []
    | heapsort' h = findMin h :: (heapsort' o deleteMin) h;
in
  fun heapsort ls = (heapsort' o build) ls

  val test_0 = heapsort [] = []
  val test_1 = heapsort [1,2,3] = [1, 2, 3]
  val test_2 = heapsort [1,3,2] = [1, 2, 3]
  val test_3 = heapsort [6,2,7,5,8,1,3,4] = [1, 2, 3, 4, 5, 6, 7, 8]
end;

```



## Stata


Variant with siftup and siftdown, using Mata.


```mata
function siftup(a, i) {
	k = i
	while (k > 1) {
		p = floor(k/2)
		if (a[k] > a[p]) {
			s = a[p]
			a[p] = a[k]
			a[k] = s
			k = p
		}
		else break
	}
}

function siftdown(a, i) {
	k = 1
	while (1) {
		l = k+k
		if (l > i) break
		if (l+1 <= i) {
			if (a[l+1] > a[l]) l++
		}
		if (a[k] < a[l]) {
			s = a[k]
			a[k] = a[l]
			a[l] = s
			k = l
		}
		else break
	}
}

function heapsort(a) {
	n = length(a)
	for (i = 2; i <= n; i++) {
		siftup(a, i)
	}
	for (i = n; i >= 2; i--) {
		s = a[i]
		a[i] = a[1]
		a[1] = s
		siftdown(a, i-1)
	}
}
```



## Swift


```Swift>func heapsort<T:Comparable
(inout list:[T]) {
    var count = list.count

    func shiftDown(inout list:[T], start:Int, end:Int) {
        var root = start

        while root * 2 + 1 <= end {
            var child = root * 2 + 1
            var swap = root

            if list[swap] < list[child] {
                swap = child
            }

            if child + 1 <= end && list[swap] < list[child + 1] {
                swap = child + 1
            }

            if swap == root {
                return
            } else {
                (list[root], list[swap]) = (list[swap], list[root])
                root = swap
            }
        }
    }

    func heapify(inout list:[T], count:Int) {
        var start = (count - 2) / 2

        while start >= 0 {
            shiftDown(&list, start, count - 1)

            start--
        }
    }

    heapify(&list, count)

    var end = count - 1

    while end > 0 {
        (list[end], list[0]) = (list[0], list[end])

        end--

        shiftDown(&list, 0, end)
    }
}
```



## Tcl

Based on the algorithm from Wikipedia:
{{works with|Tcl|8.5}}

```tcl
package require Tcl 8.5

proc heapsort {list {count ""}} {
    if {$count eq ""} {
	set count [llength $list]
    }
    for {set i [expr {$count/2 - 1}]} {$i >= 0} {incr i -1} {
	siftDown list $i [expr {$count - 1}]
    }
    for {set i [expr {$count - 1}]} {$i > 0} {} {
	swap list $i 0
	incr i -1
	siftDown list 0 $i
    }
    return $list
}
proc siftDown {varName i j} {
    upvar 1 $varName a
    while true {
	set child [expr {$i*2 + 1}]
	if {$child > $j} {
	    break
	}
	if {$child+1 <= $j && [lindex $a $child] < [lindex $a $child+1]} {
	    incr child
	}
	if {[lindex $a $i] >= [lindex $a $child]} {
	    break
	}
	swap a $i $child
	set i $child
    }
}
proc swap {varName x y} {
    upvar 1 $varName a
    set tmp [lindex $a $x]
    lset a $x [lindex $a $y]
    lset a $y $tmp
}
```

Demo code:

```tcl
puts [heapsort {1 5 3 7 9 2 8 4 6 0}]
```

{{out}}

```txt
0 1 2 3 4 5 6 7 8 9
```


=={{header|TI-83 BASIC}}==
Store list with a dimension of 7 or less into L<sub>1</sub> (if less input will be padded with zeros), run prgmSORTHEAP, look into L<sub>2</sub> for the sorted version of L<sub>1</sub>. It is possible to do this without L<sub>3</sub> (thus, in place).

 :If dim(L<sub>1</sub>)>7
 :Then
 :Disp "ERR:7"
 :Stop
 :End
 :If dim(L<sub>1</sub>)<7
 :Then
 :For(A,1,7)
 :If A>dim(L<sub>1</sub>)
 :0→L<sub>1</sub>(A)
 :End
 :End
 :{0}→L<sub>2</sub>
 :For(B,2,7)
 :0→L<sub>2</sub>(B)
 :End
 :L<sub>1</sub>→L<sub>3</sub>
 :For(B,0,6)
 :If L<sub>3</sub>(4)>L<sub>3</sub>(2)
 :Then
 :L<sub>3</sub>(2)→A
 :L<sub>3</sub>(4)→L<sub>3</sub>(2)
 :A→L<sub>3</sub>(4)
 :End
 :If L<sub>3</sub>(5)>L<sub>3</sub>(2)
 :Then
 :L<sub>3</sub>(2)→A
 :L<sub>3</sub>(5)→L<sub>3</sub>(2)
 :A→L<sub>3</sub>(5)
 :End
 :If L<sub>3</sub>(6)>L<sub>3</sub>(3)
 :Then
 :L<sub>3</sub>(3)→A
 :L<sub>3</sub>(6)→L<sub>3</sub>(3)
 :A→L<sub>3</sub>(6)
 :End
 :If L<sub>3</sub>(7)>L<sub>3</sub>(3)
 :Then
 :L<sub>3</sub>(3)→A
 :L<sub>3</sub>(7)→L<sub>3</sub>(3)
 :A→L<sub>3</sub>(7)
 :End
 :If L<sub>3</sub>(2)>L<sub>3</sub>(1)
 :Then
 :L<sub>3</sub>(1)→A
 :L<sub>3</sub>(2)→L<sub>3</sub>(1)
 :A→L<sub>3</sub>(2)
 :End
 :If L<sub>3</sub>(3)>L<sub>3</sub>(1)
 :Then
 :L<sub>3</sub>(1)→A
 :L<sub>3</sub>(3)→L<sub>3</sub>(1)
 :A→L<sub>3</sub>(3)
 :End
 :L<sub>3</sub>(1)→L<sub>2</sub>(7-B)
 :If L<sub>3</sub>(2)>L<sub>3</sub>(3)
 :Then
 :L<sub>3</sub>(2)→L<sub>3</sub>(1)
 :0→L<sub>3</sub>(2)
 :Else
 :L<sub>3</sub>(3)→L<sub>3</sub>(1)
 :0→L<sub>3</sub>(3)
 :End
 :End
 :DelVar A
 :DelVar B
 :DelVar L<sub>3</sub>
 :Return


## uBasic/4tH

<lang>PRINT "Heap sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Heapsort (n)
  PROC _ShowArray (n)
PRINT

END


_Heapsort PARAM(1)                     ' Heapsort
  LOCAL(1)
  PROC _Heapify (a@)

  b@ = a@ - 1
  DO WHILE b@ > 0
     PROC _Swap (b@, 0)
     PROC _Siftdown (0, b@)
     b@ = b@ - 1
  LOOP
RETURN


_Heapify PARAM(1)
  LOCAL(1)

  b@ = (a@ - 2) / 2
  DO WHILE b@ > -1
     PROC _Siftdown (b@, a@)
     b@ = b@ - 1
  LOOP
RETURN


_Siftdown PARAM(2)
  LOCAL(2)
  c@ = a@

  DO WHILE ((c@ * 2) + 1) < (b@)
    d@ = c@ * 2 + 1
    IF d@+1 < b@ IF @(d@) < @(d@+1) THEN d@ = d@ + 1
  WHILE @(c@) < @(d@)
    PROC _Swap (d@, c@)
    c@ = d@
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



## VBA

{{trans|FreeBASIC}}

```VBA
Sub SiftDown(list() As Integer, start As Long, eend As Long)
	Dim root As Long : root = start
	Dim lb As Long : lb = LBound(list)
	Dim temp As Integer

	While root * 2 + 1 <= eend
		Dim child As Long : child = root * 2 + 1
		If child + 1 <= eend Then
			If list(lb + child) < list(lb + child + 1) Then
				child = child + 1
			End If
		End If
		If list(lb + root) < list(lb + child) Then
			temp = list(lb + root)
			list(lb + root) = list(lb + child)
			list(lb + child) = temp

			root = child
		Else
			Exit Sub
		End If
	Wend
End Sub

Sub HeapSort(list() As Integer)
	Dim lb As Long : lb = LBound(list)
	Dim count As Long : count = UBound(list) - lb + 1
	Dim start As Long : start = (count - 2) \ 2
	Dim eend As Long : eend = count - 1

	While start >= 0
		SiftDown list(), start, eend
		start = start - 1
	Wend

	Dim temp As Integer

	While eend > 0
		temp = list(lb + eend)
		list(lb + eend) = list(lb)
		list(lb) = temp

		eend = eend - 1

		SiftDown list(), 0, eend
	Wend
End Sub
```



## zkl


```zkl
fcn heapSort(a){  // in place
   n := a.len();
   foreach start in ([(n-2)/2 .. 0,-1])
      { siftDown(a, start, n-1) }
   foreach end in ([n-1 .. 1,-1]){
      a.swap(0, end);
      siftDown(a, 0, end-1);
   }
   a
}

fcn siftDown(a, start, end){
   while((child := start*2 + 1) <= end){
      if(child < end and a[child]<a[child+1]) child+=1;
      if(a[start] >= a[child]) return();
      a.swap(start, child);
      start = child;
   }
}
```


```zkl
heapSort(L(170, 45, 75, -90, -802, 24, 2, 66)).println();
heapSort("this is a test".split("")).println();
```

{{out}}

```txt

L(-802,-90,2,24,45,66,75,170)
L(" "," "," ","a","e","h","i","i","s","s","s","t","t","t")

```

