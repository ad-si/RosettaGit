+++
title = "Permutations/Derangements"
description = ""
date = 2019-08-01T21:07:32Z
aliases = []
[extra]
id = 9631
[taxonomies]
categories = ["task"]
tags = []
+++

A [http://mathworld.wolfram.com/Derangement.html derangement] is a permutation of the order of distinct items in which ''no item appears in its original place''.

For example, the only two derangements of the three items (0, 1, 2) are (1, 2, 0), and (2, 0, 1).

The number of derangements of ''n'' distinct items is known as the subfactorial of ''n'', sometimes written as !''n''.
There are various ways to [[wp:Derangement#Counting_derangements|calculate]] !''n''.


## Task

# Create a named function/method/subroutine/... to generate derangements of the integers ''0..n-1'', (or ''1..n'' if you prefer).
# Generate ''and show'' all the derangements of 4 integers using the above routine.
# Create a function that calculates the subfactorial of ''n'', !''n''.
# Print and show a table of the ''counted'' number of derangements of ''n'' vs. the calculated !''n'' for n from 0..9 inclusive.


;Optional stretch goal:
*   Calculate   <big><big> !''20'' </big></big>


## Related tasks

*   [[Anagrams/Deranged anagrams]]
*   [[Best shuffle]]
*   [[Left_factorials]]





## 360 Assembly

Due to 32 bit integers !12 is the limit.

```360asm
*        Permutations/Derangements 01/04/2017
DERANGE  CSECT
         USING  DERANGE,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         XPRNT  PG1,L'PG1          print title
         LA     R1,4               4
         LA     R2,1               1 : combinations print
         BAL    R14,DERGEN         call dergen
         STH    R0,COUNT           count=dergen(4,1)
         XPRNT  PG2,L'PG2          print table headings
         XPRNT  PG3,L'PG3          print hyphens
         SR     R4,R4
         STH    R4,II              ii=0
       DO WHILE=(CH,R4,LE,=H'9')   do ii=0 to 9
         MVC    PG,=CL80' '          clear buffer
         XDECO  R4,PG                edit ii
         LR     R1,R4                ii
         LA     R2,0                 0 : no combination print
         BAL    R14,DERGEN           dergen(ii,0)
         XDECO  R0,PG+12             edit
         LH     R1,II                ii
         BAL    R14,SUBFACT          subfact(ii)
         XDECO  R0,PG+24             edit
         XPRNT  PG,L'PG              print
         LH     R4,II                ii
         LA     R4,1(R4)             i+1
         STH    R4,II                i=i+1
       ENDDO    ,                  enddo i
         LA     R0,12              12
         STH    R0,II              ii=12
         MVC    PG,=CL16'!xx='     init buffer
         XDECO  R0,XDEC            edit ii
         MVC    PG+1(2),XDEC+10    output
         LH     R1,II              ii
         BAL    R14,SUBFACT        subfact(ii)
         XDECO  R0,PG+4            edit subfact(ii)
         XPRNT  PG,16              print
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
*------- ----   -------------------------------------------
DERGEN   EQU    *                  dergen(n,fprt)
         ST     R14,SAVEDG
         ST     R1,N               n
         ST     R2,FPRT            fprt
       IF LTR,R1,Z,R1 THEN         if n=0 then
         LA     R0,1                 1
         B      RETDG                return(1)
       ENDIF    ,                  endif
         MVC    C,=F'0'            c=0
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,N)        do i=1 to 2
         LR     R1,R6                i
         SLA    R1,1
         STH    R6,A-2(R1)           a(i)=i
         STH    R6,AO-2(R1)          ao(i)=i
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         L      R1,N               n
         BAL    R14,FACT
         ST     R0,FACTNM1         fact(n)-1
         SR     R6,R6              i=0
       DO WHILE=(C,R6,LE,FACTNM1)  do i=0 to fact(n)-1
         L      R1,N                 n
         BAL    R14,NEXTPER          call nextper(n)
         MVI    D,X'01'              d=true
         LA     R7,1
       DO WHILE=(C,R7,LE,N)          do j=1 to n
         LR     R1,R7                  j
         SLA    R1,1
         LH     R2,A-2(R1)             a(j)
         LH     R3,AO-2(R1)            ao(j)
       IF CR,R2,EQ,R3 THEN             if a(j)=ao(j) then
         MVI    D,X'00'                  d=false
       ENDIF    ,                      endif
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
       IF CLI,D,EQ,X'01' THEN        if d then
         L      R2,C                   c
         LA     R2,1(R2)               c+1
         ST     R2,C                   c=c+1
       IF CLI,FPRT+3,EQ,X'01' THEN     if fprt=1 then
         MVC    PG,=CL80' '              clear buffer
         LA     R10,PG                   pgi=0
         LA     R7,1                     j=1
       DO WHILE=(C,R7,LE,N)              do j=1 to n
         LR     R1,R7                      j
         SLA    R1,1
         LH     R2,A-2(R1)                 a(j)
         XDECO  R2,XDEC                    edit
         MVC    0(1,R10),XDEC+11           output
         LA     R10,2(R10)                 pgi=pgi+2
         LA     R7,1(R7)                   j++
        ENDDO    ,                        enddo j
         XPRNT  PG,L'PG                   print
       ENDIF    ,                      endif
       ENDIF    ,                    endif
         LA     R6,1(R6)             i++
        ENDDO    ,                 enddo i
         L      R0,C               c
         B      RETDG              return(c)
RETDG    L      R14,SAVEDG
         BR     R14
SAVEDG   DS     A
*------- ----   -------------------------------------------
NEXTPER  EQU    *                  nextper(nk)
         ST     R14,SAVENP
         ST     R1,NK              nk
         BCTR   R1,0               nk-1
         ST     R1,NELEM           nelem=nk-1
       IF C,R1,LT,=F'1' THEN       if nelem<1 then
         LA     R0,0                 return(0)
         B      RETNP
       ENDIF    ,                  endif
         L      R8,NELEM           nelem
         BCTR   R8,0               pos=nelem-1
LOOPW1   EQU    *                  while a(pos+1)>=a(pos+2)
         LR     R1,R8                pos
         SLA    R1,1
         LH     R2,A(R1)             a(pos+1)
         CH     R2,A+2(R1)           if a(pos+1)<a(pos+2)
         BL     ELOOPW1              then exit while
         BCTR   R8,0                 pos=pos-1
       IF LTR,R8,M,R8 THEN           if pos<0 then
         LA     R1,0                   0
         L      R2,NELEM               nelem
         BAL    R14,PERMREV            call permrev(0,nelem)
         LA     R0,0                   return(0)
         B      RETNP
       ENDIF    ,                    endif
         B      LOOPW1             endwhile
ELOOPW1  L      R9,NELEM           last=nelem
LOOPW2   EQU    *                  do while a(last+1)<=a(pos+1)
         LR     R1,R9                last
         SLA    R1,1
         LH     R2,A(R1)             a(last+1)
         LR     R1,R8                pos
         SLA    R1,1
         CH     R2,A(R1)             if a(last+1)>a(pos+1)
         BH     ELOOPW2              then exit while
         BCTR   R9,0                 last=last-1
         B      LOOPW2             endwhile
ELOOPW2  LR     R1,R8              pos
         SLA    R1,1               *2
         LA     R2,A(R1)           @a(pos+1)
         LR     R1,R9              last
         SLA    R1,1
         LA     R3,A(R1)           @a(last+1)
         LH     R0,0(R2)           w=a(pos+1)
         MVC    0(2,R2),0(R3)      a(pos+1)=a(last+1)
         STH    R0,0(R3)           a(last+1)=w
         LA     R1,1(R8)           pos+1
         L      R2,NELEM           nelem
         BAL    R14,PERMREV        call permrev(pos+1,nelem)
RETNP    L      R14,SAVENP
         BR     R14
SAVENP   DS     A
*------- ----   -------------------------------------------
PERMREV  EQU    *                  permrev(firstix,lastix)
         LR     R4,R1              xfirst
         LR     R5,R2              xlast
       DO WHILE=(CR,R4,LT,R5)      do while(xfirst<xlast)
         LR     R1,R4                xfirst
         SLA    R1,1                 *2
         LA     R2,A(R1)             @a(xfirst+1)
         LR     R1,R5                xlast
         SLA    R1,1                 *2
         LA     R3,A(R1)             @a(xlast+1)
         LH     R0,0(R2)             w=a(xfirst+1)
         MVC    0(2,R2),0(R3)        a(xfirst+1)=a(xlast+1)
         STH    R0,0(R3)             a(xlast+1)=w
         LA     R4,1(R4)             xfirst=xfirst+1
         BCTR   R5,0                 xlast=xlast-1
       ENDDO    ,                  enddo
         BR     R14
*------- ----   ----------------------------------------
FACT     EQU    *                  fact(n)
       IF C,R1,LE,=F'1' THEN       if n<=1 then
         LA     R0,1                 return(1)
       ELSE     ,                  else
         LA     R5,1                 f=1
         LA     R2,1                 i=1
       DO WHILE=(CR,R2,LE,R1)        do i=1 to n
         MR     R4,R2                  f*i
         LA     R2,1(R2)               i++
       ENDDO    ,                    enddo
         LR     R0,R5                return(f)
       ENDIF    ,                  endif
         BR     R14
*------- ----   -------------------------------------------
SUBFACT  EQU    *                  subfact(n)
         ST     R1,NY              n
       IF LTR,R1,Z,R1 THEN         if n=0 then
         LA     R0,1                 return(1)
       ELSE     ,                  else
         LA     R4,1                 1
         ST     R4,TT                tt(0)=1
         ST     R4,IY                i=1
       DO WHILE=(C,R4,LE,NY)         do i=1 to n
         L      R4,IY                  i
         SRDA   R4,32
         D      R4,=F'2'               i/2
       IF LTR,R4,Z,R4 THEN             if i//2=0 then
         LA     R0,1                     nn=1
       ELSE     ,                      else
         L      R0,=F'-1'                nn=-1
       ENDIF    ,                      endif
         L      R1,IY                  i
         SLA    R1,2
         L      R3,TT-4(R1)            tt(i-1)
         M      R2,IY                  *i
         AR     R3,R0                  +nn
         L      R1,IY                  i
         SLA    R1,2
         ST     R3,TT(R1)              tt(i)=i*tt(i-1)+nn
         L      R4,IY                  i
         LA     R4,1(R4)               i++
         ST     R4,IY                  i
       ENDDO    ,                    enddo
         L      R1,NY                n
         SLA    R1,2
         L      R0,TT(R1)            return(tt(n))
       ENDIF    ,                  endif
         BR     R14
*        ----   -------------------------------------------
A        DS     12H                A work
AO       DS     12H                A origin
II       DS     H
COUNT    DS     H
N        DS     F
FPRT     DS     F                  flag for printing
C        DS     F
D        DS     X                  boolean : a(i) different ao(i)
FACTNM1  DS     F                  fact(n)-1
NK       DS     F                  n           in nextper
NELEM    DS     F                  n elements  in nextper
NY       DS     F                  n           in subfact
IY       DS     F                  i           in subfact
TT       DS     13F                tt(0:12)
PG1      DC     CL44'derangements for the numbers : 1 2 3 4 are :'
PG2      DC     CL38'  table of n     counted  calculated :'
PG3      DC     CL36' ----------- ----------- -----------'
XDEC     DS     CL12               temp for xdeco
PG       DC     CL80' '            buffer
         YREGS
         END    DERANGE
```

```txt

derangements for the numbers : 1 2 3 4 are :
2 1 4 3
2 3 4 1
2 4 1 3
3 1 4 2
3 4 1 2
3 4 2 1
4 1 2 3
4 3 1 2
4 3 2 1
  table of n     counted  calculated :
 ----------- ----------- -----------
           0           1           1
           1           0           0
           2           2           1
           3           2           2
           4           9           9
           5          44          44
           6         265         265
           7        1854        1854
           8       14833       14833
           9      133496      133496
!12=   176214841

```




## Ada

```Ada
with Ada.Text_IO; use Ada.Text_IO;
procedure DePermute is
   type U64 is mod 2**64;
   type Num is range 0 .. 20;
   type NumList is array (Natural range <>) of Num;
   type PtNumList is access all NumList;
   package IO is new Ada.Text_IO.Integer_IO (Num);
   package UIO is new Ada.Text_IO.Modular_IO (U64);

   function deranged (depth : Natural; list : PtNumList;
      show : Boolean) return U64 is
      tmp : Num;  count : U64 := 0;
   begin
      if depth = list'Length then
         if show then
            for i in list'Range loop IO.Put (list (i), 2); end loop;
            New_Line;
         end if;  return 1;
      end if;
      for i in reverse depth .. list'Last loop
         if Num (i + 1) /= list (depth) then
            tmp := list (i); list (i) := list (depth); list (depth) := tmp;
            count := count + deranged (depth + 1, list, show);
            tmp := list (i); list (i) := list (depth); list (depth) := tmp;
         end if;
      end loop;
      return count;
   end deranged;

   function gen_n (len : Natural; show : Boolean) return U64 is
      list : PtNumList;
   begin
      list := new NumList (0 .. len - 1);
      for i in list'Range loop list (i) := Num (i + 1); end loop;
      return deranged (0, list, show);
   end gen_n;

   function sub_fact (n : Natural) return U64 is begin
      if n < 2 then return U64 (1 - n);
      else return (sub_fact (n - 1) + sub_fact (n - 2)) * U64 (n - 1);
      end if;
   end sub_fact;

   count : U64;
begin
   Put_Line ("Deranged 4:");
   count := gen_n (4, True);
   Put_Line ("List vs. calc:");
   for i in Natural range 0 .. 9 loop
      IO.Put (Num (i), 1);  UIO.Put (gen_n (i, False), 7);
      UIO.Put (sub_fact (i), 7);  New_Line;
   end loop;
   Put_Line ("!20 = " & U64'Image (sub_fact (20)));
end DePermute;
```

```txt
Deranged 4:
 4 1 2 3
 4 3 1 2
 4 3 2 1
 3 4 2 1
 3 4 1 2
 3 1 4 2
 2 4 1 3
 2 3 4 1
 2 1 4 3
List vs. calc:
0      1      1
1      0      0
2      1      1
3      2      2
4      9      9
5     44     44
6    265    265
7   1854   1854
8  14833  14833
9 133496 133496
!20 =  895014631192902121
```



## AutoHotkey

Note that the permutations are generated in lexicographic order, from http://www.autohotkey.com/forum/topic77959.html

```AHK
#NoEnv
SetBatchLines -1
Process, Priority,, high

output := "Derangements for 1, 2, 3, 4:`n"

obj := [1, 2, 3, 4], objS := obj.Clone()
Loop ; permute 4
{
	obj := perm_NextObj(Obj)
	If !obj
		break
	For k, v in obj
		if ( objS[k] = v )
			continue 2
	output .= ObjDisp(obj) "`n"
}
output .= "`nTable of n, counted, calculated derangements:`n"

Loop 10 ; Count !n
{
	obj := []
	count := 0
	output .= A_Tab . (i := A_Index-1) . A_Tab
	Loop % i
		obj[A_Index] := A_Index
	objS := obj.Clone()
	Loop
	{
		obj := perm_NextObj(Obj)
		If !obj
			break
		For k, v in obj
			if ( objS[k] = v )
				continue 2
		count++
	}
	output .= count . A_Tab . cd(i) . "`n"
}
output .= "`nApproximation of !20: " . cd(20)
MsgBox % Clipboard := output

perm_NextObj(obj){ ; next lexicographic permutation
	p := 0, objM := ObjMaxIndex(obj)
	Loop % objM
	{
		If A_Index=1
			continue
		t := obj[objM+1-A_Index]
		n := obj[objM+2-A_Index]
		If ( t < n )
		{
			p := objM+1-A_Index, pC := obj[p]
			break
		}
	}
	If !p
		return false
	Loop
	{
		t := obj[objM+1-A_Index]
		If ( t > pC )
		{
			n := objM+1-A_Index, nC := obj[n]
			break
		}
	}

	obj[n] := pC, obj[p] := nC
	return ObjReverse(obj, objM-p)
}

ObjReverse(Obj, tail){
 o := ObjClone(Obj), ObjM := ObjMaxIndex(O)
 Loop % tail
	o[ObjM-A_Index+1] := Obj[ObjM+A_Index-tail]
 return o
}

ObjDisp(obj){
	For k, v in obj
		s .= v ", "
	return SubStr(s, 1, strLen(s)-2)
}


cd(n){  ; Count Derangements
	static e := 2.71828182845904523536028747135
	return n ? floor(ft(n)/e + 1/2) : 1
}
ft(n){  ; FacTorial
	a := 1
	Loop % n
		a *= A_Index
	return a
}
```

```txt
Derangements for 1, 2, 3, 4:
2, 1, 4, 3
2, 3, 4, 1
2, 4, 1, 3
3, 1, 4, 2
3, 4, 1, 2
3, 4, 2, 1
4, 1, 2, 3
4, 3, 1, 2
4, 3, 2, 1

Table of n, counted, calculated derangements:
	0	0	1
	1	0	0
	2	1	1
	3	2	2
	4	9	9
	5	44	44
	6	265	265
	7	1854	1854
	8	14833	14833
	9	133496	133496

Approximation of !20: 895014631192902144
```


## BBC BASIC

```BBC BASIC
      PRINT"Derangements for the numbers 0,1,2,3 are:"
      Count% = FN_Derangement_Generate(4,TRUE)

      PRINT'"Table of n, counted derangements, calculated derangements :"

      FOR I% = 0 TO 9
        PRINT I%, FN_Derangement_Generate(I%,FALSE), FN_SubFactorial(I%)
      NEXT

      PRINT'"There is no long int in BBC BASIC!"
      PRINT"!20 = ";FN_SubFactorial(20)

      END

      DEF FN_Derangement_Generate(N%, fPrintOut)
      LOCAL A%(), O%(), C%, D%, I%, J%
      IF N% = 0 THEN = 1
      DIM A%(N%-1), O%(N%-1)
      FOR I% = 0 TO N%-1 : A%(I%) = I% : NEXT
      O%() = A%()
      FOR I% = 0 TO FN_Factorial(DIM(A%(),1)+1)-1
        PROC_NextPermutation(A%())
        D% = TRUE
        FOR J%=0 TO N%-1
          IF A%(J%) = O%(J%) THEN D% = FALSE
        NEXT
        IF D% THEN
          C% += 1
          IF fPrintOut THEN
            FOR K% = 0 TO N%-1
              PRINT ;A%(K%);" ";
            NEXT
            PRINT
          ENDIF
        ENDIF
      NEXT
      = C%

      DEF PROC_NextPermutation(A%())
      LOCAL first, last, elementcount, pos
      elementcount = DIM(A%(),1)
      IF elementcount < 1 THEN ENDPROC
      pos = elementcount-1
      WHILE A%(pos) >= A%(pos+1)
        pos -= 1
        IF pos < 0 THEN
          PROC_Permutation_Reverse(A%(), 0, elementcount)
          ENDPROC
        ENDIF
      ENDWHILE
      last = elementcount
      WHILE A%(last) <= A%(pos)
        last -= 1
      ENDWHILE
      SWAP A%(pos), A%(last)
      PROC_Permutation_Reverse(A%(), pos+1, elementcount)
      ENDPROC

      DEF PROC_Permutation_Reverse(A%(), firstindex, lastindex)
      LOCAL first, last
      first = firstindex
      last = lastindex
      WHILE first < last
        SWAP A%(first), A%(last)
        first += 1
        last -= 1
      ENDWHILE
      ENDPROC

      DEF FN_Factorial(N) : IF (N = 1) OR (N = 0) THEN =1 ELSE = N * FN_Factorial(N-1)

      DEF FN_SubFactorial(N) : IF N=0 THEN =1 ELSE =N*FN_SubFactorial(N-1)+-1^N

      REM Or you could use:
      REM DEF FN_SubFactorial(N) : IF N<1 THEN =1 ELSE =(N-1)*(FN_SubFactorial(N-1)+FN_SubFactorial(N-2))
```


```txt
Derangements for the numbers 0,1,2,3 are:
1 0 3 2
1 2 3 0
1 3 0 2
2 0 3 1
2 3 0 1
2 3 1 0
3 0 1 2
3 2 0 1
3 2 1 0

Table of n, counted derangements, calculated derangements :
         0         1         1
         1         0         0
         2         1         1
         3         2         2
         4         9         9
         5        44        44
         6       265       265
         7      1854      1854
         8     14833     14833
         9    133496    133496

There is no long int in BBC BASIC!
!20 = 8.95014632E17
>
```



## Bracmat

The function <code>calculated-!n</code> has a local variable <code>mem</code> that memoizes already found counts.
This is done by actually updating the function's definition.
The output of the expression <code>lst$calculated-!n</code> demonstrates this.

The function <code>counted-!n</code> is also special: it is designed to always fail, forcing the match operation on the subject <code>!H</code> to assign each element in <code>!H</code> in turn to the sub-pattern <code>(%@?h:~!p)</code>, except the element that is equal to <code>!p</code>.
The derangements are built up in the last argument and accumulated in the global variable <code>D</code>.
Also the counter <code>count</code> is a global variable.


```bracmat
( ( calculated-!n
  =   memo answ
    .   (memo==)
      & ( !arg:0&1
        | !arg:1&0
        | !(memo.):? (!arg.?answ) ?&!answ
        |       (!arg+-1)
              * (calculated-!n$(!arg+-1)+calculated-!n$(!arg+-2))
            : ?answ
          & (!arg.!answ) !(memo.):?(memo.)
          & !answ
        )
  )
& ( counted-!n
  =   p P h H A Z L
    .     !arg:(%?p ?P.?H.?L)
        &   !H
          :   ?A
              (%@?h:~!p)
              (?Z&counted-!n$(!P.!A !Z.!h !L))
      |   !arg:(..?L)
        & 1+!count:?count
        & (!count.!L) !D:?D
        & ~
  )
& out$"Derangements of 1 2 3 4"
& :?D
& 0:?count
& ( counted-!n$(4 3 2 1.4 3 2 1.)
  | out$!D
  )
& ( pad
  =   len w
    .   @(!arg:? [?len)
      & @("       ":? [!len ?w)
      & !w !arg
  )
& :?K
& -1:?N
& out$(str$(N pad$List pad$Calc))
&   whl
  ' ( !N+1:<10:?N
    & (   0:?count
        & :?D
        & counted-!n$(!K.!K.)
      | out$(str$(!N pad$!count pad$(calculated-!n$!N)))
      )
    & !N !K:?K
    )
& out$("!20 =" calculated-!n$20)
& lst$calculated-!n
)
```

```txt
Derangements of 1 2 3 4
  (9.4 3 2 1)
  (8.3 4 2 1)
  (7.2 3 4 1)
  (6.4 3 1 2)
  (5.3 4 1 2)
  (4.3 1 4 2)
  (3.2 4 1 3)
  (2.4 1 2 3)
  (1.2 1 4 3)
N   List   Calc
0      1      1
1      0      0
2      1      1
3      2      2
4      9      9
5     44     44
6    265    265
7   1854   1854
8  14833  14833
9 133496 133496
!20 = 895014631192902121
(calculated-!n=
  memo answ
.   ( memo
    =
    =   (20.895014631192902121)
        (19.44750731559645106)
        (18.2355301661033953)
        (17.130850092279664)
        (16.7697064251745)
        (15.481066515734)
        (14.32071101049)
        (13.2290792932)
        (12.176214841)
        (11.14684570)
        (10.1334961)
        (9.133496)
        (8.14833)
        (7.1854)
        (6.265)
        (5.44)
        (4.9)
        (3.2)
        (2.1)
    )
  & ( !arg:0&1
    | !arg:1&0
    | !(memo.):? (!arg.?answ) ?&!answ
    |     (!arg+-1)*(calculated-!n$(!arg+-1)+calculated-!n$(!arg+-2))
        : ?answ
      & (!arg.!answ) !(memo.):?(memo.)
      & !answ
    )
);
```



## C


```c
#include <stdio.h>
typedef unsigned long long LONG;

LONG deranged(int depth, int len, int *d, int show)
{
        int i;
        char tmp;
        LONG count = 0;

        if (depth == len) {
                if (show) {
                        for (i = 0; i < len; i++) putchar(d[i] + 'a');
                        putchar('\n');
                }
                return 1;
        }
        for (i = len - 1; i >= depth; i--) {
                if (i == d[depth]) continue;

                tmp = d[i]; d[i] = d[depth]; d[depth] = tmp;
                count += deranged(depth + 1, len, d, show);
                tmp = d[i]; d[i] = d[depth]; d[depth] = tmp;
        }
        return count;
}

LONG gen_n(int n, int show)
{
        LONG i;
        int a[1024]; /* 1024 ought to be big enough for anybody */

        for (i = 0; i < n; i++) a[i] = i;
        return deranged(0, n, a, show);
}

LONG sub_fact(int n)
{
        return n < 2 ? 1 - n : (sub_fact(n - 1) + sub_fact(n - 2)) * (n - 1);
}

int main()
{
        int i;

        printf("Deranged Four:\n");
        gen_n(4, 1);

        printf("\nCompare list vs calc:\n");
        for (i = 0; i < 10; i++)
                printf("%d:\t%llu\t%llu\n", i, gen_n(i, 0), sub_fact(i));

        printf("\nfurther calc:\n");
        for (i = 10; i <= 20; i++)
                printf("%d: %llu\n", i, sub_fact(i));

        return 0;
}
```

```txt
Deranged Four:
dabc
dcab
dcba
cdba
cdab
cadb
bdac
bcda
badc

Compare list vs calc:
0:      1       1
1:      0       0
2:      1       1
3:      2       2
4:      9       9
5:      44      44
6:      265     265
7:      1854    1854
8:      14833   14833
9:      133496  133496

further calc:
10: 1334961
11: 14684570
12: 176214841
13: 2290792932
14: 32071101049
15: 481066515734
16: 7697064251745
17: 130850092279664
18: 2355301661033953
19: 44750731559645106
20: 895014631192902121
```


## C#
Recursive version


```c#

using System;
class Derangements
{
  static int n = 4;
  static int [] buf = new int [n];
  static bool [] used = new bool [n];

  static void Main()
  {
    for (int i = 0; i < n; i++) used [i] = false;
    rec(0);
  }

  static void rec(int ind)
  {
    for (int i = 0; i < n; i++)
    {
      if (!used [i] && i != ind)
      {
        used [i] = true;
        buf [ind] = i;
	if (ind + 1 < n) rec(ind + 1);
        else Console.WriteLine(string.Join(",", buf));
	used [i] = false;
      }
    }
  }
}

```



## Clojure

Generating functions with no fixed point


```Clojure
(ns derangements.core
  (:require [clojure.set :as s]))

(defn subfactorial [n]
  (case n
    0 1
    1 0
    (* (dec n) (+ (subfactorial (dec n)) (subfactorial (- n 2))))))

(defn no-fixed-point
  "f : A -> B must be a biyective function written as a hash-map, returns
  all g : A -> B such that (f(a) = b) => not(g(a) = b)"
  [f]
  (case (count f)
    0 [{}]
    1 []
    (let [g  (s/map-invert f)
          a  (first (keys f))
          a' (f a)]
      (mapcat
       (fn [b'] (let [b  (g b')
                      f' (dissoc f a b)]
                   (concat (map #(reduce conj % [[a b'] [b a']])
                                (no-fixed-point f'))
                           (map #(conj % [a b'])
                                (no-fixed-point (assoc f' b a'))))))
       (filter #(not= a' %) (keys g))))))

(defn derangements [xs]
  {:pre [(= (count xs) (count (set xs)))]}
  (map (fn [f] (mapv f xs))
       (no-fixed-point (into {} (map vector xs xs)))))

(defn -main []
  (do
    (doall (map println (derangements [0,1,2,3])))
    (doall (map #(println (str (subfactorial %) " " (count (derangements (range %)))))
                (range 10)))
    (println (subfactorial 20))))

```

```txt
[1 0 3 2]
[1 2 3 0]
[1 3 0 2]
[2 3 0 1]
[2 3 1 0]
[2 0 3 1]
[3 2 1 0]
[3 2 0 1]
[3 0 1 2]
1 1
0 0
1 1
2 2
9 9
44 44
265 265
1854 1854
14833 14833
133496 133496
895014631192902121
```



## D


### Iterative Version


```d
import std.stdio, std.algorithm, std.typecons, std.conv,
       std.range, std.traits;

T factorial(T)(in T n) pure nothrow @safe @nogc {
    Unqual!T result = 1;
    foreach (immutable i; 2 .. n + 1)
        result *= i;
    return result;
}

T subfact(T)(in T n) pure nothrow @safe @nogc {
    if (0 <= n && n <= 2)
        return n != 1;
    return (n - 1) * (subfact(n - 1) + subfact(n - 2));
}

auto derangements(in size_t n, in bool countOnly=false)
pure nothrow @safe {
    size_t[] seq = n.iota.array;
    auto ori = seq.idup;
    size_t[][] all;
    size_t cnt = n == 0;

    foreach (immutable tot; 0 .. n.factorial - 1) {
        size_t j = n - 2;
        while (seq[j] > seq[j + 1])
            j--;
        size_t k = n - 1;
        while (seq[j] > seq[k])
            k--;
        seq[k].swap(seq[j]);

        size_t r = n - 1;
        size_t s = j + 1;
        while (r > s) {
            seq[s].swap(seq[r]);
            r--;
            s++;
        }

        j = 0;
        while (j < n && seq[j] != ori[j])
            j++;
        if (j == n) {
            if (countOnly)
                cnt++;
            else
                all ~= seq.dup;
        }
    }

    return tuple(all, cnt);
}

void main() @safe {
    "Derangements for n = 4:".writeln;
    foreach (const d; 4.derangements[0])
        d.writeln;

    "\nTable of n vs counted vs calculated derangements:".writeln;
    foreach (immutable i; 0 .. 10)
        writefln("%s  %-7s%-7s", i, derangements(i, 1)[1], i.subfact);

    writefln("\n!20 = %s", 20L.subfact);
}
```

```txt
Derangements for n = 4:
[1, 0, 3, 2]
[1, 2, 3, 0]
[1, 3, 0, 2]
[2, 0, 3, 1]
[2, 3, 0, 1]
[2, 3, 1, 0]
[3, 0, 1, 2]
[3, 2, 0, 1]
[3, 2, 1, 0]

Table of n vs counted vs calculated derangements:
0  1      1
1  0      0
2  1      1
3  2      2
4  9      9
5  44     44
6  265    265
7  1854   1854
8  14833  14833
9  133496 133496

!20 = 895014631192902121
```



### Recursive Version

Slightly slower but more compact recursive version of the derangements function, based on the [[Permutations#D|D entry]] of the permutations task.
Same output.

```d
import std.stdio, std.algorithm, std.typecons, std.conv, std.range;

T factorial(T)(in T n) pure nothrow {
    Unqual!T result = 1;
    foreach (immutable i; 2 .. n + 1)
        result *= i;
    return result;
}

T subfact(T)(in T n) pure nothrow {
    if (0 <= n && n <= 2)
        return n != 1;
    return (n - 1) * (subfact(n - 1) + subfact(n - 2));
}

auto derangementsR(in size_t n, in bool countOnly=false) pure
/*nothrow*/ {
    auto seq = n.iota.array;
    immutable ori = seq.idup;
    const(size_t[])[] res;
    size_t cnt;

    void perms(in size_t[] s, in size_t[] pre=null) /*nothrow*/ {
        if (s.length) {
            foreach (immutable i, immutable c; s)
               perms(s[0 .. i] ~ s[i + 1 .. $], pre ~ c);
        } else if (zip(pre, ori).all!(po => po[0] != po[1])) {
            if (countOnly) cnt++;
            else res ~= pre;
        }
    }

    perms(seq);
    return tuple(res, cnt);
}

void main() {
    "Derangements for n = 4:".writeln;
    foreach (const d; 4.derangementsR[0])
        d.writeln;

    "\nTable of n vs counted vs calculated derangements:".writeln;
    foreach (immutable i; 0 .. 10)
        writefln("%s  %-7s%-7s", i, derangementsR(i, 1)[1], i.subfact);

    writefln("\n!20 = %s", 20L.subfact);
}
```



## EchoLisp


```scheme

(lib 'list) ;; in-permutations
(lib 'bigint)

;; generates derangements by filtering out permutations
(define (derangement? nums) ;; predicate
    (for/and ((n nums) (i (length nums))) (!= n i)))

(define (derangements n)
    (for/list ((p (in-permutations n))) #:when (derangement? p) p))

(define (count-derangements n)
    (for/sum ((p (in-permutations n))) #:when (derangement? p) 1))

;;
;;  !n = (n - 1) (!(n-1) + !(n-2))

(define (!n n)
	(* (1- n) (+ (!n (1- n)) (!n (- n 2)))))
(remember '!n #(1 0))


```

```scheme

(derangements 4)
    → ((3 0 1 2) (2 0 3 1) (2 3 0 1) (3 2 0 1) (3 2 1 0) (2 3 1 0) (1 2 3 0) (1 3 0 2) (1 0 3 2))

;; generated versus computed

(for ((i 10)) (writeln i '| (count-derangements i) (!n i)))

0     |     1     1
1     |     0     0
2     |     1     1
3     |     2     2
4     |     9     9
5     |     44     44
6     |     265     265
7     |     1854     1854
8     |     14833     14833
9     |     133496     133496

(!n 20)
    → 895014631192902121

```



## Elixir

```elixir
defmodule Permutation do
  def derangements(n) do
    list = Enum.to_list(1..n)
    Enum.filter(permutation(list), fn perm ->
      Enum.zip(list, perm) |> Enum.all?(fn {a,b} -> a != b end)
    end)
  end

  def subfact(0), do: 1
  def subfact(1), do: 0
  def subfact(n), do: (n-1) * (subfact(n-1) + subfact(n-2))

  def permutation([]), do: [[]]
  def permutation(list) do
    for x <- list, y <- permutation(list -- [x]), do: [x|y]
  end
end

IO.puts "derangements for n = 4"
Enum.each(Permutation.derangements(4), &IO.inspect &1)

IO.puts "\nNumber of derangements"
IO.puts " n    derange   subfact"
Enum.each(0..9, fn n ->
  :io.format "~2w :~9w,~9w~n", [n, length(Permutation.derangements(n)), Permutation.subfact(n)]
end)
Enum.each(10..20, fn n ->
  :io.format "~2w :~19w~n", [n, Permutation.subfact(n)]
end)
```


```txt

derangements for n = 4
[2, 1, 4, 3]
[2, 3, 4, 1]
[2, 4, 1, 3]
[3, 1, 4, 2]
[3, 4, 1, 2]
[3, 4, 2, 1]
[4, 1, 2, 3]
[4, 3, 1, 2]
[4, 3, 2, 1]

Number of derangements
 n    derange   subfact
 0 :        1,        1
 1 :        0,        0
 2 :        1,        1
 3 :        2,        2
 4 :        9,        9
 5 :       44,       44
 6 :      265,      265
 7 :     1854,     1854
 8 :    14833,    14833
 9 :   133496,   133496
10 :            1334961
11 :           14684570
12 :          176214841
13 :         2290792932
14 :        32071101049
15 :       481066515734
16 :      7697064251745
17 :    130850092279664
18 :   2355301661033953
19 :  44750731559645106
20 : 895014631192902121

```


=={{header|F_Sharp|F#}}==

### The Function


```fsharp

// Generate derangements. Nigel Galloway: July 9th., 2019
let derange n=
  let fG n i g=let e=Array.copy n in e.[i]<-n.[g]; e.[g]<-n.[i]; e
  let rec derange n g α=seq{
    match (α>0,n&&&(1<<<α)=0) with
     (true,true)->for i in [0..α-1] do if n&&&(1<<<i)=0 then let g=(fG g α i) in yield! derange (n+(1<<<i)) g (α-1); yield! derange n g (α-1)
    |(true,false)->yield! derange n g (α-1)
    |(false,false)->yield g
    |_->()}
  derange 0 [|1..n|] (n-1)

```


### The Task


```fsharp

derange 4 |> Seq.iter(printfn "%A")

```

```txt

[|4; 3; 2; 1|]
[|2; 3; 4; 1|]
[|3; 4; 2; 1|]
[|3; 4; 1; 2|]
[|4; 3; 1; 2|]
[|3; 1; 4; 2|]
[|2; 1; 4; 3|]
[|2; 4; 1; 3|]
[|4; 1; 2; 3|]

```


```fsharp

let subFact n=let rec fN n g=match n with 0m->int64(round(g/2.7182818284590452353602874713526624978m))|_->fN (n-1m) (g*n) in if n=0 then 1L else fN (decimal n) 1m
[1..9] |> Seq.iter(fun n->printfn "items=%d !n=%d derangements=%d" n (subFact n) (derange n|>Seq.length))

```

```txt

items=1 !n=0 derangements=0
items=2 !n=1 derangements=1
items=3 !n=2 derangements=2
items=4 !n=9 derangements=9
items=5 !n=44 derangements=44
items=6 !n=265 derangements=265
items=7 !n=1854 derangements=1854
items=8 !n=14833 derangements=14833
items=9 !n=133496 derangements=133496

```



## Factor

```factor
USING: combinators formatting io kernel math math.combinatorics
prettyprint sequences ;
IN: rosetta-code.derangements

: !n ( n -- m )
    {
        { 0 [ 1 ] }
        { 1 [ 0 ] }
        [ [ 1 - !n ] [ 2 - !n + ] [ 1 - * ] tri ]
    } case ;

: derangements ( n -- seq )
    <iota> dup [ [ = ] 2map [ f = ] all? ] with
    filter-permutations ;

"4 derangements" print 4 derangements . nl
"n   count    calc\n=
### ===  ===
" print
10 <iota> [
    dup [ derangements length ] [ !n ] bi
    "%d%8d%8d\n" printf
] each nl
"!20 = " write 20 !n .
```

```txt

4 derangements
V{
    { 1 0 3 2 }
    { 1 2 3 0 }
    { 1 3 0 2 }
    { 2 0 3 1 }
    { 2 3 0 1 }
    { 2 3 1 0 }
    { 3 0 1 2 }
    { 3 2 0 1 }
    { 3 2 1 0 }
}

n   count    calc
=
### ===  ===

0       1       1
1       0       0
2       1       1
3       2       2
4       9       9
5      44      44
6     265     265
7    1854    1854
8   14833   14833
9  133496  133496

!20 = 895014631192902121

```



## FreeBASIC


```freebasic
' version 08-04-2017
' compile with: fbc -s console

Sub Subfactorial(a() As ULongInt)

    Dim As ULong i
    Dim As ULongInt num

    For i = 0 To UBound(a)
        num = num * i
        If (i And 1) = 1 Then
            num -= 1
        Else
            num += 1
        End If
        a(i) = num
    Next

End Sub

' Heap's algorithm non-recursive
Function perms_derange(n As ULong, flag As Long = 0) As ULongInt
    ' fast upto n < 12
    If n = 0 Then Return 1

    Dim As ULong i, j, c1, count
    Dim As ULong a(0 To n -1), c(0 To n -1)

    For j = 0 To n -1
        a(j) = j
    Next

    While i < n
        If c(i) < i Then
            If (i And 1) = 0 Then
                Swap a(0), a(i)
            Else
                Swap a(c(i)), a(i)
            End If
            For j = 0 To n -1
                If a(j) = j Then j = 99
            Next
            If j < 99 Then
                count += 1
                If flag = 0 Then
                    c1 += 1
                    For j = 0 To n -1
                        Print a(j);
                    Next
                    If c1 > 12 Then
                        Print : c1 = 0
                    Else
                        Print " ";
                    End If
                End If
            End If
            c(i) += 1
            i = 0
        Else
            c(i) = 0
            i += 1
        End If
    Wend
    If flag = 0 AndAlso c1 <> 0 Then Print
    Return count

End Function

' ------=< MAIN >=------

Dim As ULong i, n = 4
Dim As ULongInt subfac(20)

Subfactorial(subfac())

Print "permutations derangements for n = "; n
i = perms_derange(n)
Print "count returned = "; i; " , !"; n; " calculated = "; subfac(n)

Print
Print "count  counted subfactorial"
Print "---------------------------"
For i = 0 To 9
    Print Using " ###: ########     ########"; i; perms_derange(i, 1); subfac(i)
Next
For i = 10 To 20
    Print Using " ###:   ###################"; i; subfac(i)
Next

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
permutations derangements for n = 4
1302 3012 1032 2031 2301 3201 3210 2310 1230
count returned = 9 , !4 calculated = 9

count  counted subfactorial
---------------------------
   0:        1            1
   1:        0            0
   2:        1            1
   3:        2            2
   4:        9            9
   5:       44           44
   6:      265          265
   7:     1854         1854
   8:    14833        14833
   9:   133496       133496
  10:               1334961
  11:              14684570
  12:             176214841
  13:            2290792932
  14:           32071101049
  15:          481066515734
  16:         7697064251745
  17:       130850092279664
  18:      2355301661033953
  19:     44750731559645106
  20:    895014631192902121
```



## GAP


```gap
# All of this is built-in
Derangements([1 .. 4]);
# [ [ 2, 1, 4, 3 ], [ 2, 3, 4, 1 ], [ 2, 4, 1, 3 ], [ 3, 1, 4, 2 ], [ 3, 4, 1, 2 ], [ 3, 4, 2, 1 ],
#   [ 4, 1, 2, 3 ], [ 4, 3, 1, 2 ], [ 4, 3, 2, 1 ] ]
Size(last);
# 9

NrDerangements([1 .. 4]);
# 9

# An implementation using formula D(n + 1) = n*(D(n) + D(n - 1))
NrDerangementsAlt_memo := [1, 0];
NrDerangementsAlt := function(n)
	if not IsBound(NrDerangementsAlt_memo[n + 1]) then
		NrDerangementsAlt_memo[n + 1] := (n - 1)*(NrDerangementsAlt(n - 1) + NrDerangementsAlt(n - 2));
	fi;
	return NrDerangementsAlt_memo[n + 1];
end;

L := List([0 .. 9]);

PrintArray(TransposedMat([L,
	List(L, n -> Size(Derangements([1 .. n]))),
	List(L, n -> NrDerangements([1 .. n])),
	List(L, NrDerangementsAlt)]));
# [ [       0,       1,       1,       1 ],
#   [       1,       0,       0,       0 ],
#   [       2,       1,       1,       1 ],
#   [       3,       2,       2,       2 ],
#   [       4,       9,       9,       9 ],
#   [       5,      44,      44,      44 ],
#   [       6,     265,     265,     265 ],
#   [       7,    1854,    1854,    1854 ],
#   [       8,   14833,   14833,   14833 ],
#   [       9,  133496,  133496,  133496 ] ]
```


## Go


```go
package main

import (
    "fmt"
    "math/big"
)

// task 1: function returns list of derangements of n integers
func dList(n int) (r [][]int) {
    a := make([]int, n)
    for i := range a {
        a[i] = i
    }
    // recursive closure permutes a
    var recurse func(last int)
    recurse = func(last int) {
        if last == 0 {
            // bottom of recursion.  you get here once for each permutation.
            // test if permutation is deranged.
            for j, v := range a {
                if j == v {
                    return // no, ignore it
                }
            }
            // yes, save a copy
            r = append(r, append([]int{}, a...))
            return
        }
        for i := last; i >= 0; i-- {
            a[i], a[last] = a[last], a[i]
            recurse(last - 1)
            a[i], a[last] = a[last], a[i]
        }
    }
    recurse(n - 1)
    return
}

// task 3: function computes subfactorial of n
func subFact(n int) *big.Int {
    if n == 0 {
        return big.NewInt(1)
    } else if n == 1 {
        return big.NewInt(0)
    }
    d0 := big.NewInt(1)
    d1 := big.NewInt(0)
    f := new(big.Int)
    for i, n64 := int64(1), int64(n); i < n64; i++ {
        d0, d1 = d1, d0.Mul(f.SetInt64(i), d0.Add(d0, d1))
    }
    return d1
}

func main() {
    // task 2:
    fmt.Println("Derangements of 4 integers")
    for _, d := range dList(4) {
        fmt.Println(d)
    }

    // task 4:
    fmt.Println("\nNumber of derangements")
    fmt.Println("N  Counted  Calculated")
    for n := 0; n <= 9; n++ {
        fmt.Printf("%d %8d %11s\n", n, len(dList(n)), subFact(n).String())
    }

    // stretch (sic)
    fmt.Println("\n!20 =", subFact(20))
}
```

```txt

Derangements of 4 integers
[1 0 3 2]
[3 0 1 2]
[1 3 0 2]
[2 0 3 1]
[2 3 0 1]
[3 2 0 1]
[3 2 1 0]
[2 3 1 0]
[1 2 3 0]

Number of derangements
N  Counted  Calculated
0        0           1
1        0           0
2        1           1
3        2           2
4        9           9
5       44          44
6      265         265
7     1854        1854
8    14833       14833
9   133496      133496

!20 = 895014631192902121

```



## Groovy

Solution:

```groovy
def fact = { n -> [1,(1..<(n+1)).inject(1) { prod, i -> prod * i }].max() }
def subfact
subfact = { BigInteger n -> (n == 0) ? 1 : (n == 1) ? 0 : ((n-1) * (subfact(n-1) + subfact(n-2))) }

def derangement = { List l ->
    def d = []
  if (l) l.eachPermutation { p -> if ([p,l].transpose().every{ pp, ll -> pp != ll }) d << p }
    d
}
```


Test:

```groovy
def d = derangement([1,2,3,4])
assert d.size() == subfact(4)
d.each { println it }

println """
n   # derangements   subfactorial
=
### ===========   =========
"""
(0..9). each { n ->
    def dr = derangement((1..<(n+1)) as List)
    def sf = subfact(n)
    printf('%1d   %14d   %12d\n', n, dr.size(), sf)
}

println """
!20 == ${subfact(20)}
"""
```


```txt
[2, 1, 4, 3]
[2, 3, 4, 1]
[2, 4, 1, 3]
[3, 1, 4, 2]
[3, 4, 1, 2]
[3, 4, 2, 1]
[4, 1, 2, 3]
[4, 3, 1, 2]
[4, 3, 2, 1]

n   # derangements   subfactorial
=
### ===========   =========

0                1              1
1                0              0
2                1              1
3                2              2
4                9              9
5               44             44
6              265            265
7             1854           1854
8            14833          14833
9           133496         133496

!20 == 895014631192902121
```



## Haskell



```Haskell
import Control.Monad (forM_)

import Data.List (permutations)

-- Compute all derangements of a list
derangements
  :: Eq a
  => [a] -> [[a]]
derangements = (\x -> filter (and . zipWith (/=) x)) <*> permutations

-- Compute the number of derangements of n elements
subfactorial
  :: (Eq a, Num a)
  => a -> a
subfactorial 0 = 1
subfactorial 1 = 0
subfactorial n = (n - 1) * (subfactorial (n - 1) + subfactorial (n - 2))

main :: IO ()
main
-- Generate and show all the derangements of four integers
 = do
  print $ derangements [1 .. 4]
  putStrLn ""
  -- Print the count of derangements vs subfactorial
  forM_ [1 .. 9] $
    \i ->
       putStrLn $
       mconcat
         [show (length (derangements [1 .. i])), " ", show (subfactorial i)]
  putStrLn ""
  -- Print the number of derangements in a list of 20 items
  print $ subfactorial 20
```

```txt
[[4,3,2,1],[3,4,2,1],[2,3,4,1],[4,1,2,3],[2,4,1,3],[2,1,4,3],[4,3,1,2],[3,4,1,2],[3,1,4,2]]

0 0
1 1
2 2
9 9
44 44
265 265
1854 1854
14833 14833
133496 133496

895014631192902121
```


Alternatively, this is a backtracking method:


```haskell
derangements xs = loop xs xs
    where loop [] [] = [[]]
          loop (h:hs) xs = [x:ys | x <- xs, x /= h, ys <- loop hs (delete x xs)]
```


Since the value <i>i</I> cannot occur in position <i>i</i>, we prefix <i>i</i> on all other derangements from 1 to <i>n</i> that do not include <i>i</i>.  The first method of filtering permutations is significantly faster, in practice, however.


## J

Note: <code>!n</code> in J denotes factorial (or gamma n+1), and not subfactorial.


```j
derangement=: (A.&i.~ !)~ (*/ .~: # [) i.  NB. task item 1
subfactorial=: ! * +/@(_1&^ % !)@i.@>:  NB. task item 3
```


Requested examples:


```j
   derangement 4 NB. task item 2
1 0 3 2
1 2 3 0
1 3 0 2
2 0 3 1
2 3 0 1
2 3 1 0
3 0 1 2
3 2 0 1
3 2 1 0
   (,subfactorial,#@derangement)"0 i.10  NB. task item 4
0      1      1
1      0      0
2      1      1
3      2      2
4      9      9
5     44     44
6    265    265
7   1854   1854
8  14833  14833
9 133496 133496
   subfactorial 20 NB. stretch task
8.95015e17
   subfactorial 20x NB. using extended precision
895014631192902121
```


Note that derangement 10 was painfully slow (almost 3 seconds, about 10 times slower than derangement 9 and 100 times slower than derangement 8) -- this is a brute force approach.  But brute force seems like an appropriate solution here, since factorial divided by subfactorial asymptotically approaches a value near  0.367879 (the reciprocal of e).


## Java

```java
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Derangement {

    public static void main(String[] args) {
        System.out.println("derangements for n = 4\n");
        for (Object d  : (ArrayList)(derangements(4, false)[0])) {
            System.out.println(Arrays.toString((int[])d));
        }

        System.out.println("\ntable of n vs counted vs calculated derangements\n");
        for (int i = 0; i < 10; i++) {
            int d = ((Integer)derangements(i, true)[1]).intValue();
            System.out.printf("%d  %-7d %-7d\n", i, d, subfact(i));
        }

        System.out.printf ("\n!20 = %20d\n", subfact(20L));
    }

    static Object[] derangements(int n, boolean countOnly) {
        int[] seq = iota(n);
        int[] ori = Arrays.copyOf(seq, n);
        long tot = fact(n);

        List<int[]> all = new ArrayList<int[]>();
        int cnt = n == 0 ? 1 : 0;

        while (--tot > 0) {
            int j = n - 2;
            while (seq[j] > seq[j + 1]) {
                j--;
            }
            int k = n - 1;
            while (seq[j] > seq[k]) {
                k--;
            }
            swap(seq, k, j);

            int r = n - 1;
            int s = j + 1;
            while (r > s) {
                swap(seq, s, r);
                r--;
                s++;
            }

            j = 0;
            while (j < n && seq[j] != ori[j]) {
                j++;
            }
            if (j == n) {
                if (countOnly) {
                    cnt++;
                } else {
                    all.add(Arrays.copyOf(seq, n));
                }
            }
        }
        return new Object[]{all, cnt};
    }

    static long fact(long n) {
        long result = 1;
        for (long i = 2; i <= n; i++) {
            result *= i;
        }
        return result;
    }

    static long subfact(long n) {
        if (0 <= n && n <= 2) {
            return n != 1 ? 1 : 0;
        }
        return (n - 1) * (subfact(n - 1) + subfact(n - 2));
    }

    static void swap(int[] arr, int lhs, int rhs) {
        int tmp = arr[lhs];
        arr[lhs] = arr[rhs];
        arr[rhs] = tmp;
    }

    static int[] iota(int n) {
        if (n < 0) {
            throw new IllegalArgumentException("iota cannot accept < 0");
        }
        int[] r = new int[n];
        for (int i = 0; i < n; i++) {
            r[i] = i;
        }
        return r;
    }
}
```



```txt
derangements for n = 4

[1, 0, 3, 2]
[1, 2, 3, 0]
[1, 3, 0, 2]
[2, 0, 3, 1]
[2, 3, 0, 1]
[2, 3, 1, 0]
[3, 0, 1, 2]
[3, 2, 0, 1]
[3, 2, 1, 0]

table of n vs counted vs calculated derangements

0  1       1
1  0       0
2  1       1
3  2       2
4  9       9
5  44      44
6  265     265
7  1854    1854
8  14833   14833
9  133496  133496

!20 =   895014631192902121
```



## jq

The following implementation of "derangements" generates the derangements directly, without generating all permutations. Since recent versions of jq have tail-call optimization (TCO) for arity-0 recursive functions, the workhorse inner function (deranged/0) is implemented as an arity-0 function.

```jq
def derangements:

  # In order to reference the original array conveniently, define _derangements(ary):
  def _derangements(ary):
    # We cannot put the i-th element in the i-th place:
    def deranged:  # state: [i, available]
      .[0] as $i | .[1] as $in
      | if $i == (ary|length) then []
        else
        ($in[] | select (. != ary[$i])) as $j
        |  [$j] + ([$i+1, ($in - [$j])] | deranged)
        end
      ;
    [0,ary]|deranged;
    . as $in | _derangements($in);

def subfact:
  if . == 0 then 1
  elif . == 1 then 0
  else (.-1) * (((.-1)|subfact) + ((.-2)|subfact))
  end;

# Avoid creating an array just to count the items in a stream:
def count(g): reduce g as $i (0; . + 1);
```

'''Tasks''':

```jq
 "Derangements:",
 ([range(1;5)] | derangements),
 "",
 "Counted vs Computed Derangments:",
 (range(1;10) as $i | "\($i): \(count( [range(0;$i)] | derangements)) vs \($i|subfact)"),
 "",
 "Computed approximation to !20 (15 significant digits): \(20|subfact)"
```

```sh
$ jq -n -c -r -f derangements.jq
jq -n -c -r -f derangements.jq

Derangements:
[2,1,4,3]
[2,3,4,1]
[2,4,1,3]
[3,1,4,2]
[3,4,1,2]
[3,4,2,1]
[4,1,2,3]
[4,3,1,2]
[4,3,2,1]

Counted vs Computed Derangments:
1: 0 vs 0
2: 1 vs 1
3: 2 vs 2
4: 9 vs 9
5: 44 vs 44
6: 265 vs 265
7: 1854 vs 1854
8: 14833 vs 14833
9: 133496 vs 133496

Computed approximation to !20 (15 significant digits): 895014631192902000
```



## Julia

```julia
# v0.6

using Combinatorics

derangements(n::Int) = (perm for perm in permutations(1:n)
                        if all(indx != p for (indx, p) in enumerate(perm)))

function subfact(n::Integer)::Integer
    if n in (0, 2)
        return 1
    elseif n == 1
        return 0
    elseif 1 ≤ n ≤ 18
        return round(Int, factorial(n) / e)
    elseif n > 0
        return (n - 1) * ( subfact(n - 1) + subfact(n - 2) )
    else
        error()
    end
end

println("Derangements of [1, 2, 3, 4]")
for perm in derangements(4)
    println(perm)
end

@printf("\n%5s%13s%13s\n", "n", "derangements", "!n")
for n in 1:10
    ders = derangements(n)
    subf = subfact(n)
    @printf("%5i%13i%13i\n", n, length(collect(ders)), subf)
end

println("\n!20 = ", subfact(20))
```


```txt
Derangements of [1, 2, 3, 4]
[2, 1, 4, 3]
[2, 3, 4, 1]
[2, 4, 1, 3]
[3, 1, 4, 2]
[3, 4, 1, 2]
[3, 4, 2, 1]
[4, 1, 2, 3]
[4, 3, 1, 2]
[4, 3, 2, 1]

    n derangements           !n
    1            0            0
    2            1            1
    3            2            2
    4            9            9
    5           44           44
    6          265          265
    7         1854         1854
    8        14833        14833
    9       133496       133496
   10      1334961      1334961

!20 = 895014631192902121
```



## Kotlin


```scala
// version 1.1.2

fun <T> permute(input: List<T>): List<List<T>> {
    if (input.size == 1) return listOf(input)
    val perms = mutableListOf<List<T>>()
    val toInsert = input[0]
    for (perm in permute(input.drop(1))) {
        for (i in 0..perm.size) {
            val newPerm = perm.toMutableList()
            newPerm.add(i, toInsert)
            perms.add(newPerm)
        }
    }
    return perms
}

fun derange(input: List<Int>): List<List<Int>> {
    if (input.isEmpty()) return listOf(input)
    return permute(input).filter { permutation ->
        permutation.filterIndexed { i, index -> i == index }.none()
    }
}

fun subFactorial(n: Int): Long =
    when (n) {
        0 -> 1
        1 -> 0
        else -> (n - 1) * (subFactorial(n - 1) + subFactorial(n - 2))
    }

fun main(args: Array<String>) {
    val input = listOf(0, 1, 2, 3)

    val derangements = derange(input)
    println("There are ${derangements.size} derangements of $input, namely:\n")
    derangements.forEach(::println)

    println("\nN  Counted   Calculated")
    println("-  -------   ----------")
    for (n in 0..9) {
        val list = List(n) { it }
        val counted = derange(list).size
        println("%d  %-9d %-9d".format(n, counted, subFactorial(n)))
    }
    println("\n!20 = ${subFactorial(20)}")
}
```


```txt

There are 9 derangements of [0, 1, 2, 3], namely:

[1, 2, 3, 0]
[2, 0, 3, 1]
[2, 3, 0, 1]
[2, 3, 1, 0]
[1, 0, 3, 2]
[1, 3, 0, 2]
[3, 0, 1, 2]
[3, 2, 0, 1]
[3, 2, 1, 0]

N  Counted   Calculated
-  -------   ----------
0  1         1
1  0         0
2  1         1
3  2         2
4  9         9
5  44        44
6  265       265
7  1854      1854
8  14833     14833
9  133496    133496

!20 = 895014631192902121

```



## Lua


```Lua
-- Return an iterator to produce every permutation of list
function permute (list)
    local function perm (list, n)
        if n == 0 then coroutine.yield(list) end
        for i = 1, n do
            list[i], list[n] = list[n], list[i]
            perm(list, n - 1)
            list[i], list[n] = list[n], list[i]
        end
    end
    return coroutine.wrap(function() perm(list, #list) end)
end

-- Return a copy of table t (wouldn't work for a table of tables)
function copy (t)
    if not t then return nil end
    local new = {}
    for k, v in pairs(t) do new[k] = v end
    return new
end

-- Return true if no value in t1 can be found at the same index of t2
function noMatches (t1, t2)
    for k, v in pairs(t1) do
        if t2[k] == v then return false end
    end
    return true
end

-- Return a table of all derangements of table t
function derangements (t)
    local orig = copy(t)
    local nextPerm, deranged = permute(t), {}
    local numList, keep = copy(nextPerm())
    while numList do
        if noMatches(numList, orig) then table.insert(deranged, numList) end
        numList = copy(nextPerm())
    end
    return deranged
end

-- Return the subfactorial of n
function subFact (n)
    if n < 2 then
        return 1 - n
    else
        return (subFact(n - 1) + subFact(n - 2)) * (n - 1)
    end
end

-- Return a table of the numbers 1 to n
function listOneTo (n)
    local t = {}
    for i = 1, n do t[i] = i end
    return t
end

-- Main procedure
print("Derangements of [1,2,3,4]")
for k, v in pairs(derangements(listOneTo(4))) do print("", unpack(v)) end
print("\n\nSubfactorial vs counted derangements\n")
print("\tn\t| subFact(n)\t| Derangements")
print("    " .. string.rep("-", 42))
for i = 0, 9 do
    io.write("\t" .. i .. "\t|  " .. subFact(i))
    if string.len(subFact(i)) < 5 then io.write("\t") end
    print("\t|  " .. #derangements(listOneTo(i)))
end
print("\n\nThe subfactorial of 20 is " .. subFact(20))
```

```txt
Derangements of [1,2,3,4]
        2       3       4       1
        3       4       2       1
        4       3       2       1
        4       3       1       2
        3       4       1       2
        3       1       4       2
        2       4       1       3
        4       1       2       3
        2       1       4       3


Subfactorial vs counted derangements

        n       | subFact(n)    | Derangements
    ------------------------------------------
        0       |  1            |  1
        1       |  0            |  0
        2       |  1            |  1
        3       |  2            |  2
        4       |  9            |  9
        5       |  44           |  44
        6       |  265          |  265
        7       |  1854         |  1854
        8       |  14833        |  14833
        9       |  133496       |  133496


The subfactorial of 20 is 8.950146311929e+17
```



## Mathematica


```Mathematica

Needs["Combinatorica`"]
derangements[n_] := Derangements[Range[n]]
derangements[4]
Table[{NumberOfDerangements[i], Subfactorial[i]}, {i, 9}] // TableForm
Subfactorial[20]
```

```txt

{{2, 1, 4, 3}, {2, 3, 4, 1}, {2, 4, 1, 3}, {3, 1, 4, 2}, {3, 4, 1, 2},
 {3, 4, 2, 1}, {4, 1, 2, 3}, {4, 3, 1, 2}, {4, 3, 2, 1}}

0	0
1	1
2	2
9	9
44	44
265	265
1854	1854
14833	14833
133496	133496

895014631192902121
```



## PARI/GP


```parigp
derangements(n)=if(n,round(n!/exp(1)),1);
derange(n)={
	my(v=[[]],tmp);
	for(level=1,n,
		tmp=List();
		for(i=1,#v,
			for(k=1,n,
				if(k==level, next);
				for(j=1,level-1,if(v[i][j]==k, next(2)));
				listput(tmp, concat(v[i],k))
			)
		);
		v=Vec(tmp)
	);
	v
};
derange(4)
for(n=0,9,print("!"n" = "#derange(n)" = "derangements(n)))
derangements(20)
```

```txt
%1 = [[2, 1, 4, 3], [2, 3, 4, 1], [2, 4, 1, 3], [3, 1, 4, 2], [3, 4, 1, 2], [3, 4, 2, 1], [4, 1, 2, 3], [4, 3, 1, 2], [4, 3, 2, 1]]

!0 = 1 = 1
!1 = 0 = 0
!2 = 1 = 1
!3 = 2 = 2
!4 = 9 = 9
!5 = 44 = 44
!6 = 265 = 265
!7 = 1854 = 1854
!8 = 14833 = 14833
!9 = 133496 = 133496

%2 = 895014631192902121
```



## Perl


### Traditional verbose version


```Perl
sub d {
        # compare this with the deranged() sub to see how to turn procedural
        # code into functional one ('functional' as not in 'understandable')
        $#_ ? map d([ @{$_[0]}, $_[$_] ], @_[1 .. $_-1, $_+1 .. $#_ ]),
                        grep { $_[$_] != @{$_[0]} } 1 .. $#_
            : $_[0]
}

sub deranged {  # same as sub d above, just a readable version to explain method
        my ($result, @avail) = @_;
        return $result if !@avail;              # no more elements to pick from, done

        my @list;                               # list of permutations to return
        for my $i (0 .. $#avail) {              # try to add each element to result in turn
                next if $avail[$i] == @$result; # element n at n-th position, no-no
                my $e = splice @avail, $i, 1;   # move the n-th element from available to result
                push @list, deranged([ @$result, $e ], @avail);
                                                # and recurse down, keep what's returned
                splice @avail, $i, 0, $e;       # put that element back, try next
        }
        return @list;
}

sub choose {    # choose k among n, i.e. n! / k! (n-k)!
        my ($n, $k) = @_;
        factorial($n) / factorial($k) / factorial($n - $k)
}

my @fact = (1);
sub factorial {
        # //= : standard caching technique.  If cached value available,
        #       return it; else compute, cache and return.
        #       For this specific task not really necessary.
        $fact[ $_[0] ] //= $_[0] * factorial($_[0] - 1)
}

my @subfact;
sub sub_factorial {
        my $n = shift;
        $subfact[$n] //= do     # same caching stuff, try comment out this line
        {
                # computes deranged without formula, using recursion
                my $total = factorial($n);      # total permutations
                for my $k (1 .. $n) {
                        # minus the permutations where k items are fixed
                        # to original location, and the rest deranged
                        $total -= choose($n, $k) * sub_factorial($n - $k)
                }
                $total
        }
}

print "Derangements for 4 elements:\n";
my @deranged = d([], 0 .. 3);
for (1 .. @deranged) {
        print "$_: @{$deranged[$_-1]}\n"
}

print "\nCompare list length and calculated table\n";
for (0 .. 9) {
        my @x = d([], 0 .. $_-1);
        print $_, "\t", scalar(@x), "\t", sub_factorial($_), "\n"
}

print "\nNumber of derangements:\n";
print "$_:\t", sub_factorial($_), "\n" for 1 .. 20;
```


```txt
Derangements for 4 elements:
1: 1 0 3 2
2: 1 2 3 0
3: 1 3 0 2
4: 2 0 3 1
5: 2 3 0 1
6: 2 3 1 0
7: 3 0 1 2
8: 3 2 0 1
9: 3 2 1 0

Compare list length and calculated table
0       1       1
1       0       0
2       1       1
3       2       2
4       9       9
5       44      44
6       265     265
7       1854    1854
8       14833   14833
9       133496  133496

Number of derangements:
1:      0
2:      1
3:      2
4:      9
5:      44
6:      265
7:      1854
8:      14833
9:      133496
10:     1334961
11:     14684570
12:     176214841
13:     2290792932
14:     32071101049
15:     481066515734
16:     7697064251745
17:     130850092279664
18:     2355301661033953
19:     44750731559645106
20:     895014631192902121
```



### Using a module

```perl
use ntheory ":all";

# Count derangements using derangement iterator
sub countderange {
  my($n,$s) = (shift,0);
  forderange { $s++ } $n;
  $s;
}
# Count derangements using inclusion-exclusion
sub subfactorial1 {
  my $n = shift;
  vecsum(map{ vecprod((-1)**($n-$_),binomial($n,$_),factorial($_)) }0..$n);
}
# Count derangements using simple recursion without special functions
sub subfactorial2 {
  my $n = shift;
  use bigint;  no warnings 'recursion';
  ($n < 1)  ?  1  :  $n * subfactorial2($n-1) + (-1)**$n;
}

print "Derangements of 4 items:\n";
forderange { print "@_\n" } 4;
printf "\n%3s %15s %15s\n","N","List count","!N";
printf "%3d %15d %15d %15d\n",$_,countderange($_),subfactorial1($_),subfactorial2($_) for 0..9;
printf "%3d %15s %s\n",$_,"",subfactorial2($_) for 20,200;
```

```txt

Derangements of 4 items:
1 0 3 2
1 2 3 0
1 3 0 2
2 0 3 1
2 3 0 1
2 3 1 0
3 0 1 2
3 2 0 1
3 2 1 0

  N      List count   !N (binomial)  !N (recursion)
  0               1               1               1
  1               0               0               0
  2               1               1               1
  3               2               2               2
  4               9               9               9
  5              44              44              44
  6             265             265             265
  7            1854            1854            1854
  8           14833           14833           14833
  9          133496          133496          133496
 20                 895014631192902121
200                 290131015521620609254546237518688936375622413566095185632876940298382875066633305125595907908697818551860745708196640009079772455670451355426573609799907339222509103785567575227183775791345718826220455840965346196540544976439608810006794385963854831693077054723298130736781093200499800934036993104223443563872463385599425635345341317933466521378117877578807421014599223577201

```



## Perl 6


Generate <code>List.permutations</code> and keep the ones where no elements are in their original position. This is done by zipping each permutation with the original list, and keeping the ones where none of the zipped pairs are equal.

I am using the <code>Z</code> infix zip operator with the <code>eqv</code> equivalence infix operator, all wrapped inside a <code>none()</code> Junction.

Although not necessary for this task, I have used <code>eqv</code> instead of <code>==</code> so that the <code>derangements()</code> function also works with any set of arbitrary objects (eg. strings, lists, etc.)


```perl6
sub derangements(@l) {
    @l.permutations.grep(-> @p { none(@p Zeqv @l) })
}

sub prefix:<!>(Int $n) {
    (1, 0, 1, -> $a, $b { ($++ + 2) × ($b + $a) } ... *)[$n]
}

say 'derangements([1, 2, 3, 4])';
say derangements([1, 2, 3, 4]), "\n";

say 'n == !n == derangements(^n).elems';
for 0 .. 9 -> $n {
    say "!$n == { !$n } == { derangements(^$n).elems }"
}
```

```txt

derangements([1, 2, 3, 4])
((2 1 4 3) (2 3 4 1) (2 4 1 3) (3 1 4 2) (3 4 1 2) (3 4 2 1) (4 1 2 3) (4 3 1 2) (4 3 2 1))

n == !n == derangements(^n).elems
!0 == 1 == 1
!1 == 0 == 0
!2 == 1 == 1
!3 == 2 == 2
!4 == 9 == 9
!5 == 44 == 44
!6 == 265 == 265
!7 == 1854 == 1854
!8 == 14833 == 14833
!9 == 133496 == 133496

```



## Phix

```Phix
function deranged(sequence s1, sequence s2)
    for i=1 to length(s1) do
        if s1[i]==s2[i] then return 0 end if
    end for
    return 1
end function

function derangements(integer n)
    sequence ts = tagset(n)
    sequence res = {}
    for i=1 to factorial(n) do
        sequence s = permute(i,ts)
        if deranged(s,ts) then
            res = append(res,s)
        end if
    end for
    return res
end function

function subfactorial(integer n)
    if n<2 then return 1-n end if
    return (n-1)*(subfactorial(n-1)+subfactorial(n-2))
end function

?derangements(4)
for n=0 to 9 do
    printf(1,"%d: counted:%d, calculated:%d\n",{n,length(derangements(n)),subfactorial(n)})
end for
string msg = iff(machine_bits()=32?" (incorrect on 32-bit!)":"") -- (fine on 64-bit)
printf(1,"!20=%d%s\n",{subfactorial(20),msg})

include mpfr.e
function mpz_sub_factorial(integer n)
-- probably not the most efficient way to do this!
    if n<2 then return sprintf("%d",{1-n}) end if
    mpz f = mpz_init(mpz_sub_factorial(n-1)),
        g = mpz_init(mpz_sub_factorial(n-2))
    mpz_add(f,f,g)
    mpz_mul_si(f,f,n-1)
    string res = mpz_get_str(f)
    {f,g} = mpz_free({f,g})
    return res
end function
printf(1,"!20=%s (mpfr)\n",{mpz_sub_factorial(20)})
```

```txt

0: counted:1, calculated:1
1: counted:0, calculated:0
2: counted:1, calculated:1
3: counted:2, calculated:2
4: counted:9, calculated:9
5: counted:44, calculated:44
6: counted:265, calculated:265
7: counted:1854, calculated:1854
8: counted:14833, calculated:14833
9: counted:133496, calculated:133496
!20=895014631192902186 (incorrect on 32-bit!)
!20=895014631192902121 (mpfr)

```


A more efficient method of calculating subfactorials (0 should be handled separately, or obviously prepend a 1 and extract with idx+1).

Should you instead of string results want an array of mpz for further calculations, use the mpz_init_set() call as shown:

```Phix
include mpfr.e

function subfactorial(integer n)
    sequence res = repeat(0,n)
    mpz num = mpz_init(1)

    for i=1 to n do
        mpz_mul_si(num,num,i)
        if mpz_odd(num) then
            mpz_sub_ui(num,num,1)
        else
            mpz_add_ui(num,num,1)
        end if
        res[i] = mpz_get_str(num)
--      res[i] = mpz_init_set(num)
    end for
    return res
end function

?extract(subfactorial(20),tagset(9)&20)
```

```txt
{"0","1","2","9","44","265","1854","14833","133496","895014631192902121"}
```



## PicoLisp


```PicoLisp
(load "@lib/simul.l")  # For 'permute'

(de derangements (Lst)
   (filter
      '((L) (not (find = L Lst)))
      (permute Lst) ) )

(de subfact (N)
   (if (>= 2 N)
      (if (= 1 N) 0 1)
      (*
         (dec N)
         (+ (subfact (dec N)) (subfact (- N 2))) ) ) )
```

```txt
: (derangements (range 1 4))
-> ((2 1 4 3) (2 3 4 1) (2 4 1 3) (3 1 4 2) (3 4 1 2) (3 4 2 1) (4 1 2 3) (4 3 1 2) (4 3 2 1))

: (for I (range 0 9)
   (tab (2 8 8)
      I
      (length (derangements (range 1 I)))
      (subfact I) ) )
 0       1       1
 1       0       0
 2       1       1
 3       2       2
 4       9       9
 5      44      44
 6     265     265
 7    1854    1854
 8   14833   14833
 9  133496  133496
-> NIL

: (subfact 20)
-> 895014631192902121
```



## PureBasic

Brute Force

```PureBasic

Procedure.q Subfactoral(n)
If n=0:ProcedureReturn 1:EndIf
If n=1:ProcedureReturn 0:EndIf
ProcedureReturn (Subfactoral(n-1)+Subfactoral(n-2))*(n-1)
EndProcedure

factFile.s="factorials.txt"
tempFile.s="temp.txt"
drngFile.s="derangements.txt"
DeleteFile(factFile.s)
DeleteFile(tempFile.s)
DeleteFile(drngFile.s)

n=4

; create our storage file
f.s=factFile.s
If CreateFile(0,f.s)
WriteStringN(0,"1.2")
WriteStringN(0,"2.1")
CloseFile(0)
Else
Debug "not createfile :"+f.s
EndIf

showfactorial=#False

If showfactorial
; cw("nfactorial n ="+str(n))
Debug "nfactorial n ="+Str(n)
EndIf

; build up the factorial combinations
For l=1 To n-2
Gosub nfactorial
Next

; extract the derangements
; cw("derangements["+str(perm(n))+"] for n="+str(n))
Debug "derangements["+Str(Subfactoral(n))+"] for n="+Str(n)
Gosub derangements
; cw("")
Debug ""

; show the first 20 derangements
For i=0 To 20
Debug "derangements["+Str(Subfactoral(i))+"] for n="+Str(i)
Next

End

derangements:
x=0
If ReadFile(0,factFile.s) And CreateFile(1,drngFile.s)
Repeat
r.s = ReadString(0)
cs=CountString(r.s,".")
If cs
hit=0
t.s=""
; scan for numbers at their index
For i=1 To cs+1
s.s=StringField(r.s,i,".")
t.s+s.s+"."
If Val(s.s)=i:hit+1:EndIf
Next
t.s=RTrim(t.s,".")
; show only those which are valid
If Not hit
x+1
; cw(t.s+" "+str(x))
Debug t.s+" "+Str(x)
WriteStringN(1,t.s+" "+Str(x))
EndIf
EndIf
Until Eof(0)
CloseFile(0)
CloseFile(1)
Else
Debug "not readfile :"+factFile.s
Debug "not createfile :"+drngFile.s
EndIf
; cw("")
Debug ""
Return

nfactorial:
x=0
If ReadFile(0,factFile.s) And CreateFile(1,tempFile.s)
Repeat
r.s = ReadString(0)
cs=CountString(r.s,".")
If cs
For j=1 To cs+2
t.s=""
For i=1 To cs+1
s.s=StringField(r.s,i,".")
If i=j
t.s+"."+Str(cs+2)+"."+s.s
Else
t.s+"."+s.s
EndIf
Next
If j=cs+2:t.s+"."+Str(cs+2):EndIf
t.s=Trim(t.s,".")
x+1
If cs+2=n And showfactorial
; cw(t.s+" "+str(x))
Debug t.s+" "+Str(x)
EndIf
WriteStringN(1,t.s)
Next
EndIf
Until Eof(0)
CloseFile(0)
CloseFile(1)
Else
Debug "not readfile :"+factFile.s
Debug "not createfile :"+tempFile.s
EndIf
CopyFile(tempFile.s,factFile.s)
DeleteFile(tempFile.s)
Return

```


```txt

derangements[9] for n=4
4.3.1.2 1
3.4.1.2 2
3.1.4.2 3
4.1.2.3 4
4.3.2.1 5
3.4.2.1 6
2.3.4.1 7
2.4.1.3 8
2.1.4.3 9

derangements[1] for n=0
derangements[0] for n=1
derangements[1] for n=2
derangements[2] for n=3
derangements[9] for n=4
derangements[44] for n=5
derangements[265] for n=6
derangements[1854] for n=7
derangements[14833] for n=8
derangements[133496] for n=9
derangements[1334961] for n=10
derangements[14684570] for n=11
derangements[176214841] for n=12
derangements[2290792932] for n=13
derangements[32071101049] for n=14
derangements[481066515734] for n=15
derangements[7697064251745] for n=16
derangements[130850092279664] for n=17
derangements[2355301661033953] for n=18
derangements[44750731559645106] for n=19
derangements[895014631192902121] for n=20

```


```PureBasic
Procedure.i deranged(depth, lenn, Array d(1), show)
  Protected count, tmp, i
  If depth = lenn
    If show
      For i = 0 To lenn - 1: Print(Chr(d(i) + 'a')): Next
      PrintN("")
    EndIf
    ProcedureReturn  1
  EndIf

  For i = lenn - 1 To depth Step -1
    If i = d(depth): Continue: EndIf

    tmp = d(i): d(i) = d(depth): d(depth) = tmp
    count + deranged(depth + 1, lenn, d(), show)
    tmp = d(i): d(i) = d(depth): d(depth) = tmp
  Next

  ProcedureReturn count
EndProcedure

Procedure.q sub_fact(n)
  If n = 0: ProcedureReturn 1: EndIf
  If n = 1: ProcedureReturn 0: EndIf
  ProcedureReturn (sub_fact(n - 1) + sub_fact(n - 2)) * (n - 1)
EndProcedure

Procedure.i gen_n(n, show)
  Protected r.i
  Dim a(1024)
  For i = 0 To n - 1: a(i) = i: Next
  ProcedureReturn  deranged(0, n, a(), show)
EndProcedure

If OpenConsole()
  PrintN("Deranged Four:")
  gen_n(4, 1)

  PrintN(#CRLF$ + "Compare list vs calc:")
  For i = 0 To 9
    PrintN(Str(i) + ":" + #TAB$ + Str(gen_n(i, 0)) + #TAB$ + Str(sub_fact(i)))
  Next

  PrintN(#CRLF$ + "further calc:")
  For i = 10 To 20
    PrintN(Str(i) + ": " + Str(sub_fact(i)))
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```


```txt
Deranged Four:
dabc
dcab
dcba
cdba
cdab
cadb
bdac
bcda
badc

Compare list vs calc:
0:      1       1
1:      0       0
2:      1       1
3:      2       2
4:      9       9
5:      44      44
6:      265     265
7:      1854    1854
8:      14833   14833
9:      133496  133496

further calc:
10: 1334961
11: 14684570
12: 176214841
13: 2290792932
14: 32071101049
15: 481066515734
16: 7697064251745
17: 130850092279664
18: 2355301661033953
19: 44750731559645106
20: 895014631192902121
```



## Python

Includes stretch goal.

```python
from itertools import permutations
import math


def derangements(n):
    'All deranged permutations of the integers 0..n-1 inclusive'
    return ( perm for perm in permutations(range(n))
             if all(indx != p for indx, p in enumerate(perm)) )

def subfact(n):
    if n == 2 or n == 0:
        return 1
    elif n == 1:
        return 0
    elif  1 <= n <=18:
        return round(math.factorial(n) / math.e)
    elif n.imag == 0 and n.real == int(n.real) and n > 0:
        return (n-1) * ( subfact(n - 1) + subfact(n - 2) )
    else:
        raise ValueError()

def _iterlen(iter):
    'length of an iterator without taking much memory'
    l = 0
    for x in iter:
        l += 1
    return l

if __name__ == '__main__':
    n = 4
    print("Derangements of %s" % (tuple(range(n)),))
    for d in derangements(n):
        print("  %s" % (d,))

    print("\nTable of n vs counted vs calculated derangements")
    for n in range(10):
        print("%2i %-5i %-5i" %
              (n, _iterlen(derangements(n)), subfact(n)))

    n = 20
    print("\n!%i = %i" % (n, subfact(n)))
```


```txt
Derangements of (0, 1, 2, 3)
  (1, 0, 3, 2)
  (1, 2, 3, 0)
  (1, 3, 0, 2)
  (2, 0, 3, 1)
  (2, 3, 0, 1)
  (2, 3, 1, 0)
  (3, 0, 1, 2)
  (3, 2, 0, 1)
  (3, 2, 1, 0)

Table of n vs counted vs calculated derangements
 0 1     1
 1 0     0
 2 1     1
 3 2     2
 4 9     9
 5 44    44
 6 265   265
 7 1854  1854
 8 14833 14833
 9 133496 133496

!20 = 895014631192902121
```



## Racket


```Racket

#lang racket

(define (all-misplaced? l)
  (for/and ([x (in-list l)] [n (in-naturals 1)]) (not (= x n))))

;; 1. Create a named function to generate derangements of the integers 0..n-1.
(define (derangements n)
  (define (all-misplaced? l1 l2)
    (or (null? l1)
        (and (not (eq? (car l1) (car l2)))
             (all-misplaced? (cdr l1) (cdr l2)))))
  (define l (range n))
  (for/list ([p (permutations l)] #:when (all-misplaced? p l))
    p))

;; 2. Generate and show all the derangements of 4 integers using the above
;;    routine.
(derangements 4)
;; -> '((1 0 3 2) (3 0 1 2) (1 3 0 2) (2 0 3 1) (2 3 0 1)
;;      (3 2 0 1) (1 2 3 0) (2 3 1 0) (3 2 1 0))

;; 3. Create a function that calculates the subfactorial of n, !n.
(define (sub-fact n)
  (if (< n 2) (- 1 n)
      (* (+ (sub-fact (- n 1)) (sub-fact (- n 2))) (sub1 n))))

;; 4. Print and show a table of the counted number of derangements of n vs. the
;;    calculated !n for n from 0..9 inclusive.
(for ([i 10])
  (printf "~a ~a ~a\n" i
          (~a #:width 7 #:align 'right (length (derangements i)))
          (sub-fact i)))
;; Output:
;; 0       1 1
;; 1       0 0
;; 2       1 1
;; 3       2 2
;; 4       9 9
;; 5      44 44
;; 6     265 265
;; 7    1854 1854
;; 8   14833 14833
;; 9  133496 133496

;; Extra: !20
(sub-fact 20)
;; -> 895014631192902121

```



## REXX


```rexx
/*REXX program generates all  permutations  of   N   derangements  and  subfactorial #  */
numeric digits 1000                              /*be able to handle large subfactorials*/
parse arg N .;     if N=='' | N==","  then N=4   /*Not specified?  Then use the default.*/
d= derangeSet(N)                                 /*go and build the  derangements  set. */
say d  'derangements for'    N    "items are:"
say
      do i=1  for  d                             /*display the derangements for N items.*/
      say right('derangement', 22)       right(i, length(d) )        '───►'         $.i
      end   /*i*/
say                                              /* [↓]  count and calculate subfact !L.*/
      do L=0  to 2;  d= derangeSet(L)
      say L 'items:  derangement count='right(d, 6)",  !"L'='right( !s(L), 6)
      end   /*L*/
say
say right('!20=' , 22)     !s( 20)
say right('!200=', 22)     !s(200)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
!s:         _=1;      do j=1  for arg(1);  if j//2  then _= j*_  -  1;     else _=j*_  + 1
                      end   /*j*/;                       return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
derangeSet: procedure expose $.;  parse arg x;   $.=;    #=0;   p=x-1
            if x==0  then return 1;  if x==1  then return 0
            @.1=2;  @.2=1                                    /*populate 1st derangement.*/
              do i=3  to x;  @.i=i;  end  /*i*/              /*    "    the rest of 'em.*/
            parse value  @.p  @.x   with   @.x  @.p;   call .buildD x    /*swap & build.*/
                                                                         /*build others.*/
              do while .nextD(x, 0);  call .buildD x;   end;                  return #
/*──────────────────────────────────────────────────────────────────────────────────────*/
.buildD:              do j=1  for arg(1);   if @.j==j  then return;  end
            #=#+1;    do j=1  for arg(1);   $.#= $.# @.j;            end;     return
/*──────────────────────────────────────────────────────────────────────────────────────*/
.nextD:     procedure expose @.;  parse arg n,i

              do k=n-1  by -1  for n-1;  kp=k+1;     if @.k<@.kp  then do; i=k; leave; end
              end   /*k*/

              do j=i+1  while j<n;       parse value  @.j  @.n   with   @.n  @.j;   n=n-1
              end   /*j*/
            if i==0  then return 0
              do m=i+1  while @.m<@.i;   end  /*m*/          /* [↓]  swap two values.   */
            parse  value   @.m @.i   with   @.i @.m;                          return 1
```

```txt

9 derangements for 4 items are:

           derangement 1 ───►  2 1 4 3
           derangement 2 ───►  2 3 4 1
           derangement 3 ───►  2 4 1 3
           derangement 4 ───►  3 1 4 2
           derangement 5 ───►  3 4 1 2
           derangement 6 ───►  3 4 2 1
           derangement 7 ───►  4 1 2 3
           derangement 8 ───►  4 3 1 2
           derangement 9 ───►  4 3 2 1

0 items:  derangement count=     1,  !0=     1
1 items:  derangement count=     0,  !1=     0
2 items:  derangement count=     1,  !2=     1

                  !20= 895014631192902121
                 !200= 290131015521620609254546237518688936375622413566095185632876940298382875066633305125595907908697818551860745708196640009079772455670451355426573609799907339222509103785567575227183775791345718826220455840965346196540544976439608810006794385963854831693077054723298130736781093200499800934036993104223443563872463385599425635345341317933466521378117877578807421014599223577201

```



## Ruby


```ruby
def derangements(n)
  ary = (1 .. n).to_a
  ary.permutation.select do |perm|
    ary.zip(perm).all? {|a,b| a != b}
  end
end

def subfact(n)
  case n
  when 0 then 1
  when 1 then 0
  else (n-1)*(subfact(n-1) + subfact(n-2))
  end
end

puts "derangements for n = 4"
derangements(4).each{|d|p d}

puts "\n n   derange  subfact"
(0..9).each do |n|
  puts "%2d :%8d,%8d" % [n, derangements(n).size, subfact(n)]
end

puts "\nNumber of derangements"
(10..20).each do |n|
  puts "#{n} : #{subfact(n)}"
end
```


```txt
derangements for n = 4
[2, 1, 4, 3]
[2, 3, 4, 1]
[2, 4, 1, 3]
[3, 1, 4, 2]
[3, 4, 1, 2]
[3, 4, 2, 1]
[4, 1, 2, 3]
[4, 3, 1, 2]
[4, 3, 2, 1]

 n   derange  subfact
 0 :       1,       1
 1 :       0,       0
 2 :       1,       1
 3 :       2,       2
 4 :       9,       9
 5 :      44,      44
 6 :     265,     265
 7 :    1854,    1854
 8 :   14833,   14833
 9 :  133496,  133496

Number of derangements
10 : 1334961
11 : 14684570
12 : 176214841
13 : 2290792932
14 : 32071101049
15 : 481066515734
16 : 7697064251745
17 : 130850092279664
18 : 2355301661033953
19 : 44750731559645106
20 : 895014631192902121

```



## Scala

```Scala
def derangements(n: Int) =
  (1 to n).permutations.filter(_.zipWithIndex.forall{case (a, b) => a - b != 1})

def subfactorial(n: Long): Long = n match {
  case 0 => 1
  case 1 => 0
  case _ => (n - 1) * (subfactorial(n - 1) + subfactorial(n - 2))
}

println(s"Derangements for n = 4")
println(derangements(4) mkString "\n")

println("\n%2s%10s%10s".format("n", "derange", "subfact"))
(0 to 9).foreach(n => println("%2d%10d%10d".format(n, derangements(n).size, subfactorial(n))))
(10 to 20).foreach(n => println(f"$n%2d${subfactorial(n)}%20d"))
```

```txt
Derangements for n = 4
Vector(2, 1, 4, 3)
Vector(2, 3, 4, 1)
Vector(2, 4, 1, 3)
Vector(3, 1, 4, 2)
Vector(3, 4, 1, 2)
Vector(3, 4, 2, 1)
Vector(4, 1, 2, 3)
Vector(4, 3, 1, 2)
Vector(4, 3, 2, 1)

 n   derange   subfact
 0         1         1
 1         0         0
 2         1         1
 3         2         2
 4         9         9
 5        44        44
 6       265       265
 7      1854      1854
 8     14833     14833
 9    133496    133496
10             1334961
11            14684570
12           176214841
13          2290792932
14         32071101049
15        481066515734
16       7697064251745
17     130850092279664
18    2355301661033953
19   44750731559645106
20  895014631192902121
```



## SuperCollider


```SuperCollider
(
d = { |array, n|
	Routine {
		n = n ?? { array.size.factorial };
		n.do { |i|
			var permuted = array.permute(i);
			if(array.every { |each, i| permuted[i] != each }) {
				permuted.yield
			};
		}
	};
};
f = { |n| d.((0..n-1)) };
x = f.(4);
x.all.do(_.postln); "";
)
```


Answers:

```SuperCollider

[ 3, 2, 1, 0 ]
[ 2, 3, 0, 1 ]
[ 1, 0, 3, 2 ]
[ 1, 2, 3, 0 ]
[ 2, 0, 3, 1 ]
[ 3, 2, 0, 1 ]
[ 1, 3, 0, 2 ]
[ 2, 3, 1, 0 ]
[ 3, 0, 1, 2 ]

```



```SuperCollider
(
z = { |n|
	case
	{ n <= 0 } { 1 }
	{ n == 1 } { 0 }
	{ (n - 1) * (z.(n - 1) + z.(n - 2)) }
};
p = { |i| i.asPaddedString(10, " ") };
"n    derangements    subfactorial".postln;
(0..9).do { |i|
	var derangements = f.(i).all;
	var subfactorial = z.(i);
	"%    %    %\n".postf(i, p.(derangements.size), p.(subfactorial));
};
)
```


Answers:


```SuperCollider

n    derangements    subfactorial
0             1             1
1             0             0
2             1             1
3             2             2
4             9             9
5            44            44
6           265           265
7          1854          1854
8         14833         14833
9        133496        133496

```



## Tcl

```tcl
package require Tcl 8.5;		# for arbitrary-precision integers
package require struct::list;		# for permutation enumerator

proc derangements lst {
    # Special case
    if {![llength $lst]} {return {{}}}
    set result {}
    for {set perm [struct::list firstperm $lst]} {[llength $perm]} \
	    {set perm [struct::list nextperm $perm]} {
	set skip 0
	foreach a $lst b $perm {
	    if {[set skip [string equal $a $b]]} break
	}
	if {!$skip} {lappend result $perm}
    }
    return $result
}

proc deranged1to n {
    for {set i 1;set r {}} {$i <= $n} {incr i} {lappend r $i}
    return [derangements $r]
}

proc countDeranged1to n {
    llength [deranged1to $n]
}

proc subfact n {
    if {$n == 0} {return 1}
    if {$n == 1} {return 0}
    set o 1
    set s 0
    for {set i 1} {$i < $n} {incr i} {
	set s [expr {$i * ($o + [set o $s])}]
    }
    return $s
}
```

Demonstrating with the display parts of the task:

```tcl
foreach d [deranged1to 4] {
    puts "derangement of 1..4: $d"
}

puts "\n\tcounted\tcalculated"
for {set i 0} {$i <= 9} {incr i} {
    puts "!$i\t[countDeranged1to $i]\t[subfact $i]"
}

# Stretch goal
puts "\n!20 = [subfact 20]"
```

```txt

derangement of 1..4: 2 1 4 3
derangement of 1..4: 2 3 4 1
derangement of 1..4: 2 4 1 3
derangement of 1..4: 3 1 4 2
derangement of 1..4: 3 4 1 2
derangement of 1..4: 3 4 2 1
derangement of 1..4: 4 1 2 3
derangement of 1..4: 4 3 1 2
derangement of 1..4: 4 3 2 1

	counted	calculated
!0	1	1
!1	0	0
!2	1	1
!3	2	2
!4	9	9
!5	44	44
!6	265	265
!7	1854	1854
!8	14833	14833
!9	133496	133496

!20 = 895014631192902121

```



## zkl

{{trans|Python}} mostly

```zkl
fcn subFact(n){
   if(n==0) return(1);
   if(n==1) return(0);
   (n-1)*(self.fcn(n-1) + self.fcn(n-2));
}

fcn derangements(n){
   // All deranged permutations of the integers 0..n-1 inclusive
   enum:=[0..n-1].pump(List);
   Utils.Helpers.permuteW(enum).filter('wrap(perm){
      perm.zipWith('==,enum).sum(0) == 0
   });
}
fcn derangers(n){  // just count # of derangements
   enum:=[0..n-1].pump(List);
   Utils.Helpers.permuteW(enum).reduce('wrap(sum,perm){
      sum + (perm.zipWith('==,enum).sum(0) == 0)
   },0);
}
```


```zkl
println("Derangements of 0,1,2,3:\n",derangements(4));
println("\nTable of n vs counted vs calculated derangements:");
foreach n in (10){
   println("%2d %-6d %-6d".fmt(n, derangers(n), subFact(n)));
}

n:=20; println("\n!%d = %d".fmt(n, subFact(n)));
```

```txt

Derangements of 0,1,2,3:
L(L(3,0,1,2),L(2,0,3,1),L(2,3,0,1),L(3,2,0,1),L(3,2,1,0),
L(2,3,1,0),L(1,2,3,0),L(1,3,0,2),L(1,0,3,2))

Table of n vs counted vs calculated derangements:
 0 1      1
 1 0      0
 2 1      1
 3 2      2
 4 9      9
 5 44     44
 6 265    265
 7 1854   1854
 8 14833  14833
 9 133496 133496

!20 = 895014631192902121

```

Lazy/iterators version:

```zkl
fcn derangements(n){ //-->Walker
   enum:=[0..n-1].pump(List);
   Utils.Helpers.permuteW(enum).tweak('wrap(perm){
      if(perm.zipWith('==,enum).sum(0)) Void.Skip
      else perm
   });
}
fcn derangers(n){  // just count # of derangements, w/o saving them
   derangements(n).reduce('+.fpM("10-",1),0);  // ignore perm --> '+(1,sum)...
}
```


```zkl
foreach d in (derangements(4)){ println(d) }
//rest of test code remains the same
```

