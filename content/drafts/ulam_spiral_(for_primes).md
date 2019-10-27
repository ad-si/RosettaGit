+++
title = "Ulam spiral (for primes)"
description = ""
date = 2019-10-14T01:03:19Z
aliases = []
[extra]
id = 18507
[taxonomies]
categories = []
tags = []
+++

{{task|Prime Numbers}}

An Ulam spiral (of primes) is a method of visualizing primes when expressed in a (normally counter-clockwise) outward spiral (usually starting at <big>'''1'''</big>),   constructed on a square grid, starting at the "center". 

An Ulam spiral is also known as a   ''prime spiral''. 

The first grid (green) is shown with sequential integers,   starting at   <big>'''1'''</big>. 

In an Ulam spiral of primes, only the primes are shown (usually indicated by some glyph such as a dot or asterisk),   and all non-primes as shown as a blank   (or some other whitespace). 

Of course, the grid and border are not to be displayed (but they are displayed here when using these Wiki HTML tables).

Normally, the spiral starts in the "center",   and the   2<sup>nd</sup>   number is to the viewer's right and the number spiral starts from there in a counter-clockwise direction. 

There are other geometric shapes that are used as well, including clock-wise spirals. 

Also, some spirals (for the   2<sup>nd</sup>   number)   is viewed upwards from the   1<sup>st</sup>   number instead of to the right, but that is just a matter of orientation.

Sometimes, the starting number can be specified to show more visual striking patterns (of prime densities).

[A larger than necessary grid (numbers wise) is shown here to illustrate the pattern of numbers on the diagonals   (which may be used by the method to orientate the direction of spiral-construction algorithm within the example computer programs)]. 

Then, in the next phase in the transformation of the Ulam prime spiral,   the non-primes are translated to blanks. 

In the orange grid below,   the primes are left intact,   and all non-primes are changed to blanks.  

Then, in the final transformation of the Ulam spiral (the yellow grid),   translate the primes to a glyph such as a   <big><big><b> ∙ </b></big></big>   or some other suitable glyph.



{| class="wikitable" style="float:left;border: 2px solid black; background:lightgreen; color:black; margin-left:0;margin-right:auto;text-align:center;width:34em;height:34em;table-layout:fixed;font-size:70%"
|-
| <big><big>'''65'''</big></big> || <big><big>'''64'''</big></big> || <big><big>'''63'''</big></big> || <big><big>'''62'''</big></big> || <big><big>'''61'''</big></big> || <big><big>'''60'''</big></big> || <big><big>'''59'''</big></big> || <big><big>'''58'''</big></big> || <big><big>'''57'''</big></big>
|->
| <big><big>'''66'''</big></big> || <big><big>'''37'''</big></big> || <big><big>'''36'''</big></big> || <big><big>'''35'''</big></big> || <big><big>'''34'''</big></big> || <big><big>'''33'''</big></big> || <big><big>'''32'''</big></big> || <big><big>'''31'''</big></big> || <big><big>'''56'''</big></big>
|-
| <big><big>'''67'''</big></big> || <big><big>'''38'''</big></big> || <big><big>'''17'''</big></big> || <big><big>'''16'''</big></big> || <big><big>'''15'''</big></big> || <big><big>'''14'''</big></big> || <big><big>'''13'''</big></big> || <big><big>'''30'''</big></big> || <big><big>'''55'''</big></big>
|-
| <big><big>'''68'''</big></big> || <big><big>'''39'''</big></big> || <big><big>'''18'''</big></big> || <big><big>''' 5'''</big></big> || <big><big>''' 4'''</big></big> || <big><big>''' 3'''</big></big> || <big><big>'''12'''</big></big> || <big><big>'''29'''</big></big> || <big><big>'''54'''</big></big>
|-
| <big><big>'''69'''</big></big> || <big><big>'''40'''</big></big> || <big><big>'''19'''</big></big> || <big><big>''' 6'''</big></big> || <big><big>''' 1'''</big></big> || <big><big>''' 2'''</big></big> || <big><big>'''11'''</big></big> || <big><big>'''28'''</big></big> || <big><big>'''53'''</big></big>
|-
| <big><big>'''70'''</big></big> || <big><big>'''41'''</big></big> || <big><big>'''20'''</big></big> || <big><big>''' 7'''</big></big> || <big><big>''' 8'''</big></big> || <big><big>''' 9'''</big></big> || <big><big>'''10'''</big></big> || <big><big>'''27'''</big></big> || <big><big>'''52'''</big></big>
|-
| <big><big>'''71'''</big></big> || <big><big>'''42'''</big></big> || <big><big>'''21'''</big></big> || <big><big>'''22'''</big></big> || <big><big>'''23'''</big></big> || <big><big>'''24'''</big></big> || <big><big>'''25'''</big></big> || <big><big>'''26'''</big></big> || <big><big>'''51'''</big></big>
|-
| <big><big>'''72'''</big></big> || <big><big>'''43'''</big></big> || <big><big>'''44'''</big></big> || <big><big>'''45'''</big></big> || <big><big>'''46'''</big></big> || <big><big>'''47'''</big></big> || <big><big>'''48'''</big></big> || <big><big>'''49'''</big></big> || <big><big>'''50'''</big></big>
|-
| <big><big>'''73'''</big></big> || <big><big>'''74'''</big></big> || <big><big>'''75'''</big></big> || <big><big>'''76'''</big></big> || <big><big>'''77'''</big></big> || <big><big>'''78'''</big></big> || <big><big>'''79'''</big></big> || <big><big>'''80'''</big></big> || <big><big>'''81'''</big></big>
|}

{| class="wikitable" style="float:left;border: 2px solid black; background:orange; color:black; margin-left:20px;margin-right:auto;text-align:center;width:34em;height:34em;table-layout:fixed;font-size:70%"
|-
| <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''61'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''59'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big>
|-
| <big><big>'''  '''</big></big> || <big><big>'''37'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''31'''</big></big> || <big><big>'''  '''</big></big>
|-
| <big><big>'''67'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''17'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''13'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big>
|-
| <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' 5'''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' 3'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''29'''</big></big> || <big><big>'''  '''</big></big>
|-
| <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''19'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' 2'''</big></big> || <big><big>'''11'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''53'''</big></big>
|-
| <big><big>'''  '''</big></big> || <big><big>'''41'''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' 7'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big>
|-
| <big><big>'''71'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''23'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big>
|-
| <big><big>'''  '''</big></big> || <big><big>'''43'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''47'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big>
|-
| <big><big>'''73'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''79'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big>
|}

{| class="wikitable" style="float:left;border: 2px solid black; background:yellow; color:black; margin-left:20px;margin-right:auto;text-align:center;width:34em;height:34em;table-layout:fixed;font-size:70%"
|-
| <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big>
|-
| <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big>
|-
| <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big>
|-
| <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big>
|-
| <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big>
|-
| <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big>
|-
| <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big>
|-
| <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big>
|-
| <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big> || <big><big>''' ∙'''</big></big> || <big><big>'''  '''</big></big> || <big><big>'''  '''</big></big>
|}


<br style="clear:both">
The Ulam spiral becomes more visually obvious as the grid increases in size. 


;Task
For any sized   <big>'''N x N'''</big>   grid,   construct and show an Ulam spiral (counter-clockwise) of primes starting at some specified initial number   (the default would be '''1'''),   with some suitably   ''dotty''   (glyph) representation to indicate primes,   and the absence of dots to indicate non-primes.   

You should demonstrate the generator by showing at Ulam prime spiral large enough to (almost) fill your terminal screen.


;Related tasks:
*   [[Spiral matrix]]
*   [[Zig-zag matrix]]
*   [[Identity matrix]] 
*   [[Sequence of primes by Trial Division]]


;See also
* Wikipedia entry:          [http://en.wikipedia.org/wiki/Ulam_spiral Ulam spiral]  
* MathWorld&trade; entry:   [http://mathworld.wolfram.com/PrimeSpiral.html Prime Spiral]





## 360 Assembly

{{trans|Fortran}}
Compacted and optimized solution.

```360asm
*        Ulam spiral               26/04/2016
ULAM     CSECT
         USING  ULAM,R13           set base register
SAVEAREA B      STM-SAVEAREA(R15)  skip savearea
         DC     17F'0'             savearea
STM      STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         save previous SA
         ST     R15,8(R13)         linkage in previous SA
         LR     R13,R15            establish addressability
         LA     R5,1               n=1
         LH     R8,NSIZE           x=nsize
         SRA    R8,1
         LA     R8,1(R8)           x=nsize/2+1
         LR     R9,R8              y=x
         LR     R1,R5              n
         BAL    R14,ISPRIME
         C      R0,=F'1'           if isprime(n)
         BNE    NPRMJ0
         BAL    R14,SPIRALO        spiral(x,y)=o
NPRMJ0   LA     R5,1(R5)           n=n+1
         LA     R6,1               i=1
LOOPI1   LH     R2,NSIZE           do i=1 to nsize-1 by 2
         BCTR   R2,0
         CR     R6,R2              if i>nsize-1
         BH     ELOOPI1
         LR     R7,R6              j=i; do j=1 to i
LOOPJ1   LA     R8,1(R8)           x=x+1
         LR     R1,R5              n
         BAL    R14,ISPRIME
         C      R0,=F'1'           if isprime(n)
         BNE    NPRMJ1
         BAL    R14,SPIRALO        spiral(x,y)=o
NPRMJ1   LA     R5,1(R5)           n=n+1
         BCT    R7,LOOPJ1          next j 
ELOOPJ1  LR     R7,R6              j=i; do j=1 to i
LOOPJ2   BCTR   R9,0               y=y-1
         LR     R1,R5              n
         BAL    R14,ISPRIME
         C      R0,=F'1'           if isprime(n)
         BNE    NPRMJ2
         BAL    R14,SPIRALO        spiral(x,y)=o
NPRMJ2   LA     R5,1(R5)           n=n+1
         BCT    R7,LOOPJ2          next j 
ELOOPJ2  LR     R7,R6              j=i
         LA     R7,1(R7)           j=i+1; do j=1 to i+1
LOOPJ3   BCTR   R8,0               x=x-1
         LR     R1,R5              n
         BAL    R14,ISPRIME
         C      R0,=F'1'           if isprime(n)
         BNE    NPRMJ3
         BAL    R14,SPIRALO        spiral(x,y)=o
NPRMJ3   LA     R5,1(R5)           n=n+1
         BCT    R7,LOOPJ3          next j 
ELOOPJ3  LR     R7,R6              j=i
         LA     R7,1(R7)           j=i+1; do j=1 to i+1
LOOPJ4   LA     R9,1(R9)           y=y+1
         LR     R1,R5              n
         BAL    R14,ISPRIME
         C      R0,=F'1'           if isprime(n)
         BNE    NPRMJ4
         BAL    R14,SPIRALO        spiral(x,y)=o
NPRMJ4   LA     R5,1(R5)           n=n+1
         BCT    R7,LOOPJ4          next j 
ELOOPJ4  LA     R6,2(R6)           i=i+2
         B      LOOPI1
ELOOPI1  LH     R7,NSIZE           j=nsize
         BCTR   R7,0               j=nsize-1; do j=1 to nsize-1
LOOPJ5   LA     R8,1(R8)           x=x+1
         LR     R1,R5              n
         BAL    R14,ISPRIME
         C      R0,=F'1'           if isprime(n)
         BNE    NPRMJ5
         BAL    R14,SPIRALO        spiral(x,y)=o
NPRMJ5   LA     R5,1(R5)           n=n+1
         BCT    R7,LOOPJ5          next j 
ELOOPJ5  LA     R6,1               i=1
LOOPI2   CH     R6,NSIZE           do i=1 to nsize
         BH     ELOOPI2
         LA     R10,PG             reset buffer
         LA     R7,1               j=1
LOOPJ6   CH     R7,NSIZE           do j=1 to nsize
         BH     ELOOPJ6
         LR     R1,R7              j
         BCTR   R1,0               (j-1)
         MH     R1,NSIZE           (j-1)*nsize
         AR     R1,R6              r1=(j-1)*nsize+i
         LA     R14,SPIRAL-1(R1)   @spiral(j,i)
         MVC    0(1,R10),0(R14)    output spiral(j,i)
         LA     R10,1(R10)         pgi=pgi+1
         LA     R7,1(R7)           j=j+1
         B      LOOPJ6
ELOOPJ6  XPRNT  PG,80              print
         LA     R6,1(R6)           i=i+1
         B      LOOPI2
ELOOPI2  L      R13,4(0,R13)       reset previous SA
         LM     R14,R12,12(R13)    restore previous env
         XR     R15,R15            set return code
         BR     R14                call back
ISPRIME  CNOP   0,4     ---------- isprime function
         C      R1,=F'2'           if nn=2
         BNE    NOT2
         LA     R0,1               rr=1
         B      ELOOPII
NOT2     C      R1,=F'2'           if nn<2
         BL     RRZERO
         LR     R2,R1              nn
         LA     R4,2               2
         SRDA   R2,32              shift
         DR     R2,R4              nn/2
         C      R2,=F'0'           if nn//2=0
         BNE    TAGII
RRZERO   SR     R0,R0              rr=0
         B      ELOOPII
TAGII    LA     R0,1               rr=1
         LA     R4,3               ii=3 
LOOPII   LR     R3,R4              ii
         MR     R2,R4              ii*ii
         CR     R3,R1              if ii*ii<=nn
         BH     ELOOPII
         LR     R3,R1              nn
         LA     R2,0               clear
         DR     R2,R4              nn/ii
         LTR    R2,R2              if nn//ii=0
         BNZ    NEXTII
         SR     R0,R0              rr=0
         B      ELOOPII
NEXTII   LA     R4,2(R4)           ii=ii+2
         B      LOOPII
ELOOPII  BR     R14     ---------- end isprime return rr
SPIRALO  CNOP   0,4     ---------- spiralo subroutine
         LR     R1,R8              x
         BCTR   R1,0               x-1
         MH     R1,NSIZE           (x-1)*nsize
         AR     R1,R9              r1=(x-1)*nsize+y
         LA     R10,SPIRAL-1(R1)   r10=@spiral(x,y)
         MVC    0(1,R10),O         spiral(x,y)=o
         BR     R14     ---------- end spiralo
NS       EQU    79                 4n+1
NSIZE    DC     AL2(NS)            =H'ns'
O        DC     CL1'*'             if prime
PG       DC     CL80' '            buffer
         LTORG
SPIRAL   DC     (NS*NS)CL1' '
         YREGS
         END    ULAM
```

{{out}}

```txt

        *   * *   *
 *     *         *
  *   * *
         * *     *
  * *   *       *
         * *   *
*   * *     * * *
 * * * *   *
        * * *
   *   *  ** * * *
    * * *
     *   *
*   * *   *   * *
 *   *     *     *
      *           *
 * *     *   *   *
  *           *
     *   * *
    * *   *     *

```



## Ada

This is a generic solution. It is straightforward to use it to print spirals for any kind of numbers, rather than spirals of primes, only.

The specification of package generic_ulam is as follows:


```Ada
generic
   Size: Positive; 
      -- determines the size of the square
   with function Represent(N: Natural) return String;
      -- this turns a number into a string to be printed
      -- the length of the output should not change
      -- e.g., Represent(N) may return " #" if N is a prime
      -- and "  " else
   with procedure Put_String(S: String);
      -- outputs a string, no new line
   with procedure New_Line;
      -- the name says all
package Generic_Ulam is
   
   procedure Print_Spiral;
   -- calls Put_String(Represent(I)) N^2 times
   --       and New_Line N times
   
end Generic_Ulam;
```


Here is the implementation:

```Ada
package body Generic_Ulam is
   
   subtype Index is Natural range 0 .. Size-1;
   subtype Number is Positive range 1 .. Size**2;

   function Cell(Row, Column: Index) return Number is
      -- outputs the number at the given position in the square
      -- taken from the Python solution
      X: Integer := Column - (Size-1)/2;
      Y: Integer := Row - Size/2;
      MX: Natural := abs(X);
      MY: Natural := abs(Y);
      L: Natural := 2 * Natural'Max(MX, MY);
      D: Integer;
   begin
      if Y >= X then 
         D := 3 * L + X + Y;
      else
         D := L - X - Y;
      end if;
      return (L-1) ** 2 + D;
   end Cell;
   
   procedure Print_Spiral is
      N: Number;
   begin
      for R in Index'Range loop
         for C in Index'Range loop
            N := Cell(R, C);
            Put_String(Represent(N));
         end loop;
         New_Line;
      end loop;
   end Print_Spiral;
   
end Generic_Ulam;
```


The folowing implementation prints a 29*29 spiral with the primes represented as numbers, and a 10*10 spiral with the primes as boxes. It uses the generic function Prime_Numbers.Is_Prime, as specified in [[Prime decomposition#Ada]]. 


```Ada
with Generic_Ulam, Ada.Text_IO, Prime_Numbers;

procedure Ulam is
   
   package P is new Prime_Numbers(Natural, 0, 1, 2);
   
   function Vis(N: Natural) return String is
      (if P.Is_Prime(N) then " <>" else "   ");
      
   function Num(N: Natural) return String is
      (if P.Is_Prime(N) then 
	(if N < 10 then "  " elsif N < 100 then " " else "") & Natural'Image(N)
      else " ---");
      
   procedure NL is
   begin
      Ada.Text_IO.New_Line;
   end NL;
      
   package Numeric is new Generic_Ulam(29, Num,  Ada.Text_IO.Put, NL);
   package Visual  is new Generic_Ulam(10, Vis,  Ada.Text_IO.Put, NL);
   
begin
   Numeric.Print_Spiral;
   NL;
   Visual.Print_Spiral;
end Ulam;
```


{{out}}


```txt
 --- --- --- --- --- --- --- --- --- --- --- --- 773 --- --- --- 769 --- --- --- --- --- --- --- 761 --- --- --- 757
 --- 677 --- --- --- 673 --- --- --- --- --- --- --- --- --- --- --- 661 --- 659 --- --- --- --- --- 653 --- --- ---
 787 --- 577 --- --- --- --- --- 571 --- 569 --- --- --- --- --- 563 --- --- --- --- --- 557 --- --- --- --- --- ---
 --- --- --- --- --- --- --- --- --- 479 --- --- --- --- --- --- --- --- --- --- --- 467 --- --- --- 463 --- --- ---
 --- --- --- --- 401 --- --- --- 397 --- --- --- --- --- --- --- 389 --- --- --- --- --- 383 --- --- --- --- --- ---
 --- --- --- 487 --- --- --- --- --- --- --- --- --- 317 --- --- --- 313 --- 311 --- --- --- 307 --- 461 --- 647 ---
 --- --- --- --- --- --- 257 --- --- --- --- --- 251 --- --- --- --- --- --- --- --- --- 241 --- 379 --- --- --- 751
 --- 683 --- --- --- --- --- 197 --- --- --- 193 --- 191 --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 --- --- --- --- --- --- --- --- --- --- --- --- --- --- 139 --- 137 --- --- --- --- --- 239 --- --- --- 547 --- ---
 --- --- --- 491 --- --- --- 199 --- 101 --- --- ---  97 --- --- --- --- --- --- --- 181 --- --- --- 457 --- 643 ---
 --- --- --- --- --- --- --- --- --- --- --- --- --- ---  61 ---  59 --- --- --- 131 --- --- --- --- --- --- --- ---
 --- --- --- --- --- 331 --- --- --- 103 ---  37 --- --- --- --- ---  31 ---  89 --- 179 --- --- --- --- --- 641 ---
 797 --- 587 --- 409 --- 263 --- 149 ---  67 ---  17 --- --- ---  13 --- --- --- --- --- --- --- 373 --- --- --- ---
 --- --- --- --- --- --- --- --- --- --- --- --- ---   5 ---   3 ---  29 --- --- --- --- --- --- --- --- --- --- ---
 --- --- --- --- --- --- --- --- 151 --- --- ---  19 --- ---   2  11 ---  53 --- 127 --- 233 --- --- --- 541 --- 743
 --- 691 --- --- --- --- --- --- --- 107 ---  41 ---   7 --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 --- --- --- --- --- --- --- --- --- ---  71 --- --- ---  23 --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 --- --- --- 499 --- 337 --- --- --- 109 ---  43 --- --- ---  47 --- --- ---  83 --- 173 --- --- --- 449 --- --- ---
 --- --- 593 --- --- --- 269 --- --- ---  73 --- --- --- --- ---  79 --- --- --- --- --- 229 --- 367 --- --- --- 739
 --- --- --- --- --- --- --- --- --- --- --- 113 --- --- --- --- --- --- --- --- --- --- --- 293 --- --- --- --- ---
 --- --- --- --- --- --- 271 --- 157 --- --- --- --- --- 163 --- --- --- 167 --- --- --- 227 --- --- --- --- --- ---
 --- --- --- 503 --- --- --- 211 --- --- --- --- --- --- --- --- --- --- --- 223 --- --- --- --- --- --- --- 631 ---
 --- --- --- --- 419 --- --- --- --- --- 277 --- --- --- 281 --- 283 --- --- --- --- --- --- --- --- --- --- --- ---
 --- --- --- --- --- --- --- --- --- 347 --- 349 --- --- --- 353 --- --- --- --- --- 359 --- --- --- 443 --- --- ---
 809 --- 599 --- 421 --- --- --- --- --- --- --- --- --- 431 --- 433 --- --- --- --- --- 439 --- --- --- --- --- 733
 --- 701 --- --- --- 509 --- --- --- --- --- --- --- --- --- --- --- 521 --- 523 --- --- --- --- --- --- --- --- ---
 811 --- 601 --- --- --- --- --- 607 --- --- --- --- --- 613 --- --- --- 617 --- 619 --- --- --- --- --- --- --- ---
 --- --- --- --- --- --- --- 709 --- --- --- --- --- --- --- --- --- 719 --- --- --- --- --- --- --- 727 --- --- ---
 --- --- --- --- --- --- --- --- 821 --- 823 --- --- --- 827 --- 829 --- --- --- --- --- --- --- --- --- 839 --- ---

          <>                  
             <>    <>         
    <>                <>    <>
 <>    <>          <>         
          <>    <>    <>      
       <>       <> <>    <>   
    <>    <>                  
 <>          <>               
    <>          <>          <>
 <>                <>         
```



## C


```c

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <math.h>

typedef uint32_t bitsieve;

unsigned sieve_check(bitsieve *b, const unsigned v)
{
    if ((v != 2 && !(v & 1)) || (v < 2))
        return 0;
    else
        return !(b[v >> 6] & (1 << (v >> 1 & 31)));
}

bitsieve* sieve(const unsigned v)
{
    unsigned i, j;
    bitsieve *b = calloc((v >> 6) + 1, sizeof(uint32_t));

    for (i = 3; i <= sqrt(v); i += 2)
        if (!(b[i >> 6] & (1 << (i >> 1 & 31))))
            for (j = i*i; j < v; j += (i << 1))
                b[j >> 6] |= (1 << (j >> 1 & 31));

    return b;
}

#define max(x,y) ((x) > (y) ? (x) : (y))

/* This mapping taken from python solution */
int ulam_get_map(int x, int y, int n)
{
    x -= (n - 1) / 2;
    y -= n / 2;

    int mx = abs(x), my = abs(y);
    int l = 2 * max(mx, my);
    int d = y >= x ? l * 3 + x + y : l - x - y;

    return pow(l - 1, 2) + d;
}

/* Passing a value of 0 as glyph will print numbers */
void output_ulam_spiral(int n, const char glyph)
{
    /* An even side length does not make sense, use greatest odd value < n */
    n -= n % 2 == 0 ? 1 : 0;

    const char *spaces = ".................";
    int mwidth = log10(n * n) + 1;

    bitsieve *b = sieve(n * n + 1);
    int x, y;

    for (x = 0; x < n; ++x) {
        for (y = 0; y < n; ++y) {
            int z = ulam_get_map(y, x, n);

            if (glyph == 0) {
                if (sieve_check(b, z))
                    printf("%*d ", mwidth, z);
                else
                    printf("%.*s ", mwidth, spaces);
            }
            else {
                printf("%c", sieve_check(b, z) ? glyph : spaces[0]);
            }
        }
        printf("\n");
    }

    free(b);
}

int main(int argc, char *argv[])
{
    const int n = argc < 2 ? 9 : atoi(argv[1]);

    output_ulam_spiral(n, 0);
    printf("\n");

    output_ulam_spiral(n, '#');
    printf("\n");

    return 0;
}

```

{{out}}
Run with a side-length of 29

```txt

... ... ... ... ... ... ... ... ... ... ... ... 773 ... ... ... 769 ... ... ... ... ... ... ... 761 ... ... ... 757 
... 677 ... ... ... 673 ... ... ... ... ... ... ... ... ... ... ... 661 ... 659 ... ... ... ... ... 653 ... ... ... 
787 ... 577 ... ... ... ... ... 571 ... 569 ... ... ... ... ... 563 ... ... ... ... ... 557 ... ... ... ... ... ... 
... ... ... ... ... ... ... ... ... 479 ... ... ... ... ... ... ... ... ... ... ... 467 ... ... ... 463 ... ... ... 
... ... ... ... 401 ... ... ... 397 ... ... ... ... ... ... ... 389 ... ... ... ... ... 383 ... ... ... ... ... ... 
... ... ... 487 ... ... ... ... ... ... ... ... ... 317 ... ... ... 313 ... 311 ... ... ... 307 ... 461 ... 647 ... 
... ... ... ... ... ... 257 ... ... ... ... ... 251 ... ... ... ... ... ... ... ... ... 241 ... 379 ... ... ... 751 
... 683 ... ... ... ... ... 197 ... ... ... 193 ... 191 ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... 
... ... ... ... ... ... ... ... ... ... ... ... ... ... 139 ... 137 ... ... ... ... ... 239 ... ... ... 547 ... ... 
... ... ... 491 ... ... ... 199 ... 101 ... ... ...  97 ... ... ... ... ... ... ... 181 ... ... ... 457 ... 643 ... 
... ... ... ... ... ... ... ... ... ... ... ... ... ...  61 ...  59 ... ... ... 131 ... ... ... ... ... ... ... ... 
... ... ... ... ... 331 ... ... ... 103 ...  37 ... ... ... ... ...  31 ...  89 ... 179 ... ... ... ... ... 641 ... 
797 ... 587 ... 409 ... 263 ... 149 ...  67 ...  17 ... ... ...  13 ... ... ... ... ... ... ... 373 ... ... ... ... 
... ... ... ... ... ... ... ... ... ... ... ... ...   5 ...   3 ...  29 ... ... ... ... ... ... ... ... ... ... ... 
... ... ... ... ... ... ... ... 151 ... ... ...  19 ... ...   2  11 ...  53 ... 127 ... 233 ... ... ... 541 ... 743 
... 691 ... ... ... ... ... ... ... 107 ...  41 ...   7 ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... 
... ... ... ... ... ... ... ... ... ...  71 ... ... ...  23 ... ... ... ... ... ... ... ... ... ... ... ... ... ... 
... ... ... 499 ... 337 ... ... ... 109 ...  43 ... ... ...  47 ... ... ...  83 ... 173 ... ... ... 449 ... ... ... 
... ... 593 ... ... ... 269 ... ... ...  73 ... ... ... ... ...  79 ... ... ... ... ... 229 ... 367 ... ... ... 739 
... ... ... ... ... ... ... ... ... ... ... 113 ... ... ... ... ... ... ... ... ... ... ... 293 ... ... ... ... ... 
... ... ... ... ... ... 271 ... 157 ... ... ... ... ... 163 ... ... ... 167 ... ... ... 227 ... ... ... ... ... ... 
... ... ... 503 ... ... ... 211 ... ... ... ... ... ... ... ... ... ... ... 223 ... ... ... ... ... ... ... 631 ... 
... ... ... ... 419 ... ... ... ... ... 277 ... ... ... 281 ... 283 ... ... ... ... ... ... ... ... ... ... ... ... 
... ... ... ... ... ... ... ... ... 347 ... 349 ... ... ... 353 ... ... ... ... ... 359 ... ... ... 443 ... ... ... 
809 ... 599 ... 421 ... ... ... ... ... ... ... ... ... 431 ... 433 ... ... ... ... ... 439 ... ... ... ... ... 733 
... 701 ... ... ... 509 ... ... ... ... ... ... ... ... ... ... ... 521 ... 523 ... ... ... ... ... ... ... ... ... 
811 ... 601 ... ... ... ... ... 607 ... ... ... ... ... 613 ... ... ... 617 ... 619 ... ... ... ... ... ... ... ... 
... ... ... ... ... ... ... 709 ... ... ... ... ... ... ... ... ... 719 ... ... ... ... ... ... ... 727 ... ... ... 
... ... ... ... ... ... ... ... 821 ... 823 ... ... ... 827 ... 829 ... ... ... ... ... ... ... ... ... 839 ... ... 

............#...#.......#...#
.#...#...........#.#.....#...
#.#.....#.#.....#.....#......
.........#...........#...#...
....#...#.......#.....#......
...#.........#...#.#...#.#.#.
......#.....#.........#.#...#
.#.....#...#.#...............
..............#.#.....#...#..
...#...#.#...#.......#...#.#.
..............#.#...#........
.....#...#.#.....#.#.#.....#.
#.#.#.#.#.#.#...#.......#....
.............#.#.#...........
........#...#..##.#.#.#...#.#
.#.......#.#.#...............
..........#...#..............
...#.#...#.#...#...#.#...#...
..#...#...#.....#.....#.#...#
...........#...........#.....
......#.#.....#...#...#......
...#...#...........#.......#.
....#.....#...#.#............
.........#.#...#.....#...#...
#.#.#.........#.#.....#.....#
.#...#...........#.#.........
#.#.....#.....#...#.#........
.......#.........#.......#...
........#.#...#.#.........#..

```


The following shows a spiral that's not necessarily square, which has questionable merit:

```c>#include <stdio.h

#include <stdlib.h>

int isprime(int n)
{
	int p;
	for (p = 2; p*p <= n; p++)
		if (n%p == 0) return 0;
	return n > 2;
}

int spiral(int w, int h, int x, int y)
{
	return y ? w + spiral(h - 1, w, y - 1, w - x - 1) : x;
}

int main(int c, char **v)
{
	int i, j, w = 50, h = 50, s = 1;
	if (c > 1 && (w = atoi(v[1])) <= 0) w = 50;
	if (c > 2 && (h = atoi(v[2])) <= 0) h = w;
	if (c > 3 && (s = atoi(v[3])) <= 0) s = 1;

	for (i = 0; i < h; i++) {
		for (j = 0; j < w; j++)
			putchar(isprime(w*h + s - 1 - spiral(w, h, j, i))[" #"]);
		putchar('\n');
	}
	return 0;
}
```



## C++


###  parametric version 


```cpp>#include <cmath

#include <iostream>
#include <string>
#include <iomanip>
#include <vector>

class ulamSpiral {
public:
    void create( unsigned n, unsigned startWith = 1 ) {
        _lst.clear();
        if( !( n & 1 ) ) n++;
        _mx = n;
        unsigned v = n * n;
        _wd = static_cast<unsigned>( log10( static_cast<long double>( v ) ) ) + 1;
        for( unsigned u = 0; u < v; u++ )
            _lst.push_back( -1 );

        arrange( startWith );

    }
    void display( char c ) {
        if( !c ) displayNumbers();
        else displaySymbol( c );
    }

private:
    bool isPrime( unsigned u ) {
        if( u < 4 ) return u > 1;
        if( !( u % 2 ) || !( u % 3 ) ) return false;
 
        unsigned q = static_cast<unsigned>( sqrt( static_cast<long double>( u ) ) ),
                 c = 5;
        while( c <= q ) {
            if( !( u % c ) || !( u % ( c + 2 ) ) ) return false;
            c += 6;
        }
        return true;
    }
    void arrange( unsigned s ) {
        unsigned stp = 1, n = 1, posX = _mx >> 1, 
                 posY = posX, stC = 0;
        int dx = 1, dy = 0;
    
        while( posX < _mx && posY < _mx ) {
            _lst.at( posX + posY * _mx ) =  isPrime( s ) ? s : 0;
            s++;
        
            if( dx ) {
                posX += dx;
                if( ++stC == stp ) {
                    dy = -dx;
                    dx = stC = 0;
                }
            } else {
                posY += dy;
                if( ++stC == stp ) {
                    dx = dy;
                    dy = stC = 0;
                    stp++;
                }
            }
        }
    }
    void displayNumbers() {
        unsigned ct = 0;
        for( std::vector<unsigned>::iterator i = _lst.begin(); i != _lst.end(); i++ ) {
            if( *i ) std::cout << std::setw( _wd ) << *i << " ";
            else std::cout << std::string( _wd, '*' ) << " ";
            if( ++ct >= _mx ) {
                std::cout << "\n";
                ct = 0;
            }
        }
        std::cout << "\n\n";
    }
    void displaySymbol( char c ) {
        unsigned ct = 0;
        for( std::vector<unsigned>::iterator i = _lst.begin(); i != _lst.end(); i++ ) {
            if( *i ) std::cout << c;
            else std::cout << " ";
            if( ++ct >= _mx ) {
                std::cout << "\n";
                ct = 0;
            }
        }
        std::cout << "\n\n";
    }

    std::vector<unsigned> _lst;
    unsigned _mx, _wd;
};

int main( int argc, char* argv[] )
{
    ulamSpiral ulam;
    ulam.create( 9 );
    ulam.display( 0 );
    ulam.create( 35 );
    ulam.display( '#' );
    return 0;
}
```

{{out}}

```txt

** ** ** ** 61 ** 59 ** **
** 37 ** ** ** ** ** 31 **
67 ** 17 ** ** ** 13 ** **
** ** **  5 **  3 ** 29 **
** ** 19 ** **  2 11 ** 53
** 41 **  7 ** ** ** ** **
71 ** ** ** 23 ** ** ** **
** 43 ** ** ** 47 ** ** **
73 ** ** ** ** ** 79 ** **


    # #                     #     #
     # #     #   #           #
                #   # #   #
               #   #       #   # #
    #   #           # #     #
   # #     # #     #     #
#           #           #   #     #
 #     #   #       #     #
  #   #         #   # #   # # #
 #       #     #         # #   #
    #     #   # #               #
                 # #     #   #   #
  #   #   # #   #       #   # #
                 # #   #
#       #   # #     # # #     # # #
 # # # # # # # #   #       #
                # # #           #
           #   #  ## # # #   # # #
    #       # # #
             #   #
  #   # #   # #   #   # #   #   # #
     #   #   #     #     # #   #
              #           #
         # #     #   #   #       #
#     #   #           #       #
 #     #     #   # #
            # #   #     #   #     #
 # # # #         # #     #     # #
    #   #           # #
   # #     #     #   # #
# #       #         #       #     #
           # #   # #         #
        #   #     #     #         #
     # #     #                 #
  #       #           #   #     #
```



###  generic version 

ulam.hpp

```cpp
#pragma once

#include <cmath>
#include <sstream>
#include <iomanip>

inline bool is_prime(unsigned a)  {
   if (a == 2) return true;
   if (a <= 1 || a % 2 == 0) return false;
   const unsigned max(std::sqrt(a));
   for (unsigned n = 3; n <= max; n += 2) if (a % n == 0)  return false;
   return true;
}

enum direction { RIGHT, UP, LEFT, DOWN };
const char* N = " ---";

template<const unsigned SIZE>
class Ulam
{
public:
    Ulam(unsigned start = 1, const char c = '\0') {
        direction dir = RIGHT;
        unsigned y = SIZE / 2;
        unsigned x = SIZE % 2 == 0 ?  y - 1 :  y; // shift left for even n's
        for (unsigned j = start; j <= SIZE * SIZE - 1 + start; j++) {
            if (is_prime(j)) {
                std::ostringstream os("");
                if (c == '\0') os << std::setw(4) << j;
                else           os << "  " << c << ' ';
                s[y][x] = os.str();
            }
            else s[y][x] = N;

            switch (dir) {
            case RIGHT : if (x <= SIZE - 1 && s[y - 1][x].empty() && j > start) { dir = UP; }; break;
            case UP : if (s[y][x - 1].empty()) { dir = LEFT; }; break;
            case LEFT : if (x == 0 || s[y + 1][x].empty()) { dir = DOWN; }; break;
            case DOWN : if (s[y][x + 1].empty()) { dir = RIGHT; }; break;
            }

            switch (dir) {
            case RIGHT : x += 1; break;
            case UP : y -= 1; break;
            case LEFT : x -= 1; break;
            case DOWN : y += 1; break;
            }
        }
    }

    template<const unsigned S> friend std::ostream& operator <<(std::ostream&, const Ulam<S>&);

private:
    std::string s[SIZE][SIZE];
};

template<const unsigned SIZE>
std::ostream& operator <<(std::ostream& os, const Ulam<SIZE>& u) {
    for (unsigned i = 0; i < SIZE; i++) {
        os << '[';
        for (unsigned j = 0; j < SIZE; j++) os << u.s[i][j];
        os << ']' << std::endl;
    }
    return os;
}
```

ulam.cpp

```cpp>#include <cstdlib

#include <iostream>
#include "ulam.hpp"

int main(const int argc, const char* argv[]) {
    using namespace std;

    cout << Ulam<9>() << endl;
    const Ulam<9> v(1, '*');
    cout << v << endl;

	return EXIT_SUCCESS;
}
```

{{out}}

```txt
[ --- --- --- ---  61 ---  59 --- ---]
[ ---  37 --- --- --- --- ---  31 ---]
[  67 ---  17 --- --- ---  13 --- ---]
[ --- --- ---   5 ---   3 ---  29 ---]
[ --- ---  19 --- ---   2  11 ---  53]
[ ---  41 ---   7 --- --- --- --- ---]
[  71 --- --- ---  23 --- --- --- ---]
[ ---  43 --- --- ---  47 --- --- ---]
[  73 --- --- --- --- ---  79 --- ---]

[ --- --- --- ---  *  ---  *  --- ---]
[ ---  *  --- --- --- --- ---  *  ---]
[  *  ---  *  --- --- ---  *  --- ---]
[ --- --- ---  *  ---  *  ---  *  ---]
[ --- ---  *  --- ---  *   *  ---  * ]
[ ---  *  ---  *  --- --- --- --- ---]
[  *  --- --- ---  *  --- --- --- ---]
[ ---  *  --- --- ---  *  --- --- ---]
[  *  --- --- --- --- ---  *  --- ---]
```



## Common Lisp


```lisp

(defun ulam-spiral (n)
  (loop for a in (spiral n n (* n n)) do
        (format t "~{~d~}~%" a)))

(defun spiral
    (n m b &aux (row (loop for a below n
                           collect (if (primep (- b a))
                                       '* '#\space))))
  (if (= m 1) (list row)
      (cons row (mapcar #'reverse
                        (apply #'mapcar #'list
                               (spiral (1- m) n
                                       (- b n)))))))
(defun primep (n)
  (when (> n 1) (loop for a from 2 to (isqrt n)
                      never (zerop (mod n a)))))

```


{{out}}

```txt

> (ulam-spiral 139)
  *         *       *           *               *     *       *         *           *     *           *     * *   *                       *
             *       *     *           *           *     *           *         *           *       *         *                       *     
    *     *     *           *       *         *           *                   *     *         * *   *     * *                     *       *
         * *           *         * *                             *   *             *         * *               * *     *   *               
*     *                     *             *     *         *       *     *               *                       *     *     *       *   *  
 *           * *           *           *   *                             *     *     *       *     *                 *         *           
        *               *       *     *     *   *             *   * *   * *   *                       *                         *     *    
             *           *     *                 * *           *         *       *         *             *               *                 
                *             *     *         *       *     *   *           *       *                     *             *   *       * *    
 * *   *   *     *   * *     *                           * *                           * *   *     * *     *     *   *                 *   
      *     *     *   *     *                                   *                   *   * *         *             *     *                  
             *     * *         *                   *               *     *       *     *               *     *             *     * *       
    *     *       * *                             *   *     *     *       *   *       *   *                   *     *                      
                   *     *             *   *                 *       *         *     *       *                 *           *               
    *     *                       *           *       *   *       *     *         * *         * *           *               * *       *    
     * *   * *               *   *                       *     * *   *     *       *     *     *           *     *   * *           *       
                  *   *     *             *           *           *     *         *             *               * *                     *  
     * *                               *         *     *       *     *               *       *         * *                   * *         * 
*         *     *   *     *   *       *                     *       *                           * *               *                 *      
                           *           *   *       *     *   *       *         * *     *                     *       *                     
  * *   *     *               *   *     *           * *                 *     *   *     *           * *                       * *         *
         *   *   * *       *     *     *             *     *                     *     * *   *             *   *       *               *   
      *                           * *   *     * *                           * *     *           *         *       *         *              
                         *     *           * *   *             *         * *               *       *         *       *       *             
*   * * *   *       *           *   *     *                   *     *   * *         * *         *                 * *   *     * *     *    
   *     *           *     *           *     *                 *   * *   *     *             *           *                           *     
  *                               * *         * *     *   *       *     *     *         *       *         * *         * *                 *
 *     *   *                     *     * *   *             *     *         *                 *       *                 *         *         
*     *           *               *           * *     *     *               *       *   * *   *                 *           *              
 *                 *   * *           * *   *                 *       *                             *         *   *     * *   *             
      *   *   *     *   * * *             *       *   *     *       *   * *                       *         *       *     *   *            
   *                             *   *                   *     *               * *         * *     *   *                       *     *     
          *         *             *                               *         *       *   *     *     *         * *                 *     *  
 * *           *       * *             *             *     *     *               * *     *           *     *                 *             
*           *                       *               * *         * *     *   *     *     *             *               *                 *  
           * *   * *   *     *       *       *                 *   *     * *                 *             *     *       *         *       
    *   *     *       * *   *   *         * *                 *     *     *   *     *                 * *         *                 *      
               *     *     *               * *     *         *                       *                 *     *     *                 *     
                *         * *           *                   *   *           * *     *   * *     *   * * *               * *         *      
       *     *   *   * * *         *     *         *       *   *     *             *   * *           *         *                           
                  *     *     *     *               * *   * *   *       *               *                                                  
                                                             *       *   *       *     *           * *     * * * * * * *         * * * * * 
  *   *   * * *     * *             *   *   *   *       *         * *                           *           * *           *     *     *   *
                                 *               * *   *     * *           *         *           *                 *                       
          *         *     * *         * *           * *           *                     *     * *             *   *   *         *       * *
 * *     * *         *   * * * *               *     *   *     * *   *     *             *           *     *               *               
                                                *   *                       *                 *                       *           *     *  
                 * *     *     *                       *     *       *   * *   *     *       * * *             *         *     *   *     * 
      *         *   *     * *           *     *   *               *       *           *     *           *         *     * *           *    
   *           *           *           *     *               * *   *     *                 *           *                 *                 
  *     * *     *   * *         *     * *     *             *           *     *   * *   *       * *   *     *   *                   *   *  
   *   *           *         *   *         *     *   *     *                                 * *                     * *     *   *     * * 
      *     *           *           *     *           *     *   *           *       *   *     *           *     *                       *  
           *           *       *   *     *             *                 *     * *       *       *       *         * * *                   
    *                 * * *   * *   *       *     * *         *     *     *   *           *             *     *   *             *     *    
         *     *     *     *                       *     *         * *   * *               *           *                 *                 
    *         *                   *   *             *     *       *         *       * *     * * * * *     *   *                 *     *    
               *   *         * *     * * *   *     *           * *   *     *     * *   *     *     *     * *         *     * *     *       
                                                                * *           *   *                             *                 *        
     *           *     * *   * *   *       *   * *   * *     *     * *         * * * *       * *     *   *   * *   *   *     *             
  *   *       * * *               *         * * *   *     *   *     *   * *                     *           *       *     *   *            
                                                                   * *   *     *     *                       *                 *           
  * *           *     *           *     *         *     *       *           *   *     * *             * * * *         * *   *             *
 *   *         *     *   *     *   *       *       * *       *   *   *     * *         * *         *     * *                           *   
      *     *     *                 *           *           *           *                     *     *     *                 *     *        
 *   * *     *   *           * *   *     *     *       *   * *     *     *   *   *     *     *   *   * *       * *                 * *     
    *   *   * *       * *   *     *       * *   *   * *   *   * *   *   * *   * *   *     * *                       *   *                  
                                                                     *   *                       *                 *                 *     
                                                                      * * *       *       * *       * * *         *     *   *     *        
     *   *   * *   *             * * * *       *     * * *   * * * **  *   *             *     *   * *           *         * *   * *   *   
*                 *                 *     *           *           * * *                                                                    
       *     *   *     *             *         * *         *       *   * * * * * * * *   *     * *   * *   *             * *   *     * *   
          * * *     * *           * *               * * *     * * *     * *   *       *   * *     *     *   * *         * *         * *    
   *                             *           *                 *   * *                                                                     
  *           * *   *       *     *           *         * *   *       *   * *   *   *   *     * * *     *     *       *       *            
                 * *         * *     * *   * * *   * *   *   *     * *                                                                     
                  *                       *     *     *               * *   *     *                       *     *           *     *        
 *     *   *       *     *               *     * *     *   * *         *     *       * * * *       *   *   *         *   *             *   
          *     *   *                 *                 * * *   * *   *         *   *     * *                                              
                                 *           *               *     *       *   *     *     *           *           *     *                 
        * *           *                     *     * *     *   *           *           * *   *             * *         *   *   *         *  
   *                                 *           *           *     *     * *     * *   * *     *         *             *     *   * *     * 
                              *     *                     *     * *           *   *     *           *     *                 *              
 *         * *                           *           * *   *       *   *               * *         *       * *         *         *     *   
        * * *   * * *     *     *   * *   * * *             *   * *   *                                                                    
               *           *           *           *     *           *   *     * *                     *                 *     *     *     
              *       *     *     *   *             *     *                     * *     *       *     * *           *     *   * * * *      
 *                   *     * *       * *     * * *                 * *   *     * *     *     * *   * *                     * *             
                  *           *                 *   *             *   * *   *     *                                                        
 *                 *         * *     *     *           *   *       *   *             *   *   * *       *     * *           *           *   
*       *         * *           * *     * * * * *         *       *     *     *     *             *                 *         * *   *      
         *                       *     *               *     *     *                 * *               *                 *           *     
    *         *       *                 *               * *   * *         *           * *     *   * *       *   * * * * *         *   *    
     *       * *                 * *                   * *   *       *     *     *   *         *   *     * *         *       *   *     * * 
                                    *     *         *       *     *   *                             *                 *                    
       *   *     *     * *     *   * *   *     * *   *       *     *     *   * *   *     *                           *             *     * 
  *         *           *       *     *                   *       *         *     *     *       *     * *               *           *     *
         *                 *           *   *     *                   *         * *               *                             *     *     
        *                 *             *     *     *   *     * *           *   *           * *     *         * * *     * *           *    
   * *   * *     *   *   * * * * *     *     *         * *           *         * *         *         *         * *         *     *         
*                                   *           *   *     *                 * *         *                 *           *                    
             *     *                                 * *     *               *     *           * *       * *     *       *   *       *     
  *       *   *     *           *         *                             *         * *         *       * *   * *           *         *     *
                                       *     *               *       *     *     *                 *                                       
  * *   *     * *     *     *     *                             *         *     *           *             * *     * * *               * * *
 *   * *     *     * * * * * *   *     *                           *     *             *   *       *       *     *                         
                  *                 *     *   * *           *     *   * *     *                     *     *                 *           *  
     *                         *         *     *                   *   * *     *                                     *   * *   * * * *     
      * *       *   *     *   *   *                 *       *     *   *     *             *     *     *                   *         * *    
                           *   *     *       *     *         *     * *     *     *     *         *       *                                 
    *     *     *     *   *           *     * *           *     *         * *         * *     *         *             *                   *
       *               *       *     *       *     *   *           *     *       *           *   *             *       *   *     *     * * 
      *                 *           *         * *               * *                 *                       *               *           *  
           * *                       *   *       *     *   *       *   *             *     *   *             *     * * *                 * 
      * *     *       * *     *                     *             *     *         *           *     * *   *       * *         *       *   *
               *           *         * *                             *   *     * *           *   *             *               *           
                          *     *       *                 *   *                   *   *           * *         *     *     * * *         *  
 *         *   * *         *     *   *             *     *   *           *       *     *           *   *     *               *     *   * * 
      *           *     *         *           *     *       *   *                                   *             *         *     *        
       *         * *     *               *     * *         *       *                 *     *                                     *         
  *   * * *   *   *         *       *         * *   *     *             *           *                     *       *       *   * *   *      
                           *     *   *     *             *                 *         *       *         *                       *           
    *           *                 *         * *         * *   *                   *               *                           * *   *   *  
           *                         *       *     *   *                   *   *           *                       *         *   *         
                              *   *                         *   * *     *                     * *                       *   *     *        
     *     * *   *             *     *               *     *       *   *     *     *       *     *         *           * *         *       
    *   *         *   *     *       *   *     * *   *                 *           *     *       *     *     *   *                          
                                     *             *               * *           *     *   *                   *         *     *           
          *                     * *         *                 *             *   * *   *                 * *   *     *       *     * *      
 *   *   *           *         *                 * *   *             *     *                     *                 * *           *     *   
*     *     *   *       *   *             *               *       *     *   *           *       *               * *         *              
             *   *     * *     *         * *         *       *     *     *         * *                 *         * *           *   *     * 
                *     *     *             *   *     *     *     *       *     *                                   *                 * *   *
   *           *     *                           *                 *             *     *         *           * *     *           *         
                    *                       *           *     * *           *                           *       *   *     *           * *  
   *     *                 *   *     *     *       *         *       *                 *     *   * *   *       *               * *         
      *         * *           *           *                 *         *           *     * *         * *     *         *                 *  
       *   *       *                 *   *           *             *     *     *   *       *     *   *                   *           *     
    * *   * *                           *           *             *     *                     *       *   *                 *     *        
NIL

```



## D

{{trans|python}}

```d
import std.stdio, std.math, std.algorithm, std.array, std.range;

int cell(in int n, int x, int y, in int start=1) pure nothrow @safe @nogc {
    x = x - (n - 1) / 2;
    y = y - n / 2;
    immutable l = 2 * max(x.abs, y.abs);
    immutable d = (y > x) ? (l * 3 + x + y) : (l - x - y);
    return (l - 1) ^^ 2 + d + start - 1;
}

void showSpiral(in int n, in string symbol="# ", in int start=1, string space=null) /*@safe*/ {
    if (space is null)
        space = " ".replicate(symbol.length);

    immutable top = start + n ^^ 2 + 1;
    auto isPrime = [false, false, true] ~ [true, false].replicate(top / 2);
    foreach (immutable x; 3 .. 1 + cast(int)real(top).sqrt) {
        if (!isPrime[x])
            continue;
        foreach (immutable i; iota(x ^^ 2, top, x * 2))
            isPrime[i] = false;
    }

    string cellStr(in int x) pure nothrow @safe @nogc {
        return isPrime[x] ? symbol : space;
    }

    foreach (immutable y; 0 .. n)
        n.iota.map!(x => cell(n, x, y, start)).map!cellStr.joiner.writeln;
}

void main() {
    35.showSpiral;
}
```

{{out}}

```txt
        #   #                                           #           # 
          #   #           #       #                       #           
                                #       #   #       #                 
                              #       #               #       #   #   
        #       #                       #   #           #             
      #   #           #   #           #           #                   
#                       #                       #       #           # 
  #           #       #               #           #                   
    #       #                   #       #   #       #   #   #         
  #               #           #                   #   #       #       
        #           #       #   #                               #     
                                  #   #           #       #       #   
    #       #       #   #       #               #       #   #         
                                  #   #       #                       
#               #       #   #           #   #   #           #   #   # 
  #   #   #   #   #   #   #   #       #               #               
                                #   #   #                       #     
                      #       #     # #   #   #   #       #   #   #   
        #               #   #   #                                     
                          #       #                                   
    #       #   #       #   #       #       #   #       #       #   # 
          #       #       #           #           #   #       #       
                            #                       #                 
                  #   #           #       #       #               #   
#           #       #                       #               #         
  #           #           #       #   #                               
                        #   #       #           #       #           # 
  #   #   #   #                   #   #           #           #   #   
        #       #                       #   #                         
      #   #           #           #       #   #                       
#   #               #                   #               #           # 
                      #   #       #   #                   #           
                #       #           #           #                   # 
          #   #           #                                   #       
    #               #                       #       #           #     
```



### Alternative Version

This generates a PGM image, using the module from the Grayscale Image Task;

```d
import std.stdio, std.math, std.algorithm, std.array, grayscale_image;

uint cell(in uint n, int x, int y, in uint start=1) pure nothrow @safe @nogc {
    x = x - (n - 1) / 2;
    y = y - n / 2;
    immutable l = 2 * max(x.abs, y.abs);
    immutable d = (y > x) ? (l * 3 + x + y) : (l - x - y);
    return (l - 1) ^^ 2 + d + start - 1;
}

bool[] primes(in uint n, in uint top, in uint start=1) pure nothrow @safe {
    auto isPrime = [false, false, true] ~ [true, false].replicate(top / 2);

    foreach (immutable x; 3 .. 1 + cast(uint)real(top).sqrt)
        if (isPrime[x])
            for (uint i = x ^^ 2; i < top; i += x * 2)
                isPrime[i] = false;
    return isPrime;
}

void main() {
    enum n = 512;
    enum start = 1;
    immutable top = start + n ^^ 2 + 1;
    immutable isPrime = primes(n, top, start);
    auto img = new Image!Gray(n, n);

    foreach (immutable y; 0 .. n)
        foreach (immutable x; 0 .. n)
            img[x, y] = isPrime[cell(n, x, y, start)] ? Gray.black : Gray.white;

    img.savePGM("ulam_spiral.pgm");
}
```



## EchoLisp

The plot libray includes a '''plot-spiral''' function. The nice result is here : [http://www.echolalie.org/echolisp/help.html#plot-spiral  EchoLisp Ulam spiral] .

```scheme

(lib 'plot)

(define *red* (rgb 1 0 0))
(define (ulam n nmax) (if ( prime? n) *red* (gray (// n nmax))))
(plot-spiral ulam 1000) ;; range [0...1000]

```



## Elixir

{{trans|Ruby}}

```elixir
defmodule Ulam do
  defp cell(n, x, y, start) do
    y = y - div(n, 2)
    x = x - div(n - 1, 2)
    l = 2 * max(abs(x), abs(y))
    d = if y >= x, do: l*3 + x + y, else: l - x - y
    (l - 1)*(l - 1) + d + start - 1
  end
  
  def show_spiral(n, symbol\\nil, start\\1) do
    IO.puts "\nN : #{n}"
    if symbol==nil, do: format = "~#{length(to_char_list(start + n*n - 1))}s "
    prime = prime(n*n + start)
    Enum.each(0..n-1, fn y ->
      Enum.each(0..n-1, fn x ->
        i = cell(n, x, y, start)
        if symbol do
          IO.write if i in prime, do: Enum.at(symbol,0), else: Enum.at(symbol,1)
        else
          :io.fwrite format, [if i in prime do to_char_list(i) else "" end]
        end
      end)
      IO.puts ""
    end)
  end
  
  defp prime(num), do: prime(Enum.to_list(2..num), [])
  defp prime([], p), do: Enum.reverse(p)
  defp prime([h|t], p), do: prime((for i <- t, rem(i,h)>0, do: i), [h|p])
end

Ulam.show_spiral(9)
Ulam.show_spiral(25)
Ulam.show_spiral(25, ["#"," "])
```


{{out}}

```txt

N : 9
            61    59       
   37                31    
67    17          13       
          5     3    29    
      19        2 11    53 
   41     7                
71          23             
   43          47          
73                79       

N : 25
577                     571     569                     563                     557                 
                            479                                             467             463     
        401             397                             389                     383                 
    487                                     317             313     311             307     461     
                257                     251                                     241     379         
                    197             193     191                                                     
                                                139     137                     239             547 
    491             199     101              97                             181             457     
                                                 61      59             131                         
            331             103      37                      31      89     179                     
587     409     263     149      67      17              13                             373         
                                              5       3      29                                     
                        151              19           2  11      53     127     233             541 
                            107      41       7                                                     
                                 71              23                                                 
    499     337             109      43              47              83     173             449     
593             269              73                      79                     229     367         
                                    113                                             293             
                271     157                     163             167             227                 
    503             211                                             223                             
        419                     277             281     283                                         
                            347     349             353                     359             443     
599     421                                     431     433                     439                 
            509                                             521     523                             
601                     607                     613             617     619                         

N : 25
#     # #     #     #    
       #           #   # 
  #   #       #     #    
 #         #   # #   # # 
    #     #         # #  
     #   # #             
            # #     #   #
 #   # #   #       #   # 
            # #   #      
   #   # #     # # #     
# # # # # #   #       #  
           # # #         
      #   #  ## # # #   #
       # # #             
        #   #            
 # #   # #   #   # #   # 
#   #   #     #     # #  
         #           #   
    # #     #   #   #    
 #   #           #       
  #     #   # #          
       # #   #     #   # 
# #         # #     #    
   #           # #       
#     #     #   # #      

```



## ERRE


```ERRE
PROGRAM SPIRAL

!$INTEGER

CONST RIGHT=1,UP=2,LEFT=3,DOWN=4

!$DYNAMIC
DIM SPIRAL$[0,0]

PROCEDURE PRT_ULAM(N)
  FOR ROW=0 TO N DO
    FOR COL=0 TO N DO
        PRINT(SPIRAL$[ROW,COL];)
    END FOR
    PRINT
  END FOR
  PRINT
  GET(K$)
  FOR ROW=0 TO N DO
    FOR COL=0 TO N DO
        IF VAL(SPIRAL$[ROW,COL])<>0 THEN PRINT("  * ";)  ELSE PRINT(SPIRAL$[ROW,COL];) END IF
    END FOR
    PRINT
  END FOR
END PROCEDURE

PROCEDURE IS_PRIME(A->RES%)
     LOCAL N
     IF A=2 THEN RES%=TRUE EXIT PROCEDURE END IF
     IF A<=1 OR (A MOD 2=0) THEN RES%=FALSE EXIT PROCEDURE END IF
     MAX=SQR(A)
     FOR N=3 TO MAX STEP 2 DO
        IF (A MOD N=0) THEN RES%=FALSE EXIT PROCEDURE END IF
     END FOR
     RES%=TRUE
END PROCEDURE

PROCEDURE GEN_ULAM(N,I)
     DIR=RIGHT
     J=I
     Y=INT(N/2)
     IF (N MOD 2=0) THEN X=Y-1 ELSE X=Y END IF ! shift left for even n's
          WHILE J<=(N*N)-1+I DO

              IS_PRIME(J->RES%)
              IF RES% THEN SPIRAL$[Y,X]=RIGHT$("  "+STR$(J),4) ELSE SPIRAL$[Y,X]=" ---" END IF

              CASE DIR OF
                    RIGHT->
                       IF (X<=(N-1) AND SPIRAL$[Y-1,X]="" AND J>I) THEN DIR=UP END IF
                    END ->
                    UP->
                       IF SPIRAL$[Y,X-1]="" THEN DIR=LEFT END IF
                    END ->
                    LEFT->
                       IF (X=0) OR SPIRAL$[Y+1,X]="" THEN DIR=DOWN END IF
                    END ->
                    DOWN->
                       IF SPIRAL$[Y,X+1]="" THEN DIR=RIGHT END IF
                    END ->
              END CASE

              CASE DIR OF
                    RIGHT-> X=X+1 END ->
                    UP->    Y=Y-1 END ->
                    LEFT->  X=X-1 END ->
                    DOWN->  Y=Y+1 END ->
              END CASE
              J=J+1
          END WHILE
          PRT_ULAM(N)
END PROCEDURE

BEGIN
     N=9
     !$DIM SPIRAL$[N,N]
     GEN_ULAM(N,1)
END PROGRAM
```

{{out}}

```txt

 --- --- --- ---  61 ---  59 --- ---
 ---  37 --- --- --- --- ---  31 ---
  67 ---  17 --- --- ---  13 --- ---
 --- --- ---   5 ---   3 ---  29 ---
 --- ---  19 --- ---   2  11 ---  53
 ---  41 ---   7 --- --- --- --- ---
  71 --- --- ---  23 --- --- --- ---
 ---  43 --- --- ---  47 --- --- ---
  73 --- --- --- --- ---  79 --- ---


 --- --- --- ---  *  ---  *  --- ---
 ---  *  --- --- --- --- ---  *  ---
  *  ---  *  --- --- ---  *  --- ---
 --- --- ---  *  ---  *  ---  *  ---
 --- ---  *  --- ---  *   *  ---  *
 ---  *  ---  *  --- --- --- --- ---
  *  --- --- ---  *  --- --- --- ---
 ---  *  --- --- ---  *  --- --- ---
  *  --- --- --- --- ---  *  --- ---

```



## Factor

{{trans|J}}

```factor
USING: arrays grouping kernel math math.combinatorics
math.matrices math.primes math.ranges math.statistics
prettyprint sequences sequences.repeating ;
IN: rosetta-code.ulam-spiral

: counts ( n -- seq ) 1 [a,b] 2 repeat rest ;

: vals ( n -- seq )
    [ -1 swap neg 2dup [ neg ] bi@ 4array ] [ 2 * 1 - cycle ] bi ;

: evJKT2 ( n -- seq )
    [ counts ] [ vals ] bi [ <array> ] 2map concat ;

: spiral ( n -- matrix )
    [ evJKT2 cum-sum inverse-permutation ] [ group ] bi ;

: ulam-spiral ( n -- matrix )
    spiral dup dim first sq 1 -
    [ swap - 1 + prime? "o " "  " ? ] curry matrix-map ;

: ulam-demo ( -- ) 21 ulam-spiral simple-table. ;

MAIN: ulam-demo
```

{{out}}

```txt

o           o                       o                 o       
                           o           o     o           o    
      o                 o                             o     o 
         o           o     o                                  
                              o     o                 o       
         o     o           o                       o          
                              o     o           o             
   o           o     o                 o     o     o          
o     o     o     o     o           o                       o 
                           o     o     o                      
            o           o        o  o     o     o     o       
               o     o     o                                  
                  o           o                               
   o           o     o           o           o     o          
      o           o                 o                 o     o 
                     o                                   o    
      o     o                 o           o           o       
         o                                   o                
o                 o           o     o                         
               o     o           o                 o          
o                             o     o                 o       

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Ulam_spiral_(for_primes) this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

{{works with|GNU Forth|0.7.0}}
All array manipulations were taken from Rosetta Code examples.

```forth

 43 constant border                                                         \ grid size is border x border
 border border * constant size                                                    

 variable crawler                                                           \ position of the crawler

 : set.crawler border 2 mod 0= if                                           \ positions the crawler in the middle of the grid
	 size 2 / border 2/ + 1 - crawler !
	 else 
	 size 2 / crawler ! then ;
	
 set.crawler                                                                
 create Grid size cells allot                                               \ creates the grid
 here constant GridEnd                                                      \ used for debugging 

 : is.divisor                                                                       
	over 2over
	mod 0= swap drop + ;

 : sub.one 
	 swap 1 - swap ;

 : next.div
	 is.divisor sub.one ;
 
 : three.test                                                                \ counts divisors for numbers bigger than 2
	 dup 0 
     begin
	 next.div 
     over 1 = until 
	 swap drop
	 swap drop 1 + ;
	
 : not.prime                                                                 \ counts the number of divisors. Primes have exactly two.
    dup
    2 < if drop true else 
	three.test then ;

 : sub.four                                                                   \ the crawler takes a number from the stack as direction
	 dup 4 > if 4 - then ;                                                \ this word makes the number roll over.                                                                
                                                                              \ 1-right 2-up 3-left 4-down
 : craw.left                                                                  \ rotates the crawler 90 degrees counter-clockwise
	 1 + sub.four ;

 : scan.right 
	 grid crawler @ 1 + cells + @ 0= ;                                    \ checks if cell to the right of the crawler is zero

 : scan.left 
	 grid crawler @ 1 - cells + @ 0= ;                                    \ cell to the left

 : scan.up 
	 grid crawler @ border - cells + @ 0= ;                               \ cell above

 : scan.down 
	 grid crawler @ border + cells + @ 0= ;                               \ and cell below

 : crawler.go                                                                 \ moves crawler one cell ahead checks cell to the left... 
	 dup                                                                  \ ...of the direction the crawler is facing, if zero, turns
	 1 = if crawler @ 1 + crawler ! scan.up if craw.left then else       
	 dup 
	 2 = if crawler @ border - crawler ! scan.left if craw.left then else
	 dup 
	 3 = if crawler @ 1 - crawler ! scan.down if craw.left then else
	 dup 
	 4  = if crawler @ border + crawler ! scan.right if craw.left then else
	
	 then then then then ;
	
 : run.crawler                                                              \ crawler moves through the grid and fills it with numbers
	 border 2 < if 1 grid 0 cells + ! else                              \ if the grid is a single cell, puts 1 in it
	 crawler @ border - crawler !	                                    \ crawler moves one step and turn before setting the first... 
	 4                                                                  \ ...number so it is repositioned one cell up facing down
  	 size -1 * 0 do  i
	 i -1 * grid crawler @ cells + ! drop
	 crawler.go
	 -1 +loop then drop ;
 
 : leave.primes                                                                    \ removes non-primes from the grid
	 size 0 do i
	 grid i cells + @ not.prime if 0 grid i cells + ! then drop
	 loop ;
 
 : star.draw1                                                                      \ draws a "*" where number is not zero
	 0> if 42 emit else 32 emit
	 then ;
 
 : star.draw2
	 0> if 42 emit 32 emit else 32 emit 32 emit                                 \ same but adds a space for better presentation
	 then ;
 
 : star.draw3
	 0> if 32 emit 42 emit 32 emit else 32 emit 32 emit 32 emit                 \ adds two spaces
	 then ;

 : draw.grid                                                                         \ cuts the array into lines and displays it
	page
	size 0 do i
	i border mod 0= if  cr  then
	grid i cells + @ star.draw2 drop                                             \ may use star.draw1 or 3 here
	loop ; 
	
 : ulam.spiral run.crawler leave.primes draw.grid ;                                  \ draws the spiral. Execute this word to run.

```
	
{{out}} 
<pre style="height:60ex;overflow:scroll">


            *           *           *           *               *                   *
  *       *                           *       *               *       *
                *           *       *   *       *                           *       *
      *           *   *           *       *   *                                   *
    *           *   *                                           *           *
                  *   *           *       *                       *           *
                                        *       *   *       *
  *   *                               *       *               *       *   *
    *           *       *                       *   *           *
  *   *       *   *           *   *           *           *                       *
    *   *                       *                       *       *           *   *
          *           *       *               *           *
*           *       *                   *       *   *       *   *   *
  *   *   *               *           *                   *   *       *           *
                *           *       *   *                               *           *
                                          *   *           *       *       *   *
    *       *       *       *   *       *               *       *   *
                                          *   *       *
*       *               *       *   *           *   *   *           *   *   *
  *       *   *   *   *   *   *   *   *       *               *                   *
                                        *   *   *                       *
  *                           *       *     * *   *   *   *       *   *   *
*               *               *   *   *
                                  *       *
*           *       *   *       *   *       *       *   *       *       *   *       *
      *           *       *       *           *           *   *       *
                                    *                       *                       *
  *   *                   *   *           *       *       *               *   *
    *   *           *       *                       *               *           *
          *           *           *       *   *
                                *   *       *           *       *           *       *
          *   *   *   *                   *   *           *           *   *       *
                *       *                       *   *
      *       *   *           *           *       *   *                       *
        *   *               *                   *               *           *
                              *   *       *   *                   *           *
*                       *       *           *           *                   *   *
  *               *   *           *                                   *
    *       *               *                       *       *           *
                                                              *           *       *
    *       *   *       *           *                       *
                                  *           *       *   *
        *                       *               *                               *      ok


```



## Fortran

{{works with|Fortran|95 and later}}
Only works with odd sized squares

```fortran
program ulam
  implicit none

  integer, parameter :: nsize = 49
  integer :: i, j, n, x, y
  integer :: a(nsize*nsize) = (/ (i, i = 1, nsize*nsize) /)
  character(1)  :: spiral(nsize, nsize) = " " 
  character(2)  :: sstr
  character(10) :: fmt
  
  n = 1
  x = nsize / 2 + 1
  y = x
  if(isprime(a(n))) spiral(x, y) = "O"
  n = n + 1

  do i = 1, nsize-1, 2
    do j = 1, i
      x = x + 1
      if(isprime(a(n))) spiral(x, y) = "O"
      n = n + 1
    end do

    do j = 1, i
      y = y - 1
      if(isprime(a(n))) spiral(x, y) = "O"
      n = n + 1
    end do

    do j = 1, i+1
      x = x - 1
      if(isprime(a(n))) spiral(x, y) = "O"
      n = n + 1
    end do

    do j = 1, i+1
      y = y + 1
      if(isprime(a(n))) spiral(x, y) = "O"
      n = n + 1
    end do
  end do

  do j = 1, nsize-1
    x = x + 1
    if(isprime(a(n))) spiral(x, y) = "O"
    n = n + 1
  end do

  write(sstr, "(i0)") nsize
  fmt = "(" // sstr // "(a,1x))"
  do i = 1, nsize
    write(*, fmt) spiral(:, i)
  end do

contains

function isprime(number)
  logical :: isprime
  integer, intent(in) :: number
  integer :: i
 
  if(number == 2) then
    isprime = .true.
  else if(number < 2 .or. mod(number,2) == 0) then
    isprime = .false.
  else
    isprime = .true.
    do i = 3, int(sqrt(real(number))), 2
      if(mod(number,i) == 0) then
        isprime = .false.
        exit
      end if
    end do
  end if
end function
end program
```

Output:

```txt
                O       O           O           O               O       O   O
          O   O                       O                   O   O       O   O
            O   O                                   O           O           O
                  O           O           O           O               O                   O   O
O       O       O                           O       O               O       O
                      O           O       O   O       O                           O       O
O           O           O   O           O       O   O                                   O   O   O
          O           O   O                                           O           O
                        O   O           O       O                       O           O
                                              O       O   O       O                           O
        O   O                               O       O               O       O   O
          O           O       O                       O   O           O
        O   O       O   O           O   O           O           O                       O
  O       O   O                       O                       O       O           O   O
    O           O           O       O               O           O                               O
  O   O           O       O                   O       O   O       O   O   O
    O   O   O   O               O           O                   O   O       O           O   O
                      O           O       O   O                               O           O
                                                O   O           O       O       O   O       O   O
          O       O       O       O   O       O               O       O   O                   O
                                                O   O       O                                   O
  O   O       O               O       O   O           O   O   O           O   O   O
        O       O   O   O   O   O   O   O   O       O               O                   O   O
                                              O   O   O                       O
        O                           O       O     O O   O   O   O       O   O   O           O
  O   O               O               O   O   O
                                        O       O
  O   O           O       O   O       O   O       O       O   O       O       O   O       O
O           O           O       O       O           O           O   O       O               O
                                          O                       O                       O
        O   O                   O   O           O       O       O               O   O
          O   O           O       O                       O               O           O
                O           O           O       O   O
                                      O   O       O           O       O           O       O   O
O               O   O   O   O                   O   O           O           O   O       O   O
                      O       O                       O   O
O           O       O   O           O           O       O   O                       O           O
  O           O   O               O                   O               O           O
    O                               O   O       O   O                   O           O
      O                       O       O           O           O                   O   O
        O               O   O           O                                   O
          O       O               O                       O       O           O
O                                                                   O           O       O
          O       O   O       O           O                       O                           O
    O                                   O           O       O   O                               O
  O           O                       O               O                               O       O
O               O           O       O   O       O               O           O
                                  O                                               O       O
        O                           O           O       O   O           O       O           O
```



### But if you can use complex numbers...

Notice that there each move comes in pairs, lengths 1,1, 2,2, 3,3, 4,4, ... with a quarter turn for each move. The order of the work area must be an odd number so that there is a definite middle element to start with and the worm fits between the bounds of the work area rather than striking one wall and leaving tiles unused.

```Fortran

      SUBROUTINE ULAMSPIRAL(START,ORDER)	!Idle scribbles can lead to new ideas.
Careful with phasing: each lunge's first number is the second placed along its direction.
       INTEGER START	!Usually 1.
       INTEGER ORDER	!MUST be an odd number, so there is a middle.
       INTEGER L,M,N	!Counters.
       INTEGER STEP,LUNGE	!In some direction.
       COMPLEX WAY,PLACE	!Just so.
       CHARACTER*1 SPLOT(0:1)	!Tricks for output.
       PARAMETER (SPLOT = (/" ","*"/))	!Selected according to ISPRIME(n)
       INTEGER TILE(ORDER,ORDER)	!Work area.
        WRITE (6,1) START,ORDER	!Here we go.
    1   FORMAT ("Ulam spiral starting with ",I0,", of order ",I0,/)
        IF (MOD(ORDER,2) .NE. 1) STOP "The order must be odd!"	!Otherwise, out of bounds.
        M = ORDER/2 + 1		!Find the number of the middle.
        PLACE = CMPLX(M,M)	!Start there.
        WAY = (1,0)		!Thence in the +x direction.
        N = START		!Different start, different layout.
        DO L = 1,ORDER		!Advance one step, then two, then three, etc.
          DO LUNGE = 1,2		!But two lunges for each length.
            DO STEP = 1,L			!Take the steps.
              TILE(INT(REAL(PLACE)),INT(AIMAG(PLACE))) = N	!This number for this square.
              PLACE = PLACE + WAY		!Make another step.
              N = N + 1				!Count another step.
            END DO				!And consider making another.
            IF (N .GE. ORDER**2) EXIT	!Otherwise, one lunge too many!
            WAY = WAY*(0,1)		!Rotate a quarter-turn counter-clockwise.
          END DO			!And make another lunge.
        END DO			!Until finished.
Cast forth the numbers.
c        DO L = ORDER,1,-1	!From the top of the grid to the bottom.
c          WRITE (6,66) TILE(1:ORDER,L)	!One row at at time.
c   66     FORMAT (666I6)	!This will do for reassurance.
c        END DO			!Line by line.
Cast forth the splots.
        DO L = ORDER,1,-1	!Just put out a marker.
          WRITE (6,67) (SPLOT(ISPRIME(TILE(M,L))),M = 1,ORDER)	!One line at a time.
   67     FORMAT (666A1)	!A single character at each position.
        END DO			!On to the next row.
      END SUBROUTINE ULAMSPIRAL	!So much for a boring lecture.

      INTEGER FUNCTION ISPRIME(N)	!Returns 0 or 1.
       INTEGER N	!The number.
       INTEGER F,Q	!Factor and quotient.
        ISPRIME = 0		!The more likely outcome.
        IF (N.LE.1) RETURN	!Just in case the start is peculiar.
        IF (N.LE.3) GO TO 2	!Oops! I forgot this!
        IF (MOD(N,2).EQ.0) RETURN	!Special case.
        F = 1			!Now get stuck in to testing odd numbers.
    1   F = F + 2		!A trial factor.
        Q = N/F			!The quotient.
        IF (N .EQ. Q*F) RETURN	!No remainder? Not a prime.
        IF (Q.GT.F) GO TO 1	!Thus chug up to the square root.
    2   ISPRIME = 1		!Well!
      END FUNCTION ISPRIME	!Simple enough.

      PROGRAM TWIRL
        CALL ULAMSPIRAL(1,49)
      END

```

One could escalate to declaring function IsPrime to be PURE so that it may be used in array expressions, such as CANVAS = SPLOT(ISPRIME(TILE)) where CANVAS is an array of single characters, but that would require another large array. Trying instead to do the conversion only a line at a time in the WRITE statement as SPLOT(ISPRIME(TILE(1:ORDER,L))) failed, only one symbol per line appeared. So instead, an older-style implicit DO-loop, and the results are...

```txt

        *   *     *     *       *   * *          
     * *           *         * *   * *           
      * *                 *     *     *          
         *     *     *     *       *         * * 
*   *   *             *   *       *   *          
           *     *   * *   *             *   *   
*     *     * *     *   * *                 * * *
     *     * *                     *     *       
            * *     *   *           *     *      
                       *   * *   *             * 
    * *               *   *       *   * *        
     *     *   *           * *     *             
    * *   * *     * *     *     *           *    
 *   * *           *           *   *     * *     
  *     *     *   *       *     *               *
 * *     *   *         *   * *   * * *           
  * * * *       *     *         * *   *     * *  
           *     *   * *               *     *   
                        * *     *   *   * *   * *
     *   *   *   * *   *       *   * *         * 
                        * *   *                 *
 * *   *       *   * *     * * *     * * *       
    *   * * * * * * * *   *       *         * *  
                       * * *           *         
    *             *   *  ** * * *   * * *     *  
 * *       *       * * *                         
                    *   *                        
 * *     *   * *   * *   *   * *   *   * *   *   
*     *     *   *   *     *     * *   *       *  
                     *           *           *   
    * *         * *     *   *   *       * *      
     * *     *   *           *       *     *     
        *     *     *   * *                      
                   * *   *     *   *     *   * * 
*       * * * *         * *     *     * *   * *  
           *   *           * *                   
*     *   * *     *     *   * *           *     *
 *     * *       *         *       *     *       
  *               * *   * *         *     *      
   *           *   *     *     *         * *     
    *       * *     *                 *          
     *   *       *           *   *     *         
*                                 *     *   *    
     *   * *   *     *           *             * 
  *                 *     *   * *               *
 *     *           *       *               *   * 
*       *     *   * *   *       *     *          
                 *                       *   *   
    *             *     *   * *     *   *     *   

```

Bounding the display with framework symbols might help readability, but is not in the specification.


## Go

{{trans|Kotlin}}

```go
package main

import (
	"math"
	"fmt"
)

type Direction byte

const (
	RIGHT Direction = iota
	UP
	LEFT
	DOWN
)

func generate(n,i int, c byte) {
	s := make([][]string, n)
	for i := 0; i < n; i++ { s[i] = make([]string, n) }
	dir := RIGHT
	y := n / 2
	var x int
	if (n % 2 == 0) { x = y - 1 } else { x = y } // shift left for even n's

	for j := i; j <= n * n - 1 + i; j++ {
		if (isPrime(j)) {
			if (c == 0) { s[y][x] = fmt.Sprintf("%3d", j) } else { s[y][x] = fmt.Sprintf("%2c ", c) }
		} else { s[y][x] = "---" }

		switch dir {
		case RIGHT : if (x <= n - 1 && s[y - 1][x] == "" && j > i) { dir = UP }
		case UP : if (s[y][x - 1] == "") { dir = LEFT }
		case LEFT : if (x == 0 || s[y + 1][x] == "") { dir = DOWN }
		case DOWN : if (s[y][x + 1] == "") { dir = RIGHT }
		}

		switch dir {
		case RIGHT : x += 1
		case UP : y -= 1
		case LEFT : x -= 1
		case DOWN : y += 1
		}
	}

	for _, row := range s { fmt.Println(fmt.Sprintf("%v", row)) }
	fmt.Println()
}

func isPrime(a int) bool {
	if (a == 2) { return true }
	if (a <= 1 || a % 2 == 0) { return false }
	max := int(math.Sqrt(float64(a)))
	for n := 3; n <= max; n += 2 { if (a % n == 0) { return false } }
	return true
}

func main() {
	generate(9, 1, 0) // with digits
	generate(9, 1, '*') // with *
}
```



## Haskell


Haskell encourages splitting the task into indepentend parts each having very clear functionality:

1. preparation of data: a list of numbers is mapped into the list of symbols according to primality, or any other criterion.

2. spooling the list of arbitrary data into the spiral, forming a table.
 
3. displaying arbitrary table at a console or graphically.

As a program the given task then formulates as following:


```haskell
import Data.List
import Data.Numbers.Primes

ulam n representation = swirl n . map representation 
```


Here we refference the function <code>swirl n</code>, which for a given (possibly infinite) list returns <code>n</code> whorls of a spiral.

The spiral is formed in a way we would fold a paper band: first we chop the band into pieces of increasing length, then we take necessary amount of pieces, finally we fold all pieces into the spiral, starting with the empty table by rotating it and adding pieces of data one by one:


```haskell
swirl n = spool . take (2*(n-1)+1) . chop 1

chop n lst = let (x,(y,z)) = splitAt n <$> splitAt n lst
             in x:y:chop (n+1) z

spool = foldl (\table piece -> piece : rotate table) [[]]
  where rotate = reverse . transpose
```


That's it!


###  Textual output 


Pretty printing the table of strings with given column width is simple:


```haskell
showTable w = foldMap (putStrLn . foldMap pad)
  where pad s = take w $ s ++ repeat ' '
```


{{Out}}


```txt
λ> showTable 3 $ ulam 10 show [1..]
91 92 93 94 95 96 97 98 99 100
90 57 58 59 60 61 62 63 64 65 
89 56 31 32 33 34 35 36 37 66 
88 55 30 13 14 15 16 17 38 67 
87 54 29 12 3  4  5  18 39 68 
86 53 28 11 2  1  6  19 40 69 
85 52 27 10 9  8  7  20 41 70 
84 51 26 25 24 23 22 21 42 71 
83 50 49 48 47 46 45 44 43 72 
82 81 80 79 78 77 76 75 74 73 

λ> showTable 3 $ ulam 10 (\x -> if isPrime x then show x else " . ") [1..]
 .  .  .  .  .  . 97  .  .  . 
 .  .  . 59  . 61  .  .  .  . 
89  . 31  .  .  .  .  . 37  . 
 .  .  . 13  .  .  . 17  . 67 
 .  . 29  . 3   . 5   .  .  . 
 . 53  . 11 2   .  . 19  .  . 
 .  .  .  .  .  . 7   . 41  . 
 .  .  .  .  . 23  .  .  . 71 
83  .  .  . 47  .  .  . 43  . 
 .  .  . 79  .  .  .  .  . 73 

λ> showTable 2 $ ulam 20 (\x -> if isPrime x then "*" else "") [1..]
    *           *               *       
  *       *   *       *                 
*   *                   *           *   
                      *   *       *     
    *           *   *                   
      *               *       *   *     
        *       *   *                   
      *   *   *           *   *       * 
*               *       *   *   *   *   
              *   *   *                 
    *   *   *   * *     *       *       
                      *   *   *         
                    *       *           
      *   *       *       *   *       * 
*   *           *           *       *   
  *                       *             
    *       *       *           *   *   
          *                       *     
                *   *       *           
      *           *       *   *         
```


The high modularity of the program allows us easily to start from any number and to proceed with any step size:


```txt
λ> showTable 2 $ ulam 20 (\x -> if isPrime x then "*" else "") [3,5..]
      *   *             *         *     
*   * *           *         * *   *     
  *     *   *       *   *     *       * 
*     *     *     *   *     *           
    *     * *     *   * *         *     
  *               * *   * *     *   * * 
*       * *             *   *     * * * 
      *   * *     *   * *               
  *           *   * *                   
  *     *   * *   *   * * * * *         
*   * *   * *   * * * *     *     *     
                  * *   * *             
* * * * *   * *     *   *       *     * 
    * *   * *   *       *               
            *     *     * *     *     * 
* * *       * *     *   * *   * * * *   
          *     *             *   *     
        *     *   *     * *           * 
*     *     * *     *     *         *   
      *         *       *         *     
```


Or we can form a spiral out of arbitrary data:   (''but that doesn't show the primes as this task requires''): 


```txt
λ> showTable 1 $ ulam 10 (:[]) "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse consequat lectus at massa tristique, ut vulputate arcu pretium."
assa trist
m Suspendi
 .nsectets
ttodolorus
aic rem re
 l moL s  
se,uspiiac
u tema tdo
tgnicsipin
cel tauqes
```



###  Graphical output 


Simple graphical output could be done using <code>Diagrams</code> framework:

```Haskell
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

drawTable tbl = foldl1 (===) $ map (foldl1 (|||)) tbl :: Diagram B

dots x = (circle 1 # if isPrime x then fc black else fc white) :: Diagram B

main = mainWith $ drawTable $ ulam 100 dots [1..]
```



## J


Let's start with our implementation of [[Spiral_matrix#J|spiral]]:


```J
spiral =: ,~ $ [: /: }.@(2 # >:@i.@-) +/\@# <:@+: $ (, -)@(1&,)
```


We can get a spiral starting with 1 in the center of the square by subtracting these values from the square of our size argument:


```J
   spiral 5
 0  1  2  3 4
15 16 17 18 5
14 23 24 19 6
13 22 21 20 7
12 11 10  9 8
   (*: - spiral) 5
25 24 23 22 21
10  9  8  7 20
11  2  1  6 19
12  3  4  5 18
13 14 15 16 17
```


Next, we want to determine which of these numbers are prime:


```J
   (1 p: *: - spiral) 5
0 0 1 0 0
0 0 0 1 0
1 1 0 0 1
0 1 0 1 0
1 0 0 0 1
```


And, finally, we want to use these values to select from a pair of characters:


```J
   (' o' {~ 1 p: *: - spiral) 5
  o  
   o 
oo  o
 o o 
o   o
```


If we want our spiral to start with some value other than 1, we'd add that value - 1 to our numbers right before the prime check. For this, we want a function which returns 0 when there's no left argument and one less than the left argument when it that value present. We can use : for this -- it takes two verbs, the left of which is used when no left argument is present and the right one is used when a left argument is present. (And note that in J, : is a token forming character, so we will need to leave a space to the left of : so that it does not form a different token):


```J
   (0: :(<:@[)) ''
0
   3 (0: :(<:@[)) ''
2
```


We also want to specify that our initial computations only respect the right argument, and we should maybe add a space after every character to get more of a square aspect ratio in typical text displays:


```J
ulam=: 1j1 #"1 ' o' {~ 1 p: 0: :(<:@[) + *:@] - spiral@]
```


And here it is in action:


```J
   ulam 16
          o                   o 
o       o   o                   
              o   o           o 
o   o       o               o   
              o   o       o     
    o   o           o   o   o   
  o   o   o       o             
            o   o   o           
  o       o     o o   o   o   o 
    o   o   o                   
      o       o                 
    o   o       o       o   o   
      o           o           o 
        o                       
  o           o       o       o 
o                       o       
   9 ulam 12
  o   o                 
o   o       o   o       
  o   o       o       o 
        o   o       o   
          o           o 
o   o   o   o   o       
              o   o     
            o           
  o   o   o       o     
        o               
      o           o   o 
```


To transform these spirals to the orientation which has recently been added as a part of the task, you could flip them horizontally (|."1) and vertically (|.)

It should also be possible to redefine the original spiral treatment in some other ways.

See also [https://www.youtube.com/watch?v=dBC5vnwf6Zw https://www.youtube.com/watch?v=dBC5vnwf6Zw] for some variations on this theme (the bit about perfect squares of the count of prime factors is striking).


## Java

{{works with|Java|1.5+}}

```java5
import java.util.Arrays;

public class Ulam{
	enum Direction{
		RIGHT, UP, LEFT, DOWN;
	}
	
	private static String[][] genUlam(int n){
		return genUlam(n, 1);
	}

	private static String[][] genUlam(int n, int i){
		String[][] spiral = new String[n][n];
		Direction dir = Direction.RIGHT;
		int j = i;
		int y = n / 2;
		int x = (n % 2 == 0) ? y - 1 : y; //shift left for even n's
		while(j <= ((n * n) - 1 + i)){
			spiral[y][x] = isPrime(j) ? String.format("%4d", j) : " ---";

			switch(dir){
			case RIGHT:
				if(x <= (n - 1) && spiral[y - 1][x] == null && j > i) dir = Direction.UP; break;
			case UP:
				if(spiral[y][x - 1] == null) dir = Direction.LEFT; break;
			case LEFT:
				if(x == 0 || spiral[y + 1][x] == null) dir = Direction.DOWN; break;
			case DOWN:
				if(spiral[y][x + 1] == null) dir = Direction.RIGHT; break;
			}
			
			switch(dir){
				case RIGHT:	x++; break;
				case UP: 	y--; break;
				case LEFT:	x--; break;
				case DOWN:	y++; break;
			}
			j++;
		}
		return spiral;
	}
	
	public static boolean isPrime(int a){
		   if(a == 2) return true;
		   if(a <= 1 || a % 2 == 0) return false;
		   long max = (long)Math.sqrt(a);
		   for(long n = 3; n <= max; n += 2){
		      if(a % n == 0) return false;
		   }
		   return true;
		}
	
	public static void main(String[] args){
		String[][] ulam = genUlam(9);
		for(String[] row : ulam){
			System.out.println(Arrays.toString(row).replaceAll(",", ""));
		}
		System.out.println();
		
		for(String[] row : ulam){
			System.out.println(Arrays.toString(row).replaceAll("\\[\\s+\\d+", "[  * ").replaceAll("\\s+\\d+", "   * ").replaceAll(",", ""));
		}
	}
}
```

{{out}}

```txt
[ ---  ---  ---  ---   61  ---   59  ---  ---]
[ ---   37  ---  ---  ---  ---  ---   31  ---]
[  67  ---   17  ---  ---  ---   13  ---  ---]
[ ---  ---  ---    5  ---    3  ---   29  ---]
[ ---  ---   19  ---  ---    2   11  ---   53]
[ ---   41  ---    7  ---  ---  ---  ---  ---]
[  71  ---  ---  ---   23  ---  ---  ---  ---]
[ ---   43  ---  ---  ---   47  ---  ---  ---]
[  73  ---  ---  ---  ---  ---   79  ---  ---]

[ ---  ---  ---  ---   *   ---   *   ---  ---]
[ ---   *   ---  ---  ---  ---  ---   *   ---]
[  *   ---   *   ---  ---  ---   *   ---  ---]
[ ---  ---  ---   *   ---   *   ---   *   ---]
[ ---  ---   *   ---  ---   *    *   ---   * ]
[ ---   *   ---   *   ---  ---  ---  ---  ---]
[  *   ---  ---  ---   *   ---  ---  ---  ---]
[ ---   *   ---  ---  ---   *   ---  ---  ---]
[  *   ---  ---  ---  ---  ---   *   ---  ---]
```



###  Large scale Ulam Spiral 

[[File:ulam_large_java.gif|300px|thumb|right]]
{{works with|Java|8}}

```java
import java.awt.*;
import javax.swing.*;

public class LargeUlamSpiral extends JPanel {

    public LargeUlamSpiral() {
        setPreferredSize(new Dimension(605, 605));
        setBackground(Color.white);
    }

    private boolean isPrime(int n) {
        if (n <= 2 || n % 2 == 0)
            return n == 2;
        for (int i = 3; i * i <= n; i += 2)
            if (n % i == 0)
                return false;
        return true;
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        g.setColor(getForeground());

        double angle = 0.0;
        int x = 300, y = 300, dx = 1, dy = 0;

        for (int i = 1, step = 1, turn = 1; i < 40_000; i++) {

            if (isPrime(i))
                g.fillRect(x, y, 2, 2);

            x += dx * 3;
            y += dy * 3;

            if (i == turn) {

                angle += 90.0;

                if ((dx == 0 && dy == -1) || (dx == 0 && dy == 1))
                    step++;

                turn += step;

                dx = (int) Math.cos(Math.toRadians(angle));
                dy = (int) Math.sin(Math.toRadians(-angle));
            }
        }
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Large Ulam Spiral");
            f.setResizable(false);
            f.add(new LargeUlamSpiral(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



###  Small scale Ulam Spiral 

[[File:ulam_spiral_java.gif|300px|thumb|right]]
{{works with|Java|8}}

```java
import java.awt.*;
import javax.swing.*;

public class UlamSpiral extends JPanel {

    Font primeFont = new Font("Arial", Font.BOLD, 20);
    Font compositeFont = new Font("Arial", Font.PLAIN, 16);

    public UlamSpiral() {
        setPreferredSize(new Dimension(640, 640));
        setBackground(Color.white);
    }

    private boolean isPrime(int n) {
        if (n <= 2 || n % 2 == 0)
            return n == 2;
        for (int i = 3; i * i <= n; i += 2)
            if (n % i == 0)
                return false;
        return true;
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        g.setStroke(new BasicStroke(2));

        double angle = 0.0;
        int x = 280, y = 330, dx = 1, dy = 0;

        g.setColor(getForeground());
        g.drawLine(x, y - 5, x + 50, y - 5);

        for (int i = 1, step = 1, turn = 1; i < 100; i++) {

            g.setColor(getBackground());
            g.fillRect(x - 5, y - 20, 30, 30);
            g.setColor(getForeground());
            g.setFont(isPrime(i) ? primeFont : compositeFont);
            g.drawString(String.valueOf(i), x + (i < 10 ? 4 : 0), y);

            x += dx * 50;
            y += dy * 50;

            if (i == turn) {
                angle += 90.0;

                if ((dx == 0 && dy == -1) || (dx == 0 && dy == 1))
                    step++;

                turn += step;

                dx = (int) Math.cos(Math.toRadians(angle));
                dy = (int) Math.sin(Math.toRadians(-angle));

                g.translate(9, -5);
                g.drawLine(x, y, x + dx * step * 50, y + dy * step * 50);
                g.translate(-9, 5);
            }
        }
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Ulam Spiral");
            f.setResizable(false);
            f.add(new UlamSpiral(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



## JavaScript

You can find plotting helper functions here on RosettaCode Wiki: [[User:AnatolV/Helper_Functions| VOE.js]] v.2.0.

'''Note:'''
* Find "printed" spirals in console (Chrome).

* An image uploading is still blocked. But you have a browser!? So, copy/paste/save this page and double click it.

{{trans|PARI/GP}}
{{Works with|Chrome}} (or any other browser supporting Canvas tag)

```html

<!-- UlamSpiral.html -->
<html>
<head><title>Ulam Spiral</title>
    <script src="VOE.js"></script>
<script>
// http://rosettacode.org/wiki/User:AnatolV/Helper_Functions
// Use v.2.0
var pst;

// ***** Additional helper functions
// Pad number from left
function padLeft(n,ns) {
  return ("     " + n).slice(-ns);
}

// Is number n a prime?
function isPrime(n) {
  var n2=Math.sqrt(n);
  for(var i=2; i<=n2; i++) {
    if(n%i === 0) return false;
  }//fend i
  return n !== 1;
}

function insm(mat,x,y) {
  var xz=mat[0].length, yz=xz;
  return(x>=0 && x<xz && y>=0 && y<yz)
}
// *****

function rbCheck() {
  if (document.getElementById('rbDef').checked) {pst=0}
  if (document.getElementById('rbAst').checked) {pst=1}
  if (document.getElementById('rbNum').checked) {pst=2}
}
function rbSet() {
  document.getElementById("rbDef").checked = true;
  rbCheck();
}

// The Ulam Spiral
function pspUlam() {
  var i, j, x, y, xmx, ymx, cnt, dir, M, Mij, sp=" ", sc=3;
  // Setting basic vars for canvas and matrix
  var cvs = document.getElementById('cvsId');
  var ctx = cvs.getContext("2d");
  if(pst<0||pst>2) {pst=0}
  if(pst==0) {n=100; sc=3} else {n=10; sc=5}
  console.log("sc", typeof(sc));
  if(n%2==0) {n++};
  var n2=n*n, pch, sz=n2.toString().length, pch2=sp.repeat(sz);
  var fgc="navy", bgc="white";
  // Create matrix, finding number of rows and columns
  var M=new Array(n);
  for (i=0; i<n; i++) { M[i]=new Array(n);
    for (j=0; j<n; j++) {M[i][j]=0} }
  var r = M[0].length, c = M.length, k=0, dsz=1;
  // Logging init parameters
  var ttl="Matrix ("+r+","+c+")";
  console.log(" *** Ulam spiral: ",n,"x",n,"p-flag=",pst, "sc", sc);
  // Generating and plotting Ulam spiral
  x=y=Math.floor(n/2)+1; xmx=ymx=cnt=1; dir="R";
  for(var i=1; i<=n2; i++) {  //
    if(isPrime(i))  // if prime
      { if(!insm(M,x,y)) {break};
        if(pst==2) {M[y][x]=i} else {M[y][x]=1};
      }
    // all numbers
    if(dir=="R") {if(xmx>0){x++;xmx--} else {dir="U";ymx=cnt;y--;ymx--} continue};
    if(dir=="U") {if(ymx>0){y--;ymx--} else {dir="L";cnt++;xmx=cnt;x--;xmx--} continue};
    if(dir=="L") {if(xmx>0){x--;xmx--} else {dir="D";ymx=cnt;y++;ymx--} continue};
    if(dir=="D") {if(ymx>0){y++;ymx--} else {dir="R";cnt++;xmx=cnt;x++;xmx--}; continue};
  }//fend i
  //Plot/Print according to the p-flag(0-real plot,1-"*",2-primes)
  if(pst==0) {pmat01(M, fgc, bgc, sc, 0); return};
  var logs;
  if(pst==1) {for(i=1;i<n;i++) {logs="|";
                 for(j=1;j<n;j++) { Mij=M[i][j]; if(Mij>0) {pch="*"} else {pch=" "};
                   logs+=" "+pch;}
               logs+="|"; console.log(logs);}//fiend
              pmat01(M, fgc, bgc, sc, 0); console.log("sc", sc);
              return;
              }//ifend
                   //console.log(" ",pch);} console.log(" ")}; return};
  if(pst==2) {for(i=1;i<n;i++) {logs="|";
                 for(j=1;j<n;j++) {Mij=M[i][j];
                   if(Mij==0) {pch=pch2}
                   else {pch=padLeft(Mij,sz)};
                   logs+=pch; }  //" "+
               logs+=" |"; console.log(logs);}//fiend
              pmat01(M, fgc, bgc, sc, 0); console.log("sc", sc);
			  return;
			  }//ifend

}//func end
// ******************************************
</script></head>
<body onload='rbSet();' style="font-family: arial, helvatica, sans-serif;">
  <b>Plot/print style:</b>
  <input type="radio" onclick="rbCheck();" name="rb" id="rbDef"/><b>Plot</b> 
  <input type="radio" onclick="rbCheck();" name="rb" id="rbAst"/><b>Print *</b> 
  <input type="radio" onclick="rbCheck();" name="rb" id="rbNum"/><b>Print numbers</b> 
  <input type="button" value="Plot it!" onclick="pspUlam();">
  <h3>Ulam Spiral</h3>
  <canvas id="cvsId" width="300" height="300" style="border: 2px inset;"></canvas>
</body>
</html>

```
 
{{Output}}


```txt

Print using "*"

### =========

Plot, plus this:
 *** Ulam spiral: 11 x 11 p-flag= 1 sc 5
|                    |
|           *   *    |
|     *           *  |
|   *   *       *    |
|         *   *   *  |
|       *     * *   *|
|     *   *          |
|   *       *        |
|     *       *      |
|   *           *    |

 matrix 'signature': Matrix(11x11) 22 dots 
 
Print prime numbers

### =============

Plot, plus this:
 *** Ulam spiral: 11 x 11 p-flag= 2 sc 5
|                               |
|                61    59       |
|       37                31    |
|    67    17          13       |
|              5     3    29    |
|          19        2 11    53 |
|       41     7                |
|    71          23             |
|       43          47          |
|    73                79       |
 
plot:
=====
Plot, plus this:
 *** Ulam spiral: 101 x 101 p-flag= 0 sc 3
 matrix 'signature': Matrix(101x101) 1208 dots

```



## Julia

{{works with|Julia|0.6}}


```julia
using Primes

function ulamspiral(ord::Int)
    # Possible directions
    dirs = [[0, 1], [-1, 0], [0, -1], [1, 0]]
    # fdir = ["→", "↑", "←", "↓"] # for debug pourpose
    cur = maxsteps = 1  # starting direction & starting max steps
    steps = n = 0       # starting steps     & starting number in cell
    pos = [ord ÷ 2 + 1, isodd(ord) ? ord ÷ 2 + 1 : ord ÷ 2] # starting position
    M = Matrix{Bool}(ord, ord) # result matrix
    while n < ord ^ 2  # main loop (stop when the matrix is filled)
        n += 1
        M[pos[1], pos[2]] = isprime(n)
        steps += 1
        # Debug print
        # @printf("M[%i, %i] = %5s (%2i), step %i/%i, nxt %s\n", pos[1], pos[2], isprime(n), n, steps, maxsteps, fdir[cur])
        pos  .+= dirs[cur] # increment position
        if steps == maxsteps # if reached max number of steps in that direction...
            steps = 0        # ...reset steps
            if iseven(cur) maxsteps += 1 end # if the current direction is even increase the number of steps
            cur  += 1        # change direction
            if cur > 4 cur -= 4 end # correct overflow
        end
    end
    return M
end

mprint(m::Matrix) = for i in 1:size(m, 1) println(join(el ? " ∙ " : "   " for el in m[i, :]), '\n') end

M = ulamspiral(9)
mprint(M)
```


{{out}}

```txt
             ∙     ∙       

    ∙                 ∙    

 ∙     ∙           ∙       

          ∙     ∙     ∙    

       ∙        ∙  ∙     ∙ 

    ∙     ∙                

 ∙           ∙             

    ∙           ∙          

 ∙                 ∙       
```



## Kotlin

{{trans|Java}}

```scala
object Ulam {
    fun generate(n: Int, i: Int = 1, c: Char = '*') {
        require(n > 1)
        val s = Array(n) { Array(n, { "" }) }
        var dir = Direction.RIGHT
        var y = n / 2
        var x = if (n % 2 == 0) y - 1 else y // shift left for even n's
        for (j in i..n * n - 1 + i) {
            s[y][x] = if (isPrime(j)) if (c.isDigit()) "%4d".format(j) else "  $c " else " ---"

            when (dir) {
                Direction.RIGHT -> if (x <= n - 1 && s[y - 1][x].none() && j > i) dir = Direction.UP
                Direction.UP -> if (s[y][x - 1].none()) dir = Direction.LEFT
                Direction.LEFT -> if (x == 0 || s[y + 1][x].none()) dir = Direction.DOWN
                Direction.DOWN -> if (s[y][x + 1].none()) dir = Direction.RIGHT
            }

            when (dir) {
                Direction.RIGHT -> x++
                Direction.UP -> y--
                Direction.LEFT -> x--
                Direction.DOWN -> y++
            }
        }
        for (row in s) println("[" + row.joinToString("") + ']')
        println()
    }

    private enum class Direction { RIGHT, UP, LEFT, DOWN }

    private fun isPrime(a: Int): Boolean {
        when {
            a == 2 -> return true
            a <= 1 || a % 2 == 0 -> return false
            else -> {
                val max = Math.sqrt(a.toDouble()).toInt()
                for (n in 3..max step 2)
                    if (a % n == 0) return false
                return true
            }
        }
    }
}

fun main(args: Array<String>) {
    Ulam.generate(9, c = '0')
    Ulam.generate(9)
}
```



## PARI/GP

In this version function plotulamspir() was translated from VB, plus upgraded to plot/print different kind of Ulam spirals.
My own plotting helper functions and string functions were used and made it possible.
You can find all of them here on RosettaCode Wiki.

[[File:ULAMspiral1.png|200px|right|thumb|Output ULAMspiral1.png]]
[[File:ULAMspiral2.png|200px|right|thumb|Output ULAMspiral2.png]]

{{Works with|PARI/GP|2.7.4 and above}}


```parigp

\\ Ulam spiral (plotting/printing)
\\ 4/19/16 aev
plotulamspir(n,pflg=0)={
my(n=if(n%2==0,n++,n),M=matrix(n,n),x,y,xmx,ymx,cnt,dir,n2=n*n,pch,sz=#Str(n2),pch2=srepeat(" ",sz));
if(pflg<0||pflg>2,pflg=0);
print(" *** Ulam spiral: ",n,"x",n," matrix, p-flag=",pflg);
x=y=n\2+1; xmx=ymx=cnt=1; dir="R";
for(i=1,n2,
    if(isprime(i), if(!insm(M,x,y), break); if(pflg==2, M[y,x]=i, M[y,x]=1));
    if(dir=="R", if(xmx>0, x++;xmx--, dir="U";ymx=cnt;y--;ymx--); next); 
    if(dir=="U", if(ymx>0, y--;ymx--, dir="L";cnt++;xmx=cnt;x--;xmx--); next); 
    if(dir=="L", if(xmx>0, x--;xmx--, dir="D";ymx=cnt;y++;ymx--); next); 
    if(dir=="D", if(ymx>0, y++;ymx--, dir="R";cnt++;xmx=cnt;x++;xmx--); next); 
   );\\fend
\\Plot/Print according to the p-flag(0-real plot,1-"*",2-primes)
if(pflg==0, plotmat(M));
if(pflg==1, for(i=1,n, 
            for(j=1,n, if(M[i,j]==1, pch="*", pch=" ");
                       print1(" ",pch)); print(" ")));
if(pflg==2, for(i=1,n, 
            for(j=1,n, if(M[i,j]==0, pch=pch2, pch=spad(Str(M[i,j]),sz,,1));
                       print1(" ",pch)); print(" ")));
}

{\\ Executing:
plotulamspir(9,1); \\ (see output)
plotulamspir(9,2); \\ (see output)
plotulamspir(100); \\ ULAMspiral1.png
plotulamspir(200); \\ ULAMspiral2.png
}

```
 

{{Output}}


```txt

> plotulamspir(9,1);
  *** Ulam spiral: 9x9 matrix, p-flag=1
  
          *   *
    *           *
  *   *       *
        *   *   *
      *     * *   *
    *   *
  *       *
    *       *
  *           *
 
> plotulamspir(9,2);
  *** Ulam spiral: 9x9 matrix, p-flag=2
 
             61    59
    37                31
 67    17          13
           5     3    29
       19        2 11    53
    41     7
 71          23
    43          47
 73                79
 
> plotulamspir(100); \\ ULAMspiral1.png
  *** Ulam spiral: 101x101 matrix, p-flag=0
  *** matrix(101x101) 1252 DOTS

> plotulamspir(200); \\ ULAMspiral2.png
  *** Ulam spiral: 201x201 matrix, p-flag=0
  *** matrix(201x201) 4236 DOTS

```



## Pascal

Rather than produce just splots, why not colour code them? Further, how about coding all the numbers according to the number of their first prime factor? The result looks a bit like a tartan rug. Alas, image files can't be presented, so a no show, but those with access to Turbo Pascal or similar can have a try. Amusingly enough, with black as colour zero reserved for N <= 1 (thus, the normal start square is black) and white as colour one for prime numbers, it becomes marginally convenient to regard two as the second prime... Without grid marking, finding the centre of the spiral is difficult, so showing N where it is a single digit helps. Encoding could be pressed forwards into different symbols, but enough already.

In the first part of the source are some support routines, from way back in the 1980s, written when the mainframe terminals only offered capitals and the habit lingered. They are there only so as to facilitate some gestures towards checking. The remainder is simple enough, and uses complex numbers to follow the spiral, which of course have to be implemented via ad-hoc code as they're not supported by the compiler. The scheme could be recast into the (line,column) form, counting downwards for the screen line, but array(i,j) = (x,y) means less standing upside down when devising the arithmetic for the directions, at the cost of a "downto" loop for output. An even more tricky scheme would be to ascertain N from (line,column) as the lines were written rather than compute the whole spiral first. Such a function exists. 

```Pascal

Program Ulam; Uses crt;
{Concocted by R.N.McLean (whom God preserve), ex Victoria university, NZ.}
{$B- evaluate boolean expressions only so far as necessary.}
{$R+ range checking...}

 FUNCTION Trim(S : string) : string;
  var L1,L2 : integer;
 BEGIN
  L1 := 1;
  WHILE (L1 <= LENGTH(S)) AND (S[L1] = ' ') DO INC(L1);
  L2 := LENGTH(S);
  WHILE (S[L2] = ' ') AND (L2 > L1) DO DEC(L2);
  IF L2 >= L1 THEN Trim := COPY(S,L1,L2 - L1 + 1) ELSE Trim := '';
 END; {Of Trim.}

FUNCTION Ifmt(Digits : integer) : string;
 var  S : string[255];
 BEGIN
  STR(Digits,S);
  Ifmt := Trim(S);
 END; { Ifmt }
 Function min(i,j: integer): integer;
  begin
   if i <= j then min:=i else min:=j;
  end;
 Procedure Croak(Gasp: string);        {A lethal word.}
  Begin
   WriteLn;
   WriteLn(Gasp);
   HALT;                   {This way to the egress...}
  End;
 var ScreenLine,ScreenColumn: byte;	{Line and column position.}
{
### ======================enough support================
}
 const Mstyle = 6;	{Display different results.}
 const StyleName: array[1..Mstyle] of string = ('IsPrime','First Prime Factor Index',
  'First Prime Factor','Number of Prime Factors',
  'Sum of Prime Factors','Sum of Proper Factors');
 const OrderLimit = 49; Limit2 = OrderLimit*OrderLimit;		{A 50-line screen has room for a heading.}
 var Tile: array[1..OrderLimit,1..OrderLimit] of integer; 	{Alas, can't put [Order,Order], only constants.}
 var FirstPrimeFactorIndex,FirstPrimeFactor,NumPFactor,SumPFactor,SumFactor: array[1..Limit2] of integer;
 const enuffP = 17;	{Given the value of Limit2.}
 const Prime: array[1..enuffP] of integer = (1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53);
 Procedure Prepare;	{Various arrays are to be filled for the different styles.}
  var i,j,p: integer;
  Begin
   for i:=1 to limit2 do	{Alas, can't just put A:=0;}
    begin			{Nor clear A;}
     FirstPrimeFactorIndex[i]:=1;	{Prime[1] = 1, so this means no other divisor.}
     FirstPrimeFactor[i]:=0;
     NumPFactor[i]:=0;
     SumPFactor[i]:=0;
     SumFactor[i]:=1;		{1 is counted as a proper factor.}
    end;
   FirstPrimeFactorIndex[1]:=0;	{Fiddle, as 1 is not a prime number.}
   SumFactor[1]:=0;		{N is not a proper factor of N, so 1 has no proper factors...}
   for i:=2 to enuffP do	{Prime[1] = 1, Prime[2] = 2, so start with i = 2.}
    begin
     p:=Prime[i];
     j:=p + p;
     while j <= Limit2 do
      begin
       if FirstPrimeFactorIndex[j] = 1 then FirstPrimeFactorIndex[j]:=i;
       if FirstPrimeFactor[j] = 0 then FirstPrimeFactor[j]:=p;
       SumPFactor[j]:=SumPFactor[j] + p;
       inc(NumPFactor[j]);
       j:=j + p;
      end;
    end;
   for i:=2 to Limit2 div 2 do	{Step through all possible proper factors.}
    begin			{N is not a proper factor of N, so start at 2N,}
     j:=2*i;	 		{for which N is a proper factor of 2N.}
     while j <= Limit2 do	{Sigh. for j:=2*i:Limit2:i do ... Next i;}
      begin
       SumFactor[j]:=SumFactor[j] + i;
       j:=j + i;
      end;
    end;
  End;	{Enough preparation.}

 const enuffC = 11;	{Perhaps the colours will highlight interesting patterns.}
 const colour:array[0..enuffC] of byte = (black,white,LightRed,
  LightMagenta,Yellow,LightGreen,LightCyan,LightBlue,LightGray,
  Red,Green,DarkGray);		{Colours on the screen don't always match their name!}

 Procedure UlamSpiral(Order,Start,Style: integer);	{Generate the numbers, then display.}
  Function Encode(N: integer): integer;	{Acording to Style, choose a result to show.}
   Begin
    if N <= 1 then Encode:=0
     else
      case style of
     1:if FirstPrimeFactorIndex[N] = 1 then Encode:=1 else Encode:=0;	{1 = Prime.}
     2:Encode:=FirstPrimeFactorIndex[N];
     3:Encode:=FirstPrimeFactor[N];
     4:Encode:=NumPFactor[N];
     5:Encode:=SumPFactor[N];
     6:Encode:=SumFactor[N];
      end;
   End;	{So much for encoding.}
  var Place,Way: array[1..2] of integer;	{Complex numbers.}
  var m,	{Middle.}
      N,	{Counter.}
      length,	{length of a side.}
      lunge,	{two lunges for each length.}
      step	{steps to make up a lunge of some length.}
      : integer;
  var i,j: integer;	{Steppers.}
  var code,it: integer;	{Mess with the results.}
  label XX;		{Escape the second lunge.}
  var OutF: text;	{Utter drivel. It is a disc file.}
  Begin
   Write('Ulam Spiral, order ',Order,', start ',Start,', style ',style);	{Start the heading.}
   if style <= 0 then Croak('Must be a positive style');
   if style > Mstyle then croak('Last known style is '+ifmt(Mstyle));
   if Order > OrderLimit then Croak('Array OrderLimit is order '+IFmt(OrderLimit));
   if Order mod 2 <>1 then Croak('The order must be an odd number!');
   writeln(': ',StyleName[Style]);	{Finish the heading. The pattern starts with line two.}
   Assign(OutF,'Ulam.txt'); Rewrite(OutF); Writeln(OutF,'Ulam spiral: the codes for ',StyleName[style]);
   m:=order div 2 + 1;		{This is why Order must be odd.}
   Place[1]:=m; Place[2]:=m;	{Start at the middle.}
   way[1]:=1; way[2]:=0;	{Initial direction is along the x-axis.}
   n:=Start;
   for length:=1 to Order do	{Advance through the lengths.}
    for lunge:=1 to 2 do		{Two lunges for each length.}
     begin
      for step:=1 to length do			{Make the steps.}
       begin
        Tile[Place[1],Place[2]]:=N;
        for i:=1 to 2 do Place[i]:=Place[i] + Way[i];   {Place:=Place + Way;}
        N:=N + 1;
       end;
      if N >= Order*Order then goto XX;	{Each corner piece is part of two lunges.}
      i:=Way[1]; Way[1]:=-Way[2]; Way[2]:=i;	{Way:=Way*(0,1) in complex numbers: (x,y)*(0,1) = (-y,x).}
     end;
XX:for i:=order downto 1 do     {Output: Lines count downwards, y runs upwards.}
    begin			{The first line is the topmost y.}
     for j:=1 to order do	{(line,column) = (y,x).}
      begin				{Work along the line.}
       it:=Tile[j,i];			{Grab the number.}
       code:=Encode(it);		{Presentation scheme.}
       Write(OutF,'(',it:4,':',code:2,')');	{Debugging...}
       if FirstPrimeFactorIndex[it] > 1 then TextBackGround(Black)	{Not a prime.}
        else if it = 1 then TextBackGround(Black)	{Darkness for one, also.}
         else TextBackGround(White);		{A prime number!}
       TextColor(Colour[min(code,enuffC)]);	{A lot of fuss for this!}
       {Write(code:2);}
       {Write(it:3);}
       if it <= 9 then write(it) else Write('*');	{Thus mark the centre.}
      end;					{Next position along the line.}
     if i > 1 then WriteLn;		{Ending the last line would scroll the heading up.}
     WriteLn(OutF);			{But this is good for the text file.}
    end;			{On to the next line.}
    Close(OutF);		{Finished with the trace.}
{Some revelations to help in choosing a colour sequence.}
    ScreenLine:=WhereY; ScreenColumn:=WhereX;	{Gibberish to find the location.}
    if Style > 1 then	{Only the fancier styles go beyond 0 and 1.}
     begin			{So explain only for them.}
      GoToXY(ScreenColumn + 1,ScreenLine - 4);		{Unused space is to the right.}
      TextColor(White); write('Colour sequence');	{Given 80-column displays.}
      GoToXY(ScreenColumn + 1,ScreenLine - 3);		{And no more than 50 lines.}
      for i:=1 to enuffC do begin TextColor(Colour[i]); write(i); end;	{My sequence.}
      GoToXY(ScreenColumn + 1,ScreenLine - 2);
      TextColor(White); write('From options');
      GoToXY(ScreenColumn + 1,ScreenLine - 1);
      for i:=1 to 15 do begin TextColor(i);write(i); end;		{The options.}
     end;
  End;   {of UlamSpiral.}

 var start,wot,order: integer;	{A selector.}
 BEGIN	{After all that.}
  TextMode(Lo(LastMode) + Font8x8);	{Gibberish sets 43 lines on EGA and 50 on VGA.}
  ClrScr; TextColor(White);		{This also gives character blocks that are almost square...}
  WriteLn('Presents consecutive integers in a spiral, as per Stanislaw Ulam.');
  WriteLn('Starting with 1, runs up to Order*Order.');
  Write('What value for Order? (Limit ' + Ifmt(OrderLimit),'): ');
  ReadLn(Order);			{ReadKey needs no "enter", but requires decoding.}
  if (order < 1) or (order > OrderLimit) then Croak('Out of range!');	{Oh dear.}
  Prepare;
  wot:=1;	{The original task.}
  Repeat		{Until bored?}
   ClrScr;			{Scrub any previous stuff.}
   UlamSpiral(Order,1,wot);		{The deed!}
   GoToXY(ScreenColumn + 1,ScreenLine);		{Note that the last WriteLn was skipped.}
   TextColor(White); Write('Enter 0, or 1 to '+Ifmt(Mstyle),': ');	{Wot now?}
   ReadLn(wot);						{Receive.}
  Until (wot <= 0) or (wot > Mstyle);		{Alas, "Enter" must be pressed.}
 END.

```



## Perl

{{trans|python}}{{libheader|ntheory}}

```perl
use ntheory qw/is_prime/;
use Imager;

my $n = shift || 512;
my $start = shift || 1;
my $file = "ulam.png";

sub cell {
  my($n, $x, $y, $start) = @_;
  $y -= $n>>1;
  $x -= ($n-1)>>1;
  my $l = 2*(abs($x) > abs($y) ? abs($x) : abs($y));
  my $d = ($y > $x)  ?  $l*3 + $x + $y  : $l-$x-$y;
  ($l-1)**2 + $d + $start - 1;
}

my $black = Imager::Color->new('#000000');
my $white = Imager::Color->new('#FFFFFF');
my $img = Imager->new(xsize => $n, ysize => $n, channels => 1);
$img->box(filled=>1, color=>$white);

for my $y (0 .. $n-1) {
  for my $x (0 .. $n-1) {
    my $v = cell($n, $x, $y, $start);
    $img->setpixel(x => $x, y => $y, color => $black) if is_prime($v);
  }
}

$img->write(file => $file) or die "Cannot write $file: ", $img->errstr, "\n";
```

{{out}}
Creates an image file <tt>ulam.png</tt> in current directory similar to the one on MathWorld.  The square dimension can be optionally specified.


## Perl 6


```perl6
sub MAIN($max = 160, $start = 1) {
    (my %world){0}{0} = 0;
    my $loc = 0+0i;
    my $dir = 1;
    my $n = $start;
    my $side = 0;

    while ++$side < $max {
	step for ^$side;
	turn-left;
	step for ^$side;
	turn-left;
    }

    braille-graphics %world;

    sub step {
	$loc += $dir;
	%world{$loc.im}{$loc.re} = $n if (++$n).is-prime;
    }

    sub turn-left  { $dir *= -i; }
    sub turn-right { $dir *= i; }

}

sub braille-graphics (%a) {
    my ($ylo, $yhi, $xlo, $xhi);
    for %a.keys -> $y {
	$ylo min= +$y; $yhi max= +$y;
	for %a{$y}.keys -> $x {
	    $xlo min= +$x; $xhi max= +$x;
	}
    }

    for $ylo, $ylo + 4 ...^ * > $yhi -> \y {
	for $xlo, $xlo + 2 ...^ * > $xhi -> \x {
	    my $cell = 0x2800;
	    $cell += 1   if %a{y + 0}{x + 0};
	    $cell += 2   if %a{y + 1}{x + 0};
	    $cell += 4   if %a{y + 2}{x + 0};
	    $cell += 8   if %a{y + 0}{x + 1};
	    $cell += 16  if %a{y + 1}{x + 1};
	    $cell += 32  if %a{y + 2}{x + 1};
	    $cell += 64  if %a{y + 3}{x + 0};
	    $cell += 128 if %a{y + 3}{x + 1};
	    print chr($cell);
	}
	print "\n";
    }
}
```

{{out}}

```txt
⠔⠀⠀⠀⢐⠀⠁⠀⠀⠀⢐⠁⠀⢀⠀⠄⠄⠀⢀⠀⠀⠅⢀⠁⢅⢄⠀⢀⠔⠁⠀⠀⠀⢀⢀⠀⠀⠀⠁⢀⢀⠀⠀⢔⠁⢔⠄⠀⢄⠐⠀⠀⢀⠁⠐⠄⠀⢑⠄⠁⠄⠀⠁⠄⠀⠀⠀⢐⠀⠄⠐⠀⢁⢀⠀⠀⠄⠀⢕⠐
⠄⠁⠁⠄⠀⠄⢀⠀⠐⠀⠀⠁⢁⢀⠐⠀⠀⠀⢁⠐⠄⠀⠀⠔⠀⢐⠁⠄⠀⠑⠀⢀⠁⠀⠐⠐⠁⠀⠄⠀⢀⠀⠀⠀⠐⠀⠐⠀⠅⠀⠀⢄⢀⠐⠁⠐⠄⠁⢀⠀⠀⠐⠁⠀⠀⠄⢄⠀⠕⠁⠀⠐⢄⠀⠀⢀⠐⠄⠁⠀
⠀⠀⢀⠁⢀⠀⠑⢀⠀⠄⠀⠀⠅⢑⠀⠁⠐⠀⠀⠐⠀⠄⠁⢄⠀⢑⠀⠄⢑⠀⠁⠀⢀⠑⢐⠀⠁⢀⠄⠐⢀⠄⠁⠀⠀⠀⠀⢀⠄⠀⠀⠑⠀⢀⠔⢁⠀⠀⠀⠀⠐⠁⠀⠑⢀⠀⠐⠀⢄⠔⢐⠄⠅⠐⠀⠅⢁⠀⠁⠁
⠅⢀⠄⠑⠀⠀⠅⠄⠀⢐⠐⠀⠀⠄⠀⢁⠄⠀⢔⢀⠁⠀⠔⠁⠑⢐⠀⠐⠀⠁⢑⠀⠀⢁⠀⠀⠄⠀⠀⠑⢀⠀⠄⠔⠁⠀⠄⠀⠐⢀⠀⠀⠔⠁⠀⠐⠀⠀⠐⠁⠐⠀⠀⢀⠄⠁⢁⠀⠀⠐⠀⠁⢐⠀⠐⠀⠄⠑⠐⠄
⠀⠀⠀⢐⠑⠑⢀⠁⠑⢀⠐⠀⠄⠀⠀⢀⢐⠕⠄⠀⠀⠀⠐⠀⠀⢀⠄⠀⠀⠀⠄⠐⠐⠀⠀⠁⢄⠐⠅⢀⠐⠄⠁⠐⠀⠅⢀⠐⢁⢀⠀⠁⠐⠕⠀⠀⠀⠀⢐⠀⠅⠐⠔⢁⠀⠐⠅⠐⠄⢀⠀⢄⠀⢀⠄⠀⠀⢀⠁⠐
⢀⠀⠀⢀⠁⠀⠀⠀⢁⠁⠐⠀⠁⢐⢀⠀⠁⠀⠔⠁⢄⠁⠁⠄⠀⢀⠄⠀⢐⠀⠔⠁⢀⠕⠁⢀⠁⠀⢀⠔⢐⠀⠀⠁⢀⠀⠀⢀⠔⠀⠄⠄⠁⠀⠄⢐⠄⠁⢁⠀⠄⠀⠀⠄⠀⢄⠀⢀⢀⠁⠄⢀⠄⠀⢀⠁⢁⠀⠀⠀
⠁⠄⠀⠀⠄⠀⠄⠀⠄⠑⢄⠔⠁⠀⠁⠀⠐⠀⠀⠀⠀⠀⢀⠀⠀⠄⠁⢁⠐⠀⠀⢀⠀⠄⠐⠀⢐⠀⠁⠀⠀⠁⠁⠄⠁⢄⠔⠀⠐⠀⠀⠔⢄⠀⠀⢐⠀⢅⠀⠁⢀⠀⠀⠀⠀⠀⠄⠀⠐⠀⠀⠀⠄⠀⠀⠀⠐⠀⠄⠄
⠀⠑⠐⠔⠀⢀⢀⠀⢁⠔⠁⢁⠄⠁⠀⠀⠀⠅⠀⢀⠀⠁⢀⠀⢄⠀⠀⠀⠐⠀⢀⠐⠁⢀⠀⠀⠀⢀⠁⠀⠐⠀⠄⢀⠔⢁⠀⠀⢅⠐⠑⠅⠀⠐⠀⢀⠁⢄⠀⠀⢁⠐⠄⠀⠐⢕⢀⠁⢀⠁⠑⠅⠀⢁⠐⠀⠁⠀⢁⢐
⢀⠁⠄⠀⠀⢄⠁⠄⠀⠀⠐⠅⠁⠁⢀⢀⢅⠄⠐⢁⠀⠄⠀⠄⠅⠑⢀⠔⢀⠀⠀⢁⠀⠕⠐⠀⠀⠐⢄⠐⢀⠄⠀⠁⢀⠁⢀⠀⠄⢀⠀⠀⠀⢀⠑⢐⠀⠀⠁⠄⠐⠁⠄⠄⠀⠀⠐⢄⢀⠀⠄⠄⠀⠀⠀⠄⢄⢀⠄⠀
⠐⠄⠐⠁⠀⠔⠑⠀⠀⠐⠀⠀⠑⠀⠐⠄⠀⠀⠐⠀⠁⢀⠄⠀⠀⢐⠀⠔⠀⠀⢄⠄⠁⢐⠀⠀⢄⠔⠀⢀⢐⠁⠅⢀⠀⠄⠐⠀⢐⠀⠁⢕⠄⠀⠀⢔⠁⠀⢐⠀⢀⠄⠑⠄⠀⠀⠄⠀⠀⠄⠐⠁⠄⠀⠐⠔⠄⠀⠀⠀
⢀⠐⢀⢐⠀⠀⠄⠑⠑⠐⠐⠀⠔⠑⠀⠐⠀⢀⠀⠀⢀⠄⠀⠁⢄⠀⠁⠀⢀⠁⢀⠀⠀⠀⠐⠄⠁⢐⠔⠅⢐⠅⠀⠀⠅⠀⠀⠀⠁⠀⠔⠄⠀⠔⢀⠀⠄⢁⠐⢁⢀⢁⢀⢀⢁⠄⠁⢀⠀⠀⢁⢑⠀⢀⢐⠁⠀⠀⠀⠁
⠀⠄⠅⠀⠁⠅⠁⢁⠄⠀⠀⢀⠀⢄⠀⠁⠁⢁⠀⠄⢅⢁⠄⠄⠐⢀⢀⠄⠁⠀⠀⠀⢄⠔⠁⢀⠐⠁⠄⠐⠀⠀⠁⢐⠀⠀⢀⢀⠀⠀⠀⠐⠀⠄⠐⠀⠀⠁⠄⠀⠁⠀⠀⠁⢄⠀⠁⢅⠀⠄⠁⠀⠁⠅⢀⠀⠀⠀⠐⠁
⠁⠀⠀⠀⠀⠀⠀⢄⠀⠐⢀⠀⠐⠄⠄⠁⠀⠔⠔⠄⠀⠁⠀⠐⠄⢀⠀⠄⠔⠀⠁⠀⠀⢐⢔⠁⠅⠀⠀⢐⠀⠀⠄⠄⠀⠑⠔⠀⠅⢀⠁⠔⢀⠀⢔⠀⠁⠐⠐⠐⢐⠔⠀⠐⠀⠐⠀⠀⠐⢐⠄⠐⠐⠀⠑⢐⠐⠑⠐⠐
⢀⠁⠁⠀⠀⢑⢀⠀⠑⢀⠀⢀⠀⠀⠁⢔⠀⠀⠀⠀⢀⢀⠁⠁⢄⠀⢁⠀⠀⠐⠁⢀⠐⢁⠐⠐⢁⠐⠀⢄⠐⠅⠀⢐⠄⢁⢀⠐⠄⠐⠐⠀⠀⠔⠀⠑⢔⢀⠑⠀⠀⠐⠑⠀⠀⠐⢀⢀⠑⠀⠐⢀⠀⠁⠐⠁⠀⢁⠀⠀
⠀⠔⠀⢀⠀⠀⢀⠑⠄⠁⠄⠄⢀⠐⠁⠁⠅⠁⢁⢀⠁⠀⢐⠀⠁⠅⢀⢁⠀⢀⠑⢑⠄⠀⠀⠄⠀⠁⢄⠀⢐⠄⠁⠑⢀⠅⢑⠀⠀⠀⠄⠄⢄⠄⠕⢀⠀⠔⢄⢀⠀⠀⠀⠁⢄⠄⠀⠁⠀⠐⠄⠀⠁⢀⠄⠔⠁⠀⠁⠄
⠀⠀⠐⠀⠄⠀⠀⢀⠄⠄⢄⠔⠐⢄⠀⠀⠄⠀⠀⠀⠀⠀⢄⠔⠐⠀⠔⠐⠀⠀⠔⠀⠀⢕⢔⠀⠁⢐⠁⢑⠐⠁⠀⠀⠀⢄⠀⠀⢅⠐⠑⠐⠀⠀⢐⠔⠀⠄⠀⠑⢔⠀⠀⠔⠀⠁⢀⠄⠀⠀⠀⠀⠀⠐⠀⠀⠐⠀⠐⠀
⠄⠐⠀⠄⠀⢀⠐⠀⢀⢐⠀⢀⠄⠀⢐⠀⠀⠀⠀⠐⠐⢄⠀⠀⠔⠀⢀⠀⠀⢐⢔⠀⢀⢄⠀⠄⢀⢀⠐⠁⢐⠅⠅⢀⠑⠄⠐⠐⠀⢀⠀⠁⠁⠑⠀⢁⠅⠀⠁⠄⠀⠁⠀⠀⠁⠁⠁⠐⠑⠁⠁⠀⢀⠐⢀⢀⠀⠁⠀⢀
⢁⢀⠄⠁⠐⠀⢁⠀⠀⠀⠀⠁⠀⢑⠀⢁⠐⠀⠀⠀⢁⠁⢐⠀⢀⠀⠀⠀⢕⢄⢁⢑⠄⠀⠔⢀⠐⠁⢀⠄⠐⠄⠄⢑⢄⠅⢄⠀⠁⢁⢀⠐⠁⢀⠀⠄⠀⠐⠀⠀⠀⢀⠀⠁⢄⠀⠄⠀⢀⠅⢁⠀⠀⢀⠀⠅⠄⠀⠑⠀
⠁⠄⠄⠁⠄⠀⠀⠀⠀⠁⠀⠄⠁⠀⠀⠄⠀⠀⠁⠄⠀⠁⠄⠀⠀⠄⠄⠄⠀⠀⠄⠀⠄⠁⠄⠀⠅⠄⠁⢕⢐⠀⢀⠔⠀⠔⠄⠑⠐⠀⠑⢔⠐⠁⠐⠐⠀⢄⠐⠐⠄⠀⠀⠀⠔⠑⠄⠄⠀⠀⠀⠀⢀⠄⠀⠀⠀⠁⢐⠔
⠐⠀⠑⢐⠁⠀⢐⠑⢁⢀⠐⢀⢐⠑⠁⠀⠀⢀⠀⠁⠑⠀⠑⢐⢀⠑⢐⠀⠁⢑⠀⠑⠐⠐⠐⠑⢐⠑⢑⠄⣔⢅⢁⢁⠐⢀⢁⢅⠁⠐⢐⠀⠀⠄⢀⢐⢅⢁⠀⠀⠀⠀⠐⠁⢁⠔⢀⢑⠁⢁⠐⢀⠀⠀⢄⢀⢁⢅⢀⠀
⢀⠀⠀⠁⠀⠀⠀⢐⢀⠁⠀⠀⠁⠀⠅⠀⠔⢁⢀⠀⠀⠀⢁⢁⠁⢐⠀⢀⠅⠅⢀⠀⠄⢁⠄⢄⠀⢕⠅⠑⢄⠀⠄⢄⢀⠄⢀⠄⠄⠀⢄⠀⠄⢄⠀⠀⢀⠄⢀⢀⠄⠀⠄⠄⠀⢀⠀⢄⠄⠀⢄⢀⠄⢀⢀⠄⠀⠄⠀⢀
⠐⢄⠀⠄⠀⠄⠐⠀⠀⠁⢀⠀⠅⠀⠄⠄⠀⠀⠀⢀⠔⠕⠄⠄⠑⠀⠀⠁⠀⠐⠔⢄⠀⠀⢄⠐⠔⢀⠁⢐⢀⠐⠄⠐⠁⠀⠄⠐⠐⠄⠁⠀⠐⠀⠄⠀⠑⠄⠐⠀⠀⠐⠀⠔⠀⠁⠔⠀⠁⠀⠀⠑⠄⠔⠀⠀⠀⠑⠐⠀
⠀⠀⢀⠄⠐⠀⠀⠀⢀⠄⠀⢑⢀⠁⠐⢀⠑⠀⠔⠐⢁⢐⠀⠐⢀⠀⠑⢐⠀⠀⢀⠐⢐⢔⠐⠄⢀⠁⠁⢐⠑⢄⢄⠑⠀⠁⠐⠐⢁⠐⠑⢁⠑⢀⢀⢀⠐⠁⢐⢐⠀⠐⠐⠀⢀⠑⢁⠁⠀⠀⠀⠑⠀⠁⢐⢀⠑⠀⢀⢀
⠁⢀⠀⠀⢁⠀⠀⠅⠀⠀⠅⠀⠀⠐⢀⢀⢀⠄⠀⠅⠀⢁⠔⠀⠁⢁⠁⠁⠑⢄⠀⠁⠁⢀⢀⠄⠑⢔⠀⠐⠔⠁⠀⠄⠀⠑⢀⠀⠕⠄⠀⠀⠄⢀⠀⠁⢄⠁⢄⠄⠐⠄⢄⠔⠀⠀⠐⠁⢀⠐⠀⠀⠅⠀⠀⠔⠀⢀⠁⠀
⠀⢄⠐⠁⠀⠐⠕⠀⠄⠐⠀⠐⠀⢀⠐⠐⠀⠀⠅⠀⠄⠁⢀⠄⠀⠄⠔⠑⢀⠀⠅⠀⠅⠄⠀⠄⠁⢀⠄⠀⢀⠀⢁⢀⠕⠀⠀⠑⠀⠐⠀⢄⠐⠁⢄⠄⠁⠐⠄⠐⢀⠀⠁⠄⠔⠀⢄⠀⠁⠄⠔⠁⢐⠄⠐⠀⠀⠑⢀⠔
⢀⠐⢀⠀⠀⠐⠄⠁⠐⠄⠐⠀⢀⠑⠁⠄⠀⠁⠐⠀⢀⠀⠁⢀⠀⠐⠐⠔⠁⢀⠀⠑⠀⠀⠐⠀⢔⠑⠀⢐⠀⢁⢀⠐⠀⢀⠐⢀⠄⠁⢄⠁⠀⠀⠁⠀⠀⠀⢐⢀⢁⢑⠀⢀⠑⠐⠁⠀⢀⢀⠀⠁⢀⢀⠀⠀⠐⢀⠀⠐
⢀⢁⠀⠀⢑⢅⢁⢄⢀⢀⠅⠀⠀⠄⢀⢁⢐⢁⢀⢅⢄⠀⠀⢀⢀⠐⠅⠁⠀⠀⢁⠐⠀⢀⠀⠀⠐⢀⠀⢀⠄⠅⠐⢐⠀⠀⠔⠁⠑⠐⠄⠀⠄⠀⠅⠁⠄⠐⠀⠀⠁⠁⠀⠄⠅⠀⠀⠄⠄⠅⠀⠄⠀⠄⠀⠁⠀⠀⠔⠀
⠁⠀⠀⠁⠀⠀⠀⢀⠄⠀⠀⠀⠀⠄⠄⠀⢀⠀⠐⢀⠀⠀⢄⠔⠄⠀⠄⠀⠀⠔⠕⢀⠔⠀⠀⠄⠄⠀⠁⠐⠀⠀⠕⢀⠕⠁⠀⠁⢑⠀⠀⢀⢀⠐⠄⠀⠑⠀⠀⠁⢄⠔⠑⢐⠀⠑⢄⠐⠀⠀⠐⠀⠀⠀⠀⠀⠄⠁⢀⠄
⠀⠁⠐⠀⠁⠀⠄⠀⠑⠀⠀⢀⠀⠐⠀⠄⠀⠑⠀⠀⢐⠀⠁⢅⠀⠀⠀⠐⠀⢀⠄⠀⢁⢄⠀⠁⠔⠑⠄⠀⠑⢄⠔⠁⢀⠀⠀⢄⠄⠀⠀⠐⠁⠁⢀⠐⠄⠀⠁⠐⠁⢀⢑⠁⠐⠐⢀⠑⠔⠀⠁⠀⢁⢀⠄⠁⢐⠀⠀⠀
⠀⠅⠁⢀⠐⠀⠁⠐⠀⠁⠐⢄⠀⢄⢀⠀⠄⢀⠁⢁⠄⠀⠐⠀⢑⠄⠀⠑⠐⠀⠁⠀⠁⠐⠐⠀⠁⠄⠄⢀⠄⠁⠐⢀⠄⠐⠀⠄⠀⠄⠀⠀⢀⠄⢀⢐⠀⠑⠀⠀⠄⢄⢄⠀⢅⠀⠀⠄⠀⠅⠀⠄⠐⢀⠀⠔⠅⠀⠄⢁
⠄⠀⠄⠄⠀⠄⠀⢀⠀⠐⠀⠀⠁⠀⠔⠄⠀⠀⠁⠀⠄⢄⠀⠐⠀⠀⠄⢑⠀⠁⠅⠀⠁⠀⢀⠄⠑⢀⠄⢀⢀⠔⢀⠀⠑⠄⠀⠅⠀⠀⠅⢕⠀⠐⢐⠀⠄⠕⠀⠀⢀⠀⠀⢀⠀⠁⠀⠀⠐⢀⠐⠁⢀⠔⠁⢐⠀⠀⠐⠀
⠀⠐⢁⢀⠐⠀⢀⠁⠀⠀⠁⠑⠄⠀⢁⠐⠁⠅⢀⠐⢀⠄⠀⠀⠐⠀⠅⢀⠐⢀⢀⠁⠅⢀⠀⠄⠔⠑⠁⠀⠁⠀⠐⠁⢀⠀⠀⢀⠀⠐⠄⠔⠐⠁⢄⠀⠅⢄⠑⠀⢀⠐⠀⠀⢁⢀⠀⢀⠁⢀⠁⠅⠁⠀⠁⠀⠀⠄⠀⠁
⠀⠀⢀⠀⠄⢁⠀⠀⠄⠀⢁⢁⠀⠀⠀⠐⠀⠄⠀⠐⠀⢀⢀⠁⠁⠄⠄⢀⠀⠀⠑⢀⠀⠑⠐⠁⠀⠀⠁⢐⠄⠀⢀⠐⠄⠐⢀⠁⠑⢀⠀⠀⠐⠀⢑⠄⠀⠁⠀⠅⠐⠄⠀⠀⠄⠀⠄⠁⠀⠄⢁⢀⠁⠁⠄⠀⠀⠀⠀⢄
⠑⠀⠀⠀⢀⠀⠁⠄⠐⠀⠀⠄⢄⠀⠐⠐⠀⠑⢁⠀⠔⠀⠀⠀⠀⠀⠕⢄⠀⠐⠀⢀⠔⠅⢀⠀⠐⠀⠅⢐⠀⠕⠐⢀⠀⠔⠀⠅⠀⠀⠀⠄⢀⠁⢀⠀⠀⠔⠀⠐⠁⢀⠀⠁⢀⠁⠀⠐⠐⠄⠐⠐⠄⠀⠀⠀⠀⠀⠔⠐
⠐⠀⢐⠀⠀⠀⢀⠀⠐⠐⠀⠀⠐⠀⠄⢀⠁⢄⠀⠑⢀⠀⠐⢀⢄⠀⠀⢀⠀⢅⢔⠁⠄⠐⠀⠁⠀⠑⠀⠀⠐⠁⠄⠀⢁⢀⠀⠁⠐⠁⠀⠀⠀⠀⠀⠀⠀⠀⠐⢀⠄⠀⢀⢔⠑⢅⠀⠐⢄⠁⢀⠄⢁⢀⠐⠀⠀⢀⢀⢑
⠀⠄⠅⢀⠁⠀⠀⠅⠁⠀⢄⠀⠁⠐⠁⢀⠀⠀⠀⠀⠀⠑⠀⠄⢀⠀⠀⢀⠐⠀⠀⢀⠁⠐⢀⠄⠁⢔⠄⠀⠄⠄⠑⠄⠁⠀⠀⠁⠐⠐⠄⠁⢄⠀⢀⠄⠁⠀⠔⠁⢀⠐⠄⠀⠀⠀⢁⢐⠀⠀⠄⠀⠀⢀⠀⠅⠀⠀⠀⠀
⠔⠀⠄⠀⢀⠄⠁⢀⠁⠄⠀⠀⠑⠀⠐⠁⠀⠐⠑⢀⠄⠄⠀⠀⢄⠀⠔⠔⢀⠀⠁⠀⠔⠀⢀⠄⠀⠀⠁⢐⠀⠑⠀⠀⠀⢅⠀⠀⢀⠀⠁⠄⠀⠁⢀⠀⠔⠐⠀⠀⢅⠀⠐⢀⠀⠀⠄⢀⠐⠔⠀⠁⠄⠀⠁⠀⠔⠐⠀⠀
⠀⠐⢀⠐⠀⠁⢀⠀⠀⠐⠐⢀⢄⠐⢄⠐⠀⠅⠀⠁⠁⠄⠐⢁⠄⠀⠀⢄⠀⠅⠔⠀⢁⠀⠐⠄⢀⠀⠁⠀⠐⠀⢀⠑⢀⠄⠀⠅⠀⠀⠅⠀⠀⢀⢔⠐⠄⠀⠁⠀⠐⠀⠀⢀⠑⠀⠄⠐⢁⢀⠀⢄⢄⠁⢀⠀⢐⠐⠁⠁
⠁⠄⠄⠀⠀⠁⠀⢐⠀⠐⠀⠄⢑⠀⠀⠑⠄⠁⢁⢀⠀⢑⠔⠁⠀⠄⠄⠀⢀⠀⠑⠔⠁⢁⠀⠄⢁⠔⠀⢀⠔⠀⢐⠀⠀⠀⠄⠄⠀⠐⠁⢁⠔⠁⠐⠐⠁⢀⢀⠀⠁⠔⠀⠀⠀⠅⠁⠀⠄⠀⠔⠁⢑⠀⠀⠀⠀⠀⠐⠀
⠁⠀⠅⠐⠀⠁⠀⠀⠐⠁⠀⠀⠕⠀⠀⠅⠀⠐⠀⠀⠕⠁⠀⠄⠅⠀⠀⠐⠀⠐⠅⠀⠀⠄⠀⠀⠀⠀⠀⠐⠀⠀⠀⠀⠁⠀⠀⠀⠄⠀⠐⠁⠀⠄⠁⠀⠁⠄⠀⠐⠄⠀⠅⠀⠀⠔⠁⠀⠀⠕⠀⠀⠀⠀⠀⠁⠀⠁⠀⠀

```



## Phix


```Phix
function is_prime(integer n)
    for p=2 to n do
        if p*p>n then exit end if
        if mod(n,p)=0 then return false end if
    end for
    return n>=2
end function
 
function spiral(integer w, h, x, y)
    return iff(y?w+spiral(h-1,w,y-1,w-x-1):x)
end function
 
integer w = 9, h = 9
for i=h-1 to 0 by -1 do
    for j=w-1 to 0 by -1 do
        integer p = w*h-spiral(w,h,j,i)
        puts(1,"o "[2-is_prime(p)])
    end for
    puts(1,'\n')
end for
```

{{out}}

```txt

    o o
 o     o
o o   o
   o o o
  o  oo o
 o o
o   o
 o   o
o     o

```

For something that almost fills your entire screen, change the definition of w and h to

```Phix
sequence vc = video_config()
integer w = vc[VC_SCRNCOLS]-1, h = vc[VC_SCRNLINES]-1
```



## PicoLisp


```PicoLisp
(load "@lib/simul.l")

(de ceil (A)
   (/ (+ A 1) 2) )

(de prime? (N)
   (or
      (= N 2)
      (and
         (> N 1)
         (bit? 1 N)
         (let S (sqrt N)
            (for (D 3  T  (+ D 2))
               (T (> D S) T)
               (T (=0 (% N D)) NIL) ) ) ) ) )

(de ulam (N)
   (let
      (G (grid N N)
         D '(north west south east .)
         M (ceil N) )
      (setq This
         (intern
            (pack
               (char
                  (+ 96 (if (bit? 1 N) M (inc M))) )
               M ) ) )
      (=: V '_)
      (with ((car D) This)
         (for (X 2 (>= (* N N) X) (inc X))
            (=: V (if (prime? X) '. '_))
            (setq This
               (or
                  (with ((cadr D) This)
                     (unless (: V) (pop 'D) This) )
                  ((pop D) This) ) ) ) )
      G ) )

(mapc
   '((L)
      (for This L
         (prin (align 3 (: V))) )
      (prinl) )
   (ulam 9) )

(bye)
```

{{out}}

```txt

  _  _  _  _  .  _  .  _  _
  _  .  _  _  _  _  _  .  _
  .  _  .  _  _  _  .  _  _
  _  _  _  .  _  .  _  .  _
  _  _  .  _  _  .  .  _  .
  _  .  _  .  _  _  _  _  _
  .  _  _  _  .  _  _  _  _
  _  .  _  _  _  .  _  _  _
  .  _  _  _  _  _  .  _  _

```



## PowerShell


```PowerShell

function New-UlamSpiral ( [int]$N )
    {
    #  Generate list of primes
    $Primes = @( 2 )
    For ( $X = 3; $X -le $N*$N; $X += 2 )
        {
        If ( -not ( $Primes | Where { $X % $_ -eq 0 } | Select -First 1 ) ) { $Primes += $X }
        }
 
    #  Initialize variables
    $X = 0
    $Y = -1
    $i = $N * $N + 1
    $Sign = 1

    #  Intialize array
    $A = New-Object 'boolean[,]' $N, $N

    #  Set top row
    1..$N | ForEach { $Y += $Sign; $A[$X,$Y] = --$i -in $Primes }

    #  For each remaining half spiral...
    ForEach ( $M in ($N-1)..1 )
        {
        #  Set the vertical quarter spiral
        1..$M | ForEach { $X += $Sign; $A[$X,$Y] = --$i -in $Primes }
        
        #  Curve the spiral
        $Sign = -$Sign
        
        #  Set the horizontal quarter spiral
        1..$M | ForEach { $Y += $Sign; $A[$X,$Y] = --$i -in $Primes }
        }
    
    #  Convert the array of booleans to text output of dots and spaces
    $Spiral = ForEach ( $X in 1..$N ) { ( 1..$N | ForEach { ( ' ', '.' )[$A[($X-1),($_-1)]] } ) -join '' }
    return $Spiral
    }

New-UlamSpiral 100

```

{{out}}

```txt

                           .     .                 .       .         . .     .               .     .
    .             .                                   .   .       .     .           .         .     
         .     .   .           .     .       .           .   .     .             .   .     .        
  .     .         . .           .   .                   .   .                 .       .     .       
       .             .   .           . .     .   .                             . .         .        
  . .       .   . .     .           .         .     .             .                     .     . .   
 . .     .             .   .     .             .   .       .   .     .       .   .                  
          .                       .                 . .               . .         .           .     
       .             .   .           .       .     .           .   .     .       .     .       .    
.             .         .     . .         . .         .     .           . .     .           .   .   
             .       .         .     .     .     . .     .         .     .       .     .   .        
                .     .     .             .     .   .     .       .                 .   .   .     . 
 .                                     .     . .   .                   .     .         .            
            .     .                     .     . .   .     .           . .   .     .                 
     .     .       .       .   .             .     .                           .     .   . . . . . .
. . .     . .             .           .     .         .                             .     .     .   
                   .                 .     .     .       .               .     .                    
        . .   . .       .         . .         .                             .         .           . 
     .     . .       . .           .     .               .     . .                                 .
.           .                 .         . .                 .     .   .           .                 
     . .         .         .         . .         .           . .         .     .     . . . . .   .  
    . . .         .     . .           .   .           . .     .   .     .     .             .       
                     .               . .         .                   .     .   .           .        
              . .     .       .     .     .         .       .                   .     .       .     
 .                           .     .   . .   .     .     .       .   . .     .   . .   .     . .    
.                 .                             .   .     .       .         .     .                 
 .         . .     .   .         .   .     .     .       .   . .                   . .              
. . . .   .       . .   .     . .           .         . .   . .               .                 .   
               .               . .                 .     .     .               .     .              
  .                 .             .     .     .     .       .         . . . . .     . .           . 
       . .     .       . .   .   .             .   .       .   .           .     .     . .         .
                                    .     .   . .   .             .   .                 .           
                 . .   . .     .     . .     .   . .                 . . .     . .       . .     .  
  .           . .     .       .     . .                     .     .             .   .     .     .   
               .                     . .     .   .           .     .           .           .        
                                                .   . .   .             . . .   . .   .     .     . 
         . .       .         . .               .   .       .   . .           .                      
            .     .           .     .   .           . .     .                     .     .           
             .         .     . .   . .     . .     .     .           .           .                  
.         . .             .   . .           .           .   .     . .     .                     .   
   .           .           .     .     .   .       .     .               .           .              
                          . .     .   .         .   . .   . . .                 .                 . 
 .         .   .   .       . . . .       .     .         . .   .     . .     .               .     .
      .     .                       .     .   . .               .     .     .                       
                                                 . .     .   .   . .   . . .   . .     . .         .
.       .     .     . . .     .   .   .   . .   .       .   . .         .           .     .       . 
                                                 . .   .                 .           .              
        . .   .     .     . .   .       .   . .     . . .     . . .               . .           . . 
           .   . .   . .     .   . . . . . . . .   .       .         . .         .             .    
                                                . . .           .           .     .                 
     .           . .   .     .             .   .  .. . . .   . . .     .       . . . .             .
    .         . . .       . .       .       . . .                                                   
   .                 .                       .   .                                                  
  .                       . .     .   . .   . .   .   . .   .   . .   .   . .       .     .   . .   
     . .       . .   .   .     .     .   .   .     .     . .   .       .     .     .   . .          
            .     .     .                     .           .           .           .                 
           . .     .         . .         . .     .   .   .       . .       .       .   .     .   .  
.         . . . .             . .     .   .           .       .     .         .     .           .   
         .                       .     .     .   . .                                                
  .       .           .                     . .   .     .   .     .   . . .         .               
   .   . .   .   .     . .       . . . .         . .     .     . .   . .   .       .   . .   . .    
      .                             .   .           . .                                             
 .         . .     .     .     .   . .     .     .   . .           .     .   . . .     . .         .
        .   .     . . . . .     . .       .         .       .     .             .   .               
               .           .               . .   . .         .     .                       .     .  
    .   .     .             .           .   .     .     .         . .     .       .   . .   . . .   
 . .         .       .       .       . .     .                 .             .     .   .       .    
      .     .           .     .   .       .           .   .     .           .     .           .     
 .                     . .                                 .     .   .     .         .   .         .
      .   .     .   . .       .   . .   .     .           .             .     . .     .         . . 
               .           .                 .     .   . .               .     .           .        
    .         .           .     .           .       .               .   .     .           . .     . 
       .             . . .       .     .   . .   .       .     .                       .     .     .
.                       .                 .                       .   .                             
           .     .           .             .     .   . .     .   .     .               . . . .   .  
.   .   .             . .     .                     .           . .           . .         . .     . 
   .                 .           .         .           . .     .   . .               .              
        . .           .                           . .         .       .   .   .   .             . . 
 . . . . . .     . .           .     .       .   .       .                                          
                              .               .       .   . .   . .               .     .     .     
       .         .           . .   .             .     .   .       .         .     .         . . .  
              . . .   .     . .   .     . .           .   .                   .           . .       
   .     .     .                 .                       .         .     . .               .     .  
    .         . .                 .     .   .     .     .                 . .         .   .   . .   
     .     .             .                 . .     .   .                 .       .       .     .   .
.               .             .     .     .   .     . .         . .               .                 
           .     .           .     . .               .     .     .             .             . .    
      . .         .     .     .   .       .         .                               .             . 
               .   .     . .         . .               .     .                   .   .              
  .       .         .                       . .   .       .     .   .       .             . . .   . 
     .   .         .                             .       .                 .   . .           . .   .
      .                 .   . .   .       .               .     .     . .           .               
                 .       .                 .         .     .             .   . .     .              
.         . .         .       .         .     .     .       .   .     . .         . .               
             .           .             .     .   . .   .                 .     .           .     .  
  . .                 .         . .         . .   .     .                   .     .   .           . 
 .       .         .       .               . .         .             .   . .           .     .      
    .       .         .           .     . .                           . .     .   . .               
       .   .             .   . .     .                     .     .             .     .     .       .
                . .           .     .   .     .                 . .           .     .   .           

```



## Python


```python
# coding=UTF-8
from __future__ import print_function, division
from math import sqrt

def cell(n, x, y, start=1):
    d, y, x = 0, y - n//2, x - (n - 1)//2
    l = 2*max(abs(x), abs(y))
    d = (l*3 + x + y) if y >= x else (l - x - y)
    return (l - 1)**2 + d + start - 1

def show_spiral(n, symbol='# ', start=1, space=None):
    top = start + n*n + 1
    is_prime = [False,False,True] + [True,False]*(top//2)
    for x in range(3, 1 + int(sqrt(top))):
        if not is_prime[x]: continue
        for i in range(x*x, top, x*2):
            is_prime[i] = False

    cell_str = lambda x: f(x) if is_prime[x] else space
    f = lambda _: symbol # how to show prime cells

    if space == None: space = ' '*len(symbol)

    if not len(symbol): # print numbers instead
        max_str = len(str(n*n + start - 1))
        if space == None: space = '.'*max_str + ' '
        f = lambda x: ('%' + str(max_str) + 'd ')%x

    for y in range(n):
        print(''.join(cell_str(v) for v in [cell(n, x, y, start) for x in range(n)]))
    print()

show_spiral(10, symbol=u'♞', space=u'♘') # black are the primes
show_spiral(9, symbol='', space=' - ')
# for filling giant terminals
#show_spiral(1001, symbol='*', start=42)
```

{{out}}

```txt

♘♘♘♞♘♘♘♘♘♘
♘♘♘♘♞♘♞♘♘♘
♘♞♘♘♘♘♘♞♘♞
♞♘♞♘♘♘♞♘♘♘
♘♘♘♞♘♞♘♞♘♘
♘♘♞♘♘♞♞♘♞♘
♘♞♘♞♘♘♘♘♘♘
♞♘♘♘♞♘♘♘♘♘
♘♞♘♘♘♞♘♘♘♞
♞♘♘♘♘♘♞♘♘♘

 -  -  -  - 61  - 59  -  - 
 - 37  -  -  -  -  - 31  - 
67  - 17  -  -  - 13  -  - 
 -  -  -  5  -  3  - 29  - 
 -  - 19  -  -  2 11  - 53 
 - 41  -  7  -  -  -  -  - 
71  -  -  - 23  -  -  -  - 
 - 43  -  -  - 47  -  -  - 
73  -  -  -  -  - 79  -  - 

```



## R

My own plotting helper function plotmat() was used and made it possible. You can find it here on RC (Brownian tree in R) .
;Note:
* All pictures are ready to be uploaded, when it would be allowed again.
{{trans|PARI/GP}}
{{Works with|R|3.3.1 and above}}

[[File:UlamSpiralR1.png|200px|right|thumb|Output UlamSpiralR1.png]]
[[File:UlamSpiralR2.png|200px|right|thumb|Output UlamSpiralR2.png]]


```r

## Plotting Ulam spiral (for primes) 2/12/17 aev
## plotulamspirR(n, clr, fn, ttl, psz=600), where: n - initial size;
## clr - color; fn - file name; ttl - plot title; psz - picture size.
## 
require(numbers);
plotulamspirR <- function(n, clr, fn, ttl, psz=600) {
  cat(" *** START:", date(), "n=",n, "clr=",clr, "psz=", psz, "\n");
  if (n%%2==0) {n=n+1}; n2=n*n;
  x=y=floor(n/2); xmx=ymx=cnt=1; dir="R";
  ttl= paste(c(ttl, n,"x",n," matrix."), sep="", collapse="");
  cat(" ***", ttl, "\n");
  M <- matrix(c(0), ncol=n, nrow=n, byrow=TRUE);
  for (i in 1:n2) {
    if(isPrime(i)) {M[x,y]=1};
    if(dir=="R") {if(xmx>0) {x=x+1;xmx=xmx-1}
                  else {dir="U";ymx=cnt;y=y-1;ymx=ymx-1}; next}; 
    if(dir=="U") {if(ymx>0) {y=y-1;ymx=ymx-1}
                  else {dir="L";cnt=cnt+1;xmx=cnt;x=x-1;xmx=xmx-1}; next}; 
    if(dir=="L") {if(xmx>0) {x=x-1;xmx=xmx-1} 
                  else {dir="D";ymx=cnt;y=y+1;ymx=ymx-1}; next}; 
    if(dir=="D") {if(ymx>0) {y=y+1;ymx=ymx-1} 
                  else {dir="R";cnt=cnt+1;xmx=cnt;x=x+1;xmx=xmx-1}; next}; 
  };
  plotmat(M, fn, clr, ttl,,psz);
  cat(" *** END:",date(),"\n");
}

## Executing:
plotulamspirR(100, "red", "UlamSpiralR1", "Ulam Spiral: ");
plotulamspirR(200, "red", "UlamSpiralR2", "Ulam Spiral: ",1240);

```
 

{{Output}}


```txt

> plotulamspirR(100, "red", "UlamSpiralR1", "Ulam Spiral: ");
 *** START: Sun Feb 12 12:03:34 2017 n= 100 clr= red psz= 600 
 *** Ulam Spiral: 101x101 matrix. 
 *** Matrix( 101 x 101 ) 1232 DOTS
 *** END: Sun Feb 12 12:03:37 2017 

> plotulamspirR(200, "red", "UlamSpiralR2", "Ulam Spiral: ",1240);
 *** START: Sun Feb 12 12:03:51 2017 n= 200 clr= red psz= 1240
 *** Ulam Spiral: 201x201 matrix. 
 *** Matrix( 201 x 201 ) 4196 DOTS
 *** END: Sun Feb 12 12:04:07 2017 

```



## Racket


{{trans|Python}}


```racket
#lang racket
(require (only-in math/number-theory prime?))
 
(define ((cell-fn n (start 1)) x y)
  (let* ((y (- y (quotient n 2)))
         (x (- x (quotient (sub1 n) 2)))
         (l (* 2 (if (> (abs x) (abs y)) (abs x) (abs y))))
         (d (if (>= y x) (+ (* l 3) x y) (- l x y))))
    (+ (sqr (- l 1)) d start -1)))
 
(define (show-spiral n
                     #:symbol (smb "# ")
                     #:start (start 1)
                     #:space (space (and smb (make-string (string-length smb) #\space))))
  (define top (+ start (* n n) 1))
  (define cell (cell-fn n start))
  (define print-cell
    (if smb
        (λ (i p?) (display (if p? smb space)))
        (let* ((max-len (string-length (~a (+ (sqr n) start -1))))
               (space (or space (make-string (string-length (~a (+ (sqr n) start -1))) #\_))))
          (λ (i p?)
            (display (if p? (~a #:width max-len i #:align 'right) space))
            (display #\space)))))
 
  (for* ((y (in-range 0 n)) #:when (unless (= y 0) (newline)) (x (in-range 0 n)))
    (define c (cell x y))
    (define p? (prime? c))
    (print-cell c p?))
  (newline))
 
(show-spiral 9 #:symbol #f)
(show-spiral 10 #:symbol "♞" #:space "♘") ; black are the primes
(show-spiral 50 #:symbol "*" #:start 42)
; for filling giant terminals
; (show-spiral 1001 #:symbol "*" #:start 42)
```


{{out}}

```txt
__ __ __ __ 61 __ 59 __ __ 
__ 37 __ __ __ __ __ 31 __ 
67 __ 17 __ __ __ 13 __ __ 
__ __ __  5 __  3 __ 29 __ 
__ __ 19 __ __  2 11 __ 53 
__ 41 __  7 __ __ __ __ __ 
71 __ __ __ 23 __ __ __ __ 
__ 43 __ __ __ 47 __ __ __ 
73 __ __ __ __ __ 79 __ __ 
♘♘♘♞♘♘♘♘♘♘
♘♘♘♘♞♘♞♘♘♘
♘♞♘♘♘♘♘♞♘♞
♞♘♞♘♘♘♞♘♘♘
♘♘♘♞♘♞♘♞♘♘
♘♘♞♘♘♞♞♘♞♘
♘♞♘♞♘♘♘♘♘♘
♞♘♘♘♞♘♘♘♘♘
♘♞♘♘♘♞♘♘♘♞
♞♘♘♘♘♘♞♘♘♘
  *       *         *                 *           
     * *     *                     * *            
*     *         * *   *     * *               * * 
       *                     * *               *  
  *     *           * *   *     *             *   
 *       *         *     * *     *   * *          
*       * *         *           *     * *   * * * 
     *           *     * *           *   * *      
            *     *     *         *       *       
   * *     * *         * *         *     *   *    
          *   *       *         *       *   *     
       *     * *         * *           *          
* *     * *         *       *         * * *     * 
     *     * *   *     *     * *     *            
          * *   * *           *     *             
         *       *     * *         *   *   * * * *
              *       *     *   * *               
               *     *         * *                
  *                   *   * *   * *       *     * 
     *           *     * *     *   *     *       *
    *                 * *     *     *     *       
   *     * *     *     * *   *       *   * * *    
  *           *     *     * *   *     * *         
 *     *       *     *     *                      
  *       *       * * *   *             *   * *   
                       * *   *             *   * *
*   *       *   * *   *       *           *     * 
       *   *       * *     *     * * *     * * *  
  *                 *     *     * *         *     
   *         * *   *                              
    * * *     * * *     *     * *   *   * *   * * 
     *           * *           *   * *     *   *  
*   *           *     *     *             *     * 
       *   *   *   *             *             * *
        *     *     *       *   *       *         
         *   * *   *           *       *     *    
* *   * *   *         *     *     * * *     * *   
           * *   *     *     * *                  
*         *       *     *   *       *             
       * *   * *   *             *   * *          
        *     *     *       *     *         *     
       *     *     *       *     *     *          
  *   *     *           *         *               
     *       *                 *         *        
*           *           *     *   *     *         
   *           *   * *         *           * * * *
                *     *   * *   * *         *     
 *     * *         *             *         *      
*   *     *       *               * *   *       * 
     *           *     *     *             *   *  

```



## REXX

Programming note for the showing of the spiral:   because images can't be uploaded at this time on Rosetta Code, the glyphs for primes was chosen to be a solid glyph   (or in ASCII or XML terminology, a "block"). 

This then allows the REXX program to compress two rows of the Ulam spiral into one by processing two rows at a time by comparing each character to the character on the next line   (when comparing two lines as a pair):
::* if a char on row   '''k'''   is a block, and the char on row   '''k+1'''   is a block, then a   "block"   is used.
::* if a char on row   '''k'''   is a block, and the char on row   '''k+1'''   is a blank, then a   "UHblk"   is used.
::* if a char on row   '''k'''   is a blank, and the char on row   '''k+1'''   is a block, then a   "LHblk"   is used.
::* if a char on row   '''k'''   is a blank, and the char on row   '''k+1'''   is a blank, then a     blank   is used.

For codepage 437:
::::::*   a "block" is 'db'x     █       (a full block)

::::::*   a "LHblk" is 'dc'x     ▄       (a '''L'''ower '''H'''alf '''bl'''oc'''k''')

::::::*   a "UHblk" is 'df'x     ▀       (a '''U'''pper '''H'''alf '''bl'''oc'''k''')

Or, to show all three characters in the (above) ordered next to each other (separated by a blank):   █ ▄ ▀

This allows the displaying of the Ulam prime spiral to keep a (mostly) square aspect ratio.

The characters chosen allow for the HTML on Rosetta Code to shrink (via STYLE ''font-size'') the displayed output to half their normal height.
  
===counter-clockwise===

```rexx
/*REXX program shows counter─clockwise  Ulam spiral  of primes shown in a square matrix.*/
parse arg size init char .                       /*obtain optional arguments from the CL*/
if size=='' | size==","  then size= 79           /*Not specified?  Then use the default.*/
if init=='' | init==","  then init=  1           /* "      "         "   "   "     "    */
if char==''              then char= "█"          /* "      "         "   "   "     "    */
tot=size**2                                      /*the total number of numbers in spiral*/
                                                 /*define the upper/bottom right corners*/
uR.=0; bR.=0;   do od=1  by 2  to tot;  _=od**2+1;  uR._=1;  _=_+od;   bR._=1;  end /*od*/
                                                 /*define the bottom/upper left corners.*/
bL.=0; uL.=0;   do ev=2  by 2  to tot;  _=ev**2+1;  bL._=1;  _=_+ev;   uL._=1;  end /*ev*/

app=1;    bigP=0;    #p=0;    inc=0;     minR=1;    maxR=1;    r=1;    $=0;    $.=;    !.=
                     /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ construct the spiral #s.*/
  do i=init  for tot;    r= r + inc;     minR= min(minR, r);      maxR= max(maxR, r)
  x= isPrime(i);   if x  then bigP= max(bigP, i);          #p= #p + x   /*bigP, #primes.*/
  if app  then $.r= $.r ||  x                                           /*append  token.*/
          else $.r=  x  || $.r                                          /*prepend token.*/
  if uR.i  then do;  app= 1;  inc= +1;  iterate  /*i*/;    end          /*advance  ↓    */
  if bL.i  then do;  app= 0;  inc= -1;  iterate  /*i*/;    end          /*   "     ↑    */
  if bR.i  then do;  app= 0;  inc=  0;  iterate  /*i*/;    end          /*   "     ►    */
  if uL.i  then do;  app= 1;  inc=  0;  iterate  /*i*/;    end          /*   "     ◄    */
  end   /*i*/                                                           /* [↓] pack two */
                                                                        /*lines ──► one.*/
  do j=minR  to maxR  by 2;    jp= j + 1;              $= $ + 1         /*fold two lines*/
    do k=1  for  length($.j);  top= substr($.j, k, 1)                   /*the  1st line.*/
                               bot= word( substr($.jp, k, 1)   0, 1)    /*the  2nd line.*/
    if top  then if  bot  then !.$= !.$'█'                              /*has top & bot.*/
                          else !.$= !.$'▀'                              /*has top,¬ bot.*/
            else if  bot  then !.$= !.$'▄'                              /*¬ top, has bot*/
                          else !.$= !.$' '                              /*¬ top,   ¬ bot*/
    end   /*k*/
  end     /*j*/                                  /* [↓]  show the  prime  spiral matrix.*/
                                    do m=1  for $;     say !.m;     end  /*m*/
say;  say init 'is the starting point,'  ,
          tot  'numbers used,'   #p   "primes found, largest prime:"   bigP
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPrime: procedure; parse arg x;  if wordpos(x, '2 3 5 7 11 13 17 19') \==0  then return 1
         if x<17  then return 0;                            if x// 2    ==0  then return 0
                                                            if x// 3    ==0  then return 0
         /*get the last digit*/    parse var x  ''  -1  _;  if         _==5  then return 0
                                                            if x// 7    ==0  then return 0
                                                            if x//11    ==0  then return 0
                                                            if x//13    ==0  then return 0

                  do j=17  by 6  until  j*j > x;            if x//j     ==0  then return 0
                                                            if x//(j+2) ==0  then return 0
                  end   /*j*/;          return 1
```

{{out|output|text=  when using the default input:}}

(Shown at three-quarter size.)
<pre style="font-size:75%">
      ▀     ▀     ▀          ▄  ▀  ▄ ▄▀  ▄▀     ▀       ▀    ▄     ▄      ▀  ▄▀
 ▄▀     ▀▄       ▄   ▄        ▀    ▄▀ ▀  ▄▀     ▀           ▀ ▀   ▀  ▄  ▀  ▄
▀ ▀      ▄      ▀          ▄▀    ▄▀    ▄    ▀  ▄               ▄     ▄    ▀
▀▄ ▄▀ ▀    ▄ ▄▀         ▀▄▀    ▄    ▀          ▄     ▄ ▄          ▀         ▀
  ▀    ▄         ▄  ▀      ▄ ▄▀ ▀      ▄          ▀▄ ▄  ▀   ▀  ▄     ▄  ▀  ▄ ▄
        ▀  ▄  ▀ ▀          ▄▀▄  ▀      ▄    ▀ ▀     ▀   ▀  ▄  ▀  ▄  ▀▄
    ▀ ▀     ▀      ▄▀    ▄▀  ▄ ▄▀  ▄     ▄▀    ▄  ▀    ▄   ▄ ▄     ▄  ▀▄ ▄  ▀▄
 ▄ ▄    ▀▄   ▄         ▄   ▄     ▄    ▀▄  ▀    ▄▀  ▄ ▄  ▀         ▀     ▀▄ ▄
▀    ▄  ▀ ▀   ▀     ▀▄▀▄          ▀      ▄  ▀ ▀▄  ▀ ▀▄              ▀▄     ▄
     ▄    ▀  ▄ ▄   ▄   ▄▀     ▀     ▀▄   ▄▀      ▄▀  ▄      ▀ ▀ ▀▄▀ ▀  ▄  ▀ ▀▄
       ▄ ▄   ▄ ▄     ▄    ▀▄ ▄  ▀  ▄▀ ▀▄ ▄▀             ▀  ▄▀▄ ▄     ▄ ▄      ▀
    ▀▄▀     ▀       ▀     ▀▄▀▄     ▄   ▄          ▀▄    ▀▄           ▄▀   ▀
 ▄       ▄         ▄ ▄               ▄▀  ▄▀ ▀   ▀▄   ▄ ▄      ▀ ▀ ▀▄  ▀ ▀   ▀
  ▀▄    ▀    ▄     ▄▀▄   ▄▀▄  ▀  ▄ ▄     ▄▀ ▀  ▄  ▀        ▄           ▄▀     ▀
▀ ▀  ▄          ▀▄  ▀ ▀▄     ▄   ▄▀      ▄    ▀▄  ▀     ▀ ▀    ▄▀          ▄
 ▄   ▄   ▄      ▀▄▀▄ ▄ ▄▀   ▀  ▄     ▄▀   ▀ ▀  ▄▀▄▀ ▀▄     ▄ ▄     ▄  ▀
  ▀                       ▀     ▀   ▀ ▀▄ ▄     ▄   ▄  ▀▄ ▄  ▀▄ ▄ ▄▀  ▄ ▄     ▄
    ▀     ▀ ▀ ▀     ▀   ▀   ▀   ▀ ▀   ▀▄ ▄   ▄▀   ▀ ▀         ▀▄          ▀▄
▀▄  ▀▄ ▄  ▀▄ ▄  ▀ ▀▄  ▀▄ ▄ ▄ ▄▀▄ ▄▀▄▀▄   ▄▀ ▀ ▀  ▄  ▀ ▀ ▀  ▄ ▄         ▄▀ ▀
       ▄ ▄   ▄     ▄             ▄   ▄▀ █▄▀▄ ▄ ▄   ▄ ▄▀▄     ▄    ▀  ▄ ▄▀▄ ▄
    ▀ ▀ ▀  ▄    ▀ ▀       ▀       ▀▄▀ ▀▄
     ▄ ▄   ▄   ▄▀ ▀  ▄  ▀  ▄▀ ▀▄  ▀▄▀   ▀▄  ▀ ▀▄ ▄▀  ▄▀ ▀   ▀▄  ▀ ▀▄     ▄▀  ▄
 ▄▀▄    ▀▄    ▀    ▄ ▄         ▄ ▄  ▀  ▄   ▄   ▄▀      ▄ ▄  ▀    ▄      ▀▄   ▄
▀ ▀ ▀ ▀             ▀ ▀▄    ▀▄  ▀  ▄   ▄ ▄  ▀       ▀     ▀         ▀     ▀
▀  ▄   ▄    ▀▄ ▄       ▄ ▄ ▄ ▄    ▀ ▀  ▄▀▄    ▀▄  ▀  ▄ ▄▀  ▄▀▄▀ ▀▄       ▄▀  ▄
 ▄ ▄     ▄     ▄     ▄   ▄▀▄  ▀  ▄     ▄  ▀▄▀▄           ▄     ▄   ▄ ▄ ▄     ▄
  ▀  ▄  ▀ ▀ ▀ ▀ ▀▄    ▀ ▀       ▀▄ ▄   ▄ ▄▀       ▀▄    ▀▄            ▀   ▀
   ▄▀      ▄      ▀▄       ▄ ▄▀   ▀▄    ▀     ▀      ▄  ▀ ▀     ▀  ▄    ▀▄  ▀▄▀
  ▀          ▄▀▄    ▀   ▀       ▀           ▀   ▀▄    ▀▄   ▄     ▄▀     ▀  ▄
▀    ▄▀   ▀ ▀    ▄  ▀   ▀ ▀   ▀    ▄▀    ▄   ▄ ▄▀             ▀▄    ▀▄▀     ▀
    ▀      ▄ ▄ ▄▀     ▀▄     ▄   ▄▀▄   ▄  ▀    ▄     ▄    ▀   ▀     ▀        ▄
 ▄     ▄      ▀    ▄            ▀▄     ▄   ▄ ▄     ▄   ▄▀   ▀▄               ▄
           ▄▀ ▀     ▀  ▄         ▄        ▀  ▄ ▄     ▄▀ ▀▄ ▄        ▀ ▀    ▄
▀▄     ▄ ▄  ▀        ▄     ▄       ▄   ▄▀ ▀    ▄    ▀       ▀   ▀   ▀   ▀
       ▄           ▄▀▄   ▄          ▀  ▄    ▀▄  ▀▄▀   ▀ ▀▄         ▄    ▀▄    ▀
    ▀▄▀ ▀   ▀     ▀ ▀  ▄▀     ▀ ▀           ▀  ▄▀        ▄     ▄ ▄  ▀
 ▄  ▀ ▀        ▄        ▀     ▀  ▄▀▄    ▀▄   ▄▀                ▄▀ ▀    ▄    ▀
 ▄    ▀▄           ▄▀    ▄▀▄    ▀   ▀     ▀▄▀    ▄    ▀▄▀            ▄  ▀
     ▄  ▀▄    ▀▄ ▄  ▀   ▀  ▄ ▄  ▀         ▀  ▄     ▄                   ▄  ▀▄
▀         ▀                       ▀ ▀   ▀       ▀     ▀   ▀       ▀

1 is the starting point, 6241 numbers used, 811 primes found, largest prime: 6229

```
 
{{out|output|text=  when the following input is used:     <tt> ,   41 </tt>}}

(Shown at three-quarter size.)
<pre style="font-size:75%">
    ▀▄     ▄▀    ▄      ▀  ▄      ▀▄▀  ▄      ▀     ▀     ▀          ▄  ▀  ▄ ▄▀
            ▀   ▀ ▀▄           ▄    ▀  ▄▀ ▀     ▀▄       ▄   ▄        ▀    ▄▀
 ▄▀▄   ▄▀   ▀▄▀    ▄ ▄    ▀▄   ▄ ▄  ▀     ▀      ▄      ▀          ▄▀    ▄▀ ▀▄▀
▀ ▀ ▀ ▀    ▄▀  ▄ ▄            ▀  ▄      ▀    ▄▀    ▄ ▄▀         ▀▄▀    ▄
▀  ▄   ▄    ▀▄  ▀  ▄       ▄  ▀  ▄   ▄  ▀ ▀▄  ▀          ▄  ▀      ▄ ▄▀    ▄▀
 ▄ ▄     ▄         ▄ ▄  ▀▄▀               ▀ ▀      ▄  ▀ ▀          ▄▀ ▀ ▀▄    ▀
  ▀  ▄  ▀ ▀ ▀   ▀▄         ▄ ▄▀        ▄  ▀  ▄ ▄   ▄▀      ▄▀    ▄▀▄
   ▄▀      ▄    ▀  ▄ ▄  ▀  ▄      ▀              ▄ ▄▀          ▄    ▀     ▀ ▀
  ▀          ▄▀▄    ▀▄        ▀ ▀   ▀     ▀▄▀▄              ▀▄▀  ▄
▀    ▄▀   ▀ ▀    ▄    ▀▄         ▄▀ ▀  ▄▀▄    ▀▄   ▄ ▄     ▄ ▄ ▄ ▄▀  ▄   ▄    ▀
    ▀      ▄ ▄ ▄▀     ▀ ▀      ▄  ▀  ▄ ▄      ▀    ▄▀ ▀▄ ▄▀
 ▄     ▄      ▀     ▀    ▄▀▄    ▀    ▄▀▄        ▀▄     ▄▀  ▄▀    ▄ ▄   ▄
           ▄▀ ▀   ▀ ▀   ▀  ▄▀▄      ▀  ▄ ▄    ▀      ▄▀▄ ▄     ▄             ▄
▀▄     ▄ ▄  ▀  ▄ ▄    ▀▄ ▄ ▄   ▄  ▀  ▄    ▀▄ ▄     ▄▀               ▀       ▀
       ▄            ▀    ▄▀▄  ▀▄▀    ▄ ▄    ▀    ▄▀   ▀   ▀ ▀ ▀ ▀▄▀ ▀ ▀▄    ▀
    ▀▄▀ ▀     ▀         ▀    ▄     ▄▀     ▀  ▄▀▄▀▄       ▄     ▄     ▄ ▄     ▄
 ▄  ▀ ▀          ▄            ▀     ▀▄ ▄▀ ▀  ▄▀   ▀▄    ▀▄      ▀    ▄      ▀
 ▄      ▀    ▄     ▄▀           ▀   ▀▄▀▄   ▄▀  ▄    ▀▄ ▄▀ ▀ ▀    ▄  ▀ ▀  ▄
    ▀      ▄     ▄▀     ▀ ▀  ▄  ▀  ▄    ▀▄▀            ▄   ▄ ▄    ▀        ▄  ▀
▀  ▄     ▄▀     ▀▄    ▀  ▄    ▀  ▄ ▄▀▄ ▄▀   ▀▄           ▄▀   ▀▄▀   ▀ ▀     ▀
 ▄      ▀▄     ▄   ▄       ▄   ▄ ▄   ▄▀   ▀    ▄▀▄▀ ▀     ▀▄▀ ▀         ▀▄   ▄▀
  ▀  ▄  ▀  ▄     ▄    ▀   ▀       ▀▄▀     ▀  ▄ ▄   ▄   ▄ ▄   ▄ ▄ ▄ ▄ ▄     ▄
 ▄       ▄   ▄    ▀▄ ▄ ▄    ▀▄▀▄ ▄▀▄    ▀     ▀   ▀ ▀    ▄▀   ▀▄    ▀     ▀
       ▄▀      ▄   ▄▀          ▄▀  ▄  ▀     ▀   ▀      ▄      ▀ ▀▄          ▀
▀ ▀                   ▀▄  ▀  ▄▀▄   ▄▀       ▀  ▄▀  ▄ ▄▀    ▄▀▄   ▄ ▄ ▄  ▀  ▄ ▄
 ▄ ▄▀ ▀    ▄   ▄ ▄   ▄ ▄▀  ▄▀▄   ▄    ▀▄    ▀▄ ▄  ▀                 ▀▄
 ▄      ▀     ▀▄         ▄▀  ▄ ▄  ▀▄    ▀   ▀    ▄  ▀▄▀    ▄       ▄  ▀▄ ▄  ▀▄
  ▀     ▀ ▀▄▀         ▀▄▀    ▄▀    ▄▀      ▄▀    ▄▀    ▄          ▀     ▀▄ ▄
   ▄▀ ▀     ▀▄   ▄   ▄▀     ▀▄          ▀      ▄  ▀      ▄          ▀▄     ▄
  ▀▄     ▄   ▄ ▄    ▀       ▀  ▄   ▄ ▄  ▀     ▀▄  ▀     ▀  ▄▀ ▀ ▀▄▀ ▀  ▄  ▀ ▀▄
     ▄▀           ▀    ▄ ▄      ▀  ▄  ▀   ▀ ▀   ▀▄▀        ▄▀  ▄     ▄ ▄      ▀
 ▄  ▀▄    ▀▄   ▄▀   ▀▄    ▀      ▄▀    ▄     ▄    ▀ ▀   ▀  ▄   ▄     ▄▀   ▀
 ▄       ▄   ▄▀ ▀▄ ▄            ▀  ▄    ▀   ▀    ▄   ▄  ▀  ▄      ▀▄  ▀ ▀   ▀
  ▀ ▀ ▀ ▀ ▀ ▀       ▀   ▀    ▄ ▄   ▄ ▄▀   ▀      ▄▀           ▀        ▄▀     ▀
     ▄   ▄▀   ▀ ▀▄         ▄       ▄  ▀  ▄    ▀▄          ▀     ▀    ▄     ▄
       ▄▀        ▄     ▄ ▄  ▀      ▄ ▄         ▄ ▄▀        ▄  ▀ ▀▄ ▄▀
▀   ▀▄▀                ▄▀ ▀    ▄    ▀             ▀    ▄▀  ▄▀▄▀  ▄▀    ▄ ▄   ▄
▀  ▄▀    ▄    ▀▄▀            ▄  ▀      ▄    ▀       ▀▄   ▄            ▀   ▀
  ▀  ▄     ▄                   ▄  ▀▄           ▄  ▀ ▀     ▀  ▄▀ ▀   ▀ ▀
▀       ▀     ▀   ▀       ▀                 ▀         ▀     ▀     ▀ ▀     ▀

41 is the starting point, 6241 numbers used, 805 primes found, largest prime: 6277

```

{{out|output|text=  with an input of   '''416'''    can be viewed here at   ───►   [[Ulam spiral (for primes)/REXX‎]]


### clockwise

This REXX version is presented here to show the difference between a clockwise and a counter-clockwise Ulam (prime) spiral.

```rexx
/*REXX program shows a    clockwise   Ulam spiral  of  primes  shown in a square matrix.*/
parse arg size init char .                       /*obtain optional arguments from the CL*/
if size=='' | size==","  then size= 79           /*Not specified?  Then use the default.*/
if init=='' | init==","  then init=  1           /* "      "         "   "   "     "    */
if char==''              then char= "█"          /* "      "         "   "   "     "    */
tot=size**2                                      /*the total number of numbers in spiral*/
                                                 /*define the upper/bottom right corners*/
uR.=0; bR.=0;   do od=1  by 2  to tot; _=od**2+init;  uR._=1;  _=_+od;  bR._=1; end /*od*/
                                                 /*define the bottom/upper left corners.*/
bL.=0; uL.=0;   do ev=2  by 2  to tot; _=ev**2+init;  bL._=1;  _=_+ev;  uL._=1; end /*ev*/

app=1;    bigP=0;    #p=0;    inc=0;     minR=1;    maxR=1;    r=1;    $=0;    $.=;    !.=
                     /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ construct the spiral #s.*/
  do i=init  for tot;    r= r + inc;     minR= min(minR, r);      maxR= max(maxR, r)
  x= isPrime(i);   if x  then bigP= max(bigP, i);          #p= #p + x   /*bigP, #primes.*/
  if app  then $.r= $.r ||  x                                           /*append  token.*/
          else $.r=  x  || $.r                                          /*prepend token.*/
  if uR.i  then do;  app= 1;  inc= +1;  iterate  /*i*/;    end          /*advance  ↓    */
  if bL.i  then do;  app= 0;  inc= -1;  iterate  /*i*/;    end          /*   "     ↑    */
  if bR.i  then do;  app= 0;  inc=  0;  iterate  /*i*/;    end          /*   "     ►    */
  if uL.i  then do;  app= 1;  inc=  0;  iterate  /*i*/;    end          /*   "     ◄    */
  end   /*i*/                                                           /* [↓] pack two */
                                                                        /*lines ──► one.*/
  do j=minR  to maxR  by 2;    jp= j + 1;              $= $ + 1         /*fold two lines*/
    do k=1  for  length($.j);  top= substr($.j, k, 1)                   /*the  1st line.*/
                               bot= word( substr($.jp, k, 1)   0, 1)    /*the  2nd line.*/
    if top  then if  bot  then !.$= !.$'█'                              /*has top & bot.*/
                          else !.$= !.$'▀'                              /*has top,¬ bot.*/
            else if  bot  then !.$= !.$'▄'                              /*¬ top, has bot*/
                          else !.$= !.$' '                              /*¬ top,   ¬ bot*/
    end   /*k*/
  end     /*j*/                                  /* [↓]  show the  prime  spiral matrix.*/
                                    do m=1  for $;     say !.m;     end  /*m*/
say;  say init 'is the starting point,'  ,
          tot  'numbers used,'   #p   "primes found, largest prime:"   bigP
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isPrime: procedure; parse arg x;  if wordpos(x, '2 3 5 7 11 13 17 19') \==0  then return 1
         if x<17  then return 0;                            if x// 2    ==0  then return 0
                                                            if x// 3    ==0  then return 0
         /*get the last digit*/    parse var x  ''  -1  _;  if         _==5  then return 0
                                                            if x// 7    ==0  then return 0
                                                            if x//11    ==0  then return 0
                                                            if x//13    ==0  then return 0

                  do j=17  by 6  until  j*j > x;            if x//j     ==0  then return 0
                                                            if x//(j+2) ==0  then return 0
                  end   /*j*/;          return 1
```

{{out|output|text=  when using the default input:}}

(Shown at three-quarter size.)
<pre style="font-size:75%">
▀    ▄   ▄▀    ▄ ▄         ▄ ▄    ▀ ▀   ▀    ▄  ▀  ▄  ▀   ▀       ▀    ▄   ▄
 ▄     ▄▀     ▀    ▄▀   ▀▄ ▄    ▀         ▀▄     ▄     ▄             ▄    ▀
 ▄    ▀        ▄    ▀     ▀     ▀▄ ▄▀    ▄▀ ▀▄        ▀ ▀      ▄       ▄▀
    ▀▄▀                ▄▀     ▀   ▀     ▀     ▀▄         ▄     ▄▀▄▀         ▀
    ▀ ▀▄▀   ▀     ▀▄▀▄  ▀▄    ▀ ▀      ▄    ▀▄  ▀▄       ▄         ▄▀    ▄
 ▄     ▄ ▄          ▀▄     ▄       ▄▀  ▄    ▀  ▄▀ ▀   ▀ ▀               ▀     ▀
▀          ▄▀          ▄         ▄      ▀ ▀  ▄ ▄    ▀▄   ▄ ▄▀   ▀   ▀   ▀  ▄
 ▄     ▄    ▀ ▀    ▄▀            ▄     ▄  ▀▄ ▄     ▄  ▀▄▀    ▄      ▀ ▀      ▄
           ▄ ▄▀▄       ▄     ▄  ▀▄ ▄   ▄       ▄     ▄  ▀   ▀                ▄
    ▀▄          ▀▄    ▀           ▀▄     ▄▀  ▄ ▄          ▀   ▀▄    ▀▄
▀     ▀   ▀ ▀▄ ▄    ▀   ▀ ▀   ▀     ▀           ▀▄     ▄   ▄  ▀  ▄  ▀ ▀    ▄▀
  ▀▄       ▄  ▀    ▄▀   ▀  ▄ ▄  ▀  ▄        ▀   ▀    ▄▀           ▀▄    ▀▄   ▄
    ▀▄           ▄▀           ▀  ▄▀▄   ▄▀▄    ▀    ▄    ▀▄▀     ▀       ▀   ▀ ▀
 ▄▀▄    ▀▄▀ ▀ ▀▄▀    ▄▀ ▀▄ ▄    ▀▄     ▄  ▀▄ ▄    ▀     ▀▄     ▄   ▄ ▄▀▄  ▀  ▄
   ▄   ▄     ▄ ▄       ▄ ▄▀▄ ▄▀        ▄ ▄▀ ▀  ▄     ▄ ▄   ▄ ▄   ▄       ▄   ▄
▀           ▀          ▄     ▄    ▀▄▀  ▄▀▄    ▀   ▀     ▀   ▀ ▀ ▀         ▀
▀▄▀▄▀ ▀  ▄         ▄▀▄▀     ▀  ▄▀▄     ▄   ▄▀  ▄    ▀  ▄ ▄▀      ▄  ▀    ▄▀  ▄
  ▀  ▄ ▄▀  ▄  ▀▄     ▄     ▄   ▄   ▄▀    ▄     ▄▀▄   ▄      ▀▄     ▄    ▀▄   ▄
           ▄    ▀ ▀     ▀   ▀ ▀   ▀▄▀  ▄▀   ▀ ▀   ▀   ▀ ▀   ▀   ▀ ▀       ▀
    ▀ ▀▄▀▄   ▄  ▀ ▀▄      ▀      ▄▀ ▀▄▀ ▄▄ ▄ ▄ ▄   ▄ ▄ ▄     ▄       ▄ ▄ ▄ ▄
 ▄   ▄ ▄   ▄ ▄     ▄   ▄ ▄ ▄ ▄ ▄ ▄ ▄ ▄▀ ▀▄▀      ▄    ▀    ▄ ▄    ▀    ▄▀
▀   ▀     ▀     ▀ ▀   ▀       ▀   ▀ ▀  ▄ ▄▀ ▀▄▀     ▀ ▀ ▀      ▄        ▀ ▀▄
    ▀     ▀ ▀ ▀     ▀   ▀   ▀   ▀ ▀   ▀▄ ▄    ▀▄  ▀▄▀  ▄ ▄   ▄▀▄ ▄   ▄ ▄  ▀  ▄
 ▄▀  ▄   ▄       ▄ ▄ ▄ ▄  ▀    ▄▀   ▀▄▀        ▄ ▄   ▄▀    ▄▀▄    ▀▄
     ▄          ▀▄▀    ▄▀   ▀▄   ▄    ▀  ▄▀ ▀  ▄▀ ▀ ▀          ▄      ▀    ▄
▀ ▀▄         ▄  ▀  ▄▀▄▀  ▄ ▄     ▄▀▄     ▄    ▀▄  ▀     ▀ ▀▄    ▀      ▄
 ▄▀     ▀▄         ▄▀▄    ▀   ▀      ▄   ▄▀ ▀    ▄▀  ▄ ▄           ▄    ▀     ▀
     ▄                     ▄ ▄     ▄  ▀▄  ▀ ▀   ▀  ▄     ▄    ▀ ▀ ▀  ▄▀ ▀   ▀
    ▀ ▀▄ ▄  ▀▄ ▄    ▀▄    ▀▄▀▄     ▄   ▄ ▄        ▀     ▀  ▄ ▄ ▄     ▄▀▄  ▀
     ▄       ▄ ▄   ▄   ▄  ▀     ▀   ▀▄▀  ▄▀      ▄   ▄  ▀   ▀    ▄     ▄     ▄▀
     ▄    ▀          ▄ ▄▀     ▀     ▀    ▄▀    ▄  ▀  ▄      ▀ ▀ ▀ ▀ ▀▄    ▀▄▀
▀▄ ▄    ▀▄▀  ▄▀     ▀ ▀▄   ▄     ▄▀    ▄    ▀ ▀▄  ▀▄▀▄              ▀    ▄ ▄
        ▀          ▄     ▄   ▄ ▄   ▄  ▀  ▄▀    ▄▀      ▄▀  ▄ ▄    ▀▄   ▄▀▄   ▄
    ▀ ▀    ▄▀       ▀     ▀▄ ▄  ▀      ▄  ▀       ▀        ▄     ▄   ▄▀     ▀
       ▄▀     ▀ ▀▄         ▄▀▄  ▀      ▄    ▀ ▀    ▄▀▄  ▀     ▀▄    ▀▄     ▄ ▄
 ▄▀▄       ▄ ▄      ▀    ▄    ▀▄▀              ▄  ▀  ▄ ▄▀   ▀           ▀
▀   ▀ ▀  ▄    ▀         ▀ ▀▄     ▄  ▀  ▄       ▄               ▄  ▀  ▄      ▀
▀▄▀      ▄      ▀▄   ▄      ▀     ▀▄     ▄  ▀                        ▄    ▀▄
  ▀     ▀                    ▄▀    ▄▀▄▀  ▄▀     ▀           ▀▄▀   ▀▄    ▀    ▄
      ▀     ▀     ▀             ▀     ▀   ▀     ▀       ▀                 ▀   ▀

1 is the starting point, 6241 numbers used, 811 primes found, largest prime: 6229

```



## Ring


```ring

# Project : Ulam spiral (for primes)
 
load "guilib.ring"
load "stdlib.ring"

paint = null

new qapp 
        {
        win1 = new qwidget() {
                  setwindowtitle("Ulam spiral")
                  setgeometry(100,100,560,600)
                  label1 = new qlabel(win1) {
                              setgeometry(10,10,800,600)
                              settext("")
                  }
                  new qpushbutton(win1) {
                          setgeometry(220,500,100,30)
                          settext("draw")
                          setclickevent("draw()")
                  }
                  show()
        }
        exec()
        }

func draw
        p1 = new qpicture()
               color = new qcolor() {
               setrgb(0,0,255,255)
        }
        pen = new qpen() {
                 setcolor(color)
                 setwidth(1)
        }
        paint = new qpainter() {
                  begin(p1)
                  setpen(pen)

        usn = 81
        ulamspiral(usn)

        endpaint()
        }
        label1 { setpicture(p1) show() }
        return

func ulamspiral(nr)
        button = list(nr)
        win1{
                sizenew = sqrt(nr)
                 for n = 1 to nr
                     col = n%9
                     if col = 0 col = 9 ok
                     row = ceil(n/9)

                     button[n] = new qpushbutton(win1)
                     {
                                        setgeometry(60+col*40,60+row*40,40,40)
                                        setclickevent("movetile(" + string(n) +")")   
                                        show() 
                     } 
                next
        n = 9
        result = newlist(n,n)
        k = 1 
        top = 1
        bottom = n
        left = 1
        right = n
        while (k<=n*n)
                 for i=left to right
                      result[top][i]=k
                      k = k + 1
                 next    
                 top = top + 1 
                 for i=top to bottom
                     result[i][right]=k
                     k = k + 1
                 next
                 right = right - 1 
                 for i=right to left step -1
                      result[bottom][i]=k
                      k = k + 1
                 next
                 bottom = bottom - 1 
                 for i=bottom to top step -1
                      result[i][left] = k
                      k = k + 1
                 next
                 left = left + 1
        end
        for m = 1 to n
             for p = 1 to n  
                  pos = (m-1)*n + p
                  if isprime(result[m][p])
                     button[pos] {settext(string(result[m][p]))}
                  ok
             next
         next
         }

```

Outputimage:

[https://www.dropbox.com/s/doqvdr8l852t8o9/CalmoSoftUlamSpiral.jpg?dl=0 Ulam spiral]


## Ruby

It finds the number from the position ( the coordinates ).
{{trans|Python}}

```ruby
require 'prime'

def cell(n, x, y, start=1)
  y, x = y - n/2, x - (n - 1)/2
  l = 2 * [x.abs, y.abs].max
  d = y >= x ? l*3 + x + y : l - x - y
  (l - 1)**2 + d + start - 1
end

def show_spiral(n, symbol=nil, start=1)
  puts "\nN : #{n}"
  format = "%#{(start + n*n - 1).to_s.size}s "
  n.times do |y|
    n.times do |x|
      i = cell(n,x,y,start)
      if symbol
        print i.prime? ? symbol[0] : symbol[1]
      else
        print format % (i.prime? ? i : '')
      end
    end
    puts
  end
end

show_spiral(9)
show_spiral(25)
show_spiral(25, "# ")
```


{{out}}

```txt

N : 9
            61    59       
   37                31    
67    17          13       
          5     3    29    
      19        2 11    53 
   41     7                
71          23             
   43          47          
73                79       

N : 25
577                     571     569                     563                     557                 
                            479                                             467             463     
        401             397                             389                     383                 
    487                                     317             313     311             307     461     
                257                     251                                     241     379         
                    197             193     191                                                     
                                                139     137                     239             547 
    491             199     101              97                             181             457     
                                                 61      59             131                         
            331             103      37                      31      89     179                     
587     409     263     149      67      17              13                             373         
                                              5       3      29                                     
                        151              19           2  11      53     127     233             541 
                            107      41       7                                                     
                                 71              23                                                 
    499     337             109      43              47              83     173             449     
593             269              73                      79                     229     367         
                                    113                                             293             
                271     157                     163             167             227                 
    503             211                                             223                             
        419                     277             281     283                                         
                            347     349             353                     359             443     
599     421                                     431     433                     439                 
            509                                             521     523                             
601                     607                     613             617     619                         

N : 25
#     # #     #     #    
       #           #   # 
  #   #       #     #    
 #         #   # #   # # 
    #     #         # #  
     #   # #             
            # #     #   #
 #   # #   #       #   # 
            # #   #      
   #   # #     # # #     
# # # # # #   #       #  
           # # #         
      #   #  ## # # #   #
       # # #             
        #   #            
 # #   # #   #   # #   # 
#   #   #     #     # #  
         #           #   
    # #     #   #   #    
 #   #           #       
  #     #   # #          
       # #   #     #   # 
# #         # #     #    
   #           # #       
#     #     #   # #      

```



### Another Version

computes the next spiral position.

```ruby
require 'prime'

def spiral_generator(x=0, y=0)
  Enumerator.new do |yielder|
    yielder << [x, y]                           # start position
    dx, dy = 0, 1                               # first direction
    yielder << [x+=dx, y+=dy]                   # second position
    0.step do |i|
      2.times do
        i.times{ yielder << [x+=dx, y+=dy] }    # going straight
        dx, dy = -dy, dx                        # 90 degree turn
        yielder << [x+=dx, y+=dy]
      end
    end
  end
end

def ulam_spiral(n, start=1)
  h = Hash.new(0)
  position = spiral_generator
  (start ... start+n*n).each do |i|
    pos = position.next
    h[pos] = 1  if i.prime?
  end
  
  chr = [[' ', '▄'], ['▀', '█']]
  (xmin, xmax), (ymin, ymax) = h.keys.transpose.map(&:minmax)
  (xmin..xmax).step(2).each do |x|
    puts (ymin..ymax).map{|y| chr[h[[x,y]]][h[[x+1,y]]]}.join
  end
end

[11, 122].each do |n|
  puts "\nN : #{n}"
  ulam_spiral(n)
end
```


{{out}}
<pre style="font-size:50%">
N : 11
▀   ▀▄ ▄   
▀▄▀▄   ▄▀ ▀
   ▄▀ █▄▀▄ 
▀▄▀ ▀▄     
▀▄▀   ▀▄  ▀
  ▀        

N : 122
▄    ▀      ▄ ▄▀ ▀              ▄▀       ▀           ▀▄  ▀  ▄  ▀       ▀  ▄   ▄ ▄      ▀          ▄  ▀   ▀  ▄    ▀   ▀  ▄ 
  ▄  ▀  ▄    ▀    ▄▀   ▀ ▀            ▄   ▄▀   ▀▄▀   ▀      ▄ ▄    ▀          ▄      ▀      ▄  ▀ ▀                     ▀  
        ▄ ▄      ▀   ▀▄    ▀    ▄▀    ▄  ▀    ▄▀    ▄     ▄▀  ▄       ▄     ▄▀   ▀ ▀     ▀  ▄▀    ▄  ▀     ▀   ▀▄   ▄ ▄  ▀
▄   ▄▀   ▀    ▄                  ▀ ▀  ▄           ▄   ▄  ▀     ▀ ▀   ▀    ▄   ▄     ▄       ▄  ▀   ▀                  ▄   
 ▀▄▀                      ▄    ▀    ▄       ▄  ▀      ▄            ▀   ▀▄▀         ▀ ▀▄     ▄  ▀▄     ▄          ▀        
▄▀ ▀   ▀       ▀       ▀              ▄     ▄▀           ▀    ▄       ▄▀     ▀  ▄▀▄▀    ▄    ▀       ▀  ▄     ▄▀▄  ▀   ▀ ▀
    ▄▀         ▀    ▄     ▄  ▀▄           ▄     ▄       ▄        ▀  ▄▀  ▄    ▀▄    ▀        ▄  ▀▄     ▄  ▀     ▀▄ ▄   ▄   
  ▄▀ ▀ ▀     ▀    ▄▀         ▀ ▀▄   ▄      ▀   ▀▄ ▄     ▄   ▄      ▀   ▀                 ▀▄ ▄    ▀    ▄▀          ▄       
   ▀      ▄ ▄▀▄▀    ▄  ▀   ▀ ▀    ▄▀  ▄     ▄  ▀         ▀▄   ▄▀      ▄   ▄  ▀  ▄       ▄   ▄      ▀     ▀ ▀       ▀▄ ▄  ▀
▄    ▀▄   ▄       ▄  ▀          ▄   ▄        ▀  ▄       ▄     ▄▀ ▀        ▄   ▄  ▀ ▀▄       ▄▀    ▄      ▀▄               
           ▀            ▄▀      ▄  ▀     ▀▄▀    ▄    ▀▄▀    ▄ ▄  ▀  ▄  ▀      ▄    ▀▄▀     ▀▄     ▄   ▄▀   ▀     ▀     ▀  
▄ ▄   ▄▀▄   ▄              ▀     ▀     ▀          ▄  ▀  ▄ ▄▀  ▄▀     ▀       ▀    ▄     ▄      ▀  ▄▀   ▀     ▀   ▀       ▀
     ▀          ▄     ▄▀     ▀▄       ▄   ▄        ▀    ▄▀ ▀  ▄▀     ▀           ▀ ▀   ▀  ▄  ▀  ▄   ▄ ▄ ▄ ▄ ▄ ▄▀    ▄     
           ▀ ▀ ▀     ▀ ▀      ▄      ▀          ▄▀    ▄▀    ▄    ▀  ▄               ▄     ▄    ▀     ▀     ▀     ▀ ▀     ▀
    ▄  ▀▄       ▄  ▀ ▀▄ ▄▀ ▀    ▄ ▄▀         ▀▄▀    ▄    ▀          ▄     ▄ ▄          ▀         ▀           ▀▄    ▀▄  ▀  
▄     ▄    ▀    ▄ ▄    ▀    ▄         ▄  ▀      ▄ ▄▀ ▀      ▄          ▀▄ ▄  ▀   ▀  ▄     ▄  ▀  ▄ ▄ ▄ ▄ ▄   ▄   ▄     ▄ ▄ 
  ▄    ▀ ▀     ▀ ▀ ▀         ▀  ▄  ▀ ▀          ▄▀▄  ▀      ▄    ▀ ▀     ▀   ▀  ▄  ▀  ▄  ▀▄           ▄▀                ▄▀
         ▀  ▄            ▀ ▀     ▀      ▄▀    ▄▀  ▄ ▄▀  ▄     ▄▀    ▄  ▀    ▄   ▄ ▄     ▄  ▀▄ ▄  ▀▄     ▄▀▄     ▄    ▀▄   
▄   ▄      ▀▄         ▄ ▄    ▀▄   ▄         ▄   ▄     ▄    ▀▄  ▀    ▄▀  ▄ ▄  ▀         ▀     ▀▄ ▄                 ▄ ▄     
        ▄▀ ▀ ▀ ▀ ▀   ▀    ▄  ▀ ▀   ▀     ▀▄▀▄          ▀      ▄  ▀ ▀▄  ▀ ▀▄              ▀▄     ▄          ▀       ▀    ▄ 
 ▀ ▀  ▄      ▀    ▄ ▄     ▄    ▀  ▄ ▄   ▄   ▄▀     ▀     ▀▄   ▄▀      ▄▀  ▄      ▀ ▀ ▀▄▀ ▀  ▄  ▀ ▀▄ ▄        ▀▄▀         ▀
    ▄ ▄                     ▄ ▄   ▄ ▄     ▄    ▀▄ ▄  ▀  ▄▀ ▀▄ ▄▀             ▀  ▄▀▄ ▄     ▄ ▄      ▀▄ ▄     ▄  ▀          
 ▀▄▀   ▀▄    ▀           ▀▄▀     ▀       ▀     ▀▄▀▄     ▄   ▄          ▀▄    ▀▄           ▄▀   ▀     ▀▄    ▀      ▄▀      
▄         ▄         ▄ ▄       ▄         ▄ ▄               ▄▀  ▄▀ ▀   ▀▄   ▄ ▄      ▀ ▀ ▀▄  ▀ ▀   ▀     ▀     ▀ ▀ ▀  ▄▀▄▀ ▀
▄   ▄▀    ▄            ▀▄    ▀    ▄     ▄▀▄   ▄▀▄  ▀  ▄ ▄     ▄▀ ▀  ▄  ▀        ▄           ▄▀     ▀                      
   ▀   ▀▄  ▀  ▄      ▀ ▀  ▄          ▀▄  ▀ ▀▄     ▄   ▄▀      ▄    ▀▄  ▀     ▀ ▀    ▄▀          ▄          ▀           ▀ ▀
        ▄   ▄         ▄   ▄   ▄      ▀▄▀▄ ▄ ▄▀   ▀  ▄     ▄▀   ▀ ▀  ▄▀▄▀ ▀▄     ▄ ▄     ▄  ▀            ▄    ▀▄  ▀    ▄▀  
     ▀           ▀     ▀                       ▀     ▀   ▀ ▀▄ ▄     ▄   ▄  ▀▄ ▄  ▀▄ ▄ ▄▀  ▄ ▄     ▄ ▄         ▄▀▄         
   ▀       ▀       ▀     ▀     ▀ ▀ ▀     ▀   ▀   ▀   ▀ ▀   ▀▄ ▄   ▄▀   ▀ ▀         ▀▄          ▀▄    ▀       ▀   ▀ ▀      
  ▄   ▄▀▄▀         ▀ ▀▄  ▀▄ ▄  ▀▄ ▄  ▀ ▀▄  ▀▄ ▄ ▄ ▄▀▄ ▄▀▄▀▄   ▄▀ ▀ ▀  ▄  ▀ ▀ ▀  ▄ ▄         ▄▀ ▀          ▄▀ ▀  ▄  ▀▄▀ ▀  
▄   ▄ ▄         ▄           ▄ ▄   ▄     ▄             ▄   ▄▀ █▄▀▄ ▄ ▄   ▄ ▄▀▄     ▄    ▀  ▄ ▄▀▄ ▄             ▄▀  ▄ ▄   ▄ 
     ▀   ▀    ▄▀         ▀ ▀ ▀  ▄    ▀ ▀       ▀       ▀▄▀ ▀▄                                                             
         ▀   ▀  ▄ ▄       ▄ ▄   ▄   ▄▀ ▀  ▄  ▀  ▄▀ ▀▄  ▀▄▀   ▀▄  ▀ ▀▄ ▄▀  ▄▀ ▀   ▀▄  ▀ ▀▄     ▄▀  ▄ ▄▀   ▀ ▀    ▄  ▀▄▀   ▀
     ▀                ▄▀▄    ▀▄    ▀    ▄ ▄         ▄ ▄  ▀  ▄   ▄   ▄▀      ▄ ▄  ▀    ▄      ▀▄   ▄     ▄   ▄  ▀  ▄  ▀    
  ▄  ▀   ▀ ▀        ▄▀ ▀ ▀ ▀             ▀ ▀▄    ▀▄  ▀  ▄   ▄ ▄  ▀       ▀     ▀         ▀     ▀           ▀     ▀        
   ▀▄  ▀  ▄  ▀▄   ▄ ▄▀  ▄   ▄    ▀▄ ▄       ▄ ▄ ▄ ▄    ▀ ▀  ▄▀▄    ▀▄  ▀  ▄ ▄▀  ▄▀▄▀ ▀▄       ▄▀  ▄ ▄   ▄ ▄    ▀▄▀ ▀      
    ▄ ▄     ▄    ▀    ▄ ▄     ▄     ▄     ▄   ▄▀▄  ▀  ▄     ▄  ▀▄▀▄           ▄     ▄   ▄ ▄ ▄     ▄ ▄         ▄   ▄       
 ▀      ▄          ▀   ▀  ▄  ▀ ▀ ▀ ▀ ▀▄    ▀ ▀       ▀▄ ▄   ▄ ▄▀       ▀▄    ▀▄            ▀   ▀      ▄     ▄     ▄▀    ▄ 
 ▀        ▄ ▄ ▄▀   ▀    ▄▀      ▄      ▀▄       ▄ ▄▀   ▀▄    ▀     ▀      ▄  ▀ ▀     ▀  ▄    ▀▄  ▀▄▀   ▀ ▀▄▀          ▄   
▄   ▄     ▄ ▄    ▀     ▀          ▄▀▄    ▀   ▀       ▀           ▀   ▀▄    ▀▄   ▄     ▄▀     ▀  ▄   ▄    ▀    ▄      ▀    
        ▄        ▀   ▀    ▄▀   ▀ ▀    ▄  ▀   ▀ ▀   ▀    ▄▀    ▄   ▄ ▄▀             ▀▄    ▀▄▀     ▀    ▄    ▀ ▀   ▀▄    ▀ ▀
  ▄    ▀▄▀     ▀  ▄      ▀      ▄ ▄ ▄▀     ▀▄     ▄   ▄▀▄   ▄  ▀    ▄     ▄    ▀   ▀     ▀        ▄  ▀ ▀▄    ▀▄ ▄▀        
      ▄    ▀          ▄     ▄      ▀    ▄            ▀▄     ▄   ▄ ▄     ▄   ▄▀   ▀▄               ▄ ▄ ▄ ▄   ▄         ▄ ▄ 
 ▀         ▀  ▄▀   ▀            ▄▀ ▀     ▀  ▄         ▄        ▀  ▄ ▄     ▄▀ ▀▄ ▄        ▀ ▀    ▄    ▀ ▀     ▀         ▀  
▄▀     ▀  ▄ ▄ ▄ ▄ ▄▀▄▀▄     ▄ ▄  ▀        ▄     ▄       ▄   ▄▀ ▀    ▄    ▀       ▀   ▀   ▀   ▀             ▀ ▀     ▀ ▀ ▀  
                  ▄         ▄           ▄▀▄   ▄          ▀  ▄    ▀▄  ▀▄▀   ▀ ▀▄         ▄    ▀▄    ▀    ▄▀▄ ▄  ▀▄   ▄     
       ▀ ▀    ▄     ▄    ▀▄▀ ▀   ▀     ▀ ▀  ▄▀     ▀ ▀           ▀  ▄▀        ▄     ▄ ▄  ▀           ▀▄▀    ▄    ▀▄       
        ▄      ▀▄     ▄  ▀ ▀        ▄        ▀     ▀  ▄▀▄    ▀▄   ▄▀                ▄▀ ▀    ▄    ▀  ▄▀   ▀▄▀  ▄ ▄  ▀▄ ▄  ▀
    ▄      ▀          ▄    ▀▄           ▄▀    ▄▀▄    ▀   ▀     ▀▄▀    ▄    ▀▄▀            ▄  ▀          ▄ ▄       ▄  ▀    
  ▄              ▀ ▀      ▄  ▀▄    ▀▄ ▄  ▀   ▀  ▄ ▄  ▀         ▀  ▄     ▄                   ▄  ▀▄            ▀         ▀  
   ▀▄  ▀▄ ▄  ▀  ▄   ▄▀        ▄▀                       ▀ ▀  ▄▀      ▄▀     ▀   ▀      ▄▀  ▄ ▄        ▀ ▀▄▀▄  ▀▄    ▀   ▀  
▄    ▀    ▄      ▀          ▄      ▀▄  ▀ ▀   ▀       ▀▄         ▄    ▀▄    ▀     ▀ ▀▄   ▄ ▄    ▀▄              ▀      ▄   
         ▀ ▀         ▀ ▀▄        ▀  ▄    ▀        ▄▀    ▄▀  ▄ ▄▀  ▄    ▀   ▀     ▀ ▀▄     ▄  ▀ ▀      ▄     ▄           ▄ 
 ▀ ▀▄    ▀  ▄▀ ▀    ▄         ▄  ▀    ▄    ▀ ▀        ▄▀▄▀   ▀    ▄▀            ▄   ▄ ▄▀     ▀   ▀▄     ▄    ▀       ▀   ▀
     ▀    ▄    ▀  ▄   ▄▀         ▀  ▄   ▄ ▄  ▀  ▄  ▀ ▀                ▄     ▄    ▀ ▀     ▀▄  ▀ ▀▄     ▄       ▄ ▄   ▄   ▄ 
 ▀ ▀        ▄       ▄      ▀ ▀           ▀▄    ▀▄ ▄▀     ▀  ▄       ▄   ▄  ▀ ▀▄       ▄  ▀▄    ▀   ▀  ▄            ▀     ▀
  ▄ ▄          ▀        ▄ ▄    ▀ ▀  ▄       ▄               ▄▀    ▄  ▀    ▄     ▄         ▄▀       ▀   ▀     ▀   ▀     ▀  
          ▄ ▄  ▀▄▀    ▄          ▀▄     ▄     ▄▀      ▄  ▀  ▄  ▀▄ ▄     ▄  ▀           ▀        ▄   ▄▀     ▀   ▀    ▄ ▄   
   ▀ ▀▄           ▄  ▀           ▀ ▀▄       ▄▀ ▀  ▄      ▀  ▄  ▀    ▄  ▀   ▀       ▀  ▄   ▄    ▀        ▄     ▄        ▀  
▄     ▄      ▀     ▀▄     ▄            ▀  ▄▀    ▄  ▀   ▀▄     ▄▀     ▀     ▀  ▄▀                  ▄         ▄▀▄▀    ▄  ▀  
         ▀  ▄  ▀▄     ▄     ▄▀▄     ▄  ▀▄▀▄  ▀                   ▀    ▄ ▄                           ▄▀    ▄▀▄  ▀▄    ▀▄   

```

The method of presentation of the result consulted " REXX ".


## Rust

{{trans|Kotlin}}
{{works with|Rust|1.11.0}}

```rust
use std::fmt;

enum Direction { RIGHT, UP, LEFT, DOWN }
use ulam::Direction::*;

/// Indicates whether an integer is a prime number or not.
fn is_prime(a: u32) -> bool {
    match a {
        2 => true,
        x if x <= 1 || x % 2 == 0 => false,
        _ => {
            let max = f64::sqrt(a as f64) as u32;
            let mut x =  3;
            while x <= max {
                if a % x == 0 { return false; }
                x += 2;
            }
            true
        }
    }
}

pub struct Ulam { u : Vec<Vec<String>> }

impl Ulam {
    /// Generates one `Ulam` object.
    pub fn new(n: u32, s: u32, c: char) -> Ulam {
        let mut spiral = vec![vec![String::new(); n as usize]; n as usize];
        let mut dir = RIGHT;
        let mut y = (n / 2) as usize;
        let mut x = if n % 2 == 0 { y - 1 } else { y }; // shift left for even n's
        for j in s..n * n + s {
            spiral[y][x] = if is_prime(j) {
                if c == '\0' { format!("{:4}", j) } else { format!("  {} ", c) }
            }
            else { String::from(" ---") };

            match dir {
                RIGHT => if x as u32 <= n - 1 && spiral[y - 1][x].is_empty() && j > s { dir = UP; },
                UP => if spiral[y][x - 1].is_empty() { dir = LEFT; },
                LEFT => if x == 0 || spiral[y + 1][x].is_empty() { dir = DOWN; },
                DOWN => if spiral[y][x + 1].is_empty() { dir = RIGHT; }
            };

            match dir { RIGHT => x += 1, UP => y -= 1, LEFT => x -= 1, DOWN => y += 1 };
        }
        Ulam { u: spiral }
    }
}

impl fmt::Display for Ulam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in &self.u {
            writeln!(f, "{}", format!("{:?}", row).replace("\"", "").replace(", ", ""));
        };
        writeln!(f, "")
    }
}
```

main.rs :

```rust
mod ulam;
use ulam::*;

// Program entry point.
fn main() {
    print!("{}", Ulam::new(9, 1, '\0'));
    print!("{}", Ulam::new(9, 1, '*'));
}
```

{{out}}

```txt
[ --- --- --- ---  61 ---  59 --- ---]
[ ---  37 --- --- --- --- ---  31 ---]
[  67 ---  17 --- --- ---  13 --- ---]
[ --- --- ---   5 ---   3 ---  29 ---]
[ --- ---  19 --- ---   2  11 ---  53]
[ ---  41 ---   7 --- --- --- --- ---]
[  71 --- --- ---  23 --- --- --- ---]
[ ---  43 --- --- ---  47 --- --- ---]
[  73 --- --- --- --- ---  79 --- ---]

[ --- --- --- ---  *  ---  *  --- ---]
[ ---  *  --- --- --- --- ---  *  ---]
[  *  ---  *  --- --- ---  *  --- ---]
[ --- --- ---  *  ---  *  ---  *  ---]
[ --- ---  *  --- ---  *   *  ---  * ]
[ ---  *  ---  *  --- --- --- --- ---]
[  *  --- --- ---  *  --- --- --- ---]
[ ---  *  --- --- ---  *  --- --- ---]
[  *  --- --- --- --- ---  *  --- ---]
```



## Scala

{{trans|Kotlin}}

```scala
object Ulam extends App {
    generate(9)()
    generate(9)('*')

    private object Direction extends Enumeration { val RIGHT, UP, LEFT, DOWN = Value }

    private def generate(n: Int, i: Int = 1)(c: Char = 0) {
        assert(n > 1, "n > 1")
        val s = new Array[Array[String]](n).transform {_ => new Array[String](n) }

        import Direction._
        var dir = RIGHT
        var y = n / 2
        var x = if (n % 2 == 0) y - 1 else y // shift left for even n's
        for (j <- i to n * n - 1 + i) {
            s(y)(x) = if (isPrime(j)) if (c == 0) "%4d".format(j) else s"  $c " else " ---"

            dir match {
                case RIGHT => if (x <= n - 1 && s(y - 1)(x) == null && j > i) dir = UP
                case UP => if (s(y)(x - 1) == null) dir = LEFT
                case LEFT => if (x == 0 || s(y + 1)(x) == null) dir = DOWN
                case DOWN => if (s(y)(x + 1) == null) dir = RIGHT
            }

            dir match {
                case RIGHT => x += 1
                case UP => y -= 1
                case LEFT => x -= 1
                case DOWN => y += 1
            }
        }
        println("[" + s.map(_.mkString("")).reduceLeft(_ + "]\n[" + _) + "]\n")
    }

    private def isPrime(a: Int): Boolean = {
        if (a == 2) return true
        if (a <= 1 || a % 2 == 0) return false
        val max = Math.sqrt(a.toDouble).toInt
        for (n <- 3 to max by 2)
            if (a % n == 0) return false
        true
    }
}
```



## Sidef

{{trans|Perl}}

```ruby
require('Imager')
 
var (n=512, start=1, file='ulam.png')

ARGV.getopt(
    'n=i' => \n,
    's=i' => \start,
    'f=s' => \file,
)
 
func cell(n, x, y, start) {
    y -= (n   >> 1)
    x -= (n-1 >> 1)
    var l = 2*(x.abs > y.abs ? x.abs : y.abs)
    var d = (y > x ? (l*3 + x + y) : (l - x - y))
    (l-1)**2 + d + start - 1
}
 
var black = %O<Imager::Color>.new('#000000')
var white = %O<Imager::Color>.new('#FFFFFF')
 
var img = %O<Imager>.new(xsize => n, ysize => n, channels => 1)
img.box(filled => 1, color => white)
 
for y=(^n), x=(^n) {
    if (cell(n, x, y, start).is_prime) {
        img.setpixel(x => x, y => y, color => black)
    }
}
 
img.write(file => file)
```

Output image: [https://github.com/trizen/rc/blob/master/img/ulam-spiral-sidef.png Ulam spiral]


## Tcl


This uses a coroutine to walk around the circle, laying glyphs every prime number of tiles.  Some more elaborate, interactive Tk GUIs for playing with Ulam spirals are at [http://wiki.tcl.tk/11363 Ulam Spiral] and [http://wiki.tcl.tk/23052 Ulam Spiral Demo] on the Tcl'ers Wiki.


```Tcl
proc is_prime {n} {
    if {$n == 1} {return 0}
    if {$n in {2 3 5}} {return 1}
    for {set i 2} {$i*$i <= $n} {incr i} {
        if {$n % $i == 0} {return 0}
    }
    return 1
}

proc spiral {w h} {
    yield [info coroutine]
    set x [expr {$w / 2}]
    set y [expr {$h / 2}]
    set n 1
    set dir 0
    set steps 1
    set step 1
    while {1} {
        yield [list $x $y]
        switch $dir {
            0   {incr x}
            1   {incr y -1}
            2   {incr x -1}
            3   {incr y}
        }
        if {![incr step -1]} {
            set dir [expr {($dir+1)%4}]
            if {$dir % 2 == 0} {
                incr steps
            }
            set step $steps
        }
    }
}

set radius 16
set side  [expr {1 + 2 * $radius}]
set n     [expr {$side * $side}]
set cells [lrepeat $side [lrepeat $side ""]]
set i     1

coroutine spin spiral $side $side

while {$i < $n} {
    lassign [spin] y x
    set c [expr {[is_prime $i] ? "\u169b" : " "}]
    lset cells $x $y $c
    incr i
}

puts [join [lmap row $cells {join $row " "}] \n]
```


The mark used is Unicode's OGHAM FEATHER MARK .. the closest I could find to a Tcl logo.
{{out}}

```txt
        ᚛   ᚛           ᚛       ᚛                       ᚛        
                              ᚛       ᚛   ᚛       ᚛              
                            ᚛       ᚛               ᚛       ᚛   ᚛
      ᚛       ᚛                       ᚛   ᚛           ᚛          
    ᚛   ᚛           ᚛   ᚛           ᚛           ᚛                
                      ᚛                       ᚛       ᚛          
᚛           ᚛       ᚛               ᚛           ᚛                
  ᚛       ᚛                   ᚛       ᚛   ᚛       ᚛   ᚛   ᚛      
᚛               ᚛           ᚛                   ᚛   ᚛       ᚛    
      ᚛           ᚛       ᚛   ᚛                               ᚛  
                                ᚛   ᚛           ᚛       ᚛       ᚛
  ᚛       ᚛       ᚛   ᚛       ᚛               ᚛       ᚛   ᚛      
                                ᚛   ᚛       ᚛                    
              ᚛       ᚛   ᚛           ᚛   ᚛   ᚛           ᚛   ᚛  
᚛   ᚛   ᚛   ᚛   ᚛   ᚛   ᚛   ᚛       ᚛               ᚛            
                              ᚛   ᚛   ᚛                       ᚛  
                    ᚛       ᚛     ᚛ ᚛   ᚛   ᚛   ᚛       ᚛   ᚛   ᚛
      ᚛               ᚛   ᚛   ᚛                                  
                        ᚛       ᚛                                
  ᚛       ᚛   ᚛       ᚛   ᚛       ᚛       ᚛   ᚛       ᚛       ᚛  
        ᚛       ᚛       ᚛           ᚛           ᚛   ᚛       ᚛    
                          ᚛                       ᚛              
                ᚛   ᚛           ᚛       ᚛       ᚛               ᚛
          ᚛       ᚛                       ᚛               ᚛      
᚛           ᚛           ᚛       ᚛   ᚛                            
                      ᚛   ᚛       ᚛           ᚛       ᚛          
᚛   ᚛   ᚛   ᚛                   ᚛   ᚛           ᚛           ᚛   ᚛
      ᚛       ᚛                       ᚛   ᚛                      
    ᚛   ᚛           ᚛           ᚛       ᚛   ᚛                    
  ᚛               ᚛                   ᚛               ᚛          
                    ᚛   ᚛       ᚛   ᚛                   ᚛        
              ᚛       ᚛           ᚛           ᚛                  
        ᚛   ᚛           ᚛                                   ᚛   
```



## VBScript


```vb

Function build_spiral(n)
	'declare a two dimentional array
	Dim matrix()
	ReDim matrix(n-1,n-1)
	'determine starting point
	x = (n-1)/2 : y = (n-1)/2
	'set the initial iterations
	x_max = 1 : y_max = 1 : count = 1
	'set initial direction
	dir = "R"
	'populate the array
	For i = 1 To n*n
		l = Len(n*n)
		If IsPrime(i) Then
			matrix(x,y) = Right("000" & i,l)
		Else
			matrix(x,y) = String(l,"-")
		End If
		Select Case dir
			Case "R"
				If x_max > 0 Then
					x = x + 1 : x_max = x_max - 1
				Else
					dir = "U" : y_max = count
					y = y - 1 : y_max = y_max - 1
				End If
			Case "U"
				If y_max > 0 Then
					y = y - 1 : y_max = y_max - 1
				Else
					dir = "L" : count = count + 1 : x_max = count
					x = x - 1 : x_max = x_max - 1
				End If
			Case "L"
				If x_max > 0 Then
					x = x - 1 : x_max = x_max - 1
				Else
					dir = "D" : y_max = count
					y = y + 1 : y_max = y_max - 1
				End If
			Case "D"
				If y_max > 0 Then
					y = y + 1 : y_max = y_max - 1
				Else
					dir = "R" : count = count + 1 : x_max = count
					x = x + 1 : x_max = x_max - 1
				End If
		End Select
	Next
	'print the matrix
	For y = 0 To n - 1
		For x = 0 To n - 1
			If x = n - 1 Then
				WScript.StdOut.Write matrix(x,y)
			Else
				WScript.StdOut.Write matrix(x,y) & vbTab
			End If
		Next
		WScript.StdOut.WriteLine
	Next
End Function

Function IsPrime(n)
	If n = 2 Then
		IsPrime = True
	ElseIf n <= 1 Or n Mod 2 = 0 Then
		IsPrime = False
	Else
		IsPrime = True
		For i = 3 To Int(Sqr(n)) Step 2
			If n Mod i = 0 Then
				IsPrime = False
				Exit For
			End If
		Next
	End If
End Function

'test with 9
build_spiral(9)

```


{{Out}}

```txt

--	--	--	--	61	--	59	--	--
--	37	--	--	--	--	--	31	--
67	--	17	--	--	--	13	--	--
--	--	--	05	--	03	--	29	--
--	--	19	--	--	02	11	--	53
--	41	--	07	--	--	--	--	--
71	--	--	--	23	--	--	--	--
--	43	--	--	--	47	--	--	--
73	--	--	--	--	--	79	--	--

```



## Yabasic

{{trans|Phix}}

```Yabasic
sub is_prime(n)
    local p
    
    for p=2 to n
        if p*p>n break
        if mod(n,p)=0 return false
    next
    return n>=2
end sub
 
sub spiral(w, h, x, y)
    if y then
        return w+spiral(h-1,w,y-1,w-x-1)
    else
        return x
    end if
end sub
 
w = 9 : h = 9
for i=h-1 to 0 step -1
    for j=w-1 to 0 step -1
        p = w*h-spiral(w,h,j,i)
        print mid$(" o", is_prime(p) + 1, 1);
    next
    print
next
```



## zkl

Simulates turtle graphics, spiral by walking straight while holding left hand against the wall dropping prime breadcrumbs.

Using [[Extensible prime generator#zkl]] and the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl.

```zkl
var primes =Utils.Generator(Import("sieve.zkl").postponed_sieve);  // lazy
var offsets=Utils.cycle( T(0,1),T(-1,0),T(0,-1),T(1,0) );  // (N,E,S,W), lazy
const BLACK=0, WHITE=0xff|ff|ff, GREEN=0x00|ff|00, EMPTY=0x080|80|80;
fcn uspiral(N){
   if((M:=N).isEven) M+=1;  // need odd width, height
   img,p := PPM(M,M,EMPTY), primes.next(); // 2 .. 250,007: 22,045 primes
   x,y,n := N/2,x,2; img[x,y]=GREEN; x+=1; // start on 2 facing "north"
   while(True){
      ox,oy:=offsets.next(); leftx,lefty:=offsets.peek();  // set direction
      while(True){
	 img[x,y]=( if(n==p){ p=primes.next(); WHITE } else BLACK );
	 if(n==N*N) break(2); // all done
	 n+=1;
	 if(img[x+leftx,y+lefty]==EMPTY) // nothing to my left, turn left
	    { x+=leftx; y+=lefty; break; }
	 x+=ox; y+=oy;	// move in a straight line
      }
   }
   img
}

uspiral(500).write(File("ulamSpiral.ppm","wb"));
```

{{out}}
A PPM image similar to that shown in Perl6 but denser. A green dot marks the center.

http://www.zenkinetic.com/Images/RosettaCode/ulamSpiral.png
