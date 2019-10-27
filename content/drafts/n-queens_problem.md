+++
title = "N-queens problem"
description = ""
date = 2019-10-21T23:51:15Z
aliases = []
[extra]
id = 4838
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[File:chess_queen.jpg|400px||right]]

[[File:N_queens_problem.png|400px||right]]

Solve the [[WP:Eight_queens_puzzle|eight queens puzzle]]. 


You can extend the problem to solve the puzzle with a board of size   <big>'''N'''x'''N'''</big>.
 
For the number of solutions for small values of   '''N''',   see   [http://oeis.org/A000170 oeis.org sequence A170].


;Related tasks:
* [[A* search algorithm]]
* [[Solve a Hidato puzzle]]
* [[Solve a Holy Knight's tour]]
* [[Knight's tour]]
* [[Peaceful chess queen armies]]
* [[Solve a Hopido puzzle]]
* [[Solve a Numbrix puzzle]]
* [[Solve the no connection puzzle]]






## 360 Assembly

{{trans|FORTRAN}}
Translated from the Fortran 77 solution.

For maximum compatibility, this program uses only the basic instruction set (S/360).

```360asm
*        N-QUEENS PROBLEM          04/09/2015              
         MACRO 
&LAB     XDECO  &REG,&TARGET
&LAB     B      I&SYSNDX           branch around work area
P&SYSNDX DS     0D,PL8             packed
W&SYSNDX DS     CL13               char
I&SYSNDX CVD    &REG,P&SYSNDX          convert to decimal
         MVC    W&SYSNDX,=X'40202020202020202020212060'  nice mask
         EDMK   W&SYSNDX,P&SYSNDX+2    edit and mark
         BCTR   R1,0                   locate the right place
         MVC    0(1,R1),W&SYSNDX+12    move the sign
         MVC    &TARGET.(12),W&SYSNDX  move to target
         MEND
NQUEENS  CSECT
         SAVE   (14,12)            save registers on entry
         BALR   R12,0              establish addressability
         USING  *,R12              set base register
         ST     R13,SAVEA+4        link mySA->prevSA
         LA     R11,SAVEA          mySA
         ST     R11,8(R13)         link prevSA->mySA
         LR     R13,R11            set mySA pointer
         LA     R7,LL              l
         LA     R6,1               i=1
LOOPI    LR     R1,R6              do i=1 to l
         SLA    R1,1               i*2
         STH    R6,A-2(R1)         a(i)=i
         LA     R6,1(R6)           i=i+1
         BCT    R7,LOOPI           loop do i
OPENEM   OPEN   (OUTDCB,OUTPUT)    open the printer file
         LA     R9,1               n=1 start of loop
LOOPN    CH     R9,L               do n=1 to l
         BH     ELOOPN             if n>l then exit loop
         SR     R8,R8              m=0
         LA     R10,1              i=1
         LR     R5,R9              n
         SLA    R5,1               n*2
         BCTR   R5,0               r=2*n-1
E40      CR     R10,R9             if i>n
         BH     E80                then goto e80
         LR     R11,R10            j=i
E50      LR     R1,R10             i
         SLA    R1,1               i*2
         LA     R6,A-2(R1)         r6=@a(i)
         LR     R1,R11             j
         SLA    R1,1               j*2
         LA     R7,A-2(R1)         r7=@a(j)
         MVC    Z,0(R6)            z=a(i)
         MVC    Y,0(R7)            y=a(j)
         LR     R3,R10             i
         SH     R3,Y               -y
         AR     R3,R9              p=i-y+n
         LR     R4,R10             i
         AH     R4,Y               +y
         BCTR   R4,0               q=i+y-1
         MVC    0(2,R6),Y          a(i)=y
         MVC    0(2,R7),Z          a(j)=z
         LR     R1,R3              p
         SLA    R1,1               p*2
         LH     R2,U-2(R1)         u(p)
         LTR    R2,R2              if u(p)<>0
         BNE    E60                then goto e60
         LR     R1,R4              q
         AR     R1,R5              q+r
         SLA    R1,1               (q+r)*2
         LH     R2,U-2(R1)         u(q+r)
         C      R2,=F'0'           if u(q+r)<>0
         BNE    E60                then goto e60
         LR     R1,R10             i
         SLA    R1,1               i*2
         STH    R11,S-2(R1)        s(i)=j
         LA     R0,1               r0=1
         LR     R1,R3              p
         SLA    R1,1               p*2
         STH    R0,U-2(R1)         u(p)=1
         LR     R1,R4              q
         AR     R1,R5              q+r
         SLA    R1,1               (q+r)*2
         STH    R0,U-2(R1)         u(q+r)=1
         LA     R10,1(R10)         i=i+1
         B      E40                goto e40
E60      LA     R11,1(R11)         j=j+1
         CR     R11,R9             if j<=n
         BNH    E50                then goto e50
E70      BCTR   R11,0              j=j-1
         CR     R11,R10            if j=i
         BE     E90                goto e90
         LR     R1,R10             i
         SLA    R1,1               i*2
         LA     R6,A-2(R1)         r6=@a(i)
         LR     R1,R11             j
         SLA    R1,1               j*2
         LA     R7,A-2(R1)         r7=@a(j)
         MVC    Z,0(R6)            z=a(i)
         MVC    0(2,R6),0(R7)      a(i)=a(j)
         MVC    0(2,R7),Z          a(j)=z;
         B      E70                goto e70
E80      LA     R8,1(R8)           m=m+1
E90      BCTR   R10,0              i=i-1
         LTR    R10,R10            if i=0
         BZ     ZERO               then goto zero
         LR     R1,R10             i
         SLA    R1,1               i*2
         LH     R2,A-2(R1)         r2=a(i)
         LR     R3,R10             i
         SR     R3,R2              -a(i)
         AR     R3,R9              p=i-a(i)+n
         LR     R4,R10             i
         AR     R4,R2              +a(i)
         BCTR   R4,0               q=i+a(i)-1
         LR     R1,R10             i
         SLA    R1,1               i*2
         LH     R11,S-2(R1)        j=s(i)
         LA     R0,0               r0=0
         LR     R1,R3              p
         SLA    R1,1               p*2
         STH    R0,U-2(R1)         u(p)=0
         LR     R1,R4              q
         AR     R1,R5              q+r
         SLA    R1,1               (q+r)*2
         STH    R0,U-2(R1)         u(q+r)=0
         B      E60                goto e60
ZERO     XDECO  R9,PG+0            edit N
         XDECO  R8,PG+12           edit M
         PUT    OUTDCB,PG          print buffer
         LA     R9,1(R9)           n=n+1
         B      LOOPN              loop do n
ELOOPN   CLOSE  (OUTDCB)           close output 
         L      R13,SAVEA+4        previous save area addrs
         RETURN (14,12),RC=0       return to caller with rc=0
         LTORG
SAVEA    DS     18F                save area for chaining
OUTDCB   DCB    DSORG=PS,MACRF=PM,DDNAME=OUTDD  use OUTDD in jcl
LL       EQU    14                 ll<=16
L        DC     AL2(LL)            input value
A        DS     (LL)H
S        DS     (LL)H 
Z        DS     H
Y        DS     H
PG       DS     CL24               buffer
U        DC     (4*LL-2)H'0'       stack
         REGS                      make sure to include copybook jcl 
         END    NQUEENS
```

{{out}}

```txt

           1           1
           2           0
           3           0
           4           2
           5          10
           6           4
           7          40
           8          92
           9         352
          10         724
          11        2680
          12       14200
          13       47600
          14      365596

```


=={{header|PDP-11 Assembly}}==
<lang PDP-11 Assembly>
; "eight queens problem" benchmark test

         .radix    16

         .loc 0

          nop            ;
          mov  #scr,@#E800
          mov  #88C6,@#E802
; clear the display RAM
          mov  #scr,r0
          mov  #1E0,r1
cls:      clr  (r0)+
          sob  r1,cls
; display the initial counter value
          clr  r3
          mov  #scr,r0
          jsr  pc,number
; perform the test
          jsr  pc,queens
; display the counter
          mov  #scr,r0
          jsr  pc,number
finish:   br   finish

; display the character R1 at the screen address R0,
; advance the pointer R0 to the next column
putc:     mov  r2,-(sp)
; R1 <- 6 * R1
         asl  r1        ;* 2
         mov  r1,-(sp)
         asl  r1        ;* 4
         add  (sp)+,r1  ;* 6
         add  #chars,r1
         mov  #6,r2
putc1:   movb (r1)+,(r0)
         add  #1E,r0
         sob  r2,putc1
         sub  #B2,r0         ;6 * 1E - 2 = B2
         mov  (sp)+,r2
         rts  pc

print1:   jsr  pc,putc
; print a string pointed to by R2 at the screen address R0,
; advance the pointer R0 to the next column,
; the string should be terminated by a negative byte
print:    movb (r2)+,r1
          bpl  print1
          rts  pc

; display the word R3 decimal at the screen address R0
number:   mov  sp,r1
          mov  #A0A,-(sp)
          mov  (sp),-(sp)
          mov  (sp),-(sp)
          movb #80,-(r1)
numb1:    clr  r2
          div  #A,r2
          movb r3,-(r1)
          mov  r2,r3
          bne  numb1
          mov  sp,r2
          jsr  pc,print
          add  #6,sp
          rts  pc

queens:  mov  #64,r5         ;100
l06:     clr  r3
         clr  r0
l00:     cmp  #8,r0
         beq  l05
         inc  r0
         movb #8,ary(r0)
l01:     inc  r3
         mov  r0,r1
l02:     dec  r1
         beq  l00
         movb ary(r0),r2
         movb ary(r1),r4
         sub  r2,r4
         beq  l04
         bcc  l03
         neg  r4
l03:     add  r1,r4
         sub  r0,r4
         bne  l02
l04:     decb ary(r0)
         bne  l01
         sob  r0,l04
l05:     sob  r5,l06
         mov  r3,cnt
         rts  pc

; characters, width = 8 pixels, height = 6 pixels
chars:  .byte     3C, 46, 4A, 52, 62, 3C   ;digit '0'
        .byte     18, 28, 8,  8,  8,  3E   ;digit '1'
        .byte     3C, 42, 2,  3C, 40, 7E   ;digit '2'
        .byte     3C, 42, C,  2,  42, 3C   ;digit '3'
        .byte     8,  18, 28, 48, 7E, 8    ;digit '4'
        .byte     7E, 40, 7C, 2,  42, 3C   ;digit '5'
        .byte     3C, 40, 7C, 42, 42, 3C   ;digit '6'
        .byte     7E, 2,  4,  8,  10, 10   ;digit '7'
        .byte     3C, 42, 3C, 42, 42, 3C   ;digit '8'
        .byte     3C, 42, 42, 3E, 2,  3C   ;digit '9'
        .byte     0,  0,  0,  0,  0,  0    ;space

        .even

cnt:    .blkw     1
ary:    .blkb     9

        .loc 200

scr:                                        ;display RAM

```



## ABAP


```ABAP

TYPES: BEGIN OF gty_matrix,
         1  TYPE c,
         2  TYPE c,
         3  TYPE c,
         4  TYPE c,
         5  TYPE c,
         6  TYPE c,
         7  TYPE c,
         8  TYPE c,
         9  TYPE c,
         10 TYPE c,
       END OF gty_matrix,
       gty_t_matrix TYPE STANDARD TABLE OF gty_matrix INITIAL SIZE 8.

DATA: gt_matrix TYPE gty_t_matrix,
      gs_matrix TYPE gty_matrix,
      gv_count  TYPE i VALUE 0,
      gv_solut  TYPE i VALUE 0.


SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
PARAMETERS: p_number TYPE i OBLIGATORY DEFAULT 8.
SELECTION-SCREEN END OF BLOCK b01.

" Filling empty table
START-OF-SELECTION.
  DO p_number TIMES.
    APPEND gs_matrix TO gt_matrix.
  ENDDO.

" Recursive Function
  PERFORM fill_matrix USING gv_count 1 1 CHANGING gt_matrix.
  BREAK-POINT.
*&---------------------------------------------------------------------*
*&      Form  FILL_MATRIX
*----------------------------------------------------------------------*
FORM fill_matrix  USING    p_count TYPE i
                           p_i     TYPE i
                           p_j     TYPE i
                  CHANGING p_matrix TYPE gty_t_matrix.

  DATA: lv_i      TYPE i,
        lv_j      TYPE i,
        lv_result TYPE c LENGTH 1,
        lt_matrix TYPE gty_t_matrix,
        lv_count  TYPE i,
        lv_value  TYPE c.

  lt_matrix[] = p_matrix[].
  lv_count = p_count.
  lv_i = p_i.
  lv_j = p_j.

  WHILE lv_i LE p_number.
    WHILE lv_j LE p_number.
      CLEAR lv_result.
      PERFORM check_position USING lv_i lv_j CHANGING lv_result lt_matrix.
      IF lv_result NE 'X'.
        MOVE 'X' TO lv_value.
        PERFORM get_position USING lv_i lv_j 'U' CHANGING lv_value lt_matrix.
        ADD 1 TO lv_count.
        IF lv_count EQ p_number.
          PERFORM show_matrix USING lt_matrix.
        ELSE.
          PERFORM fill_matrix USING lv_count lv_i lv_j CHANGING lt_matrix.
        ENDIF.
        lv_value = space.
        PERFORM get_position USING lv_i lv_j 'U' CHANGING lv_value lt_matrix.
        SUBTRACT 1 FROM lv_count.
      ENDIF.
      ADD 1 TO lv_j.
    ENDWHILE.
    ADD 1 TO lv_i.
    lv_j = 1.
  ENDWHILE.
ENDFORM.                    " FILL_MATRIX

*&---------------------------------------------------------------------*
*&      Form  CHECK_POSITION
*&---------------------------------------------------------------------*
FORM check_position  USING value(p_i)  TYPE i
                           value(p_j)  TYPE i
                     CHANGING p_result TYPE c
                              p_matrix TYPE gty_t_matrix.

  PERFORM get_position USING p_i p_j 'R' CHANGING p_result p_matrix.
  CHECK p_result NE 'X'.

  PERFORM check_horizontal USING p_i p_j CHANGING p_result p_matrix.
  CHECK p_result NE 'X'.

  PERFORM check_vertical USING p_i p_j CHANGING p_result p_matrix.
  CHECK p_result NE 'X'.

  PERFORM check_diagonals USING p_i p_j CHANGING p_result p_matrix.

ENDFORM.                    " CHECK_POSITION

*&---------------------------------------------------------------------*
*&      Form  GET_POSITION
*&---------------------------------------------------------------------*
FORM get_position  USING value(p_i)      TYPE i
                         value(p_j)      TYPE i
                         value(p_action) TYPE c
                      CHANGING p_result  TYPE c
                               p_matrix  TYPE gty_t_matrix.

  FIELD-SYMBOLS: <fs_lmatrix> TYPE gty_matrix,
                 <fs_lfield> TYPE any.

  READ TABLE p_matrix ASSIGNING <fs_lmatrix> INDEX p_i.
  ASSIGN COMPONENT p_j OF STRUCTURE <fs_lmatrix> TO <fs_lfield>.

  CASE p_action.
    WHEN 'U'.
      <fs_lfield> = p_result.
    WHEN 'R'.
      p_result = <fs_lfield>.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " GET_POSITION

*&---------------------------------------------------------------------*
*&      Form  CHECK_HORIZONTAL
*&---------------------------------------------------------------------*
FORM check_horizontal  USING value(p_i)      TYPE i
                             value(p_j)      TYPE i
                          CHANGING p_result  TYPE c
                                   p_matrix  TYPE gty_t_matrix.
  DATA: lv_j TYPE i,
        ls_matrix TYPE gty_matrix.

  FIELD-SYMBOLS <fs> TYPE c.

  lv_j = 1.
  READ TABLE p_matrix INTO ls_matrix INDEX p_i.
  WHILE lv_j LE p_number.
    ASSIGN COMPONENT lv_j OF STRUCTURE ls_matrix TO <fs>.
    IF <fs> EQ 'X'.
      p_result = 'X'.
      RETURN.
    ENDIF.
    ADD 1 TO lv_j.
  ENDWHILE.
ENDFORM.                    " CHECK_HORIZONTAL

*&---------------------------------------------------------------------*
*&      Form  CHECK_VERTICAL
*&---------------------------------------------------------------------*
FORM check_vertical  USING value(p_i)      TYPE i
                           value(p_j)      TYPE i
                        CHANGING p_result  TYPE c
                                 p_matrix  TYPE gty_t_matrix.
  DATA: lv_i TYPE i,
        ls_matrix TYPE gty_matrix.

  FIELD-SYMBOLS <fs> TYPE c.

  lv_i = 1.
  WHILE lv_i LE p_number.
    READ TABLE p_matrix INTO ls_matrix INDEX lv_i.
    ASSIGN COMPONENT p_j OF STRUCTURE ls_matrix TO <fs>.
    IF <fs> EQ 'X'.
      p_result = 'X'.
      RETURN.
    ENDIF.
    ADD 1 TO lv_i.
  ENDWHILE.
ENDFORM.                    " CHECK_VERTICAL

*&---------------------------------------------------------------------*
*&      Form  CHECK_DIAGONALS
*&---------------------------------------------------------------------*
FORM check_diagonals  USING value(p_i)      TYPE i
                            value(p_j)      TYPE i
                         CHANGING p_result  TYPE c
                                  p_matrix  TYPE gty_t_matrix.
  DATA: lv_dx TYPE i,
        lv_dy TYPE i.

* I++ J++ (Up Right)
  lv_dx = 1.
  lv_dy = 1.
  PERFORM check_diagonal USING p_i p_j lv_dx lv_dy CHANGING p_result p_matrix.
  CHECK p_result NE 'X'.

* I-- J-- (Left Down)
  lv_dx = -1.
  lv_dy = -1.
  PERFORM check_diagonal USING p_i p_j lv_dx lv_dy CHANGING p_result p_matrix.
  CHECK p_result NE 'X'.

* I++ J-- (Right Down)
  lv_dx = 1.
  lv_dy = -1.
  PERFORM check_diagonal USING p_i p_j lv_dx lv_dy CHANGING p_result p_matrix.
  CHECK p_result NE 'X'.

* I-- J++ (Left Up)
  lv_dx = -1.
  lv_dy = 1.
  PERFORM check_diagonal USING p_i p_j lv_dx lv_dy CHANGING p_result p_matrix.
  CHECK p_result NE 'X'.
ENDFORM.                    " CHECK_DIAGONALS

*&---------------------------------------------------------------------*
*&      Form  CHECK_DIAGONAL
*&---------------------------------------------------------------------*
FORM check_diagonal  USING value(p_i)      TYPE i
                            value(p_j)      TYPE i
                            value(p_dx)      TYPE i
                            value(p_dy)      TYPE i
                         CHANGING p_result  TYPE c
                                  p_matrix  TYPE gty_t_matrix.
  DATA: lv_i TYPE i,
        lv_j TYPE i,
        ls_matrix TYPE gty_matrix.

  FIELD-SYMBOLS <fs> TYPE c.

  lv_i = p_i.
  lv_j = p_j.
  WHILE 1 EQ 1.
    ADD: p_dx TO lv_i, p_dy TO lv_j.

    IF p_dx EQ 1.
      IF lv_i GT p_number. EXIT. ENDIF.
    ELSE.
      IF lv_i LT 1. EXIT. ENDIF.
    ENDIF.

    IF p_dy EQ 1.
      IF lv_j GT p_number. EXIT. ENDIF.
    ELSE.
      IF lv_j LT 1. EXIT. ENDIF.
    ENDIF.

    READ TABLE p_matrix INTO ls_matrix INDEX lv_i.
    ASSIGN COMPONENT lv_j OF STRUCTURE ls_matrix TO <fs>.
    IF <fs> EQ 'X'.
      p_result = 'X'.
      RETURN.
    ENDIF.
  ENDWHILE.
ENDFORM.                    " CHECK_DIAGONAL
*&---------------------------------------------------------------------*
*&      Form  SHOW_MATRIX
*----------------------------------------------------------------------*
FORM show_matrix USING p_matrix TYPE gty_t_matrix.
  DATA: lt_matrix TYPE gty_t_matrix,
        lv_j      TYPE i VALUE 1,
        lv_colum  TYPE string VALUE '-'.

  FIELD-SYMBOLS: <fs_matrix> TYPE gty_matrix,
                 <fs_field>  TYPE c.

  ADD 1 TO gv_solut.

  WRITE:/ 'Solution: ', gv_solut.

  DO p_number TIMES.
    CONCATENATE lv_colum '----' INTO lv_colum.
  ENDDO.

  LOOP AT p_matrix ASSIGNING <fs_matrix>.
    IF sy-tabix EQ 1.
      WRITE:/ lv_colum.
    ENDIF.
    WRITE:/ '|'.
    DO p_number TIMES.
      ASSIGN COMPONENT lv_j OF STRUCTURE <fs_matrix> TO <fs_field>.
      IF <fs_field> EQ space.
        WRITE: <fs_field> ,'|'.
      ELSE.
        WRITE: <fs_field> COLOR 2 HOTSPOT ON,'|'.
      ENDIF.
      ADD 1 TO lv_j.
    ENDDO.
    lv_j = 1.
    WRITE: / lv_colum.
  ENDLOOP.

  SKIP 1.
ENDFORM.                    " SHOW_MATRIX

```



## Ada


```Ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Queens is
   Board : array (1..8, 1..8) of Boolean := (others => (others => False));
   function Test (Row, Column : Integer) return Boolean is
   begin
      for J in 1..Column - 1 loop
         if (  Board (Row, J)
            or else
               (Row > J and then Board (Row - J, Column - J))
            or else
               (Row + J <= 8 and then Board (Row + J, Column - J))
            )  then
            return False;
         end if;
      end loop;
      return True;
   end Test;
   function Fill (Column : Integer) return Boolean is
   begin
      for Row in Board'Range (1) loop
         if Test (Row, Column) then
            Board (Row, Column) := True;
            if Column = 8 or else Fill (Column + 1) then
               return True;
            end if;
            Board (Row, Column) := False;
         end if;
      end loop;
      return False;
   end Fill;
begin
   if not Fill (1) then
      raise Program_Error;
   end if;
   for I in Board'Range (1) loop
      Put (Integer'Image (9 - I));
      for J in Board'Range (2) loop
         if Board (I, J) then
            Put ("|Q");
         elsif (I + J) mod 2 = 1 then
            Put ("|/");
         else
            Put ("| ");
         end if;
      end loop;
      Put_Line ("|");
   end loop;
   Put_Line ("   A B C D E F G H");
end Queens;
```

{{out}}

```txt

 8|Q|/| |/| |/| |/|
 7|/| |/| |/| |Q| |
 6| |/| |/|Q|/| |/|
 5|/| |/| |/| |/|Q|
 4| |Q| |/| |/| |/|
 3|/| |/|Q|/| |/| |
 2| |/| |/| |Q| |/|
 1|/| |Q| |/| |/| |
   A B C D E F G H

```



###  Alternate solution 

{{trans|Fortran}}

This one only counts solutions, though it's easy to do something else with each one (instead of the <code>M := M + 1;</code> line).


```ada
with Ada.Text_IO;
use Ada.Text_IO;

procedure CountQueens is
    function Queens (N : Integer) return Long_Integer is
        A : array (0 .. N) of Integer;
        U : array (0 .. 2 * N - 1) of Boolean := (others => true);
        V : array (0 .. 2 * N - 1) of Boolean := (others => true);
        M : Long_Integer := 0;
        
        procedure Sub (I: Integer) is
            K, P, Q: Integer;
        begin
            if N = I then
                M := M + 1;
            else
                for J in I .. N - 1 loop
                    P := I + A (J);
                    Q := I + N - 1 - A (J);
                    if U (P) and then V (Q) then
                        U (P) := false;
                        V (Q) := false;
                        K := A (I);
                        A (I) := A (J);
                        A (J) := K;
                        Sub (I + 1);
                        U (P) := true;
                        V (Q) := true;
                        K := A (I);
                        A (I) := A (J);
                        A (J) := K;
                    end if;
                end loop;
            end if;
        end Sub;
    begin
        for I in 0 .. N - 1 loop
            A (I) := I;
        end loop;
        Sub (0);
        return M;
    end Queens;
begin
    for N in 1 .. 16 loop
        Put (Integer'Image (N));
        Put (" ");
        Put_Line (Long_Integer'Image (Queens (N)));
    end loop;
end CountQueens;
```



## ALGOL 68

{{trans|C}}

{{works with|ALGOL 68|Standard - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}} 

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8.8d.fc9.i386]}}

```Algol68
INT ofs = 1, # Algol68 normally uses array offset of 1 #
    dim = 8; # dim X dim chess board #
[ofs:dim+ofs-1]INT b;

PROC unsafe = (INT y)BOOL:(
  INT i, t, x;
  x := b[y];
  FOR i TO y - LWB b DO
    t := b[y - i];
    IF t = x THEN break true
    ELIF t = x - i THEN break true
    ELIF t = x + i THEN break true
    FI
  OD;
  FALSE EXIT
break true:
  TRUE
);
 
INT s := 0;

PROC print board = VOID:(
  INT x, y;
  print((new line, "Solution # ", s+:=1, new line));
  FOR y FROM LWB b TO UPB b DO 
    FOR x FROM LWB b TO UPB b DO
      print("|"+(b[y]=x|"Q"|: ODD(x+y)|"/"|" "))
    OD;
    print(("|", new line))
  OD
);
 
main: (
  INT y := LWB b;
  b[LWB b] := LWB b - 1;
  FOR i WHILE y >= LWB b DO
    WHILE
      b[y]+:=1;
  # BREAK # IF b[y] <= UPB b THEN unsafe(y) ELSE FALSE FI 
    DO SKIP OD;
    IF b[y] <= UPB b  THEN
      IF y < UPB b THEN
        b[y+:=1] := LWB b - 1
      ELSE
        print board
      FI
    ELSE
      y-:=1
    FI
  OD
)
```



## Dyalog APL

More or less copied from the "DFS" lesson on tryapl.org .

```APL

⍝Solution
accm←{⍺,((⍴⍵)=⍴⊃⍺)↑⊂⍵}               
atk←{∪∊(⊂⍵)+¯1 0 1×⊂⌽⍳⍴⍵}            
dfs←{⊃∇⍨/⌽(⊂⍺ ⍺⍺ ⍵),⍺ ⍵⍵ ⍵}          
qfmt←{⍵∘.=⍳⍴⍵}      
subs←{(⊂⍵),¨(⍳⍴⊃⍺)~atk ⍵}                 
queens←{qfmt¨(↓0 ⍵⍴0)accm dfs subs ⍬}
printqueens←{i←1⋄{⎕←'answer'i⋄⎕←⍵⋄i+←1}¨queens ⍵}

⍝Example
printqueens 6

```

{{out}}

```txt

 answer  1
0 1 0 0 0 0
0 0 0 1 0 0
0 0 0 0 0 1
1 0 0 0 0 0
0 0 1 0 0 0
0 0 0 0 1 0
 answer  2
0 0 1 0 0 0
0 0 0 0 0 1
0 1 0 0 0 0
0 0 0 0 1 0
1 0 0 0 0 0
0 0 0 1 0 0
 answer  3
0 0 0 1 0 0
1 0 0 0 0 0
0 0 0 0 1 0
0 1 0 0 0 0
0 0 0 0 0 1
0 0 1 0 0 0
 answer  4
0 0 0 0 1 0
0 0 1 0 0 0
1 0 0 0 0 0
0 0 0 0 0 1
0 0 0 1 0 0
0 1 0 0 0 0

```



## AppleScript


```applescript
-- Finds all possible solutions and the unique patterns.

property Grid_Size : 8

property Patterns : {}
property Solutions : {}
property Test_Count : 0

property Rotated : {}

on run
    local diff
    local endTime
    local msg
    local rows
    local startTime
    
    set Patterns to {}
    set Solutions to {}
    set Rotated to {}
    
    set Test_Count to 0
    
    set rows to Make_Empty_List(Grid_Size)
    
    set startTime to current date
    Solve(1, rows)
    set endTime to current date
    set diff to endTime - startTime
    
    set msg to ("Found " & (count Solutions) & " solutions with " & (count Patterns) & " patterns in " & diff & " seconds.") as text
    display alert msg
    
    return Solutions
end run

on Solve(row as integer, rows as list)
    if row is greater than (count rows) then
        Append_Solution(rows)
        return
    end if
    
    repeat with column from 1 to Grid_Size
        set Test_Count to Test_Count + 1
        if Place_Queen(column, row, rows) then
            Solve(row + 1, rows)
        end if
    end repeat
end Solve

on abs(n)
    if n < 0 then
        -n
    else
        n
    end if
end abs

on Place_Queen(column as integer, row as integer, rows as list)
    local colDiff
    local previousRow
    local rowDiff
    local testColumn
    
    repeat with previousRow from 1 to (row - 1)
        set testColumn to item previousRow of rows
        
        if testColumn is equal to column then
            return false
        end if
        
        set colDiff to abs(testColumn - column) as integer
        set rowDiff to row - previousRow
        if colDiff is equal to rowDiff then
            return false
        end if
    end repeat
    
    set item row of rows to column
    return true
end Place_Queen

on Append_Solution(rows as list)
    local column
    local rowsCopy
    local testReflection
    local testReflectionText
    local testRotation
    local testRotationText
    local testRotations
    
    copy rows to rowsCopy
    set end of Solutions to rowsCopy
    local rowsCopy
    
    copy rows to testRotation
    set testRotations to {}
    repeat 3 times
        set testRotation to Rotate(testRotation)
        set testRotationText to testRotation as text
        if Rotated contains testRotationText then
            return
        end if
        set end of testRotations to testRotationText
        
        set testReflection to Reflect(testRotation)
        set testReflectionText to testReflection as text
        if Rotated contains testReflectionText then
            return
        end if
        set end of testRotations to testReflectionText
    end repeat
    
    repeat with testRotationText in testRotations
        set end of Rotated to (contents of testRotationText)
    end repeat
    set end of Rotated to (rowsCopy as text)
    set end of Rotated to (Reflect(rowsCopy) as text)
    
    set end of Patterns to rowsCopy
end Append_Solution

on Make_Empty_List(depth as integer)
    local i
    local emptyList
    
    set emptyList to {}
    repeat with i from 1 to depth
        set end of emptyList to missing value
    end repeat
    return emptyList
end Make_Empty_List

on Rotate(rows as list)
    local column
    local newColumn
    local newRow
    local newRows
    local row
    local rowCount
    
    set rowCount to (count rows)
    set newRows to Make_Empty_List(rowCount)
    repeat with row from 1 to rowCount
        set column to (contents of item row of rows)
        set newRow to column
        set newColumn to rowCount - row + 1
        set item newRow of newRows to newColumn
    end repeat
    
    return newRows
end Rotate

on Reflect(rows as list)
    local column
    local newRows
    
    set newRows to {}
    repeat with column in rows
        set end of newRows to (count rows) - column + 1
    end repeat
    
    return newRows
end Reflect
```



## Arc

This program prints out all possible solutions:

```Lisp
(def nqueens (n (o queens))
  (if (< len.queens n)
    (let row (if queens (+ 1 queens.0.0) 0)
      (each col (range 0 (- n 1))
        (let new-queens (cons (list row col) queens)
          (if (no conflicts.new-queens)
            (nqueens n new-queens)))))
    (prn queens)))

; check if the first queen in 'queens' lies on the same column or diagonal as
; any of the others
(def conflicts (queens)
  (let (curr . rest) queens
    (or (let curr-column curr.1
          (some curr-column (map [_ 1] rest)))  ; columns
        (some [diagonal-match curr _] rest))))

(def diagonal-match (curr other)
  (is (abs (- curr.0 other.0))
      (abs (- curr.1 other.1))))
```

{{out}}
The output is one solution per line, each solution in the form `((row col) (row col) (row col) ...)`:

```txt
arc> (nqueens 4)
((3 2) (2 0) (1 3) (0 1))
((3 1) (2 3) (1 0) (0 2))
```



## ATS


```ATS

(* ****** ****** *)
//
// Solving N-queen puzzle
//
(* ****** ****** *)
//
// How to test:
// ./queens
// How to compile:
// patscc -DATS_MEMALLOC_LIBC -o queens queens.dats
//
(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
//
#include
"share/HATS/atspre_staload_libats_ML.hats"
//
(* ****** ****** *)

fun
solutions(N:int) = let
//
fun
show
(
  board: list0(int)
) : void =
(
  list0_foreach<int>
  ( list0_reverse(board)
  , lam(n) => ((N).foreach()(lam(i) => print_string(if i = n then " Q" else " _")); print_newline())
  ) ;
  print_newline()
)
//
fun
safe
(
  i: int, j: int, k: int, xs: list0(int)
) : bool =
(
  case+ xs of
  | nil0() => true
  | cons0(x, xs) => x != i && x != j && x != k && safe(i, j+1, k-1, xs)
)
//
fun
loop
(
  col: int, xs: list0(int)
) : void =
(N).foreach()
(
lam(i) =>
if
safe(i, i+1, i-1, xs)
then let
  val xs = cons0(i, xs)
in
  if col = N then show(xs) else loop(col+1, xs)
end // end of [then]
)
//
in
  loop(1, nil0())
end // end of [solutions]

(* ****** ****** *)

val () = solutions(8)

(* ****** ****** *)

implement main0() = ()

(* ****** ****** *)

(* end of [queens.dats] *)

```



## AutoHotkey


###  Output to formatted Message box 

{{trans|C}}

```AutoHotkey
;
; Post: http://www.autohotkey.com/forum/viewtopic.php?p=353059#353059
; Timestamp: 05/may/2010
;

MsgBox % funcNQP(5)
MsgBox % funcNQP(8)

Return

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;
; ** USED VARIABLES **
;
; Global: All variables named Array[???]
;
; Function funcNPQ: nQueens , OutText , qIndex
;
; Function Unsafe: nIndex , Idx , Tmp , Aux
;
; Function PutBoard: Output , QueensN , Stc , xxx , yyy
;
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

funcNQP(nQueens)
{
  Global
  Array[0] := -1
  Local OutText , qIndex := 0
  While ( qIndex >= 0 )
  {
    Array[%qIndex%]++
    While ( (Array[%qIndex%] < nQueens) && Unsafe(qIndex) )
      Array[%qIndex%]++
    If ( Array[%qIndex%] < nQueens )
    {
      If ( qIndex < nQueens-1 )
        qIndex++  ,  Array[%qIndex%] := -1
      Else
        PutBoard(OutText,nQueens)
    }
    Else
      qIndex--
  }
  Return OutText
}

;------------------------------------------

Unsafe(nIndex)
{
  Global
  Local Idx := 1  ,  Tmp := 0  ,  Aux := Array[%nIndex%]
  While ( Idx <= nIndex )
  {
    Tmp := "Array[" nIndex - Idx "]"
    Tmp := % %Tmp%
    If ( ( Tmp = Aux ) || ( Tmp = Aux-Idx ) || ( Tmp = Aux+Idx ) )
      Return 1
    Idx++
  }
  Return 0
}

;------------------------------------------

PutBoard(ByRef Output,QueensN)
{
  Global
  Static Stc = 0
  Local xxx := 0 , yyy := 0
  Output .= "`n`nSolution #" (++Stc) "`n"
  While ( yyy < QueensN )
  {
    xxx := 0
    While ( xxx < QueensN )
      Output .= ( "|" ( ( Array[%yyy%] = xxx ) ? "Q" : "_" ) )  ,  xxx++
    Output .= "|`n"  ,  yyy++
  }
}
```


###  Includes a solution browser GUI 

This implementation supports N = 4..12 queens, and will find ALL solutions 
for each of the different sizes. 
The screenshot shows the first solution of 10 possible solutions for N = 5 queens.


```AutoHotkey
N := 5
Number: ; main entrance for different # of queens
    SI := 1
    Progress b2 w250 zh0 fs9, Calculating all solutions for %N% Queens ...
    Gosub GuiCreate
    Result := SubStr(Queens(N),2)
    Progress Off
    Gui Show,,%N%-Queens
    StringSplit o, Result, `n
Fill: ; show solutions
    GuiControl,,SI, %SI% / %o0%
    Loop Parse, o%SI%, `,
    {
        C := A_Index
        Loop %N%
            GuiControl,,%C%_%A_Index% ; clear fields
        GuiControl,,%C%_%A_LoopField%, r
    }
Return ;-----------------------------------------------------------------------

Queens(N) {                                 ; Size of the board
    Local c, O                              ; global array r
    r1 := 1, c := 2, r2 := 3, O := ""       ; init: r%c% = row of Queen in column c

    Right:                                  ; move to next column
        If (c = N) {                        ; found solution
            Loop %N%                        ; save row indices of Queens
                O .= (A_Index = 1 ? "`n" : ",") r%A_Index%
            GOTO % --c ? "Down" : "OUT"     ; for ALL solutions
        }
        c++, r%c% := 1                      ; next column, top row
        GoTo % BAD(c) ? "Down" : "Right"
    Down:                                   ; move down to next row
        If (r%c% = N)
            GoTo % --c ? "Down" : "OUT"
        r%c%++                              ; row down
        GoTo % BAD(c) ? "Down" : "Right"
    OUT:
        Return O
} ;----------------------------------------------------------------------------

BAD(c) { ; Check placed Queens against Queen in row r%c%, column c
    Loop % c-1
        If (r%A_Index% = r%c% || ABS(r%A_Index%-r%c%) = c-A_Index)
            Return 1
} ;----------------------------------------------------------------------------

GuiCreate: ; Draw chess board
    Gui Margin, 20, 15
    Gui Font, s16, Marlett
    Loop %N% {
        C := A_Index
        Loop %N% { ; fields
            R := A_Index, X := 40*C-17, Y := 40*R-22
            Gui Add, Progress, x%X% y%Y% w41 h41 Cdddddd, % 100*(R+C & 1) ;% shade fields
            Gui Add, Text, x%X% y%Y% w41 h41 BackGroundTrans Border Center 0x200 v%C%_%R%
        }
    }
    Gui Add, Button, x%x% w43 h25 gBF, 4 ; forth (default)
    Gui Add, Button,xm yp w43 h25 gBF, 3 ; back

    Gui Font, bold, Comic Sans MS
    Gui Add, Text,% "x62 yp hp Center 0x200 vSI w" 40*N-80

    Menu FileMenu, Add, E&xit, GuiClose
    Loop 9
        Menu CalcMenu, Add, % "Calculate " A_Index+3 " Queens", Calculate ;%
    Menu HelpMenu, Add, &About, AboutBox
    Menu MainMenu, Add, &File, :FileMenu
    Menu MainMenu, Add, &Calculate, :CalcMenu
    Menu MainMenu, Add, &Help, :HelpMenu
    Gui Menu, Mainmenu
Return ; ----------------------------------------------------------------------

AboutBox: ; message box with AboutText
    Gui 1: +OwnDialogs
    MsgBox, 64, About N-Queens, Many thanks ...
Return

Calculate: ; menu handler for calculations
    N := A_ThisMenuItemPos + 3
    Gui Destroy
    GoTo Number ; -------------------------------------------------------------

BF:
   SI := mod(SI+o0-2*(A_GuiControl=3), o0) + 1 ; left button text is "3"
   GoTo Fill ; ----------------------------------------------------------------

GuiClose:
ExitApp
```

[[image:N-Queens_SolutionBrowserGUI.png]]


## BBC BASIC

{{works with|BBC BASIC for Windows}}
The total number of solutions is displayed in the title bar and one solution is displayed.  The code could be adapted to display a selected solution or multiple solutions.
[[Image:queens8_bbc.gif|right]]
[[Image:queens9_bbc.gif|right]]
[[Image:queens10_bbc.gif|right]]

```bbcbasic
      Size% = 8
      Cell% = 32
      VDU 23,22,Size%*Cell%;Size%*Cell%;Cell%,Cell%,16,128+8,5
      *font Arial Unicode MS,16
      GCOL 3,11
      FOR i% = 0 TO Size%-1 STEP 2
        RECTANGLE FILL i%*Cell%*2,0,Cell%*2,Size%*Cell%*2
        RECTANGLE FILL 0,i%*Cell%*2,Size%*Cell%*2,Cell%*2
      NEXT
      num% = FNqueens(Size%, Cell%)
      SYS "SetWindowText", @hwnd%, "Total " + STR$(num%) + " solutions"
      REPEAT : WAIT 1 : UNTIL FALSE
      END
      
      DEF FNqueens(n%, s%)
      LOCAL i%, j%, m%, p%, q%, r%, a%(), b%(), c%()
      DIM a%(n%), b%(n%), c%(4*n%-2)
      FOR i% = 1 TO DIM(a%(),1) : a%(i%) = i% : NEXT
      m% = 0
      i% = 1
      j% = 0
      r% = 2*n%-1
      REPEAT
        i% -= 1
        j% += 1
        p% = 0
        q% = -r%
        REPEAT
          i% += 1
          c%(p%) = 1
          c%(q%+r%) = 1
          SWAP a%(i%),a%(j%)
          p% = i% - a%(i%) + n%
          q% = i% + a%(i%) - 1
          b%(i%) = j%
          j% = i% + 1
        UNTIL j% > n% OR c%(p%) OR c%(q%+r%)
        IF c%(p%)=0 IF c%(q%+r%)=0 THEN
          IF m% = 0 THEN
            FOR p% = 1 TO n%
              MOVE 2*s%*(a%(p%)-1)+6, 2*s%*p%+6
              PRINT "♛";
            NEXT
          ENDIF
          m% += 1
        ENDIF
        j% = b%(i%)
        WHILE j% >= n% AND i% <> 0
          REPEAT
            SWAP a%(i%), a%(j%)
            j% = j%-1
          UNTIL j% < i%
          i% -= 1
          p% = i% - a%(i%) + n%
          q% = i% + a%(i%) - 1
          j% = b%(i%)
          c%(p%) = 0
          c%(q%+r%) = 0
        ENDWHILE
      UNTIL i% = 0
      = m%
```



## BCPL


```BCPL
// This can be run using Cintcode BCPL freely available from www.cl.cam.ac.uk/users/mr10.

GET "libhdr.h"
 
GLOBAL { count:ug; all  }
 
LET try(ld, row, rd) BE TEST row=all

                        THEN count := count + 1

                        ELSE { LET poss = all & ~(ld | row | rd)
                               WHILE poss DO
                               { LET p = poss & -poss
                                 poss := poss - p
                                 try(ld+p << 1, row+p, rd+p >> 1)
                               }
                             }

LET start() = VALOF
{ all := 1
  
  FOR i = 1 TO 16 DO
  { count := 0
    try(0, 0, 0)
    writef("Number of solutions to %i2-queens is %i7*n", i, count)
    all := 2*all + 1
  }

  RESULTIS 0
}

```

The following is a re-implementation of the algorithm given above but
using the MC package that allows machine independent runtime generation
of native machine code (currently only available for i386 machines).
It runs about 25 times faster that the version given above.


```BCPL

GET "libhdr.h"
GET "mc.h"

MANIFEST {
 lo=1; hi=16
 dlevel=#b0000

 // Register mnemonics
 ld    = mc_a
 row   = mc_b
 rd    = mc_c
 poss  = mc_d
 p     = mc_e
 count = mc_f
}

LET start() = VALOF
{ // Load the dynamic code generation package
  LET mcseg = globin(loadseg("mci386"))
  LET mcb = 0

  UNLESS mcseg DO
  { writef("Trouble with MC package: mci386*n")
    GOTO fin
  }

  // Create an MC instance for hi functions with a data space
  // of 10 words and code space of 40000
  mcb := mcInit(hi, 10, 40000)

  UNLESS mcb DO
  { writef("Unable to create an mci386 instance*n")
    GOTO fin
  } 

  mc := 0          // Currently no selected MC instance
  mcSelect(mcb)

  mcK(mc_debug, dlevel) // Set the debugging level

  FOR n = lo TO hi DO
  { mcComment("*n*n// Code for a %nx%n board*n", n, n)
    gencode(n) // Compile the code for an nxn board
  }

  mcF(mc_end) // End of code generation

  writef("Code generation complete*n")

  FOR n = lo TO hi DO
  { LET k = mcCall(n)
    writef("Number of solutions to %i2-queens is %i9*n", n, k)
  }

fin:
  IF mc    DO mcClose()
  IF mcseg DO unloadseg(mcseg)  

  writef("*n*nEnd of run*n")
}

AND gencode(n) BE
{ LET all = (1<<n) - 1
  mcKKK(mc_entry, n, 3, 0)

  mcRK(mc_mv, ld,    0)
  mcRK(mc_mv, row,   0)
  mcRK(mc_mv, rd,    0)
  mcRK(mc_mv, count, 0)

  cmpltry(1, n, all)        // Compile the outermost call of try

  mcRR(mc_mv, mc_a, count)  // return count
  mcF(mc_rtn)
  mcF(mc_endfn)
}

AND cmpltry(i, n, all) BE
{ LET L = mcNextlab()

  mcComment("*n// Start of code from try(%n, %n, %n)*n", i, n, all)

  mcRR(mc_mv,  poss, ld)         // LET poss = (~(ld | row | rd)) & all
  mcRR(mc_or,  poss, row)
  mcRR(mc_or,  poss, rd)
  mcR (mc_not, poss)
  mcRK(mc_and, poss, all)

  mcRK(mc_cmp, poss, 0)          // IF poss DO
  TEST n-i<=2
  THEN mcJS(mc_jeq, L)           // (use a short jump if near the last row)
  ELSE mcJL(mc_jeq, L)

  TEST i=n
  THEN { // We can place a queen in the final row.
         mcR(mc_inc,  count)     //   count := count+1
       }
  ELSE { // We can place queen(s) in a non final row.
         LET M = mcNextlab()

         mcL (mc_lab,  M)        // { Start of REPEATWHILE loop

         mcRR(mc_mv,   p, poss)  //   LET p = poss & -poss
         mcR (mc_neg,  p)
         mcRR(mc_and,  p, poss)  //   // p is a valid queens position
         mcRR(mc_sub,  poss, p)  //   poss := poss - p


         mcR (mc_push, ld)       //   Save current state
         mcR (mc_push, row)
         mcR (mc_push, rd)
         mcR (mc_push, poss)
                                 //   Call try((ld+p)<<1, row+p, (rd+p)>>1)
         mcRR(mc_add,  ld,  p)
         mcRK(mc_lsh,  ld,  1)   //   ld  := (ld+p)<<1
         mcRR(mc_add,  row, p)   //   row := row+p
         mcRR(mc_add,  rd,  p)
         mcRK(mc_rsh,  rd,  1)   //   rd  := (rd+p)>>1

         cmpltry(i+1, n, all)    //   Compile code for row i+1

         mcR (mc_pop,  poss)     //   Restore the state
         mcR (mc_pop,  rd)
         mcR (mc_pop,  row)
         mcR (mc_pop,  ld)

         mcRK(mc_cmp,  poss, 0)
         mcJL(mc_jne, M)         // } REPEATWHILE poss
       }

       mcL(mc_lab, L)
       mcComment("// End   of code from try(%n, %n, %n)*n*n",
                 i, n, all)
}

```



## Befunge


This algorithm works with any board size from 4 upwards.


```befunge
<+--XX@_v#!:-1,+55,g\1$_:00g2%-0vv:,+55<&,,,,,,"Size: "
"| Q"$$$>:01p:2%!00g0>>^<<!:-1\<1>00p::2%-:40p2/50p2*1+
!77**48*+31p\:1\g,::2\g:,\3\g,,^g>0g++40g%40g\-\40g\`*-
2g05\**!!%6g04-g052!:`\g05::-1/2<^4*2%g05\+*+1*!!%6g04-
```


{{out}}


```txt
Size: 8

+---+---+---+---+---+---+---+---+
|   |   |   |   | Q |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   | Q |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
| Q |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   | Q |   |
+---+---+---+---+---+---+---+---+
|   | Q |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   |   | Q |
+---+---+---+---+---+---+---+---+
|   |   |   |   |   | Q |   |   |
+---+---+---+---+---+---+---+---+
|   |   |   | Q |   |   |   |   |
+---+---+---+---+---+---+---+---+
```



## Bracmat


```bracmat
(   ( printBoard
    =   board M L x y S R row line
      .   :?board
        & !ups:? [?M
        &   whl
          ' ( !arg:(?x.?y) ?arg
            & !M:?L
            & :?row:?line
            &   whl
              ' ( !L+-1:~<0:?L
                & !x+1:~>!M:?x
                & "---+" !line:?line
                & "   |" !row:?row
                )
            & "---+" !line:?line
            & " Q |" !row:?row
            &   whl
              ' ( !L+-1:~<0:?L
                & "---+" !line:?line
                & "   |" !row:?row
                )
            & "\n|" !row "\n+" !line !board:?board
            )
        & str$("\n+" !line !board)
    )
    ( queens
    =   hor ver up down ups downs a z A Z x y Q
      .   !arg:(?hor.?ver.?ups.?downs.?Q)
        &   !ver
          : (   
              & 1+!solutions:?solutions
              { Comment the line below if you only want a count. }
              & out$(str$("\nsolution " !solutions) printBoard$!Q)
              & ~  { Fail! (and backtrack to find more solutions)}
            |   #%?y
                ( ?z
                &   !hor
                  :   ?A
                      #%?x
                      ( ?Z
                      & !x+!y:?up
                      & !x+-1*!y:?down
                      & ~(!ups:? !up ?)
                      & ~(!downs:? !down ?)
                      &   queens
                        $ ( !A !Z
                          . !z
                          . !up !ups
                          . !down !downs
                          . (!x.!y) !Q
                          )
                      )
                )
            )
    )
& 0:?solutions
& 1 2 3 4 5 6 7 8:?H:?V   {You can edit this line to find solutions for other sizes.}
& ( queens$(!H.!V...) 
  | out$(found !solutions solutions)
  )
);
```

{{out}} (tail):

```txt

solution 91

+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   |   | Q |
+---+---+---+---+---+---+---+---+
|   |   | Q |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
| Q |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   |   |   |   | Q |   |   |
+---+---+---+---+---+---+---+---+
|   | Q |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   |   |   | Q |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   | Q |   |
+---+---+---+---+---+---+---+---+
|   |   |   | Q |   |   |   |   |
+---+---+---+---+---+---+---+---+

solution 92

+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   |   | Q |
+---+---+---+---+---+---+---+---+
|   |   |   | Q |   |   |   |   |
+---+---+---+---+---+---+---+---+
| Q |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   | Q |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   |   |   |   | Q |   |   |
+---+---+---+---+---+---+---+---+
|   | Q |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   | Q |   |
+---+---+---+---+---+---+---+---+
|   |   |   |   | Q |   |   |   |
+---+---+---+---+---+---+---+---+
found 92 solutions
```



## C

C99, compiled with <code>gcc -std=c99 -Wall</code>.  Take one commandline argument: size of board, or default to 8. Shows the board layout for each solution.
```C>#include <stdio.h

#include <stdlib.h>

int count = 0;
void solve(int n, int col, int *hist)
{
	if (col == n) {
		printf("\nNo. %d\n-----\n", ++count);
		for (int i = 0; i < n; i++, putchar('\n'))
			for (int j = 0; j < n; j++)
				putchar(j == hist[i] ? 'Q' : ((i + j) & 1) ? ' ' : '.');

		return;
	}

#	define attack(i, j) (hist[j] == i || abs(hist[j] - i) == col - j)
	for (int i = 0, j = 0; i < n; i++) {
		for (j = 0; j < col && !attack(i, j); j++);
		if (j < col) continue;

		hist[col] = i;
		solve(n, col + 1, hist);
	}
}

int main(int n, char **argv)
{
	if (n <= 1 || (n = atoi(argv[1])) <= 0) n = 8;
	int hist[n];
	solve(n, 0, hist);
}
```


Similiar to above, but using bits to save board configurations and quite a bit faster:
```c>#include <stdio.h

#include <stdlib.h>
#include <stdint.h>

typedef uint32_t uint;
uint full, *qs, count = 0, nn;

void solve(uint d, uint c, uint l, uint r)
{
	uint b, a, *s;
	if (!d) {
		count++;
#if 0
		printf("\nNo. %d\n
### =====
\n", count);
		for (a = 0; a < nn; a++, putchar('\n'))
			for (b = 0; b < nn; b++, putchar(' '))
				putchar(" -QQ"[((b == qs[a])<<1)|((a + b)&1)]);
#endif
		return;
	}

	a = (c | (l <<= 1) | (r >>= 1)) & full;
	if (a != full)
		for (*(s = qs + --d) = 0, b = 1; b <= full; (*s)++, b <<= 1)
			if (!(b & a)) solve(d, b|c, b|l, b|r);
}

int main(int n, char **argv)
{
	if (n <= 1 || (nn = atoi(argv[1])) <= 0) nn = 8;

	qs = calloc(nn, sizeof(int));
	full = (1U << nn) - 1;

	solve(nn, 0, 0, 0);
	printf("\nSolutions: %d\n", count);
	return 0;
}
```

Take that and unwrap the recursion, plus some heavy optimizations, and we have a very fast and very unreadable solution:

```c>#include <stdio.h

#include <stdlib.h>

typedef unsigned int uint;
uint count = 0;

#define ulen sizeof(uint) * 8

/* could have defined as int solve(...), but void may have less
   chance to confuse poor optimizer */
void solve(int n)
{
	int cnt = 0;
	const uint full = -(int)(1 << (ulen - n));
	register uint bits, pos, *m, d, e;

	uint b0, b1, l[32], r[32], c[32], mm[33] = {0};
	n -= 3;
	/* require second queen to be left of the first queen, so
	   we ever only test half of the possible solutions. This
	   is why we can't handle n=1 here */
	for (b0 = 1U << (ulen - n - 3); b0; b0 <<= 1) {
		for (b1 = b0 << 2; b1; b1 <<= 1) {
			d = n;
			/* c: columns occupied by previous queens.
			   l: columns attacked by left diagonals
			   r: by right diagnoals */
			c[n] = b0 | b1;
			l[n] = (b0 << 2) | (b1 << 1);
			r[n] = (b0 >> 2) | (b1 >> 1);

			/* availabe columns on current row. m is stack */
			bits = *(m = mm + 1) = full & ~(l[n] | r[n] | c[n]);

			while (bits) {
				/* d: depth, aka row. counting backwards
				   because !d is often faster than d != n */
				while (d) {
					/* pos is right most nonzero bit */
					pos = -(int)bits & bits;

					/* mark bit used. only put current bits
					   on stack if not zero, so backtracking
					   will skip exhausted rows (because reading
					   stack variable is sloooow compared to
					   registers) */
					if ((bits &= ~pos))
						*m++ = bits | d;

					/* faster than l[d+1] = l[d]... */
					e = d--;
					l[d] = (l[e] | pos) << 1;
					r[d] = (r[e] | pos) >> 1;
					c[d] =  c[e] | pos;

					bits = full & ~(l[d] | r[d] | c[d]);

					if (!bits) break;
					if (!d) { cnt++; break; }
				}
				/* Bottom of stack m is a zero'd field acting
				   as sentinel.  When saving to stack, left
				   27 bits are the available columns, while
				   right 5 bits is the depth. Hence solution
				   is limited to size 27 board -- not that it
				   matters in foreseeable future. */
				d = (bits = *--m) & 31U;
				bits &= ~31U;
			}
		}
	}
	count = cnt * 2;
}

int main(int c, char **v)
{
	int nn;
	if (c <= 1 || (nn = atoi(v[1])) <= 0) nn = 8;

	if (nn > 27) {
		fprintf(stderr, "Value too large, abort\n");
		exit(1);
	}

	/* Can't solve size 1 board; might as well skip 2 and 3 */
	if (nn < 4) count = nn == 1;
	else	    solve(nn);

	printf("\nSolutions: %d\n", count);
	return 0;
}
```


A slightly cleaned up version of the code above where some optimizations were redundant. This version is also further optimized, and runs about 15% faster than the one above on modern compilers:


```c>#include <stdio.h

#define MAXN 31

int nqueens(int n)
{
  int q0,q1;
  int cols[MAXN], diagl[MAXN], diagr[MAXN], posibs[MAXN]; // Our backtracking 'stack' 
  int num=0;
  //
  // The top level is two fors, to save one bit of symmetry in the enumeration by forcing second queen to
  // be AFTER the first queen.
  //
  for (q0=0; q0<n-2; q0++) {
    for (q1=q0+2; q1<n; q1++){
      int bit0 = 1<<q0;
      int bit1 = 1<<q1;
      int d=0; // d is our depth in the backtrack stack 
      cols[0] = bit0 | bit1 | (-1<<n); // The -1 here is used to fill all 'coloumn' bits after n ...
      diagl[0]= (bit0<<1 | bit1)<<1;
      diagr[0]= (bit0>>1 | bit1)>>1;

      //  The variable posib contains the bitmask of possibilities we still have to try in a given row ...
      int posib = ~(cols[0] | diagl[0] | diagr[0]);

      while (d >= 0) {
        while(posib) {
          int bit = posib & -posib; // The standard trick for getting the rightmost bit in the mask
          int ncols= cols[d] | bit;
          int ndiagl = (diagl[d] | bit) << 1;
          int ndiagr = (diagr[d] | bit) >> 1;
          int nposib = ~(ncols | ndiagl | ndiagr);
          posib^=bit; // Eliminate the tried possibility.

          // The following is the main additional trick here, as recognizing solution can not be done using stack level (d),
          // since we save the depth+backtrack time at the end of the enumeration loop. However by noticing all coloumns are
          // filled (comparison to -1) we know a solution was reached ...
          // Notice also that avoiding an if on the ncols==-1 comparison is more efficient!
          num += ncols==-1; 

          if (nposib) {
            if (posib) { // This if saves stack depth + backtrack operations when we passed the last possibility in a row.
              posibs[d++] = posib; // Go lower in stack ..
            }
            cols[d] = ncols;
            diagl[d] = ndiagl;
            diagr[d] = ndiagr;
            posib = nposib;
          }
        }
        posib = posibs[--d]; // backtrack ...
      }
    }
  }
  return num*2;
}


main(int ac , char **av) 
{
  if(ac != 2) {
    printf("usage: nq n\n");
    return 1;
  }
  int n = atoi(av[1]);
  if(n<1 || n > MAXN) {
    printf("n must be between 2 and 31!\n");
  }
  printf("Number of solution for %d is %d\n",n,nqueens(n));
}

```



## C++


```cpp
// Much shorter than the version below;
// uses C++11 threads to parallelize the computation; also uses backtracking
// Outputs all solutions for any table size
#include <vector>
#include <iostream>
#include <iomanip>
#include <thread>
#include <future>

// Print table. 'pos' is a vector of positions – the index in pos is the row,
// and the number at that index is the column where the queen is placed.
static void print(const std::vector<int> &pos)
{
	// print table header
	for (int i = 0; i < pos.size(); i++) {
		std::cout << std::setw(3) << char('a' + i);
	}

	std::cout << '\n';

	for (int row = 0; row < pos.size(); row++) {
		int col = pos[row];
		std::cout << row + 1 << std::setw(3 * col + 3) << " # ";
		std::cout << '\n';
	}

	std::cout << "\n\n";
}

static bool threatens(int row_a, int col_a, int row_b, int col_b)
{
	return row_a == row_b // same row
		or col_a == col_b // same column
		or std::abs(row_a - row_b) == std::abs(col_a - col_b); // diagonal
}

// the i-th queen is in the i-th row
// we only check rows up to end_idx
// so that the same function can be used for backtracking and checking the final solution
static bool good(const std::vector<int> &pos, int end_idx)
{
	for (int row_a = 0; row_a < end_idx; row_a++) {
		for (int row_b = row_a + 1; row_b < end_idx; row_b++) {
			int col_a = pos[row_a];
			int col_b = pos[row_b];
			if (threatens(row_a, col_a, row_b, col_b)) {
				return false;
			}
		}
	}

	return true;
}

static std::mutex print_count_mutex; // mutex protecting 'n_sols'
static int n_sols = 0; // number of solutions

// recursive DFS backtracking solver
static void n_queens(std::vector<int> &pos, int index)
{
	// if we have placed a queen in each row (i. e. we are at a leaf of the search tree), check solution and return
	if (index >= pos.size()) {
		if (good(pos, index)) {
			std::lock_guard<std::mutex> lock(print_count_mutex);
			print(pos);
			n_sols++;
		}

		return;
	}

	// backtracking step
	if (not good(pos, index)) {
		return;
	}

	// optimization: the first level of the search tree is parallelized
	if (index == 0) {
		std::vector<std::future<void>> fts;
		for (int col = 0; col < pos.size(); col++) {
			pos[index] = col;
			auto ft = std::async(std::launch::async, [=]{ auto cpos(pos); n_queens(cpos, index + 1); });
			fts.push_back(std::move(ft));
		}

		for (const auto &ft : fts) {
			ft.wait();
		}
	} else { // deeper levels are not
		for (int col = 0; col < pos.size(); col++) {
			pos[index] = col;
			n_queens(pos, index + 1);
		}
	}
}

int main()
{
	std::vector<int> start(12); // 12: table size
	n_queens(start, 0);
	std::cout << n_sols << " solutions found.\n";
	return 0;
}

```

{{out}}Output for N = 4:

```txt
  a  b  c  d                                                                                                  
1    #                                                                                                        
2          #                                                                                                  
3 #                                                                                                           
4       #                                                                                                     
                                                                                                              
                                                                                                              
  a  b  c  d                                                                                                  
1       #                                                                                                     
2 #                                                                                                           
3          #                                                                                                  
4    #       
```


```cpp

// A straight-forward brute-force C++ version with formatted output,
// eschewing obfuscation and C-isms, producing ALL solutions, which
// works on any OS with a text terminal.
//
// Two basic optimizations are applied:
//
//   It uses backtracking to only construct potentially valid solutions.
//
//   It only computes half the solutions by brute -- once we get the
//   queen halfway across the top row, any remaining solutions must be
//   reflections of the ones already computed.
//
// This is a bare-bones example, without any progress feedback or output
// formatting controls, which a more complete program might provide.
//
// Beware that computing anything larger than N=14 might take a while.
// (Time gets exponentially worse the higher the number.)

// Copyright 2014 Michael Thomas Greer
// Distributed under the Boost Software License, Version 1.0.
// http://www.boost.org/LICENSE_1_0.txt

#include <algorithm>
#include <ciso646>
#include <iomanip>
#include <iostream>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>


// ///////////////////////////////////////////////////////////////////////////
struct queens
/////////////////////////////////////////////////////////////////////////// //
{
  // TYPES -------------------------------------------------------------------

  // A row or column index. (May be signed or unsigned.)
  //
  typedef signed char index_type;

  // A 'solution' is a row --> column lookup of queens on the board.
  //
  // It has lexicographical order and can be transformed with a variety of
  // reflections, which, when properly combined, produce all possible
  // orientations of a solution.
  //
  struct solution_type: std::vector <index_type>
  {
    typedef std::vector <index_type> base_type;

    // constructors  . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    solution_type( std::size_t N          ): base_type( N, -1 ) { }
    solution_type( const solution_type& s ): base_type( s     ) { }

    // compare . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    bool operator < ( const solution_type& s ) const
    {
      auto mm = std::mismatch( begin(), end(), s.begin() );
      return (mm.first != end()) and (*mm.first < *mm.second);
    }

    // transformations . . . . . . . . . . . . . . . . . . . . . . . . . . . .
    void vflip() { std::reverse( begin(), end() ); }

    void hflip() { for (auto& x : *this) x = size() - 1 - x; }

    void transpose()
    {
      solution_type result( size() );
      for (index_type y = 0; (std::size_t)y < size(); y++)
        result[ (*this)[ y ] ] = y;
      swap( result );
    }
  };

  // MEMBER VALUES -----------------------------------------------------------

  const int                N;
  std::set <solution_type> solutions;

  // SOLVER ------------------------------------------------------------------

  queens( int N = 8 ):
    N( (N < 0) ? 0 : N )
  {
    // Row by row we create a potentially valid solution.
    // If a queen can be placed in a valid spot by the time
    // we get to the last row, then we've found a solution.

    solution_type solution( N );
    index_type row = 0;
    while (true)
    {
      // Advance the queen along the row
      ++solution[ row ];

      // (If we get past halfway through the first row, we're done.)
      if ((row == 0) and (solution[ 0 ] > N/2)) break;

      if (solution[ row ] < N)
      {
        // If the queen is in a good spot...
        if (ok( solution, row, solution[ row ] ))
        {
          // ...and we're on the last row
          if (row == N-1)
          {
            // Add the solution we found plus all it's reflections
            solution_type
            s = solution;  solutions.insert( s );
            s.vflip();     solutions.insert( s );
            s.hflip();     solutions.insert( s );
            s.vflip();     solutions.insert( s );
            s.transpose(); solutions.insert( s );
            s.vflip();     solutions.insert( s );
            s.hflip();     solutions.insert( s );
            s.vflip();     solutions.insert( s );
          }
          // otherwise begin marching a queen along the next row
          else solution[ ++row ] = -1;
        }

      // When we get to the end of a row's columns then
      // we need to backup a row and continue from there.
      }
      else --row;
    }
  }

  // HELPER ------------------------------------------------------------------
  // This routine helps the solver by identifying column locations
  // that do not conflict with queens already placed in prior rows.

  bool ok( const solution_type& columns, index_type row, index_type column )
  {
    for (index_type r = 0; r < row; r++)
    {
      index_type c         = columns[ r ];
      index_type delta_row = row - r;
      index_type delta_col = (c < column) ? (column - c) : (c - column);

      if ((c == column) or (delta_row == delta_col))
        return false;
    }
    return true;
  }

  // OUTPUT A SINGLE SOLUTION ------------------------------------------------
  //
  // Formatted as (for example):
  //
  //   d1 b2 g3 c4 f5 h6 e7 a8
  //   Q - - - - - - -
  //   - - - - Q - - -
  //   - - - - - - - Q
  //   - - - - - Q - -
  //   - - Q - - - - -
  //   - - - - - - Q -
  //   - Q - - - - - -
  //   - - - Q - - - -
  //
  friend
  std::ostream&
  operator << ( std::ostream& outs, const queens::solution_type& solution )
  {
    static const char* squares[] = { "- ", "Q " };
    index_type N = solution.size();

    // Display the queen positions
    for (auto n = N; n--; )
      outs << (char)('a' + solution[ n ]) << (N - n) << " ";

    // Display the board
    for (auto queen : solution)
    {
      outs << "\n";
      for (index_type col = 0; col < N; col++)
        outs << squares[ col == queen ];
    }
    return outs;
  }

  // OUTPUT ALL SOLUTIONS ----------------------------------------------------
  //
  // Display "no solutions" or "N solutions" followed by
  // each individual solution, separated by blank lines.

  friend
  std::ostream&
  operator << ( std::ostream& outs, const queens& q )
  {
    if (q.solutions.empty()) outs << "no";
    else                     outs << q.solutions.size();
    outs << " solutions";

    std::size_t n = 1;
    for (auto solution : q.solutions)
    {
      outs << "\n\n#" << n++ << "\n" << solution;
    }

    return outs;
  }
};


/* ///////////////////////////////////////////////////////////////////////////
string_to <type> ( x )
/////////////////////////////////////////////////////////////////////////// */

template <typename T>
T string_to( const std::string& s )
{
  T result;
  std::istringstream ss( s );
  ss >> result;
  if (!ss.eof()) throw std::runtime_error( "to_string(): invalid conversion" );
  return result;
}

template <typename T, T default_value>
T string_to( const std::string& s )
{
  try { return string_to <T> ( s ); }
  catch (...) { return default_value; }
}


/* ///////////////////////////////////////////////////////////////////////////
main program
/////////////////////////////////////////////////////////////////////////// */

int usage( const std::string& name )
{
  std::cerr <<
    "usage:\n  " << name << " 8\n\n"
    ""
    "Solve the N-Queens problem, brute-force,\n"
    "and show all solutions for an 8x8 board.\n\n"
    ""
    "(Specify a value other than 8 for the board size you want.)\n";
  return 1;
}

int main( int argc, char** argv )
{
  signed N =
    (argc < 2) ? 8 :
    (argc > 2) ? 0 : string_to <signed, 0> ( argv[ 1 ] );

  if (N <= 0) return usage( argv[ 0 ] );

  std::cout << queens( N ) << "\n";
}

```

{{out}} for N=4:

```txt

2 solutions

#1
c1 a2 d3 b4
- Q - -
- - - Q
Q - - -
- - Q -

#2
b1 d2 a3 c4
- - Q -
Q - - -
- - - Q
- Q - -

```


###  Alternate version 

Windows-only

```cpp

#include <windows.h>
#include <iostream>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
class point
{
public:
    int x, y;
    point(){ x = y = 0; }
    void set( int a, int b ){ x = a; y = b; }
};
//--------------------------------------------------------------------------------------------------
class nQueens
{
public:
    void solve( int c )
    {
        _count = c; int len = ( c + 1 ) * ( c + 1 ); _queens = new bool[len]; memset( _queens, 0, len );
	_cl = new bool[c]; memset( _cl, 0, c ); _ln = new bool[c]; memset( _ln, 0, c );
	point pt; pt.set( rand() % c, rand() % c ); putQueens( pt, c ); displayBoard();
	delete [] _queens; delete [] _ln; delete [] _cl;
    }

private:
    void displayBoard()
    {
	system( "cls" ); string t = "+---+", q = "| Q |", s = "|   |";
	COORD c = { 0, 0 }; HANDLE h = GetStdHandle( STD_OUTPUT_HANDLE );
	for( int y = 0, cy = 0; y < _count; y++ )
	{
	    int yy = y * _count;
	    for( int x = 0; x < _count; x++ )
	    {
		SetConsoleCursorPosition( h, c ); cout << t;
		c.Y++; SetConsoleCursorPosition( h, c );
		if( _queens[x + yy] ) cout << q; else cout << s;
		c.Y++; SetConsoleCursorPosition( h, c );
		cout << t; c.Y = cy; c.X += 4;
	    }
	    cy += 2; c.X = 0; c.Y = cy;
        }
    }

    bool checkD( int x, int y, int a, int b )
    {
	if( x < 0 || y < 0 || x >= _count || y >= _count ) return true;
	if( _queens[x + y * _count] ) return false;
	if( checkD( x + a, y + b, a, b ) ) return true;
	return false;
    }

    bool check( int x, int y )
    {
	if( _ln[y] || _cl[x] )        return false;
	if( !checkD( x, y, -1, -1 ) ) return false;
	if( !checkD( x, y,  1, -1 ) ) return false;
	if( !checkD( x, y, -1,  1 ) ) return false;
	if( !checkD( x, y,  1,  1 ) ) return false;
	return true;
    }

    bool putQueens( point pt, int cnt )
    {
	int it = _count;
	while( it )
	{
	    if( !cnt ) return true;
	    if( check( pt.x, pt.y ) )
	    {
		_queens[pt.x + pt.y * _count] = _cl[pt.x] = _ln[pt.y] = true;
		point tmp = pt; if( ++tmp.x >= _count ) tmp.x = 0; if( ++tmp.y >= _count ) tmp.y = 0;
		if( putQueens( tmp, cnt - 1 ) ) return true;
		_queens[pt.x + pt.y * _count] = _cl[pt.x] = _ln[pt.y] = false;
	    }
	    if( ++pt.x >= _count ) pt.x = 0;
	    it--;
	}
	return false;
    }

    int          _count;
    bool*        _queens, *_ln, *_cl;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    nQueens n; int nq;
    while( true )
    {
	system( "cls" ); cout << "Enter board size bigger than 3 (0 - 3 to QUIT): "; cin >> nq;
	if( nq < 4 ) return 0; n.solve( nq ); cout << endl << endl;
	system( "pause" );
    }
    return  0;
}
//--------------------------------------------------------------------------------------------------

```

{{out}}

```txt

+---+---+---+---+---+   +---+---+---+---+---+---+---+---+   +---+---+---+---+---+---+---+---+---+---+---+
| Q |   |   |   |   |   |   |   |   | Q |   |   |   |   |   |   |   |   |   |   |   |   | Q |   |   |   |
+---+---+---+---+---+   +---+---+---+---+---+---+---+---+   +---+---+---+---+---+---+---+---+---+---+---+
|   |   | Q |   |   |   |   | Q |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   | Q |   |
+---+---+---+---+---+   +---+---+---+---+---+---+---+---+   +---+---+---+---+---+---+---+---+---+---+---+
|   |   |   |   | Q |   |   |   |   |   |   |   |   | Q |   |   |   |   | Q |   |   |   |   |   |   |   |
+---+---+---+---+---+   +---+---+---+---+---+---+---+---+   +---+---+---+---+---+---+---+---+---+---+---+
|   | Q |   |   |   |   |   |   |   |   |   | Q |   |   |   |   | Q |   |   |   |   |   |   |   |   |   |
+---+---+---+---+---+   +---+---+---+---+---+---+---+---+   +---+---+---+---+---+---+---+---+---+---+---+
|   |   |   | Q |   |   | Q |   |   |   |   |   |   |   |   |   |   |   |   | Q |   |   |   |   |   |   |
+---+---+---+---+---+   +---+---+---+---+---+---+---+---+   +---+---+---+---+---+---+---+---+---+---+---+
                        |   |   | Q |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   | Q |
                        +---+---+---+---+---+---+---+---+   +---+---+---+---+---+---+---+---+---+---+---+
                        |   |   |   |   | Q |   |   |   |   | Q |   |   |   |   |   |   |   |   |   |   |
                        +---+---+---+---+---+---+---+---+   +---+---+---+---+---+---+---+---+---+---+---+
                        |   |   |   |   |   |   | Q |   |   |   |   | Q |   |   |   |   |   |   |   |   |
                        +---+---+---+---+---+---+---+---+   +---+---+---+---+---+---+---+---+---+---+---+
                                                            |   |   |   |   |   | Q |   |   |   |   |   |
                                                            +---+---+---+---+---+---+---+---+---+---+---+
                                                            |   |   |   |   |   |   |   |   | Q |   |   |
                                                            +---+---+---+---+---+---+---+---+---+---+---+
                                                            |   |   |   |   |   |   | Q |   |   |   |   |
							    +---+---+---+---+---+---+---+---+---+---+---+

```


Version using Heuristics - explained here: [http://en.wikipedia.org/wiki/8_queens_puzzle#Solution_construction Solution_construction]

```cpp

#include <windows.h>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
typedef unsigned int uint;

//--------------------------------------------------------------------------------------------------
class nQueens_Heuristic
{
public:
    void solve( uint n ) { makeList( n ); drawBoard( n ); }

private:
    void drawBoard( uint n )
    {
	system( "cls" ); string t = "+---+", q = "| Q |", s = "|   |";
	COORD c = { 0, 0 }; HANDLE h = GetStdHandle( STD_OUTPUT_HANDLE );
	uint w = 0;
	for( uint y = 0, cy = 0; y < n; y++ )
	{
	    for( uint x = 0; x < n; x++ )
	    {
		SetConsoleCursorPosition( h, c ); cout << t;
		c.Y++; SetConsoleCursorPosition( h, c );
		if( x + 1 == solution[w] ) cout << q; else cout << s;
		c.Y++; SetConsoleCursorPosition( h, c );
		cout << t; c.Y = cy; c.X += 4;
	    }
	    cy += 2; c.X = 0; c.Y = cy; w++;
	}
	solution.clear(); odd.clear(); evn.clear();
    }

    void makeList( uint n )
    {
	uint r = n % 6;
	for( uint x = 1; x <= n; x++ )
	{
	    if( x & 1 ) odd.push_back( x );
	    else evn.push_back( x );
	}
	if( r == 2 )
	{
	    swap( odd[0], odd[1] );
	    odd.erase( find( odd.begin(), odd.end(), 5 ) );
	    odd.push_back( 5 );
	}
	else if( r == 3 )
	{
	    odd.erase( odd.begin() ); odd.erase( odd.begin() );
	    odd.push_back( 1 ); odd.push_back( 3 );
	    evn.erase( evn.begin() ); evn.push_back( 2 );
	}
	vector<uint>::iterator it = evn.begin();
	while( it != evn.end() ) 
	{
	    solution.push_back( ( *it ) );
	    it++;
	}
	it = odd.begin();
	while( it != odd.end() ) 
	{
	    solution.push_back( ( *it ) );
	    it++;
	}
    }

    vector<uint> odd, evn, solution;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    uint n; nQueens_Heuristic nQH;
    while( true )
    {
	cout << "Enter board size bigger than 3 (0 - 3 to QUIT): "; cin >> n;
	if( n < 4 ) return 0;
	nQH.solve( n ); cout << endl << endl;
    }
    return 0;
}
//--------------------------------------------------------------------------------------------------

```


=={{header|C sharp|C#}}==
=== Roger Hui (1981) Algorithm ===
From Hui, Roger, The N Queens Problem, APL Quote-Quad, Volume 11, Number 3, 1981-03:-

"In a solution, each possible row (column) index must appear exactly once: an index occurring more than once means that two queens are on the same row (column); and the absence of an index means that some other index must occur more than once. Hence, we can specify an arrangement as a permutation of ⍳n , which are the column indices, with the row indices understood to be ⍳n . With this, the number of possibilities is reduced from n!n×n to !n . It remains to eliminate arrangements having two queens on the same diagonal.

If two queens occupy the same diagonal, the line connecting them has slope 1 or ¯1 . Conversely, if the line connecting two queens has slope 1 or ¯1 , the two queens share a diagonal. Therefore, we seek to eliminate all permutations specifying a pair of queens where
((change in y) ÷ (change in x)) ∊ 1 ¯1 , or (|change in y) = (|change in x)"
{{trans|J}}
{{works with|C sharp|C#|7}}
<!-- By Martin Freedman, 13/02/2018 -->

```csharp
using System.Collections.Generic;
using static System.Linq.Enumerable;
using static System.Console;
using static System.Math;

namespace N_Queens
{
    static class Program
    {
        static void Main(string[] args)
        {
            var n = 8;
            var cols = Range(0, n);
            var combs = cols.Combinations(2).Select(pairs=> pairs.ToArray());
            var solved = from v in cols.Permutations().Select(p => p.ToArray())
                         where combs.All(c => Abs(v[c[0]] - v[c[1]]) != Abs(c[0] - c[1]))
                         select v;
            
            WriteLine($"{n}-queens has {solved.Count()} solutions");
            WriteLine("Position is row, value is column:-");
            var first = string.Join(" ", solved.First());
            WriteLine($"First Solution: {first}");
            Read();
        }

        //Helpers 
        public static IEnumerable<IEnumerable<T>> Permutations<T>(this IEnumerable<T> values)
        {
            if (values.Count() == 1)
                return values.ToSingleton();

            return values.SelectMany(v => Permutations(values.Except(v.ToSingleton())), (v, p) => p.Prepend(v));
        }

        public static IEnumerable<IEnumerable<T>> Combinations<T>(this IEnumerable<T> seq) =>
            seq.Aggregate(Empty<T>().ToSingleton(), (a, b) => a.Concat(a.Select(x => x.Append(b))));

        public static IEnumerable<IEnumerable<T>> Combinations<T>(this IEnumerable<T> seq, int numItems) =>
            seq.Combinations().Where(s => s.Count() == numItems);

        public static IEnumerable<T> ToSingleton<T>(this T item) { yield return item; }
    }
}
```

Output

```txt
8-queens has 92 solutions
Position is row, value is column:-
First Solution: 0 4 7 5 2 6 1 3

```


### Hettinger Algorithm

Compare this to the Hettinger solution used in the first Python answer. The logic is similar but the diagonal calculation is different and more expensive computationally (Both suffer from being unable to eliminate permutation prefixes that are invalid e.g.  0 1 ...)

```csharp

using System.Collections.Generic;
using static System.Linq.Enumerable;
using static System.Console;
using static System.Math;

namespace N_Queens
{
    static class Program
    {
        static void Main(string[] args)
        {
            var n = 8;
            var cols = Range(0, n);
            var solved = from v in cols.Permutations().Select(p => p.ToArray())
                         where n == (from i in cols select v[i]+i).Distinct().Count()
                         where n == (from i in cols select v[i]-i).Distinct().Count()
                         select v;

            WriteLine($"{n}-queens has {solved.Count()} solutions");
            WriteLine("Position is row, value is column:-");
            var first = string.Join(" ", solved.First());
            WriteLine($"First Solution: {first}");
            Read();
        }

        //Helpers from https://gist.github.com/martinfreedman/139dd0ec7df4737651482241e48b062f

        public static IEnumerable<IEnumerable<T>> Permutations<T>(this IEnumerable<T> values)
        {
            if (values.Count() == 1)
                return values.ToSingleton();

            return values.SelectMany(v => Permutations(values.Except(v.ToSingleton())), (v, p) => p.Prepend(v));
        }

        public static IEnumerable<T> ToSingleton<T>(this T item) { yield return item; }
    }
}
```


###  Amb solution

This uses the second version of the [https://rosettacode.org/wiki/Amb#C.23 Amb C# class] in the Amb challenge. Really that is not McCarthy's Amb (Ambiguous function) and here it is used just as a simple general interface by lambdas to a standalone backtrack algorithm. Due to the specification of the Amb challenge, this, ironically (given the notion of ambiguous functions), only produces one solution not 92. It is trivial to update Amb (might be better called a backtracker rather than Amb too) but here it is just used to show how easy it is to go from a generate and prune Linq solution to a backtrack solution. The Linq filters becoming "amb" requirements.
{{works with|C sharp|C#|7.1}}
<!-- By Martin Freedman, 9/02/2018 -->

```csharp
using static System.Linq.Enumerable;
using static System.Console;

namespace N_Queens
{
    static class Program
    {
        static void Main(string[] args)
        {
            var n = 8;
            var domain = Range(0, n).ToArray();

            var amb = new Amb.Amb();
            var queens = domain.Select(_ => amb.Choose(domain)).ToArray();
            amb.Require(() => n == queens.Select(q=> q.Value).Distinct().Count());
            amb.Require(() => n == domain.Select(i=> i + queens[i].Value).Distinct().Count());
            amb.Require(() => n == domain.Select(i=> i - queens[i].Value).Distinct().Count());

            if (amb.Disambiguate())
            {
                WriteLine("Position is row, value is column:-");
                WriteLine(string.Join(" ", queens.AsEnumerable()));
            }
            else
                WriteLine("amb is angry");
            Read();
        }
    }
}
```



## Clojure

This produces all solutions by essentially a backtracking algorithm. The heart is the ''extends?'' function, which takes a partial solution for the first ''k<size'' columns and sees if the solution can be extended by adding a queen at row ''n'' of column ''k+1''. The ''extend'' function takes a list of all partial solutions for ''k'' columns and produces a list of all partial solutions for ''k+1'' columns. The final list ''solutions'' is calculated by starting with the list of 0-column solutions (obviously this is the list ''[ [] ]'', and iterates ''extend'' for ''size'' times.

```clojure
(def size 8)

(defn extends? [v n]
  (let [k (count v)]
    (not-any? true?
      (for [i (range k) :let [vi (v i)]]
        (or
          (= vi n)  ;check for shared row
          (= (- k i) (Math/abs (- n vi)))))))) ;check for shared diagonal

(defn extend [vs]
  (for [v vs
        n (range 1 (inc size)) :when (extends? v n)]
    (conj v n)))


(def solutions
  (nth (iterate extend [[]]) size))

(doseq [s solutions]
  (println s))

(println (count solutions) "solutions")
```


### Short Version


```clojure
(ns queens
  (:require [clojure.math.combinatorics :as combo]

(defn queens [n]
  (filter (fn [x] (every? #(apply distinct? (map-indexed % x)) [+ -]))
          (combo/permutations (range 1 (inc n))))) 
```



## CoffeeScript


```coffeescript

# Unlike traditional N-Queens solutions that use recursion, this
# program attempts to more closely model the "human" algorithm.
# 
# In this algorithm, the function keeps placing queens on the board
# until there is no longer a safe square.  If the 8th queen has been
# placed, the solution is noted.  If fewer than 8th queens have been
# placed, then you are at a dead end.  In either case, backtracking occurs.
# The LAST queen placed on the board gets pulled, then it gets moved
# to the next safe square.  (We backtrack even after a "good" attempt in 
# order to get to a new solution.)  This backtracking may repeat itself
# several times until the original misplaced queen finally is proven to
# be a dead end.
#
# Many N-Queens solutions use lazy logic (along with geometry shortcuts)
# to determine whether a queen is under attack.  In this algorithm, we
# are more proactive, essentially updating a sieve every time we lay a
# queen down.  To make backtracking easier, the sieve uses ref-counts vs.
# a simple safe/unsafe boolean.
#
# We precompute the "attack graph" up front, and then we essentially ignore
# the geometry of the problem.  This approach, while perhaps suboptimal for
# queens, probably is more flexible for general "coexistence" problems.
nqueens = (n) ->
  neighbors = precompute_neighbors(n)

  board = []
  num_solutions = 0
  num_backtracks = 0
  queens = []
  pos = 0

  for p in [0...n*n]
    board.push 0
  
  attack = (pos, delta=1) ->
    for neighbor in neighbors[pos]
      board[neighbor] += delta
      
  backtrack = ->
    pos = queens.pop()
    attack pos, -1 # unattack queen you just pulled
    pos += 1
    num_backtracks += 1

  # The following loop finds all 92 solutions to 
  # the 8-queens problem (for n=8).
  while true  
    if pos >= n*n
      if queens.length == 0
        break
      backtrack()
      continue

    # If a square is empty
    if board[pos] == 0
      attack pos
      queens.push pos
      if queens.length == n
        num_solutions += 1
        show_queens queens, n
        backtrack()
    pos += 1
    
  console.log "#{num_solutions} solutions"
  console.log "#{num_backtracks} backtracks"


precompute_neighbors = (n) ->
  # For each board position, build a list of all
  # the board positions that would be under attack if
  # you placed a queen on it.  This assumes a 1d array
  # of squares.
  neighbors = []

  find_neighbors = (pos) ->
    arr = []
    row = Math.floor pos / n
    col = pos % n
    for i in [0...n]
      if i != col
        arr.push row*n + i
        r1 = row + col - i
        r2 = row + i - col
        if 0 <= r1 and r1 < n
          arr.push r1*n + i
        if 0 <= r2 and r2 < n
          arr.push r2*n + i
      if i != row
        arr.push i*n + col
    arr

  for pos in [0...n*n]
    neighbors.push find_neighbors(pos) 
  neighbors


show_queens = (queens, n) ->
  # precondition: queens is a sorted array of integers,
  # and each row is represented
  console.log "\n------"
  for q in queens
    col = q % n
    s = ''
    for c in [0...n]
      if c == col
        s += "Q "
      else
        s += "* "
    console.log s + "\n"

nqueens(8)

```




## Common Lisp


```lisp
(defun queens (n &optional (m n))
  (if (zerop n)
      (list nil)
      (loop for solution in (queens (1- n) m)
            nconc (loop for new-col from 1 to m
                         when (loop for row from 1 to n
                                     for col in solution
                                     always (/= new-col col (+ col row) (- col row)))
                         collect (cons new-col solution)))))

(defun print-solution (solution)
  (loop for queen-col in solution
        do (loop for col from 1 to (length solution)
                  do (write-char (if (= col queen-col) #\Q #\.)))
           (terpri))
  (terpri))

(defun print-queens (n)
  (mapc #'print-solution (queens n)))
```



###  Alternate solution 

Translation of Fortran 77

```lisp
(defun queens1 (n)
    (let ((a (make-array n))
          (s (make-array n))
          (u (make-array (list (- (* 4 n) 2)) :initial-element t))
          y z (i 0) j p q (r (1- (* 2 n))) (m 0))
        (dotimes (i n) (setf (aref a i) i))
        (tagbody
            L1
            (if (>= i n) (go L5))
            (setf j i)
            L2
            (setf y (aref a j) z (aref a i))
            (setf p (+ (- i y) n -1) q (+ i y))
            (setf (aref a i) y (aref a j) z)
            (when (and (aref u p) (aref u (+ q r))) 
                (setf (aref s i) j (aref u p) nil (aref u (+ q r)) nil)
                (incf i)
                (go L1))
            L3
            (incf j)
            (if (< j n) (go L2))
            L4
            (decf j)
            (if (= j i) (go L6))
            (rotatef (aref a i) (aref a j))
            (go L4)
            L5
            (incf m)
            L6
            (decf i)
            (if (minusp i) (go L7))
            (setf p (+ (- i (aref a i)) n -1) q (+ i (aref a i)) j (aref s i))
            (setf (aref u p) t (aref u (+ q r)) t)
            (go L3)
            L7)
        m))

> (loop for n from 1 to 14 collect (cons n (queens1 n)))
((1 . 1) (2 . 0) (3 . 0) (4 . 2) (5 . 10) (6 . 4) (7 . 40) (8 . 92) (9 . 352)
 (10 . 724) (11 . 2680) (12 . 14200) (13 . 73712) (14 . 365596))
```


As in Fortran, the iterative function above is equivalent to the recursive function below:


```lisp
(defun queens2 (n)
    (let ((a (make-array n))
          (u (make-array (+ n n -1) :initial-element t))
          (v (make-array (+ n n -1) :initial-element t))
          (m 0))
        (dotimes (i n) (setf (aref a i) i))
        (labels ((sub (i)
            (if (= i n)
                ;(push (copy-seq a) s)
                (incf m)
                (loop for k from i below n do
                    (let ((p (+ i (aref a k)))
                          (q (+ (- i (aref a k)) n -1)))
                        (when (and (aref u p) (aref v q))
                            (setf (aref u p) nil (aref v q) nil)
                            (rotatef (aref a i) (aref a k))
                            (sub (1+ i))
                            (setf (aref u p) t (aref v q) t)
                            (rotatef (aref a i) (aref a k))))))))
            (sub 0))
        m))
```



## Curry

Three different ways of attacking the same problem. All copied from [http://web.cecs.pdx.edu/~antoy/flp/patterns/ A Catalog of Design Patterns in FLP]

```curry

-- 8-queens implementation with the Constrained Constructor pattern
-- Sergio Antoy
-- Fri Jul 13 07:05:32 PDT 2001

-- Place 8 queens on a chessboard so that no queen can capture
-- (and be captured by) any other queen.

-- Non-deterministic choice operator

infixl 0 !
X ! _ = X
_ ! Y = Y

-- A solution is represented by a list of integers.
-- The i-th integer in the list is the column of the board
-- in which the queen in the i-th row is placed.
-- Rows and columns are numbered from 1 to 8.
-- For example, [4,2,7,3,6,8,5,1] is a solution where the
-- the queen in row 1 is in column 4, etc.
-- Any solution must be a permutation of [1,2,...,8].

-- The state of a queen is its position, row and column, on the board.
-- Operation column is a particularly simple instance
-- of a Constrained Constructor pattern.
-- When it is invoked, it produces only valid states.

column = 1 ! 2 ! 3 ! 4 ! 5 ! 6 ! 7 ! 8

-- A path of the puzzle is a sequence of successive placements of
-- queens on the board.  It is not explicitly defined as a type.
-- A path is a potential solution in the making.

-- Constrained Constructor on a path
-- Any path must be valid, i.e., any column must be in the range 1..8
-- and different from any other column in the path.
-- Furthermore, the path must be safe for the queens.
-- No queen in a path may capture any other queen in the path.
-- Operation makePath add column n to path c or fails.

makePath c n | valid c && safe c 1 = n:c
    where valid c | n =:= column = uniq c
             where uniq [] = True
                   uniq (c:cs) = n /= c && uniq cs
          safe [] _ = True
          safe (c:cs) k = abs (n-c) /= k && safe cs (k+1)
             where abs x = if x < 0 then -x else x

-- extend the path argument till all the queens are on the board
-- see the Incremental Solution pattern

extend p = if (length p == 8)
             then p
             else extend (makePath p x)
      where x free

-- solve the puzzle

main = extend []

```


Another approach from the same source.


```curry

-- N-queens puzzle implemented with "Distinct Choices" pattern
-- Sergio Antoy
-- Tue Sep  4 13:16:20 PDT 2001
-- updated: Mon Sep 23 15:22:15 PDT 2002

import Integer

queens x | y =:= permute x & void (capture y) = y  where y free

capture y = let l1,l2,l3,y1,y2 free in
  l1 ++ [y1] ++ l2 ++ [y2] ++ l3 =:= y & abs (y1-y2) =:= length l2 + 1

-- negation as failure (implemented by encapsulated search):
void c = (findall \_->c) =:= []

-- How does this permutation algorithm work?
-- Only the elements [0,1,...,n-1] can be permuted.
-- The reason is that each element is used as an index in a list.
-- A list, called store, of free variables of length n is created.
-- Then, the n iterations described below are executed.
-- At the i-th iteration, an element, say s,
-- of the initial list is non-deterministically selected.
-- This element is used as index in the store.
-- The s-th variable of the store is unified with i.
-- At the end of the iterations, the elements of the store
-- are a permutation of [0,1,...,n-1], i.e., the elements
-- are unique since two iterations cannot select the same index.

permute n = result n
   where result n = if n==0 then [] else pick n store : result (n-1)
         pick i store | store !! k =:= i = k where k = range n
         range n | n > 0 = range (n-1) ! (n-1)
         store = free
-- end

```


Yet another approach, also from the same source.


```curry

-- 8-queens implementation with both the Constrained Constructor
-- and the Fused Generate and Test patterns.
-- Sergio Antoy
-- Fri Jul 13 07:05:32 PDT 2001

-- Place 8 queens on a chessboard so that no queen can capture
-- (and be captured by) any other queen.

-- Non-deterministic choice operator

infixl 0 !
X ! _ = X
_ ! Y = Y

-- A solution is represented by a list of integers.
-- The i-th integer in the list is the column of the board
-- in which the queen in the i-th row is placed.
-- Rows and columns are numbered from 1 to 8.
-- For example, [4,2,7,3,6,8,5,1] is a solution where the
-- the queen in row 1 is in column 4, etc.
-- Any solution must be a permutation of [1,2,...,8].

-- The state of a queen is its position, row and column, on the board.
-- Operation column is a particularly simple instance
-- of a Constrained Constructor pattern.
-- When it is invoked, it produces only valid states.

column = 1 ! 2 ! 3 ! 4 ! 5 ! 6 ! 7 ! 8

-- A path of the puzzle is a sequence of successive placements of
-- queens on the board.  It is not explicitly defined as a type.
-- A path is a potential solution in the making.

-- Constrained Constructor on a path
-- Any path must be valid, i.e., any column must be in the range 1..8
-- and different from any other column in the path.
-- Furthermore, the path must be safe for the queens.
-- No queen in a path may capture any other queen in the path.
-- Operation makePath add column n to path c or fails.

makePath c n | valid c && safe c 1 = n:c
    where valid c | n =:= column = uniq c
             where uniq [] = True
                   uniq (c:cs) = n /= c && uniq cs
          safe [] _ = True
          safe (c:cs) k = abs (n-c) /= k && safe cs (k+1)
             where abs x = if x < 0 then -x else x

-- extend the path argument till all the queens are on the board
-- see the Incremental Solution pattern

extend p = if (length p == 8)
             then p
             else extend (makePath p x)
      where x free

-- solve the puzzle

main = extend []

```

Mainly [http://www-ps.informatik.uni-kiel.de/~pakcs/webpakcs/main.cgi?queens webpakcs], uses constraint-solver.

```curry
import CLPFD
import Findall

queens n qs =
    qs =:= [_ | _ <- [1..n]]
  & domain qs 1 (length qs)
  & allDifferent qs
  & allSafe qs
  & labeling [FirstFail] qs

allSafe [] = success
allSafe (q:qs) = safe q qs 1 & allSafe qs

safe :: Int -> [Int] -> Int -> Success
safe _     []  _ = success
safe q (q1:qs) p = q /=# q1+#p & q /=# q1-#p & safe q qs (p+#1)

-- oneSolution  = unpack  $ queens 8
-- allSolutions = findall $ queens 8
```



## D


### Short Version

This high-level version uses the second solution of the Permutations task.

```d
void main() {
    import std.stdio, std.algorithm, std.range, permutations2;

    enum n = 8;
    n.iota.array.permutations.filter!(p =>
        n.iota.map!(i => p[i] + i).array.sort().uniq.count == n &&
        n.iota.map!(i => p[i] - i).array.sort().uniq.count == n)
    .count.writeln;
}
```

{{out}}

```txt
92
```



### Intermediate Version

This version shows all the solutions.
{{trans|C}}

```d
enum side = 8;
__gshared int[side] board;

bool isUnsafe(in int y) nothrow @nogc {
    immutable int x = board[y];
    foreach (immutable i; 1 .. y + 1) {
        immutable int t = board[y - i];
        if (t == x || t == x - i || t == x + i)
            return true;
    }

    return false;
}

void showBoard() nothrow @nogc {
    import core.stdc.stdio;

    static int s = 1;
    printf("\nSolution #%d:\n", s++);
    foreach (immutable y; 0 .. side) {
        foreach (immutable x; 0 .. side)
            putchar(board[y] == x ? 'Q' : '.');
        putchar('\n');
    }
}

void main() nothrow @nogc {
    int y = 0;
    board[0] = -1;

    while (y >= 0) {
        do {
            board[y]++;
        } while (board[y] < side && y.isUnsafe);

        if (board[y] < side) {
            if (y < (side - 1))
                board[++y] = -1;
            else
                showBoard;
        } else
            y--;
    }
}
```

{{out}}

```txt

Solution #1:
Q.......
....Q...
.......Q
.....Q..
..Q.....
......Q.
.Q......
...Q....

[...]

Solution #91:
.......Q
..Q.....
Q.......
.....Q..
.Q......
....Q...
......Q.
...Q....

Solution #92:
.......Q
...Q....
Q.......
..Q.....
.....Q..
.Q......
......Q.
....Q...
```



### Fast Version

{{trans|C}}

```d
ulong nQueens(in uint nn) pure nothrow @nogc @safe
in {
    assert(nn > 0 && nn <= 27,
           "'side' value must be in 1 .. 27.");
} body {
    if (nn < 4)
        return nn == 1;

    enum uint ulen = uint.sizeof * 8;
    immutable uint full = uint.max - ((1 << (ulen - nn)) - 1);
    immutable n = nn - 3;

    typeof(return) count;
    uint[32] l=void, r=void, c=void;
    uint[33] mm; // mm and mmi are a stack.

    // Require second queen to be left of the first queen, so
    // we ever only test half of the possible solutions. This
    // is why we can't handle n=1 here.
    for (uint b0 = 1U << (ulen - n - 3); b0; b0 <<= 1) {
        for (uint b1 = b0 << 2; b1; b1 <<= 1) {
            uint d = n;
            // c: columns occupied by previous queens.
            c[n] = b0 | b1;
            // l: columns attacked by left diagonals.
            l[n] = (b0 << 2) | (b1 << 1);
            // r: by right diagnoals.
            r[n] = (b0 >> 2) | (b1 >> 1);

            // Availabe columns on current row.
            uint bits = full & ~(l[n] | r[n] | c[n]);

            uint mmi = 1;
            mm[mmi] = bits;

            while (bits) {
                // d: depth, aka row. counting backwards.
                // Because !d is often faster than d != n.
                while (d) {
                    // immutable uint pos = 1U << bits.bsf; // Slower.
                    immutable uint pos = -int(bits) & bits;

                    // Mark bit used. Only put current bits on
                    // stack if not zero, so backtracking will
                    // skip exhausted rows (because reading stack
                    // variable is slow compared to registers).
                    bits &= ~pos;
                    if (bits) {
                        mm[mmi] = bits | d;
                        mmi++;
                    }

                    d--;
                    l[d] = (l[d + 1] | pos) << 1;
                    r[d] = (r[d + 1] | pos) >> 1;
                    c[d] =  c[d + 1] | pos;

                    bits = full & ~(l[d] | r[d] | c[d]);

                    if (!bits)
                        break;
                    if (!d) {
                        count++;
                        break;
                    }
                }

                // Bottom of stack m is a zero'd field acting as
                // sentinel.  When saving to stack, left 27 bits
                // are the available columns, while right 5 bits
                // is the depth. Hence solution is limited to size
                // 27 board -- not that it matters in foreseeable
                // future.
                mmi--;
                bits = mm[mmi];
                d = bits & 31U;
                bits &= ~31U;
            }
        }
    }

    return count * 2;
}

void main(in string[] args) {
    import std.stdio, std.conv;

    immutable uint side = (args.length >= 2) ? args[1].to!uint : 8;
    writefln("N-queens(%d) = %d solutions.", side, side.nQueens);
}
```

{{out}}

```txt
N-queens(8) = 92 solutions.
```

With side = 17:

```txt
N-queens(17) = 95815104 solutions.
```

Run-time for side = 17 compiled with ldc2 is about 49.5 seconds.

```txt
N-queens(19) = 4968057848 solutions.
```



## Dart


```dart
/**
Return true if queen placement q[n] does not conflict with
other queens q[0] through q[n-1]
*/
isConsistent(List q, int n) {
  for (int i=0; i<n; i++) {
    if (q[i] == q[n]) {
      return false; // Same column
    }
    
    if ((q[i] - q[n]) == (n - i)) {
      return false; // Same major diagonal
    }
    
    if ((q[n] - q[i]) == (n - i)) {
      return false; // Same minor diagonal
    }
  }
  
  return true;
}

/**
Print out N-by-N placement of queens from permutation q in ASCII. 
*/
printQueens(List q) {
  int N = q.length;
  for (int i=0; i<N; i++) {
    StringBuffer sb = new StringBuffer();
    for (int j=0; j<N; j++) {
      if (q[i] == j) {
        sb.write("Q ");
      } else {
        sb.write("* ");
      }
    }
    print(sb.toString());
  }
  print("");
}

/**
Try all permutations using backtracking
*/
enumerate(int N) {
  var a = new List(N);
  _enumerate(a, 0);
}

_enumerate(List q, int n) {
  if (n == q.length) {
    printQueens(q);
  } else {
    for (int i = 0; i < q.length; i++) {
      q[n] = i;
      if (isConsistent(q, n)){
        _enumerate(q, n+1);
      }
    } 
  }
}

void main() {
  enumerate(4);
}
```

{{out}}

```txt
* Q * * 
* * * Q 
Q * * * 
* * Q * 

* * Q * 
Q * * * 
* * * Q 
* Q * * 

```



## EasyLang


<lang>subr show_sol
  print "Solution " & n_sol
  print ""
  for i range n
    write "  "
    for j range n
      if j = x[i]
        write "Q "
      else
        write ". "
      .
    .
    print ""
  .
  print ""
.
subr test
  ok = 1
  for i range y
    if x[y] = x[i] or abs (x[i] - x[y]) = abs (y - i)
      ok = 0
    .
  .
.
n = 8
len x[] n
y = 0
x[0] = 0
while y >= 0
  call test
  if ok = 1 and y + 1 <> n
    y += 1
    x[y] = 0
  else
    if ok = 1
      n_sol += 1
      if n_sol <= 1
        call show_sol
      .
    .
    while y >= 0 and x[y] = n - 1
      y -= 1
    .
    if y >= 0
      x[y] += 1
    .
  .
.
print n_sol & " solutions"
```

{{out}}

```txt
Solution 1

  Q . . . . . . . 
  . . . . Q . . . 
  . . . . . . . Q 
  . . . . . Q . . 
  . . Q . . . . . 
  . . . . . . Q . 
  . Q . . . . . . 
  . . . Q . . . . 

92 solutions
```



## EchoLisp


```scheme

;; square num is i + j*N
(define-syntax-rule (sq i j) (+ i (* j N)))

;; compute diag number for each square
(define (do-diag1 i0 j0  dnum  into: dnum1 N) ;; ++i and ++j diags
	(for [(i (in-range i0 N)) (j (in-range j0 N))]
		;;(writeln   i j 'diag1 dnum)
		(vector-set! dnum1 (sq i j) dnum)))
		
(define (do-diag2 i0 j0  dnum into: dnum2 N) ;; --i and ++j diags
	(for [(i (in-range i0 -1 -1)) (j (in-range j0 N))]
		;; (writeln i j 'diag2 dnum)
		(vector-set! dnum2 (sq i j) dnum)))
		
(define (init-diags dnum1 dnum2 N)
	(define dnum 0)
		(for ((j N)) (do-diag1 0 j dnum dnum1 N) (++ dnum))
		(for ((i (in-range 1 N))) 
                     (do-diag1 i 0 dnum dnum1  N) (++ dnum))
	(set! dnum 0)
		(for ((j N)) (do-diag2 (1- N) j dnum dnum2 N) (++ dnum))
		(for ((i (1- N))) (do-diag2 i 0 dnum dnum2 N) (++ dnum)))
;; end boring diags part
		
(define (q-search i  N col diag1 diag2 dnum1 dnum2    &hits (ns))
(cond
[(= i N)  (set-box! &hits (1+ (unbox &hits))) ] ;;  (writeln  'HIT col)
	[else
	
		(for ((j N))
		(set! ns (sq i j))
		#:continue (or [col j] [diag1 [dnum1 ns]] [diag2 [dnum2 ns]])
		     (vector-set! col j i) ;; move
		     (vector-set! diag1 [dnum1 ns] #t) ;; flag busy diagonal
			(vector-set! diag2 [dnum2 ns] #t)
			(q-search (1+ i) N col diag1 diag2 dnum1 dnum2 &hits)
			(vector-set! col j #f) ;; unmove
			(vector-set! diag1 [dnum1 ns] #f)
			(vector-set! diag2 [dnum2 ns] #f))
			]))
			
(define (q-count (N 8))
	(define dnum1 (make-vector (* N N)))
	(define dnum2 (make-vector (* N N )))
	(init-diags dnum1 dnum2 N)
	
	(define diag1 (make-vector (* 2 N) #f)) ; busy diag's
	(define diag2 (make-vector (* 2 N) #f))
	(define col (make-vector N  #f))
	(define &hits (box 0))
	
	
	(q-search 0 N col diag1 diag2 dnum1 dnum2  &hits)
	(unbox &hits))
	
(define (task up-to-n)
	(for ((i up-to-n)) (writeln i ' ♕ (q-count i) 'solutions)))

```

{{out}}

```txt

(task 13)

0     ♕     1     solutions    
1     ♕     1     solutions    
2     ♕     0     solutions    
3     ♕     0     solutions    
4     ♕     2     solutions    
5     ♕     10     solutions    
6     ♕     4     solutions    
7     ♕     40     solutions    
8     ♕     92     solutions    
9     ♕     352     solutions    
10     ♕     724     solutions    
11     ♕     2680     solutions 
12     ♕     14200     solutions    

```




## Eiffel


```Eiffel

class
	QUEENS

create
	make

feature {NONE}
	counter: INTEGER

	place_queens(board: ARRAY[INTEGER]; level: INTEGER)
		local
			i, j: INTEGER
			safe: BOOLEAN
		do
			if level > board.count
			then
				counter := counter + 1
			else
				from
					i := 1
				until
					i > board.count
				loop
					safe := True
					from
						j := 1
					until
						j = level or not safe
					loop
						if (board[j] = i)
							or (j - level = i - board[j])
							or (j - level = board[j] - i)
						then
							safe := False
						end
						j := j + 1
					end
					if safe
					then
						board[level] := i
						place_queens(board, level + 1)
					end
					i := i + 1
				end
			end
		end

feature
	possible_positions_of_n_queens(n: INTEGER): INTEGER
		local
			board: ARRAY[INTEGER]
		do
			create board.make_filled (0, 1, n)
			counter := 0
			place_queens(board, 1)
			Result := counter
		end

	make
		local
			n: INTEGER
		do
			io.put_string ("Please enter the number of queens: ")
			io.read_integer
			n := io.last_integer
			print("%NPossible number of placings: " + possible_positions_of_n_queens(n).out + "%N")
		end
end

```

{{out}}

```txt

Please enter the number of queens: 1
Possible number of placings: 1

Please enter the number of queens: 2
Possible number of placings: 0

Please enter the number of queens: 3
Possible number of placings: 0

Please enter the number of queens: 4
Possible number of placings: 2

Please enter the number of queens: 5
Possible number of placings: 10

Please enter the number of queens: 6
Possible number of placings: 4

Please enter the number of queens: 7
Possible number of placings: 40

Please enter the number of queens: 8
Possible number of placings: 92

Please enter the number of queens: 9
Possible number of placings: 352

Please enter the number of queens: 10
Possible number of placings: 724

```



## Elixir

{{trans|Ruby}}

```elixir
defmodule RC do
  def queen(n, display \\ true) do
    solve(n, [], [], [], display)
  end
  
  defp solve(n, row, _, _, display) when n==length(row) do
    if display, do: print(n,row)
    1
  end
  defp solve(n, row, add_list, sub_list, display) do
    Enum.map(Enum.to_list(0..n-1) -- row, fn x ->
      add = x + length(row)             # \ diagonal check
      sub = x - length(row)             # / diagonal check
      if (add in add_list) or (sub in sub_list) do
        0
      else
        solve(n, [x|row], [add | add_list], [sub | sub_list], display)
      end
    end) |> Enum.sum                    # total of the solution
  end
  
  defp print(n, row) do
    IO.puts frame = "+" <> String.duplicate("-", 2*n+1) <> "+"
    Enum.each(row, fn x ->
      line = Enum.map_join(0..n-1, fn i -> if x==i, do: "Q ", else: ". " end)
      IO.puts "| #{line}|"
    end)
    IO.puts frame
  end
end

Enum.each(1..6, fn n ->
  IO.puts " #{n} Queen : #{RC.queen(n)}"
end)

Enum.each(7..12, fn n ->
  IO.puts " #{n} Queen : #{RC.queen(n, false)}"             # no display
end)
```


{{out}}
<pre style="height: 80ex; overflow: scroll">
+---+
| Q |
+---+
 1 Queen : 1
 2 Queen : 0
 3 Queen : 0
+---------+
| . . Q . |
| Q . . . |
| . . . Q |
| . Q . . |
+---------+
+---------+
| . Q . . |
| . . . Q |
| Q . . . |
| . . Q . |
+---------+
 4 Queen : 2
+-----------+
| . . . Q . |
| . Q . . . |
| . . . . Q |
| . . Q . . |
| Q . . . . |
+-----------+
+-----------+
| . . Q . . |
| . . . . Q |
| . Q . . . |
| . . . Q . |
| Q . . . . |
+-----------+
+-----------+
| . . . . Q |
| . . Q . . |
| Q . . . . |
| . . . Q . |
| . Q . . . |
+-----------+
+-----------+
| . . . Q . |
| Q . . . . |
| . . Q . . |
| . . . . Q |
| . Q . . . |
+-----------+
+-----------+
| . . . . Q |
| . Q . . . |
| . . . Q . |
| Q . . . . |
| . . Q . . |
+-----------+
+-----------+
| Q . . . . |
| . . . Q . |
| . Q . . . |
| . . . . Q |
| . . Q . . |
+-----------+
+-----------+
| . Q . . . |
| . . . . Q |
| . . Q . . |
| Q . . . . |
| . . . Q . |
+-----------+
+-----------+
| Q . . . . |
| . . Q . . |
| . . . . Q |
| . Q . . . |
| . . . Q . |
+-----------+
+-----------+
| . . Q . . |
| Q . . . . |
| . . . Q . |
| . Q . . . |
| . . . . Q |
+-----------+
+-----------+
| . Q . . . |
| . . . Q . |
| Q . . . . |
| . . Q . . |
| . . . . Q |
+-----------+
 5 Queen : 10
+-------------+
| . . . . Q . |
| . . Q . . . |
| Q . . . . . |
| . . . . . Q |
| . . . Q . . |
| . Q . . . . |
+-------------+
+-------------+
| . . . Q . . |
| Q . . . . . |
| . . . . Q . |
| . Q . . . . |
| . . . . . Q |
| . . Q . . . |
+-------------+
+-------------+
| . . Q . . . |
| . . . . . Q |
| . Q . . . . |
| . . . . Q . |
| Q . . . . . |
| . . . Q . . |
+-------------+
+-------------+
| . Q . . . . |
| . . . Q . . |
| . . . . . Q |
| Q . . . . . |
| . . Q . . . |
| . . . . Q . |
+-------------+
 6 Queen : 4
 7 Queen : 40
 8 Queen : 92
 9 Queen : 352
 10 Queen : 724
 11 Queen : 2680
 12 Queen : 14200

```



## Erlang

Instead of spawning a new process to search for each possible solution I backtrack.

```Erlang

-module( n_queens ).

-export( [display/1, solve/1, task/0] ).

display( Board ) ->
	%% Queens are in the positions in the Board list.
	%% Top left corner is {1, 1}, Bottom right is {N, N}. There is a queen in the max column.
	N = lists:max( [X || {X, _Y} <- Board] ),
	[display_row(Y, N, Board) || Y <- lists:seq(1, N)].

solve( N ) ->
    Positions = [{X, Y} || X <- lists:seq(1, N), Y <- lists:seq(1, N)],
    try
    bt( N, Positions, [] )

    catch
    _:{ok, Board} -> Board

    end.

task() ->
    task( 4 ),
    task( 8 ).



bt( N, Positions, Board ) -> bt_reject( is_not_allowed_queen_placement(N, Board), N, Positions, Board ).

bt_accept( true, _N, _Positions, Board ) -> erlang:throw( {ok, Board} );
bt_accept( false, N, Positions, Board ) -> bt_loop( N, Positions, [], Board ).

bt_loop( _N, [], _Rejects, _Board ) -> failed;
bt_loop( N, [Position | T], Rejects, Board ) ->
	bt( N, T ++ Rejects, [Position | Board] ),
	bt_loop( N, T, [Position | Rejects], Board ).

bt_reject( true, _N, _Positions, _Board ) -> backtrack;
bt_reject( false, N, Positions, Board ) -> bt_accept( is_all_queens(N, Board), N, Positions, Board ).

diagonals( N, {X, Y} ) ->
	D1 = diagonals( N, X + 1, fun diagonals_add1/1, Y + 1, fun diagonals_add1/1 ),
	D2 = diagonals( N, X + 1, fun diagonals_add1/1, Y - 1, fun diagonals_subtract1/1 ),
	D3 = diagonals( N, X - 1, fun diagonals_subtract1/1, Y + 1, fun diagonals_add1/1 ),
	D4 = diagonals( N, X - 1, fun diagonals_subtract1/1, Y - 1, fun diagonals_subtract1/1 ),
	D1 ++ D2 ++ D3 ++ D4.

diagonals( _N, 0, _Change_x, _Y, _Change_y ) -> [];
diagonals( _N, _X, _Change_x, 0, _Change_y ) -> [];
diagonals( N, X, _Change_x, _Y, _Change_y ) when X > N -> [];
diagonals( N, _X, _Change_x, Y, _Change_y ) when Y > N -> [];
diagonals( N, X, Change_x, Y, Change_y ) -> [{X, Y} | diagonals( N, Change_x(X), Change_x, Change_y(Y), Change_y )].

diagonals_add1( N ) -> N + 1.

diagonals_subtract1( N ) -> N - 1.

display_row( Row, N, Board ) ->
	[io:fwrite("~s", [display_queen(X, Row, Board)]) || X <- lists:seq(1, N)],
	io:nl().

display_queen( X, Y, Board ) -> display_queen( lists:member({X, Y}, Board) ).
display_queen( true ) -> " Q";
display_queen( false ) -> " .".

is_all_queens( N, Board ) -> N =:= erlang:length( Board ).

is_diagonal( _N, [] ) -> false;
is_diagonal( N, [Position | T] ) ->
	Diagonals = diagonals( N, Position ),
	T =/= (T -- Diagonals)
	orelse is_diagonal( N, T ).

is_not_allowed_queen_placement( N, Board ) ->
	Pieces = erlang:length( Board ),
	{Xs, Ys} = lists:unzip( Board ),
	Pieces =/= erlang:length( lists:usort(Xs) )
	orelse Pieces =/= erlang:length( lists:usort(Ys) )
	orelse is_diagonal( N, Board ).

task( N ) ->
    io:fwrite( "N = ~p. One solution.~n", [N] ),
    Board = solve( N ),
    display( Board ).

```

{{out}}

```txt

22> n_queens:task().
N = 4. One solution.
 . . Q .
 Q . . .
 . . . Q
 . Q . .
N = 8. One solution.
 Q . . . . . . .
 . . . . . . Q .
 . . . . Q . . .
 . . . . . . . Q
 . Q . . . . . .
 . . . Q . . . .
 . . . . . Q . .
 . . Q . . . . .

```



## ERRE

<lang>
!------------------------------------------------
! QUEENS.R : solve queens problem on a NxN board
!------------------------------------------------

PROGRAM QUEENS

DIM COL%[15]

BEGIN
  MAXSIZE%=15
  PRINT(TAB(25);" --- PROBLEMA DELLE REGINE --- ")
  PRINT
  PRINT("Board dimension ";)
  INPUT(N%)
  PRINT
  IF (N%<1 OR N%>MAXSIZE%)
    THEN
      PRINT("Illegal dimension!!")
    ELSE
      FOR CURCOLNBR%=1 TO N%
        COL%[CURCOLNBR%]=0
      END FOR
      CURCOLNBR%=1
      WHILE CURCOLNBR%>0 DO
        PLACEDAQUEEN%=FALSE
        I%=COL%[CURCOLNBR%]+1
        WHILE (I%<=N%) AND NOT PLACEDAQUEEN% DO
          PLACEDAQUEEN%=TRUE
          J%=1
          WHILE PLACEDAQUEEN% AND (J%<CURCOLNBR%) DO
            PLACEDAQUEEN%=COL%[J%]<>I%
            J%=J%+1
          END WHILE
          IF PLACEDAQUEEN%
            THEN
              DIAGNBR%=I%+CURCOLNBR%
              J%=1
              WHILE PLACEDAQUEEN% AND (J%<CURCOLNBR%) DO
                PLACEDAQUEEN%=(COL%[J%]+J%)<>DIAGNBR%
                J%=J%+1
              END WHILE
            ELSE
          END IF
          IF PLACEDAQUEEN%
            THEN
              DIAGNBR%=I%-CURCOLNBR%
              J%=1
              WHILE PLACEDAQUEEN% AND (J%<CURCOLNBR%) DO
                 PLACEDAQUEEN%=(COL%[J%]-J%)<>DIAGNBR%
                 J%=J%+1
              END WHILE
            ELSE
          END IF
          IF NOT PLACEDAQUEEN%
            THEN
              I%=I%+1
            ELSE
              COL%[CURCOLNBR%]=I%
          END IF
        END WHILE
        IF NOT PLACEDAQUEEN%
          THEN
            COL%[CURCOLNBR%]=0
            CURCOLNBR%=CURCOLNBR%-1
          ELSE
            IF CURCOLNBR%=N%
              THEN
                NSOL%=NSOL%+1
                PRINT("Soluzione";NSOL%;":";)
                FOR I%=1 TO N%
                  PRINT(COL%[I%];)
                END FOR
                PRINT
              ELSE
                CURCOLNBR%=CURCOLNBR%+1
            END IF
        END IF
      END WHILE
      PRINT("Search completed")
      REPEAT
         GET(CH$)
      UNTIL CH$<>""
    END IF
END PROGRAM

```

Note: The program prints solutions one per line. This version works well for the PC and the C-64. For PC only you can omit the % integer-type specificator with a <code>!$INTEGER</code> pragma directive.


## F Sharp


```fsharp

let rec iterate f value = seq { 
    yield value
    yield! iterate f (f value) }

let up i = i + 1
let right i = i
let down i = i - 1

let noCollisionGivenDir solution number dir =
    Seq.forall2 (<>) solution (Seq.skip 1 (iterate dir number))

let goodAddition solution number =
    List.forall (noCollisionGivenDir solution number) [ up; right; down ]

let rec extendSolution n ps =
    [0..n - 1]
    |> List.filter (goodAddition ps)
    |> List.map (fun num -> num :: ps)

let allSolutions n =
    iterate (List.collect (extendSolution n)) [[]]

// Print one solution for the 8x8 case
let printOneSolution () =
    allSolutions 8
    |> Seq.item 8
    |> Seq.head
    |> List.iter (fun rowIndex ->
        printf "|"
        [0..8] |> List.iter (fun i -> printf (if i = rowIndex then "X|" else " |"))
        printfn "")

// Print number of solution for the other cases
let printNumberOfSolutions () =
    printfn "Size\tNr of solutions"
    [1..11]
    |> List.map ((fun i -> Seq.item i (allSolutions i)) >> List.length)
    |> List.iteri (fun i cnt -> printfn "%d\t%d" (i+1) cnt)

printOneSolution()

printNumberOfSolutions()

```


The output:

<lang>
| | | |X| | | | | |
| |X| | | | | | | |
| | | | | | |X| | |
| | |X| | | | | | |
| | | | | |X| | | |
| | | | | | | |X| |
| | | | |X| | | | |
|X| | | | | | | | |

Size    Nr of solutions
1       1
2       0
3       0
4       2
5       10
6       4
7       40
8       92
9       352
10      724
11      2680

```



## Factor

{{works with|Factor|0.98}}

```factor
USING: kernel sequences math math.combinatorics formatting io locals ;
IN: queens

: /=  ( x y -- ? )   = not ; inline

:: safe?  ( board q -- ? )
    [let  q board nth :> x
      q <iota> [
         x swap
         [ board nth ] keep
         q swap -
           [ + /= ]
           [ - /= ] 3bi and
      ] all?
    ] ;

: solution? ( board -- ? )
    dup length <iota> [ dupd safe? ] all? nip ;

: queens ( n -- l )
    <iota> all-permutations [ solution? ] filter ;

: .queens ( n -- )
    queens
    [ 
      [ 1 + "%d " printf ] each nl
    ] each ;
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/N-queens_problem this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth
variable solutions
variable nodes

: bits ( n -- mask ) 1 swap lshift 1- ;
: lowBit  ( mask -- bit ) dup negate and ;
: lowBit- ( mask -- bits ) dup 1- and ;

: next3 ( dl dr f files -- dl dr f dl' dr' f' )
  invert >r
  2 pick r@ and 2* 1+
  2 pick r@ and 2/
  2 pick r> and ;

: try ( dl dr f -- )
  dup if
    1 nodes +!
    dup 2over and and
    begin ?dup while
      dup >r lowBit next3 recurse r> lowBit-
    repeat
  else 1 solutions +! then
  drop 2drop ;

: queens ( n -- )
  0 solutions ! 0 nodes !
  -1 -1 rot bits try
  solutions @ . ." solutions, " nodes @ . ." nodes" ;

8 queens  \ 92 solutions, 1965 nodes
```



## Fortran

{{works with|Fortran|95 and later}}

Using a back tracking method to find one solution

```fortran
program Nqueens
  implicit none

  integer, parameter :: n = 8  ! size of board
  integer :: file = 1, rank = 1, queens = 0
  integer :: i
  logical :: board(n,n) = .false.

  do while (queens < n)
    board(file, rank) = .true.
    if(is_safe(board, file, rank)) then
      queens = queens + 1
      file = 1
      rank = rank + 1
    else
      board(file, rank) = .false.
      file = file + 1
      do while(file > n)
         rank = rank - 1
         if (rank < 1) then
           write(*, "(a,i0)") "No solution for n = ", n
           stop
         end if  
         do i = 1, n
           if (board(i, rank)) then
             file = i
             board(file, rank) = .false.
             queens = queens - 1
             file = i + 1
             exit
           end if
         end do
       end do
    end if
  end do

  call Printboard(board)
  
contains

function is_safe(board, file, rank)
  logical :: is_safe
  logical, intent(in) :: board(:,:)
  integer, intent(in) :: file, rank
  integer :: i, f, r
  
  is_safe = .true.
  do i = rank-1, 1, -1
    if(board(file, i)) then
      is_safe = .false.
      return
    end if
  end do
  
  f = file - 1
  r = rank - 1
  do while(f > 0 .and. r > 0)
    if(board(f, r)) then
      is_safe = .false.
      return
    end if
    f = f - 1
    r = r - 1
  end do

  f = file + 1
  r = rank - 1
  do while(f <= n .and. r > 0)
    if(board(f, r)) then
      is_safe = .false.
      return
    end if
    f = f + 1
    r = r - 1
  end do
end function    

subroutine Printboard(board)
  logical, intent(in) :: board(:,:)
  character(n*4+1) :: line
  integer :: f, r
  
  write(*, "(a, i0)") "n = ", n
  line = repeat("+---", n) // "+"
  do r = 1, n
    write(*, "(a)") line
    do f = 1, n
      write(*, "(a)", advance="no") "|"
      if(board(f, r)) then
        write(*, "(a)", advance="no") " Q "
      else if(mod(f+r, 2) == 0) then
        write(*, "(a)", advance="no") "   "
      else
        write(*, "(a)", advance="no") "###"
      end if
    end do
    write(*, "(a)") "|"
  end do
  write(*, "(a)") line
end subroutine
end program
```

{{out}} for 8, 16 and 32 queens
<pre style="height:40ex;overflow:scroll">n = 8
+---+---+---+---+---+---+---+---+
| Q |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+
|###|   |###|   | Q |   |###|   |
+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   | Q |
+---+---+---+---+---+---+---+---+
|###|   |###|   |###| Q |###|   |
+---+---+---+---+---+---+---+---+
|   |###| Q |###|   |###|   |###|
+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   | Q |   |
+---+---+---+---+---+---+---+---+
|   | Q |   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+
|###|   |###| Q |###|   |###|   |
+---+---+---+---+---+---+---+---+

n = 16
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| Q |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   | Q |   |###|   |###|   |###|   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###| Q |###|   |###|   |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###| Q |###|   |###|   |###|   |###|   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   |###|   |###| Q |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   |###|   | Q |   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   |###|   |###|   | Q |   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   |###|   |###|   |###| Q |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   |###|   |###|   |###| Q |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###| Q |###|   |###|   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   | Q |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   | Q |   |###|   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   | Q |   |###|   |###|   |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   |###|   |###|   | Q |   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   | Q |   |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   |###|   |###| Q |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+

n = 32
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| Q |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   | Q |   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###| Q |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###| Q |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   | Q |   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   |###|   | Q |   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   |###| Q |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   |###|   |###|   |###|   | Q |   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   |###|   |###|   |###| Q |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###| Q |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   | Q |   |###|   |###|   |###|   |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###| Q |###|   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   | Q |   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###| Q |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###| Q |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   | Q |   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   | Q |   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###| Q |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###| Q |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   | Q |   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   | Q |   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   | Q |   |###|   |###|   |###|   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   | Q |   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   |###| Q |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###| Q |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   |###|   |###|   |###| Q |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###| Q |###|   |###|   |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   | Q |   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   |###|   |###|   | Q |   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   | Q |   |###|   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   | Q |   |###|   |###|   |###|   |###|   |###|   |###|
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###|   |###| Q |###|   |###|   |###|   |###|   |###|   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
```



### Alternate Fortran 77 solution


```fortran
C This one implements depth-first backtracking.
C See the 2nd program for Scheme on the "Permutations" page for the
C main idea.
C As is, the program only prints the number of n-queens configurations.
C To print also the configurations, uncomment the line after label 80.
      program queens
      implicit integer(a-z)
      parameter(l=18)
      dimension a(l),s(l),u(4*l-2)
      do 10 i=1,l
   10 a(i)=i
      do 20 i=1,4*l-2
   20 u(i)=0
      do 110 n=1,l
      m=0
      i=1
      r=2*n-1
      go to 40
   30 s(i)=j
      u(p)=1
      u(q+r)=1
      i=i+1
   40 if(i.gt.n) go to 80
      j=i
   50 z=a(i)
      y=a(j)
      p=i-y+n
      q=i+y-1
      a(i)=y
      a(j)=z
      if((u(p).eq.0).and.(u(q+r).eq.0)) goto 30
   60 j=j+1
      if(j.le.n) go to 50
   70 j=j-1
      if(j.eq.i) go to 90
      z=a(i)
      a(i)=a(j)
      a(j)=z
      go to 70
   80 m=m+1
C     print *,(a(k),k=1,n)
   90 i=i-1
      if(i.eq.0) go to 100
      p=i-a(i)+n
      q=i+a(i)-1
      j=s(i)
      u(p)=0
      u(q+r)=0
      go to 60
  100 print *,n,m
  110 continue
      end
 
C Output
C          1           1
C          2           0
C          3           0
C          4           2
C          5          10
C          6           4
C          7          40
C          8          92
C          9         352
C         10         724
C         11        2680
C         12       14200
C         13       73712
C         14      365596
C         15     2279184
C         16    14772512
C         17    95815104
C         18   666090624

```



```fortran
!The preceding program implements recursion using arrays, since Fortran 77 does not allow recursive
!functions. The same algorithm is much easier to follow in Fortran 90, using the RECURSIVE keyword.
!Like previously, the program only counts solutions. It's pretty straightforward to adapt it to print
!them too: one has to replace the 'm = m + 1' instruction with a PRINT statement.

function numq(n)
    implicit none
    integer :: i, n, m, a(n), numq
    logical :: up(2*n - 1), down(2*n - 1)
    do i = 1, n
        a(i) = i
    end do
    up = .true.
    down = .true.
    m = 0
    call sub(1)
    numq = m
contains
    recursive subroutine sub(i)
        integer :: i, j, k, p, q, s
        do k = i, n
            j = a(k)
            p = i + j - 1
            q = i - j + n
            if(up(p) .and. down(q)) then
                if(i == n) then
                    m = m + 1
                else
                    up(p) = .false.
                    down(q) = .false.
                    s = a(i)
                    a(i) = a(k)
                    a(k) = s
                    call sub(i + 1)
                    up(p) = .true.
                    down(q) = .true.
                    s = a(i)
                    a(i) = a(k)
                    a(k) = s
                end if
            end if
        end do
    end subroutine
end function

program queens
    implicit none
    integer :: numq, n, m
    do n = 4, 16
        m = numq(n)
        print *, n, m
    end do
end program
```



### Alternate Fortran 95 solution with OpenMP

This code is useful mainly for counting solutions. Here we use the same algorithm as with Fortran 77,
with an optimization: because of symmetry of the chess board, computations are divided by two.
The remaining is parallelized with OpenMP. The loop is done on the valid combinations of queens
in the first two columns. The original algorithm is slightly changed to start backtracking from
column three.

If using GCC, compile with ''gfortran -O2 -fopenmp queens.f90''. With Absoft Pro Fortran, ''af90 -O2 -openmp queens.f90'', and with Intel Fortran, ''ifort /fast /openmp queens.f90''.

With some versions of GCC the function OMP_GET_WTIME is not known, which seems to be a bug. Then it's enough to comment out the two calls, and the program won't display timings.


```fortran
program queens
    use omp_lib
    implicit none
    integer, parameter :: long = selected_int_kind(17)
    integer, parameter :: l = 18
    integer :: n, i, j, a(l*l, 2), k, p, q
    integer(long) :: s, b(l*l)
    real(kind(1d0)) :: t1, t2

    do n = 6, l
        k = 0
        p = n/2
        q = mod(n, 2)*(p + 1)
        do i = 1, n
            do j = 1, n
                if ((abs(i - j) > 1) .and. ((i <= p) .or. ((i == q) .and. (j < i)))) then
                    k = k + 1
                    a(k, 1) = i
                    a(k, 2) = j
                end if
            end do
        end do
        s = 0
        t1 = omp_get_wtime()
        !$omp parallel do schedule(dynamic)
        do i = 1, k
            b(i) = pqueens(n, a(i, 1), a(i, 2))
        end do
        !$omp end parallel do
        t2 = omp_get_wtime()
        print "(I4, I12, F12.3)", n, 2*sum(b(1:k)), t2 - t1
    end do
    
contains
    function pqueens(n, k1, k2) result(m)
        implicit none
        integer(long) :: m
        integer, intent(in) :: n, k1, k2
        integer, parameter :: l = 20
        integer :: a(l), s(l), u(4*l - 2)
        integer :: i, j, y, z, p, q, r

        do i = 1, n
            a(i) = i
        end do
        
        do i = 1, 4*n - 2
            u(i) = 0
        end do
        
        m = 0
        r = 2*n - 1
        if (k1 == k2) return

        p = 1 - k1 + n
        q = 1 + k1 - 1
        if ((u(p) /= 0) .or. (u(q + r) /= 0)) return

        u(p) = 1
        u(q + r) = 1
        z = a(1)
        a(1) = a(k1)
        a(k1) = z
        p = 2 - k2 + n
        q = 2 + k2 - 1
        if ((u(p) /= 0) .or. (u(q + r) /= 0)) return

        u(p) = 1
        u(q + r) = 1
        if (k2 /= 1) then
            z = a(2)
            a(2) = a(k2)
            a(k2) = z
        else
            z = a(2)
            a(2) = a(k1)
            a(k1) = z
        end if
        i = 3
        go to 40

     30 s(i) = j
        u(p) = 1
        u(q + r) = 1
        i = i + 1
     40 if (i > n) go to 80
 
        j = i

     50 z = a(i)
        y = a(j)
        p = i - y + n
        q = i + y - 1
        a(i) = y
        a(j) = z
        if ((u(p) == 0) .and. (u(q + r) == 0)) go to 30
        
     60 j = j + 1
        if (j <= n) go to 50
        
     70 j = j - 1
        if (j == i) go to 90
        
        z = a(i)
        a(i) = a(j)
        a(j) = z
        go to 70
        
        !valid queens position found
     80 m = m + 1
     
     90 i = i - 1
        if (i == 2) return
        
        p = i - a(i) + n
        q = i + a(i) - 1
        j = s(i)
        u(p) = 0
        u(q + r) = 0
        go to 60
    end function
end program
```


## FreeBASIC

Get slower for N > 14

```freebasic
' version 13-04-2017
' compile with: fbc -s console
Dim Shared As ULong count, c()

Sub n_queens(row As ULong, n As ULong, show As ULong = 0)

    Dim As ULong x, y

    For x = 1 To n
        
        For y = 1 To row -1
            If c(y) = x OrElse ((row - y) - Abs(x - c(y))) = 0 Then
                Continue For, For
            End If
        Next
        
        c(row) = x
        If row < n Then
            n_queens(row +1 , n, show)
        Else
            count += 1
            
            If show <> 0 Then
                For y = 1 To n
                    Print Using "###"; c(y);
                Next
                Print
            End If
            
        End If
        
    Next

End Sub

' ------=< MAIN >=------

Dim As ULong n = 5
ReDim c(n)
' n_queens(1, n, show = 0 only show total | show <> 0 show every solution
n_queens(1, n, 1)
Print Using "## x ## board, ##### solutions"; n; n; count
Print

For n = 1 To 14
    ReDim c(n)
    count = 0
    n_queens(1, n)
    Print Using "A ## x ## board has ######## solutions"; n; n; count
Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
  1  3  5  2  4
  1  4  2  5  3
  2  4  1  3  5
  2  5  3  1  4
  3  1  4  2  5
  3  5  2  4  1
  4  1  3  5  2
  4  2  5  3  1
  5  2  4  1  3
  5  3  1  4  2
 5 x  5 board,    10 solutions

A  1 x  1 board has        1 solutions
A  2 x  2 board has        0 solutions
A  3 x  3 board has        0 solutions
A  4 x  4 board has        2 solutions
A  5 x  5 board has       10 solutions
A  6 x  6 board has        4 solutions
A  7 x  7 board has       40 solutions
A  8 x  8 board has       92 solutions
A  9 x  9 board has      352 solutions
A 10 x 10 board has      724 solutions
A 11 x 11 board has     2680 solutions
A 12 x 12 board has    14200 solutions
A 13 x 13 board has    73712 solutions
A 14 x 14 board has   365596 solutions
```



###  Alternate version : recursive 



```freebasic
Sub aux(n As Integer, i As Integer, a() As Integer, _
        u() As Integer, v() As Integer, ByRef m As LongInt)

    Dim As Integer j, k, p, q
    If i > n Then
        m += 1
        For k = 1 To n : Print a(k); : Next : Print
    Else
        For j = i To n
            k = a(j)
            p = i - k + n
            q = i + k - 1
            If u(p) And v(q) Then
                u(p) = 0 : v(q) = 0
                a(j) = a(i) : a(i) = k
                aux(n, i + 1, a(), u(), v(), m)
                u(p) = 1 : v(q) = 1
                a(i) = a(j) : a(j) = k
            End If
        Next
    End If
End Sub

Dim As Integer n, i
Dim m As LongInt = 1
If Command(1) <> "" Then
    n = CInt(Command(1))
    ReDim a(1 To n) As Integer
    ReDim u(1 To 2 * n - 1) As Integer
    ReDim v(1 To 2 * n - 1) As Integer
    For i = 1 To n
        a(i) = i
    Next
    For i = 1 To 2 * n - 1
        u(i) = 1
        v(i) = 1
    Next
    m = 0
    aux(n, 1, a(), u(), v(), m)
    Print m
End If
```



###  Alternate version : iterative 



```freebasic
Dim As Integer n, i, j, k, p, q
Dim m As LongInt = 0

If Command(1) <> "" Then
    n = CInt(Command(1))
    ReDim a(1 To n) As Integer
    ReDim s(1 To n) As Integer
    ReDim u(1 To 2 * n - 1) As Integer
    ReDim v(1 To 2 * n - 1) As Integer
    For i = 1 To n
        a(i) = i
    Next
    For i = 1 To 2 * n - 1
        u(i) = 1
        v(i) = 1
    Next
    m = 0
    i = 1
L1: If i > n Then
        m += 1
        For k = 1 To n : Print a(k); : Next : Print
        Goto L4
    End If
    j = i
L2: k = a(j)
    p = i - k + n
    q = i + k - 1
    If u(p) And v(q) Then
        u(p) = 0 : v(q) = 0
        a(j) = a(i) : a(i) = k
        s(i) = j
        i += 1
        Goto L1
    End If
L3: j += 1 : If j <= n Goto L2
L4: i -= 1 : If i = 0 Then Print m : End
    j = s(i)
    k = a(i) : a(i) = a(j) : a(j) = k
    p = i - k + n
    q = i + k - 1
    u(p) = 1 : v(q) = 1
    Goto L3
End If
```



## GAP


Translation of Fortran 77. See also alternate Python implementation. One function to return the number of solutions, another to return the list of permutations.


```gap
NrQueens := function(n)
    local a, up, down, m, sub;
    a := [1 .. n];
    up := ListWithIdenticalEntries(2*n - 1, true);
    down := ListWithIdenticalEntries(2*n - 1, true);
    m := 0;
    sub := function(i)
        local j, k, p, q;
        for k in [i .. n] do
            j := a[k];
            p := i + j - 1;
            q := i - j + n;
            if up[p] and down[q] then
                if i = n then
                    m := m + 1;
                else
                    up[p] := false;
                    down[q] := false;
                    a[k] := a[i];
                    a[i] := j;
                    sub(i + 1);
                    up[p] := true;
                    down[q] := true;
                    a[i] := a[k];
                    a[k] := j;
                fi;
            fi;
        od;
    end;
    sub(1);
    return m;
end;

Queens := function(n)
    local a, up, down, v, sub;
    a := [1 .. n];
    up := ListWithIdenticalEntries(2*n - 1, true);
    down := ListWithIdenticalEntries(2*n - 1, true);
    v := [];
    sub := function(i)
        local j, k, p, q;
        for k in [i .. n] do
            j := a[k];
            p := i + j - 1;
            q := i - j + n;
            if up[p] and down[q] then
                if i = n then
                    Add(v, ShallowCopy(a));
                else
                    up[p] := false;
                    down[q] := false;
                    a[k] := a[i];
                    a[i] := j;
                    sub(i + 1);
                    up[p] := true;
                    down[q] := true;
                    a[i] := a[k];
                    a[k] := j;
                fi;
            fi;
        od;
    end;
    sub(1);
    return v;
end;

NrQueens(8);
a := Queens(8);;
PrintArray(PermutationMat(PermList(a[1]), 8));

[ [  1,  0,  0,  0,  0,  0,  0,  0 ],
  [  0,  0,  0,  0,  1,  0,  0,  0 ],
  [  0,  0,  0,  0,  0,  0,  0,  1 ],
  [  0,  0,  0,  0,  0,  1,  0,  0 ],
  [  0,  0,  1,  0,  0,  0,  0,  0 ],
  [  0,  0,  0,  0,  0,  0,  1,  0 ],
  [  0,  1,  0,  0,  0,  0,  0,  0 ],
  [  0,  0,  0,  1,  0,  0,  0,  0 ] ]
```



## Go

===Niklaus Wirth algorithm (Wikipedia)===

```go
// A fairly literal translation of the example program on the referenced
// WP page.  Well, it happened to be the example program the day I completed
// the task.  It seems from the WP history that there has been some churn
// in the posted example program.  The example program of the day was in
// Pascal and was credited to Niklaus Wirth, from his "Algorithms +
// Data Structures = Programs."
package main
 
import "fmt"
 
var (
    i int
    q bool
    a [9]bool
    b [17]bool
    c [15]bool // offset by 7 relative to the Pascal version
    x [9]int
)
 
func try(i int) {
    for j := 1; ; j++ {
        q = false
        if a[j] && b[i+j] && c[i-j+7] {
            x[i] = j
            a[j] = false
            b[i+j] = false
            c[i-j+7] = false
            if i < 8 {
                try(i + 1)
                if !q {
                    a[j] = true
                    b[i+j] = true
                    c[i-j+7] = true
                }
            } else {
                q = true
            }
        }
        if q || j == 8 {
            break
        }
    }
}
 
func main() {
    for i := 1; i <= 8; i++ {
        a[i] = true
    }
    for i := 2; i <= 16; i++ {
        b[i] = true
    }
    for i := 0; i <= 14; i++ {
        c[i] = true
    }
    try(1)
    if q {
        for i := 1; i <= 8; i++ {
            fmt.Println(i, x[i])
        }
    }
}
```


{{out}}

```txt

1 1
2 5
3 8
4 6
5 3
6 7
7 2
8 4

```


=== Refactored Niklaus Wirth algorithm (clearer/Go friendly solution) ===

```go
/*
 * N-Queens Problem
 *
 * For an NxN chess board, 'safely' place a chess queen in every column and row such that none can attack another.
 * This solution is based Wirth Pascal solution, although a tad cleaner, thus easier to understand as it uses Go/C
 * style indexing and naming, and also prints the Queen using a Unicode 'rune' (which other languages do not handle natively).
 *
 * N rows by N columns are number left to right top to bottom 0 - 7
 *
 * There are 2N-1 diagonals (showing an 8x8)
 *  the upper-right to lower-left are numbered row + col that is:
 *    0   1   2   3   4   5   6   7
 *    1   2   3   4   5   6   7   8
 *    2   3   4   5   6   7   8   9
 *    3   4   5   6   7   8   9  10
 *    4   5   6   7   8   9  10  11
 *    5   6   7   8   9  10  11  12
 *    6   7   8   9  10  11  12  13
 *    7   8   9  10  11  12  13  14
 * 
 *	the upper-left to lower-right are numbered N-1 + row - col
 *    7   6   5   4   3   2   1   0
 *    8   7   6   5   4   3   2   1
 *    9   8   7   6   5   4   3   2
 *   10   9   8   7   6   5   4   3
 *   11  10   9   8   7   6   5   4
 *   12  11  10   9   8   7   6   5
 *   13  12  11  10   9   8   7   6
 *   14  13  12  11  10   9   8   7
 */

package main

import "fmt"

const	N	= 8
const	HAS_QUEEN  = false
const	EMPTY  = true
const	UNASSIGNED = -1
const   white_queen = '\u2655'


var	row_num[N]int	// results, indexed by row will be the column where the queen lives (UNASSIGNED) is empty
var	right_2_left_diag[(2*N-1)]bool	// T if no queen in diag[idx]: row i, column col is diag i+col
var	left_2_right_diag[(2*N-1)]bool //  T is no queen in diag[idx], row i, column col is N-1 + i-col


func printresults() {
    for col := 0; col < N; col++ {
	if col != 0 {
	    fmt.Printf(" ");
	}
	fmt.Printf("%d,%d", col, row_num[col])
    }
    fmt.Printf("\n");
    for  row := 0; row < N; row++ {
	for col := 0; col < N; col++ {
	    if col == row_num[row] {
		fmt.Printf(" %c ", white_queen)
	    } else {
		fmt.Printf(" . ")
	    }
	}
	fmt.Printf("\n")
    }
}

/*
 * save a queen on the board by saving where we think it should go, and marking the diagonals as occupied
 */
 
func savequeen(row int, col int) {
    row_num[row] = col	// save queen column for this row
    right_2_left_diag[row+col] = HAS_QUEEN 	// mark forward diags as occupied
    left_2_right_diag[row-col+(N-1)] = HAS_QUEEN	// mark backward diags as occupied
}

/*
 * backout a previously saved queen by clearing where we put it, and marking the diagonals as empty
 */

func clearqueen(row int, col int) {
    row_num[row] = UNASSIGNED
    right_2_left_diag[row+col] = EMPTY 
    left_2_right_diag[row-col+(N-1)] = EMPTY
}

/*
 * for each column try the solutions
 */
func trycol(col int)  bool {
	// check each row to look for the first empty row that does not have a diagonal in use too
	for row := 0; row < N; row++ {
            if row_num[row] == UNASSIGNED &&	// has the row been used yet?
		    right_2_left_diag[row+col] == EMPTY &&	// check for the forward diags
		    left_2_right_diag[row-col+(N-1)] == EMPTY {	// check for the backwards diags
	        savequeen(row, col)	// this is a possible solution
	        // Tricky part here:  going forward thru the col up to but not including the rightmost one
                // if this fails, we are done, no need to search any more
                if col < N-1 && !trycol(col+1) {
		    // ok this did not work - we need to try a different row, so undo the guess
		    clearqueen(row, col)
	        } else {
		    // we have a solution on this row/col, start popping the stack.
		    return true
		} 
	    }
	}
	return false // not a solution for this col, pop the stack, undo the last guess, and try the next one
}

func main() {
    for i := 0; i < N ; i++ {
	row_num[i] = UNASSIGNED
    }
    for i := 0; i < 2*N-1 ; i++ {
	right_2_left_diag[i] = EMPTY
    }
    for i := 0; i < 2*N-1 ; i++ {
	left_2_right_diag[i] = EMPTY
    }
    trycol(0)
    printresults()
}
```

{{out}}

```txt

0,0 1,6 2,4 3,7 4,1 5,3 6,5 7,2
 ♕  .  .  .  .  .  .  .
 .  .  .  .  .  .  ♕  .
 .  .  .  .  ♕  .  .  .
 .  .  .  .  .  .  .  ♕
 .  ♕  .  .  .  .  .  .
 .  .  .  ♕  .  .  .  .
 .  .  .  .  .  ♕  .  .
 .  .  ♕  .  .  .  .  .

```



### Dancing Links / Algorithm X

Using Knuth's
[[WP:Dancing_Links|dancing links]] technique to implement his 
[[WP:Algorithm_X|Knuth's Algorithm X]].
The Go code for this technique is in the
[[N-queens_problem/dlx_go|dlx packge]].


```Go
package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"time"

	"rosettacode.org/dlx" // or where ever you put the dlx package
)

func main() {
	log.SetPrefix("N-queens: ")
	log.SetFlags(0)
	profile := flag.Bool("profile", false, "show DLX profile")
	flag.Parse()

	for N := 2; N <= 18; N++ {
		err := nqueens(N, N == 8, *profile)
		if err != nil {
			log.Fatal(err)
		}
	}
}

func nqueens(N int, printFirst, profile bool) error {
	// Build a new DLX matrix with 2N primary columns and 4N-6 secondary
	// columns: R0..R(N-1), F0..F(N-1), A1..A(2N-3), B1..B(2N-3).
	// We also know the number of cells and solution rows required.
	m := dlx.NewWithHint(2*N, 4*N-6, N*N*4-4, 8)

	s := solution{
		N:          N,
		renumFwd:   make([]int, 0, 2*N),
		renumBack:  make([]int, 2*N),
		printFirst: printFirst,
	}

	// column indexes
	iR0 := 0
	iF0 := iR0 + N
	iA1 := iF0 + N
	iB1 := iA1 + 2*N - 3

	// Use "organ-pipe" ordering. E.g. for N=8:
	// R4 F4 R3 F3 R5 F5 R2 F2 R6 F6 R1 F1 R7 F7 R0 F0
	// This can reduce the number of link updates required by
	// almost half for large N; see Knuth's paper for details.
	mid := N / 2
	for off := 0; off <= N-mid; off++ {
		i := mid - off
		if i >= 0 {
			s.renumBack[iR0+i] = len(s.renumFwd)
			s.renumBack[iF0+i] = len(s.renumFwd) + 1
			s.renumFwd = append(s.renumFwd, iR0+i, iF0+i)
		}
		if i = mid + off; off != 0 && i < N {
			s.renumBack[iR0+i] = len(s.renumFwd)
			s.renumBack[iF0+i] = len(s.renumFwd) + 1
			s.renumFwd = append(s.renumFwd, iR0+i, iF0+i)
		}
	}

	// Add constraint rows.
	// TODO: pre-eliminate symetrical possibilities.
	cols := make([]int, 4)
	for i := 0; i < N; i++ {
		for j := 0; j < N; j++ {
			cols[0] = iR0 + i    // Ri,         rank i
			cols[1] = iF0 + j    // Fj,         file j
			a := (i + j)         // A(i+j),     diagonals
			b := (N - 1 - i + j) // B(N-1-i+j), reverse diagonals
			cols = cols[:2]
			// Do organ-pipe reordering for R and F.
			for i, c := range cols {
				cols[i] = s.renumBack[c]
			}

			// Only add diagonals with more than one space; that
			// is we omit the corners: A0, A(2N-2), B0, and B(2N-2)
			if 0 < a && a < 2*N-2 {
				cols = append(cols, iA1+a-1)
			}
			if 0 < b && b < 2*N-2 {
				cols = append(cols, iB1+b-1)
			}

			m.AddRow(cols)
		}
	}

	// Search for solutions.
	start := time.Now()
	err := m.Search(s.found)
	if err != nil {
		return err
	}
	elapsed := time.Since(start)
	fmt.Printf("%d×%d queens has %2d solutions, found in %v\n", N, N, s.count, elapsed)
	if profile {
		m.ProfileWrite(os.Stderr)
	}
	return nil
}

type solution struct {
	N          int
	count      int
	renumFwd   []int // for "organ-pipe" column ordering
	renumBack  []int
	printFirst bool
}

func (s *solution) found(m *dlx.Matrix) error {
	s.count++
	if s.printFirst && s.count == 1 {
		fmt.Printf("First %d×%d queens solution:\n", s.N, s.N)
		for _, cols := range m.SolutionIDs(nil) {
			var r, f int
			for _, c := range cols {
				// Undo organ-pipe reodering
				if c < len(s.renumFwd) {
					c = s.renumFwd[c]
				}
				if c < s.N {
					r = c + 1
				} else if c < 2*s.N {
					f = c - s.N + 1
				}
			}
			fmt.Printf("    R%d F%d\n", r, f)
		}
	}
	return nil
}
```

{{out}}

```txt

2×2 queens has  0 solutions, found in 1.915µs
3×3 queens has  0 solutions, found in 1.22µs
4×4 queens has  2 solutions, found in 3.095µs
5×5 queens has 10 solutions, found in 7.15µs
6×6 queens has  4 solutions, found in 17.663µs
7×7 queens has 40 solutions, found in 54.08µs
First 8×8 queens solution:
    R5 F1
    R1 F4
    R3 F5
    R4 F3
    R6 F6
    R2 F7
    R7 F8
    R8 F2
8×8 queens has 92 solutions, found in 186.991µs
9×9 queens has 352 solutions, found in 580.225µs
10×10 queens has 724 solutions, found in 2.078235ms
11×11 queens has 2680 solutions, found in 8.186708ms
12×12 queens has 14200 solutions, found in 38.037841ms
13×13 queens has 73712 solutions, found in 183.846653ms
14×14 queens has 365596 solutions, found in 961.249859ms
15×15 queens has 2279184 solutions, found in 5.491853276s
16×16 queens has 14772512 solutions, found in 33.286561009s
17×17 queens has 95815104 solutions, found in 3m34.643824374s
18×18 queens has 666090624 solutions, found in 24m22.30241617s

```



## Groovy


### Distinct Solutions

This solver starts with the N! distinct solutions to the N-Rooks problem and then keeps only the candidates in which all Queens are mutually diagonal-safe.

```groovy
def listOrder = { a, b ->
    def k = [a.size(), b.size()].min()
    def i = (0..<k).find { a[it] != b[it] }
    (i != null) ? a[i] <=> b[i] : a.size() <=> b.size()
}

def orderedPermutations = { list ->
    def n = list.size()
    (0..<n).permutations().sort(listOrder)
}

def diagonalSafe = { list ->
    def n = list.size()
    n == 1 || (0..<(n-1)).every{ i ->
        ((i+1)..<n).every{ j ->
            !([list[i]+j-i, list[i]+i-j].contains(list[j]))
        }
    }
}

def queensDistinctSolutions = { n ->
    // each permutation is an N-Rooks solution
    orderedPermutations((0..<n)).findAll (diagonalSafe)
}
```



### Unique Solutions

Unique solutions are equivalence classes of distinct solutions, factoring out all reflections and rotations of a given solution. See the [[WP:Eight_queens_puzzle|Wikipedia page]]  for more details.

```groovy
class Reflect {
    public static final diag = { list ->
        final n = list.size()
        def tList = [0] * n
        (0..<n).each { tList[list[it]] = it }
        tList
    }
    
    public static final vert = { list ->
        list.reverse()
    }
    
    public static final horiz = { list ->
        final n = list.size()
        list.collect { n - it - 1 }
    }
}

enum Rotations {
    r0([]),
    r90([Reflect.vert, Reflect.diag]),
    r180([Reflect.vert, Reflect.diag, Reflect.vert, Reflect.diag]),
    r270([Reflect.diag, Reflect.vert]);
    
    private final List operations
    
    private Rotations(List ops) {
        operations = ops ?: []
    }
    
    public static void eliminateDups(primary, solutions) {
        (r0..r270).each { rot -> rot.eliminateDuplicates(primary, solutions) }
    }
    
    private void eliminateDuplicates(primary, solutions) {
        def rotated = [] + primary
        operations.each { rotated = it(rotated) }
        solutions.removeAll([rotated, Reflect.vert(rotated)])
    }
}

def queensUniqueSolutions = { start ->
    assert start instanceof Number || start instanceof List
    def qus = (start instanceof Number) \
                ? queensDistinctSolutions(start) \
                : [] + start
    for (def i = 0; i < qus.size()-1; i++) {
        Rotations.eliminateDups(qus[i], qus[(i+1)..<(qus.size())])
    }
    qus
}
```



### Test and Results

This script tests both distinct and unique solution lists.

```groovy
(1..9).each { n ->
    def qds = queensDistinctSolutions(n)
    def qus = queensUniqueSolutions(qds)
    println ([boardSize:n, "number of distinct solutions":qds.size(), "number of unique solutions":qus.size()])
    if(n < 9) { qus.each { println it } }
    else { println "first:${qus[0]}"; println "last:${qus[-1]}" }
    println()
}
```


Interpreting the Results:

Each individual result is given as a list of N numbers. Each number represents a column number within the list-indexed row. So, the following 4-queens solution:

```txt
[1, 3, 0, 2]
```

    
should be interpreted as follows:

```txt
row 0 has a queen in column 1
row 1 has a queen in column 3
row 2 has a queen in column 0
row 3 has a queen in column 2
```


In other words, this:

```txt
|///| Q |///|   |
 --- --- --- --- 
|   |///|   |/Q/|
 --- --- --- --- 
|/Q/|   |///|   |
 --- --- --- --- 
|   |///| Q |///|
```


Results:
<pre  style="height:60ex;overflow:scroll;">[boardSize:1, number of distinct solutions:1, number of unique solutions:1]
[0]

[boardSize:2, number of distinct solutions:0, number of unique solutions:0]

[boardSize:3, number of distinct solutions:0, number of unique solutions:0]

[boardSize:4, number of distinct solutions:2, number of unique solutions:1]
[1, 3, 0, 2]

[boardSize:5, number of distinct solutions:10, number of unique solutions:2]
[0, 2, 4, 1, 3]
[1, 4, 2, 0, 3]

[boardSize:6, number of distinct solutions:4, number of unique solutions:1]
[1, 3, 5, 0, 2, 4]

[boardSize:7, number of distinct solutions:40, number of unique solutions:6]
[0, 2, 4, 6, 1, 3, 5]
[0, 3, 6, 2, 5, 1, 4]
[1, 3, 0, 6, 4, 2, 5]
[1, 4, 0, 3, 6, 2, 5]
[1, 4, 6, 3, 0, 2, 5]
[1, 5, 2, 6, 3, 0, 4]

[boardSize:8, number of distinct solutions:92, number of unique solutions:12]
[0, 4, 7, 5, 2, 6, 1, 3]
[0, 5, 7, 2, 6, 3, 1, 4]
[1, 3, 5, 7, 2, 0, 6, 4]
[1, 4, 6, 0, 2, 7, 5, 3]
[1, 4, 6, 3, 0, 7, 5, 2]
[1, 5, 0, 6, 3, 7, 2, 4]
[1, 5, 7, 2, 0, 3, 6, 4]
[1, 6, 2, 5, 7, 4, 0, 3]
[1, 6, 4, 7, 0, 3, 5, 2]
[2, 4, 1, 7, 0, 6, 3, 5]
[2, 4, 7, 3, 0, 6, 1, 5]
[2, 5, 1, 4, 7, 0, 6, 3]

[boardSize:9, number of distinct solutions:352, number of unique solutions:46]
first:[0, 2, 5, 7, 1, 3, 8, 6, 4]
last:[3, 1, 6, 8, 0, 7, 4, 2, 5]
```



## Haskell


```haskell
import Control.Monad
import Data.List

-- given n, "queens n" solves the n-queens problem, returning a list of all the
-- safe arrangements. each solution is a list of the columns where the queens are
-- located for each row
queens :: Int -> [[Int]]
queens n = map fst $ foldM oneMoreQueen ([],[1..n]) [1..n]  where 

  -- foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
  -- foldM folds (from left to right) in the list monad, which is convenient for 
  -- "nondeterminstically" finding "all possible solutions" of something. the 
  -- initial value [] corresponds to the only safe arrangement of queens in 0 rows

  -- given a safe arrangement y of queens in the first i rows, and a list of 
  -- possible choices, "oneMoreQueen y _" returns a list of all the safe 
  -- arrangements of queens in the first (i+1) rows along with remaining choices 
  oneMoreQueen (y,d) _ = [(x:y, delete x d) | x <- d, safe x]  where

    -- "safe x" tests whether a queen at column x is safe from previous queens
    safe x = and [x /= c + n && x /= c - n | (n,c) <- zip [1..] y]

-- prints what the board looks like for a solution; with an extra newline
printSolution y = do
     let n = length y
     mapM_ (\x -> putStrLn [if z == x then 'Q' else '.' | z <- [1..n]]) y
     putStrLn ""

-- prints all the solutions for 6 queens
main = mapM_ printSolution $ queens 6
```


If you just want one solution, simply take the <code>head</code> of the result of <code>queens n</code>; since Haskell is lazy, it will only do as much work as needed to find one solution and stop.


### Alternative version


```haskell
import Control.Monad (foldM)
import Data.List ((\\))

main :: IO ()
main = mapM_ print $ queens 8

queens :: Int -> [[Int]]
queens n = foldM f [] [1..n]
    where
      f qs _ = [q:qs | q <- [1..n] \\ qs, q `notDiag` qs]
      q `notDiag` qs = and [abs (q - qi) /= i | (qi,i) <- qs `zip` [1..]]
```



### Using permutations

This version uses permutations to generate unique horizontal and vertical position for each queen. Thus, we only need to check diagonals. However, it is less efficient than the previous version because it does not prune out prefixes that are found to be unsuitable.

```haskell
import Data.List (nub, permutations)

-- checks if queens are on the same diagonal
-- with [0..] we place each queen on her own row
check f = length . nub . zipWith f [0..]

-- filters out results where 2 or more queens are on the same diagonal
-- with [0..n-1] we place each queeen on her own column
generate n = filter (\x -> check (+) x == n && check (-) x == n) $ permutations [0..n-1]

-- 8 is for "8 queens"
main = print $ generate 8
```



### In terms of foldr

A back-tracking variant using the Prelude's plain '''foldr''':
{{Trans|JavaScript}}

```haskell
import Data.List (transpose, intercalate)
import Data.Bool (bool)

queenPuzzle :: Int -> Int -> [[Int]]
queenPuzzle nRows nCols
  | nRows <= 0 = [[]]
  | otherwise =
    foldr (\qs a ->
               a ++ foldr (\iCol b -> bool  b  (b ++ [qs ++ [iCol]]) 
                                           (safe (nRows - 1) iCol qs))
                          []
                          [1 .. nCols])
          []
          (queenPuzzle (nRows - 1) nCols)

safe :: Int -> Int -> [Int] -> Bool
safe iRow iCol qs =
  (not . or) $ zipWith (\sc sr -> (iCol == sc) || 
                                  (sc + sr == (iCol + iRow)) || 
                                  (sc - sr == (iCol - iRow)))
                       qs
                       [0 .. iRow - 1]

-- TEST ---------------------------------------------------
-- 10 columns of solutions for the 7*7 board:
showSolutions :: Int -> Int -> [String]
showSolutions nCols nSize =
  map (unlines . map (intercalate "   ") . transpose . map boardLines)
      $ chunksOf nCols (queenPuzzle nSize nSize)
  where
    boardLines rows =
      [map (bool '.' '♛' . (== r)) [1 .. (length rows)] | r <- rows]

chunksOf :: Int -> [a] -> [[a]]
chunksOf i xs = splits xs
  where
    splits [] = []
    splits l  = (take i l) : splits (drop i l)

main :: IO ()
main = (putStrLn . unlines) $ showSolutions 10 7
```

{{Out}}

```txt
......♛   ......♛   ......♛   ......♛   .....♛.   .....♛.   .....♛.   .....♛.   .....♛.   .....♛.
.♛.....   ..♛....   ...♛...   ....♛..   ♛......   .♛.....   ..♛....   ..♛....   ..♛....   ...♛...
...♛...   .....♛.   ♛......   ..♛....   ..♛....   ....♛..   ......♛   ....♛..   ♛......   ......♛
.....♛.   .♛.....   ....♛..   ♛......   ....♛..   ♛......   ...♛...   ......♛   ...♛...   ♛......
♛......   ....♛..   .♛.....   .....♛.   ......♛   ...♛...   ♛......   ♛......   ......♛   ..♛....
..♛....   ♛......   .....♛.   ...♛...   .♛.....   ......♛   ....♛..   ...♛...   ....♛..   ....♛..
....♛..   ...♛...   ..♛....   .♛.....   ...♛...   ..♛....   .♛.....   .♛.....   .♛.....   .♛.....

.....♛.   ....♛..   ....♛..   ....♛..   ....♛..   ....♛..   ....♛..   ...♛...   ...♛...   ...♛...
...♛...   ♛......   ♛......   .♛.....   ..♛....   ......♛   ......♛   ♛......   ♛......   .♛.....
.♛.....   .....♛.   ...♛...   .....♛.   ♛......   .♛.....   .♛.....   ....♛..   ..♛....   ......♛
......♛   ...♛...   ......♛   ..♛....   .....♛.   ...♛...   .....♛.   .♛.....   .....♛.   ....♛..
....♛..   .♛.....   ..♛....   ......♛   ...♛...   .....♛.   ..♛....   .....♛.   .♛.....   ..♛....
..♛....   ......♛   .....♛.   ...♛...   .♛.....   ♛......   ♛......   ..♛....   ......♛   ♛......
♛......   ..♛....   .♛.....   ♛......   ......♛   ..♛....   ...♛...   ......♛   ....♛..   .....♛.

...♛...   ...♛...   ...♛...   ..♛....   ..♛....   ..♛....   ..♛....   ..♛....   ..♛....   .♛.....
.....♛.   ......♛   ......♛   ♛......   ♛......   ....♛..   .....♛.   ......♛   ......♛   ...♛...
♛......   ....♛..   ..♛....   .....♛.   .....♛.   ......♛   .♛.....   ...♛...   .♛.....   .....♛.
..♛....   .♛.....   .....♛.   .♛.....   ...♛...   .♛.....   ....♛..   ♛......   ...♛...   ♛......
....♛..   .....♛.   .♛.....   ....♛..   .♛.....   ...♛...   ♛......   ....♛..   .....♛.   ..♛....
......♛   ♛......   ....♛..   ......♛   ......♛   .....♛.   ...♛...   .♛.....   ♛......   ....♛..
.♛.....   ..♛....   ♛......   ...♛...   ....♛..   ♛......   ......♛   .....♛.   ....♛..   ......♛

.♛.....   .♛.....   .♛.....   .♛.....   .♛.....   .♛.....   ♛......   ♛......   ♛......   ♛......
...♛...   ....♛..   ....♛..   ....♛..   .....♛.   ......♛   ..♛....   ...♛...   ....♛..   .....♛.
♛......   ......♛   ..♛....   ♛......   ..♛....   ....♛..   ....♛..   ......♛   .♛.....   ...♛...
......♛   ...♛...   ♛......   ...♛...   ......♛   ..♛....   ......♛   ..♛....   .....♛.   .♛.....
....♛..   ♛......   ......♛   ......♛   ...♛...   ♛......   .♛.....   .....♛.   ..♛....   ......♛
..♛....   ..♛....   ...♛...   ..♛....   ♛......   .....♛.   ...♛...   .♛.....   ......♛   ....♛..
.....♛.   .....♛.   .....♛.   .....♛.   ....♛..   ...♛...   .....♛.   ....♛..   ...♛...   ..♛....
```


===Breadth-first search and Depth-first search===

```haskell
import Control.Monad
import System.Environment

-- | data types for the puzzle
type Row    = Int
type State  = [Row]
type Thread = [Row]

-- | utility functions
empty = null

-- | Check for infeasible states
infeasible :: Int -> (State, Thread) -> Bool
infeasible n ([], _)    = False
infeasible n ((r:rs),t) = length rs >= n || attack r rs || infeasible n (rs, t)

feasible n st = not $ infeasible n st

-- | Check if a row is attacking another row of a state
attack :: Row -> [Row] -> Bool
attack r rs = r `elem` rs
            || r `elem` (upperDiag rs)
            || r `elem` (lowerDiag rs)
  where 
    upperDiag xs = zipWith (-) xs [1..]
    lowerDiag xs = zipWith (+) xs [1..]

-- | Check if it is a goal state
isGoal :: Int -> (State, Thread) -> Bool
isGoal n (rs,t) = (feasible n (rs,t)) && (length rs == n)

-- | Perform a move
move :: Int -> (State, Thread) -> (State, Thread)
move x (s,t)  = (x:s, x:t)

choices n = [1..n]
moves n   = pure move <*> choices n

emptySt = ([],[])

-- | Breadth-first search
bfs :: Int -> [(State, Thread)] -> (State, Thread)
bfs n []                     = error "Could not find a feasible solution"
bfs n sts | (not.empty) goal = head goal
          | otherwise        = bfs n sts2
  where 
    goal = filter (isGoal n) sts2
    sts2 = filter (feasible n) $ (moves n) <*> sts

-- | Depth-first search
dfs :: Int -> (State, Thread) -> [(State, Thread)]
dfs n st | isGoal n st     = [st]
         | infeasible n st = [emptySt]
         | otherwise       = do x   <- [1..n]
                                st2 <- dfs n $ move x st
                                guard $ st2 /= emptySt
                                return st2

main = do
  [narg] <- getArgs
  let n = read narg :: Int
  print (bfs n [emptySt])
  print (head $ dfs n emptySt)
```


{{Out}}

```txt
([1,5,8,6,3,7,2,4],[1,5,8,6,3,7,2,4])
([4,2,7,3,6,8,5,1],[4,2,7,3,6,8,5,1])
```



## Heron


```heron
module NQueens {
    inherits {
        Heron.Windows.Console;
    }
    fields {
        n : Int = 4;
        sols : List = new List();
    }
    methods {
        PosToString(row : Int, col : Int) : String {
            return "row " + row.ToString() + ", col " + col.ToString();
        }
        AddQueen(b : Board, row : Int, col : Int)
        {
            if (!b.TryAddQueen(row, col))
                return;            
            if (row < n - 1)
                foreach (i in 0..n-1)
                   AddQueen(new Board(b), row + 1, i);
            else
                sols.Add(b);
        }        
        Main() {
            foreach (i in 0..n-1)
                AddQueen(new Board(), 0, i);
            foreach (b in sols) {
                b.Output();
                WriteLine("");
            }
            WriteLine("Found " + sols.Count().ToString() + " solutions");
        }
    }
}

class Board {
    fields {
        rows = new List();
    }
    methods {
        Constructor() {
            foreach (r in 0..n-1) {
                var col = new List();
                foreach (c in 0..n-1)
                    col.Add(false);
                rows.Add(col);
            }
        }
        Constructor(b : Board) {
            Constructor();
            foreach (r in 0..n-1)
                foreach (c in 0..n-1)
                    SetSpaceOccupied(r, c, b.SpaceOccupied(r, c));
        }
        SpaceOccupied(row : Int, col : Int) : Bool {
            return rows[row][col];
        }
        SetSpaceOccupied(row : Int, col : Int, b : Bool)  {
            rows[row][col] = b;
        }
        ValidPos(row : Int, col : Int) : Bool {
            return ((row >= 0) && (row < n)) && ((col >= 0) && (col < n)); 
        }
        VectorOccupied(row : Int, col : Int, rowDir : Int, colDir : Int) : Bool {
            var nextRow = row + rowDir;
            var nextCol = col + colDir;
            if (!ValidPos(nextRow, nextCol)) 
                return false;
            if (SpaceOccupied(nextRow, nextCol)) 
                return true;
            return VectorOccupied(nextRow, nextCol, rowDir, colDir);
        }
        TryAddQueen(row : Int, col : Int) : Bool {
            foreach (rowDir in -1..1)
                foreach (colDir in -1..1)
                    if (rowDir != 0 || colDir != 0)
                        if (VectorOccupied(row, col, rowDir, colDir))
                            return false;
            SetSpaceOccupied(row, col, true);
            return true;
        }
        Output() {
            foreach (row in 0..n-1) {
                foreach (col in 0..n-1) {
                    if (SpaceOccupied(row, col)) {
                        Write("Q");
                    }
                    else {
                        Write(".");
                    }
                }
                WriteLine("");
            }
        }
    }
}
```


=={{header|Icon}} and {{header|Unicon}}==
Here's a solution to the <tt>n = 8</tt> case:

```icon
procedure main()
    write(q(1), " ", q(2), " ", q(3), " ", q(4), " ", q(5), " ", q(6), " ", q(7), " ", q(8))
end

procedure q(c)
    static udiag, ddiag, row

    initial {
        udiag := list(15, 0)
        ddiag := list(15, 0)
        row := list(8, 0)
    }

    every 0 = row[r := 1 to 8] = ddiag[r + c - 1] = udiag[8 + r - c] do   # test if free
        suspend row[r] <- ddiag[r + c - 1] <- udiag[8 + r - c] <- r       # place and yield
end
```


Notes:
* Solution assumes attempting to place 8 queens on a standard chessboard, and is a simplification of a program in the [[:Category:Icon_Programming_Library|The Icon Programming Library (IPL)]] which is in the public domain.
* There are 15 left-side-down-diagonals and 15 left-side-up-diagonals represented in the lists. An unfilled row or diagonal has value 0, otherwise the row number is stored to indicate placement.
* The numeric equality operator '''=''', like all the comparators in Icon, yields the right argument as its solution, or fails. The chain of 0 = A = B = C therefore tests each of A B and C for equality with 0; these semantics read very naturally.
* '''every''' drives the chain of '''=''' tests to yield every possible result; the iterable component is the generator '''1 to 8''' which is progressively stored into '''r''' and will be backtracked if any of the equality tests fail. If all the placements are zero, the chain of equalities suceeds, and the suspend is invoked for that iteration.
* '''&lt;-''' is the "reversible assignment" operator. It restores the original value and fails if it is resumed by backtracking. The suspend will use it to temporarily consume the placements and then it will yield the value of the chosen row '''r'''.
* procedure q() attempts to place the c-th column queen into row 1 to 8 in turn, suspending only if that queen can be placed at [c,r]
* As the calls to q() are evaluated in main, each one will suspend a possible row, thereby allowing the next q(n) in main to be evaluated. If any of the q() fails to yield a row for the nth queen (or runs out of solutions) the previous, suspended calls to q() are backtracked progressively. If the final q(8) yields a row, the write() will be called with the row positions of each queen. Note that even the final q(8) will be suspended along with the other 7 calls to q(). Unless the write() is driven to produce more solutions (see next point) the suspended procedures will be closed at the "end of statement" ie after the write has "succeeded".
* If you want to derive all possible solutions, main() can be embellished with the '''every''' keyword:

```icon

procedure main()
    every write(q(1), " ", q(2), " ", q(3), " ", q(4), " ", q(5), " ", q(6), " ", q(7), " ", q(8))
end

```

This drives the backtracking to find more solutions.

The following is a general <tt>N</tt>-queens solution, adapted from
a solution placed into the public domain by Peter A. Bigot in 1990.
The program produces a solution for a specified value of <tt>N</tt>.
The comment explains how to modify the program to produce <i>all</i>
solutions for a given <tt>N</tt>.

```icon
global n, rw, dd, ud

procedure main(args)
    n := integer(args[1]) | 8
    rw := list(n)
    dd := list(2*n-1)
    ud := list(2*n-1)
    solvequeen(1)
end

procedure solvequeen(c)
    if (c > n) then return show()
    else suspend placequeen(c) & solvequeen(c+1)
end

procedure placequeen(c)
    suspend (/rw[r := 1 to n] <- /dd[r+c-1] <- /ud[n+r-c] <- c)
end

procedure show()
    static count, line, border
    initial {
        count := 0
        line := repl("|   ",n) || "|"
        border := repl("----",n) || "-"
        }
    write("solution: ", count+:=1)
    write("  ", border)
    every line[4*(!rw - 1) + 3] <- "Q" do {
        write("  ", line)
        write("  ", border)
        }
    write()
    return      # Comment out to see all possible solutions
end
```


A sample run for <tt>N = 6</tt>:

```txt
->nq 6
solution: 1
  -------------------------
  |   |   |   | Q |   |   |
  -------------------------
  | Q |   |   |   |   |   |
  -------------------------
  |   |   |   |   | Q |   |
  -------------------------
  |   | Q |   |   |   |   |
  -------------------------
  |   |   |   |   |   | Q |
  -------------------------
  |   |   | Q |   |   |   |
  -------------------------

->
```

{{libheader|Icon Programming Library}}  
Two solutions are in the IPL [http://www.cs.arizona.edu/icon/library/progs/queens.htm queens] and [http://www.cs.arizona.edu/icon/library/progs/genqueen.htm genqueen].

=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "NQueens.bas"
110 TEXT 80
120 DO
130   INPUT PROMPT "Size of board (2-12): ":N$
140   LET N=VAL(N$)
150 LOOP UNTIL N>1 AND N<13
160 NUMERIC A(1 TO N),X(1 TO N),B(2 TO 2*N),C(-N+1 TO N-1)
170 LET SOL=0
180 CALL INIT(A):CALL INIT(B):CALL INIT(C)
190 CALL TRY(1)
200 PRINT SOL;"solutions."
210 END
220 DEF WRITE
230   LET S$="":LET SOL=SOL+1
240   FOR K=1 TO N
250     LET S$=S$&CHR$(64+K)&STR$(X(K))&"  "
260   NEXT
270   PRINT S$
280 END DEF
290 DEF TRY(I)
300   NUMERIC J
310   FOR J=1 TO N
320     IF A(J) AND B(I+J) AND C(I-J) THEN
330       LET X(I)=J:LET A(J),B(I+J),C(I-J)=0
340       IF I<N THEN
350         CALL TRY(I+1)
360       ELSE
370         CALL WRITE
380       END IF
390       LET A(J),B(I+J),C(I-J)=1
400     END IF
410   NEXT
420 END DEF
430 DEF INIT(REF T)
440   FOR I=LBOUND(T) TO UBOUND(T)
450     LET T(I)=1
460   NEXT
470 END DEF
```



## J


This is one of several J solutions shown and explained on this [[J:Essays/N%20Queens%20Problem|J wiki page]]


```j
perm   =: ! A.&i. ]               NB. all permutations of integers 0 to y
comb2  =: (, #: I.@,@(</)&i.)~    NB. all size 2 combinations of integers 0 to y
mask   =: [ */@:~:&(|@-/) {
queenst=: comb2 (] #"1~ mask)&.|: perm
```


Note that the Roger Hui's approach (used here) matches the description attributed to Raymond Hettinger (in the Python implementation).  (Both were posted years ago: 1981 for Hui's version which was  used here, and 2009 for Hettinger's.) However they do use different diagonal queen clash elimination approaches -see [http://rosettacode.org/wiki/N-queens_problem#Roger_Hui_.281981.29_Algorithm C# Roger Hui Algorithm] for a comparison of the two approaches.

Example use:


```j
   $queenst 8
92 8
```


92 distinct solutions for an 8 by 8 board.


```j
   {.queenst 8
0 4 7 5 2 6 1 3
```


One of the solutions. Position indicates row number, the integer indicates column number (0..7) for each queen -- though of course you could just as validly think of that the other way around.


## Java


```java
public class NQueens {

  private static int[] b = new int[8];
  private static int s = 0;

  static boolean unsafe(int y) {
    int x = b[y];
    for (int i = 1; i <= y; i++) {
      int t = b[y - i];
      if (t == x ||
          t == x - i ||
          t == x + i) {
        return true;
      }
    }

    return false;
  }

  public static void putboard() {
    System.out.println("\n\nSolution " + (++s));
    for (int y = 0; y < 8; y++) {
      for (int x = 0; x < 8; x++) {
        System.out.print((b[y] == x) ? "|Q" : "|_");
      }
      System.out.println("|");
    }
  }

  public static void main(String[] args) {
    int y = 0;
    b[0] = -1;
    while (y >= 0) {
      do {
        b[y]++;
      } while ((b[y] < 8) && unsafe(y));
      if (b[y] < 8) {
        if (y < 7) {
          b[++y] = -1;
        } else {
          putboard();
        }
      } else {
        y--;
      }
    }
  }
}
```



## Javascript


### ES5

Algorithm uses recursive Backtracking. Checks for correct position on subfields, whichs saves a lot position checks. Needs 15.720 position checks for a 8x8 field.

```javascript
function queenPuzzle(rows, columns) {
    if (rows <= 0) {
        return [[]];
    } else {
        return addQueen(rows - 1, columns);
    }
}

function addQueen(newRow, columns, prevSolution) {
    var newSolutions = [];
    var prev = queenPuzzle(newRow, columns);
    for (var i = 0; i < prev.length; i++) {
        var solution = prev[i];
        for (var newColumn = 0; newColumn < columns; newColumn++) {
            if (!hasConflict(newRow, newColumn, solution))
                newSolutions.push(solution.concat([newColumn]))
        }
    }
    return newSolutions;
}

function hasConflict(newRow, newColumn, solution) {
    for (var i = 0; i < newRow; i++) {
        if (solution[i]     == newColumn          ||
            solution[i] + i == newColumn + newRow || 
            solution[i] - i == newColumn - newRow) {
                return true;
        }
    }
    return false;
}

console.log(queenPuzzle(8,8));
```



### ES6

Translating the ES5 version, and adding a function to display columns of solutions.

```JavaScript
(() => {
    'use strict';

    // N QUEENS PROBLEM -----------------------------------

    // queenPuzzle :: Int -> Int -> [[Int]]
    const queenPuzzle = intRows => intCols => {
        const go = nRows =>
            nRows <= 0 ? [
                []
            ] : go(nRows - 1).reduce(
                (a, solution) => append(a)(
                    enumFromTo(0)(intCols - 1)
                    .reduce((b, iCol) =>
                        safe(nRows - 1, iCol, solution) ? (
                            b.concat([solution.concat(iCol)])
                        ) : b, [])
                ), []
            );
        return go(intRows);
    };

    // safe : Int -> Int -> [Int] -> Bool
    const safe = (iRow, iCol, solution) =>
        !any(
            ([sc, sr]) => (iCol === sc) || (
                sc + sr === iCol + iRow
            ) || (sc - sr === iCol - iRow)
        )(zip(solution)(enumFromTo(0)(iRow - 1)));


    // TEST -----------------------------------------------
    // Ten columns of solutions to the 7*7 board

    // main :: IO ()
    const main = () =>
        console.log(
            showSolutions(10, 7)
        );

    // DISPLAY ---------------------------------------------

    // showSolutions :: Int -> Int -> String
    const showSolutions = (nCols, nBoardSize) =>
        intercalate('\n\n')(
            map(unlines)(
                map(compose(
                    map(intercalate('  ')),
                    transpose,
                    map(rows =>
                        map(r => concat(
                            concatMap(
                                compose(bool('.')('♛'), eq(r))
                            )(enumFromTo(1)(rows.length))
                        ))(rows)
                    )
                ))(chunksOf(nCols)(
                    join(queenPuzzle)(nBoardSize)
                ))
            )
        );

    // GENERIC FUNCTIONS ----------------------------------

    // abs :: Num a => a -> a
    const abs = Math.abs

    // bool :: a -> a -> Bool -> a
    const bool = f => t => p =>
        p ? t : f;

    // eq (==) :: Eq a => a -> a -> Bool
    const eq = a => b => a === b;

    // any :: (a -> Bool) -> [a] -> Bool
    const any = p => xs => xs.some(p);

    // (++) :: [a] -> [a] -> [a]
    const append = xs => ys => xs.concat(ys);

    // bindFn (>>=) :: (a -> b) -> (b -> a -> c) -> a -> c
    const bindFn = f => bop =>
        // Binary operator applied over f x and x.
        x => bop(f(x))(x);

    // chunksOf :: Int -> [a] -> [[a]]
    const chunksOf = n => xs =>
        xs.reduce((a, _, i, xs) =>
            i % n ? a : a.concat([xs.slice(i, i + n)]), []);

    // compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
    const compose = (...fs) =>
        x => fs.reduceRight((a, f) => f(a), x);

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = f => xs =>
        xs.flatMap(f);

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = m => n =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // identity :: a -> a
    const identity = x => x;

    // intercalate :: String -> [a] -> String
    const intercalate = s => xs => xs.join(s);

    // join :: m (m a) -> m a
    // Function instance
    const join = f => bindFn(f)(
        identity
    );

    // map :: (a -> b) -> [a] -> [b]
    const map = f => xs => xs.map(f)

    // transpose :: [[a]] -> [[a]]
    const transpose = xs =>
        xs[0].map((_, iCol) => xs.map(row => row[iCol]));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // zip :: [a] -> [b] -> [(a,b)]
    const zip = xs => ys =>
        xs.slice(0, Math.min(xs.length, ys.length))
        .map((x, i) => [x, ys[i]]);

    // MAIN ---
    return main();
})();
```



## jq


### =Single Solution=

{{works with|jq|1.4}}
This section presents a function for finding a single solution using
the formulae for explicit solutions at [[WP:Eight_queens_puzzle|Eight Queens Puzzle]].

```jq
def single_solution_queens(n):
  def q: "♛";
  def init(k): reduce range(0;k) as $i ([]; . + ["."]);
  def matrix(k): init(k) as $row | reduce range(0;k) as $i ([]; . + [$row]);
  def place(stream; i; j):
    # jq indexing is based on offsets but we are using the 1-based formulae:
    reduce stream as $s (.; setpath([-1+($s|i), -1+($s|j)]; q) );
  def even(k): 
    if ((k-2) % 6) != 0 then
         place( range(1; 1+(k/2));         .; 2*. )
       | place( range(1; 1+(k/2)); (k/2) + .; 2*. -1 )
    else place( range(1; 1+(k/2));         .; 1 + ((2*. + (k/2) - 3) % k))  
       | place( range(1; 1+(n/2)); n + 1 - .; n - ((2*. + (n/2) - 3) % n))  
    end;

  matrix(n)                          # the chess board
  | if (n % 2) == 0 then even(n)
    else even(n-1) | .[n-1][n-1] = q
    end;

# Example:
def pp: reduce .[] as $row
  ("";  reduce $row[] as $x (.; . + $x) + "\n");

single_solution_queens(8) | pp
```

{{out}}
$ jq -M -n -r -f n-queens-single-solution.jq

```sh
...♛....
.....♛..
.......♛
.♛......
......♛.
♛.......
..♛.....
....♛...
```

====Generate-and-test counter====
{{ works with|jq|1.4}}
'''Part 1: Generic functions'''

```jq
# permutations of 0 .. (n-1)
def permutations(n):
  # Given a single array, generate a stream by inserting n at different positions:
  def insert(m;n):
     if m >= 0 then (.[0:m] + [n] + .[m:]), insert(m-1;n) else empty end;

  if n==0 then []
  elif n == 1 then [1]
  else
    permutations(n-1) | insert(n-1; n)
  end;

def count(g): reduce g as $i (0; .+1);
```

'''Part 2: n-queens'''

```jq
def queens(n):
  def sums:
  . as $board
  | [ range(0;length) | . + $board[.]]
  | unique | length;

  def differences:
  . as $board
  | [ range(0;length) | . - $board[.]]
  | unique | length;

  def allowable:
    length as $n
    | sums == $n and differences == $n;

  count( permutations(n) | select(allowable) );

```

'''Example''':

```jq
queens(8)
```

{{out}}
 92


## Julia



```ruby

#!/usr/bin/env julia

__precompile__(true)

"""
# EightQueensPuzzle

Ported to **Julia** from examples in several languages from
here: https://hbfs.wordpress.com/2009/11/10/is-python-slow
"""
module EightQueensPuzzle

export main

type Board
    cols::Int
    nodes::Int
    diag45::Int
    diag135::Int
    solutions::Int

    Board() = new(0, 0, 0, 0, 0)
end

"Marks occupancy."
function mark!(b::Board, k::Int, j::Int)
    b.cols    $= (1 << j)
    b.diag135 $= (1 << (j+k))
    b.diag45  $= (1 << (32+j-k))
end

"Tests if a square is menaced."
function test(b::Board, k::Int, j::Int)
    b.cols    & (1 << j)        +
    b.diag135 & (1 << (j+k))    +
    b.diag45  & (1 << (32+j-k)) == 0
end

"Backtracking solver."
function solve!(b::Board, niv::Int, dx::Int)
    if niv > 0
        for i in 0:dx-1
            if test(b, niv, i) == true
                mark!(b, niv, i)
                solve!(b, niv-1, dx)
                mark!(b, niv, i)
            end
        end
    else
        for i in 0:dx-1
            if test(b, 0, i) == true
                b.solutions += 1
            end
        end
    end
    b.nodes += 1
    b.solutions
end

"C/C++-style `main` function."
function main()
    for n = 1:17
        gc()
        b = Board()
        @show n
        print("elapsed:")
        solutions = @time solve!(b, n-1, n)
        @show solutions
        println()
    end
end

end

using  EightQueensPuzzle

main()

```



```ruby

juser@juliabox:~$ /opt/julia-0.5/bin/julia eight_queen_puzzle.jl
n = 1
elapsed:  0.000001 seconds
solutions = 1

n = 2
elapsed:  0.000001 seconds
solutions = 0

n = 3
elapsed:  0.000001 seconds
solutions = 0

n = 4
elapsed:  0.000001 seconds
solutions = 2

n = 5
elapsed:  0.000003 seconds
solutions = 10

n = 6
elapsed:  0.000008 seconds
solutions = 4

n = 7
elapsed:  0.000028 seconds
solutions = 40

n = 8
elapsed:  0.000108 seconds
solutions = 92

n = 9
elapsed:  0.000463 seconds
solutions = 352

n = 10
elapsed:  0.002146 seconds
solutions = 724

n = 11
elapsed:  0.010646 seconds
solutions = 2680

n = 12
elapsed:  0.057603 seconds
solutions = 14200

n = 13
elapsed:  0.334600 seconds
solutions = 73712

n = 14
elapsed:  2.055078 seconds
solutions = 365596

n = 15
elapsed: 13.480449 seconds
solutions = 2279184

n = 16
elapsed: 97.192552 seconds
solutions = 14772512

n = 17
elapsed:720.314676 seconds
solutions = 95815104

```



## Kotlin

{{trans|FreeBASIC}}

```scala
// version 1.1.3

var count = 0
var c = IntArray(0)
var f = "" 

fun nQueens(row: Int, n: Int) {
    outer@ for (x in 1..n) {
        for (y in 1..row - 1) {
            if (c[y] == x) continue@outer
            if (row - y == Math.abs(x - c[y])) continue@outer           
        }
        c[row] = x
        if (row < n) nQueens(row + 1, n)
        else if (++count == 1) f = c.drop(1).map { it - 1 }.toString()
    }
}

fun main(args: Array<String>) {
   for (n in 1..14) { 
       count = 0
       c = IntArray(n + 1)
       f = ""
       nQueens(1, n)
       println("For a $n x $n board:")
       println("  Solutions = $count")
       if (count > 0) println("  First is $f")
       println()
   }
}
```


{{out}}

```txt

For a 1 x 1 board:
  Solutions = 1
  First is [0]

For a 2 x 2 board:
  Solutions = 0

For a 3 x 3 board:
  Solutions = 0

For a 4 x 4 board:
  Solutions = 2
  First is [1, 3, 0, 2]

For a 5 x 5 board:
  Solutions = 10
  First is [0, 2, 4, 1, 3]

For a 6 x 6 board:
  Solutions = 4
  First is [1, 3, 5, 0, 2, 4]

For a 7 x 7 board:
  Solutions = 40
  First is [0, 2, 4, 6, 1, 3, 5]

For a 8 x 8 board:
  Solutions = 92
  First is [0, 4, 7, 5, 2, 6, 1, 3]

For a 9 x 9 board:
  Solutions = 352
  First is [0, 2, 5, 7, 1, 3, 8, 6, 4]

For a 10 x 10 board:
  Solutions = 724
  First is [0, 2, 5, 7, 9, 4, 8, 1, 3, 6]

For a 11 x 11 board:
  Solutions = 2680
  First is [0, 2, 4, 6, 8, 10, 1, 3, 5, 7, 9]

For a 12 x 12 board:
  Solutions = 14200
  First is [0, 2, 4, 7, 9, 11, 5, 10, 1, 6, 8, 3]

For a 13 x 13 board:
  Solutions = 73712
  First is [0, 2, 4, 1, 8, 11, 9, 12, 3, 5, 7, 10, 6]

For a 14 x 14 board:
  Solutions = 365596
  First is [0, 2, 4, 6, 11, 9, 12, 3, 13, 8, 1, 5, 7, 10]

```



## Liberty BASIC

Program uses permutation generator (stores all permutations) and solves tasks 4x4 to 9x9. It prints all the solutions.

```lb

'N queens
'>10 would not work due to way permutations used
'anyway, 10 doesn't fit in memory
Input "Input N for N queens puzzle (4..9) ";N
if N<4 or N>9 then print "N out of range - quitting": end

ABC$= " "
dash$ = ""
for i = 0 to N-1
    ABC$=ABC$+" "+chr$(asc("a")+i)
    dash$ = dash$+"--"
next

dim q(N)
t0=time$("ms")

fact = 1
for i = 1 to N
    fact = fact*i
next

dim anagram$(fact)
global nPerms
print "Filling permutations array"
t0=time$("ms")
    res$=permutation$("", left$("0123456789", N))
t1=time$("ms")
print "Created all possible permutations ";t1-t0

t0=time$("ms")
'actually fact = nPerms
for k=1 to nPerms
    for i=0 to N-1
        q(i)=val(mid$(anagram$(k),i+1,1))
        'print q(i);
    next
    'print

    fail = 0
    for i=0 to N-1
        for j=i+1 to N-1
            'check rows are different
            if q(i)=q(j) then fail = 1: exit for
            'check diagonals are different
            if i+q(i)=j+q(j) then fail = 1: exit for
            'check other diagonals are different
            if i-q(i)=j-q(j) then fail = 1: exit for
        next
        if fail then exit for
    next

    if not(fail) then
        num=num+1
        print " ";dash$
            for i=0 to N-1
                print N-i; space$(2*q(i));" *"
            next
        print " ";dash$
        print ABC$
    end if

next

t1=time$("ms")
print "Time taken ";t1-t0
print "Number of solutions ";num

'----------------------------------
'from
'http://babek.info/libertybasicfiles/lbnews/nl124/wordgames.htm
'Programming a Word Game by Janet Terra,
'The Liberty Basic Newsletter - Issue #124 - September 2004
Function permutation$(pre$, post$)
'Note the variable nPerms must first be stated as a global variable.
    lgth = Len(post$)
    If lgth < 2 Then
        nPerms = nPerms + 1
        anagram$(nPerms) = pre$;post$
    Else
        For i = 1 To lgth
            tmp$=permutation$(pre$+Mid$(post$,i,1),Left$(post$,i-1)+Right$(post$,lgth-i))
        Next i
    End If
End Function


```



## Locomotive Basic


Uses the heuristic from the Wikipedia article to get one solution.


```locobasic
10 mode 1:defint a-z
20 while n<4:input "How many queens (N>=4)";n:wend
30 dim q(n),e(n),o(n)
40 r=n mod 6
50 if r<>2 and r<>3 then gosub 320:goto 220
60 for i=1 to int(n/2)
70 e(i)=2*i
80 next
90 for i=1 to round(n/2)
100 o(i)=2*i-1
110 next
120 if r=2 then gosub 410
130 if r=3 then gosub 460
140 s=1
150 for i=1 to n
160 if e(i)>0 then q(s)=e(i):s=s+1
170 next
180 for i=1 to n
190 if o(i)>0 then q(s)=o(i):s=s+1
200 next
210 ' print board
220 cls
230 for i=1 to n
240 locate i,26-q(i):print chr$(238);
250 locate i,24-n   :print chr$(96+i);
260 locate n+1,26-i :print i;
270 next
280 locate 1,1
290 call &bb06
300 end
310 ' the simple case
320 p=1
330 for i=1 to n
340 if i mod 2=0 then q(p)=i:p=p+1
350 next
360 for i=1 to n
370 if i mod 2 then q(p)=i:p=p+1
380 next
390 return
400 ' edit list when remainder is 2
410 for i=1 to n
420 if o(i)=3 then o(i)=1 else if o(i)=1 then o(i)=3
430 if o(i)=5 then o(i)=-1 else if o(i)=0 then o(i)=5:return
440 next
450 ' edit list when remainder is 3
460 for i=1 to n
470 if e(i)=2 then e(i)=-1 else if e(i)=0 then e(i)=2:goto 500
480 next
490 ' edit list some more
500 for i=1 to n
510 if o(i)=1 or o(i)=3 then o(i)=-1 else if o(i)=0 then o(i)=1:o(i+1)=3:return
520 next
```


[[File:Queens Puzzle, Locomotive Basic.png]]
[[File:20 Queens, Locomotive Basic.png]]


## Logo


```logo
to try :files :diag1 :diag2 :tried
  if :files = 0 [make "solutions :solutions+1  show :tried  stop]
  localmake "safe (bitand :files :diag1 :diag2)
  until [:safe = 0] [
    localmake "f bitnot bitand :safe minus :safe
    try bitand :files :f  ashift bitand :diag1 :f -1  (ashift bitand :diag2 :f 1)+1  fput bitnot :f :tried
    localmake "safe bitand :safe :safe-1
  ]
end

to queens :n
  make "solutions 0
  try (lshift 1 :n)-1 -1 -1 []
  output :solutions
end

print queens 8  ; 92
```



## Lua


```Lua
N = 8

-- We'll use nil to indicate no queen is present.
grid = {}
for i = 0, N do
  grid[i] = {}
end

function can_find_solution(x0, y0)
  local x0, y0 = x0 or 0, y0 or 1  -- Set default vals (0, 1).
  for x = 1, x0 - 1 do
    if grid[x][y0] or grid[x][y0 - x0 + x] or grid[x][y0 + x0 - x] then
      return false
    end
  end
  grid[x0][y0] = true
  if x0 == N then return true end
  for y0 = 1, N do
    if can_find_solution(x0 + 1, y0) then return true end
  end
  grid[x0][y0] = nil
  return false
end

if can_find_solution() then
  for y = 1, N do
    for x = 1, N do
      -- Print "|Q" if grid[x][y] is true; "|_" otherwise.
      io.write(grid[x][y] and "|Q" or "|_")
    end
    print("|")
  end
else
  print(string.format("No solution for %d queens.\n", N))
end
```



## M2000 Interpreter

{{trans|VBA}}

```M2000 Interpreter

Module N_queens {
          Const l = 15  'number of queens
          Const b = False  'print option
          Dim a(0 to  l), s(0 to l), u(0 to 4 * l - 2)
          Def long n, m, i, j, p, q, r, k, t
          For i = 1 To l: a(i) = i: Next i
          For n = 1 To l
              m = 0
              i = 1
              j = 0
              r = 2 * n - 1
              Do {
                  i--
                  j++
                  p = 0
                  q = -r
                  Do {
                      i++
                      u(p) = 1
                      u(q + r) = 1
                      Swap a(i), a(j)
                      p = i - a(i) + n
                      q = i + a(i) - 1
                      s(i) = j
                      j = i + 1
                  } Until j > n Or u(p) Or u(q + r)
                  If u(p) = 0 Then {
                      If u(q + r) = 0 Then {
                          m++  'm: number of solutions
                          If b Then {
                              Print "n="; n; "m="; m
                              For k = 1 To n {
                                  For t = 1 To n {
                                      Print If$(a(n - k + 1) = t-> "Q", ".");
                                  }
                                  Print
                              }
                          }
                      }
                  }
                  j = s(i)
                  While j >= n And i <> 0 {
                            Do {
                                      Swap a(i), a(j)
                                      j--
                            }  Until j < i
                            i--
                            p = i - a(i) + n
                            q = i + a(i) - 1
                            j = s(i)
                            u(p) = 0
                            u(q + r) = 0
                  }
              } Until i = 0
              Print n, m  'number of queens, number of solutions
          Next n
}
N_queens

```



## Mathematica

This code recurses through the possibilities, using the "safe" method to check if the current set is allowed. The recursive method has the advantage that finding all possibilities is about as hard (code-wise, not computation-wise) as finding just one.

```Mathematica
safe[q_List, n_] := 
 With[{l = Length@q}, 
  Length@Union@q == Length@Union[q + Range@l] == 
   Length@Union[q - Range@l] == l]
nQueen[q_List: {}, n_] := 
 If[safe[q, n], 
  If[Length[q] == n, {q}, 
   Cases[nQueen[Append[q, #], n] & /@ Range[n], 
    Except[{Null} | {}], {2}]], Null]
```


This returns a list of valid permutations by giving the queen's column number for each row. It can be displayed in a list of chess-board tables like this:

```Mathematica
matrixView[n_] := 
 Grid[Normal@
     SparseArray[MapIndexed[{#, First@#2} -> "Q" &, #], {n, n}, "."], 
    Frame -> All] & /@ nQueen[n]
matrixView[6] // OutputForm
```

{{out}}

```txt
{.   .   .   Q   .   ., .   .   .   .   Q   ., .   Q   .   .   .   ., .   .   Q   .   .   .}

 Q   .   .   .   .   .  .   .   Q   .   .   .  .   .   .   Q   .   .  .   .   .   .   .   Q

 .   .   .   .   Q   .  Q   .   .   .   .   .  .   .   .   .   .   Q  .   Q   .   .   .   .

 .   Q   .   .   .   .  .   .   .   .   .   Q  Q   .   .   .   .   .  .   .   .   .   Q   .

 .   .   .   .   .   Q  .   .   .   Q   .   .  .   .   Q   .   .   .  Q   .   .   .   .   .

 .   .   Q   .   .   .  .   Q   .   .   .   .  .   .   .   .   Q   .  .   .   .   Q   .   .
```


Alternate Solution
This solution uses Permutations and subsets, also prints out a board representation.


```Mathematica
n=8;cnt=1;per=Permutations[Range[n],{n}];(* All Permutations of length n *)
	Do[per[[q]]=Partition[Riffle[Reverse[Range[n]],per[[q]]],2],{q,1,Length[per]}];(* Riffled in the reverse of [range n] partitioned into pairs*)
	 Do[w=Subsets[per[[t]],{2}];(* This is a full subset of the previous set of pairs taken 2 at a time *)
tot=0;
	Do[y=Abs[w[[q,1,1]]-w[[q,2,1]]];x=Abs[w[[q,1,2]]-w[[q,2,2]]];If[x==y,tot++],{q,1,Length[w]}];(* x and y are the abs values of x1-y1 and x2-y2 if equal they are on same diagonal *)
		If[tot==0,g=Grid[Table[" ",{n},{n}],Alignment->Center,Frame->All,Spacings->{1.2,1}];(* If no clashing diagonals setup an array and print the permutation and the grid*)
		Do[g[[1,per[[t,w,1]],per[[t,w,2]]]]="Q",{w,1,n}];
			Print[cnt,"   ",per[[t]],"   ",g];cnt++],{t,1,Length[per]}]
```


Alternative Solution using Linear Programming:


```Mathematica

dispSol[sol_] := sol /. {1 -> "Q" , 0 -> "-"} // Grid

solveNqueens[n_] := 
 Module[{c, m, b, vars}, c = cqueens[n]; m = mqueens[n]; 
  vars = mqueens2[n]; b = bqueens[Length[m]]; 
  Partition[LinearProgramming[c, m, b, vars, Integers], n]]

cqueens[n_] := Table[-1, {i, n^2}]

bqueens[l_] := Table[{1, -1}, {i, l}]

mqueens2[n_] := Table[{0, 1}, {i, n^2}]

mqueens[n_] := 
 Module[{t, t2, t3, t4}, t = mqueensh[n]; t2 = Append[t, mqueensv[n]];
   t3 = Append[t2, mqueensd[n]]; t4 = Append[t3, mqueensdm[n]]; 
  Partition[Flatten[t4], n^2]]

mqueensh[n_] := 
 Module[{t}, t = Table[0, {i, n}, {j, n^2}]; 
  For[i = 1, i <= n, i++, 
   For[j = 1, j <= n, j++, t[[i, ((i - 1)*n) + j]] = 1]]; t]

mqueensv[n_] := 
 Module[{t}, t = Table[0, {i, n}, {j, n^2}]; 
  For[i = 1, i <= n, i++, 
   For[j = 1, j <= n, j++, t[[j, ((i - 1)*n) + j]] = 1]]; t]

mqueensd[n_] := 
 Module[{t}, t = Table[0, {i, (2*n) - 1}, {j, n^2}]; 
  For[k = 2, k <= 2 n, k++, 
   For[i = 1, i <= n, i++, 
    For[j = 1, j <= n, j++, 
     If[i + j == k, t[[k - 1, ((i - 1)*n) + j]] = 1]]]]; t]

mqueensdm[n_] := 
 Module[{t}, t = Table[0, {i, Sum[1, {i, 1 - n, n - 1}]}, {j, n^2}]; 
  For[k = 1 - n, k <= n - 1, k++, 
   For[i = 1, i <= n, i++, 
    For[j = 1, j <= n, j++, 
     If[i == j - k, t[[k + n, ((i - 1)*n) + j]] = 1]]]]; t]


solveNqueens[8] // dispSol

```


```txt
-	-	-	-	Q	-	-	-
-	Q	-	-	-	-	-	-
-	-	-	-	-	Q	-	-
Q	-	-	-	-	-	-	-
-	-	-	-	-	-	Q	-
-	-	-	Q	-	-	-	-
-	-	-	-	-	-	-	Q
-	-	Q	-	-	-	-	-
```



## Maxima


```maxima
/* translation of Fortran 77, return solutions as permutations */

queens(n) := block([a, i, j, m, p, q, r, s, u, v, w, y, z],
a: makelist(i, i, 1, n), s: a*0, u: makelist(0, i, 1, 4*n - 2),
m: 0, i: 1, r: 2*n - 1, w: [ ], go(L40), L30, s[i]: j, u[p]: 1,
u[q + r]: 1, i: i + 1, L40, if i > n then go(L80), j: i, L50,
z: a[i], y: a[j], p: i - y + n, q: i + y - 1, a[i]: y, a[j]: z,
if u[p] = 0 and u[q + r] = 0 then go(L30), L60, j: j + 1,
if j <= n then go(L50), L70, j: j - 1, if j = i then go(L90),
z: a[i], a[i]: a[j], a[j]: z, go(L70), L80, m: m + 1,
w: endcons(copylist(a), w), L90, i: i - 1, if i = 0 then go(L100),
p: i - a[i] + n, q: i + a[i] - 1, j: s[i], u[p]: 0, u[q + r]: 0,
go(L60), L100, w)$

queens(8); /* [[1, 5, 8, 6, 3, 7, 2, 4],
               [1, 6, 8, 3, 7, 4, 2, 5],
               ...]] */
length(%); /* 92 */
```



## MUMPS


```MUMPS
Queens	New count,flip,row,sol
	Set sol=0
	For row(1)=1:1:4 Do try(2)  ; Not 8, the other 4 are symmetric...
	;
	; Remove symmetric solutions
	Set sol="" For  Set sol=$Order(sol(sol)) Quit:sol=""  Do
	. New xx,yy
	. Kill sol($Translate(sol,12345678,87654321)) ; Vertical flip
	. Kill sol($Reverse(sol)) ; Horizontal flip
	. Set flip="--------" for xx=1:1:8 Do  ; Flip over top left to bottom right diagonal
	. . New nx,ny
	. . Set yy=$Extract(sol,xx),nx=8+1-xx,ny=8+1-yy
	. . Set $Extract(flip,ny)=nx
	. . Quit
	. Kill sol(flip)
	. Set flip="--------" for xx=1:1:8 Do  ; Flip over top right to bottom left diagonal
	. . New nx,ny
	. . Set yy=$Extract(sol,xx),nx=xx,ny=yy
	. . Set $Extract(flip,ny)=nx
	. . Quit
	. Kill sol(flip)
	. Quit
	;
	; Display remaining solutions
	Set count=0,sol="" For  Set sol=$Order(sol(sol)) Quit:sol=""  Do  Quit:sol=""
	. New s1,s2,s3,txt,x,y
	. Set s1=sol,s2=$Order(sol(s1)),s3="" Set:s2'="" s3=$Order(sol(s2))
	. Set txt="+--+--+--+--+--+--+--+--+"
	. Write !,"  ",txt Write:s2'="" " ",txt Write:s3'="" " ",txt
	. For y=8:-1:1 Do
	. . Write !,y," |"
	. . For x=1:1:8 Write $Select($Extract(s1,x)=y:" Q",x+y#2:"  ",1:"##"),"|"
	. . If s2'="" Write " |"
	. . If s2'="" For x=1:1:8 Write $Select($Extract(s2,x)=y:" Q",x+y#2:"  ",1:"##"),"|"
	. . If s3'="" Write " |"
	. . If s3'="" For x=1:1:8 Write $Select($Extract(s3,x)=y:" Q",x+y#2:"  ",1:"##"),"|"
	. . Write !,"  ",txt Write:s2'="" " ",txt Write:s3'="" " ",txt
	. . Quit
	. Set txt="   A  B  C  D  E  F  G  H"
	. Write !,"  ",txt Write:s2'="" " ",txt Write:s3'="" " ",txt Write !
	. Set sol=s3
	. Quit
	Quit
try(col)	New ok,pcol
	If col>8 Do  Quit
	. New out,x
	. Set out="" For x=1:1:8 Set out=out_row(x)
	. Set sol(out)=1
	. Quit
	For row(col)=1:1:8 Do
	. Set ok=1
	. For pcol=1:1:col-1 If row(pcol)=row(col) Set ok=0 Quit
	. Quit:'ok
	. For pcol=1:1:col-1 If col-pcol=$Translate(row(pcol)-row(col),"-") Set ok=0 Quit
	. Quit:'ok
	. Do try(col+1)
	. Quit
	Quit
Do Queens

```

<div style="overflow:scroll; height:400px;">

```MUMPS

  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
8 |  |##| Q|##|  |##|  |##| |  |##| Q|##|  |##|  |##| |  |##|  | Q|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
7 |##|  |##|  |##| Q|##|  | |##|  |##|  | Q|  |##|  | |##|  |##|  |##|  | Q|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
6 |  |##|  | Q|  |##|  |##| |  | Q|  |##|  |##|  |##| |  |##| Q|##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
5 |##| Q|##|  |##|  |##|  | |##|  |##|  |##|  |##| Q| |##|  |##|  |##|  |##| Q|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
4 |  |##|  |##|  |##|  | Q| |  |##|  |##|  | Q|  |##| |  | Q|  |##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
3 |##|  |##|  | Q|  |##|  | |##|  |##| Q|##|  |##|  | |##|  |##|  | Q|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
2 |  |##|  |##|  |##| Q|##| |  |##|  |##|  |##| Q|##| | Q|##|  |##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
1 | Q|  |##|  |##|  |##|  | | Q|  |##|  |##|  |##|  | |##|  |##|  |##| Q|##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
   A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H
 
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
8 |  |##|  |##|  | Q|  |##| |  |##|  |##|  | Q|  |##| |  |##|  |##|  | Q|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
7 |##|  | Q|  |##|  |##|  | |##|  | Q|  |##|  |##|  | |##|  |##| Q|##|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
6 |  |##|  |##|  |##| Q|##| |  |##|  |##|  |##| Q|##| |  | Q|  |##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
5 |##| Q|##|  |##|  |##|  | |##| Q|##|  |##|  |##|  | |##|  |##|  |##|  |##| Q|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
4 |  |##|  |##|  |##|  | Q| |  |##|  | Q|  |##|  |##| |  |##|  |##| Q|##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
3 |##|  |##|  | Q|  |##|  | |##|  |##|  |##|  |##| Q| |##|  |##|  |##|  | Q|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
2 | Q|##|  |##|  |##|  |##| | Q|##|  |##|  |##|  |##| | Q|##|  |##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
1 |##|  |##| Q|##|  |##|  | |##|  |##|  | Q|  |##|  | |##|  | Q|  |##|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
   A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H
 
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
8 |  |##| Q|##|  |##|  |##| |  |##|  |##| Q|##|  |##| |  |##|  | Q|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
7 |##|  |##|  |##|  | Q|  | |##| Q|##|  |##|  |##|  | |##| Q|##|  |##|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
6 |  | Q|  |##|  |##|  |##| |  |##|  | Q|  |##|  |##| |  |##|  |##|  |##| Q|##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
5 |##|  |##|  |##|  |##| Q| |##|  |##|  |##| Q|##|  | |##|  | Q|  |##|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
4 |  |##|  |##|  | Q|  |##| |  |##|  |##|  |##|  | Q| |  |##|  |##|  | Q|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
3 |##|  |##| Q|##|  |##|  | |##|  | Q|  |##|  |##|  | |##|  |##|  |##|  |##| Q|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
2 | Q|##|  |##|  |##|  |##| | Q|##|  |##|  |##|  |##| | Q|##|  |##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
1 |##|  |##|  | Q|  |##|  | |##|  |##|  |##|  | Q|  | |##|  |##|  | Q|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
   A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H
 
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
8 |  | Q|  |##|  |##|  |##| |  |##|  | Q|  |##|  |##| |  |##|  | Q|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
7 |##|  |##|  |##|  | Q|  | |##|  |##|  |##| Q|##|  | |##|  |##|  |##|  | Q|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
6 |  |##| Q|##|  |##|  |##| |  |##|  |##|  |##|  | Q| |  |##|  |##| Q|##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
5 |##|  |##|  |##| Q|##|  | |##| Q|##|  |##|  |##|  | |##| Q|##|  |##|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
4 |  |##|  |##|  |##|  | Q| |  |##|  |##|  |##| Q|##| |  |##|  |##|  | Q|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
3 |##|  |##|  | Q|  |##|  | | Q|  |##|  |##|  |##|  | | Q|  |##|  |##|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
2 | Q|##|  |##|  |##|  |##| |  |##| Q|##|  |##|  |##| |  |##| Q|##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
1 |##|  |##| Q|##|  |##|  | |##|  |##|  | Q|  |##|  | |##|  |##|  |##|  |##| Q|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
   A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H
 
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
8 |  |##| Q|##|  |##|  |##| |  |##|  |##| Q|##|  |##| |  |##|  |##| Q|##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
7 |##|  |##|  |##| Q|##|  | |##|  |##|  |##|  | Q|  | |##|  |##|  |##|  | Q|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
6 |  |##|  |##|  |##|  | Q| |  | Q|  |##|  |##|  |##| |  | Q|  |##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
5 |##| Q|##|  |##|  |##|  | |##|  |##| Q|##|  |##|  | |##|  |##|  |##| Q|##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
4 |  |##|  | Q|  |##|  |##| |  |##|  |##|  |##|  | Q| |  |##| Q|##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
3 | Q|  |##|  |##|  |##|  | | Q|  |##|  |##|  |##|  | | Q|  |##|  |##|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
2 |  |##|  |##|  |##| Q|##| |  |##| Q|##|  |##|  |##| |  |##|  | Q|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
1 |##|  |##|  | Q|  |##|  | |##|  |##|  |##| Q|##|  | |##|  |##|  |##|  |##| Q|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
   A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H
 
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
8 |  |##| Q|##|  |##|  |##| |  |##| Q|##|  |##|  |##| |  |##|  | Q|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
7 |##|  |##|  |##| Q|##|  | |##|  |##|  |##|  | Q|  | |##| Q|##|  |##|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
6 |  | Q|  |##|  |##|  |##| |  | Q|  |##|  |##|  |##| |  |##|  |##|  |##|  | Q|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
5 |##|  |##|  | Q|  |##|  | |##|  |##|  |##|  |##| Q| |##|  |##|  | Q|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
4 |  |##|  |##|  |##|  | Q| |  |##|  |##| Q|##|  |##| |  |##|  |##|  |##| Q|##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
3 | Q|  |##|  |##|  |##|  | | Q|  |##|  |##|  |##|  | | Q|  |##|  |##|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
2 |  |##|  |##|  |##| Q|##| |  |##|  | Q|  |##|  |##| |  |##| Q|##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
1 |##|  |##| Q|##|  |##|  | |##|  |##|  |##| Q|##|  | |##|  |##|  |##| Q|##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
   A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H
 
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
8 |  |##|  | Q|  |##|  |##| |  | Q|  |##|  |##|  |##| |  |##|  | Q|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
7 |##| Q|##|  |##|  |##|  | |##|  |##| Q|##|  |##|  | |##|  |##|  |##|  |##| Q|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
6 |  |##|  |##| Q|##|  |##| |  |##|  |##|  | Q|  |##| |  |##|  |##| Q|##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
5 |##|  |##|  |##|  |##| Q| |##|  |##|  |##|  |##| Q| |##|  | Q|  |##|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
4 |  |##|  |##|  | Q|  |##| |  |##| Q|##|  |##|  |##| | Q|##|  |##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
3 | Q|  |##|  |##|  |##|  | | Q|  |##|  |##|  |##|  | |##|  |##|  |##|  | Q|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
2 |  |##| Q|##|  |##|  |##| |  |##|  |##|  |##| Q|##| |  | Q|  |##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
1 |##|  |##|  |##|  | Q|  | |##|  |##|  | Q|  |##|  | |##|  |##|  |##| Q|##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
   A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H
 
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
8 |  |##|  |##|  | Q|  |##| |  |##| Q|##|  |##|  |##| |  |##| Q|##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
7 |##|  | Q|  |##|  |##|  | |##|  |##|  | Q|  |##|  | |##|  |##|  |##|  |##| Q|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
6 |  |##|  |##| Q|##|  |##| |  |##|  |##|  |##|  | Q| |  |##|  | Q|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
5 |##|  |##|  |##|  |##| Q| |##|  |##| Q|##|  |##|  | |##|  |##|  |##|  | Q|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
4 | Q|##|  |##|  |##|  |##| | Q|##|  |##|  |##|  |##| | Q|##|  |##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
3 |##|  |##| Q|##|  |##|  | |##|  |##|  |##|  | Q|  | |##|  |##|  |##| Q|##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
2 |  | Q|  |##|  |##|  |##| |  | Q|  |##|  |##|  |##| |  | Q|  |##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
1 |##|  |##|  |##|  | Q|  | |##|  |##|  |##| Q|##|  | |##|  |##|  | Q|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
   A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H
 
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
8 |  |##|  |##|  | Q|  |##| |  |##| Q|##|  |##|  |##| |  |##|  | Q|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
7 |##|  |##|  |##|  |##| Q| |##|  |##|  | Q|  |##|  | |##| Q|##|  |##|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
6 |  | Q|  |##|  |##|  |##| |  | Q|  |##|  |##|  |##| |  |##|  |##|  |##|  | Q|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
5 |##|  |##| Q|##|  |##|  | |##|  |##|  |##|  |##| Q| |##|  |##|  |##| Q|##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
4 | Q|##|  |##|  |##|  |##| | Q|##|  |##|  |##|  |##| | Q|##|  |##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
3 |##|  |##|  |##|  | Q|  | |##|  |##|  |##|  | Q|  | |##|  | Q|  |##|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
2 |  |##|  |##| Q|##|  |##| |  |##|  | Q|  |##|  |##| |  |##|  |##| Q|##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
1 |##|  | Q|  |##|  |##|  | |##|  |##|  |##| Q|##|  | |##|  |##|  |##|  | Q|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
   A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H
 
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
8 |  |##|  |##|  |##|  | Q| |  | Q|  |##|  |##|  |##| |  | Q|  |##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
7 |##| Q|##|  |##|  |##|  | |##|  |##|  |##|  | Q|  | |##|  |##|  | Q|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
6 |  |##|  |##| Q|##|  |##| |  |##|  |##| Q|##|  |##| |  |##|  |##|  |##| Q|##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
5 |##|  | Q|  |##|  |##|  | |##|  |##|  |##|  |##| Q| |##|  |##| Q|##|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
4 | Q|##|  |##|  |##|  |##| | Q|##|  |##|  |##|  |##| | Q|##|  |##|  |##|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
3 |##|  |##|  |##|  | Q|  | |##|  |##| Q|##|  |##|  | |##|  |##|  |##|  |##| Q|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
2 |  |##|  | Q|  |##|  |##| |  |##|  |##|  | Q|  |##| |  |##|  |##|  | Q|  |##|
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
1 |##|  |##|  |##| Q|##|  | |##|  | Q|  |##|  |##|  | |##|  | Q|  |##|  |##|  |
  +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+ +--+--+--+--+--+--+--+--+
   A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H    A  B  C  D  E  F  G  H
 
  +--+--+--+--+--+--+--+--+
8 |  | Q|  |##|  |##|  |##|
  +--+--+--+--+--+--+--+--+
7 |##|  |##|  |##| Q|##|  |
  +--+--+--+--+--+--+--+--+
6 |  |##|  |##|  |##|  | Q|
  +--+--+--+--+--+--+--+--+
5 |##|  | Q|  |##|  |##|  |
  +--+--+--+--+--+--+--+--+
4 | Q|##|  |##|  |##|  |##|
  +--+--+--+--+--+--+--+--+
3 |##|  |##| Q|##|  |##|  |
  +--+--+--+--+--+--+--+--+
2 |  |##|  |##|  |##| Q|##|
  +--+--+--+--+--+--+--+--+
1 |##|  |##|  | Q|  |##|  |
  +--+--+--+--+--+--+--+--+
   A  B  C  D  E  F  G  H
```
</div>


## Nim


```nim
const boardSize = 8

proc underAttack(col, queens): bool =
  if col in queens: return true
  for i, x in queens:
    if abs(col - x) == queens.len - i:
      return true
  return false

proc solve(n): auto =
  result = newSeq[seq[int]]()
  result.add(@[])
  var newSolutions = newSeq[seq[int]]()
  for row in 1..n:
    for solution in result:
      for i in 1..boardSize:
        if not underAttack(i, solution):
          newSolutions.add(solution & i)
    swap result, newSolutions
    newSolutions.setLen(0)

for answer in solve(boardSize):
  for i, x in answer:
    if i > 0: stdout.write ", "
    stdout.write "(",i,", ",x,")"
```



## Objeck

{{trans|Java}}


```objeck
﻿bundle Default {
  class NQueens {
    b : static : Int[];
    s : static : Int;

    function : Main(args : String[]) ~ Nil {
      b := Int->New[8];
      s := 0;

      y := 0;
      b[0] := -1;

      while (y >= 0) {
        do {
          b[y]+=1;
        } 
        while((b[y] < 8) & Unsafe(y));

        if(b[y] < 8) {
          if (y < 7) {
            b[y + 1] := -1;
            y += 1;
          } 
          else {
            PutBoard();
          };
        } 
        else {
          y-=1;
        };
      };
    }

    function : Unsafe(y : Int) ~ Bool {
      x := b[y];
      for(i := 1; i <= y; i+=1;) {
        t := b[y - i];
        if(t = x | t = x - i | t = x + i) {
          return true;
        };
      };
  
      return false;
    }

    function : PutBoard() ~ Nil {
      IO.Console->Print("\n\nSolution ")->PrintLine(s + 1);
      s += 1;
      for(y := 0; y < 8; y+=1;) {
        for(x := 0; x < 8; x+=1;) {
          IO.Console->Print((b[y] = x) ? "|Q" : "|_");
        };
        "|"->PrintLine();
      };
    }
  }
}

```



## OCaml

{{libheader|FaCiLe}}


```ocaml
(* Authors: Nicolas Barnier, Pascal Brisset
   Copyright 2004 CENA. All rights reserved.
   This code is distributed under the terms of the GNU LGPL *)

open Facile
open Easy

(* Print a solution *)
let print queens =
  let n = Array.length queens in
  if n <= 10 then (* Pretty printing *)
    for i = 0 to n - 1 do
      let c = Fd.int_value queens.(i) in (* queens.(i) is bound *)
      for j = 0 to n - 1 do
        Printf.printf "%c " (if j = c then '*' else '-')
      done;
      print_newline ()
    done
  else (* Short print *)
    for i = 0 to n-1 do
      Printf.printf "line %d : col %a\n" i Fd.fprint queens.(i)
    done;
  flush stdout;
;;

(* Solve the n-queens problem *)
let queens n =
  (* n decision variables in 0..n-1 *)
  let queens = Fd.array n 0 (n-1) in

  (* 2n auxiliary variables for diagonals *)
  let shift op = Array.mapi (fun i qi -> Arith.e2fd (op (fd2e qi) (i2e i))) queens in
  let diag1 = shift (+~) and diag2 = shift (-~) in

  (* Global constraints *)
  Cstr.post (Alldiff.cstr queens);
  Cstr.post (Alldiff.cstr diag1);
  Cstr.post (Alldiff.cstr diag2);

  (* Heuristic Min Size, Min Value *)
  let h a = (Var.Attr.size a, Var.Attr.min a) in
  let min_min = Goals.Array.choose_index (fun a1 a2 -> h a1 < h a2) in

  (* Search goal *)
  let labeling = Goals.Array.forall ~select:min_min Goals.indomain in

  (* Solve *)
  let bt = ref 0 in
  if Goals.solve ~control:(fun b -> bt := b) (labeling queens) then begin
    Printf.printf "%d backtracks\n" !bt;
    print queens
  end else
    prerr_endline "No solution"

let _ =
  if Array.length Sys.argv <> 2
  then raise (Failure "Usage: queens <nb of queens>");
  Gc.set ({(Gc.get ()) with Gc.space_overhead = 500}); (* May help except with an underRAMed system *)
  queens (int_of_string Sys.argv.(1));;
```

===A stand-alone OCaml solution===

```ocaml
let solutions n =

  let show board =
    let pr v =
      for i = 1 to n do
        print_string (if i=v then " q" else " _");
      done;
      print_newline() in
    List.iter pr board;
    print_newline() in

  let rec safe i j k = function
    | [] -> true
    | h::t -> h<>i && h<>j && h<>k && safe i (j+1) (k-1) t in

  let rec loop col p =
    for i = 1 to n
    do
      if safe i (i+1) (i-1) p then
        let p' = i::p in
        if col = n then show p'
        else loop (col+1) p'
    done in

  loop 1 [] in

let n =
  if Array.length Sys.argv > 1
  then int_of_string Sys.argv.(1)
  else 8 in

solutions n
```

{{out}}

```txt
$ ocaml queens.ml 6
 _ _ _ _ q _
 _ _ q _ _ _
 q _ _ _ _ _
 _ _ _ _ _ q
 _ _ _ q _ _
 _ q _ _ _ _

 _ _ _ q _ _
 q _ _ _ _ _
 _ _ _ _ q _
 _ q _ _ _ _
 _ _ _ _ _ q
 _ _ q _ _ _

 _ _ q _ _ _
 _ _ _ _ _ q
 _ q _ _ _ _
 _ _ _ _ q _
 q _ _ _ _ _
 _ _ _ q _ _

 _ q _ _ _ _
 _ _ _ q _ _
 _ _ _ _ _ q
 q _ _ _ _ _
 _ _ q _ _ _
 _ _ _ _ q _

```



## Oz

A pretty naive solution, using constraint programming:

```oz
declare
  fun {Queens N}
     proc {$ Board}
        %% a board is a N-tuple of rows
        Board = {MakeTuple queens N}
        for Y in 1..N  do
           %% a row is a N-tuple of values in [0,1]
           %% (0: no queen, 1: queen)
           Board.Y = {FD.tuple row N 0#1}
        end

        {ForAll {Rows Board} SumIs1}
        {ForAll {Columns Board} SumIs1}

        %% for every two points on a diagonal
        for [X1#Y1 X2#Y2] in {DiagonalPairs N} do
           %$ at most one of them has a queen
           Board.Y1.X1 + Board.Y2.X2 =<: 1
        end

        %% enumerate all such boards
        {FD.distribute naive {FlatBoard Board}}
     end
  end

  fun {Rows Board}
     {Record.toList Board}
  end

  fun {Columns Board}
     for X in {Arity Board.1} collect:C1 do
        {C1
         for Y in {Arity Board} collect:C2 do
            {C2 Board.Y.X}
         end}
     end
  end

  proc {SumIs1 Xs}
     {FD.sum Xs '=:' 1}
  end

  fun {DiagonalPairs N}
     proc {Coords Root}
        [X1#Y1 X2#Y2] = Root
        Diff
     in
        X1::1#N Y1::1#N
        X2::1#N Y2::1#N
        %% (X1,Y1) and (X2,Y2) are on a diagonal if {Abs X2-X1} = {Abs Y2-Y1}
        Diff::1#N-1
        {FD.distance X2 X1 '=:' Diff}
        {FD.distance Y2 Y1 '=:' Diff}
        %% enumerate all such coordinates
        {FD.distribute naive [X1 Y1 X2 Y2]}
     end
  in
     {SearchAll Coords}
  end

  fun {FlatBoard Board}
     {Flatten {Record.toList {Record.map Board Record.toList}}}
  end

  Solutions = {SearchAll {Queens 8}}
in
  {Length Solutions} = 92 %% assert
  {Inspect {List.take Solutions 3}}
```


There is a more concise and much more efficient [http://www.mozart-oz.org/documentation/fdt/node25.html#section.scripts.queens solution] in the Mozart documentation.



## Pascal


```pascal
program queens;

const l=16;

var i,j,k,m,n,p,q,r,y,z: integer;
    a,s: array[1..l] of integer;
    u: array[1..4*l-2] of integer;

label L3,L4,L5,L6,L7,L8,L9,L10;

begin
   for i:=1 to l do a[i]:=i;
   for i:=1 to 4*l-2 do u[i]:=0;
   for n:=1 to l do
   begin
      m:=0;
      i:=1;
      r:=2*n-1;
      goto L4;
L3:
      s[i]:=j;
      u[p]:=1;
      u[q+r]:=1;
      i:=i+1;
L4:
      if i>n then goto L8;
      j:=i;
L5:
      z:=a[i];
      y:=a[j];
      p:=i-y+n;
      q:=i+y-1;
      a[i]:=y;
      a[j]:=z;
      if (u[p]=0) and (u[q+r]=0) then goto L3;
L6:
      j:=j+1;
      if j<=n then goto L5;
L7:
      j:=j-1;
      if j=i then goto L9;
      z:=a[i];
      a[i]:=a[j];
      a[j]:=z;
      goto L7;
L8:
      m:=m+1;
      { uncomment the following to print solutions }
      { write(n,' ',m,':');
      for k:=1 to n do write(' ',a[k]);
      writeln; }
L9:
      i:=i-1;
      if i=0 then goto L10;
      p:=i-a[i]+n;
      q:=i+a[i]-1;
      j:=s[i];
      u[p]:=0;
      u[q+r]:=0;
      goto L6;
L10:
      writeln(n,' ',m);
   end;
end.

{ 1 1
  2 0
  3 0
  4 2
  5 10
  6 4
  7 40
  8 92
  9 352
 10 724
 11 2680
 12 14200
 13 73712
 14 365596
 15 2279184
 16 14772512 }
```



### Alternative

Using Rekusion and Nikolaus Wirth is much faster.
Ok , this http://rosettacode.org/wiki/N-queens_problem#Fast_Version is nearly 4 times faster, but uses sysmmetry (50% less to search for) :

algo:

```txt

recursion:
  If row< n then 
    For each free column (in Freecol[row..n] )
      Take free column
      check diagonals
      IF free then
        swap freecol to used column, move to next  row -> recurse(row+1)
  else
    Solution found
```

    

```pascal
program NQueens;
{$IFDEF FPC}
   {$MODE DELPHI}
   {$OPTIMIZATION ON}{$OPTIMIZATION REGVAR}{$OPTIMIZATION PeepHole}
   {$OPTIMIZATION CSE}{$OPTIMIZATION ASMCSE}
{$ELSE}
  {$Apptype console}
{$ENDIF}

uses
  sysutils;// TDatetime
const
  nmax = 17;
type
{$IFNDEF FPC}
  NativeInt = longInt;
{$ENDIF}
  //ala Nikolaus Wirth  A-1  = H - 8
  //diagonal left  (A1) to rigth (H8)
  tLR_diagonale = array[-nmax-1..nmax-1] of char;
  //diagonal right (A8) to left (H1)
  tRL_diagonale = array[0..2*nmax-2] of char;
  //up to Col are the used Cols, after that the unused
  tFreeCol = array[0..nmax-1] of nativeInt;
var
  LR_diagonale:tLR_diagonale;
  RL_diagonale:tRL_diagonale;
  //Using pChar, cause it is implicit an array
  //It is always set to
  //@LR_diagonale[row] ,@RL_diagonale[row]
  pLR,pRL : pChar;
  FreeCol : tFreeCol;
  i,
  n : nativeInt;
  gblCount : nativeUInt;
  T0,T1 : TdateTime;
procedure Solution;
var
  i : NativeInt;
begin
// Take's a lot of time under DOS/Win32
  If gblCount AND $FFF = 0 then
    write(gblCount:10,#8#8#8#8#8#8#8#8#8#8);
  // IF n< 9 then
  IF n < 0 then
   begin
     For i := 1 to n do
       write(FreeCol[i]:4);
     writeln;
   end;
end;

procedure SetQueen(Row:nativeInt);
var
  i,Col : nativeInt;
begin
IF row <= n then
  begin
  For i := row to n do
    begin
    Col := FreeCol[i];
    //check diagonals occupied
    If (ORD(pLR[-Col]) AND ORD(pRL[Col]))<>0 then
      begin
      //a "free" position is found
      //mark it
      pRL[ Col]:=#0;      //RL_Diagonale[ Row +Col] := 0;
      pLR[-Col]:=#0;      //LR_Diagonale[ Row -Col] := 0;
      //swap FreeRow[Row<->i]
      FreeCol[i] := FreeCol[Row];
      //next row
      inc(pRL);
      inc(pLR);
      FreeCol[Row] := Col;
      // check next row
        SetQueen(Row+1);
      //Undo
      dec(pLR);
      dec(pRL);
      FreeCol[Row] := FreeCol[i];
      FreeCol[i] := Col;
      pRL[ Col]:=#1;
      pLR[-Col]:=#1;
      end;
    end;
  end
else
  begin
  //solution ist found
  inc(gblCount);
  //Solution
  end;
end;

begin
  For i := 0 to nmax-1 do
    FreeCol[i] := i;
  //diagonals filled with True = #1 , something <>0
  fillchar(LR_Diagonale[low(LR_Diagonale)],sizeof(tLR_Diagonale),#1);
  fillchar(RL_Diagonale[low(RL_Diagonale)],sizeof(tRL_Diagonale),#1);
  For n := 1 to nMax do
    begin
    t0 := time;
    pLR:=@LR_Diagonale[0];
    pRL:=@RL_Diagonale[0];
    gblCount := 0;
    SetQueen(1);
    t1:= time;
    WriteLn(n:6,gblCount:12,FormatDateTime(' NN:SS.ZZZ',T1-t0),' secs');
    end;
  WriteLn('Fertig');
end.
```

{{out}}

```txt

{output: i3 4330 3.5 Ghz FPC 2.6.4
     1           1  00:00.000 secs
     2           0  00:00.000 secs
     3           0  00:00.000 secs
     4           2  00:00.000 secs
     5          10  00:00.000 secs
     6           4  00:00.000 secs
     7          40  00:00.000 secs
     8          92  00:00.000 secs
     9         352  00:00.000 secs
    10         724  00:00.001 secs
    11        2680  00:00.004 secs
    12       14200  00:00.019 secs
    13       73712  00:00.104 secs
    14      365596  00:00.610 secs
    15     2279184  00:03.837 secs
    16    14772512  00:25.684 secs
    17    95815104  03:00.950 secs=180.98 secs
Fertig}
```



## Perl


```perl
my ($board_size, @occupied, @past, @solutions);

sub try_column {
        my ($depth, @diag) = shift;
        if ($depth == $board_size) {
                push @solutions, "@past\n";
                return;
        }

        # @diag: marks cells diagonally attackable by any previous queens.
        #        Here it's pre-allocated to double size just so we don't need
        #        to worry about negative indices.
        $#diag = 2 * $board_size;
        for (0 .. $#past) {
                $diag[ $past[$_] + $depth - $_ ] = 1;
                $diag[ $past[$_] - $depth + $_ ] = 1;
        }

        for my $row (0 .. $board_size - 1) {
                next if $occupied[$row] || $diag[$row];

                # @past:     row numbers of previous queens
                # @occupied: rows already used. This gets inherited by each
                #            recursion so we don't need to repeatedly look them up
                push @past, $row;
                $occupied[$row] = 1;

                try_column($depth + 1);

                # clean up, for next recursion
                $occupied[$row] = 0;
                pop @past;
        }
}

$board_size = 12;
try_column(0);

#print for @solutions; # un-comment to see all solutions
print "total " . @solutions . " solutions\n";
```

{{out}}

```txt
total 14200 solutions
```



## Perl 6

{{works with|rakudo|2015-11-29}}
Neither pretty nor efficient, a simple backtracking solution


```perl6
sub MAIN(\N = 8) {
    sub collision(@field, $row) {
        for ^$row -> $i {
            my $distance = @field[$i] - @field[$row];
            return True if $distance == any(0, $row - $i, $i - $row);
        }
        False;
    }
    sub search(@field, $row) {
        return @field if $row == N;
        for ^N -> $i {
            @field[$row] = $i;
            return search(@field, $row + 1) || next
                unless collision(@field, $row);
        }
        ()
    }
    for 0 .. N / 2 {
        if search [$_], 1 -> @f {
            say @f;
            last;
        }
    }
}
```

{{out}}

```txt
[0 4 7 5 2 6 1 3]
```



## Phix


```Phix
--
-- demo\rosetta\n_queens.exw
-- 
### ===================

--
sequence co,    -- columns occupied
                -- (ro is implicit)
         fd,    -- forward diagonals
         bd,    -- backward diagonals
         board

atom count

procedure solve(integer row, integer N, integer show)
    for col=1 to N do
        if not co[col] then
            integer fdi = col+row-1,
                    bdi = row-col+N
            if not fd[fdi]
            and not bd[bdi] then
                board[row][col] = 'Q'
                co[col] = 1
                fd[fdi] = 1
                bd[bdi] = 1
                if row=N then
                    if show then
                        puts(1,join(board,"\n")&"\n")
                        puts(1,repeat('=',N)&"\n")
                    end if
                    count += 1
                else
                    solve(row+1,N,show)
                end if
                board[row][col] = '.'
                co[col] = 0
                fd[fdi] = 0
                bd[bdi] = 0
            end if
        end if
    end for
end procedure

procedure n_queens(integer N=8, integer show=1)
    co = repeat(0,N)
    fd = repeat(0,N*2-1)
    bd = repeat(0,N*2-1)
    board  = repeat(repeat('.',N),N)
    count = 0
    solve(1,N,show)
    printf(1,"%d queens: %d solutions\n",{N,count})
end procedure

for N=1 to 14 do
    n_queens(N,N<5)
end for
```

{{out}}

```txt

Q
=
1 queens: 1 solutions
2 queens: 0 solutions
3 queens: 0 solutions
.Q..
...Q
Q...
..Q.
====
..Q.
Q...
...Q
.Q..
====
4 queens: 2 solutions
5 queens: 10 solutions
6 queens: 4 solutions
7 queens: 40 solutions
8 queens: 92 solutions
9 queens: 352 solutions
10 queens: 724 solutions
11 queens: 2680 solutions
12 queens: 14200 solutions
13 queens: 73712 solutions
14 queens: 365596 solutions

```

N=14 takes about 10s


## PHP


Probably not a great solution given this is one of my first forays into PHP. 
First solves the n rooks problem and then finds solutions for n-queens, 
disregarding any rotations/reflections. Checked up to n=10. 


```PHP

<html>
<head>
<title>
n x n Queen solving program
</title>
</head>
<body>
<?php
echo "<h1>n x n Queen solving program</h1>";

//Get the size of the board
$boardX = $_POST['boardX'];
$boardY = $_POST['boardX'];

// Function to rotate a board 90 degrees
function rotateBoard($p, $boardX) {
$a=0;
while ($a < count($p)) {
	$b = strlen(decbin($p[$a]))-1;
	$tmp[$b] = 1 << ($boardX - $a - 1);
	++$a;
	}
ksort($tmp);
return $tmp;
}

// This function will find rotations of a solution
function findRotation($p, $boardX,$solutions){
$tmp = rotateBoard($p,$boardX);
// Rotated 90
if (in_array($tmp,$solutions)) {}
else {$solutions[] = $tmp;}

$tmp = rotateBoard($tmp,$boardX);
// Rotated 180
if (in_array($tmp,$solutions)){}
else {$solutions[] = $tmp;}

$tmp = rotateBoard($tmp,$boardX);
// Rotated 270
if (in_array($tmp,$solutions)){}
else {$solutions[] = $tmp;}

// Reflected
$tmp = array_reverse($p);
if (in_array($tmp,$solutions)){}
else {$solutions[] = $tmp;}

$tmp = rotateBoard($tmp,$boardX);
// Reflected and Rotated 90
if (in_array($tmp,$solutions)){}
else {$solutions[] = $tmp;}

$tmp = rotateBoard($tmp,$boardX);
// Reflected and Rotated 180
if (in_array($tmp,$solutions)){}
else {$solutions[] = $tmp;}

$tmp = rotateBoard($tmp,$boardX);
// Reflected and Rotated 270
if (in_array($tmp,$solutions)){}
else {$solutions[] = $tmp;}
return $solutions;
}

// This is a function which will render the board
function renderBoard($p,$boardX) {
$img = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAC0AAAAtCAYAAAA6GuKaAAAABmJLR0QA/wD/AP+gvaeTAAAGFUlEQVRYhe2YXWibVRjHf2lqP9JmaRi4YW1IalY3rbZsaddMgsquBm676b6KyNDhLiaUeSEMvPNCcNuNyJjgLiboCnoxKFlv6lcHy7AtMhhaWTVZWhisjDTEtEuW5PHiPWnfvH2TvNk6vekfDm/O+Z/zPP/3PM/5eAMb2MAG/nfYn4LNVuBj4ENgB/Ar8Ogp+KkJbwLfqvKGgbMBPwKiK+Oq3aqNdcebQEEnqAC8ruO7KBVcLF012KiKuhpFv0/prNlU239qw0x0pdBJFXt30NJDjx9Uu1Ub1TSYdq4UutcNfI61oW0Bflb8T6quRzUbNafPFdbm4zcmTucV91kZO18o/osy/GeKnzcRVFWDMT2shO4X4IL6/UqZPv2GpxHFcReUvVo1lMAYunKh+UTxeeB5A/cMkFF8RtX1eF6NE2XHTIN+ltekoHGmf0HLqe9V3Qb8ZWK4Xjf+HQP3KtCgfjeouh7v6PzWsxZ6f98De1kbjbIovumoCfcp2gzkgb8p3cJOUjpTJ3WcTfXPq/Gfmtge1Y01RaV9+jv1fAsYMnAu3XgfENJxfUoU6tmn40Kqf9Gvi1IMKX96/zWJnlLP4i7wrIEvzkQeeFfXvltnt07Vi3iX1RcyzuSzrO46ev81YS+rYcqjbUVFfIl2CSryS4ATcKCF3biQHIpf0rU/UnaKuMLqAhXlv2a4Dc4FOKi4bwyiBTgBvGYyRlT7CUPbI1b334MmY9zlhFVKjwQQ09ULaDNTNKYPbx54j9L81aNP8XldW3G8W9kt6LiY8m8Ksy1Hj0mgA+3eXYeWd2eBRkpf2A4MoO3JOYPdHPA2sMtgu07ZOavsFnegvPL72PiItWEroB0axtwtmPStxOeUHbNxH1USVe1qOm3SVkA7NIwX+1phU3YKJpyZX8swW4y1FOMsVotG1UUI1mbrH9ZeL/UQi3b0C7dS/2W0LbIsqi1E0K6PL5oRdrudHTt22Px+Pz6fD6/XS3NzM21tbSt9FhcXWVpaIhqN2mKxGLOzs8zMzJDP581MQukHw2OLPgt8VRQZDAbZv38/wWCQnTt30tKyGoRUKsWDBw/IZrOkUimcTicNDQ1s3rwZp9O50i+dTjM9Pc2NGzcIh8NEIhH9S3xuQVNV2IArp06dkoWFBRERefjwoUxMTMi5c+fk8OHD0tPTIy6Xq2Keulwu6enpkSNHjsj58+dlYmJCMpmMiIgsLCzIxYsXBe1UfNIFvoL6M2fO/Hn58uXC4OCgtLa2PsniXClOp1MGBwfl0qVLhdOnT/+BtcjX9FYe4Pe+vj6Hy+Vat9lIJpMyOTm5BLwExNfL7gpCodAFeQoIhUIXqntfhaVwFHH9+nXp7+8vuFyuWv8vKYtkMlmYnJwse+F/Urzi9/ulqanJ6gFhqTQ1NeW7u7sF6Fx3xd3d3bdERNLptITDYRkeHpZgMCgOh6MmkQ6HQ/bs2SPDw8MSDoclnU6LiMju3buvlHG9BlYX1F5gfGhoiEAgwL59+9i+fTsAuVyOWCxGPB4nHo+TSCTIZrMkEgncbjeNjY243W46OjrweDx4vV7q67WsnJmZYWxsjGvXrjE+Pm5Zj1XRX3d2dg7Nz8/bs9ksAFu2bGHXrl0EAgG2bduG1+vF4/HgdDrZtGkTdrudXC5HKpUilUpx9+5dYrEYd+7cYXp6mqmpKe7fvw9AQ0MDXV1d3L59+2Xgd4uaKqO3t/cnEZFkMikjIyNy9OhRaW9vf6Jcbm9vl2PHjsnIyIgkk0kRETl06NAHVvRYnenA8ePHJ4PBIAcOHGDr1q0AxONxbt68yezsLNFolLm5ORKJBMvLy6TTaVpaWmhubl5JD5/Ph9/vZ2BgAI/HA8C9e/cYHR3l6tWry2NjY88Bi+slGqAHOFVXVxfq7e3tGhgYqAsGgwQCAfH5fLbGxsaqBjKZDNFoVKampmyRSIRIJFK4devWn4VC4TpwEfjNipDHPdlagADaf3X9NpvthY6Ojk6Px+Mq3vLsdjv5fJ7FxUWWl5eJx+OJubm5mIjMon1O/Yr2N0G6VufrdhwrtAJtaN9+bWihzqB9pNYsbgMbeAz8C3N/JQD4H5KCAAAAAElFTkSuQmCC';
echo "<table border=1 cellspacing=0 style='text-align:center;display:inline'>";
for ($y = 0; $y < $boardX; ++$y) {
	echo '<tr>';
	for ($x = 0; $x < $boardX; ++$x){
	if (($x+$y) & 1) { $cellCol = '#9C661F';}
	else {$cellCol = '#FCE6C9';}
	
	if ($p[$y] == 1 << $x) { echo "<td bgcolor=".$cellCol."><img width=30 height=30 src='".$img."'></td>";}
	else { echo "<td bgcolor=".$cellCol."> </td>";}
	}
	echo '<tr>';
}
echo '<tr></tr></table>&nbsp';

}

//This function allows me to generate the next order of rows.
function pc_next_permutation($p) {
$size = count($p) - 1;
// slide down the array looking for where we're smaller than the next guy 

for ($i = $size - 1; $p[$i] >= $p[$i+1]; --$i) { } 

// if this doesn't occur, we've finished our permutations 
// the array is reversed: (1, 2, 3, 4) => (4, 3, 2, 1) 
if ($i == -1) { return false; } 

// slide down the array looking for a bigger number than what we found before 
for ($j = $size; $p[$j] <= $p[$i]; --$j) { } 
// swap them 
$tmp = $p[$i]; $p[$i] = $p[$j]; $p[$j] = $tmp; 
// now reverse the elements in between by swapping the ends 
for (++$i, $j = $size; $i < $j; ++$i, --$j) 
{ $tmp = $p[$i]; $p[$i] = $p[$j]; $p[$j] = $tmp; } 
return $p; 
} 

//This function needs to check the current state to see if there are any 
function checkBoard($p,$boardX) {
	$a = 0; //this is the row being checked
	while ($a < count($p)) { 
		$b = 1;
		while ($b < ($boardX - $a)){
		$x = $p[$a+$b] << $b;
		$y = $p[$a+$b] >> $b;
		if ($p[$a] == $x | $p[$a] == $y) { return false;}		
		++$b;
		}
	++$a; 
	}
	return true;
}


if (isset($_POST['process']) && isset($_POST['boardX']))
{
//Within here is the code that needs to be run if process is clicked.


//First I need to create the different possible rows
for ($x = 0; $x < $boardX; ++$x){
	$row[$x] = 1 << $x;
	}

//Now I need to create all the possible orders of rows, will be equal to [boardY]!
$solcount = 0;
$solutions = array();
while ($row != false) {
	if (checkBoard($row,$boardX)){
	if(!in_array($row,$solutions)){
		$solutions[] = $row;
			renderBoard($row,$boardX);
			$solutions = findRotation($row,$boardX,$solutions);
			++$solcount;
		}
		
	}
	$row = pc_next_permutation($row);	
	
	}
echo "

&nbsp&nbsp&nbsp&nbspRows/Columns: ".$boardX."
&nbsp&nbsp&nbsp&nbspUnique Solutions: ".$solcount."
&nbsp&nbsp&nbsp&nbspTotal Solutions: ".count($solutions)."  - Note: This includes symmetrical solutions
";
//print_r($solutions);
}

//This code collects the starting parameters
echo <<<_END
<form name="input" action="queens.php" method="post">
&nbsp&nbsp&nbsp&nbspNumber of columns/rows <select name="boardX" />
<option value="1">One</option>
<option value="2">Two</option>
<option value="3">Three</option>
<option value="4" >Four</option>
<option value="5">Five</option>
<option value="6">Six</option>
<option value="7">Seven</option>
<option value="8" selected="selected">Eight</option>
<option value="9">Nine</option>
<option value="10">Ten</option>
</select>
    <input type="hidden" name="process" value="yes" />
&nbsp<input type="submit" value="Process" />
</form>

_END;

?>
</body>
</html>
```



## Picat


```Picat
import cp.

% CP approach
queens_cp(N, Q) =>
   Q = new_list(N),
   Q :: 1..N,

   all_different(Q),
   all_different([$Q[I]-I : I in 1..N]),
   all_different([$Q[I]+I : I in 1..N]),
   solve([ff],Q).

% SAT approach (using a N x N matrix)
queens_sat(N,Q) =>
  Q = new_array(N,N),
  Q :: 0..1,
  
  foreach (K in 1-N..N-1)
    sum([Q[I,J] : I in 1..N, J in 1..N, I-J==K]) #=< 1
  end,

  foreach (K in 2..2*N)
    sum([Q[I,J] :  I in 1..N, J in 1..N, I+J==K]) #=< 1
  end,

  foreach (I in 1..N)
    sum([Q[I,J] : J in 1..N]) #= 1
  end,

  foreach (J in 1..N)
    sum([Q[I,J] : I in 1..N]) #= 1
  end,
  solve([inout],Q).


```


Output:

```txt

Picat> queens_cp(100, QCP)
QCP = [1,3,5,57,59,4,64,7,58,71,81,60,6,91,82,90,8,83,77,65,73,26,9,45,37,63,66,62,44,10,48,54,43,69,42,47,18,11,72,68,50,56,61,36,33,17,12,51,100,93,97,88,35,84,78,19,13,99,67,76,92,75,87,96,94,85,20,14,95,32,98,55,40,80,49,52,46,53,21,15,41,2,27,34,22,70,74,29,25,30,38,86,16,79,24,39,28,23,31,89] 

Picat> queens_sat(10,Q) 
Q = {{0,0,0,0,0,0,0,0,1,0},{0,1,0,0,0,0,0,0,0,0},{0,0,0,0,1,0,0,0,0,0},{0,0,1,0,0,0,0,0,0,0},{1,0,0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,0,0,1},{0,0,0,0,0,0,0,1,0,0},{0,0,0,0,0,1,0,0,0,0},{0,0,0,1,0,0,0,0,0,0},{0,0,0,0,0,0,1,0,0,0}}

% Number of solutions for N = 1..15
Picat> foreach(N in 1..15)  println(N=count_all(queens_cp(N,_))) end
1 = 1
2 = 0
3 = 0
4 = 2
5 = 10
6 = 4
7 = 40
8 = 92
9 = 352
10 = 724
11 = 2680
12 = 14200
13 = 73712
14 = 365596
15 = 2279184


```



## PicoLisp

===Calling 'permute'===

```PicoLisp
(load "@lib/simul.l")  # for 'permute'

(de queens (N)
   (let (R (range 1 N)  Cnt 0)
      (for L (permute (range 1 N))
         (when
            (= N  # from the Python solution
               (length (uniq (mapcar + L R)))
               (length (uniq (mapcar - L R))) )
            (inc 'Cnt) ) )
      Cnt ) )
```


### Permuting inline

This alternative version does not first pre-generate all permutations with
'permute', but creates them recursively. Also, it directly checks for
duplicates, instead of calling 'uniq' and 'length'. This is much faster.

```PicoLisp
(de queens (N)
   (let (R (range 1 N)  L (copy R)  X L  Cnt 0)
      (recur (X)  # Permute
         (if (cdr X)
            (do (length X)
               (recurse (cdr X))
               (rot X) )
            (or
               (seek  # Direct check for duplicates
                  '((L) (member (car L) (cdr L)))
                  (mapcar + L R) )
               (seek
                  '((L) (member (car L) (cdr L)))
                  (mapcar - L R) )
               (inc 'Cnt) ) ) )
      Cnt ) )
```

{{out}} for both cases:

```txt
: (queens 8)
-> 92
```



## PowerBASIC


###  Recursive version 

{{trans|Stata}}

```powerbasic
#COMPILE EXE
#DIM ALL

SUB aux(n AS INTEGER, i AS INTEGER, a() AS INTEGER, _
        u() AS INTEGER, v() AS INTEGER, m AS QUAD)

    LOCAL j, k, p, q AS INTEGER
    IF i > n THEN
        INCR m
        FOR k = 1 TO n : PRINT a(k); : NEXT : PRINT
    ELSE
        FOR j = i TO n
            k = a(j)
            p = i - k + n
            q = i + k - 1
            IF u(p) AND v(q) THEN
                u(p) = 0 : v(q) = 0
                a(j) = a(i) : a(i) = k
                CALL aux(n, i + 1, a(), u(), v(), m)
                u(p) = 1 : v(q) = 1
                a(i) = a(j) : a(j) = k
            END IF
        NEXT
    END IF
END SUB

FUNCTION PBMAIN () AS LONG
    LOCAL n, i AS INTEGER
    LOCAL m AS QUAD
    IF COMMAND$(1) <> "" THEN
        n = VAL(COMMAND$(1))
        REDIM a(1 TO n) AS INTEGER
        REDIM u(1 TO 2 * n - 1) AS INTEGER
        REDIM v(1 TO 2 * n - 1) AS INTEGER
        FOR i = 1 TO n
            a(i) = i
        NEXT
        FOR i = 1 TO 2 * n - 1
            u(i) = 1
            v(i) = 1
        NEXT
        m = 0
        CALL aux(n, 1, a(), u(), v(), m)
        PRINT m
    END IF
END FUNCTION
```



###  Iterative version 

{{trans|Stata}}

```powerbasic
#COMPILE EXE
#DIM ALL

FUNCTION PBMAIN () AS LONG
    LOCAL n, i, j, k, p, q AS INTEGER
    LOCAL m AS QUAD
    IF COMMAND$(1) <> "" THEN
        n = VAL(COMMAND$(1))
        REDIM a(1 TO n) AS INTEGER
        REDIM s(1 TO n) AS INTEGER
        REDIM u(1 TO 2 * n - 1) AS INTEGER
        REDIM v(1 TO 2 * n - 1) AS INTEGER
        FOR i = 1 TO n
            a(i) = i
        NEXT
        FOR i = 1 TO 2 * n - 1
            u(i) = 1
            v(i) = 1
        NEXT
        m = 0
        i = 1
      1 IF i > n THEN
            INCR m
            FOR k = 1 TO n : PRINT a(k); : NEXT : PRINT
            GOTO 4
        END IF
        j = i
      2 k = a(j)
        p = i - k + n
        q = i + k - 1
        IF u(p) AND v(q) THEN
            u(p) = 0 : v(q) = 0
            a(j) = a(i) : a(i) = k
            s(i) = j
            INCR i
            GOTO 1
        END IF
      3 INCR j : IF j <= n GOTO 2
      4 DECR i : IF i = 0 THEN PRINT m : EXIT FUNCTION
        j = s(i)
        k = a(i) : a(i) = a(j) : a(j) = k
        p = i - k + n
        q = i + k - 1
        u(p) = 1 : v(q) = 1
        GOTO 3
    END IF
END FUNCTION
```



## PowerShell

{{works with|PowerShell|2}}

```PowerShell

function PlaceQueen ( [ref]$Board, $Row, $N )
    {
    #  For the current row, start with the first column
    $Board.Value[$Row] = 0

    #  While haven't exhausted all columns in the current row...
    While ( $Board.Value[$Row] -lt $N )
        {
        #  If not the first row, check for conflicts
        $Conflict = $Row -and
                    (   (0..($Row-1)).Where{ $Board.Value[$_] -eq $Board.Value[$Row] }.Count -or
                        (0..($Row-1)).Where{ $Board.Value[$_] -eq $Board.Value[$Row] - $Row + $_ }.Count -or
                        (0..($Row-1)).Where{ $Board.Value[$_] -eq $Board.Value[$Row] + $Row - $_ }.Count )
 
        #  If no conflicts and the current column is a valid column...
        If ( -not $Conflict -and $Board.Value[$Row] -lt $N )
            {

            #  If this is the last row
            #    Board completed successfully
            If ( $Row -eq ( $N - 1 ) )
                {
                return $True
                }

            #  Recurse
            #  If all nested recursions were successful
            #    Board completed successfully
            If ( PlaceQueen $Board ( $Row + 1 ) $N )
                {
                return $True
                }
            }
        
        #  Try the next column
        $Board.Value[$Row]++
        }

    #  Everything was tried, nothing worked
    Return $False
    }
 
function Get-NQueensBoard ( $N )
    {
    #  Start with a default board (array of column positions for each row)
    $Board = @( 0 ) * $N

    #  Place queens on board
    #  If successful...
    If ( PlaceQueen -Board ([ref]$Board) -Row 0 -N $N )
        {
        #  Convert board to strings for display
        $Board | ForEach { ( @( "" ) + @(" ") * $_ + "Q" + @(" ") * ( $N - $_ ) ) -join "|" }
        }
    Else
        {
        "There is no solution for N = $N"
        }
    }

```


```PowerShell

Get-NQueensBoard 8
''
Get-NQueensBoard 3
''
Get-NQueensBoard 4
''
Get-NQueensBoard 14

```

{{out}}

```txt

|Q| | | | | | | | 
| | | | |Q| | | | 
| | | | | | | |Q| 
| | | | | |Q| | | 
| | |Q| | | | | | 
| | | | | | |Q| | 
| |Q| | | | | | | 
| | | |Q| | | | | 

There is no solution for N = 3

| |Q| | | 
| | | |Q| 
|Q| | | | 
| | |Q| | 

|Q| | | | | | | | | | | | | | 
| | |Q| | | | | | | | | | | | 
| | | | |Q| | | | | | | | | | 
| | | | | | |Q| | | | | | | | 
| | | | | | | | | | | |Q| | | 
| | | | | | | | | |Q| | | | | 
| | | | | | | | | | | | |Q| | 
| | | |Q| | | | | | | | | | | 
| | | | | | | | | | | | | |Q| 
| | | | | | | | |Q| | | | | | 
| |Q| | | | | | | | | | | | | 
| | | | | |Q| | | | | | | | | 
| | | | | | | |Q| | | | | | | 
| | | | | | | | | | |Q| | | |

```



## PL/I

This code compiles with PL/I compilers ranging from the ancient IBM MVT PL/I F compiler of the 1960s, the IBM PL/I Optimizing compiler, thru the IBM PL/I compiler for MVS and VM, to the z/OS Enterprise PL/I v4.60 compiler;spanning 50 years of PL/I compilers. It only outputs the number of solutions found for a given N instead of printing out each individual chess board solution to avoid filling up spool space for large values of N. It's trivial to add a print-out of the individual solutions.

```pli

NQUEENS: PROC OPTIONS (MAIN);                                  
 DCL A(35) BIN FIXED(31) EXTERNAL;                             
 DCL COUNT BIN FIXED(31) EXTERNAL;                             
 COUNT = 0;                                                    
 DECLARE SYSIN FILE;                                           
 DCL ABS BUILTIN;                                              
 DECLARE SYSPRINT FILE;                                        
 DECLARE N BINARY FIXED (31); /* COUNTER */                    
 /* MAIN LOOP STARTS HERE */                                   
 GET LIST (N) FILE(SYSIN); /* N QUEENS, N X N BOARD */        
 PUT SKIP (1) FILE(SYSPRINT);                                  
 PUT SKIP LIST('BEGIN N QUEENS PROCESSING *****') FILE(SYSPRINT);    
 PUT SKIP LIST('SOLUTIONS FOR N: ',N) FILE(SYSPRINT);          
 PUT SKIP (1) FILE(SYSPRINT);                                  
  IF N < 4 THEN DO;                                            
    /* LESS THAN 4 MAKES NO SENSE  */                          
      PUT SKIP (2) FILE(SYSPRINT);                             
      PUT SKIP LIST (N,' N TOO LOW') FILE (SYSPRINT);               
      PUT SKIP (2) FILE(SYSPRINT);                             
      RETURN (1);                                              
   END;                                                        
   IF N > 35 THEN DO;                                                
     /* WOULD TAKE WEEKS    */                               
       PUT SKIP (2) FILE(SYSPRINT);                                  
       PUT SKIP LIST (N,' N TOO HIGH') FILE (SYSPRINT);                   
       PUT SKIP (2) FILE(SYSPRINT);                                  
       RETURN (1);                                                   
    END;                                                             
                                                      
    CALL QUEEN(N);                                                   
                                                                     
     PUT SKIP (2) FILE(SYSPRINT);                                    
     PUT SKIP LIST (COUNT,' SOLUTIONS FOUND') FILE(SYSPRINT);             
     PUT SKIP (1) FILE(SYSPRINT);                                    
     PUT SKIP LIST ('END OF PROCESSING ****') FILE(SYSPRINT);             
     RETURN(0);                                                      
  /* MAIN LOOP ENDS ABOVE  */                                        
                                                                     
  PLACE: PROCEDURE (PS);                                             
     DCL PS BIN FIXED(31);                                           
     DCL I  BIN FIXED(31) INIT(0);                                  
     DCL A(50) BIN FIXED(31) EXTERNAL;                               
                                                                     
  DO I=1 TO PS-1;                                                    
       IF A(I) = A(PS) THEN  RETURN(0);                              
       IF ABS ( A(I) - A(PS) ) = (PS-I) THEN RETURN(0);              
  END;                                                               
    RETURN (1);                                                      
  END PLACE;                                                         

  QUEEN: PROCEDURE (N);                                          
      DCL N BIN FIXED (31);                                      
      DCL K BIN FIXED (31);                                      
      DCL A(50) BIN FIXED(31) EXTERNAL;                          
      DCL COUNT BIN FIXED(31) EXTERNAL;                          
      K = 1;                                                     
      A(K) = 0;                                                  
      DO WHILE (K > 0);                                          
         A(K) = A(K) + 1;                                        
         DO WHILE ( ( A(K)<= N) & (PLACE(K) =0) );               
             A(K) = A(K) +1;                                     
         END;                                     
         IF (A(K) <= N) THEN DO;                                 
           IF (K = N ) THEN DO;                                  
                         COUNT = COUNT + 1;                      
                 END;                                            
           ELSE DO;                                              
             K= K +1;                                            
             A(K) = 0;                                           
           END; /* OF INSIDE ELSE */                              
         END; /* OF FIRST IF */                                  
         ELSE DO;                                                
              K = K -1;                                          
         END;                                                    
      END;  /* OF EXTERNAL WHILE LOOP  */                                         
   END QUEEN;                                                    
                                                                 
 END NQUEENS;  
```



## Prolog

The code for these samples is taken from [http://www.javaist.com/blog/2008/11/06/eight-queens-problem-in-proglog/].

Solution #1:

```Prolog
solution([]).
 
solution([X/Y|Others]) :-
 solution(Others),
 member(Y, [1,2,3,4,5,6,7,8]),
 noattack(X/Y, Others).
 
noattack(_,[]).
 
noattack(X/Y,[X1/Y1|Others]) :-
 Y =\= Y1,
 Y1 - Y =\= X1 - X,
 Y1 - Y =\= X - X1,
 noattack(X/Y,Others).
 
member(Item,[Item|Rest]).
 
member(Item,[First|Rest]) :-
 member(Item,Rest).
 
template([1/Y1,2/Y2,3/Y3,4/Y4,5/Y5,6/Y6,7/Y7,8/Y8]).
```


Solution #2:

```Prolog
solution(Queens) :-
 permutation([1,2,3,4,5,6,7,8], Queens),
 safe(Queens).
 
permutation([],[]).
 
permutation([Head|Tail],PermList) :-
 permutation(Tail,PermTail),
 del(Head,PermList,PermTail).
 
del(Item,[Item|List],List).
 
del(Item,[First|List],[First|List1]) :-
 del(Item,List,List1).
 
safe([]).
 
safe([Queen|Others]) :-
 safe(Others),
 noattack(Queen,Others,1).
 
noattack(_,[],_).
 
noattack(Y,[Y1|Ylist],Xdist) :-
 Y1-Y=\=Xdist,
 Y-Y1=\=Xdist,
 Dist1 is Xdist + 1,
 noattack(Y,Ylist,Dist1).
```


Solution #3:

```Prolog
solution(Ylist) :-
 sol(Ylist,[1,2,3,4,5,6,7,8],
    [1,2,3,4,5,6,7,8],
    [-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7],
    [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]).
 
sol([],[],[],Du,Dv).
 
sol([Y|Ylist],[X|Dx1],Dy,Du,Dv) :-
 del(Y,Dy,Dy1),
 U is X-Y,
 del(U,Du,Du1),
 V is X+Y,
 del(V,Dv,Dv1),
 sol(Ylist,Dx1, Dy1,Du1,Dv1).
 
del(Item,[Item|List],List).
 
del(Item,[First|List],[First|List1]) :-
 del(Item,List,List1).
```


[http://ideone.com/Y6olN Output]:
   ?- findall(S, solution(S), LS), length(LS,N), write(N).
   92


### Alternative version

Uses non-ISO predicates between/3 and select/3 (available in SWI Prolog and GNU Prolog).

```prolog
:- initialization(main).


queens(N,Qs) :- bagof(X, between(1,N,X), Xs), place(Xs,[],Qs).

place(Xs,Qs,Res) :-
    Xs = [] -> Res = Qs
  ; select(Q,Xs,Ys), not_diag(Q,Qs,1), place(Ys,[Q|Qs],Res)
  .

not_diag(_, []     , _).
not_diag(Q, [Qh|Qs], D) :-
     abs(Q - Qh) =\= D, D1 is D + 1, not_diag(Q,Qs,D1).


main :- findall(Qs, (queens(8,Qs), write(Qs), nl), _), halt.
```

[http://ideone.com/3bbIx0 Runs in: time: 0.02 memory: 68352]


### Alternative Solution

Uses backtracking- a highly efficient mechanism in Prolog to find all solutions.
{{works with|SWI Prolog|version 6.2.6 by Jan Wielemaker, University of Amsterdam}}

```prolog
% 8 queens problem.
%  q(Row) represents a queen, allocated one per row. No rows ever clash.
%  The columns are chosen iteratively from available columns held in a
%  list, reduced with each allocation, so we need never check verticals.
%  For diagonals, we check prior to allocation whether each newly placed
%  queen will clash with any of the prior placements. This prevents
%  most invalid permutations from ever being attempted.
can_place(_, []) :- !.	   % success for empty board
can_place(q(R,C),Board) :- % check diagonals against allocated queens
	member(q(Ra,Ca), Board), abs(Ra-R) =:= abs(Ca-C), !, fail.
can_place(_,_).            % succeed if no diagonals failed

queens([], [], Board, Board).                            % found a solution
queens([q(R)|Queens], Columns, Board, Solution) :-
	nth0(_,Columns,C,Free), can_place(q(R,C),Board), % find all solutions
	queens(Queens,Free,[q(R,C)|Board], Solution).    % recursively

queens :-
  findall(q(N), between(0,7,N), Queens), findall(N, between(0,7,N), Columns),
  findall(B, queens(Queens, Columns, [], B), Boards),     % backtrack over all
  length(Boards, Len), writef('%w solutions:\n', [Len]),  % Output solutions
  member(R,Boards), reverse(R,Board), writef('  - %w\n', [Board]), fail.
queens.
```

{{out}}

```txt
?- queens.
92 solutions:
  - [q(0,0),q(1,4),q(2,7),q(3,5),q(4,2),q(5,6),q(6,1),q(7,3)]
  - [q(0,0),q(1,5),q(2,7),q(3,2),q(4,6),q(5,3),q(6,1),q(7,4)]
  - [q(0,0),q(1,6),q(2,3),q(3,5),q(4,7),q(5,1),q(6,4),q(7,2)]
  - [q(0,0),q(1,6),q(2,4),q(3,7),q(4,1),q(5,3),q(6,5),q(7,2)]
...
  - [q(0,7),q(1,1),q(2,4),q(3,2),q(4,0),q(5,6),q(6,3),q(7,5)]
  - [q(0,7),q(1,2),q(2,0),q(3,5),q(4,1),q(5,4),q(6,6),q(7,3)]
  - [q(0,7),q(1,3),q(2,0),q(3,2),q(4,5),q(5,1),q(6,6),q(7,4)]
true.
```



### Short version

SWI-Prolog 7.2.3

```Prolog
not_diagonal(X, N) :-
  maplist(plus, X, N, Z1), maplist(plus, X, Z2, N), is_set(Z1), is_set(Z2).

queens(N, Qs) :-
  numlist(1, N, P), findall(Q, (permutation(P, Q), not_diagonal(Q, P)), Qs).
```

{{out}}

```txt

?- queens(8, X), length(X, L).
X = [[1, 5, 8, 6, 3, 7, 2, 4], [1, 6, 8, 3, 7, 4, 2|...], [1, 7, 4, 6, 8, 2|...], [1, 7, 5, 8, 2|...], [2, 4, 6, 8|...], [2, 5, 7|...], [2, 5|...], [2|...], [...|...]|...],
L = 92.

```



## Pure

From the Pure (programming language) Wikipedia page


```pure
/*
 n-queens.pure
 Tectonics:
     pure -c queens.pure -o queens
  or 
     pure -q -i queens.pure
*/
using system;

queens n = search n 1 [] with
   search n i p  = [reverse p] if i>n;
                 = cat [search n (i+1) ((i,j):p) | j = 1..n; safe (i,j) p];
   safe (i,j) p  = ~any (check (i,j)) p;
   check (i1,j1) (i2,j2)
                 = i1==i2 || j1==j2 || i1+j1==i2+j2 || i1-j1==i2-j2;
end;

compiling || (puts("queens 4: " + str(queens 4)) $$ 
              puts("Solutions to queens 7: " + str(#queens 7)));
```


{{out}}

```txt
prompt$ pure -c queens.pure -o queens
prompt$ time -p ./queens
queens 4: [[(1,2),(2,4),(3,1),(4,3)],[(1,3),(2,1),(3,4),(4,2)]]
Solutions to queens 7: 40
real 0.03
user 0.02
sys 0.00

prompt$ pure -i -q queens.pure
> #queens 10;
724
> 
```



## PureBasic

A recursive approach is taken.  A queen is placed in an unused column for each new row.  An array keeps track if a queen has already been placed in a given column so that no duplicate columns result.  That handles the Rook attacks.  Bishop attacks are handled by checking the diagonal alignments of each new placement against the previously placed queens and if an attack is possible the solution backtracks.  The solutions are kept track of in a global variable and the routine <tt>queens(n)</tt> is called with the required number of queens specified.

```PureBasic
Global solutions

Procedure showBoard(Array queenCol(1))
  Protected row, column, n = ArraySize(queenCol())

  PrintN(" Solution " + Str(solutions))
  For row = 0 To n
    For column = 0 To n
      If queenCol(row) = column
        Print("|Q")
      Else
        Print("| ")
      EndIf
    Next
    PrintN("|")
  Next
EndProcedure

Macro advanceIfPossible()
  x + 1
  While x <= n And columns(x): x + 1: Wend
  If x > n
    ProcedureReturn #False ;backtrack
  EndIf 
EndMacro

Procedure placeQueens(Array queenCol(1), Array columns(1), row = 0)
  Protected n = ArraySize(queenCol())
  
  If row > n
    solutions + 1
    showBoard(queenCol())
    ProcedureReturn #False ;backtrack
  EndIf
  
  Protected x, queen, passed
  While columns(x): x + 1: Wend
    
  ;place a new queen in one of the available columns
  Repeat 
    passed = #True
    For queen = 0 To row - 1
      If ((queenCol(queen) - x) = (queen - row)) Or ((queenCol(queen) - x) = -(queen - row))
        advanceIfPossible()
        passed = #False
        Break ;ForNext loop
      EndIf
    Next
    
    If passed
      queenCol(row) = x: columns(x) = 1
      If Not placeQueens(queenCol(), columns(), row + 1)
        columns(x) = 0
        advanceIfPossible()
      EndIf 
    EndIf 
  ForEver
EndProcedure

Procedure queens(n)
  If n > 0
    Dim queenCol(n - 1)
    Dim columns(n - 1)
    placeQueens(queenCol(), columns()) 
  EndIf 
EndProcedure

If OpenConsole()
  Define i
  For i = 1 To 12
    solutions = 0
    queens(i)
    PrintN(#CRLF$ + Str(solutions) + " solutions found for " + Str(i) + "-queens.")
    Input()
  Next 
  
  Print(#CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

Sample output showing the last solution (all are actually displayed) for 1 - 12 queens:
<pre style="height:40ex;overflow:scroll"> Solution 1
|Q|

1 solutions found for 1-queens. {Press ENTER}

0 solutions found for 2-queens. {Press ENTER}

0 solutions found for 3-queens. {Press ENTER}

 Solution 2
| | |Q| |
|Q| | | |
| | | |Q|
| |Q| | |

2 solutions found for 4-queens. {Press ENTER}

 Solution 10
| | | | |Q|
| | |Q| | |
|Q| | | | |
| | | |Q| |
| |Q| | | |

10 solutions found for 5-queens. {Press ENTER}

 Solution 4
| | | | |Q| |
| | |Q| | | |
|Q| | | | | |
| | | | | |Q|
| | | |Q| | |
| |Q| | | | |

4 solutions found for 6-queens. {Press ENTER}

 Solution 40
| | | | | | |Q|
| | | | |Q| | |
| | |Q| | | | |
|Q| | | | | | |
| | | | | |Q| |
| | | |Q| | | |
| |Q| | | | | |

40 solutions found for 7-queens. {Press ENTER}

 Solution 92
| | | | | | | |Q|
| | | |Q| | | | |
|Q| | | | | | | |
| | |Q| | | | | |
| | | | | |Q| | |
| |Q| | | | | | |
| | | | | | |Q| |
| | | | |Q| | | |

92 solutions found for 8-queens. {Press ENTER}

 Solution 352
| | | | | | | | |Q|
| | | | | | |Q| | |
| | | |Q| | | | | |
| |Q| | | | | | | |
| | | | | | | |Q| |
| | | | | |Q| | | |
|Q| | | | | | | | |
| | |Q| | | | | | |
| | | | |Q| | | | |

352 solutions found for 9-queens. {Press ENTER}

 Solution 724
| | | | | | | | | |Q|
| | | | | | | |Q| | |
| | | | |Q| | | | | |
| | |Q| | | | | | | |
|Q| | | | | | | | | |
| | | | | |Q| | | | |
| |Q| | | | | | | | |
| | | | | | | | |Q| |
| | | | | | |Q| | | |
| | | |Q| | | | | | |

724 solutions found for 10-queens. {Press ENTER}

 Solution 2680
| | | | | | | | | | |Q|
| | | | | | | | |Q| | |
| | | | | | |Q| | | | |
| | | | |Q| | | | | | |
| | |Q| | | | | | | | |
|Q| | | | | | | | | | |
| | | | | | | | | |Q| |
| | | | | | | |Q| | | |
| | | | | |Q| | | | | |
| | | |Q| | | | | | | |
| |Q| | | | | | | | | |

2680 solutions found for 11-queens. {Press ENTER}

 Solution 14200
| | | | | | | | | | | |Q|
| | | | | | | | | |Q| | |
| | | | | | | |Q| | | | |
| | | | |Q| | | | | | | |
| | |Q| | | | | | | | | |
|Q| | | | | | | | | | | |
| | | | | | |Q| | | | | |
| |Q| | | | | | | | | | |
| | | | | | | | | | |Q| |
| | | | | |Q| | | | | | |
| | | |Q| | | | | | | | |
| | | | | | | | |Q| | | |

14200 solutions found for 12-queens. {Press ENTER}
```



## Python


### Python: Raymond Hettingers permutations based solution

This solution, originally by [http://code.activestate.com/recipes/576647/ Raymond Hettinger] for demonstrating the power of the itertools module, generates all solutions.


```python
from itertools import permutations

n = 8
cols = range(n)
for vec in permutations(cols):
    if n == len(set(vec[i]+i for i in cols)) \
         == len(set(vec[i]-i for i in cols)):
        print ( vec )
```


The output is presented in vector form (each number represents the column position of a queen on consecutive rows). 
The vector can be pretty printed by substituting a call to <code>board</code> instead of <code>print</code>, with the same argument, and where board is pre-defined as:

```python
def board(vec):
    print ("\n".join('.' * i + 'Q' + '.' * (n-i-1) for i in vec) + "\n===\n")
```


Raymond's description is:
:With the solution represented as a vector with one queen in each row, we don't have to check to see if two queens are on the same row. By using a permutation generator, we know that no value in the vector is repeated, so we don't have to check to see if two queens are on the same column. Since rook moves don't need to be checked, we only need to check bishop moves.

:The technique for checking the diagonals is to add or subtract the column number from each entry, so any two entries on the same diagonal will have the same value (in other words, the sum or difference is unique for each diagonal). Now all we have to do is make sure that the diagonals for each of the eight queens are distinct. So, we put them in a set (which eliminates duplicates) and check that the set length is eight (no duplicates were removed).

:Any permutation with non-overlapping diagonals is a solution. So, we print it and continue checking other permutations.

One disadvantage with this solution is that we can't simply "skip" all the permutations that start with a certain prefix, after discovering that that prefix is incompatible. 
For example, it is easy to verify that no permutation of the form (1,2,...) could ever be a solution, but since we don't have control over the generation of the permutations, we can't just tell it to "skip" all the ones that start with (1,2).


### Python: Alternative Solution

{{works with|Python|2.6, 3.x}}

```python
# From: http://wiki.python.org/moin/SimplePrograms, with permission from the author, Steve Howell
BOARD_SIZE = 8

def under_attack(col, queens):
    return col in queens or \
           any(abs(col - x) == len(queens)-i for i,x in enumerate(queens))

def solve(n):
    solutions = [[]]
    for row in range(n):
        solutions = [solution+[i+1]
                       for solution in solutions
                       for i in range(BOARD_SIZE)
                       if not under_attack(i+1, solution)]
    return solutions

for answer in solve(BOARD_SIZE): print(list(enumerate(answer, start=1)))
```



### Python: Simple Backtracking Solution

A surprisingly simple change to the above code (changing the list comprehension 
to a generator expression) produces a backtracking solution:
{{works with|Python|2.6, 3.x}}

```python
BOARD_SIZE = 8

def under_attack(col, queens):
    return col in queens or \
           any(abs(col - x) == len(queens)-i for i,x in enumerate(queens))

def solve(n):
    solutions = [[]]
    for row in range(n):
        solutions = (solution+[i+1]
                       for solution in solutions # first for clause is evaluated immediately,
                                                 # so "solutions" is correctly captured
                       for i in range(BOARD_SIZE)
                       if not under_attack(i+1, solution))
    return solutions

answers = solve(BOARD_SIZE)
first_answer = next(answers)
print(list(enumerate(first_answer, start=1)))
```


===Python: Simple Backtracking Solution (functional style)===
This simple version, which uses a generator function and lists, has an excellent performance with PyPy.

```python
def solve(n, i, a, b, c):
    if i < n:
        for j in range(n):
            if j not in a and i+j not in b and i-j not in c:
                for solution in solve(n, i+1, a+[j], b+[i+j], c+[i-j]):
                    yield solution
    else:
        yield a

for solution in solve(8, 0, [], [], []):
    print(solution)
```



### Python: backtracking on permutations

Queens positions on a n x n board are encoded as permutations of [0, 1, ..., n]. The algorithms consists in building a permutation from left to right, by swapping elements of the initial [0, 1, ..., n], recursively calling itself unless the current position is not possible. The test is done by checking only diagonals, since rows/columns have by definition of a permutation, only one queen.

This is initially a translation of the Fortran 77 solution.

The solutions are returned as a generator, using the "yield from" functionality of Python 3.3, described in [https://www.python.org/dev/peps/pep-0380/ PEP-380].


```python
def queens(n):
    a = list(range(n))
    up = [True]*(2*n - 1)
    down = [True]*(2*n - 1)
    def sub(i):
        if i == n:
            yield tuple(a)
        else:
            for k in range(i, n):
                j = a[k]
                p = i + j
                q = i - j + n - 1
                if up[p] and down[q]:
                    up[p] = down[q] = False
                    a[i], a[k] = a[k], a[i]
                    yield from sub(i + 1)
                    up[p] = down[q] = True
                    a[i], a[k] = a[k], a[i]
    yield from sub(0)

#Count solutions for n=8:
sum(1 for p in queens(8))
92
```


The preceding function does not enumerate solutions in lexicographic order, see [[Permutations#Recursive implementation]] for an explanation. The following does, but is almost 50% slower, because the exchange is always made (otherwise the loop to shift the array a by one place would not work).

However, it may be interesting to look at the first solution in lexicographic order: for growing n, and apart from a +1 offset, it gets closer and closer to the sequence [http://oeis.org/A065188 A065188] at OEIS. The first n for which the first solutions differ is n=26.


```python
def queens_lex(n):
    a = list(range(n))
    up = [True]*(2*n - 1)
    down = [True]*(2*n - 1)
    def sub(i):
        if i == n:
            yield tuple(a)
        else:
            for k in range(i, n):
                a[i], a[k] = a[k], a[i]
                j = a[i]
                p = i + j
                q = i - j + n - 1
                if up[p] and down[q]:
                    up[p] = down[q] = False
                    yield from sub(i + 1)
                    up[p] = down[q] = True
            x = a[i]
            for k in range(i + 1, n):
                a[k - 1] = a[k]
            a[n - 1] = x
    yield from sub(0)

next(queens(31))
(0, 2, 4, 1, 3, 8, 10, 12, 14, 6, 17, 21, 26, 28, 25, 27, 24, 30, 7, 5, 29, 15, 13, 11, 9, 18, 22, 19, 23, 16, 20)

next(queens_lex(31))
(0, 2, 4, 1, 3, 8, 10, 12, 14, 5, 17, 22, 25, 27, 30, 24, 26, 29, 6, 16, 28, 13, 9, 7, 19, 11, 15, 18, 21, 23, 20)

#Compare to A065188
#1, 3, 5, 2, 4, 9, 11, 13, 15, 6, 8, 19, 7, 22, 10, 25, 27, 29, 31, 12, 14, 35, 37, ...
```



### Python: fold/reduce

Expressed in terms of nested folds, allowing for graphic display of results, and listing the number of solutions found for boards of various sizes:
{{Works with|Python|3.7}}

```Python
'''N Queens problem'''

from functools import reduce
from itertools import chain


# queenPuzzle :: Int -> Int -> [[Int]]
def queenPuzzle(nRows, nCols):
    '''All board patterns of this dimension
       in which no two Queens share a row,
       column, or diagonal.
    '''
    def go(nRows):
        lessRows = nRows - 1
        return reduce(
            lambda a, xys: a + reduce(
                lambda b, iCol: b + [xys + [iCol]] if (
                    safe(lessRows, iCol, xys)
                ) else b,
                enumFromTo(1)(nCols),
                []
            ),
            go(lessRows),
            []
        ) if nRows > 0 else [[]]
    return go(nRows)


# safe :: Int -> Int -> [Int] -> Bool
def safe(iRow, iCol, pattern):
    '''True if no two queens in the pattern
       share a row, column or diagonal.
    '''
    def p(sc, sr):
        return (iCol == sc) or (
            sc + sr == (iCol + iRow)
        ) or (sc - sr == (iCol - iRow))
    return not any(map(p, pattern, range(0, iRow)))


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Number of solutions for boards of various sizes'''

    n = 5
    xs = queenPuzzle(n, n)

    print(
        str(len(xs)) + ' solutions for a {n} * {n} board:\n'.format(n=n)
    )
    print(showBoards(10)(xs))

    print(
        fTable(
            '\n\n' + main.__doc__ + ':\n'
        )(str)(lambda n: str(n).rjust(3, ' '))(
            lambda n: len(queenPuzzle(n, n))
        )(enumFromTo(1)(10))
    )


# GENERIC -------------------------------------------------

# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# chunksOf :: Int -> [a] -> [[a]]
def chunksOf(n):
    '''A series of lists of length n, subdividing the
       contents of xs. Where the length of xs is not evenly
       divible, the final list will be shorter than n.
    '''
    return lambda xs: reduce(
        lambda a, i: a + [xs[i:n + i]],
        range(0, len(xs), n), []
    ) if 0 < n else []


# intercalate :: [a] -> [[a]] -> [a]
# intercalate :: String -> [String] -> String
def intercalate(x):
    '''The concatenation of xs
       interspersed with copies of x.
    '''
    return lambda xs: x.join(xs) if isinstance(x, str) else list(
        chain.from_iterable(
            reduce(lambda a, v: a + [x, v], xs[1:], [xs[0]])
        )
    ) if xs else []


# FORMATTING ----------------------------------------------

# showBoards :: Int -> [[Int]] -> String
def showBoards(nCols):
    '''String representation, with N columns
       of a set of board patterns.
    '''
    def showBlock(b):
        return '\n'.join(map(intercalate('  '), zip(*b)))

    def go(bs):
        return '\n\n'.join(map(
            showBlock,
            chunksOf(nCols)(
                list(map(showBoard, bs))
            )
        ))
    return lambda boards: go(boards)


# showBoard :: [Int] -> String
def showBoard(xs):
    '''String representation of a Queens board.'''
    lng = len(xs)

    def showLine(n):
        return ('.' * (n - 1)) + '♛' + ('.' * (lng - n))
    return list(map(showLine, xs))


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

{{Out}}

```txt
10 solutions for a 5 * 5 board:

♛....  ♛....  .♛...  .♛...  ..♛..  ..♛..  ...♛.  ...♛.  ....♛  ....♛
..♛..  ...♛.  ...♛.  ....♛  ♛....  ....♛  ♛....  .♛...  .♛...  ..♛..
....♛  .♛...  ♛....  ..♛..  ...♛.  .♛...  ..♛..  ....♛  ...♛.  ♛....
.♛...  ....♛  ..♛..  ♛....  .♛...  ...♛.  ....♛  ..♛..  ♛....  ...♛.
...♛.  ..♛..  ....♛  ...♛.  ....♛  ♛....  .♛...  ♛....  ..♛..  .♛...

Number of solutions for boards of various sizes:

 1 ->   1
 2 ->   0
 3 ->   0
 4 ->   2
 5 ->  10
 6 ->   4
 7 ->  40
 8 ->  92
 9 -> 352
10 -> 724
```



## QB64


```QB64

DIM SHARED QUEENS AS INTEGER
PRINT "# of queens:";: INPUT QUEENS
IF QUEENS = 0 THEN END
OPEN LTRIM$(STR$(QUEENS)) + "queens.dat" FOR OUTPUT AS #1
PRINT "Queens: Calculates"; QUEENS; " queens problem."
DIM SHARED arrayqcol(QUEENS) AS LONG ' columns of queens
DIM SHARED nsolutions AS LONG
CALL dorow(1) ' start with row 1
LOCATE 22, 1
PRINT STR$(nsolutions) + " solutions"
END
SUB dorow (irow) ' starts with row irow
    FOR icol = 1 TO QUEENS
        FOR iqueen = 1 TO irow - 1 ' check for conflict with previous queens
            IF arrayqcol(iqueen) = icol THEN GOTO continue1 ' same column?
            ' iqueen is also row of queen
            IF iqueen + arrayqcol(iqueen) = irow + icol THEN GOTO continue1 ' right diagonal?
            IF iqueen - arrayqcol(iqueen) = irow - icol THEN GOTO continue1 ' left diagonal?
        NEXT iqueen
        ' at this point we can add a queen
        arrayqcol(irow) = icol ' add to array
        LOCATE irow + 2, icol: PRINT "x"; ' show progress
        _DELAY (.001) ' slows processing
        IF irow = QUEENS THEN ' solution?
            nsolutions = nsolutions + 1
            PRINT #1, "Solution #" + MID$(STR$(nsolutions), 2) + "."
            FOR i1 = 1 TO QUEENS ' rows
                s1$ = STRING$(QUEENS, ".") ' columns
                MID$(s1$, arrayqcol(i1), 1) = "x" ' x in queen column
                PRINT #1, s1$
            NEXT i1
            PRINT #1, ""
        ELSE
            CALL dorow(irow + 1) ' recursive call to next row
        END IF
        LOCATE irow + 2, icol: PRINT "."; ' remove queen
        continue1:
    NEXT icol
END SUB

```



## R


```r
# Brute force, see the "Permutations" page for the next.perm function
safe <- function(p) {
  n <- length(p)
  for (i in seq(1, n - 1)) {
    for (j in seq(i + 1, n)) {
      if (abs(p[j] - p[i]) == abs(j - i)) return(F)
    }
  }
  return(T)
}

queens <- function(n) {
  p <- 1:n
  k <- 0
  while (!is.null(p)) {
    if(safe(p)) {
      cat(p, "\n")
      k <- k + 1
    }
    p <- next.perm(p)
  }
  return(k)
}

queens(8)
# 1 5 8 6 3 7 2 4 
# ...
# 92
```



## Racket


Backtracking algorithm; returns one solution


```racket

#lang racket

(struct Q (x y) #:transparent)

;; returns true if given q1 and q2 do not conflict
(define (safe? q1 q2)
  (match* (q1 q2)
    [((Q x1 y1) (Q x2 y2))
     (not (or (= x1 x2) (= y1 y2)
              (= (abs (- x1 x2)) (abs (- y1 y2)))))]))

;; returns true if given q doesn't conflict with anything in given list of qs
(define (safe-lst? q qs) (for/and ([q2 qs]) (safe? q q2)))

(define (nqueens n)
  ;; qs is partial solution; x y is current position to try
  (let loop ([qs null] [x 0] [y 0])
    (cond [(= (length qs) n) qs]          ; found a solution
          [(>= x n) (loop qs 0 (add1 y))] ; go to next row
          [(>= y n) #f]                   ; current solution is invalid
          [else
           (define q (Q x y))
           (if (safe-lst? q qs) ; is current position safe?
               (or (loop (cons q qs) 0 (add1 y)) ; optimistically place a queen
                                                 ; (and move pos to next row)
                   (loop qs (add1 x) y))  ; backtrack if it fails
               (loop qs (add1 x) y))])))

(nqueens 8)
; => (list (Q 3 7) (Q 1 6) (Q 6 5) (Q 2 4) (Q 5 3) (Q 7 2) (Q 4 1) (Q 0 0))

```


Show result with "How to Design Programs" GUI.

```racket

(require htdp/show-queen)

(define (show-nqueens n)
  (define qs (time (nqueens n)))
  (show-queen
   (for/list ([row n])
     (for/list ([col n])
       (if (member (Q row col) qs) #t #f)))))

(show-nqueens 8)

```


[[image:Racket-nqueens.png]]

When hovering mouse, GUI also displays conflicts for potential additional queens.

[[image:Racket-nqueens-conflict.png]]


Lazy-style solution, ie, generate all solutions, then filter out invalid ones.
Computes all solutions.


```racket

#lang racket

(struct Q (x y) #:transparent)

(define-syntax-rule (lcons x y) (cons x (lazy y)))

(define (lazy-filter p? lst)
  (define flst (force lst))
  (if (null? flst) '()
      (let ([x (car flst)])
        (if (p? x)
            (lcons x (lazy-filter p? (cdr flst)))
            (lazy-filter p? (cdr flst))))))

(define (lazy-foldr f base lst)
  (define flst (force lst))
  (if (null? flst) base
      (f (car flst) (lazy (lazy-foldr f base (cdr flst))))))

(define (tails lst)
  (if (null? lst) '(())
      (cons lst (tails (cdr lst)))))

(define (safe? q1 q2)
  (match* (q1 q2)
    [((Q x1 y1) (Q x2 y2))
     (not (or (= x1 x2) (= y1 y2)
              (= (abs (- x1 x2)) (abs (- y1 y2)))))]))

(define (safe-lst? lst)
  (or (null? lst)
      (let ([q1 (car lst)])
        (for/and ([q2 (cdr lst)]) (safe? q1 q2)))))

(define (valid? lst) (andmap safe-lst? (tails lst)))

(define (nqueens n)
  (define all-possible-solutions
    (for/fold ([qss-so-far '(())]) ([row (in-range n)])
      (lazy-foldr
       (λ (qs new-qss)
         (append (for/list ([col (in-range n)]) (cons (Q row col) qs))
                 new-qss))
       '() qss-so-far)))
  (lazy-filter valid? all-possible-solutions))

```


Taking the first solution does not compute the other solutions:


```racket

(car (nqueens 8))
;; => (list (Q 7 3) (Q 6 1) (Q 5 6) (Q 4 2) (Q 3 5) (Q 2 7) (Q 1 4) (Q 0 0))

```


Computing all solutions is also possible:


```racket

(define (force-and-print qs)
  (define forced (force qs))
  (unless (null? forced)
    (printf "~v\n" (car forced))
    (force-and-print (cdr forced))))

(force-and-print (nqueens 8))

; =>
;(list (Q 7 3) (Q 6 1) (Q 5 6) (Q 4 2) (Q 3 5) (Q 2 7) (Q 1 4) (Q 0 0))
;(list (Q 7 4) (Q 6 1) (Q 5 3) (Q 4 6) (Q 3 2) (Q 2 7) (Q 1 5) (Q 0 0))
;(list (Q 7 2) (Q 6 4) (Q 5 1) (Q 4 7) (Q 3 5) (Q 2 3) (Q 1 6) (Q 0 0))
;(list (Q 7 2) (Q 6 5) (Q 5 3) (Q 4 1) (Q 3 7) (Q 2 4) (Q 1 6) (Q 0 0))
...
;(list (Q 7 5) (Q 6 3) (Q 5 6) (Q 4 0) (Q 3 2) (Q 2 4) (Q 1 1) (Q 0 7))
;(list (Q 7 3) (Q 6 6) (Q 5 4) (Q 4 1) (Q 3 5) (Q 2 0) (Q 1 2) (Q 0 7))
;(list (Q 7 4) (Q 6 6) (Q 5 1) (Q 4 5) (Q 3 2) (Q 2 0) (Q 1 3) (Q 0 7))

```


Logic borrowed from the Ruby example

```racket

#lang racket
(define (remove x lst)
  (for/list ([i (in-range (length lst))]
             #:when (not (= x i)))
    (list-ref lst i)))

(define (switch-pairs lst)
  (cond [(null? lst) '()]
        [(null? (cdr lst)) (list '() (car lst))]
        [else (append (list (cadr lst) (car lst))
                      (switch-pairs (cddr lst)))]))

(define (switch-places a1 a2 lst)
  (for/list ([i (length lst)])
    (list-ref lst (cond [(= a1 i) a2] [(= a2 i) a1] [else i]))))

(define (position-queens n)
  (cond [(= 1 n) (list (list 1))]
        [(> 4 n) #f]
        [else (possible-queens n)]))

(define (possible-queens n)
  (define rem (remainder n 12))
  (define lst (build-list n add1))
  (define evens (filter even? lst))
  (define odds (filter odd? lst))
  (cond [(or (= rem 9) (= rem 3)) (case3or9 evens odds)]
        [(= rem 8) (case8 evens odds)]
        [(= rem 2) (case2 evens odds)]
        [else (append evens odds)]))

(define (case3or9 evens odds)
  (for/fold ([acum (append (cdr evens) (list (car evens)) odds)])
            ([i (in-list '(1 3))])
    (append (remove (list-ref acum i) acum) (list i))))

(define (case8 evens odds)
  (append evens (switch-pairs odds)))

(define (case2 evens odds)
  (define nums (append evens odds))
  (define idx (map (λ(i) (list-ref nums i)) '(1 3 5)))
  (append (remove (caddr idx)
                  (switch-places (car idx) (cadr idx) nums))
          '(5)))

(define (queens n)
  (define position-numbers (position-queens n))
  (define positions-on-board
    (for/list ([i n]) (cons i (sub1 (list-ref position-numbers i)))))
  (for/list ([x n])
    (for/list ([y n])
      (if (member (cons x y) positions-on-board) "Q" "."))))

(define (print-queens n)
  (for ([x (queens n)]) (displayln (string-join x))))

```



## Rascal



```Rascal
import Prelude;

public set[list[int]] Nqueens(int n){
	cols = upTill(n);
	result = {};
	for (vector <- permutations(cols)){
		if (n == size({vector[j] + j |j <- cols}) && n == size({vector[j] - j |j <- cols})) 
			result += vector;}
	return result;
}
```



## REXX

The logic was borrowed from the Fortran example and modified for speed;   the display of the chessboard was 

also changed to allow for the aspect ratio of display terminals to make the chessboard appear square. 

Logic was added to the REXX program to preserve the color for a black square when a queen occupies it.

About half of the REXX code involves presentation (and colorization achieved through dithering) of the chessboard and queens.

```rexx
/*REXX program  places   N   queens on an  NxN  chessboard  (the eight queens problem). */
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N= 8                     /*Not specified:  Then use the default.*/
if N<1  then call nOK                            /*display a message, the board is bad. */
rank= 1;    file= 1;       #=0                   /*starting rank&file;  #≡number queens.*/
@.= 0;      pad= left('', 9* (N<18) )            /*define empty board;  set indentation.*/
                                                 /* [↓]  rank&file ≡ chessboard row&cols*/
  do  while #<N;     @.file.rank= 1              /*keep placing queens until we're done.*/
  if ok(file, rank)  then do; file= 1;  #= # + 1 /*Queen not being attacked? Then eureka*/
                              rank= rank + 1     /*use another attempt at another rank. */
                              iterate            /*go and try another queen placement.  */
                          end                    /* [↑]  found a good queen placement.  */
  @.file.rank= 0                                 /*It isn't safe.  So remove this queen.*/
  file= file+1                                   /*So, try the next (higher) chess file.*/
               do  while file>N;    rank= rank - 1;            if rank==0  then call nOK
                  do j=1  for N;    if \@.j.rank  then iterate               /*¿ocupado?*/
                  @.j.rank= 0;      #= # - 1;          file= j + 1;        leave
                  end  /*j*/
               end     /*while file>N*/
  end                  /*while    #<N*/
call show
exit  1                                          /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
nOK: say;      say  "No solution for"      N      'queens.';          say;          exit 0
/*──────────────────────────────────────────────────────────────────────────────────────*/
ok:  parse arg f,r;  fp= f + 1;   rm= r - 1      /*if return≡0,  then queen isn't safe. */
              do k=1          for rm;               if @.f.k  then return 0;           end
     f= f-1;  do k=rm  by -1  for rm  while f\==0;  if @.f.k  then return 0;  f= f-1;  end
     f= fp;   do k=rm  by -1  for rm  while f <=N;  if @.f.k  then return 0;  f= f+1;  end
     return 1   /*1≡queen is safe. */            /*  ↑↑↑↑↑↑↑↑    is queen under attack? */
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: say  'A solution for '     N     " queens:"     /*display a title to the terminal.*/
      g= substr( copies("╬═══",  N)  ,2)              /*start of all cells on chessboard*/
      say;      say pad  translate('╔'g"╗", '╦', "╬") /*display top rank (of the board).*/
      line   = '╠'g"╣";   dither= "▓";   ditherQ= '░' /*define a line for cell boundary.*/
      bar    = '║'    ;   queen = "Q"                 /*kinds:   horiz.,  vert.,  salad.*/
      Bqueen = ditherQ || queen || ditherQ            /*glyph befitting a black square Q*/
      Wqueen =         ' 'queen" "                    /*  "       "     " white    "   "*/
        do   rank=1  for N;     if rank\==1  then say pad line;    _=  /*show rank sep. */
          do file=1  for N;         B = (file + rank)  //  2           /*square black ? */
          Qgylph= Wqueen;       if  B  then Qgylph= Bqueen             /*use dithered Q.*/
          if @.file.rank then _= _ || bar || Qgylph                    /*3─char Q symbol*/
                         else if B then _=_ || bar || copies(dither,3) /*dithering      */
                                   else _=_ || bar || copies(  ' ' ,3) /* 3 blanks      */
          end   /*file*/                              /* [↑]  preserve square─ish board.*/
        say pad  _ || bar                             /*show a single rank of the board.*/
        end     /*rank*/                              /*80 cols  can view a 19x19 board.*/
      say pad  translate('╚'g"╝", '╩', "╬");   return /*display the last rank (of board)*/
```

{{out|output|text=  when using the default of an   '''8'''<small>x</small>'''8'''   chessboard:}}

```txt

A solution for  8  queens:

          ╔═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╗
          ║ Q ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║
          ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
          ║▓▓▓║   ║▓▓▓║   ║░Q░║   ║▓▓▓║   ║
          ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
          ║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║░Q░║
          ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
          ║▓▓▓║   ║▓▓▓║   ║▓▓▓║ Q ║▓▓▓║   ║
          ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
          ║   ║▓▓▓║ Q ║▓▓▓║   ║▓▓▓║   ║▓▓▓║
          ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
          ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║░Q░║   ║
          ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
          ║   ║░Q░║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║
          ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
          ║▓▓▓║   ║▓▓▓║ Q ║▓▓▓║   ║▓▓▓║   ║
          ╚═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╝

```

{{out|output|text=  when using   '''20'''<small>x</small>'''20'''   chessboard with the input of:     <tt> 20 </tt>}}

```txt

A solution for  20  queens:

 ╔═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╗
 ║ Q ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║▓▓▓║   ║░Q░║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║   ║▓▓▓║   ║▓▓▓║ Q ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║▓▓▓║ Q ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║   ║▓▓▓║   ║░Q░║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║░Q░║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║ Q ║▓▓▓║   ║▓▓▓║   ║▓▓▓║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║ Q ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║░Q░║   ║▓▓▓║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║ Q ║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║ Q ║▓▓▓║   ║▓▓▓║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║░Q░║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║░Q░║   ║▓▓▓║   ║▓▓▓║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║░Q░║   ║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║░Q░║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║ Q ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║ Q ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║ Q ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║   ║▓▓▓║   ║▓▓▓║   ║░Q░║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║
 ╠═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╬═══╣
 ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║░Q░║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║▓▓▓║   ║
 ╚═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╝

```



## Ruby

This implements the heuristics found on the wikipedia page to return just one solution

```ruby
# 1. Divide n by 12. Remember the remainder (n is 8 for the eight queens
#    puzzle).
# 2. Write a list of the even numbers from 2 to n in order.
# 3. If the remainder is 3 or 9, move 2 to the end of the list.
# 4. Append the odd numbers from 1 to n in order, but, if the remainder is 8,
#    switch pairs (i.e. 3, 1, 7, 5, 11, 9, …).
# 5. If the remainder is 2, switch the places of 1 and 3, then move 5 to the
#    end of the list.
# 6. If the remainder is 3 or 9, move 1 and 3 to the end of the list.
# 7. Place the first-column queen in the row with the first number in the
#    list, place the second-column queen in the row with the second number in
#    the list, etc.


def n_queens(n)
  if n == 1
    return "Q"
  elsif n < 4
    puts "no solutions for n=#{n}"
    return ""
  end
 
  evens = (2..n).step(2).to_a
  odds = (1..n).step(2).to_a
 
  rem = n % 12  # (1)
  nums = evens  # (2)
 
  nums.rotate if rem == 3 or rem == 9  # (3)
 
  # (4)
  if rem == 8
    odds = odds.each_slice(2).flat_map(&:reverse)
  end
  nums.concat(odds)
 
  # (5)
  if rem == 2
    nums[nums.index(1)], nums[nums.index(3)] = nums[nums.index(3)], nums[nums.index(1)]
    nums << nums.delete(5)
  end
 
  # (6)
  if rem == 3 or rem == 9
    nums << nums.delete(1)
    nums << nums.delete(3)
  end
 
  # (7)
  nums.map do |q|
    a = Array.new(n,".")
    a[q-1] = "Q"
    a*(" ")
  end
end
 
(1 .. 15).each {|n| puts "n=#{n}"; puts n_queens(n); puts}
```


{{out}}
<pre style="height:40ex;overflow:scroll">n=1
Q

n=2
no solutions for n=2


n=3
no solutions for n=3


n=4
. Q . .
. . . Q
Q . . .
. . Q .

n=5
. Q . . .
. . . Q .
Q . . . .
. . Q . .
. . . . Q

n=6
. Q . . . .
. . . Q . .
. . . . . Q
Q . . . . .
. . Q . . .
. . . . Q .

n=7
. Q . . . . .
. . . Q . . .
. . . . . Q .
Q . . . . . .
. . Q . . . .
. . . . Q . .
. . . . . . Q

n=8
. Q . . . . . .
. . . Q . . . .
. . . . . Q . .
. . . . . . . Q
. . Q . . . . .
Q . . . . . . .
. . . . . . Q .
. . . . Q . . .

n=9
. . . Q . . . . .
. . . . . Q . . .
. . . . . . . Q .
. Q . . . . . . .
. . . . Q . . . .
. . . . . . Q . .
. . . . . . . . Q
Q . . . . . . . .
. . Q . . . . . .

n=10
. Q . . . . . . . .
. . . Q . . . . . .
. . . . . Q . . . .
. . . . . . . Q . .
. . . . . . . . . Q
Q . . . . . . . . .
. . Q . . . . . . .
. . . . Q . . . . .
. . . . . . Q . . .
. . . . . . . . Q .

n=11
. Q . . . . . . . . .
. . . Q . . . . . . .
. . . . . Q . . . . .
. . . . . . . Q . . .
. . . . . . . . . Q .
Q . . . . . . . . . .
. . Q . . . . . . . .
. . . . Q . . . . . .
. . . . . . Q . . . .
. . . . . . . . Q . .
. . . . . . . . . . Q

n=12
. Q . . . . . . . . . .
. . . Q . . . . . . . .
. . . . . Q . . . . . .
. . . . . . . Q . . . .
. . . . . . . . . Q . .
. . . . . . . . . . . Q
Q . . . . . . . . . . .
. . Q . . . . . . . . .
. . . . Q . . . . . . .
. . . . . . Q . . . . .
. . . . . . . . Q . . .
. . . . . . . . . . Q .

n=13
. Q . . . . . . . . . . .
. . . Q . . . . . . . . .
. . . . . Q . . . . . . .
. . . . . . . Q . . . . .
. . . . . . . . . Q . . .
. . . . . . . . . . . Q .
Q . . . . . . . . . . . .
. . Q . . . . . . . . . .
. . . . Q . . . . . . . .
. . . . . . Q . . . . . .
. . . . . . . . Q . . . .
. . . . . . . . . . Q . .
. . . . . . . . . . . . Q

n=14
. Q . . . . . . . . . . . .
. . . Q . . . . . . . . . .
. . . . . Q . . . . . . . .
. . . . . . . Q . . . . . .
. . . . . . . . . Q . . . .
. . . . . . . . . . . Q . .
. . . . . . . . . . . . . Q
. . Q . . . . . . . . . . .
Q . . . . . . . . . . . . .
. . . . . . Q . . . . . . .
. . . . . . . . Q . . . . .
. . . . . . . . . . Q . . .
. . . . . . . . . . . . Q .
. . . . Q . . . . . . . . .

n=15
. . . Q . . . . . . . . . . .
. . . . . Q . . . . . . . . .
. . . . . . . Q . . . . . . .
. . . . . . . . . Q . . . . .
. . . . . . . . . . . Q . . .
. . . . . . . . . . . . . Q .
. Q . . . . . . . . . . . . .
. . . . Q . . . . . . . . . .
. . . . . . Q . . . . . . . .
. . . . . . . . Q . . . . . .
. . . . . . . . . . Q . . . .
. . . . . . . . . . . . Q . .
. . . . . . . . . . . . . . Q
Q . . . . . . . . . . . . . .
. . Q . . . . . . . . . . . .
```



### Alternate solution

If there is not specification, it outputs all solutions.

```ruby
class Queen
  attr_reader :count
  
  def initialize(num=8, out=true)
    @num   = num
    @out   = out
    @row   = *0...@num
    @frame = "+-" + "--" * @num + "+"
    @count = 0
    add = Array.new(2 * @num - 1, true)       # \ direction check
    sub = Array.new(2 * @num - 1, true)       # / direction check
    solve([], add, sub)
  end
  
  private
  def solve(row, add, sub)
    y = row.size
    if y == @num
      print_out(row) if @out
      @count += 1
    else
      (@row-row).each do |x|
        next unless add[x+y] and sub[x-y]
        add[x+y] = sub[x-y] = false
        solve(row+[x], add, sub)
        add[x+y] = sub[x-y] = true
      end
    end
  end
  
  def print_out(row)
    puts @frame
    row.each do |i|
      line = @num.times.map {|j| j==i ? "Q " : ". "}.join
      puts "| #{line}|"
    end
    puts @frame
  end
end
```


'''Example:'''

```ruby
(1..6).each do |n|
  puzzle = Queen.new(n)
  puts " #{n} Queen : #{puzzle.count}"
end

(7..12).each do |n|
  puzzle = Queen.new(n, false)                # do not display
  puts " #{n} Queen : #{puzzle.count}"
end
```


{{out}}
<pre style="height: 80ex; overflow: scroll">
+---+
| Q |
+---+
 1 Queen : 1
 2 Queen : 0
 3 Queen : 0
+---------+
| . Q . . |
| . . . Q |
| Q . . . |
| . . Q . |
+---------+
+---------+
| . . Q . |
| Q . . . |
| . . . Q |
| . Q . . |
+---------+
 4 Queen : 2
+-----------+
| Q . . . . |
| . . Q . . |
| . . . . Q |
| . Q . . . |
| . . . Q . |
+-----------+
+-----------+
| Q . . . . |
| . . . Q . |
| . Q . . . |
| . . . . Q |
| . . Q . . |
+-----------+
+-----------+
| . Q . . . |
| . . . Q . |
| Q . . . . |
| . . Q . . |
| . . . . Q |
+-----------+
+-----------+
| . Q . . . |
| . . . . Q |
| . . Q . . |
| Q . . . . |
| . . . Q . |
+-----------+
+-----------+
| . . Q . . |
| Q . . . . |
| . . . Q . |
| . Q . . . |
| . . . . Q |
+-----------+
+-----------+
| . . Q . . |
| . . . . Q |
| . Q . . . |
| . . . Q . |
| Q . . . . |
+-----------+
+-----------+
| . . . Q . |
| Q . . . . |
| . . Q . . |
| . . . . Q |
| . Q . . . |
+-----------+
+-----------+
| . . . Q . |
| . Q . . . |
| . . . . Q |
| . . Q . . |
| Q . . . . |
+-----------+
+-----------+
| . . . . Q |
| . Q . . . |
| . . . Q . |
| Q . . . . |
| . . Q . . |
+-----------+
+-----------+
| . . . . Q |
| . . Q . . |
| Q . . . . |
| . . . Q . |
| . Q . . . |
+-----------+
 5 Queen : 10
+-------------+
| . Q . . . . |
| . . . Q . . |
| . . . . . Q |
| Q . . . . . |
| . . Q . . . |
| . . . . Q . |
+-------------+
+-------------+
| . . Q . . . |
| . . . . . Q |
| . Q . . . . |
| . . . . Q . |
| Q . . . . . |
| . . . Q . . |
+-------------+
+-------------+
| . . . Q . . |
| Q . . . . . |
| . . . . Q . |
| . Q . . . . |
| . . . . . Q |
| . . Q . . . |
+-------------+
+-------------+
| . . . . Q . |
| . . Q . . . |
| Q . . . . . |
| . . . . . Q |
| . . . Q . . |
| . Q . . . . |
+-------------+
 6 Queen : 4
 7 Queen : 40
 8 Queen : 92
 9 Queen : 352
 10 Queen : 724
 11 Queen : 2680
 12 Queen : 14200

```



## Run BASIC


```runbasic
[loop]
input "How many queens (N>=4)";n
if n < 4 then
 print "Must be greater than 4"
 goto [loop]
end if

dim plot$(100,100)
dim q(n+20)
dim e(n+20)
dim o(n+20)
r=n mod 6
if r<>2 and r<>3 then 
  gosub [samp]
  goto [shoBoard]
end if
for i=1 to int(n/2)
  e(i) = 2 * i
next
for i=1 to int((n/2)+.5)
 o(i) = 2 *i-1
next
if r = 2 then gosub [edt2]
if r = 3 then gosub [edt3]
s = 1
for i=1 to n
  if e(i)>0 then 
    q(s) = e(i)
    s    = s+1
  end if
next
for i=1 to n
  if o(i) > 0 then 
    q(s) = o(i)
    s    = s + 1
  end if
next
' print board
[shoBoard]
cls
for i = 1 to n
  plot$(i,26-q(i)) = "*"
  plot$(i,24-n)    = chr$(96+i)
  plot$(n+1,26-i)  = str$(i)
next i
for ii = 1 to 100
 for jj = 1 to 100
  print left$(plot$(jj,ii)+" ",1);
 next jj
print
next ii
end

' the simple case
[samp] 
p = 1
for i = 1 to n
  if i mod 2=0 then 
    q(p) = i
    p    = p + 1
  end if
next i
for i = 1 to n
  if i mod 2 then 
    q(p) = i
    p    = p + 1
  end if
next
return
' edit list when remainder is 2
[edt2]
for i=1 to n
  if o(i) = 3 then 
    o(i) = 1 
   else 
    if o(i)=1 then o(i) = 3
  end if
  if o(i) = 5 then 
    o(i)= o(i) -1 
   else 
    if o(i) = 0 then 
      o(i) = 5
      return
    end if
  end if
next

' edit list when remainder is 3
[edt3]
for i = 1 to n
  if e(i) = 2 then 
    e(i)  = e(i)-1 
   else 
    if e(i) = 0 then 
      e(i) = 2
      goto [more]
    end if
  end if
next i
' edit list some more
[more]
for i = 1 to n
  if (o(i)=1 or o(i)=3) then 
    o(i) = o(i)-1 
   else 
    if o(i) = 0 then 
      o(i)   = 1
      o(i+1) = 3
      return
    end if
  end if
next
```


```txt
abcdefgh                                                                                            
   *    8                                                                                           
       *7                                                                                           
  *     6                                                                                           
        5                                                                                           
 *    * 4                                                                                           
    *   3                                                                                           
*       2                                                                                           
     *  1
```



## Rust


```rust
const N: usize = 8;

fn try(mut board: &mut [[bool; N]; N], row: usize, mut count: &mut i64) {
   if row == N {
       *count += 1;
       for r in board.iter() {
           println!("{}", r.iter().map(|&x| if x {"x"} else {"."}.to_string()).collect::<Vec<String>>().join(" "))
       }
       println!("");
       return
   }
   for i in 0..N {
       let mut ok: bool = true;
       for j in 0..row {
           if board[j][i]
               || i+j >= row && board[j][i+j-row]
               || i+row < N+j && board[j][i+row-j]
           { ok = false }
       }
       if ok {
           board[row][i] = true;
           try(&mut board, row+1, &mut count);
           board[row][i] = false;
       }
   }
}

fn main() {
   let mut board: [[bool; N]; N] = [[false; N]; N];
   let mut count: i64 = 0;
   try (&mut board, 0, &mut count);
   println!("Found {} solutions", count)
}
```



### Using Iterators

Solution to the puzzle using an iterator that yields the 92 solutions for 8 queens.

```rust
use std::collections::LinkedList;
use std::iter::IntoIterator;

fn main() {
    for (n, s) in NQueens::new(8).enumerate() {
        println!("Solution #{}:\n{}\n", n + 1, s.to_string());
    }
}

fn permutations<'a, T, I>(collection: I) -> Box<Iterator<Item=LinkedList<T>> + 'a>
    where I: 'a + IntoIterator<Item=T> + Clone,
          T: 'a + PartialEq + Copy + Clone {
    if collection.clone().into_iter().count() == 0 {
        Box::new(vec![LinkedList::new()].into_iter())
    }
    else { 
        Box::new(
            collection.clone().into_iter().flat_map(move |i| {
                permutations(collection.clone().into_iter()
                    .filter(move |&i0| i != i0)
                    .collect::<Vec<_>>())
                    .map(move |mut l| {l.push_front(i); l})
            })
        )
    }
}

pub struct NQueens {
    iterator: Box<Iterator<Item=NQueensSolution>>
}

impl NQueens {
    pub fn new(n: u32) -> NQueens {
        NQueens {
            iterator: Box::new(permutations(0..n)
                .filter(|vec| {
                    let iter = vec.iter().enumerate();
                    iter.clone().all(|(col, &row)| {
                        iter.clone().filter(|&(c,_)| c != col)
                            .all(|(ocol, &orow)| {
                            col as i32 - row as i32 != 
                                ocol as i32 - orow as i32 &&
                            col as u32 + row != ocol as u32 + orow 
                        })
                    })
                })
                .map(|vec| NQueensSolution(vec))
            )
        }
    }
}

impl Iterator for NQueens {
    type Item = NQueensSolution;
    fn next(&mut self) -> Option<NQueensSolution> {
        self.iterator.next()
    }
}

pub struct NQueensSolution(LinkedList<u32>);

impl ToString for NQueensSolution {
    fn to_string(&self) -> String {
        let mut str = String::new();
        for &row in self.0.iter() {
            for r in 0..self.0.len() as u32 {
                if r == row {
                    str.push_str("Q ");
                } else {
                    str.push_str("- ");
                }
            }
            str.push('\n');
        }
        str
    }
}
```



## SAS


```sas
/* Store all 92 permutations in a SAS dataset. Translation of Fortran 77 */
data queens;
array a{8} p1-p8;
array s{8};
array u{30};
n=8;
do i=1 to n;
a(i)=i;
end;
do i=1 to 4*n-2;
u(i)=0;
end;
m=0;
i=1;
r=2*n-1;
goto L40;
L30:
s(i)=j;
u(p)=1;
u(q+r)=1;
i=i+1;
L40:
if i>n then goto L80;
j=i;
L50:
z=a(i);
y=a(j);
p=i-y+n;
q=i+y-1;
a(i)=y;
a(j)=z;
if u(p)=0 and u(q+r)=0 then goto L30;
L60:
j=j+1;
if j<=n then goto L50;
L70:
j=j-1;
if j=i then goto L90;
z=a(i);
a(i)=a(j);
a(j)=z;
goto L70;
L80:
m=m+1;
output;
L90:
i=i-1;
if i=0 then goto L100;
p=i-a(i)+n;
q=i+a(i)-1;
j=s(i);
u(p)=0;
u(q+r)=0;
goto L60;
L100:
put n m;
keep p1-p8;
run;
```



## Scala


Extends a <code>Tuple2[T,T]</code> (also represented as <code>(T, T)</code>) using an enriched implicit class to define check that positions are safe or threatened.

Lazily generates permutations with an <code>Iterator</code>.


```scala

object NQueens {

  private implicit class RichPair[T](
    pair: (T,T))(
    implicit num: Numeric[T]
  ) {
    import num._

    def safe(x: T, y: T): Boolean =
      pair._1 - pair._2 != abs(x - y)
  }
  
  def solve(n: Int): Iterator[Seq[Int]] = {
    (0 to n-1)
      .permutations
      .filter { v =>
        (0 to n-1).forall { y =>
          (y+1 to n-1).forall { x =>
            (x,y).safe(v(x),v(y))
          }
        }
      }
  }

  def main(args: Array[String]): Unit = {
    val n = args.headOption.getOrElse("8").toInt
    val (solns1, solns2) = solve(n).duplicate
    solns1
      .zipWithIndex
      .foreach { case (soln, i) =>
        Console.out.println(s"Solution #${i+1}")
        output(n)(soln)
      }
    val n_solns = solns2.size
    if (n_solns == 1) {
      Console.out.println("Found 1 solution")
    } else {
      Console.out.println(s"Found $n_solns solutions")
    }
  }

  def output(n: Int)(board: Seq[Int]): Unit = {
    board.foreach { queen =>
      val row = 
        "_|" * queen + "Q" + "|_" * (n-queen-1)
      Console.out.println(row)
    }
  }
}

```



```txt

scala> NQueens.main(Array("8"))
Solution #1
Q|_|_|_|_|_|_|_
_|_|_|_|Q|_|_|_
_|_|_|_|_|_|_|Q
_|_|_|_|_|Q|_|_
_|_|Q|_|_|_|_|_
_|_|_|_|_|_|Q|_
_|Q|_|_|_|_|_|_
_|_|_|Q|_|_|_|_

Solution #2
Q|_|_|_|_|_|_|_
_|_|_|_|_|Q|_|_
_|_|_|_|_|_|_|Q
_|_|Q|_|_|_|_|_
_|_|_|_|_|_|Q|_
_|_|_|Q|_|_|_|_
_|Q|_|_|_|_|_|_
_|_|_|_|Q|_|_|_
...
Found 92 solutions

```



## Scheme


This is a simple breadth-first technique to retrieve all solutions. 


```scheme

(import (scheme base)
        (scheme write)
        (srfi 1))

;; return list of solutions to n-queens problem
(define (n-queens n) ; breadth-first solution
  (define (place-initial-row) ; puts a queen on each column of row 0
    (list-tabulate n (lambda (col) (list (cons 0 col)))))
  (define (place-on-row soln-so-far row)
    (define (invalid? col)
      (any (lambda (posn) 
             (or (= col (cdr posn)) ; on same column
                 (= (abs (- row (car posn))) ; on same diagonal
                    (abs (- col (cdr posn))))))
           soln-so-far))
    ;
    (do ((col 0 (+ 1 col))
         (res '() (if (invalid? col)
                    res
                    (cons (cons (cons row col) soln-so-far)
                          res))))
      ((= col n) res)))
  ;
  (do ((res (place-initial-row) 
            (apply append 
                   (map (lambda (soln-so-far) (place-on-row soln-so-far row))
                        res)))
       (row 1 (+ 1 row)))
    ((= row n) res)))

;; display solutions in 2-d array form
(define (pretty-print solutions n)
  (define (posn->index posn)
    (+ (* n (cdr posn))
       (car posn)))
  (define (pp solution)
    (let ((board (make-vector (square n) ".")))
      (for-each (lambda (queen) (vector-set! board 
                                             (posn->index queen)
                                             "Q"))
                solution)
      (let loop ((row 0)
                 (col 0))
        (cond ((= row n) 
               (newline))
              ((= col n) 
               (newline)
               (loop (+ 1 row) 0))
              (else
                (display (vector-ref board (posn->index (cons row col))))
                (loop row (+ 1 col)))))))
  ;
  (display (string-append "Found "
                          (number->string (length solutions))
                          " solutions for n="
                          (number->string n)
                          "\n\n"))
  (for-each pp solutions))

;; create table of number of solutions
(do ((n 1 (+ 1 n)))
  ((> n 10) )
  (display n)
  (display " ")
  (display (length (n-queens n)))
  (newline))

;; show some examples
(pretty-print (n-queens 1) 1)
(pretty-print (n-queens 2) 2)
(pretty-print (n-queens 3) 3)
(pretty-print (n-queens 4) 4)
(pretty-print (n-queens 5) 5)
(pretty-print (n-queens 8) 8)

```


{{out}}

```txt

1 1
2 0
3 0
4 2
5 10
6 4
7 40
8 92
9 352
10 724
Found 1 solutions for n=1

Q

Found 0 solutions for n=2

Found 0 solutions for n=3

Found 2 solutions for n=4

.Q..
...Q
Q...
..Q.

..Q.
Q...
...Q
.Q..

Found 10 solutions for n=5

Q....
...Q.
.Q...
....Q
..Q..

Q....
..Q..
....Q
.Q...
...Q.

[[ etc ]]
Found 92 solutions for n=8

Q.......
......Q.
....Q...
.......Q
.Q......
...Q....
.....Q..
..Q.....

Q.......
......Q.
...Q....
.....Q..
.......Q
.Q......
....Q...
..Q.....

[[ etc ]]

```



## Scilab

Naive brute-force search.
<lang>//Length of board side
Board_size = 8;

function flag_out = no_attack(side, board, pos)
    //Evaluates (pos(1),pos(2)) in board if it's not on any queen attacking range
    //side (scalar): board's side length
    //board (sidexside matrix): matrix of 0s and 1s representing queens on a board
    //pos (1x2 matrix): postition on board to be evaluated
    //flag_out (bool): %T if position is available, and %F otherwise
    
    //Counting queens on rows and columns
    row_col = sum(board(pos(1),:)) + sum(board(:,pos(2)));
    
    //Counting queens on first diagonal
    diag_1 = sum(...
                 diag(board, 0 +...
                     (pos(2)>pos(1))*(pos(2)-pos(1)) +...
                     (pos(1)>pos(2))*(pos(2)-pos(1))...
                     )...
                 );
    
    //Counting queens on second diagonal
    a = pos(1) + pos(2);
    if a<=side+1 then
        rows = [1:a-1]
        cols = a - rows;
    else
        d = 2*(side+1)-a-1;
        rows = [side:-1:side-d+1]
        cols  = a - rows;
    end

    diag_2 = 0;
    for i = 1:length(rows)
        diag_2 = diag_2 + board(rows(i),cols(i));
    end
    
    //Check if there's any queen
    flag_out = ( ~(row_col | diag_1 | diag_2) );
endfunction

//Solution counter
Sol_count = 0;
   
//"Soltion found" flag
Sol_found = %F;
 
//Empty board
Board = zeros(Board_size,Board_size);
 
//Matrix for backtracking
Queens= zeros(Board_size,2);

//Queens counter
N_queens = Board_size;

//Row and column counters
i = 1; j = 1;

//Start counting time
tic();

//Begin search
while i <= Board_size
    while j <= Board_size
        //Availability flag: check position (i,j)
        flag = %F;
        if (0 < i & 0 < j) & (i <= Board_size & j <= Board_size) then
            flag = no_attack(Board_size,Board,[i j]);
        end
        
        //Reset solution flag
        Sol_found = %F;
        
        if flag then
            //Put a queen on the board if position is available
            Board(i,j) = 1;
            
            //Update number of remaining queens
            N_queens = N_queens - 1;
            
            //Keep track of queens positions
            Queens(Board_size - N_queens,:) = [i j];
            
            //Jump to next row end of line is reached
            if i+1<=Board_size
                i = i + 1;
            end
            //Start over from the begining of new line
            j = 0;
            
            //Count and flag a solution if all queens have
            //been placed on the board
            if N_queens == 0 then
                Sol_count = Sol_count + 1;
                Sol_found = %T;
                break
            end
        end
        
        //Increment column number
        j = j + 1;
    end
    
    //Increment row number and start from first column
    if ~Sol_found then
        i = i + 1;
        j = 1;
        
        //Limiting placement of the first queen to the first row
        //Stop searching solutions if columns of first row 
        //have been tested
        if i == 2 & j == 1 & sum(Board) == 0  then
            break
        end
    end
    
    //Backtracking: if (i,j) reaches the and of the board
    //and there are queens left to be placed on it
    if ~Sol_found & i == Board_size + 1 & j == 1 then
        ind = Board_size - N_queens;
        if ind > 0 then
            //Recover last queen's position
            i = Queens(ind,1);
            j = Queens(ind,2);
            
            //Remove it from the board and from the counter
            Board(i,j) = 0;
            Queens(ind,:) = [0 0];
            N_queens = N_queens + 1;
            
            //Move to next column
            j = j + 1;
        end
    end 
end

//Printing result on console
disp("There are "+string(Sol_count)+" solutions for a "+...
     string(Board_size)+"x"+string(Board_size)+" board.");
//Time elapsed
disp("Time: "+string(toc())+"s.");
```


{{out}}


```txt
 There are 92 solutions for a 8x8 board.

 Time: 58.705327s.
```



## Seed7


```seed7
$ include "seed7_05.s7i";

var array integer: board is 8 times 0;
var integer: solutionNum is 0;

const func boolean: safe (in integer: y) is func
  result
    var boolean: safe is TRUE;
  local
    var integer: i is 1;
  begin
    while i < y and safe do
      safe := board[y - i] <> board[y] and
              board[y - i] <> board[y] - i and
              board[y - i] <> board[y] + i;
      incr(i);
    end while;
  end func;

const proc: putBoard is func
  local
    var integer: y is 0;
  begin
    incr(solutionNum);
    writeln;
    writeln("Solution " <& solutionNum);
    for y range 1 to 8 do
      writeln("|_" mult pred(board[y]) <& "|Q" <& "|_" mult (8 - board[y]) <& "|");
    end for;
  end func;

const proc: main is func
  local
    var integer: y is 1;
  begin
    while y >= 1 do
      repeat
        incr(board[y]);
      until board[y] > 8 or safe(y);
      if board[y] <= 8 then
        if y < 8 then
          incr(y);
          board[y] := 0;
        else
          putBoard;
        end if;
      else
        decr(y);
      end if;
    end while;
  end func;
```



## Sidef

{{trans|Perl 6}}

```ruby
func N_queens_solution(N = 8) {

    func collision(field, row) {
        for i in (^row) {
            var distance = (field[i] - field[row])
            distance ~~ [0, row-i, i-row] && return true
        }
        return false
    }

    func search(field, row) {
        row == N && return field
        for i in (^N) {
            field[row] = i
            if (!collision(field, row)) {
                return (__FUNC__(field, row+1) || next)
            }
        }
        return []
    }

    for i in (0 .. N>>1) {
        if (var r = search([i], 1)) {
            return r
        }
    }
}

for n in (1..15) {
    say "#{'%2d' % n}: #{N_queens_solution(n) || 'No solution'}"
}
```

{{out}}

```txt

 1: [0]
 2: No solution
 3: No solution
 4: [1, 3, 0, 2]
 5: [0, 2, 4, 1, 3]
 6: [1, 3, 5, 0, 2, 4]
 7: [0, 2, 4, 6, 1, 3, 5]
 8: [0, 4, 7, 5, 2, 6, 1, 3]
 9: [0, 2, 5, 7, 1, 3, 8, 6, 4]
10: [0, 2, 5, 7, 9, 4, 8, 1, 3, 6]
11: [0, 2, 4, 6, 8, 10, 1, 3, 5, 7, 9]
12: [0, 2, 4, 7, 9, 11, 5, 10, 1, 6, 8, 3]
13: [0, 2, 4, 1, 8, 11, 9, 12, 3, 5, 7, 10, 6]
14: [0, 2, 4, 6, 11, 9, 12, 3, 13, 8, 1, 5, 7, 10]
15: [0, 2, 4, 1, 9, 11, 13, 3, 12, 8, 5, 14, 6, 10, 7]

```



## SNOBOL4


```SNOBOL4

* N queens problem
* Set N to the desired number.  The program prints out all solution boards.
	N = 5
	NM1 = N - 1; NP1 = N + 1; NSZ = N * NP1; &STLIMIT = 10 ** 9; &ANCHOR = 1
	DEFINE('SOLVE(B)I')
* This pattern tests if the first queen attacks any of the others:
	TEST = BREAK('Q') 'Q' (ARBNO(LEN(N) '-') LEN(N) 'Q'
+	      | ARBNO(LEN(NP1) '-') LEN(NP1) 'Q'
+	      | ARBNO(LEN(NM1) '-') LEN(NM1) 'Q')
	P = LEN(NM1) . X LEN(1); L = 'Q' DUPL('-',NM1) ' '
	SOLVE()        :(END)
SOLVE	EQ(SIZE(B),NSZ) 	    :S(PRINT)
* Add another row with a queen:
	B = L B
LOOP	I = LT(I,N) I + 1 :F(RETURN)
	B TEST :S(NEXT)
	SOLVE(B)
* Try queen in next square:
NEXT	B P = '-' X :(LOOP)
PRINT	SOLUTION = SOLUTION + 1
	OUTPUT = 'Solution number ' SOLUTION ' is:'
PRTLOOP B LEN(NP1) . OUTPUT = :S(PRTLOOP)F(RETURN)
END

```



## Sparkling

This is somewhat a transliteration of the "shortened" C++ code above.


```sparkling
let print_table = function (pos) {
	pos.foreach(function (_, i) {
		stdout.printf("  %c", 'a' + i);
	});

	stdout.write("\n");

	pos.foreach(function (col, row) {
		stdout.printf("%d", row + 1);
		stdout.printf("%s #\n", range(col).reduce("", function (s, t) {
			return s .. "   ";
		}));
	});

	stdout.write("\n\n");
};

let threatens = function (row_a, col_a, row_b, col_b) {
	return row_a == row_b
	    or col_a == col_b
	    or abs(row_a - row_b) == abs(col_a - col_b);
};

let good = function(pos, end_idx) {
	return pos.all(function (col_a, row_a) {
		return range(row_a + 1, end_idx).all(function (row_b) {
			let col_b = pos[row_b];
			return not threatens(row_a, col_a, row_b, col_b);
		});
	});
};

// Returns number of solutions
let n_queens = function (pos, index) {
	if index >= pos.length {
		if good(pos, index) {
			print_table(pos);
			return 1;
		}

		return 0;
	}

	if not good(pos, index) {
		return 0;
	}

	return pos.map(function (_, col) {
		pos[index] = col;
		return n_queens(pos, index + 1);
	}).reduce(0, function (a, b) { return a + b; });
};

stdout.printf("%d solutions\n", n_queens(range(8), 0));
```



## SQL


This implementation, which solves the problem for n=8, makes use of Common Table Expressions and has been tested with SQLite (>=3.8.3) and Postgres (please note the related comment in the code). It might be compatible with other SQL dialects as well. A gist with the SQL file and a Python script that runs it using SQLite is available on Github: https://gist.github.com/adewes/5e5397b693eb50e67f07


```SQL

WITH RECURSIVE
  positions(i) as (
    VALUES(0)
    UNION SELECT ALL
    i+1 FROM positions WHERE i < 63
    ),
  solutions(board, n_queens) AS (
    SELECT '----------------------------------------------------------------', cast(0 AS bigint) 
      FROM positions
    UNION
    SELECT
      substr(board, 1, i) || '*' || substr(board, i+2),n_queens + 1 as n_queens
      FROM positions AS ps, solutions 
    WHERE n_queens < 8
      AND substr(board,1,i) != '*'
      AND NOT EXISTS (
        SELECT 1 FROM positions WHERE
          substr(board,i+1,1) = '*' AND
            (
                i % 8 = ps.i %8 OR
                cast(i / 8 AS INT) = cast(ps.i / 8 AS INT) OR
                cast(i / 8 AS INT) + (i % 8) = cast(ps.i / 8 AS INT) + (ps.i % 8) OR
                cast(i / 8 AS INT) - (i % 8) = cast(ps.i / 8 AS INT) - (ps.i % 8)
            )
        LIMIT 1
        ) 
   ORDER BY n_queens DESC -- remove this when using Postgres (they don't support ORDER BY in CTEs)
  )
SELECT board,n_queens FROM solutions WHERE n_queens = 8;


```



## Standard ML

This implementation uses failure continuations for backtracking.

```Standard ML

(* 
 * val threat : (int * int) -> (int * int) -> bool
 * Returns true iff the queens at the given positions threaten each other
 *)
fun threat (x, y) (x', y') =
  x = x' orelse y = y' orelse abs(x - x') = abs(y - y');

(*
 * val conflict : (int * int) -> (int * int) list -> bool
 * Returns true if there exists a conflict with the position and the list of queens.
 *)
fun conflict pos = List.exists (threat pos);

(*
 * val addqueen : (int * int * (int * int) list * (unit -> (int * int) list option)) -> (int * int) list option
 * Returns either NONE in the case that no solution exists or SOME(l) where l is a list of positions making up the solution.
 *)
fun addqueen(i, n, qs, fc) =
  let
    fun try j =
      if j > n then fc()
      else if (conflict (i, j) qs) then try (j + 1)
      else if i = n then SOME((i, j)::qs)
      else addqueen(i + 1, n, (i,j)::qs, fn() => try (j + 1))
  in
    try 1
  end;

(*
 * val queens : int -> (int * int) list option
 * Given the board dimension n, returns a solution for the n-queens problem.
 *)
fun queens(n) = addqueen(1, n, [], fn () => NONE);

(* SOME [(8,4),(7,2),(6,7),(5,3),(4,6),(3,8),(2,5),(1,1)] *)
queens(8);

(* NONE *)
queens(2);

```



## Stata


###  Iterative version 

Adapted from the Fortran 77 program, to illustrate the '''[http://www.stata.com/help.cgi?m2_goto goto]''' statement in Stata.


```stata
mata
real matrix queens(real scalar n) {
	real scalar i, j, k, p, q
	real rowvector a, s, u, v
	real matrix m
	
	m = J(0, n, .)
	a = 1..n
	s = J(1, n, 0)
	u = J(1, 2*n-1, 1)
	v = J(1, 2*n-1, 1)
	i = 1
L1:	if (i > n) {
		m = m\a
		goto L4
	}
	j=i
L2:	k = a[j]
	p = i-k+n
	q = i+k-1
	if (u[p] & v[q]) {
		u[p] = v[q] = 0
		a[j] = a[i]
		a[i] = k
		s[i++] = j
		goto L1
	}
L3:	if (++j <= n) goto L2
L4:	if (--i == 0) return(m)
	j = s[i]
	k = a[i]
	a[i] = a[j]
	a[j] = k
	p = i-k+n
	q = i+k-1
	u[p] = v[q] = 1
	goto L3
}

a = queens(8)
e = I(8)
1:/e[a[1,.],.]
       1   2   3   4   5   6   7   8
    +---------------------------------+
  1 |  1   .   .   .   .   .   .   .  |
  2 |  .   .   .   .   1   .   .   .  |
  3 |  .   .   .   .   .   .   .   1  |
  4 |  .   .   .   .   .   1   .   .  |
  5 |  .   .   1   .   .   .   .   .  |
  6 |  .   .   .   .   .   .   1   .  |
  7 |  .   1   .   .   .   .   .   .  |
  8 |  .   .   .   1   .   .   .   .  |
    +---------------------------------+


rows(a)
  92
end
```


It's also possible to save the solutions to a Stata dataset:


```stata
clear
mata: a=queens(8)
getmata (a*)=a
save queens, replace
```



###  Recursive version 


The recursive solution is adapted from one of the Python programs.


```stata
mata
real matrix queens_rec(real scalar n) {
	real rowvector a, u, v
	real matrix m
	
	a = 1..n
	u = J(1, 2*n-1, 1)
	v = J(1, 2*n-1, 1)
	m = J(0, n, .)
	queens_aux(n, 1, a, u, v, m)
	return(m)
}

void queens_aux(real scalar n, real scalar i, real rowvector a,
	real rowvector u, real rowvector v, real matrix m) {
	real scalar j, k
	
	if (i > n) {
		m = m\a
	} else {
		for (j = i; j <= n; j++) {
			k = a[j]
			p = i-k+n
			q = i+k-1
			if (u[p] & v[q]) {
				u[p] = v[q] = 0
				a[j] = a[i]
				a[i] = k
				queens_aux(n, i+1, a, u, v, m)
				u[p] = v[q] = 1
				a[i] = a[j]
				a[j] = k
			}
		}
	}
}
end
```


The iterative and the recursive programs are equivalent:


```stata
queens(8) == queens_rec(8)
  1
```



## Swift

Port of the optimized C code above

```Swift

	let maxn = 31

	func nq(n: Int) -> Int {
	    var cols = Array(repeating: 0, count: maxn)
	    var diagl = Array(repeating: 0, count: maxn)
	    var diagr = Array(repeating: 0, count: maxn)
	    var posibs = Array(repeating: 0, count: maxn)
	    var num = 0
	    for q0 in 0...n-3 {
		for q1 in q0+2...n-1 {
		    let bit0: Int = 1<<q0
		    let bit1: Int = 1<<q1
		    var d: Int = 0
		    cols[0] = bit0 | bit1 | (-1<<n)
		    diagl[0] = (bit0<<1|bit1)<<1
		    diagr[0] = (bit0>>1|bit1)>>1

		    var posib: Int = ~(cols[0] | diagl[0] | diagr[0])

		    while (d >= 0) {
			while(posib != 0) {
			    let bit: Int = posib & -posib
			    let ncols: Int = cols[d] | bit
			    let ndiagl: Int = (diagl[d] | bit) << 1;
			    let ndiagr: Int = (diagr[d] | bit) >> 1;
			    let nposib: Int = ~(ncols | ndiagl | ndiagr);
			    posib^=bit
			    num += (ncols == -1 ? 1 : 0)
			    if (nposib != 0){
				if(posib != 0) {
				    posibs[d] = posib
				    d += 1
				}
				cols[d] = ncols
				diagl[d] = ndiagl
				diagr[d] = ndiagr
				posib = nposib
			    }
			}
			d -= 1
			posib = d<0 ? n : posibs[d]

		    }
		}

	    }
	    return num*2
	}
	if(CommandLine.arguments.count == 2) {

	    let board_size: Int = Int(CommandLine.arguments[1])!
	    print ("Number of solutions for board size \(board_size) is: \(nq(n:board_size))")

	} else {
	    print("Usage: 8q <n>")
	}


```




## SystemVerilog

Create a random board configuration, with the 8-queens as a constraint

```SystemVerilog
program N_queens;

  parameter SIZE_LOG2 = 3;
  parameter SIZE = 1 << SIZE_LOG2;

  `define ABS_DIFF(a,b) (a>b?a-b:b-a)

  class board;
    rand bit [SIZE_LOG2-1:0] row[SIZE];

    constraint rook_moves {
      foreach (row[i]) foreach (row[j]) if (i < j) {
        row[i] != row[j];
      }
    }

    constraint diagonal_moves {
      foreach (row[i]) foreach (row[j]) if (i < j) {
        `ABS_DIFF(row[i], row[j]) != `ABS_DIFF(i,j);
      }
    }

    function void next;
      randomize;
      foreach (row[i]) begin
        automatic bit [SIZE-1:0] x = 1 << row[i];
        $display( "  %b", x );
      end
      $display("--");
    endfunction

  endclass

  board b = new;
  initial repeat(1) b.next;

endprogram

```



## Tailspin

A functional-ish solution utilising tailspin's data flows

```tailspin

templates queens
  def n: $;
  templates addColumn
    def prev: $;
    templates addIfPossible
      def row: $;
      def minor: $ - $prev::length - 1;
      def major: $ + $prev::length + 1;
      // If prev is not an array that contains row, send it on...
      $prev -> (<~[<$row>]> $ !)
            -> (<?($ -> [i]($ - $i !) <~[<$minor>]>)> $ !)
            -> (<?($ -> [i]($ + $i !) <~[<$major>]>)> $ !)
            -> [ $..., $row] !
    end addIfPossible
    1..$n -> addIfPossible !
  end addColumn
  1..$n -> [$] -> #
  <[]($n)> $ !
  <> $ -> addColumn -> #
end queens
 
def solutions: [ 8 -> queens ];
'For 8 queens there are $solutions::length; solutions
' -> !OUT::write
 
def columns: ['abcdefgh'...];
'One of them is $solutions(1) -> [i]('$columns($i);$;' !);
' -> !OUT::write
 
'For 3 queens there are $:[3 -> queens] -> $::length; solutions
' -> !OUT::write

```

{{out}}

```txt

For 8 queens there are 92 solutions
One of them is [a1, b5, c8, d6, e3, f7, g2, h4]
For 3 queens there are 0 solutions

```


A solution using state to find one solution if any exist

```tailspin

templates queens
  def n: $;
  templates getRowColumn
    <?($@queens.freeRows($.r) <0>)> 0 !
    <?($@queens.freeMaxs($.r + $.c) <0>)> 0 !
    <?($@queens.freeMins($.c - $.r + $n) <0>)> 0 !
    <> 1!
  end getRowColumn

  sink setRowColumn
    def p: $;
    @queens.freeRows($p.r): $p.val;
    @queens.freeMaxs($p.c + $p.r): $p.val;
    @queens.freeMins($p.c - $p.r + $n): $p.val;
  end setRowColumn

  templates placeQueen
    def c: $;
    1 -> #
    <-1> 1! // Use -1 to signal successful completion
    <$n+1> 0 !
    <?({r: $, c: $c} -> getRowColumn <1>)>
      def r: $;
      @queens.queenRows($r): $c;
      {r: $, c: $c, val: 0} -> !setRowColumn
      $c -> (<$n> -1!
        <?($c + 1 -> placeQueen <1>)> -1!
        <>
          {r: $r, c: $c, val: 1} -> !setRowColumn
          $r + 1 !) -> #
    <> $ + 1 -> #
  end placeQueen

  @: { freeRows: [1..$n -> 1],
    freeMaxs: [1..$n*2 -> 1],
    freeMins: [1..$n*2 -> 1],
    queenRows: [1..$n -> -1] };
  1 -> placeQueen -> (<1> $@queens.queenRows ! <> 'non-existent'!)!
end queens

'A solution to the 8 queens problem is $:8 -> queens;
' -> !OUT::write
'A solution to the 4 queens problem is $:4 -> queens;
' -> !OUT::write
'A solution to the 3 queens problem is $:3 -> queens;
' -> !OUT::write

```

{{out}}

```txt

A solution to the 8 queens problem is [1, 7, 5, 8, 2, 4, 6, 3]
A solution to the 4 queens problem is [3, 1, 4, 2]
A solution to the 3 queens problem is non-existent

```



## Tcl

This solution is based on the [[C]] version on [[wp:Eight queens puzzle solutions#C|wikipedia]]. By default it solves the 8-queen case; to solve for any other number, pass ''N'' as an extra argument on the script's command line (see the example for the ''N''=6 case, which has anomalously few solutions).

{{works with|Tcl|8.5}}

```tcl
package require Tcl 8.5

proc unsafe {y} {
    global b
    set x [lindex $b $y]
    for {set i 1} {$i <= $y} {incr i} {
	set t [lindex $b [expr {$y - $i}]]
	if {$t==$x || $t==$x-$i || $t==$x+$i} {
	    return 1
	}
    }
    return 0
}

proc putboard {} {
    global b s N
    puts "\n\nSolution #[incr s]"
    for {set y 0} {$y < $N} {incr y} {
	for {set x 0} {$x < $N} {incr x} {
	    puts -nonewline [expr {[lindex $b $y] == $x ? "|Q" : "|_"}]
	}
	puts "|"
    }
}

proc main {n} {
    global b N
    set N $n
    set b [lrepeat $N 0]
    set y 0
    lset b 0 -1
    while {$y >= 0} {
	lset b $y [expr {[lindex $b $y] + 1}]
	while {[lindex $b $y] < $N && [unsafe $y]} {
	    lset b $y [expr {[lindex $b $y] + 1}]
	}
	if {[lindex $b $y] >= $N} {
	    incr y -1
	} elseif {$y < $N-1} {
	    lset b [incr y] -1;
	} else {
	    putboard
	}
    }
}

main [expr {$argc ? int(0+[lindex $argv 0]) : 8}]
```

{{out}}

```txt
$ tclsh8.5 8queens.tcl 6

Solution #1
|_|Q|_|_|_|_|
|_|_|_|Q|_|_|
|_|_|_|_|_|Q|
|Q|_|_|_|_|_|
|_|_|Q|_|_|_|
|_|_|_|_|Q|_|


Solution #2
|_|_|Q|_|_|_|
|_|_|_|_|_|Q|
|_|Q|_|_|_|_|
|_|_|_|_|Q|_|
|Q|_|_|_|_|_|
|_|_|_|Q|_|_|


Solution #3
|_|_|_|Q|_|_|
|Q|_|_|_|_|_|
|_|_|_|_|Q|_|
|_|Q|_|_|_|_|
|_|_|_|_|_|Q|
|_|_|Q|_|_|_|


Solution #4
|_|_|_|_|Q|_|
|_|_|Q|_|_|_|
|Q|_|_|_|_|_|
|_|_|_|_|_|Q|
|_|_|_|Q|_|_|
|_|Q|_|_|_|_|
```



## UNIX Shell

{{works with|Bash}}
The total number of solutions for 8 queens is displayed at the end of the run.  The code could be adapted to display a selected solution or multiple solutions. This code runs anywhere you can get bash to run. 


```bash
#!/bin/bash
 
# variable declaration
typeset -i BoardSize=8
typeset -i p=0
typeset -i total=0
typeset -i board
 
# initialization
function init
{
    for (( i=0;i<$BoardSize;i++ ))
    do
        (( board[$i]=-1 ))
    done
}
 
# check if queen can be placed
function place 
{
        typeset -i flag=1
        for (( i=0;i<$1;i++ ))
        do
                if [[ (${board[$i]}-${board[$1]} -eq ${i}-${1}) || (${board[$i]}-${board[$1]} -eq ${1}-${i}) || (${board[$i]} -eq ${board[$1]}) ]]
                then
                        (( flag=0 ))
                fi
        done
        [[ $flag -eq 0 ]]
        return $?
}
 
# print the result
function out
{
        printf "Problem of queen %d:%d\n" $BoardSize $total
}
 
# free the variables
function depose
{
    unset p
    unset total
    unset board
    unset BoardSize
}
 
# back tracing
function work
{
    while [[ $p -gt -1 ]]
        do
        (( board[$p]++ ))
        if  [[ ${board[$p]} -ge ${BoardSize} ]]
        then  # back tracing
            (( p-- ))
        else  # try next position
            place $p
            if [[ $? -eq 1 ]]
            then
                (( p++ ))
                if [[ $p -ge ${BoardSize} ]]
                then
                    (( total++ ))
                    (( p-- ))
                else
                    (( board[$p]=-1 ))
                fi
            fi
        fi
    done
}
 
# entry
init
work
out
depose
```



## Ursala

This is invoked as a command line application by queens -n, where
n is a number greater than 3. Multiple solutions may be reported
but reflections and rotations thereof are omitted.

```Ursala
#import std
#import nat

remove_reflections = ^D(length@ht,~&); ~&K2hlPS+ * ^lrNCCs/~&r difference*D
remove_rotations   = ~&K2hlrS2S+ * num; ~&srlXSsPNCCs

#executable <'par',''>
#optimize+

queens =

%np+~command.options.&h.keyword.&iNC; -+
   ~&iNC+ file$[contents: --<''>+ mat` *+ %nP*=*],
   remove_rotations+ remove_reflections+ ~&rSSs+ nleq-<&l*rFlhthPXPSPS,
   ~&i&& ~&lNrNCXX; ~&rr->rl ^/~&l ~&lrrhrSiF4E?/~&rrlPlCrtPX @r ^|/~& ^|T\~& -+
      -<&l^|*DlrTS/~& ~&iiDlSzyCK9hlPNNXXtCS,
      ^jrX/~& @rZK20lrpblPOlrEkPK13lhPK2 ~&i&& nleq$-&lh+-,
   ^/~&NNXS+iota -<&l+ ~&plll2llr2lrPrNCCCCNXS*=irSxPSp+ ^H/block iota; *iiK0 ^/~& sum+-
```

The output shows one solution on each line. 
A solution is reported as a sequence of <math>n</math> numbers 
with the <math>i</math>-th number being the index of the occupied row
in the <math>i</math>-th column.

```txt

$ queens -4                     
2 3 0 1                         
$ queens -5                     
0 2 1 3 4                       
2 4 3 0 1
1 3 2 4 0
$ queens 6
4 3 0 2 1 5

```



## VBA

{{trans|BBC BASIC}}

```vb
'N-queens problem - non recursive & structured - vba - 26/02/2017
Sub n_queens()
    Const l = 15  'number of queens
    Const b = False  'print option
    Dim a(l), s(l), u(4 * l - 2)
    Dim n, m, i, j, p, q, r, k, t, z
    For i = 1 To UBound(a): a(i) = i: Next i
    For n = 1 To l
        m = 0
        i = 1
        j = 0
        r = 2 * n - 1
        Do
            i = i - 1
            j = j + 1
            p = 0
            q = -r
            Do
                i = i + 1
                u(p) = 1
                u(q + r) = 1
                z = a(j): a(j) = a(i): a(i) = z  'Swap a(i), a(j)
                p = i - a(i) + n
                q = i + a(i) - 1
                s(i) = j
                j = i + 1
            Loop Until j > n Or u(p) Or u(q + r)
            If u(p) = 0 Then
                If u(q + r) = 0 Then
                    m = m + 1  'm: number of solutions
                    If b Then
                        Debug.Print "n="; n; "m="; m
                        For k = 1 To n
                            For t = 1 To n
                                Debug.Print IIf(a(n - k + 1) = t, "Q", ".");
                            Next t
                            Debug.Print
                        Next k
                    End If
                End If
            End If
            j = s(i)
            Do While j >= n And i <> 0
                Do
                    z = a(j): a(j) = a(i): a(i) = z  'Swap a(i), a(j)
                    j = j - 1
                Loop Until j < i
                i = i - 1
                p = i - a(i) + n
                q = i + a(i) - 1
                j = s(i)
                u(p) = 0
                u(q + r) = 0
            Loop
        Loop Until i = 0
        Debug.Print n, m  'number of queens, number of solutions
    Next n
End Sub 'n_queens
```

{{out}}

```txt

 1             1 
 2             0 
 3             0 
 4             2 
 5             10 
 6             4 
 7             40 
 8             92 
 9             352 
 10            724 
 11            2680 
 12            14200 
 13            73712 
 14            365596 
 15            2279184 

```



## VBScript

{{trans|BBC BASIC}}
To have the solutions printed (raw format) uncomment the ad hoc statement.

```vb
'N-queens problem - non recursive & structured - vbs - 24/02/2017
const l=15
dim a(),s(),u(): redim a(l),s(l),u(4*l-2)
for i=1 to l: a(i)=i: next
for n=1 to l
    m=0
    i=1
    j=0
    r=2*n-1
    Do
        i=i-1
        j=j+1
        p=0
        q=-r
        Do
            i=i+1
            u(p)=1
            u(q+r)=1
            z=a(j): a(j)=a(i): a(i)=z  'swap a(i),a(j)
            p=i-a(i)+n
            q=i+a(i)-1
            s(i)=j
            j=i+1
        Loop Until j>n Or u(p)<>0 Or u(q+r)<>0
        If u(p)=0 Then
            If u(q+r)=0 Then
                m=m+1  'm: number of solutions
                'x="": for k=1 to n: x=x&" "&a(k): next: msgbox x,,m
             End If
        End If
        j=s(i)
        Do While j>=n And i<>0
            Do
                z=a(j): a(j)=a(i): a(i)=z  'swap a(i),a(j)
                j=j-1
            Loop Until j<i
            i=i-1
            p=i-a(i)+n
            q=i+a(i)-1
            j=s(i)
            u(p)=0
            u(q+r)=0
        Loop
    Loop Until i=0
    wscript.echo n &":"& m
next 'n
```

{{out}}

```txt

1 : 1
2 : 0
3 : 0
4 : 2
5 : 10
6 : 4
7 : 40
8 : 92
9 : 352
10 : 724
11 : 2680
12 : 14200
13 : 73712
14 : 365596
15 : 2279184

```



## Visual Basic

{{works with|Visual Basic|VB6 Standard}}
{{trans|BBC BASIC}}

```vb
'N-queens problem - non recursive & structured - vb6 - 25/02/2017
Sub n_queens()
    Const l = 15  'number of queens
    Const b = False  'print option
    Dim a(l), s(l), u(4 * l - 2)
    Dim n, m, i, j, p, q, r, k, t, z
    For i = 1 To UBound(a): a(i) = i: Next i
    For n = 1 To l
        m = 0
        i = 1
        j = 0
        r = 2 * n - 1
        Do
            i = i - 1
            j = j + 1
            p = 0
            q = -r
            Do
                i = i + 1
                u(p) = 1
                u(q + r) = 1
                z = a(j): a(j) = a(i): a(i) = z  'Swap a(i), a(j)
                p = i - a(i) + n
                q = i + a(i) - 1
                s(i) = j
                j = i + 1
            Loop Until j > n Or u(p) Or u(q + r)
            If u(p) = 0 Then
                If u(q + r) = 0 Then
                    m = m + 1  'm: number of solutions
                    If b Then
                        Debug.Print "n="; n; "m="; m
                        For k = 1 To n
                            For t = 1 To n
                                Debug.Print IIf(a(n - k + 1) = t, "Q", ".");
                            Next t
                            Debug.Print
                        Next k
                    End If
                End If
            End If
            j = s(i)
            Do While j >= n And i <> 0
                Do
                    z = a(j): a(j) = a(i): a(i) = z  'Swap a(i), a(j)
                    j = j - 1
                Loop Until j < i
                i = i - 1
                p = i - a(i) + n
                q = i + a(i) - 1
                j = s(i)
                u(p) = 0
                u(q + r) = 0
            Loop
        Loop Until i = 0
        Debug.Print n, m  'number of queens, number of solutions
    Next n
End Sub 'n_queens
```

{{out}}

```txt

 1             1 
 2             0 
 3             0 
 4             2 
 5             10 
 6             4 
 7             40 
 8             92 
 9             352 
 10            724 
 11            2680 
 12            14200 
 13            73712
 14            365596
 15            2279184

```



## Visual Basic .NET

{{trans|BBC BASIC}}

```vb
'N-queens problem - non recursive & structured - vb.net - 26/02/2017
Module Mod_n_queens
    Sub n_queens()
        Const l = 15  'number of queens
        Const b = False  'print option
        Dim a(l), s(l), u(4 * l - 2)
        Dim n, m, i, j, p, q, r, k, t, z
        Dim w As String
        For i = 1 To UBound(a) : a(i) = i : Next i
        For n = 1 To l
            m = 0
            i = 1
            j = 0
            r = 2 * n - 1
            Do
                i = i - 1
                j = j + 1
                p = 0
                q = -r
                Do
                    i = i + 1
                    u(p) = 1
                    u(q + r) = 1
                    z = a(j) : a(j) = a(i) : a(i) = z  'Swap a(i), a(j)
                    p = i - a(i) + n
                    q = i + a(i) - 1
                    s(i) = j
                    j = i + 1
                Loop Until j > n Or u(p) Or u(q + r)
                If u(p) = 0 Then
                    If u(q + r) = 0 Then
                        m = m + 1  'm: number of solutions
                        If b Then
                            Debug.Print("n=" & n & " m=" & m) : w = ""
                            For k = 1 To n
                                For t = 1 To n
                                    w = w & If(a(n - k + 1) = t, "Q", ".")
                                Next t
                                Debug.Print(w)
                            Next k
                        End If
                    End If
                End If
                j = s(i)
                Do While j >= n And i <> 0
                    Do
                        z = a(j) : a(j) = a(i) : a(i) = z  'Swap a(i), a(j)
                        j = j - 1
                    Loop Until j < i
                    i = i - 1
                    p = i - a(i) + n
                    q = i + a(i) - 1
                    j = s(i)
                    u(p) = 0
                    u(q + r) = 0
                Loop
            Loop Until i = 0
            Debug.Print(n & vbTab & m)  'number of queens, number of solutions
        Next n
    End Sub 'n_queens
End Module
```

{{out}}

```txt

1   1
2   0
3   0
4   2
5   10
6   4
7   40
8   92
9   352
10  724
11  2680
12  14200
13  73712
14  365596
15  2279184 

```



## Wart


```Wart
def (nqueens n queens)
  prn "step: " queens  # show progress
  if (len.queens = n)
    prn "solution! " queens
    # else
    let row (if queens (queens.zero.zero + 1) 0)
      for col 0 (col < n) ++col
        let new_queens (cons (list row col) queens)
          if (no conflicts.new_queens)
            (nqueens n new_queens)

# check if the first queen in 'queens' lies on the same column or diagonal as
# any of the others
def (conflicts queens)
  let (curr ... rest) queens
    or (let curr_column curr.1
         (some (fn(_) (= _ curr_column))
               (map cadr rest)))  # columns
       (some (fn(_) (diagonal_match curr _))
             rest)

def (diagonal_match curr other)
  (= (abs (curr.0 - other.0))
     (abs (curr.1 - other.1)))
```



## Xanadu


Copied from http://www.cs.bu.edu/~hwxi/Xanadu/Examples/

```Xanadu

int abs(i: int) {
    if (i >= 0) return i; else return -i;
}

unit print_dots(n: int) {
  while (n > 0) { print_string("."); n = n - 1; }
  return;
}

{size:int | 0 < size}
unit print_board (board[size]: int, size: int(size)) {
  var: int n, row;;

  invariant: [i:nat] (row: int(i))
  for (row = 0; row < size; row = row + 1) {
    n = board[row];
    print_dots(n-1);
    print_string("Q");
    print_dots(size - n);
    print_newline();
  }
    print_newline();
    return;
}

{size:int, j:int | 0 <= j < size}
bool test (j: int(j), board[size]: int) {
  var: int diff, i, qi, qj;;

  qj = board[j];

  invariant: [i:nat] (i: int(i))
  for (i = 0; i < j; i = i + 1) {
    qi = board[i]; diff = abs (qi - qj);
    if (diff == 0) { return false; } 
    else { if (diff == j - i) return false; }
  }
  return true;
}

{size:int | 0 < size}
nat queen(size: int(size)) {
  var: int board[], next, row; nat count;;

  count = 0; row = 0; board = alloc(size, 0);

  invariant: [n:nat | n < size] (row: int(n))
  while (true) {
    next = board[row]; next = next + 1;
    if (next > size) {
      if (row == 0) break; else { board[row] = 0; row = row - 1; }
    } else {
      board[row] = next;
      if (test(row, board)) {
        row = row + 1;
        if (row == size) {
          count = count + 1;
	  print_board(board, size);
          row = row - 1;
        }
      }
    }
  }
  return count;
}

int main () {
  return queen (8);
}
```



## XSLT

Below simple stylesheet does produce this output 
(either by XSLT processors saxon-6.5.5, xsltproc, xalan, 
or any of the big5 browsers):
<lang>
15863724
16837425
... 88 lines omitted ...
83162574
84136275

```


You can view the results directly in your browser (Chrome/FF/IE/Opera/Safari) here: [[http://stamm-wilbrandt.de/en/xsl-list/n-queens/8-queens.xsl.xml]]

This stylesheet is in category XSLT because it makes use or EXSLT [[http://exslt.org/]] exslt:node-set() extension function not available in XSLT 1.0

It is extracted from a bigger solution described in this blog posting: [[https://www.ibm.com/developerworks/mydeveloperworks/blogs/HermannSW/entry/n_queens_xsl_xml14]]
* determine all 500 n-queens solutions for 4<=n<=9
* determine distict solutions and totals
* display solutions graphically nicely
* with references to external .gif images [[http://stamm-wilbrandt.de/en/xsl-list/n-queens/n-queens.xsl.xml]]
* with internal "data:..." .gif images [[http://stamm-wilbrandt.de/en/xsl-list/n-queens/n-queens.internalImages.xsl.xml]]

This is the initial part of a screenshot from browser output:

[[image:n-queens.4-6.gif]]


Here is stylesheet 8-queens.xsl.xml which produces the (simple) output by having itself as input: [[http://stamm-wilbrandt.de/en/xsl-list/n-queens/8-queens.xsl.xml]]

```xml

<!-- 8-queens.xsl disguised as XML file for the browsers -->

<!-- Valery Chernysh's .xsl.xml technique for execution in all browsers -->
<?xml-stylesheet href="#" type="text/xsl"?>

<!-- alternative over specifying input in data:data section -->
<!DOCTYPE xsl:stylesheet [ 
  <!ENTITY N "8"> 
]>

<!-- this is the stylesheet being referenced by href="#" above -->
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:exslt="http://exslt.org/common"
  xmlns:n-queens="urn:n-queens"
  exclude-result-prefixes="n-queens exslt"
>
<!-- find David Carlisle's  exslt:node-set() for IE browsers at bottom -->

<!-- 
     Pattern allowing repeated processing of produced node-set results:
       <xsl:variable name="blah0">...</xsl:variable>
       <xsl:variable name="blah" select="exslt:node-set($blah0)"/>
-->
  <xsl:output omit-xml-declaration="yes"/>


  <!-- entry point -->
  <xsl:template match="/xsl:stylesheet">
    <!-- generate &N;x$&N;board -->
    <xsl:variable name="row0">
      <xsl:call-template name="n-queens:row">
        <xsl:with-param name="n" select="&N;"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="row" select="exslt:node-set($row0)"/>

    <xsl:variable name="rows0">
      <xsl:for-each select="$row/*">
        <r><xsl:copy-of select="$row"/></r>
      </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="rows" select="exslt:node-set($rows0)"/>

<html>
```txt

    <!-- determine all solutions of $N queens problem -->
    <xsl:call-template name="n-queens:search">
      <xsl:with-param name="b" select="$rows/*"/>
    </xsl:call-template>

```
</html>

  </xsl:template>


  <!-- recursive search for all solutions -->
  <xsl:template name="n-queens:search">    
    <xsl:param name="b"/>  <!-- remaining rows of not threatened fields -->
    <xsl:param name="s"/>  <!-- partial solution of queens fixated sofar -->

    <!-- complete board filled means solution found -->
    <xsl:if test="not($b)"> 
      <xsl:value-of select="$s"/><xsl:text>&#10;</xsl:text> 
    </xsl:if>

    <!-- check each remaining possible position in next row -->
    <xsl:for-each select="$b[1]/*">

      <!-- sieve out fields by new current (.) queen in current row -->
      <xsl:variable name="sieved0">
        <xsl:call-template name="n-queens:sieve">
          <xsl:with-param name="c" select="."/>
          <xsl:with-param name="b" select="$b[position()>1]"/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:variable name="sieved" select="exslt:node-set($sieved0)"/>

      <!-- recursive call -->
      <xsl:call-template name="n-queens:search">
        <xsl:with-param name="b" select="$sieved/*"/>
        <xsl:with-param name="s" select="concat($s, .)"/>
      </xsl:call-template>
    </xsl:for-each>
  </xsl:template>

  <!-- sieve out fields in remaining rows attacked by queen at column $c -->
  <xsl:template name="n-queens:sieve">    
    <xsl:param name="c"/>  <!-- column of newly fixed queen -->
    <xsl:param name="b"/>  <!-- remaining rows -->

    <xsl:for-each select="$b">
      <!-- row number for diagonal attack determination -->
      <xsl:variable name="r" select="position()"/>

      <!-- copy fields not vertically or diagonally attacked -->
      <r><xsl:copy-of select="*[. != $c][. - $r != $c][. + $r != $c]"/></r>
    </xsl:for-each>
  </xsl:template>

  <!-- generate node-set of the form "<f>1</f><f>2</f>...<f>$n</f>" -->
  <xsl:template name="n-queens:row">    
    <xsl:param name="n"/>

    <xsl:if test="$n>0">
      <xsl:call-template name="n-queens:row">
        <xsl:with-param name="n" select="$n - 1"/>
      </xsl:call-template>

      <f><xsl:value-of select="$n"/></f>
    </xsl:if>
  </xsl:template>


<!--
     IE browser exslt:node-set() (XSLT 1.0+), w/o msxsl pollution above

     from http://dpcarlisle.blogspot.com/2007/05/exslt-node-set-function.html
-->
<msxsl:script xmlns:msxsl="urn:schemas-microsoft-com:xslt"
              language="JScript" implements-prefix="exslt"
>
  this['node-set'] = function (x) {
    return x;
  } 
</msxsl:script>

</xsl:stylesheet>

```



## XPL0

[[File:NQueensXPL0.GIF|right]]

```XPL0
def     N=8;    \board size (NxN)
int     R, C;   \row and column of board
char    B(N,N); \board
include c:\cxpl\codes;

proc Try;       \Try adding a queen to the board
int  R;         \row, for each level of recursion

    func Okay;
    \Returns 'true' if no row, column, or diagonal from square R,C has a queen
    int I;
    [for I:= 0 to N-1 do
        [if B(I,C) then return false;                   \row is occupied
        if B(R,I) then return false;                    \column is occupied
        if R+I<N & C+I<N then
                if B(R+I, C+I) then return false;       \diagonal down right
        if R-I>=0 & C-I>=0 then
                if B(R-I, C-I) then return false;       \diagonal up left
        if R-I>=0 & C+I<N then
                if B(R-I, C+I) then return false;       \diagonal up right
        if R+I<N & C-I>=0 then
                if B(R+I, C-I) then return false;       \diagonal down left
        ];
    return true;
    ]; \Okay

[ \Try
if C>=N then
        [for R:= 0 to N-1 do                            \display solution
            [ChOut(0, ^ ); \(avoids scrolling up a color)
            for C:= 0 to N-1 do
                [Attrib(if (R|C)&1 then $0F else $4F);  \checkerboard pattern
                ChOut(6, if B(R,C) then $F2 else ^ );   \cute queen symbol
                ChOut(6, if B(R,C) then $F3 else ^ );
                ];
            CrLf(0);
            ];
        exit;                                           \one solution is enough
        ];
for R:= 0 to N-1 do
    [if Okay(R,C) then          \a queen can be placed here
        [B(R,C):= true;         \ so do it
        C:= C+1;                \move to next column
        Try;                    \ and try from there
        C:= C-1;                \didn't work: backup
        B(R,C):= false;         \undo queen placement
        ];
    ];
]; \Try


[for R:= 0 to N-1 do            \clear the board
    for C:= 0 to N-1 do
        B(R,C):= false;
C:= 0;                          \start at left column
Try;
]
```



## Yabasic


```Yabasic
DOCU The N Queens Problem:
DOCU Place N Queens on an NxN chess board
DOCU such that they don't threaten each other.

N = 8 // try some other sizes

sub threat(q1r, q1c, q2r, q2c)
// do two queens threaten each other?
    
   if q1c = q2c then    return true
   elsif (q1r - q1c) = (q2r - q2c) then return true
   elsif (q1r + q1c) = (q2r + q2c) then return true
   elsif q1r = q2r then    return true
   else return false
   end if
end sub

sub conflict(r, c, queens$)
// Would square p cause a conflict with other queens on board so far?
   local r2, c2

   for i = 1 to len(queens$) step 2
       r2 = val(mid$(queens$,i,1))
       c2 = val(mid$(queens$,i+1,1))
       if threat(r, c, r2, c2) then
            return true
       end if
    next i
   return false
end sub

sub print_board(queens$)
// print a solution, showing the Queens on the board
   local k$

   print at(1, 1);
   print "Solution #", soln, "\n\n   ";
   for c = asc("a") to (asc("a") + N - 1)
        print chr$(c)," ";
   next c
   print
   for r = 1 to N
        print r using "##"," ";
        for c = 1 to N
            pos = instr(queens$, (str$(r)+str$(c)))
            if pos and mod(pos, 2) then
                queens$ = mid$(queens$,pos)
                print "Q ";
            else
                print ". ";
            end if
        next c
        print
    next r
    print "\nPress Enter. (q to quit) "
    while(true)
        k$ = inkey$
        if lower$(k$) = "q" then
           exit
        elsif k$ = "enter" then
           break
        end if
    wend
end sub

```



## zkl

Modified from a Haskell version (if I remember correctly)

```zkl
fcn isAttacked(q, x,y) // ( (r,c), x,y ) : is queen at r,c attacked by q@(x,y)?
   { r,c:=q; (r==x or c==y or r+c==x+y or r-c==x-y) }
fcn isSafe(r,c,qs) // queen safe at (r,c)?, qs=( (r,c),(r,c)..) solution so far
   { ( not qs.filter1(isAttacked,r,c) ) }
fcn queensN(N=8,row=1,queens=T){
   qs:=[1..N].filter(isSafe.fpM("101",row,queens)) #isSafe(row,?,( (r,c),(r,c).. )
       .apply(fcn(c,r,qs){ qs.append(T(r,c)) },row,queens);
   if (row == N) return(qs);
   return(qs.apply(self.fcn.fp(N,row+1)).flatten());
}
```


```zkl
queens := queensN(4);
println(queens.len()," solution(s):");
queens.apply2(Console.println);
```

{{out}}

```txt

2 solution(s):
L(L(1,2),L(2,4),L(3,1),L(4,3))
L(L(1,3),L(2,1),L(3,4),L(4,2))

```


[[Category:Puzzles]]
