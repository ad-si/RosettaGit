+++
title = "Reduced row echelon form"
description = ""
date = 2019-07-05T14:17:50Z
aliases = []
[extra]
id = 3313
[taxonomies]
categories = []
tags = []
+++

{{wikipedia|Rref#Pseudocode}}
{{task|Matrices}}
{{omit from|GUISS}}


;Task:
Show how to compute the '''reduced row echelon form'''
(a.k.a. '''row canonical form''') of a matrix.

The matrix can be stored in any datatype that is convenient
(for most languages, this will probably be a two-dimensional array).

Built-in functions or this pseudocode (from Wikipedia) may be used:
 '''function''' ToReducedRowEchelonForm(Matrix M) '''is'''
     ''lead'' := 0
     ''rowCount'' := the number of rows in M
     ''columnCount'' := the number of columns in M
     '''for''' 0 &le; ''r'' < ''rowCount'' '''do'''
         '''if''' ''columnCount'' &le; ''lead'' '''then'''
             '''stop'''
         '''end if'''
         ''i'' = ''r''
         '''while''' M[''i'', ''lead''] = 0 '''do'''
             ''i'' = ''i'' + 1
             '''if''' ''rowCount'' = ''i'' '''then'''
                 ''i'' = ''r''
                 ''lead'' = ''lead'' + 1
                 '''if''' ''columnCount'' = ''lead'' '''then'''
                     '''stop'''
                 '''end if'''
             '''end if'''
         '''end while'''
         Swap rows ''i'' and ''r''
         If M[''r'', ''lead''] is not 0 divide row ''r'' by M[''r'', ''lead'']
         '''for''' 0 &le; ''i'' < ''rowCount'' '''do'''
             '''if''' ''i'' ≠ ''r'' '''do'''
                 Subtract M[i, lead] multiplied by row ''r'' from row ''i''
             '''end if'''
         '''end for'''
         ''lead'' = ''lead'' + 1
     '''end for'''
 '''end function'''

For testing purposes, the RREF of this matrix:

```txt

 1    2   -1   -4
 2    3   -1   -11
-2    0   -3    22

```

is:

```txt

 1    0    0   -8
 0    1    0    1
 0    0    1   -2

```






## 360 Assembly

{{trans|BBC BASIC}}

```360asm
*        reduced row echelon form  27/08/2015
RREF     CSECT
         USING  RREF,R12
         LR     R12,R15
         LA     R10,1              lead=1
         LA     R7,1
LOOPR    CH     R7,NROWS           do r=1 to nrows
         BH     ELOOPR
         CH     R10,NCOLS          if lead>=ncols
         BNL    ELOOPR
         LR     R8,R7              i=r
WHILE    LR     R1,R8              do while m(i,lead)=0
         BCTR   R1,0
         MH     R1,NCOLS
         LR     R6,R10             lead
         BCTR   R6,0
         AR     R1,R6
         SLA    R1,2
         L      R6,M(R1)           m(i,lead)
         LTR    R6,R6
         BNZ    EWHILE             m(i,lead)<>0
         LA     R8,1(R8)           i=i+1
         CH     R8,NROWS           if i=nrows
         BNE    EIF
         LR     R8,R7              i=r
         LA     R10,1(R10)         lead=lead+1
         CH     R10,NCOLS          if lead=ncols
         BE     ELOOPR
EIF      B      WHILE
EWHILE   LA     R9,1
LOOPJ1   CH     R9,NCOLS           do j=1 to ncols
         BH     ELOOPJ1
         LR     R1,R7              r
         BCTR   R1,0
         MH     R1,NCOLS
         LR     R6,R9              j
         BCTR   R6,0
         AR     R1,R6
         SLA    R1,2
         LA     R3,M(R1)           R3=@m(r,j)
         LR     R1,R8              i
         BCTR   R1,0
         MH     R1,NCOLS
         LR     R6,R9              j
         BCTR   R6,0
         AR     R1,R6
         SLA    R1,2
         LA     R4,M(R1)           R4=@m(i,j)
         L      R2,0(R3)
         MVC    0(2,R3),0(R4)      swap m(i,j),m(r,j)
         ST     R2,0(R4)
         LA     R9,1(R9)           j=j+1
         B      LOOPJ1
ELOOPJ1  LR     R1,R7              r
         BCTR   R1,0
         MH     R1,NCOLS
         LR     R6,R10             lead
         BCTR   R6,0
         AR     R1,R6
         SLA    R1,2
         L      R11,M(R1)          n=m(r,lead)
         CH     R11,=H'1'          if n^=1
         BE     ELOOPJ2
         LA     R9,1
LOOPJ2   CH     R9,NCOLS           do j=1 to ncols
         BH     ELOOPJ2
         LR     R1,R7              r
         BCTR   R1,0
         MH     R1,NCOLS
         LR     R6,R9              j
         BCTR   R6,0
         AR     R1,R6
         SLA    R1,2
         LA     R5,M(R1)           R5=@m(i,j)
         L      R2,0(R5)           m(r,j)
         LR     R1,R11             n
         SRDA   R2,32
         DR     R2,R1              m(r,j)/n
         ST     R3,0(R5)           m(r,j)=m(r,j)/n
         LA     R9,1(R9)           j=j+1
         B      LOOPJ2
ELOOPJ2  LA     R8,1
LOOPI3   CH     R8,NROWS           do i=1 to nrows
         BH     ELOOPI3
         CR     R8,R7              if i^=r
         BE     ELOOPJ3
         LR     R1,R8              i
         BCTR   R1,0
         MH     R1,NCOLS
         LR     R6,R10             lead
         BCTR   R6,0
         AR     R1,R6
         SLA    R1,2
         L      R11,M(R1)          n=m(i,lead)
         LA     R9,1
LOOPJ3   CH     R9,NCOLS           do j=1 to ncols
         BH     ELOOPJ3
         LR     R1,R8              i
         BCTR   R1,0
         MH     R1,NCOLS
         LR     R6,R9              j
         BCTR   R6,0
         AR     R1,R6
         SLA    R1,2
         LA     R4,M(R1)           R4=@m(i,j)
         L      R5,0(R4)           m(i,j)
         LR     R1,R7              r
         BCTR   R1,0
         MH     R1,NCOLS
         LR     R6,R9              j
         BCTR   R6,0
         AR     R1,R6
         SLA    R1,2
         L      R3,M(R1)           m(r,j)
         MR     R2,R11             m(r,j)*n
         SR     R5,R3              m(i,j)-m(r,j)*n
         ST     R5,0(R4)           m(i,j)=m(i,j)-m(r,j)*n
         LA     R9,1(R9)           j=j+1
         B      LOOPJ3
ELOOPJ3  LA     R8,1(R8)           i=i+1
         B      LOOPI3
ELOOPI3  LA     R10,1(R10)         lead=lead+1
         LA     R7,1(R7)           r=r+1
         B      LOOPR
ELOOPR   LA     R8,1
LOOPI4   CH     R8,NROWS           do i=1 to nrows
         BH     ELOOPI4
         SR     R10,R10            pgi=0
         LA     R9,1
LOOPJ4   CH     R9,NCOLS           do j=1 to ncols
         BH     ELOOPJ4
         LR     R1,R8              i
         BCTR   R1,0
         MH     R1,NCOLS
         LR     R6,R9              j
         BCTR   R6,0
         AR     R1,R6
         SLA    R1,2
         L      R6,M(R1)           m(i,j)
         LA     R3,PG
         AR     R3,R10
         XDECO  R6,0(R3)           edit m(i,j)
         LA     R10,12(10)         pgi=pgi+12
         LA     R9,1(R9)           j=j+1
         B      LOOPJ4
ELOOPJ4  XPRNT  PG,48              print m(i,j)
         LA     R8,1(R8)           i=i+1
         B      LOOPI4
ELOOPI4  XR     R15,R15
         BR     R14
NROWS    DC     H'3'
NCOLS    DC     H'4'
M        DC     F'1',F'2',F'-1',F'-4'
         DC     F'2',F'3',F'-1',F'-11'
         DC     F'-2',F'0',F'-3',F'22'
PG       DC     CL48' '
         YREGS
         END    RREF
```

{{out}}

```txt

           1           0           0          -8
           0           1           0           1
           0           0           1          -2

```



## ActionScript

_m being of type Vector.<Vector.<Number>> the following function
is a method of Matrix class.
Therefore return this statements are returning the Matrix object itself.


```Actionscript
public function RREF():Matrix {
   var lead:uint, i:uint, j:uint, r:uint = 0;

   for(r = 0; r < rows; r++) {
      if(columns <= lead)
         break;
      i = r;

      while(_m[i][lead] == 0) {
         i++;

         if(rows == i) {
            i = r;
            lead++;

            if(columns == lead)
               return this;
         }
      }
      rowSwitch(i, r);
      var val:Number = _m[r][lead];

      for(j = 0; j < columns; j++)
         _m[r][j] /= val;

      for(i = 0; i < rows; i++) {
         if(i == r)
            continue;
         val = _m[i][lead];

         for(j = 0; j < columns; j++)
            _m[i][j] -= val * _m[r][j];
      }
      lead++;
   }
   return this;
}
```



## Ada

matrices.ads:

```Ada
generic
   type Element_Type is private;
   Zero : Element_Type;
   with function "-" (Left, Right : in Element_Type) return Element_Type is <>;
   with function "*" (Left, Right : in Element_Type) return Element_Type is <>;
   with function "/" (Left, Right : in Element_Type) return Element_Type is <>;
package Matrices is
   type Matrix is
     array (Positive range <>, Positive range <>) of Element_Type;
   function Reduced_Row_Echelon_form (Source : Matrix) return Matrix;
end Matrices;
```


matrices.adb:

```Ada
package body Matrices is
   procedure Swap_Rows (From : in out Matrix; First, Second : in Positive) is
      Temporary : Element_Type;
   begin
      for Col in From'Range (2) loop
         Temporary          := From (First, Col);
         From (First, Col)  := From (Second, Col);
         From (Second, Col) := Temporary;
      end loop;
   end Swap_Rows;

   procedure Divide_Row
     (From    : in out Matrix;
      Row     : in Positive;
      Divisor : in Element_Type)
   is
   begin
      for Col in From'Range (2) loop
         From (Row, Col) := From (Row, Col) / Divisor;
      end loop;
   end Divide_Row;

   procedure Subtract_Rows
     (From                : in out Matrix;
      Subtrahend, Minuend : in Positive;
      Factor              : in Element_Type)
   is
   begin
      for Col in From'Range (2) loop
         From (Minuend, Col) := From (Minuend, Col) -
                                From (Subtrahend, Col) * Factor;
      end loop;
   end Subtract_Rows;

   function Reduced_Row_Echelon_form (Source : Matrix) return Matrix is
      Result : Matrix   := Source;
      Lead   : Positive := Result'First (2);
      I      : Positive;
   begin
      Rows : for Row in Result'Range (1) loop
         exit Rows when Lead > Result'Last (2);
         I := Row;
         while Result (I, Lead) = Zero loop
            I := I + 1;
            if I = Result'Last (1) then
               I    := Row;
               Lead := Lead + 1;
               exit Rows when Lead = Result'Last (2);
            end if;
         end loop;
         if I /= Row then
            Swap_Rows (From => Result, First => I, Second => Row);
         end if;
         Divide_Row
           (From    => Result,
            Row     => Row,
            Divisor => Result (Row, Lead));
         for Other_Row in Result'Range (1) loop
            if Other_Row /= Row then
               Subtract_Rows
                 (From       => Result,
                  Subtrahend => Row,
                  Minuend    => Other_Row,
                  Factor     => Result (Other_Row, Lead));
            end if;
         end loop;
         Lead := Lead + 1;
      end loop Rows;
      return Result;
   end Reduced_Row_Echelon_form;
end Matrices;
```


Example use: main.adb:

```Ada
with Matrices;
with Ada.Text_IO;
procedure Main is
   package Float_IO is new Ada.Text_IO.Float_IO (Float);
   package Float_Matrices is new Matrices (
      Element_Type => Float,
      Zero => 0.0);
   procedure Print_Matrix (Matrix : in Float_Matrices.Matrix) is
   begin
      for Row in Matrix'Range (1) loop
         for Col in Matrix'Range (2) loop
            Float_IO.Put (Matrix (Row, Col), 0, 0, 0);
            Ada.Text_IO.Put (' ');
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Print_Matrix;
   My_Matrix : Float_Matrices.Matrix :=
     ((1.0, 2.0, -1.0, -4.0),
      (2.0, 3.0, -1.0, -11.0),
      (-2.0, 0.0, -3.0, 22.0));
   Reduced   : Float_Matrices.Matrix :=
      Float_Matrices.Reduced_Row_Echelon_form (My_Matrix);
begin
   Print_Matrix (My_Matrix);
   Ada.Text_IO.Put_Line ("reduced to:");
   Print_Matrix (Reduced);
end Main;
```


{{out}}

```txt
1.0 2.0 -1.0 -4.0
2.0 3.0 -1.0 -11.0
-2.0 0.0 -3.0 22.0
reduced to:
1.0 0.0 0.0 -8.0
-0.0 1.0 0.0 1.0
-0.0 -0.0 1.0 -2.0
```



## Aime


```aime
rref(list l, integer rows, columns)
{
    integer e, f, i, j, lead, r;
    list u, v;

    lead = r = 0;
    while (r < rows && lead < columns) {
        i = r;
        while (!l.q_list(i)[lead]) {
            i += 1;
            if (i == rows) {
                i = r;
                lead += 1;
                if (lead == columns) {
                    break;
                }
            }
        }
        if (lead == columns) {
            break;
        }

        u = l[i];

        l.spin(i, r);
        e = u[lead];
        if (e) {
            for (j, f in u) {
                u[j] = f / e;
            }
        }

        for (i, v in l) {
            if (i != r) {
                e = v[lead];
                for (j, f in v) {
                    v[j] = f - u[j] * e;
                }
            }
        }

        lead += 1;

        r += 1;
    }
}

display_2(list l)
{
    for (, list u in l) {
        u.ucall(o_winteger, -1, 4);
        o_byte('\n');
    }
}

main(void)
{
    list l;

    l = list(list(1, 2, -1, -4),
             list(2, 3, -1, -11),
             list(-2, 0, -3, 22));
    rref(l, 3, 4);
    display_2(l);

    0;
}
```

{{Out}}

```txt
   1   0   0  -8
   0   1   0   1
   0   0   1  -2
```



## ALGOL 68

{{trans|Python}}
{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
<!-- {{does not work with|ELLA ALGOL 68|Any (with appropriate job cards AND formatted transput statements removed) - tested with release 1.8.8d.fc9.i386 - ELLA has not FORMATted transput, also it generates a call to undefined C external }} -->

```algol68
MODE FIELD = REAL; # FIELD can be REAL, LONG REAL etc, or COMPL, FRAC etc #
MODE VEC = [0]FIELD;
MODE MAT = [0,0]FIELD;

PROC to reduced row echelon form = (REF MAT m)VOID: (
    INT lead col := 2 LWB m;

    FOR this row FROM LWB m TO UPB m DO
        IF lead col > 2 UPB m THEN return FI;
        INT other row := this row;
        WHILE m[other row,lead col] = 0 DO
            other row +:= 1;
            IF other row > UPB m THEN
                other row := this row;
                lead col +:= 1;
                IF lead col > 2 UPB m THEN return FI
            FI
        OD;
        IF this row /= other row THEN
            VEC swap = m[this row,lead col:];
            m[this row,lead col:] := m[other row,lead col:];
            m[other row,lead col:] := swap
        FI;
        FIELD scale = 1/m[this row,lead col];
        IF scale /= 1 THEN
            m[this row,lead col] := 1;
            FOR col FROM lead col+1 TO 2 UPB m DO m[this row,col] *:= scale OD
        FI;
        FOR other row FROM LWB m TO UPB m DO
            IF this row /= other row THEN
                REAL scale = m[other row,lead col];
                m[other row,lead col]:=0;
                FOR col FROM lead col+1 TO 2 UPB m DO m[other row,col] -:= scale*m[this row,col] OD
            FI
        OD;
        lead col +:= 1
    OD;
    return: EMPTY
);

[3,4]FIELD mat := (
   ( 1, 2, -1, -4),
   ( 2, 3, -1, -11),
   (-2, 0, -3, 22)
);

to reduced row echelon form( mat );

FORMAT
  real repr = $g(-7,4)$,
  vec repr = $"("n(2 UPB mat-1)(f(real repr)", ")f(real repr)")"$,
  mat repr = $"("n(1 UPB mat-1)(f(vec repr)", "lx)f(vec repr)")"$;

printf((mat repr, mat, $l$))
```

{{out}}

```txt

(( 1.0000,  0.0000,  0.0000, -8.0000),
 ( 0.0000,  1.0000,  0.0000,  1.0000),
 ( 0.0000,  0.0000,  1.0000, -2.0000))

```



## AutoIt


```AutoIt

Global $ivMatrix[3][4] = [[1, 2, -1, -4],[2, 3, -1, -11],[-2, 0, -3, 22]]
ToReducedRowEchelonForm($ivMatrix)

Func ToReducedRowEchelonForm($matrix)
	Local $clonematrix, $i
	Local $lead = 0
	Local $rowCount = UBound($matrix) - 1
	Local $columnCount = UBound($matrix, 2) - 1
	For $r = 0 To $rowCount
		If $columnCount = $lead Then ExitLoop
		$i = $r
		While $matrix[$i][$lead] = 0
			$i += 1
			If $rowCount = $i Then
				$i = $r
				$lead += 1
				If $columnCount = $lead Then ExitLoop
			EndIf
		WEnd
		; There´s no built in Function to swap Rows of a 2-Dimensional Array
		; We need to clone our matrix to swap complete lines
		$clonematrix = $matrix ; Swap Lines, no
		For $s = 0 To $columnCount
			$matrix[$r][$s] = $clonematrix[$i][$s]
			$matrix[$i][$s] = $clonematrix[$r][$s]
		Next
		Local $m = $matrix[$r][$lead]
		For $k = 0 To $columnCount
			$matrix[$r][$k] = $matrix[$r][$k] / $m
		Next
		For $i = 0 To $rowCount
			If $i <> $r Then
				Local $m = $matrix[$i][$lead]
				For $k = 0 To $columnCount
					$matrix[$i][$k] -= $m * $matrix[$r][$k]
				Next
			EndIf
		Next
		$lead += 1
	Next
	; Console Output
	For $i = 0 To $rowCount
		ConsoleWrite("[")
		For $k = 0 To $columnCount
			ConsoleWrite($matrix[$i][$k])
			If $k <> $columnCount Then ConsoleWrite(",")
		Next
		ConsoleWrite("]" & @CRLF)
	Next
	; End of Console Output
	Return $matrix
EndFunc   ;==>ToReducedRowEchelonForm

```

{{out}}

```txt
[1,0,0,-8]
[-0,1,0,1]
[-0,-0,1,-2]
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      DIM matrix(2,3)
      matrix() = 1, 2, -1, -4, \
      \          2, 3, -1, -11, \
      \         -2, 0, -3, 22
      PROCrref(matrix())
      FOR row% = 0 TO 2
        FOR col% = 0 TO 3
          PRINT matrix(row%,col%);
        NEXT
        PRINT
      NEXT row%
      END

      DEF PROCrref(m())
      LOCAL lead%, nrows%, ncols%, i%, j%, r%, n
      nrows% = DIM(m(),1)+1
      ncols% = DIM(m(),2)+1
      FOR r% = 0 TO nrows%-1
        IF lead% >= ncols% EXIT FOR
        i% = r%
        WHILE m(i%,lead%) = 0
          i% += 1
          IF i% = nrows% THEN
            i% = r%
            lead% += 1
            IF lead% = ncols% EXIT FOR
          ENDIF
        ENDWHILE
        FOR j% = 0 TO ncols%-1 : SWAP m(i%,j%),m(r%,j%) : NEXT
        n = m(r%,lead%)
        IF n <> 0 FOR j% = 0 TO ncols%-1 : m(r%,j%) /= n : NEXT
        FOR i% = 0 TO nrows%-1
          IF i% <> r% THEN
            n = m(i%,lead%)
            FOR j% = 0 TO ncols%-1
              m(i%,j%) -= m(r%,j%) * n
            NEXT
          ENDIF
        NEXT
        lead% += 1
      NEXT r%
      ENDPROC
```

{{out}}

```txt

         1         0         0        -8
         0         1         0         1
         0         0         1        -2

```



## C


```c
#include <stdio.h>
#define TALLOC(n,typ) malloc(n*sizeof(typ))

#define EL_Type int

typedef struct sMtx {
    int     dim_x, dim_y;
    EL_Type *m_stor;
    EL_Type **mtx;
} *Matrix, sMatrix;

typedef struct sRvec {
    int     dim_x;
    EL_Type *m_stor;
} *RowVec, sRowVec;

Matrix NewMatrix( int x_dim, int y_dim )
{
    int n;
    Matrix m;
    m = TALLOC( 1, sMatrix);
    n = x_dim * y_dim;
    m->dim_x = x_dim;
    m->dim_y = y_dim;
    m->m_stor = TALLOC(n, EL_Type);
    m->mtx = TALLOC(m->dim_y, EL_Type *);
    for(n=0; n<y_dim; n++) {
        m->mtx[n] = m->m_stor+n*x_dim;
    }
    return m;
}

void MtxSetRow(Matrix m, int irow, EL_Type *v)
{
    int ix;
    EL_Type *mr;
    mr = m->mtx[irow];
    for(ix=0; ix<m->dim_x; ix++)
        mr[ix] = v[ix];
}

Matrix InitMatrix( int x_dim, int y_dim, EL_Type **v)
{
    Matrix m;
    int iy;
    m = NewMatrix(x_dim, y_dim);
    for (iy=0; iy<y_dim; iy++)
        MtxSetRow(m, iy, v[iy]);
    return m;
}

void MtxDisplay( Matrix m )
{
    int iy, ix;
    const char *sc;
    for (iy=0; iy<m->dim_y; iy++) {
        printf("   ");
        sc = " ";
        for (ix=0; ix<m->dim_x; ix++) {
            printf("%s %3d", sc, m->mtx[iy][ix]);
            sc = ",";
        }
        printf("\n");
    }
    printf("\n");
}

void MtxMulAndAddRows(Matrix m, int ixrdest, int ixrsrc, EL_Type mplr)
{
    int ix;
    EL_Type *drow, *srow;
    drow = m->mtx[ixrdest];
    srow = m->mtx[ixrsrc];
    for (ix=0; ix<m->dim_x; ix++)
        drow[ix] += mplr * srow[ix];
//	printf("Mul row %d by %d and add to row %d\n", ixrsrc, mplr, ixrdest);
//	MtxDisplay(m);
}

void MtxSwapRows( Matrix m, int rix1, int rix2)
{
    EL_Type *r1, *r2, temp;
    int ix;
    if (rix1 == rix2) return;
    r1 = m->mtx[rix1];
    r2 = m->mtx[rix2];
    for (ix=0; ix<m->dim_x; ix++)
        temp = r1[ix]; r1[ix]=r2[ix]; r2[ix]=temp;
//	printf("Swap rows %d and %d\n", rix1, rix2);
//	MtxDisplay(m);
}

void MtxNormalizeRow( Matrix m, int rix, int lead)
{
    int ix;
    EL_Type *drow;
    EL_Type lv;
    drow = m->mtx[rix];
    lv = drow[lead];
    for (ix=0; ix<m->dim_x; ix++)
        drow[ix] /= lv;
//	printf("Normalize row %d\n", rix);
//	MtxDisplay(m);
}

#define MtxGet( m, rix, cix ) m->mtx[rix][cix]

void MtxToReducedREForm(Matrix m)
{
    int lead;
    int rix, iix;
    EL_Type lv;
    int rowCount = m->dim_y;

    lead = 0;
    for (rix=0; rix<rowCount; rix++) {
        if (lead >= m->dim_x)
            return;
        iix = rix;
        while (0 == MtxGet(m, iix,lead)) {
            iix++;
            if (iix == rowCount) {
                iix = rix;
                lead++;
                if (lead == m->dim_x)
                    return;
            }
        }
        MtxSwapRows(m, iix, rix );
        MtxNormalizeRow(m, rix, lead );
        for (iix=0; iix<rowCount; iix++) {
            if ( iix != rix ) {
                lv = MtxGet(m, iix, lead );
                MtxMulAndAddRows(m,iix, rix, -lv) ;
            }
        }
        lead++;
    }
}

int main()
{
    Matrix m1;
    static EL_Type r1[] = {1,2,-1,-4};
    static EL_Type r2[] = {2,3,-1,-11};
    static EL_Type r3[] = {-2,0,-3,22};
    static EL_Type *im[] = { r1, r2, r3 };

    m1 = InitMatrix( 4,3, im );
    printf("Initial\n");
    MtxDisplay(m1);
    MtxToReducedREForm(m1);
    printf("Reduced R-E form\n");
    MtxDisplay(m1);
    return 0;
}
```



## C++

Note: This code is written in generic form. While it slightly complicates the code, it allows to use the same code for both built-in arrays and matrix classes. To use it with a matrix class, either program the matrix class to the specifications given in the matrix_traits comment, or specialize matrix_traits for the specific interface of your matrix class.

The test code uses a built-in array for the matrix.

{{works with|g++|4.1.2 20061115 (prerelease) (Debian 4.1.1-21)}}

```cpp
#include <algorithm> // for std::swap
#include <cstddef>
#include <cassert>

// Matrix traits: This describes how a matrix is accessed. By
// externalizing this information into a traits class, the same code
// can be used both with native arrays and matrix classes. To use the
// default implementation of the traits class, a matrix type has to
// provide the following definitions as members:
//
// * typedef ... index_type;
//   - The type used for indexing (e.g. size_t)
// * typedef ... value_type;
//   - The element type of the matrix (e.g. double)
// * index_type min_row() const;
//   - returns the minimal allowed row index
// * index_type max_row() const;
//   - returns the maximal allowed row index
// * index_type min_column() const;
//   - returns the minimal allowed column index
// * index_type max_column() const;
//   - returns the maximal allowed column index
// * value_type& operator()(index_type i, index_type k)
//   - returns a reference to the element i,k, where
//     min_row() <= i <= max_row()
//     min_column() <= k <= max_column()
// * value_type operator()(index_type i, index_type k) const
//   - returns the value of element i,k
//
// Note that the functions are all inline and simple, so the compiler
// should completely optimize them away.
template<typename MatrixType> struct matrix_traits
{
  typedef typename MatrixType::index_type index_type;
  typedef typename MatrixType::value_type value_type;
  static index_type min_row(MatrixType const& A)
  { return A.min_row(); }
  static index_type max_row(MatrixType const& A)
  { return A.max_row(); }
  static index_type min_column(MatrixType const& A)
  { return A.min_column(); }
  static index_type max_column(MatrixType const& A)
  { return A.max_column(); }
  static value_type& element(MatrixType& A, index_type i, index_type k)
  { return A(i,k); }
  static value_type element(MatrixType const& A, index_type i, index_type k)
  { return A(i,k); }
};

// specialization of the matrix traits for built-in two-dimensional
// arrays
template<typename T, std::size_t rows, std::size_t columns>
 struct matrix_traits<T[rows][columns]>
{
  typedef std::size_t index_type;
  typedef T value_type;
  static index_type min_row(T const (&)[rows][columns])
  { return 0; }
  static index_type max_row(T const (&)[rows][columns])
  { return rows-1; }
  static index_type min_column(T const (&)[rows][columns])
  { return 0; }
  static index_type max_column(T const (&)[rows][columns])
  { return columns-1; }
  static value_type& element(T (&A)[rows][columns],
                             index_type i, index_type k)
  { return A[i][k]; }
  static value_type element(T const (&A)[rows][columns],
                            index_type i, index_type k)
  { return A[i][k]; }
};

// Swap rows i and k of a matrix A
// Note that due to the reference, both dimensions are preserved for
// built-in arrays
template<typename MatrixType>
 void swap_rows(MatrixType& A,
                 typename matrix_traits<MatrixType>::index_type i,
                 typename matrix_traits<MatrixType>::index_type k)
{
  matrix_traits<MatrixType> mt;
  typedef typename matrix_traits<MatrixType>::index_type index_type;

  // check indices
  assert(mt.min_row(A) <= i);
  assert(i <= mt.max_row(A));

  assert(mt.min_row(A) <= k);
  assert(k <= mt.max_row(A));

  for (index_type col = mt.min_column(A); col <= mt.max_column(A); ++col)
    std::swap(mt.element(A, i, col), mt.element(A, k, col));
}

// divide row i of matrix A by v
template<typename MatrixType>
 void divide_row(MatrixType& A,
                  typename matrix_traits<MatrixType>::index_type i,
                  typename matrix_traits<MatrixType>::value_type v)
{
  matrix_traits<MatrixType> mt;
  typedef typename matrix_traits<MatrixType>::index_type index_type;

  assert(mt.min_row(A) <= i);
  assert(i <= mt.max_row(A));

  assert(v != 0);

  for (index_type col = mt.min_column(A); col <= mt.max_column(A); ++col)
    mt.element(A, i, col) /= v;
}

// in matrix A, add v times row k to row i
template<typename MatrixType>
 void add_multiple_row(MatrixType& A,
                  typename matrix_traits<MatrixType>::index_type i,
                  typename matrix_traits<MatrixType>::index_type k,
                  typename matrix_traits<MatrixType>::value_type v)
{
  matrix_traits<MatrixType> mt;
  typedef typename matrix_traits<MatrixType>::index_type index_type;

  assert(mt.min_row(A) <= i);
  assert(i <= mt.max_row(A));

  assert(mt.min_row(A) <= k);
  assert(k <= mt.max_row(A));

  for (index_type col = mt.min_column(A); col <= mt.max_column(A); ++col)
    mt.element(A, i, col) += v * mt.element(A, k, col);
}

// convert A to reduced row echelon form
template<typename MatrixType>
 void to_reduced_row_echelon_form(MatrixType& A)
{
  matrix_traits<MatrixType> mt;
  typedef typename matrix_traits<MatrixType>::index_type index_type;

  index_type lead = mt.min_row(A);

  for (index_type row = mt.min_row(A); row <= mt.max_row(A); ++row)
  {
    if (lead > mt.max_column(A))
      return;
    index_type i = row;
    while (mt.element(A, i, lead) == 0)
    {
      ++i;
      if (i > mt.max_row(A))
      {
        i = row;
        ++lead;
        if (lead > mt.max_column(A))
          return;
      }
    }
    swap_rows(A, i, row);
    divide_row(A, row, mt.element(A, row, lead));
    for (i = mt.min_row(A); i <= mt.max_row(A); ++i)
    {
      if (i != row)
        add_multiple_row(A, i, row, -mt.element(A, i, lead));
    }
  }
}

// test code
#include <iostream>

int main()
{
  double M[3][4] = { {  1, 2, -1,  -4 },
                     {  2, 3, -1, -11 },
                     { -2, 0, -3,  22 } };

  to_reduced_row_echelon_form(M);
  for (int i = 0; i < 3; ++i)
  {
    for (int j = 0; j < 4; ++j)
      std::cout << M[i][j] << '\t';
    std::cout << "\n";
  }

  return EXIT_SUCCESS;
}
```

{{out}}

```txt

1       0       0       -8
-0      1       0       1
-0      -0      1       -2

```


=={{header|C sharp|C#}}==

```csharp
using System;

namespace rref
{
    class Program
    {
        static void Main(string[] args)
        {
            int[,] matrix = new int[3, 4]{
                {  1, 2, -1,  -4 },
                {  2, 3, -1, -11 },
                { -2, 0, -3,  22 }
            };
            matrix = rref(matrix);
        }

        private static int[,] rref(int[,] matrix)
        {
            int lead = 0, rowCount = matrix.GetLength(0), columnCount = matrix.GetLength(1);
            for (int r = 0; r < rowCount; r++)
            {
                if (columnCount <= lead) break;
                int i = r;
                while (matrix[i, lead] == 0)
                {
                    i++;
                    if (i == rowCount)
                    {
                        i = r;
                        lead++;
                        if (columnCount == lead)
                        {
                        lead--;
                        break;
                        }
                    }
                }
                for (int j = 0; j < columnCount; j++)
                {
                    int temp = matrix[r, j];
                    matrix[r, j] = matrix[i, j];
                    matrix[i, j] = temp;
                }
                int div = matrix[r, lead];
                if(div != 0)
                    for (int j = 0; j < columnCount; j++) matrix[r, j] /= div;
                for (int j = 0; j < rowCount; j++)
                {
                    if (j != r)
                    {
                        int sub = matrix[j, lead];
                        for (int k = 0; k < columnCount; k++) matrix[j, k] -= (sub * matrix[r, k]);
                    }
                }
                lead++;
            }
            return matrix;
        }
    }
}
```



## Common Lisp

Direct implementation of the pseudo-code given.

```lisp
(defun convert-to-row-echelon-form (matrix)
  (let* ((dimensions (array-dimensions matrix))
	 (row-count (first dimensions))
	 (column-count (second dimensions))
	 (lead 0))
    (labels ((find-pivot (start lead)
	       (let ((i start))
		 (loop
		    :while (zerop (aref matrix i lead))
		    :do (progn
			  (incf i)
			  (when (= i row-count)
			    (setf i start)
			    (incf lead)
			    (when (= lead column-count)
			      (return-from convert-to-row-echelon-form matrix))))
		    :finally (return (values i lead)))))
	     (swap-rows (r1 r2)
	       (loop
		  :for c :upfrom 0 :below column-count
		  :do (rotatef (aref matrix r1 c) (aref matrix r2 c))))
	     (divide-row (r value)
	       (loop
		  :for c :upfrom 0 :below column-count
		  :do (setf (aref matrix r c)
			    (/ (aref matrix r c) value)))))
      (loop
	 :for r :upfrom 0 :below row-count
	 :when (<= column-count lead)
	 :do (return matrix)
	 :do (multiple-value-bind (i nlead) (find-pivot r lead)
	       (setf lead nlead)
	       (swap-rows i r)
	       (divide-row r (aref matrix r lead))
	       (loop
		  :for i :upfrom 0 :below row-count
		  :when (/= i r)
		  :do (let ((scale (aref matrix i lead)))
			(loop
			   :for c :upfrom 0 :below column-count
			   :do (decf (aref matrix i c)
				     (* scale (aref matrix r c))))))
	       (incf lead))
	 :finally (return matrix)))))
```



## D


```d
import std.stdio, std.algorithm, std.array, std.conv;

void toReducedRowEchelonForm(T)(T[][] M) pure nothrow @nogc {
    if (M.empty)
        return;
    immutable nrows = M.length;
    immutable ncols = M[0].length;

    size_t lead;
    foreach (immutable r; 0 .. nrows) {
        if (ncols <= lead)
            return;
        {
            size_t i = r;
            while (M[i][lead] == 0) {
                i++;
                if (nrows == i) {
                    i = r;
                    lead++;
                    if (ncols == lead)
                        return;
                }
            }
            swap(M[i], M[r]);
        }

        M[r][] /= M[r][lead];
        foreach (j, ref mj; M)
            if (j != r)
                mj[] -= M[r][] * mj[lead];
        lead++;
    }
}

void main() {
    auto A = [[ 1, 2, -1,  -4],
              [ 2, 3, -1, -11],
              [-2, 0, -3,  22]];

    A.toReducedRowEchelonForm;
    writefln("%(%(%2d %)\n%)", A);
}
```

{{out}}

```txt
 1  0  0 -8
 0  1  0  1
 0  0  1 -2
```



## Euphoria


```euphoria
function ToReducedRowEchelonForm(sequence M)
    integer lead,rowCount,columnCount,i
    sequence temp
    lead = 1
    rowCount = length(M)
    columnCount = length(M[1])
    for r = 1 to rowCount do
        if columnCount <= lead then
            exit
        end if
        i = r
        while M[i][lead] = 0 do
            i += 1
            if rowCount = i then
                i = r
                lead += 1
                if columnCount = lead then
                    exit
                end if
            end if
        end while
        temp = M[i]
        M[i] = M[r]
        M[r] = temp
        M[r] /= M[r][lead]
        for j = 1 to rowCount do
            if j != r then
                M[j] -= M[j][lead]*M[r]
            end if
        end for
        lead += 1
    end for
    return M
end function

? ToReducedRowEchelonForm(
    { { 1, 2, -1, -4 },
      { 2, 3, -1, -11 },
      { -2, 0, -3, 22 } })
```


{{out}}

```txt
{
  {1,0,0,-8},
  {0,1,0,1},
  {0,0,1,-2}
}
```



## Factor


```factor
USE: math.matrices.elimination
{ { 1 2 -1 -4 } { 2 3 -1 -11 } { -2 0 -3 22 } } solution .
```

{{out}}

```txt

{ { 1 0 0 -8 } { 0 1 0 1 } { 0 0 1 -2 } }

```



## Fortran


```fortran
module Rref
  implicit none
contains
  subroutine to_rref(matrix)
    real, dimension(:,:), intent(inout) :: matrix

    integer :: pivot, norow, nocolumn
    integer :: r, i
    real, dimension(:), allocatable :: trow

    pivot = 1
    norow = size(matrix, 1)
    nocolumn = size(matrix, 2)

    allocate(trow(nocolumn))

    do r = 1, norow
       if ( nocolumn <= pivot ) exit
       i = r
       do while ( matrix(i, pivot) == 0 )
          i = i + 1
          if ( norow == i ) then
             i = r
             pivot = pivot + 1
             if ( nocolumn == pivot ) return
          end if
       end do
       trow = matrix(i, :)
       matrix(i, :) = matrix(r, :)
       matrix(r, :) = trow
       matrix(r, :) = matrix(r, :) / matrix(r, pivot)
       do i = 1, norow
          if ( i /= r ) matrix(i, :) = matrix(i, :) - matrix(r, :) * matrix(i, pivot)
       end do
       pivot = pivot + 1
    end do
    deallocate(trow)
  end subroutine to_rref
end module Rref
```



```fortran
program prg_test
  use rref
  implicit none

  real, dimension(3, 4) :: m = reshape( (/  1, 2, -1, -4,  &
                                            2, 3, -1, -11, &
                                           -2, 0, -3,  22 /), &
                                        (/ 3, 4 /), order = (/ 2, 1 /) )
  integer :: i

  print *, "Original matrix"
  do i = 1, size(m,1)
     print *, m(i, :)
  end do

  call to_rref(m)

  print *, "Reduced row echelon form"
  do i = 1, size(m,1)
     print *, m(i, :)
  end do

end program prg_test
```



## Go


### 2D representation

From WP pseudocode:

```go
package main

import "fmt"

type matrix [][]float64

func (m matrix) print() {
    for _, r := range m {
        fmt.Println(r)
    }
    fmt.Println("")
}

func main() {
    m := matrix{
        { 1, 2, -1,  -4},
        { 2, 3, -1, -11},
        {-2, 0, -3,  22},
    }
    m.print()
    rref(m)
    m.print()
}

func rref(m matrix) {
    lead := 0
    rowCount := len(m)
    columnCount := len(m[0])
    for r := 0; r < rowCount; r++ {
        if lead >= columnCount {
            return
        }
        i := r
        for m[i][lead] == 0 {
            i++
            if rowCount == i {
                i = r
                lead++
                if columnCount == lead {
                    return
                }
            }
        }
        m[i], m[r] = m[r], m[i]
        f := 1 / m[r][lead]
        for j, _ := range m[r] {
            m[r][j] *= f
        }
        for i = 0; i < rowCount; i++ {
            if i != r {
                f = m[i][lead]
                for j, e := range m[r] {
                    m[i][j] -= e * f
                }
            }
        }
        lead++
    }
}
```

{{out}} (not so pretty, sorry)

```txt

[1 2 -1 -4]
[2 3 -1 -11]
[-2 0 -3 22]

[1 0 0 -8]
[-0 1 0 1]
[-0 -0 1 -2]

```



### Flat representation


```go
package main

import "fmt"

type matrix struct {
    ele    []float64
    stride int
}

func matrixFromRows(rows [][]float64) *matrix {
    if len(rows) == 0 {
        return &matrix{nil, 0}
    }
    m := &matrix{make([]float64, len(rows)*len(rows[0])), len(rows[0])}
    for rx, row := range rows {
        copy(m.ele[rx*m.stride:(rx+1)*m.stride], row)
    }
    return m
}

func (m *matrix) print(heading string) {
    if heading > "" {
        fmt.Print("\n", heading, "\n")
    }
    for e := 0; e < len(m.ele); e += m.stride {
        fmt.Printf("%6.2f ", m.ele[e:e+m.stride])
        fmt.Println()
    }
}

func (m *matrix) rref() {
    lead := 0
    for rxc0 := 0; rxc0 < len(m.ele); rxc0 += m.stride {
        if lead >= m.stride {
            return
        }
        ixc0 := rxc0
        for m.ele[ixc0+lead] == 0 {
            ixc0 += m.stride
            if ixc0 == len(m.ele) {
                ixc0 = rxc0
                lead++
                if lead == m.stride {
                    return
                }
            }
        }
        for c, ix, rx := 0, ixc0, rxc0; c < m.stride; c++ {
            m.ele[ix], m.ele[rx] = m.ele[rx], m.ele[ix]
            ix++
            rx++
        }
        if d := m.ele[rxc0+lead]; d != 0 {
            d := 1 / d
            for c, rx := 0, rxc0; c < m.stride; c++ {
                m.ele[rx] *= d
                rx++
            }
        }
        for ixc0 = 0; ixc0 < len(m.ele); ixc0 += m.stride {
            if ixc0 != rxc0 {
                f := m.ele[ixc0+lead]
                for c, ix, rx := 0, ixc0, rxc0; c < m.stride; c++ {
                    m.ele[ix] -= m.ele[rx] * f
                    ix++
                    rx++
                }
            }
        }
        lead++
    }
}

func main() {
    m := matrixFromRows([][]float64{
        {1, 2, -1, -4},
        {2, 3, -1, -11},
        {-2, 0, -3, 22},
    })
    m.print("Input:")
    m.rref()
    m.print("Reduced:")
}
```

{{out}}

```txt

Input:
[  1.00   2.00  -1.00  -4.00]
[  2.00   3.00  -1.00 -11.00]
[ -2.00   0.00  -3.00  22.00]

Reduced:
[  1.00   0.00   0.00  -8.00]
[ -0.00   1.00   0.00   1.00]
[ -0.00  -0.00   1.00  -2.00]

```



## Groovy

This solution implements the transformation to reduced row echelon form
with optional pivoting.
Options are provided for both ''partial pivoting'' and ''scaled partial pivoting''.
The default option is no pivoting at all.

```groovy
enum Pivoting {
    NONE({ i, it -> 1 }),
    PARTIAL({ i, it -> - (it[i].abs()) }),
    SCALED({ i, it -> - it[i].abs()/(it.inject(0) { sum, elt -> sum + elt.abs() } ) });

    public final Closure comparer

    private Pivoting(Closure c) {
        comparer = c
    }
}

def isReducibleMatrix = { matrix ->
    def m = matrix.size()
    m > 1 && matrix[0].size() > m && matrix[1..<m].every { row -> row.size() == matrix[0].size() }
}

def reducedRowEchelonForm = { matrix, Pivoting pivoting = Pivoting.NONE ->
    assert isReducibleMatrix(matrix)
    def m = matrix.size()
    def n = matrix[0].size()
    (0..<m).each { i ->
        matrix[i..<m].sort(pivoting.comparer.curry(i))
        matrix[i][i..<n] = matrix[i][i..<n].collect { it/matrix[i][i] }
        ((0..<i) + ((i+1)..<m)).each { k ->
            (i..<n).reverse().each { j ->
                matrix[k][j] -= matrix[i][j]*matrix[k][i]
            }
        }
    }
    matrix
}
```


This test first demonstrates the test case provided, and then demonstrates another test case designed to show the dangers of not using pivoting on an otherwise solvable matrix. Both test cases exercise all three pivoting options.

```groovy
def matrixCopy = { matrix -> matrix.collect { row -> row.collect { it } } }

println "Tests for matrix A:"
def a = [
    [1, 2, -1, -4],
    [2, 3, -1, -11],
    [-2, 0, -3, 22]
]
a.each { println it }
println()

println "pivoting == Pivoting.NONE"
reducedRowEchelonForm(matrixCopy(a)).each { println it }
println()
println "pivoting == Pivoting.PARTIAL"
reducedRowEchelonForm(matrixCopy(a), Pivoting.PARTIAL).each { println it }
println()
println "pivoting == Pivoting.SCALED"
reducedRowEchelonForm(matrixCopy(a), Pivoting.SCALED).each { println it }
println()


println "Tests for matrix B (divides by 0 without pivoting):"
def b = [
    [1, 2, -1, -4],
    [2, 4, -1, -11],
    [-2, 0, -6, 24]
]
b.each { println it }
println()

println "pivoting == Pivoting.NONE"
try {
    reducedRowEchelonForm(matrixCopy(b)).each { println it }
    println()
} catch (e) {
    println "KABOOM! ${e.message}"
    println()
}

println "pivoting == Pivoting.PARTIAL"
reducedRowEchelonForm(matrixCopy(b), Pivoting.PARTIAL).each { println it }
println()
println "pivoting == Pivoting.SCALED"
reducedRowEchelonForm(matrixCopy(b), Pivoting.SCALED).each { println it }
println()
```


{{out}}

```txt
Tests for matrix A:
[1, 2, -1, -4]
[2, 3, -1, -11]
[-2, 0, -3, 22]

pivoting == Pivoting.NONE
[1, 0, 0, -8]
[0, 1, 0, 1]
[0, 0, 1, -2]

pivoting == Pivoting.PARTIAL
[1, 0.0, 0E-11, -7.9999999997000000000150]
[0, 1, 0E-10, 0.999999999700000000010]
[0, 0.0, 1, -2.00000000030]

pivoting == Pivoting.SCALED
[1, 0, 0, -8]
[0, 1, 0, 1]
[0, 0, 1, -2]

Tests for matrix B (divides by 0 without pivoting):
[1, 2, -1, -4]
[2, 4, -1, -11]
[-2, 0, -6, 24]

pivoting == Pivoting.NONE
KABOOM! Division undefined

pivoting == Pivoting.PARTIAL
[1, 0, 0.00, -3.00]
[0, 1, 0.00, -2.00]
[0, 0, 1, -3]

pivoting == Pivoting.SCALED
[1, 0, 0, -3]
[0, 1, 0, -2]
[0, 0, 1, -3]
```



## Haskell

This program was produced by translating from the Python and gradually refactoring the result into a more functional style.


```haskell
import Data.List (find)

rref :: Fractional a => [[a]] -> [[a]]
rref m = f m 0 [0 .. rows - 1]
  where rows = length m
        cols = length $ head m

        f m _    []              = m
        f m lead (r : rs)
            | indices == Nothing = m
            | otherwise          = f m' (lead' + 1) rs
          where indices = find p l
                p (col, row) = m !! row !! col /= 0
                l = [(col, row) |
                    col <- [lead .. cols - 1],
                    row <- [r .. rows - 1]]

                Just (lead', i) = indices
                newRow = map (/ m !! i !! lead') $ m !! i

                m' = zipWith g [0..] $
                    replace r newRow $
                    replace i (m !! r) m
                g n row
                    | n == r    = row
                    | otherwise = zipWith h newRow row
                  where h = subtract . (* row !! lead')

replace :: Int -> a -> [a] -> [a]
{- Replaces the element at the given index. -}
replace n e l = a ++ e : b
  where (a, _ : b) = splitAt n l
```


=={{header|Icon}} and {{header|Unicon}}==

Works in both languages:

```unicon
procedure main(A)
    tM := [[  1,  2, -1, -4],
           [  2,  3, -1,-11],
           [ -2,  0, -3, 22]]
    showMat(rref(tM))
end

procedure rref(M)
    lead := 1
    rCount := *\M | stop("no Matrix?")
    cCount := *(M[1]) | 0
    every r := !rCount do {
        i := r
        while M[i,lead] = 0 do {
            if (i+:=1) > rCount then {
                i := r
                if cCount < (lead +:= 1) then stop("can't reduce")
                }
            }
        M[i] :=: M[r]
        if 0 ~= (m0 := M[r,lead]) then every !M[r] /:= real(m0)
        every r ~= (i := !rCount) do {
            every !(mr := copy(M[r])) *:= M[i,lead]
            every M[i,j := !cCount] -:= mr[j]
            }
        lead +:= 1
        }
    return M
end

procedure showMat(M)
    every r := !M do every writes(right(!r,5)||" " | "\n")
end
```


{{out}}

```txt

->rref
  1.0   0.0   0.0  -8.0
  0.0   1.0   0.0   1.0
  0.0   0.0   1.0  -2.0
->

```



## J

The reduced row echelon form of a matrix can be obtained using the <code>gauss_jordan</code> verb from the [http://www.jsoftware.com/wsvn/addons/trunk/math/misc/linear.ijs linear.ijs script], available as part of the <code>math/misc</code> addon. <code>gauss_jordan</code> and the verb <code>pivot</code> are shown below for completeness:

'''Solution:'''

```j
NB.*pivot v Pivot at row, column
NB. form: (row,col) pivot M
pivot=: dyad define
  'r c'=. x
  col=. c{"1 y
  y - (col - r = i.#y) */ (r{y) % r{col
)

NB.*gauss_jordan v Gauss-Jordan elimination (full pivoting)
NB. y is: matrix
NB. x is: optional minimum tolerance, default 1e_15.
NB.   If a column below the current pivot has numbers of magnitude all
NB.   less then x, it is treated as all zeros.
gauss_jordan=: verb define
  1e_15 gauss_jordan y
:
  mtx=. y
  'r c'=. $mtx
  rows=. i.r
  i=. j=. 0
  max=. i.>./
  while. (i<r) *. j<c do.
    k=. max col=. | i}. j{"1 mtx
    if. 0 < x-k{col do.           NB. if all col < tol, set to 0:
      mtx=. 0 (<(i}.rows);j) } mtx
    else.                         NB. otherwise sort and pivot:
      if. k do.
        mtx=. (<i,i+k) C. mtx
      end.
      mtx=. (i,j) pivot mtx
      i=. >:i
    end.
    j=. >:j
  end.
  mtx
)
```


'''Usage:'''

```j
   require 'math/misc/linear'
   ]A=: 1 2 _1 _4 , 2 3 _1 _11 ,: _2 0 _3 22
 1 2 _1  _4
 2 3 _1 _11
_2 0 _3  22

   gauss_jordan A
1 0 0 _8
0 1 0  1
0 0 1 _2
```


Additional examples, recommended on talk page:


```j

   gauss_jordan 2 0 _1  0  0,1 0  0 _1  0,3 0  0 _2 _1,0 1  0  0 _2,:0 1 _1  0  0
1 0 0 0 _1
0 1 0 0 _2
0 0 1 0 _2
0 0 0 1 _1
0 0 0 0  0
   gauss_jordan 1  2  3  4  3  1,2  4  6  2  6  2,3  6 18  9  9 _6,4  8 12 10 12  4,:5 10 24 11 15 _4
1 2 0 0 3 0
0 0 1 0 0 0
0 0 0 1 0 0
0 0 0 0 0 1
0 0 0 0 0 0
   gauss_jordan 0 1,1 2,:0 5
1 0
0 1
0 0
```


And:


```j
mat=: 0 ". ];._2 noun define
 1  0  0  0  0  0  1  0  0  0  0 _1  0  0  0  0  0  0
 1  0  0  0  0  0  0  1  0  0  0  0 _1  0  0  0  0  0
 1  0  0  0  0  0  0  0  1  0  0  0  0 _1  0  0  0  0
 0  1  0  0  0  0  1  0  0  0  0  0  0  0 _1  0  0  0
 0  1  0  0  0  0  0  0  1  0  0 _1  0  0  0  0  0  0
 0  1  0  0  0  0  0  0  0  0  1  0  0  0  0  0 _1  0
 0  0  1  0  0  0  1  0  0  0  0  0 _1  0  0  0  0  0
 0  0  1  0  0  0  0  0  0  1  0  0  0  0 _1  0  0  0
 0  0  0  1  0  0  0  1  0  0  0  0  0  0  0 _1  0  0
 0  0  0  1  0  0  0  0  0  1  0  0 _1  0  0  0  0  0
 0  0  0  0  1  0  0  1  0  0  0  0  0 _1  0  0  0  0
 0  0  0  0  1  0  0  0  1  0  0  0  0  0  0  0 _1  0
 0  0  0  0  1  0  0  0  0  0  1  0  0  0  0 _1  0  0
 0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0
 0  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0  0
 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  0  1
 0  0  0  0  0  1  0  0  0  0  1  0  0  0 _1  0  0  0
)
      gauss_jordan mat
1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.435897
0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.307692
0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.512821
0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0.717949
0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0.487179
0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0        0
0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0.205128
0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0.282051
0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0.333333
0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0        0
0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0.512821
0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0.641026
0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0.717949
0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0.769231
0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0.512821
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0        1
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0.820513
```



## Java

''This requires Apache Commons 2.2+''

```java
import java.util.*;
import java.lang.Math;
import org.apache.commons.math.fraction.Fraction;
import org.apache.commons.math.fraction.FractionConversionException;

/* Matrix class
 * Handles elementary Matrix operations:
 *	Interchange
 *	Multiply and Add
 *	Scale
 *	Reduced Row Echelon Form
 */
class Matrix {
	LinkedList<LinkedList<Fraction>> matrix;
	int numRows;
	int numCols;

	static class Coordinate {
		int row;
		int col;

		Coordinate(int r, int c) {
			row = r;
			col = c;
		}

		public String toString() {
			return "(" + row + ", " + col + ")";
		}
	}

	Matrix(double [][] m) {
		numRows = m.length;
		numCols = m[0].length;

		matrix = new LinkedList<LinkedList<Fraction>>();

		for (int i = 0; i < numRows; i++) {
			matrix.add(new LinkedList<Fraction>());
			for (int j = 0; j < numCols; j++) {
				try {
					matrix.get(i).add(new Fraction(m[i][j]));
				} catch (FractionConversionException e) {
					System.err.println("Fraction could not be converted from double by apache commons . . .");
				}
			}
		}
	}

	public void Interchange(Coordinate a, Coordinate b) {
		LinkedList<Fraction> temp = matrix.get(a.row);
		matrix.set(a.row, matrix.get(b.row));
		matrix.set(b.row, temp);

		int t = a.row;
		a.row = b.row;
		b.row = t;
	}

	public void Scale(Coordinate x, Fraction d) {
		LinkedList<Fraction> row = matrix.get(x.row);
		for (int i = 0; i < numCols; i++) {
			row.set(i, row.get(i).multiply(d));
		}
	}

	public void MultiplyAndAdd(Coordinate to, Coordinate from, Fraction scalar) {
		LinkedList<Fraction> row = matrix.get(to.row);
		LinkedList<Fraction> rowMultiplied = matrix.get(from.row);

		for (int i = 0; i < numCols; i++) {
			row.set(i, row.get(i).add((rowMultiplied.get(i).multiply(scalar))));
		}
	}

	public void RREF() {
		Coordinate pivot = new Coordinate(0,0);

		int submatrix = 0;
		for (int x = 0; x < numCols; x++) {
			pivot = new Coordinate(pivot.row, x);
			//Step 1
				//Begin with the leftmost nonzero column. This is a pivot column. The pivot position is at the top.
				for (int i = x; i < numCols; i++) {
					if (isColumnZeroes(pivot) == false) {
						break;
					} else {
						pivot.col = i;
					}
				}
			//Step 2
				//Select a nonzero entry in the pivot column with the highest absolute value as a pivot.
				pivot = findPivot(pivot);

				if (getCoordinate(pivot).doubleValue() == 0.0) {
					pivot.row++;
					continue;
				}

				//If necessary, interchange rows to move this entry into the pivot position.
				//move this row to the top of the submatrix
				if (pivot.row != submatrix) {
					Interchange(new Coordinate(submatrix, pivot.col), pivot);
				}

				//Force pivot to be 1
				if (getCoordinate(pivot).doubleValue() != 1) {
					/*
					System.out.println(getCoordinate(pivot));
					System.out.println(pivot);
					System.out.println(matrix);
					*/
					Fraction scalar = getCoordinate(pivot).reciprocal();
					Scale(pivot, scalar);
				}
			//Step 3
				//Use row replacement operations to create zeroes in all positions below the pivot.
				//belowPivot = belowPivot + (Pivot * -belowPivot)
				for (int i = pivot.row; i < numRows; i++) {
					if (i == pivot.row) {
						continue;
					}
					Coordinate belowPivot = new Coordinate(i, pivot.col);
					Fraction complement = (getCoordinate(belowPivot).negate().divide(getCoordinate(pivot)));
					MultiplyAndAdd(belowPivot, pivot, complement);
				}
			//Step 5
				//Beginning with the rightmost pivot and working upward and to the left, create zeroes above each pivot.
				//If a pivot is not 1, make it 1 by a scaling operation.
					//Use row replacement operations to create zeroes in all positions above the pivot
				for (int i = pivot.row; i >= 0; i--) {
					if (i == pivot.row) {
						if (getCoordinate(pivot).doubleValue() != 1.0) {
							Scale(pivot, getCoordinate(pivot).reciprocal());
						}
						continue;
					}
					if (i == pivot.row) {
						continue;
					}

					Coordinate abovePivot = new Coordinate(i, pivot.col);
					Fraction complement = (getCoordinate(abovePivot).negate().divide(getCoordinate(pivot)));
					MultiplyAndAdd(abovePivot, pivot, complement);
				}
			//Step 4
				//Ignore the row containing the pivot position and cover all rows, if any, above it.
				//Apply steps 1-3 to the remaining submatrix. Repeat until there are no more nonzero entries.
				if ((pivot.row + 1) >= numRows || isRowZeroes(new Coordinate(pivot.row+1, pivot.col))) {
					break;
				}

				submatrix++;
				pivot.row++;
		}
	}

	public boolean isColumnZeroes(Coordinate a) {
		for (int i = 0; i < numRows; i++) {
			if (matrix.get(i).get(a.col).doubleValue() != 0.0) {
				return false;
			}
		}

		return true;
	}

	public boolean isRowZeroes(Coordinate a) {
		for (int i = 0; i < numCols; i++) {
			if (matrix.get(a.row).get(i).doubleValue() != 0.0) {
				return false;
			}
		}

		return true;
	}

	public Coordinate findPivot(Coordinate a) {
		int first_row = a.row;
		Coordinate pivot = new Coordinate(a.row, a.col);
		Coordinate current = new Coordinate(a.row, a.col);

		for (int i = a.row; i < (numRows - first_row); i++) {
			current.row = i;
			if (getCoordinate(current).doubleValue() == 1.0) {
				Interchange(current, a);
			}
		}

		current.row = a.row;
		for (int i = current.row; i < (numRows - first_row); i++) {
			current.row = i;
			if (getCoordinate(current).doubleValue() != 0) {
				pivot.row = i;
				break;
			}
		}


		return pivot;
	}

	public Fraction getCoordinate(Coordinate a) {
		return matrix.get(a.row).get(a.col);
	}

	public String toString() {
		return matrix.toString().replace("], ", "]\n");
	}

	public static void main (String[] args) {
        	double[][] matrix_1 = {
			{1, 2, -1, -4},
			{2, 3, -1, -11},
			{-2, 0, -3, 22}
		};

		Matrix x = new Matrix(matrix_1);
		System.out.println("before\n" + x.toString() + "\n");
		x.RREF();
		System.out.println("after\n" + x.toString() + "\n");

		double matrix_2 [][] = {
			{2, 0, -1, 0, 0},
			{1, 0, 0, -1, 0},
			{3, 0, 0, -2, -1},
			{0, 1, 0, 0, -2},
			{0, 1, -1, 0, 0}
		};

		Matrix y = new Matrix(matrix_2);
		System.out.println("before\n" + y.toString() + "\n");
		y.RREF();
		System.out.println("after\n" + y.toString() + "\n");

		double matrix_3 [][] = {
			{1, 2, 3, 4, 3, 1},
			{2, 4, 6, 2, 6, 2},
			{3, 6, 18, 9, 9, -6},
			{4, 8, 12, 10, 12, 4},
			{5, 10, 24, 11, 15, -4}
		};

		Matrix z = new Matrix(matrix_3);
		System.out.println("before\n" + z.toString() + "\n");
		z.RREF();
		System.out.println("after\n" + z.toString() + "\n");

		double matrix_4 [][] = {
			{0, 1},
			{1, 2},
			{0,5}
		};

		Matrix a = new Matrix(matrix_4);
		System.out.println("before\n" + a.toString() + "\n");
		a.RREF();
		System.out.println("after\n" + a.toString() + "\n");
	}
}
```



## JavaScript

{{works with|SpiderMonkey}} for the <code>print()</code> function.
Extends the Matrix class defined at [[Matrix Transpose#JavaScript]]

```javascript
// modifies the matrix in-place
Matrix.prototype.toReducedRowEchelonForm = function() {
    var lead = 0;
    for (var r = 0; r < this.rows(); r++) {
        if (this.columns() <= lead) {
            return;
        }
        var i = r;
        while (this.mtx[i][lead] == 0) {
            i++;
            if (this.rows() == i) {
                i = r;
                lead++;
                if (this.columns() == lead) {
                    return;
                }
            }
        }

        var tmp = this.mtx[i];
        this.mtx[i] = this.mtx[r];
        this.mtx[r] = tmp;

        var val = this.mtx[r][lead];
        for (var j = 0; j < this.columns(); j++) {
            this.mtx[r][j] /= val;
        }

        for (var i = 0; i < this.rows(); i++) {
            if (i == r) continue;
            val = this.mtx[i][lead];
            for (var j = 0; j < this.columns(); j++) {
                this.mtx[i][j] -= val * this.mtx[r][j];
            }
        }
        lead++;
    }
    return this;
}

var m = new Matrix([
  [ 1, 2, -1, -4],
  [ 2, 3, -1,-11],
  [-2, 0, -3, 22]
]);
print(m.toReducedRowEchelonForm());
print();

m = new Matrix([
  [ 1, 2, 3, 7],
  [-4, 7,-2, 7],
  [ 3, 3, 0, 7]
]);
print(m.toReducedRowEchelonForm());
```

{{out}}

```txt
1,0,0,-8
0,1,0,1
0,0,1,-2

1,0,0,0.6666666666666663
0,1,0,1.666666666666667
0,0,1,1
```




## Julia

RowEchelon.jl offers the function <code>rref</code> to compute the reduced-row echelon form:

```txt

julia> matrix = [1 2 -1 -4 ; 2 3 -1 -11 ; -2 0 -3 22]
3x4 Int32 Array:
  1  2  -1   -4
  2  3  -1  -11
 -2  0  -3   22

julia> rref(matrix)
3x4 Array{Float64,2}:
 1.0  0.0  0.0  -8.0
 0.0  1.0  0.0   1.0
 0.0  0.0  1.0  -2.0


```



## Kotlin


```scala
// version 1.1.51

typealias Matrix = Array<DoubleArray>

/* changes the matrix to RREF 'in place' */
fun Matrix.toReducedRowEchelonForm() {
    var lead = 0
    val rowCount = this.size
    val colCount = this[0].size
    for (r in 0 until rowCount) {
        if (colCount <= lead) return
        var i = r

        while (this[i][lead] == 0.0) {
            i++
            if (rowCount == i) {
                i = r
                lead++
                if (colCount == lead) return
            }
        }

        val temp = this[i]
        this[i] = this[r]
        this[r] = temp

        if (this[r][lead] != 0.0) {
           val div = this[r][lead]
           for (j in 0 until colCount) this[r][j] /= div
        }

        for (k in 0 until rowCount) {
            if (k != r) {
                val mult = this[k][lead]
                for (j in 0 until colCount) this[k][j] -= this[r][j] * mult
            }
        }

        lead++
    }
}

fun Matrix.printf(title: String) {
    println(title)
    val rowCount = this.size
    val colCount = this[0].size

    for (r in 0 until rowCount) {
        for (c in 0 until colCount) {
            if (this[r][c] == -0.0) this[r][c] = 0.0  // get rid of negative zeros
            print("${"% 6.2f".format(this[r][c])}  ")
        }
        println()
    }

    println()
}

fun main(args: Array<String>) {
    val matrices = listOf(
        arrayOf(
            doubleArrayOf( 1.0, 2.0, -1.0, -4.0),
            doubleArrayOf( 2.0, 3.0, -1.0, -11.0),
            doubleArrayOf(-2.0, 0.0, -3.0,  22.0)
        ),
        arrayOf(
            doubleArrayOf(1.0,  2.0,  3.0,  4.0,  3.0,  1.0),
            doubleArrayOf(2.0,  4.0,  6.0,  2.0,  6.0,  2.0),
            doubleArrayOf(3.0,  6.0, 18.0,  9.0,  9.0, -6.0),
            doubleArrayOf(4.0,  8.0, 12.0, 10.0, 12.0,  4.0),
            doubleArrayOf(5.0, 10.0, 24.0, 11.0, 15.0, -4.0)
        )
    )

    for (m in matrices) {
        m.printf("Original matrix:")
        m.toReducedRowEchelonForm()
        m.printf("Reduced row echelon form:")
    }
}
```


{{out}}

```txt

Original matrix:
  1.00    2.00   -1.00   -4.00
  2.00    3.00   -1.00  -11.00
 -2.00    0.00   -3.00   22.00

Reduced row echelon form:
  1.00    0.00    0.00   -8.00
  0.00    1.00    0.00    1.00
  0.00    0.00    1.00   -2.00

Original matrix:
  1.00    2.00    3.00    4.00    3.00    1.00
  2.00    4.00    6.00    2.00    6.00    2.00
  3.00    6.00   18.00    9.00    9.00   -6.00
  4.00    8.00   12.00   10.00   12.00    4.00
  5.00   10.00   24.00   11.00   15.00   -4.00

Reduced row echelon form:
  1.00    2.00    0.00    0.00    3.00    4.00
  0.00    0.00    1.00    0.00    0.00   -1.00
  0.00    0.00    0.00    1.00    0.00    0.00
  0.00    0.00    0.00    0.00    0.00    0.00
  0.00    0.00    0.00    0.00    0.00    0.00

```



## Lua


```lua
function ToReducedRowEchelonForm ( M )
    local lead = 1
    local n_rows, n_cols = #M, #M[1]

    for r = 1, n_rows do
        if n_cols <= lead then break end

        local i = r
        while M[i][lead] == 0 do
            i = i + 1
            if n_rows == i then
                i = r
                lead = lead + 1
                if n_cols == lead then break end
            end
        end
        M[i], M[r] = M[r], M[i]

        local m = M[r][lead]
        for k = 1, n_cols do
            M[r][k] = M[r][k] / m
        end
        for i = 1, n_rows do
            if i ~= r then
                local m = M[i][lead]
                for k = 1, n_cols do
                    M[i][k] = M[i][k] - m * M[r][k]
                end
            end
        end
        lead = lead + 1
    end
end

M = { { 1, 2, -1, -4 },
      { 2, 3, -1, -11 },
      { -2, 0, -3, 22 } }

res = ToReducedRowEchelonForm( M )

for i = 1, #M do
    for j = 1, #M[1] do
        io.write( M[i][j], "  " )
    end
    io.write( "\n" )
end
```

{{out}}

```txt
1  0  0  -8
0  1  0  1
0  0  1  -2
```



## M2000 Interpreter

low bound 1 for array

```M2000 Interpreter

Module Base1 {
      dim base 1, A(3, 4)
      A(1, 1)= 1,    2,   -1,   -4,  2 ,   3,   -1,   -11,  -2  ,  0 ,  -3,    22
      lead=1
      rowcount=3
      columncount=4
      gosub disp()
      for r=1 to rowcount {
            if columncount<lead then exit
            i=r
            while A(i,lead)=0 {
                  i++
                  if rowcount=i then i=r : lead++ : if columncount<lead then exit
            }
            for c =1 to columncount {
                  swap A(i, c), A(r, c)
            }
              if A(r, lead)<>0 then {
                  div1=A(r,lead)
                  For c =1 to columncount {
                      A( r, c)/=div1
                  }
            }
            for i=1 to rowcount {
                  if i<>r then {
                        mult=A(i,lead)
                        for j=1 to columncount {
                                 A(i,j)-=A(r,j)*mult
                        }
                  }
            }
            lead=lead+1
      }
      disp()
      sub disp()
            local i, j
            for i=1 to rowcount
                  for j=1 to columncount
                        Print A(i, j),
                  Next j
                  if pos>0 then print
            Next i
      End sub
}
Base1

```


Low bound 0 for array


```M2000 Interpreter

Module base0 {
      dim base 0, A(3, 4)
      A(0, 0)= 1,    2,   -1,   -4,  2 ,   3,   -1,   -11,  -2  ,  0 ,  -3,    22
      lead=0
      rowcount=3
      columncount=4
      gosub disp()
      for r=0 to rowcount-1 {
            if columncount<=lead then exit
            i=r
            while A(i,lead)=0 {
                  i++
                  if rowcount=i then i=r : lead++ : if columncount<lead then exit
            }
            for c =0 to columncount-1 {
                  swap A(i, c), A(r, c)
            }
              if A(r, lead)<>0 then {
                  div1=A(r,lead)
                  For c =0 to columncount-1 {
                      A( r, c)/=div1
                  }
            }
            for i=0 to rowcount-1 {
                  if i<>r then {
                        mult=A(i,lead)
                        for j=0 to columncount-1 {
                                 A(i,j)-=A(r,j)*mult
                        }
                  }
            }
            lead=lead+1
      }
      disp()
      sub disp()
            local i, j
            for i=0 to rowcount-1
                  for j=0 to columncount-1
                        Print A(i, j),
                  Next j
                  if pos>0 then print
            Next i
      End sub
}
base0

```





## Maple



```Maple

with(LinearAlgebra):

ReducedRowEchelonForm(<<1,2,-2>|<2,3,0>|<-1,-1,-3>|<-4,-11,22>>);

```

{{out}}

```txt

                                [1  0  0  -8]
                                [           ]
                                [0  1  0   1]
                                [           ]
                                [0  0  1  -2]

```



## Mathematica


```Mathematica
RowReduce[{{1, 2, -1, -4}, {2, 3, -1, -11}, {-2, 0, -3, 22}}]
```

gives back:

```Mathematica
{{1, 0, 0, -8}, {0, 1, 0, 1}, {0, 0, 1, -2}}
```



## MATLAB


```MATLAB
rref([1, 2, -1, -4; 2, 3, -1, -11; -2, 0, -3, 22])
```



## Maxima


```maxima
rref(a):=block([p,q,k],[p,q]:matrix_size(a),a:echelon(a),
    k:min(p,q),
    for i thru min(p,q) do (if a[i,i]=0 then (k:i-1,return())),
    for i:k thru 2 step -1 do (for j from i-1 thru 1 step -1 do a:rowop(a,j,i,a[j,i])),
    a)$

a: matrix([12,-27,36,44,59],
          [26,41,-54,24,23],
          [33,70,59,15,-68],
          [43,16,29,-52,-61],
          [-43,20,71,88,11])$

rref(a);
matrix([1,0,0,0,1/2],[0,1,0,0,-1],[0,0,1,0,-1/2],[0,0,0,1,1],[0,0,0,0,0])
```



## Objeck


```objeck

class RowEchelon {
  function : Main(args : String[]) ~ Nil {
    matrix := [
      [1, 2, -1,  -4 ]
      [2, 3, -1, -11 ]
      [-2, 0, -3,  22]
    ];

    matrix := Rref(matrix);

    sizes := matrix->Size();
    for(i := 0; i < sizes[0]; i += 1;) {
      for(j := 0; j < sizes[1]; j += 1;) {
        IO.Console->Print(matrix[i,j])->Print(",");
      };
      IO.Console->PrintLine();
    };
  }

  function : native : Rref(matrix : Int[,]) ~ Int[,] {
    lead := 0;
    sizes := matrix->Size();
    rowCount := sizes[0];
    columnCount := sizes[1];

    for(r := 0; r < rowCount; r+=1;) {
      if (columnCount <= lead) {
        break;
      };

      i := r;
      while(matrix[i, lead] = 0) {
        i+=1;
        if (i = rowCount) {
          i := r;
          lead += 1;
          if (columnCount = lead) {
            lead-=1;
            break;
           };
        };
      };

      for (j := 0; j < columnCount; j+=1;) {
        temp := matrix[r, j];
        matrix[r, j] := matrix[i, j];
        matrix[i, j] := temp;
      };

      div := matrix[r, lead];
      for(j := 0; j < columnCount; j+=1;) {
        matrix[r, j] /= div;
      };

      for(j := 0; j < rowCount; j+=1;) {
        if (j <> r) {
          sub := matrix[j, lead];
          for (k := 0; k < columnCount; k+=1;) {
            matrix[j, k] -= sub * matrix[r, k];
          };
         };
      };
      lead+=1;
    };

    return matrix;
  }
}

```



## OCaml


```OCaml
let swap_rows m i j =
  let tmp = m.(i) in
  m.(i) <- m.(j);
  m.(j) <- tmp;
;;

let rref m =
  try
    let lead = ref 0
    and rows = Array.length m
    and cols = Array.length m.(0) in
    for r = 0 to pred rows do
      if cols <= !lead then
        raise Exit;
      let i = ref r in
      while m.(!i).(!lead) = 0 do
        incr i;
        if rows = !i then begin
          i := r;
          incr lead;
          if cols = !lead then
            raise Exit;
        end
      done;
      swap_rows m !i r;
      let lv = m.(r).(!lead) in
      m.(r) <- Array.map (fun v -> v / lv) m.(r);
      for i = 0 to pred rows do
        if i <> r then
          let lv = m.(i).(!lead) in
          m.(i) <- Array.mapi (fun i iv -> iv - lv * m.(r).(i)) m.(i);
      done;
      incr lead;
    done
  with Exit -> ()
;;

let () =
  let m =
    [| [|  1; 2; -1;  -4 |];
       [|  2; 3; -1; -11 |];
       [| -2; 0; -3;  22 |]; |]
  in
  rref m;

  Array.iter (fun row ->
    Array.iter (fun v ->
      Printf.printf " %d" v
    ) row;
    print_newline()
  ) m
```


Another implementation:

```OCaml
let rref m =
   let nr, nc = Array.length m, Array.length m.(0) in
   let add r s k =
      for i = 0 to nc-1 do m.(r).(i) <- m.(r).(i) +. m.(s).(i)*.k done in
   for c = 0 to min (nc-1) (nr-1) do
      for r = c+1 to nr-1 do
         if abs_float m.(c).(c) < abs_float m.(r).(c) then
         let v = m.(r) in (m.(r) <- m.(c); m.(c) <- v)
      done;
      let t = m.(c).(c) in
      if t <> 0.0 then
      begin
         for r = 0 to nr-1 do if r <> c then add r c (-.m.(r).(c)/.t) done;
         for i = 0 to nc-1 do m.(c).(i) <- m.(c).(i)/.t done
      end
   done;;

let mat = [|
             [|  1.0;  2.0;  -.1.0;  -.4.0;|];
             [|  2.0;  3.0;  -.1.0; -.11.0;|];
             [|-.2.0;  0.0;  -.3.0;   22.0;|]
          |] in
let pr v = Array.iter (Printf.printf " %9.4f") v; print_newline() in
let show = Array.iter pr in
   show mat;
   print_newline();
   rref mat;
   show mat
```



## Octave


```octave
A = [ 1, 2, -1, -4; 2, 3, -1, -11; -2, 0, -3, 22];
refA = rref(A);
disp(refA);
```



## PARI/GP

PARI has a built-in matrix type, but no commands for row-echelon form.  A dimension-limited one can be constructed from the built-in <code>matsolve</code> command:

```parigp
rref(M)={
  my(d=matsize(M));
  if(d[1]+1 != d[2], error("Bad size in rref"), d=d[1]);
  concat(matid(d), matsolve(matrix(d,d,x,y,M[x,y]), M[,d+1]))
};
```

Example:

```parigp
rref([1,2,-1,-4;2,3,-1,-11;-2,0,-3,22])
```

{{out}}

```txt
%1 =
[1 0 0 -8]

[0 1 0 1]

[0 0 1 -2]
```



## Perl

{{trans|Python}}
Note that the function defined here takes an array reference, which is modified in place.

```perl
sub rref
 {our @m; local *m = shift;
  @m or return;
  my ($lead, $rows, $cols) = (0, scalar(@m), scalar(@{$m[0]}));

  foreach my $r (0 .. $rows - 1)
     {$lead < $cols or return;
      my $i = $r;

      until ($m[$i][$lead])
         {++$i == $rows or next;
          $i = $r;
          ++$lead == $cols and return;}

      @m[$i, $r] = @m[$r, $i];
      my $lv = $m[$r][$lead];
      $_ /= $lv foreach @{ $m[$r] };

      my @mr = @{ $m[$r] };
      foreach my $i (0 .. $rows - 1)
         {$i == $r and next;
          ($lv, my $n) = ($m[$i][$lead], -1);
          $_ -= $lv * $mr[++$n] foreach @{ $m[$i] };}

      ++$lead;}}

sub display { join("\n" => map join(" " => map(sprintf("%4d", $_), @$_)), @{+shift})."\n" }

@m =
(
   [  1,  2,  -1,  -4 ],
   [  2,  3,  -1, -11 ],
   [ -2,  0,  -3,  22 ]
);

rref(\@m);
print display(\@m);
```

{{out}}

```txt
   1    0    0   -8
   0    1    0    1
   0    0    1   -2
```



## Perl 6

{{trans|Perl}}
{{works with|Rakudo|2018.03}}

```perl6
sub rref (@m) {
    return unless @m;
    my ($lead, $rows, $cols) = 0, +@m, +@m[0];

    for ^$rows -> $r {
        $lead < $cols or return @m;
        my $i = $r;
        until @m[$i;$lead] {
            ++$i == $rows or next;
            $i = $r;
            ++$lead == $cols and return @m;
        }
        @m[$i, $r] = @m[$r, $i] if $r != $i;
        my $lv = @m[$r;$lead];
        @m[$r] »/=» $lv;
        for ^$rows -> $n {
            next if $n == $r;
            @m[$n] »-=» @m[$r] »*» (@m[$n;$lead] // 0);
        }
        ++$lead;
    }
    @m
}

sub rat-or-int ($num) {
    return $num unless $num ~~ Rat;
    return $num.narrow if $num.narrow.WHAT ~~ Int;
    $num.nude.join: '/';
}

sub say_it ($message, @array) {
    say "\n$message";
    $_».&rat-or-int.fmt(" %5s").say for @array;
}

my @M = (
    [ # base test case
      [  1,  2,  -1,  -4 ],
      [  2,  3,  -1, -11 ],
      [ -2,  0,  -3,  22 ],
    ],
    [ # mix of number styles
      [  3,   0,  -3,    1 ],
      [ .5, 3/2,  -3,   -2 ],
      [ .2, 4/5,  -1.6, .3 ],
    ],
    [ # degenerate case
      [ 1,  2,  3,  4,  3,  1],
      [ 2,  4,  6,  2,  6,  2],
      [ 3,  6, 18,  9,  9, -6],
      [ 4,  8, 12, 10, 12,  4],
      [ 5, 10, 24, 11, 15, -4],
    ],
    [ # larger matrix
      [1,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0, -1,  0,  0,  0,  0,  0,  0],
      [1,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0, -1,  0,  0,  0,  0,  0],
      [1,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0, -1,  0,  0,  0,  0],
      [0,  1,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0, -1,  0,  0,  0],
      [0,  1,  0,  0,  0,  0,  0,  0,  1,  0,  0, -1,  0,  0,  0,  0,  0,  0],
      [0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0, -1,  0],
      [0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  0,  0, -1,  0,  0,  0,  0,  0],
      [0,  0,  1,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0, -1,  0,  0,  0],
      [0,  0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0, -1,  0,  0],
      [0,  0,  0,  1,  0,  0,  0,  0,  0,  1,  0,  0, -1,  0,  0,  0,  0,  0],
      [0,  0,  0,  0,  1,  0,  0,  1,  0,  0,  0,  0,  0, -1,  0,  0,  0,  0],
      [0,  0,  0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0, -1,  0],
      [0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0, -1,  0,  0],
      [0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0],
      [0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0],
      [0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  1],
      [0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  1,  0,  0,  0, -1,  0,  0,  0],
   ]
);

for @M -> @matrix {
    say_it( 'Original Matrix', @matrix );
    say_it( 'Reduced Row Echelon Form Matrix', rref(@matrix) );
    say "\n";
}
```


Perl 6 handles rational numbers internally as a ratio of two integers
to maintain precision.
For some situations it is useful to return the ratio
rather than the floating point result.

{{out}}

```txt

Original Matrix
     1      2     -1     -4
     2      3     -1    -11
    -2      0     -3     22

Reduced Row Echelon Form Matrix
     1      0      0     -8
     0      1      0      1
     0      0      1     -2



Original Matrix
     3      0     -3      1
   1/2    3/2     -3     -2
   1/5    4/5   -8/5   3/10

Reduced Row Echelon Form Matrix
     1      0      0  -41/2
     0      1      0  -217/6
     0      0      1  -125/6



Original Matrix
     1      2      3      4      3      1
     2      4      6      2      6      2
     3      6     18      9      9     -6
     4      8     12     10     12      4
     5     10     24     11     15     -4

Reduced Row Echelon Form Matrix
     1      2      0      0      3      4
     0      0      1      0      0     -1
     0      0      0      1      0      0
     0      0      0      0      0      0
     0      0      0      0      0      0



Original Matrix
     1      0      0      0      0      0      1      0      0      0      0     -1      0      0      0      0      0      0
     1      0      0      0      0      0      0      1      0      0      0      0     -1      0      0      0      0      0
     1      0      0      0      0      0      0      0      1      0      0      0      0     -1      0      0      0      0
     0      1      0      0      0      0      1      0      0      0      0      0      0      0     -1      0      0      0
     0      1      0      0      0      0      0      0      1      0      0     -1      0      0      0      0      0      0
     0      1      0      0      0      0      0      0      0      0      1      0      0      0      0      0     -1      0
     0      0      1      0      0      0      1      0      0      0      0      0     -1      0      0      0      0      0
     0      0      1      0      0      0      0      0      0      1      0      0      0      0     -1      0      0      0
     0      0      0      1      0      0      0      1      0      0      0      0      0      0      0     -1      0      0
     0      0      0      1      0      0      0      0      0      1      0      0     -1      0      0      0      0      0
     0      0      0      0      1      0      0      1      0      0      0      0      0     -1      0      0      0      0
     0      0      0      0      1      0      0      0      1      0      0      0      0      0      0      0     -1      0
     0      0      0      0      1      0      0      0      0      0      1      0      0      0      0     -1      0      0
     0      0      0      0      0      1      0      0      0      0      0      0      0      0      0      0      0      0
     0      0      0      0      0      0      0      0      0      1      0      0      0      0      0      0      0      0
     0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      1      0      1
     0      0      0      0      0      1      0      0      0      0      1      0      0      0     -1      0      0      0

Reduced Row Echelon Form Matrix
     1      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0  17/39
     0      1      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0   4/13
     0      0      1      0      0      0      0      0      0      0      0      0      0      0      0      0      0  20/39
     0      0      0      1      0      0      0      0      0      0      0      0      0      0      0      0      0  28/39
     0      0      0      0      1      0      0      0      0      0      0      0      0      0      0      0      0  19/39
     0      0      0      0      0      1      0      0      0      0      0      0      0      0      0      0      0      0
     0      0      0      0      0      0      1      0      0      0      0      0      0      0      0      0      0   8/39
     0      0      0      0      0      0      0      1      0      0      0      0      0      0      0      0      0  11/39
     0      0      0      0      0      0      0      0      1      0      0      0      0      0      0      0      0    1/3
     0      0      0      0      0      0      0      0      0      1      0      0      0      0      0      0      0      0
     0      0      0      0      0      0      0      0      0      0      1      0      0      0      0      0      0  20/39
     0      0      0      0      0      0      0      0      0      0      0      1      0      0      0      0      0  25/39
     0      0      0      0      0      0      0      0      0      0      0      0      1      0      0      0      0  28/39
     0      0      0      0      0      0      0      0      0      0      0      0      0      1      0      0      0  10/13
     0      0      0      0      0      0      0      0      0      0      0      0      0      0      1      0      0  20/39
     0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      1      0      1
     0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      1  32/39

```


Re-implemented without the pseudocode, expressed as elementary matrix row operations. See
http://unapologetic.wordpress.com/2009/08/27/elementary-row-and-column-operations/
and
http://unapologetic.wordpress.com/2009/09/03/reduced-row-echelon-form/

First, a procedural version:

```perl6
sub swap_rows    ( @M,         $r1, $r2 ) { @M[ $r1, $r2 ] = @M[ $r2, $r1 ] };
sub scale_row    ( @M, $scale, $r       ) { @M[$r]  =              @M[$r]  »*» $scale   };
sub shear_row    ( @M, $scale, $r1, $r2 ) { @M[$r1] = @M[$r1].list »+» ( @M[$r2] »*» $scale ) };
sub reduce_row   ( @M,         $r,  $c  ) { scale_row( @M, 1/@M[$r][$c], $r ) };
sub clear_column ( @M,         $r,  $c  ) {
    for @M.keys.grep( * != $r ) -> $row_num {
        shear_row( @M, -1*@M[$row_num][$c], $row_num, $r );
    }
}

my @M = (
    [<  1   2   -1    -4 >],
    [<  2   3   -1   -11 >],
    [< -2   0   -3    22 >],
);

my $column_count = +@( @M[0] );

my $current_col = 0;
while all( @M».[$current_col] ) == 0 {
    $current_col++;
    return if $current_col == $column_count; # Matrix was all-zeros.
}

for @M.keys -> $current_row {
    reduce_row(   @M, $current_row, $current_col );
    clear_column( @M, $current_row, $current_col );
    $current_col++;
    return if $current_col == $column_count;
}

say @($_)».fmt(' %4g') for @M;
```


And the same code, recast into OO. Also, scale and shear are recast as unscale and unshear, which fit the problem better.

```perl6
class Matrix is Array {
    method unscale_row ( @M: $scale, $row ) {
        @M[$row] = @M[$row] »/» $scale;
    }
    method unshear_row ( @M: $scale, $r1, $r2 ) {
        @M[$r1] = @M[$r1] »-» ( @M[$r2] »*» $scale );
    }
    method reduce_row ( @M: $row, $col ) {
        @M.unscale_row( @M[$row][$col], $row );
    }
    method clear_column ( @M: $row, $col ) {
        for @M.keys.grep( * != $row ) -> $scanning_row {
            @M.unshear_row( @M[$scanning_row][$col], $scanning_row, $row );
        }
    }
    method reduced_row_echelon_form ( @M: ) {
        my $column_count = +@( @M[0] );

        my $current_col = 0;
        # Skip past all-zero columns.
        while all( @M».[$current_col] ) == 0 {
            $current_col++;
            return if $current_col == $column_count; # Matrix was all-zeros.
        }

        for @M.keys -> $current_row {
            @M.reduce_row(   $current_row, $current_col );
            @M.clear_column( $current_row, $current_col );
            $current_col++;
            return if $current_col == $column_count;
        }
    }
}

my $M = Matrix.new(
    [<  1   2   -1    -4 >],
    [<  2   3   -1   -11 >],
    [< -2   0   -3    22 >],
);

$M.reduced_row_echelon_form;

say @($_)».fmt(' %4g') for @($M);
```


Note that both versions can be simplified using Z+=, Z-=, X*=,
and X/= to scale and shear.
Currently, Rakudo has a bug related to Xop= and Zop=.

Note that the negative zeros in the output are innocuous,
and also occur in the Perl 5 version.


## Phix

{{Trans|Euphoria}}

```Phix
function ToReducedRowEchelonForm(sequence M)
integer lead = 1,
        rowCount = length(M),
        columnCount = length(M[1]),
        i
    for r=1 to rowCount do
        if lead>=columnCount then exit end if
        i = r
        while M[i][lead]=0 do
            i += 1
            if i=rowCount then
                i = r
                lead += 1
                if lead=columnCount then exit end if
            end if
        end while
        -- nb M[i] is assigned before M[r], which matters when i=r:
        {M[r],M[i]} = {sq_div(M[i],M[i][lead]),M[r]}
        for j=1 to rowCount do
            if j!=r then
                M[j] = sq_sub(M[j],sq_mul(M[j][lead],M[r]))
            end if
        end for
        lead += 1
    end for
    return M
end function

? ToReducedRowEchelonForm(
    { { 1, 2, -1, -4 },
      { 2, 3, -1, -11 },
      { -2, 0, -3, 22 } })
```

{{out}}

```txt

{{1,0,0,-8},{0,1,0,1},{0,0,1,-2}}

```



## PHP

{{works with|PHP|5.x}}
{{trans|Java}}

```php
<?php

function rref($matrix)
{
    $lead = 0;
    $rowCount = count($matrix);
    if ($rowCount == 0)
        return $matrix;
    $columnCount = 0;
    if (isset($matrix[0])) {
        $columnCount = count($matrix[0]);
    }
    for ($r = 0; $r < $rowCount; $r++) {
        if ($lead >= $columnCount)
            break;        {
            $i = $r;
            while ($matrix[$i][$lead] == 0) {
                $i++;
                if ($i == $rowCount) {
                    $i = $r;
                    $lead++;
                    if ($lead == $columnCount)
                        return $matrix;
                }
            }
            $temp = $matrix[$r];
            $matrix[$r] = $matrix[$i];
            $matrix[$i] = $temp;
        }        {
            $lv = $matrix[$r][$lead];
            for ($j = 0; $j < $columnCount; $j++) {
                $matrix[$r][$j] = $matrix[$r][$j] / $lv;
            }
        }
        for ($i = 0; $i < $rowCount; $i++) {
            if ($i != $r) {
                $lv = $matrix[$i][$lead];
                for ($j = 0; $j < $columnCount; $j++) {
                    $matrix[$i][$j] -= $lv * $matrix[$r][$j];
                }
            }
        }
        $lead++;
    }
    return $matrix;
}
?>
```



## PicoLisp


```PicoLisp
(de reducedRowEchelonForm (Mat)
   (let (Lead 1  Cols (length (car Mat)))
      (for (X Mat X (cdr X))
         (NIL
            (loop
               (T (seek '((R) (n0 (get R 1 Lead))) X)
                  @ )
               (T (> (inc 'Lead) Cols)) ) )
         (xchg @ X)
         (let D (get X 1 Lead)
            (map
               '((R) (set R (/ (car R) D)))
               (car X) ) )
         (for Y Mat
            (unless (== Y (car X))
               (let N (- (get Y Lead))
                  (map
                     '((Dst Src)
                        (inc Dst (* N (car Src))) )
                     Y
                     (car X) ) ) ) )
         (T (> (inc 'Lead) Cols)) ) )
   Mat )
```

{{out}}

```txt
(reducedRowEchelonForm
   '(( 1  2  -1   -4) ( 2  3  -1  -11) (-2  0  -3   22)) )
-> ((1 0 0 -8) (0 1 0 1) (0 0 1 -2))
```



## Python



```python
def ToReducedRowEchelonForm( M):
    if not M: return
    lead = 0
    rowCount = len(M)
    columnCount = len(M[0])
    for r in range(rowCount):
        if lead >= columnCount:
            return
        i = r
        while M[i][lead] == 0:
            i += 1
            if i == rowCount:
                i = r
                lead += 1
                if columnCount == lead:
                    return
        M[i],M[r] = M[r],M[i]
        lv = M[r][lead]
        M[r] = [ mrx / float(lv) for mrx in M[r]]
        for i in range(rowCount):
            if i != r:
                lv = M[i][lead]
                M[i] = [ iv - lv*rv for rv,iv in zip(M[r],M[i])]
        lead += 1


mtx = [
   [ 1, 2, -1, -4],
   [ 2, 3, -1, -11],
   [-2, 0, -3, 22],]

ToReducedRowEchelonForm( mtx )

for rw in mtx:
  print ', '.join( (str(rv) for rv in rw) )
```



## R

{{trans|Fortran}}

```rsplus
rref <- function(m) {
  pivot <- 1
  norow <- nrow(m)
  nocolumn <- ncol(m)
  for(r in 1:norow) {
    if ( nocolumn <= pivot ) break;
    i <- r
    while( m[i,pivot] == 0 ) {
      i <- i + 1
      if ( norow == i ) {
        i <- r
        pivot <- pivot + 1
        if ( nocolumn == pivot ) return(m)
      }
    }
    trow <- m[i, ]
    m[i, ] <- m[r, ]
    m[r, ] <- trow
    m[r, ] <- m[r, ] / m[r, pivot]
    for(i in 1:norow) {
      if ( i != r )
        m[i, ] <- m[i, ] - m[r, ] * m[i, pivot]
    }
    pivot <- pivot + 1
  }
  return(m)
}

m <- matrix(c(1, 2, -1, -4,
              2, 3, -1, -11,
              -2, 0, -3, 22), 3, 4, byrow=TRUE)
print(m)
print(rref(m))
```



## Racket


```racket

#lang racket
(require math)
(define (reduced-echelon M)
  (matrix-row-echelon M #t #t))

(reduced-echelon
 (matrix [[1 2 -1 -4]
          [2 3 -1 -11]
          [-2 0 -3 22]]))

```

{{out}}

```txt

(mutable-array
    #[#[1 0 0 -8]
      #[0 1 0 1]
      #[0 0 1 -2]])

```



## REXX

''Reduced Row Echelon Form''   (a.k.a.   ''row canonical form'')   of a matrix, with optimization added.

```rexx
/*REXX pgm performs Reduced Row Echelon Form (RREF), AKA row canonical form on a matrix)*/
cols=0;   w=0;    @.=0                           /*max cols in a row; max width; matrix.*/
mat.=;                  mat.1=  '    1   2   -1      -4   '
                        mat.2=  '    2   3   -1     -11   '
                        mat.3=  '   -2   0   -3      22   '
          do r=1  until mat.r=='';      _=mat.r  /*build  @.row.col  from (matrix) mat.X*/
                    do c=1  until _='';             parse  var   _   @.r.c _
                    w=max(w, length(@.r.c) + 1)  /*find the maximum width of an element.*/
                    end   /*c*/
          cols=max(cols, c)                      /*save the maximum number of columns.  */
          end   /*r*/
rows=r - 1                                       /*adjust the row count (from  DO loop).*/
call showMat  'original matrix'                  /*display the original matrix to screen*/
!=1                                              /*set the working column pointer to  1.*/
    /* ┌──────────────────────◄────────────────◄──── Reduced Row Echelon Form on matrix.*/
  do r=1  for rows  while cols>!                 /*begin to perform the heavy lifting.  */
  j=r                                            /*use a subsitute index for the DO loop*/
      do  while  @.j.!==0;    j=j + 1
      if j==rows  then do;    j=r;      !=! + 1;    if cols==!  then leave r;   end
      end      /*while*/
                                                 /* [↓]  swap rows J,R (but not if same)*/
      do _=1  for cols  while j\==r;    parse value   @.r._  @.j._    with    @.j._  @._._
      end      /*_*/
  ?=@.r.!
      do d=1  for cols  while ?\=1;     @.r.d= @.r.d / ?
      end      /*d*/                             /* [↑] divide row J by @.r.p ──unless≡1*/
          do k=1  for rows;             ?= @.k.! /*subtract (row K)   @.r.s  from row K.*/
          if k==r | ?=0  then iterate            /*skip  if  row K is the same as row R.*/
             do s=1  for cols;          @.k.s= @.k.s   -   ? * @.r.s
             end   /*s*/
          end      /*k*/                         /* [↑]  for the rest of numbers in row.*/
  !=! + 1                                        /*bump the column pointer.             */
  end          /*r*/

call showMat  'matrix RREF'                      /*display the reduced row echelon form.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
showMat: parse arg title;          say;    say center(title, 3 + (cols+1) * w, '─');   say
               do      r=1  for rows;   _=
                    do c=1  for cols
                    if @.r.c==''  then do; say "***error*** matrix element isn't defined:"
                                           say 'row'    r",  column"    c'.';     exit 13
                                       end
                    _=_ right(@.r.c, w)
                    end   /*c*/
               say _                             /*display a matrix row to the terminal.*/
               end        /*r*/;       return
```

{{out|output|text=  when using the default (internal) input:}}

```txt

────original matrix────

    1    2   -1   -4
    2    3   -1  -11
   -2    0   -3   22

──────matrix RREF──────

    1    0    0   -8
    0    1    0    1
    0    0    1   -2

```



## Ring


```ring

# Project : Reduced row echelon form

matrix = [[1, 2, -1, -4],
              [2, 3, -1, -11],
              [ -2, 0, -3, 22]]
ref(matrix)
for row = 1 to 3
     for col = 1 to 4
           if matrix[row][col] = -0
              see "0 "
           else
              see "" + matrix[row][col] + " "
           ok
     next
     see nl
next

func ref(m)
nrows = 3
ncols = 4
lead = 1
for r = 1 to nrows
      if lead >= ncols
         exit
      ok
      i = r
      while m[i][lead] = 0
                i = i + 1
                if i = nrows
                   i = r
                   lead = lead + 1
                   if lead = ncols
                      exit 2
                   ok
                ok
      end
      for j = 1 to ncols
           temp = m[i][j]
           m[i][j] = m[r][j]
           m[r][j] = temp
      next
      n = m[r][lead]
      if n != 0
         for j = 1 to ncols
              m[r][j] = m[r][j] / n
         next
      ok
      for i = 1 to nrows
           if i != r
              n = m[i][lead]
              for j = 1 to ncols
                   m[i][j] = m[i][j] - m[r][j] * n
              next
           ok
      next
     lead = lead + 1
next

```

Output:

```txt

1 0 0 -8
0 1 0  1
0 0 1 -2

```



## Ruby

{{works with|Ruby|1.9.3}}

```ruby
# returns an 2-D array where each element is a Rational
def reduced_row_echelon_form(ary)
  lead = 0
  rows = ary.size
  cols = ary[0].size
  rary = convert_to(ary, :to_r)  # use rational arithmetic
  catch :done  do
    rows.times do |r|
      throw :done  if cols <= lead
      i = r
      while rary[i][lead] == 0
        i += 1
        if rows == i
          i = r
          lead += 1
          throw :done  if cols == lead
        end
      end
      # swap rows i and r
      rary[i], rary[r] = rary[r], rary[i]
      # normalize row r
      v = rary[r][lead]
      rary[r].collect! {|x| x / v}
      # reduce other rows
      rows.times do |i|
        next if i == r
        v = rary[i][lead]
        rary[i].each_index {|j| rary[i][j] -= v * rary[r][j]}
      end
      lead += 1
    end
  end
  rary
end

# type should be one of :to_s, :to_i, :to_f, :to_r
def convert_to(ary, type)
  ary.each_with_object([]) do |row, new|
    new << row.collect {|elem| elem.send(type)}
  end
end

class Rational
  alias _to_s to_s
  def to_s
    denominator==1 ? numerator.to_s : _to_s
  end
end

def print_matrix(m)
  max = m[0].collect {-1}
  m.each {|row| row.each_index {|i| max[i] = [max[i], row[i].to_s.length].max}}
  m.each {|row| row.each_index {|i| print "%#{max[i]}s " % row[i]}; puts}
end

mtx = [
  [ 1, 2, -1, -4],
  [ 2, 3, -1,-11],
  [-2, 0, -3, 22]
]
print_matrix reduced_row_echelon_form(mtx)
puts

mtx = [
  [ 1, 2, 3, 7],
  [-4, 7,-2, 7],
  [ 3, 3, 0, 7]
]
reduced = reduced_row_echelon_form(mtx)
print_matrix reduced
print_matrix convert_to(reduced, :to_f)
```


{{out}}

```txt

1 0 0 -8
0 1 0  1
0 0 1 -2

1 0 0 2/3
0 1 0 5/3
0 0 1   1
1.0 0.0 0.0 0.6666666666666666
0.0 1.0 0.0 1.6666666666666667
0.0 0.0 1.0                1.0

```



## Sage

{{works with|Sage|4.6.2}}

```sage
sage: m = matrix(ZZ, [[1,2,-1,-4],[2,3,-1,-11],[-2,0,-3,22]])
sage: m.rref()
[ 1  0  0 -8]
[ 0  1  0  1]
[ 0  0  1 -2]
```



## Scheme

{{Works with|Scheme|R<math>^5</math>RS}}

```scheme
(define (reduced-row-echelon-form matrix)
  (define (clean-down matrix from-row column)
    (cons (car matrix)
          (if (zero? from-row)
              (map (lambda (row)
                     (map -
                          row
                          (map (lambda (element)
                                 (/ (* element (list-ref row column))
                                    (list-ref (car matrix) column)))
                               (car matrix))))
                   (cdr matrix))
              (clean-down (cdr matrix) (- from-row 1) column))))
  (define (clean-up matrix until-row column)
    (if (zero? until-row)
        matrix
        (cons (map -
                   (car matrix)
                   (map (lambda (element)
                          (/ (* element (list-ref (car matrix) column))
                             (list-ref (list-ref matrix until-row) column)))
                        (list-ref matrix until-row)))
              (clean-up (cdr matrix) (- until-row 1) column))))
  (define (normalise matrix row with-column)
    (if (zero? row)
        (cons (map (lambda (element)
                     (/ element (list-ref (car matrix) with-column)))
                   (car matrix))
              (cdr matrix))
        (cons (car matrix) (normalise (cdr matrix) (- row 1) with-column))))
  (define (repeat procedure matrix indices)
    (if (null? indices)
        matrix
        (repeat procedure
                (procedure matrix (car indices) (car indices))
                (cdr indices))))
  (define (iota start stop)
    (if (> start stop)
        (list)
        (cons start (iota (+ start 1) stop))))
  (let ((indices (iota 0 (- (length matrix) 1))))
    (repeat normalise
            (repeat clean-up
                    (repeat clean-down
                            matrix
                            indices)
                    indices)
            indices)))
```

Example:

```scheme
(define matrix
  (list (list 1 2 -1 -4) (list 2 3 -1 -11) (list -2 0 -3 22)))

(display (reduced-row-echelon-form matrix))
(newline)
```

{{out}}
<lang>((1 0 0 -8) (0 1 0 1) (0 0 1 -2))
```



## Seed7


```seed7
const type: matrix is array array float;

const proc: toReducedRowEchelonForm (inout matrix: mat) is func
  local
    var integer: numRows is 0;
    var integer: numColumns is 0;
    var integer: row is 0;
    var integer: column is 0;
    var integer: pivot is 0;
    var float: factor is 0.0;
  begin
    numRows := length(mat);
    numColumns := length(mat[1]);
    for row range numRows downto 1 do
      column := 1;
      while column <= numColumns and mat[row][column] = 0.0 do
        incr(column);
      end while;
      if column > numColumns then
        # Empty rows are moved to the bottom
        mat := mat[.. pred(row)] & mat[succ(row) ..] & [] (mat[row]);
        decr(numRows);
      end if;
    end for;
    for pivot range 1 to numRows do
      if mat[pivot][pivot] = 0.0 then
        # Find a row were the pivot column is not zero
        row := 1;
        while row <= numRows and mat[row][pivot] = 0.0 do
          incr(row);
        end while;
        # Add row were the pivot column is not zero
        for column range 1 to numColumns do
          mat[pivot][column] +:= mat[row][column];
        end for;
      end if;
      if mat[pivot][pivot] <> 1.0 then
        # Make sure that the pivot element is 1.0
        factor := 1.0 / mat[pivot][pivot];
        for column range pivot to numColumns do
          mat[pivot][column] := mat[pivot][column] * factor;
        end for;
      end if;
      for row range 1 to numRows do
        if row <> pivot and mat[row][pivot] <> 0.0 then
          # Make sure that in all other rows the pivot column contains zero
          factor := -mat[row][pivot];
          for column range pivot to numColumns do
            mat[row][column] +:= mat[pivot][column] * factor;
          end for;
        end if;
      end for;
    end for;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#toReducedRowEchelonForm]


## Sidef

{{trans|Perl 6}}

```ruby
func rref (M) {
    var (j, rows, cols) = (0, M.len, M[0].len)

    for r in (^rows) {
        j < cols || return M

        var i = r
        while (!M[i][j]) {
            ++i == rows || next
            i = r
            ++j == cols && return M
        }

        M[i, r] = M[r, i] if (r != i)
        M[r] = (M[r] »/» M[r][j])

        for n in (^rows) {
            next if (n == r)
            M[n] = (M[n] »-« (M[r] »*» M[n][j]))
        }
        ++j
    }

    return M
}

func say_it (message, array) {
    say "\n#{message}";
    array.each { |row|
        say row.map { |n| " %5s" % n.as_rat }.join
    }
}

var M = [
    [ # base test case
      [  1,  2,  -1,  -4 ],
      [  2,  3,  -1, -11 ],
      [ -2,  0,  -3,  22 ],
    ],
    [ # mix of number styles
      [  3,   0,  -3,    1 ],
      [ .5, 3/2,  -3,   -2 ],
      [ .2, 4/5,  -1.6, .3 ],
    ],
    [ # degenerate case
      [ 1,  2,  3,  4,  3,  1],
      [ 2,  4,  6,  2,  6,  2],
      [ 3,  6, 18,  9,  9, -6],
      [ 4,  8, 12, 10, 12,  4],
      [ 5, 10, 24, 11, 15, -4],
    ],
];

M.each { |matrix|
    say_it('Original Matrix', matrix);
    say_it('Reduced Row Echelon Form Matrix', rref(matrix));
    say '';
}
```

{{out}}

```txt

Original Matrix
     1     2    -1    -4
     2     3    -1   -11
    -2     0    -3    22

Reduced Row Echelon Form Matrix
     1     0     0    -8
     0     1     0     1
     0     0     1    -2


Original Matrix
     3     0    -3     1
   1/2   3/2    -3    -2
   1/5   4/5  -8/5  3/10

Reduced Row Echelon Form Matrix
     1     0     0 -41/2
     0     1     0 -217/6
     0     0     1 -125/6


Original Matrix
     1     2     3     4     3     1
     2     4     6     2     6     2
     3     6    18     9     9    -6
     4     8    12    10    12     4
     5    10    24    11    15    -4

Reduced Row Echelon Form Matrix
     1     2     0     0     3     4
     0     0     1     0     0    -1
     0     0     0     1     0     0
     0     0     0     0     0     0
     0     0     0     0     0     0

```



## Swift


```Swift

        var lead = 0
        for r in 0..<rows {
            if (cols <= lead) { break }
            var i = r
            while (m[i][lead] == 0) {
                i += 1
                if (i == rows) {
                    i = r
                    lead += 1
                    if (cols == lead) {
                        lead -= 1
                        break
                    }
                }
            }
            for j in 0..<cols {
                let temp = m[r][j]
                m[r][j] = m[i][j]
                m[i][j] = temp
            }
            let div = m[r][lead]
            if (div != 0) {
                for j in 0..<cols {
                    m[r][j] /= div
                }
            }
            for j in 0..<rows {
                if (j != r) {
                    let sub = m[j][lead]
                    for k in 0..<cols {
                        m[j][k] -= (sub * m[r][k])
                    }
                }
            }
            lead += 1
        }

```



## Tcl

Using utility procs defined at [[Matrix Transpose#Tcl]]

```tcl
package require Tcl 8.5
namespace path {::tcl::mathop ::tcl::mathfunc}

proc toRREF {m} {
    set lead 0
    lassign [size $m] rows cols
    for {set r 0} {$r < $rows} {incr r} {
        if {$cols <= $lead} {
            break
        }
        set i $r
        while {[lindex $m $i $lead] == 0} {
            incr i
            if {$rows == $i} {
                set i $r
                incr lead
                if {$cols == $lead} {
                    # Tcl can't break out of nested loops
                    return $m
                }
            }
        }
        # swap rows i and r
        foreach idx [list $i $r] row [list [lindex $m $r] [lindex $m $i]] {
            lset m $idx $row
        }
        # divide row r by m(r,lead)
        set val [lindex $m $r $lead]
        for {set j 0} {$j < $cols} {incr j} {
            lset m $r $j [/ [double [lindex $m $r $j]] $val]
        }

        for {set i 0} {$i < $rows} {incr i} {
            if {$i != $r} {
                # subtract m(i,lead) multiplied by row r from row i
                set val [lindex $m $i $lead]
                for {set j 0} {$j < $cols} {incr j} {
                    lset m $i $j [- [lindex $m $i $j] [* $val [lindex $m $r $j]]]
                }
            }
        }
        incr lead
    }
    return $m
}

set m {{1 2 -1 -4} {2 3 -1 -11} {-2 0 -3 22}}
print_matrix $m
print_matrix [toRREF $m]
```

{{out}}

```txt
 1 2 -1  -4
 2 3 -1 -11
-2 0 -3  22
 1.0  0.0 0.0 -8.0
-0.0  1.0 0.0  1.0
-0.0 -0.0 1.0 -2.0
```


=={{header|TI-83 BASIC}}==
Builtin function: rref()

```ti83
rref([[1,2,-1,-4][2,3,-1,-11][-2,0,-3,22]])
```

{{out}}

```txt

    [[1  0  0 -8]
     [0  1  0  1]
     [0  0  1 -2]]

```



=={{header|TI-89 BASIC}}==

```ti89b
rref([1,2,–1,–4; 2,3,–1,–11; –2,0,–3,22])
```


Output (in prettyprint mode): <math>\begin{bmatrix} 1&0&0&-8 \\ 0&1&0&1 \\ 0&0&1&-2 \end{bmatrix}</math>

Matrices can also be stored in variables, and entered interactively
using the Data/Matrix Editor.


## Ursala

The most convenient representation for a matrix in Ursala is as a list of lists.
Several auxiliary functions are defined to make this task more manageable.
The pivot function reorders the rows to position the first column entry
with maximum magnitude in the first row.
The descending function is a second order function abstracting the pattern
of recursion down the major diagonal of a matrix.
The reflect function allows the code for the first phase in the reduction
to be reused during the upward traversal by appropriately permuting
the rows and columns.
The row_reduce function adds a multiple of the top row to each
subsequent row so as to cancel the first column.
These are all combined in the main rref function.


```Ursala
#import std
#import flo

pivot      = -<x fleq+ abs~~bh
descending = ~&a^&+ ^|ahPathS2fattS2RpC/~&
reflect    = ~&lxPrTSx+ *iiD ~&l-~brS+ zipp0
row_reduce = ^C/vid*hhiD *htD minus^*p/~&r times^*D/vid@bh ~&l
rref       = reflect+ (descending row_reduce)+ reflect+ descending row_reduce+ pivot

#show+

test =

printf/*=*'%8.4f' rref <
   <1.,2.,-1.,-4.>,
   <2.,3.,-1.,-11.>,
   <-2.,0.,-3.,22.>>
```

{{out}}

```txt

  1.0000  0.0000  0.0000 -8.0000
  0.0000  1.0000  0.0000  1.0000
  0.0000  0.0000  1.0000 -2.0000
```

An alternative and more efficient solution is
to use the msolve library function as shown,
which interfaces with the lapack library if available.
This solution is applicable only if the input
is a non-singular augmented square matrix.

```Ursala
#import lin

rref = @ySzSX msolve; ^plrNCTS\~& ~&iiDlSzyCK9+ :/1.+ 0.!*t
```



## VBA

{{trans|Phix}}
```vb
Private Function ToReducedRowEchelonForm(M As Variant) As Variant
    Dim lead As Integer: lead = 0
    Dim rowCount As Integer: rowCount = UBound(M)
    Dim columnCount As Integer: columnCount = UBound(M(0))
    Dim i As Integer
    For r = 0 To rowCount
        If lead >= columnCount Then
            Exit For
        End If
        i = r
        Do While M(i)(lead) = 0
            i = i + 1
            If i = rowCount Then
                i = r
                lead = lead + 1
                If lead = columnCount Then
                    Exit For
                End If
            End If
        Loop
        Dim tmp As Variant
        tmp = M(r)
        M(r) = M(i)
        M(i) = tmp
        If M(r)(lead) <> 0 Then
            div = M(r)(lead)
            For t = LBound(M(r)) To UBound(M(r))
                M(r)(t) = M(r)(t) / div
            Next t
        End If
        For j = 0 To rowCount
            If j <> r Then
                subt = M(j)(lead)
                For t = LBound(M(j)) To UBound(M(j))
                    M(j)(t) = M(j)(t) - subt * M(r)(t)
                Next t
            End If
        Next j
        lead = lead + 1
    Next r
    ToReducedRowEchelonForm = M
End Function

Public Sub main()
    r = ToReducedRowEchelonForm(Array( _
        Array(1, 2, -1, -4), _
        Array(2, 3, -1, -11), _
        Array(-2, 0, -3, 22)))
    For i = LBound(r) To UBound(r)
        Debug.Print Join(r(i), vbTab)
    Next i
End Sub
```
{{out}}

```txt
1   0   0   -8
0   1   0   1
0   0   1   -2
```



## Visual FoxPro

Translation of Fortran.

```vfp

CLOSE DATABASES ALL
LOCAL lnRows As Integer, lnCols As Integer, lcSafety As String
LOCAL ARRAY matrix[1]
lcSafety = SET("Safety")
SET SAFETY OFF
CLEAR
CREATE CURSOR results (c1 B(6), c2 B(6), c3 B(6), c4 B(6))
CREATE CURSOR curs1(c1 I, c2 I, c3 I, c4 I)
INSERT INTO curs1 VALUES (1,2,-1,-4)
INSERT INTO curs1 VALUES (2,3,-1,-11)
INSERT INTO curs1 VALUES (-2,0,-3,22)
lnRows = RECCOUNT()	&& 3
lnCols = FCOUNT()	&& 4
SELECT * FROM curs1 INTO ARRAY matrix
IF RREF(@matrix, lnRows, lnCols)
	SELECT results
	APPEND FROM ARRAY matrix
	BROWSE NORMAL IN SCREEN
ENDIF
SET SAFETY &lcSafety

FUNCTION RREF(mat, tnRows As Integer, tnCols As Integer) As Boolean
LOCAL lnPivot As Integer, i As Integer, r As Integer, j As Integer, ;
p As Double. llResult As Boolean, llExit As Boolean
llResult = .T.
llExit = .F.
lnPivot = 1
FOR r = 1 TO tnRows
	IF lnPivot > tnCols
		EXIT
	ENDIF
	i = r
	DO WHILE mat[i,lnPivot] = 0
		i = i + 1
		IF i = tnRows
			i = r
			lnPivot = lnPivot + 1
			IF lnPivot > tnCols
				llExit = .T.
				EXIT
			ENDIF
		ENDIF
	ENDDO
	IF llExit
		EXIT
	ENDIF
	ASwapRows(@mat, i, r)
	p = mat[r,lnPivot]
	IF p # 0
		FOR j = 1 TO tnCols
			mat[r,j] = mat[r,j]/p
		ENDFOR
	ELSE
		? "Divison by zero."
		llResult = .F.
		EXIT
	ENDIF
	FOR i = 1 TO tnRows
		IF i # r
			p = mat[i,lnPivot]
			FOR j = 1 TO tnCols
				mat[i,j] = mat[i,j] - mat[r,j]*p
			ENDFOR
		ENDIF
	ENDFOR
	lnPivot = lnPivot + 1
ENDFOR
RETURN llResult
ENDFUNC

PROCEDURE ASwapRows(arr, tnRow1 As Integer, tnRow2 As Integer)
*!* Interchange rows tnRow1 and tnRow2 of array arr.
LOCAL n As Integer
n = ALEN(arr,2)
LOCAL ARRAY tmp[1,n]
STORE 0 TO tmp
ACPY2(@arr, @tmp, tnRow1, 1)
ACPY2(@arr, @arr, tnRow2, tnRow1)
ACPY2(@tmp, @arr, 1, tnRow2)
ENDPROC

PROCEDURE ACPY2(m1, m2, tnSrcRow As Integer, tnDestRow As Integer)
*!* Copy m1[tnSrcRow,*] to m2[tnDestRow,*]
*!* m1 and m2 must have the same number of columns.
LOCAL n As Integer, e1 As Integer, e2 As Integer
n = ALEN(m1,2)
e1 = AELEMENT(m1,tnSrcRow,1)
e2 = AELEMENT(m2,tnDestRow,1)
ACOPY(m1, m2, e1, n, e2)
ENDPROC

```

{{out}}

```txt

   C1          C2          C3          C4
   1.000000    0.000000    0.000000    -8.000000
   0.000000    1.000000    0.000000    1.000000
   0.000000    0.000000    1.000000    -2.000000

```



## zkl

The "best" way is to use the GNU Scientific Library:

```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
fcn toReducedRowEchelonForm(M){  // in place
   lead,rows,columns := 0,M.rows,M.cols;
   foreach r in (rows){
      if (columns<=lead) return(M);
      i:=r;
      while(M[i,lead]==0){  // not a great check to use with real numbers
	 i+=1;
	 if(i==rows){
	    i=r; lead+=1;
	    if(lead==columns) return(M);
	 }
      }
      M.swapRows(i,r);
      if(x:=M[r,lead]) M[r]/=x;
      foreach i in (rows){ if(i!=r) M[i]-=M[r]*M[i,lead] }
      lead+=1;
   }
   M
}
```


```zkl
A:=GSL.Matrix(3,4).set( 1, 2, -1,  -4,
		        2, 3, -1, -11,
		       -2, 0, -3,  22);
toReducedRowEchelonForm(A).format(5,1).println();
```

{{out}}

```txt

  1.0,  0.0,  0.0, -8.0
  0.0,  1.0,  0.0,  1.0
  0.0,  0.0,  1.0, -2.0

```

Or, using lists of lists and direct implementation of the pseudo-code given,
lots of generating new rows rather than modifying the rows themselves.

```zkl
fcn toReducedRowEchelonForm(m){ // m is modified, the rows are not
   lead,rowCount,columnCount := 0,m.len(),m[1].len();
   foreach r in (rowCount){
      if(columnCount<=lead) break;
      i:=r;
      while(m[i][lead]==0){
	 i+=1;
	 if(rowCount==i){
	    i=r; lead+=1;
	    if(columnCount==lead) break;
	 }
      }//while
      m.swap(i,r); // Swap rows i and r
      if(n:=m[r][lead]) m[r]=m[r].apply('/(n)); //divide row r by M[r,lead]
      foreach i in (rowCount){
         if(i!=r) // Subtract M[i, lead] multiplied by row r from row i
	    m[i]=m[i].zipWith('-,m[r].apply('*(m[i][lead])))
      }//foreach
      lead+=1;
   }//foreach
   m
}
```


```zkl
m:=List( T( 1, 2, -1, -4,),  // T is read only list
         T( 2, 3, -1, -11,),
	 T(-2, 0, -3,  22,));
printM(m);
println("-->");
printM(toReducedRowEchelonForm(m));

fcn printM(m){ m.pump(Console.println,rowFmt) }
fcn rowFmt(row){ ("%4d "*row.len()).fmt(row.xplode()) }
```

{{out}}

```txt

   1    2   -1   -4
   2    3   -1  -11
  -2    0   -3   22
-->
   1    0    0   -8
   0    1    0    1
   0    0    1   -2

```


== References ==
<references/>
