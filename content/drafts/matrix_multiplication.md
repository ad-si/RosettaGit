+++
title = "Matrix multiplication"
description = ""
date = 2019-10-19T11:13:06Z
aliases = []
[extra]
id = 2408
[taxonomies]
categories = []
tags = []
+++

{{task|Matrices}}

;Task:
Multiply two matrices together.

They can be of any dimensions, so long as the number of columns of the first matrix is equal to the number of rows of the second matrix.





## 360 Assembly


```360asm
*        Matrix multiplication     06/08/2015
MATRIXRC CSECT                     Matrix multiplication
         USING  MATRIXRC,R13
SAVEARA  B      STM-SAVEARA(R15)
         DC     17F'0'
STM      STM    R14,R12,12(R13)
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R13,R15
         LA     R7,1               i=1
LOOPI1   CH     R7,M               do i=1 to m (R7)
         BH     ELOOPI1
         LA     R8,1               j=1
LOOPJ1   CH     R8,P               do j=1 to p (R8)
         BH     ELOOPJ1
         LR     R1,R7              i
         BCTR   R1,0
         MH     R1,P
         LR     R6,R8              j
         BCTR   R6,0
         AR     R1,R6
         SLA    R1,2
         LA     R6,0
         ST     R6,C(R1)           c(i,j)=0
         LA     R9,1               k=1
LOOPK1   CH     R9,N               do k=1 to n (R9)
         BH     ELOOPK1
         LR     R1,R7              i
         BCTR   R1,0
         MH     R1,P
         LR     R6,R8              j
         BCTR   R6,0
         AR     R1,R6
         SLA    R1,2
         L      R2,C(R1)           R2=c(i,j)
         LR     R10,R1             R10=offset(i,j)
         LR     R1,R7              i
         BCTR   R1,0
         MH     R1,N
         LR     R6,R9              k
         BCTR   R6,0
         AR     R1,R6
         SLA    R1,2
         L      R3,A(R1)           R3=a(i,k)
         LR     R1,R9              k
         BCTR   R1,0
         MH     R1,P
         LR     R6,R8              j
         BCTR   R6,0
         AR     R1,R6
         SLA    R1,2
         L      R4,B(R1)           R4=b(k,j)
         LR     R15,R3             a(i,k)
         MR     R14,R4             a(i,k)*b(k,j)
         LR     R3,R15
         AR     R2,R3              R2=R2+a(i,k)*b(k,j)
         ST     R2,C(R10)          c(i,j)=c(i,j)+a(i,k)*b(k,j)
         LA     R9,1(R9)           k=k+1
         B      LOOPK1
ELOOPK1  LA     R8,1(R8)           j=j+1
         B      LOOPJ1
ELOOPJ1  LA     R7,1(R7)           i=i+1
         B      LOOPI1
ELOOPI1  MVC    Z,=CL80' '         clear buffer
         LA     R7,1
LOOPI2   CH     R7,M               do i=1 to m
         BH     ELOOPI2
         LA     R8,1
LOOPJ2   CH     R8,P               do j=1 to p
         BH     ELOOPJ2
         LR     R1,R7              i
         BCTR   R1,0
         MH     R1,P
         LR     R6,R8              j
         BCTR   R6,0
         AR     R1,R6
         SLA    R1,2
         L      R6,C(R1)           c(i,j)
         LA     R3,Z
         AH     R3,IZ
         XDECO  R6,W
         MVC    0(5,R3),W+7        output c(i,j)
         LH     R3,IZ
         LA     R3,5(R3)
         STH    R3,IZ
         LA     R8,1(R8)           j=j+1
         B      LOOPJ2
ELOOPJ2  XPRNT  Z,80               print buffer
         MVC    IZ,=H'0'
         LA     R7,1(R7)           i=i+1
         B      LOOPI2
ELOOPI2  L      R13,4(0,R13)
         LM     R14,R12,12(R13)
         XR     R15,R15
         BR     R14
A        DC     F'1',F'2',F'3',F'4',F'5',F'6',F'7',F'8'  a(4,2)
B        DC     F'1',F'2',F'3',F'4',F'5',F'6'            b(2,3)
C        DS     12F                                      c(4,3)
N        DC     H'2'               dim(a,2)=dim(b,1)
M        DC     H'4'               dim(a,1)
P        DC     H'3'               dim(b,2)
Z        DS     CL80
IZ       DC     H'0'
W        DS     CL16
         YREGS
         END    MATRIXRC
```

{{out}}

```txt
    9   12   15
   19   26   33
   29   40   51
   39   54   69
```



## Ada

Ada has matrix multiplication predefined for any floating-point or complex type.
The implementation is provided by the standard library packages Ada.Numerics.Generic_Real_Arrays and Ada.Numerics.Generic_Complex_Arrays correspondingly. The following example illustrates use of real matrix multiplication for the type Float:

```ada
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Numerics.Real_Arrays;  use Ada.Numerics.Real_Arrays;

procedure Matrix_Product is

   procedure Put (X : Real_Matrix) is
      type Fixed is delta 0.01 range -100.0..100.0;
   begin
      for I in X'Range (1) loop
         for J in X'Range (2) loop
            Put (Fixed'Image (Fixed (X (I, J))));
         end loop;
         New_Line;
      end loop;
   end Put;

   A : constant Real_Matrix :=
         (  ( 1.0,  1.0,  1.0,   1.0),
            ( 2.0,  4.0,  8.0,  16.0),
            ( 3.0,  9.0, 27.0,  81.0),
            ( 4.0, 16.0, 64.0, 256.0)
         );
   B : constant Real_Matrix :=
         (  (  4.0,     -3.0,      4.0/3.0,  -1.0/4.0 ),
            (-13.0/3.0, 19.0/4.0, -7.0/3.0,  11.0/24.0),
            (  3.0/2.0, -2.0,      7.0/6.0,  -1.0/4.0 ),
            ( -1.0/6.0,  1.0/4.0, -1.0/6.0,   1.0/24.0)
         );
begin
   Put (A * B);
end Matrix_Product;
```

{{out}}

```txt

 1.00 0.00 0.00 0.00
 0.00 1.00 0.00 0.00
 0.00 0.00 1.00 0.00
 0.00 0.00 0.00 1.00

```

The following code illustrates how matrix multiplication could be implemented from scratch:

```ada
package Matrix_Ops is
   type Matrix is array (Natural range <>, Natural range <>) of Float;
   function "*" (Left, Right : Matrix) return Matrix;
end Matrix_Ops;

package body Matrix_Ops is
   ---------
   -- "*" --
   ---------
   function "*" (Left, Right : Matrix) return Matrix is
      Temp : Matrix(Left'Range(1), Right'Range(2)) := (others =>(others => 0.0));
   begin
      if Left'Length(2) /= Right'Length(1) then
         raise Constraint_Error;
      end if;

      for I in Left'range(1) loop
         for J in Right'range(2) loop
            for K in Left'range(2) loop
               Temp(I,J) := Temp(I,J) + Left(I, K)*Right(K, J);
            end loop;
         end loop;
      end loop;
      return Temp;
   end "*";
end Matrix_Ops;
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of FORMATted transput}}
An example of user defined Vector and Matrix Multiplication Operators:

```algol68
MODE FIELD = LONG REAL; # field type is LONG REAL #
INT default upb:=3;
MODE VECTOR = [default upb]FIELD;
MODE MATRIX = [default upb,default upb]FIELD;

# crude exception handling #
PROC VOID raise index error := VOID: GOTO exception index error;

# define the vector/matrix operators #
OP * = (VECTOR a,b)FIELD: ( # basically the dot product #
    FIELD result:=0;
    IF LWB a/=LWB b OR UPB a/=UPB b THEN raise index error FI;
    FOR i FROM LWB a TO UPB a DO result+:= a[i]*b[i] OD;
    result
  );

 OP * = (VECTOR a, MATRIX b)VECTOR: ( # overload vector times matrix #
    [2 LWB b:2 UPB b]FIELD result;
    IF LWB a/=LWB b OR UPB a/=UPB b THEN raise index error FI;
    FOR j FROM 2 LWB b TO 2 UPB b DO result[j]:=a*b[,j] OD;
    result
  );
# this is the task portion #
OP * = (MATRIX a, b)MATRIX: ( # overload matrix times matrix #
    [LWB a:UPB a, 2 LWB b:2 UPB b]FIELD result;
    IF 2 LWB a/=LWB b OR 2 UPB a/=UPB b THEN raise index error FI;
    FOR k FROM LWB result TO UPB result DO result[k,]:=a[k,]*b OD;
    result
  );

 # Some sample matrices to test #
test:(
  MATRIX a=((1,  1,  1,   1), # matrix A #
            (2,  4,  8,  16),
            (3,  9, 27,  81),
            (4, 16, 64, 256));

  MATRIX b=((  4  , -3  ,  4/3,  -1/4 ), # matrix B #
            (-13/3, 19/4, -7/3,  11/24),
            (  3/2, -2  ,  7/6,  -1/4 ),
            ( -1/6,  1/4, -1/6,   1/24));

  MATRIX prod = a * b; # actual multiplication example of A x B #

  FORMAT real fmt = $g(-6,2)$; # width of 6, with no '+' sign, 2 decimals #
  PROC real matrix printf= (FORMAT real fmt, MATRIX m)VOID:(
    FORMAT vector fmt = $"("n(2 UPB m-1)(f(real fmt)",")f(real fmt)")"$;
    FORMAT matrix fmt = $x"("n(UPB m-1)(f(vector fmt)","lxx)f(vector fmt)");"$;
    # finally print the result #
    printf((matrix fmt,m))
  );

  # finally print the result #
  print(("Product of a and b: ",new line));
  real matrix printf(real fmt, prod)
  EXIT

  exception index error:
    putf(stand error, $x"Exception: index error."l$)
)
```

{{out}}

```txt

 Product of a and b:
 ((  1.00, -0.00, -0.00, -0.00),
  ( -0.00,  1.00, -0.00, -0.00),
  ( -0.00, -0.00,  1.00, -0.00),
  ( -0.00, -0.00, -0.00,  1.00));

```



### Parallel processing

Alternatively - for multicore CPUs - use the following parallel code... The next step might be to augment with [http://www.csse.monash.edu.au/~lloyd/tildeProgLang/Algol68/strassen.a68 Strassen's O(n^log2(7)) recursive matrix multiplication algorithm]:
 '''int''' default upb := 3;
 '''mode''' '''field''' = '''long''' '''real''';
 '''mode''' '''vector''' = [default upb]'''field''';
 '''mode''' '''matrix''' = [default upb, default upb]'''field''';

 &cent; crude exception handling &cent;
 '''proc''' '''void''' raise index error := '''void''': '''goto''' exception index error;

 '''sema''' idle cpus = '''level''' ( 8 - 1 ); &cent; 8 = number of CPU cores minus parent CPU &cent;

 &cent; define an operator to slice array into quarters &cent;
 '''op''' '''top''' = ('''matrix''' m)'''int''': ( &lfloor;m + &lceil;m ) %2,
    '''bot''' = ('''matrix''' m)'''int''': '''top''' m + 1,
    '''left''' = ('''matrix''' m)'''int''': ( 2 &lfloor;m + 2 &lceil;m ) %2,
    '''right''' = ('''matrix''' m)'''int''': '''left''' m + 1,
    '''left''' = ('''vector''' v)'''int''': ( &lfloor;v + &lceil;v ) %2,
    '''right''' = ('''vector''' v)'''int''': '''left''' v + 1;
 '''prio''' '''top''' = 8, '''bot''' = 8, '''left''' = 8, '''right''' = 8; &cent; Operator priority - same as LWB & UPB &cent;

 '''op''' &times; = ('''vector''' a, b)'''field''': ( &cent; dot product &cent;
   '''if''' (&lfloor;a, &lceil;a) &ne; (&lfloor;b, &lceil;b) '''then''' raise index error '''fi''';
   '''if''' &lfloor;a = &lceil;a '''then'''
     a[&lceil;a] &times; b[&lceil;b]
   '''else'''
     '''field''' begin, end;
     []'''proc''' '''void''' schedule=(
       '''void''': begin:=a[:'''left''' a] &times; b[:'''left''' b],
       '''void''': end  :=a['''right''' a:] &times; b['''right''' b:]
     );
     '''if''' '''level''' idle cpus = 0 '''then''' &cent; use current CPU &cent;
       '''for''' thread '''to''' &lceil;schedule '''do''' schedule[thread] '''od'''
     '''else'''
       '''par''' ( &cent; run vector in parallel &cent;
         schedule[1], &cent; assume parent CPU &cent;
         ( &darr;idle cpus; schedule[2]; &uarr;idle cpus)
       )
     '''fi''';
     begin+end
   '''fi'''
 );

 '''op''' &times; = ('''matrix''' a, b)'''matrix''': &cent; matrix multiply &cent;
   '''if''' (&lfloor;a, 2 &lfloor;b) = (&lceil;a, 2 &lceil;b) '''then'''
     a[&lfloor;a, ] &times; b[, 2 &lceil;b] &cent; dot product &cent;
   '''else'''
     [&lceil;a, 2 &lceil;b] '''field''' out;
     '''if''' (2 &lfloor;a, 2 &lceil;a) &ne; (&lfloor;b, &lceil;b) '''then''' raise index error '''fi''';
     []'''struct'''('''bool''' required, '''proc''' '''void''' thread) schedule = (
       ( '''true''', &cent; calculate top left corner &cent;
         '''void''': out[:'''top''' a, :'''left''' b] := a[:'''top''' a, ] &times; b[, :'''left''' b]),
       ( &lfloor;a &ne; &lceil;a, &cent; calculate bottom left corner &cent;
         '''void''': out['''bot''' a:, :'''left''' b] := a['''bot''' a:, ] &times; b[, :'''left''' b]),
       ( 2 &lfloor;b &ne; 2 &lceil;b, &cent; calculate top right corner &cent;
         '''void''': out[:'''top''' a, '''right''' b:] := a[:'''top''' a, ] &times; b[, '''right''' b:]),
       ( (&lfloor;a, 2 &lfloor;b) &ne; (&lceil;a, 2 &lceil;b) , &cent; calculate bottom right corner &cent;
         '''void''': out['''bot''' a:, '''right''' b:] := a['''bot''' a:, ] &times; b[, '''right''' b:])
     );
     '''if''' '''level''' idle cpus = 0 '''then''' &cent; use current CPU &cent;
       '''for''' thread '''to''' &lceil;schedule '''do''' (required &rarr;schedule[thread] | thread &rarr;schedule[thread] ) '''od'''
     '''else'''
       '''par''' ( &cent; run vector in parallel &cent;
         thread &rarr;schedule[1], &cent; thread is always required, and assume parent CPU &cent;
         ( required &rarr;schedule[4] | &darr;idle cpus; thread &rarr;schedule[4]; &uarr;idle cpus),
            &cent; try to do opposite corners of matrix in parallel if CPUs are limited &cent;
         ( required &rarr;schedule[3] | &darr;idle cpus; thread &rarr;schedule[3]; &uarr;idle cpus),
         ( required &rarr;schedule[2] | &darr;idle cpus; thread &rarr;schedule[2]; &uarr;idle cpus)
       )
     '''fi''';
     out
   '''fi''';

 '''format''' real fmt = $g(-6,2)$; &cent; width of 6, with no '+' sign, 2 decimals &cent;
 '''proc''' real matrix printf= ('''format''' real fmt, '''matrix''' m)'''void''':(
   '''format''' vector fmt = $"("n(2 &lceil;m-1)(f(real fmt)",")f(real fmt)")"$;
   '''format''' matrix fmt = $x"("n(&lceil;m-1)(f(vector fmt)","lxx)f(vector fmt)");"$;
   &cent; finally print the result &cent;
   printf((matrix fmt,m))
 );

 &cent; Some sample matrices to test &cent;
 '''matrix''' a=((1,  1,  1,   1), &cent; matrix A &cent;
           (2,  4,  8,  16),
           (3,  9, 27,  81),
           (4, 16, 64, 256));

 '''matrix''' b=((  4  , -3  ,  4/3,  -1/4 ), &cent; matrix B &cent;
           (-13/3, 19/4, -7/3,  11/24),
           (  3/2, -2  ,  7/6,  -1/4 ),
           ( -1/6,  1/4, -1/6,   1/24));

 '''matrix''' c = a &times; b; &cent; actual multiplication example of A x B &cent;

 print((" A x B =",new line));
 real matrix printf(real fmt, c).

 exception index error:
   putf(stand error, $x"Exception: index error."l$)


## AppleScript

{{trans|JavaScript}}

```AppleScript
-- matrixMultiply :: Num a => [[a]] -> [[a]] -> [[a]]
to matrixMultiply(a, b)
    script rows
        property xs : transpose(b)

        on |λ|(row)
            script columns
                on |λ|(col)
                    my dotProduct(row, col)
                end |λ|
            end script

            map(columns, xs)
        end |λ|
    end script

    map(rows, a)
end matrixMultiply


-- TEST -----------------------------------------------------------
on run
    matrixMultiply({¬
        {-1, 1, 4}, ¬
        {6, -4, 2}, ¬
        {-3, 5, 0}, ¬
        {3, 7, -2} ¬
            }, {¬
        {-1, 1, 4, 8}, ¬
        {6, 9, 10, 2}, ¬
        {11, -4, 5, -3}})

    --> {{51, -8, 26, -18}, {-8, -38, -6, 34},
    --     {33, 42, 38, -14}, {17, 74, 72, 44}}
end run


-- GENERIC FUNCTIONS ----------------------------------------------

-- dotProduct :: [n] -> [n] -> Maybe n
on dotProduct(xs, ys)
    script mult
        on |λ|(a, b)
            a * b
        end |λ|
    end script

    if length of xs is not length of ys then
        missing value
    else
        sum(zipWith(mult, xs, ys))
    end if
end dotProduct

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

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

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

-- product :: Num a => [a] -> a
on product(xs)
    script mult
        on |λ|(a, b)
            a * b
        end |λ|
    end script

    foldr(mult, 1, xs)
end product

-- sum :: Num a => [a] -> a
on sum(xs)
    script add
        on |λ|(a, b)
            a + b
        end |λ|
    end script

    foldr(add, 0, xs)
end sum

-- transpose :: [[a]] -> [[a]]
on transpose(xss)
    script column
        on |λ|(_, iCol)
            script row
                on |λ|(xs)
                    item iCol of xs
                end |λ|
            end script

            map(row, xss)
        end |λ|
    end script

    map(column, item 1 of xss)
end transpose

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
on zipWith(f, xs, ys)
    set lng to min(length of xs, length of ys)
    set lst to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, item i of ys)
        end repeat
        return lst
    end tell
end zipWith
```

{{Out}}

```AppleScript
{{51, -8, 26, -18}, {-8, -38, -6, 34}, {33, 42, 38, -14}, {17, 74, 72, 44}}
```



## APL

Matrix multiply in APL is just <tt>+.×</tt>.  For example:


```apl
    x  ←  +.×

    A  ←  ↑A*¨⊂A←⍳4   ⍝  Same  A  as in other examples (1 1 1 1⍪ 2 4 8 16⍪ 3 9 27 81,[0.5] 4 16 64 256)
    B  ←  ⌹A          ⍝  Matrix inverse of A

    'F6.2' ⎕FMT A x B
1.00  0.00  0.00  0.00
0.00  1.00  0.00  0.00
0.00  0.00  1.00  0.00
0.00  0.00  0.00  1.00
```


By contrast, A×B is for element-by-element multiplication of arrays of the same shape, and if they are simple elements, this is ordinary multiplication.


## AutoHotkey

ahk [http://www.autohotkey.com/forum/topic44657-150.html discussion]

```autohotkey
Matrix("b","  ; rows separated by ","
, 1   2       ; entries separated by space or tab
, 2   3
, 3   0")
MsgBox % "B`n`n" MatrixPrint(b)
Matrix("c","
, 1 2 3
, 3 2 1")
MsgBox % "C`n`n" MatrixPrint(c)

MatrixMul("a",b,c)
MsgBox % "B * C`n`n" MatrixPrint(a)

MsgBox % MatrixMul("x",b,b)


Matrix(_a,_v) { ; Matrix structure: m_0_0 = #rows, m_0_1 = #columns, m_i_j = element[i,j], i,j > 0
   Local _i, _j = 0
   Loop Parse, _v, `,
      If (A_LoopField != "") {
         _i := 0, _j ++
         Loop Parse, A_LoopField, %A_Space%%A_Tab%
            If (A_LoopField != "")
               _i++, %_a%_%_i%_%_j% := A_LoopField
      }
   %_a% := _a, %_a%_0_0 := _j, %_a%_0_1 := _i
}
MatrixPrint(_a) {
   Local _i = 0, _t
   Loop % %_a%_0_0 {
      _i++
      Loop % %_a%_0_1
         _t .= %_a%_%A_Index%_%_i% "`t"
      _t .= "`n"
   }
   Return _t
}
MatrixMul(_a,_b,_c) {
   Local _i = 0, _j, _k, _s
   If (%_b%_0_0 != %_c%_0_1)
      Return "ERROR: inner dimensions " %_b%_0_0 " != " %_c%_0_1
   %_a% := _a, %_a%_0_0 := %_b%_0_0, %_a%_0_1 := %_c%_0_1
   Loop % %_c%_0_1 {
      _i++, _j := 0
      Loop % %_b%_0_0 {
         _j++, _k := _s := 0
         Loop % %_b%_0_1
            _k++, _s += %_b%_%_k%_%_j% * %_c%_%_i%_%_k%
         %_a%_%_i%_%_j% := _s
      }
   }
}
```


### Using Objects


```AutoHotkey
Multiply_Matrix(A,B){
	if (A[1].MaxIndex() <> B.MaxIndex())
		return
	RCols := A[1].MaxIndex()>B[1].MaxIndex()?A[1].MaxIndex():B[1].MaxIndex()
	RRows := A.MaxIndex()>B.MaxIndex()?A.MaxIndex():B.MaxIndex(),	 R := []
	Loop, % RRows {
		RRow:=A_Index
		loop, % RCols {
			RCol:=A_Index,			v := 0
			loop % A[1].MaxIndex()
				col := A_Index,		v += A[RRow, col] * B[col,RCol]
			R[RRow,RCol] := v
		}
	}
	return R
}
```

Examples:
```AutoHotkey
A := [[1,2]
	, [3,4]
	, [5,6]
	, [7,8]]

B := [[1,2,3]
	, [4,5,6]]

if Res := Multiply_Matrix(A,B)
	MsgBox % Print(Res)
else
	MsgBox Error
return
Print(M){
	for i, row in M
		for j, col in row
			Res .= (A_Index=1?"":"`t") col (Mod(A_Index,M[1].MaxIndex())?"":"`n")
	return Trim(Res,"`n")
}
```

{{out}}

```txt
9	12	15
19	26	33
29	40	51
39	54	69
```



## AWK


```AWK
# Usage: GAWK -f MATRIX_MULTIPLICATION.AWK filename
# Separate matrices a and b by a blank line
BEGIN {
    ranka1 = 0; ranka2 = 0
    rankb1 = 0; rankb2 = 0
    matrix = 1 # Indicate first (1) or second (2) matrix
    i = 0
}
NF == 0 {
    if (++matrix > 2) {
        printf("Warning: Ignoring data below line %d.\n", NR)
    }
    i = 0
    next
}
{
    # Store first matrix in a, second matrix in b
    if (matrix == 1) {
        ranka1 = ++i
        ranka2 = max(ranka2, NF)
        for (j = 1; j <= NF; j++)
            a[i,j] = $j
    }
    if (matrix == 2) {
        rankb1 = ++i
        rankb2 = max(rankb2, NF)
        for (j = 1; j <= NF; j++)
            b[i,j] = $j
    }
}
END {
    # Check ranks of a and b
    if ((ranka1 < 1) || (ranka2 < 1) || (rankb1 < 1) || (rankb2 < 1) ||
        (ranka2 != rankb1)) {
        printf("Error: Incompatible ranks (%dx%d)*(%dx%d).\n", ranka1, ranka2, rankb1, rankb2)
        exit
    }
    # Multiplication c = a * b
    for (i = 1; i <= ranka1; i++) {
        for (j = 1; j <= rankb2; j++) {
            c[i,j] = 0
            for (k = 1; k <= ranka2; k++)
                c[i,j] += a[i,k] * b[k,j]
        }
    }
    # Print matrix c
    for (i = 1; i <= ranka1; i++) {
        for (j = 1; j <= rankb2; j++)
            printf("%g%s", c[i,j], j < rankb2 ? " " : "\n")
    }
}
function max(m, n) {
    return m > n ? m : n
}
```

<p><b>Input:</b></p>

```txt

6.5 2 3
4.5 1 5

10 16 23 50
12 -8 16 -4
70 60 -1 -2

```

{{out}}

```txt

299. 268. 178.5 311.
407. 364. 114.5 211.

```



## BASIC

{{works with|QuickBasic|4.5}}
{{trans|Java}}

```qbasic
Assume the matrices to be multiplied are a and b
 IF (LEN(a,2) = LEN(b)) 'if valid dims
        n = LEN(a,2)
        m = LEN(a)
        p = LEN(b,2)

        DIM ans(0 TO m - 1, 0 TO p - 1)

        FOR i = 0 TO m - 1
                FOR j = 0 TO p - 1
                        FOR k = 0 TO n - 1
                                ans(i, j) = ans(i, j) + (a(i, k) * b(k, j))
                        NEXT k, j, i

        'print answer
        FOR i = 0 TO m - 1
                FOR j = 0 TO p - 1
                        PRINT ans(i, j);
                NEXT j
                PRINT
        NEXT i
 ELSE
        PRINT "invalid dimensions"
 END IF
```



### Full BASIC

{{works with|Full BASIC}}
{{trans|BBC_BASIC}}

```basic
DIM matrix1(4,2),matrix2(2,3)

MAT READ matrix1
DATA 1,2
DATA 3,4
DATA 5,6
DATA 7,8

MAT READ matrix2
DATA 1,2,3
DATA 4,5,6

MAT product=matrix1*matrix2
MAT PRINT product
```

{{out}}

```txt
 9             12            15
 19            26            33
 29            40            51
 39            54            69
```



## BBC BASIC

BBC BASIC has built-in matrix multiplication
(assumes default lower bound of 0):


```bbcbasic
      DIM matrix1(3,1), matrix2(1,2), product(3,2)

      matrix1() = 1, 2, \
      \           3, 4, \
      \           5, 6, \
      \           7, 8

      matrix2() = 1, 2, 3, \
      \           4, 5, 6

      product() = matrix1() . matrix2()

      FOR row% = 0 TO DIM(product(),1)
        FOR col% = 0 TO DIM(product(),2)
          PRINT product(row%,col%),;
        NEXT
        PRINT
      NEXT

```

{{out}}

```txt
         9        12        15
        19        26        33
        29        40        51
        39        54        69
```




## Burlesque



```burlesque

blsq ) {{1 2}{3 4}{5 6}{7 8}}{{1 2 3}{4 5 6}}mmsp
9 12 15
19 26 33
29 40 51
39 54 69

```



## C

For performance critical work involving matrices, especially large or sparse ones, always consider using an established library such as BLAS first.

```c
#include <stdio.h>
#include <stdlib.h>

/* Make the data structure self-contained.  Element at row i and col j
   is x[i * w + j].  More often than not, though,  you might want
   to represent a matrix some other way */
typedef struct { int h, w; double *x;} matrix_t, *matrix;

inline double dot(double *a, double *b, int len, int step)
{
	double r = 0;
	while (len--) {
		r += *a++ * *b;
		b += step;
	}
	return r;
}

matrix mat_new(int h, int w)
{
	matrix r = malloc(sizeof(matrix_t) + sizeof(double) * w * h);
	r->h = h, r->w = w;
	r->x = (double*)(r + 1);
	return r;
}

matrix mat_mul(matrix a, matrix b)
{
	matrix r;
	double *p, *pa;
	int i, j;
	if (a->w != b->h) return 0;

	r = mat_new(a->h, b->w);
	p = r->x;
	for (pa = a->x, i = 0; i < a->h; i++, pa += a->w)
		for (j = 0; j < b->w; j++)
			*p++ = dot(pa, b->x + j, a->w, b->w);
	return r;
}

void mat_show(matrix a)
{
	int i, j;
	double *p = a->x;
	for (i = 0; i < a->h; i++, putchar('\n'))
		for (j = 0; j < a->w; j++)
			printf("\t%7.3f", *p++);
	putchar('\n');
}

int main()
{
	double da[] = {	1, 1,  1,   1,
			2, 4,  8,  16,
			3, 9, 27,  81,
			4,16, 64, 256	};
	double db[] = {     4.0,   -3.0,  4.0/3,
			-13.0/3, 19.0/4, -7.0/3,
			  3.0/2,   -2.0,  7.0/6,
			 -1.0/6,  1.0/4, -1.0/6};

	matrix_t a = { 4, 4, da }, b = { 4, 3, db };
	matrix c = mat_mul(&a, &b);

	/* mat_show(&a), mat_show(&b); */
	mat_show(c);
	/* free(c) */
	return 0;
}
```


=={{header|C sharp|C#}}==

This code should work with any version of the .NET Framework and C# language


```csharp
public class Matrix
{
	int n;
	int m;
	double[,] a;

	public Matrix(int n, int m)
	{
		if (n <= 0 || m <= 0)
			throw new ArgumentException("Matrix dimensions must be positive");
		this.n = n;
		this.m = m;
		a = new double[n, m];
	}

	//indices start from one
	public double this[int i, int j]
	{
		get { return a[i - 1, j - 1]; }
		set { a[i - 1, j - 1] = value; }
	}

	public int N { get { return n; } }
	public int M { get { return m; } }

	public static Matrix operator*(Matrix _a, Matrix b)
	{
		int n = _a.N;
		int m = b.M;
		int l = _a.M;
		if (l != b.N)
			throw new ArgumentException("Illegal matrix dimensions for multiplication. _a.M must be equal b.N");
		Matrix result = new Matrix(_a.N, b.M);
		for(int i = 0; i < n; i++)
			for (int j = 0; j < m; j++)
			{
				double sum = 0.0;
				for (int k = 0; k < l; k++)
					sum += _a.a[i, k]*b.a[k, j];
				result.a[i, j] = sum;
			}
		return result;
	}
}
```



## C++


{{works with|Visual C++ 2010}}

{{libheader|Blitz++}}

```cpp
#include <iostream>
#include <blitz/tinymat.h>

int main()
{
  using namespace blitz;

  TinyMatrix<double,3,3> A, B, C;

  A = 1, 2, 3,
      4, 5, 6,
      7, 8, 9;

  B = 1, 0, 0,
      0, 1, 0,
      0, 0, 1;

  C = product(A, B);

  std::cout << C << std::endl;
}
```

{{out}}
 (3,3):
  [          1         2         3 ]
  [          4         5         6 ]
  [          7         8         9 ]


### Generic solution

main.cpp

```cpp

#include <iostream>
#include "matrix.h"

#if !defined(ARRAY_SIZE)
    #define ARRAY_SIZE(x) (sizeof((x)) / sizeof((x)[0]))
#endif

int main() {
    int  am[2][3] = {
        {1,2,3},
        {4,5,6},
    };
    int  bm[3][2] = {
        {1,2},
        {3,4},
        {5,6}
    };

    Matrix<int> a(ARRAY_SIZE(am), ARRAY_SIZE(am[0]), am[0], ARRAY_SIZE(am)*ARRAY_SIZE(am[0]));
    Matrix<int> b(ARRAY_SIZE(bm), ARRAY_SIZE(bm[0]), bm[0], ARRAY_SIZE(bm)*ARRAY_SIZE(bm[0]));
    Matrix<int> c;

    try {
        c = a * b;
        for (unsigned int i = 0; i < c.rowNum(); i++) {
            for (unsigned int j = 0; j < c.colNum(); j++) {
                std::cout <<  c[i][j] << "  ";
            }
            std::cout << std::endl;
        }
    } catch (MatrixException& e) {
        std::cerr << e.message() << std::endl;
        return e.errorCode();
    }

} /* main() */

```


matrix.h

```cpp

#ifndef _MATRIX_H
#define	_MATRIX_H

#include <sstream>
#include <string>
#include <vector>

#define MATRIX_ERROR_CODE_COUNT 5
#define MATRIX_ERR_UNDEFINED "1 Undefined exception!"
#define MATRIX_ERR_WRONG_ROW_INDEX "2 The row index is out of range."
#define MATRIX_ERR_MUL_ROW_AND_COL_NOT_EQUAL "3 The row number of second matrix must be equal with the column number of first matrix!"
#define MATRIX_ERR_MUL_ROW_AND_COL_BE_GREATER_THAN_ZERO "4 The number of rows and columns must be greater than zero!"
#define MATRIX_ERR_TOO_FEW_DATA "5 Too few data in matrix."

class MatrixException {
private:
    std::string message_;
    int errorCode_;
public:
    MatrixException(std::string message = MATRIX_ERR_UNDEFINED);

    inline std::string message() {
        return message_;
    };

    inline int errorCode() {
        return errorCode_;
    };
};

MatrixException::MatrixException(std::string message) {
    errorCode_ = MATRIX_ERROR_CODE_COUNT + 1;
    std::stringstream ss(message);
    ss >> errorCode_;
    if (errorCode_ < 1) {
        errorCode_ = MATRIX_ERROR_CODE_COUNT + 1;
    }
    std::string::size_type pos = message.find(' ');
    if (errorCode_ <= MATRIX_ERROR_CODE_COUNT && pos != std::string::npos) {
        message_ = message.substr(pos + 1);
    } else {
        message_ = message + " (This an unknown and unsupported exception!)";
    }
}

/**
 * Generic class for matrices.
 */
template <class T>
class Matrix {
private:
    std::vector<T> v; // the data of matrix
    unsigned int m;   // the number of rows
    unsigned int n;   // the number of columns
protected:

    virtual void clear() {
        v.clear();
        m = n = 0;
    }
public:

    Matrix() {
        clear();
    }
    Matrix(unsigned int, unsigned int, T* = 0, unsigned int = 0);
    Matrix(unsigned int, unsigned int, const std::vector<T>&);

    virtual ~Matrix() {
        clear();
    }
    Matrix& operator=(const Matrix&);
    std::vector<T> operator[](unsigned int) const;
    Matrix operator*(const Matrix&);

    inline unsigned int rowNum() const {
        return m;
    }

    inline unsigned int colNum() const {
        return n;
    }

    inline unsigned int size() const {
        return v.size();
    }

    inline void add(const T& t) {
        v.push_back(t);
    }
};

template <class T>
Matrix<T>::Matrix(unsigned int row, unsigned int col, T* data, unsigned int dataLength) {
    clear();
    if (row > 0 && col > 0) {
        m = row;
        n = col;
        unsigned int mxn = m * n;
        if (dataLength && data) {
            for (unsigned int i = 0; i < dataLength && i < mxn; i++) {
                v.push_back(data[i]);
            }
        }
    }
}

template <class T>
Matrix<T>::Matrix(unsigned int row, unsigned int col, const std::vector<T>& data) {
    clear();
    if (row > 0 && col > 0) {
        m = row;
        n = col;
        unsigned int mxn = m * n;
        if (data.size() > 0) {
            for (unsigned int i = 0; i < mxn && i < data.size(); i++) {
                v.push_back(data[i]);
            }
        }
    }
}

template<class T>
Matrix<T>& Matrix<T>::operator=(const Matrix<T>& other) {
    clear();
    if (other.m > 0 && other.n > 0) {
        m = other.m;
        n = other.n;
        unsigned int mxn = m * n;
        for (unsigned int i = 0; i < mxn && i < other.size(); i++) {
            v.push_back(other.v[i]);
        }
    }
    return *this;
}

template<class T>
std::vector<T> Matrix<T>::operator[](unsigned int index) const {
    std::vector<T> result;
    if (index >= m) {
        throw MatrixException(MATRIX_ERR_WRONG_ROW_INDEX);
    } else if ((index + 1) * n > size()) {
        throw MatrixException(MATRIX_ERR_TOO_FEW_DATA);
    } else {
        unsigned int begin = index * n;
        unsigned int end = begin + n;
        for (unsigned int i = begin; i < end; i++) {
            result.push_back(v[i]);
        }
    }
    return result;
}

template<class T>
Matrix<T> Matrix<T>::operator*(const Matrix<T>& other) {
    Matrix result(m, other.n);
    if (n != other.m) {
        throw MatrixException(MATRIX_ERR_MUL_ROW_AND_COL_NOT_EQUAL);
    } else if (m <= 0 || n <= 0 || other.n <= 0) {
        throw MatrixException(MATRIX_ERR_MUL_ROW_AND_COL_BE_GREATER_THAN_ZERO);
    } else if (m * n > size() || other.m * other.n > other.size()) {
        throw MatrixException(MATRIX_ERR_TOO_FEW_DATA);
    } else {
        for (unsigned int i = 0; i < m; i++) {
            for (unsigned int j = 0; j < other.n; j++) {
                T temp = v[i * n] * other.v[j];
                for (unsigned int k = 1; k < n; k++) {
                    temp += v[i * n + k] * other.v[k * other.n + j];
                }
                result.v.push_back(temp);
            }
        }
    }
    return result;
}

#endif	/* _MATRIX_H */

```


{{out}}

```txt

22  28
49  64

```



## Ceylon


```ceylon>alias Matrix =
 Integer[][];

void printMatrix(Matrix m) {
	value strings = m.collect((row) => row.collect(Integer.string));
	value maxLength = max(expand(strings).map(String.size)) else 0;
	value padded = strings.collect((row) => row.collect((s) => s.padLeading(maxLength)));
	for (row in padded) {
		print("[``", ".join(row)``]");
	}
}

Matrix? multiplyMatrices(Matrix a, Matrix b) {

	function rectangular(Matrix m) =>
			if (exists firstRow = m.first)
			then m.every((row) => row.size == firstRow.size)
			else false;

	function rowCount(Matrix m) => m.size;
	function columnCount(Matrix m) => m[0]?.size else 0;

	if (!rectangular(a) || !rectangular(b) || columnCount(a) != rowCount(b)) {
		return null;
	}

	function getNumber(Matrix m, Integer x, Integer y) {
		assert (exists number = m[y]?.get(x));
		return number;
	}

	function getRow(Matrix m, Integer rowIndex) {
		assert (exists row = m[rowIndex]);
		return row;
	}

	function getColumn(Matrix m, Integer columnIndex) => {
		for (y in 0:rowCount(m))
		getNumber(m, columnIndex, y)
	};

	return [
		for (y in 0:rowCount(a)) [
			for (x in 0:columnCount(b))
			sum { 0, for ([a1, b1] in zipPairs(getRow(a, y), getColumn(b, x))) a1 * b1 }
		]
	];
}

shared void run() {
    value m = [[1, 2, 3], [4, 5, 6]];
    printMatrix(m);
    print("---------");
    print("multiplied by");
    value m2 = [[7, 8], [9, 10], [11, 12]];
    printMatrix(m2);
    print("---------");
    print("equals:");
    value result = multiplyMatrices(m, m2);
    if (exists result) {
        printMatrix(result);
    }
    else {
        print("something went wrong!");
    }
}
```

{{out}}

```txt
[1, 2, 3]
[4, 5, 6]
---------
multiplied by
[ 7,  8]
[ 9, 10]
[11, 12]
---------
equals:
[ 58,  64]
[139, 154]
```



## Clojure



```lisp

(defn transpose
  [s]
  (apply map vector s))

(defn nested-for
  [f x y]
  (map (fn [a]
         (map (fn [b]
                (f a b)) y))
       x))

(defn matrix-mult
  [a b]
  (nested-for (fn [x y] (reduce + (map * x y))) a (transpose b)))

(def ma [[1 1 1 1] [2 4 8 16] [3 9 27 81] [4 16 64 256]])
(def mb [[4 -3 4/3 -1/4] [-13/3 19/4 -7/3 11/24] [3/2 -2 7/6 -1/4] [-1/6 1/4 -1/6 1/24]])
```


{{out}}

```txt

=> (matrix-mult ma mb)
((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1))

```



## Common Lisp


```lisp
(defun matrix-multiply (a b)
  (flet ((col (mat i) (mapcar #'(lambda (row) (elt row i)) mat))
         (row (mat i) (elt mat i)))
    (loop for row from 0 below (length a)
          collect (loop for col from 0 below (length (row b 0))
                        collect (apply #'+ (mapcar #'* (row a row) (col b col)))))))

;; example use:
(matrix-multiply '((1 2) (3 4)) '((-3 -8 3) (-2 1 4)))
```



```lisp
(defun matrix-multiply (matrix1 matrix2)
 (mapcar
  (lambda (row)
   (apply #'mapcar
    (lambda (&rest column)
     (apply #'+ (mapcar #'* row column))) matrix2)) matrix1))
```


The following version uses 2D arrays as inputs.


```lisp
(defun mmul (A B)
  (let* ((m (car (array-dimensions A)))
         (n (cadr (array-dimensions A)))
         (l (cadr (array-dimensions B)))
         (C (make-array `(,m ,l) :initial-element 0)))
    (loop for i from 0 to (- m 1) do
              (loop for k from 0 to (- l 1) do
                    (setf (aref C i k)
                          (loop for j from 0 to (- n 1)
                                sum (* (aref A i j)
                                       (aref B j k))))))
    C))
```


Example use:


```lisp
(mmul #2a((1 2) (3 4)) #2a((-3 -8 3) (-2 1 4)))
#2A((-7 -6 11) (-17 -20 25))

```


Another version:


```lisp
(defun mmult (a b)
  (loop
       with m = (array-dimension a 0)
       with n = (array-dimension a 1)
       with l = (array-dimension b 1)
       with c = (make-array (list m l) :initial-element 0)
       for i below m do
              (loop for k below l do
                    (setf (aref c i k)
                          (loop for j below n
                                sum (* (aref a i j)
                                       (aref b j k)))))
       finally (return c)))
```



## Chapel


Overload the '*' operator for arrays

```chapel
proc *(a:[], b:[]) {

    if (a.eltType != b.eltType) then
        writeln("type mismatch: ", a.eltType, " ", b.eltType);

    var ad = a.domain.dims();
    var bd = b.domain.dims();
    var (arows, acols) = ad;
    var (brows, bcols) = bd;
    if (arows != bcols) then
        writeln("dimension mismatch: ", ad, " ", bd);

    var c:[{arows, bcols}] a.eltType = 0;

    for i in arows do
        for j in bcols do
            for k in acols do
                c(i,j) += a(i,k) * b(k,j);

    return c;
}
```


example usage (I could not figure out the syntax for multi-dimensional array literals)

```chapel
var m1:[{1..2, 1..2}] int;
m1(1,1) = 1; m1(1,2) = 2;
m1(2,1) = 3; m1(2,2) = 4;
writeln(m1);

var m2:[{1..2, 1..2}] int;
m2(1,1) = 2; m2(1,2) = 3;
m2(2,1) = 4; m2(2,2) = 5;
writeln(m2);

var m3 = m1 * m2;
writeln(m3);

var m4:[{1..2, 1..3}] int;
m4(1, 1) = 1; m4(1, 2) = 2; m4(1, 3) = 3;
m4(2, 1) = 4; m4(2, 2) = 5; m4(2, 3) = 6;
writeln(m4);

var m5:[{1..3, 1..2}] int;
m5(1, 1) = 6; m5(1, 2) = -1;
m5(2, 1) = 3; m5(2, 2) =  2;
m5(3, 1) = 0; m5(3, 2) = -3;
writeln(m5);

writeln(m4 * m5);
```



## D


### Basic Version


```d
import std.stdio, std.string, std.conv, std.numeric,
       std.array, std.algorithm;

bool isRectangular(T)(in T[][] M) pure nothrow {
    return M.all!(row => row.length == M[0].length);
}

T[][] matrixMul(T)(in T[][] A, in T[][] B) pure nothrow
in {
    assert(A.isRectangular && B.isRectangular &&
           !A.empty && !B.empty && A[0].length == B.length);
} body {
    auto result = new T[][](A.length, B[0].length);
    auto aux = new T[B.length];

    foreach (immutable j; 0 .. B[0].length) {
        foreach (immutable k, const row; B)
            aux[k] = row[j];
        foreach (immutable i, const ai; A)
            result[i][j] = dotProduct(ai, aux);
    }

    return result;
}

void main() {
    immutable a = [[1, 2], [3, 4], [3, 6]];
    immutable b = [[-3, -8, 3,], [-2, 1, 4]];

    immutable form = "[%([%(%d, %)],\n %)]]";
    writefln("A = \n" ~ form ~ "\n", a);
    writefln("B = \n" ~ form ~ "\n", b);
    writefln("A * B = \n" ~ form, matrixMul(a, b));
}
```

{{out}}

```txt
A =
[[1, 2],
 [3, 4],
 [3, 6]]

B =
[[-3, -8, 3],
 [-2, 1, 4]]

A * B =
[[-7, -6, 11],
 [-17, -20, 25],
 [-21, -18, 33]]
```



### Short Version


```d
import std.stdio, std.range, std.array, std.numeric, std.algorithm;

T[][] matMul(T)(in T[][] A, in T[][] B) pure nothrow /*@safe*/ {
    const Bt = B[0].length.iota.map!(i=> B.transversal(i).array).array;
    return A.map!(a => Bt.map!(b => a.dotProduct(b)).array).array;
}

void main() {
    immutable a = [[1, 2], [3, 4], [3, 6]];
    immutable b = [[-3, -8, 3,], [-2, 1, 4]];

    immutable form = "[%([%(%d, %)],\n %)]]";
    writefln("A = \n" ~ form ~ "\n", a);
    writefln("B = \n" ~ form ~ "\n", b);
    writefln("A * B = \n" ~ form, matMul(a, b));
}
```

The output is the same.


### Pure Short Version


```d
import std.stdio, std.range, std.numeric, std.algorithm;

T[][] matMul(T)(immutable T[][] A, immutable T[][] B) pure nothrow {
    immutable Bt = B[0].length.iota.map!(i=> B.transversal(i).array)
                   .array;
    return A.map!((in a) => Bt.map!(b => a.dotProduct(b)).array).array;
}

void main() {
    immutable a = [[1, 2], [3, 4], [3, 6]];
    immutable b = [[-3, -8, 3,], [-2, 1, 4]];

    immutable form = "[%([%(%d, %)],\n %)]]";
    writefln("A = \n" ~ form ~ "\n", a);
    writefln("B = \n" ~ form ~ "\n", b);
    writefln("A * B = \n" ~ form, matMul(a, b));
}
```

The output is the same.


### Stronger Statically Typed Version

All array sizes are verified at compile-time (and no matrix is copied).
Same output.

```d
import std.stdio, std.string, std.numeric, std.algorithm, std.traits;

alias TMMul_helper(M1, M2) = Unqual!(ForeachType!(ForeachType!M1))
                             [M2.init[0].length][M1.length];

void matrixMul(T, T2, size_t k, size_t m, size_t n)
              (in ref T[m][k] A, in ref T[n][m] B,
               /*out*/ ref T2[n][k] result) pure nothrow /*@safe*/ @nogc
if (is(T2 == Unqual!T)) {
    static if (hasIndirections!T)
        T2[m] aux;
    else
        T2[m] aux = void;

    foreach (immutable j; 0 .. n) {
        foreach (immutable i, const ref bi; B)
            aux[i] = bi[j];
        foreach (immutable i, const ref ai; A)
            result[i][j] = dotProduct(ai, aux);
    }
}

void main() {
    immutable int[2][3] a = [[1, 2], [3, 4], [3, 6]];
    immutable int[3][2] b = [[-3, -8, 3,], [-2, 1, 4]];

    enum form = "[%([%(%d, %)],\n %)]]";
    writefln("A = \n" ~ form ~ "\n", a);
    writefln("B = \n" ~ form ~ "\n", b);
    TMMul_helper!(typeof(a), typeof(b)) result = void;
    matrixMul(a, b, result);
    writefln("A * B = \n" ~ form, result);
}
```



## Ela



```ela
open list

mmult a b = [ [ sum $ zipWith (*) ar bc \\ bc <- (transpose b) ] \\ ar <- a ]

[[1, 2],
 [3, 4]] `mmult` [[-3, -8, 3],
                  [-2,  1, 4]]
```



## ELLA

Sample originally from ftp://ftp.dra.hmg.gb/pub/ella (a now dead link) - Public release.

Code for matrix multiplication hardware design verification:

```ella
MAC ZIP = ([INT n]TYPE t: vector1 vector2) -> [n][2]t:
  [INT k = 1..n](vector1[k], vector2[k]).

MAC TRANSPOSE = ([INT n][INT m]TYPE t: matrix) -> [m][n]t:
  [INT i = 1..m] [INT j = 1..n] matrix[j][i].

MAC INNER_PRODUCT{FN * = [2]TYPE t -> TYPE s, FN + = [2]s -> s}
                 = ([INT n][2]t: vector) -> s:
  IF n = 1 THEN *vector[1]
  ELSE *vector[1] + INNER_PRODUCT {*,+} vector[2..n]
  FI.

MAC MATRIX_MULT {FN * = [2]TYPE t->TYPE s, FN + = [2]s->s} =
([INT n][INT m]t: matrix1, [m][INT p]t: matrix2) -> [n][p]s:
BEGIN
  LET transposed_matrix2 = TRANSPOSE matrix2.
OUTPUT [INT i = 1..n][INT j = 1..p]
       INNER_PRODUCT{*,+}ZIP(matrix1[i],transposed_matrix2[j])
END.


TYPE element = NEW elt/(1..20),
     product = NEW prd/(1..1200).

FN PLUS = (product: integer1 integer2) -> product:
  ARITH integer1 + integer2.

FN MULT = (element: integer1 integer2) -> product:
  ARITH integer1 * integer2.

FN MULT_234 = ([2][3]element:matrix1, [3][4]element:matrix2) ->
             [2][4]product:
  MATRIX_MULT{MULT,PLUS}(matrix1, matrix2).

FN TEST = () -> [2][4]product:
( LET m1 = ((elt/2, elt/1, elt/1),
            (elt/3, elt/6, elt/9)),
      m2 = ((elt/6, elt/1, elt/3, elt/4),
            (elt/9, elt/2, elt/8, elt/3),
            (elt/6, elt/4, elt/1, elt/2)).
  OUTPUT
    MULT_234 (m1, m2)
).

COM test: just displaysignal MOC
```



## Euphoria


```euphoria
function matrix_mul(sequence a, sequence b)
    sequence c
    if length(a[1]) != length(b) then
        return 0
    else
        c = repeat(repeat(0,length(b[1])),length(a))
        for i = 1 to length(a) do
            for j = 1 to length(b[1]) do
                for k = 1 to length(a[1]) do
                    c[i][j] += a[i][k]*b[k][j]
                end for
            end for
        end for
        return c
    end if
end function
```



## EGL


```EGL

program Matrix_multiplication type BasicProgram {}

	function main()
		a float[][] = [[1,2,3],[4,5,6]];
		b float[][] = [[1,2],[3,4],[5,6]];
		c float[][] = mult(a, b);
	end

	function mult(a float[][], b float[][]) returns(float[][])
		if(a.getSize() == 0)
			return (new float[0][0]);
		end
		if(a[1].getSize() != b.getSize())
			return (null); //invalid dims
		end

	   	n int = a[1].getSize();
	   	m int = a.getSize();
	   	p int = b[1].getSize();

		ans float[0][0];
		ans.resizeAll([m, p]);

		// Calculate dot product.
		for(i int from 1 to m)
			for(j int from 1 to p)
				for(k int from 1 to n)
	            	                ans[i][j] += a[i][k] * b[k][j];
				end
			end
		end
		return (ans);
	end
end

```



## Elixir


```elixir

  def mult(m1, m2) do
    Enum.map m1, fn (x) -> Enum.map t(m2), fn (y) -> Enum.zip(x, y)
        |> Enum.map(fn {x, y} -> x * y end)
        |> Enum.sum
      end
    end
  end

  def t(m) do # transpose
    List.zip(m) |> Enum.map(&Tuple.to_list(&1))
  end



```



## Erlang


```erlang


%% Multiplies two matrices. Usage example:
%% $ matrix:multiply([[1,2,3],[4,5,6]], [[4,4],[0,0],[1,4]])
%% If the dimentions are incompatible, an error is thrown.
%%
%% The erl shell may encode the lists output as strings. In order to prevent such
%% behaviour, BEFORE running matrix:multiply, run shell:strings(false) to disable
%% auto-encoding. When finished, run shell:strings(true) to reset the defaults.

-module(matrix).
-export([multiply/2]).

transpose([[]|_]) ->
    [];
transpose(B) ->
  [lists:map(fun hd/1, B) | transpose(lists:map(fun tl/1, B))].


red(Pair, Sum) ->
    X = element(1, Pair),   %gets X
    Y = element(2, Pair),   %gets Y
    X * Y + Sum.

%% Mathematical dot product. A x B = d
%% A, B = 1-dimension vector
%% d    = scalar
dot_product(A, B) ->
    lists:foldl(fun red/2, 0, lists:zip(A, B)).


%% Exposed function. Expected result is C = A x B.
multiply(A, B) ->
    %% First transposes B, to facilitate the calculations (It's easier to fetch
    %% row than column wise).
    multiply_internal(A, transpose(B)).


%% This function does the actual multiplication, but expects the second matrix
%% to be transposed.
multiply_internal([Head | Rest], B) ->
    % multiply each row by Y
    Element = multiply_row_by_col(Head, B),

    % concatenate the result of this multiplication with the next ones
    [Element | multiply_internal(Rest, B)];

multiply_internal([], B) ->
    % concatenating and empty list to the end of a list, changes nothing.
    [].


multiply_row_by_col(Row, [Col_Head | Col_Rest]) ->
    Scalar = dot_product(Row, Col_Head),

    [Scalar | multiply_row_by_col(Row, Col_Rest)];

multiply_row_by_col(Row, []) ->
    [].

```


{{out}}

```txt

[[7,16],[22,40]]

```




## ERRE


```ERRE

PROGRAM MAT_PROD

DIM A[3,1],B[1,2],ANS[3,2]

BEGIN

DATA(1,2,3,4,5,6,7,8)
DATA(1,2,3,4,5,6)

FOR I=0 TO 3 DO
   FOR J=0 TO 1 DO
      READ(A[I,J])
   END FOR
END FOR

FOR I=0 TO 1 DO
   FOR J=0 TO 2 DO
      READ(B[I,J])
   END FOR
END FOR

FOR I=0 TO UBOUND(ANS,1) DO
  FOR J=0 TO UBOUND(ANS,2) DO
     FOR K=0 TO UBOUND(A,2) DO
        ANS[I,J]=ANS[I,J]+(A[I,K]*B[K,J])
     END FOR
  END FOR
END FOR
! print answer
  FOR I=0 TO UBOUND(ANS,1) DO
     FOR J=0 TO UBOUND(ANS,2) DO
        PRINT(ANS[I,J],)
     END FOR
     PRINT
  END FOR

END PROGRAM

```

{{out}}
```txt

         9        12        15
        19        26        33
        29        40        51
        39        54        69

```



## F#


```F#

let MatrixMultiply (matrix1 : _[,] , matrix2 : _[,]) =
    let result_row = (matrix1.GetLength 0)
    let result_column = (matrix2.GetLength 1)
    let ret = Array2D.create result_row result_column 0
    for x in 0 .. result_row - 1 do
        for y in 0 .. result_column - 1 do
            let mutable acc = 0
            for z in 0 .. (matrix1.GetLength 1) - 1 do
                acc <- acc + matrix1.[x,z] * matrix2.[z,y]
            ret.[x,y] <- acc
    ret


```



## Factor

The built-in word <code>m.</code> multiplies matrices:

 ( scratchpad ) USE: math.matrices
                { { 1 2 } { 3 4 } }  { { -3 -8 3 } { -2 1 4 } } m. .
 { { -7 -6 11 } { -17 -20 25 } }


## Fantom


Using a list of lists representation.  The multiplication is done using three nested loops.


```fantom

class Main
{
  // multiply two matrices (with no error checking)
  public static Int[][] multiply (Int[][] m1, Int[][] m2)
  {
    Int[][] result := [,]
    m1.each |Int[] row1|
    {
      Int[] row := [,]
      m2[0].size.times |Int colNumber|
      {
        Int value := 0
        m2.each |Int[] row2, Int index|
        {
          value += row1[index] * row2[colNumber]
        }
        row.add (value)
      }
     result.add (row)
    }
    return result
  }

  public static Void main ()
  {
    m1 := [[1,2,3],[4,5,6]]
    m2 := [[1,2],[3,4],[5,6]]

    echo ("${m1} times ${m2} = ${multiply(m1,m2)}")
  }
}

```


{{out}}

```txt

[[1, 2, 3], [4, 5, 6]] times [[1, 2], [3, 4], [5, 6]] = [[22, 28], [49, 64]]

```




## Forth

{{libheader|Forth Scientific Library}}
{{works with|gforth|0.7.9_20170308}}

```forth
S" fsl-util.fs" REQUIRED
S" fsl/dynmem.seq" REQUIRED
: F+! ( addr -- ) ( F: r -- )  DUP F@ F+ F! ;
: FSQR ( F: r1 -- r2 ) FDUP F* ;
S" fsl/gaussj.seq" REQUIRED

3 3 float matrix A{{
1e 2e 3e  4e 5e 6e  7e 8e 9e  3 3 A{{ }}fput
3 3 float matrix B{{
3e 3e 3e  2e 2e 2e  1e 1e 1e  3 3 B{{ }}fput
float dmatrix C{{    \ result

A{{ 3 3 B{{ 3 3 & C{{ mat*
3 3 C{{ }}fprint
```



## Fortran

In ISO Fortran 90 or later, use the MATMUL intrinsic function to perform Matrix Multiply; use RESHAPE and SIZE intrinsic functions to form the matrices themselves:

```fortran
real, dimension(n,m) :: a = reshape( [ (i, i=1, n*m) ], [ n, m ] )
real, dimension(m,k) :: b = reshape( [ (i, i=1, m*k) ], [ m, k ] )
real, dimension(size(a,1), size(b,2)) :: c    ! C is an array whose first dimension (row) size
                                              ! is the same as A's first dimension size, and
                                              ! whose second dimension (column) size is the same
                                              ! as B's second dimension size.

c = matmul( a, b )

print *, 'A'
do i = 1, n
    print *, a(i,:)
end do

print *,
print *, 'B'
do i = 1, m
    print *, b(i,:)
end do

print *,
print *, 'C = AB'
do i = 1, n
    print *, c(i,:)
end do
```

For Intel 14.x or later (with compiler switch -assume realloc_lhs)

```fortran

        program mm
          real   , allocatable :: a(:,:),b(:,:)
          integer              :: l=5,m=6,n=4
          a = reshape([1:l*m],[l,m])
          b = reshape([1:m*n],[m,n])
          print'(<n>f15.7)',transpose(matmul(a,b))
        end program

```



## Frink


```frink
matprod[a is array, b is array] :=
{
   c = makeArray[[length[a], length[b@0]], 0]

   a_row = length[a]-1
   a_col = length[a@0]-1
   b_col = length[b]-1

   for row = 0 to a_row
      for col = 0 to b_col
         for inc = 0 to a_col
            c@row@col = c@row@col + (a@row@inc * b@inc@col)

   return c
}
```



## Futhark

{{incorrect|Futhark|Futhark's syntax has changed, so this example will not compile}}

Note that the transposition need not be manifested, but is merely a change of indexing.


```Futhark

fun main(x: [n][m]int, y: [m][p]int): [n][p]int =
  map (fn xr => map (fn yc => reduce (+) 0 (zipWith (*) xr yc))
                    (transpose y))
       x

```



## GAP


```gap
# Built-in
A := [[1, 2], [3, 4], [5, 6], [7, 8]];
B := [[1, 2, 3], [4, 5, 6]];

PrintArray(A);
#  [ [  1,  2 ],
#    [  3,  4 ],
#    [  5,  6 ],
#    [  7,  8 ] ]

PrintArray(B);
#  [ [  1,  2,  3 ],
#    [  4,  5,  6 ] ]

PrintArray(A * B);
#  [ [   9,  12,  15 ],
#    [  19,  26,  33 ],
#    [  29,  40,  51 ],
#    [  39,  54,  69 ] ]
```



## Go


### Library gonum/mat


```go
package main

import (
    "fmt"

    "gonum.org/v1/gonum/mat"
)

func main() {
    a := mat.NewDense(2, 4, []float64{
        1, 2, 3, 4,
        5, 6, 7, 8,
    })
    b := mat.NewDense(4, 3, []float64{
        1, 2, 3,
        4, 5, 6,
        7, 8, 9,
        10, 11, 12,
    })
    var m mat.Dense
    m.Mul(a, b)
    fmt.Println(mat.Formatted(&m))
}
```

{{out}}

```txt

⎡ 70   80   90⎤
⎣158  184  210⎦

```



### Library go.matrix


```go
package main

import (
    "fmt"

    mat "github.com/skelterjohn/go.matrix"
)

func main() {
    a := mat.MakeDenseMatrixStacked([][]float64{
        {1, 2, 3, 4},
        {5, 6, 7, 8},
    })
    b := mat.MakeDenseMatrixStacked([][]float64{
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9},
        {10, 11, 12},
    })
    fmt.Printf("Matrix A:\n%v\n", a)
    fmt.Printf("Matrix B:\n%v\n", b)
    p, err := a.TimesDense(b)
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Product of A and B:\n%v\n", p)
}
```

{{out}}

```txt

Matrix A:
{1, 2, 3, 4,
 5, 6, 7, 8}
Matrix B:
{ 1,  2,  3,
  4,  5,  6,
  7,  8,  9,
 10, 11, 12}
Product of A and B:
{ 70,  80,  90,
 158, 184, 210}

```



### 2D representation


```go
package main

import "fmt"

type Value float64
type Matrix [][]Value

func Multiply(m1, m2 Matrix) (m3 Matrix, ok bool) {
    rows, cols, extra := len(m1), len(m2[0]), len(m2)
    if len(m1[0]) != extra {
        return nil, false
    }
    m3 = make(Matrix, rows)
    for i := 0; i < rows; i++ {
        m3[i] = make([]Value, cols)
        for j := 0; j < cols; j++ {
            for k := 0; k < extra; k++ {
                m3[i][j] += m1[i][k] * m2[k][j]
            }
        }
    }
    return m3, true
}

func (m Matrix) String() string {
    rows := len(m)
    cols := len(m[0])
    out := "["
    for r := 0; r < rows; r++ {
        if r > 0 {
            out += ",\n "
        }
        out += "[ "
        for c := 0; c < cols; c++ {
            if c > 0 {
                out += ", "
            }
            out += fmt.Sprintf("%7.3f", m[r][c])
        }
        out += " ]"
    }
    out += "]"
    return out
}

func main() {
    A := Matrix{[]Value{1, 2, 3, 4},
        []Value{5, 6, 7, 8}}
    B := Matrix{[]Value{1, 2, 3},
        []Value{4, 5, 6},
        []Value{7, 8, 9},
        []Value{10, 11, 12}}
    P, ok := Multiply(A, B)
    if !ok {
        panic("Invalid dimensions")
    }
    fmt.Printf("Matrix A:\n%s\n\n", A)
    fmt.Printf("Matrix B:\n%s\n\n", B)
    fmt.Printf("Product of A and B:\n%s\n\n", P)
}
```

{{out}}

```txt

Matrix A:
[[   1.000,   2.000,   3.000,   4.000 ],
 [   5.000,   6.000,   7.000,   8.000 ]]

Matrix B:
[[   1.000,   2.000,   3.000 ],
 [   4.000,   5.000,   6.000 ],
 [   7.000,   8.000,   9.000 ],
 [  10.000,  11.000,  12.000 ]]

Product of A and B:
[[  70.000,  80.000,  90.000 ],
 [ 158.000, 184.000, 210.000 ]]

```


### Flat representation


```go
package main

import "fmt"

type matrix struct {
    stride int
    ele    []float64
}

func (m *matrix) print(heading string) {
    if heading > "" {
        fmt.Print("\n", heading, "\n")
    }
    for e := 0; e < len(m.ele); e += m.stride {
        fmt.Printf("%8.3f ", m.ele[e:e+m.stride])
        fmt.Println()
    }
}

func (m1 *matrix) multiply(m2 *matrix) (m3 *matrix, ok bool) {
    if m1.stride*m2.stride != len(m2.ele) {
        return nil, false
    }
    m3 = &matrix{m2.stride, make([]float64, (len(m1.ele)/m1.stride)*m2.stride)}
    for m1c0, m3x := 0, 0; m1c0 < len(m1.ele); m1c0 += m1.stride {
        for m2r0 := 0; m2r0 < m2.stride; m2r0++ {
            for m1x, m2x := m1c0, m2r0; m2x < len(m2.ele); m2x += m2.stride {
                m3.ele[m3x] += m1.ele[m1x] * m2.ele[m2x]
                m1x++
            }
            m3x++
        }
    }
    return m3, true
}

func main() {
    a := matrix{4, []float64{
        1, 2, 3, 4,
        5, 6, 7, 8,
    }}
    b := matrix{3, []float64{
        1, 2, 3,
        4, 5, 6,
        7, 8, 9,
        10, 11, 12,
    }}
    p, ok := a.multiply(&b)
    a.print("Matrix A:")
    b.print("Matrix B:")
    if !ok {
        fmt.Println("not conformable for matrix multiplication")
        return
    }
    p.print("Product of A and B:")
}
```

Output is similar to 2D version.


## Groovy


###  Without Indexed Loops

Uses transposition to avoid indirect element access via ranges of indexes. "assertConformable()" asserts that a & b are both rectangular lists of lists, and that row-length (number of columns) of a is equal to the column-length (number of rows) of b.

```groovy
def assertConformable = { a, b ->
    assert a instanceof List
    assert b instanceof List
    assert a.every { it instanceof List && it.size() == b.size() }
    assert b.every { it instanceof List && it.size() == b[0].size() }
}

def matmulWOIL = { a, b ->
    assertConformable(a, b)

    def bt = b.transpose()
    a.collect { ai ->
        bt.collect { btj ->
            [ai, btj].transpose().collect { it[0] * it[1] }.sum()
        }
    }
}
```



###  Without Transposition

Uses ranges of indexes, the way that matrix multiplication is typically defined. Not as elegant, but it avoids expensive transpositions. Reuses "assertConformable()" from above.

```groovy
def matmulWOT = { a, b ->
    assertConformable(a, b)

    (0..<a.size()).collect { i ->
        (0..<b[0].size()).collect { j ->
            (0..<b.size()).collect { k -> a[i][k] * b[k][j] }.sum()
        }
    }
}
```


Test:

```groovy
def m4by2 = [ [  1,  2 ],
              [  3,  4 ],
              [  5,  6 ],
              [  7,  8 ] ]

def m2by3 = [ [  1,  2,  3 ],
              [  4,  5,  6 ] ]

matmulWOIL(m4by2, m2by3).each { println it }
println()
matmulWOT(m4by2, m2by3).each { println it }
```


{{out}}

```txt
[9, 12, 15]
[19, 26, 33]
[29, 40, 51]
[39, 54, 69]

[9, 12, 15]
[19, 26, 33]
[29, 40, 51]
[39, 54, 69]
```



## Haskell


### With List and transpose

A somewhat inefficient version with lists (''transpose'' is expensive):


```Haskell
import Data.List

 mmult :: Num a => [[a]] -> [[a]] -> [[a]]
 mmult a b = [ [ sum $ zipWith (*) ar bc | bc <- (transpose b) ] | ar <- a ]

 -- Example use:
 test = [[1, 2],
         [3, 4]] `mmult` [[-3, -8, 3],
                          [-2,  1, 4]]
```


### With Array

A more efficient version, based on arrays:


```Haskell
import Data.Array

 mmult :: (Ix i, Num a) => Array (i,i) a -> Array (i,i) a -> Array (i,i) a
 mmult x y
   | x1 /= y0 || x1' /= y0'  = error "range mismatch"
   | otherwise               = array ((x0,y1),(x0',y1')) l
   where
     ((x0,x1),(x0',x1')) = bounds x
     ((y0,y1),(y0',y1')) = bounds y
     ir = range (x0,x0')
     jr = range (y1,y1')
     kr = range (x1,x1')
     l  = [((i,j), sum [x!(i,k) * y!(k,j) | k <- kr]) | i <- ir, j <- jr]
```


### With List and without transpose


```Haskell

multiply:: Num a => [[a]] -> [[a]] -> [[a]]
multiply [] _  = error "left matrix is empty"
multiply _ []  = error "right matrix is empty"
multiply us vs = map (mult [] vs) us
    where
    mult xs [] _ = xs
    mult xs _ [] = xs
    mult [] (zs:zss) (y:ys) = mult (map (\v -> v*y) zs) zss ys
    mult xs (zs:zss) (y:ys) = mult (zipWith (\u v -> u+v*y) xs zs) zss ys

main = mapM_ print $ multiply [[1, 2],[3, 4]] [[-3, -8, 3],[-2,  1, 4]]

```

{{out}}

```txt

[-7,-6,11]
[-17,-20,25]

```


```Haskell

foldlZipWith::(a -> b -> c) -> (d -> c -> d) -> d -> [a] -> [b]  -> d
foldlZipWith _ _ u [] _          = u
foldlZipWith _ _ u _ []          = u
foldlZipWith f g u (x:xs) (y:ys) = foldlZipWith f g (g u (f x y)) xs ys

foldl1ZipWith::(a -> b -> c) -> (c -> c -> c) -> [a] -> [b] -> c
foldl1ZipWith _ _ [] _          = error "First list is empty"
foldl1ZipWith _ _ _ []          = error "Second list is empty"
foldl1ZipWith f g (x:xs) (y:ys) = foldlZipWith f g (f x y) xs ys

multAdd::(a -> b -> c) -> (c -> c -> c) -> [[a]] -> [[b]] -> [[c]]
multAdd f g xs ys = map (\us -> foldl1ZipWith (\u vs -> map (f u) vs) (zipWith g) us ys) xs

mult:: Num a => [[a]] -> [[a]] -> [[a]]
mult xs ys = multAdd (*) (+) xs ys

test a b = do
  let c = mult a b
  putStrLn "a ="
  mapM_ print a
  putStrLn "b ="
  mapM_ print b
  putStrLn "c = a * b = mult a b ="
  mapM_ print c

main = test [[1, 2],[3, 4]] [[-3, -8, 3],[-2,  1, 4]]

```

{{out}}

```txt

a =
[1,2]
[3,4]
b =
[-3,-8,3]
[-2,1,4]
c = a * b = mult a b =
[-7,-6,11]
[-17,-20,25]

```



## HicEst


```hicest
REAL :: m=4, n=2, p=3, a(m,n), b(n,p), res(m,p)

a = $ ! initialize to 1, 2, ..., m*n
b = $ ! initialize to 1, 2, ..., n*p

res = 0
DO i = 1, m
  DO j = 1, p
    DO k = 1, n
      res(i,j) = res(i,j) + a(i,k) * b(k,j)
    ENDDO
  ENDDO
ENDDO

DLG(DefWidth=4, Text=a, Text=b,Y=0, Text=res,Y=0)
```


```hicest
a         b              res
1    2    1    2    3    9    12   15
3    4    4    5    6    19   26   33
5    6                   29   40   51
7    8                   39   54   69
```


=={{header|Icon}} and {{header|Unicon}}==

Using the provided matrix library:


```icon

link matrix

procedure main ()
  m1 := [[1,2,3], [4,5,6]]
  m2 := [[1,2],[3,4],[5,6]]
  m3 := mult_matrix (m1, m2)
  write ("Multiply:")
  write_matrix ("", m1) # first argument is filename, or "" for stdout
  write ("by:")
  write_matrix ("", m2)
  write ("Result: ")
  write_matrix ("", m3)
end

```


And a hand-crafted multiply procedure:


```icon

procedure multiply_matrix (m1, m2)
  result := [] # to hold the final matrix
  every row1 := !m1 do { # loop through each row in the first matrix
    row := []
    every colIndex := 1 to *m1 do { # and each column index of the result
      value := 0
      every rowIndex := 1 to *m2 do {
        value +:= row1[rowIndex] * m2[rowIndex][colIndex]
      }
      put (row, value)
    }
    put (result, row) # add each row as it is complete
  }
  return result
end

```


{{out}}

```txt

Multiply:
1 2 3
4 5 6
by:
1 2
3 4
5 6
Result:
22 28
49 64

```



## IDL



```idl>result = arr1 # arr2</lang



## Idris


```idris
import Data.Vect

Matrix : Nat -> Nat -> Type -> Type
Matrix m n t = Vect m (Vect n t)

multiply : Num t => Matrix m1 n t -> Matrix n m2 t -> Matrix m1 m2 t
multiply a b = multiply' a (transpose b)
  where
        dot : Num t => Vect n t -> Vect n t -> t
        dot v1 v2 = sum $ map (\(s1, s2) => (s1 * s2)) (zip v1 v2)

        multiply' : Num t => Matrix m1 n t -> Matrix m2 n t -> Matrix m1 m2 t
        multiply' (a::as) b = map (dot a) b :: multiply' as b
        multiply' [] _ = []
```



## J

Matrix multiply in J is <code>+/ .*</code>.  For example:

```j
   mp  =:  +/ .*      NB.  Matrix product

   A  =:  ^/~>:i. 4   NB.  Same  A  as in other examples (1 1 1 1, 2 4 8 16, 3 9 27 81,:4 16 64 256)
   B  =:  %.A         NB.  Matrix inverse of A

   '6.2' 8!:2 A mp B
1.00  0.00  0.00  0.00
0.00  1.00  0.00  0.00
0.00  0.00  1.00  0.00
0.00  0.00  0.00  1.00
```

The notation is for a generalized inner product so that

```j
x ~:/ .*. y   NB. boolean inner product ( ~: is "not equal" (exclusive or) and *. is "and")
x *./ .=  y   NB. which rows of x are the same as vector y?
x + / .=  y   NB. number of places where a value in row x equals the corresponding value in y
```

[[Floyd-Warshall_algorithm#J|etc.]]

The general inner product extends to multidimensional arrays, requiring only that <tt> x </tt>and<tt> y </tt> be conformable (trailing dimension of array <tt>x</tt> equals the leading dimension of array <tt>y</tt>).  For example, the matrix multiplication of two dimensional arrays requires <tt>x</tt> to have the same numbers of rows as <tt>y</tt> has columns, as you would expect.

Note also that <code>mp=: +/@:*"1 _</code> functions identically.

Perhaps it would have made more sense to define something more like <code>dot=: conjunction def 'u/@:v"1 _'</code> so that matrix multiplication would be <code>+dot*</code> -- this would also correspond to the original [[Matrix_multiplication#APL|APL]] implementation.


## Java


```java
public static double[][] mult(double a[][], double b[][]){//a[m][n], b[n][p]
   if(a.length == 0) return new double[0][0];
   if(a[0].length != b.length) return null; //invalid dims

   int n = a[0].length;
   int m = a.length;
   int p = b[0].length;

   double ans[][] = new double[m][p];

   for(int i = 0;i < m;i++){
      for(int j = 0;j < p;j++){
         for(int k = 0;k < n;k++){
            ans[i][j] += a[i][k] * b[k][j];
         }
      }
   }
   return ans;
}
```



## JavaScript


### ES5


### =Iterative=

{{works with|SpiderMonkey}} for the <code>print()</code> function

Extends [[Matrix Transpose#JavaScript]]

```javascript
// returns a new matrix
Matrix.prototype.mult = function(other) {
    if (this.width != other.height) {
        throw "error: incompatible sizes";
    }

    var result = [];
    for (var i = 0; i < this.height; i++) {
        result[i] = [];
        for (var j = 0; j < other.width; j++) {
            var sum = 0;
            for (var k = 0; k < this.width; k++) {
                sum += this.mtx[i][k] * other.mtx[k][j];
            }
            result[i][j] = sum;
        }
    }
    return new Matrix(result);
}

var a = new Matrix([[1,2],[3,4]])
var b = new Matrix([[-3,-8,3],[-2,1,4]]);
print(a.mult(b));
```

{{out}}

```txt
-7,-6,11
-17,-20,25
```



### =Functional=


```JavaScript
(function () {
    'use strict';

    // matrixMultiply:: [[n]] -> [[n]] -> [[n]]
    function matrixMultiply(a, b) {
        var bCols = transpose(b);

        return a.map(function (aRow) {
            return bCols.map(function (bCol) {
                return dotProduct(aRow, bCol);
            });
        });
    }

    // [[n]] -> [[n]] -> [[n]]
    function dotProduct(xs, ys) {
        return sum(zipWith(product, xs, ys));
    }

    return matrixMultiply(
        [[-1,  1,  4],
         [ 6, -4,  2],
         [-3,  5,  0],
         [ 3,  7, -2]],

        [[-1,  1,  4,  8],
         [ 6,  9, 10,  2],
         [11, -4,  5, -3]]
    );

    // --> [[51, -8, 26, -18], [-8, -38, -6, 34],
    //        [33, 42, 38, -14], [17, 74, 72, 44]]


    // GENERIC LIBRARY FUNCTIONS

    // (a -> b -> c) -> [a] -> [b] -> [c]
    function zipWith(f, xs, ys) {
        return xs.length === ys.length ? (
            xs.map(function (x, i) {
                return f(x, ys[i]);
            })
        ) : undefined;
    }

    // [[a]] -> [[a]]
    function transpose(lst) {
        return lst[0].map(function (_, iCol) {
            return lst.map(function (row) {
                return row[iCol];
            });
        });
    }

    // sum :: (Num a) => [a] -> a
    function sum(xs) {
        return xs.reduce(function (a, x) {
            return a + x;
        }, 0);
    }

    // product :: n -> n -> n
    function product(a, b) {
        return a * b;
    }

})();
```

{{Out}}

```Javascript
[[51, -8, 26, -18], [-8, -38, -6, 34],
      [33, 42, 38, -14], [17, 74, 72, 44]]
```



### ES6


```JavaScript
((() => {
    'use strict';

    // matrixMultiply :: Num a => [[a]] -> [[a]] -> [[a]]
    const matrixMultiply = (a, b) => {
        const bCols = transpose(b);
        return a.map(aRow => bCols.map(bCol => dotProduct(aRow, bCol)));
    }

    // dotProduct :: Num a => [[a]] -> [[a]] -> [[a]]
    const dotProduct = (xs, ys) => sum(zipWith(product, xs, ys));


    // GENERIC

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) =>
        xs.length === ys.length ? (
            xs.map((x, i) => f(x, ys[i]))
        ) : undefined;

    // transpose :: [[a]] -> [[a]]
    const transpose = xs =>
        xs[0].map((_, iCol) => xs.map(row => row[iCol]));

    // sum :: (Num a) => [a] -> a
    const sum = xs =>
        xs.reduce((a, x) => a + x, 0);

    // product :: Num a => a -> a -> a
    const product = (a, b) => a * b;


    // TEST
    return matrixMultiply(
        [
            [-1, 1, 4],
            [6, -4, 2],
            [-3, 5, 0],
            [3, 7, -2]
        ],

        [
            [-1, 1, 4, 8],
            [6, 9, 10, 2],
            [11, -4, 5, -3]
        ]
    );

    // --> [[51, -8, 26, -18], [-8, -38, -6, 34],
    //        [33, 42, 38, -14], [17, 74, 72, 44]]
}))();
```

{{Out}}

```JavaScript
[[51, -8, 26, -18], [-8, -38, -6, 34],
[33, 42, 38, -14], [17, 74, 72, 44]]
```



## jq

In the following, an m by n matrix is represented by an array of m arrays, each of which is of length n.

The function multiply(A;B) assumes its arguments are numeric matrices of the proper dimensions. Note that preallocating the resultant matrix would actually slow things down.

```jq
def dot_product(a; b):
  a as $a | b as $b
  | reduce range(0;$a|length) as $i (0; . + ($a[$i] * $b[$i]) );

# transpose/0 expects its input to be a rectangular matrix (an array of equal-length arrays)
def transpose:
  if (.[0] | length) == 0 then []
  else [map(.[0])] + (map(.[1:]) | transpose)
  end ;

# A and B should both be numeric matrices, A being m by n, and B being n by p.
def multiply(A; B):
  A as $A | B as $B
  | ($B[0]|length) as $p
  | ($B|transpose) as $BT
  | reduce range(0; $A|length) as $i
       ([]; reduce range(0; $p) as $j
         (.; .[$i][$j] = dot_product( $A[$i]; $BT[$j] ) )) ;
```

'''Example'''
 ((2|sqrt)/2) as $r | [ [$r, $r],  [(-($r)), $r]] as $R
 | multiply($R;$R)
{{Out}}
 [[0,1.0000000000000002],[-1.0000000000000002,0]]


## Jsish

Based on Javascript matrix entries.

Uses module listed in [[Matrix Transpose#Jsish]]


```javascript
/* Matrix multiplication, in Jsish */
require('Matrix');

if (Interp.conf('unitTest')) {
    var a = new Matrix([[1,2],[3,4]]);
    var b = new Matrix([[-3,-8,3],[-2,1,4]]);
;    a;
;    b;
;    a.mult(b);
}

/*
=!EXPECTSTART!=
a ==> { height:2, mtx:[ [ 1, 2 ], [ 3, 4 ] ], width:2 }
b ==> { height:2, mtx:[ [ -3, -8, 3 ], [ -2, 1, 4 ] ], width:3 }
a.mult(b) ==> { height:2, mtx:[ [ -7, -6, 11 ], [ -17, -20, 25 ] ], width:3 }
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u matrixMultiplication.jsi
[PASS] matrixMultiplication.jsi
```



## Julia

The multiplication is denoted by *

```Julia>julia
 [1 2 3 ; 4 5 6] * [1 2 ; 3 4 ; 5 6]  # product of a 2x3 by a 3x2
2x2 Array{Int64,2}:
 22  28
 49  64

julia> [1 2 3] * [1,2,3]   # product of a row vector by a column vector
1-element Array{Int64,1}:
 14

```



## K


```k
  (1 2;3 4)_mul (5 6;7 8)
(19 22
 43 50)
```



## Klong


```k
    mul::{[a b];b::+y;{a::x;+/'{a*x}'b}'x}
    [[1 2] [3 4]] mul [[5 6] [7 8]]
[[19 22]
 [43 50]]
```



## Kotlin


```scala
// version 1.1.3

typealias Vector = DoubleArray
typealias Matrix = Array<Vector>

operator fun Matrix.times(other: Matrix): Matrix {
    val rows1 = this.size
    val cols1 = this[0].size
    val rows2 = other.size
    val cols2 = other[0].size
    require(cols1 == rows2)
    val result = Matrix(rows1) { Vector(cols2) }
    for (i in 0 until rows1) {
        for (j in 0 until cols2) {
            for (k in 0 until rows2) {
                result[i][j] += this[i][k] * other[k][j]
            }
        }
    }
    return result
}

fun printMatrix(m: Matrix) {
    for (i in 0 until m.size) println(m[i].contentToString())
}

fun main(args: Array<String>) {
    val m1 = arrayOf(
        doubleArrayOf(-1.0,  1.0,  4.0),
        doubleArrayOf( 6.0, -4.0,  2.0),
        doubleArrayOf(-3.0,  5.0,  0.0),
        doubleArrayOf( 3.0,  7.0, -2.0)
    )
    val m2 = arrayOf(
        doubleArrayOf(-1.0,  1.0,  4.0,  8.0),
        doubleArrayOf( 6.0,  9.0, 10.0,  2.0),
        doubleArrayOf(11.0, -4.0,  5.0, -3.0)
    )
    printMatrix(m1 * m2)
}
```


{{out}}

```txt

[51.0, -8.0, 26.0, -18.0]
[-8.0, -38.0, -6.0, 34.0]
[33.0, 42.0, 38.0, -14.0]
[17.0, 74.0, 72.0, 44.0]

```



## Lang5


```Lang5
[[1 2 3] [4 5 6]] 'm dress
[[1 2] [3 4] [5 6]] 'm dress * .
```

{{out}}

```txt
[
  [   22    28  ]
  [   49    64  ]
]
```



## LFE


Use the LFE  <code>transpose/1</code> function from [[Matrix transposition]].


```lisp

(defun matrix* (matrix-1 matrix-2)
  (list-comp
    ((<- a matrix-1))
    (list-comp
      ((<- b (transpose matrix-2)))
      (lists:foldl #'+/2 0
                   (lists:zipwith #'*/2 a b)))))

```


Usage example in the LFE REPL:


```lisp

> (set ma '((1 2)
            (3 4)
            (5 6)
            (7 8)))
((1 2) (3 4) (5 6) (7 8))
> (set mb (transpose ma))
((1 3 5 7) (2 4 6 8))
> (matrix* ma mb)
((5 11 17 23) (11 25 39 53) (17 39 61 83) (23 53 83 113))

```



## Liberty BASIC

There is no native matrix capability. A set of functions is available at http://www.diga.me.uk/RCMatrixFuncs.bas implementing matrices of arbitrary dimension in a string format.

```lb

MatrixA$ ="4, 4,         1,  1,  1,  1,         2,  4,  8,  16,             3,  9, 27,  81,          4, 16, 64, 256"
MatrixB$ ="4, 4,         4, -3,  4/3, -1/4 ,   -13/3, 19/4, -7/3, 11/24,    3/2, -2, 7/6, -1/4,   -1/6, 1/4, -1/6, 1/24"

print "Product of two matrices"
call DisplayMatrix MatrixA$
print "         *"
call DisplayMatrix MatrixB$
print "         ="
MatrixP$ =MatrixMultiply$( MatrixA$, MatrixB$)
call DisplayMatrix MatrixP$

```


{{out}}

```txt
Product of two matrices
| 1.00000 1.00000 1.00000 1.00000 |
| 2.00000 4.00000 8.00000 16.00000 |
| 3.00000 9.00000 27.00000 81.00000 |
| 4.00000 16.00000 64.00000 256.00000 |

*
| 4.00000 -3.00000 1.33333 -0.25000 |
| -4.33333 4.75000 -2.33333 0.45833 |
| 1.50000 -2.00000 1.16667 -0.25000 |
| -0.16667 0.25000 -0.16667 0.04167 |

=
| 1.00000 0.00000 0.00000 0.00000 |
| 0.00000 1.00000 0.00000 0.00000 |
| 0.00000 0.00000 1.00000 0.00000 |
| 0.00000 0.00000 0.00000 1.00000 |
```



## Logo


```logo
TO LISTVMD :A :F :C :NV
;PROCEDURE LISTVMD
;A = LIST
;F = ROWS
;C = COLS
;NV = NAME OF MATRIX / VECTOR NEW
;this procedure transform a list in matrix / vector square or rect

(LOCAL "CF "CC "NV "T "W)
MAKE "CF 1
MAKE "CC 1
MAKE "NV (MDARRAY (LIST :F :C) 1)
MAKE "T :F * :C
FOR [Z 1 :T][MAKE "W ITEM :Z :A
MDSETITEM (LIST :CF :CC) :NV :W
MAKE "CC :CC + 1
IF :CC = :C + 1 [MAKE "CF :CF + 1 MAKE "CC 1]]
OUTPUT :NV
END
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


TO XX
; MAIN PROGRAM
;LRCVS 10.04.12
; THIS PROGRAM multiplies two "square" matrices / vector ONLY!!!
; THE RECTANGULAR NOT WORK!!!

CT CS HT

; FIRST DATA MATRIX / VECTOR
MAKE "A [1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49]
MAKE "FA 5 ;"ROWS
MAKE "CA 5 ;"COLS

; SECOND DATA MATRIX / VECTOR
MAKE "B [2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50]
MAKE "FB 5 ;"ROWS
MAKE "CB 5 ;"COLS


IF (OR :FA <> :CA :FB <>:CB) [PRINT "Las_matrices/vector_no_son_cuadradas THROW
"TOPLEVEL ]
IFELSE (OR :CA <> :FB :FA <> :CB) [PRINT
"Las_matrices/vector_no_son_compatibles THROW "TOPLEVEL ][MAKE "MA LISTVMD :A
:FA :CA "MA MAKE "MB LISTVMD :B :FB :CB "MB] ;APPLICATION <<< "LISTVMD"

PRINT (LIST "THIS_IS: "ROWS "X "COLS)
PRINT []
PRINT (LIST :MA "=_M1 :FA "ROWS "X :CA "COLS)
PRINT []
PRINT (LIST :MB "=_M2 :FA "ROWS "X :CA "COLS)
PRINT []


MAKE "T :FA * :CB
MAKE "RE (ARRAY :T 1)


MAKE "CO 0
FOR [AF 1 :CA][
FOR [AC 1 :CA][
MAKE "TEMP 0
FOR [I 1 :CA ][
MAKE "TEMP :TEMP + (MDITEM (LIST :I :AF) :MA) * (MDITEM (LIST :AC :I) :MB)]
MAKE "CO :CO + 1
SETITEM :CO :RE :TEMP]]


PRINT []
PRINT (LIST "THIS_IS: :FA "ROWS "X :CB "COLS)
SHOW LISTVMD :RE :FA :CB "TO ;APPLICATION <<< "LISTVMD"
END


::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\


               M1 * M2 RESULT / SOLUTION

 1  3  5  7  9    2  4  6  8 10    830 1880 2930 3980 5030
11 13 15 17 19   12 14 16 18 20    890 2040 3190 4340 5490
21 23 25 27 29 X 22 24 26 28 30 =  950 2200 3450 4700 5950
31 33 35 37 39   32 34 36 38 40   1010 2360 3710 5060 6410
41 43 45 47 49   42 44 46 48 50   1070 2520 3970 5420 6870

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\


NOW IN LOGO!!!!


THIS_IS: ROWS X COLS

{{1 3 5 7 9} {11 13 15 17 19} {21 23 25 27 29} {31 33 35 37 39} {41 43 45 47
49}} =_M1 5 ROWS X 5 COLS

{{2 4 6 8 10} {12 14 16 18 20} {22 24 26 28 30} {32 34 36 38 40} {42 44 46 48
50}} =_M2 5 ROWS X 5 COLS


THIS_IS: 5 ROWS X 5 COLS
{{830 1880 2930 3980 5030} {890 2040 3190 4340 5490} {950 2200 3450 4700 5950}
{1010 2360 3710 5060 6410} {1070 2520 3970 5420 6870}}
```



## Lua


```lua
function MatMul( m1, m2 )
    if #m1[1] ~= #m2 then       -- inner matrix-dimensions must agree
        return nil
    end

    local res = {}

    for i = 1, #m1 do
        res[i] = {}
        for j = 1, #m2[1] do
            res[i][j] = 0
            for k = 1, #m2 do
                res[i][j] = res[i][j] + m1[i][k] * m2[k][j]
            end
        end
    end

    return res
end

-- Test for MatMul
mat1 = { { 1, 2, 3 }, { 4, 5, 6 } }
mat2 = { { 1, 2 }, { 3, 4 }, { 5, 6 } }
erg = MatMul( mat1, mat2 )
for i = 1, #erg do
    for j = 1, #erg[1] do
        io.write( erg[i][j] )
        io.write("  ")
    end
    io.write("\n")
end
```



### SciLua

Using the sci.alg library from scilua.org

```Lua
local alg = require("sci.alg")
mat1 = alg.tomat{{1, 2, 3}, {4, 5, 6}}
mat2 = alg.tomat{{1, 2}, {3, 4}, {5, 6}}
mat3 = mat1[] ** mat2[]
print(mat3)
```

{{out}}

```txt
+22.00000,+28.00000
+49.00000,+64.00000
```


## M2000 Interpreter


```M2000 Interpreter

Module CheckMatMult {
	\\ Matrix Multiplication
	\\ we use array pointers so we pass arrays byvalue but change this by reference
	\\ this can be done because always arrays passed by reference,
	\\ and Read statement decide if this goes to a pointer of array or copied to a local array
	\\ the first line of code for MatMul is: Read a as array, b as array
	\\ interpreter insert this at function construction.
	\\ if a pointer inside function change to point to a new array, the this has no reflect to the passed array.
	Function MatMul(a as array, b as array) {
		if dimension(a)<>2 or dimension(b)<>2 then Error "Need two 2D arrays "
		let a2=dimension(a,2), b1=dimension(b,1)
		if a2<>b1 then Error "Need columns of first array equal to rows of second array"
		let a1=dimension(a,1), b2=dimension(b,2)
		let aBase=dimension(a,1,0)-1, bBase=dimension(b,1,0)-1
		let aBase1=dimension(a,2,0)-1, bBase1=dimension(b,2,0)-1
		link a,b to a(), b()  ' change interface for arrays
		dim base 1, c(a1, b2)
		for i=1 to a1 : let ia=i+abase : for j=1 to b2 : let jb=j+bBase1 : for k=1 to a2
		c(i,j)+=a(ia,k+aBase1)*b(k+bBase,jb)
		next k : next j : next i
		\\ redim to base 0
		dim base 0, c(a1, b2)
		=c()
	}
	\\ define arrays with different base per dimension
	\\ res() defined as empty array
	dim a(10 to 13, 4), b(4, 2 to 5), res()
	\\ numbers from ADA task
	a(10,0)= 1, 1, 1, 1, 2, 4, 8, 16, 3, 9, 27, 81, 4, 16, 64, 256
	b(0,2)= 4, -3, 4/3, -1/4, -13/3, 19/4, -7/3, 11/24, 3/2, -2, 7/6, -1/4, -1/6, 1/4, -1/6, 1/24
	res()=MatMul(a(), b())
	for i=0 to 3 :for j=0 to 3
	Print res(i,j),
	next j : Print : next i
}
CheckMatMult
Module CheckMatMult2 {
	\\ Matrix Multiplication
	\\ pass arrays by reference
	\\ if we change a passed array here, to a new array then this change also the reference array.
	Function MatMul(&a(),&b()) {
		if dimension(a())<>2 or dimension(b())<>2 then Error "Need two 2D arrays "
		let a2=dimension(a(),2), b1=dimension(b(),1)
		if a2<>b1 then Error "Need columns of first array equal to rows of second array"
		let a1=dimension(a(),1), b2=dimension(b(),2)
		let aBase=dimension(a(),1,0)-1, bBase=dimension(b(),1,0)-1
		let aBase1=dimension(a(),2,0)-1, bBase1=dimension(b(),2,0)-1
		dim base 1, c(a1, b2)
		for i=1 to a1 : let ia=i+abase : for j=1 to b2 : let jb=j+bBase1 : for k=1 to a2
		c(i,j)+=a(ia,k+aBase1)*b(k+bBase,jb)
		next k : next j : next i
		\\ redim to base 0
		dim base 0, c(a1, b2)
		=c()
	}
	\\ define arrays with different base per dimension
	\\ res() defined as empty array
	dim a(10 to 13, 4), b(4, 2 to 5), res()
	\\ numbers from ADA task
	a(10,0)= 1, 1, 1, 1, 2, 4, 8, 16, 3, 9, 27, 81, 4, 16, 64, 256
	b(0,2)= 4, -3, 4/3, -1/4, -13/3, 19/4, -7/3, 11/24, 3/2, -2, 7/6, -1/4, -1/6, 1/4, -1/6, 1/24
	res()=MatMul(&a(), &b())
	for i=0 to 3 :for j=0 to 3
	Print res(i,j),
	next j : Print : next i
}
CheckMatMult2

```


{{out}}

```txt

 1 0 0 0
 0 1 0 0
 0 0 1 0
 0 0 0 1

```


## Maple


```Maple
A := <<1|2|3>,<4|5|6>>;

B := <<1,2,3>|<4,5,6>|<7,8,9>|<10,11,12>>;

A . B;
```

{{out}}

```txt
                                    [1  2  3]
                               A := [       ]
                                    [4  5  6]

                                  [1  4  7  10]
                                  [           ]
                             B := [2  5  8  11]
                                  [           ]
                                  [3  6  9  12]

                             [14  32   50   68]
                             [                ]
                             [32  77  122  167]
```




## MathCortex


```mathcortex

>> A = [2,3; -2,1]
 2           3
-2           1

>> B = [1,2;4,2]
 1           2
 4           2

>> A * B
 14          10
 2          -2

```



## Mathematica


The Wolfram Language supports both dot products and element-wise multiplication of matrices.

This computes a dot product:


```mathematica
Dot[{{a, b}, {c, d}}, {{w, x}, {y, z}}]
```


With the following output:


```mathematica
{{a w + b y, a x + b z}, {c w + d y, c x + d z}}
```


This also computes a dot product, using the infix . notation:


```mathematica
{{a, b}, {c, d}} . {{w, x}, {y, z}}
```


This does element-wise multiplication of matrices:


```mathematica
Times[{{a, b}, {c, d}}, {{w, x}, {y, z}}]
```


With the following output:


```mathematica
{{a w, b x}, {c y, d z}}
```


Alternative infix notations '*' and ' ' (space, indicating multiplication):


```mathematica
{{a, b}, {c, d}}*{{w, x}, {y, z}}
```


```mathematica
{{a, b}, {c, d}} {{w, x}, {y, z}}
```


In all cases matrices can be fully symbolic or numeric or mixed symbolic and numeric.
Numeric matrices support arbitrary numerical magnitudes, arbitrary precision as well
as complex numbers:


```mathematica
Dot[{{85, 60, 65}, {54, 99, 33}, {46, 52, 87}}, {{89, 77, 98}, {55, 27, 25}, {80, 68, 85}}]
```


With the following output:


```mathematica
{{16065, 12585, 15355}, {12891, 9075, 10572}, {13914, 10862, 13203}}
```



## MATLAB

Matlab contains two methods of multiplying matrices: by using the "mtimes(matrix,matrix)" function, or the "*" operator.


```MATLAB>>
 A = [1 2;3 4]

A =

     1     2
     3     4

>> B = [5 6;7 8]

B =

     5     6
     7     8

>> A * B

ans =

    19    22
    43    50

>> mtimes(A,B)

ans =

    19    22
    43    50
```



## Maxima


```maxima
a: matrix([1, 2],
          [3, 4],
          [5, 6],
          [7, 8])$

b: matrix([1, 2, 3],
          [4, 5, 6])$

a . b;
/* matrix([ 9, 12, 15],
          [19, 26, 33],
          [29, 40, 51],
          [39, 54, 69]) */
```



## Nial


```nial
|A :=  4 4 reshape 1 1 1 1 2 4 8 16 3 9 27 81 4 16 64 256
=1  1  1   1
=2  4  8  16
=3  9 27  81
=4 16 64 256
|B := inverse A

|A innerproduct B
=1.        0.     8.3e-17     -2.9e-16
=1.3e-15   1.     -4.4e-16    -3.3e-16
=0.        0.      1.         4.4e-16
=0.        0.      0.         1.
```



## Nim


```nim
import strfmt

type Matrix[M,N: static[int]] = array[M, array[N, float]]

let a = [[1.0,  1.0,  1.0,   1.0],
         [2.0,  4.0,  8.0,  16.0],
         [3.0,  9.0, 27.0,  81.0],
         [4.0, 16.0, 64.0, 256.0]]

let b = [[  4.0  , -3.0  ,  4/3.0,  -1/4.0 ],
         [-13/3.0, 19/4.0, -7/3.0,  11/24.0],
         [  3/2.0, -2.0  ,  7/6.0,  -1/4.0 ],
         [ -1/6.0,  1/4.0, -1/6.0,   1/24.0]]

proc `$`(m: Matrix): string =
  result = "(["
  for r in m:
    if result.len > 2: result.add "]\n ["
    for val in r: result.add val.format("8.2f")
  result.add "])"

proc `*`[M,P,N](a: Matrix[M,P]; b: Matrix[P,N]): Matrix[M,N] =
  for i in result.low .. result.high:
    for j in result[0].low .. result[0].high:
      for k in a[0].low .. a[0].high:
        result[i][j] += a[i][k] * b[k][j]

echo a
echo b
echo a * b
echo b * a
```



## OCaml


This version works on arrays of arrays of ints:

```ocaml
let matrix_multiply x y =
  let x0 = Array.length x
  and y0 = Array.length y in
  let y1 = if y0 = 0 then 0 else Array.length y.(0) in
  let z = Array.make_matrix x0 y1 0 in
  for i = 0 to x0-1 do
    for j = 0 to y1-1 do
      for k = 0 to y0-1 do
        z.(i).(j) <- z.(i).(j) + x.(i).(k) * y.(k).(j)
      done
    done
  done;
  z
```


 # matrix_multiply [|[|1;2|];[|3;4|]|] [|[|-3;-8;3|];[|-2;1;4|]|];;
 - : int array array = [|[|-7; -6; 11|]; [|-17; -20; 25|]|]

{{trans|Scheme}}
This version works on lists of lists of ints:

```ocaml
(* equivalent to (apply map ...) *)
let rec mapn f lists =
  assert (lists <> []);
  if List.mem [] lists then
    []
  else
    f (List.map List.hd lists) :: mapn f (List.map List.tl lists)

let matrix_multiply m1 m2 =
  List.map
    (fun row ->
      mapn
       (fun column ->
         List.fold_left (+) 0
          (List.map2 ( * ) row column))
       m2)
    m1
```


 # matrix_multiply [[1;2];[3;4]] [[-3;-8;3];[-2;1;4]];;
 - : int list list = [[-7; -6; 11]; [-17; -20; 25]]


## Octave


```octave
a = zeros(4);
% prepare the matrix
% 1 1 1 1
% 2 4 8 16
% 3 9 27 81
% 4 16 64 256
for i = 1:4
  for j = 1:4
    a(i, j) = i^j;
  endfor
endfor
b = inverse(a);
a * b
```




## Ol

{{trans|Scheme}}
This version works on lists of lists:

```ol

(define (matrix-multiply matrix1 matrix2)
(map
   (lambda (row)
      (apply map
         (lambda column
            (apply + (map * row column)))
         matrix2))
   matrix1))

```


 > (matrix-multiply '((1 2) (3 4)) '((-3 -8 3) (-2 1 4)))
 ((-7 -6 11) (-17 -20 25))


## OxygenBasic

When using matrices in Video graphics, speed is important. Here is a matrix multiplier written in OxygenBasics's x86 Assembly code.

```oxygenbasic

  'Example of matrix layout mapped to an array of 4x4 cells
  '
  '  0 4 8 C
  '  1 5 9 D
  '  2 6 A E
  '  3 7 B F
  '

  % MatrixType double

  sub MatrixMul(MatrixType *A,*B,*C, sys n)
  '
### ==================================

  '
  '
  #if leftmatch matrixtype single
    % OneStep 4
    % mtype single
  #endif
  '
  #if leftmatch matrixtype double
    % OneStep 8
    % mtype double
  #endif

  sys pa=@A, pb=@B, pc=@C
  sys ColStep=OneStep*n

  mov ecx,pa
  mov edx,pb
  mov eax,pc

  mov esi,n
  (
  call column : dec esi : jg repeat
  )
  exit sub

  column:
  '======

  mov edi,n
  (
  call cell : dec edi : jg repeat
  )
  add edx,ColStep
  sub ecx,ColStep
  ret

  cell: ' row A * column B
  '
### =================


  'matrix data is stored ascending vertically then horizontally
  'thus rows are minor, columns are major
  '
  push  ecx
  push  edx
  push  eax
  mov   eax,4
  fldz
  (
  fld   mtype [ecx]
  fmul  mtype [edx]
  faddp st1
  add   ecx,ColStep 'next column of matrix A
  add   edx,OneStep 'next row of matrix B
  dec eax
  jnz repeat
  )
  pop eax
  fstp mtype [eax] 'assign to next row of matrix C
  '
  pop edx
  pop ecx
  add   eax,OneStep 'next cell in column of matrix C (columns then rows)
  add   ecx,OneStep 'next row of matrix A
  ret
  '
  end sub


  function ShowMatrix(MatrixType*A,sys n) as string
  '
### ==========================================

    string cr=chr(13)+chr(10), tab=chr(9)
    function="MATRIX " n "x" n cr cr
    sys i,j,m
    '
    for i=1 to n
      m=0
      for j=1 to n
        function+=str( A[m+i] ) tab
        m+=n
      next
      function+=cr
    next
  end function

  'TEST
  '====

  % n 4
  MatrixType A[n*n],B[n*n],C[n*n]


  'reading vertically (minor) then left to right (major)

  A <= 4,0,0,1, 0,4,0,0, 0,0,4,0, 0,0,0,4

  B <= 2,0,0,2, 0,2,0,0, 0,0,2,0, 0,0,0,2


  MatrixMul A,B,C,n

  Print ShowMatrix C,n

```



## PARI/GP


```parigp
M*N
```



## Perl

For most applications involving extensive matrix arithmetic, using the CPAN module called "PDL" (that stands for "Perl Data Language") would probably be the easiest and most efficient approach. That said, here's an implementation of matrix multiplication in plain Perl.

This function takes two references to arrays of arrays and returns the product as a reference to a new anonymous array of arrays.


```perl
sub mmult
 {
  our @a; local *a = shift;
  our @b; local *b = shift;
  my @p = [];
  my $rows = @a;
  my $cols = @{ $b[0] };
  my $n = @b - 1;
  for (my $r = 0 ; $r < $rows ; ++$r)
     {
      for (my $c = 0 ; $c < $cols ; ++$c)
         {
          $p[$r][$c] += $a[$r][$_] * $b[$_][$c]
           foreach 0 .. $n;
         }
     }
  return [@p];
 }

sub display { join("\n" => map join(" " => map(sprintf("%4d", $_), @$_)), @{+shift})."\n" }

@a =
(
   [1, 2],
   [3, 4]
);

@b =
(
   [-3, -8, 3],
   [-2,  1, 4]
);

$c = mmult(\@a,\@b);
display($c)
```

{{out}}

```txt
  -7  -6  11
 -17 -20  25
```



## Perl 6


{{trans|Perl 5}}

{{works with|Rakudo|2015-09-22}}

There are three ways in which this example differs significantly from the original Perl 5 code.  These are not esoteric differences; all three of these features typically find heavy use in Perl 6.

First, we can use a real signature that can bind two arrays as arguments, because the default in Perl 6 is not to flatten arguments unless the signature specifically requests it.
We don't need to pass the arrays with backslashes because the binding choice is made lazily
by the signature itself at run time; in Perl 5 this choice must be made at compile time.
Also, we can bind the arrays to formal parameters that are really lexical variable names; in Perl 5 they can only be bound to global array objects (via a typeglob assignment).

Second, we use the X cross operator in conjunction with a two-parameter closure to avoid writing
nested loops.  The X cross operator, along with Z, the zip operator, is a member of a class of operators that expect lists on both sides, so we call them "list infix" operators.  We tend to define these operators using capital letters so that they stand out visually from the lists on both sides.  The cross operator makes every possible combination of the one value from the first list followed by one value from the second.  The right side varies most rapidly, just like an inner loop.  (The X and Z operators may both also be used as meta-operators, Xop or Zop, distributing some other operator "op" over their generated list.  All metaoperators in Perl 6 may be applied to user-defined operators as well.)

Third is the use of prefix <tt>^</tt> to generate a list of numbers in a range.  Here it is
used on an array to generate all the indexes of the array.  We have a way of indicating a range by the infix <tt>..</tt> operator, and you can put a <tt>^</tt> on either end to exclude that endpoint.  We found ourselves writing <tt>0 ..^ @a</tt> so often that we made <tt>^@a</tt> a shorthand for that.  It's pronounced "upto". The array is evaluated in a numeric context, so it returns the number of elements it contains, which is exactly what you want for the exclusive limit of the range.


```perl6
sub mmult(@a,@b) {
    my @p;
    for ^@a X ^@b[0] -> ($r, $c) {
        @p[$r][$c] += @a[$r][$_] * @b[$_][$c] for ^@b;
    }
    @p;
}

my @a = [1,  1,  1,   1],
        [2,  4,  8,  16],
        [3,  9, 27,  81],
        [4, 16, 64, 256];

my @b = [  4  , -3  ,  4/3,  -1/4 ],
        [-13/3, 19/4, -7/3,  11/24],
        [  3/2, -2  ,  7/6,  -1/4 ],
        [ -1/6,  1/4, -1/6,   1/24];

.say for mmult(@a,@b);
```


{{out}}

```txt
[1 0 0 0]
[0 1 0 0]
[0 0 1 0]
[0 0 0 1]
```


Note that these are not rounded values, but exact, since all the math was done in rationals.
Hence we need not rely on format tricks to hide floating-point inaccuracies.

Just for the fun of it, here's a functional version that uses no temp variables or side effects.
Some people will find this more readable and elegant, and others will, well, not.


```perl6
sub mmult(\a,\b) {
    [
        for ^a -> \r {
            [
                for ^b[0] -> \c {
                    [+] a[r;^b] Z* b[^b;c]
                }
            ]
        }
    ]
}
```


Here we use Z with an "op" of <tt>*</tt>, which is a zip with multiply.  This, along with the <tt>[+]</tt> reduction operator, replaces the inner loop.  We chose to split the outer X loop back into two loops to make it convenient to collect each subarray value in <tt>[...]</tt>.  It just collects all the returned values from the inner loop and makes an array of them.  The outer loop simply returns the outer array.


## Phix

Copy of [[Matrix_multiplication#Euphoria|Euphoria]]

```Phix
function matrix_mul(sequence a, sequence b)
sequence c
    if length(a[1]) != length(b) then
        return 0
    else
        c = repeat(repeat(0,length(b[1])),length(a))
        for i=1 to length(a) do
            for j=1 to length(b[1]) do
                for k=1 to length(a[1]) do
                    c[i][j] += a[i][k]*b[k][j]
                end for
            end for
        end for
        return c
    end if
end function
```



## PicoLisp


```PicoLisp
(de matMul (Mat1 Mat2)
   (mapcar
      '((Row)
         (apply mapcar Mat2
            '(@ (sum * Row (rest))) ) )
      Mat1 ) )

(matMul
   '((1 2 3) (4 5 6))
   '((6 -1) (3 2) (0 -3)) )
```

{{out}}

```txt
-> ((12 -6) (39 -12))
```



## PL/I


```PL/I

/* Matrix multiplication of A by B, yielding C */
MMULT: procedure (a, b, c);
   declare (a, b, c)(*,*) float controlled;
   declare (i, j, m, n, p) fixed binary;

   if hbound(a,2) ^= hbound(b,1) then
      do;
         put skip list
            ('Matrices are incompatible for matrix multiplication');
         signal error;
      end;

   m = hbound(a, 1); p = hbound(b, 2);
   if allocation(c) > 0 then free c;

   allocate c(m,p);

   do  i = 1 to m;
      do j = 1 to p;
         c(i,j) = sum(a(i,*) * b(*,j) );
      end;
   end;
end MMULT;

```



## Pop11



```pop11
define matmul(a, b) -> c;
    lvars ba = boundslist(a), bb = boundslist(b);
    lvars i, i0 = ba(1), i1 = ba(2);
    lvars j, j0 = bb(1), j1 = bb(2);
    lvars k, k0 = bb(3), k1 = bb(4);
    if length(ba) /= 4 then
        throw([need_2d_array ^a])
    endif;
    if length(bb) /= 4 then
        throw([need_2d_array ^b])
    endif;
    if ba(3) /= j0 or ba(4) /= j1 then
        throw([dimensions_do_not_match ^a ^b]);
    endif;
    newarray([^i0 ^i1 ^k0 ^k1], 0) -> c;
    for i from i0 to i1 do
        for k from k0 to k1 do
            for j from j0 to j1 do
                c(i, k) + a(i, j)*b(j, k) -> c(i, k);
            endfor;
        endfor;
    endfor;
enddefine;
```


## PowerShell


```PowerShell

function multarrays($a, $b) {
    $n,$m,$p = ($a.Count - 1), ($b.Count - 1), ($b[0].Count - 1)
    if ($a[0].Count -ne $b.Count) {throw "Multiplication impossible"}
    $c = @(0)*($a[0].Count)
    foreach ($i in 0..$n) {
        $c[$i] = foreach ($j in 0..$p) {
            $sum = 0
            foreach ($k in 0..$m){$sum += $a[$i][$k]*$b[$k][$j]}
            $sum
        }
    }
    $c
 }

function show($a) { $a | foreach{"$_"}}

$a = @(@(1,2),@(3,4))
$b = @(@(5,6),@(7,8))
$c = @(5,6)
"`$a ="
show $a
""
"`$b ="
show $b
""
"`$c ="
$c
""
"`$a * `$b ="
show (multarrays $a $b)
" "
"`$a * `$c ="
show (multarrays $a $c)

```

<b>Output:</b>

```txt

$a =
1 2
3 4

$b =
5 6
7 8

$c =
5
6

$a * $b =
19 22
43 50

$a * $c =
17
39

```



## Prolog

{{trans|Scheme}}
{{works with|SWI Prolog|5.9.9}}

```prolog
% SWI-Prolog has transpose/2 in its clpfd library
:- use_module(library(clpfd)).

% N is the dot product of lists V1 and V2.
dot(V1, V2, N) :- maplist(product,V1,V2,P), sumlist(P,N).
product(N1,N2,N3) :- N3 is N1*N2.

% Matrix multiplication with matrices represented
% as lists of lists. M3 is the product of M1 and M2
mmult(M1, M2, M3) :- transpose(M2,MT), maplist(mm_helper(MT), M1, M3).
mm_helper(M2, I1, M3) :- maplist(dot(I1), M2, M3).
```


## PureBasic

Matrices represented as integer arrays with rows in the first dimension and columns in the second.

```PureBasic
Procedure multiplyMatrix(Array a(2), Array b(2), Array prd(2))
  Protected ar = ArraySize(a())    ;#rows for matrix a
  Protected ac = ArraySize(a(), 2) ;#cols for matrix a
  Protected br = ArraySize(b())    ;#rows for matrix b
  Protected bc = ArraySize(b(), 2) ;#cols for matrix b

  If ac = br
    Dim prd(ar, bc)

    Protected i, j, k
    For i = 0 To ar
      For j = 0 To bc
        For k = 0 To br ;ac
          prd(i, j) = prd(i, j) + (a(i, k) * b(k, j))
        Next
      Next
    Next

    ProcedureReturn #True  ;multiplication performed, product in prd()
  Else
    ProcedureReturn #False ;multiplication not performed, dimensions invalid
  EndIf
EndProcedure
```

Additional code to demonstrate use.

```PureBasic
DataSection
  Data.i 2,3           ;matrix a (#rows, #cols)
  Data.i 1,2,3, 4,5,6  ;elements by row

  Data.i 3,1           ;matrix b (#rows, #cols)
  Data.i 1, 5, 9       ;elements by row
EndDataSection

Procedure displayMatrix(Array a(2), text.s)
  Protected i, j
  Protected columns = ArraySize(a(), 2), rows = ArraySize(a(), 1)

  PrintN(text + ": (" + Str(rows + 1) + ", " + Str(columns + 1) + ")")
  For i = 0 To rows
    For j = 0 To columns
      Print(LSet(Str(a(i, j)), 4, " "))
    Next
    PrintN("")
  Next
  PrintN("")
EndProcedure

Procedure loadMatrix(Array a(2))
  Protected rows, columns, i, j
  Read.i rows
  Read.i columns

  Dim a(rows - 1, columns - 1)

  For i = 0 To rows - 1
    For j = 0 To columns - 1
      Read.i a(i, j)
    Next
  Next
EndProcedure

Dim a(0,0)
Dim b(0,0)
Dim c(0,0)

If OpenConsole()
  loadMatrix(a()): displayMatrix(a(), "matrix a")
  loadMatrix(b()): displayMatrix(b(), "matrix b")

  If multiplyMatrix(a(), b(), c())
    displayMatrix(c(), "product of a * b")
  Else
    PrintN("product of a * b is undefined")
  EndIf

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
matrix a: (2, 3)
1   2   3
4   5   6

matrix b: (3, 1)
1
5
9

product of a * b: (2, 1)
38
83
```



## Python


```python
a=((1,  1,  1,   1), # matrix A #
     (2,  4,  8,  16),
     (3,  9, 27,  81),
     (4, 16, 64, 256))

b=((  4  , -3  ,  4/3.,  -1/4. ), # matrix B #
     (-13/3., 19/4., -7/3.,  11/24.),
     (  3/2., -2.  ,  7/6.,  -1/4. ),
     ( -1/6.,  1/4., -1/6.,   1/24.))



def MatrixMul( mtx_a, mtx_b):
    tpos_b = zip( *mtx_b)
    rtn = [[ sum( ea*eb for ea,eb in zip(a,b)) for b in tpos_b] for a in mtx_a]
    return rtn


v = MatrixMul( a, b )

print 'v = ('
for r in v:
    print '[',
    for val in r:
        print '%8.2f '%val,
    print ']'
print ')'


u = MatrixMul(b,a)

print 'u = '
for r in u:
    print '[',
    for val in r:
        print '%8.2f '%val,
    print ']'
print ')'
```


Another one, {{trans|Scheme}}

```python
from operator import mul

def matrixMul(m1, m2):
  return map(
    lambda row:
      map(
        lambda *column:
          sum(map(mul, row, column)),
        *m2),
    m1)
```


Using list comprehensions, multiplying matrices represented as lists of lists. (Input is not validated):

```python
def mm(A, B):
    return [[sum(x * B[i][col] for i,x in enumerate(row)) for col in range(len(B[0]))] for row in A]
```


Another one, use numpy the most popular array package for python

```python

import numpy as np
np.dot(a,b)
#or if a is an array
a.dot(b)
```



## R


```r
a %*% b
```



## Racket

{{trans|Scheme}}


```racket

#lang racket
(define (m-mult m1 m2)
  (for/list ([r m1])
    (for/list ([c (apply map list m2)])
      (apply + (map * r c)))))
(m-mult '((1 2) (3 4)) '((5 6) (7 8)))
;; -> '((19 22) (43 50))

```


Alternative:

```racket

#lang racket
(require math)
(matrix* (matrix [[1 2] [3 4]]) (matrix [[5 6] [7 8]]))
;; -> (array #[#[19 22] #[43 50]])

```



## Rascal


```Rascal
public rel[real, real, real] matrixMultiplication(rel[real x, real y, real v] matrix1, rel[real x, real y, real v] matrix2){
	if (max(matrix1.x) == max(matrix2.y)){
		p = {<x1,y1,x2,y2, v1*v2> | <x1,y1,v1> <- matrix1, <x2,y2,v2> <- matrix2};

		result = {};
		for (y <- matrix1.y){
			for (x <- matrix2.x){
				v = (0.0 | it + v | <x1, y1, x2, y2, v> <- p,  x==x2 && y==y1, x1==y2 && y2==x1);
				result += <x,y,v>;
			}
		}
		return result;
	}
	else throw "Matrix sizes do not match.";

//a matrix, given by a relation of the x-coordinate, y-coordinate and value.
public rel[real x, real y, real v] matrixA = {
<0.0,0.0,12.0>, <0.0,1.0, 6.0>, <0.0,2.0,-4.0>,
<1.0,0.0,-51.0>, <1.0,1.0,167.0>, <1.0,2.0,24.0>,
<2.0,0.0,4.0>, <2.0,1.0,-68.0>, <2.0,2.0,-41.0>
};
```



## REXX


```rexx
/*REXX program multiplies two matrices together, displays the matrices and the results. */
x.=;  x.1=1 2                                    /*╔═══════════════════════════════════╗*/
      x.2=3 4                                    /*║ As none of the matrix values have ║*/
      x.3=5 6                                    /*║ a sign,  quotes aren't needed.    ║*/
      x.4=7 8                                    /*╚═══════════════════════════════════╝*/
                 do   r=1  while x.r\==''        /*build the "A" matrix from X. numbers.*/
                   do c=1  while x.r\=='';   parse var x.r a.r.c x.r;      end  /*c*/
                 end   /*r*/
Arows=r-1                                        /*adjust the number of rows  (DO loop).*/
Acols=c-1                                        /*   "    "     "    " cols    "   "  .*/
y.=;  y.1=1 2 3
      y.2=4 5 6
                 do   r=1  while y.r\==''        /*build the "B" matrix from Y. numbers.*/
                   do c=1  while y.r\=='';   parse var y.r b.r.c y.r;      end  /*c*/
                 end   /*r*/
Brows=r-1                                        /*adjust the number of rows  (DO loop).*/
Bcols=c-1                                        /*   "     "    "    " cols    "   "   */
c.=0;  w=0                                       /*W  is max width of an matrix element.*/
            do       i=1  for Arows              /*multiply matrix  A  and  B  ───►   C */
              do     j=1  for Bcols
                  do k=1  for Acols;    c.i.j=c.i.j  +  a.i.k * b.k.j
                                                     w=max(w, length(c.i.j))
                  end   /*k*/                    /*  ↑                                  */
              end       /*j*/                    /*  └──◄─── maximum width of elements. */
            end         /*i*/

call showMatrix  'A',  Arows,  Acols             /*display matrix  A ───►  the terminal.*/
call showMatrix  'B',  Brows,  Bcols             /*   "       "    B ───►   "     "     */
call showMatrix  'C',  Arows,  Bcols             /*   "       "    C ───►   "     "     */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
showMatrix: parse arg mat,rows,cols;   say;   say center(mat 'matrix', cols*(w+1) +4, "─")
                    do   r=1  for rows;  _=
                      do c=1  for cols;  _=_ right(value(mat'.'r"."c), w);  end;     say _
                    end   /*r*/
            return
```

'''output'''

```txt

─A matrix─
  1  2
  3  4
  5  6
  7  8

──B matrix───
  1  2  3
  4  5  6

──C matrix───
  9 12 15
 19 26 33
 29 40 51
 39 54 69

```



## Ring


```ring

load "stdlib.ring"
n = 3
C = newlist(n,n)
A = [[1,2,3], [4,5,6], [7,8,9]]
B = [[1,0,0], [0,1,0], [0,0,1]]
for i = 1 to n
    for j = 1 to n
       for k = 1 to n
           C[i][k] += A[i][j] * B[j][k]
       next
    next
next
for i = 1 to n
    for j = 1 to n
        see C[i][j] + " "
    next
    see nl
next

```

Output:

```txt

123
456
789

```



## Ruby

Using 'matrix' from the standard library:

```ruby
require 'matrix'

Matrix[[1, 2],
       [3, 4]] * Matrix[[-3, -8, 3],
                        [-2,  1, 4]]
```

{{out}}
 Matrix[[-7, -6, 11], [-17, -20, 25]]

Version for lists: {{trans|Haskell}}

```ruby
def matrix_mult(a, b)
  a.map do |ar|
    b.transpose.map do |bc|
      ar.zip(bc).map(&:*).inject(&:+)
    end
  end
end
```



## Rust


```rust

struct Matrix {
    dat: [[f32; 3]; 3]
}

impl Matrix {
    pub fn mult_m(a: Matrix, b: Matrix) -> Matrix
    {
        let mut out = Matrix {
            dat: [[0., 0., 0.],
                  [0., 0., 0.],
                  [0., 0., 0.]
                  ]
        };

        for i in 0..3{
            for j in 0..3 {
                for k in 0..3 {
                    out.dat[i][j] += a.dat[i][k] * b.dat[k][j];
                }
            }
        }

        out
    }

    pub fn print(self)
    {
        for i in 0..3 {
            for j in 0..3 {
                print!("{} ", self.dat[i][j]);
            }
            print!("\n");
        }
    }
}

fn main()
{
    let  a = Matrix {
        dat: [[1., 2., 3.],
              [4., 5., 6.],
              [7., 8., 9.]
              ]
    };

    let  b = Matrix {
        dat: [[1., 0., 0.],
              [0., 1., 0.],
              [0., 0., 1.]]
    };



        let c = Matrix::mult_m(a, b);


    c.print();
}


```



## Scala

{{works with|Scala|2.8}}
Assuming an array of arrays representation:


```scala
def mult[A](a: Array[Array[A]], b: Array[Array[A]])(implicit n: Numeric[A]) = {
  import n._
  for (row <- a)
  yield for(col <- b.transpose)
        yield row zip col map Function.tupled(_*_) reduceLeft (_+_)
}
```


For any subclass of <code>Seq</code> (which does not include Java-specific arrays):


```scala
def mult[A, CC[X] <: Seq[X], DD[Y] <: Seq[Y]](a: CC[DD[A]], b: CC[DD[A]])
(implicit n: Numeric[A]): CC[DD[A]] = {
  import n._
  for (row <- a)
  yield for(col <- b.transpose)
        yield row zip col map Function.tupled(_*_) reduceLeft (_+_)
}
```


Examples:


```txt

scala> Array(Array(1, 2), Array(3, 4))
res0: Array[Array[Int]] = Array(Array(1, 2), Array(3, 4))

scala> Array(Array(-3, -8, 3), Array(-2, 1, 4))
res1: Array[Array[Int]] = Array(Array(-3, -8, 3), Array(-2, 1, 4))

scala> mult(res0, res1)
res2: Array[scala.collection.mutable.GenericArray[Int]] = Array(GenericArray(-7, -6, 11), GenericArray(-17, -20, 25))

scala> res0.map(_.toList).toList
res5: List[List[Int]] = List(List(1, 2), List(3, 4))

scala> res1.map(_.toList).toList
res6: List[List[Int]] = List(List(-3, -8, 3), List(-2, 1, 4))

scala> mult(res5, res6)
res7: Seq[Seq[Int]] = List(List(-7, -6, 11), List(-17, -20, 25))

```


A fully generic multiplication that returns the same collection as received is possible,
but much more verbose.


## Scheme

{{trans|Common Lisp}}
This version works on lists of lists:

```scheme
(define (matrix-multiply matrix1 matrix2)
  (map
   (lambda (row)
    (apply map
     (lambda column
      (apply + (map * row column)))
     matrix2))
   matrix1))
```


 > (matrix-multiply '((1 2) (3 4)) '((-3 -8 3) (-2 1 4)))
 ((-7 -6 11) (-17 -20 25))


## Seed7


```seed7
const type: matrix is array array float;

const func matrix: (in matrix: left) * (in matrix: right) is func
  result
    var matrix: result is matrix.value;
  local
    var integer: i is 0;
    var integer: j is 0;
    var integer: k is 0;
    var float: accumulator is 0.0;
  begin
    if length(left[1]) <> length(right) then
      raise RANGE_ERROR;
    else
      result := length(left) times length(right[1]) times 0.0;
      for i range 1 to length(left) do
        for j range 1 to length(right) do
          accumulator := 0.0;
          for k range 1 to length(left) do
            accumulator +:= left[i][k] * right[k][j];
          end for;
          result[i][j] := accumulator;
        end for;
      end for;
    end if;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/math.htm#mmult]


## SequenceL


:The product of the ''m''×''p'' matrix ''A'' with the ''p''×''n'' matrix ''B'' is the ''m''×''n'' matrix whose (''i'',''j'')'th entry is
::<math>\sum_{k=1}^p A(i,k)B(k,j)</math>

The SequenceL definition mirrors that definition more or less exactly:


```sequencel
matmul(A(2), B(2)) [i,j] :=
        let k := 1...size(B);
        in  sum( A[i,k] * B[k,j] );

//Example Use
a := [[1, 2],
      [3, 4]];

b := [[-3, -8, 3],
      [-2,  1, 4]];

test := matmul(a, b);
```


It can be written a little more simply using the all keyword:


```sequencel
matmul(A(2), B(2)) [i,j] := sum( A[i,all] * B[all,j] );
```



## Sidef


```ruby
func matrix_multi(a, b) {
    var m = [[]]
    for r in ^a {
        for c in ^b[0] {
            for i in ^b {
                m[r][c] := 0 += (a[r][i] * b[i][c])
            }
        }
    }
    return m
}
 
var a = [
          [1, 2],
          [3, 4],
          [5, 6],
          [7, 8]
        ]
 
var b = [
          [1, 2, 3],
          [4, 5, 6]
        ]
 
for line in matrix_multi(a, b) {
    say line.map{|i|'%3d' % i }.join(', ')
}
```

{{out}}

```txt
  9,  12,  15
 19,  26,  33
 29,  40,  51
 39,  54,  69
```



## SPAD

{{works with|FriCAS}}
{{works with|OpenAxiom}}
{{works with|Axiom}}

```SPAD
(1) -> A:=matrix [[1,2],[3,4],[5,6],[7,8]]

        +1  2+
        |    |
        |3  4|
   (1)  |    |
        |5  6|
        |    |
        +7  8+
                                                        Type: Matrix(Integer)
(2) -> B:=matrix [[1,2,3],[4,5,6]]

        +1  2  3+
   (2)  |       |
        +4  5  6+
                                                        Type: Matrix(Integer)
(3) -> A*B

        +9   12  15+
        |          |
        |19  26  33|
   (3)  |          |
        |29  40  51|
        |          |
        +39  54  69+
                                                        Type: Matrix(Integer)
```


Domain:[http://fricas.github.io/api/Matrix.html?highlight=matrix Matrix(R)]


## SQL


```sql
CREATE TABLE a (x integer, y integer, e real);
CREATE TABLE b (x integer, y integer, e real);

-- test data
-- A is a 2x2 matrix
INSERT INTO a VALUES(0,0,1); INSERT INTO a VALUES(1,0,2);
INSERT INTO a VALUES(0,1,3); INSERT INTO a VALUES(1,1,4);

-- B is a 2x3 matrix
INSERT INTO b VALUES(0,0,-3); INSERT INTO b VALUES(1,0,-8); INSERT INTO b VALUES(2,0,3);
INSERT INTO b VALUES(0,1,-2); INSERT INTO b VALUES(1,1, 1); INSERT INTO b VALUES(2,1,4);

-- C is 2x2 * 2x3 so will be a 2x3 matrix
SELECT rhs.x, lhs.y, (SELECT sum(a.e*b.e) FROM a, b
                             WHERE a.y = lhs.y
                               AND b.x = rhs.x
                               AND a.x = b.y)
       INTO TABLE c
       FROM a AS lhs, b AS rhs
       WHERE lhs.x = 0 AND rhs.y = 0;
```



## Standard ML


```sml
structure Matrix = struct
local
    open Array2
    fun dot(x,y) = Vector.foldli (fn (i,xi,agg) => agg+xi*Vector.sub(y,i)) 0 x
in
val fromList = fromList
fun x*y = tabulate ColMajor (nRows x, nCols y, fn (i,j) => dot(row(x,i),column(y,j)))
(* for display *)
fun toList a =
    List.tabulate(nRows a, fn i => List.tabulate(nCols a, fn j => sub(a,i,j)))
end
end;
(* example *)
let open Matrix
    val m1 = fromList [[1,2],[3,4]]
    val m2 = fromList [[~3,~8,3],[~2,1,4]]
in
    toList (m1*m2)
end;
```

'''Output:'''

```sml
val it = [[~7,~6,11],[~17,~20,25]] : int list list
```


## Stata


###  Stata matrices


```stata
. mat a=1,2,3\4,5,6
. mat b=1,1,0,0\1,0,0,1\0,0,1,1
. mat c=a*b
. mat list c

c[2,4]
    c1  c2  c3  c4
r1   3   1   3   5
r2   9   4   6  11
```


###  Mata


```stata
: a=1,2,3\4,5,6
: b=1,1,0,0\1,0,0,1\0,0,1,1
: a*b
        1    2    3    4
    +---------------------+
  1 |   3    1    3    5  |
  2 |   9    4    6   11  |
    +---------------------+
```




## Swift



```swift
@inlinable
public func matrixMult<T: Numeric>(_ m1: [[T]], _ m2: [[T]]) -> [[T]] {
  let n = m1[0].count
  let m = m1.count
  let p = m2[0].count

  guard m != 0 else {
    return []
  }

  precondition(n == m2.count)

  var ret = Array(repeating: Array(repeating: T.zero, count: p), count: m)

  for i in 0..<m {
    for j in 0..<p {
      for k in 0..<n {
        ret[i][j] += m1[i][k] * m2[k][j]
      }
    }
  }

  return ret
}

@inlinable
public func printMatrix<T>(_ matrix: [[T]]) {
  guard !matrix.isEmpty else {
    print()

    return
  }

  let rows = matrix.count
  let cols = matrix[0].count

  for i in 0..<rows {
    for j in 0..<cols {
      print(matrix[i][j], terminator: " ")
    }

    print()
  }
}

let m1 = [
  [6.5, 2, 3],
  [4.5, 1, 5]
]

let m2 = [
  [10.0, 16, 23, 50],
  [12, -8, 16, -4],
  [70, 60, -1, -2]
]

let m3 = matrixMult(m1, m2)

printMatrix(m3)
```


{{out}}


```txt
299.0 268.0 178.5 311.0
407.0 364.0 114.5 211.0
```



## Tailspin


```tailspin

templates matmul@{B:}
  $ -> [i](def r: $;
    [1..$B(1)::length -> (def j: $; @: 0;
      1..$r::length -> @: $@ + $r($) * $B($;$j);
      $@ !)] !
  ) !
end matmul

templates printMatrix@{w:}
  templates formatN
    @: [];
    $ -> #
    '$@ -> $::length~..$w -> ' ';$@(-1..1:-1)...;' !
    <1..> ..|@: $ mod 10; $ / 10 -> #
    <0?($@ <[](0)>)> ..|@: 0;
  end formatN
  $... -> '|$(1) -> formatN;$(2..-1)... -> ', $ -> formatN;';|
' !
end printMatrix

def a: [[1, 2, 3], [4, 5, 6]];
'a:
' -> !OUT::write
$a -> printMatrix@{w:2} -> !OUT::write

def b: [[0, 1], [2, 3], [4, 5]];
'
b:
' -> !OUT::write
$b -> printMatrix@{w:2} -> !OUT::write
'
axb:
' -> !OUT::write
$a -> matmul@{B: $b} -> printMatrix@{w:2} -> !OUT::write

```

{{out}}

```txt

a:
| 1,  2,  3|
| 4,  5,  6|

b:
| 0,  1|
| 2,  3|
| 4,  5|

axb:
|16, 22|
|34, 49|

```



## Tcl

{{works with|Tcl|8.5}}

```tcl
package require Tcl 8.5
namespace path ::tcl::mathop
proc matrix_multiply {a b} {
    lassign [size $a] a_rows a_cols
    lassign [size $b] b_rows b_cols
    if {$a_cols != $b_rows} {
        error "incompatible sizes: a($a_rows, $a_cols), b($b_rows, $b_cols)"
    }
    set temp [lrepeat $a_rows [lrepeat $b_cols 0]]
    for {set i 0} {$i < $a_rows} {incr i} {
        for {set j 0} {$j < $b_cols} {incr j} {
            set sum 0
            for {set k 0} {$k < $a_cols} {incr k} {
                set sum [+ $sum [* [lindex $a $i $k] [lindex $b $k $j]]]
            }
            lset temp $i $j $sum
        }
    }
    return $temp
}
```

Using the <code>print_matrix</code> procedure defined in [[Matrix Transpose#Tcl]]

```txt
% print_matrix [matrix_multiply {{1 2} {3 4}} {{-3 -8 3} {-2 1 4}}]
 -7  -6 11
-17 -20 25
```


=={{header|TI-83 BASIC}}==
Store your matrices in <tt>[A]</tt> and <tt>[B]</tt>.

```ti83b
Disp [A]*[B]
```

An error will show if the matrices have invalid dimensions for multiplication.


'''Other way:''' enter directly your matrices:

```ti83b
[[1,2][3,4][5,6][7,8]]*[[1,2,3][4,5,6]]
```

{{out}}
  [[9  12 15]
   [19 26 33]
   [29 40 51]
   [39 54 69]]]

=={{header|TI-89 BASIC}}==

{{trans|Mathematica}}


```ti89b
[1,2; 3,4; 5,6; 7,8] → m1
[1,2,3; 4,5,6] → m2
m1 * m2
```


Or without the variables:


```ti89b
[1,2; 3,4; 5,6; 7,8] * [1,2,3; 4,5,6]
```


The result (without prettyprinting) is:


```ti89b
[[9,12,15][19,26,33][29,40,51][39,54,69]]
```



## UNIX Shell


```bash

#!/bin/bash

DELAY=0 # increase this if printing of matrices should be slower

echo "This script takes two matrices, henceforth called A and B,
and returns their product, AB.

For the time being, matrices can have integer components only.

"

read -p "Number of rows    of matrix A:  " arows
read -p "Number of columns of matrix A:  " acols
brows="$acols"
echo
echo    "Number of rows    of matrix B:  "$brows
read -p "Number of columns of matrix B:  " bcols

crows="$arows"
ccols="$bcols"
echo

echo "Number of rows    of matrix AB:  " $crows
echo "Number of columns of matrix AB:  " $ccols
echo
echo

matrixa=( )
matrixb=( )

# input matrix A

maxlengtha=0
for ((row=1; row<=arows; row++)); do
    for ((col=1; col<=acols; col++)); do
	checkentry="false"
	while [ "$checkentry" != "true" ]; do
	    read -p "Enter component A[$row, $col]:  " number
	    index=$(((row-1)*acols+col))
	    matrixa[$index]="$number"
	    [ "${matrixa[$index]}" -eq "$number" ] && checkentry="true"
	    echo
	done
	entry="${matrixa[$index]}"
	[ "${#entry}" -gt "$maxlengtha" ] && maxlengtha="${#entry}"
    done
    echo
done

# print matrix A to guard against errors

if [ "$maxlengtha" -le "5" ]; then
    width=8
else
    width=$((maxlengtha + 3))
fi

echo "This is matrix A:

"

for ((row=1; row<=arows; row++)); do
    for ((col=1; col<=acols; col++)); do

	index=$(((row-1)*acols+col))
	printf "%${width}d" "${matrixa[$index]}"
	sleep "$DELAY"

    done
    echo; echo # printf %s "\n\n" does not work...
done

echo
echo

# input matrix B

maxlengthb=0
for ((row=1; row<=brows; row++)); do
    for ((col=1; col<=bcols; col++)); do
	checkentry="false"
	while [ "$checkentry" != "true" ]; do
	    read -p "Enter component B[$row, $col]:  " number
	    index=$(((row-1)*bcols+col))
	    matrixb[$index]="$number"
	    [ "${matrixb[$index]}" -eq "$number" ] && checkentry="true"
	    echo
	done
	entry="${matrixb[$index]}"
	[ "${#entry}" -gt "$maxlengthb" ] && maxlengthb="${#entry}"
    done
    echo
done

# print matrix B to guard against errors

if [ "$maxlengthb" -le "5" ]; then
    width=8
else
    width=$((maxlengthb + 3))
fi

echo "This is matrix B:

"

for ((row=1; row<=brows; row++)); do
    for ((col=1; col<=bcols; col++)); do

	index=$(((row-1)*bcols+col))
	printf "%${width}d" "${matrixb[$index]}"
	sleep "$DELAY"

    done
    echo; echo # printf %s "\n\n" does not work...
done

read -p "Hit enter to continue"

# calculate matrix C := AB

maxlengthc=0
time for ((row=1; row<=crows; row++)); do
    for ((col=1; col<=ccols; col++)); do

	# calculate component C[$row, $col]

	runningtotal=0
	for ((j=1; j<=acols; j++)); do
	    rowa="$row"
	    cola="$j"
	    indexa=$(((rowa-1)*acols+cola))
	    rowb="$j"
	    colb="$col"
	    indexb=$(((rowb-1)*bcols+colb))

	    entry_from_A=${matrixa[$indexa]}
	    entry_from_B=${matrixb[$indexb]}

	    subtotal=$((entry_from_A * entry_from_B))
	    ((runningtotal+=subtotal))
	done

	number="$runningtotal"

	# store component in the result array
	index=$(((row-1)*ccols+col))
	matrixc[$index]="$number"

	entry="${matrixc[$index]}"
	[ "${#entry}" -gt "$maxlengthc" ] && maxlengthc="${#entry}"
    done
done

echo
read -p "Hit enter to continue"
echo

# print the matrix C

if [ "$maxlengthc" -le "5" ]; then
    width=8
else
    width=$((maxlengthc + 3))
fi

echo "The product matrix is:

"

for ((row=1; row<=crows; row++)); do
    for ((col=1; col<=ccols; col++)); do

	index=$(((row-1)*ccols+col))
	printf "%${width}d" "${matrixc[$index]}"
	sleep "$DELAY"

    done
    echo; echo # printf %s "\n\n" does not work...
done

echo
echo

```



## Ursala

There is a library function for matrix multiplication of IEEE double precision floating point
numbers. This example shows how to define and use a matrix multiplication function over
any chosen field given only the relevant product and sum functions, in this case for
the built in rational number type.


```Ursala
#import rat

a =

<
   <1/1,  1/1,  1/1,   1/1>,
   <2/1,  4/1,  8/1,  16/1>,
   <3/1,  9/1, 27/1,  81/1>,
   <4/1, 16/1, 64/1, 256/1>>

b =

<
   <  4/1, -3/1,  4/3,  -1/4>,
   <-13/3, 19/4, -7/3,  11/24>,
   <  3/2, -2/1,  7/6,  -1/4>,
   < -1/6,  1/4, -1/6,   1/24>>

mmult = *rK7lD *rlD sum:-0.+ product*p

#cast %qLL

test = mmult(a,b)
```

{{out}}

```txt
<
   <1/1,0/1,0/1,0/1>,
   <0/1,1/1,0/1,0/1>,
   <0/1,0/1,1/1,0/1>,
   <0/1,0/1,0/1,1/1>>
```



## VBA

Using Excel. The resulting matrix should be smaller than 5461 elements.

```vb
Function matrix_multiplication(a As Variant, b As Variant) As Variant
    matrix_multiplication = WorksheetFunction.MMult(a, b)
End Function
```


## VBScript


```vb

Dim matrix1(2,2)
matrix1(0,0) = 3 : matrix1(0,1) = 7 : matrix1(0,2) = 4
matrix1(1,0) = 5 : matrix1(1,1) = -2 : matrix1(1,2) = 9
matrix1(2,0) = 8 : matrix1(2,1) = -6 : matrix1(2,2) = -5
Dim matrix2(2,2)
matrix2(0,0) = 9 : matrix2(0,1) = 2 : matrix2(0,2) = 1
matrix2(1,0) = -7 : matrix2(1,1) = 3 : matrix2(1,2) = -10
matrix2(2,0) = 4 : matrix2(2,1) = 5 : matrix2(2,2) = -6

Call multiply_matrix(matrix1,matrix2)

Sub multiply_matrix(arr1,arr2)
	For i = 0 To UBound(arr1)
		For j = 0 To 2
			WScript.StdOut.Write (arr1(i,j) * arr2(i,j)) & vbTab
		Next
		WScript.StdOut.WriteLine
	Next
End Sub

```


{{Out}}

```txt

27	14	4
-35	-6	-90
32	-30	30

```



## Visual FoxPro


```vfp

LOCAL ARRAY a[4,2], b[2,3], c[4,3]
CLOSE DATABASES ALL
*!* The arrays could be created directly but I prefer to do this:
CREATE CURSOR mat1 (c1 I, c2 I)
CREATE CURSOR mat2 (c1 I, c2 I, c3 I)
*!* Since matrix multiplication of integer arrays
*!* involves only multiplication and addition,
*!* the result will contain integers
CREATE CURSOR result (c1 I, c2 I, c3 I)
INSERT INTO mat1 VALUES (1, 2)
INSERT INTO mat1 VALUES (3, 4)
INSERT INTO mat1 VALUES (5, 6)
INSERT INTO mat1 VALUES (7, 8)
SELECT * FROM mat1 INTO ARRAY a

INSERT INTO mat2 VALUES (1, 2, 3)
INSERT INTO mat2 VALUES (4, 5, 6)
SELECT * FROM mat2 INTO ARRAY b
STORE 0 TO c
MatMult(@a,@b,@c)
SELECT result
APPEND FROM ARRAY c
BROWSE


PROCEDURE MatMult(aa, bb, cc)
LOCAL n As Integer, m As Integer, p As Integer, i As Integer, j As Integer, k As Integer
IF ALEN(aa,2) = ALEN(bb,1)
	n = ALEN(aa,2)
	m = ALEN(aa,1)
	p = ALEN(bb,2)
	FOR i = 1 TO m
		FOR j = 1 TO p
			FOR k = 1 TO n
				cc[i,j] = cc[i,j] + aa[i,k]*bb[k,j]
			ENDFOR
		ENDFOR
	ENDFOR
ELSE
	? "Invalid dimensions"
ENDIF
ENDPROC

```



## XPL0


```XPL0
proc Mat4x1Mul(M, V);   \Multiply matrix M times column vector V
real M,     \4x4 matrix  [M] * [V] -> [V]
     V;     \column vector
real W(4);  \working copy of column vector
int  R;     \row
[for R:= 0 to 4-1 do
    W(R):= M(R,0)*V(0) + M(R,1)*V(1) + M(R,2)*V(2) + M(R,3)*V(3);
for R:= 0 to 4-1 do V(R):= W(R);
];

proc Mat4x4Mul(M, N);   \Multiply matrix M times matrix N
real M, N;   \4x4 matrices       [M] * [N] -> [N]
real W(4,4); \working copy of matrix N
int  C;      \column
[for C:= 0 to 4-1 do
       [W(0,C):= M(0,0)*N(0,C) + M(0,1)*N(1,C) + M(0,2)*N(2,C) + M(0,3)*N(3,C);
        W(1,C):= M(1,0)*N(0,C) + M(1,1)*N(1,C) + M(1,2)*N(2,C) + M(1,3)*N(3,C);
        W(2,C):= M(2,0)*N(0,C) + M(2,1)*N(1,C) + M(2,2)*N(2,C) + M(2,3)*N(3,C);
        W(3,C):= M(3,0)*N(0,C) + M(3,1)*N(1,C) + M(3,2)*N(2,C) + M(3,3)*N(3,C);
        ];
for C:= 0 to 4-1 do
       [N(0,C):= W(0,C);
        N(1,C):= W(1,C);
        N(2,C):= W(2,C);
        N(3,C):= W(3,C);
        ];
];
```



## XSLT 1.0

With input document ...


```xml
<?xml-stylesheet href="matmul.templ.xsl" type="text/xsl"?>
<mult>
  <A>
    <r><c>1</c><c>2</c></r>
    <r><c>3</c><c>4</c></r>
    <r><c>5</c><c>6</c></r>
    <r><c>7</c><c>8</c></r>
  </A>
  <B>
    <r><c>1</c><c>2</c><c>3</c></r>
    <r><c>4</c><c>5</c><c>6</c></r>
  </B>
</mult>
```


... and this referenced stylesheet ...


```xml
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
>
  <xsl:output method="html"/>

  <xsl:template match="/mult">
    <table>
      <tr><td>╭</td><td colspan="{count(*[2]/*[1]/*)}"/><td>╮</td></tr>
      <xsl:call-template name="prodMM">
        <xsl:with-param name="A" select="*[1]/*"/>
        <xsl:with-param name="B" select="*[2]/*"/>
      </xsl:call-template>
      <tr><td>╰</td><td colspan="{count(*[2]/*[1]/*)}"/><td>╯</td></tr>
    </table>
  </xsl:template>

  <xsl:template name="prodMM">
    <xsl:param name="A"/>
    <xsl:param name="B"/>

    <xsl:if test="$A/*">
      <tr>
        <td>│</td>
        <xsl:call-template name="prodVM">
          <xsl:with-param name="a" select="$A[1]/*"/>
          <xsl:with-param name="B" select="$B"/>
        </xsl:call-template>
        <td>│</td>
      </tr>

      <xsl:call-template name="prodMM">
        <xsl:with-param name="A" select="$A[position()>1]"/>
        <xsl:with-param name="B" select="$B"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template name="prodVM">
    <xsl:param name="a"/>
    <xsl:param name="B"/>
    <xsl:param name="col" select="1"/>

    <xsl:if test="$B/*[$col]">
      <td align="right">
        <xsl:call-template name="prod">
          <xsl:with-param name="a" select="$a"/>
          <xsl:with-param name="b" select="$B/*[$col]"/>
        </xsl:call-template>
      </td>

      <xsl:call-template name="prodVM">
        <xsl:with-param name="a"   select="$a"/>
        <xsl:with-param name="B"   select="$B"/>
        <xsl:with-param name="col" select="$col+1"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template name="prod">
    <xsl:param name="a"/>
    <xsl:param name="b"/>

    <xsl:if test="not($a)">0</xsl:if>

    <xsl:if test="$a">
      <xsl:variable name="res">
        <xsl:call-template name="prod">
          <xsl:with-param name="a" select="$a[position()>1]"/>
          <xsl:with-param name="b" select="$b[position()>1]"/>
        </xsl:call-template>
      </xsl:variable>

      <xsl:value-of select="$a[1] * $b[1] + $res"/>
    </xsl:if>
  </xsl:template>

</xsl:stylesheet>
```


{{out}} (in a browser):

```txt

╭          ╮
│  9 12	15 │
│ 19 26	33 │
│ 29 40	51 │
│ 39 54	69 │
╰          ╯

```


You may try in your browser: [[http://www.stamm-wilbrandt.de/en/blog/matmul.templ.xml]]

A slightly smaller version of above stylesheet making use of (Non-"XSLT 1.0") EXSLT functions can be founde here: [[https://www.ibm.com/developerworks/mydeveloperworks/blogs/HermannSW/entry/matrix_multiplication30]]


## zkl

Using the GNU Scientific Library:

```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
A:=GSL.Matrix(4,2).set(1,2, 3,4, 5,6, 7,8);
B:=GSL.Matrix(2,3).set(1,2,3, 4,5,6);
(A*B).format().println();   // creates a new matrix
```

{{out}}

```txt

      9.00,     12.00,     15.00
     19.00,     26.00,     33.00
     29.00,     40.00,     51.00
     39.00,     54.00,     69.00

```

Or, using lists:
{{trans|BASIC}}

```zkl
fcn matMult(a,b){
   n,m,p:=a[0].len(),a.len(),b[0].len();
   ans:=(0).pump(m,List().write, (0).pump(p,List,0).copy); // matrix of zeros
   foreach i,j,k in (m,p,n){ ans[i][j]+=a[i][k]*b[k][j]; }
   ans
}
```


```zkl
a:=L( L(1,2,), L(3,4,), L(5,6,), L(7,8) );
b:=L( L(1,2,3,), L(4,5,6) );
printM(matMult(a,b));

fcn printM(m){ m.pump(Console.println,rowFmt) }
fcn rowFmt(row){ ("%4d "*row.len()).fmt(row.xplode()) }
```

{{out}}

```txt

   9   12   15
  19   26   33
  29   40   51
  39   54   69

```



## zonnon


```zonnon

module MatrixOps;
type
	Matrix = array {math} *,* of integer;


	procedure WriteMatrix(x: array {math} *,* of integer);
	var
		i,j: integer;
	begin
		for i := 0 to len(x,0) - 1 do
			for j := 0 to len(x,1) - 1 do
				write(x[i,j]);
			end;
			writeln;
		end
	end WriteMatrix;

	procedure Multiplication;
	var
		a,b: Matrix;
	begin
		a := [[1,2],[3,4],[5,6],[7,8]];
		b := [[1,2,3],[4,5,6]];
		WriteMatrix(a * b);
	end Multiplication;

begin
	Multiplication;
end MatrixOps.

```



## ZPL


```ZPL

program matmultSUMMA;

prototype GetSingleDim(infile:file):integer;
prototype GetInnerDim(infile1:file; infile2:file):integer;

config var
          Afilename: string = "";
          Bfilename: string = "";

          Afile: file = open(Afilename,file_read);
          Bfile: file = open(Bfilename,file_read);

          default_size:integer = 4;
          m:integer = GetSingleDim(Afile);
          n:integer = GetInnerDim(Afile,Bfile);
          p:integer = GetSingleDim(Bfile);

          iters: integer = 1;

          printinput: boolean = false;
          verbose: boolean = true;
          dotiming: boolean = false;

region
       RA = [1..m,1..n];
       RB = [1..n,1..p];
       RC = [1..m,1..p];
       FCol = [1..m,*];
       FRow = [*,1..p];

var
    A : [RA] double;
    B : [RB] double;
    C : [RC] double;
    Aflood : [FCol] double;
    Bflood : [FRow] double;


procedure ReadA();
var step:double;
[RA] begin
       if (Afile != znull) then
         read(Afile,A);
       else
         step := 1.0/(m*n);
         A := ((Index1-1)*n + Index2)*step + 1.0;
       end;
     end;


procedure ReadB();
var step:double;
[RB] begin
       if (Bfile != znull) then
         read(Bfile,B);
       else
         step := 1.0/(n*p);
         B := ((Index1-1)*p + Index2)*step + 1.0;
       end;
     end;


procedure matmultSUMMA();
var
    i: integer;
    it: integer;
    runtime: double;
[RC] begin
       ReadA();
       ReadB();

       if (printinput) then
         [RA] writeln("A is:\n",A);
         [RB] writeln("B is:\n",B);
       end;

       ResetTimer();

       for it := 1 to iters do

         C := 0.0;                       -- zero C

         for i := 1 to n do
           [FCol] Aflood := >>[,i] A;       -- flood A col
           [FRow] Bflood := >>[i,] B;       -- flood B row

           C += (Aflood * Bflood);   -- multiply
         end;
       end;

       runtime := CheckTimer();

       if (verbose) then
         writeln("C is:\n",C);
       end;

       if (dotiming) then
         writeln("total runtime  = %12.6f":runtime);
         writeln("actual runtime = %12.6f":runtime/iters);
       end;
     end;


procedure GetSingleDim(infile:file):integer;
var dim:integer;
begin
  if (infile != znull) then
    read(infile,dim);
  else
    dim := default_size;
  end;
  return dim;
end;


procedure GetInnerDim(infile1:file; infile2:file):integer;
var
   col:integer;
   row:integer;
   retval:integer;
begin
  retval := -1;
  if (infile1 != znull) then
    read(infile1,col);
    retval := col;
  end;
  if (infile2 != znull) then
    read(infile2,row);
    if (retval = -1) then
      retval := row;
    else
      if (row != col) then
        halt("ERROR: Inner dimensions don't match");
      end;
    end;
  end;
  if (retval = -1) then
    retval := default_size;
  end;
  return retval;
end;

```

