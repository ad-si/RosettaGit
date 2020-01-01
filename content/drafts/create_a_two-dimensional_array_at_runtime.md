+++
title = "Create a two-dimensional array at runtime"
description = ""
date = 2019-10-04T18:25:11Z
aliases = []
[extra]
id = 1982
[taxonomies]
categories = []
tags = []
+++

{{task|Basic language learning}}
{{data structure}}
Get two integers from the user, then create a two-dimensional array where the two dimensions have the sizes given by those numbers, and which can be accessed in the most natural way possible. Write some element of that array, and then output that element. Finally destroy the array if not done by the language itself.


## 11l


```11l
V width = 3
V height = 5
V myarray = [[0] * width] * height
print(myarray[height-1][width-1])
```



## Ada


```ada

with Ada.Text_Io;
with Ada.Float_Text_Io;
with Ada.Integer_Text_Io;

procedure Two_Dimensional_Arrays is
   type Matrix_Type is array(Positive range <>, Positive range <>) of Float;
   Dim_1 : Positive;
   Dim_2 : Positive;
begin
   Ada.Integer_Text_Io.Get(Item => Dim_1);
   Ada.Integer_Text_Io.Get(Item => Dim_2);
   -- Create an inner block with the correctly sized array
   declare
      Matrix : Matrix_Type(1..Dim_1, 1..Dim_2);
   begin
      Matrix(1, Dim_2) := 3.14159;
      Ada.Float_Text_Io.Put(Item => Matrix(1, Dim_2), Fore => 1, Aft => 5, Exp => 0);
      Ada.Text_Io.New_Line;
   end;
   -- The variable Matrix is popped off the stack automatically
end Two_Dimensional_Arrays;
```

{{omit from|Modula-2}}


## ALGOL 68


```algol68
main:(
  print("Input two positive whole numbers separated by space and press newline:");
  [read int,read int] INT array;
  array[1,1]:=42;
  print (array[1,1])
)
```



## ALGOL W


```algolw
begin
    integer dimension1UpperBound, dimension2UpperBound;
    write( "upper bound for dimension 1: " );
    read( dimension1UpperBound );
    write( "upper bound for dimension 2: " );
    read( dimension2UpperBound );

    begin
        % we start a new block because declarations must precede statements %
        % and variables in array bounds must be from outside the block      %
        integer array matrix ( 1 :: dimension1UpperBound
                             , 1 :: dimension2UpperBound
                             );
        % set the first element - the program will crash if the user input  %
        % upper bounds less than 1                                          %
        matrix( 1, 1 ) := 3;
        % write it                                                          %
        write( matrix( 1, 1 ) );
        % the array is automatically deleted when the block ends            %
    end

end.
```



## APL

Arrays are an integral part of APL.  Array size, shape, and data type can be easily manipulated at runtime.


```APL
array←m n ⍴ 0 ⍝ array of zeros with shape of m by n.

array[1;1]←73 ⍝ assign a value to location 1;1.

array[1;1] ⍝ read the value back out

⎕ex 'array' ⍝ erase the array

```



## AppleScript

AppleScript has no array, but an AppleScript list can be used in a multidimensional fashion. There's no issue with their dimensions, they grow while adding elements. Memory allocation is dynamic.


```AppleScript
set R to text returned of (display dialog "Enter number of rows:" default answer 2) as integer
set c to text returned of (display dialog "Enter number of columns:" default answer 2) as integer
set array to {}
repeat with i from 1 to R
	set temp to {}
	repeat with j from 1 to c
		set temp's end to 0
	end repeat
	set array's end to temp
end repeat

-- Address the first column of the first row:
set array's item 1's item 1 to -10

-- Negative index values can be used to address from the end:
set array's item -1's item -1 to 10

-- Access an item (row 2 column 1):
set x to array's item 2's item 1

return array

-- Destroy array (typically unnecessary since it'll automatically be destroyed once script ends).
set array to {}

```



## AutoHotkey

{{works with|AutoHotkey_L}}

```AutoHotkey
Array := []
InputBox, data,, Enter two integers separated by a Space:`n(ex. 5 7)
StringSplit, i, data, %A_Space%
Array[i1,i2] := "that element"
MsgBox, % "Array[" i1 "," i2 "] = " Array[i1,i2]
```



## AutoIt


```AutoIt
; == get dimensions from user input
$sInput = InputBox('2D Array Creation', 'Input comma separated count of rows and columns, i.e. "5,3"')
$aDimension = StringSplit($sInput, ',', 2)

; == create array
Dim $a2D[ $aDimension[0] ][ $aDimension[1] ]

; == write value to last row/last column
$a2D[ UBound($a2D) -1 ][ UBound($a2D, 2) -1 ] = 'test string'

; == output this value to MsgBox
MsgBox(0, 'Output', 'row[' & UBound($a2D) -1 & '], col[' & UBound($a2D, 2) -1 & ']' & @CRLF & '= ' & $a2D[ UBound($a2D) -1 ][ UBound($a2D, 2) -1 ] )


```



## AWK

AWK has no multidimensional array; but AWK arrays (which are [[Associative array]] indeed) can be used also in a multidimensional fashion. Since AWK arrays are associative arrays, there's no issue in their dimensions: they grow while adding new key-value pair.


```awk
/[0-9]+ [0-9]+/ {
  for(i=0; i < $1; i++) {
    for(j=0; j < $2; j++) {
      arr[i, j] = i*j
    }
  }

  # how to scan "multidim" array as explained in the GNU AWK manual
  for (comb in arr) {
    split(comb, idx, SUBSEP)
    print idx[1] "," idx[2] "->" arr[idx[1], idx[2]]
  }
}
```



## BASIC


=
## Applesoft BASIC
=

```ApplesoftBasic
10 INPUT "ENTER TWO INTEGERS:"; X%, Y%
20 DIM A%(X% - 1, Y% - 1)
30 X% = RND(1) * X%
40 Y% = RND(1) * Y%
50 A%(X%, Y%) = -32767
60 PRINT A%(X%, Y%)
70 CLEAR
```


=
## BBC BASIC
=

```bbcbasic
      INPUT "Enter array dimensions separated by a comma: " a%, b%

      DIM array(a%, b%)
      array(1, 1) = PI
      PRINT array(1, 1)
```


=
## Commodore BASIC
=
Note: Size of array may be limited by RAM availability in some Commodore machines.

```FreeBasic
10 print chr$(147);chr$(14);
15 print "Size of array:"
20 print "Columns (1-20)";:input x%
25 if x%<1 or x%>20 then print "Try again.":goto 20
30 print "Rows (1-20)";:input y%
35 if y%<1 or y%>20 then print "Try again.":goto 30
40 x%=x%-1:y%=y%-1:dim a$(x%,y%)
50 nx=int(rnd(1)*x%):ny=int(rnd(1)*y%)
60 a$(nx,ny)="X"
70 print "Element";nx;",";ny;"= '";a$(nx,ny);"'"
80 clr:rem clear variables from ram

```


{{out}}

```txt

Size of array:
Columns (1-20)? 10
Rows (1-20)? 10
Element 6 , 3 = 'X'

ready.
print a$(6,3)


ready.

```

=
## FreeBASIC
=

```freebasic
' FB 1.05.0 Win64

Dim As Integer i, j
Input "Enter two positive integers, separated by a comma"; i, j
Dim a(1 To i, 1 To j) As Integer
a(i, j) = i * j
Print "a("; Str(i); ","; Str(j); ") ="; a(i, j)
Erase a
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Enter two positive integers, separated by a comma? 4, 7
a(4,7) = 28

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 INPUT PROMPT "Enter array dimensions separated by a coma: ":A,B
110 NUMERIC ARRAY(1 TO A,1 TO B)
120 LET ARRAY(1,1)=PI
130 PRINT ARRAY(1,1)
```


=
## Liberty BASIC
=
Arrays can hold numbers ( eg age( 100)( or strings ( eg name$( 100))
LB arrays can only be one or two dimensioned.
If an array is not DIMensioned explicitly, then the array will be limited to 11 elements, 0 to 10.
Non DIMensioned double subscript arrays will be limited to 100 elements 0 to 9 by 0 to 9.
The DIM statement can be followed by a list of arrays to be dimensioned, separated by commas.
REDIM redimensions an already dimensioned array and clears all elements to zero (or to an empty string in the case of string arrays).
This can be very useful for writing applications that have data sets of unknown size.
If you dimension arrays that are extra large to make sure you can hold data, but only have a small set of data, then all the space you reserved is wasted.
This hurts performance, because memory is set aside for the number of elements in the DIM statement.

```lb

input "Enter first  array dimension "; a
input "Enter second array dimension "; b

dim array( a, b)

array( 1, 1) = 123.456
print array( 1, 1)

end

```


=
## PureBasic
=


```PureBasic
If OpenConsole()
  Define x, y

  Print("Input X-Size: ")
  x = Val(Input())

  Print("Input Y-Size: ")
  y = Val(Input())

  Dim a(x,y)   ; Should really check if x & y are larger then 1, but that would be less fun....

  a(1,1)=Random(1000)
  PrintN("a(1,1)= " + Str(a(1,1)) )

  PrintN("Press ENTER to exit"):Input()
  End          ; Close down and let PureBasic delete the Console and all variables.
EndIf
```


=
## QuickBASIC
=
{{works with|QuickBasic|4.5}}

```qbasic
 CLS
 INPUT a, b 'inputs need to be separated by commas
 DIM array (1 TO a, 1 TO b)
 array(1,1) = 42
 PRINT array(1,1)
 ERASE array
```


=
## Run BASIC
=

```RunBasic
print "Enter array 1 greater than 0"; : input a1
print "Enter array 2 greater than 0"; : input a2

dim chrArray$(max(a1,1),max(a2,1))
dim numArray(max(a1,1),max(a2,1))

chrArray$(1,1) = "Hello"
numArray(1,1)  = 987.2
print chrArray$(1,1);" ";numArray(1,1)
```


=
## Sinclair ZX81 BASIC
=
Arrays are indexed from 1; the only limit on their size (which may be an exigent limit) is the available memory. We create an array, write to a randomly selected element and then print it out, and finally use <code>CLEAR</code> to destroy the array (and all the other variables in the program).

```basic
 10 PRINT "1ST DIMENSION: ";
 20 INPUT D1
 30 PRINT D1
 40 PRINT "2ND DIMENSION: ";
 50 INPUT D2
 60 PRINT D2
 70 DIM A(D1,D1)
 80 PRINT "ARRAY CREATED"
 90 LET X=1+INT (D1*RND)
100 LET Y=1+INT (D2*RND)
110 LET A(X,Y)=37
120 PRINT "ITEM ";X;", ";Y;" = ";A(X,Y)
130 CLEAR
140 PRINT "ARRAY DESTROYED"
```

{{out}}

```txt
1ST DIMENSION: 11
2ND DIMENSION: 6
ARRAY CREATED
ITEM 7, 4 = 37
ARRAY DESTROYED
```


=
## Sinclair ZX Spectrum BASIC
=

```zxbasic
10 INPUT "Size? ";rows;"*";columns
20 DIM a(rows,columns): REM defines a numeric array
30 LET a=INT (RND*rows)+1: LET c=INT (RND*columns+1): REM the array is labelled a, but the letter a is still available for variable assignment
40 LET a(a,c)=1
50 PRINT a(a,c)
60 DIM a(1): REM arrays cannot be removed without CLEARing the entire variable space, but redimensioning them to 1 will save most of the space they used
```


==={{header|TI-83 BASIC}}===

```ti83b
Input "ROWS? ",R
Input "COLS? ",C
{R,C}→dim([A])
42→[A](1,1)
Disp [A](1,1)
DelVar [A]
```


=
## Visual Basic .NET
=


```vbnet
Module Program
    Sub Main()
        Console.WriteLine("Enter two space-delimited integers:")
        Dim input = Console.ReadLine().Split()
        Dim rows = Integer.Parse(input(0))
        Dim cols = Integer.Parse(input(1))

        ' VB uses max-index for array creation.
        Dim arr(rows - 1, cols - 1) As Integer

        arr(0, 0) = 2
        Console.WriteLine(arr(0, 0))
    End Sub
End Module
```


{{out}}

```txt
Enter two space-delimited integers:
5 42
2
```



## C


###  C99

{{works with|C99}}

```c
#include <stdio.h>

int main(int argc, char **argv) {

   int user1 = 0, user2 = 0;
   printf("Enter two integers.  Space delimited, please:  ");
   scanf("%d %d",&user1, &user2);
   int array[user1][user2];
   array[user1/2][user2/2] = user1 + user2;
   printf("array[%d][%d] is %d\n",user1/2,user2/2,array[user1/2][user2/2]);

   return 0;
}
```



###  Traditional Style

Allocate multi-dimensional arrays with a single call to malloc.  The demonstration code builds a rank 3 array.

```c

/*
  assume this file is c.c ,
  compile and run on linux: cc -Wall -g -DCOMPILE_EXAMPLE c.c -lm -o c && ./c
*/

#include<stdlib.h>
#include<stdio.h>

static void error(int status, char*message) {
  fprintf(stderr,"\n%s\n",message);
  exit(status);
}

static void*dwlcalloc(int n,size_t bytes) {
  void*rv = (void*)calloc(n,bytes);
  if (NULL == rv)
    error(1, "memory allocation failure");
  return rv;
}

void*allocarray(size_t rank,size_t*shape,size_t itemSize) {
  /*
     Allocates arbitrary dimensional arrays (and inits all pointers)
     with only 1 call to malloc.  Lambert Electronics, USA, NY.
     This is wonderful because one only need call free once to deallocate
     the space.  Special routines for each size array are not need for
     allocation of for deallocation.  Also calls to malloc might be expensive
     because they might have to place operating system requests.  One call
     seems optimal.
  */
  size_t size,i,j,dataSpace,pointerSpace,pointers,nextLevelIncrement;
  char*memory,*pc,*nextpc;
  if (rank < 2) {
    if (rank < 0)
      error(1,"invalid negative rank argument passed to allocarray");
    size = rank < 1 ? 1 : *shape;
    return dwlcalloc(size,itemSize);
  }
  pointerSpace = 0, dataSpace = 1;
  for (i = 0; i < rank-1; ++i)
    pointerSpace += (dataSpace *= shape[i]);
  pointerSpace *= sizeof(char*);
  dataSpace *= shape[i]*itemSize;
  memory = pc = dwlcalloc(1,pointerSpace+dataSpace);
  pointers = 1;
  for (i = 0; i < rank-2; ) {
    nextpc = pc + (pointers *= shape[i])*sizeof(char*);
    nextLevelIncrement = shape[++i]*sizeof(char*);
    for (j = 0; j < pointers; ++j)
      *((char**)pc) = nextpc, pc+=sizeof(char*), nextpc += nextLevelIncrement;
  }
  nextpc = pc + (pointers *= shape[i])*sizeof(char*);
  nextLevelIncrement = shape[++i]*itemSize;
  for (j = 0; j < pointers; ++j)
    *((char**)pc) = nextpc, pc+=sizeof(char*), nextpc += nextLevelIncrement;
  return memory;
}

#ifdef COMPILE_EXAMPLE

#include<string.h>
#include<math.h>

#define Z 5
#define Y 10
#define X 39

#define BIND(A,L,H) ((L)<(A)?(A)<(H)?(A):(H):(L))

void p_char(void*pv) {
  char s[3];
  int i = 0;
  s[i++] = ' ', s[i++] = *(char*)pv, s[i++] = 0;
  fputs(s, stdout);
}

void display(void*a,size_t rank,size_t*shape,void(*f)(void*)) {
  int i;
  if (0 == rank)
    (*f)(a);
  else if (1 == rank) {
    for (i = 0; i < *shape; ++i)
      (*f)(a+i);
    putchar('\n');
  } else {
    for (i = 0; i < *shape; ++i)
      display(((void**)a)[i], rank-1, shape+1, f);
    putchar('\n');
  }
}

int main() {			/* character cell 3D graphics.  Whoot */
  char***array;
  float x,y,z;
  size_t rank, shape[3], i, j, k;
  rank = 0;
  shape[rank++] = Z, shape[rank++] = Y, shape[rank++] = X;
  array = allocarray(rank, shape, sizeof(char));
  memset(**array, ' ', X*Y*Z*(sizeof(***array))); /* load the array with spaces */
  for (i = 0; i < X; ++i) {
    x = i/(float)X;
    for (j = 0; j < Y; ++j) {
      y = j/(float)X;
      z = x*y*(4*M_PI);
      z = 5.2*(0.5+(0.276765 - sin(z)*cos(z)*exp(1-z))/0.844087); /* a somewhat carefully designed silly function */
      /* printf("%g %g %g\n", x, y, z); */
      k = (int)z;
      array[BIND(k, 0, Z-1)][j][i] = '@'; /* BIND ensures a valid index  */
    }
  }
  display(array, rank, shape, p_char);
  puts("\nIt is what it is.");
  free(array);
  return EXIT_SUCCESS;
}
#endif
```



This style is supported by all 'C' compilers.

```c
#include <stdio.h>
#include <stdlib.h>
int main(int argc, char **argv)
{
   int user1 = 0, user2 = 0;
   int *a1, **array, row;

   printf("Enter two integers.  Space delimited, please:  ");
   scanf("%d %d",&user1, &user2);

   a1 = malloc(user1*user2*sizeof(int));
   array = malloc(user1*sizeof(int*));
   for (row=0; row<user1; row++) array[row]=a1+row*user2;

   array[user1/2][user2/2] = user1 + user2;
   printf("array[%d][%d] is %d\n",user1/2,user2/2,array[user1/2][user2/2]);
   free(array);
   free(a1);
   return 0;
}
```

This style also supports more efficient memory utilization if you're only using a portion of the
array.  If you only need the upper right half of a square array, you can do something like the following.

```c
#include <stdio.h>
#include <stdlib.h>
int main(int argc, char **argv)
{
   int user1 = 0;
   int space_needed;
   int *a1, **array;
   int row, col, offset;

   printf("Enter size of array:  ");
   scanf("%d",&user1);

   space_needed = (user1+1)*user1/2;
   a1 = malloc(space_needed * sizeof(*a1));
   array = malloc(user1 * sizeof(*array));
   for (row=0,offset=0; row<user1; offset+=(user1-row), row++) {
      array[row]=a1+offset-row;
      for (col=row; col<user1; col++)
          array[row][col] = 1+col-row;
   }
   for (row=0; row<user1; row++)
      printf("%d ", array[row][user1-1]);
   printf("\n");

   free(array);
   free(a1);
   return 0;
}
```


This approach most closely matches the C99 example, as '''alloca''' allocates on the [[stack]], rather than the [[heap]], as '''malloc''' does.


```c
#include <stdio.h>
#include <alloca.h>
int main(int argc, char **argv)
{
   int user1 = 0, user2 = 0;
   int *a1, **array, row;

   printf("Enter two integers.  Space delimited, please:  ");
   scanf("%d %d",&user1, &user2);

   a1 = alloca(user1*user2*sizeof(int));
   array = alloca(user1*sizeof(int*));
   for (row=0; row<user1; row++) array[row]=a1+row*user2;

   array[user1/2][user2/2] = user1 + user2;
   printf("array[%d][%d] is %d\n",user1/2,user2/2,array[user1/2][user2/2]);
   return 0;
}
```



## C++

=== With language built-in facilities ===


```cpp
#include <iostream>

int main()
{
  // read values
  int dim1, dim2;
  std::cin >> dim1 >> dim2;

  // create array
  double* array_data = new double[dim1*dim2];
  double** array = new double*[dim1];
  for (int i = 0; i < dim1; ++i)
    array[i] = array_data + dim2*i;

  // write element
  array[0][0] = 3.5;

  // output element
  std::cout << array[0][0] << std::endl;

  // get rid of array
  delete[] array;
  delete[] array_data;

  return 0;
}
```



###  Using std::vector from the standard library



```cpp
#include <iostream>
#include <vector>

int main()
{
  // read values
  int dim1, dim2;
  std::cin >> dim1 >> dim2;

  // create array
  std::vector<std::vector<double> > array(dim1, std::vector<double>(dim2));

  // write element
  array[0][0] = 3.5;

  // output element
  std::cout << array[0][0] << std::endl;

  // the array is automatically freed at the end of main()
  return 0;
}
```


=== Using boost::multi_array ===
{{libheader|Boost}}

```cpp
#include <iostream>
#include <boost/multi_array.hpp>

typedef boost::multi_array<double, 2> two_d_array_type;

int main()
{
    // read values
    int dim1, dim2;
    std::cin >> dim1 >> dim2;

    // create array
    two_d_array_type A(boost::extents[dim1][dim2]);

    // write element
    A[0][0] = 3.1415;

    // read element
    std::cout << A[0][0] << std::endl;

    return 0;
}
```



###  Using boost::uBLAS

{{libheader|Boost|from 1.29}}
{{works with|Boost|1.54}}

```cpp
#include <cstdlib>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/io.hpp>

int main (const int argc, const char** argv) {
    if (argc > 2) {
        using namespace boost::numeric::ublas;

        matrix<double> m(atoi(argv[1]), atoi(argv[2])); // build
        for (unsigned i = 0; i < m.size1(); i++)
            for (unsigned j = 0; j < m.size2(); j++)
                m(i, j) = 1.0 + i + j; // fill
        std::cout << m << std::endl; // print
        return EXIT_SUCCESS;
    }

    return EXIT_FAILURE;
}
```


=={{header|C sharp|C#}}==

```csharp

class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Enter two integers. Space delimited please: ");
            string s = Console.ReadLine();

            int[,] myArray=new int[(int)s[0],(int)s[2]];
            myArray[0, 0] = 2;
            Console.WriteLine(myArray[0, 0]);

            Console.ReadLine();
        }
    }
```



## Clean


```clean
import StdEnv

Start :: *World -> { {Real} }
Start world
    # (console, world) = stdio world
      (_, dim1, console) = freadi console
      (_, dim2, console) = freadi console
    = createArray dim1 (createArray dim2 1.0)
```




## Clojure


```clojure
(let [rows (Integer/parseInt (read-line))
      cols (Integer/parseInt (read-line))
      a (to-array-2d (repeat rows (repeat cols nil)))]
  (aset a 0 0 12)
  (println "Element at 0,0:" (aget a 0 0)))
```



## Common Lisp


```lisp
(let ((d1 (read))
      (d2 (read)))
  (assert (and (typep d1 '(integer 1))
               (typep d2 '(integer 1)))
          (d1 d2))
  (let ((array (make-array (list d1 d2) :initial-element nil))
        (p1 0)
        (p2 (floor d2 2)))
    (setf (aref array p1 p2) t)
    (print (aref array p1 p2))))
```


The <tt>[http://www.lispworks.com/documentation/HyperSpec/Body/m_assert.htm assert]</tt> will allow the user to reenter the dimensions if they are not positive integers.


## Component Pascal


Arrays in Component Pascal are started from zero index. No DISPOSE-like procedures because of garbage collection.


```oberon2

MODULE TestArray;
(* Implemented in BlackBox Component Builder *)

	IMPORT Out;

	(* Open array *)

	PROCEDURE DoTwoDim*;
		VAR d: POINTER TO ARRAY OF ARRAY OF INTEGER;
	BEGIN
		NEW(d, 5, 4); (* allocating array in memory *)
		d[1, 2] := 100; (* second row, third column element *)
		d[4, 3] := -100; (* fifth row, fourth column element *)
		Out.Int(d[1, 2], 0); Out.Ln;
		Out.Int(d[4, 3], 0); Out.Ln;
	END DoTwoDim;

END TestArray.
```



## D


```d
void main() {
    import std.stdio, std.conv, std.string;
    int nRow, nCol;

    write("Give me the numer of rows: ");
    try {
        nRow = readln.strip.to!int;
    } catch (StdioException) {
        nRow = 3;
        writeln;
    }

    write("Give me the numer of columns: ");
    try {
        nCol = readln.strip.to!int;
    } catch (StdioException) {
        nCol = 5;
        writeln;
    }

    auto array = new float[][](nRow, nCol);
    array[0][0] = 3.5;
    writeln("The number at place [0, 0] is ", array[0][0]);
}
```

{{out}}

```txt
Give me the numer of rows:
Give me the numer of columns:
The number at place [0, 0] is 3.5
```



## Delphi

Dimensions are generated randomly, not input by user.

```delphi
program Project1;

{$APPTYPE CONSOLE}

uses
  SysUtils;
var
  matrix:array of array of Byte;
  i,j:Integer;
begin
  Randomize;
  //Finalization is not required in this case, but you have to do
  //so when reusing the variable in scope
  Finalize(matrix);
  //Init first dimension with random size from 1..10
  //Remember dynamic arrays are indexed from 0
  SetLength(matrix,Random(10) + 1);
  //Init 2nd dimension with random sizes too
  for i := Low(matrix) to High(matrix) do
    SetLength(matrix[i],Random(10) + 1);

  //End of code, the following part is just output
  Writeln(Format('Total amount of columns = %.2d',[Length(matrix)]));
  for i := Low(matrix) to High(matrix) do
    Writeln(Format('Column %.2d = %.2d rows',[i,Length(matrix[i])]));

  Readln;
end.
```


Test run:

```txt

Total amount of columns = 10
Column 00 = 04 rows
Column 01 = 08 rows
Column 02 = 09 rows
Column 03 = 05 rows
Column 04 = 01 rows
Column 05 = 04 rows
Column 06 = 07 rows
Column 07 = 04 rows
Column 08 = 10 rows
Column 09 = 02 rows
```



## Elena

ELENA 4.x :

Typified array

```elena
import extensions;

public program()
{
    var n := new Integer();
    var m := new Integer();

    console.write:"Enter two space delimited integers:";
    console.loadLine(n,m);

    var myArray := new Matrix<int>(n,m);

    myArray.setAt(0,0,2);

    console.printLine(myArray.at(0, 0))
}
```

Jagged array

```elena
import system'routines;
import extensions;

public program()
{
    auto n := new Integer();
    auto m := new Integer();

    console.write:"Enter two space delimited integers:";
    console.loadLine(n,m);

    auto myArray2 := new object[][](n.Value).populate:(int i => (new object[](m.Value)) );
    myArray2[0][0] := 2;
    myArray2[1][0] := "Hello";

    console.printLine(myArray2[0][0]);
    console.printLine(myArray2[1][0]);
}
```



## Elixir


```Elixir

defmodule TwoDimArray do

  def create(w, h) do
    List.duplicate(0, w)
      |> List.duplicate(h)
  end

  def set(arr, x, y, value) do
    List.replace_at(arr, x,
      List.replace_at(Enum.at(arr, x), y, value)
    )
  end

  def get(arr, x, y) do
    arr |> Enum.at(x) |> Enum.at(y)
  end
end


width = IO.gets "Enter Array Width: "
w = width |> String.trim() |> String.to_integer()

height = IO.gets "Enter Array Height: "
h = height |> String.trim() |> String.to_integer()

arr = TwoDimArray.create(w, h)
arr = TwoDimArray.set(arr,2,0,42)

IO.puts(TwoDimArray.get(arr,2,0))

```



## Erlang


```Erlang

-module( two_dimensional_array ).

-export( [create/2, get/3, set/4, task/0] ).

create( X, Y ) -> array:new( [{size, X}, {default, array:new( [{size, Y}] )}] ).

get( X, Y, Array ) -> array:get( Y, array:get(X, Array) ).

set( X, Y, Value, Array ) ->
	Y_array = array:get( X, Array ),
	New_y_array = array:set( Y, Value, Y_array ),
	array:set( X, New_y_array, Array ).

task() ->
	{ok, [X, Y]} = io:fread( "Input two integers.  Space delimited, please:  ", "~d ~d" ),
	Array = create( X, Y ),
	New_array = set( X - 1, Y - 1, X * Y, Array ),
	io:fwrite( "In position ~p ~p we have ~p~n", [X - 1, Y - 1, get( X - 1, Y - 1, New_array)] ).

```

{{out}}

```txt

7> two_dimensional_array:task().
Input two integers.  Space delimited, please:  4 5
In position 3 4 we have 20

```


## ERRE

In ERRE language arrays created at run-time is "dynamic arrays". For this task will be enough this code:
<lang>
PROGRAM DYNAMIC

!$DYNAMIC
DIM A%[0,0]

BEGIN
  PRINT(CHR$(12);) !CLS
  INPUT("Subscripts",R%,C%)
  !$DIM A%[R%,C%]
  A%[2,3]=6
  PRINT("Value in row";2;"and col";3;"is";A%[2,3])
END PROGRAM

```

You can redimension A% using pragmas:
!$ERASE A%     with a subsequent
!$DIM A%[.,.]


## Euphoria


```euphoria
include get.e

sequence array
integer height,width,i,j

height = floor(prompt_number("Enter height: ",{}))
width = floor(prompt_number("Enter width: ",{}))

array = repeat(repeat(0,width),height)

i = floor(height/2+0.5)
j = floor(width/2+0.5)
array[i][j] = height + width

printf(1,"array[%d][%d] is %d\n", {i,j,array[i][j]})
```



## Factor

Factor doesn't provide any support for easy access of 2d arrays. But since factor's written in factor, we can just add it and it's just as good :)

```factor
USING: io kernel math.matrices math.parser prettyprint
sequences ;
IN: rosettacode.runtime2darray

: set-Mi,j ( elt {i,j} matrix -- )
[ first2 swap ] dip nth set-nth ;
: Mi,j ( {i,j} matrix -- elt )
[ first2 swap ] dip nth nth ;

: example ( -- )
readln readln [ string>number ] bi@ zero-matrix ! create the array
[ [ 42 { 0 0 } ] dip set-Mi,j ] ! set the { 0 0 } element to 42
[ [ { 0 0 } ] dip Mi,j . ] ! read the { 0 0 } element
bi ;
```



## Forth


```forth
: cell-matrix
  create ( width height "name" ) over ,  * cells allot
  does> ( x y -- addr ) dup cell+ >r  @ * + cells r> + ;

5 5 cell-matrix test

36 0 0 test !
0 0 test @ .  \ 36
```


{{libheader|Forth Scientific Library}}

```forth
INTEGER DMATRIX my-matrix{{
& my-matrix{{ 8 9 }}malloc

8 my-matrix{{ 3 4 }} !
my-matrix{{ 3 4 }} @ .

& my-matrix{{ }}free
```



## Fortran

In Fortran 90 and later

```fortran
PROGRAM Example

  IMPLICIT NONE
  INTEGER :: rows, columns, errcheck
  INTEGER, ALLOCATABLE :: array(:,:)

  WRITE(*,*) "Enter number of rows"
  READ(*,*) rows
  WRITE(*,*) "Enter number of columns"
  READ(*,*) columns

  ALLOCATE (array(rows,columns), STAT=errcheck) ! STAT is optional and is used for error checking

  array(1,1) = 42

  WRITE(*,*) array(1,1)

  DEALLOCATE (array, STAT=errcheck)

END PROGRAM Example
```



## Frink


```frink

[rows, cols] = dims = eval[input["Enter dimensions: ", ["Rows", "Columns"]]]
a = new array[dims, 0]    // Create and initialize to 0
a@(rows-1)@(cols-1) = 10
println[a@(rows-1)@(cols-1)]

```


=={{header|F Sharp|F#}}==

```fsharp
open System

let width = int( Console.ReadLine() )
let height = int( Console.ReadLine() )
let arr = Array2D.create width height 0
arr.[0,0] <- 42
printfn "%d" arr.[0,0]
```


## GAP


```gap
# Creating an array of 0
a := NullMat(2, 2);
# [ [ 0, 0 ], [ 0, 0 ] ]

# Some assignments
a[1][1] := 4;
a[1][2] := 5;
a[2][1] := 3;
a[2][2] := 4;

a
# [ [ 4, 5 ], [ 3, 4 ] ]

Determinant(a);
# 1
```



## Go

Arrays in Go are only one dimensional.  Code below show the obvious way of composing a 2d array as an array of arrays that can be indexed as a[r][c].

```go
package main

import "fmt"

func main() {
    var row, col int
    fmt.Print("enter rows cols: ")
    fmt.Scan(&row, &col)

    // allocate composed 2d array
    a := make([][]int, row)
    for i := range a {
        a[i] = make([]int, col)
    }

    // array elements initialized to 0
    fmt.Println("a[0][0] =", a[0][0])

    // assign
    a[row-1][col-1] = 7

    // retrieve
    fmt.Printf("a[%d][%d] = %d\n", row-1, col-1, a[row-1][col-1])

    // remove only reference
    a = nil
    // memory allocated earlier with make can now be garbage collected.
}
```

The technique above alocates each row separately.  This might be good if you need extremely large arrays that cannot be allocated in a single piece.  It might be bad though, for locality, as there would be no guarantee that the separate allocations would be localized in memory.  A technique that maintains locality is this,

```go
    // allocate composed 2d array
    a := make([][]int, row)
    e := make([]int, row * col)
    for i := range a {
        a[i] = e[i*col:(i+1)*col]
    }
```

Now all rows are allocated with a single allocation.  Alternatively, slice e can be used directly without going through slice a.  Element r c can be accessed simply as e[r*cols+c] for example, or accessor functions can be defined such as,

```go

func get(r, c int) int {
    return e[r*cols+c]
}
```



## Groovy

Solution:

```groovy
def make2d = { nrows, ncols ->
    (0..<nrows).collect { [0]*ncols }
}
```


Test:

```groovy
def r = new Random()

System.in.splitEachLine(/,\s*/) { dim ->
    def nrows = dim[0] as int
    def ncols = dim[1] as int

    def a2d = make2d(nrows, ncols)

    def row = r.nextInt(nrows)
    def col = r.nextInt(ncols)
    def val = r.nextInt(nrows*ncols)

    a2d[row][col] = val

    println "a2d[${row}][${col}] == ${a2d[row][col]}"

    a2d.each { println it }
    println()
}
```


Input:

```txt
  3,   5
4, 4
```


Output:

```txt
a2d[0][3] == 8
[0, 0, 0, 8, 0]
[0, 0, 0, 0, 0]
[0, 0, 0, 0, 0]

a2d[2][2] == 5
[0, 0, 0, 0]
[0, 0, 0, 0]
[0, 0, 5, 0]
[0, 0, 0, 0]
```



## Haskell



```haskell
import Data.Array

doit n m = a!(0,0) where a = array ((0,0),(n,m)) [((0,0),42)]
```



## HicEst


```hicest
REAL :: array(1)

DLG(NameEdit=rows, NameEdit=cols, Button='OK', TItle='Enter array dimensions')

ALLOCATE(array, cols, rows)
array(1,1) = 1.234
WRITE(Messagebox, Name) array(1,1)
```


=={{header|Icon}} and {{header|Unicon}}==
All Icon and Unicon data objects are automatically reclaimed.
Multiply dimensioned arrays are arrays of arrays in both languages.


```icon
procedure main(args)
    nr := integer(args[1]) | 3  # Default to 3x3
    nc := integer(args[2]) | 3

    A := list(nr)
    every !A := list(nc)

    x := ?nr    # Select a random element
    y := ?nc

    A[x][y] := &pi
    write("A[",x,"][",y,"] -> ",A[x][y])
end
```


Sample output:

```txt
->ar 65 2
A[37][1] -> 3.141592654

```



## IDL

The following is only for demonstration. No real program should just assume that the user input is valid, integer, large enough etc.


```idl
read, x, prompt='Enter x size:'
read, y, prompt='Enter y size:'
d = fltarr(x,y)

d[3,4] = 5.6
print,d[3,4]
;==> outputs  5.6

delvar, d
```



## J


The natural ways of creating a two dimensional array, from array dimensions, in J are <code>i.</code> and <code>$</code>


```j
   array1=:i. 3 4   NB. a 3 by 4 array with arbitrary values
   array2=: 5 6 $ 2 NB. a 5 by 6 array where every value is the number 2
```


To update the upper left corner of the array with the value 99, you might use <code>}</code>


```j
   array1=: 99 (<0 0)} array1
```


And, to retrieve that value you might use <code>{</code>


```j
   (<0 0) { array1
```


Finally, J manages storage for you, so to delete the array, you could either have the name refer to a new value


```j>   array1=: 0</lang


or you could remove the name itself:


```j
   erase'array1'
```


Putting these ideas together and adding a few frills:


```j
task=: verb define
  assert. y -: 0 0 + , y       NB. error except when 2 dimensions are specified
  INIT=. 0                     NB. array will be populated with this value
  NEW=. 1                      NB. we will later update one location with this value
  ARRAY=. y $ INIT             NB. here, we create our 2-dimensional array
  INDEX=. < ? $ ARRAY          NB. pick an arbitrary location within our array
  ARRAY=. NEW INDEX} ARRAY     NB. use our new value at that location
  INDEX { ARRAY                NB. and return the value from that location
)
```

Passing two integers to <tt>task</tt> (as a list) satisfies the specifications for a two-dimensional array, but providing a longer list of integers accomplishes the same task on an array of as many dimensions as the count of integers given.

Example use (result should always be 1 which is the value of <code>NEW</code>):


```J
   task 99 99
1
```


The type of the array is determined by the type of the values used in filling the array. E.g., alternate data types are obtained by substituting any of the following lines:

```j
'init new' =. ' ';'x'           NB. literals
'init new' =. 1r2;2r3           NB. fractions
'init new' =. a: ; <<'Rosetta'  NB. boxes
```



## Java



```java
import java.util.Scanner;

public class twoDimArray {
  public static void main(String[] args) {
        Scanner in = new Scanner(System.in);

        int nbr1 = in.nextInt();
        int nbr2 = in.nextInt();

        double[][] array = new double[nbr1][nbr2];
        array[0][0] = 42.0;
        System.out.println("The number at place [0 0] is " + array[0][0]);
  }
}
```



## JavaScript


```javascript
var width = Number(prompt("Enter width: "));
var height = Number(prompt("Enter height: "));

//make 2D array
var arr = new Array(height);

for (var i = 0; i < h; i++) {
  arr[i] = new Array(width);
}

//set value of element
a[0][0] = 'foo';
//print value of element
console.log('arr[0][0] = ' + arr[0][0]);

//cleanup array
arr = void(0);
```



## jq

jq data types are exactly those of JSON, so there are various alternatives for representing matrices.

One way to represent an m by n matrix is as an array of m arrays, one of which is of length n, and all of which have length less than or equal to n.

If M is such as array, then the syntax M[i][j] can be used to access the (i,j) element in the conventional sense, except that the index origin in jq is 0.

Note that the expression <tt>M | getpath([i,j])</tt> would also access M[i][j].

To set the M[i][j] element to, say, e, one can use the
idiom:

    M | setpath([i,j]; e)

Here's a simple example.

```jq
# A function to create an m x n matrix
# filled with the input element
def matrix(m;n):
  . as $init
  | ( [ range(0; n + 1) ] | map($init)) as $row
  | ( [ range(0; m + 1) ] | map($row))
 ;

# Task: create a matrix with dimensions specified by the user
# and set the [1,2] element:
(0 | matrix($m|tonumber; $n|tonumber)) | setpath([1,2]; 99)
```


If the above is in a file, say 2d.jq, the invocation:

<tt>jq -n -c --arg m 2 --arg n 3 -f 2d.jq </tt>

would produce:
```jq
[[0,0,0,0],[0,0,99,0],[0,0,0,0]]
```




## Julia


Julia supports n-dimensional arrays as native data types: `Array{T, N}`, where `T` is the type of it´s elements and `N` is the number of dimensions.


```julia
function input(prompt::AbstractString)
    print(prompt)
    return readline()
end

n = input("Upper bound for dimension 1: ") |>
    x -> parse(Int, x)
m = input("Upper bound for dimension 2: ") |>
    x -> parse(Int, x)

x = rand(n, m)
display(x)
x[3, 3]         # overloads `getindex` generic function
x[3, 3] = 5.0   # overloads `setindex!` generic function
x::Matrix # `Matrix{T}` is an alias for `Array{T, 2}`
x = 0; gc() # Julia has no `del` command, rebind `x` and call the garbage collector
```


Manually calling the garbage collector may or may not actually collect the array, but it will be eventually.


## Kotlin

Program arguments provide dimensions of the array (4 5 in the example).

```scala
fun main(args: Array<String>) {
    // build
    val dim = arrayOf(10, 15)
    val array = Array(dim[0], { IntArray(dim[1]) } )

    // fill
    array.forEachIndexed { i, it ->
        it.indices.forEach { j ->
            it[j] = 1 + i + j
        }
    }

    // print
    array.forEach { println(it.asList()) }
}
```

{{out}}

```txt
[1, 2, 3, 4, 5]
[2, 3, 4, 5, 6]
[3, 4, 5, 6, 7]
[4, 5, 6, 7, 8]
```



## Logo

{{works with|UCB Logo}}

```logo
make "a2 mdarray [5 5]
mdsetitem [1 1] :a2 0     ; by default, arrays are indexed starting at 1
print mditem [1 1] :a2    ; 0
```



## Lua


```lua
function multiply(n, a, b) if a <= b then return n, multiply(n, a + 1, b) end end

a, b = io.read() + 0, io.read() + 0
matrix = {multiply({multiply(1, 1, b)}, 1, a)}
matrix[a][b] = 5
print(matrix[a][b])
print(matrix[1][1])
```



## M2000 Interpreter


```M2000 Interpreter

Module CheckArray {
      Do {
            Input "A, B=", A% ,B%
      } Until A%>0 and B%>0

      \\ 1@ is 1 Decimal
      addone=lambda N=1@ ->{=N : N++}
      Dim Base 1, Arr(A%,B%)<<addone()
      \\ pi also is decimal
      Arr(1,1)=pi
      Print Arr(1,1)
      Print Arr()
      \\ all variables/arrays/inner functions/modules erased now
}
CheckArray

```



## Maple

This hardly covers the richness and complexity of arrays in Maple, but here goes:

```Maple>
 a := Array( 1 .. 3, 1 .. 4 ): # initialised to 0s
> a[1,1] := 1: # assign an element
> a[2,3] := 4: # assign an element
> a; # display the array
                           [1    0    0    0]
                           [                ]
                           [0    0    4    0]
                           [                ]
                           [0    0    0    0]

> a := 'a': # unassign the name
> gc(); # force a garbage collection; may or may not actually collect the array, but it will be eventually
```



## Mathematica


```Mathematica

arrayFun[m_Integer,n_Integer]:=Module[{array=ConstantArray[0,{m,n}]},
   array[[1,1]]=RandomReal[];
   array[[1,1]]
]
```



=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
width = input('Array Width: ');
height = input('Array Height: ');

array  = zeros(width,height);

array(1,1) = 12;

disp(['Array element (1,1) = ' num2str(array(1,1))]);

clear array;  % de-allocate (remove) array from workspace

```



Sample Output:

```MATLAB
Array Width: 18
Array Height: 12
Array element (1,1) = 12
```



## Maxima


```Maxima
printf(true, "in the following terminate every number with semicolon `;'")$
n: readonly("Input x-size: ")$
m: readonly("Input y-size: ")$
a: make_array(fixnum, n, m)$
fillarray(a, makelist(i, i, 1, m*n))$

/* indexing starts from 0 */
print(a[0,0]);
print(a[n-1,m-1]);
```



## MAXScript


```maxscript
a = getKBValue prompt:"Enter first dimension:"
b = getKBValue prompt:"Enter second dimension:"
arr1 = #()
arr2 = #()
arr2[b] = undefined
for i in 1 to a do
(
     append arr1 (deepCopy arr2)
)
arr1[a][b] = 1
print arr1[a][b]
```



## MUMPS

This example uses a two level tree to mimic an 2D array.

```MUMPS

ARA2D
 NEW X,Y,A,I,J
REARA
 WRITE !,"Please enter two positive integers"
 READ:10 !,"First: ",X
 READ:10 !,"Second: ",Y
 GOTO:(X\1'=X)!(X<0)!(Y\1'=Y)!(Y<0) REARA
 FOR I=1:1:X FOR J=1:1:Y SET A(I,J)=I+J
 WRITE !,"The corner of X and Y is ",A(X,Y)
 KILL X,Y,A,I,J
 QUIT

```



## NetRexx

'''Note:''' No attempt is made to validate the input.
Any errors will be handled as exceptions by the underlying JVM.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

say "give me the X and Y dimensions as two positive integers:"
parse ask xDim yDim
xPos = xDim % 2 -- integer divide to get close to the middle of the array
yPos = yDim % 2

arry = Rexx[xDim, yDim]
arry[xPos, yPos] = xDim / yDim -- make up a value...
say "arry["xPos","yPos"]:" arry[xPos, yPos]
return

```

'''Output:'''

```txt

give me the X and Y dimensions as two positive integers:
1250 1777
arry[625,888]: 0.703432752

```



## Nim


```nim
import strutils, rdstdin

var
  w = readLineFromStdin("Width: ").parseInt()
  h = readLineFromStdin("Height: ").parseInt()
  s = newSeq[seq[int]](h)

for i in 0 .. < h:
  s[i].newSeq(w)
```



## Objeck


```objeck

use IO;

bundle Default {
  class TwoDee {
    function : Main(args : System.String[]) ~ Nil {
      DoIt();
    }

    function : native : DoIt() ~ Nil {
      Console->GetInstance()->Print("Enter x: ");
      x := Console->GetInstance()->ReadString()->ToInt();

      Console->GetInstance()->Print("Enter y: ");
      y := Console->GetInstance()->ReadString()->ToInt();

      if(x > 0 & y > 0) {
        array : Int[,] := Int->New[x, y];
        array[0, 0] := 2;
        array[0, 0]->PrintLine();
      };
    }
  }
}

```


=={{header|Objective-C}}==

Being Objective-C derivated from C, the [[Two-dimensional array (runtime)#C|C solution]] works fine in Objective-C too.

The "OpenStep" frameworks (GNUstep, Cocoa) does not provide a class for multidimensional array; of course it can be implemented in several way (also as a ''wrapper'' for the plain C way of handling arrays). Here I show a straightforward use of the NSMutableArray class.

{{works with|GNUstep}}
{{works with|Cocoa}}

```objc>#import <Foundation/Foundation.h


int main()
{
  @autoreleasepool {
    int num1, num2;
    scanf("%d %d", &num1, &num2);

    NSLog(@"%d %d", num1, num2);

    NSMutableArray *arr = [NSMutableArray arrayWithCapacity: (num1*num2)];
    // initialize it with 0s
    for(int i=0; i < (num1*num2); i++) [arr addObject: @0];

    // replace 0s with something more interesting
    for(int i=0; i < num1; i++) {
      for(int j=0; j < num2; j++) {
        arr[i*num2+j] = @(i*j);
      }
    }

    // access a value: i*num2+j, where i,j are the indexes for the bidimensional array
    NSLog(@"%@", arr[1*num2+3]);
  }
  return 0;
}
```



## OCaml



```ocaml
let nbr1 = read_int ();;
let nbr2 = read_int ();;
let array = Array.make_matrix nbr1 nbr2 0.0;;
array.(0).(0) <- 3.5;;
print_float array.(0).(0); print_newline ();;
```


or using the module [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Bigarray.html Bigarray]:


```ocaml
let nbr1 = read_int ();;
let nbr2 = read_int ();;
let arr = Bigarray.Array2.create Bigarray.float32 Bigarray.c_layout nbr1 nbr2 ;;
arr.{0,0} <- 3.5;;
print_float arr.{0,0}; print_newline ();;
```



## ooRexx

ooRexx arrays can be created with up to 999,999,999 dimensions...assuming you have enough memory to do so.
Actually it's the 'size' of the array that's limited (the product of the dimensions).

```ooRexx
Say "enter first dimension"
pull d1
say "enter the second dimension"
pull d2
a = .array~new(d1, d2)
a[1, 1] = "Abc"
say a[1, 1]
say d1 d2 a[d1,d2]
say a[10,10]
max=1000000000
b = .array~new(max,max)
```

{{out}}

```txt
D:\>rexx 2d
enter first dimension
3
enter the second dimension
5
Abc
3 5 The NIL object
The NIL object
       *-* Compiled method NEW with scope "Array"
    11 *-* b = .array~new(max,max)
Error 93 running D:\2d.rex line 11:  Incorrect call to method
Error 93.959:  An array cannot contain more than 99,999,999 elements
```



## Oz

Oz does not have multi-dimensional arrays. But we can create an array of arrays (similarly to most examples on this page):

```oz
declare
  %% Read width and height from stdin
  class TextFile from Open.file Open.text end
  StdIn = {New TextFile init(name:stdin)}
  Width = {String.toInt {StdIn getS($)}}
  Height = {String.toInt {StdIn getS($)}}
  %% create array
  Arr = {Array.new 1 Width unit}
in
  for X in 1..Width do
     Arr.X := {Array.new 1 Height 0}
  end
  %% set and read element
  Arr.1.1 := 42
  {Show Arr.1.1}
```



## PARI/GP


```parigp
tmp(m,n)={
  my(M=matrix(m,n,i,j,0));
  M[1,1]=1;
  M[1,1]
};
```



## Pascal

{{works with|GNU Pascal|20060325, based on gcc-3.4.4}}

The following code is standard Extended Pascal (tested with <tt>gpc --extended-pascal</tt>):


```pascal
program array2d(input, output);

type
 tArray2d(dim1, dim2: integer) = array[1 .. dim1, 1 .. dim2] of real;
 pArray2D = ^tArray2D;

var
 d1, d2: integer;
 data: pArray2D;

begin
 { read values }
 readln(d1, d2);

 { create array }
 new(data, d1, d2);

 { write element }
 data^[1,1] := 3.5;

 { output element }
 writeln(data^[1,1]);

 { get rid of array }
 dispose(data);
end.
```



## Perl

{{works with|Perl|5.x}}

Predefining an array (or multi-dimension array) size is unnecessary, Perl dynamically resizes the array to meet the requirements. Of course I'm assuming that the user is entering array size 0 based.


```perl
sub make_array($ $){
  # get array sizes from provided params, but force numeric value
  my $x = ($_[0] =~ /^\d+$/) ? shift : 0;
  my $y = ($_[0] =~ /^\d+$/) ? shift : 0;

  # define array, then add multi-dimensional elements
  my @array;
  $array[0][0] = 'X '; # first by first element
  $array[5][7] = 'X ' if (5 <= $y and 7 <= $x); # sixth by eighth element, if the max size is big enough
  $array[12][15] = 'X ' if (12 <= $y and 15 <= $x); # thirteenth by sixteenth element, if the max size is big enough

  # loop through the elements expected to exist base on input, and display the elements contents in a grid
  foreach my $dy (0 .. $y){
    foreach my $dx (0 .. $x){
      (defined $array[$dy][$dx]) ? (print $array[$dy][$dx]) : (print '. ');
    }
    print "\n";
  }
}
```


The above is a bit verbose, here is a simpler implementation:


```perl
sub array {
    my ($x, $y) = @_;
    map {[ (0) x $x ]} 1 .. $y
}

my @square = array 3, 3;

# everything above this line is mostly redundant in perl,
# since perl would have created the array automatically when used.
# however, the above function initializes the array elements to 0,
# while perl would have used undef
#
#  $cube[3][4][5] = 60  # this is valid even if @cube was previously undefined

$square[1][1] = 1;
print "@$_\n" for @square;
> 0 0 0
> 0 1 0
> 0 0 0
```



## Perl 6

{{works with|rakudo|2015-11-28}}
Line 1: The input parse doesn't care how you separate the dimensions as long as there are two distinct numbers.

Line 2: The list replication operator <tt>xx</tt> will automatically thunkify its left side so this produces new subarrays for each replication.

Line 3: Subscripting with a closure automatically passes the size of the dimension to the closure, so we pick an appropriate random index on each level.

Line 4: Print each line of the array.

```perl6
my ($major,$minor) = prompt("Dimensions? ").comb(/\d+/);
my @array = [ '@' xx $minor ] xx $major;
@array[ *.rand ][ *.rand ] = ' ';
.say for @array;
```


Typical run:
<lang>Dimensions? 5x35
[@ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @]
[@ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @]
[@ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @]
[@ @ @ @ @ @ @ @ @ @   @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @]
[@ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @]

```


The most recent versions of Rakudo have preliminary support for 'shaped arrays'. Natively shaped arrays are a flexible feature for declaring typed, potentially multi-dimensional arrays, potentially with pre-defined
dimensions. They will make memory-efficient matrix storage and matrix operations possible.


```perl6
my ($major,$minor) = +«prompt("Dimensions? ").comb(/\d+/);
my Int @array[$major;$minor] = (7 xx $minor ) xx $major;
@array[$major div 2;$minor div 2] = 42;
say @array;
```


Typical run:
<lang>Dimensions? 3 x 10
[[7 7 7 7 7 7 7 7 7 7] [7 7 7 7 7 42 7 7 7 7] [7 7 7 7 7 7 7 7 7 7]]
```



## Phix

Copy of [[Create_a_two-dimensional_array_at_runtime#Euphoria|Euphoria]]

```Phix
sequence array
integer height,width,i,j

height = floor(prompt_number("Enter height: "))
width = floor(prompt_number("Enter width: "))

array = repeat(repeat(0,width),height)

i = floor(height/2+0.5)
j = floor(width/2+0.5)
array[i][j] = height + width

printf(1,"array[%d][%d] is %d\n", {i,j,array[i][j]})
```



## PicoLisp


```PicoLisp
(de 2dimTest (DX DY)
   (let A (make (do DX (link (need DY))))
      (set (nth A 3 3) 999)            # Set A[3][3] to 999
      (mapc println A)                 # Print all
      (get A 3 3) ) )                  # Return A[3][3]

(2dimTest 5 5)
```

Output:

```txt
(NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL)
(NIL NIL 999 NIL NIL)
(NIL NIL NIL NIL NIL)
(NIL NIL NIL NIL NIL)
-> 999
```



## PL/I

<lang>
/* First way using a controlled variable: */

declare A(*,*) float controlled;
get list (m, n);
allocate A(m,n);
get list (A);
put skip list (A);

/* The array remains allocated until the program terminates, */
/* or until explicitly destroyed using a FREE statement.     */

free A;

```



```PL/I
 6.00000E+0000           5.00000E+0000           4.00000E+0000           3.00000E+0000           2.00000E+0000
 1.00000E+0000

```



```PL/I

/* Second way using a BEGIN block: */

get list (m, n);
begin;
   declare A(m, n) float;
   get list (A);
   put skip list (A);
end;

/* The array is automatically destroyed when the block terminates. */

```



```PL/I
 1.00000E+0000           2.00000E+0000           3.00000E+0000           4.00000E+0000           5.00000E+0000
 6.00000E+0000           7.00000E+0000           8.00000E+0000           9.00000E+0000           1.00000E+0001
 1.10000E+0001           1.20000E+0002

```



```PL/I

/* Third way using a PROCEDURE block: */

get list (m, n);
call S (m, n);
S: procedure (m, n);
   declare A(m, n) float;
   get list (A);
   put skip list (A);
end S;

/* The array is automatically destroyed when the procedure terminates. */

```



```PL/I

 1.00000E+0000           2.00000E+0000           3.00000E+0000           4.00000E+0000           5.00000E+0000
 6.00000E+0000           7.00000E+0000           8.00000E+0000           9.00000E+0000           1.00000E+0001
 1.10000E+0001           1.20000E+0001           1.30000E+0001           1.40000E+0001           1.50000E+0001
 1.60000E+0001           1.70000E+0001           1.80000E+0001           1.90000E+0001           2.00000E+0001

```



## Pop11


```pop11
vars itemrep;
incharitem(charin) -> itemrep;
;;; Read sizes
vars n1 = itemrep(), n2= itemrep();
;;; Create 0 based array
vars ar = newarray([0 ^(n1 - 1) 0 ^(n2 - 1)], 0);
;;; Set element value
15 -> ar(0, 0);
;;; Print element value
ar(0,0) =>
;;; Make sure array is unreferenced
0 -> ar;
```


Pop11 is garbage collected so there is no need to destroy array. However, the array is live as long as variable ar references it. The last assignment makes sure that we loose all our references to the array turning it into garbage.

Pop11 arrays may have arbitrary lower bounds, since we are given only size we create 0 based array.


## PowerShell


```PowerShell

function Read-ArrayIndex ([string]$Prompt = "Enter an integer greater than zero")
{
    [int]$inputAsInteger = 0

    while (-not [Int]::TryParse(([string]$inputString = Read-Host $Prompt), [ref]$inputAsInteger))
    {
        $inputString = Read-Host "Enter an integer greater than zero"
    }

    if ($inputAsInteger -gt 0) {return $inputAsInteger} else {return 1}
}

$x = $y = $null

do
{
    if ($x -eq $null) {$x = Read-ArrayIndex -Prompt "Enter two dimensional array index X"}
    if ($y -eq $null) {$y = Read-ArrayIndex -Prompt "Enter two dimensional array index Y"}
}
until (($x -ne $null) -and ($y -ne $null))

$array2d = New-Object -TypeName 'System.Object[,]' -ArgumentList $x, $y

```

{{Out}}

```txt

Enter two dimensional array index X: 6
Enter two dimensional array index Y: 6

```

Populate the array:

```PowerShell

[int]$k = 1

for ($i = 0; $i -lt 6; $i++)
{
    0..5 | ForEach-Object -Begin {$k += 10} -Process {$array2d[$i,$_] = $k + $_}
}

```

This is the entire array:

```PowerShell

for ($i = 0; $i -lt 6; $i++)
{
    "{0}`t{1}`t{2}`t{3}`t{4}`t{5}" -f (0..5 | ForEach-Object {$array2d[$i,$_]})
}

```

{{Out}}

```txt

11	12	13	14	15	16
21	22	23	24	25	26
31	32	33	34	35	36
41	42	43	44	45	46
51	52	53	54	55	56
61	62	63	64	65	66

```

Get an element of the array:

```PowerShell

$array2d[2,2]

```

{{Out}}

```txt

33

```



## Python

{{works with|Python| 2.5}}


```python
width = int(raw_input("Width of myarray: "))
height = int(raw_input("Height of Array: "))
myarray = [[0] * width for i in xrange(height)]
myarray[0][0] = 3.5
print myarray[0][0]
```


'''Note:''' Some people may instinctively try to write myarray as [[0] * width] * height, but the * operator creates ''n'' references to [[0] * width]

You can also use a two element tuple to index a dictionary like so:


```python
myarray = {(w,h): 0 for w in range(width) for h in range(height)}
# or, in pre 2.7 versions of Python: myarray = dict(((w,h), 0) for w in range(width) for h in range(height))
myarray[(0,0)] = 3.5
print myarray[(0,0)]
```



## R

{{trans|C}}

```r
input <- readline("Enter two integers.  Space delimited, please:  ")
dims <- as.numeric(strsplit(input, " ")[[1]])
arr <- array(dim=dims)
ii <- ceiling(dims[1]/2)
jj <- ceiling(dims[2]/2)
arr[ii, jj] <- sum(dims)
cat("array[", ii, ",", jj, "] is ", arr[ii, jj], "\n", sep="")
```



## Racket


Using a vector of vectors to represent arrays:


```Racket

#lang racket

(printf "Enter XY dimensions: ")
(define xy (cons (read) (read)))
(define array (for/vector ([x (car xy)]) (for/vector ([y (cdr xy)]) 0)))

(printf "Enter a number for the top-left: ")
(vector-set! (vector-ref array 0) 0 (read))
(printf "Enter a number for the bottom-right: ")
(vector-set! (vector-ref array (sub1 (car xy))) (sub1 (cdr xy)) (read))

array

```


Output:

```txt

Enter XY dimensions: 3 3
Enter a number for the top-left: 1
Enter a number for the bottom-right: 9
'#(#(1 0 0) #(0 0 0) #(0 0 9))

```



## REXX


```rexx
/*REXX program  allocates/populates/displays  a two-dimensional array.  */
call bloat                   /*the BLOAT procedure does all allocations.*/
                             /*no more array named   @   at this point. */
exit                         /*stick a fork in it, we're all done honey.*/
/*─────────────────────────BLOAT subroutine─────────────────────────────*/
bloat: procedure;  say       /*"PROCEDURE"  makes this a ··· procedure. */
say 'Enter two positive integers (a 2-dimensional array will be created).'
pull n m .                   /*elements are allocated as they're defined*/
                             /*N and M should be verified at this point.*/
@.=' · '                     /*Initial value for all  @  array elements,*/
                             /*this ensures  every  element has a value.*/
  do j    =1  for n          /*traipse through the first  dimension  [N]*/
      do k=1  for m          /*   "       "     "  second     "      [M]*/
      if random()//7==0  then @.j.k=j'~'k    /*populate every 7th random*/
      end  /*k*/
  end        /*j*/
                             /* [↓]  display array to console:  row,col */
  do r=1  for n;    _=       /*construct one row (or line) at a time.   */
      do c=1  for m          /*construct row one column at a time.      */
      _=_ right(@.r.c,4)     /*append a nice-aligned column to the line.*/
      end   /*kk*/           /* [↑]   an nicely aligned line is built.  */
  say _                      /*display one row at a time to the terminal*/
  end         /*jj*/
/*╔════════════════════════════════════════════════════════════════════╗
  ║ When the  RETURN  is executed (from a PROCEDURE in this case), and ║
  ║ array   @  is "de─allocated", that is, it's no longer defined, and ║
  ║ the array's storage is now free for other REXX variables.   If the ║
  ║ BLOAT   subroutine didn't have a   "PROCEDURE"   on that statement,║
  ║ the array    @    would've been left intact.    The same effect is ║
  ║ performed by a   DROP   statement   (an example is shown below).   ║
  ╚════════════════════════════════════════════════════════════════════╝*/
drop @.                      /*because of the  PROCEDURE  statement, the*/
return                       /* [↑]    DROP   statement is superfluous. */
```

'''output''' when the following input is entered after the prompt message:   <tt> 30 15 </tt>

```txt

   ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·  1~12   ·    ·    ·
   ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·  2~13   ·    ·
   ·   3~2   ·    ·    ·    ·    ·    ·    ·    ·  3~11   ·    ·    ·  3~15
   ·    ·    ·    ·    ·   4~6   ·    ·    ·    ·    ·    ·    ·    ·    ·
   ·   5~2   ·    ·    ·    ·    ·    ·   5~9   ·    ·    ·    ·    ·    ·
   ·    ·    ·   6~4   ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
   ·   7~2  7~3   ·    ·    ·    ·    ·    ·    ·  7~11   ·    ·  7~14   ·
   ·    ·   8~3   ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
   ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
   ·    ·    ·    ·    ·    ·    ·  10~8   ·    ·    ·  0~12   ·    ·    ·
   ·    ·    ·    ·    ·    ·  11~7   ·    ·    ·    ·    ·    ·    ·    ·
 12~1   ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·  2~15
   ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·  3~13   ·    ·
 14~1   ·    ·    ·    ·  14~6   ·    ·    ·    ·    ·    ·    ·  4~14   ·
 15~1 15~2   ·    ·    ·    ·    ·    ·    ·    ·    ·  5~12   ·    ·    ·
 16~1   ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
   ·    ·    ·    ·    ·    ·    ·    ·    ·  7~10   ·  7~12   ·  7~14   ·
   ·    ·    ·    ·    ·    ·    ·    ·    ·    ·  8~11   ·    ·    ·    ·
   ·    ·    ·    ·    ·  19~6   ·    ·    ·    ·  9~11   ·    ·    ·    ·
 20~1   ·    ·    ·    ·    ·    ·  20~8   ·    ·  0~11   ·    ·    ·    ·
   ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
   ·    ·    ·    ·    ·  22~6   ·    ·    ·    ·    ·    ·    ·  2~14   ·
   ·    ·    ·    ·    ·    ·    ·  23~8   ·    ·    ·    ·    ·    ·    ·
   ·    ·    ·    ·  24~5 24~6   ·    ·    ·    ·    ·    ·  4~13   ·    ·
   ·    ·    ·    ·    ·    ·    ·    ·    ·  5~10   ·    ·    ·    ·    ·
   ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
 27~1   ·  27~3   ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·  7~15
   ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·  8~15
   ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·    ·
   ·    ·    ·    ·    ·  30~6   ·  30~8 30~9   ·    ·    ·    ·    ·    ·

```



## Ring


```ring

See 'Enter width : ' give width
See 'Enter height : ' give height
width=0+width   height=0+height
aList = list(height) for x in aList  x = list(width) next
aList[1][2] = 10  See aList[1][2] + nl

```



## Ruby


```ruby
puts 'Enter width and height: '
w=gets.to_i
arr = Array.new(gets.to_i){Array.new(w)}
arr[1][3] = 5
p arr[1][3]
```




## Rust


```rust
use std::env;

fn main() {
    let mut args = env::args().skip(1).flat_map(|num| num.parse());
    let rows = args.next().expect("Expected number of rows as first argument");
    let cols = args.next().expect("Expected number of columns as second argument");

    assert_ne!(rows, 0, "rows were zero");
    assert_ne!(cols, 0, "cols were zero");

    // Creates a vector of vectors with all elements initialized to 0.
    let mut v = vec![vec![0; cols]; rows];
    v[0][0] = 1;
    println!("{}", v[0][0]);
}
```



## Scala


```Scala
object Array2D{
   def main(args: Array[String]): Unit = {
      val x = Console.readInt
      val y = Console.readInt

      val a=Array.fill(x, y)(0)
      a(0)(0)=42
      println("The number at (0, 0) is "+a(0)(0))
   }
}
```



## Scheme



###  Pure R7RS


There is no two-dimensional array-type built in to Scheme, but there is a vector.


```scheme

(import (scheme base)
        (scheme read)
        (scheme write))

;; Read x/y from user
(define x (begin (display "X: ") (flush-output-port) (read)))
(define y (begin (display "Y: ") (flush-output-port) (read)))

;; Create a vector, and fill it with a vector for each row
(define arr (make-vector x))
(do ((i 0 (+ 1 i)))
  ((= i x) )
  (vector-set! arr i (make-vector y 0)))

;; set element (x/2, y/2) to 3
(vector-set! (vector-ref arr (floor (/ x 2)))
             (floor (/ y 2))
             3)

(display arr) (newline)
(display "Retrieved: ")
(display (vector-ref (vector-ref arr (floor (/ x 2)))
                     (floor (/ y 2))))
(newline)

```


{{out}}

```txt

X: 3
Y: 5
#(#(0 0 0 0 0) #(0 0 3 0 0) #(0 0 0 0 0))
Retrieved: 3

```



###  Using a standard library


{{libheader|Scheme/SRFIs}}

There are SRFI libraries providing arrays.  This example uses SRFI 63.


```scheme

(import (except (scheme base) equal?)
        (scheme read)
        (scheme write)
        (srfi 63)       ; an array SRFI
        )

;; Read x/y from user
(define x (begin (display "X: ") (flush-output-port) (read)))
(define y (begin (display "Y: ") (flush-output-port) (read)))

;; Create an array
(define array (make-array #(0) x y))

;; Write to middle element of the array
(array-set! array 3 (floor (/ x 2)) (floor (/ y 2)))

;; Retrieve and display result
(display (array-ref array (floor (/ x 2)) (floor (/ y 2)))) (newline)

```


{{out}}

```txt

X: 3
Y: 5
3

```


The array will be destroyed by the garbage collector, when it is no longer needed.


## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: numRows is 0;
    var integer: numCols is 0;
    var array array integer: anArray is 0 times 0 times 0;
  begin
    write("Give me the numer of rows: ");
    readln(numRows);
    write("Give me the numer of columns: ");
    readln(numCols);
    anArray := numRows times numCols times 0;
    anArray[1][1] := 3;
    writeln("The number at place [1, 1] is " <& anArray[1][1]);
  end func;
```


Output:

```txt

Give me the numer of rows: 5
Give me the numer of columns: 7
The number at place [1, 1] is 3

```



## Sidef


```ruby
func make_matrix(x, y) {
    y.of { x.of(0) };
}

var y = Sys.scanln("rows: ").to_i;
var x = Sys.scanln("cols: ").to_i;

var matrix = make_matrix(x, y);   # create the matrix
matrix[y/2][x/2] = 1;             # write something inside it
say matrix;                       # display the matrix
```

{{out}}

```txt

rows: 3
cols: 4
[[0, 0, 0, 0], [0, 0, 1, 0], [0, 0, 0, 0]]

```



## Smalltalk

{{works with|Pharo}}

```smalltalk

m := (FillInTheBlankMorph request: 'Number of rows?') asNumber.
n := (FillInTheBlankMorph request: 'Number of columns?') asNumber.
aMatrix := Matrix rows: m columns: n.
aMatrix
	at: (aMatrix rowCount // 2)
	at: (aMatrix columnCount // 2)
	put: 3.4.
e := aMatrix
	at: (aMatrix rowCount // 2)
	at: (aMatrix columnCount // 2).
Transcript show: 'Entry is', e printString.

```

{{works with|GNU Smalltalk}}

Smalltalk has no problems in creating objects at runtime. I haven't found a class for multidimensional array in the standard library, so let us suppose to have a class named MultidimensionalArray.


```smalltalk
|num1 num2 arr|
num1 := stdin nextLine asInteger.
num2 := stdin nextLine asInteger.

arr := MultidimensionalArray new: { num1. num2 }.

1 to: num1 do: [ :i |
  1 to: num2 do: [ :j |
    arr at: { i. j } put: (i*j)
  ]
].

1 to: num1 do: [ :i |
  1 to: num2 do: [ :j |
    (arr at: {i. j}) displayNl
  ]
].
```


A possible implementation for a '''Bi'''dimensionalArray class is the following (changing ''Multi'' into ''Bi'' and using this class, the previous code runs fine):


```smalltalk
Object subclass: BidimensionalArray [
  |biArr|
  <comment: 'bidim array'>
].
BidimensionalArray class extend [
  new: biDim [ |r|
    r := super new.
    r init: biDim.
    ^ r
  ]
].
BidimensionalArray extend [
  init: biDim [
     biArr := Array new: (biDim at: 1).
     1 to: (biDim at: 1) do: [ :i |
       biArr at: i put: (Array new: (biDim at: 2))
     ].
     ^ self
  ]
  at: biDim [
     ^ (biArr at: (biDim at: 1)) at: (biDim at: 2)
  ]
  at: biDim put: val [
     ^ (biArr at: (biDim at: 1)) at: (biDim at: 2) put: val
  ]
].
```


Instead of implementing such a class (or the MultidimensionalArray one), we can use a LookupTable class, using Array objects as keys (each element of the array will be an index for a specific ''dimension'' of the "array"). The final effect is the same as using an array (almost in the ''AWK sense'') and the approach has some advantages.


```smalltalk
|num1 num2 pseudoArr|
num1 := stdin nextLine asInteger.
num2 := stdin nextLine asInteger.

"we can 'suggest' an initial value for the number
 of ''slot'' the table can hold; anyway, if we use
 more than these, the table automatically grows"
pseudoArr := LookupTable new: (num1 * num2).

1 to: num1 do: [ :i |
  1 to: num2 do: [ :j |
     pseudoArr at: {i. j} put: (i * j).
  ]
].

1 to: num1 do: [ :i |
  1 to: num2 do: [ :j |
     (pseudoArr at: {i. j}) displayNl.
  ]
].
```



## SNOBOL4


{{works with|Macro Spitbol}}
{{works with|Snobol4+}}
{{works with|CSnobol}}

Note: trim(input) is needed for Snobol4+.


```SNOBOL4
*       # Get user X,Y dimensions
        output = 'Enter X,Y:'; xy = trim(input)
        xy break(',') . x ',' rem . y

*       # Define and create array, 1-based
        arr = array(x ',' y) ;* Or arr = array(xy)

*       # Display array prototype
        output = 'Prototype: ' prototype(arr)

*       # Assign elements, angle or square brackets
*       # Same array can hold ints, strings, etc.
        arr<x,y> = 99; arr[1,1] = 'dog'

*       # Display elements
        output = 'arr[' xy '] = ' arr[x,y]
        output = 'arr[1,1] = ' arr[1,1]

*       # Release array for garbage collection
        arr =
end
```


Output:

```txt
Enter X,Y:
5,5
Prototype: 5,5
arr[5,5] = 99
arr[1,1] = dog
```



## Standard ML


```sml
val nbr1 = valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);
val nbr2 = valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);
val array = Array2.array (nbr1, nbr2, 0.0);
Array2.update (array, 0, 0, 3.5);
print (Real.toString (Array2.sub (array, 0, 0)) ^ "\n");
```


## Stata


```stata
display "Number of rows?" _request(nr)
display "Number of columns?" _request(nc)
matrix define a=J($nr,$nc,0)
matrix a[1,2]=1.5
matrix list a
matrix drop a
```


This is also possible in Mata, except that console input is still done from Stata:


```stata
mata
mata stata display "Number of rows?" _request(nr)
mata stata display "Number of columns?" _request(nc)
nr = strtoreal(st_global("nr"))
nc = strtoreal(st_global("nc"))
a = J(nr,nc,0)
a[1,2] = 1.5
a
mata drop a
end
```



## Swift

<lang>import Foundation

print("Enter the dimensions of the array seperated by a space (width height): ")

let fileHandle = NSFileHandle.fileHandleWithStandardInput()
let dims = NSString(data: fileHandle.availableData, encoding: NSUTF8StringEncoding)?.componentsSeparatedByString(" ")

if let dims = dims where dims.count == 2{
	let w = dims[0].integerValue
	let h = dims[1].integerValue

	if let w = w, h = h where w > 0 && h > 0 {
		var array = Array<[Int!]>(count: h, repeatedValue: Array<Int!>(count: w, repeatedValue: nil))

		array[0][0] = 2
		println(array[0][0])
		println(array)
	}
}
```

{{out}}

```txt

Enter the dimensions of the array seperated by a space (width height): 3 4
2
[[2, nil, nil], [nil, nil, nil], [nil, nil, nil], [nil, nil, nil]]

```



## Tcl

{{works with|Tcl|8.5}}

```tcl

puts "Enter width:"
set width [gets stdin]
puts "Enter height:"
set height [gets stdin]
# Initialize array
for {set i 0} {$i < $width} {incr i} {
	for {set j 0} {$j < $height} {incr j} {
		set arr($i,$j) ""
	}
}
# Store value
set arr(0,0) "abc"
# Print value
puts "Element (0/0): $arr(0,0)"
# Cleanup array
unset arr

```





## Toka

Toka has no direct support for 2D arrays, but they can be created and operated on in a manner similar to normal arrays using the following functions.


```toka
[ ( x y -- address )
  cells malloc >r
  dup cells >r
  [ r> r> r> 2dup >r >r swap malloc swap i swap array.put >r ] iterate
r> r> nip
] is 2D-array

[ ( a b address -- value )
  array.get array.get
] is 2D-get-element

[ ( value a b address -- )
  array.get array.put
] is 2D-put-element
```


And a short test:


```toka
5 5 2D-array >r             #! Create an array and save the pointer to it
10 2 3 r@ 2D-put-element    #! Set element 2,3 to 10
2 3 r@ 2D-get-element       #! Get the element at 2,3
r> drop                     #! Discard the pointer to the array
```



## Ursa


```ursa
decl int width height
out "width:  " console
set width (in int console)
out "height: " console
set height (in int console)

decl int<><> twodstream
for (decl int i) (< i height) (inc i)
        append (new int<>) twodstream
end for
for (set i 0) (< i height) (inc i)
        decl int j
        for (set j 0) (< j width) (inc j)
                append 0 twodstream<i>
        end for
end for

set twodstream<0><0> 5
out twodstream<0><0> endl console
```



## VBA



```vb

Option Explicit

Sub Main_Create_Array()
Dim NbColumns As Integer, NbRows As Integer

    'Get two integers from the user,
    Do
        NbColumns = Application.InputBox("Enter number of columns : ", "Numeric only", 3, Type:=1)
        NbRows = Application.InputBox("Enter number of rows : ", "Numeric only", 5, Type:=1)
    Loop While NbColumns = 0 Or NbRows = 0
    'Create a two-dimensional array at runtime
    ReDim myArray(1 To NbRows, 1 To NbColumns)
    'Write some element of that array,
    myArray(LBound(myArray, 1), UBound(myArray, 2)) = "Toto"
    'and then output that element.
    MsgBox myArray(LBound(myArray, 1), UBound(myArray, 2))
    'destroy the array
    Erase myArray
End Sub

```



## Vim Script


```vim
" Create a two-dimensional array with r rows and c columns.
" The optional third argument specifies the initial value
" (default is 0).
function MakeArray(r, c, ...)
    if a:0
        let init = a:1
    else
        let init = 0
    endif

    let temp = []
    for c in range(a:c)
        call add(temp, init)
    endfor

    let array = []
    for r in range(a:r)
        call add(array, temp[:])
    endfor
    return array
endfunction

let rows = input("Enter number of rows: ")
let cols = input("Enter number of columns: ")
echo "\n"
let array = MakeArray(rows, cols)
let array[rows - 1][cols - 1] = rows * cols
echo array[rows - 1][cols - 1]
unlet array
```



## XPL0


```XPL0
inc     c:\cxpl\codes;  \(command words can be abbreviated to first 3 letters)
def     IntSize=4;      \number of bytes in an integer (2 or 4 depending on version)
int     X, Y, A, I;
[X:= IntIn(0);  Y:= IntIn(0);           \get 2 dimensions from user
A:= Reserve(X*IntSize);
for I:= 0 to X-1 do A(I):= Reserve(Y*IntSize);
A(X/2, Y/2):= X+Y;
IntOut(0, A(X/2, Y/2));  CrLf(0);
]
```


Space allocated by the Reserve intrinsic is automatically released when
the procedure that calls Release returns.

Example of a 12000x100 integer array for a total of 1.2 million integers:


```txt

12000 100
12100

```



## zkl

zkl doesn't have a native array type but lists of lists can do the same thing. It is garbage collected so things are cleaned up implicitly.

This code creates a 2d row major zero filled array.

Since the array is row major, sub-scripting works as expected: [row][col].

```zkl
rows:=ask("Rows: ").toInt();
cols:=ask("columns: ").toInt();
array:=rows.pump(List.createLong(rows),List.createLong(cols,0).copy);
array[1][2]=123;
array.println();
array[1][2].println();
```

The createLong method pre-allocates a list, optionally filled with a constant or computation.
{{out}}

```txt

Rows: 3
columns: 4
L(L(0,0,0,0),L(0,0,123,0),L(0,0,0,0))
123

```

If you want Matrix/linear algebra, you can use the GNU Scientific Library:

```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
rows:=ask("Rows: ").toInt();
cols:=ask("columns: ").toInt();
m:=GSL.Matrix(rows,cols);
m[1,2]=123;
m.format().println();
println(m[1,2]);
```

Again, garbage collected.
{{out}}

```txt

Rows: 3
columns: 4
      0.00,      0.00,      0.00,      0.00
      0.00,      0.00,    123.00,      0.00
      0.00,      0.00,      0.00,      0.00
123

```


## zonnon


```zonnon

module Main;
type
	Matrix = array *,* of integer;

var
	m: Matrix;
	i,j: integer;
begin
	write("first dim? ");readln(i);
	write("second dim? ");readln(j);
	m := new Matrix(i,j);
	m[0,0] := 10;
	writeln("m[0,0]:> ",m[0,0]);
	writeln("m[0,1].> ",m[0,1])
end Main.

```

{{Out}}

```txt

first dim? 10
second dim? 10
m[0,0]:>                   10
m[0,1].>                    0

```

