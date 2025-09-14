+++
title = "Write float arrays to a text file"
description = ""
date = 2019-02-21T21:13:48Z
aliases = []
[extra]
id = 2433
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "awk",
  "bbc_basic",
  "c",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "erlang",
  "euphoria",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "hicest",
  "idl",
  "j",
  "java",
  "joy",
  "jq",
  "julia",
  "kotlin",
  "lingo",
  "lua",
  "mathematica",
  "mercury",
  "netrexx",
  "newlisp",
  "nim",
  "ocaml",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "raven",
  "rexx",
  "ring",
  "rlab",
  "ruby",
  "run_basic",
  "sas",
  "scala",
  "seed7",
  "sidef",
  "spl",
  "standard_ml",
  "tcl",
  "vba",
  "yabasic",
  "zkl",
  "zx_spectrum_basic",
]
+++

{{task}} [[Category:File_handling]]
## Task

Write two equal-sized numerical arrays 'x' and 'y' to
a two-column text file named 'filename'.

The first column of the file contains values from an 'x'-array with a
given 'xprecision', the second -- values from 'y'-array with 'yprecision'.

For example, considering:
    x = {1, 2, 3, 1e11};
    y = {1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791};
                                                           /* sqrt(x) */
    xprecision = 3;
    yprecision = 5;

The file is:
    1    1
    2    1.4142
    3    1.7321
    1e+011   3.1623e+005

This task is intended as a subtask for [[Measure relative performance of sorting algorithms implementations]].





## Ada


```ada
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Float_Text_IO;                 use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Write_Float_Array is
   type Float_Array is array (1..4) of Float;
   procedure Write_Columns
             (  File   : File_Type;
                X      : Float_Array;
                Y      : Float_Array;
                X_Pres : Natural := 3;
                Y_Pres : Natural := 5
             ) is
   begin
      for I in Float_Array'range loop
         Put (File => File, Item => X(I), Fore => 1, Aft => X_Pres - 1);
         Put (File, " ");
         Put (File => File, Item => Y(I), Fore => 1, Aft => Y_Pres - 1);
         New_Line (File);
      end loop;
   end Write_Columns;

   File : File_Type;
   X : Float_Array := (1.0, 2.0, 3.0, 1.0e11);
   Y : Float_Array;
begin
   Put ("Tell us the file name to write:");
   Create (File, Out_File, Get_Line);
   for I in Float_Array'range loop
      Y(I) := Sqrt (X (I));
   end loop;
   Write_columns (File, X, Y);
   Close (File);
end Write_Float_Array;
```



## ALGOL 68

```algol68
PROC writedat = (STRING filename, []REAL x, y, INT x width, y width)VOID: (
  FILE f;
  INT errno = open(f, filename, stand out channel);
  IF errno NE 0 THEN stop FI;
  FOR i TO UPB x DO
    # FORMAT := IF the absolute exponent is small enough, THEN use fixed ELSE use float FI; #
    FORMAT repr x := ( ABS log(x[i])<x width | $g(-x width,x width-2)$ | $g(-x width,x width-4,-1)$ ),
           repr y := ( ABS log(y[i])<y width | $g(-y width,y width-2)$ | $g(-y width,y width-4,-1)$ );
    putf(f, (repr x, x[i], $" "$, repr y, y[i], $l$))
  OD;
  close(f)
);
# Example usage: #
test:(
  []REAL x = (1, 2, 3, 1e11);
  [UPB x]REAL y; FOR i TO UPB x DO y[i]:=sqrt(x[i]) OD;
  printf(($"x before:"$, $xg$, x, $l$));
  printf(($"y before:"$, $xg$, y, $l$));
  writedat("sqrt.dat", x, y, 3+2, 5+2);

  printf($"After:"l$);
  FILE sqrt dat;
  INT errno = open(sqrt dat, "sqrt.dat", stand in channel);
  IF errno NE 0 THEN stop FI;
  on logical file end(sqrt dat, (REF FILE sqrt dat)BOOL: stop);
  TO UPB x DO
    STRING line;
    get(sqrt dat, (line, new line));
    print((line,new line))
  OD
)
```

```txt

x before: +1.00000000000000e  +0 +2.00000000000000e  +0 +3.00000000000000e  +0 +1.00000000000000e +11
y before: +1.00000000000000e  +0 +1.41421356237310e  +0 +1.73205080756888e  +0 +3.16227766016838e  +5
After:
1.000 1.00000
2.000 1.41421
3.000 1.73205
 1e11  316228

```



## AWK

As usual, the order of array traversal in AWK is not necessarily the same as the input had:

```awk
$ awk 'BEGIN{split("1 2 3 1e11",x);
> split("1 1.4142135623730951 1.7320508075688772 316227.76601683791",y);
> for(i in x)printf("%6g %.5g\n",x[i],y[i])}'
1e+11 3.1623e+05
    1 1
    2 1.4142
    3 1.7321
```

For the text file part of the task, just redirect stdout to it.


## BBC BASIC

```bbcbasic
      DIM x(3), y(3)
      x() = 1, 2, 3, 1E11
      FOR i% = 0 TO 3
        y(i%) = SQR(x(i%))
      NEXT

      xprecision = 3
      yprecision = 5

      outfile% = OPENOUT("filename.txt")
      IF outfile%=0 ERROR 100, "Could not create file"

      FOR i% = 0 TO 3
        @% = &1000000 + (xprecision << 8)
        a$ = STR$(x(i%)) + CHR$(9)
        @% = &1000000 + (yprecision << 8)
        a$ += STR$(y(i%))
        PRINT #outfile%, a$ : BPUT #outfile%, 10
      NEXT

      CLOSE #outfile%
```

```txt

1	1
2	1.4142
3	1.7321
1E11	3.1623E5

```



## C


```c
#include <stdio.h>
#include <math.h>

int main(int argc, char **argv) {

   float x[4] = {1,2,3,1e11}, y[4];
   int i = 0;
   FILE *filePtr;

   filePtr = fopen("floatArray","w");

   for (i = 0; i < 4; i++) {
      y[i] = sqrt(x[i]);
      fprintf(filePtr, "%.3g\t%.5g\n", x[i], y[i]);
   }

   return 0;
}
```


The file <tt>"floatArray"</tt> then contains the following:
<lang>1       1
2       1.4142
3       1.7321
1e+11   3.1623e+05
```


## C#

```c#
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        var x = new double[] { 1, 2, 3, 1e11 };
        var y = new double[] { 1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791 };

        int xprecision = 3;
        int yprecision = 5;

        string formatString = "{0:G" + xprecision + "}\t{1:G" + yprecision + "}";

        using (var outf = new StreamWriter("FloatArrayColumns.txt"))
            for (int i = 0; i < x.Length; i++)
                outf.WriteLine(formatString, x[i], y[i]);
    }
}
```


```txt
1	1
2	1.4142
3	1.7321
1E+11	3.1623E+05
```




## C++

Function ''writedat()'':

```cpp
template<class InputIterator, class InputIterator2>
void writedat(const char* filename,
              InputIterator xbegin, InputIterator xend,
              InputIterator2 ybegin, InputIterator2 yend,
              int xprecision=3, int yprecision=5)
{
  std::ofstream f;
  f.exceptions(std::ofstream::failbit | std::ofstream::badbit);
  f.open(filename);
  for ( ; xbegin != xend and ybegin != yend; ++xbegin, ++ybegin)
    f << std::setprecision(xprecision) << *xbegin << '\t'
      << std::setprecision(yprecision) << *ybegin << '\n';
}
```

Example:

```cpp
#include <algorithm>
#include <cmath>    // ::sqrt()
#include <fstream>
#include <iomanip>  // setprecision()
#include <iostream>
#include <string>
#include <vector>

int main()
{
  try {
    // prepare test data
    double x[] = {1, 2, 3, 1e11};
    const size_t xsize = sizeof(x) / sizeof(*x);
    std::vector<double> y(xsize);
    std::transform(&x[0], &x[xsize], y.begin(), ::sqrt);

    // write file using default precisions
    writedat("sqrt.dat", &x[0], &x[xsize], y.begin(), y.end());

    // print the result file
    std::ifstream f("sqrt.dat");
    for (std::string line; std::getline(f, line); )
      std::cout << line << std::endl;
  }
  catch(std::exception& e) {
    std::cerr << "writedat: exception: '" << e.what() << "'\n";
    return 1;
  }
  return 0;
}
```


```txt

1       1
2       1.4142
3       1.7321
1e+11   3.1623e+05

```



## COBOL


```COBOL

       identification division.
       program-id. wr-float.
       environment division.
       input-output section.
       file-control.
           select report-file assign "float.txt"
               organization sequential.
       data division.
       file section.
       fd report-file
           report is floats.
       working-storage section.
       1 i binary pic 9(4).
       1 x-values comp-2.
        2 value 1.0.
        2 value 2.0.
        2 value 3.0.
        2 value 1.0e11.
       1 redefines x-values comp-2.
        2 x occurs 4.
       1 comp-2.
        2 y occurs 4.
       report section.
       rd floats.
       1 float-line type de.
        2 line plus 1.
         3 column 1 pic -9.99e+99 source x(i).
         2 column 12 pic -9.9999e+99 source y(i).
       procedure division.
       begin.
           open output report-file
           initiate floats
           perform varying i from 1 by 1
           until i > 4
               compute y(i) = function sqrt (x(i))
               generate float-line
           end-perform
           terminate floats
           close report-file
           stop run
           .
       end program wr-float.

```

```txt

 1.00E+00   1.0000E+00
 2.00E+00   1.4142E+00
 3.00E+00   1.7321E+00
 1.00E+11   3.1623E+05

```



## Common Lisp


```lisp
(with-open-file (stream (make-pathname :name "filename") :direction :output)
    (let* ((x (make-array 4 :initial-contents '(1 2 3 1e11)))
              (y (map 'vector 'sqrt x))
              (xprecision 3)
              (yprecision 5)
              (fmt (format nil "~~,1,~d,,G~~12t~~,~dG~~%" xprecision yprecision)))
        (map nil (lambda (a b)
                     (format stream fmt a b)) x y)))
```

Using [[CLISP]] I get
 1.          1.0000
 2.          1.4142
 3.          1.7321
 1.0E+011    3.16228E+5


## D


```d
import std.file, std.conv, std.string;

void main() {
    auto x = [1.0, 2, 3, 1e11];
    auto y = [1.0, 1.4142135623730951,
              1.7320508075688772, 316227.76601683791];
    int xPrecision = 3,
        yPrecision = 5;

    string tmp;
    foreach (i, fx; x)
        tmp ~= format("%." ~ text(xPrecision) ~ "g      %." ~
                      text(yPrecision) ~ "g\r\n", fx, y[i]);

    write("float_array.txt", tmp);
}
```

```txt
1	1
2	1.4142
3	1.7321
1e+11	3.1623e+05
```



## Elixir


```elixir
defmodule Write_float_arrays do
  def task(xs, ys, fname, precision\\[]) do
    xprecision = Keyword.get(precision, :x, 2)
    yprecision = Keyword.get(precision, :y, 3)
    format = "~.#{xprecision}g\t~.#{yprecision}g~n"
    File.open!(fname, [:write], fn file ->
      Enum.zip(xs, ys)
      |> Enum.each(fn {x, y} -> :io.fwrite file, format, [x, y] end)
    end)
  end
end

x = [1.0, 2.0, 3.0, 1.0e11]
y = for n <- x, do: :math.sqrt(n)
fname = "filename.txt"

Write_float_arrays.task(x, y, fname)
IO.puts File.read!(fname)

precision = [x: 3, y: 5]
Write_float_arrays.task(x, y, fname, precision)
IO.puts File.read!(fname)
```


```txt

1.0     1.00
2.0     1.41
3.0     1.73
1.0e+11 3.16e+5

1.00    1.0000
2.00    1.4142
3.00    1.7321
1.00e+11        3.1623e+5

```



## Erlang

Erlang thinks 1 is an integer. To persuade it otherwise I have to use 1.0.


```Erlang

-module( write_float_arrays ).

-export( [task/0, to_a_text_file/3, to_a_text_file/4] ).

task() ->
	File = "afile",
	Xs = [1.0, 2.0, 3.0, 1.0e11],
	Ys = [1.0, 1.4142135623730951, 1.7320508075688772, 316227.76601683791],
	Options = [{xprecision, 3}, {yprecision, 5}],
	to_a_text_file( File, Xs, Ys, Options ),
	{ok, Contents} = file:read_file( File ),
	io:fwrite( "File contents: ~p~n", [Contents] ).

to_a_text_file( File, Xs, Ys ) -> to_a_text_file( File, Xs, Ys, [] ).

to_a_text_file( File, Xs, Ys, Options ) ->
	Xprecision = proplists:get_value( xprecision, Options, 2 ),
	Yprecision = proplists:get_value( yprecision, Options, 2 ),
	Format = lists:flatten( io_lib:format("~~.~pg ~~.~pg~n", [Xprecision, Yprecision]) ),
	{ok, IO} = file:open( File, [write] ),
	[ok = io:fwrite( IO, Format, [X, Y]) || {X, Y} <- lists:zip( Xs, Ys)],
	file:close( IO ).

```


```txt

3> write_float_arrays:task().
File contents: <<"1.00 1.0000\n2.00 1.4142\n3.00 1.7321\n1.00e+11 3.1623e+5\n">>

```



## Euphoria


```euphoria
constant x = {1, 2, 3, 1e11},
         y = {1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791}

integer fn

fn = open("filename","w")
for n = 1 to length(x) do
    printf(fn,"%.3g\t%.5g\n",{x[n],y[n]})
end for
close(fn)
```


=={{header|F Sharp|F#}}==

```fsharp>[<EntryPoint
]
let main argv =
    let x = [ 1.; 2.; 3.; 1e11 ]
    let y = List.map System.Math.Sqrt x

    let xprecision = 3
    let yprecision = 5

    use file = System.IO.File.CreateText("float.dat")
    let line = sprintf "%.*g\t%.*g"
    List.iter2 (fun x y -> file.WriteLine (line xprecision x yprecision y)) x y
    0
```

Content of File, visualized with TAB=8

```txt
1       1
2       1.4142
3       1.7321
1e+11   3.1623e+05
```



## Forth

```forth
create x  1e f, 2e       f, 3e       f, 1e11       f,
create y  1e f, 2e fsqrt f, 3e fsqrt f, 1e11 fsqrt f,

: main
  s" sqrt.txt" w/o open-file throw  to outfile-id

  4 0 do
    4 set-precision
    x i floats + f@ f.
    6 set-precision
    y i floats + f@ f. cr
  loop

  outfile-id  stdout to outfile-id
  close-file throw ;
```



## Fortran

In ANSI FORTRAN 77 or later use OPEN STATEMENT, and formatted WRITE statement with implied DO loop:

```fortran
   real x(4), y(4)
   data x / 1.0, 2.0, 4.0, 1.0e11 /

   do 10 i = 1, 4
      y = sqrt(x)
10 continue

   open(unit=15, file='two_cols.txt', status='new')
   write(15,'(f20.3,f21.4)') (x(I), y(I), I = 1, 4)
   end
```


```fortran
program writefloats
  implicit none

  real, dimension(10) :: a, sqrta
  integer :: i
  integer, parameter :: unit = 40

  a = (/ (i, i=1,10) /)
  sqrta = sqrt(a)

  open(unit, file="xydata.txt", status="new", action="write")
  call writexy(unit, a, sqrta)
  close(unit)

contains

  subroutine writexy(u, x, y)
    real, dimension(:), intent(in) :: x, y
    integer, intent(in) :: u

    integer :: i

    write(u, "(2F10.4)") (x(i), y(i), i=lbound(x,1), ubound(x,1))
  end subroutine writexy

end program writefloats
```


The arrays x and y should have same bounds (and size); this constraint is not checked.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim x(0 To 3) As Double = {1, 2, 3, 1e11}
Dim y(0 To 3) As Double = {1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791}

Open "output.txt" For Output As #1
For i As Integer = 0 To 2
  Print #1, Using "#";  x(i);
  Print #1, Spc(7); Using "#.####"; y(i)
Next
Print #1, Using "#^^^^"; x(3);
Print #1, Spc(2); Using "##.####^^^^"; y(3)
Close #1
```

Contents of output.txt :
```txt

1       1.0000
2       1.4142
3       1.7321
1E+11   3.1623E+05

```



## Go


```go
package main

import (
    "fmt"
    "os"
)

var (
    x = []float64{1, 2, 3, 1e11}
    y = []float64{1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791}

    xprecision = 3
    yprecision = 5
)

func main() {
    if len(x) != len(y) {
        fmt.Println("x, y different length")
        return
    }
    f, err := os.Create("filename")
    if err != nil {
        fmt.Println(err)
        return
    }
    for i := range x {
        fmt.Fprintf(f, "%.*e, %.*e\n", xprecision-1, x[i], yprecision-1, y[i])
    }
    f.Close()
}
```

File contents:

```txt

1.00e+00, 1.0000e+00
2.00e+00, 1.4142e+00
3.00e+00, 1.7321e+00
1.00e+11, 3.1623e+05

```



## Haskell

Probably not very idiomatic but oh well

```haskell
import System.IO
import Text.Printf
import Control.Monad

writeDat filename x y xprec yprec =
  withFile filename WriteMode $ \h ->
     -- Haskell's printf doesn't support a precision given as an argument for some reason, so we insert it into the format manually:
     let writeLine = hPrintf h $ "%." ++ show xprec ++ "g\t%." ++ show yprec ++ "g\n" in
       zipWithM_ writeLine x y
```

Example usage
 Prelude> let x = [1, 2, 3, 1e11]
 Prelude> let y = map sqrt x
 Prelude> y
 [1.0,1.4142135623730951,1.7320508075688772,316227.7660168379]
 Prelude> writeDat "sqrt.dat" x y 3 5
 Prelude> readFile "sqrt.dat" >>= putStr
 1.000	1.00000
 2.000	1.41421
 3.000	1.73205
 1.000e11	316227.76602

Alternative solution without Printf

```haskell
import System.IO
import Control.Monad
import Numeric

writeDat filename x y xprec yprec =
  withFile filename WriteMode $ \h ->
     let writeLine a b = hPutStrLn h $ showGFloat (Just xprec) a "" ++ "\t" ++ showGFloat (Just yprec) b "" in
       zipWithM_ writeLine x y
```



## HicEst


```HicEst
REAL :: n=4, x(n), y(n)
CHARACTER :: outP = "Test.txt"

OPEN(FIle = outP)
x = (1, 2, 3, 1E11)
y = x ^ 0.5
DO i = 1, n
   WRITE(FIle=outP, Format='F5, F10.3') x(i), y(i)
ENDDO
```

Alternative: Display or Edit the formatted arrays in a [http://www.HicEst.com/MatrixExplorer spreadsheet-like dialog] with a common scroll bar.
The menu More - Export - File writes the formatted arrays to a file:

```HicEst
DLG(Text=x, Format='i12', Edit=y, Format='F10.2', Y=0)
```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages.


```unicon
link printf

procedure main()
    every put(x := [], (1 to 3) | 1e11)
    every put(y := [], sqrt(!x))
    every fprintf(open("filename","w"),"%10.2e %10.4e\n", x[i := 1 to *x], y[i])
end
```


Contents of <tt>filename</tt> after running:


```txt

->cat filename
    1.00e0   1.0000e0
    2.00e0   1.4142e0
    3.00e0   1.7321e0
  1.00e+11  3.1623e+5
->

```



## IDL


 ; the data:
 x = [1,2,3,1e11]
 y=sqrt(x)
 xprecision=3
 yprecision=5

 ; NOT how one would do things in IDL, but in the spirit of the task - the output format:
 form = string(xprecision,yprecision,format='("(G0.",I0.0,",1x,G0.",I0.0,")")')

 ; file I/O:
 openw,unit,"datafile.txt",/get
   for i = 1L, n_elements(x) do printf, unit, x[i-1],y[i-1],format=form
 free_lun,unit

The file <tt>"datafile.txt"</tt> then contains the following:


```idl
1 1
2 1.4142
3 1.7321
1E+011 3.1623E+005
```


This is fairly ugly and un-IDLish.
For example one shouldn't just rely on x and y having the same size.
And if data is output in human-readable form,
it should probably be lined up more nicely.
And if it really has to be in two-column format with x and y side by side,
one might consider running ASCII_Template or some such instead of that ugly hand-formatting.


## J


```j
require 'files'            NB.  for fwrites

x          =.  1 2 3 1e11
y          =.  %: x        NB.  y is sqrt(x)

xprecision =.  3
yprecision =.  5

filename   =.  'whatever.txt'

data       =.  (0 j. xprecision,yprecision) ": x,.y

data fwrites filename
```


Or, more concisely:


```j
((0 j. 3 5) ": (,.%:) 1 2 3 1e11) fwrites 'whatever.txt' [ require 'fwrites'
```


This loses all of the inline comments and names, and instead relies on the reader's understanding of the purpose of each of the names (for example: 3 is the precision of the first column, and 5 is the precision of the second column).

Note that J's idea of precision here is "positions after the decimal point":


```j
   (0 j. 3 5) ": (,.%:) 1 2 3 1e11
           1.000      1.00000
           2.000      1.41421
           3.000      1.73205
100000000000.000 316227.76602
```



## Java


```java5
import java.io.*;

public class FloatArray {
    public static void writeDat(String filename, double[] x, double[] y,
                                int xprecision, int yprecision)
        throws IOException {
        assert x.length == y.length;
        PrintWriter out = new PrintWriter(filename);
        for (int i = 0; i < x.length; i++)
            out.printf("%."+xprecision+"g\t%."+yprecision+"g\n", x[i], y[i]);
        out.close();
    }

    public static void main(String[] args) {
        double[] x = {1, 2, 3, 1e11};
        double[] y = new double[x.length];
        for (int i = 0; i < x.length; i++)
            y[i] = Math.sqrt(x[i]);

        try {
            writeDat("sqrt.dat", x, y, 3, 5);
        } catch (IOException e) {
            System.err.println("writeDat: exception: "+e);
        }

        try {
            BufferedReader br = new BufferedReader(new FileReader("sqrt.dat"));
            String line;
            while ((line = br.readLine()) != null)
                System.out.println(line);
        } catch (IOException e) { }
    }
}
```



## Joy


```Joy

DEFINE write-floats ==
['g 0] [formatf] enconcat map rollup
['g 0] [formatf] enconcat map swap zip
"filename" "w" fopen swap
[[fputchars] 9 fputch] step 10 fputch] step
fclose.

```


Using it:


```txt

[1.0 2.0 3.0 1e11] 3
[1.0 1.41421356 1.73205080 316227.7660168] 5
write-floats.

```



## jq

''' Program''':

```jq
[1, 2, 3, 1e11] as $x
| $x | map(sqrt) as $y
| range(0; $x|length) as $i
| "\($x[$i])  \($y[$i])"
```

''' Execution''':
To write the output to "filename":

```sh
$ jq -n -r -f Write_float_arrays_to_a_text_file.jq > filename
```



## Julia


```julia
xprecision = 3
yprecision = 5
x = round.([1, 2, 3, 1e11],xprecision)
y = round.([1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791],yprecision)
writedlm("filename", [x y], '\t')
```



## Kotlin


```scala
// version 1.1.2

import java.io.File

fun main(args: Array<String>) {
    val x = doubleArrayOf(1.0, 2.0, 3.0, 1e11)
    val y = doubleArrayOf(1.0, 1.4142135623730951, 1.7320508075688772, 316227.76601683791)
    val xp = 3
    val yp = 5
    val f = "%.${xp}g\t%.${yp}g\n"
    val writer = File("output.txt").writer()
    writer.use {
        for (i in 0 until x.size) {
            val s = f.format(x[i], y[i])
            writer.write(s)
        }
    }
}
```


Contents of 'output.txt':

```txt

1.00	1.0000
2.00	1.4142
3.00	1.7321
1.00e+11	3.1623e+05

```



## Lingo


```lingo
on saveFloatLists (filename, x, y, xprecision, yprecision)
  eol = numtochar(10) -- LF
  fp = xtra("fileIO").new()
  fp.openFile(tFile, 2)
  cnt = x.count
  repeat with i = 1 to cnt
    the floatPrecision = xprecision
    fp.writeString(string(x[i])
    fp.writeString(TAB)
    the floatPrecision = yprecision
    fp.writeString(string(y[i])
    fp.writeString(eol)
  end repeat
  fp.closeFile()
end
```



```lingo
x = [1.0, PI, sqrt(2)]
y = [2.0, log(10), sqrt(3)]
saveFloatLists("floats.txt", x, y, 3, 5)
```



## Lua


```lua
filename = "file.txt"

x = { 1, 2, 3, 1e11 }
y = { 1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791 };
xprecision = 3;
yprecision = 5;

fstr = "%."..tostring(xprecision).."f ".."%."..tostring(yprecision).."f\n"

fp = io.open( filename, "w+" )

for i = 1, #x do
    fp:write( string.format( fstr, x[i], y[i] ) )
end

io.close( fp )
```



## Mathematica


```Mathematica
exportPrec[path_, data1_, data2_, prec1_, prec2_]:=Export[path,Transpose[{Map[ToString[NumberForm[#, prec2]] &, data2],Map[ToString[NumberForm[#, prec1]] &, data1]}], "Table"]
```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
    x = [1, 2, 3, 1e11];
   y = [1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791];

   fid = fopen('filename','w')
   fprintf(fid,'%.3g\t%.5g\n',[x;y]);
   fclose(fid);
```


```txt
1	1
2	1.4142
3	1.7321
1e+11	3.1623e+05
```



## Mercury


```Mercury
:- module write_float_arrays.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module float, list, math, string.

main(!IO) :-
    io.open_output("filename", OpenFileResult, !IO),
    (
        OpenFileResult = ok(File),
        X = [1.0, 2.0, 3.0, 1e11],
        list.foldl_corresponding(write_dat(File, 3, 5), X, map(sqrt, X), !IO),
        io.close_output(File, !IO)
    ;
        OpenFileResult = error(IO_Error),
        io.stderr_stream(Stderr, !IO),
        io.format(Stderr, "error: %s\n", [s(io.error_message(IO_Error))], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred write_dat(text_output_stream::in, int::in, int::in, float::in,
     float::in, io::di, io::uo) is det.

write_dat(File, XPrec, YPrec, X, Y, !IO) :-
    io.format(File, "%.*g\t%.*g\n", [i(XPrec), f(X), i(YPrec), f(Y)], !IO).
```


File contents:


```txt

1              1
2              1.4142
3              1.7321
1e+11      3.1623e+05

```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols nobinary

-- Invent a target text file name based on this program's source file name
parse source . . pgmName '.nrx' .
outFile = pgmName || '.txt'

do
  formatArrays(outFile, [1, 2, 3, 1e+11], [1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791])
catch ex = Exception
  ex.printStackTrace
end

return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- This function formats the input arrays.
--   It has defaults for the x & y precision values of 3 & 5
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method formatArrays(outFile, xf = Rexx[], yf = Rexx[], xprecision = 3, yprecision = 5) -
  public static signals IllegalArgumentException, FileNotFoundException, IOException

  if xf.length > yf.length then signal IllegalArgumentException('Y array must be at least as long as X array')

  fw = BufferedWriter(OutputStreamWriter(FileOutputStream(outFile)))

  loop i_ = 0 to xf.length - 1
    row = xf[i_].format(null, xprecision, null, xprecision).left(15) yf[i_].format(null, yprecision, null, yprecision)
    (Writer fw).write(String row)
    fw.newLine
    end i_
    fw.close

  return

```

```txt

1.000           1.00000
2.000           1.41421
3.000           1.73205
1.000E+11       3.16228E+5

```



## NewLISP


```NewLISP
; file:   write-float-array.lsp
; url:    http://rosettacode.org/wiki/Write_float_arrays_to_a_text_file
; author: oofoe 2012-01-30

; The "transpose" function is used to flip the joined lists around so
; that it's easier to iterate through them together.

(define (write-float-array x xp y yp filename)
  (let ((f (format "%%-10.%dg %%-10.%dg" xp yp))
        (o (open filename "write")))
    (dolist (v (transpose (list x y)))
      (write-line o (format f (v 0) (v 1))))
    (close o)
  ))

; Test

(write-float-array
 '(1 2 3 1e11) 3
 '(1 1.4142135623730951 1.7320508075688772 316227.76601683791) 5
 "filename.chan")

(println "File contents:")
(print (read-file "filename.chan"))

(exit)
```


```txt
File contents:
1          1
2          1.4142
3          1.7321
1e+011     3.1623e+005

```



## Nim


```nim
import strutils, math, sequtils

const
   outFileName = "floatarr2file.txt"

proc sqrt*(x: int64): float {.importc: "sqrt", header: "<math.h>".}

const
   xprecision = 3
   yprecision = 5

var a: seq[int64] = @[int64(1), 2, 3, 100_000_000_000]
var b: seq[float] = @[sqrt(a[0]), sqrt(a[1]), sqrt(a[2]), sqrt(a[3])]
var c = zip(a,b)
var res: string = ""
for t in c:
    res.add($formatFloat(float(t.a),ffDefault,xprecision) & "\t" & $formatFloat(t.b,ffDefault,yprecision) & "\n")

writeFile(outFileName,res)
var res2 = readFile(outFileName)
echo(res2)
```

```txt
1.00	1.0000
2.00	1.4142
3.00	1.7321
1.00e+11	3.1623e+05
```



## OCaml


```ocaml
let write_dat filename x y ?(xprec=3) ?(yprec=5) () =
  let oc = open_out filename in
  let write_line a b = Printf.fprintf oc "%.*g\t%.*g\n" xprec a yprec b in
    List.iter2 write_line x y;
    close_out oc
```

Example usage
 # let x = [1.0; 2.0; 3.0; 1e11];;
 val x : float list = [1.; 2.; 3.; 100000000000.]
 # let y = List.map sqrt x;;
 val y : float list =
   [1.; 1.41421356237309515; 1.73205080756887719; 316227.766016837908]
 # write_dat "sqrt.dat" x y ();;
 - : unit = ()
 # let ic = open_in "sqrt.dat";;
 val ic : in_channel = <abstr>
 # try
     while true do
       print_endline (input_line ic)
     done
   with End_of_file -> ();;
 1	1
 2	1.4142
 3	1.7321
 1e+11	3.1623e+05
 - : unit = ()


## PARI/GP

```parigp
f(x,pr)={
	Strprintf(if(x>=10^pr,
		Str("%.",pr-1,"e")
	,
		Str("%.",pr-#Str(x\1),"f")
	),x)
};
wr(x,y,xprec,yprec)={
	for(i=1,#x,
		write("filename",f(x[i],xprec),"\t",f(y[i],yprec))
	)
};
```



## Pascal


```pascal
Program WriteNumbers;

const
  x: array [1..4] of double = (1, 2, 3, 1e11);
  xprecision = 3;
  yprecision = 5;
  baseDigits = 7;

var
  i: integer;
  filename: text;

begin
  assign (filename, 'filename');
  rewrite (filename);
  for i := 1 to 4 do
    writeln (filename, x[i]:baseDigits+xprecision, sqrt(x[i]):baseDigits+yprecision);
  close (filename);
end.
```

File contents

```txt

 1.00E+000 1.0000E+000
 2.00E+000 1.4142E+000
 3.00E+000 1.7321E+000
 1.00E+011 3.1623E+005

```



## Perl


```perl
use autodie;

sub writedat {
    my ($filename, $x, $y, $xprecision, $yprecision) = @_;

    open my $fh, ">", $filename;

    for my $i (0 .. $#$x) {
        printf $fh "%.*g\t%.*g\n", $xprecision||3, $x->[$i], $yprecision||5, $y->[$i];
    }

    close $fh;
}

my @x = (1, 2, 3, 1e11);
my @y = map sqrt, @x;

writedat("sqrt.dat", \@x, \@y);
```

File contents

```txt

1	1
2	1.4142
3	1.7321
1e+11	3.1623e+05

```


Alternatively, with the CPAN List::MoreUtils package:


```perl
use autodie;
use List::MoreUtils qw(each_array);

sub writedat {
    my ($filename, $x, $y, $xprecision, $yprecision) = @_;
    open my $fh, ">", $filename;

    my $ea = each_array(@$x, @$y);
    while ( my ($i, $j) = $ea->() ) {
        printf $fh "%.*g\t%.*g\n", $xprecision||3, $i, $yprecision||5, $j;
    }

    close $fh;
}

my @x = (1, 2, 3, 1e11);
my @y = map sqrt, @x;

writedat("sqrt.dat", \@x, \@y);
```



## Perl 6

```perl6
sub writedat ( $filename, @x, @y, $x_precision = 3, $y_precision = 5 ) {
    my $fh = open $filename, :w;

    for @x Z @y -> $x, $y {
        $fh.printf: "%.*g\t%.*g\n", $x_precision, $x, $y_precision, $y;
    }

    $fh.close;
}

my @x = 1, 2, 3, 1e11;
my @y = @x.map({.sqrt});

writedat( 'sqrt.dat', @x, @y );
```

File contents

```txt
1	1
2	1.4142
3	1.7321
1e+11	3.1623e+05
```


In Perl 6 Real::base can be used to convert to Str with arbitrary precision and any base you like. Using the hyper-operator >>. let us strip loops, many temporary variables and is a candidate for autothreading.


```perl6
sub writefloat($filename, @x, @y, :$x-precision = 3, :$y-precision = 5) {
    constant TAB = "\t" xx *;
    constant NL = "\n" xx *;

    open($filename, :w).print(
        flat @x>>.base(10, $x-precision) Z TAB Z @y>>.base(10, $y-precision) Z NL);
}
my @x = 1, 2, 3, 1e11;
writefloat('sqrt.dat', @x, @x>>.sqrt, :y-precision(20));
```


File contents

```txt
1.000	1.00000000000000000000
2.000	1.41421356237309510107
3.000	1.73205080756887719318
100000000000.000	316227.76601683790795505047
```



## Phix

Copy of [[Write_float_arrays_to_a_text_file#Euphoria|Euphoria]]

```Phix
constant x = {1, 2, 3, 1e11},
         y = {1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791}

integer fn = open("filename","w")
for i=1 to length(x) do
    printf(fn,"%.3g\t%.5g\n",{x[i],y[i]})
end for
close(fn)
```

File contents:

```txt

1       1
2       1.4142
3       1.7321
1e+11   3.1623e+5

```



## PicoLisp

An exponential format like "1e11" is not supported

```PicoLisp
(setq *Xprecision 3  *Yprecision 5)

(scl 7)
(mapc
   '((X Y)
      (prinl
         (round X *Xprecision)
         "  "
         (round Y *Yprecision) ) )
   (1.0 2.0 3.0)
   (1.0 1.414213562 1.732050807) )
```

```txt
1.000  1.00000
2.000  1.41421
3.000  1.73205
```



## PL/I


```PL/I
*Process source attributes xref;
 aaa: Proc Options(main);
 declare X(5) float (9)  initial (1, 2, 3, 4, 5),
         Y(5) float (18) initial (9, 8, 7, 6, 1e9);
 declare (x_precision, y_precision) fixed binary;
 Dcl out stream output;
 open file(out) title('/OUT.TXT,type(text),recsize(100)');
 x_precision = 9;
 y_precision = 16;
 put file(out) edit((X(i),Y(i) do i=1 to 5))
                    (skip,e(19,x_precision),
                     x(2),e(24,y_precision));
 end;
```

```txt
  1.000000000E+0000  9.0000000000000000E+0000
  2.000000000E+0000  8.0000000000000000E+0000
  3.000000000E+0000  7.0000000000000000E+0000
  4.000000000E+0000  6.0000000000000000E+0000
  5.000000000E+0000  1.0000000000000000E+0009
```



## PowerShell


```PowerShell

$x = @(1, 2, 3, 1e11)
$y = @(1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791)
$xprecision = 3
$yprecision = 5
$arr = foreach($i in 0..($x.count-1)) {
    [pscustomobject]@{x = "{0:g$xprecision}"  -f $x[$i]; y = "{0:g$yprecision}" -f $y[$i]}
}
$arr | format-table -HideTableHeaders > filename.txt

```

<b>Output:</b>

```txt

1     1
2     1.4142
3     1.7321
1e+11 3.1623e+05

```



## PureBasic


```PureBasic
#Size = 4

DataSection
  Data.f 1, 2, 3, 1e11 ;x values, how many values needed is determined by #Size
EndDataSection

Dim x.f(#Size - 1)
Dim y.f(#Size - 1)

Define i
For i = 0 To #Size - 1
  Read.f x(i)
  y(i) = Sqr(x(i))
Next

Define file$, fileID, xprecision = 3, yprecision = 5, output$

file$ = SaveFileRequester("Text file for float data", "xydata.txt","Text file | *.txt", 0)
If file$
  fileID = OpenFile(#PB_Any, file$)
  If fileID
    For i = 0 To #Size - 1
      output$ = StrF(x(i), xprecision) + Chr(9) + StrF(y(i), yprecision)
      WriteStringN(fileID, output$)
    Next
    CloseFile(fileID)
  EndIf
EndIf
```

{{out}} to text file:

```txt
1.000	1.00000
2.000	1.41421
3.000	1.73205
99999997952.000	316227.75000
```



## Python

```python
import itertools
def writedat(filename, x, y, xprecision=3, yprecision=5):
    with open(filename,'w') as f:
        for a, b in itertools.izip(x, y):
            print >> f, "%.*g\t%.*g" % (xprecision, a, yprecision, b)
```

Example usage

```python>>>
 import math
>>> x = [1, 2, 3, 1e11]
>>> y = map(math.sqrt, x)
>>> y
[1.0, 1.4142135623730951, 1.7320508075688772, 316227.76601683791]
>>> writedat("sqrt.dat", x, y)
>>> # check
...
>>> for line in open('sqrt.dat'):
...   print line,
...
1       1
2       1.4142
3       1.7321
1e+011  3.1623e+005
```


```python
def writedat(filename, x, y, xprecision=3, yprecision=5):
    with open(filename,'w') as f:
        for a, b in zip(x, y):
            print("%.*g\t%.*g" % (xprecision, a, yprecision, b), file=f)
            #or, using the new-style formatting:
            #print("{1:.{0}g}\t{3:.{2}g}".format(xprecision, a, yprecision, b), file=f)
```



## R


```R
writexy <- function(file, x, y, xprecision=3, yprecision=3)
{
   #Format inputs as required, and join together
   fx <- formatC(x, digits=xprecision, format="g", flag="-")
   fy <- formatC(y, digits=yprecision, format="g", flag="-")
   dfr <- data.frame(fx, fy)
   #Write to file.  Note that this encloses the formatted number in quotes,
   write.table(dfr, file=file, sep="\t", row.names=FALSE)
   #... so we have to process the output
   str <- readLines(file)
   writeLines(gsub('"', '', str), file)
}


x <- c(1, 2, 3, 1e11)
y <- sqrt(x)
writexy('test.txt', x, y, yp=5)
```



## Racket


```Racket

#lang racket

(define xs '(1.0 2.0 3.0 1.0e11))
(define ys '(1.0 1.4142135623730951 1.7320508075688772 316227.76601683791))

(define xprecision 3)
(define yprecision 5)

(with-output-to-file "some-file" #:exists 'truncate
  (λ() (for ([x xs] [y ys])
         (displayln (~a (~r x #:precision xprecision)
                    "  "
                    (~r y #:precision yprecision))))))

#|
The output is not using exponenets as above, but that's not needed
since Racket can read these numbers fine:

1  1
2  1.41421
3  1.73205
100000000000  316227.76602
|#

```



## Raven


```Raven
3 as $xprecision
5 as $yprecision

[ ] as $results

[ 1 2 3 1e11 ] as $a

group
   $a each sqrt
list as $b

# generate format specifier   "%-8.3g %.5g\n"
"%%-8.%($xprecision)dg %%.%($yprecision)dg\n" as $f

define print2 use $v1, $v2, $f
   $v2 1.0 prefer  $v1 1.0 prefer $f format $results push

4 each as $i
   $f $b $i get $a $i get print2
$results "" join "results.dat" write
```

results.dat file contains:

```txt
1        1
2        1.4142
3        1.7321
1e+11    3.1623e+05
```



## REXX


```rexx
/*REXX program writes  two arrays  to a file  with a  specified (limited)  precision.   */
numeric digits 1000                              /*allow use of a huge number of digits.*/
oFID= 'filename'                                 /*name of the  output  File IDentifier.*/
x.=;  y.=;                     x.1= 1    ;    y.1=      1
                               x.2= 2    ;    y.2=      1.4142135623730951
                               x.3= 3    ;    y.3=      1.7320508075688772
                               x.4= 1e11 ;    y.4= 316227.76601683791
xPrecision= 3                                    /*the precision for the   X   numbers. */
yPrecision= 5                                    /* "      "      "   "    Y      "     */
                do j=1  while  x.j\==''          /*process and reformat all the numbers.*/
                newX=rule(x.j, xPrecision)       /*format  X  numbers with new precision*/
                newY=rule(y.j, yPrecision)       /*   "    Y     "      "   "      "    */
                aLine=translate(newX || left('',4) || newY,   "e",  'E')
                say aLine                        /*display re─formatted numbers ──► term*/
                call lineout oFID, aLine         /*write         "         "     "  disk*/
                end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
rule: procedure;   parse arg z 1 oz,p;     numeric digits p;       z=format(z,,p)
      parse var z  mantissa      'E'  exponent                /*get the dec dig exponent*/
      parse var    mantissa int  '.'  fraction                /* "  integer and fraction*/
                             fraction=strip(fraction, 'T', 0) /*strip  trailing  zeroes.*/
      if fraction\==''  then fraction="."fraction             /*any fractional digits ? */
      if exponent\==''  then exponent="E"exponent             /*in exponential format ? */
      z=int || fraction || exponent                           /*format #  (as per rules)*/
      if datatype(z,'W')  then return format(oz/1,,0)         /*is it a whole number ?  */
                               return format(oz/1,,,3,0)      /*3 dec. digs in exponent.*/
```

'''output'''   when using the default (internal) data:

```txt

1    1
2    1.4142
3    1.7321
1e+011    3.1623e+005

```



## Ring


```ring

# Project : Write float arrays to a text file

decimals(13)
x = [1, 2, 3, 100000000000]
y = [1, 1.4142135623730, 1.7320508075688, 316227.76601683]
str = list(4)
fn = "C:\Ring\calmosoft\output.txt"
fp = fopen(fn,"wb")
for i = 1 to 4
     str[i] = string(x[i]) + " | " + string(y[i]) + windowsnl()
     fwrite(fp, str[i])
next
fclose(fp)
fp = fopen("C:\Ring\calmosoft\output.txt","r")
r = ""
while isstring(r)
        r = fgetc(fp)
        if r = char(10) see nl
        else see r ok
end
fclose(fp)

```

Output:

```txt

1 | 1
2 | 1.4142135623730
3 | 1.7320508075688
100000000000.0000000000000 | 316227.76601683

```



## RLaB

In RLaB this task can be done in two ways:

1. Direct writing of the numerical data to the file of an array using function ''writem''. Here the writing format
is specified using the global property that is accessible through function ''format''.

```RLaB

>> x = rand(10,1); y = rand(10,1);
>> writem("mytextfile.txt", [x,y]);

```


2. Converting the numerical data to text, and then writing the text to the file, using the same function ''writem''.
Here, the writing format is specified through ''text'' function, and the result is written as a plain string matrix.

```RLaB

>> x = rand(10,1); y = rand(10,1);
>> s = text( [x,y], "%10.8f" );
>> writem("mytextfile.txt", s);

```


Please note, ''writem'' function in RLaB can operate in two-fold fashion. RLaB keeps track of the open files that were created using the built-in function ''open''.

If user writes the data to a file using ''open'' followed by ''writem'' then RLaB opens the file in append mode if it already hasn't been opened. If it has been, then the command ''open'' is ignored (say in batch mode). Then, each successive call to ''writem'' appends newest data to the end of the file while keeping the file open. RLaB will close the file (and OS will flush its file buffer) upon the command ''close''.

If user writes the data to a file by using only ''writem'' then the RLaB temporarily opens the file, writes the data to it, and then closes the file. Successive calls to ''writem'' in this mode will erase the previous content of the file.


## Ruby


```ruby
# prepare test data
x = [1, 2, 3, 1e11]
y = x.collect { |xx| Math.sqrt xx }
xprecision = 3
yprecision = 5

# write the arrays
open('sqrt.dat', 'w') do |f|
  x.zip(y) { |xx, yy| f.printf("%.*g\t%.*g\n", xprecision, xx, yprecision, yy) }
end

# print the result file
open('sqrt.dat', 'r') { |f| puts f.read }
```

Result:

```txt
1       1
2       1.4142
3       1.7321
1e+11   3.1623e+05
```



## Run BASIC


```runbasic
x$ = "1, 2, 3, 1e11"
y$ = "1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791"

open "filename" for output as #f   ' Output to "filename"
for i = 1 to 4
 print #f, using("##############.###",val(word$(x$,i,",")));"|";using("#######.#####",val(word$(y$,i,",")))
next i
close #f
```

```txt
             1.000|      1.00000
             2.000|      1.41421
             3.000|      1.73205
  100000000000.000| 316227.76602
```



## SAS


```sas
data _null_;
input x y;
file "output.txt";
put x 12.3 " " y 12.5;
cards;
1      1
2      1.4142135623730951
3      1.7320508075688772
1e11   316227.76601683791
;
run;
```



## Scala

```scala
import java.io.{File, FileWriter, IOException}

object FloatArray extends App {
  val x: List[Float] = List(1f, 2f, 3f, 1e11f)

  def writeStringToFile(file: File, data: String, appending: Boolean = false) =
    using(new FileWriter(file, appending))(_.write(data))

  def using[A <: {def close() : Unit}, B](resource: A)(f: A => B): B =
    try f(resource) finally resource.close()

  try {
    val file = new File("sqrt.dat")
    using(new FileWriter(file))(writer => x.foreach(x => writer.write(f"$x%.3g\t${math.sqrt(x)}%.5g\n")))
  } catch {
    case e: IOException => println(s"Running Example failed: ${e.getMessage}")
  }
}
```



## Seed7

The library [http://seed7.sourceforge.net/libraries/math.htm math.s7i] defines the function [http://seed7.sourceforge.net/libraries/math.htm#sqrt%28ref_float%29 sqrt]. The operators [http://seed7.sourceforge.net/libraries/float.htm#%28ref_float%29sci%28ref_integer%29 sci] and [http://seed7.sourceforge.net/libraries/float.htm#%28in_string%29exp%28in_integer%29 exp] (defined in [http://seed7.sourceforge.net/libraries/float.htm float.s7i]) support writing floating point numbers in scientific notation.
```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "math.s7i";

const proc: main is func
  local
    const array float: numbers is [] (1.0, 2.0, 3.0, 1.0e11);
    var float: aFloat is 0.0;
    var file: aFile is STD_NULL;
  begin
    aFile := open("filename", "w");
    for aFloat range numbers do
      writeln(aFile, aFloat sci 3 exp 2 <& " " <& sqrt(aFloat) sci 5 exp 2);
    end for;
    close(aFile);
  end func;
```
 1.000e+00 1.00000e+00
 2.000e+00 1.41421e+00
 3.000e+00 1.73205e+00
 1.000e+11 3.16228e+05


## Sidef

```ruby
func writedat(filename, x, y, x_precision=3, y_precision=5) {
    var fh = File(filename).open_w
 
    for a,b in (x ~Z y) {
        fh.printf("%.*g\t%.*g\n", x_precision, a, y_precision, b)
    }
 
    fh.close
}
 
var x = [1, 2, 3, 1e11]
var y = x.map{.sqrt}
 
writedat('sqrt.dat', x, y)
```

```txt

1	1
2	1.4142
3	1.7321
1e+11	3.1623e+05

```



## SPL


```spl
x = [1, 2, 3, 10^11]
y = [1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791]
xprecision = 3
yprecision = 5
> i, 1..4
  s1 = #.str(x[i],"g"+xprecision)
  s2 = #.str(y[i],"g"+yprecision)
  #.writeline("file.txt",s1+#.tab+s2)
<
```



## Standard ML


```sml
fun writeDat (filename, x, y, xprec, yprec) = let
  val os = TextIO.openOut filename
  fun write_line (a, b) =
    TextIO.output (os, Real.fmt (StringCvt.GEN (SOME xprec)) a ^ "\t" ^
                       Real.fmt (StringCvt.GEN (SOME yprec)) b ^ "\n")
in
  ListPair.appEq write_line (x, y);
  TextIO.closeOut os
end;
```

Example usage
 - val x = [1.0, 2.0, 3.0, 1e11];
 val x = [1.0,2.0,3.0,100000000000.0] : real list
 - val y = map Math.sqrt x;
 val y = [1.0,1.41421356237,1.73205080757,316227.766017] : real list
 - writeDat ("sqrt.dat", x, y, 3, 5);
 val it = () : unit
 - val is = TextIO.openIn "sqrt.dat";
 val is = - : TextIO.instream
 - print (TextIO.inputAll is);
 1.0	1.0
 2.0	1.4142
 3.0	1.7321
 1E11	3.1623E05
 val it = () : unit


## Tcl


```tcl
set x {1 2 3 1e11}
foreach a $x {lappend y [expr {sqrt($a)}]}
set fh [open sqrt.dat w]
foreach a $x b $y {
    puts $fh [format "%.*g\t%.*g" $xprecision $a $yprecision $b]
}
close $fh

set fh [open sqrt.dat]
puts [read $fh [file size sqrt.dat]]
close $fh
```

```txt
1	1
2	1.4142
3	1.7321
1e+011	3.1623e+005
```



## VBA


```vb
Public Sub main()
    x = [{1, 2, 3, 1e11}]
    y = [{1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791}]
    Dim TextFile As Integer
    TextFile = FreeFile
    Open "filename" For Output As TextFile
    For i = 1 To UBound(x)
        Print #TextFile, Format(x(i), "0.000E-000"), Format(y(i), "0.00000E-000")
    Next i
    Close TextFile
End Sub
```
```txt
1,000E000     1,00000E000
2,000E000     1,41421E000
3,000E000     1,73205E000
1,000E011     3,16228E005

```


## Yabasic


```Yabasic
x$ = "1 2 3 1e11"
pr1 = 3 : pr2 = 5

dim x$(1)
n = token(x$, x$())

f = open("filename.txt", "w")

for i = 1 to n
	print #f str$(val(x$(i)), "%1." + str$(pr1) + "g") + "\t" + str$(sqrt(val(x$(i))), "%1." + str$(pr2) + "g")
next i

close #f
```



## zkl


```zkl
fcn writeFloatArraysToFile(filename, xs,xprecision, ys,yprecision){
   f  :=File(filename,"w");
   fmt:="%%.%dg\t%%.%dg".fmt(xprecision,yprecision).fmt;  // "%.3g\t%.5g".fmt
   foreach x,y in (xs.zip(ys)){ f.writeln(fmt(x,y)); }
   f.close();
}

xs,ys := T(1.0, 2.0, 3.0, 1e11), xs.apply("sqrt");
xprecision,yprecision := 3,5;
writeFloatArraysToFile("floatArray.txt", xs,xprecision, ys,yprecision);
```

```txt

$ cat floatArray
1	1
2	1.4142
3	1.7321
1e+11	3.1623e+05

```



## ZX Spectrum Basic


The ZX Spectrum has a dedicated file format for floating point arrays.
Although this format is not a text file, it would be the usual format
for writing such data to a file on a ZX Spectrum.
Here we write the contents of the array g() to a file:


```zxbasic
SAVE "myarray" DATA g()
```

