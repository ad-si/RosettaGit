+++
title = "Largest int from concatenated ints"
description = ""
date = 2019-05-11T14:41:58Z
aliases = []
[extra]
id = 13245
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "aime",
  "algol_68",
  "autohotkey",
  "awk",
  "bbc_basic",
  "bracmat",
  "c",
  "ceylon",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "erlang",
  "factor",
  "fortran",
  "freebasic",
  "gambas",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "mathematica",
  "netrexx",
  "nim",
  "ocaml",
  "oforth",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "prolog",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "sidef",
  "tcl",
  "vbscript",
  "vim_script",
  "zkl",
]
+++

## Task

Given a set of positive integers, write a function to order the integers in such a way that the concatenation of the numbers forms the largest possible integer and return this integer.

Use the following two sets of integers as tests   and   show your program output here.

:::::*   {1, 34, 3, 98, 9, 76, 45, 4}
:::::*   {54, 546, 548, 60}



;Possible algorithms:
# A solution could be found by trying all combinations and return the best.
# Another way to solve this is to note that in the best arrangement, for any two adjacent original integers '''X''' and '''Y''', the concatenation '''X''' followed by '''Y''' will be numerically greater than or equal to the concatenation '''Y''' followed by '''X.
# Yet another way to solve this is to pad the integers to the same size by repeating the digits then sort using these repeated integers as a sort key.


## See also

*   [http://www.quora.com/Algorithms/What-is-the-most-efficient-way-to-arrange-the-given-numbers-to-form-the-biggest-number Algorithms: What is the most efficient way to arrange the given numbers to form the biggest number?]
*   [http://stackoverflow.com/questions/14532105/constructing-the-largest-number-possible-by-rearranging-a-list/14539943#14539943 Constructing the largest number possible by rearranging a list]





## Ada


The algorithmic idea is to apply a twisted comparison function:


```Ada
function Order(Left, Right: Natural) return Boolean is
      ( (Img(Left) & Img(Right)) > (Img(Right) & Img(Left)) );
```

This function converts the parameters Left and Right to strings and returns True if (Left before Right)
exceeds (Right before Left). It needs Ada 2012 -- the code for older versions of Ada would be more verbose.

The rest is straightforward: Run your favourite sorting subprogram that allows to use the function "Order" instead of standard comparison operators ("<" or ">" or so) and print the results:


```Ada
with Ada.Text_IO, Ada.Containers.Generic_Array_Sort;

procedure Largest_Int_From_List is

   function Img(N: Natural) return String is
      S: String := Integer'Image(N);
   begin
      return S(S'First+1 .. S'Last); -- First character is ' '
   end Img;

   function Order(Left, Right: Natural) return Boolean is
      ( (Img(Left) & Img(Right)) > (Img(Right) & Img(Left)) );

   type Arr_T is array(Positive range <>) of Natural;

   procedure Sort is new Ada.Containers.Generic_Array_Sort
     (Positive, Natural, Arr_T, Order);

   procedure Print_Sorted(A: Arr_T) is
      B: Arr_T := A;
   begin
      Sort(B);
      for Number of B loop
	 Ada.Text_IO.Put(Img(Number));
      end loop;
      Ada.Text_IO.New_Line;
   end Print_Sorted;

begin
   Print_Sorted((1, 34, 3, 98, 9, 76, 45, 4));
   Print_Sorted((54, 546, 548, 60));
end Largest_Int_From_List;
```



## Aime


```aime
largest(...)
{
    index x;
    for (, integer e in xcall(list).__list) {
        x[999999999 - 9.times(b_, data(), e).b_size(9).atoi] = e;
    }
    x.ucall(o_, 0);
    o_newline();
}

main(void)
{
    largest(1, 34, 3, 98, 9, 76, 45, 4);
    largest(54, 546, 548, 60);
    0;
}
```

works for input up to 999999999.
```txt
998764543431
6054854654
```



## ALGOL 68

Using method 2 - first sorting into first digit order and then comparing concatenated pairs.

```algol68
BEGIN
    # returns the integer value of s #
    OP TOINT = ( STRING s)INT:
    BEGIN
        INT result := 0;
        FOR s pos FROM LWB s TO UPB s DO
            result *:= 10 +:= ( ABS s[ s pos ] - ABS "0" )
        OD;
        result
    END # TOINT # ;
    # returns the first digit of n #
    OP FIRSTDIGIT = ( INT n )INT:
    BEGIN
        INT result := ABS n;
        WHILE result > 9 DO result OVERAB 10 OD;
        result
    END # FIRSTDIGIT # ;
    # returns a string representaton of n #
    OP TOSTRING = ( INT n )STRING: whole( n, 0 );
    # returns an array containing the values of a sorted such that concatenating the values would result in the largest value #
    OP CONCATSORT = ( []INT a )[]INT:
       IF LWB a >= UPB a THEN
           # 0 or 1 element(s) #
           a
       ELSE
           # 2 or more elements #
           [ 1 : ( UPB a - LWB a ) + 1 ]INT result := a[ AT 1 ];
           # sort the numbers into reverse first digit order #
           FOR o pos FROM UPB result - 1 BY -1 TO 1
           WHILE BOOL swapped := FALSE;
                 FOR i pos TO o pos DO
                     IF FIRSTDIGIT result[ i pos ] < FIRSTDIGIT result[ i pos + 1 ] THEN
                         INT t = result[ i pos + 1 ];
                         result[ i pos + 1 ] := result[ i pos ];
                         result[ i pos     ] := t;
                         swapped             := TRUE
                      FI
                 OD;
                 swapped
           DO SKIP OD;
           # now re-order adjacent numbers so they have the highest concatenated value #
           WHILE BOOL swapped := FALSE;
                 FOR i pos TO UPB result - 1 DO
                     STRING l := TOSTRING result[ i pos     ];
                     STRING r := TOSTRING result[ i pos + 1 ];
                     IF TOINT ( l + r ) < TOINT ( r + l ) THEN
                         INT t = result[ i pos + 1 ];
                         result[ i pos + 1 ] := result[ i pos ];
                         result[ i pos     ] := t;
                         swapped             := TRUE
                     FI
                 OD;
                 swapped
           DO SKIP OD;
           result
       FI # CONCATSORT # ;
    # prints the array a #
    OP PRINT = ( []INT a )VOID:
       FOR a pos FROM LWB a TO UPB a DO
            print( ( TOSTRING a[ a pos ] ) )
       OD # PRINT # ;

    # task test cases #
    PRINT CONCATSORT []INT( 1, 34, 3, 98, 9, 76, 45, 4 );
    print( ( newline ) );
    PRINT CONCATSORT []INT( 54, 546, 548, 60 );
    print( ( newline ) )

END
```

```txt

998764543431
6054854654

```



## AutoHotkey


```AutoHotkey
LargestConcatenatedInts(var){
	StringReplace, var, A_LoopField,%A_Space%,, all
	Sort, var, D`, fConcSort
	StringReplace, var, var, `,,, all
	return var
}

ConcSort(a, b){
	m := a . b	, n := b . a
    return m < n ? 1 : m > n ? -1 : 0
}
```

Examples:
```AutoHotkey
d =
(
1, 34, 3, 98, 9, 76, 45, 4
54, 546, 548, 60
4 , 45, 54, 5
)
loop, parse, d, `n
	MsgBox % LargestConcatenatedInts(A_LoopField)
```

```txt
998764543431
6054854654
554454
```



## AWK

```awk

function cmp(i1, v1, i2, v2, u1, u2) {
	u1 = v1""v2;
	u2 = v2""v1;
        return (u2 - u1)
}
function largest_int_from_concatenated_ints(X) {
 	PROCINFO["sorted_in"]="cmp";
	u="";
	for (i in X) u=u""X[i];
	return u
}

BEGIN {
	split("1 34 3 98 9 76 45 4",X);
	print largest_int_from_concatenated_ints(X)

	split("54 546 548 60",X);
	print largest_int_from_concatenated_ints(X)
}

```

```txt
998764543431
6054854654
```



## BBC BASIC


```bbcbasic
      DIM Nums%(10)
      Nums%()=1,34,3,98,9,76,45,4
      PRINT FNlargestint(8)
      Nums%()=54,546,548,60
      PRINT FNlargestint(4)
      END

      DEF FNlargestint(len%)
      LOCAL i%,l$,a$,b$,sorted%
      REPEAT
        sorted%=TRUE
        FOR i%=0 TO len%-2
          a$=STR$Nums%(i%)
          b$=STR$Nums%(i%+1)
          IF a$+b$<b$+a$ SWAP Nums%(i%),Nums%(i%+1):sorted%=FALSE
        NEXT
      UNTIL sorted%
      FOR i%=0 TO len%-1
        l$+=STR$Nums%(i%)
      NEXT
      =l$
```

```txt
998764543431
6054854654
```



## Bracmat


```bracmat
( ( maxnum
  =   A Z F C
    .   !arg:#
      |   !arg
        :   %@?F
            ?
            ( #%@?C
            & ( str$(!F !C)+-1*str$(!C !F):~<0
              | !C:?F
              )
            & ~
            )
            ?
      | !arg:?A !F ?Z&!F maxnum$(!A !Z)
  )
& out$(str$(maxnum$(1 34 3 98 9 76 45 4)))
& out$(str$(maxnum$(54 546 548 60)))
);
```

```txt
998764543431
6054854654
```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int catcmp(const void *a, const void *b)
{
	char ab[32], ba[32];
	sprintf(ab, "%d%d", *(int*)a, *(int*)b);
	sprintf(ba, "%d%d", *(int*)b, *(int*)a);
	return strcmp(ba, ab);
}

void maxcat(int *a, int len)
{
	int i;
	qsort(a, len, sizeof(int), catcmp);
	for (i = 0; i < len; i++)
		printf("%d", a[i]);
	putchar('\n');
}

int main(void)
{
	int x[] = {1, 34, 3, 98, 9, 76, 45, 4};
	int y[] = {54, 546, 548, 60};

	maxcat(x, sizeof(x)/sizeof(x[0]));
	maxcat(y, sizeof(y)/sizeof(y[0]));

	return 0;
}
```


```txt
998764543431
6054854654
```



## C++


```cpp
#include <iostream>
#include <sstream>
#include <algorithm>
#include <vector>
#include <string>

std::string findLargestConcat ( std::vector< int > & mynumbers ) {
   std::vector<std::string> concatnumbers ;
   std::sort ( mynumbers.begin( ) , mynumbers.end( ) ) ;
   do {
      std::ostringstream numberstream ;
      for ( int i : mynumbers )
	 numberstream << i ;
      concatnumbers.push_back( numberstream.str( ) ) ;
   } while ( std::next_permutation( mynumbers.begin( ) ,
	    mynumbers.end( ) )) ;
   return *( std::max_element( concatnumbers.begin( ) ,
	 concatnumbers.end( ) ) ) ;
}

int main( ) {
   std::vector<int> mynumbers = { 98, 76 , 45 , 34, 9 , 4 , 3 , 1 } ;
   std::vector<int> othernumbers = { 54 , 546 , 548 , 60 } ;
   std::cout << "The largest concatenated int is " <<
      findLargestConcat( mynumbers ) << " !\n" ;
   std::cout << "And here it is " << findLargestConcat( othernumbers )
      << " !\n" ;
   return 0 ;
}
```


```txt
The largest concatenated int is 998764543431 !
And here it is 6054854654 !
```


## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main(string[] args)
    {
        var source1 = new int[] { 1, 34, 3, 98, 9, 76, 45, 4 };
        var source2 = new int[] { 54, 546, 548, 60 };

        var largest1 = LargestPossibleSequence(source1);
        var largest2 = LargestPossibleSequence(source2);

        Console.WriteLine("The largest possible integer from set 1 is: {0}", largest1);
        Console.WriteLine("The largest possible integer from set 2 is: {0}", largest2);
    }

    static long LargestPossibleSequence(int[] ints)
    {
        return long.Parse(string.Join("", ints.OrderBy(i => i, new IntConcatenationComparer()).Reverse()));
    }
}

class IntConcatenationComparer : IComparer<int>
{
    public int Compare(int x, int y)
    {
        var xy = int.Parse(x.ToString() + y.ToString());
        var yx = int.Parse(y.ToString() + x.ToString());

        return xy - yx;
    }
}

```


```txt
The largest possible integer from set 1 is: 998764543431
The largest possible integer from set 2 is: 6054854654
```



## Ceylon

```ceylon
shared void run2() {

	function intConcatenationComparer(Integer x, Integer y) {
		assert(exists xy = parseInteger(x.string + y.string),
			exists yx = parseInteger(y.string + x.string));
		return yx <=> xy;
	}

	function biggestConcatenation(Integer* ints) => "".join(ints.sort(intConcatenationComparer));

	value test1 = {1, 34, 3, 98, 9, 76, 45, 4};
	value test2 = {54, 546, 548, 60};

	print("``biggestConcatenation(*test1)`` and ``biggestConcatenation(*test2)``");
}
```



## Clojure


```Clojure
(defn maxcat [coll]
  (read-string
    (apply str
           (sort (fn [x y]
                   (apply compare
                          (map read-string [(str y x) (str x y)])))
                 coll))))

(prn (map maxcat [[1 34 3 98 9 76 45 4] [54 546 548 60]]))
```


```txt
(998764543431 6054854654)
```



## Common Lisp

=== Sort by two-by-two comparison of largest concatenated result ===


```lisp

(defun int-concat (ints)
  (read-from-string (format nil "~{~a~}" ints)))

(defun by-biggest-result (first second)
  (> (int-concat  (list first second)) (int-concat (list second first))))

(defun make-largest-int (ints)
  (int-concat (sort ints #'by-biggest-result)))

```


```txt

> (make-largest-int '(1 34 3 98 9 76 45 4))
998764543431

> (make-largest-int '(54 546 548 60))
6054854654

```




###  Variation around the sort with padded most significant digit



```lisp

;; Sort criteria is by most significant digit with least digits used as a tie
;; breaker

(defun largest-msd-with-less-digits (x y)
  (flet ((first-digit (x)
           (digit-char-p (aref x 0))))
    (cond ((> (first-digit x)
              (first-digit y))
           t)
          ((> (first-digit y)
              (first-digit x))
           nil)
          ((and (= (first-digit x)
                   (first-digit y))
                (> (length x)
                   (length y)))
           nil)
          (t t))))

(loop
  :for input :in '((54 546 548 60) (1 34 3 98 9 76 45 4))
  :do (format t "~{~A~}~%"
              (sort (mapcar #'write-to-string input)
                    #'largest-msd-with-less-digits)))


```


```txt

6054548546
998764453341

```



## D

The three algorithms. Uses the second module from the Permutations Task.

```d
import std.stdio, std.algorithm, std.conv, std.array, permutations2;

auto maxCat1(in int[] arr) pure @safe {
    return arr.to!(string[]).permutations.map!join.reduce!max;
}

auto maxCat2(in int[] arr) pure nothrow @safe {
    return arr.to!(string[]).sort!q{b ~ a < a ~ b}.join;
}

auto maxCat3(in int[] arr) /*pure nothrow @safe*/ {
    immutable maxL = arr.reduce!max.text.length;
    return arr.to!(string[])
           .schwartzSort!(s => s.replicate(maxL/s.length + 1), "a > b")
           .join;
}

void main() {
    const lists = [[1, 34, 3, 98, 9, 76, 45, 4], [54, 546, 548, 60]];
    [&maxCat1, &maxCat2, &maxCat3].map!(cat => lists.map!cat).writeln;
}
```

```txt
[["998764543431", "6054854654"], ["998764543431", "6054854654"], ["998764543431", "6054854654"]]
```



## Elixir


```elixir
defmodule RC do
  def largest_int(list) do
    sorted = Enum.sort(list, fn x,y -> "#{x}#{y}" >= "#{y}#{x}" end)
    Enum.join(sorted)
  end
end

IO.inspect RC.largest_int [1, 34, 3, 98, 9, 76, 45, 4]
IO.inspect RC.largest_int [54, 546, 548, 60]
```


```txt

"998764543431"
"6054854654"

```



## Erlang


```Erlang

-module( largest_int_from_concatenated ).

-export( [ints/1, task/0] ).

ints( Ints ) ->
	Int_strings = [erlang:integer_to_list(X) || X <- Ints],
	Pad_ints = [{X ++ X, X} || X <- Int_strings],
	erlang:list_to_integer( lists:append([Int || {_Pad, Int} <- lists:reverse(lists:sort(Pad_ints))]) ).

task() ->
	[io:fwrite("Largest ~p from ~p~n", [ints(X), X]) || X <- [[1, 34, 3, 98, 9, 76, 45, 4], [54, 546, 548, 60]]].

```

```txt

8> largest_int_from_concatenated:task().
Largest 998764543431 from [1,34,3,98,9,76,45,4]
Largest 6054854654 from [54,546,548,60]

```


=={{header|F_Sharp|F#}}==

```fsharp

// Form largest integer which is a permutation from a list of integers. Nigel Galloway: March 21st., 2018
let fN g = List.map (string) g |> List.sortWith(fun n g->if n+g<g+n then 1 else -1) |> System.String.Concat

```

```txt

fN [1; 34; 3; 98; 9; 76; 45; 4] -> "998764543431"
fN [54; 546; 548; 60]           -> "6054854654"

```



## Factor

Using algorithm 3:

```factor
USING: assocs io kernel math qw sequences sorting ;
IN: rosetta-code.largest-int

: pad ( target seq -- padded )
    2dup length / swap <repetition> concat swap head ;

: largest-int ( seq -- )
    dup dup [ length ] map supremum    ! find longest length so we know how much to pad
    [ swap pad ] curry map             ! pad the integers
    <enum> sort-values                 ! sort the padded integers
    keys                               ! find the original indices of the sorted integers
    swap nths                          ! order non-padded integers according to their sorted order
    reverse concat print ;

qw{ 1 34 3 98 9 76 45 4 } qw{ 54 546 548 60 } [ largest-int ] bi@
```

```txt

998764543431
6054854654

```



## Fortran

There is often a potential ambiguity when reading numbers. While three definitely names the Platonic number notion, 3 might instead be regarded as being a text that happens to have the glyph of a number but is not a number. This sort of discussion arises when a spreadsheet has read in a text file and behold! numbers are on the display and they look just like what is displayed when numbers are being shown, but, they are ''not'' numbers, they are only drawn that way. Within the spreadsheet they are parts of some text, and the notion that takes over is one of a "blunt, heavy object", not alas close to hand.

So, the plan is to regard the numbers as being text sequences aligned to the left, containing only digit characters of course - except for the fact that CHARACTER variables often end up having trailing spaces. F2003 formalised a scheme whereby such variables can be "cut-to-fit" as execution proceeds but with earlier Fortrans the standard method is to pay attention to the number of characters in use. F90 introduced a function LEN_TRIM(text) to return the index of the last non-blank character in a text so the only problem now is to decide on how long might the largest number be (and by representing numbers as text strings, there is no difficulty with the limits of INTEGER*2 or INTEGER*4 ''etc.''), and what will be the maximum number of numbers. By devising a subroutine to do the work, these issues can be handled by the caller that is providing the data. The subroutine however intends to sort the collection of texts. This could be done by damaging its parameter which might be regarded as impolite or even unwanted so instead the sort is effected via an array XLAT and juggling its values. This has the advantage that the possibly large elements of the text array are not being moved about, but means that the subroutine must be able to have an XLAT array that is "large enough". F90 standardised the ability for a routine to declare such an array at run-time; previously, arrays within a subroutine (or indeed anywhere) had to have a size fixed at compilation time. In the past this might have been handled by the caller supplying such an array as an additional parameter.

Passing arrays as parameters can be tricky, especially for multi-dimensional arrays. This uses the old style whereby the size is left unstated via the * in <code>TEXT(*)</code>, though one could use <code>TEXT(N)</code> instead - but at the risk that the actual value of N is wrong and array index checking might be confused thereby. Still earlier one would simply place some integer value there, any valid integer, as in <code>TEXT(666)</code>, and not worry about bound checking at all because old-style compilers did not produce checking code even if it was wanted. F90 standardised the MODULE protocol, within which the size is specified as <code>TEXT(:)</code> whereby secret additional parameters are supplied that contain the actual bound information and bound checking will be correct, possibly not so if the <code>TEXT(N)</code> form is used instead and N is wrong. This extra overhead in every use is possibly better than undetected errors in some uses...

The sorting of the text array was to be by the notorious BubbleSort, taking advantage of the fact that each pass delivers the maximum value of the unsorted portion to its final position: the output could thereby be produced as the sort worked. Rather than mess about with early termination (no element being swapped) or attention to the bounds within which swapping took place, attention concentrated upon the comparison. Because of the left-alignment of the texts, a simple comparison seemed sufficient until I thought of unequal text lengths and then the following example. Suppose there are two numbers, 5, and one of 54, 55, or 56 as the other. Via normal comparisons, the 5 would always be first (because short texts are considered expanded with trailing spaces when compared against longer texts, and a space precedes every digit) however the biggest ordering is 5 54 for the first case but 56 5 for the last. This possibility is not exemplified in the specified trial sets. So, a more complex comparison is required. One could of course write a suitable function and consider the issue there but instead the comparison forms the compound text in the same manner as the result will be, in the two ways AB and BA, and looks to see which yields the bigger sequence. This need only be done for unequal length text pairs.

The source is F77 style, except for the declaration of XLAT(N), the use of <N> in the FORMAT statements instead of some large constant or similar, and the ability to declare an array via constants as in <code>(/"5","54"/)</code> rather than mess about declaring arrays and initialising them separately. The <code>I0</code> format code to convert a number (an actual number) into a digit string aligned leftwards in a CHARACTER variable of sufficient size is also a F90 introduction, though the B6700 compiler allowed a code <code>J</code> instead. This last is to demonstrate usage of actual numbers for those unpersuaded by the argument for ambiguity that allows for texts. If the <code>I0</code> format code is unavailable then <code>I9</code> (or some suitable size) could be used, followed by <code>text = ADJUSTL(text)</code>, except that this became an intrinsic function only in F90, so perhaps you will have to write a simple alignment routine.
```Fortran
      SUBROUTINE SWAP(A,B)	!Why can't the compiler supply these!
       INTEGER A,B,T
        T = B
        B = A
        A = T
      END

      SUBROUTINE BIGUP(TEXT,N)	!Outputs the numbers in TEXT to give the biggest number.
       CHARACTER*(*) TEXT(*)	!The numbers as text, aligned left.
       INTEGER N		!The number of them.
       INTEGER XLAT(N),L(N)	!An index and a set of lengths.
       INTEGER I,J,M		!Assorted steppers.
       INTEGER TI,TJ		!Fingers to a text.
       INTEGER LI,LJ		!Lengths of the fingered texts.
       INTEGER MSG		!I/O unit number.
       COMMON /IODEV/ MSG	!Old style.
        DO I = 1,N	!Step through my supply of texts.
          XLAT(I) = I		!Preparing a finger to them.
          L(I) = LEN_TRIM(TEXT(I))	!And noting their last non-blank.
        END DO		!On to the next.
        WRITE (MSG,1) "Supplied",(TEXT(I)(1:L(I)), I = 1,N)	!Show the grist.
    1   FORMAT (A12,":",<N>(A,","))	!Instead of <N>, 666 might suffice.
Crude bubblesort. No attempt at noting the bounds of swaps made.
        DO M = N,1,-1	!Just for fun, go backwards.
          DO I = 2,M		!Start a scan.
            J = I - 1		!Comparing element I to element I - 1.
            TI = XLAT(I)	!Thus finger the I'th text in XLAT order.
            TJ = XLAT(J)	!And its supposed predecessor.
            LI = L(TI)		!The length of the fingered text.
            LJ = L(TJ)		!All this to save on typing below.
            IF (LI .EQ. LJ) THEN	!If the texts are equal lengths,
              IF (TEXT(TI).LT.TEXT(TJ)) CALL SWAP(XLAT(I),XLAT(J))	!A simple comparison.
             ELSE	!But if not, construct the actual candidate texts for comparison.
              IF (TEXT(TI)(1:LI)//TEXT(TJ)(1:LJ)	!These two will be the same length.
     1        .LT.TEXT(TJ)(1:LJ)//TEXT(TI)(1:LI))	!Just as above.
     2        CALL SWAP(XLAT(I),XLAT(J))	!J shall now follow I.
            END IF			!So much for that comparison.
          END DO		!On to the next.
        END DO	!The original plan was to reveal element XLAT(M) as found.
        WRITE (MSG,2) "Biggest",(TEXT(XLAT(I))(1:L(XLAT(I))),I = N,1,-1)	!But, all at once is good too.
    2   FORMAT (A12,":",<N>(A," "))	!The space maintains identity.
      END	!That was fun.

      PROGRAM POKE
      CHARACTER*4 T1(10)	!Prepare some example arrays.
      CHARACTER*4 T2(4)		!To hold the specified examples.
      INTEGER MSG
      COMMON /IODEV/ MSG
      DATA T1(1:8)/"1","34","3","98","9","76","45","4"/
      DATA T2/"54","546","548","60"/
      MSG = 6		!Standard output.
      WRITE (MSG,1)
    1 FORMAT ("Takes a list of integers and concatenates them so as ",
     1 "to produce the biggest possible number.",/,
     2 "The result is shown with spaces between the parts ",
     3 "to show provenance. Ignore them otherwise."/)
      CALL BIGUP(T1,8)

      WRITE (MSG,*)
      CALL BIGUP(T2,4)

      WRITE (MSG,*) "These are supplied in lexicographical order..."
      CALL BIGUP((/"5","54"/),2)

      WRITE (MSG,*) "But this is not necessarily the biggest order."
      CALL BIGUP((/"5","56"/),2)

      WRITE (MSG,*) "And for those who count..."
      DO I = 1,10
        WRITE (T1(I),"(I0)") I	!This format code produces only the necessary text.
      END DO			!Thus, the numbers are aligned left in the text field.
      CALL BIGUP(T1,10)
      END
```

Output: the Fortran compiler ignores spaces when reading fortran source, so, hard-core fortranners should have no difficulty doing likewise for the output...

```txt

Takes a list of integers and concatenates them so as to produce the biggest possible number.
The result is shown with spaces between the parts to show provenance. Ignore them otherwise.

    Supplied:1,34,3,98,9,76,45,4,
     Biggest:9 98 76 45 4 34 3 1

    Supplied:54,546,548,60,
     Biggest:60 548 546 54
 These are supplied in lexicographical order...
    Supplied:5,54,
     Biggest:5 54
 But this is not necessarily the biggest order.
    Supplied:5,56,
     Biggest:56 5
 And for those who count...
    Supplied:1,2,3,4,5,6,7,8,9,10,
     Biggest:9 8 7 6 5 4 3 2 1 10

```



## FreeBASIC


```freebasic
' version 17-01-2016
' compile with: fbc -s console

' TRUE/FALSE are built-in constants since FreeBASIC 1.04
' But we have to define them for older versions.
#Ifndef TRUE    ' if TRUE is not defined then
   #Define FALSE 0
   #Define TRUE Not FALSE
#EndIf

Dim As Integer array()

Function largest(array() As Integer) As String

    Dim As Integer lb = LBound(array), ub = UBound(array)
    Dim As Integer i, flag
    Dim As String a_str(lb To ub),tmp

    For i = lb To ub
        a_str(i) = Left(Str(array(i)) & String(14, " "), 14)
    Next

    Do
        flag = TRUE
        For i = lb To ub - 1
            If a_str(i) < a_str(i+1) Then
                Swap a_str(i), a_str(i +1)
                flag = FALSE
            End If
        Next
        If flag = TRUE Then Exit Do
    Loop

    For i = lb To ub
        tmp += Trim(a_str(i))
    Next

    Return tmp

End Function

' ------=< MAIN >=------

Data 1, 34, 3, 98, 9, 76, 45, 4, -999
Data 54, 546, 548, 60, -999
Data -999

Dim As Integer i, x

Do
    ReDim array(1 To 1)
    i = 1
    Do
        Read x
        If x = -999 Then Exit Do
        If i > 1 Then
            ReDim Preserve array(1 To i)
        End If
        array(i) = x
        i += 1
    Loop
    If i = 1 Then Exit Do
    Print "Largest concatenated int from {";
    For i = lBound(array) To UBound(array)
        Print Str(array(i));
        If i = UBound(array) Then
            Print "} = "; largest(array())
        Else
            Print ",";
        End If
    Next
Loop

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
Largest concatenated int from {1,34,3,98,9,76,45,4} = 989764543431
Largest concatenated int from {54,546,548,60} = 6054854654
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=4169e7641f29ff0ae1dd202b459e60ce Click this link to run this code]'''

```gambas
'Largest int from concatenated ints

Public Sub Main()
Dim iList1 As Integer[] = [1, 34, 3, 98, 9, 76, 45, 4]      'Integer list 1
Dim iList2 As Integer[] = [54, 546, 548, 60]                'Integer list 2

Calc(iList1)                                                'Send List 1 to Calc routine
Calc(iList2)                                                'Send List 2 to Calc routine

End
'_________________________________________________________________________________________

Public Sub Calc(iList As Integer[])
Dim siCount1, siCount2, siCounter As Short                  'Counters
Dim sList As New String[]                                   'To hold converted integers
Dim bTrigger As Boolean                                     'To trigger a found match

For Each siCount1 In iList                                  'For each integer in the list..
  sList.Add(Str(siCount1))                                  'Convert to a string and add to sList
  If Len(Str(siCount1)) > siCounter Then                    'If the length of the string is greater than siCounter then..
    siCounter = Len(Str(siCount1))                          'siCounter = length of the string
  End If
Next

For siCount1 = 0 To sList.Max                               'For each item in sList
  If Len(sList[siCount1]) < siCounter Then                  'If the length of the string is less that siCounter then..
    sList[siCount1] &= Right(sList[siCount1], 1)            'Add the same digit to the string e.g. in list 1 "9" becomes "99", list 2 "54" becomes "544"
  End If
Next

sList.Sort(gb.Descent)                                      'Sort the list in decending order

For siCount1 = 0 To sList.Max                               'For each item in sList
  bTrigger = False                                          'Set bTrigger to False
  For siCount2 = 0 To iList.Max                             'Loop through each item in iList
    If Val(sList[siCount1]) = iList[siCount2] Then          'If the value of each is the same e.g. "98" = 98 then
      bTrigger = True                                       'Set bTrigger to True
      Continue                                              'Exit the loop
    Endif
  Next
  If Not bTrigger Then                                      'If there was no match e.g. there is no "99" then..
    sList[siCount1] = Left(sList[siCount1], siCounter - 1)  'Strip out the end digit e.g. "99" becomes 9 again
  End If
Next

Print Val(sList.Join(""))                                   'Join all items in sList together and print

End
```

Output:

```txt

998764543431
6054854654

```



## Go


```go
// Variation of method 3.  Repeat digits to at least the size of the longest,
// then sort as strings.
package main

import (
    "fmt"
    "math/big"
    "sort"
    "strconv"
    "strings"
)

type c struct {
    i     int
    s, rs string
}

type cc []*c

func (c cc) Len() int           { return len(c) }
func (c cc) Less(i, j int) bool { return c[j].rs < c[i].rs }
func (c cc) Swap(i, j int)      { c[i], c[j] = c[j], c[i] }

// Function required by task.  Takes a list of integers, returns big int.
func li(is ...int) *big.Int {
    ps := make(cc, len(is))
    ss := make([]c, len(is))
    ml := 0
    for j, i := range is {
        p := &ss[j]
        ps[j] = p
        p.i = i
        p.s = strconv.Itoa(i)
        if len(p.s) > ml {
            ml = len(p.s)
        }
    }
    for _, p := range ps {
        p.rs = strings.Repeat(p.s, (ml+len(p.s)-1)/len(p.s))
    }
    sort.Sort(ps)
    s := make([]string, len(ps))
    for i, p := range ps {
        s[i] = p.s
    }
    b, _ := new(big.Int).SetString(strings.Join(s, ""), 10)
    return b
}

func main() {
    fmt.Println(li(1, 34, 3, 98, 9, 76, 45, 4))
    fmt.Println(li(54, 546, 548, 60))
}
```

```txt

998764543431
6054854654

```



## Go


```go
// Variation of method 3.  Repeat digits to at least the size of the longest,
// then sort as strings.
package main

import (
    "fmt"
    "math/big"
    "sort"
    "strconv"
    "strings"
)

type c struct {
    i     int
    s, rs string
}

type cc []*c

func (c cc) Len() int           { return len(c) }
func (c cc) Less(i, j int) bool { return c[j].rs < c[i].rs }
func (c cc) Swap(i, j int)      { c[i], c[j] = c[j], c[i] }

// Function required by task.  Takes a list of integers, returns big int.
func li(is ...int) *big.Int {
    ps := make(cc, len(is))
    ss := make([]c, len(is))
    ml := 0
    for j, i := range is {
        p := &ss[j]
        ps[j] = p
        p.i = i
        p.s = strconv.Itoa(i)
        if len(p.s) > ml {
            ml = len(p.s)
        }
    }
    for _, p := range ps {
        p.rs = strings.Repeat(p.s, (ml+len(p.s)-1)/len(p.s))
    }
    sort.Sort(ps)
    s := make([]string, len(ps))
    for i, p := range ps {
        s[i] = p.s
    }
    b, _ := new(big.Int).SetString(strings.Join(s, ""), 10)
    return b
}

func main() {
    fmt.Println(li(1, 34, 3, 98, 9, 76, 45, 4))
    fmt.Println(li(54, 546, 548, 60))
}
```

```txt

998764543431
6054854654

```



## Groovy


```groovy
def largestInt = { c -> c.sort { v2, v1 -> "$v1$v2" <=> "$v2$v1" }.join('') as BigInteger }
```

Testing:

```groovy
assert largestInt([1, 34, 3, 98, 9, 76, 45, 4]) == 998764543431
assert largestInt([54, 546, 548, 60]) == 6054854654
```



## Haskell


### Compare repeated string method


```Haskell
import Data.List (sortBy)
import Data.Ord (comparing)

main = print (map maxcat [[1,34,3,98,9,76,45,4], [54,546,548,60]] :: [Integer])
    where
      sorted xs = let pad x  = concat $ replicate (maxLen `div` length x + 1) x
                      maxLen = maximum $ map length xs
                  in  sortBy (flip $ comparing pad) xs

      maxcat = read . concat . sorted . map show
```


```txt
[998764543431,6054854654]
```


Since repeating numerical string "1234" is the same as taking all the digits of 1234/9999 after the decimal point, the following does essentially the same as above:

```haskell
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Ratio ((%))

nines = iterate ((+9).(*10)) 9

maxcat = foldl (\a (n,d)->a * (1 + d) + n) 0 .
    sortBy (flip $ comparing $ uncurry (%)) .
    map (\a->(a, head $ dropWhile (<a) nines))

main = mapM_ (print.maxcat) [[1,34,3,98,9,76,45,4], [54,546,548,60]]
```



### Sort on comparison of concatenated ints method


```Haskell
import Data.List (sortBy)

main = print (map maxcat [[1,34,3,98,9,76,45,4], [54,546,548,60]] :: [Integer])
    where sorted = sortBy (\a b -> compare (b++a) (a++b))
          maxcat = read . concat . sorted . map show
```


;Output as above.


### Try all permutations method


```Haskell
import Data.List (permutations)

main :: IO ()
main =
  print
    (maxcat <$> [[1, 34, 3, 98, 9, 76, 45, 4], [54, 546, 548, 60]] :: [Integer])
  where
    maxcat = read . maximum . fmap (concatMap show) . permutations
```


;Output as above.

=={{header|Icon}} and {{header|Unicon}}==

This solution only works in Unicon as it uses a Heap class to do the heavy
lifting.


```unicon
import Collections    # For the Heap (dense priority queue) class

procedure main(a)
    write(lici(a))
end

procedure lici(a)
    every (result := "") ||:= Heap(a,,cmp).gen()
    return result
end

procedure cmp(a,b)
   return (a||b) > (b||a)
end
```


Sample runs:

```txt

->lici 1 34 3 98 9 76 45 4
998764543431
->lici 54 546 548 60
6054854654
->

```



## J

'''Solution:'''

```j>maxlen=: [:
./ #&>
maxnum=: (0 ". ;)@(\: maxlen $&> ])@(8!:0)
```

'''Usage:'''

```j
   maxnum&> 1 34 3 98 9 76 45 4 ; 54 546 548 60
998764543431 6054854654
```



## Java

This example sets up a comparator to order the numbers using <code>Collections.sort</code> as described in method #3 (padding and reverse sorting).
It was also necessary to make a join method to meet the output requirements.

```java5
import java.util.*;

public class IntConcat {

    private static Comparator<Integer> sorter = new Comparator<Integer>(){
        @Override
        public int compare(Integer o1, Integer o2){
            String o1s = o1.toString();
            String o2s = o2.toString();

            if(o1s.length() == o2s.length()){
                return o2s.compareTo(o1s);
            }

            int mlen = Math.max(o1s.length(), o2s.length());
            while(o1s.length() < mlen * 2) o1s += o1s;
            while(o2s.length() < mlen * 2) o2s += o2s;

            return o2s.compareTo(o1s);
        }
    };

    public static String join(List<?> things){
        String output = "";
        for(Object obj:things){
            output += obj;
        }
        return output;
    }

    public static void main(String[] args){
        List<Integer> ints1 = new ArrayList<Integer>(Arrays.asList(1, 34, 3, 98, 9, 76, 45, 4));

        Collections.sort(ints1, sorter);
        System.out.println(join(ints1));

        List<Integer> ints2 = new ArrayList<Integer>(Arrays.asList(54, 546, 548, 60));

        Collections.sort(ints2, sorter);
        System.out.println(join(ints2));
    }
}
```

```java5
import java.util.Comparator;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public interface IntConcat {
  public static Comparator<Integer> SORTER = (o1, o2) -> {
    String o1s = o1.toString();
    String o2s = o2.toString();

    if (o1s.length() == o2s.length()) {
      return o2s.compareTo(o1s);
    }

    int mlen = Math.max(o1s.length(), o2s.length());
    while (o1s.length() < mlen * 2) {
      o1s += o1s;
    }
    while (o2s.length() < mlen * 2) {
      o2s += o2s;
    }

    return o2s.compareTo(o1s);
  };

  public static void main(String[] args) {
    Stream<Integer> ints1 = Stream.of(
      1, 34, 3, 98, 9, 76, 45, 4
    );

    System.out.println(ints1
      .parallel()
      .sorted(SORTER)
      .map(String::valueOf)
      .collect(Collectors.joining())
    );

    Stream<Integer> ints2 = Stream.of(
      54, 546, 548, 60
    );

    System.out.println(ints2
      .parallel()
      .sorted(SORTER)
      .map(String::valueOf)
      .collect(Collectors.joining())
    );
  }
}
```

```txt
998764543431
6054854654
```


## JavaScript



### ES5



```JavaScript
 (function () {
     'use strict';

     // maxCombine :: [Int] -> Int
     function maxCombine(xs) {
         return parseInt(
             xs.sort(
                 function (x, y) {
                     var a = x.toString(),
                         b = y.toString(),
                         ab = parseInt(a + b),
                         ba = parseInt(b + a);

                     return ab > ba ? -1 : (ab < ba ? 1 : 0);
                 }
             )
             .join(''), 10
         );
     }

     return [
        [1, 34, 3, 98, 9, 76, 45, 4],
        [54, 546, 548, 60]
     ].map(maxCombine);

 })();

```


```txt
[998764543431, 6054854654]
```





### ES6


```JavaScript
var maxCombine = (a) => +(a.sort((x, y) => +("" + y + x) - +("" + x + y)).join(''));

// test & output
console.log([
  [1, 34, 3, 98, 9, 76, 45, 4],
  [54, 546, 548, 60]
].map(maxCombine));
```



## jq

### = Padding  =

''For jq versions greater than 1.4, it may be necessary to change "sort_by" to "sort".''

```jq
def largest_int:

  def pad(n):  . + (n - length) * .[length-1:];

  map(tostring)
  | (map(length) | max) as $max
  | map([., pad($max)])
  | sort_by( .[1] )
  | map( .[0] ) | reverse | join("") ;

# Examples:
([1, 34, 3, 98, 9, 76, 45, 4],
 [54, 546, 548, 60])  | largest_int

```

 $ /usr/local/bin/jq -n -M -r -f Largest_int_from_concatenated_ints.jq
 998764543431
 6054854654


### =Custom Sort=

The following uses [[Sort_using_a_custom_comparator#jq| quicksort/1]]:

```jq
def largest_int:
  map(tostring)
  | quicksort( .[0] + .[1] < .[1] + .[0] )
  | reverse | join("") ;
```



## Julia

Perhaps algorithm 3 is more efficient, but algorithm 2 is decent and very easy to implement in Julia.  So this solution uses algorithm 2.


```julia
function maxconcat(arr::Vector{<:Integer})
    b = sort(dec.(arr); lt=(x, y) -> x * y < y * x, rev=true) |> join
    return try parse(Int, b) catch parse(BigInt, b) end
end

tests = ([1, 34, 3, 98, 9, 76, 45, 4],
         [54, 546, 548, 60],
         [1, 34, 3, 98, 9, 76, 45, 4, 54, 546, 548, 60])

for arr in tests
    println("Max concatenating in $arr:\n -> ", maxconcat(arr))
end
```


```txt
Max concatenating in [1, 34, 3, 98, 9, 76, 45, 4]:
 -> 998764543431
Max concatenating in [54, 546, 548, 60]:
 -> 6054854654
Max concatenating in [1, 34, 3, 98, 9, 76, 45, 4, 54, 546, 548, 60]:
 -> 9987660548546544543431
```



## Kotlin

```scala
import java.util.Comparator

fun main(args: Array<String>) {
    val comparator = Comparator<Int> { x, y ->
        val xy = (x.toString() + y).toInt()
        val yx = (y.toString() + x).toInt()
        xy.compareTo(yx)
    }

    fun findLargestSequence(array: IntArray): String {
        return array.sortedWith(comparator).reversed().map { it.toString() }.joinToString("")
    }

    val source1 = intArrayOf(1, 34, 3, 98, 9, 76, 45, 4)
    println(findLargestSequence(source1))

    val source2 = intArrayOf(54, 546, 548, 60)
    println(findLargestSequence(source2))
}
```

 998764543431
 6054854654


## Lua


```Lua
function icsort(numbers)
	table.sort(numbers,function(x,y) return (x..y) > (y..x) end)
	return numbers
end

for _,numbers in pairs({{1, 34, 3, 98, 9, 76, 45, 4}, {54, 546, 548, 60}}) do
	print(('Numbers: {%s}\n  Largest integer: %s'):format(
		table.concat(numbers,","),table.concat(icsort(numbers))
	))
end
```

```txt
Numbers: {1,34,3,98,9,76,45,4}
  Largest integer: 998764543431
Numbers: {54,546,548,60}
  Largest integer: 6054854654
```



## Mathematica


```Mathematica
makeLargestInt[list_] := Module[{sortedlist},
  sortedlist = Sort[list, Order[ToString[#1] <> ToString[#2], ToString[#2] <> ToString[#1]] < 0 &];
  Map[ToString, sortedlist] // StringJoin // FromDigits
  ]
(* testing with two examples *)
makeLargestInt[{1, 34, 3, 98, 9, 76, 45, 4}]
makeLargestInt[{54, 546, 548, 60}]
```

```txt
998764543431
6054854654
```





## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method largestInt(il) public static
  ri = ''
  wa = ''
  -- put the list into an indexed string
  wa[0] = il.words
  loop ww = 1 to wa[0]
    wa[ww] = il.word(ww)
    end ww

  -- order the list
  loop wx = 1 to wa[0] - 1
    loop wy = wx + 1 to wa[0]
      xx = wa[wx]
      yy = wa[wy]
      xy = xx || yy
      yx = yy || xx
      if xy < yx then do
        -- swap xx and yy
        wa[wx] = yy
        wa[wy] = xx
        end
      end wy
    end wx

  -- rebuild list from indexed string
  loop ww = 1 to wa[0]
    ri = ri wa[ww]
    end ww
  return ri.space(0) -- concatenate the list elements into a single numeric

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  ints = [ -
    '1 34 3 98 9 76 45 4', -
    '54 546 548 60' -
    ]
  loop il over ints
    say largestInt(il).right(20) ':' il.space(1, ',')
    end il
  return

```

```txt

        998764543431 : 1,34,3,98,9,76,45,4
          6054854654 : 54,546,548,60

```



## Nim


```nim
import algorithm, sequtils, strutils, future

proc maxNum(x: seq[int]): string =
  var c = x.mapIt(string, $it)
  c.sort((x, y) => cmp(y&x, x&y))
  c.join()

echo maxNum(@[1, 34, 3, 98, 9, 76, 45, 4])
echo maxNum(@[54, 546, 548, 60])
```

```txt
998764543431
6054854654
```



## OCaml


```ocaml
let myCompare a b = compare (b ^ a) (a ^ b)
let icsort nums = String.concat "" (List.sort myCompare (List.map string_of_int nums))
```


;testing


```txt

# icsort [1;34;3;98;9;76;45;4];;
- : string = "998764543431"
# icsort [54;546;548;60];;
- : string = "6054854654"

```



## Oforth



```Oforth
: largestInt  map(#asString) sortWith(#[ 2dup + -rot swap + > ]) sum asInteger ;
```


```txt

[ [1, 34, 3, 98, 9, 76, 45, 4], [54, 546, 548, 60] ] map(#largestInt) .
[998764543431, 6054854654]

```



## Pascal

tested with freepascal.Used a more extreme example 3.

### algorithm 3


```pascal
const
  base    = 10;
  MaxDigitCnt = 11;
  source1 : array[0..7] of integer = (1, 34, 3, 98, 9, 76, 45, 4);
  source2 : array[0..3] of integer = (54,546,548,60);
  source3 : array[0..3] of integer = (60, 54,545454546,0);

type
  tdata = record
            datOrg,
            datMod : LongWord;
            datStrOrg       : string[MaxDigitCnt];
          end;
  tArrData = array of tData;

procedure DigitCount(var n: tdata);
begin
  with n do
    //InttoStr is very fast
    str(datOrg,datStrOrg);

end;

procedure InsertData(var n: tdata;data:LongWord);
begin
  n.datOrg := data;
  DigitCount(n);
end;

function FindMaxLen(const ArrData:tArrData): LongWord;
var
  cnt : longInt;
  res,t : LongWord;
begin
  res := 0;// 1 is minimum
  for cnt :=  High(ArrData) downto Low(ArrData) do
  begin
    t := length(ArrData[cnt].datStrOrg);
    IF res < t then
      res := t;
  end;
  FindMaxLen := res;
end;

procedure ExtendCount(var ArrData:tArrData;newLen: integer);
var
  cnt,
  i,k : integer;
begin
  For cnt := High(ArrData) downto Low(ArrData) do
    with ArrData[cnt] do
    begin
      datMod := datOrg;
      i := newlen-length(datStrOrg);
      k := 1;
      while i > 0 do
      begin
        datMod := datMod *Base+Ord(datStrOrg[k])-Ord('0');
        inc(k);
        IF k >length(datStrOrg) then
          k := 1;
        dec(i);
      end;
    end;
end;

procedure SortArrData(var ArrData:tArrData);
var
  i,
  j,idx : integer;
  tmpData : tData;
begin
  For i := High(ArrData) downto Low(ArrData)+1 do
  begin
    idx := i;
    j := i-1;
    For j := j downto Low(ArrData) do
      IF ArrData[idx].datMod < ArrData[j].datMod then
         idx := j;
    IF idx <> i then
    begin
      tmpData     := ArrData[idx];
      ArrData[idx]:= ArrData[i];
      ArrData[i]  := tmpData;
    end;
  end;
end;

procedure ArrDataOutput(const ArrData:tArrData);
var
  i,l : integer;
  s : string;
begin
{ the easy way
  For i := High(ArrData) downto Low(ArrData) do
    write(ArrData[i].datStrOrg);
  writeln;
  *}
  l := 0;
  For i := High(ArrData) downto Low(ArrData) do
    inc(l,length(ArrData[i].datStrOrg));
  setlength(s,l);
  l:= 1;
  For i := High(ArrData) downto Low(ArrData) do
    with ArrData[i] do
    begin
      move(datStrOrg[1],s[l],length(datStrOrg));
      inc(l,length(datStrOrg));
    end;
  writeln(s);
end;

procedure HighestInt(var  ArrData:tArrData);
begin
  ExtendCount(ArrData,FindMaxLen(ArrData));
  SortArrData(ArrData);
  ArrDataOutput(ArrData);
end;

var
  i : integer;
  tmpData : tArrData;
begin
  // Source1
  setlength(tmpData,length(source1));
  For i := low(tmpData) to high(tmpData) do
    InsertData(tmpData[i],source1[i]);
  HighestInt(tmpData);
  // Source2
  setlength(tmpData,length(source2));
  For i := low(tmpData) to high(tmpData) do
    InsertData(tmpData[i],source2[i]);
  HighestInt(tmpData);
  // Source3
  setlength(tmpData,length(source3));
  For i := low(tmpData) to high(tmpData) do
    InsertData(tmpData[i],source3[i]);
  HighestInt(tmpData);
end.
```

```txt
998764543431
6054854654
60545454546540
```


### =Inspired by Haskell =

generate the repetition by dividing /(10^CountDigits-1)
http://rosettacode.org/wiki/Largest_int_from_concatenated_ints#Compare_repeated_string_method


```pascal
const
  base    = 10;
  MaxDigitCnt = 11;
  source1 : array[0..7] of LongInt = (10 , 34, 3, 98, 9, 76, 45, 4);
  source2 : array[0..3] of LongInt = (54,546,548,60);
  source3 : array[0..3] of LongInt = (0,2121212122,21,60);

type
  tdata = record
            datMod : double;
            datOrg : LongInt;
//InttoStr is very fast and the string is always needed
            datStrOrg       : string[MaxDigitCnt];
          end;
  tArrData = array of tData;

procedure InsertData(var n: tdata;data:LongWord);
begin
  with n do
  begin
    datOrg := data;
    str(datOrg,datStrOrg);
  end;
end;

function FindMaxLen(const ArrData:tArrData): LongWord;
var
  cnt : longInt;
  res,t : LongWord;
begin
  res := 0;// 1 is minimum
  for cnt :=  High(ArrData) downto Low(ArrData) do
  begin
    t := length(ArrData[cnt].datStrOrg);
    IF res < t then
      res := t;
  end;
  FindMaxLen := res;
end;

procedure ExtendData(var ArrData:tArrData;newLen: integer);
var
  cnt,
  i : integer;
begin
  For cnt := High(ArrData) downto Low(ArrData) do
    with ArrData[cnt] do
    begin
      //generating 10^length(datStrOrg)
      datMod := 1;
      i := length(datStrOrg);
      // i always >= 1
      repeat
        datMod := base*datMod;
        dec(i);
      until i <= 0;
//      1/(datMod-1.0) = 1/(9...9)
      datMod := datOrg/(datMod-1.0)+datOrg;
      i := newlen-length(datStrOrg);
      For i := i downto 1 do
        datMod := datMod*Base;
    end;
end;

procedure SortArrData(var ArrData:tArrData);
//selection sort
var
  i,
  j,idx : integer;
  tmpData : tData;
begin
  For i := High(ArrData) downto Low(ArrData)+1 do
  begin
    idx := i;
    j := i-1;
    //select max
    For j := j downto Low(ArrData) do
      IF ArrData[idx].datMod < ArrData[j].datMod then
         idx := j;
    //finally swap
    IF idx <> i then
    begin
      tmpData     := ArrData[idx];
      ArrData[idx]:= ArrData[i];
      ArrData[i]  := tmpData;
    end;
  end;
end;

procedure ArrDataOutput(const ArrData:tArrData);
var
  i : integer;
begin
{ the easy way}
  For i := High(ArrData) downto Low(ArrData) do
    write(ArrData[i].datStrOrg);
  writeln;
end;

procedure HighestInt(var  ArrData:tArrData);
begin
  ExtendData(ArrData,FindMaxLen(ArrData));
  SortArrData(ArrData);
  ArrDataOutput(ArrData);
end;

var
  i : integer;
  tmpData : tArrData;
begin
  // Source1
  setlength(tmpData,length(source1));
  For i := low(tmpData) to high(tmpData) do
    InsertData(tmpData[i],source1[i]);
  HighestInt(tmpData);
  // Source2
  setlength(tmpData,length(source2));
  For i := low(tmpData) to high(tmpData) do
    InsertData(tmpData[i],source2[i]);
  HighestInt(tmpData);
  // Source3
  setlength(tmpData,length(source3));
  For i := low(tmpData) to high(tmpData) do
    InsertData(tmpData[i],source3[i]);
  HighestInt(tmpData);
end.
```

```txt
9987645434310
6054854654
602121212122210>
```



## PARI/GP

Sorts then joins. Most of the noise comes from converting a vector of integers into a concatenated integer: <code>eval(concat(apply(n->Str(n),v)))</code>. Note that the short form <code>eval(concat(apply(Str,v)))</code> is not valid here because <code>Str</code> is variadic.


```parigp
large(v)=eval(concat(apply(n->Str(n),vecsort(v,(x,y)->eval(Str(y,x,"-",x,y))))));
large([1, 34, 3, 98, 9, 76, 45, 4])
large([54, 546, 548, 60])
```

```txt
%1 = 998764543431
%2 = 6054854654
```



## Perl


```perl
sub maxnum {
    join '', sort { "$b$a" cmp "$a$b" } @_
}

print maxnum(1, 34, 3, 98, 9, 76, 45, 4), "\n";
print maxnum(54, 546, 548, 60), "\n";
```

```txt
998764543431
6054854654
```



## Perl 6


```perl6
sub maxnum(*@x) {
    [~] @x.sort: -> $a, $b { $b ~ $a leg $a ~ $b }
}

say maxnum <1 34 3 98 9 76 45 4>;
say maxnum <54 546 548 60>;
```

```txt
998764543431
6054854654
```



## Phix


```Phix
function catcmp(string a, string b)
    return compare(b&a,a&b)
end function

function method2(sequence s)
    for i=1 to length(s) do
        s[i] = sprintf("%d",s[i])
    end for
    s = custom_sort(routine_id("catcmp"),s)
    return join(s,"")
end function

? method2({1,34,3,98,9,76,45,4})
? method2({54,546,548,60})
```

```txt

"998764543431"
"6054854654"

```



## PHP


```php
function maxnum($nums) {
    usort($nums,  function ($x, $y) { return strcmp("$y$x", "$x$y"); });
    return implode('', $nums);
}

echo maxnum(array(1, 34, 3, 98, 9, 76, 45, 4)), "\n";
echo maxnum(array(54, 546, 548, 60)), "\n";
```

```txt
998764543431
6054854654
```



## PicoLisp

Here are solutions for all three algorithms.

The third solution actually avoids padding the numbers, by converting them into
circular lists and comparing these. As a drawback, however, this works only for
unique lists (as the comparison of identical numbers would not terminate), so a
better solution might involve additional checks.

```PicoLisp
(load "@lib/simul.l")  # For 'permute'
```


### Algorithm 1


```PicoLisp
(for L '((1 34 3 98 9 76 45 4) (54 546 548 60))
   (prinl (maxi format (permute L))) )
```


### Algorithm 2


```PicoLisp
(for L '((1 34 3 98 9 76 45 4) (54 546 548 60))
   (prinl
      (sort L
         '((A B)
            (>
               (format (pack A B))
               (format (pack B A)) ) ) ) ) )
```


### Algorithm 3


```PicoLisp
(for L '((1 34 3 98 9 76 45 4) (54 546 548 60))
   (prinl
      (flip
         (by '((N) (apply circ (chop N))) sort L) ) ) )
```

{{out}} in all three cases:

```txt
998764543431
6054854654
```



## PL/I


```pli

/* Largest catenation of integers            16 October 2013 */
/* Sort using method 2, comparing pairs of adjacent integers. */

Largest: procedure options (main);
   declare s(*) char (20) varying controlled, n fixed binary;
   get (n);
   allocate s(n);
   get list (s);
   s = trim(s);
   put skip edit (s) (a, x(1));
   put skip list ('Largest integer=', Largest_integer());

largest_integer: procedure () returns (char(100) varying);
   declare sorted bit (1);
   declare (true value ('1'b), false value ('0'b)) bit (1);
   declare i fixed binary;
   declare temp character(20) varying;

   do until (sorted);
      sorted = true;
      do i = 1 to n-1;
         if char(s(i)) || char(s(i+1)) < char(s(i+1)) || char(s(i)) then
            do;
               temp = s(i); s(i) = s(i+1); s(i+1) = temp; sorted = false;
            end;
      end;
   end;
   return (string(s));
end largest_integer;
end Largest;

```


```txt

54 546 548 60
Largest integer=        6054854654

1 34 3 98 9 76 45 4
Largest integer=        998764543431

```



## PowerShell

Using algorithm 3

```PowerShell
Function Get-LargestConcatenation ( [int[]]$Integers )
    {
    #  Get the length of the largest integer
    $Length = ( $Integers | Sort -Descending | Select -First 1 ).ToString().Length

    #  Convert to an array of strings,
    #  sort by each number repeated Length times and truncated to Length,
    #  and concatenate (join)
    $Concat = ( [string[]]$Integers | Sort { ( $_ * $Length ).Substring( 0, $Length ) } -Descending ) -join ''

    #  Convert to integer (upsizing type if needed)
    try           { $Integer = [ int32]$Concat }
    catch { try   { $Integer = [ int64]$Concat }
            catch { $Integer = [bigint]$Concat } }

    return $Integer
    }
```


```PowerShell
Get-LargestConcatenation 1, 34, 3, 98, 9, 76, 45, 4
Get-LargestConcatenation 54, 546, 548, 60
Get-LargestConcatenation 54, 546, 548, 60, 54, 546, 548, 60
```

```txt
998764543431
6054854654
60605485485465465454
```



## Prolog

Works with SWI-Prolog 6.5.3.

### All permutations method


```Prolog
largest_int_v1(In, Out) :-
	maplist(name, In, LC),
	aggregate(max(V), get_int(LC, V), Out).


get_int(LC, V) :-
	permutation(LC, P),
	append(P, LV),
	name(V, LV).

```

```txt
 ?- largest_int_v1([1, 34, 3, 98, 9, 76, 45, 4], Out).
Out = 998764543431.

 ?- largest_int_v1([54, 546, 548, 60], Out).
Out = 6054854654.


```



### Method 2


```Prolog
largest_int_v2(In, Out) :-
	maplist(name, In, LC),
	predsort(my_sort,LC, LCS),
	append(LCS, LC1),
	name(Out, LC1).


my_sort(R, L1, L2) :-
	append(L1, L2, V1), name(I1, V1),
	append(L2, L1, V2), name(I2, V2),
	(   I1 < I2, R = >; I1 = I2, R = '='; R = <).



% particular case  95 958
my_sort(>, [H1], [H1,  H2 | _]) :-
	H1 > H2.

my_sort(<, [H1], [H1, H2 | _]) :-
	H1 < H2.

my_sort(R, [H1], [H1, H1 | T]) :-
	my_sort(R, [H1], [H1 | T]).



% particular case  958 95
my_sort(>, [H1,  H2 | _], [H1]) :-
	H1 > H2.

my_sort(<, [H1,  H2 | _], [H1]) :-
	H1 < H2.

my_sort(R, [H1,  H1 | T], [H1]) :-
	my_sort(R, [H1 | T], [H1]) .

```


```txt
 ?- largest_int_v2([1, 34, 3, 98, 9, 76, 45, 4], Out).
Out = 998764543431 .

 ?- largest_int_v2([54, 546, 548, 60], Out).
Out = 5486054654 .

```



## Python


### Python: Sort on comparison of concatenated ints method

This also shows one of the few times where cmp= is better than key= on sorted()


```python
try:
    cmp     # Python 2 OK or NameError in Python 3
    def maxnum(x):
        return ''.join(sorted((str(n) for n in x),
                              cmp=lambda x,y:cmp(y+x, x+y)))
except NameError:
    # Python 3
    from functools import cmp_to_key
    def cmp(x, y):
        return -1 if x<y else ( 0 if x==y else 1)
    def maxnum(x):
        return ''.join(sorted((str(n) for n in x),
                              key=cmp_to_key(lambda x,y:cmp(y+x, x+y))))

for numbers in [(1, 34, 3, 98, 9, 76, 45, 4), (54, 546, 548, 60):
    print('Numbers: %r\n  Largest integer: %15s' % (numbers, maxnum(numbers)))
```


```txt
Numbers: (1, 34, 3, 98, 9, 76, 45, 4)
  Largest integer:    998764543431
Numbers: (54, 546, 548, 60)
  Largest integer:      6054854654
```



### Python: Compare repeated string method


```python
def maxnum(x):
    maxlen = len(str(max(x)))
    return ''.join(sorted((str(v) for v in x), reverse=True,
                          key=lambda i: i*(maxlen * 2 // len(i))))

for numbers in [(212, 21221), (1, 34, 3, 98, 9, 76, 45, 4), (54, 546, 548, 60)]:
    print('Numbers: %r\n  Largest integer: %15s' % (numbers, maxnum(numbers)))
```


```txt
Numbers: (212, 21221)
  Largest integer:        21221221
Numbers: (1, 34, 3, 98, 9, 76, 45, 4)
  Largest integer:    998764543431
Numbers: (54, 546, 548, 60)
  Largest integer:      6054854654
```


```python
from fractions import Fraction
from math import log10

def maxnum(x):
    return ''.join(str(n) for n in sorted(x, reverse=True,
                          key=lambda i: Fraction(i, 10**(int(log10(i))+1)-1)))

for numbers in [(1, 34, 3, 98, 9, 76, 45, 4), (54, 546, 548, 60)]:
    print('Numbers: %r\n  Largest integer: %15s' % (numbers, maxnum(numbers)))
```


;Output as first Python example, above.


### Python: Try all permutations method


```python
from itertools import permutations
def maxnum(x):
    return max(int(''.join(n) for n in permutations(str(i) for i in x)))

for numbers in [(1, 34, 3, 98, 9, 76, 45, 4), (54, 546, 548, 60)]:
    print('Numbers: %r\n  Largest integer: %15s' % (numbers, maxnum(numbers)))
```


;Output as above.


## Racket



```Racket

#lang racket
(define (largest-int ns)
  (string->number (apply ~a (sort ns (λ(x y) (string>? (~a x y) (~a y x)))))))
(map largest-int '((1 34 3 98 9 76 45 4) (54 546 548 60)))
;; -> '(998764543431 6054854654)

```



## REXX

The algorithm used is based on exact comparisons (left to right)   with   ''right digit fill''   of the   ''left digit''.

This allows the integers to be of any size.

This REXX version works with any size integer   (negative, zero, positive),   and does some basic error checking to

verify that the numbers are indeed integers   (and it also normalizes the integers).

The absolute value is used for negative numbers.


### simple integers


```rexx
/*REXX program constructs the largest integer  from an integer list using concatenation.*/
@.=.;     @.1 = '1  34  3  98  9  76  45  4'     /*the  1st  integer list to be used.   */
          @.2 = '54  546  548  60'               /* "   2nd     "      "   "  "   "     */
          @.3 = ' 4   45   54   5'               /* "   3rd     "      "   "  "   "     */
w=0                                              /* [↓]   process all the integer lists.*/
    do j=1  while @.j\==.;         z=space(@.j)  /*keep truckin' until lists exhausted. */
    w=max(w, length(z) );          $=            /*obtain maximum width to align output.*/
        do  while z\='';  idx=1;   big=norm(1)   /*keep examining the list  until  done.*/
          do k=2  to  words(z);    #=norm(k)     /*obtain an a number from the list.    */
          L=max(length(big), length(#) )         /*get the maximum length of the integer*/
          if left(#, L, left(#, 1) )   <<=   left(big, L, left(big, 1) )    then iterate
          big=#;                  idx=k          /*we found a new biggie (and the index)*/
          end   /*k*/                            /* [↑]  find max concatenated integer. */
        z=delword(z, idx, 1)                     /*delete this maximum integer from list*/
        $=$ || big                               /*append   "     "       "    ───►  $. */
        end     /*while z*/                      /* [↑]  process all integers in a list.*/
    say 'largest concatenatated integer from '  left( space(@.j), w)    " is ─────► "    $
    end         /*j*/                            /* [↑]  process each list of integers. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
norm: arg i;  #=word(z, i);   er=' ***error*** ';  if left(#, 1)=="-"  then #=substr(#, 2)
      if \datatype(#,'W')  then do; say er 'number'  #  "isn't an integer."; exit 13;  end
      return # / 1                               /*it's an integer,  then normalize it. */
```

```txt

largest concatenatated integer from  1 34 3 98 9 76 45 4  is ─────►  998764543431
largest concatenatated integer from  54 546 548 60        is ─────►  6054854654
largest concatenatated integer from  4 45 54 5            is ─────►  554454

```



### exponentiated integers

In REXX, a number such as   '''6.6e77'''   would be considered an integer   ''if''   the (current)   '''numeric digits'''   is

large enough to express that number as an integer without the exponent.

The default for REXX is   '''9'''   decimal digits,   but the   '''norm'''   function automatically uses enough decimal digits to

express the number as an integer.

This REXX version can handle any sized integer   (most REXXes can handle up to around eight million decimal

digits,   but displaying the result would be problematic for results wider than the display area).

```rexx
/*REXX program constructs the largest integer  from an integer list using concatenation.*/
@.=.;     @.1 = '1  34  3  98  9  76  45  4'     /*the  1st  integer list to be used.   */
          @.2 = '54  546  548  60'               /* "   2nd     "      "   "  "   "     */
          @.3 = ' 4   45   54   5'               /* "   3rd     "      "   "  "   "     */
          @.4 = ' 4   45   54   5   6.6e77'      /* "   4th     "      "   "  "   "     */
w=0                                              /* [↓]   process all the integer lists.*/
    do j=1  while @.j\==.;        z=space(@.j)   /*keep truckin' until lists exhausted. */
    w=max(w, length(z) );         $=             /*obtain maximum width to align output.*/
        do while z\='';  idx=1;   big=norm(1)    /*keep examining the list  until  done.*/
          do k=2  to  words(z);   #=norm(k)      /*obtain an a number from the list.    */
          L=max(length(big), length(#) )         /*get the maximum length of the integer*/
          if left(#, L, left(#, 1) )   <<=   left(big, L, left(big, 1) )    then iterate
          big=#;                  idx=k          /*we found a new biggie (and the index)*/
          end   /*k*/                            /* [↑]  find max concatenated integer. */
        z=delword(z, idx, 1)                     /*delete this maximum integer from list*/
        $=$ || big                               /*append   "     "       "    ───►  $. */
        end     /*while z*/                      /* [↑]  process all integers in a list.*/
    say 'largest concatenatated integer from '    left( space(@.j), w)       " is "      $
    end         /*j*/                            /* [↑]  process each list of integers. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
norm: arg i;  #=word(z, i);   er=' ***error*** ';  if left(#, 1)=="-"  then #=substr(#, 2)
      if \datatype(#,'N')  then do; say er 'number'  #  "isn't an number.";  exit 13;  end
                           else #=# / 1                           /*a #, so normalize it*/
      if pos('E',#)>0  then do; parse var # mant "E" pow          /*Has exponent? Expand*/
                                numeric digits pow + length(mand) /*expand digs, adjust#*/
                            end
      if \datatype(#,'W')  then do; say er 'number'  #  "isn't an integer."; exit 13;  end
      return #/1
```

```txt

largest concatenatated integer from  1 34 3 98 9 76 45 4  is  998764543431
largest concatenatated integer from  54 546 548 60        is  6054854654
largest concatenatated integer from  4 45 54 5            is  554454
largest concatenatated integer from  4 45 54 5 6.6e77     is  660000000000000000000000000000000000000000000000000000000000000000000000000000554454

```



### Alternate Version

Inspired by the previous versions.
<lang>/*REXX program constructs the largest integer  from an integer list using concatenation.*/
l.='';    l.1 = '1 34 3 98 9 76 45 4'   /*the  1st  integer list to be used.   */
          l.2 = '54 546 548 60'             /* "   2nd     "      "   "  "   "     */
          l.3 = ' 4  45  54  5'             /* "   3rd     "      "   "  "   "     */
          l.4 = ' 4  45  54  5  6.6e77'    /* "   4th     "      "   "  "   "     */
          l.5 = ' 3 3 .2'                  /* "   5th     "      "   "  "   "     */
/*
soll.1=998764543431
soll.2=6054854654
soll.3=554454
soll.4=660000000000000000000000000000000000000000000000000000000000000000000000000000545454
*/
l_length=0
Do li=1 By 1 While l.li<>''
  l_length=max(l_length,length(space(l.li)))
  End

Do li=1 By 1 While l.li<>''
  z=''
  Do j=1 To words(l.li)
    int=integer(word(l.li,j))
    If int='?' Then Do
      Say left(space(l.li),l_length) '-> ** invalid ** bad integer' word(l.li,j)
      Iterate li
      End
    Else
      z=z int
    End
/*Say copies(' ',l_length) '  ' soll.li */
  Say left(space(l.li),l_length) '->' largeint(l.li)
  End
Exit

integer: Procedure
Numeric Digits 1000
Parse Arg z
If Datatype(z,'W') Then
  Return z+0
Else
  Return '?'

largeint:
result=''
Do While z<>''                                 /* [?]  check the rest of the integers.*/
  big=word(z,1); index=1; LB=length(big)       /*assume that first integer is biggest.*/
  do k=2 to words(z);
    n=word(z,k)                                /*obtain an integer from the list.     */
    L=max(LB,length(n))                        /*get the maximum length of the integer*/
    if left(n,L,left(n,1))<<=left(big,L,left(big,1)) then iterate
    big=n; index=k                             /*we found a new biggie (and the index)*/
    LB=length(big)
    End   /*k*/
  z=delword(z,index,1)                         /*delete this maximum integer from list*/
  result=result||big                           /*append   "     "       "    ---?  $. */
  end     /*while z*/                          /* [?]  process all integers in a list.*/
Return result
```

```txt
1 34 3 98 9 76 45 4 -> 998764543431
54 546 548 60       -> 6054854654
4 45 54 5           -> 554454
4 45 54 5 6.6e77    -> 660000000000000000000000000000000000000000000000000000000000000000000000000000554454
3 3 .2              -> ** invalid ** bad integer .2
```



### Version 4

```rexx
/*REXX program constructs the largest integer from an integer list using concatenation.*/
l.='';    l.1 = '1 34 3 98 9 76 45 4'           /*the  1st  integer list to be used.   */
          l.2 = '54 546 548 60'                 /* "   2nd     "      "   "  "   "     */
          l.3 = ' 4  45  54  5'                 /* "   3rd     "      "   "  "   "     */
          l.4 = ' 4  45  54  5  6.6e77'         /* "   4th     "      "   "  "   "     */
          l.5 = ' 3 3 .2'                       /* "   5th     "      "   "  "   "     */
          l.6 = ' 4  45  54  5  6.6e1001'       /* "   6th     "      "   "  "   "     */
          l.7 = ' 4.0000 45 54 5.00'            /* "   7th     "      "   "  "   "     */
          l.8 = ' 10e999999999 5'               /* "   8th     "      "   "  "   "     */
l_length=0
Do li=1 By 1 While l.li<>''
  l_length=max(l_length,length(space(l.li)))
  End

Do li=1 By 1 While l.li<>''
  z=''
  msg=''
  Do j=1 To words(l.li)
    int=integer(word(l.li,j))
    If int='?' Then Do
      Say left(space(l.li),l_length) '-> ** invalid ** bad list item:' word(l.li,j) msg
      Iterate li
      End
    Else
      z=z int
    End
  zz=largeint(z)
  If length(zz)<60 Then
    Say left(space(l.li),l_length) '->' zz
  Else
    Say left(space(l.li),l_length) '->' left(zz,5)'...'right(zz,5)
  End
Exit

integer: Procedure Expose msg
Numeric Digits 1000
Parse Arg z
If Datatype(z,'W') Then
  Return z/1
Else Do
  If Datatype(z,'NUM') Then Do
    Do i=1 To 6 Until dig>=999999999
      dig= digits()*10
      dig=min(dig,999999999)
      Numeric Digits dig
      If Datatype(z,'W') Then
        Return z/1
      End
    msg='cannot convert it to an integer'
    Return '?'
    End
  Else Do
    msg='not a number (larger than what this REXX can handle)'
    Return '?'
    End
  End

largeint: Procedure
Parse Arg list
w.0=words(list)
Do i=1 To w.0
  w.i=word(list,i)
  End
Do wx=1 To w.0-1
  Do wy=wx+1 To w.0
    xx=w.wx
    yy=w.wy
    xy=xx||yy
    yx=yy||xx
    if xy < yx then do
      /* swap xx and yy */
      w.wx = yy
      w.wy = xx
      end
    End
  End
list=''
Do ww=1 To w.0
  list=list w.ww
  End
Return space(list,0)
```

```txt
1 34 3 98 9 76 45 4 -> 998764543431
54 546 548 60       -> 6054854654
4 45 54 5           -> 554454
4 45 54 5 6.6e77    -> 66000...54454
3 3 .2              -> ** invalid ** bad list item: .2 cannot convert it to an integer
4 45 54 5 6.6e1001  -> 66000...54454
4.0000 45 54 5.00   -> 554454
10e999999999 5      -> ** invalid ** bad list item: 10e999999999 not a number (larger than what this REXX can handle)
```



## Ring


```ring

nums=[1,34,3,98,9,76,45,4]
see largestInt(8) + nl
nums=[54,546,548,60]
see largestInt(4) + nl

func largestInt len
l = ""
sorted = false
while not sorted
      sorted=true
      for i=1 to len-1
          a=string(nums[i])
          b=string(nums[i+1])
          if a+b<b+a
             temp = nums[i]
             nums[i] = nums[i+1]
             nums[i+1] = temp
             sorted=false ok
      next
end
for i=1 to len
    l+=string(nums[i])
next
return l

```

Output:

```txt

998764543431
6054854654

```



## Ruby



### Sort on comparison of concatenated ints method

```Ruby
def icsort nums
  nums.sort { |x, y| "#{y}#{x}" <=> "#{x}#{y}" }
end

[[54, 546, 548, 60], [1, 34, 3, 98, 9, 76, 45, 4]].each do |c|
  p c # prints nicer in Ruby 1.8
  puts icsort(c).join
end
```


```txt
[54, 546, 548, 60]
6054854654
[1, 34, 3, 98, 9, 76, 45, 4]
998764543431
```



### Compare repeated string method


```ruby
def icsort nums
  maxlen = nums.max.to_s.length
  nums.map{ |x| x.to_s }.sort_by { |x| x * (maxlen * 2 / x.length) }.reverse
end

[[54, 546, 548, 60], [1, 34, 3, 98, 9, 76, 45, 4]].each do |c|
  p c # prints nicer in Ruby 1.8
  puts icsort(c).join
end
```


;Output as above.


```ruby
require 'rational' #Only needed in Ruby < 1.9

def icsort nums
  nums.sort_by { |i| Rational(i, 10**(Math.log10(i).to_i+1)-1) }.reverse
end

[[54, 546, 548, 60], [1, 34, 3, 98, 9, 76, 45, 4]].each do |c|
  p c # prints nicer in Ruby 1.8
  puts icsort(c).join
end
```


;Output as above.


## Run BASIC


```runbasic
a1$ = "1, 34, 3, 98, 9, 76, 45, 4"
a2$ = "54,546,548,60"

print "Max Num ";a1$;" = ";maxNum$(a1$)
print "Max Num ";a2$;" = ";maxNum$(a2$)

function maxNum$(a1$)
while word$(a1$,i+1,",") <> ""
 i = i + 1
 a$(i) = trim$(word$(a1$,i,","))
wend

s = 1
while s = 1
 s = 0
 for j = 1 to i -1
  if a$(j)+a$(j+1) < a$(j+1)+a$(j) then
   h$      = a$(j)
   a$(j)   = a$(j+1)
   a$(j+1) = h$
   s       = 1
  end if
 next j
wend

for j = 1 to i
 maxNum$ = maxNum$ ; a$(j)
next j
end function
```

```txt
Max Num 1, 34, 3, 98, 9, 76, 45, 4 = 998764543431
Max Num 54,546,548,60 = 6054854654
```



## Rust


```Rust
fn maxcat(a: &mut [u32]) {
    a.sort_by(|x, y| {
        let xy = format!("{}{}", x, y);
        let yx = format!("{}{}", y, x);
        xy.cmp(&yx).reverse()
    });
    for x in a {
        print!("{}", x);
    }
    println!();
}

fn main() {
    maxcat(&mut [1, 34, 3, 98, 9, 76, 45, 4]);
    maxcat(&mut [54, 546, 548, 60]);
}
```

```txt
998764543431
6054854654
```


=={{header|S-lang}}==
<lang S-lang>define catcmp(a, b)
{
   a = string(a);
   b = string(b);
   return strcmp(b+a, a+b);
}

define maxcat(arr)
{
   arr = arr[array_sort(arr, &catcmp)];
   variable result = "", elem;
   foreach elem (arr)
     result += string(elem);
   return result;
}

print("max of series 1 is " + maxcat([1, 34, 3, 98, 9, 76, 45, 4]));
print("max of series 2 is " + maxcat([54, 546, 548, 60]));

```

```txt
"max of series 1 is 998764543431"
"max of series 2 is 6054854654"

```



## Scala

```Scala
object LIFCI extends App {

  def lifci(list: List[Long]) = list.permutations.map(_.mkString).max

  println(lifci(List(1, 34, 3, 98, 9, 76, 45, 4)))
  println(lifci(List(54, 546, 548, 60)))
}
```


```txt

 998764543431
 6054854654

```



## Scheme


```Scheme
(define (cat . nums)  (apply string-append (map number->string nums)))

(define (my-compare a b)  (string>? (cat a b) (cat b a)))

(map  (lambda (xs) (string->number (apply cat (sort xs my-compare))))
      '((1 34 3 98 9 76 45 4) (54 546 548 60)))
```

```txt

(998764543431 6054854654)

```



## Sidef

```ruby
func maxnum(nums) {
    nums.sort {|x,y|  "#{y}#{x}" <=> "#{x}#{y}" };
}

[[54, 546, 548, 60], [1, 34, 3, 98, 9, 76, 45, 4]].each { |c|
    say maxnum(c).join.to_num;
}
```

```txt

6054854654
998764543431

```



## Tcl


```tcl
proc intcatsort {nums} {
    lsort -command {apply {{x y} {expr {"$y$x" - "$x$y"}}}} $nums
}
```

Demonstrating:

```tcl
foreach collection {
    {1 34 3 98 9 76 45 4}
    {54 546 548 60}
} {
    set sorted [intcatsort $collection]
    puts "\[$collection\] => \[$sorted\]  (concatenated: [join $sorted ""])"
}
```

```txt

[1 34 3 98 9 76 45 4] => [9 98 76 45 4 34 3 1]  (concatenated: 998764543431)
[54 546 548 60] => [60 548 546 54]  (concatenated: 6054854654)

```



## VBScript

```vb

Function largestint(list)
	nums = Split(list,",")
	Do Until IsSorted = True
		IsSorted = True
		For i = 0 To UBound(nums)
			If i <> UBound(nums) Then
				a = nums(i)
				b = nums(i+1)
				If CLng(a&b) < CLng(b&a) Then
					tmpnum = nums(i)
					nums(i) = nums(i+1)
					nums(i+1) = tmpnum
					IsSorted = False
				End If
			End If
		Next
	Loop
	For j = 0 To UBound(nums)
		largestint = largestint & nums(j)
	Next
End Function

WScript.StdOut.Write largestint(WScript.Arguments(0))
WScript.StdOut.WriteLine

```


```txt

F:\>cscript /nologo largestint.vbs 1,34,3,98,9,76,45,4
998764543431

F:\>cscript /nologo largestint.vbs 54,546,548,60
6054854654

```



## Vim Script

This solution is intended to be run as an Ex command within a buffer containing the integers to be processed, one per line.

```Vim
%s/\(.\+\)/\1\1/ | sort! | %s/\(.\+\)\1\n/\1/
```


;Demonstration


```Bash
$ paste -s nums
1	34	3	98	9	76	45	4
$ vim -S icsort.vim nums
998764543431
```



## zkl


```zkl
fcn bigCI(ns){
   ns.apply("toString").sort(fcn(a,b){ (a+b)>(b+a) }).concat();
}
```


```zkl
bigCI(T(1, 34, 3, 98, 9, 76, 45, 4)).println();
bigCI(T(54, 546, 548, 60)).println();
```

```txt

998764543431
6054854654

```

