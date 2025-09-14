+++
title = "Forward difference"
description = ""
date = 2019-06-06T06:06:51Z
aliases = []
[extra]
id = 2420
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
languages = [
  "ada",
  "algol_68",
  "algol_w",
  "apl",
  "applescript",
  "autohotkey",
  "awk",
  "bbc_basic",
  "c",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dart",
  "e",
  "echolisp",
  "elixir",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "go",
  "haskell",
  "hicest",
  "idl",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k4",
  "kotlin",
  "lasso",
  "logo",
  "lua",
  "m2000_interpreter",
  "maxima",
  "netrexx",
  "nial",
  "nim",
  "objeck",
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
  "pop11",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "scheme",
  "seed7",
  "sequencel",
  "sidef",
  "slate",
  "smalltalk",
  "sql",
  "standard_ml",
  "stata",
  "swift",
  "tcl",
  "ursala",
  "visual_basic_dotnet",
  "visual_foxpro",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

Provide code that produces a list of numbers which is the   <big>n<sup>th</sup></big>  order forward difference, given a non-negative integer (specifying the order) and a list of numbers.


The first-order forward difference of a list of numbers   <big>'''A'''</big>   is a new list   <big>'''B'''</big>,   where   <big><b>B</b><sub>n</sub> = <b>A</b><sub>n+1</sub> - <b>A</b><sub>n</sub></big>.

List   <big>'''B'''</big>   should have one fewer element as a result.

The second-order forward difference of   <big>'''A'''</big>   will be:

```txt

tdefmodule Diff do
	def forward(arr,i\\1) do
		forward(arr,[],i)
	end

	def forward([_|[]],diffs,i) do
		if i == 1 do
			IO.inspect diffs
		else
			forward(diffs,[],i-1)
		end
	end

	def forward([val1|[val2|vals]],diffs,i) do
		forward([val2|vals],diffs++[val2-val1],i)
	end
end

```

The same as the first-order forward difference of   <big>'''B'''</big>.

That new list will have two fewer elements than   <big>'''A'''</big>   and one less than   <big>'''B'''</big>.

The goal of this task is to repeat this process up to the desired order.

For a more formal description, see the related   [http://mathworld.wolfram.com/ForwardDifference.html Mathworld article].


;Algorithmic options:
* Iterate through all previous forward differences and re-calculate a new array each time.
* Use this formula (from [[wp:Forward difference|Wikipedia]]):
<big><big>
::: <math>\Delta^n [f](x)= \sum_{k=0}^n {n \choose k} (-1)^{n-k} f(x+k)</math>
</big></big>
::: ([[Pascal's Triangle]]   may be useful for this option.)





## Ada


```ada
with Ada.Text_Io;
with Ada.Float_Text_Io; use Ada.Float_Text_Io;
with Ada.containers.Vectors;

procedure Forward_Difference is
   package Flt_Vect is new Ada.Containers.Vectors(Positive, Float);
   use Flt_Vect;
   procedure Print(Item : Vector) is
   begin
      if not Item.Is_Empty then
         Ada.Text_IO.Put('[');
         for I in 1..Item.Length loop
            Put(Item => Item.Element(Positive(I)), Fore => 1, Aft => 1, Exp => 0);
             if Positive(I) < Positive(Item.Length) then
               Ada.Text_Io.Put(", ");
            end if;
         end loop;
         Ada.Text_Io.Put_line("]");
      else
         Ada.Text_IO.Put_Line("Empty List");
      end if;

   end Print;

  function Diff(Item : Vector; Num_Passes : Natural) return Vector is
      A : Vector := Item;
      B : Vector := Empty_Vector;
   begin
      if not A.Is_Empty then
         for I in 1..Num_Passes loop
            for I in 1..Natural(A.Length) - 1 loop
                  B.Append(A.Element(I + 1) - A.Element(I));
            end loop;
            Move(Target => A, Source => B);
         end loop;
      end if;
      return A;
   end Diff;
   Values : array(1..10) of Float := (90.0, 47.0, 58.0, 29.0, 22.0, 32.0, 55.0, 5.0, 55.0, 73.0);
   A : Vector;
begin
   for I in Values'range loop
      A.Append(Values(I)); -- Fill the vector
   end loop;
   Print(Diff(A, 1));
   Print(Diff(A, 2));
   Print(Diff(A, 9));
   Print(Diff(A, 10));
   print(Diff(A, 0));
end Forward_Difference;
```

```txt

 [-43.0, 11.0, -29.0, -7.0, 10.0, 23.0, -50.0, 50.0, 18.0]
 [54.0, -40.0, 22.0, 17.0, 13.0, -73.0, 100.0, -32.0]
 [-2921.0]
 Empty List
 [90.0, 47.0, 58.0, 29.0, 22.0, 32.0, 55.0, 5.0, 55.0, 73.0]

```



## ALGOL 68

```algol68
main:(
  MODE LISTREAL = [1:0]REAL;

  OP - = (LISTREAL a,b)LISTREAL: (
    [UPB a]REAL out;
    FOR i TO UPB out DO out[i]:=a[i]-b[i] OD;
    out
  );

  FORMAT real fmt=$zzz-d.d$;
  FORMAT repeat fmt = $n(UPB s-1)(f(real fmt)",")f(real fmt)$;
  FORMAT list fmt = $"("f(UPB s=1|real fmt|repeat fmt)")"$;

  FLEX [1:0] REAL s := (90, 47, 58, 29, 22, 32, 55, 5, 55, 73);

  printf((list fmt,s,$";"l$));
  TO UPB s-1 DO
    s := s[2:] - s[:UPB s-1];
    printf((list fmt,s,$";"l$))
  OD
)
```

```txt

(   90.0,   47.0,   58.0,   29.0,   22.0,   32.0,   55.0,    5.0,   55.0,   73.0);
(  -43.0,   11.0,  -29.0,   -7.0,   10.0,   23.0,  -50.0,   50.0,   18.0);
(   54.0,  -40.0,   22.0,   17.0,   13.0,  -73.0,  100.0,  -32.0);
(  -94.0,   62.0,   -5.0,   -4.0,  -86.0,  173.0, -132.0);
(  156.0,  -67.0,    1.0,  -82.0,  259.0, -305.0);
( -223.0,   68.0,  -83.0,  341.0, -564.0);
(  291.0, -151.0,  424.0, -905.0);
( -442.0,  575.0,-1329.0);
( 1017.0,-1904.0);
(-2921.0);

```



## ALGOL W


```algolw
begin
    % calculate forward differences                                  %

    % sets elements of B to the first order forward differences of A %
    % A should have bounds 1 :: n, B should have bounds 1 :: n - 1   %
    procedure FirstOrderFDifference ( integer array A( * )
                                    ; integer array B( * )
                                    ; integer value n
                                    ) ;
        for i := 2 until n do B( i - 1 ) := A( i ) - A( i - 1 );

    integer array v   ( 1 :: 10 );
    integer array diff( 1 ::  9 );
    integer vPos, length;

    % construct the initial values array                              %
    vPos := 1;
    for i := 90, 47, 58, 29, 22, 32, 55, 5, 55, 73 do begin
        v( vPos ) := i;
        vPos := vPos + 1
    end for_i ;
    % calculate and show the differences                              %
    i_w    := 5; % set output format %
    length := 10;
    for order := 1 until length - 1 do begin
        FirstOrderFDifference( v, diff, length );
        length := length - 1;
        write( order, ": " ); for i := 1 until length do writeon( diff( i ) );
        for i := 1 until length do v( i ) := diff( i )
    end for_order
end.
```

```txt

    1  :   -43     11    -29     -7     10     23    -50     50     18
    2  :    54    -40     22     17     13    -73    100    -32
    3  :   -94     62     -5     -4    -86    173   -132
    4  :   156    -67      1    -82    259   -305
    5  :  -223     68    -83    341   -564
    6  :   291   -151    424   -905
    7  :  -442    575  -1329
    8  :  1017  -1904
    9  : -2921

```



## APL

```apl
      list ←  90 47 58 29 22 32 55 5 55 73

      fd   ←  {⍺=0:⍵⋄(⍺-1)∇(1↓⍵)-(¯1↓⍵)}

      1 fd list
¯43 11 ¯29 ¯7 10 23 ¯50 50 18

      2 fd list
54 ¯40 22 17 13 ¯73 100 ¯32
```



## AppleScript

```AppleScript
-- FORWARD DIFFERENCE --------------------------------------------------------

-- forwardDifference :: Int -> [Int] -> [Int]
on forwardDifference(n, xs)
    set lng to length of xs

    -- atLength :: [Int] -> Bool
    script atLength
        property fullLength : lng
        property ndx : n

        on |λ|(ds)
            (atLength's fullLength) - (length of ds) ≥ atLength's ndx
        end |λ|
    end script

    -- fd :: [Int] -> [Int]
    script fd
        on |λ|(xs)
            script minus
                on |λ|(a, b)
                    a - b
                end |λ|
            end script

            zipWith(minus, tail(xs), xs)
        end |λ|
    end script

    |until|(atLength, fd, xs)
end forwardDifference


-- TEST ----------------------------------------------------------------------
on run
    set xs to {90, 47, 58, 29, 22, 32, 55, 5, 55, 73}

    script test
        on |λ|(n)
            intercalate("  ->  [", ¬
                {{n}} & intercalate(", ", forwardDifference(n, xs))) & "]"
        end |λ|
    end script

    intercalate(linefeed, map(test, enumFromTo(1, 9)))
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m > n then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

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

-- tail :: [a] -> [a]
on tail(xs)
    if length of xs > 1 then
        items 2 thru -1 of xs
    else
        {}
    end if
end tail

-- until :: (a -> Bool) -> (a -> a) -> a -> a
on |until|(p, f, x)
    set mp to mReturn(p)
    set v to x

    tell mReturn(f)
        repeat until mp's |λ|(v)
            set v to |λ|(v)
        end repeat
    end tell
    return v
end |until|

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

```txt
1  ->  [-43, 11, -29, -7, 10, 23, -50, 50, 18]
2  ->  [54, -40, 22, 17, 13, -73, 100, -32]
3  ->  [-94, 62, -5, -4, -86, 173, -132]
4  ->  [156, -67, 1, -82, 259, -305]
5  ->  [-223, 68, -83, 341, -564]
6  ->  [291, -151, 424, -905]
7  ->  [-442, 575, -1329]
8  ->  [1017, -1904]
9  ->  [-2921]
```



## AutoHotkey

contributed by Laszlo on the ahk [http://www.autohotkey.com/forum/post-276470.html#276470 forum]

```AutoHotkey
MsgBox % diff("2,3,4,3",1)
MsgBox % diff("2,3,4,3",2)
MsgBox % diff("2,3,4,3",3)
MsgBox % diff("2,3,4,3",4)

diff(list,ord) { ; high order forward differences of a list
   Loop %ord% {
      L =
      Loop Parse, list, `, %A_Space%%A_Tab%
         If (A_Index=1)
            p := A_LoopField
         Else
            L .= "," A_LoopField-p, p := A_LoopField
      list := SubStr(L,2)
   }
   Return list
}
```



## AWK



```awk
#!/usr/bin/awk -f
BEGIN {
   if (p<1) {p = 1};
}

function diff(s, p) {
   n = split(s, a, " ");
   for (j = 1; j <= p; j++) {
      for(i = 1; i <= n-j; i++) {
         a[i] = a[i+1] - a[i];
      }
   }
   s = "";
   for (i = 1; i <= n-p; i++) s = s" "a[i];
   return s;
}

{
   print diff($0, p);
}
```


Using Pascal's triangle:


```awk
#!/usr/bin/awk -f
BEGIN {
   if (p<1) {p = 1};
}

function diff(s, p) {
    # pascal triangled with sign changes
    b[1] = (p%2) ? 1 : -1;
    for (j=1; j < p; j++) {
        b[j+1] = -b[j]*(p-j)/j;
    };

    n = split(s, a, " ");
    s = "";
    for (j = 1; j <= n-p+1; j++) {
        c = 0;
        for(i = 1; i <= p; i++) {
            c += b[i]*a[j+i-1];
        }
        s = s" "c;
    }
    return s;
}

{
   print diff($0, p);
}
```


```txt
$ echo '0 1 2 4 7 4 2 1 0' | ./diff.awk -v p=1
 1 1 2 3 -3 -2 -1 -1
$ echo '0 1 2 4 7 4 2 1 0' | ./diff.awk -v p=2
 0 1 1 -6 1 1 0
$ echo '0 1 2 4 7 4 2 1 0' | ./diff.awk -v p=3
 1 0 -7 7 0 -1
$ echo '0 1 2 4 7 4 2 1 0' | ./diff.awk -v p=4
 -1 -7 14 -7 -1
$ echo '0 1 2 4 7 4 2 1 0' | ./diff.awk -v p=5
 -6 21 -21 6
$ echo '0 1 2 4 7 4 2 1 0' | ./diff.awk -v p=6
 27 -42 27
$ echo '0 1 2 4 7 4 2 1 0' | ./diff.awk -v p=7
 -69 69
$ echo '0 1 2 4 7 4 2 1 0' | ./diff.awk -v p=8
 138
$ echo '0 1 2 4 7 4 2 1 0' | ./diff.awk -v p=9

```



## BBC BASIC

```bbcbasic
      DIM A(9)
      A() = 90.0, 47.0, 58.0, 29.0, 22.0, 32.0, 55.0, 5.0, 55.0, 73.0
      PRINT "Original array: " FNshowarray(A())
      PROCforward_difference(1, A(), B())
      PRINT "Forward diff 1: " FNshowarray(B())
      PROCforward_difference(2, A(), C())
      PRINT "Forward diff 2: " FNshowarray(C())
      PROCforward_difference(9, A(), D())
      PRINT "Forward diff 9: " FNshowarray(D())
      END

      DEF PROCforward_difference(n%, a(), RETURN b())
      LOCAL c%, i%, j%
      DIM b(DIM(a(),1) - n%)
      FOR i% = 0 TO DIM(b(),1)
        b(i%) = a(i% + n%)
        c% = 1
        FOR j% = 1 TO n%
          c% = -INT(c% * (n% - j% + 1) / j% + 0.5)
          b(i%) += c% * a(i% + n% - j%)
        NEXT
      NEXT
      ENDPROC

      DEF FNshowarray(a())
      LOCAL i%, a$
      FOR i% = 0 TO DIM(a(),1)
        a$ += STR$(a(i%)) + ", "
      NEXT
      = LEFT$(LEFT$(a$))
```

```txt

Original array: 90, 47, 58, 29, 22, 32, 55, 5, 55, 73
Forward diff 1: -43, 11, -29, -7, 10, 23, -50, 50, 18
Forward diff 2: 54, -40, 22, 17, 13, -73, 100, -32
Forward diff 9: -2921

```



## C


```cpp
#include <iostream>
#include <string.h>
#include <stdio.h>

double* fwd_diff(double* x, unsigned int len, unsigned int order)
{
	unsigned int i, j;
	double* y;

	/* handle two special cases */
	if (order >= len) return 0;

	y = malloc(sizeof(double) * len);
	if (!order) {
		memcpy(y, x, sizeof(double) * len);
		return y;
	}

	/* first order diff goes from x->y, later ones go from y->y */
	for (j = 0; j < order; j++, x = y)
		for (i = 0, len--; i < len; i++)
			y[i] = x[i + 1] - x[i];

	y = realloc(y, sizeof(double) * len);
	return y;
}

int main(void)
{
	double *y, x[] = {90, 47, 58, 29, 22, 32, 55, 5, 55, 73};
	int i, len = sizeof(x) / sizeof(x[0]);

	y = fwd_diff(x, len, 1);
	for (i = 0; i < len - 1; i++)
		printf("%g ", y[i]);
	putchar('\n');

	return 0;
}
```


Use method with Pascal triangle, binomial coefficients are pre-computed


```c
#include <stdio.h>

int* binomCoeff(int n) {
     int *b = calloc(n+1,sizeof(int));
     int j;
     b[0] = n%2 ? -1 : 1;
     for (j=1 ; j<=n; j++)
           b[j] = -b[j-1]*(n+1-j)/j;

     return(b);
};

main () {
    double array[] = { 90, 47, 58, 29, 22, 32, 55, 5, 55, 73 };
    size_t lenArray = sizeof(array)/sizeof(array[0]);

    int p = 4;  // order
    int *b = binomCoeff(p);   // pre-compute binomial coefficients for order p

    int j, k;

    // compute p-th difference
    for (k=0 ; k < lenArray; k++)
        for (array[k] *= b[0], j=1 ; j<=p; j++)
            array[k] += b[j] * array[k+j];

    free(b);

    // resulting series is shorter by p elements
    lenArray -= p;
    for (k=0 ; k < lenArray; k++)  printf("%f ",array[k]);
    printf("\n");
}

```



## C#


```c#
using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static IEnumerable<int> ForwardDifference(IEnumerable<int> sequence, uint order = 1u)
    {
        switch (order)
        {
            case 0u:
                return sequence;
            case 1u:
                return sequence.Skip(1).Zip(sequence, (next, current) => next - current);
            default:
                return ForwardDifference(ForwardDifference(sequence), order - 1u);
        }
    }

    static void Main()
    {
        IEnumerable<int> sequence = new[] { 90, 47, 58, 29, 22, 32, 55, 5, 55, 73 };
        do
        {
            Console.WriteLine(string.Join(", ", sequence));
        } while ((sequence = ForwardDifference(sequence)).Any());
    }
}
```

```txt
90, 47, 58, 29, 22, 32, 55, 5, 55, 73
-43, 11, -29, -7, 10, 23, -50, 50, 18
54, -40, 22, 17, 13, -73, 100, -32
-94, 62, -5, -4, -86, 173, -132
156, -67, 1, -82, 259, -305
-223, 68, -83, 341, -564
291, -151, 424, -905
-442, 575, -1329
1017, -1904
-2921
```



## C++

This code uses a separate function to do a first-order forward difference,
which is then called several times for calculating n-th order forward difference.
No error checking is implemented.

```cpp
#include <vector>
#include <iterator>
#include <algorithm>

// calculate first order forward difference
// requires:
// * InputIterator is an input iterator
// * OutputIterator is an output iterator
// * The value type of InputIterator is copy-constructible and assignable
// * The value type of InputIterator supports operator -
// * The result type of operator- is assignable to the value_type of OutputIterator
// returns: The iterator following the output sequence
template<typename InputIterator, typename OutputIterator>
 OutputIterator forward_difference(InputIterator first, InputIterator last,
                                   OutputIterator dest)
{
  // special case: for empty sequence, do nothing
  if (first == last)
    return dest;

  typedef typename std::iterator_traits<InputIterator>::value_type value_type;

  value_type temp = *first++;
  while (first != last)
  {
    value_type temp2 = *first++;
    *dest++ = temp2 - temp;
    temp = temp2;
  }

  return dest;
}

// calculate n-th order forward difference.
// requires:
// * InputIterator is an input iterator
// * OutputIterator is an output iterator
// * The value type of InputIterator is copy-constructible and assignable
// * The value type of InputIterator supports operator -
// * The result type of operator- is assignable to the value_type of InputIterator
// * The result type of operator- is assignable to the value_type of OutputIterator
// * order >= 0
// returns: The iterator following the output sequence
template<typename InputIterator, typename OutputIterator>
 OutputIterator nth_forward_difference(int order,
                                       InputIterator first, InputIterator last,
                                       OutputIterator dest)
{
  // special case: If order == 0, just copy input to output
  if (order == 0)
    return std::copy(first, last, dest);

  // second special case: If order == 1, just forward to the first-order function
  if (order == 1)
    return forward_difference(first, last, dest);

  // intermediate results are stored in a vector
  typedef typename std::iterator_traits<InputIterator>::value_type value_type;
  std::vector<value_type> temp_storage;

  // fill the vector with the result of the first order forward difference
  forward_difference(first, last, std::back_inserter(temp_storage));

  // the next n-2 iterations work directly on the vector
  typename std::vector<value_type>::iterator begin = temp_storage.begin(),
                                             end = temp_storage.end();
  for (int i = 1; i < order-1; ++i)
    end = forward_difference(begin, end, begin);

  // the final iteration writes directly to the output iterator
  return forward_difference(begin, end, dest);
}

// example usage code
#include <iostream>

int main()
{
  double array[10] = { 90.0, 47.0, 58.0, 29.0, 22.0, 32.0, 55.0, 5.0, 55.0, 73.0 };

  // this stores the results in the vector dest
  std::vector<double> dest;
  nth_forward_difference(1, array, array+10, std::back_inserter(dest));

  // output dest
  std::copy(dest.begin(), dest.end(), std::ostream_iterator<double>(std::cout, " "));
  std::cout << std::endl;

  // however, the results can also be output as they are calculated
  nth_forward_difference(2, array, array+10, std::ostream_iterator<double>(std::cout, " "));
  std::cout << std::endl;

  nth_forward_difference(9, array, array+10, std::ostream_iterator<double>(std::cout, " "));
  std::cout << std::endl;

  nth_forward_difference(10, array, array+10, std::ostream_iterator<double>(std::cout, " "));
  std::cout << std::endl;

  nth_forward_difference(0, array, array+10, std::ostream_iterator<double>(std::cout, " "));
  std::cout << std::endl;

  // finally, the results can also be written into the original array
  // (which of course destroys the original content)
  double* end = nth_forward_difference(3, array, array+10, array);

  for (double* p = array; p < end; ++p)
    std::cout << *p << " ";
  std::cout << std::endl;

  return 0;
}
```


```txt

 -43 11 -29 -7 10 23 -50 50 18
 54 -40 22 17 13 -73 100 -32
 -2921

 90 47 58 29 22 32 55 5 55 73
 -94 62 -5 -4 -86 173 -132

```

Note the empty line indicating the empty sequence for order 10.


### Using Standard Template Library


```cpp

#include <iostream>
#include <numeric>
// Calculate the Forward Difference of a series if integers showing each order
//
// Nigel Galloway. August 20th., 2012
//
int main() {
    int x[] = {NULL,-43,11,-29,-7,10,23,-50,50,18};
    const int N = sizeof(x) / sizeof(int) - 1;
    for (int ord = 0; ord < N - 1; ord++) {
        std::adjacent_difference(x+1, x + N + 1 - ord, x);
        for (int i = 1; i < N - ord; i++) std::cout << x[i] << ' ';
        std::cout << std::endl;
    }
    return 0;
}

```

```txt

54 -40 22 17 13 -73 100 -32
-94 62 -5 -4 -86 173 -132
156 -67 1 -82 259 -305
-223 68 -83 341 -564
291 -151 424 -905
-442 575 -1329
1017 -1904
-2921

```


Usually one will not want the intermediate results,
in which case the following is sufficient:

```cpp

#include <iostream>
#include <numeric>
// Calculate the Forward Difference of a series if integers
//
// Nigel Galloway. August 20th., 2012
//
int main() {
    int x[] = {NULL,-43,11,-29,-7,10,23,-50,50,18};
    const int N = sizeof(x) / sizeof(int) - 1;
    for (int ord = 0; ord < N - 1; ord++) std::adjacent_difference(x+1, x + N + 1 - ord, x);
    std::cout << x[1] << std::endl;
    return 0;
}

```

```txt

-2921

```


===Using Pascal's Triangle===

```cpp

#include <iostream>
#include <algorithm>
// Calculate the Forward Difference of a series if integers using Pascal's Triangle
// For this example the 9th line of Pascal's Triangle is stored in P.
//
// Nigel Galloway. August 20th., 2012
//
int main() {
    const int P[] = {1,-8,28,-56,70,-56,28,-8,1};
    int x[] = {-43,11,-29,-7,10,23,-50,50,18};
    std::transform(x, x + sizeof(x) / sizeof(int), P, x, std::multiplies<int>());
    std::cout << std::accumulate(x, x + sizeof(x) / sizeof(int), 0) << std::endl;
    return 0;
}

```

```txt

-2921

```



## Clojure


```lisp
(defn fwd-diff [nums order]
  (nth (iterate #(map - (next %) %) nums) order))
```



## CoffeeScript


```coffeescript

forward_difference = (arr, n) ->
  # Find the n-th order forward difference for arr using
  # a straightforward recursive algorithm.
  # Assume arr is integers and n <= arr.length.
  return arr if n == 0
  arr = forward_difference(arr, n-1)
  (arr[i+1] - arr[i] for i in [0...arr.length - 1])

arr = [-1, 0, 1, 8, 27, 64, 125, 216]
for n in [0..arr.length]
  console.log n, forward_difference arr, n

```

```txt
> coffee forward_difference.coffee
0 [ -1, 0, 1, 8, 27, 64, 125, 216 ]
1 [ 1, 1, 7, 19, 37, 61, 91 ]
2 [ 0, 6, 12, 18, 24, 30 ]
3 [ 6, 6, 6, 6, 6 ]
4 [ 0, 0, 0, 0 ]
5 [ 0, 0, 0 ]
6 [ 0, 0 ]
7 [ 0 ]
8 []
```



## Common Lisp



```lisp
(defun forward-difference (list)
  (mapcar #'- (rest list) list))

(defun nth-forward-difference (list n)
  (setf list (copy-list list))
  (loop repeat n do (map-into list #'- (rest list) list))
  (subseq list 0 (- (length list) n)))
```




## D


### Basic Version


```d
T[] forwardDifference(T)(in T[] data, in int level) pure nothrow
in {
    assert(level >= 0 && level < data.length);
} body {
    auto result = data.dup;
    foreach (immutable i; 0 .. level)
        foreach (immutable j, ref el; result[0 .. $ - i - 1])
            el = result[j + 1] - el;
    result.length -= level;
    return result;
}

void main() {
    import std.stdio;

    const data = [90.5, 47, 58, 29, 22, 32, 55, 5, 55, 73.5];
    foreach (immutable level; 0 .. data.length)
        forwardDifference(data, level).writeln;
}
```

```txt
[90.5, 47, 58, 29, 22, 32, 55, 5, 55, 73.5]
[-43.5, 11, -29, -7, 10, 23, -50, 50, 18.5]
[54.5, -40, 22, 17, 13, -73, 100, -31.5]
[-94.5, 62, -5, -4, -86, 173, -131.5]
[156.5, -67, 1, -82, 259, -304.5]
[-223.5, 68, -83, 341, -563.5]
[291.5, -151, 424, -904.5]
[-442.5, 575, -1328.5]
[1017.5, -1903.5]
[-2921]
```



### Alternative Version

Same output.

```d
import std.stdio, std.algorithm, std.range, std.array;

auto forwardDifference(Range)(Range d, in int level) pure {
    foreach (immutable _; 0 .. level)
        d = d.zip(d.dropOne).map!(a => a[0] - a[1]).array;
    return d;
}

void main() {
    const data = [90.5, 47, 58, 29, 22, 32, 55, 5, 55, 73.5];
    foreach (immutable level; 0 .. data.length)
        forwardDifference(data, level).writeln;
}
```



### Using Vector Operations

forwardDifference mutates the array in-place (same output):

```d
import std.stdio;

T[] forwardDifference(T)(T[] s, in int n) pure nothrow @nogc {
    foreach (immutable i; 0 .. n)
        s[0 .. $ - i - 1] = s[1 .. $ - i] - s[0 .. $ - i - 1];
    return s[0 .. $ - n];
}
void main() {
    immutable A = [90.5, 47, 58, 29, 22, 32, 55, 5, 55, 73.5];
    foreach (immutable level; 0 .. A.length)
        forwardDifference(A.dup, level).writeln;
}
```



### Short Functional Version

Same output:

```d
void main() {
  import std.stdio, std.range;

  auto D = [90.5, 47, 58, 29, 22, 32, 55, 5, 55, 73.5];
  writefln("%(%s\n%)",
    recurrence!q{ (a[n - 1][0 .. $ - 1] =
                   a[n - 1][1 .. $] -
                   a[n - 1][0 .. $ - 1])[0 .. $] }(D)
    .take(D.length));
}
```



## Dart


```dart
List forwardDifference(List _list) {
  for (int i = _list.length - 1; i > 0; i--) {
    _list[i] = _list[i] - _list[i - 1];
  }

  _list.removeRange(0, 1);
  return _list;
}

void mainAlgorithms() {
  List _intList = [90, 47, 58, 29, 22, 32, 55, 5, 55, 73];

  for (int i = _intList.length - 1; i >= 0; i--) {
    List _list = forwardDifference(_intList);
    print(_list);
  }
}
```


```txt

Restarted application in 1,143ms.
flutter: [-43, 11, -29, -7, 10, 23, -50, 50, 18]
flutter: [54, -40, 22, 17, 13, -73, 100, -32]
flutter: [-94, 62, -5, -4, -86, 173, -132]
flutter: [156, -67, 1, -82, 259, -305]
flutter: [-223, 68, -83, 341, -564]
flutter: [291, -151, 424, -905]
flutter: [-442, 575, -1329]
flutter: [1017, -1904]
flutter: [-2921]
flutter: []

```




## E



```e
pragma.enable("accumulator")
/** Single step. */
def forwardDifference(seq :List) {
    return accum [] for i in 0..(seq.size() - 2) {
        _.with(seq[i + 1] - seq[i])
    }
}

/** Iterative implementation of the goal. */
def nthForwardDifference1(var seq :List, n :(int >= 0)) {
    for _ in 1..n { seq := forwardDifference(seq) }
    return seq
}

/** Imperative implementation of the goal. */
def nthForwardDifference2(seq :List, n :(int >= 0)) {
  def buf := seq.diverge()
  def finalSize := seq.size() - n
  for lim in (finalSize..!seq.size()).descending() {
    for i in 0..!lim {
      buf[i] := buf[i + 1] - buf[i]
    }
  }
  return buf.run(0, finalSize)
}

? def sampleData := [90, 47, 58, 29, 22, 32, 55, 5, 55, 73]
> for n in 0..10 {
>   def r1 := nthForwardDifference1(sampleData, n)
>   require(r1 == nthForwardDifference2(sampleData, n))
>   println(r1)
> }
```



## EchoLisp

Using the built-in function '''iterate''' which, given a function f and n, returns the function f°f°f....°f .

```lisp

(define (Δ-1 list)
(for/list ([x (cdr list)] [y list]) (- x y)))

(define (Δ-n n) (iterate Δ-1 n))

((Δ-n 9) '(90 47 58 29 22 32 55 5 55 73))
    →  (-2921)

```



## Elixir


```Elixir
defmodule Diff do
  def forward(list,i\\1) do
    forward(list,[],i)
  end

  def forward([_],diffs,1), do: IO.inspect diffs
  def forward([_],diffs,i), do: forward(diffs,[],i-1)
  def forward([val1,val2|vals],diffs,i) do
    forward([val2|vals],diffs++[val2-val1],i)
  end
end

Enum.each(1..9, fn i ->
  Diff.forward([90, 47, 58, 29, 22, 32, 55, 5, 55, 73],i)
end)
```


```txt

[-43, 11, -29, -7, 10, 23, -50, 50, 18]
[54, -40, 22, 17, 13, -73, 100, -32]
[-94, 62, -5, -4, -86, 173, -132]
[156, -67, 1, -82, 259, -305]
[-223, 68, -83, 341, -564]
[291, -151, 424, -905]
[-442, 575, -1329]
[1017, -1904]
[-2921]

```



## Erlang


```erlang
-module(forward_difference).
-export([difference/1, difference/2]).

-export([task/0]).
-define(TEST_DATA,[90, 47, 58, 29, 22, 32, 55, 5, 55, 73]).

difference([X|Xs]) ->
    {Result,_} = lists:mapfoldl(fun (N_2,N_1) -> {N_2 - N_1, N_2} end, X, Xs),
    Result.

difference([],_) -> [];
difference(List,0) -> List;
difference(List,Order) -> difference(difference(List),Order-1).

task() ->
    io:format("Initial: ~p~n",[?TEST_DATA]),
    [io:format("~3b: ~p~n",[N,difference(?TEST_DATA,N)]) || N <- lists:seq(0,length(?TEST_DATA))],
    ok.
```

```txt
80> forward_difference:task().
Initial: [90,47,58,29,22,32,55,5,55,73]
  0: [90,47,58,29,22,32,55,5,55,73]
  1: [-43,11,-29,-7,10,23,-50,50,18]
  2: [54,-40,22,17,13,-73,100,-32]
  3: [-94,62,-5,-4,-86,173,-132]
  4: [156,-67,1,-82,259,-305]
  5: [-223,68,-83,341,-564]
  6: [291,-151,424,-905]
  7: [-442,575,-1329]
  8: [1017,-1904]
  9: [-2921]
 10: []
ok

```



## Factor


```factor
USING: kernel math math.vectors sequences ;
IN: rosetacode

: 1-order ( seq -- seq' )
    [ rest-slice ] keep v- ;

: n-order ( seq n -- seq' )
    dup 0 <=
    [ drop ] [ [ 1-order ] times ] if ;

```

  ( scratchpad ) { 90.5 47 58 29 22 32 55 5 55 73.5 } 4 n-order .
  { 156.5 -67 1 -82 259 -304.5 }


## Forth


```forth
: forward-difference
  dup 0
  ?do
     swap rot over i 1+ - 0
     ?do dup i cells + dup cell+ @ over @ - swap ! loop
     swap rot
  loop -
;

create a
  90 , 47 , 58 , 29 , 22 , 32 , 55 , 5 , 55 , 73 ,

: test a 10 9 forward-difference bounds ?do i ? loop ;

test
```


## Fortran

```fortran
MODULE DIFFERENCE
  IMPLICIT NONE

  CONTAINS

  SUBROUTINE Fdiff(a, n)
    INTEGER, INTENT(IN) :: a(:), n
    INTEGER :: b(SIZE(a))
    INTEGER :: i, j, arraysize

    b = a
    arraysize = SIZE(b)
    DO i = arraysize-1, arraysize-n, -1
      DO j = 1, i
        b(j) = b(j+1) - b(j)
      END DO
    END DO
    WRITE (*,*) b(1:arraysize-n)
  END SUBROUTINE Fdiff
END MODULE DIFFERENCE
```



```fortran
PROGRAM TEST

  USE DIFFERENCE
  IMPLICIT NONE

  INTEGER :: array(10) = (/ 90, 47, 58, 29, 22, 32, 55, 5, 55, 73 /)
  INTEGER :: i

  DO i = 1, 9
    CALL Fdiff(array, i)
  END DO

  END PROGRAM TEST
```

```txt

          -43          11         -29          -7          10          23         -50          50          18
           54         -40          22          17          13         -73         100         -32
          -94          62          -5          -4         -86         173        -132
          156         -67           1         -82         259        -305
         -223          68         -83         341        -564
          291        -151         424        -905
         -442         575       -1329
         1017       -1904
        -2921

```


=={{header|F_Sharp|F#}}==
Straightforward recursive solution

```fsharp
let rec ForwardDifference input n =
    match n with
    | _ when input = [] || n < 0 -> []      // If there's no more input, just return an empty list
    | 0 -> input                            // If n is zero, we're done - return the input
    | _ -> ForwardDifference                // otherwise, recursively difference..
            (input.Tail                     // All but the first element
            |> Seq.zip input                // tupled with itself
            |> Seq.map (fun (a, b) -> b-a)  // Subtract the i'th element from the (i+1)'th
            |> Seq.toList) (n-1)            // Make into a list and do an n-1 difference on it
```



## Go


```go
package main

import "fmt"

func main() {
    a := []int{90, 47, 58, 29, 22, 32, 55, 5, 55, 73}
    fmt.Println(a)
    fmt.Println(fd(a, 9))
}

func fd(a []int, ord int) []int {
    for i := 0; i < ord; i++ {
        for j := 0; j < len(a)-i-1; j++ {
            a[j] = a[j+1] - a[j]
        }
    }
    return a[:len(a)-ord]
}
```

```txt

[90 47 58 29 22 32 55 5 55 73]
[-2921]

```



## Haskell



```haskell
forwardDifference xs = zipWith (-) (tail xs) xs

nthForwardDifference xs n = iterate forwardDifference xs !! n

> take 10 (iterate forwardDifference [90, 47, 58, 29, 22, 32, 55, 5, 55, 73])
[[90,47,58,29,22,32,55,5,55,73],
 [-43,11,-29,-7,10,23,-50,50,18],
 [54,-40,22,17,13,-73,100,-32],
 [-94,62,-5,-4,-86,173,-132],
 [156,-67,1,-82,259,-305],
 [-223,68,-83,341,-564],
 [291,-151,424,-905],
 [-442,575,-1329],
 [1017,-1904],
 [-2921]]
```



## HicEst


```hicest
REAL :: n=10, list(n)

list = ( 90, 47, 58, 29, 22, 32, 55, 5, 55, 73 )
WRITE(Format='i1, (i6)') 0, list

DO i = 1, n-1
  ALIAS(list,1,  diff,n-i) ! rename list(1 ... n-i) with diff
  diff = list($+1) - diff  ! $ is the running left hand array index
  WRITE(Format='i1, (i6)') i, diff
ENDDO

END
```

```txt
0    90    47    58    29    22    32    55     5    55    73
1   -43    11   -29    -7    10    23   -50    50    18
2    54   -40    22    17    13   -73   100   -32
3   -94    62    -5    -4   -86   173  -132
4   156   -67     1   -82   259  -305
5  -223    68   -83   341  -564
6   291  -151   424  -905
7  -442   575 -1329
8  1017 -1904
9 -2921
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main(A)    # Compute all forward difference orders for argument list
    every order := 1 to (*A-1) do showList(order, fdiff(A, order))
end

procedure fdiff(A, order)
    every 1 to order do {
        every put(B := [], A[i := 2 to *A] - A[i-1])
        A := B
        }
   return A
end

procedure showList(order, L)
    writes(right(order,3),": ")
    every writes(!L," ")
    write()
end
```


```txt

->fdiff 3 1 4 1 5 9 2 6 3
  1: -2 3 -3 4 4 -7 4 -3
  2: 5 -6 7 0 -11 11 -7
  3: -11 13 -7 -11 22 -18
  4: 24 -20 -4 33 -40
  5: -44 16 37 -73
  6: 60 21 -110
  7: -39 -131
  8: -92
->

```



## IDL


Standard IDL library function <tt>TS_diff(X,k,[/double])</tt>:


```idl
print,(x = randomu(seed,8)*100)
     15.1473      58.0953      82.7465      16.8637      97.7182      59.7856      17.7699      74.9154
print,ts_diff(x,1)
    -42.9479     -24.6513      65.8828     -80.8545      37.9326      42.0157     -57.1455     0.000000
print,ts_diff(x,2)
    -18.2967     -90.5341      146.737     -118.787     -4.08316      99.1613     0.000000     0.000000
print,ts_diff(x,3)
     72.2374     -237.271      265.524     -114.704     -103.244     0.000000     0.000000     0.000000
```



## J

Of the many ways to code this in J, a particularly concise solution is:

```j
fd=: 2&(-~/\)
```


Alternatively, to reduce the number of J primitives, use:

```j
fd=: }. - }: ^:
```


(which is also elegant, because the open-ended power conjunction reads like "to the power of anything").

For example:

```j
   list=: 90 47 58 29 22 32 55 5 55 73   NB.  Some numbers

   1 fd list
_43 11 _29 _7 10 23 _50 50 18

   2 fd list
54 _40 22 17 13 _73 100 _32
```


J is array oriented, so you can even ask for more than one forward difference at a time (i.e. <tt>N</tt> can be a list, instead of a single number):

```j
   1 2 3 fd list                          NB.  First, second, and third forward differences (simultaneously)
43 _11 29  7 _10  _23  50 _50 _18
54 _40 22 17  13  _73 100 _32   0
94 _62  5  4  86 _173 132   0   0

   a: fd list                             NB.  All forward differences
  90    47   58   29  22   32  55   5  55 73
  43   _11   29    7 _10  _23  50 _50 _18  0
  54   _40   22   17  13  _73 100 _32   0  0
  94   _62    5    4  86 _173 132   0   0  0
 156   _67    1  _82 259 _305   0   0   0  0
 223   _68   83 _341 564    0   0   0   0  0
 291  _151  424 _905   0    0   0   0   0  0
 442  _575 1329    0   0    0   0   0   0  0
1017 _1904    0    0   0    0   0   0   0  0
2921     0    0    0   0    0   0   0   0  0
   0     0    0    0   0    0   0   0   0  0
```



## Java

```java
import java.util.Arrays;
public class FD {
    public static void main(String args[]) {
        double[] a = {90, 47, 58, 29, 22, 32, 55, 5, 55, 73};
        System.out.println(Arrays.toString(dif(a, 1)));
        System.out.println(Arrays.toString(dif(a, 2)));
        System.out.println(Arrays.toString(dif(a, 9)));
        System.out.println(Arrays.toString(dif(a, 10)));      //let's test
        System.out.println(Arrays.toString(dif(a, 11)));
        System.out.println(Arrays.toString(dif(a, -1)));
        System.out.println(Arrays.toString(dif(a, 0)));
    }

    public static double[] dif(double[] a, int n) {
        if (n < 0)
            return null; // if the programmer was dumb

        for (int i = 0; i < n && a.length > 0; i++) {
            double[] b = new double[a.length - 1];
            for (int j = 0; j < b.length; j++){
                b[j] = a[j+1] - a[j];
            }
            a = b; //"recurse"
        }
        return a;
    }
}
```


 [-43.0, 11.0, -29.0, -7.0, 10.0, 23.0, -50.0, 50.0, 18.0]
 [54.0, -40.0, 22.0, 17.0, 13.0, -73.0, 100.0, -32.0]
 [-2921.0]
 []
 []
 null
 [90.0, 47.0, 58.0, 29.0, 22.0, 32.0, 55.0, 5.0, 55.0, 73.0]


## JavaScript



### ES6



```JavaScript
(() => {
    'use strict';

    // forwardDifference :: [Int] -> [Int]
    const forwardDifference = (n, xs) => {
        const fd = xs => zipWith((a, b) => a - b, tail(xs), xs);
        return until(
                m => m.index < 1,
                m => ({
                    index: m.index - 1,
                    list: fd(m.list)
                }), {
                    index: n,
                    list: xs
                }
            )
            .list;
    };


    // GENERIC FUNCTIONS ---------------------------------------

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) => {
        const ny = ys.length;
        return (xs.length <= ny ? xs : xs.slice(0, ny))
            .map((x, i) => f(x, ys[i]));
    };

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        const go = x => p(x) ? x : go(f(x));
        return go(x);
    };

    // tail :: [a] -> [a]
    const tail = xs => xs.length ? xs.slice(1) : undefined;


    // TEST ----------------------------------------------------

    // range :: Int -> Int -> [Int]
    const range = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // show :: a -> String
    const show = x => JSON.stringify(x);

    // Sample
    const test = [90, 47, 58, 29, 22, 32, 55, 5, 55, 73];

    return range(1, 9)
        .map(x => `${x}    ${show(forwardDifference(x, test))}`)
        .join('\n');
})();
```


```txt
1    [-43,11,-29,-7,10,23,-50,50,18]
2    [54,-40,22,17,13,-73,100,-32]
3    [-94,62,-5,-4,-86,173,-132]
4    [156,-67,1,-82,259,-305]
5    [-223,68,-83,341,-564]
6    [291,-151,424,-905]
7    [-442,575,-1329]
8    [1017,-1904]
9    [-2921]
```



## jq

```jq
# If n is a non-negative number and if input is
# a (possibly empty) array of numbers,
# emit an array, even if the input list is too short:
def ndiff(n):
  if n==0 then .
  elif n == 1 then . as $in | [range(1;length) | $in[.] - $in[.-1]]
  else ndiff(1) | ndiff(n-1)
  end;
```

'''Example''':

```jq
def s: [90, 47, 58, 29, 22, 32, 55, 5, 55, 73];

range(0;12) as $i | (s|ndiff($i))
```

```sh
$ jq -c -n -f forward-difference.jq
[90,47,58,29,22,32,55,5,55,73]
[-43,11,-29,-7,10,23,-50,50,18]
[54,-40,22,17,13,-73,100,-32]
[-94,62,-5,-4,-86,173,-132]
[156,-67,1,-82,259,-305]
[-223,68,-83,341,-564]
[291,-151,424,-905]
[-442,575,-1329]
[1017,-1904]
[-2921]
[]
[]
```



## Julia

Using the built-in <code>diff</code> function, which returns the 1st forward difference:


```julia
ndiff(A::Array, n::Integer) = n < 1 ? A : diff(ndiff(A, n-1))

s = [90, 47, 58, 29, 22, 32, 55, 5, 55, 73]
println.(collect(ndiff(s, i) for i in 0:9))
```


```txt
[90, 47, 58, 29, 22, 32, 55, 5, 55, 73]
[-43, 11, -29, -7, 10, 23, -50, 50, 18]
[54, -40, 22, 17, 13, -73, 100, -32]
[-94, 62, -5, -4, -86, 173, -132]
[156, -67, 1, -82, 259, -305]
[-223, 68, -83, 341, -564]
[291, -151, 424, -905]
[-442, 575, -1329]
[1017, -1904]
[-2921]
```



## K4


```k4
fd:1_-':
```


To compute the ''n''th forward difference, call as:

```k4>n fd/</lang


In order to obtain all intermediate results, call as:

```k4>n fd\</lang


```txt
  fd 1 2 4 7 11 16
1 2 3 4 5
  2 fd/1 2 4 7 11 16
1 1 1 1
  3 fd\1 2 4 7 11 16
(1 2 4 7 11 16;1 2 3 4 5;1 1 1 1;0 0 0)
```



## Kotlin


```scala
// version 1.1.2

fun forwardDifference(ia: IntArray, order: Int): IntArray {
    if (order < 0) throw IllegalArgumentException("Order must be non-negative")
    if (order == 0) return ia
    val size = ia.size
    if (size == 0) return ia  // same empty array
    if (order >= size) return intArrayOf()  // new empty array
    var old = ia
    var new = old
    var count = order
    while (count-- >= 1) {
       new = IntArray(old.size - 1)
       for (i in 0 until new.size) new[i] = old[i + 1] - old[i]
       old = new
    }
    return new
}

fun printArray(ia: IntArray) {
    print("[")
    for (i in 0 until ia.size) {
        print("%5d".format(ia[i]))
        if (i < ia .size - 1) print(", ")
    }
    println("]")
}

fun main(args: Array<String>) {
    val ia = intArrayOf(90, 47, 58, 29, 22, 32, 55, 5, 55, 73)
    for (order in 0..ia.size) {
        val fd = forwardDifference(ia, order)
        print("%2d".format(order) + ":  ")
        printArray(fd)
    }
}
```


```txt

 0:  [   90,    47,    58,    29,    22,    32,    55,     5,    55,    73]
 1:  [  -43,    11,   -29,    -7,    10,    23,   -50,    50,    18]
 2:  [   54,   -40,    22,    17,    13,   -73,   100,   -32]
 3:  [  -94,    62,    -5,    -4,   -86,   173,  -132]
 4:  [  156,   -67,     1,   -82,   259,  -305]
 5:  [ -223,    68,   -83,   341,  -564]
 6:  [  291,  -151,   424,  -905]
 7:  [ -442,   575, -1329]
 8:  [ 1017, -1904]
 9:  [-2921]
10:  []

```



## Lasso


```lasso
#!/usr/bin/lasso9

define forwardDiff(values, order::integer=1) => {
  !#order ? return #values->asArray
  local(result = array)
  iterate(#values) => {
    loop_count < #values->size ?
      #result->insert(#values->get(loop_count+1) - #values->get(loop_count))
  }
  #order > 1 ? #result = forwardDiff(#result, #order-1)
  return #result
}

local(data = (:90, 47, 58, 29, 22, 32, 55, 5, 55, 73))
with x in generateSeries(0, #data->size-1)
do stdoutnl(#x + ': ' + forwardDiff(#data, #x))
```

```txt
0: array(90, 47, 58, 29, 22, 32, 55, 5, 55, 73)
1: array(-43, 11, -29, -7, 10, 23, -50, 50, 18)
2: array(54, -40, 22, 17, 13, -73, 100, -32)
3: array(-94, 62, -5, -4, -86, 173, -132)
4: array(156, -67, 1, -82, 259, -305)
5: array(-223, 68, -83, 341, -564)
6: array(291, -151, 424, -905)
7: array(-442, 575, -1329)
8: array(1017, -1904)
9: array(-2921)
```



## Logo


```logo
to fwd.diff :l
  if empty? :l [output []]
  if empty? bf :l [output []]
  output fput (first bf :l)-(first :l) fwd.diff bf :l
end
to nth.fwd.diff :n :l
  if :n = 0 [output :l]
  output nth.fwd.diff :n-1 fwd.diff :l
end

show nth.fwd.diff 9 [90 47 58 29 22 32 55 5 55 73]
[-2921]
```



## Lua


```lua
function dif(a, b, ...)
  if(b) then return b-a, dif(b, ...) end
end
function dift(t) return {dif(unpack(t))} end
print(unpack(dift{1,3,6,10,15}))
```



## M2000 Interpreter

Function Diff(a()) get an array by value (a shallow copy)

```M2000 Interpreter

Form 80, 40
Module Forward_difference {
      Print $(0,6)  ' 6 characters column
      Dim a(), b()
      a()=(90,47,58,29,22,32,55,5,55,73)
      Function Diff(a()) {
            for i=0 to len(a())-2: a(i)=a(i+1)-a(i):Next i
            Dim a(len(a())-1) ' redim one less
            =a()
      }
      Print "Original:","",a()
      b()=a()    ' copy a() to b()
      k=1
      While len(b())>1 {
            b()=Diff(b())   ' copy returned array to b()
            Print "Difference ";k;":",b()
            k++
      }
}
Forward_difference


```

<pre style="height:30ex;overflow:scroll">
Original:             90    47    58    29    22    32    55    5    55    73
Difference 1:        -43    11   -29    -7    10    23   -50   50    18
Difference 2:         54   -40    22    17    13   -73   100  -32
Difference 3:        -94    62    -5    -4   -86   173  -132
Difference 4:        156   -67     1   -82   259  -305
Difference 5:       -223    68   -83   341  -564
Difference 6:        291  -151   424  -905
Difference 7:       -442   575 -1329
Difference 8:       1017 -1904
Difference 9:      -2921
</pre >

=={{header|Mathematica}} / {{header|Wolfram Language}}==
Built-in function:

```Mathematica
i={3,5,12,1,6,19,6,2,4,9};
Differences[i]
```

```Mathematica
{2, 7, -11, 5, 13, -13, -4, 2, 5}
```

The n<sup>th</sup> difference can be done as follows:

```Mathematica
i={3,5,12,1,6,19,6,2,4,9};
Differences[i,n]
```


=={{header|MATLAB}} / {{header|Octave}}==

This is a built-in function.
X is the list of numbers,
n is the order of the forward difference.

```MATLAB
Y = diff(X,n);
```


{{out}} 1st order forward difference.

```txt
diff([1 2 3 4 5])

ans =

     1     1     1     1
```



## Maxima


```maxima
ldiff(u, n) := block([m: length(u)], for j thru n do u: makelist(u[i + 1] - u[i], i, 1, m - j), u);
```



## NetRexx


```NetRexx
/* NetRexx*************************************************************
* Forward differences
* 18.08.2012 Walter Pachl derived from Rexx
**********************************************************************/
  Loop n=-1 To 11
    differences('90 47 58 29 22 32 55 5 55 73',n)
    End

  method differences(a,n) public static
  --arr=Rexx[11]                       -- array must be declared (zero based)
    arr=''                             -- alternative: indexed string
    m=a.words
    Select
      When n<0 Then Say 'n is negative:' n '<' 0
      When n>m Then Say 'n is too large:' n '>' m
      Otherwise Do
        Loop i=1 To m
          arr[i]=a.word(i)
          End
        Loop i = 1 to n
          t = arr[i]
          Loop j = i+1 to m
            u = arr[j]
            arr[j] = arr[j]-t
            t = u
            end
          end
        ol=''
        Loop i=n+1 to m
          ol=ol arr[i]
          End
        Say n ol
        End
      End
```

Output is the same as for Rexx


## Nial

Define forward difference for order 1

```nial
fd is - [rest, front]
```


{{out}} forward difference of 4th order

```nial
b := 90 47 58 29 22 32 55 5 55 73
4 fold fd b
= 156 -67 1 -82 259 -305
```



## Nim


```nim
proc dif(s): seq[int] =
  result = newSeq[int](s.len-1)
  for i, x in s[1..s.high]:
    result[i] = x - s[i]

proc difn(s, n): seq[int] =
  if n > 0: difn(dif(s), n-1)
  else: s

const s = @[90, 47, 58, 29, 22, 32, 55, 5, 55, 73]
echo difn(s, 0)
echo difn(s, 1)
echo difn(s, 2)
```

```txt
@[90, 47, 58, 29, 22, 32, 55, 5, 55, 73]
@[-43, 11, -29, -7, 10, 23, -50, 50, 18]
@[54, -40, 22, 17, 13, -73, 100, -32]
```



## Objeck

```objeck

bundle Default {
  class Test {
    function : Main(args : String[]) ~ Nil {
      a := [90.0, 47.0, 58.0, 29.0, 22.0, 32.0, 55.0, 5.0, 55.0, 73.0];
      Print(Diff(a, 1));
      Print(Diff(a, 2));
      Print(Diff(a, 9));
    }

    function : Print(a : Float[]) ~ Nil {
      if(a <> Nil) {
        '['->Print();
        each(i : a) {
          a[i]->Print(); ','->Print();
        };
        ']'->PrintLine();
      };
    }

    function : Diff(a : Float[], n : Int) ~ Float[] {
      if (n < 0) {
        return Nil;
       };

      for(i := 0; i < n & a->Size() > 0; i += 1;) {
        b := Float->New[a->Size() - 1];
        for(j := 0; j < b->Size(); j += 1;){
          b[j] := a[j+1] - a[j];
        };
        a := b;
      };

      return a;
    }
  }
}

```



## OCaml



```ocaml
let rec forward_difference = function
    a :: (b :: _ as xs) ->
      b - a :: forward_difference xs
  | _ ->
      []

let rec nth_forward_difference n xs =
  if n = 0 then
    xs
  else
    nth_forward_difference (pred n) (forward_difference xs)
```


```txt

# nth_forward_difference 9 [90; 47; 58; 29; 22; 32; 55; 5; 55; 73];;
- : int list = [-2921]

```



## Oforth



```Oforth
: forwardDiff(l)     l right(l size 1 -) l zipWith(#-) ;
: forwardDiffN(n, l)  l #[ forwardDiff dup println ] times(n) ;
```

```txt

10 [ 90, 47, 58, 29, 22, 32, 55, 5, 55, 73] forwardDiffN
[-43, 11, -29, -7, 10, 23, -50, 50, 18]
[54, -40, 22, 17, 13, -73, 100, -32]
[-94, 62, -5, -4, -86, 173, -132]
[156, -67, 1, -82, 259, -305]
[-223, 68, -83, 341, -564]
[291, -151, 424, -905]
[-442, 575, -1329]
[1017, -1904]
[-2921]
[]

```



## PARI/GP


```parigp
fd(v)=vector(#v-1,i,v[i+1]-v[i]);
```



## Pascal


```pascal
Program ForwardDifferenceDemo(output);

procedure fowardDifference(list: array of integer);
  var
    b: array of integer;
    i, newlength: integer;
  begin
    newlength := length(list) - 1;
    if newlength > 0 then
    begin
      setlength(b, newlength);
      for i := low(b) to high(b) do
      begin
        b[i] := list[i+1] - list[i];
        write (b[i]:6);
      end;
      writeln;
      fowardDifference(b);
    end;
  end;

var
  a: array [1..10] of integer = (90, 47, 58, 29, 22, 32, 55, 5, 55, 73);
begin
  fowardDifference(a);
end.
```

```txt
:> ./ForwardDifference
   -43    11   -29    -7    10    23   -50    50    18
    54   -40    22    17    13   -73   100   -32
   -94    62    -5    -4   -86   173  -132
   156   -67     1   -82   259  -305
  -223    68   -83   341  -564
   291  -151   424  -905
  -442   575 -1329
  1017 -1904
 -2921

```



## Perl


```perl
sub dif {
  my @s = @_;
  map { $s[$_+1] - $s[$_] } 0 .. $#s-1
}

@a = qw<90 47 58 29 22 32 55 5 55 73>;
while (@a) { printf('%6d', $_) for @a = dif @a; print "\n" }
```

```txt
   -43    11   -29    -7    10    23   -50    50    18
    54   -40    22    17    13   -73   100   -32
   -94    62    -5    -4   -86   173  -132
   156   -67     1   -82   259  -305
  -223    68   -83   341  -564
   291  -151   424  -905
  -442   575 -1329
  1017 -1904
 -2921
```



## Perl 6

<p>Here we use signature matching to bind both an entire array and a version missing the head.
The Z- operator is a zip metaoperator with a minus to subtract the two lists pairwise.
It's almost a shame to define difn, since the series and subscript are hardly longer than the call itself would be, and arguably more self-documenting than the name of the function would be.

```perl6
sub dif(@array [$, *@tail]) { @tail Z- @array }
sub difn($array, $n) { ($array, &dif ... *)[$n] }
```



## Phix


```Phix
function fwd_diff_n(sequence s, integer order)
    if order>=length(s) then ?9/0 end if
    for i=1 to order do
        for j=1 to length(s)-1 do
            s[j] = s[j+1]-s[j]
        end for
        s = s[1..-2]
    end for
    return s
end function

constant s = {90, 47, 58, 29, 22, 32, 55, 5, 55, 73}
for i=1 to 9 do
    ?fwd_diff_n(s,i)
end for
```

```txt

{-43,11,-29,-7,10,23,-50,50,18}
{54,-40,22,17,13,-73,100,-32}
{-94,62,-5,-4,-86,173,-132}
{156,-67,1,-82,259,-305}
{-223,68,-83,341,-564}
{291,-151,424,-905}
{-442,575,-1329}
{1017,-1904}
{-2921}

```



## PHP



```php
<?php

function forwardDiff($anArray, $times = 1) {
  if ($times <= 0) { return $anArray; }
  for ($accumilation = array(), $i = 1, $j = count($anArray); $i < $j; ++$i) {
    $accumilation[] = $anArray[$i] - $anArray[$i - 1];
  }
  if ($times === 1) { return $accumilation; }
  return forwardDiff($accumilation, $times - 1);
}

class ForwardDiffExample extends PweExample {

  function _should_run_empty_array_for_single_elem() {
    $expected = array($this->rand()->int());
    $this->spec(forwardDiff($expected))->shouldEqual(array());
  }

  function _should_give_diff_of_two_elem_as_single_elem() {
    $twoNums = array($this->rand()->int(), $this->rand()->int());
    $expected = array($twoNums[1] - $twoNums[0]);
    $this->spec(forwardDiff($twoNums))->shouldEqual($expected);
  }

  function _should_compute_correct_forward_diff_for_longer_arrays() {
    $diffInput = array(10, 2, 9, 6, 5);
    $expected  = array(-8, 7, -3, -1);
    $this->spec(forwardDiff($diffInput))->shouldEqual($expected);
  }

  function _should_apply_more_than_once_if_specified() {
    $diffInput = array(4, 6, 9, 3, 4);
    $expectedAfter1 = array(2, 3, -6, 1);
    $expectedAfter2 = array(1, -9, 7);
    $this->spec(forwardDiff($diffInput, 1))->shouldEqual($expectedAfter1);
    $this->spec(forwardDiff($diffInput, 2))->shouldEqual($expectedAfter2);
  }

  function _should_return_array_unaltered_if_no_times() {
    $this->spec(forwardDiff($expected = array(1,2,3), 0))->shouldEqual($expected);
  }

}
```



## PicoLisp


```PicoLisp
(de fdiff (Lst)
   (mapcar - (cdr Lst) Lst) )

(for (L (90 47 58 29 22 32 55 5 55 73) L (fdiff L))
   (println L) )
```

```txt
(90 47 58 29 22 32 55 5 55 73)
(-43 11 -29 -7 10 23 -50 50 18)
(54 -40 22 17 13 -73 100 -32)
(-94 62 -5 -4 -86 173 -132)
(156 -67 1 -82 259 -305)
(-223 68 -83 341 -564)
(291 -151 424 -905)
(-442 575 -1329)
(1017 -1904)
(-2921)
```



## PL/I


```PL/I

/* Forward differences. */ /* 23 April 2010 */
differences: procedure options (main);
   declare a(10) fixed(10) static initial
      (7, 3, 9, 250, 300, 4, 68, 72, 154, 601);
   declare (i, j, m, n, t, u) fixed binary;

   m = hbound(a,1);
   get (n); if n > m then signal error;
   put skip edit (a) (F(7));
   do i = 1 to n;
      t = a(i);
      do j = i+1 to m;
         u = a(j);
         a(j) = a(j) - t;
         t = u;
      end;
      put skip edit (a) (F(7));
   end;
   put skip edit ((a(i) do i = n+1 to m)) (F(9));

end differences;

```



## Pop11


```pop11
define forward_difference(l);
    lvars res = [], prev, el;
    if l = [] then
        return([]);
    endif;
    front(l) -> prev;
    for el in back(l) do
        cons(el - prev, res) -> res;
        el -> prev;
    endfor;
    rev(res);
enddefine;

define nth_difference(l, n);
    lvars res = l, i;
    for i from 1 to n do
        forward_difference(res) -> res;
    endfor;
    res;
enddefine;
```



## PowerShell

<math>\Delta^n [f](x)= \sum_{k=0}^n \left ({\prod_{i=1}^k \frac{n-k+i}{i}}\right ) (-1)^{n-k} f(x+k)</math>

```PowerShell
function Forward-Difference( [UInt64] $n, [Array] $f )
{
	$flen = $f.length
	if( $flen -gt [Math]::Max( 1, $n ) )
	{
		0..( $flen - $n - 1 ) | ForEach-Object {
			$l=0;
			for( $k = 0; $k -le $n; $k++ )
			{
				$j = 1
				for( $i = 1; $i -le $k; $i++ )
				{
					$j *= ( ( $n - $k + $i ) / $i )
				}
				$l += $j * ( 1 - 2 * ( ( $n - $k ) % 2 ) ) * $f[ $_ + $k ]
			}
			$l
		}
	}
}

Forward-Difference 2 1,2,4,5
```



## PureBasic


```PureBasic
Procedure forward_difference(List a())
  If ListSize(a()) <= 1
    ClearList(a()): ProcedureReturn
  EndIf
  Protected NewList b()
  CopyList(a(), b())
  LastElement(a()): DeleteElement(a())
  SelectElement(b(), 1)
  ForEach a()
    a() - b(): NextElement(b())
  Next
EndProcedure

Procedure nth_difference(List a(), List b(), n)
  Protected i
  CopyList(a(), b())
  For i = 1 To n
    forward_difference(b())
  Next
EndProcedure

Procedure.s display(List a())
  Protected output.s
  ForEach a()
    output + Str(a()) + ","
  Next
  ProcedureReturn RTrim(output,",")
EndProcedure

DataSection
  ;list data
  Data.i 10 ;element count
  Data.i 90, 47, 58, 29, 22, 32, 55, 5, 55, 73
EndDataSection

;create and fill list
Define i
NewList a()
Read.i i
While i > 0
  AddElement(a()): Read.i a(): i - 1
Wend

If OpenConsole()
  NewList b()
  For i = 1 To 10
    nth_difference(a(), b(), i)
    PrintN(Str(i) + "   [" + display(b()) + "]")
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf


```

```txt
1   [43,-11,29,7,-10,-23,50,-50,-18]
2   [54,-40,22,17,13,-73,100,-32]
3   [94,-62,5,4,86,-173,132]
4   [156,-67,1,-82,259,-305]
5   [223,-68,83,-341,564]
6   [291,-151,424,-905]
7   [442,-575,1329]
8   [1017,-1904]
9   [2921]
10   []
```



## Python


```python>>>
 dif = lambda s: [x-s[i] for i,x in enumerate(s[1:])]
>>> # or, dif = lambda s: [x-y for x,y in zip(s[1:],s)]
>>> difn = lambda s, n: difn(dif(s), n-1) if n else s

>>> s = [90, 47, 58, 29, 22, 32, 55, 5, 55, 73]
>>> difn(s, 0)
[90, 47, 58, 29, 22, 32, 55, 5, 55, 73]
>>> difn(s, 1)
[-43, 11, -29, -7, 10, 23, -50, 50, 18]
>>> difn(s, 2)
[54, -40, 22, 17, 13, -73, 100, -32]

>>> from pprint import pprint
>>> pprint( [difn(s, i) for i in xrange(10)] )
[[90, 47, 58, 29, 22, 32, 55, 5, 55, 73],
 [-43, 11, -29, -7, 10, 23, -50, 50, 18],
 [54, -40, 22, 17, 13, -73, 100, -32],
 [-94, 62, -5, -4, -86, 173, -132],
 [156, -67, 1, -82, 259, -305],
 [-223, 68, -83, 341, -564],
 [291, -151, 424, -905],
 [-442, 575, -1329],
 [1017, -1904],
 [-2921]]
```



## R


```R
forwarddif <- function(a, n) {
  if ( n == 1 )
    a[2:length(a)] - a[1:length(a)-1]
  else {
    r <- forwarddif(a, 1)
    forwarddif(r, n-1)
  }
}

fdiff <- function(a, n) {
  r <- a
  for(i in 1:n) {
    r <- r[2:length(r)] - r[1:length(r)-1]
  }
  r
}

v <- c(90, 47, 58, 29, 22, 32, 55, 5, 55, 73)

print(forwarddif(v, 9))
print(fdiff(v, 9))
```



## Racket



```Racket

#lang racket

(define (forward-difference list)
  (for/list ([x (cdr list)] [y list]) (- x y)))

(define (nth-forward-difference n list)
  (for/fold ([list list]) ([n n]) (forward-difference list)))


(nth-forward-difference 9 '(90 47 58 29 22 32 55 5 55 73))
;; -> '(-2921)

```



## REXX


### no error checking

The REXX version uses the same (input) numbers (for the default) as the   '''Ada'''   example.

This version allows a specification of the list of numbers and/or which   ''order''   to process.

```rexx
/*REXX program  computes the   forward difference   of a  list of numbers.              */
numeric digits 100                               /*ensure enough accuracy (decimal digs)*/
parse arg e ',' N                                /*get a list:  ε1 ε2 ε3 ε4 ··· , order */
if e==''  then e=90 47 58 29 22 32 55 5 55 73    /*Not specified?  Then use the default.*/
#=words(e)                                       /*#  is the number of elements in list.*/
                                                 /* [↓]  assign list numbers to @ array.*/
   do i=1  for #;  @.i=word(e, i)/1;  end /*i*/  /*process each number one at a time.   */
                                                 /* [↓]  process the optional order.    */
if N==''  then parse value 0 # # with bot top N  /*define the default  order  range.    */
          else parse var N bot 1 top             /*Not specified?  Then use only 1 order*/
say right(#  'numbers:', 44)  e                  /*display the header title   and  ···  */
say left('', 44)copies('─', length(e)+2)         /*   "     "     "   fence.            */
                                                 /* [↓]  where da rubber meets da road. */
      do o=bot  to top;        do r=1  for #;  !.r=@.r;     end /*r*/;        $=
        do j=1  for o; d=!.j;  do k=j+1  to #; parse value !.k !.k-d with d !.k; end /*k*/
        end   /*j*/
                               do i=o+1  to #; $=$ !.i/1;   end /*i*/
      if $==''  then $=' [null]'
      say right(o, 7)th(o)'─order forward difference vector ='     $
      end     /*o*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
th: procedure; x=abs(arg(1)); return word('th st nd rd',1+x//10*(x//100%10\==1)*(x//10<4))
```

'''output'''   when using the default input:

```txt

                                 10 numbers: 90 47 58 29 22 32 55 5 55 73
                                            ──────────────────────────────
      0th-order forward difference vector =  90 47 58 29 22 32 55 5 55 73
      1st-order forward difference vector =  -43 11 -29 -7 10 23 -50 50 18
      2nd-order forward difference vector =  54 -40 22 17 13 -73 100 -32
      3rd-order forward difference vector =  -94 62 -5 -4 -86 173 -132
      4th-order forward difference vector =  156 -67 1 -82 259 -305
      5th-order forward difference vector =  -223 68 -83 341 -564
      6th-order forward difference vector =  291 -151 424 -905
      7th-order forward difference vector =  -442 575 -1329
      8th-order forward difference vector =  1017 -1904
      9th-order forward difference vector =  -2921
     10th-order forward difference vector =  [null]

```

'''output'''   when the '''Tcl''''s input was used:   <tt> 90.5 47 58 29 22 32 55 5 55 73.5 </tt>

```txt

                                 10 numbers: 90.5 47 58 29 22 32 55 5 55 73.5
                                            ──────────────────────────────────
      0th-order forward difference vector =  90.5 47 58 29 22 32 55 5 55 73.5
      1st-order forward difference vector =  -43.5 11 -29 -7 10 23 -50 50 18.5
      2nd-order forward difference vector =  54.5 -40 22 17 13 -73 100 -31.5
      3rd-order forward difference vector =  -94.5 62 -5 -4 -86 173 -131.5
      4th-order forward difference vector =  156.5 -67 1 -82 259 -304.5
      5th-order forward difference vector =  -223.5 68 -83 341 -563.5
      6th-order forward difference vector =  291.5 -151 424 -904.5
      7th-order forward difference vector =  -442.5 575 -1328.5
      8th-order forward difference vector =  1017.5 -1903.5
      9th-order forward difference vector =  -2921
     10th-order forward difference vector =  [null]

```



### with error checking


```rexx
/*REXX program  computes the   forward difference   of a  list of numbers.              */
numeric digits 100                               /*ensure enough accuracy (decimal digs)*/
parse arg e ',' N                                /*get a list:  ε1 ε2 ε3 ε4 ··· , order */
if e==''  then e=90 47 58 29 22 32 55 5 55 73    /*Not specified?  Then use the default.*/
#=words(e)                                       /*#  is the number of elements in list.*/
                                                 /* [↓]  verify list items are numeric. */
   do i=1  for #;        _=word(e, i)            /*process each number one at a time.   */
   if \datatype(_, 'N')  then call ser    _    "isn't a valid number";    @.i=_/1
   end   /*i*/                                   /* [↑]  removes superfluous stuff.     */
                                                 /* [↓]  process the optional order.    */
if N==''  then parse value 0 # # with bot top N  /*define the default  order  range.    */
          else parse var N bot 1 top             /*Not specified?  Then use only 1 order*/
if #==0   then call ser     "no numbers were specified."
if N<0    then call ser  N  "(order) can't be negative."
if N>#    then call ser  N  "(order) can't be greater than"  #
say right(#  'numbers:', 44)  e                  /*display the header (title)  and ···  */
say left('', 44)copies('─', length(e)+2)         /*display the header fence.            */
                                                 /* [↓]  where da rubber meets da road. */
      do o=bot  to top;        do r=1  for #;  !.r=@.r;     end /*r*/;        $=
        do j=1  for o; d=!.j;  do k=j+1  to #; parse value !.k !.k-d with d !.k; end /*k*/
        end   /*j*/
                               do i=o+1  to #; $=$ !.i/1;   end /*i*/
      if $==''  then $=' [null]'
      say right(o, 7)th(o)'─order forward difference vector ='     $
      end     /*o*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ser:           say;           say '***error***';      say arg(1);     say;         exit 13
th: procedure; x=abs(arg(1)); return word('th st nd rd',1+x//10*(x//100%10\==1)*(x//10<4))
```

'''output'''   is the same as the REXX entry above.


### with output alignment


```rexx
/*REXX program  computes the   forward difference   of a  list of numbers.              */
numeric digits 100                               /*ensure enough accuracy (decimal digs)*/
parse arg e ',' N                                /*get a list:  ε1 ε2 ε3 ε4 ··· , order */
if e==''  then e=90 47 58 29 22 32 55 5 55 73    /*Not specified?  Then use the default.*/
#=words(e);    w=5                               /*#  is the number of elements in list.*/
                                                 /* [↓]  verify list items are numeric. */
   do i=1  for #;        _=word(e, i)            /*process each number one at a time.   */
   if \datatype(_, 'N')  then call ser    _    "isn't a valid number";    @.i=_/1
   w=max(w, length(@.i))                         /*use the maximum length of an element.*/
   end   /*i*/                                   /* [↑]  removes superfluous stuff.     */
                                                 /* [↓]  process the optional order.    */
if N==''  then parse value 0 # # with bot top N  /*define the default  order  range.    */
          else parse var N bot 1 top             /*Not specified?  Then use only 1 order*/
if #==0   then call ser     "no numbers were specified."
if N<0    then call ser  N  "(order) can't be negative."
if N>#    then call ser  N  "(order) can't be greater than"  #
_=;               do k=1  for #;  _=_ right(@.k, w);  end  /*k*/;        _=substr(_, 2)
say right(#  'numbers:', 44)  _                  /*display the header title    and ···  */
say left('', 44)copies('─', w*#+#)               /*   "     "     "   fence.            */
                                                 /* [↓]  where da rubber meets da road. */
      do o=bot  to top;        do r=1  for #;  !.r=@.r;     end /*r*/;        $=
        do j=1  for o; d=!.j;  do k=j+1  to #;     parse value  !.k  !.k-d   with   d  !.k
                               w=max(w, length(!.k))
                               end   /*k*/
        end   /*j*/
                               do i=o+1  to #; $=$ right(!.i/1, w);  end  /*i*/
      if $==''  then $=' [null]'
      say right(o, 7)th(o)'─order forward difference vector ='     $
      end     /*o*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ser:           say;           say '***error***';      say arg(1);     say;         exit 13
th: procedure; x=abs(arg(1)); return word('th st nd rd',1+x//10*(x//100%10\==1)*(x//10<4))
```

'''output'''   when using the default input:

```txt

                                 10 numbers:    90    47    58    29    22    32    55     5    55    73
                                            ────────────────────────────────────────────────────────────
      0th─order forward difference vector =     90    47    58    29    22    32    55     5    55    73
      1st─order forward difference vector =    -43    11   -29    -7    10    23   -50    50    18
      2nd─order forward difference vector =     54   -40    22    17    13   -73   100   -32
      3rd─order forward difference vector =    -94    62    -5    -4   -86   173  -132
      4th─order forward difference vector =    156   -67     1   -82   259  -305
      5th─order forward difference vector =   -223    68   -83   341  -564
      6th─order forward difference vector =    291  -151   424  -905
      7th─order forward difference vector =   -442   575 -1329
      8th─order forward difference vector =   1017 -1904
      9th─order forward difference vector =  -2921
     10th─order forward difference vector = [null]

```



### Version 2


```rexx
/* REXX ***************************************************************
* Forward differences
* 18.08.2012 Walter Pachl derived from PL/I
**********************************************************************/
Do n=-1 To 11
  Call differences '90 47 58 29 22 32 55 5 55 73',n
  End
Exit

differences: Procedure
  Parse Arg a,n
  m=words(a)
  Select
    When n<0 Then Say 'n is negative:' n '<' 0
    When n>m Then Say 'n is too large:' n '>' m
    Otherwise Do
      Do i=1 By 1 while a<>''
        Parse Var a a.i a
        End
      Do i = 1 to n;
        t = a.i;
        Do j = i+1 to m;
          u = a.j
          a.j = a.j-t;
          t = u;
          end;
        end;
      ol=''
      Do k=n+1 to m
        ol=ol a.k
        End
      Say n ol
      End
    End
  Return
```

{{out}} for Java's input

```txt

n is negative: -1 < 0
0  90 47 58 29 22 32 55 5 55 73
1  -43 11 -29 -7 10 23 -50 50 18
2  54 -40 22 17 13 -73 100 -32
3  -94 62 -5 -4 -86 173 -132
4  156 -67 1 -82 259 -305
5  -223 68 -83 341 -564
6  291 -151 424 -905
7  -442 575 -1329
8  1017 -1904
9  -2921
10
n is too large: 11 > 10

```



## Ring


```ring

# Project : Forward difference

s = [90, 47, 58, 29, 22, 32, 55, 5, 55, 73]
for p = 1 to 9
      s = fwddiff(s)
      showarray(s)
next

func fwddiff(s)
        for j=1 to len(s)-1
             s[j] = s[j+1]-s[j]
        next
        n = len(s)
        del(s, n)
        return s

func showarray(vect)
        see "{"
        svect = ""
        for n = 1 to len(vect)
              svect = svect + vect[n] + ", "
        next
        svect = left(svect, len(svect) - 2)
        see svect
        see "}" + nl

```

Output:

```txt

{-43, 11, -29, -7, 10, 23, -50, 50, 18}
{54, -40, 22, 17, 13, -73, 100, -32}
{-94, 62, -5, -4, -86, 173, -132}
{156, -67, 1, -82, 259, -305}
{-223, 68, -83, 341, -564}
{291, -151, 424, -905}
{-442, 575, -1329}
{1017, -1904}
{-2921}

```



## Ruby

This code uses new features from Ruby 1.8.7:

* Enumerable#each_cons is a new method.
* Integer#times, without a block, returns an enumerator.

```ruby
def dif(s)
  s.each_cons(2).collect { |x, y| y - x }
end

def difn(s, n)
  n.times.inject(s) { |s, | dif(s) }
end
```


```ruby
p dif([1, 23, 45, 678])      # => [22, 22, 633]
p difn([1, 23, 45, 678], 2)  # => [0, 611]
```



## Scala


```scala
def fdiff(xs: List[Int]) = (xs.tail, xs).zipped.map(_ - _)

def fdiffn(i: Int, xs: List[Int]): List[Int] = if (i == 1) fdiff(xs) else fdiffn(i - 1, fdiff(xs))
```


```scala
val l=List(90,47,58,29,22,32,55,5,55,73)
(1 to 9)foreach(x=>println(fdiffn(x,l)))
```

```txt

List(-43, 11, -29, -7, 10, 23, -50, 50, 18)
List(54, -40, 22, 17, 13, -73, 100, -32)
List(-94, 62, -5, -4, -86, 173, -132)
List(156, -67, 1, -82, 259, -305)
List(-223, 68, -83, 341, -564)
List(291, -151, 424, -905)
List(-442, 575, -1329)
List(1017, -1904)
List(-2921)

```



## Scheme


```scheme
(define (forward-diff lst)
  (if (or (null? lst) (null? (cdr lst)))
      '()
      (cons (- (cadr lst) (car lst))
            (forward-diff (cdr lst)))))

(define (nth-forward-diff n xs)
  (if (= n 0)
      xs
      (nth-forward-diff (- n 1)
                        (forward-diff xs))))
```


```txt

> (nth-forward-diff 9 '(90 47 58 29 22 32 55 5 55 73))
(-2921)

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func array integer: forwardDifference (in array integer: data) is func
  result
    var array integer: diffResult is 0 times 0;
  local
    var integer: index is 0;
  begin
    for index range 1 to pred(length(data)) do
      diffResult &:= -data[index] + data[succ(index)];
    end for;
  end func;

const proc: main is func
  local
    var array integer: data is [] (90, 47, 58, 29, 22, 32, 55, 5, 55, 73);
    var integer: level is 0;
    var integer: number is 0;
    var boolean: firstElement is TRUE;
  begin
    for level range 0 to length(data) do
      firstElement := TRUE;
      for number range data do
        if not firstElement then
          write(", ");
        end if;
        firstElement := FALSE;
        write(number);
      end for;
      writeln;
      data := forwardDifference(data);
    end for;
  end func;
```


```txt

90, 47, 58, 29, 22, 32, 55, 5, 55, 73
-43, 11, -29, -7, 10, 23, -50, 50, 18
54, -40, 22, 17, 13, -73, 100, -32
-94, 62, -5, -4, -86, 173, -132
156, -67, 1, -82, 259, -305
-223, 68, -83, 341, -564
291, -151, 424, -905
-442, 575, -1329
1017, -1904
-2921

```



## SequenceL

Solution that keeps track of intermediate values:

```sequenceL

forwardDifference(x(1), n) := forwardDifferenceHelper(x, n, [x]);

forwardDifferenceHelper(x(1), n, result(2)) :=
	let
		difference := tail(x) - x[1 ... size(x) - 1];
	in
	result when n = 0 or size(x) = 1 else
	forwardDifferenceHelper(difference, n - 1, result ++ [difference]);

```

If no intermediate values are needed, the following is sufficient:

```sequenceL

forwardDifference(x(1),n) :=
	x when n = 0 or size(x) = 1 else
	forwardDifference(tail(x) - x[1 ... size(x) - 1], n - 1);

```



## Sidef


```ruby
func dif(arr) {
    gather {
        for i (0 ..^ arr.end) {
            take(arr[i+1] - arr[i])
        }
    }
}

func difn(n, arr) {
    { arr = dif(arr) } * n
    arr
}

say dif([1, 23, 45, 678])       # => [22, 22, 633]
say difn(2, [1, 23, 45, 678])   # => [0, 611]
```



## Slate


```slate
s@(Sequence traits) forwardDifference
[
  s allButFirst with: s allButLast collect: #- `er
].

s@(Sequence traits) forwardDifference
"Without creating two intermediate throwaway Sequences."
[
  result ::= s allButFirst.
  result doWithIndex: [| :nextValue :index | result at: index infect: [| :oldValue | oldValue - (s at: index)].
  result
].

s@(Sequence traits) forwardDifference: n
[
  (0 below: n) inject: s into: [| :seq :_ | seq forwardDifference]
].
```


```slate
#data := ##(90 47 58 29 22 32 55 5 55 73).
data keysDo: [| :index | inform: (data forwardDifference: index) printString].
```



## Smalltalk

```smalltalk
Array extend [
    difference [
        ^self allButFirst with: self allButLast collect: [ :a :b | a - b ]
    ]

    nthOrderDifference: n [
        ^(1 to: n) inject: self into: [ :old :unused | old difference ]
    ]
]

s := #(90 47 58 29 22 32 55 5 55 73)
1 to: s size - 1 do: [ :i |
    (s nthOrderDifference: i) printNl ]
```



## SQL


```sql
WITH RECURSIVE
T0 (N, ITEM, LIST, NEW_LIST) AS
(
    SELECT 1,
           NULL,
           '90,47,58,29,22,32,55,5,55,73' || ',',
           NULL
     UNION ALL
    SELECT CASE
               WHEN SUBSTR(LIST, INSTR(LIST, ',') + 1, LENGTH(LIST)) = ''
               THEN N + 1
               ELSE N
           END,
           CASE
               WHEN SUBSTR(LIST, INSTR(LIST, ',') + 1, LENGTH(LIST)) <> ''
               THEN SUBSTR(LIST, 1, INSTR(LIST, ',') - 1)
               ELSE NULL
           END,
           CASE
               WHEN SUBSTR(LIST, INSTR(LIST, ',') + 1, LENGTH(LIST)) = ''
               THEN IFNULL(NEW_LIST || (SUBSTR(LIST, 1, INSTR(LIST, ',') - 1) - ITEM) || ',', '')
               ELSE SUBSTR(LIST, INSTR(LIST, ',') + 1, LENGTH(LIST))
           END,
           CASE
               WHEN SUBSTR(LIST, INSTR(LIST, ',') + 1, LENGTH(LIST)) <> ''
               THEN IFNULL(NEW_LIST, '') || IFNULL((SUBSTR(LIST, 1, INSTR(LIST, ',') - 1) - ITEM) || ',', '')
               ELSE NULL
           END
      FROM T0
     WHERE INSTR(LIST, ',') > 0
)
SELECT N,
       TRIM(LIST, ',') LIST
  FROM T0
 WHERE NEW_LIST IS NULL
   AND LIST <> ''
 ORDER BY N;
```



## Standard ML



```sml
fun forward_difference xs = ListPair.map op- (tl xs, xs)

fun nth_forward_difference n xs =
  if n = 0 then
    xs
  else
    nth_forward_difference (n-1) (forward_difference xs)
```


```txt

- nth_forward_difference 9 [90, 47, 58, 29, 22, 32, 55, 5, 55, 73];
val it = [~2921] : int list

```



## Stata

It's possible to implement differences using row indices. For instance, first forward differences of a variable x can be defined by:


```stata
gen y=x[_n+1]-x[_n]
```


However, it's much more natural to use [http://www.stata.com/help.cgi?tsvarlist time-series varlists]. In order to use them, it's necessary to first set a ''time'' variable, which may be simply an index variable.


```stata
* First create a dataset
clear all
set obs 100
gen i=_n
tsset i
gen x=rnormal()

* Differences
display "Difference order?" _request(k)
gen y=D${k}F${k}.x
```



## Swift



```swift>func forwardsDifference<T: SignedNumeric
(of arr: [T]) -> [T] {
  return zip(arr.dropFirst(), arr).map({ $0.0 - $0.1 })
}

func nthForwardsDifference<T: SignedNumeric>(of arr: [T], n: Int) -> [T] {
  assert(n >= 0)

  switch (arr, n) {
  case ([], _):
    return []
  case let (arr, 0):
    return arr
  case let (arr, i):
    return nthForwardsDifference(of: forwardsDifference(of: arr), n: i - 1)
  }
}

for diff in (0...9).map({ nthForwardsDifference(of: [90, 47, 58, 29, 22, 32, 55, 5, 55, 73], n: $0) }) {
  print(diff)
}
```


```txt
[90, 47, 58, 29, 22, 32, 55, 5, 55, 73]
[-43, 11, -29, -7, 10, 23, -50, 50, 18]
[54, -40, 22, 17, 13, -73, 100, -32]
[-94, 62, -5, -4, -86, 173, -132]
[156, -67, 1, -82, 259, -305]
[-223, 68, -83, 341, -564]
[291, -151, 424, -905]
[-442, 575, -1329]
[1017, -1904]
[-2921]
```



## Tcl


```tcl
proc do_fwd_diff {list} {
    set previous [lindex $list 0]
    set new [list]
    foreach current [lrange $list 1 end] {
        lappend new [expr {$current - $previous}]
        set previous $current
    }
    return $new
}

proc fwd_diff {list order} {
    while {$order >= 1} {
        set list [do_fwd_diff $list]
        incr order -1
    }
    return $list
}

set a {90.5 47 58 29 22 32 55 5 55 73.5}

for {set order 0} {$order <= 10} {incr order} {
    puts [format "%d\t%s" $order [fwd_diff $a $order]]
}
```

```txt
0	90.5 47 58 29 22 32 55 5 55 73.5
1	-43.5 11 -29 -7 10 23 -50 50 18.5
2	54.5 -40 22 17 13 -73 100 -31.5
3	-94.5 62 -5 -4 -86 173 -131.5
4	156.5 -67 1 -82 259 -304.5
5	-223.5 68 -83 341 -563.5
6	291.5 -151 424 -904.5
7	-442.5 575 -1328.5
8	1017.5 -1903.5
9	-2921.0
10
```



## Ursala

This function doesn't need to be defined because it's in a library already,
but it could be defined like this:

```Ursala
#import std
#import nat
#import flo

nth_diff "n" = rep"n" minus*typ
```

test program:

```Ursala
test_data = <90.,47.,58.,29.,22.,32.,55.,5.,55.,73.>

#show+

examples =

printf/*=*' %0.0f' <
   nth_diff6 test_data,
   nth_diff7 test_data>
```

```txt
 291 -151 424 -905
 -442 575 -1329
```



## Visual Basic .NET

```vbnet
Module ForwardDifference

    Sub Main()
        Dim lNum As New List(Of Integer)(New Integer() {90, 47, 58, 29, 22, 32, 55, 5, 55, 73})
        For i As UInteger = 0 To 9
            Console.WriteLine(String.Join(" ", (From n In Difference(i, lNum) Select String.Format("{0,5}", n)).ToArray()))
        Next
        Console.ReadKey()
    End Sub

    Private Function Difference(ByVal Level As UInteger, ByVal Numbers As List(Of Integer)) As List(Of Integer)
        If Level >= Numbers.Count Then Throw New ArgumentOutOfRangeException("Level", "Level must be less than number of items in Numbers")

        For i As Integer = 1 To Level
            Numbers = (From n In Enumerable.Range(0, Numbers.Count - 1) _
                       Select Numbers(n + 1) - Numbers(n)).ToList()
        Next

        Return Numbers
    End Function

End Module
```

```txt

   90    47    58    29    22    32    55     5    55    73
  -43    11   -29    -7    10    23   -50    50    18
   54   -40    22    17    13   -73   100   -32
  -94    62    -5    -4   -86   173  -132
  156   -67     1   -82   259  -305
 -223    68   -83   341  -564
  291  -151   424  -905
 -442   575 -1329
 1017 -1904
-2921

```



## Visual FoxPro


```vfp

#DEFINE CTAB CHR(9)
LOCAL lcList As String, i As Integer, n As Integer
n = 10
LOCAL ARRAY aa[n]
CLEAR
lcList = "90,47,58,29,22,32,55,5,55,73"
FOR i = 1 TO n
    aa[i] = VAL(GETWORDNUM(lcList, i, ","))
ENDFOR
ShowOutput("Original", @aa)
k = n - 1
FOR i = 1 TO n - 1
    ForwardDiff(@aa)
    ShowOutput("Difference " + TRANSFORM(i), @aa)
ENDFOR

PROCEDURE ForwardDiff(a)
LOCAL i As Integer, n As Integer
n = ALEN(a)
LOCAL ARRAY b[n-1]
FOR i = 1 TO n - 1
    b[i] = a[i+1] - a[i]
ENDFOR
DIMENSION a[n-1]
ACOPY(b, a)
ENDPROC

PROCEDURE ShowOutput(lcLabel, zz)
LOCAL i As Integer, n As Integer, lcTxt As String
n = ALEN(zz)
lcTxt = lcLabel + ":" + CTAB
FOR i = 1 TO n
    lcTxt = lcTxt + TRANSFORM(zz[i]) + CTAB
ENDFOR
lcTxt = LEFT(lcTxt, LEN(lcTxt) - 1)
? lcTxt
ENDPROC

```

```txt

Original:       90      47      58      29      22      32      55      5       55      73
Difference 1:   -43     11      -29     -7      10      23      -50     50      18
Difference 2:   54      -40     22      17      13      -73     100     -32
Difference 3:   -94     62      -5      -4      -86     173     -132
Difference 4:   156     -67     1       -82     259     -305
Difference 5:   -223    68      -83     341     -564
Difference 6:   291     -151    424     -905
Difference 7:   -442    575     -1329
Difference 8:   1017    -1904
Difference 9:   -2921

```


## zkl

```zkl
fcn forwardDiff(lst){
   if(lst.len()<2)
      return(T);
      return(T(lst[1]-lst[0]).extend(forwardDiff(lst[1,*])))
}
fcn nthForwardDiff(n,xs){
   if(n==0)
      return(xs);
      return(nthForwardDiff(n-1,forwardDiff(xs))) // tail recursion
}
```


```zkl
nthForwardDiff(9,T(90, 47, 58, 29, 22, 32, 55, 5, 55, 73)).println();
```

```txt

L(-2921)

```



## ZX Spectrum Basic


```zxbasic
10 DATA 9,0,1,2,4,7,4,2,1,0
20 LET p=1
30 READ n: DIM b(n)
40 FOR i=1 TO n
50 READ b(i)
60 NEXT i
70 FOR j=1 TO p
80 FOR i=1 TO n-j
90 LET b(i)=b(i+1)-b(i)
100 NEXT i
110 NEXT j
120 FOR i=1 TO n-p
130 PRINT b(i);" ";
140 NEXT i
```

