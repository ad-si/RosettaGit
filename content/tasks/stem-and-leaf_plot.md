+++
title = "Stem-and-leaf plot"
description = ""
date = 2019-07-02T13:14:26Z
aliases = []
[extra]
id = 5219
[taxonomies]
categories = ["task", "Probability and statistics"]
tags = []
+++

## Task

Create a well-formatted [[wp:Stem-and-leaf_plot|stem-and-leaf plot]] from the following data set, where the leaves are the last digits:

<blockquote style="font-family: monospace; white-space: pre-wrap;">12  127 28  42  39  113 42  18  44  118 44  37  113 124 37  48  127 36  29  31  125 139 131 115 105 132 104 123 35  113 122 42  117 119 58  109 23  105 63  27  44  105 99  41  128 121 116 125 32  61  37  127 29  113 121 58  114 126 53  114 96  25  109 7   31  141 46  13  27  43  117 116 27  7   68  40  31  115 124 42  128 52  71  118 117 38  27  106 33  117 116 111 40  119 47  105 57  122 109 124 115 43  120 43  27  27  18  28  48  125 107 114 34  133 45  120 30  127 31  116 146</blockquote>
<!-- The data set generation parameters: humps at 0 and 80 of width 80, 60 elements, slopes 3 and 6. -->

The primary intent of this task is the presentation of information. It is acceptable to hardcode the data set or characteristics of it (such as what the stems are) in the example, insofar as it is impractical to make the example generic to any data set. For example, in a computation-less language like HTML the data set may be entirely prearranged within the example; the interesting characteristics are how the proper visual formatting is arranged.

If possible, the output should not be a bitmap image. <tt>Monospaced plain text</tt> is acceptable, but do better if you can. It may be a window, i.e. not a file.


'''Note:''' If you wish to try multiple data sets, you might try [[Stem-and-leaf plot/Data generator|this generator]].





## ACL2


```Lisp
(defun insert (x xs)
   (cond ((endp xs) (list x))
         ((> x (first xs))
          (cons (first xs) (insert x (rest xs))))
         (t (cons x xs))))

(defun isort (xs)
   (if (endp xs)
       nil
       (insert (first xs) (isort (rest xs)))))

(defun stem-and-leaf-bins (xs bin curr)
   (cond ((endp xs) (list curr))
         ((= (floor (first xs) 10) bin)
          (stem-and-leaf-bins (rest xs)
                              bin
                              (cons (first xs) curr)))
         (t (cons curr
                  (stem-and-leaf-bins (rest xs)
                                      (floor (first xs) 10)
                                      (list (first xs)))))))

(defun print-bin (bin)
   (if (endp bin)
       nil
       (progn$ (cw " ~x0" (mod (first bin) 10))
               (print-bin (rest bin)))))

(defun stem-and-leaf-plot-r (bins)
   (if (or (endp bins) (endp (first bins)))
       nil
       (progn$ (cw "~x0 |" (floor (first (first bins)) 10))
               (print-bin (first bins))
               (cw "~%")
               (stem-and-leaf-plot-r (rest bins)))))

(defun stem-and-leaf-plot (xs)
   (stem-and-leaf-plot-r
    (reverse (stem-and-leaf-bins (reverse (isort xs))
                                 0
                                 nil))))
```



## Ada

[[GNAT]] used for sorting, could use any other sorting method.
Does not handle negative stems properly.

```Ada

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Gnat.Heap_Sort_G;
procedure stemleaf is
	data : array(Natural Range <>) of Integer := (
	0,12,127,28,42,39,113, 42,18,44,118,44,37,113,124,37,48,127,36,29,31,
	125,139,131,115,105,132,104,123,35,113,122,42,117,119,58,109,23,105,
	63,27,44,105,99,41,128,121,116,125,32,61,37,127,29,113,121,58,114,126,
	53,114,96,25,109,7,31,141,46,13,27,43,117,116,27,7,68,40,31,115,124,42,
	128,52,71,118,117,38,27,106,33,117,116,111,40,119,47,105,57,122,109,
	124,115,43,120,43,27,27,18,28,48,125,107,114,34,133,45,120, 30,127,
	31,116,146); -- Position 0 is used for storage during sorting, initialized as 0

	procedure Move (from, to : in Natural) is
	begin data(to) := data(from);
	end Move;

	function Cmp (p1, p2 : Natural) return Boolean is
	begin return data(p1)<data(p2);
	end Cmp;

	package Sorty is new GNAT.Heap_Sort_G(Move,Cmp);
	min,max,p,stemw: Integer;
begin
	Sorty.Sort(data'Last);
	min := data(1);
	max := data(data'Last);
	stemw := Integer'Image(max)'Length;
	p := 1;
	for stem in min/10..max/10 loop
		put(stem,Width=>stemw); put(" |");
		Leaf_Loop:
			while data(p)/10=stem loop
				put(" "); put(data(p) mod 10,Width=>1);
				exit Leaf_loop when p=data'Last;
				p := p+1;
		end loop Leaf_Loop;
		new_line;
	end loop;
end stemleaf;

```

Output:

```txt

   0 | 7 7
   1 | 2 3 8 8
   2 | 3 5 7 7 7 7 7 7 8 8 9 9
   3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
   4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
   5 | 2 3 7 8 8
   6 | 1 3 8
   7 | 1
   8 |
   9 | 6 9
  10 | 4 5 5 5 5 6 7 9 9 9
  11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
  12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
  13 | 1 2 3 9
  14 | 1 6

```



## AutoHotkey


```AutoHotkey
SetWorkingDir %A_ScriptDir%
#NoEnv
Data := "12  127 28  42  39  113 42  18  44  118 44  37  113 124 37  48  127 36  29  31  125 139 131 115 105 132 104 123 35  113 122 42  117 119 58  109 23  105 63  27  44  105 99  41  128 121 116 125 32  61  37  127 29  113 121 58  114 126 53  114 96  25  109 7   31  141 46  13  27  43  117 116 27  7   68  40  31  115 124 42  128 52  71  118 117 38  27  106 33  117 116 111 40  119 47  105 57  122 109 124 115 43  120 43  27  27  18  28  48  125 107 114 34  133 45  120 30  127 31  116  146"
 ; This loop removes the double/multiple spaces encountered when copying+pasting the given data set:
While (Instr(Data,"  "))
    StringReplace, Data, Data,%A_Space%%A_Space%,%A_Space%,All
; Sort the data numerically using a space as the separator:
Sort, Data,ND%A_Space%

OldStem := 0
; Parse the data using a space as the separator, storing each new string as A_LoopField and running the loop once per string:
Loop, parse, Data,%A_Space%
{
    NewStem := SubStr(A_LoopField,1,StrLen(A_LoopField)-1)     ; AutoHotkey doesn't have a Left() function, so this does the trick.
    If ( NewStem <> OldStem  and StrLen(A_LoopField) <> 1)
    {
        While(OldStem+1<>NewStem)                              ; account for all stems which don't appear (in this example, 8) but are between the lowest and highest stems
            OldStem++,ToPrint .= "`n" PadStem(oldStem)
        ToPrint .= "`n" PadStem(NewStem)
        OldStem := NewStem
    }
    Else If ( StrLen(A_LoopField)=1 and !FirstStem)
        ToPrint .= PadStem(0),FirstStem := true
    ToPrint .= SubStr(A_LoopField,strLen(A_LoopField)) " "    ; No Right() function either, so this returns the last character of A_LoopField (the string curently used by the parsing loop)
}
                                                              ; Delete the old stem and leaf file (if any), write our new contents to it, then show it:
FileDelete Stem and leaf.txt
FileAppend %ToPrint%, Stem and Leaf.txt
Run Stem and leaf.txt
return

PadStem(Stem){
    Spaces = 0
    While ( 3 - StrLen(Stem) <> Spaces )                     ; If the stems are more than 2 digits long, increase the number 3 to one more than the stem length.
        ToReturn .= " ",Spaces++
    ToReturn .= Stem
    ToReturn .= " | "
    Return ToReturn
}

```

Output:

```txt

  0 | 7 7
  1 | 2 3 8 8
  2 | 3 5 7 7 7 7 7 7 8 8 9 9
  3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 | 2 3 7 8 8
  6 | 1 3 8
  7 | 1
  8 |
  9 | 6 9
 10 | 4 5 5 5 5 6 7 9 9 9
 11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 | 1 2 3 9
 14 | 1 6
```



## AWK


```AWK

# syntax: GAWK -f STEM-AND-LEAF_PLOT.AWK
#
# sorting:
#   PROCINFO["sorted_in"] is used by GAWK
#   SORTTYPE is used by Thompson Automation's TAWK
#
BEGIN {
    data = "12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36 29 31 " \
    "125 139 131 115 105 132 104 123 35 113 122 42 117 119 58 109 23 105 63 27 44 " \
    "105 99 41 128 121 116 125 32 61 37 127 29 113 121 58 114 126 53 114 96 25 " \
    "109 7 31 141 46 13 27 43 117 116 27 7 68 40 31 115 124 42 128 52 71 118 117 " \
    "38 27 106 33 117 116 111 40 119 47 105 57 122 109 124 115 43 120 43 27 27 18 " \
    "28 48 125 107 114 34 133 45 120 30 127 31 116 146"
    data_points = split(data,data_arr," ")
    for (i=1; i<=data_points; i++) {
      x = data_arr[i]
      stem = int(x / 10)
      leaf = x % 10
      if (i == 1) {
        lo = hi = stem
      }
      lo = min(lo,stem)
      hi = max(hi,stem)
      arr[stem][leaf]++
    }
    PROCINFO["sorted_in"] = "@ind_str_asc" ; SORTTYPE = 1
    for (i=lo; i<=hi; i++) {
      printf("%4d |",i)
      arr[i][""]
      for (j in arr[i]) {
        for (k=1; k<=arr[i][j]; k++) {
          printf(" %d",j)
          leaves_printed++
        }
      }
      printf("\n")
    }
    if (data_points == leaves_printed) {
      exit(0)
    }
    else {
      printf("error: %d data points != %d leaves printed\n",data_points,leaves_printed)
      exit(1)
    }
}
function max(x,y) { return((x > y) ? x : y) }
function min(x,y) { return((x < y) ? x : y) }

```

<p>output:</p>

```txt

   0 | 7 7
   1 | 2 3 8 8
   2 | 3 5 7 7 7 7 7 7 8 8 9 9
   3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
   4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
   5 | 2 3 7 8 8
   6 | 1 3 8
   7 | 1
   8 |
   9 | 6 9
  10 | 4 5 5 5 5 6 7 9 9 9
  11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
  12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
  13 | 1 2 3 9
  14 | 1 6

```



## BBC BASIC

```bbcbasic
      INSTALL @lib$+"SORTLIB"
      Sort% = FN_sortinit(0, 0)

      DIM Data%(120)
      Data%() = \
      \  12, 127,  28,  42,  39, 113,  42,  18,  44, 118,  44,  37, 113, 124, \
      \  37,  48, 127,  36,  29,  31, 125, 139, 131, 115, 105, 132, 104, 123, \
      \  35, 113, 122,  42, 117, 119,  58, 109,  23, 105,  63,  27,  44, 105, \
      \  99,  41, 128, 121, 116, 125,  32,  61,  37, 127,  29, 113, 121,  58, \
      \ 114, 126,  53, 114,  96,  25, 109,   7,  31, 141,  46,  13,  27,  43, \
      \ 117, 116,  27,   7,  68,  40,  31, 115, 124,  42, 128,  52,  71, 118, \
      \ 117,  38,  27, 106,  33, 117, 116, 111,  40, 119,  47, 105,  57, 122, \
      \ 109, 124, 115,  43, 120,  43,  27,  27,  18,  28,  48, 125, 107, 114, \
      \  34, 133,  45, 120,  30, 127,  31, 116, 146

      PROCleafplot(Data%(), DIM(Data%(),1) + 1)
      END

      DEF PROCleafplot(x%(), n%)
      LOCAL @%, C%, i%, j%, d%
      @% = 2

      C% = n%
      CALL Sort%, x%(0)

      i% = x%(0) DIV 10 - 1
      FOR j% = 0 TO n% - 1
        d% = x%(j%) DIV 10
        WHILE d% > i%
          i% += 1
          IF j% PRINT
          PRINT i% " |" ;
        ENDWHILE
        PRINT x%(j%) MOD 10 ;
      NEXT
      PRINT
      ENDPROC
```

Output:

```txt

 0 | 7 7
 1 | 2 3 8 8
 2 | 3 5 7 7 7 7 7 7 8 8 9 9
 3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 | 2 3 7 8 8
 6 | 1 3 8
 7 | 1
 8 |
 9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6

```



## C


```c
#include <stdio.h>
#include <stdlib.h>

int icmp(const void *a, const void *b)
{
	return *(const int*)a < *(const int*)b ? -1 : *(const int*)a > *(const int*)b;
}

void leaf_plot(int *x, int len)
{
	int i, j, d;

	qsort(x, len, sizeof(int), icmp);

	i = x[0] / 10 - 1;
	for (j = 0; j < len; j++) {
		d = x[j] / 10;
		while (d > i) printf("%s%3d |", j ? "\n" : "", ++i);
		printf(" %d", x[j] % 10);
	}
}

int main()
{
	int data[] = {
	  12, 127,  28,  42,  39, 113,  42,  18,  44, 118,  44,  37, 113, 124,
	  37,  48, 127,  36,  29,  31, 125, 139, 131, 115, 105, 132, 104, 123,
	  35, 113, 122,  42, 117, 119,  58, 109,  23, 105,  63,  27,  44, 105,
	  99,  41, 128, 121, 116, 125,  32,  61,  37, 127,  29, 113, 121,  58,
	 114, 126,  53, 114,  96,  25, 109,   7,  31, 141,  46,  13,  27,  43,
	 117, 116,  27,   7,  68,  40,  31, 115, 124,  42, 128,  52,  71, 118,
	 117,  38,  27, 106,  33, 117, 116, 111,  40, 119,  47, 105,  57, 122,
	 109, 124, 115,  43, 120,  43,  27,  27,  18,  28,  48, 125, 107, 114,
	  34, 133,  45, 120,  30, 127,  31, 116, 146 };

	leaf_plot(data, sizeof(data)/sizeof(data[0]));

	return 0;
}
```
output<lang>  0 | 7 7
  1 | 2 3 8 8
  2 | 3 5 7 7 7 7 7 7 8 8 9 9
  3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 | 2 3 7 8 8
  6 | 1 3 8
  7 | 1
  8 |
  9 | 6 9
 10 | 4 5 5 5 5 6 7 9 9 9
 11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 | 1 2 3 9
 14 | 1 6
```



## C++


```cpp
#include <algorithm>
#include <iomanip>
#include <iostream>
#include <vector>

const int dataset[] = {
     12,127, 28, 42, 39,113, 42, 18, 44,118, 44, 37,113,124, 37, 48,127, 36,
     29, 31,125,139,131,115,105,132,104,123, 35,113,122, 42,117,119, 58,109,
     23,105, 63, 27, 44,105, 99, 41,128,121,116,125, 32, 61, 37,127, 29,113,
    121, 58,114,126, 53,114, 96, 25,109,  7, 31,141, 46, 13, 27, 43,117,116,
     27,  7, 68, 40, 31,115,124, 42,128, 52, 71,118,117, 38, 27,106, 33,117,
    116,111, 40,119, 47,105, 57,122,109,124,115, 43,120, 43, 27, 27, 18, 28,
     48,125,107,114, 34,133, 45,120, 30,127, 31,116,146
};
const int datasize = sizeof(dataset) / sizeof(dataset[0]);

int main()
{
    typedef std::pair<int,int> StemLeaf;
    std::vector<StemLeaf> stemplot;

    for (int i = 0; i < datasize; ++i)
    {
        stemplot.push_back(StemLeaf(dataset[i] / 10, dataset[i] % 10));
    }

    std::sort(stemplot.begin(), stemplot.end());  // order stem/leaf pairs

    int lo = stemplot.front().first; // minimum stem value
    int hi = stemplot.back().first; // maximum stem value

    for (std::vector<StemLeaf>::iterator itr = stemplot.begin(); lo <= hi; ++lo)
    {
        std::cout << std::setw(2) << lo << " |"; // print stem

        // while (there are more stems) and (stem is equal to lo)
        for ( ; itr != stemplot.end() && itr->first == lo; ++itr)
        {
            std::cout << " " << itr->second; // print leaf
        }

        std::cout << std::endl;
    }
}
```

Output:

```txt
 0 | 7 7
 1 | 2 3 8 8
 2 | 3 5 7 7 7 7 7 7 8 8 9 9
 3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 | 2 3 7 8 8
 6 | 1 3 8
 7 | 1
 8 |
 9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6
```


## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static void Main()
    {
        const string data =
        "12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36 29 31 " +
        "125 139 131 115 105 132 104 123 35 113 122 42 117 119 58 109 23 " +
        "105 63 27 44 105 99 41 128 121 116 125 32 61 37 127 29 113 121 58 " +
        "114 126 53 114 96 25 109 7 31 141 46 13 27 43 117 116 27 7 68 40 31 " +
        "115 124 42 128 52 71 118 117 38 27 106 33 117 116 111 40 119 47 " +
        "105 57 122 109 124 115 43 120 43 27 27 18 28 48 125 107 114 34 " +
        "133 45 120 30 127 31 116 146";

        int[] ints = data.Split(' ').Select(int.Parse).ToArray();

        StemAndLeafPlot(ints);

        Console.ReadKey();
    }

    public static void StemAndLeafPlot(int[] arr)
    {
        int stemMax = arr.Max() / 10;
        int stemMin = arr.Min() / 10;
        Array.Sort(arr);

        for (int i = stemMin; i <= stemMax; i++)
        {
            Console.Write("{0,3} | ", i);
            foreach (var t in arr)
            {
                if (t < 10 * i)
                    continue;
                if (t >= 10 * (i + 1))
                    break;
                Console.Write("{0} ", t % 10);
            }
            Console.WriteLine("");
        }
    }
}
```


```txt
  0 | 7 7
  1 | 2 3 8 8
  2 | 3 5 7 7 7 7 7 7 8 8 9 9
  3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 | 2 3 7 8 8
  6 | 1 3 8
  7 | 1
  8 |
  9 | 6 9
 10 | 4 5 5 5 5 6 7 9 9 9
 11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 | 1 2 3 9
 14 | 1 6
```



## Ceylon


```ceylon
"Run the module `thestemandleafplot`."
shared void run() {

	value data ="12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36 29 31 125 139 131 115 105 132 104 123 35
	             113 122 42 117 119 58 109 23 105 63 27 44 105 99 41 128 121 116 125 32 61 37 127 29 113 121 58 114
	             126 53 114 96 25 109 7 31 141 46 13 27 43 117 116 27 7 68 40 31 115 124 42 128 52 71 118 117 38 27
	             106 33 117 116 111 40 119 47 105 57 122 109 124 115 43 120 43 27 27 18 28 48 125 107 114 34 133 45
	             120 30 127 31 116 146";

	value numbers = data
			.split()
			.map(parseInteger)
			.coalesced;

	value stemsToLeaves = numbers
			.group((Integer element) => element / 10)
			.mapItems((Integer key, [Integer+] item) => item.map((Integer element) => element % 10))
			.mapItems((Integer key, {Integer+} item) => sort(item));

	value lastStem = stemsToLeaves.keys.last else 0;
	for(i in 0..lastStem) {
		print("``formatInteger(i).padLeading(2)``| ``" ".join(stemsToLeaves[i] else [])``");
	}
}
```



## Clojure


```clojure
(def data
  [12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36 29 31 125
   139 131 115 105 132 104 123 35 113 122 42 117 119 58 109 23 105 63 27
   44 105 99 41 128 121 116 125 32 61 37 127 29 113 121 58 114 126 53 114
   96 25 109 7 31 141 46 13 27 43 117 116 27 7 68 40 31 115 124 42 128 146
   52 71 118 117 38 27 106 33 117 116 111 40 119 47 105 57 122 109 124
   115 43 120 43 27 27 18 28 48 125 107 114 34 133 45 120 30 127 31 116])

(defn calc-stem [number]
  (int (Math/floor (/ number 10))))

(defn calc-leaf [number]
  (mod number 10))

(defn new-plant
  "Returns a leafless plant, with `size` empty branches,
  i.e. a hash-map with integer keys (from 0 to `size` inclusive)
  mapped to empty vectors.

  (new-plant 2) ;=> {0 [] 1 [] 2 []}"
  [size]
  (let [end (inc size)]
    (->> (repeat end [])
         (interleave (range end))
         (apply hash-map))))

(defn sprout-leaves
  [plant [stem leaf]]
  (update plant stem conj leaf))

(defn stem-and-leaf [numbers]
  (let [max-stem   (calc-stem (reduce max numbers))
        baby-plant (new-plant max-stem)
        plant      (->> (map (juxt calc-stem calc-leaf) numbers)
                        (reduce sprout-leaves baby-plant)
                        (sort))]
    (doseq [[stem leaves] plant]
      (print   (format (str "%2s") stem))
      (print   " | ")
      (println (clojure.string/join " " (sort leaves))))))

(stem-and-leaf data)

```


```txt

 0 | 7 7
 1 | 2 3 8 8
 2 | 3 5 7 7 7 7 7 7 8 8 9 9
 3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 | 2 3 7 8 8
 6 | 1 3 8
 7 | 1
 8 |
 9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6

```



## D


```d
import std.stdio, std.algorithm;

void main() {
    enum data = [12,127,28,42,39,113,42,18,44,118,44,37,113,124,37,48,
        127,36,29,31,125,139,131,115,105,132,104,123,35,113,122,42,117,
        119,58,109,23,105,63,27,44,105,99,41,128,121,116,125,32,61,37,
        127,29,113,121,58,114,126,53,114,96,25,109,7,31,141,46,13,27,
        43,117,116,27,7,68,40,31,115,124,42,128,52,71,118,117,38,27,
        106,33,117,116,111,40,119,47,105,57,122,109,124,115,43,120,43,
        27,27,18,28,48,125,107,114,34,133,45,120,30,127,31,116,146];

    int[][int] histo;
    foreach (x; data)
        histo[x / 10] ~= x % 10;
    immutable loHi = data.reduce!(min, max);
    foreach (i; loHi[0]/10 .. loHi[1]/10 + 1)
        writefln("%2d | %(%d %) ", i, histo.get(i, []).sort());
}
```

Output:

```txt
 0 | 7 7
 1 | 2 3 8 8
 2 | 3 5 7 7 7 7 7 7 8 8 9 9
 3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 | 2 3 7 8 8
 6 | 1 3 8
 7 | 1
 8 |
 9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6
```



## Elixir

```elixir
defmodule Stem_and_leaf do
  def plot(data, leaf_digits\\1) do
    multiplier = Enum.reduce(1..leaf_digits, 1, fn _,acc -> acc*10 end)
    Enum.group_by(data, fn x -> div(x, multiplier) end)
    |> Map.new(fn {k,v} -> {k, Enum.map(v, &rem(&1, multiplier)) |> Enum.sort} end)
    |> print(leaf_digits)
  end

  defp print(plot_data, leaf_digits) do
    {min, max} = Map.keys(plot_data) |> Enum.min_max
    stem_width = length(to_charlist(max))
    fmt = "~#{stem_width}w | ~s~n"
    Enum.each(min..max, fn stem ->
      leaves = Enum.map_join(Map.get(plot_data, stem, []), " ", fn leaf ->
        to_string(leaf) |> String.pad_leading(leaf_digits)
      end)
      :io.format fmt, [stem, leaves]
    end)
  end
end

data = ~w(12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36 29 31 125 139 131 115 105 132 104 123 35 113 122 42 117 119 58 109 23 105 63 27 44 105 99 41 128 121 116 125 32 61 37 127 29 113 121 58 114 126 53 114 96 25 109 7 31 141 46 13 27 43 117 116 27 7 68 40 31 115 124 42 128 52 71 118 117 38 27 106 33 117 116 111 40 119 47 105 57 122 109 124 115 43 120 43 27 27 18 28 48 125 107 114 34 133 45 120 30 127 31 116 146)
       |> Enum.map(&String.to_integer(&1))
Stem_and_leaf.plot(data)
```


```txt

 0 | 7 7
 1 | 2 3 8 8
 2 | 3 5 7 7 7 7 7 7 8 8 9 9
 3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 | 2 3 7 8 8
 6 | 1 3 8
 7 | 1
 8 |
 9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6

```



## Euphoria


```euphoria
include sort.e

procedure leaf_plot(sequence s)
    sequence stem
    s = sort(s)
    stem = repeat({},floor(s[$]/10)+1)
    for i = 1 to length(s) do
        stem[floor(s[i]/10)+1] &= remainder(s[i],10)
    end for
    for i = 1 to length(stem) do
        printf(1, "%3d | ", i-1)
        for j = 1 to length(stem[i]) do
            printf(1, "%d ", stem[i][j])
        end for
        puts(1,'\n')
    end for
end procedure

constant data = { 12, 127, 28, 42, 39, 113, 42, 18, 44, 118, 44, 37, 113, 124,
    37, 48, 127, 36, 29, 31, 125, 139, 131, 115, 105, 132, 104, 123, 35, 113,
    122, 42, 117, 119, 58, 109, 23, 105, 63, 27, 44, 105, 99, 41, 128, 121,
    116, 125, 32, 61, 37, 127, 29, 113, 121, 58, 114, 126, 53, 114, 96, 25,
    109, 7, 31, 141, 46, 13, 27, 43, 117, 116, 27, 7, 68, 40, 31, 115, 124,
    42, 128, 52, 71, 118, 117, 38, 27, 106, 33, 117, 116, 111, 40, 119, 47,
    105, 57, 122, 109, 124, 115, 43, 120, 43, 27, 27, 18, 28, 48, 125, 107,
    114, 34, 133, 45, 120, 30, 127, 31, 116, 146 }

leaf_plot(data)
```


Output:

```txt
  0 | 7 7
  1 | 2 3 8 8
  2 | 3 5 7 7 7 7 7 7 8 8 9 9
  3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 | 2 3 7 8 8
  6 | 1 3 8
  7 | 1
  8 |
  9 | 6 9
 10 | 4 5 5 5 5 6 7 9 9 9
 11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 | 1 2 3 9
 14 | 1 6

```


=={{header|F Sharp|F#}}==

```fsharp
open System

let data =
    [ 12; 127; 28; 42; 39; 113; 42; 18; 44; 118; 44; 37; 113; 124; 37; 48;
      127; 36; 29; 31; 125; 139; 131; 115; 105; 132; 104; 123; 35; 113; 122;
      42; 117; 119; 58; 109; 23; 105; 63; 27; 44; 105; 99; 41; 128; 121; 116;
      125; 32; 61; 37; 127; 29; 113; 121; 58; 114; 126; 53; 114; 96; 25; 109;
      7; 31; 141; 46; 13; 27; 43; 117; 116; 27; 7; 68; 40; 31; 115; 124; 42;
      128; 52; 71; 118; 117; 38; 27; 106; 33; 117; 116; 111; 40; 119; 47; 105;
      57; 122; 109; 124; 115; 43; 120; 43; 27; 27; 18; 28; 48; 125; 107; 114;
      34; 133; 45; 120; 30; 127; 31; 116; 146 ]

let plotStemAndLeafs items =
    let groupedItems = items |> Seq.sort
                             |> Seq.map (fun i -> i / 10, i % 10)
                             |> Seq.groupBy fst

    let maxStem = groupedItems |> Seq.maxBy fst |> fst
    let stemLeafMap = Map.ofSeq groupedItems

    [0..maxStem] |> List.iter (fun stm -> printf " %2d | " stm
                                          match stemLeafMap.TryFind stm with
                                          | None -> ()
                                          | Some items -> items |> Seq.iter (snd >> printf "%d ")
                                          printfn "")

plotStemAndLeafs data
```

Output:

```txt

  0 | 7 7
  1 | 2 3 8 8
  2 | 3 5 7 7 7 7 7 7 8 8 9 9
  3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 | 2 3 7 8 8
  6 | 1 3 8
  7 | 1
  8 |
  9 | 6 9
 10 | 4 5 5 5 5 6 7 9 9 9
 11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 | 1 2 3 9
 14 | 1 6
```



## Factor


```factor
USING: assocs formatting grouping.extras io kernel math
prettyprint sequences sorting ;

: leaf-plot ( seq -- )
    natural-sort [ 10 /i ] group-by dup keys last 1 +
    [ dup "%2d | " printf of [ 10 mod pprint bl ] each nl ] with
    each-integer ;

{
    12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36
    29 31 125 139 131 115 105 132 104 123 35 113 122 42 117 119
    58 109 23 105 63 27 44 105 99 41 128 121 116 125 32 61 37
    127 29 113 121 58 114 126 53 114 96 25 109 7 31 141 46 13 27
    43 117 116 27 7 68 40 31 115 124 42 128 146 52 71 118 117 38
    27 106 33 117 116 111 40 119 47 105 57 122 109 124 115 43
    120 43 27 27 18 28 48 125 107 114 34 133 45 120 30 127 31
    116
} leaf-plot
```

```txt

 0 | 7 7
 1 | 2 3 8 8
 2 | 3 5 7 7 7 7 7 7 8 8 9 9
 3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 | 2 3 7 8 8
 6 | 1 3 8
 7 | 1
 8 |
 9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6

```



## Forth


```forth
create data
   12 , 127 ,  28 ,  42 ,  39 , 113 ,  42 ,  18 ,  44 , 118 ,  44 ,
   37 , 113 , 124 ,  37 ,  48 , 127 ,  36 ,  29 ,  31 , 125 , 139 ,
  131 , 115 , 105 , 132 , 104 , 123 ,  35 , 113 , 122 ,  42 , 117 ,
  119 ,  58 , 109 ,  23 , 105 ,  63 ,  27 ,  44 , 105 ,  99 ,  41 ,
  128 , 121 , 116 , 125 ,  32 ,  61 ,  37 , 127 ,  29 , 113 , 121 ,
   58 , 114 , 126 ,  53 , 114 ,  96 ,  25 , 109 ,   7 ,  31 , 141 ,
   46 ,  13 ,  27 ,  43 , 117 , 116 ,  27 ,   7 ,  68 ,  40 ,  31 ,
  115 , 124 ,  42 , 128 ,  52 ,  71 , 118 , 117 ,  38 ,  27 , 106 ,
   33 , 117 , 116 , 111 ,  40 , 119 ,  47 , 105 ,  57 , 122 , 109 ,
  124 , 115 ,  43 , 120 ,  43 ,  27 ,  27 ,  18 ,  28 ,  48 , 125 ,
  107 , 114 ,  34 , 133 ,  45 , 120 ,  30 , 127 ,  31 , 116 , 146 ,
here constant data-end

: sort ( end start -- )
  over cell - swap do
    dup i cell+ do
      i @ j @ < if
        i @ j @ i ! j !
      then
    cell +loop
  cell +loop drop ;

: plot
  data-end data sort
  data
  data-end cell - @ 10 / 1+
             data @ 10 /
  do
    cr i 2 u.r ."  | "
    begin dup @ 10 /mod i = while  .  cell+  dup data-end = until else drop then
  loop
  drop ;

plot
```

Output:

```txt

 0 | 7 7
 1 | 2 3 8 8
 2 | 3 5 7 7 7 7 7 7 8 8 9 9
 3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 | 2 3 7 8 8
 6 | 1 3 8
 7 | 1
 8 |
 9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6

```



## Fortran

Because the fancy "structured" options such as DO-WHILE also involve the maddening idea of full evaluation of redundant parts of a compound boolean expression, attempts such as <code>WHILE (I <= N .AND. A(I) ''etc.'')</code> can fail, because the parts may be evaluated "in any order", and so the array be accessed out of bounds. So instead, a classic intersecting loop tangle.

Layout is easily obtained, once the span of elements belonging to each stem value is ascertained. The output loop uses the later-form array specification of A(start:stop), but in earlier Fortran an implicit DO-loop would be in order: <code>WRITE (6,12) STEM,(ABS(MOD(A(I),CLIP)), I = I1,I2 - 1)</code>

Note that the MOD function can produce unexpected values for negative numbers, and, different computer/compiler/language combinations may produce different surprises. In this case, negative values produce negative remainder values, but the ABS function suppresses the surprise.


```Fortran

      SUBROUTINE COMBSORT(A,N)
       INTEGER A(*)	!The array.
       INTEGER N	!The count.
       INTEGER H,T	!Assistants.
       LOGICAL CURSE
        H = N - 1		!Last - First, and not +1.
    1   H = MAX(1,H*10/13)	!The special feature.
        IF (H.EQ.9 .OR. H.EQ.10) H = 11	!A twiddle.
        CURSE = .FALSE.		!So far, so good.
        DO I = N - H,1,-1	!If H = 1, this is a BubbleSort.
          IF (A(I) .GT. A(I + H)) THEN	!One compare.
            T=A(I); A(I)=A(I+H); A(I+H)=T	!One swap.
            CURSE = .TRUE.			!One curse.
          END IF			!One test.
        END DO			!One loop.
        IF (CURSE .OR. H.GT.1) GO TO 1	!Work remains?
      END SUBROUTINE COMBSORT	!Good performance, small code.

      SUBROUTINE TOPIARY(A,N)	!Produces a "stem&leaf" display for the integers in A, damaging A.
       INTEGER A(*)		!An array of integers.
       INTEGER N		!Their number.
       INTEGER CLIP		!Semi-generalisation.
       PARAMETER (CLIP = 10)	!Or at least, annotation.
       INTEGER I1,I2,STEM	!Assistants.
        CALL COMBSORT(A,N)	!Rearrange the array!
        STEM = A(1)/CLIP	!The first stem value.
        I1 = 1			!The first stem's span starts here.
        I2 = I1			!And so far as I know, ends here.
   10   I2 = I2 + 1			!Probe ahead one position.
        IF (I2 .GT. N) GO TO 11		!Off the end? Don't look!
        IF (A(I2)/CLIP .EQ.STEM) GO TO 10	!Still in the same stem? Probe on.
Cast forth a STEM line, corresponding to elements I1:I2 - 1.
   11   WRITE (6,12) STEM,ABS(MOD(A(I1:I2 - 1),CLIP))	!ABS: MOD with negatives can be unexpected.
   12   FORMAT (I4,"|",(100I1))		!Layout. If more than a hundred, starts a new line.
        IF (I2 .GT. N) RETURN		!Are we there yet?
        I1 = I2				!No. This is my new span's start.
Chug along to the next STEM value.
   13   STEM = STEM + 1			!Advance to the next stem.
        IF (A(I2)/CLIP.GT.STEM) GO TO 11!Has the stem reached the impending value?
        GO TO 10			!Yes. Scan its span.
      END SUBROUTINE TOPIARY	!The days of carefully-arranged output.

      PROGRAM TEST
      INTEGER VALUES(121)	!The exact number of values.
      DATA VALUES/		!As in the specified example.
     o  12,127, 28, 42, 39,113, 42, 18, 44,118,	!A regular array
     1  44, 37,113,124, 37, 48,127, 36, 29, 31,	!Makes counting easier.
     2 125,139,131,115,105,132,104,123, 35,113,
     3 122, 42,117,119, 58,109, 23,105, 63, 27,
     4  44,105, 99, 41,128,121,116,125, 32, 61,
     5  37,127, 29,113,121, 58,114,126, 53,114,
     6  96, 25,109,  7, 31,141, 46, 13, 27, 43,
     7 117,116, 27,  7, 68, 40, 31,115,124, 42,
     8 128, 52, 71,118,117, 38, 27,106, 33,117,
     9 116,111, 40,119, 47,105, 57,122,109,124,
     o 115, 43,120, 43, 27, 27, 18, 28, 48,125,
     1 107,114, 34,133, 45,120, 30,127, 31,116,
     2 146/
        CALL TOPIARY(VALUES,121)
      END

```


Output: (If additional spacing is desired, I2 format could be used, etc.)

```txt

   0|77
   1|2388
   2|357777778899
   3|011112345677789
   4|001222233344456788
   5|23788
   6|138
   7|1
   8|
   9|69
  10|4555567999
  11|13333444555666677778899
  12|00112234445556777788
  13|1239
  14|16

```



## FreeBASIC


```FreeBASIC
' version 22-06-2015
' compile with: fbc -s console
' for boundry checks on array's compile with: fbc -s console -exx
' from the rosetta code FreeBASIC entry
#Define out_of_data 99999999 ' any number that is not in the set will do

Sub shellsort(s() As Integer)
    ' from the FreeBASIC entry at rosetta code
    ' sort from lower bound to the highter bound
    Dim As Integer lb = LBound(s)
    Dim As Integer ub = UBound(s)
    Dim As Integer done, i, inc = ub - lb

    Do
        inc = inc / 2.2
        If inc < 1 Then inc = 1
        Do
            done = 0
            For i = lb To ub - inc
                If s(i) > s(i + inc) Then
                    Swap s(i), s(i + inc)
                    done = 1
                End If
            Next
        Loop Until done = 0
    Loop Until inc = 1

End Sub

' ------=< TASK DATA >=------

Data  12, 127,  28,  42,  39, 113,  42,  18,  44, 118,  44,  37, 113, 124
Data  37,  48, 127,  36,  29,  31, 125, 139, 131, 115, 105, 132, 104, 123
Data  35, 113, 122,  42, 117, 119,  58, 109,  23, 105,  63,  27,  44, 105
Data  99,  41, 128, 121, 116, 125,  32,  61,  37, 127,  29, 113, 121,  58
Data 114, 126,  53, 114,  96,  25, 109,   7,  31, 141,  46,  13,  27,  43
Data 117, 116,  27,   7,  68,  40,  31, 115, 124,  42, 128,  52,  71, 118
Data 117,  38,  27, 106,  33, 117, 116, 111,  40, 119,  47, 105,  57, 122
Data 109, 124, 115,  43, 120,  43,  27,  27,  18,  28,  48, 125, 107, 114
Data  34, 133,  45, 120,  30, 127,  31, 116, 146
Data out_of_data

' ------=< MAIN >=------

Dim As String read_in
Dim As Integer i, x, y, count = -1 ' to let the index start on 0
Dim As Integer d()
ReDim d(300)                       ' big enough to hold data index start at 0

Do
    Read i
    If i = out_of_data Then Exit Do
    count = count + 1
    d(count) = i
Loop

ReDim Preserve d(count)            ' trim the data array
shellsort(d())                     ' sort data array

i = 0
For y =  d(0) \ 10 To d(UBound(d)) \ 10
    Print Using "#### |"; y;
    Do
        x = d(i) \ 10              ' \ = integer division
        If y = x Then
            Print Using "##"; d(i) Mod 10;
            i = i + 1
        Else
            Exit Do
        End If
    Loop While i <= UBound(d)
    Print                          ' force linefeed
Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
   0 | 7 7
   1 | 2 3 8 8
   2 | 3 5 7 7 7 7 7 7 8 8 9 9
   3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
   4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
   5 | 2 3 7 8 8
   6 | 1 3 8
   7 | 1
   8 |
   9 | 6 9
  10 | 4 5 5 5 5 6 7 9 9 9
  11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
  12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
  13 | 1 2 3 9
  14 | 1 6
```



## Go


```go
package main

import (
    "fmt"
    "sort"
    "strconv"
    "strings"
)

var data = `12 127 28 42` //...omitted...127 31 116 146`

func main() {
    // load data into map
    m := make(map[int][]string)
    for _, s := range strings.Fields(data) {
        if len(s) == 1 {
            m[0] = append(m[0], s)
        } else if i, err := strconv.Atoi(s[:len(s)-1]); err == nil {
            m[i] = append(m[i], s[len(s)-1:])
        } else {
            panic("non numeric data")
        }
    }
    // sort stem
    s := make([]int, len(m))
    var i int
    for k := range m {
        s[i] = k
        i++
    }
    sort.Ints(s)
    // print
    for k := s[0]; ; k++ {
        v := m[k]
        sort.Strings(v)
        fmt.Printf("%2d | %s\n", k, strings.Join(v, " "))
        if k == s[len(s)-1] {
            break
        }
    }
}
```

Output:

```txt

 0 | 7 7
 1 | 2 3 8 8
 2 | 3 5 7 7 7 7 7 7 8 8 9 9
 3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 | 2 3 7 8 8
 6 | 1 3 8
 7 | 1
 8 |
 9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6

```



## Haskell


```haskell
import Data.List
import Control.Arrow
import Control.Monad

nlsRaw = "12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36 29 31"
  ++ " 125 139 131 115 105 132 104 123 35 113 122 42 117 119 58 109 23 105 63"
  ++ " 27 44 105 99 41 128 121 116 125 32 61 37 127 29 113 121 58 114 126 53"
  ++ " 114 96 25 109 7 31 141 46 13 27 43 117 116 27 7 68 40 31 115 124 42 128"
  ++ " 52 71 118 117 38 27 106 33 117 116 111 40 119 47 105 57 122 109 124 115"
  ++ " 43 120 43 27 27 18 28 48 125 107 114 34 133 45 120 30 127 31 116 146"

nls :: [Int]
nls = map read $ words nlsRaw

groupWith f = takeWhile(not.null). unfoldr(Just. (partition =<< (. f). (==). f. head))
justifyR = foldl ((. return) . (++) . tail) . flip replicate ' '

task ds = mapM_ (putStrLn. showStemLeaves justifyR fb. (head *** sort.concat). unzip)
    $ groupWith fst $ stems ++ map (second return) stemLeaf
  where stemLeaf = map (`quotRem` 10) ds
	stems = map (flip(,)[]) $ uncurry enumFromTo $ minimum &&& maximum $ fst $ unzip stemLeaf
	showStemLeaves f w (a,b) = f w (show a) ++ " |" ++ concatMap (f w. show) b
	fb = length $ show $ maximum $ map abs ds
```

Output:

```txt
*Main> task  nls
  0 |  7  7
  1 |  2  3  8  8
  2 |  3  5  7  7  7  7  7  7  8  8  9  9
  3 |  0  1  1  1  1  2  3  4  5  6  7  7  7  8  9
  4 |  0  0  1  2  2  2  2  3  3  3  4  4  4  5  6  7  8  8
  5 |  2  3  7  8  8
  6 |  1  3  8
  7 |  1
  8 |
  9 |  6  9
 10 |  4  5  5  5  5  6  7  9  9  9
 11 |  1  3  3  3  3  4  4  4  5  5  5  6  6  6  6  7  7  7  7  8  8  9  9
 12 |  0  0  1  1  2  2  3  4  4  4  5  5  5  6  7  7  7  7  8  8
 13 |  1  2  3  9
 14 |  1  6
```


Or alternatively – aiming more for legibility than for economy or concision:


```haskell
import Data.List     (groupBy, intersperse, mapAccumL, sortBy)
import Data.Ord      (comparing)
import Data.Function (on)
import Control.Arrow ((&&&))

-- Strings derived from integers,
-- and split into [(initial string, final character)] tuples.

xs :: [(String, Char)]
xs = (init &&& last) . show <$> [
      12, 127, 28, 42, 39, 113, 42, 18, 44, 118, 44, 37, 113, 124, 37, 48,
      127, 36, 29, 31, 125, 139, 131, 115, 105, 132, 104, 123, 35, 113, 122,
      42, 117, 119, 58, 109, 23, 105, 63, 27, 44, 105, 99, 41, 128, 121, 116,
      125, 32, 61, 37, 127, 29, 113, 121, 58, 114, 126, 53, 114, 96, 25, 109,
      7, 31, 141, 46, 13, 27, 43, 117, 116, 27, 7, 68, 40, 31, 115, 124, 42,
      128, 52, 71, 118, 117, 38, 27, 106, 33, 117, 116, 111, 40, 119, 47, 105,
      57, 122, 109, 124, 115, 43, 120, 43, 27, 27, 18, 28, 48, 125, 107,
      114, 34, 133, 45, 120, 30, 127, 31, 116, 146
    ]

-- Re-reading the initial strings as Ints
-- (empty strings read as 0),
ns :: [(Int, Char)]
ns =
  (\x ->
      let s = fst x
      in ( if null s
             then 0
             else (read s :: Int)
         , snd x)) <$>
  xs

-- and sorting and grouping by these initial Ints,
-- interpreting them as data-collection bins.
bins :: [[(Int, Char)]]
bins =
  groupBy (on (==) fst) (sortBy (mappend (comparing fst) (comparing snd)) ns)

-- Forming bars by the ordered accumulation of final characters in each bin,
bars :: [(Int, String)]
bars = (fst . head &&& fmap snd) <$> bins

-- and obtaining a complete series, with empty bar strings
-- interpolated for any missing integers.
series :: [(Int, String)]
series =
  (concat . snd) $
  mapAccumL
    (\a x ->
        let n = fst x
        in if a == n
             then (a + 1, [x])
             else (n + 1, ((\i -> (i, "")) <$> [a .. (n - 1)]) ++ [x]))
    1
    bars

-- Assembling the series as a list of strings with right-justified indices,
justifyRight :: Int -> Char -> String -> String
justifyRight n c s = drop (length s) (replicate n c ++ s)

plotLines :: [String]
plotLines =
  foldr
    (\x a ->
        (justifyRight 2 ' ' (show (fst x)) ++ " |  " ++ intersperse ' ' (snd x)) :
        a)
    []
    series

-- and passing these over to IO as a single newline-delimited string.
main :: IO ()
main = putStrLn $ unlines plotLines

```

```txt
 0 |  7 7
 1 |  2 3 8 8
 2 |  3 5 7 7 7 7 7 7 8 8 9 9
 3 |  0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 |  0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 |  2 3 7 8 8
 6 |  1 3 8
 7 |  1
 8 |
 9 |  6 9
10 |  4 5 5 5 5 6 7 9 9 9
11 |  1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 |  0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 |  1 2 3 9
14 |  1 6
```



## HicEst

The dialog prompts for bitmap or a text image, and for the stem base.  Data are read in from clipboard.

```HicEst
REAL :: workspace(1000), base=16

DLG(CHeckbox=bitmap, NameEdit=base, DNum, MIn=1, MAx=16) ! 1 <= stem base <= 16
READ(ClipBoard, ItemS=nData) workspace    ! get raw data

ALIAS(workspace,1,  dataset,nData,  stems,nData)
SORT(Vector=dataset, Sorted=dataset)
stems = (dataset - MOD(dataset,base)) / base
dataset = dataset - base*stems
max_stem = MAX(stems)

IF( bitmap ) AXIS()
printed = 0
DO stem = 0, max_stem
  last = INDEX(stems, stem, 4) ! option 4: search backward
  IF( last > printed ) THEN
      nLeaves = last - printed
      IF(bitmap) THEN
        LINE(PenUp=1,W=8, x=0, y=stem, x=nLeaves, y=stem)
      ELSE
        ALIAS(dataset,printed+1,  leaves,nLeaves)
        WRITE(Format="i3, ':', 100Z2") stem, leaves
      ENDIF
      printed = printed + nLeaves
    ELSE
      WRITE(Format="i3, ':'") stem
    ENDIF
ENDDO
```

Shown is the given example for bitmap=0 and base 16

```txt
  0 : 7 7 C D
  1 : 2 2 7 9 B B B B B B C C D D E F F F F
  2 : 0 1 2 3 4 5 5 5 6 7 8 8 9 A A A A B B B C C C D E F
  3 : 0 0 4 5 9 A A D F
  4 : 4 7
  5 :
  6 : 0 3 8 9 9 9 9 A B D D D F
  7 : 1 1 1 1 2 2 2 3 3 3 4 4 4 4 5 5 5 5 6 6 7 7 8 8 9 9 A A B C C C D D D E F F F F
  8 : 0 0 3 4 5 B D
  9 : 2
```


=={{header|Icon}} and {{header|Unicon}}==

```unicon
procedure main(A)
    prune := integer(\A[1]) | 10   # Boundary between leaf and stem
    every put(data := [], integer(!&input))
    writes(right(oldStem := 0,5)," |")
    every item := !sort(data) do {
        leaf := item % prune
        stem := item / prune
        while (oldStem < stem) do writes("\n",right(oldStem +:= 1, 5)," |")
        writes(" ",right(leaf,*prune-1,"0"))
        }
    write()
end
```

Sample output from data.

```txt
->stem <stem.data
    0 | 7 7
    1 | 2 3 8 8
    2 | 3 5 7 7 7 7 7 7 8 8 9 9
    3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
    4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
    5 | 2 3 7 8 8
    6 | 1 3 8
    7 | 1
    8 |
    9 | 6 9
   10 | 4 5 5 5 5 6 7 9 9 9
   11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
   12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
   13 | 1 2 3 9
   14 | 1 6
->
```

And a second run with 2-digit leaves:

```txt
->stem 100 <stem.data
    0 | 07 07 12 13 18 18 23 25 27 27 27 27 27 27 28 28 29 29 30 31 31 31 31 32 33 34 35 36 37 37 37 38 39 40 40 41 42 42 42 42 43 43 43 44 44 44 45 46 47 48 48 52 53 57 58 58 61 63 68 71 96 99
    1 | 04 05 05 05 05 06 07 09 09 09 11 13 13 13 13 14 14 14 15 15 15 16 16 16 16 17 17 17 17 18 18 19 19 20 20 21 21 22 22 23 24 24 24 25 25 25 26 27 27 27 27 28 28 31 32 33 39 41 46
->
```



## J

'''Solution: (Tacit)'''

```j
stem        =: <.@(%&10)
leaf        =: 10&|
stemleaf    =: (stem@{. ; leaf)/.~ stem
expandStems =: <./ ([ + i.@>:@-~) >./
expandLeaves=: (expandStems e. ])@[ #inv ]

showStemLeaf=: (":@,.@expandStems@[ ; ":&>@expandLeaves)&>/@(>@{. ; <@{:)@|:@stemleaf@/:~
```


'''Solution: (Explicit)'''

```j
stemleafX=: monad define
  leaves=. 10 | y
  stems=. y <.@:% 10
  leaves=. stems </. leaves                           NB. group leaves by stem
  (<"0 ~.stems),.leaves
)

showStemLeafX=: monad define
  'stems leaves'=. (>@{. ; <@{:)@|: stemleafX /:~ y
  xstems=. (<./ ([ + i.@>:@-~ ) >./) stems            NB. stems including those with no leaves
  xleaves=. (xstems e. stems) #inv leaves             NB. expand leaves to match xstems
  (": ,.xstems) ; ":&> xleaves
)
```


'''Example:'''

```j
   nls =: ; <@(_&".);._2 noun define
12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36 29 31 125
139 131 115 105 132 104 123 35 113 122 42 117 119 58 109 23 105 63 27 44 105
99 41 128 121 116 125 32 61 37 127 29 113 121 58 114 126 53 114 96 25 109
7 31 141 46 13 27 43 117 116 27 7 68 40 31 115 124 42 128 52 71 118
117 38 27 106 33 117 116 111 40 119 47 105 57 122 109 124 115 43 120 43 27
27 18 28 48 125 107 114 34 133 45 120 30 127 31 116 146
)

   stemleaf nls        NB. display has been abbreviated
┌──┬─────────────────────────────────────────────┐
│1 │2 8 3 8                                      │
├──┼─────────────────────────────────────────────┤
│12│7 4 7 5 3 2 8 1 5 7 1 6 4 8 2 4 0 5 0 7      │
├──┼─────────────────────────────────────────────┤
│2 │8 9 3 7 9 5 7 7 7 7 7 8                      │
...

   showStemLeaf nls
┌──┬─────────────────────────────────────────────┐
│ 0│7 7                                          │
│ 1│2 3 8 8                                      │
│ 2│3 5 7 7 7 7 7 7 8 8 9 9                      │
│ 3│0 1 1 1 1 2 3 4 5 6 7 7 7 8 9                │
│ 4│0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8          │
│ 5│2 3 7 8 8                                    │
│ 6│1 3 8                                        │
│ 7│1                                            │
│ 8│                                             │
│ 9│6 9                                          │
│10│4 5 5 5 5 6 7 9 9 9                          │
│11│1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9│
│12│0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8      │
│13│1 2 3 9                                      │
│14│1 6                                          │
└──┴─────────────────────────────────────────────┘

   (showStemLeaf -: showStemLeafX) nls   NB. both solutions give same result
1
```


## Java

```java5
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class StemAndLeaf {
	private static int[] data = { 12, 127, 28, 42, 39, 113, 42, 18, 44, 118,
			44, 37, 113, 124, 37, 48, 127, 36, 29, 31, 125, 139, 131, 115, 105,
			132, 104, 123, 35, 113, 122, 42, 117, 119, 58, 109, 23, 105, 63,
			27, 44, 105, 99, 41, 128, 121, 116, 125, 32, 61, 37, 127, 29, 113,
			121, 58, 114, 126, 53, 114, 96, 25, 109, 7, 31, 141, 46, 13, 27,
			43, 117, 116, 27, 7, 68, 40, 31, 115, 124, 42, 128, 52, 71, 118,
			117, 38, 27, 106, 33, 117, 116, 111, 40, 119, 47, 105, 57, 122,
			109, 124, 115, 43, 120, 43, 27, 27, 18, 28, 48, 125, 107, 114, 34,
			133, 45, 120, 30, 127, 31, 116, 146 };

	public static Map<Integer, List<Integer>> createPlot(int... data){
		Map<Integer, List<Integer>> plot = new TreeMap<Integer, List<Integer>>();
		int highestStem = -1; //for filling in stems with no leaves
		for(int datum:data){
			int leaf = datum % 10;
			int stem = datum / 10; //integer division
			if(stem > highestStem){
				highestStem = stem;
			}
			if(plot.containsKey(stem)){
				plot.get(stem).add(leaf);
			}else{
				LinkedList<Integer> list = new LinkedList<Integer>();
				list.add(leaf);
				plot.put(stem, list);
			}
		}
		if(plot.keySet().size() < highestStem + 1 /*highest stem value and 0*/ ){
			for(int i = 0; i <= highestStem; i++){
				if(!plot.containsKey(i)){
					LinkedList<Integer> list = new LinkedList<Integer>();
					plot.put(i, list);
				}
			}
		}
		return plot;
	}

	public static void printPlot(Map<Integer, List<Integer>> plot){
		for(Map.Entry<Integer, List<Integer>> line : plot.entrySet()){
			Collections.sort(line.getValue());
			System.out.println(line.getKey() + " | " + line.getValue());
		}
	}

	public static void main(String[] args){
		Map<Integer, List<Integer>> plot = createPlot(data);
		printPlot(plot);
	}
}
```

```java5
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public interface StemAndLeaf {
  public static final int[] data = {12, 127, 28, 42, 39, 113, 42, 18, 44, 118,
    44, 37, 113, 124, 37, 48, 127, 36, 29, 31, 125, 139, 131, 115, 105,
    132, 104, 123, 35, 113, 122, 42, 117, 119, 58, 109, 23, 105, 63,
    27, 44, 105, 99, 41, 128, 121, 116, 125, 32, 61, 37, 127, 29, 113,
    121, 58, 114, 126, 53, 114, 96, 25, 109, 7, 31, 141, 46, 13, 27,
    43, 117, 116, 27, 7, 68, 40, 31, 115, 124, 42, 128, 52, 71, 118,
    117, 38, 27, 106, 33, 117, 116, 111, 40, 119, 47, 105, 57, 122,
    109, 124, 115, 43, 120, 43, 27, 27, 18, 28, 48, 125, 107, 114, 34,
    133, 45, 120, 30, 127, 31, 116, 146};

  public static Map<Integer, List<Integer>> createPlot(int... data) {
    Map<Integer, List<Integer>> plot = Arrays.stream(data)
      .parallel()
      .boxed()
      .collect(
        Collectors.groupingBy(
          datum -> datum / 10, // stem, integer division
          Collectors.mapping(
            datum -> datum % 10, // leaf
            Collectors.toList()
          )
        )
      )
    ;
    int highestStem = Arrays.stream(data)
      .parallel()
      .map(datum -> datum / 10)
      .max()
      .orElse(-1) //for filling in stems with no leaves
    ;
    Optional.of(plot)
      .map(Map::keySet)
      .map(Collection::size)
      .filter(size -> size < highestStem + 1 /*highest stem value and 0*/)
      .ifPresent(p ->
        IntStream.rangeClosed(
          0,
          highestStem
        )
          .parallel()
          .forEach(i ->
            plot.computeIfAbsent(i, $ -> new LinkedList<>())
          )
      )
    ;
    return plot;
  }

  public static void printPlot(Map<Integer, List<Integer>> plot) {
    plot.entrySet()
      .stream()
      .parallel()
      .peek(line -> Optional.of(line)
        .map(Map.Entry::getValue)
        .ifPresent(Collections::sort)
      )
      .map(line ->
        String.join(" ",
          String.valueOf(line.getKey()),
          "|",
          String.valueOf(line.getValue())
        )
      )
      .forEachOrdered(System.out::println)
    ;
  }

  public static void main(String... arguments) {
    Optional.of(data)
      .map(StemAndLeaf::createPlot)
      .ifPresent(StemAndLeaf::printPlot)
    ;
  }
}
```

Output:

```txt
0 | [7, 7]
1 | [2, 3, 8, 8]
2 | [3, 5, 7, 7, 7, 7, 7, 7, 8, 8, 9, 9]
3 | [0, 1, 1, 1, 1, 2, 3, 4, 5, 6, 7, 7, 7, 8, 9]
4 | [0, 0, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 6, 7, 8, 8]
5 | [2, 3, 7, 8, 8]
6 | [1, 3, 8]
7 | [1]
8 | []
9 | [6, 9]
10 | [4, 5, 5, 5, 5, 6, 7, 9, 9, 9]
11 | [1, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 9]
12 | [0, 0, 1, 1, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 7, 7, 7, 7, 8, 8]
13 | [1, 2, 3, 9]
14 | [1, 6]
```



## JavaScript


### JavaScript + DOM

It turns out that HTML+CSS renders the plot quite attractively.


```html4strict
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
<title>stem and leaf plot</title>
<script type='text/javascript'>

    function has_property(obj, propname) {
        return typeof(obj[propname]) === "undefined" ? false : true;
    }

    function compare_numbers(a, b) {return a-b;}

    function stemplot(data, target) {
        var stem_data = {};
        var all_stems = [];
        for (var i = 0; i < data.length; i++) {
            var stem = Math.floor(data[i] / 10);
            var leaf = Math.round(data[i] % 10);
            if (has_property(stem_data, stem)) {
                stem_data[stem].push(leaf);
            } else {
                stem_data[stem] = [leaf];
                all_stems.push(stem);
            }
        }
        all_stems.sort(compare_numbers);

        var min_stem = all_stems[0];
        var max_stem = all_stems[all_stems.length - 1];

        var table = document.createElement('table');
        for (var stem = min_stem; stem <= max_stem; stem++) {
            var row = document.createElement('tr');
            var label = document.createElement('th');
            row.appendChild(label);
            label.appendChild(document.createTextNode(stem));
            if (has_property(stem_data, stem)) {
                stem_data[stem].sort(compare_numbers);
                for (var i = 0; i < stem_data[stem].length; i++) {
                    var cell = document.createElement('td');
                    cell.appendChild(document.createTextNode(stem_data[stem][i]));
                    row.appendChild(cell);
                }
            }
            table.appendChild(row);
        }
        target.appendChild(table);
    }

</script>
<style type='text/css'>
    body {font-family: monospace;}
    table {border-collapse: collapse;}
    th {border-right: 1px solid black; text-align: right;}
    td {text-align: right;}
</style>
</head>
<body>

<div id="target"></div>

<script type='text/javascript'>

    var data = [
        12,127,28,42,39,113,42,18,44,118,44,37,113,124,37,48,127,36,29,31,125,139,131,
        115,105,132,104,123,35,113,122,42,117,119,58,109,23,105,63,27,44,105,99,41,128,
        121,116,125,32,61,37,127,29,113,121,58,114,126,53,114,96,25,109,7,31,141,46,13,
        27,43,117,116,27,7,68,40,31,115,124,42,128,52,71,118,117,38,27,106,33,117,116,
        111,40,119,47,105,57,122,109,124,115,43,120,43,27,27,18,28,48,125,107,114,34,
        133,45,120,30,127,31,116,146
    ];
    stemplot(data, document.getElementById('target'));

</script>

</body>
</html>
```


The output looks like:

[[File:Stemplot.png]]


### JavaScript ES6


```javascript
(() => {
    // main :: IO String
    const main = () => {

        // Strings derived from integers,
        // and split into [(initial string, final character)] tuples.

        // xs :: [(String, Char)]
        const xs = map(n => fanArrow(init, last)(n.toString()), [
            12, 127, 28, 42, 39, 113, 42, 18, 44, 118, 44, 37, 113, 124,
            37, 48, 127, 36, 29, 31, 125, 139, 131, 115, 105, 132, 104,
            123, 35, 113, 122, 42, 117, 119, 58, 109, 23, 105, 63, 27,
            44, 105, 99, 41, 128, 121, 116, 125, 32, 61, 37, 127, 29, 113,
            121, 58, 114, 126, 53, 114, 96, 25, 109, 7, 31, 141, 46, 13, 27,
            43, 117, 116, 27, 7, 68, 40, 31, 115, 124, 42, 128, 52, 71, 118,
            117, 38, 27, 106, 33, 117, 116, 111, 40, 119, 47, 105, 57, 122,
            109, 124, 115, 43, 120, 43, 27, 27, 18, 28, 48, 125, 107, 114,
            34, 133, 45, 120, 30, 127, 31, 116, 146
        ]);

        // Re-reading the initial strings as Ints
        // (empty strings read as 0),

        // ns :: [(Int, Char)]
        const ns = map(x => {
            const s = fst(x);
            return Tuple(s.length > 0 ? (
                parseInt(s, 10)
            ) : 0, snd(x));
        }, xs);

        // and sorting and grouping by these initial Ints,
        // interpreting them as data-collection bins.

        // bins :: [[(Int, Char)]]
        const bins =
            groupBy(
                (a, b) => a[0] === b[0],
                sortBy(mappendComparing([
                    [fst, true],
                    [snd, true]
                ]), ns)
            );

        // Forming bars by the ordered accumulation of
        // final characters in each bin,

        // bars :: [(Int, String)]
        const bars = map(
            fanArrow(
                x => fst(x[0]),
                x => map(snd, x)
            ),
            bins
        );

        // and obtaining a complete series, with empty bars
        // interpolated for any missing integers.

        // series :: [(Int, String)]
        const series = concat(mapAccumL(
            (a, x) => {
                const n = x[0];
                return a !== n ? (
                    Tuple(1 + n,
                        map(i => Tuple(i, []),
                            enumFromToInt(a, n - 1)
                        )
                        .concat([x])
                    )
                ) : Tuple(1 + a, [x]);
            }, 7, bars
        )[1]);

        // Assembling the series as a list of strings with
        // right-justified indices,

        // plotLines :: [String]
        const plotLines = foldr(
            (x, a) => cons(concat([
                justifyRight(2, ' ', x[0].toString()),
                ' |  ',
                unwords(x[1])
            ]), a), [],
            series
        );

        // and passing these over to IO as a single
        // newline-delimited string.

        return unlines(plotLines);
    };

    // GENERIC FUNCTIONS -----------------------------------------------------

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b
    });

    // compare :: a -> a -> Ordering
    const compare = (a, b) => a < b ? -1 : (a > b ? 1 : 0);

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        xs.length > 0 ? (() => {
            const unit = typeof xs[0] === 'string' ? '' : [];
            return unit.concat.apply(unit, xs);
        })() : [];

    // cons :: a -> [a] -> [a]
    const cons = (x, xs) => [x, ...xs];

    // enumFromToInt :: Int -> Int -> [Int]
    const enumFromToInt = (m, n) =>
        n >= m ? Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i) : [];

    // Compose a function from a simple value to a tuple of
    // the separate outputs of two different functions
    // fanArrow (&&&) :: (a -> b) -> (a -> c) -> (a -> (b, c))
    const fanArrow = (f, g) => x => Tuple(f(x), g(x));

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f => (a, b) => f.apply(null, [b, a]);

    // Note that that the Haskell signature of foldr is different from that of
    // foldl - the positions of accumulator and current value are reversed
    // foldr :: (a -> b -> b) -> b -> [a] -> b
    const foldr = (f, a, xs) => xs.reduceRight(flip(f), a);

    // fst :: (a, b) -> a
    const fst = tpl => tpl.type !== 'Tuple' ? undefined : tpl[0];

    // Typical usage: groupBy(on(eq, f), xs)
    // groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
    const groupBy = (f, xs) => {
        const dct = xs.slice(1)
            .reduce((a, x) => {
                const h = a.active.length > 0 ? a.active[0] : undefined;
                return h !== undefined && f(h, x) ? {
                    active: a.active.concat([x]),
                    sofar: a.sofar
                } : {
                    active: [x],
                    sofar: a.sofar.concat([a.active])
                };
            }, {
                active: xs.length > 0 ? [xs[0]] : [],
                sofar: []
            });
        return dct.sofar.concat(dct.active.length > 0 ? [dct.active] : []);
    };

    // init :: [a] -> [a]
    const init = xs => xs.length > 0 ? xs.slice(0, -1) : undefined;

    // justifyRight :: Int -> Char -> String -> String
    const justifyRight = (n, cFiller, strText) =>
        n > strText.length ? (
            (cFiller.repeat(n) + strText)
            .slice(-n)
        ) : strText;

    // last :: [a] -> a
    const last = xs => xs.length ? xs.slice(-1)[0] : undefined;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
    const mapAccumL = (f, acc, xs) =>
        xs.reduce((a, x, i) => {
            const pair = f(a[0], x, i);
            return Tuple(pair[0], a[1].concat(pair[1]));
        }, Tuple(acc, []));

    // mappendComparing :: [((a -> b), Bool)] -> (a -> a -> Ordering)
    const mappendComparing = fboolPairs =>
        (x, y) => fboolPairs.reduce(
            (ordr, fb) => {
                const f = fb[0];
                return ordr !== 0 ? (
                    ordr
                ) : fb[1] ? (
                    compare(f(x), f(y))
                ) : compare(f(y), f(x));
            }, 0
        );

    // snd :: (a, b) -> b
    const snd = tpl => tpl.type !== 'Tuple' ? undefined : tpl[1];

    // sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    const sortBy = (f, xs) =>
        xs.slice()
        .sort(f);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // unwords :: [String] -> String
    const unwords = xs => xs.join(' ');

    // MAIN ------------------------------------------------------------------
    return main();
})();
```

```txt
 0 |  7 7
 1 |  2 3 8 8
 2 |  3 5 7 7 7 7 7 7 8 8 9 9
 3 |  0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 |  0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 |  2 3 7 8 8
 6 |  1 3 8
 7 |  1
 8 |
 9 |  6 9
10 |  4 5 5 5 5 6 7 9 9 9
11 |  1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 |  0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 |  1 2 3 9
14 |  1 6
```



## jq


```jq
def stem_and_leaf:

  # align-right:
  def right: tostring | (4-length) * " " + .;

  sort
  | .[0] as $min
  | .[length-1] as $max
  | "\($min/10|floor|right) | " as $stem
  | reduce .[] as $d
      # state: [ stem, string ]
      ( [ 0, $stem ];
        .[0] as $stem
        | if ($d/10) | floor == $stem
          then [ $stem,     (.[1] +                      "\($d % 10)" )]
          else [ $stem + 1, (.[1] + "\n\($stem+1|right) | \($d % 10)" )]
          end )
  | .[1] ;
```

'''Example''':

```jq
def data:
 [ 12,127,28,42,39,113, 42,18,44,118,44,37,113,124,37,48,127,36,29,31,
   125,139,131,115,105,132,104,123,35,113,122,42,117,119,58,109,23,105,
   63,27,44,105,99,41,128,121,116,125,32,61,37,127,29,113,121,58,114,126,
   53,114,96,25,109,7,31,141,46,13,27,43,117,116,27,7,68,40,31,115,124,42,
   128,52,71,118,117,38,27,106,33,117,116,111,40,119,47,105,57,122,109,
   124,115,43,120,43,27,27,18,28,48,125,107,114,34,133,45,120, 30,127,
   31,116,146
 ];

data | stem_and_leaf

```

```sh

 $ jq -n -r -f stem-and-leaf_plot.jq
   0 | 77
   1 | 2388
   2 | 357777778899
   3 | 011112345677789
   4 | 001222233344456788
   5 | 23788
   6 | 138
   7 | 1
   8 | 6
   9 | 9
  10 | 4555567999
  11 | 13333444555666677778899
  12 | 00112234445556777788
  13 | 1239
  14 | 16
```



## Julia

'''The Function'''

This is a rather elaborate function that creates a string depicting a stem and leaf plot.  Much of the elaboration is to handle the case of negative numbers that have a stem of 0.  There is also a bit of work to allow for leaf sizes other than 1 (some power of 10).

```Julia

function stemleaf{T<:Real}(a::Array{T,1}, leafsize=1)
    ls = 10^int(log10(leafsize))
    (stem, leaf) = divrem(sort(int(a/ls)), 10)
    leaf[sign(stem) .== -1] *= -1
    negzero = leaf .< 0
    if any(negzero)
        leaf[negzero] *= -1
        nz = @sprintf "%10s | " "-0"
        nz *= join(map(string, leaf[negzero]), " ")
        nz *= "\n"
        stem = stem[!negzero]
        leaf = leaf[!negzero]
    else
        nz = ""
    end
    slp = ""
    for i in stem[1]:stem[end]
        i != 0 || (slp *= nz)
        slp *= @sprintf "%10d | " i
        slp *= join(map(string, leaf[stem .== i]), " ")
        slp *= "\n"
    end
    slp *= " Leaf Unit = " * string(convert(T, ls)) * "\n"
    return slp
end

```


'''Main'''

```Julia

println("Using the Task's Test Data")
test = """12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36 29
   31 125 139 131 115 105 132 104 123 35 113 122 42 117 119 58 109 23 105
   63 27 44 105 99 41 128 121 116 125 32 61 37 127 29 113 121 58 114 126
   53 114 96 25 109 7 31 141 46 13 27 43 117 116 27 7 68 40 31 115 124 42
   128 52 71 118 117 38 27 106 33 117 116 111 40 119 47 105 57 122 109
   124 115 43 120 43 27 27 18 28 48 125 107 114 34 133 45 120 30 127 31
   116 146"""
test = map(parseint, split(test, r"\s"))
println(stemleaf(test))

println("Test with Reals and Negative Zero Stem")
test = [-23.678758, -12.45, -3.4, 4.43, 5.5, 5.678, 16.87, 24.7, 56.8]
println(stemleaf(test))

println("Test with Leaf Size Scaling")
test = int(500*randn(20))
println("Using:  ", test)
println(stemleaf(test, 10))

```


```txt

Using the Task's Test Data
         0 | 7 7
         1 | 2 3 8 8
         2 | 3 5 7 7 7 7 7 7 8 8 9 9
         3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
         4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
         5 | 2 3 7 8 8
         6 | 1 3 8
         7 | 1
         8 |
         9 | 6 9
        10 | 4 5 5 5 5 6 7 9 9 9
        11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
        12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
        13 | 1 2 3 9
        14 | 1 6
 Leaf Unit = 1

Test with Reals and Negative Zero Stem
        -2 | 4
        -1 | 2
        -0 | 3
         0 | 4 6 6
         1 | 7
         2 | 5
         3 |
         4 |
         5 | 7
 Leaf Unit = 1.0

Test with Leaf Size Scaling
Using:  [318,1163,-35,-611,-436,-127,-374,-150,119,541,-670,-558,3,592,604,1181,-180,419,829,-364]
        -6 | 7 1
        -5 | 6
        -4 | 4
        -3 | 7 6
        -2 |
        -1 | 8 5 3
        -0 | 4
         0 | 0
         1 | 2
         2 |
         3 | 2
         4 | 2
         5 | 4 9
         6 | 0
         7 |
         8 | 3
         9 |
        10 |
        11 | 6 8
 Leaf Unit = 10

```



## Kotlin

```scala
// version 1.1.2

fun leafPlot(x: IntArray) {
    x.sort()
    var i = x[0] / 10 - 1
    for (j in 0 until x.size) {
        val d = x[j] / 10
        while (d > i) print("%s%3d |".format(if (j != 0) "\n" else "", ++i))
        print(" ${x[j] % 10}")
    }
    println()
}

fun main(args: Array<String>) {
    val data = intArrayOf(
         12, 127,  28,  42,  39, 113,  42,  18,  44, 118,  44,  37, 113, 124,
	 37,  48, 127,  36,  29,  31, 125, 139, 131, 115, 105, 132, 104, 123,
	 35, 113, 122,  42, 117, 119,  58, 109,  23, 105,  63,  27,  44, 105,
	 99,  41, 128, 121, 116, 125,  32,  61,  37, 127,  29, 113, 121,  58,
	114, 126,  53, 114,  96,  25, 109,   7,  31, 141,  46,  13,  27,  43,
	117, 116,  27,   7,  68,  40,  31, 115, 124,  42, 128,  52,  71, 118,
	117,  38,  27, 106,  33, 117, 116, 111,  40, 119,  47, 105,  57, 122,
	109, 124, 115,  43, 120,  43,  27,  27,  18,  28,  48, 125, 107, 114,
	 34, 133,  45, 120,  30, 127,  31, 116, 146
    )
    leafPlot(data)
}
```


```txt

  0 | 7 7
  1 | 2 3 8 8
  2 | 3 5 7 7 7 7 7 7 8 8 9 9
  3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 | 2 3 7 8 8
  6 | 1 3 8
  7 | 1
  8 |
  9 | 6 9
 10 | 4 5 5 5 5 6 7 9 9 9
 11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 | 1 2 3 9
 14 | 1 6

```



## Lua


```lua
data = { 12,127,28,42,39,113, 42,18,44,118,44,37,113,124,37,48,127,36,29,31,
	     125,139,131,115,105,132,104,123,35,113,122,42,117,119,58,109,23,105,
	     63,27,44,105,99,41,128,121,116,125,32,61,37,127,29,113,121,58,114,126,
	     53,114,96,25,109,7,31,141,46,13,27,43,117,116,27,7,68,40,31,115,124,42,
	     128,52,71,118,117,38,27,106,33,117,116,111,40,119,47,105,57,122,109,
	     124,115,43,120,43,27,27,18,28,48,125,107,114,34,133,45,120, 30,127,
	     31,116,146
       }

table.sort( data )

min, max = data[1], data[#data]

p = 1
for stem = math.floor(min/10), math.floor(max/10) do
    io.write( string.format( "%2d | ", stem ) )

    while data[p] ~= nil and math.floor( data[p]/10 ) == stem do
        io.write( string.format( "%2d ", data[p] % 10 ) )
        p = p + 1
    end

    print ""
end
```

Output:

```txt
 0 |  7  7
 1 |  2  3  8  8
 2 |  3  5  7  7  7  7  7  7  8  8  9  9
 3 |  0  1  1  1  1  2  3  4  5  6  7  7  7  8  9
 4 |  0  0  1  2  2  2  2  3  3  3  4  4  4  5  6  7  8  8
 5 |  2  3  7  8  8
 6 |  1  3  8
 7 |  1
 8 |
 9 |  6  9
10 |  4  5  5  5  5  6  7  9  9  9
11 |  1  3  3  3  3  4  4  4  5  5  5  6  6  6  6  7  7  7  7  8  8  9  9
12 |  0  0  1  1  2  2  3  4  4  4  5  5  5  6  7  7  7  7  8  8
13 |  1  2  3  9
14 |  1  6
```




## Maple


```Maple
StemPlot := proc( datatable::{rtable,list,algebraic} )
    local i, j, k, tf, LeafStemTable, LeafStemIndices;
    k:=0;

    LeafStemTable := ListTools:-Categorize( (x,y) -> iquo(x, 10) = iquo(y, 10), sort(datatable));

    if LeafStemTable = NULL then
        error "Empty List";
    elif nops( [ LeafStemTable ] ) = 1 or not( type( LeafStemTable[2], list) ) then
	LeafStemTable := [ LeafStemTable ];
    end if;

    LeafStemIndices := { seq( iquo( LeafStemTable[i][1], 10 ), i = 1..nops( [ LeafStemTable ] ) ) };

    for i from min( LeafStemIndices ) to max( LeafStemIndices ) do

        if i in LeafStemIndices then
            k := k + 1;

            if i = 0 then

		if min( datatable ) >=0 then
		    printf( "%-4a%s%-s\n", i,  "  |  ", StringTools:-Remove( "[],", convert( [seq( abs( irem( LeafStemTable[k][j], 10 ) ), j = 1..nops( LeafStemTable[k] ) )], string ) ) );
	        else
	            tf := ListTools:-Occurrences( true, (x->type(x,negative))~(LeafStemTable[k]));
		    printf( "%s%-4a%s%-s\n", "-", i,  " |  ", StringTools:-Remove( "[],", convert( [seq( abs( irem( LeafStemTable[k][j], 10 ) ), j = 1 .. tf )], string ) ) );
	            printf( "%-4a%s%-s\n", i,  "  |  ", StringTools:-Remove( "[],", convert( [seq( abs( irem( LeafStemTable[k][j], 10 ) ), j = tf + 1 .. nops( LeafStemTable[k] ) )], string ) ) );
	        end if;

            else

               	printf( "%-4a%s%-s\n", i,  "  |  ", StringTools:-Remove( "[],", convert( [seq( abs( irem( LeafStemTable[k][j], 10 ) ), j = 1..nops( LeafStemTable[k] ) )], string ) ) );

	    end if;

        else

	    printf( "%-4a%s\n", i, "  |  " );

	end if;

    end do;

    return NULL;
end proc:

Y := [ 12, 127,  28,  42,  39, 113,  42,  18,  44, 118,  44,  37, 113, 124, 37,  48, 127,  36,  29,  31, 125, 139, 131, 115, 105, 132, 104, 123, 35, 113, 122,  42, 117, 119,  58, 109,  23, 105,  63,  27,  44, 105, 99,  41, 128, 121, 116, 125,  32,  61,  37, 127,  29, 113, 121,  58, 114, 126,  53, 114,  96,  25, 109,   7,  31, 141,  46,  13,  27,  43, 117, 116,  27,   7,  68,  40,  31, 115, 124,  42, 128,  52,  71, 118, 117,  38,  27, 106,  33, 117, 116, 111,  40, 119,  47, 105,  57, 122, 109, 124, 115,  43, 120,  43,  27,  27,  18,  28,  48, 125, 107, 114, 34, 133,  45, 120,  30, 127,  31, 116, 146];

StemPlot(Y);
```



```txt
0     |  7 7
1     |  2 3 8 8
2     |  3 5 7 7 7 7 7 7 8 8 9 9
3     |  0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
4     |  0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
5     |  2 3 7 8 8
6     |  1 3 8
7     |  1
8     |
9     |  6 9
10    |  4 5 5 5 5 6 7 9 9 9
11    |  1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12    |  0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13    |  1 2 3 9
14    |  1 6
```



## Mathematica


```Mathematica
len[n_] := RealDigits[n][[2]]; padding = len[Max@ Quotient[inputdata, 10]];

For[i = Min@ Quotient[inputdata, 10],i <= Max@ Quotient[inputdata, 10], i++,
 (Print[i, If[(padding - len[i]) > 0, (padding - len[i])*" " <> " |", " |"] ,
 StringJoin[(" " <> #) & /@ Map[ToString, #]]])&@
  Select[{Quotient[#, 10], Mod[#, 10]} & /@ Sort[inputdata],Part[#, 1] == i &][[;; , 2]]]
```



```txt
0  | 7 7
1  | 2 3 8 8
2  | 3 5 7 7 7 7 7 7 8 8 9 9
3  | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
4  | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
5  | 2 3 7 8 8
6  | 1 3 8
7  | 1
8  |
9  | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6
```


=={{header|MATLAB}} / {{header|Octave}}==


```Matlab
function stem_and_leaf_plot(x,stem_unit,leaf_unit)
  if nargin < 2, stem_unit = 10; end;
  if nargin < 3,
    leaf_unit = 1;
  else
    x = leaf_unit*round(x/leaf_unit);
  end;

  stem = floor(x/stem_unit);
  leaf = mod(x,stem_unit);

  for k = min(stem):max(stem)
    printf('\n%d |',k)
    printf(' %d' ,sort(leaf(k==stem)))
  end;
  printf('\nkey:6|3=63\n');
  printf('leaf unit: %.1f\n',leaf_unit);
  printf('stem unit: %.1f\n',stem_unit);
end;

x = [12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36 29 31 125 139 131 115 105 132 104 123 35 113 122 42 117 119 58 109 23 105 63 27 44 105 99 41 128 121 116 125 32 61 37 127 29 113 121 58 114 126 53 114 96 25 109 7 31 141 46 13 27 43 117 116 27 7 68 40 31 115 124 42 128 52 71 118 117 38 27 106 33 117 116 111 40 119 47 105 57 122 109 124 115 43 120 43 27 27 18 28 48 125 107 114 34 133 45 120 30 127 31 116 146];

stem_and_leaf_plot(x);
```

Output:

```txt

0 | 7 7
1 | 2 3 8 8
2 | 3 5 7 7 7 7 7 7 8 8 9 9
3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
5 | 2 3 7 8 8
6 | 1 3 8
7 | 1
8 |
9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6
key:6|3=63
leaf unit: 1.0
stem unit: 10.0
```



## Maxima


```maxima
load(descrptive)$

data: [12, 127, 28, 42, 39, 113, 42, 18, 44, 118, 44, 37, 113, 124, 37, 48, 127,
   36, 29, 31, 125, 139, 131, 115, 105, 132, 104, 123, 35, 113, 122, 42, 117, 119,
   58, 109, 23, 105, 63, 27, 44, 105, 99, 41, 128, 121, 116, 125, 32, 61, 37, 127,
   29, 113, 121, 58, 114, 126, 53, 114, 96, 25, 109, 7, 31, 141, 46, 13, 27, 43,
   117, 116, 27, 7, 68, 40, 31, 115, 124, 42, 128, 52, 71, 118, 117, 38, 27, 106,
   33, 117, 116, 111, 40, 119, 47, 105, 57, 122, 109, 124, 115, 43, 120, 43, 27,
   27, 18, 28, 48, 125, 107, 114, 34, 133, 45, 120, 30, 127, 31, 116, 146]$

stemplot(data);
 0|77
 1|2388
 2|357777778899
 3|011112345677789
 4|001222233344456788
 5|23788
 6|138
 7|1
 9|69
10|4555567999
11|13333444555666677778899
12|00112234445556777788
13|1239
14|16
```



## Nim


```nim
import tables
import math
import strutils
import algorithm

type
  StemLeafPlot = ref object
    leafDigits: int
    multiplier: int
    plot: TableRef[int, seq[int]]

proc `$`(s: seq[int]): string =
  result = ""
  for item in s:
    result &= $item & " "

proc `$`(self: StemLeafPlot): string =
  result = ""
  var keys: seq[int] = @[]
  for stem, _ in self.plot:
    keys.add(stem)
  for printedStem in keys.min..keys.max:
    result &= align($printedStem & " | ", ($keys.max).len + 4)
    if printedStem in keys:
      self.plot[printedStem].sort(system.cmp[int])
      result &= $self.plot[printedStem]
    result &= "\n"

proc parse(self: StemLeafPlot, value: int): tuple[stem, leaf: int] =
  (value div self.multiplier, abs(value mod self.multiplier))

proc init[T](self: StemLeafPlot, leafDigits: int, data: openArray[T]) =
  self.leafDigits = leafDigits
  self.multiplier = 10 ^ leafDigits
  self.plot = newTable[int, seq[int]]()
  for value in data:
    let (stem, leaf) = self.parse(value)
    if stem notin self.plot:
      self.plot[stem] = @[leaf]
    else:
      self.plot[stem].add(leaf)

var taskData = @[12, 127,  28,  42,  39, 113,  42,  18,  44, 118,  44,  37, 113, 124,
                 37,  48, 127,  36,  29,  31, 125, 139, 131, 115, 105, 132, 104, 123,
                 35, 113, 122,  42, 117, 119,  58, 109,  23, 105,  63,  27,  44, 105,
                 99,  41, 128, 121, 116, 125,  32,  61,  37, 127,  29, 113, 121,  58,
                 114, 126,  53, 114,  96,  25, 109,   7,  31, 141,  46,  13,  27,  43,
                 117, 116,  27,   7,  68,  40,  31, 115, 124,  42, 128,  52,  71, 118,
                 117,  38,  27, 106,  33, 117, 116, 111,  40, 119,  47, 105,  57, 122,
                 109, 124, 115,  43, 120,  43,  27,  27,  18,  28,  48, 125, 107, 114,
                 34, 133,  45, 120,  30, 127,  31, 116, 146]

var negativeData = @[-24, -12, -3, 4, 6, 6, 17, 25, 57]

echo "Using the Task's Test Data"
var taskPlot = StemLeafPlot()
taskPlot.init(1, taskData)
echo $taskPlot

echo "Test with Negative Stem"
var negativePlot = StemLeafPlot()
negativePlot.init(1, negativeData)
echo $negativePlot
```


```txt
Using the Task's Test Data
  0 | 7 7
  1 | 2 3 8 8
  2 | 3 5 7 7 7 7 7 7 8 8 9 9
  3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 | 2 3 7 8 8
  6 | 1 3 8
  7 | 1
  8 |
  9 | 6 9
 10 | 4 5 5 5 5 6 7 9 9 9
 11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 | 1 2 3 9
 14 | 1 6

Test with Reals and Negative Zero Stem
-2 | 4
-1 | 2
 0 | 3 4 6 6
 1 | 7
 2 | 5
 3 |
 4 |
 5 | 7
```



## OCaml


The definition of the function <code>unique</code> below can be omited if one uses the [http://code.google.com/p/ocaml-extlib/ extlib].


```ocaml
let unique li =
  let rec aux acc = function
  | [] -> (List.rev acc)
  | x::xs ->
      if List.mem x acc
      then aux acc xs
      else aux (x::acc) xs
  in
  aux [] li
```



```ocaml
let data =
  [ 12; 127; 28; 42; 39; 113; 42; 18; 44; 118; 44; 37; 113; 124; 37; 48;
    127; 36; 29; 31; 125; 139; 131; 115; 105; 132; 104; 123; 35; 113; 122;
    42; 117; 119; 58; 109; 23; 105; 63; 27; 44; 105; 99; 41; 128; 121; 116;
    125; 32; 61; 37; 127; 29; 113; 121; 58; 114; 126; 53; 114; 96; 25; 109;
    7; 31; 141; 46; 13; 27; 43; 117; 116; 27; 7; 68; 40; 31; 115; 124; 42;
    128; 52; 71; 118; 117; 38; 27; 106; 33; 117; 116; 111; 40; 119; 47; 105;
    57; 122; 109; 124; 115; 43; 120; 43; 27; 27; 18; 28; 48; 125; 107; 114;
    34; 133; 45; 120; 30; 127; 31; 116; 146 ]

let data =
  List.map (fun d -> (d / 10, d mod 10)) data

let keys =
  List.sort compare (unique (List.map fst data))

let () =
  List.iter (fun key ->
    Printf.printf " %2d |" key;
    let vs = List.filter (fun (a,_) -> a = key) data in
    let vs = List.sort compare (List.map snd vs) in
    List.iter (Printf.printf " %d") vs;
    print_newline()
  ) keys
```


we can output the same latex code than the Perl example replacing the main function as follow:


```ocaml
let () =
  print_endline "\
\\documentclass{report}
\\usepackage{fullpage}
\\begin{document}
  \\begin{tabular}{ r | *{120}{c} }";

  List.iter (fun key ->
    Printf.printf "    %d" key;
    let vs = List.filter (fun (a,_) -> a = key) data in
    let vs = List.sort compare (List.map snd vs) in
    List.iter (Printf.printf " & %d") vs;
    print_endline " \\\\"
  ) keys;

  print_endline "\
  \\end{tabular}
\\end{document}"
```



## Perl


```perl
my @data = sort {$a <=> $b} qw( 12  127 28  42  39  113 42  18  44  118 44
37  113 124 37  48  127 36  29  31  125 139 131 115 105 132 104 123 35  113
122 42  117 119 58  109 23  105 63  27  44  105 99  41  128 121 116 125 32
61  37  127 29  113 121 58  114 126 53  114 96  25  109 7   31  141 46  13
27  43  117 116 27  7   68  40  31  115 124 42  128 52  71  118 117 38  27
106 33  117 116 111 40  119 47  105 57  122 109 124 115 43  120 43  27  27
18  28  48  125 107 114 34  133 45  120 30  127 31  116 );
my $columns = @data;

my $laststem = undef;

for my $value (@data) {
  my $stem = int($value / 10);
  my $leaf = $value % 10;
  while (not defined $laststem or $stem > $laststem) {
    if (not defined $laststem) {
      $laststem = $stem - 1;
    } else {
      print " \n";
    }
    $laststem++;
    printf "%3d |", $laststem;
  }
  print " $leaf";
}

```

```txt
  0 | 7 7
  1 | 2 3 8 8
  2 | 3 5 7 7 7 7 7 7 8 8 9 9
  3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 | 2 3 7 8 8
  6 | 1 3 8
  7 | 1
  8 |
  9 | 6 9
 10 | 4 5 5 5 5 6 7 9 9 9
 11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 | 1 2 3 9
 14 | 1
```



###  LaTeX output

generating {{header|LaTeX}}

```perl
#!/usr/bin/perl -w

my @data = sort {$a <=> $b} qw( 12  127 28  42  39  113 42  18  44  118 44
37  113 124 37  48  127 36  29  31  125 139 131 115 105 132 104 123 35  113
122 42  117 119 58  109 23  105 63  27  44  105 99  41  128 121 116 125 32
61  37  127 29  113 121 58  114 126 53  114 96  25  109 7   31  141 46  13
27  43  117 116 27  7   68  40  31  115 124 42  128 52  71  118 117 38  27
106 33  117 116 111 40  119 47  105 57  122 109 124 115 43  120 43  27  27
18  28  48  125 107 114 34  133 45  120 30  127 31  116 );

# FIXME: This should count the maximum number of leaves in any one stem;
# instead it takes the total number of data items, which is usually
# a massive overestimate.
my $columns = @data;

print <<"EOT";
\\documentclass{report}
\\usepackage{fullpage}
\\begin{document}
  \\begin{tabular}{ r | *{$columns}{c} }
EOT

my $laststem = undef;

for my $value (@data) {
  my $stem = int($value / 10);
  my $leaf = $value % 10;
  while (not defined $laststem or $stem > $laststem) {
    if (not defined $laststem) {
      $laststem = $stem - 1;
    } else {
      print " \\\\\n";
    }
    $laststem++;
    print "    $laststem";
  }
  printf " & $leaf";
}
print <<'EOT';

  \end{tabular}
\end{document}
EOT
```


LaTeX output of the Perl program:


```latex
\documentclass{report}
\usepackage{fullpage}
\begin{document}
  \begin{tabular}{ r | *{120}{c} }
    0 & 7 & 7 \\
    1 & 2 & 3 & 8 & 8 \\
    2 & 3 & 5 & 7 & 7 & 7 & 7 & 7 & 7 & 8 & 8 & 9 & 9 \\
    ...
    13 & 1 & 2 & 3 & 9 \\
    14 & 1
  \end{tabular}
\end{document}
```


The parameter to the <code>tabular</code> environment defines the columns of the table. “r” and “c” are right- and center-aligned columns, “|” is a vertical rule, and “<code>*{''count''}{''cols''}”</code> repeats a column definition ''count'' times.

To get from the program above to a rendered PDF,

 perl ./Stem-perl.pl > plot.tex && pdflatex plot.tex

and the output will be in <code>plot.pdf</code>. [http://switchb.org/kpreid/2009/12-24-rc-stemplot-perl-latex-output Output.]


## Perl 6

Handles negative stems properly.

```perl6
my @data = <
     12 127  28  42  39 113  42  18  44 118  44
     37 113 124  37  48 127  36  29  31 125 139
    131 115 105 132 104 123  35 113 122  42 117
    119  58 109  23 105  63  27  44 105  99  41
    128 121 116 125  32  61  37 127  29 113 121
     58 114 126  53 114  96  25 109   7  31 141
     46  13  27  43 117 116  27   7  68  40  31
    115 124  42 128  52  71 118 117  38  27 106
     33 117 116 111  40 119  47 105  57 122 109
    124 115  43 120  43  27  27  18  28  48 125
    107 114  34 133  45 120  30 127  31 116 146
>».Int.sort;

my Int $stem_unit = 10;
my %h = @data.classify: * div $stem_unit;

my $range = [minmax] %h.keys».Int;
my $stem_format =  "%{$range.min.chars max $range.max.chars}d";

for $range.list -> $stem {
    my $leafs = %h{$stem} // [];
    say $stem.fmt($stem_format), ' | ', ~$leafs.map: * % $stem_unit;
}
```


Output:
```txt
 0 | 7 7
 1 | 2 3 8 8
 2 | 3 5 7 7 7 7 7 7 8 8 9 9
 3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 | 2 3 7 8 8
 6 | 1 3 8
 7 | 1
 8 |
 9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6
```



## Phix

Copy of [[Stem-and-leaf_plot#Euphoria|Euphoria]]

```Phix
procedure leaf_plot(sequence s)
sequence stem
    s = sort(s)
    stem = repeat({},floor(s[$]/10)+1)
    for i=1 to length(s) do
        stem[floor(s[i]/10)+1] &= remainder(s[i],10)
    end for
    for i=1 to length(stem) do
        printf(1, "%3d | ", i-1)
        for j=1 to length(stem[i]) do
            printf(1, "%d ", stem[i][j])
        end for
        puts(1,'\n')
    end for
end procedure

constant data = { 12, 127, 28, 42, 39, 113, 42, 18, 44, 118, 44, 37, 113, 124,
    37, 48, 127, 36, 29, 31, 125, 139, 131, 115, 105, 132, 104, 123, 35, 113,
    122, 42, 117, 119, 58, 109, 23, 105, 63, 27, 44, 105, 99, 41, 128, 121,
    116, 125, 32, 61, 37, 127, 29, 113, 121, 58, 114, 126, 53, 114, 96, 25,
    109, 7, 31, 141, 46, 13, 27, 43, 117, 116, 27, 7, 68, 40, 31, 115, 124,
    42, 128, 52, 71, 118, 117, 38, 27, 106, 33, 117, 116, 111, 40, 119, 47,
    105, 57, 122, 109, 124, 115, 43, 120, 43, 27, 27, 18, 28, 48, 125, 107,
    114, 34, 133, 45, 120, 30, 127, 31, 116, 146 }

leaf_plot(data)
```

<pre style="font-size: 8px">
  0 | 7 7
  1 | 2 3 8 8
  2 | 3 5 7 7 7 7 7 7 8 8 9 9
  3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 | 2 3 7 8 8
  6 | 1 3 8
  7 | 1
  8 |
  9 | 6 9
 10 | 4 5 5 5 5 6 7 9 9 9
 11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 | 1 2 3 9
 14 | 1 6

```



## PicoLisp


```PicoLisp
(de *Data
   12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36
   29 31 125 139 131 115 105 132 104 123 35 113 122 42 117 119
   58 109 23 105 63 27 44 105 99 41 128 121 116 125 32 61 37 127
   29 113 121 58 114 126 53 114 96 25 109 7 31 141 46 13 27 43
   117 116 27 7 68 40 31 115 124 42 128 52 71 118 117 38 27 106
   33 117 116 111 40 119 47 105 57 122 109 124 115 43 120 43 27
   27 18 28 48 125 107 114 34 133 45 120 30 127 31 116 146 )

(let L
   (group
      (mapcar
         '((N)
            (cons
               (or (format (head -1 (setq N (chop N)))) 0)
               (last N) ) )
         (sort *Data) ) )
   (for I (range (caar L) (car (last L)))
      (prinl (align 3 I) " | " (glue " " (cdr (assoc I L)))) ) )
```

Output:

```txt
  0 | 7 7
  1 | 2 3 8 8
  2 | 3 5 7 7 7 7 7 7 8 8 9 9
  3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 | 2 3 7 8 8
  6 | 1 3 8
  7 | 1
  8 |
  9 | 6 9
 10 | 4 5 5 5 5 6 7 9 9 9
 11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 | 1 2 3 9
 14 | 1 6
```



## PowerShell


```powershell

$Set = -split '12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36 29 31 125 139 131 115 105 132 104 123 35 113 122 42 117 119 58 109 23 105 63 27 44 105 99 41 128 121 116 125 32 61 37 127 29 113 121 58 114 126 53 114 96 25 109 7 31 141 46 13 27 43 117 116 27 7 68 40 31 115 124 42 128 52 71 118 117 38 27 106 33 117 116 111 40 119 47 105 57 122 109 124 115 43 120 43 27 27 18 28 48 125 107 114 34 133 45 120 30 127 31 116 146'

$Data = $Set | Select @{ Label = 'Stem'; Expression = { [string][int]$_.Substring( 0, $_.Length - 1 ) } }, @{ Label = 'Leaf'; Expression = { [string]$_[-1] } }

$StemStats = $Data | Measure-Object -Property Stem -Minimum -Maximum

ForEach ( $Stem in $StemStats.Minimum..$StemStats.Maximum )
    {
    @( $Stem.ToString().PadLeft( 2, " " ), '|' ) + ( ( $Data | Where Stem -eq $Stem ).Leaf | Sort ) -join " "
    }

```

```txt

 0 | 7 7
 1 | 2 3 8 8
 2 | 3 5 7 7 7 7 7 7 8 8 9 9
 3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 | 2 3 7 8 8
 6 | 1 3 8
 7 | 1
 8 |
 9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6

```



## PureBasic

'''PureBasic Code'''

```PureBasic
If OpenConsole()
  Dim MyList(120)
  Define i, j, StemMax, StemMin
  Restore MyData          ; Get the address of MyData, e.g. the data to print as a Stem-and-leaf plot
  For a=0 To 120
    Read.i MyList(a)      ; Read the data into the used Array
    If MyList(a)>StemMax
      StemMax=MyList(a)   ; Find the largest Stem layer at the same time
    EndIf
    If MyList(a)<StemMin
      StemMin=MyList(a)   ; Find the smallest Stem layer at the same time
    EndIf
  Next
  StemMax/10: StemMin/10  ; Remove the leafs from the Stem limits
  SortArray(MyList(),#PB_Sort_Ascending)  ; Sort the data

  For i=StemMin To StemMax
    Print(RSet(Str(i),3)+" | ")           ; Print the Stem
    For j=0 To 120
      If MyList(j)<10*i                   ; Skip all smaller then current
        Continue
      ElseIf MyList(j)>=10*(i+1)          ; Break current print if a new Stem layer is reached
        Break
      Else
        Print(Str(MyList(j)%10)+" ")      ; Print all Leafs on this current Stem layer
      EndIf
    Next j
    PrintN("")
  Next i

  Print(#CRLF$+#CRLF$+"Press ENTER to exit")
  Input()
  CloseConsole()
EndIf

DataSection
MyData:
  Data.i  12,127, 28, 42, 39,113, 42, 18, 44,118, 44, 37,113,124, 37, 48,127, 36, 29, 31,125,139,131,115
  Data.i 105,132,104,123, 35,113,122, 42,117,119, 58,109, 23,105, 63, 27, 44,105, 99, 41,128,121,116,125
  Data.i  32, 61, 37,127, 29,113,121, 58,114,126, 53,114, 96, 25,109,  7, 31,141, 46, 13, 27, 43,117,116
  Data.i  27,  7, 68, 40, 31,115,124, 42,128, 52, 71,118,117, 38, 27,106, 33,117,116,111, 40,119, 47,105
  Data.i  57,122,109,124,115, 43,120, 43, 27, 27, 18, 28, 48,125,107,114, 34,133, 45,120, 30,127, 31,116,146
EndDataSection
```


'''Output'''
  0 | 7 7
  1 | 2 3 8 8
  2 | 3 5 7 7 7 7 7 7 8 8 9 9
  3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 | 2 3 7 8 8
  6 | 1 3 8
  7 | 1
  8 |
  9 | 6 9
 10 | 4 5 5 5 5 6 7 9 9 9
 11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 | 1 2 3 9
 14 | 1 6



## Python


Adjusting <code>Stem.leafdigits</code> allows you to modify how many digits of a value are used in the leaf, with the stem intervals adjusted accordingly.

```python
from collections import namedtuple
from pprint import pprint as pp
from math import floor

Stem = namedtuple('Stem', 'data, leafdigits')

data0 = Stem((12, 127, 28, 42, 39, 113, 42, 18, 44, 118, 44, 37, 113, 124, 37,
              48, 127, 36, 29, 31, 125, 139, 131, 115, 105, 132, 104, 123, 35,
              113, 122, 42, 117, 119, 58, 109, 23, 105, 63, 27, 44, 105, 99,
              41, 128, 121, 116, 125, 32, 61, 37, 127, 29, 113, 121, 58, 114,
              126, 53, 114, 96, 25, 109, 7, 31, 141, 46, 13, 27, 43, 117, 116,
              27, 7, 68, 40, 31, 115, 124, 42, 128, 52, 71, 118, 117, 38, 27,
              106, 33, 117, 116, 111, 40, 119, 47, 105, 57, 122, 109, 124, 115,
              43, 120, 43, 27, 27, 18, 28, 48, 125, 107, 114, 34, 133, 45, 120,
              30, 127, 31, 116, 146),
             1.0)

def stemplot(stem):
    d = []
    interval = int(10**int(stem.leafdigits))
    for data in sorted(stem.data):
        data = int(floor(data))
        stm, lf = divmod(data,interval)
        d.append( (int(stm), int(lf)) )
    stems, leafs = list(zip(*d))
    stemwidth = max(len(str(x)) for x in stems)
    leafwidth = max(len(str(x)) for x in leafs)
    laststem, out = min(stems) - 1, []
    for s,l in d:
        while laststem < s:
            laststem += 1
            out.append('\n%*i |' % ( stemwidth, laststem))
        out.append(' %0*i' % (leafwidth, l))
    out.append('\n\nKey:\n Stem multiplier: %i\n X | Y  =>  %i*X+Y\n'
               % (interval, interval))
    return ''.join(out)

if __name__ == '__main__':
    print( stemplot(data0) )
```


'''Sample Output'''

```txt
 0 | 7 7
 1 | 2 3 8 8
 2 | 3 5 7 7 7 7 7 7 8 8 9 9
 3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 | 2 3 7 8 8
 6 | 1 3 8
 7 | 1
 8 |
 9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6

Key:
 Stem multiplier: 10
 X | Y  =>  10*X+Y
```



Here is an another example using an OrderedDict and Counter

```python
from collections import OrderedDict, Counter

x= [12, 127, 28, 42, 39, 113, 42, 18, 44, 118, 44, 37, 113, 124, 37, 48,
    127, 36, 29, 31, 125, 139, 131, 115, 105, 132, 104, 123, 35, 113,
    122, 42, 117, 119, 58, 109, 23, 105, 63, 27, 44, 105, 99, 41, 128,
    121, 116, 125, 32, 61, 37, 127, 29, 113, 121, 58, 114, 126, 53, 114,
    96, 25, 109, 7, 31, 141, 46, 13, 27, 43, 117, 116, 27, 7, 68, 40, 31,
    115, 124, 42, 128, 52, 71, 118, 117, 38, 27, 106, 33, 117, 116, 111,
    40, 119, 47, 105, 57, 122, 109, 124, 115, 43, 120, 43, 27, 27, 18,
    28, 48, 125, 107, 114, 34, 133, 45, 120, 30, 127, 31, 116, 146]

def stemleaf(x):
    d = OrderedDict((((str(v)[:-1],' ')[v<10], Counter()) for v in sorted(x)))
    for s in ((str(v),' '+str(v))[v<10] for v in x) : d[s[:-1]][s[-1]]+=1
    m=max(len(s) for s in d)
    for k in d:
        print('%s%s | %s'%(' '*(m-len(k)),k,' '.join(sorted(d[k].elements()))))

stemleaf(x)

```


Output :


```txt

   | 7 7
 1 | 2 3 8 8
 2 | 3 5 7 7 7 7 7 7 8 8 9 9
 3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 | 2 3 7 8 8
 6 | 1 3 8
 7 | 1
 9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6

```



Or, generalising a little to write a purely declarative function (in terms of '''groupby''' and '''reduce''') which takes stem and leaf accessor functions as its first arguments:

```python
from itertools import (groupby)
from functools import (reduce)


# stemLeaf :: (String -> Int) -> (String -> String) -> String -> String
def stemLeaf(f, g, s):
    return '\n'.join(map(
        lambda x: str(x[0]).rjust(2) + ' | ' +
        reduce(lambda a, tpl: a + tpl[1] + ' ', x[1], ''),
        (groupby(sorted(
            map(lambda x: (f(x), g(x)), s.split())
        ),
            lambda x: x[0]
        ))
    ))


# main :: IO()
def main():
    def stem(s):
        return (lambda x=s[:-1]: int(x) if 0 < len(x) else 0)()

    def leaf(s):
        return s[-1]

    s = ('12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36 29 31'
         ' 125 139 131 115 105 132 104 123 35 113 122 42 117 119 58 109 23'
         ' 105 63 27 44 105 99 41 128 121 116 125 32 61 37 127 29 113 121 58'
         ' 114 126 53 114 96 25 109 7 31 141 46 13 27 43 117 116 27 7 68 40'
         ' 31 115 124 42 128 52 71 118 117 38 27 106 33 117 116 111 40 119 47'
         ' 105 57 122 109 124 115 43 120 43 27 27 18 28 48 125 107 114 34 133'
         ' 45 120 30 127 31 116 146')

    print (stemLeaf(stem, leaf, s))


main()
```


```txt
 0 | 7 7
 1 | 2 3 8 8
 2 | 3 5 7 7 7 7 7 7 8 8 9 9
 3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 | 2 3 7 8 8
 6 | 1 3 8
 7 | 1
 9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6
```



## R



```R

x <- c(12, 127, 28, 42, 39, 113, 42, 18, 44, 118, 44, 37, 113, 124, 37, 48, 127, 36,
29, 31, 125, 139, 131, 115, 105, 132, 104, 123, 35, 113, 122, 42, 117, 119, 58, 109,
23, 105, 63, 27, 44, 105, 99, 41, 128, 121, 116, 125, 32, 61, 37, 127, 29, 113,
121, 58, 114, 126, 53, 114, 96, 25, 109, 7, 31, 141, 46, 13, 27, 43, 117, 116,
27, 7, 68, 40, 31, 115, 124, 42, 128, 52, 71, 118, 117, 38, 27, 106, 33, 117,
116, 111, 40, 119, 47, 105, 57, 122, 109, 124, 115, 43, 120, 43, 27, 27, 18, 28,
48, 125, 107, 114, 34, 133, 45, 120, 30, 127, 31, 116, 146)

stem(x)

```


Output :


```txt

   0 | 77
   1 | 2388
   2 | 357777778899
   3 | 011112345677789
   4 | 001222233344456788
   5 | 23788
   6 | 138
   7 | 1
   8 |
   9 | 69
  10 | 4555567999
  11 | 13333444555666677778899
  12 | 00112234445556777788
  13 | 1239
  14 | 16

```



## Racket


```Racket

#lang racket
(define (show-stem+leaf data)
  (define xs (sort data <))
  (for ([stem (add1 (floor (/ (last xs) 10)))])
    (printf "~a|" (~a #:width 2 #:align 'right stem))
    (for ([i xs])
      (define-values [q r] (quotient/remainder i 10))
      (when (= q stem) (printf " ~a" r)))
    (newline)))
(show-stem+leaf (sequence->list (in-producer read eof)))

```


Sample run:

```txt

$ racket sl.rkt < the-data
 0| 7 7
 1| 2 3 8 8
 2| 3 5 7 7 7 7 7 7 8 8 9 9
 3| 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4| 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5| 2 3 7 8 8
 6| 1 3 8
 7| 1
 8|
 9| 6 9
10| 4 5 5 5 5 6 7 9 9 9
11| 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12| 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13| 1 2 3 9
14| 1 6

```



## REXX


### zero and positive numbers

A check is performed to verify that all input is numeric (decimal fractions are allowed as well as exponential format).

Also, a check is made if any of the numbers are negative (and an error message is issued).   Negative numbers are handled by the 2<sup>nd</sup> REXX version.

Also, all numbers that are processed are normalized.   Using a   ''sparse array''   bypasses the need for sorting.

```rexx
/*REXX program displays a stem and leaf plot of any non-negative numbers [can include 0]*/
parse arg @                                      /* [↓]  Not specified? Then use default*/
if @=''  then @=12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36 29 31 125 139,
   131 115 105 132 104 123 35 113 122 42 117 119 58 109 23 105 63 27 44 105 99 41 128 121,
   116 125  32  61 37 127 29 113 121 58 114 126 53 114 96 25 109 7 31 141 46 13 27 43 117,
   116  27 7 68 40 31 115 124 42 128 52 71 118 117 38 27 106 33 117 116 111 40 119 47 105,
   57 122 109 124 115 43 120 43 27 27 18 28 48 125 107 114 34 133 45 120 30 127 31 116 146
#.=;                            bot=.;    top=.  /* [↑]  define all #. elements as null.*/
     do j=1  for words(@);     y=word(@, j)      /*◄─── process each number in the list.*/
     if \datatype(y,"N")  then do; say '***error*** item' j "isn't numeric:" y;  exit; end
     if y<0               then do; say '***error*** item' j "is negative:"   y;  exit; end
     n=format(y, , 0) / 1                        /*normalize the numbers (not malformed)*/
     stem=word(left(n, length(n) -1)  0, 1)      /*obtain stem (1st digits) from number.*/
     parse var  n '' -1 leaf;   _=stem * sign(n) /*   "   leaf (last digit)   "    "    */
     if bot==.  then do;  bot=_;  top=_;  end    /*handle the first case for TOP and BOT*/
     bot=min(bot, _);           top=max(top, _)  /*obtain the minimum and maximum so far*/
     #.stem.leaf= #.stem.leaf   leaf             /*construct sorted stem-and-leaf entry.*/
     end   /*j*/

w=max(length(min), length(max) )    + 1          /*W:  used to right justify the output.*/
                                                 /* [↓]  display the stem-and-leaf plot.*/
     do k=bot  to top;          $=               /*$:  is the output string, a plot line*/
        do m=0  for 10;         $=$  #.k.m       /*build a line for the stem─&─leaf plot*/
        end  /*m*/
     say right(k, w)    '║'     space($)         /*display a line of stem─and─leaf plot.*/
     end   /*k*/                                 /*stick a fork in it,  we're all done. */
```

'''output'''   when using the (internal) defaults as input:

```txt

  0 ║ 7 7
  1 ║ 2 3 8 8
  2 ║ 3 5 7 7 7 7 7 7 8 8 9 9
  3 ║ 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 ║ 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 ║ 2 3 7 8 8
  6 ║ 1 3 8
  7 ║ 1
  8 ║
  9 ║ 6 9
 10 ║ 4 5 5 5 5 6 7 9 9 9
 11 ║ 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 ║ 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 ║ 1 2 3 9
 14 ║ 1 6

```


===negative, zero, and positive numbers===
This REXX version also handles negative numbers.

```rexx
/*REXX program displays a stem─and─leaf plot of any real numbers [can be:  neg, 0, pos].*/
parse arg @                                      /*obtain optional arguments from the CL*/
if @=''  then @='15 14 3 2 1 0 -1 -2 -3 -14 -15' /*Not specified?  Then use the default.*/
#.=;                  bot=.;    top=.;      z=.  /* [↑]  define all #. elements as null.*/
      do j=1  for words(@);     y=word(@, j)     /*◄─── process each number in the list.*/
      if \datatype(y,"N")  then do; say '***error*** item' j "isn't numeric:" y; exit; end
      n=format(y,,0)/1;  an=abs(n); s=sign(n)    /*normalize the numbers (not malformed)*/
      stem=left(an, length(an) -1)
      if stem==''  then if s>=0  then stem=0     /*handle case of one-digit positive #. */
                                 else stem='-0'  /*   "     "   "  "    "   negative "  */
                   else stem=s * stem            /*   "     "   " a  multi-digit number.*/
      parse var  n '' -1 leaf                    /*obtain the leaf (the last digit) of #*/
      if bot==.  then do; bot=stem; top=bot; end /*handle the first case for TOP and BOT*/
      bot=min(bot, stem);     top=max(top, stem) /*obtain the minimum and maximum so far*/
      if stem=='-0'  then z=0                    /*use  Z  as a flag to show negative 0.*/
      #.stem.leaf= #.stem.leaf  leaf             /*construct sorted stem-and-leaf entry.*/
      end   /*j*/

w=max(length(min), length(max) )  + 1            /*W:  used to right─justify the output.*/
!='-0'                                           /* [↓]  display the stem-and-leaf plot.*/
      do k=bot  to top;           $=             /*$:  is the output string, a plot line*/
      if k==z  then do                           /*handle a special case for negative 0.*/
                       do s=0  for 10; $=$ #.!.s /*build a line for the stem─&─leaf plot*/
                       end  /*s*/                /* [↑]  address special case of  -zero.*/
                    say right(!, w) '║' space($) /*display a line of stem─and─leaf plot.*/
                    end                          /* [↑]  handles special case of  -zero.*/
      $=                                         /*a new plot line  (of output).        */
                    do m=0  for 10;  $=$  #.k.m  /*build a line for the stem─&─leaf plot*/
                    end  /*m*/
      say right(k, w)    '║'    space($)         /*display a line of stem─and─leaf plot.*/
      end   /*k*/                                /*stick a fork in it,  we're all done. */
```

'''output'''   when using the (internal) defaults as input:

```txt

  -1 ║ 4 5
  -0 ║ 1 2 3
   0 ║ 0 1 2 3
   1 ║ 4 5

```



## Ring


```ring

# Project : Stem-and-leaf plot

data = list(120)
data = [12, 127,  28,  42,  39, 113,  42,  18,  44, 118,  44,  37, 113, 124,
            37,  48, 127,  36,  29,  31, 125, 139, 131, 115, 105, 132, 104, 123,
            35, 113, 122,  42, 117, 119,  58, 109,  23, 105,  63,  27,  44, 105,
            99,  41, 128, 121, 116, 125,  32,  61,  37, 127,  29, 113, 121,  58,
            114, 126,  53, 114,  96,  25, 109,   7,  31, 141,  46,  13,  27,  43,
            117, 116,  27,   7,  68,  40,  31, 115, 124,  42, 128,  52,  71, 118,
            117,  38,  27, 106,  33, 117, 116, 111,  40, 119,  47, 105,  57, 122,
            109, 124, 115,  43, 120,  43,  27,  27,  18,  28,  48, 125, 107, 114,
            34, 133,  45, 120,  30, 127,  31, 116, 146]

leafplot(data, len(data))

func leafplot(x,n)
       c = n
       x = sort(x)
       i = floor(x[1] / 10 ) - 1
       for j = 1 to n
            d = floor(x[j] / 10)
            while d > i
                     i = i + 1
                     if j > 0
                        see nl
                     ok
                     see "" + i + " |"
            end
            see "" + (x[j] % 10) + " "
       next
       see nl

```

Output:

```txt


0 |7 7
1 |2 3 8 8
2 |3 5 7 7 7 7 7 7 8 8 9 9
3 |0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
4 |0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
5 |2 3 7 8 8
6 |1 3 8
7 |1
8 |
9 |6 9
10 |4 5 5 5 5 6 7 9 9 9
11 |1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 |0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 |1 2 3 9
14 |1 6

```



## Ruby

This implementation will handle negative values.

```ruby
class StemLeafPlot
  def initialize(data, options = {})
    opts = {:leaf_digits => 1}.merge(options)
    @leaf_digits = opts[:leaf_digits]
    @multiplier = 10 ** @leaf_digits
    @plot = generate_structure(data)
  end

  private

  def generate_structure(data)
    plot = Hash.new {|h,k| h[k] = []}
    data.sort.each do |value|
      stem, leaf = parse(value)
      plot[stem] << leaf
    end
    plot
  end

  def parse(value)
    stem, leaf = value.abs.divmod(@multiplier)
    [Stem.get(stem, value), leaf.round]
  end

  public

  def print
    stem_width = Math.log10(@plot.keys.max_by {|s| s.value}.value).ceil + 1
    Stem.get_range(@plot.keys).each do |stem|
      leaves = @plot[stem].inject("") {|str,leaf| str << "%*d " % [@leaf_digits, leaf]}
      puts "%*s | %s" % [stem_width, stem, leaves]
    end

    puts "key: 5|4=#{5 * @multiplier + 4}"
    puts "leaf unit: 1"
    puts "stem unit: #@multiplier"
  end
end

class Stem
  @@cache = {}

  def self.get(stem_value, datum)
    sign = datum < 0 ? :- : :+
    cache(stem_value, sign)
  end

  private

  def self.cache(value, sign)
    if @@cache[[value, sign]].nil?
      @@cache[[value, sign]] = self.new(value, sign)
    end
    @@cache[[value, sign]]
  end

  def initialize(value, sign)
    @value = value
    @sign = sign
  end

  public

  attr_accessor :value, :sign

  def negative?
    @sign == :-
  end

  def <=>(other)
    if self.negative?
      if other.negative?
        other.value <=> self.value
      else
        -1
      end
    else
      if other.negative?
        1
      else
        self.value <=> other.value
      end
    end
  end

  def to_s
    "%s%d" % [(self.negative? ? '-' : ' '), @value]
  end

  def self.get_range(array_of_stems)
    min, max = array_of_stems.minmax
    if min.negative?
      if max.negative?
        min.value.downto(max.value).collect {|n| cache(n, :-)}
      else
        min.value.downto(0).collect {|n| cache(n, :-)} + 0.upto(max.value).collect {|n| cache(n, :+)}
      end
    else
      min.value.upto(max.value).collect {|n| cache(n, :+)}
    end
  end

end

data = DATA.read.split.map {|s| Float(s)}
StemLeafPlot.new(data).print

__END__
12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36 29 31 125 139 131
115 105 132 104 123 35 113 122 42 117 119 58 109 23 105 63 27 44 105 99 41 128
121 116 125 32 61 37 127 29 113 121 58 114 126 53 114 96 25 109 7 31 141 46 13
27 43 117 116 27 7 68 40 31 115 124 42 128 52 71 118 117 38 27 106 33 117 116
111 40 119 47 105 57 122 109 124 115 43 120 43 27 27 18 28 48 125 107 114 34
133 45 120 30 127 31 116 146
```


```txt

  0 | 7 7
  1 | 2 3 8 8
  2 | 3 5 7 7 7 7 7 7 8 8 9 9
  3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 | 2 3 7 8 8
  6 | 1 3 8
  7 | 1
  8 |
  9 | 6 9
 10 | 4 5 5 5 5 6 7 9 9 9
 11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 | 1 2 3 9
 14 | 1 6
key: 5|4=54
leaf unit: 1
stem unit: 10
```


'''Simple version'''

```ruby
class StemLeafPlot
  def initialize(data, leaf_digits=1)
    @leaf_digits = leaf_digits
    multiplier = 10 ** @leaf_digits
    @plot = data.sort.group_by{|x| x / multiplier}
    @plot.default = []
    @plot.each{|k,v| @plot[k] = v.map{|val| val % multiplier}}
  end

  def print
    min, max = @plot.keys.minmax
    stem_width = max.to_s.size
    (min..max).each do |stem|
      leaves = @plot[stem].inject("") {|str,leaf| str << "%0*d " % [@leaf_digits, leaf]}
      puts "%*s | %s" % [stem_width, stem, leaves]
    end
  end
end

data = DATA.read.split.map {|s| Integer(s)}
StemLeafPlot.new(data).print

__END__
12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36 29 31 125 139 131
115 105 132 104 123 35 113 122 42 117 119 58 109 23 105 63 27 44 105 99 41 128
121 116 125 32 61 37 127 29 113 121 58 114 126 53 114 96 25 109 7 31 141 46 13
27 43 117 116 27 7 68 40 31 115 124 42 128 52 71 118 117 38 27 106 33 117 116
111 40 119 47 105 57 122 109 124 115 43 120 43 27 27 18 28 48 125 107 114 34
133 45 120 30 127 31 116 146
```

```txt

 0 | 7 7
 1 | 2 3 8 8
 2 | 3 5 7 7 7 7 7 7 8 8 9 9
 3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 | 2 3 7 8 8
 6 | 1 3 8
 7 | 1
 8 |
 9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6

```



## Scala

```scala
def stemAndLeaf(numbers: List[Int]) = {
  val lineFormat = "%" + (numbers map (_.toString.length) max) + "d | %s"
  val map = numbers groupBy (_ / 10)
  for (stem <- numbers.min / 10 to numbers.max / 10) {
    println(lineFormat format (stem, map.getOrElse(stem, Nil) map (_ % 10) sortBy identity mkString " "))
  }
}
```


Example:

<pre style="height:40ex;overflow:scroll">
scala> val list = """12  127 28  42  39  113 42  18  44  118 44  37  113 124 37  48  127 36  29  31  125 139 131 115 105
 132 104 123 35  113 122 42  117 119 58  109 23  105 63  27  44  105 99  41  128 121 116 125 32  61  37  127 29  113 121
 58  114 126 53  114 96  25  109 7   31  141 46  13  27  43  117 116 27  7   68  40  31  115 124 42  128 52  71  118 117
 38  27  106 33  117 116 111 40  119 47  105 57  122 109 124 115 43  120 43  27  27  18  28  48  125 107 114 34  133 45
 120 30  127 31  116
     | 146""" split "\\s+" map (_.toInt) toList
list: List[Int] = List(12, 127, 28, 42, 39, 113, 42, 18, 44, 118, 44, 37, 113, 124, 37, 48, 127, 36, 29, 31, 125, 139, 1
31, 115, 105, 132, 104, 123, 35, 113, 122, 42, 117, 119, 58, 109, 23, 105, 63, 27, 44, 105, 99, 41, 128, 121, 116, 125,
32, 61, 37, 127, 29, 113, 121, 58, 114, 126, 53, 114, 96, 25, 109, 7, 31, 141, 46, 13, 27, 43, 117, 116, 27, 7, 68, 40,
31, 115, 124, 42, 128, 52, 71, 118, 117, 38, 27, 106, 33, 117, 116, 111, 40, 119, 47, 105, 57, 122, 109, 124, 115, 43, 1
20, 43, 27, 27, 18, 28, 48, 125, 107, 114, 34, 133, 45, 120, 30, 127, 31, 116, 146)

scala> stemAndLeaf(list)
  0 | 7 7
  1 | 2 3 8 8
  2 | 3 5 7 7 7 7 7 7 8 8 9 9
  3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 | 2 3 7 8 8
  6 | 1 3 8
  7 | 1
  8 |
  9 | 6 9
 10 | 4 5 5 5 5 6 7 9 9 9
 11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 | 1 2 3 9
 14 | 1 6

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: leafPlot (in var array integer: x) is func
  local
    var integer: i is 0;
    var integer: j is 0;
    var integer: d is 0;
  begin
    x := sort(x);
    i := x[1] div 10 - 1;
    for key j range x do
      d := x[j] div 10;
      while d > i do
        if j <> 1 then
          writeln;
        end if;
        incr(i);
        write(i lpad 3 <& " |");
      end while;
      write(" " <& x[j] rem 10);
    end for;
    writeln;
  end func;

const proc: main is func
  local
    const array integer: data is [] (
       12, 127,  28,  42,  39, 113,  42,  18,  44, 118,  44,  37, 113, 124,  37,  48, 127,  36,
       29,  31, 125, 139, 131, 115, 105, 132, 104, 123,  35, 113, 122,  42, 117, 119,  58, 109,
       23, 105,  63,  27,  44, 105,  99,  41, 128, 121, 116, 125,  32,  61,  37, 127,  29, 113,
      121,  58, 114, 126,  53, 114,  96,  25, 109,   7,  31, 141,  46,  13,  27,  43, 117, 116,
       27,   7,  68,  40,  31, 115, 124,  42, 128,  52,  71, 118, 117,  38,  27, 106,  33, 117,
      116, 111,  40, 119,  47, 105,  57, 122, 109, 124, 115,  43, 120,  43,  27,  27,  18,  28,
       48, 125, 107, 114,  34, 133,  45, 120,  30, 127,  31, 116, 146);
  begin
    leafPlot(data);
  end func;
```


Output:

```txt

  0 | 7 7
  1 | 2 3 8 8
  2 | 3 5 7 7 7 7 7 7 8 8 9 9
  3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 | 2 3 7 8 8
  6 | 1 3 8
  7 | 1
  8 |
  9 | 6 9
 10 | 4 5 5 5 5 6 7 9 9 9
 11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 | 1 2 3 9
 14 | 1 6

```



## Sidef

```ruby
var data = %i(
     12 127  28  42  39 113  42  18  44 118  44
     37 113 124  37  48 127  36  29  31 125 139
    131 115 105 132 104 123  35 113 122  42 117
    119  58 109  23 105  63  27  44 105  99  41
    128 121 116 125  32  61  37 127  29 113 121
     58 114 126  53 114  96  25 109   7  31 141
     46  13  27  43 117 116  27   7  68  40  31
    115 124  42 128  52  71 118 117  38  27 106
     33 117 116 111  40 119  47 105  57 122 109
    124 115  43 120  43  27  27  18  28  48 125
    107 114  34 133  45 120  30 127  31 116 146
).sort;

var stem_unit = 10;
var h = data.group_by { |i| i / stem_unit -> int }

var rng = RangeNum(h.keys.map{.to_i}.minmax);
var stem_format =  "%#{rng.min.len.max(rng.max.len)}d";

rng.each { |stem|
    var leafs = (h{stem} \\ [])
    say(stem_format % stem, ' | ', leafs.map { _ % stem_unit }.join(' '))
}
```

```txt

 0 | 7 7
 1 | 2 3 8 8
 2 | 3 5 7 7 7 7 7 7 8 8 9 9
 3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 | 2 3 7 8 8
 6 | 1 3 8
 7 | 1
 8 |
 9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6

```



## Stata



```stata
. clear all
. input x
12
127
28
...
31
116
146
end

. stem x

Stem-and-leaf plot for x

   0* | 77
   1* | 2388
   2* | 357777778899
   3* | 011112345677789
   4* | 001222233344456788
   5* | 23788
   6* | 138
   7* | 1
   8* |
   9* | 69
  10* | 4555567999
  11* | 13333444555666677778899
  12* | 00112234445556777788
  13* | 1239
  14* | 16
```



## Tcl

```tcl
package require Tcl 8.5

# How to process a single value, adding it to the table mapping stems to
# leaves.
proc addSLValue {tblName value {splitFactor 10}} {
    upvar 1 $tblName tbl
    # Extract the stem and leaf
    if {$value < 0} {
	set value [expr {round(-$value)}]
	set stem -[expr {$value / $splitFactor}]
    } else {
	set value [expr {round($value)}]
	set stem [expr {$value / $splitFactor}]
    }
    if {![info exist tbl]} {
	dict set tbl min $stem
    }
    dict set tbl max $stem
    set leaf [expr {$value % $splitFactor}]
    dict lappend tbl $stem $leaf
}

# How to do the actual output of the stem-and-leaf table, given that we have
# already done the splitting into stems and leaves.
proc printSLTable {tblName} {
    upvar 1 $tblName tbl
    # Get the range of stems
    set min [dict get $tbl min]
    set max [dict get $tbl max]
    # Work out how much width the stems take so everything lines up
    set l [expr {max([string length $min], [string length $max])}]
    # Print out the table
    for {set i $min} {$i <= $max} {incr i} {
	if {![dict exist $tbl $i]} {
	    puts [format " %*d |"    $l $i]
	} else {
	    puts [format " %*d | %s" $l $i [dict get $tbl $i]]
	}
    }
}

# Assemble the parts into a full stem-and-leaf table printer.
proc printStemLeaf {dataList {splitFactor 10}} {
    foreach value [lsort -real $dataList] {
	addSLValue tbl $value $splitFactor
    }
    printSLTable tbl
}

# Demo code
set data {
    12  127 28  42  39  113 42  18  44  118 44  37  113 124 37  48  127 36
    29  31  125 139 131 115 105 132 104 123 35  113 122 42  117 119 58  109
    23  105 63  27  44  105 99  41  128 121 116 125 32  61  37  127 29  113
    121 58  114 126 53  114 96  25  109 7   31  141 46  13  27  43  117 116
    27  7   68  40  31  115 124 42  128 52  71  118 117 38  27  106 33  117
    116 111 40  119 47  105 57  122 109 124 115 43  120 43  27  27  18  28
    48  125 107 114 34  133 45  120 30  127 31  116 146
}
printStemLeaf $data
```

Output:

```txt

  0 | 7 7
  1 | 2 3 8 8
  2 | 3 5 7 7 7 7 7 7 8 8 9 9
  3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 | 2 3 7 8 8
  6 | 1 3 8
  7 | 1
  8 |
  9 | 6 9
 10 | 4 5 5 5 5 6 7 9 9 9
 11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 | 1 2 3 9
 14 | 1 6

```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
digits=*
DATA 12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36 29 31 125 139 131 115 105 132 104 123 35 113
DATA 122 42 117 119 58 109 23 105 63 27 44 105 99 41 128 121 116 125 32 61 37 127 29 113 121 58 114 126 53 114
DATA 96 25 109 7 31 141 46 13 27 43 117 116 27 7 68 40 31 115 124 42 128 52 71 118 117 38 27 106 33 117 116 111
DATA 40 119 47 105 57 122 109 124 115 43 120 43 27 27 18 28 48 125 107 114 34 133 45 120 30 127 31 116 146

digits=SPLIT (digits,": :"), digitssort=DIGIT_SORT (digits)

SECTION format
formatstem=CENTER (currentstem,5," ")
PRINT formatstem, leaves
ENDSECTION

leaves="",currentstem=0
LOOP d=digitssort
leaf=mod(d,10),stem=d/10
IF (stem!=currentstem) THEN
 DO format
 IF (stem!=nextstem) THEN
  currentstem=nextstem=nextstem+1,leaves=""
  DO format
 ENDIF
leaves=leaf, currentstem=stem
ELSE
 leaves=APPEND (leaves,leaf), nextstem=stem+1
ENDIF
ENDLOOP
DO format

```

Output:
<pre style='height:30ex;overflow:scroll'>
  0  7'7
  1  2'3'8'8
  2  3'5'7'7'7'7'7'7'8'8'9'9
  3  0'1'1'1'1'2'3'4'5'6'7'7'7'8'9
  4  0'0'1'2'2'2'2'3'3'3'4'4'4'5'6'7'8'8
  5  2'3'7'8'8
  6  1'3'8
  7  1
  8
  9  6'9
 10  4'5'5'5'5'6'7'9'9'9
 11  1'3'3'3'3'4'4'4'5'5'5'6'6'6'6'7'7'7'7'8'8'9'9
 12  0'0'1'1'2'2'3'4'4'4'5'5'5'6'7'7'7'7'8'8
 13  1'2'3'9
 14  1'6

```



## uBasic/4tH

<lang>Push  12, 127,  28,  42,  39, 113,  42,  18,  44, 118,  44,  37, 113, 124
Push  0, 13 : Gosub _Read              ' read 1st line of data

Push  37,  48, 127,  36,  29,  31, 125, 139, 131, 115, 105, 132, 104, 123
Push  14, 27 : Gosub _Read             ' read 2nd line of data

Push  35, 113, 122,  42, 117, 119,  58, 109,  23, 105,  63,  27,  44, 105
Push  28, 41 : Gosub _Read             ' read 3rd line of data

Push  99,  41, 128, 121, 116, 125,  32,  61,  37, 127,  29, 113, 121,  58
Push  42, 55 : Gosub _Read             ' read 4tH line of data

Push 114, 126,  53, 114,  96,  25, 109,   7,  31, 141,  46,  13,  27,  43
Push  56, 69 : Gosub _Read             ' read 5th line of data

Push 117, 116,  27,   7,  68,  40,  31, 115, 124,  42, 128,  52,  71, 118
Push  70, 83 : Gosub _Read             ' read 6th line of data

Push 117,  38,  27, 106,  33, 117, 116, 111,  40, 119,  47, 105,  57, 122
Push  84, 97 : Gosub _Read             ' read 7th line of data

Push 109, 124, 115,  43, 120,  43,  27,  27,  18,  28,  48, 125, 107, 114
Push  98, 111 : Gosub _Read            ' read 8th line of data

Push  34, 133,  45, 120,  30, 127,  31, 116, 146
Push 112, 120 : Gosub _Read            ' read last line of data

Push 121 : Gosub _SimpleSort           ' now sort 121 elements

i = @(0) / 10 - 1
For j = 0 To Pop() - 1                 ' note array size was still on stack
  d = @(j) / 10
  Do While d > i
    If j Print
    i = i + 1
    If i < 10 Print " ";               ' align stem number
    Print i;" |";                      ' print stem number
  Loop
  Print @(j) % 10;" ";                 ' print leaf number
Next
Print                                  ' print final LF

End

                                       ' simplest sorting algorithm
_SimpleSort                            ' ( n -- n)
  For x = 0 To Tos() - 1
    For y = x+1 To Tos() - 1
      If @(x) > @ (y) Then             ' if larger, switch elements
         Push @(y)
         @(y) = @(x)
         @(x) = Pop()
      Endif
    Next
  Next

Return

                                       ' read a line of data backwards
_Read                                  ' (.. n1 n2 -- ..)
  For x = Pop() To Pop() Step -1       ' loop from n2 to n1
    @(x) = Pop()                       ' get element from stack
  Next
Return
```

Output:

```txt
 0 |7 7
 1 |2 3 8 8
 2 |3 5 7 7 7 7 7 7 8 8 9 9
 3 |0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 |0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 |2 3 7 8 8
 6 |1 3 8
 7 |1
 8 |
 9 |6 9
10 |4 5 5 5 5 6 7 9 9 9
11 |1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 |0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 |1 2 3 9
14 |1 6

0 OK, 0:2037

```


## Ursala


```Ursala
#import std
#import nat

data =

<
   12,127,28,42,39,113,42,18,44,118,44,37,113,124,37,48,127,36,29,31,125,139,131,
   115,105,132,104,123,35,113,122,42,117,119,58,109,23,105,63,27,44,105,99,41,128,
   121,116,125,32,61,37,127,29,113,121,58,114,126,53,114,96,25,109,7,31,141,46,13,
   27,43,117,116,27,7,68,40,31,115,124,42,128,52,71,118,117,38,27,106,33,117,116,
   111,40,119,47,105,57,122,109,124,115,43,120,43,27,27,18,28,48,125,107,114,34,
   133,45,120,30,127,31,116,146>

stemleaf_plot =

^|T(~&,' | '--)*+ -+
   ^p(pad` @hS; * ==` ~-rlT,mat` *tS)@hSS+ (%nP*)^|*H/~& ^lrNCT/iota ~&,
   ^(*+ ^C/~&+ -:0!,~&zl)+ ^|(~&,nleq-<)*+ nleq-<&l@lK2hlPrSXS+ * division\10+-

#show+

main = stemleaf_plot data
```

Reading from right to left on the bottom line of the <code>stemleaf_plot</code> function, we
obtain the quotient and remainder of every datum divided by ten, partition by
quotients, sort the partitions and sort within each partition,
make a note of the maximum quotient, and reify the table into a function.

From right to left on the line above, we apply the reified function to each
number up to the maximum quotient (i.e., including 8 which wasn't in the
original table), convert each number to a string, flatten each leaf list with spaces as
separators, and right justify the stem list.

The remaining top line forms the concatenation of each stem with its flattened leaf
list and a vertical bar between them.

output:

```txt

 0 | 7 7
 1 | 2 3 8 8
 2 | 3 5 7 7 7 7 7 7 8 8 9 9
 3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
 4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
 5 | 2 3 7 8 8
 6 | 1 3 8
 7 | 1
 8 |
 9 | 6 9
10 | 4 5 5 5 5 6 7 9 9 9
11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
13 | 1 2 3 9
14 | 1 6

```



## zkl

```zkl
fcn leaf_plot(xs){
   xs=xs.sort();
   i := xs[0] / 10 - 1;
   foreach j in (xs.len()){
      d := xs[j] / 10;
      while (d > i){ print("%s%3d |".fmt(j and "\n" or "", i+=1)); }
      print(" %d".fmt(xs[j] % 10));
   }
   println();
}

data := T(
   12, 127,  28,  42,  39, 113,  42,  18,  44, 118,  44,  37, 113, 124,
   37,  48, 127,  36,  29,  31, 125, 139, 131, 115, 105, 132, 104, 123,
   35, 113, 122,  42, 117, 119,  58, 109,  23, 105,  63,  27,  44, 105,
   99,  41, 128, 121, 116, 125,  32,  61,  37, 127,  29, 113, 121,  58,
   114, 126,  53, 114,  96,  25, 109,   7,  31, 141,  46,  13,  27,  43,
   117, 116,  27,   7,  68,  40,  31, 115, 124,  42, 128,  52,  71, 118,
   117,  38,  27, 106,  33, 117, 116, 111,  40, 119,  47, 105,  57, 122,
   109, 124, 115,  43, 120,  43,  27,  27,  18,  28,  48, 125, 107, 114,
   34, 133,  45, 120,  30, 127,  31, 116, 146 );

leaf_plot(data);
```

```txt

  0 | 7 7
  1 | 2 3 8 8
  2 | 3 5 7 7 7 7 7 7 8 8 9 9
  3 | 0 1 1 1 1 2 3 4 5 6 7 7 7 8 9
  4 | 0 0 1 2 2 2 2 3 3 3 4 4 4 5 6 7 8 8
  5 | 2 3 7 8 8
  6 | 1 3 8
  7 | 1
  8 |
  9 | 6 9
 10 | 4 5 5 5 5 6 7 9 9 9
 11 | 1 3 3 3 3 4 4 4 5 5 5 6 6 6 6 7 7 7 7 8 8 9 9
 12 | 0 0 1 1 2 2 3 4 4 4 5 5 5 6 7 7 7 7 8 8
 13 | 1 2 3 9
 14 | 1 6

```



