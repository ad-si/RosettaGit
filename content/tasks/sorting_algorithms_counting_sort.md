+++
title = "Sorting algorithms/Counting sort"
description = ""
date = 2019-09-04T23:33:01Z
aliases = []
[extra]
id = 4201
[taxonomies]
categories = ["task", "Sorting Algorithms"]
tags = []
languages = [
  "actionscript",
  "ada",
  "algol_68",
  "autohotkey",
  "basic256",
  "bbc_basic",
  "c",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "e",
  "eiffel",
  "elena",
  "elixir",
  "fortran",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "io",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "langur",
  "lua",
  "m4",
  "mathematica",
  "maxscript",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "octave",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "slate",
  "smalltalk",
  "tcl",
  "vba",
  "vbscript",
  "xpl0",
  "zkl",
]
+++

## Task

Implement the [[wp:Counting sort|Counting sort]]. This is a way of sorting integers when the minimum and maximum value are known.


Pseudocode:
 '''function''' ''countingSort''(array, min, max):
     count: '''array of''' (max - min + 1) '''elements'''
     '''initialize''' count '''with''' 0
     '''for each''' number '''in''' array '''do'''
         count[number - min] := count[number - min] + 1
     '''done'''
     z := 0
     '''for''' i '''from''' min '''to''' max '''do'''
         '''while''' ( count[i - min] > 0 ) '''do'''
             array[z] := i
             z := z+1
             count[i - min] := count[i - min] - 1
         '''done'''
     '''done'''

The ''min'' and ''max'' can be computed apart, or be known ''a priori''.


'''Note''':   we know that, given an array of integers,   its maximum and minimum values can be always found;   but if we imagine the worst case for an array that can hold up to 32 bit integers,   we see that in order to hold the counts,   an array of up to '''2<sup>32</sup>''' elements may be needed.   I.E.:   we need to hold a count value up to '''2<sup>32</sup>-1''',   which is a little over 4.2 Gbytes.   So the counting sort is more practical when the range is (very) limited,   and minimum and maximum values are known   ''a priori''.   (The use of   ''sparse arrays''   minimizes the impact of the memory usage,   as well as removing the need of having to know the minimum and maximum values   ''a priori''.)





## ActionScript


```ActionScript
function countingSort(array:Array, min:int, max:int)
{
	var count:Array = new Array(array.length);
	for(var i:int = 0; i < count.length;i++)count[i]=0;
	for(i = 0; i < array.length; i++)
	{
		count[array[i]-min] ++;
	}
	var j:uint = 0;
	for(i = min; i <= max; i++)
	{
		for(; count[i-min] > 0; count[i-min]--)
			array[j++] = i;
	}
	return array;
}
```



## Ada

Given that we know the range of data, the problem really reduces to initializing the array to the ordered range of values. The input order is irrelevant.

```Ada
with Ada.Text_Io; use Ada.Text_Io;

procedure Counting_Sort is
   type Data is array (Integer range <>) of Natural;
   procedure Sort(Item : out Data) is
   begin
      for I in Item'range loop
         Item(I) := I;
      end loop;
   end Sort;
   Stuff : Data(1..140);
begin
   Sort(Stuff);
   for I in Stuff'range loop
      Put(Natural'Image(Stuff(I)));
   end loop;
   New_Line;
end Counting_Sort;
```


### Output

 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97
 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132
 133 134 135 136 137 138 139 140


## ALGOL 68

```algol68
PROC counting sort mm = (REF[]INT array, INT min, max)VOID:
(
  INT z := LWB array - 1;
  [min:max]INT count;

  FOR i FROM LWB count TO UPB count DO count[i] := 0 OD;
  FOR i TO UPB array DO count[ array[i] ]+:=1 OD;

  FOR i FROM LWB count TO UPB count DO
    FOR j TO count[i] DO array[z+:=1] := i OD
  OD
);

PROC counting sort = (REF[]INT array)VOID:
(
  INT min, max;
  min := max := array[LWB array];

  FOR i FROM LWB array + 1 TO UPB array DO
    IF array[i] < min THEN
      min := array[i]
    ELIF array[i] > max THEN
      max := array[i]
    FI
  OD
);

# Testing (we suppose the oldest human being is less than 140 years old). #

INT n = 100;
INT min age = 0, max age = 140;
main:
(
  [n]INT ages;

  FOR i TO UPB ages DO ages[i] := ENTIER (random * ( max age + 1 ) ) OD;
  counting sort mm(ages, min age, max age);
  FOR i TO UPB ages DO print((" ", whole(ages[i],0))) OD;
  print(new line)
)
```

Sample output:

0 1 2 3 3 4 4 5 6 7 8 9 9 10 11 12 15 18 18 19 21 21 22 27 33 35 36 38 38 38 38 39 40 40 41 43 44 53 54 55 57 57 58 59 59 60 60 60 60 61 62 64 65 66 67 68 70 71 78 79 82 83 84 84 87 87 88 88 88 89 89 92 93 93 97 98 99 99 100 107 109 114 115 115 118 122 126 127 127 129 129 130 131 133 134 136 136 137 139 139

## AutoHotkey

contributed by Laszlo on the ahk [http://www.autohotkey.com/forum/post-276465.html#276465 forum]

```AutoHotkey
MsgBox % CountingSort("-1,1,1,0,-1",-1,1)

CountingSort(ints,min,max) {
   Loop % max-min+1
      i := A_Index-1, a%i% := 0
   Loop Parse, ints, `, %A_Space%%A_Tab%
      i := A_LoopField-min, a%i%++
   Loop % max-min+1 {
      i := A_Index-1, v := i+min
      Loop % a%i%
         t .= "," v
   }
   Return SubStr(t,2)
}
```



## BASIC256


```BASIC256

# counting sort

n = 10

dim test(n)
test = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1}

mn = -31
mx = 782

dim cnt(mx - mn + 1)  # count is a reserved string function name

# seems initialized as 0
# for i = 1 to n
#   print cnt[i]
# next i

# sort
for i = 0 to n-1
  cnt[test[i] - mn] = cnt[test[i] - mn] + 1
next i

# output
print "original"
for i = 0 to n-1
  print test[i] + " ";
next i
print
print "ordered"
for i = 0 to mx - mn
  if 0 < cnt[i] then  # for i = k to 0  causes error
    for k = 1 to cnt[i]
      print i + mn + " ";
    next k
  endif
next i
print

```



## BBC BASIC


```bbcbasic
      DIM test%(9)
      test%() = 4, 65, 2, -31, 0, 99, 2, 83, 782, 1
      PROCcountingsort(test%(), -31, 782)
      FOR i% = 0 TO 9
        PRINT test%(i%) ;
      NEXT
      PRINT
      END

      DEF PROCcountingsort(a%(), l%, h%)
      LOCAL i%, z%, c%()
      DIM c%(h% - l%)
      FOR i% = 0 TO DIM(a%(),1)
        c%(a%(i%) - l%) += 1
      NEXT
      FOR i% = l% TO h%
        WHILE c%(i% - l%)
          a%(z%) = i%
          z% += 1
          c%(i% - l%) -= 1
        ENDWHILE
      NEXT
      ENDPROC
```

'''Output:'''

```txt

       -31         0         1         2         2         4        65        83        99       782

```



## C


```c
#include <stdio.h>
#include <stdlib.h>

void counting_sort_mm(int *array, int n, int min, int max)
{
  int i, j, z;

  int range = max - min + 1;
  int *count = malloc(range * sizeof(*array));

  for(i = 0; i < range; i++) count[i] = 0;
  for(i = 0; i < n; i++) count[ array[i] - min ]++;

  for(i = min, z = 0; i <= max; i++) {
    for(j = 0; j < count[i - min]; j++) {
      array[z++] = i;
    }
  }

  free(count);
}

void min_max(int *array, int n, int *min, int *max)
{
  int i;

  *min = *max = array[0];
  for(i=1; i < n; i++) {
    if ( array[i] < *min ) {
      *min = array[i];
    } else if ( array[i] > *max ) {
      *max = array[i];
    }
  }
}
```


Testing (we suppose the oldest human being is less than 140 years old).


```c
#define N 100
#define MAX_AGE 140
int main()
{
  int ages[N], i;

  for(i=0; i < N; i++) ages[i] = rand()%MAX_AGE;
  counting_sort_mm(ages, N, 0, MAX_AGE);
  for(i=0; i < N; i++) printf("%d\n", ages[i]);
  return EXIT_SUCCESS;
}
```



## C++


```cpp

#include <iostream>
#include <time.h>

//------------------------------------------------------------------------------
using namespace std;

//------------------------------------------------------------------------------
const int MAX = 30;

//------------------------------------------------------------------------------
class cSort
{
public:
    void sort( int* arr, int len )
    {
	int mi, mx, z = 0; findMinMax( arr, len, mi, mx );
	int nlen = ( mx - mi ) + 1; int* temp = new int[nlen];
	memset( temp, 0, nlen * sizeof( int ) );

	for( int i = 0; i < len; i++ ) temp[arr[i] - mi]++;

	for( int i = mi; i <= mx; i++ )
	{
	    while( temp[i - mi] )
	    {
		arr[z++] = i;
		temp[i - mi]--;
	    }
	}

	delete [] temp;
    }

private:
    void findMinMax( int* arr, int len, int& mi, int& mx )
    {
	mi = INT_MAX; mx = 0;
	for( int i = 0; i < len; i++ )
	{
	    if( arr[i] > mx ) mx = arr[i];
	    if( arr[i] < mi ) mi = arr[i];
	}
    }
};
//------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    srand( time( NULL ) ); int arr[MAX];
    for( int i = 0; i < MAX; i++ )
	arr[i] = rand() % 140 - rand() % 40 + 1;

    for( int i = 0; i < MAX; i++ )
	cout << arr[i] << ", ";
    cout << endl << endl;

    cSort s; s.sort( arr, MAX );

    for( int i = 0; i < MAX; i++ )
	cout << arr[i] << ", ";
    cout << endl << endl;

    return system( "pause" );
}
//------------------------------------------------------------------------------

```

```txt

105, -21, 20, 5, 3, 25, 101, 116, 82, 5, 88, 80, -9, 26, 62, 118, 131, -31, 3, 3
8, 40, -6, 46, 90, 7, 59, 104, 76, 12, 79,

-31, -21, -9, -6, 3, 3, 5, 5, 7, 12, 20, 25, 26, 38, 40, 46, 59, 62, 76, 79, 80,
 82, 88, 90, 101, 104, 105, 116, 118, 131,

```



###  Alternate version

Uses C++11. Compile with
 g++ -std=c++11 counting.cpp

```cpp
#include <algorithm>
#include <iterator>
#include <iostream>
#include <vector>

template<typename ForwardIterator> void counting_sort(ForwardIterator begin,
                                                      ForwardIterator end) {
  auto min_max = std::minmax_element(begin, end);
  if (min_max.first == min_max.second) {  // empty range
    return;
  }
  auto min = *min_max.first;
  auto max = *min_max.second;
  std::vector<unsigned> count((max - min) + 1, 0u);
  for (auto i = begin; i != end; ++i) {
    ++count[*i - min];
  }
  for (auto i = min; i <= max; ++i) {
    for (auto j = 0; j < count[i - min]; ++j) {
      *begin++ = i;
    }
  }
}

int main() {
  int a[] = {100, 2, 56, 200, -52, 3, 99, 33, 177, -199};
  counting_sort(std::begin(a), std::end(a));
  copy(std::begin(a), std::end(a), std::ostream_iterator<int>(std::cout, " "));
  std::cout << "\n";
}
```

Output:

```txt

-199 -52 2 3 33 56 99 100 177 200

```


## C#

```c#
using System;
using System.Linq;

namespace CountingSort
{
    class Program
    {
        static void Main(string[] args)
        {
            Random rand = new Random();                                   // Just for creating a test array
            int[] arr = new int[100];                                     // of random numbers
            for (int i = 0; i < 100; i++) { arr[i] = rand.Next(0, 100); } // ...

            int[] newarr = countingSort(arr, arr.Min(), arr.Max());
        }

        private static int[] countingSort(int[] arr, int min, int max)
        {
            int[] count = new int[max - min + 1];
            int z = 0;

            for (int i = 0; i < count.Length; i++) { count[i] = 0; }
            for (int i = 0; i < arr.Length; i++) { count[arr[i] - min]++; }

            for (int i = min; i <= max; i++)
            {
                while (count[i - min]-- > 0)
                {
                    arr[z] = i;
                    z++;
                }
            }
            return arr;
        }
    }
}
```



## Common Lisp

Straightforward implementation of counting sort.  By using <code>[http://www.lispworks.com/documentation/HyperSpec/Body/f_map.htm map]</code> and <code>[http://www.lispworks.com/documentation/HyperSpec/Body/f_map_in.htm map-into]</code>, counting sort can work efficiently on both lists and vectors.  The closure given as the second argument to <code>map-into</code> returns the sorted elements of sequence.  Because <code>map-into</code> will only call the function as many times as necessary to re-populate sequence, there is no need for bounds checking.  <code>counts</code> is declared to have dynamic-extent and so a compiler might stack allocate it.


```lisp
(defun counting-sort (sequence &optional (min (reduce #'min sequence))
                                         (max (reduce #'max sequence)))
  (let ((i 0)
        (counts (make-array (1+ (- max min)) :initial-element 0
                                             :element-type `(integer 0 ,(length sequence)))))
    (declare (dynamic-extent counts))
    (map nil (lambda (n) (incf (aref counts (- n min)))) sequence)
    (map-into sequence (lambda ()
                         (do () ((plusp (aref counts i)))
                           (incf i))
                         (decf (aref counts i))
                         (+ i min)))))
```



## D


```d
import std.stdio, std.algorithm;

void countingSort(int[] array, in size_t min, in size_t max)
pure nothrow {
    auto count = new int[max - min + 1];
    foreach (number; array)
        count[number - min]++;

    size_t z = 0;
    foreach (i; min .. max + 1)
        while (count[i - min] > 0) {
            array[z] = i;
            z++;
            count[i - min]--;
        }
}

void main() {
    auto data = [9, 7, 10, 2, 9, 7, 4, 3, 10, 2, 7, 10, 2, 1, 3, 8,
                 7, 3, 9, 5, 8, 5, 1, 6, 3, 7, 5, 4, 6, 9, 9, 6, 6,
                 10, 2, 4, 5, 2, 8, 2, 2, 5, 2, 9, 3, 3, 5, 7, 8, 4];

    int dataMin = reduce!min(data);
    int dataMax = reduce!max(data);
    countingSort(data, dataMin, dataMax);
    assert(isSorted(data));
}
```



## E

Straightforward implementation, no particularly interesting characteristics.


```e
def countingSort(array, min, max) {
    def counts := ([0] * (max - min + 1)).diverge()
    for elem in array {
        counts[elem - min] += 1
    }
    var i := -1
    for offset => count in counts {
        def elem := min + offset
        for _ in 1..count {
            array[i += 1] := elem
        }
    }
}
```


<pre style="height:15ex;overflow:scroll">? def arr := [34,6,8,7,4,3,56,7,8,4,3,5,7,8,6,4,4,67,9,0,0,76,467,453,34,435,37,4,34,234,435,3,2,7,4,634,534,735,5,4,6,78,4].diverge()
# value: [34, 6, 8, 7, 4, 3, 56, 7, 8, 4, 3, 5, 7, 8, 6, 4, 4, 67, 9, 0, 0, 76, 467, 453, 34, 435, 37, 4, 34, 234, 435, 3, 2, 7, 4, 634, 534, 735, 5, 4, 6, 78, 4].diverge()

? countingSort(arr, 0, 735)
? arr
# value: [0, 0, 2, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 9, 34, 34, 34, 37, 56, 67, 76, 78, 234, 435, 435, 453, 467, 534, 634, 735].diverge()
```




## Eiffel



```Eiffel


class
	COUNTING_SORT

feature

	sort (ar: ARRAY [INTEGER]; min, max: INTEGER): ARRAY [INTEGER]
			-- Sorted Array in ascending order.
		require
			ar_not_void: ar /= Void
			lowest_index_zero: ar.lower = 0
		local
			count: ARRAY [INTEGER]
			i, j, z: INTEGER
		do
			create Result.make_empty
			Result.deep_copy (ar)
			create count.make_filled (0, 0, max - min)
			from
				i := 0
			until
				i = Result.count
			loop
				count [Result [i] - min] := count [Result [i] - min] + 1
				i := i + 1
			end
			z := 0
			from
				i := min
			until
				i > max
			loop
				from
					j := 0
				until
					j = count [i - min]
				loop
					Result [z] := i
					z := z + 1
					j := j + 1
				end
				i := i + 1
			end
		ensure
			Result_is_sorted: is_sorted (Result)
		end

feature {NONE}

	is_sorted (ar: ARRAY [INTEGER]): BOOLEAN
			--- Is 'ar' sorted in ascending order?
		require
			ar_not_empty: ar.is_empty = False
		local
			i: INTEGER
		do
			Result := True
			from
				i := ar.lower
			until
				i = ar.upper
			loop
				if ar [i] > ar [i + 1] then
					Result := False
				end
				i := i + 1
			end
		end

end

```

TEST:

```Eiffel

class
	APPLICATION

create
	make

feature

	make
		do
			create test.make_filled (0, 0, 5)
			test [0] := -7
			test [1] := 4
			test [2] := 2
			test [3] := 6
			test [4] := 1
			test [5] := 3
			io.put_string ("unsorted:%N")
			across
				test as t
			loop
				io.put_string (t.item.out + "%T")
			end
			io.new_line
			io.put_string ("sorted:%N")
			create count
			test := count.sort (test, -7, 6)
			across
				test as ar
			loop
				io.put_string (ar.item.out + "%T")
			end
		end

	count: COUNTING_SORT

	test: ARRAY [INTEGER]

end


```

```txt

unsorted:
-7 4 2 6 1 3
sorted:
-7 1 2 3 4 6

```


## Elena

ELENA 4.x :

```elena
import extensions;
import system'routines;

extension op
{
    countingSort()
        = self.clone().countingSort(self.MinimalMember, self.MaximalMember);

    countingSort(int min, int max)
    {
        int[] count := new int[](max - min + 1);
        int z := 0;

        count.populate:(int i => 0);

        for(int i := 0, i < self.Length, i += 1) { count[self[i] - min] := count[self[i] - min] + 1 };

        for(int i := min, i <= max, i += 1)
        {
            while (count[i - min] > 0)
            {
                self[z] := i;
                z += 1;

                count[i - min] := count[i - min] - 1
            }
        }
    }
}

public program()
{
    var list := new Range(0, 10).selectBy:(i => randomGenerator.eval(10)).toArray();

    console.printLine("before:", list.asEnumerable());
    console.printLine("after :", list.countingSort().asEnumerable())
}
```

```txt

before:6,5,3,1,0,0,7,7,8,2
after :0,0,1,2,3,5,6,7,7,8

```



## Elixir

```elixir
defmodule Sort do
  def counting_sort([]), do: []
  def counting_sort(list) do
    {min, max} = Enum.min_max(list)
    count = Tuple.duplicate(0, max - min + 1)
    counted = Enum.reduce(list, count, fn x,acc ->
      i = x - min
      put_elem(acc, i, elem(acc, i) + 1)
    end)
    Enum.flat_map(min..max, &List.duplicate(&1, elem(counted, &1 - min)))
  end
end

IO.inspect Sort.counting_sort([1,-2,-3,2,1,-5,5,5,4,5,9])
```


```txt

[-5, -3, -2, 1, 1, 2, 4, 5, 5, 5, 9]

```



## Fortran

```fortran
module CountingSort
  implicit none

  interface counting_sort
     module procedure counting_sort_mm, counting_sort_a
  end interface

contains

  subroutine counting_sort_a(array)
    integer, dimension(:), intent(inout) :: array

    call counting_sort_mm(array, minval(array), maxval(array))

  end subroutine counting_sort_a

  subroutine counting_sort_mm(array, tmin, tmax)
    integer, dimension(:), intent(inout) :: array
    integer, intent(in) :: tmin, tmax

    integer, dimension(tmin:tmax) :: cnt
    integer :: i, z

    cnt = 0                   ! Initialize to zero to prevent false counts
    FORALL (I=1:size(array))  ! Not sure that this gives any benefit over a DO loop.
        cnt(array(i)) = cnt(array(i))+1
    END FORALL
!
!   ok - cnt contains the frequency of every value
!   let's unwind them into the original array
!
    z = 1
    do i = tmin, tmax
       do while ( cnt(i) > 0 )
          array(z) = i
          z = z + 1
          cnt(i) = cnt(i) - 1
       end do
    end do

  end subroutine counting_sort_mm

end module CountingSort
```


Testing:


```fortran
program test
  use CountingSort
  implicit none

  integer, parameter :: n = 100, max_age = 140

  real, dimension(n) :: t
  integer, dimension(n) :: ages

  call random_number(t)
  ages = floor(t * max_age)

  call counting_sort(ages, 0, max_age)

  write(*,'(I4)') ages

end program test
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function findMax(array() As Integer) As Integer
  Dim length As Integer = UBound(array) - LBound(array) + 1
  If length = 0 Then Return 0 '' say
  If length = 1 Then Return array(LBound(array))
  Dim max As Integer = LBound(array)
  For i As Integer = LBound(array) + 1 To UBound(array)
    If array(i) > max Then max = array(i)
  Next
  Return max
End Function

Function findMin(array() As Integer) As Integer
  Dim length As Integer = UBound(array) - LBound(array) + 1
  If length = 0 Then Return 0 '' say
  If length = 1 Then Return array(LBound(array))
  Dim min As Integer = LBound(array)
  For i As Integer = LBound(array) + 1 To UBound(array)
    If array(i) < min Then min = array(i)
  Next
  Return min
End Function

Sub countingSort(array() As Integer, min As Integer, max As Integer)
  Dim count(0 To max - min) As Integer '' all zero by default
  Dim As Integer number, z
  For i As Integer = LBound(array) To UBound(array)
    number = array(i)
    count(number - min) += 1
  Next
  z = LBound(array)
  For i As Integer = min To max
    While count(i - min) > 0
      array(z) = i
      z += 1
      count(i - min) -= 1
    Wend
  Next
End Sub

Sub printArray(array() As Integer)
  For i As Integer = LBound(array) To UBound(array)
    Print Using "####"; array(i);
  Next
  Print
End Sub

Dim array(1 To 10) As Integer = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1} '' using BBC BASIC example array
Print "Unsorted : ";
printArray(array())
Dim max As Integer = findMax(array())
Dim min As Integer = findMin(array())
countingSort array(), min, max
Print "Sorted   : ";
printArray(array())
Print
Print "Press any key to quit"
Sleep
```


```txt

Unsorted :    4  65   2 -31   0  99   2  83 782   1
Sorted   :  -31   0   1   2   2   4  65  83  99 782

```



## Go

This version follows the task pseudocode above, with one more optimization.

```go
package main

import (
    "fmt"
    "runtime"
    "strings"
)

var a = []int{170, 45, 75, -90, -802, 24, 2, 66}
var aMin, aMax = -1000, 1000

func main() {
    fmt.Println("before:", a)
    countingSort(a, aMin, aMax)
    fmt.Println("after: ", a)
}

func countingSort(a []int, aMin, aMax int) {
    defer func() {
        if x := recover(); x != nil {
            // one error we'll handle and print a little nicer message
            if _, ok := x.(runtime.Error); ok &&
                strings.HasSuffix(x.(error).Error(), "index out of range") {
                fmt.Printf("data value out of range (%d..%d)\n", aMin, aMax)
                return
            }
            // anything else, we re-panic
            panic(x)
        }
    }()

    count := make([]int, aMax-aMin+1)
    for _, x := range a {
        count[x-aMin]++
    }
    z := 0
    // optimization over task pseudocode:   variable c is used instead of
    // count[i-min].  This saves some unneccessary calculations.
    for i, c := range count {
        for ; c > 0; c-- {
            a[z] = i + aMin
            z++
        }
    }
}
```

This version follows the WP pseudocode.  It can be adapted to sort items other than integers.

```go
package main

import (
    "fmt"
    "runtime"
    "strings"
)

var a = []int{170, 45, 75, -90, -802, 24, 2, 66}
var aMin, aMax = -1000, 1000

func main() {
    fmt.Println("before:", a)
    countingSort(a, aMin, aMax)
    fmt.Println("after: ", a)
}

func countingSort(a []int, aMin, aMax int) {
    defer func() {
        if x := recover(); x != nil {
            // one error we'll handle and print a little nicer message
            if _, ok := x.(runtime.Error); ok &&
                strings.HasSuffix(x.(error).Error(), "index out of range") {
                fmt.Printf("data value out of range (%d..%d)\n", aMin, aMax)
                return
            }
            // anything else, we re-panic
            panic(x)
        }
    }()

    // WP algorithm
    k := aMax - aMin // k is maximum key value. keys range 0..k
    count := make([]int, k+1)
    key := func(v int) int { return v - aMin }
    for _, x := range a {
        count[key(x)]++
    }
    total := 0
    for i, c := range count {
        count[i] = total
        total += c
    }
    output := make([]int, len(a))
    for _, x := range a {
        output[count[key(x)]] = x
        count[key(x)]++
    }
    copy(a, output)
}
```



## Groovy

Solution:

```groovy
def countingSort = { array ->
    def max = array.max()
    def min = array.min()
    // this list size allows use of Groovy's natural negative indexing
    def count = [0] * (max + 1 + [0, -min].max())
    array.each { count[it] ++ }
    (min..max).findAll{ count[it] }.collect{ [it]*count[it] }.flatten()
}
```


Test:

```groovy
println countingSort([23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4])
println countingSort([88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1])

println countingSort([15,-3,0,-1,5,4,5,20,-8])
println countingSort([34,6,8,7,4,3,56,7,8,4,3,5,7,8,6,4,4,67,9,0,0,76,467,453,34,435,37,4,34,234,435,3,2,7,4,634,534,-735,5,4,6,78,4])
// slo-o-o-o-ow due to unnecessarily large counting array
println countingSort([10000033,10000006,10000008,10000009,10000013,10000031,10000013,10000032,10000023,10000023,10000011,10000012,10000021])
```


Output:

```txt
[4, 12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]
[0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]
[-8, -3, -1, 0, 4, 5, 5, 15, 20]
[-735, 0, 0, 2, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 9, 34, 34, 34, 37, 56, 67, 76, 78, 234, 435, 435, 453, 467, 534, 634]
[10000006, 10000008, 10000009, 10000011, 10000012, 10000013, 10000013, 10000021, 10000023, 10000023, 10000031, 10000032, 10000033]
```



## Haskell

We use lists for input and output rather than arrays, since lists are used more often in Haskell.


```haskell
import Data.Array

countingSort :: (Ix n) => [n] -> n -> n -> [n]
countingSort l lo hi = concatMap (uncurry $ flip replicate) count
  where count = assocs . accumArray (+) 0 (lo, hi) . map (\i -> (i, 1)) $ l
```



## Io

```io
List do(
    countingSort := method(min, max,
        count := list() setSize(max - min + 1) mapInPlace(0)
        foreach(x,
            count atPut(x - min, count at(x - min) + 1)
        )

        j := 0
        for(i, min, max,
            while(count at(i - min) > 0,
                atPut(j, i)
                count atPut(i - min, at(i - min) - 1)
                j = j + 1
            )
        )
    self)

    countingSortInPlace := method(
        countingSort(min, max)
    )
)

l := list(2, 3, -4, 5, 1)
l countingSortInPlace println # ==> list(-4, 1, 2, 3, 5)
```


A more functional-like version:

```io
List do(
    fill := method(x, size,
        /* Resizes list to a given size and fills it with a given value. */
        setSize(size) mapInPlace(x)
    )

    countingSort := method(min, max,
        count := list() fill(0, max - min + 1)
        foreach(x,
            count atPut(x - min, count at(x - min) + 1)
        )

        return count map(i, x, list() fill(i + min, x)) \
            prepend(list()) reduce(xs, x, xs appendSeq(x))
    )

    countingSortInPlace := method(
        copy(countingSort(min, max))
    )
)

l := list(2, 3, -4, 5, 1)
l countingSortInPlace println # ==> list(-4, 1, 2, 3, 5)
```


=={{header|Icon}} and {{header|Unicon}}==
The following example is hopefully in the spirit of a counting sort using a hash table as a substituted for a sparse array.  Simply translating the pseudo-code would be very un-Iconish (as opposed to Uniconish).


```Icon
procedure main()                                         #: demonstrate various ways to sort a list and string
   write("Sorting Demo using ",image(countingsort))
   writes("  on list : ")
   writex(UL)
   displaysort(countingsort,copy(UL))
end

procedure countingsort(X)                                #: return sorted list (integers only)
local T,lower,upper

   T := table(0)                                         # hash table as sparse array
   lower := upper := X[1]

   every x := !X do {
      if not ( integer(x) = x ) then runerr(x,101)       # must be integer
      lower >:= x                                        # minimum
      upper <:= x                                        # maximum
      T[x] +:= 1                                         # record x's and duplicates
      }

   every put(X := [],( 1 to T[i := lower to upper], i) ) # reconstitute with correct order and count
   return X
end
```


Note: This example relies on [[Sorting_algorithms/Bubble_sort#Icon| the supporting procedures 'display sort', and 'writex' from Bubble Sort]].

Sample output:
```txt
Sorting Demo using procedure countingsort
  on list : [ 3 14 1 5 9 2 6 3 ]
    with op = &null:         [ 1 2 3 3 5 6 9 14 ]   (0 ms)
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>
100 PROGRAM "CountSrt.bas"
110 RANDOMIZE
120 NUMERIC ARRAY(5 TO 24)
130 CALL INIT(ARRAY)
140 CALL WRITE(ARRAY)
150 CALL COUNTINGSORT(ARRAY)
160 CALL WRITE(ARRAY)
170 DEF INIT(REF A)
180   FOR I=LBOUND(A) TO UBOUND(A)
190     LET A(I)=RND(98)+1
200   NEXT
210 END DEF
220 DEF WRITE(REF A)
230   FOR I=LBOUND(A) TO UBOUND(A)
240     PRINT A(I);
250   NEXT
260   PRINT
270 END DEF
280 DEF FMIN(REF A)
290   LET T=INF
300   FOR I=LBOUND(A) TO UBOUND(A)
310     LET T=MIN(A(I),T)
320   NEXT
330   LET FMIN=T
340 END DEF
350 DEF FMAX(REF A)
360   LET T=-INF
370   FOR I=LBOUND(A) TO UBOUND(A)
380     LET T=MAX(A(I),T)
390   NEXT
400   LET FMAX=T
410 END DEF
420 DEF COUNTINGSORT(REF A)
430   LET MX=FMAX(A):LET MN=FMIN(A):LET Z=LBOUND(A)
440   NUMERIC COUNT(0 TO MX-MN)
450   FOR I=0 TO UBOUND(COUNT)
460     LET COUNT(I)=0
470   NEXT
480   FOR I=Z TO UBOUND(A)
490     LET COUNT(A(I)-MN)=COUNT(A(I)-MN)+1
500   NEXT
510   FOR I=MN TO MX
520     DO WHILE COUNT(I-MN)>0
530       LET A(Z)=I:LET Z=Z+1:LET COUNT(I-MN)=COUNT(I-MN)-1
540     LOOP
550   NEXT
560 END DEF
```



## J

```j
csort =: monad define
  min =. <./y
  cnt =. 0 $~ 1+(>./y)-min
  for_a. y do.
    cnt =. cnt >:@{`[`]}~ a-min
  end.
  cnt # min+i.#cnt
)
```


Alternative implementation:


```j
csort=: (+/@(=/) # ]) >./ (] + 1 i.@+ -) <./
```



'''Example:'''

```j
   ] a =. _3 + 20 ?@$ 10
_2 _2 6 _1 1 6 _1 4 4 1 4 4 5 _3 5 3 0 _1 3 4

   csort a
_3 _2 _2 _1 _1 _1 0 1 1 3 3 4 4 4 4 4 5 5 6 6
```


And note that this can be further simplified if the range is known in advance (which could easily be the case -- this sorting mechanism is practical when we have a small fixed range of values that we are sorting).  Here, we do not need to inspect the data to find min and max values, since they are already known:


```j
csrt=:2 :0
  (m+i.n-m) (+/@(=/)~ # [) ]
)
```


or


```j
csrt=:2 :0
   (+/@(=/) # ])&(m+i.n-m)
)
```


Example:


```j
   (_3 csrt 17) a
_3 _2 _2 _1 _1 _1 0 1 1 3 3 4 4 4 4 4 5 5 6 6
```



## Java

```java5
public static void countingSort(int[] array, int min, int max){
	int[] count= new int[max - min + 1];
	for(int number : array){
		count[number - min]++;
	}
	int z= 0;
	for(int i= min;i <= max;i++){
		while(count[i - min] > 0){
			array[z]= i;
			z++;
			count[i - min]--;
		}
	}
}
```



## JavaScript



```javascript
var countSort = function(arr, min, max) {
    var i, z = 0, count = [];

    for (i = min; i <= max; i++) {
        count[i] = 0;
    }

    for (i=0; i < arr.length; i++) {
        count[arr[i]]++;
    }

    for (i = min; i <= max; i++) {
        while (count[i]-- > 0) {
            arr[z++] = i;
        }
    }

}
```


Testing:


```javascript
// Line breaks are in HTML

var i, ages = [];

for (i = 0; i < 100; i++) {
    ages.push(Math.floor(Math.random() * (141)));
}

countSort(ages, 0, 140);

for (i = 0; i < 100; i++) {
    document.write(ages[i] + "<br />");
}
```



## jq

The task description points out the disadvantage of using an array
to hold the counts, so in the following implementation, a JSON
object is used instead. This ensures the space requirement is just O(length). In jq, this approach is both time and space
efficient, except for the small cost of converting integers to strings, which is necessary because JSON keys must be strings.

```jq
def countingSort(min; max):
  . as $in
  | reduce range(0;length) as $i
      ( {};
        ($in[$i]|tostring) as $s | .[$s] += 1 # courtesy of the fact that in jq, (null+1) is 1
      )
  | . as $hash
  # now construct the answer:
  | reduce range(min; max+1) as $i
      ( [];
        ($i|tostring) as $s
        | if $hash[$s] == null then .
          else reduce range(0; $hash[$s]) as $j (.; . + [$i])
          end
      );
```

'''Example''':

```jq
 [1,2,1,4,0,10] | countingSort(0;10)
```

```sh

$ jq -M -c -n -f counting_sort.jq
[0,1,1,2,4,10]
```



## Julia

This is a translation of the pseudocode presented in the task description, accounting for the fact that Julia arrays start indexing at 1 rather than zero and taking care to return a result of the same type as the input.  Note that <code>cnt</code> has the machine's standard integer type (typically <code>Int64</code>), which need not match that of the input.


```julia
function countsort(a::Vector{<:Integer})
    lo, hi = extrema(a)
    b   = zeros(a)
    cnt = zeros(eltype(a), hi - lo + 1)
    for i in a cnt[i-lo+1] += 1 end
    z = 1
    for i in lo:hi
        while cnt[i-lo+1] > 0
            b[z] = i
            z += 1
            cnt[i-lo+1] -= 1
        end
    end
    return b
end

v = rand(UInt8, 20)
println("# unsorted bytes: $v\n -> sorted bytes: $(countsort(v))")
v = rand(1:2 ^ 10, 20)
println("# unsorted integers: $v\n -> sorted integers: $(countsort(v))")
```


```txt
# unsorted bytes: UInt8[0xcc, 0x67, 0x64, 0xbd, 0x74, 0x18, 0xd2, 0xf8, 0xf1, 0x6c, 0x3e, 0x7c, 0x90, 0x07, 0x48, 0x99, 0xb3, 0xf8, 0x8f, 0x23]
 -> sorted bytes: UInt8[0x07, 0x18, 0x23, 0x3e, 0x48, 0x64, 0x67, 0x6c, 0x74, 0x7c, 0x8f, 0x90, 0x99, 0xb3, 0xbd, 0xcc, 0xd2, 0xf1, 0xf8, 0xf8]
# unsorted integers: [634, 332, 756, 206, 971, 496, 962, 994, 795, 411, 981, 69, 366, 136, 227, 442, 731, 245, 179, 33]
 -> sorted integers: [33, 69, 136, 179, 206, 227, 245, 332, 366, 411, 442, 496, 634, 731, 756, 795, 962, 971, 981, 994]
```



## Kotlin


```scala
// version 1.1.0

fun countingSort(array: IntArray) {
    if (array.isEmpty()) return
    val min = array.min()!!
    val max = array.max()!!
    val count = IntArray(max - min + 1)  // all elements zero by default
    for (number in array) count[number - min]++
    var z = 0
    for (i in min..max)
        while (count[i - min] > 0) {
            array[z++] = i
            count[i - min]--
        }
}

fun main(args: Array<String>) {
    val array = intArrayOf(4, 65, 2, -31, 0, 99, 2, 83, 782, 1)
    println("Original : ${array.asList()}")
    countingSort(array)
    println("Sorted   : ${array.asList()}")
}
```


```txt

Original : [4, 65, 2, -31, 0, 99, 2, 83, 782, 1]
Sorted   : [-31, 0, 1, 2, 2, 4, 65, 83, 99, 782]

```



## Langur


```Langur
val .countingSort = f(.array) {
    val (.min, .max) = (min(.array), max(.array))

    var .count = arr(.max-.min+1, 0)
    for .i in .array {
        .count[.i-.min+1] += 1
    }
    var .result = []
    for .i of .count {
        for of .count[.i] {
            .result ~= [.i+.min-1]
        }
    }
    return .result
}

val .data = [7, 234, -234, 9, 43, 123, 14]

writeln "Original: ", .data
writeln "Sorted  : ", .countingSort(.data)
```


```txt
Original: [7, 234, -234, 9, 43, 123, 14]
Sorted  : [-234, 7, 9, 14, 43, 123, 234]
```



## Lua


```lua
function CountingSort( f )
    local min, max = math.min( unpack(f) ), math.max( unpack(f) )
    local count = {}
    for i = min, max do
        count[i] = 0
    end

    for i = 1, #f do
        count[ f[i] ] = count[ f[i] ] + 1
    end

    local z = 1
    for i = min, max do
        while count[i] > 0 do
            f[z] = i
            z = z + 1
            count[i] = count[i] - 1
        end
    end

end


f = { 15, -3, 0, -1, 5, 4, 5, 20, -8 }

CountingSort( f )

for i in next, f do
    print( f[i] )
end
```



## M4


```M4
divert(-1)

define(`randSeed',141592653)
define(`setRand',
   `define(`randSeed',ifelse(eval($1<10000),1,`eval(20000-$1)',`$1'))')
define(`rand_t',`eval(randSeed^(randSeed>>13))')
define(`random',
   `define(`randSeed',eval((rand_t^(rand_t<<18))&0x7fffffff))randSeed')

define(`set',`define(`$1[$2]',`$3')')
define(`get',`defn(`$1[$2]')')
define(`new',`set($1,size,0)')
define(`append',
   `set($1,size,incr(get($1,size)))`'set($1,get($1,size),$2)')
define(`deck',
   `new($1)for(`x',1,$2,
         `append(`$1',eval(random%$3))')')
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',incr($2),$3,`$4')')')')
define(`show',
   `for(`x',1,get($1,size),`get($1,x) ')')

define(`countingsort',
   `for(`x',$2,$3,`set(count,x,0)')`'for(`x',1,get($1,size),
      `set(count,get($1,x),incr(get(count,get($1,x))))')`'define(`z',
      1)`'for(`x',$2,$3,
         `for(`y',1,get(count,x),
            `set($1,z,x)`'define(`z',incr(z))')')')

divert
deck(`a',10,100)
show(`a')
countingsort(`a',0,99)
show(`a')
```



## Mathematica


```Mathematica
countingSort[list_] := Module[{minElem, maxElem, count, z, number},
  minElem = Min[list]; maxElem = Max[list];
  count = ConstantArray[0, (maxElem - minElem + 1)];
  For[number = 1, number < Length[list], number++,
   count[[number - minElem + 1]] = count[[number - minElem + 1]] + 1;] ;
  z = 1;
  For[i = minElem, i < maxElem, i++,
   While[count[[i - minElem + 1]] > 0,
    list[[z]] = i; z++;
    count[[i - minElem + 1]] = count[[i - minElem + 1]] - 1;]
   ];
  ]
```



```txt
countingSort@{2, 3, 1, 5, 7, 6}
->{1, 2, 3, 5, 6, 7}
```


=={{header|MATLAB}} / {{header|Octave}}==
This is a direct translation of the pseudo-code, except to compensate for MATLAB using 1 based arrays.


```MATLAB
function list = countingSort(list)

    minElem = min(list);
    maxElem = max(list);

    count = zeros((maxElem-minElem+1),1);

    for number = list
        count(number - minElem + 1) = count(number - minElem + 1) + 1;
    end

    z = 1;

    for i = (minElem:maxElem)
        while( count(i-minElem +1) > 0)
            list(z) = i;
            z = z+1;
            count(i - minElem + 1) = count(i - minElem + 1) - 1;
        end
    end

end %countingSort
```


Sample Usage:

```MATLAB
>>
 countingSort([4 3 1 5 6 2])

ans =

     1     2     3     4     5     6
```


## MAXScript


```MAXScript

fn countingSort arr =
(
	if arr.count < 2 do return arr
	local minVal = amin arr
	local maxVal = amax arr
	local count = for i in 1 to (maxVal-minVal+1) collect 0
	for i in arr do
	(
		count[i-minVal+1] = count[i-minVal+1] + 1
	)
	local z = 1
	for i = minVal to maxVal do
	(
		while (count[i-minVal+1]>0) do
		(
			arr[z] = i
			z += 1
			count[i-minVal+1] = count[i-minVal+1] - 1
		)

	)
	return arr
)
```

```MAXScript

a = for i in 1 to 15 collect random 1 30
#(7, 1, 6, 16, 27, 11, 24, 16, 25, 11, 22, 7, 28, 15, 17)
countingSort a
#(1, 6, 7, 7, 11, 11, 15, 16, 16, 17, 22, 24, 25, 27, 28)

```


=={{header|Modula-3}}==

```modula3
MODULE Counting EXPORTS Main;

IMPORT IO, Fmt;

VAR test := ARRAY [1..8] OF INTEGER {80, 10, 40, 60, 50, 30, 20, 70};

PROCEDURE Sort(VAR a: ARRAY OF INTEGER; min, max: INTEGER) =
  VAR range := max - min + 1;
      count := NEW(REF ARRAY OF INTEGER, range);
      z := 0;
  BEGIN
    FOR i := FIRST(count^) TO LAST(count^) DO
      count[i] := 0;
    END;

    FOR i := FIRST(a) TO LAST(a) DO
      INC(count[a[i] - min]);
    END;

    FOR i := min TO max DO
      WHILE (count[i - min] > 0) DO
        a[z] := i;
        INC(z);
        DEC(count[i - min]);
      END;
    END;
  END Sort;

BEGIN
  IO.Put("Unsorted: ");
  FOR i := FIRST(test) TO LAST(test) DO
    IO.Put(Fmt.Int(test[i]) & " ");
  END;
  IO.Put("\n");
  Sort(test, 10, 80);
  IO.Put("Sorted: ");
  FOR i := FIRST(test) TO LAST(test) DO
    IO.Put(Fmt.Int(test[i]) & " ");
  END;
  IO.Put("\n");
END Counting.
```

Output:

```txt

Unsorted: 80 10 40 60 50 30 20 70
Sorted: 10 20 30 40 50 60 70 80

```



## NetRexx


### Version 1

An almost direct implementation of the pseudocode.

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

import java.util.List

icounts = [int -
      1,   3,   6,   2,   7,  13,  20,  12,  21,  11 -
  ,  22,  10,  23,   9,  24,   8,  25,  43,  62,  42 -
  ,  63,  41,  18,  42,  17,  43,  16,  44,  15,  45 -
  ,  14,  46,  79, 113,  78, 114,  77,  39,  78,  38 -
]
scounts = int[icounts.length]

System.arraycopy(icounts, 0, scounts, 0, icounts.length)
lists = [ -
     icounts -
  ,  countingSort(scounts) -
]

loop ln = 0 to lists.length - 1
  cl = lists[ln]
  rep = Rexx('')
  loop ct = 0 to cl.length - 1
    rep = rep cl[ct]
    end ct
    say '['rep.strip.changestr(' ', ',')']'
  end ln

return

method getMin(array = int[]) public constant binary returns int

  amin = Integer.MAX_VALUE
  loop x_ = 0 to array.length - 1
    if array[x_] < amin then
      amin = array[x_]
    end x_

  return amin

method getMax(array = int[]) public constant binary returns int

  amax = Integer.MIN_VALUE
  loop x_ = 0 to array.length - 1
    if array[x_] > amax then
      amax = array[x_]
    end x_

  return amax

method countingSort(array = int[], amin = getMin(array), amax = getMax(array)) public constant binary returns int[]

  count = int[amax - amin + 1]
  loop nr = 0 to array.length - 1
    numbr = array[nr]
    count[numbr - amin] = count[numbr - amin] + 1
    end nr

  z_ = 0

  loop i_ = amin to amax
    loop label count while count[i_ - amin] > 0
      array[z_] = i_
      z_ = z_ + 1
      count[i_ - amin] = count[i_ - amin] - 1
      end count
    end i_

  return array

```

<pre style="overflow: scroll;">
[1,3,6,2,7,13,20,12,21,11,22,10,23,9,24,8,25,43,62,42,63,41,18,42,17,43,16,44,15,45,14,46,79,113,78,114,77,39,78,38]
[1,2,3,6,7,8,9,10,11,12,13,14,15,16,17,18,20,21,22,23,24,25,38,39,41,42,42,43,43,44,45,46,62,63,77,78,78,79,113,114]

```



### Version 2

A more Rexx-like (and shorter) version.  Due to NetRexx's built in indexed string capability, negative values are also easily supported.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method countingSort(icounts) public constant

  parse getMinMax(icounts) amin amax
  array = 0
  loop ix = 1 to icounts.words
    iw = icounts.word(ix) + 0
    array[iw] = array[iw] + 1
    end ix
  ocounts = ''
  loop ix = amin to amax
    if array[ix] = 0 then iterate ix
    loop for array[ix]
      ocounts = ocounts ix
      end
    end ix
  return ocounts.space

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method getMinMax(icounts) public constant

  amin = Long.MAX_VALUE
  amax = Long.MIN_VALUE
  loop x_ = 1 to icounts.words
    amin = icounts.word(x_).min(amin)
    amax = icounts.word(x_).max(amax)
    end x_

  return amin amax

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) public static
parse arg icounts
if icounts = '' then -
  icounts = -
    ' 1   3   6   2   7  13  20  12  21  11  22  10  23   9  24   8  25  43  62  42' -
    '63  41  18  42  17  43  16  44  15  45  14  46  79 113  78 114  77  39  78  38' -
    '0  -200 -6  -10 -0' -
    ''

say icounts.space
say countingSort(icounts)

return

```

```txt

1 3 6 2 7 13 20 12 21 11 22 10 23 9 24 8 25 43 62 42 63 41 18 42 17 43 16 44 15 45 14 46 79 113 78 114 77 39 78 38 0 -200 -6 -10 -0
-200 -10 -6 0 0 1 2 3 6 7 8 9 10 11 12 13 14 15 16 17 18 20 21 22 23 24 25 38 39 41 42 42 43 43 44 45 46 62 63 77 78 78 79 113 114

```



## Nim


```nim
proc countingSort[T](a: var openarray[T]; min, max: int) =
  let range = max - min + 1
  var count = newSeq[T](range)
  var z = 0

  for i in 0 .. < a.len: inc count[a[i] - min]

  for i in min .. max:
    for j in 0 ..  <count[i - min]:
      a[z] = i
      inc z

var a = @[5, 3, 1, 7, 4, 1, 1, 20]
countingSort(a, 1, 20)
echo a
```

Output:

```txt
@[1, 1, 1, 3, 4, 5, 7, 20]
```



## Objeck


```objeck

bundle Default {
  class Cocktail {
    function : Main(args : String[]) ~ Nil {
      values := [9, 7, 10, 2, 9, 7, 4, 3, 10, 2, 7, 10];
      CountingSort(values, 2, 10);
      each(i : values) {
        values[i]->PrintLine();
      };
    }

    function : CountingSort(array : Int[], min : Int, max : Int) ~ Nil {
      count := Int->New[max - min + 1];
      each(i : array) {
        number := array[i];
        v := count[number - min];
        count[number - min] := v + 1;
      };

      z := 0;
      for(i := min; i <= max; i += 1;) {
        while(count[i - min] > 0) {
          array[z] := i;
          z += 1;
          v := count[i - min]
          count[i - min] := v - 1;
        };
      };
    }
  }
}

```



## OCaml

For arrays:

```ocaml
let counting_sort_array arr lo hi =
  let count = Array.make (hi-lo+1) 0 in
    Array.iter (fun i -> count.(i-lo) <- count.(i-lo) + 1) arr;
    Array.concat (Array.to_list (Array.mapi (fun i x -> Array.make x (lo+i)) count))
```



## Octave

This implements the same algorithm but in a more compact way (using the same loop to count and to ''update'' the sorted vector). This implementation is ''elegant'' (and possible since the sort is not done "in place"), but not so efficient on machines that can't parallelize some operations (the vector <tt>arr</tt> is scanned for every value between <tt>minval</tt> and <tt>maxval</tt>)

```octave
function r = counting_sort(arr, minval, maxval)
  r = arr;
  z = 1;
  for i = minval:maxval
    cnt = sum(arr == i);
    while( cnt-- > 0 )
      r(z++) = i;
    endwhile
  endfor
endfunction
```


Testing:


```octave
ages = unidrnd(140, 100, 1);
sorted = counting_sort(ages, 0, 140);
disp(sorted);
```



## Oz

Using arrays as in the original algorithm. The implementation is slightly simpler because arrays can start with an arbitrary index in Oz.

```oz
declare
  proc {CountingSort Arr Min Max}
     Count = {Array.new Min Max 0}
     Z = {NewCell {Array.low Arr}}
  in
     %% fill frequency array
     for J in {Array.low Arr}..{Array.high Arr} do
        Number = Arr.J
     in
        Count.Number := Count.Number + 1
     end
     %% recreate array from frequencies
     for I in Min..Max do
        for C in 1..Count.I do
  	 Arr.(@Z) := I
  	 Z := @Z + 1
        end
     end
  end

  A = {Tuple.toArray unit(3 1 4 1 5 9 2 6 5)}
in
  {CountingSort A 1 9}
  {Show {Array.toRecord unit A}}
```


Using lists for input and output and a dictionary as a sparse array:

```oz
declare
  fun {CountingSort Xs}
     Count = {Dictionary.new}
  in
     for X in Xs do
        Count.X := {CondSelect Count X 0} + 1
     end
     {Concat {Map {Dictionary.entries Count} Repeat}}
  end

  fun {Repeat Val#Count}
     if Count == 0 then nil
     else Val|{Repeat Val#Count-1}
     end
  end

  fun {Concat Xs}
     {FoldR Xs Append nil}
  end
in
  {Show {CountingSort [3 1 4 1 5 9 2 6 5]}}
```



## PARI/GP


```parigp
countingSort(v,mn,mx)={
  my(u=vector(#v),i=0);
  for(n=mn,mx,
    for(j=1,#v,if(v[j]==n,u[i++]=n))
  );
  u
};
```



## Pascal


```pascal
program CountingSort;

procedure counting_sort(var arr : Array of Integer; n, min, max : Integer);
var
   count   : Array of Integer;
   i, j, z : Integer;
begin
   SetLength(count, max-min);
   for i := 0 to (max-min) do
      count[i] := 0;
   for i := 0 to (n-1) do
      count[ arr[i] - min ] := count[ arr[i] - min ] + 1;
   z := 0;
   for i := min to max do
      for j := 0 to (count[i - min] - 1) do begin
	 arr[z] := i;
	 z := z + 1
      end
end;

var
   ages	: Array[0..99] of Integer;
   i	: Integer;

begin
   { testing }
   for i := 0 to 99 do
      ages[i] := 139 - i;
   counting_sort(ages, 100, 0, 140);
   for i := 0 to 99 do
      writeln(ages[i]);
end.
```



## Perl



```perl
#! /usr/bin/perl
use strict;

sub counting_sort
{
    my ($a, $min, $max) = @_;

    my @cnt = (0) x ($max - $min + 1);
    $cnt[$_ - $min]++ foreach @$a;

    my $i = $min;
    @$a = map {($i++) x $_} @cnt;
}
```


Testing:


```perl
my @ages = map {int(rand(140))} 1 .. 100;

counting_sort(\@ages, 0, 140);
print join("\n", @ages), "\n";
```



## Perl 6

```perl6
sub counting-sort (@ints) {
    my $off = @ints.min;
    (my @counts)[$_ - $off]++ for @ints;
    flat @counts.kv.map: { ($^k + $off) xx ($^v // 0) }
}

# Testing:
constant @age-range = 2 .. 102;
my @ages = @age-range.roll(50);
say @ages.&counting-sort;
say @ages.sort;

say @ages.&counting-sort.join eq @ages.sort.join ?? 'ok' !! 'not ok';
```

```txt
(5 5 5 7 9 17 19 19 20 21 25 27 28 30 32 34 38 40 41 45 48 49 50 51 53 54 55 56 59 62 65 66 67 69 70 73 74 81 83 85 87 91 91 93 94 96 99 99 100 101)
(5 5 5 7 9 17 19 19 20 21 25 27 28 30 32 34 38 40 41 45 48 49 50 51 53 54 55 56 59 62 65 66 67 69 70 73 74 81 83 85 87 91 91 93 94 96 99 99 100 101)
ok

```



## Phix


```Phix
function countingSort(sequence array, integer mina, maxa)
sequence count = repeat(0,maxa-mina+1)
    for i=1 to length(array) do
        count[array[i]-mina+1] += 1
    end for
    integer z = 1
    for i=mina to maxa do
        for j=1 to count[i-mina+1] do
            array[z] := i
            z += 1
        end for
    end for
    return array
end function

sequence s = {5, 3, 1, 7, 4, 1, 1, 20}
?countingSort(s,min(s),max(s))
```

```txt

{1,1,1,3,4,5,7,20}

```



## PHP



```php
<?php

function counting_sort(&$arr, $min, $max)
{
  $count = array();
  for($i = $min; $i <= $max; $i++)
  {
    $count[$i] = 0;
  }

  foreach($arr as $number)
  {
    $count[$number]++;
  }
  $z = 0;
  for($i = $min; $i <= $max; $i++) {
    while( $count[$i]-- > 0 ) {
      $arr[$z++] = $i;
    }
  }
}
```


Testing:


```php
$ages = array();
for($i=0; $i < 100; $i++) {
  array_push($ages, rand(0, 140));
}
counting_sort($ages, 0, 140);

for($i=0; $i < 100; $i++) {
  echo $ages[$i] . "\n";
}
?>
```



## PicoLisp


```PicoLisp
(de countingSort (Lst Min Max)
   (let Count (need (- Max Min -1) 0)
      (for N Lst
         (inc (nth Count (- N Min -1))) )
      (make
         (map
            '((C I)
               (do (car C) (link (car I))) )
            Count
            (range Min Max) ) ) ) )
```

Output:


```txt
: (countingSort (5 3 1 7 4 1 1 20) 1 20)
-> (1 1 1 3 4 5 7 20)
```



## PL/I


```PL/I
count_sort: procedure (A);
   declare A(*) fixed;
   declare (min, max) fixed;
   declare i fixed binary;

   max, min = A(lbound(A,1));
   do i = 1 to hbound(A,1);
       if max < A(i) then max = A(i);
       if min > A(i) then min = A(i);
   end;

   begin;
      declare t(min:max) fixed;
      declare (i, j, k) fixed binary (31);
      t = 0;
      do i = 1 to hbound(A,1);
         j = A(i);
         t(j) = t(j) + 1;
      end;
      k = lbound(A,1);
      do i = min to max;
         if t(i) ^= 0 then
            do j = 1 to t(i);
               A(k) = i;
               k = k + 1;
            end;
      end;
   end;
end count_sort;
```



## PowerShell


```PowerShell

function countingSort($array) {
    $minmax = $array | Measure-Object -Minimum -Maximum
    $min, $max = $minmax.Minimum, $minmax.Maximum
    $count = @(0) * ($max - $min  + 1)
    foreach ($number in $array) {
        $count[$number - $min] = $count[$number - $min] + 1
    }
    $z = 0
    foreach ($i in $min..$max) {
        while (0 -lt $count[$i - $min]) {
            $array[$z] = $i
            $z = $z+1
            $count[$i - $min] = $count[$i - $min] - 1
        }
    }
    $array
}

$array = foreach ($i in 1..50) {Get-Random -Minimum 0 -Maximum 26}
"$array"
"$(countingSort $array)"

```

<b>Output:</b>

```txt

13 18 8 6 3 7 22 20 10 7 18 10 25 13 9 21 8 19 24 24 18 6 23 23 24 7 15 25 24 25 11 23 19 5 4 8 9 7 1 19 10 24 13 1 9 0 9 10 19 16
0 1 1 3 4 5 6 6 7 7 7 7 8 8 8 9 9 9 9 10 10 10 10 11 13 13 13 15 16 18 18 18 19 19 19 19 20 21 22 23 23 23 24 24 24 24 24 25 25 25

```



## PureBasic


```PureBasic
Procedure Counting_sort(Array data_array(1), min, max)
  Define i, j
  Dim c(max - min)

  For i = 0 To ArraySize(data_array())
    c(data_array(i) - min) + 1
  Next

  For i = 0 To ArraySize(c())
    While c(i)
      data_array(j) = i + min
      j + 1
      c(i) - 1
    Wend
  Next
EndProcedure
```



## Python

Follows the spirit of the counting sort but uses Pythons defaultdict(int) to initialize array accesses to zero, and list concatenation:

```python
>>>
 from collections import defaultdict
>>> def countingSort(array, mn, mx):
	count = defaultdict(int)
	for i in array:
		count[i] += 1
	result = []
	for j in range(mn,mx+1):
		result += [j]* count[j]
	return result

>>> data = [9, 7, 10, 2, 9, 7, 4, 3, 10, 2, 7, 10, 2, 1, 3, 8, 7, 3, 9, 5, 8, 5, 1, 6, 3, 7, 5, 4, 6, 9, 9, 6, 6, 10, 2, 4, 5, 2, 8, 2, 2, 5, 2, 9, 3, 3, 5, 7, 8, 4]
>>> mini,maxi = 1,10
>>> countingSort(data, mini, maxi) == sorted(data)
True
```


Using a list:
```python
def countingSort(a, min, max):
    cnt = [0] * (max - min + 1)
    for x in a:
        cnt[x - min] += 1

    return [x for x, n in enumerate(cnt, start=min)
              for i in xrange(n)]
```



## R

```R
counting_sort <- function(arr, minval, maxval) {
  r <- arr
  z <- 1
  for(i in minval:maxval) {
    cnt = sum(arr == i)
    while(cnt > 0) {
      r[z] = i
      z <- z + 1
      cnt <- cnt - 1
    }
  }
  r
}

# 140+1 instead of 140, since random numbers generated
# by runif are always less than the given maximum;
# floor(a number at most 140.9999...) is 140
ages <- floor(runif(100, 0, 140+1))
sorted <- counting_sort(ages, 0, 140)
print(sorted)
```



## Racket


```racket

#lang racket

(define (counting-sort xs min max)
  (define ns (make-vector (+ max (- min) 1) 0))
  (for ([x xs])  (vector-set! ns (- x min) (+ (vector-ref ns (- x min)) 1)))
  (for/fold ([i 0]) ([n ns] [x (in-naturals)])
    (for ([j (in-range i (+ i n ))])
      (vector-set! xs j (+ x min)))
    (+ i n))
  xs)

(counting-sort (vector 0 9 3 8 1 -1 1 2 3 7 4) -1 10)

```

Output:

```racket

'#(-1 0 1 1 2 3 3 4 7 8 9)

```



## REXX

These REXX versions make use of ''sparse'' arrays.

Negative, zero, and positive integers are supported.

### version 1


```rexx
/*REXX pgm sorts an array of integers (can be negative) using the  count─sort algorithm.*/
$=1 3 6 2 7 13 20 12 21 11 22 10 23 9 24 8 25 43 62 42 63 41 18 42 17 43 16 44 15 45 14 46 79 113 78 114 77 39 78 38
#= words($);     w= length(#);        _.= 0      /* [↑]  a list of some Recaman numbers.*/
m= 0;            LO= word($, 1);      HI= LO     /*M:  max width of any number in  @.   */
     do i=1  for #;  z= word($, i);   @.i= z;  m= max(m, length(z)) /*get from $ list.  */
     _.z= _.z + 1;   LO= min(LO, z);  HI= max(HI, z)                /*find the  LO & HI.*/
     end   /*i*/
                                                 /*W:  max index width for the  @. array*/
call show 'before sort: '                        /*show the   before   array elements.  */
           say  copies('▒', 55)                  /*show a separator line (before/after).*/
call countSort  #                                /*sort a number of entries of @. array.*/
call show ' after sort: '                        /*show the    after   array elements.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
countSort: parse arg N;  x= 1;    do k=LO  to  HI;    do x=x  for _.k;  @.x= k;  end /*x*/
                                  end   /*k*/
           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: do s=1  for #;  say right("element",20) right(s,w) arg(1) right(@.s,m); end;  return
```

(Shown at   <big>'''<sup>5</sup>/<sub>6</sub>'''</big>   size.)

<pre style="font-size:84%;height:140ex">
             element  1 before sort:    1
             element  2 before sort:    3
             element  3 before sort:    6
             element  4 before sort:    2
             element  5 before sort:    7
             element  6 before sort:   13
             element  7 before sort:   20
             element  8 before sort:   12
             element  9 before sort:   21
             element 10 before sort:   11
             element 11 before sort:   22
             element 12 before sort:   10
             element 13 before sort:   23
             element 14 before sort:    9
             element 15 before sort:   24
             element 16 before sort:    8
             element 17 before sort:   25
             element 18 before sort:   43
             element 19 before sort:   62
             element 20 before sort:   42
             element 21 before sort:   63
             element 22 before sort:   41
             element 23 before sort:   18
             element 24 before sort:   42
             element 25 before sort:   17
             element 26 before sort:   43
             element 27 before sort:   16
             element 28 before sort:   44
             element 29 before sort:   15
             element 30 before sort:   45
             element 31 before sort:   14
             element 32 before sort:   46
             element 33 before sort:   79
             element 34 before sort:  113
             element 35 before sort:   78
             element 36 before sort:  114
             element 37 before sort:   77
             element 38 before sort:   39
             element 39 before sort:   78
             element 40 before sort:   38
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
             element  1  after sort:    1
             element  2  after sort:    2
             element  3  after sort:    3
             element  4  after sort:    6
             element  5  after sort:    7
             element  6  after sort:    8
             element  7  after sort:    9
             element  8  after sort:   10
             element  9  after sort:   11
             element 10  after sort:   12
             element 11  after sort:   13
             element 12  after sort:   14
             element 13  after sort:   15
             element 14  after sort:   16
             element 15  after sort:   17
             element 16  after sort:   18
             element 17  after sort:   20
             element 18  after sort:   21
             element 19  after sort:   22
             element 20  after sort:   23
             element 21  after sort:   24
             element 22  after sort:   25
             element 23  after sort:   38
             element 24  after sort:   39
             element 25  after sort:   41
             element 26  after sort:   42
             element 27  after sort:   42
             element 28  after sort:   43
             element 29  after sort:   43
             element 30  after sort:   44
             element 31  after sort:   45
             element 32  after sort:   46
             element 33  after sort:   62
             element 34  after sort:   63
             element 35  after sort:   77
             element 36  after sort:   78
             element 37  after sort:   78
             element 38  after sort:   79
             element 39  after sort:  113
             element 40  after sort:  114

```



### version 2

```rexx
/* REXX ---------------------------------------------------------------
* 13.07.2014 Walter Pachl translated from PL/I
*--------------------------------------------------------------------*/
alist='999 888 777 1 5 13 15 17 19 21 5'
Parse Var alist lo hi .
Do i=1 By 1 While alist<>''
  Parse Var alist a.i alist;
  lo=min(lo,a.i)
  hi=max(hi,a.i)
  End
a.0=i-1

Call show 'before count_sort'
Call count_sort
Call show 'after count_sort'
Exit

count_sort: procedure Expose a. lo hi
  t.=0
  do i=1 to a.0
    j=a.i
    t.j=t.j+1
    end
  k=1
  do i=lo to hi
    if t.i<>0 then Do
      do j=1 to t.i
        a.k=i
        k=k+1
        end
      end
    end
  Return

show: Procedure Expose a.
Parse Arg head
Say head
ol=''
Do i=1 To a.0
  ol=ol right(a.i,3)
  End
Say ol
Return
```

'''Output:'''

```txt
before count_sort
 999 888 777   1   5  13  15  17  19  21   5
after count_sort
   1   5   5  13  15  17  19  21 777 888 999
```



## Ring


```ring

aList = [4, 65, 2, 99, 83, 782, 1]
see countingSort(aList, 1, 782)

func countingSort f, min, max
     count = list(max-min+1)
     for i = min to max
         count[i] = 0
     next

     for i = 1 to len(f)
         count[ f[i] ] = count[ f[i] ] + 1
     next

     z = 1
     for i = min to max
         while count[i] > 0
               f[z] = i
               z = z + 1
               count[i] = count[i] - 1
         end
     next
     return f

```



## Ruby


```ruby
class Array
  def counting_sort!
    replace counting_sort
  end

  def counting_sort
    min, max = minmax
    count = Array.new(max - min + 1, 0)
    each {|number| count[number - min] += 1}
    (min..max).each_with_object([]) {|i, ary| ary.concat([i] * count[i - min])}
  end
end

ary = [9,7,10,2,9,7,4,3,10,2,7,10,2,1,3,8,7,3,9,5,8,5,1,6,3,7,5,4,6,9,9,6,6,10,2,4,5,2,8,2,2,5,2,9,3,3,5,7,8,4]
p ary.counting_sort.join(",")
# => "1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,5,5,5,5,5,5,6,6,6,6,7,7,7,7,7,7,8,8,8,8,9,9,9,9,9,9,10,10,10,10"

p ary = Array.new(20){rand(-10..10)}
# => [-3, -1, 9, -6, -8, -3, 5, -7, 4, 0, 5, 0, 2, -2, -6, 10, -10, -7, 5, -7]
p ary.counting_sort
# => [-10, -8, -7, -7, -7, -6, -6, -3, -3, -2, -1, 0, 0, 2, 4, 5, 5, 5, 9, 10]
```



## Rust



```rust
fn counting_sort(
    mut data: Vec<usize>,
    min: usize,
    max: usize,
) -> Vec<usize> {
    // create and fill counting bucket with 0
    let mut count: Vec<usize> = Vec::with_capacity(data.len());
    count.resize(data.len(), 0);

    for num in &data {
        count[num - min] = count[num - min] + 1;
    }
    let mut z: usize = 0;
    for i in min..max+1 {
        while count[i - min] > 0 {
            data[z] = i;
            z += 1;
            count[i - min] = count[i - min] - 1;
        }
    }

    data
}

fn main() {
    let arr1 = vec![1, 0, 2, 9, 3, 8, 4, 7, 5, 6];
    println!("{:?}", counting_sort(arr1, 0, 9));

    let arr2 = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    println!("{:?}", counting_sort(arr2, 0, 9));

    let arr3 = vec![10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0];
    println!("{:?}", counting_sort(arr3, 0, 10));
}
```

```txt

[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

```



## Scala


```scala
def countSort(input: List[Int], min: Int, max: Int): List[Int] =
  input.foldLeft(Array.fill(max - min + 1)(0)) { (arr, n) =>
    arr(n - min) += 1
    arr
  }.zipWithIndex.foldLeft(List[Int]()) {
    case (lst, (cnt, ndx)) => List.fill(cnt)(ndx + min) ::: lst
  }.reverse
```


It's better (i.e. slightly faster) to reverse the frequencies list before processing it, instead of the whole result

```scala
def countSort(input: List[Int], min: Int, max: Int): List[Int] =
  input.foldLeft(Array.fill(max - min + 1)(0)) { (arr, n) =>
    arr(n - min) += 1
    arr
  }.zipWithIndex.reverse.foldLeft(List[Int]()) {
    case (lst, (cnt, ndx)) => List.fill(cnt)(ndx + min) ::: lst
  }
```



## Sidef


```ruby
func counting_sort(a, min, max) {
    var cnt = ([0] * (max - min + 1))
    a.each {|i| cnt[i-min]++ }
    cnt.map {|i| [min++] * i }.flat
}
 
var a = 100.of { 100.irand }
say counting_sort(a, 0, 100)
```



## Slate



```slate
s@(Sequence traits) countingSort &min: min &max: max
[| counts index |
  min `defaultsTo: (s reduce: #min: `er).
  max `defaultsTo: (s reduce: #max: `er).
  counts: ((0 to: max - min) project: [| :_ | 0]).
  s do: [| :value | counts at: value - min infect: [| :count | count + 1]].
  index: 0.
  min to: max do: [| :value |
    [(counts at: value - min) isPositive]
      whileTrue:
        [s at: index put: value.
         index: index + 1.
         counts at: value - min infect: [| :val | val - 1]]
  ].
  s
].
```



## Smalltalk

```smalltalk
OrderedCollection extend [
    countingSortWithMin: min andMax: max [
	|oc z|
	oc := OrderedCollection new.
	1 to: (max - min + 1) do: [ :n| oc add: 0 ].
	self do: [ :v |
	    oc at: (v - min + 1) put: ( (oc at: (v - min + 1)) + 1)
	].
	z := 1.
	min to: max do: [ :i |
	    1 to: (oc at: (i - min + 1)) do: [ :k |
		self at: z put: i.
		z := z + 1.
	    ]
	]
    ]
].
```


Testing:


```smalltalk
|ages|

ages := OrderedCollection new.

1 to: 100 do: [ :n |
    ages add: (Random between: 0 and: 140)
].

ages countingSortWithMin: 0 andMax: 140.
ages printNl.
```



## Tcl

```tcl
proc countingsort {a {min ""} {max ""}} {
    # If either of min or max weren't given, compute them now
    if {$min eq ""} {
        set min [::tcl::mathfunc::min $a]
    }
    if {$max eq ""} {
        set max [::tcl::mathfunc::max $a]
    }

    # Make the "array" of counters
    set count [lrepeat [expr {$max - $min + 1}] 0]

    # Count the values in the input list
    foreach n $a {
        set idx [expr {$n - $min}]
        lincr count $idx
    }

    # Build the output list
    set z 0
    for {set i $min} {$i <= $max} {incr i} {
        set idx [expr {$i - $min}]
        while {[lindex $count $idx] > 0} {
            lset a $z $i
            incr z
            lincr count $idx -1
        }
    }
    return $a
}

# Helper that will increment an existing element of a list
proc lincr {listname idx {value 1}} {
    upvar 1 $listname list
    lset list $idx [expr {[lindex $list $idx] + $value}]
}

# Demo code
for {set i 0} {$i < 50} {incr i} {lappend a [expr {1+ int(rand()*10)}]}
puts $a
puts [countingsort $a]
```


```txt
9 7 10 2 9 7 4 3 10 2 7 10 2 1 3 8 7 3 9 5 8 5 1 6 3 7 5 4 6 9 9 6 6 10 2 4 5 2 8 2 2 5 2 9 3 3 5 7 8 4
1 1 2 2 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 5 5 5 5 5 5 6 6 6 6 7 7 7 7 7 7 8 8 8 8 9 9 9 9 9 9 10 10 10 10

```



## VBA

```vb
Option Base 1
Private Function countingSort(array_ As Variant, mina As Long, maxa As Long) As Variant
    Dim count() As Integer
    ReDim count(maxa - mina + 1)
    For i = 1 To UBound(array_)
        count(array_(i) - mina + 1) = count(array_(i) - mina + 1) + 1
    Next i
    Dim z As Integer: z = 1
    For i = mina To maxa
        For j = 1 To count(i - mina + 1)
            array_(z) = i
            z = z + 1
        Next j
    Next i
    countingSort = array_
End Function

Public Sub main()
    s = [{5, 3, 1, 7, 4, 1, 1, 20}]
    Debug.Print Join(countingSort(s, WorksheetFunction.Min(s), WorksheetFunction.Max(s)), ", ")
End Sub
```
```txt
1, 1, 1, 3, 4, 5, 7, 20
```


## VBScript

All my other sort demos just pass in the array, thus the findMax and findMin


### ==Implementation==


```vb
function findMax( a )
	dim num
	dim max
	max = 0
	for each num in a
		if num > max then max = num
	next
	findMax = max
end function

function findMin( a )
	dim num
	dim min
	min = 0
	for each num in a
		if num < min then min = num
	next
	findMin = min
end function

'the function returns the sorted array, but the fact is that VBScript passes the array by reference anyway
function countingSort( a )
	dim count()
	dim min, max
	min = findMin(a)
	max = findMax(a)
	redim count( max - min + 1 )
	dim i
	dim z
	for i = 0 to ubound( a )
		count( a(i) - min ) = count( a( i ) - min ) + 1
	next
	z = 0
	for i = min to max
		while count( i - min) > 0
			a(z) = i
			z = z + 1
			count( i - min ) = count( i - min ) - 1
		wend
	next
	countingSort = a
end function
```



### ==Invocation==


```vb
dim a
a = array(300, 1, -2, 3, -4, 5, -6, 7, -8, 100, 11 )
wscript.echo join( a, ", " )
countingSort a
wscript.echo join( a, ", " )
```



### ==Output==


```txt

300, 1, -2, 3, -4, 5, -6, 7, -8, 100, 11
-8, -6, -4, -2, 1, 3, 5, 7, 11, 100, 300

```



## XPL0


```XPL0
include c:\cxpl\codes;

proc CountingSort(Array, Min, Max, Size); \Sort Array
int  Array, Min, Max, Size;     \minimum, maximum values, number of elements
int  Count, I, Z;
[Count:= Reserve((Max-Min+1)*4);         \Reserve Count with 4 bytes per integer
for I:= 0 to Max-Min do Count(I):= 0;    \initialize Count with 0
for I:= 0 to Size-1 do                   \for each number count its occurrences
    Count(Array(I)-Min):= Count(Array(I)-Min) + 1;
Z:= 0;
for I:= Min to Max do
    while Count(I-Min) > 0 do
        [Array(Z):= I;
        Z:= Z+1;
        Count(I-Min):= Count(I-Min) - 1;
        ];
];

int A, I;
[A:= [3, 1, 4, 1, -5, 9, 2, 6, 5, 4];
CountingSort(A, -5, 9, 10);
for I:= 0 to 10-1 do [IntOut(0, A(I));  ChOut(0, ^ )];
]
```


```txt

-5 1 1 2 3 4 4 5 6 9

```



## zkl


```zkl
fcn countingSort(array, min, max){  // modifies array
   count:=(max - min + 1).pump(List().write,0); // array of (max - min + 1) zeros
   foreach number in (array){
      count[number - min] += 1;
   }
   z:=-1;
   foreach i in ([min .. max]){
      do(count[i - min]){ array[z += 1] = i }
   }
   array
}
```


```zkl
array:=List(4, 65, 2, -31, 0, 99, 2, 83, 182, 1);
countingSort(array,(0).min(array), (0).max(array)).println();
```

```txt
L(-31,0,1,2,2,4,65,83,99,182)
```



