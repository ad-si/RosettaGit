+++
title = "Sorting algorithms/Radix sort"
description = ""
date = 2019-07-30T13:51:02Z
aliases = []
[extra]
id = 9169
[taxonomies]
categories = []
tags = []
+++

{{task|Sorting Algorithms}}
{{Sorting Algorithm}}


;Task:
Sort an integer array with the   [[wp:Radix sort|radix sort algorithm]]. 

The primary purpose is to complete the characterization of sort algorithms task.





## Ada


radix_sort.adb:

```Ada
with Ada.Text_IO;
procedure Radix_Sort is
   type Integer_Array is array (Positive range <>) of Integer;

   procedure Least_Significant_Radix_Sort (Data : in out Integer_Array; Base : Positive := 10) is
      type Bucket is record
         Count   : Natural := 0;
         Content : Integer_Array (Data'Range);
      end record;

      subtype Bucket_Index is Integer range -Base + 1 .. Base - 1;
      type Bucket_Array is array (Bucket_Index) of Bucket;

      procedure Append (To : in out Bucket; Item : Integer) is
      begin
         To.Count := To.Count + 1;
         To.Content (To.Count) := Item;
      end Append;

      function Get_Nth_Digit (Value : Integer; N : Positive) return Integer is
         Result : Integer := (Value / (Base ** (N - 1))) mod Base;
      begin
         if Value < 0 then
            Result := -Result;
         end if;
         return Result;
      end Get_Nth_Digit;

      function Get_Maximum return Natural is
         Result : Natural := 0;
      begin
         for I in Data'Range loop
            if abs (Data (I)) > Result then
               Result := abs (Data (I));
            end if;
         end loop;
         return Result;
      end Get_Maximum;

      function Split (Pass : Positive) return Bucket_Array is
         Buckets : Bucket_Array;
      begin
         for I in Data'Range loop
            Append (To   => Buckets (Get_Nth_Digit (Data (I), Pass)),
                    Item => Data (I));
         end loop;
         return Buckets;
      end Split;

      function Merge (Buckets : Bucket_Array) return Integer_Array is
         Result        : Integer_Array (Data'Range);
         Current_Index : Positive := 1;
      begin
         for Sublist in Buckets'Range loop
            for Item in 1 .. Buckets (Sublist).Count loop
               Result (Current_Index) := Buckets (Sublist).Content (Item);
               Current_Index := Current_Index + 1;
            end loop;
         end loop;
         return Result;
      end Merge;

      Max_Number  : Natural := Get_Maximum;
      Digit_Count : Positive := 1;
   begin
      -- count digits of biggest number
      while Max_Number > Base loop
         Digit_Count := Digit_Count + 1;
         Max_Number := Max_Number / Base;
      end loop;
      for Pass in 1 .. Digit_Count loop
         Data := Merge (Split (Pass));
      end loop;
   end Least_Significant_Radix_Sort;

   Test_Array : Integer_Array := (170, 45, 75, -90, -802, 24, 2, 66);
begin
   Least_Significant_Radix_Sort (Test_Array, 4);
   for I in Test_Array'Range loop
      Ada.Text_IO.Put (Integer'Image (Test_Array (I)));
   end loop;
   Ada.Text_IO.New_Line;
end Radix_Sort;
```


output:

```txt
-802-90 2 24 45 66 75 170
```





## ALGOL 68


```algol68
PROC radixsort = (REF []INT array) VOID:
(
    [UPB array]INT zero;  
    [UPB array]INT one;   
    BITS mask := 16r01;  
    INT zero_index  := 0, 
        one_index   := 0,
        array_index := 1; 

    WHILE ABS(mask) > 0 DO 
        WHILE array_index <= UPB array DO 
            IF (BIN(array[array_index]) AND mask) = 16r0 THEN 
                zero_index +:= 1;
                zero[zero_index] := array[array_index]
            ELSE            
                one_index +:= 1;
                one[one_index] := array[array_index]
            FI;
            array_index +:= 1 
        OD;
        
        array_index := 1; 
        FOR i FROM 1 TO zero_index DO 
            array[array_index] := zero[i];
            array_index +:= 1
        OD;

        FOR i FROM 1 TO one_index DO 
            array[array_index] := one[i];
            array_index +:=1
        OD;
        
        array_index := 1;
        zero_index := one_index := 0;
        mask := mask SHL 1 
    OD
);

main:
(
    [10]INT a;
    FOR i FROM 1 TO UPB a DO
        a[i] := ROUND(random*1000)
    OD;
    
    print(("Before:", a));
    print((newline, newline));
    radixsort(a);
    print(("After: ", a))
)
```

{{out}}

```txt

Before:       +459       +941       +623       +386       +263       +766       +129       +554       +160       +328
                                                                                                                     
After:        +129       +160       +263       +328       +386       +459       +554       +623       +766       +941

```



## AutoHotkey


```AutoHotkey
Radix_Sort(data){
	loop, parse, data, `,
		n := StrLen(A_LoopField)>n?StrLen(A_LoopField):n
	loop % n {
		bucket := []	,	i := A_Index
		loop, parse, data, `,
			bucket[SubStr(A_LoopField,1-i)] .= (bucket[SubStr(A_LoopField,1-i)]?",":"") A_LoopField
		data := ""
		for i, v in bucket
			data .= (data?",":"") v
	}
	return data
}
```

Examples:
```AutoHotkey
d = 170,45,75,90,802,2,24,66
MsgBox, 262144, , % Radix_Sort(d)
```

Outputs:
```txt
2,24,45,66,75,90,170,802
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
The array index is assumed to start at zero.  The third parameter of PROCradixsort() is the radix used.

```bbcbasic
      DIM test%(9)
      test%() = 4, 65, 2, -31, 0, 99, 2, 83, 782, 1
      PROCradixsort(test%(), 10, 10)
      FOR i% = 0 TO 9
        PRINT test%(i%) ;
      NEXT
      PRINT
      END
      
      DEF PROCradixsort(a%(), n%, r%)
      LOCAL d%, e%, i%, l%, m%, b%(), bucket%()
      DIM b%(n%-1), bucket%(r%-1)
      FOR i% = 0 TO n%-1
        IF a%(i%) < l% l% = a%(i%)
        IF a%(i%) > m% m% = a%(i%)
      NEXT
      a%() -= l%
      m% -= l%
      e% = 1
      WHILE m% DIV e%
        bucket%() = 0
        FOR i% = 0 TO n%-1
          bucket%(a%(i%) DIV e% MOD r%) += 1
        NEXT
        FOR i% = 1 TO r%-1
          bucket%(i%) += bucket%(i% - 1)
        NEXT
        FOR i% = n%-1 TO 0 STEP -1
          d% = a%(i%) DIV e% MOD r%
          bucket%(d%) -= 1
          b%(bucket%(d%)) = a%(i%)
        NEXT
        a%() = b%()
        e% *= r%
      ENDWHILE
      a%() += l%
      ENDPROC
```

'''Output:'''

```txt

       -31         0         1         2         2         4        65        83        99       782

```



## C

Radix sort, "digits" are most significant bits.
```c>#include <stdio.h

#include <limits.h>
#include <stdlib.h>
#include <time.h>

// Get size of statically allocated array
#define ARR_LEN(ARR) (sizeof ARR / sizeof *ARR)
// Generate random number in the interval [M,N]
#define RAND_RNG(M,N) (M + rand() / (RAND_MAX / (N - M + 1) + 1));

static void swap(unsigned *a, unsigned *b) {
    unsigned tmp = *a;
    *a = *b;
    *b = tmp;
}

/* sort unsigned ints */
static void rad_sort_u(unsigned *from, unsigned *to, unsigned bit)
{
	if (!bit || to < from + 1) return;

	unsigned *ll = from, *rr = to - 1;
	for (;;) {
		/* find left most with bit, and right most without bit, swap */
		while (ll < rr && !(*ll & bit)) ll++;
		while (ll < rr &&  (*rr & bit)) rr--;
		if (ll >= rr) break;
		swap(ll, rr);
	}

	if (!(bit & *ll) && ll < to) ll++;
	bit >>= 1;

	rad_sort_u(from, ll, bit);
	rad_sort_u(ll, to, bit);
}

/* sort signed ints: flip highest bit, sort as unsigned, flip back */
static void radix_sort(int *a, const size_t len)
{
	size_t i;
	unsigned *x = (unsigned*) a;

	for (i = 0; i < len; i++) 
            x[i] ^= INT_MIN;

        rad_sort_u(x, x + len, INT_MIN);

        for (i = 0; i < len; i++) 
            x[i] ^= INT_MIN;
}

int main(void)
{
        
    srand(time(NULL));
    int x[16];

     for (size_t i = 0; i < ARR_LEN(x); i++) 
        x[i] = RAND_RNG(-128,127)

    radix_sort(x, ARR_LEN(x));

    for (size_t i = 0; i < ARR_LEN(x); i++) 
        printf("%d%c", x[i], i + 1 < ARR_LEN(x) ? ' ' : '\n');
}
```
output
```txt
-182 -175 -151 -141 -70 -51 -20 -5 -1 41 70 103 171 198 227 242
```



## C++

Implements a least significant digit radix sort and a recursive most significant digit radix sort.

Note: the LSD radix sort uses the standard library '''std::stable_partition''' algorithm. This algorithm is guaranteed to preserve relative order and has a higher runtime cost. The MSD radix sort uses '''std::partition''' and can be significantly faster.

```cpp>#include <algorithm

#include <iostream>
#include <iterator>

// Radix sort comparator for 32-bit two's complement integers
class radix_test
{
    const int bit; // bit position [0..31] to examine
public:
    radix_test(int offset) : bit(offset) {} // constructor

    bool operator()(int value) const // function call operator
    {
        if (bit == 31) // sign bit
            return value < 0; // negative int to left partition
        else
            return !(value & (1 << bit)); // 0 bit to left partition
    }
};

// Least significant digit radix sort
void lsd_radix_sort(int *first, int *last)
{
    for (int lsb = 0; lsb < 32; ++lsb) // least-significant-bit
    {
        std::stable_partition(first, last, radix_test(lsb));
    }
}

// Most significant digit radix sort (recursive)
void msd_radix_sort(int *first, int *last, int msb = 31)
{
    if (first != last && msb >= 0)
    {
        int *mid = std::partition(first, last, radix_test(msb));
        msb--; // decrement most-significant-bit
        msd_radix_sort(first, mid, msb); // sort left partition
        msd_radix_sort(mid, last, msb); // sort right partition
    }
}

// test radix_sort
int main()
{
    int data[] = { 170, 45, 75, -90, -802, 24, 2, 66 };

    lsd_radix_sort(data, data + 8);
    // msd_radix_sort(data, data + 8);

    std::copy(data, data + 8, std::ostream_iterator<int>(std::cout, " "));

    return 0;
}
```

Output:

```txt
-802 -90 2 24 45 66 75 170 
```


=={{header|C sharp|C#}}==
{{works with|C sharp|C#|3.0+}}

```csharp
using System;

namespace RadixSort
{
    class Program
    {
        static void Sort(int[] old)
        {
            int i, j;
            int[] tmp = new int[old.Length];
            for (int shift = 31; shift > -1; --shift)
            {
                j = 0;
                for (i = 0; i < old.Length; ++i)
                {
                    bool move = (old[i] << shift) >= 0;
                    if (shift == 0 ? !move : move)  // shift the 0's to old's head
                        old[i-j] = old[i];
                    else                            // move the 1's to tmp
                        tmp[j++] = old[i];
                }
                Array.Copy(tmp, 0, old, old.Length-j, j);
            }
        }
        static void Main(string[] args)
        {
            int[] old = new int[] { 2, 5, 1, -3, 4 };
            Console.WriteLine(string.Join(", ", old));
            Sort(old);
            Console.WriteLine(string.Join(", ", old));
            Console.Read();
        }
    }
}
```



## D


### Shorter Version


```d
import std.stdio, std.math, std.traits, std.range, std.algorithm;

ElementType!R[] radixSort(size_t N=10, R)(R r)
if (hasLength!R && isRandomAccessRange!R &&
    isIntegral!(ElementType!R)) {
    alias ElementType!R E;

    static if (isDynamicArray!R)
        alias r res;         // input is array => in place sort
    else
        E[] res = r.array(); // input is Range => return a new array

    E absMax = r.map!abs().reduce!max();
    immutable nPasses = 1 + cast(int)(log(absMax) / log(N));

    foreach (pass; 0 .. nPasses) {
        auto bucket = new E[][](2 * N - 1, 0);
        foreach (v; res) {
            int bIdx = abs(v / (N ^^ pass)) % N;
            bIdx = (v < 0) ? -bIdx : bIdx;
            bucket[N + bIdx - 1] ~= v;
        }
        res = bucket.join();
    }

    return res;
}

void main() {
    auto items = [170, 45, 75, -90, 2, 24, -802, 66];
    items.radixSort().writeln();
    items.map!q{1 - a}().radixSort().writeln();
}
```

{{out}}

```txt
[-802, -90, 2, 24, 45, 66, 75, 170]
[-1, -23, -44, -65, -74, -169, 91, 803]
```



### More Efficient Version


```d
import std.array, std.traits;

// considered pure for this program
extern(C) void* alloca(in size_t length) pure nothrow;

void radixSort(size_t MAX_ALLOCA=5_000, U)(U[] data)
pure nothrow if (isUnsigned!U) {
    static void radix(in uint byteIndex, in U[] source, U[] dest)
    pure nothrow {
        immutable size_t sourceSize = source.length;
        ubyte* curByte = (cast(ubyte*)source.ptr) + byteIndex;
        uint[ubyte.max + 1] byteCounter;
        for (size_t i = 0; i < sourceSize; i++, curByte += U.sizeof)
            byteCounter[*curByte]++;

        {
            uint indexStart;
            foreach (uint i; 0 .. byteCounter.length) {
                immutable size_t tempCount = byteCounter[i];
                byteCounter[i] = indexStart;
                indexStart += tempCount;
            }
        }

        curByte = (cast(ubyte*)source.ptr) + byteIndex;
        for (size_t i = 0; i < sourceSize; i++, curByte += U.sizeof) {
            uint* countPtr = byteCounter.ptr + *curByte;
            dest[*countPtr] = source[i];
            (*countPtr)++;
        }
    }

    U[] tempData;
    if (U.sizeof * data.length <= MAX_ALLOCA) {
        U* ptr = cast(U*)alloca(data.length * U.sizeof);
        if (ptr != null)
            tempData = ptr[0 .. data.length];
    }
    if (tempData.empty)
        tempData = uninitializedArray!(U[])(data.length);

    static if (U.sizeof == 1) {
        radix(0, data, tempData);
        data[] = tempData[];
    } else {
        for (uint i = 0; i < U.sizeof; i += 2) {
            radix(i + 0, data, tempData);
            radix(i + 1, tempData, data);
        }
    }
}

void main() {
    import std.stdio;
    uint[] items = [170, 45, 75, 4294967206, 2, 24, 4294966494, 66];
    items.radixSort();
    writeln(items);
}
```

{{out}}

```txt
[2, 24, 45, 66, 75, 170, 4294966494, 4294967206]
```

Original C++ code, modified (unknown license), by Andre Reinald, Paul Harris, Ryan Rohrer, Dirk Jagdmann:
http://www.cubic.org/docs/download/radix_ar_2011.cpp


## EasyLang


<lang># only works with positive integers
# 
subr sort
  radix = 16
  max = 0
  for di range len data[]
    if data[di] > max
      max = data[di]
    .
  .
  len buck[][] radix
  pos = 1
  while pos <= max
    for i range radix
      len buck[i][] 0
    .
    for di range len data[]
      h = data[di] / pos mod radix
      buck[h][] &= data[di]
    .
    di = 0
    for i range radix
      for j range len buck[i][]
        data[di] = buck[i][j]
        di += 1
      .
    .
    pos *= radix
  .
.
data[] = [ 29 4 72 44 55 26 27 77 92 5 ]
call sort
print data[]
```



## Eiffel

Works for positive integers. Splits up into two buckets according to the binary representation of the number.

```Eiffel

class
	RADIX_SORT

feature

	radix_sort (ar: ARRAY [INTEGER])
			-- Array 'ar' sorted in ascending order.
		require
			ar_not_void: ar /= Void
			not_negative: across ar as a all a.item >= 0 end
		local
			bucket_1, bucket_0: LINKED_LIST [INTEGER]
			j, k, dig: INTEGER
		do
			create bucket_0.make
			create bucket_1.make
			dig := digits (ar)
			across
				0 |..| dig as c
			loop
				across
					ar as r
				loop
					if r.item.bit_test (c.item) then
						bucket_1.extend (r.item)
					else
						bucket_0.extend (r.item)
					end
				end
				from
					j := 1
				until
					j > bucket_0.count
				loop
					ar [j] := bucket_0 [j]
					j := j + 1
				end
				from
					k := j
					j := 1
				until
					j > bucket_1.count
				loop
					ar [k] := bucket_1 [j]
					k := k + 1
					j := j + 1
				end
				bucket_0.wipe_out
				bucket_1.wipe_out
			end
		ensure
			is_sorted: is_sorted (ar)
		end

feature {NONE}

	digits (ar: ARRAY [INTEGER]): INTEGER
			-- Number of digits of the largest item in 'ar'.
		local
			max: INTEGER
			math: DOUBLE_MATH
		do
			create math
			across
				ar as a
			loop
				if a.item > max then
					max := a.item
				end
			end
			Result := math.log_2 (max).ceiling + 1
		end

	is_sorted (ar: ARRAY [INTEGER]): BOOLEAN
			--- Is 'ar' sorted in ascending order?
		local
			i: INTEGER
		do
			Result := True
			from
				i := ar.lower
			until
				i >= ar.upper
			loop
				if ar [i] > ar [i + 1] then
					Result := False
				end
				i := i + 1
			end
		end

end

```

Test:

```Eiffel

class
	APPLICATION

create
	make

feature

	make
		local
			test: ARRAY [INTEGER]
		do
			create rs
			create test.make_empty
			test := <<5, 4, 999, 5, 70, 0, 1000, 55, 1, 2, 3>>
			io.put_string ("Unsorted:%N")
			across
				test as t
			loop
				io.put_string (t.item.out + " ")
			end
			rs.radix_sort (test)
			io.put_string ("%NSorted:%N")
			across
				test as t
			loop
				io.put_string (t.item.out + " ")
			end
		end

	rs: RADIX_SORT

end

```

{{out}}

```txt

Unsorted:
5 4 999 5 70 0 1000 55 1 2 3
Sorted:
0 1 2 3 4 5 5 55 70 999 1000

```



## Elixir

{{trans|Ruby}}

```elixir
defmodule Sort do
  def radix_sort(list), do: radix_sort(list, 10)
  
  def radix_sort([], _), do: []
  def radix_sort(list, base) do
    max = abs(Enum.max_by(list, &abs(&1)))
    sorted = radix_sort(list, base, max, 1)
    {minus, plus} = Enum.partition(sorted, &(&1<0))
    Enum.reverse(minus, plus)
  end
  
  defp radix_sort(list, _, max, m) when max<m, do: list
  defp radix_sort(list, base, max, m) do
    buckets = List.to_tuple(for _ <- 0..base-1, do: [])
    bucket2 = Enum.reduce(list, buckets, fn x,acc ->
      i = abs(x) |> div(m) |> rem(base)
      put_elem(acc, i, [x | elem(acc, i)])
    end)
    list2 = Enum.reduce(base-1..0, [], fn i,acc -> Enum.reverse(elem(bucket2, i), acc) end)
    radix_sort(list2, base, max, m*base)
  end
end

IO.inspect Sort.radix_sort([-4, 5, -26, 58, -990, 331, 331, 990, -1837, 2028])
```


{{out}}

```txt

[-1837, -990, -26, -4, 5, 58, 331, 331, 990, 2028]

```



## Fortran


```Fortran

*
### =================================================================

* RSORT - sort a list of integers by the Radix Sort algorithm
* Public domain.  This program may be used by any person for any purpose.
* Origin:  Herman Hollerith, 1887
*
*___Name____Type______In/Out____Description_____________________________
*   IX(N)   Integer   Both      Array to be sorted in increasing order
*   IW(N)   Integer   Neither   Workspace
*   N       Integer   In        Length of array
*
* ASSUMPTIONS:  Bits in an INTEGER is an even number.
*               Integers are represented by twos complement.
*
* NOTE THAT:  Radix sorting has an advantage when the input is known 
*             to be less than some value, so that only a few bits need 
*             to be compared.  This routine looks at all the bits, 
*             and is thus slower than Quicksort.
*
### =================================================================

      SUBROUTINE RSORT (IX, IW, N)      
       IMPLICIT NONE
       INTEGER IX, IW, N
       DIMENSION IX(N), IW(N)

       INTEGER I,                        ! count bits
     $         ILIM,                     ! bits in an integer
     $         J,                        ! count array elements
     $         P1OLD, P0OLD, P1, P0,     ! indices to ones and zeros
     $         SWAP
       LOGICAL ODD                       ! even or odd bit position

*      IF (N < 2) RETURN      ! validate
*
        ILIM = Bit_size(i)    !Get the fixed number of bits
*
### =================================================================

* Alternate between putting data into IW and into IX
*
### =================================================================

       P1 = N+1
       P0 = N                ! read from 1, N on first pass thru
       ODD = .FALSE.
       DO I = 0, ILIM-2
         P1OLD = P1
         P0OLD = P0         ! save the value from previous bit
         P1 = N+1
         P0 = 0                 ! start a fresh count for next bit

         IF (ODD) THEN
           DO J = 1, P0OLD, +1             ! copy data from the zeros
             IF ( BTEST(IW(J), I) ) THEN
               P1 = P1 - 1
               IX(P1) = IW(J)
             ELSE
               P0 = P0 + 1
               IX(P0) = IW(J)
             END IF
           END DO
           DO J = N, P1OLD, -1             ! copy data from the ones
             IF ( BTEST(IW(J), I) ) THEN
               P1 = P1 - 1
               IX(P1) = IW(J)
             ELSE
               P0 = P0 + 1
              IX(P0) = IW(J)
             END IF
           END DO
          
         ELSE 
           DO J = 1, P0OLD, +1             ! copy data from the zeros
             IF ( BTEST(IX(J), I) ) THEN
               P1 = P1 - 1
               IW(P1) = IX(J)
              ELSE
               P0 = P0 + 1
               IW(P0) = IX(J)
             END IF
           END DO
           DO J = N, P1OLD, -1            ! copy data from the ones
             IF ( BTEST(IX(J), I) ) THEN
               P1 = P1 - 1
               IW(P1) = IX(J)
             ELSE
               P0 = P0 + 1
               IW(P0) = IX(J)
             END IF
          END DO
         END IF  ! even or odd i
        
         ODD = .NOT. ODD
       END DO  ! next i

*
### =================================================================

*        the sign bit
*
### =================================================================

       P1OLD = P1
       P0OLD = P0
       P1 = N+1
       P0 = 0 

*          if sign bit is set, send to the zero end
       DO J = 1, P0OLD, +1
         IF ( BTEST(IW(J), ILIM-1) ) THEN 
           P0 = P0 + 1
           IX(P0) = IW(J)
         ELSE
           P1 = P1 - 1
           IX(P1) = IW(J)
         END IF
       END DO          
       DO J = N, P1OLD, -1
         IF ( BTEST(IW(J), ILIM-1) ) THEN
           P0 = P0 + 1
           IX(P0) = IW(J)
         ELSE
           P1 = P1 - 1
           IX(P1) = IW(J)
         END IF
       END DO
          
*
### =================================================================

*       Reverse the order of the greater value partition
*
### =================================================================

       P1OLD = P1
       DO J = N, (P1OLD+N)/2+1, -1
         SWAP = IX(J)
         IX(J) = IX(P1)
         IX(P1) = SWAP
         P1 = P1 + 1
       END DO
       RETURN
      END ! of RSORT


***********************************************************************
*         test program
***********************************************************************
      PROGRAM t_sort
       IMPLICIT NONE
       INTEGER I, N
       PARAMETER (N = 11)
       INTEGER IX(N), IW(N)
       LOGICAL OK
       
       DATA IX / 2, 24, 45, 0, 66, 75, 170, -802, -90, 1066, 666 /
       
       PRINT *, 'before: ', IX
       CALL RSORT (IX, IW, N)
       PRINT *, 'after: ', IX
       
*              compare
       OK = .TRUE.
       DO I = 1, N-1
         IF (IX(I) > IX(I+1)) OK = .FALSE.
       END DO
       IF (OK) THEN
         PRINT *, 't_sort: successful test'
       ELSE
         PRINT *, 't_sort: failure!'
       END IF
      END ! of test program

```


{{out}}

```txt

 before:  2 24 45 0 66 75 170 -802 -90 1066 666
 after:  -802 -90 0 2 24 45 66 75 170 666 1066
 t_sort: successful test

```



## Go

LSD radix 256, negatives handled by flipping the high bit.

```go
package main

import (
    "bytes"
    "encoding/binary"
    "fmt"
)

// declarations for word size of data
type word int32
const wordLen = 4
const highBit = -1 << 31

var data = []word{170, 45, 75, -90, -802, 24, 2, 66}

func main() {
    buf := bytes.NewBuffer(nil)
    ds := make([][]byte, len(data))
    for i, x := range data {
        binary.Write(buf, binary.LittleEndian, x^highBit)
        b := make([]byte, wordLen)
        buf.Read(b)
        ds[i] = b
    }
    bins := make([][][]byte, 256)
    for i := 0; i < wordLen; i++ {
        for _, b := range ds {
            bins[b[i]] = append(bins[b[i]], b)
        }
        j := 0
        for k, bs := range bins {
            copy(ds[j:], bs)
            j += len(bs)
            bins[k] = bs[:0]
        }
    }
    fmt.Println("original:", data)
    var w word
    for i, b := range ds {
        buf.Write(b)
        binary.Read(buf, binary.LittleEndian, &w)
        data[i] = w^highBit
    }
    fmt.Println("sorted:  ", data)
}
```

Output:

```txt

original: [170 45 75 -90 -802 24 2 66]
sorted:   [-802 -90 2 24 45 66 75 170]

```



## Groovy

This solution assumes the radix is a power of 2:

```groovy
def radixSort = { final radixExponent, list ->
    def fromBuckets = new TreeMap([0:list])
    def toBuckets = new TreeMap()
    final radix = 2**radixExponent
    final mask = radix - 1
    final radixDigitSize = (int)Math.ceil(64/radixExponent)
    final digitWidth = radixExponent
    (0..<radixDigitSize).each { radixDigit ->
        fromBuckets.values().findAll { it != null }.flatten().each {
            print '.'
            long bucketNumber = (long)((((long)it) >>> digitWidth*radixDigit) & mask)
            toBuckets[bucketNumber] = toBuckets[bucketNumber] ?: []
            toBuckets[bucketNumber] << it
        }
        (fromBuckets, toBuckets) = [toBuckets, fromBuckets]
        toBuckets.clear()
    }
    final overflow = 2**(63 % radixExponent)
    final pos = {it < overflow}
    final neg = {it >= overflow}
    final keys = fromBuckets.keySet()
    final twosComplIndx = [] + (keys.findAll(neg)) + (keys.findAll(pos))
    twosComplIndx.collect { fromBuckets[it] }.findAll { it != null }.flatten()
}
```


Test:

```groovy
println (radixSort(3, [23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4]))
println (radixSort(3, [88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1]))
println (radixSort(3, [23,-76,-990,580,97,57,350000,Long.MAX_VALUE,89,Long.MIN_VALUE,51,38,95*2**48,92,-24*2**48,46,31*2**32,24,14,12,57,78,4]))
println ()
println (radixSort(8, [23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4]))
println (radixSort(8, [88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1]))
println (radixSort(8, [23,-76,-990,580,97,57,350000,Long.MAX_VALUE,89,Long.MIN_VALUE,51,38,95*2**48,92,-24*2**48,46,31*2**32,24,14,12,57,78,4]))
println ()
println (radixSort(11, [23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4]))
println (radixSort(11, [88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1]))
println (radixSort(11, [23,-76,-990,580,97,57,350000,Long.MAX_VALUE,89,Long.MIN_VALUE,51,38,95*2**48,92,-24*2**48,46,31*2**32,24,14,12,57,78,4]))
println ()
println (radixSort(16, [23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4]))
println (radixSort(16, [88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1]))
println (radixSort(16, [23,-76,-990,580,97,57,350000,Long.MAX_VALUE,89,Long.MIN_VALUE,51,38,95*2**48,92,-24*2**48,46,31*2**32,24,14,12,57,78,4]))
println ()
println (radixSort(32, [23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4]))
println (radixSort(32, [88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1]))
println (radixSort(32, [23,-76,-990,580,97,57,350000,Long.MAX_VALUE,89,Long.MIN_VALUE,51,38,95*2**48,92,-24*2**48,46,31*2**32,24,14,12,57,78,4]))
```


Output:
<pre style="height:30ex;overflow:scroll;">..............................................................................................................................................................................................................................................................................................................................................................................................................................................................................[4, 12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]
..........................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................[0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]
..........................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................[-9223372036854775808, -6755399441055744, -990, -76, 4, 12, 14, 23, 24, 38, 46, 51, 57, 57, 78, 89, 92, 97, 580, 350000, 133143986176, 26740122787512320, 9223372036854775807]

........................................................................................................................................................................[4, 12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]
........................................................................................................................................................................................[0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]
........................................................................................................................................................................................[-9223372036854775808, -6755399441055744, -990, -76, 4, 12, 14, 23, 24, 38, 46, 51, 57, 57, 78, 89, 92, 97, 580, 350000, 133143986176, 26740122787512320, 9223372036854775807]

..............................................................................................................................[4, 12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]
..........................................................................................................................................[0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]
..........................................................................................................................................[-9223372036854775808, -6755399441055744, -990, -76, 4, 12, 14, 23, 24, 38, 46, 51, 57, 57, 78, 89, 92, 97, 580, 350000, 133143986176, 26740122787512320, 9223372036854775807]

....................................................................................[4, 12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]
............................................................................................[0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]
............................................................................................[-9223372036854775808, -6755399441055744, -990, -76, 4, 12, 14, 23, 24, 38, 46, 51, 57, 57, 78, 89, 92, 97, 580, 350000, 133143986176, 26740122787512320, 9223372036854775807]

..........................................[4, 12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]
..............................................[0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]
..............................................[-9223372036854775808, -6755399441055744, -990, -76, 4, 12, 14, 23, 24, 38, 46, 51, 57, 57, 78, 89, 92, 97, 580, 350000, 133143986176, 26740122787512320, 9223372036854775807]
```



## Haskell



```haskell
import Data.Bits (Bits(testBit, bitSize))
import Data.List (partition)

lsdSort :: (Ord a, Bits a) => [a] -> [a]
lsdSort = fixSort positiveLsdSort

msdSort :: (Ord a, Bits a) => [a] -> [a]
msdSort = fixSort positiveMsdSort

-- Fix a sort that puts negative numbers at the end, like positiveLsdSort and positiveMsdSort
fixSort sorter list = uncurry (flip (++)) (break (< 0) (sorter list))

positiveLsdSort :: (Bits a) => [a] -> [a]
positiveLsdSort list = foldl step list [0..bitSize (head list)] where
	step list bit = uncurry (++) (partition (not . flip testBit bit) list)

positiveMsdSort :: (Bits a) => [a] -> [a]
positiveMsdSort list = aux (bitSize (head list) - 1) list where
	aux _ [] = []
	aux (-1) list = list
	aux bit list = aux (bit - 1) lower ++ aux (bit - 1) upper where
		(lower, upper) = partition (not . flip testBit bit) list
```


=={{header|Icon}} and {{header|Unicon}}==

The following is nice and short and works in both languages.  However it
contains a subtle inefficiency: subscripting a numeric value first coerces it into a string.


```unicon
procedure main(A)
    every writes((!rSort(A)||" ")|"\n")
end

procedure rSort(A)
    every (min := A[1]) >:= !A
    every (mlen := *(A[1]-min)) <:= (!A - min)
    every i := !*mlen do {
        every put(b := [], |[]\12)
        every a := !A do put(b[(a-min)[-i]+2|1], a)
        every put(A := [],!!b)
        }
    return A
end
```


Sample run:

```txt

->radix 31 123 -98 7090 802 2
-98 2 31 123 802 7090
->

```



## J

{{eff note|J|/:~}}

<code>keys f/. data </code>  evaluates the function f on each group of data at the same position as similar keys.  Sorting requires ordered keys.  This code uses a J idiom: prepend the keys and matching data.  The extra data is removed by behead <code>}.</code>.


```j

radixSortR =: 3 : 0  NB. base radixSort data
16 radixSortR y
:
keys =. x #.^:_1 y NB. compute keys
length =. #{.keys
extra =. (-length) {."0 buckets =. i.x
for_pass. i.-length do.
   keys =. ; (buckets,pass{"1 keys) <@:}./.extra,keys
end.
x#.keys NB. restore the data
)
```


An alternate implementation is

```j
radixsort=: (] #~ [: +/ =/) i.@(>./)
```


This uses the maximum value of the list for the base, which allows the list to be sorted in one pass.

Example use:


```j
   radixsort ?.@#~10
4 5 6 6 6 6 6 8 8
```


Or, for negative number support:


```j
rsort=: (] + radixsort@:-) <./
```


Example:


```j
   rsort _6+?.@#~10
_2 _1 0 0 0 0 0 2 2
```



## Java


```java
public static int[] sort(int[] old) {
    // Loop for every bit in the integers
    for (int shift = Integer.SIZE - 1; shift > -1; shift--) {
        // The array to put the partially sorted array into
        int[] tmp = new int[old.length];
        // The number of 0s
        int j = 0;

        // Move the 0s to the new array, and the 1s to the old one
        for (int i = 0; i < old.length; i++) {
            // If there is a 1 in the bit we are testing, the number will be negative
            boolean move = old[i] << shift >= 0;

            // If this is the last bit, negative numbers are actually lower
            if (shift == 0 ? !move : move) {
                tmp[j] = old[i];
                j++;
            } else {
                // It's a 1, so stick it in the old array for now
                old[i - j] = old[i];
            }
        }

        // Copy over the 1s from the old array
        for (int i = j; i < tmp.length; i++) {
            tmp[i] = old[i - j];
        }

        // And now the tmp array gets switched for another round of sorting
        old = tmp;
    }

    return old;
}
```


{{trans|NetRexx}}

```Java

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

public class RSortingRadixsort00 {

  public RSortingRadixsort00() {

    return;
  }

  public static int[] lsdRadixSort(int[] tlist) {

    List<Integer> intermediates;
    int[] limits = getLimits(tlist);
    tlist = rescale(tlist, limits[1]);

    for (int px = 1; px <= limits[2]; ++px) {
      @SuppressWarnings("unchecked")
      Queue<Integer> bukits[] = new Queue[10];
      for (int ix = 0; ix < tlist.length; ++ix) {
        int cval = tlist[ix];
        int digit = (int) (cval / Math.pow(10, px - 1) % 10);
        if (bukits[digit] == null) {
          bukits[digit] = new LinkedList<>();
        }
        bukits[digit].add(cval);
      }

      intermediates = new ArrayList<>();
      for (int bi = 0; bi < 10; ++bi) {
        if (bukits[bi] != null) {
          while (bukits[bi].size() > 0) {
            int nextd;
            nextd = bukits[bi].poll();
            intermediates.add(nextd);
          }
        }
      }

      for (int iw = 0; iw < intermediates.size(); ++iw) {
        tlist[iw] = intermediates.get(iw);
      }
    }

    tlist = rescale(tlist, -limits[1]);

    return tlist;
  }

  private static int[] rescale(int[] arry, int delta) {

    for (int ix = 0; ix < arry.length; ++ix) {
      arry[ix] -= delta;
    }

    return arry;
  }

  private static int[] getLimits(int[] tlist) {

    int[] lims = new int[3];

    for (int i_ = 0; i_ < tlist.length; ++i_) {
      lims[0] = Math.max(lims[0], tlist[i_]);
      lims[1] = Math.min(lims[1], tlist[i_]);
    }
    lims[2] = (int) Math.ceil(Math.log10(lims[0] - lims[1]));

    return lims;
  }

  private static void runSample(String[] args) {

    int[][] lists = {
      new int[] { 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -0, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10, },
      new int[] { -10, -9, -8, -7, -6, -5, -4, -3, -2, -1, -0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, },
      new int[] { 2, 24, 45, 0, 66, 75, 170, -802, -90, 1066, 666, },
      new int[] { 170, 45, 75, 90, 2, 24, 802, 66, },
      new int[] { -170, -45, -75, -90, -2, -24, -802, -66, },
    };

    long etime;
    lsdRadixSort(Arrays.copyOf(lists[0], lists[0].length)); // do one pass to set up environment to remove it from timings

    for (int[] tlist : lists) {
      System.out.println(array2list(tlist));
      etime = System.nanoTime();
      tlist = lsdRadixSort(tlist);
      etime = System.nanoTime() - etime;
      System.out.println(array2list(tlist));
      System.out.printf("Elapsed time: %fs%n", ((double) etime / 1_000_000_000.0));
      System.out.println();
    }

    return;
  }

  private static List<Integer> array2list(int[] arry) {

    List<Integer> target = new ArrayList<>(arry.length);

    for (Integer iv : arry) {
      target.add(iv);
    }

    return target;
  }

  public static void main(String[] args) {

    runSample(args);

    return;
  }
}

```

{{out}}

```txt

[10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10]
[-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Elapsed time: 0.000256s

[-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
[-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Elapsed time: 0.000198s

[2, 24, 45, 0, 66, 75, 170, -802, -90, 1066, 666]
[-802, -90, 0, 2, 24, 45, 66, 75, 170, 666, 1066]
Elapsed time: 0.000187s

[170, 45, 75, 90, 2, 24, 802, 66]
[2, 24, 45, 66, 75, 90, 170, 802]
Elapsed time: 0.000088s

[-170, -45, -75, -90, -2, -24, -802, -66]
[-802, -170, -90, -75, -66, -45, -24, -2]
Elapsed time: 0.000113s


```



## jq


```jq
# Sort the input array;
# "base" must be an integer greater than 1
def radix_sort(base):
  # We only need the ceiling of non-negatives:
  def ceil: if . == floor then . else (. + 1 | floor) end;

  min as $min
  | map(. - $min)
  | ((( max|log) / (base|log)) | ceil) as $rounds
  | reduce range(0; $rounds) as $i
      # state: [ base^i, buckets ]
      ( [1, .];
        .[0] as $base_i
        | reduce .[1][] as $n 
            ([];
             (($n/$base_i) % base) as $digit
             | .[$digit] += [$n] )
        | [($base_i * base), (map(select(. != null)) | flatten)] )
  | .[1]
  | map(. + $min) ;

def radix_sort:
  radix_sort(10);

```

'''Example'''

```jq

# Verify that radix_sort agrees with sort
( [1, 3, 8, 9, 0, 0, 8, 7, 1, 6],
  [170, 45, 75, 90, 2, 24, 802, 66],
  [170, 45, 75, 90, 2, 24, -802, -66] ) 
| (radix_sort == sort)

```

{{Out}}
 true
 true
 true



## Julia

{{trans|Scala}}

```julia
function radixsort(tobesorted::Vector{Int64})
    arr = deepcopy(tobesorted)
    for shift in 63:-1:0
        tmp = Vector{Int64}(undef, length(arr))
        j = 0
        for i in 1:length(arr)
            if (shift == 0) == ((arr[i] << shift) >= 0)
                arr[i - j] = arr[i]
            else
                tmp[j + 1] = arr[i]
                j += 1
            end
        end
        tmp[j+1:end] .= arr[1:length(tmp)-j]
        arr = tmp
    end
    arr
end

function testradixsort()
    arrays = [[170, 45, 75, -90, -802, 24, 2, 66], [-4, 5, -26, 58, -990, 331, 331, 990, -1837, 2028]]
    for array in arrays 
        println(radixsort(array))
    end
end

testradixsort()

```
{{output}}
```txt

 [-802, -90, 2, 24, 45, 66, 75, 170]
 [-1837, -990, -26, -4, 5, 58, 331, 331, 990, 2028]

```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.2

fun radixSort(original: IntArray): IntArray {
    var old = original // Need this to be mutable
    // Loop for every bit in the integers
    for (shift in 31 downTo 0) {
        val tmp = IntArray(old.size)  // The array to put the partially sorted array into
        var j = 0                     // The number of 0s
        // Move the 0s to the new array, and the 1s to the old one
        for (i in 0 until old.size) {
            // If there is a 1 in the bit we are testing, the number will be negative
            val move = (old[i] shl shift) >= 0
            // If this is the last bit, negative numbers are actually lower
            val toBeMoved = if (shift == 0) !move else move
            if (toBeMoved)
                tmp[j++] = old[i]
            else {
                // It's a 1, so stick it in the old array for now
                old[i - j] = old[i]
            }
        }
        // Copy over the 1s from the old array
        for (i in j until tmp.size) tmp[i] = old[i - j]
        // And now the tmp array gets switched for another round of sorting
        old = tmp
    }
    return old
}

fun main(args: Array<String>) {
    val arrays = arrayOf(
        intArrayOf(170, 45, 75, -90, -802, 24, 2, 66),
        intArrayOf(-4, 5, -26, 58, -990, 331, 331, 990, -1837, 2028)
    )
    for (array in arrays) println(radixSort(array).contentToString())
}
```


{{out}}

```txt

[-802, -90, 2, 24, 45, 66, 75, 170]
[-1837, -990, -26, -4, 5, 58, 331, 331, 990, 2028]

```



## Mathematica


```Mathematica
ClearAll[SortByPos, RadixSort]
SortByPos[data : {_List ..}, pos_Integer] := Module[{digs, order},
  digs = data[[All, pos]];
  order = Ordering[digs];
  data[[order]]
  ]
RadixSort[x : {_Integer ..}] := Module[{y, digs, maxlen, offset},
  offset = Min[x];
  y = x - offset;
  digs = IntegerDigits /@ y;
  maxlen = Max[Length /@ digs];
  digs = IntegerDigits[#, 10, maxlen] & /@ y;
  digs = Fold[SortByPos, digs, -Range[maxlen]];
  digs = FromDigits /@ digs;
  digs += offset;
  digs
  ]
```

Testing out the algorithm:

```Mathematica
RadixSort[{170,45,75,-90,-802,24,2,66}]
RadixSort[{170,45,75,90,802,2,24,66}]
```

{{out}}

```txt
{-802,-90,2,24,45,66,75,170}
{2,24,45,66,75,90,170,802}
```




## NetRexx

Uses a suggestion in the discussion page to handle negative values.
<br />
Limitations - Handles decimal digits only.

### Using the <tt>Rexx</tt> class


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method radixSort(tlist = Rexx[]) public static returns Rexx[]

  -- scale the array to start at zero to allow handling of -ve values
  parse getLimits(tlist) maxn minn maxl .
  tlist = rescale(tlist, minn)

  loop px = maxl to 1 by -1
    bukits = ''
    loop ix = 0 to tlist.length - 1
      cval = tlist[ix].right(maxl, 0)
      parse cval . =(px) digit +1 .
      bukits[digit] = bukits[digit] (cval + 0) -- simulates a stack
      end ix
    intermediates = ''
    loop bi = 0 to 9
      intermediates = intermediates bukits[bi] -- sumulates unstack
      end bi
    -- reload array
    loop iw = 1 to intermediates.words()
      tlist[iw - 1] = intermediates.word(iw)
      end iw
    end px

  -- restore the array to original scale
  tlist = rescale(tlist, -minn)
  return tlist

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method rescale(arry = Rexx[], newbase) private static returns Rexx[]
  loop ix = 0 to arry.length - 1
    arry[ix] = arry[ix] - newbase
    end ix
  return arry
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method getLimits(arry = Rexx[]) private static returns Rexx
  maxn = 0
  minn = 0
  maxl = 0
  loop i_ = 0 to arry.length - 1
    maxn = maxn.max(arry[i_])
    minn = minn.min(arry[i_])
    end i_
  maxl = (maxn - minn).length()
  return maxn minn maxl
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  lists = [-
    [2, 24, 45, 0, 66, 75, 170, -802, -90, 1066, 666], -
    [170, 45, 75, 90, 2, 24, 802, 66], -
    [10, 9, 8, 7, 8, 5, 4, 3, 2, 1, 0], -
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10], -
    [-10, -9, -8, -7, -8, -5, -4, -3, -2, -1, -0], -
    [-0, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10], -
    [-10, -19, -18, -17, -18, -15, -14, -13, -12, -11, -100], -
    [10, 9, 8, 7, 8, 5, 4, 3, 2, 1, 0, -0, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10], -
    [-10, -9, -8, -7, -8, -5, -4, -3, -2, -1, -0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] -
  ]

  loop il = 0 to lists.length - 1
    tlist = lists[il]
    say ' Input:' Arrays.asList(tlist)
    say 'Output:' Arrays.asList(radixSort(tlist))
    say
    end il
  return

```

{{out}}

```txt

 Input: [2, 24, 45, 0, 66, 75, 170, -802, -90, 1066, 666]
Output: [-802, -90, 0, 2, 24, 45, 66, 75, 170, 666, 1066]

 Input: [170, 45, 75, 90, 2, 24, 802, 66]
Output: [2, 24, 45, 66, 75, 90, 170, 802]

 Input: [10, 9, 8, 7, 8, 5, 4, 3, 2, 1, 0]
Output: [0, 1, 2, 3, 4, 5, 7, 8, 8, 9, 10]

 Input: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Output: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

 Input: [-10, -9, -8, -7, -8, -5, -4, -3, -2, -1, 0]
Output: [-10, -9, -8, -8, -7, -5, -4, -3, -2, -1, 0]

 Input: [0, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10]
Output: [-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0]

 Input: [-10, -19, -18, -17, -18, -15, -14, -13, -12, -11, -100]
Output: [-100, -19, -18, -18, -17, -15, -14, -13, -12, -11, -10]

 Input: [10, 9, 8, 7, 8, 5, 4, 3, 2, 1, 0, 0, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10]
Output: [-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 0, 1, 2, 3, 4, 5, 7, 8, 8, 9, 10]

 Input: [-10, -9, -8, -7, -8, -5, -4, -3, -2, -1, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Output: [-10, -9, -8, -8, -7, -5, -4, -3, -2, -1, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

```


### Using <tt>Collection</tt> classes


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import java.util.Queue

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method radixSort(tlist = Rexx[]) public static returns Rexx[]

  -- scale the array to start at zero to allow handling of -ve values
  limits = ''
  parse '!MAXN !MINN !MAXL' maxn_ minn_ maxl_ .
  parse getLimits(tlist) maxn minn maxl .
  limits[maxn_] = maxn
  limits[minn_] = minn
  limits[maxl_] = maxl
  tlist = rescale(tlist, limits[minn_])

  loop px = limits[maxl_] to 1 by -1
    bukits = Queue[10] -- stacks for digits 0 .. 9
    loop ix = 0 while ix < tlist.length
      cval = tlist[ix].right(limits[maxl_], 0)
      parse cval . =(px) digit +1 . -- extract next digit (fun with parse)
      -- alternatively: digit = (cval % (10 ** (px - 1))) // 10
      if bukits[digit] == null then bukits[digit] = LinkedList()
      bukits[digit].add((cval + 0))
      end ix

    intermediates = ArrayList()
    loop bi = 0 to 9
      if bukits[bi] \= null then loop while bukits[bi].size() > 0
        nextd = bukits[bi].poll()
        intermediates.add(nextd)
        end
      end bi

    -- reload result array
    loop iw = 0 while iw < intermediates.size()
      tlist[iw] = Rexx intermediates.get(iw)
      end iw
    end px

  -- restore the array to original scale
  tlist = rescale(tlist, -limits[minn_])
  return tlist

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method rescale(arry = Rexx[], newbase) private static returns Rexx[]
  loop ix = 0 to arry.length - 1
    arry[ix] = arry[ix] - newbase
    end ix
  return arry
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method getLimits(arry = Rexx[]) private static returns Rexx
  maxn = 0
  minn = 0
  maxl = 0
  loop i_ = 0 to arry.length - 1
    maxn = maxn.max(arry[i_])
    minn = minn.min(arry[i_])
    end i_
  maxl = (maxn - minn).length()
  return maxn minn maxl
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  lists = [-
    [2, 24, 45, 0, 66, 75, 170, -802, -90, 1066, 666], -
    [170, 45, 75, 90, 2, 24, 802, 66], -
    [10, 9, 8, 7, 8, 5, 4, 3, 2, 1, 0], -
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10], -
    [-10, -9, -8, -7, -8, -5, -4, -3, -2, -1, -0], -
    [-0, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10], -
    [-10, -19, -18, -17, -18, -15, -14, -13, -12, -11, -100], -
    [10, 9, 8, 7, 8, 5, 4, 3, 2, 1, 0, -0, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10], -
    [-10, -9, -8, -7, -8, -5, -4, -3, -2, -1, -0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] -
  ]

  loop il = 0 to lists.length - 1
    tlist = lists[il]
    say ' Input:' Arrays.asList(tlist)
    say 'Output:' Arrays.asList(radixSort(tlist))
    say
    end il
  return

```



## Perl

Radix sort in base 10.

```perl
#!/usr/bin/perl
use warnings;
use strict;

sub radix {
    my @tab = ([@_]);

    my $max_length = 0;
    length > $max_length and $max_length = length for @_;
    $_ = sprintf "%0${max_length}d", $_ for @{ $tab[0] }; # Add zeros.

    for my $pos (reverse -$max_length .. -1) {
        my @newtab;
        for my $bucket (@tab) {
            for my $n (@$bucket) {
                my $char = substr $n, $pos, 1;
                $char = -1 if '-' eq $char;
                $char++;
                push @{ $newtab[$char] }, $n;
            }
        }
        @tab = @newtab;
    }

    my @return;
    my $negative = shift @tab;                            # Negative bucket must be reversed.
    push @return, reverse @$negative;
    for my $bucket (@tab) {
        push @return, @{ $bucket // [] };
    }
    $_ = 0 + $_ for @return;                              # Remove zeros.
    return @return;
}
```

To test, add the following lines:

```perl>use Test::More tests =
 1000;

for (1 .. 1000) {
    my @l = map int rand(2000) - 1000, 0 .. 20;
    is_deeply([radix(@l)], [sort { $a <=> $b } @l]);
}
```



## Perl 6

A base-10 radix sort, done on the string representation of the integers. Signs are handled by in-place reversal of the '-' bucket on the last iteration. (The sort in there is not cheating; it only makes sure we process the buckets in the right order, since <tt>classify</tt> might return the buckets in random order.  It might be more efficient to create our own ordered buckets, but this is succinct.)

```perl6
sub radsort (@ints) {
    my $maxlen = max @ints».chars;
    my @list = @ints».fmt("\%0{$maxlen}d");

    for reverse ^$maxlen -> $r {
        my @buckets = @list.classify( *.substr($r,1) ).sort: *.key;
        @buckets[0].value = @buckets[0].value.reverse.List
            if !$r and @buckets[0].key eq '-';
        @list = flat map *.value.values, @buckets;
    }
    @list».Int;
}

.say for radsort (-2_000 .. 2_000).roll(20);
```

{{out}}

```txt
-1585
-1427
-1228
-1067
-945
-657
-643
-232
-179
-28
37
411
488
509
716
724
1504
1801
1864
1939
```



## Phix


```Phix
function radixSortn(sequence s, integer n)
sequence buckets = repeat({},10)
sequence res = {}
    for i=1 to length(s) do
        integer digit = remainder(floor(s[i]/power(10,n-1)),10)+1
        buckets[digit] = append(buckets[digit],s[i])
    end for
    for i=1 to length(buckets) do
        integer len = length(buckets[i])
        if len!=0 then
            if len=1 or n=1 then
                res &= buckets[i]
            else
                res &= radixSortn(buckets[i],n-1)
            end if
        end if
    end for
    return res
end function

function split_by_sign(sequence s)
sequence buckets = {{},{}}
    for i=1 to length(s) do
        integer si = s[i]
        if si<0 then
            buckets[1] = append(buckets[1],-si)
        else
            buckets[2] = append(buckets[2],si)
        end if
    end for
    return buckets
end function

function radixSort(sequence s)
integer mins = min(s)
integer passes = max(max(s),abs(mins))
    passes = floor(log10(passes))+1
    if mins<0 then
        sequence buckets = split_by_sign(s)
        buckets[1] = reverse(sq_uminus(radixSortn(buckets[1],passes)))
        buckets[2] = radixSortn(buckets[2],passes)
        s = buckets[1]&buckets[2]
    else
        s = radixSortn(s,passes)
    end if
    return s
end function

?radixSort({1, 3, 8, 9, 0, 0, 8, 7, 1, 6})
?radixSort({170, 45, 75, 90, 2, 24, 802, 66})
?radixSort({170, 45, 75, 90, 2, 24, -802, -66})
?radixSort({100000, -10000, 400, 23, 10000})
```

{{out}}

```txt

{0,0,1,1,3,6,7,8,8,9}
{2,24,45,66,75,90,170,802}
{-802,-66,2,24,45,75,90,170}
{-10000,23,400,10000,100000}

```



## PicoLisp

This is a LSD base-2 radix sort using queues:

```PicoLisp
(de radixSort (Lst)
   (let Mask 1
      (while
         (let (Pos (list NIL NIL)  Neg (list NIL NIL)  Flg)
            (for N Lst
               (queue
                  (if2 (ge0 N) (bit? Mask N)
                     (cdr Pos) Pos Neg (cdr Neg) )
                  N )
               (and (>= (abs N) Mask) (on Flg)) )
            (setq
               Lst (conc (apply conc Neg) (apply conc Pos))
               Mask (* 2 Mask) )
            Flg ) ) )
   Lst )
```

Output:

```txt
: (radixSort (make (do 12 (link (rand -999 999)))))
-> (-999 -930 -666 -336 -218 68 79 187 391 405 697 922)
```



## PureBasic


```PureBasic
Structure bucket
  List i.i()
EndStructure

DataSection
  ;sets specify the size (1 based) followed by each integer
  set1:
  Data.i 10 ;size
  Data.i 1, 3, 8, 9, 0, 0, 8, 7, 1, 6 ;data
  set2:
  Data.i 8 
  Data.i 170, 45, 75, 90, 2, 24, 802, 66
  set3:
  Data.i 8
  Data.i 170, 45, 75, 90, 2, 24, -802, -66
EndDataSection

Procedure setIntegerArray(Array x(1), *setPtr) 
  Protected i, count
  count = PeekI(*setPtr) - 1 ;convert to zero based count
  *setPtr + SizeOf(Integer) ;move pointer forward to data
  Dim x(count)
  For i = 0 To count
    x(i) = PeekI(*setPtr + i * SizeOf(Integer))
  Next 
EndProcedure

Procedure displayArray(Array x(1))
  Protected i, Size = ArraySize(x())
  For i = 0 To Size
    Print(Str(x(i)))
    If i < Size: Print(", "): EndIf
  Next 
  PrintN("")
EndProcedure

Procedure radixSort(Array x(1), Base = 10)
  Protected count = ArraySize(x())
  If Base < 1 Or count < 1: ProcedureReturn: EndIf ;exit due to invalid values
  
  Protected i, pv, digit, digitCount, maxAbs, pass, index
  ;find element with largest number of digits
  For i = 0 To count
    If Abs(x(i)) > maxAbs
      maxAbs = Abs(x(i))
    EndIf 
  Next
  
  digitCount = Int(Log(maxAbs)/Log(Base)) + 1
  
  For pass = 1 To digitCount
    Dim sortBuckets.bucket(Base * 2 - 1)
    pv = Pow(Base, pass - 1)
    
    ;place elements in buckets according to the current place-value's digit
    For index = 0 To count
      digit = Int(x(index)/pv) % Base + Base
      AddElement(sortBuckets(digit)\i())
      sortBuckets(digit)\i() = x(index)
    Next
    
    ;transfer contents of buckets back into array
    index = 0
    For digit = 1 To (Base * 2) - 1
      ForEach sortBuckets(digit)\i()
        x(index) = sortBuckets(digit)\i()
        index + 1
      Next 
    Next
  Next
EndProcedure

If OpenConsole()
  Dim x(0)
  setIntegerArray(x(), ?set1)
  radixSort(x()): displayArray(x())
  
  setIntegerArray(x(), ?set2)
  radixSort(x()): displayArray(x())
  
  setIntegerArray(x(), ?set3)
  radixSort(x(), 2): displayArray(x())
  
  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
0, 0, 1, 1, 3, 6, 7, 8, 8, 9
2, 24, 45, 66, 75, 90, 170, 802
-802, -66, 2, 24, 45, 75, 90, 170
```



## Python

{{works with|Python|2.6}}

This is the Wikipedia example code extended with an extra pass to sort negative values correctly.


```python
#python2.6 <
from math import log
 
def getDigit(num, base, digit_num):
    # pulls the selected digit
    return (num // base ** digit_num) % base  
 
def makeBlanks(size):
    # create a list of empty lists to hold the split by digit
    return [ [] for i in range(size) ]  
 
def split(a_list, base, digit_num):
    buckets = makeBlanks(base)
    for num in a_list:
        # append the number to the list selected by the digit
        buckets[getDigit(num, base, digit_num)].append(num)  
    return buckets
 
# concatenate the lists back in order for the next step
def merge(a_list):
    new_list = []
    for sublist in a_list:
       new_list.extend(sublist)
    return new_list
 
def maxAbs(a_list):
    # largest abs value element of a list
    return max(abs(num) for num in a_list)

def split_by_sign(a_list):
    # splits values by sign - negative values go to the first bucket,
    # non-negative ones into the second
    buckets = [[], []]
    for num in a_list:
        if num < 0:
            buckets[0].append(num)
        else:
            buckets[1].append(num)
    return buckets
 
def radixSort(a_list, base):
    # there are as many passes as there are digits in the longest number
    passes = int(round(log(maxAbs(a_list), base)) + 1) 
    new_list = list(a_list)
    for digit_num in range(passes):
        new_list = merge(split(new_list, base, digit_num))
    return merge(split_by_sign(new_list))

```


An alternate implementation using which works on Python 3:


```python
#python3.7 <
def flatten(some_list):
    """
    Flatten a list of lists.
    Usage: flatten([[list a], [list b], ...])
    Output: [elements of list a, elements of list b]
    """
    new_list = []
    for sub_list in some_list:
        new_list += sub_list
    return new_list

def radix(some_list, idex=None, size=None):
    """
    Recursive radix sort
    Usage: radix([unsorted list])
    Output: [sorted list]
    """
    # Initialize variables not set in the initial call
    if size == None:
        largest_num = max(some_list)
        largest_num_str = str(largest_num)
        largest_num_len = len(largest_num_str)
        size = largest_num_len

    if idex == None:
        idex = size

    # Translate the index we're looking at into an array index.
    # e.g., looking at the 10's place for 100:
    # size: 3
    # idex: 2
    #    i: (3-2) == 1
    # str(123)[i] -> 2
    i = size - idex 

    # The recursive base case.
    # Hint: out of range indexing errors
    if i >= size:
        return some_list

    # Initialize the bins we will place numbers into
    bins = [[] for _ in range(10)]

    # Iterate over the list of numbers we are given
    for e in some_list:
        # The destination bin; e.g.,:
        #   size: 5
        #      e: 29
        #  num_s: '00029'
        #      i: 3
        # dest_c: '2'
        # dest_i: 2
        num_s  = str(e).zfill(size)
        dest_c = num_s[i]
        dest_i = int(dest_c) 
        bins[dest_i] += [e]

    result = []
    for b in bins:
        # Make the recursive call
        # Sort each of the sub-lists in our bins
        result.append(radix(b, idex-1, size))

    # Flatten our list
    # This is also called in our recursive call,
    # so we don't need flatten to be recursive.
    flattened_result = flatten(result)

    return flattened_result

```


That same example but more compact:


```python
#python3.7 <
def flatten(l):
    return [y for x in l for y in x]

def radix(l, p=None, s=None):
    if s == None:
        s = len(str(max(l)))
    if p == None:
        p = s

    i = s - p

    if i >= s:
        return l

    bins = [[] for _ in range(10)]

    for e in l:
        bins[int(str(e).zfill(s)[i])] += [e]

    return flatten([radix(b, p-1, s) for b in bins]

```



## QB64


```QB64

#lang QB64
'* don't be an a$$. Keep this credit notice with the source:
'* written/refactored by CodeGuy, 2018.
'* also works with negative numbers.
TESTN& = 63
A$ = ""
REDIM b(0 TO TESTN&) AS DOUBLE
FOR s& = -1 TO 1 STEP 2
    A$ = A$ + CHR$(13) + CHR$(10) + "Random order:"
    FOR i = 0 TO TESTN&
        b(i) = (1000 * RND) AND 1023
        IF i MOD 2 THEN b(i) = -b(i)
        IF i < TESTN& THEN
            A$ = A$ + LTRIM$(STR$(b(i))) + ","
        ELSE
            A$ = A$ + LTRIM$(STR$(b(i))) + CHR$(13) + CHR$(10)
        END IF
    NEXT
    RadixSort b(), 0, TESTN&, s&
    IF s& = -1 THEN
        A$ = A$ + "descending order" + CHR$(13) + CHR$(10)
    ELSE
        A$ = A$ + "ascending order" + CHR$(13) + CHR$(10)
    END IF

    FOR i = 0 TO TESTN&
        PRINT b(i);
        IF i < TESTN& THEN
            A$ = A$ + LTRIM$(STR$(b(i))) + ","
        ELSE
            A$ = A$ + LTRIM$(STR$(b(i))) + CHR$(13) + CHR$(10)
        END IF
    NEXT
NEXT
PRINT A$
TYPE MinMaxRec
    min AS LONG
    max AS LONG
END TYPE

SUB RadixSort (CGSortLibArr() AS DOUBLE, start&, finish&, order&)
    ArrayIsInteger CGSortLibArr(), start&, finish&, errindex&, errcon&
    IF errcon& THEN
        '* use another stable sort and sort anyway
        MergeSort CGSortLibArr(), start&, finish&, order&
    ELSE
        DIM RSMMrec AS MinMaxRec
        GetMinMaxArray CGSortLibArr(), start&, finish&, RSMMrec
        IF CGSortLibArr(RSMMrec.min) = CGSortLibArr(RSMMrec.max) THEN EXIT SUB '* no div0 bombs
        delta# = CGSortLibArr(RSMMrec.max) - CGSortLibArr(RSMMrec.min)
        DIM pow2 AS _UNSIGNED _INTEGER64
        DIM NtmpN AS _UNSIGNED _INTEGER64
        DIM Int64MaxShift AS _INTEGER64: Int64MaxShift = 2 ^ 64
        REDIM ct&(-1 TO 1)
        REDIM RadixCGSortLibArr(0 TO 1, finish& - start&) AS DOUBLE
        SELECT CASE order&
            CASE 1
                pow2 = Int64MaxShift
                bits& = LEN(Int64MaxShift) * 8
                DO UNTIL bits& < 0
                    FOR i& = start& TO finish&
                        NtmpN = Int64MaxShift * (CGSortLibArr(i&) - CGSortLibArr(RSMMrec.min)) / (delta#)
                        IF NtmpN AND pow2 THEN
                            tmpradix% = 1
                        ELSE
                            tmpradix% = 0
                        END IF
                        RadixCGSortLibArr(tmpradix%, ct&(tmpradix%)) = CGSortLibArr(i&)
                        ct&(tmpradix%) = ct&(tmpradix%) + 1
                    NEXT
                    c& = start&
                    FOR i& = 0 TO 1
                        FOR j& = 0 TO ct&(i&) - 1
                            CGSortLibArr(c&) = RadixCGSortLibArr(i&, j&)
                            c& = c& + 1
                        NEXT
                        ct&(i&) = 0
                    NEXT
                    pow2 = pow2 / 2
                    bits& = bits& - 1
                LOOP
            CASE ELSE
                pow2 = 1
                FOR bits& = 0 TO 63
                    FOR i& = start& TO finish&
                        NtmpN = Int64MaxShift * (CGSortLibArr(i&) - CGSortLibArr(RSMMrec.min)) / (delta#)
                        IF NtmpN AND pow2 THEN
                            tmpradix% = 1
                        ELSE
                            tmpradix% = 0
                        END IF
                        RadixCGSortLibArr(tmpradix%, ct&(tmpradix%)) = CGSortLibArr(i&)
                        ct&(tmpradix%) = ct&(tmpradix%) + 1
                    NEXT
                    c& = start&
                    FOR i& = 0 TO 1
                        FOR j& = 0 TO ct&(i&) - 1
                            CGSortLibArr(c&) = RadixCGSortLibArr(i&, j&)
                            c& = c& + 1
                        NEXT
                        ct&(i&) = 0
                    NEXT
                    pow2 = pow2 * 2
                NEXT
        END SELECT
        ERASE RadixCGSortLibArr, ct&
    END IF
END SUB

SUB ArrayIsInteger (CGSortLibArr() AS DOUBLE, start&, finish&, errorindex&, IsInt&)
    IsInt& = 1
    errorindex& = start&
    FOR IsIntegerS& = start& TO finish&
        IF CGSortLibArr(IsIntegerS&) MOD 1 THEN
            errorindex& = IsIntegerS&
            IsInt& = 0
            EXIT FUNCTION
        END IF
    NEXT
END FUNCTION

SUB MergeSort (CGSortLibArr() AS DOUBLE, start&, finish&, order&)
    SELECT CASE finish& - start&
        CASE IS > 31
            middle& = start& + (finish& - start&) \ 2
            MergeSort CGSortLibArr(), start&, middle&, order&
            MergeSort CGSortLibArr(), middle& + 1, finish&, order&
            'IF order& = 1 THEN
            EfficientMerge CGSortLibArr(), start&, finish&, order&
            'ELSE
            '    MergeRoutine CGSortLibArr(), start&, finish&, order&
            'END IF
        CASE IS > 0
            InsertionSort CGSortLibArr(), start&, finish&, order&
    END SELECT
END SUB

SUB EfficientMerge (right() AS DOUBLE, start&, finish&, order&)
    half& = start& + (finish& - start&) \ 2
    REDIM left(start& TO half&) AS DOUBLE '* hold the first half of the array in left() -- must be the same type as right()
    FOR LoadLeft& = start& TO half&
        left(LoadLeft&) = right(LoadLeft&)
    NEXT
    SELECT CASE order&
        CASE 1
            i& = start&
            j& = half& + 1
            insert& = start&
            DO
                IF i& > half& THEN '* left() exhausted
                    IF j& > finish& THEN '* right() exhausted
                        EXIT DO
                    ELSE
                        '* stuff remains in right to be inserted, so flush right()
                        WHILE j& <= finish&
                            right(insert&) = right(j&)
                            j& = j& + 1
                            insert& = insert& + 1
                        WEND
                        EXIT DO
                        '* and exit
                    END IF
                ELSE
                    IF j& > finish& THEN
                        WHILE i& < LoadLeft&
                            right(insert&) = left(i&)
                            i& = i& + 1
                            insert& = insert& + 1
                        WEND
                        EXIT DO
                    ELSE
                        IF right(j&) < left(i&) THEN
                            right(insert&) = right(j&)
                            j& = j& + 1
                        ELSE
                            right(insert&) = left(i&)
                            i& = i& + 1
                        END IF
                        insert& = insert& + 1
                    END IF
                END IF
            LOOP
        CASE ELSE
            i& = start&
            j& = half& + 1
            insert& = start&
            DO
                IF i& > half& THEN '* left() exhausted
                    IF j& > finish& THEN '* right() exhausted
                        EXIT DO
                    ELSE
                        '* stuff remains in right to be inserted, so flush right()
                        WHILE j& <= finish&
                            right(insert&) = right(j&)
                            j& = j& + 1
                            insert& = insert& + 1
                        WEND
                        EXIT DO
                        '* and exit
                    END IF
                ELSE
                    IF j& > finish& THEN
                        WHILE i& < LoadLeft&
                            right(insert&) = left(i&)
                            i& = i& + 1
                            insert& = insert& + 1
                        WEND
                        EXIT DO
                    ELSE
                        IF right(j&) > left(i&) THEN
                            right(insert&) = right(j&)
                            j& = j& + 1
                        ELSE
                            right(insert&) = left(i&)
                            i& = i& + 1
                        END IF
                        insert& = insert& + 1
                    END IF
                END IF
            LOOP
    END SELECT
    ERASE left
END SUB

SUB GetMinMaxArray (CGSortLibArr() AS DOUBLE, Start&, Finish&, GetMinMaxArray_minmax AS MinMaxRec)
    DIM GetGetMinMaxArray_minmaxArray_i AS LONG
    DIM GetMinMaxArray_n AS LONG
    DIM GetMinMaxArray_TT AS LONG
    DIM GetMinMaxArray_NMod2 AS INTEGER
    '* this is a workaround for the irritating malfunction
    '* of MOD using larger numbers and small divisors
    GetMinMaxArray_n = Finish& - Start&
    GetMinMaxArray_TT = GetMinMaxArray_n MOD 10000
    GetMinMaxArray_NMod2 = GetMinMaxArray_n - 10000 * ((GetMinMaxArray_n - GetMinMaxArray_TT) / 10000)
    IF (GetMinMaxArray_NMod2 MOD 2) THEN
        GetMinMaxArray_minmax.min = Start&
        GetMinMaxArray_minmax.max = Start&
        GetGetMinMaxArray_minmaxArray_i = Start& + 1
    ELSE
        IF CGSortLibArr(Start&) > CGSortLibArr(Finish&) THEN
            GetMinMaxArray_minmax.max = Start&
            GetMinMaxArray_minmax.min = Finish&
        ELSE
            GetMinMaxArray_minmax.min = Finish&
            GetMinMaxArray_minmax.max = Start&
        END IF
        GetGetMinMaxArray_minmaxArray_i = Start& + 2
    END IF

    WHILE GetGetMinMaxArray_minmaxArray_i < Finish&
        IF CGSortLibArr(GetGetMinMaxArray_minmaxArray_i) > CGSortLibArr(GetGetMinMaxArray_minmaxArray_i + 1) THEN
            IF CGSortLibArr(GetGetMinMaxArray_minmaxArray_i) > CGSortLibArr(GetMinMaxArray_minmax.max) THEN
                GetMinMaxArray_minmax.max = GetGetMinMaxArray_minmaxArray_i
            END IF
            IF CGSortLibArr(GetGetMinMaxArray_minmaxArray_i + 1) < CGSortLibArr(GetMinMaxArray_minmax.min) THEN
                GetMinMaxArray_minmax.min = GetGetMinMaxArray_minmaxArray_i + 1
            END IF
        ELSE
            IF CGSortLibArr(GetGetMinMaxArray_minmaxArray_i + 1) > CGSortLibArr(GetMinMaxArray_minmax.max) THEN
                GetMinMaxArray_minmax.max = GetGetMinMaxArray_minmaxArray_i + 1
            END IF
            IF CGSortLibArr(GetGetMinMaxArray_minmaxArray_i) < CGSortLibArr(GetMinMaxArray_minmax.min) THEN
                GetMinMaxArray_minmax.min = GetGetMinMaxArray_minmaxArray_i
            END IF
        END IF
        GetGetMinMaxArray_minmaxArray_i = GetGetMinMaxArray_minmaxArray_i + 2
    WEND
END SUB

SUB InsertionSort (CGSortLibArr() AS DOUBLE, start AS LONG, finish AS LONG, order&)
    DIM InSort_Local_ArrayTemp AS DOUBLE
    DIM InSort_Local_i AS LONG
    DIM InSort_Local_j AS LONG
    SELECT CASE order&
        CASE 1
            FOR InSort_Local_i = start + 1 TO finish
                InSort_Local_ArrayTemp = CGSortLibArr(InSort_Local_i)
                InSort_Local_j = InSort_Local_i - 1
                DO UNTIL InSort_Local_j < start
                    IF (InSort_Local_ArrayTemp < CGSortLibArr(InSort_Local_j)) THEN
                        CGSortLibArr(InSort_Local_j + 1) = CGSortLibArr(InSort_Local_j)
                        InSort_Local_j = InSort_Local_j - 1
                    ELSE
                        EXIT DO
                    END IF
                LOOP
                CGSortLibArr(InSort_Local_j + 1) = InSort_Local_ArrayTemp
            NEXT
        CASE ELSE
            FOR InSort_Local_i = start + 1 TO finish
                InSort_Local_ArrayTemp = CGSortLibArr(InSort_Local_i)
                InSort_Local_j = InSort_Local_i - 1
                DO UNTIL InSort_Local_j < start
                    IF (InSort_Local_ArrayTemp > CGSortLibArr(InSort_Local_j)) THEN
                        CGSortLibArr(InSort_Local_j + 1) = CGSortLibArr(InSort_Local_j)
                        InSort_Local_j = InSort_Local_j - 1
                    ELSE
                        EXIT DO
                    END IF
                LOOP
                CGSortLibArr(InSort_Local_j + 1) = InSort_Local_ArrayTemp
            NEXT
    END SELECT
END SUB

```



## Racket


```Racket

#lang Racket
(define (radix-sort l r)
  (define queues (for/vector #:length r ([_ r]) (make-queue)))
  (let loop ([l l] [R 1])
     (define all-zero? #t)
     (for ([x (in-list l)])
      (define x/R (quotient x R))
      (enqueue! (vector-ref queues (modulo x/R r)) x)
      (unless (zero? x/R) (set! all-zero? #f)))
    (if all-zero? l	
         (loop (let q-loop ([i 0])
                (define q (vector-ref queues i))
                (let dq-loop ()
                  (if (queue-empty? q)
                    (if (< i (sub1 r)) (q-loop (add1 i)) '())
                    (cons (dequeue! q) (dq-loop)))))
              (* R r)))))
(for/and ([i 10000]) ; run some tests on random lists with a random radix
  (define (make-random-list)
     (for/list ([i (+ 10 (random 10))]) (random 100000)))
  (define (sorted? l)
     (match l [(list) #t] [(list x) #t]
          [(list x y more ...) (and (<= x y) (sorted? (cons y more)))]))
  (sorted? (radix-sort (make-random-list) (+ 2 (random 98)))))
;; => #t, so all passed

```



## REXX

This REXX version also works with malformed integers.       '''7''',   '''007''',   '''+7''',   '''.7e1''',   '''7.0'''   are all treated as equal.

```rexx
/*REXX program performs a radix sort on an integer array (can be negative/zero/positive)*/
call gen                                         /*call subroutine to generate numbers. */
call radSort  n                                  /*invoke the  radix sort  subroutine.  */
     do j=1  for n;  say 'item'    right(j, w)    "after the radix sort:"    right(@.j, w)
     end   /*j*/                                 /* [↑]  display sorted items ───► term.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen: ILF=  0  2  3  4  5  5  7. 6  6  7 11  7 13  9  8  8 17  8 19  9 10 13 23  9 10 15 ,
           9 11 29 10 31 10 14 19 12 10 37 21 16 11 41 12 43 15 11 25 47 11 14 12 20 17 ,
          53 11 16 13 22 31 59 12 61 33 13 12 18 16 67 21 26 14 71 12 73 39 13 23 18 18 ,
          79 13 12 43 83 14 22 45 32 17 89 13 20 27 34 49 24 13 97 16 17 14  101        ,
         '22 103 19 15 55 107 13 109 18 40 15 113  -42'
              /*excluding -42, abbreviated above list is called the integer log function*/
     n= words(ILF)                                            /*    I────── L── F───────*/
     w= 0;         do m=1  for n;   _= word(ILF,m) +0;    @.m= _;    w= max(w, length(_) )
                   end   /*m*/;        return    /*W:  is the maximum width ↑ of numbers*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
radSort: procedure expose @. w;  parse arg size;   mote= c2d(' ');    #= 1;   !.#._n= size
!.#._b=1;
!.#._i=1;  do i=1  for size;  y=@.i;   @.i= right(abs(y), w, 0);  if y<0  then @.i= '-'@.i
           end  /*i*/                                             /* [↑]  negative case.*/

     do  while #\==0;   ctr.=0;  L='ffff'x;  low=!.#._b;  n=!.#._n;  $=!.#._i;   H=
     #=#-1                                                       /* [↑]   is the radix. */
           do j=low  for n;      parse var  @.j  =($)  _  +1;    ctr._=ctr._ + 1
           if ctr._==1 & _\==''  then do;  if _<<L  then L=_;    if _>>H  then H=_
                                      end  /*  ↑↑                                       */
           end   /*j*/                     /*  └┴─────◄───  <<   is a strict comparison.*/
     _=                                    /*      ┌──◄───  >>    " "    "        "     */
     if L>>H  then iterate                 /*◄─────┘                                    */
     if L==H & ctr._==0  then do; #= #+1;  !.#._b= low;  !.#._n= n;  !.#._i= $+1;  iterate
                              end
     L= c2d(L);   H= c2d(H);      ?= ctr._ + low;        top._= ?;          ts= mote
     max= L
                  do k=L  to H;   _= d2c(k,1);  c= ctr._    /* [↓]  swap 2 item radices.*/
                  if c>ts  then parse value  c k  with  ts max;     ?= ?+c;       top._= ?
                  end   /*k*/
     piv= low                                    /*set PIVot to the low part of the sort*/
             do  while piv<low+n
             it= @.piv
                        do forever;     parse var it  =($)  _  +1;         c= top._ -1
                        if piv>=c  then leave;   top._= c;    ?= @.c;    @.c= it;    it= ?
                        end   /*forever*/
             top._= piv;                          @.piv=it;          piv=piv + ctr._
             end   /*while piv<low+n */
     i= max
          do  until i==max;  _= d2c(i, 1);     i= i+1;     if i>H  then i= L;     d= ctr._
          if d<=mote  then do;         if d<2  then iterate;          b= top._
                             do k=b+1  for d-1;                       q= @.k
                               do j=k-1  by -1  to b  while q<<@.j;  jp= j+1;   @.jp= @.j
                               end   /*j*/
                                                                     jp= j+1;   @.jp= q
                             end     /*k*/
                           iterate
                           end
          #= #+1;       !.#._b= top._;       !.#._n= d;        !.#._i= $ + 1
          end   /*until i==max*/
     end        /*while #\==0 */
#= 0                                             /* [↓↓↓]  handle neg. and pos. arrays. */
        do i=size  by -1  to 1;        if @.i>=0  then iterate;  #=#+1;   @@.#=@.i
        end   /*i*/
        do j=1  for size;   if @.j>=0  then do;  #= #+1;   @@.#= @.j;  end;    @.j= @@.j+0
        end   /*j*/;                   return    /* [↑↑↑]  combine 2 lists into 1 list. */
```

{{out|output|text=    (with the middle section elided.)}}

(Output is shown at   <big>'''<sup>3</sup>/<sub>4</sub>'''</big>   size.)
<pre style="font-size:75%">
item   1 after the radix sort: -42
item   2 after the radix sort:   0
item   3 after the radix sort:   2
item   4 after the radix sort:   3
item   5 after the radix sort:   4
item   6 after the radix sort:   5
item   7 after the radix sort:   5
item   8 after the radix sort:   6
item   9 after the radix sort:   6
item  10 after the radix sort:   7
item  11 after the radix sort:   7
item  12 after the radix sort:   7
item  13 after the radix sort:   8
  .
  .
  .
(middle section elided.)
  .
  .
  .
item  92 after the radix sort:  40
item  93 after the radix sort:  41
item  94 after the radix sort:  43
item  95 after the radix sort:  43
item  96 after the radix sort:  45
item  97 after the radix sort:  47
item  98 after the radix sort:  49
item  99 after the radix sort:  53
item 100 after the radix sort:  55
item 101 after the radix sort:  59
item 102 after the radix sort:  61
item 103 after the radix sort:  67
item 104 after the radix sort:  71
item 105 after the radix sort:  73
item 106 after the radix sort:  79
item 107 after the radix sort:  83
item 108 after the radix sort:  89
item 109 after the radix sort:  97
item 110 after the radix sort: 101
item 111 after the radix sort: 103
item 112 after the radix sort: 107
item 113 after the radix sort: 109
item 114 after the radix sort: 113

```



## Ruby

Negative number handling courtesy the Tcl solution.

```ruby
class Array
  def radix_sort(base=10)
    ary = dup
    rounds = (Math.log(ary.minmax.map(&:abs).max)/Math.log(base)).floor + 1
    rounds.times do |i|
      buckets = Array.new(2*base){[]}
      base_i = base**i
      ary.each do |n|
        digit = (n/base_i) % base
        digit += base if 0<=n
        buckets[digit] << n
      end
      ary = buckets.flatten
      p [i, ary] if $DEBUG
    end
    ary
  end
  def radix_sort!(base=10)
    replace radix_sort(base)
  end
end

p [1, 3, 8, 9, 0, 0, 8, 7, 1, 6].radix_sort
p [170, 45, 75, 90, 2, 24, 802, 66].radix_sort
p [170, 45, 75, 90, 2, 24, -802, -66].radix_sort
p [100000, -10000, 400, 23, 10000].radix_sort
```

running with $DEBUG on produces:

```txt
[0, [0, 0, 1, 1, 3, 6, 7, 8, 8, 9]]
[0, 0, 1, 1, 3, 6, 7, 8, 8, 9]
[0, [170, 90, 2, 802, 24, 45, 75, 66]]
[1, [2, 802, 24, 45, 66, 170, 75, 90]]
[2, [2, 24, 45, 66, 75, 90, 170, 802]]
[2, 24, 45, 66, 75, 90, 170, 802]
[0, [-66, -802, 170, 90, 2, 24, 45, 75]]
[1, [-66, -802, 2, 24, 45, 170, 75, 90]]
[2, [-802, -66, 2, 24, 45, 75, 90, 170]]
[-802, -66, 2, 24, 45, 75, 90, 170]
[0, [-10000, 100000, 400, 10000, 23]]
[1, [-10000, 100000, 400, 10000, 23]]
[2, [-10000, 100000, 10000, 23, 400]]
[3, [-10000, 100000, 10000, 23, 400]]
[4, [-10000, 100000, 23, 400, 10000]]
[5, [-10000, 23, 400, 10000, 100000]]
[-10000, 23, 400, 10000, 100000]
```


another version (After sorting at the absolute value, it makes a negative order reverse.)

```ruby
class Array
  def radix_sort(base=10)
    ary = dup
    m, max = 1, ary.minmax.map(&:abs).max
    while m <= max
      buckets = Array.new(base){[]}
      ary.each {|n| buckets[(n.abs / m) % base] << n}
      ary = buckets.flatten
      m *= base
    end
    ary.partition{|n| n<0}.inject{|minus,plus| minus.reverse + plus}
  end
end
```


## Scala


```Scala
object RadixSort extends App {
  def sort(toBeSort: Array[Int]): Array[Int] = { // Loop for every bit in the integers
    var arr = toBeSort
    for (shift <- Integer.SIZE - 1 until -1 by -1) { // The array to put the partially sorted array into
      val tmp = new Array[Int](arr.length)
      // The number of 0s
      var j = 0
      // Move the 0s to the new array, and the 1s to the old one
      for (i <- arr.indices) // If there is a 1 in the bit we are testing, the number will be negative
        // If this is the last bit, negative numbers are actually lower
        if ((shift == 0) == (arr(i) << shift >= 0)) arr(i - j) = arr(i)
        else {
          tmp(j) = arr(i)
          j += 1
        }
      // Copy over the 1s from the old array
      arr.copyToArray(tmp, j, arr.length - j)

      // And now the tmp array gets switched for another round of sorting
      arr = tmp
    }
    arr
  }

  println(sort(Array(170, 45, 75, -90, -802, 24, 2, 66)).mkString(", "))
}
```



## Sidef

{{trans|Ruby}}

```ruby
class Array {
    method radix_sort(base=10) {
        var arr = self.clone
        var rounds = ([arr.minmax].map{.abs}.max.ilog(base) + 1)
        for i in (0..rounds) {
            var buckets = (2*base -> of {[]})
            var base_i = base**i
            for n in arr {
                var digit = (n/base_i % base)
                digit += base if (0 <= n)
                buckets[digit].append(n)
            }
            arr = buckets.flat
        }
        return arr
    }
}

for arr in [
    [1, 3, 8, 9, 0, 0, 8, 7, 1, 6],
    [170, 45, 75, 90, 2, 24, 802, 66],
    [170, 45, 75, 90, 2, 24, -802, -66],
    [100000, -10000, 400, 23, 10000],
] {
    say arr.radix_sort
}
```

{{out}}

```txt

[0, 0, 1, 1, 3, 6, 7, 8, 8, 9]
[2, 24, 45, 66, 75, 90, 170, 802]
[-802, -66, 2, 24, 45, 75, 90, 170]
[-10000, 23, 400, 10000, 100000]

```



## Tailspin


```tailspin

templates radixsort@{base:}
  sink bucketize
    def value: $;
    $ / $@radixsort.digit -> #
    <0 ?($value <0..>)>
      ..|@radixsort.positives: $value;
    <0>
      ..|@radixsort.negatives(-1): $value;
    <>
      def bucket: $ mod $base -> (<?($value<0..>)> $ + 1 ! <0> $base ! <> $ !);
      ..|@radixsort.buckets($bucket): $value;
      @radixsort.done: 0;
  end bucketize
  // Negatives get completed in wrong length-order, we need to collect by length and correct at the end
  @: { done: 1, digit: 1, positives: [], negatives: [[]], buckets: [1..$base -> []]};
  $... -> !bucketize
  $@.done -> #
  <1>
    [$@.negatives(-1..1:-1)... ..., $@.positives...] !
  <>
    def previous: $@.buckets;
    ..|@: {done: 1, digit: $@.digit * $base, buckets:[1..$base -> []]};
    ..|@.negatives: [];
    $previous... ... -> !bucketize
    $@.done -> #
end radixsort

[170, 45, 75, 91, 90, 92, 802, 24, 2, 66] -> radixsort@{base:10} -> !OUT::write
'
' -> !OUT::write
[-170, -45, -91, -90, -92, -802, -24, -2, -76] -> radixsort@{base:10} -> !OUT::write
'
' -> !OUT::write
[170, 45, 75, -91, -90, -92, -802, 24, 2, 66] -> radixsort@{base:10} -> !OUT::write
'
' -> !OUT::write
[170, 45, 75, -91, -90, -92, -802, 24, 2, 66] -> radixsort@{base:3} -> !OUT::write

```

{{out}}

```txt

[2, 24, 45, 66, 75, 90, 91, 92, 170, 802]
[-802, -170, -92, -91, -90, -76, -45, -24, -2]
[-802, -92, -91, -90, 2, 24, 45, 66, 75, 170]
[-802, -92, -91, -90, 2, 24, 45, 66, 75, 170]

```



## Tcl

{{trans|Python}}

```tcl
package require Tcl 8.5
proc splitByRadix {lst base power} {
    # create a list of empty lists to hold the split by digit
    set out [lrepeat [expr {$base*2}] {}]
    foreach item $lst {
	# pulls the selected digit
	set digit [expr {($item / $base ** $power) % $base + $base * ($item >= 0)}]
	# append the number to the list selected by the digit
	lset out $digit [list {*}[lindex $out $digit] $item]
    }
    return $out
}

# largest abs value element of a list
proc tcl::mathfunc::maxabs {lst} {
    set max [abs [lindex $lst 0]]
    for {set i 1} {$i < [llength $lst]} {incr i} {
	set v [abs [lindex $lst $i]]
	if {$max < $v} {set max $v}
    }
    return $max
}

proc radixSort {lst {base 10}} {
    # there are as many passes as there are digits in the longest number
    set passes [expr {int(log(maxabs($lst))/log($base) + 1)}]
    # For each pass...
    for {set pass 0} {$pass < $passes} {incr pass} {
	# Split by radix, then merge back into the list
	set lst [concat {*}[splitByRadix $lst $base $pass]]
    }
    return $lst
}
```

Demonstrations:

```tcl
puts [radixSort {1 3 8 9 0 0 8 7 1 6}]
puts [radixSort {170 45 75 90 2 24 802 66}]
puts [radixSort {170 45 75 90 2 24 -802 -66}]
```

Output:

```txt

0 0 1 1 3 6 7 8 8 9
2 24 45 66 75 90 170 802
-802 -66 2 24 45 75 90 170

```



## zkl

In place int sort, fairly light on garbage creation.

```zkl
fcn radixSort(ns){ // ints only, inplace, ns is mutable
   b:=(0).pump(20,List,List().copy);  // 20 [empty] buckets: -10..10
   z:=ns.reduce(fcn(a,b){ a.abs().max(b.abs()) },0); // |max or min of input|
   m:=1;
   while(z){
      ns.apply2('wrap(n){ b[(n/m)%10 +10].append(n) }); // sort on right digit
      ns.clear(); b.pump(ns.extend);		// slam buckets over src
      b.apply("clear");			     // reset buckets
      m*=10; z/=10;			// move sort digit left
   }
   ns
}
```


```zkl
radixSort(T(170, 45, 75, 90, 802, 2, 24, 66)).println();
radixSort(T(170, 45, 75, -90, -802, 24, 2, 66)).println();
```

{{out}}

```txt

L(2,24,45,66,75,90,170,802)
L(-802,-90,2,24,45,66,75,170)

```


{{omit from|GUISS}}
