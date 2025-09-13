+++
title = "Sorting algorithms/Cycle sort"
description = ""
date = 2018-11-05T05:34:26Z
aliases = []
[extra]
id = 17705
[taxonomies]
categories = ["task"]
tags = []
+++

{{draft task|Sorting Algorithms}}<!--Add this back when it gets promoted, also add a link to this page to the n^2 sorts in the template{{Sorting Algorithm}}-->
From the [[wp:Cycle sort|the Wikipedia entry]] on cycle sorting:
:'''Cycle sort''' is an in-place, unstable sorting algorithm, a comparison sort that is theoretically optimal in terms of the total number of writes to the original array, unlike any other in-place sorting algorithm. 
:It is based on the idea that the permutation to be sorted can be factored into cycles, which can individually be rotated to give a sorted result.

:Unlike nearly every other sort, items are never written elsewhere in the array simply to push them out of the way of the action. 
:Each value is either written zero times, if it's already in its correct position, or written one time to its correct position. 
:This matches the minimal number of overwrites required for a completed in-place sort.

:Minimizing the number of writes is useful when making writes to some huge data set is very expensive, such as with EEPROMs like Flash memory where each write reduces the lifespan of the memory.

## See also

* [http://www.youtube.com/watch?v=ZSJGf5Ngw18 Youtube] Visualization and audibilization of Cycle Sort algorithm.





## 360 Assembly

The program uses ASM structured macros and two ASSIST macros to keep the code as short as possible. 

```360asm
*        Cycle sort                26/06/2016
CYCLESRT CSECT
         USING  CYCLESRT,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         " 
         LR     R13,R15            "
         LA     RJ,1               jcycle=1
         L      R2,N               n
         BCTR   R2,0               n-1
         ST     R2,NM1             nm1=n-1
         DO WHILE=(C,RJ,LE,NM1)    do jcycle=1 to n-1
         LR     R1,RJ              jcycle
         SLA    R1,2               .
         L      RM,A-4(R1)         item=a(jcycle)
         LR     RK,RJ              kpos=jcycle       /*find{*/
         LA     RI,1(RJ)           i=jcycle+1
         DO WHILE=(C,RI,LE,N)      do i=jcycle+1 to n
         LR     R1,RI                i
         SLA    R1,2                 .
         L      R2,A-4(R1)           a(i)
         IF     CR,R2,LT,RM THEN     if a(i)<item then
         LA     RK,1(RK)               kpos=kpos+1
         ENDIF  ,                    end if
         LA     RI,1(RI)             i=i+1
         ENDDO  ,                  end do            /*}*/
         IF     CR,RK,NE,RJ THEN   if kpos^=jcycle then    ======
         LR     R1,RK              kpos              /*put{*/
         SLA    R1,2               .
         LA     R2,A-4(R1)         @a(kpos)
         DO WHILE=(C,RM,EQ,0(R2))  do while item=a(kpos)
         LA     RK,1(RK)             kpos=kpos+1
         LA     R2,4(R2)             @a(kpos)=@a(kpos)+4
         ENDDO  ,                  end do
         LR     R1,RK              kpos
         SLA    R1,2               .
         LA     R2,A-4(R1)         @a(kpos)
         L      RT,0(R2)           temp=a(kpos)
         ST     RM,0(R2)           a(kpos)=item
         LR     RM,RT              item=temp
         L      R2,WRITES          writes
         LA     R2,1(R2)           writes+1
         ST     R2,WRITES          writes=writes+1   /*}*/
         DO WHILE=(CR,RK,NE,RJ)    do while(kpos^=jcycle)   -----
         LR     RK,RJ                kpos=jcycle       /*find{*/
         LA     RI,1(RJ)             i=jcycle+1
         DO WHILE=(C,RI,LE,N)        do i=jcycle+1 to n
         LR     R1,RI                  i
         SLA    R1,2                   .
         L      R2,A-4(R1)             a(i)
         IF     CR,R2,LT,RM THEN       if a(i)<item then
         LA     RK,1(RK)                 kpos=kpos+1
         ENDIF  ,                       end if
         LA     RI,1(RI)               i=i+1
         ENDDO  ,                    end do            /*}*/
         LR     R1,RK                kpos              /*put{*/
         SLA    R1,2                 .
         LA     R2,A-4(R1)           @a(kpos)
         DO WHILE=(C,RM,EQ,0(R2))    do while item=a(kpos)
         LA     RK,1(RK)               kpos=kpos+1
         LA     R2,4(R2)               @a(kpos)=@a(kpos)+4
         ENDDO  ,                    end do
         LR     R1,RK                kpos
         SLA    R1,2                 .
         LA     R2,A-4(R1)           @a(kpos)
         L      RT,0(R2)             temp=a(kpos)
         ST     RM,0(R2)             a(kpos)=item
         LR     RM,RT                item=temp
         L      R2,WRITES            writes
         LA     R2,1(R2)             writes+1
         ST     R2,WRITES            writes=writes+1   /*}*/
         ENDDO  ,                  end while   ------------------
         ENDIF  ,                  end if   
### ===============

         LA     RJ,1(RJ)           jcycle=jcycle+1
         ENDDO  ,                  end do jcycle
         LA     R3,PG              pgi=0
         LA     RI,1               i=1
         DO     WHILE=(C,RI,LE,N)  do i=1 to n
         LR     R1,RI                i
         SLA    R1,2                 .
         L      R2,A-4(R1)           a(i)
         XDECO  R2,XDEC              edit a(i)
         MVC    0(4,R3),XDEC+8       output a(i)
         LA     R3,4(R3)             pgi=pgi+4
         LA     RI,1(RI)             i=i+1
         ENDDO  ,                  end do
         XPRNT  PG,L'PG            print buffer
         L      R1,WRITES          writes
         XDECO  R1,XDEC            edit writes
         MVC    XDEC(7),=CL7'writes='
         XPRNT  XDEC,L'XDEC        print buffer
         L      R13,4(0,R13)       epilog 
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
A     DC F'4',F'65',F'2',F'-31',F'0',F'99',F'2',F'83',F'782',F'1'
      DC F'45',F'82',F'69',F'82',F'104',F'58',F'88',F'112',F'89',F'74'
N        DC     A((N-A)/L'A)       number of items of a
NM1      DS     F                  n-1
PG       DC     CL80' '            buffer
XDEC     DS     CL12               temp for xdeco
WRITES   DC     F'0'               number of writes
         YREGS
RI       EQU    6                  i
RJ       EQU    7                  jcycle
RK       EQU    8                  kpos
RT       EQU    9                  temp
RM       EQU    10                 item
         END    CYCLESRT
```

```txt

 -31   0   1   2   2   4  45  58  65  69  74  82  82  83  88  89  99 104 112 782

```



## C

```C

#include <stdio.h>
#include <stdlib.h>

int cycleSort(int * list, size_t l_len);
void show_array(int * array, size_t a_len);

/*
 * Sort an array in place and return the number of writes.
 */
int cycleSort(int * list, size_t l_len)
{
  int writes = 0;

  /* Loop through the array to find cycles to rotate. */
  for (int cycleStart = 0; cycleStart < l_len - 1; ++cycleStart)
  {
    int item = list[cycleStart];
    int swap_tmp;

    /* Find where to put the item. */
    int pos = cycleStart;
    for (int i = cycleStart + 1; i < l_len; ++i)
    {
      if (list[i] < item)
      {
        ++pos;
      }
    }

    /* If the item is already there, this is not a cycle. */
    if (pos == cycleStart)
    {
      continue;
    }

    /* Otherwise, put the item there or right after any duplicates. */
    while (item == list[pos])
    {
      ++pos;
    }
    swap_tmp = list[pos];
    list[pos] = item;
    item = swap_tmp;
    ++writes;

    /* Rotate the rest of the cycle. */
    while (pos != cycleStart)
    {
      /* Find where to put the item. */
      pos = cycleStart;
      for (int i = cycleStart + 1; i < l_len; ++i)
      {
        if (list[i] < item)
        {
          ++pos;
        }
      }

      /* Put the item there or right after any duplicates. */
      while (item == list[pos])
      {
        ++pos;
      }
      swap_tmp = list[pos];
      list[pos] = item;
      item = swap_tmp;
      ++writes;
    }
  }

  return writes;
}

int main(int argc, char ** argv)
{
  int arr[] = { 0, 1, 2, 2, 2, 2, 1, 9, 3, 5, 5, 8, 4, 7, 0, 6, };
  int arr_k = sizeof(arr) / sizeof(arr[0]);
  int writes;

  show_array(arr, arr_k);
  writes = cycleSort(arr, arr_k);
  show_array(arr, arr_k);
  printf("writes: %d\n", writes);

  return 0;
}

void show_array(int * array, size_t a_len)
{
  for (int ix = 0; ix < a_len; ++ix)
  {
    printf("%d ", array[ix]);
  }
  putchar('\n');
  
  return;
}

```

```txt

0 1 2 2 2 2 1 9 3 5 5 8 4 7 0 6 
0 0 1 1 2 2 2 2 3 4 5 5 6 7 8 9 
writes: 10

```



## C++

Based on example code on Wikipedia

```Cpp

#include <time.h>
#include <iostream>
#include <vector>

using namespace std;

class cSort
{
public:
    void doIt( vector<unsigned> s )
    {
	sq = s; display(); c_sort();
	cout << "writes: " << wr << endl; display(); 
    }
private:
    void display()
    {
	copy( sq.begin(), sq.end(), ostream_iterator<unsigned>( std::cout, " " ) );
	cout << endl;
    }
    void c_sort()
    {
	wr = 0;
	unsigned it, p, vlen = static_cast<unsigned>( sq.size() ); 
	for( unsigned c = 0; c < vlen - 1; c++ )
	{
	    it = sq[c];
	    p = c;
	    for( unsigned d = c + 1; d < vlen; d++ )
		if( sq[d] < it ) p++;

	    if( c == p ) continue;

	    doSwap( p, it );

	    while( c != p )
	    {
		p = c;
		for( unsigned e = c + 1; e < vlen; e++ )
		    if( sq[e] < it ) p++;

		doSwap( p, it );
	    }
	}
    }
    void doSwap( unsigned& p, unsigned& it )
    {
	while( sq[p] == it ) p++;
	swap( it, sq[p] );
	wr++;
    }
    vector<unsigned> sq;
    unsigned wr;
};

int main(int argc, char ** argv)
{
    srand( static_cast<unsigned>( time( NULL ) ) );
    vector<unsigned> s;
    for( int x = 0; x < 20; x++ )
	s.push_back( rand() % 100 + 21 );

    cSort c; c.doIt( s );
    return 0;
}

```

```txt

38 119 38 33 33 28 24 101 108 120 99 59 69 24 117 22 90 94 78 75
writes: 19
22 24 24 28 33 33 38 38 59 69 75 78 90 94 99 101 108 117 119 120

```



## D

This version doesn't use Phobos algorithms beside 'swap'. Algorithms can be used to find where to put the item1 and elsewhere.
```d
import std.stdio, std.algorithm;

/// Sort an array in place and return the number of writes.
uint cycleSort(T)(T[] data) pure nothrow @safe @nogc {
    typeof(return) nWrites = 0;

    // Loop through the data to find cycles to rotate.
    foreach (immutable cycleStart, item1; data) {
        // Find where to put the item1.
        size_t pos = cycleStart;
        foreach (item2; data[cycleStart + 1 .. $])
            if (item2 < item1)
                pos++;

        // If the item1 is already there, this is not a cycle.
        if (pos == cycleStart)
            continue;

        // Otherwise, put the item1 there or right after any duplicates.
        while (item1 == data[pos])
            pos++;
        data[pos].swap(item1);
        nWrites++;

        // Rotate the rest of the cycle.
        while (pos != cycleStart) {
            // Find where to put the item1.
            pos = cycleStart;
            foreach (item2; data[cycleStart + 1 .. $])
                if (item2 < item1)
                    pos++;

            // Put the item1 there or right after any duplicates.
            while (item1 == data[pos])
                pos++;
            data[pos].swap(item1);
            nWrites++;
        }
    }

    return nWrites;
}

void main() {
    immutable x = [0, 1, 2, 2, 2, 2, 1, 9, 3.5, 5, 8, 4, 7, 0, 6];
    auto xs = x.dup;
    immutable nWrites = xs.cycleSort;

    if (!xs.isSorted) {
        "Wrong order!".writeln;
    } else {
        writeln(x, "\nIs correctly sorted using cycleSort to:");
        writefln("%s\nusing %d writes.", xs, nWrites);
    }
}
```

```txt
[0, 1, 2, 2, 2, 2, 1, 9, 3.5, 5, 8, 4, 7, 0, 6]
Is correctly sorted using cycleSort to:
[0, 0, 1, 1, 2, 2, 2, 2, 3.5, 4, 5, 6, 7, 8, 9]
using 10 writes.
```



## Elixir

```elixir
defmodule Sort do
  def cycleSort(list) do
    tuple = List.to_tuple(list)
    # Loop through the array to find cycles to rotate.
    {data,writes} = Enum.reduce(0 .. tuple_size(tuple)-2, {tuple,0}, fn cycleStart,{data,writes} ->
      item = elem(data, cycleStart)
      pos = find_pos(data, cycleStart, item)
      if pos == cycleStart do
        # If the item is already there, this is not a cycle.
        {data, writes}
      else
        # Otherwise, put the item there or right after any duplicates.
        {data, item} = swap(data, pos, item)
        rotate(data, cycleStart, item, writes+1)
      end
    end)
    {Tuple.to_list(data), writes}
  end
  
  # Rotate the rest of the cycle.
  defp rotate(data, cycleStart, item, writes) do
    pos = find_pos(data, cycleStart, item)
    {data, item} = swap(data, pos, item)
    if pos==cycleStart, do: {data, writes+1},
                      else: rotate(data, cycleStart, item, writes+1)
  end
  
  # Find where to put the item.
  defp find_pos(data, cycleStart, item) do
    cycleStart + Enum.count(cycleStart+1..tuple_size(data)-1, &elem(data, &1) < item)
  end
  
  # Put the item there or right after any duplicates.
  defp swap(data, pos, item) when elem(data, pos)==item, do: swap(data, pos+1, item)
  defp swap(data, pos, item) do
    {put_elem(data, pos, item), elem(data, pos)}
  end
end

IO.inspect a = [0, 1, 2, 2, 2, 2, 1, 9, 3.5, 5, 8, 4, 7, 0, 6]
{b, writes} = Sort.cycleSort(a)
IO.puts "writes : #{writes}"
IO.inspect b
```


```txt

[0, 1, 2, 2, 2, 2, 1, 9, 3.5, 5, 8, 4, 7, 0, 6]
writes : 10
[0, 0, 1, 1, 2, 2, 2, 2, 3.5, 4, 5, 6, 7, 8, 9]

```



## FreeBASIC

Uses algorithm in Wikipedia article:

```freebasic
' FB 1.05.0 Win64

' sort an array in place and return the number of writes
Function cycleSort(array() As Integer) As Integer
  Dim length As Integer = UBound(array) - LBound(array) + 1
  If Length = 0 Then Return 0
  Dim As Integer item, position, writes = 0

  ' loop through the array to find cycles to rotate
  For cycleStart As Integer = LBound(array) To UBound(array) - 1
    item = array(cycleStart)

    ' find where to put the item
    position = cycleStart
    For i As Integer = cycleStart + 1 To UBound(array)
      If array(i) < item Then position += 1
    Next i

    ' If the item is already there, this is not a cycle
    If position = cycleStart Then Continue For
    
    ' Otherwise, put the item there or right after any duplicates
    While item = array(position)
      position += 1
    Wend
    Swap array(position), item
    writes += 1

    'rotate the rest of the cycle
    While position <> cycleStart
      ' Find where to put the item
      position = cycleStart
      For i As Integer = cycleStart + 1 To UBound(array)
        If array(i) < item Then position += 1
      Next i

      ' Put the item there or right after any duplicates
      While item = array(position)
        position += 1
      Wend
      Swap array(position), item
      writes +=1
    Wend
  Next cycleStart
 
  Return writes
End Function

Sub printArray(array() As Integer)
  For i As Integer = LBound(array) To UBound(array)
    Print Str(array(i)); " ";
  Next
  Print
End Sub

Dim array(1 To 16) As Integer = {0, 1, 2, 2, 2, 2, 1, 9, 3, 5, 5, 8, 4, 7, 0, 6}
printArray(array())
Dim writes As Integer = cycleSort(array())
Print "After sorting with"; writes; " writes :"
printArray(array())
Print
Dim array2(1 To 20) As Integer = {38, 119, 38, 33, 33, 28, 24, 101, 108, 120, 99, 59, 69, 24, 117, 22, 90, 94, 78, 75}
printArray(array2())
writes = cycleSort(array2())
Print "After sorting with"; writes; " writes :"
printArray(array2())
Print 
Print "Press any key to quit"
Sleep
```


```txt

0 1 2 2 2 2 1 9 3 5 5 8 4 7 0 6
After sorting with 10 writes :
0 0 1 1 2 2 2 2 3 4 5 5 6 7 8 9

38 119 38 33 33 28 24 101 108 120 99 59 69 24 117 22 90 94 78 75
After sorting with 19 writes :
22 24 24 28 33 33 38 38 59 69 75 78 90 94 99 101 108 117 119 120

```



## Go

This implementation was translated from the example code on Wikipedia.


```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func cyclesort(ints []int) int {
	writes := 0

	for cyclestart := 0; cyclestart < len(ints)-1; cyclestart++ {
		item := ints[cyclestart]

		pos := cyclestart

		for i := cyclestart + 1; i < len(ints); i++ {
			if ints[i] < item {
				pos++
			}
		}

		if pos == cyclestart {
			continue
		}

		for item == ints[pos] {
			pos++
		}

		ints[pos], item = item, ints[pos]

		writes++

		for pos != cyclestart {
			pos = cyclestart
			for i := cyclestart + 1; i < len(ints); i++ {
				if ints[i] < item {
					pos++
				}
			}

			for item == ints[pos] {
				pos++
			}

			ints[pos], item = item, ints[pos]
			writes++
		}
	}

	return writes
}

func main() {
	rand.Seed(time.Now().Unix())

	ints := rand.Perm(10)

	fmt.Println(ints)
	fmt.Printf("writes %d\n", cyclesort(ints))
	fmt.Println(ints)
}
```


```txt

[1 9 3 5 8 4 7 0 6 2]
writes 10
[0 1 2 3 4 5 6 7 8 9]

```


Note: output may be different due to the random numbers used.


## J


J's sort is natively a single write sort, but it assigns the whole array at once. 
It would be trivial do the writes one at a time, and to avoid updating values which are not changed:


```J
noncyc=:3 :0
  writes=. 0
  for_item. /:~y do.
    if. item ~: item_index{y do.
      writes=. writes+1
      y=.item item_index} y
    end.
  end.
  smoutput (":writes),' writes'
  y 
)
```


```J
   noncyc 9 8 15 17 4 0 1 2 17 9 3 12 11 12 19 15 3 9 16 9
17 writes
0 1 2 3 3 4 8 9 9 9 9 11 12 12 15 15 16 17 17 19
```


Meanwhile, if we just wanted the "value at a time swapping" mechanism, 
an idiomatic approach might look something like this:


```j
cyc0=:3 :0
  c=. (#~ 1 < #@>)C./:/: y
  writes=. 0
  for_box. c do.
    inds=. >box
    v=. ({:inds) { y
    for_ind. inds do.
      writes=. writes+1
      t=. ind{ y
      y=. v ind} y
      v=. t
    end.
  end.
  smoutput (":writes),' writes'
  y
)
```


```J
   cyc0 9 8 15 17 4 0 1 2 17 9 3 12 11 12 19 15 3 9 16 9
18 writes
0 1 2 3 3 4 8 9 9 9 9 11 12 12 15 15 16 17 17 19
```


This gives us an extra write, because we're using a generic cycle abstraction.

Also that's still a bit different from the wikipedia algorithm. 
We might model the wikipedia algorithm like this:


```J
cyc1=:3 :0
  writes=. 0
  for_index. i.(#y)-1 do.
    item=. index{y
    adj=. item+/ .>(1+index)}.y
    if. 0<adj do.
      pos=. index+adj
      while. item=pos{y do. pos=.pos+1 end.
      writes=. writes+1
      t=. pos{y
      y=. item pos} y
      item=. t
      while. pos ~: index do.
        pos=. index+item+/ .>(1+index)}.y
        while. item=pos{y do. pos=.pos+1 end.
        writes=. writes+1
        t=. pos{y
        y=. item pos} y
        item=. t
      end.
    end.
  end.
  smoutput (":writes),' writes'
  y
)
```


```J
   cyc1 9 8 15 17 4 0 1 2 17 9 3 12 11 12 19 15 3 9 16 9
17 writes
0 1 2 3 3 4 8 9 9 9 9 11 12 12 15 15 16 17 17 19
```


Note that we've saved a write in this case, by following the wikipedia algorithm.


## Java


```java
import java.util.Arrays;

public class CycleSort {

    public static void main(String[] args) {
        int[] arr = {5, 0, 1, 2, 2, 3, 5, 1, 1, 0, 5, 6, 9, 8, 0, 1};

        System.out.println(Arrays.toString(arr));

        int writes = cycleSort(arr);
        System.out.println(Arrays.toString(arr));
        System.out.println("writes: " + writes);
    }

    static int cycleSort(int[] a) {
        int writes = 0;

        for (int cycleStart = 0; cycleStart < a.length - 1; cycleStart++) {
            int val = a[cycleStart];

            // count the number of values that are smaller than val
            // since cycleStart
            int pos = cycleStart;
            for (int i = cycleStart + 1; i < a.length; i++)
                if (a[i] < val)
                    pos++;

            // there aren't any
            if (pos == cycleStart)
                continue;

            // skip duplicates
            while (val == a[pos])
                pos++;

            // put val into final position
            int tmp = a[pos];
            a[pos] = val;
            val = tmp;
            writes++;

            // repeat as long as we can find values to swap
            // otherwise start new cycle
            while (pos != cycleStart) {
                pos = cycleStart;
                for (int i = cycleStart + 1; i < a.length; i++)
                    if (a[i] < val)
                        pos++;

                while (val == a[pos])
                    pos++;

                tmp = a[pos];
                a[pos] = val;
                val = tmp;
                writes++;
            }
        }
        return writes;
    }
}
```


```txt
[5, 0, 1, 2, 2, 3, 5, 1, 1, 0, 5, 6, 9, 8, 0, 1]
[0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 5, 5, 5, 6, 8, 9]
writes: 14
```



## Julia

```julia
function cyclesort!(v::Vector)
    writes = 0
    for (cyclestart, item) in enumerate(v)
        pos = cyclestart
        for item2 in v[cyclestart + 1:end]
            if item2 < item pos += 1 end
        end

        if pos == cyclestart continue end
        while item == v[pos]
            pos += 1
        end
        v[pos], item = item, v[pos]
        writes += 1

        while pos != cyclestart
            pos = cyclestart
            for item2 in v[cyclestart + 1:end]
                if item2 < item pos += 1 end
            end
            while item == v[pos]
                pos += 1
            end

            v[pos], item = item, v[pos]
            writes += 1
        end
    end
    return v
end

v = rand(-10:10, 10)
println("# unordered: $v\n -> ordered: ", cyclesort!(v))
```


```txt
# unordered: [-2, -2, -5, -9, 8, 7, 2, -1, 3, -6]
 -> ordered: [-9, -6, -5, -2, -2, -1, 2, 3, 7, 8]
```



## Kotlin

Translation of the algorithm in the Wikipedia article:

```scala
// version 1.1.0

/** Sort an array in place and return the number of writes */
fun <T : Comparable<T>> cycleSort(array: Array<T>): Int {
    var writes = 0

    // Loop through the array to find cycles to rotate.
    for (cycleStart in 0 until array.size - 1) {
        var item = array[cycleStart]

        // Find where to put the item.
        var pos = cycleStart
        for (i in cycleStart + 1 until array.size) if (array[i] < item) pos++

        // If the item is already there, this is not a cycle.
        if (pos == cycleStart) continue

        // Otherwise, put the item there or right after any duplicates.
        while (item == array[pos]) pos++
        val temp = array[pos]
        array[pos] = item
        item = temp
        writes++

        // Rotate the rest of the cycle.
        while (pos != cycleStart) {
            // Find where to put the item.
            pos = cycleStart
            for (i in cycleStart + 1 until array.size) if (array[i] < item) pos++

            // Otherwise, put the item there or right after any duplicates.
            while (item == array[pos]) pos++
            val temp2 = array[pos]
            array[pos] = item
            item = temp2
            writes++
        }
    }
    return writes
}

fun <T : Comparable<T>> printResults(array: Array<T>) {
    println(array.asList())
    val writes = cycleSort(array)
    println("After sorting with $writes writes:")
    println(array.asList())
    println()
}

fun main(args: Array<String>) {
    val array = arrayOf(0, 1, 2, 2, 2, 2, 1, 9, 3, 5, 5, 8, 4, 7, 0, 6)
    printResults(array)
    val array2 = arrayOf(5, 0, 1, 2, 2, 3, 5, 1, 1, 0, 5, 6, 9, 8, 0, 1)
    printResults(array2)
    val array3 = "the quick brown fox jumps over the lazy dog".split(' ').toTypedArray()
    printResults(array3)
    val array4 = "sphinx of black quartz judge my vow".replace(" ", "").toCharArray().distinct().toTypedArray()
    printResults(array4)
}
```


```txt

[0, 1, 2, 2, 2, 2, 1, 9, 3, 5, 5, 8, 4, 7, 0, 6]
After sorting with 10 writes:
[0, 0, 1, 1, 2, 2, 2, 2, 3, 4, 5, 5, 6, 7, 8, 9]

[5, 0, 1, 2, 2, 3, 5, 1, 1, 0, 5, 6, 9, 8, 0, 1]
After sorting with 14 writes:
[0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 5, 5, 5, 6, 8, 9]

[the, quick, brown, fox, jumps, over, the, lazy, dog]
After sorting with 8 writes:
[brown, dog, fox, jumps, lazy, over, quick, the, the]

[s, p, h, i, n, x, o, f, b, l, a, c, k, q, u, r, t, z, j, d, g, e, m, y, v, w]
After sorting with 26 writes:
[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]

```



## NetRexx

Direct translation of [[wp:Cycle sort|the Wikipedia entry]] example

```NetRexx
/* Rexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Sort an array in place and return the number of writes.
method cycleSort(array = Rexx[]) public static
  writes = 0

  -- Loop through the array to find cycles to rotate.
  loop cycleStart = 0 to array.length - 1 - 1
    item = array[cycleStart]

    -- Find where to put the item.
    pos = cycleStart
    loop i = cycleStart + 1 to array.length - 1
      if array[i] < item then
        pos = pos + 1
      end i

    -- If the item is already there, this is not a cycle.
    if pos == cycleStart then
      iterate

    -- Otherwise, put the item there or right after any duplicates.
    loop while item == array[pos]
      pos = pos + 1
      end
    swap_tmp = array[pos]
    array[pos] = item
    item = swap_tmp
    writes = writes + 1

    -- Rotate the rest of the cycle.
    loop while pos \= cycleStart

      -- Find where to put the item.
      pos = cycleStart
      loop i = cycleStart + 1 to array.length - 1
        if array[i] < item then
          pos = pos + 1
        end i

      -- Put the item there or right after any duplicates.
      loop while item == array[pos]
        pos = pos + 1
        end
      swap_tmp = array[pos]
      array[pos] = item
      item = swap_tmp
      writes = writes + 1

      end

    end cycleStart
  return writes

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) public static
  samples = ArrayList()
  samples.add([1, 9, 3, 5, 8, 4, 7, 0, 6, 2])
  samples.add([0, 1, 2, 2, 2, 2, 1, 9, 3.5, 5, 8, 4, 7, 0, 6])
  samples.add(['Greygill Hole', 'Ogof Draenen', 'Ogof Ffynnon Ddu', 'Malham Tarn Pot'])
  samples.add([-3.14 ,3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6, 2, 6, 4, 3, 3, 8, 3, 2, 7, 9, 5, 0, 2, 8, 8, 4])
  samples.add(['George Washington: Virginia', 'John Adams: Massachusetts', 'Thomas Jefferson: Virginia', 'James Madison: Virginia', 'James Monroe: Virginia'])

  list = Rexx[]
  loop i_ = 0 to samples.size() - 1
    list = Rexx[] samples.get(i_)
    say 'Input list ' Arrays.asList(list)
    writes = cycleSort(list)
    say 'Sorted list' Arrays.asList(list)
    say 'Total number of writes:' writes
    say
    end i_
  return

```

```txt

Input list  [1, 9, 3, 5, 8, 4, 7, 0, 6, 2]
Sorted list [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
Total number of writes: 10

Input list  [0, 1, 2, 2, 2, 2, 1, 9, 3.5, 5, 8, 4, 7, 0, 6]
Sorted list [0, 0, 1, 1, 2, 2, 2, 2, 3.5, 4, 5, 6, 7, 8, 9]
Total number of writes: 10

Input list  [Greygill Hole, Ogof Draenen, Ogof Ffynnon Ddu, Malham Tarn Pot]
Sorted list [Greygill Hole, Malham Tarn Pot, Ogof Draenen, Ogof Ffynnon Ddu]
Total number of writes: 3

Input list  [-3.14, 3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6, 2, 6, 4, 3, 3, 8, 3, 2, 7, 9, 5, 0, 2, 8, 8, 4]
Sorted list [-3.14, 0, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9]
Total number of writes: 34

Input list  [George Washington: Virginia, John Adams: Massachusetts, Thomas Jefferson: Virginia, James Madison: Virginia, James Monroe: Virginia]
Sorted list [George Washington: Virginia, James Madison: Virginia, James Monroe: Virginia, John Adams: Massachusetts, Thomas Jefferson: Virginia]
Total number of writes: 4
```



## Nim


```nim
proc cycleSort[T](a: var openArray[T]): int =
  var position, writes: int = 0
  var item: T
  for cycleStart in a.low..a.high - 1:
    item = a[cycleStart]
    position = cycleStart
    for i in cycleStart + 1..a.high:
      if a[i] < item:
        inc position
    if position == cycleStart:
      continue
    while item == a[position]:
      inc position
    swap a[position], item
    inc writes
    while position != cycleStart:
      position = cycleStart
      for i in cycleStart + 1..a.high:
        if a[i] < item:
          inc position
      while item == a[position]:
        inc position
      swap a[position], item
      inc writes
  result = writes

var array1 = @[1, 9, 3, 5, 8, 4, 7, 0, 6, 2]
var array2 = @[0'f64, 1, 2, 2, 2, 2, 1, 9, 3.5, 5, 8, 4, 7, 0, 6]
var array3 = @["Greygill Hole", "Ogof Draenen", "Ogof Ffynnon Ddu", "Malham Tarn Pot"]
var array4 = @[-3.14 ,3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6, 2, 6, 4, 3, 3, 8, 3, 2, 7, 9, 5, 0, 2, 8, 8, 4]
var array5 = @["George Washington: Virginia", "John Adams: Massachusetts", "Thomas Jefferson: Virginia", "James Madison: Virginia", "James Monroe: Virginia"]
var writes = 0

echo "Original: ", $array1
writes = array1.cycleSort()
echo "Sorted:   ", $array1
echo "Total number of writes: ", writes, "\n"

echo "Original: ", $array2
writes = array2.cycleSort()
echo "Sorted:   ", $array2
echo "Total number of writes: ", writes, "\n"

echo "Original: ", $array3
writes = array3.cycleSort()
echo "Sorted:   ", $array3
echo "Total number of writes: ", writes, "\n"

echo "Original: ", $array4
writes = array4.cycleSort()
echo "Sorted:   ", $array4
echo "Total number of writes: ", writes, "\n"

echo "Original: ", $array5
writes = array5.cycleSort()
echo "Sorted:   ", $array5
echo "Total number of writes: ", writes
```


```txt
Original: @[1, 9, 3, 5, 8, 4, 7, 0, 6, 2]
Sorted:   @[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
Total number of writes: 10

Original: @[0.0, 1.0, 2.0, 2.0, 2.0, 2.0, 1.0, 9.0, 3.5, 5.0, 8.0, 4.0, 7.0, 0.0, 6.0]
Sorted:   @[0.0, 0.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 3.5, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]
Total number of writes: 10

Original: @["Greygill Hole", "Ogof Draenen", "Ogof Ffynnon Ddu", "Malham Tarn Pot"]
Sorted:   @["Greygill Hole", "Malham Tarn Pot", "Ogof Draenen", "Ogof Ffynnon Ddu"]
Total number of writes: 3

Original: @[-3.14, 3.0, 1.0, 4.0, 1.0, 5.0, 9.0, 2.0, 6.0, 5.0, 3.0, 5.0, 8.0, 9.0, 7.0, 9.0, 3.0, 2.0, 3.0, 8.0, 4.0, 6.0, 2.0, 6.0, 4.0, 3.0, 3.0, 8.0, 3.0, 2.0, 7.0, 9.0, 5.0, 0.0, 2.0, 8.0, 8.0, 4.0]
Sorted:   @[-3.14, 0.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 2.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 3.0, 4.0, 4.0, 4.0, 4.0, 5.0, 5.0, 5.0, 5.0, 6.0, 6.0, 6.0, 7.0, 7.0, 8.0, 8.0, 8.0, 8.0, 8.0, 9.0, 9.0, 9.0, 9.0]
Total number of writes: 34

Original: @["George Washington: Virginia", "John Adams: Massachusetts", "Thomas Jefferson: Virginia", "James Madison: Virginia", "James Monroe: Virginia"]
Sorted:   @["George Washington: Virginia", "James Madison: Virginia", "James Monroe: Virginia", "John Adams: Massachusetts", "Thomas Jefferson: Virginia"]
Total number of writes: 4
```



## Objeck

```objeck
class Test {
  function : Main(args : String[]) ~ Nil {
    arr := [5, 0, 1, 2, 2, 3, 5, 1, 1, 0, 5, 6, 9, 8, 0, 1];
    arr->ToString()->PrintLine();
    writes := CycleSort(arr);
    "writes: {$writes}"->PrintLine();
    arr->ToString()->PrintLine();    
  }
  
  function : CycleSort(a : Int[]) ~ Int {
    writes := 0;
 
    for(cycleStart := 0; cycleStart < a->Size() - 1; cycleStart+=1;) {
      val := a[cycleStart];
 
      pos := cycleStart;
      for(i := cycleStart + 1; i < a->Size(); i+=1;) {
        if(a[i] < val) {
          pos++;
        };
      };
    
      if(pos <> cycleStart) {
        while(val = a[pos]) {
          pos+=1;
        };
    
        tmp := a[pos];
        a[pos] := val;
        val := tmp;
        writes+=1;

        while(pos <> cycleStart) {
          pos := cycleStart;
          for(i := cycleStart + 1; i < a->Size(); i+=1;) {
            if(a[i] < val) {
              pos+=1;
            };
          };
    
          while(val = a[pos]) {
            pos++;
          };
    
          tmp := a[pos];
          a[pos] := val;
          val := tmp;
          writes++;
        };
      };
    };
    
    return writes;
  }
}
```



```txt

[5,0,1,2,2,3,5,1,1,0,5,6,9,8,0,1]
writes: 14
[0,0,0,1,1,1,1,2,2,3,5,5,5,6,8,9]

```



## ooRexx


```oorexx
/*REXX program demonstrates a cycle sort on a list of numbers**********
* 13.06.2014 Walter Pachl
* Modified from Rexx Version 2
* ooRexx allows to pass a stemmed variable by reference
* swapping variables uses a temporary instead of the parse.
**********************************************************************/
  a.1='George Washington  Virginia'
  a.2='John Adams  Massachusetts'
  a.3='Thomas Jefferson  Virginia'
  a.4='James Madison  Virginia'
  a.5='James Monroe  Virginia'
  n=5
  Call show 'Unsorted list: '
  w=sortcycle(a.,n)
  Say 'sorted'
  Call show 'Sorted list'
  Say ' '
  Say 'This took' w 'writes.'
  Exit

sortcycle: Procedure
  Use Arg a.,n
  writes=0
  Do c=1 For n
    x=a.c
    p=c
    x=a.c
    Do j=c+1 To n
      If a.j<x Then
        p=p+1
      End
    If p==c Then
      Iterate
    Do While x==a.p
      p=p+1
      End
    t=x
    x=a.p
    a.p=t
    writes=writes+1
    Do While p\==c
      p=c
      Do k=c+1 To n
        If a.k<x Then
          p=p+1
        End
      Do While x==a.p
        p=p+1
        End
      t=x
      x=a.p
      a.p=t
      writes=writes+1
      End
    End
  Return writes

show:
  Parse Arg hdr
  Say ' '
  Say hdr
  Do i=1 To n
    Say format(i,2) a.i
    End
  Return
```

```txt

Unsorted list:
 1 George Washington  Virginia
 2 John Adams  Massachusetts
 3 Thomas Jefferson  Virginia
 4 James Madison  Virginia
 5 James Monroe  Virginia
sorted

Sorted list
 1 George Washington  Virginia
 2 James Madison  Virginia
 3 James Monroe  Virginia
 4 John Adams  Massachusetts
 5 Thomas Jefferson  Virginia

This took 4 writes.
```



## Perl

This is based on the Wikipedia pseudocode.

```perl
use strict;
use warnings;

sub cycleSort(@) {
	my ($array) = @_;
	my $writes = 0;
	
	my @alreadysorted;
	
	# For each index except the last:
	for my $start ( 0 .. $#$array - 1 ) {
		next if $alreadysorted[$start];
		my $item = $array->[$start];
		# If there are N items less than $item, then we
		# must move $item N items rightward.
		my $pos = $start + grep $array->[$_] lt $item, $start + 1 .. $#$array;
		# If the item is where it should be, continue.
		next if $pos == $start;
		# If $item is one of several repetitions, move it to the right
		# of the last repeat.
		++$pos while $item eq $array->[ $pos ];
		# Store $item at $pos, where it belongs, and fetch the
		# value that had been at $pos, and put it in $item.
		($array->[ $pos ], $item) = ($item, $array->[ $pos ]);
		++$writes;

		# Whatever $item is now, it certainly doesn't belong at $pos;
		do {
			# Find the correct $pos,
			$pos = $start + grep $array->[$_] lt $item, $start+1 .. $#$array;
			++$pos while $item eq $array->[ $pos ];
			# Swap the value there with $item,
			($array->[ $pos ] , $item ) = ($item, $array->[ $pos ]);
			# And mark $pos as having the correct value in it..
			$alreadysorted[ $pos ] = 1;
			++$writes;
			# The loop ends after we have just written an item to $start
		} while $pos != $start;
	}
	$writes;
}

use List::Util 'shuffle';
my @test = shuffle( ('a'..'z') x 2 );
print "Before sorting: @test\n";
print "There were ", cycleSort( \@test ), " writes\n";
print "After  sorting: @test\n";

```

```txt
Before sorting: a t d b f g y l t p w c r r x i y j k i z q e v a f o q j u x k m h s u v z g m b o l e n h p n c s w d
There were 50 writes
After  sorting: a a b b c c d d e e f f g g h h i i j j k k l l m m n n o o p p q q r r s s t t u u v v w w x x y y z z
```



## Perl 6


```perl6
sub cycle_sort ( @nums ) {
    my $writes = 0;

    # Loop through the array to find cycles to rotate.
    for @nums.kv -> $cycle_start, $item is copy {

        # Find where to put the item.
        my $pos = $cycle_start
                + @nums[ $cycle_start ^.. * ].grep: * < $item;

        # If the item is already there, this is not a cycle.
        next if $pos == $cycle_start;

        # Otherwise, put the item there or right after any duplicates.
        $pos++ while $item == @nums[$pos];
        ( @nums[$pos], $item ) .= reverse;
        $writes++;

        # Rotate the rest of the cycle.
        while $pos != $cycle_start {

            # Find where to put the item.
            $pos = $cycle_start
                 + @nums[ $cycle_start ^.. * ].grep: * < $item;

            # Put the item there or right after any duplicates.
            $pos++ while $item == @nums[$pos];
            ( @nums[$pos], $item ) .= reverse;
            $writes++;
        }
    }

    return $writes;
}

my @a = <0 1 2 2 2 2 1 9 3.5 5 8 4 7 0 6>;

say @a;
say 'writes ', cycle_sort(@a);
say @a;

```

```txt
0 1 2 2 2 2 1 9 3.5 5 8 4 7 0 6
writes 10
0 0 1 1 2 2 2 2 3.5 4 5 6 7 8 9

```



## Phix

plus some factoring out of common code

```Phix
sequence array
object item
integer pos, writes

procedure find_item(integer cycle_start)
    -- Find where to put the item.
    pos = cycle_start
    for i=cycle_start+1 to length(array) do
        if array[i] < item then
            pos = pos + 1
        end if
    end for 
end procedure

procedure put_item()
    -- Put the item there or right after any duplicates.
    while item == array[pos] do
        pos = pos + 1
    end while
    {array[pos],item} = {item,array[pos]}
    writes += 1
end procedure

-- Sort an array in place and return the number of writes.
procedure cycleSort()
    writes = 0
    -- Loop through the array to find cycles to rotate.
    for cycle_start=1 to length(array)-1 do
        item = array[cycle_start]
        find_item(cycle_start) 
        -- If the item is already there, this is not a cycle.
        if pos!=cycle_start then
            put_item()
            -- Rotate the rest of the cycle.
            while pos!=cycle_start do
                find_item(cycle_start) 
                put_item()
            end while
        end if 
    end for
end procedure
 
constant samples = {{1, 9, 3, 5, 8, 4, 7, 0, 6, 2},
                    {0, 1, 2, 2, 2, 2, 1, 9, 3.5, 5, 8, 4, 7, 0, 6},
                    {"Greygill Hole", "Ogof Draenen", "Ogof Ffynnon Ddu", "Malham Tarn Pot"},
                    {-3.14 ,3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6, 2, 6, 4, 3, 3, 8, 3, 2, 7, 9, 5, 0, 2, 8, 8, 4},
                    {5,0,1,2,2,3,5,1,1,0,5,6,9,8,0,1},
                    {1,1,1,1,1,1}}

for i=1 to length(samples) do
    array = samples[i] 
    puts(1,"Input list  ") ?array
    cycleSort()
    puts(1,"Sorted list ") ?array
    printf(1,"Total number of writes: %d (out of %d)\n", {writes,length(array)})
end for
```

```txt

Input list  {1,9,3,5,8,4,7,0,6,2}
Sorted list {0,1,2,3,4,5,6,7,8,9}
Total number of writes: 10 (out of 10)
Input list  {0,1,2,2,2,2,1,9,3.5,5,8,4,7,0,6}
Sorted list {0,0,1,1,2,2,2,2,3.5,4,5,6,7,8,9}
Total number of writes: 10 (out of 15)
Input list  {"Greygill Hole","Ogof Draenen","Ogof Ffynnon Ddu","Malham Tarn Pot"}
Sorted list {"Greygill Hole","Malham Tarn Pot","Ogof Draenen","Ogof Ffynnon Ddu"}
Total number of writes: 3 (out of 4)
Input list  {-3.14,3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3,2,3,8,4,6,2,6,4,3,3,8,3,2,7,9,5,0,2,8,8,4}
Sorted list {-3.14,0,1,1,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,7,7,8,8,8,8,8,9,9,9,9}
Total number of writes: 34 (out of 38)
Input list  {5,0,1,2,2,3,5,1,1,0,5,6,9,8,0,1}
Sorted list {0,0,0,1,1,1,1,2,2,3,5,5,5,6,8,9}
Total number of writes: 14 (out of 16)
Input list  {1,1,1,1,1,1}
Sorted list {1,1,1,1,1,1}
Total number of writes: 0 (out of 6)

```



## Python

The Wikipedia algorithm pseudocode is very nearly Python. The main changes needed were to change the name array to vector to stop it obscuring a built-in name, and iterating over an enumerated collection rather than using explicit indices.


```python
def cycleSort(vector):
    "Sort a vector in place and return the number of writes."
    writes = 0
 
    # Loop through the vector to find cycles to rotate.
    for cycleStart, item in enumerate(vector):
 
        # Find where to put the item.
        pos = cycleStart
        for item2 in vector[cycleStart + 1:]:
            if item2 < item:
                pos += 1
 
        # If the item is already there, this is not a cycle.
        if pos == cycleStart:
            continue
 
        # Otherwise, put the item there or right after any duplicates.
        while item == vector[pos]:
            pos += 1
        vector[pos], item = item, vector[pos]
        writes += 1
 
        # Rotate the rest of the cycle.
        while pos != cycleStart:
 
            # Find where to put the item.
            pos = cycleStart
            for item2 in vector[cycleStart + 1:]:
                if item2 < item:
                    pos += 1
 
            # Put the item there or right after any duplicates.
            while item == vector[pos]:
                pos += 1
            vector[pos], item = item, vector[pos]
            writes += 1
 
    return writes


if __name__ == '__main__':
    x = [0, 1, 2, 2, 2, 2, 1, 9, 3.5, 5, 8, 4, 7, 0, 6]
    xcopy = x[::]
    writes = cycleSort(xcopy)
    if xcopy != sorted(x):
        print('Wrong order!')
    else:
        print('%r\nIs correctly sorted using cycleSort to'
              '\n%r\nUsing %i writes.' % (x, xcopy, writes))
```


```txt
[0, 1, 2, 2, 2, 2, 1, 9, 3.5, 5, 8, 4, 7, 0, 6]
Is correctly sorted using cycleSort to
[0, 0, 1, 1, 2, 2, 2, 2, 3.5, 4, 5, 6, 7, 8, 9]
Using 10 writes.
```



## Racket


```racket
#lang racket/base
(require racket/match)

;; Sort an array in place and return the number of writes.
(define (cycle-sort! v < =?)
  (define v-len (vector-length v))
  (for/sum ; Loop through the array to find cycles to rotate.
    ((cycle-start (in-range 0 (sub1 v-len))))
    (define item (vector-ref v cycle-start))
    (define (find-insertion-point) ; Find where to put the item.    
      (+ cycle-start
         (for/sum 
           ((i (in-range (add1 cycle-start) v-len))
            #:when (< (vector-ref v i) item)) 1)))
    ;; Put the item there or right after any duplicates
    (define (insert-after-duplicates pos)
      (match (vector-ref v pos)
        [(== item =?) (insert-after-duplicates (add1 pos))]
        [tmp (vector-set! v pos item) ; / swap
             (set! item tmp)          ; \ [this is my only write point]
             pos]))
    
    (define i-p (find-insertion-point))
    (if (= i-p cycle-start)
        0 ; If the item is already there, this is not a cycle.
        (let loop ; Rotate the rest of the cycle.        
          ((e-p (insert-after-duplicates i-p))
           (W 1 #| we've already written once |#))
          (if (= e-p cycle-start)
              W
              (loop (insert-after-duplicates (find-insertion-point))
                    (add1 W))))))) ; we've written again!

(module+ main
  ;; This will be random with duplicates
  (define A (list->vector (build-list 30 (λ (i) (random 20)))))
  A
  (cycle-sort! A < =)
  A
  (define B #(1 1 1 1 1 1))
  B
  (cycle-sort! B < =))
```


```txt
'#(7 17 5 16 14 9 18 10 1 4 10 1 9 3 3 0 1 18 16 12 9 14 14 12 19 2 12 15 16 8)
28
'#(0 1 1 1 2 3 3 4 5 7 8 9 9 9 10 10 12 12 12 14 14 14 15 16 16 16 17 18 18 19)
'#(1 1 1 1 1 1)
0
```



## REXX


### version 1


```rexx
/* REXX ***************************************************************
* 12.06.2014 Walter Pachl translated from Wikipedia's code
* 20.06.2014 WP corrected (courtesy Alan Sampson)
* 30.05.2017 WP fixed for Classic Rexx (courtesy GS)
**********************************************************************/
list='1 9 3 5 8 4 7 0 6 2'
n=words(list)
Do i=0 To n-1
  array.i=word(list,i+1)
  End
Say list
writes=cyclesort()
Say 'writes='writes
ol=''
Do i=0 To n-1
  ol=ol array.i
  End
Say strip(ol)
Exit

cycleSort: procedure expose array. n
  writes = 0
  /* Loop through the array to find cycles to rotate. */
  do cycleStart=0 to n-2
    item = array.cycleStart

    /* Find where to put the item. */
    pos = cycleStart
    Do i=cycleStart+1 to n-1
      if array.i < item Then
        pos=pos+1
      End

    /* If the item is already there, this is not a cycle. */
    if pos == cycleStart Then
      Iterate

    /* Otherwise, put the item there or right after any duplicates. */
    Do while item == array.pos
      pos=pos+1
      End
    Parse Value array.pos item With item array.pos
    writes=writes+1

    /* Rotate the rest of the cycle. */
    Do while pos <> cycleStart

      /* Find where to put the item. */
      pos = cycleStart
      Do i=cycleStart + 1 to n-1
        if array.i < item Then
          pos=pos+1
        End

      /* Put the item there or right after any duplicates. */
      Do while item == array.pos
        pos=pos+1
        End
      Parse Value array.pos item With item array.pos
      writes=writes+1
      End
    End
  return writes
```

```txt
1 9 3 5 8 4 7 0 6 2
writes=10
0 1 2 3 4 5 6 7 8 9
```



### version 2

This REXX version demonstrates the use of negative numbers and non-integer values in the list.

As a default, the program uses (for the input list) some digits of pi, which for practical purposes, appear random.

```rexx
/*REXX program demonstrates a  cycle sort  on a  list of items.                         */
parse arg z                                      /* [↓] not specified?  Use "pi" digits.*/
if z='' then z=-3.14 3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4 3 3 8 3 2 7 9 5 0 2 8 8 4
say 'unsorted list: '  z                         /*show the original unsorted numbers.  */
w=sortCycle(z)                                   /*W:  the number of writes done in sort*/
say 'and took'  w  "writes."                     /*show number of writes done in sort.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sortCycle: procedure expose @.;  parse arg y;          #=words(y);   writes=0
        do i=1  for #; @.i=word(y,i); end  /*i*/ /*put each of the items ───► @.  array.*/
                                                 /* [↓]  find a  "cycle"  to rotate.    */
  do c=1  for #;    x=@.c;         p=c           /*X  is the  item  being sorted.       */
        do j=c+1  to #; if @.j<x  then p=p+1; end        /*determine where to put  X.   */
  if p==c then  iterate                          /*Is it there?  No, this ain't a cycle.*/
        do  while x==@.p;  p=p+1;  end           /*put  X  right after any duplicate.   */
  parse value  @.p  x    with  x  @.p            /*swap the two values:   @.p   and   X.*/
  writes=writes+1                                /*bump counter for the number of writes*/
      do while p\==c;     p=c                    /*rotate the rest of the "cycle".      */
        do k=c+1  to #;   if @.k<x  then p=p+1;  end  /*k*/
        do while x==@.p;  p=p+1;  end            /*put  X  here  or  right after dups.  */
      parse value  @.p  x    with    x  @.p      /*swap the two values:   @.p   and   X.*/
      writes=writes+1                            /*bump the counter for number of writes*/
      end   /*while p\==c*/
  end       /*c*/
                                                 /* [↓]  display the sorted list.       */
_=@.1;  do j=2  to #; _=_ @.j; end;       say '  sorted list: '  _
return writes
```

'''output'''   when using the default input:

```txt

unsorted list:  -3.14 3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4 3 3 8 3 2 7 9 5 0 2 8 8 4
  sorted list:  -3.14 0 1 1 2 2 2 2 2 3 3 3 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 7 7 8 8 8 8 8 9 9 9 9
and took 34 writes.

```

'''output'''   when using the input of:   <tt> FM Stereo has been around since 1961. </tt>

```txt

unsorted list:  FM Stereo has been around since 1961.
  sorted list:  1961. FM Stereo around been has since
and took 7 writes.

```

Note (for the above output).   This REXX program was executed on an ASCII machine.

On an   ASCII   machine, the order of sorting is numbers, uppercase letters, lowercase letters.

On an EBCDIC machine, the order of sorting is lowercase letters, uppercase letters, numbers.

Other (special) characters may also be in a different order.


### version 3

This version uses a faster (but a more cryptic) version of incrementing   '''1'''   (one) to   '''P'''   within two '''do''' loops.

```rexx
/*REXX program demonstrates a  cycle sort  on a  list of items.                         */
parse arg z                                      /* [↓] not specified?  Use "pi" digits.*/
if z='' then z=-3.14 3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4 3 3 8 3 2 7 9 5 0 2 8 8 4
say 'unsorted list: '  z                         /*show the original unsorted numbers.  */
w=sortCycle(z)                                   /*W:  the number of writes done in sort*/
say 'and took'  w  "writes."                     /*show number of writes done in sort.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sortCycle: procedure expose @.;  parse arg y;          #=words(y);   writes=0
        do i=1  for #; @.i=word(y,i); end  /*i*/ /*put each of the items ───► @.  array.*/
                                                 /* [↓]  find a  "cycle"  to rotate.    */
  do c=1  for #;    x=@.c;         p=c           /*X  is the  item  being sorted.       */
        do j=c+1  to #; if @.j<x  then p=p+1; end        /*determine where to put  X.   */
  if p==c then  iterate                          /*Is it there?  No, this ain't a cycle.*/
        do  while x==@.p;  p=p+1;  end           /*put  X  right after any duplicate.   */
  parse value  @.p  x    with  x  @.p            /*swap the two values:   @.p   and   X.*/
  writes=writes+1                                /*bump counter for the number of writes*/
      do while p\==c;     p=c                    /*rotate the rest of the "cycle".      */
        do k=c+1  to #;   if @.k<x  then p=p+1;  end  /*k*/
        do p=p  while x==@.p;  end               /*put   X   here  or  right after dups.*/
      parse  value  @.p  x   with   x  @.p       /*swap the two values:   @.p   and   X.*/
      writes=writes+1                            /*bump the counter for number of writes*/
      end   /*while p\==c*/
  end       /*c*/
                                                 /* [↓]  display the sorted list.       */
_=@.1;  do j=2  to #; _=_ @.j; end;           say '  sorted list: '  _
return writes
```

'''output''' is identical to the 2<sup>nd</sup> version.


### version 4

This version uses a subroutine to perform the task of handling an (sorted) item placement (possibly after duplicates).

```rexx
/*REXX program demonstrates a  cycle sort  on a  list of items.                         */
parse arg z                                      /* [↓] not specified?  Use "pi" digits.*/
if z='' then z=-3.14 3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4 3 3 8 3 2 7 9 5 0 2 8 8 4
say 'unsorted list: '  z                         /*show the original unsorted numbers.  */
w=sortCycle(z)                                   /*W:  the number of writes done in sort*/
say 'and took'  w  "writes."                     /*show number of writes done in sort.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sortCycle: procedure expose @.;    parse arg y;   #=words(y);      w=0
        do i=1  for #; @.i=word(y,i); end  /*i*/ /*put each of the items ───► @.  array.*/

  do c=1  for #;     x=@.c;      p=c             /*X  is the  item  being sorted.       */
      do j=c+1  to #; if @.j<x  then p=p+1; end  /*j*/                /*where to put  X.*/
  if p==c  then  iterate                         /*Is it there?  Then this ain't a cycle*/
  call .swap                                     /*put   X   here  or  right after dups.*/
      do while p\==c;     p=c                    /*rotate the rest of the  "cycle".     */
        do k=c+1  to #;   if @.k<x  then p=p+1;  end  /*k*/
      call .swap                                 /*put   X   here  or  right after dups.*/
      end   /*while p\==c*/
  end       /*c*/
                                                 /* [↓]  display the sorted list to term*/
_=@.1;  do j=2  to #; _=_ @.j; end;       say '  sorted list: '  _
return w                                         /* [↓] find where to put   X   into  @ */
.swap:  do p=p  while x==@.p;  end;       parse value  @.p x  with  x @.p;  w=w+1;  return
```

'''output'''   is identical to the 2<sup>nd</sup> version.


## Ring


```ring

# Project : Sorting algorithms/Cycle sort

array = [0, 1, 2, 2, 2, 2, 1, 9, 3, 5, 5, 8, 4, 7, 0, 6]
seearray(array)
writes = cyclesort(array)
see "after sorting with " + writes + " writes :" + nl
seearray(array)
see nl
array2 = [38, 119, 38, 33, 33, 28, 24, 101, 108, 120, 99, 59, 69, 24, 117, 22, 90, 94, 78, 75]
seearray(array2)
writes = cyclesort(array2)
see "after sorting with " + writes  + " writes :" + nl
seearray(array2)

func cyclesort(array) 
        length = len(array) 
        if length = 0
           return 0
        ok
        writes = 0 
        for cyclestart = 1 to len(array) - 1
             item = array[cyclestart] 
             position = cyclestart
             for i = cyclestart + 1 to len(array)
                  if array[i] < item
                     position = position +  1
                  ok
             next 
             if position = cyclestart
                loop
             ok 
             while item = array[position]
                     position = position+ 1
             end
             temp = item
             item = array[position]
             array[position] = temp
             writes = writes + 1 
             while position != cyclestart
                     position = cyclestart
                     for i = cyclestart + 1 to len(array)
                          if array[i] < item 
                             position = position + 1
                          ok
                     next 
                     while item = array[position]
                             position = position + 1
                     end
                     temp = item
                     item = array[position]
                     array[position] = temp
                     writes = writes + 1
             end
        next  
        return writes
 
func seearray(array)
        for i = 1 to len(array)
            see string(array[i]) + " " 
        next
        see nl

```

Output:

```txt

0 1 2 2 2 2 1 9 3 5 5 8 4 7 0 6 
after sorting with 10 writes :
0 0 1 1 2 2 2 2 3 4 5 5 6 7 8 9 

38 119 38 33 33 28 24 101 108 120 99 59 69 24 117 22 90 94 78 75 
after sorting with 19 writes :
22 24 24 28 33 33 38 38 59 69 75 78 90 94 99 101 108 117 119 120 

```



## Ruby

Direct translation of the pseudocode on the Wikipedia.

```ruby
def cycleSort!(array)
  writes = 0
  
  # Loop through the array to find cycles to rotate.
  for cycleStart in 0 .. array.size-2
    item = array[cycleStart]
    
    # Find where to put the item.
    pos = cycleStart
    for i in cycleStart+1 ... array.size
      pos += 1  if array[i] < item
    end
    
    # If the item is already there, this is not a cycle.
    next  if pos == cycleStart
    
    # Otherwise, put the item there or right after any duplicates.
    pos += 1  while item == array[pos]
    array[pos], item = item, array[pos]
    writes += 1
    
    # Rotate the rest of the cycle.
    while pos != cycleStart
      
      # Find where to put the item.
      pos = cycleStart
      for i in cycleStart+1 ... array.size
        pos += 1  if array[i] < item
      end
      
      # Put the item there or right after any duplicates.
      pos += 1  while item == array[pos]
      array[pos], item = item, array[pos]
      writes += 1
    end
  end
  writes
end 

p a = [0, 1, 2, 2, 2, 2, 1, 9, 3.5, 5, 8, 4, 7, 0, 6]
puts "writes : #{cycleSort!(a)}"
p a
```


```txt

[0, 1, 2, 2, 2, 2, 1, 9, 3.5, 5, 8, 4, 7, 0, 6]
writes : 10
[0, 0, 1, 1, 2, 2, 2, 2, 3.5, 4, 5, 6, 7, 8, 9]

```



## Scala

Translation of Java version

```scala

  def cycleSort(a: Array[Int]): (Array[Int], Int) = {
    var writes = 0

    for (cycleStart <- 0 until a.length - 1) {
      var value = a(cycleStart)

      // count the number of values that are smaller than value since cycleStart
      var pos = cycleStart
      
      for (i <- cycleStart + 1 until a.length)
        if (a(i) < value) pos += 1

      // skip if there aren't any
      if (pos != cycleStart) {
        
        // skip duplicates
        while (a(pos) == value) pos += 1

        // put val into final position
        val tmp = a(pos)
        a(pos) = value
        value = tmp
        writes += 1

        // repeat as long as we can find values to swap
        // otherwise start new cycle
        while (pos != cycleStart) {
          pos = cycleStart
          for (i <- cycleStart + 1 until a.length)
            if (a(i) < value) pos += 1

          while (a(pos) == value) pos += 1

          val tmp = a(pos)
          a(pos) = value
          value = tmp
          writes += 1
        }
      }
    }
    (a, writes)
  }

```



## Sidef


```ruby
func cycle_sort (array) {
    var (writes=0, pos=0)

    func f(i, Ref item, bool=false) {
        pos = (i + array.ft(i+1).count{ _ < *item })
        return(false) if (bool && pos==i)
        while (*item == array[pos]) { ++pos }
        (array[pos], *item) = (*item, array[pos])
        ++writes
        return true
    }

    array.each_kv { |i, item|
        f(i, \item, true) || next
        while (pos != i) {
            f(i, \item)
        }
    }

    return writes
}

var a = %n(0 1 2 2 2 2 1 9 3.5 5 8 4 7 0 6)

say a.join(' ')
say ('writes ', cycle_sort(a))
say a.join(' ')
```

```txt

0 1 2 2 2 2 1 9 3.5 5 8 4 7 0 6
writes 10
0 0 1 1 2 2 2 2 3.5 4 5 6 7 8 9

```



## Tcl

Direct translation of the pseudocode on the Wikipedia page

```tcl
proc cycleSort {listVar} {
    upvar 1 $listVar array
    set writes 0

    # Loop through the array to find cycles to rotate.
    for {set cycleStart 0} {$cycleStart < [llength $array]} {incr cycleStart} {
	set item [lindex $array $cycleStart]
 
	# Find where to put the item.
	set pos $cycleStart
	for {set i [expr {$pos + 1}]} {$i < [llength $array]} {incr i} {
	    incr pos [expr {[lindex $array $i] < $item}]
	}

	# If the item is already there, this is not a cycle.
	if {$pos == $cycleStart} continue
 
	# Otherwise, put the item there or right after any duplicates.
	while {$item == [lindex $array $pos]} {
	    incr pos
	}
	set tmp [lindex $array $pos]
	lset array $pos $item
	set item $tmp
	incr writes
 
	# Rotate the rest of the cycle.
	while {$pos != $cycleStart} {
	    # Find where to put the item.
	    set pos $cycleStart

	    for {set i [expr {$cycleStart + 1}]} {$i < [llength $array]} {incr i} {
		incr pos [expr {[lindex $array $i] < $item}]
	    }

	    # Put the item there or right after any duplicates.
	    while {$item == [lindex $array $pos]} {
		incr pos
	    }
	    set tmp [lindex $array $pos]
	    lset array $pos $item
	    set item $tmp
	    incr writes
	}
    }
 
    return $writes
}
```

'''Demonstrating:'''

```tcl
set example {0 1 2 2 2 2 1 9 3.5 5 8 4 7 0 6}
puts "Data was: $example"
set writes [cycleSort example]
puts "Data is now: $example"
if {$example eq [lsort -real $example]} {
    puts "\twhich is correctly sorted"
} else {
    puts "\twhich is the wrong order!"
}
puts "Writes required: $writes"
```

```txt

Data was: 0 1 2 2 2 2 1 9 3.5 5 8 4 7 0 6
Data is now: 0 0 1 1 2 2 2 2 3.5 4 5 6 7 8 9
	which is correctly sorted
Writes required: 10

```

