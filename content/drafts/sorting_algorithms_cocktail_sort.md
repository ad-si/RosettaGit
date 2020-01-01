+++
title = "Sorting algorithms/Cocktail sort"
description = ""
date = 2019-10-18T20:09:18Z
aliases = []
[extra]
id = 3146
[taxonomies]
categories = []
tags = []
+++

{{task|Sorting Algorithms}}
{{Sorting Algorithm}}
{{Wikipedia|Cocktail sort}}


The cocktail shaker sort is an improvement on the [[Bubble Sort]].

The improvement is basically that values "bubble" both directions through the array, because on each iteration the cocktail shaker sort bubble sorts once forwards and once backwards. Pseudocode for the algorithm (from [[wp:Cocktail sort|wikipedia]]):
 '''function''' ''cocktailSort''( A : list of sortable items )
  '''do'''
    swapped := false
    '''for each''' i '''in''' 0 '''to''' length( A ) - 2 '''do'''
      '''if''' A[ i ] > A[ i+1 ] '''then''' ''// test whether the two''
                                ''// elements are in the wrong''
                                ''// order''
        swap( A[ i ], A[ i+1 ] ) ''// let the two elements''
                                 ''// change places''
        swapped := true;
    '''if''' swapped = false '''then'''
      ''// we can exit the outer loop here if no swaps occurred.''
      '''break do-while loop''';
    swapped := false
    '''for each''' i '''in''' length( A ) - 2 '''down to''' 0 '''do'''
      '''if''' A[ i ] > A[ i+1 ] '''then'''
        swap( A[ i ], A[ i+1 ] )
        swapped := true;
  '''while''' swapped; ''// if no elements have been swapped,''
                 ''// then the list is sorted''


## 360 Assembly

{{trans|PL/I}}
The program uses ASM structured macros and two ASSIST macros to keep the code as short as possible.

```360asm
*        Cocktail sort             25/06/2016
COCKTSRT CSECT
         USING  COCKTSRT,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         "
         LR     R13,R15            "
         L      R2,N               n
         BCTR   R2,0               n-1
		 ST     R2,NM1             nm1=n-1
    DO UNTIL=(CLI,STABLE,EQ,X'01') repeat
         MVI    STABLE,X'01'         stable=true
         LA     RI,1                 i=1
         DO WHILE=(C,RI,LE,NM1)      do i=1 to n-1
         LR     R1,RI                  i
         SLA    R1,2                   .
         LA     R2,A-4(R1)             @a(i)
         LA     R3,A(R1)               @a(i+1)
         L      R4,0(R2)               r4=a(i)
         L      R5,0(R3)               r5=a(i+1)
         IF     CR,R4,GT,R5 THEN       if a(i)>a(i+1) then
         MVI    STABLE,X'00'             stable=false
         ST     R5,0(R2)                 a(i)=r5
         ST     R4,0(R3)                 a(i+1)=r4
		 ENDIF  ,                      end if
         LA     RI,1(RI)               i=i+1
         ENDDO  ,                    end do
         L      RI,NM1               i=n-1
         DO WHILE=(C,RI,GE,=F'1')    do i=n-1 to 1 by -1
         LR     R1,RI                  i
         SLA    R1,2                   .
         LA     R2,A-4(R1)             @a(i)
         LA     R3,A(R1)               @a(i+1)
         L      R4,0(R2)               r4=a(i)
         L      R5,0(R3)               r5=a(i+1)
         IF     CR,R4,GT,R5 THEN       if a(i)>a(i+1) then
         MVI    STABLE,X'00'             stable=false
         ST     R5,0(R2)                 a(i)=r5
         ST     R4,0(R3)                 a(i+1)=r4
		 ENDIF  ,                      end if
         BCTR   RI,0                   i=i-1
         ENDDO  ,                    end do
    ENDDO       ,                  until stable
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
STABLE   DS     X                  stable
         YREGS
RI       EQU    6                  i
         END    COCKTSRT
```

{{out}}

```txt

 -31   0   1   2   2   4  45  58  65  69  74  82  82  83  88  89  99 104 112 782

```



## ActionScript


```ActionScript
function cocktailSort(input:Array):Array {
   do {
        var swapped:Boolean=false;
	for (var i:uint = 0; i < input.length-1; i++) {
	    if (input[i]>input[i+1]) {
	    var tmp=input[i];
	    input[i]=input[i+1];
	    input[i+1]=tmp;
	    swapped=true;
	    }
	}
	if (! swapped) {
            break;
	}
	for (i = input.length -2; i >= 0; i--) {
	    if (input[i]>input[i+1]) {
	    tmp=input[i];
	    input[i]=input[i+1];
	    input[i+1]=tmp;
	    swapped=true;
	    }
        }
    } while (swapped);
   return input;
}
```



## Ada


```Ada
with Ada.Text_Io; use Ada.Text_Io;

procedure Cocktail_Sort_Test is
   procedure Cocktail_Sort (Item : in out String) is
      procedure Swap(Left, Right : in out Character) is
         Temp : Character := Left;
      begin
         Left := Right;
         Right := Temp;
      end Swap;
      Swapped : Boolean := False;
   begin
      loop
         for I in 1..Item'Last - 1 loop
            if Item(I) > Item(I + 1) then
               Swap(Item(I), Item(I + 1));
               Swapped := True;
            end if;
         end loop;
         if not Swapped then
            for I in reverse 1..Item'Last - 1 loop
               if Item(I) > Item(I + 1) then
                  Swap(Item(I), Item(I + 1));
                  Swapped := True;
               end if;
            end loop;
         end if;
         exit when not Swapped;
         Swapped := False;
      end loop;
   end Cocktail_Sort;
   Data : String := "big fjords vex quick waltz nymph";
begin
   Put_Line(Data);
   Cocktail_Sort(Data);
   Put_Line(Data);
end Cocktail_Sort_Test;
```



## ALGOL 68


```algol68
MODE DATA = CHAR;
PROC swap = (REF DATA a,b)VOID:(
  DATA tmp:=a; a:=b; b:=tmp
);

PROC cocktail sort = (REF[]DATA a)VOID: (
  WHILE
    BOOL swapped := FALSE;
    FOR i FROM LWB a TO UPB a - 1 DO
      IF a[ i ] > a[ i + 1 ] THEN # test whether the two elements are in the wrong order #
        swap( a[ i ], a[ i + 1 ] ); # let the two elements change places #
        swapped := TRUE
      FI
    OD;
    IF NOT swapped THEN
      # we can exit the outer loop here if no swaps occurred. #
      break do while loop
    FI;
    swapped := FALSE;
    FOR i FROM UPB a - 1 TO LWB a DO
      IF a[ i ] > a[ i + 1 ] THEN
        swap( a[ i ], a[ i + 1 ] );
        swapped := TRUE
      FI
    OD;
# WHILE # swapped # if no elements have been swapped, then the list is sorted #
  DO SKIP OD;
  break do while loop: SKIP
);

[32]CHAR data := "big fjords vex quick waltz nymph";
cocktail sort(data);
print(data)
```

{{out}}

```txt
     abcdefghiijklmnopqrstuvwxyz
```

Alternatively - when the data records are large - the data can be manipulated
indirectly, thus removing the need to actually swap large chunks of memory
as only addresses are swapped.

```algol68
MODE DATA = REF CHAR;
PROC swap = (REF DATA a,b)VOID:(
  DATA tmp:=a; a:=b; b:=tmp
);

PROC (REF[]DATA a)VOID cocktail sort;

[32]CHAR data := "big fjords vex quick waltz nymph";
[UPB data]DATA ref data;  FOR i TO UPB data DO ref data[i] := data[i] OD;
cocktail sort(ref data);
FOR i TO UPB ref data DO print(ref data[i]) OD; print(new line);
print((data))
```

{{out}}
```txt
     abcdefghiijklmnopqrstuvwxyz
big fjords vex quick waltz nymph
```

The above two routines both scan the entire width of the array, in both directions, even though the first and last elements of each sweep had already reached their final destination during the previous pass.  The solution is to zig-zag, but have the sweeps shorten and converge on the centre element.  This reduces the number of comparisons required by about 50%.

```algol68
PROC odd even sort = (REF []DATA a)VOID: (
  FOR offset FROM 0 DO
    BOOL swapped := FALSE;
    FOR i FROM LWB a + offset TO UPB a - 1 - offset DO
      IF a[ i ] > a[ i + 1 ] THEN # test whether the two elements are in the wrong order #
        swap( a[ i ], a[ i + 1 ] ); # let the two elements change places #
        swapped := TRUE
      FI
    OD;
  # we can exit the outer loop here if no swaps occurred. #
    IF NOT swapped THEN break do od loop FI;
    swapped := FALSE;
    FOR i FROM UPB a - 1 - offset - 1 BY - 1 TO LWB a + offset DO
      IF a[ i ] > a[ i + 1 ] THEN
        swap( a[ i ], a[ i + 1 ] );
        swapped := TRUE
      FI
    OD;
  # if no elements have been swapped, then the list is sorted #
    IF NOT swapped THEN break do od loop FI;
  OD;
  break do od loop: SKIP
);
```



## ALGOL W

As noted in the ALGOL 68 sample above, the highest and lowest elements are sorted into their correct positions each time through the main loop.
This implementation optimises by reducing the number of elements to sort on each pass through the main loop.

```algolw
begin
    % As algol W does not allow overloading, we have to have type-specific   %
    % sorting procedures - this coctail sorts an integer array               %
    % as there is no way for the procedure to determine the array bounds, we %
    % pass the lower and upper bounds in lb and ub                           %
    procedure coctailSortIntegers( integer array item( * )
                                 ; integer value lb
                                 ; integer value ub
                                 ) ;
    begin
        integer lower, upper;

        lower := lb;
        upper := ub - 1;

        while
            begin
                logical swapped;

                procedure swap( integer value i ) ;
                begin
                    integer val;
                    val           := item( i );
                    item( i )     := item( i + 1 );
                    item( i + 1 ) := val;
                    swapped       := true;
                end swap ;

                swapped := false;
                for i := lower until upper do if item( i ) > item( i + 1 ) then swap( i );
                if swapped
                then begin
                    % there was at least one unordered element so try a 2nd sort pass %
                    for i := upper step -1 until lower do if item( i ) > item( i + 1 ) then swap( i );
                    upper := upper - 1; lower := lower + 1;
                end if_swapped ;
                swapped
            end
        do  begin end;
    end coctailSortIntegers ;

    begin % test the sort                                                    %
        integer array data( 1 :: 10 );

        procedure writeData ;
        begin
            write( data( 1 ) );
            for i := 2 until 10 do writeon( data( i ) );
        end writeData ;

        % initialise data to unsorted values                                 %
        integer dPos;
        dPos  := 1;
        for i := 16, 2, -6, 9, 90, 14, 0, 23, 8, 9
        do begin
            data( dPos ) := i;
            dPos         := dPos + 1;
        end for_i ;

        i_w := 3; s_w := 1; % set output format %
        writeData;
        coctailSortIntegers( data, 1, 10 );
        writeData;
    end test ;
end.
```

{{out}}

```txt

 16   2  -6   9  90  14   0  23   8   9
 -6   0   2   8   9   9  14  16  23  90

```



## AutoHotkey

contributed by Laszlo on the ahk [http://www.autohotkey.com/forum/post-276379.html#276379 forum]

```AutoHotkey
MsgBox % CocktailSort("")
MsgBox % CocktailSort("xxx")
MsgBox % CocktailSort("3,2,1")
MsgBox % CocktailSort("dog,000000,xx,cat,pile,abcde,1,cat,zz,xx,z")

CocktailSort(var) {                      ; SORT COMMA SEPARATED LIST
   StringSplit array, var, `,            ; make array
   i0 := 1, i1 := array0                 ; start, end

   Loop {                                ; break when sorted
     Changed =
     Loop % i1-- -i0 {                   ; last entry will be in place
       j := i0+A_Index, i := j-1
       If (array%j% < array%i%)          ; swap?
         t := array%i%, array%i% := array%j%, array%j% := t
        ,Changed = 1                     ; change has happened
     }
     IfEqual Changed,, Break

     Loop % i1-i0++ {                    ; first entry will be in place
       i := i1-A_Index, j := i+1
       If (array%j% < array%i%)          ; swap?
         t := array%i%, array%i% := array%j%, array%j% := t
        ,Changed = 1                     ; change has happened
     }
     IfEqual Changed,, Break
   }

   Loop % array0                         ; construct string from sorted array
     sorted .= "," . array%A_Index%
   Return SubStr(sorted,2)               ; drop leading comma
}
```



## AWK

Sort the standard input and print it to standard output

```awk
{
  line[NR] = $0
}
END { # sort it with cocktail sort
  swapped = 0
  do {
    for(i=1; i < NR; i++) {
      if ( line[i] > line[i+1] ) {
	t = line[i]
	line[i] = line[i+1]
	line[i+1] = t
	swapped = 1
      }
    }
    if ( swapped == 0 ) break
    swapped = 0
    for(i=NR-1; i >= 1; i--) {
      if ( line[i] > line[i+1] ) {
	t = line[i]
	line[i] = line[i+1]
	line[i+1] = t
	swapped = 1
      }
    }
  } while ( swapped == 1 )
  #print it
  for(i=1; i <= NR; i++) {
    print line[i]
  }
}
```



## BBC BASIC

Sorting an integer array. '%' indicates integer variable in BBC BASIC

```BBC BASIC
DEF PROC_ShakerSort(Size%)

Start%=2
End%=Size%
Direction%=1
LastChange%=1
REPEAT
  FOR J% = Start% TO End% STEP Direction%
    IF data%(J%-1) > data%(J%) THEN
       SWAP data%(J%-1),data%(J%)
       LastChange% = J%
    ENDIF
  NEXT J%
  End% = Start%
  Start% = LastChange% - Direction%
  Direction% = Direction% * -1
UNTIL ( ( End% * Direction% ) < ( Start% * Direction% ) )

ENDPROC
```



## C


```c
#include <stdio.h>

// can be any swap function. This swap is optimized for numbers.
void swap(int *x, int *y) {
	if(x == y)
		return;
	*x ^= *y;
	*y ^= *x;
	*x ^= *y;
}
void cocktailsort(int *a, size_t n) {
	while(1) {
		// packing two similar loops into one
		char flag;
		size_t start[2] = {1, n - 1},
			   end[2] = {n, 0},
			   inc[2] = {1, -1};
		for(int it = 0; it < 2; ++it) {
			flag = 1;
			for(int i = start[it]; i != end[it]; i += inc[it])
				if(a[i - 1] > a[i]) {
					swap(a + i - 1, a + i);
					flag = 0;
				}
			if(flag)
				return;
		}
	}
}

int main(void) {
	int a[] = { 5, -1, 101, -4, 0, 1, 8, 6, 2, 3 };
	size_t n = sizeof(a)/sizeof(a[0]);

	cocktailsort(a, n);
	for (size_t i = 0; i < n; ++i)
		printf("%d ", a[i]);
	return 0;
}
```


'''Output''':

<lang>-4 -1 0 1 2 3 5 6 8 101
```



## C++


```cpp

#include <iostream>
#include <windows.h>

const int EL_COUNT = 77, LLEN = 11;

class cocktailSort
{
public:
    void sort( int* arr, int len )
    {
	bool notSorted = true;
	while( notSorted )
	{
	    notSorted = false;
	    for( int a = 0; a < len - 1; a++ )
	    {
		if( arr[a] > arr[a + 1] )
		{
		    sSwap( arr[a], arr[a + 1] );
		    notSorted = true;
		}
	    }

	    if( !notSorted ) break;
	    notSorted = false;

	    for( int a = len - 1; a > 0; a-- )
	    {
		if( arr[a - 1] > arr[a] )
		{
		    sSwap( arr[a], arr[a - 1] );
		    notSorted = true;
		}
	    }
	}
    }

private:
    void sSwap( int& a, int& b )
    {
	int t = a;
   	a = b; b = t;
    }
};

int main( int argc, char* argv[] )
{
    srand( GetTickCount() );
    cocktailSort cs;
    int arr[EL_COUNT];

    for( int x = 0; x < EL_COUNT; x++ )
        arr[x] = rand() % EL_COUNT + 1;

    std::cout << "Original: " << std::endl << "
### ====
" << std::endl;
    for( int x = 0; x < EL_COUNT; x += LLEN )
    {
	for( int s = x; s < x + LLEN; s++ )
	    std::cout << arr[s] << ", ";

	std::cout << std::endl;
    }

    //DWORD now = GetTickCount();
    cs.sort( arr, EL_COUNT );
    //now = GetTickCount() - now;

    std::cout << std::endl << std::endl << "Sorted: " << std::endl << "
### ==
" << std::endl;
    for( int x = 0; x < EL_COUNT; x += LLEN )
    {
	for( int s = x; s < x + LLEN; s++ )
	    std::cout << arr[s] << ", ";

	std::cout << std::endl;
    }

    std::cout << std::endl << std::endl << std::endl << std::endl;
    //std::cout << now << std::endl << std::endl;
    return 0;
}

```



###  Alternate version

Uses C++11. Compile with
 g++ -std=c++11 cocktail.cpp

```cpp
#include <algorithm>
#include <iostream>
#include <iterator>

template <typename RandomAccessIterator>
void cocktail_sort(RandomAccessIterator begin, RandomAccessIterator end) {
  bool swapped = true;
  while (begin != end-- && swapped) {
    swapped = false;
    for (auto i = begin; i != end; ++i) {
      if (*(i + 1) < *i) {
        std::iter_swap(i, i + 1);
        swapped = true;
      }
    }
    if (!swapped) {
      break;
    }
    swapped = false;
    for (auto i = end - 1; i != begin; --i) {
      if (*i < *(i - 1)) {
        std::iter_swap(i, i - 1);
        swapped = true;
      }
    }
    ++begin;
  }
}

int main() {
  int a[] = {100, 2, 56, 200, -52, 3, 99, 33, 177, -199};
  cocktail_sort(std::begin(a), std::end(a));
  copy(std::begin(a), std::end(a), std::ostream_iterator<int>(std::cout, " "));
  std::cout << "\n";
}
```

{{out}}

```txt

-199 -52 2 3 33 56 99 100 177 200

```


## C#

```c#
public static void cocktailSort(int[] A)
    {
        bool swapped;
        do
        {
            swapped = false;
            for (int i = 0; i <= A.Length - 2; i++)
            {
                if (A[i] > A[i + 1])
                {
                    //test whether the two elements are in the wrong order
                    int temp = A[i];
                    A[i] = A[i + 1];
                    A[i + 1] = temp;
                    swapped = true;
                }
            }
            if (!swapped)
            {
                //we can exit the outer loop here if no swaps occurred.
                break;
            }
            swapped = false;
            for (int i = A.Length - 2; i >= 0; i--)
            {
                if (A[i] > A[i + 1])
                {
                    int temp = A[i];
                    A[i] = A[i + 1];
                    A[i + 1] = temp;
                    swapped = true;
                }
            }
            //if no elements have been swapped, then the list is sorted
        } while (swapped);
    }
```



## COBOL

This is procedure division only.

```cobol
       C-SORT SECTION.
       C-000.
           DISPLAY "SORT STARTING".

           MOVE 2       TO WC-START
           MOVE WC-SIZE TO WC-END.
           MOVE 1       TO WC-DIRECTION
                           WC-LAST-CHANGE.
           PERFORM E-SHAKER UNTIL WC-END * WC-DIRECTION <
                                  WC-START * WC-DIRECTION.

           DISPLAY "SORT FINISHED".

       C-999.
           EXIT.

       E-SHAKER SECTION.
       E-000.
           PERFORM F-PASS VARYING WB-IX-1 FROM WC-START BY WC-DIRECTION
                          UNTIL WB-IX-1 = WC-END + WC-DIRECTION.

           MOVE WC-START TO WC-END.
           SUBTRACT WC-DIRECTION FROM WC-LAST-CHANGE GIVING WC-START.
           MULTIPLY WC-DIRECTION BY -1 GIVING WC-DIRECTION.

       E-999.
           EXIT.

       F-PASS SECTION.
       F-000.
           IF WB-ENTRY(WB-IX-1 - 1) > WB-ENTRY(WB-IX-1)
              SET  WC-LAST-CHANGE        TO WB-IX-1
              MOVE WB-ENTRY(WB-IX-1 - 1) TO WC-TEMP
              MOVE WB-ENTRY(WB-IX-1)     TO WB-ENTRY(WB-IX-1 - 1)
              MOVE WC-TEMP               TO WB-ENTRY(WB-IX-1).

       F-999.
           EXIT.
```



## Common Lisp

This version works on lists and vectors alike.  The vector implementation is coded directly.  The list version uses an intermediate vector.

```lisp
(defun cocktail-sort-vector (vector predicate &aux (len (length vector)))
  (labels ((scan (start step &aux swapped)
             (loop for i = start then (+ i step) while (< 0 i len) do
               (when (funcall predicate (aref vector i)
                                        (aref vector (1- i)))
                 (rotatef (aref vector i)
                          (aref vector (1- i)))
                 (setf swapped t)))
             swapped))
    (loop while (and (scan 1         1)
                     (scan (1- len) -1))))
  vector)

(defun cocktail-sort (sequence predicate)
  (etypecase sequence
    (vector (cocktail-sort-vector sequence predicate))
    (list (map-into sequence 'identity
                    (cocktail-sort-vector (coerce sequence 'vector)
                                          predicate)))))
```



## D


```d
// Written in the D programming language.
module rosettaCode.sortingAlgorithms.cocktailSort;

import std.range;

Range cocktailSort(Range)(Range data)
if (isRandomAccessRange!Range && hasLvalueElements!Range) {
    import std.algorithm : swap;
    bool swapped = void;
    void trySwap(E)(ref E lhs, ref E rhs) {
        if (lhs > rhs) {
            swap(lhs, rhs);
            swapped = true;
        }
    }

    if (data.length > 0) do {
        swapped = false;
        foreach (i; 0 .. data.length - 1)
            trySwap(data[i], data[i + 1]);
        if (!swapped)
            break;
        swapped = false;
        foreach_reverse (i; 0 .. data.length - 1)
            trySwap(data[i], data[i + 1]);
    } while(swapped);
    return data;
}

unittest {
    assert (cocktailSort([3, 1, 5, 2, 4]) == [1, 2, 3, 4, 5]);
    assert (cocktailSort([1, 2, 3, 4, 5]) == [1, 2, 3, 4, 5]);
    assert (cocktailSort([5, 4, 3, 2, 1]) == [1, 2, 3, 4, 5]);
    assert (cocktailSort((int[]).init)    == []);
    assert (cocktailSort(["John", "Kate", "Zerg", "Alice", "Joe", "Jane"]) ==
        ["Alice", "Jane", "Joe", "John", "Kate", "Zerg"]);
}

```


{{out}}

```d

import rosettaCode.sortingAlgorithms.cocktailSort;

void main() {
    import std.stdio, std.algorithm, std.range, std.random;
    //generate 10 sorted random numbers in [0 .. 10)
    rndGen.take(10).map!(a=>a%10).array.cocktailSort.writeln();
}
```


```txt
[2, 2, 3, 4, 5, 5, 7, 7, 7, 8]
```



## Delphi

Dynamic array is a 0-based array of variable length

Static array is an arbitrary-based array of fixed length

```Delphi
program TestShakerSort;

{$APPTYPE CONSOLE}

{.$DEFINE DYNARRAY}  // remove '.' to compile with dynamic array

type
  TItem = Integer;   // declare ordinal type for array item
{$IFDEF DYNARRAY}
  TArray = array of TItem;          // dynamic array
{$ELSE}
  TArray = array[0..15] of TItem;   // static array
{$ENDIF}

procedure ShakerSort(var A: TArray);
var
  Item: TItem;
  K, L, R, J: Integer;

begin
  L:= Low(A) + 1;
  R:= High(A);
  K:= High(A);
  repeat
    for J:= R downto L do begin
      if A[J - 1] > A[J] then begin
        Item:= A[J - 1];
        A[J - 1]:= A[J];
        A[J]:= Item;
        K:= J;
      end;
    end;
    L:= K + 1;
    for J:= L to R do begin
      if A[J - 1] > A[J] then begin
        Item:= A[J - 1];
        A[J - 1]:= A[J];
        A[J]:= Item;
        K:= J;
      end;
    end;
    R:= K - 1;
  until L > R;
end;

var
  A: TArray;
  I: Integer;

begin
{$IFDEF DYNARRAY}
  SetLength(A, 16);
{$ENDIF}
  for I:= Low(A) to High(A) do
    A[I]:= Random(100);
  for I:= Low(A) to High(A) do
    Write(A[I]:3);
  Writeln;
  ShakerSort(A);
  for I:= Low(A) to High(A) do
    Write(A[I]:3);
  Writeln;
  Readln;
end.
```

{{out}}

```txt

  0  3 86 20 27 67 31 16 37 42  8 47  7 84  5 29
  0  3  5  7  8 16 20 27 29 31 37 42 47 67 84 86

```



## E


```e
/** Cocktail sort (in-place) */
def cocktailSort(array) {
    def swapIndexes := 0..(array.size() - 2)
    def directions := [swapIndexes, swapIndexes.descending()]
    while (true) {
        for direction in directions {
            var swapped := false
            for a ? (array[a] > array[def b := a + 1]) in direction {
                def t    := array[a]
                array[a] := array[b]
                array[b] := t
                swapped  := true
            }
            if (!swapped) { return }
        }
    }
}
```

Note that this solution contains no repeated code to handle the forward and reverse passes.

## Eiffel


```Eiffel


class
	COCKTAIL_SORT [G -> COMPARABLE]

feature

	cocktail_sort (ar: ARRAY [G]): ARRAY [G]
			-- Array sorted in ascending order.
		require
			ar_not_empty: ar.count >= 1
		local
			not_swapped: BOOLEAN
			sol: ARRAY [G]
			i, j: INTEGER
			t: G
		do
			create Result.make_empty
			Result.deep_copy (ar)
			from
			until
				not_swapped = True
			loop
				not_swapped := True
				from
					i := Result.lower
				until
					i = Result.upper - 1
				loop
					if Result [i] > Result [i + 1] then
						Result := swap (Result, i)
						not_swapped := False
					end
					i := i + 1
				end
				from
					j := Result.upper - 1
				until
					j = Result.lower
				loop
					if Result [j] > Result [j + 1] then
						Result := swap (Result, j)
						not_swapped := False
					end
					j := j - 1
				end
			end
		ensure
			ar_is_sorted: is_sorted (Result)
		end

feature{NONE}

	swap (ar: ARRAY [G]; i: INTEGER): ARRAY [G]
			-- Array with elements i and i+1 swapped.
		require
			ar_not_void: ar /= Void
			i_is_in_bounds: ar.valid_index (i)
		local
			t: G
		do
			create Result.make_empty
			Result.deep_copy (ar)
			t := Result [i]
			Result [i] := Result [i + 1]
			Result [i + 1] := t
		ensure
			swapped_right: Result [i + 1] = ar [i]
			swapped_left: Result [i] = ar [i + 1]
		end

	is_sorted (ar: ARRAY [G]): BOOLEAN
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

Test:

```Eiffel


class
	APPLICATION

create
	make

feature

	make
		do
			test := <<5, 1, 99, 3, 2>>
			io.put_string ("unsorted%N")
			across
				test as t
			loop
				io.put_string (t.item.out + "%T")
			end
			io.new_line
			io.put_string ("sorted%N")
			create cs
			test := cs.cocktail_sort (test)
			across
				test as ar
			loop
				io.put_string (ar.item.out + "%T")
			end
		end

	cs: COCKTAIL_SORT [INTEGER]

	test: ARRAY [INTEGER]

end


```

{{out}}

```txt

unsorted
5 1 99 3 2
sorted
1 2 3 5 99

```


## Elena

ELENA 4.1 :

```elena
import extensions;
import system'math;
import system'routines;

extension op
{
    cocktailSort()
    {
        var list := self.clone();

        bool swapped  := true;
        while(swapped)
        {
            swapped := false;

            for(int i := 0, i <= list.Length - 2, i += 1)
            {
                if (list[i]>list[i+1])
                {
                    list.exchange(i,i+1);
                    swapped := true
                }
            };
            ifnot (swapped)
            {
                ^ list
            };
            swapped := false;

            for(int i := list.Length - 2, i >= 0, i -= 1)
            {
                if (list[i]>list[i+1])
                {
                    list.exchange(i,i+1);
                    swapped := true
                }
            }
        };

        ^ list
    }
}

public program()
{
    var list := new int[]::(3, 5, 1, 9, 7, 6, 8, 2, 4 );

    console.printLine("before:", list.asEnumerable());
    console.printLine("after :", list.cocktailSort().asEnumerable())
}
```

{{out}}

```txt

before:3,5,1,9,7,6,8,2,4
after :1,2,3,4,5,6,7,8,9

```



## Elixir


```elixir
defmodule Sort do
  def cocktail_sort(list) when is_list(list), do: cocktail_sort(list, [], [])

  defp cocktail_sort([], minlist, maxlist), do: Enum.reverse(minlist, maxlist)
  defp cocktail_sort([x], minlist, maxlist), do: Enum.reverse(minlist, [x | maxlist])
  defp cocktail_sort(list, minlist, maxlist) do
    {max, rev} = cocktail_max(list, [])
    {min, rest} = cocktail_min(rev, [])
    cocktail_sort(rest, [min | minlist], [max | maxlist])
  end

  defp cocktail_max([max], list), do: {max, list}
  defp cocktail_max([x,y | t], list) when x<y, do: cocktail_max([y | t], [x | list])
  defp cocktail_max([x,y | t], list)         , do: cocktail_max([x | t], [y | list])

  defp cocktail_min([min], list), do: {min, list}
  defp cocktail_min([x,y | t], list) when x>y, do: cocktail_min([y | t], [x | list])
  defp cocktail_min([x,y | t], list)         , do: cocktail_min([x | t], [y | list])
end

IO.inspect Sort.cocktail_sort([5,3,9,4,1,6,8,2,7])
```


{{out}}

```txt

[1, 2, 3, 4, 5, 6, 7, 8, 9]

```



## Euphoria


```euphoria
function cocktail_sort(sequence s)
    integer swapped, d
    object temp
    sequence fromto
    fromto = {1,length(s)-1}
    swapped = 1
    d = 1
    while swapped do
        swapped = 0
        for i = fromto[(1-d)/2+1] to fromto[(1+d)/2+1] by d do
            if compare(s[i],s[i+1])>0 then
                temp = s[i]
                s[i] = s[i+1]
                s[i+1] = temp
                swapped = 1
            end if
        end for
        d = -d
    end while
    return s
end function

constant s = rand(repeat(1000,10))
? s
? cocktail_sort(s)
```

{{out}}

```txt
{963,398,374,455,53,210,611,285,984,308}
{53,210,285,308,374,398,455,611,963,984}

```



## Factor


### Pseudocode translation

This is a faithful translation of the pseudocode given in the task description. It uses lexical variables.

```factor
USING: kernel locals math math.ranges sequences ;

:: cocktail-sort! ( seq -- seq' )
    f :> swapped!                                         ! bind false to mutable lexical variable 'swapped'. This must be done outside both while quotations so it is in scope of both.
    [ swapped ] [                                         ! is swapped true? Then execute body quotation. 'do' executes body quotation before predicate on first pass.
        f swapped!                                        ! set swapped to false
        seq length 2 - [| i |                             ! for each i in 0 to seq length - 2 do
            i i 1 + [ seq nth ] bi@ >                     ! is element at index i greater than element at index i + 1?
            [ i i 1 + seq exchange t swapped! ] when      ! if so, swap them and set swapped to true
        ] each-integer
        swapped [                                         ! skip to end of loop if swapped is false
            seq length 2 - 0 [a,b] [| i |                 ! for each i in seq length - 2 to 0 do
                i i 1 + [ seq nth ] bi@ >                 ! is element at index i greater than element at index i + 1?
                [ i i 1 + seq exchange t swapped! ] when  ! if so, swap them and set swapped to true
            ] each
        ] when
    ] do while
    seq ;                                                 ! return the sequence
```



### More idiomatic

This is perhaps a more idiomatic solution, eschewing the use of lexical variables. If we had tried to translate the pseudocode directly, we'd be dealing with a data stack that is 6+ values deep. Our main strategy against this is to factor the problem into short words that deal with only a few items at a time. When writing mutating words, we can simplify matters by writing words that return nothing, and letting the caller decide if and how to leave references on the data stack.

```factor
USING: fry kernel math math.ranges namespaces sequences ;

SYMBOL: swapped?

: dupd+ ( m obj -- m n obj ) [ dup 1 + ] dip ;

: 2nth ( n seq -- elt1 elt2 ) dupd+ [ nth ] curry bi@ ;

: ?exchange ( n seq -- )
    2dup 2nth > [ dupd+ exchange swapped? on ] [ 2drop ] if ;

: cocktail-pass ( seq forward? -- )
    '[ length 2 - 0 _ [ swap ] when [a,b] ] [ ] bi
    [ ?exchange ] curry each ;

: (cocktail-sort!) ( seq -- seq' )
    swapped? off dup t cocktail-pass
    swapped? get [ dup f cocktail-pass ] when ;

: cocktail-sort! ( seq -- seq' )
    [ swapped? get ] [ (cocktail-sort!) ] do while ;
```



## Forth


```forth
defer precedes                            ( addr addr -- flag )
                                          \ e.g. ' < is precedes
: sort                                    ( a n --)
  1- cells bounds 2>r false
  begin
    0= dup
  while
    2r@ ?do
       i cell+ @ i @ over over precedes   ( mark unsorted )
       if i cell+ ! i ! dup xor else drop drop then
    1 cells +loop
    0= dup
  while
    2r@ swap 1 cells - ?do
       i cell+ @ i @ over over precedes   ( mark unsorted )
       if i cell+ ! i ! dup xor else drop drop then
    -1 cells +loop
  repeat then drop 2r> 2drop
;
```

This is an optimized version:

```forth
: sort
  1- cells bounds 1
  begin
    >r over over r@ -rot true -rot
    r> 0< if 1 cells - then
    ?do
      i cell+ @ i @ over over precedes     ( mark unsorted )
      if i cell+ ! i ! dup xor else drop drop then
    over cells +loop
    >r negate >r swap r@ cells + r> r>
  until drop drop drop
;
```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
PROGRAM COCKTAIL
  IMPLICIT NONE

  INTEGER :: intArray(10) = (/ 4, 9, 3, -2, 0, 7, -5, 1, 6, 8 /)

  WRITE(*,"(A,10I5)") "Unsorted array:", intArray
  CALL Cocktail_sort(intArray)
  WRITE(*,"(A,10I5)") "Sorted array  :", intArray

CONTAINS

  SUBROUTINE Cocktail_sort(a)
    INTEGER, INTENT(IN OUT) :: a(:)
    INTEGER :: i, bottom, top, temp
    LOGICAL :: swapped

    bottom = 1
    top = SIZE(a) - 1
    DO WHILE (bottom < top )
       swapped = .FALSE.
       DO i = bottom, top
          IF (array(i) > array(i+1)) THEN
              temp = array(i)
              array(i) = array(i+1)
              array(i+1) = temp
              swapped = .TRUE.
          END IF
       END DO
       IF (.NOT. swapped) EXIT
       DO i = top, bottom + 1, -1
          IF (array(i) < array(i-1)) THEN
              temp = array(i)
              array(i) = array(i-1)
              array(i-1) = temp
              swapped = .TRUE.
          END IF
       END DO
       IF (.NOT. swapped) EXIT
       bottom = bottom + 1
       top = top - 1
    END DO
  END SUBROUTINE Cocktail_sort

END PROGRAM COCKTAIL
```



## FreeBASIC


```FreeBASIC
' version 21-10-2016
' compile with: fbc -s console
' for boundry checks on array's compile with: fbc -s console -exx

Sub cocktailsort(bs() As Long)
    ' sort from lower bound to the highter bound
    ' array's can have subscript range from -2147483648 to +2147483647
    Dim As Long lb = LBound(bs)
    Dim As Long ub = UBound(bs) -1
    Dim As Long done, i

    Do
        done = 0                  ' going up
        For i = lb To ub
            If bs(i) > bs(i +1) Then
                Swap bs(i), bs(i +1)
                done = 1
            End If
        Next
        ub = ub -1
        If done = 0 Then Exit Do  ' 0 means the array is sorted
        done = 0                  ' going down
        For i = ub To lb Step -1
            If bs(i) > bs(i +1) Then
                Swap bs(i), bs(i +1)
                done = 1
            End If
        Next
        lb = lb +1
    Loop Until done = 0           ' 0 means the array is sorted

End Sub

' ------=< MAIN >=------

Dim As Long i, array(-7 To 7)

Dim As Long a = LBound(array), b = UBound(array)

Randomize Timer
For i = a To b : array(i) = i  : Next
For i = a To b ' little shuffle
    Swap array(i), array(Int(Rnd * (b - a +1)) + a)
Next


Print "unsorted ";
For i = a To b : Print Using "####"; array(i); : Next : Print
cocktailsort(array())  ' sort the array
Print "  sorted ";
For i = a To b : Print Using "####"; array(i); : Next : Print


' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
unsorted   -2  -4   7  -5  -7   4   2  -1   5  -6   6   1   0  -3   3
  sorted   -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=ee5467e58f0ef649373eed5a2503b988 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim siCount, siRev, siProcess As Short
Dim bSorting As Boolean
Dim byToSort As Byte[] = [249, 28, 111, 36, 171, 98, 29, 448, 44, 154, 147, 102, 46, 183, 24,
                          120, 19, 123, 2, 17, 226, 11, 211, 25, 191, 205, 77]

Print "To sort: -"
ShowWorking(byToSort)
Print

Repeat
  bSorting = False
  siRev = byToSort.Max - 1
  For siCount = 0 To byToSort.Max - 1
    siProcess = siCount
    GoSub Check
    siProcess = siRev
    GoSub Check
    Dec siRev
  Next
  If bSorting Then ShowWorking(byToSort)
Until bSorting = False

Return

Check:

If byToSort[siProcess] > byToSort[siProcess + 1] Then
  Swap byToSort[siProcess], byToSort[siProcess + 1]
  bSorting = True
Endif

Return

End
'-----------------------------------------
Public Sub ShowWorking(byToSort As Byte[])
Dim siCount As Byte

For siCount = 0 To byToSort.Max
  Print Str(byToSort[siCount]);
  If siCount <> byToSort.Max Then Print ",";
Next

Print

End
```

Output:

```txt

To sort: -
249,28,111,36,171,98,29,192,44,154,147,102,46,183,24,120,19,123,2,17,226,11,211,25,191,205,77

2,28,111,36,171,98,29,192,44,154,147,102,46,183,24,120,19,123,11,17,226,25,211,77,191,205,249
2,11,28,36,111,98,29,171,44,154,147,102,46,183,24,120,19,123,17,25,192,77,211,191,205,226,249
2,11,17,28,36,98,29,111,44,154,147,102,46,171,24,120,19,123,25,77,183,191,192,205,211,226,249
2,11,17,19,28,36,29,98,44,111,147,102,46,154,24,120,25,123,77,171,183,191,192,205,211,226,249
2,11,17,19,24,28,29,36,44,98,111,102,46,147,25,120,77,123,154,171,183,191,192,205,211,226,249
2,11,17,19,24,25,28,29,36,44,98,102,46,111,77,120,123,147,154,171,183,191,192,205,211,226,249
2,11,17,19,24,25,28,29,36,44,46,98,77,102,111,120,123,147,154,171,183,191,192,205,211,226,249
2,11,17,19,24,25,28,29,36,44,46,77,98,102,111,120,123,147,154,171,183,191,192,205,211,226,249

```


## Go


```go
package main

import "fmt"

func main() {
    a := []int{170, 45, 75, -90, -802, 24, 2, 66}
    fmt.Println("before:", a)
    cocktailSort(a)
    fmt.Println("after: ", a)
}

func cocktailSort(a []int) {
    last := len(a) - 1
    for {
        swapped := false
        for i := 0; i < last; i++ {
            if a[i] > a[i+1] {
                a[i], a[i+1] = a[i+1], a[i]
                swapped = true
            }
        }
        if !swapped {
            return
        }
        swapped = false
        for i := last - 1; i >= 0; i-- {
            if a[i] > a[i+1] {
                a[i], a[i+1] = a[i+1], a[i]
                swapped = true
            }
        }
        if !swapped {
            return
        }
    }
}
```

More generic version that sorts anything that implements <code>sort.Interface</code>:

```go
package main

import (
  "sort"
  "fmt"
)

func main() {
    a := []int{170, 45, 75, -90, -802, 24, 2, 66}
    fmt.Println("before:", a)
    cocktailSort(sort.IntSlice(a))
    fmt.Println("after: ", a)
}

func cocktailSort(a sort.Interface) {
    last := a.Len() - 1
    for {
        swapped := false
        for i := 0; i < last; i++ {
            if a.Less(i+1, i) {
                a.Swap(i, i+1)
                swapped = true
            }
        }
        if !swapped {
            return
        }
        swapped = false
        for i := last - 1; i >= 0; i-- {
            if a.Less(i+1, i) {
                a.Swap(i, i+1)
                swapped = true
            }
        }
        if !swapped {
            return
        }
    }
}
```



## Groovy

Solution:

```groovy
def makeSwap = { a, i, j = i+1 -> print "."; a[[j,i]] = a[[i,j]] }

def checkSwap = { a, i, j = i+1 -> [(a[i] > a[j])].find{ it }.each { makeSwap(a, i, j) } }

def cocktailSort = { list ->
    if (list == null || list.size() < 2) return list
    def n = list.size()
    def swap = checkSwap.curry(list)
    while (true) {
        def swapped = (0..(n-2)).any(swap) && ((-2)..(-n)).any(swap)
        if ( ! swapped ) break
    }
    list
}
```

Test:

```groovy
println cocktailSort([23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4])
println cocktailSort([88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1])

println cocktailSort([ 3, 14, 1, 5, 9, 2, 6, 3 ])
println cocktailSort([ 3, 14 ])
println cocktailSort([ 33, 14 ])
```

{{out}}

```txt
..............................................................................................................................................[4, 12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]
.........................................................................................................................................[0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]
..............[1, 2, 3, 3, 5, 6, 9, 14]
[3, 14]
.[14, 33]
```



## Haskell


```haskell>cocktailSort :: Ord a =
 [a] -> [a]
cocktailSort l
  | not swapped1 = l
  | not swapped2 = reverse $ l1
  | otherwise    = cocktailSort l2
  where (swapped1, l1) = swappingPass (>) (False, []) l
        (swapped2, l2) = swappingPass (<) (False, []) l1

        swappingPass :: Ord a => (a -> a -> Bool) -> (Bool, [a]) -> [a] -> (Bool, [a])
        swappingPass op (swapped, l) (x1 : x2 : xs)
          | op x1 x2  = swappingPass op (True,    x2 : l) (x1 : xs)
          | otherwise = swappingPass op (swapped, x1 : l) (x2 : xs)
        swappingPass _  (swapped, l) [x] = (swapped, x : l)
        swappingPass _  pair         []  = pair
```



## Io


```io
List do (
    cocktailSortInPlace := method(
        start := 0
        end := size - 2

        loop(
            swapped := false

            for(idx, start, end,
                if(at(idx) > at(idx + 1),
                    swapped := true
                    swapIndices(idx, idx + 1)
                )
            )

            if(swapped not, break, end := end - 1)

            for (idx, end, start, -1,
                if(at(idx) > at(idx + 1),
                    swapped := true
                    swapIndices(idx, idx + 1)
                )
            )

            if(swapped not, break, start := start + 1)
        )
    self)
)

l := list(2, 3, 4, 5, 1)
l cocktailSortInPlace println # ==> list(1, 2, 3, 4, 5)
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()                     #: demonstrate various ways to sort a list and string
   demosort(cocktailsort,[3, 14, 1, 5, 9, 2, 6, 3],"qwerty")
end

procedure cocktailsort(X,op)          #: return sorted list
local i,swapped

   op := sortop(op,X)                 # select how and what we sort

   swapped := 1
   repeat                             # translation of pseudo code.  Contractions used to eliminate second loop.
      every (if /swapped then break break else swapped := &null & next) | ( i := 1 to *X-1) |
            (if /swapped then break break else swapped := &null & next) | ( i := *X-1 to 1 by -1) do
         if op(X[i+1],X[i]) then
            X[i+1] :=: X[swapped := i]
   return X
end
```


Note: This example relies on [[Sorting_algorithms/Bubble_sort#Icon| the supporting procedures 'sortop', and 'demosort' in Bubble Sort]].
The full demosort exercises the named sort of a list with op = "numeric", "string", ">>" (lexically gt, descending),">" (numerically gt, descending), a custom comparator, and also a string.
{{out|Abbreviated example output}}

```txt
Sorting Demo using procedure cocktailsort
  on list : [ 3 14 1 5 9 2 6 3 ]
    with op = &null:         [ 1 2 3 3 5 6 9 14 ]   (0 ms)
  ...
  on string : "qwerty"
    with op = &null:         "eqrtwy"   (0 ms)
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "CocktSrt.bas"
110 RANDOMIZE
120 NUMERIC ARRAY(5 TO 24)
130 CALL INIT(ARRAY)
140 CALL WRITE(ARRAY)
150 CALL COCKTAILSORT(ARRAY)
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
280 DEF COCKTAILSORT(REF A)
290   LET ST=LBOUND(A)+1:LET EN=UBOUND(A):LET D,CH=1
300   DO
310     FOR J=ST TO EN STEP D
320       IF A(J-1)>A(J) THEN LET T=A(J-1):LET A(J-1)=A(J):LET A(J)=T:LET CH=J
330     NEXT
340     LET EN=ST:LET ST=CH-D:LET D=-1*D
350   LOOP UNTIL EN*D<ST*D
360 END DEF
```



## J

{{eff note|J|/:~}}

```j
bigToLeft=: (([ (>. , <.) {.@]) , }.@])/
smallToLeft=: (([ (<. , >.) {.@]) , }.@])/
cocktailSort=: |. @: (|. @: smallToLeft @: |. @: bigToLeft ^:_)
```

Test run:

```j
   ?. 10 $ 10
4 6 8 6 5 8 6 6 6 9
   cocktailSort ?. 10 $ 10
4 5 6 6 6 6 6 8 8 9
```

As is usual with J, <code>/:~</code> is the preferred method of sorting in practice.


## Java

This algorithm sorts in place.
Call it with a copy of the array to preserve the unsorted order.

```java
public static void cocktailSort( int[] A ){
	boolean swapped;
	do {
		swapped = false;
		for (int i =0; i<=  A.length  - 2;i++) {
			if (A[ i ] > A[ i + 1 ]) {
				//test whether the two elements are in the wrong order
				int temp = A[i];
				A[i] = A[i+1];
				A[i+1]=temp;
				swapped = true;
			}
		}
		if (!swapped) {
			//we can exit the outer loop here if no swaps occurred.
			break;
		}
		swapped = false;
		for (int i= A.length - 2;i>=0;i--) {
			if (A[ i ] > A[ i + 1 ]) {
				int temp = A[i];
				A[i] = A[i+1];
				A[i+1]=temp;
				swapped = true;
			}
		}
		//if no elements have been swapped, then the list is sorted
	} while (swapped);
}
```



## JavaScript


```javascript

  // Node 5.4.1 tested implementation (ES6)
"use strict";

let arr = [4, 9, 0, 3, 1, 5];
let isSorted = true;
while (isSorted){
    for (let i = 0; i< arr.length - 1;i++){
            if (arr[i] > arr[i + 1])
             {
                let temp = arr[i];
                arr[i] = arr[i + 1];
                arr[i+1] = temp;
                isSorted = true;
             }
    }

    if (!isSorted)
        break;

    isSorted = false;

    for (let j = arr.length - 1; j > 0; j--){
            if (arr[j-1] > arr[j])
             {
                let temp = arr[j];
                arr[j] = arr[j - 1];
                arr[j - 1] = temp;
                isSorted = true;
             }
    }
}
console.log(arr);

}
```



{{out}}

```txt

 [0, 1, 3, 4, 5, 9]

```



## jq

{{ works with|jq|1.4}}

```jq
# In case your jq does not have "until" defined:
def until(cond; next):
  def _until:
    if cond then . else (next|_until) end;
  _until;
```


```jq
def cocktailSort:
  def swap(i;j): .[i] as $t | .[i] = .[j] | .[j] = $t;

  def shake(stream):
    reduce stream as $i
      (.[0]=false;
       .[1] as $A
       | if $A[ $i ] > $A[ $i+1 ] then
           [true, ($A|swap( $i; $i+1 ))]
         else .
         end);

  (length - 2) as $lm2
  # state: [swapped, A]
  | [true, .]
  | until( .[0]|not;
           shake(range(0; $lm2 + 1))
           | if .[0] then
   	       # for each i in length( A ) - 2 down to 0
               shake( $lm2 - range(0; $lm2 + 1))
	     else .
	     end )
  | .[1];
```

'''Tests:'''

```jq
def verify: if cocktailSort == sort then empty else . end;

([],
 [1],
 [1,1],
 [3, 14],
 [33, 14],
 [3, 14, 1, 5, 9, 2, 6, 3],
 [23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4],
 [88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1],
 [1.5, -1.5],
 ["cocktail", ["sort"], null, {}]
) | verify
```

{{out}}

```sh
$ jq -n -c -f cocktail_sort.jq
$
```



## Julia

{{works with|Julia|0.6}}


```julia
function coctailsort(a::Vector)
    b = copy(a)
    isordered = false
    lo, hi = 1, length(b)
    while !isordered && hi > lo
        isordered = true
        for i in lo+1:hi
            if b[i] < b[i-1]
                b[i-1], b[i] = b[i], b[i-1]
                isordered = false
            end
        end
        hi -= 1
        if isordered || hi ≤ lo break end
        for i in hi:-1:lo+1
            if b[i-1] > b[i]
                b[i-1], b[i] = b[i], b[i-1]
                isordered = false
            end
        end
        lo += 1
    end
    return b
end

v = rand(-10:10, 10)
println("# unordered: $v\n -> ordered: ", cocktailsort(v))
```


{{out}}

```txt
# unordered: [6, -9, 5, -10, 2, 4, 6, -6, -3, -8]
 -> ordered: [-10, -9, -8, -6, -3, 2, 4, 5, 6, 6]
```



## Kotlin


```scala
// version 1.1.0

fun cocktailSort(a: IntArray) {
    fun swap(i: Int, j: Int) {
        val temp = a[i]
        a[i] = a[j]
        a[j] = temp
    }
    do {
        var swapped = false
        for (i in 0 until a.size - 1)
            if (a[i] > a[i + 1]) {
                swap(i, i + 1)
                swapped = true
            }
        if (!swapped) break
        swapped = false
        for (i in a.size - 2 downTo 0)
            if (a[i] > a[i + 1]) {
                swap(i, i + 1)
                swapped = true
            }
    }
    while (swapped)
}

fun main(args: Array<String>) {
   val aa = arrayOf(
        intArrayOf(100, 2, 56, 200, -52, 3, 99, 33, 177, -199),
        intArrayOf(4, 65, 2, -31, 0, 99, 2, 83, 782, 1),
        intArrayOf(62, 83, 18, 53, 7, 17, 95, 86, 47, 69, 25, 28)
    )
    for (a in aa) {
        cocktailSort(a)
        println(a.joinToString(", "))
    }
}
```


{{out}}

```txt

-199, -52, 2, 3, 33, 56, 99, 100, 177, 200
-31, 0, 1, 2, 2, 4, 65, 83, 99, 782
7, 17, 18, 25, 28, 47, 53, 62, 69, 83, 86, 95

```



## Lua


```Lua
function cocktailSort( A )
  local swapped
  repeat
    swapped = false
    for i = 1, #A - 1 do
      if A[ i ] > A[ i+1 ] then
         A[ i ], A[ i+1 ] = A[ i+1 ] ,A[i]
         swapped=true
	   end
    end
    if swapped == false then
      break -- repeatd loop;
	end

     for i = #A - 1,1,-1 do
      if A[ i ] > A[ i+1 ] then
         A[ i ], A[ i+1 ] = A[ i+1 ] , A[ i ]
         swapped=true
	   end
    end

  until swapped==false
end
```

{{out|Example}}

```lua
list = { 5, 6, 1, 2, 9, 14, 2, 15, 6, 7, 8, 97 }
cocktailSort(list)
for i=1,#list do
    print(list[i]j)
end
```



## Maple


```Maple
arr := Array([17,3,72,0,36,2,3,8,40,0]):
len := numelems(arr):
swap := proc(arr, a, b)
	local temp := arr[a]:
	arr[a] := arr[b]:
	arr[b] := temp:
end proc:
while(true) do
	swapped := false:
	for i to len-1 do
		if arr[i] > arr[i+1] then:
			swap(arr, i, i+1):
			swapped := true:
		end if:
	end do:
	if (not swapped) then break: end if:
	swapped := false:
	for j from len-1 to 1 by -1 do
		if arr[j] > arr[j+1] then
			swap(arr,j,j+1):
			swapped := true:
		end if:
	end do:
	if (not swapped) then break: end if:
end do:
arr;
```

{{Output|Out}}

```txt
[0,0,2,3,3,8,17,36,40,72]
```



## Mathematica


```Mathematica
cocktailSort[A_List] := Module[ { swapped = True },
While[ swapped == True,
 swapped=False;
 For[ i = 1, i< Length[A]-1,i++,
   If[ A[[i]] > A[[i+1]], A[[i;;i+1]] = A[[i+1;;i;;-1]]; swapped=True;]
 ];
If[swapped == False, Break[]];
swapped=False;
For [ i= Length[A]-1, i > 0, i--,
  If[ A[[i]] > A[[i+1]], A[[i;;i+1]] = A[[i+1;;i;;-1]]; swapped = True;]
 ]]]
```

{{out|Example}}

```txt
cocktailSort[{2,1,5,3,6}]
->{1,2,3,5,6}
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function list = cocktailSort(list)

    %We have to do this because the do...while loop doesn't exist in MATLAB
    swapped = true;

    while swapped

        %Bubble sort down the list
        swapped = false;
        for i = (1:numel(list)-1)
            if( list(i) > list(i+1) )
                list([i i+1]) = list([i+1 i]); %swap
                swapped = true;
            end
        end

        if ~swapped
            break
        end

        %Bubble sort up the list
        swapped = false;
        for i = (numel(list)-1:-1:1)
            if( list(i) > list(i+1) )
                list([i i+1]) = list([i+1 i]); %swap
                swapped = true;
            end %if
        end %for
    end %while
end %cocktail sort
```

{{out|Sample usage}}

```MATLAB
cocktailSort([6 3 7 8 5 1 2 4 9])

ans =

     1     2     3     4     5     6     7     8     9
```



## MAXScript


```maxscript
fn cocktailSort arr =
(
    local swapped = true
    while swapped do
    (
        swapped = false
        for i in 1 to (arr.count-1) do
        (
            if arr[i] > arr[i+1] then
            (
                swap arr[i] arr[i+1]
                swapped = true
            )
        )
        if not swapped then exit
        for i in (arr.count-1) to 1 by -1 do
        (
            if arr[i] > arr[i+1] then
            (
                swap arr[i] arr[i+1]
                swapped = true
            )
        )
    )
    return arr
)
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

placesList = [String -
    "UK  London",     "US  New York"   -
  , "US  Boston",     "US  Washington" -
  , "UK  Washington", "US  Birmingham" -
  , "UK  Birmingham", "UK  Boston"     -
]
sortedList = cocktailSort(String[] Arrays.copyOf(placesList, placesList.length))

lists = [placesList, sortedList]
loop ln = 0 to lists.length - 1
  cl = lists[ln]
  loop ct = 0 to cl.length - 1
    say cl[ct]
    end ct
    say
  end ln

return

method cocktailSort(A = String[]) public constant binary returns String[]

  Alength = A.length
  swapped = isFalse
  loop label swapped until \swapped
    swapped = isFalse
    loop i_ = 0 to Alength - 2
      if A[i_].compareTo(A[i_ + 1]) > 0 then do
        swap      = A[i_ + 1]
        A[i_ + 1] = A[i_]
        A[i_]     = swap
        swapped = isTrue
        end
      end i_
    if \swapped then do
      leave swapped
      end
    swapped = isFalse
    loop i_ = Alength - 2 to 0 by -1
      if A[i_].compareTo(A[i_ + 1]) > 0 then do
        swap      = A[i_ + 1]
        A[i_ + 1] = A[i_]
        A[i_]     = swap
        swapped = isTrue
        end
      end i_
    end swapped

  return A

method isTrue public constant binary returns boolean
  return 1 == 1

method isFalse public constant binary returns boolean
  return \isTrue
```

{{out}}

```txt

UK  London
US  New York
US  Boston
US  Washington
UK  Washington
US  Birmingham
UK  Birmingham
UK  Boston

UK  Birmingham
UK  Boston
UK  London
UK  Washington
US  Birmingham
US  Boston
US  New York
US  Washington

```



## Nim


```nim
template trySwap(): stmt {.immediate.} =
  if a[i] < a[i-1]:
    swap a[i], a[i-1]
    t = false

proc cocktailSort[T](a: var openarray[T]) =
  var t = false
  var l = a.len
  while not t:
    t = true
    for i in 1 .. <l: trySwap
    if t: break
    for i in countdown(l-1, 1): trySwap

var a = @[4, 65, 2, -31, 0, 99, 2, 83, 782]
cocktailSort a
echo a
```

{{out}}

```txt
@[-31, 0, 2, 2, 4, 65, 83, 99, 782]
```



## Objeck


```objeck
bundle Default {
  class Cocktail {
    function : Main(args : String[]) ~ Nil {
      values := [5, -1, 101, -4, 0, 1, 8, 6,  2, 3 ];
      CocktailSort(values);
      each(i : values) {
        values[i]->PrintLine();
      };
    }

    function : CocktailSort(a : Int[]) ~ Nil {
      swapped : Bool;
      do {
        swapped := false;
        continue := true;
        for (i := 0; i <= a->Size()  - 2 & continue; i += 1;) {
          if(a[i] > a[i + 1]) {
            temp := a[i];
            a[i] := a[i+1];
            a[i+1] := temp;
            swapped := true;
          };
        };

        if(swapped <> true) {
          continue := false;
        };

        swapped := false;
        for(i := a->Size() - 2; i >= 0; i -= 1;){
          if(a[i] > a[i + 1]) {
            temp := a[i];
            a[i] := a[i+1];
            a[i + 1] := temp;
            swapped := true;
          };
        };
      }
      while(swapped);
    }
  }
}
```



## OCaml


```ocaml
let swap a i j =
  let tmp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- tmp;
;;

let cocktail_sort a =
  let begin_ = ref(-1)
  and end_ = ref(Array.length a - 2) in
  let swapped = ref true in
  try while !swapped do
    swapped := false;
    incr begin_;
    for i = !begin_ to !end_ do
      if a.(i) > a.(i+1) then begin
        swap a (i) (i+1);
        swapped := true;
      end;
    done;
    if !swapped = false then raise Exit;
    swapped := false;
    decr end_;
    for i = !end_ downto !begin_ do
      if a.(i) > a.(i+1) then begin
        swap a (i) (i+1);
        swapped := true
      end;
    done;
  done with Exit -> ()
;;

let () =
  let a = [| 3; 7; 4; 9; 6; 1; 8; 5; 2; |] in
  cocktail_sort a;
  Array.iter (Printf.printf " %d") a;
  print_newline();
;;
```

{{out}}

```txt

 1 2 3 4 5 6 7 8 9

```



## Octave


```octave
function sl = cocktailsort(l)
  swapped = true;
  while(swapped)
    swapped = false;
    for i = 1:(length(l)-1)
      if ( l(i) > l(i+1) )
	t = l(i);
	l(i) = l(i+1);
	l(i+1) = t;
	swapped = true;
      endif
    endfor
    if ( !swapped )
      break;
    endif
    swapped = false;
    for i = (length(l)-1):-1:1
      if ( l(i) > l(i+1) )
	t = l(i);
	l(i) = l(i+1);
	l(i+1) = t;
	swapped = true;
      endif
    endfor
  endwhile
  sl = l;
endfunction
```

{{out|Example}}

```octave
s = cocktailsort([5, -1, 101, -4, 0,       \
		  1, 8,    6,  2, 3 ]);
disp(s);
```



## ooRexx

{{Trans|NetRexx}}

```ooRexx
/* Rexx */

placesList = .array~of( -
    "UK  London",     "US  New York"   , "US  Boston",     "US  Washington" -
  , "UK  Washington", "US  Birmingham" , "UK  Birmingham", "UK  Boston"     -
)

sortedList = cocktailSort(placesList~allItems())

lists = .array~of(placesList, sortedList)
loop ln = 1 to lists~items()
  cl = lists[ln]
  loop ct = 1 to cl~items()
    say cl[ct]
    end ct
    say
  end ln

return
exit

::routine cocktailSort
  use arg A

  Alength = A~items()
  swapped = .false
  loop label swaps until \swapped
    swapped = .false
    loop i_ = 1 to Alength - 1
      if A[i_] > A[i_ + 1] then do
        swap      = A[i_ + 1]
        A[i_ + 1] = A[i_]
        A[i_]     = swap
        swapped = .true
        end
      end i_
    if \swapped then do
      leave swaps
      end
    swapped = .false
    loop i_ = Alength - 1 to 1 by -1
      if A[i_] > A[i_ + 1] then do
        swap      = A[i_ + 1]
        A[i_ + 1] = A[i_]
        A[i_]     = swap
        swapped = .true
        end
      end i_
    end swaps

  return A
```

{{out}}

```txt

UK  London
US  New York
US  Boston
US  Washington
UK  Washington
US  Birmingham
UK  Birmingham
UK  Boston

UK  Birmingham
UK  Boston
UK  London
UK  Washington
US  Birmingham
US  Boston
US  New York
US  Washington

```



## Oz


```oz
declare
  proc {CocktailSort Arr}
     proc {Swap I J}
        Arr.J := (Arr.I := Arr.J) %% assignment returns the old value
     end
     IsSorted = {NewCell false}
     Up = {List.number {Array.low Arr} {Array.high Arr}-1 1}
     Down = {Reverse Up}
  in
     for until:@IsSorted break:Break do
	for Range in [Up Down] do
	   IsSorted := true
	   for I in Range do
	      if Arr.I > Arr.(I+1) then
		 IsSorted := false
		 {Swap I I+1}
	      end
	   end
	   if @IsSorted then {Break} end
	end
     end
  end
  Arr = {Tuple.toArray unit(10 9 8 7 6 5 4 3 2 1)}
in
  {CocktailSort Arr}
  {Inspect Arr}
```



## PARI/GP


```parigp
cocktailSort(v)={
  while(1,
    my(done=1);
    for(i=2,#v,
      if(v[i-1]>v[i],
        my(t=v[i-1]);
        v[i-1]=v[i];
        v[i]=t;
        done=0
      )
    );
    if(done, return(v));
    done=1;
    forstep(i=#v,2,-1,
      if(v[i-1]>v[i],
        my(t=v[i-1]);
        v[i-1]=v[i];
        v[i]=t;
        done=0
      )
    );
    if(done, return(v))
  )
};
```



## Pascal

See [[Sorting_algorithms/Cocktail_sort#Delphi | Delphi]]


## Perl


```perl
use strict;
use warnings;

my @B=qw(t h e q u i c k b r o w n f o x j u m p s o v e r t h e l a z y d o g);
print "@B\n";
my @C=cocktailSort(@B);
print "@C\n";
################### cocktailSort #####################
sub cocktailSort {   #( A : list of sortable items ) defined as:
  my @A = @_;
  my $swapped = 1;
  while ($swapped == 1) {
    $swapped = 0;
    for (my $i=0; $i<($#A-1); $i+=1) {

      if ($A[$i] gt $A[$i+1]) { # test whether the two
                            # elements are in the wrong
                            # order

         ($A[$i+1], $A[$i])=($A[$i], $A[$i+1]); # let the two elements
                                                # change places
        $swapped = 1;
      }
    }
    if ($swapped == 0) {
      # we can exit the outer loop here if no swaps occurred.
      print "no more swaps";
    }
    else {
    $swapped = 0;
    for (my $i=($#A-1); $i>0 ; $i-=1) {

      if($A[$i] gt $A[$i+1]) {

        ($A[$i+1], $A[$i])=($A[$i], $A[$i+1]);
        $swapped = 1;
      }
    }
    }
#  if no elements have been swapped,
#  then the list is sorted
  }
return (@A);
#end sub
}
```



## Perl 6


```perl6
sub cocktail_sort ( @a ) {
    my $range = 0 ..^ @a.end;
    loop {
        my $swapped_forward = 0;
        for $range.list -> $i {
            if @a[$i] > @a[$i+1] {
                @a[ $i, $i+1 ] .= reverse;
                $swapped_forward = 1;
            }
        }
        last if not $swapped_forward;

        my $swapped_backward = 0;
        for $range.reverse -> $i {
            if @a[$i] > @a[$i+1] {
                @a[ $i, $i+1 ] .= reverse;
                $swapped_backward = 1;
            }
        }
        last if not $swapped_backward;
    }
    return @a;
}

my @weights = (^50).map: { 100 + ( 1000.rand.Int / 10 ) };
say @weights.sort.Str eq @weights.&cocktail_sort.Str ?? 'ok' !! 'not ok';
```



## Phix

Copy of [[Sorting_algorithms/Cocktail_sort#Euphoria|Euphoria]]

```Phix
function cocktail_sort(sequence s)
integer swapped = 1, f = 1, t = length(s)-1, d = 1
    while swapped do
        swapped = 0
        for i=f to t by d do
            if s[i]>s[i+1] then
                {s[i],s[i+1],swapped} = {s[i+1],s[i],1}
            end if
        end for
        -- swap to and from, and flip direction.
        -- additionally, we can reduce one element to be
        -- examined, depending on which way we just went.
        {f,t,d} = {t-(d=1),f-(d=-1),-d}
    end while
    return s
end function

constant s = sq_rand(repeat(1000,10))
? s
? cocktail_sort(s)
```

{{out}}

```txt

{116,496,996,670,293,446,565,423,118,976}
{116,118,293,423,446,496,565,670,976,996}

```



## PHP


```php
function cocktailSort($arr){
	if (is_string($arr)) $arr = str_split(preg_replace('/\s+/','',$arr));

	do{
		$swapped = false;
		for($i=0;$i<count($arr);$i++){
			if(isset($arr[$i+1])){
				if($arr[$i] > $arr[$i+1]){
					list($arr[$i], $arr[$i+1]) = array($arr[$i+1], $arr[$i]);
					$swapped = true;
				}
			}
		}

		if ($swapped == false) break;

		$swapped = false;
		for($i=count($arr)-1;$i>=0;$i--){
			if(isset($arr[$i-1])){
				if($arr[$i] < $arr[$i-1]) {
					list($arr[$i],$arr[$i-1]) = array($arr[$i-1],$arr[$i]);
					$swapped = true;
				}
			}
		}
	}while($swapped);

	return $arr;
}
$arr = array(5, 1, 7, 3, 6, 4, 2);
$arr2 = array("John", "Kate", "Alice", "Joe", "Jane");
$strg = "big fjords vex quick waltz nymph";
$arr = cocktailSort($arr);
$arr2 = cocktailSort($arr2);
$strg = cocktailSort($strg);
echo implode(',',$arr) . '
';
echo implode(',',$arr2) . '
';
echo implode('',$strg) . '
';
```

{{out}}

```txt

1,2,3,4,5,6,7
Alice,Jane,Joe,John,Kate
abcdefghiijklmnopqrstuvwxyz

```



## PicoLisp


```PicoLisp
(de cocktailSort (Lst)
   (use (Swapped L)
      (loop
         (off Swapped)
         (setq L Lst)
         (while (cdr L)
            (when (> (car L) (cadr L))
               (xchg L (cdr L))
               (on Swapped) )
            (pop 'L) )
         (NIL Swapped Lst)
         (off Swapped)
         (loop
            (setq L (prior L Lst))  # Not recommended (inefficient)
            (when (> (car L) (cadr L))
               (xchg L (cdr L))
               (on Swapped) )
            (T (== Lst L)) )
         (NIL Swapped Lst) ) ) )
```

{{out}}

```txt
: (cocktailSort (make (do 9 (link (rand 1 999)))))
-> (1 167 183 282 524 556 638 891 902)
: (cocktailSort (make (do 9 (link (rand 1 999)))))
-> (82 120 160 168 205 226 408 708 719)
```



## PL/I


```PL/I
cocktail: procedure (A);
   declare A(*) fixed;
   declare t fixed;
   declare stable bit (1);
   declare (i, n) fixed binary (31);

   n = hbound(A,1);
   do until (stable);
      stable = '1'b;
      do i = 1 to n-1, n-1 to 1 by -1;
         if A(i) > A(i+1) then
            do; stable = '0'b; /* still unsorted, so set false. */
                t = A(i); A(i) = A(i+1); A(i+1) = t;
            end;
      end;
   end;
end cocktail;
```



## PowerShell

Based on the entry for PowerShell in [[Bubble Sort]]

```PowerShell
function CocktailSort ($a) {
    $l = $a.Length
	$m = 0
	if( $l -gt 1 )
	{
		$hasChanged = $true
		:outer while ($hasChanged) {
			$hasChanged = $false
			$l--
			for ($i = $m; $i -lt $l; $i++) {
				if ($a[$i] -gt $a[$i+1]) {
					$a[$i], $a[$i+1] = $a[$i+1], $a[$i]
					$hasChanged = $true
				}
			}
			if(-not $hasChanged) {
				break outer
			}
			$hasChanged = $false
			for ($i = $l; $i -gt $m; $i--) {
				if ($a[$i-1] -gt $a[$i]) {
					$a[$i-1], $a[$i] = $a[$i], $a[$i-1]
					$hasChanged = $true
				}
			}
			$m++
		}
	}
	$a
}

$l = 10; CocktailSort ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( -( $l - 1 ), $l - 1 ) } )
```



## Prolog


```Prolog
ctail(_, [], Rev, Rev, sorted) :- write(Rev), nl.
ctail(fwrd, [A,B|T], In, Rev, unsorted) :- A > B, !,
	ctail(fwrd, [B,A|T], In, Rev, _).
ctail(bkwd, [A,B|T], In, Rev, unsorted) :- A < B, !,
	ctail(bkwd, [B,A|T], In, Rev, _).
ctail(D,[A|T], In, Rev, Ch) :- !, ctail(D, T, [A|In], Rev, Ch).

cocktail([], []).
cocktail(In, [Min|Out]) :-
	ctail(fwrd, In, [], [Max|Rev], SFlag),
	( SFlag=sorted->reverse([Max|Rev], [Min|Out]);
	 (ctail(bkwd, Rev, [Max], [Min|Tmp], SortFlag),
	  (SortFlag=sorted->Out=Tmp; !, cocktail(Tmp, Out)))).

test :-  In = [8,9,1,3,4,2,6,5,4],
	 writef('  input=%w\n', [In]),
	 cocktail(In, R),
	 writef('-> %w\n', [R]).

```

{{out|Example}}

```txt
?- test.
  input=[8,9,1,3,4,2,6,5,4]
[9,4,5,6,2,4,3,1,8]
[1,8,2,3,4,4,6,5,9]
[9,8,5,6,4,4,3,2]
[2,3,4,4,5,6,8,9]
[9,8,6,5,4,4,3]
-> [1,2,3,4,4,5,6,8,9]

```



## PureBasic

The following approach improves on the method in the pseudo-code by not examining indexes on either end of the array that have already been sorted by reducing the index range on each subsequent pass.

```PureBasic
;sorts an array of integers
Procedure cocktailSort(Array a(1))
  Protected index, hasChanged, low, high

  low = 0
  high = ArraySize(a()) - 1
  Repeat
    hasChanged = #False
    For index = low To high
      If a(index) > a(index + 1)
        Swap a(index), a(index + 1)
        hasChanged = #True
      EndIf
    Next
    high - 1

    If hasChanged = #False
      Break ;we can exit the outer loop here if no changes were made
    EndIf


    hasChanged = #False
    For index = high To low Step -1
      If a(index) > a(index + 1)
        Swap a(index), a(index + 1)
        hasChanged = #True
      EndIf
    Next
    low + 1
  Until hasChanged = #False ;if no elements have been changed, then the array is sorted
EndProcedure
```



## Python

This implementation takes advantage of the identical processing of the two ''for'' loops and that a ''range'' is a first-class object in Python.

```python
def cocktailSort(A):
    up = range(len(A)-1)
    while True:
        for indices in (up, reversed(up)):
            swapped = False
            for i in indices:
                if A[i] > A[i+1]:
                    A[i], A[i+1] =  A[i+1], A[i]
                    swapped = True
            if not swapped:
                return
```

{{out|Usage}}

```python
test1 = [7, 6, 5, 9, 8, 4, 3, 1, 2, 0]
cocktailSort(test1)
print test1
#>>> [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

test2=list('big fjords vex quick waltz nymph')
cocktailSort(test2)
print ''.join(test2)
#>>>      abcdefghiijklmnopqrstuvwxyz
```


This implementation is clearer in structure to it's bubblesort origins while also being ever so slightly faster, in python 3.5.2 at least.

```python
def cocktail(a):
    for i in range(len(a)//2):
        swap = False
        for j in range(1+i, len(a)-i):
            if a[j] < a[j-1]:
                a[j], a[j-1] = a[j-1], a[j]
                swap = True
        if not swap:
            break
        swap = False
        for j in range(len(a)-i-1, i, -1):
            if a[j] < a[j-1]:
                a[j], a[j-1] = a[j-1], a[j]
                swap = True
        if not swap:
            break
```



## R


```R
cocktailsort <- function(x)
{
   lenx <- length(x)
   repeat
   {
      swapped <- FALSE
      for(i in 1:(lenx-1))
      {
         if(x[i] > x[i+1])
         {
            temp <- x[i]
            x[i] <- x[i+1]
            x[i+1] <- temp
            swapped <- TRUE
         }
      }
      if(!swapped) break

      swapped <- FALSE
      for(i in (lenx-1):1)
      {
         if(x[i] > x[i+1])
         {
            temp <- x[i]
            x[i] <- x[i+1]
            x[i+1] <- temp
            swapped <- TRUE
         }
      }
      if(!swapped) break
   }
   x
}

print(cocktailsort(c(5, -1, 101, -4, 0, 1, 8,    6,  2, 3)))
```



## Racket


```racket

#lang racket
(require (only-in srfi/43 vector-swap!))

(define (cocktail-sort! xs)
  (define (ref i) (vector-ref xs i))
  (define (swap i j) (vector-swap! xs i j))
  (define len (vector-length xs))
  (define (bubble from to delta)
    (for/fold ([swaps 0]) ([i (in-range from to delta)])
      (cond [(> (ref i) (ref (+ i 1)))
             (swap i (+ i 1)) (+ swaps 1)]
            [swaps])))
  (let loop ()
    (cond [(zero? (bubble 0 (- len 2)  1)) xs]
          [(zero? (bubble (- len 2) 0  -1)) xs]
          [(loop)])))

```



## REXX


### version handles blanks

This REXX version can handle an array that may contain blanks or spaces.

```rexx
/*REXX program sorts an array using the cocktail─sort method,  A.K.A.:  happy hour sort,*/
                                                 /*   bidirectional bubble sort,        */
                                                 /*   cocktail shaker sort, ripple sort,*/
                                                 /*   a selection sort variation,       */
                                                 /*   shuffle sort,  shuttle sort,   or */
                                                 /*   a bubble sort variation.          */
call gen@                                        /*generate some array elements.        */
call show@ 'before sort'                         /*show  unsorted  array elements.      */
     say copies('█', 101)                        /*show a separator line  (a fence).    */
call cocktailSort  #                             /*invoke the cocktail sort subroutine. */
call show@ ' after sort'                         /*show    sorted  array elements.      */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
cocktailSort: procedure expose @.;    parse arg N             /*N:  is number of items. */
                     do until done;   done= 1
                         do j=1    for N-1;              jp= j+1
                         if @.j>@.jp  then do;  done=0;  _=@.j;  @.j=@.jp;  @.jp=_;  end
                         end   /*j*/
                     if done  then leave                      /*No swaps done?  Finished*/
                         do k=N-1  for N-1  by -1;       kp= k+1
                         if @.k>@.kp  then do;  done=0;  _=@.k;  @.k=@.kp;  @.kp=_;  end
                         end   /*k*/
                     end       /*until*/
              return
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen@: @.=                                        /*assign a default value for the stem. */
      @.1 ='---the 22 card tarot deck (larger deck has 56 additional cards in 4 suits)---'
      @.2 ='
### =======symbol====================pip===================================
'
      @.3 ='the juggler                  ◄───     I'
      @.4 ='the high priestess  [Popess] ◄───    II'
      @.5 ='the empress                  ◄───   III'
      @.6 ='the emperor                  ◄───    IV'
      @.7 ='the hierophant  [Pope]       ◄───     V'
      @.8 ='the lovers                   ◄───    VI'
      @.9 ='the chariot                  ◄───   VII'
      @.10='justice                      ◄───  VIII'
      @.11='the hermit                   ◄───    IX'
      @.12='fortune  [the wheel of]      ◄───     X'
      @.13='strength                     ◄───    XI'
      @.14='the hanging man              ◄───   XII'
      @.15='death  [often unlabeled]     ◄───  XIII'
      @.16='temperance                   ◄───   XIV'
      @.17='the devil                    ◄───    XV'
      @.18='lightning  [the tower]       ◄───   XVI'
      @.18='the stars                    ◄───  XVII'
      @.20='the moon                     ◄─── XVIII'
      @.21='the sun                      ◄───   XIX'
      @.22='judgment                     ◄───    XX'
      @.23='the world                    ◄───   XXI'
      @.24='the fool  [often unnumbered] ◄───  XXII'

            do #=1  until @.#==''; end;  #= #-1  /*find how many entries in the array.  */
      return                                     /* [↑]  adjust for DO loop advancement.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
show@: w= length(#);              do j=1  for #      /*#:  is the number of items in @. */
                                  say 'element'    right(j, w)     arg(1)":"    @.j
                                  end   /*j*/        /*     ↑                           */
       return                                        /*     └─────max width of any line.*/
```

{{out|output|text=  when using the internal default inputs:}}

(Shown at three-quarter size.)

<pre style="font-size:75%">
element  1 before sort: ---the 22 card tarot deck (larger deck has 56 additional cards in 4 suits)---
element  2 before sort:
### =======symbol====================pip===================================

element  3 before sort: the juggler                  ◄───     I
element  4 before sort: the high priestess  [Popess] ◄───    II
element  5 before sort: the empress                  ◄───   III
element  6 before sort: the emperor                  ◄───    IV
element  7 before sort: the hierophant  [Pope]       ◄───     V
element  8 before sort: the lovers                   ◄───    VI
element  9 before sort: the chariot                  ◄───   VII
element 10 before sort: justice                      ◄───  VIII
element 11 before sort: the hermit                   ◄───    IX
element 12 before sort: fortune  [the wheel of]      ◄───     X
element 13 before sort: strength                     ◄───    XI
element 14 before sort: the hanging man              ◄───   XII
element 15 before sort: death  [often unlabeled]     ◄───  XIII
element 16 before sort: temperance                   ◄───   XIV
element 17 before sort: the devil                    ◄───    XV
element 18 before sort: the stars                    ◄───  XVII
█████████████████████████████████████████████████████████████████████████████████████████████████████
element  1  after sort: ---the 22 card tarot deck (larger deck has 56 additional cards in 4 suits)---
element  2  after sort:
### =======symbol====================pip===================================

element  3  after sort: death  [often unlabeled]     ◄───  XIII
element  4  after sort: fortune  [the wheel of]      ◄───     X
element  5  after sort: justice                      ◄───  VIII
element  6  after sort: strength                     ◄───    XI
element  7  after sort: temperance                   ◄───   XIV
element  8  after sort: the chariot                  ◄───   VII
element  9  after sort: the devil                    ◄───    XV
element 10  after sort: the emperor                  ◄───    IV
element 11  after sort: the empress                  ◄───   III
element 12  after sort: the hanging man              ◄───   XII
element 13  after sort: the hermit                   ◄───    IX
element 14  after sort: the hierophant  [Pope]       ◄───     V
element 15  after sort: the high priestess  [Popess] ◄───    II
element 16  after sort: the juggler                  ◄───     I
element 17  after sort: the lovers                   ◄───    VI
element 18  after sort: the stars                    ◄───  XVII

```


===version handles non-blanks===
This faster REXX version can handle an array that doesn't contain blanks or spaces by using a simpler ''swap'' mechanism.

```rexx
/*──────────────────────────────────────────────────────────────────────────────────────*/
cocktailSort2: procedure expose @.;   parse arg N   /*N:  is the number of items in @.  */
                      do until done;    done= 1     /*array items may not contain blanks*/
                         do j=1  for N-1;   jp= j+1
                         if @.j>@.jp  then parse value  0  @.j @.jp  with  done  @.jp  @.j
                         end   /*j*/
                      if done  then leave           /*No swaps done?  Then we're done.  */
                         do k=N-1  for N-1  by -1;   kp= k+1
                         if @.k>@.kp  then parse value  0  @.k @.kp  with  done  @.kp  @.k
                         end   /*k*/
                      end      /*until*/
               return
```



## Ring


```ring

aList = [ 5, 6, 1, 2, 9, 14, 2, 15, 6, 7, 8, 97]
flag = 0
cocktailSort(aList)
for i=1 to len(aList)
    see "" + aList[i] + " "
next

func cocktailSort A
       n = len(A)
       while flag =  0
             flag = 1
             for i = 1 to n-1
                 if A[i] > A[i+1]
                    temp = A[i]
                    A[i] = A[i+1]
                    A [i+1] = temp
                    flag = 0
                    ok
             next
       end

```



## Ruby


```ruby
class Array
  def cocktailsort!
    begin
      swapped = false
      0.upto(length - 2) do |i|
        if self[i] > self[i + 1]
          self[i], self[i + 1] = self[i + 1], self[i]
          swapped = true
        end
      end
      break unless swapped

      swapped = false
      (length - 2).downto(0) do |i|
        if self[i] > self[i + 1]
          self[i], self[i + 1] = self[i + 1], self[i]
          swapped = true
        end
      end
    end while swapped
    self
  end
end
```


Another way

```ruby
class Array
  def cocktailsort!
    start, finish, way = 0, size-1, 1
    loop do
      swapped = false
      start.step(finish-way, way) do |i|
        if (self[i] <=> self[i + way]) == way
          self[i], self[i + way] = self[i + way], self[i]
          swapped = i
        end
      end
      break unless swapped
      start, finish, way = swapped, start, -way
    end
    self
  end
end
```


Test:

```ruby
ary = [7,6,5,9,8,4,3,1,2,0]
p ary.cocktailsort!
ary = ["John", "Kate", "Zerg", "Alice", "Joe", "Jane"]
p ary.cocktailsort!
```


{{out}}

```txt

[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
["Alice", "Jane", "Joe", "John", "Kate", "Zerg"]

```



## Run BASIC


```runbasic
for i = 1 to 100 ' fill array
  a(i) = rnd(0) * 100
next i
' ------- sort -------
beg	= 2
siz	= 100
whatWay	= 1
changed = 1
while changed
changed = 0
  FOR i = beg TO siz STEP whatWay
    IF a(i-1) > a(i) THEN
       hold	= a(i)
       a(i)	= a(i-1)
       a(i-1)	= hold
       changed	= i
    end if
  NEXT i
  siz	= beg
  beg	= changed - whatWay
  whatWay = 0 - whatWay
wend
' ------ print result --------
for i = 1 to 100
 print i;" ";a(i)
next i
end
```



## Scala


```Scala
object CocktailSort extends App {
  def sort(arr: Array[Int]) = {
    var swapped = false
    do {
      def swap(i: Int) {
        val temp = arr(i)
        arr(i) = arr(i + 1)
        arr(i + 1) = temp
        swapped = true
      }

      swapped = false
      for (i <- 0 to (arr.length - 2)) if (arr(i) > arr(i + 1)) swap(i)

      if (swapped) {
        swapped = false
        for (j <- arr.length - 2 to 0 by -1) if (arr(j) > arr(j + 1)) swap(j)
        //if no elements have been swapped, then the list is sorted
      }
    } while (swapped)
    arr
  }

  println(sort(Array(170, 45, 75, -90, -802, 24, 2, 66)).mkString(", "))

}
```


## Scilab

<lang>function varargout=cocktailSort(list_in)
    swapped = %T;
    while swapped
        swapped = %F;
        for i = [1:length(list_in)-1]
            if list_in(i) > list_in(i+1) then
                list_in([i i+1])=list_in([i+1 i]);
                swapped = %T;
            end
        end
        if ~swapped
            break
        end
        swapped = %F;
        for i = [length(list_in)-1:-1:1]
            if list_in(i) > list_in(i+1)
                list_in([i i+1]) = list_in([i+1 i])
                swapped = %T;
            end
        end
    end
    varargout=list(list_in)
endfunction

disp(cocktailSort([6 3 7 8 5 1 2 4 9]));
```

{{out}}

```txt
   1.   2.   3.   4.   5.   6.   7.   8.   9.
```



## Seed7


```seed7
const proc: cocktailSort (inout array elemType: arr) is func
  local
    var boolean: swapped is FALSE;
    var integer: i is 0;
    var elemType: help is elemType.value;
  begin
    repeat
      swapped := FALSE;
      for i range 1 to length(arr) - 1 do
        if arr[i] > arr[i + 1] then
          help := arr[i];
          arr[i] := arr[i + 1];
          arr[i + 1] := help;
          swapped := TRUE;
        end if;
      end for;
      if swapped then
        swapped := FALSE;
        for i range length(arr) - 1 downto 1 do
          if arr[i] > arr[i + 1] then
            help := arr[i];
            arr[i] := arr[i + 1];
            arr[i + 1] := help;
            swapped := TRUE;
          end if;
        end for;
      end if;
    until not swapped;
  end func;
```


Original source: [http://seed7.sourceforge.net/algorith/sorting.htm#cocktailSort]


## Sidef


```ruby
func cocktailsort(a) {
    var swapped = false
    func cmpsw(i) {
        if (a[i] > a[i+1]) {
            a[i, i+1] = a[i+1, i]
            swapped = true
        }
    }
    var max = a.end
    do {
        {|i| cmpsw(i) } << ^max
        swapped.not! && break
        {|i| cmpsw(max-i) } << 1..max
    } while (swapped)
    return a
}
```

Test:

```ruby
var numbers = [7,6,5,9,8,4,3,1,2,0]
say cocktailsort(numbers)

var strs = ["John", "Kate", "Zerg", "Alice", "Joe", "Jane"]
say cocktailsort(strs)
```

{{out}}

```txt
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
['Alice', 'Jane', 'Joe', 'John', 'Kate', 'Zerg']
```



## Slate


```slate
s@(Sequence traits) cocktailSort
[ |swapped|
  swapped: False.
  s size <= 1 ifTrue: [^ s].
  [{0 to: s size - 2. s size - 2 downTo: 0}
    do: [|:range| range do: [|:index| (s at: index) > (s at: index + 1) ifTrue: [s swap: index with: index + 1. swapped: True]].
         swapped ifFalse: [^ s].
         swapped: False].
  ] loop
].
```

{{out|Example}}

```slate
slate[1]> #( 10 9 8 7 6 0 -5) cocktailSort.
{-5. 0. 6. 7. 8. 9. 10}
```



## Smalltalk

{{works with|GNU Smalltalk}}

```smalltalk
OrderedCollection extend [
  swap: indexA and: indexB [
    |t|
    t := self at: indexA.
    self at: indexA put: (self at: indexB).
    self at: indexB put: t
  ]
  cocktailSort [
    |swapped|
    [
      swapped := false.
      1 to: (self size - 1) do: [ :i |
        (self at: i) > (self at: (i+1)) ifTrue: [
          self swap: i and: (i+1).
	  swapped := true
        ]
      ].
      swapped ifFalse: [ ^ self ].
      swapped := false.
      (self size - 1) to: 1 by: -1 do: [ :i |
        (self at: i) > (self at: (i+1)) ifTrue: [
          self swap: i and: (i+1).
	  swapped := true
        ]
      ].
      swapped
    ] whileTrue: [ ].
    ^ self
  ]
].
```

{{out|Example}}

```smalltalk
(#( 10 9 8 7 6 0 -5) asOrderedCollection cocktailSort) printNl.
```



## Swift



```swift
extension Collection where Element: Comparable {
  public func cocktailSorted() -> [Element] {
    var swapped = false
    var ret = Array(self)

    guard count > 1 else {
      return ret
    }

    repeat {
      for i in 0...ret.count-2 where ret[i] > ret[i + 1] {
        (ret[i], ret[i + 1]) = (ret[i + 1], ret[i])
        swapped = true
      }

      guard swapped else {
        break
      }

      swapped = false

      for i in stride(from: ret.count - 2, through: 0, by: -1) where ret[i] > ret[i + 1] {
        (ret[i], ret[i + 1]) = (ret[i + 1], ret[i])
        swapped = true
      }
    } while swapped

    return ret
  }
}

let arr = (1...10).shuffled()

print("Before: \(arr)")
print("Cocktail sort: \(arr.cocktailSorted())")
```


{{out}}


```txt
Before: [5, 4, 7, 10, 9, 8, 1, 6, 2, 3]
Cocktail sort: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```



## Tcl

{{tcllib|struct::list}}<!-- convenient element swapping only -->

```tcl
package require Tcl 8.5
package require struct::list

proc cocktailsort {A} {
    set len [llength $A]
    set swapped true
    while {$swapped} {
        set swapped false
        for {set i 0} {$i < $len - 1} {incr i} {
            set j [expr {$i + 1}]
            if {[lindex $A $i] > [lindex $A $j]} {
                struct::list swap A $i $j
                set swapped true
            }
        }
        if { ! $swapped} {
            break
        }
        set swapped false
        for {set i [expr {$len - 2}]} {$i >= 0} {incr i -1} {
            set j [expr {$i + 1}]
            if {[lindex $A $i] > [lindex $A $j]} {
                struct::list swap A $i $j
                set swapped true
            }
        }
    }
    return $A
}
```

{{out|Example}}

```tcl
puts [cocktailsort {8 6 4 2 1 3 5 7 9}] ;# => 1 2 3 4 5 6 7 8 9
```



## uBasic/4tH

<lang>PRINT "Cocktail sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Cocktailsort (n)
  PROC _ShowArray (n)
PRINT

END


_Cocktailsort PARAM (1)                ' Cocktail sort
  LOCAL (2)
  b@ = 0

  DO WHILE b@ = 0
    b@ = 1
    FOR c@=1 TO a@-1
      IF @(c@) < @(c@-1) THEN PROC _Swap (c@, c@-1) : b@ = 0
    NEXT
  UNTIL b@
    b@ = 1
    FOR c@=a@-1 TO 1 STEP -1
      IF @(c@) < @(c@-1) THEN PROC _Swap (c@, c@-1) : b@ = 0
    NEXT
  LOOP
RETURN


_Swap PARAM(2)                         ' Swap two array elements
  PUSH @(a@)
  @(a@) = @(b@)
  @(b@) = POP()
RETURN


_InitArray                             ' Init example array
  PUSH 4, 65, 2, -31, 0, 99, 2, 83, 782, 1

  FOR i = 0 TO 9
    @(i) = POP()
  NEXT

RETURN (i)


_ShowArray PARAM (1)                   ' Show array subroutine
  FOR i = 0 TO a@-1
    PRINT @(i),
  NEXT

  PRINT
RETURN
```


## Ursala

The same function is used for the traversal in each direction, in one case parameterized by the given predicate and in the other by its negation. Lists are used rather than arrays. The fold combinator (<code>=></code>) avoids explicit recursion.

```Ursala
#import std

ctsort = ^=+ +^|(==?/~&l+ @r+ ~&x++ @x,^/~&)+ ("p". =><> ~&r?\~&C "p"?lrhPX/~&C ~&rhPlrtPCC)^~/not ~&
```

test program:

```Ursala
#cast %s

test = ctsort(lleq) 'mdfdguldxisgbxjtqkadfkslakwkyioukdledbig'
```

{{out}}

```txt

'aabbddddddeffgggiiijkkkkklllmoqsstuuwxxy'

```



## VBA

{{trans|Phix}}
```vb
Function cocktail_sort(ByVal s As Variant) As Variant
    Dim swapped As Boolean
    Dim f As Integer, t As Integer, d As Integer, tmp As Integer
    swapped = True
    f = 1
    t = UBound(s) - 1
    d = 1
    Do While swapped
        swapped = 0
        For i = f To t Step d
            If Val(s(i)) > Val(s(i + 1)) Then
                tmp = s(i)
                s(i) = s(i + 1)
                s(i + 1) = tmp
                swapped = True
            End If
        Next i
        '-- swap to and from, and flip direction.
        '-- additionally, we can reduce one element to be
        '-- examined, depending on which way we just went.
        tmp = f
        f = t + (d = 1)
        t = tmp + (d = -1)
        d = -d
    Loop
    cocktail_sort = s
End Function

Public Sub main()
    Dim s(9) As Variant
    For i = 0 To 9
        s(i) = CStr(Int(1000 * Rnd))
    Next i
    Debug.Print Join(s, ", ")
    Debug.Print Join(cocktail_sort(s), ", ")
End Sub
```
{{out}}

```txt
45, 414, 862, 790, 373, 961, 871, 56, 949, 364
45, 56, 364, 373, 414, 790, 862, 871, 949, 961
```


## VBScript

A few of the streets nearby.

;Implementation

```vb
function cocktailSort( a )
	dim swapped
	dim i
	do
		swapped = false
		for i = 0 to ubound( a )  - 1
			if a(i) > a(i+1) then
				swap a(i), a(i+1)
				swapped = true
			end if
		next
		if not swapped then exit do
		swapped = false
		for i = ubound( a ) - 1 to 0 step -1
			if a(i) > a( i+1) then
				swap a(i), a(i+1)
				swapped = true
			end if
		next
		if not swapped then exit do
	loop
	cocktailSort = a
end function

sub swap( byref a, byref b)
	dim tmp
	tmp = a
	a = b
	b = tmp
end sub
```

;Invocation

```vb
dim a
a = array( "portcullis", "redoubt", "palissade", "turret", "collins", "the parapet", "the quarterdeck")
wscript.echo join( a, ", ")

dim b
b = cocktailSort( a )
wscript.echo join( b, ", ")
```

{{out}}

```txt

portcullis, redoubt, palissade, turret, collins, the parapet, the quarterdeck
collins, palissade, portcullis, redoubt, the parapet, the quarterdeck, turret

```



## XPL0


```XPL0
code ChOut=8, IntOut=11;

proc CocktailSort(A, L);        \Sort array A of length L
int  A, L;
int  Swapped, I, T;
[loop   [Swapped:= false;
        for I:= 0 to L-2 do
            if A(I) > A(I+1) then       \test if elements are in wrong order
                [T:= A(I);  A(I):= A(I+1);  A(I+1):= T; \elements change places
                Swapped:= true;
                ];
        if Swapped = false then quit;   \exit outer loop if no swaps occurred
        Swapped:= false;
        for I:= L-2 downto 0 do
            if A(I) > A(I+1) then
                [T:= A(I);  A(I):= A(I+1);  A(I+1):= T;
                Swapped:= true;
                ];
        \if no elements have been swapped then the list is sorted
        if not Swapped then quit;
        ];
];

int A, I;
[A:= [3, 1, 4, 1, -5, 9, 2, 6, 5, 4];
CocktailSort(A, 10);
for I:= 0 to 10-1 do [IntOut(0, A(I));  ChOut(0, ^ )];
]
```


{{out}}

```txt

-5 1 1 2 3 4 4 5 6 9

```



## zkl

This has the Wikipedia optimizations.

```zkl
fcn cocktailSort(a){
   swapped,begin,end:=False,-1,a.len() - 2;
   do{
      swapped,begin=False,begin + 1;
      foreach i in ([begin .. end]){
	 if(a[i]>a[i+1]){ a.swap(i,i+1); swapped=True; }
      }
      if(not swapped) break;
      swapped,end=False,end - 1;
      foreach i in ([end..begin,-1]){
         if(a[i]>a[i+1]){ a.swap(i,i+1); swapped=True; }
      }
   }while(swapped);
   a
}
```


```zkl
cocktailSort(List(2,1)).println();
x:=List(5, -1, 101, -4, 0, 1, 8, 6, 2, 3 );
cocktailSort(x).println();
x="the lazy fox jumped over the brown dog".split(" ").copy();
cocktailSort(x).println();
```

{{out}}

```txt

L(1,2)
L(-4,-1,0,1,2,3,5,6,8,101)
L("brown","dog","fox","jumped","lazy","over","the","the"

```



## ZX Spectrum Basic

its a "cocktail" bubble sort, but the writer called it 'zigzag' since the name was unknown

```zxbasic
5000 CLS
5002 LET a$="": FOR f=1 TO 64: LET a$=a$+CHR$ (32+INT (RND*96)): NEXT f
5004 PRINT a$; AT 10,0;"ZigZag BubbleSORT"
5010 LET la=LEN a$
5011 LET i=1: LET u=0
5020 LET d=0: LET p=(u=0)-(u=1)
5021 LET l=(i AND u=0)+(la-i+u AND u=1)
5030 IF u=0 THEN  IF a$(l+1)>=a$(l) THEN  GO TO 5050
5031 IF u=1 THEN  IF a$(l-1)<=a$(l) THEN  GO TO 5050
5040 LET d=1
5042 LET t$=a$(l+p)
5043 LET a$(l+p)=a$(l)
5044 LET a$(l)=t$
5050 LET l=l+p
5051 PRINT AT 10,21;a$(l);AT 12,0;a$
5055 IF l<=la-i AND l>=i THEN  GO TO 5023
5061 LET i=i+NOT u
5063 LET u=NOT u
5064 IF d AND i<la THEN  GO TO 5020
5072 PRINT AT 12,0;a$
9000 STOP
```

Next is an optimisation by using the margin value's as swop comparative aswell
so its swops inside and at the edges from the file

its a " Sticky (edge) Cocktail Sort"
By C. Born (freeware)

```zxbasic
5000 CLS : PRINT ;"Jumping  Zig  Zag  BubbleSORT"'"aka Sticky Cocktail Sort"
5002 LET a$="": FOR f=1 TO 96: LET a$=a$+CHR$ (48+INT (RND*48)): NEXT f
5004 PRINT 'a$
5010 LET a=LEN a$: LET i=1: LET u=0: LET up=0: LET do=0

5020 LET d=0: LET p=(NOT u)-u: LET l=(i AND NOT u)+(a AND u)

5030 IF NOT u THEN  IF a$(l+1)>=a$(l) THEN  GO TO 5060
5031 IF u THEN  IF a$(l-1)<=a$(l) THEN  GO TO 5060
5040 LET w=l+p: GO SUB 5400

5060 IF up THEN  IF a<LEN a$ THEN  IF a$(l)=a$(a+1) THEN  LET w=a: GO SUB 5400: GO SUB 5500
5061 IF do THEN  IF i>1 THEN  IF a$(l)=a$(i-1) THEN  LET w=i: GO SUB 5400: GO SUB 5500

5100 LET l=l+p
5150 PRINT AT 10,0;a$(l);AT 12,0;a$
5151 PRINT AT 21,0;i;a$(i),a;a$(a)
5155 IF NOT u THEN  IF l<a THEN  GO TO 5030
5156 IF u THEN  IF l>i THEN  GO TO 5030

5170 LET do=up=1: LET up=1
5261 LET i=i+u: LET a=a-NOT u: LET u=NOT u
5264 IF d AND i<a THEN  GO TO 5020

5272 PRINT AT 12,0;a$
5399 STOP

5400 LET d=1: LET t$=a$(w): LET a$(w)=a$(l): LET a$(l)=t$: RETURN

5500 IF a+1<=LEN a$ THEN  IF a$(a)=a$(a+1) THEN  LET a=a-1: GO TO 5500
5510 IF i-1>=1 THEN  IF a$(i)=a$(i-1) THEN  LET i=i+1: GO TO 5500
5520 RETURN
9999 CLEAR : SAVE "JZZB" LINE 0
```


{{omit from|GUISS}}
