+++
title = "Sorting algorithms/Quicksort"
description = ""
date = 2019-10-18T20:14:26Z
aliases = []
[extra]
id = 2147
[taxonomies]
categories = []
tags = []
+++

{{task|Sorting Algorithms}}
{{Sorting Algorithm}}
[[Category:Recursion]]
{{Wikipedia|Quicksort}} 


;Task:
Sort an array (or list) elements using the   [https://en.wikipedia.org/wiki/Quicksort ''quicksort'']   algorithm. 

The elements must have a   [https://en.wikipedia.org/wiki/Weak_ordering strict weak order]   and the index of the array can be of any discrete type. 

For languages where this is not possible, sort an array of integers.


Quicksort, also known as   ''partition-exchange sort'',   uses these steps.

::#   Choose any element of the array to be the pivot.
::#   Divide all other elements (except the pivot) into two partitions.
::#*   All elements less than the pivot must be in the first partition.
::#*   All elements greater than the pivot must be in the second partition.
::#   Use recursion to sort both partitions.
::#   Join the first sorted partition, the pivot, and the second sorted partition.



The best pivot creates partitions of equal length (or lengths differing by   '''1'''). 

The worst pivot creates an empty partition (for example, if the pivot is the first or last element of a sorted array). 

The run-time of Quicksort ranges from   <big> ''[[O]](n ''log'' n)'' </big>   with the best pivots, to   <big>  ''[[O]](n<sup>2</sup>)'' </big>   with the worst pivots, where   <big> ''n'' </big>   is the number of elements in the array.


This is a simple quicksort algorithm, adapted from Wikipedia.

 '''function''' ''quicksort''(array)
     less, equal, greater ''':=''' three empty arrays
     '''if''' length(array) > 1  
         pivot ''':=''' ''select any element of'' array
         '''for each''' x '''in''' array
             '''if''' x < pivot '''then add''' x '''to''' less
             '''if''' x = pivot '''then add''' x '''to''' equal
             '''if''' x > pivot '''then add''' x '''to''' greater
         quicksort(less)
         quicksort(greater)
         array ''':=''' concatenate(less, equal, greater)

A better quicksort algorithm works in place, by swapping elements within the array, to avoid the memory allocation of more arrays.

 '''function''' ''quicksort''(array)
     '''if''' length(array) > 1
         pivot ''':=''' ''select any element of'' array
         left ''':= first index of''' array
         right ''':=''' '''last index of''' array
         '''while''' left ≤ right
             '''while''' array[left] < pivot
                 left := left + 1
             '''while''' array[right] > pivot
                 right := right - 1
             '''if''' left ≤ right
                 '''swap''' array[left] '''with''' array[right]
                 left := left + 1
                 right := right - 1
         quicksort(array '''from first index to''' right)
         quicksort(array '''from''' left '''to last index''')

Quicksort has a reputation as the fastest sort. Optimized variants of quicksort are common features of many languages and libraries. One often contrasts quicksort with   [[../Merge sort|merge sort]],   because both sorts have an average time of   <big> ''[[O]](n ''log'' n)''. </big>

: ''"On average, mergesort does fewer comparisons than quicksort, so it may be better when complicated comparison routines are used. Mergesort also takes advantage of pre-existing order, so it would be favored for using sort() to merge several sorted arrays. On the other hand, quicksort is often faster for small arrays, and on arrays of a few distinct values, repeated many times."'' — http://perldoc.perl.org/sort.html

Quicksort is at one end of the spectrum of divide-and-conquer algorithms, with merge sort at the opposite end.

* Quicksort is a conquer-then-divide algorithm, which does most of the work during the partitioning and the recursive calls. The subsequent reassembly of the sorted partitions involves trivial effort.
* Merge sort is a divide-then-conquer algorithm. The partioning happens in a trivial way, by splitting the input array in half. Most of the work happens during the recursive calls and the merge phase.



With quicksort, every element in the first partition is less than or equal to every element in the second partition. Therefore, the merge phase of quicksort is so trivial that it needs no mention!

This task has not specified whether to allocate new arrays, or sort in place. This task also has not specified how to choose the pivot element. (Common ways to are to choose the first element, the middle element, or the median of three elements.) Thus there is a variety among the following implementations.





## 360 Assembly

{{trans|REXX}}
Structured version with ASM & ASSIST macros.

```360asm
*        Quicksort                 14/09/2015 & 23/06/2016
QUICKSOR CSECT
         USING  QUICKSOR,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         "
         ST     R15,8(R13)         " 
         LR     R13,R15            "
         MVC    A,=A(1)            a(1)=1
         MVC    B,=A(NN)           b(1)=hbound(t)
         L      R6,=F'1'           k=1
       DO WHILE=(LTR,R6,NZ,R6)   do while k<>0    
### ============

         LR     R1,R6              k 
         SLA    R1,2               ~
         L      R10,A-4(R1)        l=a(k)
         LR     R1,R6              k
         SLA    R1,2               ~
         L      R11,B-4(R1)        m=b(k)
         BCTR   R6,0               k=k-1
         LR     R4,R11             m
         C      R4,=F'2'           if m<2 
         BL     ITERATE            then iterate
         LR     R2,R10             l
         AR     R2,R11             +m
         BCTR   R2,0               -1
         ST     R2,X               x=l+m-1
         LR     R2,R11             m
         SRA    R2,1               m/2
         AR     R2,R10             +l
         ST     R2,Y               y=l+m/2
         L      R1,X               x
         SLA    R1,2               ~
         L      R4,T-4(R1)         r4=t(x)
         L      R1,Y               y
         SLA    R1,2               ~
         L      R5,T-4(R1)         r5=t(y)
         LR     R1,R10             l
         SLA    R1,2               ~
         L      R3,T-4(R1)         r3=t(l)
         IF     CR,R4,LT,R3        if t(x)<t(l)       ---+
         IF     CR,R5,LT,R4          if t(y)<t(x)        |
         LR     R7,R4                  p=t(x)            |
         L      R1,X                   x                 |
         SLA    R1,2                   ~                 |
         ST     R3,T-4(R1)             t(x)=t(l)         |
         ELSEIF CR,R5,GT,R3          elseif t(y)>t(l)    |
         LR     R7,R3                  p=t(l)            |
         ELSE   ,                    else                |
         LR     R7,R5                  p=t(y)            |
         L      R1,Y                   y                 |
         SLA    R1,2                   ~                 |
         ST     R3,T-4(R1)            t(y)=t(l)          |
         ENDIF  ,                    end if              |
         ELSE   ,                  else                  |
         IF     CR,R5,LT,R3          if t(y)<t(l)        |
         LR     R7,R3                  p=t(l)            |
         ELSEIF CR,R5,GT,R4          elseif t(y)>t(x)    |
         LR     R7,R4                  p=t(x)            |
         L      R1,X                   x                 |
         SLA    R1,2                   ~                 |
         ST     R3,T-4(R1)             t(x)=t(l)         |
         ELSE   ,                    else                |
         LR     R7,R5                  p=t(y)            |
         L      R1,Y                   y                 |
         SLA    R1,2                   ~                 |
         ST     R3,T-4(R1)             t(y)=t(l)         |
         ENDIF  ,                    end if              |
         ENDIF  ,                  end if             ---+
         LA     R8,1(R10)          i=l+1
         L      R9,X               j=x
FOREVER  EQU    *                  do forever  --------------------+  
         LR     R1,R8                i                             |
         SLA    R1,2                 ~                             |
         LA     R2,T-4(R1)           @t(i)                         |
         L      R0,0(R2)             t(i)                          |
         DO WHILE=(CR,R8,LE,R9,AND,  while i<=j and   ---+         |   X
               CR,R0,LE,R7)                t(i)<=p       |         |
         AH     R8,=H'1'               i=i+1             |         |
         AH     R2,=H'4'               @t(i)             |         |
         L      R0,0(R2)               t(i)              |         |
         ENDDO  ,                    end while        ---+         |
         LR     R1,R9                j                             |
         SLA    R1,2                 ~                             |
         LA     R2,T-4(R1)           @t(j)                         |
         L      R0,0(R2)             t(j)                          |
         DO WHILE=(CR,R8,LT,R9,AND,  while i<j and    ---+         |   X
               CR,R0,GE,R7)                t(j)>=p       |         |
         SH     R9,=H'1'               j=j-1             |         |
         SH     R2,=H'4'               @t(j)             |         |
         L      R0,0(R2)               t(j)              |         |
         ENDDO  ,                    end while        ---+         |
         CR     R8,R9                if i>=j                       |
         BNL    LEAVE                then leave (segment finished) |
         LR     R1,R8                i                             |
         SLA    R1,2                 ~                             |
         LA     R2,T-4(R1)           @t(i)                         |
         LR     R1,R9                j                             |
         SLA    R1,2                 ~                             |
         LA     R3,T-4(R1)           @t(j)                         |
         L      R0,0(R2)             w=t(i)       +                |
         MVC    0(4,R2),0(R3)        t(i)=t(j)    |swap t(i),t(j)  |
         ST     R0,0(R3)             t(j)=w       +                |
         B      FOREVER            end do forever  ----------------+
LEAVE    EQU    *
         LR     R9,R8              j=i
         BCTR   R9,0               j=i-1
         LR     R1,R9              j
         SLA    R1,2               ~
         LA     R3,T-4(R1)         @t(j)
         L      R2,0(R3)           t(j)
         LR     R1,R10             l
         SLA    R1,2               ~
         ST     R2,T-4(R1)         t(l)=t(j)
         ST     R7,0(R3)           t(j)=p
         LA     R6,1(R6)           k=k+1
         LR     R1,R6              k
         SLA    R1,2               ~
         LA     R4,A-4(R1)         r4=@a(k)
         LA     R5,B-4(R1)         r5=@b(k)
         IF     C,R8,LE,Y          if i<=y           ----+
         ST     R8,0(R4)             a(k)=i              |
         L      R2,X                 x                   |
         SR     R2,R8                -i                  |
         LA     R2,1(R2)             +1                  |
         ST     R2,0(R5)             b(k)=x-i+1          |
         LA     R6,1(R6)             k=k+1               |
         ST     R10,4(R4)            a(k)=l              |
         LR     R2,R9                j                   |
         SR     R2,R10               -l                  |
         ST     R2,4(R5)             b(k)=j-l            |
         ELSE   ,                  else                  |
         ST     R10,4(R4)            a(k)=l              |
         LR     R2,R9                j                   |
         SR     R2,R10               -l                  |
         ST     R2,0(R5)             b(k)=j-l            |
         LA     R6,1(R6)             k=k+1               |
         ST     R8,4(R4)             a(k)=i              |
         L      R2,X                 x                   |
         SR     R2,R8                -i                  |
         LA     R2,1(R2)             +1                  |
         ST     R2,4(R5)             b(k)=x-i+1          |
         ENDIF  ,                  end if            ----+
ITERATE  EQU    *                  
       ENDDO    ,                  end while  
### ===============

*        ***    *********          print sorted table
         LA     R3,PG              ibuffer
         LA     R4,T               @t(i)
       DO WHILE=(C,R4,LE,=A(TEND)) do i=1 to hbound(t)
         L      R2,0(R4)             t(i)
         XDECO  R2,XD                edit t(i)
         MVC    0(4,R3),XD+8         put in buffer
         LA     R3,4(R3)             ibuffer=ibuffer+1
         LA     R4,4(R4)             i=i+1
       ENDDO    ,                  end do
         XPRNT  PG,80              print buffer
         L      R13,4(0,R13)       epilog 
         LM     R14,R12,12(R13)    "
         XR     R15,R15            "
         BR     R14                exit
T        DC     F'10',F'9',F'9',F'6',F'7',F'16',F'1',F'16',F'17',F'15'
         DC     F'1',F'9',F'18',F'16',F'8',F'20',F'18',F'2',F'19',F'8'
TEND     DS     0F
NN       EQU    (TEND-T)/4)
A        DS     (NN)F              same size as T
B        DS     (NN)F              same size as T
X        DS     F
Y        DS     F
PG       DS     CL80
XD       DS     CL12
         YREGS 
         END    QUICKSOR
```

{{out}}

```txt

   1   1   2   6   7   8   8   9   9   9  10  15  16  16  16  17  18  18  19  20

```



## ABAP

This works for ABAP Version 7.40 and above

```ABAP

report z_quicksort.

data(numbers) = value int4_table( ( 4 ) ( 65 ) ( 2 ) ( -31 ) ( 0 ) ( 99 ) ( 2 ) ( 83 ) ( 782 ) ( 1 ) ).
perform quicksort changing numbers.

write `[`.
loop at numbers assigning field-symbol(<numbers>).
  write <numbers>.
endloop.
write `]`.

form quicksort changing numbers type int4_table.
  data(less) = value int4_table( ).
  data(equal) = value int4_table( ).
  data(greater) = value int4_table( ).

  if lines( numbers ) > 1.
    data(pivot) = numbers[ lines( numbers ) / 2 ].

    loop at numbers assigning field-symbol(<number>).
      if <number> < pivot.
        append <number> to less.
      elseif <number> = pivot.
        append <number> to equal.
      elseif <number> > pivot.
        append <number> to greater.
      endif.
    endloop.

    perform quicksort changing less.
    perform quicksort changing greater.

    clear numbers.
    append lines of less to numbers.
    append lines of equal to numbers.
    append lines of greater to numbers.
  endif.
endform.

```


{{out}}


```txt

[        31-         0          1          2          2          4         65         83         99        782  ]

```



## ACL2



```Lisp
(defun partition (p xs)
   (if (endp xs)
       (mv nil nil)
       (mv-let (less more)
               (partition p (rest xs))
          (if (< (first xs) p)
              (mv (cons (first xs) less) more)
              (mv less (cons (first xs) more))))))

(defun qsort (xs)
   (if (endp xs)
       nil
       (mv-let (less more)
               (partition (first xs) (rest xs))
          (append (qsort less)
                  (list (first xs))
                  (qsort more)))))
```


Usage:
<lang>> (qsort '(8 6 7 5 3 0 9))
(0 3 5 6 7 8 9)
```



## ActionScript

{{works with|ActionScript|3}}

The functional programming way

```actionscript
function quickSort (array:Array):Array
{
    if (array.length <= 1)
        return array;

    var pivot:Number = array[Math.round(array.length / 2)];

    return quickSort(array.filter(function (x:Number, index:int, array:Array):Boolean { return x <  pivot; })).concat(
            array.filter(function (x:Number, index:int, array:Array):Boolean { return x == pivot; })).concat(
        quickSort(array.filter(function (x:Number, index:int, array:Array):Boolean { return x > pivot; })));
}
```


The faster way

```actionscript
function quickSort (array:Array):Array
{
    if (array.length <= 1)
        return array;

    var pivot:Number = array[Math.round(array.length / 2)];

    var less:Array = [];
    var equal:Array = [];
    var greater:Array = [];

    for each (var x:Number in array) {
        if (x < pivot)
            less.push(x);
        if (x == pivot)
            equal.push(x);
        if (x > pivot)
            greater.push(x);
    }

    return quickSort(less).concat(
            equal).concat(
            quickSort(greater));
}
```



## Ada

This example is implemented as a generic procedure.

The procedure specification is:

```ada
-----------------------------------------------------------------------
-- Generic Quick_Sort procedure
-----------------------------------------------------------------------
generic
   type Element is private;
   type Index is (<>);
   type Element_Array is array(Index range <>) of Element;
   with function "<" (Left, Right : Element) return Boolean is <>;
procedure Quick_Sort(A : in out Element_Array);
```

The procedure body deals with any discrete index type, either an integer type or an enumerated type.

```ada
-----------------------------------------------------------------------
-- Generic Quick_Sort procedure
----------------------------------------------------------------------- 

procedure Quick_Sort (A : in out Element_Array) is
   
   procedure Swap(Left, Right : Index) is
      Temp : Element := A (Left);
   begin
      A (Left) := A (Right);
      A (Right) := Temp;
   end Swap;
  
begin
   if A'Length > 1 then
   declare
      Pivot_Value : Element := A (A'First);
      Right       : Index := A'Last;
      Left        : Index := A'First;
   begin
       loop
          while Left < Right and not (Pivot_Value < A (Left)) loop
             Left := Index'Succ (Left);
          end loop;
          while Pivot_Value < A (Right) loop
             Right := Index'Pred (Right);
          end loop;
          exit when Right <= Left;
          Swap (Left, Right);
          Left := Index'Succ (Left);
          Right := Index'Pred (Right);
       end loop;
       if Right = A'Last then
          Right := Index'Pred (Right);
          Swap (A'First, A'Last);
       end if;
       if Left = A'First then
          Left := Index'Succ (Left);
       end if;
       Quick_Sort (A (A'First .. Right));
       Quick_Sort (A (Left .. A'Last));
   end;
   end if;
end Quick_Sort;
```

An example of how this procedure may be used is:

```ada

with Ada.Text_Io;
with Ada.Float_Text_IO; use Ada.Float_Text_IO; 

procedure Sort_Test is
   type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   type Sales is array (Days range <>) of Float;
   procedure Sort_Days is new Quick_Sort(Float, Days, Sales);
   
   procedure Print (Item : Sales) is
   begin
      for I in Item'range loop
         Put(Item => Item(I), Fore => 5, Aft => 2, Exp => 0);
      end loop;
   end Print;
  
   Weekly_Sales : Sales := (Mon => 300.0, 
      Tue => 700.0, 
      Wed => 800.0, 
      Thu => 500.0, 
      Fri => 200.0, 
      Sat => 100.0, 
      Sun => 900.0);
  
begin
  
   Print(Weekly_Sales);
   Ada.Text_Io.New_Line(2);
   Sort_Days(Weekly_Sales);
   Print(Weekly_Sales);
  
end Sort_Test;
```



## ALGOL 68


```algol68
#--- Swap function ---#
PROC swap = (REF []INT array, INT first, INT second) VOID:
(
    INT temp := array[first];
    array[first] := array[second];
    array[second]:= temp
);

#--- Quick sort 3 arg function ---#
PROC quick = (REF [] INT array, INT first, INT last) VOID:
(
    INT smaller := first + 1,  
        larger  := last,
        pivot   := array[first];
  
    WHILE smaller <= larger DO
        WHILE array[smaller] < pivot   AND   smaller < last DO   
            smaller +:= 1        
        OD;
        WHILE array[larger]  > pivot   AND   larger > first DO   
            larger  -:= 1       
        OD; 
        IF smaller < larger THEN 
            swap(array, smaller, larger); 
            smaller +:= 1;
            larger  -:= 1
        ELSE
            smaller +:= 1
        FI
    OD;
    
    swap(array, first, larger);    

    IF first < larger-1 THEN
        quick(array, first, larger-1)  
    FI;
    IF last > larger +1 THEN
        quick(array, larger+1, last)   
    FI
);

#--- Quick sort 1 arg function ---#
PROC quicksort = (REF []INT array) VOID:
(
  IF UPB array > 1 THEN
    quick(array, 1, UPB array) 
  FI
);

#***************************************************************#
main:
(
    [10]INT a; 
    FOR i FROM 1 TO UPB a DO 
        a[i] := ROUND(random*1000)
    OD;                             

    print(("Before:", a));
    quicksort(a);
    print((newline, newline));
    print(("After: ", a))
)

```

{{out}}

```txt

Before:        +73       +921       +179       +961        +50       +324        +82       +178       +243       +458
                                                                                                                     
After:         +50        +73        +82       +178       +179       +243       +324       +458       +921       +961

```



## ALGOL W


```algolw
% Quicksorts in-place the array of integers v, from lb to ub %
procedure quicksort ( integer array v( * )
                    ; integer value lb, ub
                    ) ;
if ub > lb then begin
    % more than one element, so must sort %
    integer left, right, pivot;
    left   := lb;
    right  := ub;
    % choosing the middle element of the array as the pivot %
    pivot  := v( left + ( ( right + 1 ) - left ) div 2 );
    while begin
        while left  <= ub and v( left  ) < pivot do left  := left  + 1;
        while right >= lb and v( right ) > pivot do right := right - 1;
        left <= right
    end do begin
        integer swap;
        swap       := v( left  );
        v( left  ) := v( right );
        v( right ) := swap;
        left       := left  + 1;
        right      := right - 1
    end while_left_le_right ;
    quicksort( v, lb,   right );
    quicksort( v, left, ub    )
end quicksort ;
```


== {{header|APL}} ==
{{works with|Dyalog APL}}{{trans|J}}

```apl
      qsort ← {1≥⍴⍵:⍵ ⋄ e←⍵[?⍴⍵] ⋄ (∇(⍵<e)/⍵) , ((⍵=e)/⍵) , (∇(⍵>e)/⍵)}
      qsort 31 4 1 5 9 2 6 5 3 5 8
1 2 3 4 5 5 5 6 8 9 31
```


Of course, in real APL applications, one would use ⍋ to sort (which will pick a sorting algorithm suited to the argument).



## AppleScript


Emphasising clarity and simplicity more than run-time performance. (Practical scripts will often delegate sorting to the OS X shell, or, since OS X Yosemite, to Foundation classes through the ObjC interface).

{{trans|JavaScript}}
(Functional ES5 version)


```AppleScript
-- quickSort :: (Ord a) => [a] -> [a]
on quickSort(xs)
    if length of xs > 1 then
        set {h, t} to uncons(xs)
        
        -- lessOrEqual :: a -> Bool
        script lessOrEqual
            on |λ|(x)
                x ≤ h
            end |λ|
        end script
        
        set {less, more} to partition(lessOrEqual, t)
        
        quickSort(less) & h & quickSort(more)
    else
        xs
    end if
end quickSort


-- TEST -----------------------------------------------------------------------
on run
    
    quickSort([11.8, 14.1, 21.3, 8.5, 16.7, 5.7])
    
    --> {5.7, 8.5, 11.8, 14.1, 16.7, 21.3}
    
end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- partition :: predicate -> List -> (Matches, nonMatches)
-- partition :: (a -> Bool) -> [a] -> ([a], [a])
on partition(f, xs)
    tell mReturn(f)
        set lst to {{}, {}}
        repeat with x in xs
            set v to contents of x
            set end of item ((|λ|(v) as integer) + 1) of lst to v
        end repeat
        return {item 2 of lst, item 1 of lst}
    end tell
end partition

-- uncons :: [a] -> Maybe (a, [a])
on uncons(xs)
    if length of xs > 0 then
        {item 1 of xs, rest of xs}
    else
        missing value
    end if
end uncons

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
```

{{Out}}

```AppleScript
{5.7, 8.5, 11.8, 14.1, 16.7, 21.3}
```


## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly

/* ARM assembly Raspberry PI  */
/*  program quickSort.s   */
/* look pseudo code in wikipedia  quicksort */

/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall
/*********************************/
/* Initialized data              */
/*********************************/
.data
szMessSortOk:       .asciz "Table sorted.\n"
szMessSortNok:      .asciz "Table not sorted !!!!!.\n"
sMessResult:        .ascii "Value  : "
sMessValeur:        .fill 11, 1, ' '            @ size => 11
szCarriageReturn:   .asciz "\n"
 
.align 4
iGraine:  .int 123456
.equ NBELEMENTS,      10
#TableNumber:	     .int   9,5,6,1,2,3,10,8,4,7
#TableNumber:	     .int   1,3,5,2,4,6,10,8,4,7
#TableNumber:	     .int   1,3,5,2,4,6,10,8,4,7
#TableNumber:	     .int   1,2,3,4,5,6,10,8,4,7
TableNumber:	     .int   10,9,8,7,6,5,4,3,2,1
#TableNumber:	     .int   13,12,11,10,9,8,7,6,5,4,3,2,1
/*********************************/
/* UnInitialized data            */
/*********************************/
.bss  
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main 
main:                                              @ entry of program 
 
1:
    ldr r0,iAdrTableNumber                         @ address number table

    mov r1,#0                                      @ indice first item
    mov r2,#NBELEMENTS                             @ number of élements 
    bl triRapide                                   @ call quicksort
    ldr r0,iAdrTableNumber                         @ address number table
    bl displayTable
 
    ldr r0,iAdrTableNumber                         @ address number table
    mov r1,#NBELEMENTS                             @ number of élements 
    bl isSorted                                    @ control sort
    cmp r0,#1                                      @ sorted ?
    beq 2f                                    
    ldr r0,iAdrszMessSortNok                       @ no !! error sort
    bl affichageMess
    b 100f
2:                                                 @ yes
    ldr r0,iAdrszMessSortOk
    bl affichageMess
100:                                               @ standard end of the program 
    mov r0, #0                                     @ return code
    mov r7, #EXIT                                  @ request to exit program
    svc #0                                         @ perform the system call
 
iAdrsMessValeur:          .int sMessValeur
iAdrszCarriageReturn:    .int szCarriageReturn
iAdrsMessResult:          .int sMessResult
iAdrTableNumber:          .int TableNumber
iAdrszMessSortOk:         .int szMessSortOk
iAdrszMessSortNok:        .int szMessSortNok
/******************************************************************/
/*     control sorted table                                   */ 
/******************************************************************/
/* r0 contains the address of table */
/* r1 contains the number of elements  > 0  */
/* r0 return 0  if not sorted   1  if sorted */
isSorted:
    push {r2-r4,lr}                                    @ save registers
    mov r2,#0
    ldr r4,[r0,r2,lsl #2]
1:
    add r2,#1
    cmp r2,r1
    movge r0,#1
    bge 100f
    ldr r3,[r0,r2, lsl #2]
    cmp r3,r4
    movlt r0,#0
    blt 100f
    mov r4,r3
    b 1b
100:
    pop {r2-r4,lr}
    bx lr                                              @ return 


/***************************************************/
/*   Appel récursif Tri Rapide quicksort           */
/***************************************************/
/* r0 contains the address of table */
/* r1 contains index of first item  */
/* r2 contains the number of elements  > 0  */
triRapide:
    push {r2-r5,lr}                                   @ save registers
    sub r2,#1                                         @ last item index
    cmp r1,r2                                         @ first > last ? 
    bge 100f                                          @ yes -> end
    mov r4,r0                                         @ save r0
    mov r5,r2                                         @ save r2
    bl partition1                                     @ cutting into 2 parts
    mov r2,r0                                         @ index partition
    mov r0,r4                                         @ table address
    bl triRapide                                      @ sort lower part
    add r1,r2,#1                                      @ index begin = index partition + 1
    add r2,r5,#1                                      @ number of elements
    bl triRapide                                      @ sort higter part
   
 100:                                                 @ end function
    pop {r2-r5,lr}                                    @ restaur  registers 
    bx lr                                             @ return


/******************************************************************/
/*      Partition table elements                                */ 
/******************************************************************/
/* r0 contains the address of table */
/* r1 contains index of first item  */
/* r2 contains index of last item   */

partition1:
    push {r1-r7,lr}                                    @ save registers
    ldr r3,[r0,r2,lsl #2]                              @ load value last index
    mov r4,r1                                          @ init with first index
    mov r5,r1                                          @ init with first index
1:                                                     @ begin loop
    ldr r6,[r0,r5,lsl #2]                              @ load value
    cmp r6,r3                                          @ compare value
    ldrlt r7,[r0,r4,lsl #2]                            @ if < swap value table
    strlt r6,[r0,r4,lsl #2]
    strlt r7,[r0,r5,lsl #2]
    addlt r4,#1                                        @ and increment index 1
    add    r5,#1                                       @ increment index 2
    cmp r5,r2                                          @ end ?
    blt 1b                                             @ no loop
    ldr r7,[r0,r4,lsl #2]                              @ swap value
    str r3,[r0,r4,lsl #2]
    str r7,[r0,r2,lsl #2]
    mov r0,r4                                          @ return index partition
100:
    pop {r1-r7,lr}
    bx lr

/******************************************************************/
/*      Display table elements                                */ 
/******************************************************************/
/* r0 contains the address of table */
displayTable:
    push {r0-r3,lr}                                    @ save registers
    mov r2,r0                                          @ table address
    mov r3,#0
1:                                                     @ loop display table
    ldr r0,[r2,r3,lsl #2]
    ldr r1,iAdrsMessValeur                             @ display value
    bl conversion10                                    @ call function
    ldr r0,iAdrsMessResult
    bl affichageMess                                   @ display message
    add r3,#1
    cmp r3,#NBELEMENTS - 1
    ble 1b
    ldr r0,iAdrszCarriageReturn
    bl affichageMess
100:
    pop {r0-r3,lr}
    bx lr
/******************************************************************/
/*     display text with size calculation                         */ 
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                          @ save  registres
    mov r2,#0                                      @ counter length 
1:                                                 @ loop length calculation 
    ldrb r1,[r0,r2]                                @ read octet start position + index 
    cmp r1,#0                                      @ if 0 its over 
    addne r2,r2,#1                                 @ else add 1 in the length 
    bne 1b                                         @ and loop 
                                                   @ so here r2 contains the length of the message 
    mov r1,r0                                      @ address message in r1 
    mov r0,#STDOUT                                 @ code to write to the standard output Linux 
    mov r7, #WRITE                                 @ code call system "write" 
    svc #0                                         @ call systeme 
    pop {r0,r1,r2,r7,lr}                           @ restaur des  2 registres */ 
    bx lr                                          @ return  
/******************************************************************/
/*     Converting a register to a decimal unsigned                */ 
/******************************************************************/
/* r0 contains value and r1 address area   */
/* r0 return size of result (no zero final in area) */
/* area size => 11 bytes          */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}                                 @ save registers 
    mov r3,r1
    mov r2,#LGZONECAL
 
1:	                                            @ start loop
    bl divisionpar10U                               @ unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                                      @ digit
    strb r1,[r3,r2]                                 @ store digit on area
    cmp r0,#0                                       @ stop if quotient = 0 
    subne r2,#1                                     @ else previous position
    bne 1b	                                    @ and loop
                                                    @ and move digit from left of area
    mov r4,#0
2:
    ldrb r1,[r3,r2]
    strb r1,[r3,r4]
    add r2,#1
    add r4,#1
    cmp r2,#LGZONECAL
    ble 2b
                                                      @ and move spaces in end on area
    mov r0,r4                                         @ result length 
    mov r1,#' '                                       @ space
3:
    strb r1,[r3,r4]                                   @ store space in area
    add r4,#1                                         @ next position
    cmp r4,#LGZONECAL
    ble 3b                                            @ loop if r4 <= area size
 
100:
    pop {r1-r4,lr}                                    @ restaur registres 
    bx lr                                             @return
 
/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient */	
/* r1 remainder  */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                          @ save value
    //mov r3,#0xCCCD                                   @ r3 <- magic_number lower  raspberry 3
    //movt r3,#0xCCCC                                  @ r3 <- magic_number higter raspberry 3
    ldr r3,iMagicNumber                                @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                               @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0) 
    mov r0, r2, LSR #3                                 @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                               @ r2 <- r0 * 5 
    sub r1,r4,r2, lsl #1                               @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                              @ leave function 
iMagicNumber:  	.int 0xCCCCCCCD


```



## Arturo



```arturo
quickSort [items]{
	if $(size items) < 2 { items } { 
		$(quickSort|filter $(slice items 1) { & < items.0 }) + items.0 +  $(quickSort|filter $(slice items 1) { & >= items.0 })
	}
}

print $(quickSort #(3 1 2 8 5 7 9 4 6))
```


{{out}}


```txt
#(1 2 3 4 5 6 7 8 9)
```



## AWK


```awk

# the following qsort implementation extracted from:
#
#       ftp://ftp.armory.com/pub/lib/awk/qsort
#
# Copyleft GPLv2 John DuBois
#
# @(#) qsort 1.2.1 2005-10-21
# 1990 john h. dubois iii (john@armory.com)
#
# qsortArbIndByValue(): Sort an array according to the values of its elements.
#
# Input variables:
#
# Arr[] is an array of values with arbitrary (associative) indices.
#
# Output variables:
#
# k[] is returned with numeric indices 1..n.  The values assigned to these
# indices are the indices of Arr[], ordered so that if Arr[] is stepped
# through in the order Arr[k[1]] .. Arr[k[n]], it will be stepped through in
# order of the values of its elements.
#
# Return value: The number of elements in the arrays (n).
#
# NOTES:
#
# Full example for accessing results:
#
#       foolist["second"] = 2;
#       foolist["zero"] = 0;
#       foolist["third"] = 3;
#       foolist["first"] = 1;
#
#       outlist[1] = 0;
#       n = qsortArbIndByValue(foolist, outlist)
#
#       for (i = 1; i <= n; i++) {
#               printf("item at %s has value %d\n", outlist[i], foolist[outlist[i]]);
#       }
#      delete outlist; 
#
function qsortArbIndByValue(Arr, k,
                            ArrInd, ElNum)
{
        ElNum = 0;
        for (ArrInd in Arr) {
                k[++ElNum] = ArrInd;
        }
        qsortSegment(Arr, k, 1, ElNum);
        return ElNum;
}
#
# qsortSegment(): Sort a segment of an array.
#
# Input variables:
#
# Arr[] contains data with arbitrary indices.
#
# k[] has indices 1..nelem, with the indices of Arr[] as values.
#
# Output variables:
#
# k[] is modified by this function.  The elements of Arr[] that are pointed to
# by k[start..end] are sorted, with the values of elements of k[] swapped
# so that when this function returns, Arr[k[start..end]] will be in order.
#
# Return value: None.
#
function qsortSegment(Arr, k, start, end,
                      left, right, sepval, tmp, tmpe, tmps)
{
        if ((end - start) < 1) {        # 0 or 1 elements
                return;
        }
        # handle two-element case explicitly for a tiny speedup
        if ((end - start) == 1) {
                if (Arr[tmps = k[start]] > Arr[tmpe = k[end]]) {
                        k[start] = tmpe;
                        k[end] = tmps;
                }
                return;
        }
        # Make sure comparisons act on these as numbers
        left = start + 0;
        right = end + 0;
        sepval = Arr[k[int((left + right) / 2)]];
        # Make every element <= sepval be to the left of every element > sepval
        while (left < right) {
                while (Arr[k[left]] < sepval) {
                        left++;
                }
                while (Arr[k[right]] > sepval) {
                        right--;
                }
                if (left < right) {
                        tmp = k[left];
                        k[left++] = k[right];
                        k[right--] = tmp;
                }
        }
        if (left == right)
                if (Arr[k[left]] < sepval) {
                        left++;
                } else {
                        right--;
                }
        if (start < right) {
                qsortSegment(Arr, k, start, right);
        }
        if (left < end) {
                qsortSegment(Arr, k, left, end);
        }
}

```



## AutoHotkey

Translated from the python example:

```AutoHotkey
a := [4, 65, 2, -31, 0, 99, 83, 782, 7]
for k, v in QuickSort(a)
	Out .= "," v
MsgBox, % SubStr(Out, 2)
return

QuickSort(a)
{
	if (a.MaxIndex() <= 1)
		return a
	Less := [], Same := [], More := []
	Pivot := a[1]
	for k, v in a
	{
		if (v < Pivot)
			less.Insert(v)
		else if (v > Pivot)
			more.Insert(v)
		else
			same.Insert(v)
	}
	Less := QuickSort(Less)
	Out := QuickSort(More)
	if (Same.MaxIndex())
		Out.Insert(1, Same*) ; insert all values of same at index 1
	if (Less.MaxIndex())
		Out.Insert(1, Less*) ; insert all values of less at index 1
	return Out
}
```


Old implementation for AutoHotkey 1.0:

```AutoHotkey
MsgBox % quicksort("8,4,9,2,1")

quicksort(list)
{
  StringSplit, list, list, `,
  If (list0 <= 1)
    Return list
  pivot := list1
  Loop, Parse, list, `,
  {
    If (A_LoopField < pivot)
      less = %less%,%A_LoopField%
    Else If (A_LoopField > pivot)
      more = %more%,%A_LoopField%
    Else
      pivotlist = %pivotlist%,%A_LoopField%
  }
  StringTrimLeft, less, less, 1
  StringTrimLeft, more, more, 1
  StringTrimLeft, pivotList, pivotList, 1
  less := quicksort(less)
  more := quicksort(more)
  Return less . pivotList . more
}
```



## BASIC

{{works with|FreeBASIC}}
{{works with|PowerBASIC for DOS}}
{{works with|QB64}}
{{works with|QBasic}}

This is specifically for <code>INTEGER</code>s, but can be modified for any data type by changing <code>arr()</code>'s type.


```qbasic
DECLARE SUB quicksort (arr() AS INTEGER, leftN AS INTEGER, rightN AS INTEGER)

DIM q(99) AS INTEGER
DIM n AS INTEGER

RANDOMIZE TIMER

FOR n = 0 TO 99
    q(n) = INT(RND * 9999)
NEXT

OPEN "output.txt" FOR OUTPUT AS 1
    FOR n = 0 TO 99
        PRINT #1, q(n),
    NEXT
    PRINT #1,
    quicksort q(), 0, 99
    FOR n = 0 TO 99
        PRINT #1, q(n),
    NEXT
CLOSE

SUB quicksort (arr() AS INTEGER, leftN AS INTEGER, rightN AS INTEGER)
    DIM pivot AS INTEGER, leftNIdx AS INTEGER, rightNIdx AS INTEGER
    leftNIdx = leftN
    rightNIdx = rightN
    IF (rightN - leftN) > 0 THEN
        pivot = (leftN + rightN) / 2
        WHILE (leftNIdx <= pivot) AND (rightNIdx >= pivot)
            WHILE (arr(leftNIdx) < arr(pivot)) AND (leftNIdx <= pivot)
                leftNIdx = leftNIdx + 1
            WEND
            WHILE (arr(rightNIdx) > arr(pivot)) AND (rightNIdx >= pivot)
                rightNIdx = rightNIdx - 1
            WEND
            SWAP arr(leftNIdx), arr(rightNIdx)
            leftNIdx = leftNIdx + 1
            rightNIdx = rightNIdx - 1
            IF (leftNIdx - 1) = pivot THEN
                rightNIdx = rightNIdx + 1
                pivot = rightNIdx
            ELSEIF (rightNIdx + 1) = pivot THEN
                leftNIdx = leftNIdx - 1
                pivot = leftNIdx
            END IF
        WEND
        quicksort arr(), leftN, pivot - 1
        quicksort arr(), pivot + 1, rightN
    END IF
END SUB
```


=
## BBC BASIC
=

```bbcbasic
      DIM test(9)
      test() = 4, 65, 2, -31, 0, 99, 2, 83, 782, 1
      PROCquicksort(test(), 0, 10)
      FOR i% = 0 TO 9
        PRINT test(i%) ;
      NEXT
      PRINT
      END
      
      DEF PROCquicksort(a(), s%, n%)
      LOCAL l%, p, r%, t%
      IF n% < 2 THEN ENDPROC
      t% = s% + n% - 1
      l% = s%
      r% = t%
      p = a((l% + r%) DIV 2)
      REPEAT
        WHILE a(l%) < p l% += 1 : ENDWHILE
        WHILE a(r%) > p r% -= 1 : ENDWHILE
        IF l% <= r% THEN
          SWAP a(l%), a(r%)
          l% += 1
          r% -= 1
        ENDIF
      UNTIL l% > r%
      IF s% < r% PROCquicksort(a(), s%, r% - s% + 1)
      IF l% < t% PROCquicksort(a(), l%, t% - l% + 1 )
      ENDPROC
```

{{out}}

```txt

       -31         0         1         2         2         4        65        83        99       782

```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 PROGRAM "QuickSrt.bas"
110 RANDOMIZE
120 NUMERIC A(5 TO 19)
130 CALL INIT(A)
140 CALL WRITE(A)
150 CALL QSORT(LBOUND(A),UBOUND(A))
160 CALL WRITE(A)
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
280 DEF QSORT(AH,FH)
290   NUMERIC E
300   LET E=AH:LET U=FH:LET K=A(E)
310   DO UNTIL E=U
320     DO UNTIL E=U OR A(U)<K
330       LET U=U-1
340     LOOP
350     IF E<U THEN
360       LET A(E)=A(U):LET E=E+1
370       DO UNTIL E=U OR A(E)>K
380         LET E=E+1
390       LOOP
400       IF E<U THEN LET A(U)=A(E):LET U=U-1
410     END IF
420   LOOP
430   LET A(E)=K
440   IF AH<E-1 THEN CALL QSORT(AH,E-1)
450   IF E+1<FH THEN CALL QSORT(E+1,FH)
460 END DEF
```



## BCPL


```BCPL
// This can be run using Cintcode BCPL freely available from www.cl.cam.ac.uk/users/mr10.

GET "libhdr.h"

LET quicksort(v, n) BE qsort(v+1, v+n)

AND qsort(l, r) BE
{ WHILE l+8<r DO
  { LET midpt = (l+r)/2
    // Select a good(ish) median value.
    LET val   = middle(!l, !midpt, !r)
    LET i = partition(val, l, r)
    // Only use recursion on the smaller partition.
    TEST i>midpt THEN { qsort(i, r);   r := i-1 }
                 ELSE { qsort(l, i-1); l := i   }
  }

  FOR p = l+1 TO r DO  // Now perform insertion sort.
   FOR q = p-1 TO l BY -1 TEST q!0<=q!1 THEN BREAK
                                        ELSE { LET t = q!0
                                               q!0 := q!1
                                               q!1 := t
                                             }
}

AND middle(a, b, c) = a<b -> b<c -> b,
                                    a<c -> c,
                                           a,
                             b<c -> a<c -> a,
                                           c,
                                    b

AND partition(median, p, q) = VALOF
{ LET t = ?
  WHILE !p < median DO p := p+1
  WHILE !q > median DO q := q-1
  IF p>=q RESULTIS p
  t  := !p
  !p := !q
  !q := t
  p, q := p+1, q-1
} REPEAT

LET start() = VALOF {
  LET v = VEC 1000
  FOR i = 1 TO 1000 DO v!i := randno(1_000_000)
  quicksort(v, 1000)
  FOR i = 1 TO 1000 DO
  { IF i MOD 10 = 0 DO newline()
    writef(" %i6", v!i)
  }
  newline()
}
```



## Bracmat

Instead of comparing elements explicitly, this solution puts the two elements-to-compare in a sum. After evaluating the sum its terms are sorted. Numbers are sorted numerically, strings alphabetically and compound expressions by comparing nodes and leafs in a left-to right order. Now there are three cases: either the terms stayed put, or they were swapped, or they were equal and were combined into one term with a factor <code>2</code> in front. To not let the evaluator add numbers together, each term is constructed as a dotted list.

```bracmat
( ( Q
  =   Less Greater Equal pivot element
    .     !arg:%(?pivot:?Equal) %?arg
        & :?Less:?Greater
        &   whl
          ' ( !arg:%?element ?arg
            &   (.!element)+(.!pivot)               { BAD: 1900+90 adds to 1990,  GOOD: (.1900)+(.90) is sorted to (.90)+(.1900) }
              : (   (.!element)+(.!pivot)
                  & !element !Less:?Less
                |   (.!pivot)+(.!element)
                  & !element !Greater:?Greater
                | ?&!element !Equal:?Equal
                )
            )
        & Q$!Less !Equal Q$!Greater
      | !arg
  )
& out$Q$(1900 optimized variants of 4001/2 Quicksort (quick,sort) are (quick,sober) features of 90 languages)
);
```

{{out}}

```txt
  90
  1900
  4001/2
  Quicksort
  are
  features
  languages
  of
  of
  optimized
  variants
  (quick,sober)
  (quick,sort)
```



## C


```c

#include <stdio.h>

void quicksort(int *A, int len);

int main (void) {
  int a[] = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1};
  int n = sizeof a / sizeof a[0];

  int i;
  for (i = 0; i < n; i++) {
    printf("%d ", a[i]);
  }
  printf("\n");

  quicksort(a, n);

  for (i = 0; i < n; i++) {
    printf("%d ", a[i]);
  }
  printf("\n");

  return 0;
}

void quicksort(int *A, int len) {
  if (len < 2) return;

  int pivot = A[len / 2];

  int i, j;
  for (i = 0, j = len - 1; ; i++, j--) {
    while (A[i] < pivot) i++;
    while (A[j] > pivot) j--;

    if (i >= j) break;

    int temp = A[i];
    A[i]     = A[j];
    A[j]     = temp;
  }

  quicksort(A, i);
  quicksort(A + i, len - i);
}

```


{{out}}

```txt

4 65 2 -31 0 99 2 83 782 1
-31 0 1 2 2 4 65 83 99 782

```


Randomized sort with separated components.


```c

#include <stdlib.h>     // REQ: rand()

void swap(int *a, int *b) {
  int c = *a;
  *a = *b;
  *b = c;
}

int partition(int A[], int p, int q) {
  swap(&A[p + (rand() % (q - p + 1))], &A[q]);   // PIVOT = A[q]

  int i = p - 1;
  for(int j = p; j <= q; j++) {
    if(A[j] <= A[q]) {
      swap(&A[++i], &A[j]);
    }
  }

  return i;
}

void quicksort(int A[], int p, int q) {
  if(p < q) {
    int pivotIndx = partition(A, p, q);

    quicksort(A, p, pivotIndx - 1);
    quicksort(A, pivotIndx + 1, q);
  }
}

```



## C++

The following implements quicksort with a median-of-three pivot. As idiomatic in C++, the argument <tt>last</tt> is a one-past-end iterator. Note that this code takes advantage of <tt>std::partition</tt>, which is O(n). Also note that it needs a random-access iterator for efficient calculation of the median-of-three pivot (more exactly, for O(1) calculation of the iterator <tt>mid</tt>).

```cpp>#include <iterator

#include <algorithm> // for std::partition
#include <functional> // for std::less

// helper function for median of three
template<typename T>
 T median(T t1, T t2, T t3)
{
  if (t1 < t2)
  {
    if (t2 < t3)
      return t2;
    else if (t1 < t3)
      return t3;
    else
      return t1;
  }
  else
  {
    if (t1 < t3)
      return t1;
    else if (t2 < t3)
      return t3;
    else
      return t2;
  }
}

// helper object to get <= from <
template<typename Order> struct non_strict_op:
  public std::binary_function<typename Order::second_argument_type,
                              typename Order::first_argument_type,
                              bool>
{
  non_strict_op(Order o): order(o) {}
  bool operator()(typename Order::second_argument_type arg1,
                  typename Order::first_argument_type arg2) const
  {
    return !order(arg2, arg1);
  }
private:
  Order order;
};

template<typename Order> non_strict_op<Order> non_strict(Order o)
{
  return non_strict_op<Order>(o);
}

template<typename RandomAccessIterator,
         typename Order>
 void quicksort(RandomAccessIterator first, RandomAccessIterator last, Order order)
{
  if (first != last && first+1 != last)
  {
    typedef typename std::iterator_traits<RandomAccessIterator>::value_type value_type;
    RandomAccessIterator mid = first + (last - first)/2;
    value_type pivot = median(*first, *mid, *(last-1));
    RandomAccessIterator split1 = std::partition(first, last, std::bind2nd(order, pivot));
    RandomAccessIterator split2 = std::partition(split1, last, std::bind2nd(non_strict(order), pivot));
    quicksort(first, split1, order);
    quicksort(split2, last, order);
  }
}

template<typename RandomAccessIterator>
 void quicksort(RandomAccessIterator first, RandomAccessIterator last)
{
  quicksort(first, last, std::less<typename std::iterator_traits<RandomAccessIterator>::value_type>());
}
```


A simpler version of the above that just uses the first element as the pivot and only does one "partition".

```cpp>#include <iterator

#include <algorithm> // for std::partition
#include <functional> // for std::less

template<typename RandomAccessIterator,
         typename Order>
 void quicksort(RandomAccessIterator first, RandomAccessIterator last, Order order)
{
  if (last - first > 1)
  {
    RandomAccessIterator split = std::partition(first+1, last, std::bind2nd(order, *first));
    std::iter_swap(first, split-1);
    quicksort(first, split-1, order);
    quicksort(split, last, order);
  }
}

template<typename RandomAccessIterator>
 void quicksort(RandomAccessIterator first, RandomAccessIterator last)
{
  quicksort(first, last, std::less<typename std::iterator_traits<RandomAccessIterator>::value_type>());
}
```

=={{header|C sharp|C#}}==
Note that Array.Sort and ArrayList.Sort both use an unstable implementation of the quicksort algorithm.

```csharp
//
// The Tripartite conditional enables Bentley-McIlroy 3-way Partitioning.
// This performs additional compares to isolate islands of keys equal to
// the pivot value.  Use unless key-equivalent classes are of small size.
//
#define Tripartite

namespace Sort {
  using System;
  using System.Diagnostics;

  class QuickSort<T> where T : IComparable {
    #region Constants
    private const Int32 INSERTION_LIMIT_DEFAULT = 12;
    #endregion

    #region Properties
    public Int32 InsertionLimit { get; set; }
    private Random Random { get; set; }
    private T Median { get; set; }

    private Int32 Left { get; set; }
    private Int32 Right { get; set; }
    private Int32 LeftMedian { get; set; }
    private Int32 RightMedian { get; set; }
    #endregion

    #region Constructors
    public QuickSort(Int32 insertionLimit, Random random) {
      this.InsertionLimit = insertionLimit;
      this.Random = random;
    }

    public QuickSort(Int32 insertionLimit)
      : this(insertionLimit, new Random()) {
    }

    public QuickSort()
      : this(INSERTION_LIMIT_DEFAULT) {
    }
    #endregion

    #region Sort Methods
    public void Sort(T[] entries) {
      Sort(entries, 0, entries.Length - 1);
    }

    public void Sort(T[] entries, Int32 first, Int32 last) {
      var length = last + 1 - first;
      while (length > 1) {
        if (length < InsertionLimit) {
          InsertionSort<T>.Sort(entries, first, last);
          return;
        }

        Left = first;
        Right = last;
        pivot(entries);
        partition(entries);
        //[Note]Right < Left

        var leftLength = Right + 1 - first;
        var rightLength = last + 1 - Left;

        //
        // First recurse over shorter partition, then loop
        // on the longer partition to elide tail recursion.
        //
        if (leftLength < rightLength) {
          Sort(entries, first, Right);
          first = Left;
          length = rightLength;
        }
        else {
          Sort(entries, Left, last);
          last = Right;
          length = leftLength;
        }
      }
    }

    private void pivot(T[] entries) {
      //
      // An odd sample size is chosen based on the log of the interval size.
      // The median of a randomly chosen set of samples is then returned as
      // an estimate of the true median.
      //
      var length = Right + 1 - Left;
      var logLen = (Int32)Math.Log10(length);
      var pivotSamples = 2 * logLen + 1;
      var sampleSize = Math.Min(pivotSamples, length);
      var last = Left + sampleSize - 1;
      // Sample without replacement
      for (var first = Left; first <= last; first++) {
        // Random sampling avoids pathological cases
        var random = Random.Next(first, Right + 1);
        Swap(entries, first, random);
      }

      InsertionSort<T>.Sort(entries, Left, last);
      Median = entries[Left + sampleSize / 2];
    }

    private void partition(T[] entries) {
      var first = Left;
      var last = Right;
#if Tripartite
      LeftMedian = first;
      RightMedian = last;
#endif
      while (true) {
        //[Assert]There exists some index >= Left where entries[index] >= Median
        //[Assert]There exists some index <= Right where entries[index] <= Median
        // So, there is no need for Left or Right bound checks
        while (Median.CompareTo(entries[Left]) > 0) Left++;
        while (Median.CompareTo(entries[Right]) < 0) Right--;

        //[Assert]entries[Right] <= Median <= entries[Left]
        if (Right <= Left) break;

        Swap(entries, Left, Right);
        swapOut(entries);
        Left++;
        Right--;
        //[Assert]entries[first:Left - 1] <= Median <= entries[Right + 1:last]
      }

      if (Left == Right) {
        Left++;
        Right--;
      }
      //[Assert]Right < Left
      swapIn(entries, first, last);

      //[Assert]entries[first:Right] <= Median <= entries[Left:last]
      //[Assert]entries[Right + 1:Left - 1] == Median when non-empty
    }
    #endregion

    #region Swap Methods
    [Conditional("Tripartite")]
    private void swapOut(T[] entries) {
      if (Median.CompareTo(entries[Left]) == 0) Swap(entries, LeftMedian++, Left);
      if (Median.CompareTo(entries[Right]) == 0) Swap(entries, Right, RightMedian--);
    }

    [Conditional("Tripartite")]
    private void swapIn(T[] entries, Int32 first, Int32 last) {
      // Restore Median entries
      while (first < LeftMedian) Swap(entries, first++, Right--);
      while (RightMedian < last) Swap(entries, Left++, last--);
    }

    public static void Swap(T[] entries, Int32 index1, Int32 index2) {
      if (index1 != index2) {
        var entry = entries[index1];
        entries[index1] = entries[index2];
        entries[index2] = entry;
      }
    }
    #endregion
  }

  #region Insertion Sort
  static class InsertionSort<T> where T : IComparable {
    public static void Sort(T[] entries, Int32 first, Int32 last) {
      for (var index = first + 1; index <= last; index++)
        insert(entries, first, index);
    }

    private static void insert(T[] entries, Int32 first, Int32 index) {
      var entry = entries[index];
      while (index > first && entries[index - 1].CompareTo(entry) > 0)
        entries[index] = entries[--index];
      entries[index] = entry;
    }
  }
  #endregion
}
```

'''Example''':

```csharp
  using Sort;
  using System;

  class Program {
    static void Main(String[] args) {
      var entries = new Int32[] { 1, 3, 5, 7, 9, 8, 6, 4, 2 };
      var sorter = new QuickSort<Int32>();
      sorter.Sort(entries);
      Console.WriteLine(String.Join(" ", entries));
    }
  }
```

{{out}}

```txt
1 2 3 4 5 6 7 8 9
```


A very inefficient way to do qsort in C# to prove C# code can be just as compact and readable as any dynamic code


```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace QSort
{
    class QSorter
    {
        private static IEnumerable<IComparable> empty = new List<IComparable>();

        public static IEnumerable<IComparable> QSort(IEnumerable<IComparable> iEnumerable)
        {
            if(iEnumerable.Any())
            {
                var pivot = iEnumerable.First();
                return QSort(iEnumerable.Where((anItem) => pivot.CompareTo(anItem) > 0)).
                    Concat(iEnumerable.Where((anItem) => pivot.CompareTo(anItem) == 0)).
                    Concat(QSort(iEnumerable.Where((anItem) => pivot.CompareTo(anItem) < 0)));
            }
            return empty;
        }
    }
}
```



## Clojure


A very Haskell-like solution using list comprehensions and lazy evaluation.

```lisp
(defn qsort [L]
  (if (empty? L) 
      '()
      (let [[pivot & L2] L]
           (lazy-cat (qsort (for [y L2 :when (<  y pivot)] y))
                     (list pivot)
                     (qsort (for [y L2 :when (>= y pivot)] y))))))
```


Another short version (using quasiquote):


```lisp
(defn qsort [[pvt & rs]]
  (if pvt
    `(~@(qsort (filter #(<  % pvt) rs))
      ~pvt 
      ~@(qsort (filter #(>= % pvt) rs)))))
```


Another, more readable version (no macros):


```lisp
(defn qsort [[pivot & xs]]
  (when pivot
    (let [smaller #(< % pivot)]
      (lazy-cat (qsort (filter smaller xs))
		[pivot]
		(qsort (remove smaller xs))))))
```


A 3-group quicksort (fast when many values are equal):

```lisp
(defn qsort3 [[pvt :as coll]]
  (when pvt
    (let [{left -1 mid 0 right 1} (group-by #(compare % pvt) coll)]
      (lazy-cat (qsort3 left) mid (qsort3 right)))))
```


A lazier version of above (unlike group-by, filter returns (and reads) a lazy sequence)

```lisp
(defn qsort3 [[pivot :as coll]]
  (when pivot
    (lazy-cat (qsort (filter #(< % pivot) coll))
              (filter #{pivot} coll)
              (qsort (filter #(> % pivot) coll)))))
```



## COBOL

{{works with|Visual COBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. quicksort RECURSIVE.
       
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  temp                   PIC S9(8).
       
       01  pivot                  PIC S9(8).
       
       01  left-most-idx          PIC 9(5).
       01  right-most-idx         PIC 9(5).
       
       01  left-idx               PIC 9(5).
       01  right-idx              PIC 9(5).
       
       LINKAGE SECTION.
       78  Arr-Length             VALUE 50.
       
       01  arr-area.
           03  arr                PIC S9(8) OCCURS Arr-Length TIMES.
           
       01  left-val               PIC 9(5).
       01  right-val              PIC 9(5).  
       
       PROCEDURE DIVISION USING REFERENCE arr-area, OPTIONAL left-val,
               OPTIONAL right-val.
           IF left-val IS OMITTED OR right-val IS OMITTED
               MOVE 1 TO left-most-idx, left-idx
               MOVE Arr-Length TO right-most-idx, right-idx
           ELSE
               MOVE left-val TO left-most-idx, left-idx
               MOVE right-val TO right-most-idx, right-idx
           END-IF
           
           IF right-most-idx - left-most-idx < 1
               GOBACK
           END-IF
       
           COMPUTE pivot = arr ((left-most-idx + right-most-idx) / 2)
       
           PERFORM UNTIL left-idx > right-idx
               PERFORM VARYING left-idx FROM left-idx BY 1
                   UNTIL arr (left-idx) >= pivot
               END-PERFORM
               
               PERFORM VARYING right-idx FROM right-idx BY -1
                   UNTIL arr (right-idx) <= pivot
               END-PERFORM
               
               IF left-idx <= right-idx
                   MOVE arr (left-idx) TO temp
                   MOVE arr (right-idx) TO arr (left-idx)
                   MOVE temp TO arr (right-idx)
                   
                   ADD 1 TO left-idx
                   SUBTRACT 1 FROM right-idx
               END-IF
           END-PERFORM
       
           CALL "quicksort" USING REFERENCE arr-area,
               CONTENT left-most-idx, right-idx
           CALL "quicksort" USING REFERENCE arr-area, CONTENT left-idx,
               right-most-idx
               
           GOBACK
           .
```



## CoffeeScript


```coffeescript

quicksort = ([x, xs...]) ->
  return [] unless x?
  smallerOrEqual = (a for a in xs when a <= x)
  larger = (a for a in xs when a > x)
  (quicksort smallerOrEqual).concat(x).concat(quicksort larger)

```



## Common Lisp


The functional programming way


```lisp
(defun quicksort (list &aux (pivot (car list)) )
  (if (cdr list)
      (nconc (quicksort (remove-if-not #'(lambda (x) (< x pivot)) list))
             (remove-if-not #'(lambda (x) (= x pivot)) list)
             (quicksort (remove-if-not #'(lambda (x) (> x pivot)) list)))
      list))
```


With flet


```lisp
(defun qs (list)
  (if (cdr list)
      (flet ((pivot (test)
               (remove (car list) list :test-not test)))
        (nconc (qs (pivot #'>)) (pivot #'=) (qs (pivot #'<))))
      list))
```


In-place non-functional


```lisp
(defun quicksort (sequence)
  (labels ((swap (a b) (rotatef (elt sequence a) (elt sequence b)))
           (sub-sort (left right)
             (when (< left right)
               (let ((pivot (elt sequence right))
                     (index left))
                 (loop for i from left below right
                       when (<= (elt sequence i) pivot)
                         do (swap i (prog1 index (incf index))))
                 (swap right index)
                 (sub-sort left (1- index))
                 (sub-sort (1+ index) right)))))
    (sub-sort 0 (1- (length sequence)))
    sequence))
```


Functional with destructuring


```lisp

(defun quicksort (list)
  (when list
    (destructuring-bind (x . xs) list
      (nconc (quicksort (remove-if (lambda (a) (> a x)) xs))
	     `(,x)
	     (quicksort (remove-if (lambda (a) (<= a x)) xs))))))
```



## Crystal

{{trans|Ruby}}

```ruby
def quick_sort(a : Array(Int32)) : Array(Int32)
  return a if a.size <= 1
  p = a[0]
  lt, rt = a[1 .. -1].partition { |x| x < p }
  return quick_sort(lt) + [p] + quick_sort(rt)
end

a = [7, 6, 5, 9, 8, 4, 3, 1, 2, 0]
puts quick_sort(a) # => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Curry

Copied from [http://www.informatik.uni-kiel.de/~curry/examples/ Curry: Example Programs].

```curry
-- quicksort using higher-order functions:

qsort :: [Int] -> [Int] 
qsort []     = []
qsort (x:l)  = qsort (filter (<x) l) ++ x : qsort (filter (>=x) l)

goal = qsort [2,3,1,0]
```



## D

A functional version:

```d
import std.stdio, std.algorithm, std.range, std.array;

auto quickSort(T)(T[] items) pure nothrow @safe {
    if (items.length < 2)
        return items;
    immutable pivot = items[0];
    return items[1 .. $].filter!(x => x < pivot).array.quickSort ~
           pivot ~
           items[1 .. $].filter!(x => x >= pivot).array.quickSort;
}

void main() {
    [4, 65, 2, -31, 0, 99, 2, 83, 782, 1].quickSort.writeln;
}
```

{{out}}

```txt
[-31, 0, 1, 2, 2, 4, 65, 83, 99, 782]
```


A simple high-level version (same output):

```d
import std.stdio, std.array;

T[] quickSort(T)(T[] items) pure nothrow {
    if (items.empty)
        return items;
    T[] less, notLess;
    foreach (x; items[1 .. $])
        (x < items[0] ? less : notLess) ~= x;
    return less.quickSort ~ items[0] ~ notLess.quickSort;
}

void main() {
    [4, 65, 2, -31, 0, 99, 2, 83, 782, 1].quickSort.writeln;
}
```


Often short functional sieves are not a true implementations of the Sieve of Eratosthenes:
http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf

Similarly, one could argue that a true QuickSort is in-place, 
as this more efficient version (same output):

```d
import std.stdio, std.algorithm;

void quickSort(T)(T[] items) pure nothrow @safe @nogc {
    if (items.length >= 2) {
        auto parts = partition3(items, items[$ / 2]);
        parts[0].quickSort;
        parts[2].quickSort;
    }
}

void main() {
    auto items = [4, 65, 2, -31, 0, 99, 2, 83, 782, 1];
    items.quickSort;
    items.writeln;
}
```



## Dart


```dart
quickSort(List a) {
  if (a.length <= 1) {
    return a;
  }
  
  var pivot = a[0];
  var less = [];
  var more = [];
  var pivotList = [];
  
  // Partition
  a.forEach((var i){    
    if (i.compareTo(pivot) < 0) {
      less.add(i);
    } else if (i.compareTo(pivot) > 0) {
      more.add(i);
    } else {
      pivotList.add(i);
    }
  });
  
  // Recursively sort sublists
  less = quickSort(less);
  more = quickSort(more);
  
  // Concatenate results
  less.addAll(pivotList);
  less.addAll(more);
  return less;
}

void main() {
  var arr=[1,5,2,7,3,9,4,6,8];
  print("Before sort");
  arr.forEach((var i)=>print("$i"));
  arr = quickSort(arr);
  print("After sort");
  arr.forEach((var i)=>print("$i"));
}
```


## E



```e
def quicksort := {

    def swap(container, ixA, ixB) {
        def temp := container[ixA]
        container[ixA] := container[ixB]
        container[ixB] := temp
    }

    def partition(array, var first :int, var last :int) {
        if (last <= first) { return }
  
        # Choose a pivot
        def pivot := array[def pivotIndex := (first + last) // 2]
  
        # Move pivot to end temporarily
        swap(array, pivotIndex, last)
  
        var swapWith := first
  
        # Scan array except for pivot, and...
        for i in first..!last {
            if (array[i] <= pivot) {   # items ≤ the pivot
                swap(array, i, swapWith) # are moved to consecutive positions on the left
                swapWith += 1
            }
        }
  
        # Swap pivot into between-partition position.
        # Because of the swapping we know that everything before swapWith is less
        # than or equal to the pivot, and the item at swapWith (since it was not
        # swapped) is greater than the pivot, so inserting the pivot at swapWith
        # will preserve the partition.
        swap(array, swapWith, last)
        return swapWith
    }

    def quicksortR(array, first :int, last :int) {
        if (last <= first) { return }
        def pivot := partition(array, first, last)
        quicksortR(array, first, pivot - 1)
        quicksortR(array, pivot + 1, last)
    }

    def quicksort(array) { # returned from block
        quicksortR(array, 0, array.size() - 1)
    }
}
```



## EasyLang

<lang>data[] = [ 29 4 72 44 55 26 27 77 92 5 ]
# 
func qsort left right . .
  while left < right
    swap data[(left + right) / 2] data[left]
    mid = left
    for i = left + 1 to right
      if data[i] < data[left]
        mid += 1
        swap data[i] data[mid]
      .
    .
    swap data[left] data[mid]
    call qsort left mid - 1
    left = mid + 1
  .
.
# 
call qsort 0 len data[] - 1
print data[]
```



## EchoLisp


```scheme

(lib 'list) ;; list-partition

(define compare 0) ;; counter

(define (quicksort L compare-predicate: proc aux:  (part null))
(if  (<= (length L) 1) L
     (begin
     ;; counting the number of comparisons
     (set! compare (+ compare (length (rest L))))
      ;; pivot = first element of list
     (set! part (list-partition (rest L) proc (first L)))
     (append (quicksort (first part) proc )
            (list (first L)) 
            (quicksort (second part) proc)))))

```

{{out}}

```scheme

(shuffle (iota 15))
    → (10 0 14 11 13 9 2 5 4 8 1 7 12 3 6)
(quicksort (shuffle (iota 15)) <)
    → (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)

;; random list of numbers in [0 .. n[
;; count number of comparisons
(define (qtest (n 10000))
	(set! compare 0)
	(quicksort (shuffle (iota n)) >)
	(writeln 'n n 'compare# compare ))
	
(qtest 1000)
  n     1000       compare#     12764    
(qtest 10000)
  n     10000      compare#     277868    
(qtest 100000)
  n     100000     compare#     6198601    


```



## Eero

Translated from Objective-C example on this page.

```objc>#import <Foundation/Foundation.h


void quicksortInPlace(MutableArray array, const long first, const long last)
  if first >= last
    return
  Value pivot = array[(first + last) / 2]
  left := first
  right := last
  while left <= right
    while array[left] < pivot
      left++
    while array[right] > pivot
      right--
    if left <= right
      array.exchangeObjectAtIndex: left++, withObjectAtIndex: right--

  quicksortInPlace(array, first, right)
  quicksortInPlace(array, left, last)

Array quicksort(Array unsorted)
  a := []
  a.addObjectsFromArray: unsorted
  quicksortInPlace(a, 0, a.count - 1)
  return a


int main(int argc, const char * argv[])
  autoreleasepool
    a := [1, 3, 5, 7, 9, 8, 6, 4, 2]
    Log( 'Unsorted: %@', a)
    Log( 'Sorted: %@', quicksort(a) )
    b := ['Emil', 'Peg', 'Helen', 'Juergen', 'David', 'Rick', 'Barb', 'Mike', 'Tom']
    Log( 'Unsorted: %@', b)
    Log( 'Sorted: %@', quicksort(b) )

  return 0
```


Alternative implementation (not necessarily as efficient, but very readable)


```objc>#import <Foundation/Foundation.h


implementation Array (Quicksort)

  plus: Array array, return Array = 
    self.arrayByAddingObjectsFromArray: array

  filter: BOOL (^)(id) predicate, return Array
    array := []
    for id item in self
      if predicate(item)
        array.addObject: item
    return array.copy

  quicksort, return Array = self
    if self.count > 1      
      id x = self[self.count / 2]
      lesser := self.filter: (id y | return y < x)
      greater := self.filter: (id y | return y > x)
      return lesser.quicksort + [x] + greater.quicksort

end

int main()
  autoreleasepool
    a := [1, 3, 5, 7, 9, 8, 6, 4, 2]
    Log( 'Unsorted: %@', a)
    Log( 'Sorted: %@', a.quicksort )
    b := ['Emil', 'Peg', 'Helen', 'Juergen', 'David', 'Rick', 'Barb', 'Mike', 'Tom']
    Log( 'Unsorted: %@', b)
    Log( 'Sorted: %@', b.quicksort )

  return 0
```


{{out}}

```txt

2013-09-04 16:54:31.780 a.out[2201:507] Unsorted: (
    1,
    3,
    5,
    7,
    9,
    8,
    6,
    4,
    2
)
2013-09-04 16:54:31.781 a.out[2201:507] Sorted: (
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9
)
2013-09-04 16:54:31.781 a.out[2201:507] Unsorted: (
    Emil,
    Peg,
    Helen,
    Juergen,
    David,
    Rick,
    Barb,
    Mike,
    Tom
)
2013-09-04 16:54:31.782 a.out[2201:507] Sorted: (
    Barb,
    David,
    Emil,
    Helen,
    Juergen,
    Mike,
    Peg,
    Rick,
    Tom
)

```



## Eiffel

The 
```eiffel>QUICKSORT</lang
 class:

```eiffel

class
	QUICKSORT [G -> COMPARABLE]

create
	make

feature {NONE} --Implementation

	is_sorted (list: ARRAY [G]): BOOLEAN
		require
			not_void: list /= Void
		local
			i: INTEGER
		do
			Result := True
			from
				i := list.lower + 1
			invariant
				i >= list.lower + 1 and i <= list.upper + 1
			until
				i > list.upper
			loop
				Result := Result and list [i - 1] <= list [i]
				i := i + 1
			variant
				list.upper + 1 - i
			end
		end

	concatenate_array (a: ARRAY [G] b: ARRAY [G]): ARRAY [G]
		require
			not_void: a /= Void and b /= Void
		do
			create Result.make_from_array (a)
			across
				b as t
			loop
				Result.force (t.item, Result.upper + 1)
			end
		ensure
			same_size: a.count + b.count = Result.count
		end

	quicksort_array (list: ARRAY [G]): ARRAY [G]
		require
			not_void: list /= Void
		local
			less_a: ARRAY [G]
			equal_a: ARRAY [G]
			more_a: ARRAY [G]
			pivot: G
		do
			create less_a.make_empty
			create more_a.make_empty
			create equal_a.make_empty
			create Result.make_empty
			if list.count <= 1 then
				Result := list
			else
				pivot := list [list.lower]
				across
					list as li
				invariant
					less_a.count + equal_a.count + more_a.count <= list.count
				loop
					if li.item < pivot then
						less_a.force (li.item, less_a.upper + 1)
					elseif li.item = pivot then
						equal_a.force (li.item, equal_a.upper + 1)
					elseif li.item > pivot then
						more_a.force (li.item, more_a.upper + 1)
					end
				end
				Result := concatenate_array (Result, quicksort_array (less_a))
				Result := concatenate_array (Result, equal_a)
				Result := concatenate_array (Result, quicksort_array (more_a))
			end
		ensure
			same_size: list.count = Result.count
			sorted: is_sorted (Result)
		end

feature -- Initialization

	make
		do
		end

	quicksort (a: ARRAY [G]): ARRAY [G]
		do
			Result := quicksort_array (a)
		end

end

```

A test application:

```eiffel

class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
			-- Run application.
		local
			test: ARRAY [INTEGER]
			sorted: ARRAY [INTEGER]
			sorter: QUICKSORT [INTEGER]
		do
			create sorter.make
			test := <<1, 3, 2, 4, 5, 5, 7, -1>>
			sorted := sorter.quicksort (test)
			across
				sorted as s
			loop
				print (s.item)
				print (" ")
			end
			print ("%N")
		end

end

```


## Elena

ELENA 4.1 :

```elena
import extensions;
import system'routines;
import system'collections;
 
extension op
{
    quickSort()
    {
        if (self.isEmpty()) { ^ self };
 
        var pivot := self[0];
 
        auto less := new ArrayList();
        auto pivotList := new ArrayList();
        auto more := new ArrayList();
 
        self.forEach:(item)
        {
            if (item < pivot)
            {
                less.append(item)
            }
            else if (item > pivot) 
            {
                more.append(item)
            }
            else
            {
                pivotList.append(item)
            }
        };
 
        less := less.quickSort();
        more := more.quickSort();
 
        less.appendRange(pivotList);
        less.appendRange(more);
 
        ^ less
    }
}
 
public program()
{
    var list := new int[]::(3, 14, 1, 5, 9, 2, 6, 3);
 
    console.printLine("before:", list.asEnumerable());
    console.printLine("after :", list.quickSort().asEnumerable());
}
```

{{out}}

```txt

before:3,14,1,5,9,2,6,3
after :1,2,3,3,5,6,9,14

```



## Elixir


```elixir
defmodule Sort do
  def qsort([]), do: []
  def qsort([h | t]) do
    {lesser, greater} = Enum.split_with(t, &(&1 < h))
    qsort(lesser) ++ [h] ++ qsort(greater)
  end
end
```



## Erlang

like haskell.
Used by [[Measure_relative_performance_of_sorting_algorithms_implementations]]. If changed keep the interface or change [[Measure_relative_performance_of_sorting_algorithms_implementations]]

```erlang

-module( quicksort ).

-export( [qsort/1] ).

qsort([]) -> [];
qsort([X|Xs]) ->
   qsort([ Y || Y <- Xs, Y < X]) ++ [X] ++ qsort([ Y || Y <- Xs, Y >= X]).

```


multi-process implementation (number processes = number of processor cores):

```erlang

quick_sort(L) -> qs(L, trunc(math:log2(erlang:system_info(schedulers)))).

qs([],_) -> [];
qs([H|T], N) when N > 0  -> 
    {Parent, Ref} = {self(), make_ref()},
    spawn(fun()-> Parent ! {l1, Ref, qs([E||E<-T, E<H], N-1)} end), 
    spawn(fun()-> Parent ! {l2, Ref, qs([E||E<-T, H =< E], N-1)} end), 
    {L1, L2} = receive_results(Ref, undefined, undefined), 
    L1 ++ [H] ++ L2;
qs([H|T],_) ->
    qs([E||E<-T, E<H],0) ++ [H] ++ qs([E||E<-T, H =< E],0).

receive_results(Ref, L1, L2) ->
    receive
        {l1, Ref, L1R} when L2 == undefined -> receive_results(Ref, L1R, L2);
        {l2, Ref, L2R} when L1 == undefined -> receive_results(Ref, L1, L2R);
        {l1, Ref, L1R} -> {L1R, L2};
        {l2, Ref, L2R} -> {L1, L2R}
    after 5000 -> receive_results(Ref, L1, L2)
    end.

```



## ERRE


```ERRE

PROGRAM QUICKSORT_DEMO

DIM ARRAY[21]

!$DYNAMIC
DIM QSTACK[0]

!$INCLUDE="PC.LIB"

PROCEDURE QSORT(ARRAY[],START,NUM)
  FIRST=START               ! initialize work variables
  LAST=START+NUM-1
  LOOP
    REPEAT
      TEMP=ARRAY[(LAST+FIRST) DIV 2]  ! seek midpoint
      I=FIRST
      J=LAST
      REPEAT     ! reverse both < and > below to sort descending
      WHILE ARRAY[I]<TEMP DO
        I=I+1
        END WHILE
        WHILE ARRAY[J]>TEMP DO
          J=J-1
        END WHILE
        EXIT IF I>J
        IF I<J THEN SWAP(ARRAY[I],ARRAY[J]) END IF
        I=I+1
        J=J-1
      UNTIL NOT(I<=J)
      IF I<LAST THEN             ! Done
         QSTACK[SP]=I            ! Push I
         QSTACK[SP+1]=LAST       ! Push Last
         SP=SP+2
      END IF
      LAST=J
    UNTIL NOT(FIRST<LAST)

    EXIT IF SP=0
    SP=SP-2
    FIRST=QSTACK[SP]            ! Pop First
    LAST=QSTACK[SP+1]           ! Pop Last
  END LOOP
END PROCEDURE

BEGIN
   RANDOMIZE(TIMER)              ! generate a new series each run

                                 ! create an array
   FOR X=1 TO 21 DO              ! fill with random numbers
       ARRAY[X]=RND(1)*500       ! between 0 and 500
   END FOR
   PRIMO=6                       ! sort starting here
   NUM=10                        ! sort this many elements
   CLS
   PRINT("Before Sorting:";TAB(31);"After sorting:")
   PRINT("
### =========
";TAB(31);"
### ========
")
   FOR X=1 TO 21 DO              ! show them before sorting
      IF X>=PRIMO AND X<=PRIMO+NUM-1 THEN
         PRINT("==>";)
      END IF
      PRINT(TAB(5);)
      WRITE("###.##";ARRAY[X])
   END FOR

! create a stack
!$DIM QSTACK[INT(NUM/5)+10]
   QSORT(ARRAY[],PRIMO,NUM)
!$ERASE QSTACK

   LOCATE(2,1)
   FOR X=1 TO 21 DO                ! print them after sorting
      LOCATE(2+X,30)
      IF X>=PRIMO AND X<=PRIMO+NUM-1 THEN
         PRINT("==>";)             ! point to sorted items
      END IF
      LOCATE(2+X,35)
      WRITE("###.##";ARRAY[X])
   END FOR
END PROGRAM

```


=={{header|F Sharp|F#}}==

```fsharp

let rec qsort = function
    hd :: tl ->
        let less, greater = List.partition ((>=) hd) tl
        List.concat [qsort less; [hd]; qsort greater]
    | _ -> []

```



## Factor


```factor
: qsort ( seq -- seq )
    dup empty? [ 
      unclip [ [ < ] curry partition [ qsort ] bi@ ] keep
      prefix append
    ] unless ;
```



## Fexl


```Fexl
# (sort xs) is the ordered list of all elements in list xs.
# This version preserves duplicates.
\sort== 
    (\xs
    xs [] \x\xs
    append (sort; filter (gt x) xs);   # all the items less than x
    cons x; append (filter (eq x) xs); # all the items equal to x
    sort; filter (lt x) xs             # all the items greater than x
    )

# (unique xs) is the ordered list of unique elements in list xs.
\unique==
    (\xs
    xs [] \x\xs
    append (unique; filter (gt x) xs); # all the items less than x
    cons x;                            # x itself
    unique; filter (lt x) xs           # all the items greater than x
    )

```



## Forth


```forth
: mid ( l r -- mid ) over - 2/ -cell and + ;

: exch ( addr1 addr2 -- ) dup @ >r over @ swap ! r> swap ! ;

: partition ( l r -- l r r2 l2 )
  2dup mid @ >r ( r: pivot )
  2dup begin
    swap begin dup @  r@ < while cell+ repeat
    swap begin r@ over @ < while cell- repeat
    2dup <= if 2dup exch >r cell+ r> cell- then
  2dup > until  r> drop ;

: qsort ( l r -- )
  partition  swap rot
  \ 2over 2over - + < if 2swap then
  2dup < if recurse else 2drop then
  2dup < if recurse else 2drop then ;

: sort ( array len -- )
  dup 2 < if 2drop exit then
  1- cells over + qsort ;
```



## Fortran

{{Works with|Fortran|90 and later}}


```fortran
module qsort_mod

implicit none

type group
    integer :: order    ! original order of unsorted data
    real :: value       ! values to be sorted by
end type group

contains

recursive subroutine QSort(a,na)

! DUMMY ARGUMENTS
integer, intent(in) :: nA
type (group), dimension(nA), intent(in out) :: A

! LOCAL VARIABLES
integer :: left, right
real :: random
real :: pivot
type (group) :: temp
integer :: marker

    if (nA > 1) then

        call random_number(random)
        pivot = A(int(random*real(nA-1))+1)%value   ! random pivor (not best performance, but avoids worst-case)
        left = 0
        right = nA + 1

        do while (left < right)
            right = right - 1
            do while (A(right)%value > pivot)
                right = right - 1
            end do
            left = left + 1
            do while (A(left)%value < pivot)
                left = left + 1
            end do
            if (left < right) then
                temp = A(left)
                A(left) = A(right)
                A(right) = temp
            end if
        end do

        if (left == right) then
            marker = left + 1
        else
            marker = left
        end if

        call QSort(A(:marker-1),marker-1)
        call QSort(A(marker:),nA-marker+1)

    end if

end subroutine QSort

end module qsort_mod

! Test Qsort Module
program qsort_test
use qsort_mod
implicit none

integer, parameter :: l = 8
type (group), dimension(l) :: A
integer, dimension(12) :: seed = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
integer :: i
real :: random

    write (*,*) "Unsorted Values:"
    call random_seed(put = seed)
    do i = 1, l
        call random_number(random)
        A(i)%value = random
        A(i)%order = i
        if (mod(i,4) == 0) write (*,"(4(I5,1X,F8.6))") A(i-3:i)
    end do

    call QSort(A,l)
    write (*,*) "Sorted Values:"
    do i = 4, l, 4
        if (mod(i,4) == 0) write (*,"(4(I5,1X,F8.6))") A(i-3:i)
    end do

end program qsort_test
```

{{out}}

```txt

Compiled with GNU Fortran 4.6.3 
 Unsorted Values:
    1 0.228570    2 0.352733    3 0.167898    4 0.883237
    5 0.968189    6 0.806234    7 0.117714    8 0.487401
 Sorted Values:
    7 0.117714    3 0.167898    1 0.228570    2 0.352733
    8 0.487401    6 0.806234    4 0.883237    5 0.968189

```

A discussion about Quicksort pivot options, free source code for an optimized quicksort using insertion sort as a finisher, and an OpenMP multi-threaded quicksort is found at [http://balfortran.org balfortran.org]

## FreeBASIC


```freebasic
' version 23-10-2016
' compile with: fbc -s console

' sort from lower bound to the highter bound
' array's can have subscript range from -2147483648 to +2147483647

Sub quicksort(qs() As Long, l As Long, r As Long)

    Dim As ULong size = r - l +1
    If size < 2 Then Exit Sub

    Dim As Long i = l, j = r
    Dim As Long pivot = qs(l + size \ 2)

    Do
        While qs(i) < pivot
            i += 1
        Wend
        While pivot < qs(j)
            j -= 1
        Wend
        If i <= j Then
            Swap qs(i), qs(j)
            i += 1
            j -= 1
        End If
    Loop Until i > j

    If l < j Then quicksort(qs(), l, j)
    If i < r Then quicksort(qs(), i, r)

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

quicksort(array(), LBound(array), UBound(array))

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
unsorted   -5  -6  -1   0   2  -4  -7   6  -2  -3   4   7   5   1   3
  sorted   -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7
```



## FunL


```funl
def
  qsort( [] )    =  []
  qsort( p:xs )  =  qsort( xs.filter((< p)) ) + [p] + qsort( xs.filter((>= p)) )
```


Here is a more efficient version using the <code>partition</code> function.


```funl
def
  qsort( [] )    =  []
  qsort( x:xs )  =
    val (ys, zs) = xs.partition( (< x) )
    qsort( ys ) + (x : qsort( zs ))

println( qsort([4, 2, 1, 3, 0, 2]) )
println( qsort(["Juan", "Daniel", "Miguel", "William", "Liam", "Ethan", "Jacob"]) )
```


{{out}}


```txt

[0, 1, 2, 2, 3, 4]
[Daniel, Ethan, Jacob, Juan, Liam, Miguel, William]

```



## Go

Note that Go's <code>sort.Sort</code> function is a Quicksort so in practice it would be just be used.
It's actually a combination of quick sort, heap sort, and insertion sort.
It starts with a quick sort, after a depth of 2*ceil(lg(n+1)) it switches to heap sort, or once a partition becomes small (less than eight items) it switches to insertion sort.


Old school, following [http://comjnl.oxfordjournals.org/cgi/content/short/5/1/10 Hoare's 1962 paper].

As a nod to the task request to work for all types with weak strict ordering, code below uses the < operator when comparing key values.  The three points are noted in the code below.

Actually supporting arbitrary types would then require at a minimum a user supplied less-than function, and values referenced from an array of interface{} types.  More efficient and flexible though is the [http://golang.org/pkg/sort/#Interface sort interface] of the Go sort package.  Replicating that here seemed beyond the scope of the task so code was left written to sort an array of ints.

Go has no language support for indexing with discrete types other than integer types, so this was not coded.

Finally, the choice of a recursive closure over passing slices to a recursive function is really just a very small optimization.  Slices are cheap because they do not copy the underlying array, but there's still a tiny bit of overhead in constructing the slice object.  Passing just the two numbers is in the interest of avoiding that overhead.

```go
package main

import "fmt"

func main() {
    list := []int{31, 41, 59, 26, 53, 58, 97, 93, 23, 84}
    fmt.Println("unsorted:", list)

    quicksort(list)
    fmt.Println("sorted!  ", list)
}

func quicksort(a []int) {
    var pex func(int, int)
    pex = func(lower, upper int) {
        for {
            switch upper - lower {
            case -1, 0: // 0 or 1 item in segment.  nothing to do here!
                return
            case 1: // 2 items in segment
                // < operator respects strict weak order
                if a[upper] < a[lower] {
                    // a quick exchange and we're done.
                    a[upper], a[lower] = a[lower], a[upper]
                }
                return
            // Hoare suggests optimized sort-3 or sort-4 algorithms here,
            // but does not provide an algorithm.
            }

            // Hoare stresses picking a bound in a way to avoid worst case
            // behavior, but offers no suggestions other than picking a
            // random element.  A function call to get a random number is
            // relatively expensive, so the method used here is to simply
            // choose the middle element.  This at least avoids worst case
            // behavior for the obvious common case of an already sorted list.
            bx := (upper + lower) / 2
            b := a[bx]  // b = Hoare's "bound" (aka "pivot")
            lp := lower // lp = Hoare's "lower pointer"
            up := upper // up = Hoare's "upper pointer"
        outer:
            for {
                // use < operator to respect strict weak order
                for lp < upper && !(b < a[lp]) {
                    lp++
                }
                for {
                    if lp > up {
                        // "pointers crossed!"
                        break outer
                    }
                    // < operator for strict weak order
                    if a[up] < b {
                        break // inner
                    }
                    up--
                }
                // exchange
                a[lp], a[up] = a[up], a[lp]
                lp++
                up--
            }
            // segment boundary is between up and lp, but lp-up might be
            // 1 or 2, so just call segment boundary between lp-1 and lp.
            if bx < lp {
                // bound was in lower segment
                if bx < lp-1 {
                    // exchange bx with lp-1
                    a[bx], a[lp-1] = a[lp-1], b
                }
                up = lp - 2
            } else {
                // bound was in upper segment
                if bx > lp {
                    // exchange
                    a[bx], a[lp] = a[lp], b
                }
                up = lp - 1
                lp++
            }
            // "postpone the larger of the two segments" = recurse on
            // the smaller segment, then iterate on the remaining one.
            if up-lower < upper-lp {
                pex(lower, up)
                lower = lp
            } else {
                pex(lp, upper)
                upper = up
            }
        }
    }
    pex(0, len(a)-1)
}
```

{{out}}

```txt

unsorted: [31 41 59 26 53 58 97 93 23 84]
sorted!   [23 26 31 41 53 58 59 84 93 97]

```


More traditional version of quicksort. It work generically with any container that conforms to <code>sort.Interface</code>.


```go
package main

import (
    "fmt"
    "sort"
    "math/rand"
)

func partition(a sort.Interface, first int, last int, pivotIndex int) int {
    a.Swap(first, pivotIndex) // move it to beginning
    left := first+1
    right := last
    for left <= right {
        for left <= last && a.Less(left, first) {
            left++
        }
        for right >= first && a.Less(first, right) {
            right--
        }
        if left <= right {
            a.Swap(left, right)
            left++
            right--
        }
    }
    a.Swap(first, right) // swap into right place
    return right    
}

func quicksortHelper(a sort.Interface, first int, last int) {
    if first >= last {
        return
    }
    pivotIndex := partition(a, first, last, rand.Intn(last - first + 1) + first)
    quicksortHelper(a, first, pivotIndex-1)
    quicksortHelper(a, pivotIndex+1, last)
}

func quicksort(a sort.Interface) {
    quicksortHelper(a, 0, a.Len()-1)
}

func main() {
    a := []int{1, 3, 5, 7, 9, 8, 6, 4, 2}
    fmt.Printf("Unsorted: %v\n", a)
    quicksort(sort.IntSlice(a))
    fmt.Printf("Sorted: %v\n", a)
    b := []string{"Emil", "Peg", "Helen", "Juergen", "David", "Rick", "Barb", "Mike", "Tom"}
    fmt.Printf("Unsorted: %v\n", b)
    quicksort(sort.StringSlice(b))
    fmt.Printf("Sorted: %v\n", b)
}
```

{{out}}

```txt

Unsorted: [1 3 5 7 9 8 6 4 2]
Sorted: [1 2 3 4 5 6 7 8 9]
Unsorted: [Emil Peg Helen Juergen David Rick Barb Mike Tom]
Sorted: [Barb David Emil Helen Juergen Mike Peg Rick Tom]

```



## Haskell


The famous two-liner, reflecting the underlying algorithm directly:

```haskell
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y < x] ++ [x] ++ qsort [y | y <- xs, y >= x]
```

A more efficient version, doing only one comparison per element:

```haskell
import Data.List (partition)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort ys ++ x : qsort zs
  where
    (ys, zs) = partition (< x) xs
```



## IDL

IDL has a powerful optimized <tt>sort()</tt> built-in. The following is thus merely for demonstration.

```idl
function qs, arr
  if (count = n_elements(arr)) lt 2 then return,arr
  pivot = total(arr) / count ; use the average for want of a better choice
  return,[qs(arr[where(arr le pivot)]),qs(arr[where(arr gt pivot)])]
 end
```

Example:

 IDL> print,qs([3,17,-5,12,99])
      -5       3      12      17      99

=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()                     #: demonstrate various ways to sort a list and string 
   demosort(quicksort,[3, 14, 1, 5, 9, 2, 6, 3],"qwerty")
end

procedure quicksort(X,op,lower,upper)                      #: return sorted list
local pivot,x 

   if /lower := 1 then {                                   # top level call setup
      upper := *X   
      op := sortop(op,X)                                   # select how and what we sort
      }

   if upper - lower > 0 then {
      every x := quickpartition(X,op,lower,upper) do       # find a pivot and sort ...
          /pivot | X := x                                  # ... how to return 2 values w/o a structure
      X := quicksort(X,op,lower,pivot-1)                   # ... left            
      X := quicksort(X,op,pivot,upper)                     # ... right
      }

   return X                                             
end

procedure quickpartition(X,op,lower,upper)                 #: quicksort partitioner helper
local   pivot
static  pivotL
initial pivotL := list(3)

   pivotL[1] := X[lower]                                   # endpoints
   pivotL[2] := X[upper]                                   # ... and
   pivotL[3] := X[lower+?(upper-lower)]                    # ... random midpoint
   if op(pivotL[2],pivotL[1]) then pivotL[2] :=: pivotL[1] # mini-
   if op(pivotL[3],pivotL[2]) then pivotL[3] :=: pivotL[2] # ... sort
   pivot := pivotL[2]                                      # median is pivot

   lower -:= 1
   upper +:= 1
   while lower < upper do {                                # find values on wrong side of pivot ...
      while op(pivot,X[upper -:= 1])                       # ... rightmost 
      while op(X[lower +:=1],pivot)                        # ... leftmost
      if lower < upper then                                # not crossed yet
         X[lower] :=: X[upper]                             # ... swap 
      }

   suspend lower                                           # 1st return pivot point
   suspend X                                               # 2nd return modified X (in case immutable)
end
```


Implementation notes:
* Since this transparently sorts both string and list arguments the result must 'return' to bypass call by value (strings)
* The partition procedure must "return" two values - 'suspend' is used to accomplish this
Algorithm notes:
* The use of a type specific sorting operator meant that a general pivot choice need to be made. The median of the ends and random middle seemed reasonable.  It turns out to have been suggested by Sedgewick.
* Sedgewick's suggestions for tail calling to recurse into the larger side and using insertion sort below a certain size were not implemented.  (Q: does Icon/Unicon has tail calling optimizations?)
<br/>Note: This example relies on [[Sorting_algorithms/Bubble_sort#Icon| the supporting procedures 'sortop', and 'demosort' in Bubble Sort]]. The full demosort exercises the named sort of a list with op = "numeric", "string", ">>" (lexically gt, descending),">" (numerically gt, descending), a custom comparator, and also a string.

{{out}} Abbreviated 

```txt
Sorting Demo using procedure quicksort
  on list : [ 3 14 1 5 9 2 6 3 ]
    with op = &null:         [ 1 2 3 3 5 6 9 14 ]   (0 ms)
  ...
  on string : "qwerty"
    with op = &null:         "eqrtwy"   (0 ms)
```



## Io


```io
List do(
    quickSort := method(
        if(size > 1) then(
            pivot := at(size / 2 floor)
            return select(x, x < pivot) quickSort appendSeq(
                select(x, x == pivot) appendSeq(select(x, x > pivot) quickSort)
            )
        ) else(return self)
    )

    quickSortInPlace := method(
        copy(quickSort)
    )
)

lst := list(5, -1, -4, 2, 9)
lst quickSort println # ==> list(-4, -1, 2, 5, 9)
lst quickSortInPlace println # ==> list(-4, -1, 2, 5, 9)
```

Another more low-level Quicksort implementation can be found in Io's [[http://github.com/stevedekorte/io/blob/master/samples/misc/qsort.io github ]] repository.


## J

{{eff note|J|/:~}}

```j
sel=: 1 : 'x # ['

quicksort=: 3 : 0
 if.
  1 >: #y
 do.
  y
 else.
  e=. y{~?#y
  (quicksort y <sel e),(y =sel e),quicksort y >sel e
 end.
)
```


See the [[j:Essays/Quicksort|Quicksort essay]] in the J Wiki
for additional explanations and examples.


## Java



###  Imperative 

{{works with|Java|1.5+}}

{{trans|Python}}


```java5>public static <E extends Comparable<? super E>> List<E
 quickSort(List<E> arr) {
    if (arr.isEmpty())
        return arr;
    else {
        E pivot = arr.get(0);

        List<E> less = new LinkedList<E>();
        List<E> pivotList = new LinkedList<E>();
        List<E> more = new LinkedList<E>();

        // Partition
        for (E i: arr) {
            if (i.compareTo(pivot) < 0)
                less.add(i);
            else if (i.compareTo(pivot) > 0)
                more.add(i);
            else
                pivotList.add(i);
        }

        // Recursively sort sublists
        less = quickSort(less);
        more = quickSort(more);

        // Concatenate results
        less.addAll(pivotList);
        less.addAll(more);
        return less;
    }
}

```



###  Functional 

{{works with|Java|1.8}}


```java5>public static <E extends Comparable<E>> List<E
 sort(List<E> col) {
    if (col == null || col.isEmpty())
        return Collections.emptyList();
    else {
        E pivot = col.get(0);
        Map<Integer, List<E>> grouped = col.stream()
                .collect(Collectors.groupingBy(pivot::compareTo));
        return Stream.of(sort(grouped.get(1)), grouped.get(0), sort(grouped.get(-1)))
                .flatMap(Collection::stream).collect(Collectors.toList());
    }
}
```



## JavaScript



### Imperative



```javascript
function sort(array, less) {

  function swap(i, j) {
    var t = array[i];
    array[i] = array[j];
    array[j] = t;
  }

  function quicksort(left, right) {

    if (left < right) {
      var pivot = array[left + Math.floor((right - left) / 2)],
          left_new = left,
          right_new = right;

      do {
        while (less(array[left_new], pivot)) {
          left_new += 1;
        }
        while (less(pivot, array[right_new])) {
          right_new -= 1;
        }
        if (left_new <= right_new) {
          swap(left_new, right_new);
          left_new += 1;
          right_new -= 1;
        }
      } while (left_new <= right_new);

      quicksort(left, right_new);
      quicksort(left_new, right);

    }
  }

  quicksort(0, array.length - 1);

  return array;
}
```


Example:
```javascript
var test_array = [10, 3, 11, 15, 19, 1];
var sorted_array = sort(test_array, function(a,b) { return a<b; });
```


{{Out}}
```javascript
[ 1, 3, 10, 11, 15, 19 ]
```



### Functional




### =ES5=


Emphasising clarity more than run-time optimisation (for which Array.sort() would be a better option)


```JavaScript
(function () {
    'use strict';

    // quickSort :: (Ord a) => [a] -> [a]  
    function quickSort(xs) {

        if (xs.length) {
            var h = xs[0],
                t = xs.slice(1),

                lessMore = partition(function (x) {
                    return x <= h;
                }, t),
                less = lessMore[0],
                more = lessMore[1];

            return [].concat.apply(
                [], [quickSort(less), h, quickSort(more)]
            );

        } else return [];
    }


    // partition :: Predicate -> List -> (Matches, nonMatches)
    // partition :: (a -> Bool) -> [a] -> ([a], [a])
    function partition(p, xs) {
        return xs.reduce(function (a, x) {
            return (
                a[p(x) ? 0 : 1].push(x),
                a
            );
        }, [[], []]);
    }

    return quickSort([11.8, 14.1, 21.3, 8.5, 16.7, 5.7])

})();
```


{{Out}}


```txt
[5.7, 8.5, 11.8, 14.1, 16.7, 21.3]
```



### =ES6=



```javascript
Array.prototype.quick_sort = function () {
    if (this.length < 2) { return this; }

    var pivot = this[Math.round(this.length / 2)];

    return this.filter(x => x <  pivot)
               .quick_sort()
               .concat(this.filter(x => x == pivot))
               .concat(this.filter(x => x >  pivot).quick_sort());
};
```



Or, expressed in terms of a single partition, rather than two consecutive filters:


```JavaScript
(() => {
    'use strict';

    // QUICKSORT --------------------------------------------------------------

    // quickSort :: (Ord a) => [a] -> [a]
    const quickSort = xs =>
        xs.length > 1 ? (() => {
            const
                h = xs[0],
                [less, more] = partition(x => x <= h, xs.slice(1));
            return [].concat.apply(
                [], [quickSort(less), h, quickSort(more)]
            );
        })() : xs;


    // GENERIC ----------------------------------------------------------------

    // partition :: Predicate -> List -> (Matches, nonMatches)
    // partition :: (a -> Bool) -> [a] -> ([a], [a])
    const partition = (p, xs) =>
        xs.reduce((a, x) =>
            p(x) ? [a[0].concat(x), a[1]] : [a[0], a[1].concat(x)], [
                [],
                []
            ]);

    // TEST -------------------------------------------------------------------
    return quickSort([11.8, 14.1, 21.3, 8.5, 16.7, 5.7]);
})();
```

{{Out}}

```txt
[5.7, 8.5, 11.8, 14.1, 16.7, 21.3]
```



## Joy


```joy

DEFINE qsort ==
  [small]            # termination condition: 0 or 1 element
  []                 # do nothing
  [uncons [>] split] # pivot and two lists
  [enconcat]         # insert the pivot after the recursion
  binrec.            # recursion on the two lists

```



## jq

jq's built-in <tt>sort</tt> currently (version 1.4) uses the standard C qsort, a quicksort.  <tt>sort</tt> can be used on any valid JSON array.

Example:
```jq
[1, 1.1, [1,2], true, false, null, {"a":1}, null] | sort
```
{{Out}}
```jq
[null,null,false,true,1,1.1,[1,2],{"a":1}]
```


Here is an implementation in jq of the pseudo-code (and comments :-) given at the head of this article:
```jq
def quicksort:
  if length < 2 then .                            # it is already sorted
  else .[0] as $pivot
       | reduce .[] as $x
         # state: [less, equal, greater]
           ( [ [], [], [] ];                      # three empty arrays:
             if   $x  < $pivot then .[0] += [$x]  # add x to less
             elif $x == $pivot then .[1] += [$x]  # add x to equal
             else                   .[2] += [$x]  # add x to greater
             end
         )
       | (.[0] | quicksort ) + .[1] + (.[2] | quicksort )
  end ;

```
Fortunately, the example input used above produces the same output, 
and so both are omitted here.


## Julia

Built-in function for in-place sorting via quicksort (the [https://github.com/JuliaLang/julia/blob/2364748377f2a79c0485fdd5155ec2116c9f0d37/base/sort.jl#L259-L296 code from the standard library is quite readable]):

```julia
sort!(A, alg=QuickSort)
```

A simple polymorphic implementation of an in-place recursive quicksort (based on the pseudocode above):

```julia
function quicksort!(A,i=1,j=length(A))
    if j > i
        pivot = A[rand(i:j)] # random element of A
        left, right = i, j
        while left <= right
            while A[left] < pivot
                left += 1
            end
            while A[right] > pivot
                right -= 1
            end
            if left <= right
                A[left], A[right] = A[right], A[left]
                left += 1
                right -= 1
            end
        end
        quicksort!(A,i,right)
        quicksort!(A,left,j)
    end
    return A
end
```

A one-line (but rather inefficient) implementation based on the Haskell version, which operates out-of-place and allocates temporary arrays:

```julia
qsort(L) = isempty(L) ? L : vcat(qsort(filter(x -> x < L[1], L[2:end])), L[1:1], qsort(filter(x -> x >= L[1], L[2:end])))
```

{{out}}

```txt
julia> A = [84,77,20,60,47,20,18,97,41,49,31,39,73,68,65,52,1,92,15,9]

julia> qsort(A)
[1,9,15,18,20,20,31,39,41,47,49,52,60,65,68,73,77,84,92,97]

julia> quicksort!(copy(A))
[1,9,15,18,20,20,31,39,41,47,49,52,60,65,68,73,77,84,92,97]

julia> qsort(A) == quicksort!(copy(A)) == sort(A) == sort(A, alg=QuickSort)
true
```



## K


```K
quicksort:{f:*x@1?#x;:[0=#x;x;,/(_f x@&x<f;x@&x=f;_f x@&x>f)]}
```


Example:

```K

    quicksort 1 3 5 7 9 8 6 4 2

```


{{out}}

```txt

1 2 3 4 5 6 7 8 9

```



Explanation:
 

```K

  _f()

```


is the current function called recursively.


```K

   :[....] 

```


generally means :[condition1;then1;condition2;then2;....;else]. Though
here it is used as :[if;then;else].

This construct


```K

   f:*x@1?#x

```


assigns a random element in x (the argument) to f, as the pivot value.

And here is the full if/then/else clause:


```K

    :[
        0=#x;           / if length of x is zero 
        x;              / then return x
                        / else
        ,/(             / join the results of: 
          _f x@&x<f         / sort (recursively) elements less than f (pivot)
          x@&x=f            / element equal to f 
          _f x@&x>f)        / sort (recursively) elements greater than f 
     ]

```


Though - as with APL and J - for larger arrays it's much faster to 
sort using "<" (grade up) which gives the indices of the 
list sorted ascending, i.e.


```K

   t@<t:1 3 5 7 9 8 6 4 2

```



## Kotlin


```scala
import java.util.*
import java.util.Comparator

fun <T> quickSort(a: List<T>, c: Comparator<T>): ArrayList<T> {
    if (a.isEmpty()) return ArrayList(a)

    val boxes = Array(3, { ArrayList<T>() })
    fun normalise(i: Int) = i / Math.max(1, Math.abs(i))
    a.forEach { boxes[normalise(c.compare(it, a[0])) + 1].add(it) }
    arrayOf(0, 2).forEach { boxes[it] = quickSort(boxes[it], c) }
    return boxes.flatMapTo(ArrayList<T>()) { it }
}
```


Another version of the code:


```scala>fun <T : Comparable<T>
 quicksort(list: List<T>): List<T> {
    if (list.isEmpty()) return emptyList()

    val head = list.first()
    val tail = list.takeLast(list.size - 1)

    val (less, high) = tail.partition { it < head }

    return less + head + high
}

fun main(args: Array<String>) {
    val nums = listOf(9, 7, 9, 8, 1, 2, 3, 4, 1, 9, 8, 9, 2, 4, 2, 4, 6, 3)
    println(quicksort(nums))
}
```



## Lobster


```lobster
include "std.lobster"

def quicksort(xs, lt):
    if xs.length <= 1:
        xs
    else:
        pivot := xs[0]
        tail := xs.slice(1, -1)
        f1 := filter tail:  lt(_, pivot)
        f2 := filter tail: !lt(_, pivot)
        append(append(quicksort(f1, lt), [ pivot ]),
                      quicksort(f2, lt))

sorted := [ 3, 9, 5, 4, 1, 3, 9, 5, 4, 1 ].quicksort(): _a < _b
print sorted
```



## Logo


```logo
; quicksort (lists, functional)

to small? :list
  output or [empty? :list] [empty? butfirst :list]
end
to quicksort :list
  if small? :list [output :list]
  localmake "pivot first :list
  output (sentence
    quicksort filter [? < :pivot] butfirst :list
              filter [? = :pivot]          :list
    quicksort filter [? > :pivot] butfirst :list
  )
end

show quicksort [1 3 5 7 9 8 6 4 2]
```


```logo
; quicksort (arrays, in-place)

to incr :name
  make :name (thing :name) + 1
end
to decr :name
  make :name (thing :name) - 1
end
to swap :i :j :a
  localmake "t item :i :a
  setitem :i :a item :j :a
  setitem :j :a :t
end

to quick :a :low :high
  if :high <= :low [stop]
  localmake "l :low
  localmake "h :high
  localmake "pivot item ashift (:l + :h) -1  :a
  do.while [
    while [(item :l :a) < :pivot] [incr "l]
    while [(item :h :a) > :pivot] [decr "h]
    if :l <= :h [swap :l :h :a  incr "l  decr "h]
  ] [:l <= :h]
  quick :a :low :h
  quick :a :l :high
end
to sort :a
  quick :a first :a count :a
end

make "test {1 3 5 7 9 8 6 4 2}
sort :test
show :test
```



## Logtalk


```logtalk
quicksort(List, Sorted) :-
    quicksort(List, [], Sorted).

quicksort([], Sorted, Sorted).
quicksort([Pivot| Rest], Acc, Sorted) :- 
    partition(Rest, Pivot, Smaller0, Bigger0),
    quicksort(Smaller0, [Pivot| Bigger], Sorted),
    quicksort(Bigger0, Acc, Bigger).

partition([], _, [], []).
partition([X| Xs], Pivot, Smalls, Bigs) :-
    (   X @< Pivot ->
        Smalls = [X| Rest],
        partition(Xs, Pivot, Rest, Bigs)
    ;   Bigs = [X| Rest],
        partition(Xs, Pivot, Smalls, Rest)
    ).
```



## Lua

NOTE: If you want to use quicksort in a Lua script, you don't need to implement it yourself. Just do: 
```txt
table.sort(tableName)
```

===in-place===

```lua
--in-place quicksort
function quicksort(t, start, endi)
  start, endi = start or 1, endi or #t
  --partition w.r.t. first element
  if(endi - start < 1) then return t end
  local pivot = start
  for i = start + 1, endi do
    if t[i] <= t[pivot] then
      if i == pivot + 1 then
        t[pivot],t[pivot+1] = t[pivot+1],t[pivot]
      else
        t[pivot],t[pivot+1],t[i] = t[i],t[pivot],t[pivot+1]
      end
      pivot = pivot + 1
    end
  end
  t = quicksort(t, start, pivot - 1)
  return quicksort(t, pivot + 1, endi)
end

--example
print(unpack(quicksort{5, 2, 7, 3, 4, 7, 1}))
```


===non in-place===

```lua
function quicksort(t)
  if #t<2 then return t end
  local pivot=t[1]
  local a,b,c={},{},{}
  for _,v in ipairs(t) do
    if     v<pivot then a[#a+1]=v
    elseif v>pivot then c[#c+1]=v
    else                b[#b+1]=v
    end
  end
  a=quicksort(a)
  c=quicksort(c)
  for _,v in ipairs(b) do a[#a+1]=v end
  for _,v in ipairs(c) do a[#a+1]=v end
  return a
end
```



## Lucid

[http://i.csc.uvic.ca/home/hei/lup/06.html]

```lucid
qsort(a) = if eof(first a) then a else follow(qsort(b0),qsort(b1)) fi
 where
    p = first a < a;
    b0 = a whenever p;
    b1 = a whenever not p;
    follow(x,y) = if xdone then y upon xdone else x fi
                    where
                       xdone = iseod x fby xdone or iseod x; 
                    end;
 end
```



## M2000 Interpreter


### Recursive calling Functions


```M2000 Interpreter

Module Checkit1 {
      Group Quick {
      Private:
            Function partition {
                     Read &A(), p, r
                     x = A(r)
                     i = p-1
                     For j=p to r-1 {
                         If .LE(A(j), x) Then {
                                i++
                                Swap A(i),A(j)
                             }
                      }
                      Swap A(i+1),A(r)
                     = i+1
                  }
      Public:
            LE=Lambda->Number<=Number
            Function quicksort {
                 Read &A(), p, r
                 If p < r Then {
                   q = .partition(&A(), p, r)
                   Call .quicksort(&A(), p, q - 1)
                   Call .quicksort(&A(), q + 1, r)
                }
            }
      }
      Dim A(10)<<Random(50, 100)
      Print A()
      Call Quick.quicksort(&A(), 0, Len(A())-1)
      Print A()
}
Checkit1

```



### Recursive calling Subs

Variables p, r, q removed from quicksort function. we use stack for values. Also Partition push to stack now. Works for string arrays too.

```M2000 Interpreter

Module Checkit2 {
      Class Quick {
      Private:
            partition=lambda-> {
                  Read &A(), p, r : i = p-1 : x=A(r)
                  For j=p to r-1 {If .LE(A(j), x) Then i++:Swap A(i),A(j)
                  } : Swap A(i+1), A(r) :  Push i+1
            }
      Public:
            LE=Lambda->Number<=Number
            Module ForStrings {
                  .partition<=lambda-> {
                        Read &A$(), p, r : i = p-1 : x$=A$(r)
                        For j=p to r-1 {If A$(j)<= x$ Then i++:Swap A$(i),A$(j)
                        } : Swap A$(i+1), A$(r) : Push i+1
                  }
            }
            Function quicksort (ref$) {
                  myQuick()
                  sub myQuick()
                        If Stackitem() >= stackitem(2) Then drop 2 : Exit Sub
                        Over 2, 2 : Call .partition(ref$) : Over : Shiftback  3, 2
                        myQuick(number,  number - 1)
                        myQuick( number + 1, number)
                  End Sub
             } 
      }
      Quick=Quick()
      Dim A(10)
      A(0):=57, 83, 74, 98, 51, 73, 85, 76, 65, 92
      Print A()
      Call Quick.quicksort(&A(), 0, Len(A())-1)
      Print A()
      Quick=Quick()
      Quick.ForStrings
      Dim A$()
      A$()=("one","two", "three","four", "five")
      Print A$()
      Call Quick.quicksort(&A$(), 0, Len(A$())-1)
      Print A$()
}
Checkit2

```


### Non Recursive

Partition function return two values (where we want q, and use it as q-1 an q+1 now Partition() return final q-1 and q+1_
Example include numeric array, array of arrays (we provide a lambda for comparison) and string array.

```M2000 Interpreter

Module Checkit3 {
      Class Quick {
      Private:
            partition=lambda-> {
                  Read &A(), p, r : i = p-1 : x=A(r)
                  For j=p to r-1 {If .LE(A(j), x) Then i++:Swap A(i),A(j)
                  } : Swap A(i+1), A(r) :  Push  i+2, i 
            }
      Public:
            LE=Lambda->Number<=Number
            Module ForStrings {
                  .partition<=lambda-> {
                        Read &A$(), p, r : i = p-1 : x$=A$(r)
                        For j=p to r-1 {If A$(j)<= x$ Then i++:Swap A$(i),A$(j)
                        } : Swap A$(i+1), A$(r) : Push i+2, i
                  }
            }
            Function quicksort {
                 Read ref$
                 {
                         loop : If Stackitem() >= Stackitem(2) Then Drop 2 : if  empty then {Break} else continue
                         over 2,2 : call .partition(ref$) :shift 3 
                 }
            }
      }
      Quick=Quick()
      Dim A(10)<<Random(50, 100)
      Print A()
      Call Quick.quicksort(&A(), 0, Len(A())-1)
      Print A()
      Quick=Quick()
      Function join$(a$()) {
            n=each(a$(), 1, -2)
            k$=""
            while n {
                  overwrite k$, ".", n^:=array$(n)
            }
            =k$
      }
      Stack New {
                  Data "1.3.6.1.4.1.11.2.17.19.3.4.0.4" , "1.3.6.1.4.1.11.2.17.19.3.4.0.1", "1.3.6.1.4.1.11150.3.4.0.1"
                  Data "1.3.6.1.4.1.11.2.17.19.3.4.0.10", "1.3.6.1.4.1.11.2.17.5.2.0.79", "1.3.6.1.4.1.11150.3.4.0"
                  Dim Base 0, arr(Stack.Size)
                  Link arr() to arr$()
                  i=0 : While not Empty {arr$(i)=piece$(letter$+".", ".") : i++ }
      }
      \\ change comparison function
      Quick.LE=lambda (a, b)->{
            Link a, b to a$(), b$()
             def i=-1
             do {
                   i++
             } until a$(i)="" or b$(i)="" or a$(i)<>b$(i)
             if b$(i)="" then =a$(i)="":exit
             if a$(i)="" then =true:exit
             =val(a$(i))<=val(b$(i))
      }
      Call Quick.quicksort(&arr(), 0, Len(arr())-1)
      For i=0 to len(arr())-1 {
            Print join$(arr(i))
      }
      \\ Fresh load
      Quick=Quick()
      Quick.ForStrings
      Dim A$()
      A$()=("one","two", "three","four", "five")
      Print A$()
      Call Quick.quicksort(&A$(), 0, Len(A$())-1)
      Print A$()
}
Checkit3

```



## M4


```M4
dnl  return the first element of a list when called in the funny way seen below
define(`arg1', `$1')dnl
dnl
dnl  append lists 1 and 2
define(`append',
   `ifelse(`$1',`()',
      `$2',
      `ifelse(`$2',`()',
         `$1',
         `substr($1,0,decr(len($1))),substr($2,1)')')')dnl
dnl
dnl  separate list 2 based on pivot 1, appending to left 3 and right 4,
dnl  until 2 is empty, and then combine the sort of left with pivot with
dnl  sort of right
define(`sep',
   `ifelse(`$2', `()',
      `append(append(quicksort($3),($1)),quicksort($4))',
      `ifelse(eval(arg1$2<=$1),1,
         `sep($1,(shift$2),append($3,(arg1$2)),$4)',
         `sep($1,(shift$2),$3,append($4,(arg1$2)))')')')dnl
dnl
dnl  pick first element of list 1 as pivot and separate based on that
define(`quicksort',
   `ifelse(`$1', `()',
      `()',
      `sep(arg1$1,(shift$1),`()',`()')')')dnl
dnl
quicksort((3,1,4,1,5,9))
```


{{out}}

```txt

(1,1,3,4,5,9)

```



## Maple


```Maple
swap := proc(arr, a, b)
	local temp := arr[a]:
	arr[a] := arr[b]:
	arr[b] := temp:
end proc:
quicksort := proc(arr, low, high)
	local pi:
	if (low < high) then
		pi := qpart(arr,low,high):
		quicksort(arr, low, pi-1):
		quicksort(arr, pi+1, high):
	end if:
end proc:
qpart := proc(arr, low, high)
	local i,j,pivot;
	pivot := arr[high]:
	i := low-1:
	for j from low to high-1 by 1 do
		if (arr[j] <= pivot) then
			i++:
			swap(arr, i, j):
		end if;
	end do;
	swap(arr, i+1, high):
	return (i+1):
end proc:
a:=Array([12,4,2,1,0]);
quicksort(a,1,5);
a;
```

{{Out|Output}}

```txt
[0, 1, 2, 4, 12]
```



## Mathematica



```Mathematica
QuickSort[x_List] := Module[{pivot},
  If[Length@x <= 1, Return[x]];
  pivot = RandomChoice@x;
  Flatten@{QuickSort[Cases[x, j_ /; j < pivot]], Cases[x, j_ /; j == pivot], QuickSort[Cases[x, j_ /; j > pivot]]}
  ]
```



```Mathematica
qsort[{}] = {};
qsort[{x_, xs___}] := Join[qsort@Select[{xs}, # <= x &], {x}, qsort@Select[{xs}, # > x &]];
```



```Mathematica
QuickSort[{}] := {}
QuickSort[list: {__}] := With[{pivot=RandomChoice[list]},
	Join[ <|1->{}, -1->{}|>, GroupBy[list,Order[#,pivot]&] ] // Catenate[ {QuickSort@#[1], #[0], QuickSort@#[-1]} ]&
]
```



## MATLAB


This implements the pseudo-code in the specification. The input can be either a row or column vector, but the returned vector will always be a row vector. This can be modified to operate on any built-in primitive or user defined class by replacing the "<=" and ">" comparisons with "le" and "gt" functions respectively. This is because operators can not be overloaded, but the functions that are equivalent to the operators can be overloaded in class definitions.

This should be placed in a file named ''quickSort.m''.

```Matlab
function sortedArray = quickSort(array)

    if numel(array) <= 1 %If the array has 1 element then it can't be sorted       
        sortedArray = array;
        return
    end
    
    pivot = array(end);
    array(end) = [];
        
    %Create two new arrays which contain the elements that are less than or
    %equal to the pivot called "less" and greater than the pivot called
    %"greater"
    less = array( array <= pivot );
    greater = array( array > pivot );
    
    %The sorted array is the concatenation of the sorted "less" array, the
    %pivot and the sorted "greater" array in that order
    sortedArray = [quickSort(less) pivot quickSort(greater)];
    
end
```


A slightly more vectorized version of the above code that removes the need for the ''less'' and ''greater'' arrays:

```Matlab
function sortedArray = quickSort(array)

    if numel(array) <= 1 %If the array has 1 element then it can't be sorted       
        sortedArray = array;
        return
    end
    
    pivot = array(end);
    array(end) = [];
    
    sortedArray = [quickSort( array(array <= pivot) ) pivot quickSort( array(array > pivot) )];
    
end
```


Sample usage:

```MATLAB
quickSort([4,3,7,-2,9,1])

ans =

    -2     1     3     4     7     9
```



## MAXScript


```maxscript
fn quickSort arr =
(
    less = #()
    pivotList = #()
    more = #()
    if arr.count <= 1 then
    (
        arr
    )
    else
    (
        pivot = arr[arr.count/2]
        for i in arr do
        (
            case of
            (
                (i < pivot):	(append less i)
                (i == pivot):	(append pivotList i)
                (i > pivot):	(append more i)
            )
        )
        less = quickSort less
        more = quickSort more
        less + pivotList + more
    )
)
a = #(4, 89, -3, 42, 5, 0, 2, 889)
a = quickSort a
```


=={{header|Modula-2}}==

The definition module exposes the interface.
This one uses the procedure variable feature to pass a caller defined compare callback function so that it can sort various simple and structured record types.

This Quicksort assumes that you are working with an an array of pointers to an arbitrary type and are not moving the record data itself but only the pointers. The M2 type "ADDRESS" is considered compatible with any pointer type.

The use of type ADDRESS here to achieve genericity is something of a chink the the normal strongly typed flavor of Modula-2. Unlike the other language types, "system" types such as ADDRESS or WORD must be imported explicity from the SYSTEM MODULE.
The ISO standard for the "Generic Modula-2" language extension provides genericity without the chink, but most compilers have not implemented this extension.


```Modula2
(*#####################*)
 DEFINITION MODULE QSORT; 
(*#####################*)      

FROM SYSTEM IMPORT ADDRESS;

TYPE CmpFuncPtrs = PROCEDURE(ADDRESS, ADDRESS):INTEGER;

 PROCEDURE QuickSortPtrs(VAR Array:ARRAY OF ADDRESS; N:CARDINAL;
                         Compare:CmpFuncPtrs);
END QSORT.
 
```


The implementation module is not visible to clients, so it may be changed without worry so long as it still implements the definition.

Sedgewick suggests that faster sorting will be achieved if you drop back to an insertion sort once the partitions get small.


```Modula2
(*##########################*)
 IMPLEMENTATION MODULE QSORT; 
(*##########################*)

FROM SYSTEM    IMPORT ADDRESS;

CONST SmallPartition  = 9;

(*
NOTE
        1.Reference on QuickSort: "Implementing Quicksort Programs", Robert
          Sedgewick, Communications of the ACM, Oct 78, v21 #10.
*)

(*
### ========================================================
*)
 PROCEDURE QuickSortPtrs(VAR Array:ARRAY OF ADDRESS; N:CARDINAL;
                         Compare:CmpFuncPtrs);
(*
### ========================================================
*)

         (*-----------------------------*)
          PROCEDURE Swap(VAR A,B:ADDRESS);
         (*-----------------------------*)

         VAR  temp :ADDRESS;

         BEGIN

         temp := A; A := B; B := temp;

         END Swap;

         (*-------------------------------*)
          PROCEDURE TstSwap(VAR A,B:ADDRESS);
         (*-------------------------------*)

         VAR  temp   :ADDRESS;

         BEGIN

         IF Compare(A,B) > 0 THEN
            temp := A; A := B; B := temp;
         END;

         END TstSwap;

         (*--------------*)
          PROCEDURE Isort;
         (*--------------*)
         (*
                 Insertion sort.
         *)

         VAR  i,j    :CARDINAL;
              temp   :ADDRESS;

         BEGIN

         IF N < 2 THEN RETURN END;

         FOR i := N-2 TO 0 BY -1 DO
            IF Compare(Array[i],Array[i+1]) > 0 THEN
               temp := Array[i];
               j := i+1;
               REPEAT
                  Array[j-1] := Array[j];
                  INC(j);
               UNTIL (j = N) OR (Compare(Array[j],temp) >= 0);
               Array[j-1] := temp;
            END;
         END;

         END Isort;

         (*----------------------------------*)
          PROCEDURE Quick(left,right:CARDINAL);
         (*----------------------------------*)

         VAR
              i,j,
              second     :CARDINAL;
              Partition  :ADDRESS;

         BEGIN

         IF right > left THEN
            i := left; j := right;

            Swap(Array[left],Array[(left+right) DIV 2]);

            second := left+1;                          (* insure 2nd element is in   *)
            TstSwap(Array[second], Array[right]);      (* the lower part, last elem  *)
            TstSwap(Array[left], Array[right]);        (* in the upper part          *)
            TstSwap(Array[second], Array[left]);       (* THUS, only one test is     *)
                                                       (* needed in repeat loops     *)
            Partition := Array[left];

            LOOP
               REPEAT INC(i) UNTIL Compare(Array[i],Partition) >= 0;
               REPEAT DEC(j) UNTIL Compare(Array[j],Partition) <= 0;
               IF j < i THEN
                  EXIT
               END;
               Swap(Array[i],Array[j]);
            END; (*loop*)
            Swap(Array[left],Array[j]);

            IF (j > 0) AND (j-1-left >= SmallPartition) THEN
               Quick(left,j-1);
            END;
            IF right-i >= SmallPartition THEN
               Quick(i,right);
            END;
         END;

         END Quick;

 BEGIN (* QuickSortPtrs --------------------------------------------------*)

IF N > SmallPartition THEN              (* won't work for 2 elements *)
   Quick(0,N-1);
END;

Isort;

END QuickSortPtrs;

END QSORT.

```


=={{header|Modula-3}}==
This code is taken from libm3, which is basically Modula-3's "standard library".  Note that this code uses Insertion sort when the array is less than 9 elements long.


```modula3
GENERIC INTERFACE ArraySort(Elem);

PROCEDURE Sort(VAR a: ARRAY OF Elem.T; cmp := Elem.Compare);

END ArraySort.
```



```modula3
GENERIC MODULE ArraySort (Elem);

PROCEDURE Sort (VAR a: ARRAY OF Elem.T;  cmp := Elem.Compare) =
  BEGIN
    QuickSort (a, 0, NUMBER (a), cmp);
    InsertionSort (a, 0, NUMBER (a), cmp);
  END Sort;

PROCEDURE QuickSort (VAR a: ARRAY OF Elem.T;  lo, hi: INTEGER;
                     cmp := Elem.Compare) =
  CONST CutOff = 9;
  VAR i, j: INTEGER;  key, tmp: Elem.T;
  BEGIN
    WHILE (hi - lo > CutOff) DO (* sort a[lo..hi) *)

      (* use median-of-3 to select a key *)
      i := (hi + lo) DIV 2;
      IF cmp (a[lo], a[i]) < 0 THEN
        IF cmp (a[i], a[hi-1]) < 0 THEN
          key := a[i];
        ELSIF cmp (a[lo], a[hi-1]) < 0 THEN
          key := a[hi-1];  a[hi-1] := a[i];  a[i] := key;
        ELSE
          key := a[lo];  a[lo] := a[hi-1];  a[hi-1] := a[i];  a[i] := key;
        END;
      ELSE (* a[lo] >= a[i] *)
        IF cmp (a[hi-1], a[i]) < 0 THEN
          key := a[i];  tmp := a[hi-1];  a[hi-1] := a[lo];  a[lo] := tmp;
        ELSIF cmp (a[lo], a[hi-1]) < 0 THEN
          key := a[lo];  a[lo] := a[i];  a[i] := key;
        ELSE
          key := a[hi-1];  a[hi-1] := a[lo];  a[lo] := a[i];  a[i] := key;
        END;
      END;

      (* partition the array *)
      i := lo+1;  j := hi-2;

      (* find the first hole *)
      WHILE cmp (a[j], key) > 0 DO DEC (j) END;
      tmp := a[j];
      DEC (j);

      LOOP
        IF (i > j) THEN EXIT END;

        WHILE i < hi AND cmp (a[i], key) < 0 DO INC (i) END;
        IF (i > j) THEN EXIT END;
        a[j+1] := a[i];
        INC (i);

        WHILE j > lo AND cmp (a[j], key) > 0 DO DEC (j) END;
        IF (i > j) THEN  IF (j = i-1) THEN  DEC (j)  END;  EXIT  END;
        a[i-1] := a[j];
        DEC (j);
      END;

      (* fill in the last hole *)
      a[j+1] := tmp;
      i := j+2;

      (* then, recursively sort the smaller subfile *)
      IF (i - lo < hi - i)
        THEN  QuickSort (a, lo, i-1, cmp);   lo := i;
        ELSE  QuickSort (a, i, hi, cmp);     hi := i-1;
      END;

    END; (* WHILE (hi-lo > CutOff) *)
  END QuickSort;

PROCEDURE InsertionSort (VAR a: ARRAY OF Elem.T;  lo, hi: INTEGER;
                         cmp := Elem.Compare) =
  VAR j: INTEGER;  key: Elem.T;
  BEGIN
    FOR i := lo+1 TO hi-1 DO
      key := a[i];
      j := i-1;
      WHILE (j >= lo) AND cmp (key, a[j]) < 0 DO
        a[j+1] := a[j];
        DEC (j);
      END;
      a[j+1] := key;
    END;
  END InsertionSort;

BEGIN
END ArraySort.
```


To use this generic code to sort an array of text, we create two files called TextSort.i3 and TextSort.m3, respectively.


```modula3
INTERFACE TextSort = ArraySort(Text) END TextSort.
```


```modula3
MODULE TextSort = ArraySort(Text) END TextSort.
```


Then, as an example:

```modula3
MODULE Main;

IMPORT IO, TextSort;

VAR arr := ARRAY [1..10] OF TEXT {"Foo", "bar", "!ooF", "Modula-3", "hickup", 
                                 "baz", "quuz", "Zeepf", "woo", "Rosetta Code"};

BEGIN
  TextSort.Sort(arr);
  FOR i := FIRST(arr) TO LAST(arr) DO
    IO.Put(arr[i] & "\n");
  END;
END Main.
```



## Mond


Implements the simple quicksort algorithm.


```Mond
fun quicksort( arr, cmp )
{
    if( arr.length() < 2 )
        return arr;
    
    if( !cmp )
        cmp = ( a, b ) -> a - b;
    
    var a = [ ], b = [ ];
    var pivot = arr[0];
    var len = arr.length();
    
    for( var i = 1; i < len; ++i )
    {
        var item = arr[i];
        
        if( cmp( item, pivot ) < cmp( pivot, item ) )
            a.add( item );
        else
            b.add( item );
    }
    
    a = quicksort( a, cmp );
    b = quicksort( b, cmp );
    
    a.add( pivot );
    
    foreach( var item in b )
        a.add( item );
    
    return a;
}
```


;Usage


```Mond
var array = [ 532, 16, 153, 3, 63.60, 925, 0.214 ];
var sorted = quicksort( array );

printLn( sorted );
```


{{out}}

```txt
[
  0.214,
  3,
  16,
  63.6,
  153,
  532,
  925
]
```



## MUMPS


Shows quicksort on a 16-element array.


```MUMPS

main 
 new collection,size
 set size=16
 set collection=size for i=0:1:size-1 set collection(i)=$random(size)
 write "Collection to sort:",!!
 zwrite collection ; This will only work on Intersystem's flavor of MUMPS
 do quicksort(.collection,0,collection-1)
 write:$$isSorted(.collection) !,"Collection is sorted:",!!
 zwrite collection  ; This will only work on Intersystem's flavor of MUMPS
 q
quicksort(array,low,high)
 if low<high do  
 . set pivot=$$partition(.array,low,high)
 . do quicksort(.array,low,pivot-1)
 . do quicksort(.array,pivot+1,high)
 q
partition(A,p,r)
 set pivot=A(r)
 set i=p-1
 for j=p:1:r-1 do  
 . i A(j)<=pivot do  
 . . set i=i+1
 . . set helper=A(j)
 . . set A(j)=A(i)
 . . set A(i)=helper
 set helper=A(r)
 set A(r)=A(i+1)
 set A(i+1)=helper
 quit i+1
isSorted(array)
 set sorted=1
 for i=0:1:array-2 do  quit:sorted=0
 . for j=i+1:1:array-1 do  quit:sorted=0
 . . set:array(i)>array(j) sorted=0
 quit sorted

```


;Usage


```MUMPS
 do main()
```


{{out}}

```txt

Collection to sort:

collection=16
collection(0)=4
collection(1)=0
collection(2)=6
collection(3)=14
collection(4)=4
collection(5)=0
collection(6)=10
collection(7)=5
collection(8)=11
collection(9)=4
collection(10)=12
collection(11)=9
collection(12)=13
collection(13)=4
collection(14)=14
collection(15)=0

Collection is sorted:

collection=16
collection(0)=0
collection(1)=0
collection(2)=0
collection(3)=4
collection(4)=4
collection(5)=4
collection(6)=4
collection(7)=5
collection(8)=6
collection(9)=9
collection(10)=10
collection(11)=11
collection(12)=12
collection(13)=13
collection(14)=14
collection(15)=14


```



## Nemerle

{{trans|Haskell}}
A little less clean and concise than Haskell, but essentially the same.

```Nemerle
using System;
using System.Console;
using Nemerle.Collections.NList;

module Quicksort
{
    Qsort[T] (x : list[T]) : list[T]
      where T : IComparable
    {
        |[]    => []
        |x::xs => Qsort($[y|y in xs, (y.CompareTo(x) < 0)]) + [x] + Qsort($[y|y in xs, (y.CompareTo(x) > 0)])
    }
    
    Main() : void
    {
        def empty = [];
        def single = [2];
        def several = [2, 6, 1, 7, 3, 9, 4];
        WriteLine(Qsort(empty));
        WriteLine(Qsort(single));
        WriteLine(Qsort(several));
    }
}
```



## NetRexx

This sample implements both the '''simple''' and '''in place''' algorithms as described in the task's description:

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

import java.util.List

placesList = [String -
    "UK  London",     "US  New York",   "US  Boston",     "US  Washington" -
  , "UK  Washington", "US  Birmingham", "UK  Birmingham", "UK  Boston"     -
]
lists = [ -
    placesList -
  , quickSortSimple(String[] Arrays.copyOf(placesList, placesList.length)) -
  , quickSortInplace(String[] Arrays.copyOf(placesList, placesList.length)) -
]

loop ln = 0 to lists.length - 1
  cl = lists[ln]
  loop ct = 0 to cl.length - 1
    say cl[ct]
    end ct
    say
  end ln

return

method quickSortSimple(array = String[]) public constant binary returns String[]

  rl = String[array.length]
  al = List quickSortSimple(Arrays.asList(array))
  al.toArray(rl)

  return rl

method quickSortSimple(array = List) public constant binary returns ArrayList

  if array.size > 1 then do
    less    = ArrayList()
    equal   = ArrayList()
    greater = ArrayList()

    pivot = array.get(Random().nextInt(array.size - 1))
    loop x_ = 0 to array.size - 1
      if (Comparable array.get(x_)).compareTo(Comparable pivot) < 0 then less.add(array.get(x_))
      if (Comparable array.get(x_)).compareTo(Comparable pivot) = 0 then equal.add(array.get(x_))
      if (Comparable array.get(x_)).compareTo(Comparable pivot) > 0 then greater.add(array.get(x_))
      end x_
    less    = quickSortSimple(less)
    greater = quickSortSimple(greater)
    out = ArrayList(array.size)
    out.addAll(less)
    out.addAll(equal)
    out.addAll(greater)

    array = out
    end

  return ArrayList array

method quickSortInplace(array = String[]) public constant binary returns String[]

  rl = String[array.length]
  al = List quickSortInplace(Arrays.asList(array))
  al.toArray(rl)

  return rl

method quickSortInplace(array = List, ixL = int 0, ixR = int array.size - 1) public constant binary returns ArrayList

  if ixL < ixR then do
    ixP = int ixL + (ixR - ixL) % 2
    ixP = quickSortInplacePartition(array, ixL, ixR, ixP)
    quickSortInplace(array, ixL, ixP - 1)
    quickSortInplace(array, ixP + 1, ixR)
    end

  array = ArrayList(array)
  return ArrayList array

method quickSortInplacePartition(array = List, ixL = int, ixR = int, ixP = int) public constant binary returns int

  pivotValue = array.get(ixP)
  rValue     = array.get(ixR)
  array.set(ixP, rValue)
  array.set(ixR, pivotValue)
  ixStore = ixL
  loop i_ = ixL to ixR - 1
    iValue = array.get(i_)
    if (Comparable iValue).compareTo(Comparable pivotValue) < 0 then do
      storeValue = array.get(ixStore)
      array.set(i_, storeValue)
      array.set(ixStore, iValue)
      ixStore = ixStore + 1
      end
    end i_
  storeValue = array.get(ixStore)
  rValue     = array.get(ixR)
  array.set(ixStore, rValue)
  array.set(ixR, storeValue)

  return ixStore

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

UK  Birmingham
UK  Boston
UK  London
UK  Washington
US  Birmingham
US  Boston
US  New York
US  Washington

```



## Nial



```nial>quicksort is fork [ 
= [1 first,tally],
  pass,
  link [
      quicksort sublist [ < [pass, first], pass ],
      sublist [ match [pass,first],pass ],
      quicksort sublist [ > [pass,first], pass ]
  ]
]
```


Using it.

```nial
|quicksort [5, 8, 7, 4, 3]
=3 4 5 7 8
```



## Nim


```nim

proc quickSort[T](a: var openarray[T], inl = 0, inr = -1) =
  var r = if inr >= 0: inr else: a.high
  var l = inl
  let n = r - l + 1
  if n < 2: return
  let p = a[l + 3 * n div 4]
  while l <= r:
    if a[l] < p:
      inc l
      continue
    if a[r] > p:
      dec r
      continue
    if l <= r:
      swap a[l], a[r]
      inc l
      dec r
  quickSort(a, inl, r)
  quickSort(a, l, inr)

var a = @[4, 65, 2, -31, 0, 99, 2, 83, 782]
quickSort a
echo a
```

{{out}}

```txt
@[-31, 0, 2, 2, 4, 65, 83, 99, 782]
```



## Nix


```nix

let
  qs = l:
    if l == [] then []
    else
      with builtins;
      let x  = head l;
          xs = tail l;
          low  = filter (a: a < x)  xs;
          high = filter (a: a >= x) xs;
      in qs low ++ [x] ++ qs high;
in
  qs [4 65 2 (-31) 0 99 83 782]

```

{{out}}

```txt
[ -31 0 2 4 65 83 99 782 ]
```



## Objeck


```objeck

class QuickSort {
  function : Main(args : String[]) ~ Nil {
    array := [1, 3, 5, 7, 9, 8, 6, 4, 2];
    Sort(array);
    each(i : array) {
      array[i]->PrintLine();
    };
  }

  function : Sort(array : Int[]) ~ Nil {
    size := array->Size();
    if(size <= 1) {
      return;
    };
    Sort(array, 0, size - 1);
  }

  function : native : Sort(array : Int[], low : Int, high : Int) ~ Nil {
    i := low; j := high;
    pivot := array[low + (high-low)/2];

    while(i <= j) {
      while(array[i] < pivot) {
        i+=1;
      };

      while(array[j] > pivot) {
        j-=1;
      };

      if (i <= j) {
        temp := array[i];
        array[i] := array[j];
        array[j] := temp;
        i+=1; j-=1;
      };
    };

    if(low < j) {
      Sort(array, low, j);
    };

    if(i < high) {
      Sort(array, i, high);
    };
  }
}

```


=={{header|Objective-C}}==
The [http://weblog.bignerdranch.com/398-objective-c-literals-part-1/ latest XCode compiler] is assumed with [http://en.wikipedia.org/wiki/Automatic_Reference_Counting ARC] enabled.

```objc
void quicksortInPlace(NSMutableArray *array, NSInteger first, NSInteger last, NSComparator comparator) {
    if (first >= last) return;
    id pivot = array[(first + last) / 2];
    NSInteger left = first;
    NSInteger right = last;
    while (left <= right) {
        while (comparator(array[left], pivot) == NSOrderedAscending)
            left++;
        while (comparator(array[right], pivot) == NSOrderedDescending)
            right--;
        if (left <= right)
            [array exchangeObjectAtIndex:left++ withObjectAtIndex:right--];
    }
    quicksortInPlace(array, first, right, comparator);
    quicksortInPlace(array, left, last, comparator);
}

NSArray* quicksort(NSArray *unsorted, NSComparator comparator) {
    NSMutableArray *a = [NSMutableArray arrayWithArray:unsorted];
    quicksortInPlace(a, 0, a.count - 1, comparator);
    return a;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray *a = @[ @1, @3, @5, @7, @9, @8, @6, @4, @2 ];
        NSLog(@"Unsorted: %@", a);
        NSLog(@"Sorted: %@", quicksort(a, ^(id x, id y) { return [x compare:y]; }));
        NSArray *b = @[ @"Emil", @"Peg", @"Helen", @"Juergen", @"David", @"Rick", @"Barb", @"Mike", @"Tom" ];
        NSLog(@"Unsorted: %@", b);
        NSLog(@"Sorted: %@", quicksort(b, ^(id x, id y) { return [x compare:y]; }));
    }
    return 0;
}
```

{{out}}

```txt
Unsorted: (
    1,
    3,
    5,
    7,
    9,
    8,
    6,
    4,
    2
)
Sorted: (
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9
)
Unsorted: (
    Emil,
    Peg,
    Helen,
    Juergen,
    David,
    Rick,
    Barb,
    Mike,
    Tom
)
Sorted: (
    Barb,
    David,
    Emil,
    Helen,
    Juergen,
    Mike,
    Peg,
    Rick,
    Tom
)
```



## OCaml


```ocaml
let rec quicksort gt = function
  | [] -> []
  | x::xs ->
      let ys, zs = List.partition (gt x) xs in
      (quicksort gt ys) @ (x :: (quicksort gt zs))
 
let _ =
  quicksort (>) [4; 65; 2; -31; 0; 99; 83; 782; 1]
```



## Octave

{{trans|MATLAB}} (The MATLAB version works as is in Octave, provided that the code is put in a file named <tt>quicksort.m</tt>, and everything below the <tt>return</tt> must be typed in the prompt of course)


```octave
function f=quicksort(v)                       % v must be a column vector
  f = v; n=length(v);
  if(n > 1)
     vl = min(f); vh = max(f);                  % min, max
     p  = (vl+vh)*0.5;                          % pivot
     ia = find(f < p); ib = find(f == p); ic=find(f > p);
     f  = [quicksort(f(ia)); f(ib); quicksort(f(ic))];
  end
endfunction
 
N=30; v=rand(N,1); tic,u=quicksort(v); toc
u
```



## Oforth


Oforth built-in sort uses quick sort algorithm (see lang/collect/ListBuffer.of for implementation) :


```Oforth
[ 5, 8, 2, 3, 4, 1 ] sort
```



## ooRexx

{{trans|Python}}

```ooRexx
    a = .array~Of(4, 65, 2, -31, 0, 99, 83, 782, 1)
    say 'before:' a~toString( ,', ')
    a = quickSort(a)
    say ' after:' a~toString( ,', ')
    exit

::routine quickSort
    use arg arr -- the array to be sorted
    less = .array~new
    pivotList = .array~new
    more = .array~new
    if arr~items <= 1 then
        return arr
    else do
        pivot = arr[1]
        do i over arr
            if i < pivot then
                less~append(i)
            else if i > pivot then
                more~append(i)
            else
                pivotList~append(i)
        end
        less = quickSort(less)
        more = quickSort(more)
        return less~~appendAll(pivotList)~~appendAll(more)
    end
```

{{out}}

```txt
before: 4, 65, 2, -31, 0, 99, 83, 782, 1
 after: -31, 0, 1, 2, 4, 65, 83, 99, 782 
```



## Oz


```oz
declare
  fun {QuickSort Xs}
     case Xs of nil then nil
     [] Pivot|Xr then
	fun {IsSmaller X} X < Pivot end
        Smaller Larger
     in
	{List.partition Xr IsSmaller ?Smaller ?Larger}
        {Append {QuickSort Smaller} Pivot|{QuickSort Larger}}
     end
  end
in
  {Show {QuickSort [3 1 4 1 5 9 2 6 5]}}
```



## PARI/GP


```parigp
quickSort(v)={
  if(#v<2, return(v));
  my(less=List(),more=List(),same=List(),pivot);
  pivot=median([v[random(#v)+1],v[random(#v)+1],v[random(#v)+1]]); \\ Middle-of-three
  for(i=1,#v,
    if(v[i]<pivot,
      listput(less, v[i]),
      if(v[i]==pivot, listput(same, v[i]), listput(more, v[i]))
    )
  );
  concat(quickSort(Vec(less)), concat(Vec(same), quickSort(Vec(more))))
};
median(v)={
  vecsort(v)[#v>>1]
};
```



## Pascal


```pascal

{ X is array of LongInt }
Procedure QuickSort ( Left, Right : LongInt );
Var 
  i, j,
  tmp, pivot : LongInt;         { tmp & pivot are the same type as the elements of array }
Begin
  i:=Left;
  j:=Right;
  pivot := X[(Left + Right) shr 1]; // pivot := X[(Left + Rigth) div 2] 
  Repeat
    While pivot > X[i] Do inc(i);   // i:=i+1;
    While pivot < X[j] Do dec(j);   // j:=j-1;
    If i<=j Then Begin
      tmp:=X[i];
      X[i]:=X[j];
      X[j]:=tmp;
      dec(j);   // j:=j-1;
      inc(i);   // i:=i+1;
    End;
  Until i>j;
  If Left<j Then QuickSort(Left,j);
  If i<Right Then QuickSort(i,Right);
End;

```



## Perl


```perl

sub quick_sort {
    return @_ if @_ < 2;
    my $p = splice @_, int rand @_, 1;
    quick_sort(grep $_ < $p, @_), $p, quick_sort(grep $_ >= $p, @_);
}

my @a = (4, 65, 2, -31, 0, 99, 83, 782, 1);
@a = quick_sort @a;
print "@a\n";

```



## Perl 6


```perl6
# Empty list sorts to the empty list
 multi quicksort([]) { () }
 
 # Otherwise, extract first item as pivot...
 multi quicksort([$pivot, *@rest]) {
     # Partition.
     my $before := @rest.grep(* before $pivot);
     my $after  := @rest.grep(* !before $pivot);
 
     # Sort the partitions.
     flat quicksort($before), $pivot, quicksort($after)
 }
```

Note that <code>$before</code> and <code>$after</code> are bound to lazy lists, so the partitions can (at least in theory) be sorted in parallel.


## Phix


```Phix
function quick_sort(sequence x)
--
-- put x into ascending order using recursive quick sort
--
integer n, last, mid
object xi, midval

    n = length(x)
    if n<2 then
        return x    -- already sorted (trivial case)
    end if

    mid = floor((n+1)/2)
    midval = x[mid]
    x[mid] = x[1]

    last = 1
    for i=2 to n do
        xi = x[i]
        if xi<midval then
            last += 1
            x[i] = x[last]
            x[last] = xi
        end if
    end for

    return quick_sort(x[2..last]) & {midval} & quick_sort(x[last+1..n])
end function

?quick_sort({5,"oranges","and",3,"apples"})
```

{{out}}

```txt

{3,5,"and","apples","oranges"}

```



## PHP


```php
function quicksort($arr){
	$lte = $gt = array();
	if(count($arr) < 2){
		return $arr;
	}
	$pivot_key = key($arr);
	$pivot = array_shift($arr);
	foreach($arr as $val){
		if($val <= $pivot){
			$lte[] = $val;
		} else {
			$gt[] = $val;
		}
	}
	return array_merge(quicksort($lte),array($pivot_key=>$pivot),quicksort($gt));
}

$arr = array(1, 3, 5, 7, 9, 8, 6, 4, 2);
$arr = quicksort($arr);
echo implode(',',$arr);
```


```txt
1,2,3,4,5,6,7,8,9
```



```php

function quickSort(array $array) {
    // base case
    if (empty($array)) {
        return $array;
    }
    $head = array_shift($array);
    $tail = $array;
    $lesser = array_filter($tail, function ($item) use ($head) {
        return $item <= $head;
    });
    $bigger = array_filter($tail, function ($item) use ($head) {
        return $item > $head;
    });
    return array_merge(quickSort($lesser), [$head], quickSort($bigger));
}
$testCase = [1, 4, 8, 2, 8, 0, 2, 8];
$result = quickSort($testCase);
echo sprintf("[%s] ==> [%s]\n", implode(', ', $testCase), implode(', ', $result));

```


```txt
[1, 4, 8, 2, 8, 0, 2, 8] ==> [0, 1, 2, 2, 4, 8, 8, 8]
```



## PicoLisp


```lisp
(de quicksort (L)
   (if (cdr L)
      (let Pivot (car L)
          (append (quicksort (filter '((A) (< A Pivot)) (cdr L)))
                             (filter '((A) (= A Pivot))      L )
                  (quicksort (filter '((A) (> A Pivot)) (cdr L)))) )
      L) )
```



## PL/I


```pli
DCL (T(20)) FIXED BIN(31);   /* scratch space of length N */

QUICKSORT: PROCEDURE (A,AMIN,AMAX,N) RECURSIVE ;
   DECLARE (A(*))              FIXED BIN(31);
   DECLARE (N,AMIN,AMAX)       FIXED BIN(31) NONASGN;
   DECLARE (I,J,IA,IB,IC,PIV)  FIXED BIN(31);
   DECLARE (P,Q)               POINTER;
   DECLARE (AP(1))             FIXED BIN(31) BASED(P);
   
   IF(N <= 1)THEN RETURN;
   IA=0; IB=0; IC=N+1;
   PIV=(AMIN+AMAX)/2;
   DO I=1 TO N;
      IF(A(I) < PIV)THEN DO;
         IA+=1; A(IA)=A(I);
      END; ELSE IF(A(I) > PIV) THEN DO;
         IC-=1; T(IC)=A(I);
      END; ELSE DO;
         IB+=1; T(IB)=A(I);
      END;
   END;
   DO I=1  TO IB; A(I+IA)=T(I);   END;
   DO I=IC TO N;  A(I)=T(N+IC-I); END;
   P=ADDR(A(IC));
   IC=N+1-IC;
   IF(IA > 1) THEN CALL QUICKSORT(A, AMIN, PIV-1,IA);
   IF(IC > 1) THEN CALL QUICKSORT(AP,PIV+1,AMAX, IC);
   RETURN;
END QUICKSORT;
 MINMAX: PROC(A,AMIN,AMAX,N);
   DCL (AMIN,AMAX) FIXED BIN(31),
       (N,A(*))    FIXED BIN(31) NONASGN ;
   DCL (I,X,Y) FIXED BIN(31);
   
   AMIN=A(N); AMAX=AMIN;
   DO I=1 TO N-1;
      X=A(I); Y=A(I+1);
      IF (X < Y)THEN DO;
         IF (X < AMIN) THEN AMIN=X;
         IF (Y > AMAX) THEN AMAX=Y;
       END; ELSE DO;
          IF (X > AMAX) THEN AMAX=X;
          IF (Y < AMIN) THEN AMIN=Y;
       END;
   END;
   RETURN;
END MINMAX;
CALL MINMAX(A,AMIN,AMAX,N);
CALL QUICKSORT(A,AMIN,AMAX,N);
```



## PowerShell



### First solution


```PowerShell
Function SortThree( [Array] $data )
{
	if( $data[ 0 ] -gt $data[ 1 ] )
	{
		if( $data[ 0 ] -lt $data[ 2 ] )
		{
			$data = $data[ 1, 0, 2 ]
		} elseif ( $data[ 1 ] -lt $data[ 2 ] ){
			$data = $data[ 1, 2, 0 ]
		} else {
			$data = $data[ 2, 1, 0 ]
		}
	} else {
		if( $data[ 0 ] -gt $data[ 2 ] )
		{
			$data = $data[ 2, 0, 1 ]
		} elseif( $data[ 1 ] -gt $data[ 2 ] ) {
			$data = $data[ 0, 2, 1 ]
		}
	}
	$data
}

Function QuickSort( [Array] $data, $rand = ( New-Object Random ) )
{
	$datal = $data.length
	if( $datal -gt 3 )
	{
		[void] $datal--
		$median = ( SortThree $data[ 0, ( $rand.Next( 1, $datal - 1 ) ), -1 ] )[ 1 ]
		$lt = @()
		$eq = @()
		$gt = @()
		$data | ForEach-Object { if( $_ -lt $median ) { $lt += $_ } elseif( $_ -eq $median ) { $eq += $_ } else { $gt += $_ } }
		$lt = ( QuickSort $lt $rand )
		$gt = ( QuickSort $gt $rand )
		$data = @($lt) + $eq + $gt
	} elseif( $datal -eq 3 ) {
		$data = SortThree( $data )
	} elseif( $datal -eq 2 ) {
		if( $data[ 0 ] -gt $data[ 1 ] )
		{
			$data = $data[ 1, 0 ]
		}
	}
	$data
}

QuickSort 5,3,1,2,4 
QuickSort 'e','c','a','b','d' 
QuickSort 0.5,0.3,0.1,0.2,0.4 
$l = 100; QuickSort ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( 0, $l - 1 ) } )
```




### Another solution


```powershell

function quicksort($array) {
    $less, $equal, $greater = @(), @(), @()
    if( $array.Count -gt 1 ) { 
        $pivot = $array[0]
        foreach( $x in $array) {
            if($x -lt $pivot) { $less += @($x) }
            elseif ($x -eq $pivot) { $equal += @($x)}
            else { $greater += @($x) }
        }    
        $array = (@(quicksort $less) + @($equal) + @(quicksort $greater))
    }
    $array
}
$array = @(60, 21, 19, 36, 63, 8, 100, 80, 3, 87, 11)
"$(quicksort $array)"

```


```txt
The output is: 3 8 11 19 21 36 60 63 80 87 100
```



## Prolog


```prolog
qsort( [], [] ).
qsort( [H|U], S ) :- splitBy(H, U, L, R), qsort(L, SL), qsort(R, SR), append(SL, [H|SR], S).

% splitBy( H, U, LS, RS )
% True if LS = { L in U | L <= H }; RS = { R in U | R > H }
splitBy( _, [], [], []).
splitBy( H, [U|T], [U|LS], RS ) :- U =< H, splitBy(H, T, LS, RS).
splitBy( H, [U|T], LS, [U|RS] ) :- U  > H, splitBy(H, T, LS, RS).

```



## PureBasic


```PureBasic
Procedure qSort(Array a(1), firstIndex, lastIndex)
  Protected  low, high, pivotValue

  low = firstIndex
  high = lastIndex
  pivotValue = a((firstIndex + lastIndex) / 2)
  
  Repeat
    
    While a(low) < pivotValue
      low + 1
    Wend
    
    While a(high) > pivotValue
      high - 1
    Wend
    
    If low <= high
      Swap a(low), a(high)
      low + 1
      high - 1
    EndIf
    
  Until low > high
  
  If firstIndex < high
    qSort(a(), firstIndex, high)
  EndIf
  
  If low < lastIndex
    qSort(a(), low, lastIndex)
  EndIf
EndProcedure

Procedure quickSort(Array a(1))
  qSort(a(),0,ArraySize(a()))
EndProcedure
```



## Python


```python
def quickSort(arr):
    less = []
    pivotList = []
    more = []
    if len(arr) <= 1:
        return arr
    else:
        pivot = arr[0]
        for i in arr:
            if i < pivot:
                less.append(i)
            elif i > pivot:
                more.append(i)
            else:
                pivotList.append(i)
        less = quickSort(less)
        more = quickSort(more)
        return less + pivotList + more

a = [4, 65, 2, -31, 0, 99, 83, 782, 1]
a = quickSort(a)
```


In a Haskell fashion --

```python
def qsort(L):
    return (qsort([y for y in L[1:] if y <  L[0]]) + 
            L[:1] + 
            qsort([y for y in L[1:] if y >= L[0]])) if len(L) > 1 else L
```


More readable, but still using list comprehensions:

```python
def qsort(list):
    if not list:
        return []
    else:
        pivot = list[0]
        less = [x for x in list     if x <  pivot]
        more = [x for x in list[1:] if x >= pivot]
        return qsort(less) + [pivot] + qsort(more)
```


More correctly in some tests:

```python
from random import *

def qSort(a):
    if len(a) <= 1:
        return a
    else:
        q = choice(a)
        return qSort([elem for elem in a if elem < q]) + [q] * a.count(q) + qSort([elem for elem in a if elem > q])
```




```python
def quickSort(a):
    if len(a) <= 1:
        return a
    else:
        less = []
        more = []
        pivot = choice(a)
        for i in a:
            if i < pivot:
                less.append(i)
            if i > pivot:
                more.append(i)
        less = quickSort(less)
        more = quickSort(more)
        return less + [pivot] * a.count(pivot) + more
```


Returning a new list:


```python
def qsort(array):
    if len(array) < 2:
        return array
    head, *tail = array
    less = qsort([i for i in tail if i < head])
    more = qsort([i for i in tail if i >= head])
    return less + [head] + more
```


Sorting a list in place:


```python
def quicksort(array):
    _quicksort(array, 0, len(array) - 1)

def _quicksort(array, start, stop):
    if stop - start > 0:
        pivot, left, right = array[start], start, stop
        while left <= right:
            while array[left] < pivot:
                left += 1
            while array[right] > pivot:
                right -= 1
            if left <= right:
                array[left], array[right] = array[right], array[left]
                left += 1
                right -= 1
        _quicksort(array, start, right)
        _quicksort(array, left, stop)
```



## Qi


```Qi
(define keep
  _    []       -> []
  Pred [A|Rest] -> [A | (keep Pred Rest)] where (Pred A)
  Pred [_|Rest] -> (keep Pred Rest))

(define quicksort
  []    -> []
  [A|R] -> (append (quicksort (keep (>= A) R))
                   [A]
                   (quicksort (keep (< A) R))))

(quicksort [6 8 5 9 3 2 2 1 4 7])

```



## R

{{trans|Octave}}

```R
qsort <- function(v) {
  if ( length(v) > 1 ) 
  {
    pivot <- (min(v) + max(v))/2.0                            # Could also use pivot <- median(v)
    c(qsort(v[v < pivot]), v[v == pivot], qsort(v[v > pivot])) 
  } else v
}

N <- 100
vs <- runif(N)
system.time(u <- qsort(vs))
print(u)
```



## Racket


```Racket
#lang racket
(define (quicksort < l)
  (match l
    ['() '()]
    [(cons x xs) 
     (let-values ([(xs-gte xs-lt) (partition (curry < x) xs)])
       (append (quicksort < xs-lt) 
               (list x) 
               (quicksort < xs-gte)))]))
```


Examples


```Racket
(quicksort < '(8 7 3 6 4 5 2))
;returns '(2 3 4 5 6 7 8)
(quicksort string<? '("Mergesort" "Quicksort" "Bubblesort"))
;returns '("Bubblesort" "Mergesort" "Quicksort")
```



## Red


```Red

Red []

;;-------------------------------
;; we have to use function not func here, otherwise we'd have to define all "vars" as local...
qsort: function [list][
;;-------------------------------
  if 1 >= length? list [  return list ]
  left: copy [] 
  right: copy []
  eq:   copy []  ;; "equal"
  pivot: list/2 ;; simply choose second element as pivot element
  foreach ele list [
      case [
       ele < pivot [ append left ele ]
       ele > pivot [ append right ele ]
       true       [append eq ele ]
      ]
  ]
  ;; this is the last expression of the function, so coding "return" here is not necessary
  reduce [qsort left eq qsort right]
]


;; lets test the function with an array of 100k integers, range 1..1000  
list: []
loop 100000 [append list random 1000]
t0: now/time/precise  ;; start timestamp
qsort list ;; the return value (block) contains the sorted list, original list has not changed
print ["time1: "  now/time/precise   - t0]  ;; about 1.1 sec on my machine
t0: now/time/precise  
sort list  ;; just for fun time the builtin function also ( also implementation of quicksort ) 
print ["time2: " now/time/precise   - t0]

```



## REXX


### version 1


```rexx
/*REXX program  sorts  a  stemmed array  using the   quicksort  algorithm.              */
call gen@                                        /*generate the elements for the array. */
call show@   'before sort'                       /*show  the  before   array elements.  */
call qSort       #                               /*invoke the  quicksort  subroutine.   */
call show@   ' after sort'                       /*show  the   after   array elements.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
qSort: procedure expose @.; a.1=1; parse arg b.1 /*access the caller's local variable.  */
       $=1
               do  while  $\==0;    L=a.$;     t=b.$;     $=$-1;      if t<2  then iterate
               H=L+t-1;             ?=L+t%2
               if @.H<@.L  then if @.?<@.H  then do;  p=@.H;  @.H=@.L;  end
                                            else if @.?>@.L  then p=@.L
                                                             else do;  p=@.?; @.?=@.L; end
                           else if @.?<@.L  then p=@.L
                                            else if @.?>@.H  then do;  p=@.H; @.H=@.L; end
                                                             else do;  p=@.?; @.?=@.L; end
               j=L+1;                             k=h
                      do forever
                          do j=j         while j<=k & @.j<=p;  end  /*a tinie─tiny loop.*/
                          do k=k  by -1  while j <k & @.k>=p;  end  /*another   "    "  */
                      if j>=k  then leave                           /*segment finished? */
                      _=@.j;   @.j=@.k;   @.k=_                     /*swap J&K elements.*/
                      end   /*forever*/
               $=$+1
               k=j-1;   @.L=@.k;   @.k=p
               if j<=?  then do;   a.$=j;   b.$=H-j+1;   $=$+1;   a.$=L;   b.$=k-L;    end
                        else do;   a.$=L;   b.$=k-L;     $=$+1;   a.$=j;   b.$=H-j+1;  end
               end          /*while $¬==0*/
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show@: w=length(#);        do j=1  for #;  say 'element'  right(j,w)  arg(1)":"  @.j;  end
       say copies('▒', maxL + w + 22)            /*display a separator (between outputs)*/
       return
/*───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
gen@:  @.=;   maxL=0                             /*assign a default value for the array.*/
@.1  = " Rivers that form part of a (USA) state's border "                                   /*this value is adjusted later to include a prefix & suffix.*/
@.2  = '='                                                                                   /*this value is expanded later.  */
@.3  = "Perdido River                       Alabama, Florida"
@.4  = "Chattahoochee River                 Alabama, Georgia"
@.5  = "Tennessee River                     Alabama, Kentucky, Mississippi, Tennessee"
@.6  = "Colorado River                      Arizona, California, Nevada, Baja California (Mexico)"
@.7  = "Mississippi River                   Arkansas, Illinois, Iowa, Kentucky, Minnesota, Mississippi, Missouri, Tennessee, Louisiana, Wisconsin"
@.8  = "St. Francis River                   Arkansas, Missouri"
@.9  = "Poteau River                        Arkansas, Oklahoma"
@.10 = "Arkansas River                      Arkansas, Oklahoma"
@.11 = "Red River (Mississippi watershed)   Arkansas, Oklahoma, Texas"
@.12 = "Byram River                         Connecticut, New York"
@.13 = "Pawcatuck River                     Connecticut, Rhode Island and Providence Plantations"
@.14 = "Delaware River                      Delaware, New Jersey, New York, Pennsylvania"
@.15 = "Potomac River                       District of Columbia, Maryland, Virginia, West Virginia"
@.16 = "St. Marys River                     Florida, Georgia"
@.17 = "Chattooga River                     Georgia, South Carolina"
@.18 = "Tugaloo River                       Georgia, South Carolina"
@.19 = "Savannah River                      Georgia, South Carolina"
@.20 = "Snake River                         Idaho, Oregon, Washington"
@.21 = "Wabash River                        Illinois, Indiana"
@.22 = "Ohio River                          Illinois, Indiana, Kentucky, Ohio, West Virginia"
@.23 = "Great Miami River (mouth only)      Indiana, Ohio"
@.24 = "Des Moines River                    Iowa, Missouri"
@.25 = "Big Sioux River                     Iowa, South Dakota"
@.26 = "Missouri River                      Kansas, Iowa, Missouri, Nebraska, South Dakota"
@.27 = "Tug Fork River                      Kentucky, Virginia, West Virginia"
@.28 = "Big Sandy River                     Kentucky, West Virginia"
@.29 = "Pearl River                         Louisiana, Mississippi"
@.30 = "Sabine River                        Louisiana, Texas"
@.31 = "Monument Creek                      Maine, New Brunswick (Canada)"
@.32 = "St. Croix River                     Maine, New Brunswick (Canada)"
@.33 = "Piscataqua River                    Maine, New Hampshire"
@.34 = "St. Francis River                   Maine, Quebec (Canada)"
@.35 = "St. John River                      Maine, Quebec (Canada)"
@.36 = "Pocomoke River                      Maryland, Virginia"
@.37 = "Palmer River                        Massachusetts, Rhode Island and Providence Plantations"
@.38 = "Runnins River                       Massachusetts, Rhode Island and Providence Plantations"
@.39 = "Montreal River                      Michigan (upper peninsula), Wisconsin"
@.40 = "Detroit River                       Michigan, Ontario (Canada)"
@.41 = "St. Clair River                     Michigan, Ontario (Canada)"
@.42 = "St. Marys River                     Michigan, Ontario (Canada)"
@.43 = "Brule River                         Michigan, Wisconsin"
@.44 = "Menominee River                     Michigan, Wisconsin"
@.45 = "Red River of the North              Minnesota, North Dakota"
@.46 = "Bois de Sioux River                 Minnesota, North Dakota, South Dakota"
@.47 = "Pigeon River                        Minnesota, Ontario (Canada)"
@.48 = "Rainy River                         Minnesota, Ontario (Canada)"
@.49 = "St. Croix River                     Minnesota, Wisconsin"
@.50 = "St. Louis River                     Minnesota, Wisconsin"
@.51 = "Halls Stream                        New Hampshire, Canada"
@.52 = "Salmon Falls River                  New Hampshire, Maine"
@.53 = "Connecticut River                   New Hampshire, Vermont"
@.54 = "Arthur Kill                         New Jersey, New York (tidal strait)"
@.55 = "Kill Van Kull                       New Jersey, New York (tidal strait)"
@.56 = "Hudson River (lower part only)      New Jersey, New York"
@.57 = "Rio Grande                          New Mexico, Texas, Tamaulipas (Mexico), Nuevo Leon (Mexico), Coahuila de Zaragoza (Mexico), Chihuahua (Mexico)"
@.58 = "Niagara River                       New York, Ontario (Canada)"
@.59 = "St. Lawrence River                  New York, Ontario (Canada)"
@.60 = "Poultney River                      New York, Vermont"
@.61 = "Catawba River                       North Carolina, South Carolina"
@.62 = "Blackwater River                    North Carolina, Virginia"
@.63 = "Columbia River                      Oregon, Washington"
                do #=1  until  @.#==''           /*find how many entries in array,  and */
                maxL=max(maxL, length(@.#))      /*   also find the maximum width entry.*/
                end   /*#*/
#=#-1                                            /*adjust the highest element number.   */
@.1=center(@.1, maxL, '-')                       /*   "    "  header information.       */
@.2=copies(@.2, maxL)                            /*   "    "     "   separator.         */
return
```

'''output'''
<pre style="height:60ex">
element  1 before sort: ------------------------------------------------ Rivers that form part of a (USA) state's border -------------------------------------------------
element  2 before sort: 
### ============================================================================================================================================

element  3 before sort: Perdido River                       Alabama, Florida
element  4 before sort: Chattahoochee River                 Alabama, Georgia
element  5 before sort: Tennessee River                     Alabama, Kentucky, Mississippi, Tennessee
element  6 before sort: Colorado River                      Arizona, California, Nevada, Baja California (Mexico)
element  7 before sort: Mississippi River                   Arkansas, Illinois, Iowa, Kentucky, Minnesota, Mississippi, Missouri, Tennessee, Louisiana, Wisconsin
element  8 before sort: St. Francis River                   Arkansas, Missouri
element  9 before sort: Poteau River                        Arkansas, Oklahoma
element 10 before sort: Arkansas River                      Arkansas, Oklahoma
element 11 before sort: Red River (Mississippi watershed)   Arkansas, Oklahoma, Texas
element 12 before sort: Byram River                         Connecticut, New York
element 13 before sort: Pawcatuck River                     Connecticut, Rhode Island and Providence Plantations
element 14 before sort: Delaware River                      Delaware, New Jersey, New York, Pennsylvania
element 15 before sort: Potomac River                       District of Columbia, Maryland, Virginia, West Virginia
element 16 before sort: St. Marys River                     Florida, Georgia
element 17 before sort: Chattooga River                     Georgia, South Carolina
element 18 before sort: Tugaloo River                       Georgia, South Carolina
element 19 before sort: Savannah River                      Georgia, South Carolina
element 20 before sort: Snake River                         Idaho, Oregon, Washington
element 21 before sort: Wabash River                        Illinois, Indiana
element 22 before sort: Ohio River                          Illinois, Indiana, Kentucky, Ohio, West Virginia
element 23 before sort: Great Miami River (mouth only)      Indiana, Ohio
element 24 before sort: Des Moines River                    Iowa, Missouri
element 25 before sort: Big Sioux River                     Iowa, South Dakota
element 26 before sort: Missouri River                      Kansas, Iowa, Missouri, Nebraska, South Dakota
element 27 before sort: Tug Fork River                      Kentucky, Virginia, West Virginia
element 28 before sort: Big Sandy River                     Kentucky, West Virginia
element 29 before sort: Pearl River                         Louisiana, Mississippi
element 30 before sort: Sabine River                        Louisiana, Texas
element 31 before sort: Monument Creek                      Maine, New Brunswick (Canada)
element 32 before sort: St. Croix River                     Maine, New Brunswick (Canada)
element 33 before sort: Piscataqua River                    Maine, New Hampshire
element 34 before sort: St. Francis River                   Maine, Quebec (Canada)
element 35 before sort: St. John River                      Maine, Quebec (Canada)
element 36 before sort: Pocomoke River                      Maryland, Virginia
element 37 before sort: Palmer River                        Massachusetts, Rhode Island and Providence Plantations
element 38 before sort: Runnins River                       Massachusetts, Rhode Island and Providence Plantations
element 39 before sort: Montreal River                      Michigan (upper peninsula), Wisconsin
element 40 before sort: Detroit River                       Michigan, Ontario (Canada)
element 41 before sort: St. Clair River                     Michigan, Ontario (Canada)
element 42 before sort: St. Marys River                     Michigan, Ontario (Canada)
element 43 before sort: Brule River                         Michigan, Wisconsin
element 44 before sort: Menominee River                     Michigan, Wisconsin
element 45 before sort: Red River of the North              Minnesota, North Dakota
element 46 before sort: Bois de Sioux River                 Minnesota, North Dakota, South Dakota
element 47 before sort: Pigeon River                        Minnesota, Ontario (Canada)
element 48 before sort: Rainy River                         Minnesota, Ontario (Canada)
element 49 before sort: St. Croix River                     Minnesota, Wisconsin
element 50 before sort: St. Louis River                     Minnesota, Wisconsin
element 51 before sort: Halls Stream                        New Hampshire, Canada
element 52 before sort: Salmon Falls River                  New Hampshire, Maine
element 53 before sort: Connecticut River                   New Hampshire, Vermont
element 54 before sort: Arthur Kill                         New Jersey, New York (tidal strait)
element 55 before sort: Kill Van Kull                       New Jersey, New York (tidal strait)
element 56 before sort: Hudson River (lower part only)      New Jersey, New York
element 57 before sort: Rio Grande                          New Mexico, Texas, Tamaulipas (Mexico), Nuevo Leon (Mexico), Coahuila De Zaragoza (Mexico), Chihuahua (Mexico)
element 58 before sort: Niagara River                       New York, Ontario (Canada)
element 59 before sort: St. Lawrence River                  New York, Ontario (Canada)
element 60 before sort: Poultney River                      New York, Vermont
element 61 before sort: Catawba River                       North Carolina, South Carolina
element 62 before sort: Blackwater River                    North Carolina, Virginia
element 63 before sort: Columbia River                      Oregon, Washington
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
element  1  after sort: ------------------------------------------------ Rivers that form part of a (USA) state's border -------------------------------------------------
element  2  after sort: 
### ============================================================================================================================================

element  3  after sort: Arkansas River                      Arkansas, Oklahoma
element  4  after sort: Arthur Kill                         New Jersey, New York (tidal strait)
element  5  after sort: Big Sandy River                     Kentucky, West Virginia
element  6  after sort: Big Sioux River                     Iowa, South Dakota
element  7  after sort: Blackwater River                    North Carolina, Virginia
element  8  after sort: Bois de Sioux River                 Minnesota, North Dakota, South Dakota
element  9  after sort: Brule River                         Michigan, Wisconsin
element 10  after sort: Byram River                         Connecticut, New York
element 11  after sort: Catawba River                       North Carolina, South Carolina
element 12  after sort: Chattahoochee River                 Alabama, Georgia
element 13  after sort: Chattooga River                     Georgia, South Carolina
element 14  after sort: Colorado River                      Arizona, California, Nevada, Baja California (Mexico)
element 15  after sort: Columbia River                      Oregon, Washington
element 16  after sort: Connecticut River                   New Hampshire, Vermont
element 17  after sort: Delaware River                      Delaware, New Jersey, New York, Pennsylvania
element 18  after sort: Des Moines River                    Iowa, Missouri
element 19  after sort: Detroit River                       Michigan, Ontario (Canada)
element 20  after sort: Great Miami River (mouth only)      Indiana, Ohio
element 21  after sort: Halls Stream                        New Hampshire, Canada
element 22  after sort: Hudson River (lower part only)      New Jersey, New York
element 23  after sort: Kill Van Kull                       New Jersey, New York (tidal strait)
element 24  after sort: Menominee River                     Michigan, Wisconsin
element 25  after sort: Mississippi River                   Arkansas, Illinois, Iowa, Kentucky, Minnesota, Mississippi, Missouri, Tennessee, Louisiana, Wisconsin
element 26  after sort: Missouri River                      Kansas, Iowa, Missouri, Nebraska, South Dakota
element 27  after sort: Montreal River                      Michigan (upper peninsula), Wisconsin
element 28  after sort: Monument Creek                      Maine, New Brunswick (Canada)
element 29  after sort: Niagara River                       New York, Ontario (Canada)
element 30  after sort: Ohio River                          Illinois, Indiana, Kentucky, Ohio, West Virginia
element 31  after sort: Palmer River                        Massachusetts, Rhode Island and Providence Plantations
element 32  after sort: Pawcatuck River                     Connecticut, Rhode Island and Providence Plantations
element 33  after sort: Pearl River                         Louisiana, Mississippi
element 34  after sort: Perdido River                       Alabama, Florida
element 35  after sort: Pigeon River                        Minnesota, Ontario (Canada)
element 36  after sort: Piscataqua River                    Maine, New Hampshire
element 37  after sort: Pocomoke River                      Maryland, Virginia
element 38  after sort: Poteau River                        Arkansas, Oklahoma
element 39  after sort: Potomac River                       District of Columbia, Maryland, Virginia, West Virginia
element 40  after sort: Poultney River                      New York, Vermont
element 41  after sort: Rainy River                         Minnesota, Ontario (Canada)
element 42  after sort: Red River (Mississippi watershed)   Arkansas, Oklahoma, Texas
element 43  after sort: Red River of the North              Minnesota, North Dakota
element 44  after sort: Rio Grande                          New Mexico, Texas, Tamaulipas (Mexico), Nuevo Leon (Mexico), Coahuila De Zaragoza (Mexico), Chihuahua (Mexico)
element 45  after sort: Runnins River                       Massachusetts, Rhode Island and Providence Plantations
element 46  after sort: Sabine River                        Louisiana, Texas
element 47  after sort: Salmon Falls River                  New Hampshire, Maine
element 48  after sort: Savannah River                      Georgia, South Carolina
element 49  after sort: Snake River                         Idaho, Oregon, Washington
element 50  after sort: St. Clair River                     Michigan, Ontario (Canada)
element 51  after sort: St. Croix River                     Maine, New Brunswick (Canada)
element 52  after sort: St. Croix River                     Minnesota, Wisconsin
element 53  after sort: St. Francis River                   Arkansas, Missouri
element 54  after sort: St. Francis River                   Maine, Quebec (Canada)
element 55  after sort: St. John River                      Maine, Quebec (Canada)
element 56  after sort: St. Lawrence River                  New York, Ontario (Canada)
element 57  after sort: St. Louis River                     Minnesota, Wisconsin
element 58  after sort: St. Marys River                     Florida, Georgia
element 59  after sort: St. Marys River                     Michigan, Ontario (Canada)
element 60  after sort: Tennessee River                     Alabama, Kentucky, Mississippi, Tennessee
element 61  after sort: Tug Fork River                      Kentucky, Virginia, West Virginia
element 62  after sort: Tugaloo River                       Georgia, South Carolina
element 63  after sort: Wabash River                        Illinois, Indiana
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒

```



### version 2

{{trans|Python}}The Python code translates very well to [[ooRexx]] but here is a way to implement it in classic REXX as well.


```Rexx
    a = '4 65 2 -31 0 99 83 782 1'
    do i = 1 to words(a)
        queue word(a, i)
    end
    call quickSort
    parse pull item
    do queued()
        call charout ,item', '
        parse pull item
    end
    say item
    exit

quickSort: procedure
/* In classic Rexx, arguments are passed by value, not by reference so stems
    cannot be passed as arguments nor used as return values.  Putting their
    contents on the external data queue is a way to bypass this issue. */

    /* construct the input stem */
    arr.0 = queued()
    do i = 1 to arr.0
        parse pull arr.i
    end
    less.0 = 0
    pivotList.0 = 0
    more.0 = 0
    if arr.0 <= 1 then do
        if arr.0 = 1 then
            queue arr.1
        return
    end
    else do
        pivot = arr.1
        do i = 1 to arr.0
            item = arr.i
            select
                when item < pivot then do
                    j = less.0 + 1
                    less.j = item
                    less.0 = j
                end
                when item > pivot then do
                    j = more.0 + 1
                    more.j = item
                    more.0 = j
                end
                otherwise
                    j = pivotList.0 + 1
                    pivotList.j = item
                    pivotList.0 = j
            end
        end
    end
    /* recursive call to sort the less. stem */
    do i = 1 to less.0
        queue less.i
    end
    if queued() > 0 then do
        call quickSort
        less.0 = queued()
        do i = 1 to less.0
            parse pull less.i
        end
    end
    /* recursive call to sort the more. stem */
    do i = 1 to more.0
        queue more.i
    end
    if queued() > 0 then do
        call quickSort
        more.0 = queued()
        do i = 1 to more.0
            parse pull more.i
        end
    end
    /* put the contents of all 3 stems on the queue in order */
    do i = 1 to less.0
        queue less.i
    end
    do i = 1 to pivotList.0
        queue pivotList.i
    end
    do i = 1 to more.0
        queue more.i
    end
    return
```



## Ring


```ring

# Project : Sorting algorithms/Quicksort

test = [4, 65, 2, -31, 0, 99, 2, 83, 782, 1]
see "before sort:" + nl
showarray(test)
quicksort(test, 1, 10)
see "after sort:" + nl
showarray(test)
 
func quicksort(a, s, n)
       if n < 2 
          return
       ok
       t = s + n - 1
       l = s
       r = t
       p = a[floor((l + r) / 2)]
       while l <= r
               while a[l] < p 
                       l = l + 1
               end
               while a[r] > p
                       r = r - 1
               end
               if l <= r
                  temp = a[l]
                  a[l] = a[r]
                  a[r] = temp
                  l = l + 1
                  r = r - 1
              ok
       end
       if s < r 
          quicksort(a, s, r - s + 1)
       ok
       if l < t 
         quicksort(a, l, t - l + 1 )
       ok

func showarray(vect)
        svect = ""
        for n = 1 to len(vect)
              svect = svect + vect[n] + " "
        next
        svect = left(svect, len(svect) - 1)
        see svect + nl

```

Output:

```txt

before sort:
4 65 2 -31 0 99 2 83 782 1
after sort:
-31 0 1 2 2 4 65 83 99 782

```



## Ruby


```ruby
class Array
  def quick_sort
    return self if length <= 1
    pivot = self[0]
    less, greatereq = self[1..-1].partition { |x| x < pivot }
    less.quick_sort + [pivot] + greatereq.quick_sort
  end
end
```

or

```ruby
class Array
  def quick_sort
    return self if length <= 1
    pivot = sample
    group = group_by{ |x| x <=> pivot }
    group.default = []
    group[-1].quick_sort + group[0] + group[1].quick_sort
  end
end
```

or functionally

```ruby
class Array
  def quick_sort
    h, *t = self
    h ? t.partition { |e| e < h }.inject { |l, r| l.quick_sort + [h] + r.quick_sort } : []
  end
end
```



## Run BASIC


```runbasic
' -------------------------------
' quick sort
' -------------------------------
size = 50
dim s(size)			' array to sort
for i = 1 to size		' fill it with some random numbers
 s(i) = rnd(0) * 100
next i

lft  = 1
rht  = size

[qSort]
  lftHold = lft
  rhtHold = rht
  pivot   = s(lft)
  while lft < rht
    while (s(rht) >= pivot) and (lft < rht) : rht = rht - 1 :wend
    if lft <> rht then
      s(lft) = s(rht)
      lft    = lft + 1
    end if
    while (s(lft) <= pivot) and (lft < rht) : lft = lft + 1 :wend
    if lft <> rht then
      s(rht) = s(lft)
      rht    = rht - 1
    end if
  wend

  s(lft) = pivot
  pivot  = lft
  lft    = lftHold
  rht    = rhtHold
  if lft < pivot then
    rht = pivot - 1
    goto [qSort]
  end if 
 if rht > pivot then
    lft = pivot + 1
    goto [qSort]
 end if

for i = 1 to size
 print i;"-->";s(i)
next i
```



## Rust


```rust
fn main() {
    println!("Sort numbers in descending order");
    let mut numbers = [4, 65, 2, -31, 0, 99, 2, 83, 782, 1];
    println!("Before: {:?}", numbers);

    quick_sort(&mut numbers, &|x,y| x > y);
    println!("After:  {:?}\n", numbers);

    println!("Sort strings alphabetically");
    let mut strings = ["beach", "hotel", "airplane", "car", "house", "art"];
    println!("Before: {:?}", strings);

    quick_sort(&mut strings, &|x,y| x < y);
    println!("After:  {:?}\n", strings);
    
    println!("Sort strings by length");
    println!("Before: {:?}", strings);

    quick_sort(&mut strings, &|x,y| x.len() < y.len());
    println!("After:  {:?}", strings);    
}

fn quick_sort<T,F>(v: &mut [T], f: &F) 
    where F: Fn(&T,&T) -> bool
{
    let len = v.len();
    if len >= 2 {
        let pivot_index = partition(v, f);
        quick_sort(&mut v[0..pivot_index], f);
        quick_sort(&mut v[pivot_index + 1..len], f);
    }
}

fn partition<T,F>(v: &mut [T], f: &F) -> usize 
    where F: Fn(&T,&T) -> bool
{
    let len = v.len();
    let pivot_index = len / 2;
    let last_index = len - 1;

    v.swap(pivot_index, last_index);

    let mut store_index = 0;
    for i in 0..last_index {
        if f(&v[i], &v[last_index]) {
            v.swap(i, store_index);
            store_index += 1;
        }
    }

    v.swap(store_index, len - 1);
    store_index
}
```


{{out}}

```txt
Sort numbers in descending order
Before: [4, 65, 2, -31, 0, 99, 2, 83, 782, 1]
After:  [782, 99, 83, 65, 4, 2, 2, 1, 0, -31]

Sort strings alphabetically
Before: ["beach", "hotel", "airplane", "car", "house", "art"]
After:  ["airplane", "art", "beach", "car", "hotel", "house"]

Sort strings by length
Before: ["airplane", "art", "beach", "car", "hotel", "house"]
After:  ["car", "art", "house", "hotel", "beach", "airplane"]
```



## SASL

Copied from SASL manual, Appendix II, solution (2)(b)

```SASL
DEF || this rather nice solution is due to Silvio Meira
sort () = ()
sort (a : x) = sort {b <- x; b <= a } ++ a : sort { b <- x; b>a}
?
```



## Sather


```sather
class SORT{T < $IS_LT{T}} is

  private afilter(a:ARRAY{T}, cmp:ROUT{T,T}:BOOL, p:T):ARRAY{T} is
    filtered ::= #ARRAY{T};
    loop v ::= a.elt!;
      if cmp.call(v, p) then
        filtered := filtered.append(|v|);
      end;
    end;
    return filtered;
  end;

  private mlt(a, b:T):BOOL is return a < b; end;
  private mgt(a, b:T):BOOL is return a > b; end;
  quick_sort(inout a:ARRAY{T}) is
    if a.size < 2 then return; end;
    pivot ::= a.median;
    left:ARRAY{T} := afilter(a, bind(mlt(_,_)), pivot);
    right:ARRAY{T} := afilter(a, bind(mgt(_,_)), pivot);
    quick_sort(inout left);
    quick_sort(inout right);
    res ::= #ARRAY{T};
    res := res.append(left, |pivot|,  right);
    a := res;
  end;
end;
```



```sather
class MAIN is
  main is
    a:ARRAY{INT} := |10, 9, 8, 7, 6, -10, 5, 4, 656, -11|;
    b ::= a.copy;
    SORT{INT}::quick_sort(inout a);
    #OUT + a + "\n" + b.sort + "\n";
  end;
end;
```


The ARRAY class has a builtin sorting method, which is quicksort (but under certain condition an insertion sort is used instead), exactly <code>quicksort_range</code>; this implementation is original.


## Scala

What follows is a progression on genericity here.

First, a quick sort of a list of integers:


```scala
  def sort(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case head :: tail =>
      val (less, notLess) = tail.partition(_ < head) // Arbitrarily partition list in two
      sort(less) ++ (head :: sort(notLess))          // Sort each half
  }
```


Next, a quick sort of a list of some type T, given a lessThan function:


```scala
  def sort[T](xs: List[T], lessThan: (T, T) => Boolean): List[T] = xs match {
    case Nil => Nil
    case x :: xx =>
      val (lo, hi) = xx.partition(lessThan(_, x))
      sort(lo, lessThan) ++ (x :: sort(hi, lessThan))
  }
```


To take advantage of known orderings, a quick sort of a list of some type T,
for which exists an implicit (or explicit) Ordering[T]:


```scala
  def sort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = xs match {
    case Nil => Nil
    case x :: xx =>
      val (lo, hi) = xx.partition(ord.lt(_, x))
      sort[T](lo) ++ (x :: sort[T](hi))
  }
```


That last one could have worked with Ordering, but Ordering is Java, and doesn't have
the less than operator. Ordered is Scala-specific, and provides it.


```scala
  def sort[T <: Ordered[T]](xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case x :: xx =>
      val (lo, hi) = xx.partition(_ < x)
      sort(lo) ++ (x :: sort(hi))
  }
```


What hasn't changed in all these examples is ordering a list. It is possible
to write a generic quicksort in Scala, which will order any kind of collection. To do
so, however, requires that the type of the collection, itself, be made a parameter to
the function. Let's see it below, and then remark upon it:


```scala
  def sort[T, C[T] <: scala.collection.TraversableLike[T, C[T]]]
    (xs: C[T])
    (implicit ord: scala.math.Ordering[T],
      cbf: scala.collection.generic.CanBuildFrom[C[T], T, C[T]]): C[T] = {
    // Some collection types can't pattern match
    if (xs.isEmpty) {
      xs
    } else {
      val (lo, hi) = xs.tail.partition(ord.lt(_, xs.head))
      val b = cbf()
      b.sizeHint(xs.size)
      b ++= sort(lo)
      b += xs.head
      b ++= sort(hi)
      b.result()
    }
  }
```


The type of our collection is "C[T]", and,
by providing C[T] as a type parameter to TraversableLike, we ensure C[T] is capable
of returning instances of type C[T]. Traversable is the base type of all collections,
and TraversableLike is a trait which contains the implementation of most Traversable
methods.

We need another parameter, though, which is a factory capable of building a C[T] collection.
That is being passed implicitly, so callers to this method do not need to provide them, as
the collection they are using should already provide one as such implicitly. Because we need that
implicitly, then we need to ask for the "T => Ordering[T]" as well, as the "T <: Ordered[T]"
which provides it cannot be used in conjunction with implicit parameters.

The body of the function is from the list variant, since many of the Traversable
collection types don't support pattern matching, "+:" or "::".


## Scheme




```scheme
(define (split-by l p k)
  (let loop ((low '())
             (high '())
             (l l))
    (cond ((null? l)
           (k low high))
          ((p (car l))
           (loop low (cons (car l) high) (cdr l)))
          (else
           (loop (cons (car l) low) high (cdr l))))))
 
(define (quicksort l gt?)
  (if (null? l)
      '()
      (split-by (cdr l) 
                (lambda (x) (gt? x (car l)))
                (lambda (low high)
                  (append (quicksort low gt?)
                          (list (car l))
                          (quicksort high gt?))))))

(quicksort '(1 3 5 7 9 8 6 4 2) >)
```


With srfi-1:

```scheme
(define (quicksort l gt?)
  (if (null? l)
      '()
      (append (quicksort (filter (lambda (x) (gt? (car l) x)) (cdr l)) gt?)
              (list (car l))
              (quicksort (filter (lambda (x) (not (gt? (car l) x))) (cdr l)) gt?))))

(quicksort '(1 3 5 7 9 8 6 4 2) >)

```



## Seed7


```seed7
const proc: quickSort (inout array elemType: arr, in integer: left, in integer: right) is func
  local
    var elemType: compare_elem is elemType.value;
    var integer: less_idx is 0;
    var integer: greater_idx is 0;
    var elemType: help is elemType.value;
  begin
    if right > left then
      compare_elem := arr[right];
      less_idx := pred(left);
      greater_idx := right;
      repeat
        repeat
          incr(less_idx);
        until arr[less_idx] >= compare_elem;
        repeat
          decr(greater_idx);
        until arr[greater_idx] <= compare_elem or greater_idx = left;
        if less_idx < greater_idx then
          help := arr[less_idx];
          arr[less_idx] := arr[greater_idx];
          arr[greater_idx] := help;
        end if;
      until less_idx >= greater_idx;
      arr[right] := arr[less_idx];
      arr[less_idx] := compare_elem;
      quickSort(arr, left, pred(less_idx));
      quickSort(arr, succ(less_idx), right);
    end if;
  end func;

const proc: quickSort (inout array elemType: arr) is func
  begin
    quickSort(arr, 1, length(arr));
  end func;
```

Original source: [http://seed7.sourceforge.net/algorith/sorting.htm#quickSort]


## SETL

In-place sort (looks much the same as the C version)

```SETL
a := [2,5,8,7,0,9,1,3,6,4];
qsort(a);
print(a);

proc qsort(rw a);
  if #a > 1 then
    pivot := a(#a div 2 + 1);
    l := 1;
    r := #a;
    (while l < r)
      (while a(l) < pivot) l +:= 1; end;
      (while a(r) > pivot) r -:= 1; end;
      swap(a(l), a(r));
    end;
    qsort(a(1..l-1));
    qsort(a(r+1..#a));
  end if;
end proc;

proc swap(rw x, rw y);
  [y,x] := [x,y];
end proc;
```


Copying sort using comprehensions:


```SETL
a := [2,5,8,7,0,9,1,3,6,4];
print(qsort(a));

proc qsort(a);
  if #a > 1 then
    pivot := a(#a div 2 + 1);
    a := qsort([x in a | x < pivot]) +
         [x in a | x = pivot] +
         qsort([x in a | x > pivot]);
  end if;
  return a;
end proc;
```



## Sidef


```ruby
func quicksort (a) {
    a.len < 2 && return(a);
    var p = a.pop_rand;          # to avoid the worst cases
    __FUNC__(a.grep{ .< p}) + [p] + __FUNC__(a.grep{ .>= p});
}
```


## Simula


```simula
PROCEDURE QUICKSORT(A); REAL ARRAY A;
BEGIN

    PROCEDURE QS(A, FIRST, LAST); REAL ARRAY A; INTEGER FIRST, LAST;
    BEGIN
        INTEGER LEFT, RIGHT;
        LEFT := FIRST; RIGHT := LAST;
        IF RIGHT - LEFT + 1 > 1 THEN
        BEGIN
            REAL PIVOT;
            PIVOT := A((LEFT + RIGHT) // 2); 
            WHILE LEFT <= RIGHT DO
            BEGIN
                WHILE A(LEFT) < PIVOT DO LEFT := LEFT + 1;
                WHILE A(RIGHT) > PIVOT DO RIGHT := RIGHT - 1;
                IF LEFT <= RIGHT THEN
                BEGIN
                    REAL SWAP;
                    SWAP := A(LEFT); A(LEFT) := A(RIGHT); A(RIGHT) := SWAP;
                    LEFT := LEFT + 1; RIGHT := RIGHT - 1;
                END;
            END;
            QS(A, FIRST, RIGHT);
            QS(A, LEFT, LAST);
        END;
    END QS;

    QS(A, LOWERBOUND(A, 1), UPPERBOUND(A, 1));

END QUICKSORT;

```



## Standard ML


```sml
fun quicksort [] = []
  | quicksort (x::xs) =
    let 
        val (left, right) = List.partition (fn y => y<x) xs
    in
        quicksort left @ [x] @ quicksort right
    end

```

------------------------------------------------------------

Solution 2:

Without using List.partition

```sml

fun par_helper([], x, l, r) = (l, r) |
	par_helper(h::t, x, l, r) = 
		if h <= x then 
			par_helper(t, x, l @ [h], r)
		else
			par_helper(t, x, l, r @ [h]);

fun par(l, x) = par_helper(l, x, [], []);

fun quicksort [] = []
  | quicksort (h::t) =
    let 
        val (left, right) = par(t, h)
    in
        quicksort left @ [h] @ quicksort right
    end;
```



## Swift


```swift>func quicksort<T where T : Comparable
(inout elements: [T], range: Range<Int>) {
  if (range.endIndex - range.startIndex > 1) {
    let pivotIndex = partition(&elements, range)
    quicksort(&elements, range.startIndex ..< pivotIndex)
    quicksort(&elements, pivotIndex+1 ..< range.endIndex)
  }
}

func quicksort<T where T : Comparable>(inout elements: [T]) {
  quicksort(&elements, indices(elements))
}
```



## Tailspin

Simple recursive quicksort:

```tailspin

templates quicksort
  @: [];
  $ -> #
  <[](2..)>
    def pivot: $(1);
    [ [ $(2..-1)... -> (
      <..$pivot>
        $ !
      <>
        ..|@quicksort: $;
     )] -> quicksort..., $pivot, $@ -> quicksort... ] !
   <>
     $ !
end quicksort
 
[4,5,3,8,1,2,6,7,9,8,5] -> quicksort -> !OUT::write

```


In place:

```tailspin

templates quicksort
  templates partial
    def first: $(1);
    def last: $(2);
    def pivot: $@quicksort($first);
    [ $(1) + 1, $(2)  ] -> #

    <?($(2) <..~$(1)>)>
      def limit: $(2);
      @quicksort($first): $@quicksort($limit);
      @quicksort($limit): $pivot;
      [ $first, $limit - 1 ] !
      [ $limit + 1, $last ] !

    <?($@quicksort($(2)) <$pivot~..>)>
      [ $(1), $(2) - 1] -> #

    <?($@quicksort($(1)) <..$pivot>)>
      [ $(1) + 1, $(2)] -> #

    <>
      def temp: $@quicksort($(1));
      @quicksort($(1)): $@quicksort($(2));
      @quicksort($(2)): $temp;
      [ $(1) + 1, $(2) - 1] -> #
  end partial
  @: $;
  [1, $@::length] -> #
  $@ !

  <?($(1) <..~$(2)>)>
    $ -> partial -> #
end quicksort

[4,5,3,8,1,2,6,7,9,8,5] -> quicksort -> !OUT::write

```



## Tcl


```tcl
package require Tcl 8.5

proc quicksort {m} {
    if {[llength $m] <= 1} {
        return $m
    }
    set pivot [lindex $m 0]
    set less [set equal [set greater [list]]]
    foreach x $m {
        lappend [expr {$x < $pivot ? "less" : $x > $pivot ? "greater" : "equal"}] $x
    }
    return [concat [quicksort $less] $equal [quicksort $greater]]
}

puts [quicksort {8 6 4 2 1 3 5 7 9}] ;# => 1 2 3 4 5 6 7 8 9
```



## TypeScript

<lang>
/**
  Generic quicksort function using typescript generics.
  Follows quicksort as done in CLRS.
*/
export type Comparator<T> = (o1: T, o2: T) => number;


export function quickSort<T>(array: T[], compare: Comparator<T>) {
  if (array.length <= 1 || array == null) {
    return;
  }
  sort(array, compare, 0, array.length - 1);
}

function sort<T>(
    array: T[], compare: Comparator<T>, low: number, high: number) {
  if (low < high) {
    const partIndex = partition(array, compare, low, high);
    sort(array, compare, low, partIndex - 1);
    sort(array, compare, partIndex + 1, high);
  }
}

function partition<T>(
    array: T[], compare: Comparator<T>, low: number, high: number): number {
  const pivot: T = array[high];
  let i: number = low - 1;
  for (let j = low; j <= high - 1; j++) {
    if (compare(array[j], pivot) == -1) {
      i = i + 1;
      swap(array, i, j)
    }
  }
  if (compare(array[high], array[i + 1]) == -1) {
    swap(array, i + 1, high);
  }
  return i + 1;
}

function swap<T>(array: T[], i: number, j: number) {
  const newJ: T = array[i];
  array[i] = array[j];
  array[j] = newJ;
}

export function testQuickSort(): void {
  function numberComparator(o1: number, o2: number): number {
    if (o1 < o2) {
      return -1;
    } else if (o1 == o2) {
      return 0;
    }
    return 1;
  }
  let tests: number[][] = [
    [], [1], [2, 1], [-1, 2, -3], [3, 16, 8, -5, 6, 4], [1, 2, 3, 4, 5, 6],
    [1, 2, 3, 4, 5]
  ];

  for (let testArray of tests) {
    quickSort(testArray, numberComparator);
    console.log(testArray);
  }
}

```



## uBasic/4tH

<lang>PRINT "Quick sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Quicksort (n)
  PROC _ShowArray (n)
PRINT
 
END


_InnerQuick PARAM(2)
  LOCAL(4)

  IF b@ < 2 THEN RETURN
  f@ = a@ + b@ - 1
  c@ = a@
  e@ = f@
  d@ = @((c@ + e@) / 2)

  DO
    DO WHILE @(c@) < d@
      c@ = c@ + 1
    LOOP

    DO WHILE @(e@) > d@
      e@ = e@ - 1
    LOOP

    IF c@ - 1 < e@ THEN
      PROC _Swap (c@, e@)
      c@ = c@ + 1
      e@ = e@ - 1
    ENDIF

    UNTIL c@ > e@
  LOOP

  IF a@ < e@ THEN PROC _InnerQuick (a@, e@ - a@ + 1)
  IF c@ < f@ THEN PROC _InnerQuick (c@, f@ - c@ + 1)
RETURN


_Quicksort PARAM(1)                   ' Quick sort
  PROC _InnerQuick (0, a@)
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



## UnixPipes

{{works with|Zsh}}


```bash
split() {
  (while read n ; do
      test $1 -gt $n && echo $n > $2 || echo $n > $3
  done)
}

qsort() {
 (read p; test -n "$p" && (
     lc="1.$1" ; gc="2.$1"
     split $p >(qsort $lc >$lc) >(qsort $gc >$gc);
     cat $lc <(echo $p) $gc
     rm -f $lc $gc;
 ))
}

cat to.sort | qsort
```



## Ursala

The distributing bipartition operator, *|, is useful for this
algorithm. The pivot is chosen as the greater of the first two
items, this being the least sophisticated method sufficient to
ensure termination. The quicksort function is a higher order
function parameterized by the relational predicate p, which
can be chosen appropriately for the type of items in the list
being sorted. This example demonstrates sorting a list of
natural numbers.


```Ursala
#import nat

quicksort "p" = ~&itB^?a\~&a ^|WrlT/~& "p"*|^\~& "p"?hthPX/~&th ~&h

#cast %nL

example = quicksort(nleq) <694,1377,367,506,3712,381,1704,1580,475,1872>
```

{{out}}

```txt

<367,381,475,506,694,1377,1580,1704,1872,3712>

```



## V



```v
[qsort
  [joinparts [p [*l1] [*l2] : [*l1 p *l2]] view].
  [split_on_first uncons [>] split].
  [small?]
    []
    [split_on_first [l1 l2 : [l1 qsort l2 qsort joinparts]] view i]
  ifte].
```


The way of joy (using binrec)

```v
[qsort
   [small?] []
     [uncons [>] split]
     [[p [*l] [*g] : [*l p *g]] view]
    binrec].
```


{{omit from|GUISS}}


## VBA

This is the "simple" quicksort, using temporary arrays.

```vb
Public Sub Quick(a() As Variant, last As Integer)
' quicksort a Variant array (1-based, numbers or strings)
Dim aLess() As Variant
Dim aEq() As Variant
Dim aGreater() As Variant
Dim pivot As Variant
Dim naLess As Integer
Dim naEq As Integer
Dim naGreater As Integer

If last > 1 Then
    'choose pivot in the middle of the array
    pivot = a(Int((last + 1) / 2))
    'construct arrays
    naLess = 0
    naEq = 0
    naGreater = 0
    For Each el In a()
      If el > pivot Then
        naGreater = naGreater + 1
        ReDim Preserve aGreater(1 To naGreater)
        aGreater(naGreater) = el
      ElseIf el < pivot Then
        naLess = naLess + 1
        ReDim Preserve aLess(1 To naLess)
        aLess(naLess) = el
      Else
        naEq = naEq + 1
        ReDim Preserve aEq(1 To naEq)
        aEq(naEq) = el
      End If
    Next
    'sort arrays "less" and "greater"
    Quick aLess(), naLess
    Quick aGreater(), naGreater
    'concatenate
    P = 1
    For i = 1 To naLess
      a(P) = aLess(i): P = P + 1
    Next
    For i = 1 To naEq
      a(P) = aEq(i): P = P + 1
    Next
    For i = 1 To naGreater
      a(P) = aGreater(i): P = P + 1
    Next
End If
End Sub

Public Sub QuicksortTest()
Dim a(1 To 26) As Variant

 'populate a with numbers in descending order, then sort
 For i = 1 To 26: a(i) = 26 - i: Next
 Quick a(), 26
 For i = 1 To 26: Debug.Print a(i);: Next
 Debug.Print
 'now populate a with strings in descending order, then sort
 For i = 1 To 26: a(i) = Chr$(Asc("z") + 1 - i) & "-stuff": Next
 Quick a(), 26
 For i = 1 To 26: Debug.Print a(i); " ";: Next
 Debug.Print
End Sub
```


{{out}}

```txt
quicksorttest
 0  1  2  3  4  5  6  7  8  9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25 
a-stuff b-stuff c-stuff d-stuff e-stuff f-stuff g-stuff h-stuff i-stuff j-stuff k-stuff l-stuff m-stuff n-stuff o-stuff p-stuff q-stuff r-stuff s-stuff t-stuff u-stuff v-stuff w-stuff x-stuff y-stuff z-stuff 
```


Note: the "quicksort in place"


## VBScript

{{trans|BBC BASIC}}

```vb
Function quicksort(arr,s,n)
	l = s
	r = s + n - 1
	p = arr(Int((l + r)/2))
	Do Until l > r
		Do While arr(l) < p
			l = l + 1
		Loop
		Do While arr(r) > p
			r = r -1
		Loop
		If l <= r Then
			tmp = arr(l)
			arr(l) = arr(r)
			arr(r) = tmp
			l = l + 1
			r = r - 1
		End If
	Loop
	If s < r Then
		Call quicksort(arr,s,r-s+1)
	End If
	If l < t Then
		Call quicksort(arr,l,t-l+1)
	End If
	quicksort = arr
End Function

myarray=Array(9,8,7,6,5,5,4,3,2,1,0,-1)
m = quicksort(myarray,0,12)
WScript.Echo Join(m,",")
```

{{out}}

```txt
-1,0,1,2,3,4,5,5,6,7,8,9
```



## Wart


```python
def (qsort (pivot ... ns))
  (+ (qsort+keep (fn(_) (_ < pivot)) ns)
     list.pivot
     (qsort+keep (fn(_) (_ > pivot)) ns))

def (qsort x) :case x=nil
  nil
```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
string 0;                       \use zero-terminated strings

proc    QSort(Array, Num);      \Quicksort Array into ascending order
char    Array;                  \address of array to sort
int     Num;                    \number of elements in the array
int     I, J, Mid, Temp;
[I:= 0;
J:= Num-1;
Mid:= Array(J>>1);
while I <= J do
       [while Array(I) < Mid do I:= I+1;
        while Array(J) > Mid do J:= J-1;
        if I <= J then
                [Temp:= Array(I);  Array(I):= Array(J);  Array(J):= Temp;
                I:= I+1;
                J:= J-1;
                ];
        ];
if I < Num-1 then QSort(@Array(I), Num-I);
if J > 0 then QSort(Array, J+1);
];      \QSort

func    StrLen(Str);            \Return number of characters in an ASCIIZ string
char    Str;
int     I;
for I:= 0 to -1>>1-1 do
        if Str(I) = 0 then return I;

char    Str;
[Str:= "Pack my box with five dozen liquor jugs.";
QSort(Str, StrLen(Str), 1);
Text(0, Str);  CrLf(0);
]
```


{{out}}

```txt

       .Pabcdeefghiiijklmnoooqrstuuvwxyz

```



## zkl

These are the Wikipedia algorithms.

Quick sort immutable sequence using crappy pivot choice:

```zkl
fcn qtSort(list,cmp=Op("<")){	// sort immutable lists
   fcn(list,cmp,N){	// spendy to keep recreating cmp
      reg pivot=list[0], rest=list[1,*];
      left,right:=rest.filter22(cmp,pivot);
      N+=1;
      T.extend(self.fcn(left,cmp,N),T(pivot),self.fcn(right,cmp,N));
   }(list,cmp,0);
}
```

In place quick sort:

```zkl
fcn qiSort(list,cmp='<){		// in place quick sort
   fcn(list,left,right,cmp){
      if (left<right){
	 // partition list
	 pivotIndex:=(left+right)/2; // or median of first,middle,last
	 pivot:=list[pivotIndex];
	 list.swap(pivotIndex,right);	// move pivot to end
	 pivotIndex:=left;
	 i:=left; do(right-left){	// foreach i in ([left..right-1])
	    if(cmp(list[i],pivot)){	// not cheap
	       list.swap(i,pivotIndex);
	       pivotIndex+=1;
	    }
	    i+=1;
	 }
	 list.swap(pivotIndex,right);	// move pivot to final place

	 // sort the partitions
         self.fcn(list,left,pivotIndex-1,cmp);
	 return(self.fcn(list,pivotIndex+1,right,cmp));
      }
   }(list,0,list.len()-1,cmp);
   list;
}
```

