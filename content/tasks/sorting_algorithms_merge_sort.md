+++
title = "Sorting algorithms/Merge sort"
description = ""
date = 2019-07-28T19:12:29Z
aliases = []
[extra]
id = 2531
[taxonomies]
categories = ["task", "Sorting Algorithms"]
tags = []
languages = [
  "360_assembly",
  "acl2",
  "actionscript",
  "ada",
  "algol_68",
  "astro",
  "autohotkey",
  "bbc_basic",
  "c",
  "clojure",
  "cobol",
  "coffeescript",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "curry",
  "d",
  "dart",
  "e",
  "easylang",
  "eiffel",
  "elixir",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "funl",
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
  "liberty_basic",
  "logo",
  "logtalk",
  "lua",
  "lucid",
  "m2000_interpreter",
  "maple",
  "matlab",
  "maxima",
  "maxscript",
  "mercury",
  "nemerle",
  "netrexx",
  "nim",
  "ocaml",
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
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rebol",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "standard_ml",
  "swift",
  "tailspin",
  "tcl",
  "unixpipes",
  "ursala",
  "v",
  "xpl0",
  "zed",
  "zkl",
]
+++

The   '''merge sort'''   is a recursive sort of order   <big> n*log(n). </big>

It is notable for having a worst case and average complexity of   <big> ''O(n*log(n))'', </big>   and a best case complexity of   <big> ''O(n)''   </big> (for pre-sorted input).

The basic idea is to split the collection into smaller groups by halving it until the groups only have one element or no elements   (which are both entirely sorted groups).

Then merge the groups back together so that their elements are in order.

This is how the algorithm gets its   ''divide and conquer''   description.


## Task

Write a function to sort a collection of integers using the merge sort.


The merge sort algorithm comes in two parts:
    a sort function     and
    a merge function

The functions in pseudocode look like this:
 '''function''' ''mergesort''(m)
    '''var''' list left, right, result
    '''if''' length(m) ≤ 1
        '''return''' m
    '''else'''
        '''var''' middle = length(m) / 2
        '''for each''' x '''in''' m '''up to''' middle - 1
            '''add''' x '''to''' left
        '''for each''' x '''in''' m '''at and after''' middle
            '''add''' x '''to''' right
        left = mergesort(left)
        right = mergesort(right)
        '''if''' last(left) ≤ first(right)
           '''append''' right '''to''' left
           '''return''' left
        result = merge(left, right)
        '''return''' result

 '''function''' ''merge''(left,right)
    '''var''' list result
    '''while''' length(left) > 0 and length(right) > 0
        '''if''' first(left) ≤ first(right)
            '''append''' first(left) '''to''' result
            left = rest(left)
        '''else'''
            '''append''' first(right) '''to''' result
            right = rest(right)
    '''if''' length(left) > 0
        '''append''' rest(left) '''to''' result
    '''if''' length(right) > 0
        '''append''' rest(right) '''to''' result
    '''return''' result


## See also

*   the Wikipedia entry:   [[wp:Merge_sort| merge sort]]


Note:   better performance can be expected if, rather than recursing until   <big> length(m) ≤ 1, </big>   an insertion sort is used for   <big> length(m) </big>   smaller than some threshold larger than   '''1'''.   However, this complicates the example code, so it is not shown here.





## 360 Assembly

The program uses ASM structured macros and two ASSIST macros (XDECO, XPRNT) to keep the code as short as possible.

```360asm
*      Merge sort                  19/06/2016
MAIN   CSECT
       STM     R14,R12,12(R13)     save caller's registers
       LR      R12,R15             set R12 as base register
       USING   MAIN,R12            notify assembler
       LA      R11,SAVEXA          get the address of my savearea
       ST      R13,4(R11)          save caller's save area pointer
       ST      R11,8(R13)          save my save area pointer
       LR      R13,R11             set R13 to point to my save area
       LA      R1,1                1
       LA      R2,NN               hbound(a)
       BAL     R14,SPLIT           call split(1,hbound(a))
       LA      RPGI,PG             pgi=0
       LA      RI,1                i=1
       DO WHILE=(C,RI,LE,=A(NN))   do i=1 to hbound(a)
         LR    R1,RI                 i
         SLA   R1,2                  .
         L     R2,A-4(R1)            a(i)
         XDECO R2,XDEC               edit a(i)
         MVC   0(4,RPGI),XDEC+8      output a(i)
         LA    RPGI,4(RPGI)          pgi=pgi+4
         LA    RI,1(RI)              i=i+1
       ENDDO   ,                   end do
       XPRNT   PG,80               print buffer
       L       R13,SAVEXA+4        restore caller's savearea address
       LM      R14,R12,12(R13)     restore caller's registers
       XR      R15,R15             set return code to 0
       BR      R14                 return to caller
*      split(istart,iend)          ------recursive---------------------
SPLIT  STM     R14,R12,12(R13)     save all registers
       LR      R9,R1               save R1
       LA      R1,72               amount of storage required
       GETMAIN RU,LV=(R1)          allocate storage for stack
       USING   STACK,R10           make storage addressable
       LR      R10,R1              establish stack addressability
       LA      R11,SAVEXB          get the address of my savearea
       ST      R13,4(R11)          save caller's save area pointer
       ST      R11,8(R13)          save my save area pointer
       LR      R13,R11             set R13 to point to my save area
       LR      R1,R9               restore R1
       LR      RSTART,R1           istart=R1
       LR      REND,R2             iend=R2
       IF CR,REND,EQ,RSTART THEN   if iend=istart
         B     RETURN                return
       ENDIF   ,                   end if
       BCTR    R2,0                iend-1
       IF C,R2,EQ,RSTART THEN      if iend-istart=1
         LR    R1,REND               iend
         SLA   R1,2                  .
         L     R2,A-4(R1)            a(iend)
         LR    R1,RSTART             istart
         SLA   R1,2                  .
         L     R3,A-4(R1)            a(istart)
         IF CR,R2,LT,R3 THEN         if a(iend)<a(istart)
           LR  R1,RSTART               istart
           SLA R1,2                    .
           LA  R2,A-4(R1)              @a(istart)
           LR  R1,REND                 iend
           SLA R1,2                    .
           LA  R3,A-4(R1)              @a(iend)
           MVC TEMP,0(R2)              temp=a(istart)
           MVC 0(4,R2),0(R3)           a(istart)=a(iend)
           MVC 0(4,R3),TEMP            a(iend)=temp
         ENDIF ,                     end if
         B     RETURN                return
       ENDIF   ,                   end if
       LR      RMIDDL,REND         iend
       SR      RMIDDL,RSTART       iend-istart
       SRA     RMIDDL,1            (iend-istart)/2
       AR      RMIDDL,RSTART       imiddl=istart+(iend-istart)/2
       LR      R1,RSTART           istart
       LR      R2,RMIDDL           imiddl
       BAL     R14,SPLIT           call split(istart,imiddl)
       LA      R1,1(RMIDDL)        imiddl+1
       LR      R2,REND             iend
       BAL     R14,SPLIT           call split(imiddl+1,iend)
       LR      R1,RSTART           istart
       LR      R2,RMIDDL           imiddl
       LR      R3,REND             iend
       BAL     R14,MERGE           call merge(istart,imiddl,iend)
RETURN L       R13,SAVEXB+4        restore caller's savearea address
       XR      R15,R15             set return code to 0
       LA      R0,72               amount of storage to free
       FREEMAIN A=(R10),LV=(R0)    free allocated storage
       L       R14,12(R13)         restore caller's return address
       LM      R2,R12,28(R13)      restore registers R2 to R12
       BR      R14                 return to caller
       DROP    R10                 base no longer needed
*      merge(jstart,jmiddl,jend)   ------------------------------------
MERGE  STM     R1,R3,JSTART        jstart=r1,jmiddl=r2,jend=r3
       SR      R2,R1               jmiddl-jstart
       LA      RBS,2(R2)           bs=jmiddl-jstart+2
       LA      RI,1                i=1
       LR      R3,RBS              bs
       BCTR    R3,0                bs-1
       DO WHILE=(CR,RI,LE,R3)      do i=0 to bs-1
         L     R2,JSTART             jstart
         AR    R2,RI                 jstart+i
         SLA   R2,2                  .
         L     R2,A-8(R2)            a(jstart+i-1)
         LR    R1,RI                 i
         SLA   R1,2                  .
         ST    R2,B-4(R1)            b(i)=a(jstart+i-1)
         LA    RI,1(RI)              i=i+1
       ENDDO   ,                   end do
       LA      RI,1                i=1
       L       RJ,JMIDDL           j=jmiddl
       LA      RJ,1(RJ)            j=jmiddl+1
       L       RK,JSTART           k=jstart
       DO UNTIL=(CR,RI,EQ,RBS,OR,  do until i=bs or                    X
               C,RJ,GT,JEND)                j>jend
         LR    R1,RI                 i
         SLA   R1,2                  .
         L     R4,B-4(R1)            r4=b(i)
         LR    R1,RJ                 j
         SLA   R1,2                  .
         L     R3,A-4(R1)            r3=a(j)
         LR    R9,RK                 k
         SLA   R9,2                  r9 for a(k)
         IF CR,R4,LE,R3 THEN         if b(i)<=a(j)
           ST  R4,A-4(R9)              a(k)=b(i)
           LA  RI,1(RI)                i=i+1
         ELSE  ,                     else
           ST  R3,A-4(R9)              a(k)=a(j)
           LA  RJ,1(RJ)                j=j+1
         ENDIF ,                     end if
         LA    RK,1(RK)              k=k+1
       ENDDO   ,                   end do
       DO WHILE=(CR,RI,LT,RBS)     do while i<bs
         LR    R1,RI                 i
         SLA   R1,2                  .
         L     R2,B-4(R1)            b(i)
         LR    R1,RK                 k
         SLA   R1,2                  .
         ST    R2,A-4(R1)            a(k)=b(i)
         LA    RI,1(RI)              i=i+1
         LA    RK,1(RK)              k=k+1
       ENDDO   ,                   end do
       BR      R14                 return to caller
*      ------- ------------------  ------------------------------------
       LTORG
SAVEXA DS      18F                 savearea of main
NN     EQU     ((B-A)/L'A)         number of items
A      DC F'4',F'65',F'2',F'-31',F'0',F'99',F'2',F'83',F'782',F'1'
       DC F'45',F'82',F'69',F'82',F'104',F'58',F'88',F'112',F'89',F'74'
B      DS      (NN/2+1)F           merge sort static storage
TEMP   DS      F                   for swap
JSTART DS      F                   jstart
JMIDDL DS      F                   jmiddl
JEND   DS      F                   jend
PG     DC      CL80' '             buffer
XDEC   DS      CL12                for edit
STACK  DSECT                       dynamic area
SAVEXB DS      18F                 " savearea of mergsort (72 bytes)
       YREGS
RI     EQU     6                   i
RJ     EQU     7                   j
RK     EQU     8                   k
RSTART EQU     6                   istart
REND   EQU     7                   i
RMIDDL EQU     8                   i
RPGI   EQU     3                   pgi
RBS    EQU     0                   bs
       END     MAIN
```

```txt

 -31   0   1   2   2   4  45  58  65  69  74  82  82  83  88  89  99 104 112 782

```



## ACL2



```Lisp
(defun split (xys)
   (if (endp (rest xys))
       (mv xys nil)
       (mv-let (xs ys)
               (split (rest (rest xys)))
          (mv (cons (first xys) xs)
              (cons (second xys) ys)))))

(defun mrg (xs ys)
   (declare (xargs :measure (+ (len xs) (len ys))))
   (cond ((endp xs) ys)
         ((endp ys) xs)
         ((< (first xs) (first ys))
          (cons (first xs) (mrg (rest xs) ys)))
         (t (cons (first ys) (mrg xs (rest ys))))))

(defthm split-shortens
   (implies (consp (rest xs))
            (mv-let (ys zs)
                    (split xs)
               (and (< (len ys) (len xs))
                    (< (len zs) (len xs))))))

(defun msort (xs)
     (declare (xargs
            :measure (len xs)
            :hints (("Goal"
                     :use ((:instance split-shortens))))))
   (if (endp (rest xs))
       xs
       (mv-let (ys zs)
               (split xs)
          (mrg (msort ys)
               (msort zs)))))
```



## ActionScript


```ActionScript
function mergesort(a:Array)
{
	//Arrays of length 1 and 0 are always sorted
	if(a.length <= 1) return a;
	else
	{
		var middle:uint = a.length/2;
		//split the array into two
		var left:Array = new Array(middle);
		var right:Array = new Array(a.length-middle);
		var j:uint = 0, k:uint = 0;
		//fill the left array
		for(var i:uint = 0; i < middle; i++)
			left[j++]=a[i];
		//fill the right array
		for(i = middle; i< a.length; i++)
			right[k++]=a[i];
		//sort the arrays
		left = mergesort(left);
		right = mergesort(right);
		//If the last element of the left array is less than or equal to the first
		//element of the right array, they are in order and don't need to be merged
		if(left[left.length-1] <= right[0])
			return left.concat(right);
		a = merge(left, right);
		return a;
	}
}

function merge(left:Array, right:Array)
{
	var result:Array = new Array(left.length + right.length);
	var j:uint = 0, k:uint = 0, m:uint = 0;
	//merge the arrays in order
	while(j < left.length && k < right.length)
	{
		if(left[j] <= right[k])
			result[m++] = left[j++];
		else
			result[m++] = right[k++];
	}
	//If one of the arrays has remaining entries that haven't been merged, they
	//will be greater than the rest of the numbers merged so far, so put them on the
	//end of the array.
	for(; j < left.length; j++)
		result[m++] = left[j];
	for(; k < right.length; k++)
		result[m++] = right[k];
	return result;
}
```



## Ada

This example creates a generic package for sorting arrays of any type. Ada allows array indices to be any discrete type, including enumerated types which are non-numeric. Furthermore, numeric array indices can start at any value, positive, negative, or zero. The following code handles all the possible variations in index types.

```ada
generic
   type Element_Type is private;
   type Index_Type is (<>);
   type Collection_Type is array(Index_Type range <>) of Element_Type;
   with function "<"(Left, Right : Element_Type) return Boolean is <>;

package Mergesort is
   function Sort(Item : Collection_Type) return Collection_Type;
end MergeSort;
```



```ada
package body Mergesort is

   -----------
   -- Merge --
   -----------

   function Merge(Left, Right : Collection_Type) return Collection_Type is
      Result : Collection_Type(Left'First..Right'Last);
      Left_Index : Index_Type := Left'First;
      Right_Index : Index_Type := Right'First;
      Result_Index : Index_Type := Result'First;
   begin
      while Left_Index <= Left'Last and Right_Index <= Right'Last loop
         if Left(Left_Index) <= Right(Right_Index) then
            Result(Result_Index) := Left(Left_Index);
            Left_Index := Index_Type'Succ(Left_Index); -- increment Left_Index
         else
            Result(Result_Index) := Right(Right_Index);
            Right_Index := Index_Type'Succ(Right_Index); -- increment Right_Index
         end if;
         Result_Index := Index_Type'Succ(Result_Index); -- increment Result_Index
      end loop;
      if Left_Index <= Left'Last then
         Result(Result_Index..Result'Last) := Left(Left_Index..Left'Last);
      end if;
      if Right_Index <= Right'Last then
         Result(Result_Index..Result'Last) := Right(Right_Index..Right'Last);
      end if;
      return Result;
   end Merge;

   ----------
   -- Sort --
   ----------

   function Sort (Item : Collection_Type) return Collection_Type is
      Result : Collection_Type(Item'range);
      Middle : Index_Type;
   begin
      if Item'Length <= 1 then
         return Item;
      else
         Middle := Index_Type'Val((Item'Length / 2) + Index_Type'Pos(Item'First));
         declare
            Left : Collection_Type(Item'First..Index_Type'Pred(Middle));
            Right : Collection_Type(Middle..Item'Last);
         begin
            for I in Left'range loop
               Left(I) := Item(I);
            end loop;
            for I in Right'range loop
               Right(I) := Item(I);
            end loop;
            Left := Sort(Left);
            Right := Sort(Right);
            Result := Merge(Left, Right);
         end;
         return Result;
      end if;
   end Sort;

end Mergesort;
```

The following code provides an usage example for the generic package defined above.

```ada
with Ada.Text_Io; use Ada.Text_Io;
with Mergesort;

procedure Mergesort_Test is
   type List_Type is array(Positive range <>) of Integer;
   package List_Sort is new Mergesort(Integer, Positive, List_Type);
   procedure Print(Item : List_Type) is
   begin
      for I in Item'range loop
         Put(Integer'Image(Item(I)));
      end loop;
      New_Line;
   end Print;

   List : List_Type := (1, 5, 2, 7, 3, 9, 4, 6);
begin
   Print(List);
   Print(List_Sort.Sort(List));
end Mergesort_Test;
```

```txt

 1 5 2 7 3 9 4 6
 1 2 3 4 5 6 7 9

```



## ALGOL 68

Below are two variants of the same routine.  If copying the DATA type to
a different memory location is expensive, then the optimised version should
be used as the DATA elements are handled indirectly.

```algol68
MODE DATA = CHAR;

PROC merge sort = ([]DATA m)[]DATA: (
    IF LWB m >= UPB m THEN
        m
    ELSE
        INT middle = ( UPB m + LWB m ) OVER 2;
        []DATA left = merge sort(m[:middle]);
        []DATA right = merge sort(m[middle+1:]);
        flex merge(left, right)[AT LWB m]
    FI
);

# FLEX version: A demonstration of FLEX for manipulating arrays #
PROC flex merge = ([]DATA in left, in right)[]DATA:(
    [UPB in left + UPB in right]DATA result;
    FLEX[0]DATA left := in left;
    FLEX[0]DATA right := in right;

    FOR index TO UPB result DO
        # change the direction of this comparison to change the direction of the sort #
        IF LWB right > UPB right THEN
            result[index:] := left;
            stop iteration
        ELIF LWB left > UPB left THEN
            result[index:] := right;
            stop iteration
        ELIF left[1] <= right[1] THEN
            result[index] := left[1];
            left := left[2:]
        ELSE
            result[index] := right[1];
            right := right[2:]
        FI
    OD;
stop iteration:
    result
);

[32]CHAR char array data := "big fjords vex quick waltz nymph";
print((merge sort(char array data), new line));
```

```txt

     abcdefghiijklmnopqrstuvwxyz

```

Optimised version:
# avoids FLEX array copies and manipulations
# avoids type DATA memory copies, useful in cases where DATA is a large STRUCT

```algol68
PROC opt merge sort = ([]REF DATA m)[]REF DATA: (
    IF LWB m >= UPB m THEN
        m
    ELSE
        INT middle = ( UPB m + LWB m ) OVER 2;
        []REF DATA left = opt merge sort(m[:middle]);
        []REF DATA right = opt merge sort(m[middle+1:]);
        opt merge(left, right)[AT LWB m]
    FI
);

PROC opt merge = ([]REF DATA left, right)[]REF DATA:(
    [UPB left - LWB left + 1 + UPB right - LWB right + 1]REF DATA result;
    INT index left:=LWB left, index right:=LWB right;

    FOR index TO UPB result DO
        # change the direction of this comparison to change the direction of the sort #
        IF index right > UPB right THEN
            result[index:] := left[index left:];
            stop iteration
        ELIF index left > UPB left THEN
            result[index:] := right[index right:];
            stop iteration
        ELIF left[index left] <= right[index right] THEN
            result[index] := left[index left]; index left +:= 1
        ELSE
            result[index] := right[index right]; index right +:= 1
        FI
    OD;
stop iteration:
    result
);

# create an array of pointers to the data being sorted #
[UPB char array data]REF DATA data; FOR i TO UPB char array data DO data[i] := char array data[i] OD;

[]REF CHAR result = opt merge sort(data);
FOR i TO UPB result DO print((result[i])) OD; print(new line)
```

```txt

     abcdefghiijklmnopqrstuvwxyz

```




## Astro


```python
fun mergesort(m):
    if m.lenght <= 1: return m
    let middle = floor m.lenght / 2
    let left = merge(m[:middle])
    let right = merge(m[middle-1:]);

fun merge(left, right):
    let result = []
    while not (left.isempty or right.isempty):
        if left[1] <= right[1]:
            result.push! left.shift!()
        else:
            result.push! right.shift!()
    result.push! left.push! right

let arr = [7, 6, 5, 9, 8, 4, 3, 1, 2, 0]
print mergesort arr
```


== [[AutoHotkey_L]] ==
AutoHotkey_L has true array support and can dynamically grow and shrink its arrays at run time.
This version of Merge Sort only needs '''n''' locations to sort.
[http://www.autohotkey.com/forum/viewtopic.php?t=77693&highlight=| AHK forum post]

```AutoHotkey
#NoEnv

Test := []
Loop 100 {
    Random n, 0, 999
    Test.Insert(n)
}
Result := MergeSort(Test)
Loop % Result.MaxIndex() {
    MsgBox, 1, , % Result[A_Index]
    IfMsgBox Cancel
        Break
}
Return


/*
    Function MergeSort
        Sorts an array by first recursively splitting it down to its
        individual elements and then merging those elements in their
        correct order.

    Parameters
        Array   The array to be sorted

    Returns
        The sorted array
*/
MergeSort(Array)
    {
        ; Return single element arrays
        If (! Array.HasKey(2))
            Return Array

        ; Split array into Left and Right halfs
        Left := [], Right := [], Middle := Array.MaxIndex() // 2
        Loop % Middle
            Right.Insert(Array.Remove(Middle-- + 1)), Left.Insert(Array.Remove(1))
        If (Array.MaxIndex())
            Right.Insert(Array.Remove(1))

        Left := MergeSort(Left), Right := MergeSort(Right)

        ; If all the Right values are greater than all the
        ; Left values, just append Right at the end of Left.
        If (Left[Left.MaxIndex()] <= Right[1]) {
            Loop % Right.MaxIndex()
                Left.Insert(Right.Remove(1))
            Return Left
        }
        ; Loop until one of the arrays is empty
        While(Left.MaxIndex() and Right.MaxIndex())
            Left[1] <= Right[1] ? Array.Insert(Left.Remove(1))
                                : Array.Insert(Right.Remove(1))

        Loop % Left.MaxIndex()
            Array.Insert(Left.Remove(1))

        Loop % Right.MaxIndex()
            Array.Insert(Right.Remove(1))

        Return Array
    }
```



## AutoHotkey

Contributed by Laszlo on the ahk [http://www.autohotkey.com/forum/post-276483.html#276483 forum]

```AutoHotkey
MsgBox % MSort("")
MsgBox % MSort("xxx")
MsgBox % MSort("3,2,1")
MsgBox % MSort("dog,000000,cat,pile,abcde,1,zz,xx,z")

MSort(x) {                                                  ; Merge-sort of a comma separated list
   If (2 > L:=Len(x))
       Return x                                             ; empty or single item lists are sorted
   StringGetPos p, x, `,, % "L" L//2                        ; Find middle comma
   Return Merge(MSort(SubStr(x,1,p)), MSort(SubStr(x,p+2))) ; Split, Sort, Merge
}

Len(list) {
   StringReplace t, list,`,,,UseErrorLevel                  ; #commas -> ErrorLevel
   Return list="" ? 0 : ErrorLevel+1
}

Item(list,ByRef p) {                                        ; item at position p, p <- next position
   Return (p := InStr(list,",",0,i:=p+1)) ? SubStr(list,i,p-i) : SubStr(list,i)
}

Merge(list0,list1) {                                        ; Merge 2 sorted lists
   IfEqual list0,, Return list1
   IfEqual list1,, Return list0
   i0 := Item(list0,p0:=0)
   i1 := Item(list1,p1:=0)
   Loop  {
      i := i0>i1
      list .= "," i%i%                                      ; output smaller
      If (p%i%)
         i%i% := Item(list%i%,p%i%)                         ; get next item from processed list
      Else {
         i ^= 1                                             ; list is exhausted: attach rest of other
         Return SubStr(list "," i%i% (p%i% ? "," SubStr(list%i%,p%i%+1) : ""), 2)
      }
   }
}
```



## BBC BASIC


```BBCBASIC
DEFPROC_MergeSort(Start%,End%)
REM *****************************************************************
REM This procedure Merge Sorts the chunk of data% bounded by
REM Start% & End%.
REM *****************************************************************

LOCAL Middle%
IF End%=Start% ENDPROC

IF End%-Start%=1 THEN
   IF data%(End%)<data%(Start%) THEN
      SWAP data%(Start%),data%(End%)
   ENDIF
   ENDPROC
ENDIF

Middle%=Start%+(End%-Start%)/2

PROC_MergeSort(Start%,Middle%)
PROC_MergeSort(Middle%+1,End%)
PROC_Merge(Start%,Middle%,End%)

ENDPROC
:
DEF PROC_Merge(Start%,Middle%,End%)

LOCAL fh_size%
fh_size% = Middle%-Start%+1

FOR I%=0 TO fh_size%-1
  fh%(I%)=data%(Start%+I%)
NEXT I%

I%=0
J%=Middle%+1
K%=Start%

REPEAT
  IF fh%(I%) <= data%(J%) THEN
    data%(K%)=fh%(I%)
    I%+=1
    K%+=1
  ELSE
    data%(K%)=data%(J%)
    J%+=1
    K%+=1
  ENDIF
UNTIL I%=fh_size% OR J%>End%

WHILE I% < fh_size%
  data%(K%)=fh%(I%)
  I%+=1
  K%+=1
ENDWHILE

ENDPROC
```

Usage would look something like this example which sorts a series of 1000 random integers:

```BBCBASIC
REM Example of merge sort usage.
Size%=1000

S1%=Size%/2

DIM data%(Size%)
DIM fh%(S1%)

FOR I%=1 TO Size%
  data%(I%)=RND(100000)
NEXT

PROC_MergeSort(1,Size%)

END
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

void merge (int *a, int n, int m) {
    int i, j, k;
    int *x = malloc(n * sizeof (int));
    for (i = 0, j = m, k = 0; k < n; k++) {
        x[k] = j == n      ? a[i++]
             : i == m      ? a[j++]
             : a[j] < a[i] ? a[j++]
             :               a[i++];
    }
    for (i = 0; i < n; i++) {
        a[i] = x[i];
    }
    free(x);
}

void merge_sort (int *a, int n) {
    if (n < 2)
        return;
    int m = n / 2;
    merge_sort(a, m);
    merge_sort(a + m, n - m);
    merge(a, n, m);
}

int main () {
    int a[] = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1};
    int n = sizeof a / sizeof a[0];
    int i;
    for (i = 0; i < n; i++)
        printf("%d%s", a[i], i == n - 1 ? "\n" : " ");
    merge_sort(a, n);
    for (i = 0; i < n; i++)
        printf("%d%s", a[i], i == n - 1 ? "\n" : " ");
    return 0;
}
```

```txt

4 65 2 -31 0 99 2 83 782 1
-31 0 1 2 2 4 65 83 99 782

```



## C++


```cpp
#include <iterator>
#include <algorithm> // for std::inplace_merge
#include <functional> // for std::less

template<typename RandomAccessIterator, typename Order>
 void mergesort(RandomAccessIterator first, RandomAccessIterator last, Order order)
{
  if (last - first > 1)
  {
    RandomAccessIterator middle = first + (last - first) / 2;
    mergesort(first, middle, order);
    mergesort(middle, last, order);
    std::inplace_merge(first, middle, last, order);
  }
}

template<typename RandomAccessIterator>
 void mergesort(RandomAccessIterator first, RandomAccessIterator last)
{
  mergesort(first, last, std::less<typename std::iterator_traits<RandomAccessIterator>::value_type>());
}
```


## C#
```c#
namespace Sort {
  using System;

  public class MergeSort<T> where T : IComparable {
    #region Constants
    private const Int32 mergesDefault = 6;
    private const Int32 insertionLimitDefault = 12;
    #endregion

    #region Properties
    protected Int32[] Positions { get; set; }

    private Int32 merges;
    public Int32 Merges {
      get { return merges; }
      set {
        // A minimum of 2 merges are required
        if (value > 1)
          merges = value;
        else
          throw new ArgumentOutOfRangeException();

        if (Positions == null || Positions.Length != merges)
          Positions = new Int32[merges];
      }
    }

    public Int32 InsertionLimit { get; set; }
    #endregion

    #region Constructors
    public MergeSort(Int32 merges, Int32 insertionLimit) {
      Merges = merges;
      InsertionLimit = insertionLimit;
    }

    public MergeSort()
      : this(mergesDefault, insertionLimitDefault) {
    }
    #endregion

    #region Sort Methods
    public void Sort(T[] entries) {
      // Allocate merge buffer
      var entries2 = new T[entries.Length];
      Sort(entries, entries2, 0, entries.Length - 1);
    }

    // Top-Down K-way Merge Sort
    public void Sort(T[] entries1, T[] entries2, Int32 first, Int32 last) {
      var length = last + 1 - first;
      if (length < 2)
        return;
      else if (length < InsertionLimit) {
        InsertionSort<T>.Sort(entries1, first, last);
        return;
      }

      var left = first;
      var size = ceiling(length, Merges);
      for (var remaining = length; remaining > 0; remaining -= size, left += size) {
        var right = left + Math.Min(remaining, size) - 1;
        Sort(entries1, entries2, left, right);
      }

      Merge(entries1, entries2, first, last);
      Array.Copy(entries2, first, entries1, first, length);
    }
    #endregion

    #region Merge Methods
    public void Merge(T[] entries1, T[] entries2, Int32 first, Int32 last) {
      Array.Clear(Positions, 0, Merges);
      // This implementation has a quadratic time dependency on the number of merges
      for (var index = first; index <= last; index++)
        entries2[index] = remove(entries1, first, last);
    }

    private T remove(T[] entries, Int32 first, Int32 last) {
      var entry = default(T);
      var found = (Int32?)null;
      var length = last + 1 - first;

      var index = 0;
      var left = first;
      var size = ceiling(length, Merges);
      for (var remaining = length; remaining > 0; remaining -= size, left += size, index++) {
        var position = Positions[index];
        if (position < Math.Min(remaining, size)) {
          var next = entries[left + position];
          if (!found.HasValue || entry.CompareTo(next) > 0) {
            found = index;
            entry = next;
          }
        }
      }

      // Remove entry
      Positions[found.Value]++;
      return entry;
    }
    #endregion

    #region Math Methods
    private static Int32 ceiling(Int32 numerator, Int32 denominator) {
      return (numerator + denominator - 1) / denominator;
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

```c#
  using Sort;
  using System;

  class Program {
    static void Main(String[] args) {
      var entries = new Int32[] { 7, 5, 2, 6, 1, 4, 2, 6, 3 };
      var sorter = new MergeSort<Int32>();
      sorter.Sort(entries);
      Console.WriteLine(String.Join(" ", entries));
    }
  }
```

```txt
1 2 2 3 4 5 6 6 7
```



## Clojure

```lisp

(defn merge [left right]
  (cond (nil? left) right
        (nil? right) left
        :else (let [[l & *left] left
                    [r & *right] right]
                (if (<= l r) (cons l (merge *left right))
                             (cons r (merge left *right))))))

(defn merge-sort [list]
  (if (< (count list) 2)
    list
    (let [[left right] (split-at (/ (count list) 2) list)]
      (merge (merge-sort left) (merge-sort right)))))

```



## COBOL

Cobol cannot do recursion, so this version simulates recursion. The working storage is therefore pretty complex, so I have shown the whole program, not just the working procedure division parts.

```COBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      MERGESORT.
       AUTHOR.                          DAVE STRATFORD.
       DATE-WRITTEN.                    APRIL 2010.
       INSTALLATION.                    HEXAGON SYSTEMS LIMITED.
      ******************************************************************
      *                            MERGE SORT                          *
      *  The Merge sort uses a completely different paradigm, one of   *
      * divide and conquer, to many of the other sorts. The data set   *
      * is split into smaller sub sets upon which are sorted and then  *
      * merged together to form the final sorted data set.             *
      *  This version uses the recursive method. Split the data set in *
      * half and perform a merge sort on each half. This in turn splits*
      * each half again and again until each set is just one or 2 items*
      * long. A set of one item is already sorted so is ignored, a set *
      * of two is compared and swapped as necessary. The smaller data  *
      * sets are then repeatedly merged together to eventually form the*
      * full, sorted, set.                                             *
      *  Since cobol cannot do recursion this module only simulates it *
      * so is not as fast as a normal recursive version would be.      *
      *  Scales very well to larger data sets, its relative complexity *
      * means it is not suited to sorting smaller data sets: use an    *
      * Insertion sort instead as the Merge sort is a stable sort.     *
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                 ICL VME.
       OBJECT-COMPUTER.                 ICL VME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FA-INPUT-FILE  ASSIGN FL01.
           SELECT FB-OUTPUT-FILE ASSIGN FL02.

       DATA DIVISION.
       FILE SECTION.
       FD  FA-INPUT-FILE.
       01  FA-INPUT-REC.
         03  FA-DATA                    PIC 9(6).

       FD  FB-OUTPUT-FILE.
       01  FB-OUTPUT-REC                PIC 9(6).

       WORKING-STORAGE SECTION.
       01  WA-IDENTITY.
         03  WA-PROGNAME                PIC X(10) VALUE "MERGESORT".
         03  WA-VERSION                 PIC X(6) VALUE "000001".

       01  WB-TABLE.
         03  WB-ENTRY                   PIC 9(8) COMP SYNC OCCURS 100000
                                                 INDEXED BY WB-IX-1
                                                            WB-IX-2.

       01  WC-VARS.
         03  WC-SIZE                    PIC S9(8) COMP SYNC.
         03  WC-TEMP                    PIC S9(8) COMP SYNC.
         03  WC-START                   PIC S9(8) COMP SYNC.
         03  WC-MIDDLE                  PIC S9(8) COMP SYNC.
         03  WC-END                     PIC S9(8) COMP SYNC.

       01  WD-FIRST-HALF.
         03  WD-FH-MAX                  PIC S9(8) COMP SYNC.
         03  WD-ENTRY                   PIC 9(8) COMP SYNC OCCURS 50000
                                                 INDEXED BY WD-IX.

       01  WF-CONDITION-FLAGS.
         03  WF-EOF-FLAG                PIC X.
           88  END-OF-FILE              VALUE "Y".
         03  WF-EMPTY-FILE-FLAG         PIC X.
           88  EMPTY-FILE               VALUE "Y".

       01  WS-STACK.
      * This stack is big enough to sort a list of 1million items.
         03  WS-STACK-ENTRY OCCURS 20 INDEXED BY WS-STACK-TOP.
           05  WS-START                 PIC S9(8) COMP SYNC.
           05  WS-MIDDLE                PIC S9(8) COMP SYNC.
           05  WS-END                   PIC S9(8) COMP SYNC.
           05  WS-FS-FLAG               PIC X.
             88  FIRST-HALF             VALUE "F".
             88  SECOND-HALF            VALUE "S".
             88  WS-ALL                 VALUE "A".
           05  WS-IO-FLAG               PIC X.
             88  WS-IN                  VALUE "I".
             88  WS-OUT                 VALUE "O".

       PROCEDURE DIVISION.
       A-MAIN SECTION.
       A-000.
           PERFORM B-INITIALISE.

           IF NOT EMPTY-FILE
              PERFORM C-PROCESS.

           PERFORM D-FINISH.

       A-999.
           STOP RUN.

       B-INITIALISE SECTION.
       B-000.
           DISPLAY "*** " WA-PROGNAME " VERSION "
                          WA-VERSION " STARTING ***".

           MOVE ALL "N" TO WF-CONDITION-FLAGS.
           OPEN INPUT FA-INPUT-FILE.
           SET WB-IX-1 TO 0.

           READ FA-INPUT-FILE AT END MOVE "Y" TO WF-EOF-FLAG
                                                 WF-EMPTY-FILE-FLAG.

           PERFORM BA-READ-INPUT UNTIL END-OF-FILE.

           CLOSE FA-INPUT-FILE.

           SET WC-SIZE TO WB-IX-1.

       B-999.
           EXIT.

       BA-READ-INPUT SECTION.
       BA-000.
           SET WB-IX-1 UP BY 1.
           MOVE FA-DATA TO WB-ENTRY(WB-IX-1).

           READ FA-INPUT-FILE AT END MOVE "Y" TO WF-EOF-FLAG.

       BA-999.
           EXIT.

       C-PROCESS SECTION.
       C-000.
           DISPLAY "SORT STARTING".

           MOVE 1           TO WS-START(1).
           MOVE WC-SIZE     TO WS-END(1).
           MOVE "F"         TO WS-FS-FLAG(1).
           MOVE "I"         TO WS-IO-FLAG(1).
           SET WS-STACK-TOP TO 2.

           PERFORM E-MERGE-SORT UNTIL WS-OUT(1).

           DISPLAY "SORT FINISHED".

       C-999.
           EXIT.

       D-FINISH SECTION.
       D-000.
           OPEN OUTPUT FB-OUTPUT-FILE.
           SET WB-IX-1 TO 1.

           PERFORM DA-WRITE-OUTPUT UNTIL WB-IX-1 > WC-SIZE.

           CLOSE FB-OUTPUT-FILE.

           DISPLAY "*** " WA-PROGNAME " FINISHED ***".

       D-999.
           EXIT.

       DA-WRITE-OUTPUT SECTION.
       DA-000.
           WRITE FB-OUTPUT-REC FROM WB-ENTRY(WB-IX-1).
           SET WB-IX-1 UP BY 1.

       DA-999.
           EXIT.

      ******************************************************************
       E-MERGE-SORT SECTION.
      *
### ===============
                                           *
      * This section controls the simulated recursion.                 *
      ******************************************************************
       E-000.
           IF WS-OUT(WS-STACK-TOP - 1)
              GO TO E-010.

           MOVE WS-START(WS-STACK-TOP - 1) TO WC-START.
           MOVE WS-END(WS-STACK-TOP - 1)   TO WC-END.

      * First check size of part we are dealing with.
           IF WC-END - WC-START = 0
      * Only 1 number in range, so simply set for output, and move on
              MOVE "O" TO WS-IO-FLAG(WS-STACK-TOP - 1)
              GO TO E-010.

           IF WC-END - WC-START = 1
      * 2 numbers, so compare and swap as necessary. Set for output
              MOVE "O" TO WS-IO-FLAG(WS-STACK-TOP - 1)
              IF WB-ENTRY(WC-START) > WB-ENTRY(WC-END)
                 MOVE WB-ENTRY(WC-START) TO WC-TEMP
                 MOVE WB-ENTRY(WC-END) TO WB-ENTRY(WC-START)
                 MOVE WC-TEMP TO WB-ENTRY(WC-END)
                 GO TO E-010
              ELSE
                 GO TO E-010.

      * More than 2, so split and carry on down
           COMPUTE WC-MIDDLE = ( WC-START + WC-END ) / 2.

           MOVE WC-START  TO WS-START(WS-STACK-TOP).
           MOVE WC-MIDDLE TO WS-END(WS-STACK-TOP).
           MOVE "F"       TO WS-FS-FLAG(WS-STACK-TOP).
           MOVE "I"       TO WS-IO-FLAG(WS-STACK-TOP).
           SET WS-STACK-TOP UP BY 1.

           GO TO E-999.

       E-010.
           SET WS-STACK-TOP DOWN BY 1.

           IF SECOND-HALF(WS-STACK-TOP)
              GO TO E-020.

           MOVE WS-START(WS-STACK-TOP - 1) TO WC-START.
           MOVE WS-END(WS-STACK-TOP - 1)   TO WC-END.
           COMPUTE WC-MIDDLE = ( WC-START + WC-END ) / 2 + 1.

           MOVE WC-MIDDLE TO WS-START(WS-STACK-TOP).
           MOVE WC-END    TO WS-END(WS-STACK-TOP).
           MOVE "S"       TO WS-FS-FLAG(WS-STACK-TOP).
           MOVE "I"       TO WS-IO-FLAG(WS-STACK-TOP).
           SET WS-STACK-TOP UP BY 1.

           GO TO E-999.

       E-020.
           MOVE WS-START(WS-STACK-TOP - 1) TO WC-START.
           MOVE WS-END(WS-STACK-TOP - 1)   TO WC-END.
           COMPUTE WC-MIDDLE = ( WC-START + WC-END ) / 2.
           PERFORM H-PROCESS-MERGE.
           MOVE "O" TO WS-IO-FLAG(WS-STACK-TOP - 1).

       E-999.
           EXIT.

      ******************************************************************
       H-PROCESS-MERGE SECTION.
      *
### ==================
                                        *
      * This section identifies which data is to be merged, and then   *
      * merges the two data streams into a single larger data stream.  *
      ******************************************************************
       H-000.
           INITIALISE WD-FIRST-HALF.
           COMPUTE WD-FH-MAX = WC-MIDDLE - WC-START + 1.
           SET WD-IX                        TO 1.

           PERFORM HA-COPY-OUT VARYING WB-IX-1 FROM WC-START BY 1
                               UNTIL WB-IX-1 > WC-MIDDLE.

           SET WB-IX-1 TO WC-START.
           SET WB-IX-2 TO WC-MIDDLE.
           SET WB-IX-2 UP BY 1.
           SET WD-IX   TO 1.

           PERFORM HB-MERGE UNTIL WD-IX > WD-FH-MAX OR WB-IX-2 > WC-END.

           PERFORM HC-COPY-BACK UNTIL WD-IX > WD-FH-MAX.

       H-999.
           EXIT.

       HA-COPY-OUT SECTION.
       HA-000.
           MOVE WB-ENTRY(WB-IX-1) TO WD-ENTRY(WD-IX).
           SET WD-IX UP BY 1.

       HA-999.
           EXIT.

       HB-MERGE SECTION.
       HB-000.
           IF WB-ENTRY(WB-IX-2) < WD-ENTRY(WD-IX)
              MOVE WB-ENTRY(WB-IX-2) TO WB-ENTRY(WB-IX-1)
              SET WB-IX-2            UP BY 1
           ELSE
              MOVE WD-ENTRY(WD-IX) TO WB-ENTRY(WB-IX-1)
              SET WD-IX            UP BY 1.

           SET WB-IX-1 UP BY 1.

       HB-999.
           EXIT.

       HC-COPY-BACK SECTION.
       HC-000.
           MOVE WD-ENTRY(WD-IX) TO WB-ENTRY(WB-IX-1).
           SET WD-IX            UP BY 1.
           SET WB-IX-1          UP BY 1.

       HC-999.
           EXIT.
```



## CoffeeScript


```coffeescript
# This is a simple version of mergesort that returns brand-new arrays.
# A more sophisticated version would do more in-place optimizations.
merge_sort = (arr) ->
  if arr.length <= 1
    return (elem for elem in arr)
  m = Math.floor(arr.length / 2)
  arr1 = merge_sort(arr.slice 0, m)
  arr2 = merge_sort(arr.slice m)
  result = []
  p1 = p2 = 0
  while true
    if p1 >= arr1.length
      if p2 >= arr2.length
        return result
      result.push arr2[p2]
      p2 += 1
    else if p2 >= arr2.length or arr1[p1] < arr2[p2]
      result.push arr1[p1]
      p1 += 1
    else
      result.push arr2[p2]
      p2 += 1

do ->
  console.log merge_sort [2,4,6,8,1,3,5,7,9,10,11,0,13,12]
```

```txt

> coffee mergesort.coffee
[ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 ]

```



## Common Lisp


```lisp
(defun merge-sort (result-type sequence predicate)
   (let ((split (floor (length sequence) 2)))
     (if (zerop split)
       (copy-seq sequence)
       (merge result-type (merge-sort result-type (subseq sequence 0 split) predicate)
                          (merge-sort result-type (subseq sequence split)   predicate)
                          predicate))))
```


<tt>merge</tt> is a standard Common Lisp function.

 > (merge-sort 'list (list 1 3 5 7 9 8 6 4 2) #'<)
 (1 2 3 4 5 6 7 8 9)


## Crystal

```ruby
def merge_sort(a : Array(Int32)) : Array(Int32)
  return a if a.size <= 1
  m = a.size / 2
  lt = merge_sort(a[0 ... m])
  rt = merge_sort(a[m .. -1])
  return merge(lt, rt)
end

def merge(lt : Array(Int32), rt : Array(Int32)) : Array(Int32)
  result = Array(Int32).new
  until lt.empty? || rt.empty?
    result << (lt.first < rt.first ? lt.shift : rt.shift)
  end
  return result + lt + rt
end

a = [7, 6, 5, 9, 8, 4, 3, 1, 2, 0]
puts merge_sort(a) # => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Curry

Copied from [http://www.informatik.uni-kiel.de/~curry/examples/ Curry: Example Programs]

```curry
-- merge sort: sorting two lists by merging the sorted first
-- and second half of the list

sort :: ([a] -> [a] -> [a] -> Success) -> [a] -> [a] -> Success

sort merge xs ys =
   if length xs < 2 then ys =:= xs
   else sort merge (firsthalf xs) us
        & sort merge (secondhalf xs) vs
        & merge us vs ys
   where us,vs free


intMerge :: [Int] -> [Int] -> [Int] -> Success

intMerge []     ys     zs =  zs =:= ys
intMerge (x:xs) []     zs =  zs =:= x:xs
intMerge (x:xs) (y:ys) zs =
   if (x > y) then intMerge (x:xs) ys us & zs =:= y:us
              else intMerge xs (y:ys) vs & zs =:= x:vs
   where us,vs free

firsthalf  xs = take (length xs `div` 2) xs
secondhalf xs = drop (length xs `div` 2) xs



goal1 xs = sort intMerge [3,1,2] xs
goal2 xs = sort intMerge [3,1,2,5,4,8] xs
goal3 xs = sort intMerge [3,1,2,5,4,8,6,7,2,9,1,4,3] xs
```



## D

Arrays only, not in-place.

```d
import std.stdio, std.algorithm, std.array, std.range;

T[] mergeSorted(T)(in T[] D) /*pure nothrow @safe*/ {
    if (D.length < 2)
        return D.dup;
    return [D[0 .. $ / 2].mergeSorted, D[$ / 2 .. $].mergeSorted]
           .nWayUnion.array;
}

void main() {
    [3, 4, 2, 5, 1, 6].mergeSorted.writeln;
}
```



### Alternative Version

This in-place version allocates the auxiliary memory on the stack,
making life easier for the garbage collector,
but with risk of stack overflow (same output):

```d
import std.stdio, std.algorithm, core.stdc.stdlib, std.exception,
       std.range;

void mergeSort(T)(T[] data) if (hasSwappableElements!(typeof(data))) {
    immutable L = data.length;
    if (L < 2) return;
    T* ptr = cast(T*)alloca(L * T.sizeof);
    enforce(ptr != null);
    ptr[0 .. L] = data[];
    mergeSort(ptr[0 .. L/2]);
    mergeSort(ptr[L/2 .. L]);
    [ptr[0 .. L/2], ptr[L/2 .. L]].nWayUnion().copy(data);
}

void main() {
    auto a = [3, 4, 2, 5, 1, 6];
    a.mergeSort();
    writeln(a);
}
```

<!-- Missing in-place version for arrays -->
<!-- Missing generic version for Ranges -->


## Dart


```dart
void main() {
  MergeSortInDart sampleSort = MergeSortInDart();

  List<int> theResultingList = sampleSort.sortTheList([54, 89, 125, 47899, 32, 61, 42, 895647, 215, 345, 6, 21, 2, 78]);

  print('Here\'s the sorted list: ${theResultingList}');
}

/////////////////////////////////////

class MergeSortInDart {

  List<int> sortedList;

  // In Dart we often put helper functions at the bottom.
  // You could put them anywhere, we just like it this way
  // for organizational purposes. It's nice to be able to
  // read them in the order they're called.

  // Start here
  List<int> sortTheList(List<int> sortThis){
    // My parameters are listed vertically for readability. Dart
    // doesn't care how you list them, vertically or horizontally.
    sortedList = mSort(
      sortThis,
      sortThis.sublist(0, sortThis.length),
      sortThis.length,
    );
    return sortThis;
  }

  mSort(
    List<int> sortThisList,
    List<int> tempList,
    int thisListLength) {

    if (thisListLength == 1) {
      return;
    }

    // In Dart using ~/ is more efficient than using .floor()
    int middle = (thisListLength ~/ 2);

    // If you use something in a try/on/catch/finally block then
    // be sure to declare it outside the block (for scope)
    List<int> tempLeftList;

    // This was used for troubleshooting. It was left here to show
    // how a basic block try/on can be better than a debugPrint since
    // it won't print unless there's a problem.
    try {
      tempLeftList = tempList.sublist(0, middle);
    } on RangeError {
      print(
          'tempLeftList length = ${tempList.length}, thisListLength '
            'was supposedly $thisListLength and Middle was $middle');
    }

    // If you see "myList.getRange(x,y)" that's a sign the code is
    // from Dart 1 and needs to be updated. It's "myList.sublist" in Dart 2
    List<int> tempRightList = tempList.sublist(middle);

    // Left side.
    mSort(
      tempLeftList,
      sortThisList.sublist(0, middle),
      middle,
    );

    // Right side.
    mSort(
      tempRightList,
      sortThisList.sublist(middle),
      sortThisList.length - middle,
    );

    // Merge it.
    dartMerge(
      tempLeftList,
      tempRightList,
      sortThisList,
    );
  }

  dartMerge(
    List<int> leftSide,
    List<int> rightSide,
    List<int> sortThisList,
    ) {
    int index = 0;
    int elementValue;

    // This should be human readable.
    while (leftSide.isNotEmpty && rightSide.isNotEmpty) {
      if (rightSide[0] < leftSide[0]) {
        elementValue = rightSide[0];
        rightSide.removeRange(0, 1);
      } else {
        elementValue = leftSide[0];
        leftSide.removeRange(0, 1);
      }
      sortThisList[index++] = elementValue;
    }

    while (leftSide.isNotEmpty) {
      elementValue = leftSide[0];
      leftSide.removeRange(0, 1);
      sortThisList[index++] = elementValue;
    }

    while (rightSide.isNotEmpty) {
      elementValue = rightSide[0];
      rightSide.removeRange(0, 1);
      sortThisList[index++] = elementValue;
    }
    sortedList = sortThisList;
  }
}
```



## E


```e
def merge(var xs :List, var ys :List) {
    var result := []
    while (xs =~ [x] + xr && ys =~ [y] + yr) {
        if (x <= y) {
            result with= x
            xs := xr
        } else {
            result with= y
            ys := yr
        }
    }
    return result + xs + ys
}

def sort(list :List) {
    if (list.size() <= 1) { return list }
    def split := list.size() // 2
    return merge(sort(list.run(0, split)),
                 sort(list.run(split)))
}
```


## EasyLang


<lang>subr merge
  mid = left + sz
  if mid > sz_data
    mid = sz_data
  .
  right = mid + sz
  if right > sz_data
    right = sz_data
  .
  l = left
  r = mid
  for i = left to right - 1
    if r = right or l < mid and tmp[l] < tmp[r]
      data[i] = tmp[l]
      l += 1
    else
      data[i] = tmp[r]
      r += 1
    .
  .
.
subr sort
  sz_data = len data[]
  len tmp[] sz_data
  sz = 1
  while sz < sz_data
    swap tmp[] data[]
    left = 0
    while left < sz_data
      call merge
      left += sz + sz
    .
    sz += sz
  .
.
data[] = [ 29 4 72 44 55 26 27 77 92 5 ]
call sort
print data[]
```



## Eiffel


```Eiffel

class
	MERGE_SORT [G -> COMPARABLE]

create
	sort

feature

	sort (ar: ARRAY [G])
			-- Sorted array in ascending order.
		require
			ar_not_empty: not ar.is_empty
		do
			create sorted_array.make_empty
			mergesort (ar, 1, ar.count)
			sorted_array := ar
		ensure
			sorted_array_not_empty: not sorted_array.is_empty
			sorted: is_sorted (sorted_array, 1, sorted_array.count)
		end

	sorted_array: ARRAY [G]

feature {NONE}

	mergesort (ar: ARRAY [G]; l, r: INTEGER)
			-- Sorting part of mergesort.
		local
			m: INTEGER
		do
			if l < r then
				m := (l + r) // 2
				mergesort (ar, l, m)
				mergesort (ar, m + 1, r)
				merge (ar, l, m, r)
			end
		end

	merge (ar: ARRAY [G]; l, m, r: INTEGER)
			-- Merge part of mergesort.
		require
			positive_index_l: l >= 1
			positive_index_m: m >= 1
			positive_index_r: r >= 1
			ar_not_empty: not ar.is_empty
		local
			merged: ARRAY [G]
			h, i, j, k: INTEGER
		do
			i := l
			j := m + 1
			k := l
			create merged.make_filled (ar [1], 1, ar.count)
			from
			until
				i > m or j > r
			loop
				if ar.item (i) <= ar.item (j) then
					merged.force (ar.item (i), k)
					i := i + 1
				elseif ar.item (i) > ar.item (j) then
					merged.force (ar.item (j), k)
					j := j + 1
				end
				k := k + 1
			end
			if i > m then
				from
					h := j
				until
					h > r
				loop
					merged.force (ar.item (h), k + h - j)
					h := h + 1
				end
			elseif j > m then
				from
					h := i
				until
					h > m
				loop
					merged.force (ar.item (h), k + h - i)
					h := h + 1
				end
			end
			from
				h := l
			until
				h > r
			loop
				ar.item (h) := merged.item (h)
				h := h + 1
			end
		ensure
			is_partially_sorted: is_sorted (ar, l, r)
		end

	is_sorted (ar: ARRAY [G]; l, r: INTEGER): BOOLEAN
			-- Is 'ar' sorted in ascending order?
		require
			ar_not_empty: not ar.is_empty
			l_in_range: l >= 1
			r_in_range: r <= ar.count
		local
			i: INTEGER
		do
			Result := True
			from
				i := l
			until
				i = r
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
			test := <<2, 5, 66, -2, 0, 7>>
			io.put_string ("unsorted" + "%N")
			across
				test as ar
			loop
				io.put_string (ar.item.out + "%T")
			end
			io.put_string ("%N" + "sorted" + "%N")
			create merge.sort (test)
			across
				merge.sorted_array as ar
			loop
				io.put_string (ar.item.out + "%T")
			end
		end

	test: ARRAY [INTEGER]

	merge: MERGE_SORT [INTEGER]

end

```

```txt

unsorted
2 5 66 -2 0 7
sorted
-2 0 2 5 7 66

```



## Elixir


```elixir
defmodule Sort do
  def merge_sort(list) when length(list) <= 1, do: list
  def merge_sort(list) do
    {left, right} = Enum.split(list, div(length(list), 2))
    :lists.merge( merge_sort(left), merge_sort(right))
  end
end
```

Example:

```txt

iex(10)> Sort.merge_sort([5,3,9,4,1,6,8,2,7])
[1, 2, 3, 4, 5, 6, 7, 8, 9]

```



## Erlang

Below are two versions.  Both take advantage of built-in Erlang functions, lists:split and list:merge.  The multi-process version spawns a new process each time it splits.  This was slightly faster on a test system with only two cores, so it may not be the best implementation, however it does illustrate how easy it can be to add multi-threaded/process capabilities to a program.

Single-threaded version:

```erlang
mergeSort(L) when length(L) == 1 -> L;
mergeSort(L) when length(L) > 1 ->
    {L1, L2} = lists:split(length(L) div 2, L),
    lists:merge(mergeSort(L1), mergeSort(L2)).
```


Multi-process version:

```erlang
pMergeSort(L) when length(L) == 1 -> L;
pMergeSort(L) when length(L) > 1 ->
    {L1, L2} = lists:split(length(L) div 2, L),
    spawn(mergesort, pMergeSort2, [L1, self()]),
    spawn(mergesort, pMergeSort2, [L2, self()]),
    mergeResults([]).

pMergeSort2(L, Parent) when length(L) == 1 -> Parent ! L;
pMergeSort2(L, Parent) when length(L) > 1 ->
    {L1, L2} = lists:split(length(L) div 2, L),
    spawn(mergesort, pMergeSort2, [L1, self()]),
    spawn(mergesort, pMergeSort2, [L2, self()]),
    Parent ! mergeResults([]).
```



another multi-process version (number of processes == number of processor cores):

```erlang

merge_sort(List) -> m(List, erlang:system_info(schedulers)).

m([L],_) -> [L];
m(L, N) when N > 1  ->
    {L1,L2} = lists:split(length(L) div 2, L),
    {Parent, Ref} = {self(), make_ref()},
    spawn(fun()-> Parent ! {l1, Ref, m(L1, N-2)} end),
    spawn(fun()-> Parent ! {l2, Ref, m(L2, N-2)} end),
    {L1R, L2R} = receive_results(Ref, undefined, undefined),
    lists:merge(L1R, L2R);
m(L, _) -> {L1,L2} = lists:split(length(L) div 2, L), lists:merge(m(L1, 0), m(L2, 0)).

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

PROGRAM MERGESORT_DEMO

! Example of merge sort usage.

CONST SIZE%=100,S1%=50

DIM DTA%[SIZE%],FH%[S1%],STACK%[20,2]


PROCEDURE MERGE(START%,MIDDLE%,ENDS%)

LOCAL FHSIZE%

  FHSIZE%=MIDDLE%-START%+1

  FOR I%=0 TO FHSIZE%-1 DO
     FH%[I%]=DTA%[START%+I%]
  END FOR

  I%=0
  J%=MIDDLE%+1
  K%=START%

  REPEAT
    IF FH%[I%]<=DTA%[J%] THEN
        DTA%[K%]=FH%[I%]
        I%=I%+1
        K%=K%+1
      ELSE
        DTA%[K%]=DTA%[J%]
        J%=J%+1
        K%=K%+1
    END IF
  UNTIL I%=FHSIZE% OR J%>ENDS%

  WHILE I%<FHSIZE% DO
     DTA%[K%]=FH%[I%]
     I%=I%+1
     K%=K%+1
  END WHILE

END PROCEDURE

PROCEDURE MERGE_SORT(LEV->LEV)

! *****************************************************************
! This procedure Merge Sorts the chunk of DTA% bounded by
! Start% & Ends%.
! *****************************************************************

   LOCAL MIDDLE%

   IF ENDS%=START% THEN LEV=LEV-1 EXIT PROCEDURE END IF

   IF ENDS%-START%=1 THEN
      IF DTA%[ENDS%]<DTA%[START%] THEN
         SWAP(DTA%[START%],DTA%[ENDS%])
      END IF
      LEV=LEV-1
      EXIT PROCEDURE
   END IF

   MIDDLE%=START%+(ENDS%-START%)/2

   STACK%[LEV,0]=START%  STACK%[LEV,1]=ENDS%  STACK%[LEV,2]=MIDDLE%
   START%=START%  ENDS%=MIDDLE%
   MERGE_SORT(LEV+1->LEV)
   START%=STACK%[LEV,0]  ENDS%=STACK%[LEV,1]  MIDDLE%=STACK%[LEV,2]

   STACK%[LEV,0]=START%  STACK%[LEV,1]=ENDS%  STACK%[LEV,2]=MIDDLE%
   START%=MIDDLE%+1  ENDS%=ENDS%
   MERGE_SORT(LEV+1->LEV)
   START%=STACK%[LEV,0]  ENDS%=STACK%[LEV,1]  MIDDLE%=STACK%[LEV,2]

   MERGE(START%,MIDDLE%,ENDS%)

   LEV=LEV-1
END PROCEDURE

BEGIN
  FOR I%=1 TO SIZE% DO
     DTA%[I%]=RND(1)*10000
  END FOR

  START%=1  ENDS%=SIZE%
  MERGE_SORT(0->LEV)

  FOR I%=1 TO SIZE% DO
     WRITE("#####";DTA%[I%];)
  END FOR
  PRINT
END PROGRAM

```



## Euphoria


```euphoria
function merge(sequence left, sequence right)
    sequence result
    result = {}
    while length(left) > 0 and length(right) > 0 do
        if compare(left[1], right[1]) <= 0 then
            result = append(result, left[1])
            left = left[2..$]
        else
            result = append(result, right[1])
            right = right[2..$]
        end if
    end while
    return result & left & right
end function

function mergesort(sequence m)
    sequence left, right
    integer middle
    if length(m) <= 1 then
        return m
    else
        middle = floor(length(m)/2)
        left = mergesort(m[1..middle])
        right = mergesort(m[middle+1..$])
        if compare(left[$], right[1]) <= 0 then
            return left & right
        elsif compare(right[$], left[1]) <= 0 then
            return right & left
        else
            return merge(left, right)
        end if
    end if
end function

constant s = rand(repeat(1000,10))
? s
? mergesort(s)
```

```txt
{385,599,284,650,457,804,724,300,434,722}
{284,300,385,434,457,599,650,722,724,804}

```


=={{header|F Sharp|F#}}==

```fsharp
let split list =
    let rec aux l acc1 acc2 =
        match l with
            | [] -> (acc1,acc2)
            | [x] -> (x::acc1,acc2)
            | x::y::tail ->
                aux tail (x::acc1) (y::acc2)
    in aux list [] []

let rec merge l1 l2 =
    match (l1,l2) with
        | (x,[]) -> x
        | ([],y) -> y
        | (x::tx,y::ty) ->
            if x <= y then x::merge tx l2
            else y::merge l1 ty
let rec mergesort list =
    match list with
        | [] -> []
        | [x] -> [x]
        | _ -> let (l1,l2) = split list
               in merge (mergesort l1) (mergesort l2)
```



## Factor


```factor
: mergestep ( accum seq1 seq2 -- accum seq1 seq2 )
2dup [ first ] bi@ <
[ [ [ first ] [ rest-slice ] bi [ suffix ] dip ] dip ]
[ [ first ] [ rest-slice ] bi [ swap [ suffix ] dip ] dip ]
if ;

: merge ( seq1 seq2 -- merged )
[ { } ] 2dip
[ 2dup [ length 0 > ] bi@ and ]
[ mergestep ] while
append append ;

: mergesort ( seq -- sorted )
dup length 1 >
[ dup length 2 / floor [ head ] [ tail ] 2bi [ mergesort ] bi@ merge ]
[ ] if ;
```



```factor
( scratchpad ) { 4 2 6 5 7 1 3 } mergesort .
{ 1 2 3 4 5 6 7 }
```



## Forth

This is an in-place mergesort which works on arrays of integers.

```forth
: merge-step ( right mid left -- right mid+ left+ )
  over @ over @ < if
    over @ >r
    2dup - over dup cell+ rot move
    r> over !
    >r cell+ 2dup = if rdrop dup else r> then
  then cell+ ;
: merge ( right mid left -- right left )
  dup >r begin 2dup > while merge-step repeat 2drop r> ;

: mid ( l r -- mid ) over - 2/ cell negate and + ;

: mergesort ( right left -- right left )
  2dup cell+ <= if exit then
  swap 2dup mid recurse rot recurse merge ;

: sort ( addr len -- )  cells over + swap mergesort 2drop ;

create test 8 , 1 , 5 , 3 , 9 , 0 , 2 , 7 , 6 , 4 ,

: .array ( addr len -- ) 0 do dup i cells + @ . loop drop ;

test 10 2dup sort .array       \ 0 1 2 3 4 5 6 7 8 9
```



## Fortran

```fortran
      program TestMergeSort
        implicit none
        integer, parameter :: N = 8
        integer :: A(N) = (/ 1, 5, 2, 7, 3, 9, 4, 6 /)
        integer :: work((size(A) + 1) / 2)
        write(*,'(A,/,10I3)')'Unsorted array :',A
        call MergeSort(A, work)
        write(*,'(A,/,10I3)')'Sorted array :',A
      contains

      subroutine merge(A, B, C)
        implicit none
! The targe attribute is necessary, because A .or. B might overlap with C.
        integer, target, intent(in) :: A(:), B(:)
        integer, target, intent(inout) :: C(:)
        integer :: i, j, k

        if (size(A) + size(B) > size(C)) stop(1)

        i = 1; j = 1
        do k = 1, size(C)
          if (i <= size(A) .and. j <= size(B)) then
            if (A(i) <= B(j)) then
              C(k) = A(i)
              i = i + 1
            else
              C(k) = B(j)
              j = j + 1
            end if
          else if (i <= size(A)) then
            C(k) = A(i)
            i = i + 1
          else if (j <= size(B)) then
            C(k) = B(j)
            j = j + 1
          end if
        end do
      end subroutine merge

      subroutine swap(x, y)
        implicit none
        integer, intent(inout) :: x, y
        integer :: tmp
        tmp = x; x = y; y = tmp
      end subroutine

      recursive subroutine MergeSort(A, work)
        implicit none
        integer, intent(inout) :: A(:)
        integer, intent(inout) :: work(:)
        integer :: half
        half = (size(A) + 1) / 2
        if (size(A) < 2) then
          continue
        else if (size(A) == 2) then
          if (A(1) > A(2)) then
            call swap(A(1), A(2))
          end if
        else
          call MergeSort(A( : half), work)
          call MergeSort(A(half + 1 :), work)
          if (A(half) > A(half + 1)) then
            work(1 : half) = A(1 : half)
            call merge(work(1 : half), A(half + 1:), A)
          endif
        end if
      end subroutine MergeSort
      end program TestMergeSort

```



## FreeBASIC

Uses 'top down' C-like algorithm in Wikipedia article:

```freebasic
' FB 1.05.0 Win64

Sub copyArray(a() As Integer, iBegin As Integer, iEnd As Integer, b() As Integer)
  Redim b(iBegin To iEnd - 1) As Integer
  For k As Integer = iBegin To iEnd - 1
    b(k) = a(k)
  Next
End Sub

' Left source half is  a(iBegin  To iMiddle-1).
' Right source half is a(iMiddle To iEnd-1).
' Result is            b(iBegin  To iEnd-1).
Sub topDownMerge(a() As Integer, iBegin As Integer, iMiddle As Integer, iEnd As Integer, b() As Integer)
  Dim i As Integer = iBegin
  Dim j As Integer = iMiddle

  ' While there are elements in the left or right runs...
  For k As Integer = iBegin To iEnd - 1
  ' If left run head exists and is <= existing right run head.
    If i < iMiddle AndAlso (j >= iEnd OrElse a(i) <= a(j)) Then
      b(k) = a(i)
      i += 1
    Else
      b(k) = a(j)
      j += 1
    End If
  Next
End Sub

' Sort the given run of array a() using array b() as a source.
' iBegin is inclusive; iEnd is exclusive (a(iEnd) is not in the set).
Sub topDownSplitMerge(b() As Integer, iBegin As Integer, iEnd As Integer, a() As Integer)
  If (iEnd - iBegin) < 2 Then Return  '' If run size = 1, consider it sorted
  ' split the run longer than 1 item into halves
  Dim iMiddle As Integer = (iEnd + iBegin) \ 2  '' iMiddle = mid point
  ' recursively sort both runs from array a() into b()
  topDownSplitMerge(a(), iBegin,  iMiddle, b())  '' sort the left  run
  topDownSplitMerge(a(), iMiddle, iEnd, b())     '' sort the right run
  ' merge the resulting runs from array b() into a()
  topDownMerge(b(), iBegin, iMiddle, iEnd, a())
End Sub

' Array a() has the items to sort; array b() is a work array (empty initially).
Sub topDownMergeSort(a() As Integer, b() As Integer, n As Integer)
  copyArray(a(), 0, n, b())  '' duplicate array a() into b()
  topDownSplitMerge(b(), 0, n, a())  '' sort data from b() into a()
End Sub

Sub printArray(a() As Integer)
  For i As Integer = LBound(a) To UBound(a)
    Print Using "####"; a(i);
  Next
  Print
End Sub

Dim a(0 To 9) As Integer = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1}

Dim b() As Integer
Print "Unsorted : ";
printArray(a())
topDownMergeSort a(), b(), 10
Print "Sorted   : ";
printArray(a())
Print
Dim a2(0 To 8) As Integer = {7, 5, 2, 6, 1, 4, 2, 6, 3}
Erase b
Print "Unsorted : ";
printArray(a2())
topDownMergeSort a2(), b(), 9
Print "Sorted   : ";
printArray(a2())
Print
Print "Press any key to quit"
Sleep
```


```txt

Unsorted :    4  65   2 -31   0  99   2  83 782   1
Sorted   :  -31   0   1   2   2   4  65  83  99 782

Unsorted :    7   5   2   6   1   4   2   6   3
Sorted   :    1   2   2   3   4   5   6   6   7

```



## FunL


```funl
def
  sort( [] )          =  []
  sort( [x] )         =  [x]
  sort( xs )          =
    val (l, r) = xs.splitAt( xs.length()\2 )
    merge( sort(l), sort(r) )

  merge( [], xs )     =  xs
  merge( xs, [] )     =  xs
  merge( x:xs, y:ys )
    | x <= y          =  x : merge( xs, y:ys )
    | otherwise       =  y : merge( x:xs, ys )

println( sort([94, 37, 16, 56, 72, 48, 17, 27, 58, 67]) )
println( sort(['Sofía', 'Alysha', 'Sophia', 'Maya', 'Emma', 'Olivia', 'Emily']) )
```


```txt

[16, 17, 27, 37, 48, 56, 58, 67, 72, 94]
[Alysha, Emily, Emma, Maya, Olivia, Sofía, Sophia]

```



## Go


```go
package main

import "fmt"

var a = []int{170, 45, 75, -90, -802, 24, 2, 66}
var s = make([]int, len(a)/2+1) // scratch space for merge step

func main() {
    fmt.Println("before:", a)
    mergeSort(a)
    fmt.Println("after: ", a)
}

func mergeSort(a []int) {
    if len(a) < 2 {
        return
    }
    mid := len(a) / 2
    mergeSort(a[:mid])
    mergeSort(a[mid:])
    if a[mid-1] <= a[mid] {
        return
    }
    // merge step, with the copy-half optimization
    copy(s, a[:mid])
    l, r := 0, mid
    for i := 0; ; i++ {
        if s[l] <= a[r] {
            a[i] = s[l]
            l++
            if l == mid {
                break
            }
        } else {
            a[i] = a[r]
            r++
            if r == len(a) {
                copy(a[i+1:], s[l:mid])
                break
            }
        }
    }
    return
}
```



## Groovy

This is the standard algorithm, except that in the looping phase of the merge we work backwards through the left and right lists to construct the merged list, to take advantage of the [[Groovy]] ''List.pop()'' method. However, this results in a partially merged list in reverse sort order; so we then reverse it to put in back into correct order. This could play havoc with the sort stability, but we compensate by picking aggressively from the right list (ties go to the right), rather than aggressively from the left as is done in the standard algorithm.

```groovy
def merge = { List left, List right ->
    List mergeList = []
    while (left && right) {
        print "."
        mergeList << ((left[-1] > right[-1]) ? left.pop() : right.pop())
    }
    mergeList = mergeList.reverse()
    mergeList = left + right + mergeList
}

def mergeSort;
mergeSort = { List list ->

    def n = list.size()
    if (n < 2) return list

    def middle = n.intdiv(2)
    def left = [] + list[0..<middle]
    def right = [] + list[middle..<n]
    left = mergeSort(left)
    right = mergeSort(right)

    if (left[-1] <= right[0]) return left + right

    merge(left, right)
}
```

Test:

```groovy
println (mergeSort([23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4]))
println (mergeSort([88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1]))
println ()
println (mergeSort([10, 10.0, 10.00, 1]))
println (mergeSort([10, 10.00, 10.0, 1]))
println (mergeSort([10.0, 10, 10.00, 1]))
println (mergeSort([10.0, 10.00, 10, 1]))
println (mergeSort([10.00, 10, 10.0, 1]))
println (mergeSort([10.00, 10.0, 10, 1]))
```

The presence of decimal and integer versions of the same numbers, demonstrates, but of course does not '''prove''', that the sort remains stable.
```txt
.............................................................[4, 12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]
....................................................................[0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]

....[1, 10, 10.0, 10.00]
....[1, 10, 10.00, 10.0]
....[1, 10.0, 10, 10.00]
....[1, 10.0, 10.00, 10]
....[1, 10.00, 10, 10.0]
....[1, 10.00, 10.0, 10]
```



### Tail recursion version

It is possible to write a version based on tail recursion, similar to that written in Haskel, OCaml or F#.
This version also takes into account stack overflow problems induced by recursion for large lists using closure trampolines:

```groovy
split = { list ->
    list.collate((list.size()+1)/2 as int)
}

merge = { left, right, headBuffer=[] ->
    if(left.size() == 0) headBuffer+right
    else if(right.size() == 0) headBuffer+left
    else if(left.head() <= right.head()) merge.trampoline(left.tail(), right, headBuffer+left.head())
    else merge.trampoline(right.tail(), left, headBuffer+right.head())
}.trampoline()

mergesort = { List list ->
    if(list.size() < 2) list
    else merge(split(list).collect {mergesort it})
}

assert mergesort((500..1)) == (1..500)
assert mergesort([5,4,6,3,1,2]) == [1,2,3,4,5,6]
assert mergesort([3,3,1,4,6,78,9,1,3,5]) == [1,1,3,3,3,4,5,6,9,78]

```


which uses <code>List.collate()</code>, alternatively one could write a purely recursive <code>split()</code> closure as:

```groovy

split = { list, left=[], right=[] ->
    if(list.size() <2) [list+left, right]
    else split.trampoline(list.tail().tail(), [list.head()]+left,[list.tail().head()]+right)
}.trampoline()

```



## Haskell

Splitting in half in the middle like the normal merge sort does would be inefficient on the singly-linked lists used in Haskell (since you would have to do one pass just to determine the length, and then another half-pass to do the splitting). Instead, the algorithm here splits the list in half in a different way -- by alternately assigning elements to one list and the other. That way we (lazily) construct both sublists in parallel as we traverse the original list. Unfortunately, under this way of splitting we cannot do a stable sort.

```haskell
merge []         ys                   = ys
merge xs         []                   = xs
merge xs@(x:xt) ys@(y:yt) | x <= y    = x : merge xt ys
                          | otherwise = y : merge xs yt

split (x:y:zs) = let (xs,ys) = split zs in (x:xs,y:ys)
split [x]      = ([x],[])
split []       = ([],[])

mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = let (as,bs) = split xs
                in merge (mergeSort as) (mergeSort bs)
```

Alternatively, we can use bottom-up mergesort. This starts with lots of tiny sorted lists, and repeatedly merges pairs of them, building a larger and larger sorted list

```haskell
mergePairs (sorted1 : sorted2 : sorteds) = merge sorted1 sorted2 : mergePairs sorteds
mergePairs sorteds = sorteds

mergeSortBottomUp list = mergeAll (map (\x -> [x]) list)

mergeAll [sorted] = sorted
mergeAll sorteds = mergeAll (mergePairs sorteds)
```

The standard library's sort function in GHC takes a similar approach to the bottom-up code, the differece being that, instead of starting with lists of size one, which are sorted by default, it detects runs in original list and uses those:

```haskell
sort = sortBy compare
sortBy cmp = mergeAll . sequences
  where
    sequences (a:b:xs)
      | a `cmp` b == GT = descending b [a]  xs
      | otherwise       = ascending  b (a:) xs
    sequences xs = [xs]

    descending a as (b:bs)
      | a `cmp` b == GT = descending b (a:as) bs
    descending a as bs  = (a:as): sequences bs

    ascending a as (b:bs)
      | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs   = as [a]: sequences bs
```

In this code, mergeAll, mergePairs, and merge are as above, except using the specialized cmp function in merge.


## Io


```io
List do (
    merge := method(lst1, lst2,
        result := list()
        while(lst1 isNotEmpty or lst2 isNotEmpty,
            if(lst1 first <= lst2 first) then(
                result append(lst1 removeFirst)
            ) else (
                result append(lst2 removeFirst)
            )
        )
    result)

    mergeSort := method(
        if (size > 1) then(
            half_size := (size / 2) ceil
            return merge(slice(0, half_size) mergeSort,
                         slice(half_size, size) mergeSort)
        ) else (return self)
    )

    mergeSortInPlace := method(
        copy(mergeSort)
    )
)

lst := list(9, 5, 3, -1, 15, -2)
lst mergeSort println # ==> list(-2, -1, 3, 5, 9, 15)
lst mergeSortInPlace println # ==> list(-2, -1, 3, 5, 9, 15)
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()                                                         #: demonstrate various ways to sort a list and string
   demosort(mergesort,[3, 14, 1, 5, 9, 2, 6, 3],"qwerty")
end

procedure mergesort(X,op,lower,upper)                                    #: return sorted list ascending(or descending)
local middle

   if /lower := 1 then {                                                 # top level call setup
      upper := *X
      op := sortop(op,X)                                                 # select how and what we sort
      }

   if upper ~= lower then {                                              # sort all sections with 2 or more elements
      X := mergesort(X,op,lower,middle := lower + (upper - lower) / 2)
      X := mergesort(X,op,middle+1,upper)

      if op(X[middle+1],X[middle]) then                                  # @middle+1 < @middle merge if halves reversed
         X := merge(X,op,lower,middle,upper)
   }
   return X
end

procedure merge(X,op,lower,middle,upper)                                 # merge two list sections within a larger list
local p1,p2,add

   p1 := lower
   p2 := middle + 1
   add := if type(X) ~== "string" then put else "||"                     # extend X, strings require X := add (until ||:= is invocable)

   while p1 <= middle & p2 <= upper do
      if op(X[p1],X[p2]) then {                                          # @p1 < @p2
         X := add(X,X[p1])                                               # extend X temporarily (rather than use a separate temporary list)
         p1 +:= 1
         }
      else {
         X := add(X,X[p2])                                               # extend X temporarily
         p2 +:= 1
         }

   while X := add(X,X[middle >= p1]) do p1 +:= 1                         # and rest of lower or ...
   while X := add(X,X[upper  >= p2]) do p2 +:= 1                         # ... upper trailers if any

   if type(X) ~== "string" then                                          # pull section's sorted elements from extension
      every X[upper to lower by -1] := pull(X)
   else
      (X[lower+:(upper-lower+1)] := X[0-:(upper-lower+1)])[0-:(upper-lower+1)] := ""

   return X
end
```


Note: This example relies on [[Sorting_algorithms/Bubble_sort#Icon| the supporting procedures 'sortop', and 'demosort' in Bubble Sort]].
The full demosort exercises the named sort of a list with op = "numeric", "string", ">>" (lexically gt, descending),">" (numerically gt, descending), a custom comparator, and also a string.

{{out}} Abbreviated sample

```txt
Sorting Demo using procedure mergesort
  on list : [ 3 14 1 5 9 2 6 3 ]
    with op = &null:         [ 1 2 3 3 5 6 9 14 ]   (0 ms)
  ...
  on string : "qwerty"
    with op = &null:         "eqrtwy"   (0 ms)
```



## J

'''Solution'''

```j
merge     =: ,`(({.@] , ($: }.))~` ({.@] , ($: }.)) @.(>&{.))@.(*@*&#)
split     =: </.~ 0 1$~#
mergeSort =: merge & $: &>/ @ split ` ] @. (1>:#)
```

This version is usable for relative small arrays due to stack limitations for the recursive verb 'merge'.
For larger arrays replace 'merge' with the following explicit non-recursive version:

```j
merge=: 4 : 0
 if. 0= x *@*&# y do. x,y return. end.
 la=.x
 ra=.y
 z=.i.0
 while. la *@*&# ra do.
  if. la  >&{. ra do.
    z=.z,{.ra
    ra=.}.ra
  else.
    z=.z,{.la
    la=.}.la
  end.
 end.
 z,la,ra
)
```

But don't forget to use J's primitives /: or \: if you really need a sort-function.


## Java

```java5
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

public class Merge{
    public static <E extends Comparable<? super E>> List<E> mergeSort(List<E> m){
        if(m.size() <= 1) return m;

        int middle = m.size() / 2;
        List<E> left = m.subList(0, middle);
        List<E> right = m.subList(middle, m.size());

        right = mergeSort(right);
        left = mergeSort(left);
        List<E> result = merge(left, right);

        return result;
    }

    public static <E extends Comparable<? super E>> List<E> merge(List<E> left, List<E> right){
        List<E> result = new ArrayList<E>();
        Iterator<E> it1 = left.iterator();
        Iterator<E> it2 = right.iterator();

	E x = it1.next();
	E y = it2.next();
        while (true){
            //change the direction of this comparison to change the direction of the sort
            if(x.compareTo(y) <= 0){
		result.add(x);
		if(it1.hasNext()){
		    x = it1.next();
		}else{
		    result.add(y);
		    while(it2.hasNext()){
			result.add(it2.next());
		    }
		    break;
		}
	    }else{
		result.add(y);
		if(it2.hasNext()){
		    y = it2.next();
		}else{
		    result.add(x);
		    while (it1.hasNext()){
			result.add(it1.next());
		    }
		    break;
		}
	    }
        }
        return result;
    }
}
```



## JavaScript


```javascript
function merge(left, right, arr) {
  var a = 0;

  while (left.length && right.length) {
    arr[a++] = (right[0] < left[0]) ? right.shift() : left.shift();
  }
  while (left.length) {
    arr[a++] = left.shift();
  }
  while (right.length) {
    arr[a++] = right.shift();
  }
}

function mergeSort(arr) {
  var len = arr.length;

  if (len === 1) { return; }

  var mid = Math.floor(len / 2),
      left = arr.slice(0, mid),
      right = arr.slice(mid);

  mergeSort(left);
  mergeSort(right);
  merge(left, right, arr);
}

var arr = [1, 5, 2, 7, 3, 9, 4, 6, 8];
mergeSort(arr); // arr will now: 1, 2, 3, 4, 5, 6, 7, 8, 9
```



## jq

The sort function defined here will sort any JSON array.

```jq
# Input: [x,y] -- the two arrays to be merged
# If x and y are sorted as by "sort", then the result will also be sorted:
def merge:
  def m:  # state: [x, y, array]  (array being the answer)
    .[0] as $x
    | .[1] as $y
    | if   0 == ($x|length) then .[2] + $y
      elif 0 == ($y|length) then .[2] + $x
      else
        (if $x[0] <= $y[0] then [$x[1:], $y,     .[2] + [$x[0] ]]
         else                   [$x,     $y[1:], .[2] + [$y[0] ]]
         end) | m
      end;
   [.[0], .[1], []] | m;

def merge_sort:
  if length <= 1 then .
  else
    (length/2 |floor) as $len
    | . as $in
    | [ ($in[0:$len] | merge_sort), ($in[$len:] | merge_sort) ] | merge
  end;
```

'''Example''':

```jq

( [1, 3, 8, 9, 0, 0, 8, 7, 1, 6],
  [170, 45, 75, 90, 2, 24, 802, 66],
  [170, 45, 75, 90, 2, 24, -802, -66] )
| (merge_sort == sort)
```

 true
 true
 true


## Julia

```julia
function mergesort(arr::Vector)
    if length(arr) ≤ 1 return arr end
    mid = length(arr) ÷ 2
    lpart = mergesort(arr[1:mid])
    rpart = mergesort(arr[mid+1:end])
    rst = similar(arr)
    i = ri = li = 1
    @inbounds while li ≤ length(lpart) && ri ≤ length(rpart)
        if lpart[li] ≤ rpart[ri]
            rst[i] = lpart[li]
            li += 1
        else
            rst[i] = rpart[ri]
            ri += 1
        end
        i += 1
    end
    if li ≤ length(lpart)
        copy!(rst, i, lpart, li)
    else
        copy!(rst, i, rpart, ri)
    end
    return rst
end

v = rand(-10:10, 10)
println("# unordered: $v\n -> ordered: ", mergesort(v))
```


```txt
# unordered: [8, 6, 7, 1, -1, 0, -4, 7, -7, 0]
 -> ordered: [-7, -4, -1, 0, 0, 1, 6, 7, 7, 8]
```



## Kotlin


```kotlin
fun mergeSort(list: List<Int>): List<Int> {
    if (list.size <= 1) {
        return list
    }

    val left = mutableListOf<Int>()
    val right = mutableListOf<Int>()

    val middle = list.size / 2
    list.forEachIndexed { index, number ->
        if (index < middle) {
            left.add(number)
        } else {
            right.add(number)
        }
    }

    fun merge(left: List<Int>, right: List<Int>): List<Int> = mutableListOf<Int>().apply {
        var indexLeft = 0
        var indexRight = 0

        while (indexLeft < left.size && indexRight < right.size) {
            if (left[indexLeft] <= right[indexRight]) {
                add(left[indexLeft])
                indexLeft++
            } else {
                add(right[indexRight])
                indexRight++
            }
        }

        while (indexLeft < left.size) {
            add(left[indexLeft])
            indexLeft++
        }

        while (indexRight < right.size) {
            add(right[indexRight])
            indexRight++
        }
    }

    return merge(mergeSort(left), mergeSort(right))
}

fun main(args: Array<String>) {
    val numbers = listOf(5, 2, 3, 17, 12, 1, 8, 3, 4, 9, 7)
    println("Unsorted: $numbers")
    println("Sorted: ${mergeSort(numbers)}")
}
```


```txt
Unsorted: [5, 2, 3, 17, 12, 1, 8, 3, 4, 9, 7]
Sorted:   [1, 2, 3, 3, 4, 5, 7, 8, 9, 12, 17]
```



## Liberty BASIC


```lb
    itemCount = 20
    dim A(itemCount)
    dim tmp(itemCount)    'merge sort needs additionally same amount of storage

    for i = 1 to itemCount
        A(i) = int(rnd(1) * 100)
    next i

    print "Before Sort"
    call printArray itemCount

    call mergeSort 1,itemCount

    print "After Sort"
    call printArray itemCount
end

'------------------------------------------
sub mergeSort start, theEnd
    if theEnd-start < 1 then exit sub
    if theEnd-start = 1 then
        if A(start)>A(theEnd) then
            tmp=A(start)
            A(start)=A(theEnd)
            A(theEnd)=tmp
        end if
        exit sub
    end if
    middle = int((start+theEnd)/2)
    call mergeSort start, middle
    call mergeSort middle+1, theEnd
    call merge start, middle, theEnd
end sub

sub merge start, middle, theEnd
    i = start: j = middle+1: k = start
    while i<=middle OR j<=theEnd
        select case
        case i<=middle AND j<=theEnd
            if A(i)<=A(j) then
                tmp(k)=A(i)
                i=i+1
            else
                tmp(k)=A(j)
                j=j+1
            end if
            k=k+1
        case i<=middle
            tmp(k)=A(i)
            i=i+1
            k=k+1
        case else    'j<=theEnd
            tmp(k)=A(j)
            j=j+1
            k=k+1
        end select
    wend

    for i = start to theEnd
        A(i)=tmp(i)
    next
end sub

'
### =====================================

sub printArray itemCount
    for i = 1 to itemCount
        print using("###", A(i));
    next i
    print
end sub
```



## Logo

```logo
to split :size :front :list
  if :size < 1 [output list :front :list]
  output split :size-1 (lput first :list :front) (butfirst :list)
end

to merge :small :large
  if empty? :small [output :large]
  ifelse lessequal? first :small first :large ~
    [output fput first :small merge butfirst :small :large] ~
    [output fput first :large merge butfirst :large :small]
end

to mergesort :list
  localmake "half split (count :list) / 2 [] :list
  if empty? first :half [output :list]
  output merge mergesort first :half mergesort last :half
end
```



## Logtalk


```logtalk
msort([], []) :- !.
msort([X], [X]) :- !.
msort([X, Y| Xs], Ys) :-
    split([X, Y| Xs], X1s, X2s),
    msort(X1s, Y1s),
    msort(X2s, Y2s),
    merge(Y1s, Y2s, Ys).

split([], [], []).
split([X| Xs], [X| Ys], Zs) :-
    split(Xs, Zs, Ys).

merge([X| Xs], [Y| Ys], [X| Zs]) :-
    X @=< Y, !,
    merge(Xs, [Y| Ys], Zs).
merge([X| Xs], [Y| Ys], [Y| Zs]) :-
    X @> Y, !,
    merge([X | Xs], Ys, Zs).
merge([], Xs, Xs) :- !.
merge(Xs, [], Xs).
```



## Lua


```Lua
function getLower(a,b)
  local i,j=1,1
  return function()
    if not b[j] or a[i] and a[i]<b[j] then
      i=i+1; return a[i-1]
    else
      j=j+1; return b[j-1]
    end
  end
end

function merge(a,b)
  local res={}
  for v in getLower(a,b) do res[#res+1]=v end
  return res
end

function mergesort(list)
  if #list<=1 then return list end
  local s=math.floor(#list/2)
  return merge(mergesort{unpack(list,1,s)}, mergesort{unpack(list,s+1)})
end
```



## Lucid

[http://i.csc.uvic.ca/home/hei/lup/06.html]

```lucid
msort(a) = if iseod(first next a) then a else merge(msort(b0),msort(b1)) fi
  where
   p = false fby not p;
   b0 = a whenever p;
   b1 = a whenever not p;
   just(a) = ja
      where
         ja = a fby if iseod ja then eod else next a fi;
      end;
   merge(x,y) = if takexx then xx else yy fi
     where
      xx = (x) upon takexx;
      yy = (y) upon not takexx;
      takexx = if iseod(yy) then true elseif
                  iseod(xx) then false else xx <= yy fi;
     end;
  end;
```


## M2000 Interpreter


```M2000 Interpreter

module checkit {
	\\ merge sort
	group merge {
		function sort(right as stack) {
			if len(right)<=1 then =right : exit
			left=.sort(stack up right, len(right) div 2 )
			right=.sort(right)
			\\ stackitem(right) is same as stackitem(right,1)
			if stackitem(left, len(left))<=stackitem(right) then
				\\ !left take items from left for merging
				\\ so after this left and right became empty stacks
				=stack:=!left, !right
				exit
			end if
			=.merge(left, right)
		}
		function sortdown(right as stack) {
			if len(right)<=1 then =right : exit
			left=.sortdown(stack up right, len(right) div 2 )
			right=.sortdown(right)
			if stackitem(left, len(left))>stackitem(right) then
				=stack:=!left, !right : exit
			end if
			=.mergedown(left, right)
		}
		\\ left and right are pointers to stack objects
		\\ here we pass by value the pointer not the data
		function merge(left as stack, right as stack) {
			result=stack
			while len(left) > 0 and len(right) > 0
				if stackitem(left,1) <= stackitem(right) then
					result=stack:=!result, !(stack up left, 1)
				else
					result=stack:=!result, !(stack up right, 1)
				end if
			end while
			if len(right) > 0 then  result=stack:= !result,!right
			if len(left) > 0 then result=stack:= !result,!left
			=result
		}
		function mergedown(left as stack, right as stack) {
			result=stack
			while len(left) > 0 and len(right) > 0
				if stackitem(left,1) > stackitem(right) then
					result=stack:=!result, !(stack up left, 1)
				else
					result=stack:=!result, !(stack up right, 1)
				end if
			end while
			if len(right) > 0 then  result=stack:= !result,!right
			if len(left) > 0 then result=stack:= !result,!left
			=result
		}
	}
	k=stack:=7, 5, 2, 6, 1, 4, 2, 6, 3
	print merge.sort(k)
	print len(k)=0   ' we have to use merge.sort(stack(k)) to pass a copy of k

	\\ input array  (arr is a pointer to array)
	arr=(10,8,9,7,5,6,2,3,0,1)
	\\ stack(array pointer) return a stack with a copy of array items
	\\ array(stack pointer) return an array, empty the stack

	arr2=array(merge.sort(stack(arr)))
	Print type$(arr2)
	Dim a()
	\\ a() is an array as a value, so we just copy arr2 to a()
	a()=arr2
	\\ to prove we add 1 to each element of arr2
	arr2++
	Print a()  ' 0,1,2,3,4,5,6,7,8,9
	Print arr2  ' 1,2,3,4,5,6,7,8,9,11
	p=a()  ' we get a pointer
	\\ a() has a double pointer inside
	\\ so a() get just the inner pointer
	a()=array(merge.sortdown(stack(p)))
	\\ so now p (which use the outer pointer)
	\\ still points to a()
	print p   ' p point to a()

}
checkit

```



## Maple

<lang>merge := proc(arr, left, mid, right)
	local i, j, k, n1, n2, L, R;
	n1 := mid-left+1:
	n2 := right-mid:
	L := Array(1..n1):
	R := Array(1..n2):
	for i from 0 to n1-1 do
		L(i+1) :=arr(left+i):
	end do:
	for j from 0 to n2-1 do
		R(j+1) := arr(mid+j+1):
	end do:
	i := 1:
	j := 1:
	k := left:
	while(i <= n1 and j <= n2) do
		if (L[i] <= R[j]) then
			arr[k] := L[i]:
			i++:
		else
			arr[k] := R[j]:
			j++:
		end if:
		k++:
	end do:
	while(i <= n1) do
		arr[k] := L[i]:
		i++:
		k++:
	end do:
	while(j <= n2) do
		arr[k] := R[j]:
		j++:
		k++:
	end do:
end proc:
arr := Array([17,3,72,0,36,2,3,8,40,0]);
mergeSort(arr,1,numelems(arr)):
arr;
```

```txt
[0,0,2,3,3,8,17,36,40,72]
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
```Mathematica
MergeSort[m_List] := Module[{middle},
  If[Length[m] >= 2,
   middle = Ceiling[Length[m]/2];
   Apply[Merge,
    Map[MergeSort, Partition[m, middle, middle, {1, 1}, {}]]],
   m
   ]
  ]

Merge[left_List, right_List] := Module[
  {leftIndex = 1, rightIndex = 1},
  Table[
   Which[
    leftIndex > Length[left], right[[rightIndex++]],
    rightIndex > Length[right], left[[leftIndex++]],
    left[[leftIndex]] <= right[[rightIndex]], left[[leftIndex++]],
    True, right[[rightIndex++]]],
   {Length[left] + Length[right]}]
  ]
```



## MATLAB


```MATLAB
function list = mergeSort(list)

    if numel(list) <= 1
        return
    else
        middle = ceil(numel(list) / 2);
        left = list(1:middle);
        right = list(middle+1:end);

        left = mergeSort(left);
        right = mergeSort(right);

        if left(end) <= right(1)
            list = [left right];
            return
        end

        %merge(left,right)
        counter = 1;
        while (numel(left) > 0) && (numel(right) > 0)
            if(left(1) <= right(1))
                list(counter) = left(1);
                left(1) = [];
            else
                list(counter) = right(1);
                right(1) = [];
            end
            counter = counter + 1;
        end

        if numel(left) > 0
            list(counter:end) = left;
        elseif numel(right) > 0
            list(counter:end) = right;
        end
        %end merge
    end %if
end %mergeSort
```

Sample Usage:

```MATLAB>>
 mergeSort([4 3 1 5 6 2])

ans =

     1     2     3     4     5     6
```



## Maxima


```maxima
merge(a, b) := block(
   [c: [ ], i: 1, j: 1, p: length(a), q: length(b)],
   while i <= p and j <= q do (
      if a[i] < b[j] then (
         c: endcons(a[i], c),
         i: i + 1
      ) else (
         c: endcons(b[j], c),
         j: j + 1
      )
   ),
   if i > p then append(c, rest(b, j - 1)) else append(c, rest(a, i - 1))
)$

mergesort(u) := block(
   [n: length(u), k, a, b],
   if n <= 1 then u else (
      a: rest(u, k: quotient(n, 2)),
      b: rest(u, k - n),
      merge(mergesort(a), mergesort(b))
   )
)$
```


## MAXScript


```MAXScript
fn mergesort arr =
(
	local left = #()
	local right = #()
	local result = #()
	if arr.count < 2 then return arr
	else
	(
		local mid = arr.count/2
		for i = 1 to mid do
		(
			append left arr[i]
		)
		for i = (mid+1) to arr.count do
		(
			append right arr[i]
		)
		left = mergesort left
		right = mergesort right
		if left[left.count] <= right[1] do
		(
			join left right
			return left
		)
		result = _merge left right
		return result
	)
)

fn _merge a b =
(
	local result = #()
	while a.count > 0 and b.count > 0 do
	(
		if a[1] <= b[1] then
		(
			append result a[1]
			a = for i in 2 to a.count collect a[i]
		)
		else
		(
			append result b[1]
			b = for i in 2 to b.count collect b[i]
		)
	)
	if a.count > 0 do
	(
		join result a
	)
	if b.count > 0 do
	(
		join result b
	)
	return result
)
```

Output:

```MAXScript

a = for i in 1 to 15 collect random -5 20
#(-3, 13, 2, -2, 13, 9, 17, 7, 16, 19, 0, 0, 20, 18, 1)
mergeSort a
#(-3, -2, 0, 0, 1, 2, 7, 9, 13, 13, 16, 17, 18, 19, 20)

```



## Mercury

This version of a sort will sort a list of any type for which there is an ordering predicate defined.  Both a function form and a predicate form are defined here with the function implemented in terms of the predicate.  Some of the ceremony has been elided.

```mercury

:- module merge_sort.

:- interface.

:- import_module list.

:- type split_error ---> split_error.

:- func merge_sort(list(T)) = list(T).
:- pred merge_sort(list(T)::in, list(T)::out) is det.

:- implementation.

:- import_module int, exception.

merge_sort(U) = S :- merge_sort(U, S).

merge_sort(U, S) :- merge_sort(list.length(U), U, S).

:- pred merge_sort(int::in, list(T)::in, list(T)::out) is det.
merge_sort(L, U, S) :-
    ( L > 1 ->
        H = L // 2,
        ( split(H, U, F, B) ->
            merge_sort(H, F, SF),
            merge_sort(L - H, B, SB),
            merge_sort.merge(SF, SB, S)
        ; throw(split_error) )
    ; S = U ).

:- pred split(int::in, list(T)::in, list(T)::out, list(T)::out) is semidet.
split(N, L, S, E) :-
    ( N = 0 -> S = [], E = L
    ; N > 0, L = [H | L1], S = [H | S1],
      split(N - 1, L1, S1, E) ).

:- pred merge(list(T)::in, list(T)::in, list(T)::out) is det.
merge([], [], []).
merge([X|Xs], [], [X|Xs]).
merge([], [Y|Ys], [Y|Ys]).
merge([X|Xs], [Y|Ys], M) :-
    ( compare(>, X, Y) ->
        merge_sort.merge([X|Xs], Ys, M0),
        M = [Y|M0]
    ; merge_sort.merge(Xs, [Y|Ys], M0),
        M = [X|M0] ).

```



## Nim


```nim
proc merge[T](a, b: var openarray[T], left, middle, right) =
  let
    leftLen = middle - left
    rightLen = right - middle
  var
    l = 0
    r = leftLen

  for i in left .. <middle:
    b[l] = a[i]
    inc l
  for i in middle .. < right:
    b[r] = a[i]
    inc r

  l = 0
  r = leftLen
  var i = left

  while l < leftLen and r < leftLen + rightLen:
    if b[l] < b[r]:
      a[i] = b[l]
      inc l
    else:
      a[i] = b[r]
      inc r
    inc i

  while l < leftLen:
    a[i] = b[l]
    inc l
    inc i
  while r < leftLen + rightLen:
    a[i] = b[r]
    inc r
    inc i

proc mergeSort[T](a, b: var openarray[T], left, right) =
  if right - left <= 1: return

  let middle = (left + right) div 2
  mergeSort(a, b, left, middle)
  mergeSort(a, b, middle, right)
  merge(a, b, left, middle, right)

proc mergeSort[T](a: var openarray[T]) =
  var b = newSeq[T](a.len)
  mergeSort(a, b, 0, a.len)

var a = @[4, 65, 2, -31, 0, 99, 2, 83, 782]
mergeSort a
echo a
```

```txt
@[-31, 0, 2, 2, 4, 65, 83, 99, 782]
```



## OCaml


```ocaml
let rec split_at n xs =
  match n, xs with
      0, xs ->
        [], xs
    | _, [] ->
        failwith "index too large"
    | n, x::xs when n > 0 ->
        let xs', xs'' = split_at (pred n) xs in
          x::xs', xs''
    | _, _ ->
        invalid_arg "negative argument"

let rec merge_sort cmp = function
    [] -> []
  | [x] -> [x]
  | xs ->
      let xs, ys = split_at (List.length xs / 2) xs in
        List.merge cmp (merge_sort cmp xs) (merge_sort cmp ys)

let _ =
  merge_sort compare [8;6;4;2;1;3;5;7;9]
```



## Oz


```oz
declare
  fun {MergeSort Xs}
     case Xs
     of nil then nil
     [] [X] then [X]
     else
        Middle = {Length Xs} div 2
        Left Right
        {List.takeDrop Xs Middle ?Left ?Right}
     in
        {List.merge {MergeSort Left} {MergeSort Right} Value.'<'}
     end
  end
in
  {Show {MergeSort [3 1 4 1 5 9 2 6 5]}}
```



## Nemerle

This is a translation of a Standard ML example from [[wp:Standard_ML#Mergesort|Wikipedia]].

```Nemerle
using System;
using System.Console;
using Nemerle.Collections;

module Mergesort
{
    MergeSort[TEnu, TItem] (sort_me : TEnu) : list[TItem]
      where TEnu  : Seq[TItem]
      where TItem : IComparable
    {
        def split(xs) {
            def loop (zs, xs, ys) {
                |(x::y::zs, xs, ys) => loop(zs, x::xs, y::ys)
                |(x::[], xs, ys) => (x::xs, ys)
                |([], xs, ys) => (xs, ys)
            }

            loop(xs, [], [])
        }

        def merge(xs, ys) {
            def loop(res, xs, ys) {
                |(res, [], []) => res.Reverse()
                |(res, x::xs, []) => loop(x::res, xs, [])
                |(res, [], y::ys) => loop(y::res, [], ys)
                |(res, x::xs, y::ys) => if (x.CompareTo(y) < 0) loop(x::res, xs, y::ys)
                                        else loop(y::res, x::xs, ys)
            }
            loop ([], xs, ys)
        }

        def ms(xs) {
            |[] => []
            |[x] => [x]
            |_ => { def (left, right) = split(xs); merge(ms(left), ms(right)) }
        }

        ms(sort_me.NToList())
    }

    Main() : void
    {
        def test1 = MergeSort([1, 5, 9, 2, 7, 8, 4, 6, 3]);
        def test2 = MergeSort(array['a', 't', 'w', 'f', 'c', 'y', 'l']);
        WriteLine(test1);
        WriteLine(test2);
    }
}
```

```txt
[1, 2, 3, 4, 5, 6, 7, 8, 9]
[a, c, f, l, t, w, y]
```



## NetRexx


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
  , mergeSort(String[] Arrays.copyOf(placesList, placesList.length)) -
]

loop ln = 0 to lists.length - 1
  cl = lists[ln]
  loop ct = 0 to cl.length - 1
    say cl[ct]
    end ct
    say
  end ln

return

method mergeSort(m = String[]) public constant binary returns String[]

  rl = String[m.length]
  al = List mergeSort(Arrays.asList(m))
  al.toArray(rl)

  return rl

method mergeSort(m = List) public constant binary returns ArrayList

  result = ArrayList(m.size)
  left   = ArrayList()
  right  = ArrayList()
  if m.size > 1 then do
    middle = m.size % 2
    loop x_ = 0 to middle - 1
      left.add(m.get(x_))
      end x_
    loop x_ = middle to m.size - 1
      right.add(m.get(x_))
      end x_
    left  = mergeSort(left)
    right = mergeSort(right)
    if (Comparable left.get(left.size - 1)).compareTo(Comparable right.get(0)) <= 0 then do
      left.addAll(right)
      result.addAll(m)
      end
    else do
      result = merge(left, right)
      end
    end
  else do
    result.addAll(m)
    end

  return result

method merge(left = List, right = List) public constant binary returns ArrayList

  result = ArrayList()
  loop label mx while left.size > 0 & right.size > 0
    if (Comparable left.get(0)).compareTo(Comparable right.get(0)) <= 0 then do
      result.add(left.get(0))
      left.remove(0)
      end
    else do
      result.add(right.get(0))
      right.remove(0)
      end
    end mx
    if left.size > 0 then do
      result.addAll(left)
      end
    if right.size > 0 then do
      result.addAll(right)
      end

  return result

```

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



## PARI/GP

Note also that the built-in <code>vecsort</code> and <code>listsort</code> use a merge sort internally.

```parigp
mergeSort(v)={
  if(#v<2, return(v));
  my(m=#v\2,left=vector(m,i,v[i]),right=vector(#v-m,i,v[m+i]));
  left=mergeSort(left);
  right=mergeSort(right);
  merge(left, right)
};
merge(u,v)={
	my(ret=vector(#u+#v),i=1,j=1);
	for(k=1,#ret,
		if(i<=#u & (j>#v | u[i]<v[j]),
			ret[k]=u[i];
			i++
		,
			ret[k]=v[j];
			j++
		)
	);
	ret
};
```



## Pascal


```pascal
program MergeSortDemo;

type
  TIntArray = array of integer;

function merge(left, right: TIntArray): TIntArray;
  var
    i, j: integer;
  begin
    j := 0;
    setlength(merge, length(left) + length(right));
    while (length(left) > 0) and (length(right) > 0) do
    begin
      if left[0] <= right[0] then
      begin
	merge[j] := left[0];
	inc(j);
	for i := low(left) to high(left) - 1 do
	  left[i] := left[i+1];
	setlength(left, length(left) - 1);
      end
      else
      begin
	merge[j] := right[0];
	inc(j);
	for i := low(right) to high(right) - 1 do
	  right[i] := right[i+1];
	setlength(right, length(right) - 1);
      end;
    end;
    if length(left) > 0 then
      for i := low(left) to high(left) do
	  merge[j + i] := left[i];
    j := j + length(left);
    if length(right) > 0 then
      for i := low(right) to high(right) do
	  merge[j + i] := right[i];
  end;

function mergeSort(m: TIntArray): TIntArray;
  var
    left, right: TIntArray;
    i, middle: integer;
  begin
    setlength(mergeSort, length(m));
    if length(m) = 1 then
      mergeSort[0] := m[0]
    else if length(m) > 1 then
    begin
      middle := length(m) div 2;
      setlength(left, middle);
      setlength(right, length(m)-middle);
      for i := low(left) to high(left) do
        left[i] := m[i];
      for i := low(right) to high(right) do
        right[i] := m[middle+i];
      left  := mergeSort(left);
      right := mergeSort(right);
      mergeSort := merge(left, right);
    end;
  end;

var
  data: TIntArray;
  i: integer;

begin
  setlength(data, 8);
  Randomize;
  writeln('The data before sorting:');
  for i := low(data) to high(data) do
  begin
    data[i] := Random(high(data));
    write(data[i]:4);
  end;
  writeln;
  data := mergeSort(data);
  writeln('The data after sorting:');
  for i := low(data) to high(data) do
  begin
    write(data[i]:4);
  end;
  writeln;
end.
```

```txt
./MergeSort
The data before sorting:
   6   1   2   1   5   2   1   5
The data after sorting:
   1   1   1   2   2   5   5   6

```


### improvement

uses "only" one halfsized temporary array for merging, which are set to the right size in before.
small sized fields are sorted via insertion sort.
Only an array of Pointers is sorted, so no complex data transfers are needed.Sort for X,Y or whatever is easy to implement.

Works with ( Turbo -) Delphi  too.

```pascal
{$IFDEF FPC}
  {$MODE DELPHI}
  {$OPTIMIZATION ON,Regvar,ASMCSE,CSE,PEEPHOLE}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}
uses
  sysutils; //for timing
type
  tDataElem  =  record
                  myText : AnsiString;
                  myX,
                  myY : double;
                  myTag,
                  myOrgIdx : LongInt;
                end;

  tpDataElem = ^tDataElem;
  tData = array of tDataElem;

  tSortData = array of tpDataElem;
  tCompFunc = function(A,B:tpDataElem):integer;
var
  Data    : tData;
  Sortdata,
  tmpData : tSortData;

procedure InitData(var D:tData;cnt: LongWord);
var
  i,k: LongInt;
begin
  Setlength(D,cnt);
  Setlength(SortData,cnt);
  Setlength(tmpData,cnt shr 1 +1 );
  k := 10*cnt;
  For i := cnt-1 downto 0 do
  Begin
    Sortdata[i] := @D[i];
    with D[i] do
    Begin
      myText := Format('_%.9d',[random(cnt)+1]);
      myX := Random*k;
      myY := Random*k;
      myTag := Random(k);
      myOrgIdx := i;
    end;
  end;
end;

procedure FreeData(var D:tData);
begin
  Setlength(tmpData,0);
  Setlength(SortData,0);
  Setlength(D,0);
end;

function CompLowercase(A,B:tpDataElem):integer;
var
  lcA,lcB: String;
Begin
  lcA := lowercase(A^.myText);
  lcB := lowercase(B^.myText);
  result := ORD(lcA > lcB)-ORD(lcA < lcB);
end;

function myCompText(A,B:tpDataElem):integer;
{sort an array (or list) of strings in order of descending length,
  and in ascending lexicographic order for strings of equal length.}
var
  lA,lB:integer;

Begin
  lA := Length(A^.myText);
  lB := Length(B^.myText);
  result := ORD(lA<lB)-ORD(lA>lB);
  IF result = 0 then
    result := CompLowercase(A,B);
end;

function myCompX(A,B:tpDataElem):integer;
//same as sign without jumps in assembler code
begin
  result := ORD(A^.myX > B^.myX)-ORD(A^.myX < B^.myX);
end;

function myCompY(A,B:tpDataElem):integer;
Begin
  result := ORD(A^.myY > B^.myY)-ORD(A^.myY < B^.myY);
end;

function myCompTag(A,B:tpDataElem):integer;
Begin
  result := ORD(A^.myTag > B^.myTag)-ORD(A^.myTag < B^.myTag);
end;

procedure InsertionSort(left,right:integer;var a: tSortData;CompFunc: tCompFunc);
var
   Pivot : tpDataElem;
   i,j  : LongInt;
begin
 for i:=left+1 to right do
 begin
   j :=i;
   Pivot := A[j];
   while (j>left) AND (CompFunc(A[j-1],Pivot)>0) do
   begin
     A[j] := A[j-1];
     dec(j);
   end;
   A[j] :=PiVot;// s.o.
 end;
end;


procedure mergesort(left,right:integer;var a: tSortData;CompFunc: tCompFunc);
var
  i,j,k,mid :integer;
begin
{// without insertion sort
  If right>left then
}
//{ test insertion sort
  If right-left<=14 then
     InsertionSort(left,right,a,CompFunc)
  else
//}
  begin
    //recursion
    mid := (right+left) div 2;
    mergesort(left, mid,a,CompFunc);
    mergesort(mid+1, right,a,CompFunc);
    //already sorted ?
    IF CompFunc(A[Mid],A[Mid+1])<0 then
      exit;

    //##########  Merge  ##########
    //copy lower half to temporary array
    move(A[left],tmpData[0],(mid-left+1)*SizeOf(Pointer));
    i := 0;
    j := mid+1;
    k := left;
    // re-integrate
    while (k<j) AND (j<=right) do
      begin
      IF CompFunc(tmpData[i],A[j])<=0 then
        begin
        A[k] := tmpData[i];
        inc(i);
        end
      else
        begin
        A[k]:= A[j];
        inc(j);
        end;
      inc(k);
      end;
    //the rest of tmpdata a move should do too, in next life
    while (k<j) do
      begin
      A[k] := tmpData[i];
      inc(i);
      inc(k);
      end;
  end;
end;

var
  T1,T0: TDateTime;
  i : integer;
Begin
  randomize;
  InitData(Data,1*1000*1000);

  T0 := Time;
  mergesort(Low(SortData),High(SortData),SortData,@myCompText);
  T1 := Time;
  Writeln('myText ',FormatDateTime('NN:SS.ZZZ',T1-T0));
//  For i := 0 to High(Data) do  Write(SortData[i].myText);  writeln;
  T0 := Time;
  mergesort(Low(SortData),High(SortData),SortData,@myCompX);
  T1 := Time;
  Writeln('myX    ',FormatDateTime('NN:SS.ZZZ',T1-T0));
 //check
  For i := 1 to High(Data) do
    IF myCompX(SortData[i-1],SortData[i]) = 1 then
      Write(i:8);

  T0 := Time;
  mergesort(Low(SortData),High(SortData),SortData,@myCompY);
  T1 := Time;
  Writeln('myY    ',FormatDateTime('NN:SS.ZZZ',T1-T0));

  T0 := Time;
  mergesort(Low(SortData),High(SortData),SortData,@myCompTag);
  T1 := Time;
  Writeln('myTag  ',FormatDateTime('NN:SS.ZZZ',T1-T0));

  FreeData (Data);
end.

```

;output:

```txt
Free pascal 2.6.4 32bit / Win7 / i 4330 3.5 Ghz
myText 00:03.158 / nearly worst case , all strings same sized and starting with '_000..'
myX    00:00.360
myY    00:00.363
myTag  00:00.283

```



## Perl


```perl
sub merge_sort {
    my @x = @_;
    return @x if @x < 2;
    my $m = int @x / 2;
    my @a = merge_sort(@x[0 .. $m - 1]);
    my @b = merge_sort(@x[$m .. $#x]);
    for (@x) {
        $_ = !@a            ? shift @b
           : !@b            ? shift @a
           : $a[0] <= $b[0] ? shift @a
           :                  shift @b;
    }
    @x;
}

my @a = (4, 65, 2, -31, 0, 99, 83, 782, 1);
@a = merge_sort @a;
print "@a\n";
```

Also note, the built-in function [http://perldoc.perl.org/functions/sort.html sort] uses mergesort.


## Perl 6

```perl6
sub merge_sort ( @a ) {
    return @a if @a <= 1;

    my $m = @a.elems div 2;
    my @l = flat merge_sort @a[  0 ..^ $m ];
    my @r = flat merge_sort @a[ $m ..^ @a ];

    return flat @l, @r if @l[*-1] !after @r[0];
    return flat gather {
        take @l[0] before @r[0] ?? @l.shift !! @r.shift
            while @l and @r;
        take @l, @r;
    }
}
my @data = 6, 7, 2, 1, 8, 9, 5, 3, 4;
say 'input  = ' ~ @data;
say 'output = ' ~ @data.&merge_sort;
```

```txt
input  = 6 7 2 1 8 9 5 3 4
output = 1 2 3 4 5 6 7 8 9
```



## Phix

Copy of [[Sorting_algorithms/Merge_sort#Euphoria|Euphoria]]

```Phix
function merge(sequence left, sequence right)
sequence result = {}
    while length(left)>0 and length(right)>0 do
        if left[1]<=right[1] then
            result = append(result, left[1])
            left = left[2..$]
        else
            result = append(result, right[1])
            right = right[2..$]
        end if
    end while
    return result & left & right
end function

function mergesort(sequence m)
sequence left, right
integer middle
    if length(m)<=1 then
        return m
    end if
    middle = floor(length(m)/2)
    left = mergesort(m[1..middle])
    right = mergesort(m[middle+1..$])
    if left[$]<=right[1] then
        return left & right
    elsif right[$]<=left[1] then
        return right & left
    end if
    return merge(left, right)
end function

constant s = shuffle(tagset(10))
? s
? mergesort(s)
```

```txt

{8,1,2,5,10,3,9,6,7,4}
{1,2,3,4,5,6,7,8,9,10}

```



## PHP


```php
function mergesort($arr){
	if(count($arr) == 1 ) return $arr;
	$mid = count($arr) / 2;
    $left = array_slice($arr, 0, $mid);
    $right = array_slice($arr, $mid);
	$left = mergesort($left);
	$right = mergesort($right);
	return merge($left, $right);
}

function merge($left, $right){
	$res = array();
	while (count($left) > 0 && count($right) > 0){
		if($left[0] > $right[0]){
			$res[] = $right[0];
			$right = array_slice($right , 1);
		}else{
			$res[] = $left[0];
			$left = array_slice($left, 1);
		}
	}
	while (count($left) > 0){
		$res[] = $left[0];
		$left = array_slice($left, 1);
	}
	while (count($right) > 0){
		$res[] = $right[0];
		$right = array_slice($right, 1);
	}
	return $res;
}

$arr = array( 1, 5, 2, 7, 3, 9, 4, 6, 8);
$arr = mergesort($arr);
echo implode(',',$arr);
```

```txt
1,2,3,4,5,6,7,8,9
```



## PicoLisp

PicoLisp's built-in sort routine uses merge sort. This is a high level implementation.

```lisp
(de alt (List)
   (if List (cons (car List) (alt (cddr List))) ()) )

(de merge (L1 L2)
   (cond
      ((not L2) L1)
      ((< (car L1) (car L2))
         (cons (car L1) (merge L2 (cdr L1))))
      (T (cons (car L2) (merge L1 (cdr L2)))) ) )

(de mergesort (List)
   (if (cdr List)
      (merge (mergesort (alt List)) (mergesort (alt (cdr List))))
      List) )

(mergesort (8 1 5 3 9 0 2 7 6 4))
```



## PL/I


```pli
MERGE: PROCEDURE (A,LA,B,LB,C);

/* Merge A(1:LA) with B(1:LB), putting the result in C
   B and C may share the same memory, but not with A.
*/
   DECLARE (A(*),B(*),C(*)) BYADDR POINTER;
   DECLARE (LA,LB) BYVALUE NONASGN FIXED BIN(31);
   DECLARE (I,J,K) FIXED BIN(31);
   DECLARE (SX) CHAR(58) VAR BASED (PX);
   DECLARE (SY) CHAR(58) VAR BASED (PY);
   DECLARE (PX,PY) POINTER;

   I=1; J=1; K=1;
   DO WHILE ((I <= LA) & (J <= LB));
      PX=A(I); PY=B(J);
      IF(SX <= SY) THEN
         DO; C(K)=A(I); K=K+1; I=I+1; END;
      ELSE
         DO; C(K)=B(J); K=K+1; J=J+1; END;
   END;
   DO WHILE (I <= LA);
      C(K)=A(I); I=I+1; K=K+1;
   END;
   RETURN;
END MERGE;

MERGESORT: PROCEDURE (AP,N) RECURSIVE ;

/* Sort the array AP containing N pointers to strings */

     DECLARE (AP(*))              BYADDR POINTER;
     DECLARE (N)                  BYVALUE NONASGN FIXED BINARY(31);
     DECLARE (M,I)                FIXED BINARY;
     DECLARE AMP1(1)              POINTER BASED(PAM);
     DECLARE (pX,pY,PAM) POINTER;
     DECLARE SX CHAR(58) VAR BASED(pX);
     DECLARE SY CHAR(58) VAR BASED(pY);

   IF (N=1) THEN RETURN;
   M = trunc((N+1)/2);
   IF (M>1) THEN CALL MERGESORT(AP,M);
   PAM=ADDR(AP(M+1));
   IF (N-M > 1) THEN CALL MERGESORT(AMP1,N-M);
   pX=AP(M); pY=AP(M+1);
   IF SX <= SY then return;     /* Skip Merge */
   DO I=1 to M; TP(I)=AP(I); END;
   CALL MERGE(TP,M,AMP1,N-M,AP);
   RETURN;
END MERGESORT;
```



## PowerShell


```PowerShell

function MergeSort([object[]] $SortInput)
{
	# The base case exits for minimal lists that are sorted by definition
	if ($SortInput.Length -le 1) {return $SortInput}

	# Divide and conquer
	[int] $midPoint = $SortInput.Length/2
	# The @() operators ensure a single result remains typed as an array
	[object[]] $left = @(MergeSort @($SortInput[0..($midPoint-1)]))
	[object[]] $right = @(MergeSort @($SortInput[$midPoint..($SortInput.Length-1)]))

	# Merge
	[object[]] $result = @()
	while (($left.Length -gt 0) -and ($right.Length -gt 0))
	{
		if ($left[0] -lt $right[0])
		{
			$result += $left[0]
			# Use an if/else rather than accessing the array range as $array[1..0]
			if ($left.Length -gt 1){$left = $left[1..$($left.Length-1)]}
			else {$left = @()}
		}
		else
		{
			$result += $right[0]
			# Without the if/else, $array[1..0] would return the whole array when $array.Length == 1
			if ($right.Length -gt 1){$right = $right[1..$($right.Length-1)]}
			else {$right = @()}
		}
	}

	# If we get here, either $left or $right is an empty array (or both are empty!).  Since the
	# rest of the unmerged array is already sorted, we can simply string together what we have.
	# This line outputs the concatenated result.  An explicit 'return' statement is not needed.
	$result + $left + $right
}

```



## Prolog


```prolog
% msort( L, S )
% True if S is a sorted copy of L, using merge sort
msort( [], [] ).
msort( [X], [X] ).
msort( U, S ) :- split(U, L, R), msort(L, SL), msort(R, SR), merge(SL, SR, S).

% split( LIST, L, R )
% Alternate elements of LIST in L and R
split( [], [], [] ).
split( [X], [X], [] ).
split( [L,R|T], [L|LT], [R|RT] ) :- split( T, LT, RT ).

% merge( LS, RS, M )
% Assuming LS and RS are sorted, True if M is the sorted merge of the two
merge( [], RS, RS ).
merge( LS, [], LS ).
merge( [L|LS], [R|RS], [L|T] ) :- L =< R, merge(    LS, [R|RS], T).
merge( [L|LS], [R|RS], [R|T] ) :- L > R,  merge( [L|LS],   RS,  T).
```




## PureBasic

A non-optimized version with lists.

```PureBasic
Procedure display(List m())
  ForEach m()
    Print(LSet(Str(m()), 3," "))
  Next
  PrintN("")
EndProcedure

;overwrites list m() with the merger of lists ma() and mb()
Procedure merge(List m(), List ma(), List mb())
  FirstElement(m())
  Protected ma_elementExists = FirstElement(ma())
  Protected mb_elementExists = FirstElement(mb())
  Repeat
    If ma() <= mb()
      m() = ma(): NextElement(m())
      ma_elementExists = NextElement(ma())
    Else
      m() = mb(): NextElement(m())
      mb_elementExists = NextElement(mb())
    EndIf
  Until Not (ma_elementExists And mb_elementExists)

  If ma_elementExists
    Repeat
      m() = ma(): NextElement(m())
    Until Not NextElement(ma())
  ElseIf mb_elementExists
    Repeat
      m() = mb(): NextElement(m())
    Until Not NextElement(mb())
  EndIf
EndProcedure

Procedure mergesort(List m())
  Protected NewList ma()
  Protected NewList mb()

  If ListSize(m()) > 1
    Protected current, middle = (ListSize(m()) / 2 ) - 1

    FirstElement(m())
    While current <= middle
      AddElement(ma())
      ma() = m()
      NextElement(m()): current + 1
    Wend

    PreviousElement(m())
    While NextElement(m())
      AddElement(mb())
      mb() = m()
    Wend

    mergesort(ma())
    mergesort(mb())
    LastElement(ma()): FirstElement(mb())
    If ma() <= mb()
      FirstElement(m())
      FirstElement(ma())
      Repeat
        m() = ma(): NextElement(m())
      Until Not NextElement(ma())
      Repeat
        m() = mb(): NextElement(m())
      Until Not NextElement(mb())
    Else
      merge(m(), ma(), mb())
    EndIf
  EndIf
EndProcedure

If OpenConsole()
  Define i
  NewList x()

  For i = 1 To 21: AddElement(x()): x() = Random(60): Next
  display(x())
  mergesort(x())
  display(x())

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

```txt
22 51 31 59 58 45 11 2  16 56 38 42 2  10 23 41 42 25 45 28 42
2  2  10 11 16 22 23 25 28 31 38 41 42 42 42 45 45 51 56 58 59
```



## Python

```python
from heapq import merge

def merge_sort(m):
    if len(m) <= 1:
        return m

    middle = len(m) // 2
    left = m[:middle]
    right = m[middle:]

    left = merge_sort(left)
    right = merge_sort(right)
    return list(merge(left, right))
```

Pre-2.6, merge() could be implemented like this:

```python
def merge(left, right):
    result = []
    left_idx, right_idx = 0, 0
    while left_idx < len(left) and right_idx < len(right):
        # change the direction of this comparison to change the direction of the sort
        if left[left_idx] <= right[right_idx]:
            result.append(left[left_idx])
            left_idx += 1
        else:
            result.append(right[right_idx])
            right_idx += 1

    if left_idx < len(left):
        result.extend(left[left_idx:])
    if right_idx < len(right):
        result.extend(right[right_idx:])
    return result
```



## R


```r
mergesort <- function(m)
{
   merge_ <- function(left, right)
   {
      result <- c()
      while(length(left) > 0 && length(right) > 0)
      {
         if(left[1] <= right[1])
         {
            result <- c(result, left[1])
            left <- left[-1]
         } else
         {
            result <- c(result, right[1])
            right <- right[-1]
         }
      }
      if(length(left) > 0) result <- c(result, left)
      if(length(right) > 0) result <- c(result, right)
      result
   }

   len <- length(m)
   if(len <= 1) m else
   {
      middle <- length(m) / 2
      left <- m[1:floor(middle)]
      right <- m[floor(middle+1):len]
      left <- mergesort(left)
      right <- mergesort(right)
      if(left[length(left)] <= right[1])
      {
         c(left, right)
      } else
      {
         merge_(left, right)
      }
   }
}
mergesort(c(4, 65, 2, -31, 0, 99, 83, 782, 1)) # -31   0   1   2   4  65  83  99 782
```



## Racket


```racket

#lang racket

(define (merge xs ys)
  (cond [(empty? xs) ys]
        [(empty? ys) xs]
        [(match* (xs ys)
           [((list* a as) (list* b bs))
            (cond [(<= a b) (cons a (merge as ys))]
                  [         (cons b (merge xs bs))])])]))

(define (merge-sort xs)
  (match xs
    [(or (list) (list _)) xs]
    [_ (define-values (ys zs) (split-at xs (quotient (length xs) 2)))
       (merge (merge-sort ys) (merge-sort zs))]))

```

This variation is bottom up:

```racket

#lang racket

(define (merge-sort xs)
  (merge* (map list xs)))

(define (merge* xss)
  (match xss
    [(list)    '()]
    [(list xs) xss]
    [(list xs ys zss ...)
     (merge* (cons (merge xs ys) (merge* zss)))]))

(define (merge xs ys)
  (cond [(empty? xs) ys]
        [(empty? ys) xs]
        [(match* (xs ys)
           [((list* a as) (list* b bs))
            (cond [(<= a b) (cons a (merge as ys))]
                  [         (cons b (merge xs bs))])])]))

```




## REBOL


```txt
msort: function [a compare] [msort-do merge] [
    if (length? a) < 2 [return a]
    ; define a recursive Msort-do function
    msort-do: function [a b l] [mid] [
        either l < 4 [
            if l = 3 [msort-do next b next a 2]
            merge a b 1 next b l - 1
        ] [
            mid: make integer! l / 2
            msort-do b a mid
            msort-do skip b mid skip a mid l - mid
            merge a b mid skip b mid l - mid
        ]
    ]
    ; function Merge is the key part of the algorithm
    merge: func [a b lb c lc] [
        until [
            either (compare first b first c) [
                change/only a first b
                b: next b
                a: next a
                zero? lb: lb - 1
            ] [
                change/only a first c
                c: next c
                a: next a
                zero? lc: lc - 1
            ]
        ]
        loop lb [
            change/only a first b
            b: next b
            a: next a
        ]
        loop lc [
            change/only a first c
            c: next c
            a: next a
        ]
    ]
    msort-do a copy a length? a
    a
]
```



## REXX

Note:   the array elements can be anything:   integers, floating point (exponentiated), character strings ···

```rexx
/*REXX program sorts a stemmed array (numbers or chars) using the  merge─sort algorithm.*/
@.=;                 @.1 = '---The seven deadly sins---'
                     @.2 = '
### =====================
'   ;      @.6 = "envy"
                     @.3 = 'pride'                         ;      @.7 = "gluttony"
                     @.4 = 'avarice'                       ;      @.8 = "sloth"
                     @.5 = 'wrath'                         ;      @.9 = "lust"
       do #=1  until @.#=='';  end;    #=#-1     /*# ≡ the number of entries in @ array.*/
call show@     'before sort'                     /*show the   "before"  array elements. */
     say copies('▒', 75)                         /*display a separator line to the term.*/
call mergeSort      #                            /*invoke the  merge sort  for the array*/
call show@     ' after sort'                     /*show the    "after"  array elements. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
mergeSort: procedure expose @.;        call mergeTo@ 1,arg(1);             return
show@: do j=1 for #; say right('element',20) right(j,length(#)) arg(1)":" @.j; end; return
/*──────────────────────────────────────────────────────────────────────────────────────*/
mergeTo@:  procedure expose @. !.;     parse arg L,n;     if n==1  then return;      h=L+1
           if n==2  then do;  if @.L>@.h  then do; _=@.h; @.h=@.L; @.L=_; end; return; end
           m=n % 2                                     /* [↑]  handle case of two items.*/
           call mergeTo@ L+m,n-m                       /*divide items  to the left   ···*/
           call mergeTo! L,m,1                         /*   "     "     "  "  right  ···*/
           i=1;   j=L+m;            do k=L  while k<j  /*whilst items on right exist ···*/
                                    if j==L+n | !.i<=@.j  then do;  @.k=!.i;  i=i+1;   end
                                                          else do;  @.k=@.j;  j=j+1;   end
                                    end   /*k*/
           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
mergeTo!:  procedure expose @. !.; parse arg L,n,T; if n==1  then do; !.T=@.L; return; end
           if n==2  then do;   h=L+1;    q=T+1;    !.q=@.L;    !.T=@.h;        return; end
           m=n % 2                                     /* [↑]  handle case of two items.*/
           call mergeTo@ L,m                           /*divide items  to the left   ···*/
           call mergeTo! L+m,n-m,m+T                   /*   "     "     "  "  right  ···*/
           i=L;   j=m+T;            do k=T  while k<j  /*whilst items on left exist  ···*/
                                    if j==T+n | @.i<=!.j  then do;  !.k=@.i;  i=i+1;   end
                                                          else do;  !.k=!.j;  j=j+1;   end
                                    end   /*k*/
           return
```

```txt

             element 1 before sort: ---The seven deadly sins---
             element 2 before sort:
### =====================

             element 3 before sort: pride
             element 4 before sort: avarice
             element 5 before sort: wrath
             element 6 before sort: envy
             element 7 before sort: gluttony
             element 8 before sort: sloth
             element 9 before sort: lust
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
             element 1  after sort: ---The seven deadly sins---
             element 2  after sort:
### =====================

             element 3  after sort: avarice
             element 4  after sort: envy
             element 5  after sort: gluttony
             element 6  after sort: lust
             element 7  after sort: pride
             element 8  after sort: sloth
             element 9  after sort: wrath

```



## Ruby


```ruby
def merge_sort(m)
  return m if m.length <= 1

  middle = m.length / 2
  left = merge_sort(m[0...middle])
  right = merge_sort(m[middle..-1])
  merge(left, right)
end

def merge(left, right)
  result = []
  until left.empty? || right.empty?
    result << (left.first<=right.first ? left.shift : right.shift)
  end
  result + left + right
end

ary = [7,6,5,9,8,4,3,1,2,0]
p merge_sort(ary)                  # => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```


Here's a version that monkey patches the Array class, with an example that demonstrates it's a stable sort

```ruby
class Array
  def mergesort(&comparitor)
    return self if length <= 1
    comparitor ||= proc{|a, b| a <=> b}
    middle = length / 2
    left  = self[0...middle].mergesort(&comparitor)
    right = self[middle..-1].mergesort(&comparitor)
    merge(left, right, comparitor)
  end

  private
  def merge(left, right, comparitor)
    result = []
    until left.empty? || right.empty?
      # change the direction of this comparison to change the direction of the sort
      if comparitor[left.first, right.first] <= 0
        result << left.shift
      else
        result << right.shift
      end
    end
    result + left + right
  end
end

ary = [7,6,5,9,8,4,3,1,2,0]
p ary.mergesort                    # => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
p ary.mergesort {|a, b| b <=> a}   # => [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]

ary = [["UK", "London"], ["US", "New York"], ["US", "Birmingham"], ["UK", "Birmingham"]]
p ary.mergesort
# => [["UK", "Birmingham"], ["UK", "London"], ["US", "Birmingham"], ["US", "New York"]]
p ary.mergesort {|a, b| a[1] <=> b[1]}
# => [["US", "Birmingham"], ["UK", "Birmingham"], ["UK", "London"], ["US", "New York"]]
```



## Rust

```rust

fn merge<T: Copy + PartialOrd>(x1: &[T], x2: &[T], y: &mut [T]) {
	assert_eq!(x1.len() + x2.len(), y.len());
	let mut i = 0;
	let mut j = 0;
	let mut k = 0;
	while i < x1.len() && j < x2.len() {
		if x1[i] < x2[j] {
			y[k] = x1[i];
			k += 1;
			i += 1;
		} else {
			y[k] = x2[j];
			k += 1;
			j += 1;
		}
	}
	if i < x1.len() {
		y[k..].copy_from_slice(&x1[i..]);
	}
	if j < x2.len() {
		y[k..].copy_from_slice(&x2[j..]);
	}
}

```


The sort algorithm :

```rust

fn merge_sort_rec<T: Copy + Ord>(x: &mut [T]) {
	let n = x.len();
	let m = n / 2;

	if n <= 1 {
		return;
	}

	merge_sort_rec(&mut x[0..m]);
	merge_sort_rec(&mut x[m..n]);

	let mut y: Vec<T> = x.to_vec();

	merge(&x[0..m], &x[m..n], &mut y[..]);

	x.copy_from_slice(&y);
}

```


Version without recursion call (faster) :

```rust

fn merge_sort<T: Copy + PartialOrd>(x: &mut [T]) {
	let n = x.len();
	let mut y = x.to_vec();
	let mut len = 1;
	while len < n {
		let mut i = 0;
		while i < n {
			if i + len >= n {
				y[i..].copy_from_slice(&x[i..]);
			} else if i + 2 * len > n {
				merge(&x[i..i+len], &x[i+len..], &mut y[i..]);
			} else {
				merge(&x[i..i+len], &x[i+len..i+2*len], &mut y[i..i+2*len]);
			}
			i += 2 * len;
		}
		len *= 2;
		if len >= n {
			x.copy_from_slice(&y);
			return;
		}
		i = 0;
		while i < n {
			if i + len >= n {
				x[i..].copy_from_slice(&y[i..]);
			} else if i + 2 * len > n {
				merge(&y[i..i+len], &y[i+len..], &mut x[i..]);
			} else {
				merge(&y[i..i+len], &y[i+len..i+2*len], &mut x[i..i+2*len]);
			}
			i += 2 * len;
		}
		len *= 2;
	}
}

```



## Scala

The use of Stream as the merge result avoids stack overflows without resorting to
tail recursion, which would typically require reversing the result, as well as being
a bit more convoluted.
```scala
def mergeSort(input: List[Int]) = {
  def merge(left: List[Int], right: List[Int]): Stream[Int] = (left, right) match {
    case (x :: xs, y :: ys) if x <= y => x #:: merge(xs, right)
    case (x :: xs, y :: ys) => y #:: merge(left, ys)
    case _ => if (left.isEmpty) right.toStream else left.toStream
  }
  def sort(input: List[Int], length: Int): List[Int] = input match {
    case Nil | List(_) => input
    case _ =>
      val middle = length / 2
      val (left, right) = input splitAt middle
      merge(sort(left, middle), sort(right, middle + length % 2)).toList
  }
  sort(input, input.length)
}
```

Replace the first two lines of <code>merge</code> by the following:

```scala
    case (x :: xs, y :: ys) if x < y => Stream.cons(x, merge(xs, right))
    case (x :: xs, y :: ys) => Stream.cons(y, merge(left, ys))
```

I suppose I should have written this version to begin with, but I think the 2.8 version is more clear.


## Scheme


```scheme
(define (merge-sort l gt?)
  (define (merge left right)
    (cond
     ((null? left)
      right)
     ((null? right)
      left)
     ((gt? (car left) (car right))
      (cons (car right)
            (merge left (cdr right))))
     (else
      (cons (car left)
            (merge (cdr left) right)))))
  (define (take l n)
    (if (zero? n)
      (list)
      (cons (car l)
            (take (cdr l) (- n 1)))))
  (let ((half (quotient (length l) 2)))
    (if (zero? half)
      l
      (merge (merge-sort (take      l half) gt?)
             (merge-sort (list-tail l half) gt?)))))
```


 (merge-sort '(1 3 5 7 9 8 6 4 2) >)


## Seed7


```seed7
const proc: mergeSort2 (inout array elemType: arr, in integer: lo, in integer: hi, inout array elemType: scratch) is func
  local
    var integer: mid is 0;
    var integer: k is 0;
    var integer: t_lo is 0;
    var integer: t_hi is 0;
  begin
    if lo < hi then
      mid := (lo + hi) div 2;
      mergeSort2(arr, lo, mid, scratch);
      mergeSort2(arr, succ(mid), hi, scratch);
      t_lo := lo;
      t_hi := succ(mid);
      for k range lo to hi do
        if t_lo <= mid and (t_hi > hi or arr[t_lo] <= arr[t_hi]) then
          scratch[k] := arr[t_lo];
          incr(t_lo);
        else
          scratch[k] := arr[t_hi];
          incr(t_hi);
        end if;
      end for;
      for k range lo to hi do
        arr[k] := scratch[k];
      end for;
    end if;
  end func;

const proc: mergeSort2 (inout array elemType: arr) is func
  local
    var array elemType: scratch is 0 times elemType.value;
  begin
    scratch := length(arr) times elemType.value;
    mergeSort2(arr, 1, length(arr), scratch);
  end func;
```

Original source: [http://seed7.sourceforge.net/algorith/sorting.htm#mergeSort2]


## Sidef


```ruby
func merge(left, right) {
    var result = []
    while (left && right) {
        result << [right,left].min_by{.first}.shift
    }
    result + left + right
}
 
func mergesort(array) {
    var len = array.len
    len < 2 && return array
 
    var (left, right) = array.part(len//2)
 
    left  = __FUNC__(left)
    right = __FUNC__(right)
 
    merge(left, right)
}
 
# Numeric sort
var nums = rand(1..100, 10)
say mergesort(nums)
 
# String sort
var strings = rand('a'..'z', 10)
say mergesort(strings)
```



## Standard ML


```sml
fun merge cmp ([], ys) = ys
  | merge cmp (xs, []) = xs
  | merge cmp (xs as x::xs', ys as y::ys') =
      case cmp (x, y) of GREATER => y :: merge cmp (xs, ys')
                       | _       => x :: merge cmp (xs', ys)
;
fun merge_sort cmp [] = []
  | merge_sort cmp [x] = [x]
  | merge_sort cmp xs = let
      val ys = List.take (xs, length xs div 2)
      val zs = List.drop (xs, length xs div 2)
    in
      merge cmp (merge_sort cmp ys, merge_sort cmp zs)
    end
;
merge_sort Int.compare [8,6,4,2,1,3,5,7,9]
```


## Swift


```Swift
// Merge Sort in Swift 4.2
// Source: https://github.com/raywenderlich/swift-algorithm-club/tree/master/Merge%20Sort
// NOTE: by use of generics you can make it sort arrays of any type that conforms to
//       Comparable protocol, however this is not always optimal

import Foundation

func mergeSort(_ array: [Int]) -> [Int] {
  guard array.count > 1 else { return array }

  let middleIndex = array.count / 2

  let leftPart = mergeSort(Array(array[0..<middleIndex]))
  let rightPart = mergeSort(Array(array[middleIndex..<array.count]))

  func merge(left: [Int], right: [Int]) -> [Int] {
    var leftIndex = 0
    var rightIndex = 0

    var merged = [Int]()
    merged.reserveCapacity(left.count + right.count)

    while leftIndex < left.count && rightIndex < right.count {
      if left[leftIndex] < right[rightIndex] {
        merged.append(left[leftIndex])
        leftIndex += 1
      } else if left[leftIndex] > right[rightIndex] {
        merged.append(right[rightIndex])
        rightIndex += 1
      } else {
        merged.append(left[leftIndex])
        leftIndex += 1
        merged.append(right[rightIndex])
        rightIndex += 1
      }
    }

    while leftIndex < left.count {
      merged.append(left[leftIndex])
      leftIndex += 1
    }

    while rightIndex < right.count {
      merged.append(right[rightIndex])
      rightIndex += 1
    }

    return merged
  }

  return merge(left: leftPart, right: rightPart)
}
```



## Tailspin

The standard recursive merge sort

```tailspin

templates mergesort
  templates merge
    @: $(2);
    [ $(1)... -> (
      <?($@merge<[](0)>)
      | ..$@merge(1)>
        $ !
      <>
        ^@merge(1) !
        $ -> #
     ),
     $@...] !
  end merge
  $ -> #

  <[](0..1)> $!
  <>
    def half: $::length / 2;
    [$(1..$half) -> mergesort, $($half+1..-1) -> mergesort] -> merge !
end mergesort

[4,5,3,8,1,2,6,7,9,8,5] -> mergesort -> !OUT::write

```

```txt

[1, 2, 3, 4, 5, 5, 6, 7, 8, 8, 9]

```


A little different spin where the array is first split into a list of single-element lists and then merged.

```tailspin

templates mergesort
  templates merge
    @: $(2);
    $(1)... -> (
      <?($@merge<[](0)>)
      | ..$@merge(1)>
        $ !
      <>
        ^@merge(1) !
        $ -> #
     ) !
     $@... !
  end merge

  templates mergePairs
    <[](1)>
      $(1) !
    <[](2..)>
      [$(1..2) -> merge] !
      $(3..-1) -> #
  end mergePairs

  templates mergeAll
    <[](0)>
      $ !
    <[](1)>
      $(1) !
    <>
      [ $ -> mergePairs ] -> #
  end mergeAll

  $ -> [ $... -> [ $ ] ] -> mergeAll !
end mergesort

[4,5,3,8,1,2,6,7,9,8,5] -> mergesort -> !OUT::write

```

```txt

[1, 2, 3, 4, 5, 5, 6, 7, 8, 8, 9]

```



## Tcl


```tcl
package require Tcl 8.5

proc mergesort m {
    set len [llength $m]
    if {$len <= 1} {
        return $m
    }
    set middle [expr {$len / 2}]
    set left [lrange $m 0 [expr {$middle - 1}]]
    set right [lrange $m $middle end]
    return [merge [mergesort $left] [mergesort $right]]
}

proc merge {left right} {
    set result [list]
    while {[set lleft [llength $left]] > 0 && [set lright [llength $right]] > 0} {
        if {[lindex $left 0] <= [lindex $right 0]} {
            set left [lassign $left value]
        } else {
            set right [lassign $right value]
        }
        lappend result $value
    }
    if {$lleft > 0} {
        lappend result {*}$left
    }
    if {$lright > 0} {
        set result [concat $result $right] ;# another way append elements
    }
    return $result
}

puts [mergesort {8 6 4 2 1 3 5 7 9}] ;# => 1 2 3 4 5 6 7 8 9
```

Also note that Tcl's built-in <tt>lsort</tt> command uses the mergesort algorithm.


## UnixPipes

```bash
split() {
   (while read a b ; do
       echo $a > $1 ; echo $b > $2
   done)
}

mergesort() {
 xargs -n 2 | (read a b; test -n "$b" && (
     lc="1.$1" ; gc="2.$1"
     (echo $a $b;cat)|split >(mergesort $lc >$lc) >( mergesort $gc >$gc)
     sort -m $lc $gc
     rm -f $lc $gc;
 ) || echo $a)
}

cat to.sort | mergesort
```



## Ursala


```Ursala
#import std

mergesort "p" = @iNCS :-0 ~&B^?a\~&YaO "p"?abh/~&alh2faltPrXPRC ~&arh2falrtPXPRC

#show+

example = mergesort(lleq) <'zoh','zpb','hhh','egi','bff','cii','yid'>
```

```txt

bff
cii
egi
hhh
yid
zoh
zpb
```

The mergesort function could also have been defined using the built in sorting operator, -<, because the same algorithm is used.

```Ursala
mergesort "p" = "p"-<
```



## V

merge uses the helper mergei to merge two lists. The mergei takes a stack of the form [mergedlist] [list1] [list2]
it then extracts one element from list2, splits the list1 with it, joins the older merged list, first part of list1 and the element that was used for splitting (taken from list2) into the new merged list. the new list1 is the second part of the split on older list1. new list2 is the list remaining after the element e2 was extracted from it.

```v
[merge
   [mergei
       uncons [swap [>] split] dip
       [[*m] e2 [*a1] b1 a2 : [*m *a1 e2] b1 a2] view].

   [a b : [] a b] view
   [size zero?] [pop concat]
       [mergei]
   tailrec].

[msort
  [splitat [arr a : [arr a take arr a drop]] view i].
  [splitarr dup size 2 / >int splitat].

  [small?] []
    [splitarr]
    [merge]
  binrec].
```


 [8 7 6 5 4 2 1 3 9] msort puts


## XPL0

This is based on an example in "Fundamentals of Computer Algorithms" by
Horowitz & Sahni.

```XPL0
code Reserve=3, ChOut=8, IntOut=11;

proc MergeSort(A, Low, High);   \Sort array A from Low to High
int  A, Low, High;
int  B, Mid, H, I, J, K;
[if Low >= High then return;
Mid:= (Low+High) >> 1;          \split array in half (roughly)
MergeSort(A, Low, Mid);         \sort left half
MergeSort(A, Mid+1, High);      \sort right half
\Merge the two halves in to sorted order
B:= Reserve((High-Low+1)*4);    \reserve space for working array (4 bytes/int)
H:= Low;  I:= Low;  J:= Mid+1;
while H<=Mid & J<=High do       \merge while both halves have items
    if A(H) <= A(J) then [B(I):= A(H);  I:= I+1;  H:= H+1]
                    else [B(I):= A(J);  I:= I+1;  J:= J+1];
if H > Mid then                 \copy any remaining elements
     for K:= J to High do [B(I):= A(K);  I:= I+1]
else for K:= H to Mid  do [B(I):= A(K);  I:= I+1];
for K:= Low to High do A(K):= B(K);
];

int  A, I;
[A:= [3, 1, 4, 1, -5, 9, 2, 6, 5, 4];
MergeSort(A, 0, 10-1);
for I:= 0 to 10-1 do [IntOut(0, A(I));  ChOut(0, ^ )];
]
```


```txt

-5 1 1 2 3 4 4 5 6 9

```


## ZED

Source -> http://ideone.com/uZEPL4
Compiled -> http://ideone.com/SJ5EGu

This is a bottom up version of merge sort:

```zed
(append) list1 list2
comment:
#true
(003) "append" list1 list2

(car) pair
comment:
#true
(002) "car" pair

(cdr) pair
comment:
#true
(002) "cdr" pair

(cons) one two
comment:
#true
(003) "cons" one two

(map) function list
comment:
#true
(003) "map" function list

(merge) comparator list1 list2
comment:
#true
(merge1) comparator list1 list2 nil

(merge1) comparator list1 list2 collect
comment:
(null?) list2
(append) (reverse) collect list1

(merge1) comparator list1 list2 collect
comment:
(null?) list1
(append) (reverse) collect list2

(merge1) comparator list1 list2 collect
comment:
(003) comparator (car) list2 (car) list1
(merge1) comparator list1 (cdr) list2 (cons) (car) list2 collect

(merge1) comparator list1 list2 collect
comment:
#true
(merge1) comparator (cdr) list1 list2 (cons) (car) list1 collect

(null?) value
comment:
#true
(002) "null?" value

(reverse) list
comment:
#true
(002) "reverse" list

(sort) comparator jumble
comment:
#true
(car) (sort11) comparator (sort1) jumble

(sort1) jumble
comment:
#true
(map) "list" jumble

(sort11) comparator jumble
comment:
(null?) jumble
nil

(sort11) comparator jumble
comment:
(null?) (cdr) jumble
jumble

(sort11) comparator jumble
comment:
#true
(sort11) comparator
         (cons) (merge) comparator (car) jumble (002) "cadr" jumble
                (sort11) comparator (002) "cddr" jumble
```



## zkl

Pretty wasteful memory wise, probably not suitable for large sorts.
```zkl
fcn _merge(left,right){
   if (not left)  return(right);
   if (not right) return(left);
   l:=left[0]; r:=right[0];
   if (l<=r) return(L(l).extend(self.fcn(left[1,*],right)));
   else      return(L(r).extend(self.fcn(left,right[1,*])));
}

fcn merge_sort(L){
   if (L.len()<2) return(L);
   n:=L.len()/2;
   return(_merge(self.fcn(L[0,n]), self.fcn(L[n,*])));
}
```


```zkl
merge_sort(T(1,3,5,7,9,8,6,4,2)).println();
merge_sort("big fjords vex quick waltz nymph").concat().println();
```

```txt

L(1,2,3,4,5,6,7,8,9)
     abcdefghiijklmnopqrstuvwxyz

```

Or, for lists only:

```zkl
fcn mergeSort(L){
   if (L.len()<2) return(L.copy());
   n:=L.len()/2;
   self.fcn(L[0,n]).merge(self.fcn(L[n,*]));
}
```


```zkl
mergeSort(T(1,3,5,7,9,8,6,4,2)).println();
mergeSort("big fjords vex quick waltz nymph".split("")).concat().println();
```

```txt

L(1,2,3,4,5,6,7,8,9)
     abcdefghiijklmnopqrstuvwxyz

```

