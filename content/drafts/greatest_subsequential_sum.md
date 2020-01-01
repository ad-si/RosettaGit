+++
title = "Greatest subsequential sum"
description = ""
date = 2019-06-14T18:45:05Z
aliases = []
[extra]
id = 2085
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[Category:Arithmetic operations]]

;Task:
Given a sequence of integers, find a continuous subsequence which maximizes the sum of its elements, that is, the elements of no other single subsequence add up to a value larger than this one.


An empty subsequence is considered to have the sum of   '''0''';   thus if all elements are negative, the result must be the empty sequence.





## Ada


```ada
with Ada.Text_Io; use Ada.Text_Io;

procedure Max_Subarray is
   type Int_Array is array (Positive range <>) of Integer;
   Empty_Error : Exception;
   function Max(Item : Int_Array) return Int_Array is
      Start : Positive;
      Finis : Positive;
      Max_Sum : Integer := Integer'First;
      Sum : Integer;
   begin
      if Item'Length = 0 then
         raise Empty_Error;
      end if;

      for I in Item'range loop
         Sum := 0;
         for J in I..Item'Last loop
            Sum := Sum + Item(J);
            if Sum > Max_Sum then
               Max_Sum := Sum;
               Start := I;
               Finis := J;
            end if;
         end loop;
      end loop;
      return Item(Start..Finis);
   end Max;
   A : Int_Array := (-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1);
   B : Int_Array := Max(A);
begin
   for I in B'range loop
      Put_Line(Integer'Image(B(I)));
   end loop;
exception
   when Empty_Error =>
      Put_Line("Array being analyzed has no elements.");
end Max_Subarray;
```



## Aime


```aime
gsss(list l, integer &start, &end, &maxsum)
{
    integer e, f, i, sum;

    end = f = maxsum = start = sum = 0;
    for (i, e in l) {
        sum += e;
        if (sum < 0) {
            sum = 0;
            f = i + 1;
        } elif (maxsum < sum) {
            maxsum = sum;
            end = i + 1;
            start = f;
        }
    }
}

main(void)
{
    integer start, end, sum;
    list l;

    l = list(-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1);
    gsss(l, start, end, sum);
    o_("Max sum ", sum, "\n");
    if (start < end) {
        l.ocall(o_, 1, start, end - 1, " ");
        o_newline();
    }

    0;
}
```

{{Out}}

```txt
Max sum 15
 3 5 6 -2 -1 4
```



## ALGOL 68

{{trans|C}}
{{works with|ALGOL 68|Revision 1 - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
main:
(
        []INT a = (-1 , -2 , 3 , 5 , 6 , -2 , -1 , 4 , -4 , 2 , -1);

        INT begin max, end max, max sum, sum;

        sum := 0;
        begin max := 0;
        end max := -1;
        max sum := 0;


        FOR begin FROM LWB a TO UPB a DO
                sum := 0;
                FOR end FROM begin TO UPB a DO
                        sum +:= a[end];
                        IF sum > max sum THEN
                                max sum := sum;
                                begin max := begin;
                                end max := end
                        FI
                OD
        OD;

        FOR i FROM begin max TO end max DO
                print(a[i])
        OD

)
```

{{out}}

```txt

         +3         +5         +6         -2         -1         +4

```



## AppleScript

{{Trans|Haskell}}
Linear derivation of both sum and list, in a single fold:

```applescript
-- maxSubseq :: [Int] -> [Int] -> (Int, [Int])
on maxSubseq(xs)
    script go
        on |λ|(ab, x)
            set a to fst(ab)
            set {m1, m2} to {fst(a), snd(a)}
            set high to max(Tuple(0, {}), Tuple(m1 + x, m2 & {x}))
            Tuple(high, max(snd(ab), high))
        end |λ|
    end script

    snd(foldl(go, Tuple(Tuple(0, {}), Tuple(0, {})), xs))
end maxSubseq


-- TEST ---------------------------------------------------
on run
    set mx to maxSubseq({-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1})
    {fst(mx), snd(mx)}
end run


-- GENERIC ABSTRACTIONS -----------------------------------

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- gt :: Ord a => a -> a -> Bool
on gt(x, y)
    set c to class of x
    if record is c or list is c then
        fst(x) > fst(y)
    else
        x > y
    end if
end gt

-- fst :: (a, b) -> a
on fst(tpl)
    if class of tpl is record then
        |1| of tpl
    else
        item 1 of tpl
    end if
end fst

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- max :: Ord a => a -> a -> a
on max(x, y)
    if gt(x, y) then
        x
    else
        y
    end if
end max

-- snd :: (a, b) -> b
on snd(tpl)
    if class of tpl is record then
        |2| of tpl
    else
        item 2 of tpl
    end if
end snd

-- Tuple (,) :: a -> b -> (a, b)
on Tuple(a, b)
    {type:"Tuple", |1|:a, |2|:b, length:2}
end Tuple
```

{{Out}}

```txt
{15, {3, 5, 6, -2, -1, 4}}
```



## ATS


```ATS

(*
** This one is
** translated into ATS from the Ocaml entry
*)

(* ****** ****** *)
//
// How to compile:
// patscc -DATS_MEMALLOC_LIBC -o maxsubseq maxsubseq.dats
//
(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)

typedef ints = List0(int)

(* ****** ****** *)

fun
maxsubseq
  (xs: ints): (int, ints) = let
//
fun
loop
(
  sum: int, seq: ints
, maxsum: int, maxseq: ints, xs: ints
) : (int, ints) =
(
case+ xs of
| nil () =>
  (
    maxsum
  , list_vt2t(list_reverse(maxseq))
  ) (* end of [nil] *)
| cons (x, xs) => let
    val sum = sum + x
    and seq = cons (x, seq)
  in
    if sum < 0
      then loop (0, nil, maxsum, maxseq, xs)
      else (
        if sum > maxsum
          then loop (sum, seq, sum, seq, xs)
          else loop (sum, seq, maxsum, maxseq, xs)
      ) (* end of [else] *)
  end // end of [cons]
)
//
in
  loop (0, nil, 0, nil, xs)
end // end of [maxsubseq]

implement
main0 () = () where
{
val
(maxsum
,maxseq) =
maxsubseq
(
  $list{int}(~1,~2,3,5,6,~2,~1,4,~4,2,~1)
)
//
val () = println! ("maxsum = ", maxsum)
val () = println! ("maxseq = ", maxseq)
//
} (* end of [main0] *)

```

{{out}}

```txt

maxsum = 15
maxseq = 3, 5, 6, -2, -1, 4

```



## AutoHotkey

classic algorithm:

```AutoHotkey
seq = -1,-2,3,5,6,-2,-1,4,-4,2,-1
max := sum := start := 0
Loop Parse, seq, `,
   If (max < sum+=A_LoopField)
      max := sum, a := start, b := A_Index
   Else If sum <= 0
      sum := 0, start := A_Index
; read out the best subsequence
Loop Parse, seq, `,
   s .= A_Index > a && A_Index <= b ? A_LoopField "," : ""
MsgBox % "Max = " max "`n[" SubStr(s,1,-1) "]"
```



## AutoIt


```AutoIt

Local $iArray[11] = [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1]
GREAT_SUB($iArray)
Local $iArray[5] = [-1, -2, -3, -4, -5]
GREAT_SUB($iArray)
Local $iArray[15] = [7, -6, -8, 5, -2, -6, 7, 4, 8, -9, -3, 2, 6, -4, -6]
GREAT_SUB($iArray)

Func GREAT_SUB($iArray)
	Local $iSUM = 0, $iBEGIN_MAX = 0, $iEND_MAX = -1, $iMAX_SUM = 0
	For $i = 0 To UBound($iArray) - 1
		$iSUM = 0
		For $k = $i To UBound($iArray) - 1
			$iSUM += $iArray[$k]
			If $iSUM > $iMAX_SUM Then
				$iMAX_SUM = $iSUM
				$iEND_MAX = $k
				$iBEGIN_MAX = $i
			EndIf
		Next
	Next
	ConsoleWrite("> Array: [")
	For $i = 0 To UBound($iArray) - 1
		If $iArray[$i] > 0 Then ConsoleWrite("+")
		ConsoleWrite($iArray[$i])
		If $i <> UBound($iArray) - 1 Then ConsoleWrite(",")
	Next
	ConsoleWrite("]" & @CRLF & "+>Maximal subsequence: [")
	$iSUM = 0
	For $i = $iBEGIN_MAX To $iEND_MAX
		$iSUM += $iArray[$i]
		If $iArray[$i] > 0 Then ConsoleWrite("+")
		ConsoleWrite($iArray[$i])
		If $i <> $iEND_MAX Then ConsoleWrite(",")
	Next
	ConsoleWrite("]" & @CRLF & "!>SUM of subsequence: " & $iSUM & @CRLF)
EndFunc   ;==>GREAT_SUB

```


{{out}}

```txt
> Array: [-1,-2,+3,+5,+6,-2,-1,+4,-4,+2,-1]
+>Maximal subsequence: [+3,+5,+6,-2,-1,+4]
!>SUM of subsequence: 15
> Array: [-1,-2,-3,-4,-5]
+>Maximal subsequence: []
!>SUM of subsequence: 0
> Array: [+7,-6,-8,+5,-2,-6,+7,+4,+8,-9,-3,+2,+6,-4,-6]
+>Maximal subsequence: [+7,+4,+8]
!>SUM of subsequence: 19
```



## AWK

{{trans|Common Lisp}}

```awk
# Finds the subsequence of ary[1] to ary[len] with the greatest sum.
# Sets subseq[1] to subseq[n] and returns n. Also sets subseq["sum"].
# An empty subsequence has sum 0.
function maxsubseq(subseq, ary, len,    b, bp, bs, c, cp, i) {
	b = 0		# best sum
	c = 0		# current sum
	bp = 0		# position of best subsequence
	bn = 0		# length of best subsequence
	cp = 1		# position of current subsequence

	for (i = 1; i <= len; i++) {
		c += ary[i]
		if (c < 0) {
			c = 0
			cp = i + 1
		}
		if (c > b) {
			b = c
			bp = cp
			bn = i + 1 - cp
		}
	}

	for (i = 1; i <= bn; i++)
		subseq[i] = ary[bp + i - 1]
	subseq["sum"] = b
	return bn
}
```


Demonstration:
```awk
# Joins the elements ary[1] to ary[len] in a string.
function join(ary, len,    i, s) {
	s = "["
	for (i = 1; i <= len; i++) {
		s = s ary[i]
		if (i < len)
			s = s ", "
	}
	s = s "]"
	return s
}

# Demonstrates maxsubseq().
function try(str,    ary, len, max, maxlen) {
	len = split(str, ary)
	print "Array: " join(ary, len)
	maxlen = maxsubseq(max, ary, len)
	print "  Maximal subsequence: " \
	    join(max, maxlen) ", sum " max["sum"]
}

BEGIN {
	try("-1 -2 -3 -4 -5")
	try("0 1 2 -3 3 -1 0 -4 0 -1 -4 2")
	try("-1 -2 3 5 6 -2 -1 4 -4 2 -1")
}
```


{{out}}

```txt
Array: [-1, -2, -3, -4, -5]
  Maximal subsequence: [], sum 0
Array: [0, 1, 2, -3, 3, -1, 0, -4, 0, -1, -4, 2]
  Maximal subsequence: [0, 1, 2], sum 3
Array: [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1]
  Maximal subsequence: [3, 5, 6, -2, -1, 4], sum 15
```



## BBC BASIC


```bbcbasic
      DIM A%(11) : A%() = 0, 1, 2, -3, 3, -1, 0, -4, 0, -1, -4, 2
      PRINT FNshowarray(A%()) " -> " FNmaxsubsequence(A%())
      DIM B%(10) : B%() = -1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1
      PRINT FNshowarray(B%()) " -> " FNmaxsubsequence(B%())
      DIM C%(4) : C%() = -1, -2, -3, -4, -5
      PRINT FNshowarray(C%()) " -> " FNmaxsubsequence(C%())
      END

      DEF FNmaxsubsequence(a%())
      LOCAL a%, b%, i%, j%, m%, s%, a$
      a% = 1
      FOR i% = 0 TO DIM(a%(),1)
        s% = 0
        FOR j% = i% TO DIM(a%(),1)
          s% += a%(j%)
          IF s% > m% THEN
            m% = s%
            a% = i%
            b% = j%
          ENDIF
        NEXT
      NEXT i%
      IF a% > b% THEN = "[]"
      a$ = "["
      FOR i% = a% TO b%
        a$ += STR$(a%(i%)) + ", "
      NEXT
      = LEFT$(LEFT$(a$)) + "]"

      DEF FNshowarray(a%())
      LOCAL i%, a$
      a$ = "["
      FOR i% = 0 TO DIM(a%(),1)
        a$ += STR$(a%(i%)) + ", "
      NEXT
      = LEFT$(LEFT$(a$)) + "]"
```

{{out}}

```txt

[0, 1, 2, -3, 3, -1, 0, -4, 0, -1, -4, 2] -> [0, 1, 2]
[-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1] -> [3, 5, 6, -2, -1, 4]
[-1, -2, -3, -4, -5] -> []

```



## Component Pascal

Works with BlackBox Component Builder

```oberon2

MODULE OvctGreatestSubsequentialSum;
IMPORT StdLog, Strings, Args;

PROCEDURE Gss(iseq: ARRAY OF INTEGER;OUT start, end, maxsum: INTEGER);
VAR
	i,j,sum: INTEGER;
BEGIN
	i := 0; maxsum := 0; start := 0; end := -1;
	WHILE i < LEN(iseq) - 1 DO
		sum := 0; j := i;
		WHILE j < LEN(iseq) -1 DO
			INC(sum ,iseq[j]);
			IF sum > maxsum THEN
				maxsum := sum;
				start := i;
				end := j
			END;
			INC(j);
		END;
		INC(i)
	END
END Gss;

PROCEDURE Do*;
VAR
	p: Args.Params;
	iseq: POINTER TO ARRAY OF INTEGER;
	i, res, start, end, sum: INTEGER;
BEGIN
	Args.Get(p); (* Get Params *)
	NEW(iseq,p.argc);
	(* Transform params to INTEGERs *)
	FOR i := 0 TO p.argc - 1 DO
		Strings.StringToInt(p.args[i],iseq[i],res)
	END;
	Gss(iseq,start,end,sum);
	StdLog.String("[");
	FOR i := start TO  end DO
		StdLog.Int(iseq[i]);
		IF i < end THEN StdLog.String(",") END
	END;
	StdLog.String("]=");StdLog.Int(sum);StdLog.Ln;
END Do;

END OvctGreatestSubsequentialSum.

```

Execute: <br/>

```txt

[Ctrl-Q]OvctGreatestSubsequentialSum.Do -1 -2 3 5 6 -2 -1 4 -4 2 -2 ~
[Ctrl-Q]OvctGreatestSubsequentialSum.Do -1 -5 -3 ~

```

{{out}}

```txt

[ 3, 5, 6, -2, -1, 4]= 15
[]= 0

```


## Bracmat


This program iterates over all subsequences by forced backtracking, caused by the failing node <code>~</code> at the end of the middle part of the pattern. The combination of flags <code>[%</code> on an expression creates a pattern that succeeds if and only if the expression is successfully evaluated. <code>sjt</code> is an extra argument to any function that is part of a pattern and it contains the current subexpression candidate. Inside the pattern the function <code>sum</code> sums over all elements in <code>sjt</code>. The currently longest maximal subsequence is kept in <code>seq</code>.


```bracmat

(   0:?max
  & :?seq
  &   -1 -2 3 5 6 -2 -1 4 -4 2 -1
    :   ?
        [%(   (
              =   s sum
                .   ( sum
                    =   A
                      .   !arg:%?A ?arg&!A+sum$!arg
                        | 0
                    )
                  & (   sum$!sjt:>!max:?max
                      & !sjt:?seq
                    |
                    )
              )
            $
          & ~
          )
        ?
| !seq
)

```



```txt
3 5 6 -2 -1 4
```



## C


```c
#include "stdio.h"

typedef struct Range {
    int start, end, sum;
} Range;

Range maxSubseq(const int sequence[], const int len) {
    int maxSum = 0, thisSum = 0, i = 0;
    int start = 0, end = -1, j;

    for (j = 0; j < len; j++) {
        thisSum += sequence[j];
        if (thisSum < 0) {
            i = j + 1;
            thisSum = 0;
        } else if (thisSum > maxSum) {
            maxSum = thisSum;
            start = i;
            end   = j;
        }
    }

    Range r;
    if (start <= end && start >= 0 && end >= 0) {
        r.start = start;
        r.end = end + 1;
        r.sum = maxSum;
    } else {
        r.start = 0;
        r.end = 0;
        r.sum = 0;
    }
    return r;
}

int main(int argc, char **argv) {
    int a[] = {-1 , -2 , 3 , 5 , 6 , -2 , -1 , 4 , -4 , 2 , -1};
    int alength = sizeof(a)/sizeof(a[0]);

    Range r = maxSubseq(a, alength);
    printf("Max sum = %d\n", r.sum);
    int i;
    for (i = r.start; i < r.end; i++)
        printf("%d ", a[i]);
    printf("\n");

    return 0;
}
```

{{out}}

```txt
Max sum = 15
3 5 6 -2 -1 4
```



## C++


```cpp
#include <utility>
// for std::pair
#include <iterator>  // for std::iterator_traits
#include <iostream>  // for std::cout
#include <ostream>   // for output operator and std::endl
#include <algorithm> // for std::copy
#include <iterator>  // for std::output_iterator

// Function template max_subseq
//
// Given a sequence of integers, find a subsequence which maximizes
// the sum of its elements, that is, the elements of no other single
// subsequence add up to a value larger than this one.
//
// Requirements:
// * ForwardIterator is a forward iterator
// * ForwardIterator's value_type is less-than comparable and addable
// * default-construction of value_type gives the neutral element
//   (zero)
// * operator+ and operator< are compatible (i.e. if a>zero and
//   b>zero, then a+b>zero, and if a<zero and b<zero, then a+b<zero)
// * [begin,end) is a valid range
//
// Returns:
//   a pair of iterators describing the begin and end of the
//   subsequence
template<typename ForwardIterator>
 std::pair<ForwardIterator, ForwardIterator>
 max_subseq(ForwardIterator begin, ForwardIterator end)
{
  typedef typename std::iterator_traits<ForwardIterator>::value_type
    value_type;

  ForwardIterator seq_begin = begin, seq_end = seq_begin;
  value_type seq_sum = value_type();
  ForwardIterator current_begin = begin;
  value_type current_sum = value_type();

  value_type zero = value_type();

  for (ForwardIterator iter = begin; iter != end; ++iter)
  {
    value_type value = *iter;
    if (zero < value)
    {
      if (current_sum < zero)
      {
        current_sum = zero;
        current_begin = iter;
      }
    }
    else
    {
      if (seq_sum < current_sum)
      {
        seq_begin = current_begin;
        seq_end = iter;
        seq_sum = current_sum;
      }
    }
    current_sum += value;
  }

  if (seq_sum < current_sum)
  {
    seq_begin = current_begin;
    seq_end = end;
    seq_sum = current_sum;
  }

  return std::make_pair(seq_begin, seq_end);
}

// the test array
int array[] = { -1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1 };

// function template to find the one-past-end pointer to the array
template<typename T, int N> int* end(T (&arr)[N]) { return arr+N; }

int main()
{
  // find the subsequence
  std::pair<int*, int*> seq = max_subseq(array, end(array));

  // output it
  std::copy(seq.first, seq.second, std::ostream_iterator<int>(std::cout, " "));
  std::cout << std::endl;

  return 0;
}
```


## C#
'''The challange'''

```c#
using System;

namespace Tests_With_Framework_4
{
    class Program
    {
        static void Main(string[] args)
        {
            int[] integers = { -1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1 }; int length = integers.Length;
            int maxsum, beginmax, endmax, sum; maxsum = beginmax = sum = 0; endmax = -1;

            for (int i = 0; i < length; i++)
            {
                sum = 0;
                for (int k = i; k < length; k++)
                {
                    sum += integers[k];
                    if (sum > maxsum)
                    {
                        maxsum = sum;
                        beginmax = i;
                        endmax = k;
                    }
                }
            }

            for (int i = beginmax; i <= endmax; i++)
                Console.WriteLine(integers[i]);

            Console.ReadKey();
        }
    }
}
```



## Clojure

{{trans|Haskell}}
Naive algorithm:

```clojure
(defn max-subseq-sum [coll]
  (->> (take-while seq (iterate rest coll)) ; tails
       (mapcat #(reductions conj [] %)) ; inits
       (apply max-key #(reduce + %)))) ; max sum
```


{{out}}

```clojure>user
 (max-subseq-sum [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1])
[3 5 6 -2 -1 4]
```



## CoffeeScript


```coffeescript

max_sum_seq = (sequence) ->
  # This runs in linear time.
  [sum_start, sum, max_sum, max_start, max_end] = [0, 0, 0, 0, 0]
  for n, i in sequence
    sum += n
    if sum > max_sum
      max_sum = sum
      max_start = sum_start
      max_end = i + 1
    if sum < 0 # start new sequence
      sum = 0
      sum_start = i + 1
  sequence[max_start...max_end]

# tests
console.log max_sum_seq [-1, 0, 15, 3, -9, 12, -4]
console.log max_sum_seq [-1]
console.log max_sum_seq [4, -10, 3]

```



## Common Lisp



### =Linear Time=


Returns the maximum subsequence sum, the subsequence with the maximum sum, and start and end indices for the subsequence within the original sequence.  Based on the implementation at [http://wordaligned.org/articles/the-maximum-subsequence-problem Word Aligned].  Leading zeroes aren't trimmed from the subsequence.


```lisp
(defun max-subseq (list)
  (let ((best-sum 0) (current-sum 0) (end 0))
    ;; determine the best sum, and the end of the max subsequence
    (do ((list list (rest list))
         (i 0 (1+ i)))
        ((endp list))
      (setf current-sum (max 0 (+ current-sum (first list))))
      (when (> current-sum best-sum)
        (setf end i
              best-sum current-sum)))
    ;; take the subsequence of list ending at end, and remove elements
    ;; from the beginning until the subsequence sums to best-sum.
    (let* ((sublist (subseq list 0 (1+ end)))
           (sum (reduce #'+ sublist)))
      (do ((start 0 (1+ start))
           (sublist sublist (rest sublist))
           (sum sum (- sum (first sublist))))
          ((or (endp sublist) (eql sum best-sum))
           (values best-sum sublist start (1+ end)))))))
```


For example,

 > (max-subseq '(-1 -2 -3 -4 -5))
 0
 NIL
 1
 1

 > (max-subseq '(0 1 2 -3 3 -1 0 -4 0 -1 -4 2))
 3
 (0 1 2)
 0
 3


### =Brute Force=


{{trans|PicoLisp}}


```lisp
(defun max-subseq (seq)
  (loop for subsequence in (mapcon (lambda (x) (maplist #'reverse (reverse x))) seq)
        for sum = (reduce #'+ subsequence :initial-value 0)
        with max-subsequence
        maximizing sum into max
        if (= sum max) do (setf max-subsequence subsequence)
        finally (return max-subsequence))))
```



## D

{{trans|Python}}

```d
import std.stdio;

inout(T[]) maxSubseq(T)(inout T[] sequence) pure nothrow @nogc {
    int maxSum, thisSum, i, start, end = -1;

    foreach (immutable j, immutable x; sequence) {
        thisSum += x;
        if (thisSum < 0) {
            i = j + 1;
            thisSum = 0;
        } else if (thisSum > maxSum) {
            maxSum = thisSum;
            start = i;
            end   = j;
        }
    }

    if (start <= end && start >= 0 && end >= 0)
        return sequence[start .. end + 1];
    else
        return [];
}

void main() {
    const a1 = [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1];
    writeln("Maximal subsequence: ", a1.maxSubseq);

    const a2 = [-1, -2, -3, -5, -6, -2, -1, -4, -4, -2, -1];
    writeln("Maximal subsequence: ", a2.maxSubseq);
}
```

{{out}}

```txt
Maximal subsequence: [3, 5, 6, -2, -1, 4]
Maximal subsequence: []
```



### Alternative Version

{{trans|Haskell}}
This version is much less efficient. The output is similar.
Currently the D standard library lacks the sum, inits, tails functions,
and max can't be used as the maximumBy functions
(for the concatMap a map.join is enough).

```d
import std.stdio, std.algorithm, std.range, std.typecons;

mixin template InitsTails(T) {
    T[] data;
    size_t pos;
    @property bool empty() pure nothrow @nogc {
        return pos > data.length;
    }
    void popFront() pure nothrow @nogc { pos++; }
}

struct Inits(T) {
    mixin InitsTails!T;
    @property T[] front() pure nothrow @nogc { return data[0 .. pos]; }
}

auto inits(T)(T[] seq) pure nothrow @nogc { return seq.Inits!T; }

struct Tails(T) {
    mixin InitsTails!T;
    @property T[] front() pure nothrow @nogc { return data[pos .. $]; }
}

auto tails(T)(T[] seq) pure nothrow @nogc { return seq.Tails!T; }

T[] maxSubseq(T)(T[] seq) pure nothrow /*@nogc*/ {
    //return seq.tails.map!inits.joiner.reduce!(max!sum);
    return seq.tails.map!inits.join.minPos!q{ a.sum > b.sum }[0];
}

void main() {
    [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1].maxSubseq.writeln;
    [-1, -2, -3, -5, -6, -2, -1, -4, -4, -2, -1].maxSubseq.writeln;
}
```



## EchoLisp


```scheme

(lib 'struct)
(struct result (score starter))

;; the score of  i in sequence ( .. i j ...)  is max (i , i + score (j))
;; to compute score of (a b .. x y z) :
;; start with score(z) and compute scores of y , z , ..c, b , a.
;; this is O(n)

;; return value of sub-sequence
(define (max-max L into: result)
(define value
	(if
	(empty? L) -Infinity
	(max (first L)  (+ (first L) (max-max (cdr L) result )))))

    (when (> value (result-score result))
		(set-result-score! result value) ;; remember best score
		(set-result-starter! result L))  ;; and its location
	value)

;; return (best-score (best sequence))
(define (max-seq L)
	(define best (result -Infinity null))
	(max-max L into: best)
	(define score (result-score best))

	(list score
	(for/list (( n (result-starter best)))
			   #:break (zero? score)
			   (set! score (- score n))
			   n)))

(define L '(-1 -2 3 5 6 -2 -1 4 -4 2 -1))
(max-seq L)
    → (15 (3 5 6 -2 -1 4))

```



## Elixir

{{trans|Ruby}}

### Brute Force:


```elixir
defmodule Greatest do
  def subseq_sum(list) do
    limit = length(list) - 1
    ij = for i <- 0..limit, j <- i..limit, do: {i,j}
    Enum.reduce(ij, {0, []}, fn {i,j},{max, subseq} ->
      slice = Enum.slice(list, i..j)
      sum = Enum.sum(slice)
      if sum > max, do: {sum, slice}, else: {max, subseq}
    end)
  end
end
```


'''Test:'''

```elixir
data = [ [1, 2, 3, 4, 5, -8, -9, -20, 40, 25, -5],
         [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1],
         [-1, -2, -3, -4, -5],
         [] ]
Enum.each(data, fn input ->
  IO.puts "\nInput seq: #{inspect input}"
  {max, subseq} = Greatest.subseq_sum(input)
  IO.puts "  Max sum: #{max}\n   Subseq: #{inspect subseq}"
end)
```


{{out}}

```txt

Input seq: [1, 2, 3, 4, 5, -8, -9, -20, 40, 25, -5]
  Max sum: 65
   Subseq: [40, 25]

Input seq: [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1]
  Max sum: 15
   Subseq: [3, 5, 6, -2, -1, 4]

Input seq: [-1, -2, -3, -4, -5]
  Max sum: 0
   Subseq: []

Input seq: []
  Max sum: 0
   Subseq: []

```



### Linear Time Version:


```elixir
defmodule Greatest do
  def subseq_sum(list) do
    list_i = Enum.with_index(list)
    acc = {0, 0, length(list), 0, 0}
    {_,max,first,last,_} = Enum.reduce(list_i, acc, fn {elm,i},{curr,max,first,last,curr_first} ->
      if curr < 0 do
        if elm > max, do: {elm, elm, i,     i,    curr_first},
                    else: {elm, max, first, last, curr_first}
      else
        cur2 = curr + elm
        if cur2 > max, do: {cur2, cur2, curr_first, i, curr_first},
                     else: {cur2, max,  first,   last, curr_first}
      end
    end)
    {max, Enum.slice(list, first..last)}
  end
end
```

Output is the same above.


## ERRE

<lang>
PROGRAM MAX_SUM

DIM A%[11],B%[10],C%[4]

!$DYNAMIC
DIM P%[0]

PROCEDURE MAX_SUBSEQUENCE(P%[],N%->A$)
      LOCAL A%,B%,I%,J%,M%,S%
      A%=1
      FOR I%=0 TO N% DO
        S%=0
        FOR J%=I% TO N% DO
          S%+=P%[J%]
          IF S%>M% THEN
            M%=S%
            A%=I%
            B%=J%
          END IF
        END FOR
      END FOR
      IF A%>B% THEN A$="[]" EXIT PROCEDURE END IF
      A$="["
      FOR I%=A% TO B% DO
        A$+=STR$(P%[I%])+","
      END FOR
      A$=LEFT$(A$,LEN(A$)-1)+"]"
END PROCEDURE

PROCEDURE SHOW_ARRAY(P%[],N%->A$)
      LOCAL I%
      A$="["
      FOR I%=0 TO N% DO
        A$+=STR$(P%[I%])+","
      END FOR
      A$=LEFT$(A$,LEN(A$)-1)+"]"
END PROCEDURE

BEGIN

   A%[]=(0,1,2,-3,3,-1,0,-4,0,-1,-4,2)
   N%=UBOUND(A%,1)
   !$DIM P%[N%]
   SHOW_ARRAY(A%[],N%->A$)
   PRINT(A$;" -> ";)
   MAX_SUBSEQUENCE(A%[],N%->A$)
   PRINT(A$)
   !$ERASE P%

   B%[]=(-1,-2,3,5,6,-2,-1,4,-4,2,-1)
   N%=UBOUND(B%,1)
   !$DIM P%[N%]
   SHOW_ARRAY(B%[],N%->A$)
   PRINT(A$;" -> ";)
   MAX_SUBSEQUENCE(B%[],N%->A$)
   PRINT(A$)
   !$ERASE P%

   C%[]=(-1,-2,-3,-4,-5)
   N%=UBOUND(C%,1)
   !$DIM P%[N%]
   SHOW_ARRAY(C%[],N%->A$)
   PRINT(A$;" -> ";)
   MAX_SUBSEQUENCE(C%[],N%->A$)
   PRINT(A$)
   !$ERASE P%
END PROGRAM

```



## Euler Math Toolbox


The following recursive system seems to have a run time of O(n), but it needs some copying, so the run time is really O(n^2).


```Euler Math Toolbox

>function %maxsubs (v,n) ...
$if n==1 then
$  if (v[1]<0) then return {zeros(1,0),zeros(1,0)}
$  else return {v,v};
$  endif;
$endif;
${v1,v2}=%maxsubs(v[1:n-1],n-1);
$m1=sum(v1); m2=sum(v2); m3=m2+v[n];
$if m3>0 then v3=v2|v[n]; else v3=zeros(1,0); endif;
$if m3>m1 then return {v2|v[n],v3};
$else return {v1,v3};
$endif;
$endfunction
>function maxsubs (v) ...
${v1,v2}=%maxsubs(v,cols(v));
$return v1
$endfunction
>maxsubs([0, 1, 2, -3, 3, -1, 0, -4, 0, -1, -4])
 [ 0  1  2 ]
>maxsubs([-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1])
 [ 3  5  6  -2  -1  4 ]
>maxsubs([-1, -2, -3, -4, -5])
 Empty matrix of size 1x0

```


Here is a brute force method producing and testing all sums. The runtime is O(n^3).


```Euler Math Toolbox

>function maxsubsbrute (v) ...
$  n=cols(v);
$  A=zeros(n*(n-1),n);
$  k=1;
$  for i=1 to n-1;
$    for j=i to n;
$      A[k,i:j]=1;
$      k=k+1;
$    end;
$  end;
$  k1=extrema((A.v')')[4];
$  return v[nonzeros(A[k1])];
$  endfunction
>maxsubsbrute([0, 1, 2, -3, 3, -1, 0, -4, 0, -1, -4])
 [ 0  1  2 ]
>maxsubsbrute([-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1])
 [ 3  5  6  -2  -1  4 ]
>maxsubsbrute([-1, -2, -3, -4, -5])
 Empty matrix of size 1x0

```


To see, if everything works, the following tests on 10 million random sequences.


```Euler Math Toolbox

>function test ...
$  loop 1 to 10000000
$    v=intrandom(1,intrandom(6)+6,20)-10;
$    if sum(maxsubs(v))!=sum(maxsubsbrute(v)) then
$      v, error("Found a wrong test example");
$    endif;
$  endfunction
>test

```



## E

This implementation tests every combination, but it first examines the sequence to reduce the number of combinations tried: We need not consider beginning the subsequence at any point which is not the beginning, or a change from negative to positive. We need not consider ending the subsequence at any point which is not the end, or a change from positive to negative. (Zero is moot and treated as negative.)

This algorithm is therefore <math>O(nm^2)</math> where <math>n</math> is the size of the sequence and <math>m</math> is the number of sign changes in the sequence. [[User:Kevin Reid|I]] think it could be improved to <math>O(n + m^2)</math> by recording the positive and negative intervals' sums during the initial pass and accumulating the sum of those sums in the inner for loop.

<code>maxSubseq</code> returns both the maximum sum found, and the interval of indexes which produces it.


```e
pragma.enable("accumulator")

def maxSubseq(seq) {
  def size := seq.size()

  # Collect all intervals of indexes whose values are positive
  def intervals := {
    var intervals := []
    var first := 0
    while (first < size) {
      var next := first
      def seeing := seq[first] > 0
      while (next < size && (seq[next] > 0) == seeing) {
        next += 1
      }
      if (seeing) { # record every positive interval
        intervals with= first..!next
      }
      first := next
    }
    intervals
  }

  # For recording the best result found
  var maxValue := 0
  var maxInterval := 0..!0

  # Try all subsequences beginning and ending with such intervals.
  for firstIntervalIx => firstInterval in intervals {
    for lastInterval in intervals(firstIntervalIx) {
      def interval :=
        (firstInterval.getOptStart())..!(lastInterval.getOptBound())
      def value :=
        accum 0 for i in interval { _ + seq[i] }
      if (value > maxValue) {
        maxValue := value
        maxInterval := interval
      }
    }
  }

  return ["value" => maxValue,
          "indexes" => maxInterval]
}
```



```e
def seq := [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1]
def [=> value, => indexes] := maxSubseq(seq)
println(`$\
Sequence: $seq
Maximum subsequence sum: $value
Indexes: ${indexes.getOptStart()}..${indexes.getOptBound().previous()}
Subsequence: ${seq(indexes.getOptStart(), indexes.getOptBound())}
`)
```



## Euphoria


```euphoria
function maxSubseq(sequence s)
    integer sum, maxsum, first, last
    maxsum = 0
    first = 1
    last = 0
    for i = 1 to length(s) do
        sum = 0
        for j = i to length(s) do
            sum += s[j]
            if sum > maxsum then
                maxsum = sum
                first = i
                last = j
            end if
        end for
    end for
    return s[first..last]
end function

? maxSubseq({-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1})
? maxSubseq({})
? maxSubseq({-1, -5, -3})
```


{{out}}

```txt
{3,5,6,-2,-1,4}
{}
{}
```



=={{header|F_Sharp|F#}}==

```fsharp
let maxsubseq s =
    let (_, _, maxsum, maxseq) =
        List.fold (fun (sum, seq, maxsum, maxseq) x ->
            let (sum, seq) = (sum + x, x :: seq)
            if sum < 0 then (0, [], maxsum, maxseq)
            else if sum > maxsum then (sum, seq, sum, seq)
            else (sum, seq, maxsum, maxseq))
            (0, [], 0, []) s
    List.rev maxseq

printfn "%A" (maxsubseq  [-1 ; -2 ; 3 ; 5 ; 6 ; -2 ; -1 ; 4; -4 ; 2 ; -1])
```

{{out}}

```txt
[3; 5; 6; -2; -1; 4]
```



## Factor


```factor
USING: kernel locals math math.order sequences ;

:: max-with-index ( elt0 ind0 elt1 ind1 -- elt ind )
elt0 elt1 <  [ elt1 ind1 ] [ elt0 ind0 ] if ;
: last-of-max ( accseq -- ind ) -1 swap -1 [ max-with-index ] reduce-index nip ;

: max-subseq ( seq -- subseq )
dup 0 [ + 0 max ] accumulate swap suffix last-of-max head
dup 0 [ + ] accumulate swap suffix [ neg ] map last-of-max tail ;
```


```factor
( scratchpad ) { -1 -2 3 5 6 -2 -1 4 -4 2 -1 } max-subseq  dup sum  swap . .
{ 3 5 6 -2 -1 4 }
15
```



## Forth


```forth
2variable best
variable best-sum

: sum ( array len -- sum )
  0 -rot cells over + swap do i @ + cell +loop ;

: max-sub ( array len -- sub len )
  over 0 best 2!  0 best-sum !
  dup 1 do                  \ foreach length
    2dup i - 1+ cells over + swap do   \ foreach start
      i j sum
      dup best-sum @ > if
        best-sum !
        i j best 2!
      else drop then
    cell +loop
  loop
  2drop best 2@ ;

: .array  ." [" dup 0 ?do over i cells + @ . loop ." ] = " sum . ;

create test  -1 , -2 , 3 , 5 , 6 , -2 , -1 , 4 , -4 , 2 , -1 ,
create test2 -1 , -2 , 3 , 5 , 6 , -2 , -1 , 4 , -4 , 2 , 99 ,
```

{{out}}

```forth
test 11 max-sub .array [3 5 6 -2 -1 4 ] = 15 ok
test2 11 max-sub .array [3 5 6 -2 -1 4 -4 2 99 ] = 112 ok
```



## Fortran

{{works with|Fortran|95 and later}}

```fortran
program MaxSubSeq
  implicit none

  integer, parameter :: an = 11
  integer, dimension(an) :: a = (/ -1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1 /)

  integer, dimension(an,an) :: mix
  integer :: i, j
  integer, dimension(2) :: m

  forall(i=1:an,j=1:an) mix(i,j) = sum(a(i:j))
  m = maxloc(mix)
  ! a(m(1):m(2)) is the wanted subsequence
  print *, a(m(1):m(2))

end program MaxSubSeq
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Dim As Integer seq(10) = {-1 , -2 , 3 , 5 , 6 , -2 , -1 , 4 , -4 , 2 , -1}
Dim As Integer i, j, sum, maxSum, first, last

maxSum = 0

For i = LBound(seq) To UBound(seq)
  sum = 0
  For j = i To UBound(seq)
    ' only proper sub-sequences are considered
    If i = LBound(seq) AndAlso j = UBound(seq) Then Exit For
    sum += seq(j)
    If sum > maxSum Then
      maxSum = sum
      first = i
      last = j
    End If
  Next j
Next i

If maxSum > 0 Then
  Print "Maximum subsequence is from indices"; first; " to"; last
  Print "Elements are : ";
  For i = first To last
    Print seq(i); " ";
  Next
  Print
  Print "Sum is"; maxSum
Else
  Print "Maximum subsequence is the empty sequence which has a sum of 0"
End If

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Maximum subsequence is from indices 2 to 7
Elements are :  3  5  6 -2 -1  4
Sum is 15

```



## Go


```go
package main

import "fmt"

func gss(s []int) ([]int, int) {
    var best, start, end, sum, sumStart int
    for i, x := range s {
        sum += x
        switch {
        case sum > best:
            best = sum
            start = sumStart
            end = i + 1
        case sum < 0:
            sum = 0
            sumStart = i + 1
        }
    }
    return s[start:end], best
}

var testCases = [][]int{
    {-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1},
    {-1, 1, 2, -5, -6},
    {},
    {-1, -2, -1},
}

func main() {
    for _, c := range testCases {
        fmt.Println("Input:  ", c)
        subSeq, sum := gss(c)
        fmt.Println("Sub seq:", subSeq)
        fmt.Println("Sum:    ", sum, "\n")
    }
}
```

{{out}}

```txt

Input:   [-1 -2 3 5 6 -2 -1 4 -4 2 -1]
Sub seq: [3 5 6 -2 -1 4]
Sum:     15

Input:   [-1 1 2 -5 -6]
Sub seq: [1 2]
Sum:     3

Input:   []
Sub seq: []
Sum:     0

Input:   [-1 -2 -1]
Sub seq: []
Sum:     0

```



## Haskell

Naive approach which tests all possible subsequences, as in a few of the other examples. For fun, this is in point-free style and doesn't use loops:

```haskell
import Data.List (inits, tails, maximumBy)
import Data.Ord (comparing)

subseqs :: [a] -> [[a]]
subseqs = concatMap inits . tails

maxsubseq :: (Ord a, Num a) => [a] -> [a]
maxsubseq = maximumBy (comparing sum) . subseqs

main = print $ maxsubseq [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1]
```

Secondly, the linear time constant space approach:

```haskell
maxSubseq :: [Int] -> (Int, [Int])
maxSubseq =
  let go x ((h1, h2), sofar) =
        ((,) <*> max sofar) (max (0, []) (h1 + x, x : h2))
  in snd . foldr go ((0, []), (0, []))

main :: IO ()
main = print $ maxSubseq [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1]
```

{{Out}}

```txt
(15,[3,5,6,-2,-1,4])
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
L1 := [-1,-2,3,5,6,-2,-1,4,-4,2,-1]     # sample list
L  := [-1,1,2,3,4,-11]|||L1             # prepend a local maximum into the mix
write(ximage(maxsubseq(L)))
end

link ximage      # to show lists

procedure maxsubseq(L)                    #: return the subsequence of L with maximum positive sum
local i,maxglobal,maxglobalI,maxlocal,maxlocalI

maxglobal := maxlocal := 0                                           # global and local maxima

every i := 1 to *L do  {
   if (maxlocal := max(maxlocal +L[i],0)) > 0 then
      if /maxlocalI then maxlocalI := [i,i] else maxlocalI[2] := i   # local maxima subscripts
   else maxlocalI := &null                                           # reset subsequence
   if maxglobal <:= maxlocal then                                    # global maxima
      maxglobalI := copy(maxlocalI)
   }
return L[(\maxglobalI)[1]:maxglobalI[2]] | []                        # return sub-sequence or empty list
end
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Subseq.bas"
110 RANDOMIZE
120 NUMERIC A(1 TO 15)
130 PRINT "Sequence:"
140 FOR I=LBOUND(A) TO UBOUND(A)
150   LET A(I)=RND(11)-6
160   PRINT A(I);
170 NEXT
180 LET MAXSUM,ST=0:LET EN=-1
190 FOR I=LBOUND(A) TO UBOUND(A)
200   LET SUM=0
210   FOR J=I TO UBOUND(A)
220     LET SUM=SUM+A(J)
230     IF SUM>MAXSUM THEN LET MAXSUM=SUM:LET ST=I:LET EN=J
240   NEXT
250 NEXT
260 PRINT :PRINT "SubSequence with greatest sum:"
270 IF ST>0 THEN PRINT TAB(ST*3-2);
280 FOR I=ST TO EN
290   PRINT A(I);
300 NEXT
310 PRINT :PRINT "Sum:";MAXSUM
```



## J


```j
maxss=: monad define
 AS =. 0,; <:/~@i.&.> #\y
 MX =. (= >./) AS +/ . * y
 y #~ {. MX#AS
)
```


<tt>y</tt> is the input vector, an integer list.

<tt>AS</tt>  means "all sub-sequences." It is a binary table.
Each row indicates one sub-sequence; the count of columns equals the length of the input.

<tt>MX</tt>  means "maxima." It is the first location in <tt>AS</tt> where the corresponding sum is largest.

Totals for the subsequences are calculated by the phrase '<tt>AS +/ . * y</tt>' which is the inner product of the binary table with the input vector.

All solutions are found but only one is returned, to fit the output requirement.
If zero is the maximal sum the empty array is always returned, although sub-sequences of positive length (comprised of zeros) might be more interesting.

Example use:

```j
   maxss _1 _2 3 5 6 _2 _1 4 _4 2 _1
3 5 6 _2 _1 4
```


Note: if we just want the sum of the maximum subsequence,
and are not concerned with the subsequence itself, we can use:


```j>maxs=: [:
./(0>.+)/\.
```


Example use:

```j
  maxs _1 _2 3 5 6 _2 _1 4 _4 2 _1
15
```


This suggests a variant:

```j
maxSS=:monad define
  sums=: (0>.+)/\. y
  start=: sums i. max=: >./ sums
  max (] {.~ #@] |&>: (= +/\) i. 1:) y}.~start
)
```

or

```j
maxSS2=:monad define
  start=. (i. >./) (0>.+)/\. y
  ({.~ # |&>: [: (i.>./@,&0) +/\)  y}.~start
)
```


These variants are considerably faster than the first implementation, on long sequences.


## Java

{{works with|Java|1.5+}}
This is not a particularly efficient solution, but it gets the job done.

The method <tt>nextChoices</tt> was modified from an [http://www.cs.rit.edu/~vcss233/Labs/newlab02/index.html RIT CS lab].

```java
import java.util.Scanner;
import java.util.ArrayList;

public class Sub{
    private static int[] indices;

    public static void main(String[] args){
        ArrayList<Long> array= new ArrayList<Long>(); //the main set
        Scanner in = new Scanner(System.in);
        while(in.hasNextLong()) array.add(in.nextLong());
        long highSum= Long.MIN_VALUE;//start the sum at the lowest possible value
        ArrayList<Long> highSet= new ArrayList<Long>();
        //loop through all possible subarray sizes including 0
        for(int subSize= 0;subSize<= array.size();subSize++){
            indices= new int[subSize];
            for(int i= 0;i< subSize;i++) indices[i]= i;
            do{
                long sum= 0;//this subarray sum variable
                ArrayList<Long> temp= new ArrayList<Long>();//this subarray
                //sum it and save it
                for(long index:indices) {sum+= array.get(index); temp.add(array.get(index));}
                if(sum > highSum){//if we found a higher sum
                    highSet= temp;    //keep track of it
                    highSum= sum;
                }
            }while(nextIndices(array));//while we haven't tested all subarrays
        }
        System.out.println("Sum: " + highSum + "\nSet: " +
        		highSet);
    }
    /**
     * Computes the next set of choices from the previous. The
     * algorithm tries to increment the index of the final choice
     * first. Should that fail (index goes out of bounds), it
     * tries to increment the next-to-the-last index, and resets
     * the last index to one more than the next-to-the-last.
     * Should this fail the algorithm keeps starting at an earlier
     * choice until it runs off the start of the choice list without
     * Finding a legal set of indices for all the choices.
     *
     * @return true unless all choice sets have been exhausted.
     * @author James Heliotis
     */

    private static boolean nextIndices(ArrayList<Long> a) {
        for(int i= indices.length-1;i >= 0;--i){
            indices[i]++;
            for(int j=i+1;j < indices.length;++j){
                indices[j]= indices[j - 1] + 1;//reset the last failed try
            }
            if(indices[indices.length - 1] < a.size()){//if this try went out of bounds
                return true;
            }
        }
        return false;
    }
}
```


This one runs in linear time, and isn't generalized.

```java
private static int BiggestSubsum(int[] t) {
    int sum = 0;
    int maxsum = 0;

    for (int i : t) {
        sum += i;
        if (sum < 0)
            sum = 0;
        maxsum = sum > maxsum ? sum : maxsum;
    }
    return maxsum;
}
```



## JavaScript


### Imperative

Simple brute force approach.

```javascript
function MaximumSubsequence(population) {
    var maxValue = 0;
    var subsequence = [];

    for (var i = 0, len = population.length; i < len; i++) {
        for (var j = i; j <= len; j++) {
            var subsequence = population.slice(i, j);
            var value = sumValues(subsequence);
            if (value > maxValue) {
                maxValue = value;
                greatest = subsequence;
            };
        }
    }

    return greatest;
}

function sumValues(arr) {
    var result = 0;
    for (var i = 0, len = arr.length; i < len; i++) {
        result += arr[i];
    }
    return result;
}
```



### Functional

{{Trans|Haskell}}
Linear approach, deriving both list and sum in a single accumulating fold.

```JavaScript
(() => {

    // maxSubseq :: [Int] -> (Int, [Int])
    const maxSubseq = xs =>
        snd(xs.reduce((tpl, x) => {
            const [m1, m2] = Array.from(fst(tpl)),
                high = max(
                    Tuple(0, []),
                    Tuple(m1 + x, m2.concat(x))
                );
            return Tuple(high, max(snd(tpl), high));
        }, Tuple(Tuple(0, []), Tuple(0, []))));


    // TEST -----------------------------------------------
    // main :: IO ()
    const main = () => {
        const mx = maxSubseq([-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1]);
        showLog(snd(mx), fst(mx))
    }
    // [3,5,6,-2,-1,4] -> 15


    // GENERIC FUNCTIONS ----------------------------------

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // gt :: Ord a => a -> a -> Bool
    const gt = (x, y) =>
        'Tuple' === x.type ? (
            x[0] > y[0]
        ) : (x > y);

    // max :: Ord a => a -> a -> a
    const max = (a, b) => gt(b, a) ? b : a;

    // showLog :: a -> IO ()
    const showLog = (...args) =>
        console.log(
            args
            .map(JSON.stringify)
            .join(' -> ')
        );

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
[3,5,6,-2,-1,4] -> 15
```



## jq

{{works with|jq|1.4}}

This is the same linear-time algorithm as used in the [[#Ruby]] subsection on this page.

```jq
def subarray_sum:
  . as $arr
  | reduce range(0; length) as $i
      ( {"first": length, "last": 0, "curr": 0, "curr_first": 0, "max": 0};
        $arr[$i] as $e
        | (.curr + $e) as $curr
        | . + (if $e > $curr then {"curr": $e, "curr_first": $i} else {"curr": $curr} end)
        | if .curr > .max then . + {"max": $curr, "first": .curr_first, "last": $i}
          else .
          end)
  | [ .max, $arr[ .first : (1 + .last)] ];
```

'''Example''':

```jq
[1, 2, 3, 4, 5, -8, -9, -20, 40, 25, -5] | subarray_sum
```

{{out}}

```sh
$ jq -c -n -f Greatest_subsequential_sum.jq
[65,[40,25]]
```



## Jsish

From Javascript entry.

```javascript
/* Greatest Subsequential Sum, in Jsish */
function sumValues(arr) {
    var result = 0;
    for (var i = 0, len = arr.length; i < len; i++) result += arr[i];
    return result;
}

function greatestSubsequentialSum(population:array):array {
    var maxValue = (population[0]) ? population[0] : 0;
    var subsequence = [], greatest = [];

    for (var i = 0, len = population.length; i < len; i++) {
        for (var j = i; j < len; j++) {
            subsequence = population.slice(i, j);
            var value = sumValues(subsequence);
            if (value > maxValue) {
                maxValue = value;
                greatest = subsequence;
            };
        }
    }

    return [maxValue, greatest];
}

if (Interp.conf('unitTest')) {
    var gss = [-1,-2,3,5,6,-2,-1,4,-4,2,-1];
;    gss;
;    greatestSubsequentialSum(gss);
}

/*
=!EXPECTSTART!=
gss ==> [ -1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1 ]
greatestSubsequentialSum(gss) ==> [ 15, [ 3, 5, 6, -2, -1, 4 ] ]
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish --U greatestSubsequentialSum.jsi
gss ==> [ -1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1 ]
greatestSubsequentialSum(gss) ==> [ 15, [ 3, 5, 6, -2, -1, 4 ] ]
```



## Julia

{{works with|Julia|0.6}}


```julia
function gss(arr::Vector{<:Number})
    smax = hmax = tmax = 0
    for head in eachindex(arr), tail in head:length(arr)
        s = sum(arr[head:tail])
        if s > smax
            smax = s
            hmax, tmax = head, tail
        end
    end
    return arr[hmax:tmax]
end

arr = [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1]
subseq = gss(arr)
s = sum(subseq)

println("Greatest subsequential sum of $arr:\n → $subseq with sum $s")
```


{{out}}

```txt
Greatest subsequential sum of [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1]:
 → [3, 5, 6, -2, -1, 4] with sum 15
```



## Kotlin


```scala
// version 1.1

fun gss(seq: IntArray): Triple<Int, Int, Int> {
    if (seq.isEmpty()) throw IllegalArgumentException("Array cannot be empty")
    var sum: Int
    var maxSum = seq[0]
    var first = 0
    var last = 0
    for (i in 1 until seq.size) {
        sum = 0
        for (j in i until seq.size) {
            sum += seq[j]
            if (sum > maxSum) {
                maxSum = sum
                first = i
                last = j
            }
        }
    }
    return Triple(maxSum, first, last)
}

fun main(args: Array<String>) {
  val seq = intArrayOf(-1 , -2 , 3 , 5 , 6 , -2 , -1 , 4 , -4 , 2 , -1)
  val(maxSum, first, last) = gss(seq)
  if (maxSum > 0) {
      println("Maximum subsequence is from indices $first to $last")
      print("Elements are : ")
      for (i in first .. last) print("${seq[i]} ")
      println("\nSum is $maxSum")
  }
  else
      println("Maximum subsequence is the empty sequence which has a sum of 0")
}
```


{{out}}

```txt

Maximum subsequence is from indices 2 to 7
Elements are : 3 5 6 -2 -1 4
Sum is 15

```



## Liberty BASIC


```lb

'Greatest_subsequential_sum

N= 20   'number of elements

randomize 0.52
for K = 1 to 5
    a$ = using("##",int(rnd(1)*12)-5)
    for i=2 to N
        a$ = a$ +","+using("##",int(rnd(1)*12)-5)
    next
    call maxsumseq a$
next K

sub maxsumseq a$
    sum=0
    maxsum=0
    sumStart=1
    end1 =0
    start1 =1

    token$="*"
    i=0
    while 1
        i=i+1
        token$=word$(a$, i, ",")
        if token$ ="" then exit while    'end of stream
        x=val(token$)
        sum=sum+x
        if maxsum<sum then
             maxsum = sum
             start1 = sumStart
             end1 = i
        else
            if sum <0 then
                sum=0
                sumStart = i+1
            end if
        end if
    wend
    print "sequence: ";a$
    print "          ";
    for i=1 to start1-1:   print "   "; :next
    for i= start1 to end1: print "---"; :next
    print
    if end1 >0 then
        print "Maximum sum subsequense: ";start1 ;" to "; end1
    else
        print "Maximum sum subsequense: is empty"
    end if
    print "Maximum sum ";maxsum
    print
end sub

```


## Lua


```lua
function sumt(t, start, last) return start <= last and t[start] + sumt(t, start+1, last) or 0 end
function maxsub(ary, idx)
  local idx = idx or 1
  if not ary[idx] then return {} end
  local maxsum, last = 0, idx
  for i = idx, #ary do
    if sumt(ary, idx, i) > maxsum then maxsum, last = sumt(ary, idx, i), i end
  end
  local v = maxsub(ary, idx + 1)
  if maxsum < sumt(v, 1, #v) then return v end
  local ret = {}
  for i = idx, last do ret[#ret+1] = ary[i] end
  return ret
end
```



## M4


```M4
divert(-1)
define(`setrange',`ifelse(`$3',`',$2,`define($1[$2],$3)`'setrange($1,
   incr($2),shift(shift(shift($@))))')')
define(`asize',decr(setrange(`a',1,-1,-2,3,5,6,-2,-1,4,-4,2,-1)))
define(`get',`defn(`$1[$2]')')
define(`for',
   `ifelse($#,0,``$0'',
   `ifelse(eval($2<=$3),1,
   `pushdef(`$1',$2)$4`'popdef(`$1')$0(`$1',incr($2),$3,`$4')')')')
define(`maxsum',0)
for(`x',1,asize,
   `define(`sum',0)`'for(`y',x,asize,
      `define(`sum',eval(sum+get(`a',y)))`'ifelse(eval(sum>maxsum),1,
         `define(`maxsum',sum)`'define(`xmax',x)`'define(`ymax',y)')')')
divert
for(`x',xmax,ymax,`get(`a',x) ')
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

### Method 1

First we define 2 functions, one that gives all possibles subsequences (as a list of lists of indices) for a particular length. Then another extract those indices adds them up and looks for the largest sum.

```Mathematica
Sequences[m_]:=Prepend[Flatten[Table[Partition[Range[m],n,1],{n,m}],1],{}]
MaximumSubsequence[x_List]:=Module[{sums},
 sums={x[[#]],Total[x[[#]]]}&/@Sequences[Length[x]];
 First[First[sums[[Ordering[sums,-1,#1[[2]]<#2[[2]]&]]]]]
]
```



### Method 2


```Mathematica
MaximumSubsequence[x_List]:=Last@SortBy[Flatten[Table[x[[a;;b]], {b,Length[x]}, {a,b}],1],Total]
```



### Examples


```Mathematica
MaximumSubsequence[{-1,-2,3,5,6,-2,-1,4,-4,2,-1}]
MaximumSubsequence[{2,4,5}]
MaximumSubsequence[{2,-4,3}]
MaximumSubsequence[{4}]
MaximumSubsequence[{}]
```

gives back:

```txt
{3,5,6,-2,-1,4}
{2,4,5}
{3}
{4}
{}
```



## Mathprog

see [[wp:Special_ordered_set]]. Lmin specifies the minimum length of the required subsequence, and Lmax the maximum.
<lang>
/*Special ordered set of type N

  Nigel_Galloway
  January 26th, 2012
*/

param Lmax;
param Lmin;
set SOS;
param Sx{SOS};
var db{Lmin..Lmax,SOS}, binary;

maximize s : sum{q in (Lmin..Lmax),t in (0..q-1), z in SOS: z > (q-1)} Sx[z-t]*db[q,z];
sos1 : sum{t in (Lmin..Lmax),z in SOS: z > (t-1)} db[t,z] = 1;
solve;

for{t in (Lmin..Lmax),z in SOS: db[t,z] == 1} {
  printf "\nA sub-sequence of length %d sums to %f:\n", t,s;
  printf{q in (z-t+1)..z} "  %f", Sx[q];
}
printf "\n\n";

data;
param Lmin := 1;
param Lmax := 6;
param:
SOS:  Sx :=
 1     7
 2     4
 3   -11
 4     6
 5     3
 6     1
;

end;

```


produces:

<lang>
GLPSOL: GLPK LP/MIP Solver, v4.47
Parameter(s) specified in the command line:
 --math GSS.mod
Reading model section from GSS.mod...
Reading data section from GSS.mod...
38 lines were read
Generating s...
Generating sos1...
Model has been successfully generated
GLPK Integer Optimizer, v4.47
2 rows, 21 columns, 41 non-zeros
21 integer variables, all of which are binary
Preprocessing...
1 row, 21 columns, 21 non-zeros
21 integer variables, all of which are binary
Scaling...
 A: min|aij| = 1.000e+000  max|aij| = 1.000e+000  ratio = 1.000e+000
Problem data seem to be well scaled
Constructing initial basis...
Size of triangular part = 1
Solving LP relaxation...
GLPK Simplex Optimizer, v4.47
1 row, 21 columns, 21 non-zeros
*     0: obj =  1.000000000e+001  infeas = 0.000e+000 (0)
*     1: obj =  1.100000000e+001  infeas = 0.000e+000 (0)
OPTIMAL SOLUTION FOUND
Integer optimization begins...
+     1: mip =     not found yet <=              +inf        (1; 0)
+     1: >>>>>  1.100000000e+001 <=  1.100000000e+001   0.0% (1; 0)
+     1: mip =  1.100000000e+001 <=     tree is empty   0.0% (0; 1)
INTEGER OPTIMAL SOLUTION FOUND
Time used:   0.0 secs
Memory used: 0.1 Mb (135491 bytes)

A sub-sequence of length 2 sums to 11.000000:
  7.000000  4.000000

Model has been successfully processed


```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function [S,GS]=gss(a)
% Greatest subsequential sum
a =[0;a(:);0]';
ix1 = find(a(2:end) >0 & a(1:end-1) <= 0);
ix2 = find(a(2:end)<=0 & a(1:end-1) > 0);
K = 0;
S = 0;
for k = 1:length(ix1)
	s = sum(a(ix1(k)+1:ix2(k)));
	if (s>S)
		S=s; K=k;
	end;
end;
GS = a(ix1(K)+1:ix2(K));

```


Usage:

```txt

  octave:12> [S,GS]=gss([0, 1, 2, -3, 3, -1, 0, -4, 0, -1, -4, 2])
  S =  3
  GS =
   1   2

```



## NetRexx


```NetRexx
/* REXX ***************************************************************
* 10.08.2012 Walter Pachl Pascal algorithm -> Rexx -> NetRexx
**********************************************************************/
  s=' -1 -2  3  5  6 -2 -1  4 -4  2 -1'
  maxSum   = 0
  seqStart = 0
  seqEnd   = -1
  Loop i = 1 To s.words()
    seqSum = 0
    Loop j = i to s.words()
      seqSum = seqSum + s.word(j)
      if seqSum > maxSum then Do
        maxSum   = seqSum
        seqStart = i
        seqEnd   = j
        end
      end
    end
  Say 'Sequence:'
  Say s
  Say 'Subsequence with greatest sum: '
  If seqend<seqstart Then
    Say 'empty'
  Else Do
    ol='   '.copies(seqStart-1)
    Loop i = seqStart to seqEnd
      w=s.word(i)
      ol=ol||w.right(3)
      End
    Say ol
    Say 'Sum:' maxSum
    End
```

Output: the same as for Rexx


## Nim


```nim
proc maxsum(s: openArray[int]): int =
  var maxendinghere = 0
  for x in s:
    maxendinghere = max(maxendinghere + x, 0)
    result = max(result, maxendinghere)

echo maxsum(@[-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1])
```


=={{header|Oberon-2}}==
Works with oo2c Version 2

```oberon2

MODULE GreatestSubsequentialSum;
IMPORT
  Out,
  Err,
  IntStr,
  ProgramArgs,
  TextRider;
TYPE
  IntSeq= POINTER TO ARRAY OF LONGINT;

PROCEDURE ShowUsage();
BEGIN
  Out.String("Usage: GreatestSubsequentialSum {int}+");Out.Ln
END ShowUsage;

PROCEDURE Gss(iseq: IntSeq; VAR start, end, maxsum: LONGINT);
VAR
  i, j, sum: LONGINT;
BEGIN
  i := 0; maxsum := 0; start := 0; end := -1;
  WHILE (i < LEN(iseq^)) DO
    sum := 0; j := i;
    WHILE (j < LEN(iseq^) - 1) DO
      INC(sum,iseq[j]);
      IF sum > maxsum THEN
        maxsum := sum;
        start := i;
        end := j
      END;
      INC(j)
    END;
    INC(i)
  END
END Gss;


PROCEDURE GetParams():IntSeq;
VAR
  reader: TextRider.Reader;
  iseq: IntSeq;
  param: ARRAY 32 OF CHAR;
  argc,i: LONGINT;
  res: SHORTINT;
BEGIN
  iseq := NIL;
  reader := TextRider.ConnectReader(ProgramArgs.args);
  IF reader # NIL THEN
    argc := ProgramArgs.args.ArgNumber();
    IF argc < 1 THEN
      Err.String("There is no enough arguments.");Err.Ln;
      ShowUsage;
      HALT(0)
    END;

    reader.ReadLn; (* Skips program name *)

    NEW(iseq,argc);
    FOR i := 0 TO argc - 1 DO
      reader.ReadLine(param);
      IntStr.StrToInt(param,iseq[i],res);
    END
 END;
 RETURN iseq
END GetParams;

PROCEDURE Do;
VAR
  iseq: IntSeq;
  start, end, sum, i: LONGINT;
BEGIN
  iseq := GetParams();
  Gss(iseq, start, end, sum);
  i := start;
  Out.String("[");
  WHILE (i <= end) DO
    Out.LongInt(iseq[i],0);
    IF (i < end) THEN Out.Char(',') END;
    INC(i)
  END;
  Out.String("]: ");Out.LongInt(sum,0);Out.Ln
END Do;

BEGIN
  Do
END GreatestSubsequentialSum.


```

Execute:<br/>

```txt

GreatestSubsequentialSum -1 -2 3 5 6 -2 -1 4 -4 2 -2
GreatestSubsequentialSum -1 -5 -3

```

{{out}}

```txt

[3,5,6,-2,-1,4]: 15
[]: 0

```



## OCaml


```ocaml
let maxsubseq =
  let rec loop sum seq maxsum maxseq = function
    | [] -> maxsum, List.rev maxseq
    | x::xs ->
        let sum = sum + x
        and seq = x :: seq in
          if sum < 0 then
            loop 0 [] maxsum maxseq xs
          else if sum > maxsum then
            loop sum seq sum seq xs
          else
            loop sum seq maxsum maxseq xs
  in
    loop 0 [] 0 []

let _ =
  maxsubseq [-1 ; -2 ; 3 ; 5 ; 6 ; -2 ; -1 ; 4; -4 ; 2 ; -1]
```


This returns a pair of the maximum sum and (one of) the maximum subsequence(s).


## Oz


```oz
declare
  fun {MaxSubSeq Xs}

     fun {Step [Sum0 Seq0 MaxSum MaxSeq] X}
        Sum = Sum0 + X
        Seq = X|Seq0
     in
        if Sum > MaxSum then
           %% found new maximum
           [Sum Seq Sum Seq]
        elseif Sum < 0 then
           %% discard negative subseqs
           [0 nil MaxSum MaxSeq]
        else
           [Sum Seq MaxSum MaxSeq]
        end
     end

     [_ _ _ MaxSeq] = {FoldL Xs Step [0 nil 0 nil]}
  in
     {Reverse MaxSeq}
  end
in
  {Show {MaxSubSeq [~1 ~2 3 5 6 ~2 ~1 4 ~4 2 1]}}
```



## PARI/GP

Naive quadratic solution (with end-trimming).

```parigp
grsub(v)={
  my(mn=1,mx=#v,r=0,at,c);
  if(vecmax(v)<=0,return([1,0]));
  while(v[mn]<=0,mn++);
  while(v[mx]<=0,mx--);
  for(a=mn,mx,
    c=0;
    for(b=a,mx,
      c+=v[b];
      if(c>r,r=c;at=[a,b])
    )
  );
  at
};
```



## Pascal


```pascal
Program GreatestSubsequentialSum(output);

var
  a: array[1..11] of integer = (-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1);
  i, j: integer;
  seqStart, seqEnd: integer;
  maxSum, seqSum: integer;

begin
  maxSum   := 0;
  seqStart := 0;
  seqEnd   := -1;
  for i := low(a) to high(a) do
  begin
    seqSum := 0;
    for j := i to high(a) do
    begin
      seqSum := seqSum + a[j];
      if seqSum > maxSum then
      begin
        maxSum   := seqSum;
        seqStart := i;
        seqEnd   := j;
      end;
    end;
  end;

  writeln ('Sequence: ');
  for i := low(a) to high(a) do
    write (a[i]:3);
  writeln;
  writeln ('Subsequence with greatest sum: ');
  for i := low(a) to seqStart - 1 do
    write (' ':3);
  for i := seqStart to seqEnd do
    write (a[i]:3);
  writeln;
  writeln ('Sum:');
  writeln (maxSum);
end.
```

{{out}}

```txt
:> ./GreatestSubsequentialSum
Sequence:
 -1 -2  3  5  6 -2 -1  4 -4  2 -1
Subsequence with greatest sum:
        3  5  6 -2 -1  4
Sum:
15

```



## Perl

O(n) running-sum method:

```perl
use strict;

sub max_sub(\@) {
	my ($a, $maxs, $maxe, $s, $sum, $maxsum) = shift;
	foreach (0 .. $#$a) {
		my $t = $sum + $a->[$_];
		($s, $sum) = $t > 0 ? ($s, $t) : ($_ + 1, 0);

		if ($maxsum < $sum) {
			$maxsum = $sum;
			($maxs, $maxe) = ($s, $_ + 1)
		}
	}
	@$a[$maxs .. $maxe - 1]
}

my @a = map { int(rand(20) - 10) } 1 .. 10;
my @b = (-1) x 10;

print "seq: @a\nmax: [ @{[max_sub @a]} ]\n";
print "seq: @b\nmax: [ @{[max_sub @b]} ]\n";
```

{{out}}

```txt

seq: -7 5 -3 0 5 -5 -1 -1 -5 1
max: [ 5 -3 0 5 ]
seq: -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
max: [  ]

```


Naive and potentionally very slow method:

```perl
use strict;

my @a = (-1 , -2 , 3 , 5 , 6 , -2 , -1 , 4 , -4 , 2 , -1);

my @maxsubarray;
my $maxsum = 0;

foreach my $begin (0..$#a) {
        foreach my $end ($begin..$#a) {
                my $sum = 0;
                $sum += $_ foreach @a[$begin..$end];
                if($sum > $maxsum) {
                        $maxsum = $sum;
                        @maxsubarray = @a[$begin..$end];
                }
        }
}

print "@maxsubarray\n";
```



## Perl 6

{{trans|Python}}

{{works with|Rakudo|2016.12}}


```perl6
sub max-subseq (*@a) {
    my ($start, $end, $sum, $maxsum) = -1, -1, 0, 0;
    for @a.kv -> $i, $x {
        $sum += $x;
        if $maxsum < $sum {
            ($maxsum, $end) = $sum, $i;
        }
        elsif $sum < 0 {
            ($sum, $start) = 0, $i;
        }
    }
    return @a[$start ^.. $end];
}
```


Another solution, not translated from any other language:

For each starting position, we calculate all the subsets starting at that position.
They are combined with the best subset ($max-subset) from previous loops, to form (@subsets).
The best of those @subsets is saved at the new $max-subset.

Consuming the array (.shift) allows us to skip tracking the starting point; it is always 0.

The empty sequence is used to initialize $max-subset, which fulfils the "all negative" requirement of the problem.


```perl6
sub max-subseq ( *@a ) {

    my $max-subset = ();
    while @a {
        my @subsets = [\,] @a;
        @subsets.push: $max-subset;
        $max-subset = @subsets.max: { [+] .list };
        @a.shift;
    }

    return $max-subset;
}

max-subseq( -1, -2,  3,  5,  6, -2, -1,  4, -4,  2, -1 ).say;
max-subseq( -2, -2, -1,  3,  5,  6, -1,  4, -4,  2, -1 ).say;
max-subseq( -2, -2, -1, -3, -5, -6, -1, -4, -4, -2, -1 ).say;
```


{{out}}

```txt

(3 5 6 -2 -1 4)
(3 5 6 -1 4)
()
```



## Phix

{{Trans|Euphoria}}

```Phix
function maxSubseq(sequence s)
integer this, maxsum = 0, first = 1, last = 0
    for i=1 to length(s) do
        this = 0
        for j=i to length(s) do
            this += s[j]
            if this>maxsum then
                {maxsum,first,last} = {this,i,j}
            end if
        end for
    end for
    return s[first..last]
end function
? maxSubseq({-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1})
? maxSubseq({})
? maxSubseq({-1, -5, -3})
```

{{out}}

```txt

{3,5,6,-2,-1,4}
{}
{}

```



## PHP



```PHP

<?php

function max_sum_seq($sequence) {
  // This runs in linear time.
  $sum_start = 0;
  $sum = 0;
  $max_sum = 0;
  $max_start = 0;
  $max_len = 0;
  for ($i = 0; $i < count($sequence); $i += 1) {
    $n = $sequence[$i];
    $sum += $n;
    if ($sum > $max_sum) {
      $max_sum = $sum;
      $max_start = $sum_start;
      $max_len = $i + 1 - $max_start;
    }
    if ($sum < 0) { # start new sequence
      $sum = 0;
      $sum_start = $i + 1;
    }
  }
  return array_slice($sequence, $max_start, $max_len);
}

function print_array($arr) {
  if (count($arr) > 0) {
    echo join(" ", $arr);
  } else {
    echo "(empty)";
  }
  echo '
';
}
// tests
print_array(max_sum_seq(array(-1, 0, 15, 3, -9, 12, -4)));
print_array(max_sum_seq(array(-1)));
print_array(max_sum_seq(array(4, -10, 3)));
?>

```

{{out}} in browser:
<lang>
0 15 3 -9 12
(empty)
4

```



## PicoLisp


```PicoLisp
(maxi '((L) (apply + L))
   (mapcon '((L) (maplist reverse (reverse L)))
      (-1 -2 3 5 6 -2 -1 4 -4 2 -1) ) )
```

{{out}}

```txt
-> (3 5 6 -2 -1 4)
```



## PL/I


```pli
*process source attributes xref;
 ss: Proc Options(Main);
 /* REXX ***************************************************************
 * 26.08.2013 Walter Pachl translated from REXX version 3
 **********************************************************************/
  Dcl HBOUND builtin;
  Dcl SYSPRINT Print;
  Dcl (I,J,LB,MAXSUM,SEQEND,SEQSTART,SEQSUM) Bin Fixed(15);
  Dcl s(11) Bin Fixed(15) Init(-1,-2,3,5,6,-2,-1,4,-4,2,-1);
  maxSum   = 0;
  seqStart = 0;
  seqEnd   = -1;
  do i = 1 To hbound(s);
    seqSum = 0;
    Do j = i to hbound(s);
      seqSum = seqSum + s(j);
      if seqSum > maxSum then Do;
        maxSum   = seqSum;
        seqStart = i;
        seqEnd   = j;
        end;
      end;
    end;
  Put Edit('Sequence:')(Skip,a);
  Put Edit('')(Skip,a);
  Do i=1 To hbound(s);
    Put Edit(s(i))(f(3));
    End;
  Put Edit('Subsequence with greatest sum:')(Skip,a);
  If seqend<seqstart Then
    Put Edit('empty')(Skip,a);
  Else Do;
    /*ol=copies('   ',seqStart-1)*/
    lb=(seqStart-1)*3;
    Put Edit(' ')(Skip,a(lb));
    Do i = seqStart to seqEnd;
      Put Edit(s(i))(f(3));
      End;
    Put Edit('Sum:',maxSum)(Skip,a,f(5));
    End;
 End;
```

{{out}}

```txt

Sequence:
 -1 -2  3  5  6 -2 -1  4 -4  2 -1
Subsequence with greatest sum:
        3  5  6 -2 -1  4
Sum:   15

```



## Potion


```potion
gss = (lst) :
   # Find discrete integral
   integral = (0)
   accum = 0
   lst each (n): accum = accum + n, integral append(accum).
   # Check integral[b + 1] - integral[a] for all 0 <= a <= b < N
   max = -1
   max_a = 0
   max_b = 0
   lst length times (b) :
      b times (a) :
         if (integral(b + 1) - integral(a) > max) :
            max = integral(b + 1) - integral(a)
            max_a = a
            max_b = b
         .
      .
   .
   # Print the results
   if (max >= 0) :
      (lst slice(max_a, max_b) join(" + "), " = ", max, "\n") join print
   .
   else :
      "No subsequence larger than 0\n" print
   .
.

gss((-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1))
gss((-1, -2, -3, -4, -5))
gss((7,-6, -8, 5, -2, -6, 7, 4, 8, -9, -3, 2, 6, -4, -6))
```



```txt

3 + 5 + 6 + -2 + -1 + 4 = 15
No subsequence larger than 0
7 + 4 + 8 = 19

```



## Prolog


### Constraint Handling Rules

CHR is a programming language created by '''Professor Thom Frühwirth'''.

Works with SWI-Prolog and module CHR written by '''Tom Schrijvers''' and '''Jan Wielemaker'''.

```Prolog
:- use_module(library(chr)).

:- chr_constraint
	init_chr/2,
	seq/2,
	% gss(Deb, Len, TT)
	gss/3,
	% gsscur(Deb, Len, TT, IdCur)
	gsscur/4,
	memoseq/3,
	clean/0,
	greatest_subsequence/0.


greatest_subsequence <=>
	L = [-1 , -2 , 3 , 5 , 6 , -2 , -1 , 4 , -4 , 2 , -1],
	init_chr(1, L),
	find_chr_constraint(gss(Deb, Len, V)),
	clean,
	writeln(L),
	forall(between(1, Len, I),
	       (   J is I+Deb-1, nth1(J, L, N), format('~w ', [N]))),
	format('==> ~w ~n', [V]).

% destroy last constraint gss
clean \ gss(_,_,_) <=> true.
clean <=> true.

init_chr_end @ init_chr(_, []) <=> gss(0, 0, 0), gsscur(1,0,0,1).

init_chr_loop @ init_chr(N, [H|T]) <=> seq(N, H), N1 is N+1, init_chr(N1, T).

% here, we memorize the list
gsscur_with_negative @ gsscur(Deb, Len, TT, N),  seq(N, V) <=> V =< 0 |
             memoseq(Deb, Len, TT),
	     TT1 is TT + V,
	     N1 is N+1,
	     % if TT1 becomes negative,
	     % we begin a new subsequence
	     (	 TT1 < 0 -> gsscur(N1,0,0,N1)
	     ;	 Len1 is Len + 1, gsscur(Deb, Len1, TT1, N1)).

gsscur_with_positive @ gsscur(Deb, Len, TT, N),  seq(N, V) <=> V > 0 |
             TT1 is TT + V,
	     N1 is N+1,
	     Len1 is Len + 1,
	     gsscur(Deb, Len1, TT1, N1).

gsscur_end @ gsscur(Deb, Len, TT, _N) <=> memoseq(Deb, Len, TT).

memoseq(_DC, _LC, TTC), gss(D, L, TT) <=> TTC =< TT |
	       gss(D, L, TT).

memoseq(DC, LC, TTC), gss(_D, _L, TT) <=> TTC > TT |
		gss(DC, LC, TTC).
```

{{out}}

```txt
 ?- greatest_subsequence.
[-1,-2,3,5,6,-2,-1,4,-4,2,-1]
3 5 6 -2 -1 4 ==> 15
true ;
false.

```



### Brute Force

Works with [https://rosettacode.org/wiki/GNU_Prolog GNU Prolog].

```Prolog
subseq(Sub, Seq) :- suffix(X, Seq), prefix(Sub, X).

maxsubseq(List, Sub, Sum) :-
  findall(X, subseq(X, List), Subs),
  maplist(sum_list, Subs, Sums),
  max_list(Sums, Sum),
  nth(N, Sums, Sum),
  nth(N, Subs, Sub).
```

{{out}}

```txt
| ?- maxsubseq([-1,-2,3,5,6,-2,-1,4,-4,2,-1], Sub, Sum).

Sub = [3,5,6,-2,-1,4]
Sum = 15 ?

yes

```



## PureBasic


```PureBasic
If OpenConsole()
  Define s$, a, b, p1, p2, sum, max, dm=(?EndOfMyData-?MyData)
  Dim Seq.i(dm/SizeOf(Integer))
  CopyMemory(?MyData,@seq(),dm)

  For a=0 To ArraySize(seq())
    sum=0
    For b=a To ArraySize(seq())
      sum+seq(b)
      If sum>max
        max=sum
        p1=a
        p2=b
      EndIf
    Next
  Next

  For a=p1 To p2
    s$+str(seq(a))
    If a<p2
      s$+"+"
    EndIf
  Next
  PrintN(s$+" = "+str(max))

  Print("Press ENTER to quit"): Input()
  CloseConsole()
EndIf


DataSection
  MyData:
  Data.i  -1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1
  EndOfMyData:
EndDataSection
```



## Python


### Imperative

Naive, inefficient but really simple solution which tests all possible subsequences, as in a few of the other examples:

```python
def maxsubseq(seq):
  return max((seq[begin:end] for begin in xrange(len(seq)+1)
                             for end in xrange(begin, len(seq)+1)),
             key=sum)
```


Classic linear-time constant-space solution based on algorithm from "Programming Pearls" book.

```python
def maxsum(sequence):
    """Return maximum sum."""
    maxsofar, maxendinghere = 0, 0
    for x in sequence:
        # invariant: ``maxendinghere`` and ``maxsofar`` are accurate for ``x[0..i-1]``
        maxendinghere = max(maxendinghere + x, 0)
        maxsofar = max(maxsofar, maxendinghere)
    return maxsofar
```

Adapt the above-mentioned solution to return maximizing subsequence. See http://www.java-tips.org/java-se-tips/java.lang/finding-maximum-contiguous-subsequence-sum-using-divide-and-conquer-app.html

```python
def maxsumseq(sequence):
    start, end, sum_start = -1, -1, -1
    maxsum_, sum_ = 0, 0
    for i, x in enumerate(sequence):
        sum_ += x
        if maxsum_ < sum_: # found maximal subsequence so far
            maxsum_ = sum_
            start, end = sum_start, i
        elif sum_ < 0: # start new sequence
            sum_ = 0
            sum_start = i
    assert maxsum_ == maxsum(sequence)
    assert maxsum_ == sum(sequence[start + 1:end + 1])
    return sequence[start + 1:end + 1]
```

Modify ``maxsumseq()`` to allow any iterable not just sequences.

```python
def maxsumit(iterable):
    maxseq = seq = []
    start, end, sum_start = -1, -1, -1
    maxsum_, sum_ = 0, 0
    for i, x in enumerate(iterable):
        seq.append(x); sum_ += x
        if maxsum_ < sum_:
            maxseq = seq; maxsum_ = sum_
            start, end = sum_start, i
        elif sum_ < 0:
            seq = []; sum_ = 0
            sum_start = i
    assert maxsum_ == sum(maxseq[:end - start])
    return maxseq[:end - start]
```

Elementary tests:

```python
f = maxsumit
assert f([]) == []
assert f([-1]) == []
assert f([0])  == []
assert f([1])       == [1]
assert f([1, 0])    == [1]
assert f([0, 1])    == [0, 1]
assert f([0, 1, 0]) == [0, 1]
assert f([2])         == [2]
assert f([2, -1])     == [2]
assert f([-1, 2])     == [2]
assert f([-1, 2, -1]) == [2]
assert f([2, -1, 3])         == [2, -1, 3]
assert f([2, -1, 3, -1])     == [2, -1, 3]
assert f([-1, 2, -1, 3])     == [2, -1, 3]
assert f([-1, 2, -1, 3, -1]) == [2, -1, 3]
assert f([-1, 1, 2, -5, -6]) == [1,2]
```



### Functional

We can efficiently derive sum and sequence together, without mutation, using '''reduce''' to express a linear accumulation over a fold:
{{Trans|Haskell}}
{{Works with|Python|3.7}}

```python
'''Greatest subsequential sum'''

from functools import (reduce)


# maxSubseq :: [Int] -> [Int] -> (Int, [Int])
def maxSubseq(xs):
    '''Subsequence of xs with the maximum sum'''
    def go(ab, x):
        (m1, m2) = ab[0]
        hi = max((0, []), (m1 + x, m2 + [x]))
        return (hi, max(ab[1], hi))
    return reduce(go, xs, ((0, []), (0, [])))[1]


# TEST -----------------------------------------------------------
print(
    maxSubseq(
        [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1]
    )
)
```

{{Out}}

```txt
(15, [3, 5, 6, -2, -1, 4])
```



## R


```R
max.subseq <- function(x) {
  cumulative <- cumsum(x)
  min.cumulative.so.far <- Reduce(min, cumulative, accumulate=TRUE)
  end <- which.max(cumulative-min.cumulative.so.far)
  begin <- which.min(c(0, cumulative[1:end]))
  if (end >= begin) x[begin:end] else x[c()]
}
```

{{out}}

```r>
 max.subseq(c(-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1))
[1]  3  5  6 -2 -1  4
```



## Racket

Linear time version, returns the maximum subsequence and its sum.

```racket

(define (max-subseq l)
  (define-values (_ result _1 max-sum)
    (for/fold ([seq '()] [max-seq '()] [sum 0] [max-sum 0])
      ([i l])
      (cond [(> (+ sum i) max-sum)
             (values (cons i seq) (cons i seq) (+ sum i) (+ sum i))]
            [(< (+ sum i) 0)
             (values '() max-seq 0 max-sum)]
            [else
             (values (cons i seq) max-seq (+ sum i) max-sum)])))
  (values (reverse result) max-sum))

```

For example:

```Racket

> (max-subseq '(-1 -2 3 5 6 -2 -1 4 -4 2 -1))
'(3 5 6 -2 -1 4)
15

```



## Raven


```Raven
[ -1 -2 3 5 6 -2 -1 4 -4 2 -1 ] as $seq

1 31 shl as $max

0   $seq length   1 range each as $i
    0 as $sum
    $i   $seq length  1 range each as $j
        $seq $j get   $sum +  as $sum
        $sum $max > if
            $sum as $max
            $i as $i1
            $j as $j1

"Sum: " print
$i1 $j1 1 range each
    #dup "$seq[%d]\n" print
    $seq swap get "%d," print
$max   $seq $j1 get   "%d  =  %d\n" print
```

{{out}}

```txt
Sum: 3,5,6,-2,-1,4  =  15
```



## REXX


### shortest greatest subsequential sum

This REXX version will find the   sum   of the   ''shortest greatest continuous subsequence''.

```rexx
/*REXX program  finds and displays  the  shortest  greatest continuous subsequence  sum.*/
parse arg @;         w=words(@)                  /*get arg list;  number words in list. */
say 'words='w    "   list="@                     /*show number words & LIST to terminal.*/
sum=0;      at=w+1                               /*default sum, length, and "starts at".*/
L=0                                              /* [↓]  process the list of numbers.   */
     do j=1  for w;     f=word(@, j)             /*select one number at a time from list*/
         do k=j  to w;  s=f                      /* [↓]  process a sub─list of numbers. */
                        do m=j+1  to k;    s=s+word(@, m);    end  /*m*/
         if s>sum  then do;       sum=s;   at=j;  L=k-j+1;    end
         end   /*k*/                             /* [↑]  chose greatest sum of numbers. */
     end       /*j*/
say
$=subword(@,at,L);    if $==''  then $="[NULL]"  /*Englishize the  null  (value).       */
say 'sum='sum/1             "   sequence="$      /*stick a fork in it,  we're all done. */
```

'''output'''   when the following was used for the list:   <tt> -1 -2 3 5 6 -2 -1 4 -4 2 -1 </tt>

```txt

words=11    list=-1 -2 3 5 6 -2 -1 4 -4 2 -1

sum=15    sequence=3 5 6 -2 -1 4

```

'''output'''   when the following was used for the list:   <tt> 1 2 3 4 -777 1 2 3 4 0 0 </tt>

```txt

words=12    list=1 2 3 4 0 -777 1 2 3 4 0 0

sum=10    sequence=1 2 3 4

```



### longest greatest subsequential sum

This REXX version will find the   sum   of the   ''longest greatest continuous subsequence''.

```rexx
/*REXX program  finds and displays  the  longest  greatest continuous subsequence  sum. */
parse arg @;         w=words(@)                  /*get arg list;  number words in list. */
say 'words='w    "   list="@                     /*show number words & LIST to terminal,*/
sum=0;      at=w+1                               /*default sum, length, and "starts at".*/
L=0                                              /* [↓]  process the list of numbers.   */
     do j=1  for w;     f=word(@,j)              /*select one number at a time from list*/
         do k=j  to w;  _=k-j+1;     s=f         /* [↓]  process a sub─list of numbers. */
                                         do m=j+1  to k;    s=s+word(@, m);   end  /*m*/
         if (s==sum & _>L) | s>sum  then do;       sum=s;   at=j;      L=_;   end
         end   /*k*/                             /* [↑]  chose the longest greatest sum.*/
     end       /*j*/
say
$=subword(@,at,L);    if $==''  then $="[NULL]"  /*Englishize the  null   (value).      */
say 'sum='sum/1             "   sequence="$      /*stick a fork in it,  we're all done. */
```

'''output'''   when the following was used for the list:   <tt> 1 2 3 4 -777 1 2 3 4 0 0 </tt>

```txt

words=12    list=1 2 3 4 0 -777 1 2 3 4 0 0

sum=10    sequence=1 2 3 4 0 0

```


===Version 3 (translated from Pascal)===

```rexx
/* REXX ***************************************************************
* 09.08.2012 Walter Pachl translated Pascal algorithm to Rexx
**********************************************************************/
  s=' -1 -2  3  5  6 -2 -1  4 -4  2 -1'
  maxSum   = 0
  seqStart = 0
  seqEnd   = -1
  do i = 1 To words(s)
    seqSum = 0
    Do j = i to words(s)
      seqSum = seqSum + word(s,j)
      if seqSum > maxSum then Do
        maxSum   = seqSum
        seqStart = i
        seqEnd   = j
        end
      end
    end
  Say 'Sequence:'
  Say s
  Say 'Subsequence with greatest sum: '
  If seqend<seqstart Then
    Say 'empty'
  Else Do
    ol=copies('   ',seqStart-1)
    Do i = seqStart to seqEnd
      ol=ol||right(word(s,i),3)
      End
    Say ol
    Say 'Sum:' maxSum
    End
```

{{out}}

```txt

Sequence:
 -1 -2  3  5  6 -2 -1  4 -4  2 -1
Subsequence with greatest sum:
        3  5  6 -2 -1  4
Sum: 15

```



## Ring


```ring

# Project : Greatest subsequential sum

aList1 = [0, 1, 2, -3, 3, -1, 0, -4, 0, -1, -4, 2]
see "[0, 1, 2, -3, 3, -1, 0, -4, 0, -1, -4, 2]  -> " + sum(aList1) + nl
aList2 = [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1]
see "[-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1] -> " + sum(aList2) + nl
aList3 = [-1, -2, -3, -4, -5]
see "[-1, -2, -3, -4, -5] -> " + sum(aList3) + nl
aList4 = []
see "[] - > " + sum(aList4) + nl

func sum aList
     sumold = []
     sumnew = []
     snew = 0
     flag = 0
     if len(aList) = 0
        return 0
     ok
     for s=1 to len(aList)
         if aList[s] > -1
            flag = 1
         ok
     next
     if flag = 0
        return "[]"
     ok
     for n=1 to len(aList)
         sumold = []
         sold = 0
         for m=n to len(aList)
             add(sumold, aList[m])
             sold = sold + aList[m]
             if sold > snew
                snew = sold
                sumnew = sumold
             ok
         next
     next
     return showarray(sumnew)

func showarray(a)
     conv = "["
     for i = 1 to len(a)
         conv = conv + string(a[i]) + ", "
     next
     conv = left(conv, len(conv) - 2) + "]"
     return conv

```

Output:

```txt

[0, 1, 2, -3, 3, -1, 0, -4, 0, -1, -4, 2]  -> [0, 1, 2]
[-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1] -> [3, 5, 6, -2, -1, 4]
[-1, -2, -3, -4, -5] -> []
[] - > 0

```



## Ruby


### Brute Force:

Answer is stored in "slice". It is very slow O(n**3)

```ruby
def subarray_sum(arr)
  max, slice = 0, []
  arr.each_index do |i|
    (i...arr.length).each do |j|
      sum = arr[i..j].inject(0, :+)
      max, slice = sum, arr[i..j]  if sum > max
    end
  end
  [max, slice]
end
```

'''Test:'''

```ruby
[ [1, 2, 3, 4, 5, -8, -9, -20, 40, 25, -5],
  [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1],
  [-1, -2, -3, -4, -5],
  []
].each do |input|
  puts "\nInput seq: #{input}"
  puts "  Max sum: %d\n   Subseq: %s" % subarray_sum(input)
end
```

{{out}}

```txt

Input seq: [1, 2, 3, 4, 5, -8, -9, -20, 40, 25, -5]
  Max sum: 65
   Subseq: [40, 25]

Input seq: [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1]
  Max sum: 15
   Subseq: [3, 5, 6, -2, -1, 4]

Input seq: [-1, -2, -3, -4, -5]
  Max sum: 0
   Subseq: []

Input seq: []
  Max sum: 0
   Subseq: []

```



### Linear Time Version:

A better answer would run in O(n) instead of O(n**2) using numerical properties to remove the need for the inner loop.


```ruby
# the trick is that at any point
# in the iteration if starting a new chain is
# better than your current score with this element
# added to it, then do so.
# the interesting part is proving the math behind it
def subarray_sum(arr)
  curr = max = 0
  first, last, curr_first = arr.size, 0, 0
  arr.each_with_index do |e,i|
    curr += e
    if e > curr
      curr = e
      curr_first = i
    end
    if curr > max
      max = curr
      first = curr_first
      last = i
    end
  end
  return max, arr[first..last]
end
```

The test result is the same above.


## Run BASIC


```Runbasic
seq$ = "-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1"
max = -999
for i = 1 to 11
sum = 0
   for j = i to 11
      sum = sum + val(word$(seq$,j,","))
      If sum > max then
         max = sum
         i1  = i
         j1  = j
      end if
   next j
next i
print "Sum:";
for i = i1 to j1
   print word$(seq$,i,",");",";
next i
print " = ";max
```

{{out}}

```txt
Sum: 3, 5, 6, -2, -1, 4, = 15
```




## Rust

Naive brute force

```rust
fn main() {
    let nums = [1,2,39,34,20, -20, -16, 35, 0];

    let mut max = 0;
    let mut boundaries = 0..0;

    for length in 0..nums.len() {
        for start in 0..nums.len()-length {
            let sum = (&nums[start..start+length]).iter()
                .fold(0, |sum, elem| sum+elem);
            if sum > max {
                max = sum;
                boundaries = start..start+length;
            }
        }
    }

    println!("Max subsequence sum: {} for {:?}", max, &nums[boundaries]);;
}
```

{{out}}

```txt
Max subsequence sum: 96 for [1, 2, 39, 34, 20]
```



## Scala

{{works with|Scala|2.8}}
The first solution solves the problem as specified, the second gives preference to the longest subsequence in case of ties. They are both vulnerable to integer overflow.

The third solution accepts any type N for which there's a Numeric[N], which includes all standard numeric types, and can be extended to include user defined numeric classes.

The last solution keeps to linear time by increasing complexity slightly.

```scala
def maxSubseq(l: List[Int]) = l.scanRight(Nil : List[Int]) {
  case (el, acc) if acc.sum + el < 0 => Nil
  case (el, acc)                     => el :: acc
} max Ordering.by((_: List[Int]).sum)

def biggestMaxSubseq(l: List[Int]) = l.scanRight(Nil : List[Int]) {
  case (el, acc) if acc.sum + el < 0 => Nil
  case (el, acc)                     => el :: acc
} max Ordering.by((ss: List[Int]) => (ss.sum, ss.length))

def biggestMaxSubseq[N](l: List[N])(implicit n: Numeric[N]) = {
  import n._
  l.scanRight(Nil : List[N]) {
    case (el, acc) if acc.sum + el < zero => Nil
    case (el, acc)                        => el :: acc
  } max Ordering.by((ss: List[N]) => (ss.sum, ss.length))
}

def linearBiggestMaxSubseq[N](l: List[N])(implicit n: Numeric[N]) = {
  import n._
  l.scanRight((zero, Nil : List[N])) {
    case (el, (acc, _)) if acc + el < zero => (zero, Nil)
    case (el, (acc, ss))                   => (acc + el, el :: ss)
  } max Ordering.by((t: (N, List[N])) => (t._1, t._2.length)) _2
}
```



## Scheme


```scheme
(define (maxsubseq in)
  (let loop
    ((_sum 0) (_seq (list)) (maxsum 0) (maxseq (list)) (l in))
    (if (null? l)
        (cons maxsum (reverse maxseq))
        (let* ((x (car l)) (sum (+ _sum x)) (seq (cons x _seq)))
          (if (> sum 0)
              (if (> sum maxsum)
                  (loop sum seq    sum    seq (cdr l))
                  (loop sum seq maxsum maxseq (cdr l)))
              (loop 0 (list) maxsum maxseq (cdr l)))))))
```


This returns a cons of the maximum sum and (one of) the maximum subsequence(s).


## Seed7


```seed7
$ include "seed7_05.s7i";

const func array integer: maxSubseq (in array integer: sequence) is func
  result
    var array integer: maxSequence is 0 times 0;
  local
    var integer: number is 0;
    var integer: index is 0;
    var integer: currentSum is 0;
    var integer: currentStart is 1;
    var integer: maxSum is 0;
    var integer: startPos is 0;
    var integer: endPos is 0;
  begin
    for number key index range sequence do
      currentSum +:= number;
      if currentSum < 0 then
        currentStart := succ(index);
        currentSum := 0;
      elsif currentSum > maxSum then
        maxSum := currentSum;
        startPos := currentStart;
        endPos := index;
      end if;
    end for;
    if startPos <= endPos and startPos >= 1 and endPos >= 1 then
      maxSequence := sequence[startPos .. endPos];
    end if;
  end func;

const proc: main is func
  local
    const array integer: a1 is [] (-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1);
    const array integer: a2 is [] (-1, -2, -3, -5, -6, -2, -1, -4, -4, -2, -1);
    var integer: number is 0;
  begin
    write("Maximal subsequence:");
    for number range maxSubseq(a1) do
      write(" " <& number);
    end for;
    writeln;
    write("Maximal subsequence:");
    for number range maxSubseq(a2) do
      write(" " <& number);
    end for;
    writeln;
  end func;
```

{{out}}

```txt

Maximal subsequence: 3 5 6 -2 -1 4
Maximal subsequence:

```



## Sidef

{{trans|Perl 6}}

```ruby
func maxsubseq(*a) {
    var (start, end, sum, maxsum) = (-1, -1, 0, 0);
    a.each_kv { |i, x|
        sum += x;
        if (maxsum < sum) {
            maxsum = sum;
            end = i;
        }
        elsif (sum < 0) {
            sum = 0;
            start = i;
        }
    };
    a.ft(start+1, end);
}

say maxsubseq(-1, -2,  3,  5,  6, -2, -1,  4, -4,  2, -1);
say maxsubseq(-2, -2, -1,  3,  5,  6, -1,  4, -4,  2, -1);
say maxsubseq(-2, -2, -1, -3, -5, -6, -1, -4, -4, -2, -1);
```


{{out}}

```txt

[3, 5, 6, -2, -1, 4]
[3, 5, 6, -1, 4]
[]

```



## Standard ML


```sml
val maxsubseq = let
  fun loop (_, _, maxsum, maxseq) [] = (maxsum, rev maxseq)
    | loop (sum, seq, maxsum, maxseq) (x::xs) = let
        val sum = sum + x
        val seq = x :: seq
      in
        if sum < 0 then
          loop (0, [], maxsum, maxseq) xs
        else if sum > maxsum then
          loop (sum, seq, sum, seq) xs
        else
          loop (sum, seq, maxsum, maxseq) xs
      end
in
  loop (0, [], 0, [])
end;

maxsubseq [~1, ~2, 3, 5, 6, ~2, ~1, 4, ~4, 2, ~1]
```

This returns a pair of the maximum sum and (one of) the maximum subsequence(s).


## Tcl


```tcl
package require Tcl 8.5
set a {-1 -2 3 5 6 -2 -1 4 -4 2 -1}

# from the Perl solution
proc maxsumseq1 {a} {
    set len [llength $a]
    set maxsum 0

    for {set start 0} {$start < $len} {incr start} {
        for {set end $start} {$end < $len} {incr end} {
            set sum 0
            incr sum [expr [join [lrange $a $start $end] +]]
            if {$sum > $maxsum} {
                set maxsum $sum
                set maxsumseq [lrange $a $start $end]
            }
        }
    }
    return $maxsumseq
}

# from the Python solution
proc maxsumseq2 {sequence} {
    set start -1
    set end -1
    set maxsum_ 0
    set sum_ 0
    for {set i 0} {$i < [llength $sequence]} {incr i} {
        set x [lindex $sequence $i]
        incr sum_ $x
        if {$maxsum_ < $sum_} {
            set maxsum_ $sum_
            set end $i
        } elseif {$sum_ < 0} {
            set sum_ 0
            set start $i
        }
    }
    assert {$maxsum_ == [maxsum $sequence]}
    assert {$maxsum_ == [sum [lrange $sequence [expr {$start + 1}] $end]]}
    return [lrange $sequence [expr {$start + 1}] $end]
}

proc maxsum {sequence} {
    set maxsofar 0
    set maxendinghere 0
    foreach x $sequence {
        set maxendinghere [expr {max($maxendinghere + $x, 0)}]
        set maxsofar [expr {max($maxsofar, $maxendinghere)}]
    }
    return $maxsofar
}

proc assert {condition {message "Assertion failed!"}} {
    if { ! [uplevel 1 [list expr $condition]]} {
        return -code error $message
    }
}

proc sum list {
    expr [join $list +]
}


puts "sequence:  $a"
puts "maxsumseq1: [maxsumseq1 $a]"
puts [time {maxsumseq1 $a} 1000]
puts "maxsumseq2: [maxsumseq2 $a]"
puts [time {maxsumseq2 $a} 1000]
```

{{out}}

```txt
sequence:  -1 -2 3 5 6 -2 -1 4 -4 2 -1
maxsumseq1: 3 5 6 -2 -1 4
367.041 microseconds per iteration
maxsumseq2: 3 5 6 -2 -1 4
74.623 microseconds per iteration
```



## Ursala

This example solves the problem by the naive algorithm of testing all possible subsequences.

```Ursala
#import std
#import int

max_subsequence = zleq$^l&r/&+ *aayK33PfatPRTaq ^/~& sum:-0

#cast %zL

example = max_subsequence <-1,-2,3,5,6,-2,-1,4,-4,2,-1>
```

The general theory of operation is as follows.
* The <code>max_subsequence</code> function is a composition of three functions, one to generate the sequences, one to sum all of them, and one to pick out the one with the maximum sum.
* The function that sums all the sequences is <code>* ^/~& sum:-0</code> which applies to every member of a list (by the <code>*</code> operator) and forms a pair (using the <code>^</code> operator) of the identity function (<code>~&</code>) of its argument, and the reduction (<code>:-</code>) of the sum over a list with a vacuous case result of 0.
* The function that picks out the maximum sum is <code>zleq$^l&r/&</code>, which uses the maximizing operator (<code>$^</code>) over a list of pairs with respect to the integer ordering relation (<code>zleq</code>) applied to the right sides of the pairs (<code>&r</code>), after which the left side (<code>l</code>) of the maximizing pair is extracted. The <code>/&</code> inserts an extra pair <code>(<>,0)</code> at the beginning of the list before searching it in case it's empty or has only negative sums.
* The function that generates all the sequences is <code>~&aayK33PfatPRTaq</code>, which appears as a suffix of the <code>*</code> operator rather than being used explicitly.
* The sequence generating function is in the form of a recursive conditional (<code>q</code>) with predicate <code>a</code>, inductive case <code>ayK33PfatPRT</code> and base case <code>a</code>, meaning that in the base case of an empty list argument, the argument itself is returned.
* The inductive case, <code>ayK33PfatPRT</code> is a concatenation (<code>T</code>) of two functions <code>ayK33</code> and <code>fatPR</code>
* The latter function, <code>fatPR</code> is a recursive call (<code>R</code>) of the enclosing recursive conditional (<code>f</code>) with the tail of the argument (<code>at</code>).
* The remaining function, <code>ayK33</code> uses the ''triangle-squared'' combinator <code>K33</code> of the list-lead operator <code>y</code> applied to the argument <code>a</code>.
* The list lead operator <code>y</code> by itself takes a non-empty list as an argument and returns a copy with the last item deleted.
* The triangle-squared combinator <code>K33</code> constructs a function that takes an input list of a length <math>n</math>, constructs a list of <math>n</math> copies of it, and applies its operand 0 times to the head, once to the head of tail, twice to the head of the tail of the tail, and so on. Hence, an operand of <code>y</code> will generate the list of all prefixes of a list.
{{out}}

```txt

<3,5,6,-2,-1,4>
```



## XPL0


```XPL0
include c:\cxpl\codes;
int Array, Size, Sum, Best, I, Lo, Hi, BLo, BHi;

[Array:= [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1];
Size:= 11;
Best:= -100000;
for Lo:= 0 to Size-1 do
    for Hi:= Lo to Size-1 do
        [Sum:= 0;
        for I:= Lo to Hi do
                Sum:= Sum + Array(I);
        if Sum > Best then
                [Best:= Sum;  BLo:= Lo;  BHi:= Hi];
        ];
Text(0, "Sequence = ");
for I:= 0 to Size-1 do
        [IntOut(0, Array(I)); Text(0, " ")];
CrLf(0);
Text(0, "Greatest = ");
for I:= BLo to BHi do
        [IntOut(0, Array(I)); Text(0, " ")];
CrLf(0);
Text(0, "Sum = ");  IntOut(0, Best);  CrLf(0);
]
```


{{out}}

```txt

Sequence = -1 -2 3 5 6 -2 -1 4 -4 2 -1
Greatest = 3 5 6 -2 -1 4
Sum = 15

```



## zkl

{{trans|F#}}

```zkl
fcn maxsubseq(s){
   s.reduce(fcn([(sum, seq, maxsum, maxseq)], x){
      sum=sum+x; seq=T(x).extend(seq);
      if(sum < 0)     return(0,T,maxsum,maxseq);
      if (sum>maxsum) return(sum, seq, sum, seq);
		      return(sum, seq, maxsum, maxseq);
   },
   T(0,T,0,T))[3].reverse();   // -->maxseq.reverse()
}
```


```zkl
s:=maxsubseq(T(-1,-2,3,5,6,-2,-1,4,-4,2,-1));
println(s.sum()," : ",s);

s:=maxsubseq(T(-1,-2)); println(s.sum()," : ",s);

s:=maxsubseq(T); println(s.sum()," : ",s);
```

{{out}}

```txt

15 : L(3,5,6,-2,-1,4)
0 : L()
0 : L()

```



## ZX Spectrum Basic

{{trans|BBC_BASIC}}

```zxbasic
10 DATA 12,0,1,2,-3,3,-1,0,-4,0,-1,-4,2
20 DATA 11,-1,-2,3,5,6,-2,-1,4,-4,2,-1
30 DATA 5,-1,-2,-3,-4,-5
40 FOR n=1 TO 3
50 READ l
60 DIM a(l)
70 FOR i=1 TO l
80 READ a(i)
90 PRINT a(i);
100 IF i<l THEN PRINT ", ";
110 NEXT i
120 PRINT
130 LET a=1: LET m=0: LET b=0
140 FOR i=1 TO l
150 LET s=0
160 FOR j=i TO l
170 LET s=s+a(j)
180 IF s>m THEN LET m=s: LET a=i: LET b=j
190 NEXT j
200 NEXT i
210 IF a>b THEN PRINT "[]": GO TO 280
220 PRINT "[";
230 FOR i=a TO b
240 PRINT a(i);
250 IF i<b THEN PRINT ", ";
260 NEXT i
270 PRINT "]"
280 NEXT n
```

