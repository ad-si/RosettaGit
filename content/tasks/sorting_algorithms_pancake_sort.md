+++
title = "Sorting algorithms/Pancake sort"
description = ""
date = 2019-10-18T20:13:34Z
aliases = []
[extra]
id = 6758
[taxonomies]
categories = ["task", "Sorting Algorithms"]
tags = []
languages = [
  "ada",
  "algol_68",
  "autohotkey",
  "basic",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "eiffel",
  "elena",
  "elixir",
  "euphoria",
  "fortran",
  "freebasic",
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
  "maple",
  "mathematica",
  "maxscript",
  "netrexx",
  "nim",
  "ocaml",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "sidef",
  "swift",
  "tcl",
  "ubasic_4th",
  "vba",
  "zkl",
]
+++

## Task

Sort an array of integers (of any convenient size) into ascending order using [[wp:Pancake sorting|Pancake sorting]].

In short, instead of individual elements being sorted, the only operation allowed is to "flip" one end of the list, like so:
 Before:
 '''6 7 8 9''' 2 5 3 4 1
 After:
 '''9 8 7 6''' 2 5 3 4 1

Only one end of the list can be flipped; this should be the low end, but the high end is okay if it's easier to code or works better, but it '''must''' be the same end for the entire solution. (The end flipped can't be arbitrarily changed.)

Show both the initial, unsorted list and the final sorted list. (Intermediate steps during sorting are optional.) Optimizations are optional (but recommended).

For more information on pancake sorting, see [[wp:Pancake sorting|the Wikipedia entry]].

See also:
* [[Number reversal game]]
* [[Topswops]]





## Ada



```Ada
with Ada.Text_IO;
procedure Pancake_Sort is
   generic
      type Element_Type is private;
      type Index_Type is range <>;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      with function ">" (Left, Right : Element_Type) return Boolean is <>;
   procedure Pancake_Sort (Data: in out Array_Type);

   procedure Pancake_Sort (Data: in out Array_Type) is
      procedure Flip (Up_To : in Index_Type) is
         Temp : constant Array_Type := Data (Data'First .. Up_To);
      begin
         for I in Temp'Range loop
            Data (I) := Temp (Temp'First + Up_To - I);
         end loop;
      end Flip;
      Max_Index : Index_Type;
   begin
      for I in reverse Data'First + 1 .. Data'Last loop
         Max_Index := Data'First;
         for A in Data'First + 1 .. I loop
            if Data(A) > Data (Max_Index) then
               Max_Index := A;
            end if;
         end loop;
         if Max_Index /= I then
            if Max_Index > Data'First then
               Flip (Max_Index);
            end if;
            Flip (I);
         end if;
      end loop;
   end Pancake_Sort;

   type Integer_Array is array (Positive range <>) of Integer;
   procedure Int_Pancake_Sort is new Pancake_Sort (Integer, Positive, Integer_Array);
   Test_Array : Integer_Array := (3, 14, 1, 5, 9, 2, 6, 3);
begin
   Int_Pancake_Sort (Test_Array);
   for I in Test_Array'Range loop
      Ada.Text_IO.Put (Integer'Image (Test_Array (I)));
   end loop;
   Ada.Text_IO.New_Line;
end Pancake_Sort;
```


Output:

```txt
 1 2 3 3 5 6 9 14
```



## ALGOL 68

```algol68
PROC flip = ([]INT s, INT n) []INT:
   BEGIN
      [UPB s]INT ss := s;
      INT temp;
      FOR i TO n OVER 2 DO
         temp := ss[i];
         ss[i] := ss[n-i+1];
         ss[n-i+1] := temp
      OD;
      ss
   END;

PROC pancake sort = ([]INT s) []INT:
   BEGIN
      INT m;
      [UPB s]INT ss := s;
      FOR i FROM UPB s DOWNTO 2 DO
         m := 1;
         FOR j FROM 2 TO i DO
            IF ss[j] > ss[m] THEN
                m := j
            FI
         OD;

         IF m < i THEN
            IF m > 1 THEN
                ss := flip (ss,m)
            FI;
            ss := flip (ss,i)
         FI
      OD;
    ss
   END;

[10]INT s;
FOR i TO UPB s DO
   s[i] := ENTIER (next random * 100-50)
OD;
printf (($"Pancake sort demonstration"l$));
printf (($"unsorted: "10(g(4) )l$, s));
printf (($"sorted:   "10(g(4) )l$, pancake sort(s)))

```

```txt
Pancake sort demonstration
unsorted:  -26 +41  -4 +21  +8  -2 +31 -47 -41  -7
sorted:    -47 -41 -26  -7  -4  -2  +8 +21 +31 +41

```


## AutoHotkey


```autohotkey
;---------------------------------------------------------------------------
Loop { ; test loop
;---------------------------------------------------------------------------
    Loop, % Data0 := 10
        Random, Data%A_Index%, 10, 99
    Unsorted := Array2List("Data")
    PancakeSort("Data")
    Sorted := Array2List("Data")
    MsgBox, 1, Pancake Sort, %Unsorted%`n%Sorted%
    IfMsgBox, Cancel, Break
}



;---------------------------------------------------------------------------
PancakeSort(Array) { ; implementation of pancake sort algorithm
;---------------------------------------------------------------------------
    Loop, % %Array%0 - 1 {
        m := 0
        Loop, % s := %Array%0 - A_Index + 1
            If (m <= %Array%%A_Index%)
                m := %Array%%A_Index%, p := A_Index
        If (p < s) && (p > 1)
            Flip(Array, p)
        If (p < s)
            Flip(Array, s)
    }
}



;---------------------------------------------------------------------------
Flip(Array, n) { ; flip the first n members of Array
;---------------------------------------------------------------------------
    Loop, % x := n // 2 {
        i := n - A_Index + 1
        %Array%%i% := (%Array%%A_Index% "", %Array%%A_Index% := %Array%%i%)
    }
}



;---------------------------------------------------------------------------
Array2List(Array) { ; returns a space separated list from an array
;---------------------------------------------------------------------------
    Loop, % %Array%0
        List .= (A_Index = 1 ? "" : " ") %Array%%A_Index%
    Return, List
}

```



## BASIC


### Text

```qbasic
RANDOMIZE TIMER

DIM nums(9) AS INTEGER
DIM L0 AS INTEGER, L1 AS INTEGER, n AS INTEGER

'initial values
FOR L0 = 0 TO 9
    nums(L0) = L0
NEXT
'scramble
FOR L0 = 9 TO 1 STEP -1
    n = INT(RND * (L0)) + 1
    IF n <> L0 THEN SWAP nums(n), nums(L0)
NEXT
'display initial condition
FOR L0 = 0 TO 9
    PRINT nums(L0);
NEXT
PRINT

FOR L1 = 9 TO 1 STEP -1
    n = 0
    FOR L0 = 1 TO L1
        IF nums(n) < nums(L0) THEN n = L0
    NEXT

    IF (n < L1) THEN
        IF (n > 0) THEN
            FOR L0 = 0 TO (n \ 2)
                SWAP nums(L0), nums(n - L0)
            NEXT
            FOR L0 = 0 TO 9
                PRINT nums(L0);
            NEXT
            PRINT
        END IF
        FOR L0 = 0 TO (L1 \ 2)
            SWAP nums(L0), nums(L1 - L0)
        NEXT

        FOR L0 = 0 TO 9
            PRINT nums(L0);
        NEXT
        PRINT
    END IF
NEXT
```


Sample output:
 0  4  6  1  8  7  2  5  3  9
 8  1  6  4  0  7  2  5  3  9
 3  5  2  7  0  4  6  1  8  9
 7  2  5  3  0  4  6  1  8  9
 1  6  4  0  3  5  2  7  8  9
 6  1  4  0  3  5  2  7  8  9
 2  5  3  0  4  1  6  7  8  9
 5  2  3  0  4  1  6  7  8  9
 1  4  0  3  2  5  6  7  8  9
 4  1  0  3  2  5  6  7  8  9
 2  3  0  1  4  5  6  7  8  9
 3  2  0  1  4  5  6  7  8  9
 1  0  2  3  4  5  6  7  8  9
 0  1  2  3  4  5  6  7  8  9


### Graphics

This is a graphical variation of the above.


```qbasic
RANDOMIZE TIMER

CONST delay = .1    'controls display speed

DIM nums(14) AS INTEGER
DIM L0 AS INTEGER, L1 AS INTEGER, n AS INTEGER, ttmp AS SINGLE

'initial values
FOR L0 = 0 TO 14
    nums(L0) = L0
NEXT
'scramble
FOR L0 = 14 TO 1 STEP -1
    n = INT(RND * (L0)) + 1
    IF n <> L0 THEN SWAP nums(n), nums(L0)
NEXT

'display initial condition
CLS
GOSUB displayer

FOR L1 = 14 TO 1 STEP -1
    n = 0
    FOR L0 = 1 TO L1
        IF nums(n) < nums(L0) THEN n = L0
    NEXT

    IF (n < L1) THEN
        IF (n > 0) THEN
            FOR L0 = 0 TO (n \ 2)
                SWAP nums(L0), nums(n - L0)
            NEXT
            GOSUB displayer
        END IF
        FOR L0 = 0 TO (L1 \ 2)
            SWAP nums(L0), nums(L1 - L0)
        NEXT

        GOSUB displayer
    END IF
NEXT

COLOR 7
END

displayer:
    LOCATE 1, 1
    FOR L0 = 0 TO 14
        COLOR nums(L0) + 1
        IF nums(L0) < 10 THEN PRINT " ";
        PRINT RTRIM$(LTRIM$(STR$(nums(L0)))); STRING$(nums(L0), 219); SPACE$(20)
    NEXT
    ttmp = TIMER
    DO WHILE TIMER < ttmp + delay: LOOP
    RETURN
```


Sample output:

 [[File:Pancake.gif]]




## BBC BASIC


```bbcbasic
      DIM test(9)
      test() = 4, 65, 2, -31, 0, 99, 2, 83, 782, 1
      PROCpancakesort(test())
      FOR i% = 0 TO 9
        PRINT test(i%) ;
      NEXT
      PRINT
      END

      DEF PROCpancakesort(a())
      LOCAL i%, j%, m%
      FOR i% = DIM(a(),1)+1 TO 2 STEP -1
        m% = 0
        FOR j% = 1 TO i%-1
          IF a(j%) > a(m%) m% = j%
        NEXT
        m% += 1
        IF m% < i% THEN
          IF m% > 1 PROCflip(a(), m%)
          PROCflip(a(), i%)
        ENDIF
      NEXT
      ENDPROC

      DEF PROCflip(a(), n%)
      IF n% < 2 ENDPROC
      LOCAL i%
      n% -= 1
      FOR i% = 0 TO n% DIV 2
        SWAP a(i%), a(n%-i%)
      NEXT
      ENDPROC
```

'''Output:'''

```txt

       -31         0         1         2         2         4        65        83        99       782

```



## C

'''The function that sorts:'''

```c
int pancake_sort(int *list, unsigned int length)
{
    //If it's less than 2 long, just return it as sorting isn't really needed...
    if(length<2)
        return 0;

    int i,a,max_num_pos,moves;
    moves=0;

    for(i=length;i>1;i--)
    {
        //Find position of the max number in pos(0) to pos(i)
        max_num_pos=0;
        for(a=0;a<i;a++)
        {
            if(list[a]>list[max_num_pos])
                max_num_pos=a;
        }

        if(max_num_pos==i-1)
            //It's where it need to be, skip
            continue;


        //Get the found max number to the beginning of the list (unless it already is)
        if(max_num_pos)
        {
            moves++;
            do_flip(list, length, max_num_pos+1);
        }


        //And then move it to the end of the range we're working with (pos(0) to pos(i))
        moves++;
        do_flip(list, length, i);

        //Then everything above list[i-1] is sorted and don't need to be touched

    }

    return moves;
}
```


Where do_flip() is a simple function to flip a part of an array:

```c
void do_flip(int *list, int length, int num)
{
    int swap;
    int i=0;
    for(i;i<--num;i++)
    {
        swap=list[i];
        list[i]=list[num];
        list[num]=swap;
    }
}
```


'''Testing the function:'''

```c
int main(int argc, char **argv)
{
    //Just need some random numbers. I chose <100
    int list[9];
    int i;
    srand(time(NULL));
    for(i=0;i<9;i++)
        list[i]=rand()%100;


    //Print list, run code and print it again displaying number of moves
    printf("\nOriginal: ");
    print_array(list, 9);

    int moves = pancake_sort(list, 9, 1);

    printf("\nSorted: ");
    print_array(list, 9);
    printf("  - with a total of %d moves\n", moves);
}
```



## C++


```c
#include <algorithm>
#include <iostream>
#include <iterator>
#include <vector>

// pancake sort template (calls predicate to determine order)
template <typename BidIt, typename Pred>
void pancake_sort(BidIt first, BidIt last, Pred order)
{
    if (std::distance(first, last) < 2) return; // no sort needed

    for (; first != last; --last)
    {
        BidIt mid = std::max_element(first, last, order);
        if (mid == last - 1)
        {
            continue; // no flips needed
        }
        if (first != mid)
        {
            std::reverse(first, mid + 1); // flip element to front
        }
        std::reverse(first, last); // flip front to final position
    }
}

// pancake sort template (ascending order)
template <typename BidIt>
void pancake_sort(BidIt first, BidIt last)
{
    pancake_sort(first, last, std::less<typename std::iterator_traits<BidIt>::value_type>());
}

int main()
{
    std::vector<int> data;
    for (int i = 0; i < 20; ++i)
    {
        data.push_back(i); // generate test data
    }
    std::random_shuffle(data.begin(), data.end()); // scramble data

    std::copy(data.begin(), data.end(), std::ostream_iterator<int>(std::cout, " "));
    std::cout << "\n";

    pancake_sort(data.begin(), data.end()); // ascending pancake sort

    std::copy(data.begin(), data.end(), std::ostream_iterator<int>(std::cout, " "));
    std::cout << "\n";
}
```
Output:
```txt
4 10 11 15 14 16 17 1 6 9 3 7 19 2 0 12 5 18 13 8
0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
```


## C#
<lang C sharp|C#>
public static class PancakeSorter
{
    public static void Sort<T>(List<T> list) where T : IComparable
    {
        if (list.Count < 2)
        {
            return;
        }
        int i, a, max_num_pos;
        for (i = list.Count; i > 1; i--)
        {
            max_num_pos = 0;
            for (a = 0; a < i; a++)
            {
                if (list[a].CompareTo(list[max_num_pos]) > 0)
                {
                    max_num_pos = a;
                }
            }
            if (max_num_pos == i - 1)
            {
                continue;
            }
            if (max_num_pos > 0)
            {
                Flip(list, list.Count, max_num_pos + 1);
            }
            Flip(list, list.Count, i);
        }
        return;
    }
    private static void Flip<T>(List<T> list, int length, int num)
    {
        for (int i = 0; i < --num; i++)
        {
            T swap = list[i];
            list[i] = list[num];
            list[num] = swap;
        }
    }
}

```



## Clojure


```clojure

(defn pancake-sort
  [arr]
  (if (= 1 (count arr))
    arr
    (when-let [mx (apply max arr)]
      (let [tk    (split-with #(not= mx %) arr)
            tail  (second tk)
            torev (concat (first tk) (take 1 tail))
            head  (reverse torev)]
        (cons mx (pancake-sort (concat (drop 1 head) (drop 1 tail))))))))

```



## Common Lisp


```lisp
(defun pancake-sort (seq)
  "A destructive version of Pancake Sort that works with either lists or arrays of numbers."
  (defun flip (lst index)
    (setf (subseq lst 0 index) (reverse (subseq lst 0 index))))
  (loop with lst = (coerce seq 'list)
	for i from (length lst) downto 2
	for index = (position (apply #'max (subseq lst 0 i)) lst)
	do (unless (= index 0)
	     (flip lst (1+ index)))
	(flip lst i)
	finally (return (coerce lst (type-of seq)))))
```

Output:

```lisp
CL-USER> (pancake-sort '(6 7 8 9 2 5 3 4 1))  ;list
(1 2 3 4 5 6 7 8 9)
CL-USER> (pancake-sort #(6 7 8 9 2 5 3 4 1))  ;array
#(1 2 3 4 5 6 7 8 9)
CL-USER> (pancake-sort #(6.5 7.5 8 9 2 5 3 4 1.0))  ;array with integer and floating point values
#(1.0 2 3 4 5 6.5 7.5 8 9)
```



## D

```d
import std.stdio, std.algorithm;

void pancakeSort(bool tutor=false, T)(T[] data) {
    foreach_reverse (immutable i; 2 .. data.length + 1) {
        immutable maxIndex = i - data[0 .. i].minPos!q{a > b}.length;
        if (maxIndex + 1 != i) {
            if (maxIndex != 0) {
                static if (tutor)
                    writeln("With: ", data, " doflip ", maxIndex + 1);
                data[0 .. maxIndex + 1].reverse();
            }

            static if (tutor)
                writeln("With: ", data, " doflip ", i);
            data[0 .. i].reverse();
        }
    }
}

void main() {
    auto data = "769248135".dup;
    data.pancakeSort!true;
    data.writeln;
}
```

```txt
With: 769248135 doflip 3
With: 967248135 doflip 9
With: 531842769 doflip 4
With: 813542769 doflip 8
With: 672453189 doflip 2
With: 762453189 doflip 7
With: 135426789 doflip 3
With: 531426789 doflip 5
With: 241356789 doflip 2
With: 421356789 doflip 4
With: 312456789 doflip 3
With: 213456789 doflip 2
123456789
```



## Eiffel


```Eiffel

class
	PANCAKE_SORT [G -> COMPARABLE]

feature {NONE}

	arraymax (array: ARRAY [G]; upper: INTEGER): INTEGER
			--- Max item of 'array' between index 1 and 'upper'.
		require
			upper_index_positive: upper >= 0
			array_not_void: array /= Void
		local
			i: INTEGER
			cur_max: G
		do
			from
				i := 1
				cur_max := array.item (i)
				Result := i
			until
				i + 1 > upper
			loop
				if array.item (i + 1) > cur_max then
					cur_max := array.item (i + 1)
					Result := i + 1
				end
				i := i + 1
			end
		ensure
			Index_positive: Result > 0
		end

	reverse_array (ar: ARRAY [G]; upper: INTEGER): ARRAY [G]
			-- Array reversed from index one to upper.
		require
			upper_positive: upper > 0
			ar_not_void: ar /= Void
		local
			i, j: INTEGER
			new_array: ARRAY [G]
		do
			create Result.make_empty
			Result.deep_copy (ar)
			from
				i := 1
				j := upper
			until
				i > j
			loop
				Result [i] := ar [j]
				Result [j] := ar [i]
				i := i + 1
				j := j - 1
			end
		ensure
			same_length: ar.count = Result.count
		end

	sort (ar: ARRAY [G]): ARRAY [G]
			-- Sorted array in ascending order.
		local
			i: INTEGER
		do
			create Result.make_empty
			Result.deep_copy (ar)
			from
				i := ar.count
			until
				i = 1
			loop
				Result := reverse_array (reverse_array (Result, arraymax (Result, i)), i)
				i := i - 1
			end
		ensure
			same_length: ar.count = Result.count
			Result_sorted: is_sorted (Result)
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

feature

	pancake_sort (ar: ARRAY [G]): ARRAY [G]
		do
			Result := sort (ar)
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
			test := <<1, 27, 32, 99, 1, -7, 3, 5>>
			create sorter
			io.put_string ("Unsorted: ")
			across
				test as ar
			loop
				io.put_string (ar.item.out + " ")
			end
			io.put_string ("%NSorted: ")
			test := sorter.pancake_sort(test)
			across
				test as ar
			loop
				io.put_string (ar.item.out + " ")
			end
		end

	test: ARRAY [INTEGER]

	sorter: PANCAKE_SORT[INTEGER]

end

```

```txt

Unsorted: 1 27 32 99 1 -7 3 5
Sorted: -7 1 1 3 5 27 32 99

```


## Elena

ELENA 4.1 :
```elena
import extensions;

extension op
{
    pancakeSort()
    {
        var list := self.clone();

        int i := list.Length;

        if (i < 2) { ^ self };

        while (i > 1)
        {
            int max_num_pos := 0;
            int a := 0;
            while (a < i)
            {
                if (list[a] > list[max_num_pos])
                {
                    max_num_pos := a
                };

                a += 1
            };

            if (max_num_pos == i - 1)
            {
            }
            else
            {
                if (max_num_pos > 0)
                {
                    list.flip(list.Length, max_num_pos + 1)
                };

                list.flip(list.Length, i)
            };
            i -= 1
        };

        ^ list
    }

    flip(int length, int num)
    {
        int i := 0;
        int count := num - 1;
        while (i < count)
        {
            var swap := self[i];
            self[i] := self[count];
            self[count] := swap;

            i += 1;
            count -= 1
        }
    }
}

public program()
{
    var list := new int[]::(6, 7, 8, 9, 2, 5, 3, 4, 1);

    console.printLine("before:", list.asEnumerable());
    console.printLine("after :", list.pancakeSort().asEnumerable())
}
```

```txt

before:6,7,8,9,2,5,3,4,1
after :1,2,3,4,5,6,7,8,9

```



## Elixir


```elixir
defmodule Sort do
  def pancake_sort(list) when is_list(list), do: pancake_sort(list, length(list))

  defp pancake_sort(list, 0), do: list
  defp pancake_sort(list, limit) do
    index = search_max(list, limit)
    flip(list, index) |> flip(limit) |> pancake_sort(limit-1)
  end

  defp search_max([h | t], limit), do: search_max(t, limit, 2, h, 1)

  defp search_max(_, limit, index, _, max_index) when limit<index, do: max_index
  defp search_max([h | t], limit, index, max, max_index) do
    if h > max, do:   search_max(t, limit, index+1, h, index),
                else: search_max(t, limit, index+1, max, max_index)
  end

  defp flip(list, n), do: flip(list, n, [])

  defp flip(list, 0, reverse), do: reverse ++ list
  defp flip([h | t], n, reverse) do
    flip(t, n-1, [h | reverse])
  end
end

IO.inspect list = Enum.shuffle(1..9)
IO.inspect Sort.pancake_sort(list)
```


```txt

[3, 7, 2, 8, 6, 4, 9, 1, 5]
[1, 2, 3, 4, 5, 6, 7, 8, 9]

```



## Euphoria


```euphoria
function flip(sequence s, integer n)
    object temp
    for i = 1 to n/2 do
        temp = s[i]
        s[i] = s[n-i+1]
        s[n-i+1] = temp
    end for
    return s
end function

function pancake_sort(sequence s)
    integer m
    for i = length(s) to 2 by -1 do
        m = 1
        for j = 2 to i do
            if compare(s[j], s[m]) > 0 then
                m = j
            end if
        end for

        if m < i then
            if m > 1 then
                s = flip(s,m)
            end if
            s = flip(s,i)
        end if
    end for
    return s
end function

constant s = rand(repeat(100,10))

? s
? pancake_sort(s)
```


Output:

```txt
{24,32,100,15,8,34,50,79,46,52}
{8,15,24,32,34,46,50,52,79,100}

```


=={{header|F_Sharp|F#}}==

```fsharp
open System

let show data = data |> Array.iter (printf "%d ") ; printfn ""
let split (data: int[]) pos = data.[0..pos], data.[(pos+1)..]

let flip items pos =
    let lower, upper = split items pos
    Array.append (Array.rev lower) upper

let pancakeSort items =
    let rec loop data limit =
        if limit <= 0 then data
        else
            let lower, upper = split data limit
            let indexOfMax = lower |> Array.findIndex ((=) (Array.max lower))
            let partialSort = Array.append (flip lower indexOfMax |> Array.rev) upper
            loop partialSort (limit-1)

    loop items ((Array.length items)-1)
```

Usage: pancakeSort [|31; 41; 59; 26; 53; 58; 97; 93; 23; 84|] |> show

Output:

```txt

  23 26 31 41 53 58 59 84 93 97

```



## Fortran

```fortran
program Pancake_Demo
  implicit none

  integer :: list(8) = (/ 1, 4, 7, 2, 5, 8, 3, 6 /)

  call Pancake_sort(list)

contains

subroutine Pancake_sort(a)

  integer, intent(in out) :: a(:)
  integer :: i, maxpos

  write(*,*) a
  do i = size(a), 2, -1

! Find position of max number between index 1 and i
    maxpos = maxloc(a(1:i), 1)

! is it in the correct position already?
    if (maxpos == i) cycle

! is it at the beginning of the array? If not flip array section so it is
    if (maxpos /= 1) then
      a(1:maxpos) = a(maxpos:1:-1)
      write(*,*) a
    end if

! Flip array section to get max number to correct position
    a(1:i) = a(i:1:-1)
    write(*,*) a
  end do

end subroutine

end program Pancake_Demo
```

Output:

```txt

            1           4           7           2           5           8           3           6
            8           5           2           7           4           1           3           6
            6           3           1           4           7           2           5           8
            7           4           1           3           6           2           5           8
            5           2           6           3           1           4           7           8
            6           2           5           3           1           4           7           8
            4           1           3           5           2           6           7           8
            5           3           1           4           2           6           7           8
            2           4           1           3           5           6           7           8
            4           2           1           3           5           6           7           8
            3           1           2           4           5           6           7           8
            2           1           3           4           5           6           7           8
            1           2           3           4           5           6           7           8

```


## FreeBASIC


```freebasic
' version 11-04-2017
' compile with: fbc -s console
' for boundry checks on array's compile with: fbc -s console -exx

' direction = 1, (default) sort ascending
' direction <> 1 sort descending
' show = 0, (default) do not show sorting
' show <> 0, show sorting
Sub pancake_sort(a() As Long,direction As Long = 1, show As Long = 0)
    ' array's can have subscript range from -2147483648 to +2147483647
    Dim As Long i, j, n
    Dim As Long lb = LBound(a)
    Dim As Long ub = UBound(a)

    If show <> 0 Then ' show initial state
        For j = lb To ub
            Print Using "####"; a(j);
        Next
        Print
    End If

    For i = ub To lb +1 Step -1

        n = lb
        For j = lb +1 To i
            If direction = 1 Then
                If a(n) < a(j) Then n = j
            Else
                If a(n) > a(j) Then n = j
            End If
        Next

        If n < i Then
            If n > lb Then
                For j = lb To lb + ((n - lb) \ 2)
                    Swap a(j), a(lb + n - j)
                Next

                If show <> 0 Then
                    For j = lb To ub
                        Print Using "####"; a(j);
                    Next
                    Print
                End If

            End If

            For j = lb To lb + ((i - lb) \ 2)
                Swap a(j), a(lb + i - j)
            Next

            If show <> 0 Then
                For j = lb To ub
                    Print Using "####"; a(j);
                Next
                Print
            End If

        End If
    Next

End Sub

' ------=< MAIN >=------

Dim As Long i, array(-7 To 7)
Dim As Long lb = LBound(array)
Dim As Long ub = UBound(array)

Randomize Timer
For i = lb To ub : array(i) = i : Next
For i = lb To ub ' little shuffle
    Swap array(i), array(Int(Rnd * (ub - lb +1) + lb))
Next

Print "unsorted  ";
For i = lb To ub
    Print Using "####"; array(i);
Next
Print : Print

pancake_sort(array())

Print "  sorted  ";
For i = lb To ub
    Print Using "####"; array(i);
Next

Print : Print
Dim As Long l(10 To ...) = {0, -30, 20, -10, 0, 10, -20}

pancake_sort(l(),0,1)   ' sort array l, ascending and show process

Print : Print "  sorted  l()";
For i = LBound(l) To UBound(l)
    Print Using "####"; l(i);
Next
Print

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
unsorted    -1  -4   1   6   7   5   2  -3   4  -5  -2  -6   0   3  -7

  sorted    -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6   7

   0 -30  20 -10   0  10 -20
 -30   0  20 -10   0  10 -20
 -20  10   0 -10  20   0 -30
   0  20 -10   0  10 -20 -30
 -10  20   0   0  10 -20 -30
  10   0   0  20 -10 -20 -30
   0  10   0  20 -10 -20 -30
  20   0  10   0 -10 -20 -30
   0  20  10   0 -10 -20 -30
  10  20   0   0 -10 -20 -30
  20  10   0   0 -10 -20 -30

  sorted  l()  20  10   0   0 -10 -20 -30
```



## Go


```go
package main

import "fmt"

func main() {
    list := pancake{31, 41, 59, 26, 53, 58, 97, 93, 23, 84}
    fmt.Println("unsorted:", list)

    list.sort()
    fmt.Println("sorted!  ", list)
}

type pancake []int

func (a pancake) sort() {
    for uns := len(a) - 1; uns > 0; uns-- {
        // find largest in unsorted range
        lx, lg := 0, a[0]
        for i := 1; i <= uns; i++ {
            if a[i] > lg {
                lx, lg = i, a[i]
            }
        }
        // move to final position in two flips
        a.flip(lx)
        a.flip(uns)
    }
}

func (a pancake) flip(r int) {
    for l := 0; l < r; l, r = l+1, r-1 {
        a[l], a[r] = a[r], a[l]
    }
}
```

Output:

```txt

unsorted: [31 41 59 26 53 58 97 93 23 84]
sorted!   [23 26 31 41 53 58 59 84 93 97]

```



## Groovy

This formulation of the pancake sort achieves stability by picking the last index (rather than, say, the first) in the remaining sublist that matches the max value of the remaining sublist. Performance is enhanced somewhat by not flipping if the ''flipPoint'' is already at the end of the remaining sublist.

```groovy
def makeSwap = { a, i, j = i+1 -> print "."; a[[j,i]] = a[[i,j]] }

def flip = { list, n -> (0..<((n+1)/2)).each { makeSwap(list, it, n-it) } }

def pancakeSort = { list ->
    def n = list.size()
    (1..<n).reverse().each { i ->
        def max = list[0..i].max()
        def flipPoint = (i..0).find{ list[it] == max }
        if (flipPoint != i) {
            flip(list, flipPoint)
            flip(list, i)
        }
    }
    list
}
```


Test:

```groovy
println (pancakeSort([23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78,4]))
println (pancakeSort([88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70,12,7,1]))
println ()
println (pancakeSort([10, 10.0, 10.00, 1]))
println (pancakeSort([10, 10.00, 10.0, 1]))
println (pancakeSort([10.0, 10, 10.00, 1]))
println (pancakeSort([10.0, 10.00, 10, 1]))
println (pancakeSort([10.00, 10, 10.0, 1]))
println (pancakeSort([10.00, 10.0, 10, 1]))
```

The use of decimals and integers that compare as equal demonstrates, but of course not '''prove''', that the sort is stable.

Output:

```txt
..........................................................................................................................................[4, 12, 14, 23, 24, 24, 31, 35, 38, 46, 51, 57, 57, 58, 76, 78, 89, 92, 95, 97, 99]
............................................................................................................................................................................................................[0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]

...[1, 10, 10.0, 10.00]
...[1, 10, 10.00, 10.0]
...[1, 10.0, 10, 10.00]
...[1, 10.0, 10.00, 10]
...[1, 10.00, 10, 10.0]
...[1, 10.00, 10.0, 10]
```



## Haskell


```haskell
import Data.List
import Control.Arrow
import Control.Monad
import Data.Maybe

dblflipIt :: (Ord a) => [a] -> [a]
dblflipIt = uncurry ((reverse.).(++)). first reverse
  . ap (flip splitAt) (succ. fromJust. (elemIndex =<< maximum))

dopancakeSort :: (Ord a) => [a] -> [a]
dopancakeSort xs = dopcs (xs,[]) where
  dopcs ([],rs) = rs
  dopcs ([x],rs) = x:rs
  dopcs (xs,rs) = dopcs $ (init &&& (:rs).last ) $ dblflipIt xs
```

Example:

```haskell
*Main>  dopancakeSort [3,2,1,0,2]
[0,1,2,2,3]
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()                                        #: demonstrate various ways to sort a list and string
   demosort(pancakesort,[3, 14, 1, 5, 9, 2, 6, 3],"qwerty")
   pancakeflip := pancakeflipshow                       # replace pancakeflip procedure with a variant that displays each flip
   pancakesort([3, 14, 1, 5, 9, 2, 6, 3])
end

procedure pancakesort(X,op)                             #: return sorted list ascending(or descending)
local i,m

   op := sortop(op,X)                                   # select how and what we sort

   every i := *X to 2 by -1 do {                        # work back to front
      m := 1
      every j := 2 to i do
         if op(X[m],X[j]) then m := j                   # find X that belongs @i high (or low)
      if i ~= m then {                                  # not already in-place
         X := pancakeflip(X,m)                          # . bring max (min) to front
         X := pancakeflip(X,i)                          # . unsorted portion of stack
         }
      }
   return X
end

procedure pancakeflip(X,tail)                           #: return X[1:tail] flipped
local i

   i := 0
   tail := integer(\tail|*X) + 1   | runerr(101,tail)
   while X[(i +:= 1) < (tail -:= 1)] :=: X[i]              # flip
   return X
end

procedure pancakeflipshow(X,tail)                       #: return X[1:tail] flipped  (and display)
local i

   i := 0
   tail := integer(\tail|*X) + 1   | runerr(101,tail)
   while X[(i +:= 1) < (tail -:= 1)] :=: X[i]              # flip
   every writes("    ["|right(!X,4)|" ]\n")                # show X
   return X
end
```


Note: This example relies on [[Sorting_algorithms/Bubble_sort#Icon| the supporting procedures 'sortop', and 'demosort' in Bubble Sort]]. The full demosort exercises the named sort of a list with op = "numeric", "string", ">>" (lexically gt, descending),">" (numerically gt, descending), a custom comparator, and also a string.

Abbreviated sample output:
```txt
Sorting Demo using procedure pancakesort
  on list : [ 3 14 1 5 9 2 6 3 ]
    with op = &null:         [ 1 2 3 3 5 6 9 14 ]   (0 ms)
  ...
  on string : "qwerty"
    with op = &null:         "eqrtwy"   (0 ms)
```


The output below shows the flipping:
```txt

     [  14   3   1   5   9   2   6   3 ]
     [   3   6   2   9   5   1   3  14 ]
     [   9   2   6   3   5   1   3  14 ]
     [   3   1   5   3   6   2   9  14 ]
     [   6   3   5   1   3   2   9  14 ]
     [   2   3   1   5   3   6   9  14 ]
     [   5   1   3   2   3   6   9  14 ]
     [   3   2   3   1   5   6   9  14 ]
     [   3   2   3   1   5   6   9  14 ]
     [   1   3   2   3   5   6   9  14 ]
     [   3   1   2   3   5   6   9  14 ]
     [   2   1   3   3   5   6   9  14 ]
     [   2   1   3   3   5   6   9  14 ]
     [   1   2   3   3   5   6   9  14 ]
```



## J

```J
flip=: C.~ C.@i.@-
unsorted=: #~ 1 , [: >./\. 2 >/\ ]
FlDown=: flip 1 + (i. >./)@unsorted
FlipUp=: flip 1 >. [:+/>./\&|.@(< {.)

pancake=: FlipUp@FlDown^:_
```


Example use:


```J
   (,:pancake) ?~9
1 0 8 7 4 6 3 5 2
0 1 2 3 4 5 6 7 8
```


See the [[Talk:Sorting_algorithms/Pancake_sort#J_implementation|discussion page]] for illustrations of the other words.


## Java



```java

public class PancakeSort
{
   int[] heap;

   public String toString() {
      String info = "";
      for (int x: heap)
         info += x + " ";
      return info;
   }

   public void flip(int n) {
      for (int i = 0; i < (n+1) / 2; ++i) {
         int tmp = heap[i];
         heap[i] = heap[n-i];
         heap[n-i] = tmp;
      }
      System.out.println("flip(0.." + n + "): " + toString());
   }

   public int[] minmax(int n) {
      int xm, xM;
      xm = xM = heap[0];
      int posm = 0, posM = 0;

      for (int i = 1; i < n; ++i) {
         if (heap[i] < xm) {
            xm = heap[i];
            posm = i;
         }
         else if (heap[i] > xM) {
            xM = heap[i];
            posM = i;
         }
      }
      return new int[] {posm, posM};
   }

   public void sort(int n, int dir) {
      if (n == 0) return;

      int[] mM = minmax(n);
      int bestXPos = mM[dir];
      int altXPos = mM[1-dir];
      boolean flipped = false;

      if (bestXPos == n-1) {
         --n;
      }
      else if (bestXPos == 0) {
         flip(n-1);
         --n;
      }
      else if (altXPos == n-1) {
         dir = 1-dir;
         --n;
         flipped = true;
      }
      else {
         flip(bestXPos);
      }
      sort(n, dir);

      if (flipped) {
         flip(n);
      }
   }

   PancakeSort(int[] numbers) {
      heap = numbers;
      sort(numbers.length, 1);
   }

   public static void main(String[] args) {
      int[] numbers = new int[args.length];
      for (int i = 0; i < args.length; ++i)
         numbers[i] = Integer.valueOf(args[i]);

      PancakeSort pancakes = new PancakeSort(numbers);
      System.out.println(pancakes);
   }
}
```


Example:

```bash
$ java PancakeSort  1 2 5 4 3 10 9 8 7
flip(0..5): 10 3 4 5 2 1 9 8 7
flip(0..8): 7 8 9 1 2 5 4 3 10
flip(0..2): 9 8 7 1 2 5 4 3 10
flip(0..7): 3 4 5 2 1 7 8 9 10
flip(0..2): 5 4 3 2 1 7 8 9 10
flip(0..4): 1 2 3 4 5 7 8 9 10
1 2 3 4 5 7 8 9 10

$ java PancakeSort  6 7 2 1 8 9 5 3 4
flip(0..5): 9 8 1 2 7 6 5 3 4
flip(0..8): 4 3 5 6 7 2 1 8 9
flip(0..1): 3 4 5 6 7 2 1 8 9
flip(0..4): 7 6 5 4 3 2 1 8 9
flip(0..6): 1 2 3 4 5 6 7 8 9
1 2 3 4 5 6 7 8 9
```



## JavaScript


```javascript
Array.prototype.pancake_sort = function () {
    for (var i = this.length - 1; i >= 1; i--) {
        // find the index of the largest element not yet sorted
        var max_idx = 0;
        var max = this[0];
        for (var j = 1; j <= i; j++) {
            if (this[j] > max) {
                max = this[j];
                max_idx = j;
            }
        }

        if (max_idx == i)
            continue; // element already in place

        var new_slice;

        // flip this max element to index 0
        if (max_idx > 0) {
            new_slice = this.slice(0, max_idx+1).reverse();
            for (var j = 0; j <= max_idx; j++)
                this[j] = new_slice[j];
        }

        // then flip the max element to its place
        new_slice = this.slice(0, i+1).reverse();
        for (var j = 0; j <= i; j++)
            this[j] = new_slice[j];
    }
    return this;
}
ary = [7,6,5,9,8,4,3,1,2,0]
sorted = ary.concat().pancake_sort();
```



## jq

This version skips the pair of flips if the focal item is already in place.

```jq
def pancakeSort:

  def flip(i):
    . as $in | ($in[0:i+1]|reverse) + $in[i+1:] ;

  # If input is [] then return null
  def index_of_max:
    . as $in
    | reduce range(1; length) as $i
        # state: [ix, max]
        ( [ 0, $in[0] ];
          if $in[$i] > .[1] then [ $i, $in[$i] ] else . end )
    | .[0] ;

  reduce range(0; length) as $iup
    (.;
     (length - $iup - 1) as $i
     | (.[0:$i+1] | index_of_max) as $max
     # flip about $max and then about $i unless $i == $max
     | if ($i == $max) then .
       else flip($max) | flip($i)
       end ) ;
```

'''Example''':

```jq
[range(0;2), null, 1.0, 0.5, [1], [2], {"b":1}, {"a":2}, range(2;4)]
  | pancakeSort
```


```sh
$ jq -M -c -n -f pancake_sort.jq
[null,0,0.5,1,1,2,3,[1],[2],{"a":2},{"b":1}]
```



## Julia

```julia
function pancakesort!(arr::Vector{<:Real})
    len = length(arr)
    if len < 2 return arr end
    for i in len:-1:2
        j = indmax(arr[1:i])
        if i == j continue end
        arr[1:j] = reverse(arr[1:j])
        arr[1:i] = reverse(arr[1:i])
    end
    return arr
end

v = rand(-10:10, 10)
println("# unordered: $v\n -> ordered: ", pancakesort!(v))
```


```txt
# unordered: [0, -9, -8, 2, -7, 8, 6, -2, -8, 3]
 -> ordered: [-9, -8, -8, -7, -2, 0, 2, 3, 6, 8]
```



## Kotlin


```scala
// version 1.1.2

class PancakeSort(private val a: IntArray) {
    init {
        for (n in a.size downTo 2) {  // successively reduce size of array by 1
            val index = indexOfMax(n) // find index of largest
            if (index != n - 1) {     // if it's not already at the end
                if (index > 0) {      // if it's not already at the beginning
                    flip(index)       // move largest to beginning
                    println("${a.contentToString()} after flipping first ${index + 1}")
                }
                flip(n - 1)           // move largest to end
                println("${a.contentToString()} after flipping first $n")
            }
        }
    }

    private fun indexOfMax(n: Int): Int {
        var index = 0
        for (i in 1 until n) {
            if (a[i] > a[index]) index = i
        }
        return index
    }

    private fun flip(index: Int) {
        var i = index
        var j = 0
        while (j < i) {
            val temp = a[j]
            a[j] = a[i]
            a[i] = temp
            j++
            i--
        }
    }
}

fun main(args: Array<String>) {
    val a = intArrayOf(7, 6, 9, 2, 4, 8, 1, 3, 5)
    println("${a.contentToString()} initially")
    PancakeSort(a)
}
```


```txt

[7, 6, 9, 2, 4, 8, 1, 3, 5] initially
[9, 6, 7, 2, 4, 8, 1, 3, 5] after flipping first 3
[5, 3, 1, 8, 4, 2, 7, 6, 9] after flipping first 9
[8, 1, 3, 5, 4, 2, 7, 6, 9] after flipping first 4
[6, 7, 2, 4, 5, 3, 1, 8, 9] after flipping first 8
[7, 6, 2, 4, 5, 3, 1, 8, 9] after flipping first 2
[1, 3, 5, 4, 2, 6, 7, 8, 9] after flipping first 7
[5, 3, 1, 4, 2, 6, 7, 8, 9] after flipping first 3
[2, 4, 1, 3, 5, 6, 7, 8, 9] after flipping first 5
[4, 2, 1, 3, 5, 6, 7, 8, 9] after flipping first 2
[3, 1, 2, 4, 5, 6, 7, 8, 9] after flipping first 4
[2, 1, 3, 4, 5, 6, 7, 8, 9] after flipping first 3
[1, 2, 3, 4, 5, 6, 7, 8, 9] after flipping first 2

```



## Lua


```Lua
-- Initialisation
math.randomseed(os.time())
numList = {step = 0, sorted = 0}

-- Create list of n random values
function numList:build (n)
    self.values = {}
    for i = 1, n do self.values[i] = math.random(-100, 100) end
end

-- Return boolean indicating whether the list is in order
function numList:isSorted ()
    for i = 2, #self.values do
        if self.values[i] < self.values[i - 1] then return false end
    end
    print("Finished!")
    return true
end

-- Display list of numbers on one line
function numList:show ()
    if self.step == 0 then
        io.write("Initial state:\t")
    else
        io.write("After step " .. self.step .. ":\t")
    end
    for _, v in ipairs(self.values) do io.write(v .. " ") end
    print()
end

-- Reverse n values from the left
function numList:reverse (n)
    local flipped = {}
    for i, v in ipairs(self.values) do
        if i > n then
            flipped[i] = v
        else
            flipped[i] = self.values[n + 1 - i]
        end
    end
    self.values = flipped
end

-- Perform one flip of a pancake sort
function numList:pancake ()
    local maxPos = 1
    for i = 1, #self.values - self.sorted do
        if self.values[i] > self.values[maxPos] then maxPos = i end
    end
    if maxPos == 1 then
        numList:reverse(#self.values - self.sorted)
        self.sorted = self.sorted + 1
    else
        numList:reverse(maxPos)
    end
    self.step = self.step + 1
end

-- Main procedure
numList:build(10)
numList:show()
repeat
    numList:pancake()
    numList:show()
until numList:isSorted()

```

```txt
Initial state:  -67 61 80 47 21 74 43 22 66 -66
After step 1:   80 61 -67 47 21 74 43 22 66 -66
After step 2:   -66 66 22 43 74 21 47 -67 61 80
After step 3:   74 43 22 66 -66 21 47 -67 61 80
After step 4:   61 -67 47 21 -66 66 22 43 74 80
After step 5:   66 -66 21 47 -67 61 22 43 74 80
After step 6:   43 22 61 -67 47 21 -66 66 74 80
After step 7:   61 22 43 -67 47 21 -66 66 74 80
After step 8:   -66 21 47 -67 43 22 61 66 74 80
After step 9:   47 21 -66 -67 43 22 61 66 74 80
After step 10:  22 43 -67 -66 21 47 61 66 74 80
After step 11:  43 22 -67 -66 21 47 61 66 74 80
After step 12:  21 -66 -67 22 43 47 61 66 74 80
After step 13:  22 -67 -66 21 43 47 61 66 74 80
After step 14:  21 -66 -67 22 43 47 61 66 74 80
After step 15:  -67 -66 21 22 43 47 61 66 74 80
Finished!
```



## Maple


```Maple
flip := proc(arr, i)
	local start, temp, icopy;
	temp, start, icopy := 0,1,i:
	while (start < icopy) do
		arr[start], arr[icopy] := arr[icopy], arr[start]:
		start:=start+1:
		icopy:=icopy-1:
	end do:
end proc:
findMax := proc(arr, i)
	local Max, j:
	Max := 1:
	for j from 1 to i do
		if arr[j] > arr[Max] then Max := j: end if:
	end do:
	return Max:
end proc:
pancakesort := proc(arr)
	local len,i,Max;
	len := numelems(arr):
	for i from len to 2 by -1 do
		print(arr):
		Max := findMax(arr, i):
		if (not Max = i) then
			flip(arr, Max):
			flip(arr, i):
		end if:
	end do:
	print(arr);
end proc:
```

Input: arr := Array([17,3,72,0,36,2,3,8,40,1]):

```txt
               [17, 3, 72, 0, 36, 2, 3, 8, 40, 1]
               [1, 40, 8, 3, 2, 36, 0, 17, 3, 72]
               [3, 17, 0, 36, 2, 3, 8, 1, 40, 72]
               [1, 8, 3, 2, 3, 17, 0, 36, 40, 72]
               [0, 1, 8, 3, 2, 3, 17, 36, 40, 72]
               [3, 2, 3, 0, 1, 8, 17, 36, 40, 72]
               [1, 0, 3, 2, 3, 8, 17, 36, 40, 72]
               [2, 1, 0, 3, 3, 8, 17, 36, 40, 72]
               [0, 1, 2, 3, 3, 8, 17, 36, 40, 72]
               [0, 1, 2, 3, 3, 8, 17, 36, 40, 72]
```



## Mathematica


```Mathematica
LMaxPosition[ a_, n_ ] := Part[Position[a[[;;n]],Max[a[[;;n]]]],1,1]

SetAttributes[Flip,HoldFirst]; Flip[a_] := Set[a,Reverse[a]]

pancakeSort[a_] : = For[n = Length[a], n > 1, n--,
 If[LMaxPosition[a,n] < n,
  Flip[a[[;;LMaxPosition[a,n]]]]; Print[a];
  Flip[a[[;;n]]]; Print[a];
 ];
];
```



```txt
(* each major sort step is printed in example usage *)
pancakeSort[{6, 7, 8, 9, 2, 5, 3, 4, 1}]

{9,8,7,6,2,5,3,4,1}
{1,4,3,5,2,6,7,8,9}
{5,3,4,1,2,6,7,8,9}
{2,1,4,3,5,6,7,8,9}
{4,1,2,3,5,6,7,8,9}
{3,2,1,4,5,6,7,8,9}
{3,2,1,4,5,6,7,8,9}
{1,2,3,4,5,6,7,8,9}
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function list = pancakeSort(list)

    for i = (numel(list):-1:2)

        minElem = list(i);
        minIndex = i;

        %Find the min element in the current subset of the list
        for j = (i:-1:1)
            if list(j) <= minElem
                minElem = list(j);
                minIndex = j;
            end
        end

        %If the element is already in the correct position don't flip
        if i ~= minIndex

            %First flip flips the min element in the stack to the top
            list(minIndex:-1:1) = list(1:minIndex);

            %Second flip flips the min element into the correct position in
            %the stack
            list(i:-1:1) = list(1:i);

        end
    end %for
end %pancakeSort
```


Sample Usage:

```MATLAB>>
 pancakeSort([4 3 1 5 6 2])

ans =

     6     5     4     3     2     1
```



## MAXScript


```MAXScript
fn flipArr arr index =
(
	local new = #()
	for i = index to 1 by -1 do
	(
		append new arr[i]
	)
	join new (for i in (index+1) to arr.count collect arr[i])
	return new
)

fn pancakeSort arr =
(
	if arr.count < 2 then return arr
	else
	(
		for i = arr.count to 1 by -1 do
		(
			local newArr = for n in 1 to i collect arr[n]
			local oldArr = for o in (i+1) to arr.count collect arr[o]
			local maxIndices = for m in 1 to (newArr.count) where (newArr[m] == amax newArr) collect m
			local lastMaxIndex = maxIndices[maxIndices.count]
			newArr = flipArr newArr lastMaxIndex
			newArr = flipArr newArr newArr.count
			arr = join newArr oldArr
		)
		return arr
	)
)
```

Output:

```MAXScript

a = for i in 1 to 15 collect random 0 20
#(8, 13, 2, 0, 10, 8, 1, 15, 4, 7, 6, 9, 11, 3, 5)
pancakeSort a
#(0, 1, 2, 3, 4, 5, 6, 7, 8, 8, 9, 10, 11, 13, 15)

```



## NetRexx

Sorts integers, decimal numbers and strings because they're all the same to NetRexx.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import java.util.List

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method pancakeSort(tlist = List, debug = (1 == 0)) private static returns List

  if tlist.size() > 1 then do
    loop i_ = tlist.size() by -1 while i_ > 1
      maxPos = 0
      loop a_ = 0 while a_ < i_
        if Rexx tlist.get(a_) > Rexx tlist.get(maxPos) then maxPos = a_
        end a_
      if maxPos = i_ - 1 then iterate i_
      if maxPos > 0 then pancakeFlip(tlist, maxPos + 1, debug)
      pancakeFlip(tlist, i_, debug)
      end i_
    end
  return tlist

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method pancakeFlip(tlist = List, offset, debug = (1 == 0)) private static returns List
  z_ = offset - 1
  pl = 3
  if debug then do
    plx = offset.length()
    if plx > pl then pl = plx
    say '  flip{1-'offset.right(pl, 0)'} Before:' tlist
    end
  loop i_ = 0 while i_ < z_
    Collections.swap(tlist, i_, z_)
    z_ = z_ - 1
    end i_
  if debug then do
    say '  flip{1-'offset.right(pl, 0)'}  After:' tlist
    end
  return tlist

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static

  isTrue  = (1 == 1)
  isFalse = \isTrue

  parse arg debug .
  if '-debug'.abbrev(debug.lower(), 2) then debug = isTrue
  else                                      debug = isFalse

  lists = sampleData()
  loop il = 1 to lists[0]
    clist = words2list(lists[il])
    say ' Input:' clist
    say 'Output:' pancakeSort(clist, debug)
    say
    end il

  return
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method sampleData() private static
  lists = ''
  i_ = 0
  i_ = i_ + 1; lists[0] = i_; lists[i_] = '1 4 3 5 2 9 8 7 6'
  i_ = i_ + 1; lists[0] = i_; lists[i_] = '10 -9 8 -7 6 -5 4 -3 2 -1 0 -10 9 -8 7 -6 5 -4 3 -2 1'
  i_ = i_ + 1; lists[0] = i_; lists[i_] = '88 18 31 44 4 0 8 81 14 78 20 76 84 33 73 75 82 5 62 70 12 7 1'
  i_ = i_ + 1; lists[0] = i_; lists[i_] = '10 10.0 10.00 1 -10.0 10. -1'
  i_ = i_ + 1; lists[0] = i_; lists[i_] = 'To be or not to be that is the question'
  i_ = i_ + 1; lists[0] = i_; lists[i_] = '1'
  return lists
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method words2list(wordlist) private static returns List

  clist = ArrayList()
  loop w_ = 1 to wordlist.words()
    clist.add(wordlist.word(w_))
    end w_

  return clist

```

```txt

 Input: [1, 4, 3, 5, 2, 9, 8, 7, 6]
Output: [1, 2, 3, 4, 5, 6, 7, 8, 9]

 Input: [10, -9, 8, -7, 6, -5, 4, -3, 2, -1, 0, -10, 9, -8, 7, -6, 5, -4, 3, -2, 1]
Output: [-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

 Input: [88, 18, 31, 44, 4, 0, 8, 81, 14, 78, 20, 76, 84, 33, 73, 75, 82, 5, 62, 70, 12, 7, 1]
Output: [0, 1, 4, 5, 7, 8, 12, 14, 18, 20, 31, 33, 44, 62, 70, 73, 75, 76, 78, 81, 82, 84, 88]

 Input: [10, 10.0, 10.00, 1, -10.0, 10., -1]
Output: [-10.0, -1, 1, 10.00, 10.0, 10., 10]

 Input: [To, be, or, not, to, be, that, is, the, question]
Output: [be, be, is, not, or, question, that, the, to, To]

 Input: [1]
Output: [1]

```



## Nim


```nim
import algorithm

proc pancakeSort[T](list: var openarray[T]) =
  var length = list.len
  if length < 2: return

  var moves = 0

  for i in countdown(length, 2):
    var maxNumPos = 0
    for a in 0 .. <i:
      if list[a] > list[maxNumPos]:
        maxNumPos = a

    if maxNumPos == i - 1: continue

    if maxNumPos > 0:
      inc moves
      reverse(list, 0, maxNumPos)

    inc moves
    reverse(list, 0, i - 1)

var a = @[4, 65, 2, -31, 0, 99, 2, 83, 782]
pancakeSort a
echo a
```

Output:

```txt
@[-31, 0, 2, 2, 4, 65, 83, 99, 782]
```



## OCaml


```ocaml
let rec sorted = function
  | [] -> (true)
  | x::y::_ when x > y -> (false)
  | x::xs -> sorted xs

let rev_until_max li =
  let rec aux acc greater prefix suffix = function
  | x::xs when x > greater -> aux (x::acc) x acc xs xs
  | x::xs -> aux (x::acc) greater prefix suffix xs
  | [] -> (greater, (prefix @ suffix))
  in
  aux [] min_int [] li li

let pancake_sort li =
  let rec aux i li suffix =
    let greater, li = rev_until_max li in
    let suffix = greater :: suffix
    and li = List.rev li in
    if sorted li
    then (li @ suffix), i
    else aux (succ i) li suffix
  in
  aux 0 li []

let print_list li =
  List.iter (Printf.printf " %d") li;
  print_newline()

let make_rand_list n bound =
  let rec aux acc i =
    if i >= n then (acc)
    else aux ((Random.int bound)::acc) (succ i)
  in
  aux [] 0

let () =
  Random.self_init();
  let li = make_rand_list 8 100 in
  print_list li;
  let res, n = pancake_sort li in
  print_list res;
  Printf.printf " sorted in %d loops\n" n;
;;
```



## PARI/GP


```parigp
pancakeSort(v)={
  my(top=#v);
  while(top>1,
    my(mx=1,t);
    for(i=2,top,if(v[i]>v[mx], mx=i));
    if(mx==top, top--; next);
    for(i=1,mx\2,
      t=v[i];
      v[i]=v[mx+1-i];
      v[mx+1-i]=t
    );
    for(i=1,top\2,
      t=v[i];
      v[i]=v[top+1-i];
      v[top+1-i]=t
    );
    top--
  );
  v
};
```



## Pascal


```pascal
Program PancakeSort (output);

procedure flip(var b: array of integer; last: integer);

  var
    swap, i: integer;

  begin
    for i := low(b) to (last - low(b) - 1) div 2 do
    begin
      swap              := b[i];
      b[i]              := b[last-(i-low(b))];
      b[last-(i-low(b))] := swap;
    end;
  end;

procedure PancakeSort(var a: array of integer);

  var
    i, j, maxpos: integer;

  begin
    for i := high(a) downto low(a) do
    begin
// Find position of max number between beginning and i
      maxpos := i;
      for j := low(a) to i - 1 do
        if a[j] > a[maxpos] then
          maxpos := j;

// is it in the correct position already?
      if maxpos = i then
        continue;

// is it at the beginning of the array? If not flip array section so it is
      if maxpos <> low(a) then
        flip(a, maxpos);

// Flip array section to get max number to correct position
      flip(a, i);
    end;
  end;

var
  data: array of integer;
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
  PancakeSort(data);
  writeln('The data after sorting:');
  for i := low(data) to high(data) do
  begin
    write(data[i]:4);
  end;
  writeln;
end.
```

Output:

```txt
:>./PancakeSort
The data before sorting:
   3   1   3   2   4   0   2   6
The data after sorting:
   0   1   2   2   3   3   4   6

```



## Perl


```perl
sub pancake {
        my @x = @_;
        for my $idx (0 .. $#x - 1) {
                my $min = $idx;
                $x[$min] > $x[$_] and $min = $_           for $idx + 1 .. $#x;

                next if $x[$min] == $x[$idx];

                @x[$min .. $#x] = reverse @x[$min .. $#x] if $x[$min] != $x[-1];
                @x[$idx .. $#x] = reverse @x[$idx .. $#x];
        }
        @x;
}

my @a = map (int rand(100), 1 .. 10);
print "Before @a\n";
@a = pancake(@a);
print "After  @a\n";

```

Sample output:

```txt
Before 57 37 35 35 22 58 70 53 77 15
After  15 22 35 35 37 53 57 58 70 77
```



## Perl 6


```perl6
sub pancake_sort ( @a is copy ) {
    my $endpoint = @a.end;
    while $endpoint > 0 and not [<] @a {
        my $max_i = [0..$endpoint].max: { @a[$_] };
        my $max   = @a[$max_i];
        if @a[$endpoint] == $max {
            $endpoint-- while @a[$endpoint] == $max;
            next;
        }
        # @a[$endpoint] is not $max, so it needs flipping;
        # Flip twice if max is not already at the top.
        @a[0..$max_i]    .= reverse if $max_i != 0;
        @a[0..$endpoint] .= reverse;
        $endpoint--;
    }
    return @a;
}
my @data = 6, 7, 2, 1, 8, 9, 5, 3, 4;
say 'input  = ' ~ @data;
say 'output = ' ~ @data.&pancake_sort;

```


Output:
```txt
input  = 6 7 2 1 8 9 5 3 4
output = 1 2 3 4 5 6 7 8 9

```



## Phix

Copy of [[Sorting_algorithms/Pancake_sort#Euphoria|Euphoria]]

```Phix
function flip(sequence s, integer n)
    for i=1 to floor(n/2) do
        {s[i],s[n-i+1]} = {s[n-i+1],s[i]}
    end for
    return s
end function

function pancake_sort(sequence s)
integer m
    for i=length(s) to 2 by -1 do
        m = 1
        for j=2 to i do
            if s[j]>s[m] then
                m = j
            end if
        end for
        if m<i then
            if m>1 then
                s = flip(s,m)
            end if
            s = flip(s,i)
        end if
    end for
    return s
end function

constant s = shuffle(tagset(10))
? s
? pancake_sort(s)
```

```txt

{2,8,6,1,5,10,3,4,9,7}
{1,2,3,4,5,6,7,8,9,10}

```



## PicoLisp


```PicoLisp
(de pancake (Lst)
   (prog1 (flip Lst (index (apply max Lst) Lst))
      (for (L @  (cdr (setq Lst (cdr L)))  (cdr L))
         (con L (flip Lst (index (apply max Lst) Lst))) ) ) )
```

Output:

```txt
: (trace 'flip)
-> flip

: (pancake (6 7 2 1 8 9 5 3 4))
 flip : (6 7 2 1 8 9 5 3 4) 6
 flip = (9 8 1 2 7 6 5 3 4)
 flip : (8 1 2 7 6 5 3 4) 1
 flip = (8 1 2 7 6 5 3 4)
 flip : (1 2 7 6 5 3 4) 3
 flip = (7 2 1 6 5 3 4)
 flip : (2 1 6 5 3 4) 3
 flip = (6 1 2 5 3 4)
 flip : (1 2 5 3 4) 3
 flip = (5 2 1 3 4)
 flip : (2 1 3 4) 4
 flip = (4 3 1 2)
 flip : (3 1 2) 1
 flip = (3 1 2)
 flip : (1 2) 2
 flip = (2 1)
-> (9 8 7 6 5 4 3 2 1)
```



## PL/I


```PL/I

pancake_sort: procedure options (main); /* 23 April 2009 */
   declare a(10) fixed, (i, n, loc) fixed binary;

   a(1) = 3; a(2) = 9; a(3) = 2; a(4) = 7; a(5) = 10;
   a(6) = 1; a(7) = 8; a(8) = 5; a(9) = 4; a(10) = 6;

   n = hbound(A,1);
   put skip edit (A) (f(5));
   do i = 1 to n-1;
      loc = max(A, n);
      call flip (A, loc);
      call flip (A, n);
      n = n - 1;
      put skip edit (A) (f(5));
   end;

max: procedure (A, k) returns (fixed binary);
   declare A(*) fixed, k fixed binary;
   declare (i, maximum, loc) fixed binary;
   maximum = A(1); loc = 1;
   do i = 2 to k;
      if A(i) > maximum then do; maximum = A(i); loc = i; end;
   end;
   return (loc);
end max;

flip: procedure (A, k);
   declare A(*) fixed, k fixed binary;
   declare (i, t) fixed binary;
   do i = 1 to (k+1)/2;
      t = A(i); A(i) = A(k-i+1); A(k-i+1) = t;
   end;
end flip;

end pancake_sort;

```

Output:
<lang>
    3    9    2    7   10    1    8    5    4    6
    6    4    5    8    1    3    9    2    7   10
    7    2    6    4    5    8    1    3    9   10
    3    1    7    2    6    4    5    8    9   10
    5    4    6    2    3    1    7    8    9   10
    1    3    2    5    4    6    7    8    9   10
    4    1    3    2    5    6    7    8    9   10
    2    3    1    4    5    6    7    8    9   10
    1    2    3    4    5    6    7    8    9   10
    1    2    3    4    5    6    7    8    9   10

```



## PowerShell


```PowerShell
Function FlipPancake( [Object[]] $indata, $index = 1 )
{
	$data=$indata.Clone()
	$datal = $data.length - 1
	if( $index -gt 0 )
	{
		if( $datal -gt $index )
		{
			$first = $data[ $index..0 ]
			$last = $data[ ( $index + 1 )..$datal ]
			$data = $first + $last
		} else {
			$data = $data[ $index..0 ]
		}
	}
	$data
}

Function MaxIdx( [Object[]] $data )
{
	$data | ForEach-Object { $max = $data[ 0 ]; $i = 0; $maxi = 0 } { if( $_ -gt $max ) { $max = $_; $maxi = $i }; $i++ } { $maxi }
}

Function PancakeSort( [Object[]] $data, $index = 0 )
{
	"unsorted - $data"
	$datal = $data.length - 1
	if( $datal -gt 0 )
	{
		for( $i = $datal; $i -gt 0; $i-- )
		{
			$data = FlipPancake ( FlipPancake $data ( MaxIdx $data[ 0..$i ] ) ) $i
		}
	}
	"sorted - $data"
}

$l = 100; PancakeSort ( 1..$l | ForEach-Object { $Rand = New-Object Random }{ $Rand.Next( 0, $l - 1 ) } )
```



## PureBasic



```PureBasic
If OpenConsole()
  Define i, j, k, Loops
  Dim Pile(9)
  ;--------------------------------------------------------------
  ;- Create a Random Pile()
  For i=1 To 9                             ;- Initiate the Pile
    Pile(i)=i
  Next
  For i=9 To 1 Step -1                     ;- Do a Fisher-Yates shuffle
    Swap Pile(i),Pile(Random(i-1)+1)
  Next
  Print("Random Pile()    :")
  For i=1 To 9
    Print(" "+Str(Pile(i)))
  Next
  ;--------------------------------------------------------------
  ;- Start Sorting
  For i=9 To 2 Step -1
    If Pile(i)<>i       ;- Only Flip it if the current cake need Swapping
      Loops+1
      j=0
      Repeat            ;- find place of Pancake(i) in the Pile()
        j+1
      Until Pile(j)=i

      For k=1 To (j/2)  ;- Flip it up
        Swap Pile(k),Pile(j-k+1)
      Next
      For k=1 To i/2    ;- Flip in place
        Swap Pile(k),Pile(i-k+1)
      Next

    EndIf
  Next

  Print(#CRLF$+"Resulting Pile() :")
  For i=1 To 9
    Print(" "+str(Pile(i)))
  Next
  Print(#CRLF$+"All done in "+str(Loops)+" loops.")
  Print(#CRLF$+#CRLF$+"Press ENTER to quit."): Input()
  CloseConsole()
EndIf
```


'''Output can look like
 Original Pile()  : 9 4 1 8 6 3 2 5 7
 Resulting Pile() : 1 2 3 4 5 6 7 8 9
 All done in 6 loops.

 Press ENTER to quit.


## Python

'''The function:'''

```python
tutor = False

def pancakesort(data):
    if len(data) <= 1:
        return data
    if tutor: print()
    for size in range(len(data), 1, -1):
        maxindex = max(range(size), key=data.__getitem__)
        if maxindex+1 != size:
            # This indexed max needs moving
            if maxindex != 0:
                # Flip the max item to the left
                if tutor: print('With: %r doflip  %i'
                                % ( ' '.join(str(x) for x in data), maxindex+1 ))
                data[:maxindex+1] = reversed(data[:maxindex+1])
            # Flip it into its final position
            if tutor: print('With: %r  doflip %i'
                                % ( ' '.join(str(x) for x in data), size ))
            data[:size] = reversed(data[:size])
    if tutor: print()
```


'''A test:'''

```python
if __name__ == '__main__':
    import random

    tutor = True
    data = list('123456789')
    while data == sorted(data):
        random.shuffle(data)
    print('Original List: %r' % ' '.join(data))
    pancakesort(data)
    print('Pancake Sorted List: %r' % ' '.join(data))
```


'''Sample output:'''

```txt
Original List: '6 7 2 1 8 9 5 3 4'

With: '6 7 2 1 8 9 5 3 4' doflip  6
With: '9 8 1 2 7 6 5 3 4'  doflip 9
With: '4 3 5 6 7 2 1 8 9' doflip  5
With: '7 6 5 3 4 2 1 8 9'  doflip 7
With: '1 2 4 3 5 6 7 8 9' doflip  3
With: '4 2 1 3 5 6 7 8 9'  doflip 4
With: '3 1 2 4 5 6 7 8 9'  doflip 3
With: '2 1 3 4 5 6 7 8 9'  doflip 2

Pancake Sorted List: '1 2 3 4 5 6 7 8 9'
```



## Racket


```racket

#lang racket

(define (pancake-sort l)
  (define (flip l n) (append (reverse (take l n)) (drop l n)))
  (for/fold ([l l]) ([i (in-range (length l) 1 -1)])
    (let* ([i2 (cdr (for/fold ([m #f]) ([x l] [j i])
                      (if (and m (<= x (car m))) m (cons x j))))]
           [l (if (zero? i2) l (flip l (add1 i2)))])
      (flip l i))))

(pancake-sort (shuffle (range 0 10)))
;; => '(0 1 2 3 4 5 6 7 8 9)

```



## REXX


```rexx
/*REXX program  sorts and displays  an array  using the  pancake sort  algorithm.       */
call gen                                         /*generate elements in the   @.  array.*/
call show          'before sort'                 /*display the   BEFORE  array elements.*/
say copies('▒', 60)                              /*display a separator line for eyeballs*/
call pancakeSort         #                       /*invoke the   pancake  sort.   Yummy. */
call show          ' after sort'                 /*display the    AFTER array elements. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
flip: parse arg y;  do i=1  for (y+1)%2; yyy=y-i+1; _=@.i; @.i=@.yyy; @.yyy=_; end; return
show: do k=1  for #;  say @element right(k,length(#)) arg(1)':' right(@.k,9);  end; return
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen:  fibs= '-55 -21 -1 -8 -8 -21 -55 0 0'       /*some non─positive Fibonacci numbers, */
      @element= right('element', 21)             /*     most Fibs of which are repeated.*/

      /* ┌◄─┬──◄─ some paired bread primes which are of the form:  (P-3)÷2  and  2∙P+3  */
      /* │  │     where P is a prime. Bread primes are related to sandwich & meat primes*/
      /* ↓  ↓ ──── ──── ───── ────── ────── ────── ────── ─────── ─────── ─────── ──────*/
      bp=2 17 5 29 7 37 13 61 43 181 47 197 67 277 97 397 113 461 137 557 167 677 173 701,
                                                      797 1117 307 1237 1597 463 1861 467
      $=bp fibs;         #=words($)              /*combine the two lists; get # of items*/
             do j=1  for #;  @.j=word($,j);  end /*◄─── obtain a number from the $ list.*/
      return                                     /* [↑]  populate the  @.  array with #s*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
pancakeSort: procedure expose @.;   parse arg N
                       do N=N  by -1  for N-1
                       !=@.1;  ?=1;                do j=2  to N;   if @.j<=!  then iterate
                                                   !=@.j;          ?=j
                                                   end   /*j*/
                       call flip ?;   call flip N
                       end   /*N*/
             return
```

<small>(Shown at three-quarter size.)</small>
<pre style="font-size:75%;height:95ex">
              element  1 before sort:         2
              element  2 before sort:        17
              element  3 before sort:         5
              element  4 before sort:        29
              element  5 before sort:         7
              element  6 before sort:        37
              element  7 before sort:        13
              element  8 before sort:        61
              element  9 before sort:        43
              element 10 before sort:       181
              element 11 before sort:        47
              element 12 before sort:       197
              element 13 before sort:        67
              element 14 before sort:       277
              element 15 before sort:        97
              element 16 before sort:       397
              element 17 before sort:       113
              element 18 before sort:       461
              element 19 before sort:       137
              element 20 before sort:       557
              element 21 before sort:       167
              element 22 before sort:       677
              element 23 before sort:       173
              element 24 before sort:       701
              element 25 before sort:       797
              element 26 before sort:      1117
              element 27 before sort:       307
              element 28 before sort:      1237
              element 29 before sort:      1597
              element 30 before sort:       463
              element 31 before sort:      1861
              element 32 before sort:       467
              element 33 before sort:       -55
              element 34 before sort:       -21
              element 35 before sort:        -1
              element 36 before sort:        -8
              element 37 before sort:        -8
              element 38 before sort:       -21
              element 39 before sort:       -55
              element 40 before sort:         0
              element 41 before sort:         0
▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
              element  1  after sort:       -55
              element  2  after sort:       -55
              element  3  after sort:       -21
              element  4  after sort:       -21
              element  5  after sort:        -8
              element  6  after sort:        -8
              element  7  after sort:        -1
              element  8  after sort:         0
              element  9  after sort:         0
              element 10  after sort:         2
              element 11  after sort:         5
              element 12  after sort:         7
              element 13  after sort:        13
              element 14  after sort:        17
              element 15  after sort:        29
              element 16  after sort:        37
              element 17  after sort:        43
              element 18  after sort:        47
              element 19  after sort:        61
              element 20  after sort:        67
              element 21  after sort:        97
              element 22  after sort:       113
              element 23  after sort:       137
              element 24  after sort:       167
              element 25  after sort:       173
              element 26  after sort:       181
              element 27  after sort:       197
              element 28  after sort:       277
              element 29  after sort:       307
              element 30  after sort:       397
              element 31  after sort:       461
              element 32  after sort:       463
              element 33  after sort:       467
              element 34  after sort:       557
              element 35  after sort:       677
              element 36  after sort:       701
              element 37  after sort:       797
              element 38  after sort:      1117
              element 39  after sort:      1237
              element 40  after sort:      1597
              element 41  after sort:      1861

```



## Ring


```ring

pancakeList = [6, 7, 8, 9, 2, 5, 3, 4, 1]
flag = 0
see "Before :" + nl
for n = 1 to len(pancakeList)
    see pancakeList[n] + " "
next
see nl

pancakeSort(pancakeList)

see "After :" + nl
for n = 1 to len(pancakeList)
    see pancakeList[n] + " "
next
see nl

func pancakeSort A
     n = len(A)
     while flag =  0
           flag = 1
           for i = 1 to n-1
               if A[i] < A[i+1]
                  temp = A[i]
                  A[i] = A[i+1]
                  A [i+1] = temp
                  flag = 0 ok
           next
     end
     return A

```

Output:

```txt

Before :
678925341
After :
987654321

```



## Ruby


```ruby
class Array
  def pancake_sort!
    num_flips = 0
    (self.size-1).downto(1) do |end_idx|
      max     = self[0..end_idx].max
      max_idx = self[0..end_idx].index(max)
      next if max_idx == end_idx

      if max_idx > 0
        self[0..max_idx] = self[0..max_idx].reverse
        p [num_flips += 1, self]  if $DEBUG
      end

      self[0..end_idx] = self[0..end_idx].reverse
      p [num_flips += 1, self]  if $DEBUG
    end
    self
  end
end

p a = (1..9).to_a.shuffle
p a.pancake_sort!
```


'''sample output:'''

```txt
$ ruby -d sorting_pancake.rb
[7, 3, 6, 8, 2, 4, 5, 1, 9]
[1, [8, 6, 3, 7, 2, 4, 5, 1, 9]]
[2, [1, 5, 4, 2, 7, 3, 6, 8, 9]]
[3, [7, 2, 4, 5, 1, 3, 6, 8, 9]]
[4, [6, 3, 1, 5, 4, 2, 7, 8, 9]]
[5, [2, 4, 5, 1, 3, 6, 7, 8, 9]]
[6, [5, 4, 2, 1, 3, 6, 7, 8, 9]]
[7, [3, 1, 2, 4, 5, 6, 7, 8, 9]]
[8, [2, 1, 3, 4, 5, 6, 7, 8, 9]]
[9, [1, 2, 3, 4, 5, 6, 7, 8, 9]]
[1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Rust


```Rust
fn pancake_sort<T: Ord>(v: &mut [T]) {
    let len = v.len();
    // trivial case -- no flips
    if len < 2 {
        return;
    }
    for i in (0..len).rev() {
        // find index of the maximum element within `v[0..i]` (inclusive)
        let max_index = v.iter()
            .take(i + 1)
            .enumerate()
            .max_by_key(|&(_, elem)| elem)
            .map(|(idx, _)| idx)
            // safe because we already checked if `v` is empty
            .unwrap();
        // if `max_index` is not where it's supposed to be
        // do two flips to move it to `i`
        if max_index != i {
            flip(v, max_index);
            flip(v, i);
        }
    }
}

// function to flip a section of a mutable collection from 0..num (inclusive)
fn flip<E: PartialOrd>(v: &mut [E], num: usize) {
    v[0..num + 1].reverse();
}

fn main() {
    // Sort numbers
    let mut numbers = [4, 65, 2, -31, 0, 99, 2, 83, 782, 1];
    println!("Before: {:?}", numbers);
    pancake_sort(&mut numbers);
    println!("After: {:?}", numbers);

    // Sort strings
    let mut strings = ["beach", "hotel", "airplane", "car", "house", "art"];
    println!("Before: {:?}", strings);
    pancake_sort(&mut strings);
    println!("After: {:?}", strings);
}
```



## Sidef

```ruby
func pancake(a) {
    for idx in ^(a.end) {
        var min = idx
        for i in (idx+1 .. a.end) { min = i if (a[min] > a[i]) }
        next if (a[min] == a[idx])
        a[min..a.end] = [a[min..a.end]].reverse...
        a[idx..a.end] = [a[idx..a.end]].reverse...
    }
    return a
}

var arr = 10.of{ 100.irand }
say "Before: #{arr}"
say "After:  #{pancake(arr)}"
```


```txt

Before: 61 29 68 15 34 2 32 54 73 43
After:  2 15 29 32 34 43 54 61 68 73

```



## Swift

```Swift
import Foundation

struct PancakeSort {
    var arr:[Int]

    mutating func flip(n:Int) {
        for i in 0 ..< (n + 1) / 2 {
            swap(&arr[n - i], &arr[i])
        }
        println("flip(0.. \(n)): \(arr)")
    }

    func minmax(n:Int) -> [Int] {
        var xm = arr[0]
        var xM = arr[0]
        var posm = 0
        var posM = 0

        for i in 1..<n {
            if (arr[i] < xm) {
                xm = arr[i]
                posm = i
            } else if (arr[i] > xM) {
                xM = arr[i]
                posM = i
            }
        }

        return [posm, posM]
    }

    mutating func sort(var n:Int, var dir:Int) {
        if n == 0 {
            return
        }

        let mM = minmax(n)
        let bestXPos = mM[dir]
        let altXPos = mM[1 - dir]
        var flipped = false

        if bestXPos == n - 1 {
            n--
        } else if bestXPos == 0 {
            flip(n - 1)
            n--
        } else if altXPos == n - 1 {
            dir = 1 - dir
            n--
            flipped = true
        } else {
            flip(bestXPos)
        }

        sort(n, dir: dir)

        if flipped {
            flip(n)
        }
    }
}

let arr = [2, 3, 6, 1, 4, 5, 10, 8, 7, 9]
var a = PancakeSort(arr: arr)
a.sort(arr.count, dir: 1)
println(a.arr)
```

```txt

flip(0.. 6): [10, 5, 4, 1, 6, 3, 2, 8, 7, 9]
flip(0.. 9): [9, 7, 8, 2, 3, 6, 1, 4, 5, 10]
flip(0.. 8): [5, 4, 1, 6, 3, 2, 8, 7, 9, 10]
flip(0.. 6): [8, 2, 3, 6, 1, 4, 5, 7, 9, 10]
flip(0.. 7): [7, 5, 4, 1, 6, 3, 2, 8, 9, 10]
flip(0.. 6): [2, 3, 6, 1, 4, 5, 7, 8, 9, 10]
flip(0.. 2): [6, 3, 2, 1, 4, 5, 7, 8, 9, 10]
flip(0.. 5): [5, 4, 1, 2, 3, 6, 7, 8, 9, 10]
flip(0.. 4): [3, 2, 1, 4, 5, 6, 7, 8, 9, 10]
flip(0.. 2): [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

```



## Tcl


```tcl
package require Tcl 8.5
# Some simple helper procedures
proc flip {nlist n} {
    concat [lreverse [lrange $nlist 0 $n]] [lrange $nlist $n+1 end]
}
proc findmax {nlist limit} {
    lsearch -exact $nlist [tcl::mathfunc::max {*}[lrange $nlist 0 $limit]]
}

# Simple-minded pancake sort algorithm
proc pancakeSort {nlist {debug ""}} {
    for {set i [llength $nlist]} {[incr i -1] > 0} {} {
	set j [findmax $nlist $i]
	if {$i != $j} {
	    if {$j} {
		set nlist [flip $nlist $j]
		if {$debug eq "debug"} {puts [incr flips]>>$nlist}
	    }
	    set nlist [flip $nlist $i]
	    if {$debug eq "debug"} {puts [incr flips]>>$nlist}
	}
    }
    return $nlist
}
```

Demonstrate (with debug mode enabled so it prints intermediate states):

```tcl
puts [pancakeSort {27916 5928 23535 14711 32184 14621 21093 14422 29844 11093} debug]
```

Output:

```txt

1>>32184 14711 23535 5928 27916 14621 21093 14422 29844 11093
2>>11093 29844 14422 21093 14621 27916 5928 23535 14711 32184
3>>29844 11093 14422 21093 14621 27916 5928 23535 14711 32184
4>>14711 23535 5928 27916 14621 21093 14422 11093 29844 32184
5>>27916 5928 23535 14711 14621 21093 14422 11093 29844 32184
6>>11093 14422 21093 14621 14711 23535 5928 27916 29844 32184
7>>23535 14711 14621 21093 14422 11093 5928 27916 29844 32184
8>>5928 11093 14422 21093 14621 14711 23535 27916 29844 32184
9>>21093 14422 11093 5928 14621 14711 23535 27916 29844 32184
10>>14711 14621 5928 11093 14422 21093 23535 27916 29844 32184
11>>14422 11093 5928 14621 14711 21093 23535 27916 29844 32184
12>>5928 11093 14422 14621 14711 21093 23535 27916 29844 32184
5928 11093 14422 14621 14711 21093 23535 27916 29844 32184

```

As you can see, it took 12 flips.


## uBasic/4tH

<lang>PRINT "Pancake sort:"
  n = FUNC (_InitArray)
  PROC _ShowArray (n)
  PROC _Pancakesort (n)
  PROC _ShowArray (n)
PRINT

END


_Flip PARAM(1)
  LOCAL(1)

  b@ = 0

  DO WHILE b@ < a@
    PROC _Swap (b@, a@)
    b@ = b@ + 1
    a@ = a@ - 1
  LOOP
RETURN


_Pancakesort PARAM (1)                 ' Pancakesort
  LOCAL(3)

  IF a@ < 2 THEN RETURN

  FOR b@ = a@ TO 2 STEP -1

    c@  = 0

    FOR d@ = 0 TO b@ - 1
      IF @(d@) > @(c@) THEN c@ = d@
    NEXT

    IF c@ = b@ - 1 THEN CONTINUE
    IF c@ THEN PROC _Flip (c@)
    PROC _Flip (b@ - 1)

  NEXT
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


## VBA


```vb


'pancake sort
'uses two auxiliary routines "printarray" and "flip"

Public Sub printarray(A)
  For i = LBound(A) To UBound(A)
    Debug.Print A(i),
  Next
  Debug.Print
End Sub

Public Sub Flip(ByRef A, p1, p2, trace)
'flip first elements of A (p1 to p2)
 If trace Then Debug.Print "we'll flip the first "; p2 - p1 + 1; "elements of the array"
 Cut = Int((p2 - p1 + 1) / 2)
 For i = 0 To Cut - 1
   'flip position i and (n - i + 1)
   temp = A(i)
   A(i) = A(p2 - i)
   A(p2 - i) = temp
 Next
End Sub

Public Sub pancakesort(ByRef A(), Optional trace As Boolean = False)
'sort A into ascending order using pancake sort

lb = LBound(A)
ub = UBound(A)
Length = ub - lb + 1
If Length <= 1 Then 'no need to sort
  Exit Sub
End If

For i = ub To lb + 1 Step -1
  'find position of max. element in subarray A(lowerbound to i)
  P = lb
  Maximum = A(P)
  For j = lb + 1 To i
    If A(j) > Maximum Then
      P = j
      Maximum = A(j)
    End If
  Next j
  'check if maximum is already at end - then we don't need to flip
  If P < i Then
    'flip the first part of the array up to the maximum so it is at the head - skip if it is already there
    If P > 1 Then
      Flip A, lb, P, trace
      If trace Then printarray A
    End If
    'now flip again so that it is in its final position
    Flip A, lb, i, trace
    If trace Then printarray A
  End If
Next i
End Sub

'test routine
Public Sub TestPancake(Optional trace As Boolean = False)
Dim A()
A = Array(5, 7, 8, 3, 1, 10, 9, 23, 50, 0)
Debug.Print "Initial array:"
printarray A
pancakesort A, trace
Debug.Print "Final array:"
printarray A
End Sub

```


Sample output:

```txt

testpancake True
Initial array:
 5             7             8             3             1             10            9             23            50            0
we'll flip the first  9 elements of the array
 50            23            9             10            1             3             8             7             5             0
we'll flip the first  10 elements of the array
 0             5             7             8             3             1             10            9             23            50
we'll flip the first  7 elements of the array
 10            1             3             8             7             5             0             9             23            50
we'll flip the first  8 elements of the array
 9             0             5             7             8             3             1             10            23            50
we'll flip the first  7 elements of the array
 1             3             8             7             5             0             9             10            23            50
we'll flip the first  3 elements of the array
 8             3             1             7             5             0             9             10            23            50
we'll flip the first  6 elements of the array
 0             5             7             1             3             8             9             10            23            50
we'll flip the first  3 elements of the array
 7             5             0             1             3             8             9             10            23            50
we'll flip the first  5 elements of the array
 3             1             0             5             7             8             9             10            23            50
we'll flip the first  3 elements of the array
 0             1             3             5             7             8             9             10            23            50
Final array:
 0             1             3             5             7             8             9             10            23            50

```



## zkl

```zkl
fcn pancakeSort(a){
   foreach i in ([a.len()-1..1,-1]){
      j := a.index((0).max(a[0,i+1]));  // min for decending sort
      if(i != j){ a.swap(0,j); a.swap(0,i); }
   }
   a
}
```

Note: [offset,count] not [start,stop]

Finding the max index creates a partial list, which isn't good; if it matters use:

```zkl
      j := (i+1).reduce('wrap(x,y){ if(a[x]>a[y]) x else y });
```


```zkl
pancakeSort(List(7,6,9,2,4,8,1,3,5)).println();
```

```txt
L(1,2,3,4,5,6,7,8,9)
```


