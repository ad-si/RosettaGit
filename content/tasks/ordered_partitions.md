+++
title = "Ordered Partitions"
description = ""
date = 2019-07-04T12:15:55Z
aliases = []
[extra]
id = 9223
[taxonomies]
categories = ["task", "Discrete math"]
tags = []
+++

## Task

In this task we want to find the ordered partitions into fixed-size blocks.

This task is related to [[Combinations]] in that it has to do with discrete mathematics and moreover a helper function to compute combinations is (probably) needed to solve this task.

<math>partitions(\mathit{arg}_1,\mathit{arg}_2,...,\mathit{arg}_n)</math> should generate all distributions of the elements in <math>\{1,...,\Sigma_{i=1}^n\mathit{arg}_i\}</math> into <math>n</math> blocks of respective size <math>\mathit{arg}_1,\mathit{arg}_2,...,\mathit{arg}_n</math>.

Example 1: <math>partitions(2,0,2)</math> would create:


```txt

{({1, 2}, {}, {3, 4}),
 ({1, 3}, {}, {2, 4}),
 ({1, 4}, {}, {2, 3}),
 ({2, 3}, {}, {1, 4}),
 ({2, 4}, {}, {1, 3}),
 ({3, 4}, {}, {1, 2})}

```


Example 2: <math>partitions(1,1,1)</math> would create:


```txt

{({1}, {2}, {3}),
 ({1}, {3}, {2}),
 ({2}, {1}, {3}),
 ({2}, {3}, {1}),
 ({3}, {1}, {2}),
 ({3}, {2}, {1})}

```


Note that the number of elements in the list is
:<math>{\mathit{arg}_1+\mathit{arg}_2+...+\mathit{arg}_n \choose \mathit{arg}_1} \cdot {\mathit{arg}_2+\mathit{arg}_3+...+\mathit{arg}_n \choose \mathit{arg}_2} \cdot \ldots \cdot {\mathit{arg}_n \choose \mathit{arg}_n}</math>
(see [http://en.wikipedia.org/wiki/Binomial_coefficient the definition of the binomial coefficient] if you are not familiar with this notation) and the number of elements remains the same regardless of how the argument is permuted
(i.e. the [http://en.wikipedia.org/wiki/Multinomial_coefficient multinomial coefficient]).

Also, <math>partitions(1,1,1)</math> creates the permutations of <math>\{1,2,3\}</math> and thus there would be <math>3! = 6</math> elements in the list.

Note: Do not use functions that are not in the standard library of the programming language you use. Your file should be written so that it can be executed on the command line and by default outputs the result of <math>partitions(2,0,2)</math>. If the programming language does not support polyvariadic functions pass a list as an argument.

'''Notation'''

Here are some explanatory remarks on the notation used in the task description:

<math>\{1, \ldots, n\}</math> denotes the set of consecutive numbers from <math>1</math> to <math>n</math>, e.g. <math>\{1,2,3\}</math> if <math>n = 3</math>.

<math>\Sigma</math> is the mathematical notation for summation, e.g. <math>\Sigma_{i=1}^3 i = 6</math> (see also [http://en.wikipedia.org/wiki/Summation#Capital-sigma_notation]).

<math>\mathit{arg}_1,\mathit{arg}_2,...,\mathit{arg}_n</math> are the arguments — natural numbers — that the sought function receives.





## Ada

partitions.ads:

```Ada
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Ordered_Sets;
package Partitions is
   -- Argument type for Create_Partitions: Array of Numbers
   type Arguments is array (Positive range <>) of Natural;
   package Number_Sets is new Ada.Containers.Ordered_Sets
     (Natural);
   type Partition is array (Positive range <>) of Number_Sets.Set;
   function "<" (Left, Right : Partition) return Boolean;
   package Partition_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Partition);
   function Create_Partitions (Args : Arguments) return Partition_Sets.Set;
end Partitions;
```


partitions.adb:

```Ada
package body Partitions is
   -- compare number sets (not provided)
   function "<" (Left, Right : Number_Sets.Set) return Boolean is
      use type Ada.Containers.Count_Type;
      use Number_Sets;
      Left_Pos  : Cursor := Left.First;
      Right_Pos : Cursor := Right.First;
   begin
      -- compare each element, until one or both lists finishes
      while Left_Pos /= No_Element and then Right_Pos /= No_Element loop
         -- compare elements
         if Element (Left_Pos) < Element (Right_Pos) then
            return True;
         elsif Element (Left_Pos) > Element (Right_Pos) then
            return False;
         end if;
         -- increase iterator
         Next (Left_Pos);
         Next (Right_Pos);
      end loop;
      -- Right is longer
      if Right_Pos /= No_Element then
         return True;
      else
         -- Left is longer, or Left and Right are identical.
         return False;
      end if;
   end "<";
   -- compare two Partitions
   function "<" (Left, Right : Partition) return Boolean is
      use type Ada.Containers.Count_Type;
      use type Number_Sets.Set;
   begin
      -- check length
      if Left'Length < Right'Length then
         return True;
      elsif Left'Length > Right'Length then
         return False;
      end if;
      -- same length
      if Left'Length > 0 then
         for I in Left'Range loop
            if Left (I) < Right (I) then
               return True;
            elsif Left (I) /= Right (I) then
               return False;
            end if;
         end loop;
      end if;
      -- length = 0 are always smallest
      return False;
   end "<";
   -- create partitions (as the task describes)
   function Create_Partitions (Args : Arguments) return Partition_Sets.Set is
      -- permutations needed
      type Permutation is array (Positive range <>) of Natural;
      -- exception to be thrown after last permutation reached
      No_More_Permutations : exception;
      -- get initial permutation (ordered small->big)
      function Initial_Permutation (Max : Natural) return Permutation is
         Result : Permutation (1 .. Max);
      begin
         for I in 1 .. Max loop
            Result (I) := I;
         end loop;
         return Result;
      end Initial_Permutation;
      -- get next permutation
      function Next_Permutation (Current : Permutation) return Permutation is
         K      : Natural     := Current'Last - 1;
         L      : Positive    := Current'Last;
         Result : Permutation := Current;
      begin
         -- 1. Find the largest index k such that a[k] < a[k + 1].
         while K /= 0 and then Current (K) > Current (K + 1) loop
            K := K - 1;
         end loop;
         -- If no such index exists, the permutation is the last permutation.
         if K = 0 then
            raise No_More_Permutations;
         end if;
         -- 2. Find the largest index l such that a[k] < a[l].
         -- Since k + 1 is such an index, l is well defined
         -- and satisfies k < l.
         while Current (K) > Current (L) loop
            L := L - 1;
         end loop;
         -- 3. Swap a[k] with a[l].
         Result (K) := Current (L);
         Result (L) := Current (K);
         -- 4. Reverse the sequence from a[k + 1] up to and including the
         -- final element a[n].
         for I in 1 .. (Result'Last - K) / 2 loop
            declare
               Temp : constant Natural := Result (K + I);
            begin
               Result (K + I)               := Result (Result'Last - I + 1);
               Result (Result'Last - I + 1) := Temp;
            end;
         end loop;
         return Result;
      end Next_Permutation;
      Result : Partition_Sets.Set;
      Sum    : Natural := 0;
   begin
      -- get number of elements
      for I in Args'Range loop
         Sum := Sum + Args (I);
      end loop;
      declare
      -- initial permutation
         Current_Permutation : Permutation := Initial_Permutation (Sum);
      begin
         -- loop through permutations
         loop
         -- create Partition (same count of Number_Sets.Set as Args)
            declare
               Item              : Natural := Current_Permutation'First;
               Current_Partition : Partition (Args'Range);
            begin
               -- loop each partition
               for I in Args'Range loop
                  -- fill in the number of elements requested
                  for J in 1 .. Args (I) loop
                     Current_Partition (I).Insert
                       (New_Item => Current_Permutation (Item));
                     Item := Item + 1;
                  end loop;
               end loop;
               -- insert partition into result set
               Result.Insert (New_Item => Current_Partition);
            exception
               when Constraint_Error =>
                  -- partition was already inserted, ignore it.
                  -- this happens when one of the args > 1.
                  null;
            end;
            -- create next permutation
            Current_Permutation := Next_Permutation (Current_Permutation);
         end loop;
      exception
         when No_More_Permutations =>
            -- no more permutations, we are finished
            null;
      end;
      return Result;
   end Create_Partitions;
end Partitions;
```


example main.adb:

```Ada
with Ada.Text_IO;
with Partitions;
procedure Main is
   package Natural_IO is new Ada.Text_IO.Integer_IO (Natural);
   Example_Partitions : Partitions.Partition_Sets.Set;
begin
   Ada.Text_IO.Put_Line ("Partitions for (2, 0, 2):");
   -- create partition
   Example_Partitions := Partitions.Create_Partitions (Args => (2, 0, 2));
   -- pretty print the result
   declare
      use type Partitions.Partition_Sets.Cursor;
      Position : Partitions.Partition_Sets.Cursor := Example_Partitions.First;
   begin
      Ada.Text_IO.Put ('{');
      while Position /= Partitions.Partition_Sets.No_Element loop
         if Position /= Example_Partitions.First then
            Ada.Text_IO.Put (' ');
         end if;
         declare
            Current_Partition : constant Partitions.Partition :=
               Partitions.Partition_Sets.Element (Position);
         begin
            Ada.Text_IO.Put ('(');
            for I in Current_Partition'Range loop
               Ada.Text_IO.Put ('{');
               declare
                  use type Partitions.Number_Sets.Cursor;
                  Current_Number : Partitions.Number_Sets.Cursor :=
                     Current_Partition (I).First;
               begin
                  while Current_Number /= Partitions.Number_Sets.No_Element
                  loop
                     Natural_IO.Put
                       (Item  =>
                           Partitions.Number_Sets.Element (Current_Number),
                        Width => 1);
                     Partitions.Number_Sets.Next (Current_Number);
                     if Current_Number /=
                        Partitions.Number_Sets.No_Element then
                        Ada.Text_IO.Put (',');
                     end if;
                  end loop;
               end;
               Ada.Text_IO.Put ('}');
               if I /= Current_Partition'Last then
                  Ada.Text_IO.Put (", ");
               end if;
            end loop;
         end;
         Ada.Text_IO.Put (')');
         Partitions.Partition_Sets.Next (Position);
         if Position /= Partitions.Partition_Sets.No_Element then
            Ada.Text_IO.Put (',');
            Ada.Text_IO.New_Line;
         end if;
      end loop;
      Ada.Text_IO.Put ('}');
      Ada.Text_IO.New_Line;
   end;
end Main;
```


Output:

```txt
Partitions for (2, 0, 2):
{({1,2}, {}, {3,4}),
 ({1,3}, {}, {2,4}),
 ({1,4}, {}, {2,3}),
 ({2,3}, {}, {1,4}),
 ({2,4}, {}, {1,3}),
 ({3,4}, {}, {1,2})}
```



## BBC BASIC

```bbcbasic
      DIM list1%(2) : list1%() = 2, 0, 2
      PRINT "partitions(2,0,2):"
      PRINT FNpartitions(list1%())
      DIM list2%(2) : list2%() = 1, 1, 1
      PRINT "partitions(1,1,1):"
      PRINT FNpartitions(list2%())
      DIM list3%(3) : list3%() = 1, 2, 0, 1
      PRINT "partitions(1,2,0,1):"
      PRINT FNpartitions(list3%())
      END

      DEF FNpartitions(list%())
      LOCAL i%, j%, n%, p%, o$, x%()
      n% = DIM(list%(),1)
      DIM x%(SUM(list%())-1)
      FOR i% = 0 TO n%
        IF list%(i%) THEN
          FOR j% = 1 TO list%(i%)
            x%(p%) = i%
            p% += 1
          NEXT
        ENDIF
      NEXT i%
      REPEAT
        FOR i% = 0 TO n%
          o$ += " ( "
          FOR j% = 0 TO DIM(x%(),1)
            IF x%(j%) = i% o$ += STR$(j%+1) + " "
          NEXT
          o$ += ")"
        NEXT i%
        o$ += CHR$13 + CHR$10
      UNTIL NOT FNperm(x%())
      = o$

      DEF FNperm(x%())
      LOCAL i%, j%
      FOR i% = DIM(x%(),1)-1 TO 0 STEP -1
        IF x%(i%) < x%(i%+1) EXIT FOR
      NEXT
      IF i% < 0 THEN = FALSE
      j% = DIM(x%(),1)
      WHILE x%(j%) <= x%(i%) j% -= 1 : ENDWHILE
      SWAP x%(i%), x%(j%)
      i% += 1
      j% = DIM(x%(),1)
      WHILE i% < j%
        SWAP x%(i%), x%(j%)
        i% += 1
        j% -= 1
      ENDWHILE
      = TRUE
```

'''Output:'''

```txt

partitions(2,0,2):
 ( 1 2 ) ( ) ( 3 4 )
 ( 1 3 ) ( ) ( 2 4 )
 ( 1 4 ) ( ) ( 2 3 )
 ( 2 3 ) ( ) ( 1 4 )
 ( 2 4 ) ( ) ( 1 3 )
 ( 3 4 ) ( ) ( 1 2 )

partitions(1,1,1):
 ( 1 ) ( 2 ) ( 3 )
 ( 1 ) ( 3 ) ( 2 )
 ( 2 ) ( 1 ) ( 3 )
 ( 3 ) ( 1 ) ( 2 )
 ( 2 ) ( 3 ) ( 1 )
 ( 3 ) ( 2 ) ( 1 )

partitions(1,2,0,1):
 ( 1 ) ( 2 3 ) ( ) ( 4 )
 ( 1 ) ( 2 4 ) ( ) ( 3 )
 ( 1 ) ( 3 4 ) ( ) ( 2 )
 ( 2 ) ( 1 3 ) ( ) ( 4 )
 ( 2 ) ( 1 4 ) ( ) ( 3 )
 ( 3 ) ( 1 2 ) ( ) ( 4 )
 ( 4 ) ( 1 2 ) ( ) ( 3 )
 ( 3 ) ( 1 4 ) ( ) ( 2 )
 ( 4 ) ( 1 3 ) ( ) ( 2 )
 ( 2 ) ( 3 4 ) ( ) ( 1 )
 ( 3 ) ( 2 4 ) ( ) ( 1 )
 ( 4 ) ( 2 3 ) ( ) ( 1 )

```



## C

Watch out for blank for loops.  Iterative permutation generation is described at [[http://en.wikipedia.org/wiki/Permutation#Systematic_generation_of_all_permutations]]; code messness is purely mine.

```c
#include <stdio.h>

int next_perm(int size, int * nums)
{
        int *l, *k, tmp;

        for (k = nums + size - 2; k >= nums && k[0] >= k[1]; k--) {};
        if (k < nums) return 0;

        for (l = nums + size - 1; *l <= *k; l--) {};
        tmp = *k; *k = *l; *l = tmp;

        for (l = nums + size - 1, k++; k < l; k++, l--) {
                tmp = *k; *k = *l; *l = tmp;
        }

        return 1;
}

void make_part(int n, int * sizes)
{
        int x[1024], i, j, *ptr, len = 0;

        for (ptr = x, i = 0; i < n; i++)
                for (j = 0, len += sizes[i]; j < sizes[i]; j++, *(ptr++) = i);

        do {
                for (i = 0; i < n; i++) {
                        printf(" { ");
                        for (j = 0; j < len; j++)
                                if (x[j] == i) printf("%d ", j);

                        printf("}");
                }
                printf("\n");
        } while (next_perm(len, x));
}

int main()
{
        int s1[] = {2, 0, 2};
        int s2[] = {1, 2, 3, 4};

        printf("Part 2 0 2:\n");
        make_part(3, s1);

        printf("\nPart 1 2 3 4:\n");
        make_part(4, s2);

        return 1;
}
```

Output:

```txt
Part 2 0 2:
 { 0 1 } { } { 2 3 }
 { 0 2 } { } { 1 3 }
 { 0 3 } { } { 1 2 }
 { 1 2 } { } { 0 3 }
 { 1 3 } { } { 0 2 }
 { 2 3 } { } { 0 1 }

Part 1 2 3 4:
 { 0 } { 1 2 } { 3 4 5 } { 6 7 8 9 }
 { 0 } { 1 2 } { 3 4 6 } { 5 7 8 9 }
 { 0 } { 1 2 } { 3 4 7 } { 5 6 8 9 }
 { 0 } { 1 2 } { 3 4 8 } { 5 6 7 9 }
 { 0 } { 1 2 } { 3 4 9 } { 5 6 7 8 }
 { 0 } { 1 2 } { 3 5 6 } { 4 7 8 9 }
....
```

With bitfield:
```c
#include <stdio.h>

typedef unsigned int uint;

int parts[] = {2, 1, 2};
#define n_parts sizeof(parts)/sizeof(parts[0])
int bits[n_parts];

void show_part(uint x)
{
	uint i;
	putchar('{');
	for (i = 0; (1 << i) <= x; i ++)
		if (x & (1 << i)) printf(" %d", i + 1);

	printf("%s", " } ");
}

void gen_bits(uint mask, uint all, uint res, int n, int pid)
{
	uint i;
	while (!n) {
		bits[pid++] = res;
		if (pid == n_parts) {
			for (i = 0; i < n_parts; i++)
				show_part(bits[i]);
			putchar('\n');
			return;
		}
		mask = all &= ~res;
		res = 0;
		n = parts[pid];
	}

	while (mask) {
		mask &= ~(i = mask & -(int)mask);
		gen_bits(mask, all, res | i, n - 1, pid);
	}
}

int main(void)
{
	uint i, m;
	for (m = 1, i = 0; i < n_parts; i++)
		m <<= parts[i];
	m--;

	gen_bits(m, m, 0, parts[0], 0);

	return 0;
}
```



## C++


```cpp

#include <iostream>
#include <algorithm>
#include <vector>
#include <numeric>

void partitions(std::vector<size_t> args) {
  size_t sum = std::accumulate(std::begin(args), std::end(args), 0);
  std::vector<size_t> nums(sum);
  std::iota(std::begin(nums), std::end(nums), 1);
  do {
    size_t total_index = 0;
    std::vector<std::vector<size_t>> parts;
    for (const auto& a : args) {
      std::vector<size_t> part;
      bool cont = true;
      for (size_t j = 0; j < a; ++j) {
        for (const auto& p : part) {
          if (nums[total_index] < p) {
            cont = false;
            break;
          }
        }
        if (cont) {
          part.push_back(nums[total_index]);
          ++total_index;
        }
      }
      if (part.size() != a) {
        break;
      }
      parts.push_back(part);
    }
    if (parts.size() == args.size()) {
      std::cout << "(";
      for (const auto& p : parts) {
        std::cout << "{ ";
        for (const auto& e : p) {
          std::cout << e << " ";
        }
        std::cout << "},";
      }
      std::cout << ")," << std::endl;
    }
  } while (std::next_permutation(std::begin(nums), std::end(nums)));
}

int main() {
  std::vector<size_t> args = { 2, 0, 2 };
  partitions(args);
  std::cin.ignore();
  std::cin.get();
  return 0;
}
```


```txt
({ 1 2 },{ },{ 3 4 },),
({ 1 3 },{ },{ 2 4 },),
({ 1 4 },{ },{ 2 3 },),
({ 2 3 },{ },{ 1 4 },),
({ 2 4 },{ },{ 1 3 },),
({ 3 4 },{ },{ 1 2 },),
```



## C#


```c#
using System;
using System.Linq;
using System.Collections.Generic;

public static class OrderedPartitions
{
    public static void Main() {
        var input = new [] { new[] { 0, 0, 0, 0, 0 }, new[] { 2, 0, 2 }, new[] { 1, 1, 1 } };
        foreach (int[] sizes in input) {
            foreach (var partition in Partitions(sizes)) {
                Console.WriteLine(partition.Select(set => set.Delimit(", ").Encase('{','}')).Delimit(", ").Encase('(', ')'));
            }
            Console.WriteLine();
        }
    }

    static IEnumerable<IEnumerable<int[]>> Partitions(params int[] sizes) {
        var enumerators = new IEnumerator<int[]>[sizes.Length];
        var unused = Enumerable.Range(1, sizes.Sum()).ToSortedSet();
        var arrays = sizes.Select(size => new int[size]).ToArray();

        for (int s = 0; s >= 0; ) {
            if (s == sizes.Length) {
                yield return arrays;
                s--;
            }
            if (enumerators[s] == null) {
                enumerators[s] = Combinations(sizes[s], unused.ToArray()).GetEnumerator();
            } else {
                unused.UnionWith(arrays[s]);
            }
            if (enumerators[s].MoveNext()) {
                enumerators[s].Current.CopyTo(arrays[s], 0);
                unused.ExceptWith(arrays[s]);
                s++;
            } else {
                enumerators[s] = null;
                s--;
            }
        }
    }

    static IEnumerable<T[]> Combinations<T>(int count, params T[] array) {
        T[] result = new T[count];
        foreach (int pattern in BitPatterns(array.Length - count, array.Length)) {
            for (int b = 1 << (array.Length - 1), i = 0, r = 0; b > 0; b >>= 1, i++) {
                if ((pattern & b) == 0) result[r++] = array[i];
            }
            yield return result;
        }
    }

    static IEnumerable<int> BitPatterns(int ones, int length) {
        int initial = (1 << ones) - 1;
        int blockMask = (1 << length) - 1;
        for (int v = initial; v >= initial; ) {
            yield return v;
            if (v == 0) break;

            int w = (v | (v - 1)) + 1;
            w |= (((w & -w) / (v & -v)) >> 1) - 1;
            v = w & blockMask;
        }
    }

    static string Delimit<T>(this IEnumerable<T> source, string separator) => string.Join(separator, source);
    static string Encase(this string s, char start, char end) => start + s + end;
}
```

```txt

({}, {}, {}, {}, {})

({1, 2}, {}, {3, 4})
({1, 3}, {}, {2, 4})
({1, 4}, {}, {2, 3})
({2, 3}, {}, {1, 4})
({2, 4}, {}, {1, 3})
({3, 4}, {}, {1, 2})

({1}, {2}, {3})
({1}, {3}, {2})
({2}, {1}, {3})
({2}, {3}, {1})
({3}, {1}, {2})
({3}, {2}, {1})
```



## Common Lisp

Lexicographical generation of partitions. Pros: can handle duplicate elements; probably faster than some methods generating all permutations then throwing bad ones out.  Cons: clunky (which is probably my fault).

```lisp
(defun fill-part (x i j l)
  (let ((e (elt x i)))
    (loop for c in l do
	    (loop while (>= j (length e)) do
		  (setf j 0 e (elt x (incf i))))
	    (setf (elt e j) c)
	    (incf j))))

;;; take a list of lists and return next partitioning
;;; it's caller's responsibility to ensure each sublist is sorted
(defun next-part (list cmp)
  (let* ((l (coerce list 'vector))
	 (i (1- (length l)))
	 (e (elt l i)))
    (loop while (<= 0 (decf i)) do
	  ;; e holds all the right most elements
	  (let ((p (elt l i)) (q (car (last e))))
	    ;; find the right-most list that has an element that's smaller
	    ;; than _something_ in later lists
	    (when (and p (funcall cmp (first p) q))
	      ;; find largest element that can be increased
	      (loop for j from (1- (length p)) downto 0 do
		    (when (funcall cmp (elt p j) q)
		      ;; find the smallest element that's larger than
		      ;; that largest
		      (loop for x from 0 to (1- (length e)) do
			    (when (funcall cmp (elt p j) (elt e x))
			      (rotatef (elt p j) (elt e x))
			      (loop while (< (incf j) (length p)) do
			      	(setf (elt p j) (elt e (incf x))
				      (elt e x) nil))
			      (fill-part l i j (remove nil e))
			      (return-from next-part l))))
		    (setf e (append e (list (elt p j))))))
	    (setf e (append e p))))))

(let ((a '#((1 2) () (3 4))))
  (loop while a do
	(format t "~a~%" a)
	(setf a (next-part a #'<))))

(write-line "with dupe elements:")
(let ((a '#((a c) (c c d))))
  (loop while a do
	(format t "~a~%" a)
	(setf a (next-part a #'string<))))
```
output

```txt
#((1 2) NIL (3 4))
#((1 3) NIL (2 4))
#((1 4) NIL (2 3))
#((2 3) NIL (1 4))
#((2 4) NIL (1 3))
#((3 4) NIL (1 2))
with dupe elements:
#((A C) (C C D))
#((A D) (C C C))
#((C C) (A C D))
#((C D) (A C C))
```



## D

Using module of the third D entry of the Combination Task.

```d
import std.stdio, std.algorithm, std.range, std.array, std.conv,
       combinations3;

alias iRNG = int[];

iRNG[][] orderPart(iRNG blockSize...) {
    iRNG tot = iota(1, 1 + blockSize.sum).array;

    iRNG[][] p(iRNG s, in iRNG b) {
        if (b.empty)
            return [[]];
        iRNG[][] res;
        foreach (c; s.combinations(b[0]))
            foreach (r; p(setDifference(s, c).array, b.dropOne))
                res ~= c.dup ~ r;
        return res;
    }

    return p(tot, blockSize);
}

void main(in string[] args) {
    auto b = args.length > 1 ? args.dropOne.to!(int[]) : [2, 0, 2];
    writefln("%(%s\n%)", b.orderPart);
}
```

```txt
[[1, 2], [], [3, 4]]
[[1, 3], [], [2, 4]]
[[1, 4], [], [2, 3]]
[[2, 3], [], [1, 4]]
[[2, 4], [], [1, 3]]
[[3, 4], [], [1, 2]]
```



### Alternative Version

```d
import core.stdc.stdio;

void genBits(size_t N)(ref uint[N] bits, in ref uint[N] parts,
                       uint mask, uint all, uint res, uint n, uint pid)
nothrow @nogc {
    static void showPart(in uint x) nothrow @nogc {
        '['.putchar;
        for (uint i = 0; (1 << i) <= x; i++)
            if (x & (1 << i))
                printf("%d ", i + 1);
        ']'.putchar;
    }

    while (!n) {
        bits[pid] = res;
        pid++;
        if (pid == N) {
            foreach (immutable b; bits)
                showPart(b);
            '\n'.putchar;
            return;
        }
        all &= ~res;
        mask = all;
        res = 0;
        n = parts[pid];
    }

    while (mask) {
        immutable uint i = mask & -int(mask);
        mask &= ~i;
        genBits(bits, parts, mask, all, res | i, n - 1, pid);
    }
}

void main() nothrow @nogc {
    immutable uint[3] parts = [2, 0, 2];
    uint m = 1;
    foreach (immutable p; parts)
        m <<= p;

    uint[parts.length] bits;
    genBits(bits, parts, m - 1, m - 1, 0, parts[0], 0);
}
```

```txt
[1 2 ][][3 4 ]
[1 3 ][][2 4 ]
[1 4 ][][2 3 ]
[2 3 ][][1 4 ]
[2 4 ][][1 3 ]
[3 4 ][][1 2 ]
```



## EchoLisp


```scheme

(lib 'list) ;; (combinations L k)

;; add a combination to each partition in ps
(define (pproduct c ps) (for/list ((x ps)) (cons c x)))

;; apply to any type of set S
;; ns is list of cardinals for each partition
;;     for all combinations Ci of n objects from  S
;;           set S <- LS minus Ci , set n <- next n , and recurse

(define (_partitions S ns )
    (cond
    ([empty? (rest ns)]  (list (combinations S (first ns))))
    (else
        (for/fold (parts null)
        ([c (combinations S (first ns))])
        (append
           parts
           (pproduct c (_partitions (set-substract S c) (rest ns))))))))

;; task : S = ( 0 , 1 ... n-1) args = ns
(define (partitions . args)
    (for-each
	 writeln
        (_partitions (range 1  (1+ (apply + args))) args )))

```

```txt

(partitions 1 1 1)
({ 1 } { 2 } { 3 })
({ 1 } { 3 } { 2 })
({ 2 } { 1 } { 3 })
({ 2 } { 3 } { 1 })
({ 3 } { 1 } { 2 })
({ 3 } { 2 } { 1 })

(partitions 2 0 2)
({ 1 2 } () { 3 4 })
({ 1 3 } () { 2 4 })
({ 1 4 } () { 2 3 })
({ 2 3 } () { 1 4 })
({ 2 4 } () { 1 3 })
({ 3 4 } () { 1 2 })

(for-each writeln (_partitions (make-set '(b a d c )) '(1 2 1)))
({ a } { b c } { d })
({ a } { b d } { c })
({ a } { c d } { b })
({ b } { a c } { d })
({ b } { a d } { c })
({ b } { c d } { a })
({ c } { a b } { d })
({ c } { a d } { b })
({ c } { b d } { a })
({ d } { a b } { c })
({ d } { a c } { b })
({ d } { b c } { a })

```



## Elixir

Brute force approach:

```elixir
defmodule Ordered do
  def partition([]), do: [[]]
  def partition(mask) do
    sum = Enum.sum(mask)
    if sum == 0 do
      [Enum.map(mask, fn _ -> [] end)]
    else
      Enum.to_list(1..sum)
      |> permute
      |> Enum.reduce([], fn perm,acc ->
           {_, part} = Enum.reduce(mask, {perm,[]}, fn num,{pm,a} ->
             {p, rest} = Enum.split(pm, num)
             {rest, [Enum.sort(p) | a]}
           end)
           [Enum.reverse(part) | acc]
         end)
      |> Enum.uniq
    end
  end

  defp permute([]), do: [[]]
  defp permute(list), do: for x <- list, y <- permute(list -- [x]), do: [x|y]
end

Enum.each([[],[0,0,0],[1,1,1],[2,0,2]], fn test_case ->
  IO.puts "\npartitions #{inspect test_case}:"
  Enum.each(Ordered.partition(test_case), fn part ->
    IO.inspect part
  end)
end)
```


```txt

partitions []:
[]

partitions [0, 0, 0]:
[[], [], []]

partitions [1, 1, 1]:
[[3], [2], [1]]
[[3], [1], [2]]
[[2], [3], [1]]
[[2], [1], [3]]
[[1], [3], [2]]
[[1], [2], [3]]

partitions [2, 0, 2]:
[[3, 4], [], [1, 2]]
[[2, 4], [], [1, 3]]
[[1, 4], [], [2, 3]]
[[2, 3], [], [1, 4]]
[[1, 3], [], [2, 4]]
[[1, 2], [], [3, 4]]

```



## GAP


```gap
FixedPartitions := function(arg)
	local aux;
	aux := function(i, u)
		local r, v, w;
		if i = Size(arg) then
			return [[u]];
		else
			r := [ ];
			for v in Combinations(u, arg[i]) do
				for w in aux(i + 1, Difference(u, v)) do
					Add(r, Concatenation([v], w));
				od;
			od;
			return r;
		fi;
	end;
	return aux(1, [1 .. Sum(arg)]);
end;


FixedPartitions(2, 0, 2);
# [ [ [ 1, 2 ], [  ], [ 3, 4 ] ], [ [ 1, 3 ], [  ], [ 2, 4 ] ],
#   [ [ 1, 4 ], [  ], [ 2, 3 ] ], [ [ 2, 3 ], [  ], [ 1, 4 ] ],
#   [ [ 2, 4 ], [  ], [ 1, 3 ] ], [ [ 3, 4 ], [  ], [ 1, 2 ] ] ]

FixedPartitions(1, 1, 1);
# [ [ [ 1 ], [ 2 ], [ 3 ] ], [ [ 1 ], [ 3 ], [ 2 ] ], [ [ 2 ], [ 1 ], [ 3 ] ],
#   [ [ 2 ], [ 3 ], [ 1 ] ], [ [ 3 ], [ 1 ], [ 2 ] ], [ [ 3 ], [ 2 ], [ 1 ] ] ]
```



## Go


```go
package main

import (
	"fmt"
	"os"
	"strconv"
)

func gen_part(n, res []int, pos int) {
	if pos == len(res) {
		x := make([][]int, len(n))
		for i, c := range res {
			x[c] = append(x[c], i+1)
		}

		fmt.Println(x)
		return
	}

	for i := range n {
		if n[i] == 0 {
			continue
		}
		n[i], res[pos] = n[i]-1, i
		gen_part(n, res, pos+1)
		n[i]++
	}
}

func ordered_part(n_parts []int) {
	fmt.Println("Ordered", n_parts)

	sum := 0
	for _, c := range n_parts {
		sum += c
	}

	gen_part(n_parts, make([]int, sum), 0)
}

func main() {
	if len(os.Args) < 2 {
		ordered_part([]int{2, 0, 2})
		return
	}
	n := make([]int, len(os.Args)-1)
	var err error
	for i, a := range os.Args[1:] {
		n[i], err = strconv.Atoi(a)
		if err != nil {
			fmt.Println(err)
			return
		}
		if n[i] < 0 {
			fmt.Println("negative partition size not meaningful")
			return
		}
	}
	ordered_part(n)
}
```

Example command line use:

```txt

> op
Ordered [2 0 2]
[[1 2] [] [3 4]]
[[1 3] [] [2 4]]
[[1 4] [] [2 3]]
[[2 3] [] [1 4]]
[[2 4] [] [1 3]]
[[3 4] [] [1 2]]

> op 1 1 1
Ordered [1 1 1]
[[1] [2] [3]]
[[1] [3] [2]]
[[2] [1] [3]]
[[3] [1] [2]]
[[2] [3] [1]]
[[3] [2] [1]]

> op 1 2 3 4 | head
Ordered [1 2 3 4]
[[1] [2 3] [4 5 6] [7 8 9 10]]
[[1] [2 3] [4 5 7] [6 8 9 10]]
[[1] [2 3] [4 5 8] [6 7 9 10]]
[[1] [2 3] [4 5 9] [6 7 8 10]]
[[1] [2 3] [4 5 10] [6 7 8 9]]
[[1] [2 3] [4 6 7] [5 8 9 10]]
[[1] [2 3] [4 6 8] [5 7 9 10]]
[[1] [2 3] [4 6 9] [5 7 8 10]]
[[1] [2 3] [4 6 10] [5 7 8 9]]

```



## Groovy

Solution:

```groovy
def partitions = { int... sizes ->
    int n = (sizes as List).sum()
    def perms = n == 0 ? [[]] : (1..n).permutations()
    Set parts = perms.collect { p -> sizes.collect { s -> (0..<s).collect { p.pop() } as Set } }
    parts.sort{ a, b ->
        if (!a) return 0
    def comp = [a,b].transpose().find { aa, bb -> aa != bb }
    if (!comp) return 0
    def recomp = comp.collect{ it as List }.transpose().find { aa, bb -> aa != bb }
        if (!recomp) return 0
        return recomp[0] <=> recomp[1]
    }
}
```


Test:

```groovy
partitions(2, 0, 2).each {
    println it
}
```


Output:

```txt
[[1, 2], [], [3, 4]]
[[1, 3], [], [2, 4]]
[[1, 4], [], [2, 3]]
[[2, 3], [], [1, 4]]
[[2, 4], [], [1, 3]]
[[3, 4], [], [1, 2]]
```



## Haskell


```haskell
import Data.List ((\\))

comb :: Int -> [a] -> [[a]]
comb 0 _      = [[]]
comb _ []     = []
comb k (x:xs) = map (x:) (comb (k-1) xs) ++ comb k xs

partitions :: [Int] -> [[[Int]]]
partitions xs = p [1..sum xs] xs
    where p _ []      = [[]]
          p xs (k:ks) = [ cs:rs | cs <- comb k xs, rs <- p (xs \\ cs) ks ]

main = print $ partitions [2,0,2]
```


An alternative where <code>\\</code> is not needed anymore because <code>comb</code> now not only
keeps the chosen elements but also the not chosen elements together in a tuple.


```haskell
comb :: Int -> [a] -> [([a],[a])]
comb 0 xs     = [([],xs)]
comb _ []     = []
comb k (x:xs) = [ (x:cs,zs) | (cs,zs) <- comb (k-1) xs ] ++
                [ (cs,x:zs) | (cs,zs) <- comb  k    xs ]

partitions :: [Int] -> [[[Int]]]
partitions xs = p [1..sum xs] xs
    where p _ []      = [[]]
          p xs (k:ks) = [ cs:rs | (cs,zs) <- comb k xs, rs <- p zs ks ]

main = print $ partitions [2,0,2]
```


Output:


```txt

[[[1,2],[],[3,4]],[[1,3],[],[2,4]],[[1,4],[],[2,3]],[[2,3],[],[1,4]],[[2,4],[],[1,3]],[[3,4],[],[1,2]]]

```


Faster by keeping track of the length of lists:

```haskell
-- choose m out of n items, return tuple of chosen and the rest
choose aa _ 0 = [([], aa)]
choose aa@(a:as) n m
	| n == m = [(aa, [])]
	| otherwise =	map (\(x,y) -> (a:x, y)) (choose as (n-1) (m-1)) ++
			map (\(x,y) -> (x, a:y)) (choose as (n-1) m)

partitions x = combos [1..n] n x where
	n = sum x
	combos _ _ [] = [[]]
	combos s n (x:xs) = [ l : r |	(l,rest) <- choose s n x,
					r <- combos rest (n - x) xs]


main = mapM_ print $ partitions [5,5,5]
```



## J


Brute force approach:


```j
require'stats'
partitions=: ([,] {L:0 (i.@#@, -. [)&;)/"1@>@,@{@({@comb&.> +/\.)
```


First we compute each of the corresponding combinations for each argument, then we form their cartesian product and then we restructure each of those products by: eliminating from values populating the the larger set combinations the combinations already picked from the smaller set and using the combinations from the larger set to index into the options which remain.

Examples:


```j
   partitions 2 0 2
┌───┬┬───┐
│0 1││2 3│
├───┼┼───┤
│0 2││1 3│
├───┼┼───┤
│0 3││1 2│
├───┼┼───┤
│1 2││0 3│
├───┼┼───┤
│1 3││0 2│
├───┼┼───┤
│2 3││0 1│
└───┴┴───┘
   partitions 1 1 1
┌─┬─┬─┐
│0│1│2│
├─┼─┼─┤
│0│2│1│
├─┼─┼─┤
│1│0│2│
├─┼─┼─┤
│1│2│0│
├─┼─┼─┤
│2│0│1│
├─┼─┼─┤
│2│1│0│
└─┴─┴─┘
   #partitions 2 3 5
2520
   #partitions 5 7 11
|out of memory: partitions
|   #    partitions 5 7 11
   */ (! +/\.)5 7 11
1070845776
   #partitions 3 5 7
360360
   */ (! +/\.)3 5 7
360360
```


Here's some intermediate results for that first example:


```J
   +/\. 2 0 2
4 2 2
   ({@comb&.> +/\.) 2 0 2
┌─────────────────────────┬──┬─────┐
│┌───┬───┬───┬───┬───┬───┐│┌┐│┌───┐│
││0 1│0 2│0 3│1 2│1 3│2 3││││││0 1││
│└───┴───┴───┴───┴───┴───┘│└┘│└───┘│
└─────────────────────────┴──┴─────┘
   >@,@{@({@comb&.> +/\.) 2 0 2
┌───┬┬───┐
│0 1││0 1│
├───┼┼───┤
│0 2││0 1│
├───┼┼───┤
│0 3││0 1│
├───┼┼───┤
│1 2││0 1│
├───┼┼───┤
│1 3││0 1│
├───┼┼───┤
│2 3││0 1│
└───┴┴───┘
```


In other words, initially we just work with relevant combinations (working from right to left). To understand the step which produces the final result, consider this next sequence of results (J's <code>/</code> operator works from right to left, as that's the pattern established by assignment operations, and because that has some interesting and useful mathematical properties):


```J
   ([,] {L:0 (i.@#@, -. [)&;)/0 1;0 1
┌───┬───┐
│0 1│2 3│
└───┴───┘
   ([,] {L:0 (i.@#@, -. [)&;)/0 1;0 1;0 1
┌───┬───┬───┐
│0 1│2 3│4 5│
└───┴───┴───┘
   ([,] {L:0 (i.@#@, -. [)&;)/0 1;0 1;0 1;0 1
┌───┬───┬───┬───┐
│0 1│2 3│4 5│6 7│
└───┴───┴───┴───┘
```


Breaking down that last example:


```J
   (<0 1) ([,] {L:0 (i.@#@, -. [)&;)0 1;2 3;4 5
┌───┬───┬───┬───┐
│0 1│2 3│4 5│6 7│
└───┴───┴───┴───┘
```


Here, on the right hand side we form 0 1 0 1 2 3 4 5, count how many things are in it (8), form 0 1 2 3 4 5 6 7 from that and then remove 0 1 (the values in the left argument) leaving us with 2 3 4 5 6 7. Meanwhile, on the left side, keep our left argument intact and use the indices in the remaining boxes to select from the right argument. In theoretical terms this is not particularly efficient, but we are working with very short lists here (because otherwise we run out of memory for the result as a whole), so the actual cost is trivial. Also note that sequential loops tend to be faster than nested loops (though we do get the effect of a nested loop, here - and that was the theoretical inefficiency).


## JavaScript


===Functional (ES 5)===

```JavaScript
(function () {
  'use strict';

  // [n] -> [[[n]]]
  function partitions(a1, a2, a3) {
    var n = a1 + a2 + a3;

    return combos(range(1, n), n, [a1, a2, a3]);
  }

  function combos(s, n, xxs) {
    if (!xxs.length) return [[]];

    var x = xxs[0],
        xs = xxs.slice(1);

    return mb( choose(s, n, x),                 function (l_rest) {
    return mb( combos(l_rest[1], (n - x), xs),  function (r) {
      // monadic return/injection requires 1 additional
      // layer of list nesting:
      return [ [l_rest[0]].concat(r) ];

    })});
  }

  function choose(aa, n, m) {
    if (!m) return [[[], aa]];

    var a = aa[0],
        as = aa.slice(1);

    return n === m ? (
      [[aa, []]]
    ) : (
      choose(as, n - 1, m - 1).map(function (xy) {
        return [[a].concat(xy[0]), xy[1]];
      }).concat(choose(as, n - 1, m).map(function (xy) {
        return [xy[0], [a].concat(xy[1])];
      }))
    );
  }

  // GENERIC

  // Monadic bind (chain) for lists
  function mb(xs, f) {
    return [].concat.apply([], xs.map(f));
  }

  // [m..n]
  function range(m, n) {
    return Array.apply(null, Array(n - m + 1)).map(function (x, i) {
      return m + i;
    });
  }

  // EXAMPLE

  return partitions(2, 0, 2);

})();
```


```JavaScript
[[[1, 2], [], [3, 4]],
 [[1, 3], [], [2, 4]],
 [[1, 4], [], [2, 3]],
 [[2, 3], [], [1, 4]],
 [[2, 4], [], [1, 3]],
 [[3, 4], [], [1, 2]]]
```



## jq

''The approach adopted here is similar to the [[#Python]] solution''.

```jq
# Generate a stream of the distinct combinations of r items taken from the input array.
def combination(r):
  if r > length or r < 0 then empty
  elif r == length then .
  else  ( [.[0]] + (.[1:]|combination(r-1))),
        ( .[1:]|combination(r))
  end;

# Input: a mask, that is, an array of lengths.
# Output: a stream of the distinct partitions defined by the mask.
def partition:

  # partition an array of entities, s, according to a mask presented as input:
  def p(s):
    if length == 0 then []
    else . as $mask
    | (s | combination($mask[0])) as $c
    | [$c] + ($mask[1:] | p(s - $c))
    end;
 . as $mask | p( [range(1; 1 + ($mask|add))] );
```

'''Example''':

```jq
([],[0,0,0],[1,1,1],[2,0,2])
  | . as $test_case
  |  "partitions \($test_case):" , ($test_case | partition), ""
```

```sh
$ jq -M -n -c -r -f Ordered_partitions.jq

partitions []:
[]

partitions [0,0,0]:
[[],[],[]]

partitions [1,1,1]:
[[1],[2],[3]]
[[1],[3],[2]]
[[2],[1],[3]]
[[2],[3],[1]]
[[3],[1],[2]]
[[3],[2],[1]]

partitions [2,0,2]:
[[1,2],[],[3,4]]
[[1,3],[],[2,4]]
[[1,4],[],[2,3]]
[[2,3],[],[1,4]]
[[2,4],[],[1,3]]
[[3,4],[],[1,2]]
```



## Julia

The method used, as seen in the function masked(), is to take a brute force permutation of size n = sum of partition,
partition it using the provided mask, and then sort the inner lists in order to properly filter duplicates.

```Julian

using Combinatorics

function masked(mask, lis)
    combos = []
    idx = 1
    for step in mask
        if(step < 1)
            push!(combos, Array{Int,1}[])
        else
            push!(combos, sort(lis[idx:idx+step-1]))
            idx += step
        end
    end
    Array{Array{Int, 1}, 1}(combos)
end

function orderedpartitions(mask)
    tostring(masklis) = replace("$masklis", r"Array{Int\d?\d?,1}|Int\d?\d?", "")
    join([tostring(lis) for lis in unique([masked(mask, p)
                        for p in permutations(1:sum(mask))])], "\n")
end

println(orderedpartitions([2, 0, 2]))
println(orderedpartitions([1, 1, 1]))


```

```txt

[[1, 2], [], [3, 4]]
[[1, 3], [], [2, 4]]
[[1, 4], [], [2, 3]]
[[2, 3], [], [1, 4]]
[[2, 4], [], [1, 3]]
[[3, 4], [], [1, 2]]
[[1], [2], [3]]
[[1], [3], [2]]
[[2], [1], [3]]
[[2], [3], [1]]
[[3], [1], [2]]
[[3], [2], [1]]

```



## Kotlin


```scala
// version 1.1.3

fun nextPerm(perm: IntArray): Boolean {
    val size = perm.size
    var k = -1
    for (i in size - 2 downTo 0) {
        if (perm[i] < perm[i + 1]) {
            k = i
            break
        }
    }
    if (k == -1) return false  // last permutation
    for (l in size - 1 downTo k) {
        if (perm[k] < perm[l]) {
           val temp = perm[k]
           perm[k] = perm[l]
           perm[l] = temp
           var m = k + 1
           var n = size - 1
           while (m < n) {
               val temp2 = perm[m]
               perm[m++] = perm[n]
               perm[n--] = temp2
           }
           break
        }
    }
    return true
}

fun List<Int>.isMonotonic(): Boolean {
    for (i in 1 until this.size) {
        if (this[i] < this[i - 1]) return false
    }
    return true
}

fun main(args: Array<String>) {
    val sizes = args.map { it.toInt() }
    println("Partitions for $sizes:\n[")
    val totalSize = sizes.sum()
    val perm = IntArray(totalSize) { it + 1 }

    do {
        val partition = mutableListOf<List<Int>>()
        var sum = 0
        var isValid = true
        for (size in sizes) {
            if (size == 0) {
                partition.add(emptyList<Int>())
            }
            else if (size == 1) {
                partition.add(listOf(perm[sum]))
            }
            else {
                val sl = perm.slice(sum until sum + size)
                if (!sl.isMonotonic()) {
                    isValid = false
                    break
                }
                partition.add(sl)
            }
            sum += size
        }
        if (isValid) println("  $partition")
    }
    while (nextPerm(perm))
    println("]")
}
```


Combined output of 3 separate runs with different command line parameters:

```txt

Partitions for [0, 0, 0]:
[
  [[], [], []]
]

Partitions for [2, 0, 2]:
[
  [[1, 2], [], [3, 4]]
  [[1, 3], [], [2, 4]]
  [[1, 4], [], [2, 3]]
  [[2, 3], [], [1, 4]]
  [[2, 4], [], [1, 3]]
  [[3, 4], [], [1, 2]]
]

Partitions for [1, 1, 1]:
[
  [[1], [2], [3]]
  [[1], [3], [2]]
  [[2], [1], [3]]
  [[2], [3], [1]]
  [[3], [1], [2]]
  [[3], [2], [1]]
]

```



## Lua

A pretty verbose solution. Maybe somebody can replace with something terser/better.

```lua
--- Create a list {1,...,n}.
local function range(n)
  local res = {}
  for i=1,n do
    res[i] = i
  end
  return res
end

--- Return true if the element x is in t.
local function isin(t, x)
  for _,x_t in ipairs(t) do
    if x_t == x then return true end
  end
  return false
end

--- Return the sublist from index u to o (inclusive) from t.
local function slice(t, u, o)
  local res = {}
  for i=u,o do
    res[#res+1] = t[i]
  end
  return res
end

--- Compute the sum of the elements in t.
-- Assume that t is a list of numbers.
local function sum(t)
  local s = 0
  for _,x in ipairs(t) do
    s = s + x
  end
  return s
end

--- Generate all combinations of t of length k (optional, default is #t).
local function combinations(m, r)
  local function combgen(m, n)
    if n == 0 then coroutine.yield({}) end
    for i=1,#m do
      if n == 1 then coroutine.yield({m[i]})
      else
        for m0 in coroutine.wrap(function() combgen(slice(m, i+1, #m), n-1) end) do
          coroutine.yield({m[i], unpack(m0)})
        end
      end
    end
  end
  return coroutine.wrap(function() combgen(m, r) end)
end

--- Generate a list of partitions into fized-size blocks.
local function partitions(...)
  local function helper(s, ...)
    local args = {...}
    if #args == 0 then return {% templatetag openvariable %}{% templatetag closevariable %} end
    local res = {}
    for c in combinations(s, args[1]) do
      local s0 = {}
      for _,x in ipairs(s) do if not isin(c, x) then s0[#s0+1] = x end end
      for _,r in ipairs(helper(s0, unpack(slice(args, 2, #args)))) do
        res[#res+1] = {{unpack(c)}, unpack(r)}
      end
    end
    return res
  end
  return helper(range(sum({...})), ...)
end

-- Print the solution
io.write "["
local parts = partitions(2,0,2)
for i,tuple in ipairs(parts) do
  io.write "("
  for j,set in ipairs(tuple) do
    io.write "{"
    for k,element in ipairs(set) do
      io.write(element)
      if k ~= #set then io.write(", ") end
    end
    io.write "}"
    if j ~= #tuple then io.write(", ") end
  end
  io.write ")"
  if i ~= #parts then io.write(", ") end
end
io.write "]"
io.write "\n"
```


Output:


```txt

[({1, 2}, {}, {3, 4}), ({1, 3}, {}, {2, 4}), ({1, 4}, {}, {2, 3}), ({2, 3}, {}, {1, 4}), ({2, 4}, {}, {1, 3}), ({3, 4}, {}, {1, 2})]

```



## Mathematica


This code works as follows:

'''Permutations''' finds all permutations of the numbers ranging from 1 to n.

'''w''' finds the required partition for an individual permutation.

'''m''' finds partitions for all permutations.

'''Sort''' and '''Union''' eliminate duplicates.


```Mathematica

w[partitions_]:=Module[{s={},t=Total@partitions,list=partitions,k}, n=Length[list];
    While[n>0,s=Join[s,{Take[t,(k=First[list])]}];t=Drop[t,k];list=Rest[list];n--]; s]

m[p_]:=(Sort/@#)&/@(w[#,p]&/@Permutations[Range@Total[p]])//Union

```



'''Usage'''

Grid displays the output in a table.


```Mathematica

Grid@m[{2, 0, 2}]

Grid@m[{1, 1, 1}]

```


[[File:Example.png]]


## Perl

Code 1: threaded generator method.  This code demonstrates how to make something like Python's
generators or Go's channels by using Thread::Queue.  Granted, this is horribly inefficient, with constantly creating and killing threads and whatnot (every time a partition is created, a thread is made to produce the next partition, so thousands if not millions of threads live and die, depending on the problem size).  But algorithms are often more naturally expressed in a coroutine manner -- for this example, "making a new partition" and "picking elements for a partition" can be done in separate recursions cleanly if so desired.  It's about 20 times slower than the next code example, so there.


```perl
use Thread 'async';
use Thread::Queue;

sub make_slices {
        my ($n, @avail) = (shift, @{ +shift });

        my ($q, @part, $gen);
        $gen = sub {
                my $pos = shift;        # where to start in the list
                if (@part == $n) {
                        # we accumulated enough for a partition, emit them and
                        # wait for main thread to pick them up, then back up
                        $q->enqueue(\@part, \@avail);
                        return;
                }

                # obviously not enough elements left to make a partition, back up
                return if (@part + @avail < $n);

                for my $i ($pos .. @avail - 1) {                # try each in turn
                        push @part, splice @avail, $i, 1;       # take one
                        $gen->($i);                             # go deeper
                        splice @avail, $i, 0, pop @part;        # put it back
                }
        };

        $q = new Thread::Queue;
        (async{ &$gen;                  # start the main work load
                $q->enqueue(undef)      # signal that there's no more data
        })->detach;     # let the thread clean up after itself, not my problem

        return $q;
}

my $qa = make_slices(4, [ 0 .. 9 ]);
while (my $a = $qa->dequeue) {
        my $qb = make_slices(2, $qa->dequeue);

        while (my $b = $qb->dequeue) {
                my $rb = $qb->dequeue;
                print "@$a | @$b | @$rb\n";
        }
}

```


Code 2: Recursive solution.
```perl
use List::Util 1.33 qw(sum pairmap);

sub partition {
    my @mask = @_;
    my $last = sum @mask or return [map {[]} 0..$#mask];

    return pairmap {
        $b ? do {
            local $mask[$a] = $b - 1;
            map { push @{$_->[$a]}, $last; $_ }
                partition(@mask);
        } : ()
    } %mask[0..$#mask];
}

# Input & Output handling:

print "(" . join(', ', map { "{".join(', ', @$_)."}" } @$_) . ")\n"
    for partition( @ARGV ? @ARGV : (2, 0, 2) );
```


Example command-line use:

```txt

> ./ordered_partitions.pl
({3, 4}, {}, {1, 2})
({2, 4}, {}, {1, 3})
({1, 4}, {}, {2, 3})
({2, 3}, {}, {1, 4})
({1, 3}, {}, {2, 4})
({1, 2}, {}, {3, 4})

> ./ordered_partitions.pl 1 1 1
({3}, {2}, {1})
({3}, {1}, {2})
({2}, {3}, {1})
({1}, {3}, {2})
({2}, {1}, {3})
({1}, {2}, {3})

```


The set of ordered partitions is not returned in lexicographical order itself; but it's supposed to be a set so that's hopefully okay. (One could sort the output before printing, but (unlike in Perl 6) Perl's built-in sort routine cannot meaningfully compare arrays without being passed a custom comparator to do that, which is a little messy and thus omitted here.)


## Perl 6

```perl6
sub partition(@mask is copy) {
    my @op;
    my $last = [+] @mask or return [] xx 1;
    for @mask.kv -> $k, $v {
        next unless $v;
        temp @mask[$k] -= 1;
        for partition @mask -> @p {
            @p[$k].push: $last;
            @op.push: @p;
        }
    }
    return @op;
}

.say for reverse partition [2,0,2];
```

```txt
[[1, 2], (Any), [3, 4]]
[[1, 3], (Any), [2, 4]]
[[2, 3], (Any), [1, 4]]
[[1, 4], (Any), [2, 3]]
[[2, 4], (Any), [1, 3]]
[[3, 4], (Any), [1, 2]]
```



## Phix

Note that the builtin permute() function returns results in an idiosyncratic manner, so we use sort a lot and check for duplicates,
which might make this a tad inefficient.

```Phix
function partitions(sequence s)
integer N = sum(s)
sequence set = tagset(N), perm,
         results = {}, result, resi
    for i=1 to factorial(N) do
        perm = permute(i,set)
        integer n = 1
        result = {}
        for j=1 to length(s) do
            resi = {}
            for k=1 to s[j] do
                resi = append(resi,perm[n])
                n += 1
            end for
            result = append(result,sort(resi))
        end for
        if not find(result,results) then
            results = append(results,result)
        end if
    end for
    return sort(results)
end function

ppOpt({pp_Nest,1})
pp(partitions({2,0,2}))
pp(partitions({1,1,1}))
pp(partitions({1,2,0,1}))
pp(partitions({}))
pp(partitions({0,0,0}))
```

```txt

{{{1,2}, {}, {3,4}},
 {{1,3}, {}, {2,4}},
 {{1,4}, {}, {2,3}},
 {{2,3}, {}, {1,4}},
 {{2,4}, {}, {1,3}},
 {{3,4}, {}, {1,2}}}
{{{1}, {2}, {3}},
 {{1}, {3}, {2}},
 {{2}, {1}, {3}},
 {{2}, {3}, {1}},
 {{3}, {1}, {2}},
 {{3}, {2}, {1}}}
{{{1}, {2,3}, {}, {4}},
 {{1}, {2,4}, {}, {3}},
 {{1}, {3,4}, {}, {2}},
 {{2}, {1,3}, {}, {4}},
 {{2}, {1,4}, {}, {3}},
 {{2}, {3,4}, {}, {1}},
 {{3}, {1,2}, {}, {4}},
 {{3}, {1,4}, {}, {2}},
 {{3}, {2,4}, {}, {1}},
 {{4}, {1,2}, {}, {3}},
 {{4}, {1,3}, {}, {2}},
 {{4}, {2,3}, {}, {1}}}
```



## PicoLisp

Uses the 'comb' function from [[Combinations#PicoLisp]]

```PicoLisp
(de partitions (Args)
   (let Lst (range 1 (apply + Args))
      (recur (Args Lst)
         (ifn Args
            '(NIL)
            (mapcan
               '((L)
                  (mapcar
                     '((R) (cons L R))
                     (recurse (cdr Args) (diff Lst L)) ) )
               (comb (car Args) Lst) ) ) ) ) )
```

Output:

```txt
: (more (partitions (2 0 2)))
((1 2) NIL (3 4))
((1 3) NIL (2 4))
((1 4) NIL (2 3))
((2 3) NIL (1 4))
((2 4) NIL (1 3))
((3 4) NIL (1 2))
-> NIL

: (more (partitions (1 1 1)))
((1) (2) (3))
((1) (3) (2))
((2) (1) (3))
((2) (3) (1))
((3) (1) (2))
((3) (2) (1))
-> NIL
```



## Python


```python
from itertools import combinations

def partitions(*args):
    def p(s, *args):
        if not args: return [[]]
        res = []
        for c in combinations(s, args[0]):
            s0 = [x for x in s if x not in c]
            for r in p(s0, *args[1:]):
                res.append([c] + r)
        return res
    s = range(sum(args))
    return p(s, *args)

print partitions(2, 0, 2)
```


An equivalent but terser solution.

```python
from itertools import combinations as comb

def partitions(*args):
    def minus(s1, s2): return [x for x in s1 if x not in s2]
    def p(s, *args):
        if not args: return [[]]
        return [[c] + r for c in comb(s, args[0]) for r in p(minus(s, c), *args[1:])]
    return p(range(1, sum(args) + 1), *args)

print partitions(2, 0, 2)
```


Output:


```txt

[[(0, 1), (), (2, 3)], [(0, 2), (), (1, 3)], [(0, 3), (), (1, 2)], [(1, 2), (), (0, 3)], [(1, 3), (), (0, 2)], [(2, 3), (), (0, 1)]]

```



Or, more directly, without importing the ''combinations'' library:
```python
'''Ordered Partitions'''


# partitions :: [Int] -> [[[Int]]]
def partitions(xs):
    '''Ordered partitions of xs.'''
    n = sum(xs)

    def go(s, n, ys):
        return [
            [l] + r
            for (l, rest) in choose(s)(n)(ys[0])
            for r in go(rest, n - ys[0], ys[1:])
        ] if ys else [[]]
    return go(enumFromTo(1)(n), n, xs)


# choose :: [Int] -> Int -> Int -> [([Int], [Int])]
def choose(xs):
    '''(m items chosen from n items, the rest)'''
    def go(xs, n, m):
        f = cons(xs[0])
        choice = choose(xs[1:])(n - 1)
        return [([], xs)] if 0 == m else (
            [(xs, [])] if n == m else (
                [first(f)(x) for x in choice(m - 1)] +
                [second(f)(x) for x in choice(m)]
            )
        )
    return lambda n: lambda m: go(xs, n, m)


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Tests of the partitions function'''

    f = partitions
    print(
        fTable(main.__doc__ + ':')(
            lambda x: '\n' + f.__name__ + '(' + repr(x) + ')'
        )(
            lambda ps: '\n\n' + '\n'.join(
                '    ' + repr(p) for p in ps
            )
        )(f)([
            [2, 0, 2],
            [1, 1, 1]
        ])
    )


# DISPLAY -------------------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# GENERIC -------------------------------------------------

# cons :: a -> [a] -> [a]
def cons(x):
    '''Construction of a list from x as head,
       and xs as tail.
    '''
    return lambda xs: [x] + xs


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# first :: (a -> b) -> ((a, c) -> (b, c))
def first(f):
    '''A simple function lifted to a function over a tuple,
       with f applied only the first of two values.
    '''
    return lambda xy: (f(xy[0]), xy[1])


# second :: (a -> b) -> ((c, a) -> (c, b))
def second(f):
    '''A simple function lifted to a function over a tuple,
       with f applied only the second of two values.
    '''
    return lambda xy: (xy[0], f(xy[1]))


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Tests of the partitions function:

partitions([2, 0, 2]) ->

    [[1, 2], [], [3, 4]]
    [[1, 3], [], [2, 4]]
    [[1, 4], [], [2, 3]]
    [[2, 3], [], [1, 4]]
    [[2, 4], [], [1, 3]]
    [[3, 4], [], [1, 2]]

partitions([1, 1, 1]) ->

    [[1], [2], [3]]
    [[1], [3], [2]]
    [[2], [1], [3]]
    [[2], [3], [1]]
    [[3], [1], [2]]
    [[3], [2], [1]]
```



## Racket


```Racket

#lang racket
(define (comb k xs)
  (cond [(zero? k)  (list (cons '() xs))]
        [(null? xs) '()]
        [else (append (for/list ([cszs (comb (sub1 k) (cdr xs))])
                        (cons (cons (car xs) (car cszs)) (cdr cszs)))
                      (for/list ([cszs (comb k (cdr xs))])
                        (cons (car cszs) (cons (car xs) (cdr cszs)))))]))
(define (partitions xs)
  (define (p xs ks)
    (if (null? ks)
      '(())
      (for*/list ([cszs (comb (car ks) xs)] [rs (p (cdr cszs) (cdr ks))])
        (cons (car cszs) rs))))
  (p (range 1 (add1 (foldl + 0 xs))) xs))

(define (run . xs)
  (printf "partitions~s:\n" xs)
  (for ([x (partitions xs)]) (printf "  ~s\n" x))
  (newline))

(run 2 0 2)
(run 1 1 1)

```


Output:

```txt

partitions(2 0 2):
  ((1 2) () (3 4))
  ((1 3) () (2 4))
  ((1 4) () (2 3))
  ((2 3) () (1 4))
  ((2 4) () (1 3))
  ((3 4) () (1 2))

partitions(1 1 1):
  ((1) (2) (3))
  ((1) (3) (2))
  ((2) (1) (3))
  ((2) (3) (1))
  ((3) (1) (2))
  ((3) (2) (1))

```



## REXX


```rexx
//*REXX program displays the  ordered partitions  as:   orderedPartitions(i, j, k, ···). */
call orderedPartitions  2,0,2                    /*Note:      2,,2      will also work. */
call orderedPartitions  1,1,1
call orderedPartitions  1,2,0,1                  /*Note:      1,2,,1    will also work. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
orderedPartitions: procedure;  #=arg();   bot.=;   top.=;   low=;    high=;    d=123456789
t=0                                              /*T:   is the sum of all the arguments.*/
          do i=1  for #;       t=t + arg(i)      /*sum all the highest numbers in parts.*/
          end   /*i*/                            /* [↑]  may have an omitted argument.  */
hdr= ' partitions for: '                         /*define the start of the header text. */
  do j=1  for #;               _= arg(j)         /*  _:  is the    Jth   argument.      */
  len.j=max(1, _)                                /*LEN:  length of args.  «0 is special»*/
  bot.j=left(d, _);         if _==0 then bot.j=0 /*define the  bottom  number for range.*/
  top.j=right(left(d,t),_); if _==0 then top.j=0 /*  "     "     top      "    "    "   */
  @.j=left(d, t);           if _==0 then   @.j=0 /*define the digits used for  VERIFY.  */
  hdr=hdr _                                      /*build (by appending)  display header.*/
  low=low || bot.j;         high=high || top.j   /*the low and high numbers for DO below*/
  end   /*j*/
                                                 /* [↓]  same as:   okD=left('0'd, t+1) */
              /*define the legal digits to be used.  */
okD=left(0 || d,  t + 1)                         /*define the legal digits to be used.  */
say;   hdr=center(hdr" ",  60, '═');     say hdr /*display centered title for the output*/
say                                              /*show a blank line  (as a separator). */
    do g=low  to high                            /* [↑]  generate the ordered partitions*/
    if verify(g, okD) \==0  then iterate         /*filter out unwanted partitions (digs)*/
    p=1                                          /*P:  is the position of a decimal dig.*/
    $=                                           /*$:  will be the transformed numbers. */
       do k=1  for #;   _=substr(g, p, len.k)    /*verify the partitions numbers.       */
       if verify(_, @.k) \==0  then iterate g    /*is the decimal digit not valid ?     */
       !=                                        /* [↓]  validate the decimal number.   */
       if @.k\==0  then do j=1  for length(_);     z=substr(_, j, 1)        /*get a dig.*/
                        if pos(z, $)\==0               then iterate g       /*previous ?*/
                        !=!','z                                             /*add comma.*/
                        if j==1                        then iterate         /*is firstt?*/
                        if z<=substr(_, j-1, 1)        then iterate g       /*ordered  ?*/
                        if pos(z, _, 1 +pos(z, _))\==0 then iterate g       /*duplicate?*/
                        end   /*j*/
       p=p + len.k                               /*point to the next decimal digit (num)*/
       $=$ '  {'strip(translate(!, ,0), ,",")'}' /*dress number up by suppessing LZ ··· */
       end   /*k*/
    say center($, length(hdr) )                  /*display numbers in ordered partition.*/
    end      /*g*/
return
```

```txt

══════════════════ partitions for:  2 0 2 ══════════════════

                      {1,2}   {}   {3,4}
                      {1,3}   {}   {2,4}
                      {1,4}   {}   {2,3}
                      {2,3}   {}   {1,4}
                      {2,4}   {}   {1,3}
                      {3,4}   {}   {1,2}

══════════════════ partitions for:  1 1 1 ══════════════════

                        {1}   {2}   {3}
                        {1}   {3}   {2}
                        {2}   {1}   {3}
                        {2}   {3}   {1}
                        {3}   {1}   {2}
                        {3}   {2}   {1}

═════════════════ partitions for:  1 2 0 1 ═════════════════

                    {1}   {2,3}   {}   {4}
                    {1}   {2,4}   {}   {3}
                    {1}   {3,4}   {}   {2}
                    {2}   {1,3}   {}   {4}
                    {2}   {1,4}   {}   {3}
                    {2}   {3,4}   {}   {1}
                    {3}   {1,2}   {}   {4}
                    {3}   {1,4}   {}   {2}
                    {3}   {2,4}   {}   {1}
                    {4}   {1,2}   {}   {3}
                    {4}   {1,3}   {}   {2}
                    {4}   {2,3}   {}   {1}

```



## Ruby

'''Brute force approach:''' simple but very slow

```ruby
def partition(mask)
  return [[]] if mask.empty?
  [*1..mask.inject(:+)].permutation.map {|perm|
    mask.map {|num_elts| perm.shift(num_elts).sort }
  }.uniq
end
```


'''Recursive version:'''  faster
```ruby
def part(s, args)
  return [[]] if args.empty?
  s.combination(args[0]).each_with_object([]) do |c, res|
    part(s - c, args[1..-1]).each{|r| res << ([c] + r)}
  end
end
def partitions(args)
  return [[]] if args.empty?
  part((1..args.inject(:+)).to_a, args)
end
```


'''Test:'''

```ruby
[[],[0,0,0],[1,1,1],[2,0,2]].each do |test_case|
  puts "partitions #{test_case}:"
  partition(test_case).each{|part| p part }
  puts
end
```

```txt

partitions []:
[]

partitions [0, 0, 0]:
[[], [], []]

partitions [1, 1, 1]:
[[1], [2], [3]]
[[1], [3], [2]]
[[2], [1], [3]]
[[2], [3], [1]]
[[3], [1], [2]]
[[3], [2], [1]]

partitions [2, 0, 2]:
[[1, 2], [], [3, 4]]
[[1, 3], [], [2, 4]]
[[1, 4], [], [2, 3]]
[[2, 3], [], [1, 4]]
[[2, 4], [], [1, 3]]
[[3, 4], [], [1, 2]]

```



## Sidef

```ruby
func part(_,    {.is_empty}) { [[]] }
func partitions({.is_empty}) { [[]] }

func part(s, args) {
  gather {
    s.combinations(args[0], { |*c|
      part(s - c, args.ft(1)).each{|r| take([c] + r) }
    })
  }
}

func partitions(args) {
  part(@(1..args.sum), args)
}

[[],[0,0,0],[1,1,1],[2,0,2]].each { |test_case|
  say "partitions #{test_case}:"
  partitions(test_case).each{|part| say part }
  print "\n"
}
```


```txt

partitions []:
[]

partitions [0, 0, 0]:
[[], [], []]

partitions [1, 1, 1]:
[[1], [2], [3]]
[[1], [3], [2]]
[[2], [1], [3]]
[[2], [3], [1]]
[[3], [1], [2]]
[[3], [2], [1]]

partitions [2, 0, 2]:
[[1, 2], [], [3, 4]]
[[1, 3], [], [2, 4]]
[[1, 4], [], [2, 3]]
[[2, 3], [], [1, 4]]
[[2, 4], [], [1, 3]]
[[3, 4], [], [1, 2]]

```



## Tcl

```tcl
package require Tcl 8.5
package require struct::set

# Selects all k-sized combinations from a list.
# "Borrowed" from elsewhere on RC
proc selectCombinationsFrom {k l} {
    if {$k == 0} {return {}} elseif {$k == [llength $l]} {return [list $l]}
    set all {}
    set n [expr {[llength $l] - [incr k -1]}]
    for {set i 0} {$i < $n} {} {
        set first [lindex $l $i]
	incr i
        if {$k == 0} {
            lappend all $first
	} else {
	    foreach s [selectCombinationsFrom $k [lrange $l $i end]] {
		lappend all [list $first {*}$s]
	    }
        }
    }
    return $all
}

# Construct the partitioning of a given list
proc buildPartitions {lst n args} {
    # Base case when we have no further partitions to process
    if {[llength $args] == 0} {
	return [list [list $lst]]
    }
    set result {}
    set c [selectCombinationsFrom $n $lst]
    if {[llength $c] == 0} {set c [list $c]}
    foreach comb $c {
	# Sort necessary for "nice" order
	set rest [lsort -integer [struct::set difference $lst $comb]]
	foreach p [buildPartitions $rest {*}$args] {
	    lappend result [list $comb {*}$p]
	}
    }
    return $result
}

# Wrapper that assembles the initial list and calls the partitioner
proc partitions args {
    set sum [tcl::mathop::+ {*}$args]
    set startingSet {}
    for {set i 1} {$i <= $sum} {incr i} {
	lappend startingSet $i
    }

    return [buildPartitions $startingSet {*}$args]
}
```

Demonstration code:

```tcl
puts [partitions 1 1 1]
puts [partitions 2 2]
puts [partitions 2 0 2]
puts [partitions 2 2 0]
```

Output:

```txt

{1 2 3} {1 3 2} {2 1 3} {2 3 1} {3 1 2} {3 2 1}
```



## Ursala


```Ursala
#import std
#import nat

opart =

-+
  ~&art^?\~&alNCNC ^|JalSPfarSPMplrDSL/~& ^DrlPrrPlXXS/~&rt ^DrlrjXS/~&l choices@lrhPX,
  ^\~& nrange/1+ sum:-0+-
```

The library function <code>choices</code> used in this solution takes a pair <math>(s,k)</math> and returns the set of all subsets of <math>s</math> having cardinality <math>k</math>. The library function <code>nrange</code> takes a pair of natural numbers to the minimum consecutive sequence containing them. The <code>sum</code> function adds a pair of natural numbers.
```Ursala
#cast %nLLL

test = opart <2,0,2>
```

output:

```txt
<
   <<1,2>,<>,<3,4>>,
   <<1,3>,<>,<2,4>>,
   <<1,4>,<>,<2,3>>,
   <<2,3>,<>,<1,4>>,
   <<2,4>,<>,<1,3>>,
   <<3,4>,<>,<1,2>>>
```



## zkl

```zkl
fcn partitions(args){
   args=vm.arglist;
   s:=(1).pump(args.sum(0),List);  // (1,2,3,...)
   fcn(s,args,p){
      if(not args) return(T(T));
      res:=List();
      foreach c in (Utils.Helpers.pickNFrom(args[0],s)){
	 s0:=s.copy().removeEach(c);
	 foreach r in (self.fcn(s0,args[1,*])){ res.append(T(c).extend(r)) }
      }
      res
   }(s,args)
}
```


```zkl
args:=vm.arglist.apply("toInt");  // aka argv[1..]
if(not args) args=T(2,0,2);
partitions(args.xplode()).pump(Console.println,Void);
// or: foreach p in (partitions(1,1,1)){ println(p) }
```

```txt

$ zkl bbb
L(L(1,2),L(),L(3,4))
L(L(1,3),L(),L(2,4))
L(L(1,4),L(),L(2,3))
L(L(2,3),L(),L(1,4))
L(L(2,4),L(),L(1,3))
L(L(3,4),L(),L(1,2))

$ zkl bbb 1 1 1
L(L(1),L(2),L(3))
L(L(1),L(3),L(2))
L(L(2),L(1),L(3))
L(L(2),L(3),L(1))
L(L(3),L(1),L(2))
L(L(3),L(2),L(1))

```

