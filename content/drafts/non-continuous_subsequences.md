+++
title = "Non-continuous subsequences"
description = ""
date = 2019-04-06T01:08:27Z
aliases = []
[extra]
id = 2778
[taxonomies]
categories = []
tags = []
+++

{{task|Discrete math}}
Consider some sequence of elements. (It differs from a mere set of elements by having an ordering among members.)

A ''subsequence'' contains some subset of the elements of this sequence, in the same order.

A ''continuous'' subsequence is one in which no elements are missing between the first and last elements of the subsequence.

Note: Subsequences are defined ''structurally'', not by their contents. 
So a sequence ''a,b,c,d'' will always have the same subsequences and continuous subsequences, no matter which values are substituted; it may even be the same value.

'''Task''': Find all non-continuous subsequences for a given sequence. 

Example: For the sequence   ''1,2,3,4'',   there are five non-continuous subsequences, namely: 
::::*   ''1,3''
::::*   ''1,4''
::::*   ''2,4''
::::*   ''1,3,4''
::::*   ''1,2,4''

'''Goal''': There are different ways to calculate those subsequences. Demonstrate algorithm(s) that are natural for the language.


## Ada


### Recursive


```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Non_Continuous is
   type Sequence is array (Positive range <>) of Integer;
   procedure Put_NCS
             (  Tail : Sequence;                -- To generate subsequences of
                Head : Sequence := (1..0 => 1); -- Already generated
                Contiguous : Boolean := True    -- It is still continuous
             )  is
   begin
      if not Contiguous and then Head'Length > 1 then
         for I in Head'Range loop
            Put (Integer'Image (Head (I)));
         end loop;
         New_Line;
      end if;
      if Tail'Length /= 0 then 
         declare
            New_Head : Sequence (Head'First..Head'Last + 1);
         begin
            New_Head (Head'Range) := Head;
            for I in Tail'Range loop
               New_Head (New_Head'Last) := Tail (I);
               Put_NCS
               (  Tail => Tail (I + 1..Tail'Last),
                  Head => New_Head,
                  Contiguous => Contiguous and then (I = Tail'First or else Head'Length = 0)
               );
            end loop;
         end;
      end if;
   end Put_NCS;
begin
   Put_NCS ((1,2,3));     New_Line;
   Put_NCS ((1,2,3,4));   New_Line;
   Put_NCS ((1,2,3,4,5)); New_Line;
end Test_Non_Continuous;
```


{{out}}
<pre style="height:30ex;overflow:scroll"> 1 3

 1 2 4
 1 3
 1 3 4
 1 4
 2 4

 1 2 3 5
 1 2 4
 1 2 4 5
 1 2 5
 1 3
 1 3 4
 1 3 4 5
 1 3 5
 1 4
 1 4 5
 1 5
 2 3 5
 2 4
 2 4 5
 2 5
 3 5
```



## ALGOL 68


### Recursive

{{trans|Ada}} - note: This specimen retains the original [[Non-continuous subsequences#Ada|Ada]] coding style. 
{{wont work with|ALGOL 68|Revision 1 - ANDIF, OREL extensions to language used - used to mimic Ada}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
PROC test non continuous = VOID: BEGIN
   MODE SEQMODE = CHAR;
   MODE SEQ = [1:0]SEQMODE;
   MODE YIELDSEQ = PROC(SEQ)VOID;

   PROC gen ncs =
             (  SEQ tail,       # To generate subsequences of #
                SEQ head,       #           Already generated #
                BOOL contiguous,#      It is still continuous #
                YIELDSEQ yield
             )  VOID:
   BEGIN
      IF NOT contiguous ANDTH UPB head > 1 THEN
         yield (head)
      FI;
      IF UPB tail /= 0 THEN 
            [UPB head+1]SEQMODE new head;
            new head [:UPB head] := head;
            FOR i TO UPB tail DO
               new head [UPB new head] := tail [i];
               gen ncs
               (  tail[i + 1:UPB tail],
                  new head,
                  contiguous ANDTH (i = LWB tail OREL UPB head = 0),
                  yield
               )
            OD
      FI
   END # put ncs #;

 # FOR SEQ seq IN # gen ncs(("a","e","i","o","u"), (), TRUE, # ) DO ( #
 ##   (SEQ seq)VOID:
      print((seq, new line))
 # OD # )
END; test non continuous
```

{{out}}

```txt

aeiu
aeo
aeou
aeu
ai
aio
aiou
aiu
ao
aou
au
eiu
eo
eou
eu
iu

```


### Iterative

{{trans|C}} - note: This specimen retains the original [[Non-continuous subsequences#C|C]] coding style. 

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

Note: This specimen can only handle sequences of length less than ''bits width'' of '''bits'''.

```algol68
MODE SEQMODE = STRING;
MODE SEQ = [1:0]SEQMODE;
MODE YIELDSEQ = PROC(SEQ)VOID;

PROC gen ncs = (SEQ seq, YIELDSEQ yield)VOID:
BEGIN
  IF UPB seq - 1 > bits width THEN stop FI;
  [UPB seq]SEQMODE out;  INT upb out;

  BITS lim := 16r1 SHL UPB seq;
  BITS upb k := lim SHR 1;
  # assert(lim); #

  BITS empty = 16r000000000; # const #

  FOR j TO ABS lim-1 DO
    INT state := 1;
    BITS k1 := upb k; 
    WHILE k1 NE empty DO
      BITS b := BIN j AND k1;
      CASE state IN
        # state 1 # IF b NE empty THEN state +:= 1 FI,
        # state 2 # IF b EQ empty THEN state +:= 1 FI,
        # state 3 # 
          BEGIN
            IF b EQ empty THEN GO TO continue k1 FI;
            upb out := 0; 
            BITS k2 := upb k; FOR i WHILE k2 NE empty DO
              IF (BIN j AND k2) NE empty THEN out[upb out +:= 1] := seq[i] FI;
              k2 := k2 SHR 1
            OD;
            yield(out[:upb out]);
            k1 := empty # empty: ending containing loop #
         END
      ESAC;
      continue k1: k1 := k1 SHR 1
    OD
  OD
END;

main:(
  []STRING seqs = ("a","e","i","o","u");
# FOR SEQ seq IN # gen ncs(seqs, # ) DO ( #
##   (SEQ seq)VOID:
    print((seq, new line))
# OD # )
)
```

{{out}}

```txt

iu
eu
eo
eou
eiu
au
ao
aou
ai
aiu
aio
aiou
aeu
aeo
aeou
aeiu

```



## AutoHotkey

using filtered templates
ahk forum: [http://www.autohotkey.com/forum/viewtopic.php?p=277328#277328 discussion]


```AutoHotkey
MsgBox % noncontinuous("a,b,c,d,e", ",")
MsgBox % noncontinuous("1,2,3,4", ",")

noncontinuous(list, delimiter)
{
stringsplit, seq, list, %delimiter%
n := seq0                                            ; sequence length
Loop % x := (1<<n) - 1 {                                  ; try all 0-1 candidate sequences
   If !RegExMatch(b:=ToBin(A_Index,n),"^0*1*0*$") {  ; drop continuous subsequences
      Loop Parse, b
         t .= A_LoopField ? seq%A_Index% " " : ""         ; position -> number
		 t .= "`n"                                   ; new sequences in new lines
   }
}
return t
}

ToBin(n,W=16) {  ; LS W-bits of Binary representation of n
   Return W=1 ? n&1 : ToBin(n>>1,W-1) . n&1
}
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      DIM list1$(3)
      list1$() = "1", "2", "3", "4"
      PRINT "For [1, 2, 3, 4] non-continuous subsequences are:"
      PROCnon_continuous_subsequences(list1$())
      DIM list2$(4)
      list2$() = "1", "2", "3", "4", "5"
      PRINT "For [1, 2, 3, 4, 5] non-continuous subsequences are:"
      PROCnon_continuous_subsequences(list2$())
      END
      
      DEF PROCnon_continuous_subsequences(l$())
      LOCAL i%, j%, g%, n%, r%, s%, w%, a$, b$
      n% = DIM(l$(),1)
      FOR s% = 0 TO n%-2
        FOR g% = s%+1 TO n%-1
          a$ = "["
          FOR i% = s% TO g%-1
            a$ += l$(i%) + ", "
          NEXT
          FOR w% = 1 TO n%-g%
            r% = n%+1-g%-w%
            FOR i% = 1 TO 2^r%-1 STEP 2
              b$ = a$
              FOR j% = 0 TO r%-1
                IF i% AND 2^j% b$ += l$(g%+w%+j%) + ", "
              NEXT
              PRINT LEFT$(LEFT$(b$)) + "]"
            NEXT i%
          NEXT w%
        NEXT g%
      NEXT s%
      ENDPROC
```

{{out}}

```txt

For [1, 2, 3, 4] non-continuous subsequences are:
[1, 3]
[1, 3, 4]
[1, 4]
[1, 2, 4]
[2, 4]
For [1, 2, 3, 4, 5] non-continuous subsequences are:
[1, 3]
[1, 3, 4]
[1, 3, 5]
[1, 3, 4, 5]
[1, 4]
[1, 4, 5]
[1, 5]
[1, 2, 4]
[1, 2, 4, 5]
[1, 2, 5]
[1, 2, 3, 5]
[2, 4]
[2, 4, 5]
[2, 5]
[2, 3, 5]
[3, 5]

```



## Bracmat


```Bracmat
( ( noncontinuous
  =   sub
    .     ( sub
          =   su a nc
            .   !arg:(?su.?nc)
              &   !su
                :   %
                    %?a
                    ( %:[%(sub$(!sjt.!nc !a))
                    |   ?
                      & !nc:~
                      & out$(!nc !a)
                      & ~
                    )
          )
        & sub$(dummy !arg.)
      | 
  )
& noncontinuous$(e r n i t)
);

```

{{out}}

```txt
e n t
e n
e n i
e n i t
e i
e i t
e t
e r i
e r i t
e r t
e r n t
r i
r i t
r t
r n t
n t
```



## C

Note: This specimen can only handle lists of length less than the number of bits in an '''int'''. 

```C>#include <assert.h

#include <stdio.h>

int main(int c, char **v)
{
	unsigned int n = 1 << (c - 1), i = n, j, k;
	assert(n);

	while (i--) {
		if (!(i & (i + (i & -(int)i)))) // consecutive 1s
			continue;

		for (j = n, k = 1; j >>= 1; k++)
			if (i & j) printf("%s ", v[k]);

		putchar('\n');
	}

	return 0;
}
```

Example use:

```txt

$ ./noncont 1 2 3 4
1 2 4 
1 3 4 
1 3 
2 4 
1 4 
$ ./noncont 1 2 3 4 5 6 7 8 9 0 | wc -l
968

```


Using "consecutive + gap + any subsequence" to produce disjointed sequences:

```c>#include <assert.h

#include <stdio.h>
#include <stdlib.h>

void binprint(unsigned int n, unsigned int m)
{
	char c[sizeof(n) * 8 + 1];
	int i = 0;
	while (m >>= 1)	c[i++] = n & m ? '#' : '-';
	c[i] = 0;
	puts(c);
}

int main(int c, char **v)
{
	unsigned int n, gap, left, right;
	if (c < 2 || ! (n = 1 << atoi(v[1]))) n = 16;

	for (gap = 2; gap < n; gap <<= 1)
		for (left = gap << 1; left < n; left |= left << 1)
			for (right = 1; right < gap; right++)
				binprint(left | right, n);

	return 0;
}
```


### Recursive method

Using recursion and a state transition table.

```c>#include <stdio.h


typedef unsigned char sint;
enum states { s_blnk = 0, s_tran, s_cont, s_disj };

/* Recursively look at each item in list, taking both choices of
   picking the item or not.  The state at each step depends on prvious
   pickings, with the state transition table:
	blank + no pick -> blank
	blank + pick -> contiguous
	transitional + no pick -> transitional
	transitional + pick -> disjoint
	contiguous + no pick -> transitional
	contiguous + pick -> contiguous
	disjoint + pick -> disjoint
	disjoint + no pick -> disjoint
   At first step, before looking at any item, state is blank.
   Because state is known at each step and needs not be calculated,
   it can be quite fast.
*/
unsigned char tbl[][2] = {
	{ s_blnk, s_cont },
	{ s_tran, s_disj },
	{ s_tran, s_cont },
	{ s_disj, s_disj },
};

void pick(sint n, sint step, sint state, char **v, unsigned long bits)
{
	int i, b;
	if (step == n) {
		if (state != s_disj) return;
		for (i = 0, b = 1; i < n; i++, b <<= 1)
			if ((b & bits)) printf("%s ", v[i]);
		putchar('\n');
		return;
	}

	bits <<= 1;
	pick(n, step + 1, tbl[state][0], v, bits); /* no pick */
	pick(n, step + 1, tbl[state][1], v, bits | 1); /* pick */
}

int main(int c, char **v)
{
	if (c - 1 >= sizeof(unsigned long) * 4)
		printf("Too many items");
	else
		pick(c - 1, 0, s_blnk, v + 1, 0);
	return 0;
}
```
running it:

```txt
% ./a.out 1 2 3 4
1 3 
1 4 
2 4 
1 2 4 
1 3 4
% ./a.out 1 2 3 4 5 6 7 8 9 0 | wc -l
968
```



## C++


```cpp

/*
 * Nigel Galloway, July 19th., 2017 - Yes well is this any better?
 */
class N{
  uint n,i,g,e,l;
public:
  N(uint n): n(n-1),i{},g{},e(1),l(n-1){}
  bool hasNext(){
    g=(1<<n)+e;for(i=l;i<n;++i) g+=1<<i;
    if (l==2)             {l=--n; e=1; return true;}
    if (e<((1<<(l-1))-1)) {++e;        return true;}
                           e=1; --l;   return (l>0);
  }
  uint next() {return g;}
};

```

Which may be used as follows:

```cpp

int main(){
  N n(4);
  while (n.hasNext()) std::cout << n.next() << "\t* " << std::bitset<4>(n.next()) << std::endl;
}

```

{{out}}

```txt

9       * 1001
10      * 1010
11      * 1011
13      * 1101
5       * 0101

```

I can count the length of the sequence:

```cpp

int main(){
  N n(31);
  int z{};for (;n.hasNext();++z); std::cout << z << std::endl;
}

```

{{out}}

```txt

2147483151

```



## C sharp


```csharp
using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    public static void Main() {
        var sequence = new[] { "A", "B", "C", "D" };
        foreach (var subset in Subsets(sequence.Length).Where(s => !IsContinuous(s))) {
            Console.WriteLine(string.Join(" ", subset.Select(i => sequence[i])));
        }
    }
    
    static IEnumerable<List<int>> Subsets(int length) {
        int[] values = Enumerable.Range(0, length).ToArray();
        var stack = new Stack<int>(length);
        for (int i = 0; stack.Count > 0 || i < length; ) {
            if (i < length) {
                stack.Push(i++);
                yield return (from index in stack.Reverse() select values[index]).ToList();
            } else {
                i = stack.Pop() + 1;
                if (stack.Count > 0) i = stack.Pop() + 1;
            }
        }
    }

    static bool IsContinuous(List<int> list) => list[list.Count - 1] - list[0] + 1 == list.Count;

}
```

{{out}}

```txt

A B D
A C
A C D
A D
B D

```



## Clojure


Here's a simple approach that uses the clojure.contrib.combinatorics library to generate subsequences, and then filters out the continuous subsequences using a naïve subseq test:


```lisp

(use '[clojure.contrib.combinatorics :only (subsets)])

(defn of-min-length [min-length]
  (fn [s] (>= (count s) min-length)))

(defn runs [c l]
  (map (partial take l) (take-while not-empty (iterate rest c))))

(defn is-subseq? [c sub]
  (some identity (map = (runs c (count sub)) (repeat sub))))

(defn non-continuous-subsequences [s]
  (filter (complement (partial is-subseq? s)) (subsets s)))


(filter (of-min-length 2) (non-continuous-subsequences [:a :b :c :d]))

```



## CoffeeScript

Use binary bitmasks to enumerate our sequences.

```coffeescript

is_contigous_binary = (n) ->
  # return true if binary representation of n is
  # of the form 1+0+
  # examples:
  #     0 true
  #     1 true
  #   100 true
  #   110 true
  #  1001 false
  #  1010 false

  # special case zero, or you'll get an infinite loop later
  return true if n == 0

  # first remove 0s from end
  while n % 2 == 0
    n = n / 2
  
  # next, take advantage of the fact that a continuous
  # run of 1s would be of the form 2^n - 1
  is_power_of_two(n + 1)

is_power_of_two = (m) ->
  while m % 2 == 0
    m = m / 2
  m == 1

seq_from_bitmap = (arr, n) ->
  # grabs elements from array according to a bitmap
  # e.g. if n == 13 (1101), and arr = ['a', 'b', 'c', 'd'],
  # then return ['a', 'c', 'd'] (flipping bits to 1011, so
  # that least significant bit comes first)
  i = 0
  new_arr = []
  while n > 0
    if n % 2 == 1
      new_arr.push arr[i]
      n -= 1
    n /= 2
    i += 1
  new_arr

non_contig_subsequences = (arr) ->
  # Return all subsqeuences from an array that have a "hole" in
  # them.  The order of the subsequences is not specified here.
    
  # This algorithm uses binary counting, so it is limited to
  # small lists, but large lists would be unwieldy regardless.
  bitmasks = [0...Math.pow(2, arr.length)]
  (seq_from_bitmap arr, n for n in bitmasks when !is_contigous_binary n)

arr = [1,2,3,4]
console.log non_contig_subsequences arr
for n in [1..10]
  arr = [1..n]
  num_solutions = non_contig_subsequences(arr).length
  console.log "for n=#{n} there are #{num_solutions} solutions"

```


{{out}}

```txt

> coffee non_contig_subseq.coffee 
[ [ 1, 3 ],
  [ 1, 4 ],
  [ 2, 4 ],
  [ 1, 2, 4 ],
  [ 1, 3, 4 ] ]
for n=1 there are 0 solutions
for n=2 there are 0 solutions
for n=3 there are 1 solutions
for n=4 there are 5 solutions
for n=5 there are 16 solutions
for n=6 there are 42 solutions
for n=7 there are 99 solutions
for n=8 there are 219 solutions
for n=9 there are 466 solutions
for n=10 there are 968 solutions

```



## Common Lisp


<!-- kind thanks to people on #lisp for keeping me company while i debugged this one.
     originally i had (mapcar #'car ...) in function's last expression and sat dumbfounded
     looking at the screen wondering what's wrong for about half an hour -->


```lisp
(defun all-subsequences (list)
  (labels ((subsequences (tail &optional (acc '()) (result '()))
             "Return a list of the subsequence designators of the
              subsequences of tail. Each subsequence designator is a
              list of tails of tail, the subsequence being the first
              element of each tail."
             (if (endp tail)
               (list* (reverse acc) result)
               (subsequences (rest tail) (list* tail acc)
                             (append (subsequences (rest tail) acc) result))))
           (continuous-p (subsequence-d)
             "True if the designated subsequence is continuous."
             (loop for i in subsequence-d
                   for j on (first subsequence-d)
                   always (eq i j)))
           (designated-sequence (subsequence-d)
             "Destructively transforms a subsequence designator into
              the designated subsequence."
             (map-into subsequence-d 'first subsequence-d)))
    (let ((nc-subsequences (delete-if #'continuous-p (subsequences list))))
      (map-into nc-subsequences #'designated-sequence nc-subsequences))))
```


{{trans|Scheme}}


```lisp
(defun all-subsequences2 (list)
  (labels ((recurse (s list)
             (if (endp list)
                 (if (>= s 3)
                     '(())
                     '())
                 (let ((x (car list))
                       (xs (cdr list)))
                   (if (evenp s)
                       (append (mapcar (lambda (ys) (cons x ys))
                                       (recurse (+ s 1) xs))
                               (recurse s xs))
                       (append (mapcar (lambda (ys) (cons x ys))
                                       (recurse s xs))
                               (recurse (+ s 1) xs)))))))
    (recurse 0 list)))
```



## D


### Recursive Version

{{trans|Python}}

```d
T[][] ncsub(T)(in T[] seq, in uint s=0) pure nothrow @safe {
    if (seq.length) {
        typeof(return) aux;
        foreach (ys; ncsub(seq[1 .. $], s + !(s % 2)))
            aux ~= seq[0] ~ ys;
        return aux ~ ncsub(seq[1 .. $], s + s % 2);
    } else
        return new typeof(return)(s >= 3, 0);
}

void main() @safe {
    import std.stdio;

    [1, 2, 3].ncsub.writeln;
    [1, 2, 3, 4].ncsub.writeln;
    foreach (const nc; [1, 2, 3, 4, 5].ncsub)
        nc.writeln;
}
```

{{out}}

```txt
[[1, 3]]
[[1, 2, 4], [1, 3, 4], [1, 3], [1, 4], [2, 4]]
[1, 2, 3, 5]
[1, 2, 4, 5]
[1, 2, 4]
[1, 2, 5]
[1, 3, 4, 5]
[1, 3, 4]
[1, 3, 5]
[1, 3]
[1, 4, 5]
[1, 4]
[1, 5]
[2, 3, 5]
[2, 4, 5]
[2, 4]
[2, 5]
[3, 5]
```



### Faster Lazy Version

This version doesn't copy the sub-arrays.

```d
struct Ncsub(T) {
    T[] seq;

    int opApply(int delegate(ref T[]) dg) const {
        immutable n = seq.length;
        int result;
        auto S = new T[n];

        OUTER: foreach (immutable i; 1 .. 1 << n) {
            uint lenS;
            bool nc = false;
            foreach (immutable j; 0 .. n + 1) {
                immutable k = i >> j;
                if (k == 0) {
                    if (nc) {
                        auto auxS = S[0 .. lenS];
                        result = dg(auxS);
                        if (result)
                            break OUTER;
                    }
                    break;
                } else if (k % 2) {
                    S[lenS] = seq[j];
                    lenS++;
                } else if (lenS)
                    nc = true;
            }
        }

        return result;
    }
}

void main() {
    import std.array, std.range;

    //assert(24.iota.array.Ncsub!int.walkLength == 16_776_915);
    auto r = 24.iota.array;
    uint counter = 0;
    foreach (s; Ncsub!int(r))
        counter++;
    assert(counter == 16_776_915);
}
```



### Generator Version

This version doesn't copy the sub-arrays, and it's a little slower than the opApply-based version.

```d
import std.stdio, std.array, std.range, std.concurrency;

Generator!(T[]) ncsub(T)(in T[] seq) {
    return new typeof(return)({
        immutable n = seq.length;
        auto S = new T[n];

        foreach (immutable i; 1 .. 1 << n) {
            uint lenS = 0;
            bool nc = false;
            foreach (immutable j; 0 .. n + 1) {
                immutable k = i >> j;
                if (k == 0) {
                    if (nc)
                        yield(S[0 .. lenS]);
                    break;
                } else if (k % 2) {
                    S[lenS] = seq[j];
                    lenS++;
                } else if (lenS)
                    nc = true;
            }
        }
    });
}

void main() {
    assert(24.iota.array.ncsub.walkLength == 16_776_915);

    [1, 2, 3].ncsub.writeln;
    [1, 2, 3, 4].ncsub.writeln;
    foreach (const nc; [1, 2, 3, 4, 5].ncsub)
        nc.writeln;
}
```



## Elixir

{{trans|Erlang}}

```elixir
defmodule RC do
  defp masks(n) do
    maxmask = trunc(:math.pow(2, n)) - 1
    Enum.map(3..maxmask, &Integer.to_string(&1, 2))
    |> Enum.filter_map(&contains_noncont(&1), &String.rjust(&1, n, ?0)) # padding
  end
 
  defp contains_noncont(n) do
    Regex.match?(~r/10+1/, n)
  end
 
  defp apply_mask_to_list(mask, list) do
    Enum.zip(to_char_list(mask), list)
    |> Enum.filter_map(fn {include, _} -> include > ?0 end, fn {_, value} -> value end)
  end

  def ncs(list) do
    Enum.map(masks(length(list)), fn mask -> apply_mask_to_list(mask, list) end)
  end
end

IO.inspect RC.ncs([1,2,3])
IO.inspect RC.ncs([1,2,3,4])
IO.inspect RC.ncs('abcd')
```


{{out}}

```txt

[[1, 3]]
[[2, 4], [1, 4], [1, 3], [1, 3, 4], [1, 2, 4]]
['bd', 'ad', 'ac', 'acd', 'abd']

```



## Erlang

Erlang's not optimized for strings or math, so this is pretty inefficient. Nonetheless, it works by generating the set of all possible "bitmasks" (represented as strings), filters for those with non-continuous subsequences, and maps from that set over the list. One immediate point for optimization that would complicate the code a bit would be to compile the regular expression, the problem being where you'd put it.


```erlang
-module(rosetta).
-export([ncs/1]).

masks(N) ->
    MaxMask = trunc(math:pow(2, N)),
    Total = lists:map(fun(X) -> integer_to_list(X, 2) end,
                lists:seq(3, MaxMask)),
    Filtered = lists:filter(fun(X) -> contains_noncont(X) end, Total),
    lists:map(fun(X) -> string:right(X, N, $0) end, Filtered). % padding
   
contains_noncont(N) ->
    case re:run(N, "10+1") of
        {match, _} -> true;
        nomatch -> false
    end.

apply_mask_to_list(Mask, List) ->
    Zipped = lists:zip(Mask, List),
    Filtered = lists:filter(fun({Include, _}) -> Include > 48 end, Zipped),
    lists:map(fun({_, Value}) -> Value end, Filtered).

ncs(List) ->
    lists:map(fun(Mask) -> apply_mask_to_list(Mask, List) end,
                masks(length(List))).
```


{{out}}

```txt

Eshell V5.10.1  (abort with ^G)
1> c(rosetta).
{ok,rosetta}
2> rosetta:ncs([1,2,3,4]).
[[2,4],[1,4],[1,3],[1,3,4],[1,2,4]]

```


=={{header|F_Sharp|F#}}==
===Generate only the non-continuous subsequences===

```fsharp

(*
  A function to generate only the non-continuous subsequences.
  Nigel Galloway July 20th., 2017
*)
let N n =
  let     fn n = Seq.map (fun g->(2<<<n)+g)
  let rec fg n = seq{if n>0 then yield! seq{1..((1<<<n)-1)}|>fn n; yield! fg (n-1)|>fn n}
  Seq.collect fg ({1..(n-2)})

```

This may be used as follows:

```fsharp

let Ng ng = N ng |> Seq.iter(fun n->printf "%2d -> " n; {0..(ng-1)}|>Seq.iter (fun g->if (n&&&(1<<<g))>0 then printf "%d " (g+1));printfn "")
Ng 4

```

{{out}}

```txt

 5 -> 1 3 
 9 -> 1 4 
10 -> 2 4 
11 -> 1 2 4 
13 -> 1 3 4 

```

Counting the number of non-continuous subsequences is interesting:

```txt

> Seq.length (N 20);;                                                                 
Real: 00:00:00.169, CPU: 00:00:00.169, GC gen0: 0, gen1: 0
val it : int = 1048365
> Seq.length (N 23);;
Real: 00:00:01.238, CPU: 00:00:01.239, GC gen0: 0, gen1: 0
val it : int = 8388331
> Seq.length (N 24);;
Real: 00:00:02.520, CPU: 00:00:02.523, GC gen0: 0, gen1: 0
val it : int = 16776915
> Seq.length (N 25);;
Real: 00:00:04.926, CPU: 00:00:04.930, GC gen0: 0, gen1: 0
val it : int = 33554106

```


### Generate all subsequences and filter out the continuous


```fsharp

(*
  A function to filter out continuous subsequences.
  Nigel Galloway July 24th., 2017
*)
let Nonseq n=
  let fn = function
    |((n,0),true )->(n+1,1)
    |((n,_),false)->(n,0)
    |(n,_)        ->n
  {5..(1<<<n)-1}|>Seq.choose(fun i->if fst({0..n-1}|>Seq.takeWhile(fun n->(1<<<(n-1))<i)|>Seq.fold(fun n g->fn (n,(i&&&(1<<<g)>0)))(0,0)) > 1 then Some(i) else None)

```

Again counting the number of non-continuous subsequences

```txt

> Seq.length (Nonseq 20);;
Real: 00:00:02.356, CPU: 00:00:02.389, GC gen0: 183, gen1: 0
val it : int = 1048365
> Seq.length (Nonseq 23);;
Real: 00:00:20.714, CPU: 00:00:20.950, GC gen0: 1571, gen1: 0
val it : int = 8388331
> Seq.length (Nonseq 24);;
Real: 00:00:43.129, CPU: 00:00:43.601, GC gen0: 3216, gen1: 0
val it : int = 16776915
> Seq.length (Nonseq 25);;
Real: 00:01:28.853, CPU: 00:01:29.869, GC gen0: 6577, gen1: 0
val it : int = 33554106

```


### Conclusion

Find a better filter or use the generator.


## Go

Generate the power set (power sequence, actually) with a recursive function, but keep track of the state of the subsequence on the way down.  When you get to the bottom, if state == non-continuous, then include the subsequence.  It's just filtering merged in with generation.

```go
package main

import "fmt"

const ( // state:
    m   = iota // missing:  all elements missing so far
    c          // continuous:  all elements included so far are continuous
    cm         // one or more continuous followed by one or more missing
    cmc        // non-continuous subsequence
)

func ncs(s []int) [][]int {
    if len(s) < 3 {
        return nil
    }
    return append(n2(nil, s[1:], m), n2([]int{s[0]}, s[1:], c)...)
}

var skip = []int{m, cm, cm, cmc}
var incl = []int{c, c, cmc, cmc}

func n2(ss, tail []int, seq int) [][]int {
    if len(tail) == 0 {
        if seq != cmc {
            return nil
        }
        return [][]int{ss}
    }
    return append(n2(append([]int{}, ss...), tail[1:], skip[seq]),
        n2(append(ss, tail[0]), tail[1:], incl[seq])...)
}

func main() {
    ss := ncs([]int{1, 2, 3, 4})
    fmt.Println(len(ss), "non-continuous subsequences:")
    for _, s := range ss {
        fmt.Println("  ", s)
    }
}
```

{{out}}

```txt

5 non-continuous subsequences:
   [2 4]
   [1 4]
   [1 3]
   [1 3 4]
   [1 2 4]

```



## Haskell



### Generalized monadic filter



```haskell
action p x = if p x then succ x else x

fenceM p q s []     = guard (q s) >> return []
fenceM p q s (x:xs) = do
  (f,g) <- p 
  ys <- fenceM p q (g s) xs
  return $ f x ys

ncsubseq = fenceM [((:), action even), (flip const, action odd)] (>= 3) 0
```


{{out}}

```txt
*Main> ncsubseq [1..3]
[[1,3]]
*Main> ncsubseq [1..4]
[[1,2,4],[1,3,4],[1,3],[1,4],[2,4]]
*Main> ncsubseq [1..5]
[[1,2,3,5],[1,2,4,5],[1,2,4],[1,2,5],[1,3,4,5],[1,3,4],[1,3,5],[1,3],[1,4,5],[1,4],[1,5],[2,3,5],[2,4,5],[2,4],[2,5],[3,5]]
```



### Filtered templates


This implementation works by computing templates of all possible subsequences of the given length of sequence, discarding the continuous ones, then applying the remaining templates to the input list.


```haskell
continuous = null . dropWhile not . dropWhile id . dropWhile not
ncs xs = map (map fst . filter snd . zip xs) $
           filter (not . continuous) $
             mapM (const [True,False]) xs
```



### Recursive

Recursive method with powerset as helper function.


```haskell
import Data.List

poset = foldr (\x p -> p ++ map (x:) p) [[]]

ncsubs [] = [[]]
ncsubs (x:xs) = tail $ nc [x] xs
  where
    nc [_] [] = [[]]
    nc (_:x:xs) [] = nc [x] xs
    nc  xs (y:ys) = (nc (xs++[y]) ys) ++ map (xs++) (tail $ poset ys)
```


{{out}}

```txt

 *Main> ncsubs "aaa"
 ["aa"]
 (0.00 secs, 0 bytes)
 *Main> ncsubs [9..12]
 [[10,12],[9,10,12],[9,12],[9,11],[9,11,12]]
 (0.00 secs, 522544 bytes)
 *Main> ncsubs []
 [[]]
 (0.00 secs, 0 bytes)
 *Main> ncsubs [1]
 []
 (0.00 secs, 0 bytes)

```


A disjointed subsequence is a consecutive subsequence followed by a gap, 
then by any nonempty subsequence to its right:

```haskell
import Data.List (subsequences, tails, delete)

disjoint a = concatMap (cutAt a) [1..length a - 2] where
	cutAt s n = [a ++ b |	b <- delete [] (subsequences right),
				a <- init (tails left) ] where
		(left, _:right) = splitAt n s

main = print $ length $ disjoint [1..20]
```


Build a lexicographic list of consecutive subsequences, 
and a list of all subsequences, then subtract one from the other:

```haskell
import Data.List (inits, tails)

subseqs = foldr (\x s -> [x] : map (x:) s ++ s) []

consecs = concatMap (tail.inits) . tails

minus [] [] = []
minus (a:as) bb@(b:bs)
	| a == b = minus as bs
	| otherwise = a:minus as bb

disjoint s = (subseqs s) `minus` (consecs s)

main = mapM_ print $ disjoint [1..4]
```



## J

We select those combinations where the end of the first continuous subsequence appears before the start of the last continuous subsequence:


```J
allmasks=: 2 #:@i.@^ #
firstend=:1 0 i.&1@E."1 ]
laststart=: 0 1 {:@I.@E."1 ]
noncont=: <@#~ (#~ firstend < laststart)@allmasks
```


Example use:

```J
   noncont 1+i.4
┌───┬───┬───┬─────┬─────┐
│2 4│1 4│1 3│1 3 4│1 2 4│
└───┴───┴───┴─────┴─────┘
   noncont 'aeiou'
┌──┬──┬──┬───┬───┬──┬──┬───┬──┬───┬───┬────┬───┬───┬────┬────┐
│iu│eu│eo│eou│eiu│au│ao│aou│ai│aiu│aio│aiou│aeu│aeo│aeou│aeiu│
└──┴──┴──┴───┴───┴──┴──┴───┴──┴───┴───┴────┴───┴───┴────┴────┘
   #noncont i.10
968
```


Alternatively, since there are relatively few continuous sequences, we could specifically exclude them:


```J
contmasks=: a: ;@,  1 <:/~@i.&.>@i.@+ #
noncont=: <@#~ (allmasks -. contmasks)
```


(we get the same behavior from this implementation)


## Java


```java
public class NonContinuousSubsequences {

    public static void main(String args[]) {
        seqR("1234", "", 0, 0);
    }

    private static void seqR(String s, String c, int i, int added) {
        if (i == s.length()) {
            if (c.trim().length() > added)
                System.out.println(c);
        } else {
            seqR(s, c + s.charAt(i), i + 1, added + 1);
            seqR(s, c + ' ', i + 1, added);
        }
    }
}
```



```txt
12 4
1 34
1 3 
1  4
 2 4
```



## JavaScript

Uses powerset() function from [[Power Set#JavaScript|here]]. Uses a JSON stringifier from http://www.json.org/js.html

{{works with|SpiderMonkey}}

```javascript
function non_continuous_subsequences(ary) {
    var non_continuous = new Array();
    for (var i = 0; i < ary.length; i++) {
        if (! is_array_continuous(ary[i])) {
            non_continuous.push(ary[i]);
        }
    }
    return non_continuous;
}

function is_array_continuous(ary) {
    if (ary.length < 2)
        return true;
    for (var j = 1; j < ary.length; j++) {
        if (ary[j] - ary[j-1] != 1) {
            return false;
        }
    }
    return true;
}

load('json2.js'); /* http://www.json.org/js.html */

print(JSON.stringify( non_continuous_subsequences( powerset([1,2,3,4]))));
```


{{out}}

```txt
[[1,3],[1,4],[2,4],[1,2,4],[1,3,4]]
```



## jq

{{works with|jq|1.4}}
In order to handle arrays of more than a handful of elements, we define
non_continuous_subsequences/0 as a generator; that is, it produces a
stream of arrays, each of which is a non-continuous subsequence of the given sequence.

Since the non-continuous subsequences are dense in the set of all
subsets, we will use the powerset approach, and accordingly begin by
defining subsets/0 as a generator.

```jq
# Generate a stream of subsets of the input array
def subsets:
  if length == 0 then []
  else .[0] as $first
    | (.[1:] | subsets) 
    | ., ([$first] + .)
  end ;

# Generate a stream of non-continuous indices in the range 0 <= i < .
def non_continuous_indices:
  [range(0;.)] | subsets
  | select(length > 1 and length != 1 + .[length-1] - .[0]) ;

def non_continuous_subsequences:
  (length | non_continuous_indices) as $ix
  | [.[ $ix[] ]] ;
```

'''Example''': 
To show that the above approach can be used for relatively large n, let us count the number of non-continuous subsequences of [0, 1, ..., 19].

```jq
def count(f): reduce f as $i (0; . + 1);

count( [range(0;20)] | non_continuous_subsequences)

```

{{out}}
 $ jq -n -f powerset_generator.jq
 1048365


## Julia

{{works with|Julia|0.6}}

This solution uses an iterator over non-contiguous sub-sequences, <tt>NCSubSeq</tt>.  In the spirit of Julia's <tt>permutations</tt> and <tt>combinations</tt> built-ins, <tt>NCSubSeq</tt> provides an array of indices that can be used to create each subsequence from the full sequence.  Sub-sequences are indexed by integers whose bit patterns indicate which members are included.

<tt>NCSubSeq</tt> works by filtering indices according to whether all <tt>1</tt>s in these indices have bit pattern that are contiguous (using the <tt>iscontseq</tt> functions).  This is an easy to implement approach.  Greater efficiency might be achieved by exploiting the property that a sequence is contiguous if and only if its index is a difference of two powers of 2.  This property is used to create the <tt>length(NCSubSeq(n))</tt> function, which gives the number of non-contiguous sub-sequences of a sequence of length <tt>n</tt>.

<tt>NCSubSeq</tt> works transparently for sequence lengths up to <tt>WORD_SIZE-1</tt> (typically 63).  It can be extended to work for longer sequences by casting <tt>n</tt> to a larger integer, e.g. using <tt>Big(n)</tt>.  A more polished implementation would handle this extension behind the scenes.

'''Iterator and Functions'''

```Julia
iscontseq(n::Integer) = count_zeros(n) == leading_zeros(n) + trailing_zeros(n)
iscontseq(n::BigInt)  = !ismatch(r"0", rstrip(bin(n), '0'))

function makeint2seq(n::Integer)
    const idex = collect(1:n)
    function int2seq(m::Integer)
        d = digits(m, 2, n)
        idex[d .== 1]
    end
    return int2seq
end

struct NCSubSeq{T<:Integer}
    n::T
end

mutable struct NCSubState{T<:Integer}
    m::T
    m2s::Function
end

Base.iteratorsize(::NCSubSeq) = Base.HasLength()
Base.length(a::NCSubSeq) = 2 ^ a.n - a.n * (a.n + 1) ÷ 2 - 1

Base.start(a::NCSubSeq) = NCSubState(5, makeint2seq(a.n))
Base.done(a::NCSubSeq, as::NCSubState) = 2 ^ a.n - 3 < as.m
function Base.next(a::NCSubSeq, as::NCSubState)
    s = as.m2s(as.m)
    as.m += 1
    while iscontseq(as.m)
        as.m += 1
    end
    return (s, as)
end

n = 4
println("Testing NCSubSeq for ", n, " items:\n ", join(NCSubSeq(n), " "))

s = "Rosetta"
cs = split(s, "")
m = 10
n = length(NCSubSeq(length(s))) - m
println("\nThe first and last ", m, " NC sub-sequences of \"", s, "\":")
for (i, a) in enumerate(NCSubSeq(length(cs)))
    i <= m || n < i || continue
    println(@sprintf "%6d %s" i join(cs[a], ""))
    i == m || continue
    println("    .. ......")
end

using IterTools.chain

println("\nThe first and last ", m, " NC sub-sequences of \"", s, "\"")
for x in IterTools.chain(1:10, 20:10:40, big.(50:50:200))
    @printf "%7d → %d\n" x length(NCSubSeq(x))
end
```


{{out}}

```txt
Testing NCSubSeq for 4 items:
 [1, 3] [1, 4] [2, 4] [1, 2, 4] [1, 3, 4]

The first and last 10 NC sub-sequences of "Rosetta":
     1 Rs
     2 Re
     3 oe
     4 Roe
     5 Rse
     6 Rt
     7 ot
     8 Rot
     9 st
    10 Rst
    .. ......
    90 otta
    91 Rotta
    92 stta
    93 Rstta
    94 ostta
    95 Rostta
    96 Retta
    97 oetta
    98 Roetta
    99 Rsetta

The first and last 10 NC sub-sequences of "Rosetta"
      1 → 0
      2 → 0
      3 → 1
      4 → 5
      5 → 16
      6 → 42
      7 → 99
      8 → 219
      9 → 466
     10 → 968
     20 → 1048365
     30 → 1073741358
     40 → 1099511626955
     50 → 1125899906841348
    100 → 1267650600228229401496703200325
    150 → 1427247692705959881058285969449495136382735298
    200 → 1606938044258990275541962092341162602522202993782792835281275
```



## Kotlin


```scala
// version 1.1.2

fun <T> ncs(a: Array<T>) { 
    fun generate(m: Int, k: Int, c: IntArray) {
        if (k == m) {
            if (c[m - 1] != c[0] + m - 1) {
                for (i in 0 until m)  print("${a[c[i]]} ")                
                println()
            }
        }
        else { 
            for (j in 0 until a.size) {
                if (k == 0 || j > c[k - 1]) {
                    c[k] = j
                    generate(m, k + 1, c)
                }
            }
        }
    }

    for (m in 2 until a.size) {
        val c = IntArray(m) 
        generate(m, 0, c)
    }    
}  

fun main(args: Array<String>) {
    val a = arrayOf(1, 2, 3, 4)
    ncs(a)
    println()
    val ca = arrayOf('a', 'b', 'c', 'd', 'e')
    ncs(ca)
}
```


{{out}}

```txt

1 3 
1 4 
2 4 
1 2 4 
1 3 4 

a c 
a d 
a e 
b d 
b e 
c e 
a b d 
a b e 
a c d 
a c e 
a d e 
b c e 
b d e 
a b c e 
a b d e 
a c d e 

```


## M2000 Interpreter


```M2000 Interpreter

Module Non_continuous_subsequences (item$(), display){
	Function positions(n) {
		function onebit {
			=lambda b=false (&c)-> {
				=b :if c then  b~:c=not b
			}
		}
		dim k(n)=onebit(), p(n)
		m=true
		flush
		for i=1 to 2^n {
			for j=0 to n-1 :p(j)= k(j)(&m) :next 
			m1=p(0)
			m2=0
			for j=1 to n-1
				if m2 then if m1>p(j) then m2=2:exit for
				if m1 < p(j) then m2++
				m1=p(j)
			next
			if m2=2 then data cons(p())' push a copy of p() to end of stack
			m=true
		}
		=array([])
	}

	a=positions(len(item$()))
	if display then
		For i=0 to len(a)-1
			b=array(a,i)
			line$=format$("{0::-5})",i+1,)
			for j=0 to len(b)-1
				if array(b,j) then line$+=" "+item$(j)
			next
			print line$
			doc$<=line$+{
			}
		next
	end if
	line$="Non continuous subsequences:"+str$(len(a))
	Print line$
	doc$<=line$+{
	}	
}
global doc$
document doc$   ' change string to document object
Non_continuous_subsequences ("1","2","3","4"), true
Non_continuous_subsequences ("a","e","i","o","u"), true
Non_continuous_subsequences ("R","o","s","e","t","t","a"), true
Non_continuous_subsequences ("1","2","3","4","5","6","7","8","9","0"), false
clipboard doc$

```


{{out}}
<pre style="height:30ex;overflow:scroll">
    1) 1 3
    2) 1 4
    3) 2 4
    4) 1 2 4
    5) 1 3 4
Non continuous subsequences: 5
    1) a i
    2) a o
    3) e o
    4) a e o
    5) a i o
    6) a u
    7) e u
    8) a e u
    9) i u
   10) a i u
   11) e i u
   12) a e i u
   13) a o u
   14) e o u
   15) a e o u
   16) a i o u
Non continuous subsequences: 16
    1) R s
    2) R e
    3) o e
    4) R o e
    5) R s e
    6) R t
    7) o t
    8) R o t
    9) s t
   10) R s t
   11) o s t
   12) R o s t
   13) R e t
   14) o e t
   15) R o e t
   16) R s e t
   17) R t
   18) o t
   19) R o t
   20) s t
   21) R s t
   22) o s t
   23) R o s t
   24) e t
   25) R e t
   26) o e t
   27) R o e t
   28) s e t
   29) R s e t
   30) o s e t
   31) R o s e t
   32) R t t
   33) o t t
   34) R o t t
   35) s t t
   36) R s t t
   37) o s t t
   38) R o s t t
   39) R e t t
   40) o e t t
   41) R o e t t
   42) R s e t t
   43) R a
   44) o a
   45) R o a
   46) s a
   47) R s a
   48) o s a
   49) R o s a
   50) e a
   51) R e a
   52) o e a
   53) R o e a
   54) s e a
   55) R s e a
   56) o s e a
   57) R o s e a
   58) t a
   59) R t a
   60) o t a
   61) R o t a
   62) s t a
   63) R s t a
   64) o s t a
   65) R o s t a
   66) e t a
   67) R e t a
   68) o e t a
   69) R o e t a
   70) s e t a
   71) R s e t a
   72) o s e t a
   73) R o s e t a
   74) R t a
   75) o t a
   76) R o t a
   77) s t a
   78) R s t a
   79) o s t a
   80) R o s t a
   81) e t a
   82) R e t a
   83) o e t a
   84) R o e t a
   85) s e t a
   86) R s e t a
   87) o s e t a
   88) R o s e t a
   89) R t t a
   90) o t t a
   91) R o t t a
   92) s t t a
   93) R s t t a
   94) o s t t a
   95) R o s t t a
   96) R e t t a
   97) o e t t a
   98) R o e t t a
   99) R s e t t a
Non continuous subsequences: 99
Non continuous subsequences: 968

</pre >


## Mathematica

We make all the subsets then filter out the continuous ones:


```Mathematica
GoodBad[i_List]:=Not[MatchQ[Differences[i],{1..}|{}]]
n=5
Select[Subsets[Range[n]],GoodBad]
```


gives back:


```Mathematica
 {{1,3},{1,4},{1,5},{2,4},{2,5},{3,5},{1,2,4},{1,2,5},{1,3,4},{1,3,5},{1,4,5},{2,3,5},{2,4,5},{1,2,3,5},{1,2,4,5},{1,3,4,5}}
```



## Nim

{{trans|Python}}

```nim
import sequtils

proc ncsub[T](se: seq[T], s = 0): seq[seq[T]] =
  result = @[]
  if se.len > 0:
    let
      x = se[0..0]
      xs = se[1 .. -1]
      p2 = s mod 2
      p1 = (s + 1) mod 2
    for ys in ncsub(xs, s + p1):
      result.add(x & ys)
    result.add(ncsub(xs, s + p2))
  elif s >= 3:
    result.add(@[])

echo "ncsub(", toSeq 1.. 3, ") = ", ncsub(toSeq 1..3)
echo "ncsub(", toSeq 1.. 4, ") = ", ncsub(toSeq 1..4)
echo "ncsub(", toSeq 1.. 5, ") = ", ncsub(toSeq 1..5)
```

{{out}}

```txt
ncsub(@[1, 2, 3]) = @[@[1, 3]]
ncsub(@[1, 2, 3, 4]) = @[@[1, 2, 4], @[1, 3, 4], @[1, 3], @[1, 4], @[2, 4]]
ncsub(@[1, 2, 3, 4, 5]) = @[@[1, 2, 3, 5], @[1, 2, 4, 5], @[1, 2, 4], @[1, 2, 5], @[1, 3, 4, 5], @[1, 3, 4], @[1, 3, 5], @[1, 3], @[1, 4, 5], @[1, 4], @[1, 5], @[2, 3, 5], @[2, 4, 5], @[2, 4], @[2, 5], @[3, 5]]
```



## OCaml


{{trans|Generalized monadic filter}}


```ocaml
let rec fence s = function
    [] ->
      if s >= 3 then
        [[]]
      else
        []

  | x :: xs ->
      if s mod 2 = 0 then
        List.map
          (fun ys -> x :: ys)
          (fence (s + 1) xs)
        @
          fence s xs
      else
        List.map
          (fun ys -> x :: ys)
          (fence s xs)
        @
          fence (s + 1) xs

let ncsubseq = fence 0
```


{{out}}

```txt
# ncsubseq [1;2;3];;
- : int list list = [[1; 3]]
# ncsubseq [1;2;3;4];;
- : int list list = [[1; 2; 4]; [1; 3; 4]; [1; 3]; [1; 4]; [2; 4]]
# ncsubseq [1;2;3;4;5];;
- : int list list =
[[1; 2; 3; 5]; [1; 2; 4; 5]; [1; 2; 4]; [1; 2; 5]; [1; 3; 4; 5]; [1; 3; 4];
 [1; 3; 5]; [1; 3]; [1; 4; 5]; [1; 4]; [1; 5]; [2; 3; 5]; [2; 4; 5]; 
 [2; 4]; [2; 5]; [3; 5]]
```



## Oz

A nice application of finite set constraints. We just describe what we want and the constraint system will deliver it:

```oz
declare
  fun {NCSubseq SeqList}
     Seq = {FS.value.make SeqList}
     proc {Script Result}
        %% the result is a subset of Seq
        {FS.subset Result Seq}

        %% at least one element of Seq is missing
        local Gap in
           {FS.include Gap Seq}
           {FS.exclude Gap Result}
           %% and this element is between the smallest
           %% and the largest elements of the subsequence
           Gap >: {FS.int.min Result}
           Gap <: {FS.int.max Result}
        end
      
        %% enumerate all such sets
        {FS.distribute naive [Result]}
     end
  in
     {Map {SearchAll Script} FS.reflect.lowerBoundList}
  end
in
  {Inspect {NCSubseq [1 2 3 4]}}
```



## PARI/GP

Just a simple script, but it's I/O bound so efficiency isn't a concern. (Almost all subsequences are non-contiguous so looping over all possibilities isn't that bad. For length 20 about 99.98% of subsequences are non-contiguous.)

```parigp
noncontig(n)=n>>=valuation(n,2);n++;n>>=valuation(n,2);n>1;
nonContigSubseq(v)={
  for(i=5,2^#v-1,
    if(noncontig(i),
      print(vecextract(v,i))
    )
  )
};
nonContigSubseq([1,2,3])
nonContigSubseq(["a","b","c","d","e"])
```

{{out}}

```txt
[1, 3]

["a", "c"]
["a", "d"]
["b", "d"]
["a", "b", "d"]
["a", "c", "d"]
["a", "e"]
["b", "e"]
["a", "b", "e"]
["c", "e"]
["a", "c", "e"]
["b", "c", "e"]
["a", "b", "c", "e"]
["a", "d", "e"]
["b", "d", "e"]
["a", "b", "d", "e"]
["a", "c", "d", "e"]
```



## Perl


```perl
my ($max, @current);
sub non_continuous {
        my ($idx, $has_gap) = @_;
        my $found;

        for ($idx .. $max) {
                push @current, $_;
                # print "@current\n" if $has_gap; # uncomment for huge output
                $found ++ if $has_gap;
                $found += non_continuous($_ + 1, $has_gap)   if $_ < $max;
                pop @current;
                $has_gap = @current;   # don't set gap flag if it's empty still
        }
        $found;
}

$max = 20;
print "found ", non_continuous(1), " sequences\n";
```

{{out}}

```txt
found 1048365 sequences
```



## Perl 6

{{works with|rakudo|2015-09-24}}

```perl6
sub non_continuous_subsequences ( *@list ) {
    @list.combinations.grep: { 1 != all( .[ 0 ^.. .end] Z- .[0 ..^ .end] ) }
}

say non_continuous_subsequences( 1..3 )».gist;
say non_continuous_subsequences( 1..4 )».gist;
say non_continuous_subsequences(   ^4 ).map: {[<a b c d>[.list]].gist};
```

{{out}}

```txt
((1 3))
((1 3) (1 4) (2 4) (1 2 4) (1 3 4))
([a c] [a d] [b d] [a b d] [a c d])
```



## Phix

Straightforward recursive implementation, the only minor trick is that a gap does not
mean non-contiguous until you actually take something later.

Counts non-contiguous subsequences of sequences 1..20 in just over 0.5 seconds

```Phix
bool countonly = false
integer count = 0

procedure ncs(sequence rest, integer ri=0, sequence taken={}, bool contig=false, bool gap=false)
    if ri>=length(rest) then
        if contig then
            if countonly then
                count += 1
            else
                ?taken
            end if
        end if
    else
        ri += 1
        ncs(rest,ri,taken&rest[ri],gap,gap)
        ncs(rest,ri,taken,contig,length(taken)!=0)
    end if
end procedure

ncs({1,2,3})
?"==="
ncs({1,2,3,4})
?"==="
countonly = true
atom t0 = time()
sequence s = {}
for i=1 to 20 do
    count = 0
    ncs(tagset(i))
    s = append(s,count)
end for
?time()-t0
?s
```

{{out}}

```txt

{1,3}
"==="
{1,2,4}
{1,3,4}
{1,3}
{1,4}
{2,4}
"==="
0.515
{0,0,1,5,16,42,99,219,466,968,1981,4017,8100,16278,32647,65399,130918,261972,524097,1048365}

```



## PicoLisp

{{trans|Scheme}}

```PicoLisp
(de ncsubseq (Lst)
   (let S 0
      (recur (S Lst)
         (ifn Lst
            (and (>= S 3) '(NIL))
            (let (X (car Lst)  XS (cdr Lst))
               (ifn (bit? 1 S)  # even
                  (conc
                     (mapcar '((YS) (cons X YS))
                        (recurse (inc S) XS) )
                     (recurse S XS) )
                  (conc
                     (mapcar '((YS) (cons X YS))
                        (recurse S XS) )
                     (recurse (inc S) XS) ) ) ) ) ) ) )
```



## Pop11


We modify classical recursive generation of subsets, using
variables to keep track if subsequence is continuous.


```pop11
define ncsubseq(l);
    lvars acc = [], gap_started = false, is_continuous = true;
    define do_it(l1, l2);
        dlocal gap_started;
        lvars el, save_is_continuous = is_continuous;
        if l2 = [] then
            if not(is_continuous) then
                cons(l1, acc) -> acc;
            endif;
        else
            front(l2) -> el;
            back(l2) -> l2;
            not(gap_started) and is_continuous -> is_continuous;
            do_it(cons(el, l1), l2);
            save_is_continuous -> is_continuous;
            not(l1 = []) or gap_started -> gap_started;
            do_it(l1, l2);
        endif;
    enddefine;
    do_it([], rev(l));
    acc;
enddefine;

ncsubseq([1 2 3 4 5]) =>
```


{{out}}

```txt
[[1 3] [1 4] [2 4] [1 2 4] [1 3 4] [1 5] [2 5] [1 2 5] [3 5] [1 3 5]
         [2 3 5] [1 2 3 5] [1 4 5] [2 4 5] [1 2 4 5] [1 3 4 5]]
```



## PowerShell


```PowerShell
Function SubSequence ( [Array] $S, [Boolean] $all=$false )
{
   $sc = $S.count
   if( $sc -gt ( 2 - [Int32] $all ) ) {
      [void] $sc--
      0..$sc | ForEach-Object {
         $gap = $_
         "$( $S[ $_ ] )"
         if( $gap -lt $sc )
         {
            SubSequence ( ( $gap + 1 )..$sc | Where-Object { $_ -ne $gap } ) ( ( $gap -ne 0 ) -or $all ) | ForEach-Object {
               [String]::Join( ',', ( ( [String]$_ ).Split(',') | ForEach-Object {
                  $lt = $true
               } {
                  if( $lt -and ( $_ -gt $gap ) )
                  {
                     $S[ $gap ]
                     $lt = $false
                  }
                  $S[ $_ ]
               } {
                  if( $lt )
                  {
                     $S[ $gap ]
                  }
               }
               ) )
            }
         }
      }
      #[String]::Join( ',', $S)
   } else { 
      $S | ForEach-Object { [String] $_ } 
   }
}

Function NonContinuous-SubSequence ( [Array] $S )
{
   $sc = $S.count
   if( $sc -eq 3 )
   {
      [String]::Join( ',', $S[ ( 0,2 ) ] )
   } elseif ( $sc -gt 3 ) {
      [void] $sc--
      $gaps = @()
      $gaps += ( ( NonContinuous-SubSequence ( 1..$sc ) ) | ForEach-Object {
         $gap1 = ",$_,"
         "0,{0}" -f ( [String]::Join( ',', ( 1..$sc | Where-Object { $gap1 -notmatch "$_," } ) ) )
      } )
      $gaps += 1..( $sc - 1 )
      2..( $sc - 1 ) | ForEach-Object {
         $gap2 = $_ - 1
         $gaps += ( ( SubSequence ( $_..$sc ) ) | ForEach-Object {
            "$gap2,$_"
         } )
      }
      #Write-Host "S $S gaps $gaps"
      $gaps | ForEach-Object {
         $gap3 = ",$_,"
         "$( 0..$sc | Where-Object { $gap3 -notmatch ",$_," } | ForEach-Object {
            $S[$_]
         } )" -replace ' ', ','
      }
   } else { 
      $null
   }
}

( NonContinuous-SubSequence 'a','b','c','d','e' ) | Select-Object length, @{Name='value';Expression={ $_ } } | Sort-Object length, value | ForEach-Object { $_.value }
```



## Prolog

Works with SWI-Prolog.<BR>
We explain to Prolog how to build a non continuous subsequence of a list L, then we ask Prolog to fetch all the subsequences.


```Prolog

% fetch all the subsequences
ncsubs(L, LNCSL) :-
	setof(NCSL, one_ncsubs(L, NCSL), LNCSL).

% how to build one subsequence
one_ncsubs(L, NCSL) :-
	extract_elem(L, NCSL);
	(   sublist(L, L1),
	    one_ncsubs(L1, NCSL)).

% extract one element of the list
% this element is neither the first nor the last.
extract_elem(L, NCSL) :-
	length(L, Len),
	Len1 is Len - 2,
	between(1, Len1, I),
	nth0(I, L, Elem),
	select(Elem, L, NCS1),
	(   NCSL = NCS1; extract_elem(NCS1, NCSL)).

% extract the first or the last element of the list
sublist(L, SL) :-
	(L = [_|SL];
	reverse(L, [_|SL1]),
	reverse(SL1, SL)).

```

Example :

```Prolog
?- ncsubs([a,e,i,o,u], L).
L = [[a,e,i,u],[a,e,o],[a,e,o,u],[a,e,u],[a,i],[a,i,o],[a,i,o,u],[a,i,u],[a,o],[a,o,u],[a,u],[e,i,u],[e,o],[e,o,u],[e,u],[i,u]]
```



## Python

{{trans|Scheme}}


```python
def ncsub(seq, s=0):
    if seq:
        x = seq[:1]
        xs = seq[1:]
        p2 = s % 2
        p1 = not p2
        return [x + ys for ys in ncsub(xs, s + p1)] + ncsub(xs, s + p2)
    else:
        return [[]] if s >= 3 else []
```


{{out}}

```txt
>>> ncsub(range(1, 4))
[[1, 3]]
>>> ncsub(range(1, 5))
[[1, 2, 4], [1, 3, 4], [1, 3], [1, 4], [2, 4]]
>>> ncsub(range(1, 6))
[[1, 2, 3, 5], [1, 2, 4, 5], [1, 2, 4], [1, 2, 5], [1, 3, 4, 5], [1, 3, 4],
 [1, 3, 5], [1, 3], [1, 4, 5], [1, 4], [1, 5], [2, 3, 5], [2, 4, 5], [2, 4],
 [2, 5], [3, 5]]
```


A faster Python + Psyco JIT version:


```python
from sys import argv
import psyco

def C(n, k):
    result = 1
    for d in xrange(1, k+1):
        result *= n
        n -= 1
        result /= d
    return result

# http://oeis.org/A002662
nsubs = lambda n: sum(C(n, k) for k in xrange(3, n+1))

def ncsub(seq):
    n = len(seq)
    result = [None] * nsubs(n)
    pos = 0

    for i in xrange(1, 2 ** n):
        S  = []
        nc = False
        for j in xrange(n + 1):
            k = i >> j
            if k == 0:
                if nc:
                    result[pos] = S
                    pos += 1
                break
            elif k % 2:
                S.append(seq[j])
            elif S:
                nc = True
    return result

from sys import argv
import psyco
psyco.full()
n = 10 if len(argv) < 2 else int(argv[1])
print len( ncsub(range(1, n)) )
```



## R
           
The idea behind this is to loop over the possible lengths of subsequence, finding all subsequences then discarding those which are continuous.


```r
ncsub <- function(x)
{
   n <- length(x)
   a <- seq_len(n)
   seqlist <- list()
   for(i in 2:(n-1))
   {
      seqs <- combn(a, i)                                                          # Get all subseqs
      ok <- apply(seqs, 2, function(x) any(diff(x)!=1))                            # Find noncts ones
      newseqs <- unlist(apply(seqs[,ok], 2, function(x) list(x)), recursive=FALSE) # Convert matrix to list of its columns
      seqlist <- c(seqlist, newseqs)                                               # Append to existing list 
   }
   lapply(seqlist, function(index) x[index])
}
# Example usage
ncsub(1:4)
ncsub(letters[1:5])
```



## Racket


Take a simple <tt>subsets</tt> definition:

```racket

(define (subsets l)
  (if (null? l) '(())
      (append (for/list ([l2 (subsets (cdr l))]) (cons (car l) l2))
              (subsets (cdr l)))))

```

since the subsets are returned in their original order, it is also a sub-sequences function.

Now add to it a "state" counter which count one for each chunk of items included or excluded.  It's always even when we're in an excluded chunk (including the beginning) and odd when we're including items -- increment it whenever we switch from one kind of chunk to the other.  This means that we should only include subsequences where the state is 3 (included->excluded->included) or more.  Note that this results in code that is similar to the "Generalized monadic filter" entry, except a little simpler.


```racket

#lang racket
(define (non-continuous-subseqs l)
  (let loop ([l l] [x 0])
    (if (null? l) (if (>= x 3) '(()) '())
        (append (for/list ([l2 (loop (cdr l) (if (even? x) (add1 x) x))])
                  (cons (car l) l2))
                (loop (cdr l) (if (odd? x) (add1 x) x))))))
(non-continuous-subseqs '(1 2 3 4))
;; => '((1 2 4) (1 3 4) (1 3) (1 4) (2 4))

```



## REXX

This REXX version also works with non-numeric (alphabetic) items   (as well as numbers).

```rexx
/*REXX program lists all the  non─continuous subsequences  (NCS),  given a sequence.    */
parse arg list                                   /*obtain the arguments from the  C. L. */
if list='' | list==','  then list=1 2 3 4 5      /*Not specified?  Then use the default.*/
say 'list=' space(list);        say              /*display the list to the terminal.    */
w=words(list)                                    /*W:  is the number of items in list.  */
$=left(123456789, w)                             /*build a string of decimal digits.    */
tail=right($, max(0, w-2))                       /*construct a fast tail for comparisons*/
#=0                                              /* [↓]      L:   length of  Jth  item. */
    do j=13  to left($,1) || tail;  L=length(j)  /*step through list (using smart start)*/
    if verify(j, $)\==0  then iterate            /*Not one of the chosen  (sequences) ? */
    f=left(j,1)                                  /*use the fist decimal digit of  J.    */
    NCS=0                                        /*there isn't a non─continuous subseq. */
            do k=2  to L;      _=substr(j, k, 1) /*extract a single decimal digit of  J.*/
            if _ <=  f    then iterate j         /*if next digit ≤, then skip this digit*/
            if _ \== f+1  then NCS=1             /*it's OK as of now  (that is, so far).*/
            f=_                                  /*now have a  new  next decimal digit. */
            end   /*k*/

    if \NCS  then iterate                        /*not OK?  Then skip this number (item)*/
    #=#+1                                        /*Eureka!  We found a number (or item).*/
    @=;     do m=1  for L                        /*build a sequence string to display.  */
            @=@  word(list, substr(j, m, 1))     /*pick off a number (item) to display. */
            end   /*m*/

    say 'a non─continuous subsequence: '    @    /*show the non─continuous subsequence. */
    end         /*j*/
say
if #==0  then #='no'                             /*make it look more gooder Angleshy.   */
say  #  "non─continuous subsequence"s(#)     'were found.'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:  if arg(1)==1  then return '';     return word(arg(2) 's', 1)           /*pluralizer.*/
```

'''output'''   when using the input:   <tt> 1 2 3 4 </tt>

```txt

list= 1 2 3 4

a non-continuous subsequence:  1 3
a non-continuous subsequence:  1 4
a non-continuous subsequence:  2 4
a non-continuous subsequence:  1 2 4
a non-continuous subsequence:  1 3 4

5 non-continuous subsequences were found.

```

'''output'''   when using the following input:   <tt> a e I o u </tt>

```txt

list= a e I o u

a non-continuous subsequence:  a I
a non-continuous subsequence:  a o
a non-continuous subsequence:  a u
a non-continuous subsequence:  e o
a non-continuous subsequence:  e u
a non-continuous subsequence:  I u
a non-continuous subsequence:  a e o
a non-continuous subsequence:  a e u
a non-continuous subsequence:  a I o
a non-continuous subsequence:  a I u
a non-continuous subsequence:  a o u
a non-continuous subsequence:  e I u
a non-continuous subsequence:  e o u
a non-continuous subsequence:  a e I u
a non-continuous subsequence:  a e o u
a non-continuous subsequence:  a I o u

16 non-continuous subsequences were found.

```

'''output'''   when using the following [channel Islands (Great Britain)] as input:   <tt> Alderney Guernsey Herm Jersey Sark </tt>

```txt

list= Alderney Guernsey Herm Jersey Sark

a non-continuous subsequence:  Alderney Herm
a non-continuous subsequence:  Alderney Jersey
a non-continuous subsequence:  Alderney Sark
a non-continuous subsequence:  Guernsey Jersey
a non-continuous subsequence:  Guernsey Sark
a non-continuous subsequence:  Herm Sark
a non-continuous subsequence:  Alderney Guernsey Jersey
a non-continuous subsequence:  Alderney Guernsey Sark
a non-continuous subsequence:  Alderney Herm Jersey
a non-continuous subsequence:  Alderney Herm Sark
a non-continuous subsequence:  Alderney Jersey Sark
a non-continuous subsequence:  Guernsey Herm Sark
a non-continuous subsequence:  Guernsey Jersey Sark
a non-continuous subsequence:  Alderney Guernsey Herm Sark
a non-continuous subsequence:  Alderney Guernsey Jersey Sark
a non-continuous subsequence:  Alderney Herm Jersey Sark

16 non-continuous subsequences were found.

```

'''output'''   when using the following [six noble gases] as input:   <tt> helium neon argon krypton xenon radon </tt>

```txt

list= helium neon argon krypton xenon radon

a non-continuous subsequence:  helium argon
a non-continuous subsequence:  helium krypton
a non-continuous subsequence:  helium xenon
a non-continuous subsequence:  helium radon
a non-continuous subsequence:  neon krypton
a non-continuous subsequence:  neon xenon
a non-continuous subsequence:  neon radon
a non-continuous subsequence:  argon xenon
a non-continuous subsequence:  argon radon
a non-continuous subsequence:  krypton radon
a non-continuous subsequence:  helium neon krypton
a non-continuous subsequence:  helium neon xenon
a non-continuous subsequence:  helium neon radon
a non-continuous subsequence:  helium argon krypton
a non-continuous subsequence:  helium argon xenon
a non-continuous subsequence:  helium argon radon
a non-continuous subsequence:  helium krypton xenon
a non-continuous subsequence:  helium krypton radon
a non-continuous subsequence:  helium xenon radon
a non-continuous subsequence:  neon argon xenon
a non-continuous subsequence:  neon argon radon
a non-continuous subsequence:  neon krypton xenon
a non-continuous subsequence:  neon krypton radon
a non-continuous subsequence:  neon xenon radon
a non-continuous subsequence:  argon krypton radon
a non-continuous subsequence:  argon xenon radon
a non-continuous subsequence:  helium neon argon xenon
a non-continuous subsequence:  helium neon argon radon
a non-continuous subsequence:  helium neon krypton xenon
a non-continuous subsequence:  helium neon krypton radon
a non-continuous subsequence:  helium neon xenon radon
a non-continuous subsequence:  helium argon krypton xenon
a non-continuous subsequence:  helium argon krypton radon
a non-continuous subsequence:  helium argon xenon radon
a non-continuous subsequence:  helium krypton xenon radon
a non-continuous subsequence:  neon argon krypton radon
a non-continuous subsequence:  neon argon xenon radon
a non-continuous subsequence:  neon krypton xenon radon
a non-continuous subsequence:  helium neon argon krypton radon
a non-continuous subsequence:  helium neon argon xenon radon
a non-continuous subsequence:  helium neon krypton xenon radon
a non-continuous subsequence:  helium argon krypton xenon radon

42 non-continuous subsequences were found.

```



## Ring


```ring

# Project : Non-continuous subsequences

load "stdlib.ring"
list = [1,2,3,4]
items = newlist(pow(2,len(list))-1,len(list))
see "For [1, 2, 3, 4] non-continuous subsequences are:" + nl
powerset(list,4)
showarray(items,4)
see nl

list = [1,2,3,4,5]
items = newlist(pow(2,len(list))-1,len(list))
see "For [1, 2, 3, 4, 5] non-continuous subsequences are:" + nl
powerset(list,5)
showarray(items,5)

func showarray(items,ind)
        for n = 1 to len(items)
             flag = 0
             for m = 1 to ind - 1
                  if items[n][m] = 0 or items[n][m+1] = 0
                     exit
                 ok   
                 if (items[n][m] + 1) != items[n][m+1]
                     flag = 1
                     exit
                 ok
            next
            if flag = 1
               see "[" 
               str = ""
               for x = 1 to len(items[n])
                    if items[n][x] != 0  
                       str = str + items[n][x] + " "
                    ok
               next  
               str = left(str, len(str) - 1)  
               see str + "]" + nl
            ok
        next

func powerset(list,ind)
        num = 0
        num2 = 0
        items = newlist(pow(2,len(list))-1,ind)
        for i = 2 to (2 << len(list)) - 1 step 2
             num2 = 0
             num = num + 1
             for j = 1 to len(list) 
                  if i & (1 << j)
                      num2 = num2 + 1
                      if list[j] != 0
                        items[num][num2] = list[j]
                     ok
                  ok
             next
        next
        return items

```

Output:

```txt

For [1, 2, 3, 4] non-continuous subsequences are:
[1 3]
[1 4]
[2 4]
[1 2 4]
[1 3 4]

For [1, 2, 3, 4, 5] non-continuous subsequences are:
[1 3]
[1 4]
[2 4]
[1 2 4]
[1 3 4]
[1 5]
[2 5]
[1 2 5]
[3 5]
[1 3 5]
[2 3 5]
[1 2 3 5]
[1 4 5]
[2 4 5]
[1 2 4 5]
[1 3 4 5]

```



## Ruby

{{trans|Tcl}}  

Uses code from [[Power Set]].


```ruby
class Array
  def func_power_set
    inject([[]]) { |ps,item|    # for each item in the Array
      ps +                      # take the powerset up to now and add
      ps.map { |e| e + [item] } # it again, with the item appended to each element
    }
  end
  
  def non_continuous_subsequences
    func_power_set.reject {|seq| continuous?(seq)}
  end
  
  def continuous?(seq)
    seq.each_cons(2) {|a, b| return false if a.succ != b}
    true
  end
end

p (1..3).to_a.non_continuous_subsequences
p (1..4).to_a.non_continuous_subsequences
p (1..5).to_a.non_continuous_subsequences
p ("a".."d").to_a.non_continuous_subsequences
```


{{out}}

```txt
[[1, 3]]
[[1, 3], [1, 4], [2, 4], [1, 2, 4], [1, 3, 4]]
[[1, 3], [1, 4], [2, 4], [1, 2, 4], [1, 3, 4], [1, 5], [2, 5], [1, 2, 5], [3, 5], [1, 3, 5], 
 [2, 3, 5], [1, 2, 3, 5], [1, 4, 5], [2, 4, 5], [1, 2, 4, 5], [1, 3, 4, 5]]
[["a", "c"], ["a", "d"], ["b", "d"], ["a", "b", "d"], ["a", "c", "d"]]

```

It is not the value of the array element and when judging continuation in the position, it changes as follows.

```ruby
class Array
  def continuous?(seq)
    seq.each_cons(2) {|a, b| return false if index(a)+1 != index(b)}
    true
  end
end

p %w(a e i o u).non_continuous_subsequences
```


{{out}}

```txt
[["a", "i"], ["a", "o"], ["e", "o"], ["a", "e", "o"], ["a", "i", "o"], ["a", "u"], ["e", "u"], ["a", "e", "u"], ["i", "u"], ["a", "i", "u"], ["e", "i", "u"], ["a", "e", "i", "u"], ["a", "o", "u"], ["e", "o", "u"], ["a", "e", "o", "u"], ["a", "i", "o", "u"]]
```



## Scala


```Scala
object NonContinuousSubSequences extends App {

  private def seqR(s: String, c: String, i: Int, added: Int): Unit = {
    if (i == s.length) {
      if (c.trim.length > added) println(c)
    } else {
      seqR(s, c + s(i), i + 1, added + 1)
      seqR(s, c + " ", i + 1, added)
    }
  }

  seqR("1234", "", 0, 0)
}
```


## Scheme

{{trans|Generalized monadic filter}}

```scheme
(define (ncsubseq lst)
  (let recurse ((s 0)
                (lst lst))
    (if (null? lst)
        (if (>= s 3)
            '(())
            '())
        (let ((x (car lst))
              (xs (cdr lst)))
          (if (even? s)
              (append
               (map (lambda (ys) (cons x ys))
                    (recurse (+ s 1) xs))
               (recurse s xs))
              (append
               (map (lambda (ys) (cons x ys))
                    (recurse s xs))
               (recurse (+ s 1) xs)))))))
```


{{out}}

```txt
> (ncsubseq '(1 2 3))
((1 3))
> (ncsubseq '(1 2 3 4))
((1 2 4) (1 3 4) (1 3) (1 4) (2 4))
> (ncsubseq '(1 2 3 4 5))
((1 2 3 5) (1 2 4 5) (1 2 4) (1 2 5) (1 3 4 5) (1 3 4) (1 3 5) (1 3) (1 4 5) (1 4) (1 5) (2 3 5) (2 4 5) (2 4) (2 5) (3 5))
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func array bitset: ncsub (in bitset: seq, in integer: s) is func
  result
    var array bitset: subseq is 0 times {};
  local
    var bitset: x is {};
    var bitset: xs is {};
    var bitset: ys is {};
  begin
    if seq <> {} then
      x := {min(seq)};
      xs := seq - x;
      for ys range ncsub(xs, s + 1 - s rem 2) do
        subseq &:= x | ys;
      end for;
      subseq &:= ncsub(xs, s + s rem 2);
    elsif s >= 3 then
      subseq &:= {};
    end if;
  end func;

const proc: main is func
  local
    var bitset: seq is {};
  begin
    for seq range ncsub({1, 2, 3, 4}, 0) do
      writeln(seq);
    end for;
  end func;
```


{{out}}

```txt

{1, 2, 4}
{1, 3, 4}
{1, 3}
{1, 4}
{2, 4}

```



## Sidef

{{trans|Perl}}

```ruby
func non_continuous(min, max, subseq=[], has_gap=false) {

    static current = [];

    range(min, max).each { |i|
        current.push(i);
        has_gap && subseq.append([current...]);
        i < max && non_continuous(i.inc, max, subseq, has_gap);
        current.pop;
        has_gap = current.len;
    }

    subseq;
}

say non_continuous(1, 3);
say non_continuous(1, 4);
say non_continuous("a", "d");
```

{{out}}

```txt

[[1, 3]]
[[1, 2, 4], [1, 3], [1, 3, 4], [1, 4], [2, 4]]
[["a", "b", "d"], ["a", "c"], ["a", "c", "d"], ["a", "d"], ["b", "d"]]

```



## Standard ML


{{trans|Generalized monadic filter}}


```sml
fun fence s [] =
      if s >= 3 then
        [[]]
      else
        []

  | fence s (x :: xs) =
      if s mod 2 = 0 then
        map
          (fn ys => x :: ys)
          (fence (s + 1) xs)
        @
          fence s xs
      else
        map
          (fn ys => x :: ys)
          (fence s xs)
        @
          fence (s + 1) xs

fun ncsubseq xs = fence 0 xs
```


{{out}}

```txt
- ncsubseq [1,2,3];
val it = [[1,3]] : int list list
- ncsubseq [1,2,3,4];
val it = [[1,2,4],[1,3,4],[1,3],[1,4],[2,4]] : int list list
- ncsubseq [1,2,3,4,5];
val it =
  [[1,2,3,5],[1,2,4,5],[1,2,4],[1,2,5],[1,3,4,5],[1,3,4],[1,3,5],[1,3],
   [1,4,5],[1,4],[1,5],[2,3,5],...] : int list list
```



## Tcl

This Tcl implementation uses the ''subsets'' function from [[Power Set]], which is acceptable as that conserves the ordering, as well as a problem-specific test function ''is_not_continuous'' and a generic list filter ''lfilter'':


```Tcl
 proc subsets l {
     set res [list [list]]
     foreach e $l {
         foreach subset $res {lappend res [lappend subset $e]}
     }
     return $res
 }
 proc is_not_continuous seq {
     set last [lindex $seq 0]
     foreach e [lrange $seq 1 end] {
         if {$e-1 != $last} {return 1}
         set last $e
     }
     return 0
 }
 proc lfilter {f list} {
     set res {}
     foreach i $list {if [$f $i] {lappend res $i}}
     return $res
 }

% lfilter is_not_continuous [subsets {1 2 3 4}]
{1 3} {1 4} {2 4} {1 2 4} {1 3 4}
```



## Ursala


To do it the lazy programmer way, apply the powerset library function to the list, which will generate all continuous and non-continuous subsequences of it, and then delete the subsequences that are also substrings (hence continuous) using a judicious combination of the built in substring predicate (K3), negation (Z), and distributing filter (K17) operator suffixes. This function will work on lists of any type. To meet the requirement for structural equivalence, the list items are first uniquely numbered (num), and the numbers are removed afterwards (rSS).


```Ursala
#import std

noncontinuous = num; ^rlK3ZK17rSS/~& powerset

#show+

examples = noncontinuous 'abcde'
```


{{out}}

```txt
abce
abd
abde
abe
ac
acd
acde
ace
ad
ade
ae
bce
bd
bde
be
ce
```



## zkl

{{trans|JavaScript}}

```zkl
fcn non_continuous_subsequences(ary){
   pwerSet(ary).filter(fcn(list){(not isContinuous(list)) })
}
fcn isContinuous(ary){
   if(ary.len()<2) return(True);
   foreach n in (ary.len()-1){ if(1+ary[n]!=ary[n+1]) return(False); }
   return(True);
}
non_continuous_subsequences(T(1,2,3,4)).println();
```

<lang>fcn pwerSet(list){
  (0).pump(list.len(),List,List,Utils.Helpers.pickNFrom.fp1(list),
     T(T,Void.Write,Void.Write) ) .append(list)
}
```


```zkl
fcn brokenSubsequences(str){
   pwerSet(str.split("")).apply("concat")
   .filter('wrap(substr){ (not str.holds(substr)) })
}
brokenSubsequences("1234").println();
```

{{out}}

```txt

L(L(1,3),L(1,4),L(2,4),L(1,2,4),L(1,3,4))
L("13","14","24","124","134")

```

