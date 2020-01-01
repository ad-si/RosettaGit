+++
title = "Hofstadter Q sequence"
description = ""
date = 2019-10-22T04:13:36Z
aliases = []
[extra]
id = 10709
[taxonomies]
categories = []
tags = []
+++

{{task}}The [[wp:Hofstadter_sequence#Hofstadter_Q_sequence|Hofstadter Q sequence]] is defined as:
<big>
:: <math>\begin{align}
Q(1)&=Q(2)=1, \\
Q(n)&=Q\big(n-Q(n-1)\big)+Q\big(n-Q(n-2)\big), \quad n>2.
\end{align}</math>
</big>

It is defined like the [[Fibonacci sequence]], but whereas the next term in the Fibonacci sequence is the sum of the previous two terms, in the Q sequence the previous two terms tell you how far to go back in the Q sequence to find the two numbers to sum to make the next term of the sequence.


;Task:
* Confirm and display that the first ten terms of the sequence are: 1, 1, 2, 3, 3, 4, 5, 5, 6, and 6
* Confirm and display that the 1000<sup>th</sup> term is:   502


;Optional extra credit
* Count and display how many times a member of the sequence is less than its preceding term for terms up to and including the 100,000<sup>th</sup> term.
* Ensure that the extra credit solution   ''safely''   handles being initially asked for an '''n'''<sup>th</sup> term where   '''n'''   is large.

(This point is to ensure that caching and/or recursion limits, if it is a concern, is correctly handled).





## 360 Assembly

{{trans|PL/I}}

```360asm
*        Hofstrader q sequence for any n -   18/10/2015
HOFSTRAD CSECT
         USING  HOFSTRAD,R15       set base register
         MVC    Q,=F'1'            q(1)=1
         MVC    Q+4,=F'1'          q(2)=1
         LA     R4,1               i=1
LOOPI    C      R4,N               do i=1 to n
         BH     ELOOPI
         C      R4,=F'3'           if i>=3 then
         BL     NOTREC
         LR     R1,R4              i
         SLA    R1,2               i*4
         L      R2,Q-8(R1)         q(i-1)
         LR     R1,R4              i
         SR     R1,R2              i-q(i-1)
         SLA    R1,2               *4
         L      R2,Q-4(R1)         r2=q(i-q(i-1))
         LR     R1,R4              i
         SLA    R1,2               i*4
         L      R3,Q-12(R1)        q(i-2)
         LR     R1,R4              i
         SR     R1,R3              i-q(i-2)
         SLA    R1,2               *4
         L      R3,Q-4(R1)         r3=q(i-q(i-2))
         AR     R2,R3              r2=r2+r3
         LR     R1,R4              i
         SLA    R1,2               i*4
         ST     R2,Q-4(R1)         q(i)=q(i-q(i-1))+q(i-q(i-2))
NOTREC   C      R4,=F'10'          if i<=10
         BNH    PRT
         C      R4,N               or i=n then
         BNE    NOPRT
PRT      XDECO  R4,XD              edit i
         MVC    PG+2(4),XD+8       output i
         LR     R1,R4              i
         SLA    R1,2               i*4
         L      R2,Q-4(R1)         q(i)
         XDECO  R2,XD              edit q(i)
         MVC    PG+10(4),XD+8      output q(i)
         XPRNT  PG,80              print buffer
NOPRT    LA     R4,1(R4)           i=i+1
         B      LOOPI
ELOOPI   XR     R15,R15            set return code
         BR     R14                return to caller
PG       DC     CL80'n=...., q=....'  buffer
XD       DS     CL12               temporary variable
         LTORG                     insert literals for addressability
N        DC     F'1000'            n=1000
Q        DS     1000F              array q(1000)
         YREGS
         END    HOFSTRAD
```

{{out}}
<pre style="height:16ex">
n=   1, q=   1
n=   2, q=   1
n=   3, q=   2
n=   4, q=   3
n=   5, q=   3
n=   6, q=   4
n=   7, q=   5
n=   8, q=   5
n=   9, q=   6
n=  10, q=   6
n=1000, q= 502

```



## Ada


```Ada
with Ada.Text_IO;

procedure Hofstadter_Q_Sequence is

   type Callback is access procedure(N: Positive);

   procedure Q(First, Last: Positive; Q_Proc: Callback) is
   -- calls Q_Proc(Q(First)); Q_Proc(Q(First+1)); ... Q_Proc(Q(Last));
   -- precondition: Last > 2

      Q_Store: array(1 .. Last) of Natural := (1 => 1, 2 => 1, others => 0);
      -- "global" array to store the Q(I)
      -- if Q_Store(I)=0, we compute Q(I) and update Q_Store(I)
      -- else we already know Q(I) = Q_Store(I)

      function Q(N: Positive) return Positive is
      begin
         if Q_Store(N) = 0 then
            Q_Store(N) := Q(N - Q(N-1)) + Q(N-Q(N-2));
         end if;
         return Q_Store(N);
      end Q;

   begin
      for I in First .. Last loop
         Q_Proc(Q(I));
      end loop;
   end Q;

   procedure Print(P: Positive) is
   begin
      Ada.Text_IO.Put(Positive'Image(P));
   end Print;

   Decrease_Counter: Natural := 0;
   Previous_Value: Positive := 1;

   procedure Decrease_Count(P: Positive) is
   begin
      if P < Previous_Value then
         Decrease_Counter := Decrease_Counter + 1;
      end if;
      Previous_Value := P;
   end Decrease_Count;

begin
   Q(1, 10, Print'Access);
   -- the first ten terms of the sequence are: 1, 1, 2, 3, 3, 4, 5, 5, 6, and 6
   Ada.Text_IO.New_Line;

   Q(1000, 1000,  Print'Access);
   -- the 1000'th term is: 502
   Ada.Text_IO.New_Line;

   Q(2, 100_000, Decrease_Count'Access);
   Ada.Text_IO.Put_Line(Integer'Image(Decrease_Counter));
   -- how many times a member of the sequence is less than its preceding term
   -- for terms up to and including the 100,000'th term
end Hofstadter_Q_Sequence;
```


{{out}}

```txt
 1 1 2 3 3 4 5 5 6 6
 502
 49798
```



## ALGOL 68

{{trans|C}} Note: This specimen retains the original [[Hofstadter_Q_sequence#C|C]] coding style.
{{works with|ALGOL 68|Revision 1 - no extension to language used.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.3.5 algol68g-2.3.5].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}

'''File: Hofstadter_Q_sequence.a68'''
```algol68
#!/usr/local/bin/a68g --script #

INT n = 100000;
main:
(
        INT flip;
        [n]INT q;

        q[1] := q[2] := 1;

        FOR i FROM 3 TO n DO
                q[i] := q[i - q[i - 1]] + q[i - q[i - 2]] OD;

        FOR i TO 10 DO
                printf(($g(0)$, q[i], $b(l,x)$, i = 10)) OD;

        printf(($g(0)l$, q[1000]));

        flip := 0;
        FOR i TO n-1 DO
                flip +:= ABS (q[i] > q[i + 1]) OD;

        printf(($"flips: "g(0)l$, flip))
)
```

{{out}}

```txt

1 1 2 3 3 4 5 5 6 6
502
flips: 49798

```



## APL



```APL
∇ Q_sequence;seq;size
    size←100000
    seq←{⍵,+/⍵[(1+⍴⍵)-¯2↑⍵]}⍣(size-2)⊢1 1

    ⎕←'The first 10 terms are:', seq[⍳10]
    ⎕←'The 1000th term is:', seq[1000]
    ⎕←(+/ 2>/seq),'terms were preceded by a larger term.'
∇
```


{{out}}


```txt

The first 10 terms are: 1 1 2 3 3 4 5 5 6 6
The 1000th term is: 502
49798 terms were preceded by a larger term.

```



## AutoHotkey


```AutoHotkey
SetBatchLines, -1
Q := HofsQSeq(100000)

Loop, 10
	Out .= Q[A_Index] ", "

MsgBox, % "First ten:`t" Out "`n"
	. "1000th:`t`t" Q[1000] "`n"
	. "Flips:`t`t" Q.flips

HofsQSeq(n) {
	Q := {1: 1, 2: 1, "flips": 0}
	Loop, % n - 2 {
		i := A_Index + 2
		,	Q[i] := Q[i - Q[i - 1]] + Q[i - Q[A_Index]]
		if (Q[i] < Q[i - 1])
			Q.flips++
	}
	return Q
}
```

{{out}}

```txt
First ten:	1, 1, 2, 3, 3, 4, 5, 5, 6, 6,
1000th:		502
Flips:		49798
```



## AWK


```awk
#!/usr/bin/awk -f
BEGIN {
  N = 100000
  print "Q-sequence(1..10) : " Qsequence(10)
  Qsequence(N,Q)
  print "1000th number of Q sequence : " Q[1000]
  for (n=2; n<=N; n++) {
	if (Q[n]<Q[n-1]) NN++
  }
  print "number of Q(n)<Q(n+1) for n<=100000 : " NN
}

function Qsequence(N,Q) {
  Q[1] = 1
  Q[2] = 1
  seq = "1 1"
  for (n=3; n<=N; n++) {
    Q[n] = Q[n-Q[n-1]]+Q[n-Q[n-2]]
    seq = seq" "Q[n]
  }
  return seq
}
```



```txt
Q-sequence(1..10) : 1 1 2 3 3 4 5 5 6 6
1000th number of Q sequence : 502
number of Q(n)<Q(n+1) for n<=100000 : 49798
```



## BASIC256

{{trans|FreeBASIC}}

```BASIC256

limite = 100000
dim Q[limite+1]
cont = 0
Q[1] = 1
Q[2] = 1
for i = 3 to limite
	Q[i] = Q[i-Q[i-1]] + Q[i-Q[i-2]]
	if Q[i] < Q[i-1] then cont += 1
next i

print "Primeros 10 términos: ";
for i = 1 to 10
	print Q[i] + " ";
next i

print "Término número 1000:  "; Q[1000]
print "Términos menores que los anteriores: "; cont

```

{{out}}

```txt

Igual que la entrada de FreeBASIC.

```



## BBC BASIC


```bbcbasic
      PRINT "First 10 terms of Q = " ;
      FOR i% = 1 TO 10 : PRINT ;FNq(i%, c%) " "; : NEXT : PRINT
      PRINT "1000th term = " ; FNq(1000, c%)
      PRINT "100000th term = " ; FNq(100000, c%)
      PRINT "Term is less than preceding term " ; c% " times"
      END

      DEF FNq(n%, RETURN c%)
      LOCAL i%,q%()
      IF n% < 3 THEN = 1 ELSE IF n% = 3 THEN = 2
      DIM q%(n%)
      q%(1) = 1 : q%(2) = 1 : q%(3) = 2
      c% = 0
      FOR i% = 3 TO n%
        q%(i%) = q%(i% - q%(i%-1)) + q%(i% - q%(i%-2))
        IF q%(i%) < q%(i%-1) THEN c% += 1
      NEXT
      = q%(n%)
```

{{out}}

```txt

First 10 terms of Q = 1 1 2 3 3 4 5 5 6 6
1000th term = 502
100000th term = 48157
Term is less than preceding term 49798 times

```



## Bracmat


```bracmat
( 0:?memocells
& tbl$(memo,!memocells+1) { allocate array }
& ( Q
  =
    .   !arg:(1|2)&1
      |   !arg:>2
        & (   !arg:>!memocells:?memocells               { Array is too small. }
            & tbl$(memo,!memocells+1)        { Let array grow to needed size. }
          |                                         { Array is not too small. }
          )
        & ( !(!arg$memo):>0 { Set index to !arg. Return value at index if > 0 }
          |   Q$(!arg+-1*Q$(!arg+-1))+Q$(!arg+-1*Q$(!arg+-2))
            : ?(!arg$?memo)      { Set index to !arg. Store value just found. }
          )
  )
& 0:?i
&   whl
  ' (1+!i:~>10:?i&put$(str$(Q$!i " ")))
& put$\n
& whl'(1+!i:~>1000:?i&Q$!i)
& out$(Q$1000)
& 0:?previous:?lessThan:?i
&   whl
  ' ( 1+!i:~>100000:?i
    &   Q$!i
      : ( <!previous&1+!lessThan:?lessThan
        | ?
        )
      : ?previous
    )
& out$!lessThan
);
```

Output:

```txt
1 1 2 3 3 4 5 5 6 6
502
49798
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

#define N 100000
int main()
{
	int i, flip, *q = (int*)malloc(sizeof(int) * N) - 1;

	q[1] = q[2] = 1;

	for (i = 3; i <= N; i++)
		q[i] = q[i - q[i - 1]] + q[i - q[i - 2]];

	for (i = 1; i <= 10; i++)
		printf("%d%c", q[i], i == 10 ? '\n' : ' ');

	printf("%d\n", q[1000]);

	for (flip = 0, i = 1; i < N; i++)
		flip += q[i] > q[i + 1];

	printf("flips: %d\n", flip);
	return 0;
}
```

{{out}}

```txt
1 1 2 3 3 4 5 5 6 6
502
flips: 49798

```



## C++

solution modeled after Perl solution


```cpp
#include <iostream>

int main() {
   const int size = 100000;
   int hofstadters[size] = { 1, 1 };
   for (int i = 3 ; i < size; i++)
      hofstadters[ i - 1 ] = hofstadters[ i - 1 - hofstadters[ i - 1 - 1 ]] +
                             hofstadters[ i - 1 - hofstadters[ i - 2 - 1 ]];
   std::cout << "The first 10 numbers are: ";
   for (int i = 0; i < 10; i++)
      std::cout << hofstadters[ i ] << ' ';
   std::cout << std::endl << "The 1000'th term is " << hofstadters[ 999 ] << " !" << std::endl;
   int less_than_preceding = 0;
   for (int i = 0; i < size - 1; i++)
      if (hofstadters[ i + 1 ] < hofstadters[ i ])
	     less_than_preceding++;
   std::cout << "In array of size: " << size << ", ";
   std::cout << less_than_preceding << " times a number was preceded by a greater number!" << std::endl;
   return 0;
}
```

{{out}}

```txt
The first 10 numbers are: 1 1 2 3 3 4 5 5 6 6
The 1000'th term is 502 !
In array of size: 100000, 49798 times a number was preceded by a greater number!
```



## C sharp



```C sharp
using System;
using System.Collections.Generic;

namespace HofstadterQSequence
{
    class Program
    {
        // Initialize the dictionary with the first two indices filled.
        private static readonly Dictionary<int, int> QList = new Dictionary<int, int>
                                                                 {
                                                                     {1, 1},
                                                                     {2, 1}
                                                                 };

        private static void Main()
        {
            int lessThanLast = 0;
                /* Initialize our variable that holds the number of times
                                   * a member of the sequence was less than its preceding term. */

            for (int n = 1; n <= 100000; n++)
            {
                int q = Q(n); // Get Q(n).

                if (n > 1 && QList[n - 1] > q) // If Q(n) is less than Q(n - 1),
                    lessThanLast++;            // then add to the counter.

                if (n > 10 && n != 1000) continue; /* If n is greater than 10 and not 1000,
                                                    * the rest of the code in the loop does not apply,
                                                    * and it will be skipped. */

                if (!Confirm(n, q)) // Confirm Q(n) is correct.
                    throw new Exception(string.Format("Invalid result: Q({0}) != {1}", n, q));

                Console.WriteLine("Q({0}) = {1}", n, q); // Write Q(n) to the console.
            }

            Console.WriteLine("Number of times a member of the sequence was less than its preceding term: {0}.",
                              lessThanLast);
        }

        private static bool Confirm(int n, int value)
        {
            if (n <= 10)
                return new[] {1, 1, 2, 3, 3, 4, 5, 5, 6, 6}[n - 1] == value;
            if (n == 1000)
                return 502 == value;
            throw new ArgumentException("Invalid index.", "n");
        }

        private static int Q(int n)
        {
            int q;

            if (!QList.TryGetValue(n, out q)) // Try to get Q(n) from the dictionary.
            {
                q = Q(n - Q(n - 1)) + Q(n - Q(n - 2)); // If it's not available, then calculate it.
                QList.Add(n, q); // Add it to the dictionary.
            }

            return q;
        }
    }
}
```


{{out}}

```txt
Q(1) = 1
Q(2) = 1
Q(3) = 2
Q(4) = 3
Q(5) = 3
Q(6) = 4
Q(7) = 5
Q(8) = 5
Q(9) = 6
Q(10) = 6
Q(1000) = 502
Number of times a member of the sequence was less than its preceding term: 49798.
```



## Clojure

The ''qs'' function, given the initial subsequence of Q of length ''n'', produces the initial subsequence of length ''n+1''.
The subsequences are vectors for efficient indexing.
''qfirst'' iterates ''qs'' so the nth iteration is Q{1..n].

```clojure
(defn qs [q]
  (let [n (count q)]
    (condp = n
      0 [1]
      1 [1 1]
      (conj q (+ (q (- n (q (- n 1))))
                 (q (- n (q (- n 2)))))))))

(defn qfirst [n] (-> (iterate qs []) (nth n)))

(println "first 10:" (qfirst 10))
(println "1000th:" (last (qfirst 1000)))
(println "extra credit:" (->> (qfirst 100000) (partition 2 1) (filter #(apply > %)) count))
```

{{out}}
<lang>first 10: [1 1 2 3 3 4 5 5 6 6]
1000th: 502
extra credit: 49798
```



## CoffeeScript

{{trans|JavaScript}}

```coffeescript
hofstadterQ = do ->
  memo = [ 1 ,1, 1]
  Q = (n) ->
    result = memo[n]
    if typeof result != 'number'
      result = memo[n] = Q(n - Q(n - 1)) + Q(n - Q(n - 2))
    result

# some results:
console.log 'Q(' + i + ') = ' + hofstadterQ(i) for i in [1..10]
console.log 'Q(1000) = ' + hofstadterQ(1000)
```

{{out}}

```txt
Q(1) = 1
Q(2) = 1
Q(3) = 2
Q(4) = 3
Q(5) = 3
Q(6) = 4
Q(7) = 5
Q(8) = 5
Q(9) = 6
Q(10) = 6
Q(1000) = 502
```



## Common Lisp


```lisp
(defparameter *mm* (make-hash-table :test #'equal))

;;; generic memoization macro
(defmacro defun-memoize (f (&rest args) &body body)
  (defmacro hash () `(gethash (cons ',f (list ,@args)) *mm*))
  (let ((h (gensym)))
    `(defun ,f (,@args)
       (let ((,h (hash)))
	 (if ,h ,h
	   (setf (hash) (progn ,@body)))))))

;;; def q
(defun-memoize q (n)
  (if (<= n 2) 1
    (+ (q (- n (q (- n 1))))
       (q (- n (q (- n 2)))))))

;;; test
(format t "First of Q: ~a~%Q(1000): ~a~%Bumps up to 100000: ~a~%"
	(loop for i from 1 to 10 collect (q i))
	(q 1000)
	(loop with c = 0 with last-q = (q 1)
	      for i from 2 to 100000
	      do (let ((next-q (q i)))
		   (if (< next-q last-q) (incf c))
		   (setf last-q next-q))
	      finally (return c)))
```

{{out}}

```txt
First of Q: (1 1 2 3 3 4 5 5 6 6)
Q(1000): 502
Bumps up to 100000: 49798
```


Although the above definition of <code>q</code> is more general, for this specific problem the following is faster:
```lisp
(let ((cc (make-array 3 :element-type 'integer
		        :initial-element 1
			:adjustable t
			:fill-pointer 3)))
      (defun q (n)
	(when (>= n (length cc))
	  (loop for i from (length cc) below n do (q i))
	  (vector-push-extend
	    (+ (aref cc (- n (aref cc (- n 1))))
	       (aref cc (- n (aref cc (- n 2)))))
	    cc))
	(aref cc n)))
```



## D


```d
import std.stdio, std.algorithm, std.functional, std.range;

int Q(in int n) nothrow
in {
    assert(n > 0);
} body {
    alias mQ = memoize!Q;
    if (n == 1 || n == 2)
        return 1;
    else
        return mQ(n - mQ(n - 1)) + mQ(n - mQ(n - 2));
}

void main() {
    writeln("Q(n) for n = [1..10] is: ", iota(1, 11).map!Q);
    writeln("Q(1000) = ", Q(1000));
    writefln("Q(i) is less than Q(i-1) for i [2..100_000] %d times.",
             iota(2, 100_001).count!(i => Q(i) < Q(i - 1)));
}

```

{{out}}

```txt
Q(n) for n = [1..10] is: [1, 1, 2, 3, 3, 4, 5, 5, 6, 6]
Q(1000) = 502
Q(i) is less than Q(i-1) for i [2..100_000] 49798 times.

```



### Faster Version

{{trans|Python}}
Same output.

```d
import std.stdio, std.algorithm, std.range, std.array;

uint Q(in int n) nothrow
in {
    assert(n > 0);
} body {
    __gshared static Appender!(int[]) s = [0, 1, 1];

    foreach (immutable i; s.data.length .. n + 1)
        s ~= s.data[i - s.data[i - 1]] + s.data[i - s.data[i - 2]];
    return s.data[n];
}

void main() {
    writeln("Q(n) for n = [1..10] is: ", iota(1, 11).map!Q);
    writeln("Q(1000) = ", Q(1000));
    writefln("Q(i) is less than Q(i-1) for i [2..100_000] %d times.",
             iota(2, 100_001).count!(i => Q(i) < Q(i - 1)));
}

```



### Even Faster Version

This code is here to show that you don't have to use all fancy features of D. Straightforward simple code is often clearer, and faster.


```d

import std.stdio;

int[100_000] Q;

void main() {
	Q[0] = 1;
	Q[1] = 1;

	for (int i = 2; i < 100_000; i++)
	{
		Q[i] = Q[i - Q[i - 1]] + Q[i - Q[i - 2]];
	}

	write("Q(1..10) : ");
	for (int i = 0; i < 10; i++)
	{
		write(" ", Q[i]);
	}
	writeln;

	write("Q(1000) : ");
	writeln(Q[999]);

	int lt = 0;
	for (int i = 1; i < 100_000; i++)
	{
		if( Q[i-1] > Q[i] ) lt++;
	}

	writefln("Q(i) is less than Q(i-1) for i [2..100_000] %d times.", lt);
}

```



## Dart

Naive version using only recursion (Q(1000) fails due to browser script runtime restrictions)

```dart
int Q(int n) => n>2 ? Q(n-Q(n-1))+Q(n-Q(n-2)) : 1;

main() {
  for(int i=1;i<=10;i++) {
    print("Q($i)=${Q(i)}");
  }
  print("Q(1000)=${Q(1000)}");
}
```


Version featuring caching.

```dart
class Q {
  Map<int,int> _table;

  Q() {
    _table=new Map<int,int>();
    _table[1]=1;
    _table[2]=1;
  }

  int q(int n) {
    // if the cache is not filled until n-1, fill it starting with the lowest entries first
    // this avoids doing a recursion from n to 2 (e.g. if you call q(1000000) first)
    // this doesn't happen in the  tasks calls since the cache is filled ascending
    if(_table[n-1]==null) {
      for(int i=_table.length;i<n;i++) {
		q(i);
	  }
    }
    if(_table[n]==null) {
      _table[n]=q(n-q(n-1))+q(n-q(n-2));
    }

    return _table[n];
  }
}

main() {
  Q q=new Q();

  for(int i=1;i<=10;i++) {
    print("Q($i)=${q.q(i)}");
  }
  print("Q(1000)=${q.q(1000)}");

  int count=0;
  for(int i=2;i<=100000;i++) {
    if(q.q(i)<q.q(i-1)) {
      count++;
    }
  }
  print("value is smaller than previous $count times");
}
```

{{out}}

```txt
Q(1)=1
Q(2)=1
Q(3)=2
Q(4)=3
Q(5)=3
Q(6)=4
Q(7)=5
Q(8)=5
Q(9)=6
Q(10)=6
Q(1000)=502
value is smaller than previous 49798 times
```


If the maximum number is known, filling an array is probably the fastest solution.

```dart
main() {
  List<int> q=new List<int>(100001);
  q[1]=q[2]=1;

  int count=0;
  for(int i=3;i<q.length;i++) {
    q[i]=q[i-q[i-1]]+q[i-q[i-2]];
    if(q[i]<q[i-1]) {
      count++;
    }
  }
  for(int i=1;i<=10;i++) {
    print("Q($i)=${q[i]}");
  }
  print("Q(1000)=${q[1000]}");
  print("value is smaller than previous $count times");
}
```



## EchoLisp


```scheme

(define RECURSE_BUMP 500) ;; minimum of chrome:500 safari:1000 firefox:2000

;; count flips
(define (flips N)
	(for/sum ((n (in-range 2 (1+ N))))
	#:when (< (Q n) (Q (1- n)))  1))

(cache-size 120000)
(define (Q n)
	;; prevent browser stack overflow at low-cost
	(when (zero? (modulo n RECURSE_BUMP)) (for ((i (in-range 0 n RECURSE_BUMP ))) (Q i)))
	(+ (Q (- n (Q (1- n)))) (Q (- n (Q (- n 2))))))
(remember 'Q #(1 1 1)) ;; memoize and init


;; first call : check stack OK
(Q 100000) → 48157

(for ((i 11)) (write (Q i)))
1 1 1 2 3 3 4 5 5 6 6

(Q 1000)  → 502
(flips 100000) → 49798

```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
			-- Test output of the feature hofstadter_q_sequence.
		local
			count, i: INTEGER
			test: ARRAY [INTEGER]
		do
			io.put_string ("%NFirst ten numbers: %N")
			test := hofstadter_q_sequence (10)
			across
				test as ar
			loop
				io.put_string (ar.item.out + "%T")
			end
			test := hofstadter_q_sequence (100000)
			io.put_string ("1000th:%N")
			io.put_integer (test [1000])
			io.put_string ("%NNumber of Flips:%N")
			from
				i := 2
			until
				i > 100000
			loop
				if test [i] < test [i - 1] then
					count := count + 1
				end
				i := i + 1
			end
			io.put_integer (count)
		end

	hofstadter_q_sequence (lim: INTEGER): ARRAY [INTEGER]
			-- Hofstadter Q Sequence up to 'lim'.
		require
			lim_positive: lim > 0
		local
			q: ARRAY [INTEGER]
			i: INTEGER
		do
			create Result.make_filled (1, 1, lim)
			Result [1] := 1
			Result [2] := 1
			from
				i := 3
			until
				i > lim
			loop
				Result [i] := Result [i - Result [i - 1]] + Result [i - Result [i - 2]]
				i := i + 1
			end
		end

end


```

{{out}}

```txt

First ten numbers:
1 1 2 3 3 4 5 5 6 6
1000th:
502
Number of Flips:
49798

```



## Elixir

{{trans|Erlang}}
changed collection (Erlang array => Map)

```elixir
defmodule Hofstadter do
  defp flip(v2, v1) when v1 > v2, do: 1
  defp flip(_v2, _v1), do: 0

  defp list_terms(max, n, acc), do: Enum.map_join(n..max, ", ", &acc[&1])

  defp hofstadter(n, n, acc, flips) do
    IO.puts "The first ten terms are: #{list_terms(10, 1, acc)}"
    IO.puts "The 1000'th term is #{acc[1000]}"
    IO.puts "Number of flips: #{flips}"
  end
  defp hofstadter(max, n, acc, flips) do
    qn1 = acc[n-1]
    qn = acc[n - qn1] + acc[n - acc[n-2]]
    hofstadter(max, n+1, Map.put(acc, n, qn), flips + flip(qn, qn1))
  end

  def main(max \\ 100_000) do
    acc = %{1 => 1, 2 => 1}
    hofstadter(max+1, 3, acc, 0)
  end
end

Hofstadter.main
```


{{out}}

```txt

The first ten terms are: 1, 1, 2, 3, 3, 4, 5, 5, 6, 6
The 1000'th term is 502
Number of flips: 49798

```



## Erlang


```erlang
%% @author Jan Willem Luiten <jwl@secondmove.com>
%% Hofstadter Q Sequence for Rosetta Code

-module(hofstadter).
-export([main/0]).
-define(MAX, 100000).

flip(V2, V1) when V1 > V2 -> 1;
flip(_V2, _V1) -> 0.

list_terms(N, N, Acc) ->
	io:format("~w~n", [array:get(N, Acc)]);
list_terms(Max, N, Acc) ->
	io:format("~w, ", [array:get(N, Acc)]),
	list_terms(Max, N+1, Acc).

hofstadter(N, N, Acc, Flips) ->
	io:format("The first ten terms are: "),
	list_terms(9, 0, Acc),
	io:format("The 1000'th term is ~w~n", [array:get(999, Acc)]),
	io:format("Number of flips: ~w~n", [Flips]);
hofstadter(Max, N, Acc, Flips) ->
	Qn1 = array:get(N-1, Acc),
	Qn = array:get(N - Qn1, Acc) + array:get(N - array:get(N-2, Acc), Acc),
	hofstadter(Max, N+1, array:set(N, Qn, Acc), Flips + flip(Qn, Qn1)).

main() ->
	Tmp = array:set(0, 1, array:new(?MAX)),
	Acc = array:set(1, 1, Tmp),
	hofstadter(?MAX, 2, Acc, 0).

```

{{out}}

```txt

The first ten terms are: 1, 1, 2, 3, 3, 4, 5, 5, 6, 6
The 1000'th term is 502
Number of flips: 49798

```



## ERRE

{{output|ERRE}}

```ERRE

PROGRAM HOFSTADER_Q

!
! for rosettacode.org
!

DIM Q%[10000]

PROCEDURE QSEQUENCE(Q,FLAG%->SEQ$)
! if FLAG% is true accumulate sequence in SEQ$
! (attention to string var lenght=255)
! otherwise calculate values in Q%[] only

  LOCAL N
  Q%[1]=1
  Q%[2]=1
  SEQ$="1 1"
  IF NOT FLAG% THEN Q=NUM END IF
  FOR N=3 TO Q DO
    Q%[N]=Q%[N-Q%[N-1]]+Q%[N-Q%[N-2]]
    IF FLAG% THEN SEQ$=SEQ$+STR$(Q%[N]) END IF
  END FOR
END PROCEDURE

BEGIN
  NUM=10000
  QSEQUENCE(10,TRUE->SEQ$)
  PRINT("Q-sequence(1..10) : ";SEQ$)
  QSEQUENCE(1000,FALSE->SEQ$)
  PRINT("1000th number of Q sequence : ";Q%[1000])
  FOR N=2 TO NUM DO
    IF Q%[N]<Q%[N-1] THEN NN+=1 END IF
  END FOR
  PRINT("Number of Q(n)<Q(n+1) for n<=10000 : ";NN)
END PROGRAM

```

Note: The extra credit was limited to 10000 because memory addressable range is limited to 64K.
If you want to implement extra credit for 100,000 you must use external file for array Q%[].

=={{header|F_Sharp|F#}}==

```fsharp
let memoize f =
    let cache = System.Collections.Generic.Dictionary<_,_>()
    fun x ->
        match cache.TryGetValue(x) with
        | (true, v) -> v
        | (_, _) ->
            let v = f x
            cache.[x] <- v
            v

let rec q = memoize (fun i ->
    if i < 3I then 1I
    else q (i - q (i - 1I)) + q (i - q(i - 2I)))

printf "q(1 .. 10) ="; List.iter (q >> (printf " %A")) [1I .. 10I]
printfn ""
printfn "q(1000) = %A" (q 1000I)
printfn "descents(100000) = %A" (Seq.sum  (Seq.init 100000 (fun i -> if q(bigint(i)) > q(bigint(i+1)) then 1 else 0)))
```

{{out}}

```txt
q(1 .. 10) = 1 1 2 3 3 4 5 5 6 6
q(1000) = 502
descents(100000) = 49798
```



## Factor

We define a method next that takes a sequence of the first n Q values and appends the next one to it. Then we perform it 1000 times on <code>{ 1 1 }</code> and show the first 10 and 999th (because the list is zero-indexed) elements.

```factor
( scratchpad ) : next ( seq -- newseq )
dup 2 tail* over length [ swap - ] curry map
[ dupd swap nth ] map 0 [ + ] reduce suffix ;

( scratchpad ) { 1 1 } 1000 [ next ] times  dup 10 head .  999 swap nth .
{ 1 1 2 3 3 4 5 5 6 6 }
502
```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Hofstadter_Q_sequence this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Fortran

The latter-day function COUNT(''logical expression'') could easily be replaced by a simple test-and-count in the DO-loop preparing the array. One hopes that the compiler produces sensible code rather than creating an auxiliary array of boolean results then counting the ''true'' values. Rather more clunky is the need to employ odd structure for the input loop so as to handle possible bad input (text, rather than a valid number, for example) and who knows, end-of-file might happen also.


```Fortran

Calculate the Hofstadter Q-sequence, using a big array rather than recursion.
      INTEGER ENUFF
      PARAMETER (ENUFF = 100000)
      INTEGER Q(ENUFF)	!Lots of memory these days.

      Q(1) = 1		!Initial values as per the definition.
      Q(2) = 1
      Q(3:) = -123456789!This will surely cause trouble!
      DO I = 3,ENUFF	!For values beyond the second,
        Q(I) = Q(I - Q(I - 1)) + Q(I - Q(I - 2))	!Reach back according to the last two values.
      END DO
Cast forth results as per the specification.
      WRITE (6,1) Q(1:10)		!Should be 1 1 2 3 3 4 5 5 6 6...
    1 FORMAT ("First ten values:",10I2)	!Known to be one-digit numbers.
      WRITE (6,*) "Q(1000) =",Q(1000)	!Should be 502.
      WRITE (6,3) ENUFF,COUNT(Q(2:ENUFF) < Q(1:ENUFF - 1))	!Please don't create a temporary array!
    3 FORMAT ("Count of those elements 2:",I0,
     1 " which are less than their predecessor: ",I0)	!Should be 49798.
Curry favour by allowing enquiries.
   10 WRITE (6,11) ENUFF
   11 FORMAT ("Nominate an index (in 1:",I0,"): ",$)	!Obviously, the $ says don't start a new line.
      READ (5,*,END = 999, ERR = 999) I	!Ask for a number, with precautions.
      IF (I.GT.0 .AND. I.LE.ENUFF) THEN	!A good number, but, within range?
        WRITE (6,12) I,Q(I)		!Yes. Reveal the requested value.
   12   FORMAT ("Q(",I0,") = ",I0)	!This should do.
        GO TO 10			!And ask again.
      END IF		! WHILE read(5,*) i & i > 0 & i < enuff DO write(6,*) "Q(",i,")=",Q(i);
Closedown.
  999 WRITE (6,*) "Bye."
      END

```


Output:

```txt

First ten values: 1 1 2 3 3 4 5 5 6 6
 Q(1000) =         502
Count of those elements 2:100000 which are less than their predecessor: 49798
Nominate an index (in 1:100000): 100000
Q(100000) = 48157
Nominate an index (in 1:100000): 0
 Bye.

```



## FreeBASIC


```freebasic

Const limite = 100000

Dim As Long Q(limite), i, cont = 0

Q(1) = 1
Q(2) = 1
For i = 3 To limite
    Q(i) = Q(i-Q(i-1)) + Q(i-Q(i-2))
    If Q(i) < Q(i-1) Then cont += 1
Next i

Print "Primeros 10 terminos:  ";
For i = 1 To 10
    Print Q(i) &" ";
Next i
Print

Print "Termino numero 1000:  "; Q(1000)

Print "Terminos menores que los anteriores: " &cont
End

```

{{out}}

```txt

Primeros 10 terminos:  1 1 2 3 3 4 5 5 6 6
Termino numero 1000:   502
Terminos menores que los anteriores: 49798

```



## Go

Sure there are ways that run faster or handle larger numbers; for the task though, maps and recursion work just fine.

```go
package main

import "fmt"

var m map[int]int

func initMap() {
    m = make(map[int]int)
    m[1] = 1
    m[2] = 1
}

func q(n int) (r int) {
    if r = m[n]; r == 0 {
        r = q(n-q(n-1)) + q(n-q(n-2))
        m[n] = r
    }
    return
}

func main() {
    initMap()
    // task
    for n := 1; n <= 10; n++ {
        showQ(n)
    }
    // task
    showQ(1000)
    // extra credit
    count, p := 0, 1
    for n := 2; n <= 1e5; n++ {
        qn := q(n)
        if qn < p {
            count++
        }
        p = qn
    }
    fmt.Println("count:", count)
    // extra credit
    initMap()
    showQ(1e6)
}

func showQ(n int) {
    fmt.Printf("Q(%d) = %d\n", n, q(n))
}
```

{{out}}

```txt

Q(1) = 1
Q(2) = 1
Q(3) = 2
Q(4) = 3
Q(5) = 3
Q(6) = 4
Q(7) = 5
Q(8) = 5
Q(9) = 6
Q(10) = 6
Q(1000) = 502
count: 49798
Q(1000000) = 512066

```



## Haskell


The basic task:


```Haskell
qSequence = tail qq where
  qq = 0 : 1 : 1 : map g [3..]
  g n = qq !! (n - qq !! (n-1)) + qq !! (n - qq !! (n-2))

-- Output:
*Main> (take 10 qSequence, qSequence !! (1000-1))
([1,1,2,3,3,4,5,5,6,6],502)
(0.00 secs, 525044 bytes)
```


Extra credit task:


```Haskell
import Data.Array

qSequence n = arr
  where
     arr = listArray (1,n) $ 1:1: map g [3..n]
     g i = arr!(i - arr!(i-1)) +
           arr!(i - arr!(i-2))

gradualth m k arr                         -- gradually precalculate m-th item
        | m <= v = pre `seq` arr!m        --   in steps of k
  where                                   --     to prevent STACK OVERFLOW
    pre = foldl1 (\a b-> a `seq` arr!b) [u,u+k..m]
    (u,v) = bounds arr

qSeqTest m n = let arr = qSequence $ max m n in
  ( take 10 . elems  $ arr                       -- 10 first items
  , gradualth m 10000 $ arr                      -- m-th item
  , length . filter (> 0)                       -- reversals in n items
     . _S (zipWith (-)) tail . take n . elems $ arr )

_S f g x = f x (g x)
```


{{out}}

```Haskell>Prelude Main
 qSeqTest 1000 100000    -- reversals in 100,000
([1,1,2,3,3,4,5,5,6,6],502,49798)
(0.09 secs, 18879708 bytes)

Prelude Main> qSeqTest 1000000 100000   -- 1,000,000-th item
([1,1,2,3,3,4,5,5,6,6],512066,49798)
(2.80 secs, 87559640 bytes)
```


Using a list (more or less) seemlessly backed up by a double resizing array:

```haskell
q = qq (listArray (1,2) [1,1]) 1 where
    qq ar n    = (arr!n) : qq arr (n+1) where
        l = snd (bounds ar)
        step n =arr!(n - (fromIntegral (arr!(n - 1)))) +
            arr!(n - (fromIntegral (arr!(n - 2))))
        arr :: Array Int Integer
        arr | n <= l = ar
            | otherwise = listArray (1, l*2)$
                ([ar!i | i <- [1..l]] ++
                 [step i | i <- [l+1..l*2]])

main = do
    putStr("first 10: "); print (take 10 q)
    putStr("1000-th:  "); print (q !! 999)
    putStr("flips: ")
    print $ length $ filter id $ take 100000 (zipWith (>) q (tail q))
```

{{out}}

```txt

first 10: [1,1,2,3,3,4,5,5,6,6]
1000-th:  502
flips: 49798

```


List backed up by a list of arrays, with nominal constant lookup time.  ''Somehow'' faster than the previous method.

```haskell
import Data.Array
import Data.Int (Int64)

q = qq [listArray (1,2) [1,1]] 1 where
    qq a n = seek aa n : qq aa (1 + n) where
        aa  | n <= l = a
            | otherwise = listArray (l+1,l*2) (take l $ drop 2 lst):a
            where
            l = snd (bounds $ head a)
            lst = seek a (l-1):seek a l:(ext lst (l+1))
            ext (q1:q2:qs) i = (g (i-q2) + g (i-q1)):ext (q2:qs) (1+i)
            g = seek aa
        seek (ar:ars) n
            | n >= fst (bounds ar) = ar ! n
            | otherwise = seek ars n

-- Only a perf test. Task can be done exactly the same as above
main = print $ sum qqq
    where
        qqq :: [Int64]
        qqq = map fromIntegral $ take 3000000 q
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
link printf

procedure main()

V := [1, 1, 2, 3, 3, 4, 5, 5, 6, 6]
every i := 1 to *V do
   if Q(i) ~= V[i] then stop("Assertion failure for position ",i)
printf("Q(1 to %d) - verified.\n",*V)

q := Q(n := 1000)
v := 502
printf("Q[%d]=%d - %s.\n",n,v,if q = v then "verified" else "failed")

invcount := 0
every i := 2 to (n := 100000) do
   if Q(i) < Q(i-1) then {
      printf("Q(%d)=%d < Q(%d)=%d\n",i,Q(i),i-1,Q(i-1))
      invcount +:= 1
      }
printf("There were %d inversions in Q up to %d\n",invcount,n)
end



procedure Q(n) #: Hofstader Q sequence
static S
initial S := [1,1]

if q := S[n] then return q
else {
   q := Q(n - Q(n - 1)) + Q(n - Q(n - 2))
   if *S = n - 1 then {
      put(S,q)
      return q
      }
   else
      runerr(500,n)
   }
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]

{{out}}

```txt
Q(1 to 10) - verified.
Q[1000]=502 - verified.
Q(16)=9 < Q(15)=10
Q(25)=14 < Q(24)=16
Q(32)=17 < Q(31)=20
Q(36)=19 < Q(35)=21
...
Q(99996)=48252 < Q(99995)=50276
Q(99999)=48456 < Q(99998)=50901
Q(100000)=48157 < Q(99999)=48456
There were 49798 inversions in Q up to 100000
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "QSequen.bas"
110 LET LIMIT=1000
120 NUMERIC Q(1 TO LIMIT)
130 LET Q(1),Q(2)=1
140 FOR I=3 TO LIMIT
150   LET Q(I)=Q(I-Q(I-1))+Q(I-Q(I-2))
160 NEXT
170 PRINT "First 10 terms:"
180 FOR I=1 TO 10
190   PRINT Q(I);
200 NEXT
210 PRINT :PRINT "Term 1000:";Q(1000)
```



## J

'''Solution''' (''bottom-up''):
```j
   Qs=:0 1 1
   Q=: verb define
     n=. >./,y
     while. n>:#Qs do.
       Qs=: Qs,+/(-_2{.Qs){Qs
     end.
     y{Qs
)
```


'''Solution''' (''top-down''):
```j
   Q=: 1:`(+&$:/@:- $:@-& 1 2)@.(>&2)"0 M.
```


'''Example''':
```j
   Q 1+i.10
1 1 2 3 3 4 5 5 6 6
   Q 1000
502
   +/2>/\ Q 1+i.100000
49798
```


'''Note''': The bottom-up solution uses iteration and doesn't risk failure due to recursion limits or cache overflows.  The top-down solution uses recursion, and likely hews closer to the spirit of the task.  While this latter uses memoization/caching, at some point it will still hit a recursion limit (depends on the environment; in mine, it barfs at N=4402).  We use the bottom up version for the extra credit part of this task (the expression which compares adjacent numbers and gave us the result 49798).

It happens to be that the bottom-up version is written in the "explicit" style of code and the top-down version is written in the "tacit" (aka "point-free") style.  This is incidental and it's possible to write bottom-up tacitly and/or top-down explicitly.

The top-down version may be interesting as an example of algebraic factorization of code: taking advantage of some unique function composition operations in J, it manages to only mention <tt>$:</tt> (aka recursion aka "Q") twice.


## Java

[[Category:Memoization]]{{works with|Java|1.5+}}
This example also counts the number of times each n is used as an argument up to 100000 and reports the one that was used the most.

```java5
import java.util.HashMap;
import java.util.Map;

public class HofQ {
	private static Map<Integer, Integer> q = new HashMap<Integer, Integer>(){{
		put(1, 1);
		put(2, 1);
	}};

	private static int[] nUses = new int[100001];//not part of the task

	public static int Q(int n){
		nUses[n]++;//not part of the task
		if(q.containsKey(n)){
			return q.get(n);
		}
		int ans = Q(n - Q(n - 1)) + Q(n - Q(n - 2));
		q.put(n, ans);
		return ans;
	}

	public static void main(String[] args){
		for(int i = 1; i <= 10; i++){
			System.out.println("Q(" + i + ") = " + Q(i));
		}
		int last = 6;//value for Q(10)
		int count = 0;
		for(int i = 11; i <= 100000; i++){
			int curr = Q(i);
			if(curr < last) count++;
			last = curr;
			if(i == 1000) System.out.println("Q(1000) = " + curr);
		}
		System.out.println("Q(i) is less than Q(i-1) for i <= 100000 " + count + " times");

		//Optional stuff below here
		int maxUses = 0, maxN = 0;
		for(int i = 1; i<nUses.length;i++){
			if(nUses[i] > maxUses){
				maxUses = nUses[i];
				maxN = i;
			}
		}
		System.out.println("Q(" + maxN + ") was called the most with " + maxUses + " calls");
	}
}
```

{{out}}

```txt
Q(1) = 1
Q(2) = 1
Q(3) = 2
Q(4) = 3
Q(5) = 3
Q(6) = 4
Q(7) = 5
Q(8) = 5
Q(9) = 6
Q(10) = 6
Q(1000) = 502
Q(i) is less than Q(i-1) for i <= 100000 49798 times
Q(44710) was called the most with 19 calls
```



## JavaScript


### ES5

Based on memoization example from 'JavaScript: The Good Parts'.

```JavaScript
var hofstadterQ = function() {
   var memo = [1,1,1];
   var Q    = function (n) {
      var result = memo[n];
      if (typeof result !== 'number') {
         result  = Q(n - Q(n-1)) + Q(n - Q(n-2));
         memo[n] = result;
      }
      return result;
   };
   return Q;
}();

for (var i = 1; i <=10; i += 1) {
   console.log('Q('+ i +') = ' + hofstadterQ(i));
}

console.log('Q(1000) = ' + hofstadterQ(1000));

```

{{out}}

```txt

Q(1) = 1
Q(2) = 1
Q(3) = 2
Q(4) = 3
Q(5) = 3
Q(6) = 4
Q(7) = 5
Q(8) = 5
Q(9) = 6
Q(10) = 6
Q(1000) = 502
```




### ES6

Memoising with the accumulator of a fold

```JavaScript
(() => {
    'use strict';

    // hofQSeq :: Int -> [Int]
    const hofQSeq = x =>
        x > 2 ? tail(foldl((Q, n) =>
            n < 3 ? Q : Q.concat(
                Q[n - Q[n - 1]] + Q[n - Q[n - 2]]
            ), [0, 1, 1],
            range(1, x))) : (x > 0 ? take(x, [1, 1]) : undefined);


    // GENERIC FUNCTIONS -------------------------------------------

    // foldl :: (b -> a -> b) -> b -> [a] -> b
    const foldl = (f, a, xs) => xs.reduce(f, a),

        // range :: Int -> Int -> [Int]
        range = (m, n) =>
            Array.from({
                length: Math.floor(n - m) + 1
            }, (_, i) => m + i),

        // tail :: [a] -> [a]
        tail = xs => xs.length ? xs.slice(1) : undefined,

        // last :: [a] -> a
        last = xs => xs.length ? xs.slice(-1)[0] : undefined,

        // Int -> [a] -> [a]
        take = (n, xs) => xs.slice(0, n);

    // TEST --------------------------------------------------------
    return {
        firstTen: hofQSeq(10),
        thousandth: last(hofQSeq(1000)),
        'Q<Q-1UpTo10E5': hofQSeq(100000)
            .reduce((a, x, i, xs) => x < xs[i - 1] ? a + 1 : a, 0)
    };
})();
```


{{Out}}

```JavaScript
{"firstTen":[1, 1, 2, 3, 3, 4, 5, 5, 6, 6],
 "thousandth":502,
 "Q<Q-1UpTo10E5":49798}
```



## jq

For the tasks related to evaluating Q(n) directy, a recursive implementation is used, firstly because the task requirements refer to "recursion limits", and secondly to demonstrate one way to handle a cache in a functional language.  To count the number of inversions, a non-recursive approach is used as it is faster and scales linearly.

For simplicity, we also define Q(0) = 1, so that the defining
formula also holds for n == 2, and so that we can cache Q(n) at the
n-th position of an array with index origin 0.

```jq

# For n>=2, Q(n) = Q(n - Q(n-1)) + Q(n - Q(n-2))
def Q:
  def Q(n):
    n as $n
    | (if . == null then [1,1,1] else . end) as $q
    | if $q[$n] != null then $q
      else
        $q | Q($n-1) as $q1
        | $q1 | Q($n-2) as $q2
        | $q2 | Q($n - $q2[$n - 1]) as $q3   # Q(n - Q(n-1))
        | $q3 | Q($n - $q3[$n - 2]) as $q4   # Q(n - Q(n-2))
        | ($q4[$n - $q4[$n-1]] + $q4[$n - $q4[$n -2]]) as $ans
        | $q4 | setpath( [$n]; $ans)
      end ;

  . as $n | null | Q($n) | .[$n];

# count the number of times Q(i) > Q(i+1) for 0 < i < n
def flips(n):
  (reduce range(3; n) as $n
    ([1,1,1]; . + [ .[$n - .[$n-1]] + .[$n - .[$n - 2 ]] ] )) as $q
  | reduce range(0; n) as $i
      (0; . + (if $q[$i] > $q[$i + 1] then 1 else 0 end)) ;

# The three tasks:
((range(0;11), 1000) | "Q(\(.)) = \( . | Q)"),

(100000 | "flips(\(.)) = \(flips(.))")
```


### Transcript


```bash

$ uname -a
Darwin Mac-mini 13.3.0 Darwin Kernel Version 13.3.0: Tue Jun  3 21:27:35 PDT 2014; root:xnu-2422.110.17~1/RELEASE_X86_64 x86_64
$ time jq -r -n -f hofstadter.jq
Q(0) = 1
Q(1) = 1
Q(2) = 1
Q(3) = 2
Q(4) = 3
Q(5) = 3
Q(6) = 4
Q(7) = 5
Q(8) = 5
Q(9) = 6
Q(10) = 6
Q(1000) = 502
flips(100000) = 49798

real	0m0.562s
user	0m0.541s
sys	0m0.011s
```



## Julia

The following implementation accepts an argument that is a single integer, an array of integers, or a range:

```julia
function hofstQseq(n, typerst::Type=Int)
    nmax = maximum(n)
    r = Vector{typerst}(nmax)
    r[1] = 1
    if nmax ≥ 2 r[2] = 1 end
    for i in 3:nmax
        r[i] = r[i - r[i - 1]] + r[i - r[i - 2]]
    end
    return r[n]
end

println("First ten elements of sequence: ", join(hofstQseq(1:10), ", "))
println("1000-th element: ", hofstQseq(1000))

```


{{out}}

```txt
First ten elements of sequence: 1, 1, 2, 3, 3, 4, 5, 5, 6, 6
1000-th element: 502
```


And we can also count the number of times a value is less than its predecessor by, for example:

```julia
seq = hofstQseq(1:100_000)
cnt = count(diff(seq) .< 0)
println("$cnt elements are less than the preceding one.")
```


{{out}}

```txt
49798 elements are less than the preceding one.
```


Since the implementation is non-recursive, there is no issue with recursion limits.


## Kotlin


```scala
// version 1.1.4

fun main(args: Array<String>) {
    val q = IntArray(100_001)
    q[1] = 1
    q[2] = 1
    for (n in 3..100_000) q[n] = q[n - q[n - 1]] + q[n - q[n - 2]]
    print("The first 10 terms are : ")
    for (i in 1..10) print("${q[i]}  ")
    println("\n\nThe 1000th term is : ${q[1000]}")
    val flips = (2..100_000).count { q[it] < q[it - 1] }
    println("\nThe number of flips for the first 100,000 terms is : $flips")
}
```


{{out}}

```txt

The first 10 terms are : 1  1  2  3  3  4  5  5  6  6

The 1000th term is : 502

The number of flips for the first 100,000 terms is : 49798

```



## Maple

We use automatic memoisation ("option remember") in the following.  The use of "option system" assures that memoised values can be garbage collected.

```Maple
Q := proc( n )
        option remember, system;
        if n = 1 or n = 2 then
                1
        else
                thisproc( n - thisproc( n - 1 ) ) + thisproc( n - thisproc( n - 2 ) )
        end if
end proc:
```

From this we get:

```Maple>
 seq( Q( i ), i = 1 .. 10 );
                      1, 1, 2, 3, 3, 4, 5, 5, 6, 6

> Q( 1000 );
                                  502
```

To determine the number of "flips", we proceed as follows.

```Maple>
 flips := 0:
> for i from 2 to 100000 do
>       if L[ i ] < L[ i - 1 ] then
>               flips := 1 + flips
>       end if
> end do:
> flips;
                                 49798
```

Alternatively, we can build the sequence in an array.

```Maple
Qflips := proc( n )
        local a := Array( 1 .. n );
        a[ 1 ] := 1;
        a[ 2 ] := 1;
        for local i from 3 to n do
                a[ i ] := a[ i - a[ i - 1 ] ] + a[ i - a[ i - 2 ] ]
        end do;
        local flips := 0;
        for i from 2 to n do
                if a[ i ] < a[ i - 1 ] then
                        flips := 1 + flips
                end if
        end do;
        flips
end proc:
```

This gives the same result.

```Maple>
 Qflips( 10^5 );
                                 49798
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Hofstadter[1] = Hofstadter[2] = 1;
Hofstadter[n_Integer?Positive] := Hofstadter[n] = Block[{$RecursionLimit = Infinity},
   Hofstadter[n - Hofstadter[n - 1]] + Hofstadter[n - Hofstadter[n - 2]]
]
```

{{out}}

```Mathematica
Hofstadter /@ Range[10]
{1,1,2,3,3,4,5,5,6,6}
Hofstadter[1000]
502
Count[Differences[Hofstadter /@ Range[100000]], _?Negative]
49798
```


=={{header|MATLAB}} / {{header|Octave}}==
This solution pre-allocates memory and is an iterative solution, so caching or recursion limits do not apply.

```MATLAB
function Q = Qsequence(N)
  %% zeros are used to pre-allocate memory, this is not strictly necessary but can significantly improve performance for large N
  Q = [1,1,zeros(1,N-2)];
  for n=3:N
    Q(n) = Q(n-Q(n-1))+Q(n-Q(n-2));
  end;
end;
```

Confirm and display that the first ten terms of the sequence are: 1, 1, 2, 3, 3, 4, 5, 5, 6, and 6

```txt
>> Qsequence(10)
ans =
   1   1   2   3   3   4   5   5   6   6

```

Confirm and display that the 1000'th term is: 502

```txt
>> Q=Qsequence(1000); Q(end)
ans =  502

```

Count and display how many times a member of the sequence is less than its preceding term for terms up to and including the 100,000'th term.

```txt
>> sum(diff(Qsequence(100000))<0)
ans =  49798

```



## MiniScript


```MiniScript
cache = {1:1, 2:1}

Q = function(n)
    if not cache.hasIndex(n) then
        q = Q(n - Q(n-1)) + Q(n - Q(n-2))
        cache[n] = q
    end if
    return cache[n]
end function

for i in range(1,10)
    print "Q(" + i + ") = " + Q(i)
end for
print "Q(1000) = " + Q(1000)
```

{{out}}

```txt
Q(1) = 1
Q(2) = 1
Q(3) = 2
Q(4) = 3
Q(5) = 3
Q(6) = 4
Q(7) = 5
Q(8) = 5
Q(9) = 6
Q(10) = 6
Q(1000) = 502
```



## Nim


```nim
var q = @[1, 1]
for n in 2 .. <100_000: q.add q[n-q[n-1]] + q[n-q[n-2]]

echo q[0..9]
assert q[0..9] == @[1, 1, 2, 3, 3, 4, 5, 5, 6, 6]

echo q[999]
assert q[999] == 502

var lessCount = 0
for n in 1 .. <100_000:
  if q[n] < q[n-1]:
    inc lessCount
echo lessCount
```

{{out}}

```txt
@[1, 1, 2, 3, 3, 4, 5, 5, 6, 6]
502
49798
```


=={{header|Oberon-2}}==
Works with oo2c version 2

```oberon2

MODULE Hofstadter;
IMPORT
  Out;

VAR
  i,count,q,prev: LONGINT;
  founds: ARRAY 100001 OF LONGINT;

  PROCEDURE Q(n: LONGINT): LONGINT;
  BEGIN
    IF founds[n] = 0 THEN
      CASE n OF
        1 .. 2:
            founds[n] := 1
        ELSE  founds[n] := Q(n - Q(n - 1)) + Q(n - Q(n - 2))
      END
    END;
    RETURN founds[n]
  END Q;

BEGIN
  (* first ten numbers in the sequence *)
  FOR i := 1 TO 10 DO
    Out.String("At ");Out.LongInt(i,0);Out.String(":> ");Out.LongInt(Q(i),4);Out.Ln
  END;

  Out.String("1000th value: ");Out.LongInt(Q(1000),4);Out.Ln;

  prev := 1;
  FOR i := 2 TO 100000 DO
    q := Q(i);
    IF q < prev THEN INC(count) END;
    prev := q
  END;
  Out.String("terms less than the previous: ");Out.LongInt(count,4);Out.Ln
END Hofstadter.

```

Output:

```txt

At 1:>    1
At 2:>    1
At 3:>    2
At 4:>    3
At 5:>    3
At 6:>    4
At 7:>    5
At 8:>    5
At 9:>    6
At 10:>    6
1000th value:  502
terms less than the previous: 49798

```



## Oforth



```Oforth
: QSeqTask
| q i |
   ListBuffer newSize(100000) dup add(1) dup add(1) ->q
   0 3 100000 for: i [
      q add(q at(i q at(i 1-) -) q at(i q at(i 2 -) -) +)
      q at(i) q at(i 1-) < ifTrue: [ 1+ ]
      ]
   q left(10) println q at(1000) println println ;
```


{{out}}

```txt

[1, 1, 2, 3, 3, 4, 5, 5, 6, 6]
502
49798

```



## PARI/GP

Straightforward, unoptimized version; about 1 ms.

```parigp
Q=vector(1000);Q[1]=Q[2]=1;for(n=3,#Q,Q[n]=Q[n-Q[n-1]]+Q[n-Q[n-2]]);
Q1=vecextract(Q,"1..10");
print("First 10 terms: "Q1,if(Q1==[1, 1, 2, 3, 3, 4, 5, 5, 6, 6]," (as expected)"," (in error)"));
print("1000-th term: "Q[1000],if(Q[1000]==502," (as expected)"," (in error)"));
```


{{out}}

```txt
First 10 terms: [1, 1, 2, 3, 3, 4, 5, 5, 6, 6] (as expected)
1000-th term: 502 (as expected)
```



## Pascal


```pascal
Program HofstadterQSequence (output);

const
  limit = 100000;

var
  q: array [1..limit] of longint;
  i, flips: longint;

begin
  q[1] := 1;
  q[2] := 1;
  for i := 3 to limit do
    q[i] := q[i - q[i - 1]] + q[i - q[i - 2]];
  for i := 1 to 10 do
    write(q[i], ' ');
  writeln;
  writeln(q[1000]);
  flips := 0;
  for i := 1 to limit - 1 do
    if q[i] > q[i+1] then
      inc(flips);
  writeln('Flips: ', flips);
end.
```

{{out}}

```txt
:> ./HofstadterQSequence
1 1 2 3 3 4 5 5 6 6
502
Flips: 49798

```



## Perl



```Perl
my @Q = (0,1,1);
push @Q, $Q[-$Q[-1]] + $Q[-$Q[-2]] for 1..100_000;
say "First 10 terms: [@Q[1..10]]";
say "Term 1000: $Q[1000]";
say "Terms less than preceding in first 100k: ",scalar(grep { $Q[$_] < $Q[$_-1] } 2..100000);
```

{{out}}

```txt
First 10 terms: [1 1 2 3 3 4 5 5 6 6]
Term 1000: 502
Terms less than preceding in first 100k: 49798
```


A more verbose and less idiomatic solution:

```Perl
#!/usr/bin/perl
use warnings;
use strict;

my @hofstadters = ( 1 , 1 );
while ( @hofstadters < 100000 ) {
   my $nextn = @hofstadters + 1;
# array index counting starts at 0 , so we have to subtract 1 from the numbers!
   push @hofstadters ,  $hofstadters [ $nextn - 1 - $hofstadters[ $nextn - 1 - 1 ] ]
      + $hofstadters[ $nextn - 1 - $hofstadters[ $nextn - 2 - 1 ]];
}
for my $i ( 0..9 ) {
   print "$hofstadters[ $i ]\n";
}
print "The 1000'th term is $hofstadters[ 999 ]!\n";
my $less_than_preceding = 0;
for my $i ( 0..99998 ) {
   $less_than_preceding++ if $hofstadters[ $i + 1 ] < $hofstadters[ $i ];
}
print "Up to and including the 100000'th term, $less_than_preceding terms are less " .
   "than their preceding terms!\n";

```

{{out}}

```txt
1
1
2
3
3
4
5
5
6
6
The 1000'th term is 502!
Up to and including the 100000'th term, 49798 terms are less than their preceding terms!

```


This different solution uses tie to make the Q sequence look like a regular array, and only fills the cache on demand.
Some pre-allocation is done which provides a minor speed increase for the extra credit.
I could have chosen to do recursion instead of iteration, as perl has no limit on how deeply one may recurse, but did not see the benefit of doing so.


```Perl
#!perl
use strict;
use warnings;
package Hofstadter;
sub TIEARRAY {
   bless [undef, 1, 1], shift;
}
sub FETCH {
   my ($self, $n) = @_;
   die if $n < 1;
   if( $n > $#$self ) {
      my $start = $#$self + 1;
      $#$self = $n; # pre-allocate for efficiency
      for my $nn ( $start .. $n ) {
         my ($a, $b) = (1, 2);
         $_ = $self->[ $nn - $_ ] for $a, $b;
         $_ = $self->[ $nn - $_ ] for $a, $b;
         $self->[$nn] = $a + $b;
      }
   }
   $self->[$n];
}

package main;

tie my (@q), "Hofstadter";

print "@q[1..10]\n";
print $q[1000], "\n";

my $count = 0;
for my $n ( 2 .. 100_000 ) {
   $count++ if $q[$n] < $q[$n - 1];
}
print "Extra credit: $count\n";

```

{{out}}

```txt

1 1 2 3 3 4 5 5 6 6
502
Extra credit: 49798

```



## Perl 6


### OO solution

{{Works with|rakudo|2016.03}}

Similar concept as the perl5 solution, except that the cache is only filled on demand.


```perl6
class Hofstadter {
  has @!c = 1,1;
  method AT-POS ($me: Int $i) {
    @!c.push($me[@!c.elems-$me[@!c.elems-1]] +
	     $me[@!c.elems-$me[@!c.elems-2]]) until @!c[$i]:exists;
    return @!c[$i];
  }
}

# Testing:

my Hofstadter $Q .= new();

say "first ten: $Q[^10]";
say "1000th: $Q[999]";

my $count = 0;
$count++ if $Q[$_ +1 ] < $Q[$_] for  ^99_999;
say "In the first 100_000 terms, $count terms are less than their preceding terms";
```

{{out}}

```txt
first ten: 1 1 2 3 3 4 5 5 6 6
1000th: 502
In the first 100_000 terms, 49798 terms are less than their preceding terms
```



### Idiomatic solution

{{Works with|rakudo|2015-11-22}}
With a lazily generated array, we automatically get caching.

```perl6
my @Q = 1, 1, -> $a, $b {
    (state $n = 1)++;
    @Q[$n - $a] + @Q[$n - $b]
} ... *;

# Testing:

say "first ten: ", @Q[^10];
say "1000th: ", @Q[999];
say "In the first 100_000 terms, ",
   [+](@Q[1..100000] Z< @Q[0..99999]),
   " terms are less than their preceding terms";
```

(Same output.)


## Phix

Just to be flash, I also calculated the 100 millionth term - the only limiting factor here would be the length of Q
(on 32 bit, theoretical max sequence length is 402,653,177).

```Phix
sequence Q = {1,1}

function q(integer n)
    integer l = length(Q)
    while n>l do
        l += 1
        Q &= Q[l-Q[l-1]]+Q[l-Q[l-2]]
    end while
    return Q[n]
end function

{} = q(10)  -- (or collect one by one)
printf(1,"First ten terms: %s\n",{sprint(Q[1..10])})
printf(1,"1000th: %d\n",q(1000))
printf(1,"100,000th: %d\n",q(100_000))
integer n = 0
for i=2 to 100_000 do
    n += Q[i]<Q[i-1]
end for
printf(1,"Flips up to 100,000: %d\n",{n})
atom t0 = time()
printf(1,"100,000,000th: %d (%3.2fs)\n",{q(100_000_000),time()-t0})
```

{{out}}

```txt

First ten terms: {1,1,2,3,3,4,5,5,6,6}
1000th: 502
100,000th: 48157
Flips up to 100,000: 49798
100,000,000th: 50166508 (7.53s)

```



## PicoLisp


```PicoLisp
(de q (N)
   (cache '(NIL) N
      (if (>= 2 N)
         1
         (+
            (q (- N (q (dec N))))
            (q (- N (q (- N 2)))) ) ) ) )
```

Test:

```PicoLisp
: (mapcar q (range 1 10))
-> (1 1 2 3 3 4 5 5 6 6)

: (q 1000)
-> 502

: (let L (mapcar q (range 1 100000))
   (cnt < (cdr L) L) )
-> 49798
```



## PL/I


```PL/I

/* Hofstrader Q sequence for any "n". */

H: procedure options (main);  /* 28 January 2012 */
   declare n fixed binary(31);

   put ('How many values do you want? :');
   get (n);

begin;
   declare Q(n) fixed binary (31);
   declare i fixed binary (31);

   Q(1), Q(2) = 1;
   do i = 1 upthru n;
      if i >= 3 then Q(i) = ( Q(i - Q(i-1)) + Q(i - Q(i-2)) );
      if i <= 20 then put skip list ('n=' || trim(i), Q(i));
   end;
   put skip list ('n=' || trim(i), Q(i));
end;
end H;

```

{{out}}

```txt

How many values do you want? :

n=1                                  1
n=2                                  1
n=3                                  2
n=4                                  3
n=5                                  3
n=6                                  4
n=7                                  5
n=8                                  5
n=9                                  6
n=10                                 6
n=11                                 6
n=12                                 8
n=13                                 8
n=14                                 8
n=15                                10
n=16                                 9
n=17                                10
n=18                                11
n=19                                11
n=20                                12
n=1000                             502

```

{{out}} for n=100,000

```txt

n=100000                         48157

```

Bonus to produce the count of unordered values:
<lang>
   declare tally fixed binary (31) initial (0);

   do i = 1 to n-1;
      if Q(i) > Q(i+1) then tally = tally + 1;
   end;
   put skip data (tally);

```

{{out}}

```txt

n=100000                         48157
TALLY=         49798;

```



## PureBasic


```PureBasic
If Not OpenConsole("Hofstadter Q sequence")
  End 1
EndIf

#N = 100000
Define i.i, flip.i = 0
Dim q.i(#N)
q(1) = 1
q(2) = 1
For i = 3 To #N
  q(i) = q(i - q(i - 1)) + q(i - q(i - 2))
Next
For i = 1 To #N - 1
  flip + Bool(q(i) > q(i + 1))
Next

Print(~"First ten:\t")
For i = 1 To 10 : Print(LSet(Str(q(i)), 3)) : Next
PrintN(~"\n1000th:\t\t" + Str(q(1000)))
PrintN(~"Flips:\t\t" + Str(flip))
Input()
End
```

{{out}}

```txt
First ten:      1  1  2  3  3  4  5  5  6  6
1000th:         502
Flips:          49798
```



## Python


```python
def q(n):
    if n < 1 or type(n) != int: raise ValueError("n must be an int >= 1")
    try:
        return q.seq[n]
    except IndexError:
        ans = q(n - q(n - 1)) + q(n - q(n - 2))
        q.seq.append(ans)
        return ans
q.seq = [None, 1, 1]

if __name__ == '__main__':
    first10 = [q(i) for i in range(1,11)]
    assert first10 == [1, 1, 2, 3, 3, 4, 5, 5, 6, 6], "Q() value error(s)"
    print("Q(n) for n = [1..10] is:", ', '.join(str(i) for i in first10))
    assert q(1000) == 502, "Q(1000) value error"
    print("Q(1000) =", q(1000))
```


;Extra credit:
If you try and initially compute larger values of n then you tend to hit the Python recursion limit.

The function q1 gets around this by calling function q to extend the Q series in increments below the recursion limit.

The following code is to be concatenated to the code above:

```python
from sys import getrecursionlimit

def q1(n):
    if n < 1 or type(n) != int: raise ValueError("n must be an int >= 1")
    try:
        return q.seq[n]
    except IndexError:
        len_q, rlimit = len(q.seq), getrecursionlimit()
        if (n - len_q) > (rlimit // 5):
            for i in range(len_q, n, rlimit // 5):
                q(i)
        ans = q(n - q(n - 1)) + q(n - q(n - 2))
        q.seq.append(ans)
        return ans

if __name__ == '__main__':
    tmp = q1(100000)
    print("Q(i+1) < Q(i) for i [1..100000] is true %i times." %
          sum(k1 < k0 for k0, k1 in zip(q.seq[1:], q.seq[2:])))
```


{{out|Combined output}}

```txt
Q(n) for n = [1..10] is: 1, 1, 2, 3, 3, 4, 5, 5, 6, 6
Q(1000) = 502
Q(i+1) < Q(i) for i [1..10000] is true 49798 times.
```



### Alternative


```python
def q(n):
    l = len(q.seq)
    while l <= n:
        q.seq.append(q.seq[l - q.seq[l - 1]] + q.seq[l - q.seq[l - 2]])
	l += 1
    return q.seq[n]
q.seq = [None, 1, 1]

print("Q(n) for n = [1..10] is:", [q(i) for i in range(1, 11)])
print("Q(1000) =", q(1000))
q(100000)
print("Q(i+1) < Q(i) for i [1..100000] is true %i times." %
      sum([q.seq[i] > q.seq[i + 1] for i in range(1, 100000)]))
```



## R


```rsplus

cache <- vector("integer", 0)
cache[1] <- 1
cache[2] <- 1

Q <- function(n) {
  if (is.na(cache[n])) {
    value <- Q(n-Q(n-1)) + Q(n-Q(n-2))
    cache[n] <<- value
  }
  cache[n]
}

for (i in 1:1e5) {
  Q(i)
}

for (i in 1:10) {
  cat(Q(i)," ",sep = "")
}
cat("\n")
cat(Q(1000),"\n")

count <- 0
for (i in 2:1e5) {
  if (Q(i) < Q(i-1)) count <- count + 1
}
cat(count,"terms is less than its preceding term\n")

```

{{out}}

```txt

1 1 2 3 3 4 5 5 6 6
502
49798 terms is less than its preceding term

```



## Racket


```racket

#lang racket

(define t (make-hash))
(hash-set! t 0 0)
(hash-set! t 1 1)
(hash-set! t 2 1)

(define (Q n)
  (hash-ref! t n (λ() (+ (Q (- n (Q (- n 1))))
                         (Q (- n (Q (- n 2))))))))

(for/list ([i (in-range 1 11)]) (Q i))
(Q 1000)

;; extra credit
(for/sum ([i 100000]) (if (< (Q (add1 i)) (Q i)) 1 0))

```


{{out}}

```txt

'(1 1 2 3 3 4 5 5 6 6)
502
49798

```



## REXX

===non-recursive===
The REXX language doesn't allow expressions for stemmed array indices, so a temporary variable must be used.

```rexx
/*REXX program generates the    Hofstadter  Q     sequence for any specified   N.       */
parse arg a b c d .                              /*obtain optional arguments from the CL*/
if a=='' | a==","  then a=       10              /*Not specified?  Then use the default.*/
if b=='' | b==","  then b=    -1000              /* "      "         "   "   "      "   */
if c=='' | c==","  then c=  -100000              /* "      "         "   "   "      "   */
if d=='' | d==","  then d= -1000000              /* "      "         "   "   "      "   */
q.= 1;                 ac=   abs(c)              /* [↑]  negative #'s don't show values.*/
call HofstadterQ  a
call HofstadterQ  b;   say;    say  abs(b)th(b)      'value is:'      result;          say
call HofstadterQ  c
downs= 0;              do j=2  for ac-1;        jm= j - 1
                          downs= downs + (q.j<q.jm)
                       end   /*j*/

say downs  'terms are less then the previous term,'    ac || th(ac)    'term is:'     q.ac
call HofstadterQ  d;                     ad= abs(d);            say
say 'The'      ad || th(ad)        'term is'           q.ad
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
HofstadterQ: procedure expose q.; parse arg x 1 ox     /*get number to generate through.*/
                                                       /* [↑]   OX    is the same as X. */
x= abs(x)                                              /*use the absolute value for  X. */
w= length(x)                                           /*use for right justified output.*/
             do j=1  for x                             /* [↓]  use short─circuit IF test*/
             if j>2   then if q.j==1  then  do;    jm1= j - 1;             jm2= j - 2
                                                    _1= j - q.jm1;          _2= j - q.jm2
                                                   q.j= q._1  +  q._2
                                            end
             if ox>0  then say right(j,w) right(q.j,w) /*display the number if  OX > 0. */
             end    /*j*/
return q.x                                             /*return the │X│th term to caller*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
th: procedure; x=abs(arg(1)); return word('th st nd rd',1+x//10*(x//100%10\==1)*(x//10<4))
```

{{out|output|text=  when using the internal default inputs:}}

```txt

 1  1
 2  1
 3  2
 4  3
 5  3
 6  4
 7  5
 8  5
 9  6
10  6

1000th value is: 502

49798 terms are less then the previous term, 100000th term is: 48157

The 1000000th term is 512066

```


===non-recursive, simpler===
This REXX example is identical to the first version
except that it uses a function to retrieve array elements which may have index expressions.

```rexx
/*REXX program generates the    Hofstadter  Q     sequence for any specified   N.       */
parse arg a b c d .                              /*obtain optional arguments from the CL*/
if a=='' | a==","  then a=       10              /*Not specified?  Then use the default.*/
if b=='' | b==","  then b=    -1000              /* "      "         "   "   "      "   */
if c=='' | c==","  then c=  -100000              /* "      "         "   "   "      "   */
if d=='' | d==","  then d= -1000000              /* "      "         "   "   "      "   */
q.= 1;                 ac=   abs(c)              /* [↑]  negative #'s don't show values.*/
call HofstadterQ  a
call HofstadterQ  b;   say;    say  abs(b)th(b)       'value is:'       result;        say
call HofstadterQ  c
downs=0;                       do j=2  for ac-1;    jm=j-1
                               downs= downs + (q.j<q.jm)
                               end   /*j*/

say downs  'terms are less then the previous term,'    ac || th(ac)    'term is:'     q.ac
call HofstadterQ  d;                     ad=abs(d);             say
say 'The'   ad || th(ad)   'term is'   q.ad
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
HofstadterQ: procedure expose q.; parse arg x 1 ox     /*get number to generate through.*/
                                                       /* [↑]   OX    is the same as X. */
x= abs(x)                                              /*use the absolute value for  X. */
w= length(x)                                           /*use for right justified output.*/
             do j=1  for x
             if j>2   then  if q.j==1  then  q.j= q(j - q(j-1))  +  q(j - q(j-2))
             if ox>0  then  say right(j, w)       right(q.j, w)           /*if X>0, tell*/
             end    /*j*/
return q.x                                             /*return the │X│th term to caller*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
q:  parse arg ?;              return q.?               /*return value of Q.? to invoker.*/
th: procedure; x=abs(arg(1)); return word('th st nd rd',1+x//10*(x//100%10\==1)*(x//10<4))
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}

Because of the additional subroutine (function) invokes, this REXX version is about half as fast as the 1<sup>st</sup> REXX version.


### recursive


```rexx
/*REXX program generates the    Hofstadter  Q     sequence for any specified   N.       */
parse arg a b c d .                              /*obtain optional arguments from the CL*/
if a=='' | a==","  then a=       10              /*Not specified?  Then use the default.*/
if b=='' | b==","  then b=    -1000              /* "      "         "   "   "      "   */
if c=='' | c==","  then c=  -100000              /* "      "         "   "   "      "   */
if d=='' | d==","  then d= -1000000              /* "      "         "   "   "      "   */
q.=0;  q.1=1;  q.2=1;  ac=   abs(c)              /* [↑]  negative #'s don't show values.*/
call HofstadterQ  a
call HofstadterQ  b;   say;    say  abs(b)th(b)      'value is:'      result;          say
call HofstadterQ  c
downs=0;                       do j=2  for ac-1;    jm= j-1
                               downs= downs + (q.j<q.jm)
                               end   /*j*/

say downs  'terms are less then the previous term,'    ac || th(ac)    "term is:"     q.ac
call HofstadterQ  d;                     ad=abs(d);             say
say 'The'      ad || th(ad)      "term is"      q.ad
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
HofstadterQ: procedure expose q.; parse arg x 1 ox     /*get number to generate through.*/
                                                       /* [↑]   OX    is the same as X. */
x= abs(x)                                              /*use the absolute value for  X. */
w= length(x)                                           /*use for right justified output.*/
            do j=1  for x
            if q.j==0 then q.j= QR(j)                  /*Not defined?    Then define it.*/
            if ox>0   then say right(j,w) right(q.j,w) /*show if OX>0*/
            end    /*j*/
return q.x                                             /*return the │X│th term to caller*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
QR: procedure expose q.;   parse arg n                 /*this  QR function is recursive.*/
    if q.n==0  then q.n= QR(n-QR(n-1)) + QR(n-QR(n-2)) /*Not defined?    Then define it.*/
    return q.n                                         /*return the value to the invoker*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
th: procedure; x=abs(arg(1)); return word('th st nd rd',1+x//10*(x//100%10\==1)*(x//10<4))
```

{{out|output|text=  is identical to the 1<sup>st</sup> REXX version.}}

The recursive version is almost ten times slower than the (1<sup>st</sup>) non-recursive version.




## Ring


```ring

n = 20
aList = list(n)
aList[1] = 1
aList[2] = 1
for i = 1 to n
    if i >= 3 aList[i] = ( aList[i - aList[i-1]] + aList[i - aList[i-2]] ) ok
    if i <= 20 see "n = " + string(i) + " : "+ aList[i] + nl ok
next

```



## Ruby


```ruby
@cache = []
def Q(n)
  if @cache[n].nil?
    case n
    when 1, 2 then @cache[n] = 1
    else @cache[n] = Q(n - Q(n-1)) + Q(n - Q(n-2))
    end
  end
  @cache[n]
end

puts "first 10 numbers in the sequence: #{(1..10).map {|n| Q(n)}}"
puts "1000'th term: #{Q(1000)}"

prev = Q(1)
count = 0
2.upto(100_000) do |n|
  q = Q(n)
  count += 1 if q < prev
  prev = q
end
puts "number of times in the first 100,000 terms where Q(i)<Q(i-1): #{count}"
```

{{out}}

```txt
first 10 numbers in the sequence: [1, 1, 2, 3, 3, 4, 5, 5, 6, 6]
1000'th term: 502
number of times in the first 100,000 terms where Q(i)<Q(i-1): 49798
```



## Run BASIC


```Runbasic
input "How many values do you want? :";n
dim Q(n)
Q(1)	= 1
Q(2)	= 1
for i = 1 to n
  if i >= 3 then Q(i) = ( Q(i - Q(i-1)) + Q(i - Q(i-2)) )
  if i <= 20 then print "n=";using("####",i);" ";using("###",Q(i))
next i
if i > 20 then print "n=";using("####",i);using("####",Q(i))
end

```

{{out}}

```txt

How many values do you want? :?1000
n=   1   1
n=   2   1
n=   3   2
n=   4   3
n=   5   3
n=   6   4
n=   7   5
n=   8   5
n=   9   6
n=  10   6
n=  11   6
n=  12   8
n=  13   8
n=  14   8
n=  15  10
n=  16   9
n=  17  10
n=  18  11
n=  19  11
n=  20  12
n=1000 502

```



## Rust


Rust doesn't allow static Vec's (but there's lazy_static crate), thus memoization storage is allocated in <code>main</code>.


```rust
fn hofq(q: &mut Vec<u32>, x : u32) -> u32 {
    let cur_len=q.len()-1;
    let i=x as usize;
    if i>cur_len {
        // extend storage
        q.reserve(i+1);
        for j in (cur_len+1)..(i+1) {
            let qj=(q[j-q[j-1] as usize]+q[j-q[j-2] as usize]) as u32;
            q.push(qj);
        }
    }
    q[i]
}

fn main() {
    let mut q_memo: Vec<u32>=vec![0,1,1];
    let mut q=|i| {hofq(&mut q_memo, i)};
    for i in 1..11 {
        println!("Q({})={}", i, q(i));
    }
    println!("Q(1000)={}", q(1000));
    let q100001=q(100_000); // precompute all
    println!("Q(100000)={}", q100000);
    let nless=(1..100_000).fold(0,|s,i|{if q(i+1)<q(i) {s+1} else {s}});
    println!("Term is less than preceding term {} times", nless);
}

```

{{out}}

```txt

Q(1)=1
Q(2)=1
Q(3)=2
Q(4)=3
Q(5)=3
Q(6)=4
Q(7)=5
Q(8)=5
Q(9)=6
Q(10)=6
Q(1000)=502
Q(100001)=53471
Term is less than preceding term 49798 times

```



## Scala

{{works with|Scala|2.9.1}}
Naive but elegant version using only recursion doesn't work
because runtime is excessive increasing ...

```scala
object HofstadterQseq extends App {
  val Q: Int => Int = n => {
    if (n <= 2) 1
    else Q(n-Q(n-1))+Q(n-Q(n-2))
  }
  (1 to 10).map(i=>(i,Q(i))).foreach(t=>println("Q("+t._1+") = "+t._2))
  println("Q("+1000+") = "+Q(1000))
}
```



Unfortunately the function Q isn't tail recursiv,
therefore the compiler can't optimize it.
Thus we are forced to use a caching featured version.


```scala
object HofstadterQseq extends App {

  val HofQ = scala.collection.mutable.Map((1->1),(2->1))

  val Q: Int => Int = n => {
    if (n < 1) 0
    else {
      val res = HofQ.keys.filter(_==n).toList match {
        case Nil => {val v = Q(n-Q(n-1))+Q(n-Q(n-2)); HofQ += (n->v); v}
        case xs => HofQ(n)
      }
      res
    }
  }

  (1 to 10).map(i=>(i,Q(i))).foreach(t=>println("Q("+t._1+") = "+t._2))
  println("Q("+1000+") = "+Q(1000))
  println((3 to 100000).filter(i=>Q(i)<Q(i-1)).size)
}
```

{{out}}

```txt
Q(1) = 1
Q(2) = 1
Q(3) = 2
Q(4) = 3
Q(5) = 3
Q(6) = 4
Q(7) = 5
Q(8) = 5
Q(9) = 6
Q(10) = 6
Q(1000) = 502
49798
```



## Scheme

I wish there were a portable way to <code>define-syntax</code>,
or to resize arrays, or to do formated output--anything to make the code
less silly looking while still run under more than one interpreter.

```lisp
(define qc '#(0 1 1))
(define filled 3)
(define len 3)

;; chicken scheme: vector-resize!
;; gambit: vector-append
(define (extend-qc)
  (let* ((new-len (* 2 len))
	 (new-qc (make-vector new-len)))
    (let copy ((n 0))
      (if (< n len)
	(begin
	  (vector-set! new-qc n (vector-ref qc n))
	  (copy (+ 1 n)))))
    (set! len new-len)
    (set! qc new-qc)))

(define (q n)
  (let loop ()
    (if (>= filled len) (extend-qc))
    (if (>= n filled)
      (begin
	(vector-set! qc filled (+ (q (- filled (q (- filled 1))))
				  (q (- filled (q (- filled 2))))))
	(set! filled (+ 1 filled))
	(loop))
      (vector-ref qc n))))

(display "Q(1 .. 10): ")
(let loop ((i 1))
  ;; (print) behave differently regarding newline across compilers
  (display (q i))
  (display " ")
  (if (< i 10)
    (loop (+ 1 i))
    (newline)))

(display "Q(1000): ")
(display (q 1000))
(newline)

(display "bumps up to 100000: ")
(display
  (let loop ((s 0) (i 1))
    (if (>= i 100000) s
      (loop (+ s (if (> (q i) (q (+ 1 i))) 1 0)) (+ 1 i)))))
(newline)
```

{{out}}

```txt
Q(1 .. 10): 1 1 2 3 3 4 5 5 6 6
Q(1000): 502
bumps up to 100000: 49798
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const type: intHash is hash [integer] integer;

var intHash: qHash is intHash.value;

const func integer: q (in integer: n) is func
  result
    var integer: q is 1;
  begin
    if n in qHash then
      q := qHash[n];
    else
      if n > 2 then
        q := q(n - q(pred(n))) + q(n - q(n - 2));
      end if;
      qHash @:= [n] q;
    end if;
  end func;

const proc: main is func
  local
    var integer: n is 0;
    var integer: less_than_preceding is 0;
  begin
    writeln("q(n) for n = 1 .. 10:");
    for n range 1 to 10 do
      write(q(n) <& " ");
    end for;
    writeln;
    writeln("q(1000)=" <& q(1000));
    for n range 2 to 100000 do
      if q(n) < q(pred(n)) then
        incr(less_than_preceding);
      end if;
    end for;
    writeln("q(n) < q(n-1) for n = 2 .. 100000: " <& less_than_preceding);
  end func;
```


{{out}}

```txt

q(n) for n = 1 .. 10:
1 1 2 3 3 4 5 5 6 6
q(1000)=502
q(n) < q(n-1) for n = 2 .. 100000: 49798

```



## Sidef

Using a memoized function:

```ruby
func Q(n) is cached {
    n <= 2 ? 1
           : Q(n - Q(n-1))+Q(n-Q(n-2))
}
 
say "First 10 terms: #{ {|n| Q(n) }.map(1..10) }"
say "Term 1000: #{Q(1000)}"
say "Terms less than preceding in first 100k: #{2..100000->count{|i|Q(i)<Q(i-1)}}"
```


Using an array:

```ruby
var Q = [0, 1, 1]
100_000.times {
    Q << (Q[-Q[-1]] + Q[-Q[-2]])
}
 
say "First 10 terms: #{Q.ft(1, 10)}"
say "Term 1000: #{Q[1000]}"
say "Terms less than preceding in first 100k: #{2..100000->count{|i|Q[i]<Q[i-1]}}"
```

{{out}}

```txt

First 10 terms: [1, 1, 2, 3, 3, 4, 5, 5, 6, 6]
Term 1000: 502
Terms less than preceding in first 100k: 49798

```



## Tailspin


```tailspin

templates q
  def outputFrom: $(1);
  def until: $(2);
  @: [1,1];
  1..$until -> #
  <$@::length~..>
    ..|@: $@($ - $@($ - 1)) + $@($ - $@($ - 2));
    $ -> #
  <$outputFrom..>
    $@($) !
end q

[1,10] -> q -> '$; ' -> !OUT::write
'
' -> !OUT::write

[1000,1000] -> q -> '$;
' -> !OUT::write

templates countDownSteps
  @: 0;
  def qs: $;
  2..$qs::length -> #
  $@ !
  <?($qs($) <..~$qs($-1)>)> @: $@ + 1;
end countDownSteps

[[1, 100000] -> q] -> countDownSteps -> 'Less than previous $; times' -> !OUT::write

```

{{out}}

```txt

1 1 2 3 3 4 5 5 6 6
502
Less than previous 49798 times

```



## Tcl


```tcl
package require Tcl 8.5

# Index 0 is not used, but putting it in makes the code a bit shorter
set tcl::mathfunc::Qcache {Q:-> 1 1}
proc tcl::mathfunc::Q {n} {
    variable Qcache
    if {$n >= [llength $Qcache]} {
	lappend Qcache [expr {Q($n - Q($n-1)) + Q($n - Q($n-2))}]
    }
    return [lindex $Qcache $n]
}

# Demonstration code
for {set i 1} {$i <= 10} {incr i} {
    puts "Q($i) == [expr {Q($i)}]"
}
# This runs very close to recursion limit...
puts "Q(1000) == [expr Q(1000)]"
# This code is OK, because the calculations are done step by step
set q [expr Q(1)]
for {set i 2} {$i <= 100000} {incr i} {
    incr count [expr {$q > [set q [expr {Q($i)}]]}]
}
puts "Q(i)<Q(i-1) for i \[2..100000\] is true $count times"
```

{{out}}

```txt

Q(1) == 1
Q(2) == 1
Q(3) == 2
Q(4) == 3
Q(5) == 3
Q(6) == 4
Q(7) == 5
Q(8) == 5
Q(9) == 6
Q(10) == 6
Q(1000) == 502
Q(i)<Q(i-1) for i [2..100000] is true 49798 times

```



## VBScript


```vb

Sub q_sequence(n)
	Dim Q()
	ReDim Q(n)
	Q(1)=1 : Q(2)=1 : Q(3)=2
	less_precede = 0
	For i = 4 To n
	 Q(i)=Q(i-Q(i-1))+Q(i-Q(i-2))
	 If Q(i) < Q(i-1) Then
	 	less_precede = less_precede + 1
	 End If
	Next
	WScript.StdOut.Write "First 10 terms of the sequence: "
	For j = 1 To 10
		If j < 10 Then
			WScript.StdOut.Write Q(j) & ", "
		Else
			WScript.StdOut.Write "and " & Q(j)
		End If
	Next
	WScript.StdOut.WriteLine
	WScript.StdOut.Write "1000th term of the sequence: " & Q(1000)
	WScript.StdOut.WriteLine
	WScript.StdOut.Write "Number of times the member of the sequence is less than its preceding term: " &_
		less_precede
End Sub

q_sequence(100000)

```


{{Out}}

```txt

First 10 terms of the sequence: 1, 1, 2, 3, 3, 4, 5, 5, 6, and 6
1000th term of the sequence: 502
Number of times the member of the sequence is less than its preceding term: 49798

```



## uBasic/4tH

{{trans|BBC BASIC}}
uBasic/4tH simply lacks the memory to make it through to the 1000th term. 256 is the best it can do.
<lang>Print "First 10 terms of Q = " ;
For i = 1 To 10 : Print FUNC(_q(i));" "; : Next : Print
Print "256th term = ";FUNC(_q(256))

End

_q Param(1)
  Local(2)

  If a@ < 3 Then Return (1)
  If a@ = 3 Then Return (2)

  @(0) = 1 : @(1) = 1 : @(2) = 2
  c@ = 0

  For b@ = 3 To a@-1
    @(b@) = @(b@ - @(b@-1)) + @(b@ - @(b@-2))
    If @(b@) < @(b@-1) Then c@ = c@ + 1
  Next

Return (@(a@-1))
```

{{out}}

```txt
First 10 terms of Q = 1 1 2 3 3 4 5 5 6 6
256th term = 123

0 OK, 0:320
```



## VBA


```vb
Public Q(100000) As Long
Public Sub HofstadterQ()
    Dim n As Long, smaller As Long
    Q(1) = 1
    Q(2) = 1
    For n = 3 To 100000
        Q(n) = Q(n - Q(n - 1)) + Q(n - Q(n - 2))
        If Q(n) < Q(n - 1) Then smaller = smaller + 1
    Next n
    Debug.Print "First ten terms:"
    For i = 1 To 10
        Debug.Print Q(i);
    Next i
    Debug.print
    Debug.Print "The 1000th term is:"; Q(1000)
    Debug.Print "Number of times smaller:"; smaller
End Sub
```
{{out}}

```txt
First ten terms:
 1  1  2  3  3  4  5  5  6  6
The 1000th term is: 502
Number of times smaller: 49798
```


## Visual FoxPro


```vfp

LOCAL p As Integer, i As Integer
CLEAR
p = 0
? "Hofstadter Q Sequence"
? "First 10 terms:"
FOR i = 1 TO 10
	?? Q(i, @p)
ENDFOR
? "1000th term:", Q(1000, @p)
? "100000th term:", q(100000, @p)
? "Number of terms less than the preceding term:", p

FUNCTION Q(n As Integer, k As Integer) As Integer
LOCAL i As Integer
LOCAL ARRAY aq[n]
aq[1] = 1
IF n > 1
    aq[2] = 1
ENDIF
k = 0
FOR i = 3 TO n
    aq[i] = aq[i - aq[i-1]] + aq[i-aq[i-2]]
    IF aq(i) < aq(i-1)
    	k = k + 1
    ENDIF
ENDFOR
RETURN aq[n]
ENDFUNC

```

{{out}}

```txt

Hofstadter Q Sequence
First 10 terms:  1    1    2    3    3    4    5    5    6   6
1000th term:     502
100000th term:   48157
Number of terms less than the preceding term:  49798

```



## XPL0


```XPL0
code ChOut=8, CrLf=9, IntOut=11;
int  N, C, Q(100_001);
[Q(1):= 1;  Q(2):= 1;  C:= 0;
for N:= 3 to 100_000 do
        [Q(N):= Q(N-Q(N-1)) + Q(N-Q(N-2));
        if Q(N) < Q(N-1) then C:= C+1;
        ];
for N:= 1 to 10 do
        [IntOut(0, Q(N));  ChOut(0, ^ )];
CrLf(0);
IntOut(0, Q(1000));  CrLf(0);
IntOut(0, C);  CrLf(0);
]
```


{{out}}

```txt

1 1 2 3 3 4 5 5 6 6
502
49798

```



## zkl

{{trans|ALGOL 68}}

```zkl
const n = 0d100_000;
q:=(n+1).pump(List.createLong(n+1).write); // (0,1,2,...,n) base 1
q[1] = q[2] = 1;

foreach i in ([3..n]) { q[i] = q[i - q[i - 1]] + q[i - q[i - 2]] }

q[1,10].concat(" ").println();
println(q[1000]);

flip := 0;
foreach i in (n){ flip += (q[i] > q[i + 1]) }
println("flips: ",flip);
```

{{out}}

```txt

1 1 2 3 3 4 5 5 6 6
502
flips: 49798

```



## ZX Spectrum Basic

{{trans|BBC_BASIC}}
Extra credit 100000 is not implemented because of memory limitations.

```zxbasic
10 PRINT "First 10 terms of Q = "
20 FOR i=1 TO 10: GO SUB 1000: PRINT s;" ";: NEXT i: PRINT
30 LET i=1000
40 PRINT "1000th term = ";: GO SUB 1000: PRINT s
50 PRINT "Term is less than preceding term ";c;" times"
100 STOP
1000 REM Qsequence subroutine
1010 IF i<3 THEN LET s=1: RETURN
1020 IF i=3 THEN LET s=2: RETURN
1030 DIM q(i)
1040 LET q(1)=1: LET q(2)=1: LET q(3)=2
1050 LET c=0
1060 FOR j=3 TO i
1070 LET q(j)=q(j-q(j-1))+q(j-q(j-2))
1080 IF q(j)<q(j-1) THEN LET c=c+1
1090 NEXT j
1100 LET s=q(i)
1110 RETURN
```

