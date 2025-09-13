+++
title = "Longest common subsequence"
description = ""
date = 2019-06-17T03:35:23Z
aliases = []
[extra]
id = 2852
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

{{task}}[[Category:Recursion]][[Category:Memoization]]
The '''longest common subsequence''' (or [http://en.wikipedia.org/wiki/Longest_common_subsequence_problem '''LCS''']) of groups A and B is the longest group of elements from A and B that are common between the two groups and in the same order in each group. For example, the sequences "1234" and "1224533324" have an LCS of "1234":
 '''<u>1234</u>'''
 '''<u>12</u>'''245'''<u>3</u>'''332'''<u>4</u>'''
For a string example, consider the sequences "thisisatest" and "testing123testing". An LCS would be "tsitest":
 '''<u>t</u>'''hi'''<u>si</u>'''sa'''<u>test</u>'''
 '''<u>t</u>'''e'''<u>s</u>'''t'''<u>i</u>'''ng123'''<u>test</u>'''ing

In this puzzle, your code only needs to deal with strings. Write a function which returns an LCS of two strings (case-sensitive). You don't need to show multiple LCS's.

For more information on this problem please see [[wp:Longest_common_subsequence_problem|Wikipedia]].


## Ada

Using recursion:

```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_LCS is
   function LCS (A, B : String) return String is
   begin
      if A'Length = 0 or else B'Length = 0 then
         return "";
      elsif A (A'Last) = B (B'Last) then
         return LCS (A (A'First..A'Last - 1), B (B'First..B'Last - 1)) & A (A'Last);
      else
         declare
            X : String renames LCS (A, B (B'First..B'Last - 1));
            Y : String renames LCS (A (A'First..A'Last - 1), B);
         begin
            if X'Length > Y'Length then
               return X;
            else
               return Y;
            end if;
         end;
      end if;
   end LCS;
begin
   Put_Line (LCS ("thisisatest", "testing123testing"));
end Test_LCS;
```

```txt

tsitest

```

Non-recursive solution:

```ada
with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_LCS is
   function LCS (A, B : String) return String is
      L : array (A'First..A'Last + 1, B'First..B'Last + 1) of Natural;
   begin
      for I in L'Range (1) loop
         L (I, B'First) := 0;
      end loop;
      for J in L'Range (2) loop
         L (A'First, J) := 0;
      end loop;
      for I in A'Range loop
         for J in B'Range loop
            if A (I) = B (J) then
               L (I + 1, J + 1) := L (I, J) + 1;
            else
               L (I + 1, J + 1) := Natural'Max (L (I + 1, J), L (I, J + 1));
            end if;
         end loop;
      end loop;
      declare
         I : Integer := L'Last (1);
         J : Integer := L'Last (2);
         R : String (1..Integer'Max (A'Length, B'Length));
         K : Integer := R'Last;
      begin
         while I > L'First (1) and then J > L'First (2) loop
            if L (I, J) = L (I - 1, J) then
               I := I - 1;
            elsif L (I, J) = L (I, J - 1) then
               J := J - 1;
            else
               I := I - 1;
               J := J - 1;
               R (K) := A (I);
               K := K - 1;
            end if;
         end loop;
         return R (K + 1..R'Last);
      end;
   end LCS;
begin
   Put_Line (LCS ("thisisatest", "testing123testing"));
end Test_LCS;
```

```txt

tsitest

```



## ALGOL 68

```algol68
main:(
   PROC lcs = (STRING a, b)STRING:
   BEGIN
      IF UPB a = 0 OR UPB b = 0 THEN
         ""
      ELIF a [UPB a] = b [UPB b] THEN
         lcs (a [:UPB a - 1], b [:UPB b - 1]) + a [UPB a]
      ELSE
         STRING x = lcs (a, b [:UPB b - 1]);
         STRING y = lcs (a [:UPB a - 1], b);
         IF UPB x > UPB y THEN x ELSE y FI
      FI
   END # lcs #;
   print((lcs ("thisisatest", "testing123testing"), new line))
)
```

```txt

tsitest

```


## APL

```APL
lcs←{
     ⎕IO←0
     betterof←{⊃(</+/¨⍺ ⍵)⌽⍺ ⍵}                     ⍝ better of 2 selections
     cmbn←{↑,⊃∘.,/(⊂⊂⍬),⍵}                          ⍝ combine lists
     rr←{∧/↑>/1 ¯1↓[1]¨⊂⍵}                          ⍝ rising rows
     hmrr←{∨/(rr ⍵)∧∧/⍵=⌈\⍵}                        ⍝ has monotonically rising rows
     rnbc←{{⍵/⍳⍴⍵}¨↓[0]×⍵}                          ⍝ row numbers by column
     valid←hmrr∘cmbn∘rnbc                           ⍝ any valid solutions?
     a w←(</⊃∘⍴¨⍺ ⍵)⌽⍺ ⍵                            ⍝ longest first
     matches←a∘.=w
     aps←{⍵[;⍒+⌿⍵]}∘{(⍵/2)⊤⍳2*⍵}                    ⍝ all possible subsequences
     swps←{⍵/⍨∧⌿~(~∨⌿⍺)⌿⍵}                          ⍝ subsequences with possible solns
     sstt←matches swps aps⊃⍴w                       ⍝ subsequences to try
     w/⍨{
         ⍺←0⍴⍨⊃⍴⍵                                   ⍝ initial selection
         (+/⍺)≥+/⍵[;0]:⍺                            ⍝ no scope to improve
         this←⍺ betterof{⍵×valid ⍵/matches}⍵[;0]    ⍝ try to improve
         1=1⊃⍴⍵:this                                ⍝ nothing left to try
         this ∇ 1↓[1]⍵                              ⍝ keep looking
     }sstt
 }
```



## AutoHotkey

{{trans|Java}} using dynamic programming

ahk forum: [http://www.autohotkey.com/forum/viewtopic.php?t=44657&start=135 discussion]

```AutoHotkey
lcs(a,b) { ; Longest Common Subsequence of strings, using Dynamic Programming
   Loop % StrLen(a)+2 {                          ; Initialize
      i := A_Index-1
      Loop % StrLen(b)+2
         j := A_Index-1, len%i%_%j% := 0
   }
   Loop Parse, a                                 ; scan a
   {
      i := A_Index, i1 := i+1, x := A_LoopField
      Loop Parse, b                              ; scan b
      {
         j := A_Index, j1 := j+1, y := A_LoopField
         len%i1%_%j1% := x=y ? len%i%_%j% + 1
         : (u:=len%i1%_%j%) > (v:=len%i%_%j1%) ? u : v
      }
   }
   x := StrLen(a)+1, y := StrLen(b)+1
   While x*y {                                   ; construct solution from lengths
     x1 := x-1, y1 := y-1
     If (len%x%_%y% = len%x1%_%y%)
         x := x1
     Else If  (len%x%_%y% = len%x%_%y1%)
         y := y1
     Else
         x := x1, y := y1, t := SubStr(a,x,1) t
   }
   Return t
}
```





## BASIC

```qbasic
FUNCTION lcs$ (a$, b$)
    IF LEN(a$) = 0 OR LEN(b$) = 0 THEN
	lcs$ = ""
    ELSEIF RIGHT$(a$, 1) = RIGHT$(b$, 1) THEN
	lcs$ = lcs$(LEFT$(a$, LEN(a$) - 1), LEFT$(b$, LEN(b$) - 1)) + RIGHT$(a$, 1)
    ELSE
	x$ = lcs$(a$, LEFT$(b$, LEN(b$) - 1))
	y$ = lcs$(LEFT$(a$, LEN(a$) - 1), b$)
	IF LEN(x$) > LEN(y$) THEN
		lcs$ = x$
	ELSE
		lcs$ = y$
	END IF
    END IF
END FUNCTION
```











## BBC BASIC

This makes heavy use of BBC BASIC's shortcut '''LEFT$(a$)''' and '''RIGHT$(a$)''' functions.

```bbcbasic
      PRINT FNlcs("1234", "1224533324")
      PRINT FNlcs("thisisatest", "testing123testing")
      END

      DEF FNlcs(a$, b$)
      IF a$="" OR b$="" THEN = ""
      IF RIGHT$(a$) = RIGHT$(b$) THEN = FNlcs(LEFT$(a$), LEFT$(b$)) + RIGHT$(a$)
      LOCAL x$, y$
      x$ = FNlcs(a$, LEFT$(b$))
      y$ = FNlcs(LEFT$(a$), b$)
      IF LEN(y$) > LEN(x$) SWAP x$,y$
      = x$
```

'''Output:'''

```txt

1234
tsitest

```



## Bracmat


```bracmat
  ( LCS
  =   A a ta B b tb prefix
    .     !arg:(?prefix.@(?A:%?a ?ta).@(?B:%?b ?tb))
        & ( !a:!b&LCS$(!prefix !a.!ta.!tb)
          | LCS$(!prefix.!A.!tb)&LCS$(!prefix.!ta.!B)
          )
      | !prefix:? ([>!max:[?max):?lcs
      |
  )
& 0:?max
& :?lcs
& LCS$(.thisisatest.testing123testing)
& out$(max !max lcs !lcs);
```

```txt
max 7 lcs t s i t e s t
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

#define MAX(a, b) (a > b ? a : b)

int lcs (char *a, int n, char *b, int m, char **s) {
    int i, j, k, t;
    int *z = calloc((n + 1) * (m + 1), sizeof (int));
    int **c = calloc((n + 1), sizeof (int *));
    for (i = 0; i <= n; i++) {
        c[i] = &z[i * (m + 1)];
    }
    for (i = 1; i <= n; i++) {
        for (j = 1; j <= m; j++) {
            if (a[i - 1] == b[j - 1]) {
                c[i][j] = c[i - 1][j - 1] + 1;
            }
            else {
                c[i][j] = MAX(c[i - 1][j], c[i][j - 1]);
            }
        }
    }
    t = c[n][m];
    *s = malloc(t);
    for (i = n, j = m, k = t - 1; k >= 0;) {
        if (a[i - 1] == b[j - 1])
            (*s)[k] = a[i - 1], i--, j--, k--;
        else if (c[i][j - 1] > c[i - 1][j])
            j--;
        else
            i--;
    }
    free(c);
    free(z);
    return t;
}

```

Testing

```c
int main () {
    char a[] = "thisisatest";
    char b[] = "testing123testing";
    int n = sizeof a - 1;
    int m = sizeof b - 1;
    char *s = NULL;
    int t = lcs(a, n, b, m, &s);
    printf("%.*s\n", t, s); // tsitest
    return 0;
}
```



## C++

'''The Longest Common Subsequence (LCS) Problem'''

Defining a subsequence to be a string obtained by deleting zero or more symbols from an input string, the LCS Problem is to find a subsequence of maximum length that is common to two input strings.

'''Background'''

Where the number of symbols appearing in matches is small relative to the length of the input strings, reuse of the symbols necessarily increases; and the number of matches will tend towards quadratic, O(m*n) growth.

This occurs, for example, in Bioinformatics applications of nucleotide and protein sequencing.

Here the "divide and conquer" approach of Hirschberg limits the space required to O(m+n).  However, this approach requires O(m*n) time even in the best case.

This quadratic time dependency may become prohibitive, given very long input strings.  Thus, heuristics are often favored over optimal Dynamic Programming solutions.

In the application of comparing file revisions, records from the input files form a large symbol space; and the number of symbols used for matches may approach the length of the LCS.

Assuming a uniform distribution of symbols, the number of matches may tend only towards linear, O(m+n) growth.

A binary search optimization due to Hunt and Szymanski can be applied in this case, which results in expected performance of O(n log m), given m <= n.  In the worst case, performance degrades to O(m*n log m) time if the number of matches, and the space required to represent them, should grow to O(m*n).

More recent improvements by Rick and by Goeman and Clausen reduce the time bound to O(n*s + min(p*m, p*(n-p))) where the alphabet is of size s and the LCS is of length p.

'''References'''

"A linear space algorithm for computing maximal common subsequences"<br />
by Daniel S. Hirschberg, published June 1975<br />
Communications of the ACM [Volume 18, Number 6, pp. 341–343]

"An Algorithm for Differential File Comparison"<br />
by James W. Hunt and M. Douglas McIlroy, June 1976<br />
Computing Science Technical Report, Bell Laboratories 41

"A Fast Algorithm for Computing Longest Common Subsequences"<br />
by James W. Hunt and Thomas G. Szymanski, published May 1977<br />
Communications of the ACM [Volume 20, Number 5, pp. 350-353]

"A new flexible algorithm for the longest common subsequence problem"<br />
by Claus Rick, published 1995, Proceedings, 6th Annual Symposium on<br />
Combinatorial Pattern Matching [Lecture Notes in Computer Science,<br />
Springer Verlag, Volume 937, pp. 340-351]

"A New Practical Linear Space Algorithm for the Longest Common<br />
Subsequence Problem" by Heiko Goeman and Michael Clausen,<br />
published 2002, Kybernetika [volume 38, Issue 1, pp. 45-66]

'''Hunt and Szymanski algorithm'''

```cpp
#include <stdint.h>
#include <string>
#include <memory>                       // for shared_ptr<>
#include <iostream>
#include <deque>
#include <map>
#include <algorithm>                    // for lower_bound()
#include <iterator>                     // for next() and prev()

using namespace std;

class LCS {
protected:
  // This linked list class is used to trace the LCS candidates
  class Pair {
  public:
    uint32_t index1;
    uint32_t index2;
    shared_ptr<Pair> next;

    Pair(uint32_t index1, uint32_t index2, shared_ptr<Pair> next = nullptr)
      : index1(index1), index2(index2), next(next) {
    }

    static shared_ptr<Pair> Reverse(const shared_ptr<Pair> pairs) {
      shared_ptr<Pair> head = nullptr;
      for (auto next = pairs; next != nullptr; next = next->next)
        head = make_shared<Pair>(next->index1, next->index2, head);
      return head;
    }
  };

  typedef deque<shared_ptr<Pair>> PAIRS;
  typedef deque<uint32_t> THRESHOLD;
  typedef deque<uint32_t> INDEXES;
  typedef map<char, INDEXES> CHAR2INDEXES;
  typedef deque<INDEXES*> MATCHES;

  // return the LCS as a linked list of matched index pairs
  uint32_t Pairs(MATCHES& matches, shared_ptr<Pair> *pairs) {
    auto trace = pairs != nullptr;
    PAIRS traces;
    THRESHOLD threshold;

    //
    //[Assert]After each index1 iteration threshold[index3] is the least index2
    // such that the LCS of s1[0:index1] and s2[0:index2] has length index3 + 1
    //
    uint32_t index1 = 0;
    for (const auto& it1 : matches) {
      if (!it1->empty()) {
        auto dq2 = *it1;
        auto limit = threshold.end();
        for (auto it2 = dq2.rbegin(); it2 != dq2.rend(); it2++) {
          // Each of the index1, index2 pairs considered here correspond to a match
          auto index2 = *it2;

          //
          // Note: The reverse iterator it2 visits index2 values in descending order,
          // allowing thresholds to be updated in-place.  std::lower_bound() is used
          // to perform a binary search.
          //
          limit = lower_bound(threshold.begin(), limit, index2);
          auto index3 = distance(threshold.begin(), limit);

          //
          // Look ahead to the next index2 value to optimize space used in the Hunt
          // and Szymanski algorithm.  If the next index2 is also an improvement on
          // the value currently held in threshold[index3], a new Pair will only be
          // superseded on the next index2 iteration.
          //
          // Depending on match redundancy, the number of Pair constructions may be
          // divided by factors ranging from 2 up to 10 or more.
          //
          auto skip = next(it2) != dq2.rend() &&
            (limit == threshold.begin() || *prev(limit) < *next(it2));
          if (skip) continue;

          if (limit == threshold.end()) {
            // insert case
            threshold.push_back(index2);
            // Refresh limit iterator:
            limit = prev(threshold.end());
            if (trace) {
              auto prefix = index3 > 0 ? traces[index3 - 1] : nullptr;
              auto last = make_shared<Pair>(index1, index2, prefix);
              traces.push_back(last);
            }
          }
          else if (index2 < *limit) {
            // replacement case
            *limit = index2;
            if (trace) {
              auto prefix = index3 > 0 ? traces[index3 - 1] : nullptr;
              auto last = make_shared<Pair>(index1, index2, prefix);
              traces[index3] = last;
            }
          }
        }                                 // next index2
      }

      index1++;
    }                                     // next index1

    if (trace) {
      auto last = traces.size() > 0 ? traces.back() : nullptr;
      // Reverse longest back-trace
      *pairs = Pair::Reverse(last);
    }

    auto length = threshold.size();
    return length;
  }

  //
  // Match() avoids incurring m*n comparisons by using the associative
  // memory implemented by CHAR2INDEXES to achieve O(m+n) performance,
  // where m and n are the input lengths.
  //
  // The lookup time can be assumed constant in the case of characters.
  // The symbol space is larger in the case of records; but the lookup
  // time will be O(log(m+n)), at most.
  //
  void Match(CHAR2INDEXES& indexes, MATCHES& matches,
    const string& s1, const string& s2) {
    uint32_t index = 0;
    for (const auto& it : s2)
      indexes[it].push_back(index++);

    for (const auto& it : s1) {
      auto& dq2 = indexes[it];
      matches.push_back(&dq2);
    }
  }

  string Select(shared_ptr<Pair> pairs, uint32_t length,
    bool right, const string& s1, const string& s2) {
    string buffer;
    buffer.reserve(length);
    for (auto next = pairs; next != nullptr; next = next->next) {
      auto c = right ? s2[next->index2] : s1[next->index1];
      buffer.push_back(c);
    }
    return buffer;
  }

public:
  string Correspondence(const string& s1, const string& s2) {
    CHAR2INDEXES indexes;
    MATCHES matches;                    // holds references into indexes
    Match(indexes, matches, s1, s2);
    shared_ptr<Pair> pairs;             // obtain the LCS as index pairs
    auto length = Pairs(matches, &pairs);
    return Select(pairs, length, false, s1, s2);
  }
};
```

'''Example''':

```cpp
    LCS lcs;
    auto s = lcs.Correspondence(s1, s2);
    cout << s << endl;
```


## C#

### With recursion


```c#
using System;

namespace LCS
{
    class Program
    {
        static void Main(string[] args)
        {
            string word1 = "thisisatest";
            string word2 = "testing123testing";

            Console.WriteLine(lcsBack(word1, word2));
            Console.ReadKey();
        }

        public static string lcsBack(string a, string b)
        {
            string aSub = a.Substring(0, (a.Length - 1 < 0) ? 0 : a.Length - 1);
            string bSub = b.Substring(0, (b.Length - 1 < 0) ? 0 : b.Length - 1);

            if (a.Length == 0 || b.Length == 0)
                return "";
            else if (a[a.Length - 1] == b[b.Length - 1])
                return lcsBack(aSub, bSub) + a[a.Length - 1];
            else
            {
                string x = lcsBack(a, bSub);
                string y = lcsBack(aSub, b);
                return (x.Length > y.Length) ? x : y;
            }
        }
    }
}
```



## Clojure

Based on algorithm from Wikipedia.

```Clojure
(defn longest [xs ys] (if (> (count xs) (count ys)) xs ys))

(def lcs
  (memoize
   (fn [[x & xs] [y & ys]]
     (cond
      (or (= x nil) (= y nil)) nil
      (= x y) (cons x (lcs xs ys))
      :else (longest (lcs (cons x xs) ys)
                     (lcs xs (cons y ys)))))))
```



## CoffeeScript



```coffeescript

lcs = (s1, s2) ->
  len1 = s1.length
  len2 = s2.length

  # Create a virtual matrix that is (len1 + 1) by (len2 + 1),
  # where m[i][j] is the longest common string using only
  # the first i chars of s1 and first j chars of s2.  The
  # matrix is virtual, because we only keep the last two rows
  # in memory.
  prior_row = ('' for i in [0..len2])

  for i in [0...len1]
    row = ['']
    for j in [0...len2]
      if s1[i] == s2[j]
        row.push prior_row[j] + s1[i]
      else
        subs1 = row[j]
        subs2 = prior_row[j+1]
        if subs1.length > subs2.length
          row.push subs1
        else
          row.push subs2
    prior_row = row

  row[len2]

s1 = "thisisatest"
s2 = "testing123testing"
console.log lcs(s1, s2)
```



## Common Lisp

Here's a memoizing/dynamic-programming solution that uses an <var>n &times; m</var> array where <var>n</var> and <var>m</var> are the lengths of the input arrays.  The first return value is a sequence (of the same type as array1) which is the longest common subsequence.  The second return value is the length of the longest common subsequence.

```lisp
(defun longest-common-subsequence (array1 array2)
  (let* ((l1 (length array1))
         (l2 (length array2))
         (results (make-array (list l1 l2) :initial-element nil)))
    (declare (dynamic-extent results))
    (labels ((lcs (start1 start2)
               ;; if either sequence is empty, return (() 0)
               (if (or (eql start1 l1) (eql start2 l2)) (list '() 0)
                 ;; otherwise, return any memoized value
                 (let ((result (aref results start1 start2)))
                   (if (not (null result)) result
                     ;; otherwise, compute and store a value
                     (setf (aref results start1 start2)
                           (if (eql (aref array1 start1) (aref array2 start2))
                             ;; if they start with the same element,
                             ;; move forward in both sequences
                             (destructuring-bind (seq len)
                                 (lcs (1+ start1) (1+ start2))
                               (list (cons (aref array1 start1) seq) (1+ len)))
                             ;; otherwise, move ahead in each separately,
                             ;; and return the better result.
                             (let ((a (lcs (1+ start1) start2))
                                   (b (lcs start1 (1+ start2))))
                               (if (> (second a) (second b))
                                 a
                                 b)))))))))
      (destructuring-bind (seq len) (lcs 0 0)
        (values (coerce seq (type-of array1)) len)))))
```


For example,


```lisp
(longest-common-subsequence "123456" "1a2b3c")
```


produces the two values


```lisp
"123"
3
```



### An alternative adopted from Clojure


Here is another version with its own memoization macro:


```lisp
(defmacro mem-defun (name args body)
  (let ((hash-name (gensym)))
    `(let ((,hash-name (make-hash-table :test 'equal)))
       (defun ,name ,args
         (or (gethash (list ,@args) ,hash-name)
             (setf (gethash (list ,@args) ,hash-name)
                   ,body))))))

(mem-defun lcs (xs ys)
  (labels ((longer (a b) (if (> (length a) (length b)) a b)))
     (cond ((or (null xs) (null ys)) nil)
           ((equal (car xs) (car ys)) (cons (car xs) (lcs (cdr xs) (cdr ys))))
	   (t (longer (lcs (cdr xs) ys)
		      (lcs xs (cdr ys)))))))
```


When we test it, we get:


```lisp
(coerce (lcs (coerce "thisisatest" 'list) (coerce "testing123testing" 'list)) 'string))))

"tsitest"
```



## D

Both versions don't work correctly with Unicode text.


### Recursive version


```d
import std.stdio, std.array;

T[] lcs(T)(in T[] a, in T[] b) pure nothrow @safe {
    if (a.empty || b.empty) return null;
    if (a[0] == b[0])
        return a[0] ~ lcs(a[1 .. $], b[1 .. $]);
    const longest = (T[] x, T[] y) => x.length > y.length ? x : y;
    return longest(lcs(a, b[1 .. $]), lcs(a[1 .. $], b));
}

void main() {
    lcs("thisisatest", "testing123testing").writeln;
}
```

```txt
tsitest
```



### Faster dynamic programming version

The output is the same.

```d
import std.stdio, std.algorithm, std.traits;

T[] lcs(T)(in T[] a, in T[] b) pure /*nothrow*/ {
    auto L = new uint[][](a.length + 1, b.length + 1);

    foreach (immutable i; 0 .. a.length)
        foreach (immutable j; 0 .. b.length)
            L[i + 1][j + 1] = (a[i] == b[j]) ? (1 + L[i][j]) :
                              max(L[i + 1][j], L[i][j + 1]);

    Unqual!T[] result;
    for (auto i = a.length, j = b.length; i > 0 && j > 0; ) {
        if (a[i - 1] == b[j - 1]) {
            result ~= a[i - 1];
            i--;
            j--;
        } else
            if (L[i][j - 1] < L[i - 1][j])
                i--;
            else
                j--;
    }

    result.reverse(); // Not nothrow.
    return result;
}

void main() {
    lcs("thisisatest", "testing123testing").writeln;
}
```



### Hirschberg algorithm version

See: http://en.wikipedia.org/wiki/Hirschberg_algorithm

This is currently a little slower than the classic dynamic programming version, but it uses a linear amount of memory, so it's usable for much larger inputs. To speed up this code on dmd remove the memory allocations from lensLCS, and do not use the retro range (replace it with foreach_reverse). The output is the same.

Adapted from Python code: http://wordaligned.org/articles/longest-common-subsequence


```d
import std.stdio, std.algorithm, std.range, std.array, std.string, std.typecons;

uint[] lensLCS(R)(R xs, R ys) pure nothrow @safe {
    auto prev = new typeof(return)(1 + ys.length);
    auto curr = new typeof(return)(1 + ys.length);

    foreach (immutable x; xs) {
        swap(curr, prev);
        size_t i = 0;
        foreach (immutable y; ys) {
            curr[i + 1] = (x == y) ? prev[i] + 1 : max(curr[i], prev[i + 1]);
            i++;
        }
    }

    return curr;
}

void calculateLCS(T)(in T[] xs, in T[] ys, bool[] xs_in_lcs,
                     in size_t idx=0) pure nothrow @safe {
    immutable nx = xs.length;
    immutable ny = ys.length;

    if (nx == 0)
        return;

    if (nx == 1) {
        if (ys.canFind(xs[0]))
            xs_in_lcs[idx] = true;
    } else {
        immutable mid = nx / 2;
        const xb = xs[0.. mid];
        const xe = xs[mid .. $];
        immutable ll_b = lensLCS(xb, ys);

        const ll_e = lensLCS(xe.retro, ys.retro); // retro is slow with dmd.

        //immutable k = iota(ny + 1)
        //              .reduce!(max!(j => ll_b[j] + ll_e[ny - j]));
        immutable k = iota(ny + 1)
                      .minPos!((i, j) => tuple(ll_b[i] + ll_e[ny - i]) >
                                         tuple(ll_b[j] + ll_e[ny - j]))[0];

        calculateLCS(xb, ys[0 .. k], xs_in_lcs, idx);
        calculateLCS(xe, ys[k .. $], xs_in_lcs, idx + mid);
    }
}

const(T)[] lcs(T)(in T[] xs, in T[] ys) pure /*nothrow*/ @safe {
    auto xs_in_lcs = new bool[xs.length];
    calculateLCS(xs, ys, xs_in_lcs);
    return zip(xs, xs_in_lcs).filter!q{ a[1] }.map!q{ a[0] }.array; // Not nothrow.
}

string lcsString(in string s1, in string s2) pure /*nothrow*/ @safe {
    return lcs(s1.representation, s2.representation).assumeUTF;
}

void main() {
    lcsString("thisisatest", "testing123testing").writeln;
}
```



## Dart


```dart
import 'dart:math';

String lcsRecursion(String a, String b) {
  int aLen = a.length;
  int bLen = b.length;

  if (aLen == 0 || bLen == 0) {
    return "";
  } else if (a[aLen-1] == b[bLen-1]) {
    return lcsRecursion(a.substring(0,aLen-1),b.substring(0,bLen-1)) + a[aLen-1];
  } else {
    var x = lcsRecursion(a, b.substring(0,bLen-1));
    var y = lcsRecursion(a.substring(0,aLen-1), b);
    return (x.length > y.length) ? x : y;
  }
}

String lcsDynamic(String a, String b) {
  var lengths = new List<List<int>>.generate(a.length + 1,
      (_) => new List.filled(b.length+1, 0), growable: false);

  // row 0 and column 0 are initialized to 0 already
  for (int i = 0; i < a.length; i++) {
    for (int j = 0; j < b.length; j++) {
      if (a[i] == b[j]) {
        lengths[i+1][j+1] = lengths[i][j] + 1;
      } else {
        lengths[i+1][j+1] = max(lengths[i+1][j], lengths[i][j+1]);
      }
    }
  }

  // read the substring out from the matrix
  StringBuffer reversedLcsBuffer = new StringBuffer();
  for (int x = a.length, y = b.length; x != 0 && y != 0;) {
    if (lengths[x][y] == lengths[x-1][y]) {
      x--;
    } else if (lengths[x][y] == lengths[x][y-1]) {
      y--;
    } else {
      assert(a[x-1] == b[y-1]);
      reversedLcsBuffer.write(a[x-1]);
      x--;
      y--;
    }
  }

  // reverse String
  var reversedLCS = reversedLcsBuffer.toString();
  var lcsBuffer = new StringBuffer();
  for(var i = reversedLCS.length - 1; i>=0; i--) {
    lcsBuffer.write(reversedLCS[i]);
  }
  return lcsBuffer.toString();
}

void main() {
  print("lcsDynamic('1234', '1224533324') =  ${lcsDynamic('1234', '1224533324')}");
  print("lcsDynamic('thisisatest', 'testing123testing') = ${lcsDynamic('thisisatest', 'testing123testing')}");
  print("lcsDynamic('', 'x') = ${lcsDynamic('', 'x')}");
  print("lcsDynamic('x', 'x') = ${lcsDynamic('x', 'x')}");
  print('');
  print("lcsRecursion('1234', '1224533324') = ${lcsRecursion('1234', '1224533324')}");
  print("lcsRecursion('thisisatest', 'testing123testing') = ${lcsRecursion('thisisatest', 'testing123testing')}");
  print("lcsRecursion('', 'x') = ${lcsRecursion('', 'x')}");
  print("lcsRecursion('x', 'x') = ${lcsRecursion('x', 'x')}");
}

```

```txt
lcsDynamic('1234', '1224533324') = 1234
lcsDynamic('thisisatest', 'testing123testing') = tsitest
lcsDynamic('', 'x') =
lcsDynamic('x', 'x') = x

lcsRecursion('1234', '1224533324') = 1234
lcsRecursion('thisisatest', 'testing123testing') = tsitest
lcsRecursion('', 'x') =
lcsRecursion('x', 'x') = x
```



## Egison



```egison

(define $common-seqs
  (lambda [$xs $ys]
    (match-all [xs ys] [(list char) (list char)]
      [[(loop $i [1 $n] <join _ <cons $c_i ...>> _)
        (loop $i [1 ,n] <join _ <cons ,c_i ...>> _)]
       (map (lambda [$i] c_i) (between 1 n))])))

(define $lcs (compose common-seqs rac))

```

'''Output:'''

```egison

> (lcs "thisisatest" "testing123testing"))
"tsitest"

```



## Elixir

### Simple recursion

This solution is Brute force. It is slow
```elixir
defmodule LCS do
  def lcs(a, b) do
    lcs(to_charlist(a), to_charlist(b), []) |> to_string
  end

  defp lcs([h|at], [h|bt], res), do: lcs(at, bt, [h|res])
  defp lcs([_|at]=a, [_|bt]=b, res) do
    Enum.max_by([lcs(a, bt, res), lcs(at, b, res)], &length/1)
  end
  defp lcs(_, _, res), do: res |> Enum.reverse
end

IO.puts LCS.lcs("thisisatest", "testing123testing")
IO.puts LCS.lcs('1234','1224533324')
```



### Dynamic Programming

```elixir
defmodule LCS do
  def lcs_length(s,t), do: lcs_length(s,t,Map.new) |> elem(0)

  defp lcs_length([],t,cache), do: {0,Map.put(cache,{[],t},0)}
  defp lcs_length(s,[],cache), do: {0,Map.put(cache,{s,[]},0)}
  defp lcs_length([h|st]=s,[h|tt]=t,cache) do
    {l,c} = lcs_length(st,tt,cache)
    {l+1,Map.put(c,{s,t},l+1)}
  end
  defp lcs_length([_sh|st]=s,[_th|tt]=t,cache) do
    if Map.has_key?(cache,{s,t}) do
      {Map.get(cache,{s,t}),cache}
    else
      {l1,c1} = lcs_length(s,tt,cache)
      {l2,c2} = lcs_length(st,t,c1)
      l = max(l1,l2)
      {l,Map.put(c2,{s,t},l)}
    end
  end

  def lcs(s,t) do
    {s,t} = {to_charlist(s),to_charlist(t)}
    {_,c} = lcs_length(s,t,Map.new)
    lcs(s,t,c,[]) |> to_string
  end

  defp lcs([],_,_,acc), do: Enum.reverse(acc)
  defp lcs(_,[],_,acc), do: Enum.reverse(acc)
  defp lcs([h|st],[h|tt],cache,acc), do: lcs(st,tt,cache,[h|acc])
  defp lcs([_sh|st]=s,[_th|tt]=t,cache,acc) do
    if Map.get(cache,{s,tt}) > Map.get(cache,{st,t}) do
      lcs(s,tt,cache,acc)
    else
      lcs(st,t,cache,acc)
    end
  end
end

IO.puts LCS.lcs("thisisatest","testing123testing")
IO.puts LCS.lcs("1234","1224533324")
```


```txt

tsitest
1234

```

Referring to LCS [[Shortest common supersequence#Elixir|here]].


## Erlang

This implementation also includes the ability to calculate the length of the longest common subsequence. In calculating that length, we generate a cache which can be traversed to generate the longest common subsequence.

```erlang

module(lcs).
-compile(export_all).

lcs_length(S,T) ->
    {L,_C} = lcs_length(S,T,dict:new()),
    L.

lcs_length([]=S,T,Cache) ->
    {0,dict:store({S,T},0,Cache)};
lcs_length(S,[]=T,Cache) ->
    {0,dict:store({S,T},0,Cache)};
lcs_length([H|ST]=S,[H|TT]=T,Cache) ->
    {L,C} = lcs_length(ST,TT,Cache),
    {L+1,dict:store({S,T},L+1,C)};
lcs_length([_SH|ST]=S,[_TH|TT]=T,Cache) ->
    case dict:is_key({S,T},Cache) of
        true -> {dict:fetch({S,T},Cache),Cache};
        false ->
            {L1,C1} = lcs_length(S,TT,Cache),
            {L2,C2} = lcs_length(ST,T,C1),
            L = lists:max([L1,L2]),
            {L,dict:store({S,T},L,C2)}
    end.

lcs(S,T) ->
    {_,C} = lcs_length(S,T,dict:new()),
    lcs(S,T,C,[]).

lcs([],_,_,Acc) ->
    lists:reverse(Acc);
lcs(_,[],_,Acc) ->
    lists:reverse(Acc);
lcs([H|ST],[H|TT],Cache,Acc) ->
    lcs(ST,TT,Cache,[H|Acc]);
lcs([_SH|ST]=S,[_TH|TT]=T,Cache,Acc) ->
    case dict:fetch({S,TT},Cache) > dict:fetch({ST,T},Cache) of
        true ->
            lcs(S,TT,Cache, Acc);
        false ->
            lcs(ST,T,Cache,Acc)
    end.

```

'''Output:'''

```erlang

77> lcs:lcs("thisisatest","testing123testing").
"tsitest"
78> lcs:lcs("1234","1224533324").
"1234"

```


We can also use the process dictionary to memoize the recursive implementation:


```erlang

lcs(Xs0, Ys0) ->
    CacheKey = {lcs_cache, Xs0, Ys0},
    case get(CacheKey)
    of  undefined ->
            Result =
                case {Xs0, Ys0}
                of  {[], _} -> []
                ;   {_, []} -> []
                ;   {[Same | Xs], [Same | Ys]} ->
                        [Same | lcs(Xs, Ys)]
                ;   {[_ | XsRest]=XsAll, [_ | YsRest]=YsAll} ->
                        A = lcs(XsRest, YsAll),
                        B = lcs(XsAll , YsRest),
                        case length(A) > length(B)
                        of  true  -> A
                        ;   false -> B
                        end
                end,
            undefined = put(CacheKey, Result),
            Result
    ;   Result ->
            Result
    end.

```


Similar to the above, but without using the process dictionary:

```erlang

-module(lcs).

%% API exports
-export([
        lcs/2
]).

%%
### ==============================================================

%% API functions
%%
### ==============================================================


lcs(A, B) ->
        {LCS, _Cache} = get_lcs(A, B, [], #{}),
        lists:reverse(LCS).

%%
### ==============================================================

%% Internal functions
%%
### ===============================================


get_lcs(A, B, Acc, Cache) ->
        case maps:find({A, B, Acc}, Cache) of
                {ok, LCS} -> {LCS, Cache};
                error     ->
                        {NewLCS, NewCache} = compute_lcs(A, B, Acc, Cache),
                        {NewLCS, NewCache#{ {A, B, Acc} => NewLCS }}
        end.

compute_lcs(A, B, Acc, Cache) when length(A) == 0 orelse length(B) == 0 ->
        {Acc, Cache};
compute_lcs([Token |ATail], [Token |BTail], Acc, Cache) ->
        get_lcs(ATail, BTail, [Token |Acc], Cache);
compute_lcs([_AToken |ATail]=A, [_BToken |BTail]=B, Acc, Cache) ->
        {LCSA, CacheA} = get_lcs(A, BTail, Acc, Cache),
        {LCSB, CacheB} = get_lcs(ATail, B, Acc, CacheA),
        LCS = case length(LCSA) > length(LCSB) of
                true  -> LCSA;
                false -> LCSB
        end,
        {LCS, CacheB}.

```


'''Output:'''

```erlang

48> lcs:lcs("thisisatest", "testing123testing").
"tsitest"

```



## Fortran

Using the <tt>iso_varying_string</tt> module which can be found [http://www.fortran.com/iso_varying_string.f95 here] (or equivalent module conforming to the ISO/IEC 1539-2:2000 API or to a subset according to the need of this code: <code>char</code>, <code>len</code>, <code>//</code>, <code>extract</code>, <code>==</code>, <code>=</code>)


```fortran
program lcstest
  use iso_varying_string
  implicit none

  type(varying_string) :: s1, s2

  s1 = "thisisatest"
  s2 = "testing123testing"
  print *, char(lcs(s1, s2))

  s1 = "1234"
  s2 = "1224533324"
  print *, char(lcs(s1, s2))

contains

  recursive function lcs(a, b) result(l)
    type(varying_string) :: l
    type(varying_string), intent(in) :: a, b

    type(varying_string) :: x, y

    l = ""
    if ( (len(a) == 0) .or. (len(b) == 0) ) return
    if ( extract(a, len(a), len(a)) == extract(b, len(b), len(b)) ) then
       l = lcs(extract(a, 1, len(a)-1), extract(b, 1, len(b)-1)) // extract(a, len(a), len(a))
    else
       x = lcs(a, extract(b, 1, len(b)-1))
       y = lcs(extract(a, 1, len(a)-1), b)
       if ( len(x) > len(y) ) then
          l = x
       else
          l = y
       end if
    end if
  end function lcs

end program lcstest
```

=={{header|F Sharp|F#}}==
Copied and slightly adapted from OCaml (direct recursion)

```fsharp
open System

let longest xs ys = if List.length xs > List.length ys then xs else ys

let rec lcs a b =
    match a, b with
    | [], _
    | _, []        -> []
    | x::xs, y::ys ->
        if x = y then
            x :: lcs xs ys
        else
            longest (lcs a ys) (lcs xs b)

[<EntryPoint>]
let main argv =
    let split (str:string) = List.init str.Length (fun i -> str.[i])
    printfn "%A" (String.Join("",
        (lcs (split "thisisatest") (split "testing123testing"))))
    0

```



## Factor


```factor
USE: lcs
"thisisatest" "testing123testing" lcs print
```

```txt

tsitest

```



## Go

### Recursion

Brute force

```go
func lcs(a, b string) string {
    aLen := len(a)
    bLen := len(b)
    if aLen == 0 || bLen == 0 {
        return ""
    } else if a[aLen-1] == b[bLen-1] {
        return lcs(a[:aLen-1], b[:bLen-1]) + string(a[aLen-1])
    }
    x := lcs(a, b[:bLen-1])
    y := lcs(a[:aLen-1], b)
    if len(x) > len(y) {
        return x
    }
    return y
}
```



### Dynamic Programming


```go
func lcs(a, b string) string {
	arunes := []rune(a)
	brunes := []rune(b)
	aLen := len(arunes)
	bLen := len(brunes)
	lengths := make([][]int, aLen+1)
	for i := 0; i <= aLen; i++ {
		lengths[i] = make([]int, bLen+1)
	}
	// row 0 and column 0 are initialized to 0 already

	for i := 0; i < aLen; i++ {
		for j := 0; j < bLen; j++ {
			if arunes[i] == brunes[j] {
				lengths[i+1][j+1] = lengths[i][j] + 1
			} else if lengths[i+1][j] > lengths[i][j+1] {
				lengths[i+1][j+1] = lengths[i+1][j]
			} else {
				lengths[i+1][j+1] = lengths[i][j+1]
			}
		}
	}

	// read the substring out from the matrix
	s := make([]rune, 0, lengths[aLen][bLen])
	for x, y := aLen, bLen; x != 0 && y != 0; {
		if lengths[x][y] == lengths[x-1][y] {
			x--
		} else if lengths[x][y] == lengths[x][y-1] {
			y--
		} else {
			s = append(s, arunes[x-1])
			x--
			y--
		}
	}
	// reverse string
	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		s[i], s[j] = s[j], s[i]
	}
	return string(s)
}
```



## Groovy

Recursive solution:

```groovy
def lcs(xstr, ystr) {
    if (xstr == "" || ystr == "") {
        return "";
    }

    def x = xstr[0];
    def y = ystr[0];

    def xs = xstr.size() > 1 ? xstr[1..-1] : "";
    def ys = ystr.size() > 1 ? ystr[1..-1] : "";

    if (x == y) {
        return (x + lcs(xs, ys));
    }

    def lcs1 = lcs(xstr, ys);
    def lcs2 = lcs(xs, ystr);

    lcs1.size() > lcs2.size() ? lcs1 : lcs2;
}

println(lcs("1234", "1224533324"));
println(lcs("thisisatest", "testing123testing"));
```

```txt
1234
tsitest
```



## Haskell


The [[wp:Longest_common_subsequence#Solution_for_two_sequences|Wikipedia solution]] translates directly into Haskell, with the only difference that equal characters are added in front:


```haskell>longest xs ys = if length xs
 length ys then xs else ys

lcs [] _ = []
lcs _ [] = []
lcs (x:xs) (y:ys)
  | x == y    = x : lcs xs ys
  | otherwise = longest (lcs (x:xs) ys) (lcs xs (y:ys))
```


A Memoized version of the naive algorithm.


```haskell
import qualified Data.MemoCombinators as M

lcs = memoize lcsm
       where
         lcsm [] _ = []
         lcsm _ [] = []
         lcsm (x:xs) (y:ys)
           | x == y    = x : lcs xs ys
           | otherwise = maxl (lcs (x:xs) ys) (lcs xs (y:ys))

maxl x y = if length x > length y then x else y
memoize = M.memo2 mString mString
mString = M.list M.char -- Chars, but you can specify any type you need for the memo
```


Memoization (aka dynamic programming) of that uses ''zip'' to make both the index and the character available:


```haskell
import Data.Array

lcs xs ys = a!(0,0) where
  n = length xs
  m = length ys
  a = array ((0,0),(n,m)) $ l1 ++ l2 ++ l3
  l1 = [((i,m),[]) | i <- [0..n]]
  l2 = [((n,j),[]) | j <- [0..m]]
  l3 = [((i,j), f x y i j) | (x,i) <- zip xs [0..], (y,j) <- zip ys [0..]]
  f x y i j
    | x == y    = x : a!(i+1,j+1)
    | otherwise = longest (a!(i,j+1)) (a!(i+1,j))
```

All 3 solutions work of course not only with strings, but also with any other list. Example:

```haskell
*Main> lcs "thisisatest" "testing123testing"
"tsitest"
```

The dynamic programming version without using arrays:

```haskell
import Data.List

longest xs ys = if length xs > length ys then xs else ys

lcs xs ys = head $ foldr(\xs -> map head. scanr1 f. zipWith (\x y -> [x,y]) xs) e m where
    m = map (\x -> flip (++) [[]] $ map (\y -> [x | x==y]) ys) xs
    e = replicate (length ys) []
    f [a,b] [c,d]
     | null a = longest b c: [b]
     | otherwise = (a++d):[b]
```



Simple and slow solution:


```haskell
import Data.Ord
import Data.List

--          longest                        common
lcs xs ys = maximumBy (comparing length) $ intersect (subsequences xs) (subsequences ys)

main = print $ lcs "thisisatest" "testing123testing"
```

```txt
"tsitest"
```


=={{header|Icon}} and {{header|Unicon}}==
This solution is a modified variant of the recursive solution.  The modifications include (a) deleting all characters not common to both strings and (b) stripping off common prefixes and suffixes in a single step.

{{libheader|Icon Programming Library}} [http://www.cs.arizona.edu/icon/library/src/procs/strings.icn Uses deletec from strings]


```Icon
procedure main()
LCSTEST("thisisatest","testing123testing")
LCSTEST("","x")
LCSTEST("x","x")
LCSTEST("beginning-middle-ending","beginning-diddle-dum-ending")
end

link strings

procedure LCSTEST(a,b)    #: helper to show inputs and results
write("lcs( ",image(a),", ",image(b)," ) = ",image(res := lcs(a,b)))
return res
end

procedure lcs(a,b)     #: return longest common sub-sequence of characters (modified recursive method)
local i,x,y
local c,nc

   if *(a|b) = 0 then return ""                               # done if either string is empty
   if a == b then return a                                    # done if equal

   if *(a ++ b -- (c := a ** b)) > 0 then {                   # find all characters not in common
      a := deletec(a,nc := ~c)                                # .. remove
      b := deletec(b,nc)                                      # .. remove
      }                                                       # only unequal strings and shared characters beyond

   i := 0 ; while a[i+1] == b[i+1] do i +:=1                  # find common prefix ...
   if *(x := a[1+:i]) > 0  then                               # if any
      return x || lcs(a[i+1:0],b[i+1:0])                      # ... remove and process remainder

   i := 0 ; while a[-(i+1)] == b[-(i+1)] do i +:=1            # find common suffix ...
   if *(y := a[0-:i]) > 0 then                                # if any
      return lcs(a[1:-i],b[1:-i]) || y                        # ... remove and process remainder

   return if *(x := lcs(a,b[1:-1])) > *(y := lcs(a[1:-1],b)) then x else y  # divide, discard, and keep longest
end
```

```txt
lcs( "thisisatest", "testing123testing" ) = "tsitest"
lcs( "", "x" ) = ""
lcs( "x", "x" ) = "x"
lcs( "beginning-middle-ending", "beginning-diddle-dum-ending" ) = "beginning-iddle-ending"
```



## J


```j
lcs=: dyad define
 |.x{~ 0{"1 cullOne^:_ (\: +/"1)(\:{."1) 4$.$. x =/ y
)

cullOne=: ({~[: <@<@< [: (i. 0:)1,[: *./[: |: 2>/\]) :: ]
```


Here's [[Longest_common_subsequence/J|another approach]]:


```J
mergeSq=: ;@}:  ~.@, {.@;@{. ,&.> 3 {:: 4&{.
common=: 2 2 <@mergeSq@,;.3^:_ [: (<@#&.> i.@$) =/
lcs=: [ {~ 0 {"1 ,&$ #: 0 ({:: (#~ [: (= >./) #@>)) 0 ({:: ,) common
```


Example use (works with either definition of lcs):


```J
   'thisisatest' lcs 'testing123testing'
tsitest
```


'''Dynamic programming version'''

```j
longest=: ]`[@.(>&#)
upd=:{:@[,~ ({.@[ ,&.> {:@])`({:@[ longest&.> {.@])@.(0 = #&>@{.@[)
lcs=: 0{:: [: ([: {.&> [: upd&.>/\.<"1@:,.)/ a:,.~a:,~=/{"1 a:,.<"0@[
```

'''Output:'''

```j
   '1234' lcs '1224533324'
1234

   'thisisatest' lcs 'testing123testing'
tsitest
```


'''Recursion'''

```j
lcs=:;(($:}.) longest }.@[ $: ])`({.@[,$:&}.)@.(=&{.)`((i.0)"_)@.(+.&(0=#))&((e.#[)&>/) ;~
```



## Java


### Recursion

This is not a particularly fast algorithm, but it gets the job done eventually. The speed is a result of many recursive function calls.

```java
public static String lcs(String a, String b){
    int aLen = a.length();
    int bLen = b.length();
    if(aLen == 0 || bLen == 0){
        return "";
    }else if(a.charAt(aLen-1) == b.charAt(bLen-1)){
        return lcs(a.substring(0,aLen-1),b.substring(0,bLen-1))
            + a.charAt(aLen-1);
    }else{
        String x = lcs(a, b.substring(0,bLen-1));
        String y = lcs(a.substring(0,aLen-1), b);
        return (x.length() > y.length()) ? x : y;
    }
}
```



### Dynamic Programming


```java
public static String lcs(String a, String b) {
    int[][] lengths = new int[a.length()+1][b.length()+1];

    // row 0 and column 0 are initialized to 0 already

    for (int i = 0; i < a.length(); i++)
        for (int j = 0; j < b.length(); j++)
            if (a.charAt(i) == b.charAt(j))
                lengths[i+1][j+1] = lengths[i][j] + 1;
            else
                lengths[i+1][j+1] =
                    Math.max(lengths[i+1][j], lengths[i][j+1]);

    // read the substring out from the matrix
    StringBuffer sb = new StringBuffer();
    for (int x = a.length(), y = b.length();
         x != 0 && y != 0; ) {
        if (lengths[x][y] == lengths[x-1][y])
            x--;
        else if (lengths[x][y] == lengths[x][y-1])
            y--;
        else {
            assert a.charAt(x-1) == b.charAt(y-1);
            sb.append(a.charAt(x-1));
            x--;
            y--;
        }
    }

    return sb.reverse().toString();
}
```



## JavaScript


### Recursion

This is more or less a translation of the recursive Java version above.

```javascript
function lcs(a, b) {
  var aSub = a.substr(0, a.length - 1);
  var bSub = b.substr(0, b.length - 1);

  if (a.length === 0 || b.length === 0) {
    return '';
  } else if (a.charAt(a.length - 1) === b.charAt(b.length - 1)) {
    return lcs(aSub, bSub) + a.charAt(a.length - 1);
  } else {
    var x = lcs(a, bSub);
    var y = lcs(aSub, b);
    return (x.length > y.length) ? x : y;
  }
}
```


ES6 recursive implementation


```javascript

var longest = (xs, ys) => (xs.length > ys.length) ? xs : ys;

var lcs = (xx, yy) => {
  if (!xx.length || !yy.length) { return ''; }

  var x = xx[0],
      y = yy[0];
  xs = xx.slice(1);
  ys = yy.slice(1);

  return (x === y) ? lcs(xs, ys) :
                     longest(lcs(xx, ys), lcs(xs, yy));
};
```



### Dynamic Programming

This  version runs in O(mn) time and consumes O(mn) space.
Factoring out loop edge cases could get a small constant time improvement, and it's fairly trivial to edit the final loop to produce a full diff in addition to the lcs.

```javascript
function lcs(x,y){
	var s,i,j,m,n,
		lcs=[],row=[],c=[],
		left,diag,latch;
	//make sure shorter string is the column string
	if(m<n){s=x;x=y;y=s;}
	m = x.length;
	n = y.length;
	//build the c-table
	for(j=0;j<n;row[j++]=0);
	for(i=0;i<m;i++){
		c[i] = row = row.slice();
		for(diag=0,j=0;j<n;j++,diag=latch){
			latch=row[j];
			if(x[i] == y[j]){row[j] = diag+1;}
			else{
				left = row[j-1]||0;
				if(left>row[j]){row[j] = left;}
			}
		}
	}
	i--,j--;
	//row[j] now contains the length of the lcs
	//recover the lcs from the table
	while(i>-1&&j>-1){
		switch(c[i][j]){
			default: j--;
				lcs.unshift(x[i]);
			case (i&&c[i-1][j]): i--;
				continue;
			case (j&&c[i][j-1]): j--;
		}
	}
	return lcs.join('');
}
```


'''BUG note: In line 6, m and n are not yet initialized, and so x and y are never swapped.'''
'''Swapping is useless here, and becomes wrong when extending the algorithm to produce a diff.'''

The final loop can be modified to concatenate maximal common substrings rather than individual characters:

```javascript
	var t=i;
	while(i>-1&&j>-1){
		switch(c[i][j]){
			default:i--,j--;
				continue;
			case (i&&c[i-1][j]):
				if(t!==i){lcs.unshift(x.substring(i+1,t+1));}
				t=--i;
				continue;
			case (j&&c[i][j-1]): j--;
				if(t!==i){lcs.unshift(x.substring(i+1,t+1));}
				t=i;
		}
	}
	if(t!==i){lcs.unshift(x.substring(i+1,t+1));}
```



### Greedy Algorithm

This is an heuristic algorithm that won't always return the correct answer, but is significantly faster and less memory intensive than the dynamic programming version, in exchange for giving up the ability to re-use the table to find alternate solutions and greater complexity in generating diffs. Note that this implementation uses a binary buffer for additional efficiency gains, but it's simple to transform to use string or array concatenation;

```javascript
function lcs_greedy(x,y){
  var p1, i, idx,
      symbols = {},
      r = 0,
      p = 0,
      l = 0,
      m = x.length,
      n = y.length,
      s = new Buffer((m < n) ? n : m);

  p1 = popsym(0);

  for (i = 0; i < m; i++) {
    p = (r === p) ? p1 : popsym(i);
    p1 = popsym(i + 1);
    if (p > p1) {
      i += 1;
      idx = p1;
    } else {
      idx = p;
    }

    if (idx === n) {
      p = popsym(i);
    } else {
      r = idx;
      s[l] = x.charCodeAt(i);
      l += 1;
    }
  }
  return s.toString('utf8', 0, l);

  function popsym(index) {
    var s = x[index],
        pos = symbols[s] + 1;

    pos = y.indexOf(s, ((pos > r) ? pos : r));
    if (pos === -1) { pos = n; }
    symbols[s] = pos;
    return pos;
  }
}
```


Note that it won't return the correct answer for all inputs. For example:
```javascript
lcs_greedy('bcaaaade', 'deaaaabc'); // 'bc' instead of 'aaaa'
```



## jq

Naive recursive version:

```jq
def lcs(xstr; ystr):
  if (xstr == "" or ystr == "") then ""
  else
    xstr[0:1] as $x
    |  xstr[1:] as $xs
    |  ystr[1:] as $ys
    | if ($x == ystr[0:1]) then ($x + lcs($xs; $ys))
      else
        lcs(xstr; $ys) as $one
	| lcs($xs; ystr) as $two
	| if ($one|length) > ($two|length) then $one else $two end
      end
  end;
```


Example:

```jq
lcs("1234"; "1224533324"),
lcs("thisisatest"; "testing123testing")
```

Output:
```sh

# jq -n -f lcs-recursive.jq
"1234"
"tsitest"
```



## Julia

```julia
longest(a::String, b::String) = length(a) ≥ length(b) ? a : b

"""
julia> lcsrecursive("thisisatest", "testing123testing")
"tsitest"
"""
# Recursive
function lcsrecursive(xstr::String, ystr::String)
    if length(xstr) == 0 || length(ystr) == 0
        return ""
    end

    x, xs, y, ys = xstr[1], xstr[2:end], ystr[1], ystr[2:end]
    if x == y
        return string(x, lcsrecursive(xs, ys))
    else
        return longest(lcsrecursive(xstr, ys), lcsrecursive(xs, ystr))
    end
end

# Dynamic
function lcsdynamic(a::String, b::String)
    lengths = zeros(Int, length(a) + 1, length(b) + 1)

    # row 0 and column 0 are initialized to 0 already
    for (i, x) in enumerate(a), (j, y) in enumerate(b)
        if x == y
            lengths[i+1, j+1] = lengths[i, j] + 1
        else
            lengths[i+1, j+1] = max(lengths[i+1, j], lengths[i, j+1])
        end
    end

    # read the substring out from the matrix
    result = ""
    x, y = length(a) + 1, length(b) + 1
    while x > 1 && y > 1
        if lengths[x, y] == lengths[x-1, y]
            x -= 1
        elseif lengths[x, y] == lengths[x, y-1]
            y -= 1
        else
            @assert a[x-1] == b[y-1]
            result = string(a[x-1], result)
            x -= 1
            y -= 1
        end
    end

    return result
end


@show lcsrecursive("thisisatest", "testing123testing")
@time lcsrecursive("thisisatest", "testing123testing")
@show lcsdynamic("thisisatest", "testing123testing")
@time lcsdynamic("thisisatest", "testing123testing")
```


```txt
lcsrecursive("thisisatest", "testing123testing") = "tsitest"
  0.038153 seconds (537.77 k allocations: 16.415 MiB)
lcsdynamic("thisisatest", "testing123testing") = "tsitest"
  0.000004 seconds (12 allocations: 2.141 KiB)
```



## Kotlin


```scala
// version 1.1.2

fun lcs(x: String, y: String): String {
    if (x.length == 0 || y.length == 0) return ""
    val x1 = x.dropLast(1)
    val y1 = y.dropLast(1)
    if (x.last() == y.last()) return lcs(x1, y1) + x.last()
    val x2 = lcs(x, y1)
    val y2 = lcs(x1, y)
    return if (x2.length > y2.length) x2 else y2
}

fun main(args: Array<String>) {
    val x = "thisisatest"
    val y = "testing123testing"
    println(lcs(x, y))
}
```


```txt

tsitest

```



## Liberty BASIC


```lb

'variation of BASIC example
w$="aebdef"
z$="cacbc"
print lcs$(w$,z$)

'output:
'ab

wait

FUNCTION lcs$(a$, b$)
    IF LEN(a$) = 0 OR LEN(b$) = 0 THEN
        lcs$ = ""
        exit function
    end if

    IF RIGHT$(a$, 1) = RIGHT$(b$, 1) THEN
        lcs$ = lcs$(LEFT$(a$, LEN(a$) - 1), LEFT$(b$, LEN(b$) - 1)) + RIGHT$(a$, 1)
        exit function
    ELSE
        x$ = lcs$(a$, LEFT$(b$, LEN(b$) - 1))
        y$ = lcs$(LEFT$(a$, LEN(a$) - 1), b$)
        IF LEN(x$) > LEN(y$) THEN
            lcs$ = x$
            exit function
        ELSE
            lcs$ = y$
            exit function
        END IF
    END IF
END FUNCTION

```



## Logo

This implementation works on both words and lists.

```logo
to longest :s :t
  output ifelse greater? count :s count :t [:s] [:t]
end
to lcs :s :t
  if empty? :s [output :s]
  if empty? :t [output :t]
  if equal? first :s first :t [output combine  first :s  lcs bf :s bf :t]
  output longest lcs :s bf :t  lcs bf :s :t
end
```



## Lua


```lua
function LCS( a, b )
    if #a == 0 or #b == 0 then
        return ""
    elseif string.sub( a, -1, -1 ) == string.sub( b, -1, -1 ) then
        return LCS( string.sub( a, 1, -2 ), string.sub( b, 1, -2 ) ) .. string.sub( a, -1, -1 )
    else
        local a_sub = LCS( a, string.sub( b, 1, -2 ) )
        local b_sub = LCS( string.sub( a, 1, -2 ), b )

        if #a_sub > #b_sub then
            return a_sub
        else
            return b_sub
        end
    end
end

print( LCS( "thisisatest", "testing123testing" ) )
```



## M4


```M4
define(`set2d',`define(`$1[$2][$3]',`$4')')
define(`get2d',`defn($1[$2][$3])')
define(`tryboth',
   `pushdef(`x',lcs(`$1',substr(`$2',1),`$1 $2'))`'pushdef(`y',
         lcs(substr(`$1',1),`$2',`$1 $2'))`'ifelse(eval(len(x)>len(y)),1,
         `x',`y')`'popdef(`x')`'popdef(`y')')
define(`checkfirst',
   `ifelse(substr(`$1',0,1),substr(`$2',0,1),
      `substr(`$1',0,1)`'lcs(substr(`$1',1),substr(`$2',1))',
      `tryboth(`$1',`$2')')')
define(`lcs',
   `ifelse(get2d(`c',`$1',`$2'),`',
        `pushdef(`a',ifelse(
           `$1',`',`',
           `$2',`',`',
           `checkfirst(`$1',`$2')'))`'a`'set2d(`c',`$1',`$2',a)`'popdef(`a')',
        `get2d(`c',`$1',`$2')')')

lcs(`1234',`1224533324')

lcs(`thisisatest',`testing123testing')
```

Note: the caching (set2d/get2d) obscures the code even more than usual, but is necessary in order to get the second test to run in a reasonable amount of time.


## Maple


```Maple

> StringTools:-LongestCommonSubSequence( "thisisatest", "testing123testing" );
                               "tsitest"

```



## Mathematica

A built-in function can do this for us:

```Mathematica
a = "thisisatest";
b = "testing123testing";
LongestCommonSequence[a, b]
```

gives:

```Mathematica>tsitest</lang

Note that Mathematica also has a built-in function called LongestCommonSubsequence[a,b]:

''finds the longest contiguous subsequence of elements common to the strings or lists a and b.''

which would give "test" as the result for LongestCommonSubsequence[a, b].

The description for LongestCommonSequence[a,b] is:

''finds the longest sequence of contiguous or disjoint elements common to the strings or lists a and b.''

I added this note because the name of this article suggests LongestCommonSubsequence does the job, however LongestCommonSubsequence performs the puzzle-description.


## Nim


### Recursion

```nim
proc lcs(x, y): string =
  if x == "" or y == "":
    return ""

  if x[0] == y[0]:
    return x[0] & lcs(x[1..x.high], y[1..y.high])

  let a = lcs(x, y[1..y.high])
  let b = lcs(x[1..x.high], y)
  result = if a.len > b.len: a else: b

echo lcs("1234", "1224533324")
echo lcs("thisisatest", "testing123testing")
```



### Dynamic Programming

```nim
proc lcs(a, b): string =
  var ls = newSeq[seq[int]] a.len+1
  for i in 0 .. a.len:
    ls[i].newSeq b.len+1

  for i, x in a:
    for j, y in b:
      if x == y:
        ls[i+1][j+1] = ls[i][j] + 1
      else:
        ls[i+1][j+1] = max(ls[i+1][j], ls[i][j+1])

  result = ""
  var x = a.len
  var y = b.len
  while x > 0 and y > 0:
    if ls[x][y] == ls[x-1][y]:
      dec x
    elif ls[x][y] == ls[x][y-1]:
      dec y
    else:
      assert a[x-1] == b[y-1]
      result = a[x-1] & result
      dec x
      dec y

echo lcs("1234", "1224533324")
echo lcs("thisisatest", "testing123testing")
```



## OCaml


### Recursion

from Haskell

```ocaml>let longest xs ys = if List.length xs
 List.length ys then xs else ys

let rec lcs a b = match a, b with
   [], _
 | _, []        -> []
 | x::xs, y::ys ->
    if x = y then
      x :: lcs xs ys
    else
      longest (lcs a ys) (lcs xs b)
```



### Memoized recursion


```ocaml

let lcs xs ys =
  let cache = Hashtbl.create 16 in
  let rec lcs xs ys =
    try Hashtbl.find cache (xs, ys) with
    | Not_found ->
        let result =
          match xs, ys with
          | [], _ -> []
          | _, [] -> []
          | x :: xs, y :: ys when x = y ->
              x :: lcs xs ys
          | _ :: xs_rest, _ :: ys_rest ->
              let a = lcs xs_rest ys in
              let b = lcs xs      ys_rest in
              if (List.length a) > (List.length b) then a else b
        in
        Hashtbl.add cache (xs, ys) result;
        result
  in
  lcs xs ys
```



### Dynamic programming


```ocaml
let lcs xs' ys' =
  let xs = Array.of_list xs'
  and ys = Array.of_list ys' in
  let n = Array.length xs
  and m = Array.length ys in
  let a = Array.make_matrix (n+1) (m+1) [] in
  for i = n-1 downto 0 do
    for j = m-1 downto 0 do
      a.(i).(j) <- if xs.(i) = ys.(j) then
                     xs.(i) :: a.(i+1).(j+1)
                   else
                     longest a.(i).(j+1) a.(i+1).(j)
    done
  done;
  a.(0).(0)
```


Because both solutions only work with lists, here are some functions to convert to and from strings:

```ocaml
let list_of_string str =
  let result = ref [] in
  String.iter (fun x -> result := x :: !result)
              str;
  List.rev !result

let string_of_list lst =
  let result = String.create (List.length lst) in
  ignore (List.fold_left (fun i x -> result.[i] <- x; i+1) 0 lst);
  result
```


Both solutions work. Example:

```txt

# string_of_list (lcs (list_of_string "thisisatest")
                      (list_of_string "testing123testing"));;
- : string = "tsitest"

```



## Oz

Recursive solution:

```oz
declare
  fun {LCS Xs Ys}
     case [Xs Ys]
     of [nil _]                   then nil
     [] [_ nil]                   then nil
     [] [X|Xr  Y|Yr] andthen X==Y then X|{LCS Xr Yr}
     [] [_|Xr  _|Yr]              then {Longest {LCS Xs Yr} {LCS Xr Ys}}
     end
  end

  fun {Longest Xs Ys}
     if {Length Xs} > {Length Ys} then Xs else Ys end
  end
in
  {System.showInfo {LCS "thisisatest" "testing123testing"}}
```



## Pascal

```pascal
Program LongestCommonSubsequence(output);

function lcs(a, b: string): string;
  var
    x, y: string;
    lenga, lengb: integer;
  begin
    lenga := length(a);
    lengb := length(b);
    lcs := '';
    if (lenga >  0) and (lengb >  0) then
      if a[lenga] =  b[lengb] then
        lcs := lcs(copy(a, 1, lenga-1), copy(b, 1, lengb-1)) + a[lenga]
      else
      begin
        x := lcs(a, copy(b, 1, lengb-1));
        y := lcs(copy(a, 1, lenga-1), b);
        if length(x) > length(y) then
          lcs := x
        else
          lcs := y;
      end;
  end;

var
  s1, s2: string;
begin
  s1 := 'thisisatest';
  s2 := 'testing123testing';
  writeln (lcs(s1, s2));
  s1 := '1234';
  s2 := '1224533324';
  writeln (lcs(s1, s2));
end.
```

```txt
:> ./LongestCommonSequence
tsitest
1234

```



## Perl


```perl
sub lcs {
    my ($a, $b) = @_;
    if (!length($a) || !length($b)) {
        return "";
    }
    if (substr($a, 0, 1) eq substr($b, 0, 1)) {
        return substr($a, 0, 1) . lcs(substr($a, 1), substr($b, 1));
    }
    my $c = lcs(substr($a, 1), $b) || "";
    my $d = lcs($a, substr($b, 1)) || "";
    return length($c) > length($d) ? $c : $d;
}

print lcs("thisisatest", "testing123testing") . "\n";
```



## Perl 6


### Recursion

This solution is similar to the Haskell one. It is slow.

```perl6
say lcs("thisisatest", "testing123testing");sub lcs(Str $xstr, Str $ystr) {
    return "" unless $xstr && $ystr;

    my ($x, $xs, $y, $ys) = $xstr.substr(0, 1), $xstr.substr(1), $ystr.substr(0, 1), $ystr.substr(1);
    return $x eq $y
        ?? $x ~ lcs($xs, $ys)
        !! max(:by{ $^a.chars }, lcs($xstr, $ys), lcs($xs, $ystr) );
}

say lcs("thisisatest", "testing123testing");
```



### Dynamic Programming

```perl6

sub lcs(Str $xstr, Str $ystr) {
    my ($xlen, $ylen) = ($xstr, $ystr)>>.chars;
    my @lengths = map {[(0) xx ($ylen+1)]}, 0..$xlen;

    for $xstr.comb.kv -> $i, $x {
        for $ystr.comb.kv -> $j, $y {
            @lengths[$i+1][$j+1] = $x eq $y ?? @lengths[$i][$j]+1 !! (@lengths[$i+1][$j], @lengths[$i][$j+1]).max;
        }
    }

    my @x = $xstr.comb;
    my ($x, $y) = ($xlen, $ylen);
    my $result = "";
    while $x != 0 && $y != 0 {
        if @lengths[$x][$y] == @lengths[$x-1][$y] {
            $x--;
        }
        elsif @lengths[$x][$y] == @lengths[$x][$y-1] {
            $y--;
        }
        else {
            $result = @x[$x-1] ~ $result;
            $x--;
            $y--;
        }
    }

    return $result;
}

say lcs("thisisatest", "testing123testing");
```



### Bit Vector

Bit parallel dynamic programming with nearly linear complexity O(n). It is fast.

```perl6
sub lcs(Str $xstr, Str $ystr) {
    my ($a,$b) = ([$xstr.comb],[$ystr.comb]);

    my $positions;
    for $a.kv -> $i,$x { $positions{$x} +|= 1 +< $i };

    my $S = +^0;
    my $Vs = [];
    my ($y,$u);
    for (0..+$b-1) -> $j {
        $y = $positions{$b[$j]} // 0;
        $u = $S +& $y;
        $S = ($S + $u) +| ($S - $u);
        $Vs[$j] = $S;
    }

    my ($i,$j) = (+$a-1, +$b-1);
    my $result = "";
    while ($i >= 0 && $j >= 0) {
        if ($Vs[$j] +& (1 +< $i)) { $i-- }
        else {
            unless ($j && +^$Vs[$j-1] +& (1 +< $i)) {
                $result = $a[$i] ~ $result;
                $i--;
            }
            $j--;
        }
    }
    return $result;
}

say lcs("thisisatest", "testing123testing");
```



## Phix

If you want this to work with (utf8) Unicode text, just chuck the inputs through utf8_to_utf32() first (and the output through utf32_to_utf8()).

```Phix
function lcs(sequence a, b)
sequence res = ""
    if length(a) and length(b) then
        if a[$]=b[$] then
            res = lcs(a[1..-2],b[1..-2])&a[$]
        else
            sequence l = lcs(a,b[1..-2]),
                     r = lcs(a[1..-2],b)
            res = iff(length(l)>length(r)?l:r)
        end if
    end if
    return res
end function

constant tests = {{"1234","1224533324"},
                  {"thisisatest","testing123testing"}}
for i=1 to length(tests) do
    string {a,b} = tests[i]
    ?lcs(a,b)
end for
```

```txt

"1234"
"tsitest"

```


### Alternate version

same output

```Phix
function LCSLength(sequence X, sequence Y)
sequence C = repeat(repeat(0,length(Y)+1),length(X)+1)
    for i=1 to length(X) do
        for j=1 to length(Y) do
            if X[i]=Y[j] then
                C[i+1][j+1] := C[i][j]+1
            else
                C[i+1][j+1] := max(C[i+1][j], C[i][j+1])
            end if
        end for
    end for
    return C
end function

function backtrack(sequence C, sequence X, sequence Y, integer i, integer j)
    if i=0 or j=0 then
        return ""
    elsif X[i]=Y[j] then
        return backtrack(C, X, Y, i-1, j-1) & X[i]
    else
        if C[i+1][j]>C[i][j+1] then
            return backtrack(C, X, Y, i, j-1)
        else
            return backtrack(C, X, Y, i-1, j)
        end if
    end if
end function

function lcs(sequence a, sequence b)
    return backtrack(LCSLength(a,b),a,b,length(a),length(b))
end function

constant tests = {{"1234","1224533324"},
                  {"thisisatest","testing123testing"}}
for i=1 to length(tests) do
    string {a,b} = tests[i]
    ?lcs(a,b)
end for
```



## PicoLisp


```PicoLisp
(de commonSequences (A B)
   (when A
      (conc
         (when (member (car A) B)
            (mapcar '((L) (cons (car A) L))
               (cons NIL (commonSequences (cdr A) (cdr @))) ) )
         (commonSequences (cdr A) B) ) ) )

(maxi length
   (commonSequences
      (chop "thisisatest")
      (chop "testing123testing") ) )
```

```txt
-> ("t" "s" "i" "t" "e" "s" "t")
```



## PowerShell

Returns a sequence (array) of a type:

```PowerShell

function Get-Lcs ($ReferenceObject, $DifferenceObject)
{
    $longestCommonSubsequence = @()
    $x = $ReferenceObject.Length
    $y = $DifferenceObject.Length

    $lengths = New-Object -TypeName 'System.Object[,]' -ArgumentList ($x + 1), ($y + 1)

    for($i = 0; $i -lt $x; $i++)
    {
        for ($j = 0; $j -lt $y; $j++)
        {
            if ($ReferenceObject[$i] -ceq $DifferenceObject[$j])
            {
                $lengths[($i+1),($j+1)] = $lengths[$i,$j] + 1
            }
            else
            {
                $lengths[($i+1),($j+1)] = [Math]::Max(($lengths[($i+1),$j]),($lengths[$i,($j+1)]))
            }
        }
    }

    while (($x -ne 0) -and ($y -ne 0))
    {
        if ( $lengths[$x,$y] -eq $lengths[($x-1),$y])
        {
            --$x
        }
        elseif ($lengths[$x,$y] -eq $lengths[$x,($y-1)])
        {
            --$y
        }
        else
        {
            if ($ReferenceObject[($x-1)] -ceq $DifferenceObject[($y-1)])
            {
                $longestCommonSubsequence = ,($ReferenceObject[($x-1)]) + $longestCommonSubsequence
            }

            --$x
            --$y
        }
    }

    $longestCommonSubsequence
}

```

Returns the character array as a string:

```PowerShell

(Get-Lcs -ReferenceObject "thisisatest" -DifferenceObject "testing123testing") -join ""

```

```txt

tsitest

```

Returns an array of integers:

```PowerShell

Get-Lcs -ReferenceObject @(1,2,3,4) -DifferenceObject @(1,2,2,4,5,3,3,3,2,4)

```

```txt

1
2
3
4

```

Given two lists of objects, return the LCS of the ID property:

```PowerShell

$list1

ID   X   Y
--   -   -
 1 101 201
 2 102 202
 3 103 203
 4 104 204
 5 105 205
 6 106 206
 7 107 207
 8 108 208
 9 109 209

$list2

ID   X   Y
--   -   -
 1 101 201
 3 103 203
 5 105 205
 7 107 207
 9 109 209

Get-Lcs -ReferenceObject $list1.ID -DifferenceObject $list2.ID

```

```txt

1
3
5
7
9

```



## Prolog


### Recursive Version

First version:

```Prolog
test :-
    time(lcs("thisisatest", "testing123testing", Lcs)),
    writef('%s',[Lcs]).


lcs([ H|L1],[ H|L2],[H|Lcs]) :- !,
    lcs(L1,L2,Lcs).

lcs([H1|L1],[H2|L2],Lcs):-
    lcs(    L1 ,[H2|L2],Lcs1),
    lcs([H1|L1],    L2 ,Lcs2),
    longest(Lcs1,Lcs2,Lcs),!.

lcs(_,_,[]).


longest(L1,L2,Longest) :-
    length(L1,Length1),
    length(L2,Length2),
    ((Length1 > Length2) -> Longest = L1; Longest = L2).
```

Second version, with memoization:

```Prolog
%declare that we will add lcs_db facts during runtime
:- dynamic lcs_db/3.

test :-
    retractall(lcs_db(_,_,_)), %clear the database of known results
    time(lcs("thisisatest", "testing123testing", Lcs)),
    writef('%s',[Lcs]).


% check if the result is known
lcs(L1,L2,Lcs) :-
    lcs_db(L1,L2,Lcs),!.

lcs([ H|L1],[ H|L2],[H|Lcs]) :- !,
    lcs(L1,L2,Lcs).

lcs([H1|L1],[H2|L2],Lcs) :-
    lcs(    L1 ,[H2|L2],Lcs1),
    lcs([H1|L1],    L2 ,Lcs2),
    longest(Lcs1,Lcs2,Lcs),!,
    assert(lcs_db([H1|L1],[H2|L2],Lcs)).

lcs(_,_,[]).


longest(L1,L2,Longest) :-
    length(L1,Length1),
    length(L2,Length2),
    ((Length1 > Length2) -> Longest = L1; Longest = L2).
```

Example for "beginning-middle-ending" and "beginning-diddle-dum-ending" <BR>
First version :

```Prolog
?- time(lcs("beginning-middle-ending","beginning-diddle-dum-ending", Lcs)),writef('%s', [Lcs]).
% 10,875,184 inferences, 1.840 CPU in 1.996 seconds (92% CPU, 5910426 Lips)
beginning-iddle-ending
```

Second version which is much faster :

```Prolog
?- time(lcs("beginning-middle-ending","beginning-diddle-dum-ending", Lcs)),writef('%s', [Lcs]).
% 2,376 inferences, 0.010 CPU in 0.003 seconds (300% CPU, 237600 Lips)
beginning-iddle-ending
```



## PureBasic

```PureBasic
Procedure.s lcs(a$, b$)
  Protected x$ , lcs$
  If Len(a$) = 0 Or Len(b$) = 0
    lcs$ = ""
  ElseIf Right(a$, 1) = Right(b$, 1)
    lcs$ = lcs(Left(a$, Len(a$) - 1), Left(b$, Len(b$) - 1)) + Right(a$, 1)
  Else
    x$ = lcs(a$, Left(b$, Len(b$) - 1))
    y$ = lcs(Left(a$, Len(a$) - 1), b$)
    If Len(x$) > Len(y$)
      lcs$ = x$
    Else
      lcs$ = y$
    EndIf
  EndIf
  ProcedureReturn lcs$
EndProcedure
OpenConsole()
PrintN( lcs("thisisatest", "testing123testing"))
PrintN("Press any key to exit"): Repeat: Until Inkey() <> ""
```



## Python

The simplest way is to use [http://mlpy.sourceforge.net/docs/3.5/lcs.html LCS within mlpy package]


### Recursion

This solution is similar to the Haskell one. It is slow.

```python
def lcs(xstr, ystr):
    """
    >>> lcs('thisisatest', 'testing123testing')
    'tsitest'
    """
    if not xstr or not ystr:
        return ""
    x, xs, y, ys = xstr[0], xstr[1:], ystr[0], ystr[1:]
    if x == y:
        return x + lcs(xs, ys)
    else:
        return max(lcs(xstr, ys), lcs(xs, ystr), key=len)
```

Test it:

```python
if __name__=="__main__":
    import doctest; doctest.testmod()
```



### Dynamic Programming


```python
def lcs(a, b):
    # generate matrix of length of longest common subsequence for substrings of both words
    lengths = [[0] * (len(b)+1) for _ in range(len(a)+1)]
    for i, x in enumerate(a):
        for j, y in enumerate(b):
            if x == y:
                lengths[i+1][j+1] = lengths[i][j] + 1
            else:
                lengths[i+1][j+1] = max(lengths[i+1][j], lengths[i][j+1])

    # read a substring from the matrix
    result = ''
    j = len(b)
    for i in range(1, len(a)+1):
        if lengths[i][j] != lengths[i-1][j]:
            result += a[i-1]

    return result
```



## Racket


```racket
#lang racket
(define (longest xs ys)
  (if (> (length xs) (length ys))
      xs ys))

(define memo (make-hash))
(define (lookup xs ys)
  (hash-ref memo (cons xs ys) #f))
(define (store xs ys r)
  (hash-set! memo (cons xs ys) r)
  r)

(define (lcs/list sx sy)
  (or (lookup sx sy)
      (store sx sy
             (match* (sx sy)
               [((cons x xs) (cons y ys))
                (if (equal? x y)
                    (cons x (lcs/list xs ys))
                    (longest (lcs/list sx ys) (lcs/list xs sy)))]
               [(_ _) '()]))))

(define (lcs sx sy)
  (list->string (lcs/list (string->list sx) (string->list sy))))

(lcs "thisisatest" "testing123testing")
```

```txt
"tsitest">
```



## REXX


```rexx
/*REXX program to test the  LCS (Longest Common Subsequence) subroutine.*/
parse arg aaa bbb .                    /*get two arguments (strings).   */
say 'string A = 'aaa                   /*echo string  A  to screen.     */
say 'string B = 'bbb                   /*echo string  B  to screen.     */
say '     LCS = 'lcs(aaa,bbb)          /*tell Longest Common Sequence.  */
exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────LCS subroutine──────────────────────*/
lcs: procedure; parse arg a,b,z        /*Longest Common Subsequence.    */
                                       /*reduce recursions, removes the */
                                       /*chars in A ¬ in B, & vice-versa*/
if z=='' then return lcs( lcs(a,b,0), lcs(b,a,0), 9)
j=length(a)
if z==0 then do                        /*special invocation:  shrink Z. */
                                  do j=1  for j;   _=substr(a,j,1)
                                  if pos(_,b)\==0  then z=z||_
                                  end   /*j*/
             return substr(z,2)
             end
k=length(b)
if j==0 | k==0  then return ''         /*Either string null?    Bupkis. */
_=substr(a,j,1)
if _==substr(b,k,1)  then return lcs(substr(a,1,j-1),substr(b,1,k-1),9)_
x=lcs(a,substr(b,1,k-1),9)
y=lcs(substr(a,1,j-1),b,9)
if length(x)>length(y)  then return x
                             return y
```

```txt

string A=1234
string B=1224533324
     LCS=1234

```

```txt

string A=thisisatest
string B=testing123testing
     LCS=tsitest

```



## Ring


```ring

see longest("1267834", "1224533324") + nl

func longest a, b
     if a = "" or b = "" return "" ok
     if right(a, 1) = right(b, 1)
        lcs = longest(left(a, len(a) - 1), left(b, len(b) - 1)) + right(a, 1)
        return lcs
     else
        x1 = longest(a, left(b, len(b) - 1))
        x2 = longest(left(a, len(a) - 1), b)
        if len(x1) > len(x2)
           lcs = x1
           return lcs
        else
           lcs = x2
           return lcs ok ok

```

Output:

```txt

1234

```



## Ruby


### Recursion

This solution is similar to the Haskell one. It is slow (The time complexity is exponential.)
```ruby
=begin
irb(main):001:0> lcs('thisisatest', 'testing123testing')
=> "tsitest"
=end
def lcs(xstr, ystr)
  return "" if xstr.empty? || ystr.empty?

  x, xs, y, ys = xstr[0..0], xstr[1..-1], ystr[0..0], ystr[1..-1]
  if x == y
    x + lcs(xs, ys)
  else
    [lcs(xstr, ys), lcs(xs, ystr)].max_by {|x| x.size}
  end
end
```



### Dynamic programming

Walker class for the LCS matrix:


```ruby
class LCS
  SELF, LEFT, UP, DIAG = [0,0], [0,-1], [-1,0], [-1,-1]

  def initialize(a, b)
    @m = Array.new(a.length) { Array.new(b.length) }
    a.each_char.with_index do |x, i|
      b.each_char.with_index do |y, j|
        match(x, y, i, j)
      end
    end
  end

  def match(c, d, i, j)
    @i, @j = i, j
    @m[i][j] = compute_entry(c, d)
  end

  def lookup(x, y)        [@i+x, @j+y]                      end
  def valid?(i=@i, j=@j)  i >= 0 && j >= 0                  end

  def peek(x, y)
    i, j = lookup(x, y)
    valid?(i, j) ? @m[i][j] : 0
  end

  def compute_entry(c, d)
    c == d ? peek(*DIAG) + 1 : [peek(*LEFT), peek(*UP)].max
  end

  def backtrack
    @i, @j = @m.length-1, @m[0].length-1
    y = []
    y << @i+1 if backstep? while valid?
    y.reverse
  end

  def backtrack2
    @i, @j = @m.length-1, @m[0].length-1
    y = []
    y << @j+1 if backstep? while valid?
    [backtrack, y.reverse]
  end

  def backstep?
    backstep = compute_backstep
    @i, @j = lookup(*backstep)
    backstep == DIAG
  end

  def compute_backstep
    case peek(*SELF)
    when peek(*LEFT) then LEFT
    when peek(*UP)   then UP
    else                  DIAG
    end
  end
end

def lcs(a, b)
  walker = LCS.new(a, b)
  walker.backtrack.map{|i| a[i]}.join
end

if $0 == __FILE__
  puts lcs('thisisatest', 'testing123testing')
  puts lcs("rosettacode", "raisethysword")
end
```


```txt

tsitest
rsetod

```

Referring to LCS [[Levenshtein distance/Alignment#Ruby|here]] and [[Shortest common supersequence#Ruby|here]].


## Run BASIC


```runbasic
a$	= "aebdaef"
b$	= "cacbac"
print lcs$(a$,b$)
end

FUNCTION lcs$(a$, b$)
IF a$ = "" OR b$ = "" THEN
  lcs$ = ""
  goto [ext]
end if

IF RIGHT$(a$, 1) = RIGHT$(b$, 1) THEN
  lcs$ = lcs$(LEFT$(a$, LEN(a$) - 1), LEFT$(b$, LEN(b$) - 1)) + RIGHT$(a$, 1)
  goto [ext]
 ELSE
  x1$ = lcs$(a$, LEFT$(b$, LEN(b$) - 1))
  x2$ = lcs$(LEFT$(a$, LEN(a$) - 1), b$)
  IF LEN(x1$) > LEN(x2$) THEN
    lcs$ = x1$
    goto [ext]
   ELSE
    lcs$ = x2$
    goto [ext]
  END IF
END IF
[ext]
END FUNCTION
```

```txt
aba
```



## Rust

Dynamic programming version:

```rust

use std::cmp;

fn lcs(string1: String, string2: String) -> (usize, String){
    let total_rows = string1.len() + 1;
    let total_columns = string2.len() + 1;
    // rust doesn't allow accessing string by index
    let string1_chars = string1.as_bytes();
    let string2_chars = string2.as_bytes();

    let mut table = vec![vec![0; total_columns]; total_rows];

    for row in 1..total_rows{
        for col in 1..total_columns {
            if string1_chars[row - 1] == string2_chars[col - 1]{
                table[row][col] = table[row - 1][col - 1] + 1;
            } else {
                table[row][col] = cmp::max(table[row][col-1], table[row-1][col]);
            }
        }
    }

    let mut common_seq = Vec::new();
    let mut x = total_rows - 1;
    let mut y = total_columns - 1;

    while x != 0 && y != 0 {
        // Check element above is equal
        if table[x][y] == table[x - 1][y] {
            x = x - 1;
        }
        // check element to the left is equal
        else if table[x][y] == table[x][y - 1] {
            y = y - 1;
        }
        else {
            // check the two element at the respective x,y position is same
            assert_eq!(string1_chars[x-1], string2_chars[y-1]);
            let char = string1_chars[x - 1];
            common_seq.push(char);
            x = x - 1;
            y = y - 1;
        }
    }
    common_seq.reverse();
    (table[total_rows - 1][total_columns - 1], String::from_utf8(common_seq).unwrap())
}

fn main() {
    let res = lcs("abcdaf".to_string(), "acbcf".to_string());
    assert_eq!((4 as usize, "abcf".to_string()), res);
    let res = lcs("thisisatest".to_string(), "testing123testing".to_string());
    assert_eq!((7 as usize, "tsitest".to_string()), res);
    // LCS for input Sequences “AGGTAB” and “GXTXAYB” is “GTAB” of length 4.
    let res = lcs("AGGTAB".to_string(), "GXTXAYB".to_string());
    assert_eq!((4 as usize, "GTAB".to_string()), res);
}
```



## Scala

Using lazily evaluated lists:

```scala
  def lcsLazy[T](u: IndexedSeq[T], v: IndexedSeq[T]): IndexedSeq[T] = {
    def su = subsets(u).to(LazyList)
    def sv = subsets(v).to(LazyList)
    su.intersect(sv).headOption match{
      case Some(sub) => sub
      case None => IndexedSeq[T]()
    }
  }

  def subsets[T](u: IndexedSeq[T]): Iterator[IndexedSeq[T]] = {
    u.indices.reverseIterator.flatMap{n => u.indices.combinations(n + 1).map(_.map(u))}
  }
```


Using recursion:

```scala
  def lcsRec[T]: (IndexedSeq[T], IndexedSeq[T]) => IndexedSeq[T] = {
    case (a +: as, b +: bs) if a == b => a +: lcsRec(as, bs)
    case (as, bs) if as.isEmpty || bs.isEmpty => IndexedSeq[T]()
    case (a +: as, b +: bs) =>
      val (s1, s2) = (lcsRec(a +: as, bs), lcsRec(as, b +: bs))
      if(s1.length > s2.length) s1 else s2
  }
```


```txt
scala> lcsLazy("thisisatest", "testing123testing").mkString
res0: String = tsitest

scala> lcsRec("thisisatest", "testing123testing").mkString
res1: String = tsitest
```

Recursive version:

```scala
  def lcs[T]: (List[T], List[T]) => List[T] = {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (x :: xs, y :: ys) if x == y => x :: lcs(xs, ys)
    case (x :: xs, y :: ys)           => {
      (lcs(x :: xs, ys), lcs(xs, y :: ys)) match {
        case (xs, ys) if xs.length > ys.length => xs
        case (xs, ys)                          => ys
      }
    }
  }
```


The dynamic programming version:


```scala
  case class Memoized[A1, A2, B](f: (A1, A2) => B) extends ((A1, A2) => B) {
    val cache = scala.collection.mutable.Map.empty[(A1, A2), B]
    def apply(x: A1, y: A2) = cache.getOrElseUpdate((x, y), f(x, y))
  }

  lazy val lcsM: Memoized[List[Char], List[Char], List[Char]] = Memoized {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (x :: xs, y :: ys) if x == y => x :: lcsM(xs, ys)
    case (x :: xs, y :: ys)           => {
      (lcsM(x :: xs, ys), lcsM(xs, y :: ys)) match {
        case (xs, ys) if xs.length > ys.length => xs
        case (xs, ys)                          => ys
      }
    }
  }
```


  scala> lcsM("thisiaatest".toList, "testing123testing".toList).mkString
  res0: String = tsitest


## Scheme


Port from Clojure.


```scheme

;; using srfi-69
(define (memoize proc)
  (let ((results (make-hash-table)))
    (lambda args
      (or (hash-table-ref results args (lambda () #f))
          (let ((r (apply proc args)))
            (hash-table-set! results args r)
            r)))))

(define (longest xs ys)
  (if (> (length xs)
         (length ys))
      xs ys))

(define lcs
  (memoize
   (lambda (seqx seqy)
     (if (pair? seqx)
         (let ((x (car seqx))
               (xs (cdr seqx)))
           (if (pair? seqy)
               (let ((y (car seqy))
                     (ys (cdr seqy)))
                 (if (equal? x y)
                     (cons x (lcs xs ys))
                     (longest (lcs seqx ys)
                              (lcs xs seqy))))
               '()))
         '()))))

```


Testing:

```scheme


(test-group
 "lcs"
 (test '()  (lcs '(a b c) '(A B C)))
 (test '(a) (lcs '(a a a) '(A A a)))
 (test '()  (lcs '() '(a b c)))
 (test '()  (lcs '(a b c) '()))
 (test '(a c) (lcs '(a b c) '(a B c)))
 (test '(b) (lcs '(a b c) '(A b C)))

 (test     '(  b   d e f     g h   j)
      (lcs '(a b   d e f     g h i j)
           '(A b c d e f F a g h   j))))

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func string: lcs (in string: a, in string: b) is func
  result
    var string: lcs is "";
  local
    var string: x is "";
    var string: y is "";
  begin
    if a <> "" and b <> "" then
      if a[length(a)] = b[length(b)] then
        lcs := lcs(a[.. pred(length(a))], b[.. pred(length(b))]) & str(a[length(a)]);
      else
        x := lcs(a, b[.. pred(length(b))]);
        y := lcs(a[.. pred(length(a))], b);
        if length(x) > length(y) then
          lcs := x;
        else
          lcs := y;
        end if;
      end if;
    end if;
  end func;

const proc: main is func
  begin
    writeln(lcs("thisisatest", "testing123testing"));
    writeln(lcs("1234", "1224533324"));
  end func;
```


Output:

```txt

tsitest
1234

```



## SequenceL

It is interesting to note that x and y are computed in parallel, dividing work across threads repeatedly down through the recursion.


```sequencel>import <Utilities/Sequence.sl
;

lcsBack(a(1), b(1)) :=
    let
        aSub := allButLast(a);
        bSub := allButLast(b);

        x := lcsBack(a, bSub);
        y := lcsBack(aSub, b);
    in
        [] when size(a) = 0 or size(b) = 0
    else
        lcsBack(aSub, bSub) ++ [last(a)] when last(a) = last(b)
    else
        x when size(x) > size(y)
    else
        y;

main(args(2)) :=
        lcsBack(args[1], args[2]) when size(args) >=2
    else
        lcsBack("thisisatest", "testing123testing");
```


```txt

"tsitest"

```



## SETL

Recursive; Also works on tuples (vectors)

```setl
    op .longest(a, b);
      return if #a > #b then a else b end;
    end .longest;

    procedure lcs(a, b);
      if exists empty in {a, b} | #empty = 0 then
        return empty;
      elseif a(1) = b(1) then
        return a(1) + lcs(a(2..), b(2..));
      else
        return lcs(a(2..), b) .longest lcs(a, b(2..));
      end;
    end lcs;
```



## Sidef


```ruby
func lcs(xstr, ystr) is cached {

    xstr.is_empty && return xstr;
    ystr.is_empty && return ystr;

    var(x, xs, y, ys) = (xstr.ft(0,0), xstr.ft(1),
                         ystr.ft(0,0), ystr.ft(1));

    if (x == y) {
        x + lcs(xs, ys)
    } else {
        [lcs(xstr, ys), lcs(xs, ystr)].max_by { .len };
    }
}

say lcs("thisisatest", "testing123testing");
```

```txt

tsitest

```



## Slate

We define this on the <tt>Sequence</tt> type since there is nothing string-specific about the concept.

### Recursion

```slate
s1@(Sequence traits) longestCommonSubsequenceWith: s2@(Sequence traits)
[
  s1 isEmpty \/ s2 isEmpty ifTrue: [^ {}].
  s1 last = s2 last
    ifTrue: [(s1 allButLast longestCommonSubsequenceWith: s2 allButLast) copyWith: s1 last]
    ifFalse: [| x y |
              x: (s1 longestCommonSubsequenceWith: s2 allButLast).
              y: (s1 allButLast longestCommonSubsequenceWith: s2).
              x length > y length ifTrue: [x] ifFalse: [y]]
].
```


### Dynamic Programming

```slate
s1@(Sequence traits) longestCommonSubsequenceWith: s2@(Sequence traits)
[| lengths |
  lengths: (ArrayMD newWithDimensions: {s1 length `cache. s2 length `cache} defaultElement: 0).
  s1 doWithIndex: [| :elem1 :index1 |
    s2 doWithIndex: [| :elem2 :index2 |
      elem1 = elem2
        ifTrue: [lengths at: {index1 + 1. index2 + 1} put: (lengths at: {index1. index2}) + 1]
        ifFalse: [lengths at: {index1 + 1. index2 + 1} put:
          ((lengths at: {index1 + 1. index2}) max: (lengths at: {index1. index2 + 1}))]]].
  ([| :result index1 index2 |
   index1: s1 length.
   index2: s2 length.
   [index1 isPositive /\ index2 isPositive]
     whileTrue:
       [(lengths at: {index1. index2}) = (lengths at: {index1 - 1. index2})
          ifTrue: [index1: index1 - 1]
          ifFalse: [(lengths at: {index1. index2}) = (lengths at: {index1. index2 - 1})]
            ifTrue: [index2: index2 - 1]
            ifFalse: ["assert: (s1 at: index1 - 1) = (s2 at: index2 - 1)."
                      result nextPut: (s1 at: index1 - 1).
                      index1: index1 - 1.
                      index2: index2 - 1]]
   ] writingAs: s1) reverse
].
```


## Swift

Swift 2.2

### Recursion


```Swift
func rlcs(_ s1: String, _ s2: String) -> String {
        let x = s1.characters.count
        let y = s2.characters.count

        if x == 0 || y == 0 {
            return ""
        } else if s1[s1.index(s1.startIndex, offsetBy: x-1)] == s2[s2.index(s2.startIndex, offsetBy: y-1)] {
            return rlcs(String(s1[s1.startIndex..<s1.index(s1.startIndex, offsetBy: x-1)]),
                        String(s2[s2.startIndex..<s2.index(s2.startIndex, offsetBy: y-1)])) + String(s1[s1.index(s1.startIndex, offsetBy: x-1)])
        } else {
            let xstr = rlcs(s1, String(s2[s2.startIndex..<s2.index(s2.startIndex, offsetBy: y-1)]))
            let ystr = rlcs(String(s1[s1.startIndex..<s1.index(s1.startIndex, offsetBy: x-1)]), s2)

            return xstr.characters.count > ystr.characters.count ? xstr : ystr
        }
    }
```



### Dynamic Programming


```Swift
func lcs(s1:String, _ s2:String) -> String {
    var x = s1.characters.count
    var y = s2.characters.count
    var lens = Array(count: x + 1, repeatedValue:
        Array(count: y + 1, repeatedValue: 0))
    var returnStr = ""

    for i in 0..<x {
        for j in 0..<y {
            if s1[s1.startIndex.advancedBy(i)] == s2[s2.startIndex.advancedBy(j)] {
                lens[i + 1][j + 1] = lens[i][j] + 1
            } else {
                lens[i + 1][j + 1] = max(lens[i + 1][j], lens[i][j + 1])
            }
        }
    }

    while x != 0 && y != 0 {
        if lens[x][y] == lens[x - 1][y] {
            --x
        } else if lens[x][y] == lens[x][y - 1] {
            --y
        } else {
            returnStr += String(s1[s1.startIndex.advancedBy(x - 1)])
            --x
            --y
        }
    }

    return String(returnStr.characters.reverse())
}
```



## Tcl


### Recursive

```tcl
proc r_lcs {a b} {
    if {$a eq "" || $b eq ""} {return ""}
    set a_ [string range $a 1 end]
    set b_ [string range $b 1 end]
    if {[set c [string index $a 0]] eq [string index $b 0]} {
        return "$c[r_lcs $a_ $b_]"
    } else {
        set x [r_lcs $a $b_]
        set y [r_lcs $a_ $b]
        return [expr {[string length $x] > [string length $y] ? $x :$y}]
    }
}
```


### Dynamic

```tcl
package require Tcl 8.5
namespace import ::tcl::mathop::+
namespace import ::tcl::mathop::-
namespace import ::tcl::mathfunc::max

proc d_lcs {a b} {
    set la [string length $a]
    set lb [string length $b]
    set lengths [lrepeat [+ $la 1] [lrepeat [+ $lb 1] 0]]

    for {set i 0} {$i < $la} {incr i} {
        for {set j 0} {$j < $lb} {incr j} {
            if {[string index $a $i] eq [string index $b $j]} {
                lset lengths [+ $i 1] [+ $j 1] [+ [lindex $lengths $i $j] 1]
            } else {
                lset lengths [+ $i 1] [+ $j 1] [max [lindex $lengths [+ $i 1] $j] [lindex $lengths $i [+ $j 1]]]
            }
        }
    }

    set result ""
    set x $la
    set y $lb
    while {$x >0 && $x > 0} {
        if {[lindex $lengths $x $y] == [lindex $lengths [- $x 1] $y]} {
            incr x -1
        } elseif {[lindex $lengths $x $y] == [lindex $lengths $x [- $y 1]]} {
            incr y -1
        } else {
            if {[set c [string index $a [- $x 1]]] ne [string index $b [- $y 1]]} {
                error "assertion failed: a.charAt(x-1) == b.charAt(y-1)"
            }
            append result $c
            incr x -1
            incr y -1
        }
    }
    return [string reverse $result]
}
```


### Performance Comparison


```tcl
% time {d_lcs thisisatest testing123testing} 10
637.5 microseconds per iteration
% time {r_lcs thisisatest testing123testing} 10
1275566.8 microseconds per iteration
```



## Ursala

This uses the same recursive algorithm as in the Haskell example,
and works on lists of any type.

```Ursala
#import std

lcs = ~&alrB^& ~&E?abh/~&alh2fabt2RC @faltPrXlrtPXXPW leql?/~&r ~&l
```

test program:

```Ursala
#cast %s

example = lcs('thisisatest','testing123testing')
```

```txt
'tsitest'
```



## zkl

This is quite vile in terms of [time] efficiency, another algorithm should be used for real work.
```zkl
fcn lcs(a,b){
   if(not a or not b) return("");
   if (a[0]==b[0]) return(a[0] + self.fcn(a[1,*],b[1,*]));
   return(fcn(x,y){if(x.len()>y.len())x else y}(lcs(a,b[1,*]),lcs(a[1,*],b)))
}
```

The last line looks strange but it is just return(lambda longest(lcs.lcs))
```txt

zkl: lcs("thisisatest", "testing123testing")
tsitest

```

