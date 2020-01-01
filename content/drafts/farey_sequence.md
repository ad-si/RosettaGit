+++
title = "Farey sequence"
description = ""
date = 2019-10-21T19:05:19Z
aliases = []
[extra]
id = 17470
[taxonomies]
categories = []
tags = []
+++

{{task}}

The   [[wp:Farey sequence|Farey sequence]]   ''' ''F''<sub>n</sub>'''   of order   '''n'''   is the sequence of completely reduced fractions between   '''0'''   and   '''1'''   which, when in lowest terms, have denominators less than or equal to   '''n''',   arranged in order of increasing size.

The   ''Farey sequence''   is sometimes incorrectly called a   ''Farey series''.


Each Farey sequence:
:::*   starts with the value   '''0''',   denoted by the fraction     <math> \frac{0}{1} </math>
:::*   ends with the value   '''1''',   denoted by the fraction   <math> \frac{1}{1}</math>.


The Farey sequences of orders   '''1'''   to   '''5'''   are:

:<math>{\bf\it{F}}_1 = \frac{0}{1}, \frac{1}{1}</math>
:

:<math>{\bf\it{F}}_2 = \frac{0}{1}, \frac{1}{2}, \frac{1}{1}</math>
:

:<math>{\bf\it{F}}_3 = \frac{0}{1}, \frac{1}{3}, \frac{1}{2}, \frac{2}{3}, \frac{1}{1}</math>
:

:<math>{\bf\it{F}}_4 = \frac{0}{1}, \frac{1}{4}, \frac{1}{3}, \frac{1}{2}, \frac{2}{3}, \frac{3}{4}, \frac{1}{1}</math>
:

:<math>{\bf\it{F}}_5 = \frac{0}{1}, \frac{1}{5}, \frac{1}{4}, \frac{1}{3}, \frac{2}{5}, \frac{1}{2}, \frac{3}{5}, \frac{2}{3}, \frac{3}{4}, \frac{4}{5}, \frac{1}{1}</math>

;Task
*   Compute and show the Farey sequence for orders   '''1'''   through   '''11'''   (inclusive).
*   Compute and display the   ''number''   of fractions in the Farey sequence for order   '''100'''   through   '''1,000'''   (inclusive)   by hundreds.
*   Show the fractions as   <big> '''n/d''' </big>   (using the solidus [or slash] to separate the numerator from the denominator).


;See also
*    OEIS sequence    [http://oeis.org/A006842 A006842 numerators   of Farey series of order 1, 2, ···]
*    OEIS sequence    [http://oeis.org/A006843 A006843 denominators of Farey series of order 1, 2, ···]
*    OEIS sequence    [http://oeis.org/A005728 A005728 number of fractions in Farey series of order n.]
*   MathWorld entry   [http://mathworld.wolfram.com/FareySequence.html Farey sequence]





## AWK


```AWK

# syntax: GAWK -f FAREY_SEQUENCE.AWK
BEGIN {
    for (i=1; i<=11; i++) {
      farey(i); printf("\n")
    }
    for (i=100; i<=1000; i+=100) {
      printf(" %d items\n",farey(i))
    }
    exit(0)
}
function farey(n,  a,aa,b,bb,c,cc,d,dd,items,k) {
    a = 0; b = 1; c = 1; d = n
    printf("%d:",n)
    if (n <= 11) {
      printf(" %d/%d",a,b)
    }
    while (c <= n) {
      k = int((n+b)/d)
      aa = c; bb = d; cc = k*c-a; dd = k*d-b
      a = aa; b = bb; c = cc; d = dd
      items++
      if (n <= 11) {
        printf(" %d/%d",a,b)
      }
    }
    return(1+items)
}

```

{{out}}

```txt

1: 0/1 1/1
2: 0/1 1/2 1/1
3: 0/1 1/3 1/2 2/3 1/1
4: 0/1 1/4 1/3 1/2 2/3 3/4 1/1
5: 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
6: 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
7: 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
8: 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
9: 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
10: 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
11: 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1
100: 3045 items
200: 12233 items
300: 27399 items
400: 48679 items
500: 76117 items
600: 109501 items
700: 149019 items
800: 194751 items
900: 246327 items
1000: 304193 items

```



## APL


```APL

farey←{{⍵[⍋⍵]}∪∊{(0,⍳⍵)÷⍵}¨⍳⍵}
fract←{1∧(0(⍵=0)+⊂⍵)*1 ¯1}
print←{{(⍕⍺),'/',(⍕⍵),' '}⌿↑fract farey ⍵}

```

Note that this is a brute-force algorithm, not the sequential one given on Wikipedia.
Basically, given n this one generates and then sorts the set

```txt

{ p/q | p,q integers, 0 <= p <= q, 1 <= q <= n }

```
.

{{out}}

```txt

      {⍵⍪(⊂'¯¯¯¯¯')⍪⍉↑print¨⍵}⍳11    ⍝ Sequences
     1      2      3      4      5      6      7      8      9     10      11
 ¯¯¯¯¯  ¯¯¯¯¯  ¯¯¯¯¯  ¯¯¯¯¯  ¯¯¯¯¯  ¯¯¯¯¯  ¯¯¯¯¯  ¯¯¯¯¯  ¯¯¯¯¯  ¯¯¯¯¯   ¯¯¯¯¯
  0/1    0/1    0/1    0/1    0/1    0/1    0/1    0/1    0/1    0/1     0/1
  1/1    1/2    1/3    1/4    1/5    1/6    1/7    1/8    1/9   1/10    1/11
         1/1    1/2    1/3    1/4    1/5    1/6    1/7    1/8    1/9    1/10
                2/3    1/2    1/3    1/4    1/5    1/6    1/7    1/8     1/9
                1/1    2/3    2/5    1/3    1/4    1/5    1/6    1/7     1/8
                       3/4    1/2    2/5    2/7    1/4    1/5    1/6     1/7
                       1/1    3/5    1/2    1/3    2/7    2/9    1/5     1/6
                              2/3    3/5    2/5    1/3    1/4    2/9    2/11
                              3/4    2/3    3/7    3/8    2/7    1/4     1/5
                              4/5    3/4    1/2    2/5    1/3    2/7     2/9
                              1/1    4/5    4/7    3/7    3/8   3/10     1/4
                                     5/6    3/5    1/2    2/5    1/3    3/11
                                     1/1    2/3    4/7    3/7    3/8     2/7
                                            5/7    3/5    4/9    2/5    3/10
                                            3/4    5/8    1/2    3/7     1/3
                                            4/5    2/3    5/9    4/9    4/11
                                            5/6    5/7    4/7    1/2     3/8
                                            6/7    3/4    3/5    5/9     2/5
                                            1/1    4/5    5/8    4/7     3/7
                                                   5/6    2/3    3/5     4/9
                                                   6/7    5/7    5/8    5/11
                                                   7/8    3/4    2/3     1/2
                                                   1/1    7/9   7/10    6/11
                                                          4/5    5/7     5/9
                                                          5/6    3/4     4/7
                                                          6/7    7/9     3/5
                                                          7/8    4/5     5/8
                                                          8/9    5/6    7/11
                                                          1/1    6/7     2/3
                                                                 7/8    7/10
                                                                 8/9     5/7
                                                                9/10    8/11
                                                                 1/1     3/4
                                                                         7/9
                                                                         4/5
                                                                        9/11
                                                                         5/6
                                                                         6/7
                                                                         7/8
                                                                         8/9
                                                                        9/10
                                                                       10/11
                                                                         1/1

      {⍵,'|',[1.5]≢∘farey¨⍵}100×⍳10    ⍝ Sequence lengths
 100 |   3045
 200 |  12233
 300 |  27399
 400 |  48679
 500 |  76117
 600 | 109501
 700 | 149019
 800 | 194751
 900 | 246327
1000 | 304193

```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void farey(int n)
{
	typedef struct { int d, n; } frac;
	frac f1 = {0, 1}, f2 = {1, n}, t;
	int k;

	printf("%d/%d %d/%d", 0, 1, 1, n);
	while (f2.n > 1) {
		k = (n + f1.n) / f2.n;
		t = f1, f1 = f2, f2 = (frac) { f2.d * k - t.d, f2.n * k - t.n };
		printf(" %d/%d", f2.d, f2.n);
	}

	putchar('\n');
}

typedef unsigned long long ull;
ull *cache;
size_t ccap;

ull farey_len(int n)
{
	if (n >= ccap) {
		size_t old = ccap;
		if (!ccap) ccap = 16;
		while (ccap <= n) ccap *= 2;
		cache = realloc(cache, sizeof(ull) * ccap);
		memset(cache + old, 0, sizeof(ull) * (ccap - old));
	} else if (cache[n])
		return cache[n];

	ull len = (ull)n*(n + 3) / 2;
	int p, q = 0;
	for (p = 2; p <= n; p = q) {
		q = n/(n/p) + 1;
		len -= farey_len(n/p) * (q - p);
	}

	cache[n] = len;
	return len;
}

int main(void)
{
	int n;
	for (n = 1; n <= 11; n++) {
		printf("%d: ", n);
		farey(n);
	}

	for (n = 100; n <= 1000; n += 100)
		printf("%d: %llu items\n", n, farey_len(n));

	n = 10000000;
	printf("\n%d: %llu items\n", n, farey_len(n));
	return 0;
}
```

{{out}}

```txt

1: 0/1 1/1
2: 0/1 1/2 1/1
3: 0/1 1/3 1/2 2/3 1/1
4: 0/1 1/4 1/3 1/2 2/3 3/4 1/1
5: 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
6: 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
7: 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
8: 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
9: 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
10: 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
11: 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1
100: 3045 items
200: 12233 items
300: 27399 items
400: 48679 items
500: 76117 items
600: 109501 items
700: 149019 items
800: 194751 items
900: 246327 items
1000: 304193 items

10000000: 30396356427243 items

```



## C#

{{works with|C sharp|7}}
{{trans|Java}}

```c#
using System;
using System.Collections.Generic;
using System.Linq;

public static class FareySequence
{
    public static void Main() {
        for (int i = 1; i <= 11; i++) {
            Console.WriteLine($"F{i}: " + string.Join(", ", Generate(i).Select(f => $"{f.num}/{f.den}")));
        }
        for (int i = 100; i <= 1000; i+=100) {
            Console.WriteLine($"F{i} has {Generate(i).Count()} terms.");
        }
    }

    public static IEnumerable<(int num, int den)> Generate(int i) {
        var comparer = Comparer<(int n, int d)>.Create((a, b) => (a.n * b.d).CompareTo(a.d * b.n));
        var seq = new SortedSet<(int n, int d)>(comparer);
        for (int d = 1; d <= i; d++) {
            for (int n = 0; n <= d; n++) {
                seq.Add((n, d));
            }
        }
        return seq;
    }
}
```

{{out}}
<pre style="height:30ex;overflow:scroll">
F1: 0/1, 1/1
F2: 0/1, 1/2, 1/1
F3: 0/1, 1/3, 1/2, 2/3, 1/1
F4: 0/1, 1/4, 1/3, 1/2, 2/3, 3/4, 1/1
F5: 0/1, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 1/1
F6: 0/1, 1/6, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 5/6, 1/1
F7: 0/1, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 2/5, 3/7, 1/2, 4/7, 3/5, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 1/1
F8: 0/1, 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8, 1/1
F9: 0/1, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 1/1
F10: 0/1, 1/10, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 3/10, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 7/10, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 9/10, 1/1
F11: 0/1, 1/11, 1/10, 1/9, 1/8, 1/7, 1/6, 2/11, 1/5, 2/9, 1/4, 3/11, 2/7, 3/10, 1/3, 4/11, 3/8, 2/5, 3/7, 4/9, 5/11, 1/2, 6/11, 5/9, 4/7, 3/5, 5/8, 7/11, 2/3, 7/10, 5/7, 8/11, 3/4, 7/9, 4/5, 9/11, 5/6, 6/7, 7/8, 8/9, 9/10, 10/11, 1/1
F100 has 3045 terms.
F200 has 12233 terms.
F300 has 27399 terms.
F400 has 48679 terms.
F500 has 76117 terms.
F600 has 109501 terms.
F700 has 149019 terms.
F800 has 194751 terms.
F900 has 246327 terms.
F1000 has 304193 terms.

```



## Common Lisp


{{improve|Common Lisp|

 The output for the first and last term   (as per the task's requirement)

 is to show the first term as   <big>'''0/1'''</big>,

 and to show the last term as   <big>'''1/1'''</big>.

 }}

The common lisp version of the code is taken from the scala version with some modifications:

```lisp
(defun farey (n)
  (labels ((helper (begin end)
	     (let ((med (/ (+ (numerator begin) (numerator end))
			   (+ (denominator begin) (denominator end)))))
	       (if (<= (denominator med) n)
		   (append (helper begin med)
			   (list med)
			   (helper med end))))))
      (append (list 0) (helper 0 1) (list 1))))


(loop for i from 1 to 11 do
     (format t "~a: ~{~a ~}~%" i (farey i)))

(loop for i from 100 to 1001 by 100 do
     (format t "Farey sequence of order ~a has ~a terms.~%" i (length (farey i))))

```

{{out}}

```txt
1: 0 1
2: 0 1/2 1
3: 0 1/3 1/2 2/3 1
4: 0 1/4 1/3 1/2 2/3 3/4 1
5: 0 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1
6: 0 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1
7: 0 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1
8: 0 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1
9: 0 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1
10: 0 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1
11: 0 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1

NIL

Farey sequence of order 100 has 3045 terms.
Farey sequence of order 200 has 12233 terms.
Farey sequence of order 300 has 27399 terms.
Farey sequence of order 400 has 48679 terms.
Farey sequence of order 500 has 76117 terms.
Farey sequence of order 600 has 109501 terms.
Farey sequence of order 700 has 149019 terms.
Farey sequence of order 800 has 194751 terms.
Farey sequence of order 900 has 246327 terms.
Farey sequence of order 1000 has 304193 terms.

NIL

```

<math>Insert formula here</math>


## Crystal



{{incorrect|Crystal|

 The output for the first and last term   (as per the task's requirement)

 is to show the first term as   <big>'''0/1'''</big>,

 and to show the last term as   <big>'''1/1'''</big>.

 Also, the first term is missing.

 }}


{{trans|the function from Lua, the output from Ruby.}}

```ruby
require "big"

def farey (n)
    a, b, c, d = 0, 1, 1, n
    fracs = [] of BigRational
    while c <= n
        k = (n + b) / d
        a, b, c, d = c, d, k * c - a, k * d - b
        fracs << BigRational.new(a,b)
    end
    fracs.uniq.sort
end

puts "Farey sequence for order 1 through 11 (inclusive):"
(1..11).each do |n|
  puts "F(#{n}): " + farey(n).join(", ")
end

puts "Number of fractions in the Farey sequence:"
(100..1000).step(100) do |i|
  puts "F(%4d) =%7d" % [i, farey(i).size]
end

```

{{out}}

```txt

F(1): 1
F(2): 1/2, 1
F(3): 1/3, 1/2, 2/3, 1
F(4): 1/4, 1/3, 1/2, 2/3, 3/4, 1
F(5): 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 1
F(6): 1/6, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 5/6, 1
F(7): 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 2/5, 3/7, 1/2, 4/7, 3/5, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 1
F(8): 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8, 1
F(9): 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 7/9, 4/5, 5/6, 7/8, 8/9, 1
F(10): 1/10, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 3/10, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 7/10, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 9/10, 1
F(11): 1/11, 1/10, 1/9, 1/8, 1/7, 1/6, 2/11, 1/5, 2/9, 1/4, 3/11, 2/7, 3/10, 1/3, 4/11, 3/8, 2/5, 3/7, 4/9, 5/11, 1/2, 6/11, 5/9, 4/7, 3/5, 5/8, 7/11, 2/3, 7/10, 5/7, 8/11, 3/4, 7/9, 4/5, 9/11, 5/6, 6/7, 7/8, 8/9, 9/10, 10/11, 1
Number of fractions in the Farey sequence:
F( 100) =   3044
F( 200) =  12232
F( 300) =  27398
F( 400) =  48678
F( 500) =  76116
F( 600) = 109500
F( 700) = 149018
F( 800) = 194750
F( 900) = 246326
F(1000) = 304192

```



## D



{{improve|D|

 The output for the first and last term   (as per the task's requirement)

 is to show the first term as   <big>'''0/1'''</big>,

 and to show the last term as   <big>'''1/1'''</big>.

 }}


This imports the module from the Arithmetic/Rational task.

```d
import std.stdio, std.algorithm, std.range, arithmetic_rational;

auto farey(in int n) pure nothrow @safe {
    return rational(0, 1).only.chain(
            iota(1, n + 1)
            .map!(k => iota(1, k + 1).map!(m => rational(m, k)))
            .join.sort().uniq);
}

void main() @safe {
    writefln("Farey sequence for order 1 through 11:\n%(%s\n%)",
             iota(1, 12).map!farey);
    writeln("\nFarey sequence fractions, 100 to 1000 by hundreds:\n",
            iota(100, 1_001, 100).map!(i => i.farey.walkLength));
}
```

{{out}}

```txt
Farey sequence for order 1 through 11:
[0, 1]
[0, 1/2, 1]
[0, 1/3, 1/2, 2/3, 1]
[0, 1/4, 1/3, 1/2, 2/3, 3/4, 1]
[0, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 1]
[0, 1/6, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 5/6, 1]
[0, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 2/5, 3/7, 1/2, 4/7, 3/5, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 1]
[0, 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8, 1]
[0, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 1]
[0, 1/10, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 3/10, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 7/10, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 9/10, 1]
[0, 1/11, 1/10, 1/9, 1/8, 1/7, 1/6, 2/11, 1/5, 2/9, 1/4, 3/11, 2/7, 3/10, 1/3, 4/11, 3/8, 2/5, 3/7, 4/9, 5/11, 1/2, 6/11, 5/9, 4/7, 3/5, 5/8, 7/11, 2/3, 7/10, 5/7, 8/11, 3/4, 7/9, 4/5, 9/11, 5/6, 6/7, 7/8, 8/9, 9/10, 10/11, 1]

Farey sequence fractions, 100 to 1000 by hundreds:
[3045, 12233, 27399, 48679, 76117, 109501, 149019, 194751, 246327, 304193]
```



### Alternative Version

This is as fast as the C entry (total run-time is 0.20 seconds).
{{trans|C}}

```d
import core.stdc.stdio: printf, putchar;

void farey(in uint n) nothrow @nogc {
    static struct Frac { uint d, n; }

    Frac f1 = { 0, 1 }, f2 = { 1, n };

    printf("%u/%u %u/%u", 0, 1, 1, n);
    while (f2.n > 1) {
        immutable k = (n + f1.n) / f2.n;
        immutable aux = f1;
        f1 = f2;
        f2 = Frac(f2.d * k - aux.d, f2.n * k - aux.n);
        printf(" %u/%u", f2.d, f2.n);
    }

    putchar('\n');
}

ulong fareyLength(in uint n, ref ulong[] cache) pure nothrow @safe {
    if (n >= cache.length) {
        auto newLen = cache.length;
        if (newLen == 0)
            newLen = 16;
        while (newLen <= n)
            newLen *= 2;
        cache.length = newLen;
    } else if (cache[n])
        return cache[n];

    ulong len = ulong(n) * (n + 3) / 2;
    for (uint p = 2, q = 0; p <= n; p = q) {
        q = n / (n / p) + 1;
        len -= fareyLength(n / p, cache) * (q - p);
    }

    cache[n] = len;
    return len;
}

void main() nothrow {
    foreach (immutable uint n; 1 .. 12) {
        printf("%u: ", n);
        n.farey;
    }

    ulong[] cache;
    for (uint n = 100; n <= 1_000; n += 100)
        printf("%u: %llu items\n", n, fareyLength(n, cache));

    immutable uint n = 10_000_000;
    printf("\n%u: %llu items\n", n, fareyLength(n, cache));
}
```

{{out}}

```txt
1: 0/1 1/1
2: 0/1 1/2 1/1
3: 0/1 1/3 1/2 2/3 1/1
4: 0/1 1/4 1/3 1/2 2/3 3/4 1/1
5: 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
6: 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
7: 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
8: 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
9: 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
10: 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
11: 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1
100: 3045 items
200: 12233 items
300: 27399 items
400: 48679 items
500: 76117 items
600: 109501 items
700: 149019 items
800: 194751 items
900: 246327 items
1000: 304193 items

10000000: 30396356427243 items
```



## EchoLisp



{{improve|EchoLisp|

 The output for the first and last term   (as per the task's requirement)

 is to show the first term as   <big>'''0/1'''</big>,

 and to show the last term as   <big>'''1/1'''</big>.

 }}



```scheme

(define distinct-divisors
	(compose make-set prime-factors))

;; euler totient :  Φ :  n / product(p_i) * product (p_i - 1)
;; # of divisors <= n

(define (Φ n)
	(let ((pdiv (distinct-divisors n)))
	(/ (* n  (for/product ((p pdiv)) (1- p))) (for/product ((p pdiv)) p))))

;; farey-sequence length |Fn|  = 1 + sigma (m=1..) Φ(m)

(define ( F-length n) (1+ (for/sum ((m (1+ n))) (Φ m))))

;; farey sequence
;; apply the definition :  O(n^2)
(define (Farey N)
	(set! N (1+ N))
	(make-set (for*/list ((n N) (d (in-range n N))) (rational n d))))


```

{{out}}

```scheme

(for ((n (in-range 1 12))) ( printf "F(%d) %s" n (Farey n)))
F(1) { 0 1 }
F(2) { 0 1/2 1 }
F(3) { 0 1/3 1/2 2/3 1 }
F(4) { 0 1/4 1/3 1/2 2/3 3/4 1 }
F(5) { 0 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1 }
F(6) { 0 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1 }
F(7) { 0 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1 }
F(8) { 0 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1 }
F(9) { 0 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1 }
F(10) { 0 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1 }
F(11) { 0 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1 }

(for (( n (in-range 100 1100 100))) (printf "|F(%d)| = %d" n (F-length n)))
|F(100)| = 3045
|F(200)| = 12233
|F(300)| = 27399
|F(400)| = 48679
|F(500)| = 76117
|F(600)| = 109501
|F(700)| = 149019
|F(800)| = 194751
|F(900)| = 246327
|F(1000)| = 304193

(for (( n '(10_000 100_000))) (printf "|F(%d)| = %d" n (F-length n)))
|F(10000)| = 30397487
|F(100000)| = 3039650755

```



## Factor

Factor's <code>ratio</code> type automatically reduces fractions such as <code>0/1</code> and <code>1/1</code> to integers, so we print those separately at the beginning and ending of every sequence. This implementation makes use of the algorithm for calculating the next term from the wiki page [https://en.wikipedia.org/wiki/Farey_sequence#Next_term]. It also makes use of Euler's totient function for recursively calculating the length [https://en.wikipedia.org/wiki/Farey_sequence#Sequence_length_and_index_of_a_fraction].

```factor
USING: formatting io kernel math math.primes.factors math.ranges
locals prettyprint sequences sequences.extras sets tools.time ;
IN: rosetta-code.farey-sequence

! Given the order n and a farey pair, calculate the next member
! of the sequence.
:: p/q ( n a/b c/d -- p/q )
    a/b c/d [ >fraction ] bi@ :> ( a b c d )
    n b + d / >integer [ c * a - ] [ d * b - ] bi / ;

: print-farey ( order -- )
    [ "F(%-2d): " printf ] [ 0 1 pick / ] bi "0/1 " write
    [ dup 1 = ] [ dup pprint bl 3dup p/q [ nip ] dip ] until
    3drop "1/1" print ;

: φ ( n -- m ) ! Euler's totient function
    [ factors members [ 1 swap recip - ] map-product ] [ * ] bi ;

: farey-length ( order -- length )
   dup 1 = [ drop 2 ]
   [ [ 1 - farey-length ] [ φ ] bi + ] if ;

: part1 ( -- ) 11 [1,b] [ print-farey ] each nl ;

: part2 ( -- )
    100 1,000 100 <range>
    [ dup farey-length "F(%-4d): %-6d members.\n" printf ] each ;

: main ( -- ) [ part1 part2 nl ] time ;

MAIN: main
```

{{out}}

```txt

F(1 ): 0/1 1/1
F(2 ): 0/1 1/2 1/1
F(3 ): 0/1 1/3 1/2 2/3 1/1
F(4 ): 0/1 1/4 1/3 1/2 2/3 3/4 1/1
F(5 ): 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
F(6 ): 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
F(7 ): 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
F(8 ): 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
F(9 ): 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
F(10): 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
F(11): 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1

F(100 ): 3045   members.
F(200 ): 12233  members.
F(300 ): 27399  members.
F(400 ): 48679  members.
F(500 ): 76117  members.
F(600 ): 109501 members.
F(700 ): 149019 members.
F(800 ): 194751 members.
F(900 ): 246327 members.
F(1000): 304193 members.

Running time: 0.033974675 seconds


```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Farey_sequence this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## FreeBASIC


```freebasic
' version 05-04-2017
' compile with: fbc -s console

' TRUE/FALSE are built-in constants since FreeBASIC 1.04
' But we have to define them for older versions.
#Ifndef TRUE
    #Define FALSE 0
    #Define TRUE Not FALSE
#EndIf

Function farey(n As ULong, descending As Long) As ULong

    Dim As Long a, b = 1, c = 1, d = n, k
    Dim As Long aa, bb, cc, dd, count

    If descending = TRUE Then
        a = 1 : c = n -1
    End If

    count += 1
    If n < 12 Then Print Str(a); "/"; Str(b); " ";

    While ((c <= n) And Not descending) Or ((a > 0) And descending)
        aa = a : bb = b : cc = c : dd = d
        k = (n + b) \ d
        a = cc : b = dd : c = k * cc - aa : d = k * dd - bb
        count += 1
        If n < 12 Then Print Str(a); "/"; Str(b); " ";
    Wend

    If n < 12 Then Print

    Return count

End Function

' ------=< MAIN >=------

For i As Long = 1 To 11
    Print "F"; Str(i); " = ";
    farey(i, FALSE)
Next
Print
For i As Long= 100 To 1000 Step 100
    Print "F";Str(i);
    Print iif(i <> 1000, " ", ""); " = ";
    Print Using "######"; farey(i, FALSE)
Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
F1 = 0/1 1/1
F2 = 0/1 1/2 1/1
F3 = 0/1 1/3 1/2 2/3 1/1
F4 = 0/1 1/4 1/3 1/2 2/3 3/4 1/1
F5 = 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
F6 = 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
F7 = 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
F8 = 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
F9 = 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
F10 = 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
F11 = 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1

F100  =   3045
F200  =  12233
F300  =  27399
F400  =  48679
F500  =  76117
F600  = 109501
F700  = 149019
F800  = 194751
F900  = 246327
F1000 = 304193
```



## FunL

Translation of Python code at [http://en.wikipedia.org/wiki/Farey_sequence#Next_term].

```funl
def farey( n ) =
  res = seq()
  a, b, c, d = 0, 1, 1, n
  res += "$a/$b"

  while c <= n
    k = (n + b)\d
    a, b, c, d = c, d, k*c - a, k*d - b
    res += "$a/$b"

for i <- 1..11
  println( "$i: ${farey(i).mkString(', ')}" )

for i <- 100..1000 by 100
  println( "$i: ${farey(i).length()}" )
```


{{out}}


```txt

1: 0/1, 1/1
2: 0/1, 1/2, 1/1
3: 0/1, 1/3, 1/2, 2/3, 1/1
4: 0/1, 1/4, 1/3, 1/2, 2/3, 3/4, 1/1
5: 0/1, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 1/1
6: 0/1, 1/6, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 5/6, 1/1
7: 0/1, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 2/5, 3/7, 1/2, 4/7, 3/5, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 1/1
8: 0/1, 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8, 1/1
9: 0/1, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 1/1
10: 0/1, 1/10, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 3/10, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 7/10, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 9/10, 1/1
11: 0/1, 1/11, 1/10, 1/9, 1/8, 1/7, 1/6, 2/11, 1/5, 2/9, 1/4, 3/11, 2/7, 3/10, 1/3, 4/11, 3/8, 2/5, 3/7, 4/9, 5/11, 1/2, 6/11, 5/9, 4/7, 3/5, 5/8, 7/11, 2/3, 7/10, 5/7, 8/11, 3/4, 7/9, 4/5, 9/11, 5/6, 6/7, 7/8, 8/9, 9/10, 10/11, 1/1
100: 3045
200: 12233
300: 27399
400: 48679
500: 76117
600: 109501
700: 149019
800: 194751
900: 246327
1000: 304193

```



## Go


```go
package main

import "fmt"

type frac struct{ num, den int }

func (f frac) String() string {
    return fmt.Sprintf("%d/%d", f.num, f.den)
}

func f(l, r frac, n int) {
    m := frac{l.num + r.num, l.den + r.den}
    if m.den <= n {
        f(l, m, n)
        fmt.Print(m, " ")
        f(m, r, n)
    }
}

func main() {
    // task 1.  solution by recursive generation of mediants
    for n := 1; n <= 11; n++ {
        l := frac{0, 1}
        r := frac{1, 1}
        fmt.Printf("F(%d): %s ", n, l)
        f(l, r, n)
        fmt.Println(r)
    }
    // task 2.  direct solution by summing totient function
    // 2.1 generate primes to 1000
    var composite [1001]bool
    for _, p := range []int{2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31} {
        for n := p * 2; n <= 1000; n += p {
            composite[n] = true
        }
    }
    // 2.2 generate totients to 1000
    var tot [1001]int
    for i := range tot {
        tot[i] = 1
    }
    for n := 2; n <= 1000; n++ {
        if !composite[n] {
            tot[n] = n - 1
            for a := n * 2; a <= 1000; a += n {
                f := n - 1
                for r := a / n; r%n == 0; r /= n {
                    f *= n
                }
                tot[a] *= f
            }
        }
    }
    // 2.3 sum totients
    for n, sum := 1, 1; n <= 1000; n++ {
        sum += tot[n]
        if n%100 == 0 {
            fmt.Printf("|F(%d)|: %d\n", n, sum)
        }
    }
}
```

{{out}}

```txt
F(1): 0/1 1/1
F(2): 0/1 1/2 1/1
F(3): 0/1 1/3 1/2 2/3 1/1
F(4): 0/1 1/4 1/3 1/2 2/3 3/4 1/1
F(5): 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
F(6): 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
F(7): 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
F(8): 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
F(9): 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
F(10): 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
F(11): 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1
|F(100)|: 3045
|F(200)|: 12233
|F(300)|: 27399
|F(400)|: 48679
|F(500)|: 76117
|F(600)|: 109501
|F(700)|: 149019
|F(800)|: 194751
|F(900)|: 246327
|F(1000)|: 304193

```




## Haskell

Generating an n'th order Farey sequence follows the algorithm described in Wikipedia.  However, for fun, to generate a list of Farey sequences we generate only the highest order sequence, creating the rest by successively pruning the original.

```Haskell
import Data.List (unfoldr, mapAccumR)
import Data.Ratio ((%), denominator, numerator)
import Text.Printf (PrintfArg, printf)

-- The n'th order Farey sequence.
farey :: Integer -> [Rational]
farey n = 0 : unfoldr step (0, 1, 1, n)
  where
    step (a, b, c, d)
      | c > n = Nothing
      | otherwise =
        let k = (n + b) `quot` d
        in Just (c %d, (c, d, k * c - a, k * d - b))

-- A list of pairs, (n, fn n), where fn is a function applied to the n'th order
-- Farey sequence.  We assume the list of orders is increasing.  Only the
-- highest order Farey sequence is evaluated; the remainder are generated by
-- successively pruning this sequence.
fareys :: ([Rational] -> a) -> [Integer] -> [(Integer, a)]
fareys fn ns = snd $ mapAccumR prune (farey $ last ns) ns
  where
    prune rs n =
      let rs'' = filter ((<= n) . denominator) rs
      in (rs'', (n, fn rs''))

fprint
  :: (PrintfArg b)
  => String -> [(Integer, b)] -> IO ()
fprint fmt = mapM_ (uncurry $ printf fmt)

showFracs :: [Rational] -> String
showFracs =
  unwords .
  map
    (concat . ([show . numerator, const "/", show . denominator] <*>) . pure)

main :: IO ()
main = do
  putStrLn "Farey Sequences\n"
  fprint "%2d %s\n" $ fareys showFracs [1 .. 11]
  putStrLn "\nSequence Lengths\n"
  fprint "%4d %d\n" $ fareys length [100,200 .. 1000]
```

Output:

```txt
Farey Sequences

 1 0/1 1/1
 2 0/1 1/2 1/1
 3 0/1 1/3 1/2 2/3 1/1
 4 0/1 1/4 1/3 1/2 2/3 3/4 1/1
 5 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
 6 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
 7 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
 8 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
 9 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
10 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
11 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1

Sequence Lengths

 100 3045
 200 12233
 300 27399
 400 48679
 500 76117
 600 109501
 700 149019
 800 194751
 900 246327
1000 304193
```



## J



{{improve|J|

 The output for the first and last term   (as per the task's requirement)

 is to show the first term as   <big>'''0/1'''</big>,

 and to show the last term as   <big>'''1/1'''</big>.

 Also, please ''translate'' the    '''r'''   character to a solidus if possible.

 }}


J has an internal data representation for completely reduced rational numbers. This displays as integers where that is possible and otherwise displays as NNNrDDD where the part to the left of the 'r' is the numerator and the part to the right of the 'r' is the denominator.

This mechanism is a part of J's "constant language", and is similar to scientific notation (which uses an 'e' instead of an 'r') and with J's complex number notation (which uses a 'j' instead of an 'r'), and which follow similar display rules.

This mechanism also hints that J's type promotion rules are designed to give internally consistent results a priority. As much as possible you do not get different results from the same operation just because you "used a different data type". J's design adopts the philosophy that "different results from the same operation based on different types" is likely to introduce errors in thinking. (Of course there are machine limits and certain floating point operations tend to introduce internal inconsistencies, but those are mentioned only in passing - they are not directly relevant to this task.)


```J
Farey=:3 :0
  0,/:~~.(#~ <:&1),%/~1x+i.y
)
```


Required examples:


```J
   Farey 1
0 1
   Farey 2
0 1r2 1
   Farey 3
0 1r3 1r2 2r3 1
   Farey 4
0 1r4 1r3 1r2 2r3 3r4 1
   Farey 5
0 1r5 1r4 1r3 2r5 1r2 3r5 2r3 3r4 4r5 1
   Farey 6
0 1r6 1r5 1r4 1r3 2r5 1r2 3r5 2r3 3r4 4r5 5r6 1
   Farey 7
0 1r7 1r6 1r5 1r4 2r7 1r3 2r5 3r7 1r2 4r7 3r5 2r3 5r7 3r4 4r5 5r6 6r7 1
   Farey 8
0 1r8 1r7 1r6 1r5 1r4 2r7 1r3 3r8 2r5 3r7 1r2 4r7 3r5 5r8 2r3 5r7 3r4 4r5 5r6 6r7 7r8 1
   Farey 9
0 1r9 1r8 1r7 1r6 1r5 2r9 1r4 2r7 1r3 3r8 2r5 3r7 4r9 1r2 5r9 4r7 3r5 5r8 2r3 5r7 3r4 7r9 4r5 5r6 6r7 7r8 8r9 1
   Farey 10
0 1r10 1r9 1r8 1r7 1r6 1r5 2r9 1r4 2r7 3r10 1r3 3r8 2r5 3r7 4r9 1r2 5r9 4r7 3r5 5r8 2r3 7r10 5r7 3r4 7r9 4r5 5r6 6r7 7r8 8r9 9r10 1
   Farey 11
0 1r11 1r10 1r9 1r8 1r7 1r6 2r11 1r5 2r9 1r4 3r11 2r7 3r10 1r3 4r11 3r8 2r5 3r7 4r9 5r11 1r2 6r11 5r9 4r7 3r5 5r8 7r11 2r3 7r10 5r7 8r11 3r4 7r9 4r5 9r11 5r6 6r7 7r8 8r9 9r10 10r11 1
   (,. #@Farey"0) 100*1+i.10
 100   3045
 200  12233
 300  27399
 400  48679
 500  76117
 600 109501
 700 149019
 800 194751
 900 246327
1000 304193
```



###  Optimized


A small change in the 'Farey' function makes the last request, faster.

A second change in the 'Farey' function makes the last request, much faster.

A third change in the 'Farey' function makes the last request, again, a little bit faster.

<strike>Even if it is 20 times faster, the response time is just acceptable.</strike>
Now the response time is quite satisfactory.

The script produces the sequences in rational number notation as well in fractional number notation.


```J
Farey=: 3 : '/:~,&0 1~.(#~<&1),(1&+%/2&+)i.y-1'

NB. rational number notation
rplc&(' 0';'= 0r0');,&('r1',LF)@:,~&'F'@:":@:x:&.>(,Farey)&.>1+i.11

NB. fractional number notation
rplc&('r';'/';' 0';'= 0/0');,&('r1',LF)@:,~&'F'@:":@:x:&.>(,Farey)&.>1+i.11

NB. number of fractions
;,&(' items',LF)@:,~&'F'@:":&.>(,.#@:Farey)&.>100*1+i.10
```


{{out}}

```txt

F1= 0r0 1r1
F2= 0r0 1r2 1r1
F3= 0r0 1r3 1r2 2r3 1r1
F4= 0r0 1r4 1r3 1r2 2r3 3r4 1r1
F5= 0r0 1r5 1r4 1r3 2r5 1r2 3r5 2r3 3r4 4r5 1r1
F6= 0r0 1r6 1r5 1r4 1r3 2r5 1r2 3r5 2r3 3r4 4r5 5r6 1r1
F7= 0r0 1r7 1r6 1r5 1r4 2r7 1r3 2r5 3r7 1r2 4r7 3r5 2r3 5r7 3r4 4r5 5r6 6r7 1r1
F8= 0r0 1r8 1r7 1r6 1r5 1r4 2r7 1r3 3r8 2r5 3r7 1r2 4r7 3r5 5r8 2r3 5r7 3r4 4r5 5r6 6r7 7r8 1r1
F9= 0r0 1r9 1r8 1r7 1r6 1r5 2r9 1r4 2r7 1r3 3r8 2r5 3r7 4r9 1r2 5r9 4r7 3r5 5r8 2r3 5r7 3r4 7r9 4r5 5r6 6r7 7r8 8r9 1r1
F10= 0r0 1r10 1r9 1r8 1r7 1r6 1r5 2r9 1r4 2r7 3r10 1r3 3r8 2r5 3r7 4r9 1r2 5r9 4r7 3r5 5r8 2r3 7r10 5r7 3r4 7r9 4r5 5r6 6r7 7r8 8r9 9r10 1r1
F11= 0r0 1r11 1r10 1r9 1r8 1r7 1r6 2r11 1r5 2r9 1r4 3r11 2r7 3r10 1r3 4r11 3r8 2r5 3r7 4r9 5r11 1r2 6r11 5r9 4r7 3r5 5r8 7r11 2r3 7r10 5r7 8r11 3r4 7r9 4r5 9r11 5r6 6r7 7r8 8r9 9r10 10r11 1r1

F1= 0/0 1/1
F2= 0/0 1/2 1/1
F3= 0/0 1/3 1/2 2/3 1/1
F4= 0/0 1/4 1/3 1/2 2/3 3/4 1/1
F5= 0/0 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
F6= 0/0 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
F7= 0/0 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
F8= 0/0 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
F9= 0/0 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
F10= 0/0 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
F11= 0/0 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1

F100 3045 items
F200 12233 items
F300 27399 items
F400 48679 items
F500 76117 items
F600 109501 items
F700 149019 items
F800 194751 items
F900 246327 items
F1000 304193 items

```



## Java

{{works with|Java|1.5+}}
This example uses the fact that it generates the fraction candidates from the bottom up as well as <code>Set</code>'s internal duplicate removal (based on <code>Comparable.compareTo</code>) to get rid of un-reduced fractions. It also uses <code>TreeSet</code> to sort based on the value of the fraction.

```java5
import java.util.TreeSet;

public class Farey{
	private static class Frac implements Comparable<Frac>{
		int num;
		int den;

		public Frac(int num, int den){
			this.num = num;
			this.den = den;
		}

		@Override
		public String toString(){
			return num + "/" + den;
		}

		@Override
		public int compareTo(Frac o){
			return Double.compare((double)num / den, (double)o.num / o.den);
		}
	}

	public static TreeSet<Frac> genFarey(int i){
		TreeSet<Frac> farey = new TreeSet<Frac>();
		for(int den = 1; den <= i; den++){
			for(int num = 0; num <= den; num++){
				farey.add(new Frac(num, den));
			}
		}
		return farey;
	}

	public static void main(String[] args){
		for(int i = 1; i <= 11; i++){
			System.out.println("F" + i + ": " + genFarey(i));
		}

		for(int i = 100; i <= 1000; i += 100){
			System.out.println("F" + i + ": " + genFarey(i).size() + " members");
		}
	}
}
```

{{out}}

```txt
F1: [0/1, 1/1]
F2: [0/1, 1/2, 1/1]
F3: [0/1, 1/3, 1/2, 2/3, 1/1]
F4: [0/1, 1/4, 1/3, 1/2, 2/3, 3/4, 1/1]
F5: [0/1, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 1/1]
F6: [0/1, 1/6, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 5/6, 1/1]
F7: [0/1, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 2/5, 3/7, 1/2, 4/7, 3/5, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 1/1]
F8: [0/1, 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8, 1/1]
F9: [0/1, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 1/1]
F10: [0/1, 1/10, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 3/10, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 7/10, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 9/10, 1/1]
F11: [0/1, 1/11, 1/10, 1/9, 1/8, 1/7, 1/6, 2/11, 1/5, 2/9, 1/4, 3/11, 2/7, 3/10, 1/3, 4/11, 3/8, 2/5, 3/7, 4/9, 5/11, 1/2, 6/11, 5/9, 4/7, 3/5, 5/8, 7/11, 2/3, 7/10, 5/7, 8/11, 3/4, 7/9, 4/5, 9/11, 5/6, 6/7, 7/8, 8/9, 9/10, 10/11, 1/1]
F100: 3045 members
F200: 12233 members
F300: 27399 members
F400: 48679 members
F500: 76117 members
F600: 109501 members
F700: 149019 members
F800: 194751 members
F900: 246327 members
F1000: 304193 members
```



## Julia

{{trans|Java}}

```julia
using DataStructures

function farey(n::Int)::OrderedSet{Rational}
    rst = OrderedSet{Rational}(Rational[0, 1])
    for den in 1:n, num in 1:den-1
        push!(rst, Rational(num, den))
    end
    return rst
end

for n in 1:11
    print("F_$n: ")
    for frac in farey(n)
        print(numerator(frac), "/", denominator(frac), " ")
    end
    println()
end

for n in 100:100:1000
    println("F_$n has ", length(farey(n)), " fractions")
end
```


{{out}}

```txt
F_1: 0/1 1/1
F_2: 0/1 1/2 1/1
F_3: 0/1 1/2 1/3 2/3 1/1
F_4: 0/1 1/2 1/3 2/3 1/4 3/4 1/1
F_5: 0/1 1/2 1/3 2/3 1/4 3/4 1/5 2/5 3/5 4/5 1/1
F_6: 0/1 1/2 1/3 2/3 1/4 3/4 1/5 2/5 3/5 4/5 1/6 5/6 1/1
F_7: 0/1 1/2 1/3 2/3 1/4 3/4 1/5 2/5 3/5 4/5 1/6 5/6 1/7 2/7 3/7 4/7 5/7 6/7 1/1
F_8: 0/1 1/2 1/3 2/3 1/4 3/4 1/5 2/5 3/5 4/5 1/6 5/6 1/7 2/7 3/7 4/7 5/7 6/7 1/8 3/8 5/8 7/8 1/1
F_9: 0/1 1/2 1/3 2/3 1/4 3/4 1/5 2/5 3/5 4/5 1/6 5/6 1/7 2/7 3/7 4/7 5/7 6/7 1/8 3/8 5/8 7/8 1/9 2/9 4/9 5/9 7/9 8/9 1/1
F_10: 0/1 1/2 1/3 2/3 1/4 3/4 1/5 2/5 3/5 4/5 1/6 5/6 1/7 2/7 3/7 4/7 5/7 6/7 1/8 3/8 5/8 7/8 1/9 2/9 4/9 5/9 7/9 8/9 1/10 3/10 7/10 9/10 1/1
F_11: 0/1 1/2 1/3 2/3 1/4 3/4 1/5 2/5 3/5 4/5 1/6 5/6 1/7 2/7 3/7 4/7 5/7 6/7 1/8 3/8 5/8 7/8 1/9 2/9 4/9 5/9 7/9 8/9 1/10 3/10 7/10 9/10 1/11 2/11 3/11 4/11 5/11 6/11 7/11 8/11 9/11 10/11 1/1
F_100 has 3045 fractions
F_200 has 12233 fractions
F_300 has 27399 fractions
F_400 has 48679 fractions
F_500 has 76117 fractions
F_600 has 109501 fractions
F_700 has 149019 fractions
F_800 has 194751 fractions
F_900 has 246327 fractions
F_1000 has 304193 fractions
```



## Kotlin


```scala
// version 1.1

fun farey(n: Int): List<String> {
    var a = 0
    var b = 1
    var c = 1
    var d = n
    val f = mutableListOf("$a/$b")
    while (c <= n) {
        val k = (n + b) / d
        val aa = a
        val bb = b
        a = c
        b = d
        c = k * c - aa
        d = k * d - bb
        f.add("$a/$b")
    }
    return f.toList()
}

fun main(args: Array<String>) {
    for (i in 1..11)
        println("${"%2d".format(i)}: ${farey(i).joinToString(" ")}")
    println()
    for (i in 100..1000 step 100)
        println("${"%4d".format(i)}: ${"%6d".format(farey(i).size)} fractions")
}
```


{{out}}

```txt

 1: 0/1 1/1
 2: 0/1 1/2 1/1
 3: 0/1 1/3 1/2 2/3 1/1
 4: 0/1 1/4 1/3 1/2 2/3 3/4 1/1
 5: 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
 6: 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
 7: 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
 8: 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
 9: 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
10: 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
11: 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1

 100:   3045 fractions
 200:  12233 fractions
 300:  27399 fractions
 400:  48679 fractions
 500:  76117 fractions
 600: 109501 fractions
 700: 149019 fractions
 800: 194751 fractions
 900: 246327 fractions
1000: 304193 fractions

```



## Lua


```Lua
-- Return farey sequence of order n
function farey (n)
    local a, b, c, d, k = 0, 1, 1, n
    local farTab = {{a, b}}
    while c <= n do
        k = math.floor((n + b) / d)
        a, b, c, d = c, d, k * c - a, k * d - b
        table.insert(farTab, {a, b})
    end
    return farTab
end

-- Main procedure
for i = 1, 11 do
    io.write(i .. ": ")
    for _, frac in pairs(farey(i)) do io.write(frac[1] .. "/" .. frac[2] .. " ") end
    print()
end
for i = 100, 1000, 100 do print(i .. ": " .. #farey(i) .. " items") end
```

{{out}}

```txt
1: 0/1 1/1
2: 0/1 1/2 1/1
3: 0/1 1/3 1/2 2/3 1/1
4: 0/1 1/4 1/3 1/2 2/3 3/4 1/1
5: 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
6: 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
7: 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
8: 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
9: 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
10: 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
11: 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1
100: 3045 items
200: 12233 items
300: 27399 items
400: 48679 items
500: 76117 items
600: 109501 items
700: 149019 items
800: 194751 items
900: 246327 items
1000: 304193 items
```



## Maple


```Maple
#Displays terms in Farey_sequence of order n
farey_sequence := proc(n)
	local a,b,c,d,k;
	a,b,c,d := 0,1,1,n;
	printf("%d/%d", a,b);
	while(c <= n) do
		k := trunc((n+b)/d);
		a,b,c,d := c,d,c*k-a,d*k-b;
		printf(", %d/%d", a,b);
	end do;
	printf("\n");
end proc;

#Returns the length of a Farey sequence
farey_len := proc(n)
	return 1 + add(NumberTheory:-Totient(k), k=1..n);
end proc;

for i to 11 do
	farey_sequence(i);
end do;
printf("\n");
for j from 100 to 1000 by 100 do
	printf("%d\n", farey_len(j));
end do;
```

{{Out|Output}

```txt

0/1, 1/1
0/1, 1/2, 1/1
0/1, 1/3, 1/2, 2/3, 1/1
0/1, 1/4, 1/3, 1/2, 2/3, 3/4, 1/1
0/1, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 1/1
0/1, 1/6, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 5/6, 1/1
0/1, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 2/5, 3/7, 1/2, 4/7, 3/5, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 1/1
0/1, 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8, 1/1
0/1, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 1/1
0/1, 1/10, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 3/10, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 7/10, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 9/10, 1/1
0/1, 1/11, 1/10, 1/9, 1/8, 1/7, 1/6, 2/11, 1/5, 2/9, 1/4, 3/11, 2/7, 3/10, 1/3, 4/11, 3/8, 2/5, 3/7, 4/9, 5/11, 1/2, 6/11, 5/9, 4/7, 3/5, 5/8, 7/11, 2/3, 7/10, 5/7, 8/11, 3/4, 7/9, 4/5, 9/11, 5/6, 6/7, 7/8, 8/9, 9/10, 10/11, 1/1

3045
12233
27399
48679
76117
109501
149019
194751
246327
304193

```



## Mathematica



{{improve|Mathematica|

 The output for the first and last term   (as per the task's requirement)

 is to show the first term as   <big>'''0/1'''</big>,

 and to show the last term as   <big>'''1/1'''</big>.

 }}


FareySequence is a built-in command in the Wolfram Language

```Mathematica
FareySequence /@ Range[11]
Table[Length[FareySequence[n]], {n, 100, 1000, 100}]
```

{{out}}

```txt
{{0, 1}, {0, 1/2, 1}, {0, 1/3, 1/2, 2/3, 1}, {0, 1/4, 1/3, 1/2, 2/3, 3/4, 1},
   {0, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 1},
   {0, 1/6, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 5/6, 1},
   {0, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 2/5, 3/7, 1/2, 4/7, 3/5, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 1},
   {0, 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8, 1},
   {0, 1/9,1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 1},
  {0, 1/10, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 3/10, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 7/10, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 9/10, 1},
  {0, 1/11, 1/10, 1/9, 1/8, 1/7, 1/6, 2/11, 1/5, 2/9, 1/4, 3/11, 2/7, 3/10, 1/3, 4/11, 3/8, 2/5, 3/7, 4/9, 5/11, 1/2, 6/11, 5/9, 4/7, 3/5, 5/8, 7/11, 2/3, 7/10, 5/7, 8/11, 3/4, 7/9, 4/5, 9/11, 5/6, 6/7, 7/8, 8/9, 9/10, 10/11,1}}


{3045, 12233, 27399, 48679, 76117, 109501, 149019, 194751, 246327, 304193}
```



## Nim

{{trans|D}}

```Nim
import strformat

proc farey(n: int) =
  var f1 = (d: 0, n: 1)
  var f2 = (d: 1, n: n)
  write(stdout, fmt"0/1 1/{n}")
  while f2.n > 1:
    let k = (n + f1.n) div f2.n
    let aux = f1
    f1 = f2
    f2 = (f2.d * k - aux.d, f2.n * k - aux.n)
    write(stdout, fmt" {f2.d}/{f2.n}")
  write(stdout, "\n")

proc fareyLength(n: int, cache: var seq[int]): int =
  if n >= cache.len:
    var newLen = cache.len
    if newLen == 0:
      newLen = 16
    while newLen <= n:
      newLen *= 2
    cache.setLen(newLen)
  elif cache[n] != 0:
    return cache[n]

  var length = n * (n + 3) div 2
  var p = 2
  var q = 0
  while p <= n:
    q = n div (n div p) + 1
    dec length, fareyLength(n div p, cache) * (q - p)
    p = q
  cache[n] = length
  return length

for n in 1..11:
  write(stdout, fmt"{n:>8}: ")
  farey(n)

var cache: seq[int] = @[]
for n in countup(100, 1000, step=100):
  echo fmt"{n:>8}: {fareyLength(n, cache):14} items"

let n = 10_000_000
echo fmt"{n}: {fareyLength(n, cache):14} items"
```

{{out}}

```txt

       1: 0/1 1/1
       2: 0/1 1/2 1/1
       3: 0/1 1/3 1/2 2/3 1/1
       4: 0/1 1/4 1/3 1/2 2/3 3/4 1/1
       5: 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
       6: 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
       7: 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
       8: 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
       9: 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
      10: 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
      11: 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1
     100:           3045 items
     200:          12233 items
     300:          27399 items
     400:          48679 items
     500:          76117 items
     600:         109501 items
     700:         149019 items
     800:         194751 items
     900:         246327 items
    1000:         304193 items
10000000: 30396356427243 items

```



## PARI/GP


```parigp
Farey(n)=my(v=List()); for(k=1,n,for(i=0,k,listput(v,i/k))); vecsort(Set(v));
countFarey(n)=1+sum(k=1, n, eulerphi(k));
for(n=1,11,print(Farey(n)))
apply(countFarey, 100*[1..10])
```

{{out}}

```txt
[0, 1]
[0, 1/2, 1]
[0, 1/3, 1/2, 2/3, 1]
[0, 1/4, 1/3, 1/2, 2/3, 3/4, 1]
[0, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 1]
[0, 1/6, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 5/6, 1]
[0, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 2/5, 3/7, 1/2, 4/7, 3/5, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 1]
[0, 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8, 1]
[0, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 1]
[0, 1/10, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 3/10, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 7/10, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 9/10, 1]
[0, 1/11, 1/10, 1/9, 1/8, 1/7, 1/6, 2/11, 1/5, 2/9, 1/4, 3/11, 2/7, 3/10, 1/3, 4/11, 3/8, 2/5, 3/7, 4/9, 5/11, 1/2, 6/11, 5/9, 4/7, 3/5, 5/8, 7/11, 2/3, 7/10, 5/7, 8/11, 3/4, 7/9, 4/5, 9/11, 5/6, 6/7, 7/8, 8/9, 9/10, 10/11, 1]

%1 = [3045, 12233, 27399, 48679, 76117, 109501, 149019, 194751, 246327, 304193]
```



## Pascal


{{incorrect|Pascal|

 Some of the output is missing  (as per the task's requirements.


 Compute and show the Farey sequence for orders   1   through   11   (inclusive).

 Compute and display the   number   of fractions in the Farey sequence for
                             order   100   through   1,000   (inclusive)   by hundreds.

 }}


Using a function, to get next in Farey sequence. calculated as stated in wikipedia article, see Lua [[http://rosettacode.org/wiki/Farey_sequence#Lua]].
So there is no need to store them in a big array..

```pascal
program Farey;
 {$IFDEF FPC }{$MODE DELPHI}{$ELSE}{$APPTYPE CONSOLE}{$ENDIF}
uses
   sysutils;
type
   tNextFarey= record
                 nom,dom,n,c,d: longInt;
               end;

function  InitFarey(maxdom:longINt):tNextFarey;
Begin
  with result do
  Begin
    nom := 0; dom := 1; n   := maxdom;
    c   := 1; d   := maxdom;
  end;
end;

function NextFarey(var fn:tNextFarey):boolean;
var
  k,tmp: longInt;
Begin
  with fn do
  Begin
    k := trunc((n + dom)/d);
    tmp := c;c:= k*c-nom;nom:= tmp;
    tmp := d;d:= k*d-dom;dom:= tmp;
    result := nom <> dom;
  end;
end;

var
  TestF : tNextFarey;
  cnt: NativeInt;
Begin
  TestF:= InitFarey(10);
  cnt := 1;// out of InitFarey
  repeat
    write(TestF.nom,'/',TestF.dom,',');
    inc(cnt);
  until NOT(NextFarey(TestF));
  writeln(TestF.nom,'/',TestF.dom);
  writeln(cnt);

  TestF:= InitFarey(10000);
  cnt := 1;
  repeat
    inc(cnt);
  until NOT(NextFarey(TestF));
  writeln(TestF.n:10,cnt:16);
end.
```

{{Out}}

```txt
  0/1,1/10,1/9,1/8,1/7,1/6,1/5,2/9,1/4,2/7,3/10,1/3,3/8,2/5,3/7,4/9,1/2
 ,5/9,4/7,3/5,5/8,2/3,7/10,5/7,3/4,7/9,4/5,5/6,6/7,7/8,8/9,9/10,1/1
33
     10000        30397487

real    0m0.331s
```



## Perl



###  Recurrence

This uses the recurrence from Concrete Mathematics exercise 4.61 to create them quickly (this is also on the Wikipedia page).  It also uses the totient sum to quickly get the counts.
{{libheader|ntheory}}

```perl
use warnings;
use strict;
use Math::BigRat;
use ntheory qw/euler_phi vecsum/;

sub farey {
  my $N = shift;
  my @f;
  my($m0,$n0, $m1,$n1) = (0, 1, 1, $N);
  push @f, Math::BigRat->new("$m0/$n0");
  push @f, Math::BigRat->new("$m1/$n1");
  while ($f[-1] < 1) {
    my $m = int( ($n0 + $N) / $n1) * $m1 - $m0;
    my $n = int( ($n0 + $N) / $n1) * $n1 - $n0;
    ($m0,$n0, $m1,$n1) = ($m1,$n1, $m,$n);
    push @f, Math::BigRat->new("$m/$n");
  }
  @f;
}
sub farey_count { 1 + vecsum(euler_phi(1, shift)); }

for (1 .. 11) {
  my @f = map { join "/", $_->parts }   # Force 0/1 and 1/1
          farey($_);
  print "F$_: [@f]\n";
}
for (1 .. 10, 100000) {
  print "F${_}00: ", farey_count(100*$_), " members\n";
}
```

{{out}}

```txt

F1: [0/1 1/1]
F2: [0/1 1/2 1/1]
F3: [0/1 1/3 1/2 2/3 1/1]
F4: [0/1 1/4 1/3 1/2 2/3 3/4 1/1]
F5: [0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1]
F6: [0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1]
F7: [0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1]
F8: [0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1]
F9: [0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1]
F10: [0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1]
F11: [0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1]
F100: 3045 members
F200: 12233 members
F300: 27399 members
F400: 48679 members
F500: 76117 members
F600: 109501 members
F700: 149019 members
F800: 194751 members
F900: 246327 members
F1000: 304193 members
F10000000: 30396356427243 members

```



###  Mapped Rationals

Similar to Pari and Perl6.  Same output, quite slow.  Using the recursive formula for the count, utilizing the Memoize module, would be a big help.

```perl
use warnings;
use strict;
use Math::BigRat;

sub farey {
  my $n = shift;
  my %v;
  for my $k (1 .. $n) {
    for my $i (0 .. $k) {
      $v{ Math::BigRat->new("$i/$k")->bstr }++;
    }
  }
  my @f = sort {$a <=> $b }
          map { Math::BigRat->new($_) }
          keys %v;
  @f;
}

for (1 .. 11) {
  my @f = map { join "/", $_->parts }   # Force 0/1 and 1/1
          farey($_);
  print "F$_: [@f]\n";
}
for (1 .. 10) {
  my @f = farey(100*$_);
  print "F${_}00: ", scalar(@f), " members\n";
}
```



## Perl 6

{{Works with|rakudo|2018.10}}
<!-- bug bites at farey(362): sub farey ($order) { unique 0/1, |(1..$order).map: { |(1..$^d).map: { $^n/$d } } } -->


```perl6
sub farey ($order) {
    my @l = 0/1, 1/1;
    (2..$order).map: { push @l, |(1..$^d).map: { $_/$d } }
    unique @l
}

say "Farey sequence order ";
.say for (1..11).hyper(:1batch).map: { "$_: ", .&farey.sort.map: *.nude.join('/') };
.say for (100, 200 ... 1000).race(:1batch).map: { "Farey sequence order $_ has " ~ [.&farey].elems ~ ' elements.' }
```

{{out}}

```txt
Farey sequence order
1: 0/1 1/1
2: 0/1 1/2 1/1
3: 0/1 1/3 1/2 2/3 1/1
4: 0/1 1/4 1/3 1/2 2/3 3/4 1/1
5: 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
6: 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
7: 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
8: 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
9: 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
10: 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
11: 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1
Farey sequence order 100 has 3045 elements.
Farey sequence order 200 has 12233 elements.
Farey sequence order 300 has 27399 elements.
Farey sequence order 400 has 48679 elements.
Farey sequence order 500 has 76117 elements.
Farey sequence order 600 has 109501 elements.
Farey sequence order 700 has 149019 elements.
Farey sequence order 800 has 194751 elements.
Farey sequence order 900 has 246327 elements.
Farey sequence order 1000 has 304193 elements.
```



## Phix

{{trans|AWK}}

```Phix
function farey(integer n)
integer a=0, b=1, c=1, d=n
integer items=1
    if n<=11 then
        printf(1,"%d: %d/%d",{n,a,b})
    end if
    while c<=n do
        integer k = floor((n+b)/d)
        {a,b,c,d} = {c,d,k*c-a,k*d-b}
        items += 1
        if n<=11 then
            printf(1," %d/%d",{a,b})
        end if
    end while
    return items
end function

printf(1,"Farey sequence for order 1 through 11:\n")
for i=1 to 11 do
    {} = farey(i)
    printf(1,"\n")
end for
printf(1,"Farey sequence fractions, 100 to 1000 by hundreds:\n")
sequence nf = {}
for i=100 to 1000 by 100 do
    nf = append(nf,farey(i))
end for
?nf
```

{{out}}

```txt

Farey sequence for order 1 through 11:
1: 0/1 1/1
2: 0/1 1/2 1/1
3: 0/1 1/3 1/2 2/3 1/1
4: 0/1 1/4 1/3 1/2 2/3 3/4 1/1
5: 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
6: 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
7: 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
8: 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
9: 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
10: 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
11: 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1
Farey sequence fractions, 100 to 1000 by hundreds:
{3045,12233,27399,48679,76117,109501,149019,194751,246327,304193}

```



## Prolog

The following uses SWI-Prolog's rationals (rdiv(p,q)) and assumes the availability of predsort/3.
The presentation is top-down.

```Prolog
task(1) :-
	between(1, 11, I),
	farey(I, F),
	write(I), write(': '),
	rwrite(F), nl, fail; true.

task(2) :- between(1, 10, I),
	I100 is I*100,
	farey( I100, F),
	length(F,N),
	write('|F('), write(I100), write(')| = '), writeln(N), fail; true.

% farey(+Order, Sequence)
farey(Order, Sequence) :-
  bagof( R,
	 I^J^(between(1, Order, J), between(0, J, I), R is I rdiv J),
	 S),
  predsort( rcompare, S, Sequence ).

rprint( rdiv(A,B) ) :- write(A), write(/), write(B), !.
rprint( I ) :- integer(I), write(I), write(/), write(1), !.

rwrite([]).
rwrite([R]) :- rprint(R).
rwrite([R, T|Rs]) :- rprint(R), write(', '), rwrite([T|Rs]).

rcompare(<, A, B) :- A < B, !.
rcompare(>, A, B) :- A > B, !.
rcompare(=, A, B) :- A =< B.
```

Interactive session:
```txt
?- task(1).
1: 0/1, 1/1
2: 0/1, 1/2, 1/1
3: 0/1, 1/3, 1/2, 2/3, 1/1
4: 0/1, 1/4, 1/3, 1/2, 2/3, 3/4, 1/1
5: 0/1, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 1/1
6: 0/1, 1/6, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 5/6, 1/1
7: 0/1, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 2/5, 3/7, 1/2, 4/7, 3/5, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 1/1
8: 0/1, 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8, 1/1
9: 0/1, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 1/1
10: 0/1, 1/10, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 3/10, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 7/10, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 9/10, 1/1
11: 0/1, 1/11, 1/10, 1/9, 1/8, 1/7, 1/6, 2/11, 1/5, 2/9, 1/4, 3/11, 2/7, 3/10, 1/3, 4/11, 3/8, 2/5, 3/7, 4/9, 5/11, 1/2, 6/11, 5/9, 4/7, 3/5, 5/8, 7/11, 2/3, 7/10, 5/7, 8/11, 3/4, 7/9, 4/5, 9/11, 5/6, 6/7, 7/8, 8/9, 9/10, 10/11, 1/1
true

?- task(2).
|F(100)| = 3045
|F(200)| = 12233
|F(300)| = 27399
|F(400)| = 48679
|F(500)| = 76117
|F(600)| = 109501
|F(700)| = 149019
|F(800)| = 194751
|F(900)| = 246327
|F(1000)| = 304193
true.
```



## PureBasic


```purebasic
EnableExplicit

Structure farey_struc
  complex.POINT
  quotient.d
EndStructure

#MAXORDER=1000
Global NewList fareylist.farey_struc()

Define v_start.i,
       v_end.i,
       v_step.i,
       order.i,
       fractions.i,
       check.b,
       t$

Procedure farey(order)
  NewList sequence.farey_struc()
  Define quotient.d,
         divisor.i,
         dividend.i

  For divisor=1 To order
    For dividend=0 To divisor
      quotient.d=dividend/divisor
      AddElement(sequence())
      sequence()\complex\x=dividend
      sequence()\complex\y=divisor
      sequence()\quotient=quotient
    Next
  Next

  SortStructuredList(sequence(),#PB_Sort_Ascending,
                     OffsetOf(farey_struc\quotient),
                     TypeOf(farey_struc\quotient))

  FirstElement(sequence())
  quotient=sequence()\quotient
  AddElement(fareylist())
  fareylist()\complex\x=sequence()\complex\x
  fareylist()\complex\y=sequence()\complex\y
  fareylist()\quotient=sequence()\quotient

  ForEach sequence()
    If quotient=sequence()\quotient : Continue : EndIf
    quotient=sequence()\quotient
    AddElement(fareylist())
    fareylist()\complex\x=sequence()\complex\x
    fareylist()\complex\y=sequence()\complex\y
    fareylist()\quotient=sequence()\quotient
  Next
  FreeList(sequence())
EndProcedure

OpenConsole("Farey sequence [Input exit = program end]")
Repeat
  Print("Input-> start end step [start>=1; end<=1000; step>=1; (start<end)] : ")
  t$=Input() : If Trim(LCase(t$))="exit" : End : EndIf
  v_start=Val(StringField(t$,1," "))
  v_end=Val(StringField(t$,2," "))
  v_step=Val(StringField(t$,3," "))
  check=Bool(v_start>=1 And v_end<=#MAXORDER And v_step>=1 And v_start<v_end)
Until check=#True
PrintN(~"\n"+LSet("-",80,"-"))

order=v_start
While order<=v_end
  FreeList(fareylist()) : NewList fareylist()
  farey(order)
  fractions=ListSize(fareylist())
  PrintN("Farey sequence for order "+Str(order)+" has "+Str(fractions)+" fractions.")
  If fractions<100
    ForEach fareylist()
      If ListIndex(fareylist()) % 7 = 0 : PrintN("") : EndIf
      Print(~"\t"+
             RSet(Str(fareylist()\complex\x),2," ")+"/"+
             RSet(Str(fareylist()\complex\y),2," "))
    Next
  EndIf
  PrintN(~"\n"+LSet("=",80,"="))
  order+v_step
Wend
Input()
```

{{out}}

```txt
Input-> start end step [start>=1; end<=1000; step>=1; (start<end)] : 1 12 1

--------------------------------------------------------------------------------
Farey sequence for order 1 has 2 fractions.

         0/ 1    1/ 1

### ==========================================================================

Farey sequence for order 2 has 3 fractions.

         0/ 1    1/ 2    1/ 1

### ==========================================================================

Farey sequence for order 3 has 5 fractions.

         0/ 1    1/ 3    1/ 2    2/ 3    1/ 1

### ==========================================================================

Farey sequence for order 4 has 7 fractions.

         0/ 1    1/ 4    1/ 3    1/ 2    2/ 3    3/ 4    1/ 1

### ==========================================================================

Farey sequence for order 5 has 11 fractions.

         0/ 1    1/ 5    1/ 4    1/ 3    2/ 5    1/ 2    3/ 5
         2/ 3    3/ 4    4/ 5    1/ 1

### ==========================================================================

Farey sequence for order 6 has 13 fractions.

         0/ 1    1/ 6    1/ 5    1/ 4    1/ 3    2/ 5    1/ 2
         3/ 5    2/ 3    3/ 4    4/ 5    5/ 6    1/ 1

### ==========================================================================

Farey sequence for order 7 has 19 fractions.

         0/ 1    1/ 7    1/ 6    1/ 5    1/ 4    2/ 7    1/ 3
         2/ 5    3/ 7    1/ 2    4/ 7    3/ 5    2/ 3    5/ 7
         3/ 4    4/ 5    5/ 6    6/ 7    1/ 1

### ==========================================================================

Farey sequence for order 8 has 23 fractions.

         0/ 1    1/ 8    1/ 7    1/ 6    1/ 5    1/ 4    2/ 7
         1/ 3    3/ 8    2/ 5    3/ 7    1/ 2    4/ 7    3/ 5
         5/ 8    2/ 3    5/ 7    3/ 4    4/ 5    5/ 6    6/ 7
         7/ 8    1/ 1

### ==========================================================================

Farey sequence for order 9 has 29 fractions.

         0/ 1    1/ 9    1/ 8    1/ 7    1/ 6    1/ 5    2/ 9
         1/ 4    2/ 7    1/ 3    3/ 8    2/ 5    3/ 7    4/ 9
         1/ 2    5/ 9    4/ 7    3/ 5    5/ 8    2/ 3    5/ 7
         3/ 4    7/ 9    4/ 5    5/ 6    6/ 7    7/ 8    8/ 9
         1/ 1

### ==========================================================================

Farey sequence for order 10 has 33 fractions.

         0/ 1    1/10    1/ 9    1/ 8    1/ 7    1/ 6    1/ 5
         2/ 9    1/ 4    2/ 7    3/10    1/ 3    3/ 8    2/ 5
         3/ 7    4/ 9    1/ 2    5/ 9    4/ 7    3/ 5    5/ 8
         2/ 3    7/10    5/ 7    3/ 4    7/ 9    4/ 5    5/ 6
         6/ 7    7/ 8    8/ 9    9/10    1/ 1

### ==========================================================================

Farey sequence for order 11 has 43 fractions.

         0/ 1    1/11    1/10    1/ 9    1/ 8    1/ 7    1/ 6
         2/11    1/ 5    2/ 9    1/ 4    3/11    2/ 7    3/10
         1/ 3    4/11    3/ 8    2/ 5    3/ 7    4/ 9    5/11
         1/ 2    6/11    5/ 9    4/ 7    3/ 5    5/ 8    7/11
         2/ 3    7/10    5/ 7    8/11    3/ 4    7/ 9    4/ 5
         9/11    5/ 6    6/ 7    7/ 8    8/ 9    9/10   10/11
         1/ 1

### ==========================================================================

Farey sequence for order 12 has 47 fractions.

         0/ 1    1/12    1/11    1/10    1/ 9    1/ 8    1/ 7
         1/ 6    2/11    1/ 5    2/ 9    1/ 4    3/11    2/ 7
         3/10    1/ 3    4/11    3/ 8    2/ 5    5/12    3/ 7
         4/ 9    5/11    1/ 2    6/11    5/ 9    4/ 7    7/12
         3/ 5    5/ 8    7/11    2/ 3    7/10    5/ 7    8/11
         3/ 4    7/ 9    4/ 5    9/11    5/ 6    6/ 7    7/ 8
         8/ 9    9/10   10/11   11/12    1/ 1

### ==========================================================================

```


```txt
Input-> start end step [start>=1; end<=1000; step>=1; (start<end)] : 100 1000 100

--------------------------------------------------------------------------------
Farey sequence for order 100 has 3045 fractions.


### ==========================================================================

Farey sequence for order 200 has 12233 fractions.


### ==========================================================================

Farey sequence for order 300 has 27399 fractions.


### ==========================================================================

Farey sequence for order 400 has 48679 fractions.


### ==========================================================================

Farey sequence for order 500 has 76117 fractions.


### ==========================================================================

Farey sequence for order 600 has 109501 fractions.


### ==========================================================================

Farey sequence for order 700 has 149019 fractions.


### ==========================================================================

Farey sequence for order 800 has 194751 fractions.


### ==========================================================================

Farey sequence for order 900 has 246327 fractions.


### ==========================================================================

Farey sequence for order 1000 has 304193 fractions.


### ==========================================================================

```



## Python


```python
from fractions import Fraction


class Fr(Fraction):
    def __repr__(self):
        return '(%s/%s)' % (self.numerator, self.denominator)


def farey(n, length=False):
    if not length:
        return [Fr(0, 1)] + sorted({Fr(m, k) for k in range(1, n+1) for m in range(1, k+1)})
    else:
        #return 1         +    len({Fr(m, k) for k in range(1, n+1) for m in range(1, k+1)})
        return  (n*(n+3))//2 - sum(farey(n//k, True) for k in range(2, n+1))

if __name__ == '__main__':
    print('Farey sequence for order 1 through 11 (inclusive):')
    for n in range(1, 12):
        print(farey(n))
    print('Number of fractions in the Farey sequence for order 100 through 1,000 (inclusive) by hundreds:')
    print([farey(i, length=True) for i in range(100, 1001, 100)])
```


{{out}}

```txt
Farey sequence for order 1 through 11 (inclusive):
[(0/1), (1/1)]
[(0/1), (1/2), (1/1)]
[(0/1), (1/3), (1/2), (2/3), (1/1)]
[(0/1), (1/4), (1/3), (1/2), (2/3), (3/4), (1/1)]
[(0/1), (1/5), (1/4), (1/3), (2/5), (1/2), (3/5), (2/3), (3/4), (4/5), (1/1)]
[(0/1), (1/6), (1/5), (1/4), (1/3), (2/5), (1/2), (3/5), (2/3), (3/4), (4/5), (5/6), (1/1)]
[(0/1), (1/7), (1/6), (1/5), (1/4), (2/7), (1/3), (2/5), (3/7), (1/2), (4/7), (3/5), (2/3), (5/7), (3/4), (4/5), (5/6), (6/7), (1/1)]
[(0/1), (1/8), (1/7), (1/6), (1/5), (1/4), (2/7), (1/3), (3/8), (2/5), (3/7), (1/2), (4/7), (3/5), (5/8), (2/3), (5/7), (3/4), (4/5), (5/6), (6/7), (7/8), (1/1)]
[(0/1), (1/9), (1/8), (1/7), (1/6), (1/5), (2/9), (1/4), (2/7), (1/3), (3/8), (2/5), (3/7), (4/9), (1/2), (5/9), (4/7), (3/5), (5/8), (2/3), (5/7), (3/4), (7/9), (4/5), (5/6), (6/7), (7/8), (8/9), (1/1)]
[(0/1), (1/10), (1/9), (1/8), (1/7), (1/6), (1/5), (2/9), (1/4), (2/7), (3/10), (1/3), (3/8), (2/5), (3/7), (4/9), (1/2), (5/9), (4/7), (3/5), (5/8), (2/3), (7/10), (5/7), (3/4), (7/9), (4/5), (5/6), (6/7), (7/8), (8/9), (9/10), (1/1)]
[(0/1), (1/11), (1/10), (1/9), (1/8), (1/7), (1/6), (2/11), (1/5), (2/9), (1/4), (3/11), (2/7), (3/10), (1/3), (4/11), (3/8), (2/5), (3/7), (4/9), (5/11), (1/2), (6/11), (5/9), (4/7), (3/5), (5/8), (7/11), (2/3), (7/10), (5/7), (8/11), (3/4), (7/9), (4/5), (9/11), (5/6), (6/7), (7/8), (8/9), (9/10), (10/11), (1/1)]
Number of fractions in the Farey sequence for order 100 through 1,000 (inclusive) by hundreds:
[3045, 12233, 27399, 48679, 76117, 109501, 149019, 194751, 246327, 304193]
```



And as an alternative to importing the Fraction library, we can also sketch out a Ratio type of our own:
{{Works with|Python|3.7}}

```python
'''Farey sequence'''

from itertools import (chain, count, islice)
from math import gcd


# farey :: Int -> [Ratio Int]
def farey(n):
    '''Farey sequence of order n.'''
    return sorted(
        nubBy(on(eq)(fromRatio))(
            bind(enumFromTo(1)(n))(
                lambda k: bind(enumFromTo(0)(k))(
                    lambda m: [ratio(m)(k)]
                )
            )
        ),
        key=fromRatio
    ) + [ratio(1)(1)]


# fareyLength :: Int -> Int
def fareyLength(n):
    '''Number of terms in a Farey sequence
       of order n.'''
    def go(x):
        return (x * (x + 3)) // 2 - sum(
            go(x // k) for k in enumFromTo(2)(x)
        )
    return go(n)


# showFarey :: [Ratio Int] -> String
def showFarey(xs):
    '''Stringification of a Farey sequence.'''
    return '(' + ', '.join(map(showRatio, xs)) + ')'


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Tests'''

    print(
        fTable(
            'Farey sequence for orders 1-11 (inclusive):\n'
        )(str)(showFarey)(
            farey
        )(enumFromTo(1)(11))
    )
    print(
        fTable(
            '\n\nNumber of fractions in the Farey sequence ' +
            'for order 100 through 1,000 (inclusive) by hundreds:\n'
        )(str)(str)(
            fareyLength
        )(enumFromThenTo(100)(200)(1000))
    )


# GENERIC -------------------------------------------------

# bind(>>=) :: [a] -> (a -> [b]) -> [b]
def bind(xs):
    '''List monad injection operator.
       Two computations sequentially composed,
       with any value produced by the first
       passed as an argument to the second.'''
    return lambda f: list(
        chain.from_iterable(
            map(f, xs)
        )
    )


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# enumFromThenTo :: Int -> Int -> Int -> [Int]
def enumFromThenTo(m):
    '''Integer values enumerated from m to n
       with a step defined by nxt-m.
    '''
    def go(nxt, n):
        d = nxt - m
        return islice(count(0), m, d + n, d)
    return lambda nxt: lambda n: (
        list(go(nxt, n))
    )


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# eq (==) :: Eq a => a -> a -> Bool
def eq(a):
    '''Simple equality of a and b.'''
    return lambda b: a == b


# fromRatio :: Ratio Int -> Float
def fromRatio(r):
    '''A floating point value derived from a
       a rational value.
    '''
    return r.get('numerator') / r.get('denominator')


# nubBy :: (a -> a -> Bool) -> [a] -> [a]
def nubBy(p):
    '''A sublist of xs from which all duplicates,
       (as defined by the equality predicate p)
       are excluded.
    '''
    def go(xs):
        if not xs:
            return []
        x = xs[0]
        return [x] + go(
            list(filter(
                lambda y: not p(x)(y),
                xs[1:]
            ))
        )
    return lambda xs: go(xs)


# on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
def on(f):
    '''A function returning the value of applying
      the binary f to g(a) g(b)
    '''
    return lambda g: lambda a: lambda b: f(g(a))(g(b))


# ratio :: Int -> Int -> Ratio Int
def ratio(n):
    '''Rational value constructed
       from a numerator and a denominator.
    '''
    def go(n, d):
        g = gcd(n, d)
        return {
            'type': 'Ratio',
            'numerator': n // g, 'denominator': d // g
        }
    return lambda d: go(n * signum(d), abs(d))


# showRatio :: Ratio -> String
def showRatio(r):
    '''String representation of the ratio r.'''
    d = r.get('denominator')
    return str(r.get('numerator')) + (
        '/' + str(d) if 1 != d else ''
    )


# signum :: Num -> Num
def signum(n):
    '''The sign of n.'''
    return -1 if 0 > n else (1 if 0 < n else 0)


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


# unlines :: [String] -> String
def unlines(xs):
    '''A single string derived by the intercalation
       of a list of strings with the newline character.
    '''
    return '\n'.join(xs)


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Farey sequence for orders 1-11 (inclusive):

 1 -> (0/1, 1/1, 1/1)
 2 -> (0/1, 1/2, 1/1, 1/1)
 3 -> (0/1, 1/3, 1/2, 2/3, 1/1, 1/1)
 4 -> (0/1, 1/4, 1/3, 1/2, 2/3, 3/4, 1/1, 1/1)
 5 -> (0/1, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 1/1, 1/1)
 6 -> (0/1, 1/6, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 5/6, 1/1, 1/1)
 7 -> (0/1, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 2/5, 3/7, 1/2, 4/7, 3/5, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 1/1, 1/1)
 8 -> (0/1, 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8, 1/1, 1/1)
 9 -> (0/1, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 1/1, 1/1)
10 -> (0/1, 1/10, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 3/10, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 7/10, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 9/10, 1/1, 1/1)
11 -> (0/1, 1/11, 1/10, 1/9, 1/8, 1/7, 1/6, 2/11, 1/5, 2/9, 1/4, 3/11, 2/7, 3/10, 1/3, 4/11, 3/8, 2/5, 3/7, 4/9, 5/11, 1/2, 6/11, 5/9, 4/7, 3/5, 5/8, 7/11, 2/3, 7/10, 5/7, 8/11, 3/4, 7/9, 4/5, 9/11, 5/6, 6/7, 7/8, 8/9, 9/10, 10/11, 1/1, 1/1)

Number of fractions in the Farey sequence for order 100 through 1,000 (inclusive) by hundreds:

 100 -> 3045
 200 -> 12233
 300 -> 27399
 400 -> 48679
 500 -> 76117
 600 -> 109501
 700 -> 149019
 800 -> 194751
 900 -> 246327
1000 -> 304193
```



## Racket


Once again, racket's ''math/number-theory'' package comes to the rescue!

```racket
#lang racket
(require math/number-theory)
(define (display-farey-sequence order show-fractions?)
  (define f-s (farey-sequence order))
  (printf "-- Farey Sequence for order ~a has ~a fractions~%" order (length f-s))
  ;; racket will simplify 0/1 and 1/1 to 0 and 1 respectively, so deconstruct into numerator and
  ;; denomimator (and take the opportunity to insert commas
  (when show-fractions?
    (displayln
     (string-join
      (for/list ((f f-s))
        (format "~a/~a" (numerator f) (denominator f)))
      ", "))))

; compute and show the Farey sequence for order:
;  1   through   11   (inclusive).
(for ((order (in-range 1 (add1 11)))) (display-farey-sequence order #t))
; compute and display the number of fractions in the Farey sequence for order:
;  100   through   1,000   (inclusive)   by hundreds.
(for ((order (in-range 100 (add1 1000) 100))) (display-farey-sequence order #f))
```


{{out}}

```txt
-- Farey Sequence for order 1 has 2 fractions
0/1, 1/1
-- Farey Sequence for order 2 has 3 fractions
0/1, 1/2, 1/1
-- Farey Sequence for order 3 has 5 fractions
0/1, 1/3, 1/2, 2/3, 1/1
-- Farey Sequence for order 4 has 7 fractions
0/1, 1/4, 1/3, 1/2, 2/3, 3/4, 1/1
-- Farey Sequence for order 5 has 11 fractions
0/1, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 1/1
-- Farey Sequence for order 6 has 13 fractions
0/1, 1/6, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 5/6, 1/1
-- Farey Sequence for order 7 has 19 fractions
0/1, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 2/5, 3/7, 1/2, 4/7, 3/5, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 1/1
-- Farey Sequence for order 8 has 23 fractions
0/1, 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8, 1/1
-- Farey Sequence for order 9 has 29 fractions
0/1, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 1/1
-- Farey Sequence for order 10 has 33 fractions
0/1, 1/10, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 3/10, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 7/10, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 9/10, 1/1
-- Farey Sequence for order 11 has 43 fractions
0/1, 1/11, 1/10, 1/9, 1/8, 1/7, 1/6, 2/11, 1/5, 2/9, 1/4, 3/11, 2/7, 3/10, 1/3, 4/11, 3/8, 2/5, 3/7, 4/9, 5/11, 1/2, 6/11, 5/9, 4/7, 3/5, 5/8, 7/11, 2/3, 7/10, 5/7, 8/11, 3/4, 7/9, 4/5, 9/11, 5/6, 6/7, 7/8, 8/9, 9/10, 10/11, 1/1
-- Farey Sequence for order 100 has 3045 fractions
-- Farey Sequence for order 200 has 12233 fractions
-- Farey Sequence for order 300 has 27399 fractions
-- Farey Sequence for order 400 has 48679 fractions
-- Farey Sequence for order 500 has 76117 fractions
-- Farey Sequence for order 600 has 109501 fractions
-- Farey Sequence for order 700 has 149019 fractions
-- Farey Sequence for order 800 has 194751 fractions
-- Farey Sequence for order 900 has 246327 fractions
-- Farey Sequence for order 1000 has 304193 fractions
```



## REXX


```rexx
/*REXX program  computes and displays  a  Farey sequence  (or the number of fractions). */
parse arg L H I .                                /*obtain optional arguments from the CL*/
if L=='' | L==","  then L=5                      /*Not specified?  Then use the default.*/
oldL=L                                           /*original  L   (negativity=no display.*/
L=abs(L)                                         /*but ··· use   │L│   for all else.    */
if H=='' | H==","  then H=L                      /*Not specified?  Then use the default.*/
if I=='' | I==","  then I=1                      /* "      "         "   "   "     "    */
                                                 /*step through the range by increment. */
     do n=L  to  H  by I                         /*process the range (could be only one)*/
     @=fareyF(n);    #=' 'words(@)" "            /*go ye forth and compute Farey numbers*/
     say center('Farey sequence for order '    n    " has"    #    'fractions.', 150, "═")
     if oldL<0  then iterate                     /*don't display Farey fractions if neg.*/
     say @;          say                         /*show Farey fractions and a blank line*/
     end   /*n*/                                 /* [↑]  build/display Farey fractions. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
fareyF: procedure; parse arg x;  n.1=0;  d.1=1;  n.2=1;  d.2=x         /*some kit parts.*/
        $=n.1'/'d.1  n.2"/"d.2                   /*a starter kit for the Farey sequence.*/
                                                 /* [↓]  now, build on the starter kit. */
                   do j=1;    y=j+1;   z=j+2     /*construct from thirds  and  on  "up".*/
                   n.z= (d.j+x) % d.y*n.y - n.j  /*    "     the fraction   numerator.  */
                   d.z= (d.j+x) % d.y*d.y - d.j  /*    "      "     "     denominator.  */
                   if n.z>x  then leave          /*Should the construction be stopped ? */
                   $=$  n.z'/'d.z                /*Heck no, add fraction to party mix.  */
                   end   /*j*/                   /* [↑]   construct the Farey sequence. */
        return $                                 /*return with the Farey fractions.     */
```

'''output'''   when using the following for input:   <tt>   1   11 </tt>

```txt

═══════════════════════════════════════════════════Farey sequence for order  1  has  2  fractions.════════════════════════════════════════════════════
0/1 1/1

═══════════════════════════════════════════════════Farey sequence for order  2  has  3  fractions.════════════════════════════════════════════════════
0/1 1/2 1/1

═══════════════════════════════════════════════════Farey sequence for order  3  has  5  fractions.════════════════════════════════════════════════════
0/1 1/3 1/2 2/3 1/1

═══════════════════════════════════════════════════Farey sequence for order  4  has  7  fractions.════════════════════════════════════════════════════
0/1 1/4 1/3 1/2 2/3 3/4 1/1

═══════════════════════════════════════════════════Farey sequence for order  5  has  11  fractions.═══════════════════════════════════════════════════
0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1

═══════════════════════════════════════════════════Farey sequence for order  6  has  13  fractions.═══════════════════════════════════════════════════
0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1

═══════════════════════════════════════════════════Farey sequence for order  7  has  19  fractions.═══════════════════════════════════════════════════
0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1

═══════════════════════════════════════════════════Farey sequence for order  8  has  23  fractions.═══════════════════════════════════════════════════
0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1

═══════════════════════════════════════════════════Farey sequence for order  9  has  29  fractions.═══════════════════════════════════════════════════
0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1

══════════════════════════════════════════════════Farey sequence for order  10  has  33  fractions.═══════════════════════════════════════════════════
0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1

══════════════════════════════════════════════════Farey sequence for order  11  has  43  fractions.═══════════════════════════════════════════════════
0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1

```

'''output'''   when using the following for input:   <tt>   -100   1000   100 </tt>

```txt

═════════════════════════════════════════════════Farey sequence for order  100  has  3045  fractions.═════════════════════════════════════════════════
════════════════════════════════════════════════Farey sequence for order  200  has  12233  fractions.═════════════════════════════════════════════════
════════════════════════════════════════════════Farey sequence for order  300  has  27399  fractions.═════════════════════════════════════════════════
════════════════════════════════════════════════Farey sequence for order  400  has  48679  fractions.═════════════════════════════════════════════════
════════════════════════════════════════════════Farey sequence for order  500  has  76117  fractions.═════════════════════════════════════════════════
════════════════════════════════════════════════Farey sequence for order  600  has  109501  fractions.════════════════════════════════════════════════
════════════════════════════════════════════════Farey sequence for order  700  has  149019  fractions.════════════════════════════════════════════════
════════════════════════════════════════════════Farey sequence for order  800  has  194751  fractions.════════════════════════════════════════════════
════════════════════════════════════════════════Farey sequence for order  900  has  246327  fractions.════════════════════════════════════════════════
═══════════════════════════════════════════════Farey sequence for order  1000  has  304193  fractions.════════════════════════════════════════════════

```



## Ring


```ring

# Project : Farey sequence

for i = 1 to 11
     count = 0
     see "F" + string(i) + " = "
     farey(i, false)
next
see nl
for x = 100 to 1000 step 100
      count = 0
      see "F" + string(x) + " = "
      see farey(x, false)
      see nl
next

func farey(n, descending)
        a = 0
        b = 1
        c = 1
        d = n
        if descending = true
           a = 1
           c = n -1
        ok
        count = count + 1
        if n < 12
           see string(a) + "/" + string(b) + " "
        ok
        while ((c <= n) and not descending) or ((a > 0) and descending)
                  aa = a
                  bb = b
                  cc = c
                  dd = d
                  k = floor((n + b) / d)
                  a = cc
                  b = dd
                  c = k * cc - aa
                  d = k * dd - bb
                  count = count + 1
                  if n < 12
                     see string(a) + "/" + string(b) + " "
                  ok
        end
        if n < 12
           see nl
        ok
        return count

```

Output:

```txt

F1 = 0/1 1/1
F2 = 0/1 1/2 1/1
F3 = 0/1 1/3 1/2 2/3 1/1
F4 = 0/1 1/4 1/3 1/2 2/3 3/4 1/1
F5 = 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
F6 = 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
F7 = 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
F8 = 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
F9 = 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
F10 = 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
F11 = 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1

F100  =   3045
F200  =  12233
F300  =  27399
F400  =  48679
F500  =  76117
F600  = 109501
F700  = 149019
F800  = 194751
F900  = 246327
F1000 = 304193

```



## Ruby

{{trans|Python}}

```ruby
def farey(n, length=false)
  if length
    (n*(n+3))/2 - (2..n).sum{|k| farey(n/k, true)}
  else
    (1..n).each_with_object([]){|k,a|(0..k).each{|m|a << Rational(m,k)}}.uniq.sort
  end
end

puts 'Farey sequence for order 1 through 11 (inclusive):'
for n in 1..11
  puts "F(#{n}): " + farey(n).join(", ")
end
puts 'Number of fractions in the Farey sequence:'
for i in (100..1000).step(100)
  puts "F(%4d) =%7d" % [i, farey(i, true)]
end
```


{{out}}

```txt

Farey sequence for order 1 through 11 (inclusive):
F(1): 0/1, 1/1
F(2): 0/1, 1/2, 1/1
F(3): 0/1, 1/3, 1/2, 2/3, 1/1
F(4): 0/1, 1/4, 1/3, 1/2, 2/3, 3/4, 1/1
F(5): 0/1, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 1/1
F(6): 0/1, 1/6, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 5/6, 1/1
F(7): 0/1, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 2/5, 3/7, 1/2, 4/7, 3/5, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 1/1
F(8): 0/1, 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8, 1/1
F(9): 0/1, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 1/1
F(10): 0/1, 1/10, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 3/10, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 7/10, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 9/10, 1/1
F(11): 0/1, 1/11, 1/10, 1/9, 1/8, 1/7, 1/6, 2/11, 1/5, 2/9, 1/4, 3/11, 2/7, 3/10, 1/3, 4/11, 3/8, 2/5, 3/7, 4/9, 5/11, 1/2, 6/11, 5/9, 4/7, 3/5, 5/8, 7/11, 2/3, 7/10, 5/7, 8/11, 3/4, 7/9, 4/5, 9/11, 5/6, 6/7, 7/8, 8/9, 9/10, 10/11, 1/1
Number of fractions in the Farey sequence:
F( 100) =   3045
F( 200) =  12233
F( 300) =  27399
F( 400) =  48679
F( 500) =  76117
F( 600) = 109501
F( 700) = 149019
F( 800) = 194751
F( 900) = 246327
F(1000) = 304193

```



## Scala


```scala

object Farey {

  def fareySequence(n: Int, start: (Int, Int), stop: (Int, Int)): Stream[(Int, Int)] = {
    val (nominator_l, denominator_l) = start
    val (nominator_r, denominator_r) = stop

    val mediant = ((nominator_l + nominator_r), (denominator_l + denominator_r))

    if (mediant._2 <= n) fareySequence(n, start, mediant) ++ mediant #:: fareySequence(n, mediant, stop)
    else Stream.empty
  }

  def farey(n: Int, start: (Int, Int) = (0, 1), stop: (Int, Int) = (1, 1)): Stream[(Int, Int)] = {
    start #:: fareySequence(n, start, stop) ++ stop #:: Stream.empty[(Int, Int)]
  }

  def main(args: Array[String]): Unit = {
    for (i <- 1 to 11) {
      println(s"$i: " + farey(i).map(e => s"${e._1}/${e._2}").mkString(", "))
    }
    println
    for (i <- 100 to 1000 by 100) {
      println(s"$i: " + farey(i).length + " elements")
    }
  }

}

```


{{out}}

```txt

1: 0/1, 1/1
2: 0/1, 1/2, 1/1
3: 0/1, 1/3, 1/2, 2/3, 1/1
4: 0/1, 1/4, 1/3, 1/2, 2/3, 3/4, 1/1
5: 0/1, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 1/1
6: 0/1, 1/6, 1/5, 1/4, 1/3, 2/5, 1/2, 3/5, 2/3, 3/4, 4/5, 5/6, 1/1
7: 0/1, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 2/5, 3/7, 1/2, 4/7, 3/5, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 1/1
8: 0/1, 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8, 1/1
9: 0/1, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 1/1
10: 0/1, 1/10, 1/9, 1/8, 1/7, 1/6, 1/5, 2/9, 1/4, 2/7, 3/10, 1/3, 3/8, 2/5, 3/7, 4/9, 1/2, 5/9, 4/7, 3/5, 5/8, 2/3, 7/10, 5/7, 3/4, 7/9, 4/5, 5/6, 6/7, 7/8, 8/9, 9/10, 1/1
11: 0/1, 1/11, 1/10, 1/9, 1/8, 1/7, 1/6, 2/11, 1/5, 2/9, 1/4, 3/11, 2/7, 3/10, 1/3, 4/11, 3/8, 2/5, 3/7, 4/9, 5/11, 1/2, 6/11, 5/9, 4/7, 3/5, 5/8, 7/11, 2/3, 7/10, 5/7, 8/11, 3/4, 7/9, 4/5, 9/11, 5/6, 6/7, 7/8, 8/9, 9/10, 10/11, 1/1

100: 3045 elements
200: 12233 elements
300: 27399 elements
400: 48679 elements
500: 76117 elements
600: 109501 elements
700: 149019 elements
800: 194751 elements
900: 246327 elements
1000: 304193 elements

```



## Scheme



```scheme

(import (scheme base)
        (scheme write))

;; create a generator for Farey sequence n
;; using next term formula from https://en.wikipedia.org/wiki/Farey_sequence
(define (farey-generator n)
  (let ((a #f) (b 1) (c #f) (d n))
    (lambda ()
      (cond ((not a)    ; first item in sequence
             (set! a 0)
             (/ a b))
            ((not c)    ; second item in sequence
             (set! c 1)
             (/ c d))
            ((= c d)    ; return #f when finished sequence
             #f)
            (else       ; compute next term
              (let* ((f (floor (/ (+ n b) d)))
                     (p (- (* f c) a))
                     (q (- (* f d) b)))
                (set! a c)
                (set! b d)
                (set! c p)
                (set! d q)
                (/ p q)))))))

(define (farey-sequence n display?)
  (define (display-rat n) ; ensure 0,1 show /1
    (display n)
    (when (= 1 (denominator n))
      (display "/1"))
    (display " "))
  ;
  (let ((gen (farey-generator n)))
    (do ((res (gen) (gen))
         (count 0 (+ 1 count)))
      ((not res) (when display? (newline))
                 count)
      (when display? (display-rat res)))))

;;

(display "Farey sequence for order 1 through 11 (inclusive):\n")
(do ((i 1 (+ i 1)))
  ((> i 11) )
  (display (string-append "F(" (number->string i) "): "))
  (farey-sequence i #t))

(display "\nNumber of fractions in the Farey sequence:\n")
(do ((i 100 (+ i 100)))
  ((> i 1000) )
  (display
    (string-append "F(" (number->string i) ") = "
                   (number->string (farey-sequence i #f))))
  (newline))

```


{{out}}

```txt

Farey sequence for order 1 through 11 (inclusive):
F(1): 0/1 1/1
F(2): 0/1 1/2 1/1
F(3): 0/1 1/3 1/2 2/3 1/1
F(4): 0/1 1/4 1/3 1/2 2/3 3/4 1/1
F(5): 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
F(6): 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
F(7): 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
F(8): 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
F(9): 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
F(10): 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
F(11): 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1

Number of fractions in the Farey sequence:
F(100) = 3045
F(200) = 12233
F(300) = 27399
F(400) = 48679
F(500) = 76117
F(600) = 109501
F(700) = 149019
F(800) = 194751
F(900) = 246327
F(1000) = 304193

```



## Sidef


```ruby
func farey_count(n) {   # A005728
    1 + sum(1..n, {|k| euler_phi(k) })
}

func farey(n) {

    var seq = [0]
    var (a,b,c,d) = (0,1,1,n)

    while (c <= n) {
        var k = (n+b)//d
        (a,b,c,d) = (c, d, k*c - a, k*d - b)
        seq << a/b
    }

    return seq
}

say "Farey sequence for order 1 through 11 (inclusive):"
for n in (1..11) {
    say("F(%2d): %s" % (n, farey(n).map{.as_frac}.join(" ")))
}

say "\nNumber of fractions in the Farey sequence:"
for n in (100..1000 -> by(100)) {
    say ("F(%4d) =%7d" % (n, farey_count(n)))
}
```

{{out}}

```txt

Farey sequence for order 1 through 11 (inclusive):
F( 1): 0/1 1/1
F( 2): 0/1 1/2 1/1
F( 3): 0/1 1/3 1/2 2/3 1/1
F( 4): 0/1 1/4 1/3 1/2 2/3 3/4 1/1
F( 5): 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
F( 6): 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
F( 7): 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
F( 8): 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
F( 9): 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
F(10): 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
F(11): 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1

Number of fractions in the Farey sequence:
F( 100) =   3045
F( 200) =  12233
F( 300) =  27399
F( 400) =  48679
F( 500) =  76117
F( 600) = 109501
F( 700) = 149019
F( 800) = 194751
F( 900) = 246327
F(1000) = 304193

```



## Stata



```stata
mata
function totient(n_) {
	n = n_
	if (n<4) {
		if (n<1) return(.)
		else if (n>1) return(n-1)
		else return(1)
	}
	else {
		r = 1
		if (mod(n,2)==0) {
			n = floor(n/2)
			while (mod(n,2)==0) {
				n = floor(n/2)
				r = r*2
			}
		}
		for (k=3; k*k<=n; k=k+2) {
			if (mod(n,k)==0) {
				r = r*(k-1)
				n = floor(n/k)
				while (mod(n,k)==0) {
					n = floor(n/k)
					r = r*k
				}
			}
		}
		if (n>1) r = r*(n-1)
		return(r)
	}
}

function map(f,a) {
	n = rows(a)
	p = cols(a)
	b = J(n,p,.)
	for (i=1; i<=n; i++) {
		for (j=1; j<=p; j++) {
			b[i,j] = (*f)(a[i,j])
		}
	}
	return(b)
}

function farey_length(n) {
	return(1+sum(map(&totient(),1::n)))
}

function farey(n) {
	m = 1+sum(map(&totient(),1::n))
	r = J(m,2,.)
	r[1,.] = 0,1
	a = 0
	b = 1
	c = 1
	d = n
	i = 1
	while (c<=n) {
		k = floor((n+b)/d)
		a = k*c-a
		b = k*d-b
		swap(a,c)
		swap(b,d)
		r[++i,.] = a,b
	}
	return(r)
}

for (n=1; n<=11; n++) {
	a = farey(n)
	m = rows(a)
	for (i=1; i<=m; i++) printf("%f/%f ",a[i,1],a[i,2])
	printf("\n")
}

map(&farey_length(),100*(1..10))
end
```


'''Output'''


```txt
0/1 1/1
0/1 1/2 1/1
0/1 1/3 1/2 2/3 1/1
0/1 1/4 1/3 1/2 2/3 3/4 1/1
0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1

            1        2        3        4        5        6        7        8        9       10
    +-------------------------------------------------------------------------------------------+
  1 |    3045    12233    27399    48679    76117   109501   149019   194751   246327   304193  |
    +-------------------------------------------------------------------------------------------+
```



## Swift

Class with computed properties:

```swift
class Farey {
    let n: Int

    init(_ x: Int) {
        n = x
    }

    //using algorithm from wikipedia
    var sequence: [(Int,Int)] {
        var a = 0
        var b = 1
        var c = 1
        var d = n
        var results = [(a, b)]
        while c <= n {
            let k = (n + b) / d
            let oldA = a
            let oldB = b
            a = c
            b = d
            c = k * c - oldA
            d = k * d - oldB
            results += [(a, b)]
        }
        return results
    }

    var formattedSequence: String {
        var s = "\(n):"
        for pair in sequence {
            s += " \(pair.0)/\(pair.1)"
        }
        return s
    }

}

print("Sequences\n")

for n in 1...11 {
    print(Farey(n).formattedSequence)
}

print("\nSequence Lengths\n")

for n in 1...10 {
    let m = n * 100
    print("\(m): \(Farey(m).sequence.count)")
}
```

{{out}}

```txt

Sequences

1: 0/1 1/1
2: 0/1 1/2 1/1
3: 0/1 1/3 1/2 2/3 1/1
4: 0/1 1/4 1/3 1/2 2/3 3/4 1/1
5: 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
6: 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
7: 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
8: 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
9: 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
10: 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
11: 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1

Sequence Lengths

100: 3045
200: 12233
300: 27399
400: 48679
500: 76117
600: 109501
700: 149019
800: 194751
900: 246327
1000: 304193

```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

proc farey {n} {
    set nums [lrepeat [expr {$n+1}] 1]
    set result {{0 1}}
    for {set found 1} {$found} {} {
	set nj [lindex $nums [set j 1]]
	for {set found 0;set i 1} {$i <= $n} {incr i} {
	    if {[lindex $nums $i]*$j < $nj*$i} {
		set nj [lindex $nums [set j $i]]
		set found 1
	    }
	}
	lappend result [list $nj $j]
	for {set i $j} {$i <= $n} {incr i $j} {
	    lset nums $i [expr {[lindex $nums $i] + 1}]
	}
    }
    return $result
}

for {set i 1} {$i <= 11} {incr i} {
    puts F($i):\x20[lmap n [farey $i] {join $n /}]
}
for {set i 100} {$i <= 1000} {incr i 100} {
    puts |F($i)|\x20=\x20[llength [farey $i]]
}
```

{{out}}

```txt

F(1): 0/1 1/1
F(2): 0/1 1/2 1/1
F(3): 0/1 1/3 1/2 2/3 1/1
F(4): 0/1 1/4 1/3 1/2 2/3 3/4 1/1
F(5): 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
F(6): 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
F(7): 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
F(8): 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
F(9): 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
F(10): 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
F(11): 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1
|F(100)| = 3045
|F(200)| = 12233
|F(300)| = 27399
|F(400)| = 48679
|F(500)| = 76117
|F(600)| = 109501
|F(700)| = 149019
|F(800)| = 194751
|F(900)| = 246327
|F(1000)| = 304193

```



## zkl

{{trans|C}}

```zkl
fcn farey(n){
   f1,f2:=T(0,1),T(1,n);  // fraction is (num,dnom)
   print("%d/%d %d/%d".fmt(0,1,1,n));
   while(f2[1]>1){
      k,t  :=(n + f1[1])/f2[1], f1;
      f1,f2 = f2,T(f2[0]*k - t[0], f2[1]*k - t[1]);
      print(" %d/%d".fmt(f2.xplode()));
   }
   println();
}
```


```zkl
foreach n in ([1..11]){ print("%2d: ".fmt(n)); farey(n); }
```

{{out}}

```txt

 1: 0/1 1/1
 2: 0/1 1/2 1/1
 3: 0/1 1/3 1/2 2/3 1/1
 4: 0/1 1/4 1/3 1/2 2/3 3/4 1/1
 5: 0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1
 6: 0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1
 7: 0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1
 8: 0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1
 9: 0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1
10: 0/1 1/10 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 3/10 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 7/10 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 9/10 1/1
11: 0/1 1/11 1/10 1/9 1/8 1/7 1/6 2/11 1/5 2/9 1/4 3/11 2/7 3/10 1/3 4/11 3/8 2/5 3/7 4/9 5/11 1/2 6/11 5/9 4/7 3/5 5/8 7/11 2/3 7/10 5/7 8/11 3/4 7/9 4/5 9/11 5/6 6/7 7/8 8/9 9/10 10/11 1/1

```


```zkl
fcn farey_len(n){
   var cache=Dictionary();	// 107 keys to 1,000; 6323@10,000,000
   if(z:=cache.find(n)) return(z);

   len,p,q := n*(n + 3)/2, 2,0;
   while(p<=n){
      q=n/(n/p) + 1;
      len-=self.fcn(n/p) * (q - p);
      p=q;
   }
   cache[n]=len;   // len is returned
}
```


```zkl
foreach n in ([100..1000,100]){
   println("%4d: %7,d items".fmt(n,farey_len(n)));
}
n:=0d10_000_000;
println("\n%,d: %,d items".fmt(n,farey_len(n)));
```

{{out}}

```txt

 100:   3,045 items
 200:  12,233 items
 300:  27,399 items
 400:  48,679 items
 500:  76,117 items
 600: 109,501 items
 700: 149,019 items
 800: 194,751 items
 900: 246,327 items
1000: 304,193 items

10,000,000: 30,396,356,427,243 items

```

