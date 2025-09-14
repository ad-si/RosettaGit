+++
title = "Levenshtein distance/Alignment"
description = ""
date = 2019-01-02T14:27:41Z
aliases = []
[extra]
id = 13411
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "d",
  "go",
  "java",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "racket",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "tcl",
  "zkl",
]
+++

The [[Levenshtein distance]] algorithm returns the number of atomic operations (insertion, deletion or edition) that must be performed on a string in order to obtain an other one, but it does not say anything about the actual operations used or their order.

An alignment is a notation used to describe the operations used to turn a string into an other.  At some point in the strings, the minus character ('-') is placed in order to signify that a character must be added at this very place.  For instance, an alignment between the words 'place' and 'palace' is:


```txt

P-LACE
PALACE

```



## Task

Write a function that shows the alignment of two strings for the corresponding levenshtein distance.

As an example, use the words "rosettacode" and "raisethysword".

You can either implement an algorithm, or use a dedicated library (thus showing us how it is named in your language).





## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct edit_s edit_t, *edit;
struct edit_s {
	char c1, c2;
	int n;
	edit next;
};

void leven(char *a, char *b)
{
	int i, j, la = strlen(a), lb = strlen(b);
	edit *tbl = malloc(sizeof(edit) * (1 + la));
	tbl[0] = calloc((1 + la) * (1 + lb), sizeof(edit_t));
	for (i = 1; i <= la; i++)
		tbl[i] = tbl[i-1] + (1+lb);

	for (i = la; i >= 0; i--) {
		char *aa = a + i;
		for (j = lb; j >= 0; j--) {
			char *bb = b + j;
			if (!*aa && !*bb) continue;

			edit e = &tbl[i][j];
			edit repl = &tbl[i+1][j+1];
			edit dela = &tbl[i+1][j];
			edit delb = &tbl[i][j+1];

			e->c1 = *aa;
			e->c2 = *bb;
			if (!*aa) {
				e->next = delb;
				e->n = e->next->n + 1;
				continue;
			}
			if (!*bb) {
				e->next = dela;
				e->n = e->next->n + 1;
				continue;
			}

			e->next = repl;
			if (*aa == *bb) {
				e->n = e->next->n;
				continue;
			}

			if (e->next->n > delb->n) {
				e->next = delb;
				e->c1 = 0;
			}
			if (e->next->n > dela->n) {
				e->next = dela;
				e->c1 = *aa;
				e->c2 = 0;
			}
			e->n = e->next->n + 1;
		}
	}

	edit p = tbl[0];
	printf("%s -> %s: %d edits\n", a, b, p->n);

	while (p->next) {
		if (p->c1 == p->c2)
			printf("%c", p->c1);
		else {
			putchar('(');
			if (p->c1) putchar(p->c1);
			putchar(',');
			if (p->c2) putchar(p->c2);
			putchar(')');
		}

		p = p->next;
	}
	putchar('\n');

	free(tbl[0]);
	free(tbl);
}

int main(void)
{
	leven("raisethysword", "rosettacode");
	return 0;
}
```

```txt

raisethysword -> rosettacode: 8 edits
r(a,o)(i,)set(h,t)(y,a)(s,c)(w,)o(r,d)(d,e)

```



## D

Using the standard library.

```d
void main() {
    import std.stdio, std.algorithm;

    immutable s1 = "rosettacode";
    immutable s2 = "raisethysword";

    string s1b, s2b;
    size_t pos1, pos2;

    foreach (immutable c; levenshteinDistanceAndPath(s1, s2)[1]) {
        final switch (c) with (EditOp) {
            case none, substitute:
                s1b ~= s1[pos1++];
                s2b ~= s2[pos2++];
                break;
            case insert:
                s1b ~= "_";
                s2b ~= s2[pos2++];
                break;
            case remove:
                s1b ~= s1[pos1++];
                s2b ~= "_";
                break;
        }
    }

    writeln(s1b, "\n", s2b);
}
```

```txt
r_oset_tacode
raisethysword
```



## Go

Alignment computed by the Needleman-Wunch algorithm, which is used in bioinformatics.

```go
package main

import (
    "fmt"

    "github.com/biogo/biogo/align"
    ab "github.com/biogo/biogo/alphabet"
    "github.com/biogo/biogo/feat"
    "github.com/biogo/biogo/seq/linear"
)

func main() {
    // Alphabets for things like DNA are predefined in biogo, but we
    // define our own here.
    lc := ab.Must(ab.NewAlphabet("-abcdefghijklmnopqrstuvwxyz",
        feat.Undefined, '-', 0, true))
    // Construct scoring matrix for Needleman-Wunch algorithm.
    // We leave zeros on the diagonal for the Levenshtein distance of an
    // exact match and put -1s everywhere else for the Levenshtein distance
    // of an edit.
    nw := make(align.NW, lc.Len())
    for i := range nw {
        r := make([]int, lc.Len())
        nw[i] = r
        for j := range r {
            if j != i {
                r[j] = -1
            }
        }
    }
    // define input sequences
    a := &linear.Seq{Seq: ab.BytesToLetters([]byte("rosettacode"))}
    a.Alpha = lc
    b := &linear.Seq{Seq: ab.BytesToLetters([]byte("raisethysword"))}
    b.Alpha = lc
    // perform alignment
    aln, err := nw.Align(a, b)
    // format and display result
    if err != nil {
        fmt.Println(err)
        return
    }
    fa := align.Format(a, b, aln, '-')
    fmt.Printf("%s\n%s\n", fa[0], fa[1])
    aa := fmt.Sprint(fa[0])
    ba := fmt.Sprint(fa[1])
    ma := make([]byte, len(aa))
    for i := range ma {
        if aa[i] == ba[i] {
            ma[i] = ' '
        } else {
            ma[i] = '|'
        }
    }
    fmt.Println(string(ma))
}
```

The lines after the alignment point out the 8 edits.

```txt

r-oset-tacode
raisethysword
 ||   |||| ||

```



## Java


```java
public class LevenshteinAlignment {

    public static String[] alignment(String a, String b) {
        a = a.toLowerCase();
        b = b.toLowerCase();
        // i == 0
        int[][] costs = new int[a.length()+1][b.length()+1];
        for (int j = 0; j <= b.length(); j++)
            costs[0][j] = j;
        for (int i = 1; i <= a.length(); i++) {
            costs[i][0] = i;
            for (int j = 1; j <= b.length(); j++) {
                costs[i][j] = Math.min(1 + Math.min(costs[i-1][j], costs[i][j-1]), a.charAt(i - 1) == b.charAt(j - 1) ? costs[i-1][j-1] : costs[i-1][j-1] + 1);
            }
        }

	// walk back through matrix to figure out path
	StringBuilder aPathRev = new StringBuilder();
	StringBuilder bPathRev = new StringBuilder();
	for (int i = a.length(), j = b.length(); i != 0 && j != 0; ) {
	    if (costs[i][j] == (a.charAt(i - 1) == b.charAt(j - 1) ? costs[i-1][j-1] : costs[i-1][j-1] + 1)) {
		aPathRev.append(a.charAt(--i));
		bPathRev.append(b.charAt(--j));
	    } else if (costs[i][j] == 1 + costs[i-1][j]) {
		aPathRev.append(a.charAt(--i));
		bPathRev.append('-');
	    } else if (costs[i][j] == 1 + costs[i][j-1]) {
		aPathRev.append('-');
		bPathRev.append(b.charAt(--j));
	    }
	}
        return new String[]{aPathRev.reverse().toString(), bPathRev.reverse().toString()};
    }

    public static void main(String[] args) {
	String[] result = alignment("rosettacode", "raisethysword");
	System.out.println(result[0]);
	System.out.println(result[1]);
    }
}
```

```txt

r-oset-tacode
raisethysword

```



## Julia

```julia
function levenshteinalign(a::AbstractString, b::AbstractString)
    a = lowercase(a)
    b = lowercase(b)
    len_a = length(a)
    len_b = length(b)

    costs = Matrix{Int}(len_a + 1, len_b + 1)
    costs[1, :] .= 0:len_b
    @inbounds for i in 2:(len_a + 1)
        costs[i, 1] = i
        for j in 2:(len_b + 1)
            tmp = ifelse(a[i-1] == b[j-1], costs[i-1, j-1], costs[i-1, j-1] + 1)
            costs[i, j] = min(1 + min(costs[i-1, j], costs[i, j-1]), tmp)
        end
    end

    apathrev = IOBuffer()
    bpathrev = IOBuffer()
    local i = len_a + 1
    local j = len_b + 1
    @inbounds while i != 1 && j != 1
        tmp = ifelse(a[i-1] == b[j-1], costs[i-1, j-1], costs[i-1, j-1] + 1)
        if costs[i, j] == tmp
            i -= 1
            j -= 1
            print(apathrev, a[i])
            print(bpathrev, b[j])
        elseif costs[i, j] == 1 + costs[i-1, j]
            i -= 1
            print(apathrev, a[i])
            print(bpathrev, '-')
        elseif costs[i, j] == 1 + costs[i, j-1]
            j -= 1
            print(apathrev, '-')
            print(bpathrev, b[j])
        end
    end

    return reverse(String(take!(apathrev))), reverse(String(take!(bpathrev)))
end

foreach(println, levenshteinalign("rosettacode", "raisethysword"))
foreach(println, levenshteinalign("place", "palace"))
```


```txt
r-oset-tacode
raisethysword
p-lace
palace
```



## Kotlin

```scala
// version 1.1.3

fun levenshteinAlign(a: String, b: String): Array<String> {
    val aa = a.toLowerCase()
    val bb = b.toLowerCase()
    val costs = Array(a.length + 1) { IntArray(b.length + 1) }
    for (j in 0..b.length) costs[0][j] = j
    for (i in 1..a.length) {
        costs[i][0] = i
        for (j in 1..b.length) {
            val temp = costs[i - 1][j - 1] + (if (aa[i - 1] == bb[j - 1]) 0 else 1)
            costs[i][j] = minOf(1 + minOf(costs[i - 1][j], costs[i][j - 1]), temp)
        }
    }

    // walk back through matrix to figure out path
    val aPathRev = StringBuilder()
    val bPathRev = StringBuilder()
    var i = a.length
    var j = b.length
    while (i != 0 && j != 0) {
        val temp = costs[i - 1][j - 1] + (if (aa[i - 1] == bb[j - 1]) 0 else 1)
        when (costs[i][j]) {
            temp -> {
                aPathRev.append(aa[--i])
                bPathRev.append(bb[--j])
            }

            1 + costs[i-1][j] -> {
                aPathRev.append(aa[--i])
                bPathRev.append('-')
            }

            1 + costs[i][j-1] -> {
                aPathRev.append('-')
                bPathRev.append(bb[--j])
            }
        }
    }
    return arrayOf(aPathRev.reverse().toString(), bPathRev.reverse().toString())
}

fun main(args: Array<String>) {
    var result = levenshteinAlign("place", "palace")
    println(result[0])
    println(result[1])
    println()
    result = levenshteinAlign("rosettacode","raisethysword")
    println(result[0])
    println(result[1])
}
```


```txt

p-lace
palace

r-oset-tacode
raisethysword

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
```Mathematica
DamerauLevenshteinDistance["rosettacode", "raisethysword"]
```


```txt
8
```



## Perl


```perl
use strict;
use warnings;

use List::Util qw(min);

sub levenshtein_distance_alignment {
    my @s = ('^', split //, shift);
    my @t = ('^', split //, shift);

    my @A;
    @{$A[$_][0]}{qw(d s t)} = ($_, join('', @s[1 .. $_]), ('~' x $_)) for 0 .. $#s;
    @{$A[0][$_]}{qw(d s t)} = ($_, ('-' x $_), join '', @t[1 .. $_])  for 0 .. $#t;
    for my $i (1 .. $#s) {
        for my $j (1 .. $#t) {
	    if ($s[$i] ne $t[$j]) {
		$A[$i][$j]{d} = 1 + (
		    my $min = min $A[$i-1][$j]{d}, $A[$i][$j-1]{d}, $A[$i-1][$j-1]{d}
		);
		@{$A[$i][$j]}{qw(s t)} =
		$A[$i-1][$j]{d} == $min ? ($A[$i-1][$j]{s}.$s[$i], $A[$i-1][$j]{t}.'-') :
		$A[$i][$j-1]{d} == $min ? ($A[$i][$j-1]{s}.'-', $A[$i][$j-1]{t}.$t[$j]) :
		($A[$i-1][$j-1]{s}.$s[$i], $A[$i-1][$j-1]{t}.$t[$j]);
	    }
            else {
		@{$A[$i][$j]}{qw(d s t)} = (
		    $A[$i-1][$j-1]{d},
		    $A[$i-1][$j-1]{s}.$s[$i],
		    $A[$i-1][$j-1]{t}.$t[$j]
		);
            }
        }
    }
    return @{$A[-1][-1]}{'s', 't'};
}

print  join "\n", levenshtein_distance_alignment "rosettacode", "raisethysword";
```

```txt
ro-settac-o-de
raisethysword-
```



## Perl 6

```perl6
sub align ( Str $σ, Str $t ) {
    my @s = flat *, $σ.comb;
    my @t = flat *, $t.comb;

    my @A;
    @A[$_][ 0]<d s t> = $_, @s[1..$_].join, '-' x $_ for ^@s;
    @A[ 0][$_]<d s t> = $_, '-' x $_, @t[1..$_].join for ^@t;

    for 1 ..^ @s X 1..^ @t -> (\i, \j) {
	if @s[i] ne @t[j] {
	    @A[i][j]<d> = 1 + my $min =
	    min @A[i-1][j]<d>, @A[i][j-1]<d>, @A[i-1][j-1]<d>;
	    @A[i][j]<s t> =
	    @A[i-1][j]<d> == $min ??  (@A[i-1][j]<s t> Z~ @s[i], '-') !!
	    @A[i][j-1]<d> == $min ??  (@A[i][j-1]<s t> Z~ '-', @t[j]) !!
	    (@A[i-1][j-1]<s t> Z~ @s[i], @t[j]);
	} else {
	    @A[i][j]<d s t> = @A[i-1][j-1]<d s t> Z~ '', @s[i], @t[j];
	}
    }

    return @A[*-1][*-1]<s t>;
}

.say for align 'rosettacode', 'raisethysword';
```

```txt
ro-settac-o-de
raisethysword-
```



## Phix

{{trans|Kotlin}} plus the indicator from Go

```Phix
function LevenshteinAlignment(string a, b)
    integer la = length(a)+1,
            lb = length(b)+1
    sequence costs = repeat(repeat(0,lb),la)
    for j=1 to lb do
        costs[1][j] = j
    end for
    for i=2 to la do
        costs[i][1] = i
        for j=2 to lb do
            integer tmp1 = 1+min(costs[i-1][j], costs[i][j-1]),
                    tmp2 = costs[i-1][j-1] + (a[i-1]!=b[j-1])
            costs[i][j] = min(tmp1, tmp2)
        end for
    end for
    -- walk back through matrix to figure out the path
    string arev = "",
           brev = ""
    integer i = la, j = lb
    while i>1 and j>1 do
        integer tmp = costs[i-1][j-1] + (a[i-1]!=b[j-1])
        switch costs[i][j] do
            case tmp:                   i -= 1; arev &= a[i]
                                        j -= 1; brev &= b[j]
            case 1 + costs[i-1][j]:     i -= 1; arev &= a[i]
                                        brev &= '-'
            case 1 + costs[i][j-1]:     arev &= '-'
                                        j -= 1; brev &= b[j]
        end switch
    end while
    return {reverse(arev),reverse(brev)}
end function

procedure test(string a,b)
    {a,b} = LevenshteinAlignment(a,b)
    string c = sq_add(repeat(' ',length(a)),sq_mul(sq_ne(a,b),'|'-' '))
    printf(1,"%s\n%s\n%s\n\n",{a,b,c})
end procedure
test("rosettacode", "raisethysword")
test("place", "palace")
```

```txt

r-oset-tacode
raisethysword
 ||   |||| ||

p-lace
palace
 |

```



## Racket

===Simple version (no aligment)===
First we will analyze this solution that only computes the distance.
See http://blog.racket-lang.org/2012/08/dynamic-programming-versus-memoization.html
for a discussion of the code.


```racket
#lang racket

(define (memoize f)
  (local ([define table (make-hash)])
    (lambda args
      (dict-ref! table args (λ () (apply f args))))))

(define levenshtein
  (memoize
   (lambda (s t)
     (cond
       [(and (empty? s) (empty? t)) 0]
       [(empty? s) (length t)]
       [(empty? t) (length s)]
       [else
        (if (equal? (first s) (first t))
            (levenshtein (rest s) (rest t))
            (min (add1 (levenshtein (rest s) t))
                 (add1 (levenshtein s (rest t)))
                 (add1 (levenshtein (rest s) (rest t)))))]))))
```

'''Demonstration:'''

```racket
(levenshtein (string->list "rosettacode")
             (string->list "raisethysword"))
```

```txt
8
```



### Complete version

Now we extend the code from http://blog.racket-lang.org/2012/08/dynamic-programming-versus-memoization.html to show also the alignment. The code is very similar, but it stores the partial results (number of edits and alignment of each substring) in a lev structure.

```Racket
#lang racket

(struct lev (n s t))

(define (lev-add old n sx tx)
  (lev (+ n (lev-n old))
       (cons sx (lev-s old))
       (cons tx (lev-t old))))

(define (list-repeat n v)
  (build-list n (lambda (_) v)))

(define (memoize f)
  (local ([define table (make-hash)])
    (lambda args
      (dict-ref! table args (λ () (apply f args))))))

(define levenshtein/list
  (memoize
   (lambda (s t)
     (cond
       [(and (empty? s) (empty? t))
        (lev 0 '() '())]
       [(empty? s)
        (lev (length t) (list-repeat (length t) #\-) t)]
       [(empty? t)
        (lev (length s) s (list-repeat (length s) #\-))]
       [else
        (if (equal? (first s) (first t))
            (lev-add (levenshtein/list (rest s) (rest t))
                     0 (first s) (first t))
            (argmin lev-n (list (lev-add (levenshtein/list (rest s) t)
                                         1 (first s) #\-)
                                (lev-add (levenshtein/list s (rest t))
                                         1 #\- (first t))
                                (lev-add (levenshtein/list (rest s) (rest t))
                                         1 (first s) (first t)))))]))))

(define (levenshtein s t)
  (let ([result (levenshtein/list (string->list s)
                                  (string->list t))])
    (values (lev-n result)
            (list->string (lev-s result))
            (list->string (lev-t result)))))
```

'''Demonstration:'''

```racket
(let-values ([(dist exp-s exp-t)
              (levenshtein "rosettacode" "raisethysword")])
  (printf "levenshtein: ~a edits\n" dist)
  (displayln exp-s)
  (displayln exp-t))
```

```txt
levenshtein: 8 edits
r-oset-taco-de
raisethysword-
```



## Ruby

uses "lcs" from [[Longest common subsequence#Ruby|here]]

```ruby
require 'lcs'

def levenshtein_align(a, b)
  apos, bpos = LCS.new(a, b).backtrack2

  c = ""
  d = ""
  x0 = y0 = -1
  dx = dy = 0
  apos.zip(bpos) do |x,y|
    diff = x + dx - y - dy
    if diff < 0
      dx -= diff
      c += "-" * (-diff)
    elsif diff > 0
      dy += diff
      d += "-" * diff
    end
    c += a[x0+1..x]
    x0 = x
    d += b[y0+1..y]
    y0 = y
  end

  c += a[x0+1..-1]
  d += b[y0+1..-1]
  diff = a.length + y0 - b.length - x0
  if diff < 0
    c += "-" * (-diff)
  elsif diff > 0
    d += "-" * diff
  end
  [c, d]
end

puts levenshtein_align("rosettacode", "raisethysword")
```


```txt

r-oset-taco-de
raisethysword-

```



## Rust

'''Cargo.toml'''

```txt
[dependencies]
edit-distance = "^1.0.0"
```


'''src/main.rs'''

```Rust
extern crate edit_distance;

edit_distance("rosettacode", "raisethysword");
```



## Scala

{{Out}}Best seen running in your browser either by [https://scastie.scala-lang.org/I8BAESkNTjukVPzsWOUyPA ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/I8BAESkNTjukVPzsWOUyPA].

```Scala
import scala.collection.mutable
import scala.collection.parallel.ParSeq

object LevenshteinAlignment extends App {
  val vlad = new Levenshtein("rosettacode", "raisethysword")
  val alignment = vlad.revLevenstein()

  class Levenshtein(s1: String, s2: String) {
    val memoizedCosts = mutable.Map[(Int, Int), Int]()

    def revLevenstein(): (String, String) = {
      def revLev: (Int, Int, String, String) => (String, String) = {
        case (_, 0, revS1, revS2) => (revS1, revS2)
        case (0, _, revS1, revS2) => (revS1, revS2)
        case (i, j, revS1, revS2) =>
          if (memoizedCosts(i, j) == (memoizedCosts(i - 1, j - 1)
                                      + (if (s1(i - 1) != s2(j - 1)) 1 else 0)))
            revLev(i - 1, j - 1, s1(i - 1) + revS1, s2(j - 1) + revS2)
          else if (memoizedCosts(i, j) == 1 + memoizedCosts(i - 1, j))
            revLev(i - 1, j, s1(i - 1) + revS1, "-" + revS2)
          else
            revLev(i, j - 1, "-" + revS1, s2(j - 1) + revS2)
      }

      revLev(s1.length, s2.length, "", "")
    }

    private def levenshtein: Int = {
      def lev: ((Int, Int)) => Int = {
        case (k1, k2) =>
          memoizedCosts.getOrElseUpdate((k1, k2), (k1, k2) match {
            case (i, 0) => i
            case (0, j) => j
            case (i, j) =>
              ParSeq(1 + lev((i - 1, j)),
                1 + lev((i, j - 1)),
                lev((i - 1, j - 1))
                  + (if (s1(i - 1) != s2(j - 1)) 1 else 0)).min
          })
      }

      lev((s1.length, s2.length))
    }

    levenshtein
  }

  println(alignment._1)
  println(alignment._2)

}
```


## Sidef

```ruby
func align(s, t) {
    s.chars!.prepend!('^')
    t.chars!.prepend!('^')

    var A = []
    {|i| A[i][0]{@|<d s t>} = (i, s.ft(1, i).join, '~' * i) } << ^s
    {|i| A[0][i]{@|<d s t>} = (i, '-' * i, t.ft(1, i).join) } << ^t

    for i (1 .. s.end) {
      for j (1 .. t.end) {
        if (s[i] != t[j]) {
          A[i][j]{:d} = 1+(
            var min = Math.min(A[i-1][j]{:d}, A[i][j-1]{:d}, A[i-1][j-1]{:d})
          )
          A[i][j]{@|<s t>} = (A[i-1][j]{:d} == min
              ? [A[i-1][j]{:s}+s[i], A[i-1][j]{:t}+'-']
              : (A[i][j-1]{:d} == min
              ? [A[i][j-1]{:s}+'-', A[i][j-1]{:t}+t[j]]
              : [A[i-1][j-1]{:s}+s[i], A[i-1][j-1]{:t}+t[j]]))...
        }
        else {
          A[i][j]{@|<d s t>} = (
              A[i-1][j-1]{:d},
              A[i-1][j-1]{:s}+s[i],
              A[i-1][j-1]{:t}+t[j],
          )
        }
      }
    }
    return [A[-1][-1]{@|<s t>}]
}

align("rosettacode", "raisethysword").each { .say }
```

 ro-settac-o-de
 raisethysword-


## Tcl

```tcl
package require struct::list
proc levenshtein/align {a b} {
    lassign [struct::list longestCommonSubsequence [split $a ""] [split $b ""]]\
	    apos bpos
    set c ""
    set d ""
    set x0 [set y0 -1]
    set dx [set dy 0]
    foreach x $apos y $bpos {
	if {$x+$dx < $y+$dy} {
	    set n [expr {($y+$dy)-($x+$dx)}]
	    incr dx $n
	    append c [string repeat "-" $n]
	} elseif {$x+$dx > $y+$dy} {
	    set n [expr {($x+$dx)-($y+$dy)}]
	    incr dy $n
	    append d [string repeat "-" $n]
	}
	append c [string range $a $x0+1 $x]
	set x0 $x
	append d [string range $b $y0+1 $y]
	set y0 $y
    }
    append c [string range $a $x0+1 end]
    append d [string range $b $y0+1 end]
    set al [string length $a]
    set bl [string length $b]
    if {$al+$y0 < $bl+$x0} {
	append c [string repeat "-" [expr {$bl+$x0-$y0-$al}]]
    } elseif {$bl+$x0 < $al+$y0} {
	append d [string repeat "-" [expr {$al+$y0-$x0-$bl}]]
    }
    return $c\n$d
}

puts [levenshtein/align "rosettacode" "raisethysword"]
```

```txt

r-oset-taco-de
raisethysword-

```



## zkl

```zkl
fcn alignment(a,b){
   a,b = a.toLower(), b.toLower();
   costs := (a.len()+1).pump(List(),'wrap(a){ [1..b.len()].pump(List(a)) });
   foreach i,j in (a.len()+1, [1..b.len()]){
      costs[i][j] = ( 1 + costs[i-1][j].min(costs[i][j-1]) )
         .min(if(a[i-1] == b[j-1]) costs[i-1][j-1] else costs[i-1][j-1] + 1);
   }
   // walk back through matrix to figure out path
   aPathRev,bPathRev := Data(),Data();  // byte buckets
   i,j := a.len(), b.len();
   while(i!=0 and j!= 0){
      if (costs[i][j] ==
          ( if(a[i-1]==b[j-1]) costs[i-1][j-1] else costs[i-1][j-1]+1 )){
         aPathRev.append(a[i-=1]);
	 bPathRev.append(b[j-=1]);
      } else if(costs[i][j] == 1+costs[i-1][j]){
	 aPathRev.append(a[i-=1]);
	 bPathRev.append("-");
      } else if (costs[i][j] == 1+costs[i][j-1]){
	 aPathRev.append("-");
	 bPathRev.append(b[j-=1]);
      }
   }
   return(aPathRev.text.reverse(), bPathRev.text.reverse())
}
```


```zkl
result := alignment("rosettacode", "raisethysword");
println(result[0]);
println(result[1]);
```

```txt

r-oset-tacode
raisethysword

```

