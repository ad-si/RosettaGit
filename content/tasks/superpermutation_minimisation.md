+++
title = "Superpermutation minimisation"
description = ""
date = 2019-07-27T18:57:22Z
aliases = []
[extra]
id = 18234
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "awk",
  "c",
  "d",
  "elixir",
  "freebasic",
  "go",
  "j",
  "java",
  "julia",
  "kotlin",
  "objeck",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "sidef",
  "zkl",
]
+++

A superpermutation of N different characters is a string consisting of an arrangement of multiple copies of those N different characters in which every permutation of those characters can be found as a substring.

For example, representing the characters as A..Z, using N=2 we choose to use the first two characters 'AB'.

The permutations of 'AB' are the two, (i.e. two-factorial), strings: 'AB' and 'BA'.

A too obvious method of generating a superpermutation is to just join all the permutations together forming 'ABBA'.

A little thought will produce the shorter (in fact the shortest) superpermutation of 'ABA' - it contains 'AB' at the beginning and contains 'BA' from the middle to the end.

The "too obvious" method of creation generates a string of length N!*N. Using this as a yardstick, the task is to investigate other methods of generating superpermutations of N from 1-to-7 characters, that never generate larger superpermutations.

Show descriptions and comparisons of algorithms used here, and select the "Best" algorithm as being the one generating shorter superpermutations.

The problem of generating the shortest superpermutation for each N ''might'' be NP complete, although the minimal strings for small values of N have been found by brute -force searches.

;Reference:
* [http://www.njohnston.ca/2013/04/the-minimal-superpermutation-problem/ The Minimal Superpermutation Problem]. by Nathaniel Johnston.
* [http://oeis.org/A180632 oeis A180632] gives 0-5 as 0, 1, 3, 9, 33, 153. 6 is thought to be 872.
* [https://www.youtube.com/watch?v=wJGE4aEWc28 Superpermutations - Numberphile]. A video
* [https://www.youtube.com/watch?v=OZzIvl1tbPo Superpermutations: the maths problem solved by 4chan - Standupmaths]. A video of recent (2018) mathematical progress.
* [https://www.youtube.com/watch?v=_tpNuulTeSQ New Superpermutations Discovered!] Standupmaths & Numberphile.




## AWK


```AWK

# syntax: GAWK -f SUPERPERMUTATION_MINIMISATION.AWK
# converted from C
BEGIN {
    arr[0] # prevents fatal: attempt to use scalar 'arr' as an array
    limit = 11
    for (n=0; n<=limit; n++) {
      leng = super_perm(n)
      printf("%2d %d ",n,leng)
#     for (i=0; i<length(arr); i++) { printf(arr[i]) } # un-comment to see the string
      printf("\n")
    }
    exit(0)
}
function fact_sum(n,  f,s,x) {
    f = 1
    s = x = 0
    for (;x<n;) {
      f *= ++x
      s += f
    }
    return(s)
}
function super_perm(n,  i,leng) {
    delete arr
    pos = n
    leng = fact_sum(n)
    for (i=0; i<leng; i++) {
      arr[i] = ""
    }
    for (i=0; i<=n; i++) {
      cnt[i] = i
    }
    for (i=1; i<=n; i++) {
      arr[i-1] = i + "0"
    }
    while (r(n)) { }
    return(leng)
}
function r(n,  c) {
    if (!n) { return(0) }
    c = arr[pos-n]
    if (!--cnt[n]) {
      cnt[n] = n
      if (!r(n-1)) { return(0) }
    }
    arr[pos++] = c
    return(1)
}

```

```txt

 0 0
 1 1
 2 3
 3 9
 4 33
 5 153
 6 873
 7 5913
 8 46233
 9 409113
10 4037913
11 43954713

```


## C

Finding a string whose length follows [https://oeis.org/A007489 OEIS A007489]. Complexity is the length of output string.  It is know to be ''not'' optimal.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX 12
char *super = 0;
int pos, cnt[MAX];

// 1! + 2! + ... + n!
int fact_sum(int n)
{
	int s, x, f;
	for (s = 0, x = 0, f = 1; x < n; f *= ++x, s += f);
	return s;
}

int r(int n)
{
	if (!n) return 0;

	char c = super[pos - n];
	if (!--cnt[n]) {
		cnt[n] = n;
		if (!r(n-1)) return 0;
	}
	super[pos++] = c;
	return 1;
}

void superperm(int n)
{
	int i, len;

	pos = n;
	len = fact_sum(n);
	super = realloc(super, len + 1);
	super[len] = '\0';

	for (i = 0; i <= n; i++) cnt[i] = i;
	for (i = 1; i <= n; i++) super[i - 1] = i + '0';

	while (r(n));
}

int main(void)
{
	int n;
	for (n = 0; n < MAX; n++) {
		printf("superperm(%2d) ", n);
		superperm(n);
		printf("len = %d", (int)strlen(super));
		// uncomment next line to see the string itself
		// printf(": %s", super);
		putchar('\n');
	}

	return 0;
}
```

```txt

superperm( 0) len = 0
superperm( 1) len = 1
superperm( 2) len = 3
superperm( 3) len = 9
superperm( 4) len = 33
superperm( 5) len = 153
superperm( 6) len = 873
superperm( 7) len = 5913
superperm( 8) len = 46233
superperm( 9) len = 409113
superperm(10) len = 4037913
superperm(11) len = 43954713

```



## D

The greedy algorithm from the Python entry. This is a little more complex than the Python code because it uses some helper arrays to avoid some allocations inside the loops, to increase performance.

```d
import std.stdio, std.ascii, std.algorithm, core.memory, permutations2;

/** Uses greedy algorithm of adding another char (or two, or three, ...)
until an unseen perm is formed in the last n chars. */
string superpermutation(in uint n) nothrow
in {
    assert(n > 0 && n < uppercase.length);
} out(result) {
    // It's a superpermutation.
    assert(uppercase[0 .. n].dup.permutations.all!(p => result.canFind(p)));
} body {
    string result = uppercase[0 .. n];

    bool[const char[]] toFind;
    GC.disable;
    foreach (const perm; result.dup.permutations)
        toFind[perm] = true;
    GC.enable;
    toFind.remove(result);

    auto trialPerm = new char[n];
    auto auxAdd = new char[n];

    while (toFind.length) {
        MIDDLE: foreach (immutable skip; 1 .. n) {
            auxAdd[0 .. skip] = result[$ - n .. $ - n + skip];
            foreach (const trialAdd; auxAdd[0 .. skip].permutations!false) {
                trialPerm[0 .. n - skip] = result[$ + skip - n .. $];
                trialPerm[n - skip .. $] = trialAdd[];
                if (trialPerm in toFind) {
                    result ~= trialAdd;
                    toFind.remove(trialPerm);
                    break MIDDLE;
                }
            }
        }
    }

    return result;
}

void main() {
    foreach (immutable n; 1 .. 8)
        n.superpermutation.length.writeln;
}
```

```txt
1
3
9
35
164
932
6247
```

Using the ldc2 compiler with n=10, it finds the result string of length 4_235_533 in less than 9 seconds.


### Faster Version

From the C version with some improvements.

```d
import std.stdio, std.range, std.algorithm, std.ascii;

enum uint nMax = 12;

__gshared char[] superperm;
__gshared uint pos;
__gshared uint[nMax] count;

/// factSum(n) = 1! + 2! + ... + n!
uint factSum(in uint n) pure nothrow @nogc @safe {
    return iota(1, n + 1).map!(m => reduce!q{ a * b }(1u, iota(1, m + 1))).sum;
}

bool r(in uint n) nothrow @nogc {
    if (!n)
        return false;

    immutable c = superperm[pos - n];
    if (!--count[n]) {
        count[n] = n;
        if (!r(n - 1))
            return false;
    }
    superperm[pos++] = c;
    return true;
}

void superPerm(in uint n) nothrow {
    static immutable chars = digits ~ uppercase;
    static assert(chars.length >= nMax);
    pos = n;
    superperm.length = factSum(n);

    foreach (immutable i; 0 .. n + 1)
        count[i] = i;
    foreach (immutable i; 1 .. n + 1)
        superperm[i - 1] = chars[i];

    while (r(n)) {}
}

void main() {
    foreach (immutable n; 0 .. nMax) {
        superPerm(n);
        writef("superPerm(%2d) len = %d", n, superperm.length);
        // Use -version=doPrint to see the string itself.
        version (doPrint) write(": ", superperm);
        writeln;
    }
}
```

```txt
superPerm( 0) len = 0
superPerm( 1) len = 1
superPerm( 2) len = 3
superPerm( 3) len = 9
superPerm( 4) len = 33
superPerm( 5) len = 153
superPerm( 6) len = 873
superPerm( 7) len = 5913
superPerm( 8) len = 46233
superPerm( 9) len = 409113
superPerm(10) len = 4037913
superPerm(11) len = 43954713
```

Run-time: about 0.40 seconds.


## Elixir

```elixir
defmodule Superpermutation do
  def minimisation(1), do: [1]
  def minimisation(n) do
    Enum.chunk(minimisation(n-1), n-1, 1)
    |> Enum.reduce({[],nil}, fn sub,{acc,last} ->
      if Enum.uniq(sub) == sub do
        i = if acc==[], do: 0, else: Enum.find_index(sub, &(&1==last)) + 1
        {acc ++ (Enum.drop(sub,i) ++ [n] ++ sub), List.last(sub)}
      else
        {acc, last}
      end
    end)
    |> elem(0)
  end
end

to_s = fn list -> Enum.map_join(list, &Integer.to_string(&1,16)) end
Enum.each(1..8, fn n ->
  result = Superpermutation.minimisation(n)
  :io.format "~3w: len =~8w : ", [n, length(result)]
  IO.puts if n<5, do: Enum.join(result),
                else: to_s.(Enum.take(result,20)) <> "...." <> to_s.(Enum.slice(result,-20..-1))
end)
```


```txt

  1: len =       1 : 1
  2: len =       3 : 121
  3: len =       9 : 123121321
  4: len =      33 : 123412314231243121342132413214321
  5: len =     153 : 12345123415234125341....14352143251432154321
  6: len =     873 : 12345612345162345126....62154326154321654321
  7: len =    5913 : 12345671234561723456....65432716543217654321
  8: len =   46233 : 12345678123456718234....43281765432187654321

```




## FreeBASIC


```freebasic
' version 28-06-2018
' compile with: fbc -s console

Function superpermsize(n As UInteger) As UInteger

    Dim As UInteger x, y, sum, fac
    For x = 1 To n
        fac = 1
        For y = 1 To x
            fac *= y
        Next
        sum += fac
    Next

    Function = sum

End Function

Function superperm(n As UInteger) As String

    If n = 1 Then Return "1"

    Dim As String sup_perm = "1", insert
    Dim As String p, q()
    Dim As UInteger a, b, i, l, x

    For x = 2 To n
        insert = IIf(x < 10, Str(x), Chr(x + 55))
        l = Len(sup_perm)
        If l > 1 Then l = Len(sup_perm) - x +2
        ReDim q(l)
        For i = 1 To l
            p = Mid(sup_perm, i, x -1)
            If x > 2 Then
            For a = 0 To Len(p) -2
                For b = a+1 To Len(p) -1
                    If p[a] = p[b] Then Continue For, For, For
                Next
            Next
            End If
            q(i) = p + insert + p
        Next
        sup_perm = q(1)
        For i = 2 To UBound(q)
            a = x -1
            Do
                If Right(sup_perm, a) = Left(q(i), a) Then
                    sup_perm += Mid(q(i), a +1)
                    Exit Do
                End If
                a -= 1
            Loop
        Next
    Next

    Function = sup_perm

End Function

' ------=< MAIN >=------

Dim As String superpermutation
Dim As UInteger n

For n = 1 To 10
    superpermutation = superperm(n)
    Print Using "### ######## ########   "; n; superpermsize(n); Len(superpermutation);
    If n < 5 Then
        Print superpermutation
    Else
        Print
    End If
Next

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
  1        1        1   1
  2        3        3   121
  3        9        9   123121321
  4       33       33   123412314231243121342132413214321
  5      153      153
  6      873      873
  7     5913     5913
  8    46233    46233
  9   409113   409113
 10  4037913  4037913
```



## Go

```go
package main

import "fmt"

const max = 12

var (
    super []byte
    pos   int
    cnt   [max]int
)

// 1! + 2! + ... + n!
func factSum(n int) int {
    s := 0
    for x, f := 0, 1; x < n; {
        x++
        f *= x
        s += f
    }
    return s
}

func r(n int) bool {
    if n == 0 {
        return false
    }
    c := super[pos-n]
    cnt[n]--
    if cnt[n] == 0 {
        cnt[n] = n
        if !r(n - 1) {
            return false
        }
    }
    super[pos] = c
    pos++
    return true
}

func superperm(n int) {
    pos = n
    le := factSum(n)
    super = make([]byte, le)
    for i := 0; i <= n; i++ {
        cnt[i] = i
    }
    for i := 1; i <= n; i++ {
        super[i-1] = byte(i) + '0'
    }

    for r(n) {
    }
}

func main() {
    for n := 0; n < max; n++ {
        fmt.Printf("superperm(%2d) ", n)
        superperm(n)
        fmt.Printf("len = %d\n", len(super))
    }
}
```


```txt

superperm( 0) len = 0
superperm( 1) len = 1
superperm( 2) len = 3
superperm( 3) len = 9
superperm( 4) len = 33
superperm( 5) len = 153
superperm( 6) len = 873
superperm( 7) len = 5913
superperm( 8) len = 46233
superperm( 9) len = 409113
superperm(10) len = 4037913
superperm(11) len = 43954713

```



## J


If there's an 872 long superpermutation for a six letter alphabet, this is not optimal.


```J
approxmin=:3 :0
  seqs=. y{~(A.&i.~ !)#y
  r=.{.seqs
  seqs=.}.seqs
  while.#seqs do.
    for_n. i.-#y do.
      tail=. (-n){. r
      b=. tail -:"1 n{."1 seqs
      if. 1 e.b do.
        j=. b i.1
        r=. r, n}.j{seqs
        seqs=. (<<<j) { seqs
        break.
      end.
    end.
  end.
  r
)
```


Some sequence lengths:


```J
   (#, #@approxmin)@> (1+i.8) {.&.> <'abcdefghijk'
1     1
2     3
3     9
4    33
5   153
6   873
7  5913
8 46233
```



## Java

Translation of [[Superpermutation_minimisation#C|C]] via [[Superpermutation_minimisation#D|D]]
```java
import static java.util.stream.IntStream.rangeClosed;

public class Test {
    final static int nMax = 12;

    static char[] superperm;
    static int pos;
    static int[] count = new int[nMax];

    static int factSum(int n) {
        return rangeClosed(1, n)
                .map(m -> rangeClosed(1, m).reduce(1, (a, b) -> a * b)).sum();
    }

    static boolean r(int n) {
        if (n == 0)
            return false;

        char c = superperm[pos - n];
        if (--count[n] == 0) {
            count[n] = n;
            if (!r(n - 1))
                return false;
        }
        superperm[pos++] = c;
        return true;
    }

    static void superPerm(int n) {
        String chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

        pos = n;
        superperm = new char[factSum(n)];

        for (int i = 0; i < n + 1; i++)
            count[i] = i;
        for (int i = 1; i < n + 1; i++)
            superperm[i - 1] = chars.charAt(i);

        while (r(n)) {
        }
    }

    public static void main(String[] args) {
        for (int n = 0; n < nMax; n++) {
            superPerm(n);
            System.out.printf("superPerm(%2d) len = %d", n, superperm.length);
            System.out.println();
        }
    }
}
```



```txt
superPerm( 0) len = 0
superPerm( 1) len = 1
superPerm( 2) len = 3
superPerm( 3) len = 9
superPerm( 4) len = 33
superPerm( 5) len = 153
superPerm( 6) len = 873
superPerm( 7) len = 5913
superPerm( 8) len = 46233
superPerm( 9) len = 409113
superPerm(10) len = 4037913
superPerm(11) len = 43954713
```




## Julia

Runs in about 1/4 second.

```julia
const nmax = 12

function r!(n, s, pos, count)
    if n == 0
        return false
    end
    c = s[pos + 1 - n]
    count[n + 1] -= 1
    if count[n + 1] == 0
        count[n + 1] = n
        if r!(n - 1, s, pos, count) == 0
            return false
        end
    end
    s[pos + 1] = c
    pos += 1
    true
end

function superpermutation(n)
    count = zeros(nmax)
    pos = n
    superperm = zeros(UInt8, n < 2 ? n : mapreduce(factorial, +, 1:n))
    for i in 0:n-1
        count[i + 1] = i
        superperm[i + 1] = Char(i + '0')
    end
    count[n + 1] = n
    while r!(n, superperm, pos, count) ; end
    superperm
end

function testsuper(N, verbose=false)
    for i in 0:N-1
        s = superpermutation(i)
        println("Superperm($i) has length $(length(s)) ", (verbose ? String(s) : ""))
    end
end

testsuper(nmax)

```
```txt

Superperm(0) has length 0
Superperm(1) has length 1
Superperm(2) has length 3
Superperm(3) has length 9
Superperm(4) has length 33
Superperm(5) has length 153
Superperm(6) has length 873
Superperm(7) has length 5913
Superperm(8) has length 46233
Superperm(9) has length 409113
Superperm(10) has length 4037913
Superperm(11) has length 43954713

```



## Kotlin

```scala
// version 1.1.2

const val MAX = 12

var sp = CharArray(0)
val count = IntArray(MAX)
var pos = 0

fun factSum(n: Int): Int {
    var s = 0
    var x = 0
    var f = 1
    while (x < n) {
        f *= ++x
        s += f
    }
    return s
}

fun r(n: Int): Boolean {
    if (n == 0) return false
    val c = sp[pos - n]
    if (--count[n] == 0) {
        count[n] = n
        if (!r(n - 1)) return false
    }
    sp[pos++] = c
    return true
}

fun superPerm(n: Int) {
    pos = n
    val len = factSum(n)
    if (len > 0) sp = CharArray(len)
    for (i in 0..n) count[i] = i
    for (i in 1..n) sp[i - 1] = '0' + i
    while (r(n)) {}
}

fun main(args: Array<String>) {
    for (n in 0 until MAX) {
        superPerm(n)
        println("superPerm(${"%2d".format(n)}) len = ${sp.size}")
    }
}
```


```txt

superPerm( 0) len = 0
superPerm( 1) len = 1
superPerm( 2) len = 3
superPerm( 3) len = 9
superPerm( 4) len = 33
superPerm( 5) len = 153
superPerm( 6) len = 873
superPerm( 7) len = 5913
superPerm( 8) len = 46233
superPerm( 9) len = 409113
superPerm(10) len = 4037913
superPerm(11) len = 43954713

```



## Objeck

```objeck
class SuperPermutation {
  @super : static : Char[];
  @pos : static : Int;
  @cnt : static : Int[];

  function : Main(args : String[]) ~ Nil {
    max := 12;
    @cnt := Int->New[max];
    @super := Char->New[0];

    for(n := 0; n < max; n += 1;) {
      "superperm({$n}) "->Print();
      SuperPerm(n);
      len := @super->Size() - 1;
      "len = {$len}"->PrintLine();
    };
  }

  function : native : FactSum(n : Int) ~ Int {
    s := 0; x := 0; f := 1;
    while(x < n) {
      f *= ++x; s += f;
    };
    return s;
  }

  function : native : R(n : Int) ~ Bool {
    if(n = 0) {
      return false;
     };

    c := @super[@pos - n];
    if(--@cnt[n] = 0) {
      @cnt[n] := n;
      if(<>R(n - 1)) {
        return false;
      };
    };
    @super[@pos++] := c;

    return true;
  }

  function : SuperPerm(n : Int) ~ Nil {
    @pos := n;
    len := FactSum(n);

    tmp := Char->New[len + 1];
    Runtime->Copy(tmp, 0, @super, 0, @super->Size());
    @super := tmp;

    for(i := 0; i <= n; i += 1;) {
      @cnt[i] := i;
    };

    for(i := 1; i <= n; i += 1;) {
      @super[i - 1] := i + '0';
    };

    do {
      r := R(n);
    }
    while(r);
  }
}

```


```txt

superperm(0) len = 0
superperm(1) len = 1
superperm(2) len = 3
superperm(3) len = 9
superperm(4) len = 33
superperm(5) len = 153
superperm(6) len = 873
superperm(7) len = 5913
superperm(8) len = 46233
superperm(9) len = 409113
superperm(10) len = 4037913
superperm(11) len = 43954713

```



## Perl

This uses a naive method of just concatenating the new permutation to the end (or prepending to the front) if it is not already in the string.  Adding to the end is similar to Python's '''s_perm1()''' function.

```perl
use ntheory qw/forperm/;
for my $len (1..8) {
  my($pre, $post, $t) = ("","");
  forperm {
    $t = join "",@_;
    $post .= $t      unless index($post ,$t) >= 0;
    $pre = $t . $pre unless index($pre, $t) >= 0;
  } $len;
  printf "%2d: %8d %8d\n", $len, length($pre), length($post);
}
```

```txt
 1:        1        1
 2:        4        4
 3:       12       15
 4:       48       64
 5:      240      325
 6:     1440     1956
 7:    10080    13699
 8:    80640   109600
```

The permutations are generated in lexicographic order, and it seems prepending them leads to smaller strings than adding to the end.  These are still quite a bit larger than the heuristic methods.


## Perl 6

```perl
for 1..8 -> $len {
  my $pre = my $post = my $t = '';
  for  ('a'..'z')[^$len].permutations -> @p {
     $t = @p.join('');
     $post ~= $t        unless index($post, $t);
     $pre   = $t ~ $pre unless index($pre,  $t);
  }
  printf "%1d: %8d %8d\n", $len, $pre.chars, $post.chars;
}
```

```txt
1:        1        1
2:        4        4
3:       12       15
4:       48       64
5:      240      325
6:     1440     1956
7:    10080    13699
8:    80640   109600
```



## Phix

```Phix
constant nMax = 12

atom t0 = time()
string superperm
sequence count
integer pos

function factSum(int n)
    integer s = 0, f = 1
    for i=1 to n do
        f *= i
        s += f
    end for
    return s
end function

function r(int n)
    if (n == 0) then return false end if
    integer c = superperm[pos-n+1]
    count[n] -= 1
    if count[n]=0 then
        count[n] = n
        if not r(n-1) then return false end if
    end if
    pos += 1
    superperm[pos] = c
    return true
end function

procedure superPerm(int n)
    string chars = "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"[1..n]
    pos = n
    superperm = chars&repeat(' ',factSum(n)-n)
    count = tagset(n)
    while r(n) do end while
    if n=0 then
        if superperm!="" then ?9/0 end if
    elsif n<=9 then
        -- (I estimate it would take at least 5 days to validate
        --  superPerm(12), feel free to try it on your own time)
        for i=1 to factorial(n) do
            if not match(permute(i,chars),superperm) then ?9/0 end if
        end for
    end if
end procedure

for n=0 to nMax do
    superPerm(n)
    integer l = length(superperm)
    if l>40 then superperm[20..-20] = "..." end if
    string e = elapsed(time()-t0)
    printf(1,"superPerm(%2d) len = %d  %s (%s)\n", {n, l, superperm, e})
end for
```

```txt

superPerm( 0) len = 0   (0s)
superPerm( 1) len = 1  1 (0s)
superPerm( 2) len = 3  121 (0s)
superPerm( 3) len = 9  123121321 (0s)
superPerm( 4) len = 33  123412314231243121342132413214321 (0s)
superPerm( 5) len = 153  1234512341523412534...4352143251432154321 (0s)
superPerm( 6) len = 873  1234561234516234512...2154326154321654321 (0.0s)
superPerm( 7) len = 5913  1234567123456172345...5432716543217654321 (0.7s)
superPerm( 8) len = 46233  1234567812345671823...3281765432187654321 (0.7s)
superPerm( 9) len = 409113  1234567891234567819...9187654321987654321 (0.8s)
superPerm(10) len = 4037913  123456789A123456789...987654321A987654321 (1.2s)
superPerm(11) len = 43954713  123456789AB12345678...87654321BA987654321 (6.5s)
superPerm(12) len = 522956313  123456789ABC1234567...7654321CBA987654321 (1 minute and 09s)

```



### Alternative

Finds the longest overlap, similar to Python's greedy s_perm0 but theoretically more efficient.

I also tried prefixing res with any longer overlap at the start, but it just made things worse.

Uses factSum() from above, and compares that with these results (which are always worse for >3).

```Phix
procedure superPerm(int n)
    atom t0 = time()
    string chars = "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"[1..n]
    integer f = factorial(n)
    sequence perms = repeat("",f)
    for i=1 to f do
        perms[i] = permute(i,chars)
    end for
    string res = perms[$]
    perms = perms[1..$-1]
    while length(perms) do
        integer best = 0, bi = length(perms)
        for i=1 to length(perms) do
            string pi = perms[i]
            integer m = length(res),
                    k = find(res[m],pi)
            for l=k to 1 by -1 do
                if res[m]!=pi[l] then
                    k = 0
                    exit
                end if
                m -= 1
            end for
            if k>best then
                best = k
                bi = i
            end if
        end for
        if match(perms[bi],res) then
            ?9/0 -- (sanity check)
        else
            res &= perms[bi][best+1..$]
        end if
        perms[bi] = perms[$]
        perms = perms[1..$-1]
    end while
    integer lr = length(res)
    integer fsn = factSum(n)
    string op = {"<","=",">"}[compare(lr,fsn)+2]
    t0 = time()-t0
    string e = iff(t0>1?", "&elapsed(t0):"")
    printf(1,"superPerm(%d) len = %d (%s%d%s)\n",{n,lr,op,fsn,e})
end procedure

for n=1 to 7 do     -- (note: 8 takes 65x longer than 7)
    superPerm(n)
end for
```

```txt

superPerm(1) len = 1 (=1)
superPerm(2) len = 3 (=3)
superPerm(3) len = 9 (=9)
superPerm(4) len = 35 (>33)
superPerm(5) len = 162 (>153)
superPerm(6) len = 924 (>873)
superPerm(7) len = 6250 (>5913, 2.5s)
superPerm(8) len = 48703 (>46233, 2 minutes and 43s)

```



## Python


```python
"Generate a short Superpermutation of n characters A... as a string using various algorithms."


from __future__ import print_function, division

from itertools import permutations
from math import factorial
import string
import datetime
import gc



MAXN = 7


def s_perm0(n):
    """
    Uses greedy algorithm of adding another char (or two, or three, ...)
    until an unseen perm is formed in the last n chars
    """
    allchars = string.ascii_uppercase[:n]
    allperms = [''.join(p) for p in permutations(allchars)]
    sp, tofind = allperms[0], set(allperms[1:])
    while tofind:
        for skip in range(1, n):
            for trial_add in (''.join(p) for p in permutations(sp[-n:][:skip])):
                #print(sp, skip, trial_add)
                trial_perm = (sp + trial_add)[-n:]
                if trial_perm in tofind:
                    #print(sp, skip, trial_add)
                    sp += trial_add
                    tofind.discard(trial_perm)
                    trial_add = None    # Sentinel
                    break
            if trial_add is None:
                break
    assert all(perm in sp for perm in allperms) # Check it is a superpermutation
    return sp

def s_perm1(n):
    """
    Uses algorithm of concatenating all perms in order if not already part
    of concatenation.
    """
    allchars = string.ascii_uppercase[:n]
    allperms = [''.join(p) for p in sorted(permutations(allchars))]
    perms, sp = allperms[::], ''
    while perms:
        nxt = perms.pop()
        if nxt not in sp:
            sp += nxt
    assert all(perm in sp for perm in allperms)
    return sp

def s_perm2(n):
    """
    Uses algorithm of concatenating all perms in order first-last-nextfirst-
    nextlast... if not already part of concatenation.
    """
    allchars = string.ascii_uppercase[:n]
    allperms = [''.join(p) for p in sorted(permutations(allchars))]
    perms, sp = allperms[::], ''
    while perms:
        nxt = perms.pop(0)
        if nxt not in sp:
            sp += nxt
        if perms:
            nxt = perms.pop(-1)
            if nxt not in sp:
                sp += nxt
    assert all(perm in sp for perm in allperms)
    return sp

def _s_perm3(n, cmp):
    """
    Uses algorithm of concatenating all perms in order first,
    next_with_LEASTorMOST_chars_in_same_position_as_last_n_chars, ...
    """
    allchars = string.ascii_uppercase[:n]
    allperms = [''.join(p) for p in sorted(permutations(allchars))]
    perms, sp = allperms[::], ''
    while perms:
        lastn = sp[-n:]
        nxt = cmp(perms,
                  key=lambda pm:
                    sum((ch1 == ch2) for ch1, ch2 in zip(pm, lastn)))
        perms.remove(nxt)
        if nxt not in sp:
            sp += nxt
    assert all(perm in sp for perm in allperms)
    return sp

def s_perm3_max(n):
    """
    Uses algorithm of concatenating all perms in order first,
    next_with_MOST_chars_in_same_position_as_last_n_chars, ...
    """
    return _s_perm3(n, max)

def s_perm3_min(n):
    """
    Uses algorithm of concatenating all perms in order first,
    next_with_LEAST_chars_in_same_position_as_last_n_chars, ...
    """
    return _s_perm3(n, min)


longest = [factorial(n) * n for n in range(MAXN + 1)]
weight, runtime = {}, {}
print(__doc__)
for algo in [s_perm0, s_perm1, s_perm2, s_perm3_max, s_perm3_min]:
    print('\n###\n### %s\n###' % algo.__name__)
    print(algo.__doc__)
    weight[algo.__name__], runtime[algo.__name__] = 1, datetime.timedelta(0)
    for n in range(1, MAXN + 1):
        gc.collect()
        gc.disable()
        t = datetime.datetime.now()
        sp = algo(n)
        t = datetime.datetime.now() - t
        gc.enable()
        runtime[algo.__name__] += t
        lensp = len(sp)
        wt = (lensp / longest[n]) ** 2
        print('  For N=%i: SP length %5i Max: %5i Weight: %5.2f'
              % (n, lensp, longest[n], wt))
        weight[algo.__name__] *= wt
    weight[algo.__name__] **= 1 / n  # Geometric mean
    weight[algo.__name__] = 1 / weight[algo.__name__]
    print('%*s Overall Weight: %5.2f in %.1f seconds.'
          % (29, '', weight[algo.__name__], runtime[algo.__name__].total_seconds()))

print('\n###\n### Algorithms ordered by shortest superpermutations first\n###')
print('\n'.join('%12s (%.3f)' % kv for kv in
                sorted(weight.items(), key=lambda keyvalue: -keyvalue[1])))

print('\n###\n### Algorithms ordered by shortest runtime first\n###')
print('\n'.join('%12s (%.3f)' % (k, v.total_seconds()) for k, v in
                sorted(runtime.items(), key=lambda keyvalue: keyvalue[1])))

```


```txt
Generate a short Superpermutation of n characters A... as a string using various algorithms.

###
### s_perm0
###

    Uses greedy algorithm of adding another char (or two, or three, ...)
    until an unseen perm is formed in the last n chars

  For N=1: SP length     1 Max:     1 Weight:  1.00
  For N=2: SP length     3 Max:     4 Weight:  0.56
  For N=3: SP length     9 Max:    18 Weight:  0.25
  For N=4: SP length    35 Max:    96 Weight:  0.13
  For N=5: SP length   164 Max:   600 Weight:  0.07
  For N=6: SP length   932 Max:  4320 Weight:  0.05
  For N=7: SP length  6247 Max: 35280 Weight:  0.03
                              Overall Weight:  6.50 in 0.1 seconds.

###
### s_perm1
###

    Uses algorithm of concatenating all perms in order if not already part
    of concatenation.

  For N=1: SP length     1 Max:     1 Weight:  1.00
  For N=2: SP length     4 Max:     4 Weight:  1.00
  For N=3: SP length    15 Max:    18 Weight:  0.69
  For N=4: SP length    64 Max:    96 Weight:  0.44
  For N=5: SP length   325 Max:   600 Weight:  0.29
  For N=6: SP length  1956 Max:  4320 Weight:  0.21
  For N=7: SP length 13699 Max: 35280 Weight:  0.15
                              Overall Weight:  2.32 in 0.1 seconds.

###
### s_perm2
###

    Uses algorithm of concatenating all perms in order first-last-nextfirst-
    nextlast... if not already part of concatenation.

  For N=1: SP length     1 Max:     1 Weight:  1.00
  For N=2: SP length     4 Max:     4 Weight:  1.00
  For N=3: SP length    15 Max:    18 Weight:  0.69
  For N=4: SP length    76 Max:    96 Weight:  0.63
  For N=5: SP length   420 Max:   600 Weight:  0.49
  For N=6: SP length  3258 Max:  4320 Weight:  0.57
  For N=7: SP length 24836 Max: 35280 Weight:  0.50
                              Overall Weight:  1.49 in 0.3 seconds.

###
### s_perm3_max
###

    Uses algorithm of concatenating all perms in order first,
    next_with_MOST_chars_in_same_position_as_last_n_chars, ...

  For N=1: SP length     1 Max:     1 Weight:  1.00
  For N=2: SP length     4 Max:     4 Weight:  1.00
  For N=3: SP length    15 Max:    18 Weight:  0.69
  For N=4: SP length    56 Max:    96 Weight:  0.34
  For N=5: SP length   250 Max:   600 Weight:  0.17
  For N=6: SP length  1482 Max:  4320 Weight:  0.12
  For N=7: SP length 10164 Max: 35280 Weight:  0.08
                              Overall Weight:  3.06 in 50.2 seconds.

###
### s_perm3_min
###

    Uses algorithm of concatenating all perms in order first,
    next_with_LEAST_chars_in_same_position_as_last_n_chars, ...

  For N=1: SP length     1 Max:     1 Weight:  1.00
  For N=2: SP length     4 Max:     4 Weight:  1.00
  For N=3: SP length    15 Max:    18 Weight:  0.69
  For N=4: SP length    88 Max:    96 Weight:  0.84
  For N=5: SP length   540 Max:   600 Weight:  0.81
  For N=6: SP length  3930 Max:  4320 Weight:  0.83
  For N=7: SP length 33117 Max: 35280 Weight:  0.88
                              Overall Weight:  1.16 in 49.8 seconds.

###
### Algorithms ordered by shortest superpermutations first
###
     s_perm0 (6.501)
 s_perm3_max (3.057)
     s_perm1 (2.316)
     s_perm2 (1.494)
 s_perm3_min (1.164)

###
### Algorithms ordered by shortest runtime first
###
     s_perm0 (0.099)
     s_perm1 (0.102)
     s_perm2 (0.347)
 s_perm3_min (49.764)
 s_perm3_max (50.192)
```



### Alternative Version

```python
from array import array
from string import ascii_uppercase, digits
from operator import mul

try:
    import psyco
    psyco.full()
except:
    pass

N_MAX = 12

# fact_sum(n) = 1! + 2! + ... + n!
def fact_sum(n):
    return sum(reduce(mul, xrange(1, m + 1), 1) for m in xrange(1, n + 1))


def r(n, superperm, pos, count):
    if not n:
        return False

    c = superperm[pos - n]
    count[n] -= 1
    if not count[n]:
        count[n] = n
        if not r(n - 1, superperm, pos, count):
            return False

    superperm[pos] = c
    pos += 1
    return True


def super_perm(n, superperm, pos, count, chars = digits + ascii_uppercase):
    assert len(chars) >= N_MAX
    pos = n
    superperm += array("c", " ") * (fact_sum(n) - len(superperm))

    for i in xrange(n + 1):
        count[i] = i
    for i in xrange(1, n + 1):
        superperm[i - 1] = chars[i]

    while r(n, superperm, pos, count):
        pass


def main():
    superperm = array("c", "")
    pos = 0
    count = array("l", [0]) * N_MAX

    for n in xrange(N_MAX):
        super_perm(n, superperm, pos, count)
        print "Super perm(%2d) len = %d" % (n, len(superperm)),
        #print superperm.tostring(),
        print

main()
```

It is four times slower than the D entry. The output is about the same as the D entry.


## Racket

```racket
#lang racket/base
(require racket/list racket/format)

(define (index-of1 x l) (for/first ((i (in-naturals 1)) (m (in-list l)) #:when (equal? m x)) i))

(define (sprprm n)
  (define n-1 (- n 1))
  (define sp:n-1 (superperm n-1))
  (let loop ((subs (let loop ((sp sp:n-1) (i (- (length sp:n-1) n-1 -1)) (rv null))
                     (cond
                       [(zero? i) (reverse rv)]
                       [else
                        (define sub (take sp n-1))
                        (loop (cdr sp)
                              (- i 1)
                              (if (check-duplicates sub) rv (cons sub rv)))])))
             (ary null))
    (if (null? subs)
        ary
        (let ((sub (car subs)))
          (define i (if (null? ary) 0 (index-of1 (last ary) sub)))
          (loop (cdr subs) (append ary (drop sub i) (list n) sub))))))

(define superperm
  (let ((hsh (make-hash (list (cons 1 (list 1))))))
    (lambda (n) (hash-ref! hsh n (lambda () (sprprm n))))))


(define (20..20 ary)
  (if (< (length ary) 41) ary (append (take ary 20) (cons '.. (take-right ary 20)))))

(for* ((n (in-range 1 (add1 8))) (ary (in-value (superperm n))))
  (printf "~a: len = ~a : ~a~%" (~a n #:width 3) (~a (length ary) #:width 8) (20..20 ary)))
```


```txt
1  : len = 1        : (1)
2  : len = 3        : (1 2 1)
3  : len = 9        : (1 2 3 1 2 1 3 2 1)
4  : len = 33       : (1 2 3 4 1 2 3 1 4 2 3 1 2 4 3 1 2 1 3 4 2 1 3 2 4 1 3 2 1 4 3 2 1)
5  : len = 153      : (1 2 3 4 5 1 2 3 4 1 5 2 3 4 1 2 5 3 4 1 .. 1 4 3 5 2 1 4 3 2 5 1 4 3 2 1 5 4 3 2 1)
6  : len = 873      : (1 2 3 4 5 6 1 2 3 4 5 1 6 2 3 4 5 1 2 6 .. 6 2 1 5 4 3 2 6 1 5 4 3 2 1 6 5 4 3 2 1)
7  : len = 5913     : (1 2 3 4 5 6 7 1 2 3 4 5 6 1 7 2 3 4 5 6 .. 6 5 4 3 2 7 1 6 5 4 3 2 1 7 6 5 4 3 2 1)
8  : len = 46233    : (1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 1 8 2 3 4 .. 4 3 2 8 1 7 6 5 4 3 2 1 8 7 6 5 4 3 2 1)
```



## REXX


### version 1

This REXX version just does simple finds for the permutations.

```rexx
/*REXX program attempts  to find better  minimizations  for computing superpermutations.*/
parse arg cycles .                               /*obtain optional arguments from the CL*/
if cycles=='' | cycles==","  then cycles=7       /*Not specified?  Then use the default.*/

      do n=0  to  cycles
      #=0;                           $.=         /*populate the first permutation.      */
              do pop=1  for n;       @.pop=d2x(pop);       $.0=$.0 || @.pop;  end  /*pop*/

              do  while aPerm(n, 0)
              if n\==0  then #=#+1;  $.#=;     do j=1  for n; $.#=$.# || @.j; end  /*j*/
              end  /*while*/
      z=$.0
      nm=n-1
              do ?=1  for #;  if $.j==''  then iterate;   if pos($.?, z)\==0  then iterate
              parse  var  $.?  h  2  R  1  L  =(n)
              if  left(z, nm)==R  then do;   z=h || z;   iterate;   end
              if right(z,  1)==h  then do;   z=z || R;   iterate;   end
              z=z || $.?
              end   /*?*/                        /* [↑]  more IFs could be added for opt*/

       say 'length of superpermutation('n") ="  length(z)
       end   /*cycle*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
aPerm: procedure expose @.;     parse arg n,i;    nm=n-1;  if n==0  then return 0
           do k=nm  by -1  for nm; kp=k+1; if @.k<@.kp  then do; i=k;leave; end; end /*k*/
           do j=i+1  while  j<n;  parse value  @.j @.n  with  @.n @.j;    n=n-1; end /*j*/
       if i==0  then return 0
           do m=i+1  while @.m<@.i; end /*m*/;    parse value  @.m  @.i   with   @.i  @.m
       return 1
```

```txt

length of superpermutation(0) = 0
length of superpermutation(1) = 1
length of superpermutation(2) = 2
length of superpermutation(3) = 9
length of superpermutation(4) = 50
length of superpermutation(5) = 302
length of superpermutation(6) = 1922
length of superpermutation(7) = 13652
length of superpermutation(8) = 109538

```



### version 2


```rexx
/*REXX program attempts  to find better  minimizations  for computing superpermutations.*/
parse arg cycles .                               /*obtain optional arguments from the CL*/
if cycles=='' | cycles==","  then cycles=7       /*Not specified?  Then use the default.*/

      do n=0  to  cycles
      #=0;                           $.=         /*populate the first permutation.      */
              do pop=1  for n;       @.pop=d2x(pop);       $.0=$.0 || @.pop;  end  /*pop*/

              do  while aPerm(n,0);
              if n\==0  then #=#+1;  $.#=;     do j=1  for n; $.#=$.# || @.j; end  /*j*/
              end  /*while*/
      z=$.0
c=0                                              /*count of found permutations (so far).*/
          do j=1  while c\==#
          if j>#  then do;  c=c+1                /*exhausted finds and shortcuts; concat*/
                            z=z || $.j;  $.j=
                            j=1
                       end
          if $.j==''         then iterate        /*Already found? Then ignore this perm.*/
          if pos($.j,z)\==0  then do;  c=c+1
                                       $.j=
                                       iterate
                                  end

              do k=n-1  to 1  by -1              /*handle the shortcuts in perm finding.*/
              if substr($.j, k)==left(z, k)  then do;  c=c+1 /*found a rightish shortcut*/
                                                       z=left($.j, k-1) || z;     $.j=
                                                       iterate j
                                                  end
              if left($.j, k) ==right(z, k)  then do;  c=c+1 /*found a  leftish shortcut*/
                                                       z=z || substr($.j, k+1);   $.j=
                                                       iterate j
                                                  end
              end   /*k*/                        /* [↑]  more IFs could be added for opt*/
           end      /*j*/
       say 'length of superpermutation('n") ="  length(z)
       end   /*cycle*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
aPerm: procedure expose @.;     parse arg n,i;    nm=n-1;  if n==0  then return 0
           do k=nm  by -1  for nm; kp=k+1; if @.k<@.kp  then do; i=k;leave; end; end /*k*/
           do j=i+1  while  j<n;  parse value  @.j @.n  with  @.n @.j;    n=n-1; end /*j*/
       if i==0  then return 0
           do m=i+1  while @.m<@.i; end /*m*/;   parse value  @.m @.i  with  @.i @.m
       return 1
```

```txt

superpermutation(0) = 0
superpermutation(1) = 1
superpermutation(2) = 3
superpermutation(3) = 9
superpermutation(4) = 35
superpermutation(5) = 183
superpermutation(6) = 1411
superpermutation(7) = 12137

```



## Ruby


### Non Recursive Version


```ruby
#A straight forward implementation of N. Johnston's algorithm. I prefer to look at this as 2n+1 where
#the second n is first n reversed, and the 1 is always the second symbol. This algorithm will generate
#just the left half of the result by setting l to [1,2] and looping from 3 to 6. For the purpose of
#this task I am going to start from an empty array and generate the whole strings using just the
#rules.
#
#Nigel Galloway: December 16th., 2014
#
l = []
(1..6).each{|e|
  a, i = [], e-2
  (0..l.length-e+1).each{|g|
     if not (n = l[g..g+e-2]).uniq!
       a.concat(n[(a[0]? i : 0)..-1]).push(e).concat(n)
       i = e-2
     else
       i -= 1
     end
   }
   a.each{|n| print n}; puts "\n\n"
   l = a
}
```

```txt
1

121

123121321

123412314231243121342132413214321

123451234152341253412354123145231425314235142315423124531243512431524312543121345213425134215342135421324513241532413524132541321453214352143251432154321

123456123451623451263451236451234651234156234152634152364152346152341652341256341253641253461253416253412653412356412354612354162354126354123654123145623145263145236145231645231465231425631425361425316425314625314265314235614235164235146235142635142365142315642315462315426315423615423165423124563124536124531624531264531246531243561243516243512643512463512436512431562431526431524631524361524316524312564312546312543612543162543126543121345621345261345216345213645213465213425613425163425136425134625134265134215634215364215346215342615342165342135642135462135426135421635421365421324561324516324513624513264513246513241563241536241532641532461532416532413562413526413524613524163524136524132564132546132541632541362541326541321456321453621453261453216453214653214356214352614352164352146352143652143256143251643251463251436251432651432156432154632154362154326154321654321
```


### Recursive Version


```ruby
def superperm(n)
  return [1] if n==1
  superperm(n-1).each_cons(n-1).with_object([]) do |sub, ary|
    next if sub.uniq!
    i = ary.empty? ? 0 : sub.index(ary.last)+1
    ary.concat(sub[i..-1] + [n] + sub)
  end
end

def to_16(a) a.map{|x| x.to_s(16)}.join end

for n in 1..10
  ary = superperm(n)
  print "%3d: len =%8d :" % [n, ary.size]
  puts n<5 ? ary.join : to_16(ary.first(20)) + "...." + to_16(ary.last(20))
end
```

  1: len =       1 :1
  2: len =       3 :121
  3: len =       9 :123121321
  4: len =      33 :123412314231243121342132413214321
  5: len =     153 :12345123415234125341....14352143251432154321
  6: len =     873 :12345612345162345126....62154326154321654321
  7: len =    5913 :12345671234561723456....65432716543217654321
  8: len =   46233 :12345678123456718234....43281765432187654321
  9: len =  409113 :12345678912345678192....29187654321987654321
 10: len = 4037913 :123456789a1234567891....1987654321a987654321

## Scala


```Scala
object SuperpermutationMinimisation extends App {
  val nMax = 12

  @annotation.tailrec
  def factorial(number: Int, acc: Long = 1): Long =
    if (number == 0) acc else factorial(number - 1, acc * number)

  def factSum(n: Int): Long = (1 to n).map(factorial(_)).sum

  for (n <- 0 until nMax) println(f"superPerm($n%2d) len = ${factSum(n)}%d")

}
```



## Sidef

```ruby
for len in (1..8) {
    var (pre="", post="")
    @^len -> permutations {|*p|
        var t = p.join
        post.append!(t) if !post.contains(t)
        pre.prepend!(t) if !pre.contains(t)
    }
    printf("%2d: %8d %8d\n", len, pre.len, post.len)
}
```

```txt

 1:        1        1
 2:        4        4
 3:       12       15
 4:       48       64
 5:      240      325
 6:     1440     1956
 7:    10080    13699
 8:    80640   109600

```



## zkl

It crawls ...

```zkl
const MAX = 12;
var super=Data(), pos, cnt;  // global state, ick

fcn fact_sum(n){ // -->1! + 2! + ... + n!
   [1..n].reduce(fcn(s,n){ s + [2..n].reduce('*,1) },0)
}

fcn r(n){
   if (not n) return(0);

   c := super[pos - n];
   if (not (cnt[n]-=1)){
      cnt[n] = n;
      if (not r(n-1)) return(0);
   }
   super[pos] = c; pos+=1;
   1
}

fcn superperm(n){
   pos = n;
   len := fact_sum(n);
   super.fill(0,len);  // this is pretty close to recalloc()

   cnt = (n+1).pump(List()); //-->(0,1,2,3,..n)
   foreach i in (n){ super[i] = i + 0x31; } //-->"1" ... "123456789:;"
   while (r(n)){}
}

foreach n in (MAX){
   superperm(n);
   print("superperm(%2d) len = %d".fmt(n,super.len()));
   // uncomment next line to see the string itself
   //print(": %s".fmt(super.text));
   println();
}
```

```txt

superperm( 0) len = 0:
superperm( 1) len = 1: 1
superperm( 2) len = 3: 121
superperm( 3) len = 9: 123121321
superperm( 4) len = 33: 123412314231243121342132413214321
superperm( 5) len = 153: 123451234152341253412354123145231425314235142315423124531243512431524312543121345213425134215342135421324513241532413524132541321453214352143251432154321
superperm( 6) len = 873
superperm( 7) len = 5913
superperm( 8) len = 46233
superperm( 9) len = 409113
superperm(10) len = 4037913
superperm(11) len = 43954713

```

