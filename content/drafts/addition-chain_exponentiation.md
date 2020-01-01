+++
title = "Addition-chain exponentiation"
description = ""
date = 2019-05-15T20:18:36Z
aliases = []
[extra]
id = 10399
[taxonomies]
categories = []
tags = []
+++

{{draft task|Logic}}{{wikipedia|Addition-chain exponentiation}}
In cases of special objects (such as with [[wp:Matrix (mathematics)|matrices]]) the operation of multiplication can be excessively expensive.  In these cases the operation of multiplication should be avoided or reduced to a minimum.

In [[wp:mathematics|mathematics]] and [[wp:computer science|computer science]], optimal '''addition-chain exponentiation''' is a method of [[wp:exponentiation|exponentiation]] by positive [[wp:integer|integer]] powers that requires a minimal number of multiplications. It works by creating a shortest [[wp:addition chain|addition chain]] that generates the desired exponent. Each exponentiation in the chain can be evaluated by multiplying  two of the earlier exponentiation results.  More generally, ''addition-chain exponentiation'' may also refer to exponentiation by non-minimal addition chains constructed by a variety of algorithms (since a shortest addition chain is very difficult to find).

The shortest addition-chain [[wp:algorithm|algorithm]] requires no more multiplications than [[wp:binary exponentiation|binary exponentiation]] and usually less. The first example of where it does better is for <math> a^{15}</math>, where the binary method needs six multiplies but a shortest addition chain requires only five:

:<math>a^{15} = a \times (a \times [a \times a^2]^2)^2  \!</math>  (binary, 6 multiplications)
:<math>a^{15} = a^3 \times ([a^3]^2)^2  \!</math> (shortest addition chain, 5 multiplications)
On the other hand, the addition-chain method is much more complicated, since the determination of a shortest addition chain seems quite difficult: no efficient optimal methods are currently known for arbitrary exponents, and the related problem of finding a shortest addition chain for a given set of exponents has been proven [[wp:NP-complete|NP-complete]].
{|class=wikitable
|+Table demonstrating how to do ''Exponentiation'' using ''Addition Chains''
|-
!Number of
Multiplications || Actual
Exponentiation || Specific implementation of
''Addition Chains'' to do Exponentiation
|-
|0|| a<sup>1</sup> || a
|-
|1|| a<sup>2</sup> || a × a
|-
|2|| a<sup>3</sup> || a × a × a
|-
|2|| a<sup>4</sup> || (a × a→b) × b
|-
|3|| a<sup>5</sup> || (a × a→b) × b × a
|-
|3|| a<sup>6</sup> || (a × a→b) × b × b
|-
|4|| a<sup>7</sup> || (a × a→b) × b × b × a
|-
|3|| a<sup>8</sup> || ((a × a→b) × b→d) × d
|-
|4|| a<sup>9</sup> || (a × a × a→c) × c × c
|-
|4|| a<sup>10</sup> || ((a × a→b) × b→d) × d × b
|-
|5|| a<sup>11</sup> || ((a × a→b) × b→d) × d × b × a
|-
|4|| a<sup>12</sup> || ((a × a→b) × b→d) × d × d
|-
|5|| a<sup>13</sup> || ((a × a→b) × b→d) × d × d × a
|-
|5|| a<sup>14</sup> || ((a × a→b) × b→d) × d × d × b
|-
|5|| a<sup>15</sup> || ((a × a→b) × b × a→e) × e × e
|-
|4|| a<sup>16</sup> || (((a × a→b) × b→d) × d→h) × h
|}
The number of multiplications required follows this sequence:
0, 1, 2, 2, 3, 3, 4, 3, 4, 4,  5, 4, 5, 5, 5, 4, 5, 5, 6, 5,
6, 6, 6, 5, 6, 6, 6, 6, 7, 6,  7, 5, 6, 6, 7, 6, 7, 7, 7, 6,
7, 7, 7, 7, 7, 7, 8, 6, 7, 7,  7, 7, 8, 7, 8, 7, 8, 8, 8, 7,
8, 8, 8, 6, 7, 7, 8, 7, 8, 8,  9, 7, 8, 8, 8, 8, 8, 8, 9, 7,
8, 8, 8, 8, 8, 8, 9, 8, 9, 8,  9, 8, 9, 9, 9, 7, 8, 8, 8, 8...

This sequence can be found at: http://oeis.org/A003313

'''Task requirements:'''

Using the following values:
<math>A=\begin{bmatrix}
  \sqrt{\frac{1}{2}} & 0          &\sqrt{\frac{1}{2}} & 0          & 0 & 0 &\\
   0          &\sqrt{\frac{1}{2}} & 0          &\sqrt{\frac{1}{2}} & 0 & 0 &\\
   0          &\sqrt{\frac{1}{2}} & 0          &-\sqrt{\frac{1}{2}} & 0 & 0 &\\
  -\sqrt{\frac{1}{2}} & 0          &\sqrt{\frac{1}{2}} & 0          & 0 & 0 &\\
   0          & 0          & 0          & 0          & 0 & 1 &\\
   0          & 0          & 0          & 0          & 1 & 0 &\\
\end{bmatrix}</math> <math>m=31415</math> and <math>n=27182</math>

Repeat task [[Matrix-exponentiation operator]], except use '''addition-chain exponentiation''' to better calculate:
:<math> A ^ {m}</math>, <math>A ^ {n}</math> and <math>A ^ {n \times m}</math>.

As an easier alternative to doing the matrix manipulation above, generate the ''addition-chains'' for 12509, 31415 and 27182 and use ''addition-chain exponentiation'' to calculate these two equations:
* 1.00002206445416<sup>31415</sup>
* 1.00002550055251<sup>27182</sup>

Also: Display a count of how many multiplications were done in each case.

Note: There are two ways to approach this task:
* Brute force - try every permutation possible and pick one with the least number of multiplications. If the brute force is a simpler algorithm, then present it as a subtask under the subtitle "Brute force", eg <nowiki>
### Brute Force
</nowiki>.
* Some clever algorithm - the wikipedia page has some hints, subtitle the code with the name of algorithm.

Note: Binary exponentiation does not usually produce the best solution.  Provide only optimal solutions.

[[wp:Kudoc|Kudos]] (κῦδος) for providing a routine that generate sequence A003313 in the output.


Also, see the Rosetta Code task:   [http://rosettacode.org/wiki/Knuth%27s_power_tree
Knuth's power tree].


## C

Using complex instead of matrix.  Requires [[Addition-chain exponentiation/Achain.c|Achain.c]].  It takes a long while to compute the shortest addition chains, such that if you don't have the chain lengths precomputed and stored somewhere, you are probably better off with a binary chain (normally not shortest but very simple to calculate) whatever you intend to use the chains for.

```c
#include <stdio.h>

#include "achain.c" /* not common practice */

/* don't have a C99 compiler atm */
typedef struct {double u, v;} cplx;

inline cplx c_mul(cplx a, cplx b)
{
	cplx c;
	c.u = a.u * b.u - a.v * b.v;
	c.v = a.u * b.v + a.v * b.u;
	return c;
}

cplx chain_expo(cplx x, int n)
{
	int i, j, k, l, e[32];
	cplx v[32];

	l = seq(n, 0, e);

	puts("Exponents:");
	for (i = 0; i <= l; i++)
		printf("%d%c", e[i], i == l ? '\n' : ' ');

	v[0] = x; v[1] = c_mul(x, x);
	for (i = 2; i <= l; i++) {
		for (j = i - 1; j; j--) {
			for (k = j; k >= 0; k--) {
				if (e[k] + e[j] < e[i]) break;
				if (e[k] + e[j] > e[i]) continue;
				v[i] = c_mul(v[j], v[k]);
				j = 1;
				break;
			}
		}
	}
	printf("(%f + i%f)^%d = %f + i%f\n",
		x.u, x.v, n, v[l].u, v[l].v);

	return x;
}

int bin_len(int n)
{
	int r, o;
	for (r = o = -1; n; n >>= 1, r++)
		if (n & 1) o++;
	return r + o;
}

int main()
{
	cplx	r1 = {1.0000254989, 0.0000577896},
		r2 = {1.0000220632, 0.0000500026};
	int n1 = 27182, n2 = 31415, i;

	init();
	puts("Precompute chain lengths");
	seq_len(n2);

	chain_expo(r1, n1);
	chain_expo(r2, n2);
	puts("\nchain lengths: shortest binary");
	printf("%14d %7d %7d\n", n1, seq_len(n1), bin_len(n1));
	printf("%14d %7d %7d\n", n2, seq_len(n2), bin_len(n2));
	for (i = 1; i < 100; i++)
		printf("%14d %7d %7d\n", i, seq_len(i), bin_len(i));
	return 0;
}
```

output

```txt
...
Exponents:
1 2 4 8 10 18 28 46 92 184 212 424 848 1696 3392 6784 13568 27136 27182
(1.000025 + i0.000058)^27182 = -0.000001 + i2.000001
Exponents:
1 2 4 8 16 17 33 49 98 196 392 784 1568 3136 6272 6289 12561 25122 31411 31415
(1.000022 + i0.000050)^31415 = -0.000001 + i2.000000

chain lengths: shortest binary
         27182      18      21
         31415      19      24
             1       0       0
             2       1       1
             3       2       2
             4       2       2
      ...
            89       9       9
            90       8       9
            91       9      10
            92       8       9
            93       9      10
      ...
```



## Go

{{trans|C}}
Though adjusted to deal with matrices rather than complex numbers.

Calculating A ^ (m * n) from scratch using this method would take 'for ever' so I've calculated it instead as (A ^ m) ^ n.

```go
package main

import (
    "fmt"
    "math"
)

const (
    N    = 32
    NMAX = 40000
)

var (
    u     = [N]int{0: 1, 1: 2} // upper bounds
    l     = [N]int{0: 1, 1: 2} // lower bounds
    out   = [N]int{}
    sum   = [N]int{}
    tail  = [N]int{}
    cache = [NMAX + 1]int{2: 1}
    known = 2
    stack = 0
    undo  = [N * N]save{}
)

type save struct {
    p *int
    v int
}

func replace(x *[N]int, i, n int) {
    undo[stack].p = &x[i]
    undo[stack].v = x[i]
    x[i] = n
    stack++
}

func restore(n int) {
    for stack > n {
        stack--
        *undo[stack].p = undo[stack].v
    }
}

/* lower and upper bounds */
func lower(n int, up *int) int {
    if n <= 2 || (n <= NMAX && cache[n] != 0) {
        if up != nil {
            *up = cache[n]
        }
        return cache[n]
    }
    i, o := -1, 0
    for ; n != 0; n, i = n>>1, i+1 {
        if n&1 != 0 {
            o++
        }
    }
    if up != nil {
        i--
        *up = o + i
    }
    for {
        i++
        o >>= 1
        if o == 0 {
            break
        }
    }
    if up == nil {
        return i
    }
    for o = 2; o*o < n; o++ {
        if n%o != 0 {
            continue
        }
        q := cache[o] + cache[n/o]
        if q < *up {
            *up = q
            if q == i {
                break
            }
        }
    }
    if n > 2 {
        if *up > cache[n-2]+1 {
            *up = cache[n-1] + 1
        }
        if *up > cache[n-2]+1 {
            *up = cache[n-2] + 1
        }
    }
    return i
}

func insert(x, pos int) bool {
    save := stack
    if l[pos] > x || u[pos] < x {
        return false
    }
    if l[pos] == x {
        goto replU
    }
    replace(&l, pos, x)
    for i := pos - 1; u[i]*2 < u[i+1]; i-- {
        t := l[i+1] + 1
        if t*2 > u[i] {
            goto bail
        }
        replace(&l, i, t)
    }
    for i := pos + 1; l[i] <= l[i-1]; i++ {
        t := l[i-1] + 1
        if t > u[i] {
            goto bail
        }
        replace(&l, i, t)
    }
replU:
    if u[pos] == x {
        return true
    }
    replace(&u, pos, x)
    for i := pos - 1; u[i] >= u[i+1]; i-- {
        t := u[i+1] - 1
        if t < l[i] {
            goto bail
        }
        replace(&u, i, t)
    }
    for i := pos + 1; u[i] > u[i-1]*2; i++ {
        t := u[i-1] * 2
        if t < l[i] {
            goto bail
        }
        replace(&u, i, t)
    }
    return true
bail:
    restore(save)
    return false
}

func try(p, q, le int) bool {
    pl := cache[p]
    if pl >= le {
        return false
    }
    ql := cache[q]
    if ql >= le {
        return false
    }
    var pu, qu int
    for pl < le && u[pl] < p {
        pl++
    }
    for pu = pl - 1; pu < le-1 && u[pu+1] >= p; pu++ {

    }
    for ql < le && u[ql] < q {
        ql++
    }
    for qu = ql - 1; qu < le-1 && u[qu+1] >= q; qu++ {

    }
    if p != q && pl <= ql {
        pl = ql + 1
    }
    if pl > pu || ql > qu || ql > pu {
        return false
    }
    if out[le] == 0 {
        pu = le - 1
        pl = pu
    }
    ps := stack
    for ; pu >= pl; pu-- {
        if !insert(p, pu) {
            continue
        }
        out[pu]++
        sum[pu] += le
        if p != q {
            qs := stack
            j := qu
            if j >= pu {
                j = pu - 1
            }
            for ; j >= ql; j-- {
                if !insert(q, j) {
                    continue
                }
                out[j]++
                sum[j] += le
                tail[le] = q
                if seqRecur(le - 1) {
                    return true
                }
                restore(qs)
                out[j]--
                sum[j] -= le
            }
        } else {
            out[pu]++
            sum[pu] += le
            tail[le] = p
            if seqRecur(le - 1) {
                return true
            }
            out[pu]--
            sum[pu] -= le
        }
        out[pu]--
        sum[pu] -= le
        restore(ps)
    }
    return false
}

func seqRecur(le int) bool {
    n := l[le]
    if le < 2 {
        return true
    }
    limit := n - 1
    if out[le] == 1 {
        limit = n - tail[sum[le]]
    }
    if limit > u[le-1] {
        limit = u[le-1]
    }
    // Try to break n into p + q, and see if we can insert p, q into
    // list while satisfying bounds.
    p := limit
    for q := n - p; q <= p; q, p = q+1, p-1 {
        if try(p, q, le) {
            return true
        }
    }
    return false
}

func seq(n, le int, buf []int) int {
    if le == 0 {
        le = seqLen(n)
    }
    stack = 0
    l[le], u[le] = n, n
    for i := 0; i <= le; i++ {
        out[i], sum[i] = 0, 0
    }
    for i := 2; i < le; i++ {
        l[i] = l[i-1] + 1
        u[i] = u[i-1] * 2
    }
    for i := le - 1; i > 2; i-- {
        if l[i]*2 < l[i+1] {
            l[i] = (1 + l[i+1]) / 2
        }
        if u[i] >= u[i+1] {
            u[i] = u[i+1] - 1
        }
    }
    if !seqRecur(le) {
        return 0
    }
    if buf != nil {
        for i := 0; i <= le; i++ {
            buf[i] = u[i]
        }
    }
    return le
}

func seqLen(n int) int {
    if n <= known {
        return cache[n]
    }
    // Need all lower n to compute sequence.
    for known+1 < n {
        seqLen(known + 1)
    }
    var ub int
    lb := lower(n, &ub)
    for lb < ub && seq(n, lb, nil) == 0 {
        lb++
    }
    known = n
    if n&1023 == 0 {
        fmt.Printf("Cached %d\n", known)
    }
    cache[n] = lb
    return lb
}

func binLen(n int) int {
    r, o := -1, -1
    for ; n != 0; n, r = n>>1, r+1 {
        if n&1 != 0 {
            o++
        }
    }
    return r + o
}

type(
    vector = []float64
    matrix []vector
)

func (m1 matrix) mul(m2 matrix) matrix {
    rows1, cols1 := len(m1), len(m1[0])
    rows2, cols2 := len(m2), len(m2[0])
    if cols1 != rows2 {
        panic("Matrices cannot be multiplied.")
    }
    result := make(matrix, rows1)
    for i := 0; i < rows1; i++ {
        result[i] = make(vector, cols2)
        for j := 0; j < cols2; j++ {
            for k := 0; k < rows2; k++ {
                result[i][j] += m1[i][k] * m2[k][j]
            }
        }
    }
    return result
}

func (m matrix) pow(n int, printout bool) matrix {
    e := make([]int, N)
    var v [N]matrix
    le := seq(n, 0, e)
    if printout {
        fmt.Println("Addition chain:")
        for i := 0; i <= le; i++ {
            c := ' '
            if i == le {
                c = '\n'
            }
            fmt.Printf("%d%c", e[i], c)
        }
    }
    v[0] = m
    v[1] = m.mul(m)
    for i := 2; i <= le; i++ {
        for j := i - 1; j != 0; j-- {
            for k := j; k >= 0; k-- {
                if e[k]+e[j] < e[i] {
                    break
                }
                if e[k]+e[j] > e[i] {
                    continue
                }
                v[i] = v[j].mul(v[k])
                j = 1
                break
            }
        }
    }
    return v[le]
}

func (m matrix) print() {
    for _, v := range m {
        fmt.Printf("% f\n", v)
    }
    fmt.Println()
}

func main() {
    m := 27182
    n := 31415
    fmt.Println("Precompute chain lengths:")
    seqLen(n)
    rh := math.Sqrt(0.5)
    mx := matrix{
        {rh, 0, rh, 0, 0, 0},
        {0, rh, 0, rh, 0, 0},
        {0, rh, 0, -rh, 0, 0},
        {-rh, 0, rh, 0, 0, 0},
        {0, 0, 0, 0, 0, 1},
        {0, 0, 0, 0, 1, 0},
    }
    fmt.Println("\nThe first 100 terms of A003313 are:")
    for i := 1; i <= 100; i++ {
        fmt.Printf("%d ", seqLen(i))
        if i%10 == 0 {
            fmt.Println()
        }
    }
    exs := [2]int{m, n}
    mxs := [2]matrix{}
    for i, ex := range exs {
        fmt.Println("\nExponent:", ex)
        mxs[i] = mx.pow(ex, true)
        fmt.Printf("A ^ %d:-\n\n", ex)
        mxs[i].print()
        fmt.Println("Number of A/C multiplies:", seqLen(ex))
        fmt.Println("  c.f. Binary multiplies:", binLen(ex))
    }
    fmt.Printf("\nExponent: %d x %d = %d\n", m, n, m*n)
    fmt.Printf("A ^ %d = (A ^ %d) ^ %d:-\n\n", m*n, m, n)
    mx2 := mxs[0].pow(n, false)
    mx2.print()
}
```


{{out}}

```txt

Precompute chain lengths:
Cached 1024
Cached 2048
Cached 3072
....
Cached 28672
Cached 29696
Cached 30720

The first 100 terms of A003313 are:
0 1 2 2 3 3 4 3 4 4
5 4 5 5 5 4 5 5 6 5
6 6 6 5 6 6 6 6 7 6
7 5 6 6 7 6 7 7 7 6
7 7 7 7 7 7 8 6 7 7
7 7 8 7 8 7 8 8 8 7
8 8 8 6 7 7 8 7 8 8
9 7 8 8 8 8 8 8 9 7
8 8 8 8 8 8 9 8 9 8
9 8 9 9 9 7 8 8 8 8

Exponent: 27182
Addition chain:
1 2 4 8 10 18 28 46 92 184 212 424 848 1696 3392 6784 13568 27136 27182
A ^ 27182:-

[-0.500000 -0.500000 -0.500000  0.500000  0.000000  0.000000]
[ 0.500000 -0.500000 -0.500000 -0.500000  0.000000  0.000000]
[-0.500000 -0.500000  0.500000 -0.500000  0.000000  0.000000]
[ 0.500000 -0.500000  0.500000  0.500000  0.000000  0.000000]
[ 0.000000  0.000000  0.000000  0.000000  1.000000  0.000000]
[ 0.000000  0.000000  0.000000  0.000000  0.000000  1.000000]

Number of A/C multiplies: 18
  c.f. Binary multiplies: 21

Exponent: 31415
Addition chain:
1 2 4 8 16 17 33 49 98 196 392 784 1568 3136 6272 6289 12561 25122 31411 31415
A ^ 31415:-

[ 0.707107  0.000000  0.000000 -0.707107  0.000000  0.000000]
[ 0.000000  0.707107  0.707107  0.000000  0.000000  0.000000]
[ 0.707107  0.000000  0.000000  0.707107  0.000000  0.000000]
[ 0.000000  0.707107 -0.707107  0.000000  0.000000  0.000000]
[ 0.000000  0.000000  0.000000  0.000000  0.000000  1.000000]
[ 0.000000  0.000000  0.000000  0.000000  1.000000  0.000000]

Number of A/C multiplies: 19
  c.f. Binary multiplies: 24

Exponent: 27182 x 31415 = 853922530
A ^ 853922530 = (A ^ 27182) ^ 31415:-

[-0.500000  0.500000 -0.500000  0.500000  0.000000  0.000000]
[-0.500000 -0.500000 -0.500000 -0.500000  0.000000  0.000000]
[-0.500000 -0.500000  0.500000  0.500000  0.000000  0.000000]
[ 0.500000 -0.500000 -0.500000  0.500000  0.000000  0.000000]
[ 0.000000  0.000000  0.000000  0.000000  1.000000  0.000000]
[ 0.000000  0.000000  0.000000  0.000000  0.000000  1.000000]

```


Below is the original solution which was ruled inadmissible because it uses 'star chains' and is therefore non-optimal.

I think it should nevertheless be retained as it is an interesting approach and there are other solutions to this task which are based on it.


```go
/*
Continued fraction addition chains, as described in "Efficient computation
of addition chains" by F. Bergeron, J. Berstel, and S. Brlek, published in
Journal de théorie des nombres de Bordeaux, 6 no. 1 (1994), p. 21-38,
accessed at http://www.numdam.org/item?id=JTNB_1994__6_1_21_0.
*/
package main

import (
    "fmt"
    "math"
)

// Representation of addition chains.
// Notes:
// 1. While an []int might represent addition chains in general, the
// techniques here work only with "star" chains, as described in the paper.
// Knowledge that the chains are star chains allows certain optimizations.
// 2. The paper descibes a linked list representation which encodes both
// addends of numbers in the chain.  This allows additional optimizations, but
// for the purposes of the RC task, this simpler representation is adequate.
type starChain []int

// ⊗= operator.  modifies receiver.
func (s1 *starChain) cMul(s2 starChain) {
    p := *s1
    i := len(p)
    n := p[i-1]
    p = append(p, s2[1:]...)
    for ; i < len(p); i++ {
        p[i] *= n
    }
    *s1 = p
}

// ⊕= operator.  modifies receiver.
func (p *starChain) cAdd(j int) {
    c := *p
    *p = append(c, c[len(c)-1]+j)
}

// The γ function described in the paper returns a set of numbers in general,
// but certain γ functions return only singletons.  The dichotomic strategy
// is one of these and gives good results so it is the one used for the
// RC task.  Defining the package variable γ to be a singleton allows some
// simplifications in the code.
var γ singleton

type singleton func(int) int

func dichotomic(n int) int {
    return n / (1 << uint((λ(n)+1)/2))
}

// integer log base 2
func λ(n int) (a int) {
    for n != 1 {
        a++
        n >>= 1
    }
    return
}

// minChain as described in the paper.
func minChain(n int) starChain {
    switch a := λ(n); {
    case n == 1<<uint(a):
        r := make(starChain, a+1)
        for i := range r {
            r[i] = 1 << uint(i)
        }
        return r
    case n == 3:
        return starChain{1, 2, 3}
    }
    return chain(n, γ(n))
}

// chain as described in the paper.
func chain(n1, n2 int) starChain {
    q, r := n1/n2, n1%n2
    if r == 0 {
        c := minChain(n2)
        c.cMul(minChain(q))
        return c
    }
    c := chain(n2, r)
    c.cMul(minChain(q))
    c.cAdd(r)
    return c
}

func main() {
    m := 31415
    n := 27182
    show(m)
    show(n)
    show(m * n)
    showEasier(m, 1.00002206445416)
    showEasier(n, 1.00002550055251)
}

func show(e int) {
    fmt.Println("exponent:", e)
    s := math.Sqrt(.5)
    a := matrixFromRows([][]float64{
        {s, 0, s, 0, 0, 0},
        {0, s, 0, s, 0, 0},
        {0, s, 0, -s, 0, 0},
        {-s, 0, s, 0, 0, 0},
        {0, 0, 0, 0, 0, 1},
        {0, 0, 0, 0, 1, 0},
    })
    γ = dichotomic
    sc := minChain(e)
    fmt.Println("addition chain:", sc)
    a.scExp(sc).print("a^e")
    fmt.Println("count of multiplies:", mCount)
    fmt.Println()
}

var mCount int

func showEasier(e int, a float64) {
    fmt.Println("exponent:", e)
    γ = dichotomic
    sc := minChain(e)
    fmt.Printf("%.14f^%d: %.14f\n", a, sc[len(sc)-1], scExp64(a, sc))
    fmt.Println("count of multiplies:", mCount)
    fmt.Println()
}

func scExp64(a float64, sc starChain) float64 {
    mCount = 0
    p := make([]float64, len(sc))
    p[0] = a
    for i := 1; i < len(p); i++ {
        d := sc[i] - sc[i-1]
        j := i - 1
        for sc[j] != d {
            j--
        }
        p[i] = p[i-1] * p[j]
        mCount++
    }
    return p[len(p)-1]
}

func (m *matrix) scExp(sc starChain) *matrix {
    mCount = 0
    p := make([]*matrix, len(sc))
    p[0] = m.copy()
    for i := 1; i < len(p); i++ {
        d := sc[i] - sc[i-1]
        j := i - 1
        for sc[j] != d {
            j--
        }
        p[i] = p[i-1].multiply(p[j])
        mCount++
    }
    return p[len(p)-1]
}

func (m *matrix) copy() *matrix {
    return &matrix{append([]float64{}, m.ele...), m.stride}
}

// code below copied from matrix multiplication task
type matrix struct {
    ele    []float64
    stride int
}

func matrixFromRows(rows [][]float64) *matrix {
    if len(rows) == 0 {
        return &matrix{nil, 0}
    }
    m := &matrix{make([]float64, len(rows)*len(rows[0])), len(rows[0])}
    for rx, row := range rows {
        copy(m.ele[rx*m.stride:(rx+1)*m.stride], row)
    }
    return m
}

func (m *matrix) print(heading string) {
    if heading > "" {
        fmt.Print(heading, "\n")
    }
    for e := 0; e < len(m.ele); e += m.stride {
        fmt.Printf("%6.3f ", m.ele[e:e+m.stride])
        fmt.Println()
    }
}

func (m1 *matrix) multiply(m2 *matrix) (m3 *matrix) {
    m3 = &matrix{make([]float64, (len(m1.ele)/m1.stride)*m2.stride), m2.stride}
    for m1c0, m3x := 0, 0; m1c0 < len(m1.ele); m1c0 += m1.stride {
        for m2r0 := 0; m2r0 < m2.stride; m2r0++ {
            for m1x, m2x := m1c0, m2r0; m2x < len(m2.ele); m2x += m2.stride {
                m3.ele[m3x] += m1.ele[m1x] * m2.ele[m2x]
                m1x++
            }
            m3x++
        }
    }
    return m3
}
```

Output (manually wrapped at 80 columns.)

```txt

exponent: 31415
addition chain: [1 2 4 5 10 20 25 50 55 110 220 245 490 980 1960 3920 7840
15680 31360 31415]
a^e
[ 0.707  0.000  0.000 -0.707  0.000  0.000]
[ 0.000  0.707  0.707  0.000  0.000  0.000]
[ 0.707  0.000  0.000  0.707  0.000  0.000]
[ 0.000  0.707 -0.707  0.000  0.000  0.000]
[ 0.000  0.000  0.000  0.000  0.000  1.000]
[ 0.000  0.000  0.000  0.000  1.000  0.000]
count of multiplies: 19

exponent: 27182
addition chain: [1 2 4 8 10 18 28 46 92 184 212 424 848 1696 3392 6784 13568
27136 27182]
a^e
[-0.500 -0.500 -0.500  0.500  0.000  0.000]
[ 0.500 -0.500 -0.500 -0.500  0.000  0.000]
[-0.500 -0.500  0.500 -0.500  0.000  0.000]
[ 0.500 -0.500  0.500  0.500  0.000  0.000]
[ 0.000  0.000  0.000  0.000  1.000  0.000]
[ 0.000  0.000  0.000  0.000  0.000  1.000]
count of multiplies: 18

exponent: 853922530
addition chain: [1 2 4 5 7 12 24 48 96 103 206 309 412 721 1133 1854 3708 4841
9682 19364 21218 26059 52118 104236 208472 416944 833888 1667776 3335552 6671104
13342208 26684416 53368832 106737664 213475328 426950656 853901312 853922530]
a^e
[-0.500  0.500 -0.500  0.500  0.000  0.000]
[-0.500 -0.500 -0.500 -0.500  0.000  0.000]
[-0.500 -0.500  0.500  0.500  0.000  0.000]
[ 0.500 -0.500 -0.500  0.500  0.000  0.000]
[ 0.000  0.000  0.000  0.000  1.000  0.000]
[ 0.000  0.000  0.000  0.000  0.000  1.000]
count of multiplies: 37

exponent: 31415
1.00002206445416^31415: 1.99999999989447
count of multiplies: 19

exponent: 27182
1.00002550055251^27182: 1.99999999997876
count of multiplies: 18

```



## Phix


###  Brute force

Naieve brute force search, no attempt to optimise, manages about 4 million checks/s.

Replacing the recursion with an internal stack and chosen with a fixed length array might help, but
otherwise I got no good ideas at all for trimming the search space.

Giving it the same length, I think, yields the same result as [[Knuth%27s_power_tree#Phix]], and at least
in the cases that I tried, somewhat faster than the method on that page.

If you know the A003313 number, you can throw that at it and wait (for several billion years) or get the
power tree length and loop trying to find a path one shorter (and wait several trillion years). For the
path() and treepow() routines see link above.

Note that "tries" overflows (crashes) at 1073741824, which I kept in as a deliberate limiter.

```Phix
atom t1 = time()+1
integer tries = 0
function addition_chain(integer target, len, sequence chosen={1})
-- target and len must be >=2
    tries += 1
    integer l = length(chosen),
            last = chosen[$]
    if last=target then return chosen end if
    if l=len then
        if time()>t1 then
            ?{"addition_chain",chosen,tries}
            t1 = time()+1
        end if
    else
        for i=l to 1 by -1 do
            integer next = last+chosen[i]
            if next<=target then
                sequence res = addition_chain(target,len,chosen&next)
                if length(res) then return res end if
            end if
        end for
    end if
    return {}
end function

-- first, some proof of correctness at the lower/trivial end:

sequence res = repeat(0,120)
res[2] = 1
for n=3 to length(res) do
    integer l = length(path(n))
    while true do
        sequence ac = addition_chain(n,l)
        if length(ac)=0 then exit end if
        l = length(ac)-1
    end while
    res[n] = l
end for
puts(1,"The first 120 members of A003313:\n")
pp(res)

printf(1,"addition_chain(31,8):%s\n",{sprint(addition_chain(31,8))})
```

{{out}}

```txt

The first 120 members of A003313:
{0,1,2,2,3,3,4,3,4,4,5,4,5,5,5,4,5,5,6,5,6,6,6,5,6,6,6,6,7,6,7,5,6,6,7,6,7,
 7,7,6,7,7,7,7,7,7,8,6,7,7,7,7,8,7,8,7,8,8,8,7,8,8,8,6,7,7,8,7,8,8,9,7,8,8,
 8,8,8,8,9,7,8,8,8,8,8,8,9,8,9,8,9,8,9,9,9,7,8,8,8,8,9,8,9,8,9,9,9,8,9,9,9,
 8,9,9,9,9,9,9,9,8}
addition_chain(31,8):{1,2,4,8,10,20,30,31}

```

On the task numbers, however, as to be expected, it struggles but probably would eventually get there if the overflow on tries were removed:
<pre style="font-size: 11px">--?path(12509)                       -- {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,12288,12416,12480,12496,12504,12508,12509}
--?addition_chain(12509,21) -- 0s       {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,12288,12416,12480,12496,12504,12508,12509}
--?addition_chain(12509,20) -- 12.3s    {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,4160,8320,12480,12496,12504,12508,12509}
--?addition_chain(12509,19) -- 1.1s     {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,4160,4168,8336,12504,12508,12509}
--?addition_chain(12509,18) -- bust
--?addition_chain(12509,17) -- bust

--?path(31415)                       -- {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,24576,28672,30720,31232,31360,31392,31408,31412,31414,31415}
--?addition_chain(31415,25) -- 0s       {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,24576,28672,30720,31232,31360,31392,31408,31412,31414,31415}
--?addition_chain(31415,24) -- bust
--?addition_chain(31415,23) -- bust
--?addition_chain(31415,22) -- 137s     {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,10240,10368,10384,10386,20772,31158,31414,31415}
--?addition_chain(31415,21) -- 116s     {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,6144,6272,6288,6290,12562,25124,31414,31415}
--?addition_chain(31415,20) -- bust
--?addition_chain(31415,19) -- bust

--?path(27182)                       -- {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,24576,26624,27136,27168,27176,27180,27182}
--?addition_chain(27182,22) -- 0s       {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,24576,26624,27136,27168,27176,27180,27182}
--?addition_chain(27182,21) -- 19.4s    {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,8704,8712,8714,17428,26142,27166,27182}
--?addition_chain(27182,20) -- 14.2s    {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,5120,5128,5640,5642,10770,21540,27182}
--?addition_chain(27182,19) -- bust
--?addition_chain(27182,18) -- bust
```

Once you have an addition chain, of course, apply it as per [[Knuth%27s_power_tree#Phix]], and of course length(pn) is the number of multiplies that will perform.

```txt
--sequence pn = {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,24576,28672,30720,31232,31360,31392,31408,31412,31414,31415}
--sequence pn = {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,10240,10368,10384,10386,20772,31158,31414,31415}
--sequence pn = {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,6144,6272,6288,6290,12562,25124,31414,31415}
--sequence pn = {1,2,4,8,16,17,33,49,98,196,392,784,1568,3136,6272,6289,12561,25122,31411,31415}    -- (from C)
--printf(1,"%3g ^ %d (%d) = %s\n", {1.00002206445416,31415,length(pn),treepow(1.00002206445416,31415,pn)})
--1.00002 ^ 31415 (25) = 1.9999999998949602994638558
--1.00002 ^ 31415 (22) = 1.9999999998949602994638556
--1.00002 ^ 31415 (21) = 1.9999999998949602994638552
--1.00002 ^ 31415 (20) = 1.9999999998949602994638291

--sequence pn = {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,24576,26624,27136,27168,27176,27180,27182}
--sequence pn = {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,8704,8712,8714,17428,26142,27166,27182}
--sequence pn = {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,5120,5128,5640,5642,10770,21540,27182}
--sequence pn = {1,2,4,8,10,18,28,46,92,184,212,424,848,1696,3392,6784,13568,27136,27182}       -- (from C)
--printf(1,"%3g ^ %d (%d) = %s\n", {1.00002550055251,27182,length(pn),treepow(1.00002550055251,27182,pn)})
--1.00003 ^ 27182 (22) = 1.9999999999727968238298861
--1.00003 ^ 27182 (21) = 1.9999999999727968238298859
--1.00003 ^ 27182 (20) = 1.9999999999727968238298855
--1.00003 ^ 27182 (19) = 1.9999999999727968238298527
```



## Racket


{{incorrect|matlab|The task states: ''Binary exponentiation does not usually produce the best solution. Provide only optimal solutions.'' This answer only considers binary exponentiation, which is not enough to give an optimal solution.}}


The addition chains correspond to binary exponentiation.

```racket

#lang racket
(define (chain n)
  ; computes a simple addition chain for n
  (cond [(= n 1)   '()]
        [(even? n) (define n/2 (/ n 2))
                   (cons (list n n/2 n/2) (chain n/2))]
        [(odd? n)  (define n-1 (- n 1))
                   (cons (list n n-1 1) (chain (- n 1)))]))

(define mult
  (let ([n 0])
    (λ xs
      (cond [(equal? xs (list 'count)) n]
            [(equal? xs (list 'reset)) (set! n 0)]
            [else (set! n (+ n 1))
                  (apply * xs)]))))

(define (expt/chain x n chain)
    ; computes x^n using the addition chain
  (define ht (make-hash))
  (hash-set! ht 1 x)
  (define (expt1 n)
    (or (hash-ref ht n #f)
        (let ()
          (define x^n
            (match (assoc n chain)
              [(list _ s t) (mult (expt1 s) (expt1 t))]))
          (hash-set! ht n x^n)
          x^n)))
  (expt1 n))

(define (test x n)
  (displayln (~a "Chain for " n "\n" (chain n)))
  (mult 'reset)
  (displayln (~a x " ^ " n " = " (expt/chain x n (chain n))))
  (displayln (~a "Multiplications: " (mult 'count)))
  (newline))

(test 1.00002206445416 31415)
(test 1.00002550055251 27182)

```

Output:

```racket

Chain for 31415
((31415 31414 1) (31414 15707 15707) (15707 15706 1) (15706 7853 7853) (7853 7852 1) (7852 3926 3926) (3926 1963 1963) (1963 1962 1) (1962 981 981) (981 980 1) (980 490 490) (490 245 245) (245 244 1) (244 122 122) (122 61 61) (61 60 1) (60 30 30) (30 15 15) (15 14 1) (14 7 7) (7 6 1) (6 3 3) (3 2 1) (2 1 1))
1.00002206445416 ^ 31415 = 1.9999999998913485
Multiplications: 24

Chain for 27182
((27182 13591 13591) (13591 13590 1) (13590 6795 6795) (6795 6794 1) (6794 3397 3397) (3397 3396 1) (3396 1698 1698) (1698 849 849) (849 848 1) (848 424 424) (424 212 212) (212 106 106) (106 53 53) (53 52 1) (52 26 26) (26 13 13) (13 12 1) (12 6 6) (6 3 3) (3 2 1) (2 1 1))
1.00002550055251 ^ 27182 = 1.9999999999774538
Multiplications: 21

```



## Tcl

{{incorrect|Tcl|The tasks explicitly asks to give only optimal solutions, star chains are not enough.}}

Using code at [[Matrix multiplication#Tcl]] and [[Matrix Transpose#Tcl]] (not shown here).
{{trans|Go}}

```tcl
# Continued fraction addition chains, as described in "Efficient computation
# of addition chains" by F. Bergeron, J. Berstel, and S. Brlek, published in
# Journal de théorie des nombres de Bordeaux, 6 no. 1 (1994), p. 21-38,
# accessed at http://www.numdam.org/item?id=JTNB_1994__6_1_21_0.
#
# Uses the dichotomic strategy, which produces good results with simpler
# coding than for a pluggable non-deterministic strategy.

package require Tcl 8.5
namespace path {::tcl::mathop ::tcl::mathfunc}

proc minchain {n} {
    if {!($n & ($n-1))} {
	for {set i 1} {$i <= $n} {incr i $i} {lappend c $i}
	return $c
    } elseif {$n == 3} {
	return {1 2 3}
    }
    return [chain $n [expr {$n >> int(ceil(floor(log($n)/log(2))/2))}]]
}
proc chain {n1 n2} {
    set q [expr {$n1 / $n2}]
    set r [expr {$n1 % $n2}]
    if {$r == 0} {
	return [chain.* [minchain $n2] [minchain $q]]
    } else {
	return [chain.+ [chain.* [chain $n2 $r] [minchain $q]] $r]
    }
}
proc chain.+ {ns k} {
    return [lappend ns [expr {[lindex $ns end] + $k}]]
}
proc chain.* {ns ms} {
    set n_k [lindex $ns end]
    foreach m_i $ms {
	if {$m_i==1} continue
	lappend ns [expr {$n_k * $m_i}]
    }
    return $ns
}

# Generate a lambda term to do exponentiation with a given multiplier command.
# Works by extracting information from the addition chain; the lambda term
# generated is minimal
proc makeExponentiationLambda {n mulfunc} {
    set chain [minchain $n]
    set cmd {set a0}
    set idxes 0
    foreach c0 [lrange $chain 0 end-1] c1 [lrange $chain 1 end] {
	lappend idxes [lsearch $chain [expr {$c1 - $c0}]]
    }
    for {set i 1} {$i<[llength $chain]} {incr i} {
	set cmd "$mulfunc \[$cmd\] \$a[lindex $idxes $i]"
	if {$i in $idxes} {
	    set cmd "set a$i \[$cmd\]"
	}
    }
    list a0 $cmd
}

# Demonstrating application of problem to matrix exponentiation
proc count_mult {a b} {incr ::countMult;matrix_multiply $a $b}
set m 31415
set n 27182
set mn [expr {$m*$n}]
set pow_m [makeExponentiationLambda $m count_mult]
set pow_n [makeExponentiationLambda $n count_mult]
set pow_mn [makeExponentiationLambda $mn count_mult]

set rh [expr {sqrt(0.5)}]
set mrh [expr {-$rh}]
set A [subst {
    {$rh 0 $rh 0 0 0}
    {0 $rh 0 $rh 0 0}
    {0 $rh 0 $mrh 0 0}
    {$mrh 0 $rh 0 0 0}
    {0 0 0 0 0 1}
    {0 0 0 0 1 0}
}]
puts "A**$m"; set countMult 0
print_matrix [apply $pow_m $A] %6.3f
puts "$countMult matrix multiplies"
puts "A**$n"; set countMult 0
print_matrix [apply $pow_n $A] %6.3f
puts "$countMult matrix multiplies"
puts "A**$mn"; set countMult 0
print_matrix [apply $pow_mn $A] %6.3f
puts "$countMult matrix multiplies"
```

{{out}}

```txt
A**31415
 0.707  0.000  0.000 -0.707  0.000  0.000
 0.000  0.707  0.707  0.000  0.000  0.000
 0.707  0.000  0.000  0.707  0.000  0.000
 0.000  0.707 -0.707  0.000  0.000  0.000
 0.000  0.000  0.000  0.000  0.000  1.000
 0.000  0.000  0.000  0.000  1.000  0.000
19 matrix multiplies
A**27182
-0.500 -0.500 -0.500  0.500  0.000  0.000
 0.500 -0.500 -0.500 -0.500  0.000  0.000
-0.500 -0.500  0.500 -0.500  0.000  0.000
 0.500 -0.500  0.500  0.500  0.000  0.000
 0.000  0.000  0.000  0.000  1.000  0.000
 0.000  0.000  0.000  0.000  0.000  1.000
18 matrix multiplies
A**853922530
-0.500  0.500 -0.500  0.500  0.000  0.000
-0.500 -0.500 -0.500 -0.500  0.000  0.000
-0.500 -0.500  0.500  0.500  0.000  0.000
 0.500 -0.500 -0.500  0.500  0.000  0.000
 0.000  0.000  0.000  0.000  1.000  0.000
 0.000  0.000  0.000  0.000  0.000  1.000
37 matrix multiplies
```


{{omit from|Brlcad}}
{{omit from|GUISS}}
{{omit from|Lilypond}}
{{omit from|Openscad}}
{{omit from|TPP}}

[[Category:Matrices]]
