+++
title = "Addition chains"
description = ""
date = 2019-08-27T03:32:39Z
aliases = []
[extra]
id = 19987
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
An [[wp:Addition_chain|addition chain]]  of length r for n is a sequence 1 = a(0) < a(1) < a(2) ... < a(r) = n , such as a(k) = a(i) + a(j) ( i < k and j < k , i may be = j) . Each member is the sum of two earlier members, not necessarily distincts.

A [[wp:Addition_chain#Brauer_chain|Brauer chain]] for n is an addition chain where a(k) = a(k-1) + a(j) with j < k. Each member uses the previous member as a summand.

We are interested in chains of minimal length L(n).

'''Task'''

For each n in {7,14,21,29,32,42,64} display the following : L(n), the count of Brauer chains of length L(n), an example of such a Brauer chain, the count of non-brauer chains of length L(n), an example of such a chain. (NB: counts may be 0 ).

Extra-credit: Same task for n in {47, 79, 191, 382 , 379, 12509}

'''References'''
* OEIS sequences A079301, A079302. [https://oeis.org/A079301]
* Richard K. Guy - Unsolved problems in Number Theory - C6 - Addition chains.

'''Example'''

* minimal chain length  l(19) = 6
* brauer-chains(19) : count = 31 Ex: ( 1 2 3 4 8 11 19)
* non-brauer-chains(19) : count = 2 Ex: ( 1 2 3 6 7 12 19)





## C

{{trans|Kotlin}}

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TRUE 1
#define FALSE 0

typedef int bool;

typedef struct {
    int x, y;
} pair;

int* example = NULL;
int exampleLen = 0;

void reverse(int s[], int len) {
    int i, j, t;
    for (i = 0, j = len - 1; i < j; ++i, --j) {
        t = s[i];
        s[i] = s[j];
        s[j] = t;
    }
}

pair tryPerm(int i, int pos, int seq[], int n, int len, int minLen);

pair checkSeq(int pos, int seq[], int n, int len, int minLen) {
    pair p;
    if (pos > minLen || seq[0] > n) {
        p.x = minLen; p.y = 0;
        return p;
    }
    else if (seq[0] == n) {
        example = malloc(len * sizeof(int));
        memcpy(example, seq, len * sizeof(int));
        exampleLen = len;
        p.x = pos; p.y = 1;
        return p;
    }
    else if (pos < minLen) {
        return tryPerm(0, pos, seq, n, len, minLen);
    }
    else {
        p.x = minLen; p.y = 0;
        return p;
    }
}

pair tryPerm(int i, int pos, int seq[], int n, int len, int minLen) {
    int *seq2;
    pair p, res1, res2;
    size_t size = sizeof(int);
    if (i > pos) {
        p.x = minLen; p.y = 0;
        return p;
    }
    seq2 = malloc((len + 1) * size);
    memcpy(seq2 + 1, seq, len * size);
    seq2[0] = seq[0] + seq[i];
    res1 = checkSeq(pos + 1, seq2, n, len + 1, minLen);
    res2 = tryPerm(i + 1, pos, seq, n, len, res1.x);
    free(seq2);
    if (res2.x < res1.x)
        return res2;
    else if (res2.x == res1.x) {
        p.x = res2.x; p.y = res1.y + res2.y;
        return p;
    }
    else {
        printf("Error in tryPerm\n");
        p.x = 0; p.y = 0;
        return p;
    }
}

pair initTryPerm(int x, int minLen) {
    int seq[1] = {1};
    return tryPerm(0, 0, seq, x, 1, minLen);
}

void printArray(int a[], int len) {
    int i;
    printf("[");
    for (i = 0; i < len; ++i) printf("%d ", a[i]);
    printf("\b]\n");
}

bool isBrauer(int a[], int len) {
    int i, j;
    bool ok;
    for (i = 2; i < len; ++i) {
        ok = FALSE;
        for (j = i - 1; j >= 0; j--) {
            if (a[i-1] + a[j] == a[i]) {
                ok = TRUE;
                break;
            }
        }
        if (!ok) return FALSE;
    }
    return TRUE;
}

bool isAdditionChain(int a[], int len) {
    int i, j, k;
    bool ok, exit;
    for (i = 2; i < len; ++i) {
        if (a[i] > a[i - 1] * 2) return FALSE;
        ok = FALSE; exit = FALSE;
        for (j = i - 1; j >= 0; --j) {
            for (k = j; k >= 0; --k) {
               if (a[j] + a[k] == a[i]) { ok = TRUE; exit = TRUE; break; }
            }
            if (exit) break;
        }
        if (!ok) return FALSE;
    }
    if (example == NULL && !isBrauer(a, len)) {
        example = malloc(len * sizeof(int));
        memcpy(example, a, len * sizeof(int));
        exampleLen = len;
    }
    return TRUE;
}

void nextChains(int index, int len, int seq[], int *pcount) {
    for (;;) {
        int i;
        if (index < len - 1) {
           nextChains(index + 1, len, seq, pcount);
        }
        if (seq[index] + len - 1 - index >= seq[len - 1]) return;
        seq[index]++;
        for (i = index + 1; i < len - 1; ++i) {
            seq[i] = seq[i-1] + 1;
        }
        if (isAdditionChain(seq, len)) (*pcount)++;
    }
}

int findNonBrauer(int num, int len, int brauer) {
    int i, count = 0;
    int *seq = malloc(len * sizeof(int));
    seq[0] = 1;
    seq[len - 1] = num;
    for (i = 1; i < len - 1; ++i) {
        seq[i] = seq[i - 1] + 1;
    }
    if (isAdditionChain(seq, len)) count = 1;
    nextChains(2, len, seq, &count);
    free(seq);
    return count - brauer;
}

void findBrauer(int num, int minLen, int nbLimit) {
    pair p = initTryPerm(num, minLen);
    int actualMin = p.x, brauer = p.y, nonBrauer;
    printf("\nN = %d\n", num);
    printf("Minimum length of chains : L(%d) = %d\n", num, actualMin);
    printf("Number of minimum length Brauer chains : %d\n", brauer);
    if (brauer > 0) {
        printf("Brauer example : ");
        reverse(example, exampleLen);
        printArray(example, exampleLen);
    }
    if (example != NULL) {
        free(example);
        example = NULL;
        exampleLen = 0;
    }
    if (num <= nbLimit) {
        nonBrauer = findNonBrauer(num, actualMin + 1, brauer);
        printf("Number of minimum length non-Brauer chains : %d\n", nonBrauer);
        if (nonBrauer > 0) {
            printf("Non-Brauer example : ");
            printArray(example, exampleLen);
        }
        if (example != NULL) {
            free(example);
            example = NULL;
            exampleLen = 0;
        }
    }
    else {
        printf("Non-Brauer analysis suppressed\n");
    }
}

int main() {
    int i;
    int nums[12] = {7, 14, 21, 29, 32, 42, 64, 47, 79, 191, 382, 379};
    printf("Searching for Brauer chains up to a minimum length of 12:\n");
    for (i = 0; i < 12; ++i) findBrauer(nums[i], 12, 79);
    return 0;
}
```


{{out}}

```txt

Searching for Brauer chains up to a minimum length of 12:

N = 7
Minimum length of chains : L(7) = 4
Number of minimum length Brauer chains : 5
Brauer example : [1 2 3 4 7]
Number of minimum length non-Brauer chains : 0

N = 14
Minimum length of chains : L(14) = 5
Number of minimum length Brauer chains : 14
Brauer example : [1 2 3 4 7 14]
Number of minimum length non-Brauer chains : 0

N = 21
Minimum length of chains : L(21) = 6
Number of minimum length Brauer chains : 26
Brauer example : [1 2 3 4 7 14 21]
Number of minimum length non-Brauer chains : 3
Non-Brauer example : [1 2 4 5 8 13 21]

N = 29
Minimum length of chains : L(29) = 7
Number of minimum length Brauer chains : 114
Brauer example : [1 2 3 4 7 11 18 29]
Number of minimum length non-Brauer chains : 18
Non-Brauer example : [1 2 3 6 9 11 18 29]

N = 32
Minimum length of chains : L(32) = 5
Number of minimum length Brauer chains : 1
Brauer example : [1 2 4 8 16 32]
Number of minimum length non-Brauer chains : 0

N = 42
Minimum length of chains : L(42) = 7
Number of minimum length Brauer chains : 78
Brauer example : [1 2 3 4 7 14 21 42]
Number of minimum length non-Brauer chains : 6
Non-Brauer example : [1 2 4 5 8 13 21 42]

N = 64
Minimum length of chains : L(64) = 6
Number of minimum length Brauer chains : 1
Brauer example : [1 2 4 8 16 32 64]
Number of minimum length non-Brauer chains : 0

N = 47
Minimum length of chains : L(47) = 8
Number of minimum length Brauer chains : 183
Brauer example : [1 2 3 4 7 10 20 27 47]
Number of minimum length non-Brauer chains : 37
Non-Brauer example : [1 2 3 5 7 14 19 28 47]

N = 79
Minimum length of chains : L(79) = 9
Number of minimum length Brauer chains : 492
Brauer example : [1 2 3 4 7 9 18 36 43 79]
Number of minimum length non-Brauer chains : 129
Non-Brauer example : [1 2 3 5 7 12 24 31 48 79]

N = 191
Minimum length of chains : L(191) = 11
Number of minimum length Brauer chains : 7172
Brauer example : [1 2 3 4 7 8 15 22 44 88 103 191]
Non-Brauer analysis suppressed

N = 382
Minimum length of chains : L(382) = 11
Number of minimum length Brauer chains : 4
Brauer example : [1 2 4 5 9 14 23 46 92 184 198 382]
Non-Brauer analysis suppressed

N = 379
Minimum length of chains : L(379) = 12
Number of minimum length Brauer chains : 6583
Brauer example : [1 2 3 4 7 10 17 27 44 88 176 203 379]
Non-Brauer analysis suppressed

```



## C++

While this worked, something made it run extremely slow.
{{trans|D}}

```cpp
#include <iostream>
#include <tuple>
#include <vector>

std::pair<int, int> tryPerm(int, int, const std::vector<int>&, int, int);

std::pair<int, int> checkSeq(int pos, const std::vector<int>& seq, int n, int minLen) {
    if (pos > minLen || seq[0] > n) return { minLen, 0 };
    else if (seq[0] == n)           return { pos, 1 };
    else if (pos < minLen)          return tryPerm(0, pos, seq, n, minLen);
    else                            return { minLen, 0 };
}

std::pair<int, int> tryPerm(int i, int pos, const std::vector<int>& seq, int n, int minLen) {
    if (i > pos) return { minLen, 0 };

    std::vector<int> seq2{ seq[0] + seq[i] };
    seq2.insert(seq2.end(), seq.cbegin(), seq.cend());
    auto res1 = checkSeq(pos + 1, seq2, n, minLen);
    auto res2 = tryPerm(i + 1, pos, seq, n, res1.first);

    if (res2.first < res1.first)       return res2;
    else if (res2.first == res1.first) return { res2.first, res1.second + res2.second };
    else                               throw std::runtime_error("tryPerm exception");
}

std::pair<int, int> initTryPerm(int x) {
    return tryPerm(0, 0, { 1 }, x, 12);
}

void findBrauer(int num) {
    auto res = initTryPerm(num);
    std::cout << '\n';
    std::cout << "N = " << num << '\n';
    std::cout << "Minimum length of chains: L(n)= " << res.first << '\n';
    std::cout << "Number of minimum length Brauer chains: " << res.second << '\n';
}

int main() {
    std::vector<int> nums{ 7, 14, 21, 29, 32, 42, 64, 47, 79, 191, 382, 379 };
    for (int i : nums) {
        findBrauer(i);
    }

    return 0;
}
```

{{out}}

```txt

N = 7
Minimum length of chains: L(n)= 4
Number of minimum length Brauer chains: 5

N = 14
Minimum length of chains: L(n)= 5
Number of minimum length Brauer chains: 14

N = 21
Minimum length of chains: L(n)= 6
Number of minimum length Brauer chains: 26

N = 29
Minimum length of chains: L(n)= 7
Number of minimum length Brauer chains: 114

N = 32
Minimum length of chains: L(n)= 5
Number of minimum length Brauer chains: 1

N = 42
Minimum length of chains: L(n)= 7
Number of minimum length Brauer chains: 78

N = 64
Minimum length of chains: L(n)= 6
Number of minimum length Brauer chains: 1

N = 47
Minimum length of chains: L(n)= 8
Number of minimum length Brauer chains: 183

N = 79
Minimum length of chains: L(n)= 9
Number of minimum length Brauer chains: 492

N = 191
Minimum length of chains: L(n)= 11
Number of minimum length Brauer chains: 7172

N = 382
Minimum length of chains: L(n)= 11
Number of minimum length Brauer chains: 4

N = 379
Minimum length of chains: L(n)= 12
Number of minimum length Brauer chains: 6583
```


=={{header|C#|C sharp}}==
{{trans|Java}}

```csharp
using System;

namespace AdditionChains {
    class Program {
        static int[] Prepend(int n, int[] seq) {
            int[] result = new int[seq.Length + 1];
            Array.Copy(seq, 0, result, 1, seq.Length);
            result[0] = n;
            return result;
        }

        static Tuple<int, int> CheckSeq(int pos, int[] seq, int n, int min_len) {
            if (pos > min_len || seq[0] > n) return new Tuple<int, int>(min_len, 0);
            if (seq[0] == n) return new Tuple<int, int>(pos, 1);
            if (pos < min_len) return TryPerm(0, pos, seq, n, min_len);
            return new Tuple<int, int>(min_len, 0);
        }

        static Tuple<int, int> TryPerm(int i, int pos, int[] seq, int n, int min_len) {
            if (i > pos) return new Tuple<int, int>(min_len, 0);

            Tuple<int, int> res1 = CheckSeq(pos + 1, Prepend(seq[0] + seq[i], seq), n, min_len);
            Tuple<int, int> res2 = TryPerm(i + 1, pos, seq, n, res1.Item1);

            if (res2.Item1 < res1.Item1) return res2;
            if (res2.Item1 == res1.Item1) return new Tuple<int, int>(res2.Item1, res1.Item2 + res2.Item2);

            throw new Exception("TryPerm exception");
        }

        static Tuple<int, int> InitTryPerm(int x) {
            return TryPerm(0, 0, new int[] { 1 }, x, 12);
        }

        static void FindBrauer(int num) {
            Tuple<int, int> res = InitTryPerm(num);
            Console.WriteLine();
            Console.WriteLine("N = {0}", num);
            Console.WriteLine("Minimum length of chains: L(n)= {0}", res.Item1);
            Console.WriteLine("Number of minimum length Brauer chains: {0}", res.Item2);
        }

        static void Main(string[] args) {
            int[] nums = new int[] { 7, 14, 21, 29, 32, 42, 64, 47, 79, 191, 382, 379 };
            Array.ForEach(nums, n => FindBrauer(n));
        }
    }
}
```

{{out}}

```txt
N = 7
Minimum length of chains: L(n)= 4
Number of minimum length Brauer chains: 5

N = 14
Minimum length of chains: L(n)= 5
Number of minimum length Brauer chains: 14

N = 21
Minimum length of chains: L(n)= 6
Number of minimum length Brauer chains: 26

N = 29
Minimum length of chains: L(n)= 7
Number of minimum length Brauer chains: 114

N = 32
Minimum length of chains: L(n)= 5
Number of minimum length Brauer chains: 1

N = 42
Minimum length of chains: L(n)= 7
Number of minimum length Brauer chains: 78

N = 64
Minimum length of chains: L(n)= 6
Number of minimum length Brauer chains: 1

N = 47
Minimum length of chains: L(n)= 8
Number of minimum length Brauer chains: 183

N = 79
Minimum length of chains: L(n)= 9
Number of minimum length Brauer chains: 492

N = 191
Minimum length of chains: L(n)= 11
Number of minimum length Brauer chains: 7172

N = 382
Minimum length of chains: L(n)= 11
Number of minimum length Brauer chains: 4

N = 379
Minimum length of chains: L(n)= 12
Number of minimum length Brauer chains: 6583
```



## D

{{trans|Scala}}

```D
import std.stdio;
import std.typecons;

alias Pair = Tuple!(int, int);

auto check_seq(int pos, int[] seq, int n, int min_len) {
    if (pos>min_len || seq[0]>n) return Pair(min_len, 0);
    else if (seq[0] == n)        return Pair(    pos, 1);
    else if (pos<min_len)        return try_perm(0, pos, seq, n, min_len);
    else                         return Pair(min_len, 0);
}

auto try_perm(int i, int pos, int[] seq, int n, int min_len) {
    if (i>pos) return Pair(min_len, 0);

    auto res1 = check_seq(pos+1, [seq[0]+seq[i]]~seq, n, min_len);
    auto res2 = try_perm(i+1, pos, seq, n, res1[0]);

    if (res2[0] < res1[0])       return res2;
    else if (res2[0] == res1[0]) return Pair(res2[0], res1[1]+res2[1]);
    else                         throw new Exception("Try_perm exception");
}

auto init_try_perm = function(int x) => try_perm(0, 0, [1], x, 12);

void find_brauer(int num) {
    auto res = init_try_perm(num);
    writeln;
    writeln("N = ", num);
    writeln("Minimum length of chains: L(n)= ", res[0]);
    writeln("Number of minimum length Brauer chains: ", res[1]);
}

void main() {
    auto nums = [7, 14, 21, 29, 32, 42, 64, 47, 79, 191, 382, 379];
    foreach (i; nums) {
        find_brauer(i);
    }
}
```

{{out}}

```txt
N = 7
Minimum length of chains: L(n)= 4
Number of minimum length Brauer chains: 5

N = 14
Minimum length of chains: L(n)= 5
Number of minimum length Brauer chains: 14

N = 21
Minimum length of chains: L(n)= 6
Number of minimum length Brauer chains: 26

N = 29
Minimum length of chains: L(n)= 7
Number of minimum length Brauer chains: 114

N = 32
Minimum length of chains: L(n)= 5
Number of minimum length Brauer chains: 1

N = 42
Minimum length of chains: L(n)= 7
Number of minimum length Brauer chains: 78

N = 64
Minimum length of chains: L(n)= 6
Number of minimum length Brauer chains: 1

N = 47
Minimum length of chains: L(n)= 8
Number of minimum length Brauer chains: 183

N = 79
Minimum length of chains: L(n)= 9
Number of minimum length Brauer chains: 492

N = 191
Minimum length of chains: L(n)= 11
Number of minimum length Brauer chains: 7172

N = 382
Minimum length of chains: L(n)= 11
Number of minimum length Brauer chains: 4

N = 379
Minimum length of chains: L(n)= 12
Number of minimum length Brauer chains: 6583
```



## EchoLisp


```scheme

;; 2^n
(define exp2 (build-vector 32 (lambda(i)(expt 2 i))))

;; counters and results
(define-values (*minlg* *counts* *chains* *calls*) '(0 null null 0))

(define (register-hit chain lg  )
(define idx (if (brauer? chain lg) 0 1))
    (when (< lg *minlg*)
        (set! *counts* (make-vector 2 0))
        (set! *chains* (make-vector 2 ""))
        (set! *minlg* lg))
    (vector+= *counts* idx 1)
    (vector-set! *chains* idx (vector->list chain)))

;; is chain a brauer chain ?
(define (brauer? chain lg)
    (for [(i (in-range 1 lg))]
        #:break (not (vector-search* (- [chain i] [chain (1- i)]) chain)) => #f
        #t))

;; all min chains to target n (brute force)
(define (chains n chain  lg   (a)  (top) (tops null))
(++ *calls*)
(set! top [chain  lg])
    (cond
    [(> lg *minlg*) #f] ;; too long
    [(= n top) (register-hit chain lg)]  ;; hit
    [(< n top) #f] ;; too big
    [(and (< *minlg* 32) (< (* top [exp2 (- *minlg* lg)]) n)) #f] ;; too small
    [else
    (for*  ([i (in-range lg -1 -1)] [j (in-range lg (1- i) -1)])
          (set! a (+ [chain i] [chain j]))
          #:continue (<= a top) ;; increasing sequence
          #:continue (memq a tops) ;; prevent duplicates
          (set! tops (cons a tops))
          (vector-push chain a)
          (chains n chain  (1+ lg))
          (vector-pop chain))]))


(define (task n)
    (set!-values (*minlg* *calls*) '(Infinity 0 ))
    (chains n (make-vector 1 1) 0)
    (printf "L(%d) = %d - brauer-chains: %d  non-brauer: %d  chains: %a %a "
         n *minlg* [*counts* 0] [*counts* 1] [*chains* 0] [*chains* 1]))

```

{{out}}

```txt

(for-each task {7 14 21 29 32 42 64})

L(7) = 4 - brauer-chains: 5 non-brauer: 0 chains: (1 2 3 4 7)
L(14) = 5 - brauer-chains: 14 non-brauer: 0 chains: (1 2 3 4 7 14)
L(21) = 6 - brauer-chains: 26 non-brauer: 3 chains: (1 2 3 4 7 14 21) (1 2 4 5 8 13 21)
L(29) = 7 - brauer-chains: 114 non-brauer: 18 chains: (1 2 3 4 7 11 18 29) (1 2 3 6 9 11 18 29)
L(32) = 5 - brauer-chains: 1 non-brauer: 0 chains: (1 2 4 8 16 32)
L(42) = 7 - brauer-chains: 78 non-brauer: 6 chains: (1 2 3 4 7 14 21 42) (1 2 4 5 8 13 21 42)
L(64) = 6 - brauer-chains: 1 non-brauer: 0 chains: (1 2 4 8 16 32 64)

;; a few extras
(task 47)
L(47) = 8 - brauer-chains: 183 non-brauer: 37 chains: (1 2 3 4 7 10 20 27 47) (1 2 3 5 7 14 19 28 47)
(task 79)
L(79) = 9 - brauer-chains: 492 non-brauer: 129 chains: (1 2 3 4 7 9 18 36 43 79) (1 2 3 5 7 12 24 31 48 79)

```



## Go

{{trans|Kotlin}}

```go
package main

import "fmt"

var example []int

func reverse(s []int) {
    for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
        s[i], s[j] = s[j], s[i]
    }
}

func checkSeq(pos, n, minLen int, seq []int) (int, int) {
    switch {
    case pos > minLen || seq[0] > n:
        return minLen, 0
    case seq[0] == n:
        example = seq
        return pos, 1
    case pos < minLen:
        return tryPerm(0, pos, n, minLen, seq)
    default:
        return minLen, 0
    }
}

func tryPerm(i, pos, n, minLen int, seq []int) (int, int) {
    if i > pos {
        return minLen, 0
    }
    seq2 := make([]int, len(seq)+1)
    copy(seq2[1:], seq)
    seq2[0] = seq[0] + seq[i]
    res11, res12 := checkSeq(pos+1, n, minLen, seq2)
    res21, res22 := tryPerm(i+1, pos, n, res11, seq)
    switch {
    case res21 < res11:
        return res21, res22
    case res21 == res11:
        return res21, res12 + res22
    default:
        fmt.Println("Error in tryPerm")
        return 0, 0
    }
}

func initTryPerm(x, minLen int) (int, int) {
    return tryPerm(0, 0, x, minLen, []int{1})
}

func findBrauer(num, minLen, nbLimit int) {
    actualMin, brauer := initTryPerm(num, minLen)
    fmt.Println("\nN =", num)
    fmt.Printf("Minimum length of chains : L(%d) = %d\n", num, actualMin)
    fmt.Println("Number of minimum length Brauer chains :", brauer)
    if brauer > 0 {
        reverse(example)
        fmt.Println("Brauer example :", example)
    }
    example = nil
    if num <= nbLimit {
        nonBrauer := findNonBrauer(num, actualMin+1, brauer)
        fmt.Println("Number of minimum length non-Brauer chains :", nonBrauer)
        if nonBrauer > 0 {
            fmt.Println("Non-Brauer example :", example)
        }
        example = nil
    } else {
        println("Non-Brauer analysis suppressed")
    }
}

func isAdditionChain(a []int) bool {
    for i := 2; i < len(a); i++ {
        if a[i] > a[i-1]*2 {
            return false
        }
        ok := false
    jloop:
        for j := i - 1; j >= 0; j-- {
            for k := j; k >= 0; k-- {
                if a[j]+a[k] == a[i] {
                    ok = true
                    break jloop
                }
            }
        }
        if !ok {
            return false
        }
    }
    if example == nil && !isBrauer(a) {
        example = make([]int, len(a))
        copy(example, a)
    }
    return true
}

func isBrauer(a []int) bool {
    for i := 2; i < len(a); i++ {
        ok := false
        for j := i - 1; j >= 0; j-- {
            if a[i-1]+a[j] == a[i] {
                ok = true
                break
            }
        }
        if !ok {
            return false
        }
    }
    return true
}

func nextChains(index, le int, seq []int, pcount *int) {
    for {
        if index < le-1 {
            nextChains(index+1, le, seq, pcount)
        }
        if seq[index]+le-1-index >= seq[le-1] {
            return
        }
        seq[index]++
        for i := index + 1; i < le-1; i++ {
            seq[i] = seq[i-1] + 1
        }
        if isAdditionChain(seq) {
            (*pcount)++
        }
    }
}

func findNonBrauer(num, le, brauer int) int {
    seq := make([]int, le)
    seq[0] = 1
    seq[le-1] = num
    for i := 1; i < le-1; i++ {
        seq[i] = seq[i-1] + 1
    }
    count := 0
    if isAdditionChain(seq) {
        count = 1
    }
    nextChains(2, le, seq, &count)
    return count - brauer
}

func main() {
    nums := []int{7, 14, 21, 29, 32, 42, 64, 47, 79, 191, 382, 379}
    fmt.Println("Searching for Brauer chains up to a minimum length of 12:")
    for _, num := range nums {
        findBrauer(num, 12, 79)
    }
}
```


{{out}}

```txt

Searching for Brauer chains up to a minimum length of 12:

N = 7
Minimum length of chains : L(7) = 4
Number of minimum length Brauer chains : 5
Brauer example : [1 2 3 4 7]
Number of minimum length non-Brauer chains : 0

N = 14
Minimum length of chains : L(14) = 5
Number of minimum length Brauer chains : 14
Brauer example : [1 2 3 4 7 14]
Number of minimum length non-Brauer chains : 0

N = 21
Minimum length of chains : L(21) = 6
Number of minimum length Brauer chains : 26
Brauer example : [1 2 3 4 7 14 21]
Number of minimum length non-Brauer chains : 3
Non-Brauer example : [1 2 4 5 8 13 21]

N = 29
Minimum length of chains : L(29) = 7
Number of minimum length Brauer chains : 114
Brauer example : [1 2 3 4 7 11 18 29]
Number of minimum length non-Brauer chains : 18
Non-Brauer example : [1 2 3 6 9 11 18 29]

N = 32
Minimum length of chains : L(32) = 5
Number of minimum length Brauer chains : 1
Brauer example : [1 2 4 8 16 32]
Number of minimum length non-Brauer chains : 0

N = 42
Minimum length of chains : L(42) = 7
Number of minimum length Brauer chains : 78
Brauer example : [1 2 3 4 7 14 21 42]
Number of minimum length non-Brauer chains : 6
Non-Brauer example : [1 2 4 5 8 13 21 42]

N = 64
Minimum length of chains : L(64) = 6
Number of minimum length Brauer chains : 1
Brauer example : [1 2 4 8 16 32 64]
Number of minimum length non-Brauer chains : 0

N = 47
Minimum length of chains : L(47) = 8
Number of minimum length Brauer chains : 183
Brauer example : [1 2 3 4 7 10 20 27 47]
Number of minimum length non-Brauer chains : 37
Non-Brauer example : [1 2 3 5 7 14 19 28 47]

N = 79
Minimum length of chains : L(79) = 9
Number of minimum length Brauer chains : 492
Brauer example : [1 2 3 4 7 9 18 36 43 79]
Number of minimum length non-Brauer chains : 129
Non-Brauer example : [1 2 3 5 7 12 24 31 48 79]

N = 191
Minimum length of chains : L(191) = 11
Number of minimum length Brauer chains : 7172
Brauer example : [1 2 3 4 7 8 15 22 44 88 103 191]
Non-Brauer analysis suppressed

N = 382
Minimum length of chains : L(382) = 11
Number of minimum length Brauer chains : 4
Brauer example : [1 2 4 5 9 14 23 46 92 184 198 382]
Non-Brauer analysis suppressed

N = 379
Minimum length of chains : L(379) = 12
Number of minimum length Brauer chains : 6583
Brauer example : [1 2 3 4 7 10 17 27 44 88 176 203 379]
Non-Brauer analysis suppressed

```



## Java

{{trans|D}}

```Java
public class AdditionChains {
    private static class Pair {
        int f, s;

        Pair(int f, int s) {
            this.f = f;
            this.s = s;
        }
    }

    private static int[] prepend(int n, int[] seq) {
        int[] result = new int[seq.length + 1];
        result[0] = n;
        System.arraycopy(seq, 0, result, 1, seq.length);
        return result;
    }

    private static Pair check_seq(int pos, int[] seq, int n, int min_len) {
        if (pos > min_len || seq[0] > n) return new Pair(min_len, 0);
        else if (seq[0] == n) return new Pair(pos, 1);
        else if (pos < min_len) return try_perm(0, pos, seq, n, min_len);
        else return new Pair(min_len, 0);
    }

    private static Pair try_perm(int i, int pos, int[] seq, int n, int min_len) {
        if (i > pos) return new Pair(min_len, 0);

        Pair res1 = check_seq(pos + 1, prepend(seq[0] + seq[i], seq), n, min_len);
        Pair res2 = try_perm(i + 1, pos, seq, n, res1.f);

        if (res2.f < res1.f) return res2;
        else if (res2.f == res1.f) return new Pair(res2.f, res1.s + res2.s);
        else throw new RuntimeException("Try_perm exception");
    }

    private static Pair init_try_perm(int x) {
        return try_perm(0, 0, new int[]{1}, x, 12);
    }

    private static void find_brauer(int num) {
        Pair res = init_try_perm(num);
        System.out.println();
        System.out.println("N = " + num);
        System.out.println("Minimum length of chains: L(n)= " + res.f);
        System.out.println("Number of minimum length Brauer chains: " + res.s);
    }

    public static void main(String[] args) {
        int[] nums = new int[]{7, 14, 21, 29, 32, 42, 64, 47, 79, 191, 382, 379};
        for (int i : nums) {
            find_brauer(i);
        }
    }
}
```

{{out}}

```txt
N = 7
Minimum length of chains: L(n)= 4
Number of minimum length Brauer chains: 5

N = 14
Minimum length of chains: L(n)= 5
Number of minimum length Brauer chains: 14

N = 21
Minimum length of chains: L(n)= 6
Number of minimum length Brauer chains: 26

N = 29
Minimum length of chains: L(n)= 7
Number of minimum length Brauer chains: 114

N = 32
Minimum length of chains: L(n)= 5
Number of minimum length Brauer chains: 1

N = 42
Minimum length of chains: L(n)= 7
Number of minimum length Brauer chains: 78

N = 64
Minimum length of chains: L(n)= 6
Number of minimum length Brauer chains: 1

N = 47
Minimum length of chains: L(n)= 8
Number of minimum length Brauer chains: 183

N = 79
Minimum length of chains: L(n)= 9
Number of minimum length Brauer chains: 492

N = 191
Minimum length of chains: L(n)= 11
Number of minimum length Brauer chains: 7172

N = 382
Minimum length of chains: L(n)= 11
Number of minimum length Brauer chains: 4

N = 379
Minimum length of chains: L(n)= 12
Number of minimum length Brauer chains: 6583
```




## Julia

{{trans|Python}}

```julia
checksequence(pos, seq, n, minlen) =
    pos > minlen || seq[1] > n ? (minlen, 0) :
    seq[1] == n ? (pos, 1) :
    pos < minlen ? trypermutation(0, pos, seq, n, minlen) : (minlen, 0)

function trypermutation(i, pos, seq, n, minlen)
    if i > pos
        return minlen, 0
    end
    res1 = checksequence(pos + 1, pushfirst!(deepcopy(seq), seq[1] + seq[i + 1]), n, minlen)
    res2 = trypermutation(i + 1, pos, seq, n, res1[1])
    if res2[1] < res1[1]
        return res2
    elseif res2[1] == res1[1]
        return res2[1], res1[2] + res2[2]
    else
        throw("trypermutation exception: res2 head > res1 head")
    end
end

for num in [7, 14, 21, 29, 32, 42, 64, 47, 79, 191, 382, 379]
    (minlen, nchains) = trypermutation(0, 0, [1], num, 12)
    println("N = $num\nMinimum length of chains: L(n) = $minlen")
    println("Number of minimum length Brauer chains: $nchains")
end

```
{{out}}

```txt

N = 7
Minimum length of chains: L(n) = 4
Number of minimum length Brauer chains: 5
N = 14
Minimum length of chains: L(n) = 5
Number of minimum length Brauer chains: 14
N = 21
Minimum length of chains: L(n) = 6
Number of minimum length Brauer chains: 26
N = 29
Minimum length of chains: L(n) = 7
Number of minimum length Brauer chains: 114
N = 32
Minimum length of chains: L(n) = 5
Number of minimum length Brauer chains: 1
N = 42
Minimum length of chains: L(n) = 7
Number of minimum length Brauer chains: 78
N = 64
Minimum length of chains: L(n) = 6
Number of minimum length Brauer chains: 1
N = 47
Minimum length of chains: L(n) = 8
Number of minimum length Brauer chains: 183
N = 79
Minimum length of chains: L(n) = 9
Number of minimum length Brauer chains: 492
N = 191
Minimum length of chains: L(n) = 11
Number of minimum length Brauer chains: 7172
N = 382
Minimum length of chains: L(n) = 11
Number of minimum length Brauer chains: 4
N = 379
Minimum length of chains: L(n) = 12
Number of minimum length Brauer chains: 6583

```




## Kotlin

As far as the minimal Brauer chains are concerned, I've translated the code in the Scala entry which even on my modest machine is reasonably fast for generating these in isolation - negligible for N <= 79, 10 seconds for N = 191, 25 seconds for N = 382 and about 2.5 minutes for N = 379. However, N = 12509 (which according to tables requires a minimum length of 17) is still well out of reach using this code.

I've then extended the code to count the number of non-Brauer chains of the same minimum length - basically 'brute' force to generate all addition chains and then subtracted the number of Brauer ones - plus examples for both. For N <= 64 this adds little to the execution time but adds about 1 minute for N = 79 and I gave up waiting for N = 191! To deal with these glacial execution times, I've added code which enables you to suppress the non-Brauer generation for N above a specified figure.

```scala
// version 1.1.51

var example: List<Int>? = null

fun checkSeq(pos: Int, seq: List<Int>, n: Int, minLen: Int): Pair<Int, Int> =
    if (pos > minLen || seq[0] > n) minLen to 0
    else if (seq[0] == n)           { example = seq; pos to 1 }
    else if (pos < minLen)          tryPerm(0, pos, seq, n, minLen)
    else                            minLen to 0

fun tryPerm(i: Int, pos: Int, seq: List<Int>, n: Int, minLen: Int): Pair<Int, Int> {
    if (i > pos) return minLen to 0
    val res1 = checkSeq(pos + 1, listOf(seq[0] + seq[i]) + seq, n, minLen)
    val res2 = tryPerm(i + 1, pos, seq, n, res1.first)
    return if (res2.first < res1.first)       res2
           else if (res2.first == res1.first) res2.first to (res1.second + res2.second)
           else                               { println("Exception in tryPerm"); 0 to 0 }
}

fun initTryPerm(x: Int, minLen: Int) = tryPerm(0, 0, listOf(1), x, minLen)

fun findBrauer(num: Int, minLen: Int, nbLimit: Int) {
    val (actualMin, brauer) = initTryPerm(num, minLen)
    println("\nN = $num")
    println("Minimum length of chains : L($num) = $actualMin")
    println("Number of minimum length Brauer chains : $brauer")
    if (brauer > 0) println("Brauer example : ${example!!.reversed()}")
    example = null
    if (num <= nbLimit) {
        val nonBrauer = findNonBrauer(num, actualMin + 1, brauer)
        println("Number of minimum length non-Brauer chains : $nonBrauer")
        if (nonBrauer > 0) println("Non-Brauer example : ${example!!}")
        example = null
    }
    else {
        println("Non-Brauer analysis suppressed")
    }
}

fun isAdditionChain(a: IntArray): Boolean {
    for (i in 2 until a.size) {
        if (a[i] > a[i - 1] * 2) return false
        var ok = false
        jloop@ for (j in i - 1 downTo 0) {
            for (k in j downTo 0) {
               if (a[j] + a[k] == a[i]) { ok = true; break@jloop }
            }
        }
        if (!ok) return false
    }
    if (example == null && !isBrauer(a)) example = a.toList()
    return true
}

fun isBrauer(a: IntArray): Boolean {
    for (i in 2 until a.size) {
        var ok = false
        for (j in i - 1 downTo 0) {
            if (a[i - 1] + a[j] == a[i]) { ok = true; break }
        }
        if (!ok) return false
    }
    return true
}

fun findNonBrauer(num: Int, len: Int, brauer: Int): Int {
    val seq = IntArray(len)
    seq[0] = 1
    seq[len - 1] = num
    for (i in 1 until len - 1) seq[i] = seq[i - 1] + 1
    var count = if (isAdditionChain(seq)) 1 else 0

    fun nextChains(index: Int) {
        while (true) {
            if (index < len - 1) nextChains(index + 1)
            if (seq[index] + len - 1 - index >= seq[len - 1]) return
            seq[index]++
            for (i in index + 1 until len - 1) seq[i] = seq[i - 1] + 1
            if (isAdditionChain(seq)) count++
        }
    }

    nextChains(2)
    return count - brauer
}

fun main(args: Array<String>) {
    val nums = listOf(7, 14, 21, 29, 32, 42, 64, 47, 79, 191, 382, 379)
    println("Searching for Brauer chains up to a minimum length of 12:")
    for (num in nums) findBrauer(num, 12, 79)
}
```


{{out}}

```txt

Searching for Brauer chains up to a minimum length of 12:

N = 7
Minimum length of chains : L(7) = 4
Number of minimum length Brauer chains : 5
Brauer example : [1, 2, 3, 4, 7]
Number of minimum length non-Brauer chains : 0

N = 14
Minimum length of chains : L(14) = 5
Number of minimum length Brauer chains : 14
Brauer example : [1, 2, 3, 4, 7, 14]
Number of minimum length non-Brauer chains : 0

N = 21
Minimum length of chains : L(21) = 6
Number of minimum length Brauer chains : 26
Brauer example : [1, 2, 3, 4, 7, 14, 21]
Number of minimum length non-Brauer chains : 3
Non-Brauer example : [1, 2, 4, 5, 8, 13, 21]

N = 29
Minimum length of chains : L(29) = 7
Number of minimum length Brauer chains : 114
Brauer example : [1, 2, 3, 4, 7, 11, 18, 29]
Number of minimum length non-Brauer chains : 18
Non-Brauer example : [1, 2, 3, 6, 9, 11, 18, 29]

N = 32
Minimum length of chains : L(32) = 5
Number of minimum length Brauer chains : 1
Brauer example : [1, 2, 4, 8, 16, 32]
Number of minimum length non-Brauer chains : 0

N = 42
Minimum length of chains : L(42) = 7
Number of minimum length Brauer chains : 78
Brauer example : [1, 2, 3, 4, 7, 14, 21, 42]
Number of minimum length non-Brauer chains : 6
Non-Brauer example : [1, 2, 4, 5, 8, 13, 21, 42]

N = 64
Minimum length of chains : L(64) = 6
Number of minimum length Brauer chains : 1
Brauer example : [1, 2, 4, 8, 16, 32, 64]
Number of minimum length non-Brauer chains : 0

N = 47
Minimum length of chains : L(47) = 8
Number of minimum length Brauer chains : 183
Brauer example : [1, 2, 3, 4, 7, 10, 20, 27, 47]
Number of minimum length non-Brauer chains : 37
Non-Brauer example : [1, 2, 3, 5, 7, 14, 19, 28, 47]

N = 79
Minimum length of chains : L(79) = 9
Number of minimum length Brauer chains : 492
Brauer example : [1, 2, 3, 4, 7, 9, 18, 36, 43, 79]
Number of minimum length non-Brauer chains : 129
Non-Brauer example : [1, 2, 3, 5, 7, 12, 24, 31, 48, 79]

N = 191
Minimum length of chains : L(191) = 11
Number of minimum length Brauer chains : 7172
Brauer example : [1, 2, 3, 4, 7, 8, 15, 22, 44, 88, 103, 191]
Non-Brauer analysis suppressed

N = 382
Minimum length of chains : L(382) = 11
Number of minimum length Brauer chains : 4
Brauer example : [1, 2, 4, 5, 9, 14, 23, 46, 92, 184, 198, 382]
Non-Brauer analysis suppressed

N = 379
Minimum length of chains : L(379) = 12
Number of minimum length Brauer chains : 6583
Brauer example : [1, 2, 3, 4, 7, 10, 17, 27, 44, 88, 176, 203, 379]
Non-Brauer analysis suppressed

```



## Perl

{{trans|Perl 6}}

```perl
use strict;
use feature 'say';

my @Example = ();

sub checkSeq {
   my($pos, $n, $minLen, @seq) = @_;
   if ($pos > $minLen || $seq[0] > $n) {
      return $minLen, 0;
   } elsif ($seq[0] == $n) {
      @Example = @seq;
      return $pos, 1;
   } elsif ($pos < $minLen) {
      return tryPerm(0, $pos, $n, $minLen, @seq);
   } else {
      return $minLen, 0;
   }
}

sub tryPerm {
   my($i, $pos, $n, $minLen, @seq) = @_;
   return $minLen, 0 if $i > $pos;
   my @res1 = checkSeq($pos+1, $n, $minLen, ($seq[0]+$seq[$i],@seq));
   my @res2 = tryPerm($i+1, $pos, $n, $res1[0], @seq);
   if ($res2[0] < $res1[0]) {
      return $res2[0], $res2[1];
   } elsif ($res2[0] == $res1[0]) {
      return $res2[0], $res1[1]+$res2[1];
   } else {
      say "Error in tryPerm";
      return 0, 0;
   }
}

sub initTryPerm {
   my($x, $minLen) = @_;
   return tryPerm(0, 0, $x, $minLen, (1));
}

sub findBrauer {
   my($num, $minLen, $nbLimit) = @_;
   my ($actualMin, $brauer) = initTryPerm($num, $minLen);
   say "\nN = ". $num;
   say "Minimum length of chains : L($num) = $actualMin";
   say "Number of minimum length Brauer chains : ". $brauer;
   say "Brauer example : ". join ' ', reverse @Example if $brauer > 0;
   @Example = ();
   if ($num <= $nbLimit) {
      my $nonBrauer = findNonBrauer($num, $actualMin+1, $brauer);
      say "Number of minimum length non-Brauer chains : ". $nonBrauer;
      say "Non-Brauer example : ". join ' ', @Example if $nonBrauer > 0;
      @Example = ();
   } else {
      say "Non-Brauer analysis suppressed";
   }
}

sub isAdditionChain {
   my(@a) = @_;
   for my $i (2 .. $#a) {
      return 0 if $a[$i] > $a[$i-1]*2;
      my $ok = 0;
      for my $j (reverse 0 .. $i-1) {
          for my $k (reverse 0 .. $j) {
            $ok = 1, last if $a[$j]+$a[$k] == $a[$i];
         }
      }
      return 0 unless $ok;
   }
   @Example = @a if !isBrauer(@a) and !@Example;
   return 1;
}

sub isBrauer {
   my(@a) = @_;
   for my $i (2 .. $#a) {
      my $ok = 0;
      for my $j (reverse 0 .. $i-1) {
         $ok = 1, last if $a[$i-1]+$a[$j] == $a[$i];
      }
      return 0 unless $ok;
   }
   return 1;
}

sub findNonBrauer {
   our($num, $len, $brauer) = @_;
   our @seq = 1 .. $len-1; push @seq, $num;
   our $count = isAdditionChain(@seq) ? 1 : 0;

   sub nextChains {
      my($index) = @_;
      while () {
         nextChains($index+1) if $index < $len-1;
         return if ($seq[$index]+$len-1-$index >= $seq[$len-1]);
         $seq[$index]++;
         for ($index+1 .. $len-2) { $seq[$_] = $seq[$_-1] + 1;}
         $count++ if isAdditionChain(@seq);
      }
   }

   nextChains(2);
   return $count - $brauer;
}

my @nums = (7, 14, 21, 29, 32, 42, 64);  # unlock below for extra credits,
                                         # 47, 79, 191, 382, 379, 379, 12509);
say "Searching for Brauer chains up to a minimum length of 12:";
for (@nums) { findBrauer $_, 12, 79 }
```

{{out}}
<pre style="height:35ex">N = 7
Minimum length of chains : L(7) = 4
Number of minimum length Brauer chains : 5
Brauer example : 1 2 3 4 7
Number of minimum length non-Brauer chains : 0

N = 14
Minimum length of chains : L(14) = 5
Number of minimum length Brauer chains : 14
Brauer example : 1 2 3 4 7 14
Number of minimum length non-Brauer chains : 0

N = 21
Minimum length of chains : L(21) = 6
Number of minimum length Brauer chains : 26
Brauer example : 1 2 3 4 7 14 21
Number of minimum length non-Brauer chains : 3
Non-Brauer example : 1 2 4 5 8 13 21

N = 29
Minimum length of chains : L(29) = 7
Number of minimum length Brauer chains : 114
Brauer example : 1 2 3 4 7 11 18 29
Number of minimum length non-Brauer chains : 18
Non-Brauer example : 1 2 3 6 9 11 18 29

N = 32
Minimum length of chains : L(32) = 5
Number of minimum length Brauer chains : 1
Brauer example : 1 2 4 8 16 32
Number of minimum length non-Brauer chains : 0

N = 42
Minimum length of chains : L(42) = 7
Number of minimum length Brauer chains : 78
Brauer example : 1 2 3 4 7 14 21 42
Number of minimum length non-Brauer chains : 6
Non-Brauer example : 1 2 4 5 8 13 21 42

N = 64
Minimum length of chains : L(64) = 6
Number of minimum length Brauer chains : 1
Brauer example : 1 2 4 8 16 32 64
Number of minimum length non-Brauer chains : 0
```



## Perl 6

{{trans|Kotlin}}

```perl6
my @Example = ();

sub check-Sequence($pos, @seq, $n, $minLen --> List)  {
   if ($pos > $minLen or @seq[0] > $n) {
      return $minLen, 0;
   } elsif (@seq[0] == $n) {
      @Example = @seq;
      return $pos, 1;
   } elsif ($pos < $minLen) {
      return try-Permutation 0, $pos, @seq, $n, $minLen;
   } else {
      return $minLen, 0;
   }
}

multi sub try-Permutation($i, $pos, @seq, $n, $minLen --> List) {
   return $minLen, 0 if $i > $pos;
   my @res1 = check-Sequence $pos+1, (@seq[0]+@seq[$i],@seq).flat, $n, $minLen;
   my @res2 = try-Permutation $i+1, $pos, @seq, $n, @res1[0];
   if (@res2[0] < @res1[0]) {
      return @res2[0], @res2[1];
   } elsif (@res2[0] == @res1[0]) {
      return @res2[0], @res1[1]+@res2[1];
   } else {
      note "Error in try-Permutation";
      return 0, 0;
   }
}

multi sub try-Permutation($x, $minLen --> List) {
   return try-Permutation 0, 0, [1], $x, $minLen;
}

sub find-Brauer($num, $minLen, $nbLimit) {
   my ($actualMin, $brauer) = try-Permutation $num, $minLen;
   say "\nN = ", $num;
   say "Minimum length of chains : L($num) = $actualMin";
   say "Number of minimum length Brauer chains : ", $brauer;
   say "Brauer example : ", @Example.reverse if $brauer > 0;
   @Example = ();
   if ($num ≤ $nbLimit) {
      my $nonBrauer = find-Non-Brauer $num, $actualMin+1, $brauer;
      say "Number of minimum length non-Brauer chains : ", $nonBrauer;
      say "Non-Brauer example : ", @Example if $nonBrauer > 0;
      @Example = ();
   } else {
      say "Non-Brauer analysis suppressed";
   }
}

sub is-Addition-Chain(@a --> Bool) {
   for 2 .. @a.end -> $i {
      return False if @a[$i] > @a[$i-1]*2 ;
      my $ok = False;
      for $i-1 … 0 -> $j {
         for $j … 0 -> $k {
            { $ok = True; last } if @a[$j]+@a[$k] == @a[$i];
         }
      }
      return False unless $ok;
   }

   @Example = @a unless @Example or is-Brauer @a;
   return True;
}

sub is-Brauer(@a --> Bool) {
   for 2 .. @a.end -> $i {
      my $ok = False;
      for $i-1 … 0 -> $j {
         { $ok = True; last } if @a[$i-1]+@a[$j] == @a[$i];
      }
      return False unless $ok;
   }
   return True;
}

sub find-Non-Brauer($num, $len, $brauer --> Int) {
   my @seq   = flat 1 .. $len-1, $num;
   my $count = is-Addition-Chain(@seq) ?? 1 !! 0;

   sub next-Chains($index) {
      loop {
         next-Chains $index+1 if $index < $len-1;
         return if @seq[$index]+$len-1-$index ≥ @seq[$len-1];
         @seq[$index]++;
         for $index^..^$len-1 { @seq[$_] = @seq[$_-1] + 1 }
         $count++ if is-Addition-Chain @seq;
      }
   }

   next-Chains 2;
   return $count - $brauer;
}

say "Searching for Brauer chains up to a minimum length of 12:";
find-Brauer $_, 12, 79 for 7, 14, 21, 29, 32, 42, 64 #, 47, 79, 191, 382, 379, 379, 12509 # un-comment for extra-credit
```

{{out}}

```txt
Searching for Brauer chains up to a minimum length of 12:

N = 7
Minimum length of chains : L(7) = 4
Number of minimum length Brauer chains : 5
Brauer example : (1 2 3 4 7)
Number of minimum length non-Brauer chains : 0

N = 14
Minimum length of chains : L(14) = 5
Number of minimum length Brauer chains : 14
Brauer example : (1 2 3 4 7 14)
Number of minimum length non-Brauer chains : 0

N = 21
Minimum length of chains : L(21) = 6
Number of minimum length Brauer chains : 26
Brauer example : (1 2 3 4 7 14 21)
Number of minimum length non-Brauer chains : 3
Non-Brauer example : [1 2 4 5 8 13 21]

N = 29
Minimum length of chains : L(29) = 7
Number of minimum length Brauer chains : 114
Brauer example : (1 2 3 4 7 11 18 29)
Number of minimum length non-Brauer chains : 18
Non-Brauer example : [1 2 3 6 9 11 18 29]

N = 32
Minimum length of chains : L(32) = 5
Number of minimum length Brauer chains : 1
Brauer example : (1 2 4 8 16 32)
Number of minimum length non-Brauer chains : 0

N = 42
Minimum length of chains : L(42) = 7
Number of minimum length Brauer chains : 78
Brauer example : (1 2 3 4 7 14 21 42)
Number of minimum length non-Brauer chains : 6
Non-Brauer example : [1 2 4 5 8 13 21 42]

N = 64
Minimum length of chains : L(64) = 6
Number of minimum length Brauer chains : 1
Brauer example : (1 2 4 8 16 32 64)
Number of minimum length non-Brauer chains : 0
```



## Phix

Modification of [[Addition-chain_exponentiation#Phix]], which probably will be faster if you already know l(n) and only want one (Brauer).

Note the internal values of l(n) are [consistently] +1 compared to what the rest of the world says.

```Phix
constant nums = {7, 14, 21, 29, 32, 42, 64, 47, 79, 191, 382, 379}
constant maxlen = 13
constant max_non_brauer = 379

function isBrauer(sequence a)
-- translated from Go
    for i=3 to length(a) do
        bool ok = false
        for j=i-1 to 1 by -1 do
            if a[i-1]+a[j] == a[i] then
                ok = true
                exit
            end if
        end for
        if not ok then
            return false
        end if
    end for
    return true
end function

integer last_lm = 0
procedure progress(string m)
    puts(1,m&repeat(' ',max(0,last_lm-length(m)))&"\r")
    last_lm = length(m)
end procedure

integer brauer_count,
        non_brauer_count
sequence brauer_example,
         non_brauer_example

atom t1 = time()+1
atom tries = 0

function addition_chains(integer target, len, sequence chosen={1})
-- nb: target and len must be >=2
    tries += 1
    integer l = length(chosen),
            last = chosen[l]
    if last=target then
        if l<len then
            brauer_count = 0
            non_brauer_count = 0
        end if
        if isBrauer(chosen) then
            brauer_count += 1
            brauer_example = chosen
        else
            non_brauer_count += 1
            non_brauer_example = chosen
        end if
        return l
    end if
    if l=len then
        if time()>t1 then
            progress(sprintf("working... %s, %,d permutations",{sprint(chosen[1..l]),tries}))
            t1 = time()+1
        end if
    elsif target>max_non_brauer then
        for i=l to 1 by -1 do
            integer next = last+chosen[i]
            if next<=target and next>chosen[$] and i<=len then
                len = addition_chains(target,len,chosen&next)
            end if
        end for
    else
        sequence ndone = {} -- if chosen was {1,2,4,5}, don't recurse {1,2,4,5,6} twice,
                            -- once because 5+1=6, and again because 4+2=6, or similar.
        while true do
            for i=l to 1 by -1 do
                integer next = last+chosen[i]
                if next<=target and next>chosen[$] and i<=len and not find(next,ndone) then
                    ndone = append(ndone,next)
                    len = addition_chains(target,len,chosen&next)
                end if
            end for
            l -= 1
            if l=0 then exit end if
            last = chosen[l]
        end while
    end if
    return len
end function

printf(1,"Searching for Brauer chains up to a minimum length of %d:\n",{maxlen-1})
for i=1 to length(nums) do
    atom t = time()
    brauer_count = 0
    brauer_example = {}
    non_brauer_count = 0
    integer num = nums[i],
            l = addition_chains(num,maxlen)
    integer bc = brauer_count,
            nbc = non_brauer_count
    string bs = iff(bc?" eg "&sprint(brauer_example)&",":""),
           ns = iff(nbc?" eg "&sprint(non_brauer_example)&",":""),
           e = elapsed_short(time()-t)
    progress("") -- (wipe it clean)
    printf(1,"l(%d) = %d, Brauer:%d,%s Non-Brauer:%d,%s (%s, %d perms)\n",{num,l-1,bc,bs,nbc,ns,e,tries})
end for
```

{{out}}

```txt

Searching for Brauer chains up to a minimum length of 12:
l(7) = 4, Brauer:5, eg {1,2,3,4,7}, Non-Brauer:0, (0s, 18 perms)
l(14) = 5, Brauer:14, eg {1,2,3,4,7,14}, Non-Brauer:0, (0s, 153 perms)
l(21) = 6, Brauer:26, eg {1,2,3,4,7,14,21}, Non-Brauer:3, eg {1,2,4,5,8,13,21}, (0s, 1014 perms)
l(29) = 7, Brauer:114, eg {1,2,3,4,7,11,18,29}, Non-Brauer:18, eg {1,2,3,6,9,11,18,29}, (0s, 7610 perms)
l(32) = 5, Brauer:1, eg {1,2,4,8,16,32}, Non-Brauer:0, (0s, 7780 perms)
l(42) = 7, Brauer:78, eg {1,2,3,4,7,14,21,42}, Non-Brauer:6, eg {1,2,4,5,8,13,21,42}, (0s, 15935 perms)
l(64) = 6, Brauer:1, eg {1,2,4,8,16,32,64}, Non-Brauer:0, (0s, 17018 perms)
l(47) = 8, Brauer:183, eg {1,2,3,4,7,10,20,27,47}, Non-Brauer:37, eg {1,2,3,5,7,14,19,28,47}, (0s, 105418 perms)
l(79) = 9, Brauer:492, eg {1,2,3,4,7,9,18,36,43,79}, Non-Brauer:129, eg {1,2,3,5,7,12,24,31,48,79}, (0s, 998358 perms)
l(191) = 11, Brauer:7172, eg {1,2,3,4,7,8,15,22,44,88,103,191}, Non-Brauer:2615, eg {1,2,3,4,7,9,14,23,46,92,99,191}, (1:41, 174071925 perms)
l(382) = 11, Brauer:4, eg {1,2,4,5,9,14,23,46,92,184,198,382}, Non-Brauer:0, (2:53, 467243477 perms)
l(379) = 12, Brauer:6583, eg {1,2,3,4,7,10,17,27,44,88,176,203,379}, Non-Brauer:2493, eg {1,2,3,4,7,14,17,31,62,124,131,248,379}, (29:45, 3349176887 perms)

```

For comparison with the Kotlin timings, setting the constant max_non_brauer to 79 yields the following
(making it about 20% slower than the Go submission above, on the same box)

```txt

Searching for Brauer chains up to a minimum length of 12:
l(7) = 4, Brauer:5, eg {1,2,3,4,7}, Non-Brauer:0, (0s, 18 perms)
l(14) = 5, Brauer:14, eg {1,2,3,4,7,14}, Non-Brauer:0, (0s, 153 perms)
l(21) = 6, Brauer:26, eg {1,2,3,4,7,14,21}, Non-Brauer:3, eg {1,2,4,5,8,13,21}, (0s, 1014 perms)
l(29) = 7, Brauer:114, eg {1,2,3,4,7,11,18,29}, Non-Brauer:18, eg {1,2,3,6,9,11,18,29}, (0s, 7610 perms)
l(32) = 5, Brauer:1, eg {1,2,4,8,16,32}, Non-Brauer:0, (0s, 7780 perms)
l(42) = 7, Brauer:78, eg {1,2,3,4,7,14,21,42}, Non-Brauer:6, eg {1,2,4,5,8,13,21,42}, (0s, 15935 perms)
l(64) = 6, Brauer:1, eg {1,2,4,8,16,32,64}, Non-Brauer:0, (0s, 17018 perms)
l(47) = 8, Brauer:183, eg {1,2,3,4,7,10,20,27,47}, Non-Brauer:37, eg {1,2,3,5,7,14,19,28,47}, (0s, 105418 perms)
l(79) = 9, Brauer:492, eg {1,2,3,4,7,9,18,36,43,79}, Non-Brauer:129, eg {1,2,3,5,7,12,24,31,48,79}, (0s, 998358 perms)
l(191) = 11, Brauer:7172, eg {1,2,3,4,7,8,15,22,44,88,103,191}, Non-Brauer:0, (11s, 43748038 perms)
l(382) = 11, Brauer:4, eg {1,2,4,5,9,14,23,46,92,184,198,382}, Non-Brauer:0, (17s, 103474842 perms)
l(379) = 12, Brauer:6583, eg {1,2,3,4,7,10,17,27,44,88,176,203,379}, Non-Brauer:0, (2:19, 622842429 perms)

```



## Python

{{trans|Java}}

```python
def prepend(n, seq):
    return [n] + seq

def check_seq(pos, seq, n, min_len):
    if pos > min_len or seq[0] > n:
        return min_len, 0
    if seq[0] == n:
        return pos, 1
    if pos < min_len:
        return try_perm(0, pos, seq, n, min_len)
    return min_len, 0

def try_perm(i, pos, seq, n, min_len):
    if i > pos:
        return min_len, 0

    res1 = check_seq(pos + 1, prepend(seq[0] + seq[i], seq), n, min_len)
    res2 = try_perm(i + 1, pos, seq, n, res1[0])

    if res2[0] < res1[0]:
        return res2
    if res2[0] == res1[0]:
        return res2[0], res1[1] + res2[1]
    raise Exception("try_perm exception")

def init_try_perm(x):
    return try_perm(0, 0, [1], x, 12)

def find_brauer(num):
    res = init_try_perm(num)
    print
    print "N = ", num
    print "Minimum length of chains: L(n) = ", res[0]
    print "Number of minimum length Brauer chains: ", res[1]

# main
nums = [7, 14, 21, 29, 32, 42, 64, 47, 79, 191, 382, 379]
for i in nums:
    find_brauer(i)
```

{{out}}

```txt

N =  7
Minimum length of chains: L(n) =  4
Number of minimum length Brauer chains:  5

N =  14
Minimum length of chains: L(n) =  5
Number of minimum length Brauer chains:  14

N =  21
Minimum length of chains: L(n) =  6
Number of minimum length Brauer chains:  26

N =  29
Minimum length of chains: L(n) =  7
Number of minimum length Brauer chains:  114

N =  32
Minimum length of chains: L(n) =  5
Number of minimum length Brauer chains:  1

N =  42
Minimum length of chains: L(n) =  7
Number of minimum length Brauer chains:  78

N =  64
Minimum length of chains: L(n) =  6
Number of minimum length Brauer chains:  1

N =  47
Minimum length of chains: L(n) =  8
Number of minimum length Brauer chains:  183

N =  79
Minimum length of chains: L(n) =  9
Number of minimum length Brauer chains:  492

N =  191
Minimum length of chains: L(n) =  11
Number of minimum length Brauer chains:  7172

N =  382
Minimum length of chains: L(n) =  11
Number of minimum length Brauer chains:  4

N =  379
Minimum length of chains: L(n) =  12
Number of minimum length Brauer chains:  6583
```



## Racket


This implementation uses the [https://docs.racket-lang.org/rosette-guide/index.html Rosette] language in Racket. It is inefficient as it asks an SMT solver to enumerate every possible solutions. However, it is very straightforward to write, and in fact is quite efficient for computing <code>l(n)</code> and finding one example (solve n = 379 in ~3 seconds).


```racket
#lang rosette

(define (basic-constraints xs n)
  (assert (= 1 (first xs)))
  (assert (= n (last xs)))
  (assert (apply < xs))
  (for ([x (in-list (rest xs))] [xi (in-naturals 1)])
    (assert
     (apply || (for*/list ([(y yi) (in-parallel (in-list xs) (in-range xi))]
                           [(z zi) (in-parallel (in-list xs) (in-range xi))])
                 (= x (+ y z)))))))

(define (next-sol xs the-mod)
  (not (apply && (for/list ([x (in-list xs)]) (= x (evaluate x the-mod))))))

(define (try-len r n enumerate?)
  (define xs (build-list (add1 r)
                         (thunk* (define-symbolic* x integer?)
                                 x)))
  (basic-constraints xs n)
  (define sols (let loop ([sols '()])
                 (define the-mod (solve #t))
                 (cond
                   [(unsat? the-mod) sols]
                   [enumerate? (assert (next-sol xs the-mod))
                               (loop (cons (evaluate xs the-mod) sols))]
                   [else (list (evaluate xs the-mod))])))
  (clear-state!)
  (if (empty? sols) #f (cons sols r)))

(define (brauer? xs)
  (for/and ([x (in-list (rest xs))] [xi (in-naturals 1)] [x* (in-list xs)])
    (for/or ([y (in-list xs)] [yi (in-range xi)]) (= x (+ x* y)))))

(define (report-chain chain name)
  (printf "#~a chains: ~a\n" name (length chain))
  (when (not (empty? chain)) (printf "example: ~a\n" (first chain))))

(define (compute n enumerate?)
  (define sols (for/or ([r (in-naturals 1)]) (try-len r n enumerate?)))
  (printf "minimal chain length l(~a) = ~a\n" n (cdr sols))
  (cond
    [enumerate?
     (define-values (brauer-chain non-brauer-chain) (partition brauer? (car sols)))
     (report-chain brauer-chain "brauer")
     (report-chain non-brauer-chain "non-brauer")]
    [else (printf "example: ~a\n" (first (car sols)))]))

(define (compute/time n #:enumerate? enumerate?)
  (match-define-values (_ _ real _) (time-apply compute (list n enumerate?)))
  (printf "total time (ms): ~a\n\n" real))

(for ([x (in-list '(19 7 14 21 29 32 42 64 47 79))])
  (compute/time x #:enumerate? #t))

(for ([x (in-list '(191 382 379 12509))])
  (compute/time x #:enumerate? #f))
```


{{out}}

```txt

minimal chain length l(19) = 6
#brauer chains: 31
example: (1 2 3 4 8 16 19)
#non-brauer chains: 2
example: (1 2 3 6 7 12 19)
total time (ms): 245

minimal chain length l(7) = 4
#brauer chains: 5
example: (1 2 3 6 7)
#non-brauer chains: 0
total time (ms): 47

minimal chain length l(14) = 5
#brauer chains: 14
example: (1 2 3 5 7 14)
#non-brauer chains: 0
total time (ms): 95

minimal chain length l(21) = 6
#brauer chains: 26
example: (1 2 3 4 7 14 21)
#non-brauer chains: 3
example: (1 2 4 5 8 13 21)
total time (ms): 204

minimal chain length l(29) = 7
#brauer chains: 114
example: (1 2 3 6 7 13 16 29)
#non-brauer chains: 18
example: (1 2 3 6 9 11 18 29)
total time (ms): 1443

minimal chain length l(32) = 5
#brauer chains: 1
example: (1 2 4 8 16 32)
#non-brauer chains: 0
total time (ms): 42

minimal chain length l(42) = 7
#brauer chains: 78
example: (1 2 3 6 9 15 21 42)
#non-brauer chains: 6
example: (1 2 4 5 8 13 21 42)
total time (ms): 808

minimal chain length l(64) = 6
#brauer chains: 1
example: (1 2 4 8 16 32 64)
#non-brauer chains: 0
total time (ms): 52

minimal chain length l(47) = 8
#brauer chains: 183
example: (1 2 3 5 8 11 22 44 47)
#non-brauer chains: 37
example: (1 2 3 5 7 14 19 28 47)
total time (ms): 6011

minimal chain length l(79) = 9
#brauer chains: 492
example: (1 2 4 8 12 13 25 29 54 79)
#non-brauer chains: 129
example: (1 2 4 8 9 12 21 29 58 79)
total time (ms): 38038

minimal chain length l(191) = 11
example: (1 2 4 8 16 24 28 29 53 69 138 191)
total time (ms): 1601

minimal chain length l(382) = 11
example: (1 2 4 5 9 14 23 46 92 184 368 382)
total time (ms): 2313

minimal chain length l(379) = 12
example: (1 2 4 8 12 24 48 72 73 121 129 258 379)
total time (ms): 3176

minimal chain length l(12509) = 17
example: (1 2 3 6 12 13 24 48 96 192 384 768 781 1562 3124 6248 12496 12509)
total time (ms): 237617

```



## Scala

Following Scala implementation finds number of minimum length Brauer chains and corresponding length.

```Scala

object chains{

    def check_seq(pos:Int,seq:List[Int],n:Int,min_len:Int):(Int,Int) = {
        if(pos>min_len || seq(0)>n)             (min_len,0)
        else if(seq(0) == n)                    (pos,1)
        else if(pos<min_len)                    try_perm(0,pos,seq,n,min_len)
        else                                    (min_len,0)
    }

    def try_perm(i:Int,pos:Int,seq:List[Int],n:Int,min_len:Int):(Int,Int) = {
        if(i>pos)           return (min_len,0)
        val res1 = check_seq(pos+1,seq(0)+seq(i) :: seq,n,min_len)
        val res2 = try_perm(i+1,pos,seq,n,res1._1)
        if(res2._1 < res1._1)                   res2
        else if(res2._1 == res1._1)             (res2._1,res1._2 + res2._2)
        else {
            println("Try_perm exception")
            (0,0)
        }
    }
    val init_try_perm = (x:Int) => try_perm(0,0,List[Int](1),x,10)
    def find_brauer(num:Int): Unit = {
        val res = init_try_perm(num)
        println()
        println("N = %d".format(num))
        println("Minimum length of chains: L(n)= " + res._1 + f"\nNumber of minimum length Brauer chains: " + res._2)
    }
    def main(args:Array[String]) :Unit = {
        val nums = List(7,14,21,29,32,42,64)
        for (i <- nums)     find_brauer(i)
    }
}

```


```txt

N = 7
Minimum length of chains: L(n)= 4
Number of minimum length Brauer chains: 5

N = 14
Minimum length of chains: L(n)= 5
Number of minimum length Brauer chains: 14

N = 21
Minimum length of chains: L(n)= 6
Number of minimum length Brauer chains: 26

N = 29
Minimum length of chains: L(n)= 7
Number of minimum length Brauer chains: 114

N = 32
Minimum length of chains: L(n)= 5
Number of minimum length Brauer chains: 1

N = 42
Minimum length of chains: L(n)= 7
Number of minimum length Brauer chains: 78

N = 64
Minimum length of chains: L(n)= 6
Number of minimum length Brauer chains: 1
N = 47
Minimum length of chains: L(n)= 8
Number of minimum length Brauer chains: 183

N = 79
Minimum length of chains: L(n)= 9
Number of minimum length Brauer chains: 492

N = 191
Minimum length of chains: L(n)= 11
Number of minimum length Brauer chains: 7172

N = 191
Minimum length of chains: L(n)= 11
Number of minimum length Brauer chains: 7172

N = 382
Minimum length of chains: L(n)= 11
Number of minimum length Brauer chains: 4

N = 379
Minimum length of chains: L(n)= 12
Number of minimum length Brauer chains: 6583


```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Function Prepend(n As Integer, seq As List(Of Integer)) As List(Of Integer)
        Dim result As New List(Of Integer) From {
            n
        }
        result.AddRange(seq)
        Return result
    End Function

    Function CheckSeq(pos As Integer, seq As List(Of Integer), n As Integer, min_len As Integer) As Tuple(Of Integer, Integer)
        If pos > min_len OrElse seq(0) > n Then
            Return Tuple.Create(min_len, 0)
        End If
        If seq(0) = n Then
            Return Tuple.Create(pos, 1)
        End If
        If pos < min_len Then
            Return TryPerm(0, pos, seq, n, min_len)
        End If
        Return Tuple.Create(min_len, 0)
    End Function

    Function TryPerm(i As Integer, pos As Integer, seq As List(Of Integer), n As Integer, min_len As Integer) As Tuple(Of Integer, Integer)
        If i > pos Then
            Return Tuple.Create(min_len, 0)
        End If

        Dim res1 = CheckSeq(pos + 1, Prepend(seq(0) + seq(i), seq), n, min_len)
        Dim res2 = TryPerm(i + 1, pos, seq, n, res1.Item1)

        If res2.Item1 < res1.Item1 Then
            Return res2
        End If
        If res2.Item1 = res1.Item1 Then
            Return Tuple.Create(res2.Item1, res1.Item2 + res2.Item2)
        End If

        Throw New Exception("TryPerm exception")
    End Function

    Function InitTryPerm(x As Integer) As Tuple(Of Integer, Integer)
        Return TryPerm(0, 0, New List(Of Integer) From {1}, x, 12)
    End Function

    Sub FindBrauer(num As Integer)
        Dim res = InitTryPerm(num)
        Console.WriteLine("N = {0}", num)
        Console.WriteLine("Minimum length of chains: L(n) = {0}", res.Item1)
        Console.WriteLine("Number of minimum length Brauer chains: {0}", res.Item2)
        Console.WriteLine()
    End Sub

    Sub Main()
        Dim nums() = {7, 14, 21, 29, 32, 42, 64, 47, 79, 191, 382, 379}
        Array.ForEach(nums, Sub(n) FindBrauer(n))
    End Sub

End Module
```

{{out}}

```txt
N = 7
Minimum length of chains: L(n) = 4
Number of minimum length Brauer chains: 5

N = 14
Minimum length of chains: L(n) = 5
Number of minimum length Brauer chains: 14

N = 21
Minimum length of chains: L(n) = 6
Number of minimum length Brauer chains: 26

N = 29
Minimum length of chains: L(n) = 7
Number of minimum length Brauer chains: 114

N = 32
Minimum length of chains: L(n) = 5
Number of minimum length Brauer chains: 1

N = 42
Minimum length of chains: L(n) = 7
Number of minimum length Brauer chains: 78

N = 64
Minimum length of chains: L(n) = 6
Number of minimum length Brauer chains: 1

N = 47
Minimum length of chains: L(n) = 8
Number of minimum length Brauer chains: 183

N = 79
Minimum length of chains: L(n) = 9
Number of minimum length Brauer chains: 492

N = 191
Minimum length of chains: L(n) = 11
Number of minimum length Brauer chains: 7172

N = 382
Minimum length of chains: L(n) = 11
Number of minimum length Brauer chains: 4

N = 379
Minimum length of chains: L(n) = 12
Number of minimum length Brauer chains: 6583
```



## zkl

{{trans|EchoLisp}}

```zkl
var exp2=(32).pump(List,(2).pow),   // 2^n, n=0..31
    _minlg, _counts, _chains;      // counters and results

fcn register_hit(chain,lg){  // save [upto 2] chains
   idx:=(if(isBrauer(chain,lg)) 0 else 1);
   if(lg<_minlg) _counts,_chains,_minlg=List(0,0), List("",""), lg;
   _counts[idx]+=1;
   _chains[idx]=chain.copy();
}
    // is chain a brauer chain ?
fcn isBrauer(chain,lg){
   foreach i in (lg){
      if(not chain.holds(chain[i+1] - chain[i])) return(False);
    }
    True
}
    // all min chains to target n (brute force)
fcn chains(n,chain,lg){
   top,tops:=chain[lg], List();
   if(lg>_minlg)   {}			   // too long
   else if(n==top) register_hit(chain,lg); // hit
   else if(n<top)  {}			   // too big
   else if((_minlg<32) and (top*exp2[_minlg - lg]<n)){} // too small
   else{
      foreach i,j in ([lg..0,-1],[lg..i,-1]){
         a:=chain[i] + chain[j];
	 if(a<=top)        continue; // increasing sequence
	 if(tops.holds(a)) continue; // prevent duplicates
	 tops.append(a);
	 chain.append(a);
	 self.fcn(n,chain,lg+1);     // recurse
	 chain.pop();
      }
   }
}
```


```zkl
fcn task(n){
   _minlg=(0).MAX;
   chains(n,List(1),0);
   println("L(%2d) = %d; Brauer-chains: %3d; non-brauer: %3d; chains: %s"
         .fmt(n,_minlg,_counts.xplode(),_chains.filter()));
}
T(7,14,21,29,32,42,64,47,79).apply2(task);
```

{{out}}

```txt

L( 7) = 4; Brauer-chains:   5; non-brauer:   0; chains: L(L(1,2,3,4,7))
L(14) = 5; Brauer-chains:  14; non-brauer:   0; chains: L(L(1,2,3,4,7,14))
L(21) = 6; Brauer-chains:  26; non-brauer:   3; chains: L(L(1,2,3,4,7,14,21),L(1,2,4,5,8,13,21))
L(29) = 7; Brauer-chains: 114; non-brauer:  18; chains: L(L(1,2,3,4,7,11,18,29),L(1,2,3,6,9,11,18,29))
L(32) = 5; Brauer-chains:   1; non-brauer:   0; chains: L(L(1,2,4,8,16,32))
L(42) = 7; Brauer-chains:  78; non-brauer:   6; chains: L(L(1,2,3,4,7,14,21,42),L(1,2,4,5,8,13,21,42))
L(64) = 6; Brauer-chains:   1; non-brauer:   0; chains: L(L(1,2,4,8,16,32,64))
L(47) = 8; Brauer-chains: 183; non-brauer:  37; chains: L(L(1,2,3,4,7,10,20,27,47),L(1,2,3,5,7,14,19,28,47))
L(79) = 9; Brauer-chains: 492; non-brauer: 129; chains: L(L(1,2,3,4,7,9,18,36,43,79),L(1,2,3,5,7,12,24,31,48,79))

```

