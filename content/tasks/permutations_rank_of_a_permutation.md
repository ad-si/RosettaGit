+++
title = "Permutations/Rank of a permutation"
description = ""
date = 2018-10-04T22:44:00Z
aliases = []
[extra]
id = 12454
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "d",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "mathematica",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "tcl",
  "zkl",
]
+++

A particular ranking of a permutation associates an integer with a particular ordering of all the permutations of a set of distinct items.
For our purposes the ranking will assign integers <math>0 .. (n! - 1)</math> to an ordering of all the permutations of the integers <math>0 .. (n - 1)</math>.

For example, the permutations of the digits zero to 3 arranged lexicographically have the following rank:


```txt
  PERMUTATION      RANK
  (0, 1, 2, 3) ->  0
  (0, 1, 3, 2) ->  1
  (0, 2, 1, 3) ->  2
  (0, 2, 3, 1) ->  3
  (0, 3, 1, 2) ->  4
  (0, 3, 2, 1) ->  5
  (1, 0, 2, 3) ->  6
  (1, 0, 3, 2) ->  7
  (1, 2, 0, 3) ->  8
  (1, 2, 3, 0) ->  9
  (1, 3, 0, 2) -> 10
  (1, 3, 2, 0) -> 11
  (2, 0, 1, 3) -> 12
  (2, 0, 3, 1) -> 13
  (2, 1, 0, 3) -> 14
  (2, 1, 3, 0) -> 15
  (2, 3, 0, 1) -> 16
  (2, 3, 1, 0) -> 17
  (3, 0, 1, 2) -> 18
  (3, 0, 2, 1) -> 19
  (3, 1, 0, 2) -> 20
  (3, 1, 2, 0) -> 21
  (3, 2, 0, 1) -> 22
  (3, 2, 1, 0) -> 23
```


Algorithms exist that can generate a rank from a permutation for some particular ordering of permutations, and that can generate the same rank from the given individual permutation (i.e. given a rank of 17 produce (2, 3, 1, 0) in the example above).

One use of such algorithms could be in generating a small, random, sample of permutations of <math>n</math> items without duplicates when the total number of permutations is large. Remember that the total number of permutations of <math>n</math> items is given by <math>n!</math> which grows large very quickly: A 32 bit integer can only hold <math>12!</math>, a 64 bit integer only <math>20!</math>. It becomes difficult to take the straight-forward approach of generating all permutations then taking a random sample of them.

A [http://stackoverflow.com/questions/12884428/generate-sample-of-1-000-000-random-permutations question on the Stack Overflow site] asked how to generate one million random and indivudual permutations of 144 items.


## Task

# Create a function to generate a permutation from a rank.
# Create the inverse function that given the permutation generates its rank.
# Show that for <math>n=3</math> the two functions are indeed inverses of each other.
# Compute and show here 4 random, individual, samples of permutations of 12 objects.


;Stretch goal:
* State how reasonable it would be to use your program to address the limits of the Stack Overflow question.


## References

# [http://webhome.cs.uvic.ca/~ruskey/Publications/RankPerm/RankPerm.html Ranking and Unranking Permutations in Linear Time] by Myrvold & Ruskey. (Also available via Google [https://docs.google.com/viewer?a=v&q=cache:t8G2xQ3-wlkJ:citeseerx.ist.psu.edu/viewdoc/download%3Fdoi%3D10.1.1.43.4521%26rep%3Drep1%26type%3Dpdf+&hl=en&gl=uk&pid=bl&srcid=ADGEESgDcCc4JVd_57ziRRFlhDFxpPxoy88eABf9UG_TLXMzfxiC8D__qx4xfY3JAhw_nuPDrZ9gSInX0MbpYjgh807ZfoNtLrl40wdNElw2JMdi94Znv1diM-XYo53D8uelCXnK053L&sig=AHIEtbQtx-sxcVzaZgy9uhniOmETuW4xKg here]).
# [http://www.davdata.nl/math/ranks.html Ranks] on the DevData site.
# [http://stackoverflow.com/a/1506337/10562 Another answer] on Stack Overflow to a different question that explains its algorithm in detail.





## C


###  C: Myrvold and Ruskey

This is algorithm #1 from the M&R paper.

```c
#include <stdio.h>
#include <stdlib.h>

#define SWAP(a,b) do{t=(a);(a)=(b);(b)=t;}while(0)

void _mr_unrank1(int rank, int n, int *vec) {
    int t, q, r;
    if (n < 1) return;

    q = rank / n;
    r = rank % n;
    SWAP(vec[r], vec[n-1]);
    _mr_unrank1(q, n-1, vec);
}

int _mr_rank1(int n, int *vec, int *inv) {
    int s, t;
    if (n < 2) return 0;

    s = vec[n-1];
    SWAP(vec[n-1], vec[inv[n-1]]);
    SWAP(inv[s], inv[n-1]);
    return s + n * _mr_rank1(n-1, vec, inv);
}

/* Fill the integer array <vec> (of size <n>) with the
 * permutation at rank <rank>.
 */
void get_permutation(int rank, int n, int *vec) {
    int i;
    for (i = 0; i < n; ++i) vec[i] = i;
    _mr_unrank1(rank, n, vec);
}

/* Return the rank of the current permutation of array <vec>
 * (of size <n>).
 */
int get_rank(int n, int *vec) {
    int i, r, *v, *inv;

    v = malloc(n * sizeof(int));
    inv = malloc(n * sizeof(int));

    for (i = 0; i < n; ++i) {
        v[i] = vec[i];
        inv[vec[i]] = i;
    }
    r = _mr_rank1(n, v, inv);
    free(inv);
    free(v);
    return r;
}

int main(int argc, char *argv[]) {
    int i, r, tv[4];

    for (r = 0; r < 24; ++r) {
        printf("%3d: ", r);
        get_permutation(r, 4, tv);

        for (i = 0; i < 4; ++i) {
            if (0 == i) printf("[ ");
            else printf(", ");
            printf("%d", tv[i]);
        }
        printf(" ] = %d\n", get_rank(4, tv));
    }
}

```


```txt
  0: [ 1, 2, 3, 0 ] = 0
  1: [ 3, 2, 0, 1 ] = 1
  2: [ 1, 3, 0, 2 ] = 2
  3: [ 1, 2, 0, 3 ] = 3
  4: [ 2, 3, 1, 0 ] = 4
  5: [ 2, 0, 3, 1 ] = 5
  6: [ 3, 0, 1, 2 ] = 6
  7: [ 2, 0, 1, 3 ] = 7
  8: [ 1, 3, 2, 0 ] = 8
  9: [ 3, 0, 2, 1 ] = 9
 10: [ 1, 0, 3, 2 ] = 10
 11: [ 1, 0, 2, 3 ] = 11
 12: [ 2, 1, 3, 0 ] = 12
 13: [ 2, 3, 0, 1 ] = 13
 14: [ 3, 1, 0, 2 ] = 14
 15: [ 2, 1, 0, 3 ] = 15
 16: [ 3, 2, 1, 0 ] = 16
 17: [ 0, 2, 3, 1 ] = 17
 18: [ 0, 3, 1, 2 ] = 18
 19: [ 0, 2, 1, 3 ] = 19
 20: [ 3, 1, 2, 0 ] = 20
 21: [ 0, 3, 2, 1 ] = 21
 22: [ 0, 1, 3, 2 ] = 22
 23: [ 0, 1, 2, 3 ] = 23

```



## D

Currently this doesn't use BigInts.

```d
import std.stdio, std.algorithm, std.range;

alias TRank = ulong;

TRank factorial(in uint n) pure nothrow {
    TRank result = 1;
    foreach (immutable i; 2 .. n + 1)
        result *= i;
    return result;
}

/// Fill the integer array <vec> with the permutation at rank <rank>.
void computePermutation(size_t N)(ref uint[N] vec, TRank rank)
pure nothrow if (N > 0 && N < 22) {
    N.iota.copy(vec[]);

    foreach_reverse (immutable n; 1 .. N + 1) {
        immutable size_t r = rank % n;
        rank /= n;
        swap(vec[r], vec[n - 1]);
    }
}

/// Return the rank of the current permutation.
TRank computeRank(size_t N)(in ref uint[N] vec) pure nothrow
if (N > 0 && N < 22) {
    uint[N] vec2, inv = void;

    TRank mrRank1(in uint n) nothrow {
        if (n < 2)
            return 0;

        immutable s = vec2[n - 1];
        swap(vec2[n - 1], vec2[inv[n - 1]]);
        swap(inv[s], inv[n - 1]);
        return s + n * mrRank1(n - 1);
    }

    vec2[] = vec[];
    foreach (immutable i; 0 .. N)
        inv[vec[i]] = i;
    return mrRank1(N);
}

void main() {
    import std.random;

    uint[4] items1 = void;
    immutable rMax1 = items1.length.factorial;
    for (TRank rank = 0; rank < rMax1; rank++) {
        items1.computePermutation(rank);
        writefln("%3d: %s = %d", rank, items1, items1.computeRank);
    }
    writeln;

    uint[21] items2 = void;
    immutable rMax2 = items2.length.factorial;
    foreach (immutable _; 0 .. 5) {
        immutable rank = uniform(0, rMax2);
        items2.computePermutation(rank);
        writefln("%20d: %s = %d", rank, items2, items2.computeRank);
    }
}
```

```txt
  0: [1, 2, 3, 0] = 0
  1: [3, 2, 0, 1] = 1
  2: [1, 3, 0, 2] = 2
  3: [1, 2, 0, 3] = 3
  4: [2, 3, 1, 0] = 4
  5: [2, 0, 3, 1] = 5
  6: [3, 0, 1, 2] = 6
  7: [2, 0, 1, 3] = 7
  8: [1, 3, 2, 0] = 8
  9: [3, 0, 2, 1] = 9
 10: [1, 0, 3, 2] = 10
 11: [1, 0, 2, 3] = 11
 12: [2, 1, 3, 0] = 12
 13: [2, 3, 0, 1] = 13
 14: [3, 1, 0, 2] = 14
 15: [2, 1, 0, 3] = 15
 16: [3, 2, 1, 0] = 16
 17: [0, 2, 3, 1] = 17
 18: [0, 3, 1, 2] = 18
 19: [0, 2, 1, 3] = 19
 20: [3, 1, 2, 0] = 20
 21: [0, 3, 2, 1] = 21
 22: [0, 1, 3, 2] = 22
 23: [0, 1, 2, 3] = 23

 5757702426915204486: [1, 4, 12, 10, 5, 13, 20, 9, 17, 8, 2, 19, 15, 3, 16, 11, 14, 18, 7, 6, 0] = 5757702426915204486
 9054497950101639559: [17, 3, 19, 0, 12, 9, 20, 1, 5, 7, 15, 2, 16, 10, 18, 4, 8, 11, 14, 6, 13] = 9054497950101639559
 6430238494482930297: [15, 7, 13, 3, 11, 0, 4, 2, 20, 5, 10, 16, 14, 6, 19, 18, 12, 1, 17, 8, 9] = 6430238494482930297
 6844249986266452118: [18, 20, 11, 19, 10, 12, 8, 9, 3, 13, 7, 15, 0, 1, 6, 5, 14, 17, 4, 16, 2] = 6844249986266452118
12804085840772788456: [8, 4, 14, 2, 5, 12, 19, 0, 9, 17, 11, 7, 16, 1, 20, 6, 10, 15, 18, 3, 13] = 12804085840772788456
```


## FreeBASIC


### Up to 20 objects


```freebasic
' version 31-03-2017
' compile with: fbc -s console

' Myrvold and Ruskey
' only for up to 20 elements, 21! > 2^64 -1
Function Factorial(n As Integer) As ULongInt

    Dim As ULongInt tmp = 1

    For i As ULong = 2 To n
        tmp *= i
    Next

    Return tmp

End Function

Sub unrank1(n As ULong, r As ULongInt , pi() As UByte)

    If n > 0 Then
        Swap pi(n -1), pi(r Mod n)
        unrank1(n -1, (r \ n), pi())
    End If

End Sub

Function rank1(n As ULongInt, pi() As UByte, pi_inv() As UByte) As ULongInt

    If n = 1 Then Return 0

    Dim As UByte s = pi(n -1)

    Swap pi(n -1), pi(pi_inv(n -1))
    Swap pi_inv(s), pi_inv(n -1)

    Return (s + n * rank1(n -1, pi(), pi_inv()))

End Function

Sub unrank2(n As ULong, r As ULongInt, pi() As ubyte)

    If n > 0 Then
        Dim As ULongInt fac = Factorial(n - 1)
        Dim As ULongint s = r \ fac
        Swap pi(n -1), pi(s)
        unrank2(n -1, r - s * fac, pi())
    End If

End Sub

Function rank2(n As ULong, pi() As UByte, pi_inv() As UByte) As ULongInt

    If n = 1 Then Return 0
    Dim As UByte s = pi(n -1)
    Swap pi(n -1), pi(pi_inv(n -1))
    Swap pi_inv(s), pi_inv(n -1)
    Return (s * Factorial(n -1) + rank2(n -1, pi(), pi_inv()))

End Function

' ------=< MAIN >=------

Dim As ULongInt i, i1, j, n, n1
Dim As UByte pi(), pi_inv()
Dim As String frmt1, frmt2
Randomize timer

n = 3 : n1 = Factorial(n)
ReDim pi(n -1), pi_inv(n - 1)
frmt1 = " ###"
frmt2 = "##"

Print "Rank:     unrank1       rank1"

For i = 0 To n1 -1
    For j = 0 To n -1
        pi(j) = j
    Next
    Print Using frmt1 & ": --> "; i;
    unrank1(n, i, pi())
    For j = 0 To n -1
        Print Using frmt2; pi(j);
        pi_inv(pi(j))= j
    Next

    Print Using "  -->" & frmt1; rank1(n, pi(), pi_inv())

Next

n = 12 : n1 = Factorial(n)
ReDim pi(n -1), pi_inv(n - 1)
frmt1 = "###########"
frmt2 = "###"
Print : Print "4 random samples of permutations from 12 objects"
Print "  Rank:                     unrank1                         rank1"

For i = 1 To 4
    i1 = Int(Rnd * n1)
    For j = 0 To n -1 : pi(j) = j : Next
    Print Using frmt1 & ": --> "; i1; : unrank1(n, i1, pi())
    For j = 0 To n -1 : Print Using frmt2; pi(j);
    pi_inv(pi(j))= j : Next
    Print Using "  -->" & frmt1; rank1(n, pi(), pi_inv())
Next

Print : Print String(69,"-") : Print
Print "Rank:     unrank2       rank2"

n = 3 : n1 = Factorial(n)
ReDim pi(n -1), pi_inv(n - 1)
frmt1 = " ###"
frmt2 = "##"


For i = 0 To n1 -1
    For j = 0 To n -1
        pi(j) = j
    Next
    Print Using  frmt1 & ": --> "; i;
    unrank2(n, i, pi())
    For j = 0 To n -1
        Print Using frmt2; pi(j);
        pi_inv(pi(j))= j
    Next

    Print Using "  -->" & frmt1; rank2(n, pi(), pi_inv())

Next

n = 12 : n1 = Factorial(n)
ReDim pi(n -1), pi_inv(n - 1)
frmt1 = "###########"
frmt2 = "###"
Print : Print "4 random samples of permutations from 12 objects"
Print "  Rank:                     unrank2                         rank2"

For i = 1 To 4
    i1 = Int(Rnd * n1)
    For j = 0 To n -1 : pi(j) = j : Next
    Print Using frmt1 & ": --> "; i1; : unrank2(n, i1, pi())
    For j = 0 To n -1 : Print Using frmt2; pi(j);
    pi_inv(pi(j))= j : Next
    Print Using "  -->" & frmt1; rank2(n, pi(), pi_inv())
Next

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
Rank:     unrank1       rank1
   0: -->  1 2 0  -->   0
   1: -->  2 0 1  -->   1
   2: -->  1 0 2  -->   2
   3: -->  2 1 0  -->   3
   4: -->  0 2 1  -->   4
   5: -->  0 1 2  -->   5

4 random samples of permutations from 12 objects
  Rank:                     unrank1                         rank1
   60255761: -->   1  2  6  9 11  7  4  8 10  3  0  5  -->   60255761
  165590690: -->   9  3  8  0  1  7  6 11  5  4 10  2  -->  165590690
  163838188: -->  10  3  2  5  6  9  1  7  0  8 11  4  -->  163838188
  116369064: -->   2  5  7  1  4 11  6  8 10  3  9  0  -->  116369064

---------------------------------------------------------------------

Rank:     unrank2       rank2
   0: -->  1 2 0  -->   0
   1: -->  2 1 0  -->   1
   2: -->  2 0 1  -->   2
   3: -->  0 2 1  -->   3
   4: -->  1 0 2  -->   4
   5: -->  0 1 2  -->   5

4 random samples of permutations from 12 objects
  Rank:                     unrank2                         rank2
  112880724: -->  10  5  8 11  3  7  6  4  0  1  9  2  -->  112880724
  414249353: -->   7  8  3  6 11  9  2  0  5  1  4 10  -->  414249353
  436729447: -->   2  9  5  1  0  6  7  8  4  3 11 10  -->  436729447
  198756321: -->   0  8 11  1  9  2  5  3  6  7 10  4  -->  198756321
```


### Using GMP for the big numbers

```freebasic
' version 31-03-2017
' compile with: fbc -s console

' Myrvold and Ruskey
#Include Once "gmp.bi"
' next two gmp integer are made shared to make things a little easier
Dim Shared As Mpz_ptr _tmp1_, _tmp2_
_tmp1_ = Allocate(Len( __mpz_struct)) : Mpz_init(_tmp1_)
_tmp2_ = Allocate(Len( __mpz_struct)) : Mpz_init(_tmp2_)

Sub unrank1(n As ULong, rank As Mpz_ptr, pi As String)

    If n > 0 Then
        ' _tmp1_ = quotient, _tmp2_ = remainder
        Mpz_fdiv_qr_ui(_tmp1_, _tmp2_, rank, n)
        Dim As UInteger r = Mpz_get_ui(_tmp2_)
        Swap pi[n -1], pi[r]
        unrank1(n -1, _tmp1_, pi)
    End If

End Sub

Function rank1(n As ULong, pi As String, pi_inv As String) As Mpz_ptr

    Dim As Mpz_ptr ret_val = Allocate( Len( __mpz_struct)) : Mpz_init(ret_val)

    If n = 1 Then Return ret_val ' ret_val = 0

    Dim As UByte s = pi[n -1]

    Swap pi[n -1], pi[pi_inv[n -1]]
    Swap pi_inv[s], pi_inv[n -1]

    _tmp1_ = rank1(n -1, pi, pi_inv)
    Mpz_mul_ui(_tmp1_, _tmp1_, n)
    Mpz_add_ui(_tmp1_, _tmp1_, s)

    Return _tmp1_

End Function

' ------=< MAIN >=------
Dim As Mpz_ptr rank_nr = Allocate( Len( __mpz_struct)) : Mpz_init(rank_nr)
Dim As Mpz_ptr max_nr  = Allocate( Len( __mpz_struct)) : Mpz_init(max_nr)

Dim As ULong i, j, n = 144
Dim As String tmp, pi_start, pi = Space(144), pi_inv = pi
Dim As ZString Ptr gmp_str : gmp_str = Allocate(1000)

Mpz_fac_ui(max_nr, n)

Randomize Timer
Dim Shared As __gmp_randstate_struct rnd_
Gmp_randinit_mt(@rnd_) ' Mersenne Twister

For i = 0 To 200 ' create seed
    tmp += Str(Int(Rnd * 10))
Next

Mpz_set_str(_tmp1_, tmp, 10)
Gmp_randseed(@rnd_, _tmp1_)   ' seed the random generator

' set random generator give number from 0 to max_nr -1
Mpz_fac_ui(max_nr, n)

' setup the starting position
For j = 0 To n -1
    pi[j] = j
Next

pi_start = pi

For i = 1 To 4

    pi = pi_start

    Mpz_urandomm(rank_nr, @rnd_, max_nr)
    ' comment out the next 2 lines if you don't want the rank number
    gmp_str = Mpz_get_str(0, 10, rank_nr)
    Print *gmp_str

    unrank1(n, rank_nr, pi)
    For j = 0 To n -1
        Print pi[j]; " ";
    Next
    Print : Print

Next

' test rank1
For j = 0 To n -1
    pi_inv[pi[j]] = j
Next

Print "Calculate rank from last return of unrank1" : Print

_tmp2_ = rank1(n, pi, pi_inv)
gmp_str = Mpz_get_str(0, 10, _tmp2_)
Print *gmp_str

Print
If Mpz_cmp(rank_nr, _tmp2_) = 0 Then
    Print "Both numbers are equal"
Else
    Print "Oh no, they are different"
End If

' clean up
Gmp_randclear(@rnd_): DeAllocate(gmp_str)
Mpz_clear(_tmp1_)  : Mpz_clear(_tmp2_)
Mpz_clear(rank_nr) : Mpz_clear(max_nr)

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
862993243747395020367391904490190117106585269146708404403331561502140758428008657463653366643611155380240799622282374456087743515643868809722278635878978733132591027133490410275442362000562723356064257191755491520940533177124123076535632269113257248
84 75 30 143 96 137 6 93 1 7 139 13 128 26 54 53 58 80 49 121 48 57 110 18 65 39 28 123 104 74 0 115 118 55 94 124 79 3 40 37 31 19 91 15 45 4 99 85 12 125 73 105 50 103 2 72 100 106 140 59 131 111 132 71 87 101 33 83 116 61 117 46 23 102 22 78 34 122 97 67 21 66 38 95 133 56 51 119 134 130 76 136 10 5 108 86 98 64 68 14 89 127 36 126 29 32 42 135 107 47 88 141 109 82 90 138 41 44 9 113 81 43 77 25 17 52 69 70 20 60 129 92 120 142 24 11 112 27 63 8 114 62 35 16

47917723140095432157221855096154499194255080002636063395350120073895688582070571512322163408396671848674709640997560726617711987824266984538507398611573887009966471806599805673347764192963174688919909816413311485279097811517332552325468088920570999
51 66 35 132 62 41 85 11 102 10 8 77 47 86 67 133 7 113 68 63 55 14 2 53 136 123 120 134 98 44 82 135 38 56 33 3 18 73 59 39 24 87 60 49 100 130 54 125 142 12 143 36 108 79 88 52 48 40 70 121 97 106 27 50 84 81 101 103 69 104 65 80 127 21 129 138 71 83 0 31 109 61 91 42 9 89 25 28 37 1 57 111 94 64 90 58 128 139 72 17 22 29 78 137 34 95 45 122 43 4 112 32 105 119 116 114 46 140 5 126 117 141 99 75 74 16 92 93 30 15 76 124 131 19 6 118 115 13 110 107 26 20 96 23

637688984437952365760382441209327118273063325774553997216592843807533086351173589568994187460815088373920825908715346187961371659995682060813263279599005102426783594559898201756229325528014412155293729428911717526485652807912451026347128904648868007
19 83 140 55 33 120 47 36 25 67 88 37 96 56 81 45 0 119 40 136 118 132 106 4 51 21 94 124 115 48 43 14 50 65 117 6 12 42 91 135 69 101 125 113 93 1 100 63 139 8 85 134 22 44 38 92 107 18 29 73 123 78 59 137 16 98 114 103 58 80 57 77 84 129 102 17 111 46 76 122 13 141 68 143 23 74 90 20 70 7 61 53 121 99 52 126 133 72 34 15 30 75 3 27 116 86 54 105 32 95 97 41 110 71 79 62 131 9 128 26 49 109 35 2 130 82 104 142 66 138 127 60 24 64 112 11 31 5 10 108 89 28 39 87

535399200299805723659842638329571779126013359471038683394020671731433287381148973163739727170996720550440081854802426694781596591608094704788192315510887507170772714624651896802083185333111090759049957219000637867635603301395042723152111042332094947
57 21 94 2 117 69 46 105 86 43 109 25 101 62 50 89 121 7 142 95 113 136 51 15 14 111 54 66 61 1 138 116 23 19 123 129 12 134 82 127 73 100 78 39 37 135 63 90 139 28 67 58 122 49 125 99 60 4 44 141 76 71 11 108 59 110 9 112 5 92 16 124 131 56 48 72 68 40 74 34 130 55 133 42 119 132 97 22 118 52 140 83 103 27 32 84 96 65 10 107 88 18 30 87 115 120 93 70 35 64 75 91 8 126 20 31 98 128 104 13 80 77 0 106 6 33 17 36 41 24 102 137 81 38 45 53 79 143 29 26 114 47 85 3

Calculate rank from last return of unrank1

535399200299805723659842638329571779126013359471038683394020671731433287381148973163739727170996720550440081854802426694781596591608094704788192315510887507170772714624651896802083185333111090759049957219000637867635603301395042723152111042332094947

Both numbers are equal
```



## Go


```go
package main

import (
    "fmt"
    "math/rand"
)

// returns permutation q of n items, using Myrvold-Ruskey rank.
func MRPerm(q, n int) []int {
    p := ident(n)
    var r int
    for n > 0 {
        q, r = q/n, q%n
        n--
        p[n], p[r] = p[r], p[n]
    }
    return p
}

// returns identity permutation of n items.
func ident(n int) []int {
    p := make([]int, n)
    for i := range p {
        p[i] = i
    }
    return p
}

// returns Myrvold-Ruskey rank of permutation p
func MRRank(p []int) (r int) {
    p = append([]int{}, p...)
    inv := inverse(p)
    for i := len(p) - 1; i > 0; i-- {
        s := p[i]
        p[inv[i]] = s
        inv[s] = inv[i]
    }
    for i := 1; i < len(p); i++ {
        r = r*(i+1) + p[i]
    }
    return
}

// returns inverse of a permutation.
func inverse(p []int) []int {
    r := make([]int, len(p))
    for i, x := range p {
        r[x] = i
    }
    return r
}

// returns n!
func fact(n int) (f int) {
    for f = n; n > 2; {
        n--
        f *= n
    }
    return
}

func main() {
    n := 3
    fmt.Println("permutations of", n, "items")
    f := fact(n)
    for i := 0; i < f; i++ {
        p := MRPerm(i, n)
        fmt.Println(i, p, MRRank(p))
    }
    n = 12
    fmt.Println("permutations of", n, "items")
    f = fact(n)
    m := map[int]bool{}
    for len(m) < 4 {
        r := rand.Intn(f)
        if m[r] {
            continue
        }
        m[r] = true
        fmt.Println(r, MRPerm(r, n))
    }
}
```

```txt

permutations of 3 items
0 [1 2 0] 0
1 [2 0 1] 1
2 [1 0 2] 2
3 [2 1 0] 3
4 [0 2 1] 4
5 [0 1 2] 5
permutations of 12 items
340494881 [4 2 3 9 0 8 10 11 1 6 7 5]
469128647 [7 6 10 5 2 3 1 0 8 4 9 11]
460982459 [4 9 5 7 0 8 6 10 2 1 3 11]
432900481 [0 6 5 10 8 2 4 7 3 9 11 1]

```



## Haskell

Without the random part.

```haskell
fact :: Int -> Int
fact n = product [1 .. n]

-- Always assume elements are unique.

rankPerm [] _ = []
rankPerm list n = c : rankPerm (a ++ b) r
  where
    (q, r) = n `divMod` fact (length list - 1)
    (a, c:b) = splitAt q list

permRank [] = 0
permRank (x:xs) = length (filter (< x) xs) * fact (length xs) + permRank xs

main :: IO ()
main = mapM_ f [0 .. 23]
  where
    f n = print (n, p, permRank p)
      where
        p = rankPerm [0 .. 3] n
```

from rank to permutation back to rank:

```txt

(0,[0,1,2,3],0)
(1,[0,1,3,2],1)
(2,[0,2,1,3],2)
(3,[0,2,3,1],3)
(4,[0,3,1,2],4)
(5,[0,3,2,1],5)
(6,[1,0,2,3],6)
(7,[1,0,3,2],7)
(8,[1,2,0,3],8)
(9,[1,2,3,0],9)
(10,[1,3,0,2],10)
(11,[1,3,2,0],11)
(12,[2,0,1,3],12)
(13,[2,0,3,1],13)
(14,[2,1,0,3],14)
(15,[2,1,3,0],15)
(16,[2,3,0,1],16)
(17,[2,3,1,0],17)
(18,[3,0,1,2],18)
(19,[3,0,2,1],19)
(20,[3,1,0,2],20)
(21,[3,1,2,0],21)
(22,[3,2,0,1],22)
(23,[3,2,1,0],23)

```



## J

The J primitive <code>A.</code> provides an effective solution to this task. Generating 4 random permutations of 144 items takes about 6 milliseconds so solving the Stack Overflow question ought to take about 100 minutes on that particular test machine.


```j
   A. 2 0 1                 NB. return rank of permutation
4
   4 A. i.3                 NB. return permutation of rank 4
2 0 1
   0 1 2 3 4 5 A. i. 3      NB. generate all 6 permutations for 3 items
0 1 2
0 2 1
1 0 2
1 2 0
2 0 1
2 1 0
   A. 0 1 2 3 4 5 A. i. 3   NB. ranks of each permuation
0 1 2 3 4 5
   ]ranks=: 4 ? !12         NB. 4 random numbers sampled from integers 0 to 12!
315645285 249293994 432230943 23060830
   ranks A. i.12            NB. 4 random samples of 12 items
 7 10 11  8  4 0  2 3 9 5  6 1
 6  2  8 11 10 0  5 9 7 1  3 4
10  9  1  2  0 3  8 6 7 5 11 4
 0  7  4  6 11 5 10 3 9 8  1 2
   (4 ?@$ !144x) A. i.144   NB. 4 random samples of 144 items
117 36 129  85 128  95 27  14 15 119 45 60  21  98 135 106 18 64 132  97  79  84  35 139 101  75  59 13 141  99 86  40 10 140 23  92 125   6 68  41  69  20  56  12 127 65 142 116  71  54   1   5 121  8  78  73 48 30 80 131 111  57 66 100 138  77 37 124 136...
111 65 136  58  92  46  4  83 20  54 21 10  72 110  56  28 13 18  73 133 105 117  63 126 114  43   5 80  45  88 86 108 11  29  0 129  71 141 59  53 113 137   2 102  95 15  35  74 107  61 134  36  32 19 106 100 55 69 76 142  64  49  9  30  47 123 12  97  42...
 64 76 139 122  37 127 57 143 32 108 46 17 126   9  51  59  1 74  23  89  42 124 132  19  93 137  70 86  14 112 83  91 63  39 73  18  90 120 53 103 140  87  43  55 131 40 142 102 107 111  80  65  61 34  66  75 88 92 13 138  50 117 97  20  44   7 56  94  41...
139 87  98 118 125  65 35 112 10  43 85 66  58 131  36  30 50 11 136 130  71 100  79 142  40  69 101 84 143  33 95  26 18  94 13  68   8   0 47  70 129  48 107  64  93 16  83  39  29  81   6 105  78 92 104  60 15 55  4  14   7  91 86  12  31  46 20 133  53...
```



## Java


The basic approach used is to consider the rank a variable-base number, where the symbol set used to encode each digit is the set of all symbols available, minus the symbols already used by digits to the left. Because the symbol used and its numerical value are easily conflated, it may be easier to think of the digits using letters. If n = 5, we may have an ordered symbol set [A, B, C, D, E]. If the leftmost digits of the permutation used B and C, the set is reduced to [A, D, E]. So if E is encountered in the next digit, it has a numerical value of 2 (its index within the symbol set).

Regarding generating a random permutation, the code generates a random rank and converts it to a permutation and back to a rank, to demonstrate that it functions correctly. However, if random permutations were all that was needed, a simpler approach would be to generate random ranks that were already in variable-base form (int[] rather than a large BigInteger). This would reduce the CPU/memory requirements, as it wouldn't need to do arbitrary-precision arithmetic, and could instead do everything with 32-bit ints. The method would simply translate the int[] variable-base rank into an int[] permutation.

'''Code:'''


```java
import java.math.BigInteger;
import java.util.*;

class RankPermutation
{
  public static BigInteger getRank(int[] permutation)
  {
    int n = permutation.length;
    BitSet usedDigits = new BitSet();
    BigInteger rank = BigInteger.ZERO;
    for (int i = 0; i < n; i++)
    {
      rank = rank.multiply(BigInteger.valueOf(n - i));
      int digit = 0;
      int v = -1;
      while ((v = usedDigits.nextClearBit(v + 1)) < permutation[i])
        digit++;
      usedDigits.set(v);
      rank = rank.add(BigInteger.valueOf(digit));
    }
    return rank;
  }

  public static int[] getPermutation(int n, BigInteger rank)
  {
    int[] digits = new int[n];
    for (int digit = 2; digit <= n; digit++)
    {
      BigInteger divisor = BigInteger.valueOf(digit);
      digits[n - digit] = rank.mod(divisor).intValue();
      if (digit < n)
        rank = rank.divide(divisor);
    }
    BitSet usedDigits = new BitSet();
    int[] permutation = new int[n];
    for (int i = 0; i < n; i++)
    {
      int v = usedDigits.nextClearBit(0);
      for (int j = 0; j < digits[i]; j++)
        v = usedDigits.nextClearBit(v + 1);
      permutation[i] = v;
      usedDigits.set(v);
    }
    return permutation;
  }

  public static void main(String[] args)
  {
    for (int i = 0; i < 6; i++)
    {
      int[] permutation = getPermutation(3, BigInteger.valueOf(i));
      System.out.println(String.valueOf(i) + " --> " + Arrays.toString(permutation) + " --> " + getRank(permutation));
    }
    Random rnd = new Random();
    for (int n : new int[] { 12, 144 })
    {
      BigInteger factorial = BigInteger.ONE;
      for (int i = 2; i <= n; i++)
        factorial = factorial.multiply(BigInteger.valueOf(i));
      // Create 5 random samples
      System.out.println("n = " + n);
      for (int i = 0; i < 5; i++)
      {
        BigInteger rank = new BigInteger((factorial.bitLength() + 1) << 1, rnd);
        rank = rank.mod(factorial);
        int[] permutation = getPermutation(n, rank);
        System.out.println("  " + rank + " --> " + Arrays.toString(permutation) + " --> " + getRank(permutation));
      }
    }
  }

}
```


```txt
0 --> [0, 1, 2] --> 0
1 --> [0, 2, 1] --> 1
2 --> [1, 0, 2] --> 2
3 --> [1, 2, 0] --> 3
4 --> [2, 0, 1] --> 4
5 --> [2, 1, 0] --> 5
n = 12
  459460043 --> [11, 5, 7, 1, 3, 8, 10, 6, 2, 9, 4, 0] --> 459460043
  242238791 --> [6, 0, 9, 5, 11, 2, 8, 4, 10, 7, 3, 1] --> 242238791
  43886594 --> [1, 2, 0, 11, 6, 8, 7, 9, 3, 5, 4, 10] --> 43886594
  356431614 --> [8, 11, 2, 3, 0, 6, 10, 5, 4, 1, 7, 9] --> 356431614
  344630629 --> [8, 6, 11, 7, 3, 0, 5, 10, 4, 1, 9, 2] --> 344630629
n = 144
  4081840330208521662873206235509318516433197707528534675764597914562081781405735597462986112994867493592438058582576097677821221600974033669483476390594977992091821541791590292404824493917612378402336513544253687968370430594214991000754150383016996226 --> [105, 129, 133, 132, 96, 116, 91, 74, 128, 49, 29, 20, 118, 82, 61, 107, 63, 43, 115, 86, 25, 8, 102, 122, 18, 66, 67, 71, 47, 143, 1, 44, 109, 111, 131, 52, 58, 117, 130, 135, 48, 76, 101, 87, 142, 139, 33, 103, 31, 51, 136, 120, 55, 34, 78, 62, 77, 140, 9, 119, 99, 10, 27, 46, 2, 110, 80, 73, 53, 17, 68, 125, 35, 41, 6, 75, 92, 124, 16, 36, 26, 19, 7, 114, 59, 112, 108, 106, 90, 11, 70, 69, 113, 89, 134, 30, 84, 138, 22, 79, 100, 28, 94, 0, 57, 121, 141, 127, 39, 50, 15, 24, 64, 72, 60, 83, 81, 137, 42, 3, 14, 54, 98, 21, 4, 38, 65, 45, 123, 85, 95, 5, 93, 40, 56, 13, 23, 12, 97, 126, 37, 104, 32, 88] --> 4081840330208521662873206235509318516433197707528534675764597914562081781405735597462986112994867493592438058582576097677821221600974033669483476390594977992091821541791590292404824493917612378402336513544253687968370430594214991000754150383016996226
  2303393071343830907186150492848666018688104049718820526188554488347193604795830950201535244280363069858008773693396780501147779890336031158296935978114061601636771344904185209112272057234471902872428445042702453836810958756677409979103815382798559553 --> [59, 109, 108, 98, 87, 75, 50, 8, 51, 44, 78, 63, 19, 127, 140, 45, 115, 128, 35, 26, 6, 39, 2, 7, 94, 14, 66, 5, 9, 31, 36, 68, 142, 138, 124, 130, 71, 129, 97, 58, 12, 132, 40, 80, 139, 143, 118, 79, 46, 137, 116, 111, 117, 136, 103, 100, 43, 24, 65, 70, 73, 67, 76, 106, 102, 17, 134, 49, 54, 33, 0, 77, 89, 92, 56, 47, 42, 1, 57, 32, 30, 62, 25, 83, 29, 15, 126, 13, 55, 131, 4, 10, 85, 84, 91, 18, 121, 88, 104, 28, 95, 60, 72, 120, 114, 21, 133, 105, 37, 38, 3, 11, 122, 61, 74, 52, 16, 113, 96, 81, 99, 23, 101, 141, 69, 93, 110, 82, 34, 27, 123, 135, 86, 125, 90, 112, 48, 41, 53, 22, 64, 107, 119, 20] --> 2303393071343830907186150492848666018688104049718820526188554488347193604795830950201535244280363069858008773693396780501147779890336031158296935978114061601636771344904185209112272057234471902872428445042702453836810958756677409979103815382798559553
  848047014832080341751646538335377058839867620964996131754382188532880389234227596454312311520517451021055848535846150020108831781620073357956660780586648787723957832482692127095007493517434391511473891562806437927741246265623743953254070095131510626 --> [22, 0, 47, 4, 3, 10, 27, 40, 139, 30, 75, 21, 66, 68, 131, 7, 73, 79, 23, 135, 35, 126, 116, 62, 86, 140, 71, 113, 2, 114, 9, 76, 1, 132, 133, 48, 65, 78, 107, 12, 29, 16, 96, 8, 121, 56, 64, 108, 129, 50, 6, 119, 124, 13, 34, 33, 84, 111, 26, 141, 20, 120, 87, 142, 134, 55, 97, 25, 106, 39, 91, 49, 46, 72, 14, 19, 89, 63, 60, 54, 94, 11, 98, 31, 88, 101, 110, 37, 70, 95, 112, 143, 123, 41, 117, 92, 74, 90, 109, 115, 18, 130, 24, 5, 137, 138, 99, 77, 82, 118, 57, 43, 53, 102, 42, 105, 127, 61, 103, 59, 80, 45, 52, 15, 93, 83, 122, 100, 51, 128, 44, 32, 17, 38, 104, 81, 85, 36, 125, 67, 136, 28, 58, 69] --> 848047014832080341751646538335377058839867620964996131754382188532880389234227596454312311520517451021055848535846150020108831781620073357956660780586648787723957832482692127095007493517434391511473891562806437927741246265623743953254070095131510626
  3867119554188057320189830921237689379982446172130048814726708614069209339326961586538911571940960420075292018388619717817588596221122134526830668232334295443338743434633976186486468178683660450737612737202079329785249513121919049677640145391966805396 --> [100, 47, 42, 69, 16, 43, 66, 107, 73, 79, 12, 80, 41, 50, 9, 126, 95, 36, 26, 51, 123, 45, 52, 3, 93, 29, 83, 17, 82, 5, 81, 85, 131, 122, 113, 6, 75, 28, 59, 64, 138, 20, 74, 114, 27, 65, 105, 116, 62, 142, 141, 35, 115, 4, 49, 78, 2, 97, 130, 89, 110, 57, 90, 127, 72, 119, 44, 13, 99, 112, 118, 103, 77, 125, 92, 133, 104, 60, 76, 70, 23, 53, 55, 38, 108, 84, 96, 54, 128, 140, 25, 11, 24, 7, 124, 136, 8, 111, 46, 63, 31, 1, 102, 67, 94, 0, 132, 37, 91, 10, 101, 22, 34, 68, 14, 71, 21, 87, 30, 40, 129, 120, 18, 58, 61, 86, 56, 143, 32, 33, 15, 88, 137, 98, 106, 109, 135, 134, 48, 139, 121, 39, 19, 117] --> 3867119554188057320189830921237689379982446172130048814726708614069209339326961586538911571940960420075292018388619717817588596221122134526830668232334295443338743434633976186486468178683660450737612737202079329785249513121919049677640145391966805396
  4939977610738364532346788397924709243352263476618646232552639571823537393872611379891069437022354024663565745556550101222893478863124970842071831822047445469519025638779057663111504542116305321498448757808078281473228131641155000612722388784688217279 --> [128, 23, 97, 115, 131, 15, 21, 61, 90, 116, 32, 80, 59, 137, 7, 63, 43, 55, 11, 83, 27, 138, 114, 40, 122, 4, 132, 125, 54, 25, 95, 111, 72, 84, 17, 13, 31, 10, 16, 52, 126, 129, 91, 18, 47, 53, 34, 119, 57, 41, 110, 134, 108, 58, 127, 82, 66, 70, 33, 9, 98, 142, 100, 121, 30, 105, 36, 120, 48, 2, 28, 37, 5, 46, 44, 71, 107, 45, 19, 141, 86, 76, 109, 143, 118, 3, 130, 89, 73, 42, 56, 94, 35, 67, 136, 12, 74, 123, 24, 64, 93, 26, 14, 112, 88, 29, 77, 60, 85, 6, 0, 96, 103, 62, 50, 124, 8, 135, 22, 79, 68, 139, 78, 101, 20, 117, 51, 133, 81, 102, 39, 99, 113, 38, 104, 69, 106, 75, 92, 87, 49, 1, 140, 65] --> 4939977610738364532346788397924709243352263476618646232552639571823537393872611379891069437022354024663565745556550101222893478863124970842071831822047445469519025638779057663111504542116305321498448757808078281473228131641155000612722388784688217279
```



## Julia

Julia has native support for permutations.  Depending upon its arguments, <tt>nthperm</tt> will either return the rank of a permutation or permute a vector according to the provided rank.  <tt>randperm(n)</tt> returns a random permutation of <tt>n</tt> objects.

Note that, because Julia uses 1-based array indexing, permutations consists of lists of [1, ..., <tt>n</tt>] rather than the [0, ..., <tt>n-1</tt>] used for many of the other solutions to this task.  Also, Julian partition ranks range from 1 to <tt>n!</tt> rather than from 0 to <tt>n!-1</tt>.

Unfortunately, as of the 0.3 release of Julian, this code can not be used to address the StackOverflow question.  Although many of Julia's permutation built-in functions support large permutations, <tt>nthperm(p)</tt> is limited to partitions of no more than 20 objects.  <tt>p = randperm(114)</tt> works fine, but <tt>nthperm(p)</tt> throws an <tt>OverflowError</tt>.  Arguably, this is a bug, and it may be rectified in future releases.

```Julia

nobjs = 4
a = collect(1:nobjs)
println("All permutations of ", nobjs, " objects:")
for i in 1:factorial(nobjs)
    p = nthperm(a, i)
    prank = nthperm(p)
    print(@sprintf("%5d => ", i))
    println(p, " (", prank, ")")
end

nobjs = 12
nsamp = 4
ptaken = Int[]
println()
println(nsamp, " random permutations of ", nobjs, " objects:")
for i in 1:nsamp
    p = randperm(nobjs)
    prank = nthperm(p)
    while prank in ptaken
        p = randperm(nobjs)
        prank = nthperm(p)
    end
    push!(ptaken, prank)
    println("         ", p, " (", prank, ")")
end

```


```txt

All permutations of 4 objects:
    1 => [1,2,3,4] (1)
    2 => [1,2,4,3] (2)
    3 => [1,3,2,4] (3)
    4 => [1,3,4,2] (4)
    5 => [1,4,2,3] (5)
    6 => [1,4,3,2] (6)
    7 => [2,1,3,4] (7)
    8 => [2,1,4,3] (8)
    9 => [2,3,1,4] (9)
   10 => [2,3,4,1] (10)
   11 => [2,4,1,3] (11)
   12 => [2,4,3,1] (12)
   13 => [3,1,2,4] (13)
   14 => [3,1,4,2] (14)
   15 => [3,2,1,4] (15)
   16 => [3,2,4,1] (16)
   17 => [3,4,1,2] (17)
   18 => [3,4,2,1] (18)
   19 => [4,1,2,3] (19)
   20 => [4,1,3,2] (20)
   21 => [4,2,1,3] (21)
   22 => [4,2,3,1] (22)
   23 => [4,3,1,2] (23)
   24 => [4,3,2,1] (24)

4 random permutations of 12 objects:
         [5,9,4,1,11,12,6,8,10,2,7,3] (186192332)
         [10,12,4,3,2,9,7,1,8,6,11,5] (396717496)
         [8,1,2,5,10,3,11,9,12,7,6,4] (279524016)
         [1,10,4,12,6,5,8,9,3,2,7,11] (30095719)

```



## Kotlin

```scala
// version 1.1.2

import java.util.Random

fun IntArray.swap(i: Int, j: Int) {
    val temp = this[i]
    this[i] = this[j]
    this[j] = temp
}

tailrec fun mrUnrank1(rank: Int, n: Int, vec: IntArray) {
    if (n < 1) return
    val q = rank / n
    val r = rank % n
    vec.swap(r, n - 1)
    mrUnrank1(q, n - 1, vec)
}

fun mrRank1(n: Int, vec: IntArray, inv: IntArray): Int {
    if (n < 2) return 0
    val s = vec[n - 1]
    vec.swap(n - 1, inv[n - 1])
    inv.swap(s, n - 1)
    return s + n * mrRank1(n - 1, vec, inv)
}

fun getPermutation(rank: Int, n: Int, vec: IntArray) {
    for (i in 0 until n) vec[i] = i
    mrUnrank1(rank, n, vec)
}

fun getRank(n: Int, vec: IntArray): Int {
    val v   = IntArray(n)
    val inv = IntArray(n)
    for (i in 0 until n) {
        v[i] = vec[i]
        inv[vec[i]] = i
    }
    return mrRank1(n, v, inv)
}

fun main(args: Array<String>) {
    var tv = IntArray(3)
    for (r in 0..5) {
        getPermutation(r, 3, tv)
        System.out.printf("%2d -> %s -> %d\n", r, tv.contentToString(), getRank(3, tv))
    }
    println()
    tv = IntArray(4)
    for (r in 0..23) {
        getPermutation(r, 4, tv)
        System.out.printf("%2d -> %s -> %d\n", r, tv.contentToString(), getRank(4, tv))
    }

    println()
    tv = IntArray(12)
    val a = IntArray(4)
    val rand = Random()
    val fact12 = (2..12).fold(1) { acc, i -> acc * i }
    for (i in 0..3) a[i] = rand.nextInt(fact12)
    for (r in a) {
        getPermutation(r, 12, tv)
        System.out.printf("%9d -> %s -> %d\n", r, tv.contentToString(), getRank(12, tv))
    }
}
```


```txt

 0 -> [1, 2, 0] -> 0
 1 -> [2, 0, 1] -> 1
 2 -> [1, 0, 2] -> 2
 3 -> [2, 1, 0] -> 3
 4 -> [0, 2, 1] -> 4
 5 -> [0, 1, 2] -> 5

 0 -> [1, 2, 3, 0] -> 0
 1 -> [3, 2, 0, 1] -> 1
 2 -> [1, 3, 0, 2] -> 2
 3 -> [1, 2, 0, 3] -> 3
 4 -> [2, 3, 1, 0] -> 4
 5 -> [2, 0, 3, 1] -> 5
 6 -> [3, 0, 1, 2] -> 6
 7 -> [2, 0, 1, 3] -> 7
 8 -> [1, 3, 2, 0] -> 8
 9 -> [3, 0, 2, 1] -> 9
10 -> [1, 0, 3, 2] -> 10
11 -> [1, 0, 2, 3] -> 11
12 -> [2, 1, 3, 0] -> 12
13 -> [2, 3, 0, 1] -> 13
14 -> [3, 1, 0, 2] -> 14
15 -> [2, 1, 0, 3] -> 15
16 -> [3, 2, 1, 0] -> 16
17 -> [0, 2, 3, 1] -> 17
18 -> [0, 3, 1, 2] -> 18
19 -> [0, 2, 1, 3] -> 19
20 -> [3, 1, 2, 0] -> 20
21 -> [0, 3, 2, 1] -> 21
22 -> [0, 1, 3, 2] -> 22
23 -> [0, 1, 2, 3] -> 23

323620184 -> [9, 2, 4, 5, 1, 7, 3, 10, 6, 11, 0, 8] -> 323620184
100091296 -> [2, 10, 6, 9, 5, 0, 3, 8, 1, 7, 11, 4] -> 100091296
310989081 -> [8, 10, 0, 6, 2, 5, 3, 1, 4, 7, 11, 9] -> 310989081
259044329 -> [6, 1, 3, 8, 4, 9, 2, 11, 10, 7, 0, 5] -> 259044329

```



## Mathematica

```mathematica
fromrank[list_, 0] := list;
fromrank[list_, n_] :=
  Prepend[fromrank[DeleteCases[list, #],
      Mod[n, (Length@list - 1)!]], #] &@
   RankedMin[list, Quotient[n, (Length@list - 1)!] + 1];

rank[{}] = 0;
rank[{x_, y___}] := Count[{y}, _?(# < x &)] Length@{y}! + rank[{y}];

Print /@ Table[{n, fromrank[{0, 1, 2, 3}, n],
    rank@fromrank[{0, 1, 2, 3}, n]}, {n, 0, 23}];

Do[Print@fromrank[Range[0, 12 - 1], RandomInteger[12!]], {4}];
Do[Print@fromrank[Range[0, 144 - 1], RandomInteger[144!]], {4}];
```

```txt
{0,{0,1,2,3},0}
{1,{0,1,3,2},1}
{2,{0,2,1,3},2}
{3,{0,2,3,1},3}
{4,{0,3,1,2},4}
{5,{0,3,2,1},5}
{6,{1,0,2,3},6}
{7,{1,0,3,2},7}
{8,{1,2,0,3},8}
{9,{1,2,3,0},9}
{10,{1,3,0,2},10}
{11,{1,3,2,0},11}
{12,{2,0,1,3},12}
{13,{2,0,3,1},13}
{14,{2,1,0,3},14}
{15,{2,1,3,0},15}
{16,{2,3,0,1},16}
{17,{2,3,1,0},17}
{18,{3,0,1,2},18}
{19,{3,0,2,1},19}
{20,{3,1,0,2},20}
{21,{3,1,2,0},21}
{22,{3,2,0,1},22}
{23,{3,2,1,0},23}
{8,1,10,2,3,6,4,5,9,0,7,11}
{9,0,11,2,3,6,8,10,1,5,7,4}
{6,4,7,0,3,10,5,11,1,9,2,8}
{7,11,0,3,1,6,10,2,4,5,8,9}
{117,128,29,8,33,81,15,54,132,64,80,6,20,111,22,44,49,28,96,40,55,66,58,126,16,97,26,41,57,32,39,138,71,83,3,76,110,107,103,31,88,141,5,50,67,19,122,127,35,47,93,23,106,84,116,108,69,14,4,139,123,62,133,78,87,131,102,134,74,13,1,104,0,135,72,100,37,113,61,89,34,105,82,25,129,46,43,95,109,65,121,70,73,85,48,120,143,51,98,79,101,56,68,52,24,119,140,17,18,45,11,60,91,7,90,10,27,59,53,21,99,36,115,63,2,12,38,92,118,125,136,112,137,124,30,42,9,77,75,86,114,142,130,94}
{77,39,109,18,142,36,22,6,13,12,139,27,92,134,47,30,88,107,21,85,137,70,9,7,26,86,10,63,143,119,116,130,74,72,81,19,108,136,122,104,1,58,24,73,42,45,135,121,8,76,83,14,99,65,141,128,82,124,79,75,87,91,15,95,96,114,11,52,50,129,34,29,125,89,84,71,20,4,140,49,118,16,64,46,68,110,100,62,123,3,131,53,5,113,37,43,48,44,51,105,32,67,115,111,132,38,102,55,0,93,94,23,25,17,80,133,112,78,56,103,138,61,97,2,54,31,60,120,40,41,126,66,117,127,28,35,98,59,57,69,106,33,101,90}
{118,52,6,67,24,8,105,3,55,29,99,111,14,21,0,48,45,80,131,63,76,16,68,25,125,72,47,98,126,75,28,30,85,143,129,90,32,54,119,117,116,88,115,73,123,11,7,78,141,87,114,91,96,37,106,46,31,133,100,20,17,27,42,36,134,138,120,9,66,74,112,33,77,101,104,12,64,121,40,58,43,136,61,135,132,79,81,49,69,19,82,4,53,1,38,84,108,56,70,86,10,89,107,44,15,35,26,5,110,137,62,140,92,113,103,142,97,51,93,65,128,18,95,50,102,23,2,57,13,34,71,130,83,94,39,60,59,109,122,41,127,22,124,139}
{100,48,125,13,85,95,54,128,111,79,10,103,83,52,143,78,16,114,133,105,43,53,104,37,12,5,17,26,68,61,73,141,34,14,138,140,59,21,77,99,29,117,108,62,39,41,45,91,116,25,131,31,118,94,90,46,49,98,51,137,7,101,44,56,50,87,110,120,142,27,135,18,58,113,69,75,42,107,130,86,80,127,123,15,3,1,122,47,134,106,119,92,136,20,55,74,36,65,67,72,81,0,23,96,32,88,126,97,19,64,4,102,76,66,35,132,139,60,129,6,30,124,57,70,71,82,22,112,24,9,115,63,8,33,89,93,84,38,28,11,109,121,2,40}
```


## PARI/GP

The functions are built into GP already: <code>numtoperm</code> and <code>permtonum</code>

```parigp
vector(3!,i,permtonum(numtoperm(3,i-1)))==vector(3!,i,i-1)
vector(4,i,numtoperm(12,random(12!)))
for(i=1,1e6,numtoperm(144,random(144!)))
```

```txt
%1 = 1
%2 = [[3, 11, 7, 9, 10, 5, 2, 12, 8, 1, 4, 6], [11, 9, 6, 1, 5, 3, 10, 2, 7, 4, 12, 8], [12, 3, 10, 6, 7, 4, 9, 11, 2, 8, 1, 5], [6, 10, 7, 2, 9, 12, 11, 4, 8, 3, 1, 5]]
```



## Perl

The ntheory module gives us <code>numtoperm</code> and <code>permtonum</code> commands similar to Pari/GP, though in the preferred lexicographic order.  Values larger than the native int size are handled as well.  The <code>randperm</code> function is useful for random permutations, using a Knuth shuffle and a Chacha/20 CSPRNG supplying randomness.  A Macbook takes a bit under 4 seconds to generate 1 million random permutations of 144 objects, or 8 seconds if inserting into a set to ensure uniqueness.
```perl
use ntheory qw/:all/;

my $n = 3;
print "    Iterate Lexicographic rank/unrank of $n objects\n";
for my $k (0 .. factorial($n)-1) {
  my @perm = numtoperm($n, $k);
  my $rank = permtonum(\@perm);
  die unless $rank == $k;
  printf "%2d --> [@perm] --> %2d\n", $k, $rank;
}
print "\n";

print "    Four 12-object random permutations using ranks\n";
print join(" ", numtoperm(12,urandomm(factorial(12)))), "\n"  for 1..4;
print "\n";
print "    Four 12-object random permutations using randperm\n";
print join(" ", randperm(12)),"\n"  for 1..4;
print "\n";
print "    Four 4-object random permutations of 100k objects using randperm\n";
print join(" ", randperm(100000,4)),"\n"  for 1..4;

```


```txt

    Iterate Lexicographic rank/unrank of 3 objects
 0 --> [0 1 2] -->  0
 1 --> [0 2 1] -->  1
 2 --> [1 0 2] -->  2
 3 --> [1 2 0] -->  3
 4 --> [2 0 1] -->  4
 5 --> [2 1 0] -->  5

    Four 12-object random permutations using ranks
0 6 1 4 2 5 3 7 8 9 11 10
4 2 8 6 0 3 9 10 7 11 5 1
6 1 7 2 4 5 3 0 11 10 8 9
0 2 3 9 10 1 4 7 11 8 6 5

    Four 12-object random permutations using randperm
1 7 5 6 2 9 0 3 10 11 4 8
6 11 7 2 5 4 0 9 3 8 1 10
1 9 6 11 10 8 4 0 2 3 5 7
10 0 1 8 7 4 5 6 11 3 9 2

    Four 4-object random permutations of 100k objects using randperm
51080 2774 79078 28078
38822 21928 77796 36832
6089 14383 3397 31577
61539 80497 47550 53322

```



## Perl 6

It is similar to Haskell, but separate something like [https://en.wikipedia.org/wiki/Inversion_(discrete_mathematics) inversion vector].
It is easy generate random inversion vector without BigInt.


```perl6
use v6;

sub rank2inv ( $rank, $n = * ) {
    $rank.polymod( 1 ..^ $n );
}

sub inv2rank ( @inv ) {
    [+] @inv Z* [\*] 1, 1, * + 1 ... *
}

sub inv2perm ( @inv, @items is copy = ^@inv.elems ) {
    my @perm;
    for @inv.reverse -> $i {
        @perm.append: @items.splice: $i, 1;
    }
    @perm;
}

sub perm2inv ( @perm ) {     #not in linear time
    (
        { @perm[++$ .. *].grep( * < $^cur ).elems } for @perm;
    ).reverse;
}

for ^6 {
    my @row.push: $^rank;
    for ( *.&rank2inv(3) , &inv2perm, &perm2inv, &inv2rank )  -> &code {
        @row.push: code( @row[*-1] );
    }
    say @row;
}

my $perms =  4;      #100;
my $n     = 12;      #144;

say 'Via BigInt rank';
for ( ( ^([*] 1 .. $n) ).pick($perms) ) {
    say $^rank.&rank2inv($n).&inv2perm;
};

say 'Via inversion vectors';
for ( { my $i=0;  inv2perm (^++$i).roll xx $n } ... *  ).unique( with => &[eqv] ).[^$perms] {
    .say;
};

say 'Via Perl 6 method pick';
for ( { [(^$n).pick(*)] } ... * ).unique( with => &[eqv] ).head($perms) {
    .say
};

```

```txt

[0 (0 0 0) [0 1 2] (0 0 0) 0]
[1 (0 1 0) [0 2 1] (0 1 0) 1]
[2 (0 0 1) [1 0 2] (0 0 1) 2]
[3 (0 1 1) [1 2 0] (0 1 1) 3]
[4 (0 0 2) [2 0 1] (0 0 2) 4]
[5 (0 1 2) [2 1 0] (0 1 2) 5]
Via BigInt rank
[4 3 1 8 6 2 0 7 9 11 5 10]
[0 8 11 4 9 3 7 5 2 6 10 1]
[5 7 9 11 10 6 4 1 2 3 0 8]
[9 11 8 6 3 5 7 2 4 0 1 10]
Via inversion vectors
[9 0 3 1 8 2 4 5 11 7 10 6]
[7 3 1 10 0 6 4 11 2 9 8 5]
[9 8 5 11 1 10 0 7 4 6 2 3]
[10 8 6 5 4 9 0 2 11 7 1 3]
Via Perl 6 method pick
[11 0 7 10 9 4 1 8 6 5 2 3]
[4 5 8 3 2 1 7 9 11 0 10 6]
[11 7 9 4 0 8 10 1 5 2 6 3]
[11 10 0 3 4 6 7 9 8 5 1 2]

```



## Phix

```Phix
function get_rank(sequence l)
    integer r = length(l)
    sequence inv = repeat(0,r)
    for i=1 to r do
        inv[l[i]+1] = i-1
    end for
    integer res = 0, mul = 1
    for n=r to 2 by -1 do
        integer s = l[n]
        l[inv[n]+1] = s
        inv[s+1] = inv[n]
        res += s*mul
        mul *= n
    end for
    return res
end function

puts(1,"rank->permute->rank:\n")
sequence l = tagset(2,0)
for n=1 to factorial(length(l)) do
    sequence p = permute(n,l)
    ?{n-1,p,get_rank(p)}
end for

puts(1,"4 random individual samples of 12 items:\n")
l = tagset(11,0)
for i=1 to 4 do
    ?permute(rand(factorial(12)),l)
end for
```

```txt

rank->permute->rank:
{0,{1,2,0},0}
{1,{2,0,1},1}
{2,{1,0,2},2}
{3,{2,1,0},3}
{4,{0,2,1},4}
{5,{0,1,2},5}
4 random individual samples of 12 items:
{4,5,12,1,11,8,0,9,2,6,7,3,10}
{7,2,3,0,5,1,4,6,9,11,8,10,12}
{10,8,12,1,9,3,2,4,11,0,5,6,7}
{3,4,0,8,12,11,7,1,6,5,9,10,2}

```

It is worth noting that while the permute builtin can scramble anything, the get_rank function above

(like most others on this page) can only handle permulations of the integers 0 to n-1. Of course as

long as you permute indexes rather than the data itself, that does not matter.

It proved utterly trivial to convert that to bigatoms, however also utterly pointless, since it would take

over 5 days to generate a million perms, whereas a million shuffle(l) (of 144 items) takes just 20 seconds,

and in fact storing the integer(string) ranks takes twice as much space as 144 small integers anyway.


## Python

===Python: Myrvold & Ruskey===
This is based on the work shown in the paper by Myrvold & Ruskey and has algorithms for the two types of ordering of permutations that they show.

Their algorithm is efficient and pythons transparent use of arbitrary precision integers allows the Stack Overflow questions limits to be used. (Chopped at four rather than a million random samples although function get_random_ranks(144, 1e6) doesn't take long).


```python
from math import factorial as fact
from random import randrange
from textwrap import wrap

def identity_perm(n):
    return list(range(n))

def unranker1(n, r, pi):
    while n > 0:
        n1, (rdivn, rmodn) = n-1, divmod(r, n)
        pi[n1], pi[rmodn] = pi[rmodn], pi[n1]
        n = n1
        r = rdivn
    return pi

def init_pi1(n, pi):
    pi1 = [-1] * n
    for i in range(n):
        pi1[pi[i]] = i
    return pi1

def ranker1(n, pi, pi1):
    if n == 1:
        return 0
    n1 = n-1
    s = pi[n1]
    pi[n1], pi[pi1[n1]] = pi[pi1[n1]], pi[n1]
    pi1[s], pi1[n1] = pi1[n1], pi1[s]
    return s + n * ranker1(n1, pi, pi1)

def unranker2(n, r, pi):
    while n > 0:
        n1 = n-1
        s, rmodf = divmod(r, fact(n1))
        pi[n1], pi[s] = pi[s], pi[n1]
        n = n1
        r = rmodf
    return pi

def ranker2(n, pi, pi1):
    if n == 1:
        return 0
    n1 = n-1
    s = pi[n1]
    pi[n1], pi[pi1[n1]] = pi[pi1[n1]], pi[n1]
    pi1[s], pi1[n1] = pi1[n1], pi1[s]
    return s * fact(n1) + ranker2(n1, pi, pi1)

def get_random_ranks(permsize, samplesize):
    perms = fact(permsize)
    ranks = set()
    while len(ranks) < samplesize:
        ranks |= set( randrange(perms)
                      for r in range(samplesize - len(ranks)) )
    return ranks

def test1(comment, unranker, ranker):
    n, samplesize, n2 = 3, 4, 12
    print(comment)
    perms = []
    for r in range(fact(n)):
        pi = identity_perm(n)
        perm = unranker(n, r, pi)
        perms.append((r, perm))
    for r, pi in perms:
        pi1 = init_pi1(n, pi)
        print('  From rank %2i to %r back to %2i' % (r, pi, ranker(n, pi[:], pi1)))
    print('\n  %i random individual samples of %i items:' % (samplesize, n2))
    for r in get_random_ranks(n2, samplesize):
        pi = identity_perm(n2)
        print('    ' + ' '.join('%2i' % i for i in unranker(n2, r, pi)))
    print('')

def test2(comment, unranker):
    samplesize, n2 = 4, 144
    print(comment)
    print('  %i random individual samples of %i items:' % (samplesize, n2))
    for r in get_random_ranks(n2, samplesize):
        pi = identity_perm(n2)
        print('    ' + '\n      '.join(wrap(repr(unranker(n2, r, pi)))))
    print('')

if __name__ == '__main__':
    test1('First ordering:', unranker1, ranker1)
    test1('Second ordering:', unranker2, ranker2)
    test2('First ordering, large number of perms:', unranker1)
```


```txt
First ordering:
  From rank  0 to [1, 2, 0] back to  0
  From rank  1 to [2, 0, 1] back to  1
  From rank  2 to [1, 0, 2] back to  2
  From rank  3 to [2, 1, 0] back to  3
  From rank  4 to [0, 2, 1] back to  4
  From rank  5 to [0, 1, 2] back to  5

  4 random individual samples of 12 items:
     1  4  5 10  7  3  2  6  9 11  0  8
     0  4  9 11  8 10  7  2  6  5  1  3
    11  8  2  3 10  7  4  6  1  0  5  9
    11  6  7  1  9  5  4  0  2  8  3 10

Second ordering:
  From rank  0 to [1, 2, 0] back to  0
  From rank  1 to [2, 1, 0] back to  1
  From rank  2 to [2, 0, 1] back to  2
  From rank  3 to [0, 2, 1] back to  3
  From rank  4 to [1, 0, 2] back to  4
  From rank  5 to [0, 1, 2] back to  5

  4 random individual samples of 12 items:
     8  9  4 11  7  0  3  6  1 10  2  5
     1  2  0 11  8  6  3  9  4 10  7  5
     9  1  4  8  2 10  3  7  0  6 11  5
     4  7 11  2  5  0  3  8  9  6 10  1

First ordering, large number of perms:
  4 random individual samples of 144 items:
    [92, 32, 141, 93, 97, 45, 70, 134, 60, 5, 99, 4, 13, 80, 68, 100, 77,
      115, 116, 34, 50, 117, 26, 31, 88, 128, 14, 35, 106, 129, 114, 73,
      101, 142, 29, 11, 1, 86, 17, 38, 130, 140, 84, 51, 81, 110, 111, 64,
      24, 3, 16, 6, 139, 104, 103, 8, 75, 62, 43, 113, 137, 48, 22, 53, 30,
      125, 33, 67, 69, 143, 83, 121, 123, 138, 102, 87, 57, 49, 58, 82, 20,
      109, 89, 59, 96, 56, 19, 10, 90, 28, 41, 94, 107, 108, 95, 74, 21, 66,
      40, 25, 46, 78, 44, 112, 124, 36, 135, 42, 132, 79, 37, 63, 15, 2, 61,
      47, 85, 72, 0, 119, 39, 52, 55, 65, 136, 23, 18, 127, 27, 126, 131,
      133, 71, 54, 7, 98, 9, 12, 118, 122, 120, 91, 76, 105]
    [36, 141, 8, 114, 28, 42, 32, 40, 75, 134, 72, 106, 78, 107, 43, 83,
      0, 13, 41, 48, 66, 4, 98, 136, 34, 35, 46, 92, 15, 135, 17, 79, 22,
      118, 95, 137, 81, 121, 60, 74, 110, 33, 80, 14, 62, 125, 3, 56, 115,
      49, 27, 1, 7, 84, 20, 64, 47, 140, 124, 119, 143, 23, 93, 87, 25, 12,
      69, 105, 68, 112, 10, 44, 101, 138, 99, 51, 127, 52, 122, 61, 21, 108,
      117, 39, 2, 85, 53, 130, 120, 18, 16, 109, 126, 103, 113, 5, 82, 19,
      73, 45, 58, 102, 129, 54, 96, 31, 57, 97, 65, 133, 59, 132, 88, 30,
      89, 86, 9, 116, 71, 142, 77, 94, 104, 63, 37, 139, 70, 131, 11, 111,
      123, 128, 24, 76, 90, 29, 6, 38, 100, 26, 55, 91, 50, 67]
    [32, 8, 35, 70, 140, 136, 59, 16, 15, 93, 118, 26, 132, 98, 71, 6,
      107, 19, 65, 11, 42, 97, 78, 85, 28, 96, 119, 82, 44, 2, 125, 58, 117,
      21, 27, 39, 48, 52, 92, 139, 142, 49, 38, 54, 79, 57, 127, 45, 109,
      17, 75, 95, 101, 86, 14, 122, 108, 131, 31, 141, 110, 128, 53, 84, 30,
      23, 66, 41, 13, 37, 36, 130, 113, 46, 50, 47, 115, 7, 100, 51, 137,
      134, 10, 55, 64, 43, 99, 102, 135, 24, 20, 73, 9, 90, 60, 111, 104,
      143, 1, 103, 3, 129, 12, 138, 33, 67, 123, 22, 112, 68, 120, 81, 133,
      91, 61, 25, 124, 56, 40, 121, 4, 18, 114, 126, 80, 106, 83, 5, 94,
      116, 88, 105, 63, 87, 77, 89, 72, 62, 74, 0, 29, 69, 34, 76]
    [28, 68, 101, 27, 40, 79, 73, 2, 75, 97, 135, 23, 17, 95, 58, 26, 80,
      93, 49, 31, 140, 39, 25, 35, 83, 114, 1, 13, 48, 29, 134, 122, 70, 0,
      91, 62, 77, 37, 14, 44, 24, 50, 126, 9, 42, 67, 100, 104, 99, 59, 55,
      21, 123, 138, 33, 116, 127, 115, 82, 61, 117, 130, 43, 86, 22, 12, 56,
      60, 47, 78, 121, 131, 36, 38, 51, 4, 139, 142, 128, 98, 84, 92, 111,
      5, 53, 106, 34, 6, 3, 20, 72, 112, 11, 136, 94, 32, 71, 132, 88, 124,
      85, 110, 108, 137, 30, 89, 74, 120, 8, 102, 41, 81, 129, 107, 63, 118,
      66, 7, 133, 65, 125, 64, 90, 141, 109, 57, 18, 69, 103, 76, 113, 16,
      52, 15, 19, 46, 96, 45, 10, 54, 105, 143, 87, 119]
```



### Python: Iterative

Functions ranker1 and ranker2 can be changed from the recursive form as used in the paper to iterative versions if they are replaced with the following code that yields the same results:

```python
def ranker1(n, pi, pi1):
    if n == 1:
        return 0
    n1 = n-1
    s = pi[n1]
    pi[n1], pi[pi1[n1]] = pi[pi1[n1]], pi[n1]
    pi1[s], pi1[n1] = pi1[n1], pi1[s]
    return s + n * ranker1(n1, pi, pi1)

def ranker2(n, pi, pi1):
    result = 0
    for i in range(n-1, 0, -1):
        s = pi[i]
        pi[i], pi[pi1[i]] = pi[pi1[i]], pi[i]
        pi1[s], pi1[i] = pi1[i], pi1[s]
        result += s * fact(i)
    return result
```



## Racket

Mimicking the Haskell style.

```racket

#lang racket (require math)
(define-syntax def (make-rename-transformer #'match-define-values))

(define (perm xs n)
  (cond [(empty? xs) '()]
        [else (def (q r) (quotient/remainder n (factorial (sub1 (length xs)))))
              (def (a (cons c b)) (split-at xs q))
              (cons c (perm (append a b) r))]))

(define (rank ys)
  (cond [(empty? ys) 0]
        [else (def ((cons x0 xs)) ys)
              (+ (rank xs)
                 (* (for/sum ([x xs] #:when (< x x0)) 1)
                    (factorial (length xs))))]))

```

```txt

(for/list ([n 24])
  (define p (perm '(0 1 2 3) n))
  (list n p (rank p)))

'((0 (0 1 2 3) 0)
  (1 (0 1 3 2) 1)
  (2 (0 2 1 3) 2)
  (3 (0 2 3 1) 3)
  (4 (0 3 1 2) 4)
  (5 (0 3 2 1) 5)
  (6 (1 0 2 3) 6)
  (7 (1 0 3 2) 7)
  (8 (1 2 0 3) 8)
  (9 (1 2 3 0) 9)
  (10 (1 3 0 2) 10)
  (11 (1 3 2 0) 11)
  (12 (2 0 1 3) 12)
  (13 (2 0 3 1) 13)
  (14 (2 1 0 3) 14)
  (15 (2 1 3 0) 15)
  (16 (2 3 0 1) 16)
  (17 (2 3 1 0) 17)
  (18 (3 0 1 2) 18)
  (19 (3 0 2 1) 19)
  (20 (3 1 0 2) 20)
  (21 (3 1 2 0) 21)
  (22 (3 2 0 1) 22)
  (23 (3 2 1 0) 23))

```



## REXX

The   '''permsets'''   subroutine (actually, a function) is a modified version of a REXX subroutine used elsewhere in Rosetta Code.

This modified version starts the permute numbers with   '''0'''   (zero)   instead of   '''1'''.

Since this REXX program generates permutations without recursive calls,
testing for the limit for a   ''stack overflow''   wouldn't be necessary using this REXX program.

```rexx
/*REXX program displays permutations of   N   number of  objects  (1, 2, 3,  ···).      */
parse arg N y seed .                             /*obtain optional arguments from the CL*/
if N=='' | N==","  then N= 4                     /*Not specified?  Then use the default.*/
if y=='' | y==","  then y=17                     /* "      "         "   "   "     "    */
if datatype(seed,'W')  then call random ,,seed   /*can make RANDOM numbers repeatable.  */
permutes= permSets(N)                            /*returns  N! (number of permutations).*/
w= length(permutes)                              /*used for aligning the  SAY  output.  */
@.=
      do p=0  to permutes-1                      /*traipse through each of the permutes.*/
      z=permSets(N, p)                           /*get which of the  permutation  it is.*/
      say 'for'     N     "items, permute rank"      right(p,w)        'is: '        z
      @.p=z                                      /*define a rank permutation in @ array.*/
      end   /*p*/
say                                              /* [↓]  displays a particular perm rank*/
say '  the permutation rank of'  y  "is: "   @.y /*display a particular permuation rank.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
permSets:   procedure expose @. #;     #=0;    parse arg x,r,c;   c=space(c);      xm=x -1
                    do j=1  for x;     @.j=j-1;   end  /*j*/
            _=0;    do u=2  for xm;    _=_ @.u;   end  /*u*/
            if r==#  then return _;            if c==_  then return #
                    do  while .permSets(x,0);  #=#+1;  _=@.1
                       do v=2  for xm;    _=_  @.v;    end  /*v*/
                    if r==#  then return  _;   if c==_  then return #
                    end   /*while···*/
            return #+1
/*──────────────────────────────────────────────────────────────────────────────────────*/
.permSets:  procedure expose @.;       parse arg p,q;    pm=p-1
                  do k=pm  by -1  for pm;   kp=k+1;  if @.k<@.kp  then do; q=k; leave; end
                  end   /*k*/

                  do j=q+1  while j<p;  parse  value  @.j  @.p   with   @.p  @.j;   p=p -1
                  end   /*j*/
            if q==0  then return 0
                  do p=q+1  while @.p<@.q;   end  /*p*/
            parse  value   @.p  @.q   with   @.q  @.p
            return 1
```

```txt

for 4 items, permute rank  0 is:  0 1 2 3
for 4 items, permute rank  1 is:  0 1 3 2
for 4 items, permute rank  2 is:  0 2 1 3
for 4 items, permute rank  3 is:  0 2 3 1
for 4 items, permute rank  4 is:  0 3 1 2
for 4 items, permute rank  5 is:  0 3 2 1
for 4 items, permute rank  6 is:  1 0 2 3
for 4 items, permute rank  7 is:  1 0 3 2
for 4 items, permute rank  8 is:  1 2 0 3
for 4 items, permute rank  9 is:  1 2 3 0
for 4 items, permute rank 10 is:  1 3 0 2
for 4 items, permute rank 11 is:  1 3 2 0
for 4 items, permute rank 12 is:  2 0 1 3
for 4 items, permute rank 13 is:  2 0 3 1
for 4 items, permute rank 14 is:  2 1 0 3
for 4 items, permute rank 15 is:  2 1 3 0
for 4 items, permute rank 16 is:  2 3 0 1
for 4 items, permute rank 17 is:  2 3 1 0
for 4 items, permute rank 18 is:  3 0 1 2
for 4 items, permute rank 19 is:  3 0 2 1
for 4 items, permute rank 20 is:  3 1 0 2
for 4 items, permute rank 21 is:  3 1 2 0
for 4 items, permute rank 22 is:  3 2 0 1
for 4 items, permute rank 23 is:  3 2 1 0

  the permutation rank of 17 is:  2 3 1 0

```



## Ring


```ring

# Project : Permutations/Rank of a permutation

list = [0, 1, 2, 3]
for perm = 0 to 23
    str = ""
    for i = 1 to len(list)
        str = str + list[i] + ", "
    next
    see nl
    str = left(str, len(str)-2)
    see "(" + str + ") -> " + perm
    nextPermutation(list)
next

func nextPermutation(a)
     elementcount = len(a)
     if elementcount < 1 then return ok
     pos = elementcount-1
     while a[pos] >= a[pos+1]
           pos -= 1
           if pos <= 0 permutationReverse(a, 1, elementcount)
              return
           ok
     end
     last = elementcount
     while a[last] <= a[pos]
           last -= 1
     end
     temp = a[pos]
     a[pos] = a[last]
     a[last] = temp
     permutationReverse(a, pos+1, elementcount)

 func permutationReverse(a, first, last)
      while first < last
            temp = a[first]
            a[first] = a[last]
            a[last] = temp
            first = first + 1
            last = last - 1
      end

```

Output:

```txt

(0, 1, 2, 3) -> 0
(0, 1, 3, 2) -> 1
(0, 2, 1, 3) -> 2
(0, 2, 3, 1) -> 3
(0, 3, 1, 2) -> 4
(0, 3, 2, 1) -> 5
(1, 0, 2, 3) -> 6
(1, 0, 3, 2) -> 7
(1, 2, 0, 3) -> 8
(1, 2, 3, 0) -> 9
(1, 3, 0, 2) -> 10
(1, 3, 2, 0) -> 11
(2, 0, 1, 3) -> 12
(2, 0, 3, 1) -> 13
(2, 1, 0, 3) -> 14
(2, 1, 3, 0) -> 15
(2, 3, 0, 1) -> 16
(2, 3, 1, 0) -> 17
(3, 0, 1, 2) -> 18
(3, 0, 2, 1) -> 19
(3, 1, 0, 2) -> 20
(3, 1, 2, 0) -> 21
(3, 2, 0, 1) -> 22
(3, 2, 1, 0) -> 23

```



## Ruby


```ruby
class Permutation
  include Enumerable
  attr_reader :num_elements, :size

  def initialize(num_elements)
    @num_elements = num_elements
    @size = fact(num_elements)
  end

  def each
    return self.to_enum unless block_given?
    (0...@size).each{|i| yield unrank(i)}
  end

  def unrank(r)  # nonrecursive version of Myrvold Ruskey unrank2 algorithm.
    pi = (0...num_elements).to_a
    (@num_elements-1).downto(1) do |n|
      s, r = r.divmod(fact(n))
      pi[n], pi[s] = pi[s], pi[n]
    end
    pi
  end

  def rank(pi)  # nonrecursive version of Myrvold Ruskey rank2 algorithm.
    pi = pi.dup
    pi1 = pi.zip(0...pi.size).sort.map(&:last)
    (pi.size-1).downto(0).inject(0) do |memo,i|
      pi[i], pi[pi1[i]] = pi[pi1[i]], (s = pi[i])
      pi1[s], pi1[i] = pi1[i], pi1[s]
      memo += s * fact(i)
    end
  end

  private
  def fact(n)
    n.zero? ? 1 : n.downto(1).inject(:*)
  end
end
```

Demo:

```ruby
puts "All permutations of 3 items from and back to rank."
perm = Permutation.new(3)
(0...perm.size).each{|num| puts "#{num} --> #{prm=perm.unrank(num)} --> #{perm.rank(prm)}"}

puts "\n4 random samples of 12 items from and back to rank."
perm = Permutation.new(12)
4.times{ puts "%9d --> %s --> %9d" % [r=rand(perm.size), prm=perm.unrank(r), perm.rank(prm)]}

puts "\n4 random uniq samples of 144 items:"
perm, rands = Permutation.new(144), {}
# generate 1_000_000 unique random numbers in the range (0...144!) (takes about 2.5 seconds)
rands[rand(perm.size)] = true until rands.size == 1_000_000

random_perms = rands.each_key.lazy{|k| perm.unrank(k)}
# random_perms is lazy. Generate permutations one by one:
4.times do
  p r = random_perms.next
  p prm = perm.unrank(r)
  p perm.rank(prm) == r
end
```

<pre style="overflow:auto">
All permutations of 3 items from and back to rank.
0 --> [1, 2, 0] --> 0
1 --> [2, 1, 0] --> 1
2 --> [2, 0, 1] --> 2
3 --> [0, 2, 1] --> 3
4 --> [1, 0, 2] --> 4
5 --> [0, 1, 2] --> 5

4 random samples of 12 items from and back to rank.
 99442243 --> [7, 6, 8, 3, 10, 1, 9, 11, 0, 4, 5, 2] -->  99442243
337326172 --> [7, 6, 10, 0, 2, 3, 11, 1, 5, 9, 4, 8] --> 337326172
  4778098 --> [9, 6, 7, 5, 2, 8, 11, 4, 10, 3, 1, 0] -->   4778098
468353447 --> [9, 4, 2, 3, 6, 10, 1, 7, 5, 0, 8, 11] --> 468353447

4 random uniq samples of 144 items:
2764575360456493218947191346199563786344679440083424317164174964729286291440941279012888793089017925404664271586714233946361449878956286173028220761623980334868793458099951877456608056440904453668056576598920423670911869425117449937938198123808117853
[98, 65, 31, 138, 68, 69, 10, 35, 143, 17, 49, 90, 100, 58, 79, 37, 89, 94, 11, 122, 5, 119, 47, 123, 128, 41, 67, 75, 33, 13, 8, 55, 80, 99, 85, 81, 45, 126, 115, 59, 133, 44, 140, 113, 54, 142, 125, 127, 50, 20, 70, 36, 60, 72, 52, 86, 134, 18, 96, 61, 114, 56, 1, 112, 109, 26, 2, 62, 97, 88, 57, 16, 83, 22, 76, 64, 129, 117, 121, 136, 73, 34, 27, 48, 3, 137, 104, 132, 101, 32, 7, 23, 25, 30, 4, 78, 6, 120, 28, 19, 42, 111, 124, 43, 135, 118, 131, 14, 40, 29, 51, 92, 93, 77, 95, 116, 106, 139, 91, 66, 102, 63, 84, 130, 21, 12, 38, 105, 141, 74, 46, 0, 39, 24, 108, 53, 15, 87, 107, 9, 82, 110, 103, 71]
true
581378746619134421610590787114075883604639790081640446988853702372476235480735354011614748882838959943069972039765230962978435387565720009660107060797009355093335736922722607963964082214822692777188121231234716922053393200924081454086807468556348101
[19, 48, 16, 89, 72, 109, 46, 34, 3, 52, 66, 122, 56, 119, 47, 84, 67, 88, 135, 113, 142, 4, 103, 124, 75, 31, 86, 81, 143, 2, 96, 117, 127, 39, 41, 33, 71, 106, 54, 17, 140, 83, 80, 68, 77, 125, 28, 38, 1, 139, 53, 35, 78, 65, 22, 10, 123, 20, 116, 23, 62, 32, 108, 29, 120, 128, 59, 111, 82, 64, 43, 42, 141, 70, 85, 57, 91, 44, 7, 61, 137, 45, 130, 132, 100, 112, 26, 58, 50, 55, 131, 27, 0, 74, 114, 115, 104, 95, 76, 73, 79, 5, 51, 93, 101, 24, 94, 8, 110, 138, 133, 92, 69, 134, 25, 87, 118, 121, 60, 126, 40, 36, 18, 98, 9, 99, 12, 129, 30, 107, 90, 63, 21, 97, 14, 49, 37, 13, 102, 105, 6, 136, 11, 15]
true
3862828872557168655028927772314213364838836589595169123363755483846504171095746597867391615687467779932466719611353020149271086793608853169788146783569713115890942229488492262312988677553070567823077055419333248270326769623765923370471995234511383513
[11, 29, 50, 68, 131, 139, 39, 84, 16, 143, 125, 62, 56, 120, 42, 80, 38, 83, 81, 49, 47, 116, 57, 34, 23, 124, 40, 137, 117, 135, 127, 43, 119, 18, 72, 33, 69, 141, 104, 102, 74, 133, 21, 0, 132, 78, 51, 98, 3, 7, 15, 26, 9, 122, 115, 103, 140, 85, 61, 130, 96, 87, 123, 5, 92, 22, 24, 93, 4, 88, 107, 121, 41, 136, 118, 65, 6, 77, 109, 60, 36, 105, 2, 30, 94, 10, 82, 110, 89, 106, 113, 32, 17, 111, 126, 71, 129, 46, 53, 20, 108, 55, 12, 28, 142, 37, 44, 45, 63, 73, 70, 134, 75, 99, 97, 14, 64, 90, 114, 8, 101, 66, 48, 27, 76, 13, 128, 112, 91, 95, 52, 25, 59, 35, 67, 86, 19, 58, 79, 138, 1, 54, 31, 100]
true
2918302898022044898081980708265879018867198180047026696353345520178885195108521785334274488901945168232326479535713828754624213355248635860483646511664861481065235873861496511507890774391370483865270125811094164531568140755985423978860285012352192610
[66, 140, 109, 101, 20, 62, 13, 60, 0, 116, 107, 1, 87, 99, 110, 38, 64, 25, 130, 2, 17, 84, 123, 36, 31, 94, 134, 51, 55, 103, 81, 30, 106, 15, 61, 21, 59, 142, 71, 92, 54, 79, 48, 132, 111, 7, 67, 65, 82, 49, 115, 10, 105, 19, 32, 22, 141, 9, 68, 53, 3, 63, 74, 56, 42, 23, 136, 129, 57, 126, 77, 39, 137, 50, 121, 117, 93, 5, 46, 45, 89, 96, 41, 122, 72, 47, 37, 11, 58, 91, 70, 128, 43, 127, 76, 95, 138, 100, 90, 8, 119, 120, 143, 14, 6, 33, 83, 28, 44, 27, 135, 88, 35, 78, 86, 18, 69, 73, 85, 112, 29, 80, 4, 104, 52, 108, 12, 114, 133, 118, 97, 26, 34, 24, 113, 40, 98, 124, 139, 125, 131, 16, 102, 75]
true

```

Really generating one million unique random permutations of 144 elements would take an estimated 11 hours on one core of my machine.


## Scala


```scala
import scala.math._

def factorial(n: Int): BigInt = {
  (1 to n).map(BigInt.apply).fold(BigInt(1))(_ * _)
}

def indexToPermutation(n: Int, x: BigInt): List[Int] = {
  indexToPermutation((0 until n).toList, x)
}

def indexToPermutation(ns: List[Int], x: BigInt): List[Int] = ns match {
  case Nil => Nil
  case _ => {
    val (iBig, xNew) = x /% factorial(ns.size - 1)
    val i = iBig.toInt
    ns(i) :: indexToPermutation(ns.take(i) ++ ns.drop(i + 1), xNew)
  }
}

def permutationToIndex[A](xs: List[A])(implicit ord: Ordering[A]): BigInt = xs match {
  case Nil => BigInt(0)
  case x :: rest => factorial(rest.size) * rest.count(ord.lt(_, x)) + permutationToIndex(rest)
}
```


```scala
def check(n: Int, x: BigInt): Boolean = {
  val perm = indexToPermutation(n, x)
  val xOut = permutationToIndex(perm)
  println(s"$x -> $perm -> $xOut")
  xOut == x
}

if ((0 to 5).map(BigInt.apply).forall(check(3, _))) {
  println("Success!")
} else {
  println("Failed")
}
```


```txt
0 -> List(0, 1, 2) -> 0
1 -> List(0, 2, 1) -> 1
2 -> List(1, 0, 2) -> 2
3 -> List(1, 2, 0) -> 3
4 -> List(2, 0, 1) -> 4
5 -> List(2, 1, 0) -> 5
Success!
```



## Tcl

```tcl
# A functional swap routine
proc swap {v idx1 idx2} {
    lset v $idx2 [lindex $v $idx1][lset v $idx1 [lindex $v $idx2];subst ""]
}

# Fill the integer array <vec> with the permutation at rank <rank>
proc computePermutation {vecName rank} {
    upvar 1 $vecName vec
    if {![info exist vec] || ![llength $vec]} return
    set N [llength $vec]
    for {set n 0} {$n < $N} {incr n} {lset vec $n $n}
    for {set n $N} {$n>=1} {incr n -1} {
	set r [expr {$rank % $n}]
	set rank [expr {$rank / $n}]
	set vec [swap $vec $r [expr {$n-1}]]
    }
}

# Return the rank of the current permutation.
proc computeRank {vec} {
    if {![llength $vec]} return
    set inv [lrepeat [llength $vec] 0]
    set i -1
    foreach v $vec {lset inv $v [incr i]}
    # First argument is lambda term
    set mrRank1 {{f n vec inv} {
	if {$n < 2} {return 0}
	set s [lindex $vec [set n1 [expr {$n - 1}]]]
	set vec [swap $vec $n1 [lindex $inv $n1]]
	set inv [swap $inv $s $n1]
	return [expr {$s + $n * [apply $f $f $n1 $vec $inv]}]
    }}
    return [apply $mrRank1 $mrRank1 [llength $vec] $vec $inv]
}
```

Demonstrating:

```tcl
proc factorial {n} {
    for {set result 1; set i 2} {$i <= $n} {incr i} {
	set result [expr {$result * $i}]
    }
    return $result
}

set items1 [lrepeat 4 ""]
set rMax1 [factorial [llength $items1]]
for {set rank 0} {$rank < $rMax1} {incr rank} {
    computePermutation items1 $rank
    puts [format "%3d: \[%s\] = %d" \
	    $rank [join $items1 ", "] [computeRank $items1]]
}
puts ""
set items2 [lrepeat 21 ""]
set rMax2 [factorial [llength $items2]]
foreach _ {1 2 3 4} {
    # Note that we're casting to (potential) bignum, so entier() not int()
    set rank [expr {entier(rand() * $rMax2)}]
    computePermutation items2 $rank
    puts [format "%20lld: \[%s\] = %lld" \
	    $rank [join $items2 ", "] [computeRank $items2]]
}
```

<pre style="overflow:auto">
  0: [1, 2, 3, 0] = 0
  1: [3, 2, 0, 1] = 1
  2: [1, 3, 0, 2] = 2
  3: [1, 2, 0, 3] = 3
  4: [2, 3, 1, 0] = 4
  5: [2, 0, 3, 1] = 5
  6: [3, 0, 1, 2] = 6
  7: [2, 0, 1, 3] = 7
  8: [1, 3, 2, 0] = 8
  9: [3, 0, 2, 1] = 9
 10: [1, 0, 3, 2] = 10
 11: [1, 0, 2, 3] = 11
 12: [2, 1, 3, 0] = 12
 13: [2, 3, 0, 1] = 13
 14: [3, 1, 0, 2] = 14
 15: [2, 1, 0, 3] = 15
 16: [3, 2, 1, 0] = 16
 17: [0, 2, 3, 1] = 17
 18: [0, 3, 1, 2] = 18
 19: [0, 2, 1, 3] = 19
 20: [3, 1, 2, 0] = 20
 21: [0, 3, 2, 1] = 21
 22: [0, 1, 3, 2] = 22
 23: [0, 1, 2, 3] = 23

22386545476695773184: [16, 8, 18, 19, 10, 6, 13, 9, 20, 3, 2, 7, 17, 11, 5, 14, 1, 15, 12, 4, 0] = 22386545476695773184
16971674357521238016: [2, 10, 8, 17, 4, 16, 7, 1, 14, 19, 15, 5, 13, 3, 6, 0, 9, 12, 11, 20, 18] = 16971674357521238016
 2200782205637410304: [12, 2, 19, 5, 6, 1, 8, 0, 16, 20, 18, 3, 9, 7, 11, 4, 13, 17, 15, 10, 14] = 2200782205637410304
49795340002029010944: [16, 20, 10, 3, 12, 6, 5, 1, 4, 8, 19, 0, 7, 14, 17, 18, 11, 13, 2, 15, 9] = 49795340002029010944

```

The above code is not a particularly good use of a random number generator; the Tcl PRNG does not generate enough bits of true randomness per call to <code>rand()</code> for it to be useful in such a simplistic approach. However, it will do for the purposes of demonstration. A more sophisticated RNG is this one:

```tcl
proc uniform {limit} {
    set bits [expr {ceil(log($limit)/log(2))+10}]
    for {set b [set r 0]} {$b < $bits} {incr b 16} {
	incr r [expr {2**$b * int(rand() * 2**16)}]
    }
    return [expr {$r % $limit}]
}
```


### Addressing the extra credit

The technique above can generate any random permutation of a list, but an equally viable approach (and one especially suitable to the SO question where the number of required permutations is small with respect to the number of possible permutations) would be to use a [[Knuth shuffle]] on a list and just use that with a quick check for repetitions, which could be implemented using a simple hashing check. (On the other hand, building a hash table of a million strings of 144 characters would not be too awkward on modern hardware.)


## zkl

```zkl
fcn computePermutation(items,rank){  // in place permutation of items
   foreach n in ([items.len()..1,-1]){
      r:=rank%n;
      rank/=n;
      items.swap(r,n-1);
   }
   items
}
```


```zkl
    /// Return the rank of a permutation.
fcn computeRank(perm){
   N,p2,inv := perm.len(), perm.copy(), List.createLong(N,0);
   foreach n in (N){ inv[perm[n]] = n; }
   fcn(n,p2,inv){
      if(n<2) return(0);
      i,s:= n-1, p2[i];
      p2.swap(i,inv[i]);
      inv.swap(s,i);
      s + n*self.fcn(i,p2,inv);
   }(N,p2,inv);
}
```


```zkl
    // do some random permutations of 4
fcn factorial(n){ (1).reduce(n,'*) }
items,max:=(4).toList(),factorial(items.len());  // (0,1,2,3), read only
foreach rank in ((5).pump(List,(0).random.fp(max)).sort()){
   p:=computePermutation(items.copy(),rank);
   println("%3d: %s = %d".fmt(rank, p, computeRank(p)));
}
println();

    // Permutations of 12 to compare to other solutions
items:=(12).toList();  // (0,1,2,3,..11)
foreach rank in (T(340494881,469128647,460982459,432900481)){
   p:=computePermutation(items.copy(),rank);
   println("%12d: %s = %d".fmt(rank,p,computeRank(p)));
}
```

```txt

  3: L(1,2,0,3) = 3
 15: L(2,1,0,3) = 15
 16: L(3,2,1,0) = 16
 20: L(3,1,2,0) = 20
 22: L(0,1,3,2) = 22

   340494881: L(4,2,3,9,0,8,10,11,1,6,7,5) = 340494881
   469128647: L(7,6,10,5,2,3,1,0,8,4,9,11) = 469128647
   460982459: L(4,9,5,7,0,8,6,10,2,1,3,11) = 460982459
   432900481: L(0,6,5,10,8,2,4,7,3,9,11,1) = 432900481

```


### Addressing the extra credit

computePermutation works fine with BigInts but there is no current way to generate a random number in range to 250!
If a 63 bit range was OK, then (but don't hold your breath, this takes a while):

```zkl
var [const] BN=Import("zklBigNum");  // libGMP
one44Bang:=BN(144).factorial();	     // 250 digits
// Needed: random number [0,one44Bang)
do(0d1_000_000){
   p:=computePermutation((144).toList().copy(),(0).random((0).MAX));
}
```

and the TCL idea of shuffles (List.shuffle()) makes more sense.
