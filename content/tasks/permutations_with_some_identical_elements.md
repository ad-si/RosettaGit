+++
title = "Permutations with some identical elements"
description = ""
date = 2019-09-11T10:25:35Z
aliases = []
[extra]
id = 22435
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "dart",
  "factor",
  "go",
  "julia",
  "minizinc",
  "perl",
  "perl_6",
  "phix",
  "related_tasks",
  "rexx",
  "sidef",
  "tailspin",
  "zkl",
]
+++

Sometimes you want to find all permutations of elements where some elements are repeated, e.g. you have 3 red balls, 2 blue balls and one black ball. 

If you just do all permutations of the 6 elements, each permutation will be duplicated 12 times where you can't tell that the identical elements have switched places.


Given an input of the form   <big>[a<sub>1</sub>, a<sub>2</sub>, ···, a<sub>k</sub>]</big>   where   <big>a<sub>k</sub></big>   denotes how many duplicates of element   <big>k</big>   you should have, 

each   <big>a<sub>k</sub> &gt; 0</big>   and the sum of all   <big>a<sub>k</sub></big>   is   <big>'''n'''</big>.

You should get   n! <big>/</big> (a<sub>1</sub>! &times; a<sub>2</sub>! ... &times; a<sub>k</sub>!)    permutations as a result. 

(You may, of course, denote the elements   <big>0..k-1</big>   if that works better.)


For example, the input   [2,1]   should give results   (1,1,2),   (1,2,1)   and  (2,1,1).

Alternatively, if zero-based:   (0,0,1),   (0,1,0)   and   (1,0,0).


## Task

List the permutations you get from the input   [2, 3, 1]. 

Optionally output the permutations as strings where the first element is represented by '''A''', the second by '''B''' and the third by '''C'''  

(the example result would then be '''AAB''', '''ABA''' and '''BAA''').


## Related tasks

*   [[Anagrams]]
*   [[Permutations]]
*   [[Permutations/Derangements]]





## Dart

```dart

import 'dart:io';

void main() {
  stdout.writeln(distinctPerms([2,3,1]).map((p) => alpha("ABC", p)).toList());
}

String alpha(String alphabet, List<int> perm) {
  return perm.map((i) => alphabet[i]).join("");
}

Iterable<List<int>> distinctPerms(List<int> reps) sync* {
  Iterable<List<int>> perms(List<List<int>> elements) sync* {
    if (elements.length == 1) {
      yield List.of(elements[0]);
    } else {
      for (int k = 0; k < elements.length; k++) {
        List<List<int>> tail = [];
        for (int i = 0; i < elements.length; i++) {
          if (i == k) {
            if (elements[i].length > 1) {
              tail.add(List.of(elements[i].skip(1)));
            }
          } else {
            tail.add(elements[i]);
          }
        }
        yield* perms(tail).map((t) {
          t.insert(0, elements[k][0]);
          return t;
        });
      }
    }
  }
  List<List<int>> elements = [];
  for (int i = 0; i < reps.length; i++) {
    elements.add(List.filled(reps[i], i));
  }
  yield* perms(elements);
}

```

```txt

[AABBBC, AABBCB, AABCBB, AACBBB, ABABBC, ABABCB, ABACBB, ABBABC, ABBACB, ABBBAC, ABBBCA, ABBCAB, ABBCBA, ABCABB, ABCBAB, ABCBBA, ACABBB, ACBABB, ACBBAB, ACBBBA, BAABBC, BAABCB, BAACBB, BABABC, BABACB, BABBAC, BABBCA, BABCAB, BABCBA, BACABB, BACBAB, BACBBA, BBAABC, BBAACB, BBABAC, BBABCA, BBACAB, BBACBA, BBBAAC, BBBACA, BBBCAA, BBCAAB, BBCABA, BBCBAA, BCAABB, BCABAB, BCABBA, BCBAAB, BCBABA, BCBBAA, CAABBB, CABABB, CABBAB, CABBBA, CBAABB, CBABAB, CBABBA, CBBAAB, CBBABA, CBBBAA]

```



## Factor

Removing duplicates from the list of all permutations:

```factor
USING: arrays grouping math math.combinatorics prettyprint
sequences sets ;

: distinct-permutations ( seq -- seq )
    [ CHAR: A + <array> ] map-index "" concat-as <permutations>
    members ;

{ 2 3 1 } distinct-permutations 10 group simple-table.
```

```txt

AABBBC AABBCB AABCBB AACBBB ABABBC ABABCB ABACBB ABBABC ABBACB ABBBAC
ABBBCA ABBCAB ABBCBA ABCABB ABCBAB ABCBBA ACABBB ACBABB ACBBAB ACBBBA
BAABBC BAABCB BAACBB BABABC BABACB BABBAC BABBCA BABCAB BABCBA BACABB
BACBAB BACBBA BBAABC BBAACB BBABAC BBABCA BBACAB BBACBA BBBAAC BBBACA
BBBCAA BBCAAB BBCABA BBCBAA BCAABB BCABAB BCABBA BCBAAB BCBABA BCBBAA
CAABBB CABABB CABBAB CABBBA CBAABB CBABAB CBABBA CBBAAB CBBABA CBBBAA

```

Generating distinct permutations directly (more efficient in time and space):
```factor
USING: arrays io kernel locals math math.ranges sequences ;
IN: rosetta-code.distinct-permutations

: should-swap? ( start curr seq -- ? )
    [ nipd nth ] [ <slice> member? not ] 3bi ;

:: .find-permutations ( seq index n -- )
    index n >= [ seq write bl ] [
        index n [a,b) [
            :> i
            index i seq should-swap? [
                index i seq exchange
                seq index 1 + n .find-permutations
                index i seq exchange
            ] when
        ] each
    ] if ;

: first-permutation ( nums charset -- seq )
    [ <array> ] 2map "" concat-as ;

: .distinct-permutations ( nums charset -- )
    first-permutation 0 over length .find-permutations nl ;

: main ( -- )
    { 2 1 } "12"
    { 2 3 1 } "123"
    { 2 3 1 } "ABC"
    [ .distinct-permutations ] 2tri@ ;

MAIN: main
```

```txt

112 121 211 
112223 112232 112322 113222 121223 121232 121322 122123 122132 122213 122231 122321 122312 123221 123212 123122 132221 132212 132122 131222 211223 211232 211322 212123 212132 212213 212231 212321 212312 213221 213212 213122 221123 221132 221213 221231 221321 221312 222113 222131 222311 223121 223112 223211 231221 231212 231122 232121 232112 232211 312221 312212 312122 311222 321221 321212 321122 322121 322112 322211 
AABBBC AABBCB AABCBB AACBBB ABABBC ABABCB ABACBB ABBABC ABBACB ABBBAC ABBBCA ABBCBA ABBCAB ABCBBA ABCBAB ABCABB ACBBBA ACBBAB ACBABB ACABBB BAABBC BAABCB BAACBB BABABC BABACB BABBAC BABBCA BABCBA BABCAB BACBBA BACBAB BACABB BBAABC BBAACB BBABAC BBABCA BBACBA BBACAB BBBAAC BBBACA BBBCAA BBCABA BBCAAB BBCBAA BCABBA BCABAB BCAABB BCBABA BCBAAB BCBBAA CABBBA CABBAB CABABB CAABBB CBABBA CBABAB CBAABB CBBABA CBBAAB CBBBAA 

```



## Go

This is based on the C++ code [https://www.geeksforgeeks.org/distinct-permutations-string-set-2/ here].

```go
package main

import "fmt"

func shouldSwap(s []byte, start, curr int) bool {
    for i := start; i < curr; i++ {
        if s[i] == s[curr] {
            return false
        }
    }
    return true
}

func findPerms(s []byte, index, n int, res *[]string) {
    if index >= n {
        *res = append(*res, string(s))
        return
    }
    for i := index; i < n; i++ {
        check := shouldSwap(s, index, i)
        if check {
            s[index], s[i] = s[i], s[index]
            findPerms(s, index+1, n, res)
            s[index], s[i] = s[i], s[index]
        }
    }
}

func createSlice(nums []int, charSet string) []byte {
    var chars []byte
    for i := 0; i < len(nums); i++ {
        for j := 0; j < nums[i]; j++ {
            chars = append(chars, charSet[i])
        }
    }
    return chars
}

func main() {
    var res, res2, res3 []string
    nums := []int{2, 1}
    s := createSlice(nums, "12")
    findPerms(s, 0, len(s), &res)
    fmt.Println(res)
    fmt.Println()

    nums = []int{2, 3, 1}
    s = createSlice(nums, "123")
    findPerms(s, 0, len(s), &res2)
    fmt.Println(res2)
    fmt.Println()

    s = createSlice(nums, "ABC")
    findPerms(s, 0, len(s), &res3)
    fmt.Println(res3)
}
```


```txt

[112 121 211]

[112223 112232 112322 113222 121223 121232 121322 122123 122132 122213 122231 122321 122312 123221 123212 123122 132221 132212 132122 131222 211223 211232 211322 212123 212132 212213 212231 212321 212312 213221 213212 213122 221123 221132 221213 221231 221321 221312 222113 222131 222311 223121 223112 223211 231221 231212 231122 232121 232112 232211 312221 312212 312122 311222 321221 321212 321122 322121 322112 322211]

[AABBBC AABBCB AABCBB AACBBB ABABBC ABABCB ABACBB ABBABC ABBACB ABBBAC ABBBCA ABBCBA ABBCAB ABCBBA ABCBAB ABCABB ACBBBA ACBBAB ACBABB ACABBB BAABBC BAABCB BAACBB BABABC BABACB BABBAC BABBCA BABCBA BABCAB BACBBA BACBAB BACABB BBAABC BBAACB BBABAC BBABCA BBACBA BBACAB BBBAAC BBBACA BBBCAA BBCABA BBCAAB BBCBAA BCABBA BCABAB BCAABB BCBABA BCBAAB BCBBAA CABBBA CABBAB CABABB CAABBB CBABBA CBABAB CBAABB CBBABA CBBAAB CBBBAA]

```



## Julia

====With the Combinatorics package, create all permutations and filter out the duplicates====

```julia
using Combinatorics

catlist(spec) = mapreduce(i -> repeat([i], spec[i]), vcat, 1:length(spec))

alphastringfromintvector(v) = String([Char(Int('A') + i - 1) for i in v])

function testpermwithident(spec)
    println("\nTesting $spec yielding:")
    for (i, p) in enumerate(unique(collect(permutations(catlist(spec)))))
        print(alphastringfromintvector(p), "  ", i % 10 == 0 ? "\n" : "")
    end
end

testpermwithident([2, 3, 1])

```
```txt

Testing [2, 3, 1] yielding:
AABBBC  AABBCB  AABCBB  AACBBB  ABABBC  ABABCB  ABACBB  ABBABC  ABBACB  ABBBAC
ABBBCA  ABBCAB  ABBCBA  ABCABB  ABCBAB  ABCBBA  ACABBB  ACBABB  ACBBAB  ACBBBA
BAABBC  BAABCB  BAACBB  BABABC  BABACB  BABBAC  BABBCA  BABCAB  BABCBA  BACABB
BACBAB  BACBBA  BBAABC  BBAACB  BBABAC  BBABCA  BBACAB  BBACBA  BBBAAC  BBBACA
BBBCAA  BBCAAB  BBCABA  BBCBAA  BCAABB  BCABAB  BCABBA  BCBAAB  BCBABA  BCBBAA
CAABBB  CABABB  CABBAB  CABBBA  CBAABB  CBABAB  CBABBA  CBBAAB  CBBABA  CBBBAA

```



### =Generate directly=

```julia

alpha(s, v) = map(i -> s[i], v)

function distinctPerms(spec)
    function perm(elements)
        if length(elements) == 1
            deepcopy(elements)
        else
          [pushfirst!(p, elements[k][1]) for k in 1:length(elements) for p in perm(filter(dups -> length(dups) != 0,
            [ if i == k dups[2:end] else dups end
              for (i, dups) in enumerate(elements)]))
          ]
        end
    end
    elements = [fill(x...) for x in enumerate(spec)]
    perm(elements)
end

println(map(p -> join(alpha("ABC", p), ""), distinctPerms([2, 3, 1])))

```

```txt

["AABBBC", "AABBCB", "AABCBB", "AACBBB", "ABABBC", "ABABCB", "ABACBB", "ABBABC", "ABBACB", "ABBBAC", "ABBBCA", "ABBCAB", "ABBCBA", "ABCABB", "ABCBAB", "ABCBBA", "ACABBB", "ACBABB", "ACBBAB", "ACBBBA", "BAABBC", "BAABCB", "BAACBB", "BABABC", "BABACB", "BABBAC", "BABBCA", "BABCAB", "BABCBA", "BACABB", "BACBAB", "BACBBA", "BBAABC", "BBAACB", "BBABAC", "BBABCA", "BBACAB", "BBACBA", "BBBAAC", "BBBACA", "BBBCAA", "BBCAAB", "BBCABA", "BBCBAA", "BCAABB", "BCABAB", "BCABBA", "BCBAAB", "BCBABA", "BCBBAA", "CAABBB", "CABABB", "CABBAB", "CABBBA", "CBAABB", "CBABAB", "CBABBA", "CBBAAB", "CBBABA", "CBBBAA"]

```



## MiniZinc


```MiniZinc

%Permutations with some identical elements. Nigel Galloway, September 9th., 2019
include "count.mzn";
enum                   N={A,B,C};
array [1..6] of var N: G; constraint count(G,A,2) /\ count(G,C,1);
output [show(G)]

```

minizinc --all-solutions produces:

```txt

[C, B, B, B, A, A]
----------
[B, C, B, B, A, A]
----------
[B, B, C, B, A, A]
----------
[B, B, B, C, A, A]
----------
[C, B, B, A, A, B]
----------
[B, C, B, A, A, B]
----------
[B, B, C, A, A, B]
----------
[B, B, B, A, A, C]
----------
[C, B, A, B, A, B]
----------
[B, C, A, B, A, B]
----------
[B, B, A, C, A, B]
----------
[B, B, A, B, A, C]
----------
[C, A, B, B, A, B]
----------
[B, A, C, B, A, B]
----------
[B, A, B, C, A, B]
----------
[B, A, B, B, A, C]
----------
[A, C, B, B, A, B]
----------
[A, B, C, B, A, B]
----------
[A, B, B, C, A, B]
----------
[A, B, B, B, A, C]
----------
[C, B, B, A, B, A]
----------
[B, C, B, A, B, A]
----------
[B, B, C, A, B, A]
----------
[B, B, B, A, C, A]
----------
[C, B, A, A, B, B]
----------
[B, C, A, A, B, B]
----------
[B, B, A, A, C, B]
----------
[B, B, A, A, B, C]
----------
[C, A, B, A, B, B]
----------
[B, A, C, A, B, B]
----------
[B, A, B, A, C, B]
----------
[B, A, B, A, B, C]
----------
[A, C, B, A, B, B]
----------
[A, B, C, A, B, B]
----------
[A, B, B, A, C, B]
----------
[A, B, B, A, B, C]
----------
[C, B, A, B, B, A]
----------
[B, C, A, B, B, A]
----------
[B, B, A, C, B, A]
----------
[B, B, A, B, C, A]
----------
[C, A, A, B, B, B]
----------
[B, A, A, C, B, B]
----------
[B, A, A, B, C, B]
----------
[B, A, A, B, B, C]
----------
[A, C, A, B, B, B]
----------
[A, B, A, C, B, B]
----------
[A, B, A, B, C, B]
----------
[A, B, A, B, B, C]
----------
[C, A, B, B, B, A]
----------
[B, A, C, B, B, A]
----------
[B, A, B, C, B, A]
----------
[B, A, B, B, C, A]
----------
[A, C, B, B, B, A]
----------
[A, B, C, B, B, A]
----------
[A, B, B, C, B, A]
----------
[A, B, B, B, C, A]
----------
[A, A, C, B, B, B]
----------
[A, A, B, C, B, B]
----------
[A, A, B, B, C, B]
----------
[A, A, B, B, B, C]
----------

### ====


```



## Perl

```perl>use ntheory qw<formultiperm
;

formultiperm { print join('',@_) . ' ' } [<1 1 2>];           print "\n\n";
formultiperm { print join('',@_) . ' ' } [<1 1 2 2 2 3>];     print "\n\n";
formultiperm { print join('',@_) . ' ' } [split //,'AABBBC']; print "\n";
```

```txt
112 121 211

112223 112232 112322 113222 121223 121232 121322 122123 122132 122213 122231 122312 122321 123122 123212 123221 131222 132122 132212 132221 211223 211232 211322 212123 212132 212213 212231 212312 212321 213122 213212 213221 221123 221132 221213 221231 221312 221321 222113 222131 222311 223112 223121 223211 231122 231212 231221 232112 232121 232211 311222 312122 312212 312221 321122 321212 321221 322112 322121 322211

AABBBC AABBCB AABCBB AACBBB ABABBC ABABCB ABACBB ABBABC ABBACB ABBBAC ABBBCA ABBCAB ABBCBA ABCABB ABCBAB ABCBBA ACABBB ACBABB ACBBAB ACBBBA BAABBC BAABCB BAACBB BABABC BABACB BABBAC BABBCA BABCAB BABCBA BACABB BACBAB BACBBA BBAABC BBAACB BBABAC BBABCA BBACAB BBACBA BBBAAC BBBACA BBBCAA BBCAAB BBCABA BBCBAA BCAABB BCABAB BCABBA BCBAAB BCBABA BCBBAA CAABBB CABABB CABBAB CABBBA CBAABB CBABAB CBABBA CBBAAB CBBABA CBBBAA
```



## Perl 6

```perl6
sub permutations-with-some-identical-elements ( @elements, @reps = () ) {
    with @elements { (@reps ?? flat $_ Zxx @reps !! flat .keys.map(*+1) Zxx .values).permutations».join.unique }
 }

for (<2 1>,), (<2 3 1>,), (<A B C>, <2 3 1>), (<🦋 ⚽ 🙄>, <2 2 1>) {
    put permutations-with-some-identical-elements |$_;
    say '';
}
```

```txt
112 121 211

112223 112232 112322 113222 121223 121232 121322 122123 122132 122213 122231 122312 122321 123122 123212 123221 131222 132122 132212 132221 211223 211232 211322 212123 212132 212213 212231 212312 212321 213122 213212 213221 221123 221132 221213 221231 221312 221321 222113 222131 222311 223112 223121 223211 231122 231212 231221 232112 232121 232211 311222 312122 312212 312221 321122 321212 321221 322112 322121 322211

AABBBC AABBCB AABCBB AACBBB ABABBC ABABCB ABACBB ABBABC ABBACB ABBBAC ABBBCA ABBCAB ABBCBA ABCABB ABCBAB ABCBBA ACABBB ACBABB ACBBAB ACBBBA BAABBC BAABCB BAACBB BABABC BABACB BABBAC BABBCA BABCAB BABCBA BACABB BACBAB BACBBA BBAABC BBAACB BBABAC BBABCA BBACAB BBACBA BBBAAC BBBACA BBBCAA BBCAAB BBCABA BBCBAA BCAABB BCABAB BCABBA BCBAAB BCBABA BCBBAA CAABBB CABABB CABBAB CABBBA CBAABB CBABAB CBABBA CBBAAB CBBABA CBBBAA

🦋🦋⚽⚽🙄 🦋🦋⚽🙄⚽ 🦋🦋🙄⚽⚽ 🦋⚽🦋⚽🙄 🦋⚽🦋🙄⚽ 🦋⚽⚽🦋🙄 🦋⚽⚽🙄🦋 🦋⚽🙄🦋⚽ 🦋⚽🙄⚽🦋 🦋🙄🦋⚽⚽ 🦋🙄⚽🦋⚽ 🦋🙄⚽⚽🦋 ⚽🦋🦋⚽🙄 ⚽🦋🦋🙄⚽ ⚽🦋⚽🦋🙄 ⚽🦋⚽🙄🦋 ⚽🦋🙄🦋⚽ ⚽🦋🙄⚽🦋 ⚽⚽🦋🦋🙄 ⚽⚽🦋🙄🦋 ⚽⚽🙄🦋🦋 ⚽🙄🦋🦋⚽ ⚽🙄🦋⚽🦋 ⚽🙄⚽🦋🦋 🙄🦋🦋⚽⚽ 🙄🦋⚽🦋⚽ 🙄🦋⚽⚽🦋 🙄⚽🦋🦋⚽ 🙄⚽🦋⚽🦋 🙄⚽⚽🦋🦋

```



## Phix

```Phix
function shouldSwap(string s, integer start, curr)
    for i=start to curr-1 do
        if s[i] == s[curr] then
            return false
        end if
    end for
    return true
end function
 
function findPerms(string s, integer i=1, sequence res={})
    if i>length(s) then
        res = append(res, s)
    else
        for j=i to length(s) do
            if shouldSwap(s, i, j) then
                {s[i], s[j]} = {s[j], s[i]}
                res = findPerms(s, i+1, res)
                {s[i], s[j]} = {s[j], s[i]}
            end if
        end for
    end if
    return res
end function
 
function createSlice(sequence nums, string charSet)
    string chars = ""
    for i=1 to length(nums) do
        chars &= repeat(charSet[i],nums[i])
    end for
    return chars
end function
 
pp(findPerms(createSlice({2,1}, "12")))     -- (=== findPerms("112"))
pp(findPerms(createSlice({2,3,1}, "123")))  -- (=== findPerms("112223"))
pp(findPerms(createSlice({2,3,1}, "ABC")))  -- (=== findPerms("AABBBC"))
```

```txt

{`112`, `121`, `211`}
{`112223`, `112232`, `112322`, `113222`, `121223`, `121232`, `121322`,
 `122123`, `122132`, `122213`, `122231`, `122321`, `122312`, `123221`,
 `123212`, `123122`, `132221`, `132212`, `132122`, `131222`, `211223`,
 `211232`, `211322`, `212123`, `212132`, `212213`, `212231`, `212321`,
 `212312`, `213221`, `213212`, `213122`, `221123`, `221132`, `221213`,
 `221231`, `221321`, `221312`, `222113`, `222131`, `222311`, `223121`,
 `223112`, `223211`, `231221`, `231212`, `231122`, `232121`, `232112`,
 `232211`, `312221`, `312212`, `312122`, `311222`, `321221`, `321212`,
 `321122`, `322121`, `322112`, `322211`}
{`AABBBC`, `AABBCB`, `AABCBB`, `AACBBB`, `ABABBC`, `ABABCB`, `ABACBB`,
 `ABBABC`, `ABBACB`, `ABBBAC`, `ABBBCA`, `ABBCBA`, `ABBCAB`, `ABCBBA`,
 `ABCBAB`, `ABCABB`, `ACBBBA`, `ACBBAB`, `ACBABB`, `ACABBB`, `BAABBC`,
 `BAABCB`, `BAACBB`, `BABABC`, `BABACB`, `BABBAC`, `BABBCA`, `BABCBA`,
 `BABCAB`, `BACBBA`, `BACBAB`, `BACABB`, `BBAABC`, `BBAACB`, `BBABAC`,
 `BBABCA`, `BBACBA`, `BBACAB`, `BBBAAC`, `BBBACA`, `BBBCAA`, `BBCABA`,
 `BBCAAB`, `BBCBAA`, `BCABBA`, `BCABAB`, `BCAABB`, `BCBABA`, `BCBAAB`,
 `BCBBAA`, `CABBBA`, `CABBAB`, `CABABB`, `CAABBB`, `CBABBA`, `CBABAB`,
 `CBAABB`, `CBBABA`, `CBBAAB`, `CBBBAA`}

```



## REXX


### shows permutation list


```rexx
/*REXX program  computes and displays  the  permutations  with some identical elements. */
parse arg g                                      /*obtain optional arguments from the CL*/
if g='' | g=","  then g= 2 3 1                   /*Not specified?  Then use the defaults*/
#= words(g)                                      /*obtain the number of source items.   */
@= left('ABCDEFGHIJKLMNOPQRSTUVWXYZ',  #)        /*@:   the (output) letters to be used.*/
LO=                                              /*LO:  the start of the sequence.      */
HI=                                              /*HI:   "   end   "  "      "          */
      do i=1  for #;      @.i= word(g, i)        /*get number of characters for an arg. */
      LO= LO || copies(i, @.i)                   /*build the  LO  number for the range. */
      HI=       copies(i, @.i) || HI             /*  "    "   HI     "    "   "    "    */
      end   /*i*/
$=                                               /*initialize the output string to null.*/
      do j=LO  to  HI                            /*generate the enumerated output string*/
      if verify(j, LO)\==0  then iterate         /*An invalid digital string?  Then skip*/
         do k=1  for #                           /*parse string for correct # of digits.*/
         if countstr(k, j)\==@.k  then iterate j /*Incorrect number of digits?  Skip.   */
         end   /*k*/
      $= $ j                                     /*append digital string to the list.   */
      end      /*j*/
                                                 /*stick a fork in it,  we're all done. */
say 'number of permutations: '    words($)
say
say strip(translate($, @, left(123456789, #) ) ) /*display the translated string to term*/
```

```txt

number of permutations:  3

AAB ABA BAA

```

```txt

number of permutations:  60

AABBBC AABBCB AABCBB AACBBB ABABBC ABABCB ABACBB ABBABC ABBACB ABBBAC ABBBCA ABBCAB ABBCBA ABCABB ABCBAB ABCBBA ACABBB ACBABB ACBBAB ACBBBA BAABBC BAABCB BAACBB BABABC BABACB BABBAC BABBCA BABCAB BABCBA BACABB BACBAB BACBBA BBAABC BBAACB BBABAC BBABCA BBACAB BBACBA BBBAAC BBBACA BBBCAA BBCAAB BBCABA BBCBAA BCAABB BCABAB BCABBA BCBAAB BCBABA BCBBAA CAABBB CABABB CABBAB CABBBA CBAABB CBABAB CBABBA CBBAAB CBBABA CBBBAA

```



### only shows permutation count

If any of the arguments is negative, the list of the permutations is suppressed, only the permutation count is shown. 

```rexx
/*REXX program  computes and displays  the  permutations  with some identical elements. */
parse arg g                                      /*obtain optional arguments from the CL*/
if g='' | g=","  then g= 2 3 1                   /*Not specified?  Then use the defaults*/
#= words(g)                                      /*obtain the number of source items.   */
@= left('ABCDEFGHIJKLMNOPQRSTUVWXYZ',  #)        /*@:   the (output) letters to be used.*/
show= 1                                          /*if = 1,  will show permutation list. */
sum= 0
LO=                                              /*LO:  the start of the sequence.      */
HI=                                              /*HI:   "   end   "  "      "          */
      do i=1  for #;        y= word(g, i)        /*get number of characters for an arg. */
      show= show & y>=0;    a= abs(y)            /*Is it negative?  Then don't show list*/
      sum= sum + a;       @.i= a                 /*use the absolute value of an argument*/
      LO= LO || copies(i, @.i)                   /*build the  LO  number for the range. */
      HI=       copies(i, @.i) || HI             /*  "    "   HI     "    "   "    "    */
      end   /*i*/
$=                                               /*initialize the output string to null.*/
numeric digits max(9, sum)                       /*ensure enough numeric decimal digits.*/

      do j=LO  to  HI                            /*generate the enumerated output string*/
      if verify(j, LO)\==0  then iterate         /*An invalid digital string?  Then skip*/
         do k=1  for #                           /*parse string for correct # of digits.*/
         if countstr(k, j)\==@.k  then iterate j /*Incorrect number of digits?  Skip.   */
         end   /*k*/
      $= $ j                                     /*append digital string to the list.   */
      end      /*j*/
                                                 /*stick a fork in it,  we're all done. */
say 'number of permutations: '    words($)       /*# perms with some identical elements.*/
say
if show  then say strip(translate($, @, left(123456789, #) ) ) /*display translated str.*/
```

```txt

number of permutations:  3360

```

```txt

number of permutations:  1260

```

```txt

number of permutations:  15120

```

```txt

number of permutations:  1512

```

```txt

number of permutations:  5040

```



## Sidef

Simple implementation, by filtering out the duplicated permutations.

```ruby
func permutations_with_some_identical_elements (reps) {
    reps.map_kv {|k,v| v.of(k+1)... }.permutations.uniq
}

say permutations_with_some_identical_elements([2,1]).map{.join}.join(' ')
say permutations_with_some_identical_elements([2,3,1]).map{.join}.join(' ')
```

```txt

112 121 211
112223 112232 112322 113222 121223 121232 121322 122123 122132 122213 122231 122312 122321 123122 123212 123221 131222 132122 132212 132221 211223 211232 211322 212123 212132 212213 212231 212312 212321 213122 213212 213221 221123 221132 221213 221231 221312 221321 222113 222131 222311 223112 223121 223211 231122 231212 231221 232112 232121 232211 311222 312122 312212 312221 321122 321212 321221 322112 322121 322211

```


More efficient approach, by generating the permutations without duplicates:

```ruby
func next_uniq_perm (array) {

    var k = array.end
    return ([], false) if (k < 0)
    var i = k-1

    while ((i >= 0) && (array[i] >= array[i+1])) {
        --i
    }

    return (array.flip, false) if (i == -1)

    if (array[i+1] > array[k]) {
        array = [array.slice(0, i)..., array.slice(i+1, k).flip...]
    }

    var j = i+1
    while (array[i] >= array[j]) {
        j++
    }

    array.clone!
    array.swap(i,j)

    return (array, true)
}

func unique_permutations(array, reps=[]) {
    var perm = (reps ? reps : array).map_kv {|k,v| v.of { reps ? array[k] : (k+1) }... }
    var perms = [perm]
    loop {
        (perm, var more) = next_uniq_perm(perm)
        break if !more
        perms << perm
    }
    return perms
}

for a,b in ([[[2,1]], [[2,3,1]], [%w(A B C), [2,3,1]]]) {
    say "\nPermutations with array = #{a}#{b ? \" and reps = #{b}\" : ''}:"
    say unique_permutations(a,b).map{.join}.join(' ')
}
```

```txt

Permutations with array = [2, 1]:
112 121 211

Permutations with array = [2, 3, 1]:
112223 112232 112322 113222 121223 121232 121322 122123 122132 122213 122231 122312 122321 123122 123212 123221 131222 132122 132212 132221 211223 211232 211322 212123 212132 212213 212231 212312 212321 213122 213212 213221 221123 221132 221213 221231 221312 221321 222113 222131 222311 223112 223121 223211 231122 231212 231221 232112 232121 232211 311222 312122 312212 312221 321122 321212 321221 322112 322121 322211

Permutations with array = ["A", "B", "C"] and reps = [2, 3, 1]:
AABBBC AABBCB AABCBB AACBBB ABABBC ABABCB ABACBB ABBABC ABBACB ABBBAC ABBBCA ABBCAB ABBCBA ABCABB ABCBAB ABCBBA ACABBB ACBABB ACBBAB ACBBBA BAABBC BAABCB BAACBB BABABC BABACB BABBAC BABBCA BABCAB BABCBA BACABB BACBAB BACBBA BBAABC BBAACB BBABAC BBABCA BBACAB BBACBA BBBAAC BBBACA BBBCAA BBCAAB BBCABA BBCBAA BCAABB BCABAB BCABBA BCBAAB BCBABA BCBBAA CAABBB CABABB CABBAB CABBBA CBAABB CBABAB CBABBA CBBAAB CBBABA CBBBAA

```



## Tailspin

Creates lots of new arrays, which might be wasteful.

```tailspin

templates distinctPerms
  templates perms
    <[](1)> $(1) !
    <>
      def elements: $;
      1..$::length -> (
        def k: $;
        def tail: $elements -> [i](
          <?($i <$k>)> $ -> (<[](2..)> $(2..-1)!) !
          <> $!
        );
        $tail -> perms -> [$elements($k;1), $...] !
      ) !
  end perms
  $ -> [i]([1..$ -> $i] !) -> perms !
end distinctPerms
 
def alpha: ['ABC'...];
[[2,3,1] -> distinctPerms -> '$alpha($)...;' ] -> !OUT::write

```

```txt

[AABBBC, AABBCB, AABCBB, AACBBB, ABABBC, ABABCB, ABACBB, ABBABC, ABBACB, ABBBAC, ABBBCA, ABBCAB, ABBCBA, ABCABB, ABCBAB, ABCBBA, ACABBB, ACBABB, ACBBAB, ACBBBA, BAABBC, BAABCB, BAACBB, BABABC, BABACB, BABBAC, BABBCA, BABCAB, BABCBA, BACABB, BACBAB, BACBBA, BBAABC, BBAACB, BBABAC, BBABCA, BBACAB, BBACBA, BBBAAC, BBBACA, BBBCAA, BBCAAB, BBCABA, BBCBAA, BCAABB, BCABAB, BCABBA, BCBAAB, BCBABA, BCBBAA, CAABBB, CABABB, CABBAB, CABBBA, CBAABB, CBABAB, CBABBA, CBBAAB, CBBABA, CBBBAA]

```

Work in place (slightly modified from the Go solution to preserve lexical order)
```tailspin

templates distinctPerms
  templates shouldSwap@{start:}
        <?($@distinctPerms($start..~$) <~[<$@distinctPerms($)>]>)> $!
  end shouldSwap
  templates findPerms
    <$@distinctPerms::length..> $@distinctPerms !
    <>
      def index: $;
      $index..$@distinctPerms::length -> shouldSwap@{start: $index}
      -> (
          @findPerms: $;
          @distinctPerms([$, $index]): $@distinctPerms([$index, $])...;
          $index + 1 -> findPerms !
      ) !
      @distinctPerms([$@findPerms, $index..~$@findPerms]): $@distinctPerms($index..$@findPerms)...;
  end findPerms
  @: $ -> [i](1..$ -> $i !);
  1 -> findPerms !
end distinctPerms
 
def alpha: ['ABC'...];
[[2,3,1] -> distinctPerms -> '$alpha($)...;' ] -> !OUT::write

```

```txt

[AABBBC, AABBCB, AABCBB, AACBBB, ABABBC, ABABCB, ABACBB, ABBABC, ABBACB, ABBBAC, ABBBCA, ABBCAB, ABBCBA, ABCABB, ABCBAB, ABCBBA, ACABBB, ACBABB, ACBBAB, ACBBBA, BAABBC, BAABCB, BAACBB, BABABC, BABACB, BABBAC, BABBCA, BABCAB, BABCBA, BACABB, BACBAB, BACBBA, BBAABC, BBAACB, BBABAC, BBABCA, BBACAB, BBACBA, BBBAAC, BBBACA, BBBCAA, BBCAAB, BBCABA, BBCBAA, BCAABB, BCABAB, BCABBA, BCBAAB, BCBABA, BCBBAA, CAABBB, CABABB, CABBAB, CABBBA, CBAABB, CBABAB, CBABBA, CBBAAB, CBBABA, CBBBAA]

```



## zkl


```zkl
  // eg ( (2,3,1), "ABC" ) == permute "A","A","B","B","B","C" and remove duplicates
  //  --> ( "AABBBC", "AABBCB" .. )
  // this gets ugly lots sooner than it should
fcn permutationsWithSomeIdenticalElements(ns,abcs){
   ns.zipWith(fcn(n,c){ List.createLong(n,c) },abcs).flatten() : # (3,"B")-->("B","B,"B")
   Utils.Helpers.permute(_) : Utils.Helpers.listUnique(_)
   .apply("concat")  // ("A","A","B","B","B","C")-->"AABBCB" 
}
```


```zkl
permutationsWithSomeIdenticalElements(T(2,1),"123").println();
permutationsWithSomeIdenticalElements(T(2,1),L("\u2192","\u2191")).concat("  ").println();

z:=permutationsWithSomeIdenticalElements(T(2,3,1),"ABC");
println(z.len());
z.pump(Void,T(Void.Read,9,False),  // print rows of ten items
	fcn{ vm.arglist.concat("  ").println() });
```

```txt

L("112","121","211")
→→↑  →↑→  ↑→→
60
AABBBC  AABBCB  AABCBB  AACBBB  ACABBB  CAABBB  CABABB  ACBABB  ABCABB  ABACBB
ABABCB  ABABBC  BAABBC  BAABCB  BAACBB  BACABB  BCAABB  CBAABB  ABBABC  ABBACB
ABBCAB  ABCBAB  ACBBAB  CABBAB  BABABC  BABACB  BABCAB  BACBAB  BCABAB  CBABAB
CBBAAB  BCBAAB  BBCAAB  BBACAB  BBAACB  BBAABC  CBABBA  BCABBA  BACBBA  BABCBA
BABBCA  BABBAC  CBBABA  BCBABA  BBCABA  BBACBA  BBABCA  BBABAC  ABBBAC  ABBBCA
ABBCBA  ABCBBA  ACBBBA  CABBBA  BBBAAC  BBBACA  BBBCAA  BBCBAA  BCBBAA  CBBBAA

```

