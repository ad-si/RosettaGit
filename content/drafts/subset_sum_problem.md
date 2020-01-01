+++
title = "Subset sum problem"
description = ""
date = 2019-05-28T20:32:30Z
aliases = []
[extra]
id = 11129
[taxonomies]
categories = []
tags = []
+++

{{draft task|Discrete math}}

;Task:
Implement a function/procedure/method/subroutine that takes a set/array/list/stream/table/collection of words with integer weights, and identifies a non-empty subset of them whose weights sum to zero (cf. the Dropbox [http://www.dropbox.com/jobs/challenges Diet] candidate screening exercise and the [[wp:Subset sum problem|Subset sum problem]] Wikipedia article).


;For example:
This set of weighted words, one solution would be the set of words:
:::*   {elysee,   efferent,   deploy,   departure,   centipede,   bonnet,   balm,   archbishop}
because their respective weights of:
:::*   -326,   54,   44,   952,   -658,   452,   397,   and   -915
sum to zero.

:::::: {| style="text-align: left; width: 40%;" border="5" cellpadding="2" cellspacing="2"
|+              Table of weighted words
|- style="background-color: rgb(255, 204, 255);"
! word !! weight
|-
| alliance || -624
|-
| archbishop || -915
|-
| balm || 397
|-
| bonnet || 452
|-
| brute || 870
|-
| centipede || -658
|-
| cobol || 362
|-
| covariate || 590
|-
| departure || 952
|-
| deploy || 44
|-
| diophantine || 645
|-
| efferent || 54
|-
| elysee || -326
|-
| eradicate || 376
|-
| escritoire || 856
|-
| exorcism || -983
|-
| fiat || 170
|-
| filmy || -874
|-
| flatworm || 503
|-
| gestapo || 915
|-
| infra || -847
|-
| isis || -982
|-
| lindholm || 999
|-
| markham || 475
|-
| mincemeat || -880
|-
| moresby || 756
|-
| mycenae || 183
|-
| plugging || -266
|-
| smokescreen || 423
|-
| speakeasy || -745
|-
| vein || 813
|}


Another solution would be the set of words {flatworm, gestapo, infra, isis, lindholm, plugging, smokescreen, speakeasy}, because their respective weights of 503, 915, -847, -982, 999, -266, 423, and -745 also sum to zero.

You may assume the weights range from -1000 to 1000.

If there are multiple solutions, only one needs to be found.

Use any algorithm you want and demonstrate it on a set of at least 30 weighted words with the results shown in a human readable form.

Note that an implementation that depends on enumerating all possible subsets is likely to be infeasible.





## Ada


```Ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
procedure SubsetSum is
   function "+"(S:String) return Unbounded_String renames To_Unbounded_String;
   type Point is record
      str : Unbounded_String;
      num : Integer;
   end record;
   type Points is array (Natural range <>) of Point;
   type Indices is array (Natural range <>) of Natural;

   procedure Print (data : Points; list : Indices; len : Positive) is begin
      Put (len'Img & ":");
      for i in 0..len-1 loop
         Put (" "& To_String(data(list(i)).str));
      end loop; New_Line;
   end Print;

   function Check (data : Points; list : Indices; len : Positive) return Boolean is
      sum : Integer := 0;
   begin
      for i in 0..len-1 loop sum := sum + data(list(i)).num; end loop;
      return sum = 0;
   end Check;

   procedure Next (list : in out Indices; n, r : Positive ) is begin
      for i in reverse 0..r-1 loop
         if list(i)/=i+n-r then list(i):=list(i)+1;
            for j in i+1..r-1 loop list(j):=list(j-1)+1; end loop; exit;
         end if;
      end loop;
   end Next;

   data : constant Points := ((+"alliance", -624), (+"archbishop", -915),
      (+"balm", 397), (+"bonnet", 452), (+"brute", 870),
      (+"centipede", -658), (+"cobol", 362), (+"covariate", 590),
      (+"departure", 952), (+"deploy", 44), (+"diophantine", 645),
      (+"efferent", 54), (+"elysee", -326), (+"eradicate", 376),
      (+"escritoire", 856), (+"exorcism", -983), (+"fiat", 170),
      (+"filmy", -874), (+"flatworm", 503), (+"gestapo", 915),
      (+"infra", -847), (+"isis", -982), (+"lindholm", 999),
      (+"markham", 475), (+"mincemeat", -880), (+"moresby", 756),
      (+"mycenae", 183), (+"plugging", -266), (+"smokescreen", 423),
      (+"speakeasy", -745), (+"vein", 813));
   list, last : Indices (data'Range);
begin
   for len in 2..data'Length loop
      for i in 0..len-1 loop list(i):=i; end loop;
      loop
         if Check(data, list, len) then Print(data, list, len); exit; end if;
         last := list;
         Next(list, data'Length, len);
         exit when last=list;
      end loop;
   end loop;
end SubsetSum;
```

{{out}}

```txt
2: archbishop gestapo
3: centipede markham mycenae
4: alliance balm deploy mycenae
5: alliance brute covariate deploy mincemeat
6: alliance archbishop balm deploy gestapo mycenae
7: alliance archbishop bonnet cobol departure exorcism moresby
8: alliance archbishop balm bonnet fiat flatworm isis lindholm
9: alliance archbishop balm bonnet brute covariate eradicate mincemeat plugging
10: alliance archbishop balm bonnet brute centipede cobol departure deploy mincemeat
11: alliance archbishop balm bonnet brute centipede cobol departure infra moresby speakeasy
12: alliance archbishop balm bonnet brute centipede cobol covariate diophantine efferent elysee infra
13: alliance archbishop balm bonnet brute centipede cobol covariate departure efferent eradicate filmy isis
14: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy elysee filmy markham speakeasy
15: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy elysee exorcism flatworm infra mycenae
16: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine elysee exorcism filmy gestapo infra
17: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine exorcism isis mincemeat mycenae plugging vein
18: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee exorcism filmy isis mycenae vein
19: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate exorcism fiat infra isis smokescreen
20: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate exorcism gestapo infra isis smokescreen speakeasy
21: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate exorcism flatworm infra lindholm mincemeat plugging speakeasy
22: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate escritoire exorcism fiat filmy flatworm mincemeat plugging speakeasy
23: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate escritoire exorcism infra isis mincemeat moresby mycenae smokescreen speakeasy
24: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee exorcism filmy gestapo infra markham mincemeat moresby mycenae plugging smokescreen speakeasy
25: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine eradicate exorcism fiat filmy flatworm infra isis lindholm markham mincemeat moresby mycenae plugging speakeasy
26: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine elysee eradicate escritoire exorcism fiat filmy gestapo infra isis markham mincemeat mycenae plugging speakeasy vein
27: alliance archbishop balm bonnet brute centipede covariate departure deploy efferent elysee eradicate escritoire exorcism fiat filmy flatworm infra isis lindholm markham mincemeat moresby mycenae plugging smokescreen speakeasy
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    char *word;
    int weight;
} item_t;

item_t items[] = {
    {"alliance",     -624},
    {"archbishop",   -915},
    {"balm",          397},
    {"bonnet",        452},
    {"brute",         870},
    {"centipede",    -658},
    {"cobol",         362},
    {"covariate",     590},
    {"departure",     952},
    {"deploy",         44},
    {"diophantine",   645},
    {"efferent",       54},
    {"elysee",       -326},
    {"eradicate",     376},
    {"escritoire",    856},
    {"exorcism",     -983},
    {"fiat",          170},
    {"filmy",        -874},
    {"flatworm",      503},
    {"gestapo",       915},
    {"infra",        -847},
    {"isis",         -982},
    {"lindholm",      999},
    {"markham",       475},
    {"mincemeat",    -880},
    {"moresby",       756},
    {"mycenae",       183},
    {"plugging",     -266},
    {"smokescreen",   423},
    {"speakeasy",    -745},
    {"vein",          813},
};

int n = sizeof (items) / sizeof (item_t);
int *set;

void subsum (int i, int weight) {
    int j;
    if (i && !weight) {
        for (j = 0; j < i; j++) {
            item_t item = items[set[j]];
            printf("%s%s", j ? " " : "", items[set[j]].word);
        }
        printf("\n");
    }
    for (j = i ? set[i - 1] + 1: 0; j < n; j++) {
        set[i] = j;
        subsum(i + 1, weight + items[j].weight);
    }
}

int main () {
    set = malloc(n * sizeof (int));
    subsum(0, 0);
    return 0;
}
```


{{output}}
```txt
alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate escritoire exorcism fiat filmy flatworm mincemeat plugging speakeasy
...
```



## C++

{{trans|C#}}

```cpp
#include <iostream>
#include <vector>

std::ostream& operator<<(std::ostream& out, const std::string& str) {
    return out << str.c_str();
}

std::vector<std::pair<std::string, int>> items{
    {"alliance",     -624},
    {"archbishop",   -915},
    {"balm",          397},
    {"bonnet",        452},
    {"brute",         870},
    {"centipede",    -658},
    {"cobol",         362},
    {"covariate",     590},
    {"departure",     952},
    {"deploy",         44},
    {"diophantine",   645},
    {"efferent",       54},
    {"elysee",       -326},
    {"eradicate",     376},
    {"escritoire",    856},
    {"exorcism",     -983},
    {"fiat",          170},
    {"filmy",        -874},
    {"flatworm",      503},
    {"gestapo",       915},
    {"infra",        -847},
    {"isis",         -982},
    {"lindholm",      999},
    {"markham",       475},
    {"mincemeat",    -880},
    {"moresby",       756},
    {"mycenae",       183},
    {"plugging",     -266},
    {"smokescreen",   423},
    {"speakeasy",    -745},
    {"vein",          813},
};

std::vector<int> indices;
int count = 0;
const int LIMIT = 5;

void subsum(int i, int weight) {
    if (i != 0 && weight == 0) {
        for (int j = 0; j < i; ++j) {
            auto item = items[indices[j]];
            std::cout << (j ? " " : "") << item.first;
        }
        std::cout << '\n';
        if (count < LIMIT) count++;
        else return;
    }
    int k = (i != 0) ? indices[i - 1] + 1 : 0;
    for (int j = k; j < items.size(); ++j) {
        indices[i] = j;
        subsum(i + 1, weight + items[j].second);
        if (count == LIMIT) return;
    }
}

int main() {
    indices.resize(items.size());
    subsum(0, 0);
    return 0;
}
```

{{out}}

```txt
alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate escritoire exorcism fiat filmy flatworm mincemeat plugging speakeasy
alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate escritoire exorcism infra isis mincemeat moresby mycenae smokescreen speakeasy
alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate escritoire fiat infra isis markham mincemeat plugging speakeasy
alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate exorcism fiat infra isis mincemeat moresby plugging vein
alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate exorcism fiat infra isis smokescreen
```


## C#
{{trans|Java}}

```c#
using System;
using System.Collections.Generic;

namespace SubsetSum {
    class Item {
        public Item(string word, int weight) {
            Word = word;
            Weight = weight;
        }

        public string Word { get; set; }
        public int Weight { get; set; }

        public override string ToString() {
            return string.Format("({0}, {1})", Word, Weight);
        }
    }

    class Program {
        private static readonly List<Item> items = new List<Item>() {
            new Item("alliance", -624),
            new Item("archbishop", -915),
            new Item("balm", 397),
            new Item("bonnet", 452),
            new Item("brute", 870),
            new Item("centipede", -658),
            new Item("cobol", 362),
            new Item("covariate", 590),
            new Item("departure", 952),
            new Item("deploy", 44),
            new Item("diophantine", 645),
            new Item("efferent", 54),
            new Item("elysee", -326),
            new Item("eradicate", 376),
            new Item("escritoire", 856),
            new Item("exorcism", -983),
            new Item("fiat", 170),
            new Item("filmy", -874),
            new Item("flatworm", 503),
            new Item("gestapo", 915),
            new Item("infra", -847),
            new Item("isis", -982),
            new Item("lindholm", 999),
            new Item("markham", 475),
            new Item("mincemeat", -880),
            new Item("moresby", 756),
            new Item("mycenae", 183),
            new Item("plugging", -266),
            new Item("smokescreen", 423),
            new Item("speakeasy", -745),
            new Item("vein", 813),
        };

        private static readonly int n = items.Count;
        private static readonly int LIMIT = 5;

        private static int[] indices = new int[n];
        private static int count = 0;

        private static void ZeroSum(int i, int w) {
            if (i != 0 && w == 0) {
                for (int j = 0; j < i; j++) {
                    Console.Write("{0} ", items[indices[j]]);
                }
                Console.WriteLine("\n");
                if (count < LIMIT) count++;
                else return;
            }
            int k = (i != 0) ? indices[i - 1] + 1 : 0;
            for (int j = k; j < n; j++) {
                indices[i] = j;
                ZeroSum(i + 1, w + items[j].Weight);
                if (count == LIMIT) return;
            }
        }

        static void Main(string[] args) {
            Console.WriteLine("The weights of the following {0} subsets add up to zero:\n", LIMIT);
            ZeroSum(0, 0);
        }
    }
}
```

{{out}}

```txt
The weights of the following 5 subsets add up to zero:

(alliance, -624) (archbishop, -915) (balm, 397) (bonnet, 452) (brute, 870) (centipede, -658) (cobol, 362) (covariate, 590) (departure, 952) (deploy, 44) (diophantine, 645) (efferent, 54) (elysee, -326) (eradicate, 376) (escritoire, 856) (exorcism, -983) (fiat, 170) (filmy, -874) (flatworm, 503) (mincemeat, -880) (plugging, -266) (speakeasy, -745)

(alliance, -624) (archbishop, -915) (balm, 397) (bonnet, 452) (brute, 870) (centipede, -658) (cobol, 362) (covariate, 590) (departure, 952) (deploy, 44) (diophantine, 645) (efferent, 54) (elysee, -326) (eradicate, 376) (escritoire, 856) (exorcism, -983) (infra, -847) (isis, -982) (mincemeat, -880) (moresby, 756) (mycenae, 183) (smokescreen, 423) (speakeasy, -745)

(alliance, -624) (archbishop, -915) (balm, 397) (bonnet, 452) (brute, 870) (centipede, -658) (cobol, 362) (covariate, 590) (departure, 952) (deploy, 44) (diophantine, 645) (efferent, 54) (elysee, -326) (eradicate, 376) (escritoire, 856) (fiat, 170) (infra, -847) (isis, -982) (markham, 475) (mincemeat, -880) (plugging, -266) (speakeasy, -745)

(alliance, -624) (archbishop, -915) (balm, 397) (bonnet, 452) (brute, 870) (centipede, -658) (cobol, 362) (covariate, 590) (departure, 952) (deploy, 44) (diophantine, 645) (efferent, 54) (elysee, -326) (eradicate, 376) (exorcism, -983) (fiat, 170) (infra, -847) (isis, -982) (mincemeat, -880) (moresby, 756) (plugging, -266) (vein, 813)

(alliance, -624) (archbishop, -915) (balm, 397) (bonnet, 452) (brute, 870) (centipede, -658) (cobol, 362) (covariate, 590) (departure, 952) (deploy, 44) (diophantine, 645) (efferent, 54) (elysee, -326) (eradicate, 376) (exorcism, -983) (fiat, 170) (infra, -847) (isis, -982) (smokescreen, 423)
```



## D

A simple brute-force solution. This used the module of the third D solution of the Combinations Task.
{{trans|Ruby}}

```d
void main() {
    import std.stdio, std.algorithm, std.typecons, combinations3;

    alias P = tuple;
    immutable items = [
P("alliance",  -624),  P("archbishop",  -915),  P("balm",        397),
P("bonnet",     452),  P("brute",        870),  P("centipede",  -658),
P("cobol",      362),  P("covariate",    590),  P("departure",   952),
P("deploy",      44),  P("diophantine",  645),  P("efferent",     54),
P("elysee",    -326),  P("eradicate",    376),  P("escritoire",  856),
P("exorcism",  -983),  P("fiat",         170),  P("filmy",      -874),
P("flatworm",   503),  P("gestapo",      915),  P("infra",      -847),
P("isis",      -982),  P("lindholm",     999),  P("markham",     475),
P("mincemeat", -880),  P("moresby",      756),  P("mycenae",     183),
P("plugging",  -266),  P("smokescreen",  423),  P("speakeasy",  -745),
P("vein",       813)];

    foreach (immutable n; 1 .. items.length)
        foreach (const comb; items.combinations(n))
            if (comb.map!q{ a[1] }.sum == 0)
                return writefln("A subset of length %d: %-(%s, %)", n,
                                comb.map!q{ a[0] });
    "No solution found.".writeln;
}
```

{{out}}

```txt
A subset of length 2: archbishop, gestapo
```



### Alternative Version

This version prints all the 349_167 solutions in about 1.8 seconds and counts them in about 0.05 seconds.
{{trans|C}}

```d
import std.stdio, std.algorithm;

enum showAllSolutions = true;

struct Item { string data; int weight; }
struct Sum { int sum; uint mask; }

immutable Item[] em = [
    {"alliance",  -624},  {"archbishop",  -915},  {"balm",        397},
    {"bonnet",     452},  {"brute",        870},  {"centipede",  -658},
    {"cobol",      362},  {"covariate",    590},  {"departure",   952},
    {"deploy",      44},  {"diophantine",  645},  {"efferent",     54},
    {"elysee",    -326},  {"eradicate",    376},  {"escritoire",  856},
    {"exorcism",  -983},  {"fiat",         170},  {"filmy",      -874},
    {"flatworm",   503},  {"gestapo",      915},  {"infra",      -847},
    {"isis",      -982},  {"lindholm",     999},  {"markham",     475},
    {"mincemeat", -880},  {"moresby",      756},  {"mycenae",     183},
    {"plugging",  -266},  {"smokescreen",  423},  {"speakeasy",  -745},
    {"vein",       813}];

Sum[] mkSums(in Item[] p, in size_t n, in size_t shift) {
    auto r = new Sum[1 << n];
    foreach (immutable i; 0 .. n)
        r[1 << i].sum = p[i].weight;

    foreach (immutable i, ref ri; r) {
        immutable size_t b = i & -int(i);
        ri = Sum(r[i & ~b].sum + r[b].sum, i << shift);
    }

    return r.sort!q{ a.sum < b.sum }.release;
}

void showMask(in uint mask) nothrow {
    for (size_t m = 0; (1U << m) <= mask; m++)
        if (mask & (1U << m))
            // Much faster than writeln.
            // The names are all zero-terminated.
            printf("%s ", em[m].data.ptr);
    if (mask)
        putchar('\n');
}

int printList(in int i, in int j, in int i1, in int j1,
              in Sum[] l, in Sum[] r) nothrow {
    int s = (i1 - i) * (j - j1);
    if (!l[i].sum)
        s--;

    static if (showAllSolutions)
        foreach (immutable x; i .. i1)
            foreach_reverse (immutable size_t y; j1 + 1 .. j + 1)
                showMask(l[x].mask | r[y].mask);
    return s;
}

void main() {
    immutable N = em.length;
    assert(N <= em[0].sizeof * 8, "Not enough bits in the mask");
    immutable size_t n1 = N / 2;
    immutable size_t n2 = N - n1;
    immutable size_t n1p = 1 << n1;
    immutable size_t n2p = 1 << n2;

    auto l = mkSums(em[], n1, 0);
    auto r = mkSums(em[n1 .. $], n2, n1);

    size_t sols = 0;
    int i = 0;
    int j = n2p - 1;
    while (true) {
        while (l[i].sum + r[j].sum) {
            while (i < n1p && l[i].sum + r[j].sum < 0)
                i++;
            while (j >= 0 && l[i].sum + r[j].sum > 0)
                j--;
            if (i >= n1p || j < 0)
                break;
        }
        if (i >= n1p || j < 0)
            break;

        int i1 = i + 1;
        while (i1 < n1p && l[i1].sum == l[i].sum)
            i1++;

        int j1 = j - 1;
        while (j1 >= 0 && r[j1].sum == r[j].sum)
            j1--;

        sols += printList(i, j, i1, j1, l, r);
        i = i1;
        j = j1;
    }

    writeln("Zero sums: ", sols);
}
```

{{out}}

```txt
Zero sums: 349167
```



## EchoLisp


### Dynamic programming

We use the Pseudo-polynomial time dynamic programming solution, found in the [[wp:Subset sum problem|Subset sum problem]] Wikipedia article. If A and B are the min and max possible sums, the time and memory needed are '''O((B-A)*N)'''. '''Q''' is an array such as Q(i,s) = true if there is a nonempty subset of x0, ..., xi which sums to s.


```scheme

;; 0 <= i < N , A <= s <  B , -A = abs(A)
;; mapping two dims Q(i,s) to one-dim Q(qidx(i,s)) :

(define-syntax-rule (qidx i s) (+ i (* (+ s -A) N)))

;; filling the Q array with true/false values
;; Q(i, s) := Q(i − 1, s) or (xi == s) or Q(i − 1, s − xi),  for A ≤ s < B.

(define (fillQ  xs (ds))
    (define N (length xs))
    (define A (apply + (filter negative? xs)))
    (define B (1+ (apply + (filter positive? xs))))
    (define -A (abs A))
    (define Q (make-vector (* N (- B A))))
    (set! xs (list->vector xs))

    (printf "Q[%d] allocated." (vector-length Q))
    (for ((s (in-range A B)))
            (vector-set! Q (qidx 0 s ) (= [xs 0] s)))

    (for*   ([i (in-range 1 N)]
             [s (in-range A B)])

        (set! ds (- s [xs i]))
        (vector-set! Q (qidx i s)
            (or
                [Q (qidx (1- i) s)]
                (= [xs i] s)
                (and (>= ds A) (< ds B) [Q (qidx (1- i) ds )])))

        ;; stop on first zero-sum found
         #:break (and (zero? s) [Q (qidx i s)]) => (solQ Q xs i s -A N)
        ))

;; backtracking to get the list of i's such as sum([xs i]) = 0
;; start from q[i,0] === true

(define  (solQ Q xs i s -A N  (sol null))
    (cond
        (( = s [xs i]) (cons  i  sol))
        ([Q (qidx (1- i ) s)] (solQ Q xs (1- i) s -A N sol))
        (else  (solQ Q xs (1- i) (- s [xs i]) -A N (cons i sol)))))

(define (task input)
    (map  (lambda(i)  (first (list-ref input i))) (fillQ (map rest input))))


```

{{out}}

```txt

(define input
    '({"alliance" . -624}
    {"archbishop" . -915}
    {"balm" . 397}
    {"bonnet" . 452}
    {"brute" . 870}
    {"centipede" . -658}
    {"cobol" . 362}
    {"covariate" . 590}
    {"departure" . 952}
    {"deploy" . 44}
    {"diophantine" . 645}
    {"efferent" . 54}
    {"elysee" . -326}
    {"eradicate" . 376}
    {"escritoire" . 856}
    {"exorcism" . -983}
    {"fiat" . 170}
    {"filmy" . -874}
    {"flatworm" . 503}
    {"gestapo" . 915}
    {"infra" . -847}
    {"isis" . -982}
    {"lindholm" . 999}
    {"markham" . 475}
    {"mincemeat" . -880}
    {"moresby" . 756}
    {"mycenae" . 183}
    {"plugging" . -266}
    {"smokescreen" . 423}
    {"speakeasy" . -745}
    {"vein" . 813}))

(task input)
Q[587016] allocated.
    → ("archbishop" "balm" "bonnet" "centipede" "cobol" "covariate"
"deploy" "efferent" "elysee")

;; using Haskell test data
(define items
    '[-61 1 32 373 311 249 311 32 -92 -185 -433
    -402 -247 156 125 249 32 -464 -278 218 32 -123
    -216 373 -185 -402 156 -402 -61 -31 902 ])

(map (lambda(i) (list-ref items i)) (fillQ items))

Q[221185] allocated.
   → (-61 32 373 311 249 311 32 -92 -185 -433 -402 -247 156 125 249 32 -
    464 -278 218 32 -123 -216 373 -185 -402 156 -402 -61 902)

```


### Brute force

We use the '''powerset''' procrastinator which gives in sequence all subsets of the input list.

```scheme

(lib 'sequences) ;; for powerset

(define (sum0? xs)
	(zero? (apply + (map rest xs))))

;; filter the powerset and
;; take first 5 solutions
(for-each writeln (take (filter sum0? (powerset input)) 5))

()  ;; empty

(("archbishop" . -915) ("balm" . 397) ("bonnet" . 452)
("centipede" . -658) ("cobol" . 362) ("covariate" . 590)
("deploy" . 44) ("efferent" . 54) ("elysee" . -326))

(("archbishop" . -915) ("balm" . 397) ("bonnet" . 452)
("centipede" . -658) ("departure" . 952) ("deploy" . 44)
("efferent" . 54) ("elysee" . -326))

(("alliance" . -624) ("brute" . 870) ("centipede" . -658)
("cobol" . 362) ("elysee" . -326) ("eradicate" . 376))

(("alliance" . -624) ("archbishop" . -915) ("bonnet" . 452)
("centipede" . -658) ("cobol" . 362) ("covariate" . 590)
("deploy" . 44) ("diophantine" . 645) ("efferent" . 54)
("elysee" . -326) ("eradicate" . 376))

```



## FunL


```funl
def subsetSum( s, w, v ) =
  def sumset( a ) = foldl1( (+), map(w, a) )

  for i <- s.subsets() if i != {}
    if sumset( i ) == v
      return Some( i )

  None

s = {
  ('alliance', -624),
  ('archbishop', -915),
  ('balm', 397),
  ('bonnet', 452),
  ('brute', 870),
  ('centipede', -658),
  ('cobol', 362),
  ('covariate', 590),
  ('departure', 952),
  ('deploy', 44),
  ('diophantine', 645),
  ('efferent', 54),
  ('elysee', -326),
  ('eradicate', 376),
  ('escritoire', 856),
  ('exorcism', -983),
  ('fiat', 170),
  ('filmy', -874),
  ('flatworm', 503),
  ('gestapo', 915),
  ('infra', -847),
  ('isis', -982),
  ('lindholm', 999),
  ('markham', 475),
  ('mincemeat', -880),
  ('moresby', 756),
  ('mycenae', 183),
  ('plugging', -266),
  ('smokescreen', 423),
  ('speakeasy', -745),
  ('vein', 813)
  }

for i <- 0..5
  println( i, subsetSum(s, snd, i).get() )
```


{{out}}


```txt

0, {(archbishop, -915), (gestapo, 915)}
1, {(fiat, 170), (vein, 813), (isis, -982)}
2, {(alliance, -624), (departure, 952), (elysee, -326)}
3, {(alliance, -624), (archbishop, -915), (departure, 952), (covariate, 590)}
4, {(markham, 475), (infra, -847), (eradicate, 376)}
5, {(flatworm, 503), (eradicate, 376), (filmy, -874)}

```



## Go


```go
package main

import "fmt"

type ww struct {
    word   string
    weight int
}

var input = []*ww{
    {"alliance", -624},
    {"archbishop", -915},
    {"balm", 397},
    {"bonnet", 452},
    {"brute", 870},
    {"centipede", -658},
    {"cobol", 362},
    {"covariate", 590},
    {"departure", 952},
    {"deploy", 44},
    {"diophantine", 645},
    {"efferent", 54},
    {"elysee", -326},
    {"eradicate", 376},
    {"escritoire", 856},
    {"exorcism", -983},
    {"fiat", 170},
    {"filmy", -874},
    {"flatworm", 503},
    {"gestapo", 915},
    {"infra", -847},
    {"isis", -982},
    {"lindholm", 999},
    {"markham", 475},
    {"mincemeat", -880},
    {"moresby", 756},
    {"mycenae", 183},
    {"plugging", -266},
    {"smokescreen", 423},
    {"speakeasy", -745},
    {"vein", 813},
}

type sss struct {
    subset []*ww
    sum    int
}

func main() {
    ps := []sss{{nil, 0}}
    for _, i := range input {
        pl := len(ps)
        for j := 0; j < pl; j++ {
            subset := append([]*ww{i}, ps[j].subset...)
            sum := i.weight + ps[j].sum
            if sum == 0 {
                fmt.Println("this subset sums to 0:")
                for _, i := range subset {
                    fmt.Println(*i)
                }
                return
            }
            ps = append(ps, sss{subset, sum})
        }
    }
    fmt.Println("no subset sums to 0")
}
```

{{out}}

```txt

this subset sums to 0:
{elysee -326}
{efferent 54}
{deploy 44}
{covariate 590}
{cobol 362}
{centipede -658}
{bonnet 452}
{balm 397}
{archbishop -915}

```



## Haskell


```haskell
combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations _ []     = []
combinations k (x:xs) = map (x:) (combinations (k - 1) xs) ++
                          combinations k xs

data W = W { word   :: String,
             weight :: Int }

solver :: [W] -> [[W]]
solver it = [comb | n <- [1 .. length it],
                    comb <- combinations n it,
                    sum (map weight comb) == 0]

items =  [W "alliance"    (-624),  W "archbishop" (-915),
          W "balm"          397,   W "bonnet"       452,
          W "brute"         870,   W "centipede"  (-658),
          W "cobol"         362,   W "covariate"    590,
          W "departure"     952,   W "deploy"        44,
          W "diophantine"   645,   W "efferent"      54,
          W "elysee"      (-326),  W "eradicate"    376,
          W "escritoire"    856,   W "exorcism"   (-983),
          W "fiat"          170,   W "filmy"      (-874),
          W "flatworm"      503,   W "gestapo"      915,
          W "infra"       (-847),  W "isis"       (-982),
          W "lindholm"      999,   W "markham"      475,
          W "mincemeat"   (-880),  W "moresby"      756,
          W "mycenae"       183,   W "plugging"   (-266),
          W "smokescreen"   423,   W "speakeasy"  (-745),
          W "vein"          813]

main = print $ map word $ head $ solver items
```

{{out}}

```txt
["archbishop","gestapo"]
```


Non brute-force: the list of numbers used here are different, and difficult for a bruteforce method.

```haskell
subsum :: Int -> [Int] -> [Int]
subsum w =
  snd . head . filter ((== w) . fst) . (++ [(w, [])]) . foldl s [(0, [])]
  where
    s a x = merge a $ map f a
      where
        f (a, l) = (a + x, l ++ [x])

    -- Keep list of sums sorted and unique.
    merge [] a = a
    merge a [] = a
    merge a@((av, al):as) b@((bv, bl):bs)
      | av < bv = (av, al) : merge as b
      | av == bv = (bv, bl) : merge as bs
      | otherwise = (bv, bl) : merge a bs

items :: [Int]
items = [-61, 1, 32, 373, 311, 249, 311, 32, -92, -185, -433,
 -402, -247, 156, 125, 249, 32, -464, -278, 218, 32, -123,
 -216, 373, -185, -402, 156, -402, -61, -31, 902 ]

main :: IO ()
main = print $ subsum 0 items
```

{{out}}

```txt
[-61,32,373,311,249,311,32,-92,-185,-433,-402,-247,156,125,249,32,-464,-278,218,32,-123,-216,373,-185,-402,156,-402,-61,902]
```


=={{header|Icon}} and {{header|Unicon}}==
{{trans|Ruby}}

```Icon
link printf,lists

procedure main()
   BruteZeroSubset(string2table(
        "alliance/-624/archbishop/-915/balm/397/bonnet/452/brute/870/_
         centipede/-658/cobol/362/covariate/590/departure/952/deploy/44/_
         diophantine/645/efferent/54/elysee/-326/eradicate/376/escritoire/856/_
         exorcism/-983/fiat/170/filmy/-874/flatworm/503/gestapo/915/infra/-847/_
         isis/-982/lindholm/999/markham/475/mincemeat/-880/moresby/756/_
         mycenae/183/plugging/-266/smokescreen/423/speakeasy/-745/vein/813/"))
end

procedure BruteZeroSubset(words)                # brute force 1 of each length
   every n := 1 to *words do {
      every t := tcomb(words,n) do {            # generate combination
         every (sum := 0) +:= words[!t]         # sum combination
         if sum = 0 then {
            printf("A zero-sum subset of length %d : %s\n",n,list2string(sort(t)))
            break next                          # found one
            }
         }
         printf("No zero-sum subsets of length %d\n",n)
      }
end

# helper procedures

procedure tcomb(T, i)		    #: Table (key) combinations
   local K
   every put(K := [],key(T))        # list of keys
   every suspend lcomb(K,i)         # return list combs
end

procedure list2string(L)            #: format list as a string
   every (s := "[ ") ||:= !L || " " # reformat as string
   return s || "]"
end

procedure string2table(s,d)         #: format string "k1/v1/.../kn/vn" as table
   T := table()
   /d := "/"
   s ? until pos(0) do
      T[1(tab(find(d)),=d)] := numeric(1(tab(find(d)),=d))

   return T
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]
[http://www.cs.arizona.edu/icon/library/src/procs/lists.icn lists.icn provides lcomb for list combinations]

{{Out}}
```txt
No zero-sum subsets of length 1
A zero-sum subset of length 2 : [ archbishop gestapo ]
A zero-sum subset of length 3 : [ centipede markham mycenae ]
A zero-sum subset of length 4 : [ alliance balm deploy mycenae ]
A zero-sum subset of length 5 : [ balm eradicate isis markham plugging ]
A zero-sum subset of length 6 : [ archbishop balm escritoire exorcism fiat markham ]
A zero-sum subset of length 7 : [ balm bonnet cobol fiat filmy isis markham ]
A zero-sum subset of length 8 : [ balm bonnet cobol filmy markham mincemeat speakeasy vein ]
A zero-sum subset of length 9 : [ alliance archbishop balm bonnet cobol lindholm markham mincemeat plugging ]
A zero-sum subset of length 10 : [ archbishop balm bonnet cobol filmy gestapo markham mincemeat speakeasy vein ]
A zero-sum subset of length 11 : [ alliance archbishop balm bonnet cobol deploy gestapo isis markham mincemeat moresby ]
A zero-sum subset of length 12 : [ alliance archbishop balm bonnet cobol exorcism fiat lindholm markham mincemeat plugging vein ]
A zero-sum subset of length 13 : [ alliance archbishop balm bonnet brute cobol deploy diophantine exorcism markham mincemeat plugging smokescreen ]
A zero-sum subset of length 14 : [ alliance archbishop balm bonnet centipede cobol diophantine exorcism lindholm markham mincemeat mycenae plugging vein ]
A zero-sum subset of length 15 : [ alliance archbishop balm bonnet cobol diophantine fiat gestapo isis markham mincemeat mycenae plugging speakeasy vein ]
A zero-sum subset of length 16 : [ alliance archbishop balm bonnet brute cobol diophantine eradicate exorcism filmy infra lindholm markham mincemeat plugging vein ]
A zero-sum subset of length 17 : [ alliance archbishop balm bonnet centipede cobol covariate deploy diophantine exorcism filmy lindholm markham mincemeat plugging smokescreen vein ]
A zero-sum subset of length 18 : [ alliance archbishop balm bonnet centipede cobol diophantine eradicate escritoire exorcism filmy gestapo infra markham mincemeat moresby plugging vein ]
A zero-sum subset of length 19 : [ alliance archbishop balm bonnet cobol diophantine efferent exorcism filmy flatworm gestapo infra isis lindholm markham mincemeat moresby plugging vein ]
A zero-sum subset of length 20 : [ alliance archbishop balm bonnet centipede cobol deploy diophantine efferent escritoire exorcism fiat filmy gestapo isis lindholm markham mincemeat plugging vein ]
A zero-sum subset of length 21 : [ alliance archbishop balm bonnet brute centipede cobol covariate deploy diophantine efferent elysee exorcism filmy gestapo infra markham mincemeat moresby plugging vein ]
A zero-sum subset of length 22 : [ alliance archbishop balm bonnet centipede cobol deploy diophantine eradicate escritoire exorcism fiat filmy gestapo isis lindholm markham mincemeat plugging smokescreen speakeasy vein ]
A zero-sum subset of length 23 : [ alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine exorcism filmy flatworm gestapo infra isis markham mincemeat moresby plugging speakeasy vein ]
A zero-sum subset of length 24 : [ alliance archbishop balm bonnet brute centipede cobol departure deploy diophantine efferent escritoire exorcism filmy gestapo infra isis markham mincemeat moresby mycenae plugging speakeasy vein ]
A zero-sum subset of length 25 : [ alliance archbishop balm bonnet brute centipede cobol covariate deploy diophantine efferent elysee eradicate exorcism filmy gestapo infra isis markham mincemeat moresby mycenae plugging smokescreen vein ]
A zero-sum subset of length 26 : [ alliance archbishop balm bonnet centipede cobol covariate departure deploy diophantine efferent elysee eradicate escritoire exorcism fiat filmy gestapo infra isis lindholm markham mincemeat plugging speakeasy vein ]
A zero-sum subset of length 27 : [ alliance archbishop balm bonnet brute centipede covariate departure deploy efferent elysee eradicate escritoire exorcism fiat filmy flatworm infra isis lindholm markham mincemeat moresby mycenae plugging smokescreen speakeasy ]
No zero-sum subsets of length 28
No zero-sum subsets of length 29
No zero-sum subsets of length 30
No zero-sum subsets of length 31
```



## J


Task data:


```J
text=:0 :0
alliance     -624
archbishop   -915
balm          397
bonnet        452
brute         870
centipede    -658
cobol         362
covariate     590
departure     952
deploy         44
diophantine   645
efferent       54
elysee       -326
eradicate     376
escritoire    856
exorcism     -983
fiat          170
filmy        -874
flatworm      503
gestapo       915
infra        -847
isis         -982
lindholm      999
markham       475
mincemeat    -880
moresby       756
mycenae       183
plugging     -266
smokescreen   423
speakeasy    -745
vein          813
)

words=:{.@;:;._2 text
numbs=:+/|:0&".;._2 text
```


Implementation:


```J
wsum0=:4 :0
  p=:(#~ 0&<)y
  n=:(#~ 0&>)y
  poss=: +/@#~2#.inv 2 i.@^#
  P=:poss p
  N=:poss -n
  choose=:(1{I.P e. N){P
  keep=: [ #~ #&2@#@[ #: choose i.~ ]
  ;:inv words #~y e. (p keep P),n keep N
)
```


Task example:


```J
   words wsum0 numbs
centipede markham mycenae
```


Note also that there are over 300,000 valid solutions here. More than can be comfortably displayed:


```J
   Ps=: </.~ /:~ (I.P e. N){P
   Ns=: </.~ /:~ (I.N e. P){N
   +/#@,@{"1 Ps,.Ns
349168
```


(One of those is the empty solution, but the rest of them are valid.)


## Java

{{trans|Kotlin}}

```Java
public class SubsetSum {
    private static class Item {
        private String word;
        private int weight;

        public Item(String word, int weight) {
            this.word = word;
            this.weight = weight;
        }

        @Override
        public String toString() {
            return String.format("(%s, %d)", word, weight);
        }
    }

    private static Item[] items = new Item[]{
        new Item("alliance", -624),
        new Item("archbishop", -915),
        new Item("balm", 397),
        new Item("bonnet", 452),
        new Item("brute", 870),
        new Item("centipede", -658),
        new Item("cobol", 362),
        new Item("covariate", 590),
        new Item("departure", 952),
        new Item("deploy", 44),
        new Item("diophantine", 645),
        new Item("efferent", 54),
        new Item("elysee", -326),
        new Item("eradicate", 376),
        new Item("escritoire", 856),
        new Item("exorcism", -983),
        new Item("fiat", 170),
        new Item("filmy", -874),
        new Item("flatworm", 503),
        new Item("gestapo", 915),
        new Item("infra", -847),
        new Item("isis", -982),
        new Item("lindholm", 999),
        new Item("markham", 475),
        new Item("mincemeat", -880),
        new Item("moresby", 756),
        new Item("mycenae", 183),
        new Item("plugging", -266),
        new Item("smokescreen", 423),
        new Item("speakeasy", -745),
        new Item("vein", 813),
    };

    private static final int n = items.length;
    private static final int[] indices = new int[n];
    private static int count = 0;

    private static final int LIMIT = 5;

    private static void zeroSum(int i, int w) {
        if (i != 0 && w == 0) {
            for (int j = 0; j < i; ++j) {
                System.out.printf("%s ", items[indices[j]]);
            }
            System.out.println("\n");
            if (count < LIMIT) count++;
            else return;
        }
        int k = (i != 0) ? indices[i - 1] + 1 : 0;
        for (int j = k; j < n; ++j) {
            indices[i] = j;
            zeroSum(i + 1, w + items[j].weight);
            if (count == LIMIT) return;
        }
    }

    public static void main(String[] args) {
        System.out.printf("The weights of the following %d subsets add up to zero:\n\n", LIMIT);
        zeroSum(0, 0);
    }
}
```

{{out}}

```txt
The weights of the following 5 subsets add up to zero:

(alliance, -624) (archbishop, -915) (balm, 397) (bonnet, 452) (brute, 870) (centipede, -658) (cobol, 362) (covariate, 590) (departure, 952) (deploy, 44) (diophantine, 645) (efferent, 54) (elysee, -326) (eradicate, 376) (escritoire, 856) (exorcism, -983) (fiat, 170) (filmy, -874) (flatworm, 503) (mincemeat, -880) (plugging, -266) (speakeasy, -745)

(alliance, -624) (archbishop, -915) (balm, 397) (bonnet, 452) (brute, 870) (centipede, -658) (cobol, 362) (covariate, 590) (departure, 952) (deploy, 44) (diophantine, 645) (efferent, 54) (elysee, -326) (eradicate, 376) (escritoire, 856) (exorcism, -983) (infra, -847) (isis, -982) (mincemeat, -880) (moresby, 756) (mycenae, 183) (smokescreen, 423) (speakeasy, -745)

(alliance, -624) (archbishop, -915) (balm, 397) (bonnet, 452) (brute, 870) (centipede, -658) (cobol, 362) (covariate, 590) (departure, 952) (deploy, 44) (diophantine, 645) (efferent, 54) (elysee, -326) (eradicate, 376) (escritoire, 856) (fiat, 170) (infra, -847) (isis, -982) (markham, 475) (mincemeat, -880) (plugging, -266) (speakeasy, -745)

(alliance, -624) (archbishop, -915) (balm, 397) (bonnet, 452) (brute, 870) (centipede, -658) (cobol, 362) (covariate, 590) (departure, 952) (deploy, 44) (diophantine, 645) (efferent, 54) (elysee, -326) (eradicate, 376) (exorcism, -983) (fiat, 170) (infra, -847) (isis, -982) (mincemeat, -880) (moresby, 756) (plugging, -266) (vein, 813)

(alliance, -624) (archbishop, -915) (balm, 397) (bonnet, 452) (brute, 870) (centipede, -658) (cobol, 362) (covariate, 590) (departure, 952) (deploy, 44) (diophantine, 645) (efferent, 54) (elysee, -326) (eradicate, 376) (exorcism, -983) (fiat, 170) (infra, -847) (isis, -982) (smokescreen, 423)
```




## Julia


```julia
using Combinatorics

const pairs = [
    "alliance" => -624, "archbishop" => -915, "balm" => 397, "bonnet" => 452,
    "brute" => 870, "centipede" => -658, "cobol" => 362, "covariate" => 590,
    "departure" => 952, "deploy" => 44, "diophantine" => 645, "efferent" => 54,
    "elysee" => -326, "eradicate" => 376, "escritoire" => 856, "exorcism" => -983,
    "fiat" => 170, "filmy" => -874, "flatworm" => 503, "gestapo" => 915,
    "infra" => -847, "isis" => -982, "lindholm" => 999, "markham" => 475,
    "mincemeat" => -880, "moresby" => 756, "mycenae" => 183, "plugging" => -266,
    "smokescreen" => 423, "speakeasy" => -745, "vein" => 813]
const weights = [v for (k, v) in pairs]
const weightkeyed = Dict(Pair(v, k) for (k, v) in pairs)

function zerosums()
    total = 0
    for i in 1:length(weights)
        print("\nFor length $i: ")
        sets = [a for a in combinations(weights, i) if sum(a) == 0]
        if (n = length(sets)) == 0
            print("None")
        else
            total += n
            print("$n sets, example: ", map(x -> weightkeyed[x], rand(sets)))
        end
    end
    println("\n\nGrand total sets: $total")
end

zerosums()

```
{{out}}

```txt

For length 1: None
For length 2: 1 sets, example: ["archbishop", "gestapo"]
For length 3: 2 sets, example: ["centipede", "markham", "mycenae"]
For length 4: 9 sets, example: ["balm", "efferent", "filmy", "smokescreen"]
For length 5: 48 sets, example: ["departure", "elysee", "lindholm", "mincemeat", "speakeasy"]
For length 6: 178 sets, example: ["alliance", "bonnet", "centipede", "isis", "lindholm", "vein"]
For length 7: 629 sets, example: ["balm", "brute", "cobol", "deploy", "filmy", "isis", "mycenae"]
For length 8: 1634 sets, example: ["alliance", "brute", "centipede", "departure", "mincemeat", "mycenae", "plugging", "smokescreen"]
For length 9: 4040 sets, example: ["alliance", "brute", "deploy", "efferent", "elysee", "fiat", "filmy", "flatworm", "mycenae"]
For length 10: 8673 sets, example: ["archbishop", "brute", "escritoire", "filmy", "gestapo", "isis", "lindholm", "mincemeat", "moresby", "speakeasy"]
For length 11: 15680 sets, example: ["cobol", "covariate", "deploy", "efferent", "elysee", "eradicate", "exorcism", "filmy", "flatworm", "lindholm", "speakeasy"]
For length 12: 25492 sets, example: ["alliance", "balm", "diophantine", "efferent", "eradicate", "fiat", "filmy", "flatworm", "infra", "isis", "lindholm", "mycenae"]
For length 13: 35940 sets, example: ["alliance", "archbishop", "bonnet", "departure", "deploy", "efferent", "fiat", "filmy", "gestapo", "infra", "moresby", "mycenae", "plugging"]
For length 14: 44920 sets, example: ["archbishop", "centipede", "cobol", "covariate", "deploy", "efferent", "eradicate", "escritoire", "fiat", "gestapo", "isis", "mincemeat", "speakeasy", "vein"]
For length 15: 49368 sets, example: ["alliance", "archbishop", "bonnet", "covariate", "departure", "diophantine", "efferent", "elysee", "eradicate", "escritoire", "isis", "mincemeat", "plugging", "speakeasy", "vein"]
For length 16: 47835 sets, example: ["alliance", "archbishop", "balm", "bonnet", "centipede", "cobol", "covariate", "departure", "elysee", "flatworm", "infra", "isis", "moresby", "mycenae", "plugging", "smokescreen"]
For length 17: 40960 sets, example: ["balm", "bonnet", "centipede", "cobol", "covariate", "departure", "deploy", "diophantine", "efferent", "elysee", "eradicate", "fiat", "filmy", "isis", "mincemeat", "smokescreen", "speakeasy"]
For length 18: 31139 sets, example: ["balm", "bonnet", "centipede", "covariate", "departure", "deploy", "diophantine", "efferent", "elysee", "eradicate", "exorcism", "gestapo", "infra", "isis", "mincemeat", "mycenae", "speakeasy", "vein"]
For length 19: 20530 sets, example: ["alliance", "archbishop", "balm", "bonnet", "departure", "deploy", "elysee", "exorcism", "fiat", "gestapo", "infra", "isis", "lindholm", "markham", "mincemeat", "mycenae", "plugging", "smokescreen", "vein"]
For length 20: 11926 sets, example: ["archbishop", "bonnet", "brute", "centipede", "cobol", "deploy", "diophantine", "elysee", "eradicate", "exorcism", "fiat", "isis", "lindholm", "markham", "mincemeat", "moresby", "mycenae", "plugging", "smokescreen", "speakeasy"]
For length 21: 6089 sets, example: ["alliance", "balm", "bonnet", "brute", "centipede", "cobol", "covariate", "departure", "deploy", "elysee", "escritoire", "exorcism", "filmy", "flatworm", "infra", "isis", "markham", "mincemeat", "moresby", "mycenae", "plugging"]
For length 22: 2688 sets, example: ["archbishop", "balm", "bonnet", "brute", "centipede", "covariate", "deploy", "diophantine", "elysee", "eradicate", "exorcism", "filmy", "flatworm", "gestapo", "isis", "markham", "mincemeat", "moresby", "mycenae", "plugging", "smokescreen", "speakeasy"]
For length 23: 956 sets, example: ["alliance", "archbishop", "balm", "brute", "centipede", "cobol", "departure", "deploy", "efferent", "elysee", "eradicate", "escritoire", "exorcism", "fiat", "gestapo", "infra", "isis", "lindholm", "markham", "mincemeat", "moresby", "plugging", "speakeasy"]
For length 24: 341 sets, example: ["alliance", "archbishop", "balm", "brute", "centipede", "cobol", "covariate", "departure", "deploy", "diophantine", "efferent", "elysee", "eradicate", "escritoire", "exorcism", "infra", "isis", "lindholm", "markham", "mincemeat", "mycenae", "plugging", "smokescreen", "speakeasy"]
For length 25: 73 sets, example: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "departure", "deploy", "diophantine", "efferent", "elysee", "exorcism", "fiat", "filmy", "flatworm", "gestapo", "infra", "isis", "lindholm", "markham", "mincemeat", "mycenae", "speakeasy", "vein"]
For length 26: 14 sets, example: ["alliance", "archbishop", "balm", "bonnet", "centipede", "cobol", "departure", "diophantine", "efferent", "elysee", "eradicate", "escritoire", "exorcism", "fiat", "filmy", "flatworm", "gestapo", "infra", "isis", "lindholm", "mincemeat", "mycenae", "plugging", "smokescreen", "speakeasy", "vein"]
For length 27: 2 sets, example: ["alliance", "archbishop", "balm", "brute", "centipede", "cobol", "covariate", "deploy", "diophantine", "efferent", "elysee", "eradicate", "exorcism", "fiat", "filmy", "flatworm", "gestapo", "infra", "isis", "lindholm", "mincemeat", "moresby", "mycenae", "plugging", "smokescreen", "speakeasy", "vein"]
For length 28: None
For length 29: None
For length 30: None
For length 31: None

Grand total sets: 349167

```



## Kotlin

{{trans|C}}

```scala
// version 1.1.2

class Item(val word: String, val weight: Int) {
    override fun toString() = "($word $weight)"
}

val items = arrayOf(
    Item("alliance",   -624),
    Item("archbishop", -915),
    Item("balm",        397),
    Item("bonnet",      452),
    Item("brute",       870),
    Item("centipede",  -658),
    Item("cobol",       362),
    Item("covariate",   590),
    Item("departure",   952),
    Item("deploy",       44),
    Item("diophantine", 645),
    Item("efferent",     54),
    Item("elysee",     -326),
    Item("eradicate",   376),
    Item("escritoire",  856),
    Item("exorcism",   -983),
    Item("fiat",        170),
    Item("filmy",      -874),
    Item("flatworm",    503),
    Item("gestapo",     915),
    Item("infra",      -847),
    Item("isis",       -982),
    Item("lindholm",    999),
    Item("markham",     475),
    Item("mincemeat",  -880),
    Item("moresby",     756),
    Item("mycenae",     183),
    Item("plugging",   -266),
    Item("smokescreen", 423),
    Item("speakeasy",  -745),
    Item("vein",        813)
)

val n = items.size
val indices = IntArray(n)
var count = 0

const val LIMIT = 5

fun zeroSum(i: Int, w: Int) {
    if (i != 0 && w == 0) {
        for (j in 0 until i) print("${items[indices[j]]} ")
        println("\n")
        if (count < LIMIT) count++ else return
    }
    val k = if (i != 0) indices[i - 1] + 1 else 0
    for (j in k until n) {
        indices[i] = j
        zeroSum(i + 1, w + items[j].weight)
        if (count == LIMIT) return
    }
}

fun main(args: Array<String>) {
    println("The weights of the following $LIMIT subsets add up to zero:\n")
    zeroSum(0, 0)
}
```


{{out}}

```txt

The weights of the following 5 subsets add up to zero:

(alliance -624) (archbishop -915) (balm 397) (bonnet 452) (brute 870) (centipede -658) (cobol 362) (covariate 590) (departure 952) (deploy 44) (diophantine 645) (efferent 54) (elysee -326) (eradicate 376) (escritoire 856) (exorcism -983) (fiat 170) (filmy -874) (flatworm 503) (mincemeat -880) (plugging -266) (speakeasy -745)

(alliance -624) (archbishop -915) (balm 397) (bonnet 452) (brute 870) (centipede -658) (cobol 362) (covariate 590) (departure 952) (deploy 44) (diophantine 645) (efferent 54) (elysee -326) (eradicate 376) (escritoire 856) (exorcism -983) (infra -847) (isis -982) (mincemeat -880) (moresby 756) (mycenae 183) (smokescreen 423) (speakeasy -745)

(alliance -624) (archbishop -915) (balm 397) (bonnet 452) (brute 870) (centipede -658) (cobol 362) (covariate 590) (departure 952) (deploy 44) (diophantine 645) (efferent 54) (elysee -326) (eradicate 376) (escritoire 856) (fiat 170) (infra -847) (isis -982) (markham 475) (mincemeat -880) (plugging -266) (speakeasy -745)

(alliance -624) (archbishop -915) (balm 397) (bonnet 452) (brute 870) (centipede -658) (cobol 362) (covariate 590) (departure 952) (deploy 44) (diophantine 645) (efferent 54) (elysee -326) (eradicate 376) (exorcism -983) (fiat 170) (infra -847) (isis -982) (mincemeat -880) (moresby 756) (plugging -266) (vein 813)

(alliance -624) (archbishop -915) (balm 397) (bonnet 452) (brute 870) (centipede -658) (cobol 362) (covariate 590) (departure 952) (deploy 44) (diophantine 645) (efferent 54) (elysee -326) (eradicate 376) (exorcism -983) (fiat 170) (infra -847) (isis -982) (smokescreen 423)

```



## Mathematica


```Mathematica
a = {{"alliance", -624}, {"archbishop", -915}, {"balm", 397}, {"bonnet", 452},
{"brute", 870}, {"centipede", -658}, {"cobol", 362}, {"covariate",  590},{"departure", 952},
{"deploy", 44}, {"diophantine", 645}, {"efferent", 54}, {"elysee", -326}, {"eradicate", 376},
{"escritoire", 856}, {"exorcism", -983}, {"fiat", 170}, {"filmy", -874}, {"flatworm", 503},
{"gestapo", 915}, {"infra", -847}, {"isis", -982}, {"lindholm", 999}, {"markham", 475},
{"mincemeat", -880}, {"moresby", 756}, {"mycenae", 183}, {"plugging", -266}, {"smokescreen", 423},
{"speakeasy", -745}, {"vein", 813}};

result = Rest@Select[ Subsets[a, 7], (Total[#[[;; , 2]]] == 0) &];
Map[ (Print["A zero-sum subset of length ", Length[#],  " : ", #[[;; , 1]]])& , result ]
```



```txt
A zero-sum subset of length 2 : {archbishop,gestapo}
A zero-sum subset of length 3 : {centipede,markham,mycenae}
A zero-sum subset of length 3 : {exorcism,fiat,vein}
A zero-sum subset of length 4 : {alliance,balm,deploy,mycenae}
A zero-sum subset of length 4 : {balm,efferent,filmy,smokescreen}
A zero-sum subset of length 4 : {bonnet,elysee,escritoire,isis}
A zero-sum subset of length 4 : {brute,centipede,efferent,plugging}
....
```


The above code uses a brute-force approach, but Mathematica includes several solution schemes that can be used to solve this problem. We can cast it as an integer linear programming problem, and thus find the largest or smallest subset sum, or even sums with specific constraints, such as a sum using three negative values and nine positive values.


```Mathematica
a = {{"alliance", -624}, {"archbishop", -915}, {"balm", 397}, {"bonnet", 452},
{"brute", 870}, {"centipede", -658}, {"cobol", 362}, {"covariate",  590},{"departure", 952},
{"deploy", 44}, {"diophantine", 645}, {"efferent", 54}, {"elysee", -326}, {"eradicate", 376},
{"escritoire", 856}, {"exorcism", -983}, {"fiat", 170}, {"filmy", -874}, {"flatworm", 503},
{"gestapo", 915}, {"infra", -847}, {"isis", -982}, {"lindholm", 999}, {"markham", 475},
{"mincemeat", -880}, {"moresby", 756}, {"mycenae", 183}, {"plugging", -266}, {"smokescreen", 423},
{"speakeasy", -745}, {"vein", 813}};

desiredValue = 0;
aNames = #[[1]] & /@ a;
aValues = #[[2]] & /@ a;
aOnes = ConstantArray[1, Length[a]];
aZeroOnes = ConstantArray[{0, 1}, Length[a]];
Off[LinearProgramming::lpip];

maxSoln =
 LinearProgramming[-aOnes, {aValues}, {{desiredValue, 0}}, aZeroOnes, Integers];

Print["Maximal solution: ", Select[Transpose[{maxSoln*aValues, aNames}], #[[1]] != 0 &]];

minSoln =
 LinearProgramming[
  aOnes, {aValues, aOnes}, {{desiredValue, 0}, {1, 1}}, aZeroOnes, Integers];

Print["Minimal solution: ", Select[Transpose[{minSoln*aValues, aNames}], #[[1]] != 0 &]];

threeNineSoln =
 LinearProgramming[
  aOnes, {aValues,
          Boole[# < 0] & /@ aValues,
          Boole[# > 0] & /@ aValues},
  {{desiredValue, 0}, {3, 0}, {9, 0}}, aZeroOnes, Integers];

Print["3 -ves, 9 +ves: ", Select[Transpose[{threeNineSoln*aValues, aNames}], #[[1]] != 0 &]];

```



```txt
Maximal solution: {{-624, alliance}, {-915, archbishop}, {397, balm},
    {870, brute}, {-658, centipede}, {362, cobol}, {590, covariate},
    {44, deploy}, {645, diophantine}, {54, efferent}, {-326, elysee},
    {376, eradicate}, {-983, exorcism}, {170, fiat}, {-874, filmy},
    {503, flatworm}, {915, gestapo}, {-847, infra}, {-982, isis},
    {999, lindholm}, {-880, mincemeat}, {756, moresby}, {183, mycenae},
    {-266, plugging}, {423, smokescreen}, {-745, speakeasy}, {813, vein}}

Minimal solution: {{-915, archbishop}, {915, gestapo}}

3 -ves, 9 +ves: {{-915, archbishop}, {397, balm}, {452, bonnet},
    {362, cobol}, {44, deploy}, {54, efferent}, {-983, exorcism},
    {170, fiat}, {503, flatworm}, {-982, isis}, {475, markham},
    {423, smokescreen}}.
```


=={{header|Modula-2}}==

```modula2
MODULE SubsetSum;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

TYPE
    String = ARRAY[0..63] OF CHAR;
    Item = RECORD
        word : String;
        weight : INTEGER;
    END;

PROCEDURE WriteItem(self : Item);
VAR buf : String;
BEGIN
    FormatString("(%s, %i)", buf, self.word, self.weight);
    WriteString(buf);
END WriteItem;

CONST N = 31;
VAR
    items : ARRAY[0..N] OF Item;
    indicies : ARRAY[0..N] OF INTEGER;
    count : INTEGER;
PROCEDURE Init;
VAR i : INTEGER;
BEGIN
    items[0] := Item{"alliance", -624};
    items[1] := Item{"archbishop", -915};
    items[2] := Item{"balm", 397};
    items[3] := Item{"bonnet", 452};
    items[4] := Item{"brute", 870};
    items[5] := Item{"centipede", -658};
    items[6] := Item{"cobol", 362};
    items[7] := Item{"covariate", 590};
    items[8] := Item{"departure", 952};
    items[9] := Item{"deploy", 44};
    items[10] := Item{"diophantine", 645};
    items[11] := Item{"efferent", 54};
    items[12] := Item{"elysee", -326};
    items[13] := Item{"eradicate", 376};
    items[14] := Item{"escritoire", 856};
    items[15] := Item{"exorcism", -983};
    items[16] := Item{"fiat", 170};
    items[17] := Item{"filmy", -874};
    items[18] := Item{"flatworm", 503};
    items[19] := Item{"gestapo", 915};
    items[20] := Item{"infra", -847};
    items[21] := Item{"isis", -982};
    items[22] := Item{"lindholm", 999};
    items[23] := Item{"markham", 475};
    items[24] := Item{"mincemeat", -880};
    items[25] := Item{"moresby", 756};
    items[26] := Item{"mycenae", 183};
    items[27] := Item{"plugging", -266};
    items[28] := Item{"smokescreen", 423};
    items[29] := Item{"speakeasy", -745};
    items[30] := Item{"vein", 813};

    count := 0;
END Init;

CONST LIMIT = 5;
PROCEDURE ZeroSum(i,w : INTEGER);
VAR j,k : INTEGER;
BEGIN
    IF (i#0) AND (w=0) THEN
        FOR j:=0 TO i-1 DO
            WriteItem(items[indicies[j]]);
            WriteString(" ");
        END;
        WriteLn;
        WriteString("---------------");
        WriteLn;
        IF count<LIMIT THEN
            INC(count)
        ELSE
            RETURN;
        END;
    END;
    IF i#0 THEN
        k := indicies[i-1]+1;
    ELSE
        k := 0;
    END;
    FOR j:=k TO N-1 DO
        indicies[i] := j;
        ZeroSum(i+1,w+items[j].weight);
        IF count=LIMIT THEN RETURN; END;
    END;
END ZeroSum;

VAR buf : ARRAY[0..63] OF CHAR;
VAR d : INTEGER;
BEGIN
    Init;
    d := LIMIT;
    FormatString("The weights of the following %i subsets add up to zero:\n\n", buf, d);
    WriteString(buf);
    ZeroSum(0,0);

    ReadChar;
END SubsetSum.
```



## OCaml


Just search randomly until a result is found:


```ocaml
let d =
  [ "alliance", -624;  "archbishop", -915;  "balm", 397;  "bonnet", 452;
    "brute", 870;  "centipede", -658;  "cobol", 362;  "covariate", 590;
    "departure", 952;  "deploy", 44;  "diophantine", 645;  "efferent", 54;
    "elysee", -326;  "eradicate", 376;  "escritoire", 856;  "exorcism", -983;
    "fiat", 170;  "filmy", -874;  "flatworm", 503;  "gestapo", 915;
    "infra", -847;  "isis", -982;  "lindholm", 999;  "markham", 475;
    "mincemeat", -880;  "moresby", 756;  "mycenae", 183;  "plugging", -266;
    "smokescreen", 423;  "speakeasy", -745;  "vein", 813; ]

let sum = List.fold_left (fun sum (_,w) -> sum + w) 0
let p = function [] -> false | lst -> (sum lst) = 0

let take lst set =
  let x = List.nth set (Random.int (List.length set)) in
  (x::lst, List.filter (fun y -> y <> x) set)

let swap (a, b) = (b, a)
let pop lst set = swap (take set lst)

let () =
  Random.self_init ();
  let rec aux lst set =
    let f =
      match lst, set with
      | [], _ -> take
      | _, [] -> pop
      | _ -> if Random.bool () then take else pop
    in
    let lst, set = f lst set in
    if p lst then lst
    else aux lst set
  in
  let res = aux [] d in
  List.iter (fun (n,w) -> Printf.printf " %4d\t%s\n" w n) res
```



## Perl

{{libheader|ntheory}}

```perl
use ntheory qw/:all/;

my %pairs = (
    alliance => -624, archbishop => -915, balm => 397, bonnet => 452,
    brute => 870, centipede => -658, cobol => 362, covariate => 590,
    departure => 952, deploy => 44, diophantine => 645, efferent => 54,
    elysee => -326, eradicate => 376, escritoire => 856, exorcism => -983,
    fiat => 170, filmy => -874, flatworm => 503, gestapo => 915,
    infra => -847, isis => -982, lindholm => 999, markham => 475,
    mincemeat => -880, moresby => 756, mycenae => 183, plugging => -266,
    smokescreen => 423, speakeasy => -745, vein => 813 );
# sort so we get the same order each time
my @names = sort keys(%pairs);
my @weights = @pairs{@names};  # hash slice gives all values in same order

foreach my $n (1 .. @names) {
    forcomb {
        # Remove the "lastfor, " to get all combinations
        lastfor, print "Length $n: @names[@_]\n" if vecsum(@weights[@_]) == 0;
    } @names, $n;
}
```

Printing just the first one found for each number of elements:
{{out}}

```txt

Length 2: archbishop gestapo
Length 3: centipede markham mycenae
Length 4: alliance balm deploy mycenae
Length 5: alliance brute covariate deploy mincemeat
Length 6: alliance archbishop balm deploy gestapo mycenae
Length 7: alliance archbishop bonnet cobol departure exorcism moresby
Length 8: alliance archbishop balm bonnet fiat flatworm isis lindholm
Length 9: alliance archbishop balm bonnet brute covariate eradicate mincemeat plugging
... to length 27 ...

```


We can also use different modules for this brute force method.  Assuming the same pairs/names/weights variables:

```perl
use List::Util qw/sum/;
use Algorithm::Combinatorics qw/combinations/;
foreach my $n (1 .. @names) {
  my $iter = combinations([0..$#weights], $n);
  while (my $c = $iter->next) {
    next if sum(@weights[@$c]);
    print "Length $n: @names[@$c]\n";
    last;
  }
}
```



## Perl 6


```perl6
my @pairs =
    alliance => -624, archbishop => -915, balm => 397, bonnet => 452,
    brute => 870, centipede => -658, cobol => 362, covariate => 590,
    departure => 952, deploy => 44, diophantine => 645, efferent => 54,
    elysee => -326, eradicate => 376, escritoire => 856, exorcism => -983,
    fiat => 170, filmy => -874, flatworm => 503, gestapo => 915,
    infra => -847, isis => -982, lindholm => 999, markham => 475,
    mincemeat => -880, moresby => 756, mycenae => 183, plugging => -266,
    smokescreen => 423, speakeasy => -745, vein => 813;
my @weights = @pairs».value;
my %name = @pairs.hash.invert;

.say for (1..27).hyper(:3batch).map: -> $n {
    given @weights.combinations($n).first({ 0 == [+] @^comb }) {
        when .so { "Length $n: ({.map: {%name{$_}}})" }
        default  { "Length $n: (none)" }
    }
}
```

{{out}}

```txt
Length 1: (none)
Length 2: (archbishop gestapo)
Length 3: (centipede markham mycenae)
Length 4: (alliance balm deploy mycenae)
Length 5: (alliance brute covariate deploy mincemeat)
Length 6: (alliance archbishop balm deploy gestapo mycenae)
Length 7: (alliance archbishop bonnet cobol departure exorcism moresby)
Length 8: (alliance archbishop balm bonnet fiat flatworm isis lindholm)
Length 9: (alliance archbishop balm bonnet brute covariate eradicate mincemeat plugging)
Length 10: (alliance archbishop balm bonnet brute centipede cobol departure deploy mincemeat)
Length 11: (alliance archbishop balm bonnet brute centipede cobol departure infra moresby speakeasy)
Length 12: (alliance archbishop balm bonnet brute centipede cobol covariate diophantine efferent elysee infra)
Length 13: (alliance archbishop balm bonnet brute centipede cobol covariate departure efferent eradicate filmy isis)
Length 14: (alliance archbishop balm bonnet brute centipede cobol covariate departure deploy elysee filmy markham speakeasy)
Length 15: (alliance archbishop balm bonnet brute centipede cobol covariate departure deploy elysee exorcism flatworm infra mycenae)
Length 16: (alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine elysee exorcism filmy gestapo infra)
Length 17: (alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine exorcism isis mincemeat mycenae plugging vein)
Length 18: (alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee exorcism filmy isis mycenae vein)
Length 19: (alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate exorcism fiat infra isis smokescreen)
Length 20: (alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate exorcism gestapo infra isis smokescreen speakeasy)
Length 21: (alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate exorcism flatworm infra lindholm mincemeat plugging speakeasy)
Length 22: (alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate escritoire exorcism fiat filmy flatworm mincemeat plugging speakeasy)
Length 23: (alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate escritoire exorcism infra isis mincemeat moresby mycenae smokescreen speakeasy)
Length 24: (alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee exorcism filmy gestapo infra markham mincemeat moresby mycenae plugging smokescreen speakeasy)
Length 25: (alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine eradicate exorcism fiat filmy flatworm infra isis lindholm markham mincemeat moresby mycenae plugging speakeasy)
Length 26: (alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine elysee eradicate escritoire exorcism fiat filmy gestapo infra isis markham mincemeat mycenae plugging speakeasy vein)
Length 27: (alliance archbishop balm bonnet brute centipede covariate departure deploy efferent elysee eradicate escritoire exorcism fiat filmy flatworm infra isis lindholm markham mincemeat moresby mycenae plugging smokescreen speakeasy)
```



## Phix

Simple Brute force

```Phix
sequence {words,weights} = columnize({{"alliance",   -624},
                                      {"archbishop", -915},
                                      {"balm",        397},
                                      {"bonnet",      452},
                                      {"brute",       870},
                                      {"centipede",  -658},
                                      {"cobol",       362},
                                      {"covariate",   590},
                                      {"departure",   952},
                                      {"deploy",       44},
                                      {"diophantine", 645},
                                      {"efferent",     54},
                                      {"elysee",     -326},
                                      {"eradicate",   376},
                                      {"escritoire",  856},
                                      {"exorcism",   -983},
                                      {"fiat",        170},
                                      {"filmy",      -874},
                                      {"flatworm",    503},
                                      {"gestapo",     915},
                                      {"infra",      -847},
                                      {"isis",       -982},
                                      {"lindholm",    999},
                                      {"markham",     475},
                                      {"mincemeat",  -880},
                                      {"moresby",     756},
                                      {"mycenae",     183},
                                      {"plugging",   -266},
                                      {"smokescreen", 423},
                                      {"speakeasy",  -745},
                                      {"vein",        813}})

function comb(sequence pool, integer needed, done=0, sequence chosen={})
    if needed=0 then    -- got a full set
        integer t = 0
        for i=1 to length(chosen) do
            t += weights[chosen[i]]
        end for
        if t=0 then
            for i=1 to length(chosen) do
                chosen[i] = words[chosen[i]]
            end for
            printf(1,"%d: %s\n",{length(chosen),sprint(chosen)})
            return 1
        end if
    elsif done+needed<=length(pool) then
        -- get all combinations with and without the next item:
        done += 1
        if comb(pool,needed-1,done,append(chosen,pool[done]))
        or comb(pool,needed,done,chosen) then
            return 1
        end if
    end if
    return 0
end function

integer n = length(weights)
for i=1 to n do
    if comb(tagset(n),i)=0 then
        printf(1,"%d: No zero-sum subsets of that length\n",{i})
    end if
end for
```

{{out}}

```txt

1: No zero-sum subsets of that length
2: {"archbishop","gestapo"}
3: {"centipede","markham","mycenae"}
4: {"alliance","balm","deploy","mycenae"}
5: {"alliance","brute","covariate","deploy","mincemeat"}
6: {"alliance","archbishop","balm","deploy","gestapo","mycenae"}
7: {"alliance","archbishop","bonnet","cobol","departure","exorcism","moresby"}
8: {"alliance","archbishop","balm","bonnet","fiat","flatworm","isis","lindholm"}
9: {"alliance","archbishop","balm","bonnet","brute","covariate","eradicate","mincemeat","plugging"}
10: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","departure","deploy","mincemeat"}
11: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","departure","infra","moresby","speakeasy"}
12: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","covariate","diophantine","efferent","elysee","infra"}
13: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","covariate","departure","efferent","eradicate","filmy","isis"}
14: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","covariate","departure","deploy","elysee","filmy","markham","speakeasy"}
15: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","covariate","departure","deploy","elysee","exorcism","flatworm","infra","mycenae"}
16: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","covariate","departure","deploy","diophantine","elysee","exorcism","filmy","gestapo","infra"}
17: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","covariate","departure","deploy","diophantine","exorcism","isis","mincemeat","mycenae","plugging","vein"}
18: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","covariate","departure","deploy","diophantine","efferent","elysee","exorcism","filmy","isis","mycenae","vein"}
19: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","covariate","departure","deploy","diophantine","efferent","elysee","eradicate","exorcism","fiat","infra","isis","smokescreen"}
20: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","covariate","departure","deploy","diophantine","efferent","elysee","eradicate","exorcism","gestapo","infra","isis","smokescreen","speakeasy"}
21: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","covariate","departure","deploy","diophantine","efferent","elysee","eradicate","exorcism","flatworm","infra","lindholm","mincemeat","plugging","speakeasy"}
22: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","covariate","departure","deploy","diophantine","efferent","elysee","eradicate","escritoire","exorcism","fiat","filmy","flatworm","mincemeat","plugging","speakeasy"}
23: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","covariate","departure","deploy","diophantine","efferent","elysee","eradicate","escritoire","exorcism","infra","isis","mincemeat","moresby","mycenae","smokescreen","speakeasy"}
24: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","covariate","departure","deploy","diophantine","efferent","elysee","exorcism","filmy","gestapo","infra","markham","mincemeat","moresby","mycenae","plugging","smokescreen","speakeasy"}
25: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","covariate","departure","deploy","diophantine","eradicate","exorcism","fiat","filmy","flatworm","infra","isis","lindholm","markham","mincemeat","moresby","mycenae","plugging","speakeasy"}
26: {"alliance","archbishop","balm","bonnet","brute","centipede","cobol","covariate","departure","deploy","diophantine","elysee","eradicate","escritoire","exorcism","fiat","filmy","gestapo","infra","isis","markham","mincemeat","mycenae","plugging","speakeasy","vein"}
27: {"alliance","archbishop","balm","bonnet","brute","centipede","covariate","departure","deploy","efferent","elysee","eradicate","escritoire","exorcism","fiat","filmy","flatworm","infra","isis","lindholm","markham","mincemeat","moresby","mycenae","plugging","smokescreen","speakeasy"}
28: No zero-sum subsets of that length
29: No zero-sum subsets of that length
30: No zero-sum subsets of that length
31: No zero-sum subsets of that length

```


### Alternative

Using the harder set of weights from Go, and the version 1 approach of Python (modified to omit words and
using a dictionary so that fractional weights can be accomodated).

This is significantly faster (near instant, in fact) than an "all possible combinations" approach.

Note that new_dict(tid) has been introduced for this task in 0.8.0, which has not yet been shipped.

Shows the first zero-sum subset found, only.

```Phix
constant weights = {-61, 1, 32, 373, 311, 249, 311, 32, -92, -185, -433,
                    -402, -247, 156, 125, 249, 32, -464, -278, 218, 32,
                    -123, -216, 373, -185, -402, 156, -402, -61, -31, 902 }

integer sums = new_dict()

for w=1 to length(weights) do
    -- make a separate modifiable copy of sums, otherwise
    -- it c/would mark sums[weights[w]*{2,3,etc}] as valid,
    -- ie there cannot be any w in the getd_by_index(node).
    integer s = new_dict(sums)
    atom v = weights[w]
    if getd_index(v,s)=NULL then setd(v,{w},s) end if

    sequence sk = getd_all_keys(s)
    for i=1 to length(sk) do
        integer node = getd_index(sk[i],sums)
        if node!=NULL and getd_index(sk[i]+v,s)=0 then
            setd(sk[i]+v,getd_by_index(node,sums)&w,s)
        end if
    end for
    destroy_dict(sums)
    sums = s

    integer node = getd_index(0,sums)
    if node!=0 then
        sequence s0 = getd_by_index(node,sums)
        atom t = 0  -- (sanity check)
        for i=1 to length(s0) do
            integer si = s0[i]
            t += weights[si]
            s0[i] = weights[si]
        end for
        printf(1,"Total %d for %s\n",{t,sprint(s0)})
        exit
    end if
end for
```

{{out}}

```txt

Total 0 for {-61,32,373,311,249,311,32,-92,-185,-433,-402,-247,156,125,249,32,-464,-278,218,32,-123,-216,373,-185,-402,156,-402,-61,902}

```



## PicoLisp


```PicoLisp
(de *Words
   (alliance . -624) (archbishop . -915) (balm . 397) (bonnet . 452)
   (brute . 870) (centipede . -658) (cobol . 362) (covariate . 590)
   (departure . 952) (deploy . 44) (diophantine . 645) (efferent . 54)
   (elysee . -326) (eradicate . 376) (escritoire . 856) (exorcism . -983)
   (fiat . 170) (filmy . -874) (flatworm . 503) (gestapo . 915)
   (infra . -847) (isis . -982) (lindholm . 999) (markham . 475)
   (mincemeat . -880) (moresby . 756) (mycenae . 183) (plugging . -266)
   (smokescreen . 423) (speakeasy . -745) (vein . 813) )
```

Minimal brute force solution:

```PicoLisp
(load "@lib/simul.l")  # For 'subsets'

(pick
   '((N)
      (find '((L) (=0 (sum cdr L)))
         (subsets N *Words) ) )
   (range 1 (length *Words)) )
```

{{Out}}

```txt
-> ((archbishop . -915) (gestapo . 915))
```



## Python


### Version 1


```python
words = { # some values are different from example
	"alliance": -624,	"archbishop": -925,	"balm":	397,
	"bonnet": 452,		"brute": 870,		"centipede": -658,
	"cobol": 362,		"covariate": 590,	"departure": 952,
	"deploy": 44,		"diophantine": 645,	"efferent": 54,
	"elysee": -326,		"eradicate": 376,	"escritoire": 856,
	"exorcism": -983,	"fiat": 170,		"filmy": -874,
	"flatworm": 503,	"gestapo": 915,		"infra": -847,
	"isis": -982,		"lindholm": 999,	"markham": 475,
	"mincemeat": -880,	"moresby": 756,		"mycenae": 183,
	"plugging": -266,	"smokescreen": 423,	"speakeasy": -745,
	"vein": 813
}

neg = 0
pos = 0
for (w,v) in words.iteritems():
	if v > 0: pos += v
	else:     neg += v

sums = [0] * (pos - neg + 1)

for (w,v) in words.iteritems():
	s = sums[:]
	if not s[v - neg]: s[v - neg] = (w,)

	for (i, w2) in enumerate(sums):
		if w2 and not s[i + v]:
			s[i + v] = w2 + (w,)

	sums = s
	if s[-neg]:
		for x in s[-neg]:
			print(x, words[x])
		break
```

{{out}}
```txt

('mycenae', 183)
('speakeasy', -745)
('bonnet', 452)
('lindholm', 999)
('cobol', 362)
('archbishop', -925)
('elysee', -326)

```


### Brute force


```python>>>
 from itertools import combinations
>>>
>>> word2weight = {"alliance": -624, "archbishop": -915, "balm": 397, "bonnet": 452,
  "brute": 870, "centipede": -658, "cobol": 362, "covariate": 590,
  "departure": 952, "deploy": 44, "diophantine": 645, "efferent": 54,
  "elysee": -326, "eradicate": 376, "escritoire": 856, "exorcism": -983,
  "fiat": 170, "filmy": -874, "flatworm": 503, "gestapo": 915,
  "infra": -847, "isis": -982, "lindholm": 999, "markham": 475,
  "mincemeat": -880, "moresby": 756, "mycenae": 183, "plugging": -266,
  "smokescreen": 423, "speakeasy": -745, "vein": 813}
>>> answer = None
>>> for r in range(1, len(word2weight)+1):
	if not answer:
		for comb in combinations(word2weight, r):
			if sum(word2weight[w] for w in comb) == 0:
				answer = [(w, word2weight[w]) for w in comb]
				break


>>> answer
[('archbishop', -915), ('gestapo', 915)]
```



## Racket


```racket

#lang racket

(define words
  '([alliance -624] [archbishop -915] [balm 397] [bonnet 452] [brute 870]
    [centipede -658] [cobol 362] [covariate 590] [departure 952] [deploy 44]
    [diophantine 645] [efferent 54] [elysee -326] [eradicate 376]
    [escritoire 856] [exorcism -983] [fiat 170] [filmy -874] [flatworm 503]
    [gestapo 915] [infra -847] [isis -982] [lindholm 999] [markham 475]
    [mincemeat -880] [moresby 756] [mycenae 183] [plugging -266]
    [smokescreen 423] [speakeasy -745] [vein 813]))

;; Simple brute-force solution to find the smallest subset
(define (nsubsets l n)
  (cond [(zero? n) '(())] [(null? l) '()]
        [else (append (for/list ([l2 (nsubsets (cdr l) (- n 1))])
                        (cons (car l) l2))
                      (nsubsets (cdr l) n))]))
(for*/first ([i (sub1 (length words))] [s (nsubsets words (add1 i))]
             #:when (zero? (apply + (map cadr s))))
  (map car s))
;; => '(archbishop gestapo)

;; Alternative: customize the subsets to ones with zero sum, abort early
;; if we're in a hopeless case (using the fact that weights are <1000)
(define (zero-subsets l)
  (define (loop l len n r sum)
    (cond [(zero? n) (when (zero? sum) (displayln (reverse r)))]
          [(and (pair? l) (<= sum (* 1000 n)))
           (when (< n len) (loop (cdr l) (sub1 len) n r sum))
           (loop (cdr l) (sub1 len) (sub1 n) (cons (caar l) r)
                 (+ (cadar l) sum))]))
  (define len (length l))
  (for ([i (sub1 len)]) (loop l len (add1 i) '() 0)))
(zero-subsets words)

```

{{out}}

```txt

'(archbishop gestapo) ; <- the first solution
(archbishop gestapo)
(exorcism fiat vein)
(centipede markham mycenae)
... 43M of printouts ...
(alliance archbishop balm bonnet brute centipede covariate departure deploy efferent elysee eradicate escritoire exorcism fiat filmy flatworm infra isis lindholm markham mincemeat moresby mycenae plugging smokescreen speakeasy)

```



## REXX

This REXX solution isn't limited to integers for the weights.     This isn't a brute force solution.

While optimizing the original program, it was found that sorting the names by weight could yield a vastly

improved algorithm (by an order of magnitude), so the extra code to sort the list was included, as well as

another sort to show the solutions in alphabetical order.   Support was also added to allow specification of

which "chunk" to search for solutions   (that is, out of the 31 names, take a "chunk" at a time).

Added was "que pasa" informational messages.   The sum   (which is zero for this task)   can be any number,

and can be specifiable on the command line.

```rexx
/*REXX program finds some  non─null subsets  of a  weighted list  whose  sum eqals zero.*/
parse arg  target stopAt chunkette .             /*option optional arguments from the CL*/
if target=='' | target==","  then target= 0      /*Not specified?  Then use the default.*/
if stopAt=='' | stopAt==","  then stopAt= 1      /* "      "         "   "   "     "    */
y=0
zzz= 'alliance  -624              archbishop  -915                balm        397'  ,
     'bonnet     452              brute        870                centipede  -658'  ,
     'cobol      362              covariate    590                departure   952'  ,
     'deploy      44              diophantine  645                efferent     54'  ,
     'elysee    -326              eradicate    376                escritoire  856'  ,
     'exorcism  -983              fiat         170                filmy      -874'  ,
     'flatworm   503              gestapo      915                infra      -847'  ,
     'isis      -982              lindholm     999                markham     475'  ,
     'mincemeat -880              moresby      756                mycenae     183'  ,
     'plugging  -266              smokescreen  423                speakeasy  -745'  ,
     'vein       813'
@.=0
                do N=1  until zzz=''             /*construct an array from the ZZZ list.*/
                parse var  zzz   @.N  #.N  zzz   /*pick from the list like a nose.      */
                end   /*N*/
call eSort N                                     /*sort the names with weights.         */
call tellZ  'sorted'                             /*display the sorted list.             */
chunkStart= 1                                    /*the default place to  start.         */
chunkEnd  = N                                    /* "     "      "    "   end.          */
if chunkette\==''  then do                       /*solutions just for a chunkette.      */
                        chunkStart= chunkette
                        chunkEnd  = chunkette
                        end
call time 'Reset'                                /*reset the REXX elapsed time.         */
??= 0                                            /*the number of solutions  (so far).   */
      do chunk=chunkStart  to chunkEnd           /*traipse through the items.           */
      call tello center(' doing chunk:'   chunk" ", 79, '─')
      call combN N, chunk                        /*N  items,   a  CHUNK  at a time.     */
      _= ??;            if _==0  then _= 'No'    /*Englishise for a zero count.         */
      call tello _  'solution's(??)     "found so far."
      end   /*chunk*/

if ??==0  then ??= 'no'                          /*Englishise the solutions number.     */
call tello   'Found'    ??    "subset"s(??)    'whose summed weight's(??)    "="    target
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
combN: procedure expose @. #. ?? stopAt target;       parse arg x,y;        !.= @.0
       base= x+1;        bbase= base - y         /*!.n   are the combination digits.    */
         do n=1  for y;  !.n=n                   /*construct the first combination.     */
         end   /*n*/
       ym= y-1
         do j=1;       _=!.1;         s= #._     /*obtain the first digit and the sum.  */
         if s>target  then leave                 /*Is  1st dig>target?  Then we're done.*/
           do k=2  for ym;   _= !.k;  s= s + #._ /*Σ the weights;  is sum > target ?    */
           if s>target  then do;      if .combUp(k-1)  then return;    iterate j;    end
           end   /*k*/
         if s==target  then call telly           /*have we found a pot of gold?         */
         !.y= !.y + 1;   if !.y==base  then  if .combUp(ym)  then leave    /*bump digit.*/
         end      /*j*/;               return    /*done with this combination set.      */
/*──────────────────────────────────────────────────────────────────────────────────────*/
.combUp: procedure expose !. y bbase;  parse arg d;        if d==0  then return 1
         p= !.d;   do u=d  to y;      !.u=p + 1  /*add one to digit we're pointing at.  */
                   if !.u >= bbase+u  then return .combUp(u-1)
                   p= !.u                        /*P   will be used for the next digt.  */
                   end   /*u*/;       return 0   /*go back and sum this combination.    */
/*──────────────────────────────────────────────────────────────────────────────────────*/
eSort: procedure expose #. @. $.;          parse arg N,$;              h=N
         do  while h>1;                    h= h%2
           do i=1  for  N-h;     j=i;      k= h+i
           if $==. then do while $.k<$.j;  parse value $.j $.k         with $.k $.j
                        if h>=j  then leave;     j= j-h;               k= k-h
                        end   /*while $.k<$.j*/
                   else do while #.k<#.j; parse value @.j @.k #.j #.k with @.k @.j #.k #.j
                        if h>=j  then leave;     j= j-h;               k= k-h
                        end   /*while #.k<#.j*/
           end   /*i*/
         end     /*while h>1*/;            return
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:     if arg(1)==1  then return arg(3);  return word(arg(2) 's',1)  /*simple pluralizer*/
tello: parse arg _,e;  if e==.  then say;  say _;  call lineout 'SUBSET.'y, _;      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
telly: ??= ??+1;                     nameL=      /*start with a  "null"  name list.     */
                 do gi=1  for y;     ggg= !.gi   /*build duplicate array (to be sorted).*/
                 $.gi= @.ggg                     /*transform from  index ──►  a name.   */
                 end   /*gi*/                    /*build duplicate array (to be sorted).*/
       call eSort y, .                           /*sort the names alphabetically.       */
         do gs=1  for y;   nameL= nameL $.gs     /*build a list of names whose  sum = 0 */
         end   /*gs*/                            /*the list of names could be sorted.   */
       call tello  '['y"   name"s(y)']'      space(nameL)
       if ??<stopAt | stopAt==0  then return     /*see if we reached a  (or the)  limit.*/
       call tello 'Stopped after finding '   ??   " subset"s(??)'.', .
       exit                                      /*a short─timer,  we should quit then. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tellz:             do j=1  for N                 /*show a list of names and weights.    */
                   call tello  right('['j']', 30)       right(@.j, 11)       right(#.j, 5)
                   end   /*j*/
       call tello
       call tello    'There are  '     N     " entries in the (above)"   arg(1)   'table.'
       call tello;                  return
```

Output note:   this program also writes the displayed output to file(s):   SUBSET.nnn

 ──────── where   nnn   is the ''chunk'' number.

{{out|output|text=  when using the input of:     <tt> 0   12 </tt>}}

(The above arguments set the target sum to zero, and limits the finding of a dozen solutions.)

```txt

                           [1]    exorcism  -983
                           [2]        isis  -982
                           [3]  archbishop  -915
                           [4]   mincemeat  -880
                           [5]       filmy  -874
                           [6]       infra  -847
                           [7]   speakeasy  -745
                           [8]   centipede  -658
                           [9]    alliance  -624
                          [10]      elysee  -326
                          [11]    plugging  -266
                          [12]      deploy    44
                          [13]    efferent    54
                          [14]        fiat   170
                          [15]     mycenae   183
                          [16]       cobol   362
                          [17]   eradicate   376
                          [18]        balm   397
                          [19] smokescreen   423
                          [20]      bonnet   452
                          [21]     markham   475
                          [22]    flatworm   503
                          [23]   covariate   590
                          [24] diophantine   645
                          [25]     moresby   756
                          [26]        vein   813
                          [27]  escritoire   856
                          [28]       brute   870
                          [29]     gestapo   915
                          [30]   departure   952
                          [31]    lindholm   999

There are   31  entries in the (above) sorted table.

─────────────────────────────── doing chunk: 1 ────────────────────────────────
No solutions found so far.
─────────────────────────────── doing chunk: 2 ────────────────────────────────
[2   names] archbishop gestapo
1 solution found so far.
─────────────────────────────── doing chunk: 3 ────────────────────────────────
[3   names] exorcism fiat vein
[3   names] centipede markham mycenae
3 solutions found so far.
─────────────────────────────── doing chunk: 4 ────────────────────────────────
[4   names] exorcism gestapo speakeasy vein
[4   names] deploy exorcism moresby mycenae
[4   names] bonnet elysee escritoire isis
[4   names] eradicate isis mycenae smokescreen
[4   names] balm efferent filmy smokescreen
[4   names] centipede covariate gestapo infra
[4   names] centipede covariate speakeasy vein
[4   names] brute centipede efferent plugging
[4   names] alliance balm deploy mycenae

Stopped after finding  12  subsets.

```



## Ring


```ring

# Project : Subset sum problem

knap = [["alliance", -624],
            ["archbishop", -915],
            ["balm", 397],
            ["bonnet", 452],
            ["brute", 870],
            ["centipede", -658],
            ["cobol", 362],
            ["covariate", 590],
            ["departure", 952],
            ["deploy", 44],
            ["diophantine", 645],
            ["efferent", 54],
            ["elysee", -326],
            ["eradicate", 376],
            ["escritoire", 856],
            ["exorcism", -983],
            ["fiat", 170],
            ["filmy", -874],
            ["flatworm", 503],
            ["gestapo", 915],
            ["infra", -847],
            ["isis", -982],
            ["lindholm", 999],
            ["markham", 475],
            ["mincemeat", -880],
            ["moresby", 756],
            ["mycenae", 183],
            ["plugging", -266],
            ["smokescreen", 423],
            ["speakeasy", -745],
            ["vein", 813]]

knapsack = createDimList([pow(2, len(knap)),len(knap)+2])
knapweight = createDimList([pow(2, len(knap)),len(knap)+2])
lenknap = list(pow(2, len(knap)))

powerset(knap)

func powerset(list)
        n1 = 0
        num = 0
        for i = 2 to (2 << len(list)) - 1 step 2
             n2 = 0
             n1 = n1 + 1
             weight = 0
             for j = 1 to len(list)
                  if i & (1 << j)
                     n2 = n2 + 1
                     knapsack[n1][n2] = list[j][1]
                     weight = weight + list[j][2]
                     knapweight[n1][n2] = list[j][2]
                  ok
             next
             lenknap[n1] = n2+1
             if weight = 0
             see "" + num + ": "
                for p = 1 to lenknap[n1]-1
                      see "{" + knapsack[n1][p] + " " + knapweight[n1][p]+ "}"
                next
                see nl
                num = num + 1
             ok
         next

func createDimList(dimArray)
        sizeList = len(dimArray)
        newParms = []
        for i = 2 to sizeList
            Add(newParms, dimArray[i])
        next
        alist = list(dimArray[1])
        if sizeList = 1
           return aList
        ok
        for t in alist
              t = createDimList(newParms)
        next
        return alist

```

Output:

```txt

0: {(archbishop, -915), (gestapo, 915)}
1: {(fiat, 170), (vein, 813), (isis, -982)}
2: {(alliance, -624), (departure, 952), (elysee, -326)}
3: {(alliance, -624), (archbishop, -915), (departure, 952), (covariate, 590)}
4: {(markham, 475), (infra, -847), (eradicate, 376)}
5: {(flatworm, 503), (eradicate, 376), (filmy, -874)}

```



## Ruby

a brute force solution:

```ruby
weights = {
  'alliance'   =>-624, 'archbishop'=>-915, 'balm'       => 397, 'bonnet'   => 452,
  'brute'      => 870, 'centipede' =>-658, 'cobol'      => 362, 'covariate'=> 590,
  'departure'  => 952, 'deploy'    =>  44, 'diophantine'=> 645, 'efferent' =>  54,
  'elysee'     =>-326, 'eradicate' => 376, 'escritoire' => 856, 'exorcism' =>-983,
  'fiat'       => 170, 'filmy'     =>-874, 'flatworm'   => 503, 'gestapo'  => 915,
  'infra'      =>-847, 'isis'      =>-982, 'lindholm'   => 999, 'markham'  => 475,
  'mincemeat'  =>-880, 'moresby'   => 756, 'mycenae'    => 183, 'plugging' =>-266,
  'smokescreen'=> 423, 'speakeasy' =>-745, 'vein'       => 813,
}

words = weights.keys
1.upto(words.length) do |n|
  zerosum = words.combination(n).find do |subset|
    subset.reduce(0) {|sum, word| sum + weights[word]} == 0
  end

  if zerosum
    puts "a subset of length #{n} that sums to zero: #{zerosum}"
  else
    puts "no subsets of length #{n} sum to zero"
  end
end
```


{{out}}
<pre style="height: 40ex; overflow: scroll">
no subsets of length 1 sum to zero
a subset of length 2 that sums to zero: ["archbishop", "gestapo"]
a subset of length 3 that sums to zero: ["centipede", "markham", "mycenae"]
a subset of length 4 that sums to zero: ["alliance", "balm", "deploy", "mycenae"]
a subset of length 5 that sums to zero: ["alliance", "brute", "covariate", "deploy", "mincemeat"]
a subset of length 6 that sums to zero: ["alliance", "archbishop", "balm", "deploy", "gestapo", "mycenae"]
a subset of length 7 that sums to zero: ["alliance", "archbishop", "bonnet", "cobol", "departure", "exorcism", "moresby"]
a subset of length 8 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "fiat", "flatworm", "isis", "lindholm"]
a subset of length 9 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "covariate", "eradicate", "mincemeat", "plugging"]
a subset of length 10 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "departure", "deploy", "mincemeat"]
a subset of length 11 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "departure", "infra", "moresby", "speakeasy"]
a subset of length 12 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "covariate", "diophantine", "efferent", "elysee", "infra"]
a subset of length 13 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "covariate", "departure", "efferent", "eradicate", "filmy", "isis"]
a subset of length 14 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "covariate", "departure", "deploy", "elysee", "filmy", "markham", "speakeasy"]
a subset of length 15 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "covariate", "departure", "deploy", "elysee", "exorcism", "flatworm", "infra", "mycenae"]
a subset of length 16 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "covariate", "departure", "deploy", "diophantine", "elysee", "exorcism", "filmy", "gestapo", "infra"]
a subset of length 17 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "covariate", "departure", "deploy", "diophantine", "exorcism", "isis", "mincemeat", "mycenae", "plugging", "vein"]
a subset of length 18 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "covariate", "departure", "deploy", "diophantine", "efferent", "elysee", "exorcism", "filmy", "isis", "mycenae", "vein"]
a subset of length 19 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "covariate", "departure", "deploy", "diophantine", "efferent", "elysee", "eradicate", "exorcism", "fiat", "infra", "isis", "smokescreen"]
a subset of length 20 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "covariate", "departure", "deploy", "diophantine", "efferent", "elysee", "eradicate", "exorcism", "gestapo", "infra", "isis", "smokescreen", "speakeasy"]
a subset of length 21 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "covariate", "departure", "deploy", "diophantine", "efferent", "elysee", "eradicate", "exorcism", "flatworm", "infra", "lindholm", "mincemeat", "plugging", "speakeasy"]
a subset of length 22 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "covariate", "departure", "deploy", "diophantine", "efferent", "elysee", "eradicate", "escritoire", "exorcism", "fiat", "filmy", "flatworm", "mincemeat", "plugging", "speakeasy"]
a subset of length 23 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "covariate", "departure", "deploy", "diophantine", "efferent", "elysee", "eradicate", "escritoire", "exorcism", "infra", "isis", "mincemeat", "moresby", "mycenae", "smokescreen", "speakeasy"]
a subset of length 24 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "covariate", "departure", "deploy", "diophantine", "efferent", "elysee", "exorcism", "filmy", "gestapo", "infra", "markham", "mincemeat", "moresby", "mycenae", "plugging", "smokescreen", "speakeasy"]
a subset of length 25 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "covariate", "departure", "deploy", "diophantine", "eradicate", "exorcism", "fiat", "filmy", "flatworm", "infra", "isis", "lindholm", "markham", "mincemeat", "moresby", "mycenae", "plugging", "speakeasy"]
a subset of length 26 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "cobol", "covariate", "departure", "deploy", "diophantine", "elysee", "eradicate", "escritoire", "exorcism", "fiat", "filmy", "gestapo", "infra", "isis", "markham", "mincemeat", "mycenae", "plugging", "speakeasy", "vein"]
a subset of length 27 that sums to zero: ["alliance", "archbishop", "balm", "bonnet", "brute", "centipede", "covariate", "departure", "deploy", "efferent", "elysee", "eradicate", "escritoire", "exorcism", "fiat", "filmy", "flatworm", "infra", "isis", "lindholm", "markham", "mincemeat", "moresby", "mycenae", "plugging", "smokescreen", "speakeasy"]
no subsets of length 28 sum to zero
no subsets of length 29 sum to zero
no subsets of length 30 sum to zero
no subsets of length 31 sum to zero
```



## Scala

{{Out}}Best seen running in your browser by [https://scastie.scala-lang.org/KnhJimmSTL6QXIGTAdZjBQ Scastie (remote JVM)].

```Scala
object SubsetSum extends App {
  private val LIMIT = 5
  private val n = items.length
  private val indices = new Array[Int](n)
  private var count = 0

  private def items = Seq(
    Item("alliance", -624),
    Item("archbishop", -915),
    Item("balm", 397),
    Item("bonnet", 452),
    Item("brute", 870),
    Item("centipede", -658),
    Item("cobol", 362),
    Item("covariate", 590),
    Item("departure", 952),
    Item("deploy", 44),
    Item("diophantine", 645),
    Item("efferent", 54),
    Item("elysee", -326),
    Item("eradicate", 376),
    Item("escritoire", 856),
    Item("exorcism", -983),
    Item("fiat", 170),
    Item("filmy", -874),
    Item("flatworm", 503),
    Item("gestapo", 915),
    Item("infra", -847),
    Item("isis", -982),
    Item("lindholm", 999),
    Item("markham", 475),
    Item("mincemeat", -880),
    Item("moresby", 756),
    Item("mycenae", 183),
    Item("plugging", -266),
    Item("smokescreen", 423),
    Item("speakeasy", -745),
    Item("vein", 813)
  )

  private def zeroSum(i: Int, w: Int): Unit = {
    if (count < LIMIT) {
      if (i != 0 && w == 0) {
        for (j <- 0 until i) print(f"${items(indices(j))}%s ")
        println
        count += 1
      } else
        for (j <- (if (i != 0) indices(i - 1) + 1 else 0) until n) {
          indices(i) = j
          zeroSum(i + 1, w + items(j).weight)
        }
    }
  }

  // Not optimized
  private case class Item(word: String, weight: Int) {
    override def toString: String = f"($word%s, $weight%d)"
  }

  println(f"The weights of the following $LIMIT%d subsets add up to zero:")
  zeroSum(0, 0)

}
```


## Sidef


```ruby
var pairs = Hash(
    alliance    => -624, archbishop => -915,
    brute       =>  870, centipede  => -658,
    departure   =>  952, deploy     =>   44,
    elysee      => -326, eradicate  =>  376,
    fiat        =>  170, filmy      => -874,
    infra       => -847, isis       => -982,
    mincemeat   => -880, moresby    =>  756,
    smokescreen =>  423, speakeasy  => -745,
    balm        =>  397, bonnet     =>  452,
    cobol       =>  362, covariate  =>  590,
    diophantine =>  645, efferent   =>   54,
    escritoire  =>  856, exorcism   => -983,
    flatworm    =>  503, gestapo    =>  915,
    lindholm    =>  999, markham    =>  475,
    mycenae     =>  183, plugging   => -266,
    vein        =>  813,
)

var weights = pairs.keys.sort.map{|k| pairs{k} }
var inverse = pairs.flip

for n in (1 .. weights.end) {
    var found = false
    weights.combinations(n, {|*comb|
        if (comb.sum == 0) {
            say "Length #{n}: "+" ".join(inverse{comb...})
            found = true
            break
        }
    })
    found || say "Length #{n}: (none)"
}
```

{{out}}
<pre style="height: 40ex; overflow: scroll">
Length 1: (none)
Length 2: archbishop gestapo
Length 3: centipede markham mycenae
Length 4: alliance balm deploy mycenae
Length 5: alliance brute covariate deploy mincemeat
Length 6: alliance archbishop balm deploy gestapo mycenae
Length 7: alliance archbishop bonnet cobol departure exorcism moresby
Length 8: alliance archbishop balm bonnet fiat flatworm isis lindholm
Length 9: alliance archbishop balm bonnet brute covariate eradicate mincemeat plugging
Length 10: alliance archbishop balm bonnet brute centipede cobol departure deploy mincemeat
Length 11: alliance archbishop balm bonnet brute centipede cobol departure infra moresby speakeasy
Length 12: alliance archbishop balm bonnet brute centipede cobol covariate diophantine efferent elysee infra
Length 13: alliance archbishop balm bonnet brute centipede cobol covariate departure efferent eradicate filmy isis
Length 14: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy elysee filmy markham speakeasy
Length 15: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy elysee exorcism flatworm infra mycenae
Length 16: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine elysee exorcism filmy gestapo infra
Length 17: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine exorcism isis mincemeat mycenae plugging vein
Length 18: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee exorcism filmy isis mycenae vein
Length 19: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate exorcism fiat infra isis smokescreen
Length 20: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate exorcism gestapo infra isis smokescreen speakeasy
Length 21: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate exorcism flatworm infra lindholm mincemeat plugging speakeasy
Length 22: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate escritoire exorcism fiat filmy flatworm mincemeat plugging speakeasy
Length 23: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee eradicate escritoire exorcism infra isis mincemeat moresby mycenae smokescreen speakeasy
Length 24: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine efferent elysee exorcism filmy gestapo infra markham mincemeat moresby mycenae plugging smokescreen speakeasy
Length 25: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine eradicate exorcism fiat filmy flatworm infra isis lindholm markham mincemeat moresby mycenae plugging speakeasy
Length 26: alliance archbishop balm bonnet brute centipede cobol covariate departure deploy diophantine elysee eradicate escritoire exorcism fiat filmy gestapo infra isis markham mincemeat mycenae plugging speakeasy vein
Length 27: alliance archbishop balm bonnet brute centipede covariate departure deploy efferent elysee eradicate escritoire exorcism fiat filmy flatworm infra isis lindholm markham mincemeat moresby mycenae plugging smokescreen speakeasy
Length 28: (none)
Length 29: (none)
Length 30: (none)
```



## Tcl

As it turns out that the problem space has small subsets that sum to zero, it is more efficient to enumerate subsets in order of their size rather than doing a simple combination search. This is not true of all possible input data sets though; the problem is known to be NP-complete after all.

```tcl
proc subsetsOfSize {set size} {
    if {$size <= 0} {
	return
    } elseif {$size == 1} {
	foreach elem $set {lappend result [list $elem]}
    } else {
	incr size [set i -1]
	foreach elem $set {
	    foreach sub [subsetsOfSize [lreplace $set [incr i] $i] $size] {
		lappend result [lappend sub $elem]
	    }
	}
    }
    return $result
}
proc searchForSubset {wordweights {minsize 1}} {
    set words [dict keys $wordweights]
    for {set i $minsize} {$i < [llength $words]} {incr i} {
	foreach subset [subsetsOfSize $words $i] {
	    set w 0
	    foreach elem $subset {incr w [dict get $wordweights $elem]}
	    if {!$w} {return $subset}
	}
    }
    # Nothing was found
    return -code error "no subset sums to zero"
}
```

Demonstrating:

```tcl
set wordweights {
    alliance	 -624
    archbishop	 -915
    balm	 397
    bonnet	 452
    brute	 870
    centipede	 -658
    cobol	 362
    covariate	 590
    departure	 952
    deploy	 44
    diophantine	 645
    efferent	 54
    elysee	 -326
    eradicate	 376
    escritoire	 856
    exorcism	 -983
    fiat	 170
    filmy	 -874
    flatworm	 503
    gestapo	 915
    infra	 -847
    isis	 -982
    lindholm	 999
    markham	 475
    mincemeat	 -880
    moresby	 756
    mycenae	 183
    plugging	 -266
    smokescreen	 423
    speakeasy	 -745
    vein	 813
}
set zsss [searchForSubset $wordweights]
puts "Found zero-summing subset: [join [lsort $zsss] {, }]"
```

{{Out}}

```txt

Found zero-summing subset: archbishop, gestapo

```



## Ursala

This solution scans the set sequentially while maintaining a record of all distinct sums obtainable by words encountered thus far, and stops when a zero sum is found.

```Ursala
#import std
#import int

weights =

{
   'alliance': -624,
   'archbishop': -915,
   'balm': 397,
   'bonnet': 452,
   'brute': 870,
   'centipede': -658,
   'cobol': 362,
   'covariate': 590,
   'departure': 952,
   'deploy': 44,
   'diophantine': 645,
   'efferent': 54,
   'elysee': -326,
   'eradicate': 376,
   'escritoire': 856,
   'exorcism': -983,
   'fiat': 170,
   'filmy': -874,
   'flatworm': 503,
   'gestapo': 915,
   'infra': -847,
   'isis': -982,
   'lindholm': 999,
   'markham': 475,
   'mincemeat': -880,
   'moresby': 756,
   'mycenae': 183,
   'plugging': -266,
   'smokescreen': 423,
   'speakeasy': -745,
   'vein': 813}

nullset = ~&nZFihmPB+ =><> ~&ng?r\~&r ^TnK2hS\~&r ^C/~&lmPlNCX *D ^A/sum@lmPrnPX ~&lrmPC

#cast %zm

main = nullset weights
```

The name of the function that takes the weighted set is <code>nullset</code>. It manipulates a partial result represented as a list of pairs, each containing a subset of weighted words and the sum of their weights. Here is a rough translation:
* <code>=><></code>  fold right combinator with the empty list as the vacuuous case
* <code>~&ng?r\~&r</code>  If the partial result contains a zero sum, return it.
* <code>^TnK2hS\~&r</code>  Concatenate the partial result with the new list of subsets (computed as follows) and delete duplicate sums.
* <code>^C/~&lmPlNCX</code>  Cons a singleton subset containing the next word to the partial results.
* <code>*D</code>  Distribute the next word in the set to the partial results and do the following to each.
* <code>sum@lmPrnPX</code>  Add the weight of the new word to the existing sum.
* <code>~&lrmPC</code>  Cons the new word to the list of existing ones.
* <code>~&nZFihmPB+</code>  To conclude, search for a result with a zero sum, if any, and return its associated subset of weighted words.
{{Out}}

```txt

<
   'flatworm': 503,
   'gestapo': 915,
   'infra': -847,
   'isis': -982,
   'lindholm': 999,
   'plugging': -266,
   'smokescreen': 423,
   'speakeasy': -745>

```



## zkl

{{trans|C}}

```zkl
var items=T(
T("alliance",  -624),  T("archbishop",  -915),  T("balm",        397),
T("bonnet",     452),  T("brute",        870),  T("centipede",  -658),
T("cobol",      362),  T("covariate",    590),  T("departure",   952),
T("deploy",      44),  T("diophantine",  645),  T("efferent",     54),
T("elysee",    -326),  T("eradicate",    376),  T("escritoire",  856),
T("exorcism",  -983),  T("fiat",         170),  T("filmy",      -874),
T("flatworm",   503),  T("gestapo",      915),  T("infra",      -847),
T("isis",      -982),  T("lindholm",     999),  T("markham",     475),
T("mincemeat", -880),  T("moresby",      756),  T("mycenae",     183),
T("plugging",  -266),  T("smokescreen",  423),  T("speakeasy",  -745),
T("vein",       813));

fcn subSum(set,i,weight){
   if(i and not weight){
      itms:=i.pump(List,'wrap(n){ items[set[n]][0] });
      println(itms.len(),": ",itms.concat(","));
      throw(Exception.TheEnd);
   }
   foreach j in ([i and set[i-1] + 1 or 0 .. items.len()-1]){
      set[i]=j;
      self.fcn(set, i+1, weight + items[j][1]);
   }
}

set:=List.createLong(items.len(),0);
try{ subSum(set,0,0); }catch(TheEnd){}
```

{{out}}

```txt

22: alliance,archbishop,balm,bonnet,brute,centipede,cobol,covariate,departure,deploy,diophantine,efferent,elysee,eradicate,escritoire,exorcism,fiat,filmy,flatworm,mincemeat,plugging,speakeasy

```

