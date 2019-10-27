+++
title = "Kolakoski sequence"
description = ""
date = 2019-07-04T20:57:56Z
aliases = []
[extra]
id = 21846
[taxonomies]
categories = []
tags = []
+++

{{task}}
The [[wp:Kolakoski sequence|Kolakoski sequence]] is an infinite sequence of [[wp:Natural number|natural numbers]], (excluding zero); with the property that:
: ''if you form a new sequence from the counts of runs of the same number in the first sequence, this new sequence is the same as the first sequence''.

;Example:
This is ''not'' a Kolakoski sequence:

```txt
1,1,2,2,2,1,2,2,1,2,...
```

Its sequence of run counts, (sometimes called a run length encoding, (RLE); but a true RLE also gives the character that each run encodes), is calculated like this:

: Starting from the leftmost number of the sequence we have <code>2</code> ones, followed by <code>3</code> twos, then <code>1</code> ones, <code>2</code> twos, <code>1</code> one, ...

The above gives the RLE of:

```txt
2, 3, 1, 2, 1, ...
```


The original sequence is different from its RLE in this case. '''It would be the same for a true Kolakoski sequence'''.

;Creating a Kolakoski sequence:

Lets start with the two numbers <code>(1, 2)</code> that we will cycle through; i.e. they will be used in this order:
 <code>1,2,1,2,1,2,....</code>

# We start the sequence <code>s</code> with the first item from the cycle <code>c</code>:
 <code>1</code>
# An index, <code>k</code>, into the, (expanding), sequence will step, or index through each item of the sequence <code>s</code> from the first, at its own rate.
 
We will arrange that the <code>k</code>'th item of <code>s</code> states how many ''times'' the ''last'' item of <code>s</code>should appear at the end of <code>s</code>.

We started <code>s</code> with <code>1</code> and therefore <code>s[k]</code> states that it should appear only the <code>1</code> time.


<ol start="3">
<li><p>Increment <code>k</code></p></li>
<li><p>Get the next item from <code>c</code> and append it to the end of sequence <code>s</code>. <code>s</code> will then become:
 <code>1, 2</code></p></li>
<li><p><code>k</code> was moved to the second item in the list and <code>s[k]</code> states that it should appear two times, so append another of the last item to the sequence <code>s</code>:
 <code>1, 2,2</code></p></li>
<li><p>Increment <code>k</code></p></li>
<li><p>Append the next item from the cycle to the list:
 <code>1, 2,2, 1</code></p></li>
<li><p><code>k</code> is now at the third item in the list that states that the last item should appear twice so add another copy of the last item to the sequence <code>s</code>:
 <code>1, 2,2, 1,1</code></p></li>
<li><p>increment k</p></li></ol>

...

'''Note''' that the RLE of <code>1, 2, 2, 1, 1, ...</code> begins <code>1, 2, 2</code> which is the beginning of the original sequence. The generation algorithm ensures that this will always be the case.

;Task:
# Create a routine/proceedure/function/... that given an initial ordered list/array/tuple etc of the natural numbers <code>(1, 2)</code>, returns the next number from the list when accessed in a cycle.
# Create another routine that when given the initial ordered list <code>(1, 2)</code> and the minimum length of the sequence  to generate; uses the first routine and the algorithm above, to generate at least the requested first members of the kolakoski sequence.
# Create a routine that when given a sequence, creates the run length encoding of that sequence (as defined above) and returns the result of checking if sequence starts with the exact members of its RLE. (But ''note'', due to sampling, do not compare the last member of the RLE).
# Show, on this page, (compactly), the first 20 members of the sequence generated from <code>(1, 2)</code>
# Check the sequence againt its RLE.
# Show, on this page, the first 20 members of the sequence generated from <code>(2, 1)</code>
# Check the sequence againt its RLE.
# Show, on this page, the first 30 members of the Kolakoski sequence generated from <code>(1, 3, 1, 2)</code>
# Check the sequence againt its RLE.
# Show, on this page, the first 30 members of the Kolakoski sequence generated from <code>(1, 3, 2, 1)</code>
# Check the sequence againt its RLE.
(There are [[wp:Kolakoski_sequence#From_finite_integer_sets|rules]] on generating Kolakoski sequences from this method that are broken by the last example)

## C

{{trans|Kotlin}}

```c>#include <stdio.h

#include <stdlib.h>

#define TRUE 1
#define FALSE 0

typedef int bool;

int next_in_cycle(int *c, int len, int index) {
    return c[index % len];
}

void kolakoski(int *c, int *s, int clen, int slen) {
    int i = 0, j, k = 0;
    while (TRUE) {
        s[i] = next_in_cycle(c, clen, k);
        if (s[k] > 1) {
            for (j = 1; j < s[k]; ++j) {
                if (++i == slen) return;
                s[i] = s[i - 1];
            }
        }
        if (++i == slen) return;
        k++;
    }
}

bool possible_kolakoski(int *s, int len) {
    int i, j = 0, prev = s[0], count = 1;
    int *rle = calloc(len, sizeof(int));
    bool result = TRUE;
    for (i = 1; i < len; ++i) {
        if (s[i] == prev) {
            count++;
        }
        else {
            rle[j++] = count;
            count = 1;
            prev = s[i];
        }
    }
    /* no point adding final 'count' to rle as we're not going to compare it anyway */
    for (i = 0; i < j; i++) {
        if (rle[i] != s[i]) {
           result = FALSE;
           break;
        }
    }
    free(rle);
    return result;
}

void print_array(int *a, int len) {
    int i;
    printf("[");
    for (i = 0; i < len; ++i) {
       printf("%d", a[i]);
       if (i < len - 1) printf(", ");
    }
    printf("]");
}

int main() {
    int i, clen, slen, *s;
    int c0[2] = {1, 2};
    int c1[2] = {2, 1};
    int c2[4] = {1, 3, 1, 2};
    int c3[4] = {1, 3, 2, 1};
    int *cs[4] = {c0, c1, c2, c3};
    bool p;
    int clens[4] = {2, 2, 4, 4};
    int slens[4] = {20, 20, 30, 30};
    for (i = 0; i < 4; ++i) {
        clen = clens[i];
        slen = slens[i];
        s = calloc(slen, sizeof(int));
        kolakoski(cs[i], s, clen, slen);
        printf("First %d members of the sequence generated by ", slen);
        print_array(cs[i], clen);
        printf(":\n");
        print_array(s, slen);
        printf("\n");
        p = possible_kolakoski(s, slen);
        printf("Possible Kolakoski sequence? %s\n\n", p ? "True" : "False");
        free(s); 
    }
    return 0;
}
```


{{output}}

```txt

First 20 members of the sequence generated by [1, 2]:
[1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1]
Possible Kolakoski sequence? True

First 20 members of the sequence generated by [2, 1]:
[2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2]
Possible Kolakoski sequence? True

First 30 members of the sequence generated by [1, 3, 1, 2]:
[1, 3, 3, 3, 1, 1, 1, 2, 2, 2, 1, 3, 1, 2, 2, 1, 1, 3, 3, 1, 2, 2, 2, 1, 3, 3, 1, 1, 2, 1]
Possible Kolakoski sequence? True

First 30 members of the sequence generated by [1, 3, 2, 1]:
[1, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 3, 3, 2, 2, 1, 1, 3, 2, 1, 1, 1, 1, 3, 3, 3, 2, 2, 1]
Possible Kolakoski sequence? False

```



## C++


```cpp>#include <iostream

#include <vector>

using Sequence = std::vector<int>;

std::ostream& operator<<(std::ostream& os, const Sequence& v) {
  os << "[ ";
  for (const auto& e : v) {
    std::cout << e << ", ";
  }
  os << "]";
  return os;
}

int next_in_cycle(const Sequence& s, size_t i) {
  return s[i % s.size()];
}

Sequence gen_kolakoski(const Sequence& s, int n) {
  Sequence seq;
  for (size_t i = 0; seq.size() < n; ++i) {
    const int next = next_in_cycle(s, i);
    Sequence nv(i >= seq.size() ? next : seq[i], next);
    seq.insert(std::end(seq), std::begin(nv), std::end(nv));
  }
  return { std::begin(seq), std::begin(seq) + n };
}

bool is_possible_kolakoski(const Sequence& s) {
  Sequence r;
  for (size_t i = 0; i < s.size();) {
    int count = 1;
    for (size_t j = i + 1; j < s.size(); ++j) {
      if (s[j] != s[i]) break;
      ++count;
    }
    r.push_back(count);
    i += count;
  }
  for (size_t i = 0; i < r.size(); ++i) if (r[i] != s[i]) return false;
  return true;
}

int main() {
  std::vector<Sequence> seqs = {
    { 1, 2 },
    { 2, 1 },
    { 1, 3, 1, 2 },
    { 1, 3, 2, 1 }
  };
  for (const auto& s : seqs) {
    auto kol = gen_kolakoski(s, s.size() > 2 ? 30 : 20);
    std::cout << "Starting with: " << s << ": " << std::endl << "Kolakoski sequence: " 
      << kol << std::endl << "Possibly kolakoski? " << is_possible_kolakoski(kol) << std::endl;		
  }
  return 0;
}
```

{{out}}

```txt
Starting with: [ 1, 2, ]: 
Kolakoski sequence: [ 1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1, ]
Possibly kolakoski? 1
Starting with: [ 2, 1, ]: 
Kolakoski sequence: [ 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2, ]
Possibly kolakoski? 1
Starting with: [ 1, 3, 1, 2, ]: 
Kolakoski sequence: [ 1, 3, 3, 3, 1, 1, 1, 2, 2, 2, 1, 3, 1, 2, 2, 1, 1, 3, 3, 1, 2, 2, 2, 1, 3, 3, 1, 1, 2, 1, ]
Possibly kolakoski? 1
Starting with: [ 1, 3, 2, 1, ]: 
Kolakoski sequence: [ 1, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 3, 3, 2, 2, 1, 1, 3, 2, 1, 1, 1, 1, 3, 3, 3, 2, 2, 1, ]
Possibly kolakoski? 0
```


=={{header|C#|C sharp}}==
{{trans|Java}}

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace KolakoskiSequence {
    class Crutch {
        public readonly int len;
        public int[] s;
        public int i;

        public Crutch(int len) {
            this.len = len;
            s = new int[len];
            i = 0;
        }

        public void Repeat(int count) {
            for (int j = 0; j < count; j++) {
                if (++i == len) return;
                s[i] = s[i - 1];
            }
        }
    }

    static class Extension {
        public static int NextInCycle(this int[] self, int index) {
            return self[index % self.Length];
        }

        public static int[] Kolakoski(this int[] self, int len) {
            Crutch c = new Crutch(len);

            int k = 0;
            while (c.i < len) {
                c.s[c.i] = self.NextInCycle(k);
                if (c.s[k] > 1) {
                    c.Repeat(c.s[k] - 1);
                }
                if (++c.i == len) return c.s;
                k++;
            }
            return c.s;
        }

        public static bool PossibleKolakoski(this int[] self) {
            int[] rle = new int[self.Length];
            int prev = self[0];
            int count = 1;
            int pos = 0;
            for (int i = 1; i < self.Length; i++) {
                if (self[i] == prev) {
                    count++;
                }
                else {
                    rle[pos++] = count;
                    count = 1;
                    prev = self[i];
                }
            }
            // no point adding final 'count' to rle as we're not going to compare it anyway
            for (int i = 0; i < pos; i++) {
                if (rle[i] != self[i]) {
                    return false;
                }
            }
            return true;
        }

        public static string AsString(this int[] self) {
            StringBuilder sb = new StringBuilder("[");
            int count = 0;
            foreach (var item in self) {
                if (count > 0) {
                    sb.Append(", ");
                }
                sb.Append(item);
                count++;
            }
            return sb.Append("]").ToString();
        }
    }

    class Program {
        static void Main(string[] args) {
            int[][] ias = {
                new int[]{1, 2},
                new int[]{2, 1},
                new int[]{1, 3, 1, 2},
                new int[]{1, 3, 2, 1}
            };
            int[] lens = { 20, 20, 30, 30 };

            for (int i = 0; i < ias.Length; i++) {
                int len = lens[i];
                int[] kol = ias[i].Kolakoski(len);

                Console.WriteLine("First {0} members of the sequence by {1}: ", len, ias[i].AsString());
                Console.WriteLine(kol.AsString());
                Console.WriteLine("Possible Kolakoski sequence? {0}", kol.PossibleKolakoski());
                Console.WriteLine();
            }
        }
    }
}
```



## D

{{trans|Kotlin}}

```d
import std.stdio;

void repeat(int count, void delegate(int) action) {
    for (int i=0; i<count; i++) {
        action(i);
    }
}

T nextInCycle(T)(T[] self, int index) {
    return self[index % self.length];
}

T[] kolakoski(T)(T[] self, int len) {
    T[] s;
    s.length = len;
    int i;
    int k;
    while (i<len) {
        s[i] = self.nextInCycle(k);
        if (s[k] > 1) {
            repeat(s[k] - 1,
                (int j) {
                    if (++i == len) return;
                    s[i] = s[i-1];
                }
            );
        }
        if (++i == len) return s;
        k++;
    }
    return s;
}

bool possibleKolakoski(T)(T[] self) {
    auto len = self.length;
    T[] rle;
    auto prev = self[0];
    int count = 1;
    foreach (i; 1..len) {
        if (self[i] == prev) {
            count++;
        } else {
            rle ~= count;
            count = 1;
            prev = self[i];
        }
    }
    // no point adding final 'count' to rle as we're not going to compare it anyway
    foreach (i; 0..rle.length) {
        if (rle[i] != self[i]) {
            return false;
        }
    }
    return true;
}

void main() {
    auto ias = [[1,2],[2,1],[1,3,1,2],[1,3,2,1]];
    auto lens = [20,20,30,30];

    foreach (i,ia; ias) {
        auto len = lens[i];
        auto kol = ia.kolakoski(len);
        writeln("First ", len, " members of the sequence generated by ", ia, ":");
        writeln(kol);
        write("Possible Kolakoski sequence? ");
        if (kol.possibleKolakoski) {
            writeln("Yes");
        } else {
            writeln("no");
        }
        writeln;
    }
}
```

{{out}}

```txt
First 20 members of the sequence generated by [1, 2]:
[1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1]
Possible Kolakoski sequence? Yes

First 20 members of the sequence generated by [2, 1]:
[2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2]
Possible Kolakoski sequence? Yes

First 30 members of the sequence generated by [1, 3, 1, 2]:
[1, 3, 3, 3, 1, 1, 1, 2, 2, 2, 1, 3, 1, 2, 2, 1, 1, 3, 3, 1, 2, 2, 2, 1, 3, 3, 1, 1, 2, 1]
Possible Kolakoski sequence? Yes

First 30 members of the sequence generated by [1, 3, 2, 1]:
[1, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 3, 3, 2, 2, 1, 1, 3, 2, 1, 1, 1, 1, 3, 3, 3, 2, 2, 1]
Possible Kolakoski sequence? no
```



## Go

{{trans|Kotlin}}

```go
package main

import "fmt"

func nextInCycle(c []int, index int) int {
    return c[index % len(c)]
}

func kolakoski(c []int, slen int) []int {
    s := make([]int, slen)
    i, k := 0, 0
    for {
        s[i] = nextInCycle(c, k)
        if s[k] > 1 {
            for j := 1; j < s[k]; j++ {
                i++
                if i == slen {
                    return s
                }
                s[i] = s[i - 1]
            }
        }
        i++
        if i == slen {
            return s
        }
        k++
    }
}

func possibleKolakoski(s []int) bool {
    slen := len(s)
    rle := make([]int, 0, slen)
    prev := s[0]
    count := 1
    for i := 1; i < slen; i++ {
        if s[i] == prev {
            count++
        } else {
            rle = append(rle, count)
            count = 1
            prev = s[i]
        }
    }
    // no point adding final 'count' to rle as we're not going to compare it anyway
    for i := 0; i < len(rle); i++ {
        if rle[i] != s[i] {
            return false
        }
    }
    return true
}

func printInts(ia []int, suffix string) {
    fmt.Print("[")
    alen := len(ia)
    for i := 0; i < alen; i++ {
        fmt.Print(ia[i])
        if i < alen - 1 {
            fmt.Print(", ")
        }
    }
    fmt.Printf("]%s\n", suffix)
}

func main() {
    ias := make([][]int, 4)
    ias[0] = []int{1, 2}
    ias[1] = []int{2, 1}
    ias[2] = []int{1, 3, 1, 2}
    ias[3] = []int{1, 3, 2, 1}
    slens := []int{20, 20, 30, 30}
    for i, ia := range ias {
        slen := slens[i]
        kol := kolakoski(ia, slen)
        fmt.Printf("First %d members of the sequence generated by ", slen)
        printInts(ia, ":")
        printInts(kol, "")
        p := possibleKolakoski(kol)
        poss := "Yes"
        if !p {
            poss = "No"
        }
        fmt.Println("Possible Kolakoski sequence?", poss, "\n")
    }
}
```


{{out}}

```txt

First 20 members of the sequence generated by [1, 2]:
[1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1]
Possible Kolakoski sequence? Yes 

First 20 members of the sequence generated by [2, 1]:
[2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2]
Possible Kolakoski sequence? Yes 

First 30 members of the sequence generated by [1, 3, 1, 2]:
[1, 3, 3, 3, 1, 1, 1, 2, 2, 2, 1, 3, 1, 2, 2, 1, 1, 3, 3, 1, 2, 2, 2, 1, 3, 3, 1, 1, 2, 1]
Possible Kolakoski sequence? Yes 

First 30 members of the sequence generated by [1, 3, 2, 1]:
[1, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 3, 3, 2, 2, 1, 1, 3, 2, 1, 1, 1, 1, 3, 3, 3, 2, 2, 1]
Possible Kolakoski sequence? No 

```



## Haskell


```haskell
import Data.List (group)
import Control.Monad (forM_)

replicateAtLeastOne :: Int -> a -> [a]
replicateAtLeastOne n x = x : replicate (n-1) x

zipWithLazy :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithLazy f ~(x:xs) ~(y:ys) = f x y : zipWithLazy f xs ys

kolakoski :: [Int] -> [Int]
kolakoski items = s
  where s = concat $ zipWithLazy replicateAtLeastOne s $ cycle items

rle :: Eq a => [a] -> [Int]
rle = map length . group

sameAsRleUpTo :: Int -> [Int] -> Bool
sameAsRleUpTo n s = r == take (length r) prefix
  where prefix = take n s
        r = init $ rle prefix

main :: IO ()
main = forM_ [([1, 2], 20),
              ([2, 1], 20), 
              ([1, 3, 1, 2], 30),
              ([1, 3, 2, 1], 30)]
        $ \(items, n) -> do
          putStrLn $ "First " ++ show n ++ " members of the sequence generated by " ++ show items ++ ":"
          let s = kolakoski items
          print $ take n s
          putStrLn $ "Possible Kolakoski sequence? " ++ show (sameAsRleUpTo n s)
          putStrLn ""
```


{{output}}

```txt

First 20 members of the sequence generated by [1,2]:
[1,2,2,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2,2,1]
Possible Kolakoski sequence? True

First 20 members of the sequence generated by [2,1]:
[2,2,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2,2,1,2]
Possible Kolakoski sequence? True

First 30 members of the sequence generated by [1,3,1,2]:
[1,3,3,3,1,1,1,2,2,2,1,3,1,2,2,1,1,3,3,1,2,2,2,1,3,3,1,1,2,1]
Possible Kolakoski sequence? True

First 30 members of the sequence generated by [1,3,2,1]:
[1,3,3,3,2,2,2,1,1,1,1,1,3,3,2,2,1,1,3,2,1,1,1,1,3,3,3,2,2,1]
Possible Kolakoski sequence? False


```



## Java

{{trans|Kotlin}}

```java
import java.util.Arrays;

public class Kolakoski {
    private static class Crutch {
        final int len;
        int[] s;
        int i;

        Crutch(int len) {
            this.len = len;
            s = new int[len];
            i = 0;
        }

        void repeat(int count) {
            for (int j = 0; j < count; j++) {
                if (++i == len) return;
                s[i] = s[i - 1];
            }
        }
    }

    private static int nextInCycle(final int[] self, int index) {
        return self[index % self.length];
    }

    private static int[] kolakoski(final int[] self, int len) {
        Crutch c = new Crutch(len);

        int k = 0;
        while (c.i < len) {
            c.s[c.i] = nextInCycle(self, k);
            if (c.s[k] > 1) {
                c.repeat(c.s[k] - 1);
            }
            if (++c.i == len) return c.s;
            k++;
        }
        return c.s;
    }

    private static boolean possibleKolakoski(final int[] self) {
        int[] rle = new int[self.length];
        int prev = self[0];
        int count = 1;
        int pos = 0;
        for (int i = 1; i < self.length; i++) {
            if (self[i] == prev) {
                count++;
            } else {
                rle[pos++] = count;
                count = 1;
                prev = self[i];
            }
        }
        // no point adding final 'count' to rle as we're not going to compare it anyway
        for (int i = 0; i < pos; i++) {
            if (rle[i] != self[i]) {
                return false;
            }
        }
        return true;
    }

    public static void main(String[] args) {
        int[][] ias = new int[][]{
            new int[]{1, 2},
            new int[]{2, 1},
            new int[]{1, 3, 1, 2},
            new int[]{1, 3, 2, 1}
        };
        int[] lens = new int[]{20, 20, 30, 30};

        for (int i=0; i<ias.length; i++) {
            int len = lens[i];
            int[] kol = kolakoski(ias[i], len);

            System.out.printf("First %d members of the sequence generated by %s: \n", len, Arrays.toString(ias[i]));
            System.out.printf("%s\n", Arrays.toString(kol));
            System.out.printf("Possible Kolakoski sequence? %s\n\n", possibleKolakoski(kol));
        }
    }
}
```

{{out}}

```txt
First 20 members of the sequence generated by [1, 2]: 
[1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1]
Possible Kolakoski sequence? true

First 20 members of the sequence generated by [2, 1]: 
[2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2]
Possible Kolakoski sequence? true

First 30 members of the sequence generated by [1, 3, 1, 2]: 
[1, 3, 3, 3, 1, 1, 1, 2, 2, 2, 1, 3, 1, 2, 2, 1, 1, 3, 3, 1, 2, 2, 2, 1, 3, 3, 1, 1, 2, 1]
Possible Kolakoski sequence? true

First 30 members of the sequence generated by [1, 3, 2, 1]: 
[1, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 3, 3, 2, 2, 1, 1, 3, 2, 1, 1, 1, 1, 3, 3, 3, 2, 2, 1]
Possible Kolakoski sequence? false
```



## Julia

{{trans|C}}

```julia
function kolakoski(vec, len)
    seq = Vector{Int}()
    k = 0
    denom = length(vec)
    while length(seq) < len
        n = vec[k % denom + 1]
        k += 1
        seq = vcat(seq, repeat([n], k > length(seq) ? n : seq[k]))
    end
    seq[1:len]
end

function iskolakoski(seq)
    count = 1
    rle = Vector{Int}()
    for i in 2:length(seq)
        if seq[i] == seq[i - 1]
            count += 1
        else
            push!(rle, count)
            count = 1
        end
    end
    rle == seq[1:length(rle)]
end

const tests = [[[1, 2], 20],[[2, 1] ,20], [[1, 3, 1, 2], 30], [[1, 3, 2, 1], 30]]

for t in tests
    vec, n = t[1], t[2]
    seq = kolakoski(vec, n)
    println("Kolakoski from $(vec): first $n numbers are $seq.")
    println("\t\tDoes this look like a Kolakoski sequence? ", iskolakoski(seq) ? "Yes" : "No")
end

```
 {{output}} 
```txt

 Kolakoski from [1, 2]: first 20 numbers are [1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1].
                Does this look like a Kolakoski sequence? Yes
 Kolakoski from [2, 1]: first 20 numbers are [2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2].
                Does this look like a Kolakoski sequence? Yes
 Kolakoski from [1, 3, 1, 2]: first 30 numbers are [1, 3, 3, 3, 1, 1, 1, 2, 2, 2, 1, 3, 1, 2, 2, 1, 1, 3, 3, 1, 2, 2, 2, 1, 3, 3, 1, 1, 2, 1].
                Does this look like a Kolakoski sequence? Yes
 Kolakoski from [1, 3, 2, 1]: first 30 numbers are [1, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 3, 3, 2, 2, 1, 1, 3, 2, 1, 1, 1, 1, 3, 3, 3, 2, 2, 1].
                Does this look like a Kolakoski sequence? No

```



## Kotlin


```scala
// Version 1.2.41

fun IntArray.nextInCycle(index: Int) = this[index % this.size]

fun IntArray.kolakoski(len: Int): IntArray {
    val s = IntArray(len)
    var i = 0
    var k = 0
    while (true) {
        s[i] = this.nextInCycle(k)
        if (s[k] > 1) {
            repeat(s[k] - 1) {
                if (++i == len) return s
                s[i] = s[i - 1]
            }
        }
        if (++i == len) return s
        k++
    }
}

fun IntArray.possibleKolakoski(): Boolean {
    val len = this.size
    val rle = mutableListOf<Int>()
    var prev = this[0]
    var count = 1
    for (i in 1 until len) {
        if (this[i] == prev) {
            count++
        }
        else {
            rle.add(count)
            count = 1
            prev = this[i]
        }      
    }
    // no point adding final 'count' to rle as we're not going to compare it anyway
    for (i in 0 until rle.size) {
        if (rle[i] != this[i]) return false
    }
    return true
}

fun main(args: Array<String>) {
    val ias = listOf(
        intArrayOf(1, 2), intArrayOf(2, 1),
        intArrayOf(1, 3, 1, 2), intArrayOf(1, 3, 2, 1)
    )
    val lens = intArrayOf(20, 20, 30, 30)
    for ((i, ia) in ias.withIndex()) {
        val len = lens[i]
        val kol = ia.kolakoski(len)
        println("First $len members of the sequence generated by ${ia.asList()}:")
        println(kol.asList())
        val p = kol.possibleKolakoski()
        println("Possible Kolakoski sequence? ${if (p) "Yes" else "No"}\n")
    }
}
```


{{output}}

```txt

First 20 members of the sequence generated by [1, 2]:
[1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1]
Possible Kolakoski sequence? Yes

First 20 members of the sequence generated by [2, 1]:
[2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2]
Possible Kolakoski sequence? Yes

First 30 members of the sequence generated by [1, 3, 1, 2]:
[1, 3, 3, 3, 1, 1, 1, 2, 2, 2, 1, 3, 1, 2, 2, 1, 1, 3, 3, 1, 2, 2, 2, 1, 3, 3, 1, 1, 2, 1]
Possible Kolakoski sequence? Yes

First 30 members of the sequence generated by [1, 3, 2, 1]:
[1, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 3, 3, 2, 2, 1, 1, 3, 2, 1, 1, 1, 1, 3, 3, 3, 2, 2, 1]
Possible Kolakoski sequence? No


```



## Lua

{{trans|C}}

```lua
function next_in_cycle(c,length,index)
    local pos = index % length
    return c[pos]
end

function kolakoski(c,s,clen,slen)
    local i = 0
    local k = 0

    while true do
        s[i] = next_in_cycle(c,clen,k)
        if s[k] > 1 then
            for j=1,s[k]-1 do
                i = i + 1
                if i == slen then
                    return nil
                end
                s[i] = s[i - 1]
            end
        end
        i = i + 1
        if i == slen then
            return nil
        end
        k = k + 1
    end
    return nil
end

function possible_kolakoski(s,length)
    local j = 0
    local prev = s[0]
    local count = 1
    local rle = {}
    local result = "True"

    for i=0,length do
        rle[i] = 0
    end

    for i=1,length-1 do
        if s[i] == prev then
            count = count + 1
        else
            rle[j] = count
            j = j + 1
            count = 1
            prev = s[i]
        end
    end

    -- no point adding the final 'count' to rle as we're not going to compare it anyway
    for i=0,j-1 do
        if rle[i] ~= s[i] then
            result = "False"
            break
        end
    end

    return result
end

function print_array(a)
    io.write("[")
    for i=0,#a do
        if i>0 then
            io.write(", ")
        end
        io.write(a[i])
    end
    io.write("]")
end

-- main
local c0 =    {[0]=1,  [1]=2}
local c1 =    {[0]=2,  [1]=1}
local c2 =    {[0]=1,  [1]=3,  [2]=1,  [3]=2}
local c3 =    {[0]=1,  [1]=3,  [2]=2,  [3]=1}

local cs =    {[0]=c0, [1]=c1, [2]=c2, [3]=c3}
local clens = {[0]=2,  [1]=2,  [2]=4,  [3]=4}
local slens = {[0]=20, [1]=20, [2]=30, [3]=30}

for i=0,3 do
    local clen = clens[i]
    local slen = slens[i]
    local s = {}

    for j=0,slen-1 do
        s[j] = 0
    end

    kolakoski(cs[i],s,clen,slen)
    io.write(string.format("First %d members of the sequence generated by ", slen))
    print_array(cs[i])
    print(":")
    print_array(s)
    print()

    local p = possible_kolakoski(s,slen)
    print(string.format("Possible Kolakoski sequence? %s", p))

    print()
end
```

{{out}}

```txt
First 20 members of the sequence generated by [1, 2]:
[1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1]
Possible Kolakoski sequence? True

First 20 members of the sequence generated by [2, 1]:
[2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2]
Possible Kolakoski sequence? True

First 30 members of the sequence generated by [1, 3, 1, 2]:
[1, 3, 3, 3, 1, 1, 1, 2, 2, 2, 1, 3, 1, 2, 2, 1, 1, 3, 3, 1, 2, 2, 2, 1, 3, 3, 1, 1, 2, 1]
Possible Kolakoski sequence? True

First 30 members of the sequence generated by [1, 3, 2, 1]:
[1, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 3, 3, 2, 2, 1, 1, 3, 2, 1, 1, 1, 1, 3, 3, 3, 2, 2, 1]
Possible Kolakoski sequence? False
```



## Perl

{{trans|Perl 6}}

```perl
sub kolakoski {
    my($terms,@seed) = @_;
    my @k;
    my $k = $seed[0] == 1 ? 1 : 0;
    if ($k == 1) { @k = (1, split //, (($seed[1]) x $seed[1])) }
    else         { @k = ($seed[0]) x $seed[0] }
    do {
        $k++;
        push @k, ($seed[$k % @seed]) x $k[$k];
    } until $terms <= @k;
    @k[0..$terms-1]
}

sub rle {
    (my $string = join '', @_) =~ s/((.)\2*)/length $1/eg;
    split '', $string
}

for ([20,1,2], [20,2,1], [30,1,3,1,2], [30,1,3,2,1]) {
    $terms = shift @$_;
    print "\n$terms members of the series generated from [@$_] is:\n";
    print join(' ', @kolakoski = kolakoski($terms, @$_)) . "\n";
    $status = join('', @rle = rle(@kolakoski)) eq join('', @kolakoski[0..$#rle]) ? 'True' : 'False';
    print "Looks like a Kolakoski sequence?: $status\n";
}
```

{{out}}

```txt
20 members of the series generated from [1 2] is:
1 2 2 1 1 2 1 2 2 1 2 2 1 1 2 1 1 2 2 1
Looks like a Kolakoski sequence?: True

20 members of the series generated from [2 1] is:
2 2 1 1 2 1 2 2 1 2 2 1 1 2 1 1 2 2 1 2
Looks like a Kolakoski sequence?: True

30 members of the series generated from [1 3 1 2] is:
1 3 3 3 1 1 1 2 2 2 1 3 1 2 2 1 1 3 3 1 2 2 2 1 3 3 1 1 2 1
Looks like a Kolakoski sequence?: True

30 members of the series generated from [1 3 2 1] is:
1 3 3 3 2 2 2 1 1 1 1 1 3 3 2 2 1 1 3 2 1 1 1 1 3 3 3 2 2 1
Looks like a Kolakoski sequence?: False
```



## Perl 6

{{works with|Rakudo|2018.04.01}}


```perl6
sub kolakoski (*@seed) {
    my $k = @seed[0] == 1 ?? 1 !! 0;
    my @k = flat @seed[0] == 1 ?? (1, @seed[1] xx @seed[1]) !! @seed[0] xx @seed[0],
      { $k++; @seed[$k % @seed] xx @k[$k] } … *
}

sub rle (*@series) { @series.join.subst(/((.)$0*)/, -> { $0.chars }, :g).comb».Int }

# Testing
for [1, 2], 20,
    [2, 1], 20,
    [1, 3, 1, 2], 30,
    [1, 3, 2, 1], 30
  -> @seed, $terms {
    say "\n## $terms members of the series generated from { @seed.perl } is:\n   ",
    my @kolakoski = kolakoski(@seed)[^$terms];
    my @rle = rle @kolakoski;
    say "   Looks like a Kolakoski sequence?: ", @rle[*] eqv @kolakoski[^@rle];
}
```

{{out}}

```txt
## 20 members of the series generated from [1, 2] is:
   [1 2 2 1 1 2 1 2 2 1 2 2 1 1 2 1 1 2 2 1]
   Looks like a Kolakoski sequence?: True

## 20 members of the series generated from [2, 1] is:
   [2 2 1 1 2 1 2 2 1 2 2 1 1 2 1 1 2 2 1 2]
   Looks like a Kolakoski sequence?: True

## 30 members of the series generated from [1, 3, 1, 2] is:
   [1 3 3 3 1 1 1 2 2 2 1 3 1 2 2 1 1 3 3 1 2 2 2 1 3 3 1 1 2 1]
   Looks like a Kolakoski sequence?: True

## 30 members of the series generated from [1, 3, 2, 1] is:
   [1 3 3 3 2 2 2 1 1 1 1 1 3 3 2 2 1 1 3 2 1 1 1 1 3 3 3 2 2 1]
   Looks like a Kolakoski sequence?: False
```



## Phix

{{trans|C}}

```Phix
function kolakoski(sequence cycle, integer n)
    sequence s = {}
    integer k = 1
    while length(s)<n do
        integer c = cycle[mod(k-1,length(cycle))+1]
        s &= repeat(c,iff(k>length(s)?c:s[k]))
        k += 1
    end while
    s = s[1..n]
    return s
end function
 
function possible_kolakoski(sequence s)
    integer count = 1
    sequence rle = {}
    for i=2 to length(s) do
        if s[i]==s[i-1] then
            count += 1
        else
            rle &= count
            count = 1
        end if
    end for
    -- (final count probably incomplete, so ignore it)
    return rle = s[1..length(rle)]
end function
 
constant cycles = {{1,2},20,
                   {2,1},20,
                   {1,3,1,2},30,
                   {1,3,2,1},30}

for i=1 to length(cycles) by 2 do
    {sequence c, integer n} = cycles[i..i+1]
    sequence s = kolakoski(c,n)
    printf(1,"First %d members of the sequence generated by %s\n", {n,sprint(c)})
    ?s
    bool p = possible_kolakoski(s)
    printf(1,"Possible Kolakoski sequence? %s\n\n", {iff(p ? "Yes" : "No")})
end for
```

{{out}}

```txt

First 20 members of the sequence generated by {1,2}
{1,2,2,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2,2,1}
Possible Kolakoski sequence? Yes

First 20 members of the sequence generated by {2,1}
{2,2,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2,2,1,2}
Possible Kolakoski sequence? Yes

First 30 members of the sequence generated by {1,3,1,2}
{1,3,3,3,1,1,1,2,2,2,1,3,1,2,2,1,1,3,3,1,2,2,2,1,3,3,1,1,2,1}
Possible Kolakoski sequence? Yes

First 30 members of the sequence generated by {1,3,2,1}
{1,3,3,3,2,2,2,1,1,1,1,1,3,3,2,2,1,1,3,2,1,1,1,1,3,3,3,2,2,1}
Possible Kolakoski sequence? No

```



## Python

Python 3.6+


```python
import itertools

def cycler(start_items):
	return itertools.cycle(start_items).__next__

def _kolakoski_gen(start_items):
    s, k = [], 0
    c = cycler(start_items)
    while True:
        c_next = c()
        s.append(c_next)
        sk = s[k]
        yield sk
        if sk > 1:
            s += [c_next] * (sk - 1)
        k += 1

def kolakoski(start_items=(1, 2), length=20):
    return list(itertools.islice(_kolakoski_gen(start_items), length))

def _run_len_encoding(truncated_series):
    return [len(list(group)) for grouper, group in itertools.groupby(truncated_series)][:-1]

def is_series_eq_its_rle(series):
    rle = _run_len_encoding(series)
    return (series[:len(rle)] == rle) if rle else not series

if __name__ == '__main__':
    for start_items, length in [((1, 2), 20), ((2, 1), 20), 
                                ((1, 3, 1, 2), 30), ((1, 3, 2, 1), 30)]:
        print(f'\n## {length} members of the series generated from {start_items} is:')
        s = kolakoski(start_items, length)
        print(f'  {s}')
        ans = 'YES' if is_series_eq_its_rle(s) else 'NO'
        print(f'  Does it look like a Kolakoski sequence: {ans}')
```


{{out}}

```txt

## 20 members of the series generated from (1, 2) is:
  [1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1]
  Does it look like a Kolakoski sequence: YES

## 20 members of the series generated from (2, 1) is:
  [2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2]
  Does it look like a Kolakoski sequence: YES

## 30 members of the series generated from (1, 3, 1, 2) is:
  [1, 3, 3, 3, 1, 1, 1, 2, 2, 2, 1, 3, 1, 2, 2, 1, 1, 3, 3, 1, 2, 2, 2, 1, 3, 3, 1, 1, 2, 1]
  Does it look like a Kolakoski sequence: YES

## 30 members of the series generated from (1, 3, 2, 1) is:
  [1, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 3, 3, 2, 2, 1, 1, 3, 2, 1, 1, 1, 1, 3, 3, 3, 2, 2, 1]
  Does it look like a Kolakoski sequence: NO
```



## Ruby


```Ruby
def create_generator(ar)
  Enumerator.new do |y|
    cycle = ar.cycle
    s = []
    loop do
      t = cycle.next
      s.push(t)
      v = s.shift
      y << v
      (v-1).times{s.push(t)}
    end
  end
end

def rle(ar)
  ar.slice_when{|a,b| a != b}.map(&:size)
end

[[20, [1,2]], 
 [20, [2,1]], 
 [30, [1,3,1,2]],
 [30, [1,3,2,1]]].each do |num,ar|
  puts "\nFirst #{num} of the sequence generated by #{ar.inspect}:"
  p res = create_generator(ar).take(num)
  puts "Possible Kolakoski sequence? #{res.join.start_with?(rle(res).join)}"
end

```

{{out}}

```txt

First 20 of the sequence generated by [1, 2]:
[1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1]
Possible Kolakoski sequence? true

First 20 of the sequence generated by [2, 1]:
[2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2]
Possible Kolakoski sequence? true

First 30 of the sequence generated by [1, 3, 1, 2]:
[1, 3, 3, 3, 1, 1, 1, 2, 2, 2, 1, 3, 1, 2, 2, 1, 1, 3, 3, 1, 2, 2, 2, 1, 3, 3, 1, 1, 2, 1]
Possible Kolakoski sequence? true

First 30 of the sequence generated by [1, 3, 2, 1]:
[1, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 3, 3, 2, 2, 1, 1, 3, 2, 1, 1, 1, 1, 3, 3, 3, 2, 2, 1]
Possible Kolakoski sequence? false

```



## zkl

{{trans|Python}}

```zkl
fcn kolakoski(start_items=List(1,2), length=20){  //-->List
   Walker.tweak(fcn(s,rk,cw){	// infinite iterator
      s.append( c_next:=cw() );
      sk:=s[rk.inc()];		// inc returns previous value, ie k++
      if(sk>1) s.extend((List.createLong(sk - 1,c_next)));  // list of sk cn's
      sk		// where we are in s, not end of s
   }.fp(List(), Ref(0), Walker.cycle(start_items).next) )
   .walk(length);	// iterate length times, return list
}
```


```zkl
fcn _run_len_encoding(truncated_series){  //List-->List
   truncated_series.reduce(fcn(a,b,rm,s){ # if trailing singleton, it is ignored
      if(a==b){ rm.inc(); return(b); }
      s.append(rm.value);
      rm.set(1);
      b
   }.fp2(Ref(1),s:=List()) );
   s
} 
fcn is_series_eq_its_rle(series){	//-->Bool
   rle:=_run_len_encoding(series);
   series[0,rle.len()]==rle
}
```


```zkl
foreach sl in (List( L( L(1,2), 20), L( L(2, 1), 20),
                     L( L(1,3,1,2), 30), L( L(1,3,2,1), 30) )){
   start_items, length := sl;
   println("First %d members of the series generated from (%s) are:"
           .fmt(length,start_items.concat(",")));
   println("   (%s)".fmt(( s:=kolakoski(start_items, length) ).concat(",") ));
   println("   Does it look like a Kolakoski sequence: ",is_series_eq_its_rle(s) )
}
```

{{out}}

```txt

First 20 members of the series generated from (1,2) are:
   (1,2,2,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2,2,1)
   Does it look like a Kolakoski sequence: True
First 20 members of the series generated from (2,1) are:
   (2,2,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2,2,1,2)
   Does it look like a Kolakoski sequence: True
First 30 members of the series generated from (1,3,1,2) are:
   (1,3,3,3,1,1,1,2,2,2,1,3,1,2,2,1,1,3,3,1,2,2,2,1,3,3,1,1,2,1)
   Does it look like a Kolakoski sequence: True
First 30 members of the series generated from (1,3,2,1) are:
   (1,3,3,3,2,2,2,1,1,1,1,1,3,3,2,2,1,1,3,2,1,1,1,1,3,3,3,2,2,1)
   Does it look like a Kolakoski sequence: False

```

