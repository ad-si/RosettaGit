+++
title = "Burrows–Wheeler transform"
description = ""
date = 2019-10-12T17:22:18Z
aliases = []
[extra]
id = 21925
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
{{Wikipedia|Burrows–Wheeler_transform}}


The Burrows–Wheeler transform (BWT, also called block-sorting compression) rearranges a character string into runs of similar characters.

This is useful for compression, since it tends to be easy to compress a string that has runs of repeated characters by techniques such as move-to-front transform and run-length encoding.

More importantly, the transformation is reversible, without needing to store any additional data.

The BWT is thus a "free" method of improving the efficiency of text compression algorithms, costing only some extra computation.


Source: [[wp:Burrows–Wheeler_transform|Burrows–Wheeler transform]]





## C

{{trans|Python}}

```c
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

const char STX = '\002', ETX = '\003';

int compareStrings(const void *a, const void *b) {
    char *aa = *(char **)a;
    char *bb = *(char **)b;
    return strcmp(aa, bb);
}

int bwt(const char *s, char r[]) {
    int i, len = strlen(s) + 2;
    char *ss, *str;
    char **table;
    if (strchr(s, STX) || strchr(s, ETX)) return 1;
    ss = calloc(len + 1, sizeof(char));
    sprintf(ss, "%c%s%c", STX, s, ETX);
    table = malloc(len * sizeof(const char *));
    for (i = 0; i < len; ++i) {
        str = calloc(len + 1, sizeof(char));
        strcpy(str, ss + i);
        if (i > 0) strncat(str, ss, i);
        table[i] = str;
    }
    qsort(table, len, sizeof(const char *), compareStrings);
    for(i = 0; i < len; ++i) {
        r[i] = table[i][len - 1];
        free(table[i]);
    }
    free(table);
    free(ss);
    return 0;
}

void ibwt(const char *r, char s[]) {
    int i, j, len = strlen(r);
    char **table = malloc(len * sizeof(const char *));
    for (i = 0; i < len; ++i) table[i] = calloc(len + 1, sizeof(char));
    for (i = 0; i < len; ++i) {
        for (j = 0; j < len; ++j) {
            memmove(table[j] + 1, table[j], len);
            table[j][0] = r[j];
        }
        qsort(table, len, sizeof(const char *), compareStrings);
    }
    for (i = 0; i < len; ++i) {
        if (table[i][len - 1] == ETX) {
            strncpy(s, table[i] + 1, len - 2);
            break;
        }
    }
    for (i = 0; i < len; ++i) free(table[i]);
    free(table);
}

void makePrintable(const char *s, char t[]) {
    strcpy(t, s);
    for ( ; *t != '\0'; ++t) {
        if (*t == STX) *t = '^';
        else if (*t == ETX) *t = '|';
    }
}

int main() {
    int i, res, len;
    char *tests[6], *t, *r, *s;
    tests[0] = "banana";
    tests[1] = "appellee";
    tests[2] = "dogwood";
    tests[3] = "TO BE OR NOT TO BE OR WANT TO BE OR NOT?";
    tests[4] = "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES",
    tests[5] = "\002ABC\003";
    for (i = 0; i < 6; ++i) {
        len = strlen(tests[i]);
        t = calloc(len + 1, sizeof(char));
        makePrintable(tests[i], t);
        printf("%s\n", t);
        printf(" --> ");
        r = calloc(len + 3, sizeof(char));
        res = bwt(tests[i], r);
        if (res == 1) {
            printf("ERROR: String can't contain STX or ETX\n");
        }
        else {
            makePrintable(r, t);
            printf("%s\n", t);
        }
        s = calloc(len + 1, sizeof(char));
        ibwt(r, s);
        makePrintable(s, t);
        printf(" --> %s\n\n", t);
        free(t);
        free(r);
        free(s);
    }
    return 0;
}
```


{{out}}

```txt

banana
 --> |annb^aa
 --> banana

appellee
 --> |e^elplepa
 --> appellee

dogwood
 --> |do^oodwg
 --> dogwood

TO BE OR NOT TO BE OR WANT TO BE OR NOT?
 --> |?OOORREEETTRTW   BBB  ATTT   NNOOONOO^
 --> TO BE OR NOT TO BE OR WANT TO BE OR NOT?

SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES
 --> |STEXYDST.E.IXXIIXXSSMPPS.B..EE.^.USFXDIIOIIIT
 --> SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES

^ABC|
 --> ERROR: String can't contain STX or ETX
 -->

```



## C++

{{trans|C#}}

```cpp
#include <algorithm>
#include <iostream>
#include <vector>

const int STX = 0x02;
const int ETX = 0x03;

void rotate(std::string &a) {
    char t = a[a.length() - 1];
    for (int i = a.length() - 1; i > 0; i--) {
        a[i] = a[i - 1];
    }
    a[0] = t;
}

std::string bwt(const std::string &s) {
    for (char c : s) {
        if (c == STX || c == ETX) {
            throw std::runtime_error("Input can't contain STX or ETX");
        }
    }

    std::string ss;
    ss += STX;
    ss += s;
    ss += ETX;

    std::vector<std::string> table;
    for (size_t i = 0; i < ss.length(); i++) {
        table.push_back(ss);
        rotate(ss);
    }
    //table.sort();
    std::sort(table.begin(), table.end());

    std::string out;
    for (auto &s : table) {
        out += s[s.length() - 1];
    }
    return out;
}

std::string ibwt(const std::string &r) {
    int len = r.length();
    std::vector<std::string> table(len);
    for (int i = 0; i < len; i++) {
        for (int j = 0; j < len; j++) {
            table[j] = r[j] + table[j];
        }
        std::sort(table.begin(), table.end());
    }
    for (auto &row : table) {
        if (row[row.length() - 1] == ETX) {
            return row.substr(1, row.length() - 2);
        }
    }
    return {};
}

std::string makePrintable(const std::string &s) {
    auto ls = s;
    for (auto &c : ls) {
        if (c == STX) {
            c = '^';
        } else if (c == ETX) {
            c = '|';
        }
    }
    return ls;
}

int main() {
    auto tests = {
        "banana",
        "appellee",
        "dogwood",
        "TO BE OR NOT TO BE OR WANT TO BE OR NOT?",
        "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES",
        "\u0002ABC\u0003"
    };

    for (auto &test : tests) {
        std::cout << makePrintable(test) << "\n";
        std::cout << " --> ";

        std::string t;
        try {
            t = bwt(test);
            std::cout << makePrintable(t) << "\n";
        } catch (std::runtime_error &e) {
            std::cout << "Error " << e.what() << "\n";
        }

        std::string r = ibwt(t);
        std::cout << " --> " << r << "\n\n";
    }

    return 0;
}
```

{{out}}

```txt
banana
 --> |annb^aa
 --> banana

appellee
 --> |e^elplepa
 --> appellee

dogwood
 --> |do^oodwg
 --> dogwood

TO BE OR NOT TO BE OR WANT TO BE OR NOT?
 --> |?OOORREEETTRTW   BBB  ATTT   NNOOONOO^
 --> TO BE OR NOT TO BE OR WANT TO BE OR NOT?

SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES
 --> |STEXYDST.E.IXXIIXXSSMPPS.B..EE.^.USFXDIIOIIIT
 --> SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES

^ABC|
 --> Error Input can't contain STX or ETX
 -->
```


=={{header|C#|C sharp}}==
{{trans|D}}

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace BurrowsWheeler {
    class Program {
        const char STX = (char)0x02;
        const char ETX = (char)0x03;

        private static void Rotate(ref char[] a) {
            char t = a.Last();
            for (int i = a.Length - 1; i > 0; --i) {
                a[i] = a[i - 1];
            }
            a[0] = t;
        }

        // For some reason, strings do not compare how whould be expected
        private static int Compare(string s1, string s2) {
            for (int i = 0; i < s1.Length && i < s2.Length; ++i) {
                if (s1[i] < s2[i]) {
                    return -1;
                }
                if (s2[i] < s1[i]) {
                    return 1;
                }
            }
            if (s1.Length < s2.Length) {
                return -1;
            }
            if (s2.Length < s1.Length) {
                return 1;
            }
            return 0;
        }

        static string Bwt(string s) {
            if (s.Any(a => a == STX || a == ETX)) {
                throw new ArgumentException("Input can't contain STX or ETX");
            }
            char[] ss = (STX + s + ETX).ToCharArray();
            List<string> table = new List<string>();
            for (int i = 0; i < ss.Length; ++i) {
                table.Add(new string(ss));
                Rotate(ref ss);
            }
            table.Sort(Compare);
            return new string(table.Select(a => a.Last()).ToArray());
        }

        static string Ibwt(string r) {
            int len = r.Length;
            List<string> table = new List<string>(new string[len]);
            for (int i = 0; i < len; ++i) {
                for (int j = 0; j < len; ++j) {
                    table[j] = r[j] + table[j];
                }
                table.Sort(Compare);
            }
            foreach (string row in table) {
                if (row.Last() == ETX) {
                    return row.Substring(1, len - 2);
                }
            }
            return "";
        }

        static string MakePrintable(string s) {
            return s.Replace(STX, '^').Replace(ETX, '|');
        }

        static void Main() {
            string[] tests = new string[] {
                "banana",
                "appellee",
                "dogwood",
                "TO BE OR NOT TO BE OR WANT TO BE OR NOT?",
                "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES",
                "\u0002ABC\u0003"
            };

            foreach (string test in tests) {
                Console.WriteLine(MakePrintable(test));
                Console.Write(" --> ");

                string t = "";
                try {
                    t = Bwt(test);
                    Console.WriteLine(MakePrintable(t));
                } catch (Exception e) {
                    Console.WriteLine("ERROR: {0}", e.Message);
                }

                string r = Ibwt(t);
                Console.WriteLine(" --> {0}", r);
                Console.WriteLine();
            }
        }
    }
}
```

{{out}}

```txt
banana
 --> |annb^aa
 --> banana

appellee
 --> |e^elplepa
 --> appellee

dogwood
 --> |do^oodwg
 --> dogwood

TO BE OR NOT TO BE OR WANT TO BE OR NOT?
 --> |?OOORREEETTRTW   BBB  ATTT   NNOOONOO^
 --> TO BE OR NOT TO BE OR WANT TO BE OR NOT?

SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES
 --> |STEXYDST.E.IXXIIXXSSMPPS.B..EE.^.USFXDIIOIIIT
 --> SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES

^ABC|
 --> ERROR: Input can't contain STX or ETX
 -->
```



## D

{{trans|Kotlin}}

```d
import std.algorithm.iteration;
import std.algorithm.mutation;
import std.algorithm.searching;
import std.algorithm.sorting;
import std.array;
import std.stdio;
import std.string;

immutable STX = 0x02;
immutable ETX = 0x03;

string bwt(string s) {
    if (s.any!"a==0x02 || a==0x03") {
        throw new Exception("Input can't contain STX or ETX");
    }
    char[] ss = (STX ~ s ~ ETX).dup;
    string[] table;
    foreach (i; 0..ss.length) {
        table ~= ss.idup;
        bringToFront(ss[0..$-1], ss[$-1..$]);
    }
    table.sort();
    return table.map!"a[$-1]".array;
}

string ibwt(string r) {
    const len = r.length;
    string[] table;
    table.length = len;
    foreach (_; 0..len) {
        foreach (i; 0..len) {
            table[i] = r[i] ~ table[i];
        }
        table.sort();
    }
    foreach (row; table) {
        if (row[$-1] == ETX) {
            return row[1..len-1];
        }
    }
    return "";
}

string makePrintable(string s) {
    return tr(s, "\u0002\u0003", "^|");
}

void main() {
    immutable tests = [
        "banana",
        "appellee",
        "dogwood",
        "TO BE OR NOT TO BE OR WANT TO BE OR NOT?",
        "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES",
        "\u0002ABC\u0003"
    ];

    foreach (test; tests) {
        writeln(test.makePrintable);
        write(" --> ");

        string t;
        try {
            t = bwt(test);
            writeln(t.makePrintable);
        } catch (Exception e) {
            writeln("ERROR: ", e.message);
        }

        auto r = ibwt(t);
        writeln(" --> ", r);
        writeln;
    }
}
```

{{out}}

```txt
banana
 --> |annb^aa
 --> banana

appellee
 --> |e^elplepa
 --> appellee

dogwood
 --> |do^oodwg
 --> dogwood

TO BE OR NOT TO BE OR WANT TO BE OR NOT?
 --> |?OOORREEETTRTW   BBB  ATTT   NNOOONOO^
 --> TO BE OR NOT TO BE OR WANT TO BE OR NOT?

SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES
 --> |STEXYDST.E.IXXIIXXSSMPPS.B..EE.^.USFXDIIOIIIT
 --> SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES

^ABC|
 --> ERROR: Input can't contain STX or ETX
 -->
```



## Factor

Factor has a Burrows-Wheeler transform implementation in its standard library. In addition to the transformed sequence, the <code>bwt</code> word also outputs an index for use with the inverse <code>ibwt</code> word.

```factor
USING: formatting io kernel math.transforms.bwt sequences ;
{
    "banana" "dogwood" "TO BE OR NOT TO BE OR WANT TO BE OR NOT?"
    "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES"
} [
    [ print ] [ bwt ] bi
    2dup "  bwt-->%3d %u\n" printf
    ibwt "  ibwt->    %u\n" printf nl
] each
```

{{out}}

```txt

banana
  bwt-->  3 "nnbaaa"
  ibwt->    "banana"

dogwood
  bwt-->  1 "odoodwg"
  ibwt->    "dogwood"

TO BE OR NOT TO BE OR WANT TO BE OR NOT?
  bwt--> 36 "OOORREEETTRTW   BBB  ATTT   NNOOONOO?   "
  ibwt->    "TO BE OR NOT TO BE OR WANT TO BE OR NOT?"

SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES
  bwt--> 29 "TEXYDST.E.IXIXIXXSSMPPS.B..E.S.EUSFXDIIOIIIT"
  ibwt->    "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES"

```



## Go

{{trans|Python}}

```go
package main

import (
    "fmt"
    "sort"
    "strings"
)

const stx = "\002"
const etx = "\003"

func bwt(s string) (string, error) {
    if strings.Index(s, stx) >= 0 || strings.Index(s, etx) >= 0 {
        return "", fmt.Errorf("String can't contain STX or ETX")
    }
    s = stx + s + etx
    le := len(s)
    table := make([]string, le)
    table[0] = s
    for i := 1; i < le; i++ {
        table[i] = s[i:] + s[:i]
    }
    sort.Strings(table)
    lastBytes := make([]byte, le)
    for i := 0; i < le; i++ {
        lastBytes[i] = table[i][le-1]
    }
    return string(lastBytes), nil
}

func ibwt(r string) string {
    le := len(r)
    table := make([]string, le)
    for range table {
        for i := 0; i < le; i++ {
            table[i] = r[i:i+1] + table[i]
        }
        sort.Strings(table)
    }
    for _, row := range table {
        if strings.HasSuffix(row, etx) {
            return row[1 : le-1]
        }
    }
    return ""
}

func makePrintable(s string) string {
    // substitute ^ for STX and | for ETX to print results
    t := strings.Replace(s, stx, "^", 1)
    return strings.Replace(t, etx, "|", 1)
}

func main() {
    tests := []string{
        "banana",
        "appellee",
        "dogwood",
        "TO BE OR NOT TO BE OR WANT TO BE OR NOT?",
        "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES",
        "\002ABC\003",
    }
    for _, test := range tests {
        fmt.Println(makePrintable(test))
        fmt.Print(" --> ")
        t, err := bwt(test)
        if err != nil {
            fmt.Println("ERROR:", err)
        } else {
            fmt.Println(makePrintable(t))
        }
        r := ibwt(t)
        fmt.Println(" -->", r, "\n")
    }
}
```


{{out}}

```txt

banana
 --> |annb^aa
 --> banana

appellee
 --> |e^elplepa
 --> appellee

dogwood
 --> |do^oodwg
 --> dogwood

TO BE OR NOT TO BE OR WANT TO BE OR NOT?
 --> |?OOORREEETTRTW   BBB  ATTT   NNOOONOO^
 --> TO BE OR NOT TO BE OR WANT TO BE OR NOT?

SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES
 --> |STEXYDST.E.IXXIIXXSSMPPS.B..EE.^.USFXDIIOIIIT
 --> SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES

^ABC|
 --> ERROR: String can't contain STX or ETX
 -->

```



## Haskell


```haskell
-- A straightforward, inefficient implementation of the Burrows–Wheeler
-- transform, based on the description in the Wikipedia article.
--
-- Special characters are *not* used to indicate the start or end of sequences,
-- so all strings can be represented.

import Data.List ((!!), find, sort, tails, transpose)
import Data.Maybe (fromJust)
import Text.Printf (printf)

newtype BWT a = BWT [Val a]

bwt :: Ord a => [a] -> BWT a
bwt xs = let n  = length xs + 2
             ys = transpose $ sort $ take n $ tails $ cycle $ pos xs
         in BWT $ ys !! (n-1)

invBwt :: Ord a => BWT a -> [a]
invBwt (BWT xs) = let ys = iterate step (map (const []) xs) !! length xs
                  in unpos $ fromJust $ find ((== Post) . last) ys
  where step = sort . zipWith (:) xs


data Val a = In a | Pre | Post deriving (Eq, Ord)

pos :: [a] -> [Val a]
pos xs = Pre : map In xs ++ [Post]

unpos :: [Val a] -> [a]
unpos xs = [x | In x <- xs]


main :: IO ()
main = mapM_ testBWT [ "", "a", "BANANA", "dogwood",
                       "TO BE OR NOT TO BE OR WANT TO BE OR NOT?",
                       "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES" ]

testBWT :: String -> IO ()
testBWT xs = let fwd = bwt xs
                 inv = invBwt fwd
             in printf "%s\n\t%s\n\t%s\n" xs (pretty fwd) inv
  where pretty (BWT ps) = map prettyVal ps
        prettyVal (In c) = c
        prettyVal Pre    = '^'
        prettyVal Post   = '|'
```


{{out}}

```txt


        |^

a
        ^|a
        a
BANANA
        BNN^AA|A
        BANANA
dogwood
        ^ooodwg|d
        dogwood
TO BE OR NOT TO BE OR WANT TO BE OR NOT?
        OOORREEETTRTW   BBB  ATTT   NNOOONOO^   |?
        TO BE OR NOT TO BE OR WANT TO BE OR NOT?
SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES
        TEXYDST.E.IXIXIXXSSMPPS.B..E.^.UESFXDIIOIIIT|S
        SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES

```



## Julia


```julia
bwsort(vec) = sort(vec, lt = (a, b) -> string(a) < string(b))

function burrowswheeler_encode(s)
    if match(r"\x02|\x03", s) != nothing
        throw("String for Burrows-Wheeler input cannot contain STX or ETX")
    end
    s = "\x02" * s * "\x03"
    String([t[end] for t in bwsort([circshift([c for c in s], n) for n in 0:length(s)-1])])
end

function burrowswheeler_decode(s)
    len, v = length(s), [c for c in s]
    m = fill(' ', len, len)
    for col in len:-1:1
        m[:, col] .= v
        for (i, row) in enumerate(bwsort([collect(r) for r in eachrow(m)]))
            m[i, :] .= row
        end
    end
    String(m[findfirst(row -> m[row, end] == '\x03', 1:len), 2:end-1])
end

for s in ["BANANA", "dogwood", "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES",
    "TO BE OR NOT TO BE OR WANT TO BE OR NOT?", "Oops\x02"]
    println("Original: ", s, "\nTransformation: ", burrowswheeler_encode(s),
        "\nInverse transformation: ", burrowswheeler_decode(burrowswheeler_encode(s)), "\n")
end

```
{{out}}

```txt

Original: BANANA
Transformation: BNN�AA�A
Inverse transformation: BANANA

Original: dogwood
Transformation: �do�oodwg
Inverse transformation: dogwood

Original: SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES
Transformation: TEXYDST.E.IXIXIXXSSMPPS.B..E.�.UESFXDIIOIIIT�S
Inverse transformation: SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES

Original: TO BE OR NOT TO BE OR WANT TO BE OR NOT?
Transformation: OOORREEETTRTW   BBB  ATTT   NNOOONOO�   �?
Inverse transformation: TO BE OR NOT TO BE OR WANT TO BE OR NOT?

ERROR: LoadError: "String for Burrows-Wheeler input cannot contain STX or ETX"

```



## Kotlin

{{trans|Python}}

```scala
// Version 1.2.60

const val STX = "\u0002"
const val ETX = "\u0003"

fun bwt(s: String): String {
    if (s.contains(STX) || s.contains(ETX)) {
        throw RuntimeException("String can't contain STX or ETX")
    }
    val ss = STX + s + ETX
    val table = Array<String>(ss.length) { ss.substring(it) + ss.substring(0, it) }
    table.sort()
    return String(table.map { it[it.lastIndex] }.toCharArray())
}

fun ibwt(r: String): String {
    val len = r.length
    val table = Array<String>(len) { "" }
    repeat(len) {
        for (i in 0 until len) {
            table[i] = r[i].toString() + table[i]
        }
        table.sort()
    }
    for (row in table) {
        if (row.endsWith(ETX)) {
            return row.substring(1, len - 1)
        }
    }
    return ""
}

fun makePrintable(s: String): String {
    // substitute ^ for STX and | for ETX to print results
    return s.replace(STX, "^").replace(ETX, "|")
}

fun main(args: Array<String>) {
    val tests = listOf(
        "banana",
        "appellee",
        "dogwood",
        "TO BE OR NOT TO BE OR WANT TO BE OR NOT?",
        "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES",
        "\u0002ABC\u0003"
    )
    for (test in tests) {
        println(makePrintable(test))
        print(" --> ")
        var t = ""
        try {
            t = bwt(test)
            println(makePrintable(t))
        }
        catch (ex: RuntimeException) {
            println("ERROR: " + ex.message)
        }
        val r = ibwt(t)
        println(" --> $r\n")
    }
}
```


{{output}}

```txt

banana
 --> |annb^aa
 --> banana

appellee
 --> |e^elplepa
 --> appellee

dogwood
 --> |do^oodwg
 --> dogwood

TO BE OR NOT TO BE OR WANT TO BE OR NOT?
 --> |?OOORREEETTRTW   BBB  ATTT   NNOOONOO^
 --> TO BE OR NOT TO BE OR WANT TO BE OR NOT?

SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES
 --> |STEXYDST.E.IXXIIXXSSMPPS.B..EE.^.USFXDIIOIIIT
 --> SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES

^ABC|
 --> ERROR: String can't contain STX or ETX
 -->

```



## Perl

{{trans|Perl 6}}

```perl
use utf8;
binmode STDOUT, ":utf8";

use constant STX => '👍 ';

sub transform {
    my($s) = @_;
    my($t);
    warn "String can't contain STX character." and exit if $s =~ /STX/;
    $s = STX . $s;
    $t .= substr($_,-1,1) for sort map { rotate($s,$_) } 1..length($s);
    return $t;
}

sub rotate { my($s,$n) = @_; join '', (split '', $s)[$n..length($s)-1, 0..$n-1] }

sub ɯɹoɟsuɐɹʇ {
    my($s) = @_;
    my @s = split '', $s;
    my @t = sort @s;
    map { @t = sort map { $s[$_] . $t[$_] } 0..length($s)-1 } 1..length($s)-1;
    for (@t) {
        next unless /${\(STX)}$/;  # interpolate the constant
        chop $_ and return $_
    }
}

for $phrase (qw<BANANA dogwood SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES>,
    'TO BE OR NOT TO BE OR WANT TO BE OR NOT?') {
    push @res, 'Original:            '. $phrase;
    push @res, 'Transformed:         '. transform $phrase;
    push @res, 'Inverse transformed: '. ɯɹoɟsuɐɹʇ transform $phrase;
    push @res, '';
}

print join "\n", @res;
```

{{out}}

```txt
Original:            BANANA
Transformed:         BNN👍 AAA
Inverse transformed: BANANA

Original:            dogwood
Transformed:         👍 ooodwgd
Inverse transformed: dogwood

Original:            SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES
Transformed:         TEXYDST.E.IXIXIXXSSMPPS.B..E.👍 .UESFXDIIOIIITS
Inverse transformed: SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES

Original:            TO BE OR NOT TO BE OR WANT TO BE OR NOT?
Transformed:         OOORREEETTRTW   BBB  ATTT   NNOOONOO👍    ?
Inverse transformed: TO BE OR NOT TO BE OR WANT TO BE OR NOT?
```



## Perl 6

{{works with|Rakudo|2018.06}}


```perl6
# STX can be any character that doesn't appear in the text.
# Using a visible character here for ease of viewing.

constant \STX = '👍';

# Burrows-Wheeler transform
sub transform (Str $s is copy) {
    note "String can't contain STX character." and exit if $s.contains: STX;
    $s = STX ~ $s;
    (^$s.chars).map({ $s.comb.list.rotate: $_ }).sort[*;*-1].join
}

# Burrows-Wheeler inverse transform
sub ɯɹoɟsuɐɹʇ (Str $s) {
    my @t = $s.comb.sort;
    @t = ($s.comb Z~ @t).sort for 1..^$s.chars;
    @t.first( *.ends-with: STX ).chop
}

# TESTING
for |<BANANA dogwood SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES>,
    'TO BE OR NOT TO BE OR WANT TO BE OR NOT?', "Oops{STX}"
    -> $phrase {
    say 'Original:            ', $phrase;
    say 'Transformed:         ', transform $phrase;
    say 'Inverse transformed: ', ɯɹoɟsuɐɹʇ transform $phrase;
    say '';
}
```

{{out}}

```txt
Original:            BANANA
Transformed:         BNN👍AAA
Inverse transformed: BANANA

Original:            dogwood
Transformed:         👍ooodwgd
Inverse transformed: dogwood

Original:            SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES
Transformed:         TEXYDST.E.IXIXIXXSSMPPS.B..E.👍.UESFXDIIOIIITS
Inverse transformed: SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES

Original:            TO BE OR NOT TO BE OR WANT TO BE OR NOT?
Transformed:         OOORREEETTRTW   BBB  ATTT   NNOOONOO👍   ?
Inverse transformed: TO BE OR NOT TO BE OR WANT TO BE OR NOT?

Original:            Oops👍
String can't contain STX character.
```



## Phix

An efficient implementation, based mainly on http://spencer-carroll.com/an-easy-to-understand-explanation-of-the-burrows-wheeler-transform/

Perhaps not ultra-fast, it takes around about ten seconds to transform and invert a 100K string. Note: requires 0.8.0+

```Phix
--demo\rosetta\burrows_wheeler.exw
--/*
The traditional method:

    7 banana$           $banana 6
    6 $banana   ===>    a$banan 5
    5 a$banan           ana$ban 3
    4 na$bana   sort    anana$b 1
    3 ana$ban           banana$ 7
    2 nana$ba   ===>    na$bana 4
    1 anana$b           nana$ba 2
                              ^ desired answer == "annb$aa"

First ignore the numbers: the desired answer is found by creating a table of all
rotations of "banana$", sorting it, and then extracting the right-hand column.

However, there is no need to actually create such a table, which could be very
expensive for long strings, instead just number them logically (admittedly that
was somewhat arbitrarily chosen to get the indexes to work out nicely, I picked
the original index of the last character), and perform a custom sort on those.

The latter effectively just recreates the rotations one character at a time until
there is a mismatch (which there always will be since there is only one $).
The left hand column is my arbitrary numbering scheme and the right hand column
is those sorted into order, which is also the indexes to the original string of
the characters that we want.

The code below uses $ as the terminator, but eg 1 (== '\#01') should be fine,
except of course for the display of that on a console.
--*/
constant terminator = '$'

function rot_sort(integer i,j, sequence s)
-- i,j are indexes of the last character, so bump before first compare.
-- eg/ie rot_sort(i,j,s) should yield compare(rotate(s,i),rotate(s,j)),
--       as in rot_sort(7,6,"banana$") == compare("banana$","$banana")
--       - but one character at a time rather than constructing both.
    integer l = length(s)
    while true do
        i = mod(i,l)+1
        j = mod(j,l)+1
        integer c = compare(s[i],s[j])
        if c!=0 then return c end if
    end while
end function

function burrows_wheeler_transform(string s)
    if find(terminator,s) then ?9/0 end if
    s &= terminator
    integer l = length(s)
    sequence t = custom_sort(routine_id("rot_sort"),tagset(l),{s})
    string res = repeat(' ',l)
    for i=1 to l do
        res[i] = s[t[i]]
    end for
    return res
end function

--/*
Inversion. The traditional method is add column and sort, seven times,
to reconstruct the table above, then pick the entry that ends with the
marker. Showing that technique in full detail here is not helpful, and
like above that would be hideously inefficient for large strings.

                $banana         1  $ (1 ) a  2
                a$banan         2  a ( 1) n  6
                ana$ban         3  a ( 2) n  7
                anana$b         4  a ( 3) b  5
                banana$         5  b      $  1
                na$bana         6  n (2 ) a  3
                nana$ba         7  n (3 ) a  4
                ^     ^            ^      ^  ^
                f     l            f      l  t

However, we already have the last column, and the first is just that
sorted alphabetically, and with just those two, we have all possible
character pairings of the original message. The trick is in figuring
out how to stitch them together in the right order. If you carefully
study the three that end in a, and the three that start in a, notice
the $banan,na$ban,nana$b parts are sorted in the same order, whether
they are prefixed with a or not. That is, the middle (parenthesised)
matching numbers are both 123, not 123 and say 231. It is quite hard
to see that being useful, but eventually the penny should drop. The
right-hand 1 with an a rotated right gives the left-hand 1, and the
same goes for 2 and 3: they are in fact links to the prior pairing.

In other words the first a in l always corresponds to the first in f,
the second to the second, and so on, and that (amazingly) forms the
order in which the pairings need to be daisy-chained together.

Try following (1->)2a->6n->3a->7n->4a->5b->$, == reverse("banana"),
in the above f and t tables.

The code below builds a queue of 'a' ({1,6,7}, built backwards) from
l (aka s), into which we pop into t the {2,3,4} of the 'a' in f, and
likewise for all other letters, forming the links for each pairing.
See the trivial step 3 scan below, then go back and stare at f and
t as shown above, and once again, eventually the penny should drop.
I will admit I had to read ten or so explanations before I got it.

--*/

function inverse_burrows_wheeler(string s)
    if find('\0',s) then ?9/0 end if -- (doable, but needs some +1s)
    integer l = length(s), c
    string f = sort(s)
    sequence q = repeat(0,256), -- queue heads (per char)
             x = repeat(0,l),   -- queue links
             t = repeat(0,l)    -- reformed/pairing links
    -- Step 1. discover/build queues (backwards)
    for i=l to 1 by -1 do
        c = s[i]
        x[i] = q[c]
        q[c] = i
    end for
    -- Step 2. reform/pop char queues into pairing links
    for i=1 to l do
        c = f[i]
        t[q[c]] = i
        q[c] = x[q[c]]
    end for
    -- Step 3. rebuild (backwards)
    c = find(terminator,f)
    string res = repeat(' ',l-1)
    for i=l-1 to 1 by -1 do
        c = t[c]        -- (first time in, skip the end marker)
        res[i] = f[c]
    end for
    return res
end function

procedure test(string src)
string enc = burrows_wheeler_transform(src),
       dec = inverse_burrows_wheeler(enc)
    printf(1,"original: %s --> %s\n inverse: %s\n",{src,enc,dec})
end procedure

test("banana")
test("dogwood")
test("TO BE OR NOT TO BE OR WANT TO BE OR NOT?")
test("SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES")
```

{{out}}

```txt

original: banana --> annb$aa
 inverse: banana
original: dogwood --> do$oodwg
 inverse: dogwood
original: TO BE OR NOT TO BE OR WANT TO BE OR NOT? --> OOORREEETTR?TW   BBB  ATTT   NNOOONOO$
 inverse: TO BE OR NOT TO BE OR WANT TO BE OR NOT?
original: SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES --> STEXYDST.E.IXXIIXXSSMPPS.B..EE.$.USFXDIIOIIIT
 inverse: SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES

```



## Python

This Python implementation sacrifices speed for simplicity: the program is short, but takes more than the linear time that would be desired in a practical implementation.

Using the STX/ETX control codes to mark the start and end of the text, and using s[i:] + s[:i] to construct the ith rotation of s, the forward transform takes the last character of each of the sorted rows.
The inverse transform repeatedly inserts r as the left column of the table and sorts the table. After the whole table is built, it returns the row that ends with ETX, minus the STX and ETX.
Ref: [https://www.codespeedy.com/burrows-wheeler-transform-in-python/ Burrows Wheeler Transform in Python]


```Python

def bwt(s):
    """Apply Burrows-Wheeler transform to input string."""
    assert "\002" not in s and "\003" not in s, "Input string cannot contain STX and ETX characters"
    s = "\002" + s + "\003"  # Add start and end of text marker
    table = sorted(s[i:] + s[:i] for i in range(len(s)))  # Table of rotations of string
    last_column = [row[-1:] for row in table]  # Last characters of each row
    return "".join(last_column)  # Convert list of characters into string


def ibwt(r):
    """Apply inverse Burrows-Wheeler transform."""
    table = [""] * len(r)  # Make empty table
    for i in range(len(r)):
        table = sorted(r[i] + table[i] for i in range(len(r)))  # Add a column of r
    s = [row for row in table if row.endswith("\003")][0]  # Find the correct row (ending in ETX)
    return s.rstrip("\003").strip("\002")  # Get rid of start and end markers

```



## REXX

Programming note:   a little bit of code was added to support more (legal) characters in the input string for the '''BWT'''

function.   The error recovery and error messages are rudimentary when an illegal character in the input is detected.

```rexx
/*REXX program performs a  Burrows─Wheeler transform  (BWT)  on a character string(s).  */
$.=                                              /*the default text for (all) the inputs*/
parse arg $.1                                    /*obtain optional arguments from the CL*/
if $.1=''  then do;  $.1= "banana"               /*Not specified?  Then use the defaults*/
                     $.2= "BANANA"
                     $.3= "appellee"
                     $.4= "dogwood"
                     $.5= "TO BE OR NOT TO BE OR WANT TO BE OR NOT?"
                     $.6= "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES"
                     $.7= "^ABC|"
                     $.7= "bad─bad thingy"'fd'x  /* ◄─── this string can't be processed.*/
                end
                                                 /* [↑]  show blank line between outputs*/
       do t=1  while $.t\='';  if t\==1 then say /*process each of the inputs (or input)*/
       out=  BWT($.t)                            /*invoke the  BWT  function, get result*/
       ori= iBWT(out)                            /*   "    "  iBWT      "      "     "  */
       say '   input ───► '   $.t                /*display    input      string to term.*/
       say '  output ───► '   out                /*   "       output        "    "   "  */
       say 'original ───► '   ori                /*   "    reconstituted    "    "   "  */
       end    /*t*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
BWT:   procedure expose ?.; parse arg y,,$       /*obtain the input;  nullify $ string. */
       ?.1= 'fd'x;          ?.2= "fc"x           /*assign the  STX  and  ETX  strings.  */
         do i=1  for 2                           /* [↓]  check for invalid input string.*/
         _= verify(y, ?.i, 'M');  if _==0  then iterate;        er= '***error***  BWT: '
         say er "invalid input: "    y
         say er 'The input string contains an invalid character at position' _"."; exit 13
         end   /*i*/                            /* [↑]  if error,  perform a hard exit.*/
       y= ?.1 || y || ?.2                        /*obtain the input;  add a fence to it.*/
       L= length(y);            m= L-1           /*get the length of new text; get  L-1.*/
       @.1= y                                    /*define the first element of the table*/
                    do j=2  for m;  _= j-1       /*now, define the rest of the elements.*/
                    @.j= right(@._,1)left(@._,m) /*construct a table from the  Y  input.*/
                    end   /*j*/                  /* [↑]  each element: left & right part*/
       call cSort L                              /*invoke lexicographical sort for array*/
                    do k=1  for L                /* [↓]  construct the answer from array*/
                    $=$ || right(@.k, 1)         /*build the answer from each of  ···   */
                    end   /*k*/                  /* ··· the array's right─most character*/
       return $                                  /*return the constructed answer.       */
/*──────────────────────────────────────────────────────────────────────────────────────*/
iBWT:  procedure expose ?.; parse arg y,,@.      /*obtain the input;  nullify @. string.*/
       L= length(y)                              /*compute the length of the input str. */
                   do   j=1  for L               /* [↓]  step through each input letters*/
                     do k=1  for L               /* [↓]  step through each row of table.*/
                     @.k= substr(y, k, 1) || @.k /*construct a row of the table of chars*/
                     end   /*k*/                 /* [↑]  order of table row is inverted.*/
                   call cSort L                  /*invoke lexicographical sort for array*/
                   end    /*j*/                  /* [↑]  answer is the penultimate entry*/
         do #=1
         if right(@.#, 1)==?.2  then return substr(@.#, 2, L-2)  /*return correct result*/
         end   /*#*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
cSort: procedure expose @.;  parse arg n;  m=n-1 /*N: is the number of @ array elements.*/
           do m=m  for m  by -1  until ok;  ok=1 /*keep sorting the  @ array until done.*/
             do j=1  for m;  k= j+1;   if @.j<<=@.k  then iterate   /*elements in order?*/
             _= @.j;  @.j= @.k;  @.k= _;   ok= 0 /*swap two elements;  flag as not done.*/
             end   /*j*/
           end     /*m*/;       return
```

{{out|output|text=  when using the default inputs:}}

```txt

   input ───►  banana
  output ───►  bnn²aaaⁿ
original ───►  banana

   input ───►  BANANA
  output ───►  BNN²AAAⁿ
original ───►  BANANA

   input ───►  appellee
  output ───►  ²lpelepaeⁿ
original ───►  appellee

   input ───►  dogwood
  output ───►  ²ooodwgdⁿ
original ───►  dogwood

   input ───►  TO BE OR NOT TO BE OR WANT TO BE OR NOT?
  output ───►  OOORREEETTRTW   BBB  ATTT   NNOOONOO²   ?ⁿ
original ───►  TO BE OR NOT TO BE OR WANT TO BE OR NOT?

   input ───►  SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES
  output ───►  TEXYDST.E.IXIXIXXSSMPPS.B..E.².UESFXDIIOIIITSⁿ
original ───►  SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES

***error***  BWT:  invalid input:  bad─bad thingy²
***error***  BWT:  The input string contains an invalid character at position 15.

```



## Scala

{{trans|Kotlin}}

```scala
import scala.collection.mutable.ArrayBuffer

object BWT {
  val STX = '\u0002'
  val ETX = '\u0003'

  def bwt(s: String): String = {
    if (s.contains(STX) || s.contains(ETX)) {
      throw new RuntimeException("String can't contain STX or ETX")
    }
    var ss = STX + s + ETX
    var table = new ArrayBuffer[String]()
    (0 until ss.length).foreach(_ => {
      table += ss
      ss = ss.substring(1) + ss.charAt(0)
    })
    table.sorted.map(a => a.last).mkString
  }

  def ibwt(r: String): String = {
    var table = Array.fill(r.length)("")
    (0 until r.length).foreach(_ => {
      (0 until r.length).foreach(i => {
        table(i) = r.charAt(i) + table(i)
      })
      table = table.sorted
    })
    table.indices.foreach(i => {
      val row = table(i)
      if (row.last == ETX) {
        return row.substring(1, row.length - 1)
      }
    })
    ""
  }

  def makePrintable(s: String): String = {
    s.replace(STX, '^').replace(ETX, '|')
  }

  def main(args: Array[String]): Unit = {
    val tests = Array("banana",
      "appellee",
      "dogwood",
      "TO BE OR NOT TO BE OR WANT TO BE OR NOT?",
      "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES",
      "\u0002ABC\u0003"
    )

    tests.foreach(test => {
      println(makePrintable(test))
      print(" --> ")

      try {
        val t = bwt(test)
        println(makePrintable(t))
        val r = ibwt(t)
        printf(" --> %s\n", r)
      } catch {
        case e: Exception => printf("ERROR: %s\n", e.getMessage)
      }

      println()
    })
  }
}
```

{{out}}

```txt
banana
 --> |annb^aa
 --> banana

appellee
 --> |e^elplepa
 --> appellee

dogwood
 --> |do^oodwg
 --> dogwood

TO BE OR NOT TO BE OR WANT TO BE OR NOT?
 --> |?OOORREEETTRTW   BBB  ATTT   NNOOONOO^
 --> TO BE OR NOT TO BE OR WANT TO BE OR NOT?

SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES
 --> |STEXYDST.E.IXXIIXXSSMPPS.B..EE.^.USFXDIIOIIIT
 --> SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES

^ABC|
 --> ERROR: String can't contain STX or ETX
```



## Sidef

{{trans|Python}}

```ruby
class BurrowsWheelerTransform (String L = "\002") {

    method encode(String s) {
        assert(!s.contains(L), "String cannot contain `#{L.dump}`")
        s = (L + s)
        s.len.of{|i| s.substr(i) + s.substr(0, i) }.sort.map{.last}.join
    }

    method decode(String s) {
        var t = s.len.of("")
        var c = s.chars
        { t = (c »+« t).sort } * s.len
        t.first { .begins_with(L) }.substr(L.len)
    }
}

var tests = [
    "banana", "appellee", "dogwood", "TOBEORNOTTOBEORTOBEORNOT"
    "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES",
]

var bwt = BurrowsWheelerTransform(L: '$')

tests.each { |str|
    var enc = bwt.encode(str)
    var dec = bwt.decode(enc)
    say "BWT(#{dec.dump}) = #{enc.dump}"
    assert_eq(str, dec)
}
```

{{out}}

```txt

BWT("banana") = "annb$aa"
BWT("appellee") = "e$elplepa"
BWT("dogwood") = "do$oodwg"
BWT("TOBEORNOTTOBEORTOBEORNOT") = "TOOOBBBRRTTTEEENNOOOOR$TO"
BWT("SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES") = "STEXYDST.E.IXXIIXXSSMPPS.B..EE.$.USFXDIIOIIIT"

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    ReadOnly STX As Char = Chr(&H2)
    ReadOnly ETX As Char = Chr(&H3)

    Sub Rotate(Of T)(a As T())
        Dim o = a.Last
        For i = a.Length - 1 To 1 Step -1
            a(i) = a(i - 1)
        Next
        a(0) = o
    End Sub

    Private Function Compare(s1 As String, s2 As String) As Integer
        Dim i = 0
        While i < s1.Length AndAlso i < s2.Length
            Dim a = s1(i)
            Dim b = s2(i)
            If a < b Then
                Return -1
            End If
            If b < a Then
                Return 1
            End If
            i += 1
        End While
        If s1.Length < s2.Length Then
            Return -1
        End If
        If s2.Length < s1.Length Then
            Return 1
        End If
        Return 0
    End Function

    Function Bwt(s As String) As String
        If s.Any(Function(c) c = STX OrElse c = ETX) Then
            Throw New ArgumentException("Input can't contain STX or ETX")
        End If
        Dim ss = (STX + s + ETX).ToCharArray
        Dim table As New List(Of String)
        For i = 0 To ss.Length - 1
            table.Add(New String(ss))
            Rotate(ss)
        Next
        table.Sort(Function(a As String, b As String) Compare(a, b))
        Return New String(table.Select(Function(a) a.Last).ToArray)
    End Function

    Function Ibwt(r As String) As String
        Dim len = r.Length
        Dim sa(len - 1) As String
        Dim table As New List(Of String)(sa)
        For i = 0 To len - 1
            For j = 0 To len - 1
                table(j) = r(j) + table(j)
            Next
            table.Sort(Function(a As String, b As String) Compare(a, b))
        Next
        For Each row In table
            If row.Last = ETX Then
                Return row.Substring(1, len - 2)
            End If
        Next
        Return ""
    End Function

    Function MakePrintable(s As String) As String
        Return s.Replace(STX, "^").Replace(ETX, "|")
    End Function

    Sub Main()
        Dim tests As String() = {
            "banana",
            "appellee",
            "dogwood",
            "TO BE OR NOT TO BE OR WANT TO BE OR NOT?",
            "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES",
            STX + "ABC" + ETX
        }

        For Each test In tests
            Console.WriteLine(MakePrintable(test))
            Console.Write(" --> ")

            Dim t = ""
            Try
                t = Bwt(test)
                Console.WriteLine(MakePrintable(t))
            Catch ex As Exception
                Console.WriteLine("ERROR: {0}", ex.Message)
            End Try

            Dim r = Ibwt(t)
            Console.WriteLine(" --> {0}", r)
            Console.WriteLine()
        Next
    End Sub

End Module
```

{{out}}

```txt
banana
 --> |annb^aa
 --> banana

appellee
 --> |e^elplepa
 --> appellee

dogwood
 --> |do^oodwg
 --> dogwood

TO BE OR NOT TO BE OR WANT TO BE OR NOT?
 --> |?OOORREEETTRTW   BBB  ATTT   NNOOONOO^
 --> TO BE OR NOT TO BE OR WANT TO BE OR NOT?

SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES
 --> |STEXYDST.E.IXXIIXXSSMPPS.B..EE.^.USFXDIIOIIIT
 --> SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES

^ABC|
 --> ERROR: Input can't contain STX or ETX
 -->
```



## zkl


```zkl
class BurrowsWheelerTransform{
   fcn init(chr="$"){ var special=chr; }
   fcn encode(str){
      _assert_(not str.holds(special), "String cannot contain char \"%s\"".fmt(special) );
      str=str.append(special);
      str.len().pump(List().merge,'wrap(n){ String(str[n,*],str[0,n]) })
      .pump(String,T("get",-1));	// last char of each "permutation"
   }
   fcn decode(str){
      table:=List.createLong(str.len(),"");	// ("",""..), mutable
      do(str.len()){
	 foreach n in (str.len()){ table[n]=str[n] + table[n] }
	 table.sort();
      }   // --> ("$dogwood","d$dogwoo","dogwood$",...)
      table.filter1("%s*".fmt(special).glob)[1,*];  // str[0]==$, often first element
   }
}
```


```zkl
BWT:=BurrowsWheelerTransform();
//BWT.encode("$"); // --> assert(bbb.zkl:25): String cannot contain char "$"

tests:=T(
    "banana", "appellee", "dogwood", "TO BE OR NOT TO BE OR WANT TO BE OR NOT?",
    "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES",);
foreach test in (tests){
   enc:=BWT.encode(test);
   println("%s\n  -->%s\n  -->%s".fmt(test,enc,BWT.decode(enc)));
}
```

{{out}}

```txt

banana
  -->annb$aa
  -->banana
appellee
  -->e$elplepa
  -->appellee
dogwood
  -->do$oodwg
  -->dogwood
TO BE OR NOT TO BE OR WANT TO BE OR NOT?
  -->OOORREEETTR?TW   BBB  ATTT   NNOOONOO$
  -->TO BE OR NOT TO BE OR WANT TO BE OR NOT?
SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES
  -->STEXYDST.E.IXXIIXXSSMPPS.B..EE.$.USFXDIIOIIIT
  -->SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES

```

