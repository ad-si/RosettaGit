+++
title = "Largest number divisible by its digits"
description = ""
date = 2019-10-02T03:57:03Z
aliases = []
[extra]
id = 21584
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "awk",
  "c",
  "clojure",
  "cpp",
  "csharp",
  "d",
  "factor",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "mathematica",
  "perl",
  "perl_6",
  "phix",
  "prolog",
  "python",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "sidef",
  "zkl",
]
+++

## Task

Find the largest base 10 integer whose digits are all different,   and   is evenly divisible by each of its individual digits.


These numbers are also known as   '''Lynch-Bell numbers''',   numbers   '''n'''   such that the
(base ten) digits are all different (and do not include zero)   and   '''n'''   is divisible by each of its individual digits.


For example: 135 is evenly divisible by 1, 3 and 5.

Note that the digit zero (0) can not be in the number as integer division by zero is undefined. The digits must all be unique so a base 10 number will have at most 9 digits.

Feel free to use analytics and clever algorithms to reduce the search space your example needs to visit, but it must do an actual search. (Don't just feed it the answer and verify it is correct.)


;Stretch goal:
Do the same thing for hexadecimal.


;Also see:
*   The OEIS sequence:   [http://oeis.org/A115569 A115569: Lynch-Bell numbers].





## AWK


### Base 10

Ruling out 9- and 8-digit numbers (see first paragraph in the Perl 6 example), we are looking for 7-digit numbers. In order to be a solution such a number has to be divisible by 12 = 2*2*3, because its digits must contain at least 2 of the numbers 2, 4, 6, 8 (leading to a factor of 2*2) and its digits must contain at least one of the numbers 3, 6, 9 (leading to a factor of 3).

The program does a brute force search, starting with the largest possible 7-digit number and iterates over all smaller numbers divisible by 12. It checks for each iteration, if the number in question consists of different digits and is divisible by those digits.

```AWK
# Usage: gawk -f LARGEST_NUMBER_DIVISIBLE_BY_ITS_DIGITS_10.AWK
BEGIN {
    base = 10
    comdiv = 12
    startn = 9876543
    stopn = 1000000
    solve(startn, stopn)
}
function solve(startn, stopn,    n, d) {
    for (n = startn - startn % comdiv; n > stopn; n -= comdiv) {
        if (hasuniqedigits(n)) {
            # Check divisibility of n by all its digits
            for (d = 2; d < base; d++) {
                if ((dcount[d]) && (n % d)) {
                    break
                }
            }
            if (d == base) {
                printf("%d\n", n)
                return
            }
        }
    }
}
function hasuniqedigits(n,    d) {
    # Returns 1, if n consists of unique digits in range 1..(base-1)
    # The array dcount stores the count (up to 1) of those digits
    for (d = 1; d < base; d++)
        dcount[d] = 0
    while (n) {
        d = n % base
        if ((d == 0) || (++dcount[d] > 1))
            return 0
        n = int(n / base)
    }
    return 1
}
```

```txt

9867312

```



### Base 16

In the hexadecimal case we cannot rule out 15-digit numbers, thus all digits from 1 to f (hex) are present. The number has to be divisible by all its digits, therefore it has to be divisible by the least common multiple of the numbers 1, 2, 3, ..., 15 (360360).

AWK does not support arbitrary long integers, so we have to use an array of digits for its representation. It makes use of functions hexmod (modulus) and hexsub (subtraction), which act on an array.

The program does a brute force search, starting with the largest possible 15-digit number and iterates over all smaller numbers divisible by 360360. It checks for each iteration, if the number in question consists of different digits (by construction it is then also divisible by its digits).


```AWK
# Usage: GAWK -f LARGEST_NUMBER_DIVISIBLE_BY_ITS_DIGITS_16.AWK
BEGIN {
    base = 16
    size = 15
    # startn = FEDCB A9876 54321 (hex)
    for (i = 1; i <= size; i++) {
        startn[i] = i
    }
    comdiv = 360360 # lcm(1..15)
    solve(startn)
}
function solve(n,    r, i) {
    r = hexmod(n, comdiv)
    hexsub(n, r)
    while (n[size] > 0) {
        if (hasuniqedigits(n)) {
            for (i = size; i > 0; i--)
                printf("%0x", n[i])
            printf("\n")
            return
        }
        hexsub(n, comdiv)
    }
}
function hasuniqedigits(n,    d, i) {
    # Return 1, if n is an array of unique digits in range 1..(base-1)
    # The array dcount stores the count (up to 1) of those digits
    for (d = 1; d < base; d++)
        dcount[d] = 0
    for (i = 1; i <= size; i++) {
        d = n[i]
        if ((d == 0) || (++dcount[d] > 1))
            return 0
    }
    return 1
}
function hexmod(n, k,    i, r) {
    # Return n mod k, where n is an array and k is a number
    for (i = size; i > 0; i--) {
        r = (r * base + n[i]) % k
    }
    return r
}
function hexsub(n, m) {
    # Calculate n = n - m, where n is an array and m is a number
    for (i = 1; m && (i <= size); i++) {
        n[i] -= m % base
        m = int(m / base)
        if (n[i] < 0) {
            n[i] += base
            m++
        }
    }
}
```

```txt

fedcb59726a1348

```


## C


### Base 10

The number can't contain 0 and 5, 0 is obvious, 5 because the number must end in 5 for it to be a multiple of that number and if that happens, all the even digits are ruled out which severely reduces the number's length since the other condition is that all digits must be unique. However, this means the number must be even and thus end only in 2,4,6,8. This speeds up the search by a factor of 2. The same approach when applied to hexadecimals takes a very long, long time.

```C

#include<stdio.h>

int main()
{
	int num = 9876432,diff[] = {4,2,2,2},i,j,k=0;
	char str[10];

		start:snprintf(str,10,"%d",num);

		for(i=0;str[i+1]!=00;i++){
			if(str[i]=='0'||str[i]=='5'||num%(str[i]-'0')!=0){
				num -= diff[k];
				k = (k+1)%4;
				goto start;
			}
			for(j=i+1;str[j]!=00;j++)
				if(str[i]==str[j]){
					num -= diff[k];
					k = (k+1)%4;
					goto start;
			}
		}

	printf("Number found : %d",num);
	return 0;
}

```

Output:

```txt

Number found : 9867312

```



### Base 16

```C>#include<stdio.h

#include<string.h>

#define TRUE 1
#define FALSE 0

typedef char bool;

typedef unsigned long long uint64;

bool div_by_all(uint64 num, char digits[], int len) {
    int i, d;
    for (i = 0; i < len; ++i) {
        d = digits[i];
        d = (d <= '9') ? d - '0' : d - 'W';
        if (num % d != 0) return FALSE;
    }
    return TRUE;
}

int main() {
    uint64 i, magic = 15 * 14 * 13 * 12 * 11;
    uint64 high = 0xfedcba987654321 / magic * magic;
    int j, len;
    char c, *p, s[17], sd[16], found[16];

    for (i = high; i >= magic; i -= magic) {
        if (i % 16 == 0) continue;   // can't end in '0'
        snprintf(s, 17, "%llx", i);  // always generates lower case a-f
        if (strchr(s, '0') - s >= 0) continue; // can't contain '0'
        for (j = 0; j < 16; ++j) found[j] = FALSE;
        len = 0;
        for (p = s; *p; ++p) {
            if (*p <= '9') {
                c = *p - '0';
            } else {
                c = *p - 87;
            }
            if (!found[c]) {
                found[c] = TRUE;
                sd[len++] = *p;
            }
        }
        if (len != p - s) {
            continue;  // digits must be unique
        }
        if (div_by_all(i, sd, len)) {
            printf("Largest hex number is %llx\n", i);
            break;
        }
    }
    return 0;
}
```


```txt

Largest hex number is fedcb59726a1348

```



## C++

###  Base 10


```cpp
#include <iostream>
#include <sstream>
#include <set>

bool checkDec(int num) {
    std::set<int> set;

    std::stringstream ss;
    ss << num;
    auto str = ss.str();

    for (int i = 0; i < str.size(); ++i) {
        char c = str[i];
        int d = c - '0';
        if (d == 0) return false;
        if (num % d != 0) return false;
        if (set.find(d) != set.end()) {
            return false;
        }
        set.insert(d);
    }

    return true;
}

int main() {
    for (int i = 98764321; i > 0; i--) {
        if (checkDec(i)) {
            std::cout << i << "\n";
            break;
        }
    }

    return 0;
}
```

```txt
9867312
```


## C#
###  Base 10


```c#
using System;
using System.Collections.Generic;
using System.Linq;

namespace LargestNumber {
    class Program {
        static bool ChkDec(int num) {
            HashSet<int> set = new HashSet<int>();

            return num.ToString()
                .Select(c => c - '0')
                .All(d => (d != 0) && (num % d == 0) && set.Add(d));
        }

        static void Main() {
            int result = Enumerable.Range(0, 98764321)
                .Reverse()
                .Where(ChkDec)
                .First();
            Console.WriteLine(result);
        }
    }
}
```

```txt
9867312
```



## Clojure


###  Base Agnostic


This is a generic solution that works for any number base. Just change the line '''(def the_base 16)'''. The performance may be questionable for large bases which do not have a Lynch-Bell number using all digits.

Rather than searching through all numbers with unique digits, a space whose size verges on N factorial,
this algorithm instead works with the non-empty sets of non-zero digits, a space of size 2^(N-1) - 1.
For a given subset, it finds the least common multiple for that subset and examines each multiple of the LCM which is between
the largest and smallest positive numbers that can be constructed using each digit from that subset exactly once.

```clojure

(require '[clojure.string :as str]) ;'
(def the_base 16)

;A sequence named digits containing the non-zero digits for the current number base.
(def digits (rest (range the_base)))

;A container for the digits which are prime.
(def primes [])

;Populate the primes sequence with the primes less than the current base.
(for [n digits] (if (= 1 (count (filter (fn[m] (and (< m n) (= 0 (mod n m)))) digits)) ) (def primes (conj primes n))))

;Determines the highest power of a given prime p that divides a given integer n.
(defn duplicity [n p partial] (if (= 0 (mod n p)) (duplicity (/ n p) p (conj partial p)) partial))

;Constructs the prime factorization of a given integer.
(defn factorize [n] (let [a (flatten (for [p (filter #(< % n) primes)]
  (remove #(= 1 %) (duplicity n p [1]))))] (if (= 0 (count a)) (lazy-seq [n]) a) ))

;Determines the number of times a given number appears in a given sequence of numbers.
(defn multiplicity [s n] (count (filter #(= n %) s)))

;Combines two sequence two create their "union" in the sense that in the resulting sequence
;each element from each sequence is uniquely represented and no smaller sequence would suffice.
;For example if one sequence contains two A's and other contains three A's, then the result will contain three A's.
;This is used to generate representations of prime factorizations and to construct least common multiples from them.
(defn combine [x y] (concat x (flatten (for [w (dedupe y)] (repeat (- (multiplicity y w) (multiplicity x w)) w) ))))

;deterimes the lcm least common multiple for a set of digits.
(defn lcm [s] (reduce * (reduce combine (map factorize s))))

;Retuns x^n.
(defn exp [x n] (reduce * (repeat n x)))

;Generates all non-empty subsequences for a sequence.
(defn non_empty_subsets [s] (for [x (reverse (rest (range (exp 2 (count s)))))]
  (remove nil? (for [i (range (count s))] (if (bit-test x i) (nth s i))))))


;Generates from a given sequence of digits in the current base the number that is s[0]s[1]s[2]...s[n].
;More generally, produces s[0]*the_base^n + s[1]*the_base^(n-1) + ... + s[n-1]*the_base^1 + s[n]*the_base^0
;for an arbitrary sequence of numbers.
(defn power_up [s] (reduce + (loop [idx (- (count s) 1) s_next s]
  (if (zero? idx) s_next (recur (dec idx) (map-indexed #(if (< %1 idx) (* %2 the_base) %2) s_next))))))

;Here is an alternative version of power_up that could be more efficient as it does not repeatedly recalculate powers of the base.
;Instead it calculates the dot product of s with a pre-populated sequence of powers of the base.
;Calculates the dot product of two vectors/sequences
;(defn dot [xs ys] (reduce + (map * xs ys)))
;(def places (map #(exp the_base %) (range the_base)))
;(defn power_up [s] (dot s (reverse (take (count s) places))))


;Returns the largest integer which contains each item from a given sequence exactly once as a digit.
(defn max_for_digits [s] (power_up (sort #(> %1 %2) s)))

;Returns the smallest non-negative integer which contains each item from a given sequence exactly once as a digit.
(defn min_for_digits [s] (power_up (sort #(< %1 %2) s)))

;calculate the logarithm of the input in the current base.
(defn log_base [x] (/ (Math/log x) (Math/log the_base)))

;Removes the zeros from a sequence
(defn remove_zeros [s] (remove #(= % 0) s))

;Returns the largest integer that is a multiple of a given integer and does not exceed another given integer.
(defn first_multiple_not_after [n ub] (loop [m ub] (if (= 0 (mod m n)) m (recur (dec m)))))

;creates a representation in the current base of a positive integer as a sequence listing the digits for the number in the base.
(defn representation [n] (let [full_power (int (log_base n))]
  (loop [power full_power place (exp the_base full_power) rep [] rem n ] (if (= power -1) rep
    (recur (dec power) (/ place the_base) (conj rep (int (/ rem place))) (- rem (* place (int (/ rem place)))))))))

;determines if a given number is exactly comprised of a given set of digits.
(defn digit_qualifies [m s] (let [rep_m (representation m)] (= (sort s) (sort rep_m))))

;Returns a sequence containing the largest Lynch-Bell number for the current base and a given sequence of digits
;or an empty sequence if there is none.
(defn find_s_largest_lb [s] (let [lb (min_for_digits s)] (let [m (lcm s)]
  (loop [v (first_multiple_not_after m (max_for_digits s))] (if (< v lb) []
    (if (digit_qualifies v s) (representation v) (recur (- v m))))))))

;Finds the largest Lynch-Bell number for the current base by looking for the largest for all subsets of a given size
;and picking the largest from those working from the largest size (most digits) to the smallest.
(defn find_largest_lb [] (let [subsets (non_empty_subsets (reverse digits))]
  (loop [s_size (- the_base 1)] (let [hits (remove #(= (count %) 0) (map find_s_largest_lb (filter #(= (count %) s_size) subsets)))]
    (if (pos? (count hits)) (first (sort #(first (remove_zeros (map - %2 %1))) hits)) (recur (dec s_size)))))))

;Converts small integers to hexidecimal digits.
;This isn't being used but could be leveraged to make output that looks normal for base 16.
(defn hex_digit [v] (case v 15 "F" 14 "E" 13 "D" 12 "C" 11 "B" 10 "A" (str v)))

(find_largest_lb)

```


```txt

[15 14 13 12 11 5 9 7 2 6 10 1 3 4 8]  (base 16)
[10 9 8 7 6 2 4 1 3]  (base 11)
[9 8 6 7 3 1 2]  (base 10)

```



## D

###  Base 10


```d
import std.algorithm.iteration : filter, map;
import std.algorithm.searching : all;
import std.conv : to;
import std.range : iota;
import std.stdio : writeln;

bool chkDec(int num) {
    int[int] set;

    return num
        .to!string
        .map!(c => c.to!int - '0')
        .all!(d => (d != 0) && (num % d == 0) && set[d]++ < 1);
}

auto lcm(R)(R r) {
    return r.reduce!((a,b) => a * b / gcd(a,b));
}

void main() {
    // base 10
    iota(98764321, 0, -1)
        .filter!chkDec
        .front
        .writeln;
}
```

```txt
9867312
```



## Factor


### Base 10

This program works by filtering all the 8-digit permutations (of which there are only ~40,000) for all-digit-divisibility, and upon finding none, it will then generate the 7-digit combinations (of which there are 8) of the 8 possible digits, and then filter all permutations of the 8 combinations for all-digit-divisibility. Upon finding many, it will simply select the largest element which is our answer. If there hadn't been any 7-digit solutions, it would have gone down to six and then five, etc.

```factor
USING: io kernel math math.combinatorics math.parser math.ranges
sequences tools.time ;
IN: rosetta-code.largest-divisible

: all-div? ( seq -- ? )
    [ string>number ] [ string>digits ] bi [ mod ] with map
    sum 0 = ;

: n-digit-all-div ( n -- seq )
    "12346789" swap <combinations>
    [ [ all-div? ] filter-permutations ] map concat ;

: largest-divisible ( -- str )
    8 [ dup n-digit-all-div dup empty? ] [ drop 1 - ] while
    nip supremum ;

: largest-divisible-demo ( -- )
    [ largest-divisible print ] time ;

MAIN: largest-divisible-demo
```

```txt

9867312
Running time: 0.07224931499999999 seconds

```



## Go

### base 10



```go
package main

import (
    "fmt"
    "strconv"
    "strings"
)

func divByAll(num int, digits []byte) bool {
    for _, digit := range digits {
        if num%int(digit-'0') != 0 {
            return false
        }
    }
    return true
}

func main() {
    magic := 9 * 8 * 7
    high := 9876432 / magic * magic
    for i := high; i >= magic; i -= magic {
        if i%10 == 0 {
            continue // can't end in '0'
        }
        s := strconv.Itoa(i)
        if strings.ContainsAny(s, "05") {
            continue // can't contain '0'or '5'
        }
        var set = make(map[byte]bool)
        var sd []byte // distinct digits
        for _, b := range []byte(s) {
            if !set[b] {
                set[b] = true
                sd = append(sd, b)
            }
        }
        if len(sd) != len(s) {
            continue // digits must be unique
        }
        if divByAll(i, sd) {
            fmt.Println("Largest decimal number is", i)
            return
        }
    }
}
```


```txt

Largest decimal number is 9867312

```



### base 16



```go
package main

import (
    "fmt"
    "strconv"
    "strings"
)

func divByAll(num int64, digits []byte) bool {
    for _, digit := range digits {
        var d int64
        if digit <= '9' {
            d = int64(digit - '0')
        } else {
            d = int64(digit - 'W')
        }
        if num%d != 0 {
            return false
        }
    }
    return true
}

func main() {
    var magic int64 = 15 * 14 * 13 * 12 * 11
    high := 0xfedcba987654321 / magic * magic
    for i := high; i >= magic; i -= magic {
        if i%16 == 0 {
            continue // can't end in '0'
        }
        s := strconv.FormatInt(i, 16) // always generates lower case a-f
        if strings.IndexByte(s, '0') >= 0 {
            continue // can't contain '0'
        }
        var set = make(map[byte]bool)
        var sd []byte // distinct digits
        for _, b := range []byte(s) {
            if !set[b] {
                set[b] = true
                sd = append(sd, b)
            }
        }
        if len(sd) != len(s) {
            continue // digits must be unique
        }
        if divByAll(i, sd) {
            fmt.Printf("Largest hex number is %x\n", i)
            return
        }
    }
}
```


```txt

Largest hex number is fedcb59726a1348

```



## Haskell



### base 10

Using the analysis provided in the Perl 6 (base 10) example:


```haskell
import Data.List (maximumBy, permutations, delete)
import Data.Ord (comparing)

unDigits :: [Int] -> Int
unDigits = foldl ((+) . (10 *)) 0

ds :: [Int]
ds = [1, 2, 3, 4, 6, 7, 8, 9] -- 0 (and thus 5) are both unworkable

lcmDigits :: Int
lcmDigits = foldr1 lcm ds -- 504

sevenDigits :: [[Int]]
sevenDigits = (`delete` ds) <$> [1, 4, 7] -- Dropping any one of these three

main :: IO ()
main =
  print $
  maximumBy
    (comparing
       (\x ->
           if rem x lcmDigits == 0 -- Checking for divisibility by all digits
             then x
             else 0))
    (unDigits <$> concat (permutations <$> sevenDigits))
```

Test run from inside the Atom editor:

```txt
9867312
[Finished in 0.395s]
```



### base 16

First member of a descending sequence of multiples of 360360 that uses the full set of 15 digits when expressed in hex.

```haskell
import Data.Set (fromList)
import Numeric (showHex)

lcmDigits :: Int
lcmDigits = foldr1 lcm [1 .. 15] -- 360360

upperLimit :: Int
upperLimit =
  let allDigits = 0xfedcba987654321
  in allDigits - rem allDigits lcmDigits

main :: IO ()
main =
  print $
  head
    (filter ((15 ==) . length . fromList) $
     (`showHex` []) <$> [upperLimit,upperLimit - lcmDigits .. 1])
```

Test run from inside the Atom editor:

```txt
"fedcb59726a1348"
[Finished in 2.319s]
```



## J

The 536 values found---all base 10 numbers that are divisible by their digits without repetition---are sorted descending, hence 9867312 is the greatest number divisible by its digits in base 10.

```J

   Filter =: (#~`)(`:6)
   combinations =: <@#"1~ [: #: [: i. 2 ^ #
   permutations =: A.&i.~ !
   f =: [: \:~ _ ". [: ; [: ({~ permutations@#)L:_1 }.@combinations
   test =: 0 = ([: +/ (|~ 10&#.inv))&>

   test Filter f '12346789'
9867312 9812376 9782136 9781632 9723168 9718632 9678312 9617832 9617328 9283176 9278136 9237816 9231768 9182376 9176832 9176328 9163728 8973216 8912736 8796312 8731296 8617392 8367912 8312976 8219736 8176392 8163792 8123976 7921368 7916832 7916328 7892136 ...

```

Working in base 16 using the largest possible solution also a multiple of the least common multiple, subtract the LCM until all the digits appear.

```J

   NB. 16bfedcba987654321 loses precision and so we need to work in extended data type

   [ HEX_DIGITS =: >: i. _15x
15 14 13 12 11 10 9 8 7 6 5 4 3 2 1

   [ LCM =: *./ HEX_DIGITS
360360

   ] START =: <.&.(%&LCM)16#.HEX_DIGITS
1147797409030632360

   Until =: conjunction def 'u^:(0-:v)^:_'
   assert 9 -: >:Until(>&8)1

   test=: 0 -.@e. HEX_DIGITS e. 16&#.inv

   [ SOLUTION =: -&LCM Until test START
1147797065081426760

   '16b' , (16 #.inv SOLUTION) { Num_j_ , 26 }. Alpha_j_
16bfedcb59726a1348


```



## Java

###  Base 10

Using the analysis provided in the Perl 6 (base 10) example:

```java
public class LynchBell {

    static String s = "";

    public static void main(String args[]) {
        //Highest number with unique digits (no 0 or 5)
        int i = 98764321;
        boolean isUnique = true;
        boolean canBeDivided = true;
        while (i>0) {
            s = String.valueOf(i);
            isUnique = uniqueDigits(i);
            if (isUnique) {
                //Number has unique digits
                canBeDivided = testNumber(i);
                if(canBeDivided) {
                    System.out.println("Number found: " + i);
                    i=0;
                }
            }
            i--;
        }
    }

    public static boolean uniqueDigits(int i) {
        //returns true, if unique digits, false otherwise
        for (int k = 0; k<s.length();k++) {
            for(int l=k+1; l<s.length();l++) {
                if(s.charAt(l)=='0' || s.charAt(l)=='5') {
                    //0 or 5 is a digit
                    return false;
                }
                if(s.charAt(k) == s.charAt(l)) {
                    //non-unique digit
                    return false;
                }
            }
        }
        return true;
    }

    public static boolean testNumber(int i) {
        //Tests, if i is divisible by all its digits (0 is not a digit already)
        int j = 0;
        boolean divisible = true;
        // TODO: divisible by all its digits
        for (char ch: s.toCharArray()) {
            j = Character.getNumericValue(ch);
            divisible = ((i%j)==0);
            if (!divisible) {
                return false;
            }
        }
        return true;
    }
}
```


```txt
Number found: 9867312
```



## Julia

###  Base 10

```julia
function main()
    num = 9876432
    dif = [4, 2, 2, 2]
    local k = 1
    @label start
    local str = dec(num)
    for (i, ch) in enumerate(str)
        if ch in ('0', '5') || num % (ch - '0') != 0
            num -= dif[k]
            k = (k + 1) % 4 + 1
            @goto start
        end
        for j in i+1:endof(str)
            if str[i] == str[j]
                num -= dif[k]
                k = (k + 1) % 4 + 1
                @goto start
            end
        end
    end

    return num
end

println("Number found: ", main())
```


```txt
Number found: 9867312
```



## Kotlin

Makes use of the Perl 6 entry's analysis:

### base 10


```scala
// version 1.1.4-3

fun Int.divByAll(digits: List<Char>) = digits.all { this % (it - '0') == 0 }

fun main(args: Array<String>) {
    val magic = 9 * 8 * 7
    val high = 9876432 / magic * magic
    for (i in high downTo magic step magic) {
        if (i % 10 == 0) continue            // can't end in '0'
        val s = i.toString()
        if ('0' in s || '5' in s) continue   // can't contain '0' or '5'
        val sd = s.toCharArray().distinct()
        if (sd.size != s.length) continue    // digits must be unique
        if (i.divByAll(sd)) {
            println("Largest decimal number is $i")
            return
        }
    }
}
```


```txt

Largest decimal number is 9867312

```



### base 16


```scala
// version 1.1.4-3

fun Long.divByAll(digits: List<Char>) =
    digits.all { this % (if (it <= '9') it - '0' else it - 'W')  == 0L }

fun main(args: Array<String>) {
    val magic = 15L * 14 * 13 * 12 * 11
    val high = 0xfedcba987654321L / magic * magic
    for (i in high downTo magic step magic) {
        if (i % 16 == 0L) continue           // can't end in '0'
        val s = i.toString(16)               // always generates lower case a-f
        if ('0' in s) continue               // can't contain '0'
        val sd = s.toCharArray().distinct()
        if (sd.size != s.length) continue    // digits must be unique
        if (i.divByAll(sd)) {
            println("Largest hex number is ${i.toString(16)}")
            return
        }
    }
}
```


```txt

Largest hex number is fedcb59726a1348

```



## Mathematica



### base 10


```Mathematica
 Max@Select[FromDigits/@Rest@Flatten[Permutations/@Subsets[Range@9,9],1],And@@IntegerQ/@(#/IntegerDigits@#)&]
```


```txt

9867312

```




## Perl

### Base 10



```perl
my $step = 9 * 8 * 7;                               # 504, interval between tests

my $initial = int(9876432 / $step) * $step;         # largest 7 digit multiple of 504 < 9876432

for($test = $initial; $test > 0 ; $test -= $step) { # decrement by 504
    next if $test =~ /[05]/;                        # skip numbers containing 0 or 5
    next if $test =~ /(.).*\1/;                     # skip numbers with non unique digits

    for (split '', $test) {                         # skip numbers that don't divide evenly by all digits
        next unless ($test / $_) % 1;
    }

    printf "Found $test after %d steps\n", ($initial-$test)/$step;
    for (split '', $test) {
       printf "%s / %s = %s\n", $test, $_, $test / $_;
    }
    last
}
```

```txt
Found 9867312 after 18 steps
9867312 / 9 = 1096368
9867312 / 8 = 1233414
9867312 / 6 = 1644552
9867312 / 7 = 1409616
9867312 / 3 = 3289104
9867312 / 1 = 9867312
9867312 / 2 = 4933656
```


### Base 16



```perl
use bigint;  # Very slow, but consistent results even with 32-bit Perl

my $hex = 'FEDCBA987654321';                      # largest possible hex number
$step = Math::BigInt::blcm(1..15);
$initial = int(hex($hex) / $step) * $step;

for($num = $initial; $num > 0 ; $num -= $step) {  # decrement by lcm

    my $test = sprintf '%x', $num;
    next if $test =~ /0/;                         # skip numbers containing 0
    next if $test =~ /(.).*\1/;                   # skip numbers with non unique digits

    push @res, sprintf "Found $test after %d steps\n", ($initial-$num)/$step;
    push @res, ' 'x12 . 'In base 16' . ' 'x36 . 'In base 10';
    for (split '', $test) {
        push @res, sprintf "%s / %s = %x  |  %d / %2d = %19d",
          $test, $_, $num / hex($_),
          $num, hex($_), $num / hex($_);
    }
    last
}

print join "\n", @res;
```

```txt
Found fedcb59726a1348 after 954460 steps
            In base 16                                    In base 10
fedcb59726a1348 / f = 10fda5b4be4f038  |  1147797065081426760 / 15 =   76519804338761784
fedcb59726a1348 / e = 1234561d150b83c  |  1147797065081426760 / 14 =   81985504648673340
fedcb59726a1348 / d = 139ad2e43e0c668  |  1147797065081426760 / 13 =   88292081929340520
fedcb59726a1348 / c = 153d0f21ede2c46  |  1147797065081426760 / 12 =   95649755423452230
fedcb59726a1348 / b = 172b56538f25ed8  |  1147797065081426760 / 11 =  104345187734675160
fedcb59726a1348 / 5 = 32f8f11e3aed0a8  |  1147797065081426760 /  5 =  229559413016285352
fedcb59726a1348 / 9 = 1c5169829283b08  |  1147797065081426760 /  9 =  127533007231269640
fedcb59726a1348 / 7 = 2468ac3a2a17078  |  1147797065081426760 /  7 =  163971009297346680
fedcb59726a1348 / 2 = 7f6e5acb93509a4  |  1147797065081426760 /  2 =  573898532540713380
fedcb59726a1348 / 6 = 2a7a1e43dbc588c  |  1147797065081426760 /  6 =  191299510846904460
fedcb59726a1348 / a = 197c788f1d76854  |  1147797065081426760 / 10 =  114779706508142676
fedcb59726a1348 / 1 = fedcb59726a1348  |  1147797065081426760 /  1 = 1147797065081426760
fedcb59726a1348 / 3 = 54f43c87b78b118  |  1147797065081426760 /  3 =  382599021693808920
fedcb59726a1348 / 4 = 3fb72d65c9a84d2  |  1147797065081426760 /  4 =  286949266270356690
fedcb59726a1348 / 8 = 1fdb96b2e4d4269  |  1147797065081426760 /  8 =  143474633135178345
```



## Perl 6

### Base 10


The number can not have a zero in it, that implies that it can not have a 5 either since if it has a 5, it must be divisible by 5, but the only numbers divisible by 5 end in 5 or 0. It can't be zero, and if it is odd, it can't be divisible by 2, 4, 6 or 8. So that leaves 98764321 as possible digits the number can contain. The sum of those 8 digits is not divisible by three so the largest possible integer must use no more than 7 of them (since 3, 6 and 9 would be eliminated). Strictly by removing possibilities that cannot possibly work we are down to at most 7 digits.

We can deduce that the digit that won't get used is one of 1, 4, or 7 since those are the only ones where the removal will yield a sum divisible by 3. It is ''extremely'' unlikely be 1, since EVERY number is divisible by 1. Removing it reduces the number of digits available but doesn't gain anything as far as divisibility. It is unlikely to be 7 since 7 is prime and can't be made up of multiples of other numbers. Practically though, the code to accommodate these observations is longer running and more complex than just brute-forcing it from here.

In order to accommodate the most possible digits, the number must be divisible by 7, 8 and 9. If that is true then it is automatically divisible by 2, 3, 4, & 6 as they can all be made from the combinations of multiples of 2 and 3 which are present in 8 & 9; so we'll only bother to check multiples of 9 * 8 * 7 or 504.

All these optimizations get the run time to well under 1 second.


```perl6
my $magic-number = 9 * 8 * 7;                        # 504

my $div = 9876432 div $magic-number * $magic-number; # largest 7 digit multiple of 504 < 9876432

for $div, { $_ - $magic-number } ... * -> $test {    # only generate multiples of 504
    next if $test ~~ / <[05]> /;                     # skip numbers containing 0 or 5
    next if $test ~~ / (.).*$0 /;                    # skip numbers with non unique digits

    say "Found $test";                               # Found a solution, display it
    for $test.comb {
        printf "%s / %s = %s\n", $test, $_, $test / $_;
    }
    last
}
```

```txt
Found 9867312
9867312 / 9 = 1096368
9867312 / 8 = 1233414
9867312 / 6 = 1644552
9867312 / 7 = 1409616
9867312 / 3 = 3289104
9867312 / 1 = 9867312
9867312 / 2 = 4933656
```



### Base 16


There are fewer analytical optimizations available for base 16. Other than 0, no digits can be ruled out so a much larger space must be searched. We'll start at the largest possible permutation (FEDCBA987654321) and work down so as soon as we find '''a''' solution, we know it is '''the''' solution.  The combination of <code>.race.grep</code> with <code>.first</code> lets us utilize concurrency and exit early when the single desired solution is found.


```perl6
sub find-match ($num) {
    my $test = $num.base(16);
    return if index $test, 0;
    return if $test.comb.Bag.values.max > 1;
    $num;
}
my $hex = 'FEDCBA987654321';        # largest possible hex number
my $magic-number = [lcm] 1 .. 15;   # find least common multiple
my $div = :16($hex) div $magic-number * $magic-number;

# hunt for target stepping backwards in multiples of the lcm
my $target = ($div, { $_ - $magic-number } ... 0).race.grep(*.&find-match).first;
my $hexnum = $target.base(16);

say "Found $hexnum"; # Found a solution, display it

say ' ' x 12, 'In base 16', ' ' x 36, 'In base 10';
for $hexnum.comb {
    printf "%s / %s = %s  |  %d / %2d = %19d\n",
        $hexnum, $_, ($target / :16($_)).base(16),
        $target, :16($_), $target / :16($_);}
```

```txt
Found FEDCB59726A1348
            In base 16                                    In base 10
FEDCB59726A1348 / F = 10FDA5B4BE4F038  |  1147797065081426760 / 15 =   76519804338761784
FEDCB59726A1348 / E = 1234561D150B83C  |  1147797065081426760 / 14 =   81985504648673340
FEDCB59726A1348 / D = 139AD2E43E0C668  |  1147797065081426760 / 13 =   88292081929340520
FEDCB59726A1348 / C = 153D0F21EDE2C46  |  1147797065081426760 / 12 =   95649755423452230
FEDCB59726A1348 / B = 172B56538F25ED8  |  1147797065081426760 / 11 =  104345187734675160
FEDCB59726A1348 / 5 = 32F8F11E3AED0A8  |  1147797065081426760 /  5 =  229559413016285352
FEDCB59726A1348 / 9 = 1C5169829283B08  |  1147797065081426760 /  9 =  127533007231269640
FEDCB59726A1348 / 7 = 2468AC3A2A17078  |  1147797065081426760 /  7 =  163971009297346680
FEDCB59726A1348 / 2 = 7F6E5ACB93509A4  |  1147797065081426760 /  2 =  573898532540713380
FEDCB59726A1348 / 6 = 2A7A1E43DBC588C  |  1147797065081426760 /  6 =  191299510846904460
FEDCB59726A1348 / A = 197C788F1D76854  |  1147797065081426760 / 10 =  114779706508142676
FEDCB59726A1348 / 1 = FEDCB59726A1348  |  1147797065081426760 /  1 = 1147797065081426760
FEDCB59726A1348 / 3 = 54F43C87B78B118  |  1147797065081426760 /  3 =  382599021693808920
FEDCB59726A1348 / 4 = 3FB72D65C9A84D2  |  1147797065081426760 /  4 =  286949266270356690
FEDCB59726A1348 / 8 = 1FDB96B2E4D4269  |  1147797065081426760 /  8 =  143474633135178345
```



## Phix


###  base 10

```Phix
integer magic = 9*8*7,
        high  = 9876432,
        n = high-mod(high,magic)
sequence seen
while true do
    string s = sprintf("%d",n)
    seen = {1,0,0,0,0,1,0,0,0,0}
    for j=1 to length(s) do
        seen[s[j]-'0'+1] += 1
    end for
    if max(seen)=1 then exit end if
    n -= magic
end while
-- may as well quickly verify...
seen[5+1] = 0   -- (skipping 5)
for i=1 to 9 do --  ( and 0 )
    if seen[i+1] then
        if mod(n,i)!=0 then ?9/0 end if
    end if
end for
printf(1,"%d (%d iterations)\n",{n,(high-n)/magic})
```

```txt

9867312 (18 iterations)

```



###  base 16

using gmp (15 times faster than bigatom)

```Phix
atom t0 = time()
integer count = 0
string s
include mpfr.e
mpz lcm15 = mpz_init(lcm(tagset(15))),
    d = mpz_init(),
    r = mpz_init()
mpz_set_str(d,"FEDCBA987654321",16)
mpz_mod(r,d,lcm15)
mpz_sub(d,d,r) -- d:= max k*lcm <= "FE..21"
r = mpz_free(r) -- (no longer used)
while true do
    s = mpz_get_str(d,16)
    if sort(s)="123456789abcdef" then exit end if
    mpz_sub(d,d,lcm15)
    count += 1
end while
string e = elapsed(time()-t0)
printf(1,"%s (%d iterations, %s)\n",{s,count,e})
```

```txt

fedcb59726a1348 (954460 iterations, 4s)

```



## Prolog

This will work with any radix, including base 10 and base 16.

```prolog
%  Find the largest integer divisible by all it's digits, with no digit repeated.
%  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%  We go for a generic algorithm here.  Test for divisibility is done by
%  calculating the least common multiplier for all digits, and testing
%  whether a candidate can be divided by the LCM without remainder.
%
%  Instead of iterating numbers and checking whether the number has
%  repeating digits, it is more efficient to generate permutations of
%  digits and then convert to a number.  Doing it this way reduces search
%  space greatly.
%
% Notes:
%  For decimal numbers we could improve times by testing only numbers
%  of length 7 (since 5x2=10 and 0 is not one of our digits, and 9x2=18
%  which needs 2 digits to store), but that sort of logic does not
%  hold for hexadecimal numbers.
%  We could also explicitly eliminate odd numbers, but the double validity
%  check actually slows us down very slightly instead of speeding us up.

:- dynamic
	trial/1.       % temporarily store digit combinations here.

gcd(X, X, X).  % Calculate greatest common divisor
gcd(M, N, X) :- N > M, B is N-M, gcd(M,B,X).
gcd(M, N, X) :- N < M, A is M-N, gcd(A,N,X).

lcm(A, B, LCM) :- gcd(A,B,GCD), LCM is A * B / GCD.

lcm([H], H).   % Calculate least common multiplier
lcm([A|T], LCM) :- lcm(T, B), !, lcm(A,B,LCM).

mkint(_, Val, [], Val).     % Result = Val where list is empty
mkint(Radix, Val, [H|T], Int) :-  % (((I0*10+I1)*10+I2)*10+In)...
	V0 is Val*Radix+H, !, mkint(Radix, V0, T, Int).

% Turn a list of digits into an integer number using Radix.
mkint(Radix, [H|T], Int) :- mkint(Radix, H, T, Int).

domain(0, []).       % For example, domain(5) is [1,2,3,4,5]
domain(N, [N|Digits]) :-
   succ(N0, N), !, domain(N0, Digits).

trial(0, Digits, Digits).   % generates a combination of digits to test
trial(N, D, Digits) :-      % remove N digits, and find remaining combinations
    append(L0,[_|L1],D), succ(N0, N), trial(N0, L1, Dx),
    append(L0, Dx, Digits). % trial(1, [3,2,1], D) -> D=[2,1]; D=[3,1]; D=[3,2].

make_trials(_,_) :- retractall(trial(_)), fail.
make_trials(N,Domain) :- trial(N, Domain, Digits), asserta(trial(Digits)), fail.
make_trials(_,_).           % trials are stored highest values to lowest

combinations(Radix, NDigits) :-  % Precalculate all possible digit combinations
    succ(R0, Radix), domain(R0, Domain), Nskip is R0 - NDigits,
    make_trials(Nskip, Domain).

test(Radix, Digits, LCM, Number) :-  % Make an integer and check for divisibility
   mkint(Radix, Digits, Number), 0 is Number mod LCM.

bignum(Radix, Number) :-
   succ(R0, Radix), between(1,R0,N), NDigits is Radix - N,    % loop decreasing length
   combinations(Radix, NDigits),            % precalc digit combos with length=NDigits
   trial(Digits), lcm(Digits, LCM),         % for a combination, calculate LCM
   permutation(Digits, Trial),              % generate a permutation
   test(Radix, Trial, LCM, Number).         % test for divisibility

largest_decimal(N) :- bignum(10, N), !.
largest_hex(N, H) :- bignum(16, N), !, sformat(H, '~16r', [N]).
```


```txt
?- time(largest_decimal(S)).
% 20,043,250 inferences, 3.086 CPU in 3.089 seconds (100% CPU, 6493905 Lips)
S = 9867312.

?- time(largest_hex(S,H)).
% 73,332,059 inferences, 11.800 CPU in 11.803 seconds (100% CPU, 6214553 Lips)
S = 1147797065081426760,
H = "fedcb59726a1348".
```



## Python


### base 10

Using the insights presented in the preamble to the '''Perl 6''' (base 10) example:
```python
'''Largest number divisible by its digits'''

from itertools import (chain, permutations)
from collections import (defaultdict)
from inspect import (signature)
from functools import (reduce)
from math import (gcd)


# main :: IO ()
def main():
    '''Tests'''

    # (Division by zero is not an option, so we omit 0 and 5)
    digits = [1, 2, 3, 4, 6, 7, 8, 9]

    # Least common multiple of the digits above
    lcmDigits = foldl1(lcm)(digits)

    # Any set of 7 distinct digits obtained by deleting
    # [1, 4 or 7] from the list of 8 above
    sevenDigits = fmap(flip(delete)(digits))(
        [1, 4, 7]
    )

    print(
        max(
            fmap(intFromDigits)(
                concatMap(permutations)(sevenDigits)
            ),
            key=lambda n: n if 0 == n % lcmDigits else 0
        )
    )


# intFromDigits :: [Int] -> Int
def intFromDigits(xs):
    '''An integer derived from an
       ordered list of digits.
    '''
    return reduce(lambda a, x: a * 10 + x, xs, 0)


# GENERIC --------------------------------------------------

# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''A concatenated list over which a function has been mapped.
       The list monad can be derived by using a function f which
       wraps its output in a list,
       (using an empty list to represent computational failure).'''
    return lambda xs: list(
        chain.from_iterable(map(f, xs))
    )


# delete :: Eq a => a -> [a] -> [a]
def delete(x):
    '''xs with the first instance of
       x removed.
    '''
    def go(xs):
        ys = xs.copy()
        ys.remove(x)
        return ys
    return lambda xs: go(xs)


# flip :: (a -> b -> c) -> b -> a -> c
def flip(f):
    '''The (curried or uncurried) function f with its
       arguments reversed.
    '''
    if 1 < len(signature(f).parameters):
        return lambda a, b: f(b, a)
    else:
        return lambda a: lambda b: f(b)(a)


# foldl1 :: (a -> a -> a) -> [a] -> a
def foldl1(f):
    '''Left to right reduction of the
       non-empty list xs, using the binary
       operator f, with the head of xs
       as the initial acccumulator value.
    '''
    return lambda xs: reduce(
        lambda a, x: f(a)(x), xs[1:], xs[0]
    ) if xs else None


# fmap :: Functor f => (a -> b) -> f a -> f b
def fmap(f):
    '''A function f mapped over a functor.'''
    def go(x):
        return defaultdict(list, [
            ('list', fmapList),
            # ('iter', fmapNext),
            # ('Either', fmapLR),
            # ('Maybe', fmapMay),
            # ('Tree', fmapTree),
            # ('tuple', fmapTuple),
            ('function', fmapFn)
        ])[
            typeName(x)
        ](f)(x)
    return lambda v: go(v)


# fmapList :: (a -> b) -> [a] -> [b]
def fmapList(f):
    '''fmap over a list.
       f lifted to a function over a list.
    '''
    return lambda xs: list(map(f, xs))


# fmapFn :: (a -> b) -> (r -> a) -> r -> b
def fmapFn(f):
    '''fmap over a function.
       The composition of f and g.
    '''
    return lambda g: lambda x: f(g(x))


# lcm :: Int -> Int -> Int
def lcm(x):
    '''The smallest positive integer divisible
       without remainder by both x and y.
    '''
    return lambda y: (
        0 if (0 == x or 0 == y) else abs(
            y * (x // gcd(x, y))
        )
    )


# typeName :: a -> String
def typeName(x):
    '''Name string for a built-in or user-defined type.
       Selector for type-specific instances
       of polymorphic functions.
    '''
    if isinstance(x, dict):
        return x.get('type') if 'type' in x else 'dict'
    else:
        return 'iter' if hasattr(x, '__next__') else (
            type(x).__name__
        )


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
9867312
```



### base 16

Descending from the upper limit, in steps of 360360 (least common multiple of the fifteen digit values), until the first number that uses all fifteen digits when expressed in hexadecimal.

```python
'''Largest number divisible by its hex digits'''

from functools import (reduce)
from math import (gcd)


# main :: IO ()
def main():
    '''First integer evenly divisible by each of its
       hex digits, none of which appear more than once.
    '''

    # Least common multiple of digits [1..15]
    # ( -> 360360 )
    lcmDigits = foldl1(lcm)(
        enumFromTo(1)(15)
    )
    allDigits = 0xfedcba987654321

    # ( -> 1147797409030632360 )
    upperLimit = allDigits - (allDigits % lcmDigits)

    # Possible numbers
    xs = enumFromThenToNext(upperLimit)(
        upperLimit - lcmDigits
    )(1)

    print(
        hex(
            until(lambda x: 15 == len(set(showHex(x))))(
                lambda _: next(xs)
            )(next(xs))
        )
    )   # --> 0xfedcb59726a1348


#  GENERIC ABSTRACTIONS -----------------------------------

# enumFromThenToNext :: Int -> Int -> Int -> Gen [Int]
def enumFromThenToNext(m):
    '''Non-finite series of integer values enumerated
       from m to n with a step size defined by nxt-m.
    '''
    def go(m, nxt):
        d = nxt - m
        v = m
        while True:
            yield v
            v = d + v
    return lambda nxt: lambda n: go(m, nxt)


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# foldl1 :: (a -> a -> a) -> [a] -> a
def foldl1(f):
    '''Left to right reduction of the
       non-empty list xs, using the binary
       operator f, with the head of xs
       as the initial acccumulator value.
    '''
    return lambda xs: reduce(
        lambda a, x: f(a)(x), xs[1:], xs[0]
    ) if xs else None


# lcm :: Int -> Int -> Int
def lcm(x):
    '''The smallest positive integer divisible
       without remainder by both x and y.
    '''
    return lambda y: (
        0 if (0 == x or 0 == y) else abs(
            y * (x // gcd(x, y))
        )
    )


# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    '''The result of repeatedly applying f until p holds.
       The initial seed value is x.
    '''
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)


# showHex :: Int -> String
def showHex(n):
    '''Hexadecimal string representation
       of an integer value.
    '''
    return hex(n)[2:]


# MAIN --
if __name__ == '__main__':
    main()
```

```txt
0xfedcb59726a1348
[Finished in 1.243s]
```



## REXX


### base 10

This REXX version uses mostly the same logic and deductions that the   '''Perl 6'''   example does,   but it performs

the tests in a different order for maximum speed.

The inner   '''do'''   loop is only executed a score of times;   the 1<sup>st</sup>   '''if'''   statement does the bulk of the eliminations.

```rexx
/*REXX program finds the largest (decimal) integer divisible by all its decimal digits. */
$= 7 * 8 * 9                                     /*a number that it must be divisible by*/
start= 9876432 % $ * $                           /*the number to start the sieving from.*/
t= 0                                             /*the number of divisibility trials.   */
    do #=start  by -$                            /*search from largest number downwards.*/
    if # // $             \==0  then iterate     /*Not divisible?   Then keep searching.*/
    if verify(50, #, 'M') \==0  then iterate     /*does it contain a  five  or a  zero? */
    t= t+1                                       /*curiosity's sake, track # of trials. */
          do j=1  for length(#) - 1              /*look for a possible duplicated digit.*/
          if pos( substr( #, j, 1), #, j+1) \==0  then iterate #
          end   /*j*/                            /* [↑]  Not unique? Then keep searching*/
                                                 /* [↓]  superfluous, but check anyways.*/
          do v=1  for length(#)                  /*verify the # is divisible by all digs*/
          if # // substr(#, v, 1)           \==0  then iterate #
          end   /*v*/                            /* [↑]  ¬divisible?  Then keep looking.*/
    leave                                        /*we found a number, so go display it. */
    end   /*#*/

say 'found '   #    "  (in "   t   ' trials)'    /*stick a fork in it,  we're all done. */
```

Timing note:   execution time is under   '''<sup>1</sup>/<sub>2</sub>'''   millisecond   (essentially not measurable in the granularity of the REXX timer under Microsoft Windows).

```txt

found  9867312   (in  11  trials)

```



### base 16

The "magic" number was expanded to handle hexadecimal numbers.

Note that   '''15*14*13*12*11'''   is the same as   '''13*11*9*8*7*5'''.

```rexx
/*REXX program finds the largest  hexadecimal  integer divisible by all its hex digits. */
numeric digits 20                                /*be able to handle the large hex nums.*/
bigH= 'fedcba987654321'                          /*biggest hexadecimal number possible. */
bigN= x2d(bigH)                                  /*   "         "        "    in decimal*/
$= 15 * 14 * 13 * 12 * 11                        /*a number that it must be divisible by*/
start= bigN % $ * $                              /*the number to start the sieving from.*/
t=0                                              /*the number of divisibility trials.   */
    do #=start  by -$                            /*search from largest poss. # downwards*/
    if # // $    \==0  then iterate              /*Not divisible?   Then keep searching.*/
    h= d2x(#)                                    /*convert decimal number to hexadecimal*/
    if pos(0, h) \==0  then iterate              /*does hexadecimal number contain a 0? */
    t= t+1                                       /*curiosity's sake, track # of trials. */
          do j=1  for length(h) - 1              /*look for a possible duplicated digit.*/
          if pos( substr(h, j, 1),  h, j+1) \==0  then iterate #
          end   /*j*/                            /* [↑]  Not unique? Then keep searching*/

          do v=1  for length(h)                  /*verify the # is divisible by all digs*/
          if # // x2d(substr( h, v, 1)  )    \==0  then iterate #
          end   /*v*/                            /* [↑]  ¬divisible?  Then keep looking.*/
    leave                                        /*we found a number, so go display it. */
    end   /*#*/

say 'found '   h    "  (in "   t   ' trials)'    /*stick a fork in it,  we're all done. */
```

```txt

found  FEDCB59726A1348   (in  287747  trials)

```



## Ring


```ring

# Project : Largest number divisible by its digits

for n = 9867000  to 9867400
    numbers = list(9)
    for t=1 to 9
        numbers[t] = 0
    next
    flag = 1
    flag2 = 1
    flag3 = 1
    str=string(n)
    for m=1 to len(str)
        if number(str[m]) > 0
           numbers[number(str[m])] = numbers[number(str[m])] + 1
        else
           flag2 = 0
        ok
    next
    if flag2 = 1
       for p=1 to 9
           if numbers[p] = 0 or numbers[p] = 1
           else
              flag = 0
           ok
       next
       if flag = 1
          for x=1 to len(str)
              if n%(number(str[x])) != 0
                 flag3 = 0
              ok
          next
          if flag3 = 1
             see n + nl
          ok
       ok
    ok
next

```

Output:

```txt

9867312

```



## Ruby


### base 10

Following the reasoning of the Perl 6 sample.

```ruby
magic_number = 9*8*7
div          = 9876432.div(magic_number) * magic_number
candidates   = div.step(0, -magic_number)

res = candidates.find do |c|
  digits = c.digits
  (digits & [0,5]).empty? && digits == digits.uniq
end

puts res # => 9867312
```



## Scala


### base 10

This example starts with a lazily evaluated list of decreasing decimal numbers, starting with 98764321 (5 is eliminated as per the Pearl 6 analysis). It applies a filter to only accept numbers with distinct, nonzero digits that all divide the number itself, and then returns the head of the list.


```scala
import scala.collection.mutable

def largestDecimal: Int = Iterator.from(98764321, -1).filter(chkDec).next
def chkDec(num: Int): Boolean = {
  val set = mutable.HashSet[Int]()
  num.toString.toVector.map(_.asDigit).forall(d => (d != 0) && (num%d == 0) && set.add(d))
}
```


```txt
scala> println(s"Base 10: $largestDecimal")
Base 10: 9867312
```



### base 16

While concise, the previous example is relatively slow, taking nearly 30 seconds to complete. So, instead of simply moving on to a base 16 version, this next example is a fast version for arbitrary base. Starting with a list of digits generated from the given base, the program generates a lazily evaluated list of all possible combinations of digits in blocks of decreasing length. Each block is passed to a function that generates a list of numbers for each combination which are divisible by all the digits, then filters it for numbers which are made up of the required digits. The blocks are checked in order until a number is found.


```scala
import spire.math.SafeLong
import spire.implicits._

object LargestNumDivisibleByDigits {
  def main(args: Array[String]): Unit = {
    for(b <- Seq(10, 16)){
      val tStart = System.currentTimeMillis
      val res = getLargestNum(b).toBigInt.toString(b)
      val tDur = System.currentTimeMillis - tStart
      println(s"Base $b: $res [${tDur}ms]")
    }
  }

  def getLargestNum(base: SafeLong): SafeLong = {
    def chkNum(digits: Vector[SafeLong])(num: SafeLong): Boolean = {
      val lst = LazyList.iterate((num%base, num/base)){case (_, src) => (src%base, src/base)}.take(digits.length).map(_._1)
      lst.diff(digits).isEmpty
    }

    def chkChunk(combo: Vector[SafeLong]): Option[SafeLong] = {
      val lcm = combo.reduce(_.lcm(_))
      val ulim = combo.zipWithIndex.map{case (n, i) => n*(base ** i)}.reduce(_+_)
      Iterator.iterate(ulim - (ulim%lcm))(_ - lcm).takeWhile(_ > 0).find(chkNum(combo))
    }

    val baseDigits: Vector[SafeLong] = Vector.range(1, base.toInt).map(SafeLong(_))
    def chkBlock(digits: Iterator[Vector[SafeLong]]): Option[SafeLong] = digits.map(chkChunk).collect{case Some(n) => n}.maxOption
    Iterator.from(base.toInt - 1, -1).map(len => chkBlock(baseDigits.combinations(len))).collect{case Some(n) => n}.next
  }
}
```


```txt
Base 10: 9867312 [1144ms]
Base 16: fedcb59726a1348 [1090ms]
```



## Sidef


### base 10


```ruby
func largest_number(base) {

    var digits = @(base ^.. 1)

    digits.each {|k|
        digits.variations(k, {|*a|
            var n = Number(a.join, base)
            if (a.all {|d| d.divides(n) }) {
                return n
            }
        })
    }
}

say largest_number(10)   #=> 9867312
```



## zkl


### base 10

```zkl
const magic_number=9*8*7; # 504
const div=9876432 / magic_number * magic_number; #largest 7 digit multiple of 504 < 9876432

foreach test in ([div..0,-magic_number]){
   text:=test.toString();
   if(text.holds("0","5"))		 continue; # skip numbers containing 0 or 5
   if(text.unique().len()!=text.len())   continue; # skip numbers with non unique digits
   if(test.split().filter1('%.fp(test))) continue; # skip numbers that don't divide evenly by all digits

   println("Found ",test); # Found a solution, display it
   foreach d in (test.split()){
      println("%s / %s = %s".fmt(test,d, test/d));
   }
   break;
}
```

```txt

Found 9867312
9867312 / 9 = 1096368
9867312 / 8 = 1233414
9867312 / 6 = 1644552
9867312 / 7 = 1409616
9867312 / 3 = 3289104
9867312 / 1 = 9867312
9867312 / 2 = 4933656

```



### base 16

```zkl
const bigN=0xfedcba987654321; // biggest hexadecimal number possible.
lcm:=lcmNs([1..15]);	// 360360, smallest # that will divide answer
upperLimit:=bigN - bigN%lcm; // start at a mulitple of whatever the answer is

foreach test in ([upperLimit..1,-lcm]){
   text:=test.toString(16);
   if(15!=text.unique().len()) continue;
   println(text);
   break;
}
```


```zkl
fcn lcmNs(ns){ ns.reduce(fcn(m,n){ (m*n).abs()/m.gcd(n) }) }
```

```txt

fedcb59726a1348

```

