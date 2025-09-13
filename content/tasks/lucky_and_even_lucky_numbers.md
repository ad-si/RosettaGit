+++
title = "Lucky and even lucky numbers"
description = ""
date = 2019-01-19T00:46:50Z
aliases = []
[extra]
id = 17325
[taxonomies]
categories = ["task"]
tags = []
+++

<small>Note that in the following explanation list indices are assumed to start at ''one''.</small>

;Definition of lucky numbers
''[[wp:Lucky number|Lucky numbers]]'' are positive integers that are formed by:

# Form a list of all the positive odd integers > 0
<math>1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39 ...</math>
# Return the first number from the list (which is '''1''').
# <small>(Loop begins here)</small>
#* Note then return the second number from the list (which is '''3''').
#* Discard every third, (as noted), number from the list to form the new list
<math>1, 3, 7, 9, 13, 15, 19, 21, 25, 27, 31, 33, 37, 39, 43, 45, 49, 51, 55, 57 ...</math>
# <small>(Expanding the loop a few more times...)</small>
#* Note then return the third number from the list (which is '''7''').
#* Discard every 7<sup>th</sup>, (as noted), number from the list to form the new list
<math>1, 3, 7, 9, 13, 15, 21, 25, 27, 31, 33, 37, 43, 45, 49, 51, 55, 57, 63, 67 ...</math>
#* Note then return the 4<sup>th</sup> number from the list (which is '''9''').
#* Discard every 9<sup>th</sup>, (as noted), number from the list to form the new list
<math>1, 3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 45, 49, 51, 55, 63, 67, 69, 73 ...</math>
#* Take the 5<sup>th</sup>, i.e. '''13'''.  Remove every 13<sup>th</sup>.
#* Take the 6<sup>th</sup>, i.e. '''15'''.  Remove every 15<sup>th</sup>.
#* Take the 7<sup>th</sup>, i.e. '''21'''.  Remove every 21<sup>th</sup>.
#* Take the 8<sup>th</sup>, i.e. '''25'''.  Remove every 25<sup>th</sup>.
# <small>(Rule for the loop)</small>
#* Note the <math>n</math><sup>th</sup>, which is <math>m</math>.
#* Remove every <math>m</math><sup>th</sup>.
#* Increment <math>n</math>.

;Definition of even lucky numbers
This follows the same rules as the definition of lucky numbers above ''except for the very first step'':
# Form a list of all the positive '''even''' integers > 0
<math>2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40 ...</math>
# Return the first number from the list (which is '''2''').
# <small>(Loop begins here)</small>
#* Note then return the second number from the list (which is '''4''').
#* Discard every 4<sup>th</sup>, (as noted), number from the list to form the new list
<math>2, 4, 6, 10, 12, 14, 18, 20, 22, 26, 28, 30, 34, 36, 38, 42, 44, 46, 50, 52 ...</math>
# <small>(Expanding the loop a few more times...)</small>
#* Note then return the third number from the list (which is '''6''').
#* Discard every 6<sup>th</sup>, (as noted), number from the list to form the new list
<math>2, 4, 6, 10, 12, 18, 20, 22, 26, 28, 34, 36, 38, 42, 44, 50, 52, 54, 58, 60 ...</math>
#* Take the 4<sup>th</sup>, i.e. '''10'''.  Remove every 10<sup>th</sup>.
#* Take the 5<sup>th</sup>, i.e. '''12'''.  Remove every 12<sup>th</sup>.
# <small>(Rule for the loop)</small>
#* Note the <math>n</math><sup>th</sup>, which is <math>m</math>.
#* Remove every <math>m</math><sup>th</sup>.
#* Increment <math>n</math>.

;Task requirements
* Write one or two subroutines (functions) to generate ''lucky numbers'' and ''even lucky numbers''
* Write a command-line interface to allow selection of which kind of numbers and which number(s). Since input is from the command line, tests should be made for the common errors:
** missing arguments
** too many arguments
** number (or numbers) aren't legal
** misspelled argument ('''lucky''' or '''evenLucky''')
* The command line handling should:
** support mixed case handling of the (non-numeric) arguments
** support printing a particular number
** support printing a range of numbers by their index
** support printing a range of numbers by their values
* The resulting list of numbers should be printed on a single line.


The program should support the arguments:

```txt

                             what is displayed  (on a single line)
       argument(s)              (optional verbiage is encouraged)
  ╔═══════════════════╦════════════════════════════════════════════════════╗
  ║  j                ║  Jth       lucky number                            ║
  ║  j  ,      lucky  ║  Jth       lucky number                            ║
  ║  j  ,  evenLucky  ║  Jth  even lucky number                            ║
  ║                   ║                                                    ║
  ║  j  k             ║  Jth  through  Kth (inclusive)       lucky numbers ║
  ║  j  k      lucky  ║  Jth  through  Kth (inclusive)       lucky numbers ║
  ║  j  k  evenLucky  ║  Jth  through  Kth (inclusive)  even lucky numbers ║
  ║                   ║                                                    ║
  ║  j -k             ║  all       lucky numbers in the range  j ──► |k|   ║
  ║  j -k      lucky  ║  all       lucky numbers in the range  j ──► |k|   ║
  ║  j -k  evenLucky  ║  all  even lucky numbers in the range  j ──► |k|   ║
  ╚═══════════════════╩════════════════════════════════════════════════════╝
                           where    |k|    is the absolute value of   k

```

Demonstrate the program by:
* showing the first twenty ''lucky'' numbers
* showing the first twenty ''even lucky'' numbers
* showing all ''lucky'' numbers between 6,000 and 6,100 (inclusive)
* showing all ''even lucky'' numbers in the same range as above
* showing the 10,000<sup>th</sup> ''lucky'' number (extra credit)
* showing the 10,000<sup>th</sup> ''even lucky'' number (extra credit)

## See also

* This task is related to the [[Sieve of Eratosthenes]] task.
* OEIS Wiki [http://oeis.org/wiki/Lucky_numbers Lucky numbers].
* Sequence [https://oeis.org/A000959 A000959 lucky numbers] on The On-Line Encyclopedia of Integer Sequences.
* Sequence [https://oeis.org/A045954 A045954 even lucky numbers or ELN] on The On-Line Encyclopedia of Integer Sequences.
* Entry [http://mathworld.wolfram.com/LuckyNumber.html lucky numbers] on The Eric Weisstein's World of Mathematics.




## C++

```cpp
#include <algorithm>
#include <iostream>
#include <iterator>
#include <vector>

const int luckySize = 60000;
std::vector<int> luckyEven(luckySize);
std::vector<int> luckyOdd(luckySize);

void init() {
    for (int i = 0; i < luckySize; ++i) {
        luckyEven[i] = i * 2 + 2;
        luckyOdd[i] = i * 2 + 1;
    }
}

void filterLuckyEven() {
    for (size_t n = 2; n < luckyEven.size(); ++n) {
        int m = luckyEven[n - 1];
        int end = (luckyEven.size() / m) * m - 1;
        for (int j = end; j >= m - 1; j -= m) {
            std::copy(luckyEven.begin() + j + 1, luckyEven.end(), luckyEven.begin() + j);
            luckyEven.pop_back();
        }
    }
}

void filterLuckyOdd() {
    for (size_t n = 2; n < luckyOdd.size(); ++n) {
        int m = luckyOdd[n - 1];
        int end = (luckyOdd.size() / m) * m - 1;
        for (int j = end; j >= m - 1; j -= m) {
            std::copy(luckyOdd.begin() + j + 1, luckyOdd.end(), luckyOdd.begin() + j);
            luckyOdd.pop_back();
        }
    }
}

void printBetween(size_t j, size_t k, bool even) {
    std::ostream_iterator<int> out_it{ std::cout, ", " };

    if (even) {
        size_t max = luckyEven.back();
        if (j > max || k > max) {
            std::cerr << "At least one are is too big\n";
            exit(EXIT_FAILURE);
        }

        std::cout << "Lucky even numbers between " << j << " and " << k << " are: ";
        std::copy_if(luckyEven.begin(), luckyEven.end(), out_it, [j, k](size_t n) {
            return j <= n && n <= k;
        });
    } else {
        size_t max = luckyOdd.back();
        if (j > max || k > max) {
            std::cerr << "At least one are is too big\n";
            exit(EXIT_FAILURE);
        }

        std::cout << "Lucky numbers between " << j << " and " << k << " are: ";
        std::copy_if(luckyOdd.begin(), luckyOdd.end(), out_it, [j, k](size_t n) {
            return j <= n && n <= k;
        });
    }
    std::cout << '\n';
}

void printRange(size_t j, size_t k, bool even) {
    std::ostream_iterator<int> out_it{ std::cout, ", " };
    if (even) {
        if (k >= luckyEven.size()) {
            std::cerr << "The argument is too large\n";
            exit(EXIT_FAILURE);
        }
        std::cout << "Lucky even numbers " << j << " to " << k << " are: ";
        std::copy(luckyEven.begin() + j - 1, luckyEven.begin() + k, out_it);
    } else {
        if (k >= luckyOdd.size()) {
            std::cerr << "The argument is too large\n";
            exit(EXIT_FAILURE);
        }
        std::cout << "Lucky numbers " << j << " to " << k << " are: ";
        std::copy(luckyOdd.begin() + j - 1, luckyOdd.begin() + k, out_it);
    }
}

void printSingle(size_t j, bool even) {
    if (even) {
        if (j >= luckyEven.size()) {
            std::cerr << "The argument is too large\n";
            exit(EXIT_FAILURE);
        }
        std::cout << "Lucky even number " << j << "=" << luckyEven[j - 1] << '\n';
    } else {
        if (j >= luckyOdd.size()) {
            std::cerr << "The argument is too large\n";
            exit(EXIT_FAILURE);
        }
        std::cout << "Lucky number " << j << "=" << luckyOdd[j - 1] << '\n';
    }
}

void help() {
    std::cout << "./lucky j [k] [--lucky|--evenLucky]\n";
    std::cout << "\n";
    std::cout << "       argument(s)        |  what is displayed\n";
    std::cout << "
### ========================================
\n";
    std::cout << "-j=m                      |  mth lucky number\n";
    std::cout << "-j=m  --lucky             |  mth lucky number\n";
    std::cout << "-j=m  --evenLucky         |  mth even lucky number\n";
    std::cout << "-j=m  -k=n                |  mth through nth (inclusive) lucky numbers\n";
    std::cout << "-j=m  -k=n  --lucky       |  mth through nth (inclusive) lucky numbers\n";
    std::cout << "-j=m  -k=n  --evenLucky   |  mth through nth (inclusive) even lucky numbers\n";
    std::cout << "-j=m  -k=-n               |  all lucky numbers in the range [m, n]\n";
    std::cout << "-j=m  -k=-n  --lucky      |  all lucky numbers in the range [m, n]\n";
    std::cout << "-j=m  -k=-n  --evenLucky  |  all even lucky numbers in the range [m, n]\n";
}

int main(int argc, char **argv) {
    bool evenLucky = false;
    int j = 0;
    int k = 0;

    // skip arg 0, because that is just the executable name
    if (argc < 2) {
        help();
        exit(EXIT_FAILURE);
    }

    bool good = false;
    for (int i = 1; i < argc; ++i) {
        if ('-' == argv[i][0]) {
            if ('-' == argv[i][1]) {
                // long args
                if (0 == strcmp("--lucky", argv[i])) {
                    evenLucky = false;
                } else if (0 == strcmp("--evenLucky", argv[i])) {
                    evenLucky = true;
                } else {
                    std::cerr << "Unknown long argument: [" << argv[i] << "]\n";
                    exit(EXIT_FAILURE);
                }
            } else {
                // short args
                if ('j' == argv[i][1] && '=' == argv[i][2] && argv[i][3] != 0) {
                    good = true;
                    j = atoi(&argv[i][3]);
                } else if ('k' == argv[i][1] && '=' == argv[i][2]) {
                    k = atoi(&argv[i][3]);
                } else {
                    std::cerr << "Unknown short argument: " << argv[i] << '\n';
                    exit(EXIT_FAILURE);
                }
            }
        } else {
            std::cerr << "Unknown argument: " << argv[i] << '\n';
            exit(EXIT_FAILURE);
        }
    }
    if (!good) {
        help();
        exit(EXIT_FAILURE);
    }

    init();
    filterLuckyEven();
    filterLuckyOdd();
    if (k > 0) {
        printRange(j, k, evenLucky);
    } else if (k < 0) {
        printBetween(j, -k, evenLucky);
    } else {
        printSingle(j, evenLucky);
    }

    return 0;
}
```

```txt
&gt;LuckyNumbers.exe -j=1 -k=20
Lucky numbers 1 to 20 are: 1, 3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 49, 51, 63, 67, 69, 73, 75, 79,

&gt;LuckyNumbers.exe -j=1 -k=20 --evenLucky
Lucky even numbers 1 to 20 are: 2, 4, 6, 10, 12, 18, 20, 22, 26, 34, 36, 42, 44, 50, 52, 54, 58, 68, 70, 76,

&gt;LuckyNumbers.exe -j=6000 -k=-6100
Lucky numbers between 6000 and 6100 are: 6009, 6019, 6031, 6049, 6055, 6061, 6079, 6093,

&gt;LuckyNumbers.exe -j=6000 -k=-6100 --evenLucky
Lucky even numbers between 6000 and 6100 are: 6018, 6020, 6022, 6026, 6036, 6038, 6050, 6058, 6074, 6090, 6092,

&gt;LuckyNumbers.exe -j=10000
Lucky number 10000=115591

&gt;LuckyNumbers.exe -j=10000 --evenLucky
Lucky even number 10000=111842
```



## D


```D
import std.algorithm;
import std.concurrency;
import std.conv;
import std.getopt;
import std.range;
import std.stdio;

auto lucky(bool even, int nmax=200_000) {
    import std.container.array;

    int start = even ? 2 : 1;

    return new Generator!int({
        auto ln = make!(Array!int)(iota(start,nmax,2));

        // yield the first number
        yield(ln[0]);

        int n=1;
        for(; n<ln.length/2+1; n++) {
            yield(ln[n]);

            int step = ln[n]-1;

            // remove the non-lucky numbers related to the current lucky number
            for (int i=step; i<ln.length; i+=step) {
                ln.linearRemove(ln[].drop(i).take(1));
            }
        }

        // yield all remaining values
        foreach(val; ln[n..$]) {
            yield(val);
        }
    });
}

void help(Option[] opt) {
    defaultGetoptPrinter("./lucky j [k] [--lucky|--evenLucky]", opt);

    writeln;
    writeln("       argument(s)        |  what is displayed");
    writeln("
### ========================================
");
    writeln("-j=m                      |  mth lucky number");
    writeln("-j=m  --lucky             |  mth lucky number");
    writeln("-j=m  --evenLucky         |  mth even lucky number");
    writeln("-j=m  -k=n                |  mth through nth (inclusive) lucky numbers");
    writeln("-j=m  -k=n  --lucky       |  mth through nth (inclusive) lucky numbers");
    writeln("-j=m  -k=n  --evenLucky   |  mth through nth (inclusive) even lucky numbers");
    writeln("-j=m  -k=-n               |  all lucky numbers in the range [m, n]");
    writeln("-j=m  -k=-n  --lucky      |  all lucky numbers in the range [m, n]");
    writeln("-j=m  -k=-n  --evenLucky  |  all even lucky numbers in the range [m, n]");
}

void main(string[] args) {
    int j;
    int k;
    bool evenLucky = false;

    void luckyOpt() {
        evenLucky = false;
    }
    auto helpInformation = getopt(
        args,
        std.getopt.config.passThrough,
        std.getopt.config.required,
        "j", "The starting point to generate lucky numbers", &j,
        "k", "The ending point for generating lucky numbers", &k,
        "lucky", "Specify to generate a list of lucky numbers", &luckyOpt,
        "evenLucky", "Specify to generate a list of even lucky numbers", &evenLucky
    );

    if (helpInformation.helpWanted) {
        help(helpInformation.options);
        return;
    }

    if (k>0) {
        lucky(evenLucky).drop(j-1).take(k-j+1).writeln;
    } else if (k<0) {
        auto f = (int a) => j<=a && a<=-k;
        lucky(evenLucky, -k).filter!f.writeln;
    } else {
        lucky(evenLucky).drop(j-1).take(1).writeln;
    }
}
```


```txt
.\lucky_and_even_lucky_numbers.exe -j=1 -k=20
[1, 3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 49, 51, 63, 67, 69, 73, 75, 79]

.\lucky_and_even_lucky_numbers.exe -j=1 -k=20 --evenLucky
[2, 4, 6, 10, 12, 18, 20, 22, 26, 34, 36, 42, 44, 50, 52, 54, 58, 68, 70, 76]

.\lucky_and_even_lucky_numbers.exe -j=6000 -k=-6100
[6009, 6019, 6031, 6049, 6055, 6061, 6079, 6093]

.\lucky_and_even_lucky_numbers.exe -j=6000 -k=-6100 --evenLucky
[6018, 6020, 6022, 6026, 6036, 6038, 6050, 6058, 6074, 6090, 6092]

.\lucky_and_even_lucky_numbers.exe -j=10000
[115591]

.\lucky_and_even_lucky_numbers.exe -j=10000 --evenLucky
[111842]
```



## Go

```go
package main

import (
    "fmt"
    "log"
    "os"
    "strconv"
    "strings"
)

const luckySize = 60000

var luckyOdd = make([]int, luckySize)
var luckyEven = make([]int, luckySize)

func init() {
    for i := 0; i < luckySize; i++ {
        luckyOdd[i] = i*2 + 1
        luckyEven[i] = i*2 + 2
    }
}

func filterLuckyOdd() {
    for n := 2; n < len(luckyOdd); n++ {
        m := luckyOdd[n-1]
        end := (len(luckyOdd)/m)*m - 1
        for j := end; j >= m-1; j -= m {
            copy(luckyOdd[j:], luckyOdd[j+1:])
            luckyOdd = luckyOdd[:len(luckyOdd)-1]
        }
    }
}

func filterLuckyEven() {
    for n := 2; n < len(luckyEven); n++ {
        m := luckyEven[n-1]
        end := (len(luckyEven)/m)*m - 1
        for j := end; j >= m-1; j -= m {
            copy(luckyEven[j:], luckyEven[j+1:])
            luckyEven = luckyEven[:len(luckyEven)-1]
        }
    }
}

func printSingle(j int, odd bool) error {
    if odd {
        if j >= len(luckyOdd) {
            return fmt.Errorf("the argument, %d, is too big", j)
        }
        fmt.Println("Lucky number", j, "=", luckyOdd[j-1])
    } else {
        if j >= len(luckyEven) {
            return fmt.Errorf("the argument, %d, is too big", j)
        }
        fmt.Println("Lucky even number", j, "=", luckyEven[j-1])
    }
    return nil
}

func printRange(j, k int, odd bool) error {
    if odd {
        if k >= len(luckyOdd) {
            return fmt.Errorf("the argument, %d, is too big", k)
        }
        fmt.Println("Lucky numbers", j, "to", k, "are:")
        fmt.Println(luckyOdd[j-1 : k])
    } else {
        if k >= len(luckyEven) {
            return fmt.Errorf("the argument, %d, is too big", k)
        }
        fmt.Println("Lucky even numbers", j, "to", k, "are:")
        fmt.Println(luckyEven[j-1 : k])
    }
    return nil
}

func printBetween(j, k int, odd bool) error {
    var r []int
    if odd {
        max := luckyOdd[len(luckyOdd)-1]
        if j > max || k > max {
            return fmt.Errorf("at least one argument, %d or %d, is too big", j, k)
        }
        for _, num := range luckyOdd {
            if num < j {
                continue
            }
            if num > k {
                break
            }
            r = append(r, num)
        }
        fmt.Println("Lucky numbers between", j, "and", k, "are:")
        fmt.Println(r)
    } else {
        max := luckyEven[len(luckyEven)-1]
        if j > max || k > max {
            return fmt.Errorf("at least one argument, %d or %d, is too big", j, k)
        }
        for _, num := range luckyEven {
            if num < j {
                continue
            }
            if num > k {
                break
            }
            r = append(r, num)
        }
        fmt.Println("Lucky even numbers between", j, "and", k, "are:")
        fmt.Println(r)
    }
    return nil
}

func main() {
    nargs := len(os.Args)
    if nargs < 2 || nargs > 4 {
        log.Fatal("there must be between 1 and 3 command line arguments")
    }
    filterLuckyOdd()
    filterLuckyEven()
    j, err := strconv.Atoi(os.Args[1])
    if err != nil || j < 1 {
        log.Fatalf("first argument, %s, must be a positive integer", os.Args[1])
    }
    if nargs == 2 {
        if err := printSingle(j, true); err != nil {
            log.Fatal(err)
        }
        return
    }

    if nargs == 3 {
        k, err := strconv.Atoi(os.Args[2])
        if err != nil {
            log.Fatalf("second argument, %s, must be an integer", os.Args[2])
        }
        if k >= 0 {
            if j > k {
                log.Fatalf("second argument, %d, can't be less than first, %d", k, j)
            }
            if err := printRange(j, k, true); err != nil {
                log.Fatal(err)
            }
        } else {
            l := -k
            if j > l {
                log.Fatalf("second argument, %d, can't be less in absolute value than first, %d", k, j)
            }
            if err := printBetween(j, l, true); err != nil {
                log.Fatal(err)
            }
        }
        return
    }

    var odd bool
    switch lucky := strings.ToLower(os.Args[3]); lucky {
    case "lucky":
        odd = true
    case "evenlucky":
        odd = false
    default:
        log.Fatalf("third argument, %s, is invalid", os.Args[3])
    }
    if os.Args[2] == "," {
        if err := printSingle(j, odd); err != nil {
            log.Fatal(err)
        }
        return
    }

    k, err := strconv.Atoi(os.Args[2])
    if err != nil {
        log.Fatal("second argument must be an integer or a comma")
    }
    if k >= 0 {
        if j > k {
            log.Fatalf("second argument, %d, can't be less than first, %d", k, j)
        }
        if err := printRange(j, k, odd); err != nil {
            log.Fatal(err)
        }
    } else {
        l := -k
        if j > l {
            log.Fatalf("second argument, %d, can't be less in absolute value than first, %d", k, j)
        }
        if err := printBetween(j, l, odd); err != nil {
            log.Fatal(err)
        }
    }
}
```


```txt

$ ./lucky 1 20
Lucky numbers 1 to 20 are:
[1 3 7 9 13 15 21 25 31 33 37 43 49 51 63 67 69 73 75 79]

$ ./lucky 1 20 evenLucky
Lucky even numbers 1 to 20 are:
[2 4 6 10 12 18 20 22 26 34 36 42 44 50 52 54 58 68 70 76]

$ ./lucky 6000 -6100
Lucky numbers between 6000 and 6100 are:
[6009 6019 6031 6049 6055 6061 6079 6093]

$ ./lucky 6000 -6100 evenLucky
Lucky even numbers between 6000 and 6100 are:
[6018 6020 6022 6026 6036 6038 6050 6058 6074 6090 6092]

$ ./lucky 10000 , lucky
Lucky number 10000 = 115591

$ ./lucky 10000 , evenLucky
Lucky even number 10000 = 111842

```



## Haskell


Haskell is a very nice language for this problem because it is a lazy language. Here regular expressions and data types are used.

```Haskell

import System.Environment
import Text.Regex.Posix

data Lucky = Lucky | EvenLucky

helpMessage :: IO ()
helpMessage = do
  putStrLn "                           what is displayed  (on a single line)"
  putStrLn "     argument(s)              (optional verbiage is encouraged)"
  putStrLn "
### ================
|
### =============================================
"
  putStrLn " j                    | Jth       lucky number                            "
  putStrLn " j  ,          lucky  | Jth       lucky number                            "
  putStrLn " j  ,      evenLucky  | Jth  even lucky number                            "
  putStrLn "                                                                          "
  putStrLn " j  k                 | Jth  through  Kth (inclusive)       lucky numbers "
  putStrLn " j  k          lucky  | Jth  through  Kth (inclusive)       lucky numbers "
  putStrLn " j  k      evenlucky  | Jth  through  Kth (inclusive)  even lucky numbers "
  putStrLn "                                                                          "
  putStrLn " j -k                 | all       lucky numbers in the range  j -> |k|    "
  putStrLn " j -k          lucky  | all       lucky numbers in the range  j -> |k|    "
  putStrLn " j -k      evenlucky  | all  even lucky numbers in the range  j -> |k|    "
  putStrLn "
### ================
|
### =============================================
"

oddNumbers :: [Int]
oddNumbers = filter odd [1..]

evenNumbers :: [Int]
evenNumbers = filter even [1..]

luckyNumbers :: [Int] -> [Int]
luckyNumbers xs =
  let i = 3 in
  sieve i xs
    where
      sieve i (ln:s:xs) =
        ln : sieve (i + 1) (s : [x | (n, x) <- zip [i..] xs, rem n s /= 0])

nth :: Int -> Lucky -> Int
nth j Lucky     = luckyNumbers oddNumbers !! (j-1)
nth j EvenLucky = luckyNumbers evenNumbers !! (j-1)

range :: Int -> Int -> Lucky -> [Int]
range x x2 Lucky     = drop (x-1) (take x2 (luckyNumbers oddNumbers))
range x x2 EvenLucky = drop (x-1) (take x2 (luckyNumbers evenNumbers))

interval :: Int -> Int -> Lucky -> [Int]
interval x x2 Lucky     = dropWhile (<x) (takeWhile (<=x2) (luckyNumbers oddNumbers))
interval x x2 EvenLucky = dropWhile (<x) (takeWhile (<=x2) (luckyNumbers evenNumbers))

lucky :: [String] -> Lucky
lucky xs =
  if "evenLucky" `elem` xs
   then EvenLucky
   else Lucky

readn :: String -> Int
readn s = read s :: Int

isInt :: String -> Bool
isInt s = not (null (s =~ "-?[0-9]{0,10}" :: String))

main :: IO ()
main = do
  args <- getArgs
  if head args == "--help" || null args
    then
      helpMessage
    else
      let l = lucky args in
      case map readn (filter isInt args) of
        [] -> do
          putStrLn "Invalid input, missing arguments"
          putStrLn "Type --help"
        [x] -> print (nth x l)
        [x, x2] -> if x2 > 0
          then print (range x x2 l)
          else print (interval x (-x2) l)
        _ -> do
          putStrLn "Invalid input, wrong number of arguments"
          putStrLn "Type --help"
```

```txt
$ luckyNumbers 1 20
[1,3,7,9,13,15,21,25,31,33,37,43,49,51,63,67,69,73,75,79]
$ luckyNumbers 1 20 evenLucky
[2,4,6,10,12,18,20,22,26,34,36,42,44,50,52,54,58,68,70,76]
$ luckyNumbers 6000 -6100 lucky
[6009,6019,6031,6049,6055,6061,6079,6093]
$ luckyNumbers 6000 -6100 evenLucky
[6018,6020,6022,6026,6036,6038,6050,6058,6074,6090,6092]
$ luckyNumbers 10000
115591
$ luckyNumbers 10000 evenLucky
111842
```



## J


Not going for extra credit because I want to encourage functional reactive "types" in J. (Note that FRP, along with an ML typed compiler, would probably remove the motivation for the while loop in this implementation.)

Implementation:


```J
luckySeq=:3 :0
  1 luckySeq y
:
  len=.0
  nth=.0
  seq=.x+2*i.4*y
  while.  len~:#seq do.
    len=. #seq
    nth=. nth+1
    seq=. nth exclude seq
  end.
)

exclude=: ] #~ 1 - #@] $ -@{ {. 1:

lucky=:''
evenLucky=:0
program=:3 :0
  range=: |y-.0
  seq=. (1+0 e.y) luckySeq >./range
  if. 0><./y do.
    (#~ e.&(thru/range)) seq
  else.
    (<:thru/range) { seq
  end.
)

thru=: <./ + i.@(+*)@-~
```


Task:


```J
   program 1 20
1 3 7 9 13 15 21 25 31 33 37 43 49 51 63 67 69 73 75 79
   program 1 20,evenLucky
2 4 6 10 12 18 20 22 26 34 36 42 44 50 52 54 58 68 70 76
   program 6000,-6100
6009 6019 6031 6049 6055 6061 6079 6093
   program 6000,-6100,evenLucky
6018 6020 6022 6026 6036 6038 6050 6058 6074 6090 6092
```


Note that I've used the J command line rather than a unix or windows command line. This is because J is portable to a wide variety of environments (including phones) and there's no reliably common command line that exists across all these environments. Therefore, J must provide its own, and J's command line requires some slight syntax changes from the suggestions implicit in this task.


## Julia

This iterator for lucky numbers is semi-lazy: it completes one pass of the filter each iteration.

```julia
using Base, StringDistances

struct Lucky
    start::Int
    nmax::Int
    Lucky(iseven, nmax) = new(iseven ? 2 : 1, nmax)
end

struct LuckyState
    nextindex::Int
    sequence::Vector{Int}
end

Base.eltype(iter::Lucky) = Int

function Base.iterate(iter::Lucky, state = LuckyState(1, collect(iter.start:2:iter.nmax)))
    if length(state.sequence) < state.nextindex
        return nothing
    elseif state.nextindex == 1
        return (iter.start, LuckyState(2, state.sequence))
    end
    result = state.sequence[state.nextindex]
    newsequence = Vector{Int}()
    for (i, el) in enumerate(state.sequence)
        if i % result != 0
            push!(newsequence, el)
        end
    end
    (result, LuckyState(state.nextindex + 1, newsequence))
end

function luckyindex(j, wanteven, k=0)
    topindex = max(j, k) + 4
    luck = Lucky(wanteven, topindex * 20)
    iter_result = iterate(luck)
    while iter_result != nothing
        (elem, state) = iter_result
        iter_result = iterate(luck, state)
        if iter_result != nothing && iter_result[2].nextindex > topindex
            return iter_result[2].sequence[k > j ? (j:k) : j]
        end
    end
    throw("Index $j out of range for nmax of $(luck.nmax).")
end

function luckyrange(j, k, wanteven)
    topvalue = max(j, k)
    luck = Lucky(wanteven, topvalue + 1)
    iter_result = iterate(luck)
    (elem, state) = iter_result # save next to last result
    while iter_result != nothing
        (elem, state) = iter_result
        iter_result = iterate(luck, state)
    end
    filter(x -> (j <= x <= k), state.sequence)
end

function helpdisplay(exitlevel=1)
    println("\n", PROGRAM_FILE, " j [-][k] [lucky|evenLucky]")
    println("\tj: index wanted or a starting point (index or value)",
            "\n\tk: optional ending point (index), \n\t-k: optional ending point (value)\n")
    helpstring =
""" | Argument(s)        |    What is printed                                  |
 |--------------------------------------------------------------------------|
 | j                  |  jth lucky number (required argument)               |
 | j , lucky          |  jth lucky number                                   |
 | j , evenLucky      |  jth even lucky number                              |
 | j  k               |  jth through kth (inclusive) lucky numbers          |
 | j  k  lucky        |  jth through kth (inclusive) lucky numbers          |
 | j  k  evenLucky    |  jth through kth (inclusive) even lucky numbers     |
 | j  -k              |  all lucky numbers in the value range [m, |k|]      |
 | j  -k  lucky       |  all lucky numbers in the value range [m, |k|]      |
 | j  -k  evenLucky   |  all even lucky numbers in the value range [m, |k|] |
 |--------------------------------------------------------------------------|\n\n"""

    println(helpstring)
    exit(exitlevel)
end

function parsecommandline()
    comma = false
    evenLucky = false
    range = false
    j = k = 0
    if length(ARGS) < 1
        helpdisplay()
    end
    for (pos, arg) in enumerate(ARGS)
        if pos == 1
            j = tryparse(Int, arg)
            if j == nothing
                println("The first argument must be a positive integer.\n")
                helpdisplay()
            end
        elseif pos == 2 || (pos == 3 && comma)
            k = tryparse(Int, arg)
            if k == nothing
                k = 0
                if arg == ","
                    comma = true
                    continue
                elseif arg == "lucky"
                    continue
                elseif arg == "evenLucky"
                    evenLucky = true
                elseif compare(Hamming(), arg, "lucky") > 0.4 || compare(Hamming(), arg, "evenLucky") > 0.4
                    println("Did you misspell \"lucky\" or \"evenLucky\"? Check capitalization.\n")
                    helpdisplay()
                else
                    helpdisplay()
                end
            elseif k < 0
                k = -k
                range = true
            end
        elseif pos == 3 || pos == 4 && comma
            if arg == ","
                comma = true
                continue
            elseif arg == "lucky"
                continue
            elseif arg == "evenLucky"
                evenLucky = true
            elseif compare(Hamming(), arg, "lucky") > 0.1 || compare(Hamming(), arg, "evenLucky") > 0.1
                println("Did you misspell "\lucky\" or "\evenLucky\"?\n\n")
                helpdisplay()
            else
                helpdisplay()
            end
        elseif arg == "lucky"
                continue
        elseif arg == "evenLucky"
                evenLucky = true
        else
            println("Too many arguments.\n")
            helpdisplay()
        end
    end
    (j, k, evenLucky, range)
end

function runopts()
    (j, k, evenLucky, range) = parsecommandline()
    if j < 1 || (k != 0 && j >= k)
        throw("Lucky number integer parameters out of range: $(typeof(j)), $j, $(typeof(k)), $k")
    end
    if range
        println(luckyrange(j, k, evenLucky))
    else
        println(luckyindex(j, evenLucky, k))
    end
end

runopts()
```
 {{output}}
```txt

> julia luckymath.jl 1 20
[1, 3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 49, 51, 63, 67, 69, 73, 75, 79]
> julia luckymath.jl 1 20 evenLucky
[2, 4, 6, 10, 12, 18, 20, 22, 26, 34, 36, 42, 44, 50, 52, 54, 58, 68, 70, 76]
> julia luckymath.jl 6000 -6100
[6009, 6019, 6031, 6049, 6055, 6061, 6079, 6093]
> julia luckymath.jl 6000 -6100 evenLucky
[6018, 6020, 6022, 6026, 6036, 6038, 6050, 6058, 6074, 6090, 6092]
> julia luckymath.jl 10000
115591
> julia luckymath.jl 10000 evenLucky
111842

```



## Kotlin


```scala
// version 1.1.51

typealias IAE = IllegalArgumentException

val luckyOdd  = MutableList(100000) { it * 2 + 1 }
val luckyEven = MutableList(100000) { it * 2 + 2 }

fun filterLuckyOdd() {
    var n = 2
    while (n < luckyOdd.size) {
        val m = luckyOdd[n - 1]
        val end = (luckyOdd.size / m) * m - 1
        for (j in end downTo m - 1 step m) luckyOdd.removeAt(j)
        n++
    }
}

fun filterLuckyEven() {
    var n = 2
    while (n < luckyEven.size) {
        val m = luckyEven[n - 1]
        val end = (luckyEven.size / m) * m - 1
        for (j in end downTo m - 1 step m) luckyEven.removeAt(j)
        n++
    }
}

fun printSingle(j: Int, odd: Boolean) {
    if (odd) {
        if (j >= luckyOdd.size) throw IAE("Argument is too big")
        println("Lucky number $j = ${luckyOdd[j - 1]}")
    }
    else {
        if (j >= luckyEven.size) throw IAE("Argument is too big")
        println("Lucky even number $j = ${luckyEven[j - 1]}")
    }
}

fun printRange(j: Int, k: Int, odd: Boolean) {
    if (odd) {
        if (k >= luckyOdd.size) throw IAE("Argument is too big")
        println("Lucky numbers $j to $k are:\n${luckyOdd.drop(j - 1).take(k - j + 1)}")
    }
    else {
        if (k >= luckyEven.size) throw IAE("Argument is too big")
        println("Lucky even numbers $j to $k are:\n${luckyEven.drop(j - 1).take(k - j + 1)}")
    }
}

fun printBetween(j: Int, k: Int, odd: Boolean) {
    val range = mutableListOf<Int>()
    if (odd) {
        val max = luckyOdd[luckyOdd.lastIndex]
        if (j > max || k > max) {
            throw IAE("At least one argument is too big")
        }
        for (num in luckyOdd) {
            if (num < j) continue
            if (num > k) break
            range.add(num)
        }
        println("Lucky numbers between $j and $k are:\n$range")
    }
    else {
        val max = luckyEven[luckyEven.lastIndex]
        if (j > max || k > max) {
            throw IAE("At least one argument is too big")
        }
        for (num in luckyEven) {
            if (num < j) continue
            if (num > k) break
            range.add(num)
        }
        println("Lucky even numbers between $j and $k are:\n$range")
    }
}

fun main(args: Array<String>) {
    if (args.size !in 1..3) throw IAE("There must be between 1 and 3 command line arguments")
    filterLuckyOdd()
    filterLuckyEven()
    val j = args[0].toIntOrNull()
    if (j == null || j < 1) throw IAE("First argument must be a positive integer")
    if (args.size == 1) { printSingle(j, true); return }

    if (args.size == 2) {
        val k = args[1].toIntOrNull()
        if (k == null) throw IAE("Second argument must be an integer")
        if (k >= 0) {
            if (j > k) throw IAE("Second argument can't be less than first")
            printRange(j, k, true)
        }
        else {
           val l = -k
            if (j > l) throw IAE("The second argument can't be less in absolute value than first")
            printBetween(j, l, true)
        }
        return
    }

    var odd =
        if (args[2].toLowerCase() == "lucky") true
        else if (args[2].toLowerCase() == "evenlucky") false
        else throw IAE("Third argument is invalid")

    if (args[1] == ",") {
        printSingle(j, odd)
        return
    }

    val k = args[1].toIntOrNull()
    if (k == null) throw IAE("Second argument must be an integer or a comma")

    if (k >= 0) {
        if (j > k) throw IAE("Second argument can't be less than first")
        printRange(j, k, odd)
    }
    else {
        val l = -k
        if (j > l) throw IAE("The second argument can't be less in absolute value than first")
        printBetween(j, l, odd)
    }
}
```


```txt

$ java -jar lucky.jar 1 20
Lucky numbers 1 to 20 are:
[1, 3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 49, 51, 63, 67, 69, 73, 75, 79]

$ java -jar lucky.jar 1 20 evenLucky
Lucky even numbers 1 to 20 are:
[2, 4, 6, 10, 12, 18, 20, 22, 26, 34, 36, 42, 44, 50, 52, 54, 58, 68, 70, 76]

$ java -jar lucky.jar 6000 -6100
Lucky numbers between 6000 and 6100 are:
[6009, 6019, 6031, 6049, 6055, 6061, 6079, 6093]

$ java -jar lucky.jar 6000 -6100 evenLucky
Lucky even numbers between 6000 and 6100 are:
[6018, 6020, 6022, 6026, 6036, 6038, 6050, 6058, 6074, 6090, 6092]

$ java -jar lucky.jar 10000 , lucky
Lucky number 10000 = 115591

$ java -jar lucky.jar 10000 , evenLucky
Lucky even number 10000 = 111842

```



## Perl

The module <code>Perl6::GatherTake</code> emulates the Perl 6 gather/take syntax, and allows us to access values from what acts (mostly) like a lazy list.
```perl
use Perl6::GatherTake;

sub luck {
my($a,$b) = @_;

gather {
    my $i = $b;
    my(@taken,@rotor,$j);

    take 0; # 0th index is a placeholder
    push @taken, take $a;

    while () {
        for ($j = 0; $j < @rotor; $j++) {
            --$rotor[$j] or last;
        }
        if ($j < @rotor) {
            $rotor[$j] = $taken[$j+1];
        }
        else {
            take $i;
            push @taken, $i;
            push @rotor, $i - @taken;
        }
        $i += 2;
    }
}
}

# fiddle with user input
$j = shift || usage();
$k = shift || ',';
$l = shift || 'lucky';
usage() unless $k =~ /,|-?\d+/;
usage() unless $l =~ /^(even)?lucky$/i;
sub usage { print "Args must be:  j [,|k|-k] [lucky|evenlucky]\n" and exit }

# seed the iterator
my $lucky = $l =~ /^l/i ? luck(1,3) : luck(2,4);

# access values from 'lazy' list
if ($k eq ',') {
    print $lucky->[$j]
} elsif ($k > $j) {
    print $lucky->[$_] . ' ' for $j..$k
} elsif ($k < 0) {
    while () { last if abs($k) < $lucky->[$i++] } # must first extend the array
    print join ' ', grep { $_ >= $j and $_ <= abs($k) } @$lucky
}

print "\n"
```

```txt
$ ./lucky
Args must be:  j [,|k|-k] [lucky|evenlucky]
$ ./lucky 20 , lucky
79
$ ./lucky 20 , evenlucky
76
$ ./lucky 1 20
1 3 7 9 13 15 21 25 31 33 37 43 49 51 63 67 69 73 75 79
$ ./lucky 1 20 evenlucky
2 4 6 10 12 18 20 22 26 34 36 42 44 50 52 54 58 68 70 76
$ ./lucky 6000 -6100
6009 6019 6031 6049 6055 6061 6079 6093
$ ./lucky 6000 -6100 evenLucky
6018 6020 6022 6026 6036 6038 6050 6058 6074 6090 6092
$ ./lucky 10000
115591
$ ./lucky 10000 , EVENLUCKY
111842
```



## Perl 6


```perl6
sub luck(\a,\b) {
    gather {
	my @taken = take a;
	my @rotor;
	my $i = b;

	loop {
	    loop (my $j = 0; $j < @rotor; $j++) {
		--@rotor[$j] or last;
	    }
	    if $j < @rotor {
		@rotor[$j] = @taken[$j+1];
	    }
	    else {
		push @taken, take $i;
		push @rotor, $i - @taken;
	    }
	    $i += 2;
	}
    }
}

constant @lucky = luck(1,3);
constant @evenlucky = luck(2,4);

subset Luck where m:i/^ 'even'? 'lucky' $/;

multi MAIN (Int $num where * > 0) {
    say @lucky[$num-1];
}

multi MAIN (Int $num where * > 0, ',', Luck $howlucky = 'lucky') {
    say @::(lc $howlucky)[$num-1];
}

multi MAIN (Int $first where * > 0, Int $last where * > 0, Luck $howlucky = 'lucky') {
    say @::(lc $howlucky)[$first-1 .. $last - 1];
}

multi MAIN (Int $min where * > 0, Int $neg-max where * < 0, Luck $howlucky = 'lucky') {
    say grep * >= $min, (@::(lc $howlucky) ...^ * > abs $neg-max);
}
```

```txt
$ ./lucky
Usage:
  ./lucky <num>
  ./lucky <num> , [<howlucky>]
  ./lucky <first> <last> [<howlucky>]
  ./lucky <min> <neg-max> [<howlucky>]
$ ./lucky 20 , lucky
79
$ ./lucky 20 , evenlucky
76
$ ./lucky 1 20
1 3 7 9 13 15 21 25 31 33 37 43 49 51 63 67 69 73 75 79
$ ./lucky 1 20 evenlucky
2 4 6 10 12 18 20 22 26 34 36 42 44 50 52 54 58 68 70 76
$ ./lucky 6000 -6100
6009 6019 6031 6049 6055 6061 6079 6093
$ ./lucky 6000 -6100 evenLucky
6018 6020 6022 6026 6036 6038 6050 6058 6074 6090 6092
$ ./lucky 10000
115591
$ ./lucky 10000 , EVENLUCKY
111842
```



## Phix


```Phix
constant luckyMax = 120000

sequence lucky

procedure filterLucky()
    integer n = 2
    while n<=length(lucky) do
        integer m = lucky[n],
                l = m-1
        for k=m+1 to length(lucky) do
            if mod(k,m)!=0 then
                l += 1
                lucky[l] = lucky[k]
            end if
        end for
        if l>=length(lucky) then exit end if
        lucky = lucky[1..l]
        n += 1
    end while
end procedure

constant helptxt = """
   argument(s)   |  what is displayed

### =================================

 j               |  jth lucky number
 j [,] lucky     |  jth lucky number
 j [,] evenLucky |  jth even lucky number
 j k             |  jth through kth (inclusive) lucky numbers
 j k lucky       |  jth through kth (inclusive) lucky numbers
 j k evenLucky   |  jth through kth (inclusive) even lucky numbers
 j -k            |  all lucky numbers in the range j to k
 j -k lucky      |  all lucky numbers in the range j to k
 j -k evenLucky  |  all even lucky numbers in the range j to k
"""

procedure fatal(string msg)
    puts(1,msg)
    puts(1,helptxt)
    {} = wait_key()
    abort(0)
end procedure

procedure main()
    sequence cl = command_line()
    integer j,k,l,m,n
    bool single = true, range = true, odd = true
    if length(cl)=2 then
--      fatal("at least one argument must be supplied") -- (if preferred)
        sequence tests = {"1 20",
                          "1 20 evenLucky",
                          "20 lucky",
                          "20 evenLucky",
                          "6000 -6100",
                          "6000 -6100 evenLucky",
                          "10000 lucky",
                          "10000 evenLucky"}
        -- (done this way to exercise the real command line handling)
        if cl[1]=cl[2] then                     -- (compiled)
            cl = cl[1..1]
        elsif platform()=WINDOWS then           -- (and interpreted)
            cl[1] = substitute(cl[1],"pw","p")  -- (pw.exe -> p.exe)
        end if
        for i=1 to length(cl) do
            if find(' ',cl[i]) then cl[i] = sprintf("\"%s\"",{cl[i]}) end if
        end for
        for t=1 to length(tests) do
            string cmd = join(append(cl,tests[t]))
--          printf(1,"running %s\n",{cmd})
            {} = system_exec(cmd)
        end for
        puts(1, "tests complete\n")
        {} = wait_key()
    else
        cl = cl[3..$] -- ({1,2} are {interperter,source} or {exe,exe})

        --
        -- Allow eg "lucky j , evenLucky" to be == "lucky j evenLucky"
        --
        if length(cl)=3 and cl[2]="," then cl[2..2] = {} end if

        for i=1 to length(cl) do
            string cli = cl[i]
            if cli[1]<='9' then     -- (includes '-')
                sequence d = scanf(cl[i],"%d")
                if length(d)!=1 then
                    fatal("unrecognised "&cli)
                end if
                if i>2 then
                    fatal("too many numbers")
                end if
                n = d[1][1]
                if i=1 then
                    if n<1 then
                        fatal("first argument must be a positive integer")
                    end if
                    j = n
                else
                    single = false
                    if n<0 then
                        range = false
                        n = -n
                    end if
                    if n<j then
                        fatal("second argument cannot be less than first")
                    end if
                    k = n
                end if
            else
                l = find(cli,{"lucky","evenLucky"})
                if l=0 then
                    fatal("unrecognised "&cli)
                end if
                if i!=length(cl) then
                    fatal(cli&" must be last parameter")
                end if
                odd = (l=1)
            end if
        end for

        lucky = tagset(luckyMax,2-odd,2)
        filterLucky()
        printf(1,"Output when args are %s\n",{join(cl)})
        string even = iff(odd?"":"even ")
        if single then
            if j>length(lucky) then
                fatal(sprintf("the argument, %d, is too big", j))
            end if
            printf(1,"Lucky %snumber %d = %d\n",{even,j, lucky[j]})
        elsif range then
            if k>length(lucky) then
                fatal(sprintf("the argument, %d, is too big", k))
            end if
            printf(1,"Lucky %snumbers %d to %d are: %s\n",{even,j,k,sprint(lucky[j..k])})
        else
            if j>lucky[$] then
                fatal("start of range is too big")
            elsif k>lucky[$] then
                fatal("end of range is too big")
            end if
            m = abs(binary_search(j,lucky))
            n = binary_search(k,lucky)
            if n<0 then n = -n-1 end if
            printf(1,"Lucky %snumbers between %d and %d are: %s\n", {even,j,k,sprint(lucky[m..n])})
        end if
    end if
end procedure
main()
```

```txt

Output when args are 1 20
Lucky numbers 1 to 20 are: {1,3,7,9,13,15,21,25,31,33,37,43,49,51,63,67,69,73,75,79}
Output when args are 1 20 evenLucky
Lucky even numbers 1 to 20 are: {2,4,6,10,12,18,20,22,26,34,36,42,44,50,52,54,58,68,70,76}
Output when args are 20 lucky
Lucky number 20 = 79
Output when args are 20 evenLucky
Lucky even number 20 = 76
Output when args are 6000 -6100
Lucky numbers between 6000 and 6100 are: {6009,6019,6031,6049,6055,6061,6079,6093}
Output when args are 6000 -6100 evenLucky
Lucky even numbers between 6000 and 6100 are: {6018,6020,6022,6026,6036,6038,6050,6058,6074,6090,6092}
Output when args are 10000 lucky
Lucky number 10000 = 115591
Output when args are 10000 evenLucky
Lucky even number 10000 = 111842
tests complete

```



## PicoLisp


```PicoLisp
(off *Even)

(de nn (Lst N)
   (seek
      '((L)
         (when (car L) (=0 (dec 'N))) )
      Lst ) )

(de lucky (B)
   (let Lst (range (if *Even 2 1) B 2)
      (for A (cdr Lst)
         (for (L (nn Lst A) L (nn (cdr L) A))
            (set L) ) )
      (filter bool Lst) ) )

(argv . *Argv) # without validations
(when (= "evenLucky" (last *Argv)) (on *Even))
(setq *Lst (lucky 7000))
(let (A (format (car *Argv))  B (format (cadr *Argv)))
   (println
      (if (lt0 B)
         (filter '((N) (<= A N (abs B))) *Lst)
         (head B (nth *Lst A)) ) ) )
```

```txt
$ pil ./lucky.l 1 20
(1 3 7 9 13 15 21 25 31 33 37 43 49 51 63 67 69 73 75 79)
$ pil ./lucky.l 1 20 evenLucky
(2 4 6 10 12 18 20 22 26 34 36 42 44 50 52 54 58 68 70 76)
$ pil ./lucky.l 6000 -6100
(6009 6019 6031 6049 6055 6061 6079 6093)
$ pil ./lucky.l 6000 -6100 evenLucky
(6018 6020 6022 6026 6036 6038 6050 6058 6074 6090 6092)
```



## Python

The generator

```python
from __future__ import print_function

def lgen(even=False, nmax=1000000):
    start = 2 if even else 1
    n, lst = 1, list(range(start, nmax + 1, 2))
    lenlst = len(lst)
    yield lst[0]
    while n < lenlst and lst[n] < lenlst:
        yield lst[n]
        n, lst = n + 1, [j for i,j in enumerate(lst, 1) if i % lst[n]]
        lenlst = len(lst)
    # drain
    for i in lst[n:]:
        yield i
```


The argument handler

```python
from itertools import islice
import sys, re

class ArgumentError(Exception):
    pass
def arghandler(argstring):
    match_obj = re.match( r"""(?mx)
    (?:
      (?P<SINGLE>
         (?: ^ (?P<SINGLEL> \d+ ) (?:  | \s , \s lucky ) \s* $ )
        |(?: ^ (?P<SINGLEE> \d+ ) (?:  | \s , \s evenLucky ) \s* $ )
      )
     |(?P<KTH>
         (?: ^ (?P<KTHL> \d+ \s \d+ ) (?:  | \s lucky ) \s* $ )
        |(?: ^ (?P<KTHE> \d+ \s \d+ ) (?:  | \s evenLucky ) \s* $ )
      )
     |(?P<RANGE>
         (?: ^ (?P<RANGEL> \d+ \s -\d+ ) (?:  | \s lucky ) \s* $ )
        |(?: ^ (?P<RANGEE> \d+ \s -\d+ ) (?:  | \s evenLucky ) \s* $ )
      )
    )""", argstring)

    if match_obj:
        # Retrieve group(s) by name
        SINGLEL = match_obj.group('SINGLEL')
        SINGLEE = match_obj.group('SINGLEE')
        KTHL = match_obj.group('KTHL')
        KTHE = match_obj.group('KTHE')
        RANGEL = match_obj.group('RANGEL')
        RANGEE = match_obj.group('RANGEE')
        if SINGLEL:
            j = int(SINGLEL)
            assert 0 < j < 10001, "Argument out of range"
            print("Single %i'th lucky number:" % j, end=' ')
            print( list(islice(lgen(), j-1, j))[0] )
        elif SINGLEE:
            j = int(SINGLEE)
            assert 0 < j < 10001, "Argument out of range"
            print("Single %i'th even lucky number:" % j, end=' ')
            print( list(islice(lgen(even=True), j-1, j))[0] )
        elif KTHL:
            j, k = [int(num) for num in KTHL.split()]
            assert 0 < j < 10001, "first argument out of range"
            assert 0 < k < 10001 and k > j, "second argument out of range"
            print("List of %i ... %i lucky numbers:" % (j, k), end=' ')
            for n, luck in enumerate(lgen(), 1):
                if n > k: break
                if n >=j: print(luck, end = ', ')
            print('')
        elif KTHE:
            j, k = [int(num) for num in KTHE.split()]
            assert 0 < j < 10001, "first argument out of range"
            assert 0 < k < 10001 and k > j, "second argument out of range"
            print("List of %i ... %i even lucky numbers:" % (j, k), end=' ')
            for n, luck in enumerate(lgen(even=True), 1):
                if n > k: break
                if n >=j: print(luck, end = ', ')
            print('')
        elif RANGEL:
            j, k = [int(num) for num in RANGEL.split()]
            assert 0 < j < 10001, "first argument out of range"
            assert 0 < -k < 10001 and -k > j, "second argument out of range"
            k = -k
            print("List of lucky numbers in the range %i ... %i :" % (j, k), end=' ')
            for n in lgen():
                if n > k: break
                if n >=j: print(n, end = ', ')
            print('')
        elif RANGEE:
            j, k = [int(num) for num in RANGEE.split()]
            assert 0 < j < 10001, "first argument out of range"
            assert 0 < -k < 10001 and -k > j, "second argument out of range"
            k = -k
            print("List of even lucky numbers in the range %i ... %i :" % (j, k), end=' ')
            for n in lgen(even=True):
                if n > k: break
                if n >=j: print(n, end = ', ')
            print('')
    else:
        raise ArgumentError('''

  Error Parsing Arguments!

  Expected Arguments of the form (where j and k are integers):

      j                #  Jth       lucky number
      j  ,      lucky  #  Jth       lucky number
      j  ,  evenLucky  #  Jth  even lucky number
                       #
      j  k             #  Jth  through  Kth (inclusive)       lucky numbers
      j  k      lucky  #  Jth  through  Kth (inclusive)       lucky numbers
      j  k  evenLucky  #  Jth  through  Kth (inclusive)  even lucky numbers
                       #
      j -k             #  all       lucky numbers in the range  j --? |k|
      j -k      lucky  #  all       lucky numbers in the range  j --? |k|
      j -k  evenLucky  #  all  even lucky numbers in the range  j --? |k|
        ''')

if __name__ == '__main__':
    arghandler(' '.join(sys.argv[1:]))
```


```txt
# Output when arguments are: 1 20 lucky
List of 1 ... 20 lucky numbers: 1, 3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 49, 51, 63, 67, 69, 73, 75, 79,
# Output when arguments are: 1 20 evenLucky
List of 1 ... 20 even lucky numbers: 2, 4, 6, 10, 12, 18, 20, 22, 26, 34, 36, 42, 44, 50, 52, 54, 58, 68, 70, 76,
# Output when arguments are: 6000 -6100 lucky
List of lucky numbers in the range 6000 ... 6100 : 6009, 6019, 6031, 6049, 6055, 6061, 6079, 6093,
# Output when arguments are: 6000 -6100 evenLucky
List of even lucky numbers in the range 6000 ... 6100 : 6018, 6020, 6022, 6026, 6036, 6038, 6050, 6058, 6074, 6090, 6092,
# Output when arguments are: 10000
Single 10000'th lucky number: 115591
# Output when arguments are: 10000 , evenLucky
Single 10000'th even lucky number: 111842
```



## REXX

This REXX version does extra error checking for the arguments.

```REXX
/*REXX program displays  lucky  or  evenLucky  integers   (numbers  or  a number range).*/
parse arg bot top func _ .                       /*obtain required & optional arguments.*/
if func==''  then func='lucky'                   /*Not specified?  Then use the default.*/
s=left('s', bot\==top  &  top\==",")             /*plural results (or maybe not plural).*/
say func  'number's":"   bot  top   '───►'   $lucky(bot, top, func, _)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
$lucky: arg x,y,f,?;  if y=='' | y==","  then y=x        /*obtain some arguments; set Y.*/
        #=0;        $=;      ny=y<0                      /*set variable NOY: value range*/
        if f==''  then f='LUCKY';  lucky= (f=="LUCKY")   /*assume  LUCKY  if omitted.   */
        if f\=='LUCKY' & f\=='EVENLUCKY'  then return  'function not valid: '     f
        if arg()>3  &  ?\=''      then return  "too many arguments entered: "     ?
        if x=''                   then return  "1st argument is missing."
        if x<1                    then return  "1st argument isn't a positive integer: " x
        if \datatype(x,'W')       then return  "1st argument isn't an integer: "  x
        if \datatype(y,'W')       then return  "2nd argument isn't an integer: "  y
        if x>ay                   then return  "2nd argument is less than 1st arg."
        ay=abs(y); yL=ay; if y>0  then yL=y*10+y+y       /*adjust the upper  Y  limit.  */
                                                         /* [↓]  build LUCKY | EVENLUCKY*/
            do j=1  until j>=yL                          /*construct list pos. integers.*/
            if j//2==(\lucky)  then iterate              /*EVENLUCKY? Use only even ints*/

            if lucky  then if (j+1)//6==0  then iterate  /*prune       if  mod 6 ≡ zero.*/
                                           else nop      /*balance the   IF-THEN  logic.*/
                      else if  j   //8==0  then iterate  /*prune next  if  mod 8 ≡ zero.*/
            #=#+1                                        /*bump the counter of numbers. */
            $=$ j                                        /*append integer to the $ list.*/
            end   /*j*/
        q=0
            do p=3  until  q=='';     q=word($,p)        /*start to prune  integer list.*/
            if q>#  then leave                           /*if integer is too large, stop*/
                               do j=#%q*q  by -q  to q   /*elide every  Qth  integer.   */
                               $=delword($, j, 1)        /*delete a particular number.  */
                               #=#-1                     /*decrease the integer count.  */
                               end   /*j*/               /*delete from the right side.  */
            end   /*p*/
        @.=
                  do k=1; parse var $ q $;  if q==''  then leave;  @.k=q;  end  /*k*/
        @.0=k-1
                  do m=1  for #                          /*restrict the found integers. */
                  if (\ny  &  (m<x  |  m>ay))  |  (ny  &  (@.m<x | @.m>ay))  then @.m=
                  end   /*m*/                            /* [↑]  a list of #s or a range*/
        _=
                  do b=1  for @.0; _=_ @.b; end  /*b*/   /*construct a list of integers.*/
        return space(_)                                  /*remove superfluous blanks.   */
```

'''output'''   when the input is:   <tt> 1   20   lucky </tt>

```txt

lucky numbers: 1 20 ───► 1 3 7 9 13 15 21 25 31 33 37 43 49 51 63 67 69 73 75 79

```

'''output'''   when the input is:   <tt> 1   20   evenLucky </tt>

```txt

evenLucky numbers: 1 20 ───► 2 4 6 10 12 18 20 22 26 34 36 42 44 50 52 54 58 68 70 76

```

'''output'''   when the input is:   <tt> 6000   -6100   lucky</tt>

```txt

lucky numbers: 6000 -6100 ───► 6009 6019 6031 6049 6055 6061 6079 6093

```

'''output'''   when the input is:   <tt> 6000   -6100   venLucky</tt>

```txt

evenLucky numbers: 6000 -6100 ───► 6018 6020 6022 6026 6036 6038 6050 6058 6074 6090 6092

```

'''output'''   when the input is:   <tt> 10000 </tt>

```txt

lucky number: 10000 ───► 115591

```

'''output'''   when the input is:   <tt> 10000   ,   evenLucky </tt>

```txt

evenLucky number: 10000 ───► 111842

```



## Ring


```ring

# Project : Lucky and even lucky numbers

lucky = list(50)
dellucky = []
for n = 1 to 50
     lucky[n] = 2*n-1
next
see "the first 20 lucky numbers:" + nl
luckynumbers(lucky)
showarray(lucky)
see nl

lucky = list(50)
dellucky = []
for n = 1 to 50
     lucky[n] = 2*n
next
see "the first 20 even lucky numbers:" + nl
luckynumbers(lucky)
showarray(lucky)
see nl

lucky = list(20000)
dellucky = []
for n = 1 to 10000
     lucky[n] = 2*n-1
next
see "lucky numbers between 6,000 and 6,100:" + nl
luckynumbers2(lucky)
showarray2(lucky)
see nl

lucky = list(20000)
dellucky = []
for n = 1 to 10000
     lucky[n] = 2*n
next
see "even lucky numbers between 6,000 and 6,100:" + nl
luckynumbers2(lucky)
showarray2(lucky)
see nl

func luckynumbers(lucky)
      for n = 2 to len(lucky)
          dellucky = []
          for m = lucky[n] to len(lucky) step lucky[n]
              add(dellucky, m)
          next
          for p = len(dellucky)  to 1 step -1
              del(lucky, dellucky[p])
          next
      next

func luckynumbers2(lucky)
      for n = 2 to len(lucky)
          dellucky = []
          for m = lucky[n] to len(lucky) step lucky[n]
              add(dellucky, m)
          next
          for p = len(dellucky)  to 1 step -1
              del(lucky, dellucky[p])
          next
          if lucky[n] >= 6100
             exit
          ok
next

func showarray(vect)
      see "["
      svect = ""
      for n = 1 to 20
          svect = svect + vect[n] + ", "
      next
      svect = left(svect, len(svect) - 2)
      see svect
      see "]" + nl

func showarray2(vect)
      see "["
      svect = ""
      for n = 1 to len(vect)
          if vect[n] >= 6000 and vect[n] <= 6100
             svect = svect + vect[n] + ", "
          ok
      next
      svect = left(svect, len(svect) - 2)
      see svect
      see "]" + nl

```

Output:

```txt

the first 20 lucky numbers:
[1, 3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 49, 51, 63, 67, 69, 73, 75, 79]

the first 20 even lucky numbers:
[2, 4, 6, 10, 12, 18, 20, 22, 26, 34, 36, 42, 44, 50, 52, 54, 58, 68, 70, 76]

lucky numbers between 6,000 and 6,100:
[6009, 6019, 6031, 6049, 6055, 6061, 6079, 6093]

even lucky numbers between 6,000 and 6,100:
[6018, 6020, 6022, 6026, 6036, 6038, 6050, 6058, 6074, 6090, 6092]

```



## Ruby

```ruby
def generator(even=false, nmax=1000000)
  start = even ? 2 : 1
  Enumerator.new do |y|
    n = 1
    ary = [0] + (start..nmax).step(2).to_a      # adds [0] to revise the 0 beginnings.
    y << ary[n]
    while (m = ary[n+=1]) < ary.size
      y << m
      (m...ary.size).step(m){|i| ary[i]=nil}
      ary.compact!                              # remove nil
    end
    # drain
    ary[n..-1].each{|i| y << i}
    raise StopIteration
  end
end

def lucky(argv)
  j, k = argv[0].to_i, argv[1].to_i
  mode = /even/i=~argv[2] ? :'even lucky' : :lucky
  seq = generator(mode == :'even lucky')
  ord = ->(n){"#{n}#{(n%100).between?(11,19) ? 'th' : %w[th st nd rd th th th th th th][n%10]}"}
  if k.zero?
    puts "#{ord[j]} #{mode} number: #{seq.take(j).last}"
  elsif 0 < k
    puts "#{ord[j]} through #{ord[k]} (inclusive) #{mode} numbers",
         "  #{seq.take(k)[j-1..-1]}"
  else
    k = -k
    ary = []
    loop do
      case num=seq.next
      when 1...j
      when j..k  then ary << num
      else break
      end
    end
    puts "all #{mode} numbers in the range #{j}..#{k}",
         "  #{ary}"
  end
end

if __FILE__ == $0
  lucky(ARGV)
end
```


```txt

C:\>ruby lucky.rb 1 20
1st through 20th (inclusive) lucky numbers
  [1, 3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 49, 51, 63, 67, 69, 73, 75, 79]

C:\>ruby lucky.rb 1 20 evenLucky
1st through 20th (inclusive) even lucky numbers
  [2, 4, 6, 10, 12, 18, 20, 22, 26, 34, 36, 42, 44, 50, 52, 54, 58, 68, 70, 76]

C:\>ruby lucky.rb 6000 -6100 Lucky
all lucky numbers in the range 6000..6100
  [6009, 6019, 6031, 6049, 6055, 6061, 6079, 6093]

C:\>ruby lucky.rb 6000 -6100 evenLucky
all even lucky numbers in the range 6000..6100
  [6018, 6020, 6022, 6026, 6036, 6038, 6050, 6058, 6074, 6090, 6092]

C:\>ruby lucky.rb 10000
10000th lucky number: 115591

C:\>ruby lucky.rb 10000 , EVENLUCKY
10000th even lucky number: 111842

```



## Swift

The lucky numbers sequence:


```swift
struct LuckyNumbers : Sequence, IteratorProtocol {
  let even: Bool
  let through: Int

  private var drainI = 0
  private var n = 0
  private var lst: [Int]

  init(even: Bool = false, through: Int = 1_000_000) {
    self.even = even
    self.through = through
    self.lst = Array(stride(from: even ? 2 : 1, through: through, by: 2))
  }

  mutating func next() -> Int? {
    guard n != 0 else {
      defer { n += 1 }

      return lst[0]
    }

    while n < lst.count && lst[n] < lst.count {
      let retVal = lst[n]

      lst = lst.enumerated().filter({ ($0.offset + 1) % lst[n] != 0  }).map({ $0.element })
      n += 1

      return retVal
    }

    if drainI == 0 {
      lst = Array(lst.dropFirst(n))
    }

    while drainI < lst.count {
      defer { drainI += 1 }

      return lst[drainI]
    }

    return nil
  }
}
```


The main file:

```swift
let args = Array(CommandLine.arguments.dropFirst())

guard let sj = args.first, let j = Int(sj), j > 0, j <= 10_000 else {
  fatalError("Incorrect j")
}

func evenString(_ even: Bool) -> String {
  return even ? "even" : ""
}

func jLuckyNumber(_ j: Int, even: Bool) {
  print("The \(j)th \(evenString(even)) lucky number is \(Array(LuckyNumbers(even: even))[j-1..<j].first!)")
}

func luckyNumbersKth(j: Int, k: Int, even: Bool) {
  print("List of \(j) ... \(k) \(evenString(even)) lucky numbers: ", terminator: "")

  for (offset, luck) in LuckyNumbers(even: even).lazy.enumerated() {
    guard offset + 1 <= k else { break }

    if offset + 1 >= j {
      print(luck, terminator: ", ")
    }
  }

  print()
}

func luckyNumbersRange(j: Int, k: Int, even: Bool) {
  print("List of \(evenString(even)) lucky numbers in the range \(j) ... \(-k): ", terminator: "")

  for lucky in LuckyNumbers(even: even).lazy {
    guard lucky <= -k else { break }

    if lucky >= j {
      print(lucky, terminator: ", ")
    }
  }

  print()
}

switch args.count {
case 1:
  jLuckyNumber(j, even: false)
case 2:
  switch Int(args.last!) {
  case let k? where k > 0 && k <= 10_000 && k > j:
    luckyNumbersKth(j: j, k: k, even: false)
  case let k? where k < 0 && -k > j:
    luckyNumbersRange(j: j, k: k, even: false)
  case _:
    fatalError("Bad args")
  }
case 3:
  switch (Int(args[1]), args.last!) {
  case (nil, "lucky"):
    jLuckyNumber(j, even: false)
  case (nil, "evenLucky"):
    jLuckyNumber(j, even: true)
  case let (k?, "lucky") where k > 0 && k <= 10_000 && k > j:
    luckyNumbersKth(j: j, k: k, even: false)
  case let (k?, "evenLucky") where k > 0 && k <= 10_000 && k > j:
    luckyNumbersKth(j: j, k: k, even: true)
  case let (k?, "lucky") where k < 0 && -k > j:
    luckyNumbersRange(j: j, k: k, even: false)
  case let (k?, "evenLucky") where k < 0 && -k > j:
    luckyNumbersRange(j: j, k: k, even: true)
  case _:
    fatalError("Bad args")
  }
case _:
  fatalError()
}
```


```txt
$ ./main 1 20 lucky
List of 1 ... 20  lucky numbers: 1, 3, 7, 9, 13, 15, 21, 25, 31, 33, 37, 43, 49, 51, 63, 67, 69, 73, 75, 79,
$ ./main 1 20 evenLucky
List of 1 ... 20 even lucky numbers: 2, 4, 6, 10, 12, 18, 20, 22, 26, 34, 36, 42, 44, 50, 52, 54, 58, 68, 70, 76,
$ ./main 6000 -6100 lucky
List of  lucky numbers in the range 6000 ... 6100: 6009, 6019, 6031, 6049, 6055, 6061, 6079, 6093,
$ ./main 6000 -6100 evenLucky
List of even lucky numbers in the range 6000 ... 6100: 6018, 6020, 6022, 6026, 6036, 6038, 6050, 6058, 6074, 6090, 6092,
$ ./main 10000
The 10000th  lucky number is 115591
$ ./main 10000 , evenLucky
The 10000th even lucky number is 111842
```



## Tcl

```tcl
#!/usr/bin/env tclsh8.6
package require Tcl 8.6

proc lgen {{even false} {nmax 200000}} {
    coroutine lgen.[incr ::lgen] apply {{start nmax} {
	set n 1
	for {set i $start} {$i <= $nmax+1} {incr i 2} {lappend lst $i}
	yield [info coroutine]
	yield [lindex $lst 0]
	while {$n < [llength $lst] && [lindex $lst $n] < [llength $lst]} {
	    yield [lindex $lst $n]
	    set lst [set i 0;lmap j $lst {
		if {[incr i] % [lindex $lst $n]} {set j} else continue
	    }]
	    incr n
	}
	foreach i [lrange $lst $n end] {
	    yield $i
	}
    }} [expr {$even ? 2 : 1}] $nmax
}

proc collectIndices {generator from to} {
    set result {}
    for {set i 0} {$i <= $to} {incr i} {
	set n [$generator]
	if {$i >= $from} {lappend result $n}
    }
    rename $generator {}
    return $result
}
proc collectValues {generator from to} {
    set result {}
    while 1 {
	set n [$generator]
	if {$n > $to} break
	if {$n >= $from} {lappend result $n}
    }
    rename $generator {}
    return $result
}

if {$argc<1||$argc>3} {
    puts stderr "wrong # args: should be \"$argv0 from ?to? ?evenOdd?\""
    exit 1
}
lassign $argv from to evenOdd
if {$argc < 3} {set evenOdd lucky}
if {$argc < 2} {set to ,}
if {![string is integer -strict $from] || $from < 1} {
    puts stderr "\"from\" must be positive integer"
    exit 1
} elseif {$to ne "," && (![string is integer -strict $to] || $to == 0)} {
    puts stderr "\"to\" must be positive integer or comma"
    exit 1
} elseif {[set evenOdd [string tolower $evenOdd]] ni {lucky evenlucky}} {
    puts stderr "\"evenOdd\" must be \"lucky\" or \"evenLucky\""
    exit 1
}
set l [lgen [expr {$evenOdd eq "evenlucky"}]]
set evenOdd [lindex {"lucky" "even lucky"} [expr {$evenOdd eq "evenlucky"}]]
if {$to eq ","} {
    puts "$from'th $evenOdd number = [collectIndices $l [incr from -1] $from]"
} elseif {$to < 0} {
    set to [expr {-$to}]
    puts "all $evenOdd numbers from $from to $to: [join [collectValues $l $from $to] ,]"
} else {
    puts "$from'th to $to'th $evenOdd numbers: [join [collectIndices $l [incr from -1] [incr to -1]] ,]"
}
```

```txt

bash$ lucky.tcl 1 20
1'th to 20'th lucky numbers: 1,3,7,9,13,15,21,25,31,33,37,43,49,51,63,67,69,73,75,79
bash$ lucky.tcl 1 20 evenLucky
1'th to 20'th even lucky numbers: 2,4,6,10,12,18,20,22,26,34,36,42,44,50,52,54,58,68,70,76
bash$ lucky.tcl 6000 -6100
all lucky numbers from 6000 to 6100: 6009,6019,6031,6049,6055,6061,6079,6093
bash$ lucky.tcl 6000 -6100 evenLucky
all even lucky numbers from 6000 to 6100: 6018,6020,6022,6026,6036,6038,6050,6058,6074,6090,6092
bash$ lucky.tcl 10000
10000'th lucky number = 115591
bash$ lucky.tcl 10000 , evenLucky
10000'th even lucky number = 111842

```



## zkl

The lucky number generator works by chaining filters to a even or odd infinite sequence. So it acts like a sieve as each starting number percolates through the filters. It also means there are lots and lots of filters, which doesn't scale well but works for the examples.

```zkl
fcn lgen(a){
   ns,idx:=[a..*,2],2;
   vm.yield(ns.next());
   while(1){
      n:=ns.next();
      vm.yield(n);
      ns=ns.tweak(skipper.fp1(n,Ref(idx+=1)));  // tack on another filter
   }
}
fcn skipper(n,skp,cnt){ z:=cnt.inc(); if(z%skp==0) Void.Skip else n  }
```

The command line is a bit funky (by Unix standards) so we just hard code it and use exceptions (such as trying to convert "foo" to int) to show the options.

```zkl
cmdLineArgs,j,k,start:=vm.arglist,Void,Void,1;
try{
   j=cmdLineArgs[0].toInt();
   na:=cmdLineArgs.len();
   if(na>1){
      if(cmdLineArgs[1]==",")
         start=cmdLineArgs[2][0].toLower()=="e" and 2 or 1;
      else{
	 k=cmdLineArgs[1].toInt();
	 if(na>2)
	    start=cmdLineArgs[2][0].toLower()=="e" and 2 or 1;
      }
   }
}catch{
   fcn options{
      "args: j | j , [even]lucky | j k [even]lucky | j -k [even]lucky"
      .println();
      System.exit(1);
   }()
}
luckies:=Utils.Generator(lgen,start);
try{
   if(Void==k) luckies.drop(j-1).next().println();
   else{
      if(k>0) luckies.drop(j-1).walk(k-j+1).println();
      else{ k=-k;
	 while((n:=luckies.next())<j){}
	 luckies.push(n);
	 luckies.pump(List,'wrap(n){ n<=k and n or Void.Stop }).println();
      }
   }
}catch(TheEnd){ options() }
```

```txt

$ zkl lucky
args: j | j , [even]lucky | j k [even]lucky | j -k [even]lucky
$ zkl lucky 1 20
L(1,3,7,9,13,15,21,25,31,33,37,43,49,51,63,67,69,73,75,79)
$ zkl lucky 1 20 evenLucky
L(2,4,6,10,12,18,20,22,26,34,36,42,44,50,52,54,58,68,70,76)
$ zkl lucky 6000 -6100
L(6009,6019,6031,6049,6055,6061,6079,6093)
$ zkl lucky 6000 -6100 Even
L(6018,6020,6022,6026,6036,6038,6050,6058,6074,6090,6092)
$ zkl lucky 10000
115591
$ zkl lucky 10000 , evenLucky
111842
$ zkl lucky 6000 -5000
L()
$ zkl lucky 4 2
args: j | j , [even]lucky | j k [even]lucky | j -k [even]lucky

```

