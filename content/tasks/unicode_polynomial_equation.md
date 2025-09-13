+++
title = "Unicode polynomial equation"
description = ""
date = 2019-03-30T20:18:01Z
aliases = []
[extra]
id = 10876
[taxonomies]
categories = ["task"]
tags = []
+++

{{difficulty}}{{draft task}}<!--{{Wikipedia|Polynomial}}-->
The objective of this task is to parse in a difficult [[wp:polynomial|polynomial]], and generate a "pretty" representation of the polynomial in Unicode.

In the target language define a "polynomial" object (or structure or record).  Using this object also define the routines for parsing a polynomial as input, and generating a normalised [[wp:Unicode|Unicode]] representation of the polynomial as output.

'''Task details:'''

Given a string containing an untidy Unicode polynomial, e.g.

```txt

-0.00x⁺¹⁰ + 1.0·x ** 5 + -2e0x^4 + +0,042.00 × x ⁺³ + +.0x² + 20.000 000 000x¹ - -1x⁺⁰ + .0x⁻¹ + 20.x¹

```

Coerce (or convert) the string into the "polynomial" object, at the same time normalise the polynomial to a canonical form.
The ideal normalised output (in this example) would be:

```txt

x⁵ - 2x⁴ + 42x³ + 40x + 1

```


{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Specific examples of Unicode and polynomial texts to be parsed as test cases.
|-
!Description|| Input example test cases
|-
|"Zero" coefficients are removed||x⁵ - 2x⁴ + 42x³ + 0x² + 40x + 1
|-
|The "0" polynomial case||0e+0x⁰⁰⁷ + 00e-00x + 0x + .0x⁰⁵ - 0.x⁴ + 0×x³ + 0x⁻⁰ + 0/x + 0/x³ + 0x⁻⁵
|-
|"One" coefficients are normalised||1x⁵ - 2x⁴ + 42x³ + 40x + 1x⁰
|-
|Signs are normalised||+x⁺⁵ + -2x⁻⁻⁴ + 42x⁺⁺³ + +40x - -1
|-
|ASCII representations are parsed||x^5 - 2x**4 + 42x^3 + 40x + 1
|-
|Non-ASCII representations are parsed||x↑5 - 2.00·x⁴ + 42.00·x³ + 40.00·x + 1 (c.f. [[wp:Knuth's up-arrow notation|↑]] & [[wp:·#In_mathematics_and_science|·]])
|-
|Specifically permit non-polynomials where terms have negative exponents||x⁻⁵ - 2⁄x⁴ + 42x⁻³ + 40/x + 1x⁻⁰ (n.b. [[wp:Unicode_numerals#Fractions|Unicode Fraction]])
|-
|Spaces in numbers and between operators are ignored||x⁵ - 2x⁴ + 42.000 000x³ + 40x + 1
|-
|Single commas are ignored in numbers||x⁵ - 2x⁴ + 0,042x³ + 40.000,000x + 1
|-
|A coefficient may be duplicated, zero, or missing||0x⁷ + 10x + 10x + x⁵ - 2x⁴ + 42x³ + 20x + 1
|-
|Support [[wp:Scientific_notation#Examples_and_alternatives|Scientific notation]] and optionally<BR>support [http://unicode.org/charts/PDF/U2300.pdf Unicode Decimal Exponent Symbol] [http://mailcom.com/unicode/DecimalExponent.ttf U+23E8/⏨]
||1E0x⁵ - 2,000,000.e-6x⁴ + 4.2⏨1x³ + .40e+2x + 1
|-
|Unicode characters that must be specifically supported are: ||⁰ ¹ ² ³ ⁴ ⁵ ⁶ ⁷ ⁸ ⁹ ⁻ ⁺ · × ⁄ ↑ ⏨.
Where · & × are multiplication, and ⁄ is Unicode Fraction.
|- 
|Support fractions for both input and output. || x⁵ - x⁴⁄2 + 405x³⁄4 + 403x⁄4 + 5⁄2<BR>On output round the decimal to appropriate fraction.
|- 
|Optionally support [[wp:Number Forms|Unicode Vulgar fractions]] for both input and output.<BR>¼ ½ ¾ ⅐ ⅑ ⅒ ⅓ ⅔ ⅕ ⅖ ⅗ ⅘ ⅙ ⅚ ⅛ ⅜ ⅝ ⅞ ↉ || x⁵ - ½x⁴ + 101¼x³ + 100¾x + 2½<BR>On output round the decimal to appropriate fraction.
|}

There are (at least) three possible ways of achieving this task.
* Using an external parsing library.
* Using a built-in parsing/formatting library.
* Coding a custom polynomial parsing routing. 
Either one, or all of these approaches are accepted and appear as a subtitle.
## Go

Although this program provides full support for Unicode vulgar fractions, note that there is no guarantee that they (or arithmetic on them) will successfully 'round trip' due to floating point arithmetic being used in the underlying calculations and some of them being recurring rather than exact decimals in any case.

```go
package main

import (
    "fmt"
    "log"
    "math"
    "regexp"
    "strconv"
    "strings"
)

var powers = strings.NewReplacer(
    "0", "⁰",
    "1", "¹",
    "2", "²",
    "3", "³",
    "4", "⁴",
    "5", "⁵",
    "6", "⁶",
    "7", "⁷",
    "8", "⁸",
    "9", "⁹",
    "-", "⁻",
)

var fractions = [][2]string{
    {".25", "¼"},
    {".5", "½"},
    {".75", "¾"},
    {".14285714285714285", "⅐"},
    {".1111111111111111", "⅑"},
    {".1", "⅒"},
    {".3333333333333333", "⅓"},
    {".6666666666666666", "⅔"},
    {".2", "⅕"},
    {".4", "⅖"},
    {".6", "⅗"},
    {".8", "⅘"},
    {".16666666666666666", "⅙"},
    {".8333333333333334", "⅚"},
    {".125", "⅛"},
    {".375", "⅜"},
    {".625", "⅝"},
    {".875", "⅞"},
}

func printEquation(coefs map[int]float64) {
    fmt.Print("=> ")
    if len(coefs) == 0 {
        fmt.Println("0\n")
        return
    }
    max, min := math.MinInt32, math.MaxInt32
    for k := range coefs {
        if k > max {
            max = k
        }
        if k < min {
            min = k
        }
    }
    for p := max; p >= min; p-- {
        if c := coefs[p]; c != 0 {
            if p < max {
                sign := "+"
                if c < 0 {
                    sign = "-"
                    c = -c
                }
                fmt.Printf(" %s ", sign)
            }
            if c != 1 || (c == 1 && p == 0) {
                cs := fmt.Sprintf("%v", c)
                ix := strings.Index(cs, ".")
                if ix >= 0 {
                    dec := cs[ix:]
                    for _, frac := range fractions {
                        if dec == frac[0] {
                            cs = strings.Replace(cs, dec, frac[1], 1)
                            break
                        }
                    }
                }
                if cs[0] == '0' && len(cs) > 1 && cs[1] != '.' {
                    cs = cs[1:]
                }
                fmt.Print(cs)
            }
            if p != 0 {
                ps := strconv.Itoa(p)
                ps = powers.Replace(ps)
                if ps == "¹" {
                    ps = ""
                }
                fmt.Printf("x%s", ps)
            }
        }
    }
    fmt.Println("\n")
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    equs := []string{
        `-0.00x⁺¹⁰ + 1.0·x ** 5 + -2e0x^4 + +0,042.00 × x ⁺³ + +.0x² + 20.000 000 000x¹ - -1x⁺⁰ + .0x⁻¹ + 20.x¹`,
        `x⁵ - 2x⁴ + 42x³ + 0x² + 40x + 1`,
        `0e+0x⁰⁰⁷ + 00e-00x + 0x + .0x⁰⁵ - 0.x⁴ + 0×x³ + 0x⁻⁰ + 0/x + 0/x³ + 0x⁻⁵`,
        `1x⁵ - 2x⁴ + 42x³ + 40x + 1x⁰`,
        `+x⁺⁵ + -2x⁻⁻⁴ + 42x⁺⁺³ + +40x - -1`,
        `x^5 - 2x**4 + 42x^3 + 40x + 1`,
        `x↑5 - 2.00·x⁴ + 42.00·x³ + 40.00·x + 1`,
        `x⁻⁵ - 2⁄x⁴ + 42x⁻³ + 40/x + 1x⁻⁰`,
        `x⁵ - 2x⁴ + 42.000 000x³ + 40x + 1`,
        `x⁵ - 2x⁴ + 0,042x³ + 40.000,000x + 1`,
        `0x⁷ + 10x + 10x + x⁵ - 2x⁴ + 42x³ + 20x + 1`,
        `1E0x⁵ - 2,000,000.e-6x⁴ + 4.2⏨1x³ + .40e+2x + 1`,
        `x⁵ - x⁴⁄2 + 405x³⁄4 + 403x⁄4 + 5⁄2`,
        `x⁵ - 0.5x⁴ + 101.25x³ + 100.75x + 2.5`,
        `x⁻⁵ - 2⁄x⁴ + 42x⁻³ - 40/x`,
        `⅐x⁵ - ⅓x⁴ - ⅔x⁴ + 42⅕x³ + ⅑x - 40⅛ - ⅝`,
    }
    rgx := regexp.MustCompile(`\s+(\+|-)\s+`)
    rep := strings.NewReplacer(
        ",", "",
        " ", "",
        "¼", ".25",
        "½", ".5",
        "¾", ".75",
        "⅐", ".14285714285714285",
        "⅑", ".1111111111111111",
        "⅒", ".1",
        "⅓", ".3333333333333333",
        "⅔", ".6666666666666666",
        "⅕", ".2",
        "⅖", ".4",
        "⅗", ".6",
        "⅘", ".8",
        "⅙", ".16666666666666666",
        "⅚", ".8333333333333334",
        "⅛", ".125",
        "⅜", ".375",
        "⅝", ".625",
        "⅞", ".875",
        "↉", ".0",
        "⏨", "e",
        "⁄", "/",
    )
    rep2 := strings.NewReplacer(
        "⁰", "0",
        "¹", "1",
        "²", "2",
        "³", "3",
        "⁴", "4",
        "⁵", "5",
        "⁶", "6",
        "⁷", "7",
        "⁸", "8",
        "⁹", "9",
        "⁻⁻", "",
        "⁻", "-",
        "⁺", "",
        "**", "",
        "^", "",
        "↑", "",
        "⁄", "/",
    )
    var err error

    for _, equ := range equs {
        fmt.Println(equ)
        terms := rgx.Split(equ, -1)
        ops := rgx.FindAllString(equ, -1)
        for i := 0; i < len(ops); i++ {
            ops[i] = strings.TrimSpace(ops[i])
        }
        coefs := make(map[int]float64)
        for i, term := range terms {
            s := strings.Split(term, "x")
            t := s[0]
            t = strings.TrimRight(t, "·× ")
            t = rep.Replace(t)
            c := 1.0
            inverse := false
            if t != "" {
                if t == "+" || t == "-" {
                    t += "1"
                }
                ix := strings.Index(t, "/")
                if ix == len(t)-1 {
                    inverse = true
                    t = t[0 : len(t)-1]
                    c, err = strconv.ParseFloat(t, 64)
                    check(err)
                } else if ix >= 0 {
                    u := strings.Split(t, "/")
                    m, err := strconv.ParseFloat(u[0], 64)
                    check(err)
                    n, err := strconv.ParseFloat(u[1], 64)
                    check(err)
                    c = m / n
                } else {
                    c, err = strconv.ParseFloat(t, 64)
                    check(err)
                }
                if i > 0 && ops[i-1] == "-" {
                    c = -c
                }
                if c == -0.0 {
                    c = 0
                }
            }
            if len(s) == 1 {
                coefs[0] += c
                continue
            }
            u := s[1]
            u = strings.TrimSpace(u)
            if u == "" {
                p := 1
                if inverse {
                    p = -1
                }
                if c != 0 {
                    coefs[p] += c
                }
                continue
            }
            u = rep2.Replace(u)
            jx := strings.Index(u, "/")
            p := 1
            if jx >= 0 {
                v := strings.Split(u, "/")
                p, _ = strconv.Atoi(v[0])
                d, err := strconv.ParseFloat(v[1], 64)
                check(err)
                c /= d
            } else {
                p, _ = strconv.Atoi(strings.TrimSpace(u))
            }
            if inverse {
                p = -p
            }
            if c != 0 {
                coefs[p] += c
            }
        }
        printEquation(coefs)
    }
}
```


```txt

-0.00x⁺¹⁰ + 1.0·x ** 5 + -2e0x^4 + +0,042.00 × x ⁺³ + +.0x² + 20.000 000 000x¹ - -1x⁺⁰ + .0x⁻¹ + 20.x¹
=> x⁵ - 2x⁴ + 42x³ + 40x + 1

x⁵ - 2x⁴ + 42x³ + 0x² + 40x + 1
=> x⁵ - 2x⁴ + 42x³ + 40x + 1

0e+0x⁰⁰⁷ + 00e-00x + 0x + .0x⁰⁵ - 0.x⁴ + 0×x³ + 0x⁻⁰ + 0/x + 0/x³ + 0x⁻⁵
=> 0

1x⁵ - 2x⁴ + 42x³ + 40x + 1x⁰
=> x⁵ - 2x⁴ + 42x³ + 40x + 1

+x⁺⁵ + -2x⁻⁻⁴ + 42x⁺⁺³ + +40x - -1
=> x⁵ - 2x⁴ + 42x³ + 40x + 1

x^5 - 2x**4 + 42x^3 + 40x + 1
=> x⁵ - 2x⁴ + 42x³ + 40x + 1

x↑5 - 2.00·x⁴ + 42.00·x³ + 40.00·x + 1
=> x⁵ - 2x⁴ + 42x³ + 40x + 1

x⁻⁵ - 2⁄x⁴ + 42x⁻³ + 40/x + 1x⁻⁰
=> 1 + 40x⁻¹ + 42x⁻³ - 2x⁻⁴ + x⁻⁵

x⁵ - 2x⁴ + 42.000 000x³ + 40x + 1
=> x⁵ - 2x⁴ + 42x³ + 40x + 1

x⁵ - 2x⁴ + 0,042x³ + 40.000,000x + 1
=> x⁵ - 2x⁴ + 42x³ + 40x + 1

0x⁷ + 10x + 10x + x⁵ - 2x⁴ + 42x³ + 20x + 1
=> x⁵ - 2x⁴ + 42x³ + 40x + 1

1E0x⁵ - 2,000,000.e-6x⁴ + 4.2⏨1x³ + .40e+2x + 1
=> x⁵ - 2x⁴ + 42x³ + 40x + 1

x⁵ - x⁴⁄2 + 405x³⁄4 + 403x⁄4 + 5⁄2
=> x⁵ + ½x⁴ + 101¼x³ + 103¼

x⁵ - 0.5x⁴ + 101.25x³ + 100.75x + 2.5
=> x⁵ - ½x⁴ + 101¼x³ + 100¾x + 2½

x⁻⁵ - 2⁄x⁴ + 42x⁻³ - 40/x
=> -40x⁻¹ + 42x⁻³ - 2x⁻⁴ + x⁻⁵

⅐x⁵ - ⅓x⁴ - ⅔x⁴ + 42⅕x³ + ⅑x - 40⅛ - ⅝
=> ⅐x⁵ - x⁴ + 42⅕x³ + ⅑x - 40¾


```



## Phix

To simplify this task I first created a test file (save as utf8, Unicode_polynomial_equation.txt):

```txt

-0.00x⁺¹⁰ + 1.0·x ** 5 + -2e0x^4 + +0,042.00 × x ⁺³ + +.0x² + 20.000 000 000x¹ - -1x⁺⁰ + .0x⁻¹ + 20.x¹
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
===> x^5 - 2x^4 + 42x^3 + 40x + 1
x⁵ - 2x⁴ + 42x³ + 0x² + 40x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
0e+0x⁰⁰⁷ + 00e-00x + 0x + .0x⁰⁵ - 0.x⁴ + 0×x³ + 0x⁻⁰ + 0/x + 0/x³ + 0x⁻⁵
==> 0
1x⁵ - 2x⁴ + 42x³ + 40x + 1x⁰
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
+x⁺⁵ + -2x⁻⁻⁴ + 42x⁺⁺³ + +40x - -1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
x^5 - 2x**4 + 42x^3 + 40x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
x↑5 - 2.00·x⁴ + 42.00·x³ + 40.00·x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
x⁻⁵ - 2⁄x⁴ + 42x⁻³ + 40/x + 1x⁻⁰
==> 1 + 40x⁻¹ + 42x⁻³ - 2x⁻⁴ + x⁻⁵
===> 1 + 40x^-1 + 42x^-3 - 2x^-4 + x^-5
x⁵ - 2x⁴ + 42.000 000x³ + 40x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
x⁵ - 2x⁴ + 0,042x³ + 40.000,000x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
0x⁷ + 10x + 10x + x⁵ - 2x⁴ + 42x³ + 20x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
1E0x⁵ - 2,000,000.e-6x⁴ + 4.2⏨1x³ + .40e+2x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
x⁵ - x⁴⁄2 + 405x³⁄4 + 403x⁄4 + 5⁄2
==> x⁵ - ½x⁴ + 101¼x³ + 100¾x + 2½
===> x^5 - 0.5x^4 + 101.25x^3 + 100.75x + 2.5
x⁵ - ½x⁴ + 101¼x³ + 100¾x + 2½
==> x⁵ - ½x⁴ + 101¼x³ + 100¾x + 2½
x<sup>5</sup> - 2x<sup>4</sup> + 42x<sup>3</sup> + 40x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
x^5 - 2x^4 + 42x^3 + 40x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
x⁵ - 2x⁴ + 42x³ + 40x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1
x5 - 2x4 + 42x3 + 40x + 1
==> x⁵ - 2x⁴ + 42x³ + 40x + 1

```

Explanation: 

On finding a line beginning with "==> ", parse the previous line and check that the output matches the rest of the line.

Lines beginning "===> " contain alternative (valid) representations of the previous line, for use when uni_frac is false. Note these equivalences can then be used further on, eg the result on line 5 maps to line 3 via 2, not that it matters should the same equivalent be defined twice, and also note that "==> " should always be the unicode version and "===> " the ascii one.

Obviously this is not well tested and //will// fail on the next thing thrown at it, but
it shows the basic approach. Error handling omitted using that good ol' standby excuse
"for clarity".

Note that space is not skipped when getting exponents, otherwise it might wrongly treat
say "x + 2" as "x^2", since we /are/ allowing exponentiation symbols to be omitted. At
the start of parse() we strip spaces around '^' (etc) to help with that.

Processing the input in utf32 form is a complete no-brainer; whereas constructing utf8 
output is a little more subtle, but avoids creating a utf32 "string" just because all
bytes happen to be less than 255, which would not necessarily be valid utf8/32.


```Phix
-- demo\rosetta\Unicode_polynomial_equation.exw
constant uni_frac = false   -- if true output unicode superscripts and vulgar fractions

constant UTF8BOM = {#EF,#BB,#BF},
         SPTWO = #00B2,         -- superscript two
         STHRE = #00B3,         -- superscript three
         MIDOT = #00B7,         -- middle dot
         SPONE = #00B9,         -- superscript one
         QUART = #00BC,         -- one quarter
         AHALF = #00BD,         -- one half
         THQTR = #00BE,         -- three quarters
         MULTY = #00D7,         -- multiplication sign
         DIVDE = #2044,         -- division sign
         SZERO = #2070,         -- superscript zero
         SFOUR = #2074,         -- superscript four
         SFIVE = #2075,         -- superscript five
         SPSIX = #2076,         -- superscript six
         SSEVN = #2077,         -- superscript seven
         SEGHT = #2078,         -- superscript eight
         SNINE = #2079,         -- superscript nine
         SPLUS = #207A,         -- superscript plus
         SMNUS = #207B,         -- superscript minus
         SVNTH = #2150,         -- one seventh
         NINTH = #2151,         -- one ninth
         TENTH = #2152,         -- one tenth
         THIRD = #2153,         -- one third
         TWTHD = #2154,         -- two thirds
         FIFTH = #2155,         -- one fifth
         TWFTH = #2156,         -- two fifths
         THFTH = #2157,         -- three fifths
         FRFTH = #2158,         -- four fifths
         SIXTH = #2159,         -- one sixth
         FVSIX = #215A,         -- five sixths
         EIGTH = #215B,         -- one eigth
         THEGH = #215C,         -- three eigths
         FVEGH = #215D,         -- five eigths
--       ZTHRD = #2189,         -- zero thirds[??]
         UPARW = #2191,         -- uparrow
         BASET = #23E8,         -- base 10
         SPACE = ' ',           -- space
             T = 10,            -- align nxt tbl
$

constant {vulgar_fractions,unicode_vulgar_fractions} = columnize({{{1,4},QUART},
                                                                  {{1,2},AHALF},
                                                                  {{3,4},THQTR},
                                                                  {{1,7},SVNTH},
                                                                  {{1,9},NINTH},
                                                                  {{1,T},TENTH},
                                                                  {{1,3},THIRD},
                                                                  {{2,3},TWTHD},
                                                                  {{1,5},FIFTH},
                                                                  {{2,5},TWFTH},
                                                                  {{3,5},THFTH},
                                                                  {{4,5},FRFTH},
                                                                  {{1,6},SIXTH},
                                                                  {{5,6},FVSIX},
                                                                  {{1,8},EIGTH},
                                                                  {{3,8},THEGH},
                                                                  {{5,8},FVEGH}})

constant EXPONENTS = {SZERO,SPONE,SPTWO,STHRE,SFOUR,SFIVE,SPSIX,SSEVN,SEGHT,SNINE}

function skip(sequence s, integer sdx, sequence set)
    while sdx<=length(s) 
      and find(s[sdx],set) do
        sdx += 1
    end while
    return sdx
end function

function get_sign(sequence s, integer sdx, bool allow_superscripts)
integer sgn = +1, ch
    for sdx=sdx to length(s) do
        ch = s[sdx]
        if allow_superscripts then
            ch = iff(ch=SPLUS?'+':
                 iff(ch=SMNUS?'-':
                 iff(ch=SPACE?'?':ch))) -- (do not skip spaces, see note)
        end if
        if ch!='+'
        and ch!=' ' then
            if ch!='-' then exit end if
            sgn *= -1
        end if
    end for
    return {sgn,sdx}
end function

function get_num(sequence s, integer sdx, atom n=0, bool allow_superscripts=false, as_fraction=false)
integer sgn = +1, ch, f, e10, d = 1
atom p10 = 10
bool none = not as_fraction -- (cope with "x" == "1x^1" and != "0x^0")
                            -- (but not when processing the "34" of "12.34", obvs)
    if not as_fraction then
        {sgn,sdx} = get_sign(s,sdx,allow_superscripts)
    end if
    if not allow_superscripts then
        sdx = skip(s,sdx,{' '}) 
    end if
    for sdx=sdx to length(s) do
        ch = s[sdx]
        if ch>='0' and ch<='9' then
            if as_fraction then
                n += (ch-'0')/p10
                p10 *= 10
            else
                n = n*10+ch-'0'
            end if
            none = false
        elsif allow_superscripts then
            f = find(ch,EXPONENTS)
            if f=0 then exit end if
            n = n*10+f-1
            none = false
        elsif not find(ch," ,") then
            exit
        end if
    end for
    if not allow_superscripts then
        if find(ch,{'e','E',BASET}) then
            {e10,f,sdx} = get_num(s,sdx+1)
            if f!=1 then ?9/0 end if
            n *= power(10,e10)
        elsif ch='.' then
            if as_fraction then ?9/0 end if
            {n,f,sdx} = get_num(s,sdx+1,n,as_fraction:=true)
            if f!=1 then ?9/0 end if
            none = false
        else
            f = find(ch,unicode_vulgar_fractions)
            if f!=0 then
                if as_fraction then ?9/0 end if
                integer {vfn,vfd} = vulgar_fractions[f]
                if uni_frac then
                    n = n*vfd + vfn
                    if d!=1 then ?9/0 end if
                    d = vfd
                else
                    n += vfn/vfd
                end if
                sdx += 1
                none = false
            end if
        end if
    end if
    if none then n = 1 end if
    n *= sgn
    return {n,d,sdx}
end function

function get_term(sequence s, integer sdx)
integer last_sdx = sdx, -- (infinite loop check/prevent)
        e = 0
atom c, d = 1, f
bool sdiv = false
    {c,d,sdx} = get_num(s,sdx)
    sdx = skip(s,sdx,{' ',MIDOT,MULTY})
    if sdx<=length(s) 
    and (s[sdx]='/' or
         s[sdx]=DIVDE) then
        sdx += 1
        if sdx<=length(s) and s[sdx]!='x' then
            {d,f,sdx} = get_num(s,sdx)
            if f!=1 then ?9/0 end if
            if not uni_frac then
                c /= d
                d = 1
            end if
        else
            sdiv = true
        end if
    end if
    if sdx<=length(s) and s[sdx]='x' then
        sdx = skip(s,sdx+1,{'^','*',SPLUS,UPARW})
        {e,f,sdx} = get_num(s,sdx,allow_superscripts:=true)
        if f!=1 then ?9/0 end if
        if sdiv then e = -e end if
    else
        if sdiv then ?9/0 end if
    end if
    if sdx<=length(s) 
    and (s[sdx]='/' or
         s[sdx]=DIVDE) then
        if d!=1 then ?9/0 end if
        {d,f,sdx} = get_num(s,sdx+1)
        if f!=1 then ?9/0 end if
        if not uni_frac then
            c /= d
            d = 1
        end if
    end if
    if sdx=last_sdx then ?9/0 end if
    return {e,c,d,sdx}
end function

function unicode_superscripts(integer e)
-- display helper
string res = ""
    if e>9 then
        res = unicode_superscripts(floor(e/10))
        e = remainder(e,10)
    end if
    res &= utf32_to_utf8({EXPONENTS[e+1]})
    return res
end function

enum EXP, COEF, FRAC    -- contents of terms[i]

function poly(sequence terms)
-- display helper
string r = ""
    for t=length(terms) to 1 by -1 do
        {integer e, atom c, integer f} = terms[t]
        if c!=0 then
            if c=1 and f=1 and e!=0 then
                r &= iff(r=""? "":" + ")
            elsif c=-1 and f=1 and e!=0 then
                r &= iff(r=""?"-":" - ")
            else
                if r!="" then
                    r &= iff(c<0?" - ":" + ")
                    c = abs(c)
                end if
                if f!=1 then    -- (hence/only when uni_frac==true)
                    integer k = find({remainder(c,f),f},vulgar_fractions)
                    if k then
                        c = floor(c/f)
                        if c!=0 then r &= sprintf("%d",c) end if
                        r &= utf32_to_utf8({unicode_vulgar_fractions[k]})
                    else
                        r &= sprintf("%g",c/f)
                    end if
                else
                    r &= sprintf("%g",c)
                end if
            end if
            if e!=0 then
                r &= 'x'
                if e!=1 then
                    if uni_frac then
                        if e<0 then
                            r &= utf32_to_utf8({SMNUS})
                            e = -e
                        end if
                        r &= unicode_superscripts(e)
                    else
                        r &= sprintf("^%d",e)
                    end if
                end if
            end if
        end if
    end for
    if r="" then r="0" end if
    return r
end function

function parse(sequence s)
sequence terms = {}
integer sdx = 1, e, f
atom c
    s = match_replace("<sup>",s,"^")
    s = match_replace("</sup>",s,"")
    s = match_replace("**",s,"^")
    s = match_replace(" ^",s,"^")
    s = match_replace("^ ",s,"^")
    s = match_replace({' ',SPLUS},s,{SPLUS})
    s = match_replace({' ',SMNUS},s,{SMNUS})
    for i=1 to length(EXPONENTS) do
        e = EXPONENTS[i]
        s = match_replace({' ',e},s,{e})
    end for 
    while sdx<=length(s) do
        {e,c,f,sdx} = get_term(s,sdx)
        if c!=0 then -- (aside: +5 -5 may yet leave c==0)
            terms = append(terms,{e,c,f})
        end if
    end while
    terms = sort(terms)
    -- merge, eg "10x^2 + 10x^2" -> 20x^2
    for i=length(terms) to 2 by -1 do
        if terms[i][EXP] = terms[i-1][EXP] then
            if terms[i-1][FRAC]!=terms[i][FRAC] then
                ?9/0 -- placeholder for more code
            else
                terms[i-1][COEF] += terms[i][COEF]
            end if
            terms[i..i] = {} -- (delete 2nd)
        end if
    end for
    return poly(terms)
end function

sequence alts = {},     -- (unicode versions)
         altn = {},     -- (idx of ascii equivalents)
         lines = read_lines("Unicode_polynomial_equation.txt")

if lines[1][1..3] = UTF8BOM then
    -- remove/ignore any utf8 byte order mark
    lines[1] = lines[1][4..$]
end if

for i=2 to length(lines) do
    if length(lines[i])>5 
    and lines[i][1..5] = "===> " then
        alts = append(alts,lines[i-1][5..$])
        altn = append(altn,i)
    end if
end for

for i=2 to length(lines) do
    if length(lines[i])>4 
    and lines[i][1..4] = "==> " then
        sequence line = utf8_to_utf32(lines[i-1])
        sequence res = parse(line)
        sequence expected = lines[i][5..$]
        if res=expected then
            -- (res is the unicode version)
            if platform()!=WINDOWS or res="0" then
                printf(1,"%2d: %40s   ok\n",{i-1,res})
            else
                -- (unicode output on windows consoles is fiddly...)
                printf(1,"%2d: ok\n",i-1)
            end if
        else
            integer k = find(expected,alts)
            if k and res=lines[altn[k]][6..$] then
                -- (res is the ascii equivalent)
                printf(1,"%2d: %40s   ok\n",{i-1,res})
            else
                printf(1,"%d: error - %s\n",{i-1,res})
            end if
        end if
    end if
end for
```

uni_frac = false

```txt

 1:             x^5 - 2x^4 + 42x^3 + 40x + 1   ok
 4:             x^5 - 2x^4 + 42x^3 + 40x + 1   ok
 6:                                        0   ok
 8:             x^5 - 2x^4 + 42x^3 + 40x + 1   ok
10:             x^5 - 2x^4 + 42x^3 + 40x + 1   ok
12:             x^5 - 2x^4 + 42x^3 + 40x + 1   ok
14:             x^5 - 2x^4 + 42x^3 + 40x + 1   ok
16:       1 + 40x^-1 + 42x^-3 - 2x^-4 + x^-5   ok
19:             x^5 - 2x^4 + 42x^3 + 40x + 1   ok
21:             x^5 - 2x^4 + 42x^3 + 40x + 1   ok
23:             x^5 - 2x^4 + 42x^3 + 40x + 1   ok
25:             x^5 - 2x^4 + 42x^3 + 40x + 1   ok
27: x^5 - 0.5x^4 + 101.25x^3 + 100.75x + 2.5   ok
30: x^5 - 0.5x^4 + 101.25x^3 + 100.75x + 2.5   ok
32:             x^5 - 2x^4 + 42x^3 + 40x + 1   ok
34:             x^5 - 2x^4 + 42x^3 + 40x + 1   ok
36:             x^5 - 2x^4 + 42x^3 + 40x + 1   ok
38:             x^5 - 2x^4 + 42x^3 + 40x + 1   ok

```

uni_frac = true (linux only, unless you have managed to find and install a decent windows console unicode font, which I haven't)

```txt

 1:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
 4:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
 6:                                        0   ok
 8:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
10:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
12:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
14:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
16:           1 + 40x⁻¹ + 42x⁻³ - 2x⁻⁴ + x⁻⁵   ok
19:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
21:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
23:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
25:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
27:           x⁵ - ½x⁴ + 101¼x³ + 100¾x + 2½   ok
30:           x⁵ - ½x⁴ + 101¼x³ + 100¾x + 2½   ok
32:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
34:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
36:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok
38:                x⁵ - 2x⁴ + 42x³ + 40x + 1   ok

```


