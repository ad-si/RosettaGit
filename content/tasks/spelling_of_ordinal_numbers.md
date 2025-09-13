+++
title = "Spelling of ordinal numbers"
description = ""
date = 2019-07-30T21:36:13Z
aliases = []
[extra]
id = 21587
[taxonomies]
categories = ["task"]
tags = []
+++

'''Ordinal numbers'''   (as used in this Rosetta Code task),   are numbers that describe the   ''position''   of something in a list.

It is this context that ordinal numbers will be used, using an English-spelled name of an ordinal number.


The ordinal numbers are   (at least, one form of them):
   1st  2nd  3rd  4th  5th  6th  7th  ···  99th  100th  ···  1000000000th  ···  etc

sometimes expressed as:
   1<sup>st</sup>  2<sup>nd</sup>  3<sup>rd</sup>  4<sup>th</sup>  5<sup>th</sup>  6<sup>th</sup>  7<sup>th</sup>  ···  99<sup>th</sup>  100<sup>th</sup>  ···  1000000000<sup>th</sup>  ···


For this task, the following (English-spelled form) will be used:
   first second third fourth fifth sixth seventh ninety-nineth one hundredth one billionth


Furthermore, the American version of numbers will be used here   (as opposed to the British).

'''2,000,000,000'''   is two billion,   ''not''   two milliard.


## Task

Write a driver and a function (subroutine/routine ···) that returns the English-spelled ordinal version of a specified number   (a positive integer).

Optionally, try to support as many forms of an integer that can be expressed:   '''123'''   '''00123.0'''   '''1.23e2'''   all are forms of the same integer.

Show all output here.


;Test cases:
Use (at least) the test cases of:
   1  2  3  4  5  11  65  100  101  272  23456  8007006005004003


## Related tasks

*   [[Number names]]
*   [[N'th]]





## AutoHotkey

Based on [[Number_names#AutoHotkey|Number_names]]

```AutoHotkey
OrdinalNumber(n){
	OrdinalNumber := {"one":"first", "two":"second", "three":"third", "five":"fifth", "eight":"eighth", "nine":"ninth", "twelve": "twelfth"}
	RegExMatch(n, "\w+$", m)
	return (OrdinalNumber[m] ? RegExReplace(n, "\w+$", OrdinalNumber[m]) : n "th")
}

Spell(n) { ; recursive function to spell out the name of a max 36 digit integer, after leading 0s removed 
    Static p1=" thousand ",p2=" million ",p3=" billion ",p4=" trillion ",p5=" quadrillion ",p6=" quintillion " 
         , p7=" sextillion ",p8=" septillion ",p9=" octillion ",p10=" nonillion ",p11=" decillion " 
         , t2="twenty",t3="thirty",t4="forty",t5="fifty",t6="sixty",t7="seventy",t8="eighty",t9="ninety" 
         , o0="zero",o1="one",o2="two",o3="three",o4="four",o5="five",o6="six",o7="seven",o8="eight" 
         , o9="nine",o10="ten",o11="eleven",o12="twelve",o13="thirteen",o14="fourteen",o15="fifteen" 
         , o16="sixteen",o17="seventeen",o18="eighteen",o19="nineteen"
		 
    n :=RegExReplace(n,"^0+(\d)","$1") ; remove leading 0s from n 
    If  (11 < d := (StrLen(n)-1)//3)   ; #of digit groups of 3 
        Return "Number too big"
    If (d)                             ; more than 3 digits 1000+ 
        Return Spell(SubStr(n,1,-3*d)) p%d% ((s:=SubStr(n,1-3*d)) ? ", " Spell(s) : "") 
    i := SubStr(n,1,1) 
    If (n > 99)                        ; 3 digits 100..999
        Return o%i% " hundred" ((s:=SubStr(n,2)) ? " and " Spell(s) : "") 
    If (n > 19)                        ; n = 20..99 
        Return t%i% ((o:=SubStr(n,2)) ? "-" o%o% : "") 
    Return o%n%                        ; n = 0..19 
} 

PrettyNumber(n) { ; inserts thousands separators into a number string 
    Return RegExReplace( RegExReplace(n,"^0+(\d)","$1"), "\G\d+?(?=(\d{3})+(?:\D|$))", "$0,")
}
```

Example:
```AutoHotkey
for i, n in StrSplit("1 2 3 4 5 11 65 100 101 272 23456 8007006005004003", " ")
    res .= PrettyNumber(n) "`t" Spell(n) "`t" OrdinalNumber(Spell(n)) "`n"
MsgBox % res

```

Outputs:
```txt
1	first
2	second
3	third
4	fourth
5	fifth
11	eleventh
65	sixty-fifth
100	one hundredth
101	one hundred and first
272	two hundred and seventy-second
23,456	twenty-three thousand , four hundred and fifty-sixth
8,007,006,005,004,003	eight quadrillion , seven trillion , six billion , five million , four thousand , third
```



## Clojure


```clojure
(def test-cases [1 2 3 4 5 11 65 100 101 272 23456 8007006005004003])
(pprint
  (sort (zipmap test-cases (map #(clojure.pprint/cl-format nil "~:R" %) test-cases))))

```

```txt

([1 "first"]
 [2 "second"]
 [3 "third"]
 [4 "fourth"]
 [5 "fifth"]
 [11 "eleventh"]
 [65 "sixty-fifth"]
 [100 "one hundredth"]
 [101 "one hundred first"]
 [272 "two hundred seventy-second"]
 [23456 "twenty-three thousand, four hundred fifty-sixth"]
 [8007006005004003
  "eight quadrillion, seven trillion, six billion, five million, four thousand, third"])

```



## Factor

Factor's <code>math.text.english</code> vocabulary provides the <code>number>text</code> word for converting numbers to written English. It also provides the <code>ordinal-suffix</code> word for getting the suffix for a given number, such as <tt>th</tt> for <tt>12</tt>. The bulk of this code deals with converting the output of <code>number>text</code> to ordinal format. 

```factor
USING: assocs formatting grouping kernel literals locals math
math.parser math.text.english qw regexp sequences
splitting.extras ;
IN: rosetta-code.spelling-ordinal-numbers

<PRIVATE

! Factor supports the arbitrary use of commas in integer
! literals, as some number systems (e.g. Indian) don't solely
! break numbers up into triplets.

CONSTANT: test-cases qw{
    1 2 3 4 5 11 65 100 101 272 23456 8007006005004003 123
    00123.0 1.23e2 1,2,3 0b1111011 0o173 0x7B 2706/22
}

CONSTANT: replacements $[
    qw{
        one    first
        two    second
        three  third
        five   fifth
        eight  eighth
        nine   ninth
        twelve twelfth
    } 2 group
]

: regular-ordinal ( n -- str )
    [ number>text ] [ ordinal-suffix ] bi append ;

! Since Factor's number>text word contains commas and "and",
! strip them out with a regular expression.
  
: text>ordinal-text ( str -- str' ) R/ \sand|,/ "" re-replace ;

PRIVATE>
    
:: number>ordinal-text ( n! -- str )
    n >integer n!
    n number>text " ,-" split* dup last replacements at
    [ [ but-last ] dip suffix "" join ]
    [ drop n regular-ordinal          ] if* text>ordinal-text ;

<PRIVATE
    
: print-ordinal-pair ( str x -- )
    number>ordinal-text "%16s => %s\n" printf ;
    
PRIVATE>

: ordinal-text-demo ( -- )
    test-cases [ dup string>number print-ordinal-pair ] each
    "C{ 123 0 }" C{ 123 0 } print-ordinal-pair ;
    
MAIN: ordinal-text-demo
```

```txt

               1 => first
               2 => second
               3 => third
               4 => fourth
               5 => fifth
              11 => eleventh
              65 => sixty-fifth
             100 => one hundredth
             101 => one hundred first
             272 => two hundred seventy-second
           23456 => twenty-three thousand four hundred fifty-sixth
8007006005004003 => eight quadrillion seven trillion six billion five million four thousand third
             123 => one hundred twenty-third
         00123.0 => one hundred twenty-third
          1.23e2 => one hundred twenty-third
           1,2,3 => one hundred twenty-third
       0b1111011 => one hundred twenty-third
           0o173 => one hundred twenty-third
            0x7B => one hundred twenty-third
         2706/22 => one hundred twenty-third
      C{ 123 0 } => one hundred twenty-third

```



## Go

As with the Kotlin solution, this uses the output of <code>say</code> from the
[[Number_names#Go|Number_names]] task.

```Go
import (
	"fmt"
	"strings"
)

func main() {
	for _, n := range []int64{
		1, 2, 3, 4, 5, 11, 65, 100, 101, 272, 23456, 8007006005004003,
	} {
		fmt.Println(sayOrdinal(n))
	}
}

var irregularOrdinals = map[string]string{
	"one":    "first",
	"two":    "second",
	"three":  "third",
	"five":   "fifth",
	"eight":  "eighth",
	"nine":   "ninth",
	"twelve": "twelfth",
}

func sayOrdinal(n int64) string {
	s := say(n)
	i := strings.LastIndexAny(s, " -")
	i++
	// Now s[:i] is everything upto and including the space or hyphen
	// and s[i:] is the last word; we modify s[i:] as required.
	// Since LastIndex returns -1 if there was no space/hyphen,
	// `i` will be zero and this will still be fine.
	if x, ok := irregularOrdinals[s[i:]]; ok {
		s = s[:i] + x
	} else if s[len(s)-1] == 'y' {
		s = s[:i] + s[i:len(s)-1] + "ieth"
	} else {
		s = s[:i] + s[i:] + "th"
	}
	return s
}

// Below is a copy of https://rosettacode.org/wiki/Number_names#Go

var small = [...]string{"zero", "one", "two", "three", "four", "five", "six",
	"seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen",
	"fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"}
var tens = [...]string{"", "", "twenty", "thirty", "forty",
	"fifty", "sixty", "seventy", "eighty", "ninety"}
var illions = [...]string{"", " thousand", " million", " billion",
	" trillion", " quadrillion", " quintillion"}

func say(n int64) string {
	var t string
	if n < 0 {
		t = "negative "
		// Note, for math.MinInt64 this leaves n negative.
		n = -n
	}
	switch {
	case n < 20:
		t += small[n]
	case n < 100:
		t += tens[n/10]
		s := n % 10
		if s > 0 {
			t += "-" + small[s]
		}
	case n < 1000:
		t += small[n/100] + " hundred"
		s := n % 100
		if s > 0 {
			t += " " + say(s)
		}
	default:
		// work right-to-left
		sx := ""
		for i := 0; n > 0; i++ {
			p := n % 1000
			n /= 1000
			if p > 0 {
				ix := say(p) + illions[i]
				if sx != "" {
					ix += " " + sx
				}
				sx = ix
			}
		}
		t += sx
	}
	return t
}
```

```txt
first
second
third
fourth
fifth
eleventh
sixty-fifth
one hundredth
one hundred first
two hundred seventy-second
twenty-three thousand four hundred fifty-sixth
eight quadrillion seven trillion six billion five million four thousand third
```



## Julia

This makes use of code posted on this site by MichaeLeroy for a similar task at https://rosettacode.org/wiki/Number_names#Julia. The function num2text is used (but not included here) as posted from that location.

```Julia

const irregular = Dict("one" => "first", "two" => "second", "three" => "third", "five" => "fifth", 
                                "eight" => "eighth", "nine" => "ninth", "twelve" => "twelfth")
const suffix = "th"
const ysuffix = "ieth"

function numtext2ordinal(s)
    lastword = split(s)[end]
    redolast = split(lastword, "-")[end]
    if redolast != lastword
        lastsplit = "-"
        word = redolast
    else
        lastsplit = " "
        word = lastword
    end
    firstpart = reverse(split(reverse(s), lastsplit, limit=2)[end])
    firstpart = (firstpart == word) ? "": firstpart * lastsplit
    if haskey(irregular, word)
        word = irregular[word]
    elseif word[end] == 'y'
        word = word[1:end-1] * ysuffix
    else
        word = word * suffix
    end
    firstpart * word
end

const testcases =  [1  2  3  4  5  11  65  100  101  272  23456  8007006005004003]
for n in testcases
    println("$n => $(numtext2ordinal(num2text(n)))")
end

```

```txt

1 => first
2 => second
3 => third
4 => fourth
5 => fifth
11 => eleventh
65 => sixty-fifth
100 => one hundredth
101 => one hundred and first
272 => two hundred and seventy-second
23456 => twenty-three thousand four hundred and fifty-sixth
8007006005004003 => eight quadrillion seven trillion six billion five million four thousand and third

```



## Kotlin

This makes use of the code at https://rosettacode.org/wiki/Number_names#Kotlin, which I also wrote, and adjusts it to output the corresponding ordinal. Although, for good measure, the program can deal with negative integers, zero and UK-style numbers (the insertion of 'and' at strategic places, no 'milliards' I promise!) none of these are actually tested in line with the task's requirements.  

```scala
// version 1.1.4-3

typealias IAE = IllegalArgumentException

val names = mapOf(
    1 to "one",
    2 to "two",
    3 to "three",
    4 to "four",
    5 to "five",
    6 to "six",
    7 to "seven",
    8 to "eight",
    9 to "nine",
    10 to "ten",
    11 to "eleven",
    12 to "twelve",
    13 to "thirteen",
    14 to "fourteen",
    15 to "fifteen",
    16 to "sixteen",
    17 to "seventeen",
    18 to "eighteen",
    19 to "nineteen",
    20 to "twenty",
    30 to "thirty",
    40 to "forty",
    50 to "fifty",
    60 to "sixty",
    70 to "seventy",
    80 to "eighty",
    90 to "ninety"
)
val bigNames = mapOf(
    1_000L to "thousand",
    1_000_000L to "million",
    1_000_000_000L to "billion",
    1_000_000_000_000L to "trillion",
    1_000_000_000_000_000L to "quadrillion",
    1_000_000_000_000_000_000L to "quintillion"
)

val irregOrdinals = mapOf(
    "one" to "first",
    "two" to "second",
    "three" to "third",
    "five" to "fifth",
    "eight" to "eighth",
    "nine" to "ninth",
    "twelve" to "twelfth"
)

fun String.toOrdinal(): String {
    val splits = this.split(' ', '-')
    var last = splits[splits.lastIndex]
    return if (irregOrdinals.containsKey(last)) this.dropLast(last.length) + irregOrdinals[last]!!
           else if (last.endsWith("y")) this.dropLast(1) + "ieth"
           else this + "th"
} 
 
fun numToOrdinalText(n: Long, uk: Boolean = false): String {
    if (n == 0L) return "zeroth"  // or alternatively 'zeroeth'
    val neg = n < 0L
    val maxNeg = n == Long.MIN_VALUE
    var nn = if (maxNeg) -(n + 1) else if (neg) -n else n
    val digits3 = IntArray(7)
    for (i in 0..6) {  // split number into groups of 3 digits from the right
        digits3[i] = (nn % 1000).toInt()
        nn /= 1000
    }
 
    fun threeDigitsToText(number: Int) : String {
        val sb = StringBuilder()
        if (number == 0) return ""
        val hundreds = number / 100
        val remainder = number % 100
        if (hundreds > 0) {
            sb.append(names[hundreds], " hundred")
            if (remainder > 0) sb.append(if (uk) " and " else " ")
        }
        if (remainder > 0) {
            val tens = remainder / 10
            val units = remainder % 10
            if (tens > 1) {
                sb.append(names[tens * 10])
                if (units > 0) sb.append("-", names[units])
            }
            else sb.append(names[remainder])
        }
        return sb.toString()
    }
 
    val strings = Array<String>(7) { threeDigitsToText(digits3[it]) }
    var text = strings[0]
    var andNeeded = uk && digits3[0] in 1..99
    var big = 1000L
    for (i in 1..6) {
        if (digits3[i] > 0) {
            var text2 = strings[i] + " " + bigNames[big]
            if (text.length > 0) {
                text2 += if (andNeeded) " and " else ", "
                andNeeded = false
            }
            else andNeeded = uk && digits3[i] in 1..99
            text = text2 + text
        }
        big *= 1000
    }
    if (maxNeg) text = text.dropLast(5) + "eight"
    if (neg) text = "minus " + text
    return text.toOrdinal()
}

fun numToOrdinalText(s: String, uk: Boolean = false): String {
    val d = s.toDoubleOrNull() ?: throw IAE("String is not numeric") 
    if (d !in Long.MIN_VALUE.toDouble() .. Long.MAX_VALUE.toDouble())
        throw IAE("Double is outside the range of a Long Integer")
    val n = d.toLong()
    if (n.toDouble() != d) throw IAE("String does not represent a Long Integer")
    return numToOrdinalText(n, uk)
}
 
fun main(args: Array<String>) {
    val la = longArrayOf(1, 2, 3, 4, 5, 11, 65, 100, 101, 272, 23456, 8007006005004003)
    println("Using US representation:")
    for (i in la) println("${"%16d".format(i)} = ${numToOrdinalText(i)}")
    val sa = arrayOf("123", "00123.0", "1.23e2")
    for (s in sa) println("${"%16s".format(s)} = ${numToOrdinalText(s)}")
}
```


```txt

Using US representation:
               1 = first
               2 = second
               3 = third
               4 = fourth
               5 = fifth
              11 = eleventh
              65 = sixty-fifth
             100 = one hundredth
             101 = one hundred first
             272 = two hundred seventy-second
           23456 = twenty-three thousand, four hundred fifty-sixth
8007006005004003 = eight quadrillion, seven trillion, six billion, five million, four thousand, third
             123 = one hundred twenty-third
         00123.0 = one hundred twenty-third
          1.23e2 = one hundred twenty-third

```




## Perl

Adding zero to the input forces a numeric conversion (any identity operation would suffice).

```perl
use Lingua::EN::Numbers 'num2en_ordinal';

printf "%16s : %s\n", $_, num2en_ordinal(0+$_) for
    <1 2 3 4 5 11 65 100 101 272 23456 8007006005004003 123 00123.0 '00123.0' 1.23e2 '1.23e2'>;
```

```txt
               1 : first
               2 : second
               3 : third
               4 : fourth
               5 : fifth
              11 : eleventh
              65 : sixty-fifth
             100 : one hundredth
             101 : one hundred and first
             272 : two hundred and seventy-second
           23456 : twenty-three thousand four hundred and fifty-sixth
8007006005004003 : eight quadrillion, seven trillion, six billion, five million, four thousand and third
             123 : one hundred and twenty-third
         00123.0 : one hundred and twenty-third
         00123.0 : one hundred and twenty-third
          1.23e2 : one hundred and twenty-third
          1.23e2 : one hundred and twenty-third
```



## Perl 6

Rakudo version 2019.07.1 is updated to Unicode version 12.1. Unicode version 12.0 introduced some new numeric digits, which changed the output here a bit. This will ''work'' with earlier versions of Rakudo, but will yield slightly different results.

This would be pretty simple to implement from scratch; it would be straightforward to do a minor modification of the [[Number_names#Perl_6| Number names]] task code. Much simpler to just use the Lingua::EN::Numbers module from the Perl 6 ecosystem though. It will easily handles ordinal number conversions.

We need to be slightly careful of terminology. In Perl 6, 123, 00123.0, & 1.23e2 are not all integers. They are respectively an Int (integer), a Rat (rational number) and a Num (floating point number). (The fourth numeric is a Complex) For this task it doesn't much matter as the ordinal routine coerces its argument to an Int, but to Perl 6 they are different things. We can further abuse allomorphic and String types and role mixins for some somewhat non-intuitive results as well.

Note that the different allomorphic integer forms of 123 are evaluated on ''use'', not on ''assignment''. They can be passed around in parameters, but until they are used numerically, they retain their stringy characteristics and are distinctive, determinable through introspection. The numerics are evaluated on assignment, hence the stringified output not exactly matching the input format. The mixin role returns different things depending on what context you evaluate it under. When evaluated as a string it is 17, numerically, it is 123.

Perl 6 uses Unicode natively. If a glyph has a Unicode 'Numeric Digit' (<:Nd>) property, it is treated as a numeric digit, and may be used as one.

It is not really clear what is meant by "Write a driver and a function...". Well, the function part is clear enough; driver not so much. Perhaps this will suffice.


```perl6
use Lingua::EN::Numbers;

# The task
+$_ ?? printf( "Type: \%-14s %16s : %s\n", .^name, $_, .&ordinal ) !! say "\n$_:" for

# Testing
'Required tests',
1, 2, 3, 4, 5, 11, 65, 100, 101, 272, 23456, 8007006005004003,

'Optional tests - different forms of 123',
'Numerics',
123, 00123.0, 1.23e2, 123+0i,

'Allomorphs',
|<123 1_2_3 00123.0 1.23e2 123+0i 0b1111011 0o173 0x7B 861/7>,

'Numeric Strings',
|'1_2_3 00123.0 1.23e2 123+0i 0b1111011 0o173 0x7B 861/7'.words,

'Unicode Numeric Strings',
# (Only using groups of digits from the same Unicode block. Technically,
# digits from any block could be combined with digits from any other block.)
|(^0x1FFFF).grep( { .chr ~~ /<:Nd>/ and .unival == 1|2|3 }).rotor(3)».chr».join,

'Role Mixin',
'17' but 123;
```

```txt
Required tests:
Type: Int                           1 : first
Type: Int                           2 : second
Type: Int                           3 : third
Type: Int                           4 : fourth
Type: Int                           5 : fifth
Type: Int                          11 : eleventh
Type: Int                          65 : sixty-fifth
Type: Int                         100 : one hundredth
Type: Int                         101 : one hundred first
Type: Int                         272 : two hundred seventy-second
Type: Int                       23456 : twenty-three thousand, four hundred fifty-sixth
Type: Int            8007006005004003 : eight quadrillion, seven trillion, six billion, five million, four thousand third

Optional tests - different forms of 123:

Numerics:
Type: Int                         123 : one hundred twenty-third
Type: Rat                         123 : one hundred twenty-third
Type: Num                         123 : one hundred twenty-third
Type: Complex                  123+0i : one hundred twenty-third

Allomorphs:
Type: IntStr                      123 : one hundred twenty-third
Type: IntStr                    1_2_3 : one hundred twenty-third
Type: RatStr                  00123.0 : one hundred twenty-third
Type: NumStr                   1.23e2 : one hundred twenty-third
Type: ComplexStr               123+0i : one hundred twenty-third
Type: IntStr                0b1111011 : one hundred twenty-third
Type: IntStr                    0o173 : one hundred twenty-third
Type: IntStr                     0x7B : one hundred twenty-third
Type: RatStr                    861/7 : one hundred twenty-third

Numeric Strings:
Type: Str                       1_2_3 : one hundred twenty-third
Type: Str                     00123.0 : one hundred twenty-third
Type: Str                      1.23e2 : one hundred twenty-third
Type: Str                      123+0i : one hundred twenty-third
Type: Str                   0b1111011 : one hundred twenty-third
Type: Str                       0o173 : one hundred twenty-third
Type: Str                        0x7B : one hundred twenty-third
Type: Str                       861/7 : one hundred twenty-third

Unicode Numeric Strings:
Type: Str                         123 : one hundred twenty-third
Type: Str                         ١٢٣ : one hundred twenty-third
Type: Str                         ۱۲۳ : one hundred twenty-third
Type: Str                         ߁߂߃ : one hundred twenty-third
Type: Str                         १२३ : one hundred twenty-third
Type: Str                         ১২৩ : one hundred twenty-third
Type: Str                         ੧੨੩ : one hundred twenty-third
Type: Str                         ૧૨૩ : one hundred twenty-third
Type: Str                         ୧୨୩ : one hundred twenty-third
Type: Str                         ௧௨௩ : one hundred twenty-third
Type: Str                         ౧౨౩ : one hundred twenty-third
Type: Str                         ೧೨೩ : one hundred twenty-third
Type: Str                         ൧൨൩ : one hundred twenty-third
Type: Str                         ෧෨෩ : one hundred twenty-third
Type: Str                         ๑๒๓ : one hundred twenty-third
Type: Str                         ໑໒໓ : one hundred twenty-third
Type: Str                         ༡༢༣ : one hundred twenty-third
Type: Str                         ၁၂၃ : one hundred twenty-third
Type: Str                         ႑႒႓ : one hundred twenty-third
Type: Str                         ១២៣ : one hundred twenty-third
Type: Str                         ᠑᠒᠓ : one hundred twenty-third
Type: Str                         ᥇᥈᥉ : one hundred twenty-third
Type: Str                         ᧑᧒᧓ : one hundred twenty-third
Type: Str                         ᪁᪂᪃ : one hundred twenty-third
Type: Str                         ᪑᪒᪓ : one hundred twenty-third
Type: Str                         ᭑᭒᭓ : one hundred twenty-third
Type: Str                         ᮱᮲᮳ : one hundred twenty-third
Type: Str                         ᱁᱂᱃ : one hundred twenty-third
Type: Str                         ᱑᱒᱓ : one hundred twenty-third
Type: Str                         ꘡꘢꘣ : one hundred twenty-third
Type: Str                         ꣑꣒꣓ : one hundred twenty-third
Type: Str                         ꤁꤂꤃ : one hundred twenty-third
Type: Str                         ꧑꧒꧓ : one hundred twenty-third
Type: Str                         ꧱꧲꧳ : one hundred twenty-third
Type: Str                         ꩑꩒꩓ : one hundred twenty-third
Type: Str                         ꯱꯲꯳ : one hundred twenty-third
Type: Str                         １２３ : one hundred twenty-third
Type: Str                         𐒡𐒢𐒣 : one hundred twenty-third
Type: Str                         𐴱𐴲𐴳 : one hundred twenty-third
Type: Str                         𑁧𑁨𑁩 : one hundred twenty-third
Type: Str                         𑃱𑃲𑃳 : one hundred twenty-third
Type: Str                         𑄷𑄸𑄹 : one hundred twenty-third
Type: Str                         𑇑𑇒𑇓 : one hundred twenty-third
Type: Str                         𑋱𑋲𑋳 : one hundred twenty-third
Type: Str                         𑑑𑑒𑑓 : one hundred twenty-third
Type: Str                         𑓑𑓒𑓓 : one hundred twenty-third
Type: Str                         𑙑𑙒𑙓 : one hundred twenty-third
Type: Str                         𑛁𑛂𑛃 : one hundred twenty-third
Type: Str                         𑜱𑜲𑜳 : one hundred twenty-third
Type: Str                         𑣡𑣢𑣣 : one hundred twenty-third
Type: Str                         𑱑𑱒𑱓 : one hundred twenty-third
Type: Str                         𑵑𑵒𑵓 : one hundred twenty-third
Type: Str                         𑶡𑶢𑶣 : one hundred twenty-third
Type: Str                         𖩡𖩢𖩣 : one hundred twenty-third
Type: Str                         𖭑𖭒𖭓 : one hundred twenty-third
Type: Str                         𝟏𝟐𝟑 : one hundred twenty-third
Type: Str                         𝟙𝟚𝟛 : one hundred twenty-third
Type: Str                         𝟣𝟤𝟥 : one hundred twenty-third
Type: Str                         𝟭𝟮𝟯 : one hundred twenty-third
Type: Str                         𝟷𝟸𝟹 : one hundred twenty-third
Type: Str                         𞥑𞥒𞥓 : one hundred twenty-third

Role Mixin:
Type: Str+{<anon|1>}               17 : one hundred twenty-third
```



## Phix


```Phix
include demo\rosetta\number_names.exw

constant {irregs,ordinals} = columnize({{"one","first"},
                                        {"two","second"},
                                        {"three","third"},
                                        {"five","fifth"},
                                        {"eight","eighth"},
                                        {"nine","ninth"},
                                        {"twelve","twelfth"}})
 
function ordinal(string s)
    integer i
    for i=length(s) to 1 by -1 do
        integer ch = s[i]
        if ch=' ' or ch='-' then exit end if
    end for
    integer k = find(s[i+1..$],irregs)
    if k then
        s = s[1..i]&ordinals[k]
    elsif s[$]='y' then
        s[$..$] = "ieth"
    else
        s &= "th"
    end if
    return s
end function

constant tests = {1, 2, 3, 4, 5, 11, 65, 100, 101, 272, 23456, 8007006005004003,
                  123, 00123.0, 1.23e2, 0b1111011, 0o173, 0x7B, 861/7}

for i=1 to length(tests) do
    puts(1,ordinal(spell(tests[i]))&'\n')
end for
```

```txt

first
second
third
fourth
fifth
eleventh
sixty-fifth
one hundredth
one hundred and first
two hundred and seventy-second
twenty-three thousand, four hundred and fifty-sixth
eight quadrillion, seven trillion, six billion, five million, four thousand, and third
one hundred and twenty-third
one hundred and twenty-third
one hundred and twenty-third
one hundred and twenty-third
one hundred and twenty-third
one hundred and twenty-third
one hundred and twenty-third

```


As with the Go solution, this uses the output of spell_integer from the Number_names task (not included here). 


## Python


As with the Go solution, this uses the output of <code>spell_integer</code> from the
[[Number_names#Python|Number_names]] task.


```Python
irregularOrdinals = {
	"one":    "first",
	"two":    "second",
	"three":  "third",
	"five":   "fifth",
	"eight":  "eighth",
	"nine":   "ninth",
	"twelve": "twelfth",
}

def num2ordinal(n):
    conversion = int(float(n))
    num = spell_integer(conversion)
    hyphen = num.rsplit("-", 1)
    num = num.rsplit(" ", 1)
    delim = " "
    if len(num[-1]) > len(hyphen[-1]):
        num = hyphen
        delim = "-"
    
    if num[-1] in irregularOrdinals:
        num[-1] = delim + irregularOrdinals[num[-1]]
    elif num[-1].endswith("y"):
        num[-1] = delim + num[-1][:-1] + "ieth"
    else:
        num[-1] = delim + num[-1] + "th"
    return "".join(num)
    
if __name__ == "__main__":
    tests = "1  2  3  4  5  11  65  100  101  272  23456  8007006005004003 123   00123.0   1.23e2".split()
    for num in tests:
        print("{} => {}".format(num, num2ordinal(num)))


#This is a copy of the code from https://rosettacode.org/wiki/Number_names#Python

TENS = [None, None, "twenty", "thirty", "forty",
        "fifty", "sixty", "seventy", "eighty", "ninety"]
SMALL = ["zero", "one", "two", "three", "four", "five",
         "six", "seven", "eight", "nine", "ten", "eleven",
         "twelve", "thirteen", "fourteen", "fifteen",
         "sixteen", "seventeen", "eighteen", "nineteen"]
HUGE = [None, None] + [h + "illion" 
                       for h in ("m", "b", "tr", "quadr", "quint", "sext", 
                                  "sept", "oct", "non", "dec")]
 
def nonzero(c, n, connect=''):
    return "" if n == 0 else connect + c + spell_integer(n)
 
def last_and(num):
    if ',' in num:
        pre, last = num.rsplit(',', 1)
        if ' and ' not in last:
            last = ' and' + last
        num = ''.join([pre, ',', last])
    return num
 
def big(e, n):
    if e == 0:
        return spell_integer(n)
    elif e == 1:
        return spell_integer(n) + " thousand"
    else:
        return spell_integer(n) + " " + HUGE[e]
 
def base1000_rev(n):
    # generates the value of the digits of n in base 1000
    # (i.e. 3-digit chunks), in reverse.
    while n != 0:
        n, r = divmod(n, 1000)
        yield r
 
def spell_integer(n):
    if n < 0:
        return "minus " + spell_integer(-n)
    elif n < 20:
        return SMALL[n]
    elif n < 100:
        a, b = divmod(n, 10)
        return TENS[a] + nonzero("-", b)
    elif n < 1000:
        a, b = divmod(n, 100)
        return SMALL[a] + " hundred" + nonzero(" ", b, ' and')
    else:
        num = ", ".join([big(e, x) for e, x in
                         enumerate(base1000_rev(n)) if x][::-1])
        return last_and(num)
```

<b>Output</b>

```txt

1 =>  first
2 =>  second
3 =>  third
4 =>  fourth
5 =>  fifth
11 =>  eleventh
65 => sixty-fifth
100 => one hundredth
101 => one hundred and first
272 => two hundred and seventy-second
23456 => twenty-three thousand, four hundred and fifty-sixth
8007006005004003 => eight quadrillion, seven trillion, six billion, five million, four thousand, and third
123 => one hundred and twenty-third
00123.0 => one hundred and twenty-third
1.23e2 => one hundred and twenty-third

```



## REXX


```REXX
/*REXX programs spells out  ordinal numbers  (in English, using the American system).   */
numeric digits 3000                              /*just in case the user uses gihugic #s*/
parse arg n                                      /*obtain optional arguments from the CL*/

if n='' | n=","  then n= 1  2  3  4  5  11  65  100  101  272  23456  8007006005004003

pgmOpts= 'ordinal  quiet'                        /*define options needed for $SPELL#.REX*/


     do j=1  for words(n)                        /*process each of the specified numbers*/
     x=word(n, j)                                /*obtain a number from the input list. */
     os=$spell#(x  pgmOpts)                      /*invoke REXX routine to spell ordinal#*/
     say right(x, max(20, length(x) ) )      ' spelled ordinal number ───► '      os
     end   /*j*/
```

```txt

                   1  spelled ordinal number ───►  first
                   2  spelled ordinal number ───►  second
                   3  spelled ordinal number ───►  third
                   4  spelled ordinal number ───►  fourth
                   5  spelled ordinal number ───►  fifth
                  11  spelled ordinal number ───►  eleventh
                  65  spelled ordinal number ───►  sixty-fifth
                 100  spelled ordinal number ───►  one hundredth
                 101  spelled ordinal number ───►  one hundred first
                 272  spelled ordinal number ───►  two hundred seventy-second
               23456  spelled ordinal number ───►  twenty-three thousand four hundred fifty-sixth
    8007006005004003  spelled ordinal number ───►  eight quadrillion seven trillion six billion five million four thousand third

```

The   '''$SPELL#.REX'''   routine can be found here   ───►   [[$SPELL.REX|$SPELL#.REX]]. 




## Sidef


```ruby
var lingua_en = frequire('Lingua::EN::Numbers')
var tests = [1,2,3,4,5,11,65,100,101,272,23456,8007006005004003]

tests.each {|n|
    printf("%16s : %s\n", n, lingua_en.num2en_ordinal(n))
}
```

```txt

               1 : first
               2 : second
               3 : third
               4 : fourth
               5 : fifth
              11 : eleventh
              65 : sixty-fifth
             100 : one hundredth
             101 : one hundred and first
             272 : two hundred and seventy-second
           23456 : twenty-three thousand four hundred and fifty-sixth
8007006005004003 : eight quadrillion, seven trillion, six billion, five million, four thousand and third

```



## VBA

Inspired by the Phix solution. Uses [[Number_names#VBA|Number names]]

```vb
Private Function ordinal(s As String) As String
    Dim irregs As New Collection
    irregs.Add "first", "one"
    irregs.Add "second", "two"
    irregs.Add "third", "three"
    irregs.Add "fifth", "five"
    irregs.Add "eighth", "eight"
    irregs.Add "ninth", "nine"
    irregs.Add "twelfth", "twelve"
    Dim i As Integer
    For i = Len(s) To 1 Step -1
        ch = Mid(s, i, 1)
        If ch = " " Or ch = "-" Then Exit For
    Next i
    On Error GoTo 1
    ord = irregs(Right(s, Len(s) - i))
    ordinal = Left(s, i) & ord
    Exit Function
1:
    If Right(s, 1) = "y" Then
        s = Left(s, Len(s) - 1) & "ieth"
    Else
        s = s & "th"
    End If
    ordinal = s
End Function
Public Sub ordinals()
    tests = [{1, 2, 3, 4, 5, 11, 65, 100, 101, 272, 23456, 8007006005004003, 123, 00123.0, 1.23E2}]
    init
    For i = 1 To UBound(tests)
        Debug.Print ordinal(spell(tests(i)))
    Next i
End Sub
```
```txt
first
second
third
fourth
fifth
eleventh
sixty-fifth
one hundredth
one hundred and first
two hundred and seventy-second
twenty-three thousand, four hundred and fifty-sixth
eight quadrillion, seven trillion, six billion, five million, four thousandth
one hundred and twenty-third
one hundred and twenty-third
```


## zkl


```zkl
fcn nth(n,th=True){
   var [const]
     nmsth=T("","first","second","third","fourth","fifth","sixth","seventh","eighth","ninth"),
     nms1=T("","one","two","three","four","five","six","seven","eight","nine"),
     nms10=T("ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"),
     nms10th=T("tenth","eleventh","twelfth","thirteenth","fourteenth","fifteenth","sixteenth","seventeenth","eighteenth","nineteenth"),
     nms20=T("twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"),
     nms1000=T("thousand","million","billion","trillion","quadrillion"); // 3,6,9,12,15

   if     (n<0)  String("negative ",nth(-n,th));
   else if(n<10) th and nmsth[n] or nms1[n];
   else if(n<20) th and nms10th[n-10] or nms10[n-10];
   else if(n<10) th and nmsth[n] or nms1[n];
   else if(n<100){
      m,txt := n%10,nms20[n/10-2];
      if(m) String(txt,dash(n%10,"-",th));
      else  String(txt[0,-1],"ieth");
   }
   else if(n<1000) String(nms1[n/100]," hundred",dash(n%100," ",th));
   else{
      n=n.toInt();   // yuck, only here to handle floats, 1.23-->"first"
      ds:=(n.numDigits()-1)/3*3; // 1e3->3, 1e4-->3, 1e5-->3, 1e6-->6, 1e7-->6
      z:=(10).pow(ds);  // 1234-->1000, 12345-->10000
      thou:=ds/3 - 1;	// 1000-->0, 10000-->0, 2,000,000-->1
      nnn,ys := n/z, n%z;
      String((if(nnn<10) nms1[nnn] else nth(nnn,False)),
	  " ",nms1000[thou],
	  if(ys==0) "th" else String(" ",nth(ys,th)));
   }
}
fcn dash(n,d,th){ if(n) String(d,nth(n,th)) else (th and "th" or "") }
```


```zkl
testNs:=L(1,2,3,4,5,11,65,100,101,272,23456,8007006005004003,
          123,00123.0,1.23e2,);
foreach n in (testNs){ 
   if(n.isType(Float)) println("%16.2f --> %s".fmt(n,nth(n)));
   else		       println("%16d --> %s".fmt(n,nth(n)));
}
```

```txt

               1 --> first
               2 --> second
               3 --> third
               4 --> fourth
               5 --> fifth
              11 --> eleventh
              65 --> sixty-fifth
             100 --> one hundredth
             101 --> one hundred first
             272 --> two hundred seventy-second
           23456 --> twenty-three thousand four hundred fifty-sixth
8007006005004003 --> eight quadrillion seven trillion six billion five million four thousand third
             123 --> one hundred twenty-third
          123.00 --> one hundred twenty-third
          123.00 --> one hundred twenty-third

```

