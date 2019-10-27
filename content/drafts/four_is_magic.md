+++
title = "Four is magic"
description = ""
date = 2019-08-25T10:46:16Z
aliases = []
[extra]
id = 21612
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Write a subroutine, function, whatever it may be called in your language, that takes an integer number and returns an English text sequence starting with the English cardinal representation of that integer, the word 'is' and then the English cardinal representation of the count of characters that made up the first word, followed by a comma.

Continue the sequence by using the previous count word as the first word of the next phrase, append 'is' and the cardinal count of the letters in ''that'' word. 

Continue until you reach four. Since four has four characters, finish by adding the words 'four is magic' and a period. All integers will eventually wind up at four.

For instance, suppose your are given the integer '''3'''. Convert '''3''' to '''Three''', add ''' is ''', then the cardinal character count of three, or '''five''', with a comma to separate if from the next phrase. Continue the sequence '''five is four,''' (five has four letters), and finally, '''four is magic.''' 

      '''Three is five, five is four, four is magic.'''

For reference, here are outputs for 0 through 9.

      Zero is four, four is magic.
      One is three, three is five, five is four, four is magic.
      Two is three, three is five, five is four, four is magic.
      Three is five, five is four, four is magic.
      Four is magic.
      Five is four, four is magic.
      Six is three, three is five, five is four, four is magic.
      Seven is five, five is four, four is magic.
      Eight is five, five is four, four is magic.
      Nine is four, four is magic.


;Some task guidelines:
:* You may assume the input will only contain integer numbers.
:* Cardinal numbers between 20 and 100 may use either hyphens or spaces as word separators but they must use a word separator. ('''23''' is '''twenty three''' or '''twenty-three''' not '''twentythree'''.)
:* Cardinal number conversions should follow the [[wp:Long_and_short_scales| English short scale]]. (billion is 1e9, trillion is 1e12, etc.)
:* Cardinal numbers should not include commas. ('''20140''' is '''twenty thousand one hundred forty''' not '''twenty thousand, one hundred forty'''.)
:* When converted to a string, '''100''' should be '''one hundred''', not '''a hundred''' or '''hundred''', '''1000''' should be '''one thousand''', not '''a thousand''' or '''thousand'''.
:* When converted to a string, there should be no '''and''' in the cardinal string. '''130''' should be '''one hundred thirty''' not '''one hundred and thirty'''.
:* When counting characters, count ''all'' of the characters in the cardinal number including spaces and hyphens. '''One hundred fifty-one''' should be '''21''' not '''18'''.
:* The output should follow the format "N is K, K is M, M is ... four is magic." (unless the input is 4, in which case the output should simply be "four is magic.")
:* The output can either be the return value from the function, or be displayed from within the function.
:* You are encouraged, though not mandated to use proper sentence capitalization.
:* You may optionally support negative numbers. '''-7''' is '''negative seven'''.
:* Show the output here for a small representative sample of values, at least 5 but no more than 25. You are free to choose which which numbers to use for output demonstration.


You can choose to use a library, (module, external routine, whatever) to do the cardinal conversions as long as the code is easily and freely available to the public.  

If you roll your own, make the routine accept at minimum any integer from 0 up to 999999. If you use a pre-made library, support at least up to unsigned 64 bit integers. (or the largest integer supported in your language if it is less.)

Four is magic is a popular code-golf task. '''This is not code golf.''' Write legible, idiomatic and well formatted code.<BR>


;Related tasks:
:*   [[Four is the number of_letters in the ...]]
:*   [[Look-and-say sequence]]
:*   [[Number names]]
:*   [[Self-describing numbers]]
:*   [[Self-referential sequence]]
:*   [[Spelling of ordinal numbers]]





## AWK


```AWK

# syntax: GAWK -f FOUR_IS_MAGIC.AWK
BEGIN {
    init_numtowords()
    n = split("-1 0 1 2 3 4 5 6 7 8 9 11 21 1995 1000000 1234567890 1100100100100",arr," ")
    for (i=1; i<=n; i++) {
      a = arr[i]
      printf("%s: ",a)
      do {
        if (a == 4) {
          break
        }
        a = numtowords(a)
        b = numtowords(length(a))
        printf("%s is %s, ",a,b)
        a = length(a)
      } while (b !~ /^four$/)
      printf("four is magic.\n")
    }
    exit(0)
}
# source: The AWK Programming Language, page 75
function numtowords(n,  minus,str) {
    if (n < 0) {
      n = n * -1
      minus = "minus "
    }
    if (n == 0) {
      str = "zero"
    }
    else {
      str = intowords(n)
    }
    gsub(/  /," ",str)
    gsub(/ $/,"",str)
    return(minus str)
}
function intowords(n) {
    n = int(n)
    if (n >= 1000000000000) {
      return intowords(n/1000000000000) " trillion " intowords(n%1000000000000)
    }
    if (n >= 1000000000) {
      return intowords(n/1000000000) " billion " intowords(n%1000000000)
    }
    if (n >= 1000000) {
      return intowords(n/1000000) " million " intowords(n%1000000)
    }
    if (n >= 1000) {
      return intowords(n/1000) " thousand " intowords(n%1000)
    }
    if (n >= 100) {
      return intowords(n/100) " hundred " intowords(n%100)
    }
    if (n >= 20) {
      return tens[int(n/10)] " " intowords(n%10)
    }
    return(nums[n])
}
function init_numtowords() {
    split("one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen",nums," ")
    split("ten twenty thirty forty fifty sixty seventy eighty ninety",tens," ")
}

```

{{out}}

```txt

-1: minus one is nine, nine is four, four is magic.
0: zero is four, four is magic.
1: one is three, three is five, five is four, four is magic.
2: two is three, three is five, five is four, four is magic.
3: three is five, five is four, four is magic.
4: four is magic.
5: five is four, four is magic.
6: six is three, three is five, five is four, four is magic.
7: seven is five, five is four, four is magic.
8: eight is five, five is four, four is magic.
9: nine is four, four is magic.
11: eleven is six, six is three, three is five, five is four, four is magic.
21: twenty one is ten, ten is three, three is five, five is four, four is magic.
1995: one thousand nine hundred ninety five is thirty seven, thirty seven is twelve, twelve is six, six is three, three is five, five is four, four is magic.
1000000: one million is eleven, eleven is six, six is three, three is five, five is four, four is magic.
1234567890: one billion two hundred thirty four million five hundred sixty seven thousand eight hundred ninety is ninety eight, ninety eight is twelve, twelve is six, six is three, three is five, five is four, four is magic.
1100100100100: one trillion one hundred billion one hundred million one hundred thousand one hundred is eighty five, eighty five is eleven, eleven is six, six is three, three is five, five is four, four is magic.

```



## Common Lisp


```Lisp
(defun integer-to-text (int)
  (format nil "~@(~A~)" (with-output-to-string (out)
                          (loop for n = int then (length c)
                                for c = (format nil "~R" n)
                                while (/= n 4)
                                do (format out "~A is ~R, " c (length c))
                                finally (format out "four is magic.")))))
```


{{out}}

```txt

"One thousand twenty-four is twenty-four, twenty-four is eleven, eleven is six, six is three, three is five, five is four, four is magic."

```


=={{header|F_Sharp|F#}}==

### The Function


```fsharp

//Express an Integer in English Language. Nigel Galloway: September 19th., 2018
let fN=[|[|"";"one";"two";"three";"four";"five";"six";"seven";"eight";"nine"|];
         [|"ten";"eleven";"twelve";"thirteen";"fourteen";"fifteen";"sixteen";"seventeen";"eighteen";"nineteen"|];
         [|"";"";"twenty";"thirty";"fourty";"fifty";"sixty";"seventy";"eighty";"ninety"|]|]
let rec I2α α β=match α with |α when α<20     ->β+fN.[α/10].[α%10]
                             |α when α<100    ->I2α (α%10) (β+fN.[2].[α/10]+if α%10>0 then " " else "")
                             |α when α<1000   ->I2α (α-(α/100)*100) (β+fN.[0].[α/100]+" hunred"+if α%100>0 then " and " else "")
                             |α when α<1000000->I2α (α%1000) (β+(I2α (α/1000) "")+" thousand"+if α%100=0 then "" else if (α-(α/1000)*1000)<100 then " and " else " ")

```


### The Task


```fsharp

let rec printI2α=function |0->printf "naught->"; printI2α 6
                          |4->printfn "four is magic"
                          |n when n<0->let g = I2α -n "minus " in printf "%s->" g; printI2α (g.Length)
                          |n         ->let g = I2α n "" in printf "%s->" g; printI2α (g.Length)
let N=System.Random()
List.init 25 (fun _->N.Next 999999) |> List.iter printI2α

```

{{out}}

```txt

seven hundred and fifty thousand nine hundred and eighty eight->sixty->five->four is magic
nine hundred and fifty four thousand two hundred and twenty two->sixty one->nine->four is magic
three hundred and seventy two thousand nine hundred and thirty one->sixty four->ten->three->five->four is magic
six hundred and three thousand six hundred and eighteen->fifty three->eleven->six->three->five->four is magic
two hundred and forty nine thousand three hundred and eighty eight->sixty five->ten->three->five->four is magic
four hundred and sixty two thousand four hundred and ninety nine->sixty two->nine->four is magic
six hundred and fifty thousand eight hundred and seventy five->fifty nine->ten->three->five->four is magic
six hundred and ninety three thousand two hundred and seventy nine->sixty four->ten->three->five->four is magic
one hundred and thirty three thousand four hundred and seventy six->sixty four->ten->three->five->four is magic
seven hundred and thirty two thousand nine hundred and fifteen->sixty->five->four is magic
seven hundred and seven thousand five hundred and forty one->fifty eight->eleven->six->three->five->four is magic
twenty five thousand six hundred and two->thirty nine->eleven->six->three->five->four is magic
seven hundred and sixty nine thousand two hundred and sixty four->sixty two->nine->four is magic
eight hundred and ninety five thousand eight hundred and two->fifty eight->eleven->six->three->five->four is magic
four hundred and eleven thousand one hundred and four->fifty one->nine->four is magic
four hundred and ninety five thousand eight hundred and eighty one->sixty four->ten->three->five->four is magic
six hundred and fifty six thousand one hundred and eighty seven->sixty one->nine->four is magic
five hundred and twenty two thousand seven hundred and fifty->fifty eight->eleven->six->three->five->four is magic
three hundred and forty four thousand and ninety two->fifty two->nine->four is magic
three hundred and forty one thousand seven hundred and forty four->sixty five->ten->three->five->four is magic
eight hundred and eighty four thousand two hundred and fifty->fifty eight->eleven->six->three->five->four is magic
six hundred and forty thousand seven hundred and sixteen->fifty five->ten->three->five->four is magic
six hundred and eight thousand three hundred and five->fifty one->nine->four is magic
three hundred and ninety nine thousand two hundred and sixty eight->sixty four->ten->three->five->four is magic
six hundred and ninety two thousand two hundred and seventy five->sixty two->nine->four is magic

```

Some particular values:

```txt

printI2α 0 -> naught->six->three->five->four is magic
printI2α 4 -> four is magic
printI2α 999999 -> nine hundred and ninety nine thousand nine hundred and ninety nine->sixty four->ten->three->five->four is magic
printI2α -23 -> minus twenty three->eighteen->eight->five->four is magic

```



## Factor

Factor's <code>math.text.english</code> vocabulary does most of the heavy lifting. Since <code>number>text</code> produces <tt><i>" and "</i></tt> and <tt><i>","</i></tt> in its output, they are removed with a regular expression.

```factor
USING: ascii formatting io kernel make math.text.english regexp
sequences ;
IN: rosetta-code.four-is-magic

! Strip " and " and "," from the output of Factor's number>text
! word with a regular expression.
: number>english ( n -- str )
    number>text R/ and |,/ "" re-replace ;

! Return the length of the input integer's text form.
! e.g. 1 -> 3
: next-len ( n -- m ) number>english length ;

! Given a starting integer, return the sequence of lengths
! terminating with 4.
! e.g. 1 -> { 1 3 5 4 }
: len-chain ( n -- seq )
    [ [ dup 4 = ] [ dup , next-len ] until , ] { } make ;

! Convert a non-four number to its phrase form.
! e.g. 6 -> "six is three, "
: non-four ( n -- str )
    number>english dup length number>english
    "%s is %s, " sprintf ;

! Convert any number to its phrase form.
! e.g. 4 -> "four is magic."
: phrase ( n -- str )
    dup 4 = [ drop "four is magic." ] [ non-four ] if ;
    
: say-magic ( n -- )
    len-chain [ phrase ] map concat capitalize print ;
    
{ 1 4 -11 100 112719908181724 -612312 } [ say-magic ] each
```

{{out}}

```txt

One is three, three is five, five is four, four is magic.
Four is magic.
Negative eleven is fifteen, fifteen is seven, seven is five, five is four, four is magic.
One hundred is eleven, eleven is six, six is three, three is five, five is four, four is magic.
One hundred twelve trillion seven hundred nineteen billion nine hundred eight million one hundred eighty-one thousand seven hundred twenty-four is one hundred forty-three, one hundred forty-three is twenty-three, twenty-three is twelve, twelve is six, six is three, three is five, five is four, four is magic.
Negative six hundred twelve thousand three hundred twelve is fifty-seven, fifty-seven is eleven, eleven is six, six is three, three is five, five is four, four is magic.

```



## Go

Uses the <code>say</code> function from the
[[Number names#Go|Number names]] task.

```go
package main

import (
	"fmt"
	"math"
	"strings"
)

func main() {
	for _, n := range [...]int64{
		0, 4, 6, 11, 13, 75, 100, 337, -164,
		math.MaxInt64,
	} {
		fmt.Println(fourIsMagic(n))
	}
}

func fourIsMagic(n int64) string {
	s := say(n)
	s = strings.ToUpper(s[:1]) + s[1:]
	t := s
	for n != 4 {
		n = int64(len(s))
		s = say(n)
		t += " is " + s + ", " + s
	}
	t += " is magic."
	return t
}
 
// Following is from https://rosettacode.org/wiki/Number_names#Go

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

{{out}}

```txt

Zero is four, four is magic.
Four is magic.
Six is three, three is five, five is four, four is magic.
Eleven is six, six is three, three is five, five is four, four is magic.
Thirteen is eight, eight is five, five is four, four is magic.
Seventy-five is twelve, twelve is six, six is three, three is five, five is four, four is magic.
One hundred is eleven, eleven is six, six is three, three is five, five is four, four is magic.
Three hundred thirty-seven is twenty-six, twenty-six is ten, ten is three, three is five, five is four, four is magic.
Negative one hundred sixty-four is thirty-one, thirty-one is ten, ten is three, three is five, five is four, four is magic.
Nine quintillion two hundred twenty-three quadrillion three hundred seventy-two trillion thirty-six billion eight hundred fifty-four million seven hundred seventy-five thousand eight hundred seven is one hundred ninety-six, one hundred ninety-six is twenty-two, twenty-two is ten, ten is three, three is five, five is four, four is magic.

```



## J


```J

names =. 'one';'two';'three';'four';'five';'six';'seven';'eight';'nine';'ten';'eleven';'twelve';'thirteen';'fourteen';'fifteen';'sixteen';'seventeen';'eighteen';'nineteen'

tens =. '';'twenty';'thirty';'forty';'fifty';'sixty';'seventy';'eighty';'ninety'

NB. selects the xth element from list y
lookup =: >@{:@{.

NB. string formatting
addspace =: ((' '"_, ]) ` ]) @. (<&0 @ {: @ $)

NB. numbers in range 1 to 19
s1 =: lookup&names

NB. numbers in range 20 to 99
s2d=: (lookup&tens @ <. @ %&10) , addspace @ (s1 @ (10&|))

NB. numbers in range 100 to 999
s3d =: s1 @ (<.@%&100), ' hundred', addspace @ s2d @ (100&|)

NB. numbers in range 1 to 999
s123d =: s1 ` s2d ` s3d @. (>& 19 + >&99)

NB. numbers in range 1000 to 999999
s456d =: (s123d @<.@%&1000), ' thousand', addspace @ s123d @ (1000&|)

NB. stringify numbers in range 1 to 999999
stringify =: s123d ` s456d @. (>&999)

NB. takes an int and returns an int of the length of the string of the input
lengthify =: {: @ $ @ stringify 

NB. determines the string that should go after ' is '
what =: ((stringify @ lengthify), (', '"_)) ` ('magic'"_) @. (=&4)

runonce =: stringify , ' is ', what

run =: runonce, ((run @ lengthify) ` (''"_) @. (=&4))

doall =: run"0

inputs =: 4 8 16 25 89 365 2586 25865 369854 

doall inputs


```

{{out}} 


```txt


four is magic     
                                                                                                                                                       
eight is five, five is four, four is magic         
                                                                                                                      
sixteen is seven, seven is five, five is four, four is magic      
                                                                                                       
twenty five is eleven, eleven is six, six is three, three is five, five is four, four is magic        
                                                                   
eighty nine is eleven, eleven is six, six is three, three is five, five is four, four is magic         
                                                                  
three hundred sixty five is twenty four, twenty four is eleven, eleven is six, six is three, three is five, five is four, four is magic      
                            
two thousand five hundred eighty six is thirty six, thirty six is ten, ten is three, three is five, five is four, four is magic           
                               
twenty five thousand eight hundred sixty five is forty five, forty five is ten, ten is three, three is five, five is four, four is magic         
                        
three hundred sixty nine thousand eight hundred fifty four is fifty eight, fifty eight is eleven, eleven is six, six is three, three is five, five is four, four is magic
   

```



## Julia


```julia
# The num2text routines are from the "Number names" task, updated for Julia 1.0

const stext = ["one", "two", "three", "four", "five",
               "six", "seven", "eight", "nine"]
const teentext = ["eleven", "twelve", "thirteen", "fourteen",
                  "fifteen", "sixteen", "seventeen",
                  "eighteen", "nineteen"]
const tenstext = ["ten", "twenty", "thirty", "forty", "fifty",
                  "sixty", "seventy", "eighty", "ninety"]
const ordstext = ["million", "billion", "trillion",
                  "quadrillion", "quintillion", "sextillion",
                  "septillion", "octillion", "nonillion",
                  "decillion", "undecillion", "duodecillion",
                  "tredecillion", "quattuordecillion", "quindecillion",
                  "sexdecillion", "septendecillion", "octodecillion",
                  "novemdecillion", "vigintillion"]
 
function normalize_digits!(a)
    while  0 < length(a) && a[end] == 0
        pop!(a)
    end
    return length(a)
end
 
function digits2text!(d, use_short_scale=true)
    ndig = normalize_digits!(d)
    0 < ndig || return ""
    if ndig < 7
        s = ""
        if 3 < ndig
            t = digits2text!(d[1:3])
            s = digits2text!(d[4:end])*" thousand"
            0 < length(t) || return s
            if occursin("and", t)
                return s*" "*t
            else
                return s*" and "*t
            end
        end
        if ndig == 3
            s *= stext[pop!(d)]*" hundred"
            ndig = normalize_digits!(d)
            0 < ndig || return s
            s *= " and "
        end
        1 < ndig || return s*stext[pop!(d)]
        j, i = d
        j != 0 || return s*tenstext[i]
        i != 1 || return s*teentext[j]
        return s*tenstext[i]*"-"*stext[j]
    end
    s = digits2text!(d[1:6])
    d = d[7:end]
    dgrp = use_short_scale ? 3 : 6
    ord = 0
    while(dgrp < length(d))
        ord += 1
        t = digits2text!(d[1:dgrp])
        d = d[(dgrp+1):end]
        0 < length(t) || continue
        t = t*" "*ordstext[ord]
        if length(s) == 0
            s = t
        else
            s = t*" "*s
        end
    end
    ord += 1
    t = digits2text!(d)*" "*ordstext[ord]
    0 < length(s) || return t
    return t*" "*s
end
 
function num2text(n, use_short_scale=true)
    -1 < n || return "minus "*num2text(-n, use_short_scale)
    0 < n || return "zero"
    toobig = use_short_scale ? big(10)^66 : big(10)^126
    n < toobig || return "too big to say"
    return digits2text!(digits(n, base=10), use_short_scale)
end
  
 
function magic(n)
    str = uppercasefirst(num2text(n))
    n = length(str)
    while true
        numtext = num2text(n)
        str *= " is " * numtext
        if numtext == "four"
            break
        end
        str *= ", " * numtext
        n = length(numtext)
    end
    println(str[1:7] == "Four is" ? "Four is magic." : "$str, four is magic.")
end

 
for n in [0, 4, 6, 11, 13, 75, 337, -164, 9_876_543_209]
    magic(n)
end

```
 {{output}} 
```txt

Zero is four, four is magic.
Four is magic.
Six is three, three is five, five is four, four is magic.
Eleven is six, six is three, three is five, five is four, four is magic.
Thirteen is eight, eight is five, five is four, four is magic.
Seventy-five is twelve, twelve is six, six is three, three is five, five is four, four is magic.
Three hundred and thirty-seven is thirty, thirty is six, six is three, three is five, five is four, four is magic.
Minus one hundred and sixty-four is thirty-two, thirty-two is ten, ten is three, three is five, five is four, four is magic.
Nine billion eight hundred and seventy-six million five hundred and forty-three thousand two hundred and nine is one hundred and nine, one hundred and nine is twenty, twenty is six, six is three, three is five, five is four, four is magic.

```



## Kotlin

This uses the code I wrote for the [[Number names]] task, appropriately adjusted to deal with this task. Input is limited to '''signed''' 64 bit integers as Kotlin doesn't currently support unsigned types.

```scala
// version 1.1.4-3

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

fun numToText(n: Long): String {
    if (n == 0L) return "zero"
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
            if (remainder > 0) sb.append(" ")
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
    var big = 1000L
    for (i in 1..6) {
        if (digits3[i] > 0) {
            var text2 = strings[i] + " " + bigNames[big]
            if (text.length > 0) text2 += " "
            text = text2 + text
        }
        big *= 1000
    }
    if (maxNeg) text = text.dropLast(5) + "eight"
    if (neg) text = "negative " + text
    return text
}

fun fourIsMagic(n: Long): String {
    if (n == 4L) return "Four is magic."
    var text = numToText(n).capitalize()
    val sb = StringBuilder()
    while (true) {
        val len = text.length.toLong()
        if (len == 4L) return sb.append("$text is four, four is magic.").toString()
        val text2 = numToText(len)
        sb.append("$text is $text2, ")
        text = text2
    }
}

fun main(args: Array<String>) {
    val la = longArrayOf(0, 4, 6, 11, 13, 75, 100, 337, -164, 9_223_372_036_854_775_807L)
    for (i in la) {
        println(fourIsMagic(i))
        println()
    }
}
```


{{out}}

```txt

Zero is four, four is magic.

Four is magic.

Six is three, three is five, five is four, four is magic.

Eleven is six, six is three, three is five, five is four, four is magic.

Thirteen is eight, eight is five, five is four, four is magic.

Seventy-five is twelve, twelve is six, six is three, three is five, five is four, four is magic.

One hundred is eleven, eleven is six, six is three, three is five, five is four, four is magic.

Three hundred thirty-seven is twenty-six, twenty-six is ten, ten is three, three is five, five is four, four is magic.

Negative one hundred sixty-four is thirty-one, thirty-one is ten, ten is three, three is five, five is four, four is magic.

Nine quintillion two hundred twenty-three quadrillion three hundred seventy-two trillion thirty-six billion eight hundred fifty-four million seven hundred seventy-five thousand eight hundred seven is one hundred ninety-six, one hundred ninety-six is twenty-two, twenty-two is ten, ten is three, three is five, five is four, four is magic.

```



## Perl

{{trans|Perl 6}}

```perl
use Lingua::EN::Numbers qw(num2en);

sub cardinal {
    my($n) = @_;
    (my $en = num2en($n)) =~ s/\ and|,//g;
    $en;
}

sub magic {
    my($int) = @_;
    my $str;
    while () {
       $str .= cardinal($int) . " is ";
       if ($int == 4) {
           $str .= "magic.\n";
           last
       } else {
           $int = length cardinal($int);
           $str .= cardinal($int) . ", ";
       }
   }
   ucfirst $str;
}

print magic($_) for 0, 4, 6, 11, 13, 75, 337, -164, 9_876_543_209;
```

{{out}}

```txt
Zero is four, four is magic.
Four is magic.
Six is three, three is five, five is four, four is magic.
Eleven is six, six is three, three is five, five is four, four is magic.
Thirteen is eight, eight is five, five is four, four is magic.
Seventy-five is twelve, twelve is six, six is three, three is five, five is four, four is magic.
Three hundred thirty-seven is twenty-six, twenty-six is ten, ten is three, three is five, five is four, four is magic.
Negative one hundred sixty-four is thirty-one, thirty-one is ten, ten is three, three is five, five is four, four is magic.
Nine billion eight hundred seventy-six million five hundred forty-three thousand two hundred nine is ninety-seven, ninety-seven is twelve, twelve is six, six is three, three is five, five is four, four is magic.
```



## Perl 6

{{works with|Rakudo|2017.09}}
Lingua::EN::Numbers module available from the [https://modules.perl6.org/search/?q=Lingua%3A%3AEN%3A%3ANumbers Perl 6 ecosystem].


```perl6
use Lingua::EN::Numbers; # Version 2.4.0 or higher

sub card ($n) { cardinal($n).subst(/','/, '', :g) }

sub magic (Int $int is copy) {
    my $string;
    loop {
       $string ~= "{ card($int) } is ";
       if $int = ($int == 4) ?? 0 !! card($int).chars {
           $string ~= "{ card($int) }, "
       } else {
           $string ~= "magic.\n";
           last
       }
   }
   $string.tc
}

.&magic.say for 0, 4, 6, 11, 13, 75, 337, -164, 9876543209, 2**256;
```

{{out}}
<pre style="width:98%;overflow:wrap;">Zero is four, four is magic.

Four is magic.

Six is three, three is five, five is four, four is magic.

Eleven is six, six is three, three is five, five is four, four is magic.

Thirteen is eight, eight is five, five is four, four is magic.

Seventy-five is twelve, twelve is six, six is three, three is five, five is four, four is magic.

Three hundred thirty-seven is twenty-six, twenty-six is ten, ten is three, three is five, five is four, four is magic.

Negative one hundred sixty-four is thirty-one, thirty-one is ten, ten is three, three is five, five is four, four is magic.

Nine billion eight hundred seventy-six million five hundred forty-three thousand two hundred nine is ninety-seven, ninety-seven is twelve, twelve is six, six is three, three is five, five is four, four is magic.

One hundred fifteen quattuorvigintillion seven hundred ninety-two trevigintillion eighty-nine duovigintillion two hundred thirty-seven unvigintillion three hundred sixteen vigintillion one hundred ninety-five novemdecillion four hundred twenty-three octodecillion five hundred seventy septendecillion nine hundred eighty-five sexdecillion eight quindecillion six hundred eighty-seven quattuordecillion nine hundred seven tredecillion eight hundred fifty-three duodecillion two hundred sixty-nine undecillion nine hundred eighty-four decillion six hundred sixty-five nonillion six hundred forty octillion five hundred sixty-four septillion thirty-nine sextillion four hundred fifty-seven quintillion five hundred eighty-four quadrillion seven trillion nine hundred thirteen billion one hundred twenty-nine million six hundred thirty-nine thousand nine hundred thirty-six is eight hundred sixty-nine, eight hundred sixty-nine is twenty-four, twenty-four is eleven, eleven is six, six is three, three is five, five is four, four is magic.
```



## Phix

Note that on 32-bit Phix integers/atoms are only accurate to 9,007,199,254,740,992 (a hardware limit of 64-bit floating point registers) so if you need more than that this will need to be reworked to use bigatoms.

```Phix
--<adapted from demo\rosetta\number_names.exw, which alas outputs ",", "and", uses "minus" instead of "negative", etc...>
constant twenties = {"zero","one","two","three","four","five","six","seven","eight","nine","ten",
    "eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"},
         decades = {"twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"}
 
function hundred(integer n)
    if n<20 then
        return twenties[mod(n,20)+1]
    elsif mod(n,10)=0 then
        return decades[mod(floor(n/10),10)-1]
    end if
    return decades[mod(floor(n/10),10)-1] & '-' & twenties[mod(n,10)+1]
end function
 
function thousand(integer n)
    if n<100 then
        return hundred(n)
    elsif mod(n,100)=0 then
        return twenties[mod(floor(n/100),20)+1]&" hundred"
    end if
    return twenties[mod(floor(n/100),20)+1] & " hundred " & hundred(mod(n,100))
end function
 
constant orders = {{power(10,12),"trillion"},
                   {power(10,9),"billion"},
                   {power(10,6),"million"},
                   {power(10,3),"thousand"}}
 
function triplet(integer n)
atom order, high, low
string name, res = ""
    for i=1 to length(orders) do
        {order,name} = orders[i]
        high = floor(n/order)
        low = mod(n,order)
        if high!=0 then
            res &= thousand(high)&' '&name
        end if
        n = low
        if low=0 then exit end if
        if length(res) and high!=0 then
            res &= " "
        end if
    end for
    if n!=0 or res="" then
        res &= thousand(floor(n))
    end if
    return res
end function
 
function spell(integer n)
string res = ""
    if n<0 then
        res = "negative "
        n = -n
    end if
    res &= triplet(n)
    return res
end function
--</adapted from number_names.exw>

function fourIsMagic(atom n)
    string s = spell(n)
    s[1] = upper(s[1])
    string t = s
    while n!=4 do
        n = length(s)
        s = spell(n)
        t &= " is " & s & ", " & s
    end while
    t &= " is magic.\n"
    return t
end function

constant tests = {-7, -1, 0, 1, 2, 3, 4, 23, 1e9, 20140, 100, 130, 151, 999999}
for i=1 to length(tests) do
    puts(1,fourIsMagic(tests[i]))
end for
```

{{out}}

```txt

Negative seven is fourteen, fourteen is eight, eight is five, five is four, four is magic.
Negative one is twelve, twelve is six, six is three, three is five, five is four, four is magic.
Zero is four, four is magic.
One is three, three is five, five is four, four is magic.
Two is three, three is five, five is four, four is magic.
Three is five, five is four, four is magic.
Four is magic.
Twenty-three is twelve, twelve is six, six is three, three is five, five is four, four is magic.
One billion is eleven, eleven is six, six is three, three is five, five is four, four is magic.
Twenty thousand one hundred forty is thirty-three, thirty-three is twelve, twelve is six, six is three, three is five, five is four, four is magic.
One hundred is eleven, eleven is six, six is three, three is five, five is four, four is magic.
One hundred thirty is eighteen, eighteen is eight, eight is five, five is four, four is magic.
One hundred fifty-one is twenty-one, twenty-one is ten, ten is three, three is five, five is four, four is magic.
Nine hundred ninety-nine thousand nine hundred ninety-nine is fifty-eight, fifty-eight is eleven, eleven is six, six is three, three is five, five is four, four is magic.

```



## Python

Python 3 version. Should work for integers up to at least 10^3003. It can be extended easily to arbitrary integers by adding to the numbers dict.


```python
import random
from collections import OrderedDict

numbers = {  # taken from https://en.wikipedia.org/wiki/Names_of_large_numbers#cite_ref-a_14-3
    1: 'one',
    2: 'two',
    3: 'three',
    4: 'four',
    5: 'five',
    6: 'six',
    7: 'seven',
    8: 'eight',
    9: 'nine',
    10: 'ten',
    11: 'eleven',
    12: 'twelve',
    13: 'thirteen',
    14: 'fourteen',
    15: 'fifteen',
    16: 'sixteen',
    17: 'seventeen',
    18: 'eighteen',
    19: 'nineteen',
    20: 'twenty',
    30: 'thirty',
    40: 'forty',
    50: 'fifty',
    60: 'sixty',
    70: 'seventy',
    80: 'eighty',
    90: 'ninety',
    100: 'hundred',
    1000: 'thousand',
    10 ** 6: 'million',
    10 ** 9: 'billion',
    10 ** 12: 'trillion',
    10 ** 15: 'quadrillion',
    10 ** 18: 'quintillion',
    10 ** 21: 'sextillion',
    10 ** 24: 'septillion',
    10 ** 27: 'octillion',
    10 ** 30: 'nonillion',
    10 ** 33: 'decillion',
    10 ** 36: 'undecillion',
    10 ** 39: 'duodecillion',
    10 ** 42: 'tredecillion',
    10 ** 45: 'quattuordecillion',
    10 ** 48: 'quinquadecillion',
    10 ** 51: 'sedecillion',
    10 ** 54: 'septendecillion',
    10 ** 57: 'octodecillion',
    10 ** 60: 'novendecillion',
    10 ** 63: 'vigintillion',
    10 ** 66: 'unvigintillion',
    10 ** 69: 'duovigintillion',
    10 ** 72: 'tresvigintillion',
    10 ** 75: 'quattuorvigintillion',
    10 ** 78: 'quinquavigintillion',
    10 ** 81: 'sesvigintillion',
    10 ** 84: 'septemvigintillion',
    10 ** 87: 'octovigintillion',
    10 ** 90: 'novemvigintillion',
    10 ** 93: 'trigintillion',
    10 ** 96: 'untrigintillion',
    10 ** 99: 'duotrigintillion',
    10 ** 102: 'trestrigintillion',
    10 ** 105: 'quattuortrigintillion',
    10 ** 108: 'quinquatrigintillion',
    10 ** 111: 'sestrigintillion',
    10 ** 114: 'septentrigintillion',
    10 ** 117: 'octotrigintillion',
    10 ** 120: 'noventrigintillion',
    10 ** 123: 'quadragintillion',
    10 ** 153: 'quinquagintillion',
    10 ** 183: 'sexagintillion',
    10 ** 213: 'septuagintillion',
    10 ** 243: 'octogintillion',
    10 ** 273: 'nonagintillion',
    10 ** 303: 'centillion',
    10 ** 306: 'uncentillion',
    10 ** 309: 'duocentillion',
    10 ** 312: 'trescentillion',
    10 ** 333: 'decicentillion',
    10 ** 336: 'undecicentillion',
    10 ** 363: 'viginticentillion',
    10 ** 366: 'unviginticentillion',
    10 ** 393: 'trigintacentillion',
    10 ** 423: 'quadragintacentillion',
    10 ** 453: 'quinquagintacentillion',
    10 ** 483: 'sexagintacentillion',
    10 ** 513: 'septuagintacentillion',
    10 ** 543: 'octogintacentillion',
    10 ** 573: 'nonagintacentillion',
    10 ** 603: 'ducentillion',
    10 ** 903: 'trecentillion',
    10 ** 1203: 'quadringentillion',
    10 ** 1503: 'quingentillion',
    10 ** 1803: 'sescentillion',
    10 ** 2103: 'septingentillion',
    10 ** 2403: 'octingentillion',
    10 ** 2703: 'nongentillion',
    10 ** 3003: 'millinillion'
}
numbers = OrderedDict(sorted(numbers.items(), key=lambda t: t[0], reverse=True))


def string_representation(i: int) -> str:
    """
    Return the english string representation of an integer
    """
    if i == 0:
        return 'zero'

    words = ['negative'] if i < 0 else []
    working_copy = abs(i)

    for key, value in numbers.items():
        if key <= working_copy:
            times = int(working_copy / key)

            if key >= 100:
                words.append(string_representation(times))

            words.append(value)
            working_copy -= times * key

        if working_copy == 0:
            break

    return ' '.join(words)


def next_phrase(i: int):
    """
    Generate all the phrases
    """
    while not i == 4:  # Generate phrases until four is reached
        str_i = string_representation(i)
        len_i = len(str_i)

        yield str_i, 'is', string_representation(len_i)

        i = len_i

    # the last phrase
    yield string_representation(i), 'is', 'magic'


def magic(i: int) -> str:
    phrases = []

    for phrase in next_phrase(i):
        phrases.append(' '.join(phrase))

    return f'{", ".join(phrases)}.'.capitalize()


if __name__ == '__main__':

    for j in (random.randint(0, 10 ** 3) for i in range(5)):
        print(j, ':\n', magic(j), '\n')

    for j in (random.randint(-10 ** 24, 10 ** 24) for i in range(2)):
        print(j, ':\n', magic(j), '\n')
```


{{out}}
<pre style="width:98%;overflow:wrap;">475 :
 Four hundred seventy five is twenty five, twenty five is eleven, eleven is six, six is three, three is five, five is four, four is magic. 

968 :
 Nine hundred sixty eight is twenty four, twenty four is eleven, eleven is six, six is three, three is five, five is four, four is magic. 

304 :
 Three hundred four is eighteen, eighteen is eight, eight is five, five is four, four is magic. 

544 :
 Five hundred forty four is twenty three, twenty three is twelve, twelve is six, six is three, three is five, five is four, four is magic. 

394 :
 Three hundred ninety four is twenty five, twenty five is eleven, eleven is six, six is three, three is five, five is four, four is magic. 

-49587779907680717664396 :
 Negative forty nine sextillion five hundred eighty seven quintillion seven hundred seventy nine quadrillion nine hundred seven trillion six hundred eighty billion seven hundred seventeen million six hundred sixty four thousand three hundred ninety six is two hundred fifty one, two hundred fifty one is twenty one, twenty one is ten, ten is three, three is five, five is four, four is magic. 

874143425855745733896030 :
 Eight hundred seventy four sextillion one hundred forty three quintillion four hundred twenty five quadrillion eight hundred fifty five trillion seven hundred forty five billion seven hundred thirty three million eight hundred ninety six thousand thirty is two hundred fifty three, two hundred fifty three is twenty three, twenty three is twelve, twelve is six, six is three, three is five, five is four, four is magic. 
```



## Racket


```racket

#lang racket

(require rackunit)

(define smalls
  (map symbol->string
       '(zero one two three four five six seven eight nine ten eleven twelve
         thirteen fourteen fifteen sixteen seventeen eighteen nineteen)))
 
(define tens
  (map symbol->string
       '(zero ten twenty thirty forty fifty sixty seventy eighty ninety)))
 
(define larges
  (map symbol->string
       '(thousand million billion trillion quadrillion quintillion sextillion
         septillion octillion nonillion decillion undecillion duodecillion
         tredecillion quattuordecillion quindecillion sexdecillion
         septendecillion octodecillion novemdecillion vigintillion)))
 
(define (number->words n)
  (define (step div suffix separator [subformat number->words])
    (define-values [q r] (quotient/remainder n div))
    (define S (if suffix (~a (subformat q) " " suffix) (subformat q)))
    (if (zero? r) S (~a S separator (number->words r))))
  (cond [(< n 0) (~a "negative " (number->words (- n)))]
        [(< n 20) (list-ref smalls n)]
        [(< n 100) (step 10 #f "-" (curry list-ref tens))]
        [(< n 1000) (step 100 "hundred" " ")]
        [else (let loop ([N 1000000] [D 1000] [unit larges])
                (cond [(null? unit)
                       (error 'number->words "number too big: ~e" n)]
                      [(< n N) (step D (car unit) " ")]
                      [else (loop (* 1000 N) (* 1000 D) (cdr unit))]))]))
 
(define (first-cap s)
  (~a (string-upcase (substring s 0 1)) (substring s 1)))
 
(define (magic word [acc null])
  (if (equal? word "four")
      (string-join (reverse (cons "four is magic." acc)) ", \n")
      (let* ([word-len (string-length word)]
             [words (number->words word-len)])
        (magic words
               (cons (string-append word " is " words) acc)))))

(define (number-magic n) 
  (first-cap (magic (number->words n))))
 
(for ([n (append (range 11)
                 '(-10 23 172 20140 100 130 999999 876000000
                       874143425855745733896030))])
  (displayln n)
  (displayln (number-magic n))
  (newline))

```


{{out}}


```txt

0
Zero is four, 
four is magic.

1
One is three, 
three is five, 
five is four, 
four is magic.

2
Two is three, 
three is five, 
five is four, 
four is magic.

3
Three is five, 
five is four, 
four is magic.

4
Four is magic.

5
Five is four, 
four is magic.

6
Six is three, 
three is five, 
five is four, 
four is magic.

7
Seven is five, 
five is four, 
four is magic.

8
Eight is five, 
five is four, 
four is magic.

9
Nine is four, 
four is magic.

10
Ten is three, 
three is five, 
five is four, 
four is magic.

-10
Negative ten is twelve, 
twelve is six, 
six is three, 
three is five, 
five is four, 
four is magic.

23
Twenty-three is twelve, 
twelve is six, 
six is three, 
three is five, 
five is four, 
four is magic.

172
One hundred seventy-two is twenty-three, 
twenty-three is twelve, 
twelve is six, 
six is three, 
three is five, 
five is four, 
four is magic.

20140
Twenty thousand one hundred forty is thirty-three, 
thirty-three is twelve, 
twelve is six, 
six is three, 
three is five, 
five is four, 
four is magic.

100
One hundred is eleven, 
eleven is six, 
six is three, 
three is five, 
five is four, 
four is magic.

130
One hundred thirty is eighteen, 
eighteen is eight, 
eight is five, 
five is four, 
four is magic.

999999
Nine hundred ninety-nine thousand nine hundred ninety-nine is fifty-eight, 
fifty-eight is eleven, 
eleven is six, 
six is three, 
three is five, 
five is four, 
four is magic.

876000000
Eight hundred seventy-six million is thirty-three, 
thirty-three is twelve, 
twelve is six, 
six is three, 
three is five, 
five is four, 
four is magic.

874143425855745733896030
Eight hundred seventy-four sextillion one hundred forty-three quintillion four hundred twenty-five quadrillion eight hundred fifty-five trillion seven hundred forty-five billion seven hundred thirty-three million eight hundred ninety-six thousand thirty is two hundred fifty-three, 
two hundred fifty-three is twenty-three, 
twenty-three is twelve, 
twelve is six, 
six is three, 
three is five, 
five is four, 
four is magic.

```



## REXX

The numbers used for the default were taken from the   '''Kotlin'''   example.

Numbers are limited to 3,003 decimal digits, the maximum number that the   '''$SPELL#'''   REXX program will handle.

```rexx
/*REXX pgm converts a # to English into the phrase:  a is b, b is c, ... four is magic. */
numeric digits 3003                              /*be able to handle gihugic numbers.   */
parse arg x                                      /*obtain optional numbers from the C.L.*/
if x=''  then x=-164 0 4 6 11 13 75 100 337 9223372036854775807    /*use these defaults?*/
@.=.                                             /*stemmed array used for memoization.  */
       do j=1  for words(x)                      /*process each of the numbers in list. */
       say 4_is( word(x, j) )                    /*display phrase that'll be returned.  */
       say                                       /*display a blank line between outputs.*/
       end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
4_is:  procedure expose @.;  parse arg #,,$                   /*obtain the start number.*/
       if #\==4  then do  until L==4                          /*Not 4?   Process number.*/
                      @.#=$spell#(#  'quiet minus negative')  /*spell number in English.*/
                      #=@.#;            L=length(#)           /*get the length of spelt#*/
                      if @.L==.  then @.L=$spell#(L 'quiet')  /*¬spelt before? Spell it.*/
                      $=$  #  "is"  @.L','                    /*add phrase to the answer*/
                      #=L                                     /*use the new number, ··· */
                      end   /*until*/                         /* ··· which will be spelt*/
       $=strip($ 'four is magic.')               /*finish the sentence with the finale. */
       parse var $ first 2 other;  upper first   /*capitalize the first letter of output*/
       return first || other                     /*return the sentence to the invoker.  */
```

The   '''$SPELL#.REX'''   routine can be found here   ───►   [[$SPELL.REX|$SPELL#.REX]]. 



{{out|output|text=  when using the default inputs:}}

```txt

Negative one hundred sixty-four is thirty-one, thirty-one is ten, ten is three, three is five, five is four, four is magic.

Zero is four, four is magic.

Four is magic.

Six is three, three is five, five is four, four is magic.

Eleven is six, six is three, three is five, five is four, four is magic.

Thirteen is eight, eight is five, five is four, four is magic.

Seventy-five is twelve, twelve is six, six is three, three is five, five is four, four is magic.

One hundred is eleven, eleven is six, six is three, three is five, five is four, four is magic.

Three hundred thirty-seven is twenty-six, twenty-six is ten, ten is three, three is five, five is four, four is magic.

Nine quintillion two hundred twenty-three quadrillion three hundred seventy-two trillion thirty-six billion eight hundred fifty-four million seven hundred seventy-five thousand eight hundred seven is one hundred ninety-six, one hundred ninety-six is twenty-two, twenty-two is ten, ten is three, three is five, five is four, four is magic.

```



## Ring



```ring

/*  Checking numbers from 0 to 10 */
for c = 0 to 10
	See checkmagic(c) + NL
next


/* The functions */

Func CheckMagic numb 
	CardinalN = ""
	Result = ""
	if isnumber(numb) = false or numb < 0 or numb > 999_999_999_999_999
		Return "ERROR: Number entered is incorrect"
	ok
	if numb = 4 
		Result = "Four is magic."
	else
		While True
			if CardinalN = "four" 
				Result += "four is magic"
				exit
			ok
			strnumb = StringNumber(numb)
			CardinalN = StringNumber(len(strnumb))
			Result += strnumb + " is " + CardinalN + ", "
			numb = len(strnumb)
		End
		Result += "."
		Result = upper(Result[1]) + Right(Result, len(Result) -1)
	ok

	Return  Result

Func StringNumber cnumb

	NumStr = [:n0 = "zero", :n1 = "one", :n2 = "two", :n3 = "three", :n4 = "four", :n5 = "five", 
		:n6 = "six", :n7 = "seven", :n8 = "eight", :n9 = "nine", :n10 = "ten", 
		:n11 = "eleven", :n12 = "twelve", :n13 = "thirteen", :n14 = "fourteen", :n15 = "fifteen", 
		:n16 = "sixteen", :n17 = "seventeen", :n18 = "eighteen", :n19 = "nineteen", 
		:n20 = "twenty", :n30 = "thirty", :n40 = "fourty", :n50 = "fifty", :n60 = "sixty", :n70 = "seventy", :n80 = "eighty", :n90 = "ninety"]

	numLev = [:l1 = "", :l2 = "thousand", :l3 = "million", :l4 = "billion", :l5 = "trillion"]

	Result = ""

	if cnumb > 0 
		decimals(0)
		snumb = string((cnumb))
		lnumb = [""]
		fl = floor(len(snumb) / 3)
		if fl > 0
			for i = 1 to  fl
				lnumb[i] = right(snumb, 3)
				snumb = left(snumb, len(snumb) -3)
				lnumb + ""
			next
			if (len(snumb) % 3) > 0
				lnumb[len(lnumb)] = snumb
			else
				del(lnumb, len(lnumb))
			ok
		else
			lnumb[1] = snumb
		ok
		for l = len(lnumb) to 1 step -1
			bnumb = lnumb[l]
			bResult = ""
			if number(bnumb) != 0
				for n = len(bnumb) to 1 step -1
					if (len(bnumb) = 3 and n = 2) or (len(bnumb) = 2 and n = 1)
						if number(bnumb[n]) > 1
							eval("bResult = NumStr[:n" + bnumb[n] + "0] + ' ' + bResult")
						elseif number(bnumb[n]) = 1
							eval("bResult = NumStr[:n" + bnumb[n] + bnumb[n+1] + "] + ' ' + bResult")
						ok
					else
						if len(bnumb) = 3 and n = 1 and number(bnumb[1]) > 0
							if trim(bResult) != ""
								bResult = " " + bResult
							ok
							if number(bnumb[1]) > 1
								bResult = "hundreds" + bResult
							else
								bResult = "hundred" + bResult
							ok
							if left(trim(bResult), 7) = "hundred"
								bResult = bResult + " "
							ok
						ok
						if (len(bnumb) = 3 and n = 1 and number(bnumb[1]) = 0) OR (len(bnumb) = n and number(bnumb[n]) = 0) OR (len(bnumb) = 3 and number(bnumb[2]) = 1) OR (len(bnumb) = 2 and number(bnumb[1]) = 1)
							loop
						ok
						eval("bResult = NumStr[:n" + bnumb[n] + "] + ' ' + bResult")
					ok
				next
				Result = Result + bResult
				if  l > 1 
					if number(bnumb) > 1
						eval("Result = Result + numLev[:l" + l + "] + 's ' ")
					else
						eval("Result = Result + numLev[:l" + l + "] + ' ' ")
					ok
				ok
			ok
		next
	else
		Result = Result + NumStr[:n0]
	ok
	
Return trim(Result)

```

Output:

```txt

Zero is four, four is magic.
One is three, three is five, five is four, four is magic.
Two is three, three is five, five is four, four is magic.
Three is five, five is four, four is magic.
Four is magic.
Five is four, four is magic.
Six is three, three is five, five is four, four is magic.
Seven is five, five is four, four is magic.
Eight is five, five is four, four is magic.
Nine is four, four is magic.
Ten is three, three is five, five is four, four is magic.

```



## Sidef

{{trans|Perl}}

```ruby
func cardinal(n) {
    static lingua_en = frequire("Lingua::EN::Numbers")
    lingua_en.num2en(n) - / and|,/g
}

func four_is_magic(n) {
    var str = ""
    loop {
       str += (cardinal(n) + " is ")
       if (n == 4) {
           str += "magic."
           break
       } else {
           n = cardinal(n).len
           str += (cardinal(n) + ", ")
       }
   }
   str.tc
}

[0, 4, 6, 11, 13, 75, 337, -164, 9_876_543_209].each { |n|
    say four_is_magic(n)
}
```

{{out}}

```txt

Zero is four, four is magic.
Four is magic.
Six is three, three is five, five is four, four is magic.
Eleven is six, six is three, three is five, five is four, four is magic.
Thirteen is eight, eight is five, five is four, four is magic.
Seventy-five is twelve, twelve is six, six is three, three is five, five is four, four is magic.
Three hundred thirty-seven is twenty-six, twenty-six is ten, ten is three, three is five, five is four, four is magic.
Negative one hundred sixty-four is thirty-one, thirty-one is ten, ten is three, three is five, five is four, four is magic.
Nine billion eight hundred seventy-six million five hundred forty-three thousand two hundred nine is ninety-seven, ninety-seven is twelve, twelve is six, six is three, three is five, five is four, four is magic.

```



## zkl

{{trans|Perl6}}
Limitiation: zkl only has 64 bit signed integars.

Uses the nth function from [[Spelling_of_ordinal_numbers#zkl]]


```zkl
fcn fourIsMagic(int){
   if(int==0) return("Zero is four, four is magic.");
   string:="";
   while(1){ c:=nth(int,False);
      string+="%s is ".fmt(c);
      if(int = ( if(int==4) 0 else c.len() )){
	 string+="%s, ".fmt(nth(int,False));
      }else{
         string+="magic.";
	 break;
      }
   }
   string[0].toUpper() + string[1,*]
}
```


```zkl
foreach n in (T(0,4,6,11,13,75,337,-164,9876543209)){
   println(fourIsMagic(n),"\n")
}
```

{{out}}

```txt

Zero is four, four is magic.

Four is magic.

Six is three, three is five, five is four, four is magic.

Eleven is six, six is three, three is five, five is four, four is magic.

Thirteen is eight, eight is five, five is four, four is magic.

Seventy-five is twelve, twelve is six, six is three, three is five, five is four, four is magic.

Three hundred thirty-seven is twenty-six, twenty-six is ten, ten is three, three is five, five is four, four is magic.

Negative one hundred sixty-four is thirty-one, thirty-one is ten, ten is three, three is five, five is four, four is magic.

Nine billion eight hundred seventy-six million five hundred forty-three thousand two hundred nine is ninety-seven, ninety-seven is twelve, twelve is six, six is three, three is five, five is four, four is magic.

```

