+++
title = "Four is the number of letters in the ..."
description = ""
date = 2019-09-02T17:54:22Z
aliases = []
[extra]
id = 21590
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "go",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "related_tasks",
  "rexx",
  "zkl",
]
+++

The     <big> '''Four is ...''' </big>     sequence is based on the counting of the number of
letters in the words of the (never─ending) sentence:
   <big>Four is the number of letters in the first word of this sentence, two in the second,
   three in the third, six in the fourth, two in the fifth, seven in the sixth, ··· </big>


;Definitions and directives:
:*   English is to be used in spelling numbers.
:*   '''Letters'''   are defined as the upper─ and lowercase letters in the Latin alphabet   ('''A──►Z'''   and   '''a──►z''').
:*   Commas are not counted,   nor are hyphens (dashes or minus signs).
:*   '''twenty─three'''   has eleven letters.
:*   '''twenty─three'''   is considered one word   (which is hyphenated).
:*   no   <big>''' ''and'' '''</big>   words are to be used when spelling a (English) word for a number.
:*   The American version of numbers will be used here in this task   (as opposed to the British version).
          '''2,000,000,000'''   is two billion,   ''not''   two milliard.


## Task

:*   Write a driver (invoking routine) and a function (subroutine/routine···) that returns the sequence (for any positive integer) of the number of letters in the first   '''N'''    words in the never─ending sentence.   For instance, the portion of the never─ending sentence shown above (2<sup>nd</sup> sentence of this task), the sequence would be:
          '''4  2  3  6  2  7'''
:*   Only construct as much as is needed for the never─ending sentence.
:*   Write a driver (invoking routine) to show the number of letters in the   N<sup>th</sup>   word,   ''as well as''   showing the   N<sup>th</sup>   word itself.
:*   After each test case, show the total number of characters   (including blanks, commas, and punctuation)   of the sentence that was constructed.
:*   Show all output here.


;Test cases:
  Display the first  201  numbers in the sequence   (and the total number of characters in the sentence).
  Display the number of letters  (and the word itself)  of the       1,000<sup>th</sup>  word.
  Display the number of letters  (and the word itself)  of the      10,000<sup>th</sup>  word.
  Display the number of letters  (and the word itself)  of the     100,000<sup>th</sup>  word.
  Display the number of letters  (and the word itself)  of the   1,000,000<sup>th</sup>  word.
  Display the number of letters  (and the word itself)  of the  10,000,000<sup>th</sup>  word  (optional).


## Related tasks

:*   [[Four is magic]]
:*   [[Look-and-say sequence]]
:*   [[Number names]]
:*   [[Self-describing numbers]]
:*   [[Self-referential sequence]]
:*   [[Spelling of ordinal numbers]]


;Also see:
:*   See the OEIS sequence [[oeis:A072425| A72425  "Four is the number of letters..."]].
:*   See the OEIS sequence [[oeis:A072424| A72424  "Five's the number of letters..."]]





## Go

This is a naive non-optimized implementation
that stores each word of the sentence so far.
It uses the
<code>sayOrdinal</code> and <code>say</code>
functions from the
[[Spelling_of_ordinal_numbers#Go|Spelling of ordinal numbers]] task
(omitted from this listing).

```Go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	f := NewFourIsSeq()
	fmt.Print("The lengths of the first 201 words are:")
	for i := 1; i <= 201; i++ {
		if i%25 == 1 {
			fmt.Printf("\n%3d: ", i)
		}
		_, n := f.WordLen(i)
		fmt.Printf(" %2d", n)
	}
	fmt.Println()
	fmt.Println("Length of sentence so far:", f.TotalLength())
	/* For debugging:
	log.Println("sentence:", strings.Join(f.words, " "))
	for i, w := range f.words {
		log.Printf("%3d: %2d %q\n", i, countLetters(w), w)
	}
	log.Println(f.WordLen(2202))
	log.Println("len(f.words):", len(f.words))
	log.Println("sentence:", strings.Join(f.words, " "))
	*/
	for i := 1000; i <= 1e7; i *= 10 {
		w, n := f.WordLen(i)
		fmt.Printf("Word %8d is %q, with %d letters.", i, w, n)
		fmt.Println("  Length of sentence so far:", f.TotalLength())
	}
}

type FourIsSeq struct {
	i     int      // index of last word processed
	words []string // strings.Join(words," ") gives the sentence so far
}

func NewFourIsSeq() *FourIsSeq {
	return &FourIsSeq{
		//words: strings.Fields("Four is the number of letters in the first word of this sentence,"),
		words: []string{
			"Four", "is", "the", "number",
			"of", "letters", "in", "the",
			"first", "word", "of", "this", "sentence,",
		},
	}
}

// WordLen returns the w'th word and its length (only counting letters).
func (f *FourIsSeq) WordLen(w int) (string, int) {
	for len(f.words) < w {
		f.i++
		n := countLetters(f.words[f.i])
		ns := say(int64(n))
		os := sayOrdinal(int64(f.i+1)) + ","
		// append something like: "two in the second,"
		f.words = append(f.words, strings.Fields(ns)...)
		f.words = append(f.words, "in", "the")
		f.words = append(f.words, strings.Fields(os)...)
	}
	word := f.words[w-1]
	return word, countLetters(word)
}

// TotalLength returns the total number of characters (including blanks,
// commas, and punctuation) of the sentence so far constructed.
func (f FourIsSeq) TotalLength() int {
	cnt := 0
	for _, w := range f.words {
		cnt += len(w) + 1
	}
	return cnt - 1
}

func countLetters(s string) int {
	cnt := 0
	for _, r := range s {
		if unicode.IsLetter(r) {
			cnt++
		}
	}
	return cnt
}

// ...
// the contents of
// https://rosettacode.org/wiki/Spelling_of_ordinal_numbers#Go
// omitted from this listing
// ...

```

```txt
The lengths of the first 201 words are:
  1:   4  2  3  6  2  7  2  3  5  4  2  4  8  3  2  3  6  5  2  3  5  3  2  3  6
 26:   3  2  3  5  5  2  3  5  3  2  3  7  5  2  3  6  4  2  3  5  4  2  3  5  3
 51:   2  3  8  4  2  3  7  5  2  3 10  5  2  3 10  3  2  3  9  5  2  3  9  3  2
 76:   3 11  4  2  3 10  3  2  3 10  5  2  3  9  4  2  3 11  5  2  3 12  3  2  3
101:  11  5  2  3 12  3  2  3 11  5  2  3 11  3  2  3 13  5  2  3 12  4  2  3 11
126:   4  2  3  9  3  2  3 11  5  2  3 12  4  2  3 11  5  2  3 12  3  2  3 11  5
151:   2  3 11  5  2  3 13  4  2  3 12  3  2  3 11  5  2  3  8  3  2  3 10  4  2
176:   3 11  3  2  3 10  5  2  3 11  4  2  3 10  4  2  3 10  3  2  3 12  5  2  3
201:  11
Length of sentence so far: 1203
Word     1000 is "in", with 2 letters.  Length of sentence so far: 6279
Word    10000 is "in", with 2 letters.  Length of sentence so far: 64140
Word   100000 is "one", with 3 letters.  Length of sentence so far: 659474
Word  1000000 is "the", with 3 letters.  Length of sentence so far: 7113621
Word 10000000 is "thousand", with 8 letters.  Length of sentence so far: 70995756

```



## Julia

The functions num2text and numtext2ordinal are from the "Spelling of ordinal numbers" and "Number names" tasks, updated for Julia 1.0 and to remove the "and" words.
```julia
using DataStructures # for deque

const seed = "Four is the number of letters in the first word of this sentence, "
const (word2, word3) = ("in", "the")

lettercount(w) = length(w) -  length(collect(eachmatch(r"-", w)))
splits(txt) = [x.match for x in eachmatch(r"[\w\-]+", txt)]
todq(sentence) = (d = Deque{String}(); map(x->push!(d, x), splits(sentence)[2:end]); d)

struct CountLetters
    seedsentence::String
    words::Deque{String}
    commasafter::Vector{Int}
    CountLetters(s) = new(s, todq(s), [13])
    CountLetters() = CountLetters(seed)
end

function Base.iterate(iter::CountLetters, state = (1, 5, ""))
    if length(iter.words) < 1
        return nothing
    end
    returnword = popfirst!(iter.words)
    nextwordindex = state[1] + 1
    wordlen = lettercount(returnword)
    wordvec = vcat(num2text(wordlen), word2, word3, splits(numtext2ordinal(num2text(nextwordindex))))
    map(x -> push!(iter.words, x), wordvec) 
    push!(iter.commasafter, length(iter.words))
    added = length(returnword) + (nextwordindex in iter.commasafter ? 2 : 1)
    (wordlen, (nextwordindex, state[2] + added, returnword))
end

Base.eltype(iter::CountLetters) = Int

function firstN(n = 201)
    countlet = CountLetters()
    print("It is interesting how identical lengths align with 20 columns.\n   1:   4")
    iter_result = iterate(countlet)
    itercount = 2
    while iter_result != nothing
        (wlen, state) = iter_result
        print(lpad(string(wlen), 4))
        if itercount % 20 == 0
            print("\n", lpad(itercount+1, 4), ":")
        elseif itercount >= n
            break
        end
        iter_result = iterate(countlet, state)
        itercount += 1
    end
    println()
end

function sumwords(iterations)
    countlet = CountLetters()
    iter_result = iterate(countlet)
    itercount = 2
    while iter_result != nothing
        (wlen, state) = iter_result
        if itercount == iterations
            return state
        end
        iter_result = iterate(countlet, state)
        itercount += 1
    end
    throw("Iteration failed on \"Four is the number\" task.")
end

firstN()

for n in [2202, 1000, 10000, 100000, 1000000, 10000000]
    (itercount, totalletters, lastword) = sumwords(n)
    println("$n words -> $itercount iterations, $totalletters letters total, ",
            "last word \"$lastword\" with $(length(lastword)) letters.")
end
```
 {{output}} 
```txt

It is interesting how identical lengths align with 20 columns.
   1:   4   2   3   6   2   7   2   3   5   4   2   4   8   3   2   3   6   5   2   3
  21:   5   3   2   3   6   3   2   3   5   5   2   3   5   3   2   3   7   5   2   3
  41:   6   4   2   3   5   4   2   3   5   3   2   3   8   4   2   3   7   5   2   3
  61:  10   5   2   3  10   3   2   3   9   5   2   3   9   3   2   3  11   4   2   3
  81:  10   3   2   3  10   5   2   3   9   4   2   3  11   5   2   3  12   3   2   3
 101:  11   5   2   3  12   3   2   3  11   5   2   3  11   3   2   3  13   5   2   3
 121:  12   4   2   3  11   4   2   3   9   3   2   3  11   5   2   3  12   4   2   3
 141:  11   5   2   3  12   3   2   3  11   5   2   3  11   5   2   3  13   4   2   3
 161:  12   3   2   3  11   5   2   3   8   3   2   3  10   4   2   3  11   3   2   3
 181:  10   5   2   3  11   4   2   3  10   4   2   3  10   3   2   3  12   5   2   3
 201:  11
2202 words -> 2202 iterations, 14035 letters total, last word "ninety-ninth" with 12 letters.
1000 words -> 1000 iterations, 6290 letters total, last word "in" with 2 letters.
10000 words -> 10000 iterations, 64320 letters total, last word "in" with 2 letters.
100000 words -> 100000 iterations, 661369 letters total, last word "one" with 3 letters.
1000000 words -> 1000000 iterations, 7127541 letters total, last word "the" with 3 letters.
10000000 words -> 10000000 iterations, 71103026 letters total, last word "thousand" with 8 letters.

```



## Kotlin

This pulls in (slightly adjusted) code from related tasks to convert numbers to text or ordinals.

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
    if (this == "zero") return "zeroth"  // or alternatively 'zeroeth'
    val splits = this.split(' ', '-')
    val last = splits[splits.lastIndex]
    return if (irregOrdinals.containsKey(last)) this.dropLast(last.length) + irregOrdinals[last]!!
           else if (last.endsWith("y")) this.dropLast(1) + "ieth"
           else this + "th"
}

fun numToText(n: Long, uk: Boolean = false): String {
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

    val strings = Array(7) { threeDigitsToText(digits3[it]) }
    var text = strings[0]
    var andNeeded = uk && digits3[0] in 1..99
    var big = 1000L
    for (i in 1..6) {
        if (digits3[i] > 0) {
            var text2 = strings[i] + " " + bigNames[big]
            if (text.isNotEmpty()) {
                text2 += if (andNeeded) " and " else " "  // no commas inserted in this version
                andNeeded = false
            }
            else andNeeded = uk && digits3[i] in 1..99
            text = text2 + text
        }
        big *= 1000
    }
    if (maxNeg) text = text.dropLast(5) + "eight"
    if (neg) text = "minus " + text
    return text
}

val opening = "Four is the number of letters in the first word of this sentence,".split(' ')

val String.adjustedLength get() = this.replace(",", "").replace("-", "").length  // no ',' or '-'

fun getWords(n: Int): List<String> {
    val words = mutableListOf<String>()
    words.addAll(opening)
    if (n > opening.size) {
        var k = 2
        while (true) {
            val len = words[k - 1].adjustedLength
            val text = numToText(len.toLong())
            val splits = text.split(' ')
            words.addAll(splits)
            words.add("in")
            words.add("the")
            val text2 = numToText(k.toLong()).toOrdinal() + ","  // add trailing comma
            val splits2 = text2.split(' ')
            words.addAll(splits2)
            if (words.size >= n) break
            k++
        }
    }
    return words
}

fun getLengths(n: Int): Pair<List<Int>, Int> {
    val words = getWords(n)
    val lengths = words.take(n).map { it.adjustedLength }
    val sentenceLength = words.sumBy { it.length } + words.size - 1  // includes hyphens, commas & spaces
    return Pair(lengths, sentenceLength)
}

fun getLastWord(n: Int): Triple<String, Int, Int> {
    val words = getWords(n)
    val nthWord = words[n - 1]
    val nthWordLength = nthWord.adjustedLength
    val sentenceLength = words.sumBy { it.length } + words.size - 1  // includes hyphens, commas & spaces
    return Triple(nthWord, nthWordLength, sentenceLength)
}

fun main(args: Array<String>) {
    var n = 201
    println("The lengths of the first $n words are:\n")
    val (list, sentenceLength) = getLengths(n)
    for (i in 0 until n) {
        if (i % 25 == 0) {
            if (i > 0) println()
            print("${"%3d".format(i + 1)}: ")
        }
        print("%3d".format(list[i]))
    }
    println("\n\nLength of sentence = $sentenceLength\n")
    n = 1_000
    do {
        var (word, wLen, sLen) = getLastWord(n)
        if (word.endsWith(",")) word = word.dropLast(1)  // strip off any trailing comma
        println("The length of word $n [$word] is $wLen")
        println("Length of sentence = $sLen\n")
        n *= 10
    }
    while (n <= 10_000_000)
}
```


```txt

The lengths of the first 201 words are:

  1:   4  2  3  6  2  7  2  3  5  4  2  4  8  3  2  3  6  5  2  3  5  3  2  3  6
 26:   3  2  3  5  5  2  3  5  3  2  3  7  5  2  3  6  4  2  3  5  4  2  3  5  3
 51:   2  3  8  4  2  3  7  5  2  3 10  5  2  3 10  3  2  3  9  5  2  3  9  3  2
 76:   3 11  4  2  3 10  3  2  3 10  5  2  3  9  4  2  3 11  5  2  3 12  3  2  3
101:  11  5  2  3 12  3  2  3 11  5  2  3 11  3  2  3 13  5  2  3 12  4  2  3 11
126:   4  2  3  9  3  2  3 11  5  2  3 12  4  2  3 11  5  2  3 12  3  2  3 11  5
151:   2  3 11  5  2  3 13  4  2  3 12  3  2  3 11  5  2  3  8  3  2  3 10  4  2
176:   3 11  3  2  3 10  5  2  3 11  4  2  3 10  4  2  3 10  3  2  3 12  5  2  3
201:  11

Length of sentence = 1203

The length of word 1000 [in] is 2
Length of sentence = 6279

The length of word 10000 [in] is 2
Length of sentence = 64140

The length of word 100000 [one] is 3
Length of sentence = 659474

The length of word 1000000 [the] is 3
Length of sentence = 7113621

The length of word 10000000 [thousand] is 8
Length of sentence = 70995756

```



## Perl

Uses <code>Lingua::EN::Numbers</code> module to generate number names. State variable in <tt>extend_to</tt> routine keeps track of last word tallied.
```perl
use feature 'state';
use Lingua::EN::Numbers qw(num2en num2en_ordinal);

my @sentence = split / /, 'Four is the number of letters in the first word of this sentence, ';

sub extend_to {
    my($last) = @_;
    state $index = 1;
    until ($#sentence > $last) {
        push @sentence, split ' ', num2en(alpha($sentence[$index])) . ' in the ' . no_c(num2en_ordinal(1+$index)) . ',';
        $index++;
    }
}

sub alpha { my($s) = @_; $s =~ s/\W//gi; length $s }
sub no_c  { my($s) = @_; $s =~ s/\ and|,//g;   return $s }
sub count { length(join ' ', @sentence[0..-1+$_[0]]) . " characters in the sentence, up to and including this word.\n" }

print "First 201 word lengths in the sequence:\n";
extend_to(201);
for (0..200) {
    printf "%3d", alpha($sentence[$_]);
    print "\n" unless ($_+1) % 32;
}
print "\n" . count(201) . "\n";

for (1e3, 1e4, 1e5, 1e6, 1e7) {
    extend_to($_);
    print
        ucfirst(num2en_ordinal($_)) .  " word, '$sentence[$_-1]' has " . alpha($sentence[$_-1]) .  " characters. \n" .
        count($_) . "\n";
}
```

```txt
First 201 word lengths in the sequence:
  4  2  3  6  2  7  2  3  5  4  2  4  8  3  2  3  6  5  2  3  5  3  2  3  6  3  2  3  5  5  2  3
  5  3  2  3  7  5  2  3  6  4  2  3  5  4  2  3  5  3  2  3  8  4  2  3  7  5  2  3 10  5  2  3
 10  3  2  3  9  5  2  3  9  3  2  3 11  4  2  3 10  3  2  3 10  5  2  3  9  4  2  3 11  5  2  3
 12  3  2  3 11  5  2  3 12  3  2  3 11  5  2  3 11  3  2  3 13  5  2  3 12  4  2  3 11  4  2  3
  9  3  2  3 11  5  2  3 12  4  2  3 11  5  2  3 12  3  2  3 11  5  2  3 11  5  2  3 13  4  2  3
 12  3  2  3 11  5  2  3  8  3  2  3 10  4  2  3 11  3  2  3 10  5  2  3 11  4  2  3 10  4  2  3
 10  3  2  3 12  5  2  3 11
1203 characters in the sentence, up to and including this word.

One thousandth word, 'in' has 2 characters. 
6249 characters in the sentence, up to and including this word.

Ten thousandth word, 'in' has 2 characters. 
64097 characters in the sentence, up to and including this word.

One hundred thousandth word, 'one' has 3 characters. 
659455 characters in the sentence, up to and including this word.

One millionth word, 'the' has 3 characters. 
7113560 characters in the sentence, up to and including this word.

Ten millionth word, 'thousand' has 8 characters. 
70995729 characters in the sentence, up to and including this word.
```



## Perl 6

Uses the Lingua::EN::Numbers module to generate both cardinal and ordinal numbers. This module places commas in number words between 3-orders-of-magnitude clusters. E.G. <code>12345678.&ordinal</code> becomes: twelve million, three hundred forty-five thousand, six hundred seventy-eighth. Uses a custom 'no-commas' routine to filter them out for accurate character counts. Generates the 'sentence' lazily so only the words needed are ever calculated and reified.


```perl6
use Lingua::EN::Numbers;
no-commas(True);

my $index = 1;
my @sentence = flat 'Four is the number of letters in the first word of this sentence, '.words,
  { @sentence[$index++].&alpha.&cardinal, 'in', 'the', |($index.&ordinal ~ ',').words } ... * ;

sub alpha ( $str ) { $str.subst(/\W/, '', :g).chars }
sub count ( $index ) { @sentence[^$index].join(' ').chars ~ " characters in the sentence, up to and including this word.\n" }

say 'First 201 word lengths in the sequence:';
put ' ', map { @sentence[$_].&alpha.fmt("%2d") ~ (((1+$_) %% 25) ?? "\n" !! '') }, ^201;
say 201.&count;

for 1e3, 1e4, 1e5, 1e6, 1e7 {
    say "{.&ordinal.tc} word, '{@sentence[$_ - 1]}', has {@sentence[$_ - 1].&alpha} characters. ", .&count
}
```

```txt
First 201 word lengths in the sequence:
  4  2  3  6  2  7  2  3  5  4  2  4  8  3  2  3  6  5  2  3  5  3  2  3  6
  3  2  3  5  5  2  3  5  3  2  3  7  5  2  3  6  4  2  3  5  4  2  3  5  3
  2  3  8  4  2  3  7  5  2  3 10  5  2  3 10  3  2  3  9  5  2  3  9  3  2
  3 11  4  2  3 10  3  2  3 10  5  2  3  9  4  2  3 11  5  2  3 12  3  2  3
 11  5  2  3 12  3  2  3 11  5  2  3 11  3  2  3 13  5  2  3 12  4  2  3 11
  4  2  3  9  3  2  3 11  5  2  3 12  4  2  3 11  5  2  3 12  3  2  3 11  5
  2  3 11  5  2  3 13  4  2  3 12  3  2  3 11  5  2  3  8  3  2  3 10  4  2
  3 11  3  2  3 10  5  2  3 11  4  2  3 10  4  2  3 10  3  2  3 12  5  2  3
 11
1203 characters in the sentence, up to and including this word.

One thousandth word, 'in', has 2 characters. 6249 characters in the sentence, up to and including this word.

Ten thousandth word, 'in', has 2 characters. 64097 characters in the sentence, up to and including this word.

One hundred thousandth word, 'one', has 3 characters. 659455 characters in the sentence, up to and including this word.

One millionth word, 'the', has 3 characters. 7113560 characters in the sentence, up to and including this word.

Ten millionth word, 'thousand', has 8 characters. 70995729 characters in the sentence, up to and including this word.
```



## Phix

Note that my version of [[Number_names]] includes "and" (and ","), that others do not, hence the kill_and()/grr below and the minor mismatch of sentence lengths.

```Phix
include demo\rosetta\number_names.exw

-- as per Spelling_of_ordinal_numbers#Phix:
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
--/copy of Spelling_of_ordinal_numers#Phix

function countLetters(string s)
    integer res = 0
    for i=1 to length(s) do
        integer ch = s[i]
        if (ch>='A' and ch<='Z')
        or (ch>='a' and ch<='z') then
            res += 1
        end if
    end for
    return res
end function

sequence words = split("Four is the number of letters in the first word of this sentence,")
integer fi = 1

function kill_and(sequence s)
--grr...
    for i=length(s) to 1 by -1 do
        if s[i] = "and" then
            s[i..i] = {}
        end if
    end for
    return s
end function

function WordLen(integer w)
-- Returns the w'th word and its length (only counting letters).
    while length(words)<w do
        fi += 1
        integer n = countLetters(words[fi])
        sequence ns = kill_and(split(spell(n)))
        sequence os = kill_and(split(ordinal(spell(fi)) & ","))
        -- append eg {"two","in","the","second,"}
        words &= ns&{"in","the"}&os
    end while
    string word = words[w]
    return {word, countLetters(word)}
end function

function TotalLength()
-- Returns the total number of characters (including blanks,
-- commas, and punctuation) of the sentence so far constructed.
    integer res = 0
    for i=1 to length(words) do
        res += length(words[i])+1
    end for
    return res
end function

procedure main()
integer i,n
string w
    printf(1,"The lengths of the first 201 words are:\n")
    for i=1 to 201 do
        if mod(i,25)==1 then
            printf(1,"\n%3d: ", i)
        end if
        {?,n} = WordLen(i)
        printf(1," %2d", n)
    end for
    printf(1,"\nLength of sentence so far:%d\n", TotalLength())
    for p=3 to 7 do
        i = power(10,p)
        {w, n} = WordLen(i)
        printf(1,"Word %8d is \"%s\", with %d letters.", {i, w, n})
        printf(1,"  Length of sentence so far:%d\n", TotalLength())
    end for
end procedure
main()
```

```txt

The lengths of the first 201 words are:

  1:   4  2  3  6  2  7  2  3  5  4  2  4  8  3  2  3  6  5  2  3  5  3  2  3  6
 26:   3  2  3  5  5  2  3  5  3  2  3  7  5  2  3  6  4  2  3  5  4  2  3  5  3
 51:   2  3  8  4  2  3  7  5  2  3 10  5  2  3 10  3  2  3  9  5  2  3  9  3  2
 76:   3 11  4  2  3 10  3  2  3 10  5  2  3  9  4  2  3 11  5  2  3 12  3  2  3
101:  11  5  2  3 12  3  2  3 11  5  2  3 11  3  2  3 13  5  2  3 12  4  2  3 11
126:   4  2  3  9  3  2  3 11  5  2  3 12  4  2  3 11  5  2  3 12  3  2  3 11  5
151:   2  3 11  5  2  3 13  4  2  3 12  3  2  3 11  5  2  3  8  3  2  3 10  4  2
176:   3 11  3  2  3 10  5  2  3 11  4  2  3 10  4  2  3 10  3  2  3 12  5  2  3
201:  11
Length of sentence so far:1204
Word     1000 is "in", with 2 letters.  Length of sentence so far:6280
Word    10000 is "in", with 2 letters.  Length of sentence so far:64692
Word   100000 is "one", with 3 letters.  Length of sentence so far:671578
Word  1000000 is "the", with 3 letters.  Length of sentence so far:7235383
Word 10000000 is "thousand,", with 8 letters.  Length of sentence so far:72079160

```



## REXX


```rexx
/*REXX pgm finds/shows the number of letters in the  Nth  word in a constructed sentence*/
@= 'Four is the number of letters in the first word of this sentence,'             /*···*/
                                                 /* [↑]   the start of a long sentence. */
parse arg N M                                    /*obtain optional argument from the CL.*/
if N='' | N="," then N= 201                      /*Not specified?  Then use the default.*/
if M='' | M="," then M=1000 10000 100000 1000000 /* "      "         "   "   "     "    */
@abcU= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'              /*define the uppercase Latin alphabet. */
!.=.;     #.=.;      q=1;       w=length(N)      /* [↓]  define some helpful low values.*/
call tell N
if N<0  then say y     ' is the length of word '         a          "  ["word(@, a)"]"
say                                              /* [↑]  N negative?  Just show 1 number*/
say 'length of sentence= '   length(@)           /*display the length of the @ sentence.*/

if M\==''  then do k=1  for words(M)  while M\=0 /*maybe handle counts  (if specified). */
                x=word(M, k)                     /*obtain the  Kth  word of the M list. */
                call tell  -x                    /*invoke subroutine (with negative arg)*/
                say
                say y     ' is the length of word '      x       "  ["word(@, x)"]"
                say 'length of sentence= '  length(@)    /*display length of @ sentence.*/
                end   /*k*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
wordLen: arg ?;         return length(?) - length( space( translate(?, , @abcU), 0) )
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell: parse arg z,,$;   idx=1;    a=abs(z);     group=25     /*show 25 numbers per line.*/
                                                 /*Q is the last number spelt by $SPELL#*/
        do j=1  for a                            /*traipse through all the numbers to N.*/
          do 2                                   /*perform loop twice  (well ··· maybe).*/
          y=wordLen( word(@, j) )                /*get the  Jth  word from the sentence.*/
          if y\==0  then leave                   /*Is the word spelt?   Then we're done.*/
          q=q + 1                                /*bump the on─going (moving) # counter.*/
          if #.q==.  then #.q=$spell#(q 'Q ORD') /*need to spell A as an ordinal number?*/
               _=wordLen( word(@, q) )           /*use the length of the ordinal number.*/
          if !._==.  then !._=$spell#(_ 'Q')     /*Not spelled?   Then go and spell it. */
          @=@  !._   'in the'    #.q","          /*append words to never─ending sentence*/
          end   /*2*/                            /* [↑]   Q ≡ Quiet      ORD ≡ ORDinal  */

        $=$ || right(y, 3)                       /* [↓]  append a justified # to a line.*/
        if j//group==0 & z>0  then do; say right(idx, w)'►'$;   idx=idx+group;   $=;   end
        end   /*j*/                              /* [↑]  show line if there's enough #s.*/

      if $\=='' & z>0 then say right(idx, w)'►'$ /*display if there are residual numbers*/
      return
```

The   '''$SPELL#.REX'''   routine can be found here   ───►   [[$SPELL.REX|$SPELL#.REX]]. 



```txt

  1►  4  2  3  6  2  7  2  3  5  4  2  4  8  3  2  3  6  5  2  3  5  3  2  3  6
 26►  3  2  3  5  5  2  3  5  3  2  3  7  5  2  3  6  4  2  3  5  4  2  3  5  3
 51►  2  3  8  4  2  3  7  5  2  3 10  5  2  3 10  3  2  3  9  5  2  3  9  3  2
 76►  3 11  4  2  3 10  3  2  3 10  5  2  3  9  4  2  3 11  5  2  3 12  3  2  3
101► 11  5  2  3 12  3  2  3 11  5  2  3 11  3  2  3 13  5  2  3 12  4  2  3 11
126►  4  2  3  9  3  2  3 11  5  2  3 12  4  2  3 11  5  2  3 12  3  2  3 11  5
151►  2  3 11  5  2  3 13  4  2  3 12  3  2  3 11  5  2  3  8  3  2  3 10  4  2
176►  3 11  3  2  3 10  5  2  3 11  4  2  3 10  4  2  3 10  3  2  3 12  5  2  3
201► 11

length of sentence=  1203

2  is the length of word  1000   [in]
length of sentence=  6279

2  is the length of word  10000   [in]
length of sentence=  64140

3  is the length of word  100000   [one]
length of sentence=  659474

3  is the length of word  1000000   [the]
length of sentence=  7113621

```



## zkl

Uses the nth function from [[Spelling_of_ordinal_numbers#zkl]]

```zkl
   // Built the sentence in little chucks but only save the last one
   // Save the word counts
fcn fourIsThe(text,numWords){
   const rmc="-,";
   seq:=(text - rmc).split().apply("len").copy();  // (4,2,3,6...)
   szs:=Data(numWords + 100,Int).howza(0).extend(seq);	// bytes
   cnt,lastWords := seq.len(),"";
   total:=seed.len() - 1;  // don't count trailing space

   foreach idx in ([1..]){
      sz:=szs[idx];
      a,b := nth(sz,False),nth(idx+1);  // "two","three hundred sixty-seventh"
      lastWords="%s in the %s, ".fmt(a,b);
      ws:=lastWords.counts(" ")[1];  // "five in the forty-ninth " --> 4
      cnt+=ws; total+=lastWords.len();
      lastWords.split().pump(szs.append,'-(rmc),"len");
      if(cnt>=numWords){
	 if(cnt>numWords){
	    z,n:=lastWords.len(),z-2;
	    do(cnt - numWords){ n=lastWords.rfind(" ",n) - 1; }
	    lastWords=lastWords[0,n+1]; total-=(z - n);
	 }
	 break;
      }
   }
   return(lastWords.strip(),szs,total);
}
fcn lastWord(sentence){ sentence[sentence.rfind(" ")+1,*] }
```


```zkl
var seed="Four is the number of letters in the first word of this sentence, ";
sentence,szs,total := fourIsThe(seed,201);
print("  1:");
foreach n,x in ([1..201].zip(szs)){
   print("%3d".fmt(x));
   if(0 == n%25) print("\n%3d:".fmt(n+1));
}
println("\nLength of above sentence: ",total);
```

```txt

  1:  4  2  3  6  2  7  2  3  5  4  2  4  8  3  2  3  6  5  2  3  5  3  2  3  6
 26:  3  2  3  5  5  2  3  5  3  2  3  7  5  2  3  6  4  2  3  5  4  2  3  5  3
 51:  2  3  8  4  2  3  7  5  2  3 10  5  2  3 10  3  2  3  9  5  2  3  9  3  2
 76:  3 11  4  2  3 10  3  2  3 10  5  2  3  9  4  2  3 11  5  2  3 12  3  2  3
101: 11  5  2  3 12  3  2  3 11  5  2  3 11  3  2  3 13  5  2  3 12  4  2  3 11
126:  4  2  3  9  3  2  3 11  5  2  3 12  4  2  3 11  5  2  3 12  3  2  3 11  5
151:  2  3 11  5  2  3 13  4  2  3 12  3  2  3 11  5  2  3  8  3  2  3 10  4  2
176:  3 11  3  2  3 10  5  2  3 11  4  2  3 10  4  2  3 10  3  2  3 12  5  2  3
201: 11
Length of above sentence: 1203

```


```zkl
n:=1000; do(5){
   sentence,x,total := fourIsThe(seed,n);
   word:=lastWord(sentence);
   println("%,d words: \"%s\" [%d]. Length=%,d"
	   .fmt(n,word,word.len(),total));
   n*=10;
}
```

```txt

1,000 words: "in" [2]. Length=6,247
10,000 words: "in" [2]. Length=64,095
100,000 words: "one" [3]. Length=659,453
1,000,000 words: "the" [3]. Length=7,140,558
10,000,000 words: "thousand" [8]. Length=71,250,727

```

