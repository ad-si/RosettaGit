+++
title = "Textonyms"
description = ""
date = 2018-08-10T23:20:12Z
aliases = []
[extra]
id = 18632
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_68",
  "clojure",
  "cpp",
  "d",
  "go",
  "haskell",
  "io",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "perl",
  "perl_6",
  "phix",
  "powershell",
  "python",
  "racket",
  "rexx",
  "ruby",
  "sidef",
  "tcl",
  "vbscript",
  "zkl",
]
+++

When entering text on a phone's digital pad it is possible that a particular combination of digits corresponds to more than one word. Such are called textonyms.

Assuming the digit keys are mapped to letters as follows:
     2 -> ABC
     3 -> DEF
     4 -> GHI
     5 -> JKL
     6 -> MNO
     7 -> PQRS
     8 -> TUV
     9 -> WXYZ  


## Task

Write a program that finds textonyms in a list of words such as   
[[Textonyms/wordlist]]   or   
[http://www.puzzlers.org/pub/wordlists/unixdict.txt unixdict.txt].

The task should produce a report:

 There are #{0} words in #{1} which can be represented by the digit key mapping.
 They require #{2} digit combinations to represent them.
 #{3} digit combinations represent Textonyms.

Where:
 #{0} is the number of words in the list which can be represented by the digit key mapping.
 #{1} is the URL of the wordlist being used.
 #{2} is the number of digit combinations required to represent the words in #{0}.
 #{3} is the number of #{2} which represent more than one word.

At your discretion show a couple of examples of your solution displaying Textonys. 

E.G.:

  2748424767 -> "Briticisms", "criticisms"


;Extra credit:
Use a word list and keypad mapping other than English.





## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}} Uses the Algol 68G specific "to upper" procedure.

```algol68
# find textonyms in a list of words #
# use the associative array in the Associate array/iteration task     #
PR read "aArray.a68" PR

# returns the number of occurances of ch in text #
PROC count = ( STRING text, CHAR ch )INT:
     BEGIN
         INT result := 0;
         FOR c FROM LWB text TO UPB text DO IF text[ c ] = ch THEN result +:= 1 FI OD;
         result
     END # count # ;

CHAR invalid char = "*";

# returns text with the characters replaced by their text digits      #
PROC to text = ( STRING text )STRING:
     BEGIN
         STRING result := text;
         FOR pos FROM LWB result TO UPB result DO
             CHAR c = to upper( result[ pos ] );
             IF   c = "A" OR c = "B" OR c = "C"            THEN result[ pos ] := "2"
             ELIF c = "D" OR c = "E" OR c = "F"            THEN result[ pos ] := "3"
             ELIF c = "G" OR c = "H" OR c = "I"            THEN result[ pos ] := "4"
             ELIF c = "J" OR c = "K" OR c = "L"            THEN result[ pos ] := "5"
             ELIF c = "M" OR c = "N" OR c = "O"            THEN result[ pos ] := "6"
             ELIF c = "P" OR c = "Q" OR c = "R" OR c = "S" THEN result[ pos ] := "7"
             ELIF c = "T" OR c = "U" OR c = "V"            THEN result[ pos ] := "8"
             ELIF c = "W" OR c = "X" OR c = "Y" OR c = "Z" THEN result[ pos ] := "9"
             ELSE # not a character that can be encoded #       result[ pos ] := invalid char
             FI
         OD;
         result
     END # to text # ;

# read the list of words and store in an associative array           #

CHAR separator = "/"; # character that will separate the textonyms   #

IF  FILE input file;
    STRING file name = "unixdict.txt";
    open( input file, file name, stand in channel ) /= 0
THEN
    # failed to open the file #
    print( (  "Unable to open """ + file name + """", newline ) )
ELSE
    # file opened OK #
    BOOL at eof := FALSE;
    # set the EOF handler for the file #
    on logical file end( input file, ( REF FILE f )BOOL:
                                     BEGIN
                                         # note that we reached EOF on the #
                                         # latest read #
                                         at eof := TRUE;
                                         # return TRUE so processing can continue #
                                         TRUE
                                     END
                       );
    REF AARRAY words   := INIT LOC AARRAY;
    INT word count     := 0;
    INT combinations   := 0;
    INT multiple count := 0;
    INT max length     := 0;
    WHILE STRING word;
          get( input file, ( word, newline ) );
          NOT at eof
    DO
        STRING text word = to text( word );
        IF count( text word, invalid char ) = 0 THEN
            # the word can be fully encoded #
            word count +:= 1;
            INT length := ( UPB word - LWB word ) + 1;
            IF length > max length THEN
                # this word is longer than the maximum length found so far #
                max length := length
            FI; 
            IF ( words // text word ) = "" THEN
                # first occurance of this encoding #
                combinations +:= 1;
                words // text word := word
            ELSE
                # this encoding has already been used #
                IF count( words // text word, separator ) = 0
                THEN
                    # this is the second time this encoding is used #
                    multiple count +:= 1
                FI;
                words // text word +:= separator + word
            FI
        FI
    OD;
    # close the file #
    close( input file );

    # find the maximum number of textonyms #

    INT max textonyms := 0;

    REF AAELEMENT e := FIRST words;
    WHILE e ISNT nil element DO
        INT textonyms := count( value OF e, separator );
        IF  textonyms > max textonyms 
        THEN
            max textonyms := textonyms
        FI;
        e := NEXT words
    OD;

    print( ( "There are ", whole( word count, 0 ), " words in ", file name, " which can be represented by the digit key mapping.", newline ) );
    print( ( "They require ", whole( combinations, 0 ), " digit combinations to represent them.", newline ) );
    print( ( whole( multiple count, 0 ), " combinations represent Textonyms.", newline ) );
 
    # show the textonyms with the maximum number #
    print( ( "The maximum number of textonyms for a particular digit key mapping is ", whole( max textonyms + 1, 0 ), " as follows:", newline ) ); 
    e := FIRST words;
    WHILE e ISNT nil element DO
        IF  INT textonyms := count( value OF e, separator );
            textonyms = max textonyms 
        THEN
            print( ( "    ", key OF e, " encodes ", value OF e, newline ) )
        FI;
        e := NEXT words
    OD;

    # show the textonyms with the maximum length #
    print( ( "The longest words are ", whole( max length, 0 ), " chracters long", newline ) );
    print( ( "Encodings with this length are:", newline ) );
    e := FIRST words;
    WHILE e ISNT nil element DO
        IF max length = ( UPB key OF e - LWB key OF e ) + 1
        THEN
            print( ( "    ", key OF e, " encodes ", value OF e, newline ) )
        FI;
        e := NEXT words
    OD;

FI

```

```txt

There are 24978 words in unixdict.txt which can be represented by the digit key mapping.
They require 22903 digit combinations to represent them.
1473 combinations represent Textonyms.
The maximum number of textonyms for a particular digit key mapping is 9 as follows:
    269 encodes amy/any/bmw/bow/box/boy/cow/cox/coy
    729 encodes paw/pax/pay/paz/raw/ray/saw/sax/say
The longest words are 22 chracters long
Encodings with this length are:
    3532876362374256472749 encodes electroencephalography

```



## C++


```cpp

#include <fstream>
#include <iostream>
#include <unordered_map>
#include <vector>

struct Textonym_Checker {
private:
    int total;
    int elements;
    int textonyms;
    int max_found;
    std::vector<std::string> max_strings;
    std::unordered_map<std::string, std::vector<std::string>> values;

    int get_mapping(std::string &result, const std::string &input)
    {
        static std::unordered_map<char, char> mapping = {
            {'A', '2'}, {'B', '2'}, {'C', '2'},
            {'D', '3'}, {'E', '3'}, {'F', '3'},
            {'G', '4'}, {'H', '4'}, {'I', '4'},
            {'J', '5'}, {'K', '5'}, {'L', '5'},
            {'M', '6'}, {'N', '6'}, {'O', '6'},
            {'P', '7'}, {'Q', '7'}, {'R', '7'}, {'S', '7'},
            {'T', '8'}, {'U', '8'}, {'V', '8'},
            {'W', '9'}, {'X', '9'}, {'Y', '9'}, {'Z', '9'}
        };

        result = input;
        for (char &c : result) {
            if (!isalnum(c)) return 0;
            if (isalpha(c)) c = mapping[toupper(c)];
        }

        return 1;
    }

public:
    Textonym_Checker(void) : total(0), elements(0), textonyms(0), max_found(0) { }

    ~Textonym_Checker(void) { }

    void add(const std::string &str) {
        std::string mapping;
        total += 1;

        if (!get_mapping(mapping, str)) return;

        const int num_strings = values[mapping].size();

        textonyms += num_strings == 1 ? 1 : 0;
        elements  += 1;

        if (num_strings > max_found) {
            max_strings.clear();
            max_strings.push_back(mapping);
            max_found = num_strings;
        }
        else if (num_strings == max_found) {
            max_strings.push_back(mapping);
        }

        values[mapping].push_back(str);
    }

    void results(const std::string &filename) {
        std::cout << "Read " << total << " words from " << filename << "\n\n";

        std::cout << "There are " << elements << " words in " << filename;
        std::cout << " which can be represented by the digit key mapping.\n";
        std::cout << "They require " << values.size() <<
                     " digit combinations to represent them.\n";
        std::cout << textonyms << " digit combinations represent Textonyms.\n\n";
        std::cout << "The numbers mapping to the most words map to ";
        std::cout << max_found + 1 << " words each:\n";

        for (auto it1 = max_strings.begin(); it1 != max_strings.end(); ++it1) {
            std::cout << '\t' << *it1 << " maps to: ";
            for (auto it2 = values[*it1].begin(); it2 != values[*it1].end(); ++it2) {
                std::cout << *it2 << " ";
            }
            std::cout << "\n";
        }
        std::cout << '\n';
    }

    void match(const std::string &str) {
        auto match = values.find(str);

        if (match == values.end()) {
            std::cout << "Key '" << str << "' not found\n";
        }
        else {
            std::cout << "Key '" << str << "' matches: ";
            for (auto it = values[str].begin(); it != values[str].end(); ++it)
                std::cout << *it << " ";
            std::cout << '\n';
        }
    }
};

int main(void)
{
    std::string filename = "unixdict.txt";
    std::ifstream input(filename);
    Textonym_Checker tc;

    if (input.is_open()) {
        std::string line;
        while (getline(input, line))
            tc.add(line);
    }

    input.close();

    tc.results(filename);
    tc.match("001");
    tc.match("228");
    tc.match("27484247");
    tc.match("7244967473642");
}

```

```txt

Read 25104 words from unixdict.txt

There are 24988 words in unixdict.txt which can be represented by the digit key mapping.
They require 22905 digit combinations to represent them.
1477 digit combinations represent Textonyms.

The numbers mapping to the most words map to 9 words each:
	269 maps to: amy any bmw bow box boy cow cox coy 
	729 maps to: paw pax pay paz raw ray saw sax say 

Key '001' not found
Key '228' matches: aau act bat cat 
Key '27484247' not found
Key '7244967473642' matches: schizophrenia schizophrenic 

```



## Clojure

The [[Textonyms#Tcl|Tcl example]] counts all the words which share a digit sequence with another word. Like the other 
examples, this considers a textonym to be a digit sequence which maps to more than one word.

```Clojure

(def table
  {\a 2 \b 2 \c 2       \A 2 \B 2 \C 2
   \d 3 \e 3 \f 3       \D 3 \E 3 \F 3
   \g 4 \h 4 \i 4       \G 4 \H 4 \I 4
   \j 5 \k 5 \l 5       \J 5 \K 5 \L 5
   \m 6 \n 6 \o 6       \M 6 \N 6 \O 6
   \p 7 \q 7 \r 7 \s 7  \P 7 \Q 7 \R 7 \S 7
   \t 8 \u 8 \v 8       \T 8 \U 8 \V 8
   \w 9 \x 9 \y 9 \z 9  \W 9 \X 9 \Y 9 \Z 9})

(def words-url "http://www.puzzlers.org/pub/wordlists/unixdict.txt")

(def words (-> words-url slurp clojure.string/split-lines))

(def digits (partial map table))

(let [textable  (filter #(every? table %) words) ;; words with letters only
      mapping   (group-by digits textable)       ;; map of digits to words
      textonyms (filter #(< 1 (count (val %))) mapping)] ;; textonyms only
  (print 
   (str "There are " (count textable) " words in " \' words-url \'
        " which can be represented by the digit key mapping. They require "
        (count mapping) " digit combinations to represent them. "
        (count textonyms) " digit combinations represent Textonyms.")))

```

```txt

There are 24978 words in 'http://www.puzzlers.org/pub/wordlists/unixdict.txt' which can be represented by the digit key mapping. They require 22903 digit combinations to represent them. 1473 digit combinations represent Textonyms.

```



## D

```d
void main() {
    import std.stdio, std.string, std.range, std.algorithm, std.ascii;

    immutable src = "unixdict.txt";
    const words = src.File.byLineCopy.map!strip.filter!(w => w.all!isAlpha).array;

    immutable table = makeTrans("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
                                "2223334445556667777888999922233344455566677778889999");

    string[][string] dials;
    foreach (const word; words)
        dials[word.translate(table)] ~= word;

    auto textonyms = dials.byPair.filter!(p => p[1].length > 1).array;

    writefln("There are %d words in %s which can be represented by the digit key mapping.", words.length, src);
    writefln("They require %d digit combinations to represent them.", dials.length);
    writefln("%d digit combinations represent Textonyms.", textonyms.length);

    "\nTop 5 in ambiguity:".writeln;
    foreach (p; textonyms.schwartzSort!(p => -p[1].length).take(5))
        writefln("    %s => %-(%s %)", p[]);

    "\nTop 5 in length:".writeln;
    foreach (p; textonyms.schwartzSort!(p => -p[0].length).take(5))
        writefln("    %s => %-(%s %)", p[]);
}
```

```txt
There are 24978 words in unixdict.txt which can be represented by the digit key mapping.
They require 22903 digit combinations to represent them.
1473 digit combinations represent Textonyms.

Top 5 in ambiguity:
    729 => paw pax pay paz raw ray saw sax say
    269 => amy any bmw bow box boy cow cox coy
    2273 => acre bard bare base cape card care case
    726 => pam pan ram ran sam san sao scm
    426 => gam gao ham han ian ibm ibn

Top 5 in length:
    25287876746242 => claustrophobia claustrophobic
    7244967473642 => schizophrenia schizophrenic
    666628676342 => onomatopoeia onomatopoeic
    49376746242 => hydrophobia hydrophobic
    6388537663 => mettlesome nettlesome
```



## Go

Uses a local file and shows its name rather than re-fetching a URL each run and printing that URL.

Like the [[Textonyms#Python|Phython example]],
the examples shown are the numbers that map to the most words.

```go
package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"strings"
	"unicode"
)

func main() {
	log.SetFlags(0)
	log.SetPrefix("textonyms: ")

	wordlist := flag.String("wordlist", "wordlist", "file containing the list of words to check")
	flag.Parse()
	if flag.NArg() != 0 {
		flag.Usage()
		os.Exit(2)
	}

	t := NewTextonym(phoneMap)
	_, err := ReadFromFile(t, *wordlist)
	if err != nil {
		log.Fatal(err)
	}
	t.Report(os.Stdout, *wordlist)
}

// phoneMap is the digit to letter mapping of a typical phone.
var phoneMap = map[byte][]rune{
	'2': []rune("ABC"),
	'3': []rune("DEF"),
	'4': []rune("GHI"),
	'5': []rune("JKL"),
	'6': []rune("MNO"),
	'7': []rune("PQRS"),
	'8': []rune("TUV"),
	'9': []rune("WXYZ"),
}

// ReadFromFile is a generic convience function that allows the use of a
// filename with an io.ReaderFrom and handles errors related to open and
// closing the file.
func ReadFromFile(r io.ReaderFrom, filename string) (int64, error) {
	f, err := os.Open(filename)
	if err != nil {
		return 0, err
	}
	n, err := r.ReadFrom(f)
	if cerr := f.Close(); err == nil && cerr != nil {
		err = cerr
	}
	return n, err
}

type Textonym struct {
	numberMap map[string][]string // map numeric string into words
	letterMap map[rune]byte       // map letter to digit
	count     int                 // total number of words in numberMap
	textonyms int                 // number of numeric strings with >1 words
}

func NewTextonym(dm map[byte][]rune) *Textonym {
	lm := make(map[rune]byte, 26)
	for d, ll := range dm {
		for _, l := range ll {
			lm[l] = d
		}
	}
	return &Textonym{letterMap: lm}
}

func (t *Textonym) ReadFrom(r io.Reader) (n int64, err error) {
	t.numberMap = make(map[string][]string)
	buf := make([]byte, 0, 32)
	sc := bufio.NewScanner(r)
	sc.Split(bufio.ScanWords)
scan:
	for sc.Scan() {
		buf = buf[:0]
		word := sc.Text()

		// XXX we only bother approximating the number of bytes
		// consumed. This isn't used in the calling code and was
		// only included to match the io.ReaderFrom interface.
		n += int64(len(word)) + 1

		for _, r := range word {
			d, ok := t.letterMap[unicode.ToUpper(r)]
			if !ok {
				//log.Printf("ignoring %q\n", word)
				continue scan
			}
			buf = append(buf, d)
		}
		//log.Printf("scanned %q\n", word)
		num := string(buf)
		t.numberMap[num] = append(t.numberMap[num], word)
		t.count++
		if len(t.numberMap[num]) == 2 {
			t.textonyms++
		}
		//log.Printf("%q → %v\t→ %v\n", word, num, t.numberMap[num])
	}
	return n, sc.Err()
}

func (t *Textonym) Most() (most int, subset map[string][]string) {
	for k, v := range t.numberMap {
		switch {
		case len(v) > most:
			subset = make(map[string][]string)
			most = len(v)
			fallthrough
		case len(v) == most:
			subset[k] = v
		}
	}
	return most, subset
}

func (t *Textonym) Report(w io.Writer, name string) {
	// Could be fancy and use text/template package but fmt is sufficient
	fmt.Fprintf(w, `
There are %v words in %q which can be represented by the digit key mapping.
They require %v digit combinations to represent them.
%v digit combinations represent Textonyms.
`,
		t.count, name, len(t.numberMap), t.textonyms)

	n, sub := t.Most()
	fmt.Fprintln(w, "\nThe numbers mapping to the most words map to",
		n, "words each:")
	for k, v := range sub {
		fmt.Fprintln(w, "\t", k, "maps to:", strings.Join(v, ", "))
	}
}
```

```txt

There are 13085 words in "wordlist" which can be represented by the digit key mapping.
They require 11932 digit combinations to represent them.
661 digit combinations represent Textonyms.

The numbers mapping to the most words map to 15 words each:
	 27 maps to: AP, AQ, AR, AS, Ar, As, BP, BR, BS, Br, CP, CQ, CR, Cr, Cs

```

```txt

There are 24978 words in "unixdict.txt" which can be represented by the digit key mapping.
They require 22903 digit combinations to represent them.
1473 digit combinations represent Textonyms.

The numbers mapping to the most words map to 9 words each:
	 269 maps to: amy, any, bmw, bow, box, boy, cow, cox, coy
	 729 maps to: paw, pax, pay, paz, raw, ray, saw, sax, say

```



## Haskell


```haskell
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Char (toUpper)
import Data.List (sortBy, groupBy)
import Data.Function (on)

toKey :: Char -> Maybe Char
toKey ch 
    | ch < 'A' = Nothing
    | ch < 'D' = Just '2'
    | ch < 'G' = Just '3'
    | ch < 'J' = Just '4'
    | ch < 'M' = Just '5'
    | ch < 'P' = Just '6'
    | ch < 'T' = Just '7'
    | ch < 'W' = Just '8'
    | ch <= 'Z' = Just '9'
    | otherwise = Nothing

toKeyString :: String -> Maybe String
toKeyString st = 
    let mch = map (toKey.toUpper) st 
    in if any isNothing mch then Nothing
                            else Just $ map (fromMaybe '!') mch

showTextonym :: [(String,String)] -> IO ()
showTextonym ts = do
    let keyCode = fst $ head ts
    putStrLn $ keyCode ++  " => " ++ concat [w ++ " " | (_,w) <- ts ]

main :: IO()
main = do
    let src = "unixdict.txt" 
    contents <- readFile src

    let wordList = lines contents
        keyedList = [(key, word) | (Just key, word) <- filter (isJust.fst) $ zip (map toKeyString wordList) wordList]
        groupedList = groupBy ((==) `on` fst) $  sortBy (compare `on` fst) keyedList
        textonymList = filter ((>1) . length) groupedList

    putStrLn $ "There are " ++ show (length keyedList) ++ " words in " ++ src ++ " which can be represented by the digit key mapping."
    putStrLn $ "They require " ++ show (length groupedList) ++ " digit combinations to represent them."
    putStrLn $ show (length textonymList) ++ " digit combinations represent Textonyms."
    putStrLn ""
    putStrLn "Top 5 in ambiguity:"
    mapM_ showTextonym $ take 5 $ sortBy (flip compare `on` length) textonymList
    putStrLn ""
    putStrLn "Top 5 in length:"
    mapM_ showTextonym $ take 5 $ sortBy (flip compare `on` (length.fst.head)) textonymList 
```

<pre style="font-size:80%">There are 24978 words in unixdict.txt which can be represented by the digit key mapping.
They require 22903 digit combinations to represent them.
1473 digit combinations represent Textonyms.

Top 5 in ambiguity:
269 => amy any bmw bow box boy cow cox coy 
729 => paw pax pay paz raw ray saw sax say 
2273 => acre bard bare base cape card care case 
726 => pam pan ram ran sam san sao scm 
426 => gam gao ham han ian ibm ibn 

Top 5 in length:
25287876746242 => claustrophobia claustrophobic 
7244967473642 => schizophrenia schizophrenic 
666628676342 => onomatopoeia onomatopoeic 
49376746242 => hydrophobia hydrophobic 
2668368466 => contention convention 
```



## Io



```Io
main := method(
    setupLetterToDigitMapping

    file := File clone openForReading("./unixdict.txt")
    words := file readLines
    file close

    wordCount := 0
    textonymCount := 0
    dict := Map clone
    words foreach(word,
        (key := word asPhoneDigits) ifNonNil(
            wordCount = wordCount+1
            value := dict atIfAbsentPut(key,list())
            value append(word)
            if(value size == 2,textonymCount = textonymCount+1)
        )
    )   
    write("There are ",wordCount," words in ",file name)
    writeln(" which can be represented by the digit key mapping.")
    writeln("They require ",dict size," digit combinations to represent them.")
    writeln(textonymCount," digit combinations represent Textonyms.")

    samplers := list(maxAmbiquitySampler, noMatchingCharsSampler)
    dict foreach(key,value,
        if(value size == 1, continue)
        samplers foreach(sampler,sampler examine(key,value))
    )
    samplers foreach(sampler,sampler report)
)

setupLetterToDigitMapping := method(
    fromChars := Sequence clone
    toChars := Sequence clone
    list(
        list("ABC", "2"), list("DEF", "3"), list("GHI", "4"),
        list("JKL", "5"), list("MNO", "6"), list("PQRS","7"),
        list("TUV", "8"), list("WXYZ","9")
    ) foreach( map,
        fromChars appendSeq(map at(0), map at(0) asLowercase)
        toChars alignLeftInPlace(fromChars size, map at(1))
    )

    Sequence asPhoneDigits := block(
        str := call target asMutable translate(fromChars,toChars)
        if( str contains(0), nil, str )
    ) setIsActivatable(true)
)   

maxAmbiquitySampler := Object clone do(
    max := list()
    samples := list()
    examine := method(key,textonyms,
        i := key size - 1
        if(i > max size - 1,
            max setSize(i+1)
            samples setSize(i+1)
        )
        nw := textonyms size
        nwmax := max at(i)
        if( nwmax isNil or nw > nwmax,
            max atPut(i,nw)
            samples atPut(i,list(key,textonyms))
        )
    )
    report := method(
        writeln("\nExamples of maximum ambiquity for each word length:")
        samples foreach(sample,
            sample ifNonNil(
                writeln("    ",sample at(0)," -> ",sample at(1) join(" "))
            )
        )
    )
)

noMatchingCharsSampler := Object clone do(
    samples := list()
    examine := method(key,textonyms,
        for(i,0,textonyms size - 2 ,
            for(j,i+1,textonyms size - 1,
                if( _noMatchingChars(textonyms at(i), textonyms at(j)),
                    samples append(list(textonyms at(i),textonyms at(j)))
                )
            )
        )
    )
    _noMatchingChars := method(t1,t2,
        t1 foreach(i,ich,
            if(ich == t2 at(i), return false)
        )
        true
    )       
    report := method(
        write("\nThere are ",samples size," textonym pairs which ")
        writeln("differ at each character position.")
        if(samples size > 10, writeln("The ten largest are:"))
        samples sortInPlace(at(0) size negate)
        if(samples size > 10,samples slice(0,10),samples) foreach(sample,
            writeln("    ",sample join(" ")," -> ",sample at(0) asPhoneDigits)
        )
    )
)

main
```

```txt
There are 24978 words in unixdict.txt which can be represented by the digit key mapping.
They require 22903 digit combinations to represent them.
1473 digit combinations represent Textonyms.

Examples of maximum ambiquity for each word length:
    7 -> p q r s
    46 -> gm go ho in io
    269 -> amy any bmw bow box boy cow cox coy
    2273 -> acre bard bare base cape card care case
    42779 -> garry gassy happy harpy harry
    723353 -> paddle raffle saddle
    2667678 -> comport compost consort
    38465649 -> ethology etiology
    468376377 -> governess inverness
    6388537663 -> mettlesome nettlesome
    49376746242 -> hydrophobia hydrophobic
    666628676342 -> onomatopoeia onomatopoeic
    7244967473642 -> schizophrenia schizophrenic
    25287876746242 -> claustrophobia claustrophobic

There are 275 textonym pairs which differ at each character position.
The ten largest are:
    pistol shrunk -> 747865
    hotbed invade -> 468233
    aback cabal -> 22225
    about bantu -> 22688
    adams bebop -> 23267
    rival shuck -> 74825
    astor crump -> 27867
    knack local -> 56225
    rice shad -> 7423
    ammo coon -> 2666
```



## J



```J
require'regex strings web/gethttp'

strip=:dyad define
  (('(?s)',x);'') rxrplc y
)

fetch=:monad define
  txt=. '.*
```txt
' strip '
```
.*' strip gethttp y
  cutopen tolower txt-.' '
)

keys=:noun define
 2 abc
 3 def
 4 ghi
 5 jkl
 6 mno
 7 pqrs
 8 tuv
 9 wxyz
)

reporttext=:noun define
There are #{0} words in #{1} which can be represented by the digit key mapping.
They require #{2} digit combinations to represent them.
#{3} digit combinations represent Textonyms.
)

report=:dyad define
  x rplc (":&.>y),.~('#{',":,'}'"_)&.>i.#y
)

textonymrpt=:dyad define
  'digits letters'=. |:>;,&.>,&.>/&.>/"1 <;._1;._2 x
  valid=. (#~ */@e.&letters&>) fetch y NB. ignore illegals
  reps=. {&digits@(letters&i.)&.> valid NB. reps is digit seq
  reporttext report (#valid);y;(#~.reps);+/(1<#)/.~reps
)
```


Required example:


```J
   keys textonymrpt 'http://rosettacode.org/wiki/Textonyms/wordlist'
There are 13085 words in http://rosettacode.org/wiki/Textonyms/wordlist which can be represented by the digit key mapping.
They require 11932 digit combinations to represent them.
661 digit combinations represent Textonyms.
```


In this example, the intermediate results in textonymrpt would look like this (just looking at the first 5 elements of the really big values:


```J
   digits
22233344455566677778889999
   letters
abcdefghijklmnopqrstuvwxyz
   5{.valid
┌─┬──┬───┬───┬──┐
│a│aa│aaa│aam│ab│
└─┴──┴───┴───┴──┘
   5{.reps
┌─┬──┬───┬───┬──┐
│2│22│222│226│22│
└─┴──┴───┴───┴──┘
```


Here's another example:


```J
   keys textonymrpt 'http://www.puzzlers.org/pub/wordlists/unixdict.txt'
There are 24978 words in http://www.puzzlers.org/pub/wordlists/unixdict.txt which can be represnted by the digit key mapping.
They require 22903 digit combinations to represent them.
1473 digit combinations represent Textonyms.
```



## Java

```java

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Vector;

public class RTextonyms {

  private static final Map<Character, Character> mapping;
  private int total, elements, textonyms, max_found;
  private String filename, mappingResult;
  private Vector<String> max_strings;
  private Map<String, Vector<String>> values;

  static {
    mapping = new HashMap<Character, Character>();
    mapping.put('A', '2'); mapping.put('B', '2'); mapping.put('C', '2');
    mapping.put('D', '3'); mapping.put('E', '3'); mapping.put('F', '3');
    mapping.put('G', '4'); mapping.put('H', '4'); mapping.put('I', '4');
    mapping.put('J', '5'); mapping.put('K', '5'); mapping.put('L', '5');
    mapping.put('M', '6'); mapping.put('N', '6'); mapping.put('O', '6');
    mapping.put('P', '7'); mapping.put('Q', '7'); mapping.put('R', '7'); mapping.put('S', '7');
    mapping.put('T', '8'); mapping.put('U', '8'); mapping.put('V', '8');
    mapping.put('W', '9'); mapping.put('X', '9'); mapping.put('Y', '9'); mapping.put('Z', '9');
  }

  public RTextonyms(String filename) {

    this.filename = filename;
    this.total = this.elements = this.textonyms = this.max_found = 0;
    this.values = new HashMap<String, Vector<String>>();
    this.max_strings = new Vector<String>();

    return;
  }

  public void add(String line) {

    String mapping = "";
    total++;
    if (!get_mapping(line)) {
      return;
    }
    mapping = mappingResult;

    if (values.get(mapping) == null) {
      values.put(mapping, new Vector<String>());
    }

    int num_strings;
    num_strings = values.get(mapping).size();
    textonyms += num_strings == 1 ? 1 : 0;
    elements++;

    if (num_strings > max_found) {
      max_strings.clear();
      max_strings.add(mapping);
      max_found = num_strings;
    }
    else if (num_strings == max_found) {
      max_strings.add(mapping);
    }

    values.get(mapping).add(line);

    return;
  }

  public void results() {

    System.out.printf("Read %,d words from %s%n%n", total, filename);
    System.out.printf("There are %,d words in %s which can be represented by the digit key mapping.%n", elements,
        filename);
    System.out.printf("They require %,d digit combinations to represent them.%n", values.size());
    System.out.printf("%,d digit combinations represent Textonyms.%n", textonyms);
    System.out.printf("The numbers mapping to the most words map to %,d words each:%n", max_found + 1);
    for (String key : max_strings) {
      System.out.printf("%16s maps to: %s%n", key, values.get(key).toString());
    }
    System.out.println();

    return;
  }

  public void match(String key) {

    Vector<String> match;
    match = values.get(key);
    if (match == null) {
      System.out.printf("Key %s not found%n", key);
    }
    else {
      System.out.printf("Key %s matches: %s%n", key, match.toString());
    }

    return;
  }

  private boolean get_mapping(String line) {

    mappingResult = line;
    StringBuilder mappingBuilder = new StringBuilder();
    for (char cc : line.toCharArray()) {
      if (Character.isAlphabetic(cc)) {
        mappingBuilder.append(mapping.get(Character.toUpperCase(cc)));
      }
      else if (Character.isDigit(cc)) {
        mappingBuilder.append(cc);
      }
      else {
        return false;
      }
    }
    mappingResult = mappingBuilder.toString();

    return true;
  }

  public static void main(String[] args) {

    String filename;
    if (args.length > 0) {
      filename = args[0];
    }
    else {
      filename = "./unixdict.txt";
    }
    RTextonyms tc;
    tc = new RTextonyms(filename);
    Path fp = Paths.get(filename);
    try (Scanner fs = new Scanner(fp, StandardCharsets.UTF_8.name())) {
      while (fs.hasNextLine()) {
        tc.add(fs.nextLine());
      }
    }
    catch (IOException ex) {
      ex.printStackTrace();
    }

    List<String> numbers = Arrays.asList(
        "001", "228", "27484247", "7244967473642",
        "."
        );

    tc.results();
    for (String number : numbers) {
      if (number.equals(".")) {
        System.out.println();
      }
      else {
        tc.match(number);
      }
    }

    return;
  }
}

```

```txt

Read 25,104 words from ./unixdict.txt

There are 24,988 words in ./unixdict.txt which can be represented by the digit key mapping.
They require 22,905 digit combinations to represent them.
1,477 digit combinations represent Textonyms.
The numbers mapping to the most words map to 9 words each:
             269 maps to: [amy, any, bmw, bow, box, boy, cow, cox, coy]
             729 maps to: [paw, pax, pay, paz, raw, ray, saw, sax, say]

Key 001 not found
Key 228 matches: [aau, act, bat, cat]
Key 27484247 not found
Key 7244967473642 matches: [schizophrenia, schizophrenic]

```



## jq

The following requires a version of jq with "gsub".

```jq
def textonym_value:
    gsub("a|b|c|A|B|C"; "2")
  | gsub("d|e|f|D|E|F"; "3")
  | gsub("g|h|i|G|H|I"; "4")
  | gsub("j|k|l|J|K|L"; "5")
  | gsub("m|n|o|M|N|O"; "6")
  | gsub("p|q|r|s|P|Q|R|S"; "7")
  | gsub("t|u|v|T|U|V"; "8")
  | gsub("w|x|y|z|W|X|Y|Z"; "9");

def explore:
  # given an array (or hash), find the maximum length of the items (or values):
  def max_length: [.[] | length] | max;

  # The length of the longest textonym in the dictionary of numericString => array:
  def longest:
    [to_entries[] | select(.value|length > 1) | .key | length] | max;

  # pretty-print a key-value pair:
  def pp: "\(.key) maps to: \(.value|tostring)";
  
  split("\n")
  | map(select(test("^[a-zA-Z]+$")))  # select the strictly alphabetic strings
  | length as $nwords
  | reduce .[] as $line
    ( {};
      ($line | textonym_value) as $key
      | .[$key] += [$line] )
  | max_length as $max_length
  | longest    as $longest
  | "There are \($nwords) words in the Textonyms/wordlist word list that can be represented by the digit-key mapping.",
    "They require \(length) digit combinations to represent them.",
    "\( [.[] | select(length>1) ] | length ) digit combinations represent Textonyms.",
    "The numbers mapping to the most words map to \($max_length) words:",
     (to_entries[] | select((.value|length) == $max_length) | pp ),
    "The longest Textonyms in the word list have length \($longest):",
     (to_entries[] | select((.key|length) == $longest and (.value|length > 1)) | pp)
;

explore
```

```sh
$ jq -R -r -c -s -f textonyms.jq textonyms.txt
There are 13085 words in the Textonyms/wordlist word list that can be represented by the digit-key mapping.
They require 11932 digit combinations to represent them.
661 digit combinations represent Textonyms.
The numbers mapping to the most words map to 15 words:
27 maps to: ["AP","AQ","AR","AS","Ar","As","BP","BR","BS","Br","CP","CQ","CR","Cr","Cs"]
The longest Textonyms in the word list have length 11:
26456746242 maps to: ["Anglophobia","Anglophobic"]
24636272673 maps to: ["CinemaScope","Cinemascope"]
```



## Julia

This solution uses an <tt>aspell</tt> dictionary on the local machine as its word source.  The character to number mapping is done via regular expressions and Julia's <tt>replace</tt> function.  Because this list contains accented characters, the matching expressions were expanded to include such characters.  Words are case sensitive, but the mapping is not, so for example both "Homer" and "homer" are included in the tabulation, each coded as "46637".

'''Function'''

```Julia

const tcode = (Regex=>Char)[r"A|B|C|Ä|Å|Á|Â|Ç" => '2',
                            r"D|E|F|È|Ê|É" => '3',
                            r"G|H|I|Í" => '4',
                            r"J|K|L" => '5',
                            r"M|N|O|Ó|Ö|Ô|Ñ" => '6',
                            r"P|Q|R|S" => '7',
                            r"T|U|V|Û|Ü" => '8',
                            r"W|X|Y|Z" => '9']

function tpad(str::IOStream)
    tnym = (String=>Array{String,1})[]
    for w in eachline(str)
        w = chomp(w)
        t = uppercase(w)
        for (k,v) in tcode
            t = replace(t, k, v)
        end
        t = replace(t, r"\D", '1')
        tnym[t] = [get(tnym, t, String[]), w]
    end
    return tnym
end

```


'''Main'''

```Julia

dname = "/usr/share/dict/american-english"
DF = open(dname, "r")
tnym = tpad(DF)
close(DF)

println("The character to digit mapping is done according to")
println("these regular expressions (following uppercase conversion):")
for k in sort(collect(keys(tcode)), by=x->tcode[x])
    println("    ", tcode[k], " -> ", k)
end
println("Unmatched non-digit characters are mapped to 1")

println()    
print("There are ", sum(map(x->length(x), values(tnym))))
println(" words in ", dname)
println("  which can be represented by the digit key mapping.")
print("They require ", length(keys(tnym)))
println(" digit combinations to represent them.")
print(sum(map(x->length(x)>1, values(tnym))))
println(" digit combinations represent Textonyms.")

println()
println("The degeneracies of telephone key encodings are:")
println("  Words Encoded   Number of codes")
dgen = zeros(maximum(map(x->length(x), values(tnym))))
for v in values(tnym)
    dgen[length(v)] += 1
end
for (i, d) in enumerate(dgen)
    println(@sprintf "%10d  %15d" i d)
end

println()
dgen = length(dgen) - 2
println("Codes mapping to ", dgen, " or more words:")
for (k, v) in tnym
    dgen <= length(v) || continue
    println(@sprintf "%7s (%2d) %s" k length(v) join(v, ", "))
end

```


```txt

The character to digit mapping is done according to
these regular expressions (following uppercase conversion):
    2 -> r"A|B|C|Ä|Å|Á|Â|Ç"
    3 -> r"D|E|F|È|Ê|É"
    4 -> r"G|H|I|Í"
    5 -> r"J|K|L"
    6 -> r"M|N|O|Ó|Ö|Ô|Ñ"
    7 -> r"P|Q|R|S"
    8 -> r"T|U|V|Û|Ü"
    9 -> r"W|X|Y|Z"
Unmatched non-digit characters are mapped to 1

There are 99171 words in /usr/share/dict/american-english
  which can be represented by the digit key mapping.
They require 89353 digit combinations to represent them.
6860 digit combinations represent Textonyms.

The degeneracies of telephone key encodings are:
  Words Encoded   Number of codes
         1            82493
         2             5088
         3             1104
         4              383
         5              159
         6               72
         7               24
         8               16
         9                8
        10                4
        11                1
        12                1

Codes mapping to 10 or more words:
    269 (11) Amy, BMW, Cox, Coy, any, bow, box, boy, cow, cox, coy
  22737 (12) acres, bards, barer, bares, barfs, baser, bases, caper, capes, cards, cares, cases
   2273 (10) Case, acre, bard, bare, barf, base, cape, card, care, case
  46637 (10) Homer, goner, goods, goofs, homer, homes, hones, hoods, hoofs, inner
   7217 (10) PA's, PC's, Pa's, Pb's, Ra's, Rb's, SC's, Sb's, Sc's, pa's
   4317 (10) GE's, Gd's, Ge's, HF's, He's, Hf's, ID's, he's, id's, if's

```



## Kotlin


```scala
// version 1.1.4-3

import java.io.File

val wordList = "unixdict.txt"
val url = "http://www.puzzlers.org/pub/wordlists/unixdict.txt"

const val DIGITS = "22233344455566677778889999"

val map = mutableMapOf<String, MutableList<String>>()

fun processList() {
    var countValid = 0
    val f = File(wordList)
    val sb = StringBuilder()

    f.forEachLine { word->
        var valid = true
        sb.setLength(0)
        for (c in word.toLowerCase()) {
            if (c !in 'a'..'z') {
                valid = false
                break
            } 
            sb.append(DIGITS[c - 'a'])
        }
        if (valid) {
            countValid++
            val key = sb.toString()
            if (map.containsKey(key)) {
                map[key]!!.add(word)
            }
            else {
                map.put(key, mutableListOf(word))
            }
        }    
    }
    var textonyms = map.filter { it.value.size > 1 }.toList() 
    val report = "There are $countValid words in '$url' " +
                 "which can be represented by the digit key mapping.\n" +
                 "They require ${map.size} digit combinations to represent them.\n" +
                 "${textonyms.size} digit combinations represent Textonyms.\n"
    println(report)

    val longest = textonyms.sortedByDescending { it.first.length }
    val ambiguous = longest.sortedByDescending { it.second.size }

    println("Top 8 in ambiguity:\n")
    println("Count   Textonym  Words")
    println("
### ===  ========  ==
")
    var fmt = "%4d    %-8s  %s"
    for (a in ambiguous.take(8)) println(fmt.format(a.second.size, a.first, a.second))

    fmt = fmt.replace("8", "14")
    println("\nTop 6 in length:\n")
    println("Length  Textonym        Words")
    println("
### ===  ==============  ==
")
    for (l in longest.take(6)) println(fmt.format(l.first.length, l.first, l.second))           
}

fun main(args: Array<String>) {
    processList()
}
```


```txt

There are 24978 words in 'http://www.puzzlers.org/pub/wordlists/unixdict.txt' which can be represented by the digit key mapping.
They require 22903 digit combinations to represent them.
1473 digit combinations represent Textonyms.

Top 8 in ambiguity:

Count   Textonym  Words

### ===  ========  ==

   9    269       [amy, any, bmw, bow, box, boy, cow, cox, coy]
   9    729       [paw, pax, pay, paz, raw, ray, saw, sax, say]
   8    2273      [acre, bard, bare, base, cape, card, care, case]
   8    726       [pam, pan, ram, ran, sam, san, sao, scm]
   7    4663      [gone, good, goof, home, hone, hood, hoof]
   7    7283      [pate, pave, rate, rave, saud, save, scud]
   7    426       [gam, gao, ham, han, ian, ibm, ibn]
   7    782       [pta, pub, puc, pvc, qua, rub, sub]

Top 6 in length:

Length  Textonym        Words

### ===  ==============  ==

  14    25287876746242  [claustrophobia, claustrophobic]
  13    7244967473642   [schizophrenia, schizophrenic]
  12    666628676342    [onomatopoeia, onomatopoeic]
  11    49376746242     [hydrophobia, hydrophobic]
  10    2668368466      [contention, convention]
  10    6388537663      [mettlesome, nettlesome]

```



## Lua


```Lua
-- Global variables
http = require("socket.http")
keys = {"VOICEMAIL", "abc", "def", "ghi", "jkl", "mno", "pqrs", "tuv", "wxyz"}
dictFile = "http://www.puzzlers.org/pub/wordlists/unixdict.txt"

-- Return the sequence of keys required to type a given word
function keySequence (str)
    local sequence, noMatch, letter = ""
    for pos = 1, #str do
        letter = str:sub(pos, pos)
        for i, chars in pairs(keys) do
            noMatch = true
            if chars:match(letter) then
                sequence = sequence .. tostring(i)
                noMatch = false
                break
            end
        end
        if noMatch then return nil end
    end
    return tonumber(sequence)
end

-- Generate table of words grouped by key sequence
function textonyms (dict)
    local combTable, keySeq = {}
    for word in dict:gmatch("%S+") do
        keySeq = keySequence(word)
        if keySeq then
            if combTable[keySeq] then
                table.insert(combTable[keySeq], word)
            else
                combTable[keySeq] = {word}
            end
        end
    end
    return combTable
end

-- Analyse sequence table and print details
function showReport (keySeqs)
    local wordCount, seqCount, tCount = 0, 0, 0
    for seq, wordList in pairs(keySeqs) do
        wordCount = wordCount + #wordList
        seqCount = seqCount + 1
        if #wordList > 1 then tCount = tCount + 1 end
    end
    print("There are " .. wordCount .. " words in " .. dictFile)
    print("which can be represented by the digit key mapping.")
    print("They require " .. seqCount .. " digit combinations to represent them.")
    print(tCount .. " digit combinations represent Textonyms.")
end

-- Main procedure
showReport(textonyms(http.request(dictFile)))
```

```txt
There are 24983 words in http://www.puzzlers.org/pub/wordlists/unixdict.txt
which can be represented by the digit key mapping.
They require 22908 digit combinations to represent them.
1473 digit combinations represent Textonyms.
```



## Perl


```perl
my $src = 'unixdict.txt';

# filter word-file for valid input, transform to low-case 
open $fh, "<", $src;
@words = grep { /^[a-zA-Z]+$/ } <$fh>;
map { tr/A-Z/a-z/ } @words;

# translate words to dials
map { tr/abcdefghijklmnopqrstuvwxyz/22233344455566677778889999/ } @dials = @words;

# get unique values (modify @dials) and non-unique ones (are textonyms)
@dials = grep {!$h{$_}++} @dials;
@textonyms = grep { $h{$_} > 1 } @dials;

print "There are @{[scalar @words]} words in '$src' which can be represented by the digit key mapping.
They require @{[scalar @dials]} digit combinations to represent them.
@{[scalar @textonyms]} digit combinations represent Textonyms.";
```

```txt
There are 24978 words in 'unixdict.txt' which can be represented by the digit key mapping.
They require 22903 digit combinations to represent them.
1473 digit combinations represent Textonyms.
```



## Perl 6


```perl6
my $src = 'unixdict.txt';

my @words = slurp($src).lines.grep(/ ^ <alpha>+ $ /);

my @dials = @words.classify: {
    .trans('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
        => '2223334445556667777888999922233344455566677778889999');
}

my @textonyms = @dials.grep(*.value > 1);

say qq:to 'END';
    There are {+@words} words in $src which can be represented by the digit key mapping.
    They require {+@dials} digit combinations to represent them.
    {+@textonyms} digit combinations represent Textonyms.
    END

say "Top 5 in ambiguity:";
say "    ",$_ for @textonyms.sort(-*.value)[^5];

say "\nTop 5 in length:";
say "    ",$_ for @textonyms.sort(-*.key.chars)[^5];
```

```txt
There are 24978 words in unixdict.txt which can be represented by the digit key mapping.
They require 22903 digit combinations to represent them.
1473 digit combinations represent Textonyms.

Top 5 in ambiguity:
    269 => amy any bmw bow box boy cow cox coy
    729 => paw pax pay paz raw ray saw sax say
    2273 => acre bard bare base cape card care case
    726 => pam pan ram ran sam san sao scm
    426 => gam gao ham han ian ibm ibn

Top 5 in length:
    25287876746242 => claustrophobia claustrophobic
    7244967473642 => schizophrenia schizophrenic
    666628676342 => onomatopoeia onomatopoeic
    49376746242 => hydrophobia hydrophobic
    2668368466 => contention convention
```



## Phix


```Phix
sequence digit = repeat(-1,255)
         digit['a'..'c'] = '2'
         digit['d'..'f'] = '3'
         digit['g'..'i'] = '4'
         digit['j'..'l'] = '5'
         digit['m'..'o'] = '6'
         digit['p'..'s'] = '7'
         digit['t'..'v'] = '8'
         digit['w'..'z'] = '9'

function digits(string word)
    for i=1 to length(word) do
        integer ch = word[i]
        if ch<'a' or ch>'z' then return "" end if
        word[i] = digit[ch]
    end for
    return word
end function
    
sequence words = {}, last=""
object word, keycode
integer keycode_count = 0, textonyms = 0, 
        this_count = 0, max_count = 0, max_idx

integer fn = open("demo\\unixdict.txt","r")
while 1 do
    word = trim(gets(fn))
    if atom(word) then exit end if
    keycode = digits(word)
    if length(keycode) then
        words = append(words, {keycode, word})
    end if
end while
close(fn)
printf(1,"There are %d words in unixdict.txt which can be represented by the digit key mapping.\n",{length(words)})

words = sort(words)
for i=1 to length(words) do
    {keycode,word} = words[i]
    if keycode=last then
        textonyms += this_count=1
        this_count += 1
        if this_count>max_count then
            max_count = this_count
            max_idx = i
        end if
    else
        keycode_count += 1
        last = keycode
        this_count = 1
    end if
end for

printf(1,"They require %d digit combinations to represent them.\n",{keycode_count})
printf(1,"%d digit combinations represent Textonyms.\n",{textonyms})

sequence dups = {}
for i=max_idx-max_count+1 to max_idx do
    dups = append(dups,words[i][2])
end for

printf(1,"The maximum number of textonyms for a particular digit key mapping is %d:\n",{max_count})
printf(1," %s encodes %s\n",{words[max_idx][1],join(dups,"/")})
```

```txt

There are 24979 words in unixdict.txt which can be represented by the digit key mapping.
They require 22904 digit combinations to represent them.
1473 digit combinations represent Textonyms.
The maximum number of textonyms for a particular digit key mapping is 9:
 269 encodes amy/any/bmw/bow/box/boy/cow/cox/coy

```



## PowerShell


```PowerShell

$url  = "http://www.puzzlers.org/pub/wordlists/unixdict.txt"
$file = "$env:TEMP\unixdict.txt"
(New-Object System.Net.WebClient).DownloadFile($url, $file)
$unixdict = Get-Content -Path $file

[string]$alpha = "abcdefghijklmnopqrstuvwxyz"
[string]$digit = "22233344455566677778889999"

$table = [ordered]@{}

for ($i = 0; $i -lt $alpha.Length; $i++)
{ 
    $table.Add($alpha[$i], $digit[$i])
}

$words = foreach ($word in $unixdict)
{
    if ($word -match "^[a-z]*$")
    {
        [PSCustomObject]@{
            Word   = $word
            Number = ($word.ToCharArray() | ForEach-Object {$table.$_}) -join ""
        }
    }
}

$digitCombinations = $words | Group-Object -Property Number

$textonyms = $digitCombinations | Where-Object -Property Count -GT 1 | Sort-Object -Property Count -Descending

Write-Host ("There are {0} words in {1} which can be represented by the digit key mapping." -f $words.Count, $url)
Write-Host ("They require {0} digit combinations to represent them."                        -f $digitCombinations.Count)
Write-Host ("{0} digit combinations represent Textonyms.`n"                                 -f $textonyms.Count)

Write-Host "Top 5 in ambiguity:"
$textonyms | Select-Object -First 5 -Property Count,
                                              @{Name="Textonym"; Expression={$_.Name}},
                                              @{Name="Words"   ; Expression={$_.Group.Word -join ", "}} | Format-Table -AutoSize
Write-Host "Top 5 in length:"
$textonyms | Sort-Object {$_.Name.Length} -Descending |
             Select-Object -First 5 -Property @{Name="Length"  ; Expression={$_.Name.Length}},
                                              @{Name="Textonym"; Expression={$_.Name}},
                                              @{Name="Words"   ; Expression={$_.Group.Word -join ", "}} | Format-Table -AutoSize

Remove-Item -Path $file -Force -ErrorAction SilentlyContinue

```

```txt

There are 24978 words in http://www.puzzlers.org/pub/wordlists/unixdict.txt which can be represented by the digit key mapping.
They require 22903 digit combinations to represent them.
1473 digit combinations represent Textonyms.

Top 5 in ambiguity:

Count Textonym Words                                         
----- -------- -----                                         
    9 729      paw, pax, pay, paz, raw, ray, saw, sax, say   
    9 269      amy, any, bmw, bow, box, boy, cow, cox, coy   
    8 726      pam, pan, ram, ran, sam, san, sao, scm        
    8 2273     acre, bard, bare, base, cape, card, care, case
    7 426      gam, gao, ham, han, ian, ibm, ibn             


Top 5 in length:

Length Textonym       Words                         
------ --------       -----                         
    14 25287876746242 claustrophobia, claustrophobic
    13 7244967473642  schizophrenia, schizophrenic  
    12 666628676342   onomatopoeia, onomatopoeic    
    11 49376746242    hydrophobia, hydrophobic      
    10 6388537663     mettlesome, nettlesome        

```



## Python


```python
from collections import defaultdict
import urllib.request

CH2NUM = {ch: str(num) for num, chars in enumerate('abc def ghi jkl mno pqrs tuv wxyz'.split(), 2) for ch in chars}
URL = 'http://www.puzzlers.org/pub/wordlists/unixdict.txt'


def getwords(url):
 return urllib.request.urlopen(url).read().decode("utf-8").lower().split()

def mapnum2words(words):
    number2words = defaultdict(list)
    reject = 0
    for word in words:
        try:
            number2words[''.join(CH2NUM[ch] for ch in word)].append(word)
        except KeyError:
            # Reject words with non a-z e.g. '10th'
            reject += 1
    return dict(number2words), reject

def interactiveconversions():
    global inp, ch, num
    while True:
        inp = input("\nType a number or a word to get the translation and textonyms: ").strip().lower()
        if inp:
            if all(ch in '23456789' for ch in inp):
                if inp in num2words:
                    print("  Number {0} has the following textonyms in the dictionary: {1}".format(inp, ', '.join(
                        num2words[inp])))
                else:
                    print("  Number {0} has no textonyms in the dictionary.".format(inp))
            elif all(ch in CH2NUM for ch in inp):
                num = ''.join(CH2NUM[ch] for ch in inp)
                print("  Word {0} is{1} in the dictionary and is number {2} with textonyms: {3}".format(
                    inp, ('' if inp in wordset else "n't"), num, ', '.join(num2words[num])))
            else:
                print("  I don't understand %r" % inp)
        else:
            print("Thank you")
            break


if __name__ == '__main__':
    words = getwords(URL)
    print("Read %i words from %r" % (len(words), URL))
    wordset = set(words)
    num2words, reject = mapnum2words(words)
    morethan1word = sum(1 for w in num2words if len(num2words[w]) > 1)
    maxwordpernum = max(len(values) for values in num2words.values())
    print("""
There are {0} words in {1} which can be represented by the Textonyms mapping.
They require {2} digit combinations to represent them.
{3} digit combinations represent Textonyms.\
""".format(len(words) - reject, URL, len(num2words), morethan1word))

    print("\nThe numbers mapping to the most words map to %i words each:" % maxwordpernum)
    maxwpn = sorted((key, val) for key, val in num2words.items() if len(val) == maxwordpernum)
    for num, wrds in maxwpn:
        print("  %s maps to: %s" % (num, ', '.join(wrds)))

    interactiveconversions()
```


```txt
Read 25104 words from 'http://www.puzzlers.org/pub/wordlists/unixdict.txt'

There are 24978 words in http://www.puzzlers.org/pub/wordlists/unixdict.txt which can be represented by the Textonyms mapping.
They require 22903 digit combinations to represent them.
1473 digit combinations represent Textonyms.

The numbers mapping to the most words map to 9 words each:
  269 maps to: amy, any, bmw, bow, box, boy, cow, cox, coy
  729 maps to: paw, pax, pay, paz, raw, ray, saw, sax, say

Type a number or a word to get the translation and textonyms: rosetta
  Word rosetta is in the dictionary and is number 7673882 with textonyms: rosetta

Type a number or a word to get the translation and textonyms: code
  Word code is in the dictionary and is number 2633 with textonyms: bode, code, coed

Type a number or a word to get the translation and textonyms: 2468
  Number 2468 has the following textonyms in the dictionary: ainu, chou

Type a number or a word to get the translation and textonyms: 3579
  Number 3579 has no textonyms in the dictionary.

Type a number or a word to get the translation and textonyms: 
Thank you
```



## Racket


This version allows digits to be used (since you can usually enter them through an SMS-style keypad).

<code>unixdict.txt</code> has words like <q>2nd</q> which would not be valid using letters only, but is textable.


```racket
#lang racket
(module+ test (require tests/eli-tester))
(module+ test
  (test
   (map char->sms-digit (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ."))
   => (list 2 2 2 3 3 3 4 4 4 5 5 5 6 6 6 7 7 7 7 8 8 8 9 9 9 9 #f)))

(define char->sms-digit
  (match-lambda
    [(? char-lower-case? (app char-upcase C)) (char->sms-digit C)]
    ;; Digits, too, can be entered on a text pad!
    [(? char-numeric? (app char->integer c)) (- c (char->integer #\0))]
    [(or #\A #\B #\C) 2]
    [(or #\D #\E #\F) 3]
    [(or #\G #\H #\I) 4]
    [(or #\J #\K #\L) 5]
    [(or #\M #\N #\O) 6]
    [(or #\P #\Q #\R #\S) 7]
    [(or #\T #\U #\V) 8]
    [(or #\W #\X #\Y #\Z) 9]
    [_ #f]))

(module+ test
  (test
   (word->textonym "criticisms") => 2748424767
   (word->textonym "Briticisms") => 2748424767
   (= (word->textonym "Briticisms") (word->textonym "criticisms"))))

(define (word->textonym w)
  (for/fold ((n 0)) ((s (sequence-map char->sms-digit (in-string w))) #:final (not s))
    (and s (+ (* n 10) s))))

(module+ test
  (test
   ((cons-uniquely 'a) null) => '(a)
   ((cons-uniquely 'a) '(b)) => '(a b)
   ((cons-uniquely 'a) '(a b c)) => '(a b c)))

(define ((cons-uniquely a) d)
  (if (member a d) d (cons a d)))

(module+ test
  (test
   (with-input-from-string "criticisms" port->textonym#) =>
   (values 1 (hash 2748424767 '("criticisms")))
   (with-input-from-string "criticisms\nBriticisms" port->textonym#) =>
   (values 2 (hash 2748424767 '("Briticisms" "criticisms")))
   (with-input-from-string "oh-no!-dashes" port->textonym#) =>
   (values 0 (hash))))

(define (port->textonym#)
  (for/fold
   ((n 0) (t# (hash)))
   ((w (in-port read-line)))
    (define s (word->textonym w))
    (if s
        (values (+ n 1) (hash-update t# s (cons-uniquely w) null))
        (values n t#))))

(define (report-on-file f-name)
  (define-values (n-words textonym#) (with-input-from-file f-name port->textonym#))
  
  (define n-textonyms (for/sum ((v (in-hash-values textonym#)) #:when (> (length v) 1)) 1))
  
  (printf "--- report on ~s ends ---~%" f-name)
  (printf
   #<<EOS
There are ~a words in ~s which can be represented by the digit key mapping.
They require ~a digit combinations to represent them.
~a digit combinations represent Textonyms.

EOS
   n-words f-name (hash-count textonym#) n-textonyms)
  
  ;; Show all the 6+ textonyms
  (newline)
  (for (((k v) (in-hash textonym#)) #:when (>= (length v) 6)) (printf "~a -> ~s~%" k v))
  (printf "--- report on ~s ends ---~%" f-name))

(module+ main
  (report-on-file "data/unixdict.txt"))
```


```txt
--- report on "data/unixdict.txt" ends ---
There are 24988 words in "data/unixdict.txt" which can be represented by the digit key mapping.
They require 22905 digit combinations to represent them.
1477 digit combinations represent Textonyms.

226 -> ("can" "cam" "ban" "bam" "acm" "abo")
269 -> ("coy" "cox" "cow" "boy" "box" "bow" "bmw" "any" "amy")
426 -> ("ibn" "ibm" "ian" "han" "ham" "gao" "gam")
529 -> ("lay" "lax" "law" "kay" "jay" "jaw")
627 -> ("oar" "ncr" "nbs" "nap" "mar" "map")
729 -> ("say" "sax" "saw" "ray" "raw" "paz" "pay" "pax" "paw")
726 -> ("scm" "sao" "san" "sam" "ran" "ram" "pan" "pam")
782 -> ("sub" "rub" "qua" "pvc" "puc" "pub" "pta")
786 -> ("sun" "sum" "run" "rum" "quo" "pun")
843 -> ("vie" "vhf" "uhf" "tie" "tid" "the")
2273 -> ("case" "care" "card" "cape" "base" "bare" "bard" "acre")
2253 -> ("calf" "cake" "bale" "bald" "bake" "able")
2666 -> ("coon" "conn" "boon" "boom" "bonn" "ammo")
4663 -> ("hoof" "hood" "hone" "home" "goof" "good" "gone")
7283 -> ("scud" "save" "saud" "rave" "rate" "pave" "pate")
7243 -> ("said" "sage" "raid" "rage" "paid" "page")
7325 -> ("seal" "reck" "real" "peck" "peal" "peak")
7673 -> ("sore" "rose" "rope" "pose" "pore" "pope")
--- report on "data/unixdict.txt" ends ---
1 test passed
3 tests passed
3 tests passed
3 tests passed
```



## REXX

This REXX version checks for and displays the count of the number of (illegal) words not representable by the ''key digits''.

It also detects and displays the count of duplicate words.

```rexx
/*REXX program counts and displays the number of textonyms that are in a dictionary file*/
parse arg iFID .                                 /*obtain optional fileID from the C.L. */
if iFID=='' | iFID=="," then iFID='UNIXDICT.TXT' /*Not specified?  Then use the default.*/
@.=0                                             /*the placeholder of digit combinations*/
!.=;  $.=                                        /*sparse array of textonyms;  words.   */
alphabet= 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'           /*the supported alphabet to be used.   */
digitKey=  22233344455566677778889999            /*translated alphabet to digit keys.   */
digKey=0;            wordCount=0                 /*number digit combinations; wordCount.*/
ills=0;    dups=0;   longest=0;  mostus=0        /*illegals; duplicated words; longest..*/
first=.;   last=.;   long=0;     most=0          /*first, last, longest, most counts.   */
#=0                                              /*number of textonyms in file (so far).*/
call linein  iFID, 1, 0                          /*point to the first char in dictionary*/    /* ◄■■■■■ optional.*/

  do  while  lines(iFID)\==0                     /*keep reading the file until exhausted*/
          x=linein(iFID);   y=x;      upper x    /*get a word; save a copy; uppercase it*/
  if \datatype(x,'U')  then do;  ills=ills + 1;  iterate;  end       /*Not legal?  Skip.*/
  if $.x==.            then do;  dups=dups + 1;  iterate;  end       /*Duplicate?  Skip.*/
  $.x=.                                          /*indicate that it's a righteous word. */
  wordCount=wordCount + 1                        /*bump the word count  (for the file). */
  z=translate(x, digitKey, alphabet)             /*build a translated digit key word.   */
  @.z=@.z + 1                                    /*flag that the digit key word exists. */
  !.z=!.z  y;         _=words(!.z)               /*build list of equivalent digit key(s)*/

  if _>most  then do; mostus=z;  most=_;  end    /*remember the  "mostus"  digit keys.  */

  if @.z==2  then do; #=# + 1                    /*bump the count of the  textonyms.    */
                      if first==.   then first=z /*the first textonym found.            */
                      last=z                     /* "   last     "      "               */
                      _=length(!.z)              /*the length (# chars) of the digit key*/
                      if _>longest  then long=z  /*is this the  longest  textonym ?     */
                      longest=max(_, longest)    /*now, use this length as a target/goal*/
                  end                            /* [↑]  discretionary  (extra credit). */

  if @.z==1  then digKey=digKey + 1              /*bump the count of digit key words.   */
  end   /*while*/
                        @whichCan...=    'which can be represented by digit key mapping.'
                        @Ta         =    'There are '
say 'The dictionary file being used is: '        iFID
say                     @Ta wordCount    ' words in the file'      @whichCan...
if ills\==0   then say  @Ta ills         ' word's(ills)    "contained illegal characters."
if dups\==0   then say  @Ta dups   " duplicate word"s(dups)  'in the dictionary detected.'
say 'The textonyms require '    digKey   " combination"s(digKey)   'to represent them.'
say @Ta   #   ' digit combination's(#)   " that can represent Textonyms."
say
if first\==.  then say  '    first digit key='   !.first
if  last\==.  then say  '     last digit key='   !.last
if  long\==0  then say  '  longest digit key='   !.long
if  most\==0  then say  ' numerous digit key='   !.mostus       ' ('most     "words)"
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:  if arg(1)==1  then return '';         return "s"              /*a simple pluralizer.*/
```

```txt

The dictionary file being used is:  UNIXDICT.TXT
There are  24978  words in the file which can be represented by digit key mapping.
There are  126  words contained illegal characters.
The textonyms require  22903  combinations to represent them.
There are  1473  digit combinations  that can represent Textonyms.

    first digit key=  aaa aba abc cab
     last digit key=  woe zoe
  longest digit key=  claustrophobia claustrophobic
 numerous digit key=  amy any bmw bow box boy cow cox coy  (9 words)

```

```txt

The dictionary file being used is:  textonyms.txt
There are  12990  words in the file which can be represented by digit key mapping.
There are  95  duplicate words in the dictionary detected.
The textonyms require  11932  combinations to represent them.
There are  650  digit combinations  that can represent Textonyms.

    first digit key=  AA AB AC BA BB BC CA CB
     last digit key=  Phillip Phillis
  longest digit key=  Anglophobia Anglophobic
 numerous digit key=  AP AQ AR AS BP BR BS CP CQ CR Cs  (11 words)

```



## Ruby


```ruby

Textonyms = Hash.new {|n, g| n[g] = []}
File.open("Textonyms.txt") do |file|
  file.each_line {|line|
    Textonyms[(n=line.chomp).gsub(/a|b|c|A|B|C/, '2').gsub(/d|e|f|D|E|F/, '3').gsub(/g|h|i|G|H|I/, '4').gsub(/p|q|r|s|P|Q|R|S/, '7')
                     .gsub(/j|k|l|J|K|L/, '5').gsub(/m|n|o|M|N|O/, '6').gsub(/t|u|v|T|U|V/, '8').gsub(/w|x|y|z|W|X|Y|Z/, '9')] += [n]
  }
end

```

```txt

puts "There are #{Textonyms.inject(0){|n,g| n+g[1].length}} words in #{"Wordlist"} which can be represnted by the Textonyms mapping."
puts "They require #{Textonyms.length} digit combinations to represent them."
puts "#{Textonyms.inject(0){|n,g| g[1].length > 1 ? n+1 : n}} digit combinations correspond to a Textonym"

There are 132916 words in Wordlist which can be represnted by the Textonyms mapping.
They require 117868 digit combinations to represent them.
9579 digit combinations correspond to a Textonym

```


```txt

puts Textonymes["7353284667"]

rejections
selections

```


```txt

puts Textonymes["736672"]

remora
senora

```



## Sidef

```ruby
var words = ARGF.grep(/^[[:alpha:]]+\z/);

var dials = words.group_by {
    .tr('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',
        '2223334445556667777888999922233344455566677778889999');
}

var textonyms = dials.grep_v { .len > 1 };

say <<-END;
    There are #{words.len} words which can be represented by the digit key mapping.
    They require #{dials.len} digit combinations to represent them.
    #{textonyms.len} digit combinations represent Textonyms.
    END

say "Top 5 in ambiguity:";
say textonyms.sort_by { |_,v| -v.len }.first(5).join("\n");

say "\nTop 5 in length:";
say textonyms.sort_by { |k,_| -k.len }.first(5).join("\n");
```

```txt

$ sidef textonyms.sf < unixdict.txt 
There are 24978 words which can be represented by the digit key mapping.
They require 22903 digit combinations to represent them.
1473 digit combinations represent Textonyms.

Top 5 in ambiguity:
["729", ["paw", "pax", "pay", "paz", "raw", "ray", "saw", "sax", "say"]]
["269", ["amy", "any", "bmw", "bow", "box", "boy", "cow", "cox", "coy"]]
["2273", ["acre", "bard", "bare", "base", "cape", "card", "care", "case"]]
["726", ["pam", "pan", "ram", "ran", "sam", "san", "sao", "scm"]]
["782", ["pta", "pub", "puc", "pvc", "qua", "rub", "sub"]]

Top 5 in length:
["25287876746242", ["claustrophobia", "claustrophobic"]]
["7244967473642", ["schizophrenia", "schizophrenic"]]
["666628676342", ["onomatopoeia", "onomatopoeic"]]
["49376746242", ["hydrophobia", "hydrophobic"]]
["2668368466", ["contention", "convention"]]

```



## VBScript


```vb
Set objFSO = CreateObject("Scripting.FileSystemObject")
Set objInFile = objFSO.OpenTextFile(objFSO.GetParentFolderName(WScript.ScriptFullName) &_
	"\unixdict.txt",1)
Set objKeyMap = CreateObject("Scripting.Dictionary")
	With objKeyMap
		.Add "ABC", "2" : .Add "DEF", "3" : .Add "GHI", "4" : .Add "JKL", "5"
		.Add "MNO", "6" : .Add "PQRS", "7" : .Add "TUV", "8" : .Add "WXYZ", "9"
	End With

'Instantiate or Intialize Counters
TotalWords = 0
UniqueCombinations = 0
Set objUniqueWords = CreateObject("Scripting.Dictionary")
Set objMoreThanOneWord = CreateObject("Scripting.Dictionary")

Do Until objInFile.AtEndOfStream
	Word = objInFile.ReadLine
	c = 0
	Num = ""
	If Word <> "" Then
		For i = 1 To Len(Word)
			For Each Key In objKeyMap.Keys
				If InStr(1,Key,Mid(Word,i,1),1) > 0 Then
					Num = Num & objKeyMap.Item(Key)
					c = c + 1
				End If
			Next
		Next
		If c = Len(Word) Then
			TotalWords = TotalWords + 1
			If objUniqueWords.Exists(Num) = False Then
				objUniqueWords.Add Num, ""
				UniqueCombinations = UniqueCombinations + 1
			Else
				If objMoreThanOneWord.Exists(Num) = False Then
					objMoreThanOneWord.Add Num, ""
				End If
			End If
		End If
	End If
Loop	

WScript.Echo "There are " & TotalWords & " words in ""unixdict.txt"" which can be represented by the digit key mapping." & vbCrLf &_
			 "They require " & UniqueCombinations & " digit combinations to represent them." & vbCrLf &_
                         objMoreThanOneWord.Count &  " digit combinations represent Textonyms."

objInFile.Close
```

```txt
There are 24978 words in "unixdict.txt" which can be represented by the digit key mapping.
They require 22903 digit combinations to represent them.
1473 digit combinations represent Textonyms.
```



## Tcl



```Tcl
set keymap {
    2 -> ABC
    3 -> DEF
    4 -> GHI
    5 -> JKL
    6 -> MNO
    7 -> PQRS
    8 -> TUV
    9 -> WXYZ  
}

set url http://www.puzzlers.org/pub/wordlists/unixdict.txt

set report {
There are %1$s words in %2$s which can be represented by the digit key mapping.
They require %3$s digit combinations to represent them.
%4$s digit combinations represent Textonyms.

A %5$s-letter textonym which has %6$s combinations is %7$s:

  %8$s
}

package require http
proc geturl {url} {
    try {
        set tok [http::geturl $url]
        return [http::data $tok]
    } finally {
        http::cleanup $tok
    }
}

proc main {keymap url} {
    foreach {digit -> letters} $keymap {
        foreach l [split $letters ""] {
            dict set strmap $l $digit
        }
    }
    set doc [geturl $url]
    foreach word [split $doc \n] {
        if {![string is alpha -strict $word]} continue
        dict lappend words [string map $strmap [string toupper $word]] $word
    }

    set ncombos [dict size $words]
    set nwords 0
    set ntextos 0
    set nmax 0
    set dmax ""
    dict for {d ws} $words {
        puts [list $d $ws]
        set n [llength $ws]
        incr nwords $n
        if {$n > 1} {
            incr ntextos $n
        }
        if {$n >= $nmax && [string length $d] > [string length $dmax]} {
            set nmax $n
            set dmax $d
        }
    }
    set maxwords [dict get $words $dmax]
    set lenmax [llength $maxwords]
    format $::report $nwords $url $ncombos $ntextos $lenmax $nmax $dmax $maxwords
}

puts [main $keymap $url]
```


```txt

There are 24978 words in http://www.puzzlers.org/pub/wordlists/unixdict.txt which can be represented by the digit key mapping.
They require 22903 digit combinations to represent them.
3548 digit combinations represent Textonyms.

A 6-letter textonym which has 6 combinations is 2253:

  able bake bald bale cake calf

```



## zkl

Like the Python example, this solution uses the Unix Dictionary, rather than the textonyms word list as I don't want to parse the HTML.

```zkl
URL:="http://www.puzzlers.org/pub/wordlists/unixdict.txt";
var ZC=Import("zklCurl");
var keypad=Dictionary(
   "a",2,"b",2,"c",2,  "d",3,"e",3,"f",3,  "g",4,"h",4,"i",4,
   "j",5,"k",5,"l",5,  "m",6,"n",6,"o",6,  "p",7,"q",7,"r",7,"s",7,
   "t",8,"u",8,"v",8,  "w",9,"x",9,"y",9,"z",9);
//fcn numerate(word){ word.toLower().apply(keypad.find.fp1("")) }
fcn numerate(word){ word.toLower().apply(keypad.get) } //-->textonym or error
println("criticisms --> ",numerate("criticisms"));

words:=ZC().get(URL); //--> T(Data,bytes of header, bytes of trailer)
words=words[0].del(0,words[1]);  // remove HTTP header
println("Read %d words from %s".fmt(words.len(1),URL));

wcnt:=Dictionary();
foreach word in (words.walker(11)){  // iterate over stripped lines
   w2n:=try{ numerate(word) }catch(NotFoundError){ continue }; 
   wcnt.appendV(w2n,word);  // -->[textonym:list of words]
}

moreThan1Word:=wcnt.reduce(fcn(s,[(k,v)]){ s+=(v.len()>1) },0);
maxWordPerNum:=(0).max(wcnt.values.apply("len"));

("There are %d words which can be represented by the Textonyms mapping.\n"
"There are %d overlaps.").fmt(wcnt.len(),moreThan1Word).println();
 
println("Max collisions: %d words:".fmt(maxWordPerNum));
foreach k,v in (wcnt.filter('wrap([(k,v)]){ v.len()==maxWordPerNum })){
   println("  %s is the textonym of: %s".fmt(k,v.concat(", ")));
}
```

```txt

criticisms --> 2748424767
Read 25104 words from http://www.puzzlers.org/pub/wordlists/unixdict.txt
There are 22903 words which can be represented by the Textonyms mapping.
There are 1473 overlaps.
Max collisions: 9 words:
  729 is the textonym of: paw, pax, pay, paz, raw, ray, saw, sax, say
  269 is the textonym of: amy, any, bmw, bow, box, boy, cow, cox, coy

```

