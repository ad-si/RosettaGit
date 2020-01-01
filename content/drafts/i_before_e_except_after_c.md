+++
title = "I before E except after C"
description = ""
date = 2019-09-02T19:12:26Z
aliases = []
[extra]
id = 12739
[taxonomies]
categories = []
tags = []
+++

{{task}}

The phrase     [[wp:I before E except after C| "I before E, except after C"]]     is a
widely known mnemonic which is supposed to help when spelling English words.


;Task:
Using the word list from   [http://wiki.puzzlers.org/pub/wordlists/unixdict.txt http://wiki.puzzlers.org/pub/wordlists/unixdict.txt],

check if the two sub-clauses of the phrase are plausible individually:
:::#   ''"I before E when not preceded by C"''
:::#   ''"E before I when preceded by C"''



If both sub-phrases are plausible then the original phrase can be said to be plausible.

Something is plausible if the number of words having the feature is more than two times the number of words having the opposite feature (where feature is 'ie' or 'ei' preceded or not by 'c' as appropriate).


;Stretch goal:
As a stretch goal use the entries from the table of [http://ucrel.lancs.ac.uk/bncfreq/lists/1_2_all_freq.txt Word Frequencies in Written and Spoken English: based on the British National Corpus], (selecting those rows with three space or tab separated words only), to see if the phrase is plausible when word frequencies are taken into account.


''Show your output here as well as your program.''


;cf.:
* [http://news.bbc.co.uk/1/hi/education/8110573.stm Schools to rethink 'i before e'] - BBC news, 20 June 2009
* [http://www.youtube.com/watch?v=duqlZXiIZqA I Before E Except After C] - [[wp:QI|QI]] Series 8 Ep 14, (humorous)
* [http://ucrel.lancs.ac.uk/bncfreq/ Companion website] for the book: "Word Frequencies in Written and Spoken English: based on the British National Corpus".





## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}} Uses non-standard procedure to lower available in Algol 68G.

```algol68
# tests the plausibility of "i before e except after c" using unixdict.txt #

# implements the plausibility test specified by the task                   #
# returns TRUE if with > 2 * without                                       #
PROC plausible = ( INT with, without )BOOL: with > 2 * without;

# shows the plausibility of with and without                               #
PROC show plausibility = ( STRING legend, INT with, without )VOID:
     print( ( legend, IF plausible( with, without ) THEN " is plausible" ELSE " is not plausible" FI, newline ) );

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
    INT    cei := 0;
    INT    xei := 0;
    INT    cie := 0;
    INT    xie := 0;
    WHILE STRING word;
          get( input file, ( word, newline ) );
          NOT at eof
    DO
        # examine the word for cie, xie (x /= c), cei and xei (x /= c)      #
        FOR pos FROM LWB word TO UPB word DO word[ pos ] := to lower( word[ pos ] ) OD;
        IF   word = "ie" THEN
            xie +:= 1
        ELIF word = "ei" THEN
            xei +:= 1
        ELSE
            INT length = ( UPB word - LWB word ) + 1;
            IF length > 1 THEN
                IF   word[ LWB word ] = "i" AND word[ LWB word + 1 ] = "e" THEN
                    # word starts ie                                        #
                    xie +:= 1
                ELIF word[ LWB word ] = "e" AND word[ LWB word + 1 ] = "i" THEN
                    # word starts ei                                        #
                    xei +:= 1
                FI;
                FOR pos FROM LWB word + 1 TO UPB word - 1 DO
                    IF   word[ pos ] = "i" AND word[ pos + 1 ] = "e" THEN
                        # have i before e, check the preceeding character   #
                        IF word[ pos - 1 ] = "c" THEN cie ELSE xie FI +:= 1
                    ELIF word[ pos ] = "e" AND word[ pos + 1 ] = "i" THEN
                        # have e before i, check the preceeding character   #
                        IF word[ pos - 1 ] = "c" THEN cei ELSE xei FI +:= 1
                    FI
                OD
            FI
        FI
    OD;
    # close the file #
    close( input file );

    # test the hypothesis                                                    #
    print( ( "cie occurances: ", whole( cie, 0 ), newline ) );
    print( ( "xie occurances: ", whole( xie, 0 ), newline ) );
    print( ( "cei occurances: ", whole( cei, 0 ), newline ) );
    print( ( "xei occurances: ", whole( xei, 0 ), newline ) );
    show plausibility( "i before e except after c", xie, cie );
    show plausibility( "e before i except after c", xei, cei );
    show plausibility( "i before e   when after c", cie, xie );
    show plausibility( "e before i   when after c", cei, xei );
    show plausibility( "i before e     in general", xie + cie, xei + cei );
    show plausibility( "e before i     in general", xei + cei, xie + cie )
FI
```

{{out}}

```txt

cie occurances: 24
xie occurances: 466
cei occurances: 13
xei occurances: 217
i before e except after c is plausible
e before i except after c is plausible
i before e   when after c is not plausible
e before i   when after c is not plausible
i before e     in general is plausible
e before i     in general is not plausible

```



## AutoHotkey


```AutoHotkey
WordList := URL_ToVar("http://wiki.puzzlers.org/pub/wordlists/unixdict.txt")
WordList := RegExReplace(WordList, "i)cie", "", cieN)
WordList := RegExReplace(WordList, "i)cei", "", ceiN)
RegExReplace(WordList, "i)ie", "", ieN)
RegExReplace(WordList, "i)ei", "", eiN)

cei := ceiN / cieN > 2 ? "plausible" : "implausible"
ei  := ieN  / eiN  > 2 ? "plausible" : "implausible"
ova := cei = "plausible." && ei = "plausible" ? "plausible" : "implausible"

MsgBox, % """I before E when not preceded by C"" is " ei ".`n"
        . ieN " cases for and " eiN " cases against is a ratio of " ieN / eiN ".`n`n"
        . """E before I when preceded by C"" is " cei ".`n"
        . ceiN " cases for and " cieN " cases against is a ratio of " ceiN / cieN ".`n`n"
        . "Overall the rule is " ova "."

URL_ToVar(URL) {
    WebRequest := ComObjCreate("WinHttp.WinHttpRequest.5.1")
    WebRequest.Open("GET", URL)
    WebRequest.Send()
    return, WebRequest.ResponseText
}
```

{{out}}

```txt
"I before E when not preceded by C" is plausible.
466 cases for and 217 cases against is a ratio of 2.147465.

"E before I when preceded by C" is implausible.
13 cases for and 24 cases against is a ratio of 0.541667.

Overall the rule is implausible.
```



## AWK


```awk
#!/usr/bin/awk -f

/.ei/ {nei+=cnt($3)}
/cei/ {cei+=cnt($3)}

/.ie/ {nie+=cnt($3)}
/cie/ {cie+=cnt($3)}

function cnt(c) {
	if (c<1) return 1;
	return c;
}

END {
	printf("cie: %i\nnie: %i\ncei: %i\nnei: %i\n",cie,nie-cie,cei,nei-cei);
	v = v2 = "";
	if (nie < 3 * cie) {
		v =" not";
	}
	print "I before E when not preceded by C: is"v" plausible";
	if (nei > 3 * cei)  {
		v = v2 =" not";
	}
	print "E before I when preceded by C: is"v2" plausible";
        print "Overall rule is"v" plausible";
}
```


Usage:

```txt
$ awk -f ./i_before_e_except_after_c.awk unixdict.txt
cie: 24
nie: 464
cei: 13
nei: 194
I before E when not preceded by C: is plausible
E before I when preceded by C: is not plausible

$ awk -f i_before_e_except_after_c.awk 1_2_all_freq.txt
cie: 994
nie: 8148
cei: 327
nei: 4826
I before E when not preceded by C: is plausible
E before I when preceded by C: is not plausible
Overall rule is not plausible
```



## Batch File

Download first the text file, then put it on the same directory with this sample code:

```dos
::I before E except after C task from Rosetta Code Wiki
::Batch File Implementation

@echo off
setlocal enabledelayedexpansion
	::Initialization
set ie=0
set ei=0
set cie=0
set cei=0

set propos1=FALSE
set propos2=FALSE
set propos3=FALSE

	::Do the matching
for /f %%X in (unixdict.txt) do (
	set word=%%X
	if not "!word:ie=!"=="!word!" if "!word:cie=!"=="!word!" (set /a ie+=1)
	if not "!word:ei=!"=="!word!" if "!word:cei=!"=="!word!" (set /a ei+=1)
	if not "!word:cei=!"=="!word!" (set /a cei+=1)
	if not "!word:cie=!"=="!word!" (set /a cie+=1)
)

set /a "counter1=!ei!*2,counter2=!cie!*2"

if !ie! gtr !counter1! set propos1=TRUE
echo.Plausibility of "I before E when not preceded by C": !propos1! (!ie! VS !ei!)

if !cei! gtr !counter2! set propos2=TRUE
echo.Plausibility of "E before I when preceded by C": !propos2! (!cei! VS !cie!)

if !propos1!==TRUE if !propos2!==TRUE (set propos3=TRUE)
echo.Overall plausibility of "I before E EXCEPT after C": !propos3!

pause
exit /b 0
```

{{Out}}

```txt
Plausibility of "I before E when not preceded by C": TRUE (465 VS 213)
Plausibility of "E before I when preceded by C": FALSE (13 VS 24)
Overall plausibility of "I before E EXCEPT after C": FALSE
Press any key to continue . . .
```


==== '''Fast solution using standard external commands FINDSTR and FIND:''' ====
Each word is counted once if word has at least one occurrence of test string (word with 2 or more occurrences only counts once).
The same word may count toward different categories.

```dos
@echo off
setlocal enableDelayedExpansion
for /f %%A in ('findstr /i "^ie [^c]ie" unixdict.txt ^| find /c /v ""') do set Atrue=%%A
for /f %%A in ('findstr /i "^ei [^c]ei" unixdict.txt ^| find /c /v ""') do set Afalse=%%A
for /f %%A in ('findstr /i "[c]ei" unixdict.txt ^| find /c /v ""') do set Btrue=%%A
for /f %%A in ('findstr /i "[c]ie" unixdict.txt ^| find /c /v ""') do set Bfalse=%%A
set /a "Aresult=Atrue/Afalse/2, Bresult=Btrue/Bfalse/2, Result=^!^!Aresult*Bresult"
set "Answer1=Plausible" & set "Answer0=Implausible"
echo I before E when not preceded by C: True=%Atrue% False=%Afalse% : !Answer%Aresult%!
echo E before I when preceded by C: True=%Btrue% False=%Bfalse% : !Answer%Bresult%!
echo I before E, except after C : !Answer%Result%!
```

{{Out}}

```txt
I before E when not preceded by C: True=465 False=213 : Plausible
E before I when preceded by C: True=13 False=24 : Implausible
I before E, except after C : Implausible
```


==== '''Stretch solution using standard external command FINDSTR:''' ====
Each word frequency is included once if word has at least one occurrence of test string (word with 2 or more occurrences only counts once).
The same word frequency may count toward different categories.

```dos
@echo off
setlocal enableDelayedExpansion
set /a Atrue=Afalse=Btrue=Bfalse=0
for /f "tokens=3*" %%A in ('findstr /i "[^c]ie" 1_2_all_freq.txt') do if "%%B" equ "" set /a Atrue+=%%A
for /f "tokens=3*" %%A in ('findstr /i "[^c]ei" 1_2_all_freq.txt') do if "%%B" equ "" set /a Afalse+=%%A
for /f "tokens=3*" %%A in ('findstr /i "[c]ei" 1_2_all_freq.txt') do if "%%B" equ "" set /a Btrue+=%%A
for /f "tokens=3*" %%A in ('findstr /i "[c]ie" 1_2_all_freq.txt') do if "%%B" equ "" set /a Bfalse+=%%A
set /a "Aresult=Atrue/Afalse/2, Bresult=Btrue/Bfalse/2, Result=^!^!Aresult*Bresult"
set "Answer1=Plausible" & set "Answer0=Implausible"
echo I before E when not preceded by C: True=%Atrue% False=%Afalse% : !Answer%Aresult%!
echo E before I when preceded by C: True=%Btrue% False=%Bfalse% : !Answer%Bresult%!
echo I before E, except after C : !Answer%Result%!
```

{{Out}}

```txt
I before E when not preceded by C: True=8192 False=4826 : Implausible
E before I when preceded by C: True=327 False=994 : Implausible
I before E, except after C : Implausible
```



## C

Inspired by the J solution, but implemented as a single pass through the data,
we have [http://flex.sourceforge.net/ flex] build the finite state machine in C.
This may in turn motivate me to provide a second J solution as a single pass FSM.
Please find the program output hidden at the top of the source as part of the build and example run.

```c

%{
  /*
    compilation and example on a GNU linux system:

    $ flex --case-insensitive --noyywrap --outfile=cia.c source.l
    $ make LOADLIBES=-lfl cia
    $ ./cia < unixdict.txt
    I before E when not preceded by C: plausible
    E before I when preceded by C: implausible
    Overall, the rule is: implausible
  */
  int cie, cei, ie, ei;
%}

%%

cie ++cie, ++ie; /* longer patterns are matched preferentially, consuming input */
cei ++cei, ++ei;
ie ++ie;
ei ++ei;
.|\n ;

%%

int main() {
  cie = cei = ie = ei = 0;
  yylex();
  printf("%s: %s\n","I before E when not preceded by C", (2*ei < ie ? "plausible" : "implausible"));
  printf("%s: %s\n","E before I when preceded by C", (2*cie < cei ? "plausible" : "implausible"));
  printf("%s: %s\n","Overall, the rule is", (2*(cie+ei) < (cei+ie) ? "plausible" : "implausible"));
  return 0;
}

```




## C++


* If the file changes, the outcome will possibly be different.
:*   <code>sha1 of file 2013-12-30: 058f8872306ef36f679d44f1b556334a13a85b57  unixdict.txt</code>
* Build with: <code>g++ -Wall -std=c++0x  thisfile.cpp -lboost_regex</code>
:*   (Test used 4.4, so only a limited number of C++11 features were used.)


```cpp
#include <iostream>
#include <fstream>
#include <string>
#include <tuple>
#include <vector>
#include <stdexcept>
#include <boost/regex.hpp>



struct Claim {
        Claim(const std::string& name) : name_(name), pro_(0), against_(0), propats_(), againstpats_() {
        }

        void add_pro(const std::string& pat) {
               propats_.push_back(std::make_tuple(boost::regex(pat), pat[0] == '^'));
        }
        void add_against(const std::string& pat) {
               againstpats_.push_back(std::make_tuple(boost::regex(pat), pat[0] == '^'));
        }
        bool plausible() const { return pro_ > against_*2; }
        void check(const char * buf, uint32_t len) {
                for (auto i = propats_.begin(), ii = propats_.end(); i != ii; ++i) {
                        uint32_t pos = 0;
                        boost::cmatch m;
                        if (std::get<1>(*i) && pos > 0) continue;
                        while (pos < len && boost::regex_search(buf+pos, buf+len, m, std::get<0>(*i))) {
                                ++pro_;
                                if (pos > 0) std::cerr << name_ << " [pro] multiple matches in: " << buf << "\n";
                                pos += m.position() + m.length();
                        }
                }
                for (auto i = againstpats_.begin(), ii = againstpats_.end(); i != ii; ++i) {
                        uint32_t pos = 0;
                        boost::cmatch m;
                        if (std::get<1>(*i) && pos > 0) continue;
                        while (pos < len && boost::regex_search(buf+pos, buf+len, m, std::get<0>(*i))) {
                                ++against_;
                                if (pos > 0) std::cerr << name_ << " [against] multiple matches in: " << buf << "\n";
                                pos += m.position() + m.length();
                        }
                }
        }
        friend std::ostream& operator<<(std::ostream& os, const Claim& c);
private:
        std::string name_;
        uint32_t pro_;
        uint32_t against_;
        // tuple<regex,begin only>
        std::vector<std::tuple<boost::regex,bool>> propats_;
        std::vector<std::tuple<boost::regex,bool>> againstpats_;
};

std::ostream& operator<<(std::ostream& os, const Claim& c) {
        os << c.name_ << ": matches: " << c.pro_ << " vs. counter matches: " << c.against_ << ". ";
        os << "Plausibility: " << (c.plausible() ? "yes" : "no") << ".";
        return os;
}


int main(int argc, char ** argv) {
        try {
                if (argc < 2) throw std::runtime_error("No input file.");
                std::ifstream is(argv[1]);
                if (! is) throw std::runtime_error("Input file not valid.");

                Claim ieclaim("[^c]ie");
                ieclaim.add_pro("[^c]ie");
                ieclaim.add_pro("^ie");
                ieclaim.add_against("[^c]ei");
                ieclaim.add_against("^ei");

                Claim ceiclaim("cei");
                ceiclaim.add_pro("cei");
                ceiclaim.add_against("cie");

                {
                        const uint32_t MAXLEN = 32;
                        char buf[MAXLEN];
                        uint32_t longest = 0;
                        while (is) {
                                is.getline(buf, sizeof(buf));
                                if (is.gcount() <= 0) break;
                                else if (is.gcount() > longest) longest = is.gcount();
                                ieclaim.check(buf, is.gcount());
                                ceiclaim.check(buf, is.gcount());
                        }
                        if (longest >= MAXLEN) throw std::runtime_error("Buffer too small.");
                }

                std::cout << ieclaim << "\n";
                std::cout << ceiclaim << "\n";
                std::cout << "Overall plausibility: " << (ieclaim.plausible() && ceiclaim.plausible() ? "yes" : "no") << "\n";


        } catch (const std::exception& ex) {
                std::cerr << "*** Error: " << ex.what() << "\n";
                return -1;
        }
        return 0;
}

```


{{out}}

```txt

[^c]ie [pro] multiple matches in: siegfried
[^c]ie [against] multiple matches in: weinstein
[^c]ie: matches: 466 vs. counter matches: 217. Plausibility: yes.
cei: matches: 13 vs. counter matches: 24. Plausibility: no.
Overall plausibility: no

```


=={{header|C#|C sharp}}==
{{trans|Java}}

```csharp
using System;
using System.Collections.Generic;
using System.IO;

namespace IBeforeE {
    class Program {
        static bool IsOppPlausibleWord(string word) {
            if (!word.Contains("c") && word.Contains("ei")) {
                return true;
            }
            if (word.Contains("cie")) {
                return true;
            }
            return false;
        }

        static bool IsPlausibleWord(string word) {
            if (!word.Contains("c") && word.Contains("ie")) {
                return true;
            }
            if (word.Contains("cei")) {
                return true;
            }
            return false;
        }

        static bool IsPlausibleRule(string filename) {
            IEnumerable<string> wordSource = File.ReadLines(filename);
            int trueCount = 0;
            int falseCount = 0;

            foreach (string word in wordSource) {
                if (IsPlausibleWord(word)) {
                    trueCount++;
                }
                else if (IsOppPlausibleWord(word)) {
                    falseCount++;
                }
            }

            Console.WriteLine("Plausible count: {0}", trueCount);
            Console.WriteLine("Implausible count: {0}", falseCount);
            return trueCount > 2 * falseCount;
        }

        static void Main(string[] args) {
            if (IsPlausibleRule("unixdict.txt")) {
                Console.WriteLine("Rule is plausible.");
            }
            else {
                Console.WriteLine("Rule is not plausible.");
            }
        }
    }
}
```

{{out}}

```txt
Plausible count: 384
Implausible count: 204
Rule is not plausible.
```



## Clojure


The output here was generated with the files as of 21st June 2016.


```clojure

(ns i-before-e.core
  (:require [clojure.string :as s])
  (:gen-class))

(def patterns {:cie #"cie" :ie #"(?<!c)ie" :cei #"cei" :ei #"(?<!c)ei"})

(defn update-counts
  "Given a map of counts of matching patterns and a word, increment any count if the word matches it's pattern."
  [counts [word freq]]
  (apply hash-map (mapcat (fn [[k v]] [k (if (re-seq (patterns k) word) (+ freq v) v)]) counts)))

(defn count-ie-ei-combinations
  "Update counts of all ie and ei combinations"
  [words]
  (reduce update-counts {:ie 0 :cie 0 :ei 0 :cei 0} words))

(defn apply-freq-1
  "Apply a frequency of one to words"
  [words]
  (map #(vector % 1) words))

(defn- format-plausible
  [plausible?]
  (if plausible? "plausible" "implausible"))

(defn- apply-rule [desc examples contra]
  (let [plausible? (<= (* 2 contra) examples)]
    (println (format "The sub rule %s is %s. There are %d examples and %d counter-examples.\n" desc (format-plausible plausible?) examples contra))
    plausible?))

(defn i-before-e-except-after-c-plausible?
  "Check if i before e after c plausible?"
  [description words]
  (do
    (println description)
    (let [counts (count-ie-ei-combinations words)
          subrule1 (apply-rule "I before E when not preceeded by C" (:ie counts) (:ei counts))
          subrule2 (apply-rule "E before I when preceeded by C" (:cei counts) (:cie counts))
          rule (and subrule1 subrule2)]
      (println (format "Overall the rule 'I before E except after C' is %s" (format-plausible rule)))
      rule)))

(defn format-freq-line [line] (letfn [(format-line [xs] [(first xs) (read-string (last xs))])]
                                       (-> line
                                           s/trim
                                           (s/split #"\s")
                                           format-line)))

(defn -main []
  (with-open [rdr (clojure.java.io/reader "http://wiki.puzzlers.org/pub/wordlists/unixdict.txt")]
   (i-before-e-except-after-c-plausible? "Check unixdist list" (apply-freq-1 (line-seq rdr))))
  (with-open [rdr (clojure.java.io/reader "http://ucrel.lancs.ac.uk/bncfreq/lists/1_2_all_freq.txt")]
   (i-before-e-except-after-c-plausible? "Word frequencies (stretch goal)" (map format-freq-line (drop 1 (line-seq rdr))))))

```


{{out}}

```txt

lein run
Check unixdist list
The sub rule I before E when not preceeded by C is plausible. There are 465 examples and 213 counter-examples.

The sub rule E before I when preceeded by C is implausible. There are 13 examples and 24 counter-examples.

Overall the rule 'I before E except after C' is implausible
Word frequencies (stretch goal)
The sub rule I before E when not preceeded by C is implausible. There are 8192 examples and 4826 counter-examples.

The sub rule E before I when preceeded by C is implausible. There are 327 examples and 994 counter-examples.

Overall the rule 'I before E except after C' is implausible

```



## Coco


First we need to set the variable <code>dict</code> to the text of the dictionary as a string. How to do this depends on your JavaScript platform. Using Node.js, for example, you could download a copy of the dictionary to <code>/tmp/unixdict.txt</code> and then say <code>dict = fs.readFileSync '/tmp/unixdict.txt', {encoding: 'UTF-8'}</code>.

Now we can do the task:


```coco
ie-npc = ei-npc = ie-pc = ei-pc = 0
for word of dict.toLowerCase!.match /\S+/g
    ++ie-npc if /(^|[^c])ie/.test word
    ++ei-npc if /(^|[^c])ei/.test word
    ++ie-pc if word.indexOf('cie') > -1
    ++ei-pc if word.indexOf('cei') > -1

p1 = ie-npc > 2 * ei-npc
p2 = ei-pc > 2 * ie-pc

console.log '(1) is%s plausible.', if p1 then '' else ' not'
console.log '(2) is%s plausible.', if p2 then '' else ' not'
console.log 'The whole phrase is%s plausible.', if p1 and p2 then '' else ' not'
```



## Common Lisp



```lisp

(defun test-rule (rule-name examples counter-examples)
  (let ((plausible (if (> examples (* 2 counter-examples)) 'plausible 'not-plausible)))
    (list rule-name plausible examples counter-examples)))

(defun plausibility (result-string file parser)
  (let ((cei 0) (cie 0) (ie 0) (ei 0))
    (macrolet ((search-count (&rest terms)
                 (when terms
                   `(progn
                      (when (search ,(string-downcase (symbol-name (car terms))) word)
                        (incf ,(car terms) freq))
                      (search-count ,@(cdr terms))))))
      (with-open-file (stream file :external-format :latin-1)
        (loop :for raw-line = (read-line stream nil 'eof)
              :until (eq raw-line 'eof)
              :for line = (string-trim '(#\Tab #\Space) raw-line)
              :for (word freq) = (funcall parser line)
              :do (search-count cei cie ie ei))
        (print-result result-string cei cie ie ei)))))

(defun print-result (result-string cei cie ie ei)
  (let ((results (list (test-rule "I before E when not preceded by C" (- ie cie) (- ei cei))
                       (test-rule "E before I when preceded by C" cei cie))))
    (format t "~a:~%~{~{~2TThe rule \"~a\" is ~S. There were ~a examples and ~a counter-examples.~}~^~%~}~%~%~2TOverall the rule is ~S~%~%"
            result-string results (or (find 'not-plausible (mapcar #'cadr results)) 'plausible))))

(defun parse-dict (line) (list line 1))

(defun parse-freq (line)
  (list (subseq line 0 (position #\Tab line))
        (parse-integer (subseq line (position #\Tab line :from-end t)) :junk-allowed t)))

(plausibility "Dictionary" #p"unixdict.txt" #'parse-dict)
(plausibility "Word frequencies (stretch goal)" #p"1_2_all_freq.txt" #'parse-freq)

```


{{out}}

```txt
Dictionary:
  The rule "I before E when not preceded by C" is PLAUSIBLE. There were 465 examples and 213 counter-examples.
  The rule "E before I when preceded by C" is NOT-PLAUSIBLE. There were 13 examples and 24 counter-examples.

  Overall the rule is NOT-PLAUSIBLE

Word frequencies (stretch goal):
  The rule "I before E when not preceded by C" is NOT-PLAUSIBLE. There were 8163 examples and 4826 counter-examples.
  The rule "E before I when preceded by C" is NOT-PLAUSIBLE. There were 327 examples and 994 counter-examples.

  Overall the rule is NOT-PLAUSIBLE

```



## D

The extra work has not been attempted

```D
import std.file;
import std.stdio;

int main(string[] args) {
    if (args.length < 2) {
        stderr.writeln(args[0], " filename");
        return 1;
    }

    int cei, cie, ie, ei;
    auto file = File(args[1]);
    foreach(line; file.byLine) {
        auto res = eval(cast(string) line);
        cei += res.cei;
        cie += res.cie;
        ei += res.ei;
        ie += res.ie;
    }

    writeln("CEI: ", cei, "; CIE: ", cie);
    writeln("EI: ", ei, "; IE: ", ie);

    writeln("'I before E when not preceded by C' is ", verdict(ie, ei));
    writeln("'E before I when preceded by C' is ", verdict(cei, cie));

    return 0;
}

string verdict(int a, int b) {
    import std.format;
    if (a > 2*b) {
        return format("plausible with evidence %f", cast(double)a/b);
    }
    return format("not plausible with evidence %f", cast(double)a/b);
}

struct Evidence {
    int cei;
    int cie;
    int ei;
    int ie;
}

Evidence eval(string word) {
    enum State {
        START,
        C,
        E,
        I,
        CE,
        CI,
    }

    State state;
    Evidence cnt;
    for(int i=0; i<word.length; ++i) {
        char c = word[i];
        switch(state) {
            case State.START:
                if (c == 'c') {
                    state = State.C;
                }
                if (c == 'e') {
                    state = State.E;
                }
                if (c == 'i') {
                    state = State.I;
                }
                break;
            case State.C:
                if (c == 'e') {
                    state = State.CE;
                } else if (c == 'i') {
                    state = State.CI;
                } else if (c != 'c') {
                    state = State.START;
                }
                break;
            case State.E:
                if (c == 'c') {
                    state = State.C;
                } else if (c == 'i') {
                    cnt.ei++;
                    state = State.I;
                } else if (c != 'e') {
                    state = State.START;
                }
                break;
            case State.I:
                if (c == 'c') {
                    state = State.C;
                } else if (c == 'e') {
                    cnt.ie++;
                    state = State.E;
                } else if (c != 'i') {
                    state = State.START;
                }
                break;
            case State.CE:
                if (c == 'i') {
                    cnt.cei++;
                    state = State.I;
                }
                if (c == 'c') {
                    state = State.C;
                }
                state = State.START;
                break;
            case State.CI:
                if (c == 'e') {
                    cnt.cie++;
                    state = State.E;
                }
                if (c == 'c') {
                    state = State.C;
                }
                state = State.START;
                break;
            default:
                assert(0);
        }
    }
    return cnt;
}
```


{{out}}

```txt
CEI: 13; CIE: 24
EI: 217; IE: 466
'I before E when not preceded by C' is plausible with evidence 2.147465
'E before I when preceded by C' is not plausible with evidence 0.541667
```



## Elixir

{{trans|Ruby}}

```elixir
defmodule RC do
  def task(path) do
    plausibility_ratio = 2
    rules = [ {"I before E when not preceded by C:", "ie", "ei"},
              {"E before I when preceded by C:", "cei", "cie"} ]
    regex = ~r/ie|ei|cie|cei/
    counter = File.read!(path) |> countup(regex)
    Enum.all?(rules, fn {str, x, y} ->
      nx = counter[x]
      ny = counter[y]
      ratio = nx / ny
      plausibility = if ratio > plausibility_ratio, do: "Plausible", else: "Implausible"
      IO.puts str
      IO.puts "  #{x}: #{nx}; #{y}: #{ny}; Ratio: #{Float.round(ratio,3)}: #{plausibility}"
      ratio > plausibility_ratio
    end)
  end

  def countup(binary, regex) do
    String.split(binary)
    |> Enum.reduce(Map.new, fn word,acc ->
         if match = Regex.run(regex, word),
             do: Dict.update(acc, hd(match), 1, &(&1+1)), else: acc
       end)
  end
end

path = hd(System.argv)
IO.inspect RC.task(path)
```


{{out}}

```txt

C:\Elixir>elixir test.exs \work\unixdict.txt
I before E when not preceded by C:
  ie: 462; ei: 212; Ratio: 2.179: Plausible
E before I when preceded by C:
  cei: 13; cie: 24; Ratio: 0.542: Implausible
false

```



## Erlang


```erlang

-module(cei).
-export([plaus/0,count/3]).

plaus() ->
    {ok,Words} = file:read_file("unixdict.txt"),
    Swords = string:tokens(erlang:binary_to_list(Words), "\n"),
    EiF = count(Swords,"[^c]ei",0),
    IeF = count(Swords,"[^c]ie",0),
    CeiF = count(Swords,"cei",0),
    CieF = count(Swords,"cie",0),
    if CeiF >= 2 * CieF -> P1= 'is'; true -> P1 = 'is not' end,
    if IeF >= 2 * EiF -> P2 = 'is'; true -> P2 = 'is not' end,
    if P1 == 'is' andalso p2 == 'is' -> P3 ='is'; true -> P3 = 'is not' end,
    io:format("Proposition 1. ~w plausible: ie ~w, ei ~w~n", [P2,IeF,EiF]),
    io:format("Proposition 2. ~w plausible: cei ~w, cie ~w~n", [P1,CeiF,CieF]),
    io:format("The rule ~w plausible~n", [P3]).

count(List,Pattern,Acc) when length(List) == 0 -> Acc;
count(List,Pattern,Acc) ->
    [H|T] = List,
    case re:run(H,Pattern,[global,{capture,none}]) of
        match -> count(T,Pattern, Acc + 1);
        nomatch -> count(T,Pattern, Acc)
    end.

```

{{output}}

```txt

69> cei:plaus().
Proposition 1. is plausible: ie 464, ei 194
Proposition 2. is not plausible: cei 13, cie 24
The rule 'is not' plausible

```



## Factor


```factor
USING: combinators formatting generalizations io.encodings.utf8
io.files kernel literals math prettyprint regexp sequences ;
IN: rosetta-code.i-before-e

: correct ( #correct #incorrect rule-str -- )
    pprint " is correct for %d and incorrect for %d.\n" printf ;

: plausibility ( #correct #incorrect -- str )
    2 * > "plausible" "implausible" ? ;

: output ( #correct #incorrect rule-str -- )
    [ correct ] curry
    [ plausibility "This is %s.\n\n" printf ] 2bi ;

"unixdict.txt" utf8 file-lines ${
    R/ cei/ R/ cie/ R/ [^c]ie/ R/ [^c]ei/
    [ count-matches ]
    [ map-sum       ]
    [ 4 apply-curry ] bi@
} cleave

"I before E when not preceded by C"
"E before I when preceded by C" [ output ] bi@
```

{{out}}

```txt

"I before E when not preceded by C" is correct for 465 and incorrect for 195.
This is plausible.

"E before I when preceded by C" is correct for 13 and incorrect for 24.
This is implausible.

```



## Fortran

Please find the linux build instructions along with example run in the comments at the beginning of the f90 source.  Thank you.

```FORTRAN

!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Sat May 18 22:19:19
!
!a=./F && make $a && $a < unixdict.txt
!f95 -Wall -ffree-form F.F -o F
!   ie   ei  cie  cei
!  490  230   24   13
!         [^c]ie plausible
!            cei implausible
! ([^c]ie)|(cei) implausible
!
!Compilation finished at Sat May 18 22:19:19

! test the plausibility of i before e except...
program cia
  implicit none
  character (len=256) :: s
  integer :: ie, ei, cie, cei
  integer :: ios
  data ie, ei, cie, cei/4*0/
  do while (.true.)
    read(5,*,iostat = ios)s
    if (0 .ne. ios) then
      exit
    endif
    call lower_case(s)
    cie = cie + occurrences(s, 'cie')
    cei = cei + occurrences(s, 'cei')
    ie = ie + occurrences(s, 'ie')
    ei = ei + occurrences(s, 'ei')
  enddo
  write(6,'(1x,4(a4,1x))') 'ie','ei','cie','cei'
  write(6,'(1x,4(i4,1x))') ie,ei,cie,cei ! 488 230 24 13
  write(6,'(1x,2(a,1x))') '        [^c]ie',plausibility(ie,ei)
  write(6,'(1x,2(a,1x))') '           cei',plausibility(cei,cie)
  write(6,'(1x,2(a,1x))') '([^c]ie)|(cei)',plausibility(ie+cei,ei+cie)

contains

  subroutine lower_case(s)
    character(len=*), intent(inout) :: s
    integer :: i
    do i=1, len_trim(s)
      s(i:i) = achar(ior(iachar(s(i:i)),32))
    enddo
  end subroutine lower_case

  integer function occurrences(a,b)
    character(len=*), intent(in) :: a, b
    integer :: i, j, n
    n = 0
    i = 0
    j = index(a, b)
    do while (0 .lt. j)
      n = n+1
      i = i+len(b)+j-1
      j = index(a(i:), b)
    end do
    occurrences = n
  end function occurrences

  character*(32) function plausibility(da, nyet)
    integer, intent(in) :: da, nyet
    !write(0,*)da,nyet
    if (nyet*2 .lt. da) then
      plausibility = 'plausible'
    else
      plausibility = 'implausible'
    endif
  end function plausibility
end program cia

```



## FreeBASIC


```FreeBASIC
Function getfile(file As String) As String
    Dim As Integer F = Freefile
    Dim As String text,intext
    Open file For Input As #F
    Line Input #F,text
    While Not Eof(F)
        Line Input #F,intext
        text=text+Chr(10)+intext
    Wend
    close #F
    Return text
End Function

Function TALLY(instring As String,PartString As String) As Integer
        Dim count As Integer
        var lens2=Len(PartString)
        Dim As String s=instring
        Dim As Integer position=Instr(s,PartString)
        If position=0 Then Return 0
        While position>0
            count=count+1
            position=Instr(position+Lens2,s,PartString)
        Wend
        Function=count
    End Function

Dim As String myfile="unixdict.txt"

Dim As String wordlist= getfile(myfile)
wordlist=lcase(wordlist)

print
print "The number of words in unixdict.txt  ",TALLY(wordlist,chr(10))+1
print
dim as integer cei=TALLY(wordlist,"cei")
print "Instances of cei",cei
dim as integer cie=TALLY(wordlist,"cie")
print "Instances of cie",cie
print
dim as integer ei=TALLY(wordlist,"ei")
print "Instances of *ei, where * is not c",ei-cei
dim as integer ie=TALLY(wordlist,"ie")
print "Instances of *ie, where * is not c",ie-cie
print
print "Conclusion:"
print "ie is plausible when not preceeded by c, the ratio is ";(ie-cie)/(ei-cei)
print "ei is not plausible when preceeded by c, the ratio is ";cei/cie
print "So, the idea is not plausible."

Sleep
```

{{out}}

```txt
The number of words in unixdict.txt        25104

Instances of cei             13
Instances of cie             24

Instances of *ei, where * is not c         217
Instances of *ie, where * is not c         466

Conclusion:
ie is plausible when not preceeded by c, the ratio is  2.147465437788018
ei is not plausible when preceeded by c, the ratio is  0.5416666666666666
So, the idea is not plausible.
```



## Go


```go
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strings"
)

func main() {
	f, err := os.Open("unixdict.txt")
	if err != nil {
		log.Fatalln(err)
	}
	defer f.Close()

	s := bufio.NewScanner(f)
	rie := regexp.MustCompile("^ie|[^c]ie")
	rei := regexp.MustCompile("^ei|[^c]ei")
	var cie, ie int
	var cei, ei int
	for s.Scan() {
		line := s.Text()
		if strings.Contains(line, "cie") {
			cie++
		}
		if strings.Contains(line, "cei") {
			cei++
		}
		if rie.MatchString(line) {
			ie++
		}
		if rei.MatchString(line) {
			ei++
		}
	}
	err = s.Err()
	if err != nil {
		log.Fatalln(err)
	}

	if check(ie, ei, "I before E when not preceded by C") &&
		check(cei, cie, "E before I when preceded by C") {
		fmt.Println("Both plausable.")
		fmt.Println(`"I before E, except after C" is plausable.`)
	} else {
		fmt.Println("One or both implausable.")
		fmt.Println(`"I before E, except after C" is implausable.`)
	}
}

// check checks if a statement is plausible. Something is plausible if a is more
// than two times b.
func check(a, b int, s string) bool {
	switch {
	case a > b*2:
		fmt.Printf("%q is plausible (%d vs %d).\n", s, a, b)
		return true
	case a >= b:
		fmt.Printf("%q is implausible (%d vs %d).\n", s, a, b)
	default:
		fmt.Printf("%q is implausible and contra-indicated (%d vs %d).\n",
			s, a, b)
	}
	return false
}
```


{{out}}

```txt
"I before E when not preceded by C" is plausible (465 vs 213).
"E before I when preceded by C" is implausible and contra-indicated (13 vs 24).
One or both implausable.
"I before E, except after C" is implausable.

```



## Haskell

Using Regular Expressions, you can quickly count all occurrences of words that follow this rule and words that don't. In this solution, TDFA -- a fast, POSIX ERE engine -- was used. However, substituting any other regex engine for TDFA should only require changing the import statement. See [http://www.haskell.org/haskellwiki/Regular_expressions this page] for a list of the most common regex engines available in Haskell.

This solution does not attempt the stretch goal.


```Haskell
import Network.HTTP
import Text.Regex.TDFA
import Text.Printf

getWordList :: IO String
getWordList  =  do
    response  <-  simpleHTTP.getRequest$ url
    getResponseBody response
        where url = "http://wiki.puzzlers.org/pub/wordlists/unixdict.txt"

main = do
    words <- getWordList
    putStrLn "Checking Rule 1: \"I before E when not preceded by C\"..."
    let numTrueRule1   =  matchCount (makeRegex "[^c]ie" :: Regex) words
        numFalseRule1  =  matchCount (makeRegex "[^c]ei" :: Regex) words
        rule1Plausible  =  numTrueRule1 > (2*numFalseRule1)
    printf "Rule 1 is correct for %d\n        incorrect for %d\n" numTrueRule1 numFalseRule1
    printf "*** Rule 1 is %splausible.\n" (if rule1Plausible then "" else "im")

    putStrLn "Checking Rule 2: \"E before I when preceded by C\"..."
    let numTrueRule2   =  matchCount (makeRegex "cei" :: Regex) words
        numFalseRule2  =  matchCount (makeRegex "cie" :: Regex) words
        rule2Plausible  =  numTrueRule2 > (2*numFalseRule2)
    printf "Rule 2 is correct for %d\n        incorrect for %d\n" numTrueRule2 numFalseRule2
    printf "*** Rule 2 is %splausible.\n" (if rule2Plausible then "" else "im")
```


{{out}}

```txt

Checking Rule 1: "I before E when not preceded by C"...
Rule 1 is correct for 465
        incorrect for 195
*** Rule 1 is plausible.
Checking Rule 2: "E before I when preceded by C"...
Rule 2 is correct for 13
        incorrect for 24
*** Rule 2 is implausible.

```


=={{header|Icon}} and {{header|Unicon}}==

This solution only works in Unicon, but wouldn't be hard to adapt to Icon.
Assumes that words that start with "ei" violate "i before e except after
c" and that occurrences of "ei" and "ie" that occur multiple times in the
same input line should all be tested.


```Unicon
import Utils		# To get the FindFirst class

procedure main(a)
    showCounts := "--showcounts" == !a
    totals := table(0)
    phrases := ["cei","cie","ei","ie"]  # Longer phrases first
    ff := FindFirst(phrases)

    every map(!&input) ?
        while totals[2(tab(ff.locate()), ff.moveMatch(), move(-1))] +:= 1

    eiP := totals["cei"] > 2* totals["cie"]
    ieP := (totals["ie"]+totals["cei"]) > 2* totals["ei"]
    write("phrase is ",((\ieP & \eiP),"plausible")|"not plausible")
    write("ie is ",(\ieP,"plausible")|"not plausible")
    write("ei is ",(\eiP,"plausible")|"not plausible")

    if \showCounts then every write(phrase := !phrases,": ",totals[phrase])
end
```


{{out}} of running with <tt>--showcounts</tt> flag:

```txt

-> ei --showcounts <unixdict.txt
phrase is not plausible
ie is plausible
ei is not plausible
cei: 13
cie: 24
ei: 217
ie: 466
->

```



###  stretch goal



```Unicon
import Utils		# To get the FindFirst class

procedure main(a)
    WS := " \t"
    showCounts := "--showcounts" == !a
    phrases := ["cei","cie","ei","ie"]
    ff := FindFirst(phrases)
    totals := table(0)

    every map(!&input) ? {
        w := (tab(many(WS)),tab(upto(WS)))             # word
        (tab(many(WS)),tab(upto(WS)))                  # Skip part of speech
        n := integer((tab(many(WS)),tab(upto(WS)|0))) | next   # frequency?

        \w ? while totals[2(tab(ff.locate()), ff.moveMatch(), move(-1))] +:= n
        }

    eiP := totals["cei"] > 2* totals["cie"]
    ieP := (totals["ie"]+totals["cei"]) > 2* totals["ei"]
    write("phrase is ",((\ieP & \eiP),"plausible")|"not plausible")
    write("ie is ",(\ieP,"plausible")|"not plausible")
    write("ei is ",(\eiP,"plausible")|"not plausible")

    if \showCounts then every write(phrase := !phrases,": ",totals[phrase])
end
```


{{out}}

```txt

->ei2 --showcounts <1_2*txt
phrase is not plausible
ie is not plausible
ei is not plausible
cei: 327
cie: 994
ei: 4826
ie: 8207
->

```



## J


After downloading unixdict to /tmp:


```J
   dict=:tolower fread '/tmp/unixdict.txt'
```


Investigating the rules:


```J
   +/'cie' E. dict
24
   +/'cei' E. dict
13
   +/'ie' E. dict
490
   +/'ei' E. dict
230
```


So, based on unixdict.txt, the "I before E" rule seems plausible (490 > 230 by more than a factor of 2), but the exception does not make much sense (we see almost twice as many i before e after a c as we see e before i after a c).

Note that if we looked at frequency of use for words, instead of considering all words to have equal weights, we might come up with a different answer.


###  stretch goal


After downloading 1_2_all_freq to /tmp, we can read it into J, and break out the first column (as words) and the third column as numbers:


```J
allfreq=: |:}.<;._1;._2]1!:1<'/tmp/1_2_all_freq.txt'

words=:         >0 { allfreq
freqs=: 0 {.@".&>2 { allfreq
```


With these definitions, we can define a prevalence verb which will tell us how often a particular substring is appears in use:


```J
prevalence=:verb define
  (y +./@E."1 words) +/ .* freqs
)
```


Investigating our original proposed rules:


```J
   'ie' %&prevalence 'ei'
1.76868
```


A generic "i before e" rule is not looking quite as good now - words that have i before e are used less than twice as much as words which use e before i.


```J
   'cei' %&prevalence 'cie'
0.328974
```


An "except after c" variant is looking awful now - words that use the cie sequence are three times as likely as words that use the cei sequence.  So, of course, if we modified our original rule with this exception it would weaken the original rule:


```J
   ('ie' -&prevalence 'cie') % ('ei' -&prevalence 'cei')
1.68255
```


Note that we might also want to consider non-adjacent matches (the regular expression 'i.*e' instead of 'ie' or perhaps 'c.*ie' or 'c.*i.*e' instead of 'cie') - this would be straightforward to check, but this would bulk up the page.

## Java

Download and save wordlist to unixdict.txt.


```java

import java.io.BufferedReader;
import java.io.FileReader;

public class IbeforeE
{
	public static void main(String[] args)
	{
		IbeforeE now=new IbeforeE();
		String wordlist="unixdict.txt";
		if(now.isPlausibleRule(wordlist))
			System.out.println("Rule is plausible.");
		else
			System.out.println("Rule is not plausible.");
	}
	boolean isPlausibleRule(String filename)
	{
		int truecount=0,falsecount=0;
		try
		{
			BufferedReader br=new BufferedReader(new FileReader(filename));
			String word;
			while((word=br.readLine())!=null)
			{
				if(isPlausibleWord(word))
					truecount++;
				else if(isOppPlausibleWord(word))
					falsecount++;
			}
			br.close();
		}
		catch(Exception e)
		{
			System.out.println("Something went horribly wrong: "+e.getMessage());
		}

		System.out.println("Plausible count: "+truecount);
		System.out.println("Implausible count: "+falsecount);
		if(truecount>2*falsecount)
			return true;
		return false;
	}
	boolean isPlausibleWord(String word)
	{
		if(!word.contains("c")&&word.contains("ie"))
			return true;
		else if(word.contains("cei"))
			return true;
		return false;
	}
	boolean isOppPlausibleWord(String word)
	{
		if(!word.contains("c")&&word.contains("ei"))
			return true;
		else if(word.contains("cie"))
			return true;
		return false;
	}
}

```


{{out}}

```txt
Plausible count: 384
Implausible count: 204
Rule is not plausible.
```




## jq

{{works with|jq|with regex support}}

WARNING: The problem statement is misleading as the rule only applies to syllables that rhyme with "see".

```jq
def plausibility_ratio: 2;

# scan/2 produces a stream of matches but the first match of a segment (e.g. cie)
# blocks further matches with that segment, and therefore if scan produces "ie",
# it was NOT preceded by "c".
def dictionary:
  reduce .[] as $word
    ( {};
      reduce ($word | scan("ie|ei|cie|cei")) as $found ( .; .[$found] += 1 ));

def rules:
  { "I before E when not preceded by C": ["ie",  "ei"],
    "E before I when preceded by C":     ["cei", "cie"]
   };

# Round to nearest integer or else "round-up"
def round:
  if . < 0 then (-1 * ((- .) | round) | if . == -0 then 0 else . end)
  else floor as $x | if (. - $x) < 0.5 then $x else $x+1 end
  end;

def assess:
  (split("\n") | dictionary) as $dictionary
  | rules as $rules
  | ($rules | keys[]) as $key
  | $rules[$key] as $fragments
  | $dictionary[$fragments[0]] as $x
  | $dictionary[$fragments[1]] as $y
  | ($x / $y) as $ratio
  | (if $ratio > plausibility_ratio then "plausible"
     else "implausible" end) as $plausibility
  | " -- the rule \"\($key)\" is \($plausibility)
    as ratio = \($x)/\($y) ~ \($ratio * 100 |round)%"  ;

"Using the problematic criterion specified in the task requirements:", assess
```

{{out}}
Using http://www.puzzlers.org/pub/wordlists/unixdict.txt as of June 2015:

```sh
$ jq -s -R -r -f I_before_E_except_after_C.jq unixdict.txt
Using the problematic criterion specified in the task requirements:
 -- the rule "E before I when preceded by C" is implausible
    as ratio = 13/24 ~ 54%
 -- the rule "I before E when not preceded by C" is plausible
    as ratio = 464/217 ~ 214%
```



## Julia


```julia
# v0.0.6

open("unixdict.txt") do txtfile
    rule1, notrule1, rule2, notrule2 = 0, 0, 0, 0
    for word in eachline(txtfile)
        # "I before E when not preceded by C"
        if ismatch(r"ie"i, word)
            if ismatch(r"cie"i, word)
                notrule1 += 1
            else
                rule1 += 1
            end
        end
        # "E before I when preceded by C"
        if ismatch(r"ei"i, word)
            if ismatch(r"cei"i, word)
                rule2 += 1
            else
                notrule2 += 1
            end
        end
    end

    print("Plausibility of \"I before E when not preceded by C\": ")
    println(rule1 > 2 * notrule1 ? "PLAUSIBLE" : "UNPLAUSIBLE")
    print("Plausibility of \"E before I when preceded by C\":")
    println(rule2 > 2 * notrule2 ? "PLAUSIBLE" : "UNPLAUSIBLE")
end
```


{{out}}

```txt
Plausibility of "I before E when not preceded by C": PLAUSIBLE
Plausibility of "E before I when preceded by C":UNPLAUSIBLE
```



## Kotlin


```scala
// version 1.0.6

import java.net.URL
import java.io.InputStreamReader
import java.io.BufferedReader

fun isPlausible(n1: Int, n2: Int) = n1 > 2 * n2

fun printResults(source: String, counts: IntArray) {
    println("Results for $source")
    println("  i before e except after c")
    println("    for     ${counts[0]}")
    println("    against ${counts[1]}")
    val plausible1 = isPlausible(counts[0], counts[1])
    println("  sub-rule is${if (plausible1) "" else " not"} plausible\n")
    println("  e before i when preceded by c")
    println("    for     ${counts[2]}")
    println("    against ${counts[3]}")
    val plausible2 = isPlausible(counts[2], counts[3])
    println("  sub-rule is${if (plausible2) "" else " not"} plausible\n")
    val plausible = plausible1 && plausible2
    println("  rule is${if (plausible) "" else " not"} plausible")
}

fun main(args: Array<String>) {
    val url = URL("http://wiki.puzzlers.org/pub/wordlists/unixdict.txt")
    val isr = InputStreamReader(url.openStream())
    val reader = BufferedReader(isr)
    val regexes = arrayOf(
        Regex("(^|[^c])ie"),     // i before e when not preceded by c (includes words starting with ie)
        Regex("(^|[^c])ei"),     // e before i when not preceded by c (includes words starting with ei)
        Regex("cei"),            // e before i when preceded by c
        Regex("cie")             // i before e when preceded by c
    )
    val counts = IntArray(4) // corresponding counts of occurrences
    var word = reader.readLine()
    while (word != null) {
        for (i in 0..3) counts[i] += regexes[i].findAll(word).toList().size
        word = reader.readLine()
    }
    reader.close()
    printResults("unixdict.txt", counts)

    val url2 = URL("http://ucrel.lancs.ac.uk/bncfreq/lists/1_2_all_freq.txt")
    val isr2 = InputStreamReader(url2.openStream())
    val reader2 = BufferedReader(isr2)
    val counts2 = IntArray(4)
    reader2.readLine() // read header line
    var line = reader2.readLine() // read first line and store it
    var words: List<String>
    val splitter = Regex("""(\t+|\s+)""")
    while (line != null) {
        words = line.split(splitter)
        if (words.size == 4)  // first element is empty
            for (i in 0..3) counts2[i] += regexes[i].findAll(words[1]).toList().size * words[3].toInt()
        line = reader2.readLine()
    }
    reader2.close()
    println()
    printResults("British National Corpus", counts2)
}
```


{{out}}

```txt

Results for unixdict.txt
  i before e except after c
    for     466
    against 217
  sub-rule is plausible

  e before i when preceded by c
    for     13
    against 24
  sub-rule is not plausible

  rule is not plausible

Results for British National Corpus
  i before e except after c
    for     8192
    against 4826
  sub-rule is not plausible

  e before i when preceded by c
    for     327
    against 994
  sub-rule is not plausible

  rule is not plausible

```



## Lasso


```lasso

local(cie,cei,ie,ei) = (:0,0,0,0)

local(match_ie) = regExp(`[^c]ie`)
local(match_ei) = regExp(`[^c]ei`)

with word in include_url(`http://wiki.puzzlers.org/pub/wordlists/unixdict.txt`)->asString->split("\n")
where #word >> `ie` or #word >> `ei`
do {
    #word >> `cie`
        ? #cie++
    #word >> `cei`
        ? #cei++

    #match_ie->reset(-input=#word, -ignoreCase)&find
        ? #ie++
    #match_ei->reset(-input=#word, -ignoreCase)&find
        ? #ei++
}

local(ie_plausible)  = (#ie  >= (2 * #ei))
local(cei_plausible) = (#cei >= (2 * #cie))

stdoutnl(
    `The rule "I before E when not preceded by C" is ` +
    (#ie_plausible ? '' | 'NOT-') + `PLAUSIBLE. There were ` +
    #ie + ` examples and ` + #ei + ` counter-examples.`
)
stdoutnl(
    `The rule "E before I when preceded by C" is ` +
    (#cei_plausible ? `` | `NOT-`) + `PLAUSIBLE. There were ` +
    #cei + ` examples and ` + #cie + ` counter-examples.`
)
stdoutnl(`Overall the rule is ` + (#ie_plausible and #cei_plausible ? `` | `NOT-`) + `PLAUSIBLE`)

```

{{out}}

```txt

The rule "I before E when not preceded by C" is PLAUSIBLE. There were 464 examples and 194 counter-examples.
The rule "E before I when preceded by C" is NOT-PLAUSIBLE. There were 13 examples and 24 counter-examples.
Overall the rule is NOT-PLAUSIBLE

```



## Lua


```Lua
-- Needed to get dictionary file from web server
local http = require("socket.http")

-- Return count of words that contain pattern
function count (pattern, wordList)
    local total = 0
    for word in wordList:gmatch("%S+") do
        if word:match(pattern) then total = total + 1 end
    end
    return total
end

-- Check plausibility of case given its opposite
function plaus (case, opposite, words)
    if count(case, words) > 2 * count(opposite, words) then
        print("PLAUSIBLE")
        return true
    else
        print("IMPLAUSIBLE")
        return false
    end
end

-- Main procedure
local page = http.request("http://wiki.puzzlers.org/pub/wordlists/unixdict.txt")
io.write("I before E when not preceded by C: ")
local sub1 = plaus("[^c]ie", "cie", page)
io.write("E before I when preceded by C: ")
local sub2 = plaus("cei", "[^c]ei", page)
io.write("Overall the phrase is ")
if not (sub1 and sub2) then io.write("not ") end
print("plausible.")
```

{{out}}

```txt
I before E when not preceded by C: PLAUSIBLE
E before I when preceded by C: IMPLAUSIBLE
Overall the phrase is not plausible.
```



## Maple


```Maple
words:= HTTP:-Get("http://wiki.puzzlers.org/pub/wordlists/unixdict.txt"):
lst := StringTools:-Split(words[2],"\n"):
xie, cie, cei, xei := 0, 0, 0, 0:
for item in lst do
	if searchtext("ie", item) <> 0 then
		if searchtext("cie", item) <> 0 then
			cie := cie + 1:
		else
			xie := xie + 1:
		fi:
	fi:
	if searchtext("ei", item) <> 0 then
		if searchtext("cei", item) <> 0 then
			cei := cei + 1:
		else
			xei := xei + 1:
		fi:
	fi:
od:
p1, p2 := evalb(xie > 2*xei),evalb(cei > 2*cie);
printf("The first phrase is %s with supporting features %d, anti features %d\n", piecewise(p1, "plausible", "not plausible"), xie, xei);
printf("The seond phrase is %s with supporting features %d, anti features %d\n", piecewise(p2, "plausible", "not plausible"), cei, cie);
printf("The overall phrase is %s\n", piecewise(p1 and p2, "plausible", "not plausible")):
```

{{Out|Output}}

```txt
The first phrase is plausible with supporting features 465 and anti features 213
The second phrase is not plausible with supporting features 13 and anti features 24
The overall phrase is not plausible
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
wordlist =
  Import["http://wiki.puzzlers.org/pub/wordlists/unixdict.txt",
   "Words"];
Print["The number of words in unixdict.txt = " <>
  ToString[Length[wordlist]]]
StringMatchQ[#, ___ ~~ "c" ~~ "i" ~~ "e" ~~ ___] & /@ wordlist ;
cie = Count[%, True];
StringMatchQ[#, ___ ~~ "c" ~~ "e" ~~ "i" ~~ ___] & /@ wordlist ;
cei = Count[%, True];
StringMatchQ[#, ___ ~~ "i" ~~ "e" ~~ ___] & /@ wordlist ;
ie = Count[%, True] - cie;
StringMatchQ[#, ___ ~~ "e" ~~ "i" ~~ ___] & /@ wordlist ;
ei = Count[%, True] - cei;
test1 = ie > 2 ei;
Print["The rule \"I before E when not preceded by C\" is " <>
  If[test1, "PLAUSIBLE", "NOT PLAUSIBLE"]]
Print["There were " <> ToString[ie] <> " examples and " <>
  ToString[ei]  <> " counter examples, for a ratio of " <>
  ToString[N[ie/ei]]]
test2 = cei > 2 cie;
Print["The rule \"E before I when preceded by C\" is " <>
  If[test2, "PLAUSIBLE", "NOT PLAUSIBLE"]]
Print["There were " <> ToString[cei] <> " examples and " <>
  ToString[cie]  <> " counter examples, for a ratio of " <>
  ToString[N[cei/cie]]]
Print["Overall the rule is " <>
  If[test1 && test2, "PLAUSIBLE", "NOT PLAUSIBLE" ]]
```


{{out}}

```mathematica
The number of words in unixdict.txt = 25104
The rule "I before E when not preceded by C" is PLAUSIBLE
There were 465 examples and 213 counter examples, for a ratio of 2.1831
The rule "E before I when preceded by C" is NOT PLAUSIBLE
There were 13 examples and 24 counter examples, for a ratio of 0.541667
Overall the rule is NOT PLAUSIBLE

```


=={{header|MATLAB}} / {{header|Octave}}==
{{incomplete|MATLAB|Is the original phrase plausible?}}


```MATLAB
function i_before_e_except_after_c(f)

fid = fopen(f,'r');
nei = 0;
cei = 0;
nie = 0;
cie = 0;
while ~feof(fid)
	c = strsplit(strtrim(fgetl(fid)),char([9,32]));
	if length(c) > 2,
		n = str2num(c{3});
	else
		n = 1;
	end;
	if strfind(c{1},'ei')>1, nei=nei+n; end;
	if strfind(c{1},'cei'),  cei=cei+n; end;
	if strfind(c{1},'ie')>1, nie=nie+n; end;
	if strfind(c{1},'cie'),  cie=cie+n; end;
end;
fclose(fid);

printf('cie: %i\nnie: %i\ncei: %i\nnei: %i\n',cie,nie-cie,cei,nei-cei);
v = '';
if (nie < 3 * cie)
	v=' not';
end
printf('I before E when not preceded by C: is%s plausible\n',v);
v = '';
if (nei > 3 * cei)
	v=' not';
end
printf('E before I when preceded by C: is%s plausible\n',v);

```



```txt
octave:23> i_before_e_except_after_c 1_2_all_freq.txt
cie: 994
nie: 8133
cei: 327
nei: 4274
I before E when not preceded by C: is plausible
E before I when preceded by C: is not plausible
octave:24> i_before_e_except_after_c unixdict.txt
cie: 24
nie: 464
cei: 13
nei: 191
I before E when not preceded by C: is plausible
E before I when preceded by C: is not plausible
```



## Objeck

{{trans|Seed7}}

```objeck

use HTTP;
use Collection;

class HttpTest {
  function : Main(args : String[]) ~ Nil {
    IsPlausibleRule("http://wiki.puzzlers.org/pub/wordlists/unixdict.txt");
  }

  function : PlausibilityCheck(comment : String, x : Int, y : Int) ~ Bool {
    ratio := x->As(Float) / y->As(Float);
    "  Checking plausibility of: {$comment}"->PrintLine();
    if(x > 2 * y) {
      "    PLAUSIBLE. As we have counts of {$x} vs {$y} words, a ratio of {$ratio} times"->PrintLine();
    }
    else if(x > y) {
      "    IMPLAUSIBLE. As although we have counts of {$x} vs {$y} words, a ratio of {$ratio} times does not make it plausible"->PrintLine();
    }
    else {
      "    IMPLAUSIBLE, probably contra-indicated. As we have counts of {$x} vs {$y} words, a ratio of {$ratio} times"->PrintLine();
    };

    return x > 2 * y;
  }

  function : IsPlausibleRule(url : String) ~ Nil {
    truecount := 0;
    falsecount := 0;

    client := HttpClient->New();
    data := client->Get(url)->Get(0)->As(String);
    data := data->ToLower();
    words := data->Split("\n");

    cie := Count("cie", words);
    cei := Count("cei", words);
    not_c_ie := Count("ie", words) - cie;
    not_c_ei := Count("ei", words) - cei;

    "Checking plausibility of \"I before E except after C\":"->PrintLine();
    if(PlausibilityCheck("I before E when not preceded by C", not_c_ie, not_c_ei) &
        PlausibilityCheck("E before I when preceded by C", cei, cie)) {
      "OVERALL IT IS PLAUSIBLE!"->PrintLine();
    }
    else {
      "OVERALL IT IS IMPLAUSIBLE!"->PrintLine();
      "(To be plausible, one word count must exceed another by 2 times)"->PrintLine();
    };
  }

  function : Count(check: String, words : String[]) ~ Int {
    count := 0;

    each(i : words) {
      if(words[i]->Find(check) > -1) {
        count += 1;
      };
    };

    return count;
  }
}

```


{{out}}

```txt

Checking plausibility of "I before E except after C":
  Checking plausibility of: I before E when not preceded by C
    PLAUSIBLE. As we have counts of 465 vs 213 words, a ratio of 2.183 times
  Checking plausibility of: E before I when preceded by C
            IMPLAUSIBLE, probably contra-indicated. As we have counts of 13 vs 24 words, a ratio of 0.542 times
OVERALL IT IS IMPLAUSIBLE!
(To be plausible, one word count must exceed another by 2 times)

```



## Perl


```perl
#!/usr/bin/perl
use warnings;
use strict;

sub result {
    my ($support, $against) = @_;
    my $ratio  = sprintf '%.2f', $support / $against;
    my $result = $ratio >= 2;
    print "$support / $against = $ratio. ", 'NOT ' x !$result, "PLAUSIBLE\n";
    return $result;
}

my @keys  = qw(ei cei ie cie);
my %count;

while (<>) {
    for my $k (@keys) {
        $count{$k}++ if -1 != index $_, $k;
    }
}

my ($support, $against, $result);

print 'I before E when not preceded by C: ';
$support = $count{ie} - $count{cie};
$against = $count{ei} - $count{cei};
$result += result($support, $against);

print 'E before I when preceded by C: ';
$support = $count{cei};
$against = $count{cie};
$result += result($support, $against);

print 'Overall: ', 'NOT ' x ($result < 2), "PLAUSIBLE.\n";
```


{{out}}

```txt
I before E when not preceded by C: 465 / 213 = 2.18. PLAUSIBLE
E before I when preceded by C: 13 / 24 = 0.54. NOT PLAUSIBLE
Overall: NOT PLAUSIBLE.
```



### Perl: Stretch Goal

Just replace the while loop with the following one:

```perl
while (<>) {
    my @columns = split;
    next if 3 < @columns;
    my ($word, $freq) = @columns[0, 2];
    for my $k (@keys) {
        $count{$k} += $freq if -1 != index $word, $k;
    }
}
```

{{out}}

```txt
I before E when not preceded by C: 8148 / 4826 = 1.69. NOT PLAUSIBLE
E before I when preceded by C: 327 / 994 = 0.33. NOT PLAUSIBLE
Overall: NOT PLAUSIBLE.
```



## Perl 6

This solution uses grammars and actions to parse the given file, the <tt>Bag</tt> for tallying up occurrences of each possible thing we're looking for ("ie", "ei", "cie", and "cei"), and junctions to determine the plausibility of a phrase from the subphrases. Note that a version of rakudo newer than the January 2014 compiler or Star releases is needed, as this code relies on a recent bugfix to the <tt>make</tt> function.

```perl6
grammar CollectWords {
    token TOP {
        [^^ <word> $$ \n?]+
    }

    token word {
        [ <with_c> | <no_c> | \N ]+
    }

    token with_c {
        c <ie_part>
    }

    token no_c {
        <ie_part>
    }

    token ie_part {
        ie | ei | eie # a couple words in the list have "eie"
    }
}

class CollectWords::Actions {
    method TOP($/) {
        make $<word>».ast.flat.Bag;
    }

    method word($/) {
        if $<with_c> + $<no_c> {
            make flat $<with_c>».ast, $<no_c>».ast;
        } else {
            make ();
        }
    }

    method with_c($/) {
        make "c" X~ $<ie_part>.ast;
    }

    method no_c($/) {
        make "!c" X~ $<ie_part>.ast;
    }

    method ie_part($/) {
        if ~$/ eq 'eie' {
            make ('ei', 'ie');
        } else {
            make ~$/;
        }
    }
}

sub plausible($good, $bad, $msg) {
    if $good > 2*$bad {
        say "$msg: PLAUSIBLE ($good  vs. $bad ✘)";
        return True;
    } else {
        say "$msg: NOT PLAUSIBLE ($good  vs. $bad ✘)";
        return False;
    }
}

my $results = CollectWords.parsefile("unixdict.txt", :actions(CollectWords::Actions)).ast;

my $phrasetest = [&] plausible($results<!cie>, $results<!cei>, "I before E when not preceded by C"),
                     plausible($results<cei>, $results<cie>, "E before I when preceded by C");

say "I before E except after C: ", $phrasetest ?? "PLAUSIBLE" !! "NOT PLAUSIBLE";
```


{{out}}


```txt
I before E when not preceded by C: PLAUSIBLE (466  vs. 217 ✘)
E before I when preceded by C: NOT PLAUSIBLE (13  vs. 24 ✘)
I before E except after C: NOT PLAUSIBLE
```



### Perl 6: Stretch Goal

Note that within the original text file, a tab character was erroneously replaced with a space. Thus, the following changes to the text file are needed before this solution will run:

```txt
--- orig_1_2_all_freq.txt	2014-02-01 14:36:53.124121018 -0800
+++ 1_2_all_freq.txt	2014-02-01 14:37:10.525552980 -0800
@@ -2488,7 +2488,7 @@
 	other than	Prep	43
 	visited	Verb	43
 	cross	NoC	43
-	lie Verb	43
+	lie	Verb	43
 	grown	Verb	43
 	crowd	NoC	43
 	recognised	Verb	43
```


This solution requires just a few modifications to the grammar and actions from the non-stretch goal.

```perl6
grammar CollectWords {
    token TOP {
        ^^ \t Word \t PoS \t Freq $$ \n
        [^^ <word> $$ \n?]+
    }

    token word {
        \t+
        [ <with_c> | <no_c> | \T ]+ \t+
        \T+ \t+ # PoS doesn't matter to us, so ignore it
        $<freq>=[<.digit>+] \h*
    }

    token with_c {
        c <ie_part>
    }

    token no_c {
        <ie_part>
    }

    token ie_part {
        ie | ei
    }
}

class CollectWords::Actions {
    method TOP($/) {
        make $<word>».ast.flat.Bag;
    }

    method word($/) {
        if $<with_c> + $<no_c> {
            make flat $<with_c>».ast xx $<freq>, $<no_c>».ast xx $<freq>;
        } else {
            make ();
        }
    }

    method with_c($/) {
        make "c" ~ $<ie_part>;
    }

    method no_c($/) {
        make "!c" ~ $<ie_part>;
    }
}

sub plausible($good, $bad, $msg) {
    if $good > 2*$bad {
        say "$msg: PLAUSIBLE ($good  vs. $bad ✘)";
        return True;
    } else {
        say "$msg: NOT PLAUSIBLE ($good  vs. $bad ✘)";
        return False;
    }
}

# can't use .parsefile like before due to the non-Unicode £ in this file.
my $file = slurp("1_2_all_freq.txt", :enc<iso-8859-1>);
my $results = CollectWords.parse($file, :actions(CollectWords::Actions)).ast;

my $phrasetest = [&] plausible($results<!cie>, $results<!cei>, "I before E when not preceded by C"),
                     plausible($results<cei>, $results<cie>, "E before I when preceded by C");

say "I before E except after C: ", $phrasetest ?? "PLAUSIBLE" !! "NOT PLAUSIBLE";
```


{{out}}

```txt
I before E when not preceded by C: NOT PLAUSIBLE (8222  vs. 4826 ✘)
E before I when preceded by C: NOT PLAUSIBLE (327  vs. 994 ✘)
I before E except after C: NOT PLAUSIBLE
```



## Phix

Kept dirt simple, difficult to imagine anything being much faster than this.

```Phix
--
-- demo\rosetta\IbeforeE.exw
--
### ===================

--
procedure fatal(string msg)
    printf(1,"unixdict.txt %s. download it from http://wiki.puzzlers.org/pub/wordlists/unixdict.txt\n",{msg})
    if getc(0) then end if
    abort(1)
end procedure

procedure show_plausibility(string msg, integer w, wo)
    printf(1, "%s (pro: %3d, anti: %3d) is%s plausible\n",{msg,w,wo," not"[1..-(w<2*wo)]})
end procedure

integer fn = open(join_path({"..","unixdict.txt"}),"r")
if fn=-1 then
    fn = open("unixdict.txt","r")
    if fn=-1 then fatal("not found") end if
end if
string text = get_text(fn)
close(fn)
-- Note: my unixdict.txt begins with "10th" and ends
-- with "zygote", so boundary checks can be skipped.
integer {cei,xei,cie,xie} @= 0
for i=1 to length(text) do
    if text[i]='i' then
        if text[i-1]='e' then
            if text[i-2]='c' then
                cei += 1
            else
                xei += 1
            end if
        end if
        -- (not elsif here; "eie" occurs twice)
        if text[i+1]='e' then
            if text[i-1]='c' then
                cie += 1
            else
                xie += 1
            end if
        end if
    end if
end for
printf(1,"occurances: cie:%d, xie:%d, cei:%d, xei:%d\n", {cie,xie,cei,xei})
show_plausibility("i before e except after c", xie, cie)
show_plausibility("e before i except after c", xei, cei)
show_plausibility("i before e   when after c", cie, cei)
show_plausibility("e before i   when after c", cei, cie)
show_plausibility("i before e     in general", xie + cie, xei + cei)
show_plausibility("e before i     in general", xei + cei, xie + cie)
```

{{out}}
Although the output matches, I decided to use different metrics from ALGOL 68 for the middle two conclusions.

I am not confident these are meaningful/correct logical inferences anyway, but the raw numbers are right.

(Being told ib4eeac is more often wrong than right has quite clearly made me start to doubt myself.)

```txt

occurances: cie:24, xie:466, cei:13, xei:217
i before e except after c (pro: 466, anti:  24) is plausible
e before i except after c (pro: 217, anti:  13) is plausible
i before e   when after c (pro:  24, anti:  13) is not plausible
e before i   when after c (pro:  13, anti:  24) is not plausible
i before e     in general (pro: 490, anti: 230) is plausible
e before i     in general (pro: 230, anti: 490) is not plausible

```



## PicoLisp


```PicoLisp
(de ibEeaC (File . Prg)
   (let
      (Cie (let N 0 (in File (while (from "cie") (run Prg))))
         Nie (let N 0 (in File (while (from "ie") (run Prg))))
         Cei (let N 0 (in File (while (from "cei") (run Prg))))
         Nei (let N 0 (in File (while (from "ei") (run Prg)))) )
      (prinl "cie: " Cie)
      (prinl "nie: " (dec 'Nie Cie))
      (prinl "cei: " Cei)
      (prinl "nei: " (dec 'Nei Cei))
      (let (NotI (> (* 3 Cie) Nie)  NotE (> Nei (* 3 Cei)))
         (prinl
            "I before E except after C: is"
            (and NotI " not")
            " plausible" )
         (prinl
            "E before I when after C: is"
            (and NotE " not")
            " plausible" )
         (prinl
            "Overall rule is"
            (and (or NotI NotE) " not")
            " plausible" ) ) ) )

(ibEeaC "unixdict.txt"
   (inc 'N) )

(prinl)

(ibEeaC "1_2_all_freq.txt"
   (inc 'N (format (stem (line) "\t"))) )
```

Output:

```txt
cie: 24
nie: 466
cei: 13
nei: 217
I before E except after C: is plausible
E before I when after C: is not plausible
Overall rule is not plausible

cie: 994
nie: 8148
cei: 327
nei: 4826
I before E except after C: is plausible
E before I when after C: is not plausible
Overall rule is not plausible
```



## PowerShell


```Powershell
$Web = New-Object -TypeName Net.Webclient
$Words = $web.DownloadString('http://wiki.puzzlers.org/pub/wordlists/unixdict.txt')

$IE = $EI = $CIE = $CEI = @()

$Clause1 = $Clause2 = $MainClause = $false

foreach ($Word in $Words.split())
{
    switch ($Word)
    {
        {($_ -like '*ie*') -and ($_ -notlike '*cie*')} {$IE += $Word}
        {($_ -like '*ei*') -and ($_ -notlike '*cei*')} {$EI += $Word}
        {$_ -like '*cei*'} {$CEI += $Word}
        {$_ -like '*cie*'} {$CIE += $Word}
    }
}

if ($IE.count -gt $EI.count * 2)
{$Clause1 = $true}
"The plausibility of 'I before E when not preceded by C' is $Clause1"

if ($CEI.count -gt $CIE.count * 2)
{$Clause2 = $true}
"The plausibility of 'E before I when preceded by C' is $Clause2"

if ($Clause1 -and $Clause2)
{$MainClause = $True}
"The plausibility of the phrase 'I before E except after C' is $MainClause"
```

{{out}}

```txt

The plausibility of 'I before E when not preceded by C' is True
The plausibility of 'E before I when preceded by C' is False
The plausibility of the phrase 'I before E except after C' is False

```


=
## Alternative Implementation
=

```Powershell
$Web = New-Object -TypeName Net.Webclient
$Words = $web.DownloadString('http://wiki.puzzlers.org/pub/wordlists/unixdict.txt')

$IE = $EI = $CIE = $CEI = @()

$Clause1 = $Clause2 = $MainClause = $false

foreach ($Word in $Words.split())
{
    switch ($Word)
    {
        {$_ -like '*cei*'} {$CEI += $Word; break}
        {$_ -like '*cie*'} {$CIE += $Word; break}
        {$_ -like '*ie*'}  {$IE += $Word}
        {$_ -like '*ei*'}  {$EI += $Word}
    }
}

if ($IE.count -gt $EI.count * 2)
{$Clause1 = $true}
"The plausibility of 'I before E when not preceded by C' is $Clause1"

if ($CEI.count -gt $CIE.count * 2)
{$Clause2 = $true}
"The plausibility of 'E before I when preceded by C' is $Clause2"

if ($Clause1 -and $Clause2)
{$MainClause = $True}
"The plausibility of the phrase 'I before E except after C' is $MainClause"
```

{{out}}

```txt

The plausibility of 'I before E when not preceded by C' is True
The plausibility of 'E before I when preceded by C' is False
The plausibility of the phrase 'I before E except after C' is False

```



## PureBasic


```purebasic
If ReadFile(1,GetPathPart(ProgramFilename())+"wordlist(en).txt")
  While Not Eof(1)
    wl$+ReadString(1)+";"
  Wend
  CloseFile(1)
EndIf

OpenConsole()
PrintN("Number of words in [wordlist(en).txt]: "+CountString(wl$,";"))
cei.i=CountString(wl$,"cei") : PrintN("Instances of [cei]                   : "+Str(cei))
cie.i=CountString(wl$,"cie") : PrintN("Instances of [cie]                   : "+Str(cie))
Print("Rule: 'e' before 'i' when preceded by 'c' is = ")
If cei>cie : PrintN("plausible") : Else : PrintN("not plausible") : EndIf
wl$=RemoveString(wl$,"cei")  : wl$=RemoveString(wl$,"cie")
PrintN("")
ei.i=CountString(wl$,"ei")   : PrintN("Instances of [*ei] '*'<>'c'          : "+Str(ei))
ie.i=CountString(wl$,"ie")   : PrintN("Instances of [*ie] '*'<>'c'          : "+Str(ie))
Print("Rule: 'i' before 'e' when not preceded by 'c' is = ")
If ie>ei : PrintN("plausible") : Else : PrintN("not plausible") : EndIf
PrintN("")
Print("Overall the rule is : ")
If cei>cie And ie>ei : PrintN("PLAUSIBLE") : Else : PrintN("NOT PLAUSIBLE") :  EndIf
Input()
```

{{out}}

```txt

Number of words in [wordlist(en).txt]: 25104
Instances of [cei]                   : 13
Instances of [cie]                   : 24
Rule: 'e' before 'i' when preceded by 'c' is = not plausible

Instances of [*ei] '*'<>'c'          : 217
Instances of [*ie] '*'<>'c'          : 466
Rule: 'i' before 'e' when not preceded by 'c' is = plausible

Overall the rule is : NOT PLAUSIBLE

```


## Python


```python
import urllib.request
import re

PLAUSIBILITY_RATIO = 2

def plausibility_check(comment, x, y):
    print('\n  Checking plausibility of: %s' % comment)
    if x > PLAUSIBILITY_RATIO * y:
        print('    PLAUSIBLE. As we have counts of %i vs %i, a ratio of %4.1f times'
              % (x, y, x / y))
    else:
        if x > y:
            print('    IMPLAUSIBLE. As although we have counts of %i vs %i, a ratio of %4.1f times does not make it plausible'
                  % (x, y, x / y))
        else:
            print('    IMPLAUSIBLE, probably contra-indicated. As we have counts of %i vs %i, a ratio of %4.1f times'
                  % (x, y, x / y))
    return x > PLAUSIBILITY_RATIO * y

def simple_stats(url='http://wiki.puzzlers.org/pub/wordlists/unixdict.txt'):
    words = urllib.request.urlopen(url).read().decode().lower().split()
    cie = len({word for word in words if 'cie' in word})
    cei = len({word for word in words if 'cei' in word})
    not_c_ie = len({word for word in words if re.search(r'(^ie|[^c]ie)', word)})
    not_c_ei = len({word for word in words if re.search(r'(^ei|[^c]ei)', word)})
    return cei, cie, not_c_ie, not_c_ei

def print_result(cei, cie, not_c_ie, not_c_ei):
    if ( plausibility_check('I before E when not preceded by C', not_c_ie, not_c_ei)
         & plausibility_check('E before I when preceded by C', cei, cie) ):
        print('\nOVERALL IT IS PLAUSIBLE!')
    else:
        print('\nOVERALL IT IS IMPLAUSIBLE!')
    print('(To be plausible, one count must exceed another by %i times)' % PLAUSIBILITY_RATIO)

print('Checking plausibility of "I before E except after C":')
print_result(*simple_stats())
```


{{out}}

```txt
Checking plausibility of "I before E except after C":

  Checking plausibility of: I before E when not preceded by C
    PLAUSIBLE. As we have counts of 465 vs 213, a ratio of  2.2 times

  Checking plausibility of: E before I when preceded by C
    IMPLAUSIBLE, probably contra-indicated. As we have counts of 13 vs 24, a ratio of  0.5 times

OVERALL IT IS IMPLAUSIBLE!
(To be plausible, one count must exceed another by 2 times)
```



### Python: Stretch Goal

Add the following to the bottom of the previous program:

```python
def stretch_stats(url='http://ucrel.lancs.ac.uk/bncfreq/lists/1_2_all_freq.txt'):
    freq = [line.strip().lower().split()
            for line in urllib.request.urlopen(url)
            if len(line.strip().split()) == 3]
    wordfreq = [(word.decode(), int(frq))
                for word, pos, frq in freq[1:]
                if (b'ie' in word) or (b'ei' in word)]
    cie = sum(frq for word, frq in wordfreq if 'cie' in word)
    cei = sum(frq for word, frq in wordfreq if 'cei' in word)
    not_c_ie = sum(frq for word, frq in wordfreq if re.search(r'(^ie|[^c]ie)', word))
    not_c_ei = sum(frq for word, frq in wordfreq if re.search(r'(^ei|[^c]ei)', word))
    return cei, cie, not_c_ie, not_c_ei

print('\n\nChecking plausibility of "I before E except after C"')
print('And taking account of word frequencies in British English:')
print_result(*stretch_stats())
```


{{out|Produces this extra output}}

```txt
Checking plausibility of "I before E except after C"
And taking account of word frequencies in British English:

  Checking plausibility of: I before E when not preceded by C
    IMPLAUSIBLE. As although we have counts of 8192 vs 4826, a ratio of  1.7 times does not make it plausible

  Checking plausibility of: E before I when preceded by C
    IMPLAUSIBLE, probably contra-indicated. As we have counts of 327 vs 994, a ratio of  0.3 times

OVERALL IT IS IMPLAUSIBLE!
(To be plausible, one count must exceed another by 2 times)
```



## R


```rsplus
words = tolower(readLines("http://wiki.puzzlers.org/pub/wordlists/unixdict.txt"))
ie.npc = sum(grepl("(?<!c)ie", words, perl = T))
ei.npc = sum(grepl("(?<!c)ei", words, perl = T))
ie.pc = sum(grepl("cie", words, fixed = T))
ei.pc = sum(grepl("cei", words, fixed = T))

p1 = ie.npc > 2 * ei.npc
p2 = ei.pc > 2 * ie.pc

message("(1) is ", (if (p1) "" else "not "), "plausible.")
message("(2) is ", (if (p2) "" else "not "), "plausible.")
message("The whole phrase is ", (if (p1 && p2) "" else "not "), "plausible.")
```


{{out}}

```txt
(1) is plausible.
(2) is not plausible.
The whole phrase is not plausible.
```



## Racket


```racket
#lang racket

(define (get-tallies filename line-parser . patterns)
  (for/fold ([totals (make-list (length patterns) 0)])
    ([line (file->lines filename)])
    (match-let ([(list word n) (line-parser line)])
      (for/list ([p patterns] [t totals])
        (if (regexp-match? p word)
            (+ n t) t)))))

(define (plausible test) (string-append (if test "" "IM") "PLAUSIBLE"))

(define (subrule description examples counters)
  (let ([result (> examples (* 2 counters))])
    (printf "  The sub-rule \"~a\" is ~a.  There were ~a examples and ~a counter-examples.\n"
            description (plausible result) examples counters)
    result))

(define (plausibility description filename parser)
  (printf "~a:\n" description)
  (match-let ([(list cei cie ie ei) (get-tallies filename parser "cei" "cie" "ie" "ei")])
    (let ([rule1 (subrule "I before E when not preceded by C" (- ie cie) (- ei cei))]
          [rule2 (subrule "E before I when preceded by C" cei cie)])
      (printf "\n  Overall, the rule \"I before E, except after C\" is ~a.\n"
              (plausible (and rule1 rule2))))))

(define (parse-frequency-data line)
  (let ([words (string-split line)])
    (list (string-join (drop-right words 2)) (string->number (last words)))))

(plausibility "Dictionary" "unixdict.txt" (λ (line) (list line 1))) (newline)
(plausibility "Word frequencies (stretch goal)" "1_2_all_freq.txt" parse-frequency-data)
```


{{out}}

```txt

Dictionary:
  The sub-rule "I before E when not preceded by C" is PLAUSIBLE.  There were 465 examples and 213 counter-examples.
  The sub-rule "E before I when preceded by C" is IMPLAUSIBLE.  There were 13 examples and 24 counter-examples.

  Overall, the rule "I before E, except after C" is IMPLAUSIBLE.

Word frequencies (stretch goal):
  The sub-rule "I before E when not preceded by C" is IMPLAUSIBLE.  There were 8163 examples and 4826 counter-examples.
  The sub-rule "E before I when preceded by C" is IMPLAUSIBLE.  There were 327 examples and 994 counter-examples.

  Overall, the rule "I before E, except after C" is IMPLAUSIBLE.

```



## REXX

The following assumptions were made about the (default) dictionary:
::*   there could be leading and/or trailing blanks or tabs
::*   the dictionary words are in mixed case.
::*   there could be blank lines
::*   there may be more than one occurrence of a target string within a word   [einsteinium]


### unweighted version


```rexx
/*REXX program shows  plausibility  of  "I before E"  when not preceded by C,  and      */
/*───────────────────────────────────── "E before I"  when     preceded by C.           */
parse arg iFID .                                 /*obtain optional argument from the CL.*/
if iFID=='' | iFID=="," then iFID='UNIXDICT.TXT' /*Not specified?  Then use the default.*/
#.=0                                             /*zero out the various word counters.  */
     do r=0  while  lines(iFID)\==0              /*keep reading the dictionary 'til done*/
     u=space( lineIn(iFID), 0);      upper u     /*elide superfluous blanks and tabs.   */
     if u==''  then iterate                      /*Is it a blank line?   Then ignore it.*/
     #.words=#.words + 1                         /*keep running count of number of words*/
     if pos('EI', u)\==0 & pos('IE', u)\==0  then #.both=#.both + 1  /*the word has both*/
     call find  'ie'                                                 /*look for   ie    */
     call find  'ei'                                                 /*  "   "    ei    */
     end   /*r*/                                 /*at exit of DO loop,   R = # of lines.*/

L=length(#.words)                                /*use this to align the output numbers.*/
say 'lines in the  '         iFID         " dictionary: "            r
say 'words in the  '         iFID         " dictionary: "            #.words
say
say 'words with "IE" and "EI" (in same word): '    right(#.both, L)
say 'words with "IE" and     preceded by "C": '    right(#.ie.c ,L)
say 'words with "IE" and not preceded by "C": '    right(#.ie.z ,L)
say 'words with "EI" and     preceded by "C": '    right(#.ei.c ,L)
say 'words with "EI" and not preceded by "C": '    right(#.ei.z ,L)
say;                         mantra= 'The spelling mantra  '
p1=#.ie.z / max(1, #.ei.z);  phrase= '"I before E when not preceded by C"'
say mantra phrase   ' is '   word("im", 1 + (p1>2) )'plausible.'
p2=#.ie.c / max(1, #.ei.c);  phrase= '"E before I when     preceded by C"'
say mantra phrase   ' is '   word("im", 1 + (p2>2) )'plausible.'
po=(p1>2 & p2>2);            say 'Overall, it is'    word("im", 1 + po)'plausible.'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
find: arg x;  s=1;  do forever;           _=pos(x, u, s);          if _==0  then return
                    if substr(u, _ - 1 + (_==1)*999, 1)=='C'  then #.x.c=#.x.c + 1
                                                              else #.x.z=#.x.z + 1
                    s=_ + 1                      /*handle the cases of multiple finds.  */
                    end   /*forever*/
```

{{out|output|text=  when using the default dictionary:}}

```txt

lines in the   UNIXDICT.TXT  dictionary:  25104
words in the   UNIXDICT.TXT  dictionary:  25104

words with "IE" and "EI" (in same word):      4
words with "IE" and     preceded by "C":     24
words with "IE" and not preceded by "C":    466
words with "EI" and     preceded by "C":     13
words with "EI" and not preceded by "C":    217

The spelling mantra   "I before E when not preceded by C"  is  plausible.
The spelling mantra   "E before I when     preceded by C"  is  implausible.
Overall, it is implausible.

```



### weighted version

Using the default word frequency count file, several discrepancies (or not) became apparent:
::*   some "words" were in fact,   phrases
::*   some words were in the form of     x / y     indicating x OR y
::*   some words were in the form of     x/y       (with no blanks)   indicating x OR y,   or a word
::*   some words had a   '''~'''    prefix
::*   some words had a   '''*'''    suffix
::*   some words had a   '''~'''    suffix
::*   some words had a   '''~'''   and   '''*'''   suffix
::*   one word had a   '''~'''    prefix and a   '''~'''   suffix
::*   some lines had an imbedded   '''[xxx]'''   comment
::*   some words had a   ''' ' '''   (quote)   prefix to indicate a:
::::*   possessive
::::*   plural
::::*   contraction
::::*   word   (as is)
All of the cases when an asterisk   ['''*''']   or tilde   ['''~''']   was used <u>weren't</u> programmatically handled within the REXX program;   it is assumed that prefixes and suffixes were being used to indicate multiple words that either begin or end with (any) string   (or in some case, both).

A cursory look at the file seems to indicate that the use of the tilde and/or asterisk doesn't affect the rules for the mantra phrases.

```rexx
/*REXX program shows  plausibility  of  "I before E"  when not preceded by C,  and      */
/*───────────────────────────────────── "E before I"  when     preceded by C,  using a  */
/*───────────────────────────────────── weighted frequency for each word.               */
parse arg iFID wFID .                            /*obtain optional arguments from the CL*/
if iFID=='' | iFID=="," then iFID='UNIXDICT.TXT' /*Not specified?  Then use the default.*/
if wFID=='' | wFID=="," then wFID='WORDFREQ.TXT' /* "      "         "   "   "     "    */
cntl=xrange(, ' ')                               /*get all manner of tabs, control chars*/
#.=0                                             /*zero out the various word counters.  */
f.=1                                             /*default word frequency multiplier.   */
    do recs=0  while lines(wFID)\==0             /*read a record from the file 'til done*/
    u=translate( linein(wFID), , cntl);  upper u /*translate various tabs and cntl chars*/
    u=translate(u, '*', "~")                     /*translate tildes (~)  to an asterisk.*/
    if u==''                 then iterate        /*Is this a blank line? Then ignore it.*/
    freq=word(u, words(u) )                      /*obtain the last token on the line.   */
    if \datatype(freq, 'W')  then iterate        /*FREQ not an integer?  Then ignore it.*/
    parse var  u   w.1  '/'  w.2  .              /*handle case of:   ααα/ßßß  ···       */

         do j=1  for 2;  w.j=word(w.j, 1)        /*strip leading and/or trailing blanks.*/
         _=w.j;   if _==''          then iterate /*if not present, then ignore it.      */
         if j==2  then if w.2==w.1  then iterate /*second word ≡ first word?  Then skip.*/
         #.freqs=#.freqs + 1                     /*bump word counter in the  FREQ  list.*/
         f._=f._ + freq                          /*add to a word's frequency count.     */
         end   /*ws*/
    end        /*recs*/                          /*at exit of DO loop, RECS = # of recs.*/

if    recs\==0  then say 'lines in the  '        wFID        "       list: "      recs
if #.freqs\==0  then say 'words in the  '        wFID        "       list: "      #.freqs
if #.freqs ==0  then weighted=
                else weighted= ' (weighted)'
say
    do r=0  while  lines(iFID)\==0               /*keep reading the dictionary 'til done*/
    u=space( linein(iFID), 0);      upper u      /*elide superfluous blanks and tabs.   */
    if u==''  then iterate                       /*Is it a blank line?   Then ignore it.*/
    #.words=#.words + 1                          /*keep running count of number of words*/
    one=f.u
    if pos('EI', u)\==0 & pos('IE', u)\==0  then #.both=#.both + one /*the word has both*/
    call find  'ie'                                                  /*look for   ie    */
    call find  'ei'                                                  /*  "   "    ei    */
    end   /*r*/                                  /*at exit of DO loop,   R = # of lines.*/

L=length(#.words)                                /*use this to align the output numbers.*/
say 'lines in the  '         iFID         ' dictionary: '             r
say 'words in the  '         iFID         ' dictionary: '             #.words
say
say 'words with "IE" and "EI" (in same word): '    right(#.both, L)   weighted
say 'words with "IE" and     preceded by "C": '    right(#.ie.c ,L)   weighted
say 'words with "IE" and not preceded by "C": '    right(#.ie.z ,L)   weighted
say 'words with "EI" and     preceded by "C": '    right(#.ei.c ,L)   weighted
say 'words with "EI" and not preceded by "C": '    right(#.ei.z ,L)   weighted
say;                         mantra= 'The spelling mantra  '
p1=#.ie.z / max(1, #.ei.z);  phrase= '"I before E when not preceded by C"'
say mantra phrase   ' is '   word("im", 1 + (p1>2) )'plausible.'
p2=#.ie.c / max(1, #.ei.c);  phrase= '"E before I when     preceded by C"'
say mantra phrase   ' is '   word("im", 1 + (p2>2) )'plausible.'
po=(p1>2 & p2>2);            say 'Overall, it is'    word("im",1 + po)'plausible.'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
find: arg x;  s=1;  do forever;           _=pos(x, u, s);          if _==0  then return
                    if substr(u, _ - 1 + (_==1)*999, 1)=='C'  then #.x.c=#.x.c + one
                                                              else #.x.z=#.x.z + one
                    s=_ + 1                      /*handle the cases of multiple finds.  */
```

{{out|output|text=  when using the default dictionary and default word frequency list:}}

```txt

lines in the   WORDFREQ.TXT        list:  7727
words in the   WORDFREQ.TXT        list:  7728

lines in the   UNIXDICT.TXT  dictionary:  25104
words in the   UNIXDICT.TXT  dictionary:  25104

words with "IE" and "EI" (in same word):      4  (weighted)
words with "IE" and     preceded by "C":    719  (weighted)
words with "IE" and not preceded by "C":   3818  (weighted)
words with "EI" and     preceded by "C":    100  (weighted)
words with "EI" and not preceded by "C":   4875  (weighted)

The spelling mantra   "I before E when not preceded by C"  is  implausible.
The spelling mantra   "E before I when     preceded by C"  is  plausible.
Overall, it is implausible.

```



## Ring


```ring

# Project : I before E except after C

fn1 = "unixdict.txt"

fp = fopen(fn1,"r")
str = fread(fp, getFileSize(fp))
fclose(fp)
strcount = str2list(str)
see "The number of words in unixdict : " + len(strcount) + nl
cei = count(str, "cei")
cie = count(str, "cie")
ei = count(str, "ei")
ie = count(str, "ie")
see "Instances of cei : " + cei + nl
see "Instances of cie : " + cie + nl
see "Rule: 'e' before 'i' when preceded by 'c' is = "
if cei>cie see "plausible" + nl else see"not plausible" + nl ok
see "Instances of *ei, where * is not c : " + (ei-cei) + nl
see "Instances of *ie, where * is not c: " + (ie-cie) + nl
see "Rule: 'i' before 'e' when not preceded by 'c' is = "
if ie>ei see "plausible" + nl else see "not plausible" + nl ok
see "Overall the rule is : "
if cei>cie and ie>ei see "PLAUSIBLE" + nl else see "NOT PLAUSIBLE" + nl ok

func getFileSize fp
       c_filestart = 0
       c_fileend = 2
       fseek(fp,0,c_fileend)
       nfilesize = ftell(fp)
       fseek(fp,0,c_filestart)
       return nfilesize

func count(cString,dString)
       sum = 0
       while substr(cString,dString) > 0
               sum = sum + 1
               cString = substr(cString,substr(cString,dString)+len(string(sum)))
       end
       return sum

```

Output:

```txt

The number of words in unixdict : 25104
Instances of cei : 13
Instances of cie : 24
Rule: 'e' before 'i' when preceded by 'c' is = not plausible
Instances of *ei, where * is not c : 217
Instances of *ie, where * is not c: 466
Rule: 'i' before 'e' when not preceded by 'c' is = plausible
Overall the rule is : NOT PLAUSIBLE

```



## Ruby


```ruby
require 'open-uri'

plausibility_ratio = 2
counter = Hash.new(0)
path = 'http://wiki.puzzlers.org/pub/wordlists/unixdict.txt'
rules = [['I before E when not preceded by C:', 'ie', 'ei'],
         ['E before I when preceded by C:', 'cei', 'cie']]

open(path){|f| f.each{|line| line.scan(/ie|ei|cie|cei/){|match| counter[match] += 1 }}}

overall_plausible = rules.all? do |(str, x, y)|
  num_x, num_y, ratio = counter[x], counter[y], counter[x] / counter[y].to_f
  plausibility = ratio > plausibility_ratio
  puts str
  puts "#{x}: #{num_x}; #{y}: #{num_y}; Ratio: #{ratio.round(2)}: #{ plausibility ? 'Plausible' : 'Implausible'}"
  plausibility
end

puts "Overall: #{overall_plausible ? 'Plausible' : 'Implausible'}."

```

{{out}}

```txt

I before E when not preceded by C:
ie: 464; ei: 217; Ratio: 2.14: Plausible
E before I when preceded by C:
cei: 13; cie: 24; Ratio: 0.54: Implausible
Overall: Implausible.

```



## Rust


```rust
use std::default::Default;
use std::ops::AddAssign;

use itertools::Itertools;
use reqwest::get;

#[derive(Default, Debug)]
struct Feature<T> {
    pub cie: T,
    pub xie: T,
    pub cei: T,
    pub xei: T,
}

impl AddAssign<Feature<bool>> for Feature<u64> {
    fn add_assign(&mut self, rhs: Feature<bool>) {
        self.cei += rhs.cei as u64;
        self.xei += rhs.xei as u64;
        self.cie += rhs.cie as u64;
        self.xie += rhs.xie as u64;
    }
}

fn check_feature(word: &str) -> Feature<bool> {
    let mut feature: Feature<bool> = Default::default();

    for window in word.chars().tuple_windows::<(char, char, char)>() {
        match window {
            ('c', 'e', 'i') => { feature.cei = true }
            ('c', 'i', 'e') => { feature.cie = true }
            (not_c, 'e', 'i') if not_c != 'c' => (feature.xei = true),
            (not_c, 'i', 'e') if not_c != 'c' => (feature.xie = true),
            _ => {}
        }
    }

    feature
}


fn maybe_is_feature_plausible(feature_count: u64, opposing_count: u64) -> Option<bool> {
    if feature_count > 2 * opposing_count { Some(true) } else if opposing_count > 2 * feature_count { Some(false) } else { None }
}

fn print_feature_plausibility(feature_plausibility: Option<bool>, feature_name: &str) {
    let plausible_msg =
        match feature_plausibility {
            None => " is implausible",
            Some(true) => "is plausible",
            Some(false) => "is definitely implausible",
        };

    println!("{} {}", feature_name, plausible_msg)
}

fn main() {
    let mut res = get(" http://wiki.puzzlers.org/pub/wordlists/unixdict.txt").unwrap();
    let texts = res.text().unwrap();

    let mut feature_count: Feature<u64> = Default::default();
    for word in texts.lines() {
        let feature = check_feature(word);
        feature_count += feature;
    }

    println!("Counting {:#?}", feature_count);

    let xie_plausibility =
        maybe_is_feature_plausible(feature_count.xie, feature_count.cie);
    let cei_plausibility =
        maybe_is_feature_plausible(feature_count.cei, feature_count.xei);

    print_feature_plausibility(xie_plausibility, "I before E when not preceded by C");
    print_feature_plausibility(cei_plausibility, "E before I when preceded by C");
    println!("The rule in general is {}",
             if xie_plausibility.unwrap_or(false) && cei_plausibility.unwrap_or(false)
             { "Plausible" } else { "Implausible" }
    );
}

```


```txt

Counting Feature {
    cie: 24,
    xie: 464,
    cei: 13,
    xei: 194,
}
I before E when not preceded by C is plausible
E before I when preceded by C is definitely implausible
The rule in general is Implausible

```




## Scala


```Scala
object I_before_E_except_after_C extends App {
  val testIE1 = "(^|[^c])ie".r // i before e when not preceded by c
  val testIE2 = "cie".r // i before e when preceded by c
  var countsIE = (0,0)

  val testCEI1 = "cei".r // e before i when preceded by c
  val testCEI2 = "(^|[^c])ei".r // e before i when not preceded by c
  var countsCEI = (0,0)

  scala.io.Source.fromURL("http://wiki.puzzlers.org/pub/wordlists/unixdict.txt").getLines.map(_.toLowerCase).foreach{word =>
    if (testIE1.findFirstIn(word).isDefined) countsIE = (countsIE._1 + 1, countsIE._2)
    if (testIE2.findFirstIn(word).isDefined) countsIE = (countsIE._1, countsIE._2 + 1)
    if (testCEI1.findFirstIn(word).isDefined) countsCEI = (countsCEI._1 + 1, countsCEI._2)
    if (testCEI2.findFirstIn(word).isDefined) countsCEI = (countsCEI._1, countsCEI._2 + 1)
  }

  def plausible(counts: (Int,Int)) = counts._1 > (2 * counts._2)
  def plausibility(plausible: Boolean) = if (plausible) "plausible" else "implausible"
  def plausibility(counts: (Int, Int)): String = plausibility(plausible(counts))
  println("I before E when not preceded by C: "+plausibility(countsIE))
  println("E before I when preceded by C: "+plausibility(countsCEI))
  println("Overall: "+plausibility(plausible(countsIE) && plausible(countsCEI)))
}
```

{{out}}

```txt
I before E when not preceded by C: plausible
E before I when preceded by C: implausible
Overall: implausible
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "gethttp.s7i";
  include "float.s7i";

const integer: PLAUSIBILITY_RATIO is 2;

const func boolean: plausibilityCheck (in string: comment, in integer: x, in integer: y) is func
  result
    var boolean: plausible is FALSE;
  begin
    writeln("  Checking plausibility of: " <& comment);
    if x > PLAUSIBILITY_RATIO * y then
      writeln("    PLAUSIBLE. As we have counts of " <& x <& " vs " <& y <&
              " words, a ratio of " <& flt(x) / flt(y) digits 1 lpad 4 <& " times");
    elsif x > y then
      writeln("    IMPLAUSIBLE. As although we have counts of " <& x <& " vs " <& y <&
              " words, a ratio of " <& flt(x) / flt(y) digits 1 lpad 4 <& " times does not make it plausible");
    else
      writeln("    IMPLAUSIBLE, probably contra-indicated. As we have counts of " <& x <& " vs " <& y <&
              " words, a ratio of " <& flt(x) / flt(y) digits 1 lpad 4 <& " times");
    end if;
    plausible := x > PLAUSIBILITY_RATIO * y;
  end func;

const func integer: count (in string: stri, in array string: words) is func
  result
    var integer: count is 0;
  local
    var integer: index is 0;
  begin
    for key index range words do
      if pos(words[index], stri) <> 0 then
        incr(count);
      end if;
    end for;
  end func;

const proc: main is func
  local
    var array string: words is 0 times "";
    var integer: cie is 0;
    var integer: cei is 0;
    var integer: not_c_ie is 0;
    var integer: not_c_ei is 0;
  begin
    words := split(lower(getHttp("wiki.puzzlers.org/pub/wordlists/unixdict.txt")), "\n");
    cie := count("cie", words);
    cei := count("cei", words);
    not_c_ie := count("ie", words) - cie;
    not_c_ei := count("ei", words) - cei;
    writeln("Checking plausibility of \"I before E except after C\":");
    if plausibilityCheck("I before E when not preceded by C", not_c_ie, not_c_ei) and
        plausibilityCheck("E before I when preceded by C", cei, cie) then
      writeln("OVERALL IT IS PLAUSIBLE!");
    else
      writeln("OVERALL IT IS IMPLAUSIBLE!");
      writeln("(To be plausible, one word count must exceed another by " <& PLAUSIBILITY_RATIO <& " times)");
    end if;
  end func;
```


{{out}}

```txt

Checking plausibility of "I before E except after C":
  Checking plausibility of: I before E when not preceded by C
    PLAUSIBLE. As we have counts of 465 vs 213 words, a ratio of  2.2 times
  Checking plausibility of: E before I when preceded by C
    IMPLAUSIBLE, probably contra-indicated. As we have counts of 13 vs 24 words, a ratio of  0.5 times
OVERALL IT IS IMPLAUSIBLE!
(To be plausible, one word count must exceed another by 2 times)

```


## Swift

Using [https://github.com/johnno1962/SwiftRegex/blob/master/SwiftRegex.swift SwiftRegex] for easy regex in strings.

```Swift
import Foundation

let request = NSURLRequest(URL: NSURL(string: "http://wiki.puzzlers.org/pub/wordlists/unixdict.txt")!)

NSURLConnection.sendAsynchronousRequest(request, queue: NSOperationQueue()) {res, data, err in
    if (data != nil) {
        if let fileAsString = NSString(data: data, encoding: NSUTF8StringEncoding) {
            var firstCase = false
            var secondCase = false
            var cie = 0
            var cei = 0
            var not_c_ie = 0
            var not_c_ei = 0
            let words = fileAsString.componentsSeparatedByString("\n")
            for word in words {
                var wordRegex = RegexMutable(word as String)
                if (wordRegex["cie"]) {
                    cie++
                }
                if (wordRegex["cei"]) {
                    cei++
                }
                if (wordRegex["(^ie|[^c]ie)"].matches().count != 0) {
                    not_c_ie++
                }
                if (wordRegex["(^ei|[^c]ei)"].matches().count != 0) {
                    not_c_ei++
                }
            }


            if (not_c_ie > not_c_ei * 2) {
                println("I before E when not preceded by C is plausable")
                firstCase = true
            } else {
                println("I before E when not preceded by C is not plausable")
            }

            if (cei > cie * 2) {
                secondCase = true
                println("E before I when preceded by C is plausable")
            } else {
                println("E before I when preceded by C is not plausable")
            }

            if (firstCase && secondCase) {
                println("I before E except after C is plausible")
            } else {
                println("I before E except after C is not plausible")
            }
        }
    }
}

CFRunLoopRun()
```

{{out}}

```txt

I before E when not preceded by C is plausable
E before I when preceded by C is not plausable
I before E except after C is not plausible
```



## Tcl

{{trans|Python}}<!-- very approximately, mainly for the messages -->

```tcl
package require http

variable PLAUSIBILITY_RATIO 2.0
proc plausible {description x y} {
    variable PLAUSIBILITY_RATIO
    puts "  Checking plausibility of: $description"
    if {$x > $PLAUSIBILITY_RATIO * $y} {
	set conclusion "PLAUSIBLE"
	set fmt "As we have counts of %i vs %i words, a ratio of %.1f times"
	set result true
    } elseif {$x > $y} {
	set conclusion "IMPLAUSIBLE"
	set fmt "As although we have counts of %i vs %i words,"
	append fmt " a ratio of %.1f times does not make it plausible"
	set result false
    } else {
	set conclusion "IMPLAUSIBLE, probably contra-indicated"
	set fmt "As we have counts of %i vs %i words, a ratio of %.1f times"
	set result false
    }
    puts [format "    %s.\n    $fmt" $conclusion $x $y [expr {double($x)/$y}]]
    return $result
}

set t [http::geturl http://wiki.puzzlers.org/pub/wordlists/unixdict.txt]
set words [split [http::data $t] "\n"]
http::cleanup $t
foreach {name pattern} {ie (?:^|[^c])ie ei (?:^|[^c])ei cie cie cei cei} {
    set count($name) [llength [lsearch -nocase -all -regexp $words $pattern]]
}

puts "Checking plausibility of \"I before E except after C\":"
if {
    [plausible "I before E when not preceded by C" $count(ie) $count(ei)] &&
    [plausible "E before I when preceded by C" $count(cei) $count(cie)]
} then {
    puts "\nOVERALL IT IS PLAUSIBLE!"
} else {
    puts "\nOVERALL IT IS IMPLAUSIBLE!"
}
puts "\n(To be plausible, one word count must exceed another by\
	$PLAUSIBILITY_RATIO times)"
```

{{out}}
<!-- note that checking the pronunciation of the words indicates a key guard on the real rule that isn't normally stated -->

```txt

Checking plausibility of "I before E except after C":
  Checking plausibility of: I before E when not preceded by C
    PLAUSIBLE.
    As we have counts of 465 vs 213 words, a ratio of 2.2 times
  Checking plausibility of: E before I when preceded by C
    IMPLAUSIBLE, probably contra-indicated.
    As we have counts of 13 vs 24 words, a ratio of 0.5 times

OVERALL IT IS IMPLAUSIBLE!

(To be plausible, one word count must exceed another by 2.0 times)

```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT,{}
words=REQUEST("http://wiki.puzzlers.org/pub/wordlists/unixdict.txt")
size=SIZE(words)
ieei=cie=xie=cei=xei=0

LOOP word=words
IF (word.nc." ie "," ei ") CYCLE

IF (word.ct." ie "&& word.ct." ei ") THEN
 ieei=ieei+1
  IF (word.ct." Cie ") THEN
   cie=cie+1
  ELSEIF (word.ct." Cei ") THEN
   cei=cei+1
  ELSE
   xei=xei+1
  ENDIF
ENDIF

IF (word.ct." ie ") THEN
  IF (word.ct." Cie ") THEN
    cie=cie+1
  ELSE
    xie=xie+1
  ENDIF
ELSEIF (word.ct." ei ") THEN
  IF (word.ct." Cei ") THEN
    cei=cei+1
  ELSE
    xei=xei+1
  ENDIF
ENDIF

ENDLOOP

PRINT "ieee ", ieei
PRINT "cie  ", cie
PRINT "xie  ", xie
PRINT "cei  ", cei
PRINT "xei  ", xei

doublexei=2*xei
doublecei=cei*2

IF (xie>doublexei) THEN
 check1="plausible"
ELSE
 check1="not plausible"
ENDIF

IF (cei>xei) THEN
 check2="plausible"
ELSE
 check2="not plausible"
ENDIF
IF (check1==check2) THEN
 checkall="plausible"
ELSE
 checkall="not plausible"
ENDIF

TRAcE *check1,check2,checkall

```

Output:

```txt

ieee 4
cie  24
xie  465
cei  13
xei  213
TRACE *    62    -*SKRIPTE  203
check1       = plausible
check2       = not plausible
checkall     = not plausible

```



## UNIX Shell


```bash
#!/bin/sh

matched() {
  grep -Poe "$1" unixdict.txt | wc -l
}

check() {
  local num_for="$(matched "$3")"
  local num_against="$(matched "$2")"
  if [ "$num_for" -le "$(expr 2 \* "$num_against")" ]; then
    echo "Clause $1 not plausible ($num_for examples; $num_against counterexamples)"
    return 1
  else
    echo "Clause $1 is plausible ($num_for examples; $num_against counterexamples)"
    return 0
  fi
}

check 1 '(?<!c)ei' '(?<!c)ie'
PLAUSIBLE_1=$?
check 2 'cie' 'cei'
PLAUSIBLE_2=$?
if [ $PLAUSIBLE_1 -eq 0 -a $PLAUSIBLE_2 -eq 0 ]; then
  echo "Overall, the rule is plausible"
else
  echo "Overall, the rule is not plausible"
fi

```

{{out}}

```txt

Clause 1 is plausible (466 examples; 217 counterexamples)
Clause 2 not plausible (13 examples; 24 counterexamples)
Overall, the rule is not plausible

```



## VBScript

The sample text was downloaded and saved in the same folder as the script.

```vb

Set objFSO = CreateObject("Scripting.FileSystemObject")
Set srcFile = objFSO.OpenTextFile(objFSO.GetParentFolderName(WScript.ScriptFullName) &_
	"\unixdict.txt",1,False,0)

cei = 0 : cie = 0 : ei = 0 : ie = 0

Do Until srcFile.AtEndOfStream
	word = srcFile.ReadLine
	If InStr(word,"cei") Then
		cei = cei + 1
	ElseIf InStr(word,"cie") Then
		cie = cie + 1
	ElseIf InStr(word,"ei") Then
		ei = ei + 1
	ElseIf InStr(word,"ie") Then
		ie = ie + 1
	End If
Loop

FirstClause = False
SecondClause = False
Overall = False

'testing the first clause
If  ie > ei*2 Then
	WScript.StdOut.WriteLine "I before E when not preceded by C is plausible."
	FirstClause = True
Else
	WScript.StdOut.WriteLine "I before E when not preceded by C is NOT plausible."
End If

'testing the second clause
If cei > cie*2 Then
	WScript.StdOut.WriteLine "E before I when not preceded by C is plausible."
	SecondClause = True
Else
	WScript.StdOut.WriteLine "E before I when not preceded by C is NOT plausible."
End If

'overall clause
If FirstClause And SecondClause Then
	WScript.StdOut.WriteLine "Overall it is plausible."
Else
	WScript.StdOut.WriteLine "Overall it is NOT plausible."
End If

srcFile.Close
Set objFSO = Nothing

```


{{Out}}

```txt

I before E when not preceded by C is plausible.
E before I when not preceded by C is NOT plausible.
Overall it is NOT plausible.

```



## Visual Basic .NET

'''Compiler:''' Roslyn Visual Basic (language version >= 15.3)
{{works with|.NET Core|2.1}}

Implemented using both a single-pass loop and regex. Implementation used is toggled with compiler constant.

Regex implementation does not technically conform to specification because it counts the number of occurrences of "ie" and "ei" instead of the number of words.


```vbnet
Option Compare Binary
Option Explicit On
Option Infer On
Option Strict On

Imports System.Text.RegularExpressions

#Const USE_REGEX = False

Module Program
    ' Supports both local and remote files
    Const WORDLIST_URI = "http://wiki.puzzlers.org/pub/wordlists/unixdict.txt"


    ' The support factor of a word for EI or IE is the number of occurrences that support the rule minus the number that oppose it.
    ' I.e., for IE:
    '   - increased when not preceded by C
    '   - decreased when preceded by C
    ' and for EI:
    '   - increased when preceded by C
    '   - decreased when not preceded by C
    Private Function GetSupportFactor(word As String) As (IE As Integer, EI As Integer)
        Dim IE, EI As Integer

        ' Enumerate the letter pairs in the word.
        For i = 0 To word.Length - 2
            Dim pair = word.Substring(i, 2)

            ' Instances at the beginning of a word count towards the factor and are treated as not preceded by C.
            Dim prevIsC As Boolean = i > 0 AndAlso String.Equals(word(i - 1), "c"c, StringComparison.OrdinalIgnoreCase)

            If pair.Equals("ie", StringComparison.OrdinalIgnoreCase) Then
                IE += If(Not prevIsC, 1, -1)
            ElseIf pair.Equals("ei", StringComparison.OrdinalIgnoreCase) Then
                EI += If(prevIsC, 1, -1)
            End If
        Next

        If Math.Abs(IE) > 1 Or Math.Abs(EI) > 1 Then Debug.WriteLine($"{word}: {GetSupportFactor}")
        Return (IE, EI)
    End Function


    ' Returns the number of words that support or oppose the rule.
    Private Function GetPlausabilities(words As IEnumerable(Of String)) As (ieSuppCount As Integer, ieOppCount As Integer, eiSuppCount As Integer, eiOppCount As Integer)
        Dim ieSuppCount, ieOppCount, eiSuppCount, eiOppCount As Integer

        For Each word In words
            Dim status = GetSupportFactor(word)
            If status.IE > 0 Then
                ieSuppCount += 1
            ElseIf status.IE < 0 Then
                ieOppCount += 1
            End If
            If status.EI > 0 Then
                eiSuppCount += 1
            ElseIf status.EI < 0 Then
                eiOppCount += 1
            End If
        Next

        Return (ieSuppCount, ieOppCount, eiSuppCount, eiOppCount)
    End Function


    ' Takes entire file instead of individual words.
    ' Returns the number of instances of IE or EI that support or oppose the rule.
    Private Function GetPlausabilitiesRegex(words As String) As (ieSuppCount As Integer, ieOppCount As Integer, eiSuppCount As Integer, eiOppCount As Integer)
        ' Gets number of occurrences of the pattern, case-insensitive.
        Dim count = Function(pattern As String) Regex.Matches(words, pattern, RegexOptions.IgnoreCase).Count

        Dim ie = count("[^c]ie")
        Dim ei = count("[^c]ei")
        Dim cie = count("cie")
        Dim cei = count("cei")

        Return (ie, cie, cei, ei)
    End Function


    Sub Main()
        Dim file As String
        Dim wc As New Net.WebClient()
        Try
            Console.WriteLine("Fetching file...")
            file = wc.DownloadString(WORDLIST_URI)
            Console.WriteLine("Success.")
            Console.WriteLine()
        Catch ex As Net.WebException
            Console.WriteLine(ex.Message)
            Exit Sub
        Finally
            wc.Dispose()
        End Try

#If USE_REGEX Then
        Dim res = GetPlausabilitiesRegex(file)
#Else
        Dim words = file.Split({vbCr, vbLf}, StringSplitOptions.RemoveEmptyEntries)
        Dim res = GetPlausabilities(words)
#End If

        Dim PrintResult =
        Function(suppCount As Integer, oppCount As Integer, printEI As Boolean) As Boolean
            Dim ratio = suppCount / oppCount,
                plausible = ratio > 2
#If Not USE_REGEX Then
            Console.WriteLine($"    Words with no instances of {If(printEI, "EI", "IE")} or equal numbers of supporting/opposing occurrences: {words.Length - suppCount - oppCount}")
#End If
            Console.WriteLine($"    Number supporting: {suppCount}")
            Console.WriteLine($"    Number opposing: {oppCount}")
            Console.WriteLine($"    {suppCount}/{oppCount}={ratio:N3}")
            Console.WriteLine($"    Rule therefore IS {If(plausible, "", "NOT ")}plausible.")
            Return plausible
        End Function

#If USE_REGEX Then
        Console.WriteLine($"Total occurrences of IE: {res.ieOppCount + res.ieSuppCount}")
        Console.WriteLine($"Total occurrences of EI: {res.eiOppCount + res.eiSuppCount}")
#Else
        Console.WriteLine($"Total words: {words.Length}")
#End If

        Console.WriteLine()
        Console.WriteLine("""IE is not preceded by C""")
        Dim iePlausible = PrintResult(res.ieSuppCount, res.ieOppCount, False)

        Console.WriteLine()
        Console.WriteLine("""EI is preceded by C""")
        Dim eiPlausible = PrintResult(res.eiSuppCount, res.eiOppCount, True)

        Console.WriteLine()
        Console.WriteLine($"Rule thus overall IS {If(iePlausible AndAlso eiPlausible, "", "NOT ")}plausible.")
    End Sub
End Module

```


{{out|case=Loop implementation}}

```txt
Fetching file...
Success.

Total words: 25104

"IE is not preceded by C"
    Words with no instances of IE or equal numbers of supporting/opposing occurrences: 24615
    Number supporting: 465
    Number opposing: 24
    465/24=19.375
    Rule therefore IS plausible.

"EI is preceded by C"
    Words with no instances of EI or equal numbers of supporting/opposing occurrences: 24878
    Number supporting: 13
    Number opposing: 213
    13/213=0.061
    Rule therefore IS NOT plausible.

Rule thus overall IS NOT plausible.
```


{{out|case=Regex implementation}}

```txt
Fetching file...
Success.

Total occurrences of IE: 490
Total occurrences of EI: 230

"IE is not preceded by C"
    Number supporting: 466
    Number opposing: 24
    466/24=19.417
    Rule therefore IS plausible.

"EI is preceded by C"
    Number supporting: 13
    Number opposing: 217
    13/217=0.060
    Rule therefore IS NOT plausible.

Rule thus overall IS NOT plausible.
```



## zkl


```zkl
fcn wcnt(wordList,altrs,aAdjust,bltrs,bAdjust,text){
   a:=wordList.reduce('wrap(cnt,word){ cnt+word.holds(altrs) },0) - aAdjust;
   b:=wordList.reduce('wrap(cnt,word){ cnt+word.holds(bltrs) },0) - bAdjust;
   ratio:=a.toFloat()/b;
   "%s is %splausible".fmt(text,ratio<2 and "im" or "").println();
   "  %d cases for and %d cases against is a ratio of %.3f.".fmt(a,b,ratio).println();
   return(a,b,ratio);
}
wordList:=File("unixdict.txt").read();
```


```zkl
a,b,r1:=wcnt(wordList,"cei",0,"cie",0,"E before I when preceded by C");
_,_,r2:=wcnt(wordList,"ie",b,"ei",a,  "I before E when not preceded by C");
"Overall the rule is %splausible".fmt((r1<2 or r2<2) and "im" or "").println();
```

{{out}}

```txt

E before I when preceded by C is implausible
  13 cases for and 24 cases against is a ratio of 0.542.
I before E when not preceded by C is plausible
  465 cases for and 213 cases against is a ratio of 2.183.
Overall the rule is implausible

```

Stretch

```zkl
fcn wc2(wordList,altrs,aAdjust,bltrs,bAdjust,text){
   a,b:=wordList.reduce('wrap(cnts,line){
      // don't care if line is "Word PoS Freq" or "as yet Adv 14"
      word,_,n:=line.split();
      if(word.holds(altrs)) cnts[0]=cnts[0]+n;
      if(word.holds(bltrs)) cnts[1]=cnts[1]+n;
      cnts
   },L(0,0));
   a-=aAdjust; b-=bAdjust;
   ratio:=a.toFloat()/b;
   "%s is %splausible".fmt(text,ratio<2 and "im" or "").println();
   "  %d cases for and %d cases against is a ratio of %.3f.".fmt(a,b,ratio).println();
   return(a,b,ratio);
}
wordList:=File("1_2_all_freq.txt").read();
```

{{out}}

```txt

E before I when preceded by C is implausible
  327 cases for and 994 cases against is a ratio of 0.329.
I before E when not preceded by C is implausible
  8148 cases for and 4826 cases against is a ratio of 1.688.
Overall the rule is implausible

```

