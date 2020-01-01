+++
title = "Password generator"
description = ""
date = 2019-05-06T22:22:36Z
aliases = []
[extra]
id = 20851
[taxonomies]
categories = []
tags = []
+++

{{task}}

Create a password generation program which will generate passwords containing random ASCII characters from the following groups:
          lower-case letters:  a ──► z
          upper-case letters:  A ──► Z
                      digits:  0 ──► 9
  other printable characters: <big><big> !"#$%&'()*+,-./:;<=>?@[]^_{|}~ </big></big>
  (the above character list excludes white-space, backslash and grave)


The generated password(s) must include   ''at least one''   (of each of the four groups):
    lower-case letter,
    upper-case letter,
    digit  (numeral),   and
    one  "other"  character.



The user must be able to specify the password length and the number of passwords to generate.

The passwords should be displayed or written to a file, one per line.

The randomness should be from a system source or library.

The program should implement a help option or button which should describe the program and options when invoked.

You may also allow the user to specify a seed value, and give the option of excluding visually similar characters.

For example:           <big><big> Il1     O0     5S     2Z </big></big>           where the characters are:
::::*   capital eye, lowercase ell, the digit one
::::*   capital oh, the digit zero
::::*   the digit five, capital ess
::::*   the digit two, capital zee
<!-- or zed, on the other side of the pond. -->





## AWK


```AWK

# syntax: GAWK -f PASSWORD_GENERATOR.AWK [-v mask=x] [-v xt=x]
#
# examples:
#   REM 4 character passwords using Rosetta Code default of: lower, upper, number, other
#   GAWK -f PASSWORD_GENERATOR.AWK
#
#   REM 8 character passwords; Rosetta Code default plus another four
#   GAWK -f PASSWORD_GENERATOR.AWK -v mask=LUNPEEEE
#
#   REM 8 character passwords ignoring Rosetta Code requirement
#   GAWK -f PASSWORD_GENERATOR.AWK -v mask=EEEEEEEE
#
#   REM help
#   GAWK -f PASSWORD_GENERATOR.AWK ?
#
# sorting:
#   PROCINFO["sorted_in"] is used by GAWK
#   SORTTYPE is used by Thompson Automation's TAWK
#
BEGIN {
    PROCINFO["sorted_in"] = "@ind_str_asc" ; SORTTYPE = 1
    srand()
# setup strings of valid characters used by mask
    arr["L"] = "abcdefghijklmnopqrstuvwxyz"        # Lower case
    arr["U"] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"        # Upper case
    arr["N"] = "0123456789"                        # Numbers
    arr["P"] = "!\"#$%&'()*+,-./:;<=>?@[]^_{|}~"   # Punctuation: I.E. other
    arr["A"] = arr["L"] arr["U"]                   # Alphabetic: lower and upper case
    arr["E"] = arr["L"] arr["U"] arr["N"] arr["P"] # Everything: lower, upper, number, punctuation
    arr["B"] = " "                                 # Blank: a space
# validate array index and length of assignment
    for (i in arr) {
      if (length(i) != 1) {
        error(sprintf("arr[%s], index is invalid",i))
      }
      if (length(arr[i]) == 0) {
        error(sprintf("arr[%s], is null",i))
      }
      mask_valids = sprintf("%s%s",mask_valids,i)
    }
# validate command line variables
    if (mask == "") {
      mask = "LUNP" # satisfy Rosetta Code task requirement
    }
    if (xt == "") {
      xt = 10 # default iterations
    }
    if (xt !~ /^[0-9]+$/) {
      error("xt is not 0-9")
    }
# validate each character in mask
    for (i=1; i<=length(mask); i++) {
      c = substr(mask,i,1)
      if (!(c in arr)) {
        error(sprintf("mask position %d is %s, invalid",i,c))
      }
    }
# help
    if (ARGV[1] == "?") {
      syntax()
      exit(0)
    }
    if (ARGC-1 != 0) {
      error("no files allowed on command line")
    }
# make passwords
    if (errors == 0) {
      for (i=1; i<=xt; i++) {
        make_password()
      }
    }
    exit(errors+0)
}
function error(message) {
    printf("error: %s\n",message)
    errors++
}
function make_password(  c,i,indx,password,valids) {
    for (i=1; i<=length(mask); i++) {  # for each character in mask
      c = substr(mask,i,1)             # extract 1 character from mask
      valids = arr[c]                  # valid characters for this position in mask
      indx = int(rand() * length(valids)) + 1
      c = substr(valids,indx,1)        # extract 1 character from list of valids
      password = password c            # build password
    }
    printf("%s\n",password)
}
function syntax(  cmd,i) {
    cmd = "GAWK -f PASSWORD_GENERATOR.AWK"
    printf("syntax: %s [-v mask=x] [-v xt=x]\n\n",cmd)
    printf("  mask  1..n bytes determines password format and length; consists of %s\n",mask_valids)
    for (i in arr) {
      printf("%9s - %s\n",i,(arr[i] == " ") ? "<space>" : arr[i])
    }
    printf("  xt    number of passwords to generate\n\n")
    printf("example: %s -v mask=%s -v xt=%s\n",cmd,mask,xt)
}

```

{{out}}

```txt

GAWK -f PASSWORD_GENERATOR.AWK -v mask=LUNPEEEE -v xt=5
iX1<-a%p
xH6]rrZ+
eK8*5@P/
cY9'kwG*
qS9=7wtU

```



## C


```c

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define DEFAULT_LENGTH 4
#define DEFAULT_COUNT 1

char* symbols[] = {"ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", "0123456789", "!\"#$%&'()*+,-./:;<=>?@[]^_{|}~"};
int length = DEFAULT_LENGTH;
int count = DEFAULT_COUNT;
unsigned seed;
char exSymbols = 0;

void GetPassword () {
    //create an array of values that determine the number of characters from each category
    int lengths[4] = {1, 1, 1, 1};
    int count = 4;
    while (count < length) {
        lengths[rand()%4]++;
        count++;
    }

    //loop through the array of lengths and set the characters in password
    char password[length + 1];
    for (int i = 0; i < length; ) {
        //pick which string to read from
        int str = rand()%4;
        if (!lengths[str])continue;   //if the number of characters for that string have been reached, continue to the next interation

        char c;
        switch (str) {
            case 2:
                c = symbols[str][rand()%10];
                while (exSymbols && (c == 'I' || c == 'l' || c == '1' || c == 'O' || c == '0' || c == '5' || c == 'S' || c == '2' || c == 'Z'))
                    c = symbols[str][rand()%10];
                password[i] = c;
            break;

            case 3:
                c = symbols[str][rand()%30];
                while (exSymbols && (c == 'I' || c == 'l' || c == '1' || c == 'O' || c == '0' || c == '5' || c == 'S' || c == '2' || c == 'Z'))
                    c = symbols[str][rand()%30];
                password[i] = c;
            break;

            default:
                c = symbols[str][rand()%26];
                while (exSymbols && (c == 'I' || c == 'l' || c == '1' || c == 'O' || c == '0' || c == '5' || c == 'S' || c == '2' || c == 'Z'))
                    c = symbols[str][rand()%26];
                password[i] = c;
            break;
        }

        i++;
        lengths[str]--;
    }

    password [length] = '\0';
    printf ("%s\n", password);
}

int main (int argc, char* argv[]) {
    seed = (unsigned)time(NULL);

    //handle user input from the command line
    for (int i = 1; i < argc; i++) {
        switch (argv[i][1]) {
            case 'l':
                if (sscanf (argv[i+1], "%d", &length) != 1) {
                    puts ("Unrecognized input. Syntax: -l [integer]");
                    return -1;
                }

                if (length < 4) {
                    puts ("Password length must be at least 4 characters.");
                    return -1;
                }
                i++;
            break;

            case 'c':
                if (sscanf (argv[i+1], "%d", &count) != 1) {
                    puts ("Unrecognized input. Syntax: -c [integer]");
                    return -1;
                }

                if (count <= 0) {
                    puts ("Count must be at least 1.");
                    return -1;
                }
                i++;
            break;

            case 's':
                if (sscanf (argv[i+1], "%d", &seed) != 1) {
                    puts ("Unrecognized input. Syntax: -s [integer]");
                    return -1;
                }
                i++;
            break;

            case 'e':
                exSymbols = 1;
            break;

            default:
                help:
                printf ("Help:\nThis program generates a random password.\n"
                "Commands:"
                   "Set password length: -l [integer]\n"
                   "Set password count: -c [integer]\n"
                   "Set seed: -s [integer]\n"
                   "Exclude similiar characters: -e\n"
                   "Display help: -h");
                return 0;
            break;
        }
    }

    srand (seed);

    for (int i = 0; i < count; i++)
        GetPassword();

    return 0;
}

```


{{out}}

```txt

./password
5zK@

./password -c 3 -l 10
L=C17QQn<$
uyC;X5kc76
D8Yw.827$]

```


=={{header|C sharp|C#}}==

```csharp
using System;
using System.Linq;

class Program
{
    const string Lower = "abcdefghijklmnopqrstuvwxyz";
    const string Upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    const string Digits = "0123456789";
    const string Symbols = "!\"#$%&'()*+,-./:;<=>?@[]^_{|}~";
    static readonly string[] Full = {Lower, Upper, Digits, Symbols, Lower + Upper + Digits + Symbols};

    const string Similar = "Il1O05S2Z";
    static readonly string[] Excluded = Full.Select(x => new string(x.Except(Similar).ToArray())).ToArray();

    static Random _rng = new Random();
    static string[] _symbolSet = Full;

    static void Main(string[] args)
    {
        int length = 12, count = 1;
        try
        {
            foreach (var x in args.Select(arg => arg.Split(':')))
            {
                switch (x[0])
                {
                    case "-l": length = int.Parse(x[1]); break;
                    case "-c": count = int.Parse(x[1]); break;
                    case "-s": _rng = new Random(x[1].GetHashCode()); break;
                    case "-x": _symbolSet = bool.Parse(x[1]) ? Excluded : Full; break;
                    default: throw new FormatException("Could not parse arguments");
                }
            }
        }
        catch { ShowUsage(); return; }
        try
        {
            for (int i = 0; i < count; i++)
                Console.WriteLine(GeneratePass(length));
        }
        catch (Exception ex) { Console.WriteLine("Error: " + ex.Message); }
    }

    static void ShowUsage()
    {
        Console.WriteLine("Usage: PASSGEN [-l:length] [-c:count] [-s:seed] [-x:(true|false)]");
        Console.WriteLine("\t-l: the length of the generated passwords");
        Console.WriteLine("\t-c: the number of passwords to generate");
        Console.WriteLine("\t-s: seed for the random number generator");
        Console.WriteLine("\t-x: exclude similar characters: " + Similar);
        Console.WriteLine("Example: PASSGEN -l:10 -c:5 -s:\"Sample Seed\" -x:true");
    }

    static string GeneratePass(int length)
    {
        var minLength = _symbolSet.Length - 1;
        if(length < minLength)
            throw new Exception("password length must be " + minLength + " or greater");

        int[] usesRemaining = Enumerable.Repeat(1, _symbolSet.Length).ToArray();
        usesRemaining[minLength] = length - minLength;
        var password = new char[length];
        for (int ii = 0; ii < length; ii++)
        {
            int set = _rng.Next(0, _symbolSet.Length);
            if (usesRemaining[set] > 0)
            {
                usesRemaining[set]--;
                password[ii] = _symbolSet[set][_rng.Next(0, _symbolSet[set].Length)];
            }
            else ii--;
        }
        return new string(password);
    }
}
```

{{out}}

```txt
PASSGEN -l:12 -c:6
Bc'9(i(&bw]G
g=9JnTy2tzMn
G8^UtYdx4D9Y
^S4uq?*yXEA=
W{3*hdk&0p?R
"4Ips4;8S3o~
```



## C++


```cpp
#include <iostream>
#include <string>
#include <algorithm>
#include <ctime>

const std::string CHR[] = { "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz",
                            "0123456789", "!\"#$%&'()*+,-./:;<=>?@[]^_{|}~" };
const std::string UNS = "O0l1I5S2Z";

std::string createPW( int len, bool safe ) {
    std::string pw;
    char t;
    for( int x = 0; x < len; x += 4 ) {
        for( int y = x; y < x + 4 && y < len; y++ ) {
            do {
                t = CHR[y % 4].at( rand() % CHR[y % 4].size() );
            } while( safe && UNS.find( t ) != UNS.npos );
            pw.append( 1, t );
        }
    }
    std::random_shuffle( pw.begin(), pw.end() );
    return pw;
}
void generate( int len, int count, bool safe ) {
    for( int c = 0; c < count; c++ ) {
        std::cout << createPW( len, safe ) << "\n";
    }
    std::cout << "\n\n";
}
int main( int argc, char* argv[] ){
    if( argv[1][1] == '?' || argc < 5 ) {
        std::cout << "Syntax: PWGEN length count safe seed /?\n"
                     "length:\tthe length of the password(min 4)\n"
                     "count:\thow many passwords should be generated\n"
                     "safe:\t1 will exclude visually similar characters, 0 won't\n"
                     "seed:\tnumber to seed the random generator or 0\n"
                     "/?\tthis text\n\n";
    } else {
        int l = atoi( argv[1] ),
            c = atoi( argv[2] ),
            e = atoi( argv[3] ),
            s = atoi( argv[4] );
        if( l < 4 ) {
            std::cout << "Passwords must be at least 4 characters long.\n\n";
        } else {
            if (s == 0) {
               std::srand( time( NULL ) );
            } else {
               std::srand( unsigned( s ) );
            }
            generate( l, c, e != 0 );
        }
    }
    return 0;
}
```

{{out}}

```txt

PWGEN 6 6 1 8
9@NcCe
4jERh!
<zX4Mr
Cob+X7
GU8_jt
rF9t%F

PWGEN 6 6 0 8
cn2ZO~
W:iRm7
0|hyJU
.h3JfL
3oq;FT
u*VT4t

```



## Ceylon



<b>module.ceylon</b>:


```ceylon

module rosetta.passwordgenerator "1.0.0" {
    import ceylon.random "1.2.2";
}


```


<b>run.ceylon:</b>


```ceylon

import ceylon.random {
    DefaultRandom,
    Random
}

Character[] lowerCaseChars = 'a'..'z';
Character[] upperCaseChars = 'A'..'Z';
Character[] numsAsChars = '0'..'9';
Character[] specialChars = "!\"#$%&'()*+,-./:;<=>?@[]^_{|}~".sequence();
Character[] allChars = lowerCaseChars.append(upperCaseChars)
                                     .append(numsAsChars)
                                     .append(specialChars);

shared void run() {
    print("2 passwords of size 8");
    print("------");
    generatePasswords(8,2).each(print);
    print("------");
    print("5 passwords of size 12");
    print("------");
    generatePasswords(12,5).each(print);
    print("------");
    print("7 passwords of size 16");
    print("------");
    generatePasswords(16,7).each(print);
    print("------");
    print("2 passwords of size 16 exclude AiX24!~");
    print("------");
    generatePasswords(16,7,"AiX24!~".sequence()).each(print);
}

String[] generatePasswords(Integer numChars, Integer numPasswords, Character[] excludedChars=[])
    => [for (count in 1..numPasswords) generatePassword(numChars,excludedChars) ];

//The generated password(s) must include at least one lower-case letter, one upper-case letter and one character from digits and 'other'.
String generatePassword(Integer numChars, Character[] excludedChars) {
    "Must provide a password length of at least 4"
    assert(numChars >= 4);

    value random = DefaultRandom();

    value fixedPartOfPassword = passwordFragmentForRequirements(random,excludedChars);

    value randomPart =
       randomPartOfPassword{ random = random;
                             index = fixedPartOfPassword.size + 1;
                             randomPart = "";
                             numChars = numChars;
                             candidateChars = filterCharsToExclude(allChars, excludedChars); };

    value unshuffledPassword = fixedPartOfPassword + randomPart;

    assert(unshuffledPassword.string.size == numChars);

    "excluded chars contained"
    assert(! unshuffledPassword.any((char) => excludedChars.contains(char)));

    "Sizes not equal"
    assert(unshuffledPassword.size == numChars);

    value shuffledPassword =
        shuffleCharsRecursive(unshuffledPassword.string,"",unshuffledPassword.indexes().sequence());

    "Sizes not equal"
    assert(shuffledPassword.string.size == numChars);

    "set of characters not equal"
    assert(set(unshuffledPassword) == set(shuffledPassword));

    return shuffledPassword;
}

String passwordFragmentForRequirements(Random random, Character[] excludedChars)
    => String({filterAndUse(random,lowerCaseChars, excludedChars),
               filterAndUse(random,upperCaseChars,excludedChars),
               filterAndUse(random,numsAsChars,excludedChars),
               filterAndUse(random,specialChars,excludedChars)});

String randomPartOfPassword(Random random,
                            Integer index,
                            String randomPart,
                            Integer numChars,
                            Character[] candidateChars) {
    if(index <= numChars) {
        value candidateCharsIndex = random.nextInteger(candidateChars.size);
        assert(exists allChar = candidateChars[candidateCharsIndex]);
        return randomPartOfPassword{ random=random;
                                     index=index+1;
                                     randomPart = randomPart + allChar.string;
                                     numChars =numChars;
                                     candidateChars = candidateChars; };
    }

    return randomPart;
}

String shuffleCharsRecursive(String orig,String shuffledString,Integer[] remainingOrigIndexes) {
    value random = DefaultRandom();

    if (nonempty constRemainingIndexes = remainingOrigIndexes) {
        value randomIndex = random.nextInteger(orig.size);

        assert(exists nextChar = orig[randomIndex]);

        String newShuffledString;
        Integer[] newRemainingIndexes;

        if (constRemainingIndexes.contains(randomIndex)) {
            newShuffledString = shuffledString + nextChar.string;
            newRemainingIndexes = constRemainingIndexes.filter((index) => randomIndex != index).sequence();
        } else {
            newShuffledString = shuffledString;
            newRemainingIndexes = constRemainingIndexes;
        }

        return shuffleCharsRecursive{ orig=orig;
                                      shuffledString=newShuffledString;
                                      remainingOrigIndexes=newRemainingIndexes; };
    }

    return shuffledString;
}

Character filterAndUse(Random random, Character[] chars, Character[] excludedChars) {
    value charsToUse = filterCharsToExclude(chars, excludedChars);
    value charToUseIndex = random.nextInteger(charsToUse.size);
    assert( exists charToUse = charsToUse[charToUseIndex]);
    return charToUse;
}

Character[] filterCharsToExclude(Character[] chars, Character[] charsToExclude)
    => chars.filter((char) => ! charsToExclude.contains(char)).sequence();

```



{{out}}

```txt

2 passwords of size 8
------
o"{#3A]F
7OZ{k-!-
------
5 passwords of size 12
------
>jA%dQ[71RR{
e-(KQgjr!H1B
XF41ALUX&odg
)vpjL6YI=S38
[=e8."8!A*Dn
------
7 passwords of size 16
------
r/|'y0G&5:UA]AF#
75}P4L>^R0'#WkLR
bK2{Tg1del{pM/}n
86mz,.NfUB3b-A4)
4KOh7D5'W<oQ=/(e
s1b8~XA*f'y6mCA8
D^RwIB9-tL:_JTD7
------
2 passwords of size 16 exclude AiX24!~
------
8bx3$y=ZPdcT"Ls3
{M$EeG{x:]"6cZO|
"{?zn{80?g^eGK&w
u{VN#jy{TZpJ}5/F
5;_VaVG?^O:6xCo%
v:xx%Ht*Zc13GekC
Mg0M:BpCbz3O0BDo

```



## Clojure


```clojure
(ns pwdgen.core
  (:require [clojure.set :refer [difference]]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(def minimum-length 4)
(def cli-options
  [["-c" "--count NUMBER" "Number of passwords to generate"
    :default 1
    :parse-fn #(Integer/parseInt %)]
   ["-l" "--length NUMBER" "Length of the generated passwords"
    :default 8
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= minimum-length %) (str "Must be greater than or equal to " minimum-length)]]
   ["-x", "--exclude-similar" "Exclude similar characters"]
   ["-h" "--help"]])

(def lowercase (map char (range (int \a) (inc (int \z)))))
(def uppercase (map char (range (int \A) (inc (int \Z)))))
(def numbers (map char (range (int \0) (inc (int \9)))))
(def specials (remove (set (concat lowercase uppercase numbers [\` \\])) (map char (range (int \!) (inc (int \~))))))
(def similar #{\I \l \1 \| \O \0 \5 \S \2 \Z})

(defn sanitize [coll options]
  (if (:exclude-similar options) (into '() (difference (set coll) similar)) coll))

(defn generate-password [options]
  (let [upper (rand-nth (sanitize uppercase options))
        lower (rand-nth (sanitize lowercase options))
        number (rand-nth (sanitize numbers options))
        special (rand-nth (sanitize specials options))
        combined (shuffle (sanitize (concat lowercase uppercase numbers specials) options))]
    (shuffle (into (list upper lower number special) (take (- (:length options) minimum-length) combined)))))

(defn -main [& args]
  (let [{:keys [options summary]} (parse-opts args cli-options)]
    (if (:help options) (println summary)
      (dotimes [n (:count options)]
        (println (apply str (generate-password options)))))))
```


{{out}}

```txt

$ lein run -- -h
  -c, --count NUMBER     1  Number of passwords to generate
  -l, --length NUMBER    8  Length of the generated passwords
  -x, --exclude-similar     Exclude similar characters
  -h, --help

$ lein run -l 20 -c 2
xn~a9UDE,rUm+x2w3o00
vlA|&fI>W57D^U:3g9L!

$ lein run --length 5 --count 3 -x
VT6-u
#kR6Y
Pj+4b

```



## Common Lisp


```lisp

(defvar *lowercase* '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                      #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

(defvar *uppercase* '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                      #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(defvar *numbers* '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defvar *special-characters* '(#\! #\\ #\# #\$ #\% #\& #\' #\( #\) #\*
                              #\+ #\, #\- #\. #\/ #\: #\; #\< #\= #\>
                              #\? #\@ #\[ #\] #\^ #\_ #\{ #\| #\} #\~))

(defvar *similar-characters* '(#\I #\l #\1 #\| #\O #\0 #\5 #\S #\2 #\Z))

(defun make-readable (s)
  (remove-if (lambda (x) (member x *similar-characters*)) s))

(defun shuffle-list (input-list)
  (loop with l = (length input-list)
     for i below l
     do (rotatef (nth i input-list)
         (nth (random l) input-list)))
  input-list)

(defun generate-password (len human-readable)
  (let*
    ((upper (if human-readable (make-readable *uppercase*) *uppercase*))
    (lower (if human-readable (make-readable *lowercase*) *lowercase*))
    (number (if human-readable (make-readable *numbers*) *numbers*))
    (special (if human-readable (make-readable *special-characters*) *special-characters*))
    (character-groups (list upper lower number special))
    (initial-password (reduce (lambda (acc x)
      (cons (nth (random (length x)) x) acc))
      character-groups :initial-value NIL)))

    (coerce (shuffle-list (reduce (lambda (acc x)
      (declare (ignore x))
      (let ((group (nth (random (length character-groups)) character-groups)))
        (cons (nth (random (length group)) group) acc)))
      (make-list (- len 4)) :initial-value initial-password)) 'string)))

(defun main (len count &optional human-readable)
  (if (< len 4)
    (print "Length must be at least 4~%")
    (loop for x from 1 to count do
      (print (generate-password len human-readable)))))

```

{{out}}

```txt

$ (main 10 4 t)
"AV}HU&?8^s"
"y~868Wr3?j"
"4wy:Wy$zR8"
"Lx'6fsU+3t"
"&zXi3s71Jf"
"7ppunc1;G1"

```



## Crystal


```Crystal
require "random/secure"

special_chars = true
similar = 0
require_groups = true
length = 20
count = 1

def show_usage
  puts <<-USAGE

       Passwords generator
       Usage: pass2 [count] [-l<length>] [-s{0|1|2}] [-ng] [-ns]

         count: number of passwords to be generated (default: 1)
         -l<length>: length of passwords (default: 20)
         -s{0|1|2}: exclude visually similar characters.
                      0 - don't exclude (default)
                      1 - exclude 0, O, 1, I, l, |
                      2 - also exclude 2, Z, 5, S
        -ng: don''t require password to include character from every group
        -ns: don''t include special chars in password

        Default value of switches is choosen to match the task in page header, but I suggest to use the "-s1 -ng -ns".
       USAGE
end

count_set = false
begin
  ARGV.each do |arg|
    case arg
    when /-l(\d+)/
      length = $1.to_i
      raise "Error: minimal length is 4" if length < 4
    when /-s(\d)/
      similar = $1.to_i
      raise "Error: minimal length is 4" unless {0, 1, 2}.includes? similar
    when /-ng/
      require_groups = false
    when /-ns/
      special_chars = false
    when /(\d+)/
      raise "incorrect arguments" if count_set
      count_set = true
      count = $1.to_i
    else
      raise "incorrect argument"
    end
  end
rescue ex
  puts ex.message
  show_usage
  exit
end

# actual program
GROUPS = [('a'..'z').to_a, ('A'..'Z').to_a, ('0'..'9').to_a]
GROUPS << "!\"#$%&'()*+,-./:;<=>?@[]^_{|}~".chars if special_chars

letters = GROUPS.flatten
letters -= "1l|I0O".chars if similar > 0
letters -= "5S2Z".chars if similar > 1

loop do
  s = Array(Char).new(length) { letters.sample(Random::Secure) }
  # it's better to just discard in order to don't lose enthropy
  next if require_groups && GROUPS.any? { |x| (s & x).size == 0 }
  puts s.join
  count -= 1
  break if count <= 0
end

```


{{out}}

```txt
user@laptop:/mnt/d/Projects/crystal$ ./pass2
0X{feScBZ_/b>7)>aL8)
user@laptop:/mnt/d/Projects/crystal$ ./pass2 -?
incorrect argument

Passwords generator
Usage: pass2 [count] [-l<length>] [-s{0|1|2}] [-ng] [-ns]

  count: number of passwords to be generated (default: 1)
  -l<length>: length of passwords (default: 20)
  -s{0|1|2}: exclude visually similar characters.
               0 - don't exclude (default)
               1 - exclude 0, O, 1, I, l, |
               2 - also exclude 2, Z, 5, S
 -ng: don't require password to include character from every group
 -ns: don't include special chars in password

 Default value of switches is choosen to match the task in page header, but I suggest to use the "-s1 -ng -ns".
user@laptop:/mnt/d/Projects/crystal$ ./pass2 10 -l20 -s2
j;K#3znen(j9%Bk6<tXf
k]h&i9[#["Pzr^zd<.9]
Xr%:7RP4%]^EXW?xB$A]
@{Jq/HWC%6.6+>BuGg/%
!99Y}8nEM$*hwRQ<-+Q>
*:"[r$tyf8zsv$%}n*M[
4nxHcMjn*v}j_qYon!qc
W#QqTu3NFF/m;Prs3]qe
i:_AEtzJQ^J]ek:G9[8=
=#_i3t]p}^gJP"z(&QaA
user@laptop:/mnt/d/Projects/crystal$ ./pass2 10 -l10 -s1 -ng -ns
fd5rHMaT6v
GzwccX2jQZ
Lx8rivLSLQ
a9NzxSQMhm
w5rrfpwJt8
RsbtZbgMxM
FcRPVxrxAA
s9fzZMwb8d
pKGyGPT7yN
ABZcXEPNWq
user@laptop:/mnt/d/Projects/crystal$

```



## Elixir


```elixir
defmodule Password do
  @lower Enum.map(?a..?z, &to_string([&1]))
  @upper Enum.map(?A..?Z, &to_string([&1]))
  @digit Enum.map(?0..?9, &to_string([&1]))
  @other ~S"""
!"#$%&'()*+,-./:;<=>?@[]^_{|}~
""" |> String.codepoints |> List.delete_at(-1)
  @all @lower ++ @upper ++ @digit ++ @other

  def generator do
    {len, num} = get_argv
    Enum.each(1..num, fn _ ->
      pswd = [Enum.random(@lower), Enum.random(@upper), Enum.random(@digit), Enum.random(@other)]
      IO.puts generator(len-4, pswd)
    end)
  end

  def generator(0, pswd), do: Enum.shuffle(pswd) |> Enum.join
  def generator(len, pswd), do: generator(len-1, [Enum.random(@all) | pswd])

  def get_argv do
    {len,num} = case System.argv do
      ["?"]     -> usage
      []        -> {8,1}
      [len]     -> {String.to_integer(len), 1}
      [len,num] -> {String.to_integer(len), String.to_integer(num)}
      _         -> usage
    end
    if len<4 or num<1, do: usage, else: {len,num}
  end

  defp usage do
    IO.puts ["Password generator\n",
             "Usage: [password length(=8)] [number of passwords to generate(=1)]"]
    exit(:normal)
  end
end

Password.generator
```


{{out}}

```txt

Elixir> elixir pswdgen.exs
Yu4*CSi|

Elixir> elixir pswdgen.exs 20 5
TWRfdAVYf_jl"Y~8gzx]
-)-D8Xz_fq5}c[STq("9
)xc30V06lEW&>k{9L_ON
GU$t*{887i5Ef@y(%VN'
}(b!m:DT%A7tLy[|qz{C

```


=={{header|F_Sharp|F#}}==

### The function


```fsharp

// A function to generate passwords of a given length. Nigel Galloway: May 2nd., 2018
let N = (set)"qwertyuiopasdfghjklzxcvbnm"
let I = (set)"QWERTYUIOPASDFGHJKLZXCVBNM"
let G = (set)"7894561230"
let E = (set)"""!"#$%&'()*+,-./:;<=>?@[]^_{|}~[||]"""
let L = Array.ofSeq (Set.unionMany [N;I;G;E])
let y = System.Random 23
let pWords n=
  let fN n = not (Set.isEmpty (Set.intersect N n )||Set.isEmpty (Set.intersect I n )||Set.isEmpty (Set.intersect G n )||Set.isEmpty (Set.intersect E n ))
  Seq.initInfinite(fun _->(set)(List.init n (fun _->L.[y.Next()%(Array.length L)])))|>Seq.filter fN|>Seq.map(Set.toArray >> System.String)

```


### A possible use

Print 5 password of length 8

```fsharp

pWords 8 |> Seq.take 5 |> Seq.iter(printfn "%s")

```

{{out}}

```txt

+06:?H^w
$36=?Ofn
37<@TXk
,169GJWl
&6CDelow

```


## Gambas

'''[https:c/gambas-playground.proko.eu/?gist=0ef1242c761d8a39297fb913fc6a56c0 Click this link to run this code]'''

```gambas
' Gambas module file

' INSTRUCTIONS
' I have not used a GUI as you could not run this in the 'Gambas Playground'
' Click on the link above to run this program
' The user can specify the password length and the number of passwords
' to generate by altering the values of the 2 lines below.

Public Sub Main()
Dim siPasswordLength As Short = 20                                      'Password length
Dim siPasswordQuantity As Short = 20                                    'Password quantity
Dim sLower As String = "abcdefghijklmnopqrstuvwxyz"                     'Lower case characters
Dim sUpper As String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     'Upper case characters
Dim sNumber As String = "1234567890"                                    'Numbers
Dim sOther As String = "'!#$%&'()*+,-./:;<=>?@[]^_{|}~" & Chr(34)       'Other characters + quote
Dim sNoGo As String[] = ["I1", "1I", "l1", "1l", "Il",
                        "lI", "O0", "0O", "S5", "5S", "Z2", "2Z"]       'Undesirable string combinations (can be added to if required)
Dim sData As String = sLower & sUpper & sNumber & sOther                'Create 1 string to pick the password characters from
Dim sToCheck, sPassword As String                                       'To hold a possible password for checking, to hold the passwords
Dim siCount, siLoop, siCounter As Short                                 'Various counters
Dim bPass As Boolean                                                    'To Pass or not to Pass!

For siCount = 1 To siPasswordQuantity                                   'Loop the amount of passwords required
  For siLoop = 1 To siPasswordLength                                    'Loop for each charater of the required length
    sToCheck &= Mid(sData, Rand(1, Len(sData)), 1)                      'Get a random character from sData
  Next

  bPass = False                                                         'Set bPass to False
  For siCounter = 1 To Len(sToCheck)                                    'Loop through each character in the generated password
    If InStr(sLower, Mid(sToCheck, siCounter, 1)) Then bPass = True     'If a LOWER CASE letter is included set bPass to True
  Next

  If bPass Then                                                         'If bPass is True then
    bPass = False                                                       'bPass is False
    For siCounter = 1 To Len(sToCheck)                                  'Loop through each character in the generated password
      If InStr(sUpper, Mid(sToCheck, siCounter, 1)) Then bPass = True   'If an UPPER CASE letter is included set bPass to True
    Next
  End If

  If bPass Then                                                         'If bPass is True then
    bPass = False                                                       'bPass is False
    For siCounter = 1 To Len(sToCheck)                                  'Loop through each character in the generated password
      If InStr(sNumber, Mid(sToCheck, siCounter, 1)) Then bPass = True  'If a NUMBER is included set bPass to True
    Next
  End If

  If bPass Then                                                         'If bPass is True then
    bPass = False                                                       'bPass is False
    For siCounter = 1 To Len(sToCheck)                                  'Loop through each character in the generated password
      If InStr(sOther, Mid(sToCheck, siCounter, 1)) Then bPass = True   'If an 'OTHER CHARACTER' is included set bPass to True
    Next
  End If

  If bPass Then
    For siCounter = 1 To sNoGo.Max                                      'Loop through each undesirable strings e.g. "0O"
      If InStr(sToCheck, sNoGo[siCounter]) Then bPass = False           'If an undesirable combination is located then set bPass to False
    Next
  Endif

  If bPass = True Then                                                  'If bPass is True (all checks have been passed) then
    sPassword &= sToCheck & gb.NewLine                                  'Add the new password to sPassword with a newline
  Else                                                                  'Else
    Dec siCount                                                         'Decrease the loop counter by one
  Endif
  sToCheck = ""                                                         'Clear sToCheck
Next

Print sPassword                                                         'Print the password list

End
```

Output:

```txt

+j<Zwk,h&0-Bb976hs^B
HCMC2T,rI_&&wBjavysX
/OS6'd-+o|:'[L$1|u56
DP/<$27oa/c)[/t1YI@F
k@fu3yk=vsXsA!3rENN^
7~V?DDZl$W>mwA'tn5~9
:K]LWNxdrUmhwj_2>-85
,/NLH}#r8DeytFkFl.[>
6&mgbo11r)IbI;n@RDal
K$0m^J0r3fk~r56?H+5:
;"AT-D9m^*VKVM"0Gx|}
8k}IT{DAV|P!'wl}64g$
jq3G$h^D&19>?+_9q^:Z
+O$0z;1zN5y'*(79H$Q,

```



## Go


```Go

package main

import (
	"crypto/rand"
	"math/big"
	"strings"
	"flag"
	"math"
        "fmt"
)

var lowercase = "abcdefghijklmnopqrstuvwxyz"
var uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var numbers = "0123456789"
var signs = "!\"#$%&'()*+,-./:;<=>?@[]^_{|}~"
var similar = "Il1O05S2Z"

func check(e error){
	if e != nil {
		panic(e)
	}
}

func randstr(length int, alphastr string) string{
	alphabet := []byte(alphastr)
	pass := make([]byte,length)
	for i := 0; i < length; i++ {
		bign, err := rand.Int(rand.Reader, big.NewInt(int64(len(alphabet))))
		check(err)
		n := bign.Int64()
		pass[i] = alphabet[n]
	}
	return string(pass)
}

func verify(pass string,checkUpper bool,checkLower bool, checkNumber bool, checkSign bool) bool{
	isValid := true
	if(checkUpper){
		isValid = isValid && strings.ContainsAny(pass,uppercase)
	}
	if(checkLower){
		isValid = isValid && strings.ContainsAny(pass,lowercase)
	}
	if(checkNumber){
		isValid = isValid && strings.ContainsAny(pass,numbers)
	}
	if(checkSign){
		isValid = isValid && strings.ContainsAny(pass,signs)
	}
	return isValid
}


func main() {
	passCount := flag.Int("pc", 6, "Number of passwords")
	passLength := flag.Int("pl", 10, "Passwordlength")
	useUpper := flag.Bool("upper", true, "Enables or disables uppercase letters")
	useLower := flag.Bool("lower", true, "Enables or disables lowercase letters")
	useSign := flag.Bool("sign", true, "Enables or disables signs")
	useNumbers := flag.Bool("number", true, "Enables or disables numbers")
	useSimilar := flag.Bool("similar", true,"Enables or disables visually similar characters")
	flag.Parse()

	passAlphabet := ""
	if *useUpper {
		passAlphabet += uppercase
	}
	if *useLower {
		passAlphabet += lowercase
	}
	if *useSign {
		passAlphabet += signs
	}
	if *useNumbers {
		passAlphabet += numbers
	}
	if !*useSimilar {
		for _, r := range similar{
			passAlphabet = strings.Replace(passAlphabet,string(r),"", 1)
		}
	}
	fmt.Printf("Generating passwords with an average entropy of %.1f bits \n", math.Log2(float64(len(passAlphabet))) * float64(*passLength))
	for i := 0; i < *passCount;i++{
		passFound := false
		pass := ""
		for(!passFound){
			pass = randstr(*passLength,passAlphabet)
			passFound = verify(pass,*useUpper,*useLower,*useNumbers,*useSign)
		}
		fmt.Println(pass)
	}
}

```

{{out}}

```txt

Generating passwords with an average entropy of 65.2 bits
.JJ%z({4,x
.[n9d7,f8U
1Y:)pL7&R6
:~+I5xCh1#
%6|_gJ!"}m
jOU8z^f_1J

```



## Haskell


The password generation process.

The function <code>password</code> for given length and a list of char sets which should be included, generates random password.


```Haskell
import Control.Monad
import Control.Monad.Random
import Data.List

password :: MonadRandom m => [String] -> Int -> m String
password charSets n = do
  parts <- getPartition n
  chars <- zipWithM replicateM parts (uniform <$> charSets)
  shuffle (concat chars)
  where
    getPartition n = adjust <$> replicateM (k-1) (getRandomR (1, n `div` k))
    k = length charSets
    adjust p = (n - sum p) : p

shuffle :: (Eq a, MonadRandom m) => [a] -> m [a]
shuffle [] = pure []
shuffle lst = do
  x <- uniform lst
  xs <- shuffle (delete x lst)
  return (x : xs)
```


For example:


```txt
λ> password ["abcxyz","123"] 10
"x3b1y2ac3z"

λ> replicateM 5 (password ["abcxyz","123","#!"] 8)
["y!b#zyx2","xzcx!1xb","3zcax#aa","!ca1a2c#","#21a#zyc"]
```


User interface (uses a powerful and easy-to-use command-line  [https://hackage.haskell.org/package/options-1.2.1.1/docs/Options.html option parser]).


```Haskell
import Options

data Opts = Opts { optLength :: Int
                 , optCount :: Int
                 , optReadable :: Bool }

instance Options Opts where
    defineOptions = Opts <$>
      simpleOption "length" 8 "password length" <*>
      simpleOption "count" 1 "number of passwords to be generated" <*>
      simpleOption "readable" False "Whether to use only readable characters"

main = runCommand $ \opts args -> do
  let n = optCount opts
      l = optLength opts
      s = if (optReadable opts)
          then zipWith (\\) charSets visualySimilar
          else charSets
  res <- replicateM n (password s (max 4 l))
  mapM_ putStrLn res
  where
    charSets = [ ['a'..'z']
               , ['A'..'Z']
               , ['0'..'9']
               , "!\"#$%&'()*+,-./:;<=>?@[]^_{|}~" ]

    visualySimilar = ["l","IOSZ","012","!|.,"]
```


{{Out}}


```txt
$ ./password -h
Help Options:
  -h, --help
    Show option summary.
  --help-all
    Show all help options.

Application Options:
  --length :: int
    password length
    default: 8
  --count :: int
    number of passwords to be generated
    default: 1
  --readable :: bool
    whether to avoid visually similar characters
    default: false

$ ./password
PBpx#9hj

$ ./password --length 10 --count 10
2gso<zJtn@
sv:W7inoWk
fovo0mUO:q
[4bC4liy,k
s?cwr2JbTm
z2mrZpav!x
2#bq0rwoOI
W*pv19wrjb
Yx2j)f6is"
j6YM0_qd&v

$ ./password --readable --length 10 --count 10
bsx<xL8aA"
c&~E6ja5oY
Ri7tg7h$hz
oekF}9ab6v
jmmK5_cii7
P@xcqspY8d
kdnkgJ*Ak8
ue~uAuN7g+
gzbkK9$6ye
m#jdQan94z
```



## J


Implementation:


```J
thru=: <. + i.@(+*)@-~
chr=: a.&i.

lower=: 'a' thru&.chr 'z'
upper=: 'A' thru&.chr 'Z'
digit=: '0' thru&.chr '9'
other=: ('!' thru&.chr '~')-.lower,upper,digit,'`\'
all=: lower,upper,digit,other

pwgen =:verb define"0 :: pwhelp
  NB. pick one of each, remainder from all, then shuffle
  (?~y) { (?@# { ])every lower;upper;digit;other;(y-4)#<all
:
  pwgen x#y
)

pwhelp =:echo bind (noun define)
  [x] pwgen y - generates passwords of length y
  optional x says how many to generate (if you want more than 1)

  y must be at least 4 because
  passwords must contain four different kinds of characters.
)
```


Example use (from J command line):


```J
   pwgen'help'
  [x] pwgen y - generates passwords of length y
  optional x says how many to generate (if you want more than 1)

  y must be at least 4 because
  passwords must contain four different kinds of characters.

   pwgen 10
S%"x8X0}:K
   5 pwgen 10
+u.9(XDM.O
]a@Yb6j~DI
Oo?|2oc4yi
9V9[EJ:Txs
$vYd(>4L:m
```



## Java

{{works with|Java|7}}

```java
import java.util.*;

public class PasswordGenerator {
    final static Random rand = new Random();

    public static void main(String[] args) {
        int num, len;

        try {
            if (args.length != 2)
                throw new IllegalArgumentException();

            len = Integer.parseInt(args[0]);
            if (len < 4 || len > 16)
                throw new IllegalArgumentException();

            num = Integer.parseInt(args[1]);
            if (num < 1 || num > 10)
                throw new IllegalArgumentException();

            for (String pw : generatePasswords(num, len))
                System.out.println(pw);

        } catch (IllegalArgumentException e) {
            String s = "Provide the length of the passwords (min 4, max 16) you "
                    + "want to generate,\nand how many (min 1, max 10)";
            System.out.println(s);
        }
    }

    private static List<String> generatePasswords(int num, int len) {
        final String s = "!\"#$%&'()*+,-./:;<=>?@[]^_{|}~";

        List<String> result = new ArrayList<>();

        for (int i = 0; i < num; i++) {
            StringBuilder sb = new StringBuilder();
            sb.append(s.charAt(rand.nextInt(s.length())));
            sb.append((char) (rand.nextInt(10) + '0'));
            sb.append((char) (rand.nextInt(26) + 'a'));
            sb.append((char) (rand.nextInt(26) + 'A'));

            for (int j = 4; j < len; j++) {
                int r = rand.nextInt(93) + '!';
                if (r == 92 || r == 96) {
                    j--;
                } else {
                    sb.append((char) r);
                }
            }
            result.add(shuffle(sb));
        }
        return result;
    }

    public static String shuffle(StringBuilder sb) {
        int len = sb.length();
        for (int i = len - 1; i > 0; i--) {
            int r = rand.nextInt(i);
            char tmp = sb.charAt(i);
            sb.setCharAt(i, sb.charAt(r));
            sb.setCharAt(r, tmp);
        }
        return sb.toString();
    }
}
```



```txt
7V;m
4u>I
J2$s
I8~h

2Q4,dD.Y'z>8
<|U)qS7S9d_M
c4*hLisq^e26
'H$1e)BpYh*8
*'R8yEmvh9v]
```



## Julia

{{works with|Julia|0.6}}


```julia
function passgen(len::Integer; simchars::Bool=true)::String
    if len < 4; error("length must be at least 4") end
    # Definitions
    DIGIT = collect('0':'9')
    UPPER = collect('A':'Z')
    LOWER = collect('a':'z')
    OTHER = collect("!\"#\$%&'()*+,-./:;<=>?@[]^_{|}~")
    if !simchars
        setdiff!(DIGIT, ['0', '1', '2', '5'])
        setdiff!(UPPER, ['O', 'I', 'Z', 'S'])
        setdiff!(LOWER, [     'l'])
    end
    ALL = union(DIGIT, UPPER, LOWER, OTHER)

    chars = collect(rand(set) for set in (DIGIT, UPPER, LOWER, OTHER))
    len -= 4
    append!(chars, rand(ALL, len))

    return join(shuffle!(chars))
end

function passgen(io::IO, len::Int=8, npass::Int=1; seed::Int=-1, simchars::Bool=true)::Vector{String}
    if seed > -1; srand(seed) end
    passwords = collect(passgen(len; simchars=simchars) for i in 1:npass)
    writedlm(io, passwords, '\n')
    return passwords
end

passgen(STDOUT, 10, 12; seed = 1)
```


{{out}}

```txt
p-7w^hK~8r
fDU~f^b5)J
j5.phuRNgB
[/0'0AUUFx
8h;BPr$#[[
R*8h<xAetD
ZelB^Y8lT0
"c4S""j/2QoZ"
~C-o18d6eH
TdrmQ(7?1~
^UH&Iu1*l9
sg8SAF;<4y
```



## Kotlin


```Groovy
// version 1.1.4-3

import java.util.Random
import java.io.File

val r = Random()
val rr = Random()  // use a separate generator for shuffles
val ls = System.getProperty("line.separator")

var lower = "abcdefghijklmnopqrstuvwxyz"
var upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var digit = "0123456789"
var other = """!"#$%&'()*+,-./:;<=>?@[]^_{|}~"""

val exclChars = arrayOf(
    "'I', 'l' and '1'",
    "'O' and '0'     ",
    "'5' and 'S'     ",
    "'2' and 'Z'     "
)

fun String.shuffle(): String {
    val sb = StringBuilder(this)
    var n = sb.length
    while (n > 1) {
        val k = rr.nextInt(n--)
        val t = sb[n]
        sb[n] = sb[k]
        sb[k] = t
    }
    return sb.toString()
}

fun generatePasswords(pwdLen: Int, pwdNum: Int, toConsole: Boolean, toFile: Boolean) {
    val sb = StringBuilder()
    val ll = lower.length
    val ul = upper.length
    val dl = digit.length
    val ol = other.length
    val tl = ll + ul + dl + ol
    var fw = if (toFile) File("pwds.txt").writer() else null

    if (toConsole) println("\nThe generated passwords are:")
    for (i in 0 until pwdNum) {
        sb.setLength(0)
        sb.append(lower[r.nextInt(ll)])
        sb.append(upper[r.nextInt(ul)])
        sb.append(digit[r.nextInt(dl)])
        sb.append(other[r.nextInt(ol)])

        for (j in 0 until pwdLen - 4) {
            val k = r.nextInt(tl)
            sb.append(when (k) {
                in 0 until ll -> lower[k]
                in ll until ll + ul -> upper[k - ll]
                in ll + ul until tl - ol -> digit[k - ll - ul]
                else -> other[tl - 1 - k]
            })
        }
        var pwd = sb.toString()
        repeat(5) { pwd = pwd.shuffle() } // shuffle 5 times say
        if (toConsole) println("  ${"%2d".format(i + 1)}:  $pwd")
        if (toFile) {
            fw!!.write(pwd)
            if (i < pwdNum - 1) fw.write(ls)
        }
    }
    if (toFile) {
       println("\nThe generated passwords have been written to the file pwds.txt")
       fw!!.close()
    }
}

fun printHelp() {
    println("""
        |This program generates up to 99 passwords of between 5 and 20 characters in
        |length.
        |
        |You will be prompted for the values of all parameters when the program is run
        |- there are no command line options to memorize.
        |
        |The passwords can either be written to the console or to a file (pwds.txt),
        |or both.
        |
        |The passwords must contain at least one each of the following character types:
        |   lower-case letters :  a -> z
        |   upper-case letters :  A -> Z
        |   digits             :  0 -> 9
        |   other characters   :  !"#$%&'()*+,-./:;<=>?@[]^_{|}~
        |
        |Optionally, a seed can be set for the random generator
        |(any non-zero Long integer) otherwise the default seed will be used.
        |Even if the same seed is set, the passwords won't necessarily be exactly
        |the same on each run as additional random shuffles are always performed.
        |
        |You can also specify that various sets of visually similar characters
        |will be excluded (or not) from the passwords, namely: Il1  O0  5S  2Z
        |
        |Finally, the only command line options permitted are -h and -help which
        |will display this page and then exit.
        |
        |Any other command line parameters will simply be ignored and the program
        |will be run normally.
        |
    """.trimMargin())
}

fun main(args: Array<String>) {
    if (args.size == 1 && (args[0] == "-h" || args[0] == "-help")) {
       printHelp()
       return
    }

    println("Please enter the following and press return after each one")

    var pwdLen: Int?
    do {
       print("  Password length (5 to 20)     : ")
       pwdLen = readLine()!!.toIntOrNull() ?: 0
    }
    while (pwdLen !in 5..20)

    var pwdNum: Int?
    do {
       print("  Number to generate (1 to 99)  : ")
       pwdNum = readLine()!!.toIntOrNull() ?: 0
    }
    while (pwdNum !in 1..99)

    var seed: Long?
    do {
       print("  Seed value (0 to use default) : ")
       seed = readLine()!!.toLongOrNull()
    }
    while (seed == null)
    if (seed != 0L) r.setSeed(seed)

    println("  Exclude the following visually similar characters")
    for (i in 0..3) {
        var yn: String
        do {
            print("    ${exclChars[i]} y/n : ")
            yn = readLine()!!.toLowerCase()
        }
        while (yn != "y" && yn != "n")
        if (yn == "y") {
            when (i) {
                0 -> {
                    upper = upper.replace("I", "")
                    lower = lower.replace("l", "")
                    digit = digit.replace("1", "")
                }

                1 -> {
                    upper = upper.replace("O", "")
                    digit = digit.replace("0", "")
                }

                2 -> {
                    upper = upper.replace("S", "")
                    digit = digit.replace("5", "")
                }

                3 -> {
                    upper = upper.replace("Z", "")
                    digit = digit.replace("2", "")
                }
            }
        }
    }

    var toConsole: Boolean?
    do {
        print("  Write to console   y/n : ")
        val t = readLine()!!
        toConsole = if (t == "y") true else if (t == "n") false else null
    }
    while (toConsole == null)

    var toFile: Boolean? = true
    if (toConsole) {
        do {
            print("  Write to file      y/n : ")
            val t = readLine()!!
            toFile = if (t == "y") true else if (t == "n") false else null
        }
        while (toFile == null)
    }

    generatePasswords(pwdLen!!, pwdNum!!, toConsole, toFile!!)
}
```


Sample input and output:

```txt

Please enter the following and press return after each one
  Password length (5 to 20)     : 8
  Number to generate (1 to 99)  : 10
  Seed value (0 to use default) : 0
  Exclude the following visually similar characters
    'I', 'l' and '1' y/n : n
    'O' and '0'      y/n : n
    '5' and 'S'      y/n : n
    '2' and 'Z'      y/n : n
  Write to console   y/n : y
  Write to file      y/n : y

The generated passwords are:
   1:  n09&VswT
   2:  b/:7XL'r
   3:  k1<M2T[/
   4:  1uWCkx6*
   5:  Rr?/4)'d
   6:  _i5,%jAy
   7:  1(Jn]ZD+
   8:  qa1lGNo)
   9:  cfr9{}{K
  10:  OPO3b{tY

The generated passwords have been written to the file pwds.txt

```



## Lua


```Lua
function randPW (length)
    local index, pw, rnd = 0, ""
    local chars = {
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        "abcdefghijklmnopqrstuvwxyz",
        "0123456789",
        "!\"#$%&'()*+,-./:;<=>?@[]^_{|}~"
    }
    repeat
        index = index + 1
        rnd = math.random(chars[index]:len())
        if math.random(2) == 1 then
            pw = pw .. chars[index]:sub(rnd, rnd)
        else
            pw = chars[index]:sub(rnd, rnd) .. pw
        end
        index = index % #chars
    until pw:len() >= length
    return pw
end

math.randomseed(os.time())
if #arg ~= 2 then
    print("\npwgen.lua")
    print("
### ===
\n")
    print("A Lua script to generate random passwords.\n")
    print("Usage:    lua pwgen.lua [password length] [number of passwords to generate]\n")
    os.exit()
end
for i = 1, arg[2] do print(randPW(tonumber(arg[1]))) end
```

Command line session:

```txt
>lua pwgen.lua

pwgen.lua

### ===


A Lua script to generate random passwords.

Usage:  lua pwgen.lua [password length] [number of passwords to generate]


>lua pwgen.lua 8 4
!A%1Ghs5
)cU?2eT5
9wHh1'V/
6lN9*Nx<
```



## Mathematica



```Mathematica

(* Length is the Length of the password, num is the number you want, \
and similar=1 if you want similar characters, 0 if not. True and \
False, should work in place of 1/0 *)
pwgen[length_Integer: 5, num_Integer: 1, similars_Boolean: 1] :=
 pwgenerator[length, num, similars] =
  Module[{list, valid, validchars, similar, k, j, password, pwlist,
    lcase, ucase, digits, spec, s}, lcase = Alphabet[];
   ucase = Capitalize[lcase];
   digits = Range[0, 9];
   spec = StringPartition["!:#$%\'()*+,-./:;>=<?@{}[]^_|~", 1];
   validchars = Flatten[Union[lcase, ucase, digits, spec]];
   similar = StringPartition["Il10O5S2Z", 1];
   list = {};

   Table[valid = 0;
    While[valid == 0,
     For[j = 0; k = {};, j < length, j++,
      AppendTo[k, RandomInteger[{1, Length[validchars]}]]];
     k = Flatten[k];
     password = validchars[[k]];
     Which[(Intersection[password, similar] >= 1 && similars == 0 ),
      valid = 0;, (Intersection[password, similar] == 0 &&
        similars == 1),
      valid = 0;, (Intersection[password, similar] == 0 &&
        similars == 0 ), valid = 1;
      Return[password], (Intersection[password, similar] >= 1 &&
        similars == 1), valid = 1; Return[password]];
     ], {num}];


   ]

```




## OCaml



```ocaml
let lower = "abcdefghijklmnopqrstuvwxyz"
let upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let digit = "0123456789"
let other = "!\"#$%&'()*+,-./:;<=>?@[]^_{|}~"
(* unconfuse syntax highlighter: " *)

let visually_similar = "Il1O05S2Z"


let mk_pwd len readable =
  let get_char i =
    match i mod 4 with
    | 0 -> lower.[Random.int (String.length lower)]
    | 1 -> upper.[Random.int (String.length upper)]
    | 2 -> digit.[Random.int (String.length digit)]
    | 3 -> other.[Random.int (String.length other)]
    | _ -> raise Exit
  in
  let f =
    if readable then
      (fun i ->
        let rec aux () =
          let c = get_char i in
          if String.contains visually_similar c
          then aux ()
          else (String.make 1 c)
        in
        aux ()
      )
    else
      (fun i ->
        let c = get_char i in
        (String.make 1 c)
      )
  in
  let r = Array.init len f in
  Array.sort (fun _ _ -> (Random.int 3) - 1) r;
  (String.concat "" (Array.to_list r))


let () =
  Random.self_init ();
  let num = ref 1 in
  let len = ref 8 in
  let readable = ref false in
  let speclist = [
    "-n", Arg.Set_int num, "number of passwords";
    "-c", Arg.Set_int len, "number of characters";
    "--readable", Arg.Set readable, "readable";
    "--rand-init", Arg.String (fun s ->
        Random.full_init
          (Array.map int_of_char (Array.of_seq (String.to_seq s)))
      ), "initialise the random generator with a string";
  ] in
  Arg.parse speclist (fun s -> invalid_arg s) "Password generator";
  for i = 1 to !num do
    print_endline (mk_pwd !len !readable)
  done
```


{{out}}


```txt
$ ocaml mkpwd.ml --help
Password generator
  -n number of passwords
  -c number of characters
  --readable readable
  --rand-init initialise the random generator with a string
  --help  Display this list of options

$ ocaml mkpwd.ml
Zz&tJ[40

$ ocaml mkpwd.ml -n 3 -c 16 --readable --rand-init rosettacode.org
_6yAXM(o4+Gw~73p
]]Pf}84"bwKUH84b
.TijWj7~743Q<K;q
```



## ooRexx

{{trans||REXX}}

```oorexx
/*REXX program generates a  random password  according to the Rosetta Code task's rules.*/
parse arg L N seed xxx dbg                       /*obtain optional arguments from the CL*/
casl= 'abcdefghijklmnopqrstuvwxyz'               /*define lowercase alphabet.           */
casu= translate(casle)                           /*define uppercase alphabet.           */
digs= '0123456789'                               /*define digits.                       */
/* avoiding the ambiguous characters Il1 o0 5S                                          */
casl= 'abcdefghijkmnpqrtuvwxy'                   /*define lowercase alphabet.           */
casu= translate(casl)                            /*define uppercase alphabet.           */
digs= '0123456789'                               /*define digits.                       */
spec= '''!"#$%&()+,-./:;<=>?@[]^{|}~'            /*define a bunch of special characters.*/
if L='?'               then call help            /*does user want documentation shown?  */
if L=''  | L=","       then L=8                  /*Not specified?  Then use the default.*/
If N=''  | N=","       then N=1                  /* "      "         "   "   "     "    */
If xxx=''| xxx=","     then xxx='1lI0O2Z5S'      /* "      "         "   "   "     "    */
if seed>'' &,
   seed<>','            then Do
  if \datatype(seed,'W')then call ser "seed is not an integer:" seed
  Call random ,,seed                             /*the seed for repeatable RANDOM BIF #s*/
  End
casl=remove(xxx,casl)
casu=remove(xxx,casu)
digs=remove(xxx,digs)
Say 'casl='casl
Say 'casu='casu
Say 'digs='digs
Say 'spec='spec
if \datatype(L,   'W')  then call ser "password length, it isn't an integer: "       L
if L<4                  then call ser "password length, it's too small: "            L
if L>80                 then call ser "password length, it's too large: "            L
if \datatype(N,   'W')  then call ser "number of passwords, it isn't an integer: "   N
if N<0                  then call ser "number of passwords, it's too small: "        N

    do g=1  for N                                /*generate  N  passwords (default is 1)*/
    pw=letterL()||letterU()||numeral()||special()/*generate  4  random  PW constituents.*/
            do k=5  to  L;       z=random(1, 4)  /* [?]  flush out PW with more parts.  */
            if z==1  then pw=pw || letterL()     /*maybe append random lowercase letter.*/
            if z==2  then pw=pw || letterU()     /*  "      "      "   uppercase    "   */
            if z==3  then pw=pw || numeral()     /*  "      "      "       numeral      */
            if z==4  then pw=pw || special()     /*  "      "      "   special character*/
            end   /*k*/                          /* [?]  code below randomizes PW chars.*/
    t=length(pw)                                 /*the length of the password (in bytes)*/
            do L+L                               /*perform a random number of char swaps*/
            a=random(1,t);     x=substr(pw,a,1)  /*A: 1st char location;  X is the char.*/
            b=random(1,t);     y=substr(pw,b,1)  /*B: 2nd   "      "      Y  "  "    "  */
            pw=overlay(x,pw,b);  pw=overlay(y,pw,a)  /* swap the two chars.             */
            end  /*swaps*/                       /* [?]  perform extra swap to be sure. */
    say right(g,length(N))  'password is: ' pw counts() /*display the  Nth  password    */
    end       /*g*/
exit                                             /*stick a fork in it,  we're all done. */
/*--------------------------------------------------------------------------------------*/
ser:      say;   say '***error*** invalid' arg(1);  exit 13   /*display an error message*/
letterL:  return substr(casl, random(1, length(casl)), 1)   /*return random lowercase.*/
letterU:  return substr(casu, random(1, length(casu)), 1)   /*   "      "   uppercase.*/
numeral:  return substr(digs, random(1, length(digs)), 1)   /*   "      "    numeral. */
special:  return substr(spec, random(1, length(spec)), 1)   /*   "      " special char*/
remove: Procedure
  Parse arg nono,s
  Return space(translate(s,'',nono),0)
/*--------------------------------------------------------------------------------------*/
counts:
  If dbg>'' Then Do
    cnt.=0
    str.=''
    Do j=1 To length(pw)
      c=substr(pw,j,1)
      If pos(c,casL)>0 Then Do; cnt.0casL=cnt.0casL+1; str.0casL=str.0casL||c; End
      If pos(c,casU)>0 Then Do; cnt.0casU=cnt.0casU+1; str.0casU=str.0casU||c; End
      If pos(c,digs)>0 Then Do; cnt.0digs=cnt.0digs+1; str.0digs=str.0digs||c; End
      If pos(c,spec)>0 Then Do; cnt.0spec=cnt.0spec+1; str.0spec=str.0spec||c; End
      End
    txt=cnt.0casL cnt.0casU cnt.0digs cnt.0spec
    If pos(' 0 ',txt)>0 Then
      txt=txt 'error'
    Return txt str.0casL str.0casU str.0digs str.0spec
    End
  Else
    txt=''
  Return txt
help:     signal .; .:   do j=sigL+2  to sourceline()-1; say sourceline(j); end;    exit 0
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~ documentation begins on next line.~~~~~~~~~~~~~~~~~~~~~~~~~
+-----------------------------------------------------------------------------+
¦ Documentation for the  GENPW  program:                                      ¦
¦                                                                             ¦
¦ Rexx genpwd <length|,> <howmany|,> <seed|,> <xxx|,>     <dbg>               ¦
¦                     8           1     n     '1lI0O2Z5S'  none    Defaults   ¦
¦                                                                             ¦
¦--- where:                                                                   ¦
¦           length      is the length of the passwords to be generated.       ¦
¦                       The default is  8.                                    ¦
¦                       If a  comma (,)  is specified, the default is used.   ¦
¦                       The minimum is  4,   the maximum is  80.              ¦
¦                                                                             ¦
¦           howMany     is the number of passwords to be generated.           ¦
¦                       The default is  1.                                    ¦
¦           xxx         Characters NOT to be used in generated passwords      ¦
¦           dbg         Schow count of characters in the 4 groups             ¦
+-----------------------------------------------------------------------------+
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~documentation ends on the previous line.~~~~~~~~~~~~~~~~~~~*/
```

{{out}}

```txt
D:\>rexx genpwd 12 4 33 , x
casl=abcdefghijkmnpqrtuvwxy
casu=ABCDEFGHJKMNPQRTUVWXY
digs=346789
spec='!"#$%&()+,-./:;<=>?@[]^{|}~
1 password is:  B78<3(3Et(U8 1 3 5 3 t BEU 78338 <((
2 password is:  v+k6X6AV^;[7 2 3 3 4 vk XAV 667 +^;[
3 password is:  x3MV:7?CcH{D 2 5 2 3 xc MVCHD 37 :?{
4 password is:  C49PHCj[~D9/ 1 5 3 3 j CPHCD 499 [~/

D:\>regina genpwd 12 4 33 , x
casl=abcdefghijkmnpqrtuvwxy
casu=ABCDEFGHJKMNPQRTUVWXY
digs=346789
spec='!"#$%&()+,-./:;<=>?@[]^{|}~
1 password is:  e4)XnD4jq"xb 6 2 2 2 enjqxb XD 44 )"
2 password is:  9r3H:a97HyW8 3 3 5 1 ray HHW 93978 :
3 password is:  y76-^r^M{8JQ 2 3 3 4 yr MJQ 768 -^^{
4 password is:  C$W@aMHBjc8g 4 5 1 2 ajcg CWMHB 8 $@
```



## PARI/GP


PARI/GP has a very good builtin random generator.

```parigp
passwd(len=8, count=1, seed=0) =
{
  if (len <= 4, print("password too short, minimum len=4"); return(), seed, setrand(seed));

  my (C=["abcdefghijklmnopqrstuvwxyz","ABCDEFGHIJKLMNOPQRSTUVWXYZ","0123456789","!\"#$%&'()*+,-./:;<=>?@[]^_{|}~"], P, j);

  for (i=1, count, K = vector(#C); P = "";
    for (l=1, len, K[j = random(#C)+1]++;
      P = concat(P, Strchr(Vecsmall(C[j])[random(#C[j])+1]))
    );
    if (prod(z=1, #K, K[z]), print(P), i--)    \\ if password contains all char classes print else redo
  );
}

addhelp(passwd, "passwd({len},{count},{seed}): Password generator, optional: len (min=4, default=8), count (default=1), seed (default=0: no seed)");
```


Output: ''passwd()''
```txt
34K76+mB
```


Output of 10 passwords of length 8 with seed enabled: ''passwd(8,10,1)''
```txt

6,89DhJa
xO<XsR8b
R&UagR5h
*21e((D%
pR5Ss&rW
7o7]Fuj+
:N6C6y2L
$L?1s4x)
1R5q'5pw
#4vmVHy5

```


Show help: ''?passwd''
```txt
passwd({len},{count},{seed}): Password generator, optional: len (min=4, default=8), count (default=1), seed (default=0: no seed)
```


## Pascal


```PASCAL

program passwords (input,output);

{$mode objfpc}
{$H+} { We will need ansi strings instead of short strings
        to hold passwords longer than 255 characters.
        We need to assemble a string to check that each
        password contains an upper, lower, numeral and symbol }

{ This is a random password generater in PASCAL
  Copyright Englebert Finklestien on this day which is
  Setting Orange day 48 of The Aftermath YOLD 3184.
  It is distributed under the terms of the GNU GPL (v3)
  As published by the free software foundation.

  Usage :-
  Without any command line arguments this program
  will display 8 8-character long completely random passwords using
  all 96 available character glyphs from the basic ascii set.

  With a single integer numerical argument it will
  produce eight passwords of the chosen length.

  With a second integer numerical argument between 1 and 65536 it
  will produce that number of passwords.

  With two integer arguments the first is taken as the length of password
  and the second as the number of passwords to produce.

  The length of passwords can also be specified using -l or --length options

  The number of passwords can also be specified using -n or --number options

  It is also possible to exclude those glyphs which are simmilar enough to be
  confused (e.g. O and 0) using the -e or --exclude option

  There are also the standard -a --about and -h --help options

  Other options will produce an error message and the program will halt without
  producing any passwords at all.

 }

uses sysutils, getopts;



var c : char;
    optionindex : Longint;
    theopts : array[1..5] of TOption;
    numpass, lenpass, count,j : integer;
    i : longint; { used to get a random number }
    strength : byte; {  check inclusion of different character groups }
    password : string; { To hold a password as we generate it }
    exc : boolean;
    ex : set of char;

procedure about;
begin
     writeln('Engleberts random password generator');
     writeln('Writen in FreePascal on Linux');
     writeln('This is free software distributed under the GNU GPL v3');
     writeln;
end;

procedure help;
begin
    writeln('Useage:-');
    writeln('passwords   produce 8 passwords each 8 characters long.');
    writeln('Use one or more of the following switches to control the output.');
    writeln('passwords --number=xx -nxx --length=xx -lxx --exclude -e --about -a --help -h');
    writeln('passwords ll nn produce nn passwords of length ll');
    writeln('The exclude option excludes easily confused characters such as `0` and `O` from');
    writeln('the generated passwords.');
    writeln;
end;


begin
  numpass := 8;
  lenpass := 8;
  exc := False;
  ex := ['1','!','l','|','i','I','J','0','O','S','$','5',';',':',',','.','\']; { Set of ambiguous characters }
  OptErr := True;
  Randomize;  {initialise the random number generator}
  {set up to handle the command line options}
  with theopts[1] do
   begin
    name:='length';
    has_arg:=1;
    flag:=nil;
    value:=#0;
   end;
  with theopts[2] do
   begin
    name:='number';
    has_arg:=1;
    flag:=nil;
    value:=#0;
   end;
  with theopts[3] do
   begin
    name:='help';
    has_arg:=0;
    flag:=nil;
    value:=#0;
   end;
  with theopts[4] do
   begin
    name:='about';
    has_arg:=0;
    flag:=nil;
    value:=#0;
   end;
  with theopts[5] do
   begin
    name:='exclude';
    has_arg:=0;
    flag:=nil;
    value:=#0;
   end;

{ Get and process long and short versions of command line args. }
  c:=#0;
  repeat
    c:=getlongopts('ahel:n:t:',@theopts[1],optionindex);
    case c of
      #0 : begin
               if (theopts[optionindex].name = 'exclude') then exc := True;
               if (theopts[optionindex].name = 'length') then lenpass := StrtoInt(optarg);
               if (theopts[optionindex].name = 'number') then numpass := StrtoInt(optarg);
	       if (theopts[optionindex].name = 'about') then about;
	       if (theopts[optionindex].name = 'help') then help;
           end;
      'a' : about;
      'h' : help;
      'e' : exc := True;
      'l' : lenpass := StrtoInt(optarg);
      'n' : numpass := StrtoInt(optarg);
      '?',':' : writeln ('Error with opt : ',optopt);
    end; { case }
  until c=endofoptions;
  { deal with any remaining command line parameters (two integers)}
  if optind<=paramcount then
    begin
       count:=1;
       while optind<=paramcount do
         begin
	    if (count=1) then lenpass := StrtoInt(paramstr(optind)) else numpass := StrtoInt(paramstr(optind));
            inc(optind);
	    inc(count);
         end;
    end;
	if not (exc) then ex :=['\']; { if we are not going to exclude characters set the exclusion set to almost empty }
	{ This generates and displays the actual passwords  }
    for count := 1 to numpass do begin
       strength := $00;
       repeat
          password :='';
          for j:= 1 to lenpass do begin
             repeat
                i:=Random(130);
             until (i>32) and (i<127) and (not(chr(i) in ex)) ;
             AppendStr(password,chr(i));
             if (CHR(i) in ['0'..'9']) then strength := strength or $01;
             if (chr(i) in ['a'..'z']) then strength := strength or $02;
             if (chr(i) in ['A'..'Z']) then strength := strength or $04;
             if (chr(i) in ['!'..'/']) then strength := strength or $08;
             if (chr(i) in [':'..'@']) then strength := strength or $08;
             if (chr(i) in ['['..'`']) then strength := strength or $08;
             if (chr(i) in ['{'..'~']) then strength := strength or $08;
         end;
       until strength = $0f;
    writeln(password);
    end;
end.


```

Useage for the output example: passwords --about -h --length=12 --number 12 --exclude

{{out}}

```txt


Engleberts random password generator
Writen in FreePascal on Linux
This is free software distributed under the GNU GPL v3

Useage:-
passwords   produce 8 passwords each 8 characters long.
Use one or more of the following switches to control the output.
passwords --number=xx -nxx --length=xx -lxx --exclude -e --about -a --help -h
passwords ll nn produce nn passwords of length ll
The exclude option excludes easily confused characters such as `0` and `O` from
the generated passwords.

G6ZnfCMoR8)8
]V-Y"L8tFPYM
gveA]%2onN)g
j?4Rz)C(x?RY
mQ]2c)RP~dvU
8<GT{w)]>w^t
Fw"^~73zY8`"
d4KGQmTB4#Nn
PwqYLMEpegv3
mbe^NCTF'a~t
g@vc"B{X+9kg
4tD3w?Vgo_R@


```



## Perl

We use Math::Random, for two reasons: 1) we can use its random_permutation() function; 2) there is a security aspect to this task, so we should avoid using the built-in rand() function as it is presumably less sophisticated.

```perl
use strict;
use English;
use Const::Fast;
use Getopt::Long;
use Math::Random;

my $pwd_length = 8;
my $num_pwds = 6;
my $seed_phrase = 'TOO MANY SECRETS';
my %opts = ('password_length=i' => \$pwd_length,
            'num_passwords=i' => \$num_pwds,
            'seed_phrase=s' => \$seed_phrase,
            'help!' => sub {command_line_help(); exit 0;});
if (! GetOptions(%opts)) {command_line_help(); exit 1; }
$num_pwds >= 1 or die "Number of passwords must be at least 1";
$pwd_length >= 4 or die "Password length must be at least 4";
random_set_seed_from_phrase($seed_phrase);

const my @lcs    => 'a' .. 'z';
const my @ucs    => 'A' .. 'Z';
const my @digits => 0 .. 9;
const my $others => q{!"#$%&'()*+,-./:;<=>?@[]^_{|}~};
# Oh syntax highlighter, please be happy once more "
foreach my $i (1 .. $num_pwds) {
    print gen_password(), "\n";
}
exit 0;

sub gen_password {
    my @generators = (\&random_lc, \&random_uc, \&random_digit, \&random_other);
    my @chars = map {$ARG->()} @generators;  # At least one char of each type.
    for (my $j = 0; $j < $pwd_length - 4; $j++) {
        my $one_of_four = random_uniform_integer(1, 0, 3);
        push @chars, $generators[$one_of_four]->();
    }
    return join(q{}, random_permutation(@chars));
}

sub random_lc {
    my $idx = random_uniform_integer(1, 0, scalar(@lcs)-1);
    return $lcs[$idx];
}

sub random_uc {
    my $idx = random_uniform_integer(1, 0, scalar(@ucs)-1);
    return $ucs[$idx];
}

sub random_digit {
    my $idx = random_uniform_integer(1, 0, scalar(@digits)-1);
    return $digits[$idx];
}

sub random_other {
    my $idx = random_uniform_integer(1, 0, length($others)-1);
    return substr($others, $idx, 1);
}

sub command_line_help {
    print "Usage: $PROGRAM_NAME ",
          "[--password_length=<l> (default: $pwd_length)] ",
          "[--num_passwords=<n> (default: $num_pwds)] ",
          "[--seed_phrase=<s> (default: $seed_phrase)] ",
          "[--help]\n";
    return;
}
```

Transcript (with paths redacted):

```txt
...>password_generator.pl --help
Usage: ...\password_generator.pl [--password_length=<l> (default: 8)] [--num_passwords=<n> (default: 6)] [--seed_phrase=<s> (default: TOO MANY SECRETS)] [--help]

...>password_generator.pl
sc3O~3e0
pE{uh7)%
3J:'L)x8
I{A:h454
tCTH}8(h
b0&S$ZtI
```



## Perl 6

{{works with|Rakudo|2016.05}}


```perl6
my @chars =
  set('a' .. 'z'),
  set('A' .. 'Z'),
  set('0' .. '9'),
  set(<!"#$%&'()*+,-./:;<=>?@[]^_{|}~>.comb);

# bleh. unconfuse syntax highlighter. '"

sub MAIN ( Int :$l = 8, Int :$c = 1, Str :$x = '' ) {
    note 'Password length must be >= 4' and exit if $l < 4;
    note 'Can not generate fewer than 0 passwords' and exit if $c < 0;
    my $chars = [∪] @chars».=&filter;
    note 'Can not exclude an entire required character group' and exit
      if any(@chars».elems) == 0;
    for ^$c {
        my @pswd;
        @pswd.push( @chars[$_].roll ) for ^4;
        @pswd.push( $chars    .roll ) for 4 ..^ $l;
        say [~] @pswd.pick(*);
    }

    sub filter (Set $set) { $set ∖ set($x.comb) }
}

sub USAGE() {
    say qq:to/END/;
    Specify a length:              --l=8 (default 8),
    Specify a count:               --c=1 (default 1),
    Specify characters to exclude: --x=
    (must escape characters significant to the shell)
    E.G.
    {$*PROGRAM-NAME} --l=14 --c=5 --x=0O\\\"\\\'1l\\\|I
    END
}
```

'''Sample output:'''
Using defaults:

```txt
c?6!xU+u
```

With passed parameters: --l=14 --c=5 --x=0O\'\"1l\|I

```txt
6H~jC+5(+&H44x
+Rr}2>htHMa.Y9
t~#&N]sp_zGK2#
TcP73CJ@euFMjj
9%-tYX]z?8-xA5
```



## Phix


```Phix
sequence az = "abcdefghijklmnopqrstuvwxyz",
         AZ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
         O9 = "1234567890",
         OT = "!\"#$%&'()*+,-./:;<=>?@[]^_{|}~",
         S4 = {az,AZ,O9,OT}

function password(integer len, integer n, sequence exclude="Il1O05S2Z")
    sequence res = {}
    for i=1 to n do
        sequence sel = shuffle({1,2,3,4}&sq_rand(repeat(4,len-4)))
        string pw = ""
        for c=1 to length(sel) do
            string S4c = S4[sel[c]]
            while 1 do
                integer ch = S4c[rand(length(S4c))]
                if not find(ch,exclude) then
                    pw &= ch
                    exit
                end if
            end while
        end for
        res = append(res,pw)
    end for
    return res
end function

integer len = prompt_number("Password length(4..99):",{4,99})
integer n = prompt_number("Passwords required(1..99):",{1,99})
sequence passwords = password(len,n)
for i=1 to length(passwords) do
    printf(1,"%s\n",{passwords[i]})
end for
```

{{out}}

```txt

Password length(4..99):12
Passwords required(1..99):6
:EtF77s~%bok
C^Pb&NH8@?u6
RBy%Ep*N9W!t
K%6hxc?C_4_3
vpRBJP)A9@,7
V_3dDf7RhY7N

```



## PicoLisp


```PicoLisp
#!/usr/bin/pil

# Default seed
(seed (in "/dev/urandom" (rd 8)))

# Global defaults
(setq
   *PwCount 1
   *PwLength 12
   *UppChars (mapcar char (range (char "A") (char "Z")))
   *LowChars (mapcar lowc *UppChars)
   *Digits (mapcar format (range 0 9))
   *Others (chop "!\"#$%&'()*+,-./:;<=>?@[]^_{|}~") )

# Command line options
(de -count ()
   (setq *PwCount (format (opt))) )

(de -length ()
   (setq *PwLength (format (opt))) )

(de -seed ()
   (seed (opt)) )

(de -exclude ()
   (for C (chop (opt))
      (del C '*UppChars)
      (del C '*LowChars)
      (del C '*Digits)
      (del C '*Others) ) )

(de -help ()
   (prinl "Generate password(s)")
   (prinl "Options:")
   (prinl "   --help")
   (prinl "   --count <num>")
   (prinl "   --length <num>")
   (prinl "   --seed <chars>")
   (prinl "   --exclude <chars>")
   (bye) )

(load T)

# Return random character from list
(de randChar (Lst)
   (get Lst (rand 1 (length Lst))) )

# Generate password(s)
(do *PwCount
   (prinl
      (by '(NIL (rand)) sort
         (make
            (link
               (randChar *UppChars)  # At least one from each group
               (randChar *LowChars)
               (randChar *Digits)
               (randChar *Others) )
            (do (- *PwLength 4)
               (link
                  (randChar
                     (caar
                        (rot '(*UppChars *Others *Digits *LowChars))) ) ) ) ) ) ) )

(bye)
```

Test:

```txt
$ genpw --help
Generate password(s)
Options:
   --help
   --count <num>
   --length <num>
   --seed <chars>
   --exclude <chars>

$ genpw
[Rg"ia27B?M1

$ genpw  --count 4  --length 20
3O-c23;TbZ~5qAor|!6Q
+H$.bH(aqS19N85a&7aA
?|{(v1EavtB83TTl85W
U%GoEbG%p006l)|+Y1i0
```



## PowerShell


```PowerShell

function New-RandomPassword
{
  <#
    .SYNOPSIS
        Generates one or more passwords.
    .DESCRIPTION
        Generates one or more passwords.
    .PARAMETER Length
        The password length (default = 8).
    .PARAMETER Count
        The number of passwords to generate.
    .PARAMETER Source
        An array of strings containing characters from which the password will be generated.  The default is good for most uses.
    .PARAMETER ExcludeSimilar
        A switch which indicates that visually similar characters should be ignored.
    .EXAMPLE
        New-RandomPassword

        Generates one password of the default length (8 characters).
    .EXAMPLE
        New-RandomPassword -Count 4

        Generates four passwords each of the default length (8 characters).
    .EXAMPLE
        New-RandomPassword -Length 12 -Source abcdefghijklmnopqrstuvwxyz, ABCDEFGHIJKLMNOPQRSTUVWXYZ, 0123456789

        Generates a password with a length of 12 containing at least one char from each string in Source
    .EXAMPLE
        New-RandomPassword -Count 4 -ExcludeSimilar

        Generates four passwords each of the default length (8 characters) while excluding similar characters "Il1O05S2Z".
  #>
    [CmdletBinding()]
    [OutputType([string])]
    Param
    (
        [Parameter(Mandatory=$false)]
        [ValidateRange(1,[Int]::MaxValue)]
        [Alias("l")]
        [int]
        $Length = 8,

        [Parameter(Mandatory=$false)]
        [ValidateRange(1,[Int]::MaxValue)]
        [Alias("n","c")]
        [int]
        $Count = 1,

        [Parameter(Mandatory=$false)]
        [Alias("s")]
        [string[]]
        $Source = @("abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "0123456789", "!\`"#$%&'()*+,-./:;<=>?@[]^_{|}~"),

        [Parameter(Mandatory=$false)]
        [Alias("x")]
        [switch]
        $ExcludeSimilar
    )

    Begin
    {
        [char[][]] $charArrays = $Source
        [char[]]   $allChars   = $charArrays | ForEach-Object {$_}
        [char[]]   $similar    = "Il1O05S2Z".ToCharArray()

        $random = New-Object -TypeName System.Security.Cryptography.RNGCryptoServiceProvider

        function Get-Seed
        {
            $bytes = New-Object -TypeName System.Byte[] -Argument 4
            $random.GetBytes($bytes)
            [BitConverter]::ToUInt32($bytes, 0)
        }

        function Add-PasswordCharacter ([char[]]$From)
        {
            $key = Get-Seed

            while ($password.ContainsKey($key))
            {
                $key = Get-Seed
            }

            $index = (Get-Seed) % $From.Count

            if ($ExcludeSimilar)
            {
                while ($From[$index] -in $similar)
                {
                    $index = (Get-Seed) % $From.Count
                }
            }

            $password.Add($key, $From[$index])
        }
    }
    Process
    {
        for ($i = 1;$i -le $Count; $i++)
        {
            [hashtable] $password = @{}

            foreach ($array in $charArrays)
            {
                if($password.Count -lt $Length)
                {
                    Add-PasswordCharacter -From $array # Append to $password
                }
            }

            for ($j = $password.Count; $j -lt $Length; $j++)
            {
                Add-PasswordCharacter -From $allChars  # Append to $password
            }

            ($password.GetEnumerator() | Select-Object -ExpandProperty Value) -join ""
        }
    }
}

```


```PowerShell

New-RandomPassword -Length 12 -Count 4 -ExcludeSimilar

```

{{Out}}

```txt

}[:=~f$9<Q{'
;EB$JX3/)-qq
u;DD.9t3-3]_
/4#YwjRB4-W-

```

Make it Unix-like:

```PowerShell

Set-Alias -Name nrp -Value New-RandomPassword -Description "Generates one or more passwords"

nrp -l 12 -n 4 -x

```

{{Out}}

```txt

]?Vwrj377tAV
g%\QEx)$3|7"
U*89)9TCKw$j
bvG7UQ3%/4F#

```



## PureBasic


```PureBasic
EnableExplicit

Procedure.b CheckPW(pw.s)
  Define flag.b=#True,
         tmp.b=#False,
         c.c,
         s.s,
         i.i
  For c='a' To 'z'
    tmp=Bool(FindString(pw,Chr(c)))
    If tmp : Break : EndIf
  Next
  flag & tmp
  tmp=#False
  For c='A' To 'Z'
    tmp=Bool(FindString(pw,Chr(c)))
    If tmp : Break : EndIf
  Next
  flag & tmp
  tmp=#False
  For c='0' To '9'
    tmp=Bool(FindString(pw,Chr(c)))
    If tmp : Break : EndIf
  Next
  flag & tmp
  tmp=#False
  For c='!' To '/'
    s+Chr(c)
  Next
  For c=':' To '@'
    s+Chr(c)
  Next
  s+"[]^_{|}~"
  For i=1 To Len(pw)
    tmp=Bool(FindString(s,Mid(pw,i,1)))
    If tmp : Break : EndIf
  Next
  flag & tmp
  ProcedureReturn flag
EndProcedure

Procedure.s InputHdl(prompt.s="")
  Define txt.s,
         s.s,
         r.i,
         hlp.s
  Restore Help_01
  Read.s hlp
  Print(prompt)
  Repeat
    s=Inkey()
    If s<>""
      If FindString("0123456789",s)
        txt+s
        Print(s)
      EndIf
      If s=Chr(27)
        txt="0"
        Break
      EndIf
    ElseIf RawKey()
      r=RawKey()
      If r=112
        PrintN("")
        PrintN(hlp)
        Print(~"\n"+prompt)
      EndIf
    EndIf
    Delay(20)
  Until s=Chr(13)
  PrintN("")
  ProcedureReturn txt
EndProcedure

NewList PasswordChar.c()
Define c.c,
       pwlen.i,
       n_of_pw.i,
       pwstr.s,
       i.i
For c='!' To '~'
  If c<>'\' And c<>'`'
    AddElement(PasswordChar()) : PasswordChar()=c
  EndIf
Next
OpenConsole("Password generator: F1=Help; Esc=End")
Repeat
  pwlen=Abs(Val(InputHdl("Length of the password (n>=4): ")))
  If pwlen=0 : Break : EndIf
  If pwlen<4 : Continue : EndIf
  n_of_pw=Abs(Val(InputHdl("Number of passwords (n>=1): ")))
  If n_of_pw=0 : Break : EndIf
  For i=1 To n_of_pw
    Repeat
      pwstr=Mid(pwstr,2)
      RandomizeList(PasswordChar())
      ResetList(PasswordChar())
      While NextElement(PasswordChar())
        pwstr+Chr(PasswordChar())
        If Len(pwstr)>=pwlen : Break : EndIf
      Wend
    Until CheckPW(pwstr)
    PrintN(RSet(Str(i),Len(Str(n_of_pw))," ")+") "+pwstr)
    pwstr=""
  Next
  PrintN("")
ForEver
End

DataSection
  Help_01:
  Data.s ~"Help: Password generator\n"+
         ~"------------------------\n"+
         ~"Blabla bla blabla bla blablabla.\n"+
         ~"Blabla bla  blablabla.\n"+
         ~"Bla blabla bla blablabla bla.\n"+
         ~"Blabla bla blabla bla.\n"+
         ~"Bla blabla bla blablabla blablabla.\n"+
         ~"Blabla bla blabla bla blablabla.\n"+
         ~"Blabla blabla bla blablabla."
  EndOfHelp:
EndDataSection
```

{{out}}

```txt
Length of the password (n>=4): 10
Number of passwords (n>=1): 12
 1) UteCZm/!9V
 2) R1B*C'gw&<
 3) uPDw.:FhY2
 4) v&0HD6tA);
 5) Ldspz:XcT4
 6) ^a9>Viv"R2
 7) k*x=6VCqMd
 8) y6Jz)p|^=h
 9) UO|sFD^Ry2
10) 1g5*e/:kZf
11) y;mJ{g7QX#
12) _Nh=:'V|a2

Length of the password (n>=4):
```



## Python



```python
import random

lowercase = 'abcdefghijklmnopqrstuvwxyz' # same as string.ascii_lowercase
uppercase = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' # same as string.ascii_uppercase
digits = '0123456789'                    # same as string.digits
punctuation = '!"#$%&\'()*+,-./:;<=>?@[]^_{|}~' # like string.punctuation but without backslash \ nor grave `

allowed = lowercase + uppercase + digits + punctuation

visually_similar = 'Il1O05S2Z'


def new_password(length:int, readable=True) -> str:
    if length < 4:
        print("password length={} is too short,".format(length),
            "minimum length=4")
        return ''
    choice = random.SystemRandom().choice
    while True:
        password_chars = [
            choice(lowercase),
            choice(uppercase),
            choice(digits),
            choice(punctuation)
            ] + random.sample(allowed, length-4)
        if (not readable or
                all(c not in visually_similar for c in password_chars)):
            random.SystemRandom().shuffle(password_chars)
            return ''.join(password_chars)


def password_generator(length, qty=1, readable=True):
    for i in range(qty):
        print(new_password(length, readable))

```

{{output}}

```txt
>>> password_generator(14, 4)
i&H.j9F$)'V}!o
w&.U6vaf/HD;sA
i8Hfyq@&M?g:L6
j#%JxdbG9@fvX*
>>> password_generator(8, 4, readable=False)
A#f5c;(E
z@C9iIa1
R{s320IH
$4FLjCL0
```



## Racket


```racket

#lang racket

(require racket/cmdline)

(define valid-uppercase '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
                          #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
                          #\U #\V #\W #\X #\Y #\Z))
(define valid-lowercase '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
                          #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t
                          #\u #\v #\w #\x #\y #\z))
(define valid-numbers '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(define valid-symbols '(#\! #\\ #\" #\# #\$ #\% #\& #\' #\( #\)
                        #\* #\+ #\, #\- #\. #\/ #\: #\; #\< #\=
                        #\> #\? #\@ #\[ #\] #\^ #\_ #\{ #\| #\}
                        #\~))

(define visual-invalid '(#\0 #\O #\1 #\I #\l #\| #\5 #\S #\2 #\Z))

(define (is-readable?  c)
  (empty? (filter (lambda (x) (eq? x c)) visual-invalid)))

(define (random-selection lst)
  (list-ref lst (random (length lst))))

(define (generate len readable)
  (let ([upper (if readable (filter is-readable? valid-uppercase) valid-uppercase)]
        [lower (if readable (filter is-readable? valid-lowercase) valid-lowercase)]
        [numbers (if readable (filter is-readable? valid-numbers) valid-numbers)]
        [symbols (if readable (filter is-readable? valid-symbols) valid-symbols)])
    (let loop ([lst (map random-selection (list upper lower numbers symbols))])
      (cond
        [(<= len (length lst)) (shuffle lst)]
        [else (match (random 4)
                [0 (loop (cons (random-selection upper) lst))]
                [1 (loop (cons (random-selection lower) lst))]
                [2 (loop (cons (random-selection numbers) lst))]
                [3 (loop (cons (random-selection symbols) lst))])]))))

(define (run len cnt seed readable)
  (random-seed seed)
  (let loop ([x cnt])
    (unless (zero? x)
      (display (list->string (generate len readable)))
      (newline)
      (loop (- x 1)))))

(define len (make-parameter 10))
(define cnt (make-parameter 1))
(define seed (make-parameter (random 1 1000000)))
(define readable? (make-parameter #f))

(command-line #:program "passwdgen"
              #:once-each
              [("-l" "--length") integer "password length"  (len (string->number integer))]
              [("-c" "--count") integer "number of password" (cnt (string->number integer))]
              [("-s" "--seed") integer "random generator seed" (seed (string->number integer))]
              [("-r" "--readable") "safe characters" (readable? #t)])


(run (len) (cnt) (seed) (readable?))

```

'''Sample output:'''

```txt

$ racket ./passwdgen.rkt
4K{3EU30nP
$ racket ./passwdgen.rkt -l 15 -c 3 -s 1 -r
3_7tY~krwNz8MBP
966xU!&KT6mpW,M
Wh,(o*c_/Bf99K4

```



## REXX

This REXX code was written as generically and idiomatically as possible so that the   ''special characters''   and

the lowercase and uppercase alphabets may be extended with letters from other alphabets.

This REXX version supports specifying a seed for the   '''random'''   BIF,   as well as specification of characters

 to   ''not''   to be used for the password.

This version isn't restricted to '''ASCII''' characters,   it also supports '''EBCDIC''' as well.

It has a REXX statement   (as a comment)   to support the writing of the generated passwords to a file.

The documentation (help) could be expanded greatly, but only an abridged version is included here to keep

this REXX example relatively small for this Rosetta Code task.

Also, much more error checking should be added;   not the least of it would be:
:::*   if too many arguments specified   (on the command line)
:::*   checking if the (random) seed is valid
:::*   better (informative) error messages   (more verbose)
:::*   don't let the user generate a gazillion passwords
:::*   checking if the hexadecimal literal   ('''yyy''')   is valid
:::*   checking (for instance) if all digits were excluded via the   <b>'''xxx'''</b>   and/or   '''yyy'''   option

```rexx
/*REXX program generates a  random password  according to the Rosetta Code task's rules.*/
@L='abcdefghijklmnopqrstuvwxyz'; @U=@L; upper @U /*define lower-, uppercase Latin chars.*/
@#= 0123456789                                   /*   "   " string of base ten numerals.*/
@@= '!"#$%&()+,-./:;<=>?@[]^{|}~' || "'"         /*define a bunch of special characters.*/
parse arg L N seed xxx yyy .                     /*obtain optional arguments from the CL*/
if L=='?'               then signal help         /*does user want documentation shown?  */
if L=='' | L==","       then L=8                 /*Not specified?  Then use the default.*/
if N=='' | N==","       then N=1                 /* "      "         "   "   "     "    */
if xxx\==''             then call weed  xxx      /*Any chars to be ignored?  Scrub lists*/
if yyy\==''             then call weed  x2c(yyy) /*Hex   "    "  "     "       "     "  */
if  datatype(seed,'W')  then call random ,,seed  /*the seed for repeatable RANDOM BIF #s*/
if \datatype(L,   'W')  then call serr  "password length, it isn't an integer: "       L
if L<4                  then call serr  "password length, it's too small  (< 4): "     L
if L>80                 then call serr  "password length, it's too large  (> 80): "    L
if \datatype(N,   'W')  then call serr  "number of passwords, it isn't an integer: "   N

    do g=1  to N;       $=                       /*generate N passwords (default is one)*/
        do k=1  for L;       z=k;   if z>4  then z=random(1,4) /*1st four parts │ random*/
        if z==1  then $=$ || substr(@L,random(1,length(@L)),1) /*append lowercase letter*/
        if z==2  then $=$ || substr(@U,random(1,length(@U)),1) /*   "   uppercase    "  */
        if z==3  then $=$ || substr(@#,random(1,length(@#)),1) /*   "    numeral        */
        if z==4  then $=$ || substr(@@,random(1,length(@@)),1) /*   "  special character*/
        end   /*k*/
                                                 /* [↓]  scrambles PW, hides gen order. */
        do a=1  for L;          b=random(1, L)   /*swap every character with another.   */
        parse var $ =(a) x +1 =(b)  y  +1        /*≡  x=substr($,a,1);  y=substr($,b,1) */
        $=overlay(x,$,b);       $=overlay(y,$,a) /*(both statements) swap two characters*/
        end  /*L+L*/                             /* [↑]  more swaps obfuscates gen order*/

    say right(g, length(N))  'password is: '  $  /*display the  Nth  password to console*/
    /*      call lineout 'GENPW.PW', $  */       /*and also write the password to a file*/     /*or not.*/
    end      /*g*/                               /* [↑]  {a comment}   fileID= GENPW.PW */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
weed:  parse arg ig;   @L=dont(@L);   @U=dont(@U);   @#=dont(@#);   @@=dont(@@);    return
dont:  return space( translate(arg(1), , ig), 0)              /*remove chars from a list*/
serr:  say;   say '***error*** invalid'  arg(1);  exit 13     /*display an error message*/
help:  signal .; .: do j=sigL+1 to sourceline(); say strip(left(sourceline(j),79)); end /*
╔═════════════════════════════════════════════════════════════════════════════╗
║  GENPW  ?                    ◄─── shows this documentation.                 ║
║  GENPW                       ◄─── generates 1 password  (with length  8).   ║
║  GENPW len                   ◄─── generates (all) passwords with this length║
║  GENPW  ,   n                ◄─── generates     N      number of passwords. ║
║  GENPW  ,   ,  seed          ◄─── generates passwords  using a random seed. ║
║  GENPW  ,   ,    ,  xxx      ◄─── generates passwords that don't contain xxx║
║  GENPW  ,   ,    ,   ,  yyy  ◄─── generates passwords that don't contain yyy║
║                                                                             ║
╟──────────── where   [if a  comma (,)  is specified,  the default is used]:  ║
║ len     is the length of the passwords to be generated.    The default is 8.║
║         The minimum is  4,   the maximum is  80.                            ║
║ n       is the number of passwords to be generated.        The default is 1.║
║ seed    is an integer seed used for the RANDOM BIF.     (Default is random.)║
║ xxx     are characters to  NOT  be used for generating passwords.           ║
║         The default is to use  all  the  (normal)  available characters.    ║
║ yyy     (same as XXX,  except the chars are expressed as hexadecimal pairs).║
╚═════════════════════════════════════════════════════════════════════════════╝         */
```

'''output'''   when using the inputs of:   <tt> 10   20 </tt>

```txt

 1 password is:  EQG@~?~a[6
 2 password is:  2J:"Cm|ol<
 3 password is:  Io32~fa[?L
 4 password is:  r)@;2;PsZE
 5 password is:  }#H"7Rk?4{
 6 password is:  cHB8Q!%@PI
 7 password is:  a7m75132.S
 8 password is:  Qcg4>FeNb7
 9 password is:  52vHl'+HIP
10 password is:  139Vk]aIL-
11 password is:  f1g;}b/W1W
12 password is:  lC74rIv5s<
13 password is:  O9$8g1}er4
14 password is:  c|E?Uh4c8~
15 password is:  :39OK2E8u#
16 password is:  vw=+uI-X+3
17 password is:  8YhJ)BS>>~
18 password is:  Vd]ZCUw%<0
19 password is:  6([b4Qk;O7
20 password is:  5ST.}:1t@O

```



## Ring


```ring

# Project : Password generator

chars = list(4)
strp = list(2)
password = ""
chars[1] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
chars[2] = "abcdefghijklmnopqrstuvwxyz"
chars[3] = "0123456789"
chars[4] = "!\#$%&'()*+,-./:;<=>?@[]^_{|}~"

init()
plen = number(strp[1])

for n = 1 to strp[2]
     passwords(chars)
     see "password = " + password + nl
next

func passwords(chars)
       index = 0
       password = ""
       while index < plen
                 index = index + 1
                 charsind1 = index % len(chars) + 1
                 charsind2 = random(len(chars[charsind1])-1) + 1
                 password = password + chars[charsind1][charsind2]
       end

func init()
       fp = fopen("C:\Ring\calmosoft\pwgen.ring","r")
       r = ""
       str = ""
       nr = 0
       while isstring(r)
               r = fgetc(fp)
               if r != char(10) and not feof(fp)
                  str = str + r
                  nr = nr + 1
                  strp[nr] = str
               else
                  str = ""
               ok
       end
       fclose(fp)

```

Output:

```txt

password = w6+Am5]N
password = e9(Ca9,I
password = u8/Ah8%H
password = c4\Nc2_J

```



## Ruby


```Ruby
ARRS = [("a".."z").to_a,
        ("A".."Z").to_a,
        ("0".."9").to_a,
        %q(!"#$%&'()*+,-./:;<=>?@[]^_{|}~).chars] # " quote to reset clumsy code colorizer
ALL  = ARRS.flatten

def generate_pwd(size, num)
  raise ArgumentError, "Desired size too small" unless size >= ARRS.size
  num.times.map do
    arr = ARRS.map(&:sample)
    (size - ARRS.size).times{ arr << ALL.sample}
    arr.shuffle.join
  end
end

puts generate_pwd(8,3)

```



## Run BASIC


```Runbasic
a$(1) = "0123456789"
a$(2) = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
a$(3) = "abcdefghijklmnopqrstuvwxyz"
a$(4) = "!""#$%&'()*+,-./:;<=>?@[]^_{|}~"
a$(0) = a$(1) + a$(2) + a$(3) + a$(4)

[main]
print "----------- Password Generator -----------"
input "Number of Characters:";howBig
if howBig < 1 then goto [exit]

input "How many to generate:";howMany
if howMany < 1 then goto [main]

' -----------------------------
' Generate Password
' -----------------------------
[gen]
cls
print "Generate ";howMany;" passwords with ";howBig;" characters"
i = 0
while i < howMany
	pw$	= ""
	ok$	= "...."
	pw$	= ""
	for j = 1 to howBig
	 	w$ = mid$(a$(0),int(rnd(0) * len(a$(0))) + 1,1)
		for k = 1 to 4
			if instr(a$(k),w$) then ok$ = left$(ok$,k-1) + "*" + mid$(ok$,k+1)
	 	next k
	 	pw$ = pw$ + w$
	next j
	if ok$ = "****" then	' Do we pass with the requirements
		i	= i + 1
		print "#";i;" ";pw$
	end if
WEND
goto [main]
[exit]	' get outta here
end
```
Output:

```txt
Generate 10 passwords with 7 characters
#1 69+;Jj8
#2 80+T_e9
#3 1NEy[7e
#4 vj4~EvD
#5 3E(852y
#6 Cxjo]5R
#7 Hm5'tF+
#8 %i^<N7>
#9 9P8Qx_P
#10 f0Qho:5
```



## Scala

Using SBT to run rather than a shell script or executable jar:

```scala
object makepwd extends App {

  def newPassword( salt:String = "", length:Int = 13, strong:Boolean = true ) = {

    val saltHash = salt.hashCode & ~(1 << 31)

    import java.util.Calendar._

    val cal = java.util.Calendar.getInstance()
    val rand = new scala.util.Random((cal.getTimeInMillis+saltHash).toLong)
    val lower = ('a' to 'z').mkString
    val upper = ('A' to 'Z').mkString
    val nums = ('0' to '9').mkString
    val strongs = "!\"#$%&'()*+,-./:;<=>?@[]^_{|}~"
    val unwanted = if( strong ) "" else "0Ol"

    val pool = (lower + upper + nums + (if( strong ) strongs else "")).
               filterNot( c => unwanted.contains(c) )

    val pwdStream = Stream.continually( (for( n <- 1 to length; c = pool(rand.nextInt(pool.length) ) ) yield c).mkString )

    // Drop passwords that don't have at least one of each required character
    pwdStream.filter( pwd =>
      pwd.exists(_.isUpper) &&
      pwd.exists(_.isLower) &&
      pwd.exists(_.isDigit) &&
      (if(strong) pwd.exists(! _.isLetterOrDigit) else true)
    ).head
  }

  val pwdLength = """^(\d{1,4})$""".r
  val howMany = """^\-n(\d{0,3})$""".r
  val help = """^\-\-(help)$""".r
  val pwdSalt = """^\-s(.*)""".r
  val strongOption = """(?i)(strong)""".r


  var (salt,length,strong,helpWanted,count,unknown) = ("",13,false,false,1,false)

  args.foreach{
    case pwdLength(l) =>    length = math.min(math.max(l.toInt,6),4000)
    case strongOption(s) => strong = true
    case pwdSalt(s) =>      salt = s
    case howMany(c) =>      count = math.min(c.toInt,100)
    case help(h) =>         helpWanted = true
    case _ =>               unknown = true
  }

  if( count > 1 ) println

  if( helpWanted || unknown ) {
    println( """
  makepwd <length> "strong" -s<salt> -n<how-many> --help

    <length>     = how long should the password be
    "strong"     = strong password, omit if special characters not wanted
    -s<salt>     = "-s" followed by any non-blank characters
                     (increases password randomness)
    -n<how-many> = "-n" followed by the number of passwords wanted
    --help       = displays this

  For example: makepwd 13 strong -n20 -sABCDEFG
""".stripMargin )
  }
  else for( i <- 1 to count ) println( newPassword( i + salt, length, strong ) )

  if( count > 1 ) println
}
```

{{output}}
> sbt "run --help"

```txt
...

  makepwd <length> "strong" -s<salt> -n<how-many> --help

    <length>     = how long should the password be
    "strong"     = strong password, omit if special characters not wanted
    -s<salt>     = "-s" followed by any non-blank characters
                     (increases password randomness)
    -n<how-many> = "-n" followed by the number of passwords wanted
    --help       = displays this

  For example: makepwd 13 strong -n20 -sABCDEFG

```

> sbt "run 13 strong -sMySecret -n3"

```txt
...

}mR46_*cOq&v0
Ab~A!ddH8%JPd
z{A.m+$cqy#9I

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func string: generate (in integer: length) is func
  result
    var string: password is "";
  local
    const set of char: allowed is {'!' .. '~'} - {'\\', '`'};
    const set of char: special is allowed - {'A' .. 'Z'} | {'a' .. 'z'} | {'0' .. '9'};
    var integer: index is 0;
    var char: ch is ' ';
    var boolean: ucPresent is FALSE;
    var boolean: lcPresent is FALSE;
    var boolean: digitPresent is FALSE;
    var boolean: specialPresent is FALSE;
  begin
    repeat
      password := "";
      ucPresent := FALSE;
      lcPresent := FALSE;
      digitPresent := FALSE;
      specialPresent := FALSE;
      for index range 1 to length do
        ch := rand(allowed);
        ucPresent := ucPresent or ch in {'A' .. 'Z'};
        lcPresent := lcPresent or ch in {'a' .. 'z'};
        digitPresent := digitPresent or ch in {'0' .. '9'};
        specialPresent := specialPresent or ch in special;
        password &:= ch;
      end for;
    until ucPresent and lcPresent and digitPresent and specialPresent;
  end func;

const proc: main is func
  local
    var integer: length is 0;
    var integer: count is 0;
  begin
    if length(argv(PROGRAM)) <> 2 or not isDigitString(argv(PROGRAM)[1]) or
       not isDigitString(argv(PROGRAM)[2]) then
      writeln("Usage: pwgen length count");
      writeln("       pwgen -?");
      writeln("length: The length of the password (min 4)");
      writeln("count:  How many passwords should be generated");
      writeln("-?  Write this text");
    else
      length := integer(argv(PROGRAM)[1]);
      count := integer(argv(PROGRAM)[2]);
      if length < 4 then
        writeln("Passwords must be at least 4 characters long.");
      else
        for count do
          writeln(generate(length));
        end for;
      end if;
    end if;
  end func;
```


{{out}}

```txt
$ pwgen -?
Usage: pwgen length count
       pwgen -?
length: The length of the password (min 4)
count:  How many passwords should be generated
-?  Write this text
$ pwgen 8 3
S1uqOqU~
@m'-.D,9
>tl1fvEU
$
```



## Swift

Swift uses arc4random() to generate fast and high quality random numbers. However the usage of a user defined seed is not possible within arc4random(). To fulfill the requirements this code uses the C functions srand() and rand() that are integrated into the Swift file via an Bridging-Header.


'''C file to generate random numbers'''

```cpp
#include <iostream>
#include <time.h>

void initRandom(const unsigned int seed){
    if(seed==0){
        srand((unsigned) time(NULL));
    }
    else{
        srand(seed);
    }
}

int getRand(const int upperBound){
    return rand() % upperBound;
}
```


'''Bridging-Header to include C file into Swift'''

```C
int getRand(const int upperBound);
void initRandom(const unsigned int seed);
```


'''Swift file'''

```swift
import Foundation
import GameplayKit  // for use of inbuilt Fisher-Yates-Shuffle

/* Prints the usage of this code */
func printHelp() -> Void {
    print("Usage: PasswordGenerator [-l:length] [-c:count] [-s:seed] [-x:exclude] [-h:help]")
    print("\t-l: length of the passwords (at leas 4 characters)")
    print("\t-c: number of passwords to generate")
    print("\t-s: seed of the random number generator")
    print("\t-x: exclude of visually similar characters \"Il1O05S2Z\"")
    print("\t-h: print this help")
    exit(0)
}

/* Set characters for generating passwords */
let _lower:String = "abcdefghijklmnopqrstuvwxyz"
let _lowerWithoutSimilar:String = "abcdefghijkmnopqrstuvwxyz"
let _upper:String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let _upperWithoutSimilar = "ABCDEFGHJKLMNPQRTUVWXY"
let _number:String = "0123456789"
let _numerWithoutSimilar:String = "1346789"
let _other:String = "!\"#$%&'()*+,-./:;<=>?@[]^_{|}~"

/* Generate character arrays out of strings */
let upper = Array(_upper.characters)
let upperWithoutSimilar = Array(_upperWithoutSimilar.characters)
let lower = Array(_lower.characters)
let lowerWithoutSimilar = Array(_lowerWithoutSimilar.characters)
let other = Array(_other.characters)
let number = Array(_number.characters)
let numberWithoutSimilar = Array(_numerWithoutSimilar.characters)

var length:Int=0, count:Int=0, seed:Int=0, xclude:Bool=false

/* Parse CLI arguments */
for i in 1..<CommandLine.arguments.count{
    var arg = CommandLine.arguments[i]
    var argument = arg.components(separatedBy: ":")
    switch(argument[0]){
    case "-l":
        length=Int(argument[1])!
        if length < 4 {
            print("A password must contain of at least 4 characters.")
            exit(-1)
        }
        break
    case "-c":
        count=Int(argument[1])!
        break
    case "-s":
        seed=Int(argument[1])!
        break
    case "-x":
        xclude=true
        break
    case "-h":
        printHelp()
    default:
        print("Could not parse CLI arguments. Use -h for help.")
        exit(0)
    }
}

/* Generate password of given length */
func generatePassword(length len:Int, exclude xcl:Bool) -> String{

    var ret:String = "", loopCount:Int = 0

    while(loopCount < len){
        if ret.characters.count < len {
            if xcl {
                ret += String(upperWithoutSimilar[Int(getRand(Int32(upperWithoutSimilar.count-1)))])
            }
            else {
                ret += String(upper[Int(getRand(Int32(upper.count)))])
            }
        }

        if ret.characters.count < len {
            if xcl {
                ret += String(lowerWithoutSimilar[Int(getRand(Int32(lowerWithoutSimilar.count-1)))])
            }
            else {
                ret += String(lower[Int(getRand(Int32(lower.count-1)))])
            }
        }

        if ret.characters.count < len {
            if xcl {
                ret += String(numberWithoutSimilar[Int(getRand(Int32(numberWithoutSimilar.count-1)))])
            }
            else {
                ret += String(number[Int(getRand(Int32(number.count-1)))])
            }
        }

        if ret.characters.count < len {
            ret += String(other[Int(getRand(Int32(other.count-1)))])
        }
        loopCount += 4
    }

    // Shuffle the array with an internal shuffle function
    let shuffled = GKRandomSource.sharedRandom().arrayByShufflingObjects(in: Array(ret.characters))
    ret = ""
    for element in shuffled {
        ret += String(describing: element)
    }
    return ret
}

if xclude {
    print("Generating \(count) passwords with length \(length) excluding visually similar characters...")
}
else {
    print("Generating \(count) passwords with length \(length) not excluding visually similar characters...")
}

initRandom(UInt32(0))    // initialize with C func srand()

// generate the passwords
for i in 1...count {
    print("\(i).\t\(generatePassword(length:length,exclude:xclude))")
}
```

{{out}}

```txt
$ PasswordGenerator -h
Usage: PasswordGenerator [-l:length] [-c:count] [-s:seed] [-x:exclude] [-h:help]
	-l: length of the passwords (at leas 4 characters)
	-c: number of passwords to generate
	-s: seed of the random number generator
	-x: exclude of visually similar characters "Il1O05S2Z"
	-h: print this help

$ PasswordGenerator -l:10 -c:5 -x
Generating 5 passwords with length 10 excluding visually similar characters...
1.	Fs7]r)N4Ap
2.	(o6GgXE=6a
3.	A!A7k|N3mi
4.	6]XDKo!nx8
5.	Gi6b>6Mv&C
```



## VBA


```vb

Option Explicit
Sub Main()
Dim s() As String, i As Long
        Debug.Print "list of 10 passwords : "
'do a list of 10 passwords with password's lenght = 21 and visually similar = False
    s = Gp(10, 21, False)
        'return
        Debug.Print "1- with password's lenght = 21 and visually similar = False :"
        For i = 1 To UBound(s): Debug.Print s(i): Next
'do a list of 10 passwords with pattern = "A/9-a/1-9/4-!/5" and visually similar = True
    s = Gp(10, "A/9-a/1-9/4-!/5", True)
        'return
        Debug.Print "2- with pattern = ""A/9-a/1-9/4-!/5"" and visually similar = True :"
        For i = 1 To UBound(s): Debug.Print s(i): Next
End Sub
Sub HelpMe()
Dim s As String
    s = "Help :" & vbCrLf
    s = s & "----------------------------------" & vbCrLf
    s = s & "The function (named : Gp) needs 3 required parameters :" & vbCrLf & vbCrLf
    s = s & "1- Nb_Passwords (Long) : the number of passwords to generate." & vbCrLf & vbCrLf
    s = s & "2- NbChar_Or_Pattern (Variant) : either a number or a pattern" & vbCrLf
    s = s & "      If number : NbChar_Or_Pattern specify the password length. All the digits are random ASCII characters" & vbCrLf
    s = s & "      If pattern : NbChar_Or_Pattern specify the password length and the layout of passwords." & vbCrLf
    s = s & "             The pattern is built like this :" & vbCrLf
    s = s & "                 ""A"" means Upper case, ""a"" means lower case, 9 means numerics and ! means others characters." & vbCrLf
    s = s & "                 ""-"" is the separator between these values." & vbCrLf
    s = s & "                 the number of characters is specified after the sign (required): ""/""" & vbCrLf
    s = s & "                 example of pattern available : ""A/3-a/2-9/1-!/1""" & vbCrLf & vbCrLf
    s = s & "3- Excl_Similar_Chars (Boolean) : True if you want the option of excluding visually similar characters."
    Debug.Print s
End Sub
Private Function Gp(Nb_Passwords As Long, NbChar_Or_Pattern As Variant, Excl_Similar_Chars As Boolean) As String()
'generate a list of passwords
Dim l As Long, s() As String
    ReDim s(1 To Nb_Passwords)
    If IsNumeric(NbChar_Or_Pattern) Then
        For l = 1 To Nb_Passwords
            s(l) = p(CLng(NbChar_Or_Pattern), Excl_Similar_Chars)
        Next l
    Else
        For l = 1 To Nb_Passwords
            s(l) = ttt(CStr(NbChar_Or_Pattern), Excl_Similar_Chars)
        Next l
    End If
    Gp = s
End Function
Public Function p(n As Long, e As Boolean) As String
'create 1 password without pattern (just with the password's lenght)
Dim t As String, i As Long, a As Boolean, b As Boolean, c As Boolean, d As Boolean
    Randomize Timer
    If n < 4 Then
        p = "Error. Numbers of characters is too small. Min : 4"
    ElseIf n >= 4 And n < 7 Then
        T = u(122, 97) & u(90, 65) & u(57, 48) & v
        For j = 5 To n
            i = Int((4 * Rnd) + 1)
            Select Case i
                Case 1: T = T & u(122, 97)
                Case 2: T = T & u(90, 65)
                Case 3: T = T & u(57, 48)
                Case 4: T = T & v
            End Select
        Next j
        'Debug.Print T
        p = y(T)
    Else
        Do
            i = Int((4 * Rnd) + 1)
            Select Case i
                Case 1: t = t & u(122, 97): a = True
                Case 2: t = t & u(90, 65): b = True
                Case 3: t = t & u(57, 48): c = True
                Case 4: t = t & v: d = True
            End Select
            If Len(t) >= 2 And e Then
                If x(t) Then t = Left(t, Len(t) - 1)
            End If
            If Len(t) = n Then
                If a And b And c And d Then
                    Exit Do
                Else
                    w t, a, b, c, d
                    p = p(n, e)
                End If
            ElseIf Len(t) > n Then
                w t, a, b, c, d
                p = p(n, e)
            End If
        Loop
        p = t
    End If
End Function
Public Function ttt(s As String, e As Boolean) As String
'create 1 password with pattern
Dim a, i As Long, j As Long, st As String, Nb As Long
    a = Split(s, "-")
    For i = 0 To UBound(a)
        Select Case Left(a(i), 1)
            Case "A"
                Nb = CLng(Split(a(i), "/")(1)): j = 0
                Do
                    j = j + 1
                    st = st & u(90, 65)
                    If Len(st) >= 2 And e Then
                        If x(st) Then st = Left(st, Len(st) - 1): j = j - 1
                    End If
                Loop While j < Nb
            Case "a"
                Nb = CLng(Split(a(i), "/")(1)): j = 0
                Do
                    j = j + 1
                    st = st & u(122, 97)
                    If Len(st) >= 2 And e Then
                        If x(st) Then st = Left(st, Len(st) - 1): j = j - 1
                    End If
                Loop While j < Nb
            Case "9"
                Nb = CLng(Split(a(i), "/")(1)): j = 0
                Do
                    j = j + 1
                    st = st & u(57, 48)
                    If Len(st) >= 2 And e Then
                        If x(st) Then st = Left(st, Len(st) - 1): j = j - 1
                    End If
                Loop While j < Nb
            Case "!"
                Nb = CLng(Split(a(i), "/")(1)): j = 0
                Do
                    j = j + 1
                    st = st & v
                    If Len(st) >= 2 And e Then
                        If x(st) Then st = Left(st, Len(st) - 1): j = j - 1
                    End If
                Loop While j < Nb
        End Select
    Next i
    ttt = y(st)
End Function
Private Function u(m As Long, l As Long) As String
'random 1 character in lower/upper case or numeric
    Randomize Timer
    u = Chr(Int(((m - l + 1) * Rnd) + l))
End Function
Private Function v() As String
'random 1 character "special"
    Randomize Timer
    v = Mid("!""#$%&'()*+,-./:;<=>?@[]^_{|}~", Int((30 * Rnd) + 1), 1)
End Function
Private Sub w(t As String, a As Boolean, b As Boolean, c As Boolean, d As Boolean)
    t = vbNullString: a = False: b = False: c = False: d = False
End Sub
Private Function x(s As String) As Boolean
'option of excluding visually similar characters
Dim t, i As Long
Const d As String = "Il I1 l1 lI 1l 1I 0O O0 5S S5 2Z 2? Z? Z2 ?2 ?Z DO OD"
    t = Split(d, " ")
    For i = 0 To UBound(t)
        If Right(s, 2) = t(i) Then
            x = True: Exit Function
        End If
    Next
End Function
Private Function y(s As String) As String
'shuffle the password's letters only if pattern
Dim i&, t, r As String, d() As Long
    t = Split(StrConv(s, vbUnicode), Chr(0))
    d = z(UBound(t))
    For i = 0 To UBound(t)
        r = r & t(d(i))
    Next i
    y = Left(r, Len(r) - 1)
End Function
Private Function z(l As Long) As Long()
'http://rosettacode.org/wiki/Best_shuffle#VBA
Dim i As Long, ou As Long, temp() As Long
Dim c As New Collection
    ReDim temp(l)
    If l = 1 Then
        temp(0) = 0
    ElseIf l = 2 Then
        temp(0) = 1: temp(1) = 0
    Else
        Randomize
        Do
            ou = Int(Rnd * l)
            On Error Resume Next
            c.Add CStr(ou), CStr(ou)
            If Err <> 0 Then
                On Error GoTo 0
            Else
                temp(ou) = i
                i = i + 1
            End If
        Loop While c.Count <> l
    End If
    z = temp
End Function
```

{{out}}
Function Gp :

```txt
list of 10 passwords :
1- with password's lenght = 21 and visually similar = False :
;OK6^D26"S1^ih77<pR~v
EH9csF8+hC"pw70dL5},A
F1qIC#xXZ!%mQ2kb5&>q1
c|e0*emQ2-bN1}QL-fFB3
/U9)@J54zY44(gI,/vZ5t
3W:!y44(n36nc2<"KW5)Z
7[oOK6^D26"S1^ih77<pR
zveH9csF8+hC"pw70dL5}
,A21qIC#xXZ!%mQ2kb5&>
=11|e0*emQ2-bN1}QL-fF
2- with pattern = "A/9-a/1-9/4-!/5" and visually similar = True :
)C4N#<>r@NDUX094LKF
EJL~!:JKS39U3<AW5"s
H$0t?1'V7+DHV#WFQQ8
71!t0T)BSGZ|V=>KI9V
I50GN^"+T8Vs>E"NKI1
|JAOrWY"WW97_7[Y[2K
EBoB/3)C#Z$F6U3V1,O
ANOBPZ*X2I-3@H]58e:
7D1$7V+BHQH'FV0#w{Q
&%72&LFL6iS~C(HAPQ8
```

Sub HelpMe :

```txt
Help :
----------------------------------
The function (named : Gp) needs 3 required parameters :

1- Nb_Passwords (Long) : the number of passwords to generate.

2- NbChar_Or_Pattern (Variant) : either a number or a pattern
      If number : NbChar_Or_Pattern specify the password length. All the digits are random ASCII characters
      If pattern : NbChar_Or_Pattern specify the password length and the layout of passwords.
             The pattern is built like this :
                 "A" means Upper case, "a" means lower case, 9 means numerics and ! means others characters.
                 "-" is the separator between these values.
                 the number of characters is specified after the sign (required): "/"
                 example of pattern available : "A/3-a/2-9/1-!/1"

3- Excl_Similar_Chars (Boolean) : True if you want the option of excluding visually similar characters.

```



## zkl

Put the following code into a file (such as pwdg.zkl):

```zkl
var pwdLen=10, pwds=1, xclude="";

argh:=Utils.Argh(
L("+xclude","","Don't use these characters",fcn(arg){ xclude=arg }),
L("+len","","Number of characters in password", fcn(arg){ pwdLen=arg.toInt() } ),
L("+num","","Number of passwords to generate", fcn(arg){ pwds=arg.toInt() } ),
);
try{ argh.parse(vm.arglist) }catch{ System.exit(1) }

isd:='wrap(w){ w.pump(String) - xclude }; // iterator to String
g1,g2,g3 := isd(["a".."z"]), isd(["A".."Z"]), isd(["0".."9"]);
g4:="!\"#$%&'()*+,-./:;<=>?@[]^_{|}~" - xclude;
all:=String(g1,g2,g3,g4);
fcn rnd(s){ s[(0).random(s.len())] }  // pick a random character from s
    // generate random characters of filler needed to complete password
fill:=(pwdLen-4).pump.fp(String,rnd.fp(all)); // a deferred/pending calculation

do(numPwds){
   // Data is byte bucket (and editor). I can shuffle a Data but not a String.
   pwd:=T(g1,g2,g3,g4).pump(Data,rnd); // 1 from each of these into a Data
   pwd.extend(fill()).shuffle().text.println();
}
```

This is a command line program so output can be redirected.
{{out}}

```txt

$ zkl pwdg.zkl -?
Unknown option: ?
Options:
  --len <arg>: Number of characters in password
  --num <arg>: Number of passwords to generate
  --xclude <arg>: Don't use these characters

$ zkl pwdg.zkl --len 10 --xclude "Il1 O0 5S 2Z" --num 5
xyo9p$T]L8
D6}KeDYVq6
4<?BCWpRLj
4~x_46-Tqi
*:XE3G@myQ

```

