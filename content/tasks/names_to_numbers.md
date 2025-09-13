+++
title = "Names to numbers"
description = ""
date = 2018-06-25T10:09:51Z
aliases = []
[extra]
id = 13501
[taxonomies]
categories = ["task"]
tags = []
+++

Translate the spelled-out English name of a number to a number. You can use a preexisting implementation or roll your own, but you should support inputs up to at least one million (or the maximum value of your language's default bounded integer type, if that's less).

Support for inputs other than positive integers (like zero, negative integers, fractions and floating-point numbers) is optional.

## See also

* [[Number names]] — the reverse operation.


## D

This uses the D module from the Number names task.
```d
import std.stdio, std.array, std.string, std.algorithm, std.bigint,
       std.range, number_names;

BigInt bigIntFromWords(in string num)
in {
    assert(!num.empty);
} body {
    auto words = num.replace(",", "").replace(" and ", " ")
                 .replace("-", " ").split;

    immutable sign = (words[0] == "minus") ? -1 : +1;
    if (sign == -1)
        words = words[1 .. $];

    BigInt bsmall, total;
    foreach (const word; words) {
        if (small.canFind(word)) {
            bsmall += small.countUntil(word);
        } else if (tens.canFind(word)) {
            bsmall += tens.countUntil(word) * 10;
        } else if (word == "hundred") {
            bsmall *= 100;
        } else if (word == "thousand") {
            total += bsmall * 1000;
            bsmall = 0;
        } else if (huge.canFind(word)) {
            total += bsmall * BigInt(1000) ^^ huge.countUntil(word);
            bsmall = 0;
        } else {
            immutable msg = format("Don't understand %s part of %s",
                                   word, num);
            throw new Exception(msg);
        }
    }

    return sign * (total + bsmall);
}

void main() {
    foreach (immutable n; iota(-10000, 10000, 17))
        assert(n == n.BigInt.spellBigInt.bigIntFromWords);

    foreach (immutable p; 0 .. 20) {
        auto n = 13.BigInt ^^ p;
        assert(n == n.spellBigInt.bigIntFromWords);
    }

    writeln("This shows <==> for a successful round trip, " ~
            " <??> otherwise:");
    foreach (immutable n; [0, -3, 5, -7, 11, -13, 17, -19, 23, -29]) {
        const txt = n.BigInt.spellBigInt;
        auto num = txt.bigIntFromWords;
        writefln("%+4d <%s> %s", n, (n == num) ? "==" : "??", txt);
    }
    writeln;

    long n = 201_021_002_001;
    while (n) {
        const txt = n.BigInt.spellBigInt;
        auto num = txt.bigIntFromWords;
        writefln("%12d <%s> %s", n, (n == num) ? "==" : "??", txt);
        n /= -10;
    }
    const txt = n.BigInt.spellBigInt;
    auto num = txt.bigIntFromWords;
    writefln("%12d <%s> %s", n, (n == num) ? "==" : "??", txt);
}
```

```txt
This shows <==> for a successful round trip,  <??> otherwise:
  +0 <==> zero
  -3 <==> minus three
  +5 <==> five
  -7 <==> minus seven
 +11 <==> eleven
 -13 <==> minus thirteen
 +17 <==> seventeen
 -19 <==> minus nineteen
 +23 <==> twenty-three
 -29 <==> minus twenty-nine

201021002001 <==> two hundred and one billion, twenty-one million, two thousand, and one
-20102100200 <==> minus twenty billion, one hundred and two million, one hundred thousand, and two hundred
  2010210020 <==> two billion, ten million, two hundred and ten thousand, and twenty
  -201021002 <==> minus two hundred and one million, twenty-one thousand, and two
    20102100 <==> twenty million, one hundred and two thousand, and one hundred
    -2010210 <==> minus two million, ten thousand, two hundred and ten
      201021 <==> two hundred and one thousand, and twenty-one
      -20102 <==> minus twenty thousand, one hundred and two
        2010 <==> two thousand, and ten
        -201 <==> minus two hundred and one
          20 <==> twenty
          -2 <==> minus two
           0 <==> zero
```



## Common Lisp

A counterpart to (format t "~R" ...).

```Lisp
(defpackage number-names
  (:use cl))

(in-package number-names)

(defparameter *ones*
  '((one   . 1)
    (two   . 2)
    (three . 3)
    (four  . 4)
    (five  . 5)
    (six   . 6)
    (seven . 7)
    (eight . 8)
    (nine  . 9)))

(defparameter *teens*
  '((ten       . 10)
    (eleven    . 11)
    (twelve    . 12)
    (thirteen  . 13)
    (fourteen  . 14)
    (fifteen   . 15)
    (sixteen   . 16)
    (seventeen . 17)
    (eighteen  . 18)
    (nineteen  . 19)))

(defparameter *tens*
  '((twenty  . 20)
    (thirty  . 30)
    (forty   . 40)
    (fifty   . 50)
    (sixty   . 60)
    (seventy . 70)
    (eighty  . 80)
    (ninety  . 90)))

(defparameter *hundred*
  '((hundred . 100)))

(defparameter *illions*
  '((quintillion . 1000000000000000000)
    (quadrillion . 1000000000000000)
    (trillion    . 1000000000000)
    (billion     . 1000000000)
    (million     . 1000000)
    (thousand    . 1000)))

(defparameter *delims* '(#\Space #\Tab #\Newline #\-))

;; Turn a single delimited word into an atom.
(defun tokenize-word (word)
  (let ((stream (make-string-output-stream)))
    (loop do
         (let ((char (pop word)))
           (cond ((null char) (return))
                 ((member char *delims*) (return))
                 (t (write-char char stream)))))
    (let ((out (get-output-stream-string stream)))
      (values (intern (string-upcase out) 'number-names)
              word))))

;; Tokenize the input string.
(defun tokenize (word)
  (let ((word (coerce word 'list))
        (tokens (list)))
    (loop do
         (let ((char (pop word)))
           (cond ((null char) (return))
                 ((member char *delims*) nil)
                 (t (multiple-value-bind (token rest-word)
                        (tokenize-word (push char word))
                      (setf word rest-word)
                      (push token tokens))))))
    (reverse tokens)))

;; Define a state machine to parse a subsection of a number
;; that precedes an -illion.
(defmacro defstate (name end-transitions-p &rest transitions)
  (let ((token (gensym "TOKEN"))
        (number (gensym "NUMBER"))
        (illions (gensym "ILLIONS"))
        (illion (gensym "ILLION")))
  `(defun ,name (,token ,number ,illions)
     ,(append '(cond)
              (loop for trans in transitions collect
                   (destructuring-bind (place to-state op) trans
                     `((assoc ,token ,place)
                       (values ',to-state
                               (,op ,number (cdr (assoc ,token ,place)))))))
              (when end-transitions-p
                `(((assoc ,token ,illions)
                   (throw 'done
                     (let ((,illion (assoc ,token ,illions)))
                       (values (* ,number (cdr ,illion)) (car ,illion)))))
                  ((null ,token) (throw 'done (values ,number nil)))))
              `((t (error "Unexpected token ~a" ,token)))))))

(defstate state-a nil
  (*ones* state-b +)
  (*tens* state-d +)
  (*teens* state-e +))

(defstate state-b t
  (*hundred* state-c *))

(defstate state-c t
  (*ones* state-e +)
  (*tens* state-d +)
  (*teens* state-e +))

(defstate state-d t
  (*ones* state-e +))

(defstate state-e t)

(defun consume-illions (illion illions)
  (cond ((null illions) nil)
        ((eq illion (caar illions)) (cdr illions))
        (t (consume-illions illion (cdr illions)))))

;; Parse a number up to the next -illion.
;; Errors on numbers that (format t "~R" ..)
;; would not generate, like "one thousand one million".
(defun parse-sub-number (tokens illions)
  (let ((number 0)
        (state 'state-a))
    (multiple-value-bind (number illion)
        (catch 'done
          (loop do
               (let ((token (pop tokens)))
                 (multiple-value-bind (next-state next-number)
                     (funcall state token number illions)
                   (setf state next-state)
                   (setf number next-number)))))
      (values number
              (if illion
                  (consume-illions illion illions)
                  illions)
              tokens))))

;; Parse the list of tokenized number parts.
(defun parse-number (tokens)
  (let ((illions *illions*)
        (total 0)
        (negative-p (eq (car tokens) 'negative)))
    (when negative-p (pop tokens))
    (if (eq (car tokens) 'zero)
        (if (null (cdr tokens))
            0
            (error "Unexpected token ~a" (cadr tokens)))
        (loop do
             (multiple-value-bind (number new-illions rest-tokens)
                 (parse-sub-number tokens illions)
               (setf illions new-illions)
               (incf total number)
               (setf tokens rest-tokens)
               (unless tokens (return (* (if negative-p -1 1) total))))))))

(defun parse (word)
  (parse-number (tokenize word)))

(defun test ()
  (let ((test-numbers
         '(+0
           -3
           +5
           -7
           +11
           -13
           +17
           -19
           +23
           -29
           201021002001
           -20102100201
           2010210020
           -201021002
           20102100
           -2010210
           201021
           -20103
           2010
           -201
           20
           -2
           0)))
    (princ "number => (format t \"~R\" number) => (parse (format t \"~R\" number))")
    (terpri)
    (mapc (lambda (number)
            (let ((word (format nil "~R" number)))
              (format t "~a => ~a => ~a~%" number word (parse word))))
          test-numbers))
  (values))
```

Running the test procedure:

```none
CL-USER> (number-names::test)
number => (format t "~R" number) => (parse (format t "~R" number))
0 => zero => 0
-3 => negative three => -3
5 => five => 5
-7 => negative seven => -7
11 => eleven => 11
-13 => negative thirteen => -13
17 => seventeen => 17
-19 => negative nineteen => -19
23 => twenty-three => 23
-29 => negative twenty-nine => -29
201021002001 => two hundred one billion twenty-one million two thousand one => 201021002001
-20102100201 => negative twenty billion one hundred two million one hundred thousand two hundred one => -20102100201
2010210020 => two billion ten million two hundred ten thousand twenty => 2010210020
-201021002 => negative two hundred one million twenty-one thousand two => -201021002
20102100 => twenty million one hundred two thousand one hundred => 20102100
-2010210 => negative two million ten thousand two hundred ten => -2010210
201021 => two hundred one thousand twenty-one => 201021
-20103 => negative twenty thousand one hundred three => -20103
2010 => two thousand ten => 2010
-201 => negative two hundred one => -201
20 => twenty => 20
-2 => negative two => -2
0 => zero => 0
; No value
```



## Go

```go
package main

import (
    "fmt"
    "math"
    "regexp"
    "strings"
)

var names = map[string]int64{
    "one":         1,
    "two":         2,
    "three":       3,
    "four":        4,
    "five":        5,
    "six":         6,
    "seven":       7,
    "eight":       8,
    "nine":        9,
    "ten":         10,
    "eleven":      11,
    "twelve":      12,
    "thirteen":    13,
    "fourteen":    14,
    "fifteen":     15,
    "sixteen":     16,
    "seventeen":   17,
    "eighteen":    18,
    "nineteen":    19,
    "twenty":      20,
    "thirty":      30,
    "forty":       40,
    "fifty":       50,
    "sixty":       60,
    "seventy":     70,
    "eighty":      80,
    "ninety":      90,
    "hundred":     100,
    "thousand":    1000,
    "million":     1000000,
    "billion":     1000000000,
    "trillion":    1000000000000,
    "quadrillion": 1000000000000000,
    "quintillion": 1000000000000000000,
}

var seps = regexp.MustCompile(`,|-| and | `)
var zeros = regexp.MustCompile(`^(zero|nought|nil|none|nothing)$`)

func nameToNum(name string) (int64, error) {
    text := strings.ToLower(strings.TrimSpace(name))
    isNegative := strings.HasPrefix(text, "minus ")
    if isNegative {
        text = text[6:]
    }
    if strings.HasPrefix(text, "a ") {
        text = "one" + text[1:]
    }
    words := seps.Split(text, -1)
    for i := len(words) - 1; i >= 0; i-- {
        if words[i] == "" {
            if i < len(words)-1 {
                copy(words[i:], words[i+1:])
            }
            words = words[:len(words)-1]
        }
    }
    size := len(words)
    if size == 1 && zeros.MatchString(words[0]) {
        return 0, nil
    }
    var multiplier, lastNum, sum int64 = 1, 0, 0
    for i := size - 1; i >= 0; i-- {
        num, ok := names[words[i]]
        if !ok {
            return 0, fmt.Errorf("'%s' is not a valid number", words[i])
        } else {
            switch {
            case num == lastNum, num >= 1000 && lastNum >= 100:
                return 0, fmt.Errorf("'%s' is not a well formed numeric string", name)
            case num >= 1000:
                multiplier = num
                if i == 0 {
                    sum += multiplier
                }
            case num >= 100:
                multiplier *= 100
                if i == 0 {
                    sum += multiplier
                }
            case num >= 20 && lastNum >= 10 && lastNum <= 90:
                return 0, fmt.Errorf("'%s' is not a well formed numeric string", name)
            case num >= 20:
                sum += num * multiplier
            case lastNum >= 1 && lastNum <= 90:
                return 0, fmt.Errorf("'%s' is not a well formed numeric string", name)
            default:
                sum += num * multiplier
            }
        }
        lastNum = num
    }

    if isNegative && sum == -sum {
        return math.MinInt64, nil
    }
    if sum < 0 {
        return 0, fmt.Errorf("'%s' is outside the range of an int64", name)
    }
    if isNegative {
        return -sum, nil
    } else {
        return sum, nil
    }
}

func main() {
    names := [...]string{
        "none",
        "one",
        "twenty-five",
        "minus one hundred and seventeen",
        "hundred and fifty-six",
        "minus two thousand two",
        "nine thousand, seven hundred, one",
        "minus six hundred and twenty six thousand, eight hundred and fourteen",
        "four million, seven hundred thousand, three hundred and eighty-six",
        "fifty-one billion, two hundred and fifty-two million, seventeen thousand, one hundred eighty-four",
        "two hundred and one billion, twenty-one million, two thousand and one",
        "minus three hundred trillion, nine million, four hundred and one thousand and thirty-one",
        "seventeen quadrillion, one hundred thirty-seven",
        "a quintillion, eight trillion and five",
        "minus nine quintillion, two hundred and twenty-three quadrillion, three hundred and seventy-two trillion, thirty-six billion, eight hundred and fifty-four million, seven hundred and seventy-five thousand, eight hundred and eight",
    }
    for _, name := range names {
        num, err := nameToNum(name)
        if err != nil {
            fmt.Println(err)
        } else {
            fmt.Printf("%20d = %s\n", num, name)
        }
    }
}
```


```txt

                   0 = none
                   1 = one
                  25 = twenty-five
                -117 = minus one hundred and seventeen
                 156 = hundred and fifty-six
               -2002 = minus two thousand two
                9701 = nine thousand, seven hundred, one
             -626814 = minus six hundred and twenty six thousand, eight hundred and fourteen
             4700386 = four million, seven hundred thousand, three hundred and eighty-six
         51252017184 = fifty-one billion, two hundred and fifty-two million, seventeen thousand, one hundred eighty-four
        201021002001 = two hundred and one billion, twenty-one million, two thousand and one
    -300000009401031 = minus three hundred trillion, nine million, four hundred and one thousand and thirty-one
   17000000000000137 = seventeen quadrillion, one hundred thirty-seven
 1000008000000000005 = a quintillion, eight trillion and five
-9223372036854775808 = minus nine quintillion, two hundred and twenty-three quadrillion, three hundred and seventy-two trillion, thirty-six billion, eight hundred and fifty-four million, seven hundred and seventy-five thousand, eight hundred and eight

```



## Haskell

```Haskell
import Data.Char (toLower)

type Symbol = (String, Integer)
type BinOp = (Integer -> Integer -> Integer)
type State = [Transition]

data Transition = Transition [Symbol] State BinOp
                | Illion State BinOp
                | Done

type Words = [String]
type Accumulator = Integer
type TapeValue = (Accumulator, [Symbol], Words)

ones, teens, tens, hundred, illions :: [Symbol]
ones =
    [("one", 1)
    ,("two", 2)
    ,("three", 3)
    ,("four", 4)
    ,("five", 5)
    ,("six", 6)
    ,("seven", 7)
    ,("eight", 8)
    ,("nine", 9)]

teens =
    [("ten", 10)
    ,("eleven", 11)
    ,("twelve", 12)
    ,("thirteen", 13)
    ,("fourteen", 14)
    ,("fifteen", 15)
    ,("sixteen", 16)
    ,("seventeen", 17)
    ,("eighteen", 18)
    ,("nineteen", 19)]

tens =
    [("twenty", 20)
    ,("thirty", 30)
    ,("forty", 40)
    ,("fifty", 50)
    ,("sixty", 60)
    ,("seventy", 70)
    ,("eighty", 80)
    ,("ninety", 90)]

hundred =
    [("hundred", 100)]

illions =
    [("quintillion", 10 ^ 18)
    ,("quadrillion", 10 ^ 15) 
    ,("trillion", 10 ^ 12)
    ,("billion", 10 ^ 9)
    ,("million", 10 ^ 6)
    ,("thousand", 10 ^ 3)]

tokenize :: String -> Words
tokenize = words . (map replace) . (map toLower)
    where
      replace c
          | elem c ['a'..'z'] = c
          | otherwise         = ' '

lookupRest :: (Eq a) => a -> [(a,b)] -> Maybe (b, [(a,b)])
lookupRest _ [] = Nothing
lookupRest x ((y,z):ws) = if x == y
                          then Just (z, ws)
                          else lookupRest x ws

runState :: State -> TapeValue -> TapeValue
runState []     (_, _, word:_)             = error $ "Unexpected token: " ++ word
runState _      tv@(_, _, [])              = tv
runState (t:ts) tv@(int, illions, word:wx) =
    case t of
      Transition table state op ->
          case lookup word table of
            Nothing  -> runState ts tv
            Just num -> runState state (op num int, illions, wx)
      Illion state op           ->
          case lookupRest word illions of
            Nothing              -> runState ts tv
            Just (num, illions') -> runState state (op num int, illions', wx)
      Done                      -> tv

stateIllion, stateA, stateB, stateC, stateD, stateE :: State
stateIllion = [Illion [Done] (*)]

stateA = [Transition ones stateB (+)
         ,Transition tens stateD (+)
         ,Transition teens stateE (+)]

stateB = [Transition hundred stateC (*)]
         ++ stateIllion

stateC = [Transition ones stateE (+)
         ,Transition tens stateD (+)
         ,Transition teens stateE (+)]
         ++ stateIllion

stateD = [Transition ones stateE (+)]
         ++ stateIllion

stateE = stateIllion ++ [Done]

parseSubWord :: [Symbol] -> Words -> TapeValue
parseSubWord illions w = runState stateA (0, illions, w)
                  
parse :: [Symbol] -> Words -> Integer
parse _       [] = 0
parse illions wx = let (i, illions', wx') = parseSubWord illions wx
                   in i + parse illions' wx'

integerSpell :: String -> Integer
integerSpell wx =
    case tokenize wx of
      ("negative":"zero":[]) -> -0
      ("zero":[])            -> 0
      ("negative":wx')       -> negate $ parse illions wx'
      wx'                    -> parse illions wx'
```



## J

Define the verb usinv to convert number names to numbers.
File number_names.ijs contains the code of [[Number_names#J|number names project]].

```J

NB. standard profile defines  tolower  and  delete extra blanks.
load'number_names.ijs'

cut =: #@:[ }.&.> [ (E. <;.1 ]) ,

usinv =: 3 : 0
 U =. 'ones' ; }. ENU
 A0 =. ;@:(' and'&cut)^:([: +./ ' and '&E.) tolower deb y NB. standardize to us form.
 A =. ,&' ones'^:(U -.@e.~ [: {: ;:) A0
 B =. ', ' cut A NB. box the comma separated phrases.
 C =. ' ' cut L:0 B NB. box words within phrases.
 M =. ENU (1000x ^ #@:[ | (i. {:&>)) C  NB. powers of 1000
 assert *./ 2 >/\ M NB. the phrases properly ordered.
 D=. (<'hundred')&cut&> C
 M +/ .*+/"1 ,"2 (([: (* 100 ^ 2 ~: #) (#EN100)|EN100&i.)&>)D
)

```


```txt

   (-: [&.(us :.usinv))0
1
   (-: [&.(us :.usinv))2340202340220204
1
   (-: [&.:(us :.usinv))2340202340220204x
1

```



## Kotlin


```scala
// version 1.1.2

val names = mapOf<String, Long>(
    "one" to 1,
    "two" to 2,
    "three" to 3,
    "four" to 4,
    "five" to 5,
    "six" to 6,
    "seven" to 7,
    "eight" to 8,
    "nine" to 9,
    "ten" to 10,
    "eleven" to 11,
    "twelve" to 12,
    "thirteen" to 13,
    "fourteen" to 14,
    "fifteen" to 15,
    "sixteen" to 16,
    "seventeen" to 17,
    "eighteen" to 18,
    "nineteen" to 19,
    "twenty" to 20,
    "thirty" to 30,
    "forty" to 40,
    "fifty" to 50,
    "sixty" to 60,
    "seventy" to 70,
    "eighty" to 80,
    "ninety" to 90,
    "hundred" to 100,
    "thousand" to 1_000,
    "million" to 1_000_000,
    "billion" to 1_000_000_000,
    "trillion" to 1_000_000_000_000L,
    "quadrillion" to 1_000_000_000_000_000L,
    "quintillion" to 1_000_000_000_000_000_000L
)

val zeros = listOf("zero", "nought", "nil", "none", "nothing")

fun nameToNum(name: String): Long {
    var text = name.trim().toLowerCase()
    val isNegative = text.startsWith("minus ")
    if (isNegative) text = text.drop(6)
    if (text.startsWith("a ")) text = "one" + text.drop(1)
    val words = text.split(",", "-", " and ", " ").filter { it != "" }

    val size = words.size
    if (size == 1 && words[0] in zeros) return 0L

    var multiplier = 1L
    var lastNum = 0L
    var sum = 0L
    for (i in size - 1 downTo 0) {
        val num: Long? = names[words[i]]
        if (num == null)
            throw IllegalArgumentException("'${words[i]}' is not a valid number")
        else if (num == lastNum)
            throw IllegalArgumentException("'$name' is not a well formed numeric string")
        else if (num >= 1000) {
            if (lastNum >= 100)
                throw IllegalArgumentException("'$name' is not a well formed numeric string")
            multiplier = num
            if (i == 0) sum += multiplier
        } else if (num >= 100) {
            multiplier *= 100
            if (i == 0) sum += multiplier
        } else if (num >= 20) {
            if (lastNum in 10..90)
                throw IllegalArgumentException("'$name' is not a well formed numeric string")
            sum += num * multiplier
        } else {
            if (lastNum in 1..90)
                throw IllegalArgumentException("'$name' is not a well formed numeric string")
            sum += num * multiplier
        }
        lastNum = num
    }

    if (isNegative && sum == -sum)
        return Long.MIN_VALUE
    else if (sum < 0L)
        throw IllegalArgumentException("'$name' is outside the range of a Long integer")

    return if (isNegative) -sum else sum
}

fun main(args: Array<String>) {
    val names = arrayOf(
        "none",
        "one",
        "twenty-five",
        "minus one hundred and seventeen",
        "hundred and fifty-six",
        "minus two thousand two",
        "nine thousand, seven hundred, one",
        "minus six hundred and twenty six thousand, eight hundred and fourteen",
        "four million, seven hundred thousand, three hundred and eighty-six",
        "fifty-one billion, two hundred and fifty-two million, seventeen thousand, one hundred eighty-four",
        "two hundred and one billion, twenty-one million, two thousand and one",
        "minus three hundred trillion, nine million, four hundred and one thousand and thirty-one",
        "seventeen quadrillion, one hundred thirty-seven",
        "a quintillion, eight trillion and five",
        "minus nine quintillion, two hundred and twenty-three quadrillion, three hundred and seventy-two trillion, thirty-six billion, eight hundred and fifty-four million, seven hundred and seventy-five thousand, eight hundred and eight"
    )
    for (name in names) println("${"%20d".format(nameToNum(name))} = $name")
}
```


```txt

                   0 = none
                   1 = one
                  25 = twenty-five
                -117 = minus one hundred and seventeen
                 156 = hundred and fifty-six
               -2002 = minus two thousand two
                9701 = nine thousand, seven hundred, one
             -626814 = minus six hundred and twenty six thousand, eight hundred and fourteen
             4700386 = four million, seven hundred thousand, three hundred and eighty-six
         51252017184 = fifty-one billion, two hundred and fifty-two million, seventeen thousand, one hundred eighty-four
        201021002001 = two hundred and one billion, twenty-one million, two thousand and one
    -300000009401031 = minus three hundred trillion, nine million, four hundred and one thousand and thirty-one
   17000000000000137 = seventeen quadrillion, one hundred thirty-seven
 1000008000000000005 = a quintillion, eight trillion and five
-9223372036854775808 = minus nine quintillion, two hundred and twenty-three quadrillion, three hundred and seventy-two trillion, thirty-six billion, eight hundred and fifty-four million, seven hundred and seventy-five thousand, eight hundred and eight

```



## Perl

The following code reads a file line-by-line. It echos comment lines starting with a hashmark and blank lines. Remaining lines are output followed by an arrow "=>" and any non-negative integer number names translated into numbers, e.g., a line with "ninety-nine" is output like this: "ninety-nine => 99".

```perl
use strict;
use List::Util qw(sum);

our %nums = (
    zero      => 0,	one	=> 1,	two	=> 2,	three	=> 3,
    four      => 4,	five	=> 5,	six	=> 6,	seven	=> 7,
    eight     => 8,	nine	=> 9,	ten	=> 10,	eleven	=> 11,
    twelve    => 12,	thirteen => 13,	fourteen => 14,	fifteen	=> 15,
    sixteen   => 16,	seventeen => 17, eighteen => 18, nineteen => 19,
    twenty    => 20,	thirty	=> 30,	forty	=> 40,	fifty	=> 50,
    sixty     => 60,	seventy	=> 70,	eighty	=> 80,	ninety	=> 90,
    hundred   => 100,	thousand => 1_000, million => 1_000_000,
    billion   => 1_000_000_000,         trillion => 1_000_000_000_000,
    # My ActiveState Win32 Perl uses e-notation after 999_999_999_999_999
    quadrillion => 1e+015,              quintillion =>  1e+018);

# Groupings for thousands, millions, ..., quintillions
our $groups = qr/\d{4}|\d{7}|\d{10}|\d{13}|1e\+015|1e\+018/;

# Numeral or e-notation
our $num = qr/\d+|\d+e\+\d+/;

while (<>) {
        # skip blank lines
        if(/^\s*$/) { print; next; }
        # echo comment lines
        if( /^\s*#.*$/ ) { print; next; }
	chomp;
	my $orig = $_;
	s/-/ /g;                # convert hyphens to spaces
	s/\s\s+/ /g;            # remove duplicate whitespace, convert ws to space
	s/ $//g;                # remove trailing blank
	s/^ //g;                # remove leading blank
	$_ = lc($_);            # convert to lower case
	# tokenize sentence boundaries
	s/([\.\?\!]) / $1\n/g;
	s/([\.\?\!])$/ $1\n/g;
	# tokenize other punctuation and symbols
	s/\$(.)/\$ $1/g;             # prefix
	s/(.)([\;\:\%',])/$1 $2/g;   # suffix

	foreach my $key (keys %nums) { s/\b$key\b/$nums{$key}/eg; }

	s/(\d) , (\d)/$1 $2/g;
	s/(\d) and (\d)/$1 $2/g;

	s/\b(\d) 100 (\d\d) (\d) (${groups})\b/($1 * 100 + $2 + $3) * $4/eg;

	s/\b(\d) 100 (\d\d) (${groups})\b/($1 * 100 + $2) * $3/eg;
	s/\b(\d) 100 (\d) (${groups})\b/($1 * 100 + $2) * $3/eg;
	s/\b(\d) 100 (${groups})\b/$1 * $2 * 100/eg;

        s/\b100 (\d\d) (\d) (${groups})\b/(100 + $1 + $2) * $3/eg;
        s/\b100 (\d\d) (${groups})\b/(100 + $1) * $2/eg;
        s/\b100 (\d) (${groups})\b/(100 + $1) * $2/eg;
        s/\b100 (${groups})\b/$1 * 100/eg;

	s/\b(\d\d) (\d) (${groups})\b/($1 + $2) * $3/eg;
	s/\b(\d{1,2}) (${groups})\b/$1 * $2/eg;

	s/\b(\d\d) (\d) 100\b/($1 + $2) * 100/eg;
	s/\b(\d{1,2}) 100\b/$1 * 100/eg;

        # Date anomolies: nineteen eighty-four and twenty thirteen
	s/\b(\d{2}) (\d{2})\b/$1 * 100 + $2/eg;

	s/((?:${num} )*${num})/sum(split(" ",$1))/eg;

	print $orig, " => ", $_, "\n";
}
```

Here is a sample '''input''' file:

```txt

# For numbers between 21 and 99 inclusive, we're supposed to use a hyphen,
# but the bank still cashes our check without the hyphen:
Seventy-two dollars
Seventy two dollars

# For numbers bigger than 100, we're not supposed to use "and,"
# except we still use "and" anyway, e.g.,
One Hundred and One Dalmatians
A Hundred and One Dalmatians
One Hundred One Dalmatians
Hundred and One Dalmatians
One Thousand and One Nights
Two Thousand and One: A Space Odyssey

# Date anomolies
Twenty Thirteen
Nineteen Eighty-Four

# Maximum value an "unsigned long int" can hold on a 32-bit machine = 2^32 - 1
# define ULONG_MAX	4294967295
four billion, two hundred ninety-four million, nine hundred sixty-seven thousand, two hundred ninety five

# Max positive integer on 32-bit Perl is 9_007_199_254_740_992 = 2^53
# Use Math::BigInt if you need more.
# Note Perl usually stringifies to 15 digits of precision and this has 16
Nine quadrillion, seven trillion, one hundred ninety-nine billion, two hundred fifty-four million, seven hundred forty thousand, nine hundred ninety two

Nine Hundred Ninety-Nine
One Thousand One Hundred Eleven
Eleven Hundred Eleven
Eight Thousand Eight Hundred Eighty-Eight
Eighty-Eight Hundred Eighty-Eight
Seven Million Seven Hundred Seventy-Seven Thousand Seven Hundred Seventy-Seven
Ninety-Nine Trillion Nine Hundred Ninety-Nine Billion Nine Hundred Ninety-Nine Million Nine Hundred Ninety-Nine Thousand Nine Hundred Ninety-Nine

ninety-nine
three hundred
three hundred and ten
one thousand, five hundred and one
twelve thousand, six hundred and nine
five hundred and twelve thousand, six hundred and nine
forty-three million, one hundred and twelve thousand, six hundred and nine
two billion, one hundred

zero
eight
one hundred
one hundred twenty three
one thousand one
ninety nine thousand nine hundred ninety nine
one hundred thousand
nine billion one hundred twenty three million four hundred fifty six thousand seven hundred eighty nine
one hundred eleven billion one hundred eleven

```

And here is the resulting '''output''' file:

```txt

# For numbers between 21 and 99 inclusive, we're supposed to use a hyphen,
# but the bank still cashes our check without the hyphen:
Seventy-two dollars => 72 dollars
Seventy two dollars => 72 dollars

# For numbers bigger than 100, we're not supposed to use "and,"
# except we still use "and" anyway, e.g.,
One Hundred and One Dalmatians => 101 dalmatians
A Hundred and One Dalmatians => a 101 dalmatians
One Hundred One Dalmatians => 101 dalmatians
Hundred and One Dalmatians => 101 dalmatians
One Thousand and One Nights => 1001 nights
Two Thousand and One: A Space Odyssey => 2001 : a space odyssey

# Date anomolies
Twenty Thirteen => 2013
Nineteen Eighty-Four => 1984

# Maximum value an "unsigned long int" can hold on a 32-bit machine = 2^32 - 1
# define ULONG_MAX	4294967295
four billion, two hundred ninety-four million, nine hundred sixty-seven thousand, two hundred ninety five => 4294967295

# Max positive integer on 32-bit Perl is 9_007_199_254_740_992 = 2^53
# Use Math::BigInt if you need more.
# Note Perl usually stringifies to 15 digits of precision and this has 16
Nine quadrillion, seven trillion, one hundred ninety-nine billion, two hundred fifty-four million, seven hundred forty thousand, nine hundred ninety two => 9.00719925474099e+015

Nine Hundred Ninety-Nine => 999
One Thousand One Hundred Eleven => 1111
Eleven Hundred Eleven => 1111
Eight Thousand Eight Hundred Eighty-Eight => 8888
Eighty-Eight Hundred Eighty-Eight => 8888
Seven Million Seven Hundred Seventy-Seven Thousand Seven Hundred Seventy-Seven => 7777777
Ninety-Nine Trillion Nine Hundred Ninety-Nine Billion Nine Hundred Ninety-Nine Million Nine Hundred Ninety-Nine Thousand Nine Hundred Ninety-Nine => 99999999999999

ninety-nine => 99
three hundred => 300
three hundred and ten => 310
one thousand, five hundred and one => 1501
twelve thousand, six hundred and nine => 12609
five hundred and twelve thousand, six hundred and nine => 512609
forty-three million, one hundred and twelve thousand, six hundred and nine => 43112609
two billion, one hundred => 2000000100

zero => 0
eight => 8
one hundred => 100
one hundred twenty three => 123
one thousand one => 1001
ninety nine thousand nine hundred ninety nine => 99999
one hundred thousand => 100000
nine billion one hundred twenty three million four hundred fifty six thousand seven hundred eighty nine => 9123456789
one hundred eleven billion one hundred eleven => 111000000111

```



## Perl 6

```perl6
my $DEBUG = False;

constant @M = ('ones', 'thousand',
    ((<m b tr quadr quint sext sept oct non>,
        (map { ('', <un duo tre quattuor quin sex septen octo novem>).flat X~ $_ },
	    <dec vigint trigint quadragint quinquagint sexagint septuagint octogint nonagint>),
	        'cent').flat X~ 'illion')).flat;

constant %M = @M Z=> (1, 1000, 1000000 ... *);

grammar Number-Names {
    token TOP { ^ [.*? <number>]* .* $ }

    method returns {
	say callframe(1).subname, ' -> ', self.ast if $DEBUG;
	self;
    }
 
    method trimspace {
	my $pos = self.pos;
	--$pos while $pos and self.orig.substr($pos-1,1) ~~ /\s/;
	self.cursor($pos);
    }

    token gotsome($oldself) {
	<?{ self.pos != $oldself.pos }>
    }

    token number { :i «
	:my $*SOFAR = 0;
	[
	| 'zero' { make 0 }
	| ('minus' \s+ | 'negative' \s+)?
	    <group>+ % [','\s+|'and'\s+|<?after \s>] <quantnoun>
	    { make [+]($<group>[]».ast) * ($0 ?? -1 !! 1) * $<quantnoun>.ast }
	]
	<.gotsome(self)>
	<.trimspace>
	<.returns>
    }

    token group { :i
	<?{ $*SOFAR %% 1000 }>
	<hundreds> \s* <tweens($*SOFAR + $<hundreds>.ast)> \s* <zillions>
	<.gotsome(self)>
	\s*[ $ || <?after \W> || <?before \W> || <?before < th st nd rd th > » > ]
	{
	    my $group = $<zillions>.ast * ($<hundreds>.ast + $<tweens>.ast);
	    return () if $*SOFAR and $group > $*SOFAR;
	    $*SOFAR += $group;
	    make $group;
	}
	<.returns>
    }

    token hundreds { :i
	[
	| <date> <!!{ !$*SOFAR }>     { make $<date>.ast }
	| <ones>  ['-'|\s*] 'hundred' { make $<ones>.ast * 100 }
	| <?{ not $*SOFAR }>
	  <tweeny>['-'|\s*] 'hundred' { make $<tweeny>.ast * 100 }
	| ''                          { make 0 }
	]
	<.returns>
    }

    token tweens($c = 0) { :i
	:my $*SOFAR = $c;
	\s*
	[
	| 'ten'                           { make 10 }
	| <teens> <!before \s*'hundred'>  { make $<teens>.ast }
	| <ones> \s+ 'and' \s+ <tens>     { make $<ones>.ast + $<tens>.ast }
	| <scores> \s+ 'and' \s+ <ones>   { make $<scores>.ast + $<ones>.ast }
	| <scores> \s+ 'and' \s+ <tweeny> { make $<scores>.ast + $<tweeny>.ast }
	| <tens>[['-'|\s*['and'\s*]?]<ones>]? { make $<tens>.ast + ($<ones> ?? $<ones>.ast !! 0) }
	| <ones>                          { make $<ones>.ast }
	| <scores>                        { make $<scores>.ast }
	| 'and'\s+ <?{ $c }> <tweens>     { make $<tweens>.ast }
	| (\d+[\.\d+]?) <?{ !$c }>        { make +$0 }
	| ''                              { make 0 }
	]
	\s*
	<.returns>
    }

    token tweeny {
	[
	| <ones>[\s+|\-]                  { make $<ones>.ast }
	| 'ten'[\s+|\-]                   { make 10 }
	| <teens>[\s+|\-]                 { make $<teens>.ast }
	| <tens>[[\s*|\-]<ones>]?[\s+|\-] { make $<tens>.ast + ($<ones> ?? $<ones>.ast !! 0) }
	]
    }

    token date { :i
	<tweeny>
	[
	| 'ten'                          { make $<tweeny>.ast * 100 + 10 }
	| <teens> <!before \s*'hundred'> { make $<tweeny>.ast * 100 + $<teens>.ast }
	| <tens>[['-'|\s*]<ones>]?       { make $<tweeny>.ast * 100 + $<tens>.ast + ($<ones> ?? $<ones>.ast !! 0) }
	| ['ought'|oh?][\s+|\-]<ones>    { make $<tweeny>.ast * 100 + $<ones>.ast }
	]
    }

    token scores { :i
	[
	| ['one'|'a']?['-'|\s*] 'score' { make 20 }
	| 'two'       ['-'|\s*] 'score' { make 40 }
	| 'three'     ['-'|\s*] 'score' { make 60 }
	| 'four'      ['-'|\s*] 'score' { make 80 }
	]
	<.returns>
    }

    token teens { :i
	[
	| 'eleven'    { make 11 }
	| 'twelve'    { make 12 }
	| 'twelf'     { make 12 }
	| 'thirteen'  { make 13 }
	| 'fourteen'  { make 14 }
	| 'fifteen'   { make 15 }
	| 'sixteen'   { make 16 }
	| 'seventeen' { make 17 }
	| 'eighteen'  { make 18 }
	| 'nineteen'  { make 19 }
	]
	<.returns>
    }

    token tens { :i
        [
        | 'twent'[y|ie]  { make 20 }
        | 'thirt'[y|ie]  { make 30 }
        | 'fort'[y|ie]   { make 40 }
        | 'fift'[y|ie]   { make 50 }
        | 'sixt'[y|ie]   { make 60 }
        | 'sevent'[y|ie] { make 70 }
        | 'eight'[y|ie]  { make 80 }
        | 'ninet'[y|ie]  { make 90 }
        ]
        <.returns>
    }

    token ones { :i
	[
	| 'one'              { make 1 }
	| 'fir'<?before st>  { make 1 }
	| 'two'              { make 2 }
	| 'seco'<?before nd> { make 2 }
	| 'three'            { make 3 }
	| 'thi'<?before rd>  { make 3 }
	| 'four'             { make 4 }
	| 'five'             { make 5 }
	| 'fif'<?before th>  { make 5 }
	| 'six'              { make 6 }
	| 'seven'            { make 7 }
	| 'eight'            { make 8 }
	| 'eigh'<?before th> { make 8 }
	| 'nine'             { make 9 }
	| 'nin'<?before th>  { make 9 }
	| <?{ !$*SOFAR }> ['a'\s+]? <?before 'hundred' | 'thousand' | <zillions> <?{ $<zillions>.Str ne '' }> >  { make 1 }
	]
	<.returns>
    }

    token zillions { :i
	[
	| (\w+) <?{ %M{lc ~$0} }> { make +%M{lc ~$0} }
	| <quantnoun> { make $<quantnoun>.ast }
	| '' { make 1 }
	]
	<.returns>
    }

    token quantnoun { :i
	\s*
	[
	| 'pair's? ' of'? { make 2 }
	| 'dozen'         { make 12 }
	| 'gross' ' of'?  { make 144 }
	| ''              { make 1 }
	]
	<.returns>
    }
}

sub commify($_) {
    /\d\d\d\d\d/
	?? .flip.subst(/\d\d\d<?before \d>/, -> $/ {$/~','},:g).flip
	!! $_;
}

sub MAIN ($file = 'numnames') {
    $DEBUG = True if $file eq '-';
    my $fh = $file eq '-' ?? $*IN !! open($file);
    for $fh.lines -> $line {
	say $line;
	my $parse = Number-Names.parse($line);
	if $parse and +$parse<number> {
	    print "\t==> ";
	    my $prev = 0;
	    for $parse<number>[*] -> $n {
		print substr($line, $prev, $n.from - $prev);
		print commify(~$n.ast);
		$prev = $n.to;
	    }
	    say substr($line, $prev);
	}
    }
}
```

```txt
Seventy-two dollars
	==> 72 dollars
Seventy two dollars
	==> 72 dollars
One Hundred and One Dalmatians
	==> 101 Dalmatians
A Hundred and One Dalmatians
	==> 101 Dalmatians
One Hundred One Dalmatians
	==> 101 Dalmatians
Hundred and One Dalmatians
	==> 101 Dalmatians
One Thousand and One Nights
	==> 1001 Nights
Two Thousand and One: A Space Odyssey
	==> 2001: A Space Odyssey
math one-oh-one
	==> math 101
Charlemagne died in eight-fourteen.
	==> Charlemagne died in 814.
ten sixty-six
	==> 1066
Nineteen Ought Three
	==> 1903
Nineteen Eighty-Four
	==> 1984
Twenty Thirteen
	==> 2013
four billion, two hundred ninety-four million, nine hundred sixty-seven thousand, two hundred ninety five
	==> 4,294,967,295
Nine quadrillion, seven trillion, one hundred ninety-nine billion, two hundred fifty-four million, seven hundred forty thousand, nine hundred ninety two
	==> 9,007,199,254,740,992
Nine Hundred Ninety-Nine
	==> 999
One Thousand One Hundred Eleven
	==> 1111
Eleven Hundred Eleven
	==> 1111
Eight Thousand Eight Hundred Eighty-Eight
	==> 8888
Eighty-Eight Hundred Eighty-Eight
	==> 8888
Seven Million Seven Hundred Seventy-Seven Thousand Seven Hundred Seventy-Seven
	==> 7,777,777
Ninety-Nine Trillion Nine Hundred Ninety-Nine Billion Nine Hundred Ninety-Nine Million Nine Hundred Ninety-Nine Thousand Nine Hundred Ninety-Nine
	==> 99,999,999,999,999
ninety-nine
	==> 99
three hundred
	==> 300
three hundred and ten
	==> 310
one thousand, five hundred and one
	==> 1501
twelve thousand, six hundred and nine
	==> 12,609
five hundred and twelve thousand, six hundred and nine
	==> 512,609
forty-three million, one hundred and twelve thousand, six hundred and nine
	==> 43,112,609
two billion, one hundred
	==> 2,000,000,100
zero
	==> 0
eight
	==> 8
one hundred
	==> 100
one hundred twenty three
	==> 123
one thousand one
	==> 1001
ninety nine thousand nine hundred ninety nine
	==> 99,999
one hundred thousand
	==> 100,000
nine billion one hundred twenty three million four hundred fifty six thousand seven hundred eighty nine
	==> 9,123,456,789
one hundred eleven billion one hundred eleven
	==> 111,000,000,111
sing a song of sixpence, a pocket full of rye, four and twenty blackbirds baked in a pie
	==> sing a song of sixpence, a pocket full of rye, 24 blackbirds baked in a pie
Fourscore and seven years ago, our four fathers...
	==> 87 years ago, our 4 fathers...
The Ninety and Nine
	==> The 99
Seven Brides for Seven Brothers
	==> 7 Brides for 7 Brothers
Cheaper by the Dozen
	==> Cheaper by the 12
five dozen roses
	==> 60 roses
a dozen dozen roses
	==> 144 roses
two gross of eggs
	==> 288 eggs
two hundred pairs of socks
	==> 400 socks
The First Noël
	==> The 1st Noël
second place
	==> 2nd place
Forty-second Street
	==> 42nd Street
the twenty-third Psalm
	==> the 23rd Psalm
The Fourth Estate
	==> The 4th Estate
The Fifth Essence
	==> The 5th Essence
The sixth sick shiek's sixth sheep's sick.
	==> The 6th sick shiek's 6th sheep's sick.
in seventh heaven
	==> in 7th heaven
The Eighth Wonder of the World!
	==> The 8th Wonder of the World!
Malher's Ninth Symphony
	==> Malher's 9th Symphony
a tenth of all he had
	==> a 10th of all he had
the eleventh hour
	==> the 11th hour
Twelfth Night
	==> 12th Night
Friday the Thirteenth
	==> Friday the 13th
Twentieth-Century Fox
	==> 20th-Century Fox
one novemseptuagintillion, one octoseptuagintillion, one septenseptuagintillion, one sexseptuagintillion, one quinseptuagintillion, one quattuorseptuagintillion, one treseptuagintillion, one duoseptuagintillion, one unseptuagintillion, one septuagintillion, one novemsexagintillion, one octosexagintillion, one septensexagintillion, one sexsexagintillion, one quinsexagintillion, one quattuorsexagintillion, one tresexagintillion, one duosexagintillion, one unsexagintillion, one sexagintillion, one novemquinquagintillion, one octoquinquagintillion, one septenquinquagintillion, one sexquinquagintillion, one quinquinquagintillion, one quattuorquinquagintillion, one trequinquagintillion, one duoquinquagintillion, one unquinquagintillion, one quinquagintillion, one novemquadragintillion, one octoquadragintillion, one septenquadragintillion, one sexquadragintillion, one quinquadragintillion, one quattuorquadragintillion, one trequadragintillion, one duoquadragintillion, one unquadragintillion, one quadragintillion, one novemtrigintillion, one octotrigintillion, one septentrigintillion, one sextrigintillion, one quintrigintillion, one quattuortrigintillion, one tretrigintillion, one duotrigintillion, one untrigintillion, one trigintillion, one novemvigintillion, one octovigintillion, one septenvigintillion, one sexvigintillion, one quinvigintillion, one quattuorvigintillion, one trevigintillion, one duovigintillion, one unvigintillion, one vigintillion, one novemdecillion, one octodecillion, one septendecillion, one sexdecillion, one quindecillion, one quattuordecillion, one tredecillion, one duodecillion, one undecillion, one decillion, one nonillion, one octillion, one septillion, one sextillion, one quintillion, one quadrillion, one trillion, one billion, one million, one thousand, one
	==> 1,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001,001
```



## Phix

Uses [[Number_names#Phix|Number_names]] as an executable library.

```Phix
--
-- demo\rosetta\Names_to_numbers.exw
--
include demo\rosetta\Number_names.exw

constant {tokens,tokvals} = columnize({{"zero",0},{"one",1},{"two",2},{"three",3},{"four",4},{"five",5},{"six",6},
                                       {"seven",7},{"eight",8},{"nine",9},{"ten",10},
                                       {"eleven",11},{"twelve",12},{"thirteen",13},{"fourteen",14},{"fifteen",15},
                                       {"sixteen",16},{"seventeen",17},{"eighteen",18},{"nineteen",19},
                                       {"twenty",20},{"thirty",30},{"forty",40},{"fifty",50},{"sixty",60},
                                       {"seventy",70},{"eighty",80},{"ninety",90},{"hundred",100},{"thousand",1e3},
                                       {"million",1e6},{"billion",1e9},{"trillion",1e12}})
function parse(string s)
sequence words
integer negmul = 1, hund = 0
atom total = 0, k
    s = substitute(s,",","")
    s = substitute(s," and "," ")
    s = substitute(s,"-"," ")
    words = split(s)
    if length(words)=0 then return "invalid" end if
    if words[1]=="minus" then
        negmul = -1
        words = words[2..$]
    end if
    for i=1 to length(words) do
        k = find(words[i],tokens)
        if k=0 then
            if words[i]!="point" then return "invalid" end if
            total += hund
            hund = 0
            integer tens = 10
            for j=i+1 to length(words) do
                k = find(words[j],tokens)
                if k=0 then return "invalid" end if
                k = tokvals[k]
                if k>9 then return "invalid" end if
                total += k/tens
                tens *= 10
            end for
            exit
        end if
        k = tokvals[k]
        if k<100 then
            hund += k
        elsif k=100 then
            hund *= k
        else
            total += hund*k
            hund = 0
        end if
    end for
    return negmul*(total+hund)  
end function

atom si
    for i=1 to length(Samples) do
        si = Samples[i]
        string s = spell(si)
--      if parse(s)=si then
        if abs(parse(s)-si)<1e-6 then
            s = "<==> "&s
        else
            s = "???? "&s
        end if
        printf(1,"%18s %s\n",{smartp(si),s})
    end for
```

<pre style="font-size: 8px">
                99 <==> ninety-nine
               300 <==> three hundred
               310 <==> three hundred and ten
               417 <==> four hundred and seventeen
              1501 <==> one thousand, five hundred and one
             12609 <==> twelve thousand, six hundred and nine
   200000000000100 <==> two hundred trillion, and one hundred
   999999999999999 <==> nine hundred and ninety-nine trillion, nine hundred and ninety-nine billion, nine hundred and ninety-nine million, nine hundred and ninety-nine thousand, nine hundred and ninety-nine
  -123456787654321 <==> minus one hundred and twenty-three trillion, four hundred and fifty-six billion, seven hundred and eighty-seven million, six hundred and fifty-four thousand, three hundred and twenty-one
   102003000400005 <==> one hundred and two trillion, three billion, four hundred thousand, and five
        1020030004 <==> one billion, twenty million, thirty thousand, and four
            102003 <==> one hundred and two thousand, and three
               102 <==> one hundred and two
                 1 <==> one
                 0 <==> zero
                -1 <==> minus one
               -99 <==> minus ninety-nine
             -1501 <==> minus one thousand, five hundred and one
              1234 <==> one thousand, two hundred and thirty-four
             12.34 <==> twelve point three four
        10000001.2 <==> ten million, and one point two
             0.001 <==> zero point zero zero one
        -2.7182818 <==> minus two point seven one eight two eight one eight
      201021002001 <==> two hundred and one billion, twenty-one million, two thousand, and one
      -20102100200 <==> minus twenty billion, one hundred and two million, one hundred thousand, and two hundred
        2010210020 <==> two billion, ten million, two hundred and ten thousand, and twenty
        -201021002 <==> minus two hundred and one million, twenty-one thousand, and two
          20102100 <==> twenty million, one hundred and two thousand, and one hundred
          -2010210 <==> minus two million, ten thousand, two hundred and ten
            201021 <==> two hundred and one thousand, and twenty-one
            -20102 <==> minus twenty thousand, one hundred and two
              2010 <==> two thousand, and ten
              -201 <==> minus two hundred and one
                20 <==> twenty
                -2 <==> minus two

```



## Python

This example assumes that the module from [[Number_names#Python]] is stored as spell_integer.py.

The example understands the textual format generated from number-to-names module. 

<small>Note: This example and [[Number_names#Python]] need to be kept in sync</small>

```python
from spell_integer import spell_integer, SMALL, TENS, HUGE

def int_from_words(num):
    words = num.replace(',','').replace(' and ', ' ').replace('-', ' ').split()
    if words[0] == 'minus':
        negmult = -1
        words.pop(0)
    else:
        negmult = 1
    small, total = 0, 0
    for word in words:
        if word in SMALL:
            small += SMALL.index(word)
        elif word in TENS:
            small += TENS.index(word) * 10
        elif word == 'hundred':
            small *= 100
        elif word == 'thousand':
            total += small * 1000
            small = 0
        elif word in HUGE:
            total += small * 1000 ** HUGE.index(word)
            small = 0
        else:
            raise ValueError("Don't understand %r part of %r" % (word, num))
    return negmult * (total + small)


if __name__ == '__main__':
    # examples
    for n in range(-10000, 10000, 17):
        assert n == int_from_words(spell_integer(n))

    for n in range(20):
        assert 13**n == int_from_words(spell_integer(13**n))
    
    print('\n##\n## These tests show <==> for a successful round trip, otherwise <??>\n##\n') 
    for n in (0, -3, 5, -7, 11, -13, 17, -19, 23, -29):
        txt = spell_integer(n)
        num = int_from_words(txt)
        print('%+4i <%s> %s' % (n, '==' if n == num else '??', txt))
    print('')  
    
    n = 201021002001
    while n:
        txt = spell_integer(n)
        num = int_from_words(txt)
        print('%12i <%s> %s' % (n, '==' if n == num else '??', txt))
        n //= -10
    txt = spell_integer(n)
    num = int_from_words(txt)
    print('%12i <%s> %s' % (n, '==' if n == num else '??', txt))
    print('')
```

```txt
##
## These tests show <==> for a successful round trip, otherwise <??>
##

  +0 <==> zero
  -3 <==> minus three
  +5 <==> five
  -7 <==> minus seven
 +11 <==> eleven
 -13 <==> minus thirteen
 +17 <==> seventeen
 -19 <==> minus nineteen
 +23 <==> twenty-three
 -29 <==> minus twenty-nine

201021002001 <==> two hundred and one billion, twenty-one million, two thousand, and one
-20102100201 <==> minus twenty billion, one hundred and two million, one hundred thousand, two hundred and one
  2010210020 <==> two billion, ten million, two hundred and ten thousand, and twenty
  -201021002 <==> minus two hundred and one million, twenty-one thousand, and two
    20102100 <==> twenty million, one hundred and two thousand, and one hundred
    -2010210 <==> minus two million, ten thousand, two hundred and ten
      201021 <==> two hundred and one thousand, and twenty-one
      -20103 <==> minus twenty thousand, one hundred and three
        2010 <==> two thousand, and ten
        -201 <==> minus two hundred and one
          20 <==> twenty
          -2 <==> minus two
           0 <==> zero
```



## Racket


Extension of the [[Number_names#Racket | number to names]] code:


```racket

#lang racket

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

(define (integer->english n)
  (define (step div suffix separator [subformat integer->english])
    (define-values [q r] (quotient/remainder n div))
    (define S (if suffix (~a (subformat q) " " suffix) (subformat q)))
    (if (zero? r) S (~a S separator (integer->english r))))
  (cond [(< n 0) (~a "negative " (integer->english (- n)))]
        [(< n 20) (list-ref smalls n)]
        [(< n 100) (step 10 #f "-" (curry list-ref tens))]
        [(< n 1000) (step 100 "hundred" " and ")]
        [else (let loop ([N 1000000] [D 1000] [unit larges])
                (cond [(null? unit)
                       (error 'integer->english "number too big: ~e" n)]
                      [(< n N) (step D (car unit) ", ")]
                      [else (loop (* 1000 N) (* 1000 D) (cdr unit))]))]))

(define (english->integer str)
  (define (word->integer word)
    (or (for/first ([s (in-list smalls)] [n (in-naturals)]
                    #:when (equal? word s))
          n)
        (for/first ([s (in-list tens)] [n (in-naturals)]
                    #:when (equal? word s))
          (* 10 n))
        (for/first ([s (in-list larges)] [n (in-naturals 1)]
                    #:when (equal? word s))
          (expt 10 (* 3 n)))
        0))
  (for/sum ([part (in-list (string-split str #rx" *, *"))])
    (let loop ([part (regexp-split #rx"[ -]" part)] [sum 0])
      (match part
        [(list n)
         (let ([n (word->integer n)]) (if (< 999 n) (* sum n) (+ sum n)))]
        [(list n "hundred" rest ...)
         (loop rest (+ sum (* 100 (word->integer n))))]
        [(list n rest ...)
         (loop rest (+ sum (word->integer n)))]
        [_ sum]))))

(for ([size 10])
  (define e   (expt 10 size))
  (define n   (+ (* e (random e)) (random e)))
  (define eng (integer->english n))
  (define n2  (english->integer eng))
  (if (= n2 n)
    (printf "~s <-> ~a\n" n eng)
    (printf "Fail: ~s -> ~a -> ~s\n" n eng n2)))

```


```txt

0 <-> zero
90 <-> ninety
361 <-> three hundred and sixty-one
528142 <-> five hundred and twenty-eight thousand, one hundred and forty-two
92596865 <-> ninety-two million, five hundred and ninety-six thousand, eight hundred and sixty-five
690856444 <-> six hundred and ninety million, eight hundred and fifty-six thousand, four hundred and forty-four
245479718757 <-> two hundred and forty-five billion, four hundred and seventy-nine million, seven hundred and eighteen thousand, seven hundred and fifty-seven
37308165785935 <-> thirty-seven trillion, three hundred and eight billion, one hundred and sixty-five million, seven hundred and eighty-five thousand, nine hundred and thirty-five
7878883296406183 <-> seven quadrillion, eight hundred and seventy-eight trillion, eight hundred and eighty-three billion, two hundred and ninety-six million, four hundred and six thousand, one hundred and eighty-three
552966175718680570 <-> five hundred and fifty-two quadrillion, nine hundred and sixty-six trillion, one hundred and seventy-five billion, seven hundred and eighteen million, six hundred and eighty thousand, five hundred and seventy

```



## Ruby

This solution uses "Number names" from [[Number_names#Ruby | here]]
```ruby
require 'number_names'

def int_from_words(num)
  words = num.downcase.gsub(/(,| and |-)/,' ').split
  if words[0] =~ /(minus|negative)/
    negmult = -1
    words.shift
  else
    negmult = 1
  end
  small, total = 0, 0
  for word in words
    case word
    when *SMALL
      small += SMALL.index(word)
    when *TENS
      small += TENS.index(word) * 10
    when 'hundred'
      small *= 100
    when 'thousand'
      total += small * 1000
      small = 0
    when *BIG
      total += small * 1000 ** BIG.index(word)
      small = 0
    else
      raise ArgumentError, "Don't understand %s part of %s" % [word, num]
    end
  end
  negmult * (total + small)
end
```


Examples:

```ruby
for n in (-10000..10000).step(17)
  raise unless n == int_from_words(wordify(n))
end

for n in 0...20
  raise unless 13**n == int_from_words(wordify(13**n))
end

puts "##\n## These tests show <==> for a successful round trip, otherwise <??>\n##"
for n in [0, -3, 5, -7, 11, -13, 17, -19, 23, -29]
  txt = wordify(n)
  num = int_from_words(txt)
  puts '%+4i <%s> %s' % [n, n==num ? '==' : '??', txt]
end
puts

n = 201021002001
loop do
  txt = wordify(n)
  num = int_from_words(txt)
  puts '%12i <%s> %s' % [n, n==num ? '==' : '??', txt]
  break if n==0
  n /= -10
end
```


```txt

##
## These tests show <==> for a successful round trip, otherwise <??>
##
  +0 <==> zero
  -3 <==> negative three
  +5 <==> five
  -7 <==> negative seven
 +11 <==> eleven
 -13 <==> negative thirteen
 +17 <==> seventeen
 -19 <==> negative nineteen
 +23 <==> twenty-three
 -29 <==> negative twenty-nine

201021002001 <==> two hundred and one billion, twenty-one million, two thousand, one
-20102100201 <==> negative twenty billion, one hundred and two million, one hundred thousand, two hundred and one
  2010210020 <==> two billion, ten million, two hundred and ten thousand, twenty
  -201021002 <==> negative two hundred and one million, twenty-one thousand, two
    20102100 <==> twenty million, one hundred and two thousand, one hundred
    -2010210 <==> negative two million, ten thousand, two hundred and ten
      201021 <==> two hundred and one thousand, twenty-one
      -20103 <==> negative twenty thousand, one hundred and three
        2010 <==> two thousand, ten
        -201 <==> negative two hundred and one
          20 <==> twenty
          -2 <==> negative two
           0 <==> zero

```



## Sidef

```ruby
func names_to_number(str) {

    static nums = Hash.new(
           zero => 0,             one => 1,             two => 2,
          three => 3,            four => 4,            five => 5,
            six => 6,           seven => 7,           eight => 8,
           nine => 9,             ten => 10,         eleven => 11,
         twelve => 12,       thirteen => 13,       fourteen => 14,
        fifteen => 15,        sixteen => 16,      seventeen => 17,
       eighteen => 18,       nineteen => 19,         twenty => 20,
         thirty => 30,          forty => 40,          fifty => 50,
          sixty => 60,        seventy => 70,         eighty => 80,
         ninety => 90,        hundred => 1e2,      thousand => 1e3,
        million => 1e6,       billion => 1e9,      trillion => 1e12,
    quadrillion => 1e15,  quintillion => 1e18,
    );

    # Groupings for thousands, millions, ..., quintillions
    static groups = /\d{4}|\d{7}|\d{10}|\d{13}|\d{16}|\d{19}/;

    # Numeral
    static num = /\d+/;

    str.trim!;                      # remove leading and trailing whitespace
    str.gsub!('-', ' ');            # convert hyphens to spaces
    str.words!.join!(' ');          # remove duplicate whitespace, convert ws to space
    str.lc!;                        # convert to lower case

    # tokenize sentence boundaries
    str.gsub!(/([.?!]) /, {|a| ' ' + a + "\n"});
    str.gsub!(/([.?!])$/, {|a| ' ' + a + "\n"});

    # tokenize other punctuation and symbols
    str.gsub!(/\$(.)/,           {|a| "$ #{a}" });       # prefix
    str.gsub!(/(.)([;:%'',])/, {|a,b| "#{a} #{b}"});     # suffix

    nums.each { |key, value| str.gsub!(Regex.new('\b' + key + '\b'), value) };

    str.gsub!(/(\d) , (\d)/,   {|a,b| a + ' ' + b});
    str.gsub!(/(\d) and (\d)/, {|a,b| a + ' ' + b});

    static regex = [
        Regex.new('\b(\d) 100 (\d\d) (\d) (' + groups + ')\b'),
        Regex.new('\b(\d) 100 (\d\d) (' + groups + ')\b'),
        Regex.new('\b(\d) 100 (\d) (' + groups + ')\b'),
        Regex.new('\b(\d) 100 (' + groups + ')\b'),
        Regex.new('\b100 (\d\d) (\d) (' + groups + ')\b'),
        Regex.new('\b100 (\d\d) (' + groups + ')\b'),
        Regex.new('\b100 (\d) (' + groups + ')\b'),
        Regex.new('\b100 (' + groups + ')\b'),
        Regex.new('\b(\d\d) (\d) (' + groups + ')\b'),
        Regex.new('\b(\d{1,2}) (' + groups + ')\b'),
        Regex.new('((?:' + num + ' )*' + num + ')'),
    ];

    str.gsub!(regex[0], {|a,b,c,d| (a.to_i*100 + b.to_i + c.to_i) * d.to_i });
    str.gsub!(regex[1], {|a,b,c|   (a.to_i*100 + b.to_i) * c.to_i });
    str.gsub!(regex[2], {|a,b,c|   (a.to_i*100 + b.to_i) * c.to_i });
    str.gsub!(regex[3], {|a,b|     (a.to_i * b.to_i * 100) });
    str.gsub!(regex[4], {|a,b,c|   (100 + a.to_i + b.to_i) * c.to_i });
    str.gsub!(regex[5], {|a,b|     (100 + a.to_i) * b.to_i });
    str.gsub!(regex[6], {|a,b|     (100 + a.to_i) * b.to_i });
    str.gsub!(regex[7], {|a|       (a.to_i * 100) });
    str.gsub!(regex[8], {|a,b,c|   (a.to_i + b.to_i) * c.to_i });
    str.gsub!(regex[9], {|a,b|     (a.to_i * b.to_i) });

    str.gsub!(/\b(\d\d) (\d) 100\b/, {|a,b| (a.to_i + b.to_i) * 100});
    str.gsub!(/\b(\d{1,2}) 100\b/,   {|a|   (a.to_i * 100) });
    str.gsub!(/\b(\d{2}) (\d{2})\b/, {|a,b| (a.to_i * 100) + b.to_i});
    str.gsub!(regex[10], {|a| a.split(' ').map{.to_i}.sum });
}
```


Usage:

```ruby
ARGF.each { |line|
    say "#{line.chomp.dump} --> #{names_to_number(line).dump}";
}
```


Sample:

```txt

$ sidef names2nums.sf input.txt
"Seventy two dollars" --> "72 dollars"
"One Hundred and One Dalmatians" --> "101 dalmatians"
"Two Thousand and One: A Space Odyssey" --> "2001 : a space odyssey"
"Twenty Thirteen" --> "2013"
"Nineteen Eighty-Four" --> "1984"
"one thousand, five hundred and one" --> "1501"
"three hundred and ten" --> "310"
"ninety-nine" --> "99"
"ninety nine thousand nine hundred ninety nine" --> "99999"
"five hundred and twelve thousand, six hundred and nine" --> "512609"
"two billion, one hundred" --> "2000000100"

```



## Tcl

```tcl
package require Tcl 8.6
proc name2num name {
    set words [regexp -all -inline {[a-z]+} [string tolower $name]]
    set tokens {
	"zero" 0 "one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7
	"eight" 8 "nine" 9 "ten" 10 "eleven" 11 "twelve" 12 "thirteen" 13
	"fourteen" 14 "fifteen" 15 "sixteen" 16 "seventeen" 17 "eighteen" 18
	"nineteen" 19 "twenty" 20 "thirty" 30 "forty" 40 "fifty" 50 "sixty" 60
	"seventy" 70 "eighty" 80 "ninety" 90 "hundred" 100 "thousand" 1000
	"million" 1000000 "billion" 1000000000 "trillion" 1000000000000
	"quadrillion" 1000000000000000 "qintillion" 1000000000000000000
    }
    set values {}
    set groups {}
    set previous -inf
    set sign 1
    foreach word $words {
	if {[dict exists $tokens $word]} {
	    set value [dict get $tokens $word]
	    if {$value < $previous} {
		# Check if we have to propagate backwards the "large" terms
		if {[set mult [lindex $values end]] > 99} {
		    for {set i [llength $groups]} {[incr i -1] >= 0} {} {
			if {[lindex $groups $i end] >= $mult} {
			    break
			}
			lset groups $i end+1 $mult
		    }
		}
		lappend groups $values
		set values {}
	    } elseif {$value < 100 && $previous < 100 && $previous >= 0} {
		# Special case: dates
		lappend groups [lappend values 100]
		set values {}
	    }
	    lappend values $value
	    set previous $value
	} elseif {$word eq "minus"} {
	    set sign -1
	}
    }
    lappend groups $values
    set groups [lmap prodgroup $groups {tcl::mathop::* {*}$prodgroup}]
    # Special case: dates
    if {[llength $groups] == 2} {
	if {[lmap g $groups {expr {$g < 100 && $g >= 10}}] eq {1 1}} {
	    lset groups 0 [expr {[lindex $groups 0] * 100}]
	}
    }
    return [expr {$sign * [tcl::mathop::+ {*}$groups]}]
}
```

Demonstrating/testing (based on [[#Perl|Perl]] code's samples):

```tcl
set samples {
    "Seventy-two dollars"
    "Seventy two dollars"
    "One Hundred and One Dalmatians"
    "A Hundred and One Dalmatians"
    "One Hundred One Dalmatians"
    "Hundred and One Dalmatians"
    "One Thousand and One Nights"
    "Two Thousand and One: A Space Odyssey"
    "Twenty Thirteen"
    "Nineteen Eighty-Four"
    "four billion, two hundred ninety-four million, nine hundred sixty-seven thousand, two hundred ninety five"
    "Nine quadrillion, seven trillion, one hundred ninety-nine billion, two hundred fifty-four million, seven hundred forty thousand, nine hundred ninety two"
    "Nine Hundred Ninety-Nine"
    "One Thousand One Hundred Eleven"
    "Eleven Hundred Eleven"
    "Eight Thousand Eight Hundred Eighty-Eight"
    "Eighty-Eight Hundred Eighty-Eight"
    "Seven Million Seven Hundred Seventy-Seven Thousand Seven Hundred Seventy-Seven"
    "Ninety-Nine Trillion Nine Hundred Ninety-Nine Billion Nine Hundred Ninety-Nine Million Nine Hundred Ninety-Nine Thousand Nine Hundred Ninety-Nine"
    "ninety-nine"
    "three hundred"
    "three hundred and ten"
    "one thousand, five hundred and one"
    "twelve thousand, six hundred and nine"
    "five hundred and twelve thousand, six hundred and nine"
    "forty-three million, one hundred and twelve thousand, six hundred and nine"
    "two billion, one hundred"
    "zero"
    "eight"
    "one hundred"
    "one hundred twenty three"
    "one thousand one"
    "ninety nine thousand nine hundred ninety nine"
    "one hundred thousand"
    "nine billion one hundred twenty three million four hundred fifty six thousand seven hundred eighty nine"
    "one hundred eleven billion one hundred eleven"
    "minus fifty six"
}
foreach s $samples {
    puts "$s => [name2num $s]"
}
```

```txt

Seventy-two dollars => 72
Seventy two dollars => 72
One Hundred and One Dalmatians => 101
A Hundred and One Dalmatians => 101
One Hundred One Dalmatians => 101
Hundred and One Dalmatians => 101
One Thousand and One Nights => 1001
Two Thousand and One: A Space Odyssey => 2001
Twenty Thirteen => 2013
Nineteen Eighty-Four => 1984
four billion, two hundred ninety-four million, nine hundred sixty-seven thousand, two hundred ninety five => 4294967295
Nine quadrillion, seven trillion, one hundred ninety-nine billion, two hundred fifty-four million, seven hundred forty thousand, nine hundred ninety two => 9007199254740992
Nine Hundred Ninety-Nine => 999
One Thousand One Hundred Eleven => 1111
Eleven Hundred Eleven => 1111
Eight Thousand Eight Hundred Eighty-Eight => 8888
Eighty-Eight Hundred Eighty-Eight => 8888
Seven Million Seven Hundred Seventy-Seven Thousand Seven Hundred Seventy-Seven => 7777777
Ninety-Nine Trillion Nine Hundred Ninety-Nine Billion Nine Hundred Ninety-Nine Million Nine Hundred Ninety-Nine Thousand Nine Hundred Ninety-Nine => 99999999999999
ninety-nine => 99
three hundred => 300
three hundred and ten => 310
one thousand, five hundred and one => 1501
twelve thousand, six hundred and nine => 12609
five hundred and twelve thousand, six hundred and nine => 512609
forty-three million, one hundred and twelve thousand, six hundred and nine => 43112609
two billion, one hundred => 2000000100
zero => 0
eight => 8
one hundred => 100
one hundred twenty three => 123
one thousand one => 1001
ninety nine thousand nine hundred ninety nine => 99999
one hundred thousand => 100000
nine billion one hundred twenty three million four hundred fifty six thousand seven hundred eighty nine => 9123456789
one hundred eleven billion one hundred eleven => 111000000111
minus fifty six => -56

```



## zkl


```zkl
var names=T("zero","one","two","three","four","five","six","seven","eight",
	    "nine","ten","eleven","twelve","thirteen","fourteen","fifteen",
	    "sixteen","seventeen","eighteen","nineteen","twenty",
	    "thirty","forty","fifty","sixty","seventy","eighty","ninety",
	    "hundred","thousand","million","billion", "trillion")
	 .zip([0..20].chain([30..90,10]).walk().append(
	      100,1000,1000000,1000000000,1000000000000))
	 .toDictionary();

fcn stringToNumber(s){
   s=s.toLower().replace(",","").replace(" and "," ").replace("-"," ");
   words,minus,total,hund,prev := s.split(), 1,0,0,0;
   if(not words) return(0);
   if(words[0]=="minus"){ minus=-1; words=words[1,*]; }
   foreach w in (words){
      n:=names.find(w);
      if(Void==n) throw(Exception.ValueError("I don't know "+w));
      if(n<100){
      	 dn,dp := n.numDigits,prev.numDigits;  // 0,1,2 and 0,1,2,3,...
	 if(dp<3)
	    if((dn==dp==1) or dn==0 or dp==0) hund*=10; 
	    else if(dn==2)		      hund*=100; 
	 hund+=n;
      }
      else if(n==100) hund*=n;
      else{ total+=hund*n; hund=0; }
      prev=n;
   }
   minus*(total + hund)  
}
```


```zkl
foreach s in (T("eighty-five thousand and one ",
     " one hundred and fifty-five thousand and nineteen",
     "one thousand,   nine hundred and eighty-four","Nineteen Eighty-Four", 
     "six thousand Nineteen Eighty-Four",
     "","zero","zero zero one","one zero","one zero two","one one three",
     "ninety one","one ninety","ninety ninety",
     "minus four million, five hundred and forty-seven thousand",
     "six million, seven hundred and sixty-six thousand and twenty-seven")){
   println("\"%s\" is %,d".fmt(s,stringToNumber(s)));
}
```

```txt

"eighty-five thousand and one " is 85,001
" one hundred and fifty-five thousand and nineteen" is 155,019
"one thousand,   nine hundred and eighty-four" is 1,984
"Nineteen Eighty-Four" is 1,984
"six thousand Nineteen Eighty-Four" is 7,984
"" is 0
"zero" is 0
"zero zero one" is 1
"one zero" is 10
"one zero two" is 102
"one one three" is 113
"ninety one" is 91
"one ninety" is 190
"ninety ninety" is 9,090
"minus four million, five hundred and forty-seven thousand" is -4,547,000
"six million, seven hundred and sixty-six thousand and twenty-seven" is 6,766,027

```

