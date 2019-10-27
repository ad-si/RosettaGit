+++
title = "Vampire number"
description = ""
date = 2019-10-11T09:57:29Z
aliases = []
[extra]
id = 13146
[taxonomies]
categories = []
tags = []
+++

{{task|Mathematics}}

A [[wp:Vampire_number|vampire number]] is a natural number with an even number of digits, that can be factored into two integers. These two factors are called the ''fangs'', and must have the following properties:
* they each contain half the number of the digits of the original number
* together they consist of exactly the same digits as the original number
* at most one of them has a trailing zero



An example of a Vampire number and its fangs: <code> 1260 : (21, 60) </code>


;Task:
# Print the first 25 Vampire numbers and their fangs. 
# Check if the following numbers are Vampire numbers and, if  so, print them and their fangs: 
<big><code> 16758243290880,  24959017348650,  14593825548650 </code></big>



Note that a Vampire number can have more than one pair of fangs.


;See also:
* [http://www.numberphile.com/videos/vampire_numbers.html numberphile.com].
* [http://users.cybercity.dk/~dsl522332/math/vampires/ Vampire search algorithm]
* [[oeis:A014575|Vampire numbers on OEIS]]





## AutoHotkey

The following code should work for older (1.0.*) AHK versions as well:

```AutoHotkey
SetBatchLines -1 ; used to improve performance
; (you can make it much faster by removing the informative tooltips)

;********************
; CONFIG
;********************
StartingNumber := 10
NumberLimit := 126030
CounterLimit := 25 ; calculations stop when one of these limits is reached
AdditionalNumbers := "16758243290880,24959017348650,14593825548650"
;********************

CurrentCounter := 0, CurrentNumber := StartingNumber

Loop  {
    if !Mod(A_Index,75) ; informative tooltip (every 75 calculations, to avoid slowing down)
        ToolTip, % "Checking numbers...`nNumber: " CurrentNumber
            . "/" NumberLimit "`nFound: " CurrentCounter "/" CounterLimit
    if ( CurrentCounter >= CounterLimit ) || ( CurrentNumber >= NumberLimit )
        Break
    if Mod(StrLen(CurrentNumber),2)
        CurrentNumber *= 10
    else if ( ( CurrentResult := GetFangs(CurrentNumber) ) <> "" )
        Output .= "`n" CurrentNumber ":`t" CurrentResult, CurrentCounter++
    CurrentNumber++
}
ToolTip ; hide informative tooltip

MsgBox % SubStr(Output,2) ; show output (first part)

Output := ""
Loop, Parse, AdditionalNumbers, % ","
{
    ToolTip, % "Getting fangs for " A_LoopField " ..." ; informative tooltip
    Output .= "`n" A_LoopField ":`n`t" GetFangs(A_LoopField) "`n"
}
ToolTip ; hide informative tooltip

MsgBox % SubStr(Output,2) ; show output (second part - additional numbers)
ExitApp

;----------------------------------------------------------------------------------

CharSorter(Input) { ; required by GetFangs()
    Loop, Parse, Input
        Output .= A_LoopField "`n"
    Sort, Output
    StringReplace, Output, Output, % "`n",, All
    Return Output
}

;----------------------------------------------------------------------------------

GetFangs(CurrentNumber) { ; requires CharSorter()
    ResultIndex := 1
    Length := StrLen(CurrentNumber)
    Power := (Length//2)-1
    if Mod(Length,2) OR !Power
        Return ""
    NumberLimit := Floor(Sqrt(CurrentNumber))
    Lower := 10 ** Power
    Loop, % NumberLimit - Lower {
        if !Mod(CurrentNumber,Lower) {
            FactorTwo := CurrentNumber//Lower
            if ( !Mod(Lower,10) && !Mod(FactorTwo,10) )
                Return ""
            Check := CharSorter( Lower . FactorTwo )
            if (CharSorter(CurrentNumber) = Check) && (StrLen(Lower) = StrLen(FactorTwo))
                Output .= "`n`t[" Lower "," FactorTwo "]"
        }
        Lower++
    }
    Return SubStr(Output,3) ; 3 = 1 + length of "`n`t"
}
```

{{out}}

```txt
1260:	[21,60]
1395:	[15,93]
1435:	[35,41]
1530:	[30,51]
1827:	[21,87]
2187:	[27,81]
6880:	[80,86]
102510:	[201,510]
104260:	[260,401]
105210:	[210,501]
105264:	[204,516]
105750:	[150,705]
108135:	[135,801]
110758:	[158,701]
115672:	[152,761]
116725:	[161,725]
117067:	[167,701]
118440:	[141,840]
123354:	[231,534]
124483:	[281,443]
125248:	[152,824]
125433:	[231,543]
125460:	[204,615]
	[246,510]
125500:	[251,500]
126027:	[201,627]
------------------------------------
16758243290880:
	[1982736,8452080]
	[2123856,7890480]
	[2751840,6089832]
	[2817360,5948208]

24959017348650:
	[2947050,8469153]
	[2949705,8461530]
	[4125870,6049395]
	[4129587,6043950]
	[4230765,5899410]

14593825548650:
```


## Bracmat


```Bracmat
( ( vampire
  =   N len R fangsList
    .   !arg:@(?N:? [?len)
      & 1/2*!len:~/:?len
      & ( R
        =     len numpart left right allowed fangs rdigits
            , tried digit untried head tail found
          .   !arg:(?len.?left.?numpart.?allowed)
            & :?found
            & (   !len:>0
                & ( @( !numpart
                     :   ?tried
                         ( #%@?digit
                         & !allowed:?head !digit ?tail
                         & !head !tail:?allowed
                         )
                         ( ?untried
                         &     R
                             $ ( !len+-1
                               . 10*!left+!digit
                               . str$(!tried !untried)
                               . 0 1 2 3 4 5 6 7 8 9
                               )
                           : ?fangs
                         & !found !fangs:?found
                         & ~
                         )
                     )
                  | !found
                  )
              |   !N*!left^-1:~/?right:~<!left:?rdigits
                & (!left*1/10:/|!right*1/10:/)
                & ( @( !numpart
                     :   ?
                         ( #%@?digit ?
                         & @(!rdigits:?head !digit ?tail)
                         & str$(!head !tail):?rdigits
                         & ~
                         )
                     )
                  | !rdigits:&(!left,!right)
                  )
              )
        )
      &   R$(!len.0.!N.1 2 3 4 5 6 7 8 9)
        : ( 
          |   ?fangsList
            & out$(!N !fangsList)
            & 1+!count:?count
          )
  )
& 0:?count
& 10:?i
& 16758243290880 24959017348650 14593825548650:?bignums
&   whl
  ' ( ( vampire$!i&1+!i:?i
      | !i*10:?i
      )
    & (!count:<25|!bignums:%?i ?bignums)
    )
);
```

Output:

```txt
1260 (21,60)
1395 (15,93)
1435 (35,41)
1530 (30,51)
1827 (21,87)
2187 (27,81)
6880 (80,86)
102510 (201,510)
104260 (260,401)
105210 (210,501)
105264 (204,516)
105750 (150,705)
108135 (135,801)
110758 (158,701)
115672 (152,761)
116725 (161,725)
117067 (167,701)
118440 (141,840)
120600 (201,600)
123354 (231,534)
124483 (281,443)
125248 (152,824)
125433 (231,543)
125460 (246,510) (204,615)
125500 (251,500)
  16758243290880
  (1982736,8452080)
  (2123856,7890480)
  (2751840,6089832)
  (2817360,5948208)
  24959017348650
  (2949705,8461530)
  (2947050,8469153)
  (4230765,5899410)
  (4129587,6043950)
  (4125870,6049395)
```



## C


```c>#include <stdio.h

#include <stdlib.h>
#include <stdint.h>
#include <math.h>

typedef uint64_t xint;
typedef unsigned long long ull;

xint tens[20];

inline xint max(xint a, xint b) { return a > b ? a : b; }
inline xint min(xint a, xint b) { return a < b ? a : b; }
inline int ndigits(xint x)
{
	int n = 0;
	while (x) n++, x /= 10;
	return n;
}

inline xint dtally(xint x)
{
	xint t = 0;
	while (x) t += 1<<((x%10) * 6), x /= 10;

	return t;
}

int fangs(xint x, xint *f)
{
	int n = 0;
	int nd = ndigits(x);
	if (nd & 1) return 0;
	nd /= 2;

	xint lo, hi;
	lo = max(tens[nd-1], (x + tens[nd] - 2)/ (tens[nd] - 1));
	hi = min(x / lo, sqrt(x));

	xint a, b, t = dtally(x);
	for (a = lo; a <= hi; a++) {
		b = x / a;
		if (a * b == x && ((a%10) || (b%10)) && t == dtally(a) + dtally(b))
			f[n++] = a;
	}

	return n;
}

void show_fangs(xint x, xint *f, xint cnt)
{
	printf("%llu", (ull)x);
	int i;
	for (i = 0; i < cnt; i++)
		printf(" = %llu x %llu", (ull)f[i], (ull)(x / f[i]));
	putchar('\n');
}

int main(void)
{
	int i, j, n;
	xint x, f[16], bigs[] = {16758243290880ULL, 24959017348650ULL, 14593825548650ULL, 0};

	tens[0] = 1;
	for (i = 1; i < 20; i++)
		tens[i] = tens[i-1] * 10;

	for (x = 1, n = 0; n < 25; x++) {
		if (!(j = fangs(x, f))) continue;
		printf("%2d: ", ++n);
		show_fangs(x, f, j);
	}

	putchar('\n');
	for (i = 0; bigs[i]; i++) {
		if ((j = fangs(bigs[i], f)))
			show_fangs(bigs[i], f, j);
		else
			printf("%llu is not vampiric\n", (ull)bigs[i]);
	}

	return 0;
}
```


```txt
 1: 1260 = 21 x 60
 2: 1395 = 15 x 93
 3: 1435 = 35 x 41
 4: 1530 = 30 x 51
 5: 1827 = 21 x 87
 6: 2187 = 27 x 81
 7: 6880 = 80 x 86
 8: 102510 = 201 x 510
 9: 104260 = 260 x 401
10: 105210 = 210 x 501
11: 105264 = 204 x 516
12: 105750 = 150 x 705
13: 108135 = 135 x 801
14: 110758 = 158 x 701
15: 115672 = 152 x 761
16: 116725 = 161 x 725
17: 117067 = 167 x 701
18: 118440 = 141 x 840
19: 120600 = 201 x 600
20: 123354 = 231 x 534
21: 124483 = 281 x 443
22: 125248 = 152 x 824
23: 125433 = 231 x 543
24: 125460 = 204 x 615 = 246 x 510
25: 125500 = 251 x 500

16758243290880 = 1982736 x 8452080 = 2123856 x 7890480 = 2751840 x 6089832 = 2817360 x 5948208
24959017348650 = 2947050 x 8469153 = 2949705 x 8461530 = 4125870 x 6049395 = 4129587 x 6043950 = 4230765 x 5899410
14593825548650 is not vampiric
```


=={{header|C sharp|C#}}==
{{trans|C}}

```csharp
using System;

namespace RosettaVampireNumber
{
    class Program
    {
        static void Main(string[] args)
        {
            int i, j, n;
            ulong x;
            var f = new ulong[16];
            var bigs = new ulong[] { 16758243290880UL, 24959017348650UL, 14593825548650UL, 0 };
            ulong[] tens = new ulong[20];
            tens[0] = 1;
            for (i = 1; i < 20; i++)
                tens[i] = tens[i - 1] * 10;
            
            for (x = 1, n = 0; n < 25; x++)
            {
                if ((j = fangs(x, f, tens)) == 0) continue;
                Console.Write(++n + ": ");
                show_fangs(x, f, j);
            }

            Console.WriteLine();
            for (i = 0; bigs[i] > 0 ; i++)
            {
                if ((j = fangs(bigs[i], f, tens)) > 0)
                    show_fangs(bigs[i], f, j);
                else
                    Console.WriteLine(bigs[i] + " is not vampiric.");
            }
            Console.ReadLine();
        }

        private static void show_fangs(ulong x, ulong[] f, int cnt)
        {
            Console.Write(x); 
            int i;
            for (i = 0; i < cnt; i++)
                Console.Write(" = " + f[i] + " * " + (x / f[i]));
            Console.WriteLine();
        }

        private static int fangs(ulong x, ulong[] f, ulong[] tens)
        {
            int n = 0;
            int nd = ndigits(x);
            if ((nd & 1) > 0) return 0;
            nd /= 2;

            ulong lo, hi;
            lo = Math.Max(tens[nd - 1], (x + tens[nd] - 2) / (tens[nd] - 1));
            hi = Math.Min(x / lo, (ulong) Math.Sqrt(x));

            ulong a, b, t = dtally(x);
            for (a = lo; a <= hi; a++)
            {
                b = x / a;
                if (a * b == x && ((a % 10) > 0 || (b % 10) > 0) && t == dtally(a) + dtally(b))
                    f[n++] = a;
            }

            return n;
        }

        private static ulong dtally(ulong x)
        {
            ulong t = 0;
            while (x > 0)
            {
                t += 1UL << (int)((x % 10) * 6);
                x /= 10;
            }

            return t;
        }

        private static int ndigits(ulong x)
        {
            int n = 0;
            while (x > 0)
            {
                n++;
                x /= 10;
            }
            return n;
        }
    }
}
```

{{out}}

```txt

1: 1260 = 21 * 60
2: 1395 = 15 * 93
3: 1435 = 35 * 41
4: 1530 = 30 * 51
5: 1827 = 21 * 87
6: 2187 = 27 * 81
7: 6880 = 80 * 86
8: 102510 = 201 * 510
9: 104260 = 260 * 401
10: 105210 = 210 * 501
11: 105264 = 204 * 516
12: 105750 = 150 * 705
13: 108135 = 135 * 801
14: 110758 = 158 * 701
15: 115672 = 152 * 761
16: 116725 = 161 * 725
17: 117067 = 167 * 701
18: 118440 = 141 * 840
19: 120600 = 201 * 600
20: 123354 = 231 * 534
21: 124483 = 281 * 443
22: 125248 = 152 * 824
23: 125433 = 231 * 543
24: 125460 = 204 * 615 = 246 * 510
25: 125500 = 251 * 500

16758243290880 = 1982736 * 8452080 = 2123856 * 7890480 = 2751840 * 6089832 = 2817360 * 5948208
24959017348650 = 2947050 * 8469153 = 2949705 * 8461530 = 4125870 * 6049395 = 4129587 * 6043950 = 4230765 * 5899410
14593825548650 is not vampiric.

```



## C++


```cpp>#include <vector

#include <utility>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>
#include <cmath>
 
bool isVampireNumber( long number, std::vector<std::pair<long, long> > & solution ) {
   std::ostringstream numberstream ;
   numberstream << number ;
   std::string numberstring( numberstream.str( ) ) ;
   std::sort ( numberstring.begin( ) , numberstring.end( ) ) ;
   int fanglength = numberstring.length( ) / 2 ;
   long start = static_cast<long>( std::pow( 10 , fanglength - 1 ) ) ;
   long end = sqrt(number) ;
   for ( long i = start ; i <= end ; i++ ) { 
      if ( number % i == 0 ) {
	 long quotient = number / i ;
	 if ( ( i % 10 == 0 ) && ( quotient % 10 == 0 ) ) 
	    continue ;
	 numberstream.str( "" ) ; //clear the number stream
	 numberstream << i << quotient ;
	 std::string divisorstring ( numberstream.str( ) ) ;
         std::sort ( divisorstring.begin( ) , divisorstring.end( ) ) ;
	 if ( divisorstring == numberstring ) {
	    std::pair<long , long> divisors = std::make_pair( i, quotient ) ;
	    solution.push_back( divisors ) ;
	 }
      }
   }
   return !solution.empty( ) ;
}
 
void printOut( const std::pair<long, long> & solution ) {
   std::cout << "[ " << solution.first << " , " << solution.second << " ]" ;
}
 
int main( ) {
   int vampireNumbersFound = 0 ;
   std::vector<std::pair<long , long> > solutions ;
   double i = 1.0 ;
   while ( vampireNumbersFound < 25 ) {
      long start = static_cast<long>( std::pow( 10 , i ) ) ;
      long end = start * 10 ;
      for ( long num = start ; num < end ; num++ ) {
	 if ( isVampireNumber( num , solutions ) ) {
            vampireNumbersFound++ ;
	    std::cout << vampireNumbersFound << " :" << num << " is a vampire number! These are the fangs:\n" ;
	    std::for_each( solutions.begin( ) , solutions.end( ) , printOut ) ;
	    std::cout << "\n_______________" << std::endl ;
	    solutions.clear( ) ;	    
	    if ( vampireNumbersFound == 25 ) 
	       break ;
	 }
      }
      i += 2.0 ;
   }
   std::vector<long> testnumbers ;
   testnumbers.push_back( 16758243290880 ) ;
   testnumbers.push_back( 24959017348650 ) ;
   testnumbers.push_back( 14593825548650 ) ;
   for ( std::vector<long>::const_iterator svl = testnumbers.begin( ) ; 
	 svl != testnumbers.end( ) ; svl++ ) {
      if ( isVampireNumber( *svl , solutions ) ) {
	 std::cout << *svl << " is a vampire number! The fangs:\n" ;
	 std::for_each( solutions.begin( ) , solutions.end( ) , printOut ) ;
	 std::cout << std::endl ;
	 solutions.clear( ) ;
      } else {
	 std::cout << *svl << " is not a vampire number!" << std::endl ;
      }
   }
   return 0 ;
}
```

{{out}}

```txt

1 :1260 is a vampire number! These are the fangs:
[ 21 , 60 ]
_______________
2 :1395 is a vampire number! These are the fangs:
[ 15 , 93 ]
_______________
3 :1435 is a vampire number! These are the fangs:
[ 35 , 41 ]
_______________
4 :1530 is a vampire number! These are the fangs:
[ 30 , 51 ]
_______________
5 :1827 is a vampire number! These are the fangs:
[ 21 , 87 ]
_______________
6 :2187 is a vampire number! These are the fangs:
[ 27 , 81 ]
_______________
7 :6880 is a vampire number! These are the fangs:
[ 80 , 86 ]
_______________
8 :102510 is a vampire number! These are the fangs:
[ 201 , 510 ]
_______________
9 :104260 is a vampire number! These are the fangs:
[ 260 , 401 ]
_______________
10 :105210 is a vampire number! These are the fangs:
[ 210 , 501 ]
_______________
11 :105264 is a vampire number! These are the fangs:
[ 204 , 516 ]
_______________
12 :105750 is a vampire number! These are the fangs:
[ 150 , 705 ]
_______________
13 :108135 is a vampire number! These are the fangs:
[ 135 , 801 ]
_______________
14 :110758 is a vampire number! These are the fangs:
[ 158 , 701 ]
_______________
15 :115672 is a vampire number! These are the fangs:
[ 152 , 761 ]
_______________
16 :116725 is a vampire number! These are the fangs:
[ 161 , 725 ]
_______________
17 :117067 is a vampire number! These are the fangs:
[ 167 , 701 ]
_______________
18 :118440 is a vampire number! These are the fangs:
[ 141 , 840 ]
_______________
19 :120600 is a vampire number! These are the fangs:
[ 201 , 600 ]
_______________
20 :123354 is a vampire number! These are the fangs:
[ 231 , 534 ]
_______________
21 :124483 is a vampire number! These are the fangs:
[ 281 , 443 ]
_______________
22 :125248 is a vampire number! These are the fangs:
[ 152 , 824 ]
_______________
23 :125433 is a vampire number! These are the fangs:
[ 231 , 543 ]
_______________
24 :125460 is a vampire number! These are the fangs:
[ 204 , 615 ][ 246 , 510 ]
_______________
25 :125500 is a vampire number! These are the fangs:
[ 251 , 500 ]
_______________
16758243290880 is a vampire number! The fangs:
[ 1982736 , 8452080 ][ 2123856 , 7890480 ][ 2751840 , 6089832 ][ 2817360 , 5948208 ]
24959017348650 is a vampire number! The fangs:
[ 2947050 , 8469153 ][ 2949705 , 8461530 ][ 4125870 , 6049395 ][ 4129587 , 6043950 ][ 4230765 , 5899410 ]
14593825548650 is not a vampire number!

```



## Clojure


```Clojure
(defn factor-pairs [n]
  (for [x (range 2 (Math/sqrt n))
        :when (zero? (mod n x))]
    [x (quot n x)]))

(defn fangs [n]
  (let [dlen (comp count str)
        half (/ (dlen n) 2)
        halves? #(apply = (cons half (map dlen %)))
        digits  #(sort (apply str %))]
    (filter #(and (halves? %)
                  (= (sort (str n)) (digits %)))
            (factor-pairs n))))

(defn vampiric? [n]
  (let [fangs (fangs n)]
    (if (empty? fangs) nil [n fangs])))

(doseq [n (take 25 (keep vampiric? (range)))]
  (prn n))

(doseq [n [16758243290880, 24959017348650, 14593825548650]]
  (println (or (vampiric? n) (str n " is not vampiric."))))
```



## Common Lisp



```lisp
(defun trailing-zerop (number)
  "Is the lowest digit of `number' a 0"
  (zerop (rem number 10)))

(defun integer-digits (integer)
  "Return the number of digits of the `integer'"
  (assert (integerp integer))
  (length (write-to-string integer)))

(defun paired-factors (number)
  "Return a list of pairs that are factors of `number'"
  (loop
    :for candidate :from 2 :upto (sqrt number)
    :when (zerop (mod number candidate))
      :collect (list candidate (/ number candidate))))

(defun vampirep (candidate &aux
                             (digits-of-candidate (integer-digits candidate))
                             (half-the-digits-of-candidate (/ digits-of-candidate
                                                              2)))
  "Is the `candidate' a vampire number?"
  (remove-if #'(lambda (pair)
                 (> (length (remove-if #'null (mapcar #'trailing-zerop pair)))
                     1)) 
             (remove-if-not #'(lambda (pair)
                                (string= (sort (copy-seq (write-to-string candidate))
                                               #'char<)
                                         (sort (copy-seq (format nil "~A~A" (first pair) (second pair)))
                                               #'char<)))
                            (remove-if-not #'(lambda (pair)
                                               (and (eql (integer-digits (first pair))
                                                         half-the-digits-of-candidate)
                                                    (eql (integer-digits (second pair))
                                                         half-the-digits-of-candidate)))
                                           (paired-factors candidate)))))

(defun print-vampire (candidate fangs &optional (stream t))
  (format stream
          "The number ~A is a vampire number with fangs: ~{ ~{~A~^, ~}~^; ~}~%"
          candidate
          fangs))

;; Print the first 25 vampire numbers

(loop
  :with count := 0
  :for candidate :from 0
  :until (eql count 25)
  :for fangs := (vampirep candidate)
  :do
     (when fangs
       (print-vampire candidate fangs)
       (incf count)))

;; Check if 16758243290880, 24959017348650, 14593825548650 are vampire numbers

(dolist (candidate '(16758243290880 24959017348650 14593825548650))
  (let ((fangs (vampirep candidate)))
    (when fangs
      (print-vampire candidate fangs))))

```


{{out}}

```txt

The number 1260 is a vampire number with fangs:  21, 60
The number 1395 is a vampire number with fangs:  15, 93
The number 1435 is a vampire number with fangs:  35, 41
The number 1530 is a vampire number with fangs:  30, 51
The number 1827 is a vampire number with fangs:  21, 87
The number 2187 is a vampire number with fangs:  27, 81
The number 6880 is a vampire number with fangs:  80, 86
The number 102510 is a vampire number with fangs:  201, 510
The number 104260 is a vampire number with fangs:  260, 401
The number 105210 is a vampire number with fangs:  210, 501
The number 105264 is a vampire number with fangs:  204, 516
The number 105750 is a vampire number with fangs:  150, 705
The number 108135 is a vampire number with fangs:  135, 801
The number 110758 is a vampire number with fangs:  158, 701
The number 115672 is a vampire number with fangs:  152, 761
The number 116725 is a vampire number with fangs:  161, 725
The number 117067 is a vampire number with fangs:  167, 701
The number 118440 is a vampire number with fangs:  141, 840
The number 120600 is a vampire number with fangs:  201, 600
The number 123354 is a vampire number with fangs:  231, 534
The number 124483 is a vampire number with fangs:  281, 443
The number 125248 is a vampire number with fangs:  152, 824
The number 125433 is a vampire number with fangs:  231, 543
The number 125460 is a vampire number with fangs:  204, 615;  246, 510
The number 125500 is a vampire number with fangs:  251, 500

The number 16758243290880 is a vampire number with fangs:  1982736, 8452080;  2123856, 7890480;  2751840, 6089832;  2817360, 5948208
The number 24959017348650 is a vampire number with fangs:  2947050, 8469153;  2949705, 8461530;  4125870, 6049395;  4129587, 6043950;  4230765, 5899410
The number 16758243290880 is a vampire number with fangs:  1982736, 8452080;  2123856, 7890480;  2751840, 6089832;  2817360, 5948208
The number 24959017348650 is a vampire number with fangs:  2947050, 8469153;  2949705, 8461530;  4125870, 6049395;  4129587, 6043950;  4230765, 5899410

```



## D

The two versions show two styles of D code: compact script-like mostly-functional code, and efficient lower-level code.
===High-Level Version===
{{trans|Clojure}}
(Runtime about 1.34 seconds with dmd.) 

```d
import std.stdio, std.range, std.algorithm, std.typecons, std.conv;

auto fangs(in long n) pure nothrow @safe {
    auto pairs = iota(2, cast(int)(n ^^ 0.5)) // n.isqrt
                 .filter!(x => !(n % x)).map!(x => [x, n / x]);
    enum dLen = (in long x) => x.text.length;
    immutable half = dLen(n) / 2;
    enum halvesQ = (long[] p) => p.all!(u => dLen(u) == half);
    enum digits = (long[] p) => dtext(p[0], p[1]).dup.sort();
    const dn = n.to!(dchar[]).sort();
    return tuple(n, pairs.filter!(p => halvesQ(p) && dn == digits(p)));
}

void main() {
    foreach (v; int.max.iota.map!fangs.filter!q{ !a[1].empty }
                .take(25).chain([16758243290880, 24959017348650,
                                 14593825548650].map!fangs))
        writefln("%d: (%(%(%s %)) (%))", v[]);
}
```

{{out}}

```txt
1260: (21 60)
1395: (15 93)
1435: (35 41)
1530: (30 51)
1827: (21 87)
2187: (27 81)
6880: (80 86)
102510: (201 510)
104260: (260 401)
105210: (210 501)
105264: (204 516)
105750: (150 705)
108135: (135 801)
110758: (158 701)
115672: (152 761)
116725: (161 725)
117067: (167 701)
118440: (141 840)
120600: (201 600)
123354: (231 534)
124483: (281 443)
125248: (152 824)
125433: (231 543)
125460: (204 615) (246 510)
125500: (251 500)
16758243290880: (1982736 8452080) (2123856 7890480) (2751840 6089832) (2817360 5948208)
24959017348650: (2947050 8469153) (2949705 8461530) (4125870 6049395) (4129587 6043950) (4230765 5899410)
14593825548650: ()
```



### Fast Version

{{trans|C}}
(Runtime about 0.27 seconds with dmd, about 0.25 seconds with ldc2 compilers.)

```d
import std.stdio, std.math, std.algorithm, std.array, std.traits;

T[N] pows(T, size_t N)() pure nothrow @safe @nogc {
    typeof(return) result;
    result[0] = 1;
    foreach (immutable i, ref r; result[1 .. $])
        r = result[i] * 10;
    return result;
}

__gshared immutable tenPowsU = pows!(uint, 10);
__gshared immutable tenPowsUL = pows!(ulong, 20);

size_t nDigits(T)(in T x) pure nothrow @safe @nogc {
    Unqual!T y = x;
    size_t n = 0;
    while (y) {
        n++;
        y /= 10;
    }
    return n;
}

T dTally(T)(in T x) pure nothrow @safe @nogc {
    Unqual!T y = x;
    T t = 0;
    while (y) {
        t += 1 << ((y % 10) * 6);
        y /= 10;
    }
    return t;
}

T[] fangs(T)(in T x, T[] f)
pure nothrow @safe @nogc if (is(T == uint) || is(T == ulong)) {
    alias tenPows = Select!(is(T == ulong), tenPowsUL, tenPowsU);

    immutable nd0 = nDigits(x);
    if (nd0 & 1)
        return null;
    immutable nd = nd0 / 2;

    immutable lo = max(tenPows[nd - 1],
                       (x + tenPows[nd] - 2) / (tenPows[nd] - 1));
    immutable hi = min(x / lo, cast(T)sqrt(real(x)));
    immutable t = x.dTally;

    size_t n = 0;
    foreach (immutable a; lo .. hi + 1) {
        immutable b = x / a;
        if (a * b == x
            && (a % 10 || b % 10)
            && t == (a.dTally + b.dTally)) {
            f[n] = a;
            n++;
        }
    }

    return f[0 .. n];
}

void showFangs(T)(in T x, in T[] fs) {
    x.write;
    foreach (immutable fi; fs)
        writef(" = %d x %d", fi, x / fi);
    writeln;
}

void main() {
    uint[16] fu;
    for (uint x = 1, n = 0; n < 25; x++) {
        const fs = fangs(x, fu);
        if (fs.empty)
            continue;
        n++;
        writef("%2d: ", n);
        showFangs(x, fs);
    }
    writeln;

    static immutable ulong[3] bigs = [16_758_243_290_880UL,
                                      24_959_017_348_650UL,
                                      14_593_825_548_650UL];
    ulong[fu.length] ful;
    foreach (immutable bi; bigs) {
        const fs = fangs(bi, ful);
        if (fs.empty)
            writeln(bi, " is not vampiric");
        else
            showFangs(bi, fs);
    }
}
```

{{out}}

```txt
 2: 1395 = 15 x 93
 3: 1435 = 35 x 41
 4: 1530 = 30 x 51
 5: 1827 = 21 x 87
 6: 2187 = 27 x 81
 7: 6880 = 80 x 86
 8: 102510 = 201 x 510
 9: 104260 = 260 x 401
10: 105210 = 210 x 501
11: 105264 = 204 x 516
12: 105750 = 150 x 705
13: 108135 = 135 x 801
14: 110758 = 158 x 701
15: 115672 = 152 x 761
16: 116725 = 161 x 725
17: 117067 = 167 x 701
18: 118440 = 141 x 840
19: 120600 = 201 x 600
20: 123354 = 231 x 534
21: 124483 = 281 x 443
22: 125248 = 152 x 824
23: 125433 = 231 x 543
24: 125460 = 204 x 615 = 246 x 510
25: 125500 = 251 x 500

16758243290880 = 1982736 x 8452080 = 2123856 x 7890480 = 2751840 x 6089832 = 2817360 x 5948208
24959017348650 = 2947050 x 8469153 = 2949705 x 8461530 = 4125870 x 6049395 = 4129587 x 6043950 = 4230765 x 5899410
14593825548650 is not vampiric
```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	fang_check (original, fang1, fang2: INTEGER_64): BOOLEAN
			-- Are 'fang1' and 'fang2' correct fangs of the 'original' number?
		require
			original_positive: original > 0
			fangs_positive: fang1 > 0 and fang2 > 0
		local
			original_length: INTEGER
			fang, ori: STRING
			sort_ori, sort_fang: SORTED_TWO_WAY_LIST [CHARACTER]
		do
			create sort_ori.make
			create sort_fang.make
			create ori.make_empty
			create fang.make_empty
			original_length := original.out.count // 2
			if fang1.out.count /= original_length or fang2.out.count /= (original_length) then
				Result := False
			elseif fang1.out.ends_with ("0") and fang2.out.ends_with ("0") then
				Result := False
			else
				across
					1 |..| original.out.count as c
				loop
					sort_ori.extend (original.out [c.item])
				end
				across
					sort_ori as o
				loop
					ori.extend (o.item)
				end
				across
					1 |..| fang1.out.count as c
				loop
					sort_fang.extend (fang1.out [c.item])
					sort_fang.extend (fang2.out [c.item])
				end
				across
					sort_fang as f
				loop
					fang.extend (f.item)
				end
				Result := fang.same_string (ori)
			end
		ensure
			fangs_right_length: Result implies original.out.count = fang1.out.count + fang2.out.count
		end

	make
                -- Uses fang_check to find vampire nubmers.
		local
			i, numbers: INTEGER
			fang1, fang2: INTEGER_64
			num: ARRAY [INTEGER_64]
			math: DOUBLE_MATH
		do
			create math
			from
				i := 1000
			until
				numbers > 25
			loop
				if i.out.count \\ 2 = 0 then
					from
						fang1 := 10
					until
						fang1 >= math.sqrt (i)
					loop
						if (i \\ fang1 = 0) then
							fang2 := i // fang1
							if i \\ 9 = (fang1 + fang2) \\ 9 then
								if fang1 * fang2 = i and fang1 <= fang2 and then fang_check (i, fang1, fang2) then
									numbers := numbers + 1
									io.put_string (i.out + ": " + fang1.out + " " + fang2.out)
									io.new_line
								end
							end
						end
						fang1 := fang1 + 1
					end
				end
				i := i + 1
			end
			num := <<16758243290880, 24959017348650, 14593825548650>>
			across
				num as n
			loop
				from
					fang1 := 1000000
				until
					fang1 >= math.sqrt (n.item) + 1
				loop
					if (n.item \\ fang1 = 0) then
						fang2 := (n.item // fang1)
						if fang1 * fang2 = n.item and fang1 <= fang2 and then fang_check (n.item, fang1, fang2) then
							io.put_string (n.item.out + ": " + fang1.out + " " + fang2.out + "%N")
						end
					end
					fang1 := fang1 + 1
				end
			end
		end

end

```

{{out}}

```txt

1260: 21, 60
1395: 15, 93
1435: 35, 41
1530: 30, 51
1827: 21, 87
2187: 27, 81
6880: 80, 86
102510: 201, 510
104260: 260, 401
105210: 210, 501
105264: 204, 516
105750: 150, 705
108135: 135, 801
110758: 158, 701
115672: 152, 761
116725: 161, 725
117067: 167, 701
118440: 141, 840
120600: 201, 600
123354: 231, 534
124483: 281, 443
125248: 152, 824
125433: 231, 543
125460: 204, 615
125460: 246, 510
125500: 251, 500
126027: 201, 627
16758243290880: 1982736, 8452080
16758243290880: 2123856, 7890480
16758243290880: 2751840, 6089832
16758243290880: 2817360, 5948208
24959017348650: 2947050, 8469153
24959017348650: 2949705, 8461530
24959017348650: 4125870, 6049395
24959017348650: 4129587, 6043950
24959017348650: 4230765, 5899410

```



## Elixir

{{works with|Elixir|1.1+}}

```elixir
defmodule Vampire do
  def factor_pairs(n) do
    first = trunc(n / :math.pow(10, div(char_len(n), 2)))
    last  = :math.sqrt(n) |> round
    for i <- first .. last, rem(n, i) == 0, do: {i, div(n, i)}
  end
  
  def vampire_factors(n) do
    if rem(char_len(n), 2) == 1 do
      []
    else
      half = div(length(to_char_list(n)), 2)
      sorted = Enum.sort(String.codepoints("#{n}"))
      Enum.filter(factor_pairs(n), fn {a, b} ->
        char_len(a) == half && char_len(b) == half &&
        Enum.count([a, b], fn x -> rem(x, 10) == 0 end) != 2 &&
        Enum.sort(String.codepoints("#{a}#{b}")) == sorted
      end)
    end
  end
  
  defp char_len(n), do: length(to_char_list(n))
  
  def task do
    Enum.reduce_while(Stream.iterate(1, &(&1+1)), 1, fn n, acc ->
      case vampire_factors(n) do
        [] -> {:cont, acc}
        vf -> IO.puts "#{n}:\t#{inspect vf}"
              if acc < 25, do: {:cont, acc+1}, else: {:halt, acc+1}
      end
    end)
    IO.puts ""
    Enum.each([16758243290880, 24959017348650, 14593825548650], fn n ->
      case vampire_factors(n) do
        [] -> IO.puts "#{n} is not a vampire number!"
        vf -> IO.puts "#{n}:\t#{inspect vf}"
      end
    end)
  end
end

Vampire.task
```


{{out}}

```txt

1260:	[{21, 60}]
1395:	[{15, 93}]
1435:	[{35, 41}]
1530:	[{30, 51}]
1827:	[{21, 87}]
2187:	[{27, 81}]
6880:	[{80, 86}]
102510:	[{201, 510}]
104260:	[{260, 401}]
105210:	[{210, 501}]
105264:	[{204, 516}]
105750:	[{150, 705}]
108135:	[{135, 801}]
110758:	[{158, 701}]
115672:	[{152, 761}]
116725:	[{161, 725}]
117067:	[{167, 701}]
118440:	[{141, 840}]
120600:	[{201, 600}]
123354:	[{231, 534}]
124483:	[{281, 443}]
125248:	[{152, 824}]
125433:	[{231, 543}]
125460:	[{204, 615}, {246, 510}]
125500:	[{251, 500}]

16758243290880:	[{1982736, 8452080}, {2123856, 7890480}, {2751840, 6089832}, {2817360, 5948208}]
24959017348650:	[{2947050, 8469153}, {2949705, 8461530}, {4125870, 6049395}, {4129587, 6043950}, {4230765, 5899410}]
14593825548650 is not a vampire number!

```



## Factor


```factor
USING: combinators.short-circuit fry io kernel lists lists.lazy math
math.combinatorics math.functions math.primes.factors math.statistics
math.text.utils prettyprint sequences sets ;
IN: rosetta-code.vampire-number

: digits ( n -- m )
    log10 floor >integer 1 + ;

: same-digits? ( n n1 n2 -- ? )
    [ 1 digit-groups ] tri@ append [ histogram ] bi@ = ;    

: half-len-factors ( n -- seq )
    [ divisors ] [ digits ] bi 2/ '[ digits _ = ] filter ;
    
: same-digit-factors ( n -- seq )
    dup half-len-factors 2 <combinations> [ first2 same-digits? ] with filter ;
    
: under-two-trailing-zeros? ( seq -- ? )
    [ 10 mod ] map [ 0 = ] count 2 < ;
    
: tentative-fangs ( n -- seq )
    same-digit-factors [ under-two-trailing-zeros? ] filter ;
    
: fangs ( n -- seq )
    [ tentative-fangs ] [ ] bi '[ product _ = ] filter ;
    
: vampire? ( n -- ? )
    { [ digits even? ] [ fangs empty? not ] } 1&& ;
    
: first25 ( -- seq )
    25 0 lfrom [ vampire? ] lfilter ltake list>array ;
    
: .vamp-with-fangs ( n -- )
    [ pprint bl ] [ fangs [ pprint bl ] each ] bi nl ;
    
: part1 ( -- )
    first25 [ .vamp-with-fangs ] each ;
    
: part2 ( -- ) { 16758243290880 24959017348650 14593825548650 }
    [ dup vampire? [ .vamp-with-fangs ] [ drop ] if ] each ;
    
: main ( -- ) part1 part2 ;

MAIN: main
```

{{out}}

```txt

1260 { 21 60 }
1395 { 15 93 }
1435 { 35 41 }
1530 { 30 51 }
1827 { 21 87 }
2187 { 27 81 }
6880 { 80 86 }
102510 { 201 510 }
104260 { 260 401 }
105210 { 210 501 }
105264 { 204 516 }
105750 { 150 705 }
108135 { 135 801 }
110758 { 158 701 }
115672 { 152 761 }
116725 { 161 725 }
117067 { 167 701 }
118440 { 141 840 }
120600 { 201 600 }
123354 { 231 534 }
124483 { 281 443 }
125248 { 152 824 }
125433 { 231 543 }
125460 { 204 615 } { 246 510 }
125500 { 251 500 }
16758243290880 { 1982736 8452080 } { 2123856 7890480 } { 2751840 6089832 } { 2817360 5948208 }
24959017348650 { 2947050 8469153 } { 2949705 8461530 } { 4125870 6049395 } { 4129587 6043950 } { 4230765 5899410 }

```



## FreeBASIC


```FreeBASIC
'Vampire numbers.
'FreeBASIC version 24. Windows
'Vampire.bas
Function WithinString(n As Ulongint,f As Ulongint) As Integer
    var m=Str(n),p=Str(f)
    For z As Integer=0 To Len(p)-1
        var i=Instr(m,Chr(p[z]))
        If i Then
            m=Mid(m,1,i-1)+Mid(m,i+1)
        Else
            Return 0
        End If
    Next z
    Return -1
End Function

Sub AllFactors(N As Ulongint,factors() As Ulongint) 
    Dim As String Sn=Str(n)
    Dim As Integer half=Len(sn)\2
    Redim factors(1 To 1)
    #macro bsort(array)
    For p1 As Integer  = 1 To Ubound(array) - 1
        For p2 As Integer  = p1 + 1 To Ubound(array)  
            If array(p1)>array(p2) Then Swap array(p1),array(p2)
        Next p2
    Next p1
    #endmacro
    
    Dim As Ulongint c
    For i As Ulongint = 1 To Sqr(N)       
        If N Mod i=0 Then
            If Len(Str(i))=half Then
                If WithinString(N,i) Then
                    c=c+1
                    Redim Preserve factors(1 To c)
                    factors(c)=i
                End If
            End If
            If N <> i*i Then 
                If Len(Str(n\i))=half Then
                    If WithinString(N,n\i) Then
                        c=c+1
                        Redim Preserve factors(1 To c)
                        factors(c)=n\i
                    End If
                End If
            End If 
        End If 
    Next i
    bsort(factors)
End Sub

Function VampireNumbers(N As Ulongint) As Integer
    Dim As Integer flag
    Dim As Ulongint LastFactor
    Redim As Ulongint Factor()
    AllFactors(N,Factor())
    For p1 As Integer = 1 To Ubound(Factor)
        For p2 As Integer=1 To Ubound(Factor)
            If Factor(p1)*Factor(p2)=n Then
                If Factor(p1) Mod 10<>0 Or Factor(p2) Mod 10 <>0 Then 
                    If WithinString(n,valulng(Str(Factor(p1))+Str(Factor(p2)))) Then
                        If LastFactor=Factor(p2) Then Exit For,For
                        flag=1
                        Print n;": [";Factor(p1);",";Factor(p2);"]"
                        LastFactor=Factor(p1)
                    End If
                End If
            End If
        Next p2
    Next p1
    If flag Then Return -1
End Function

'
### =========== IMPLEMENT ===========================

print "First 28 Vampire numbers"
print
Print "Number: [fangs]"
Print
Dim As Ulongint n=1000
Dim As Integer count
Dim As Double t1,t2
t1=Timer
Do
    n=n+1
    Var s=Str(n)
    If Len(s) Mod 2<>0 Then n=n*10
    If vampireNumbers(n) Then count=count+1
Loop Until count=27 
Print
print "Individual tests:"
print
'individual tests
n=16758243290880ull
If Not vampirenumbers(n) Then Print n;": [returns no fangs]"
Print
n=24959017348650ull
If Not vampirenumbers(n) Then Print n;": [returns no fangs]"
print
n=14593825548650ull
If Not vampirenumbers(n) then print n;": [returns no fangs]"
t2=Timer
print
Print "Completed in ";
Print t2-t1;" Seconds"
Sleep
```

{{out}}

```txt
First 28 Vampire numbers

Number: [fangs]

1260: [21,60]
1395: [15,93]
1435: [35,41]
1530: [30,51]
1827: [21,87]
2187: [27,81]
6880: [80,86]
102510: [201,510]
104260: [260,401]
105210: [210,501]
105264: [204,516]
105750: [150,705]
108135: [135,801]
110758: [158,701]
115672: [152,761]
116725: [161,725]
117067: [167,701]
118440: [141,840]
120600: [201,600]
123354: [231,534]
124483: [281,443]
125248: [152,824]
125433: [231,543]
125460: [204,615]
125460: [246,510]
125500: [251,500]
126027: [201,627]
126846: [261,486]

Individual tests:

16758243290880: [1982736,8452080]
16758243290880: [2123856,7890480]
16758243290880: [2751840,6089832]
16758243290880: [2817360,5948208]

24959017348650: [2947050,8469153]
24959017348650: [2949705,8461530]
24959017348650: [4125870,6049395]
24959017348650: [4129587,6043950]
24959017348650: [4230765,5899410]

14593825548650: [returns no fangs]

Completed in  1.286374813709699 Seconds
```


## Go

{{trans|C}}

```go
package main

import (
    "fmt" 
    "math" 
)

func max(a, b uint64) uint64 {
    if a > b {
        return a
    }
    return b
}

func min(a, b uint64) uint64 {
    if a < b {
        return a
    }
    return b 
}
    
func ndigits(x uint64) (n int) {
    for ; x > 0; x /= 10 {
        n++ 
    }
    return 
}

func dtally(x uint64) (t uint64) {
    for ; x > 0; x /= 10 {
        t += 1 << (x % 10 * 6)
    }
    return 
}

var tens [20]uint64

func init() {
    tens[0] = 1
    for i := 1; i < 20; i++ {
        tens[i] = tens[i-1] * 10
    }
}

func fangs(x uint64) (f []uint64) {
    nd := ndigits(x)
    if nd&1 == 1 {
        return
    }
    nd /= 2
    lo := max(tens[nd-1], (x+tens[nd]-2)/(tens[nd]-1))
    hi := min(x/lo, uint64(math.Sqrt(float64(x))))
    t := dtally(x)
    for a := lo; a <= hi; a++ {
        b := x / a
        if a*b == x &&
            (a%10 > 0 || b%10 > 0) &&
            t == dtally(a)+dtally(b) {
            f = append(f, a)
        }
    }
    return
}

func showFangs(x uint64, f []uint64) {
    fmt.Print(x)
    if len(f) > 1 {
        fmt.Println()
    }
    for _, a := range f {
        fmt.Println(" =", a, "×", x/a)
    }
}

func main() {
    for x, n := uint64(1), 0; n < 26; x++ {
        if f := fangs(x); len(f) > 0 {
            n++
            fmt.Printf("%2d: ", n)
            showFangs(x, f)
        }
    }
    fmt.Println()
    for _, x := range []uint64{16758243290880, 24959017348650, 14593825548650} {
        if f := fangs(x); len(f) > 0 {
            showFangs(x, f)
        } else {
            fmt.Println(x, "is not vampiric")
        }
    }
}
```

{{out}}

```txt

 1: 1260 = 21 × 60
 2: 1395 = 15 × 93
 3: 1435 = 35 × 41
 4: 1530 = 30 × 51
 5: 1827 = 21 × 87
 6: 2187 = 27 × 81
 7: 6880 = 80 × 86
 8: 102510 = 201 × 510
 9: 104260 = 260 × 401
10: 105210 = 210 × 501
11: 105264 = 204 × 516
12: 105750 = 150 × 705
13: 108135 = 135 × 801
14: 110758 = 158 × 701
15: 115672 = 152 × 761
16: 116725 = 161 × 725
17: 117067 = 167 × 701
18: 118440 = 141 × 840
19: 120600 = 201 × 600
20: 123354 = 231 × 534
21: 124483 = 281 × 443
22: 125248 = 152 × 824
23: 125433 = 231 × 543
24: 125460
 = 204 × 615
 = 246 × 510
25: 125500 = 251 × 500
26: 126027 = 201 × 627

16758243290880
 = 1982736 × 8452080
 = 2123856 × 7890480
 = 2751840 × 6089832
 = 2817360 × 5948208
24959017348650
 = 2947050 × 8469153
 = 2949705 × 8461530
 = 4125870 × 6049395
 = 4129587 × 6043950
 = 4230765 × 5899410
14593825548650 is not vampiric

```



## Haskell



```haskell
import Data.List (sort)
import Control.Arrow ((&&&))

-- VAMPIRE NUMBERS ------------------------------------------------------------
vampires :: [Int]
vampires = filter (not . null . fangs) [1 ..]

fangs :: Int -> [(Int, Int)]
fangs n
  | odd w = []
  | otherwise = ((,) <*> quot n) <$> filter isfang (integerFactors n)
  where
    ndigit :: Int -> Int
    ndigit 0 = 0
    ndigit n = 1 + ndigit (quot n 10)
    w = ndigit n
    xmin = 10 ^ (quot w 2 - 1)
    xmax = xmin * 10
    isfang x =
      x > xmin &&
      x < y &&
      y < xmax && -- same length
      (quot x 10 /= 0 || quot y 10 /= 0) && -- not zero-ended
      sort (show n) == sort (show x ++ show y)
      where
        y = quot n x

-- FACTORS --------------------------------------------------------------------
integerFactors :: Int -> [Int]
integerFactors n
  | n < 1 = []
  | otherwise =
    lows ++
    (quot n <$>
     (if intSquared == n -- A perfect square,
        then tail -- and cofactor of square root would be redundant.
        else id)
       (reverse lows))
  where
    (intSquared, lows) =
      (^ 2) &&& (filter ((0 ==) . rem n) . enumFromTo 1) $
      floor (sqrt $ fromIntegral n)

-- TEST -----------------------------------------------------------------------
main :: IO [()]
main =
  mapM
    (print . ((,) <*>) fangs)
    (take 25 vampires ++ [16758243290880, 24959017348650, 14593825548650])
```

{{Out}}

```txt

(1260,[(21,60)])
(1395,[(15,93)])
(1435,[(35,41)])
(1530,[(30,51)])
(1827,[(21,87)])
(2187,[(27,81)])
(6880,[(80,86)])
(102510,[(201,510)])
(104260,[(260,401)])
(105210,[(210,501)])
(105264,[(204,516)])
(105750,[(150,705)])
(108135,[(135,801)])
(110758,[(158,701)])
(115672,[(152,761)])
(116725,[(161,725)])
(117067,[(167,701)])
(118440,[(141,840)])
(120600,[(201,600)])
(123354,[(231,534)])
(124483,[(281,443)])
(125248,[(152,824)])
(125433,[(231,543)])
(125460,[(204,615),(246,510)])
(125500,[(251,500)])
(16758243290880,[(1982736,8452080),(2123856,7890480),(2751840,6089832),(2817360,5948208)])
(24959017348650,[(2947050,8469153),(2949705,8461530),(4125870,6049395),(4129587,6043950),(4230765,5899410)])
(14593825548650,[])
```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages.


```unicon
procedure main()
    write("First 25 vampire numbers and their fangs:")
    every fangs := vampire(n := seq())\25 do write(right(n,20),":",fangs)
    write("\nOther numbers:")
    every n := 16758243290880 | 24959017348650 | 14593825548650 do
        write(right(n,20),": ",vampire(n)|"toothless")
end

procedure vampire(n)
    ns := string(n)
    if *ns % 2 = 1 then fail
    every (fangs := "") ||:= " "||fangCheck(n, *ns/2, f1 := 2 to integer(sqrt(n)), n/f1)
    if *fangs > 0 then return fangs
end

procedure fangCheck(n, n2, f1, f2)
    if f1*f2 ~= n then fail
    if n2 ~= *(f1|f2) then fail
    if (f1|f2) % 10 ~= 0 then
         if csort(f1||f2) == csort(n) then return "("||f1||","||f2||")"
end

procedure csort(s)  # Adapted from csort(s) in Icon IPL
    every (s1 := "", c := !cset(s)) do every find(c, s) do s1 ||:= c
    return s1
end
```


Output:

```txt

First 25 vampire numbers and their fangs:
                1260: (21,60)
                1395: (15,93)
                1435: (35,41)
                1530: (30,51)
                1827: (21,87)
                2187: (27,81)
                6880: (80,86)
              102510: (201,510)
              104260: (260,401)
              105210: (210,501)
              105264: (204,516)
              105750: (150,705)
              108135: (135,801)
              110758: (158,701)
              115672: (152,761)
              116725: (161,725)
              117067: (167,701)
              118440: (141,840)
              120600: (201,600)
              123354: (231,534)
              124483: (281,443)
              125248: (152,824)
              125433: (231,543)
              125460: (204,615) (246,510)
              125500: (251,500)

Other numbers:
      16758243290880: (1982736,8452080) (2123856,7890480) (2751840,6089832) (2817360,5948208)
      24959017348650: (2947050,8469153) (2949705,8461530) (4125870,6049395) (4129587,6043950) (4230765,5899410)
      14593825548650: toothless

```



## J


```J

Filter=: (#~`)(`:6)
odd =: 2&|
even =: -.@:odd
factors =: [: ([: /:~ [: */"1 ([: x: [) ^"1 [: > [: , [: { [: <@:i.@>: ])/ __ q: ]
digits =: 10&(#.inv)
tally =: # : [:
half =: -: : [:
even_number_of_digits =: even@:tally@:digits
same_digits =: digits@:[ -:&(/:~) ,&digits/@:]
assert 1 -: 1234 same_digits 23 14
assert 0 -: 1234 same_digits 23 140
half_the_digits =: (half@:tally@:digits@:[ = tally@:digits&>@:]) # ]
factors_with_half_the_digits =: half_the_digits factors
large =: (> <.@:%:)~ # ]
candidates =: large factors_with_half_the_digits
one_trailing_zero_permitted =: (0 < [: tally 0 -.~ 10&|)"1 Filter
pairs =: (% ,. ]) one_trailing_zero_permitted@:candidates
fangs =: (same_digits"0 1 # ]) pairs

   A=:(0 2 -.@:-: $)&>Filter<@fangs"0]1000+i.1e4
   B=:(0 2 -.@:-: $)&>Filter<@fangs"0]100000+i.25501
   (,: */@:{.&.>)A,B
┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┐
│21 60│15 93│35 41│30 51│21 87│27 81│80 86│201 510│260 401│210 501│204 516│150 705│135 801│158 701│152 761│161 725│167 701│141 840│201 600│231 534│281 443│152 824│231 543│246 510│251 500│
│     │     │     │     │     │     │     │       │       │       │       │       │       │       │       │       │       │       │       │       │       │       │       │204 615│       │
├─────┼─────┼─────┼─────┼─────┼─────┼─────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
│1260 │1395 │1435 │1530 │1827 │2187 │6880 │102510 │104260 │105210 │105264 │105750 │108135 │110758 │115672 │116725 │117067 │118440 │120600 │123354 │124483 │125248 │125433 │125460 │125500 │
└─────┴─────┴─────┴─────┴─────┴─────┴─────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┘

   <@fangs"0[] 16758243290880 24959017348650 14593825548650
┌───────────────┬───────────────┬──┐
│2817360 5948208│4230765 5899410│  │
│2751840 6089832│4129587 6043950│  │
│2123856 7890480│4125870 6049395│  │
│1982736 8452080│2949705 8461530│  │
│               │2947050 8469153│  │
└───────────────┴───────────────┴──┘
   
   fangs f.  NB. <laugh>
((10&(#.^:_1)@:[ -:&(/:~) ,&(10&(#.^:_1))/@:])"0 1 # ]) ((% ,. ]) (#~ (0 < [: # :[: 0 -.~ 10&|)"1)@:(((> <.@:%:)~ # ]) (((-: :[:@:(# :[:)@:(10&(#.^:_1))@:[ = # :[:@:(10&(#.^:_1))&>@:]) # ]) ([: ([: /:~ [: */"1 ([: x: [) ^"1 [: > [: , [: { [: <@:i.@>: ])/ __ q: ]))))

```



## Java

{{works with|Java|1.5+}}

```java5
import java.util.Arrays;
import java.util.HashSet;

public class VampireNumbers{
    private static int numDigits(long num){
        return Long.toString(Math.abs(num)).length();
    }

    private static boolean fangCheck(long orig, long fang1, long fang2){
        if(Long.toString(fang1).endsWith("0") && Long.toString(fang2).endsWith("0")) return false;

        int origLen = numDigits(orig);
        if(numDigits(fang1) != origLen / 2 || numDigits(fang2) != origLen / 2) return false;

        byte[] origBytes = Long.toString(orig).getBytes();
        byte[] fangBytes = (Long.toString(fang1) + Long.toString(fang2)).getBytes();
        Arrays.sort(origBytes);
        Arrays.sort(fangBytes);
        return Arrays.equals(origBytes, fangBytes);
    }

    public static void main(String[] args){
        HashSet<Long> vamps = new HashSet<Long>();
        for(long i = 10; vamps.size() <= 25; i++ ){
            if((numDigits(i) % 2) != 0) {i = i * 10 - 1; continue;}
            for(long fang1 = 2; fang1 <= Math.sqrt(i) + 1; fang1++){
                if(i % fang1 == 0){
                    long fang2 = i / fang1;
                    if(fangCheck(i, fang1, fang2) && fang1 <= fang2){
                        vamps.add(i);
                        System.out.println(i + ": [" + fang1 + ", " + fang2 +"]");
                    }
                }
            }
        }
        Long[] nums = {16758243290880L, 24959017348650L, 14593825548650L};
        for(Long i : nums){
            for(long fang1 = 2; fang1 <= Math.sqrt(i) + 1; fang1++){
                if(i % fang1 == 0){
                    long fang2 = i / fang1;
                    if(fangCheck(i, fang1, fang2) && fang1 <= fang2){
                        System.out.println(i + ": [" + fang1 + ", " + fang2 +"]");
                    }
                }
            }
        }
    }
}
```

Output:

```txt
1260: [21, 60]
1395: [15, 93]
1435: [35, 41]
1530: [30, 51]
1827: [21, 87]
2187: [27, 81]
6880: [80, 86]
102510: [201, 510]
104260: [260, 401]
105210: [210, 501]
105264: [204, 516]
105750: [150, 705]
108135: [135, 801]
110758: [158, 701]
115672: [152, 761]
116725: [161, 725]
117067: [167, 701]
118440: [141, 840]
120600: [201, 600]
123354: [231, 534]
124483: [281, 443]
125248: [152, 824]
125433: [231, 543]
125460: [204, 615]
125460: [246, 510]
125500: [251, 500]
126027: [201, 627]
16758243290880: [1982736, 8452080]
16758243290880: [2123856, 7890480]
16758243290880: [2751840, 6089832]
16758243290880: [2817360, 5948208]
24959017348650: [2947050, 8469153]
24959017348650: [2949705, 8461530]
24959017348650: [4125870, 6049395]
24959017348650: [4129587, 6043950]
24959017348650: [4230765, 5899410]
```



### Alternative version


```java
public class VampireNumber {

    public static void main(String args[]) {

        // scan only the ranges that have an even number of digits
        // for instance: 10 .. 99, 1000 .. 9999 etc
        long countVamps = 0, start = 10, tens = 10;
        outer:
        for (int numDigits = 2; numDigits <= 18; numDigits += 2) {
            long end = start * 10;
            for (long i = start; i < end; i++) {
                if (countFangs(i, tens) > 0) {
                    if (++countVamps >= 26)
                        break outer;
                }
            }
            start *= 100;
            tens *= 10;
        }
        System.out.println();

        long[] bigs = {16758243290880L, 24959017348650L,
            14593825548650L};

        for (long b : bigs)
            countFangs(b, 10000000L);
    }

    private static int countFangs(long n, long tens) {
        int countFangs = 0;

        // limit the search space for factors (as in C example)
        long lo = Math.max(tens / 10, (n + tens - 2) / (tens - 1));
        long hi = Math.min(n / lo, (long) Math.sqrt(n));

        long nTally = tallyDigits(n);

        for (long a = lo; a <= hi; a++) {
            long b = n / a;

            if (a * b != n)
                continue;

            // check for mod 9 congruence
            if (n % 9 != (a + b) % 9)
                continue;

            if (a % 10 == 0 && b % 10 == 0)
                continue;

            if (nTally == tallyDigits(a) + tallyDigits(b)) {
                if (countFangs == 0)
                    System.out.printf("\n%d : ", n);
                System.out.printf("[%d, %d]", a, b);
                countFangs++;
            }
        }
        return countFangs;
    }

    // sum to a unique number to represent set of digits (as in C example)
    private static long tallyDigits(long n) {
        long total = 0;
        while (n > 0) {
            total += 1L << ((n % 10) * 6);
            n /= 10;
        }
        return total;
    }
}
```


Output:


```txt

1260 : [21, 60]
1395 : [15, 93]
1435 : [35, 41]
1530 : [30, 51]
1827 : [21, 87]
2187 : [27, 81]
6880 : [80, 86]
102510 : [201, 510]
104260 : [260, 401]
105210 : [210, 501]
105264 : [204, 516]
105750 : [150, 705]
108135 : [135, 801]
110758 : [158, 701]
115672 : [152, 761]
116725 : [161, 725]
117067 : [167, 701]
118440 : [141, 840]
120600 : [201, 600]
123354 : [231, 534]
124483 : [281, 443]
125248 : [152, 824]
125433 : [231, 543]
125460 : [204, 615][246, 510]
125500 : [251, 500]
126027 : [201, 627]

16758243290880 : [1982736, 8452080][2123856, 7890480][2751840, 6089832][2817360, 5948208]
24959017348650 : [2947050, 8469153][2949705, 8461530][4125870, 6049395][4129587, 6043950][4230765, 5899410]
```



## Julia

'''Functions'''

```Julia

function divisors{T<:Integer}(n::T)
    !isprime(n) || return [one(T), n]
    d = [one(T)]
    for (k, v) in factor(n)
        e = T[k^i for i in 1:v]
        append!(d, vec([i*j for i in d, j in e]))
    end
    sort(d)
end

function vampirefangs{T<:Integer}(n::T)
    fangs = T[]
    isvampire = false
    vdcnt = ndigits(n)
    fdcnt = vdcnt>>1
    iseven(vdcnt) || return (isvampire, fangs)
    !isprime(n) || return (isvampire, fangs)
    vdigs = sort(digits(n))
    d = divisors(n)
    len = length(d)
    len = iseven(len) ? len>>1 : len>>1 + 1
    for f in d[1:len]
        ndigits(f) == fdcnt || continue
        g = div(n, f)
        f%10!=0 || g%10!=0 || continue
        sort([digits(f), digits(g)]) == vdigs || continue
        isvampire = true
        append!(fangs, [f, g])
    end
    if isvampire
        fangs = reshape(fangs, (2,length(fangs)>>1))'
    end
    return (isvampire, fangs)
end

```


'''Main'''

```Julia

function showvampire{T<:Integer}(i::T, n::T, fangs::Array{T,2})
    s = @sprintf "%6d  %14d %s\n" i n join(fangs[1,:], "\u00d7")
    for i in 2:size(fangs)[1]
        s *= " "^23*join(fangs[i,:], "\u00d7")*"\n"
    end
    return s
end

vgoal = 25
vcnt = 0
dcnt = 0
println("Finding the first ", vgoal, " vampire numbers.")
println("     N         Vampire Fangs")
while vcnt < vgoal
    dcnt += 2
    for i in (10^(dcnt-1)):(10^dcnt-1)
        (isvampire, fangs) = vampirefangs(i)
        isvampire || continue
        vcnt += 1
        print(showvampire(vcnt, i, fangs))
        vcnt < vgoal || break
    end
end

test = [16758243290880, 24959017348650, 14593825548650]
println()
println("Checking a few numbers.")
println("     N         Vampire Fangs")
for (i, v) in enumerate(test)
    (isvampire, fangs) = vampirefangs(v)
    if isvampire
        print(showvampire(i, v, fangs))
    else
        println(@sprintf "%6d  %14d is not a vampire" i v)
    end
end

```


{{out}}

```txt

Finding the first 25 vampire numbers.
     N         Vampire Fangs
     1            1260 21×60
     2            1395 15×93
     3            1435 35×41
     4            1530 30×51
     5            1827 21×87
     6            2187 27×81
     7            6880 80×86
     8          102510 201×510
     9          104260 260×401
    10          105210 210×501
    11          105264 204×516
    12          105750 150×705
    13          108135 135×801
    14          110758 158×701
    15          115672 152×761
    16          116725 161×725
    17          117067 167×701
    18          118440 141×840
    19          120600 201×600
    20          123354 231×534
    21          124483 281×443
    22          125248 152×824
    23          125433 231×543
    24          125460 204×615
                       246×510
    25          125500 251×500

Checking a few numbers.
     N         Vampire Fangs
     1  16758243290880 1982736×8452080
                       2123856×7890480
                       2751840×6089832
                       2817360×5948208
     2  24959017348650 2947050×8469153
                       2949705×8461530
                       4125870×6049395
                       4129587×6043950
                       4230765×5899410
     3  14593825548650 is not a vampire

```



## Kotlin


```scala
// version 1.1

data class Fangs(val fang1: Long = 0L, val fang2: Long = 0L)

fun pow10(n: Int): Long = when {
    n < 0 -> throw IllegalArgumentException("Can't be negative")
    else -> {
        var pow = 1L
        for (i in 1..n) pow *= 10L
        pow
    }
}

fun countDigits(n: Long): Int = when {
    n < 0L -> throw IllegalArgumentException("Can't be negative")
    n == 0L -> 1
    else -> {
        var count = 0
        var nn = n
        while (nn > 0L) {
            count++
            nn /= 10L
        }
        count
    }
}

fun hasTrailingZero(n: Long): Boolean = when {
    n < 0L -> throw IllegalArgumentException("Can't be negative")
    else -> n % 10L == 0L
}

fun sortedString(s: String): String {
    val ca = s.toCharArray()
    ca.sort()
    return String(ca)
}

fun isVampiric(n: Long, fl: MutableList<Fangs>): Boolean {
    if (n < 0L) return false
    val len = countDigits(n)
    if (len % 2L == 1L) return false
    val hlen = len / 2
    val first = pow10(hlen - 1)
    val last = 10L * first
    var j: Long
    var cd: Int
    val ss = sortedString(n.toString())
    for (i in first until last) {
        if (n % i != 0L) continue
        j = n / i
        if (j < i) return fl.size > 0
        cd = countDigits(j)
        if (cd > hlen) continue
        if (cd < hlen) return fl.size > 0
        if (ss != sortedString(i.toString() + j.toString())) continue
        if (!(hasTrailingZero(i) && hasTrailingZero(j))) {
            fl.add(Fangs(i, j))
        }
    }
    return fl.size > 0
}

fun showFangs(fangsList: MutableList<Fangs>): String {
    var s = ""
    for ((fang1, fang2) in fangsList) {
        s += " = $fang1 x $fang2"
    }
    return s
}

fun main(args: Array<String>) {
    println("The first 25 vampire numbers and their fangs are:")
    var count = 0
    var n: Long = 0
    val fl = mutableListOf<Fangs>()
    while (true) {
        if (isVampiric(n, fl)) {
            count++
            println("${"%2d".format(count)} : $n\t${showFangs(fl)}")
            fl.clear()
            if (count == 25) break
        }
        n++
    }
    println()
    val va = longArrayOf(16758243290880L, 24959017348650L, 14593825548650L)
    for (v in va) {
        if (isVampiric(v, fl)) {
            println("$v\t${showFangs(fl)}")
            fl.clear()
        } else {
            println("$v\t = not vampiric")
        }
    }
}
```


{{out}}

```txt

The first 25 vampire numbers and their fangs are:
 1 : 1260        = 21 x 60
 2 : 1395        = 15 x 93
 3 : 1435        = 35 x 41
 4 : 1530        = 30 x 51
 5 : 1827        = 21 x 87
 6 : 2187        = 27 x 81
 7 : 6880        = 80 x 86
 8 : 102510      = 201 x 510
 9 : 104260      = 260 x 401
10 : 105210      = 210 x 501
11 : 105264      = 204 x 516
12 : 105750      = 150 x 705
13 : 108135      = 135 x 801
14 : 110758      = 158 x 701
15 : 115672      = 152 x 761
16 : 116725      = 161 x 725
17 : 117067      = 167 x 701
18 : 118440      = 141 x 840
19 : 120600      = 201 x 600
20 : 123354      = 231 x 534
21 : 124483      = 281 x 443
22 : 125248      = 152 x 824
23 : 125433      = 231 x 543
24 : 125460      = 204 x 615 = 246 x 510
25 : 125500      = 251 x 500

16758243290880   = 1982736 x 8452080 = 2123856 x 7890480 = 2751840 x 6089832 = 2817360 x 5948208
24959017348650   = 2947050 x 8469153 = 2949705 x 8461530 = 4125870 x 6049395 = 4129587 x 6043950 = 4230765 x 5899410
14593825548650   = not vampiric

```



## Mathematica


```Mathematica
ClearAll[VampireQ]
VampireQ[num_Integer] := Module[{poss, divs},
  divs = Select[Divisors[num], # <= Sqrt[num] &];
  poss = {#, num/#} & /@ divs;
  If[Length[poss] > 0,
   poss = Select[poss, Mod[#, 10] =!= {0, 0} &];
   If[Length[poss] > 0,
    poss = Select[poss, Length[IntegerDigits[First[#]]] == Length[IntegerDigits[Last[#]]] &];
    If[Length[poss] > 0,
     poss = Select[poss, Sort[IntegerDigits[num]] == Sort[Join @@ (IntegerDigits /@ #)] &];
     If[Length[poss] > 0
      ,
      Sow[{num, poss}];
      True
      ,
      False
     ]
    ,
    False
    ]
   ,
   False
   ]
  ,
  False
 ]
]
```

Testing out the function:

```Mathematica
Reap[Scan[VampireQ,Range[126027]]][[2,1]]//Grid
Reap[VampireQ[#]] & /@ {16758243290880, 24959017348650, 14593825548650} // Grid
```

{{out}}

```txt

1260	{{21,60}}
1395	{{15,93}}
1435	{{35,41}}
1530	{{30,51}}
1827	{{21,87}}
2187	{{27,81}}
6880	{{80,86}}
102510	{{201,510}}
104260	{{260,401}}
105210	{{210,501}}
105264	{{204,516}}
105750	{{150,705}}
108135	{{135,801}}
110758	{{158,701}}
115672	{{152,761}}
116725	{{161,725}}
117067	{{167,701}}
118440	{{141,840}}
120600	{{201,600}}
123354	{{231,534}}
124483	{{281,443}}
125248	{{152,824}}
125433	{{231,543}}
125460	{{204,615},{246,510}}
125500	{{251,500}}

True	{{{16758243290880,{{1982736,8452080},{2123856,7890480},{2751840,6089832},{2817360,5948208}}}}}
True	{{{24959017348650,{{2947050,8469153},{2949705,8461530},{4125870,6049395},{4129587,6043950},{4230765,5899410}}}}}
False	{}
```



## PARI/GP


{{Lines too long|PARI/GP}}


```parigp
fang(n)=my(v=digits(n),u=List());if(#v%2,return([]));fordiv(n,d,if(#Str(d)==#v/2 && #Str(n/d)==#v/2 && vecsort(v)==vecsort(concat(digits(d),digits(n/d))) && (d%10 || (n/d)%10), if(d^2>n,return(Vec(u))); listput(u, d))); Vec(u)
k=25;forstep(d=4,6,2,for(n=10^(d-1),10^d-1,f=fang(n); for(i=1,#f,print(n" "f[i]" "n/f[i]); if(i==#f && k--==0,return))))
print();v=[16758243290880, 24959017348650, 14593825548650];
for(i=1,#v,f=fang(v[i]); for(j=1,#f, print(v[i]" "f[j]" "v[i]/f[j])))
```

Output:

```txt
1260 21 60
1395 15 93
1435 35 41
1530 30 51
1827 21 87
2187 27 81
6880 80 86
102510 201 510
104260 260 401
105210 210 501
105264 204 516
105750 150 705
108135 135 801
110758 158 701
115672 152 761
116725 161 725
117067 167 701
118440 141 840
120600 201 600
123354 231 534
124483 281 443
125248 152 824
125433 231 543
125460 204 615
125460 246 510
125500 251 500

16758243290880 1982736 8452080
16758243290880 2123856 7890480
16758243290880 2751840 6089832
16758243290880 2817360 5948208
24959017348650 2947050 8469153
24959017348650 2949705 8461530
24959017348650 4125870 6049395
24959017348650 4129587 6043950
24959017348650 4230765 5899410
```



## Perl


The trailing zeros condition is first triggered when searching for the 26th vampire number.


```perl
#!/usr/bin/perl
use warnings;
use strict;
use feature qw(say);

sub fangs {
    my $vampire = shift;
    my $length  = length 0 + $vampire;
    return if $length % 2;
    my $fang_length = $length / 2;
    my $from        = '1' . '0' x ($fang_length - 1);
    my $to          = '9' x $fang_length;
    my $sorted      = join q(), sort split //, $vampire;
    my @fangs;
    for my $f1 ($from .. 1 + sqrt $vampire) {
        next if $vampire % $f1;
        my $f2 = $vampire / $f1;
        next if $sorted ne join q(), sort split //, $f1 . $f2;
        next if 2 == grep '0' eq substr($_, -1 , 1), $f1, $f2; # Needed for the 26th number.
        push @fangs, [$f1, $f2];
    }
    return @fangs;
}

my $count = 0;
my $i     = 9;
while ($count < 25) {
    $i++;
    my @f = fangs($i);
    $count++, say join ' ', "$count. $i:", map "[@$_]", @f if @f;
}

say join ' ', $_, map "[@$_]", fangs($_) for 16758243290880, 24959017348650, 14593825548650;
```


A faster version, using the `divisors()` function from the <code>ntheory</code> library.
{{trans|Sidef}}
{{libheader|ntheory}}

```perl
use ntheory qw(sqrtint logint divisors);

sub is_vampire {
    my ($n) = @_;

    return if (length($n) % 2 or $n < 0);

    my $l1 = 10**logint(sqrtint($n), 10);
    my $l2 = sqrtint($n);

    my $s = join('', sort split(//, $n));

    my @fangs;

    foreach my $d (divisors($n)) {

        $d < $l1 and next;
        $d > $l2 and last;

        my $t = $n / $d;

        next if ($d % 10 == 0 and $t % 10 == 0);
        next if (join('', sort split(//, "$d$t")) ne $s);

        push @fangs, [$d, $t];
    }

    return @fangs;
}

print "First 25 Vampire Numbers:\n";

for (my ($n, $i) = (1, 1) ; $i <= 25 ; ++$n) {
    if (my @fangs = is_vampire($n)) {
        printf("%2d. %6s : %s\n", $i++, $n, join(' ', map { "[@$_]" } @fangs));
    }
}

print "\nIndividual tests:\n";

foreach my $n (16758243290880, 24959017348650, 14593825548650) {
    my @fangs = is_vampire($n);
    print("$n: ", (@fangs ? join(' ', map { "[@$_]" } @fangs)
                          : "is not a vampire number"), "\n");
}
```

{{out}}

```txt

First 25 Vampire Numbers:
 1.   1260 : [21 60]
 2.   1395 : [15 93]
 3.   1435 : [35 41]
 4.   1530 : [30 51]
 5.   1827 : [21 87]
 6.   2187 : [27 81]
 7.   6880 : [80 86]
 8. 102510 : [201 510]
 9. 104260 : [260 401]
10. 105210 : [210 501]
11. 105264 : [204 516]
12. 105750 : [150 705]
13. 108135 : [135 801]
14. 110758 : [158 701]
15. 115672 : [152 761]
16. 116725 : [161 725]
17. 117067 : [167 701]
18. 118440 : [141 840]
19. 120600 : [201 600]
20. 123354 : [231 534]
21. 124483 : [281 443]
22. 125248 : [152 824]
23. 125433 : [231 543]
24. 125460 : [204 615] [246 510]
25. 125500 : [251 500]

Individual tests:
16758243290880: [1982736 8452080] [2123856 7890480] [2751840 6089832] [2817360 5948208]
24959017348650: [2947050 8469153] [2949705 8461530] [4125870 6049395] [4129587 6043950] [4230765 5899410]
14593825548650: is not a vampire number

```



## Perl 6



```perl6
sub is_vampire (Int $num) {
    my $digits = $num.comb.sort;
    my @fangs;
    (10**$num.sqrt.log(10).floor .. $num.sqrt.ceiling).map: -> $this {
        next if $num % $this;
        my $that = $num div $this;
        next if $this %% 10 && $that %% 10;
        @fangs.push("$this x $that") if ($this ~ $that).comb.sort eq $digits;
    }
    @fangs
}

constant @vampires = flat (3..*).map: -> $s, $e {
    (10**$s .. 10**$e).hyper.map: -> $n {
        next unless my @fangs = is_vampire($n);
        "$n: { @fangs.join(', ') }"
    }
}

say "\nFirst 25 Vampire Numbers:\n";

.say for @vampires[^25];

say "\nIndividual tests:\n";

.say for (16758243290880, 24959017348650, 14593825548650).hyper(:1batch).map: {
    "$_: " ~ (is_vampire($_).join(', ') || 'is not a vampire number.')
}
```



```txt
First 25 Vampire Numbers:

1260: 21 x 60
1395: 15 x 93
1435: 35 x 41
1530: 30 x 51
1827: 21 x 87
2187: 27 x 81
6880: 80 x 86
102510: 201 x 510
104260: 260 x 401
105210: 210 x 501
105264: 204 x 516
105750: 150 x 705
108135: 135 x 801
110758: 158 x 701
115672: 152 x 761
116725: 161 x 725
117067: 167 x 701
118440: 141 x 840
120600: 201 x 600
123354: 231 x 534
124483: 281 x 443
125248: 152 x 824
125433: 231 x 543
125460: 204 x 615, 246 x 510
125500: 251 x 500

Individual tests:

16758243290880: 1982736 x 8452080, 2123856 x 7890480, 2751840 x 6089832, 2817360 x 5948208
24959017348650: 2947050 x 8469153, 2949705 x 8461530, 4125870 x 6049395, 4129587 x 6043950, 4230765 x 5899410
14593825548650: is not a vampire number.
```



## Phix


```Phix
function vampire(atom v)
    sequence res = {}
    if v>=0 then
        string vs = sprintf("%d",v)
        if mod(length(vs),2)=0 then -- even length
            vs = sort(vs)
            for i=power(10,length(vs)/2-1) to floor(sqrt(v)) do
                if remainder(v,i)=0 then
                    integer i2 = v/i
                    string si = sprintf("%d",i),
                           s2 = sprintf("%d",i2)
                    if (si[$]!='0' or s2[$]!='0')
                    and sort(si&s2)=vs then
                        res = append(res,{i,i2})
                    end if
                end if
            end for
        end if
    end if
    return res
end function

integer found = 0
atom i = 0
sequence res
puts(1,"The first 26 vampire numbers and their fangs:\n")
while found<26 do
    res = vampire(i)
    if length(res) then
        found += 1
        printf(1,"%d: %d: %s\n",{found,i,sprint(res)})
    end if
    i += 1
end while
puts(1,"\n")

constant tests = {16758243290880,
                  24959017348650,
                  14593825548650}

for t=1 to length(tests) do
    i = tests[t]
    res = vampire(i)
    printf(1,"%d: %s\n",{i,iff(res={}?"not a vampire number":sprint(res))})
end for
```

{{out}}

```txt

The first 26 vampire numbers and their fangs:
1: 1260: {{21,60}}
2: 1395: {{15,93}}
3: 1435: {{35,41}}
4: 1530: {{30,51}}
5: 1827: {{21,87}}
6: 2187: {{27,81}}
7: 6880: {{80,86}}
8: 102510: {{201,510}}
9: 104260: {{260,401}}
10: 105210: {{210,501}}
11: 105264: {{204,516}}
12: 105750: {{150,705}}
13: 108135: {{135,801}}
14: 110758: {{158,701}}
15: 115672: {{152,761}}
16: 116725: {{161,725}}
17: 117067: {{167,701}}
18: 118440: {{141,840}}
19: 120600: {{201,600}}
20: 123354: {{231,534}}
21: 124483: {{281,443}}
22: 125248: {{152,824}}
23: 125433: {{231,543}}
24: 125460: {{204,615},{246,510}}
25: 125500: {{251,500}}
26: 126027: {{201,627}}

16758243290880: {{1982736,8452080},{2123856,7890480},{2751840,6089832},{2817360,5948208}}
24959017348650: {{2947050,8469153},{2949705,8461530},{4125870,6049395},{4129587,6043950},{4230765,5899410}}
14593825548650: not a vampire number

```



## PureBasic


```PureBasic
EnableExplicit
DisableDebugger

Macro CheckVamp(CheckNum)
  c=0 : i=CheckNum : Print(~"\nCheck number: "+Str(i)+~"\n")
  Gosub VampireLoop : If c=0 : Print(Str(i)+" is not vampiric.") : EndIf : PrintN("")
EndMacro

Procedure.i Factor(number.i,counter.i)
  If number>0 And number>=counter*counter And number%counter=0
      ProcedureReturn counter
  EndIf  
  ProcedureReturn 0
EndProcedure

Procedure.b IsVampire(f1.i,f2.i)
  Define a.s=Str(f1*f2),
         b.s=Str(f1),
         c.s=Str(f2),
         d.s=b+c,
         i.i
  If Len(a)=Len(d) And Len(b)=Len(c)
    While Len(a)
      i=FindString(d,Left(a,1))
      If i
        a=Mid(a,2)
        d=RemoveString(d,Mid(d,i,1),#PB_String_NoCase,i,1)
      Else
        ProcedureReturn #False        
      EndIf
    Wend
    ProcedureReturn Bool(Len(d)=0)
  EndIf
  ProcedureReturn #False
EndProcedure

OpenConsole("Vampire number")
Define i.i,
       j.i,
       m.i,
       c.i=0

PrintN("The first 25 Vampire numbers...")
While c<25 : i+1 : Gosub VampireLoop : Wend
PrintN("")
CheckVamp(16758243290880) : CheckVamp(24959017348650) : CheckVamp(14593825548650)
Input()
End

VampireLoop:
  For j=1 To Int(Sqr(i))
    If Factor(i,j)>0
      m=i/j
    Else
      Continue
    EndIf
    If IsVampire(m,j)
      c+1
      PrintN(RSet(Str(c),3," ")+". "+RSet(Str(i),10," ")+": ["+Str(j)+", "+Str(m)+"]")
    EndIf    
  Next  
Return
```

{{out}}

```txt
The first 25 Vampire numbers...
  1.       1260: [21, 60]
  2.       1395: [15, 93]
  3.       1435: [35, 41]
  4.       1530: [30, 51]
  5.       1827: [21, 87]
  6.       2187: [27, 81]
  7.       6880: [80, 86]
  8.     102510: [201, 510]
  9.     104260: [260, 401]
 10.     105210: [210, 501]
 11.     105264: [204, 516]
 12.     105750: [150, 705]
 13.     108135: [135, 801]
 14.     110758: [158, 701]
 15.     115672: [152, 761]
 16.     116725: [161, 725]
 17.     117067: [167, 701]
 18.     118440: [141, 840]
 19.     120600: [201, 600]
 20.     123354: [231, 534]
 21.     124483: [281, 443]
 22.     125248: [152, 824]
 23.     125433: [231, 543]
 24.     125460: [204, 615]
 25.     125460: [246, 510]


Check number: 16758243290880
  1. 1675824329: [1982736, 8452080]
  2. 1675824329: [2123856, 7890480]
  3. 1675824329: [2751840, 6089832]
  4. 1675824329: [2817360, 5948208]


Check number: 24959017348650
  1. 2495901734: [2947050, 8469153]
  2. 2495901734: [2949705, 8461530]
  3. 2495901734: [4125870, 6049395]
  4. 2495901734: [4129587, 6043950]
  5. 2495901734: [4230765, 5899410]


Check number: 14593825548650
14593825548650 is not vampiric.
```



## Python

This routine finds ''all'' the fangs for a number.

```python
from __future__ import division

import math
from operator import mul
from itertools import product
from functools import reduce


def fac(n):
    '''\
    return the prime factors for n
    >>> fac(600)
    [5, 5, 3, 2, 2, 2]
    >>> fac(1000)
    [5, 5, 5, 2, 2, 2]
    >>>  
    '''
    step = lambda x: 1 + x*4 - (x//2)*2
    maxq = int(math.floor(math.sqrt(n)))
    d = 1
    q = n % 2 == 0 and 2 or 3 
    while q <= maxq and n % q != 0:
        q = step(d)
        d += 1
    res = []
    if q <= maxq:
        res.extend(fac(n//q))
        res.extend(fac(q)) 
    else: res=[n]
    return res

def fact(n):
    '''\
    return the prime factors and their multiplicities for n
    >>> fact(600)
    [(2, 3), (3, 1), (5, 2)]
    >>> fact(1000)
    [(2, 3), (5, 3)]
    >>> 
    '''
    res = fac(n)
    return [(c, res.count(c)) for c in set(res)]

def divisors(n):
    'Returns all the divisors of n'
    factors = fact(n)   # [(primefactor, multiplicity), ...]
    primes, maxpowers = zip(*factors)
    powerranges = (range(m+1) for m in maxpowers)
    powers = product(*powerranges)
    return (
        reduce(mul,
               (prime**power for prime, power in zip(primes, powergroup)),
               1)
        for powergroup in powers)
    
def vampire(n):
    fangsets = set( frozenset([d, n//d])
                    for d in divisors(n)
                    if (len(str(d)) == len(str(n))/2.
                        and sorted(str(d) + str(n//d)) == sorted(str(n))
                        and (str(d)[-1] == 0) + (str(n//d)[-1] == 0) <=1) )
    return sorted(tuple(sorted(fangs)) for fangs in fangsets)
    

if __name__ == '__main__':
    print('First 25 vampire numbers')
    count = n = 0
    while count <25:
        n += 1
        fangpairs = vampire(n)
        if fangpairs:
            count += 1
            print('%i: %r' % (n, fangpairs))
    print('\nSpecific checks for fangpairs')
    for n in (16758243290880, 24959017348650, 14593825548650):
        fangpairs = vampire(n)
        print('%i: %r' % (n, fangpairs))
```

{{out}}

```txt
First 25 vampire numbers
1260: [(21, 60)]
1395: [(15, 93)]
1435: [(35, 41)]
1530: [(30, 51)]
1827: [(21, 87)]
2187: [(27, 81)]
6880: [(80, 86)]
102510: [(201, 510)]
104260: [(260, 401)]
105210: [(210, 501)]
105264: [(204, 516)]
105750: [(150, 705)]
108135: [(135, 801)]
110758: [(158, 701)]
115672: [(152, 761)]
116725: [(161, 725)]
117067: [(167, 701)]
118440: [(141, 840)]
120600: [(201, 600)]
123354: [(231, 534)]
124483: [(281, 443)]
125248: [(152, 824)]
125433: [(231, 543)]
125460: [(204, 615), (246, 510)]
125500: [(251, 500)]

Specific checks for fangpairs
16758243290880: [(1982736, 8452080), (2123856, 7890480), (2751840, 6089832), (2817360, 5948208)]
24959017348650: [(2947050, 8469153), (2949705, 8461530), (4125870, 6049395), (4129587, 6043950), (4230765, 5899410)]
14593825548650: []
```


This alternative solution is not fast but it's short:
{{trans|Clojure}}

```python
from math import sqrt
from itertools import imap, ifilter, islice, count

def factor_pairs(n):
    return ((x, n // x) for x in xrange(2, int(sqrt(n))) if n % x == 0)

def fangs(n):
    dlen = lambda x: len(str(x))
    half = dlen(n) // 2
    digits = lambda (x, y): sorted(str(x) + str(y))
    halvesQ = lambda xs: all(y == half for y in imap(dlen, xs))
    dn = sorted(str(n))
    return [p for p in factor_pairs(n) if halvesQ(p) and dn==digits(p)]

def vampiricQ(n):
    fn = fangs(n)
    return (n, fn) if fn else None

for v in islice(ifilter(None, imap(vampiricQ, count())), 0, 25):
    print v

for n in [16758243290880, 24959017348650, 14593825548650]:
    print vampiricQ(n) or str(n) + " is not vampiric."
```

{{out}}

```txt
(1260, [(21, 60)])
(1395, [(15, 93)])
(1435, [(35, 41)])
(1530, [(30, 51)])
(1827, [(21, 87)])
(2187, [(27, 81)])
(6880, [(80, 86)])
(102510, [(201, 510)])
(104260, [(260, 401)])
(105210, [(210, 501)])
(105264, [(204, 516)])
(105750, [(150, 705)])
(108135, [(135, 801)])
(110758, [(158, 701)])
(115672, [(152, 761)])
(116725, [(161, 725)])
(117067, [(167, 701)])
(118440, [(141, 840)])
(120600, [(201, 600)])
(123354, [(231, 534)])
(124483, [(281, 443)])
(125248, [(152, 824)])
(125433, [(231, 543)])
(125460, [(204, 615), (246, 510)])
(125500, [(251, 500)])
(16758243290880L, [(1982736, 8452080L), (2123856, 7890480L), (2751840, 6089832L), (2817360, 5948208L)])
(24959017348650L, [(2947050, 8469153L), (2949705, 8461530L), (4125870, 6049395L), (4129587, 6043950L), (4230765, 5899410L)])
14593825548650 is not vampiric.
```



## Racket


Naive implementation, but notice the sequence-filter/sequence-map composition --
it's a good way to "find the first n numbers meeting predicate p?":

```racket
#lang racket

;; chock full of fun... including divisors
(require math/number-theory)

;; predicate to tell if n is a vampire number
(define (sub-vampire?-and-fangs n)
  (define digit-count-n (add1 (order-of-magnitude n)))
  (define (string-sort-characters s) (sort (string->list s) char<?))
  (define digits-in-order-n (string-sort-characters (number->string n)))
  (define (fangs-of-n? d e)
    (and (<= d e) ; avoid duplication
         (= (add1 (order-of-magnitude d)) (add1 (order-of-magnitude e)) (/ digit-count-n 2))
         (not (= 0 (modulo d 10) (modulo e 10)))
         (equal? digits-in-order-n
                 (string-sort-characters (string-append (number->string d) (number->string e))))))
  
  (let* ((fangses (for*/list ((d (in-list (divisors n))) #:when (fangs-of-n? d (/ n d)))
                    (list d (/ n d)))))
    (and (not (null? fangses)) (cons n fangses))))

(define (vampire?-and-fangs n)
  (and (odd? (order-of-magnitude n)) ; even number of digits - else not even worth looking!
       (sub-vampire?-and-fangs n)))

(displayln "First 25 vampire numbers:")
(for ((vmp (sequence-filter identity (sequence-map vampire?-and-fangs (in-naturals 1))))
      (cnt (in-range 1 (add1 25))))
  (printf "#~a ~a~%" cnt vmp))

(displayln "Test the big numbers:")
(displayln (vampire?-and-fangs 16758243290880))
(displayln (vampire?-and-fangs 24959017348650))
(displayln (vampire?-and-fangs 14593825548650))
```

Output:

```txt
First 25 vampire numbers:
#1 (1260 (21 60))
#2 (1395 (15 93))
#3 (1435 (35 41))
#4 (1530 (30 51))
#5 (1827 (21 87))
#6 (2187 (27 81))
#7 (6880 (80 86))
#8 (102510 (201 510))
#9 (104260 (260 401))
#10 (105210 (210 501))
#11 (105264 (204 516))
#12 (105750 (150 705))
#13 (108135 (135 801))
#14 (110758 (158 701))
#15 (115672 (152 761))
#16 (116725 (161 725))
#17 (117067 (167 701))
#18 (118440 (141 840))
#19 (120600 (201 600))
#20 (123354 (231 534))
#21 (124483 (281 443))
#22 (125248 (152 824))
#23 (125433 (231 543))
#24 (125460 (204 615) (246 510))
#25 (125500 (251 500))
Test the big numbers:
(16758243290880 (1982736 8452080) (2123856 7890480) (2751840 6089832) (2817360 5948208))
(24959017348650 (2947050 8469153) (2949705 8461530) (4125870 6049395) (4129587 6043950) (4230765 5899410))
#f
```



## REXX

Note:   if the argument is negative, its absolute value is used to test if <u>that</u> single number is vampiric.

```rexx
/*REXX program displays  N  vampire numbers,  or  verifies  if  a number is vampiric.   */
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N= 25                    /*Not specified?  Then use the default.*/
!.0= 1260;   !.1= 11453481;   !.2= 115672;   !.3= 124483;   !.4= 105264  /*lowest #, dig*/
!.5= 1395;   !.6=   126846;   !.7=   1827;   !.8= 110758;   !.9= 156289  /*   "   "   " */
#= 0;          L= length(N);       aN= abs(N)    /*num. of vampire numbers found, so far*/
numeric digits max(9, length(aN) )               /*be able to handle ginormus numbers.  */
@vamp= right('vampire number', 20)               /*literal used when showing a vampire #*/
if N>0 then do j=1260  until  # >= N             /*search until N vampire numbers found.*/
            if length(j) // 2  then do;  j= j*10 - 1;  iterate   /*bump J to even length*/
                                    end                          /* [↑]  check if odd.  */
            parse var  j  ''  -1  _              /*obtain the last decimal digit of  J. */
            if j<!._  then iterate               /*is number tenable based on last dig? */
            f= vampire(j)                        /*obtain the  fangs  of  J.            */
            if f==''  then iterate               /*Are fangs null?   Yes, not vampire.  */
            #= # + 1                             /*bump the vampire count, Vlad.        */
            say @vamp   right(#, L)      "is: "      right(j, 9)',  fangs='   f
            end   /*j*/                          /* [↑]  process a range of numbers.    */
       else do;  f= vampire(aN)                  /* [↓]  process a number; obtain fangs.*/
            if f==''  then say       aN     " isn't a vampire number."
                      else say       aN     " is a vampire number, fangs="    f
            end
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
vampire: procedure; parse arg x,, $. a bot;   L= length(x)  /*get arg;  compute len of X*/
         if L//2  then return ''                            /*is L odd?   Then ¬vampire.*/
                     do k=1  for L;    _= substr(x, k, 1);         $._= $._ || _
                     end   /*k*/
         w= L % 2                                           /*%:   is REXX's integer  ÷ */
                     do m=0  for 10;   bot= bot || $.m
                     end   /*m*/
         top= left( reverse(bot), w)
         bot= left(bot, w)                                  /*determine limits of search*/
         inc= x // 2    +    1                              /*X is odd? INC=2. No? INC=1*/
         beg= max(bot, 10**(w-1) )                          /*calculate where  to start.*/
         if inc==2  then  if  beg//2==0  then beg= beg + 1  /*possibly adjust the start.*/
                                                            /* [↑]  odd  BEG  if odd INC*/
                  do d=beg  to  min(top, 10**w - 1)  by inc /*use smart start, end, inc.*/
                  if x // d \==0          then iterate      /*X  not ÷ by D?  Then skip,*/
                  q= x % d;     if d>q    then iterate      /*is   D > Q      Then skip.*/
                  if q*d//9 \== (q+d)//9  then iterate      /*modulo 9 congruence test. */
                  if length(q)    \==w    then iterate      /*Len of Q ¬= W?  Then skip.*/
                  parse var  q  ''  -1  _                   /*get last decimal dig. of Q*/
                  if _==0                 then if right(d, 1) ==0  then iterate
                  dq= d || q
                  t= x;             do i=1  for  L;          p= pos( substr(dq, i, 1),  t)
                                    if p==0  then iterate d; t= delstr(t, p, 1)
                                    end   /*i*/
                  a=a  '['d"∙"q'] '                         /*construct formatted fangs.*/
                  end   /*d*/                               /* [↑]  ∙  is a round bullet*/
         return a                                           /*return    formatted fangs.*/
```

{{out|output|text=  when using the default input:}}

```txt

      vampire number  1 is:       1260,  fangs=  [21∙60]
      vampire number  2 is:       1395,  fangs=  [15∙93]
      vampire number  3 is:       1435,  fangs=  [35∙41]
      vampire number  4 is:       1530,  fangs=  [30∙51]
      vampire number  5 is:       1827,  fangs=  [21∙87]
      vampire number  6 is:       2187,  fangs=  [27∙81]
      vampire number  7 is:       6880,  fangs=  [80∙86]
      vampire number  8 is:     102510,  fangs=  [201∙510]
      vampire number  9 is:     104260,  fangs=  [260∙401]
      vampire number 10 is:     105210,  fangs=  [210∙501]
      vampire number 11 is:     105264,  fangs=  [204∙516]
      vampire number 12 is:     105750,  fangs=  [150∙705]
      vampire number 13 is:     108135,  fangs=  [135∙801]
      vampire number 14 is:     110758,  fangs=  [158∙701]
      vampire number 15 is:     115672,  fangs=  [152∙761]
      vampire number 16 is:     116725,  fangs=  [161∙725]
      vampire number 17 is:     117067,  fangs=  [167∙701]
      vampire number 18 is:     118440,  fangs=  [141∙840]
      vampire number 19 is:     120600,  fangs=  [201∙600]
      vampire number 20 is:     123354,  fangs=  [231∙534]
      vampire number 21 is:     124483,  fangs=  [281∙443]
      vampire number 22 is:     125248,  fangs=  [152∙824]
      vampire number 23 is:     125433,  fangs=  [231∙543]
      vampire number 24 is:     125460,  fangs=  [204∙615]  [246∙510]
      vampire number 25 is:     125500,  fangs=  [251∙500]

```

{{out|output|text=  when using the input of:   <tt> -16758243290880 </tt>}}

```txt

16758243290880  is a vampire number, fangs=  [1982736∙8452080]  [2123856∙7890480]  [2751840∙6089832]  [2817360∙5948208]

```

{{out|output|text=  when using the input of:   <tt> -24959017348650 </tt>}}

```txt

24959017348650  is a vampire number, fangs=  [2947050∙8469153]  [2949705∙8461530]  [4125870∙6049395]  [4129587∙6043950]  [4230765∙5899410]

```

{{out|output|text=  when using the input of:   <tt> -14593825548650 </tt>}}

```txt

14593825548650  isn't a vampire number.

```



## Ring


```ring

# Project : Vampire number

for p = 10 to 127000
     vampire(p)
next

func vampire(listnum)
        sum = 0
        flag = 1
        list = list(len(string(listnum)))
        total = newlist(len(list),2)
       for n = 1 to len(string(listnum))
            liststr = string(listnum)
            list[n] = liststr[n]
       next

       for perm = 1 to fact(len(list))
            numstr = substr(list2str(list), nl, "")
            num1 = number(left(numstr,len(numstr)/2))
            num2 = number(right(numstr,len(numstr)/2))
            if (listnum = num1 * num2)
                for n = 1 to len(total)
                     if (num1 = total[n][2] and num2 = total[n][1]) or
                         (num1 = total[n][1] and num2 = total[n][2])
                         flag = 0
                     ok
               next
               if flag = 1
                   sum = sum + 1
                   total[sum][1] = num1
                   total[sum][2] = num2                
                   see "" + listnum + ": [" + num1 + "," + num2 + "]" + nl
                ok
             ok
            nextPermutation(list)
       next
 
func nextPermutation(a)
     elementcount = len(a)
     if elementcount < 1 then return ok
     pos = elementcount-1
     while a[pos] >= a[pos+1] 
           pos -= 1
           if pos <= 0 permutationReverse(a, 1, elementcount)
              return ok
     end
     last = elementcount
     while a[last] <= a[pos]
           last -= 1
     end
     temp = a[pos]
     a[pos] = a[last]
     a[last] = temp
     permutationReverse(a, pos+1, elementcount)
 
 func permutationReverse a, first, last
      while first < last
            temp = a[first]
            a[first] = a[last]
            a[last] = temp
            first += 1
            last -= 1
      end

func fact(nr)
        if nr = 1 
           return 1 
        else
           return nr * fact(nr-1)
        ok

func newlist(x,y)
        if isstring(x) x=0+x ok
        if isstring(y) y=0+y ok
        alist = list(x)
        for t in alist
              t = list(y)
        next
        return alist

```

Output:

```txt

1260:	[21,60]
1395:	[15,93]
1435:	[35,41]
1530:	[30,51]
1827:	[21,87]
2187:	[27,81]
6880:	[80,86]
102510:	[201,510]
104260:	[260,401]
105210:	[210,501]
105264:	[204,516]
105750:	[150,705]
108135:	[135,801]
110758:	[158,701]
115672:	[152,761]
116725:	[161,725]
117067:	[167,701]
118440:	[141,840]
123354:	[231,534]
124483:	[281,443]
125248:	[152,824]
125433:	[231,543]
125460:	[204,615]
	        [246,510]
125500:	[251,500]
126027:	[201,627]

```



## Ruby


```Ruby
def factor_pairs n
  first = n / (10 ** (n.to_s.size / 2) - 1)
  (first .. n ** 0.5).map { |i| [i, n / i] if n % i == 0 }.compact
end

def vampire_factors n
  return [] if n.to_s.size.odd?
  half = n.to_s.size / 2
  factor_pairs(n).select do |a, b|
    a.to_s.size == half && b.to_s.size == half &&
    [a, b].count {|x| x%10 == 0} != 2          &&
    "#{a}#{b}".chars.sort == n.to_s.chars.sort
  end
end

i = vamps = 0
until vamps == 25
  vf = vampire_factors(i += 1)
  unless vf.empty?
    puts "#{i}:\t#{vf}"
    vamps += 1
  end
end

[16758243290880, 24959017348650, 14593825548650].each do |n|
  if (vf = vampire_factors n).empty?
    puts "#{n} is not a vampire number!"
  else
    puts "#{n}:\t#{vf}"
  end
end
```

{{out}}

```txt

1260:	[[21, 60]]
1395:	[[15, 93]]
1435:	[[35, 41]]
1530:	[[30, 51]]
1827:	[[21, 87]]
2187:	[[27, 81]]
6880:	[[80, 86]]
102510:	[[201, 510]]
104260:	[[260, 401]]
105210:	[[210, 501]]
105264:	[[204, 516]]
105750:	[[150, 705]]
108135:	[[135, 801]]
110758:	[[158, 701]]
115672:	[[152, 761]]
116725:	[[161, 725]]
117067:	[[167, 701]]
118440:	[[141, 840]]
120600:	[[201, 600]]
123354:	[[231, 534]]
124483:	[[281, 443]]
125248:	[[152, 824]]
125433:	[[231, 543]]
125460:	[[204, 615], [246, 510]]
125500:	[[251, 500]]
16758243290880:	[[1982736, 8452080], [2123856, 7890480], [2751840, 6089832], [2817360, 5948208]]
24959017348650:	[[2947050, 8469153], [2949705, 8461530], [4125870, 6049395], [4129587, 6043950], [4230765, 5899410]]
14593825548650 is not a vampire number!

```



## Rust


```Rust
use std::cmp::{max, min};

static TENS: [u64; 20] = [
    1,
    10,
    100,
    1000,
    10000,
    100000,
    1000000,
    10000000,
    100000000,
    1000000000,
    10000000000,
    100000000000,
    1000000000000,
    10000000000000,
    100000000000000,
    1000000000000000,
    10000000000000000,
    100000000000000000,
    1000000000000000000,
    10000000000000000000,
];

/// Get the number of digits present in x
fn ndigits(mut x: u64) -> u64 {
    let mut n = 0;

    while x != 0 {
        n += 1;
        x /= 10;
    }

    n
}

fn dtally(mut x: u64) -> u64 {
    let mut t = 0;

    while x != 0 {
        t += 1 << ((x % 10) * 6);
        x /= 10;
    }

    t
}

/// Get a list of all fangs of x. Get only the first divider of each fang. The second one can be found simply with x / fang.
fn fangs(x: u64) -> Vec<u64> {
    let mut nd = ndigits(x) as usize;

    let mut fangs = vec![];

    if nd & 1 != 1 {
        nd /= 2;

        let lo = max(TENS[nd - 1], (x + TENS[nd] - 2) / (TENS[nd] - 1));
        let hi = min(x / lo, (x as f64).sqrt() as u64);

        let t = dtally(x);

        for a in lo..(hi + 1) {
            let b = x / a;
            if a * b == x && ((a % 10) > 0 || b % 10 > 0) && t == dtally(a) + dtally(b) {
                fangs.push(a);
            }
        }
    }

    fangs
}

/// Pretty print the fangs of x
fn print_fangs(x: u64, fangs: Vec<u64>) {
    print!("{} = ", x);

    if fangs.is_empty() {
        print!("is not vampiric");
    } else {
        for fang in fangs {
            print!("{} x {}, ", fang, x / fang);
        }
    }
    print!("\n");
}

fn main() {
    println!("The first 25 vampire numbers are :");

    let mut nfangs = 0;
    let mut x = 1;

    while nfangs < 25 {
        let fangs = fangs(x);
        if !fangs.is_empty() {
            nfangs += 1;
            print_fangs(x, fangs);
        }

        x += 1;
    }

    println!("\nSpecial requests :");

    print_fangs(16758243290880, fangs(16758243290880));
    print_fangs(24959017348650, fangs(24959017348650));
    print_fangs(14593825548650, fangs(14593825548650));
}

#[test]
fn test() {
    assert_eq!(
        fangs(16758243290880),
        vec![1982736, 2123856, 2751840, 2817360]
    );

    assert_eq!(
        fangs(24959017348650),
        vec![2947050, 2949705, 4125870, 4129587, 4230765]
    );

    assert_eq!(fangs(14593825548650), vec![]);
}
```



## Scala

{{works with|Scala|2.9.1}}

```Scala
import Stream._
import math._
import scala.collection.mutable.ListBuffer

object VampireNumbers extends App {
  val elapsed: (=> Unit) => Long = f => {val s = System.currentTimeMillis; f; (System.currentTimeMillis - s)/1000}

  val sexp = from(1, 2)   // stream of integer: 1,3,5,7, ...
  val rs: Stream[Int] => Stream[Pair[Long,Long]] = exps => Pair(pow(10,exps.head).toLong,(pow(10,exps.head)*10-1).toLong)#::rs(exps.tail)
  val srs = rs(sexp)   // stream of ranges: [10..99], [1000..9999], [100000..999999], ...
  val cs: Stream[Pair[Long,Long]] => Stream[Long] = rs => (rs.head._1 to rs.head._2).toStream#:::cs(rs.tail)
  val scs = cs(srs)   // stream of candidates: 10,11,..,99,1000,1001,..,9999, ...
  val it = scs.iterator

  val checkVN: Long => Pair[Long,Seq[Pair[Long,Long]]] = n => {
    val check: Pair[Long,Long] => Pair[Long,Long] = p => {
      val len: Long => Int = n => n.toString.size
      val (a,b) = p
      if ((a%10==0)&&(b%10==0)) Pair(0,0) else 
      if (len(a) != len(b)) Pair(0,0) else 
      if (n.toString.toList.diff(a.toString.toList++b.toString.toList)!=Nil) Pair(0,0) else p
    }
    Pair(n,(pow(10,log10(sqrt(n).toLong).toLong).toLong+1 to sqrt(n).toLong).filter{i=>n%i==0}
     .map {fac =>Pair(fac,n/fac)}.map {p => check(p)}.filter {p => p._1 != 0})
  }

  val et = elapsed {
    val lb = new ListBuffer[Pair[Long,Seq[Pair[Long,Long]]]]
    while ((lb.size<25)&&(it.hasNext)) {
      checkVN(it.next) match {
        case (n, Seq()) => 
        case p          => lb += p
      }
    }

    lb.toList.zipWithIndex.foreach {p =>
      println(p._2+1+": "+p._1._1+(p._1._2:\"")((x,y)=>" = "+x._1+" x "+x._2+y))
    }
    println

    List(16758243290880L, 24959017348650L, 14593825548650L)
      .map {checkVN(_)}
      .foreach {
         case (n, Seq()) => println(n+" is not vampiric")
         case p => println(p._1+(p._2:\"")((x,y)=>" = "+x._1+" x "+x._2+y))
       }
  }

  println("\n"+"elapsed time: "+et+" seconds")
}
```

Output:
<pre style="height: 30ex; overflow: scroll">1: 1260 = 21 x 60
2: 1395 = 15 x 93
3: 1435 = 35 x 41
4: 1530 = 30 x 51
5: 1827 = 21 x 87
6: 2187 = 27 x 81
7: 6880 = 80 x 86
8: 102510 = 201 x 510
9: 104260 = 260 x 401
10: 105210 = 210 x 501
11: 105264 = 204 x 516
12: 105750 = 150 x 705
13: 108135 = 135 x 801
14: 110758 = 158 x 701
15: 115672 = 152 x 761
16: 116725 = 161 x 725
17: 117067 = 167 x 701
18: 118440 = 141 x 840
19: 120600 = 201 x 600
20: 123354 = 231 x 534
21: 124483 = 281 x 443
22: 125248 = 152 x 824
23: 125433 = 231 x 543
24: 125460 = 204 x 615 = 246 x 510
25: 125500 = 251 x 500

16758243290880 = 1982736 x 8452080 = 2123856 x 7890480 = 2751840 x 6089832 = 2817360 x 5948208
24959017348650 = 2947050 x 8469153 = 2949705 x 8461530 = 4125870 x 6049395 = 4129587 x 6043950 = 4230765 x 5899410
14593825548650 is not vampiric

elapsed time: 11 seconds
```



## Sidef


```ruby
func is_vampire (n) {
    return [] if n.ilog10.is_even

    var l1 = n.isqrt.ilog10.ipow10
    var l2 = n.isqrt

    var s = n.digits.sort.join

    gather {
        n.divisors.each { |d|

            d < l1 && next
            d > l2 && break

            var t = n/d

            next if (d%%10 && t%%10)
            next if ("#{d}#{t}".sort != s)

            take([d, t])
        }
    }
}

say "First 25 Vampire Numbers:"

with (1) { |i|
    for (var n = 1; i <= 25; ++n) {
        var fangs = is_vampire(n)
        printf("%2d. %6s : %s\n", i++, n, fangs.join(' ')) if fangs
    }
}

say "\nIndividual tests:"

[16758243290880, 24959017348650, 14593825548650].each { |n|
    var fangs = is_vampire(n)
    say "#{n}: #{fangs ? fangs.join(', ') : 'is not a vampire number'}"
}
```

{{out}}

```txt

First 25 Vampire Numbers:
 1.   1260 : [21, 60]
 2.   1395 : [15, 93]
 3.   1435 : [35, 41]
 4.   1530 : [30, 51]
 5.   1827 : [21, 87]
 6.   2187 : [27, 81]
 7.   6880 : [80, 86]
 8. 102510 : [201, 510]
 9. 104260 : [260, 401]
10. 105210 : [210, 501]
11. 105264 : [204, 516]
12. 105750 : [150, 705]
13. 108135 : [135, 801]
14. 110758 : [158, 701]
15. 115672 : [152, 761]
16. 116725 : [161, 725]
17. 117067 : [167, 701]
18. 118440 : [141, 840]
19. 120600 : [201, 600]
20. 123354 : [231, 534]
21. 124483 : [281, 443]
22. 125248 : [152, 824]
23. 125433 : [231, 543]
24. 125460 : [204, 615] [246, 510]
25. 125500 : [251, 500]

Individual tests:
16758243290880: [1982736, 8452080], [2123856, 7890480], [2751840, 6089832], [2817360, 5948208]
24959017348650: [2947050, 8469153], [2949705, 8461530], [4125870, 6049395], [4129587, 6043950], [4230765, 5899410]
14593825548650: is not a vampire number

```



## Swift



```swift
import Foundation

func vampire<T>(n: T) -> [(T, T)] where T: BinaryInteger, T.Stride: SignedInteger {
  let strN = String(n).sorted()
  let fangLength = strN.count / 2
  let start = T(pow(10, Double(fangLength - 1)))
  let end = T(Double(n).squareRoot())

  var fangs = [(T, T)]()

  for i in start...end where n % i == 0 {
    let quot = n / i

    guard i % 10 != 0 || quot % 10 != 0 else {
      continue
    }

    if "\(i)\(quot)".sorted() == strN {
      fangs.append((i, quot))
    }
  }

  return fangs
}

var count = 0
var i = 1.0

while count < 25 {
  let start = Int(pow(10, i))
  let end = start * 10

  for num in start...end {
    let fangs = vampire(n: num)

    guard !fangs.isEmpty else { continue }

    count += 1

    print("\(num) is a vampire number with fangs: \(fangs)")

    guard count != 25 else { break }
  }

  i += 2
}

for (vamp, fangs) in [16758243290880, 24959017348650, 14593825548650].lazy.map({ ($0, vampire(n: $0)) }) {
  if fangs.isEmpty {
    print("\(vamp) is not a vampire number")
  } else {
    print("\(vamp) is a vampire number with fangs: \(fangs)")
  }
}
```


{{out}}


```txt
1260 is a vampire number with fangs: [(21, 60)]
1395 is a vampire number with fangs: [(15, 93)]
1435 is a vampire number with fangs: [(35, 41)]
1530 is a vampire number with fangs: [(30, 51)]
1827 is a vampire number with fangs: [(21, 87)]
2187 is a vampire number with fangs: [(27, 81)]
6880 is a vampire number with fangs: [(80, 86)]
102510 is a vampire number with fangs: [(201, 510)]
104260 is a vampire number with fangs: [(260, 401)]
105210 is a vampire number with fangs: [(210, 501)]
105264 is a vampire number with fangs: [(204, 516)]
105750 is a vampire number with fangs: [(150, 705)]
108135 is a vampire number with fangs: [(135, 801)]
110758 is a vampire number with fangs: [(158, 701)]
115672 is a vampire number with fangs: [(152, 761)]
116725 is a vampire number with fangs: [(161, 725)]
117067 is a vampire number with fangs: [(167, 701)]
118440 is a vampire number with fangs: [(141, 840)]
120600 is a vampire number with fangs: [(201, 600)]
123354 is a vampire number with fangs: [(231, 534)]
124483 is a vampire number with fangs: [(281, 443)]
125248 is a vampire number with fangs: [(152, 824)]
125433 is a vampire number with fangs: [(231, 543)]
125460 is a vampire number with fangs: [(204, 615), (246, 510)]
125500 is a vampire number with fangs: [(251, 500)]
16758243290880 is a vampire number with fangs: [(1982736, 8452080), (2123856, 7890480), (2751840, 6089832), (2817360, 5948208)]
24959017348650 is a vampire number with fangs: [(2947050, 8469153), (2949705, 8461530), (4125870, 6049395), (4129587, 6043950), (4230765, 5899410)]
14593825548650 is not a vampire number
```



## Tcl

{{trans|Ruby}}

```tcl
proc factorPairs {n {from 2}} {
    set result [list 1 $n]
    if {$from<=1} {set from 2}
    for {set i $from} {$i<=sqrt($n)} {incr i} {
	if {$n%$i} {} {lappend result $i [expr {$n/$i}]}
    }
    return $result
}

proc vampireFactors {n} {
    if {[string length $n]%2} return
    set half [expr {[string length $n]/2}]
    set digits [lsort [split $n ""]]
    set result {}
    foreach {a b} [factorPairs $n [expr {10**$half/10}]] {
	if {
	    [string length $a]==$half && [string length $b]==$half &&
	    ($a%10 || $b%10) && $digits eq [lsort [split $a$b ""]]
	} then {
	    lappend result [list $a $b]
	}
    }
    return $result
}
```

Demonstrating:

```tcl
# A nicer way to print the evidence of vampire-ness
proc printVampire {n pairs} {
    set out "${n}:"
    foreach p $pairs {
	append out " \[[join $p {, }]\]"
    }
    puts $out
}
set n 0
for {set i 0} {$i < 25} {incr i} {
    while 1 {
	if {[llength [set vamps [vampireFactors [incr n]]]]} {
	    printVampire $n $vamps
	    break
	}
    }
}
puts ""
foreach n {16758243290880 24959017348650 14593825548650} {
    if {[llength [set vamps [vampireFactors $n]]]} {
	printVampire $n $vamps
    } else {
	puts "$n is not a vampire number"
    }
}
```

{{out}}

```txt

1260: [21, 60]
1395: [15, 93]
1435: [35, 41]
1530: [30, 51]
1827: [21, 87]
2187: [27, 81]
6880: [80, 86]
102510: [201, 510]
104260: [260, 401]
105210: [210, 501]
105264: [204, 516]
105750: [150, 705]
108135: [135, 801]
110758: [158, 701]
115672: [152, 761]
116725: [161, 725]
117067: [167, 701]
118440: [141, 840]
120600: [201, 600]
123354: [231, 534]
124483: [281, 443]
125248: [152, 824]
125433: [231, 543]
125460: [204, 615] [246, 510]
125500: [251, 500]

16758243290880: [1982736, 8452080] [2123856, 7890480] [2751840, 6089832] [2817360, 5948208]
24959017348650: [2947050, 8469153] [2949705, 8461530] [4125870, 6049395] [4129587, 6043950] [4230765, 5899410]
14593825548650 is not a vampire number

```



## zkl


```zkl
fcn fangs(N){ //-->if Vampire number: (N,(a,b,c,...)), where a*x==N
   var [const] tens=[0 .. 18].pump(List,(10.0).pow,"toInt");

   (half:=N.numDigits) : if (_.isOdd) return(T);;
   half/=2; digits:=N.toString().sort();
   lo:=tens[half-1].max((N+tens[half])/(tens[half]));
   hi:=(N/lo).min(N.toFloat().sqrt());
   fs:=[lo .. hi].filter('wrap(n){
      N%n==0 and (n%10!=0 or (N/n)%10!=0) and
      (n.toString()+(N/n).toString()).sort()==digits
   });
   fs and T(N,fs) or T;
}
```


```zkl
fcn vampiric(fangs,n=Void){
   if(not fangs) return(n,"Not a Vampire number");
   v:=fangs[0]; T(v,fangs[1].apply('wrap(n){T(n,v/n)})) }

T(16758243290880, 24959017348650, 14593825548650)
.pump(Console.println,fcn(n){"%d: %s".fmt(vampiric(fangs(n),n).xplode())});

(0).walker(*).tweak(fangs).filter(26)
.pump(Console.println,vampiric);
```

{{out}}

```txt

16758243290880: L(L(1982736,8452080),L(2123856,7890480),L(2751840,6089832),L(2817360,5948208))
24959017348650: L(L(2947050,8469153),L(2949705,8461530),L(4125870,6049395),L(4129587,6043950),L(4230765,5899410))
14593825548650: Not a Vampire number
L(1260,L(L(21,60)))
L(1395,L(L(15,93)))
L(1435,L(L(35,41)))
L(1530,L(L(30,51)))
L(1827,L(L(21,87)))
L(2187,L(L(27,81)))
L(6880,L(L(80,86)))
L(102510,L(L(201,510)))
L(104260,L(L(260,401)))
L(105210,L(L(210,501)))
L(105264,L(L(204,516)))
L(105750,L(L(150,705)))
L(108135,L(L(135,801)))
L(110758,L(L(158,701)))
L(115672,L(L(152,761)))
L(116725,L(L(161,725)))
L(117067,L(L(167,701)))
L(118440,L(L(141,840)))
L(120600,L(L(201,600)))
L(123354,L(L(231,534)))
L(124483,L(L(281,443)))
L(125248,L(L(152,824)))
L(125433,L(L(231,543)))
L(125460,L(L(204,615),L(246,510)))
L(125500,L(L(251,500)))
L(126027,L(L(201,627)))

```


{{omit from|Brlcad}}
{{omit from|Lilypond}}
{{omit from|Openscad}}
