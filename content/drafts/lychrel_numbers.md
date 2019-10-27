+++
title = "Lychrel numbers"
description = ""
date = 2018-09-23T18:54:13Z
aliases = []
[extra]
id = 19542
[taxonomies]
categories = []
tags = []
+++

[[Category:Palindromes]]
{{task}}
# Take an integer n, greater than zero.
# Form the next n of its series by reversing the digits of the current n and adding the result to the current n.
# Stop when n becomes palindromic - i.e. the digits of n in reverse order == n.



The above recurrence relation when applied to most starting numbers n = 1, 2, ... terminates in a palindrome quite quickly, for example if n<sub>0</sub> = 12 we get

```txt
12
12 + 21 = 33, a palindrome!
```


And if n<sub>0</sub> = 55 we get

```txt
55
55 + 55 = 110
110 + 011 = 121, a palindrome!
```


Notice that the check for a palindrome happens ''after'' an addition.


Some starting numbers seem to go on forever; the recurrence relation for 196 has been calculated for millions of repetitions forming numbers with millions of digits, without forming a palindrome. These numbers that do not end in a palindrome are called '''Lychrel numbers'''.

For the purposes of this task a Lychrel number is any starting number that does not form a palindrome within 500 (or more) iterations.


;Seed and related Lychrel numbers:
Any integer produced in the sequence of a Lychrel number is also a Lychrel number.

In general, any sequence from one Lychrel number ''might'' converge to join the sequence from a prior Lychrel number candidate; for example the sequences for the numbers 196 and then 689 begin:

```txt
196
196 + 691 = 887
887 + 788 = 1675
1675 + 5761 = 7436
7436 + 6347 = 13783
13783 + 38731 = 52514
52514 + 41525 = 94039
...


689
689 + 986 = 1675
1675 + 5761 = 7436
...
```

So we see that the sequence starting with 689 converges to, and continues with the same numbers as that for 196. Because of this we can further split the Lychrel numbers into true '''Seed''' Lychrel number candidates, and '''Related''' numbers that produce no palindromes but have integers in their sequence seen as part of the sequence generated from a lower Lychrel number.


;Task:
*  Find the number of seed Lychrel number candidates and related numbers for n in the range 1..10000 inclusive. (With that iteration limit of 500).
*  Print the number of seed Lychrels found; the actual seed Lychrels; and just the ''number'' of relateds found.
*  Print any seed Lychrel or related number that is itself a palindrome.



Show all output here.


;References:
* [https://www.youtube.com/watch?v=bN8PE3eljdA What's special about 196?] Numberphile video.
* [http://oeis.org/A023108 A023108] Positive integers which apparently never result in a palindrome under repeated applications of the function f(x) = x + (x with digits reversed).
* [http://mathoverflow.net/questions/117104/status-of-the-196-conjecture/117277#117277 Status of the 196 conjecture?] Mathoverflow.





## ALGOL 68

As suggested by the Fortran example, uses a character array to hold the large numbers for convenience.


Uses the associative array from the Associative array iteration task.

```algol68
PR read "aArray.a68" PR

# number of additions to attempt before deciding the number is Lychrel #
INT max additions = 500;

# buffer to hold number during testing                                 #
# addition of two equal sized numbers can produce a number of at most  #
# one additional digit, so for 500 additions of numbers up to 10 000   #
# we need a bit over 500 digits...                                     #
[ 512 ]CHAR number;
FOR c TO UPB number DO number[ c ] := "0" OD;
# count of used digits in number                                       #
INT digits := 0;

# sets the number buffer to the specified positive value               #
PROC set number = ( INT value )VOID:
    BEGIN
        digits := 0;
        INT v  := ABS value;
        WHILE digits +:= 1;
              number[ digits ] := REPR ( ABS "0" + v MOD 10 );
              v OVERAB 10;
              v > 0
        DO SKIP OD
     END # set number # ;

# adds the reverse of number to itself                                 #
PROC add reverse = VOID:
     BEGIN
        [ digits + 1 ]CHAR result;
        INT carry := 0;
        INT r pos := digits;
        FOR d pos TO digits DO
            INT sum = ( ( ABS number[ d pos ] + ABS number[ r pos ] + carry )
                      - ( 2 * ABS "0" )
                      );
            IF sum < 10 THEN
                # no carry required                                    #
                result[ d pos ] := REPR( sum + ABS "0" );
                carry           := 0
            ELSE
                # need to carry                                        #
                result[ d pos ] := REPR ( ( sum - 10 ) + ABS "0" );
                carry           := 1
            FI;
            r pos -:= 1
        OD;
        IF carry /= 0 THEN
            # need another digit                                       #
            digits +:= 1;
            result[ digits ] := REPR ( ABS "0" + carry )
        FI;
        number[ : digits ] := result[ : digits ]
     END # add reverse # ;

# returns TRUE if number is a palindrome, FALSE otherwise              #
PROC is palindromic = BOOL:
     BEGIN
        BOOL result := TRUE;
        INT d pos := 1;
        INT r pos := digits;
        WHILE IF   d pos >= r pos
              THEN FALSE
              ELSE result := ( number[ d pos ] = number[ r pos ] )
              FI
        DO
            d pos +:= 1;
            r pos -:= 1
        OD;
        result
     END # is palindromic # ;

# associative array of numbers that are not palindromic after 500      #
# iterations                                                           #
REF AARRAY related := INIT HEAP AARRAY;

# adds the elements of the specified path to the related numbers       #
PROC add related numbers = ( REF AARRAY path )VOID:
     BEGIN
        REF AAELEMENT r := FIRST path;
        WHILE r ISNT nil element DO
            related // key OF r := "Y";
            r := NEXT path
        OD
     END # add related numbers # ;

# Lychrel number results                                               #
# lychrel[n] is:                                                       #
#    not lychrel      if n becomes palidromic before 500 additions     #
#    lychrel seed     if n is a seed lychrel number                    #
#    lychrel related  if n is a related lychrel number                 #
#    untested         if n hasn't been tested yet                      #
INT not lychrel      = 0;
INT lychrel seed     = 1;
INT lychrel related  = 2;
INT untested         = 3;
INT max number = 10 000;
[ max number ]INT lychrel;
FOR n TO UPB lychrel DO lychrel[ n ] := untested OD;
[ UPB lychrel ]BOOL palindromic;
FOR n TO UPB palindromic DO palindromic[ n ] := FALSE OD;
INT seed count       := 0;
INT related count    := 0;
INT palindrome count := 0;

# set the lychrel array to the values listed above                     #
FOR n TO UPB lychrel DO
    # classify this number                                             #
    set number( n );
    palindromic[ n ] := is palindromic;
    add reverse;
    REF AARRAY path  := INIT HEAP AARRAY;
    BOOL continue searching := TRUE;
    TO max additions WHILE continue searching :=
                     IF related CONTAINSKEY number[ : digits ] THEN
                         # this number is already known to be lychrel  #
                         lychrel[ n ] := lychrel related;
                         add related numbers( path );
                         related count +:= 1;
                         FALSE
                     ELIF is palindromic THEN
                         # have reached a palindrome                   #
                         lychrel[ n ] := not lychrel;
                         FALSE
                     ELSE
                         # not palindromic - try another iteration     #
                         path // number[ : digits ] := "Y";
                         add reverse;
                         TRUE
                     FI
    DO SKIP OD;
    IF continue searching THEN
        # we have a lychrel seed                                       #
        add related numbers( path );
        lychrel[ n ] := lychrel seed;
        seed count  +:= 1
    FI;
    IF palindromic[ n ] AND ( lychrel[ n ] = lychrel seed OR lychrel[ n ] = lychrel related ) THEN
        # have a palindromic related or seed lychrel number            #
        palindrome count +:= 1
    FI
OD;

# print the lychrel numbers up to max number                           #
print( ( "There are "
       , whole( seed count,    0 ), " seed Lychrel numbers and "
       , whole( related count, 0 ), " related Lychrel numbers up to "
       , whole( UPB lychrel,   0 )
       , newline
       )
     );
# shpw the seeds                                                       #
print( ( "Seed Lychrel numbers up to ", whole( UPB lychrel, 0 ), ":" ) );
FOR n TO UPB lychrel DO IF lychrel[ n ] = lychrel seed THEN print( ( " ", whole( n, 0 ) ) ) FI OD;
print( ( newline ) );
# show the Lychrel numbers that are palindromic                        #
print( ( "There are "
       , whole( palindrome count, 0 )
       , " palindromic Lychrel numbers up to "
       , whole( UPB lychrel, 0 )
       , ":"
       )
     );
FOR n TO UPB lychrel DO
    IF ( lychrel[ n ] = lychrel seed OR lychrel[ n ] = lychrel related ) AND palindromic[ n ] THEN print( ( " ", whole( n, 0 ) ) ) FI
OD;
print( ( newline ) )

```

{{out}}

```txt

There are 5 seed Lychrel numbers and 244 related Lychrel numbers up to 10000
Seed Lychrel numbers up to 10000: 196 879 1997 7059 9999
There are 3 palindromic Lychrel numbers up to 10000: 4994 8778 9999

```



## BASIC

{{works with|QB64|1.1}}
With minimum modifications, also:
{{works with|VB-DOS|1.0}}
{{works with|QB|4.5}}
{{works with|QBASIC|1.1}}
{{works with|PDS|7.1}}

I highly recommend to run this code in QB64. If this code is run in the other related versions, it can take about 20 minutes to run. In QB64 it only takes just seconds (11 seconds in my computer). This code has been tested to calculate up to 15000 numbers. BASIC doesn't have a way to manage such large numbers, but I implemented a way to do the sum through strings (an easy way to create a kind of array).

```qbasic

' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '
' Lychrel Numbers V1.0                              '
'                                                   '
' Developed by A. David Garza Marín in QB64 for     '
' RosettaCode. December 2, 2016.                    '
' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '

OPTION _EXPLICIT  ' Change to OPTION EXPLICIT in VB-DOS or PDS 7.1. Remove in QB and QBASIC

' SUBs and FUNCTIONs
DECLARE SUB doAddLychrel (WhichNumber AS LONG)
DECLARE SUB doAddRelated (WhichNumber AS LONG)
DECLARE SUB doFindLychrelPalyndromes ()
DECLARE FUNCTION CalculateMaxRes& (WhichNumber AS LONG)
DECLARE FUNCTION IsLychrel% (WhichNumber AS LONG)
DECLARE FUNCTION Reverse$ (WhatToReverse AS STRING)
DECLARE FUNCTION Sum$ (N1 AS STRING, N2 AS STRING)

' Var
DIM sN1 AS STRING, sN2 AS STRING, sLychrel AS STRING
DIM i AS LONG, iLychrel AS INTEGER, iCount AS INTEGER, l AS INTEGER
DIM c1 AS INTEGER, l1 AS INTEGER, c2 AS INTEGER, l2 AS INTEGER
DIM lMC AS LONG, lT AS LONG
DIM lLC AS LONG, lRL AS LONG, lPN AS LONG
CONST MaxNumbers = 10000, MaxIterations = 500, False = 0, True = NOT False

' Init
REDIM lLC(1, 0) AS LONG, lRL(0) AS LONG, lPN(0) AS LONG
lMC = CalculateMaxRes&(MaxNumbers)
lT = TIMER

' ------------------ Main program -----------------------------------------
CLS
PRINT "Lychrel Numbers 1.0"
PRINT
PRINT "Calculating numbers from 1 to"; MaxNumbers;
c1 = POS(1)
l1 = CSRLIN
COLOR 7 + 16: PRINT "...": PRINT
COLOR 7
c2 = POS(1)
l2 = CSRLIN
FOR i = 1 TO MaxNumbers
  sN1 = LTRIM$(STR$(i))
  sN2 = Reverse$(sN1)
  iCount = 0
  LOCATE l2, 1: PRINT "Analyzing number"; i;
  COLOR 7 + 16: PRINT "...": COLOR 7
  DO
    iCount = iCount + 1
    sN1 = Sum$(sN1, sN2)
    sN2 = Reverse$(sN1)
  LOOP UNTIL iCount >= MaxIterations OR sN1 = sN2

  IF sN1 <> sN2 THEN
    IF IsLychrel%(i) THEN
      doAddLychrel i
    ELSE
      doAddRelated i
    END IF
    PRINT "Potential Lychrel numbers:"; UBOUND(lLC, 2); "{";
    FOR l = 1 TO UBOUND(lLC, 2)
      PRINT lLC(1, l);
    NEXT
    PRINT "}"

    IF UBOUND(lRL) > 0 THEN
      PRINT "Kin Lychrel numbers found:"; UBOUND(lRL)
    END IF
  END IF
NEXT i

' Look for palyndromes
IF UBOUND(lLC, 2) > 0 THEN
  doFindLychrelPalyndromes
END IF

' Shows the results
CLS
PRINT "Lychrel numbers 1.0 in QB64"
PRINT
PRINT "Lychrel numbers found: "; UBOUND(lLC, 2)
PRINT "Lychrel numbers: {";
FOR i = 1 TO UBOUND(lLC, 2)
  PRINT lLC(1, i);
NEXT
PRINT "}"
PRINT
PRINT "Kin numbers found: "; UBOUND(lRL)
' You can uncomment the following lines if you want to see the
'   Kin or Related numbers found.
'PRINT "Kin numbers: {";
'FOR i = 1 TO UBOUND(lRL)
'  PRINT lRL(i);
'NEXT i
'PRINT "}"
PRINT
PRINT "Palyndrome Lychrel numbers found:"; UBOUND(lPN); "{";
FOR i = 1 TO UBOUND(lPN)
  PRINT lPN(i);
NEXT i
PRINT "}"
lT = TIMER - lT
PRINT
PRINT USING "Calculation took ##:## seconds."; FIX(lT / 60), (lT MOD 60)
PRINT "End of program."
' ------------------ End of Main program --------------------------------------
END

FUNCTION CalculateMaxRes& (WhichNumber AS LONG)
  ' Var
  IF (WhichNumber MOD 10) <> 0 THEN
    CalculateMaxRes& = WhichNumber + VAL(Reverse$(LTRIM$(STR$(WhichNumber))))
  ELSE
    CalculateMaxRes& = (WhichNumber - 1) + VAL(Reverse$(LTRIM$(STR$(WhichNumber - 1))))
  END IF
END FUNCTION

SUB doAddLychrel (WhichNumber AS LONG)
  ' Var
  DIM iMaxC AS INTEGER, iMaxR AS INTEGER
  DIM lRes AS LONG, iRow AS INTEGER
  DIM sN1 AS STRING, sN2 AS STRING
  DIM lNum(1 TO 10) AS LONG
  SHARED lLC() AS LONG, lMC AS LONG

  '
  lNum(1) = WhichNumber
  iRow = 1
  iMaxR = 10
  lRes = WhichNumber
  DO
    iRow = iRow + 1
    IF iRow > iMaxR THEN
      ' Change to REDIM PRESERVE for VB-DOS, QB, QBASIC and PDS 7.1
	  REDIM _PRESERVE lNum(1 TO iMaxR + 10) AS LONG 
    END IF
    sN1 = LTRIM$(STR$(lRes))
    sN2 = Reverse$(sN1)
    lRes = VAL(Sum$(sN1, sN2))
    lNum(iRow) = lRes
  LOOP UNTIL lRes > lMC
 
  ' Change to REDIM PRESERVE for VB-DOS, QB, QBASIC and PDS 7.1
  REDIM _PRESERVE lNum(1 TO iRow) AS LONG

  ' Now, Gathers the size of the lLC table
  iMaxC = UBOUND(lLC, 2) + 1
  IF iMaxC = 1 THEN
    ERASE lLC
    iMaxR = iRow
  ELSE
    iMaxR = UBOUND(lLC, 1)
  END IF

  IF iMaxC = 1 THEN
    REDIM lLC(1 TO iMaxR, 1 TO iMaxC) AS LONG
  ELSE
    ' Change to REDIM PRESERVE for VB-DOS, QB, QBASIC and PDS 7.1
	REDIM _PRESERVE lLC(1 TO iMaxR, 1 TO iMaxC) AS LONG
  END IF

  ' Assigns the result to the table
  FOR lRes = 1 TO iRow
    lLC(lRes, iMaxC) = lNum(lRes)
  NEXT lRes

  ERASE lNum
END SUB

SUB doAddRelated (WhichNumber AS LONG)
  ' Var
  DIM iMax AS INTEGER
  SHARED lRL() AS LONG

  iMax = UBOUND(lRL) + 1
  IF iMax = 1 THEN
    ERASE lRL
  END IF
  ' Change to REDIM PRESERVE for VB-DOS, QB, QBASIC and PDS 7.1
  REDIM _PRESERVE lRL(1 TO iMax) AS LONG
  lRL(iMax) = WhichNumber
END SUB

SUB doFindLychrelPalyndromes ()
  ' Var
  DIM iMaxC AS INTEGER, i AS INTEGER, iCount AS INTEGER
  DIM sN1 AS STRING, sN2 AS STRING
  SHARED lLC() AS LONG, lRL() AS LONG, lPN() AS LONG

  ' Verify seeds
  iMaxC = UBOUND(lLC, 2)
  FOR i = 1 TO iMaxC
    IF lLC(1, i) > 0 THEN
      sN1 = LTRIM$(STR$(lLC(1, i)))
      sN2 = Reverse$(sN1)
      IF sN1 = sN2 THEN
        iCount = iCount + 1
        GOSUB AddSpaceForItem
        lPN(iCount) = VAL(sN1)
      END IF
    END IF
  NEXT i

  ' Verify Kins
  iMaxC = UBOUND(lRL)
  FOR i = 1 TO iMaxC
    IF lRL(i) > 0 THEN
      sN1 = LTRIM$(STR$(lRL(i)))
      sN2 = Reverse$(sN1)
      IF sN1 = sN2 THEN
        iCount = iCount + 1
        GOSUB AddSpaceForItem
        lPN(iCount) = VAL(sN1)
      END IF
    END IF
  NEXT i

  ' Change to REDIM PRESERVE for VB-DOS, QB, QBASIC and PDS 7.1
  REDIM _PRESERVE lPN(1 TO iCount) AS LONG

  EXIT SUB

  AddSpaceForItem:
  IF UBOUND(lPN) < iCount THEN
    IF UBOUND(lPN) = 0 THEN
      ERASE lPN
    END IF
    ' Change to REDIM PRESERVE for VB-DOS, QB, QBASIC and PDS 7.1
	REDIM _PRESERVE lPN(1 TO iCount + 9) AS LONG
  END IF
  RETURN


END SUB

FUNCTION IsLychrel% (WhichNumber AS LONG)
  ' Var
  DIM iMaxC AS INTEGER, iMaxR AS INTEGER
  DIM iCol AS INTEGER, iRow AS INTEGER
  DIM lToCompare AS LONG
  DIM YesItIs AS INTEGER
  DIM sN1 AS STRING, sN2 AS STRING
  SHARED lLC() AS LONG, lMC AS LONG

  iMaxC = UBOUND(lLC, 2)
  iMaxR = UBOUND(lLC, 1)
  lToCompare = WhichNumber
  IF iMaxC > 0 THEN
    DO
      sN1 = LTRIM$(STR$(lToCompare))
      sN2 = Reverse$(sN1)
      lToCompare = VAL(Sum$(sN1, sN2))
      iCol = 0
      DO
        iCol = iCol + 1
        iRow = 0
        DO
          iRow = iRow + 1
        LOOP UNTIL iRow = iMaxR OR lToCompare = lLC(iRow, iCol)
      LOOP UNTIL iCol = iMaxC OR lToCompare = lLC(iRow, iCol)
    LOOP UNTIL lToCompare >= lMC OR lToCompare = lLC(iRow, iCol)
    YesItIs = (lToCompare <> lLC(iRow, iCol))
  ELSE
    YesItIs = True
  END IF

  IsLychrel = YesItIs

END FUNCTION

FUNCTION Reverse$ (WhatToReverse AS STRING)
  ' Var
  DIM sChar AS STRING
  DIM sRes AS STRING
  DIM i AS INTEGER, l AS INTEGER

  l = LEN(WhatToReverse)
  sRes = ""
  FOR i = 1 TO l
    sRes = MID$(WhatToReverse, i, 1) + sRes
  NEXT i

  Reverse$ = sRes
END FUNCTION

FUNCTION Sum$ (N1 AS STRING, N2 AS STRING)
  ' Var
  DIM iN1 AS INTEGER, iN2 AS INTEGER, iSum AS INTEGER
  DIM i AS INTEGER, l AS INTEGER, iCarry AS INTEGER, lM AS LONG
  DIM sRes AS STRING

  IF LEN(N1) > LEN(N2) THEN
    l = LEN(N1)
  ELSE
    l = LEN(N2)
  END IF
  lM = 2147483647 / 2

  ' Add trailing zeroes (uncomment in case strings have not equal number of digits)
  ' N1 = STRING$(l - LEN(N1), 48) + N1
  ' N2 = STRING$(l - LEN(N2), 48) + N2

  ' Hace la suma
  IF VAL(N1) < lM THEN
    sRes = LTRIM$(STR$(VAL(N1) + VAL(N2)))
  ELSE

    FOR i = l TO 1 STEP -1
      iN1 = VAL(MID$(N1, i, 1))
      iN2 = VAL(MID$(N2, i, 1))

      iSum = iN1 + iN2 + iCarry

      iCarry = FIX(iSum / 10)
      iSum = iSum MOD 10

      sRes = LTRIM$(STR$(iSum)) + sRes
    NEXT i
    IF iCarry > 0 THEN sRes = LTRIM$(STR$(iCarry)) + sRes
  END IF

  Sum$ = sRes

END FUNCTION

```


Output while running:

```txt

Lychrel Numbers 1.0

Calculating numbers from 1 to 10000 ...

Analyzing number 4764 ...
Potential Lychrel numbers: 3 { 196  879  1997  }
Kin Lychrel numbers found: 64

```


Output when finished:


```txt

Lychrel numbers 1.0 in QB64

Lychrel numbers found: 5
Lychrel numbers: { 196  879  1997  7059  9999 }

Kin numbers found: 244

Palyndrome Lychrel numbers found: 3 { 9999  4994  8778 }

Calculation took  0:11 seconds.
End of program.

```



## Clojure


```lisp
(ns lychrel.core
  (require [clojure.string :as s])
  (require [clojure.set :as set-op])
  (:gen-class))

(defn palindrome? "Returns true if given number is a palindrome (number on base 10)"
  [number]
  (let [number-str (str number)]
    (= number-str (s/reverse number-str))))

(defn delete-leading-zeros
  "Delete leading zeros so that you can read the string"
  [number-str]
  (read-string (re-find (re-pattern "[1-9]\\d*") number-str))
  )

(defn lychrel "Returns T if number is a candidate Lychrel (up to max iterations), and a second value with the sequence of sums"
  ([number] (lychrel number 500))
  ([number depth]
   (let [next-number (+' number (delete-leading-zeros (s/reverse (str number))))
         depth (- depth 1)]
     (if (palindrome? next-number) (conj [next-number] number)
                                   (if (not= depth 0) (conj (lychrel next-number depth) number) (conj [] nil))
                                   )
     )))

(defn lychrel? "Test if number is a possible lychrel number"
  [number]
  (= nil (first (lychrel number 500))))

(defn lychrel-up-to-n "Get all lychrels up to N"
  [N]
  (filter lychrel? (range 1 N)))

(defn make-kin "Removes the starting number of the list, the starting number"
  [kin]
  (rest (butlast kin)))

(defn calc-seed "The seeding" []
  (let [kin-set (atom #{})
        seed-set (atom #{})]
    (fn [n] (let [lychrel-seed (set #{(last n)})
                  kins (set (butlast n))]
              (if (= kins (clojure.set/difference kins @kin-set))
                (do (swap! kin-set clojure.set/union kins)
                    (swap! seed-set clojure.set/union lychrel-seed)
                    @kin-set))
              @seed-set
              ))))

(defn filter-seeds "Filtering the seed through the paths"
  []
  (let [calc-f (calc-seed)
        all-lychrels (for [lychrel-list (filter lychrel? (range 1 10000))]
                       (filter (fn [x] (> 1000001 x)) (rest (lychrel lychrel-list))))]
    (last (for [ll all-lychrels]
            (do (calc-f ll))))))

(defn -main
  "Here we do the three tasks:
      Get all possible Lychrel numbers up to 10000
      Count them
      Reduce all possible numbers to seed"

  [& args]
  (let [lychrels-n (filter-seeds)
        count-lychrels (count lychrels-n)
        related-n (- (count (filter lychrel? (range 1 10000))) count-lychrels)
        palindrom-n (filter palindrome? (filter lychrel? (range 1 10000)))
        count-palindromes (count palindrom-n)
        ]
    (println count-lychrels "Lychrel seeds:"  lychrels-n)
    (println related-n "Lychrel related.")
    (println count-palindromes "Lychrel palindromes found:" palindrom-n))
  )

```


{{out}}

```txt
>(-main)
5 Lychrel seeds: #{7059 1997 196 879 9999}
244 Lychrel related.
3 Lychrel palindromes found: (4994 8778 9999)
=> nil

```



## Common Lisp


```lisp
(defun Lychrel (number &optional (max 500))
 "Returns T if number is a candidate Lychrel (up to max iterations), and a second value with the sequence of sums"
  (do* ((n number (+ n (parse-integer rev-str)))
        (n-str (write-to-string n) (write-to-string n))
        (rev-str (reverse n-str) (reverse n-str))
        (i 0 (1+ i))
        (list (list n) (cons n list)) )
    ((or (> i max) (and (string= n-str rev-str) (> i 0))) (values (not (string= n-str rev-str)) (nreverse list)))))


(defun Lychrel-test (n &optional (max 500))
 "Tests the first n numbers up to max number of iterations"
  (let ((seeds nil)
        (related 0)
        (palyndromes nil) )
    (dotimes (i (1+ n))
      (multiple-value-bind (Lychrel-p seq) (Lychrel i max)
        (when Lychrel-p
          (if (find seq seeds :test #'intersection :key #'cdr)
            (incf related)
            (push (cons i seq) seeds) )
          (when (= i (parse-integer (reverse (write-to-string i))))
            (push i palyndromes) ))))
    (format T "Testing numbers: 1 to ~D~%Iteration maximum: ~D~%~%Number of Lychrel seeds found: ~d => ~a~%~
    Number of related found: ~D~%Palyndrome Lychrel numbers: ~D => ~a~%"
    n max (length seeds) (nreverse (mapcar #'car seeds)) related (length palyndromes) (nreverse palyndromes)) ))

```


{{out}}

```txt
>(lychrel-test 10000)
Testing numbers: 1 to 10000
Iteration maximum: 500

Number of Lychrel seeds found: 5 => (196 879 1997 7059 9999)
Number of related found: 244
Palyndrome Lychrel numbers: 3 => (4994 8778 9999)
NIL

```



## D

{{trans|Python}}

```d
import std.stdio, std.conv, std.range, std.typecons, std.bigInt;

auto rev(in BigInt n) {
    return n.text.retro.text.BigInt;
}

alias Res = Tuple!(bool, BigInt);

Res lychrel(BigInt n) {
    static Res[BigInt] cache;
    if (n in cache)
        return cache[n];
 
    auto r = n.rev;
    auto res = Res(true, n);
    immutable(BigInt)[] seen;
    foreach (immutable i; 0 .. 1_000) {
        n += r;
        r = n.rev;
        if (n == r) {
            res = Res(false, BigInt(0));
            break;
        }
        if (n in cache) {
            res = cache[n];
            break;
        }
        seen ~= n;
    }
    
    foreach (x; seen)
        cache[x] = res;
    return res;
}

void main() {
    BigInt[] seeds, related, palin;

    foreach (immutable i; BigInt(1) .. BigInt(10_000)) { // 1_000_000
        const tf_s = i.lychrel;
        if (!tf_s[0])
            continue;
        (i == tf_s[1] ? seeds : related) ~= i;
        if (i == i.rev)
            palin ~= i;
    }
    
    writeln(seeds.length, " Lychrel seeds: ", seeds);
    writeln(related.length, " Lychrel related");
    writeln(palin.length, " Lychrel palindromes: ", palin);
}
```

{{out}}

```txt
5 Lychrel seeds: [196, 879, 1997, 7059, 9999]
244 Lychrel related
3 Lychrel palindromes: [4994, 8778, 9999]
```

Almost two times slower than the Python version with LDC2, probably because of the lower efficiency of the big integers.


## Fortran

Fortran lacks built-in support for arbitrary-precision arithmetic, but fortunately, long addition is easily implemented. Digit strings are preferable anyway, as with the numbers in normal variables, there would be a lot of struggle in extracting the base ten digits - unless one was working on a base ten computer such as the IBM1620. A convenient approach is to have an array to hold the digits, with element zero having the count of digits in use. Alas, if one tries to use small digits (such as INTEGER*1) a count cannot exceed +127 and one soon finds that this problem needs longer digit strings. One could convert to using INTEGER*2, but instead the latter-day feature of defining a "type" can be employed to create a compound entity with a large integer for the count and still INTEGER*1 for the digits. An alternative for the digits would be to use a CHARACTER array, and rely on functions CHAR and ICHAR to get at the numerical values.

Arithmetic on BIGINT values is easy when only addition is needed, and there might arise special properties, for instance, imagine equivalencing the digits to a sequence of 64-bit integers and performing the addition 64-bits at a time and thus eight digits in each blow! Alas, alignment will be a problem as a digit string will not often be a multiple of eight in length and the requirement to access the digits from both ends of the string exacerbates the problem. On the other hand, one-byte integers are all very well, but the cpu will have to struggle to isolate each digit when accessing memory, as with 32-bit words or even 64-bit words. But that's a problem for the cpu. Here, the method has two parts: first is to perform the addition of all the digits in one go via a FOR ALL statement, and who knows, perhaps it will all happen in parallel. Second is a fixup pass, to spread the carries appropriately. This will involve multiple passes through the array memory so it might be better to employ a single pass with proper attention to carries: a complex loop (whose code will be in the cpu's on-chip memory) that makes lesser demands on memory transfer via one pass only. 

So the plan is to have an array STASH of BIGINT elements, and auxiliary arrays that finger elements in this stash. These arrays are needed to form the various lists of numbers of one sort or another that will be kept, and some searched. Such lists can have their elements juggled easily (being simple integers) as opposed to moving BIGINT elements about. It turns out that most numbers will be stored for one list or another, so the initial version did not escalate to a scheme for deleting numbers from the STASH so as to save storage. Later however I realised that with numbers being tested incrementally, and the ADVANCE to the next step always producing a larger number, the starting value for a march need not be stored because it will never again appear. However, sometimes the initial value ''is'' to be stored in a list (because a Long March happens to start with a palindromic number and such are to be noted, in L0P) so a fuller scheme was developed. Since usage is generally simple, there was no need for a "usecount" entry for each item when considering a "free" operation, just an AVAILS list of unused STASH entries. Not storing the start value greatly reduced the size of the PLIST (those numbers leading to a palindrome) and also removed the problem of multiple entries with equal values. The LLIST (numbers leading to a Long March) contained only 2,500 entries (500 from each of five discovered Lychrel numbers, not including their starting values) plus just four more, from numbers joining a Long March on their ''second'' step. After the second step, no value was found in either the LLIST or PLIST - the values involved soon exceeded the upper bound of 10000 for the search. And, once the values have escalated to say sixty digits (or more), a few hundred special values in a list are very unlikely to be stepped upon.

In the event, after reaching 10,000, of the 2,504 values in list LYCHREL known to lead to a long march, just a further nine were less than 20,000, the twenty-first was 109,989 and the twenty-ninth was 1,067,869. Of those 652 known to lead to a palindrome, 213 were less than 10,000 (and so will never be referred to again) and entry 385 was 19,888. Extending the inspection to 20,000 extended 652 to 3,940 such that entry 428 was 20,091. There were now 42 Lychrel potentials with 21,070 waypoints stored but only 15 were less than 20,000. These lists could be purged of their no-longer needed early entries, say after every thousand inspections, but the LYCHREL list's growth is the problem. Its numerous very large numbers (now up to 224 digits) are very unlikely to be stepped upon, but even so, the joiners of a Long March increased from 244 to 662. Whereas in the first ten thousand, all such latecomers to a Long March were discovered by their second step, 10,653 is found on its ''fourth'' step to be 903,408, and 14,995 attained 182,353,171 on step eight. Thus, trimming the not-so-many early values won't reduce the storage demand by much, and of the more numerous larger values, one might, just might, be stepped upon again thus declaring a sequence's start to be not a "seed" value...

The service routines are not presented with an index to the STASH, but an actual BIGINT as the parameter, thereby enabling dereferencing. That is, for access via the STASH, a number's digit is accessed with two indices: STASH(i).DIGIT(j) but if a BIGINT is passed as a parameter, its digits may be accessed via one dimension only, often a significant saving. Though not if parameters are passed via copy-in, copy-out! Some languages, such as Pascal have a "with" statement, which achieves dereferencing somewhat as follows: <code>With Stash[i] do ... Digit[j] ...;</code> so that within its scope, all mentions of Digit are implicitly Stash[i].Digit and who knows, the compiler might produce good code as well.

Early trial runs showed that a list may have thousands of numbers, and many linear searches of so many can be expected to be slow - indeed, a pause in the production of results was apparent. So, a binary search. But this requires a sorted list. The Combsort works well, but greatly slowed execution because the complete sorting of a list takes time when it is done often. Fortunately, most additions to the lists are in increasing numerical order, so Insertionsort proved far better - though in the event the speed was about the same as for a linear search with no sorting. Ah well. Otherwise, one would be stuck with having a sorted area and an overflow area (unsorted) for a compound search, with a full sort whenever the overflow area becomes big enough. Or, escalating to a hash-based scheme. Even so, this encouraged a second try at speeding the calculation. The numbers of a march all being increasing means that an ordered collection of values is to hand and so, lists of such numbers can be kept in sorted order without having to be resorted. In the event, typically just a few (often just one) value is being added, or occasionally, the five hundred values of a long march. So, for the few, add them one at a time by using a variant of the Insertionsort, where the location for insertion is determined by a binary search thus greatly reducing the number of calls to ORDER. Further, the shifting of entries to make room can be done in one array-assignment statement, rather than stepwise. For the many, a merge process is the obvious scheme. With these improvements, the results appeared without delay, as fast as the screen could scroll.

But at the cost of added effort in programming. As distinct from languages already offering associative arrays, lists and their manipulation as built-in facilities.

The growth of the numbers was impressive, so a simple investigation: what of the growth per step for 196? The digit strings lengthened in a linear-looking way, so, what of a plot of step as x, vs Log10(n) as y? The result for 500 numbers went from step 0 and Log10(196) = 2·292256 to step 499 and 215·266386 and the linear correlation coefficient came out as 1·000 with y = 0·43328*x + 2·1616, which is to say that each advance increased the number by a factor of about 2·7. However, the plot (which alas, can't be posted here) does ''not'' show a random scatter about the straight line, it instead shows the points wobbling above and below the fitted line. A plot of the residuals shows the deviations coming in waves, but also cannot be posted here...


```Fortran

      SUBROUTINE CROAK(GASP)	!A dying message.
       CHARACTER*(*) GASP	!The message.
        WRITE (6,*) "Oh dear! ",GASP	!The gasp.
        STOP "Alas."		!The death.
      END			!Goodbye, cruel world.

      MODULE LYCHREL SEARCH	!Assistants for the pursuit of Lychrel numbers.
       INTEGER LOTSADIGITS,BASE	!Parameters.
       PARAMETER (LOTSADIGITS = 1000, BASE = 10)	!This should do.
       TYPE BIGINT	!Can't use an element zero as the count, as lots of digits are needed.
        INTEGER LDIGIT	!Count in use.
        INTEGER*1 DIGIT(LOTSADIGITS)	!Stored in increasing powers of BASE.
       END TYPE BIGINT		!No fractional parts.
       INTEGER MSTASH		!Now for my stash.
       PARAMETER (MSTASH = 66666)	!This should be enough.
       TYPE(BIGINT) STASH(MSTASH)	!The work area.
       INTEGER AVAILS(0:MSTASH)		!A list of available STASH entries.
       INTEGER PLIST(0:MSTASH)	!These strings of fingers
       INTEGER LLIST(0:MSTASH)	!Each starting with a count
       INTEGER LYCHREL(0:MSTASH)!Finger BIGINTs in the STASH
       INTEGER L0P(0:MSTASH)	!Without the body of the BIGINT being copied.
       INTEGER LONGESTNUMBER	!Keep track out of curiosity.
       DATA LONGESTNUMBER/0/	!No digits so far.
       CONTAINS		!A miscellany.
Commence with some STASH service. If problems were to arise, better error messages would be in order.
        SUBROUTINE PREPARESTASH	!Since fancy usage is involved, fancy initialisation is needed.
         INTEGER I	!A stepper.
          AVAILS(0) = MSTASH	!All are available.
          FORALL (I = 1:MSTASH) AVAILS(I) = MSTASH + 1 - I	!Will be used stack style.
          STASH.LDIGIT = -666				!This will cause trouble!
        END SUBROUTINE PREPARESTASH	!Simple enough.
        SUBROUTINE GRABSTASH(X)	!Finger an available STASH.
         INTEGER X		!The finger.
         INTEGER L		!The last.
          L = AVAILS(0)		!Pick on the last in the list.
          IF (L .LE. 0) CALL CROAK("Run out of stashes!")	!Hopefully not.
          X = AVAILS(L)		!Select some element or other.
          AVAILS(L) = 0		!Might as well unfinger.
          AVAILS(0) = L - 1	!As well as drop off the list.
        END SUBROUTINE GRABSTASH!Can't be bothered making this a function. Sudden death instead.
        SUBROUTINE FREESTASH(X)	!Unhand a STASH.
         INTEGER X		!The finger.
          IF (X.EQ.0) RETURN	!A non-finger.
          IF (X.LT.0) CALL CROAK("Unhanding a non-stash!")	!Paranoia.
          IF (X.GT.MSTASH) CALL CROAK("Not a stash!")		!I don't expect any of these.
          IF (AVAILS(0).GE.MSTASH) CALL CROAK("Over subscribed!")	!But on principle...
          AVAILS(0) = AVAILS(0) + 1	!So, append to the list of available entries.
          AVAILS(AVAILS(0)) = X		!Thus.
          X = 0				!The finger is amputated.
        END SUBROUTINE FREESTASH	!Last out, first in. Etc.

        SUBROUTINE BIGWRITE(T,N)!Reveal an annotated bignumber.
         CHARACTER*(*) T	!The text of its name.
         TYPE(BIGINT) N		!The value of its name.
          WRITE (6,1) T,N.LDIGIT,N.DIGIT(N.LDIGIT:1:-1)	!Roll!
    1     FORMAT (A,"(1:",I0,") = ",(300I1))	!This should do.
        END SUBROUTINE BIGWRITE	!Perhaps bigger than Integer*8.

        SUBROUTINE SHOWLIST(BLAH,LIST)	!One can become confused.
         INTEGER LIST(0:MSTASH)	!Explicit bounds prevents confusion.
         CHARACTER*(*) BLAH	!An identifying message.
         CHARACTER*4 ZOT	!Scratchpad.
         INTEGER I,IT		!Stepper.
c         REAL*8 V,P		!For logarithmic output.
c         INTEGER J		!Another stepper needed.
          WRITE (6,1) BLAH,LIST(0)		!A heading.
    1     FORMAT ("The count for ",A," is ",I0)	!Ah, layout.
          DO I = 1,LIST(0)		!Work through the list.
            WRITE(ZOT,"('#',I3)") I	!Prepare an annotation.
            IT = LIST(I)		!Finger the stash.
            CALL BIGWRITE(ZOT,STASH(IT))	!Show.
c            V = 0		!Convert the BIGINT to a floating-point number.
c            P = 1		!Tracking the powers of BASE, presumed ten.
c         PP:DO J = STASH(IT).LDIGIT,1,-1	!Start with the highest power.
c              V = V + STASH(IT).DIGIT(J)/P	!Deem it a units digit.
c              P = P*10				!The next digit will have a lesser power.
c              IF (P.GT.1000000) EXIT PP		!This will do.
c            END DO PP				!On to the next digit.
c            V = STASH(IT).LDIGIT - 1 + LOG10(V)	!Convert. LOG10(196) = 2.29225607135648
c            WRITE (6,*) I,V			!Reveal.
          END DO			!On to the next.
        END SUBROUTINE SHOWLIST	!Adrift in indirection.

        SUBROUTINE BIGLOAD(A,NUM)	!Shatters a proper number into its digits.
Can't overflow, because the biggest normal integer has far fewer than LOTSADIGITS digits.
         TYPE(BIGINT) A		!The bignumber.
         INTEGER NUM		!The normal integer to put in it.
         INTEGER L,N		!Assistants.
          N = NUM		!A copy that I can damage.
          A.DIGIT = 0		!Scrub, against a future carry..
          L = 0			!No digits so far.
          DO WHILE(N .GT. 0)	!Some number remaining?
            L = L + 1			!Yes. Count another digit.
            A.DIGIT(L) = MOD(N,BASE)	!Place it.
            N = N/BASE			!Reduce the number.
          END DO		!And try again.
          A.LDIGIT = MAX(1,L)	!Zero will have one digit, a zero.
        END SUBROUTINE BIGLOAD	!A small service.

Continue with routines for doing the work.
        SUBROUTINE ADVANCE(A,B)	!Advance A, giving B.
C Experiment shows that for 196, log10(v) = 0·43328*step + 2·1616, or, each advance increases by a factor of ~2·7.
         TYPE(BIGINT) A,B	!To be twiddled.
         INTEGER D		!An assistant for carrying.
         INTEGER I		!A stepper.
          B.LDIGIT = A.LDIGIT		!Same size, so far.
          FORALL(I = 1:A.LDIGIT)	!Do these in any order, even in parallel!
     1     B.DIGIT(I) = A.DIGIT(I)			!Add each digit
     2                + A.DIGIT(A.LDIGIT - I + 1)	! to its other-end partner.
          B.DIGIT(B.LDIGIT + 1) = 0	!Perhaps another digit will be needed shortly.
          DO I = 1,B.LDIGIT		!Now slow down and taste the carry.
            D = B.DIGIT(I) - BASE	!Allowable digits are 0:BASE - 1.
            IF (D.GE.0) THEN		!So, has this digit overflowed?
              B.DIGIT(I) = D			!Yes. Only addition, so no div and mod stuff.
              B.DIGIT(I + 1) = B.DIGIT(I + 1) + 1	!Carry one up, corresponding to subtracting one BASE down.
            END IF			!So much for that digit.
          END DO		!On to the next digit, of higher power.
          IF (D.GE.0) THEN	!A carry from the highest digit?
            IF (B.LDIGIT .GE. LOTSADIGITS) CALL CROAK("Overflow!")	!Oh dear.
            B.LDIGIT = B.LDIGIT + 1	!NB! Always there is room left for ONE more digit.
            LONGESTNUMBER = MAX(B.LDIGIT,LONGESTNUMBER)	!Perhaps a surprise awaits.
          END IF		!Avoids overflow testing for every carry above.
        END SUBROUTINE ADVANCE	!That was fun!

        LOGICAL FUNCTION PALINDROME(N)	!Perhaps a surprise property?
Calls a one-digit number palindromic through the execution of vacuous logic.
         TYPE(BIGINT) N	!The big number to inspect.
          PALINDROME = ALL(N.DIGIT(1:N.LDIGIT/2)	!Compare each digit in the first half...
     1                 .EQ.N.DIGIT(N.LDIGIT:N.LDIGIT/2 + 1:-1))	!To its opposite digit in the second. Whee!
        END FUNCTION PALINDROME	!If an odd number of digits, ignores the middle digit.

        INTEGER FUNCTION ORDER(A,B)	!Does B follow A?
         TYPE(BIGINT) A,B	!The two numbers.
         INTEGER I		!A stepper.
          IF (A.LDIGIT - B.LDIGIT) 1,10,2	!First, compare the lengths.
    1     ORDER = +1		!B has more digits.
         RETURN			!So, A must be smaller: in order.
    2     ORDER = -1		!A has more digits.
         RETURN			!So B must be smaller: reverse order.
Compare the digits of the two numbers, known to be of equal length.
   10     DO 11 I = A.LDIGIT,1,-1	!The last digit has the highest power.
            IF (A.DIGIT(I) - B.DIGIT(I)) 1,11,2	!Compare the digits.
   11     CONTINUE		!If they match, continue with the next digit.
          ORDER = 0		!If all match, A = B.
        END FUNCTION ORDER	!Ah, three-way tests...

        SUBROUTINE COMBSORT(XNDX)	!Sort according to STASH(XNDX)) by reordering XNDX.
Crank up a Comb sort of array STASH as indexed by XNDX.
         INTEGER XNDX(0:),T	!The index to be rearranged.
         INTEGER I,H		!Tools. H ought not be a small integer.
         LOGICAL CURSE		!Annoyance.
          H = XNDX(0) - 1	!Last - First, and not +1.
          IF (H.LE.0) GO TO 999	!Ha ha.
    1     H = MAX(1,H*10/13)	!The special feature.
          IF (H.EQ.9 .OR. H.EQ.10) H = 11	!A twiddle.
          CURSE = .FALSE.		!So far, so good.
          DO I = XNDX(0) - H,1,-1	!If H = 1, this is a BubbleSort.
            IF (ORDER(STASH(XNDX(I)),STASH(XNDX(I + H))) .LT. 0) THEN	!One compare.
              T=XNDX(I); XNDX(I)=XNDX(I+H); XNDX(I+H)=T		!One swap.
              CURSE = .TRUE.				!One curse.
            END IF				!One test.
          END DO			!One loop.
          IF (CURSE .OR. H.GT.1) GO TO 1!Work remains?
  999    RETURN			!If not, we're done.
        END SUBROUTINE COMBSORT	!Good performance, and simple.

        SUBROUTINE INSERTIONSORT(XNDX)	!Adjust XNDX according to STASH(XNDX)
Crank up an Insertion sort of array STASH as indexed by XNDX.
         INTEGER XNDX(0:),IT	!The index to be prepared, and a safe place.
         INTEGER I,L		!Fingers.
          DO L = 2,XNDX(0)	!Step along the array.
            IF (ORDER(STASH(XNDX(L - 1)),STASH(XNDX(L))) .LT. 0) THEN	!Disorder?
              I = L		!Yes. Element L belongs earlier.
              IT = XNDX(L)	!Save it so that others can be shifted up one.
    1         XNDX(I) = XNDX(I - 1)	!Shift one.
              I = I - 1		!The next candidate back.
              IF (I.GT.1 .AND. ORDER(STASH(XNDX(I - 1)),	!Do I have to go further back?
     1                               STASH(IT)).LT.0) GO TO 1	!Yes.
              XNDX(I) = IT	!Done. Place it in the space created.
            END IF		!So much for that comparison.
          END DO		!On to the next.
        END SUBROUTINE INSERTIONSORT	!Swift only if the array is nearly in order.

        INTEGER FUNCTION FOUNDIN(XNDX,X)	!Search STASH(XNDX) for X.
Crank up a binary serach. This uses EXCLUSIVE bounds.
         INTEGER XNDX(0:)	!The list of elements.
         TYPE(BIGINT) X		!The value to be sought.
         INTEGER L,P,R		!Fingers for the binary search.
          L = 0			!Establish outer bounds.
          R = XNDX(0) + 1	!One before, and one after, the first and last.
    1     P = (R - L)/2		!Probe point offset. Beware integer overflow with (L + R)/2.
          IF (P.LE.0) THEN	!Is the search span exhausted?
            FOUNDIN = -L	!Alas. X should follow position L but doesn't.
           RETURN		!Nothing more to search.
          END IF		!Otherwise, STASH(XNDX(L + 1)) <= X <= STASH(XNDX(R - 1))
          P = L + P		!Convert from offset to probe point.
          IF (ORDER(X,STASH(XNDX(P)))) 2,4,3	!Compare X to the probe point's value.
    2     L = P			!STASH(XNDX(P)) < X: advance L to P.
          GO TO 1		!Try again.
    3     R = P			!X < STASH(XNDX(P)): retract R to P.
          GO TO 1		!Try again.
Caught it! X = STASH(XNDX(P)) - the result indexes XNDX, not STASH.
    4     FOUNDIN = P		!So, X is found, here! *Not* at P, but at XNDX(P).
        END FUNCTION FOUNDIN	!Hopefully, without array copy-in, copy-out.

        SUBROUTINE INSPECT(S1,S2,ENUFF)	!Follow the Lychrel protocol.
Careful! A march is stopped on encountering a palindrome, BUT, the starting value is not itself checked.
         INTEGER S1,S2		!Start and stop values.
         INTEGER ENUFF		!An infinite trail can't be followed.
         INTEGER STEP,SEED	!Counts the advances along the long march.
         INTEGER START		!The first value of a march has special treatment.
         INTEGER MARCH(0:ENUFF)	!The waypoints of a long march.
         INTEGER DEJAVU		!Some may prove to be junctions.
         INTEGER LP,NP,LL,NL	!Counters for various odd outcomes.
          NP = 0		!No palindromic stops.
          LP = 0		!Nor any that head for one.
          NL = 0		!No neverending sequences seen.
          LL = 0		!Nor any that head for one.
          WRITE (6,10) S1,S2,ENUFF	!Announce the plan.
   10     FORMAT ("Starting values ",I0," to ",I0,"; step limit ",I0)
C   For each try, steps (but not the START) are saved in MARCH, and those steps end up in one list or another.
C Thus, there is no removal of their entries from the working STASH. If a plaindrome is found, or a step's value is
c noticed in the PLIST or LYCHREL list, the last STEP is not saved (being in that list, as a different entry in STASH)
c and its redundant value in STASH is unhanded via CALL FREESTASH(MARCH(STEP)).
c But if instead the MARCH is to be added to the LYCHREL list, the last value is included.
c   Since tries are made in increasing order, and ADVANCE always produces a bigger number, there is no point
c in saving the START value in STASH, except, some START values turn out to be notable and being saved in L0P
c or LLIST, their entry in STASH must not be rugpulled, thus the START = 0 to note this and prevent FREESTASH.
      TRY:DO SEED = S1,S2	!Here we go.
            CALL GRABSTASH(START)		!Ask for a starting space.
            CALL BIGLOAD(STASH(START),SEED)	!The starting value.
c            CALL BIGWRITE("N0",STASH(START))	!Show it.
c            IF (FOUNDIN(LYCHREL,STASH(START)) .GT. 0) THEN!	Have we been here during a long march?
c              WRITE (6,*) SEED,"Falls in!"		!Yes!
c              CYCLE TRY				!Nothing will be learnt by continuing.
c            END IF				!Otherwise, we're not discouraged.
            STEP = 1				!Even the longest journey stars with its first step.
            MARCH(0) = STEP			!And I'm remembering every step.
            CALL GRABSTASH(MARCH(STEP))		!A place for my first footfall.
            CALL ADVANCE(STASH(START),STASH(MARCH(STEP)))	!Start the stomping.
Contemplate the current step.
  100       CONTINUE	!Can't label a blank line...
c            CALL BIGWRITE("N1",STASH(MARCH(STEP)))	!Progress may be of interest.
            DEJAVU = FOUNDIN(PLIST,STASH(MARCH(STEP)))	!A value known to later become a palindrome?
            IF (DEJAVU .GT. 0) THEN			!It being amongst those stashed for that cause.
c              WRITE (6,*) SEED,STEP,"Deja vu! List#",DEJAVU
              LP = LP + 1				!Count a late starter.
              CALL FREESTASH(MARCH(STEP))		!The last step is already known.
              CALL SLURP(PLIST)				!Add the MARCH to the palindrome starters.
              GO TO 110					!And we're finished with this try.
            END IF					!So much for prospective palindromes.
            IF (PALINDROME(STASH(MARCH(STEP)))) THEN	!Attained an actual palindrome?
c              WRITE (6,*) SEED,STEP,"Palindrome!"	!Yes!
              NP = NP + 1				!Count a proper palendrome.
              CALL FREESTASH(MARCH(STEP))		!The last step is a palindrome.
              CALL SLURP(PLIST)				!So remember only those non-palindromes before it.
              GO TO 110					!This is a pretext for ending.
            END IF					!Since one could advance past a palindrome, and then find another.
            DEJAVU = FOUNDIN(LYCHREL,STASH(MARCH(STEP)))	!A value known to later pass ENUFF?
            IF (DEJAVU .GT. 0) THEN			!If so, there is no need to follow that march again.
c              WRITE (6,*) SEED,STEP,"Latecomer! List#",DEJAVU	!So this starter has been preempted.
c              CALL BIGWRITE("Latecomer!",STASH(LYCHREL(DEJAVU)))	!Or, STASH(MARCH(STEP)), they being equal.
              LLIST(0) = LLIST(0) + 1			!Count another latecomer.
              LLIST(LLIST(0)) = START			!This was its starting value's finger.
              IF (PALINDROME(STASH(START))) THEN	!Perhaps its starting value was also already a palindrome?
                L0P(0) = L0P(0) + 1				!Yes! Count another such.
                L0P(L0P(0)) = START				!Using a finger, rather than copying a BIGINT.
              END IF					!A Lychrel number whose zeroth step is palindromic.
              START = 0					!This is not to be unfingered!
              LL = LL + 1				!Anyway, this path has joined a known long march.
              CALL FREESTASH(MARCH(STEP))		!This value is already fingered in a list.
              CALL SLURP(LYCHREL)			!So, all its steps do so also, including the last.
              GO TO 110					!Even though its later start might find a palindrome within ENUFF steps.
            END IF					!So much for discoveries at each step.
            IF (STEP.LT.ENUFF) THEN	!Are we there yet?
              STEP = STEP + 1			!It seems there is no reason to stop.
              MARCH(0) = STEP			!So, the long march extends.
              CALL GRABSTASH(MARCH(STEP))	!Another step.
              CALL ADVANCE(STASH(MARCH(STEP - 1)),STASH(MARCH(STEP)))	!Lurch forwards.
              GO TO 100				!And see what happens here.
            END IF			!The end of the loop.
Chase completed, having reached STEP = ENUFF with no decision.
            WRITE (6,*) SEED,"retains Lychrel potential."	!Since a palindrome has not been reached.
            IF (PALINDROME(STASH(START))) THEN	!Perhaps it started as a palindrome?
              L0P(0) = L0P(0) + 1			!It did!
              L0P(L0P(0)) = START			!So remember it.
              START = 0					!And don't unhand this entry, a finger to it being saved.
            END IF				!Enough shades of classification.
            NL = NL + 1			!Count another at the end of a long march.
            CALL SLURP(LYCHREL)		!And save the lot, including the final value.
  110       IF (START.GT.0) CALL FREESTASH(START)	!The starting value was not held as a part of the MARCH.
          END DO TRY		!Start another.
Cast forth a summary.
          WRITE (6,*)
          WRITE (6,11) NL,ENUFF,LL,LYCHREL(0)
   11     FORMAT (I8," starters attained step ",I0," and ",
     1     I0," joined a long march. "
     2     I0," waypoints stashed.")
c          CALL SHOWLIST("starters that joined a Long March",LLIST)
          CALL SHOWLIST("Long Marchers starting as palindromes",L0P)
          WRITE (6,12) NP,LP,PLIST(0)
   12     FORMAT (I8," starters ended as a palindrome, ",
     1     I0," joined a route leading to a palindrome. ",
     2     I0," waypoints stashed.")
         CONTAINS	!An assistant.
          SUBROUTINE SLURP(LIST)	!Adds the elements in MARCH to LIST.
C   Entries in LIST are such that STASH(LIST(~)) is ordered. Because I arrange this.
C   Entries MARCH are also in increasing order as they are successive values from ADVANCE.
C   Accordingly, if only a few entries are to be added, a binary search can be used to find the
C location for insertion, and then existing entries can be shifted up to make room for the new entry.
C This is the basic task of Insertionsort, however, this shift can be conducted without invoking ORDER
C to determine its bounds, as is done by Insertionsort, and ORDER takes a lot of time to decide.
C In other words, the binary search makes only a few calls to ORDER and then the moves follow, in one go,
C whereas the Insertionsort makes as many calls to ORDER as it makes moves, and does so stepwise.
C   When many entries are to be added, they could be appended to LIST and then COMBSORT invoked.
C This is infrequent as few numbers evoke a lengthy march, so that method is adequate.
C But since both lists are in order, a merge of the two lists would be more stylish, if encoded clunkily.
C   It turns out that the incoming set may finger a value that is already in LIST,
c so equal values may appear. With the binary search method, such incomers can simply be skipped,
c but with the merge it is a bit more messy. In the event, the new value is discarded and the
c old one retained.
C   For instance, 5 -> 10 -> 11 so fingers for 5 and 10 are added to PLIST.
C Later on, 10 -> 11 and a finger for 10 is to be added to PLIST, because the starting value is not checked.
C However, a later version refrains from adding the (unchecked) starting value, and then, no worries.
           INTEGER LIST(0:MSTASH)	!Some string of numbers.
           INTEGER MIST(0:MSTASH)	!LIST(0) is the upper bound, but risks stack overflow.
           INTEGER I,S,L		!A stepper.
Check for annoying nullities.
    1       IF (MARCH(0).LE.0) RETURN	!An empty march already!
            IF (MARCH(MARCH(0)).LE.0) THEN	!Otherwise, look at the last step.
              MARCH(0) = MARCH(0) - 1		!It was boring.
              GO TO 1				!So, go back one and look afresh.
            END IF			!Having snipped off trailing nullities, what remains means effort.
Can't escape some work.
            IF (MARCH(0) .LE. 6) THEN	!If we have a few only,
              DO I = 1,MARCH(0)		!Work through them one by one,
                S = MARCH(I)			!The finger for this step of the march.
                IF (S.LE.0) CYCLE		!A stump?
                L = FOUNDIN(LIST,STASH(S))	!Using a binary search to find the proper place.
                IF (L.GT.0) THEN		!If already present,
                  CALL FREESTASH(S)		!Its entry is no longer needed.
                  CYCLE				!So skip this.
                END IF				!But if not found, a place must be made.
                L = 1 - L			!Finger where the missing element should be.
                LIST(LIST(0) + 1:L + 1:-1) = LIST(LIST(0):L:-1)	!Shift followers up to make space.
                LIST(0) = LIST(0) + 1		!Count another.
                LIST(L) = S			!Place it.
              END DO			!On to the next.
             ELSE	!But if there are many to add, merge the two lists... Both are ordered.
              MIST(0:LIST(0)) = LIST(0:LIST(0))	!Copy the source list.
              LIST(0) = 0		!It is to be reformed by the merge.
              L = 1			!Start with the first in MIST.
              DO I = 1,MARCH(0)		!And work along the long MARCH.
                S = MARCH(I)		!So, a step from the long march.
                IF (S.LE.0) CYCLE	!And if not an empty step, we have one.
   11           LIST(0) = LIST(0) + 1		!Count in another for this list.
                IF (L.LE.MIST(0)) THEN		!Still have suppliers from what had been LIST?
                  IF (ORDER(STASH(MIST(L)),STASH(S))) 14,13,12	!Yes, Which is to be selected?
   12             LIST(LIST(0)) = MIST(L)	!STASH(MIST(L)) precedes STASH(S), so take it.
                  L = L + 1			!Advance to the next MIST entry.
                  GO TO 11			!And look again.
   13             CALL FREESTASH(S)		!Equal. Discard the new entry.
                  S = MIST(L)			!And pefer the established entry.
                  L = L + 1			!Advance past the MIST(L) entry.
                END IF			!Thus, MIST entries have been rolled until a MARCH entry was due.
   14           LIST(LIST(0)) = S	!Save the finger - which may have come from MIST...
              END DO			!On to the next MARCH.
              S = MIST(0) - L + 1	!The number of MIST entries still waiting.
              IF (S .GT. 0) THEN	!Are there any?
                LIST(LIST(0) + 1:LIST(0) + S) = MIST(L:MIST(0))	!Yes. Append them.
                LIST(0) = LIST(0) + S	!And count them in.
              END IF			!So much for the merge.
            END IF			!Otherwise, a COMBSORT would do.
          END SUBROUTINE SLURP	!Can't overflow, because each LIST has room for MSTASH entries.
        END SUBROUTINE INSPECT	!That was fun.
      END MODULE LYCHREL SEARCH	!Enough of that.

      PROGRAM TEST
      USE LYCHREL SEARCH
       CALL PREPARESTASH
Clear my lists.
       LYCHREL = 0	!No Lychrel candidates.
       LLIST = 0	!No latecomers to a Lychrel candidacy sequence.
       PLIST = 0	!No numbers leading to a palindrome.
       L0P = 0		!No Lychrel/latecomers starting as a palindrome.

       CALL INSPECT(1,10000,500)	!Whee!

       WRITE (6,*) "Longest digit string =",LONGESTNUMBER
       WRITE (6,*) "Unused STASH entries =",AVAILS(0)
      END

```


Output: edited to put twenty numbers per line and omit the (1:''n'') of the output from BIGWRITE...
Of the starters that joined a long march, six did so with their starting value (not tested for, just as the starting value is not tested to see if it is already a palindrome): they were 887, 1675, 1857, 7436, 9438, and 9988. Different long marches are involved: 887, 1675 and 7436 belong to 196 whereas 1857 and 9438 belong to 879, and 9438 belongs to 1997.

```txt

Starting values 1 to 10000; step limit 500
         196 retains Lychrel potential.
         879 retains Lychrel potential.
        1997 retains Lychrel potential.
        7059 retains Lychrel potential.
        9999 retains Lychrel potential.

       5 starters attained step 500 and 244 joined a long march. 2504 waypoints stashed.
The count for starters that joined a Long March is 244
  295,  394,  493,  592,  689,  691,  788,  790,  887,  978,  986, 1495, 1497, 1585, 1587, 1675, 1677, 1765, 1767, 1855,
 1857, 1945, 1947, 2494, 2496, 2584, 2586, 2674, 2676, 2764, 2766, 2854, 2856, 2944, 2946, 2996, 3493, 3495, 3583, 3585,
 3673, 3675, 3763, 3765, 3853, 3855, 3943, 3945, 3995, 4079, 4169, 4259, 4349, 4439, 4492, 4494, 4529, 4582, 4584, 4619,
 4672, 4674, 4709, 4762, 4764, 4799, 4852, 4854, 4889, 4942, 4944, 4979, 4994, 5078, 5168, 5258, 5348, 5438, 5491, 5493,
 5528, 5581, 5583, 5618, 5671, 5673, 5708, 5761, 5763, 5798, 5851, 5853, 5888, 5941, 5943, 5978, 5993, 6077, 6167, 6257,
 6347, 6437, 6490, 6492, 6527, 6580, 6582, 6617, 6670, 6672, 6707, 6760, 6762, 6797, 6850, 6852, 6887, 6940, 6942, 6977,
 6992, 7076, 7149, 7166, 7239, 7256, 7329, 7346, 7419, 7436, 7491, 7509, 7526, 7581, 7599, 7616, 7671, 7689, 7706, 7761,
 7779, 7796, 7851, 7869, 7886, 7941, 7959, 7976, 7991, 8058, 8075, 8079, 8089, 8148, 8165, 8169, 8179, 8238, 8255, 8259,
 8269, 8328, 8345, 8349, 8359, 8418, 8435, 8439, 8449, 8490, 8508, 8525, 8529, 8539, 8580, 8598, 8615, 8619, 8629, 8670,
 8688, 8705, 8709, 8719, 8760, 8778, 8795, 8799, 8809, 8850, 8868, 8885, 8889, 8899, 8940, 8958, 8975, 8979, 8989, 8990,
 9057, 9074, 9078, 9088, 9147, 9164, 9168, 9178, 9237, 9254, 9258, 9268, 9327, 9344, 9348, 9358, 9417, 9434, 9438, 9448,
 9507, 9524, 9528, 9538, 9597, 9614, 9618, 9628, 9687, 9704, 9708, 9718, 9777, 9794, 9798, 9808, 9867, 9884, 9888, 9898,
 9957, 9974, 9978, 9988
The count for Long Marchers starting as palindromes is 3
#  1(1:4) = 4994
#  2(1:4) = 8778
#  3(1:4) = 9999
    3044 starters ended as a palindrome, 6707 joined a route leading to a palindrome. 652 waypoints stashed.
 Longest digit string =         221
 Unused STASH entries =       63265

```



## FreeBASIC


```freebasic
' version 13-09-2015
' compile with: fbc -s console

' iteration limit
#Define max_it 500
' the highest number to be tested
#Define max_number_to_test 10000

Dim As String num, rev
Dim As String temp(), store()

Dim As Integer x, x1, palindrome
Dim As UInteger it, s, carry, sum, match, seed, related
Dim As UInteger num2test, p_count, lychrel_palindrome()

For num2test = 1 To max_number_to_test
    num = Str(num2test)
    rev = num : s = Len(num) - 1
    For x = 0 To s
        rev[s - x] = num[x]
    Next
    ' if num = rev then palindrome = -1 else palindrome = 0
    ' palindrome is set to the result of the compare of num and rev
    palindrome = (num = rev)
    it = 0
    ReDim temp(1 To max_it)
    Do
        carry = 0
        For x = s To 0 Step -1 'add the two numbers
            sum = num[x] + rev[x] + carry
            If sum > (9 + 48 + 48) Then
                num[x] = sum - 10 - 48
                carry = 1
            Else
                num[x] = sum - 48
                carry = 0
            End If
        Next
        If carry = 1 Then num = "1" + num
        it = it + 1 : temp(it) = num
        rev = num : s = Len(num) - 1
        For x = 0 To s
            rev[s - x] = num[x]
        Next
    Loop Until num = rev OrElse it = max_it
    If it = max_it Then
        match = 0
        ' if it's palindrome then save the number
        If palindrome <> 0 Then
            p_count = p_count + 1
            ReDim Preserve lychrel_palindrome(1 To p_count)
            lychrel_palindrome(p_count) = num2test
        End If
        For x = 1 To seed ' check against previous found seed(s)
            For x1 = max_it To 1 Step -1
                If store(x, 1) = temp(x1) Then
                    match = 1
                    related = related + 1
                    Exit For, For
                Else
                    If Len(store(x,1)) > Len(temp(x1)) Then
                        Exit For
                    End If
                End If
            Next
        Next
        ' no match found then it's a new seed, store it
        If match = 0 Then
            seed = seed + 1
            ReDim Preserve store(seed, 1)
            store(seed, 0) = Str(num2test)
            store(seed, 1) = temp(max_it)
        End If
    End If
Next

Print
Print "                      Testing numbers: 1 to "; Str(max_number_to_test)
Print "                    Iteration maximum: ";Str(max_it)
Print
Print "              Number of Lychrel seeds: "; seed
Print "                       Lychrel number: ";
For x = 1 To seed : Print store(x,0); " "; : Next : Print
Print "             Number of relateds found: "; related
Print " Lychrel numbers that are palindromes: "; p_count
Print "                  Lychrel palindromes: ";
For x = 1 To p_count : Print lychrel_palindrome(x); " "; : Next : Print
Print

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
                      Testing numbers: 1 to 10000
                    Iteration maximum: 500

              Number of Lychrel seeds: 5
                       Lychrel number: 196 879 1997 7059 9999 
             Number of relateds found: 244
 Lychrel numbers that are palindromes: 3
                  Lychrel palindromes: 4994 8778 9999
```



## Go


```go
package main

import (
	"flag"
	"fmt"
	"math"
	"math/big"
	"os"
)

var maxRev = big.NewInt(math.MaxUint64 / 10) // approximate
var ten = big.NewInt(10)

// Reverse sets `result` to the value of the base ten digits of `v` in
// reverse order and returns `result`.
// Only handles positive integers.
func reverseInt(v *big.Int, result *big.Int) *big.Int {
	if v.Cmp(maxRev) <= 0 {
		// optimize small values that fit within uint64
		result.SetUint64(reverseUint64(v.Uint64()))
	} else {
		if true {
			// Reverse the string representation
			s := reverseString(v.String())
			result.SetString(s, 10)
		} else {
			// This has fewer allocations but is slower:
			// Use a copy of `v` since we mutate it.
			v := new(big.Int).Set(v)
			digit := new(big.Int)
			result.SetUint64(0)
			for v.BitLen() > 0 {
				v.QuoRem(v, ten, digit)
				result.Mul(result, ten)
				result.Add(result, digit)
			}
		}
	}
	return result
}

func reverseUint64(v uint64) uint64 {
	var r uint64
	for v > 0 {
		r *= 10
		r += v % 10
		v /= 10
	}
	return r
}

func reverseString(s string) string {
	b := make([]byte, len(s))
	for i, j := 0, len(s)-1; j >= 0; i, j = i+1, j-1 {
		b[i] = s[j]
	}
	return string(b)
}

var known = make(map[string]bool)

func Lychrel(n uint64, iter uint) (isLychrel, isSeed bool) {
	v, r := new(big.Int).SetUint64(n), new(big.Int)
	reverseInt(v, r)
	seen := make(map[string]bool)
	isLychrel = true
	isSeed = true
	for i := iter; i > 0; i-- {
		str := v.String()
		if seen[str] {
			//log.Println("found a loop with", n, "at", str)
			isLychrel = true
			break
		}
		if ans, ok := known[str]; ok {
			//log.Println("already know:", str, ans)
			isLychrel = ans
			isSeed = false
			break
		}
		seen[str] = true

		v = v.Add(v, r)
		//log.Printf("%v + %v = %v\n", str, r, v)
		reverseInt(v, r)
		if v.Cmp(r) == 0 {
			//log.Println(v, "is a palindrome,", n, "is not a Lychrel number")
			isLychrel = false
			isSeed = false
			break
		}
	}
	for k := range seen {
		known[k] = isLychrel
	}
	//if isLychrel { log.Printf("%v may be a Lychrel number\n", n) }
	return isLychrel, isSeed
}

func main() {
	max := flag.Uint64("max", 10000, "search in the range 1..`N` inclusive")
	iter := flag.Uint("iter", 500, "limit palindrome search to `N` iterations")
	flag.Parse()
	if flag.NArg() != 0 {
		flag.Usage()
		os.Exit(2)
	}

	fmt.Printf("Calculating using n = 1..%v and %v iterations:\n", *max, *iter)
	var seeds []uint64
	var related int
	var pals []uint64
	for i := uint64(1); i <= *max; i++ {
		if l, s := Lychrel(i, *iter); l {
			if s {
				seeds = append(seeds, i)
			} else {
				related++
			}
			if i == reverseUint64(i) {
				pals = append(pals, i)
			}
		}
	}

	fmt.Println("      Number of Lychrel seeds:", len(seeds))
	fmt.Println("                Lychrel seeds:", seeds)
	fmt.Println("            Number of related:", related)
	fmt.Println("Number of Lychrel palindromes:", len(pals))
	fmt.Println("          Lychrel palindromes:", pals)
}
```

{{out}}

```txt

Calculating using n = 1..10000 and 500 iterations:
      Number of Lychrel seeds: 5
                Lychrel seeds: [196 879 1997 7059 9999]
            Number of related: 244
Number of Lychrel palindromes: 3
          Lychrel palindromes: [4994 8778 9999]

```



## Haskell


```Haskell
module Main where

import Data.List

procLychrel :: Integer -> [Integer]
procLychrel a = a : pl 0 a
  where
    pl c n =
      if c > 500
        then []
        else let s = n + reverseInteger n
             in if isPalindrome s
                  then [s]
                  else s : pl (c + 1) s

isPalindrome :: Integer -> Bool
isPalindrome n =
  let s = show n
  in (s == reverse s)

isLychrel :: Integer -> Bool
isLychrel n = length (procLychrel n) > 500

reverseInteger :: Integer -> Integer
reverseInteger = read . reverse . show

seedAndRelated :: (Int, [Integer], [Integer], Int)
seedAndRelated =
  let (seed, related, _) = foldl sar ([], [], []) [1 .. 10000]
      lseed = length seed
      lrelated = length related
      totalCount = lseed + lrelated
      pal = filter isPalindrome $ seed ++ related
  in (totalCount, pal, seed, lrelated)
  where
    sar (seed, related, lych) x =
      let s = procLychrel x
          sIsLychrel = length s > 500
          (isIn, isOut) = partition (`elem` lych) s
          newLych = lych ++ isOut
      in if sIsLychrel
           then if null isIn -- seed lychrel number
                  then (x : seed, related, newLych)
                  else (seed, x : related, newLych) -- related lychrel number
           else (seed, related, lych)

main = do
  let (totalCount, palindromicLychrel, lychrelSeeds, relatedCount) = seedAndRelated
  putStrLn $ "[1..10,000] contains " ++ show totalCount ++ " Lychrel numbers."
  putStrLn $ show palindromicLychrel ++ " are palindromic Lychrel numbers."
  putStrLn $ show lychrelSeeds ++ " are Lychrel seeds."
  putStrLn $ "There are " ++ show relatedCount ++ " related Lychrel numbers."
```

{{Out}}

```Haskell

[1..10,000] contains 249 Lychrel numbers.
[9999,8778,4994] are palindromic Lychrel numbers.
[9999,7059,1997,879,196] are Lychrel seeds.
There are 244 related Lychrel numbers.

```



## J


Reverse digits:


```J
revdig=:('x',~|.)&.":"0
```


Note that we need extended precision numbers because 500 iterations gets us into very large numbers even when we start small. For example, starting from 12:


```J
   (+revdig)^:500(12)
989330226271793404132120335101444022368928728236373385750622261099268167362912850818170039990942038798808908830140199820181718948328173760873880172226156484473632718819962221534191534021132404387162721132989
```


Test whether a number is a lychrel (true or related) number within that 500 iteration limit:


```J
lychrel500=: (~: revdig)@((+^:~: revdig)^:500)@(+revdig)"0
```


Number of these lychrel numbers not exceeding 10000 (we start from 0 here, because that's natural for J's primitives, so we need 10001 integers if we are to reach 10000):


```J
   #L=:I.lychrel500 i.10001
249
```


(What are they? Note that this isn't a task requirement - just curiosity.)


```J
  3 83$L
 196  295  394  493  592  689  691  788  790  879  887  978  986 1495 1497 1585 1587 1675 1677 1765 1767 1855 1857 1945 1947 1997 2494 2496 2584 2586 2674 2676 2764 2766 2854 2856 2944 2946 2996 3493 3495 3583 3585 3673 3675 3763 3765 3853 3855 3943 3945 3995 4079 4169 4259 4349 4439 4492 4494 4529 4582 4584 4619 4672 4674 4709 4762 4764 4799 4852 4854 4889 4942 4944 4979 4994 5078 5168 5258 5348 5438 5491 5493
5528 5581 5583 5618 5671 5673 5708 5761 5763 5798 5851 5853 5888 5941 5943 5978 5993 6077 6167 6257 6347 6437 6490 6492 6527 6580 6582 6617 6670 6672 6707 6760 6762 6797 6850 6852 6887 6940 6942 6977 6992 7059 7076 7149 7166 7239 7256 7329 7346 7419 7436 7491 7509 7526 7581 7599 7616 7671 7689 7706 7761 7779 7796 7851 7869 7886 7941 7959 7976 7991 8058 8075 8079 8089 8148 8165 8169 8179 8238 8255 8259 8269 8328
8345 8349 8359 8418 8435 8439 8449 8490 8508 8525 8529 8539 8580 8598 8615 8619 8629 8670 8688 8705 8709 8719 8760 8778 8795 8799 8809 8850 8868 8885 8889 8899 8940 8958 8975 8979 8989 8990 9057 9074 9078 9088 9147 9164 9168 9178 9237 9254 9258 9268 9327 9344 9348 9358 9417 9434 9438 9448 9507 9524 9528 9538 9597 9614 9618 9628 9687 9704 9708 9718 9777 9794 9798 9808 9867 9884 9888 9898 9957 9974 9978 9988 9999
```


To figure out which of these lychrel numbers were "true" lychrel numbers, we will need to work with sequences of sums seeded from these numbers. So let's just find those sequences:


```J
   S=:(+revdig)^:(i.501)"0 L
```


And, let's take a peek at some of the numbers in these sequences (the first 10 values in each lychrel sequence, starting from the first lychrel candidate seeds):


```J
   10 10 {.S
196  887 1675  7436 13783  52514  94039  187088  1067869 10755470
295  887 1675  7436 13783  52514  94039  187088  1067869 10755470
394  887 1675  7436 13783  52514  94039  187088  1067869 10755470
493  887 1675  7436 13783  52514  94039  187088  1067869 10755470
592  887 1675  7436 13783  52514  94039  187088  1067869 10755470
689 1675 7436 13783 52514  94039 187088 1067869 10755470 18211171
691  887 1675  7436 13783  52514  94039  187088  1067869 10755470
788 1675 7436 13783 52514  94039 187088 1067869 10755470 18211171
790  887 1675  7436 13783  52514  94039  187088  1067869 10755470
879 1857 9438 17787 96558 182127 903408 1707717  8884788 17759676
```


So most of these are related... which ones are not? (An integer is owned if it is in the sequence for this lychrel candidate or for any previous candidates. A lychrel candidate is a true lychrel number if none of its sequence is owned by any previous candidate.)


```J
   owned=: <@~.@,\S
   #T=: (-. (<"1 S) +./@e.&> a:,}:owned)#L
5
   T
196 879 1997 7059 9999
```


And, with that, we can find the count of "just related" lychrel numbers:


```J
   L -&# T
244
```


And, finally, which of the (true or related) lychrel numbers were themselves palindromes?


```J
   (#~(=revdig))L
4994 8778 9999
```


To reiterate, here's the collected definitions with the task required results (everything above this point was basically just documentation - J tends to need that kind of documentation, because of the nature of the language):


```J
   revdig=:('x',~|.)&.":"0
   lychrel500=: (~: revdig)@((+^:~: revdig)^:500)@(+revdig)"0
   L=:I.lychrel500 i.10001
   S=:(+revdig)^:(i.501)"0 L
   owned=: <@~.@,\S
   #T=: (-. (<"1 S) +./@e.&> a:,}:owned)#L
5
   T
196 879 1997 7059 9999
   L -&# T
244
   (#~(=revdig))L
4994 8778 9999
```


T is the "seed" or "true" lychrel numbers, and #T is how many of them we found. L is all of the candidates (both seeds and relateds), so the difference in the lengths of L and T is the number of related.


## Java

Translation of [[Lychrel_numbers#Python|Python]] via [[Lychrel_numbers#D|D]]
{{works with|Java|8}}

```java
import java.math.BigInteger;
import java.util.*;

public class Lychrel {

    static Map<BigInteger, Tuple> cache = new HashMap<>();

    static class Tuple {
        final Boolean flag;
        final BigInteger bi;

        Tuple(boolean f, BigInteger b) {
            flag = f;
            bi = b;
        }
    }

    static BigInteger rev(BigInteger bi) {
        String s = new StringBuilder(bi.toString()).reverse().toString();
        return new BigInteger(s);
    }

    static Tuple lychrel(BigInteger n) {
        Tuple res;
        if ((res = cache.get(n)) != null)
            return res;

        BigInteger r = rev(n);
        res = new Tuple(true, n);
        List<BigInteger> seen = new ArrayList<>();

        for (int i = 0; i < 500; i++) {
            n = n.add(r);
            r = rev(n);

            if (n.equals(r)) {
                res = new Tuple(false, BigInteger.ZERO);
                break;
            }

            if (cache.containsKey(n)) {
                res = cache.get(n);
                break;
            }

            seen.add(n);
        }

        for (BigInteger bi : seen)
            cache.put(bi, res);

        return res;
    }

    public static void main(String[] args) {

        List<BigInteger> seeds = new ArrayList<>();
        List<BigInteger> related = new ArrayList<>();
        List<BigInteger> palin = new ArrayList<>();

        for (int i = 1; i <= 10_000; i++) {
            BigInteger n = BigInteger.valueOf(i);

            Tuple t = lychrel(n);

            if (!t.flag)
                continue;

            if (n.equals(t.bi))
                seeds.add(t.bi);
            else
                related.add(t.bi);

            if (n.equals(t.bi))
                palin.add(t.bi);
        }

        System.out.printf("%d Lychrel seeds: %s%n", seeds.size(), seeds);
        System.out.printf("%d Lychrel related%n", related.size());
        System.out.printf("%d Lychrel palindromes: %s%n", palin.size(), palin);
    }
}
```



```txt
5 Lychrel seeds: [196, 879, 1997, 7059, 9999]
244 Lychrel related
5 Lychrel palindromes: [196, 879, 1997, 7059, 9999]
```



## jq


```jq
# This workhorse function assumes its arguments are
# non-negative integers represented as decimal strings:
def add(num1;num2):
  if (num1|length) < (num2|length) then add(num2;num1)
  else  (num1 | explode | map(.-48) | reverse) as $a1
  | (num2 | explode | map(.-48) | reverse) as $a2
  | reduce range(0; num1|length) as $ix
      ($a2;  # result
       ( $a1[$ix] + .[$ix] ) as $r
         | if $r > 9 # carrying
           then
             .[$ix + 1] = ($r / 10 | floor) +  (if $ix + 1 >= length then 0 else .[$ix + 1] end )
             | .[$ix] = $r - ( $r / 10 | floor ) * 10
           else
             .[$ix] = $r
           end )
  | reverse | map(.+48) | implode
  end ;

# Input: an array
def is_palindrome:
  . as $in
  | (length -1) as $n
  | all( range(0; length/2); $in[.] == $in[$n - .]);

# Input: a string representing a decimal number.
# Output: a stream of such strings generated in accordance with the Lychrel rule, 
# subject to the limitation imposed by "limit", and ending in true if the previous item in the stream is a palindrome
def next_palindromes(limit):
   def toa: explode | map(.-48);
   def tos: map(.+48) | implode;
   def myadd(x;y): add(x|tos; y|tos) | toa;
   # input: [limit, n]
   def next:
     .[0] as $limit
     | .[1] as $n
     | if $limit <= 0 then empty
       else  myadd($n ; $n|reverse) as $sum
	| ($sum,
         if ($sum | is_palindrome) then true else [$limit - 1, $sum] | next end)
       end;
  [limit, toa] | next | if type == "boolean" then . else tos end;

# Consider integers in range(0;n) using maxiter as the maximum number
# of iterations in the search for palindromes.
# Emit a dictionary:
# { seed: _, palindromic_seed: _, related: _} + {($n): $n} for all related $n
# where .seed is an array of integers holding the potential Lychrel seeds, etc
def lychrel_dictionary(n; maxiter):
    reduce range(0; n) as $i ({};
        ($i | tostring) as $is
	| if .[$is] then .related += [$i]
	  else [$is | next_palindromes(maxiter)] as $seq
	  | . as $dict
          # | ([$i, $seq] | debug) as $debug
          | if $seq[-1] == true then .
	    else if ($is | explode | is_palindrome) then .palindromic_seed += [$i] else . end
            | if any($seq[]; $dict[.]) then .related += [$i]
              else .seed += [$i] 
              end
            | reduce $seq[] as $n (.; if .[$n] then . else .[$n] = $n end)
            end
          end ) ;

```

  
'''The task'''

```jq
lychrel_dictionary(10001; 500)
| {seed, palindromic_seed, related: (.related | length) }
```


'''Output'''

```txt
{
  "seed": [
    196,
    879,
    1997,
    7059,
    9999
  ],
  "palindromic_seed": [
    4994,
    8778,
    9999
  ],
  "related": 244
}
```



## Julia

{{works with|Julia|0.6}}


```julia
const cache = Dict{BigInt, Tuple{Bool, BigInt}}()

Base.reverse(n::Integer) = parse(BigInt, string(n) |> reverse)
function lychrel(n::BigInt)::Tuple{Bool, BigInt}
    if haskey(cache, n)
        return cache[n]
    end

    r = reverse(n)
    rst = (true, n)
    seen = Set{BigInt}()
    for i in 0:500
        n += r
        r = reverse(n)
        if n == r
            rst = (false, big(0))
            break
        end
        if haskey(cache, n)
            rst = cache[n]
            break
        end
        push!(seen, n)
    end

    for bi in seen
        cache[bi] = rst
    end
    return rst
end

seeds   = BigInt[]
related = BigInt[]
palin   = BigInt[]

for n in big.(1:10000)
    t = lychrel(n)
    if ! t[1]
        continue
    end
    if n == t[2]
        push!(seeds, n)
    else
        push!(related, n)
    end

    if n == t[2]
        push!(palin, t[2])
    end
end

println(length(seeds),   " lychrel seeds: ", join(seeds, ", "))
println(length(related), " lychrel related")
println(length(palin),   " lychrel palindromes: ", join(palin, ", "))
```


{{out}}

```txt
5 lychrel seeds: 196, 879, 1997, 7059, 9999
244 lychrel related
5 lychrel palindromes: 196, 879, 1997, 7059, 9999
```



## Kotlin


```scala
// version 1.0.6

import java.math.BigInteger

const val ITERATIONS = 500
const val LIMIT = 10000

val bigLimit = BigInteger.valueOf(LIMIT.toLong())

// In the sieve,  0 = not Lychrel, 1 = Seed Lychrel, 2 = Related Lychrel
val lychrelSieve    = IntArray(LIMIT + 1)  // all zero by default
val seedLychrels    = mutableListOf<Int>()
val relatedLychrels = mutableSetOf<BigInteger>()

fun isPalindrome(bi: BigInteger): Boolean {
    val s = bi.toString()
    return s == s.reversed()
}

fun lychrelTest(i: Int, seq: MutableList<BigInteger>){
    if (i < 1) return
    var bi = BigInteger.valueOf(i.toLong())
    (1 .. ITERATIONS).forEach {
        bi += BigInteger(bi.toString().reversed())
        seq.add(bi)
        if (isPalindrome(bi)) return
    }
    for (j in 0 until seq.size) {
        if (seq[j] <= bigLimit) lychrelSieve[seq[j].toInt()] = 2 
        else break
    } 
    val sizeBefore = relatedLychrels.size
    relatedLychrels.addAll(seq)  // if all of these can be added 'i' must be a seed Lychrel
    if (relatedLychrels.size - sizeBefore == seq.size) {
        seedLychrels.add(i)
        lychrelSieve[i] = 1 
    }
    else {
        relatedLychrels.add(BigInteger.valueOf(i.toLong()))
        lychrelSieve[i] = 2
    }        
}

fun main(args: Array<String>) {   
    val seq  = mutableListOf<BigInteger>()
    for (i in 1 .. LIMIT) 
        if (lychrelSieve[i] == 0) { 
           seq.clear() 
           lychrelTest(i, seq)
        } 
    var related = lychrelSieve.count { it == 2 }
    println("Lychrel numbers in the range [1, $LIMIT]")
    println("Maximum iterations = $ITERATIONS")
    println("\nThere are ${seedLychrels.size} seed Lychrel numbers, namely")
    println(seedLychrels)
    println("\nThere are also $related related Lychrel numbers in this range")    
    val palindromes = mutableListOf<Int>()
    for (i in 1 .. LIMIT)
        if (lychrelSieve[i] > 0 && isPalindrome(BigInteger.valueOf(i.toLong()))) palindromes.add(i)
    println("\nThere are ${palindromes.size} palindromic Lychrel numbers, namely")
    println(palindromes)
}
```


{{out}}

```txt

Lychrel numbers in the range [1, 10000]
Maximum iterations = 500

There are 5 seed Lychrel numbers, namely
[196, 879, 1997, 7059, 9999]

There are also 244 related Lychrel numbers in this range

There are 3 palindromic Lychrel numbers, namely
[4994, 8778, 9999]

```



## Mathematica

A few convenient functions:

```Mathematica
palindromeQ[n_Integer] := 
 Block[{digits = IntegerDigits[n]}, digits == Reverse@digits]

nextNumber[n_Integer] := n + FromDigits[Reverse@IntegerDigits@n]

lychrelQ[n_Integer] := ! 
  palindromeQ@
   Catch[Nest[If[palindromeQ[#], Throw[#], nextNumber[#]] &, 
     nextNumber[n], 500]]
```


A list of all integers less 10,000 that do not evolve to a palindrome in 500 iterations

```Mathematica
candidates = Cases[Range[10000], _?(lychrelQ@# &)];
```

The Lychrel seeds can be obtained from the list of candidates as follows:

```Mathematica
seeds = {};
NestWhile[(seeds = {seeds, First@#}; 
   test = NestList[nextNumber, First@#, 10]; 
   DeleteCases[
    Rest[#], _?(IntersectingQ[test, 
        NestList[nextNumber, #, 10]] &)]) &, candidates, 
 Length@# > 0 &]; seeds = Flatten@seeds
```

{{out}}

```txt

{196,879,1997,7059,9999}
```

Note, here only 10 iterations were done for comparison. While reducing the number of
iterations is clearly effective for intergers less than 10,000 this may not be sufficient
for all integers.

The number of related Lychrel numbers is:

```Mathematica
Length@candidates - Length@seeds
```

{{out}}

```txt
244

```

The Lycrel numbers that are also palindromes are:

```Mathematica
Cases[candidates, _?(palindromeQ@# &)]
```

{{out}}

```txt
{4994,8778,9999}

```


=={{Header|Pascal}}==
{{works with|Free Pascal}}
I use an array of byte.The trick is to add first, so i don't need to create a reversed number and only have to sum up one half and than correct the carry, which is most time consuming.
I don't limit the cycles but the digits, which makes string comparisons much easier.
A pre-check, when 10 digits are reached speed things up.But i have to take into account, that many numbers, reaching 10 digits, are not lychrel.
Using a simple array instead of Stringlist for related lychrels saves enourmous amount of memory ( 2Gb -> 100Mb //maxnum= 100 Mio )
The main intention was to get below 1min for 100e6 ;-)


```pascal
program p196;
{$IFDEF FPC}
  {$R-}
  {$MODE DELPHI}
  {$Optimization ON}
{$Else}
  {$APPTYPE console}
{$Endif}
{
nativeUint = LongWord;//Uint64
nativeInt = LongInt;//Int64
}

uses
  SysUtils,classes;

const
  cMAXNUM     = 100*1000*1000;//100*1000*1000;
  cMaxCycle   = 1500;
  cChkDigit   =   10;//first check lychrel found
  MaxLen      =  256;//maximal count of digits
  cMaxDigit   =  MAXLEN-1;

type
  TDigit = byte;
  tpDigit =^TDigit;
  tDigitArr = array[0..0] of tDigit;
  tpDigitArr = ^tDigitArr;
  //LSB at position 0
  TNumber = record
               nNum : array [0..cMaxDigit] of TDigit;
               nMaxPos : LongInt;
            end;
  tRelated = array[0..cMAXNUM] of byte;

procedure NumberFromString(var Number: TNumber; S: string);
var
  i,le: integer;
begin
  le := Length(s);
  IF le > cMaxDigit then
    le := cMaxDigit;
  Number.nMaxPos:= le-1;
  for i:= 0 to le-1 do
    Number.nNum[i]:= Ord(s[le- i]) - Ord('0');
end;

function NumberToString(var Number: TNumber): string;
var
  i,l: integer;
begin
  i := Number.nMaxPos;
  If i <= MAXLEN then
  begin
    SetLength(Result, i+1);
    l := i+1;
    i := 0;
    repeat
      Result[l]:= Chr(Ord('0') + Number.nNum[i]);
      inc(i);
      dec(l);
    until l <= 0;
  end
  else
  begin
    SetLength(Result, MAXLEN);
    fillchar(Result[1],MAXLEN,'.');
    For l := 1 to MAXLEN DIV 2-1 do
    Begin
      Result[l] := Chr(Ord('0') + Number.nNum[i-l+1]);
      Result[l+MAXLEN DIV 2+1] := Chr(Ord('0') + Number.nNum[24-l]);
    end;
  end;
end;

procedure CorrectCarry(var Number : TNumber);
//correct sum of digit to digit and carry
//without IF d>10 then...
var
  d,i,carry: nativeInt;
  p: tpDigitArr;
begin
  carry  := 0;
  i := Number.nMaxPos;
  p := @Number.nNum[i];
  i := -i;
  For i := i to 0 do
  Begin
   d := p^[i]+carry;
   carry := ord(d>=10);//0, 1-> (-10 AND(-carry) = 0 ,-10
   p^[i] := d+(-10 AND(-carry));
  end;
  //correct length
  IF carry >0 then
  begin
    i := Number.nMaxPos+1;
    Number.nNum[i] :=1;
    NUmber.nMaxPos := i;
  end;
end;

procedure NumberAdd(var Number : TNumber);
// first add than correct carry
var
  //pointer, for fpc is a little bit slow with dynamic array
  loIdx,hiIdx: integer;
  sum: nativeUint;
begin
  loIdx := 0;
  hiIdx := Number.nMaxPos;
  while loIdx< hiIdx  do
  Begin
    sum := Number.nNum[loIdx]+Number.nNum[hiIdx];
    Number.nNum[loIdx] := sum;
    Number.nNum[hiIdx] := sum;
    inc(loIdx);
    dec(HiIdx);
  end;

  IF loIdx = hiIdx then
    Number.nNum[loIdx] := 2*Number.nNum[loIdx];

  CorrectCarry(Number);
end;

function PalinCheck(var A: TNumber): boolean;
var
  loIdx,hiIdx: integer;
begin
  loIdx := 0;
  hiIdx := A.nMaxPos;
  repeat
    Result:= A.nNum[loIdx]=A.nNum[hiIdx];
    inc(loIdx);
    dec(hiIdx);
  until Not(Result) OR (hiIdx<loIdx);
end;

procedure ShowPalinLychrel(var Related:tRelated);
var
  i : NativeInt;
  s : string;
  slRes : TStringList;
  Work: TNumber;
Begin
  slRes := TStringList.create;

  For i := 0 to  High(Related) do
  begin
    IF Related[i] <> 0 then
    Begin
      s := IntToSTr(i);
      NumberFromString(Work,s);
      If PalinCheck(Work) then
        slRes.Add(s);
    end;
  end;

  Writeln('number of palindromatic lychrel ',slRes.count:8);
  IF slRes.Count < 10 then
  Begin
    For i := 0 to slRes.count-2 do
      write(slRes[i]:8,',');
    writeln(slRes[slRes.count-1]:8);
  end;
  slRes.free;
end;

var
  Related : tRelated;
  slSeedCache,
  slFirstChkCache : TStringList;

  Seeds : array of LongInt;
  Work: TNumber;
  num,findpos,InsPos : LongInt;
  relCount : NativeUint;
  s,f: string;

  procedure FirstCheckCache;
  //Test if Work is already in Cache
  //if not it will be inserted
  //Inspos saves the position
  var
    i : LongInt;
  Begin
    f:= NumberToString(Work);
    IF slFirstChkCache.find(f,i) then
    Begin
      IF slFirstChkCache.Objects[i]<> NIL then
      Begin
        Related[num] := 2;
        inc(RelCount);
      end;
    end
    else
    Begin
     //memorize the number as Object
      InsPos := slFirstChkCache.addObject(f,TObject(num));
    end;
  end;

begin
  fillchar(Related[1],Length(Related),#0);
  relCount := 0;

  slSeedCache := TStringList.create;
  slSeedCache.sorted := true;
  slSeedCache.duplicates := dupIgnore;

  slFirstChkCache := TStringList.create;
  slFirstChkCache.sorted := true;
  slFirstChkCache.duplicates := dupIgnore;

  setlength(Seeds,0);

  num := 1;
  repeat
    s := IntToStr(num);
    NumberFromString(Work, s);

    findPos := -1;
    InsPos  := -1;

    //early test if already in Cache
    repeat
      NumberAdd(Work);
      IF (Work.nMaxPos = cChkDigit) then
      Begin
        FirstCheckCache;
        BREAK;
      end;
    until PalinCheck(Work);

    //IF new number of cChkDigit length inserted in Cache
    IF (InsPos >= 0) AND NOT (PalinCheck(Work)) then
    Begin
      //check for lychrel
      while Work.nMaxPos < cMaxDigit do
      Begin
        NumberAdd(Work);
        if PalinCheck(Work) then
          BREAK;
      end;

      if Work.nMaxPos >= cMaxDigit then
      Begin
        f := NumberToString(Work);
        //new lychrel seed found
        IF NOT(slSeedCache.find(f,findPos)) then
        Begin
          //memorize the number by misusing of Object
          slSeedCache.addObject(f,TObject(num));
          setlength(Seeds,length(Seeds)+1);
          Seeds[High(Seeds)] := num;
          Related[num] := 1;
        end
        else
        Begin
          //a new way to known lycrel seed found, so memorize it
          Related[num] := 2;
          inc(RelCount)
        end
      end
      else
        //mark slFirstChkCache[InsPos] as not_lychrel
        slFirstChkCache.Objects[InsPos] := NIL;
    end;
    inc(num);
  until num > cMAXNUM;
  writeln ('Lychrel from 1 to ',cMAXNUM);
  writeln('number of cached ',cChkDigit,' digit ',slFirstChkCache.Count);
  writeln('number of lychrel seed          ',length(Seeds):8);
  IF length(Seeds) < 10 then
  Begin
    For InsPos:= 0 to High(Seeds)-1 do
      write(Seeds[InsPos],',');
    writeln(Seeds[High(seeds)]);
  end;
  writeln('number of lychrel related       ',RelCount:8);
  ShowPalinLychrel(Related);
  slFirstChkCache.free;
  slSeedCache.free;
  setlength(Seeds,0);
end.
{
...
Lychrel from 1 to 100000000
number of cached 10 digit 7008
number of lychrel seed              3552
number of lychrel related       28802508
number of palindromatic lychrel     5074

real  0m48.634s
user  0m48.579s
sys 0m0.012s
}
```


{{out}}

```txt
Lychrel from 1 to 10000
number of cached 10 digit 9
number of lychrel seed                 5
196,879,1997,7059,9999
number of lychrel related            244
number of palindromatic lychrel        3
    4994,    8778,    9999

```


=={{Header|Perl}}==

```perl
use strict;
use English;
use bigint;
use Const::Fast;

const my $n_max => 10_000;
const my $iter_cutoff => 500;

my @seq_dump = ();
my @seed_lychrels = ();
my @related_lychrels = ();
for (my $n=1; $n<=$n_max; $n++) {
    my @seq = lychrel_sequence($n);
    if (scalar(@seq) == $iter_cutoff) {
        if (has_overlap(\@seq, \@seq_dump)) {
            push @related_lychrels, $n;
        }
        else {
            push @seed_lychrels, $n;
        }
        @seq_dump = set_union(\@seq_dump, \@seq);
    }
}
print "Number of seed Lychrels <= $n_max: ", scalar(@seed_lychrels), "\n";
print "Seed Lychrels <= $n_max: ", join(q{, }, @seed_lychrels), "\n";
print "Number of related Lychrels <= $n_max: ", scalar(@related_lychrels), "\n";
print "Palindromes among seed and related <= $n_max: ",
      join(q{, },
           sort {$a <=> $b}
                grep {is_palindrome($ARG)} (@seed_lychrels, @related_lychrels)),
      "\n";
exit 0;

sub lychrel_sequence {
    my $n = shift;
    my @seq = ();
    for (1 .. $iter_cutoff) {
        $n = next_n($n);
        if (is_palindrome($n)) { return (); }
        else { push @seq, $n; }
    }
    return @seq;
}

sub next_n {
    my $n = shift;
    return $n + reverse($n . q{});
}

sub is_palindrome {
    my $n = shift;
    return $n eq reverse($n . q{});
}

sub has_overlap {
    my ($a, $b) = @ARG;
    my %h;
    foreach my $k (@{$a}) { $h{$k}++; }
    foreach my $k (@{$b}) { return 1 if exists $h{$k}; }
    return 0;
}

sub set_union {
    my ($a, $b) = @ARG;
    my %h;
    foreach my $k (@{$a}) { $h{$k}++; }
    foreach my $k (@{$b}) { $h{$k}++; }
    return keys(%h);
}
```

{{out}}

```txt

Number of seed Lychrels <= 10000: 5
Seed Lychrels <= 10000: 196, 879, 1997, 7059, 9999
Number of related Lychrels <= 10000: 244
Palindromes among seed and related <= 10000: 4994, 8778, 9999

```



## Perl 6

{{works with|Rakudo|2018.03}}


```perl6
my %lychrels;
my @seeds;
my @palindromes;
my $count;
my $max = 500;
my $limit = '10_000';

for 1 .. $limit -> $int {
    my @test;
    my $index = 0;
    if $int.&is-lychrel {
        print "\b" x 20, "Found Lychrel: $int";
        %lychrels.push: ($int => @test).invert;
        @palindromes.push: $int if $int == $int.flip;
        $count++;
    }
    print "\b" x 20;

    sub is-lychrel (Int $l) {
        return True if $index++ > $max;
        @test.push: my $m = $l + $l.flip;
        return False if $m == $m.flip;
        $m.&is-lychrel;
    }
}

for %lychrels{*}»[0].unique.sort -> $ly {
    my $next = False;
    for %lychrels -> $l {
        for $l.value[1..*] -> $lt {
            $next = True and last if $ly == $lt;
            last if $ly < $lt;
        }
        last if $next;
    }
    next if $next;
    @seeds.push: $ly;
}

say "   Number of Lychrel seed numbers < $limit: ", +@seeds;
say "             Lychrel seed numbers < $limit: ", join ", ", @seeds;
say "Number of Lychrel related numbers < $limit: ", +$count - @seeds;
say "    Number of Lychrel palindromes < $limit: ", +@palindromes;
say "              Lychrel palindromes < $limit: ", join ", ", @palindromes;
```


{{out}}

```txt
   Number of Lychrel seed numbers < 10_000: 5
             Lychrel seed numbers < 10_000: 196, 879, 1997, 7059, 9999
Number of Lychrel related numbers < 10_000: 244
    Number of Lychrel palindromes < 10_000: 3
              Lychrel palindromes < 10_000: 4994, 8778, 9999
```



## Phix


```Phix
constant iterations = 500,
         limit = 10000
 
sequence seeds = {},
         cache = {}
 
sequence temp = repeat(0,iterations)
sequence palin = {}
integer related = 0

for n=1 to limit do
    string num = sprintf("%d",n),
           rev = reverse(num)
    bool palindrome = (num=rev)
    for i=1 to iterations do
        integer digit, carry = 0
        for x=length(num) to 1 by -1 do
            digit = num[x]+rev[x]+carry-'0'
            carry = digit>'9'
            num[x] = digit-carry*10
        end for
        if carry then num = "1" & num end if
        temp[i] = num
        rev = reverse(num)
        if num=rev then exit end if
    end for
    if num!=rev then
        bool no_match = true
        num = sprintf("%d",n)
        if palindrome then
            palin = append(palin, num)
        end if
        for c=1 to length(cache) do
            string seed = cache[c]
            -- check against previous found seeds
            for i=iterations to 1 by -1 do
                string ti = temp[i]
                if length(seed)>length(ti) then
                    exit
                elsif seed=ti then
                    no_match = false
                    related += 1
                    exit
                end if
            end for
            if no_match=false then exit end if
        end for
        if no_match then
            seeds = append(seeds,num)
            cache = append(cache,temp[$])
        end if
    end if
end for
 
printf(1,"%d lychrel seeds: %s\n",{length(seeds),join(seeds, ", ")})
printf(1,"related lychrel: %d\n",related)
printf(1,"%d lychrel palindromes: %s\n", {length(palin),join(palin, ", ")})
```

{{out}}
Completes in under a second

```txt

5 lychrel seeds: 196, 879, 1997, 7059, 9999
related lychrel: 244
3 lychrel palindromes: 4994, 8778, 9999

```


=={{Header|PicoLisp}}==

```PicoLisp
(de pali? (N)
   (let N (chop N)
      (= N (reverse N)) ) )  
(de lychrel (A)
   (let 
      (R NIL
         Lst
         (make
            (for X A
               (let (N X  C 500)
                  (while
                     (and
                        (inc 'N (format (flip (chop N))))
                        (gt0 (dec 'C))
                        (not (pali? N)) ) )
                  (and (=0 C) (link X) ) ) ) )
         Lst22
         (by
            '((N)
               (let (X N  C 3)
                  (prog1
                     (bool
                        (or
                           (member (+ N (format (flip (chop N)))) R)
                           (find
                              '((N) (member N R))
                              (make
                                 (do C
                                    (link (inc 'X (format (flip (chop X))))) ) ) ) ) )
                  (do C
                     (push1
                        'R
                        (inc 'N (format (flip (chop N)))) ) ) ) ) )
            group
            Lst )
         P (filter pali? Lst) )
      (prinl
         "Using n = 1..10000 and limiting each search to 500 reverse-digits-and-adds" )
      (prinl
         "  Number of Lychrel numbers: "
         (length (car Lst22)) )
      (prin "    Lychrel numbers: ")
      (println (car Lst22))
      (prinl
         "  Number of Lychrel related: "
         (length (cadr Lst22)) )
      (prinl
         "  Number of Lychrel palindroms: "
         (length P) )
      (prin "    Lychrel palindromes: ")
      (println P) ) )
      
(lychrel 10000)

(bye)
```


{{out}}

```txt

Using n = 1..10000 and limiting each search to 500 reverse-digits-and-adds
  Number of Lychrel numbers: 5
    Lychrel numbers: (196 879 1997 7059 9999)
  Number of Lychrel related: 244
  Number of Lychrel palindroms: 3
    Lychrel palindromes: (4994 8778 9999)

```



## PowerShell

{{works with|PowerShell|4}}

```PowerShell

function Test-Palindrome ( [String]$String )
    {
    #  Default value
    $IsPalindrome = $True
 
    #  For each position in the string from the start to the middle
    ForEach ( $Index in ( 0..($String.Length -shr 1 ) ) )
        {
        #  Check if the character at this position matches the corresponding character at the other end
        $IsPalindrome = $IsPalindrome -and $String[$Index] -eq $String[$String.Length - $Index - 1]
        }
 
    return $IsPalindrome
    }
 
function Get-LychrelNumbers ( [int]$Upto, [int]$IterationLimit = 500 )
    {
    #  Initialize empty collections
    #  (More typically, PowerShell will use Arrays, but we are using
    #   ArrayLists here to optimize speed and memory usage.)
    $Seeds       = New-Object System.Collections.ArrayList
    $Related     = New-Object System.Collections.ArrayList
    $Palindromes = New-Object System.Collections.ArrayList
 
    #  For each integer in the test range...
    ForEach ( $N in [bigint]1..[bigint]$Upto )
        {
        #  If N was already identified as a related Lychrel in another
        #  Lychrel series, we don't need to test it
        If ( $N -notin $Related )
            {
            #  Default value
            $IsRelated = $False
 
            #  Initialize empty collection
            $NSeries = New-Object System.Collections.ArrayList
 
            #  Starting at N
            $S = $N
 
            #  Convert S to a string for testing and processing
            $SString = $S.ToString()
 
            #  For each iteration up to the maximum...
            #  (Generally we would not use ForEach with Break for a loop with
            #   these requirements, but this script is optimized for speed
            #   and ForEach ( $x in $y ) is the fastest loop in PowerShell.)
            ForEach ( $i in 1..$IterationLimit )
                {
                #  Add the reverse of S to S
                $S += [bigint]($SString[($SString.Length)..0] -join '' )
 
                #  If S appears in the series of a smaller seed Lychrel
                If ( $S -in $Related )
                    {
                    #  S is a related Lychrel; exit processing loop
                    $IsRelated = $True
                    Break
                    }
 
                #  Convert S to a string for testing and processing
                $SString = $S.ToString()
           
                If ( Test-Palindrome $SString )
                    {
                    Break
                    }
 
                #  Add S to the series we are testing
                #  (for possible inclusion as a related number if N turns out to be a seed)
                $NSeries.Add( $S )
                }
 
            #  If series did not terminate before the iteration limit...
            #  (Including if we stopped early because it join up with an earlier non-terminating series)
            If ( $IsRelated -or $i -eq $IterationLimit )
                {
                #  Add everything in the series above N to the list of related Lychrel numbers
                $Related.AddRange( $NSeries )
 
                #  If N is a related Lychrel number, add it to the list
                If ( $IsRelated         ) { $Related.    Add( $N ) }
 
                #  Otherwise add it to the list of Seed Lychrel numbers
                Else                      { $Seeds.      Add( $N ) }
 
                #  If N is a palindrome, add it to the list of palindrome Lychrel numbers
                If ( Test-Palindrome $N ) { $Palindromes.Add( $N ) }
                }
            }
        }
 
    #  Convert the various arraylists of big integers to arrays of integers and assign
    #  them as properties of a custom return object. Filter out those Related Lychrel
    #  numbers which exceed our search range.
    $LychrelNumbers = [pscustomobject]@{
            Seeds       = [int[]]$Seeds
            Related     = [int[]]$Related.Where{ $_ -le $Upto }
            Palindromes = [int[]]$Palindromes }
 
    return $LychrelNumbers
    }

```


```PowerShell

$Upto = 10000
$IterationLimit = 500
$LychrelNumbers = Get-LychrelNumbers -Upto $Upto -IterationLimit $IterationLimit
 
"Searching N = 1 through $Upto with a maximum of $IterationLimit iterations"
"      Number of seed Lychrel numbers: " + $LychrelNumbers.Seeds.Count
"                Seed Lychrel numbers: " + ( $LychrelNumbers.Seeds -join ", " )
"   Number of related Lychrel numbers: " + $LychrelNumbers.Related.Count
"Number of palindrome Lychrel numbers: " + $LychrelNumbers.Palindromes.Count
"          Palindrome Lychrel numbers: " + ( $LychrelNumbers.Palindromes -join ", " )

```

{{out}}

```txt

Searching N = 1 through 10000 with a maximum of 500 iterations
      Number of seed Lychrel numbers: 5
                Seed Lychrel numbers: 196, 879, 1997, 7059, 9999
   Number of related Lychrel numbers: 244
Number of palindrome Lychrel numbers: 3
          Palindrome Lychrel numbers: 4994, 8778, 9999

```


=={{Header|Python}}==

```python
from __future__ import print_function

def add_reverse(num, max_iter=1000):
    i, nums = 0, {num}
    while True:
        i, num = i+1, num + reverse_int(num)
        nums.add(num)
        if reverse_int(num) == num or i >= max_iter:
            break
    return nums
    
#@functools.lru_cache(maxsize=2**20)
def reverse_int(num):
    return int(str(num)[::-1])

def split_roots_from_relateds(roots_and_relateds):
    roots = roots_and_relateds[::]
    i = 1
    while i < len(roots):
        this = roots[i]
        if any(this.intersection(prev) for prev in roots[:i]):
            del roots[i]
        else:
            i += 1
    root = [min(each_set) for each_set in roots]
    related = [min(each_set) for each_set in roots_and_relateds]
    related = [n for n in related if n not in root]
    return root, related

def find_lychrel(maxn, max_reversions):
    'Lychrel number generator'
    series = [add_reverse(n, max_reversions*2) for n in range(1, maxn + 1)]
    roots_and_relateds = [s for s in series if len(s) > max_reversions]
    return split_roots_from_relateds(roots_and_relateds)


if __name__ == '__main__':
    maxn, reversion_limit = 10000, 500
    print("Calculations using n = 1..%i and limiting each search to 2*%i reverse-digits-and-adds"
          % (maxn, reversion_limit))
    lychrel, l_related = find_lychrel(maxn, reversion_limit)
    print('  Number of Lychrel numbers:', len(lychrel))
    print('    Lychrel numbers:', ', '.join(str(n) for n in lychrel))
    print('  Number of Lychrel related:', len(l_related))
    #print('    Lychrel related:', ', '.join(str(n) for n in l_related))
    pals = [x for x in lychrel + l_related  if x == reverse_int(x)]
    print('  Number of Lychrel palindromes:', len(pals))
    print('    Lychrel palindromes:', ', '.join(str(n) for n in pals))
```


{{out}}

```txt
Calculations using n = 1..10000 and limiting each search to 2*500 reverse-digits-and-adds
  Number of Lychrel numbers: 5
    Lychrel numbers: 196, 879, 1997, 7059, 9999
  Number of Lychrel related: 244
  Number of Lychrel palindromes: 3
    Lychrel palindromes: 9999, 4994, 8778
```


If we test the numbers in ascending order, many of them would have been encountered when checking smaller numbers (i.e. 10 is seen when testing 5), caching them causes a large performance boost, more so with larger ranges.

```python
from __future__ import print_function

def rev(n): return int(str(n)[::-1])

def lychel(n, cache = {}):
    if n in cache: return cache[n]

    n0, r = n, rev(n)
    res, seen = (True, n), []
    for i in range(1000):
        n += r
        r = rev(n)
        if n == r:
            res = (False, 0)
            break
        if n in cache:
            res = cache[n]
            break
        seen.append(n)

    for x in seen: cache[x] = res
    return res

seeds, related, palin = [], [], []

for i in range(1, 1000000):
    tf, s = lychel(i)
    if not tf: continue
    (seeds if i == s else related).append(i)
    if i == rev(i): palin.append(i)

print("%d Lychel seeds:"%len(seeds), seeds)
print("%d Lychel related" % len(related))
print("%d Lychel palindromes:" % len(palin), palin)
```


=={{Header|Racket}}==


```racket
#lang racket
(require racket/splicing)

(define (reverse-digits_10 N)
  (let inr ((n N) (m 0))
    (match n
      [0 m]
      [(app (curryr quotient/remainder 10) q r)
       (inr q (+ r (* 10 m)))])))

(define (palindrome?_10 n)
  (= n (reverse-digits_10 n)))

;; hash of integer? -> one of 'seed 'related #f
(splicing-let ((memo# (make-hash)))
  (define (generate-lychrel?-chain i i-rev n acc)
    (cond
      [(zero? n) ; out of steam
       (cons 'seed acc)]
      [else
       (let* ((i+ (+ i i-rev)) (i+-rev (reverse-digits_10 i+)))
         (cond
           [(= i+ i+-rev) ; palindrome breaks the chain
            (cons #f acc)]
           [(eq? 'related (hash-ref memo# i+ #f)) ; deja vu
            (cons 'related acc)]
           [else ; search some more
            (generate-lychrel?-chain i+ i+-rev (sub1 n) (cons i+ acc))]))]))
  
  ;; returns 'seed, 'related or #f depending of the Lychrel-ness of a number
  (define (lychrel-number? i #:n (n 500))
    (match (hash-ref memo# i 'unfound)
      ['unfound
       (match (generate-lychrel?-chain i (reverse-digits_10 i) n null)
         [(cons 'related chain) 'related]
         [(cons (and seed (or (and 'seed (app (λ (_) 'related) related?))
                              (and #f (app (λ (_) #f) related?))))
                chain)
          (for ((c (in-list chain))) (hash-set! memo# c related?))
          (hash-set! memo# i seed)
          seed])]
      [seed/related/false seed/related/false])))

(module+ main
  (define-values (seeds/r n-relateds palindromes/r)
    (for/fold ((s/r null) (r 0) (p/r null))
              ((i (in-range 1 (add1 10000))))
      (define lych? (lychrel-number? i))
      (define p/r+ (if (and lych? (palindrome?_10 i)) (cons (list i (list lych?)) p/r) p/r))
      (match lych?
        ['seed (values (cons i s/r) r p/r+)]
        ['related (values s/r (add1 r) p/r+)]
        [#f (values s/r r p/r+)])))
  
  (printf #<<EOS
Seed Lychrel numbers:        ~a count:~a
Related Lychrel numbers:     count:~a
Palindromic Lychrel numbers: ~a~%
EOS
          (reverse seeds/r) (length seeds/r) n-relateds (reverse palindromes/r)))
```


{{out}}

```txt
Seed Lychrel numbers:        (196 879 1997 7059 9999) count:5
Related Lychrel numbers:     count:244
Palindromic Lychrel numbers: ((4994 (related)) (8778 (related)) (9999 (seed)))
```


=={{Header|REXX}}==

```rexx
/*REXX program finds and displays  Lychrel numbers,  related numbers, and  palindromes. */
numeric digits 250                               /*ensure enough decimal digits for adds*/
parse arg high .                                 /*obtain optional argument from the CL.*/
if high='' | high==","  then high=10000          /*Not specified?  Then use the default.*/
limit=500                                        /*limit:  number of Lychrel iterations.*/
T.=0;   @.=0;   #.=0;   R=;     w=length(high)   /*W:  is used for formatting numbers.  */
$=                                               /*the list of  Lychrel  numbers.       */
    do j=1  for high;   call Lychrel j           /*find the    Lychrel   numbers.       */
    end   /*j*/
p=                                               /*the list of palindromes.             */
    do k=1  for high
    if #.k                  then $=$ k           /*build a list of Lychrel numbers.     */
    if T.k                  then R=R k           /*  "   "   "   "    "    related nums.*/
    if T.k & k==reverse(k)  then p=p k           /*  "   "   "   "    "    palindromes. */
    end   /*k*/

say 'Found in the range  1  to'    high    " (limiting searches to "    limit   ' steps):'
say
say right(words($)           , w)        'Lychrel numbers:'             $
say right(words(R) - words($), w)        'Lychrel related numbers.'
say right(words(p)           , w)        'Lychrel palindromes:'         p
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Lychrel: procedure expose limit @. #. T.;  parse arg x 1 z  /*set X and Z to argument 1.*/
         rels=0                                             /*# related numbers (so far)*/
                    do  limit;             z=z + reverse(z) /*add the reverse of Z ···  */
                    if z==reverse(z)  then return           /*is the new Z a palindrome?*/
                    rels=rels + 1;         !.rels=z         /*add to the related numbers*/
                    end   /*limit*/                         /* [↑]  only DO limit times.*/
         #.x=1                                              /*mark number as a  Lychrel.*/
         T.x=1;     do a=1  for rels;      _=!.a            /*process "related" numbers.*/
                    if @._  then #.x=0                      /*unmark number as  Lychrel.*/
                            else @._=1                      /*  mark    "    "    "     */
                    T._=1                                   /*mark number as  "related".*/
                    end   /*a*/
         return
```

{{out|output|text=  when using the default inputs:}}

```txt

Found in the range  1  to 10000  (limiting searches to  1000  steps):

    5 Lychrel numbers:  196 879 1997 7059 9999
  244 Lychrel related numbers.
    3 Lychrel palindromes:  4994 8778 9999

```



## Ruby

{{trans|Python}}

```ruby
require 'set'

def add_reverse(num, max_iter=1000)
  (1..max_iter).each_with_object(Set.new([num])) do |_,nums|
    num += reverse_int(num)
    nums << num
    return nums if palindrome?(num)
  end
end

def palindrome?(num)
  num == reverse_int(num)
end

def reverse_int(num)
  num.to_s.reverse.to_i
end

def split_roots_from_relateds(roots_and_relateds)
  roots = roots_and_relateds.dup
  i = 1
  while i < roots.length
    this = roots[i]
    if roots[0...i].any?{|prev| this.intersect?(prev)}
      roots.delete_at(i)
    else
      i += 1
    end
  end
  root = roots.map{|each_set| each_set.min}
  related = roots_and_relateds.map{|each_set| each_set.min}
  related = related.reject{|n| root.include?(n)}
  return root, related
end

def find_lychrel(maxn, max_reversions)
  series = (1..maxn).map{|n| add_reverse(n, max_reversions*2)}
  roots_and_relateds = series.select{|s| s.length > max_reversions}
  split_roots_from_relateds(roots_and_relateds)
end

maxn, reversion_limit = 10000, 500
puts "Calculations using n = 1..#{maxn} and limiting each search to 2*#{reversion_limit} reverse-digits-and-adds"
lychrel, l_related = find_lychrel(maxn, reversion_limit)
puts "  Number of Lychrel numbers: #{lychrel.length}"
puts "    Lychrel numbers: #{lychrel}"
puts "  Number of Lychrel related: #{l_related.length}"
pals = (lychrel + l_related).select{|x| palindrome?(x)}.sort
puts "  Number of Lychrel palindromes: #{pals.length}"
puts "    Lychrel palindromes: #{pals}"
```


{{out}}

```txt

Calculations using n = 1..10000 and limiting each search to 2*500 reverse-digits-and-adds
  Number of Lychrel numbers: 5
    Lychrel numbers: [196, 879, 1997, 7059, 9999]
  Number of Lychrel related: 244
  Number of Lychrel palindromes: 3
    Lychrel palindromes: [4994, 8778, 9999]

```



## Rust

This uses the num library for arbitrary-sized integer support as normal integers will overflow.

'''Cargo.toml'''

```txt
[package]
name = "lychrel"
version = "0.1.0"
authors = ["monsieursquirrel"]

[dependencies]
num = "0.1.27"
```


'''src/main.rs'''

```rust
extern crate num;
use num::FromPrimitive;
use num::bigint::BigInt;

use std::collections::HashSet;

/// Reverse a number then add it to the original.
fn rev_add(num: &BigInt) -> BigInt {
    let rev_string: String = num.to_string().chars().rev().collect();
    // should be safe, our string is guaranteed to be a number
    let rev_val: BigInt = rev_string.parse().unwrap();
    num + rev_val
}

/// Check if a number is a palindrome when written in base 10.
fn is_palindrome(num: &BigInt) -> bool {
    let num_string = num.to_string();
    let rev_string: String = num_string.chars().rev().collect();
    let comp_len = num_string.len() / 2;
    num_string[0..comp_len] == rev_string[0..comp_len]
}

/// Perform a lychrel test on a number, stopping after max_tests
/// Returns the sequence of numbers if this number is a lychrel, None otherwise.
fn test_lychrel(num: &BigInt, max_tests: usize) -> Option<Vec<BigInt>> {
    let mut sequence = Vec::<BigInt>::new();

    let is_lychrel = (0..max_tests)
        .scan(num.clone(), |current, _| {
            *current = rev_add(current);
            Some(current.clone())
        })
        .inspect(|current| sequence.push(current.clone()))
        .filter(|curent| is_palindrome(curent))
        .next()
        .is_none();

    if is_lychrel {
        Some(sequence)
    }
    else {
        None
    }
}

/// Determine if the sequence for a lychrel number is related to a previously seen sequence
fn is_related(seq: &Vec<BigInt>, lychrel_seq_numbers: &HashSet<BigInt>) -> bool {
    seq.iter().filter(|num| lychrel_seq_numbers.contains(num)).next().is_some()
}

/// Find the lychrel numbers up to max_num (inclusive).
/// Returns a tuple (lychrel numbers, related numbers, palindrome lychrel/related numbers)
fn find_lychrels(max_num: u64, max_tests: usize) -> (Vec<BigInt>, Vec<BigInt>, Vec<BigInt>) {
    // storage for various outputs
    let mut lychrels = Vec::<BigInt>::new();
    let mut relateds = Vec::<BigInt>::new();
    let mut palindrome_lychrels = Vec::<BigInt>::new();

    let mut lychrel_seq_numbers: HashSet<BigInt> = HashSet::new();

    for i in (1..(max_num + 1)) {
        let num = FromPrimitive::from_u64(i).unwrap();
        let maybe_lychrel = test_lychrel(&num, max_tests);

        if let Some(lychrel_seq) = maybe_lychrel {
            // it's a lychrel - check if it's a related number
            let related = is_related(&lychrel_seq, &lychrel_seq_numbers);

            // update our sequences
            for seq_num in lychrel_seq.into_iter() {
                lychrel_seq_numbers.insert(seq_num);
            }

            if !related {
                // the number has a new lychrel sequence, store it
                lychrels.push(num.clone());
            }
            else {
                // just count it as a related number
                relateds.push(num.clone());
            }

            if is_palindrome(&num) {
                // doesn't matter if palindromes are related or not
                palindrome_lychrels.push(num.clone());
            }
        }
    }

    (lychrels, relateds, palindrome_lychrels)
}

fn print_nums(before: &str, numbers: &Vec<BigInt>) {
    print!("{}", before);
    for (i, current) in numbers.iter().enumerate() {
        print!("{}", current);
        if i + 1 < numbers.len() {
            print!(", ");
        }
    }
    println!("");
}

fn main() {
    let max_num: u64 = 10_000;
    let max_tests: usize = 500;

    println!("Calculations using n = 1..{} and limiting each search to {} reverse-digits-and-adds",
        max_num, max_tests);

    let (lychrels, relateds, palindrome_lychrels) = find_lychrels(max_num, max_tests);

    println!("Number of Lychrel numbers: {}", lychrels.len());
    print_nums("Lychrel numbers: ", &lychrels);
    println!("Number of Lychrel related: {}", relateds.len());
    println!("Number of Lychrel palindromes: {}", palindrome_lychrels.len());
    print_nums("Lychrel palindromes: ", &palindrome_lychrels);
}
```


{{out}}

```txt
Calculations using n = 1..10000 and limiting each search to 500 reverse-digits-and-adds
Number of Lychrel numbers: 5
Lychrel numbers: 196, 879, 1997, 7059, 9999
Number of Lychrel related: 244
Number of Lychrel palindromes: 3
Lychrel palindromes: 4994, 8778, 9999
```



## Scala

Using a '''Map''' to prevent duplicate values in the Lychrel sequence when determining seeds.

```scala
import scala.collection.mutable.LinkedHashMap

val range = 1 to 10000
val maxIter = 500;

def lychrelSeq( seed:BigInt ) : Stream[BigInt] = { 
  def reverse( v:BigInt ) = BigInt(v.toString.reverse)
  def isPalindromic( v:BigInt ) = { val s = (v + reverse(v)).toString; s == s.reverse }

  def loop( v:BigInt ):Stream[BigInt] = v #:: loop( v + reverse(v) )
  val seq = loop(seed)

  seq.take( seq.take(maxIter).indexWhere( isPalindromic(_) ) match { 
    case -1 => maxIter
    case n => n + 1
  })
}

// A quick test
assert( lychrelSeq(56).length == 1 )
assert( lychrelSeq(57).length == 2 )
assert( lychrelSeq(59).length == 3 )
assert( lychrelSeq(89).length == 24 )
assert( lychrelSeq(10911).length == 55 )

val lychrelNums = for( n <- range if lychrelSeq(n).length == maxIter ) yield n

val (seeds,related) = {
  val lycs = LinkedHashMap[BigInt,Int]()

  // Fill the Map not allowing duplicate values
  lychrelNums.foreach{ n => val ll = lychrelSeq(n).map( (_ -> n) ); LinkedHashMap( ll:_* ) ++= lycs }
  
  for( n <- lychrelNums ) {
    val ll = lychrelSeq(n).map( (_ -> n) )
    val mm = LinkedHashMap( ll:_* )
    lycs ++= (mm ++= lycs)
  }

  // Group by the Lychrel Number
  val zz = lycs.groupBy{ _._2 }.map{ case (k,m) => k -> m.keys.toList.sorted }

  // Now, group by size, seeds will have 500 or maxIter
  val yy = lychrelNums.groupBy( n => zz.filterKeys(_==n).values.flatten.size < maxIter )

  // Results: seeds are false, related true
  (yy.filterKeys(_ == false).values.toVector.flatten,
   yy.filterKeys(_ ==  true).values.toVector.flatten)
}

val lychrelPals = for( n <- lychrelNums; if n.toString == n.toString.reverse ) yield n

// Show the results
{
println( s"There are ${lychrelNums.size} Lychrel Numbers between ${range.min} and ${range.max} \n   when limited to $maxIter iterations:" )
println
println( s"\t        Seeds: ${seeds.size} (${seeds.mkString(", ")})" )
println( s"\tRelated Count: ${related.size}" )
println( s"\t  Palindromes: (${lychrelPals.mkString(", ")})")
}
```


{{out}}

```txt
There are 249 Lychrel Numbers between 1 and 10000
   when limited to 500 iterations:

                Seeds: 5 (196, 879, 1997, 7059, 9999)
        Related Count: 244
          Palindromes: (4994, 8778, 9999)

```



## Sidef

{{trans|Perl 6}}

```ruby
var (
    lychrels = [],
    palindromes = [],
    seeds = [],
    max = 500,
)
 
for i in (1 .. 10_000) {
    var (test = [], count = 0)
 
    func lychrel(l) {
        count++ > max && return true
        test << (var m = (l + Num(Str(l).flip)))
        Str(m).is_palindrome && return false
        lychrel(m)
    }
 
    if (lychrel(i)) {
        lychrels << Pair(Str(i), test)
    }
}
 
seeds << lychrels[0]
 
for l in lychrels {
    if (l.key.is_palindrome) {
        palindromes << l.key
    }
 
    var h = Hash()
    h.set_keys(l.value...)
 
    var trial = seeds.count_by { |s|
        s.value.any { |k| h.contains(k) } ? break : true
    }
 
    if (trial == seeds.len) {
        seeds << l
    }
}
 
say ("   Number of Lychrel seed numbers < 10_000: ", seeds.len)
say ("             Lychrel seed numbers < 10_000: ", seeds.map{.key}.join(', '))
say ("Number of Lychrel related numbers < 10_000: ", lychrels.len - seeds.len)
say ("    Number of Lychrel palindromes < 10_000: ", palindromes.len)
say ("              Lychrel palindromes < 10_000: ", palindromes.join(', '))
```

{{out}}

```txt

   Number of Lychrel seed numbers < 10_000: 5
             Lychrel seed numbers < 10_000: 196, 879, 1997, 7059, 9999
Number of Lychrel related numbers < 10_000: 244
    Number of Lychrel palindromes < 10_000: 3
              Lychrel palindromes < 10_000: 4994, 8778, 9999

```



## Tcl


{{works with|Tcl|8.5}}

As we collect Lychrel numbers, we can save some time by storing all the members of their orbits in a hash table so that related numbers can be quickly identified.  This is what the <tt>$visited</tt> variable is for:  using the keys of a dictionary (or on earlier Tcl versions, an array) for a hash table is a common trick in Tcl.  Native bignums mean we don't have to worry about overflow


```Tcl
proc pal? {n} {
    expr {$n eq [string reverse $n]}
}

proc step {n} {
    set n_ [string reverse $n]
    set n_ [string trimleft $n_ 0]
    expr {$n + $n_}
}

proc lychrels {{max 10000} {iters 500}} {

    set lychrels {}     ;# true Lychrels
    set visited {}      ;# visited numbers for Related test
    set nRelated 0      ;# count of related numbers seen
    set pals {}         ;# palindromic Lychrels and related

    puts "Searching for Lychrel numbers in \[1,$max\]"
    puts "With a maximum of $iters iterations"

    for {set i 1} {$i <= $max} {incr i} {
        set n $i        ;# seed the sequence
        set seq {}      ;# but don't check the seed, nor remember it for the Related test
        for {set j 0} {$j < $iters} {incr j} {
            set n [step $n]
            dict set seq $n {}
            if {[dict exists $visited $n]} {
                incr nRelated
                if {[pal? $i]} {
                    lappend pals $i
                }
                break   ;# bail out if it's Related
            }
            if {[pal? $n]} {
                break   ;# bail out if it's a palindrome
            }
        }
        if {$j >= $iters} {    ;# the loop was exhausted: must be a Lychrel!
            if {[pal? $i]} {
                lappend pals $i
            }
            lappend lychrels $i
            set visited [dict merge $visited $seq]
        }
    }

    puts "[llength $lychrels] Lychrel numbers found:"
    puts $lychrels
    puts "Count of related numbers: $nRelated"
    puts "Palindromic Lychrel and related numbers: $pals"
}

lychrels
```


{{out}}

```txt
Searching for Lychrel numbers in [1,10000]
With a maximum of 500 iterations
5 Lychrel numbers found:
196 879 1997 7059 9999
Count of related numbers: 244
Palindromic Lychrel and related numbers: 4994 8778 9999
```



## zkl

Uses the GNU GMP big int library.

The Lychrel number chain is stored as a long string with '\0' separating each number, which we can then search to find relateds.

```zkl
var BN=Import("zklBigNum");

   // 192-->"887\01675\07436\013783\0..." ~60k bytes
fcn lychrel(n,returnListof=False){ n=BN(n); //-->Bool|Data(==StringBuf)
   sink:=(if(returnListof) Data(0,String) else Void);
   nls:=(500).pump(sink,'wrap(){
      ns:=n.add(BN(n.toString().reverse())).toString();
      if(ns==ns.reverse()) return(Void.Stop,False);  // not a Lychrel number
      ns
   });
   if(nls) if(returnListof) return(sink.mode(Int).howza(2)) else return(True);
   False;
}
fcn isPalindrome(n){ n==n.toString().reverse().toInt() }
fcn findSeeds(lychrels){
   seeds:=List(lychrels[0]);
   foreach n,lnns in ([1..].zip(lychrels[1,*])){ ln,seq:=lnns;
      foreach _,s2 in (seeds){
	 foreach s3 in (seq){
	    if(Void!=(z:=s2.findString(s3)) and 0==s2[z-1]) break(2);
	 }
      }
      fallthrough{ seeds.append(lnns) }
   }
   seeds.apply("get",0)
}
```


```zkl
lychrels:=[1..0d10_000].filter(lychrel).apply(fcn(n){ T(n,lychrel(n,True)) });
println("[1..10,000] contains ",lychrels.len()," Lychrel numbers.");

lychrels.pump(List,fcn([(n,_)]){ isPalindrome(n) and n or Void.Skip })
   .println("<-- Palindromic Lychrel numbers");

(n:=findSeeds(lychrels))
.println("<-- Lychrel seeds (%d) ==> %d related"
.fmt(n.len(),lychrels.len() - n.len()));
```

{{out}}

```txt

[1..10,000] contains 249 Lychrel numbers.
L(4994,8778,9999)<-- Palindromic Lychrel numbers
L(196,879,1997,7059,9999)<-- Lychrel seeds (5) ==> 244 related

```

