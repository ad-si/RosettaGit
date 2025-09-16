+++
title = "Happy numbers"
description = ""
date = 2019-09-16T15:42:51Z
aliases = []
[extra]
id = 4140
[taxonomies]
categories = ["task", "Arithmetic operations"]
tags = []
languages = [
  "8th",
  "acl2",
  "actionscript",
  "ada",
  "algol_68",
  "algol_w",
  "apl",
  "applescript",
  "autohotkey",
  "awk",
  "batch_file",
  "bbc_basic",
  "bori",
  "brat",
  "c",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "dart",
  "dc",
  "dcl",
  "dwscript",
  "dyalect",
  "e",
  "eiffel",
  "elena",
  "elixir",
  "erlang",
  "euphoria",
  "factor",
  "false",
  "fantom",
  "forth",
  "fortran",
  "freebasic",
  "frege",
  "go",
  "groovy",
  "harbour",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "lasso",
  "liberty_basic",
  "locomotive_basic",
  "logo",
  "lolcode",
  "lua",
  "m2000_interpreter",
  "maple",
  "matlab",
  "maxscript",
  "mercury",
  "ml",
  "mlite",
  "mumps",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oorexx",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "potion",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "salmon",
  "scala",
  "scheme",
  "scratch",
  "seed7",
  "sequencel",
  "setl",
  "sidef",
  "smalltalk",
  "swift",
  "tcl",
  "tuscript",
  "ubasic_4th",
  "unix_shell",
  "ursala",
  "vala",
  "vba",
  "vbscript",
  "visual_basic_dotnet",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

From Wikipedia, the free encyclopedia:
:A [[wp:Happy number|happy number]] is defined by the following process:

: Starting with any positive integer, replace the number by the sum of the squares of its digits, and repeat the process until the number equals   '''1'''   (where it will stay),   or it loops endlessly in a cycle which does not include   '''1'''.   Those numbers for which this process ends in   '''1'''   are happy numbers,   while those that do not end in   '''1'''   are unhappy numbers.

Display an example of your output here.


;task:
Find and print the first 8 happy numbers.


## See also

* [[oeis:A007770|The     happy numbers on OEIS:   A007770]]
* [[oeis:A031177|The unhappy numbers on OEIS;   A031177]]





## 8th


```8th

: until!  "not while!" eval i;

with: w
with: n

: sumsqd  \ n -- n
    0 swap repeat
        0; 10 /mod -rot sqr + swap
    again ;

: cycle \ n xt -- n
    >r
    dup r@ exec  \ -- tortoise, hare
    repeat
        swap r@ exec
        swap r@ exec r@ exec
    2dup = until!
    rdrop drop ;

: happy?  ' sumsqd cycle 1 = ;

: .happy \ n --
    1 repeat
        dup happy? if  dup . space  swap 1- swap  then 1+
    over 0 > while!
    2drop cr ;

;with
;with

```

```txt

ok> 8 .happy
1 7 10 13 19 23 28 31

```



## ACL2


```Lisp
(include-book "arithmetic-3/top" :dir :system)

(defun sum-of-digit-squares (n)
   (if (zp n)
       0
       (+ (expt (mod n 10) 2)
          (sum-of-digit-squares (floor n 10)))))

(defun is-happy-r (n seen)
   (let ((next (sum-of-digit-squares n)))
        (cond ((= next 1) t)
              ((member next seen) nil)
              (t (is-happy-r next (cons next seen))))))

(defun is-happy (n)
   (is-happy-r n nil))

(defun first-happy-nums-r (n i)
   (cond ((zp n) nil)
         ((is-happy i)
          (cons i (first-happy-nums-r (1- n) (1+ i))))
         (t (first-happy-nums-r n (1+ i)))))

(defun first-happy-nums (n)
   (first-happy-nums-r n 1))
```

Output:

```txt
(1 7 10 13 19 23 28 31)
```



## ActionScript


```ActionScript
function sumOfSquares(n:uint)
{
	var sum:uint = 0;
	while(n != 0)
	{
		sum += (n%10)*(n%10);
		n /= 10;
	}
	return sum;
}
function isInArray(n:uint, array:Array)
{
	for(var k = 0; k < array.length; k++)
		if(n == array[k]) return true;
	return false;
}
function isHappy(n)
{
	var sequence:Array = new Array();
	while(n != 1)
	{
		sequence.push(n);
		n = sumOfSquares(n);
		if(isInArray(n,sequence))return false;
	}
	return true;
}
function printHappy()
{
	var numbersLeft:uint = 8;
	var numberToTest:uint = 1;
	while(numbersLeft != 0)
	{
		if(isHappy(numberToTest))
		{
			trace(numberToTest);
			numbersLeft--;
		}
		numberToTest++;
	}
}
printHappy();
```

Sample output:

```txt

1
7
10
13
19
23
28
31

```



## Ada


```Ada
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Containers.Ordered_Sets;

procedure Test_Happy_Digits is
   function Is_Happy (N : Positive) return Boolean is
      package Sets_Of_Positive is new Ada.Containers.Ordered_Sets (Positive);
      use Sets_Of_Positive;
      function Next (N : Positive) return Natural is
         Sum   : Natural := 0;
         Accum : Natural := N;
      begin
         while Accum > 0 loop
            Sum   := Sum + (Accum mod 10) ** 2;
            Accum := Accum / 10;
         end loop;
         return Sum;
      end Next;
      Current : Positive := N;
      Visited : Set;
   begin
      loop
         if Current = 1 then
            return True;
         elsif Visited.Contains (Current) then
            return False;
         else
            Visited.Insert (Current);
            Current := Next (Current);
         end if;
      end loop;
   end Is_Happy;
   Found : Natural := 0;
begin
   for N in Positive'Range loop
      if Is_Happy (N) then
         Put (Integer'Image (N));
         Found := Found + 1;
         exit when Found = 8;
      end if;
   end loop;
end Test_Happy_Digits;
```

Sample output:

```txt

 1 7 10 13 19 23 28 31

```



## ALGOL 68

```algol68
INT base10 = 10, num happy = 8;

PROC next = (INT in n)INT: (
  INT n := in n;
  INT out := 0;
  WHILE n NE 0 DO
    out +:= ( n MOD base10 ) ** 2;
    n := n OVER base10
  OD;
  out
);

PROC is happy = (INT in n)BOOL: (
  INT n := in n;
  FOR i WHILE n NE 1 AND n NE 4 DO n := next(n) OD;
  n=1
);

INT count := 0;
FOR i WHILE count NE num happy DO
  IF is happy(i) THEN
    count +:= 1;
    print((i, new line))
  FI
OD
```

Output:

```txt

         +1
         +7
        +10
        +13
        +19
        +23
        +28
        +31

```



## ALGOL W


```algolw
begin
    % find some happy numbers                                      %
    % returns true if n is happy, false otherwise; n must be >= 0  %
    logical procedure isHappy( integer value n ) ;
        if n < 2 then true
        else begin
            % seen is used to hold the values of the cycle of the  %
            % digit square sums, as noted in the Batch File        %
            % version, we do not need a large array. The digit     %
            % square sum of 9 999 999 999 is 810...                %
            integer array seen( 0 :: 32 );
            integer number, trys;
            number             := n;
            trys               := -1;
            while begin
                logical terminated;
                integer tPos;
                terminated     := false;
                tPos           := 0;
                while not terminated and tPos <= trys do begin
                    terminated := seen( tPos ) = number;
                    tPos       := tPos + 1
                end while_not_terminated_and_tPos_lt_trys ;
                number > 1 and not terminated
            end do begin
                integer sum;
                trys           := trys + 1;
                seen( trys )   := number;
                sum            := 0;
                while number > 0 do begin
                    integer digit;
                    digit      := number rem 10;
                    number     := number div 10;
                    sum        := sum + ( digit * digit )
                end while_number_gt_0 ;
                number         := sum
            end while_number_gt_1_and_not_terminated ;
            number = 1
        end isHappy ;
    % print the first 8 happy numbers                               %
    begin
        integer       happyCount, n;
        happyCount := 0;
        n          := 1;
        write( "first 8 happy numbers: " );
        while happyCount < 8 do begin
            if isHappy( n ) then begin
                writeon( i_w := 1, " ", n );
                happyCount := happyCount + 1
            end if_isHappy_n ;
            n := n + 1
        end while_happyCount_lt_8
    end
end.
```

```txt

first 8 happy numbers:  1   7   10   13   19   23   28   31

```



## APL



### Tradfn


```APL
     ∇ HappyNumbers arg;⎕IO;∆roof;∆first;bin;iroof
[1]   ⍝0: Happy number
[2]   ⍝1: http://rosettacode.org/wiki/Happy_numbers
[3]    ⎕IO←1                              ⍝ Index origin
[4]    ∆roof ∆first←2↑arg,10              ⍝
[5]
[6]    bin←{
[7]        ⍺←⍬                            ⍝ Default left arg
[8]        ⍵=1:1                          ⍝ Always happy!
[9]
[10]       numbers←⍎¨1⊂⍕⍵                 ⍝ Split numbers into parts
[11]       next←+/{⍵*2}¨numbers           ⍝ Sum and square of numbers
[12]
[13]       next∊⍺:0                       ⍝ Return 0, if already exists
[14]       (⍺,next)∇ next                 ⍝ Check next number (recursive)
[15]
[16]   }¨iroof←⍳∆roof                     ⍝ Does all numbers upto ∆root smiles?
[17]
[18]   ⎕←~∘0¨∆first↑bin/iroof             ⍝ Show ∆first numbers, but not 0
     ∇
```


```txt

      HappyNumbers 100 8
 1  7  10  13  19  23  28  31

```



### Dfn


```APL

 HappyNumbers←{          ⍝ return the first ⍵ Happy Numbers
     ⍺←⍬                 ⍝ initial list
     ⍵=+/⍺:⍸⍺            ⍝ 1's mark happy numbers
     sq←×⍨               ⍝ square function (times selfie)
     isHappy←{           ⍝ is ⍵ a happy number?
         ⍺←⍬             ⍝ previous sums
         ⍵=1:1           ⍝ if we get to 1, it's happy
         n←+/sq∘⍎¨⍕⍵     ⍝ sum of the square of the digits
         n∊⍺:0           ⍝ if we hit this sum before, it's not happy
         (⍺,n)∇ n}       ⍝ recurse until it's happy or not
     (⍺,isHappy 1+≢⍺)∇ ⍵ ⍝ recurse until we have ⍵ happy numbers
 }
      HappyNumbers 8
1 7 10 13 19 23 28 31

```



## AppleScript



### Iteration


```AppleScript
on run
    set howManyHappyNumbers to 8
    set happyNumberList to {}
    set globalCounter to 1

    repeat howManyHappyNumbers times
        repeat while not isHappy(globalCounter)
            set globalCounter to globalCounter + 1
        end repeat
        set end of happyNumberList to globalCounter
        set globalCounter to globalCounter + 1
    end repeat
    log happyNumberList
end run

on isHappy(numberToCheck)
    set localCycle to {}
    repeat while (numberToCheck ≠ 1)
        if localCycle contains numberToCheck then
            exit repeat
        end if
        set end of localCycle to numberToCheck
        set tempNumber to 0
        repeat while (numberToCheck > 0)
            set digitOfNumber to numberToCheck mod 10
            set tempNumber to tempNumber + (digitOfNumber ^ 2)
            set numberToCheck to (numberToCheck - digitOfNumber) / 10
        end repeat
        set numberToCheck to tempNumber
    end repeat
    return (numberToCheck = 1)
end isHappy
```


```txt

Result: (*1, 7, 10, 13, 19, 23, 28, 31*)

```



### Functional composition

```AppleScript
-- HAPPY NUMBERS --------------------------------------------------------------

-- isHappy :: Int -> Bool
on isHappy(n)

    -- endsInOne :: [Int] -> Int -> Bool
    script endsInOne

        -- sumOfSquaredDigits :: Int -> Int
        script sumOfSquaredDigits

            -- digitSquared :: Int -> Int -> Int
            script digitSquared
                on |λ|(a, x)
                    (a + (x as integer) ^ 2) as integer
                end |λ|
            end script

            on |λ|(n)
                foldl(digitSquared, 0, splitOn("", n as string))
            end |λ|
        end script

        -- [Int] -> Int -> Bool
        on |λ|(s, n)
            if n = 1 then
                true
            else
                if s contains n then
                    false
                else
                    |λ|(s & n, |λ|(n) of sumOfSquaredDigits)
                end if
            end if
        end |λ|
    end script

    endsInOne's |λ|({}, n)
end isHappy

-- TEST -----------------------------------------------------------------------
on run

    -- seriesLength :: {n:Int, xs:[Int]} -> Bool
    script seriesLength
        property target : 8

        on |λ|(rec)
            length of xs of rec = target of seriesLength
        end |λ|
    end script

    -- succTest :: {n:Int, xs:[Int]} -> {n:Int, xs:[Int]}
    script succTest
        on |λ|(rec)
            tell rec to set {xs, n} to {its xs, its n}

            script testResult
                on |λ|(x)
                    if isHappy(x) then
                        xs & x
                    else
                        xs
                    end if
                end |λ|
            end script

            {n:n + 1, xs:testResult's |λ|(n)}
        end |λ|
    end script

    xs of |until|(seriesLength, succTest, {n:1, xs:{}})

    --> {1, 7, 10, 13, 19, 23, 28, 31}
end run


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- splitOn :: Text -> Text -> [Text]
on splitOn(strDelim, strMain)
    set {dlm, my text item delimiters} to {my text item delimiters, strDelim}
    set xs to text items of strMain
    set my text item delimiters to dlm
    return xs
end splitOn

-- until :: (a -> Bool) -> (a -> a) -> a -> a
on |until|(p, f, x)
    set mp to mReturn(p)
    set v to x

    tell mReturn(f)
        repeat until mp's |λ|(v)
            set v to |λ|(v)
        end repeat
    end tell
    return v
end |until|
```

```AppleScript
{1, 7, 10, 13, 19, 23, 28, 31}
```



## AutoHotkey


```AutoHotkey
Loop {
  If isHappy(A_Index) {
    out .= (out="" ? "" : ",") . A_Index
    i ++
    If (i = 8) {
      MsgBox, The first 8 happy numbers are: %out%
      ExitApp
    }
  }
}

isHappy(num, list="") {
  list .= (list="" ? "" : ",") . num
  Loop, Parse, num
    sum += A_LoopField ** 2
  If (sum = 1)
    Return true
  Else If sum in %list%
    Return false
  Else Return isHappy(sum, list)
}
```


```txt

The first 8 happy numbers are: 1,7,10,13,19,23,28,31

```


### Alternative version


```AutoHotkey
while h < 8
    if (Happy(A_Index)) {
        Out .= A_Index A_Space
        h++
    }
MsgBox, % Out

Happy(n) {
    Loop, {
        Loop, Parse, n
            t += A_LoopField ** 2
        if (t = 89)
            return, 0
        if (t = 1)
            return, 1
        n := t, t := 0
    }
}
```


```txt
1 7 10 13 19 23 28 31
```


=={{Header|AutoIt}}==

```autoit

$c = 0
$k = 0
While $c < 8
	$k += 1
	$n = $k
	While $n <> 1
		$s = StringSplit($n, "")
		$t = 0
		For $i = 1 To $s[0]
			$t += $s[$i] ^ 2
		Next
		$n = $t
		Switch $n
			Case 4,16,37,58,89,145,42,20
			ExitLoop
		EndSwitch
	WEnd
	If $n = 1 Then
		ConsoleWrite($k & " is Happy" & @CRLF)
		$c += 1
	EndIf
WEnd

```



```txt

Use a set of numbers (4,16,37,58,89,145,42,20) to indicate a loop and exit.
Output:
1 is Happy
7 is Happy
10 is Happy
13 is Happy
19 is Happy
23 is Happy
28 is Happy
31 is Happy

```



### Alternative version


```autoit

$c = 0
$k = 0
While $c < 8
	$a = ObjCreate("System.Collections.ArrayList")
	$k += 1
	$n = $k
	While $n <> 1
		If $a.Contains($n) Then
			ExitLoop
		EndIf
		$a.add($n)
		$s = StringSplit($n, "")
		$t = 0
		For $i = 1 To $s[0]
			$t += $s[$i] ^ 2
		Next
		$n = $t
	WEnd
	If $n = 1 Then
		ConsoleWrite($k & " is Happy" & @CRLF)
		$c += 1
	EndIf
	$a.Clear
WEnd

```


```txt

Saves all numbers in a list, duplicate entry indicates a loop.
Output:
1 is Happy
7 is Happy
10 is Happy
13 is Happy
19 is Happy
23 is Happy
28 is Happy
31 is Happy

```



## AWK


```awk
function is_happy(n)
{
  if ( n in happy ) return 1;
  if ( n in unhappy ) return 0;
  cycle[""] = 0
  while( (n!=1) && !(n in cycle) ) {
    cycle[n] = n
    new_n = 0
    while(n>0) {
      d = n % 10
      new_n += d*d
      n = int(n/10)
    }
    n = new_n
  }
  if ( n == 1 ) {
    for (i_ in cycle) {
      happy[cycle[i_]] = 1
      delete cycle[i_]
    }
    return 1
  } else {
    for (i_ in cycle) {
      unhappy[cycle[i_]] = 1
      delete cycle[i_]
    }
    return 0
  }
}

BEGIN {
  cnt = 0
  happy[""] = 0
  unhappy[""] = 0
  for(j=1; (cnt < 8); j++) {
    if ( is_happy(j) == 1 ) {
      cnt++
      print j
    }
  }
}
```

Result:

```txt
1
7
10
13
19
23
28
31
```



### Alternative version


Alternately, for legibility one might write:


```awk
BEGIN {
    for (i = 1; i < 50; ++i){
        if (isHappy(i)) {
            print i;
        }
    }
    exit
}

function isHappy(n,    seen) {
    delete seen;
    while (1) {
        n = sumSqrDig(n)
        if (seen[n]) {
            return n == 1
        }
        seen[n] = 1
    }
}

function sumSqrDig(n,     d, tot) {
    while (n) {
        d = n % 10
        tot += d * d
        n = int(n/10)
    }
    return tot
}
```



## Batch File

happy.bat

```dos
@echo off
setlocal enableDelayedExpansion
::Define a list with 10 terms as a convenience for defining a loop
set "L10=0 1 2 3 4 5 6 7 8 9"
shift /1 & goto %1
exit /b


:list min count
:: This routine prints all happy numbers > min (arg1)
:: until it finds count (arg2) happy numbers.
set /a "n=%~1, cnt=%~2"
call :listInternal
exit /b


:test min [max]
:: This routine sequentially tests numbers between min (arg1) and max (arg2)
:: to see if they are happy. If max is not specified then it defaults to min.
set /a "min=%~1"
if "%~2" neq "" (set /a "max=%~2") else set max=%min%
::The FOR /L loop does not detect integer overflow, so must protect against
::an infinite loop when max=0x7FFFFFFFF
set end=%max%
if %end% equ 2147483647 set /a end-=1
for /l %%N in (%min% 1 %end%) do (
  call :testInternal %%N && (echo %%N is happy :^)) || echo %%N is sad :(
)
if %end% neq %max% call :testInternal %max% && (echo %max% is happy :^)) || echo %max% is sad :(
exit /b


  :listInternal
  :: This loop sequentially tests each number >= n. The loop conditionally
  :: breaks within the body once cnt happy numbers have been found, or if
  :: the max integer value is reached. Performance is improved by using a
  :: FOR loop to perform most of the looping, with a GOTO only needed once
  :: per 100 iterations.
  for %%. in (
    %L10% %L10% %L10% %L10% %L10% %L10% %L10% %L10% %L10% %L10%
  ) do (
    call :testInternal !n! && (
      echo !n!
      set /a cnt-=1
      if !cnt! leq 0 exit /b 0
    )
    if !n! equ 2147483647 (
      >&2 echo ERROR: Maximum integer value reached
      exit /b 1
    )
    set /a n+=1
  )
  goto :listInternal


  :testInternal n
  :: This routine loops until the sum of squared digits converges on 1 (happy)
  :: or it detects a cycle (sad). It exits with errorlevel 0 for happy and 1 for sad.
  :: Performance is improved by using a FOR loop for the looping instead of a GOTO.
  :: Numbers less than 1000 never neeed more than 20 iterations, and any number
  :: with 4 or more digits shrinks by at least one digit each iteration.
  :: Since Windows batch can't handle more than 10 digits, allowance for 27
  :: iterations is enough, and 30 is more than adequate.
  setlocal
  set n=%1
  for %%. in (%L10% %L10% %L10%) do (
    if !n!==1 exit /b 0
    %= Only numbers < 1000 can cycle =%
    if !n! lss 1000 (
      if defined t.!n! exit /b 1
      set t.!n!=1
    )
    %= Sum the squared digits                                          =%
    %= Batch can't handle numbers greater than 10 digits so we can use =%
    %= a constrained FOR loop and avoid a slow goto                    =%
    set sum=0
    for /l %%N in (1 1 10) do (
      if !n! gtr 0 set /a "sum+=(n%%10)*(n%%10), n/=10"
    )
    set /a n=sum
  )
```

Sample usage and output

```txt

>happy list 1 8
1
7
10
13
19
23
28
31

>happy list 1000000000 10
1000000000
1000000003
1000000009
1000000029
1000000030
1000000033
1000000039
1000000067
1000000076
1000000088

>happy test 30
30 is sad :(

>happy test 31
31 is happy :)

>happy test 1 10
1 is happy :)
2 is sad :(
3 is sad :(
4 is sad :(
5 is sad :(
6 is sad :(
7 is happy :)
8 is sad :(
9 is sad :(
10 is happy :)

>happy test "50 + 10 * 5"
100 is happy :)

>happy test 0x7fffffff
2147483647 is sad :(

>happy test 0x7ffffffd
2147483645 is happy :)

>happy list 0x7ffffff0 10
2147483632
2147483645
ERROR: Maximum integer value reached

```



## BBC BASIC

```bbcbasic
      number% = 0
      total% = 0
      REPEAT
        number% += 1
        IF FNhappy(number%) THEN
          PRINT number% " is a happy number"
          total% += 1
        ENDIF
      UNTIL total% = 8
      END

      DEF FNhappy(num%)
      LOCAL digit&()
      DIM digit&(10)
      REPEAT
        digit&() = 0
        $$^digit&(0) = STR$(num%)
        digit&() AND= 15
        num% = MOD(digit&())^2 + 0.5
      UNTIL num% = 1 OR num% = 4
      = (num% = 1)
```

Output:

```txt
         1 is a happy number
         7 is a happy number
        10 is a happy number
        13 is a happy number
        19 is a happy number
        23 is a happy number
        28 is a happy number
        31 is a happy number
```



## Bori


```bori
bool isHappy (int n)
{
   ints cache;

   while (n != 1)
   {
      int sum = 0;

      if (cache.contains(n))
         return false;

      cache.add(n);
      while (n != 0)
      {
         int digit = n % 10;
         sum += (digit * digit);
         n = (int)(n / 10);
      }
      n = sum;
   }
   return true;
}

void test ()
{
   int num = 1;
   ints happynums;

   while (happynums.count() < 8)
   {
      if (isHappy(num))
         happynums.add(num);
      num++;
   }
   puts("First 8 happy numbers : " + str.newline + happynums);
}
```

Output:

```txt
First 8 happy numbers :
[1, 7, 10, 13, 19, 23, 28, 31]
```



## Brat


```brat
include :set

happiness = set.new 1
sadness = set.new

sum_of_squares_of_digits = { num |
  num.to_s.dice.reduce 0 { sum, n | sum = sum + n.to_i ^ 2 }
}

happy? = { n, seen = set.new |
  when {true? happiness.include? n } { happiness.merge seen << n; true }
    { true? sadness.include? n } { sadness.merge seen; false }
    { true? seen.include? n } { sadness.merge seen; false }
    { true } { seen << n; happy? sum_of_squares_of_digits(n), seen }
}

num = 1
happies = []

while { happies.length < 8 } {
  true? happy?(num)
    { happies << num }

  num = num + 1
}

p "First eight happy numbers: #{happies}"
p "Happy numbers found: #{happiness.to_array.sort}"
p "Sad numbers found: #{sadness.to_array.sort}"
```

Output:

```txt
First eight happy numbers: [1, 7, 10, 13, 19, 23, 28, 31]
Happy numbers found: [1, 7, 10, 13, 19, 23, 28, 31, 49, 68, 82, 97, 100, 130]
Sad numbers found: [2, 3, 4, 5, 6, 8, 9, 11, 12, 14, 15, 16, 17, 18, 20, 21, 22, 24, 25, 26, 27, 29, 30, 34, 36, 37, 40, 41, 42, 45, 50, 52, 53, 58, 61, 64, 65, 81, 85, 89, 145]
```



## C

Recursively look up if digit square sum is happy.

```c
#include <stdio.h>

#define CACHE 256
enum { h_unknown = 0, h_yes, h_no };
unsigned char buf[CACHE] = {0, h_yes, 0};

int happy(int n)
{
	int sum = 0, x, nn;
	if (n < CACHE) {
		if (buf[n]) return 2 - buf[n];
		buf[n] = h_no;
	}

	for (nn = n; nn; nn /= 10) x = nn % 10, sum += x * x;

	x = happy(sum);
	if (n < CACHE) buf[n] = 2 - x;
	return x;
}

int main()
{
	int i, cnt = 8;
	for (i = 1; cnt || !printf("\n"); i++)
		if (happy(i)) --cnt, printf("%d ", i);

	printf("The %dth happy number: ", cnt = 1000000);
	for (i = 1; cnt; i++)
		if (happy(i)) --cnt || printf("%d\n", i);

	return 0;
}
```

output
```txt
1 7 10 13 19 23 28 31
The 1000000th happy number: 7105849
```

Without caching, using cycle detection:

```c
#include <stdio.h>

int dsum(int n)
{
	int sum, x;
	for (sum = 0; n; n /= 10) x = n % 10, sum += x * x;
	return sum;
}

int happy(int n)
{
	int nn;
	while (n > 999) n = dsum(n); /* 4 digit numbers can't cycle */
	nn = dsum(n);
	while (nn != n && nn != 1)
		n = dsum(n), nn = dsum(dsum(nn));
	return n == 1;
}

int main()
{
	int i, cnt = 8;
	for (i = 1; cnt || !printf("\n"); i++)
		if (happy(i)) --cnt, printf("%d ", i);

	printf("The %dth happy number: ", cnt = 1000000);
	for (i = 1; cnt; i++)
		if (happy(i)) --cnt || printf("%d\n", i);

	return 0;
}
```
 Output is same as above, but much slower.


## C++

```cpp
#include <map>
#include <set>

bool happy(int number) {
  static std::map<int, bool> cache;

  std::set<int> cycle;
  while (number != 1 && !cycle.count(number)) {
    if (cache.count(number)) {
      number = cache[number] ? 1 : 0;
      break;
    }
    cycle.insert(number);
    int newnumber = 0;
    while (number > 0) {
      int digit = number % 10;
      newnumber += digit * digit;
      number /= 10;
    }
    number = newnumber;
  }
  bool happiness = number == 1;
  for (std::set<int>::const_iterator it = cycle.begin();
       it != cycle.end(); it++)
    cache[*it] = happiness;
  return happiness;
}

#include <iostream>

int main() {
  for (int i = 1; i < 50; i++)
    if (happy(i))
      std::cout << i << std::endl;
  return 0;
}
```

Output:

```txt
1
7
10
13
19
23
28
31
32
44
49
```

Alternative version without caching:

```cpp
unsigned int happy_iteration(unsigned int n)
{
  unsigned int result = 0;
  while (n > 0)
  {
    unsigned int lastdig = n % 10;
    result += lastdig*lastdig;
    n /= 10;
  }
  return result;
}

bool is_happy(unsigned int n)
{
  unsigned int n2 = happy_iteration(n);
  while (n != n2)
  {
    n = happy_iteration(n);
    n2 = happy_iteration(happy_iteration(n2));
  }
  return n == 1;
}

#include <iostream>

int main()
{
  unsigned int current_number = 1;

  unsigned int happy_count = 0;
  while (happy_count != 8)
  {
    if (is_happy(current_number))
    {
      std::cout << current_number << " ";
      ++happy_count;
    }
    ++current_number;
  }
  std::cout << std::endl;
}
```

Output:

```txt
1 7 10 13 19 23 28 31
```

Cycle detection in <code>is_happy()</code> above is done using [[wp:Floyd's cycle-finding algorithm|Floyd's cycle-finding algorithm]].

## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HappyNums
{
    class Program
    {
        public static bool ishappy(int n)
        {
            List<int> cache = new List<int>();
            int sum = 0;
            while (n != 1)
            {
                if (cache.Contains(n))
                {
                    return false;
                }
                cache.Add(n);
                while (n != 0)
                {
                    int digit = n % 10;
                    sum += digit * digit;
                    n /= 10;
                }
                n = sum;
                sum = 0;
            }
           return true;
        }

        static void Main(string[] args)
        {
            int num = 1;
            List<int> happynums = new List<int>();

            while (happynums.Count < 8)
            {
                if (ishappy(num))
                {
                    happynums.Add(num);
                }
                num++;
            }
            Console.WriteLine("First 8 happy numbers : " + string.Join(",", happynums));
        }
    }
}
```


```txt

First 8 happy numbers : 1,7,10,13,19,23,28,31

```


===Alternate (cacheless)===
Instead of caching and checking for being stuck in a loop, one can terminate on the "unhappy" endpoint of 89.  One might be temped to try caching the so-far-found happy and unhappy numbers and checking the cache to speed things up.  However, I have found that the cache implementation overhead reduces performance compared to this cacheless version.<br/>
Reaching 10 million, the <34 second computation time was from Tio.run.  It takes under 5 seconds on a somewhat modern CPU.  If you edit it to max out at 100 million, it takes about 50 seconds (on the somewhat modern CPU).
```c#
using System;
using System.Collections.Generic;
class Program
{

    static int[] sq = { 1, 4, 9, 16, 25, 36, 49, 64, 81 };

    static bool isOne(int x)
    {
        while (true)
        {
            if (x == 89) return false;
            int s = 0, t;
            do if ((t = (x % 10) - 1) >= 0) s += sq[t]; while ((x /= 10) > 0);
            if (s == 1) return true;
            x = s;
        }
    }

    static void Main(string[] args)
    {
        const int Max = 10_000_000; DateTime st = DateTime.Now;
        Console.Write("---Happy Numbers---\nThe first 8:");
        int c = 0, i; for (i = 1; c < 8; i++)
            if (isOne(i)) Console.Write("{0} {1}", c == 0 ? "" : ",", i, ++c);
        for (int m = 10; m <= Max; m *= 10)
        {
            Console.Write("\nThe {0:n0}th: ", m);
            for (; c < m; i++) if (isOne(i)) c++;
            Console.Write("{0:n0}", i - 1);
        }
        Console.WriteLine("\nComputation time {0} seconds.", (DateTime.Now - st).TotalSeconds);
    }
}
```

```txt
---Happy Numbers---
The first 8: 1, 7, 10, 13, 19, 23, 28, 31
The 10th: 44
The 100th: 694
The 1,000th: 6,899
The 10,000th: 67,169
The 100,000th: 692,961
The 1,000,000th: 7,105,849
The 10,000,000th: 71,313,350
Computation time 33.264518 seconds.
```



## Clojure


```clojure
(defn happy? [n]
  (loop [n n, seen #{}]
    (cond
      (= n 1)  true
      (seen n) false
      :else
        (recur (->> (str n)
                    (map #(Character/digit % 10))
                    (map #(* % %))
                    (reduce +))
               (conj seen n)))))

(def happy-numbers (filter happy? (iterate inc 1)))

(println (take 8 happy-numbers))
```

Output:
```txt
(1 7 10 13 19 23 28 31)
```

===Alternate Version (with caching)===

```clojure
(require '[clojure.set :refer [union]])

(def ^{:private true} cache {:happy (atom #{}) :sad (atom #{})})

(defn break-apart [n]
  (->> (str n)
       (map str)
       (map #(Long/parseLong %))))

(defn next-number [n]
  (->> (break-apart n)
       (map #(* % %))
       (apply +)))

(defn happy-or-sad? [prev n]
  (cond (or (= n 1) ((deref (:happy cache)) n)) :happy
	(or ((deref (:sad cache)) n) (some #(= % n) prev)) :sad
	:else :unknown))

(defn happy-algo [n]
  (let [get-next (fn [[prev n]] [(conj prev n) (next-number n)])
	my-happy-or-sad? (fn [[prev n]] [(happy-or-sad? prev n) (conj prev n)])
	unknown? (fn [[res nums]] (= res :unknown))
	[res nums] (->> [#{} n]
			(iterate get-next)
			(map my-happy-or-sad?)
			(drop-while unknown?)
			first)
	_ (swap! (res cache) union nums)]
    res))

(def happy-numbers (->> (iterate inc 1)
                        (filter #(= :happy (happy-algo %)))))

(println (take 8 happy-numbers))
```

Same output.


## CoffeeScript


```coffeescript
happy = (n) ->
  seen = {}
  while true
    n = sum_digit_squares(n)
    return true if n == 1
    return false if seen[n]
    seen[n] = true

sum_digit_squares = (n) ->
  sum = 0
  for c in n.toString()
    d = parseInt(c)
    sum += d*d
  sum

i = 1
cnt = 0
while cnt < 8
  if happy(i)
    console.log i
    cnt += 1
  i += 1
```

output

```txt

> coffee happy.coffee
1
7
10
13
19
23
28
31

```



## Common Lisp


```lisp
(defun sqr (n)
  (* n n))

(defun sum-of-sqr-dgts (n)
  (loop for i = n then (floor i 10)
        while (plusp i)
        sum (sqr (mod i 10))))

(defun happy-p (n &optional cache)
  (or (= n 1)
      (unless (find n cache)
        (happy-p (sum-of-sqr-dgts n)
                 (cons n cache)))))

(defun happys (&aux (happys 0))
  (loop for i from 1
        while (< happys 8)
        when (happy-p i)
        collect i and do (incf happys)))

(print (happys))

```


Output:
```txt
(1 7 10 13 19 23 28 31)
```



## D


```d
bool isHappy(int n) pure nothrow {
    int[int] past;

    while (true) {
        int total = 0;
        while (n > 0) {
            total += (n % 10) ^^ 2;
            n /= 10;
        }
        if (total == 1)
            return true;
        if (total in past)
            return false;
        n = total;
        past[total] = 0;
    }
}

void main() {
    import std.stdio, std.algorithm, std.range;

    int.max.iota.filter!isHappy.take(8).writeln;
}
```

```txt
[1, 7, 10, 13, 19, 23, 28, 31]
```


### Alternative Version


```d
import std.stdio, std.algorithm, std.range, std.conv, std.string;

bool isHappy(int n) pure nothrow {
    int[int] seen;

    while (true) {
        immutable t = n.text.representation.map!q{(a - '0') ^^ 2}.sum;
        if (t == 1)
            return true;
        if (t in seen)
            return false;
        n = t;
        seen[t] = 0;
    }
}

void main() {
    int.max.iota.filter!isHappy.take(8).writeln;
}
```

Same output.


## Dart


```dart
main() {
  HashMap<int,bool> happy=new HashMap<int,bool>();
  happy[1]=true;

  int count=0;
  int i=0;

  while(count<8) {
    if(happy[i]==null) {
      int j=i;
      Set<int> sequence=new Set<int>();
      while(happy[j]==null && !sequence.contains(j)) {
        sequence.add(j);
        int sum=0;
        int val=j;
        while(val>0) {
          int digit=val%10;
          sum+=digit*digit;
          val=(val/10).toInt();
        }
        j=sum;
      }
      bool sequenceHappy=happy[j];
      Iterator<int> it=sequence.iterator();
      while(it.hasNext()) {
        happy[it.next()]=sequenceHappy;
      }
    }
    if(happy[i]) {
      print(i);
      count++;
    }
    i++;
  }
}
```



## dc


```dc
[lcI~rscd*+lc0<H]sH
[0rsclHxd4<h]sh
[lIp]s_
0sI[lI1+dsIlhx2>_z8>s]dssx
```

Output:

```txt
1
7
10
13
19
23
28
31
```


## DCL


```DCL
$ happy_1 = 1
$ found = 0
$ i = 1
$ loop1:
$  n = i
$  seen_list = ","
$  loop2:
$   if f$type( happy_'n ) .nes. "" then $ goto happy
$   if f$type( unhappy_'n ) .nes. "" then $ goto unhappy
$   if f$locate( "," + n + ",", seen_list ) .eq. f$length( seen_list )
$   then
$    seen_list = seen_list + f$string( n ) + ","
$   else
$    goto unhappy
$   endif
$   ns = f$string( n )
$   nl = f$length( ns )
$   j = 0
$   sumsq = 0
$   loop3:
$    digit = f$integer( f$extract( j, 1, ns ))
$    sumsq = sumsq + digit * digit
$    j = j + 1
$    if j .lt. nl then $ goto loop3
$    n = sumsq
$   goto loop2
$  unhappy:
$  j = 1
$  loop4:
$   x = f$element( j, ",", seen_list )
$   if x .eqs. "" then $ goto continue
$   unhappy_'x = 1
$   j = j + 1
$   goto loop4
$  happy:
$  found = found + 1
$  found_'found = i
$  if found .eq. 8 then $ goto done
$  j = 1
$  loop5:
$   x = f$element( j, ",", seen_list )
$   if x .eqs. "" then $ goto continue
$   happy_'x = 1
$   j = j + 1
$   goto loop5
$  continue:
$  i = i + 1
$  goto loop1
$ done:
$ show symbol found*
```

```txt
  FOUND = 8   Hex = 00000008  Octal = 00000000010
  FOUND_1 = 1   Hex = 00000001  Octal = 00000000001
  FOUND_2 = 7   Hex = 00000007  Octal = 00000000007
  FOUND_3 = 10   Hex = 0000000A  Octal = 00000000012
  FOUND_4 = 13   Hex = 0000000D  Octal = 00000000015
  FOUND_5 = 19   Hex = 00000013  Octal = 00000000023
  FOUND_6 = 23   Hex = 00000017  Octal = 00000000027
  FOUND_7 = 28   Hex = 0000001C  Octal = 00000000034
  FOUND_8 = 31   Hex = 0000001F  Octal = 00000000037
```


=={{header|Déjà Vu}}==

```dejavu
next-num:
	0
	while over:
		over
		* dup % swap 10
		+
		swap floor / swap 10 swap
	drop swap

is-happy happies n:
	if has happies n:
		return happies! n
	local :seq set{ n }
	n
	while /= 1 dup:
		next-num
		if has seq dup:
			drop
			set-to happies n false
			return false
		if has happies dup:
			set-to happies n dup happies!
			return
		set-to seq over true
	drop
	set-to happies n true
	true

local :h {}
1 0
while > 8 over:
	if is-happy h dup:
		!print( "A happy number: " over )
		swap ++ swap
	++
drop
drop
```

```txt
A happy number: 1
A happy number: 7
A happy number: 10
A happy number: 13
A happy number: 19
A happy number: 23
A happy number: 28
A happy number: 31
```



## DWScript


```delphi
function IsHappy(n : Integer) : Boolean;
var
   cache : array of Integer;
   sum : Integer;
begin
   while True do begin
      sum := 0;
      while n>0 do begin
         sum += Sqr(n mod 10);
         n := n div 10;
      end;
      if sum = 1 then
         Exit(True);
      if sum in cache then
         Exit(False);
      n := sum;
      cache.Add(sum);
   end;
end;

var n := 8;
var i : Integer;

while n>0 do begin
   Inc(i);
   if IsHappy(i) then begin
      PrintLn(i);
      Dec(n);
   end;
end;
```

Output:

```txt
1
7
10
13
19
23
28
31
```



## Dyalect



```dyalect
func happy(n) {
    var m = []
    while n > 1 {
        m.add(n)
        var x = n
        n = 0
        while x > 0 {
            var d = x % 10
            n += d * d
            x /= 10
        }
        if m.indexOf(n) != -1 {
            return false
        }
    }
    return true
}

var (n, found) = (1, 0)
while found < 8 {
    if happy(n) {
        print("\(n) ", terminator: "")
        found += 1
    }
    n += 1
}
print()
```


```txt
1 7 10 13 19 23 28 31
```



## E

```e
def isHappyNumber(var x :int) {
  var seen := [].asSet()
  while (!seen.contains(x)) {
    seen with= x
    var sum := 0
    while (x > 0) {
      sum += (x % 10) ** 2
      x //= 10
    }
    x := sum
    if (x == 1) { return true }
  }
  return false
}

var count := 0
for x ? (isHappyNumber(x)) in (int >= 1) {
  println(x)
  if ((count += 1) >= 8) { break }
}
```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
			-- Run application.
		local
			l_val: INTEGER
		do
			from
				l_val := 1
			until
				l_val > 100
			loop
				if is_happy_number (l_val) then
					print (l_val.out)
					print ("%N")
				end
				l_val := l_val + 1
			end
		end

feature -- Happy number

	is_happy_number (a_number: INTEGER): BOOLEAN
			-- Is `a_number' a happy number?
		require
			positive_number: a_number > 0
		local
			l_number: INTEGER
			l_set: ARRAYED_SET [INTEGER]
		do
			from
				l_number := a_number
				create l_set.make (10)
			until
				l_number = 1 or l_set.has (l_number)
			loop
				l_set.put (l_number)
				l_number := square_sum_of_digits (l_number)
			end

			Result := (l_number = 1)
		end

feature{NONE} -- Implementation

	square_sum_of_digits (a_number: INTEGER): INTEGER
			-- Sum of the sqares of digits of `a_number'.
		require
			positive_number: a_number > 0
		local
			l_number, l_digit: INTEGER
		do
			from
				l_number := a_number
			until
				l_number = 0
			loop
				l_digit := l_number \\ 10
				Result := Result + l_digit * l_digit
				l_number := l_number // 10
			end
		end

end


```


## Elena

ELENA 4.x :

```elena
import extensions;
import system'collections;
import system'routines;

isHappy(int n)
{
    auto cache := new List<int>(5);
    int sum := 0;
    int num := n;
    while (num != 1)
    {
        if (cache.indexOfElement:num != -1)
        {
            ^ false
        };
        cache.append(num);
        while (num != 0)
        {
            int digit := num.mod:10;
            sum += (digit*digit);
            num /= 10
        };
        num := sum;
        sum := 0
    };

    ^ true
}

public program()
{
    auto happynums  := new List<int>(8);
    int num := 1;
    while (happynums.Length < 8)
    {
        if (isHappy(num))
        {
            happynums.append(num)
        };

        num += 1
    };
    console.printLine("First 8 happy numbers: ", happynums.asEnumerable())
}
```

```txt

First 8 happy numbers: 1,7,10,13,19,23,28,31

```



## Elixir


```elixir
defmodule Happy do
  def task(num) do
    Process.put({:happy, 1}, true)
    Stream.iterate(1, &(&1+1))
    |> Stream.filter(fn n -> happy?(n) end)
    |> Enum.take(num)
  end

  defp happy?(n) do
    sum = square_sum(n, 0)
    val = Process.get({:happy, sum})
    if val == nil do
      Process.put({:happy, sum}, false)
      val = happy?(sum)
      Process.put({:happy, sum}, val)
    end
    val
  end

  defp square_sum(0, sum), do: sum
  defp square_sum(n, sum) do
    r = rem(n, 10)
    square_sum(div(n, 10), sum + r*r)
  end
end

IO.inspect Happy.task(8)
```


```txt

[1, 7, 10, 13, 19, 23, 28, 31]

```



## Erlang


```Erlang
-module(tasks).
-export([main/0]).
-import(lists, [map/2, member/2, sort/1, sum/1]).

is_happy(X, XS) ->
    if
	X == 1 ->
	    true;
	X < 1 ->
	    false;
	true ->
	    case member(X, XS) of
		true -> false;
		false ->
		    is_happy(sum(map(fun(Z) -> Z*Z end,
				     [Y - 48 || Y <- integer_to_list(X)])),
			     [X|XS])
	    end
    end.

main(X, XS) ->
    if
	length(XS) == 8 ->
	    io:format("8 Happy Numbers: ~w~n", [sort(XS)]);
	true ->
	    case is_happy(X, []) of
		true -> main(X + 1, [X|XS]);
		false -> main(X + 1, XS)
	    end
    end.
main() ->
    main(0, []).

```

Command:
```Bash
erl -run tasks main -run init stop -noshell
```

Output:
```Bash
8 Happy Numbers: [1,7,10,13,19,23,28,31]
```


In a more functional style (assumes integer_to_list/1 will convert to the ASCII value of a number, which then has to be converted to the integer value by subtracting 48):

```Erlang
-module(tasks).

-export([main/0]).

main() -> io:format("~w ~n", [happy_list(1, 8, [])]).

happy_list(_, N, L) when length(L) =:= N -> lists:reverse(L);
happy_list(X, N, L) ->
	Happy = is_happy(X),
	if Happy -> happy_list(X + 1, N, [X|L]);
	true -> happy_list(X + 1, N, L) end.

is_happy(1) -> true;
is_happy(4) -> false;
is_happy(N) when N > 0 ->
	N_As_Digits = [Y - 48 || Y <- integer_to_list(N)],
	is_happy(lists:foldl(fun(X, Sum) -> (X * X) + Sum end, 0, N_As_Digits));
is_happy(_) -> false.
```

Output:

```txt
[1,7,10,13,19,23,28,31]
```



## Euphoria


```euphoria
function is_happy(integer n)
    sequence seen
    integer k
    seen = {}
    while n > 1 do
        seen &= n
        k = 0
        while n > 0 do
            k += power(remainder(n,10),2)
            n = floor(n/10)
        end while
        n = k
        if find(n,seen) then
            return 0
        end if
    end while
    return 1
end function

integer n,count
n = 1
count = 0
while count < 8 do
    if is_happy(n) then
        ? n
        count += 1
    end if
    n += 1
end while
```

Output:

```txt
1
7
10
13
19
23
28
31

```


=={{header|F_Sharp|F#}}==
This requires the F# power pack to be referenced and the 2010 beta of F#

```fsharp
open System.Collections.Generic
open Microsoft.FSharp.Collections

let answer =
    let sqr x = x*x                                                 // Classic square definition
    let rec AddDigitSquare n =
        match n with
        | 0 -> 0                                                    // Sum of squares for 0 is 0
        | _ -> sqr(n % 10) + (AddDigitSquare (n / 10))              // otherwise add square of bottom digit to recursive call
    let dict = new Dictionary<int, bool>()                          // Dictionary to memoize values
    let IsHappy n =
        if dict.ContainsKey(n) then                                 // If we've already discovered it
            dict.[n]                                                // Return previously discovered value
        else
            let cycle = new HashSet<_>(HashIdentity.Structural)     // Set to keep cycle values in
            let rec isHappyLoop n =
                if cycle.Contains n then n = 1                      // If there's a loop, return true if it's 1
                else
                    cycle.Add n |> ignore                           // else add this value to the cycle
                    isHappyLoop (AddDigitSquare n)                  // and check the next number in the cycle
            let f = isHappyLoop n                                   // Keep track of whether we're happy or not
            cycle |> Seq.iter (fun i -> dict.[i] <- f)              // and apply it to all the values in the cycle
            f                                                       // Return the boolean

    1                                                               // Starting with 1,
    |> Seq.unfold (fun i -> Some (i, i + 1))                        // make an infinite sequence of consecutive integers
    |> Seq.filter IsHappy                                           // Keep only the happy ones
    |> Seq.truncate 8                                               // Stop when we've found 8
    |> Seq.iter (Printf.printf "%d\n")				    // Print results

```

Output:

```txt

1
7
10
13
19
23
28
31

```



## Factor


```factor
USING: combinators kernel make math sequences ;

: squares ( n -- s )
    0 [ over 0 > ] [ [ 10 /mod sq ] dip + ] while nip ;

: (happy?) ( n1 n2 -- ? )
    [ squares ] [ squares squares ] bi* {
        { [ dup 1 = ] [ 2drop t ] }
        { [ 2dup = ] [ 2drop f ] }
        [ (happy?) ]
    } cond ;

: happy? ( n -- ? )
    dup (happy?) ;

: happy-numbers ( n -- seq )
    [
        0 [ over 0 > ] [
            dup happy? [ dup , [ 1 - ] dip ] when 1 +
        ] while 2drop
    ] { } make ;
```

```factor
8 happy-numbers ! { 1 7 10 13 19 23 28 31 }
```



## FALSE


```false
[$10/$10*@\-$*\]m:             {modulo squared and division}
[$m;![$9>][m;!@@+\]#$*+]s:     {sum of squares}
[$0[1ø1>][1ø3+ø3ø=|\1-\]#\%]f: {look for duplicates}

{check happy number}
[
  $1[f;!~2ø1=~&][1+\s;!@]#     {loop over sequence until 1 or duplicate}
  1ø1=                         {return value}
  \[$0=~][@%1-]#%              {drop sequence and counter}
]h:

0 1
"Happy numbers:"
[1ø8=~][h;![" "$.\1+\]?1+]#
%%
```


```txt
Happy numbers: 1 7 10 13 19 23 28 31
```



## Fantom


```fantom
class Main
{
  static Bool isHappy (Int n)
  {
    Int[] record := [,]
    while (n != 1 && !record.contains(n))
    {
      record.add (n)
      // find sum of squares of digits
      newn := 0
      while (n > 0)
      {
        newn += (n.mod(10) * n.mod(10))
        n = n.div(10)
      }
      n = newn
    }
    return (n == 1)
  }

  public static Void main ()
  {
    i := 1
    count := 0
    while (count < 8)
    {
      if (isHappy (i))
      {
        echo (i)
        count += 1
      }
      i += 1
    }
  }
}

```

Output:

```txt

1
7
10
13
19
23
28
31

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Happy_numbers this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth
: next ( n -- n )
  0 swap begin 10 /mod >r  dup * +  r> ?dup 0= until ;

: cycle? ( n -- ? )
  here dup @ cells +
  begin dup here >
  while 2dup @ = if 2drop true exit then
        1 cells -
  repeat
  1 over +!  dup @ cells + !  false ;

: happy? ( n -- ? )
  0 here !  begin next dup cycle? until  1 = ;

: happy-numbers ( n -- )
  0 swap 0 do
    begin 1+ dup happy? until dup .
  loop drop ;

8 happy-numbers  \ 1 7 10 13 19 23 28 31
```



### Lookup Table

Every sequence either ends in 1, or contains a 4 as part of a cycle.  Extending the table through 9 is a (modest) optimization/memoization.  This executes '500000 happy-numbers' about 5 times faster than the above solution.

```forth
CREATE HAPPINESS 0 C, 1 C, 0 C, 0 C, 0 C, 0 C, 0 C, 1 C, 0 C, 0 C,
: next ( n -- n')
   0 swap BEGIN dup WHILE 10 /mod >r  dup * +  r> REPEAT drop ;
: happy? ( n -- t|f)
   BEGIN dup 10 >= WHILE next REPEAT  chars HAPPINESS + C@ 0<> ;
: happy-numbers ( n --)  >r 0
   BEGIN r@ WHILE
     BEGIN 1+ dup happy? UNTIL dup . r> 1- >r
   REPEAT r> drop drop ;
8 happy-numbers
```

```txt
1 7 10 13 19 23 28 31
```

Produces the 1 millionth happy number with:

```forth
: happy-number ( n -- n')  \ produce the nth happy number
   >r 0  BEGIN r@ WHILE
     BEGIN 1+ dup happy? UNTIL  r> 1- >r
   REPEAT r> drop ;
1000000 happy-number .  \ 7105849
```

in about 9 seconds.


## Fortran


```fortran
program happy

  implicit none
  integer, parameter :: find = 8
  integer :: found
  integer :: number

  found = 0
  number = 1
  do
    if (found == find) then
      exit
    end if
    if (is_happy (number)) then
      found = found + 1
      write (*, '(i0)') number
    end if
    number = number + 1
  end do

contains

  function sum_digits_squared (number) result (result)

    implicit none
    integer, intent (in) :: number
    integer :: result
    integer :: digit
    integer :: rest
    integer :: work

    result = 0
    work = number
    do
      if (work == 0) then
        exit
      end if
      rest = work / 10
      digit = work - 10 * rest
      result = result + digit * digit
      work = rest
    end do

  end function sum_digits_squared

  function is_happy (number) result (result)

    implicit none
    integer, intent (in) :: number
    logical :: result
    integer :: turtoise
    integer :: hare

    turtoise = number
    hare = number
    do
      turtoise = sum_digits_squared (turtoise)
      hare = sum_digits_squared (sum_digits_squared (hare))
      if (turtoise == hare) then
        exit
      end if
    end do
    result = turtoise == 1

  end function is_happy

end program happy
```

Output:

```txt
1
7
10
13
19
23
28
31
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function isHappy(n As Integer) As Boolean
  If n < 0 Then Return False
  ' Declare a dynamic array to store previous sums.
  ' If a previous sum is duplicated before a sum of 1 is reached
  ' then the number can't be "happy" as the cycle will just repeat
  Dim prevSums() As Integer
  Dim As Integer digit, ub, sum = 0
  Do
    While n > 0
      digit = n Mod 10
      sum += digit * digit
      n \= 10
    Wend
    If sum = 1 Then Return True
    ub = UBound(prevSums)
    If ub > -1 Then
      For i As Integer = 0 To ub
         If sum = prevSums(i) Then Return False
      Next
    End If
    ub += 1
    Redim Preserve prevSums(0 To ub)
    prevSums(ub) = sum
    n = sum
    sum  = 0
  Loop
End Function

Dim As Integer n = 1, count = 0

Print "The first 8 happy numbers are : "
Print
While count < 8
  If isHappy(n) Then
    count += 1
    Print count;" =>"; n
  End If
  n += 1
Wend
Print
Print "Press any key to quit"
Sleep
```


```txt

 1 => 1
 2 => 7
 3 => 10
 4 => 13
 5 => 19
 6 => 23
 7 => 28
 8 => 31

```



## Frege


```frege
module Happy where

import Prelude.Math
-- ugh, since Frege doesn't have Set, use Map instead
import Data.Map (member, insertMin, empty emptyMap)

digitToInteger :: Char -> Integer
digitToInteger c = fromInt $ (ord c) - (ord '0')

isHappy :: Integer -> Bool
isHappy = p emptyMap
  where p _ 1n = true
        p s n | n `member` s = false
              | otherwise  = p (insertMin n () s) (f n)
        f = sum . map (sqr . digitToInteger) . unpacked . show

main _ = putStrLn $ unwords $ map show $ take 8 $ filter isHappy $ iterate (+ 1n) 1n
```


```txt

1 7 10 13 19 23 28 31
runtime 0.614 wallclock seconds.

```



## Go


```go
package main

import "fmt"

func happy(n int) bool {
	m := make(map[int]bool)
	for n > 1 {
		m[n] = true
		var x int
		for x, n = n, 0; x > 0; x /= 10 {
			d := x % 10
			n += d * d
		}
		if m[n] {
			return false
		}
	}
	return true
}

func main() {
	for found, n := 0, 1; found < 8; n++ {
		if happy(n) {
			fmt.Print(n, " ")
			found++
		}
	}
	fmt.Println()
}
```

```txt

1 7 10 13 19 23 28 31

```



## Groovy


```groovy
Number.metaClass.isHappy = {
    def number = delegate as Long
    def cycle = new HashSet<Long>()
    while (number != 1 && !cycle.contains(number)) {
        cycle << number
        number = (number as String).collect { d = (it as Long); d * d }.sum()
    }
    number == 1
}

def matches = []
for (int i = 0; matches.size() < 8; i++) {
    if (i.happy) { matches << i }
}
println matches
```

```txt
[1, 7, 10, 13, 19, 23, 28, 31]
```



## Harbour


```xbase
PROCEDURE Main()
   LOCAL i := 8, nH := 0

   ? hb_StrFormat( "The first %d happy numbers are:", i )
   ?

   WHILE i > 0
      IF IsHappy( ++nH )
	?? hb_NtoS( nH ) + " "
	--i
      ENDIF
   END

   RETURN

STATIC FUNCTION IsHappy( nNumber )
   STATIC aUnhappy := {}
   LOCAL nDigit, nSum := 0, cNumber := hb_NtoS( nNumber )

   FOR EACH nDigit IN cNumber
      nSum += Val( nDigit ) ^ 2
   NEXT

   IF nSum == 1
      aUnhappy := {}
      RETURN .T.
   ELSEIF AScan( aUnhappy, nSum ) > 0
     RETURN .F.
   ENDIF

   AAdd( aUnhappy, nSum )

   RETURN IsHappy( nSum )
```

Output:

  The first 8 happy numbers are:
  1 7 10 13 19 23 28 31


## Haskell


```haskell
import Data.Char (digitToInt)
import Data.Set (member, insert, empty)

isHappy :: Integer -> Bool
isHappy = p empty
  where
    p _ 1 = True
    p s n
      | n `member` s = False
      | otherwise = p (insert n s) (f n)
    f = sum . fmap ((^ 2) . toInteger . digitToInt) . show

main :: IO ()
main = mapM_ print $ take 8 $ filter isHappy [1 ..]
```

```txt
1
7
10
13
19
23
28
31
```


We can create a cache for small numbers to greatly speed up the process:

```haskell
import Data.Array (Array, (!), listArray)

happy :: Int -> Bool
happy x
  | xx <= 150 = seen ! xx
  | otherwise = happy xx
  where
    xx = dsum x
    seen :: Array Int Bool
    seen =
      listArray (1, 150) $ True : False : False : False : (happy <$> [5 .. 150])
    dsum n
      | n < 10 = n * n
      | otherwise =
        let (q, r) = n `divMod` 10
        in r * r + dsum q

main :: IO ()
main = print $ sum $ take 10000 $ filter happy [1 ..]
```

```txt
327604323
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)
local n
n := arglist[1] | 8    # limiting number of happy numbers to generate, default=8
writes("The first ",n," happy numbers are:")
every writes(" ", happy(seq()) \ n )
write()
end

procedure happy(i)    #: returns i if i is happy
local n

    if  4 ~= (0 <= i) then { # unhappy if negative, 0, or 4
        if i = 1 then return i
        every (n := 0) +:= !i ^ 2
        if happy(n) then return i
        }
end
```

Usage and Output:

```txt

| happynum.exe

The first 8 happy numbers are: 1 7 10 13 19 23 28 31

```



## J


```j
   8{. (#~1=+/@(*:@(,.&.":))^:(1&~:*.4&~:)^:_ "0) 1+i.100
1 7 10 13 19 23 28 31
```

This is a repeat while construction

```j
 f ^: cond ^: _   input
```

that produces an array of 1's and 4's, which is converted to 1's and 0's forming a binary array having a 1 for a happy number. Finally the happy numbers are extracted by a binary selector.

```j
 (binary array) # 1..100
```

So for easier reading the solution could be expressed as:

```j
   cond=: 1&~: *. 4&~:     NB. not equal to 1 and not equal to 4
   sumSqrDigits=: +/@(*:@(,.&.":))

   sumSqrDigits 123        NB. test sum of squared digits
14
   8{. (#~ 1 = sumSqrDigits ^: cond ^:_ "0) 1 + i.100
1 7 10 13 19 23 28 31
```



## Java

```java5
import java.util.HashSet;
public class Happy{
   public static boolean happy(long number){
       long m = 0;
       int digit = 0;
       HashSet<Long> cycle = new HashSet<Long>();
       while(number != 1 && cycle.add(number)){
           m = 0;
           while(number > 0){
               digit = (int)(number % 10);
               m += digit*digit;
               number /= 10;
           }
           number = m;
       }
       return number == 1;
   }

   public static void main(String[] args){
       for(long num = 1,count = 0;count<8;num++){
           if(happy(num)){
               System.out.println(num);
               count++;
           }
       }
   }
}
```

Output:

```txt
1
7
10
13
19
23
28
31
```




### Java 1.8

```java


import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

public class HappyNumbers {


    public static void main(String[] args) {

        for (int current = 1, total = 0; total < 8; current++)
            if (isHappy(current)) {
                System.out.println(current);
                total++;
            }
    }


    public static boolean isHappy(int number) {
        HashSet<Integer> cycle = new HashSet<>();
        while (number != 1 && cycle.add(number)) {
            List<String> numStrList = Arrays.asList(String.valueOf(number).split(""));
            number = numStrList.stream().map(i -> Math.pow(Integer.parseInt(i), 2)).mapToInt(i -> i.intValue()).sum();
        }
        return number == 1;
    }
}
```

Output:

```txt
1
7
10
13
19
23
28
31
```



## JavaScript



### ES5


### =Iteration=


```javascript
function happy(number) {
    var m, digit ;
    var cycle = [] ;

    while(number != 1 && cycle[number] !== true) {
        cycle[number] = true ;
        m = 0 ;
        while (number > 0) {
            digit = number % 10 ;
            m += digit * digit ;
            number = (number  - digit) / 10 ;
        }
        number = m ;
    }
    return (number == 1) ;
}

var cnt = 8 ;
var number = 1 ;

while(cnt-- > 0) {
    while(!happy(number))
        number++ ;
    document.write(number + " ") ;
    number++ ;
}
```

Output:

```txt
1 7 10 13 19 23 28 31
```



### ES6


### =Functional composition=

```JavaScript
(() => {

    // isHappy :: Int -> Bool
    const isHappy = n => {
        const f = n =>
            foldl(
                (a, x) => a + raise(read(x), 2), // ^2
                0,
                splitOn('', show(n))
            ),
            p = (s, n) => n === 1 ? (
                true
            ) : member(n, s) ? (
                false
            ) : p(
                insert(n, s), f(n)
            );
        return p(new Set(), n);
    };

    // GENERIC FUNCTIONS ------------------------------------------------------

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => xs.filter(f);

    // foldl :: (b -> a -> b) -> b -> [a] -> b
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // insert :: Ord a => a -> Set a -> Set a
    const insert = (e, s) => s.add(e);

    // member :: Ord a => a -> Set a -> Bool
    const member = (e, s) => s.has(e);

    // read :: Read a => String -> a
    const read = JSON.parse;

    // show :: a -> String
    const show = x => JSON.stringify(x);

    // splitOn :: String -> String -> [String]
    const splitOn = (cs, xs) => xs.split(cs);

    // raise :: Num -> Int -> Num
    const raise = (n, e) => Math.pow(n, e);

    // take :: Int -> [a] -> [a]
    const take = (n, xs) => xs.slice(0, n);

    // TEST -------------------------------------------------------------------
    return show(
        take(8, filter(isHappy, enumFromTo(1, 50)))
    );
})()
```

```JavaScript
[1, 7, 10, 13, 19, 23, 28, 31]
```


Or, to stop immediately at the 8th member of the series, we can preserve functional composition while using an iteratively implemented '''until()''' function:

```JavaScript
(() => {

    // isHappy :: Int -> Bool
    const isHappy = n => {
        const f = n =>
            foldl(
                (a, x) => a + raise(read(x), 2), // ^2
                0,
                splitOn('', show(n))
            ),
            p = (s, n) => n === 1 ? (
                true
            ) : member(n, s) ? (
                false
            ) : p(
                insert(n, s), f(n)
            );
        return p(new Set(), n);
    };

    // GENERIC FUNCTIONS ------------------------------------------------------

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => xs.filter(f);

    // foldl :: (b -> a -> b) -> b -> [a] -> b
    const foldl = (f, a, xs) => xs.reduce(f, a);

    // insert :: Ord a => a -> Set a -> Set a
    const insert = (e, s) => s.add(e);

    // member :: Ord a => a -> Set a -> Bool
    const member = (e, s) => s.has(e);

    // read :: Read a => String -> a
    const read = JSON.parse;

    // show :: a -> String
    const show = x => JSON.stringify(x);

    // splitOn :: String -> String -> [String]
    const splitOn = (cs, xs) => xs.split(cs);

    // raise :: Num -> Int -> Num
    const raise = (n, e) => Math.pow(n, e);

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    // TEST -------------------------------------------------------------------
    return show(
        until(
            m => m.xs.length === 8,
            m => {
                const n = m.n;
                return {
                    n: n + 1,
                    xs: isHappy(n) ? m.xs.concat(n) : m.xs
                };
            }, {
                n: 1,
                xs: []
            }
        )
        .xs
    );
})();
```

```JavaScript
[1, 7, 10, 13, 19, 23, 28, 31]
```



## jq

```jq
def is_happy_number:
  def next: tostring | explode | map( (. - 48) | .*.) | add;
  def last(g): reduce g as $i (null; $i);
  # state: either 1 or [i, o]
  # where o is an an object with the previously encountered numbers as keys
  def loop:
   recurse( if      . == 1 then empty    # all done
            elif .[0] == 1 then 1        # emit 1
            else (.[0]| next) as $n
            | if $n == 1 then 1
              elif .[1]|has($n|tostring) then empty
              else [$n, (.[1] + {($n|tostring):true}) ]
              end
            end );
  1 == last( [.,{}] | loop );
```

'''Emit a stream of the first n happy numbers''':

```jq
# Set n to -1 to continue indefinitely:
def happy(n):
  def subtask:  # state: [i, found]
    if .[1] == n then empty
    else .[0] as $n
    | if ($n | is_happy_number) then $n, ([ $n+1, .[1]+1 ] | subtask)
      else  (.[0] += 1) | subtask
      end
    end;
    [0,0] | subtask;

happy($n|tonumber)
```

```sh
$ jq --arg n 8 -n -f happy.jq
1
7
10
13
19
23
28
31

```



## Julia


```julia

function happy(x)
	happy_ints = ref(Int)
	int_try = 1
	while length(happy_ints) < x
		n = int_try
		past = ref(Int)
		while n != 1
	        	n = sum([y^2 for y in digits(n)])
	        	contains(past,n) ? break : push!(past,n)
	    	end
		n == 1 && push!(happy_ints,int_try)
		int_try += 1
	end
	return happy_ints
end
```

Output

```txt
 julia> happy(8)
8-element Int32 Array:
  1
  7
 10
 13
 19
 23
 28
 31
```

A recursive version:

```julia
sumhappy(n) = sum(x->x^2, digits(n))

function ishappy(x, mem = [])
  x == 1?   true :
  x in mem? false :
  ishappy(sumhappy(x),[mem ; x])
end

nexthappy (x) = ishappy(x+1) ? x+1 : nexthappy(x+1)

happy(n) = [z = 1 ; [z = nexthappy(z) for i = 1:n-1]]

```

```txt
julia> show(happy(8))
[1,7,10,13,19,23,28,31,32]
```


Alternate, Translation of C

Faster with use of cache
```julia
const CACHE = 256
buf = zeros(Int,CACHE)
buf[1] = 1
#happy(n) returns 1 if happy, 0 if not
function happy(n)
	if n < CACHE
		buf[n] > 0 && return 2-buf[n]
		buf[n] = 2
	end
	sum = 0
	nn = n
	while nn != 0
		x = nn%10
		sum += x*x
		nn = int8(nn/10)
	end
	x = happy(sum)
	n < CACHE && (buf[n] = 2-x)
	return x
end
function main()
	i = 1; counter = 1000000
	while counter > 0
		if happy(i) == 1
			counter -= 1
		end
		i += 1
	end
	return i-1
end
```



## K


```k
  hpy: {x@&1={~|/x=1 4}{_+/_sqr 0$'$x}//:x}

  hpy 1+!100
1 7 10 13 19 23 28 31 32 44 49 68 70 79 82 86 91 94 97 100

  8#hpy 1+!100
1 7 10 13 19 23 28 31
```


Another implementation which is easy to follow is given below:

```K

/ happynum.k

/ sum of squares of digits of an integer
dgtsmsqr: {d::(); (0<){d::d,x!10; x%:10}/x; +/d*d}
/ Test if an integer is a Happy number
isHappy: {s::(); while[1<x;a:(dgtsmsqr x); :[(a _in s); :0; s::s,a]; x:a];:1} / Returns 1 if Happy
/ Generate first x Happy numbers and display the list
hnum: {[x]; h::();i:1;while[(#h)<x; :[(isHappy i); h::(h,i)]; i+:1]; `0: ,"List of ", ($x), " Happy Numbers"; h}


```


The output of a session with this implementation is given below:
```txt

K Console - Enter \ for help

  \l happynum
  hnum 8
List of 8 Happy Numbers
1 7 10 13 19 23 28 31

```



## Kotlin

```scala
// version 1.0.5-2

fun isHappy(n: Int): Boolean {
    val cache = mutableListOf<Int>()
    var sum = 0
    var nn = n
    var digit: Int
    while (nn != 1) {
        if (nn in cache) return false
        cache.add(nn)
        while (nn != 0) {
            digit = nn % 10
            sum += digit * digit
            nn /= 10
        }
        nn = sum
        sum = 0
    }
    return true
}

fun main(args: Array<String>) {
    var num = 1
    val happyNums = mutableListOf<Int>()
    while (happyNums.size < 8) {
        if (isHappy(num)) happyNums.add(num)
        num++
    }
    println("First 8 happy numbers : " + happyNums.joinToString(", "))
}
```


```txt

First 8 happy numbers : 1, 7, 10, 13, 19, 23, 28, 31

```



## Lasso


```lasso
#!/usr/bin/lasso9

define isHappy(n::integer) => {
  local(past = set)
  while(#n != 1) => {
    #n = with i in string(#n)->values sum math_pow(integer(#i), 2)
    #past->contains(#n) ? return false | #past->insert(#n)
  }
  return true
}

with x in generateSeries(1, 500)
  where isHappy(#x)
  take 8
select #x
```

Output:

```lasso
1, 7, 10, 13, 19, 23, 28, 31
```



## Liberty BASIC


```lb
    ct = 0
    n = 0
    DO
        n = n + 1
        IF HappyN(n, sqrInt$) = 1 THEN
            ct = ct + 1
            PRINT ct, n
        END IF
    LOOP UNTIL ct = 8
END

FUNCTION HappyN(n, sqrInts$)
    n$ = Str$(n)
    sqrInts = 0
    FOR i = 1 TO Len(n$)
        sqrInts = sqrInts + Val(Mid$(n$, i, 1)) ^ 2
    NEXT i
    IF sqrInts = 1 THEN
        HappyN = 1
        EXIT FUNCTION
    END IF
    IF Instr(sqrInts$, ":";Str$(sqrInts);":") > 0 THEN
        HappyN = 0
        EXIT FUNCTION
    END IF
    sqrInts$ = sqrInts$ + Str$(sqrInts) + ":"
    HappyN = HappyN(sqrInts, sqrInts$)
END FUNCTION
```

Output:-

```txt
1             1
2             7
3             10
4             13
5             19
6             23
7             28
8             31

```



## Locomotive Basic



```locobasic
10 mode 1:defint a-z
20 for i=1 to 100
30 i2=i
40 for l=1 to 20
50 a$=str$(i2)
60 i2=0
70 for j=1 to len(a$)
80 d=val(mid$(a$,j,1))
90 i2=i2+d*d
100 next j
110 if i2=1 then print i;"is a happy number":n=n+1:goto 150
120 if i2=4 then 150 ' cycle found
130 next l
140 ' check if we have reached 8 numbers yet
150 if n=8 then end
160 next i
```


[[File:Happy Numbers, Locomotive BASIC.png]]


## Logo



```logo
to sum_of_square_digits :number
  output (apply "sum (map [[d] d*d] ` :number))
end

to is_happy? :number [:seen []]
  output cond [
    [ [:number = 1] "true ]
    [ [member? :number :seen] "false ]
    [ else (is_happy? (sum_of_square_digits :number) (lput :number :seen))]
  ]
end

to n_happy :count [:start 1] [:result []]
  output cond [
    [ [:count <= 0] :result ]
    [ [is_happy? :start]
      (n_happy (:count-1) (:start+1) (lput :start :result)) ]
    [ else
      (n_happy :count (:start+1) :result) ]
  ]
end

print n_happy 8
bye
```


Output:

```txt
1 7 10 13 19 23 28 31
```



## LOLCODE

```lolcode
OBTW
  Happy Numbers Rosetta Code task in LOLCODE
  Requires 1.3 for BUKKIT availability
TLDR
HAI 1.3
CAN HAS STDIO?

BTW Simple list implementation.
BTW Used for the list of numbers already seen in IZHAPPY

BTW Create a list
HOW IZ I MAEKLIST
  I HAS A LIST ITZ A BUKKIT
  LIST HAS A LENGTH ITZ 0
  FOUND YR LIST
IF U SAY SO

BTW Append an item to list
HOW IZ I PUTIN YR LIST AN YR ITEM
  LIST HAS A SRS LIST'Z LENGTH ITZ ITEM
  LIST'Z LENGTH R SUM OF LIST'Z LENGTH AN 1
IF U SAY SO

BTW Check for presence of an item in the list
HOW IZ I DUZLISTHAS YR HAYSTACK AN YR NEEDLE
  IM IN YR BARN UPPIN YR INDEX WILE DIFFRINT INDEX AN HAYSTACK'Z LENGTH
    I HAS A ITEM ITZ HAYSTACK'Z SRS INDEX
    BOTH SAEM ITEM AN NEEDLE
    O RLY?
      YA RLY
        FOUND YR WIN
    OIC
  IM OUTTA YR BARN
  FOUND YR FAIL
IF U SAY SO

BTW Calculate the next number using the happy formula
HOW IZ I HAPPYSTEP YR NUM
  I HAS A NEXT ITZ 0
  IM IN YR LOOP
    BOTH SAEM NUM AN 0
    O RLY?
      YA RLY
        GTFO
    OIC
    I HAS A DIGIT ITZ MOD OF NUM AN 10
    NUM R QUOSHUNT OF NUM AN 10
    I HAS A SQUARE ITZ PRODUKT OF DIGIT AN DIGIT
    NEXT R SUM OF NEXT AN SQUARE
  IM OUTTA YR LOOP
  FOUND YR NEXT
IF U SAY SO

BTW Check to see if a number is happy
HOW IZ I IZHAPPY YR NUM
  I HAS A SEENIT ITZ I IZ MAEKLIST MKAY
  IM IN YR LOOP
    BOTH SAEM NUM AN 1
    O RLY?
      YA RLY
        FOUND YR WIN
    OIC
    I IZ DUZLISTHAS YR SEENIT AN YR NUM MKAY
    O RLY?
      YA RLY
        FOUND YR FAIL
    OIC
    I IZ PUTIN YR SEENIT AN YR NUM MKAY
    NUM R I IZ HAPPYSTEP YR NUM MKAY
  IM OUTTA YR LOOP
IF U SAY SO

BTW Print out the first 8 happy numbers
I HAS A KOUNT ITZ 0
IM IN YR LOOP UPPIN YR NUM WILE DIFFRINT KOUNT AN 8
  I IZ IZHAPPY YR NUM MKAY
  O RLY?
    YA RLY
      KOUNT R SUM OF KOUNT AN 1
      VISIBLE NUM
  OIC
IM OUTTA YR LOOP
KTHXBYE
``` >

Output:
```txt
1
7
10
13
19
23
28
31
```



## Lua


```lua
function digits(n)
  if n > 0 then return n % 10, digits(math.floor(n/10)) end
end
function sumsq(a, ...)
  return a and a ^ 2 + sumsq(...) or 0
end
local happy = setmetatable({true, false, false, false}, {
      __index = function(self, n)
         self[n] = self[sumsq(digits(n))]
         return self[n]
      end } )
i, j = 0, 1
repeat
   i, j = happy[j] and (print(j) or i+1) or i, j + 1
until i == 8
```

Output:

```txt
1
7
10
13
19
23
28
31
```



## M2000 Interpreter

Lambda Function PrintHappy has a closure another lambda function IsHappy which has a closure of another lambda function the sumOfSquares.



```M2000 Interpreter

Function FactoryHappy {
      sumOfSquares= lambda (n) ->{
                  k$=str$(abs(n),"")
                  Sum=0
                  For i=1 to len(k$)
                        sum+=val(mid$(k$,i,1))**2
                  Next i
                  =sum
      }
      IsHappy=Lambda sumOfSquares (n) ->{
            Inventory sequence
            While n<>1 {
                  Append sequence, n
                  n=sumOfSquares(n)
                   if exist(sequence, n) then =false : Break
            }
            =True
      }
      =Lambda IsHappy ->{
                  numleft=8
                  numToTest=1
                  While numleft {
                        if ishappy(numToTest) Then {
                              Print numToTest
                              numleft--
                        }
                        numToTest++
                  }
      }
}
PrintHappy=factoryHappy()
Call PrintHappy()

```

```txt

 1
 7
10
13
19
23
28
31
```



## Maple

To begin, here is a procedure to compute the sum of the squares of the digits of a positive integer.  It uses the built-in procedure irem, which computes the integer remainder and, if passed a name as the optional third argument, assigns it the corresponding quotient.  (In other words, it performs integer division with remainder.  There is also a dual, companion procedure iquo, which returns the integer quotient and assigns the remainder to the (optional) third argument.)

```Maple
SumSqDigits := proc( n :: posint )
        local s := 0;
        local m := n;
        while m <> 0 do
                s := s + irem( m, 10, 'm' )^2
        end do;
        s
end proc:
```

(Note that the unevaluation quotes on the third argument to irem are essential here, as that argument must be a name and, if m were passed without quotes, it would evaluate to a number.)

For example,

```Maple

> SumSqDigits( 1234567890987654321 );
                                  570

```

We can check this by computing it another way (more directly).

```Maple

> n := 1234567890987654321:
> `+`( op( map( parse, StringTools:-Explode( convert( n, 'string' ) ) )^~2) );
                                  570

```

The most straight-forward way to check whether a number is happy or sad seems also to be the fastest (that I could think of).

```Maple
Happy? := proc( n )
        if n = 1 then
                true
        elif n = 4 then
                false
        else
                local s := SumSqDigits( n );
                while not ( s in { 1, 4 } ) do
                        s := SumSqDigits( s )
                end do;
                evalb( s = 1 )
        end if
end proc:
```

We can use this to determine the number of happy (H) and sad (S) numbers up to one million as follows.

```Maple

> H, S := selectremove( Happy?, [seq]( 1 .. N ) ):
> nops( H ), nops( S );
                             143071, 856929

```

Finally, to solve the stated problem, here is a completely straight-forward routine to locate the first N happy numbers, returning them in a set.

```Maple
FindHappiness := proc( N )
        local count := 0;
        local T := table();
        for local i while count < N do
                if Happy?( i ) then
                        count := 1 + count;
                        T[ count ] := i
                end if
        end do;
        {seq}( T[ i ], i = 1 .. count )
end proc:
```

With input equal to 8, we get

```Maple

> FindHappiness( 8 );
                     {1, 7, 10, 13, 19, 23, 28, 31}

```

For completeness, here is an implementation of the cycle detection algorithm for recognizing happy numbers.  It is much slower, however.

```Maple
Happy? := proc( n :: posint )
        local a, b;
        a, b := n, SumSqDigits( n );
        while a <> b do
                a := SumSqDigits( a );
                b := (SumSqDigits@@2)( b )
        end do;
        evalb( a = 1 )
end proc:
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Custom function HappyQ:

```Mathematica
AddSumSquare[input_]:=Append[input,Total[IntegerDigits[Last[input]]^2]]
NestUntilRepeat[a_,f_]:=NestWhile[f,{a},!MemberQ[Most[Last[{##}]],Last[Last[{##}]]]&,All]
HappyQ[a_]:=Last[NestUntilRepeat[a,AddSumSquare]]==1
```

Examples for a specific number:

```Mathematica
HappyQ[1337]
HappyQ[137]
```

gives back:

```Mathematica
True
False
```

Example finding the first 8:

```Mathematica
m = 8;
n = 1;
i = 0;
happynumbers = {};
While[n <= m,
 i++;
 If[HappyQ[i],
  n++;
  AppendTo[happynumbers, i]
  ]
 ]
happynumbers
```

gives back:

```Mathematica
{1, 7, 10, 13, 19, 23, 28, 31}
```



## MATLAB

Recursive version:

```MATLAB
function findHappyNumbers
    nHappy = 0;
    k = 1;
    while nHappy < 8
        if isHappyNumber(k, [])
            fprintf('%d ', k)
            nHappy = nHappy+1;
        end
        k = k+1;
    end
    fprintf('\n')
end

function hap = isHappyNumber(k, prev)
    if k == 1
        hap = true;
    elseif ismember(k, prev)
        hap = false;
    else
        hap = isHappyNumber(sum((sprintf('%d', k)-'0').^2), [prev k]);
    end
end
```

```txt
1 7 10 13 19 23 28 31
```



## MAXScript


```MAXScript

fn isHappyNumber n =
(
	local pastNumbers = #()
	while n != 1 do
	(
		n = n as string
		local newNumber = 0
		for i = 1 to n.count do
		(
			local digit = n[i] as integer
			newNumber += pow digit 2
		)
		n = newNumber
		if (finditem pastNumbers n) != 0 do return false
		append pastNumbers newNumber
	)
	n == 1
)
printed = 0
for i in (for h in 1 to 500 where isHappyNumber h collect h) do
(
	if printed == 8 do exit
	print i as string
	printed += 1

)

```

Output:

```MAXScript

1
7
10
13
19
23
28
31

```



## Mercury


```Mercury
:- module happy.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, set_tree234.

main(!IO) :-
    print_line(get_n_happy_numbers(8, 1), !IO).

:- func get_n_happy_numbers(int, int) = list(int).

get_n_happy_numbers(NumToFind, N) =
    ( if NumToFind > 0 then
       ( if is_happy(N, init)
       then [N | get_n_happy_numbers(NumToFind - 1, N + 1)]
       else get_n_happy_numbers(NumToFind, N + 1)
       )
    else
       []
    ).

:- pred is_happy(int::in, set_tree234(int)::in) is semidet.

is_happy(1, _).
is_happy(N, !.Seen) :-
   not member(N, !.Seen),
   insert(N, !Seen),
   is_happy(sum_sqr_digits(N), !.Seen).

:- func sum_sqr_digits(int) = int.

sum_sqr_digits(N) =
   ( if N < 10 then sqr(N) else sqr(N mod 10) + sum_sqr_digits(N div 10) ).

:- func sqr(int) = int.

sqr(X) = X * X.
```

```txt
[1, 7, 10, 13, 19, 23, 28, 31]
```



## ML

=
## mLite
=

```ocaml
(*
A happy number is defined by the following process. Starting with any positive integer, replace the number
by the sum of the squares of its digits, and repeat the process until the number equals 1 (where it will
stay), or it loops endlessly in a cycle which does not include 1. Those numbers for which this process ends
 in 1 are happy numbers, while those that do not end in 1 are unhappy numbers. Display an example of your
output here.
*)

local
	fun get_digits
			(d, s) where (d = 0) = s
		| 	(d, s) = get_digits( d div 10, (d mod 10) :: s)
		| 	n = get_digits( n div 10, [n mod 10] )
	;
	fun mem
			(x, []) = false
		| 	(x, a :: as) where (x = a) = true
		| 	(x, _ :: as) = mem (x, as)
in
	fun happy
			1 = "happy"
		|	n =
				let
					val this = (fold (+,0) ` map (fn n = n ^ 2) ` get_digits n);
					val sads = [2, 4, 16, 37, 58, 89, 145, 42, 20]
				in
					if (mem (n,sads)) then
						"unhappy"
					else
						happy this
				end
end
;

foreach (fn n = (print n; print " is "; println ` happy n)) ` iota 10;

```

Output:

```txt
1 is happy
2 is unhappy
3 is unhappy
4 is unhappy
5 is unhappy
6 is unhappy
7 is happy
8 is unhappy
9 is unhappy
10 is happy
```



## MUMPS


```MUMPS
ISHAPPY(N)
 ;Determines if a number N is a happy number
 ;Note that the returned strings do not have a leading digit unless it is a happy number
 IF (N'=N\1)!(N<0) QUIT "Not a positive integer"
 NEW SUM,I
 ;SUM is the sum of the square of each digit
 ;I is a loop variable
 ;SEQ is the sequence of previously checked SUMs from the original N
 ;If it isn't set already, initialize it to an empty string
 IF $DATA(SEQ)=0 NEW SEQ SET SEQ=""
 SET SUM=0
 FOR I=1:1:$LENGTH(N) DO
 .SET SUM=SUM+($EXTRACT(N,I)*$EXTRACT(N,I))
 QUIT:(SUM=1) SUM
 QUIT:$FIND(SEQ,SUM)>1 "Part of a sequence not containing 1"
 SET SEQ=SEQ_","_SUM
 QUIT $$ISHAPPY(SUM)
HAPPY(C) ;Finds the first C happy numbers
 NEW I
 ;I is a counter for what integer we're looking at
 WRITE !,"The first "_C_" happy numbers are:"
 FOR I=1:1 QUIT:C<1  SET Q=+$$ISHAPPY(I) WRITE:Q !,I SET:Q C=C-1
 KILL I
 QUIT
```

Output:
```txt

USER>D HAPPY^ROSETTA(8)

The first 8 happy numbers are:
1
7
10
13
19
23
28
31
USER>W:+$$ISHAPPY^ROSETTA(320) "Happy Number"
Happy Number
USER>W:+$$ISHAPPY^ROSETTA(321) "Happy Number"

USER>

```



## NetRexx

```netrexx
/*NetRexx program to display the 1st 8 (or specified arg) happy numbers*/
limit	 = arg[0]                        /*get argument for  LIMIT.        */
say limit
if limit = null, limit ='' then limit=8  /*if not specified, set LIMIT to 8*/
haps	 = 0                             /*count of happy numbers so far.  */

loop n=1 while haps < limit              /*search integers starting at one.*/
  q=n                                    /*Q may or may not be "happy".    */
  a=0

  loop forever                           /*see if  Q  is a happy number.   */
    if q==1 then do                      /*if  Q  is unity, then it's happy*/
      haps = haps + 1                    /*bump the count of happy numbers.*/
      say n                              /*display the number.             */
      iterate n                          /*and then keep looking for more. */
    end

    sum=0                                /*initialize sum to zero.         */

    loop j=1 for q.length                /*add the squares of the numerals.*/
      sum = sum + q.substr(j,1) ** 2
    end

    if a[sum] then iterate n             /*if already summed, Q is unhappy.*/
    a[sum]=1                             /*mark the sum as being found.    */
    q=sum                                /*now, lets try the  Q  sum.      */
  end
end
```

;Output

```txt

1
7
10
13
19
23
28
31

```

Sample output when 100 is specified as the program's argument.
<pre style="height:30ex;overflow:scroll">
1
7
10
13
19
23
28
31
32
44
49
68
70
79
82
86
91
94
97
100
103
109
129
130
133
139
167
176
188
190
192
193
203
208
219
226
230
236
239
262
263
280
291
293
301
302
310
313
319
320
326
329
331
338
356
362
365
367
368
376
379
383
386
391
392
397
404
409
440
446
464
469
478
487
490
496
536
556
563
565
566
608
617
622
623
632
635
637
638
644
649
653
655
656
665
671
673
680
683
694

```



## Nim

```nim
import intsets

proc happy(n: int): bool =
  var
    n = n
    past = initIntSet()
  while n != 1:
    let s = $n
    n = 0
    for c in s:
      let i = ord(c) - ord('0')
      n += i * i
    if n in past:
      return false
    past.incl(n)
  return true

for x in 0..31:
  if happy(x):
    echo x
```

Output:

```txt
1
7
10
13
19
23
28
31
```



## Objeck


```objeck
use IO;
use Structure;

bundle Default {
  class HappyNumbers {
    function : native : IsHappy(n : Int) ~ Bool {
      cache := IntVector->New();
        sum := 0;
        while(n <> 1) {
          if(cache->Has(n)) {
            return false;
          };

          cache->AddBack(n);
          while(n <> 0) {
            digit := n % 10;
            sum += (digit * digit);
            n /= 10;
          };

          n := sum;
          sum := 0;
        };

        return true;
      }

      function : Main(args : String[]) ~ Nil {
        num := 1;
        happynums := IntVector->New();

        while(happynums->Size() < 8) {
          if(IsHappy(num)) {
            happynums->AddBack(num);
        };

        num += 1;
      };

      Console->Print("First 8 happy numbers: ");
      each(i : happynums) {
        Console->Print(happynums->Get(i))->Print(",");
      };
      Console->PrintLine("");
    }
  }
}
```

output:

```txt
First 8 happy numbers: 1,7,10,13,19,23,28,31,
```



## OCaml

Using [[wp:Cycle detection|Floyd's cycle-finding algorithm]].

```ocaml
open Num

let step =
	let rec aux s n =
	if n =/ Int 0 then s else
		let q = quo_num n (Int 10)
		and r = mod_num n (Int 10)
		in aux (s +/ (r */ r)) q
	in aux (Int 0) ;;

let happy n =
	let rec aux x y =
		if x =/ y then x else aux (step x) (step (step y))
	in (aux n (step n)) =/ Int 1 ;;

let first n =
	let rec aux v x n =
		if n = 0 then v else
			if happy x
			then aux (x::v) (x +/ Int 1) (n - 1)
			else aux v (x +/ Int 1) n
	in aux [ ] (Int 1) n ;;

List.iter print_endline (
	List.rev_map string_of_num (first 8)) ;;
```

Output:

```txt
$ ocaml nums.cma happy_numbers.ml
1
7
10
13
19
23
28
31
```



## Oforth



```Oforth
: isHappy(n)
| cycle |
   ListBuffer new ->cycle

   while(n 1 <>) [
      cycle include(n) ifTrue: [ false return ]
      cycle add(n)
      0 n asString apply(#[ asDigit sq + ]) ->n
      ]
   true ;

: happyNum(N)
| numbers |
   ListBuffer new ->numbers
   1 while(numbers size N <>) [ dup isHappy ifTrue: [ dup numbers add ] 1+ ]
   numbers println ;
```


Output:

```txt

>happyNum(8)
[1, 7, 10, 13, 19, 23, 28, 31]

```



## ooRexx


```ooRexx

count = 0
say "First 8 happy numbers are:"
loop i = 1 while count < 8
    if happyNumber(i) then do
        count += 1
        say i
    end
end

::routine happyNumber
  use strict arg number

  -- use to trace previous cycle results
  previous = .set~new
  loop forever
      -- stop when we hit the target
      if number = 1 then return .true
      -- stop as soon as we start cycling
      if previous[number] \== .nil then return .false
      previous~put(number)
      next = 0
      -- loop over all of the digits
      loop digit over number~makearray('')
          next += digit * digit
      end
      -- and repeat the cycle
      number = next
  end

```


```txt

First 8 happy numbers are:
1
7
10
13
19
23
28
31

```



## Oz


```oz
functor
import
  System
define
  fun {IsHappy N}
     {IsHappy2 N nil}
  end

  fun {IsHappy2 N Seen}
     if     N == 1          then true
     elseif {Member N Seen} then false
     else
	Next = {Sum {Map {Digits N} Square}}
     in
	{IsHappy2 Next N|Seen}
     end
  end

  fun {Sum Xs}
     {FoldL Xs Number.'+' 0}
  end

  fun {Digits N}
     {Map {Int.toString N} fun {$ D} D - &0 end}
  end

  fun {Square N} N*N end

  fun lazy {Nat I}
     I|{Nat I+1}
  end

  %% List.filter is eager. But we need a lazy Filter:
  fun lazy {LFilter Xs P}
     case Xs of X|Xr andthen {P X} then X|{LFilter Xr P}
     [] _|Xr then {LFilter Xr P}
     [] nil then nil
     end
  end

  HappyNumbers = {LFilter {Nat 1} IsHappy}
in
  {System.show {List.take HappyNumbers 8}}
end
```

Output:

```txt
[1 7 10 13 19 23 28 31]
```



## PARI/GP

If the number has more than three digits, the sum of the squares of its digits has fewer digits than the number itself.  If the number has three digits, the sum of the squares of its digits is at most 3 * 9^2 = 243.  A simple solution is to look up numbers up to 243 and calculate the sum of squares only for larger numbers.

```parigp
H=[1,7,10,13,19,23,28,31,32,44,49,68,70,79,82,86,91,94,97,100,103,109,129,130,133,139,167,176,188,190,192,193,203,208,219,226,230,236,239];
isHappy(n)={
  if(n<262,
    setsearch(H,n)>0
  ,
    n=eval(Vec(Str(n)));
    isHappy(sum(i=1,#n,n[i]^2))
  )
};
select(isHappy, vector(31,i,i))
```

Output:

```txt
%1 = [1, 7, 10, 13, 19, 23, 28, 31]
```



## Pascal


```pascal
Program HappyNumbers (output);

uses
  Math;

function find(n: integer; cache: array of integer): boolean;
  var
    i: integer;
  begin
    find := false;
    for i := low(cache) to high(cache) do
      if cache[i] = n then
        find := true;
  end;

function is_happy(n: integer): boolean;
  var
    cache: array of integer;
    sum: integer;
  begin
    setlength(cache, 1);
    repeat
      sum := 0;
      while n > 0 do
      begin
        sum := sum + (n mod 10)**2;
        n := n div 10;
      end;
      if sum = 1 then
      begin
        is_happy := true;
        break;
      end;
      if find(sum, cache) then
      begin
        is_happy := false;
        break;
      end;
      n := sum;
      cache[high(cache)]:= sum;
      setlength(cache, length(cache)+1);
    until false;
  end;

var
  n, count: integer;

begin
  n := 1;
  count := 0;
  while count < 8 do
  begin
    if is_happy(n) then
    begin
      inc(count);
      write(n, ' ');
    end;
    inc(n);
  end;
  writeln;
end.
```

Output:

```txt
:> ./HappyNumbers
1 7 10 13 19 23 28 31

```


### alternative for counting fast

The Cache is limited to maximum value of the sum of squarded digits and filled up in a blink of an eye.Calculation of sum of squared digits is improved.Saving this SqrdSumCache speeds up x100.

So i am able to check if the 1'000'000 th happy number is 7105849 as stated in C language.This seems to be true.
Tested with free Pascal 2.6.5.

```pascal
Program HappyNumbers (output);
// NativeUInt: LongWord 32-Bit-OS/ Uint64 64-Bit-OS
{$IFDEF FPC}
  {$MODE DELPHI}
  {$OPTIMIZATION ON,Regvar,PEEPHOLE,CSE,ASMCSE}
  {$CODEALIGN proc=32}
{$ELSE}
  //for Delphi
  {$APPLICATION CONSOLE}
{$ENDIF}
const
  HighCache = 19*(9*9);//sum sqrdigt of Uint64
  cDigit  = 1000;
type
  tCache     = array[0..HighCache] of Word;
  tSqrdCache = array[0..cDigit] of Word;
  tSqrdSumCache = array[0..HighCache] of Word;
var
  Cache : tCache;
  SqrdCache :tSqrdCache;
  SqrdSumCache :tSqrdSumCache;

function find(n: NativeUint;const cache: tCache): boolean;
var
  i: NativeUint;
begin
  find := false;
  for i := low(cache) to high(cache) do
    if cache[i] = n then
      find := true;
  writeln(i:10,n:10);
end;

procedure InitSqrdCache;
var
  i,n,sum,r: NativeUint;
begin
  For i := 0 to  cDigit do
  Begin
    sum := 0;
    n := i;
    while n > 0 do
    begin
      r := n;
      n := n div 10;
      r := r-10*n;
      sum := sum + r*r;
    end;
    SqrdCache[i] := sum;
  end;
end;

function SumSqrdDgt(n: NativeUint): NativeUint;
var
  sum,r: NativeUint;
begin
  sum := 0;
  while n > cDigit do
  begin
    r := n;
    n := n div cDigit;
    r := r-cDigit*n;
    sum := sum + SqrdCache[r];
  end;
  SumSqrdDgt := sum + SqrdCache[n];
end;


procedure Inithappy;
var
  n,s,p : NativeUint;
Begin
  fillchar(SqrdSumCache,SizeOf(SqrdSumCache),#0);
  InitSqrdCache;
  fillChar(Cache,SizeOf(Cache),#0);
  Cache[1] := 1;
  For n := 1 to High(Cache) do
  Begin
    If Cache[n] = 0 then
    Begin
      //start a linked list
      Cache[n] := n;
      p := n;
      s := SumSqrdDgt(p);
      while Cache[s] = 0 do
      Begin
        Cache[s] := p;
        p := s;
        s := SumSqrdDgt(p);
      end;
      //mark linked list backwards as happy number
      IF Cache[s] = 1 then
      Begin
        repeat
          s := Cache[p];
          Cache[p] := 1;
          p := s;
        until s = n;
        Cache[n] := 1;
      end;
    end;
  end;
end;

function nextCdigits(sqSum: NativeUint):NativeUint;
var
  i,cnt : LongInt;
Begin
  cnt:= SqrdSumCache[sqSum];
  If cnt = 0 then
  Begin
    For i := 0 to Cdigit-1 do
      cnt := cnt + Ord(Cache[sqSum+SqrdCache[i]]=1);
    //saving calculation->speed up x100
    SqrdSumCache[sqSum] := cnt;
  end;
  nextCdigits := cnt;
end;

function is_happy(n: NativeUint): boolean;inline;
begin
  is_happy := Cache[SumSqrdDgt(n)]=1
end;

function nthHappy(Limit: NativeUint):NativeUint;
var
  n,
  count : NativeUint;
begin
  n:= 0;
  count := 0;
  // big steps
  IF limit>cDigit then
    repeat
      inc(count,nextCdigits(SumSqrdDgt(n)));
      inc(n,cDigit);
    until count >= Limit-cDigit;
  // small steps
  repeat
    if is_happy(n) then
      inc(count);
    inc(n);
  until count >= Limit;
  nthHappy:= n-1;
end;

var
  n, count,Limit: NativeUint;
begin
  Inithappy;
  n := 1;
  count := 0;
  while count < 8 do
  begin
    if is_happy(n) then
    begin
      inc(count);
      write(n, ' ');
    end;
    inc(n);
  end;
  writeln;

  n := 1;
  Limit := 10;// 1En
  repeat
    writeln('10e',n,' nth happy number ',nthHappy(limit):13);
    inc(n);
    Limit := limit*10;
  until n> 8;
end.
```

;output:

```txt

1 7 10 13 19 23 28 31
....
10e1 nth happy number            44
10e2 nth happy number           694
10e3 nth happy number          6899
10e4 nth happy number         67169
10e5 nth happy number        692961
10e6 nth happy number       7105849
10e7 nth happy number      71313350
10e8 nth happy number     698739425

real    0m0.006s
{Linux 64 Bit
10e9 nth happy number    6788052776
10e10 nth happy number   66305148869

real    0m0.521s

```



## Perl

Since all recurrences end with 1 or repeat (37,58,89,145,42,20,4,16), we can do this test very quickly without having to make hashes of seen numbers.

```perl
use List::Util qw(sum);

sub ishappy {
  my $s = shift;
  while ($s > 6 && $s != 89) {
    $s = sum(map { $_*$_ } split(//,$s));
  }
  $s == 1;
}

my $n = 0;
print join(" ", map { 1 until ishappy(++$n); $n; } 1..8), "\n";
```

```txt
1 7 10 13 19 23 28 31
```


Or we can solve using only the rudimentary task knowledge as below.  Note the slightly different ways of doing the digit sum and finding the first 8 numbers where ishappy(n) is true -- this shows there's more than one way to do even these small sub-tasks.
```perl
use List::Util qw(sum);
sub is_happy {
    my ($n) = @_;
    my %seen;
    while (1) {
        $n = sum map { $_ ** 2 } split //, $n;
        return 1 if $n == 1;
        return 0 if $seen{$n}++;
    }
}

my $n;
is_happy( ++$n ) and print "$n " or redo for 1..8;
```

```txt
1 7 10 13 19 23 28 31
```



## Perl 6

```perl6
sub happy (Int $n is copy --> Bool) {
  loop {
      state %seen;
      $n = [+] $n.comb.map: { $_ ** 2 }
      return True  if $n == 1;
      return False if %seen{$n}++;
  }
}

say join ' ', grep(&happy, 1 .. *)[^8];
```

```txt
1 7 10 13 19 23 28 31
```

Here's another approach that uses a different set of tricks including lazy lists, gather/take, repeat-until, and the cross metaoperator X.

```perl6
my @happy = lazy gather for 1..* -> $number {
    my %stopper = 1 => 1;
    my $n = $number;
    repeat until %stopper{$n}++ {
        $n = [+] $n.comb X** 2;
    }
    take $number if $n == 1;
}

say ~@happy[^8];
```

Output is the same as above.

Here is a version using a subset and an anonymous recursion (we cheat a little bit by using the knowledge that 7 is the second happy number):

```perl6
subset Happy of Int where sub ($n) {
    $n == 1 ?? True  !!
    $n < 7  ?? False !!
    &?ROUTINE([+] $n.comb »**» 2);
}

say (grep Happy, 1 .. *)[^8];
```

Again, output is the same as above.  It is not clear whether this version returns in finite time for any integer, though.

There's more than one way to do it...


## Phix

Copy of [[Happy_numbers#Euphoria|Euphoria]] tweaked to give a one-line output

```Phix
function is_happy(integer n)
sequence seen = {}
integer k
    while n>1 do
        seen &= n
        k = 0
        while n>0 do
            k += power(remainder(n,10),2)
            n = floor(n/10)
        end while
        n = k
        if find(n,seen) then
            return 0
        end if
    end while
    return 1
end function

integer n = 1
sequence s = {}
while length(s)<8 do
    if is_happy(n) then
        s &= n
    end if
    n += 1
end while
?s
```

```txt

{1,7,10,13,19,23,28,31}

```



## PHP

```php
function isHappy($n) {
    while (1) {
        $total = 0;
        while ($n > 0) {
            $total += pow(($n % 10), 2);
            $n /= 10;
        }
        if ($total == 1)
            return true;
        if (array_key_exists($total, $past))
            return false;
        $n = $total;
        $past[$total] = 0;
    }
}

$i = $cnt = 0;
while ($cnt < 8) {
    if (isHappy($i)) {
        echo "$i ";
        $cnt++;
    }
    $i++;
}
```


```txt
1 7 10 13 19 23 28 31
```



## PicoLisp


```PicoLisp
(de happy? (N)
   (let Seen NIL
      (loop
         (T (= N 1) T)
         (T (member N Seen))
         (setq N
            (sum '((C) (** (format C) 2))
               (chop (push 'Seen N)) ) ) ) ) )

(let H 0
   (do 8
      (until (happy? (inc 'H)))
      (printsp H) ) )
```

Output:

```txt
1 7 10 13 19 23 28 31
```



## PL/I


```PL/I
test: proc options (main); /* 19 November 2011 */
   declare (i, j, n, m, nh initial (0) ) fixed binary (31);

main_loop:
   do j = 1 to 100;
      n = j;
      do i = 1 to 100;
         m = 0;
         /* Form the sum of squares of the digits. */
         do until (n = 0);
            m = m + mod(n, 10)**2;
            n = n/10;
         end;
         if m = 1 then
            do;
               put skip list (j || ' is a happy number');
               nh = nh + 1;
               if nh = 8 then return;
               iterate main_loop;
            end;
         n = m; /* Replace n with the new number formed from digits. */
      end;
   end;
end test;

```

OUTPUT:

```txt

             1 is a happy number
             7 is a happy number
            10 is a happy number
            13 is a happy number
            19 is a happy number
            23 is a happy number
            28 is a happy number
            31 is a happy number

```



## Potion


```potion
sqr = (n): n * n.

isHappy = (n) :
   loop :
      if (n == 1): return true.
      if (n == 4): return false.
      sum = 0
      n = n string
      n length times (i): sum = sum + sqr(n(i) number integer).
      n = sum
   .
.

firstEight = ()
i = 0
while (firstEight length < 8) :
   i++
   if (isHappy(i)): firstEight append(i).
.
firstEight string print
```



## PowerShell


```PowerShell
function happy([int] $n) {
    $a=@()
    for($i=2;$a.count -lt $n;$i++) {
        $sum=$i
        $hist=@{}
        while( $hist[$sum] -eq $null ) {
            if($sum -eq 1) {
                $a+=$i
            }
            $hist[$sum]=$sum
            $sum2=0
            foreach($j in $sum.ToString().ToCharArray()) {
                $k=([int]$j)-0x30
                $sum2+=$k*$k
            }
            $sum=$sum2
        }
    }
    $a -join ','
}
```

Output :

```PowerShell
happy(8)
7,10,13,19,23,28,31,32
```



## Prolog

```Prolog
happy_numbers(L, Nb) :-
    % creation of the list
    length(L, Nb),
    % Process of this list
    get_happy_number(L, 1).


% the game is over
get_happy_number([], _).

% querying the newt happy_number
get_happy_number([H | T], N) :-
     N1 is N+1,
    (is_happy_number(N) ->
        H = N,
        get_happy_number(T, N1);
        get_happy_number([H | T], N1)).

% we must memorized the numbers reached
is_happy_number(N) :-
    is_happy_number(N, [N]).

% a number is happy when we get 1
is_happy_number(N, _L) :-
    get_next_number(N, 1), !.

% or when this number is not already reached !
is_happy_number(N, L) :-
    get_next_number(N, NN),
    \+member(NN, L),
    is_happy_number(NN, [NN | L]).

% Process of the next number from N
get_next_number(N, NewN) :-
    get_list_digits(N, LD),
    maplist(square, LD, L),
    sumlist(L, NewN).

get_list_digits(N, LD) :-
	number_chars(N, LCD),
	maplist(number_chars_, LD, LCD).

number_chars_(D, CD) :-
	number_chars(D, [CD]).

square(N, SN) :-
	SN is N * N.
```

Output :

```Prolog
 ?- happy_numbers(L, 8).
L = [1,7,10,13,19,23,28,31].
```



## PureBasic


```PureBasic
#ToFind=8
#MaxTests=100
#True = 1: #False = 0
Declare is_happy(n)

If OpenConsole()
  Define i=1,Happy
  Repeat
    If is_happy(i)
      Happy+1
      PrintN("#"+Str(Happy)+RSet(Str(i),3))
    EndIf
    i+1
  Until Happy>=#ToFind
  ;
  Print(#CRLF$+#CRLF$+"Press ENTER to exit"): Input()
  CloseConsole()
EndIf

Procedure is_happy(n)
  Protected i,j=n,dig,sum
  Repeat
    sum=0
    While j
      dig=j%10
      j/10
      sum+dig*dig
    Wend
    If sum=1: ProcedureReturn #True: EndIf
    j=sum
    i+1
  Until i>#MaxTests
  ProcedureReturn #False
EndProcedure
```

Sample output:

```txt
#1  1
#2  7
#3 10
#4 13
#5 19
#6 23
#7 28
#8 31
```



## Python


### Procedural


```python
>>>
 def happy(n):
    past = set()
    while n != 1:
        n = sum(int(i)**2 for i in str(n))
        if n in past:
            return False
        past.add(n)
    return True

>>> [x for x in xrange(500) if happy(x)][:8]
[1, 7, 10, 13, 19, 23, 28, 31]
```



### Composition of pure functions


Drawing 8 terms from a non finite stream, rather than assuming prior knowledge of the finite sample size required:

```python
'''Happy numbers'''

from itertools import islice


# main :: IO ()
def main():
    '''Test'''
    print(
        take(8)(
            happyNumbers()
        )
    )


# happyNumbers :: Gen [Int]
def happyNumbers():
    '''Generator :: non-finite stream of happy numbers.'''
    x = 1
    while True:
        x = until(isHappy)(succ)(x)
        yield x
        x = succ(x)


# isHappy :: Int -> Bool
def isHappy(n):
    '''Happy number sequence starting at n reaches 1 ?'''
    seen = set()

    # p :: Int -> Bool
    def p(x):
        if 1 == x or x in seen:
            return True
        else:
            seen.add(x)
            return False

    # f :: Int -> Int
    def f(x):
        return sum(int(d)**2 for d in str(x))

    return 1 == until(p)(f)(n)


# GENERIC -------------------------------------------------

# succ :: Int -> Int
def succ(x):
    '''The successor of an integer.'''
    return 1 + x


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.'''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(islice(xs, n))
    )


# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    '''The result of repeatedly applying f until p holds.
       The initial seed value is x.'''
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)


if __name__ == '__main__':
    main()
```

```txt
[1, 7, 10, 13, 19, 23, 28, 31]
```



## R


```R
is.happy <- function(n)
{
   stopifnot(is.numeric(n) && length(n)==1)
   getdigits <- function(n)
   {
      as.integer(unlist(strsplit(as.character(n), "")))
   }
   digits <- getdigits(n)
   previous <- c()
   repeat
   {
      sumsq <- sum(digits^2, na.rm=TRUE)
      if(sumsq==1L)
      {
         happy <- TRUE
         break
      } else if(sumsq %in% previous)
      {
         happy <- FALSE
         attr(happy, "cycle") <- previous
         break
      } else
      {
         previous <- c(previous, sumsq)
         digits <- getdigits(sumsq)
      }
   }
   happy
}
```

Example usage

```R
is.happy(2)
```

 [1] FALSE
 attr(,"cycle")
 [1]   4  16  37  58  89 145  42  20

```R
#Find happy numbers between 1 and 50
which(apply(rbind(1:50), 2, is.happy))
```

 1  7 10 13 19 23 28 31 32 44 49

```R
#Find the first 8 happy numbers
happies <- c()
i <- 1L
while(length(happies) < 8L)
{
   if(is.happy(i)) happies <- c(happies, i)
   i <- i + 1L
}
happies
```

 1  7 10 13 19 23 28 31


## Racket


```Racket
#lang racket
(define (sum-of-squared-digits number (result 0))
  (if (zero? number)
      result
      (sum-of-squared-digits (quotient number 10)
                             (+ result (expt (remainder number 10) 2)))))

(define (happy-number? number (seen null))
  (define next (sum-of-squared-digits number))
  (cond ((= 1 next)
         #t)
        ((memq next seen)
         #f)
        (else
         (happy-number? next (cons number seen)))))

(define (get-happys max)
  (for/list ((x (in-range max))
             #:when (happy-number? x))
    x))

(display (take (get-happys 100) 8)) ;displays (1 7 10 13 19 23 28 31)
```



## REXX


### unoptimized


```REXX
/*REXX program  computes  and  displays  a  specified  amount  of   happy   numbers.    */
parse arg limit .                                /*obtain optional argument from the CL.*/
if limit=='' | limit==","  then limit=8          /*Not specified?  Then use the default.*/
haps=0                                           /*count of the happy numbers  (so far).*/

  do n=1  while haps<limit;    @.=0;  q=n        /*search the integers starting at unity*/
         do  until q==1                          /*determine if   Q   is a happy number.*/
         s=0                                     /*prepare to add squares of digits.    */
                 do j=1  for length(q)           /*sum the squares of the decimal digits*/
                 s=s + substr(q, j, 1) **2       /*add the square  of  a  decimal digit.*/
                 end   /*j*/

         if @.s  then iterate n                  /*if already summed,   Q   is unhappy. */
         @.s=1;  q=s                             /*mark the sum as found;   try  Q  sum.*/
         end   /*until*/
  say n                                          /*display the number    (N  is happy). */
  haps=haps+1                                    /*bump the  count  of  happy numbers.  */
  end          /*n*/
                                                 /*stick a fork in it,  we're all done. */
```

```txt

1
7
10
13
19
23
28
31

```


===optimized, vertical list===
This REXX code uses additional memorization (by keeping track of happy and unhappy numbers),

it's about   2 <sup>1</sup>/<sub>2</sub>   times faster than the unoptimized version.


This REXX version also accepts a   ''range''   of happy numbers to be shown,   that is,

it can show the 2000<sup>th</sup> through the 2032<sup>nd</sup> (inclusive) happy numbers   (as shown below).

```rexx
/*REXX program   computes  and  displays   a specified  range  of   happy   numbers.    */
parse arg L H .                                  /*obtain optional arguments from the CL*/
if L=='' | L==","  then L=8                      /*Not specified?  Then use the default.*/
if H=='' | H==","  then do;  H=L; L=1;  end      /*use a range for the displaying of #s.*/
            do i=0  to 9;  #.i=i**2;  end /*i*/  /*build a squared decimal digit table. */
@.=0;   @.1=1;       !.=@.;    !.2=1;    !.4=1   /*sparse array:   @≡happy,  !≡unhappy. */
haps=0                                           /*count of the happy numbers  (so far).*/

    do n=1  while  haps<H                        /*search integers starting at unity.   */
    if !.n  then iterate                         /*if  N  is unhappy, then try another. */
    q=n                                          /* [↓]    Q  is the number being tested*/
          do  until q==1;    s=0                 /*see if  Q  is a  happy number.       */
          ?=q                                    /* [↓]    ?  is destructively parsed.  */
               do length(q)                      /*parse all the    decimal digits of ? */
               parse var  ?  _  +1 ?             /*obtain a  single decimal digit  of ? */
               s=s + #._                         /*add the square of that decimal digit.*/
               end   /*length(q)*/               /* [↑]  perform the    DO    W  times. */
          if !.s  then do; !.n=1; iterate n; end /*is  S  unhappy?    Then  Q  is also. */
          if @.s  then leave                     /*Have we found a  happy  number?      */
          q=s                                    /*try the  Q  sum to see if it's happy.*/
          end   /*until*/
    @.n=1                                        /*mark      N      as a   happy number.*/
    haps=haps+1                                  /*bump the counter of the happy numbers*/
    if haps<L  then iterate                      /*don't display  if    N    is too low.*/
    say  right(n, 30)                            /*display right justified happy number.*/
    end        /*n*/
                                                 /*stick a fork in it,  we're all done. */
```

```txt

                         13141
                         13142
                         13148
                         13158
                         13177
                         13182
                         13184
                         13185
                         13188
                         13203
                         13212
                         13214
                         13218
                         13221
                         13228
                         13230
                         13233
                         13241
                         13247
                         13248
                         13258
                         13266
                         13274
                         13281
                         13282
                         13284
                         13285
                         13299
                         13300
                         13302
                         13303
                         13305
                         13307

```


===optimized, horizontal list===
This REXX version is identical to the optimized version,   but displays the numbers in a horizontal list.

```rexx
/*REXX program   computes  and  displays   a specified  range  of   happy   numbers.    */
sw=linesize() - 1                                /*obtain the screen width  (less one). */
parse arg limit .                                /*obtain optional argument from the CL.*/
if L=='' | L==","  then L=8                      /*Not specified?  Then use the default.*/
if H=='' | H==","  then do;  H=L; L=1;  end      /*use a range for the displaying of #s.*/
            do i=0  to 9;  #.i=i**2;  end /*i*/  /*build a squared decimal digit table. */
@.=0;   @.1=1;        !.=@.;    !.2=1;    !.4=1  /*sparse array:   @≡happy,  !≡unhappy. */
haps=0                                           /*count of the happy numbers  (so far).*/
$=
    do n=1  while  haps<H                        /*search integers starting at  unity.  */
    if !.n  then iterate                         /*if  N  is unhappy, then try another. */
    q=n                                          /*(below)  Q  is the number tested.    */
          do  until q==1;          s=0           /*see if   Q  is a happy number.       */
          ?=q                                    /* [↓]    ?  is destructively PARSEd.  */
               do length(q)                      /*parse all the    decimal digits of ? */
               parse var  ?  _  +1 ?             /*obtain a  single decimal digit  of ? */
               s=s + #._                         /*add the square of that decimal digit.*/
               end   /*length(q)*/               /* [↑]  perform the   DO    W   times. */

          if !.s  then do; !.n=1; iterate n; end /*is  S  unhappy?    Then  Q  is also. */
          if @.s  then leave                     /*Have we found a  happy  number?      */
           q=s                                   /*try the  Q  sum to see if it's happy.*/
           end   /*until*/
    @.n=1                                        /*mark     N     as a   happy number.  */
    haps=haps+1                                  /*bump the count of the happy numbers. */
    if haps<L  then iterate                      /*don't display it,   N   is too low.  */
    $=$ n                                        /*add   N   to the horizontal list.    */
    if length($ n)>sw  then do                   /*if the list is too long, then split  */
                            say strip($)         /*     ··· and display what we've got. */
                            $=n                  /*Set the next line to overflow.       */
                            end                  /* [↑]  new line now contains overflow.*/
    end     /*n*/
if $\=''  then say  strip($)                     /*display any residual happy numbers.  */
                                                 /*stick a fork in it,  we're all done. */
```

This REXX program makes use of   '''linesize'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console).

Some REXXes don't have this BIF, so the   '''linesize.rex'''   REXX program is included here   ──►   [[LINESIZE.REX]].


(The   ''linesize''   for the terminal being used for this example was   200.)


(Shown at two-thirds size.)
<b>
<pre style="font-size:67%">
1 7 10 13 19 23 28 31 32 44 49 68 70 79 82 86 91 94 97 100 103 109 129 130 133 139 167 176 188 190 192 193 203 208 219 226 230 236 239 262 263 280 291 293 301 302 310 313 319 320 326 329 331 338 356
362 365 367 368 376 379 383 386 391 392 397 404 409 440 446 464 469 478 487 490 496 536 556 563 565 566 608 617 622 623 632 635 637 638 644 649 653 655 656 665 671 673 680 683 694 700 709 716 736 739
748 761 763 784 790 793 802 806 818 820 833 836 847 860 863 874 881 888 899 901 904 907 910 912 913 921 923 931 932 937 940 946 964 970 973 989 998 1000 1003 1009 1029 1030 1033 1039 1067 1076 1088
1090 1092 1093 1112 1114 1115 1121 1122 1125 1128 1141 1148 1151 1152 1158 1177 1182 1184 1185 1188 1209 1211 1212 1215 1218 1221 1222 1233 1247 1251 1257 1258 1274 1275 1277 1281 1285 1288 1290 1299
1300 1303 1309 1323 1330 1332 1333 1335 1337 1339 1353 1366 1373 1390 1393 1411 1418 1427 1444 1447 1448 1457 1472 1474 1475 1478 1481 1484 1487 1511 1512 1518 1521 1527 1528 1533 1547 1557 1572 1574
1575 1578 1581 1582 1587 1599 1607 1636 1663 1666 1670 1679 1697 1706 1717 1724 1725 1727 1733 1742 1744 1745 1748 1752 1754 1755 1758 1760 1769 1771 1772 1784 1785 1796 1808 1812 1814 1815 1818 1821
1825 1828 1841 1844 1847 1851 1852 1857 1874 1875 1880 1881 1882 1888 1900 1902 1903 1920 1929 1930 1933 1959 1967 1976 1992 1995 2003 2008 2019 2026 2030 2036 2039 2062 2063 2080 2091 2093 2109 2111
2112 2115 2118 2121 2122 2133 2147 2151 2157 2158 2174 2175 2177 2181 2185 2188 2190 2199 2206 2211 2212 2221 2224 2242 2245 2254 2257 2258 2260 2275 2285 2300 2306 2309 2313 2331 2333 2338 2339 2360
2369 2383 2390 2393 2396 2417 2422 2425 2448 2452 2455 2457 2458 2471 2475 2478 2484 2485 2487 2511 2517 2518 2524 2527 2528 2542 2545 2547 2548 2554 2555 2557 2568 2571 2572 2574 2575 2581 2582 2584
2586 2602 2603 2620 2630 2639 2658 2685 2693 2714 2715 2717 2725 2741 2745 2748 2751 2752 2754 2755 2771 2784 2800 2811 2815 2818 2825 2833 2844 2845 2847 2851 2852 2854 2856 2865 2874 2881 2899 2901
2903 2910 2919 2930 2933 2936 2963 2989 2991 2998 3001 3002 3010 3013 3019 3020 3026 3029 3031 3038 3056 3062 3065 3067 3068 3076 3079 3083 3086 3091 3092 3097 3100 3103 3109 3123 3130 3132 3133 3135
3137 3139 3153 3166 3173 3190 3193 3200 3206 3209 3213 3231 3233 3238 3239 3260 3269 3283 3290 3293 3296 3301 3308 3310 3312 3313 3315 3317 3319 3321 3323 3328 3329 3331 3332 3338 3346 3351 3355 3356
3364 3365 3367 3371 3376 3380 3382 3383 3391 3392 3436 3456 3463 3465 3466 3506 3513 3531 3535 3536 3546 3553 3560 3563 3564 3602 3605 3607 3608 3616 3620 3629 3634 3635 3637 3643 3645 3646 3650 3653
3654 3661 3664 3667 3670 3673 3676 3680 3689 3692 3698 3706 3709 3713 3731 3736 3760 3763 3766 3779 3789 3790 3797 3798 3803 3806 3823 3830 3832 3833 3860 3869 3879 3896 3897 3901 3902 3907 3910 3913
3920 3923 3926 3931 3932 3962 3968 3970 3977 3978 3986 3987 4004 4009 4040 4046 4064 4069 4078 4087 4090 4096 4111 4118 4127 4144 4147 4148 4157 4172 4174 4175 4178 4181 4184 4187 4217 4222 4225 4248
4252 4255 4257 4258 4271 4275 4278 4284 4285 4287 4336 4356 4363 4365 4366 4400 4406 4414 4417 4418 4428 4441 4447 4449 4455 4460 4471 4474 4477 4481 4482 4494 4517 4522 4525 4527 4528 4536 4545 4552
4554 4555 4558 4563 4571 4572 4577 4582 4585 4599 4604 4609 4633 4635 4636 4640 4653 4663 4690 4708 4712 4714 4715 4718 4721 4725 4728 4741 4744 4747 4751 4752 4757 4774 4775 4780 4781 4782 4788 4807
4811 4814 4817 4824 4825 4827 4841 4842 4852 4855 4870 4871 4872 4878 4887 4888 4900 4906 4944 4959 4960 4995 5036 5056 5063 5065 5066 5111 5112 5118 5121 5127 5128 5133 5147 5157 5172 5174 5175 5178
5181 5182 5187 5199 5211 5217 5218 5224 5227 5228 5242 5245 5247 5248 5254 5255 5257 5268 5271 5272 5274 5275 5281 5282 5284 5286 5306 5313 5331 5335 5336 5346 5353 5360 5363 5364 5417 5422 5425 5427
5428 5436 5445 5452 5454 5455 5458 5463 5471 5472 5477 5482 5485 5499 5506 5517 5524 5525 5527 5533 5542 5544 5545 5548 5552 5554 5555 5558 5560 5569 5571 5572 5584 5585 5596 5603 5605 5606 5628 5630
5633 5634 5643 5650 5659 5660 5666 5682 5695 5712 5714 5715 5718 5721 5722 5724 5725 5741 5742 5747 5751 5752 5774 5781 5789 5798 5799 5811 5812 5817 5821 5822 5824 5826 5842 5845 5854 5855 5862 5871
5879 5897 5919 5949 5956 5965 5978 5979 5987 5991 5994 5997 6008 6017 6022 6023 6032 6035 6037 6038 6044 6049 6053 6055 6056 6065 6071 6073 6080 6083 6094 6107 6136 6163 6166 6170 6179 6197 6202 6203
6220 6230 6239 6258 6285 6293 6302 6305 6307 6308 6316 6320 6329 6334 6335 6337 6343 6345 6346 6350 6353 6354 6361 6364 6367 6370 6373 6376 6380 6389 6392 6398 6404 6409 6433 6435 6436 6440 6453 6463
6490 6503 6505 6506 6528 6530 6533 6534 6543 6550 6559 6560 6566 6582 6595 6605 6613 6616 6631 6634 6637 6643 6650 6656 6661 6665 6673 6701 6703 6710 6719 6730 6733 6736 6763 6789 6791 6798 6800 6803
6825 6830 6839 6852 6879 6893 6897 6899 6904 6917 6923 6932 6938 6940 6955 6971 6978 6983 6987 6989 6998 7000 7009 7016 7036 7039 7048 7061 7063 7084 7090 7093 7106 7117 7124 7125 7127 7133 7142 7144
7145 7148 7152 7154 7155 7158 7160 7169 7171 7172 7184 7185 7196 7214 7215 7217 7225 7241 7245 7248 7251 7252 7254 7255 7271 7284 7306 7309 7313 7331 7336 7360 7363 7366 7379 7389 7390 7397 7398 7408
7412 7414 7415 7418 7421 7425 7428 7441 7444 7447 7451 7452 7457 7474 7475 7480 7481 7482 7488 7512 7514 7515 7518 7521 7522 7524 7525 7541 7542 7547 7551 7552 7574 7581 7589 7598 7599 7601 7603 7610
7619 7630 7633 7636 7663 7689 7691 7698 7711 7712 7721 7739 7744 7745 7754 7788 7793 7804 7814 7815 7824 7839 7840 7841 7842 7848 7851 7859 7869 7878 7884 7887 7893 7895 7896 7900 7903 7916 7930 7937
7938 7958 7959 7961 7968 7973 7983 7985 7986 7995 8002 8006 8018 8020 8033 8036 8047 8060 8063 8074 8081 8088 8099 8108 8112 8114 8115 8118 8121 8125 8128 8141 8144 8147 8151 8152 8157 8174 8175 8180
8181 8182 8188 8200 8211 8215 8218 8225 8233 8244 8245 8247 8251 8252 8254 8256 8265 8274 8281 8299 8303 8306 8323 8330 8332 8333 8360 8369 8379 8396 8397 8407 8411 8414 8417 8424 8425 8427 8441 8442
8452 8455 8470 8471 8472 8478 8487 8488 8511 8512 8517 8521 8522 8524 8526 8542 8545 8554 8555 8562 8571 8579 8597 8600 8603 8625 8630 8639 8652 8679 8693 8697 8699 8704 8714 8715 8724 8739 8740 8741
8742 8748 8751 8759 8769 8778 8784 8787 8793 8795 8796 8801 8808 8810 8811 8812 8818 8821 8847 8848 8874 8877 8880 8881 8884 8909 8929 8936 8937 8957 8963 8967 8969 8973 8975 8976 8990 8992 8996 9001
9004 9007 9010 9012 9013 9021 9023 9031 9032 9037 9040 9046 9064 9070 9073 9089 9098 9100 9102 9103 9120 9129 9130 9133 9159 9167 9176 9192 9195 9201 9203 9210 9219 9230 9233 9236 9263 9289 9291 9298
9301 9302 9307 9310 9313 9320 9323 9326 9331 9332 9362 9368 9370 9377 9378 9386 9387 9400 9406 9444 9459 9460 9495 9519 9549 9556 9565 9578 9579 9587 9591 9594 9597 9604 9617 9623 9632 9638 9640 9655
9671 9678 9683 9687 9689 9698 9700 9703 9716 9730 9737 9738 9758 9759 9761 9768 9773 9783 9785 9786 9795 9809 9829 9836 9837 9857 9863 9867 9869 9873 9875 9876 9890 9892 9896 9908 9912 9915 9921 9928
9945 9951 9954 9957 9968 9975 9980 9982 9986 10000 10003 10009 10029 10030 10033 10039 10067 10076 10088 10090 10092 10093 10112 10114 10115 10121 10122 10125 10128 10141 10148 10151 10152 10158
10177 10182 10184 10185 10188 10209 10211 10212 10215 10218 10221 10222 10233 10247 10251 10257 10258 10274 10275 10277 10281 10285 10288 10290 10299 10300 10303 10309 10323 10330 10332 10333 10335
10337

```

</b>


## Ring


```ring
n = 1
found = 0

While found < 8
    If IsHappy(n)
          found += 1
          see string(found) + " : " + string(n) + nl
    ok
    n += 1
End

Func IsHappy n
    cache = []
    While n != 1
        Add(cache,n)
        t = 0
        strn = string(n)
        for e in strn
            t += pow(number(e),2)
        next
        n = t
        If find(cache,n) Return False ok
    End
    Return True

```

```txt

1 : 1
2 : 7
3 : 10
4 : 13
5 : 19
6 : 23
7 : 28
8 : 31

```



## Ruby

```ruby
require 'set' # Set: Fast array lookup / Simple existence hash

@seen_numbers = Set.new
@happy_numbers = Set.new

def happy?(n)
  return true if n == 1 # Base case
  return @happy_numbers.include?(n) if @seen_numbers.include?(n) # Use performance cache, and stop unhappy cycles

  @seen_numbers << n
  digit_squared_sum = n.to_s.each_char.inject(0) { |sum, c| sum + c.to_i**2 } # In Rails: n.to_s.each_char.sum { c.to_i**2 }

  if happy?(digit_squared_sum)
    @happy_numbers << n
    true # Return true
  else
    false # Return false
  end
end
```


Helper method to produce output:

```ruby
def print_happy
  happy_numbers = []

  1.step do |i|
    break if happy_numbers.length >= 8
    happy_numbers << i if happy?(i)
  end

  p happy_numbers
end

print_happy
```


```ruby
[1, 7, 10, 13, 19, 23, 28, 31]
```



### Alternative version


```ruby
@memo = [0,1]
def happy(n)
  sum = n.to_s.chars.map{|c| c.to_i**2}.inject(:+)
  return @memo[sum] if @memo[sum]==0 or @memo[sum]==1
  @memo[sum] = 0                        # for the cycle check
  @memo[sum] = happy(sum)               # return 1:Happy number, 0:other
end

i = count = 0
while count < 8
  i += 1
  puts i or count+=1 if happy(i)==1
end

puts
for i in 99999999999900..99999999999999
  puts i if happy(i)==1
end
```


```txt

1
7
10
13
19
23
28
31

99999999999901
99999999999910
99999999999914
99999999999915
99999999999916
99999999999937
99999999999941
99999999999951
99999999999956
99999999999961
99999999999965
99999999999973

```



## Run BASIC


```runbasic
for i = 1 to 100
 if happy(i) = 1 then
  cnt = cnt + 1
  PRINT cnt;". ";i;" is a happy number "
  if cnt = 8 then end
 end if
next i

FUNCTION happy(num)
while count < 50 and happy <> 1
  num$	= str$(num)
  count	= count + 1
  happy	= 0
  for i = 1 to len(num$)
    happy = happy + val(mid$(num$,i,1)) ^ 2
  next i
  num = happy
wend
end function
```


```txt
1. 1 is a happy number
2. 7 is a happy number
3. 10 is a happy number
4. 13 is a happy number
5. 19 is a happy number
6. 23 is a happy number
7. 28 is a happy number
8. 31 is a happy number

```



## Rust

In Rust, using a tortoise/hare cycle detection algorithm (generic for integer types)

```rust
#![feature(core)]

fn sumsqd(mut n: i32) -> i32 {
    let mut sq = 0;
    while n > 0 {
        let d = n % 10;
        sq += d*d;
        n /= 10
    }
    sq
}

use std::num::Int;
fn cycle<T: Int>(a: T, f: fn(T) -> T) -> T {
    let mut t = a;
    let mut h = f(a);

    while t != h {
        t = f(t);
        h = f(f(h))
    }
    t
}

fn ishappy(n: i32) -> bool {
    cycle(n, sumsqd) == 1
}

fn main() {
    let happy = std::iter::count(1, 1)
                    .filter(|&n| ishappy(n))
                    .take(8)
                    .collect::<Vec<i32>>();

    println!("{:?}", happy)
}
```

```txt

[1, 7, 10, 13, 19, 23, 28, 31]

```



## Salmon


```Salmon
variable happy_count := 0;
outer:
iterate(x; [1...+oo])
  {
    variable seen := <<(* --> false)>>;
    variable now := x;
    while (true)
      {
        if (seen[now])
          {
            if (now == 1)
              {
                ++happy_count;
                print(x, " is happy.\n");
                if (happy_count == 8)
                    break from outer;;
              };
            break;
          };
        seen[now] := true;
        variable new := 0;
        while (now != 0)
          {
            new += (now % 10) * (now % 10);
            now /::= 10;
          };
        now := new;
      };
  };
```

This Salmon program produces the following output:

```txt
1 is happy.
7 is happy.
10 is happy.
13 is happy.
19 is happy.
23 is happy.
28 is happy.
31 is happy.
```



## Scala


```scala
scala>
 def isHappy(n: Int) = {
     |   new Iterator[Int] {
     |   val seen = scala.collection.mutable.Set[Int]()
     |   var curr = n
     |   def next = {
     |     val res = curr
     |     curr = res.toString.map(_.asDigit).map(n => n * n).sum
     |     seen += res
     |     res
     |   }
     |   def hasNext = !seen.contains(curr)
     | }.toList.last == 1
     | }
isHappy: (n: Int)Boolean

scala> Iterator from 1 filter isHappy take 8 foreach println
1
7
10
13
19
23
28
31

```



## Scheme


```scheme
(define (number->list num)
  (do ((num num (quotient num 10))
       (lst '() (cons (remainder num 10) lst)))
    ((zero? num) lst)))

(define (happy? num)
  (let loop ((num num) (seen '()))
    (cond ((= num 1) #t)
          ((memv num seen) #f)
          (else (loop (apply + (map (lambda (x) (* x x)) (number->list num)))
                      (cons num seen))))))

(display "happy numbers:")
(let loop ((n 1) (more 8))
  (cond ((= more 0) (newline))
        ((happy? n) (display " ") (display n) (loop (+ n 1) (- more 1)))
        (else (loop (+ n 1) more))))
```

The output is:

```txt
happy numbers: 1 7 10 13 19 23 28 31
```



## Scratch

Scratch is a free visual programming language. Click the link, then "See inside" to view the code.

https://scratch.mit.edu/projects/78912620/

This code will allow you to check if a positive interger (<=9999) is a happy number. It will also output a list of the first 8 happy numbers. (1 7 10 13 19 23 28 31)


## Seed7


```seed7
$ include "seed7_05.s7i";

const type: cacheType is hash [integer] boolean;
var cacheType: cache is cacheType.value;

const func boolean: happy (in var integer: number) is func
  result
    var boolean: isHappy is FALSE;
  local
    var bitset: cycle is bitset.value;
    var integer: newnumber is 0;
    var integer: cycleNum is 0;
  begin
    while number > 1 and number not in cycle do
      if number in cache then
        number := ord(cache[number]);
      else
        incl(cycle, number);
        newnumber := 0;
        while number > 0 do
          newnumber +:= (number rem 10) ** 2;
          number := number div 10;
        end while;
        number := newnumber;
      end if;
    end while;
    isHappy := number = 1;
    for cycleNum range cycle do
      cache @:= [cycleNum] isHappy;
    end for;
  end func;

const proc: main is func
  local
    var integer: number is 0;
  begin
    for number range 1 to 50 do
      if happy(number) then
        writeln(number);
      end if;
    end for;
  end func;
```


Output:

```txt

1
7
10
13
19
23
28
31
32
44
49

```



## SETL


```SETL
proc is_happy(n);
  s := [n];
  while n > 1 loop
    if (n := +/[val(i)**2: i in str(n)]) in s then
       return false;
    end if;
    s with:= n;
  end while;
  return true;
end proc;
```


```SETL
happy := [];
n := 1;
until #happy = 8 loop
  if is_happy(n) then happy with:= n; end if;
  n +:= 1;
end loop;

print(happy);
```

Output:

```txt
[1 7 10 13 19 23 28 31]
```

Alternative version:

```SETL
print([n : n in [1..100] | is_happy(n)](1..8));
```

Output:

```txt
[1 7 10 13 19 23 28 31]
```



## Sidef


```ruby
func happy(n) is cached {
    static seen = Hash()

    return true  if n.is_one
    return false if seen.exists(n)

    seen{n} = 1
    happy(n.digits.sum { _*_ })
}

say 8.defs {|i|
    happy(i) ? i : nil
}
```


```txt

[1, 7, 10, 13, 19, 23, 28, 31]

```



## SequenceL


```sequencel
import <Utilities/Math.sl>
;
import <Utilities/Conversion.sl>;

main(argv(2)) := findHappys(stringToInt(head(argv)));

findHappys(count) := findHappysHelper(count, 1, []);

findHappysHelper(count, n, happys(1)) :=
        happys when size(happys) = count
    else
        findHappysHelper(count, n + 1, happys ++ [n]) when isHappy(n)
    else
        findHappysHelper(count, n + 1, happys);

isHappy(n) := isHappyHelper(n, []);

isHappyHelper(n, cache(1)) :=
    let
        digits[i] := (n / integerPower(10, i - 1)) mod 10
                    foreach i within 1 ... ceiling(log(10, n + 1));
        newN := sum(integerPower(digits, 2));
    in
        false when some(n = cache)
    else
        true when n = 1
    else
        isHappyHelper(newN, cache ++ [n]);
```


```txt

$>happy.exe 8
[1,7,10,13,19,23,28,31]

```



## Smalltalk

In addition to the "Python's cache mechanism", the use of a Bag assures that found e.g. the happy 190, we already have in cache also the happy 910 and 109, and so on.

```smalltalk
Object subclass: HappyNumber [
  |cache negativeCache|
  HappyNumber class >> new [ |me|
    me := super new.
    ^ me init
  ]
  init [ cache := Set new. negativeCache := Set new. ]

  hasSad: aNum [
    ^ (negativeCache includes: (self recycle: aNum))
  ]
  hasHappy: aNum [
    ^ (cache includes: (self recycle: aNum))
  ]
  addHappy: aNum [
    cache add: (self recycle: aNum)
  ]
  addSad: aNum [
    negativeCache add: (self recycle: aNum)
  ]

  recycle: aNum [ |r n| r := Bag new.
    n := aNum.
    [ n > 0 ]
    whileTrue: [ |d|
      d := n rem: 10.
      r add: d.
      n := n // 10.
    ].
    ^r
  ]

  isHappy: aNumber [ |cycle number newnumber|
    number := aNumber.
    cycle := Set new.
    [ (number ~= 1) & ( (cycle includes: number) not ) ]
    whileTrue: [
      (self hasHappy: number)
      ifTrue: [ ^true ]
      ifFalse: [
        (self hasSad: number) ifTrue: [ ^false ].
        cycle add: number.
        newnumber := 0.
        [ number > 0 ]
        whileTrue: [ |digit|
          digit := number rem: 10.
          newnumber := newnumber + (digit * digit).
 	  number := (number - digit) // 10.
        ].
        number := newnumber.
      ]
    ].
    (number = 1)
    ifTrue: [
      cycle do: [ :e | self addHappy: e ].
      ^true
    ]
    ifFalse: [
      cycle do: [ :e | self addSad: e ].
      ^false
    ]
  ]
].
```


```smalltalk
|happy|
happy := HappyNumber new.

1 to: 31 do: [ :i |
  (happy isHappy: i)
  ifTrue: [ i displayNl ]
].
```

Output:
 1
 7
 10
 13
 19
 23
 28
 31

an alternative version is:
```smalltalk
|next isHappy happyNumbers|

next :=
    [:n |
        (n printString collect:[:ch | ch digitValue squared] as:Array) sum
    ].

isHappy :=
    [:n |  | t already |
        already := Set new.
        t := n.
        [ t == 1 or:[ (already includes:t)]] whileFalse:[
            already add:t.
            t := next value:t.
        ].
        t == 1
    ].

happyNumbers := OrderedCollection new.
try := 1.
[happyNumbers size < 8] whileTrue:[
      (isHappy value:try) ifTrue:[ happyNumbers add:try].
      try := try + 1
].
happyNumbers printCR
```

Output:
OrderedCollection(1 7 10 13 19 23 28 31)

## Swift


```Swift
func isHappyNumber(var n:Int) -> Bool {
    var cycle = [Int]()

    while n != 1 && !cycle.contains(n) {
        cycle.append(n)
        var m = 0
        while n > 0 {
            let d = n % 10
            m += d * d
            n = (n  - d) / 10
        }
        n = m
    }
    return n == 1
}

var found = 0
var count = 0
while found != 8 {
    if isHappyNumber(count) {
        print(count)
        found++
    }
    count++
}
```

```txt

1
7
10
13
19
23
28
31
```



## Tcl

using code from [[Sum of squares#Tcl]]

```tcl
proc is_happy n {
    set seen [list]
    while {$n > 1 && [lsearch -exact $seen $n] == -1} {
        lappend seen $n
        set n [sum_of_squares [split $n ""]]
    }
    return [expr {$n == 1}]
}

set happy [list]
set n -1
while {[llength $happy] < 8} {
    if {[is_happy $n]} {lappend happy $n}
    incr n
}
puts "the first 8 happy numbers are: [list $happy]"
```


```txt
the first 8 happy numbers are: {1 7 10 13 19 23 28 31}
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT
SECTION check
  IF (n!=1) THEN
   n = STRINGS (n,":>/:")
    LOOP/CLEAR nr=n
     square=nr*nr
     n=APPEND (n,square)
    ENDLOOP
   n=SUM(n)
   r_table=QUOTES (n)
   BUILD R_TABLE/word/EXACT chk=r_table
   IF (seq.ma.chk) THEN
    status="next"
   ELSE
    seq=APPEND (seq,n)
   ENDIF
   RELEASE r_table chk
  ELSE
    PRINT checkednr," is a happy number"
    happynrs=APPEND (happynrs,checkednr)
    status="next"
  ENDIF
ENDSECTION

happynrs=""

LOOP n=1,100
sz_happynrs=SIZE(happynrs)
IF (sz_happynrs==8) EXIT
checkednr=VALUE(n)
status=seq=""
 LOOP
  IF (status=="next") EXIT
  DO check
 ENDLOOP
ENDLOOP
```

Output:

```txt

1 is a happy number
7 is a happy number
10 is a happy number
13 is a happy number
19 is a happy number
23 is a happy number
28 is a happy number
31 is a happy number

```



## uBasic/4tH

<lang>
' ************************
' MAIN
' ************************

PROC _PRINT_HAPPY(20)
END

' ************************
' END MAIN
' ************************

' ************************
' SUBS & FUNCTIONS
' ************************

' --------------------
_is_happy PARAM(1)
' --------------------
LOCAL (5)
  f@ = 100
  c@ = a@
  b@ = 0

  DO WHILE b@ < f@
    e@ = 0

    DO WHILE c@
      d@ = c@ % 10
      c@ = c@ / 10
      e@ = e@ + (d@ * d@)
    LOOP

  UNTIL e@ = 1
    c@ = e@
    b@ = b@ + 1
  LOOP

RETURN(b@ < f@)

' --------------------
_PRINT_HAPPY PARAM(1)
' --------------------
LOCAL (2)
  b@ = 1
  c@ = 0

  DO

    IF FUNC (_is_happy(b@)) THEN
       c@ = c@ + 1
       PRINT b@
    ENDIF

    b@ = b@ + 1
    UNTIL c@ + 1 > a@
  LOOP

RETURN

' ************************
' END SUBS & FUNCTIONS
' ************************

```



## UNIX Shell

```bash
#!/bin/bash
function sum_of_square_digits
{
  local -i n="$1" sum=0
  while (( n )); do
    local -i d=n%10
    let sum+=d*d
    let n=n/10
  done
  echo "$sum"
}

function is_happy?
{
   local -i n="$1"
   local seen=()
   while (( n != 1 )); do
     if [ -n "${seen[$n]}" ]; then
        return 1
     fi
     seen[n]=1
     let n="$(sum_of_square_digits "$n")"
   done
   return 0
}

function first_n_happy
{
  local -i count="$1"
  local -i n
  for (( n=0; count; n+=1 )); do
  if is_happy? "$n"; then
    echo "$n"
    let count-=1
  fi
  done
  return 0
}

first_n_happy 8
```

Output:
```txt
1
7
10
13
19
23
28
31
```



## Ursala

The happy function is a predicate testing whether a given number is happy,
and first(p) defines a function mapping a number n to the first n
positive naturals having property p.

```Ursala
#import std
#import nat

happy = ==1+ ^== sum:-0+ product*iip+ %np*hiNCNCS+ %nP

first "p" = ~&i&& iota; ~&lrtPX/&; leql@lrPrX->lrx ^|\~& ^/successor@l ^|T\~& "p"&& ~&iNC

#cast %nL

main = (first happy) 8
```

output:

```txt
<1,7,10,13,19,23,28,31>
```



## Vala

```vala
using Gee;

/* function to sum the square of the digits */
int sum(int input){
	// convert input int to string
	string input_str = input.to_string();
	int total = 0;
	// read through each character in string, square them and add to total
	for (int x = 0; x < input_str.length; x++){
		// char.digit_value converts char to the decimal value the char it represents holds
		int digit = input_str[x].digit_value();
		total += (digit * digit);
	}

	return total;
} // end sum

/* function to decide if a number is a happy number */
bool is_happy(int total){
	var past = new HashSet<int>();
	while(true){
		total = sum(total);
		if (total == 1){
			return true;}

		if (total in past){
			return false;}

		past.add(total);
	} // end while loop
} // end happy

public static void main(){
	var happynums = new ArrayList<int>();
	int x = 1;

	while (happynums.size < 8){
		if (is_happy(x) == true)
			happynums.add(x);
		x++;
	}

	foreach(int num in happynums)
		stdout.printf("%d ", num);
	stdout.printf("\n");
} // end main
```

The output is:

```txt

1 7 10 13 19 23 28 31

```



## VBA



```vb

Option Explicit

Sub Test_Happy()
Dim i&, Cpt&

    For i = 1 To 100
        If Is_Happy_Number(i) Then
            Debug.Print "Is Happy : " & i
            Cpt = Cpt + 1
            If Cpt = 8 Then Exit For
        End If
    Next
End Sub

Public Function Is_Happy_Number(ByVal N As Long) As Boolean
Dim i&, Number$, Cpt&
    Is_Happy_Number = False 'default value
    Do
        Cpt = Cpt + 1       'Count Loops
        Number = CStr(N)    'conversion Long To String to be able to use Len() function
        N = 0
        For i = 1 To Len(Number)
            N = N + CInt(Mid(Number, i, 1)) ^ 2
        Next i
        'If Not N = 1 after 50 Loop ==> Number Is Not Happy
        If Cpt = 50 Then Exit Function
    Loop Until N = 1
    Is_Happy_Number = True
End Function

```

```txt
Is Happy : 1
Is Happy : 7
Is Happy : 10
Is Happy : 13
Is Happy : 19
Is Happy : 23
Is Happy : 28
Is Happy : 31
```



## VBScript


```vb

count = 0
firsteigth=""
For i = 1 To 100
	If IsHappy(CInt(i)) Then
		firsteight = firsteight & i & ","
		count = count + 1
	End If
	If count = 8 Then
		Exit For
	End If
Next
WScript.Echo firsteight

Function IsHappy(n)
	IsHappy = False
	m = 0
	Do Until m = 60
		sum = 0
		For j = 1 To Len(n)
			sum = sum + (Mid(n,j,1))^2
		Next
		If sum = 1 Then
			IsHappy = True
			Exit Do
		Else
			n = sum
			m = m + 1
		End If
	Loop
End Function

```


```txt
1,7,10,13,19,23,28,31,
```



## Visual Basic .NET

This version uses Linq to carry out the calculations.

```vbnet
Module HappyNumbers
    Sub Main()
        Dim n As Integer = 1
        Dim found As Integer = 0

        Do Until found = 8
            If IsHappy(n) Then
                found += 1
                Console.WriteLine("{0}: {1}", found, n)
            End If
            n += 1
        Loop

        Console.ReadLine()
    End Sub

    Private Function IsHappy(ByVal n As Integer)
        Dim cache As New List(Of Long)()

        Do Until n = 1
            cache.Add(n)
            n = Aggregate c In n.ToString() _
                Into Total = Sum(Int32.Parse(c) ^ 2)
            If cache.Contains(n) Then Return False
        Loop

        Return True
    End Function
End Module
```

The output is:

```txt
1: 1
2: 7
3: 10
4: 13
5: 19
6: 23
7: 28
8: 31
```


### Cacheless version

Curiously, this runs in about two thirds of the time of the cacheless C# version on Tio.run.

```vbnet
Module Module1

    Dim sq As Integer() = {1, 4, 9, 16, 25, 36, 49, 64, 81}

    Function isOne(x As Integer) As Boolean
        While True
            If x = 89 Then Return False
            Dim t As Integer, s As Integer = 0
            Do
                t = (x Mod 10) - 1 : If t >= 0 Then s += sq(t)
                x \= 10
            Loop While x > 0
            If s = 1 Then Return True
            x = s
        End While
        Return False
    End Function

    Sub Main(ByVal args As String())
        Const Max As Integer = 10_000_000
        Dim st As DateTime = DateTime.Now
        Console.Write("---Happy Numbers---" & vbLf & "The first 8:")
        Dim i As Integer = 1, c As Integer = 0
        While c < 8
            If isOne(i) Then Console.Write("{0} {1}", If(c = 0, "", ","), i, c) : c += 1
            i += 1
        End While
        Dim m As Integer = 10
        While m <= Max
            Console.Write(vbLf & "The {0:n0}th: ", m)
            While c < m
                If isOne(i) Then c += 1
                i += 1
            End While
            Console.Write("{0:n0}", i - 1)
            m = m * 10
        End While
        Console.WriteLine(vbLf & "Computation time {0} seconds.", (DateTime.Now - st).TotalSeconds)
    End Sub
End Module
```

```txt
---Happy Numbers---
The first 8: 1, 7, 10, 13, 19, 23, 28, 31
The 10th: 44
The 100th: 694
The 1,000th: 6,899
The 10,000th: 67,169
The 100,000th: 692,961
The 1,000,000th: 7,105,849
The 10,000,000th: 71,313,350
Computation time 19.235551 seconds.
```



## XPL0

The largest possible 32-bit integer is less than 9,999,999,999. The sum of
the squares of these ten digits is 10*9^2 = 810. If a cycle consisted of all
the values smaller than 810, an array size of 810 would still be sufficiently
large to hold them. Actually, tests show that the array only needs to hold 16
numbers.


```XPL0
int List(810);          \list of numbers in a cycle
int Inx;                \index for List
include c:\cxpl\codes;


func HadNum(N);         \Return 'true' if number N is in the List
int N;
int I;
[for I:= 0 to Inx-1 do
        if N = List(I) then return true;
return false;
]; \HadNum


func SqDigits(N);       \Return the sum of the squares of the digits of N
int N;
int S, D;
[S:= 0;
while N do
        [N:= N/10;
        D:= rem(0);
        S:= S + D*D;
        ];
return S;
]; \SqDigits


int N0, N, C;
[N0:= 0;                \starting number
C:= 0;                  \initialize happy (starting) number counter
repeat  N:= N0;
        Inx:= 0;        \reset List index
        loop    [N:= SqDigits(N);
                if N = 1 then                   \happy number
                        [IntOut(0, N0);  CrLf(0);
                        C:= C+1;
                        quit;
                        ];
                if HadNum(N) then quit;         \if unhappy number then quit
                List(Inx):= N;                  \if neither, add it to the List
                Inx:= Inx+1;                    \ and continue the cycle
                ];
        N0:= N0+1;                              \next starting number
until   C=8;            \done when 8 happy numbers have been found
]
```


Output:

```txt

1
7
10
13
19
23
28
31

```



## zkl

Here is a function that generates a continuous stream of happy numbers. Given that there are lots of happy numbers, caching them doesn't seem like a good idea memory wise. Instead, a num of squared digits == 4 is used as a proxy for a cycle (see the Wikipedia article, there are several number that will work).
```zkl
fcn happyNumbers{  // continously spew happy numbers
   foreach N in ([1..]){
       n:=N; while(1){
	 n=n.split().reduce(fcn(p,n){ p + n*n },0);
	 if(n==1) { vm.yield(N); break; }
	 if(n==4) break;  // unhappy cycle
      }
   }
}
```


```zkl
h:=Utils.Generator(happyNumbers);
h.walk(8).println();
```

```txt
L(1,7,10,13,19,23,28,31)
```

Get the one million-th happy number. Nobody would call this quick.

```zkl
Utils.Generator(happyNumbers).drop(0d1_000_000-1).next().println();
```

```txt
7105849
```



## ZX Spectrum Basic

```zxbasic
10 FOR i=1 TO 100
20 GO SUB 1000
30 IF isHappy=1 THEN PRINT i;" is a happy number"
40 NEXT i
50 STOP
1000 REM Is Happy?
1010 LET isHappy=0: LET count=0: LET num=i
1020 IF count=50 OR isHappy=1 THEN RETURN
1030 LET n$=STR$ (num)
1040 LET count=count+1
1050 LET isHappy=0
1060 FOR j=1 TO LEN n$
1070 LET isHappy=isHappy+VAL n$(j)^2
1080 NEXT j
1090 LET num=isHappy
1100 GO TO 1020
```

