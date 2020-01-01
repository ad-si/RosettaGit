+++
title = "Fibonacci n-step number sequences"
description = ""
date = 2019-07-28T17:56:32Z
aliases = []
[extra]
id = 11776
[taxonomies]
categories = []
tags = []
+++

{{task}}
These number series are an expansion of the ordinary [[Fibonacci sequence]] where:
# For <math>n = 2</math> we have the Fibonacci sequence; with initial values <math>[1, 1]</math> and <math>F_k^2 = F_{k-1}^2 + F_{k-2}^2</math>
# For <math>n = 3</math> we have the tribonacci sequence; with initial values <math>[1, 1, 2]</math> and <math>F_k^3 = F_{k-1}^3 + F_{k-2}^3 + F_{k-3}^3</math>
# For <math>n = 4</math> we have the tetranacci sequence; with initial values <math>[1, 1, 2, 4]</math> and <math>F_k^4 = F_{k-1}^4 + F_{k-2}^4 + F_{k-3}^4 + F_{k-4}^4</math>
...
# For general <math>n>2</math> we have the Fibonacci <math>n</math>-step sequence - <math>F_k^n</math>; with initial values of the first <math>n</math> values of the <math>(n-1)</math>'th Fibonacci <math>n</math>-step sequence <math>F_k^{n-1}</math>; and <math>k</math>'th value of this <math>n</math>'th sequence being <math>F_k^n = \sum_{i=1}^{(n)} {F_{k-i}^{(n)}}</math>

For small values of <math>n</math>, [[wp:Number prefix#Greek_series|Greek numeric prefixes]] are sometimes used to individually name each series.

:::: {| style="text-align: left;" border="4" cellpadding="2" cellspacing="2"
|+ Fibonacci <math>n</math>-step sequences
|- style="background-color: rgb(255, 204, 255);"
! <math>n</math> !! Series name !! Values
|-
|  2 ||  fibonacci || 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 ...
|-
|  3 || tribonacci || 1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136 ...
|-
|  4 || tetranacci || 1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536 ...
|-
|  5 || pentanacci || 1 1 2 4 8 16 31 61 120 236 464 912 1793 3525 6930 ...
|-
|  6 ||  hexanacci || 1 1 2 4 8 16 32 63 125 248 492 976 1936 3840 7617 ...
|-
|  7 || heptanacci || 1 1 2 4 8 16 32 64 127 253 504 1004 2000 3984 7936 ...
|-
|  8 ||  octonacci || 1 1 2 4 8 16 32 64 128 255 509 1016 2028 4048 8080 ...
|-
|  9 ||  nonanacci || 1 1 2 4 8 16 32 64 128 256 511 1021 2040 4076 8144 ...
|-
| 10 ||  decanacci || 1 1 2 4 8 16 32 64 128 256 512 1023 2045 4088 8172 ...
|}

Allied sequences can be generated where the initial values are changed:
: '''The [[wp:Lucas number|Lucas series]]''' sums the two preceding values like the fibonacci series for <math>n=2</math> but uses <math>[2, 1]</math> as its initial values.

<!-- Lucas numbers, Lucas number, Lucas series     [added to make searches easier.] -->



;Task:
# Write a function to generate Fibonacci <math>n</math>-step number sequences given its initial values and assuming the number of initial values determines how many previous values are summed to make the next number of the series.
# Use this to print and show here at least the first ten members of the Fibo/tribo/tetra-nacci and Lucas sequences.


;Related tasks:
*   [[Fibonacci sequence]]
*   [http://mathworld.wolfram.com/Fibonaccin-StepNumber.html Wolfram Mathworld]
*   [[Hofstadter Q sequence‎]]
*   [[Leonardo numbers]]


;Also see:
*   [https://www.youtube.com/watch?v=PeUbRXnbmms Lucas Numbers - Numberphile] (Video)
*   [https://www.youtube.com/watch?v=fMJflV_GUpU Tribonacci Numbers (and the Rauzy Fractal) - Numberphile] (Video)






## Ada


First, we specify a package Bonacci, that defines the type Sequence (of Positive numbers), a function Generate that takes a given Start sequence and outputs a generalized N-Bonacci Sequence of a spefified Length, and some constant start sequences.


```Ada
package Bonacci is

   type Sequence is array(Positive range <>) of Positive;

   function Generate(Start: Sequence; Length: Positive := 10) return Sequence;

   Start_Fibonacci:  constant Sequence := (1, 1);
   Start_Tribonacci: constant Sequence := (1, 1, 2);
   Start_Tetranacci: constant Sequence := (1, 1, 2, 4);
   Start_Lucas:      constant Sequence := (2, 1);
end Bonacci;
```


The implementation is quite straightforward.


```Ada
package body Bonacci is

   function Generate(Start: Sequence; Length: Positive := 10) return Sequence is
   begin
      if Length <= Start'Length then
         return Start(Start'First .. Start'First+Length-1);
      else
         declare
            Sum: Natural := 0;
         begin
            for I in Start'Range loop
               Sum := Sum + Start(I);
            end loop;
            return Start(Start'First)
              & Generate(Start(Start'First+1 .. Start'Last) & Sum, Length-1);
         end;
      end if;
   end Generate;

end Bonacci;
```


Finally, we actually generate some sequences, as required by the task. For convenience, we define a procedure Print that outputs a sequence,


```Ada
with Ada.Text_IO, Bonacci;

procedure Test_Bonacci is

   procedure Print(Name: String; S: Bonacci.Sequence) is
   begin
      Ada.Text_IO.Put(Name & "(");
      for I in S'First .. S'Last-1 loop
         Ada.Text_IO.Put(Integer'Image(S(I)) & ",");
      end loop;
      Ada.Text_IO.Put_Line(Integer'Image(S(S'Last)) & " )");
   end Print;

begin
   Print("Fibonacci:   ", Bonacci.Generate(Bonacci.Start_Fibonacci));
   Print("Tribonacci:  ", Bonacci.Generate(Bonacci.Start_Tribonacci));
   Print("Tetranacci:  ", Bonacci.Generate(Bonacci.Start_Tetranacci));
   Print("Lucas:       ", Bonacci.Generate(Bonacci.Start_Lucas));
   Print("Decanacci:   ",
         Bonacci.Generate((1, 1, 2, 4, 8, 16, 32, 64, 128, 256), 15));
end Test_Bonacci;
```


The output:


```txt
Fibonacci:   ( 1, 1, 2, 3, 5, 8, 13, 21, 34, 55 )
Tribonacci:  ( 1, 1, 2, 4, 7, 13, 24, 44, 81, 149 )
Tetranacci:  ( 1, 1, 2, 4, 8, 15, 29, 56, 108, 208 )
Lucas:       ( 2, 1, 3, 4, 7, 11, 18, 29, 47, 76 )
Decanacci:   ( 1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1023, 2045, 4088, 8172 )
```



## ACL2


```lisp
(defun sum (xs)
   (if (endp xs)
       0
       (+ (first xs)
          (sum (rest xs)))))

(defun n-bonacci (prevs limit)
   (if (zp limit)
       nil
       (let ((next (append (rest prevs)
                           (list (sum prevs)))))
            (cons (first next)
                  (n-bonacci next (1- limit))))))
```


Output:

```txt
&gt; (n-bonacci '(1 1) 10)
(1 2 3 5 8 13 21 34 55 89)
&gt; (n-bonacci '(1 1 2) 10)
(1 2 4 7 13 24 44 81 149 274)
&gt; (n-bonacci '(1 1 2 4) 10)
(1 2 4 8 15 29 56 108 208 401)
&gt; (n-bonacci '(2 1) 10)
(1 3 4 7 11 18 29 47 76 123)
```



## ALGOL 68


```algol68
# returns an array of the first required count elements of an a n-step fibonacci sequence #
# the initial values are taken from the init array                                        #
PROC n step fibonacci sequence = ( []INT init, INT required count )[]INT:
     BEGIN
         [ 1 : required count ]INT result;
         []INT initial values = init[ AT 1 ];
         INT step             = UPB initial values;
         # install the initial values                                                     #
         FOR n TO step DO result[ n ] := initial values[ n ] OD;
         # calculate the rest of the sequence                                             #
         FOR n FROM step + 1 TO required count DO
             result[ n ] := 0;
             FOR p FROM n - step TO n - 1 DO result[ n ] +:= result[ p ] OD
         OD;
         result
     END; # required count #

# prints the elements of a sequence                                                       #
PROC print sequence = ( STRING legend, []INT sequence )VOID:
     BEGIN
        print( ( legend, ":" ) );
        FOR e FROM LWB sequence TO UPB sequence DO print( ( " ", whole( sequence[ e ], 0 ) ) ) OD;
        print( ( newline ) )
     END; # print sequence #

# print some sequences                                                                    #
print sequence( "fibonacci   ", n step fibonacci sequence( ( 1, 1 ),       10 ) );
print sequence( "tribonacci  ", n step fibonacci sequence( ( 1, 1, 2 ),    10 ) );
print sequence( "tetrabonacci", n step fibonacci sequence( ( 1, 1, 2, 4 ), 10 ) );
print sequence( "lucus       ", n step fibonacci sequence( ( 2, 1 ),       10 ) )

```

{{out}}

```txt

fibonacci   : 1 1 2 3 5 8 13 21 34 55
tribonacci  : 1 1 2 4 7 13 24 44 81 149
tetrabonacci: 1 1 2 4 8 15 29 56 108 208
lucus       : 2 1 3 4 7 11 18 29 47 76

```



## AppleScript


```applescript
use AppleScript version "2.4"
use framework "Foundation"
use scripting additions


-- Start sequence -> Number of terms -> terms
-- takeNFibs :: [Int] -> Int -> [Int]
on takeNFibs(xs, n)
    script go
        on |λ|(xs, n)
            if 0 < n and 0 < length of xs then
                cons(head(xs), ¬
                    |λ|(append(tail(xs), {sum(xs)}), n - 1))
            else
                {}
            end if
        end |λ|
    end script
    go's |λ|(xs, n)
end takeNFibs

-- fibInit :: Int -> [Int]
on fibInit(n)
    script powerOfTwo
        on |λ|(x)
            2 ^ x as integer
        end |λ|
    end script
    cons(1, map(powerOfTwo, enumFromToInt(0, n - 2)))
end fibInit

-- TEST ---------------------------------------------------
on run
    set intTerms to 15
    script series
        on |λ|(s, n)
            justifyLeft(12, space, s & "nacci") & " -> " & ¬
                showJSON(takeNFibs(fibInit(n), intTerms))
        end |λ|
    end script

    set strTable to unlines(zipWith(series, ¬
        words of ("fibo tribo tetra penta hexa hepta octo nona deca"), ¬
        enumFromToInt(2, 10)))

    justifyLeft(12, space, "Lucas ") & " -> " & ¬
        showJSON(takeNFibs({2, 1}, intTerms)) & linefeed & strTable
end run

-- GENERIC FUNCTIONS --------------------------------------

-- Append two lists.
-- append (++) :: [a] -> [a] -> [a]
-- append (++) :: String -> String -> String
on append(xs, ys)
    xs & ys
end append

-- cons :: a -> [a] -> [a]
on cons(x, xs)
    if list is class of xs then
        {x} & xs
    else
        x & xs
    end if
end cons

-- enumFromToInt :: Int -> Int -> [Int]
on enumFromToInt(m, n)
    if m ≤ n then
        set lst to {}
        repeat with i from m to n
            set end of lst to i
        end repeat
        return lst
    else
        return {}
    end if
end enumFromToInt

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

-- head :: [a] -> a
on head(xs)
    if xs = {} then
        missing value
    else
        item 1 of xs
    end if
end head

-- justifyLeft :: Int -> Char -> String -> String
on justifyLeft(n, cFiller, strText)
    if n > length of strText then
        text 1 thru n of (strText & replicate(n, cFiller))
    else
        strText
    end if
end justifyLeft

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

-- Egyptian multiplication - progressively doubling a list, appending
-- stages of doubling to an accumulator where needed for binary
-- assembly of a target length
-- replicate :: Int -> a -> [a]
on replicate(n, a)
    set out to {}
    if n < 1 then return out
    set dbl to {a}

    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- showJSON :: a -> String
on showJSON(x)
    set c to class of x
    if (c is list) or (c is record) then
        set ca to current application
        set {json, e} to ca's NSJSONSerialization's ¬
            dataWithJSONObject:x options:0 |error|:(reference)
        if json is missing value then
            e's localizedDescription() as text
        else
            (ca's NSString's alloc()'s ¬
                initWithData:json encoding:(ca's NSUTF8StringEncoding)) as text
        end if
    else if c is date then
        "\"" & ((x - (time to GMT)) as «class isot» as string) & ".000Z" & "\""
    else if c is text then
        "\"" & x & "\""
    else if (c is integer or c is real) then
        x as text
    else if c is class then
        "null"
    else
        try
            x as text
        on error
            ("«" & c as text) & "»"
        end try
    end if
end showJSON

-- sum :: [Num] -> Num
on sum(xs)
    script add
        on |λ|(a, b)
            a + b
        end |λ|
    end script

    foldl(add, 0, xs)
end sum

-- tail :: [a] -> [a]
on tail(xs)
    set blnText to text is class of xs
    if blnText then
        set unit to ""
    else
        set unit to {}
    end if
    set lng to length of xs
    if 1 > lng then
        missing value
    else if 2 > lng then
        unit
    else
        if blnText then
            text 2 thru -1 of xs
        else
            rest of xs
        end if
    end if
end tail

-- unlines :: [String] -> String
on unlines(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, linefeed}
    set str to xs as text
    set my text item delimiters to dlm
    str
end unlines

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
on zipWith(f, xs, ys)
    set lng to min(length of xs, length of ys)
    if 1 > lng then return {}
    set lst to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, item i of ys)
        end repeat
        return lst
    end tell
end zipWith

```

{{Out}}

```txt
Lucas        -> [2,1,3,4,7,11,18,29,47,76,123,199,322,521,843]
fibonacci    -> [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]
tribonacci   -> [1,1,2,4,7,13,24,44,81,149,274,504,927,1705,3136]
tetranacci   -> [1,1,2,4,8,15,29,56,108,208,401,773,1490,2872,5536]
pentanacci   -> [1,1,2,4,8,16,31,61,120,236,464,912,1793,3525,6930]
hexanacci    -> [1,1,2,4,8,16,32,63,125,248,492,976,1936,3840,7617]
heptanacci   -> [1,1,2,4,8,16,32,64,127,253,504,1004,2000,3984,7936]
octonacci    -> [1,1,2,4,8,16,32,64,128,255,509,1016,2028,4048,8080]
nonanacci    -> [1,1,2,4,8,16,32,64,128,256,511,1021,2040,4076,8144]
decanacci    -> [1,1,2,4,8,16,32,64,128,256,512,1023,2045,4088,8172]
```



## AutoHotkey


```AutoHotkey
for i, seq in ["nacci", "lucas"]
    Loop, 9 {
        Out .= seq "(" A_Index + 1 "): "
        for key, val in NStepSequence(i, 1, A_Index + 1, 15)
            Out .= val (A_Index = 15 ? "`n" : "`, ")
    }
MsgBox, % Out

NStepSequence(v1, v2, n, k) {
    a := [v1, v2]
    Loop, % k - 2 {
        a[j := A_Index + 2] := 0
        Loop, % j < n + 2 ? j - 1 : n
            a[j] += a[j - A_Index]
    }
    return, a
}
```

'''Output:'''

```txt
nacci(2): 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610
nacci(3): 1, 1, 2, 4, 7, 13, 24, 44, 81, 149, 274, 504, 927, 1705, 3136
nacci(4): 1, 1, 2, 4, 8, 15, 29, 56, 108, 208, 401, 773, 1490, 2872, 5536
nacci(5): 1, 1, 2, 4, 8, 16, 31, 61, 120, 236, 464, 912, 1793, 3525, 6930
nacci(6): 1, 1, 2, 4, 8, 16, 32, 63, 125, 248, 492, 976, 1936, 3840, 7617
nacci(7): 1, 1, 2, 4, 8, 16, 32, 64, 127, 253, 504, 1004, 2000, 3984, 7936
nacci(8): 1, 1, 2, 4, 8, 16, 32, 64, 128, 255, 509, 1016, 2028, 4048, 8080
nacci(9): 1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 511, 1021, 2040, 4076, 8144
nacci(10): 1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1023, 2045, 4088, 8172
lucas(2): 2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123, 199, 322, 521, 843
lucas(3): 2, 1, 3, 6, 10, 19, 35, 64, 118, 217, 399, 734, 1350, 2483, 4567
lucas(4): 2, 1, 3, 6, 12, 22, 43, 83, 160, 308, 594, 1145, 2207, 4254, 8200
lucas(5): 2, 1, 3, 6, 12, 24, 46, 91, 179, 352, 692, 1360, 2674, 5257, 10335
lucas(6): 2, 1, 3, 6, 12, 24, 48, 94, 187, 371, 736, 1460, 2896, 5744, 11394
lucas(7): 2, 1, 3, 6, 12, 24, 48, 96, 190, 379, 755, 1504, 2996, 5968, 11888
lucas(8): 2, 1, 3, 6, 12, 24, 48, 96, 192, 382, 763, 1523, 3040, 6068, 12112
lucas(9): 2, 1, 3, 6, 12, 24, 48, 96, 192, 384, 766, 1531, 3059, 6112, 12212
lucas(10): 2, 1, 3, 6, 12, 24, 48, 96, 192, 384, 768, 1534, 3067, 6131, 12256
```



## AWK


```AWK

function sequence(values, howmany) {
	init_length = length(values)
	for (i=init_length + 1; i<=howmany; i++) {
	   values[i] = 0
           for (j=1; j<=init_length; j++) {
             values[i] += values[i-j]
	   }
        }
	result = ""
        for (i in values) {
          result = result values[i] " "
        }
	delete values
	return result
}

# print some sequences
END	{
		a[1] = 1; a[2] = 1
		print("fibonacci :\t",sequence(a, 10))

		a[1] = 1; a[2] = 1; a[3] = 2
		print("tribonacci :\t",sequence(a, 10))

		a[1] = 1 ; a[2] = 1 ; a[3] = 2 ; a[4] = 4
		print("tetrabonacci :\t",sequence(a, 10))

		a[1] = 2; a[2] = 1
		print("lucas :\t\t",sequence(a, 10))
	}

```

'''Output:'''

```txt

fibonacci :	 1 1 2 3 5 8 13 21 34 55
tribonacci :	 1 1 2 4 7 13 24 44 81 149
tetrabonacci :	 1 1 2 4 8 15 29 56 108 208
lucas :		 2 1 3 4 7 11 18 29 47 76

```



## Batch File


```dos

@echo off

echo Fibonacci Sequence:
call:nfib 1 1
echo.

echo Tribonacci Sequence:
call:nfib 1 1 2
echo.

echo Tetranacci Sequence:
call:nfib 1 1 2 4
echo.

echo Lucas Numbers:
call:nfib 2 1
echo.

pause>nul
exit /b

:nfib
setlocal enabledelayedexpansion

for %%i in (%*) do (
  set /a count+=1
  set seq=!seq! %%i
)
set "seq=%seq% ^| "
set n=-%count%
set /a n+=1
for %%i in (%*) do (
  set F!n!=%%i
  set /a n+=1
)

for /l %%i in (1,1,10) do (
  set /a termstart=%%i-%count%%
  set /a termend=%%i-1
  for /l %%j in (!termstart!,1,!termend!) do (
    set /a F%%i+=!F%%j!
  )
  set seq=!seq! !F%%i!
)
echo %seq%

endlocal
exit /b

```

{{out}}

```txt

Fibonacci Sequence:
 1 1 |  2 3 5 8 13 21 34 55 89 144

Tribonacci Sequence:
 1 1 2 |  4 7 13 24 44 81 149 274 504 927

Tetranacci Sequence:
 1 1 2 4 |  8 15 29 56 108 208 401 773 1490 2872

Lucas Numbers:
 2 1 |  3 4 7 11 18 29 47 76 123 199

```




## BBC BASIC

The BBC BASIC '''SUM''' function is useful here.

```bbcbasic
      @% = 5 : REM Column width

      PRINT "Fibonacci:"
      DIM f2%(1) : f2%() = 1,1
      FOR i% = 1 TO 12 : PRINT f2%(0); : PROCfibn(f2%()) : NEXT : PRINT " ..."

      PRINT "Tribonacci:"
      DIM f3%(2) : f3%() = 1,1,2
      FOR i% = 1 TO 12 : PRINT f3%(0); : PROCfibn(f3%()) : NEXT : PRINT " ..."

      PRINT "Tetranacci:"
      DIM f4%(3) : f4%() = 1,1,2,4
      FOR i% = 1 TO 12 : PRINT f4%(0); : PROCfibn(f4%()) : NEXT : PRINT " ..."

      PRINT "Lucas:"
      DIM fl%(1) : fl%() = 2,1
      FOR i% = 1 TO 12 : PRINT fl%(0); : PROCfibn(fl%()) : NEXT : PRINT " ..."
      END

      DEF PROCfibn(f%())
      LOCAL i%, s%
      s% = SUM(f%())
      FOR i% = 1 TO DIM(f%(),1)
        f%(i%-1) = f%(i%)
      NEXT
      f%(i%-1) = s%
      ENDPROC
```

'''Output:'''

```txt

Fibonacci:
    1    1    2    3    5    8   13   21   34   55   89  144 ...
Tribonacci:
    1    1    2    4    7   13   24   44   81  149  274  504 ...
Tetranacci:
    1    1    2    4    8   15   29   56  108  208  401  773 ...
Lucas:
    2    1    3    4    7   11   18   29   47   76  123  199 ...

```



## Befunge



```befunge>110p>
55+109"iccanaceD"22099v
v9013"Tetranacci"9014"Lucas"<
>"iccanobirT"2109"iccanobiF"v
>>:#,_0p20p0>:01-\2>#v0>#g<>>
 ^_@#:,+55$_^ JH v`1:v#\p03<
 _$.1+:77+`^vg03:_0g+>\:1+#^
 50p-\30v v\<>\30g1-\^$$_:1-
 05g04\g< >`#^_:40p30g0>^!:g
```


{{out}}


```txt
Fibonacci       1 1 2 3 5 8 13 21 34 55 89 144 233 377 610
Tribonacci      1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136
Tetranacci      1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536
Lucas           2 1 3 4 7 11 18 29 47 76 123 199 322 521 843
Decanacci       1 1 2 4 8 16 32 64 128 256 512 1023 2045 4088 8172
```



## Bracmat

{{trans|PicoLisp}}

```bracmat
( ( nacci
  =   Init Cnt N made tail
    .   ( plus
        =   n
          .   !arg:#%?n ?arg&!n+plus$!arg
            | 0
        )
      & !arg:(?Init.?Cnt)
      & !Init:? [?N
      & !Init:?made
      & !Cnt+-1*!N:?times
      & -1+-1*!N:?M
      &   whl
        ' ( !times+-1:~<0:?times
          & !made:? [!M ?tail
          & !made plus$!tail:?made
          )
      & !made
  )
& ( pad
  =   len w
    .   @(!arg:? [?len)
      & @("          ":? [!len ?w)
      & !w !arg
  )
&     (fibonacci.1 1)
      (tribonacci.1 1 2)
      (tetranacci.1 1 2 4)
      (pentanacci.1 1 2 4 8)
      (hexanacci.1 1 2 4 8 16)
      (heptanacci.1 1 2 4 8 16 32)
      (octonacci.1 1 2 4 8 16 32 64)
      (nonanacci.1 1 2 4 8 16 32 64 128)
      (decanacci.1 1 2 4 8 16 32 64 128 256)
      (lucas.2 1)
  : ?L
&   whl
  ' ( !L:(?name.?Init) ?L
    & out$(str$(pad$!name ": ") nacci$(!Init.12))
    )
);
```

Output:

```txt
 fibonacci:  1 1 2 3 5 8 13 21 34 55 89 144
tribonacci:  1 1 2 4 7 13 24 44 81 149 274 504
tetranacci:  1 1 2 4 8 15 29 56 108 208 401 773
pentanacci:  1 1 2 4 8 16 31 61 120 236 464 912
 hexanacci:  1 1 2 4 8 16 32 63 125 248 492 976
heptanacci:  1 1 2 4 8 16 32 64 127 253 504 1004
 octonacci:  1 1 2 4 8 16 32 64 128 255 509 1016
 nonanacci:  1 1 2 4 8 16 32 64 128 256 511 1021
 decanacci:  1 1 2 4 8 16 32 64 128 256 512 1023
     lucas:  2 1 3 4 7 11 18 29 47 76 123 199
```



## C


```c
/*
The function anynacci determines the n-arity of the sequence from the number of seed elements. 0 ended arrays are used since C does not have a way of determining the length of dynamic and function-passed integer arrays.*/

#include<stdlib.h>
#include<stdio.h>

int *
anynacci (int *seedArray, int howMany)
{
  int *result = malloc (howMany * sizeof (int));
  int i, j, initialCardinality;

  for (i = 0; seedArray[i] != 0; i++);
  initialCardinality = i;

  for (i = 0; i < initialCardinality; i++)
    result[i] = seedArray[i];

  for (i = initialCardinality; i < howMany; i++)
    {
      result[i] = 0;
      for (j = i - initialCardinality; j < i; j++)
        result[i] += result[j];
    }
  return result;
}

int
main ()
{
  int fibo[] = { 1, 1, 0 }, tribo[] = { 1, 1, 2, 0 }, tetra[] = { 1, 1, 2, 4, 0 }, luca[] = { 2, 1, 0 };
  int *fibonacci = anynacci (fibo, 10), *tribonacci = anynacci (tribo, 10), *tetranacci = anynacci (tetra, 10),
      *lucas = anynacci(luca, 10);
  int i;

  printf ("\nFibonacci\tTribonacci\tTetranacci\tLucas\n");

  for (i = 0; i < 10; i++)
    printf ("\n%d\t\t%d\t\t%d\t\t%d", fibonacci[i], tribonacci[i],
            tetranacci[i], lucas[i]);

  return 0;
}
```


Output:

```txt

Fibonacci       Tribonacci      Tetranacci      Lucas

1               1               1               2
1               1               1               1
2               2               2               3
3               4               4               4
5               7               8               7
8               13              15              11
13              24              29              18
21              44              56              29
34              81              108             47
55              149             208             76

```



## C++


```cpp
#include <vector>
#include <iostream>
#include <numeric>
#include <iterator>
#include <memory>
#include <string>
#include <algorithm>
#include <iomanip>

std::vector<int> nacci ( const std::vector<int> & start , int arity ) {
   std::vector<int> result ( start ) ;
   int sumstart = 1 ;//summing starts at vector's begin + sumstart as
                     //soon as the vector is longer than arity
   while ( result.size( ) < 15 ) { //we print out the first 15 numbers
      if ( result.size( ) <= arity )
	 result.push_back( std::accumulate( result.begin( ) ,
		  result.begin( ) + result.size( ) , 0 ) ) ;
      else {
	 result.push_back( std::accumulate ( result.begin( ) +
	  sumstart , result.begin( ) + sumstart + arity  , 0 )) ;
	 sumstart++ ;
      }
   }
   return std::move ( result ) ;
}

int main( ) {
   std::vector<std::string> naccinames {"fibo" , "tribo" ,
      "tetra" , "penta" , "hexa" , "hepta" , "octo" , "nona" , "deca" } ;
   const std::vector<int> fibo { 1 , 1 } , lucas { 2 , 1 } ;
   for ( int i = 2 ; i < 11 ; i++ ) {
      std::vector<int> numberrow = nacci ( fibo , i ) ;
      std::cout << std::left << std::setw( 10 ) <<
	 naccinames[ i - 2 ].append( "nacci" ) <<
	 std::setw( 2 ) << " : " ;
      std::copy ( numberrow.begin( ) , numberrow.end( ) ,
	    std::ostream_iterator<int>( std::cout , " " ) ) ;
      std::cout << "...\n" ;
      numberrow = nacci ( lucas , i ) ;
      std::cout << "Lucas-" << i ;
      if ( i < 10 )               //for formatting purposes
	 std::cout << "    : " ;
      else
	 std::cout << "   : " ;
      std::copy ( numberrow.begin( ) , numberrow.end( ) ,
	    std::ostream_iterator<int>( std::cout , " " ) ) ;
      std::cout << "...\n" ;
   }
   return 0 ;
}
```

Output:

```txt
fibonacci  : 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 ...
Lucas-2    : 2 1 3 4 7 11 18 29 47 76 123 199 322 521 843 ...
tribonacci : 1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136 ...
Lucas-3    : 2 1 3 6 10 19 35 64 118 217 399 734 1350 2483 4567 ...
tetranacci : 1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536 ...
Lucas-4    : 2 1 3 6 12 22 43 83 160 308 594 1145 2207 4254 8200 ...
pentanacci : 1 1 2 4 8 16 31 61 120 236 464 912 1793 3525 6930 ...
Lucas-5    : 2 1 3 6 12 24 46 91 179 352 692 1360 2674 5257 10335 ...
hexanacci  : 1 1 2 4 8 16 32 63 125 248 492 976 1936 3840 7617 ...
Lucas-6    : 2 1 3 6 12 24 48 94 187 371 736 1460 2896 5744 11394 ...
heptanacci : 1 1 2 4 8 16 32 64 127 253 504 1004 2000 3984 7936 ...
Lucas-7    : 2 1 3 6 12 24 48 96 190 379 755 1504 2996 5968 11888 ...
octonacci  : 1 1 2 4 8 16 32 64 128 255 509 1016 2028 4048 8080 ...
Lucas-8    : 2 1 3 6 12 24 48 96 192 382 763 1523 3040 6068 12112 ...
nonanacci  : 1 1 2 4 8 16 32 64 128 256 511 1021 2040 4076 8144 ...
Lucas-9    : 2 1 3 6 12 24 48 96 192 384 766 1531 3059 6112 12212 ...
decanacci  : 1 1 2 4 8 16 32 64 128 256 512 1023 2045 4088 8172 ...
Lucas-10   : 2 1 3 6 12 24 48 96 192 384 768 1534 3067 6131 12256 ...

```



### Alternate Version

This version focuses on a clean, simple class that adapts to any pair of starting numbers and any order.  Rather than summing over all history every time, it uses an O(1) incremental update to a running total.  Thus, performance remains essentially unchanged even for very large orders.


```cpp

#include <iostream>
#include <vector>

// This class forms a simple 'generator', where operator() returns the next
// element in the series.  It uses a small sliding window buffer to minimize
// storage overhead.
class nacci_t
{
    std::vector< int >  history;
    unsigned            windex;             // sliding window index
    unsigned            rindex;             // result index
    int                 running_sum;        // sum of values in sliding window

  public:

    nacci_t( unsigned int order, int a0 = 1, int a1 = 1 )
    :   history( order + 1 ), windex( 0 ), rindex( order - 1 ),
        running_sum( a0 + a1 )
    {
        // intialize sliding window
        history[order - 1] = a0;
        history[order - 0] = a1;
    }

    int operator()()
    {
        int result   = history[ rindex ];   // get 'nacci number to return
        running_sum -= history[ windex ];   // old 'nacci falls out of window

        history[ windex ] = running_sum;    // new 'nacci enters the window
        running_sum      += running_sum;    // new 'nacci added to the sum

        if ( ++windex == history.size() ) windex = 0;
        if ( ++rindex == history.size() ) rindex = 0;

        return result;
    }
};

int main()
{
    for ( unsigned int i = 2; i <= 10; ++i )
    {
        nacci_t nacci( i ); // fibonacci sequence

        std::cout << "nacci( " << i << " ): ";

        for ( int j = 0; j < 10; ++j )
            std::cout << " " << nacci();

        std::cout << std::endl;
    }

    for ( unsigned int i = 2; i <= 10; ++i )
    {
        nacci_t lucas( i, 2, 1 ); // Lucas sequence

        std::cout << "lucas( " << i << " ): ";

        for ( int j = 0; j < 10; ++j )
            std::cout << " " << lucas();

        std::cout << std::endl;
    }
}

```



## C sharp


```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Fibonacci
{
    class Program
    {
        static void Main(string[] args)
        {
            PrintNumberSequence("Fibonacci", GetNnacciNumbers(2, 10));
            PrintNumberSequence("Lucas", GetLucasNumbers(10));
            PrintNumberSequence("Tribonacci", GetNnacciNumbers(3, 10));
            PrintNumberSequence("Tetranacci", GetNnacciNumbers(4, 10));
            Console.ReadKey();
        }

        private static IList<ulong> GetLucasNumbers(int length)
        {
            IList<ulong> seedSequence = new List<ulong>() { 2, 1 };
            return GetFibLikeSequence(seedSequence, length);
        }

        private static IList<ulong> GetNnacciNumbers(int seedLength, int length)
        {
            return GetFibLikeSequence(GetNacciSeed(seedLength), length);
        }

        private static IList<ulong> GetNacciSeed(int seedLength)
        {
            IList<ulong> seedSquence = new List<ulong>() { 1 };

            for (uint i = 0; i < seedLength - 1; i++)
            {
                seedSquence.Add((ulong)Math.Pow(2, i));
            }

            return seedSquence;
        }

        private static IList<ulong> GetFibLikeSequence(IList<ulong> seedSequence, int length)
        {
            IList<ulong> sequence = new List<ulong>();

            int count = seedSequence.Count();

            if (length <= count)
            {
                sequence = seedSequence.Take((int)length).ToList();
            }
            else
            {
                sequence = seedSequence;

                for (int i = count; i < length; i++)
                {
                    ulong num = 0;

                    for (int j = 0; j < count; j++)
                    {
                        num += sequence[sequence.Count - 1 - j];
                    }

                    sequence.Add(num);
                }
            }

            return sequence;
        }

        private static void PrintNumberSequence(string Title, IList<ulong> numbersequence)
        {
            StringBuilder output = new StringBuilder(Title).Append("   ");

            foreach (long item in numbersequence)
            {
                output.AppendFormat("{0}, ", item);
            }

            Console.WriteLine(output.ToString());
        }
    }
}
```


```txt
Fibonacci   1, 1, 2, 3, 5, 8, 13, 21, 34, 55,
Lucas   2, 1, 3, 4, 7, 11, 18, 29, 47, 76,
Tribonacci   1, 1, 2, 4, 7, 13, 24, 44, 81, 149,
Tetranacci   1, 1, 2, 4, 8, 15, 29, 56, 108, 208,
```



## Clojure


```clojure
(defn nacci [init]
  (letfn [(s [] (lazy-cat init (apply map + (map #(drop % (s)) (range (count init))))))]
    (s)))

(let [show (fn [name init] (println "first 20" name (take 20 (nacci init))))]
  (show "Fibonacci" [1 1])
  (show "Tribonacci" [1 1 2])
  (show "Tetranacci" [1 1 2 4])
  (show "Lucas" [2 1]))
```

{{out}}

```txt
first 20 Fibonacci (1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765)
first 20 Tribonacci (1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136 5768 10609 19513 35890 66012)
first 20 Tetranacci (1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536 10671 20569 39648 76424 147312)
first 20 Lucas (2 1 3 4 7 11 18 29 47 76 123 199 322 521 843 1364 2207 3571 5778 9349)
```




## Common Lisp


```lisp

(defun gen-fib (lst m)
  "Return the first m members of a generalized Fibonacci sequence using lst as initial values
   and the length of lst as step."
  (let ((l (- (length lst) 1)))
       (do* ((fib-list (reverse lst) (cons (loop for i from 0 to l sum (nth i fib-list)) fib-list))
	     (c (+ l 2) (+ c 1)))
	    ((> c m) (reverse fib-list)))))

(defun initial-values (n)
  "Return the initial values of the Fibonacci n-step sequence"
  (cons 1
        (loop for i from 0 to (- n 2)
              collect (expt 2 i))))

(defun start ()
  (format t "Lucas series: ~a~%" (gen-fib '(2 1) 10))
  (loop for i from 2 to 4
        do (format t "Fibonacci ~a-step sequence: ~a~%" i (gen-fib (initial-values i) 10))))
```

{{out}}

```txt
Lucas series: (2 1 3 4 7 11 18 29 47 76)
Fibonacci 2-step sequence: (1 1 2 3 5 8 13 21 34 55)
Fibonacci 3-step sequence: (1 1 2 4 7 13 24 44 81 149)
Fibonacci 4-step sequence: (1 1 2 4 8 15 29 56 108 208)
```


## D


### Basic Memoization


```d
void main() {
    import std.stdio, std.algorithm, std.range, std.conv;

    const(int)[] memo;
    size_t addNum;

    void setHead(int[] head) nothrow @safe {
        memo = head;
        addNum = head.length;
    }

    int fibber(in size_t n) nothrow @safe {
        if (n >= memo.length)
            memo ~= iota(n - addNum, n).map!fibber.sum;
        return memo[n];
    }

    setHead([1, 1]);
    10.iota.map!fibber.writeln;
    setHead([2, 1]);
    10.iota.map!fibber.writeln;

    const prefixes = "fibo tribo tetra penta hexa hepta octo nona deca";
    foreach (immutable n, const name; prefixes.split.enumerate(2)) {
        setHead(1 ~ iota(n - 1).map!q{2 ^^ a}.array);
        writefln("n=%2d, %5snacci -> %(%d %) ...", n, name,
                 15.iota.map!fibber);
    }
}
```

{{out}}

```txt
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
[2, 1, 3, 4, 7, 11, 18, 29, 47, 76]
n= 2,  fibonacci -> 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 ...
n= 3, tribonacci -> 1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136 ...
n= 4, tetranacci -> 1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536 ...
n= 5, pentanacci -> 1 1 2 4 8 16 31 61 120 236 464 912 1793 3525 6930 ...
n= 6,  hexanacci -> 1 1 2 4 8 16 32 63 125 248 492 976 1936 3840 7617 ...
n= 7, heptanacci -> 1 1 2 4 8 16 32 64 127 253 504 1004 2000 3984 7936 ...
n= 8,  octonacci -> 1 1 2 4 8 16 32 64 128 255 509 1016 2028 4048 8080 ...
n= 9,  nonanacci -> 1 1 2 4 8 16 32 64 128 256 511 1021 2040 4076 8144 ...
n=10,  decanacci -> 1 1 2 4 8 16 32 64 128 256 512 1023 2045 4088 8172 ...
```



### Callable Struct

The output is similar.

```d
import std.stdio, std.algorithm, std.range, std.conv;

struct fiblike(T) {
    const(T)[] memo;
    immutable size_t addNum;

    this(in T[] start) nothrow @safe {
        this.memo = start.dup;
        this.addNum = start.length;
    }

    T opCall(in size_t n) nothrow @safe {
        if (n >= memo.length)
            memo ~= iota(n - addNum, n)
                    .map!(i => opCall(i))
                    .sum
                    .to!int;
        return memo[n];
    }
}

void main() {
    auto fibo = fiblike!int([1, 1]);
    iota(10).map!fibo.writeln;

    auto lucas = fiblike!int([2, 1]);
    iota(10).map!lucas.writeln;

    const prefixes = "fibo tribo tetra penta hexa hepta octo nona deca";
    foreach (immutable n, const name; prefixes.split.enumerate(2)) {
        auto fib = fiblike!int(1 ~ iota(n - 1).map!q{2 ^^ a}.array);
        writefln("n=%2d, %5snacci -> %(%d %) ...",
                 n, name, 15.iota.map!fib);
    }
}
```



### Struct With opApply

The output is similar.

```d
import std.stdio, std.algorithm, std.range, std.traits;

struct Fiblike(T) {
    T[] tail;

    int opApply(int delegate(immutable ref T) dg) {
        int result, pos;
        foreach (immutable x; tail) {
            result = dg(x);
            if (result)
                return result;
        }
        foreach (immutable i; tail.length.iota.cycle) {
            immutable x = tail.sum;
            result = dg(x);
            if (result)
                break;
            tail[i] = x;
        }
        return result;
    }
}

// std.range.take doesn't work with opApply.
ForeachType!It[] takeApply(It)(It iterable, in size_t n) {
    typeof(return) result;
    foreach (immutable x; iterable) {
        result ~= x;
        if (result.length == n)
            break;
    }
    return result;
}

void main() {
    Fiblike!int([1, 1]).takeApply(10).writeln;
    Fiblike!int([2, 1]).takeApply(10).writeln;

    const prefixes = "fibo tribo tetra penta hexa hepta octo nona deca";
    foreach (immutable n, const name; prefixes.split.enumerate(2)) {
        auto fib = Fiblike!int(1 ~ iota(n - 1).map!q{2 ^^ a}.array);
        writefln("n=%2d, %5snacci -> %s", n, name, fib.takeApply(15));
    }
}
```



### Range Generator Version


```d
void main() {
    import std.stdio, std.algorithm, std.range, std.concurrency;

    immutable fibLike = (int[] tail) => new Generator!int({
        foreach (x; tail)
            yield(x);
        foreach (immutable i; tail.length.iota.cycle)
            yield(tail[i] = tail.sum);
    });

    foreach (seed; [[1, 1], [2, 1]])
        fibLike(seed).take(10).writeln;

    immutable prefixes = "fibo tribo tetra penta hexa hepta octo nona deca";
    foreach (immutable n, const name; prefixes.split.enumerate(2)) {
        auto fib = fibLike(1 ~ iota(n - 1).map!q{2 ^^ a}.array);
        writefln("n=%2d, %5snacci -> %(%s, %), ...", n, name, fib.take(15));
    }
}
```

{{out}}

```txt
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
[2, 1, 3, 4, 7, 11, 18, 29, 47, 76]
n= 2,  fibonacci -> 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, ...
n= 3, tribonacci -> 1, 1, 2, 4, 7, 13, 24, 44, 81, 149, 274, 504, 927, 1705, 3136, ...
n= 4, tetranacci -> 1, 1, 2, 4, 8, 15, 29, 56, 108, 208, 401, 773, 1490, 2872, 5536, ...
n= 5, pentanacci -> 1, 1, 2, 4, 8, 16, 31, 61, 120, 236, 464, 912, 1793, 3525, 6930, ...
n= 6,  hexanacci -> 1, 1, 2, 4, 8, 16, 32, 63, 125, 248, 492, 976, 1936, 3840, 7617, ...
n= 7, heptanacci -> 1, 1, 2, 4, 8, 16, 32, 64, 127, 253, 504, 1004, 2000, 3984, 7936, ...
n= 8,  octonacci -> 1, 1, 2, 4, 8, 16, 32, 64, 128, 255, 509, 1016, 2028, 4048, 8080, ...
n= 9,  nonanacci -> 1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 511, 1021, 2040, 4076, 8144, ...
n=10,  decanacci -> 1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1023, 2045, 4088, 8172, ...
```



## EchoLisp


```scheme

;; generate a recursive lambda() for a x-nacci
;; equip it with memoïzation
;; bind it to its name
(define (make-nacci name seed)
		(define len (1+ (vector-length seed)))
		(define-global name
			`(lambda(n) (for/sum ((i (in-range (1- n) (- n ,len) -1)))  (,name i))))
		(remember name seed)
		name)

(define nacci-family `(
	(Fibonacci #(1 1))
	(Tribonacci #(1 1 2))
	(Tetranacci #(1 1 2 4))
	(Decanacci #(1 1 2 4 8 16 32 64 128 256))
	(Random-😜-nacci ,(list->vector (take 6 (shuffle (iota 100)))))
	(Lucas #(2 1))))

(define (task naccis)
	(for ((nacci naccis))
		(define-values (name seed) nacci)
		(make-nacci name seed)
		(printf "%s[%d]  → %d" name (vector-length seed) (take name 16))))

```


{{out}}

```txt

(task nacci-family )

Fibonacci[2] → (1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987)
Tribonacci[3] → (1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136 5768)
Tetranacci[4] → (1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536 10671)
Decanacci[10] → (1 1 2 4 8 16 32 64 128 256 512 1023 2045 4088 8172 16336)
Random-😜-nacci[6] → (95 52 16 48 59 56 326 557 1062 2108 4168 8277 16498 32670 64783 128504)
Lucas[2] → (2 1 3 4 7 11 18 29 47 76 123 199 322 521 843 1364)

```



## Elixir

{{trans|Ruby}}

```elixir
defmodule RC do
  def anynacci(start_sequence, count) do
    n = length(start_sequence)
    anynacci(Enum.reverse(start_sequence), count-n, n)
  end

  def anynacci(seq, 0, _), do: Enum.reverse(seq)
  def anynacci(seq, count, n) do
    next = Enum.sum(Enum.take(seq, n))
    anynacci([next|seq], count-1, n)
  end
end

IO.inspect RC.anynacci([1,1], 15)

naccis = [ lucus:      [2,1],
           fibonacci:  [1,1],
           tribonacci: [1,1,2],
           tetranacci: [1,1,2,4],
           pentanacci: [1,1,2,4,8],
           hexanacci:  [1,1,2,4,8,16],
           heptanacci: [1,1,2,4,8,16,32],
           octonacci:  [1,1,2,4,8,16,32,64],
           nonanacci:  [1,1,2,4,8,16,32,64,128],
           decanacci:  [1,1,2,4,8,16,32,64,128,256] ]
Enum.each(naccis, fn {name, list} ->
  :io.format("~11s: ", [name])
  IO.inspect RC.anynacci(list, 15)
end)
```


{{out}}

```txt

      lucus: [2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123, 199, 322, 521, 843]
  fibonacci: [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610]
 tribonacci: [1, 1, 2, 4, 7, 13, 24, 44, 81, 149, 274, 504, 927, 1705, 3136]
 tetranacci: [1, 1, 2, 4, 8, 15, 29, 56, 108, 208, 401, 773, 1490, 2872, 5536]
 pentanacci: [1, 1, 2, 4, 8, 16, 31, 61, 120, 236, 464, 912, 1793, 3525, 6930]
  hexanacci: [1, 1, 2, 4, 8, 16, 32, 63, 125, 248, 492, 976, 1936, 3840, 7617]
 heptanacci: [1, 1, 2, 4, 8, 16, 32, 64, 127, 253, 504, 1004, 2000, 3984, 7936]
  octonacci: [1, 1, 2, 4, 8, 16, 32, 64, 128, 255, 509, 1016, 2028, 4048, 8080]
  nonanacci: [1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 511, 1021, 2040, 4076, 8144]
  decanacci: [1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1023, 2045, 4088, 8172]

```



## Erlang

<lang>
-module( fibonacci_nstep ).

-export( [nacci/2, task/0] ).

nacci( N, Ns ) when N =< erlang:length(Ns) ->
	{Sequence, _Not_sequence} = lists:split( N, Ns ),
	Sequence;
nacci( N, Ns ) ->
	Nth = erlang:length( Ns ),
	{_Nth, Sequence_reversed} = lists:foldl( fun nacci_foldl/2, {Nth, lists:reverse(Ns)}, lists:seq(Nth+1, N) ),
	lists:reverse( Sequence_reversed ).

task() ->
	Names_and_funs = [{X, fun (N) -> nacci( N, Y ) end} || {X, Y} <- [{fibonacci, [1, 1]}, {tribonacci, [1, 1, 2]}, {tetranacci, [1, 1, 2, 4]}, {lukas, [2, 1]}]],
	[io:fwrite( "~p: ~p~n", [X, Y(10)] ) || {X, Y} <- Names_and_funs].



nacci_foldl( _N, {Nth, Ns} ) ->
	{Sum_ns, _Not_sum_ns} = lists:split( Nth, Ns ),
	{Nth, [lists:sum(Sum_ns) | Ns]}.

```

{{out}}

```txt

59> fibonacci_nstep:task().
fibonacci: [1,1,2,3,5,8,13,21,34,55]
tribonacci: [1,1,2,4,7,13,24,44,81,149]
tetranacci: [1,1,2,4,8,15,29,56,108,208]
lukas: [2,1,3,4,7,11,18,29,47,76]

```



## ERRE


```ERRE

PROGRAM FIBON

!
! for rosettacode.org
!

DIM F[20]

PROCEDURE FIB(TIPO$,F$)
 FOR I%=0 TO 20 DO
   F[I%]=0
 END FOR
 B=0
 LOOP
  Q=INSTR(F$,",")
  B=B+1
  IF Q=0 THEN
      F[B]=VAL(F$)
      EXIT
    ELSE
      F[B]=VAL(MID$(F$,1,Q-1))
      F$=MID$(F$,Q+1)
  END IF
 END LOOP

 PRINT(TIPO$;" =>";)
 FOR I=B TO 14+B DO
    IF I<>B THEN PRINT(",";) END IF
    PRINT(F[I-B+1];)
    FOR J=(I-B)+1 TO I DO
        F[I+1]=F[I+1]+F[J]
    END FOR
 END FOR
 PRINT
END PROCEDURE

BEGIN
   PRINT(CHR$(12);) ! CLS
   FIB("Fibonacci","1,1")
   FIB("Tribonacci","1,1,2")
   FIB("Tetranacci","1,1,2,4")
   FIB("Lucas","2,1")
END PROGRAM

```


=={{header|F_Sharp|F#}}==

```fsharp
let fibinit = Seq.append (Seq.singleton 1) (Seq.unfold (fun n -> Some(n, 2*n)) 1)

let fiblike init =
    Seq.append
        (Seq.ofList init)
        (Seq.unfold
            (function   | least :: rest ->
                            let this = least + Seq.reduce (+) rest
                            Some(this, rest @ [this])
                        | _ -> None) init)

let lucas = fiblike [2; 1]

let nacci n = Seq.take n fibinit |> Seq.toList |> fiblike

[<EntryPoint>]
let main argv =
    let start s = Seq.take 15 s |> Seq.toList
    let prefix = "fibo tribo tetra penta hexa hepta octo nona deca".Split()
    Seq.iter
        (fun (p, n) -> printfn "n=%2i, %5snacci -> %A" n p (start (nacci n)))
        (Seq.init prefix.Length (fun i -> (prefix.[i], i+2)))
    printfn "      lucas      -> %A" (start (fiblike [2; 1]))
    0
```

Output

```txt
n= 2,  fibonacci -> [1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377; 610]
n= 3, tribonacci -> [1; 1; 2; 4; 7; 13; 24; 44; 81; 149; 274; 504; 927; 1705; 3136]
n= 4, tetranacci -> [1; 1; 2; 4; 8; 15; 29; 56; 108; 208; 401; 773; 1490; 2872; 5536]
n= 5, pentanacci -> [1; 1; 2; 4; 8; 16; 31; 61; 120; 236; 464; 912; 1793; 3525; 6930]
n= 6,  hexanacci -> [1; 1; 2; 4; 8; 16; 32; 63; 125; 248; 492; 976; 1936; 3840; 7617]
n= 7, heptanacci -> [1; 1; 2; 4; 8; 16; 32; 64; 127; 253; 504; 1004; 2000; 3984; 7936]
n= 8,  octonacci -> [1; 1; 2; 4; 8; 16; 32; 64; 128; 255; 509; 1016; 2028; 4048; 8080]
n= 9,  nonanacci -> [1; 1; 2; 4; 8; 16; 32; 64; 128; 256; 511; 1021; 2040; 4076; 8144]
n=10,  decanacci -> [1; 1; 2; 4; 8; 16; 32; 64; 128; 256; 512; 1023; 2045; 4088; 8172]
      lucas      -> [2; 1; 3; 4; 7; 11; 18; 29; 47; 76; 123; 199; 322; 521; 843]
```



## Factor

<code>building</code> is a dynamic variable that refers to the sequence being built by <code>make</code>. This is useful when the next element of the sequence depends on previous elements.

```factor
USING: formatting fry kernel make math namespaces qw sequences ;

: n-bonacci ( n initial -- seq ) [
        [ [ , ] each ] [ length - ] [ length ] tri
        '[ building get _ tail* sum , ] times
    ] { } make ;

qw{ fibonacci tribonacci tetranacci lucas }
{ { 1 1 } { 1 1 2 } { 1 1 2 4 } { 2 1 } }
[ 10 swap n-bonacci "%-10s %[%3d, %]\n" printf ] 2each
```

{{out}}

```txt

fibonacci  {   1,   1,   2,   3,   5,   8,  13,  21,  34,  55 }
tribonacci {   1,   1,   2,   4,   7,  13,  24,  44,  81, 149 }
tetranacci {   1,   1,   2,   4,   8,  15,  29,  56, 108, 208 }
lucas      {   2,   1,   3,   4,   7,  11,  18,  29,  47,  76 }

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Fibonacci_n-step_number_sequences this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth
: length @ ;                          \ length of an array is stored at its address
: a{ here cell allot ;
: } , here over - cell / over ! ;

defer nacci

: step ( a- i n -- a- i m )
    >r 1- 2dup nacci r> + ;

: steps ( a- i n -- m )
    0 tuck do step loop nip nip ;

:noname ( a- i -- n )
    over length over >                \ if i is within the array
    if cells + @                      \ fetch i...if not,
    else over length 1- steps         \ get length of array for calling step and recurse
    then ; is nacci

: show-nacci 11 1 do dup i nacci . loop cr drop ;

." fibonacci: " a{ 1 , 1 } show-nacci
." tribonacci: " a{ 1 , 1 , 2 } show-nacci
." tetranacci: " a{ 1 , 1 , 2 , 4 } show-nacci
." lucas: " a{ 2 , 1 } show-nacci

```

{{out}}

```txt
fibonacci: 1 1 2 3 5 8 13 21 34 55
tribonacci: 1 1 2 4 7 13 24 44 81 149
tetranacci: 1 1 2 4 8 15 29 56 108 208
lucas: 2 1 3 4 7 11 18 29 47 76

```



## Fortran


```fortran

! save this program as file f.f08
! gnu-linux command to  build and test
! $ a=./f && gfortran -Wall -std=f2008 $a.f08 -o $a && echo -e 2\\n5\\n\\n | $a

! -*- mode: compilation; default-directory: "/tmp/" -*-
! Compilation started at Fri Apr  4 23:20:27
!
! a=./f && gfortran -Wall -std=f2008 $a.f08 -o $a && echo -e 2\\n8\\ny\\n | $a
! Enter the number of terms to sum: Show the the first how many terms of the sequence?   Accept this initial sequence (y/n)?
!            1           1
!            1           1           2           3           5           8          13          21
!
! Compilation finished at Fri Apr  4 23:20:27

program f
  implicit none
  integer :: n, terms
  integer, allocatable, dimension(:) :: sequence
  integer :: i
  character :: answer
  write(6,'(a)',advance='no')'Enter the number of terms to sum: '
  read(5,*) n
  if ((n < 2) .or. (29 < n)) stop'Unreasonable!  Exit.'
  write(6,'(a)',advance='no')'Show the the first how many terms of the sequence?  '
  read(5,*) terms
  if (terms < 1) stop'Lazy programmer has not implemented backward sequences.'
  n = min(n, terms)
  allocate(sequence(1:terms))
  sequence(1) = 1
  do i = 0, n - 2
     sequence(i+2) = 2**i
  end do
  write(6,*)'Accept this initial sequence (y/n)?'
  write(6,*) sequence(:n)
  read(5,*) answer
  if (answer .eq. 'n') then
     write(6,*) 'Fine.  Enter the initial terms.'
     do i=1, n
        write(6, '(i2,a2)', advance = 'no') i, ': '
        read(5, *) sequence(i)
     end do
  end if
  call nacci(n, sequence)
  write(6,*) sequence(:terms)
  deallocate(sequence)

contains

    subroutine nacci(n, s)
      ! nacci =:  (] , +/@{.)^:(-@#@]`(-#)`])
      integer, intent(in) :: n
      integer, intent(inout), dimension(:) :: s
      integer :: i, terms
      terms = size(s)
!      do i = n+1, terms
 !        s(i) = sum(s(i-n:i-1))
  !    end do
      i = n+1
      if (n+1 .le. terms) s(i) = sum(s(i-n:i-1))
      do i = n + 2, terms
         s(i) = 2*s(i-1) - s(i-(n+1))
      end do
    end subroutine nacci
end program f

```



```txt

$ ./f  # Lucas series
Enter the number of terms to sum: 2
Show the the first how many terms of the sequence?  10
 Accept this initial sequence (y/n)?
           1           1
n
 Fine.  Enter the initial terms.
 1: 2
 2: 1
           2           1           3           4           7          11          18          29          47          76
$
$
$
$
$
$
$ ./f # Waltzing the 6-step
Enter the number of terms to sum: 6
Show the the first how many terms of the sequence?  10
 Accept this initial sequence (y/n)?
           1           1           2           4           8          16
y
           1           1           2           4           8          16          32          63         125         248
$

```


```txt

$ for n in 2 3 4;do echo -e $n\\n10\\ny|./f;done
Enter the number of terms to sum: Show the the first how many terms of the sequence?   Accept this initial sequence (y/n)?
           1           1
           1           1           2           3           5           8          13          21          34          55
Enter the number of terms to sum: Show the the first how many terms of the sequence?   Accept this initial sequence (y/n)?
           1           1           2
           1           1           2           4           7          13          24          44          81         149
Enter the number of terms to sum: Show the the first how many terms of the sequence?   Accept this initial sequence (y/n)?
           1           1           2           4
           1           1           2           4           8          15          29          56         108         208

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' Deduces the step, n, from the length of the dynamic array passed in
' and fills it out to 'size' elements
Sub fibN (a() As Integer, size As Integer)
  Dim lb As Integer = LBound(a)
  Dim ub As Integer = UBound(a)
  Dim length As Integer = ub - lb + 1
  If length < 2 OrElse length >= size Then Return
  ub = lb + size - 1
  Redim Preserve a(lb To ub)
  Dim sum As Integer
  For i As Integer = lb + length to ub
    sum = 0
    For j As Integer = 1 To Length
      sum += a(i - j)
    Next j
    a(i) = sum
  Next i
End Sub

Sub printSeries(a() As Integer, name_ As String) '' name is a keyword
  Print name_; " =>";
  For i As Integer = LBound(a) To UBound(a)
    Print Using "####"; a(i);
    Print " ";
  Next
  Print
End Sub

Const size As Integer = 13 '' say
Redim a(1 To 2) As Integer
a(1) = 1 : a(2) = 1
fibN(a(), size)
printSeries(a(), "fibonacci ")
Redim Preserve a(1 To 3)
a(3) = 2
fibN(a(), size)
printSeries(a(), "tribonacci")
Redim Preserve a(1 To 4)
a(4) = 4
fibN(a(), size)
printSeries(a(), "tetranacci")
erase a
Redim a(1 To 2)
a(1) = 2 : a(2) = 1
fibN(a(), size)
printSeries(a(), "lucas     ")
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

fibonacci  =>   1    1    2    3    5    8   13   21   34   55   89  144  233
tribonacci =>   1    1    2    4    7   13   24   44   81  149  274  504  927
tetranacci =>   1    1    2    4    8   15   29   56  108  208  401  773 1490
lucas      =>   2    1    3    4    7   11   18   29   47   76  123  199  322

```



## FunL


```funl
import util.TextTable
native scala.collection.mutable.Queue

def fibLike( init ) =
  q = Queue()

  for i <- init do q.enqueue( i )

  def fib =
    q.enqueue( sum(q) )
    q.dequeue() # fib()

  0 # fib()

def fibN( n ) = fibLike( [1] + [2^i | i <- 0:n-1] )

val lucas = fibLike( [2, 1] )

t = TextTable()
t.header( 'k', 'Fibonacci', 'Tribonacci', 'Tetranacci', 'Lucas' )
t.line()

for i <- 1..5
  t.rightAlignment( i )

seqs = (fibN(2), fibN(3), fibN(4), lucas)

for k <- 1..10
  t.row( ([k] + [seqs(i)(k) | i <- 0:4]).toIndexedSeq() )

print( t )
```


{{out}}


```txt

+----+-----------+------------+------------+-------+
| k  | Fibonacci | Tribonacci | Tetranacci | Lucas |
+----+-----------+------------+------------+-------+
|  1 |         1 |          1 |          1 |     2 |
|  2 |         1 |          1 |          1 |     1 |
|  3 |         2 |          2 |          2 |     3 |
|  4 |         3 |          4 |          4 |     4 |
|  5 |         5 |          7 |          8 |     7 |
|  6 |         8 |         13 |         15 |    11 |
|  7 |        13 |         24 |         29 |    18 |
|  8 |        21 |         44 |         56 |    29 |
|  9 |        34 |         81 |        108 |    47 |
| 10 |        55 |        149 |        208 |    76 |
+----+-----------+------------+------------+-------+

```



## Go

Solution using separate goroutines.

```Go
package main

import "fmt"

func g(i []int, c chan<- int) {
	var sum int
	b := append([]int(nil), i...) // make a copy
	for _, t := range b {
		c <- t
		sum += t
	}
	for {
		for j, t := range b {
			c <- sum
			b[j], sum = sum, sum+sum-t
		}
	}
}

func main() {
	for _, s := range [...]struct {
		seq string
		i   []int
	}{
		{"Fibonacci", []int{1, 1}},
		{"Tribonacci", []int{1, 1, 2}},
		{"Tetranacci", []int{1, 1, 2, 4}},
		{"Lucas", []int{2, 1}},
	} {
		fmt.Printf("%10s:", s.seq)
		c := make(chan int)
		// Note/warning: these goroutines are leaked.
		go g(s.i, c)
		for j := 0; j < 10; j++ {
			fmt.Print(" ", <-c)
		}
		fmt.Println()
	}
}
```

{{out}}

```txt

 Fibonacci: 1 1 2 3 5 8 13 21 34 55
Tribonacci: 1 1 2 4 7 13 24 44 81 149
Tetranacci: 1 1 2 4 8 15 29 56 108 208
     Lucas: 2 1 3 4 7 11 18 29 47 76

```



## Groovy


### ==Solution==


```groovy
def fib = { List seed, int k=10 ->
    assert seed : "The seed list must be non-null and non-empty"
    assert seed.every { it instanceof Number } : "Every member of the seed must be a number"
    def n = seed.size()
    assert n > 1 : "The seed must contain at least two elements"
    List result = [] + seed
    if (k < n) {
        result[0..k]
    } else {
        (n..k).inject(result) { res, kk ->
            res << res[-n..-1].sum()
        }
    }
}
```


### ==Test==


```groovy
[
    ' fibonacci':[1,1],
    'tribonacci':[1,1,2],
    'tetranacci':[1,1,2,4],
    'pentanacci':[1,1,2,4,8],
    ' hexanacci':[1,1,2,4,8,16],
    'heptanacci':[1,1,2,4,8,16,32],
    ' octonacci':[1,1,2,4,8,16,32,64],
    ' nonanacci':[1,1,2,4,8,16,32,64,128],
    ' decanacci':[1,1,2,4,8,16,32,64,128,256],
    '     lucas':[2,1],
].each { name, seed ->
    println "${name}: ${fib(seed,10)}"
}

println "  lucas[0]: ${fib([2,1],0)}"
println "  tetra[3]: ${fib([1,1,2,4],3)}"
```

{{out}}

```txt
 fibonacci: [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
tribonacci: [1, 1, 2, 4, 7, 13, 24, 44, 81, 149, 274]
tetranacci: [1, 1, 2, 4, 8, 15, 29, 56, 108, 208, 401]
pentanacci: [1, 1, 2, 4, 8, 16, 31, 61, 120, 236, 464]
 hexanacci: [1, 1, 2, 4, 8, 16, 32, 63, 125, 248, 492]
heptanacci: [1, 1, 2, 4, 8, 16, 32, 64, 127, 253, 504]
 octonacci: [1, 1, 2, 4, 8, 16, 32, 64, 128, 255, 509]
 nonanacci: [1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 511]
 decanacci: [1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512]
     lucas: [2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123]
  lucas[0]: [2]
  tetra[3]: [1, 1, 2, 4]
```



## Haskell


```haskell
import Data.List (tails)
import Control.Monad (zipWithM_)

fiblike :: [Integer] -> [Integer]
fiblike st = xs where
  xs = st ++ map (sum . take n) (tails xs)
  n = length st

nstep :: Int -> [Integer]
nstep n = fiblike $ take n $ 1 : iterate (2*) 1

main :: IO ()
main = do
  print $ take 10 $ fiblike [1,1]
  print $ take 10 $ fiblike [2,1]
  zipWithM_ (\n name -> do putStr (name ++ "nacci -> ")
                           print $ take 15 $ nstep n)
    [2..] (words "fibo tribo tetra penta hexa hepta octo nona deca")
```

{{out}}

```txt

[1,1,2,3,5,8,13,21,34,55]
[2,1,3,4,7,11,18,29,47,76]
fibonacci -> [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]
tribonacci -> [1,1,2,4,7,13,24,44,81,149,274,504,927,1705,3136]
tetranacci -> [1,1,2,4,8,15,29,56,108,208,401,773,1490,2872,5536]
pentanacci -> [1,1,2,4,8,16,31,61,120,236,464,912,1793,3525,6930]
hexanacci -> [1,1,2,4,8,16,32,63,125,248,492,976,1936,3840,7617]
heptanacci -> [1,1,2,4,8,16,32,64,127,253,504,1004,2000,3984,7936]
octonacci -> [1,1,2,4,8,16,32,64,128,255,509,1016,2028,4048,8080]
nonanacci -> [1,1,2,4,8,16,32,64,128,256,511,1021,2040,4076,8144]
decanacci -> [1,1,2,4,8,16,32,64,128,256,512,1023,2045,4088,8172]

```


Or alternatively, without imports – using only the default Prelude:

```haskell
nFibs :: [Int] -> [Int]
nFibs [] = []
nFibs xs =
  let go ys@(z:zs) = z : go (zs ++ [sum ys])
  in go xs

fibInit :: Int -> [Int]
fibInit = (1 :) . fmap (2 ^) . enumFromTo 0 . subtract 2

-- TEST ---------------------------------------------------------------
main :: IO ()
main = do
  putStrLn $
    justifyLeft 12 ' ' "Lucas" ++ "-> " ++ show (take 15 (nFibs [2, 1]))
  (putStrLn . unlines)
    (zipWith
       (\s n ->
           justifyLeft 12 ' ' (s ++ "naccci") ++
           ("-> " ++ show (take 15 (nFibs (fibInit n)))))
       (words "fibo tribo tetra penta hexa hepta octo nona deca")
       [2 ..])

justifyLeft :: Int -> Char -> String -> String
justifyLeft n c s = take n (s ++ replicate n c)
```


```txt
Lucas       -> [2,1,3,4,7,11,18,29,47,76,123,199,322,521,843]
fibonaccci  -> [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]
tribonaccci -> [1,1,2,4,7,13,24,44,81,149,274,504,927,1705,3136]
tetranaccci -> [1,1,2,4,8,15,29,56,108,208,401,773,1490,2872,5536]
pentanaccci -> [1,1,2,4,8,16,31,61,120,236,464,912,1793,3525,6930]
hexanaccci  -> [1,1,2,4,8,16,32,63,125,248,492,976,1936,3840,7617]
heptanaccci -> [1,1,2,4,8,16,32,64,127,253,504,1004,2000,3984,7936]
octonaccci  -> [1,1,2,4,8,16,32,64,128,255,509,1016,2028,4048,8080]
nonanaccci  -> [1,1,2,4,8,16,32,64,128,256,511,1021,2040,4076,8144]
decanaccci  -> [1,1,2,4,8,16,32,64,128,256,512,1023,2045,4088,8172]
```


=={{header|Icon}} and {{header|Unicon}}==

Works in both languages:

```unicon
procedure main(A)
    every writes("F2:\t"|right((fnsGen(1,1))\14,5) | "\n")
    every writes("F3:\t"|right((fnsGen(1,1,2))\14,5) | "\n")
    every writes("F4:\t"|right((fnsGen(1,1,2,4))\14,5) | "\n")
    every writes("Lucas:\t"|right((fnsGen(2,1))\14,5) | "\n")
    every writes("F?:\t"|right((fnsGen!A)\14,5) | "\n")
end

procedure fnsGen(cache[])
    n := *cache
    every i := seq() do {
        if i > *cache then every (put(cache,0),cache[i] +:= cache[i-n to i-1])
        suspend cache[i]
        }
end
```


Output:

```txt

->fns 3 1 4 1 5
F2:         1    1    2    3    5    8   13   21   34   55   89  144  233  377
F3:         1    1    2    4    7   13   24   44   81  149  274  504  927 1705
F4:         1    1    2    4    8   15   29   56  108  208  401  773 1490 2872
Lucas:      2    1    3    4    7   11   18   29   47   76  123  199  322  521
F?:         3    1    4    1    5   14   25   49   94  187  369  724 1423 2797
->

```


A slightly longer version of <tt>fnsGen</tt> that reduces the memory
footprint is:

```unicon
procedure fnsGen(cache[])
    every i := seq() do {
        if i := (i > *cache, *cache) then {
             every (sum := 0) +:= !cache
             put(cache, sum)              # cache only 'just enough'
             pop(cache)
             }
        suspend cache[i]
        }
end
```


The output is identical.


## J


'''Solution''':
```j
   nacci     =:  (] , +/@{.)^:(-@#@]`(-#)`])
```

'''Example''' ''(Lucas)'':
```j
   10 nacci 2 1 NB.  Lucas series, first 10 terms
2 1 3 4 7 11 18 29 47 76
```

'''Example''' ''(extended 'nacci series)'':
```j
   TESTS     =:  }."1 fixdsv noun define  [   require 'tables/dsv'             NB.  Tests from task description
	 2 	fibonacci 	1 1 2 3 5  8 13 21  34  55  89  144  233  377  610 ...
	 3 	tribonacci	1 1 2 4 7 13 24 44  81 149 274  504  927 1705 3136 ...
	 4 	tetranacci	1 1 2 4 8 15 29 56 108 208 401  773 1490 2872 5536 ...
	 5 	pentanacci	1 1 2 4 8 16 31 61 120 236 464  912 1793 3525 6930 ...
	 6 	hexanacci 	1 1 2 4 8 16 32 63 125 248 492  976 1936 3840 7617 ...
	 7 	heptanacci	1 1 2 4 8 16 32 64 127 253 504 1004 2000 3984 7936 ...
	 8 	octonacci 	1 1 2 4 8 16 32 64 128 255 509 1016 2028 4048 8080 ...
	 9 	nonanacci 	1 1 2 4 8 16 32 64 128 256 511 1021 2040 4076 8144 ...
	10	decanacci 	1 1 2 4 8 16 32 64 128 256 512 1023 2045 4088 8172 ...
)
   testNacci =:  ] -: #@] nacci {.                                             NB. Given an order & test sequence, compare nacci to sequence
   OT        =:  __ ".&.> (<<<1) { |: TESTS                                    NB. 'nacci order and test sequence
   (> 1 {"1 TESTS) ,. ' ' ,. (u: 16b274c 16b2713) {~ (testNacci }:)&>/ OT      NB. ✓ or ❌ for success or failure
fibonacci  ✓
tribonacci ✓
tetranacci ✓
pentanacci ✓
hexanacci  ✓
heptanacci ✓
octonacci  ✓
nonanacci  ✓
decanacci  ✓
```



## Java


'''Code:'''


```java
class Fibonacci
{
  public static int[] lucas(int n, int numRequested)
  {
    if (n < 2)
      throw new IllegalArgumentException("Fibonacci value must be at least 2");
    return fibonacci((n == 2) ? new int[] { 2, 1 } : lucas(n - 1, n), numRequested);
  }

  public static int[] fibonacci(int n, int numRequested)
  {
    if (n < 2)
      throw new IllegalArgumentException("Fibonacci value must be at least 2");
    return fibonacci((n == 2) ? new int[] { 1, 1 } : fibonacci(n - 1, n), numRequested);
  }

  public static int[] fibonacci(int[] startingValues, int numRequested)
  {
    int[] output = new int[numRequested];
    int n = startingValues.length;
    System.arraycopy(startingValues, 0, output, 0, n);
    for (int i = n; i < numRequested; i++)
      for (int j = 1; j <= n; j++)
        output[i] += output[i - j];
    return output;
  }

  public static void main(String[] args)
  {
    for (int n = 2; n <= 10; n++)
    {
      System.out.print("nacci(" + n + "):");
      for (int value : fibonacci(n, 15))
        System.out.print(" " + value);
      System.out.println();
    }
    for (int n = 2; n <= 10; n++)
    {
      System.out.print("lucas(" + n + "):");
      for (int value : lucas(n, 15))
        System.out.print(" " + value);
      System.out.println();
    }
  }
}
```


Output:

```txt
nacci(2): 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610
nacci(3): 1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136
nacci(4): 1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536
nacci(5): 1 1 2 4 8 16 31 61 120 236 464 912 1793 3525 6930
nacci(6): 1 1 2 4 8 16 32 63 125 248 492 976 1936 3840 7617
nacci(7): 1 1 2 4 8 16 32 64 127 253 504 1004 2000 3984 7936
nacci(8): 1 1 2 4 8 16 32 64 128 255 509 1016 2028 4048 8080
nacci(9): 1 1 2 4 8 16 32 64 128 256 511 1021 2040 4076 8144
nacci(10): 1 1 2 4 8 16 32 64 128 256 512 1023 2045 4088 8172
lucas(2): 2 1 3 4 7 11 18 29 47 76 123 199 322 521 843
lucas(3): 2 1 3 6 10 19 35 64 118 217 399 734 1350 2483 4567
lucas(4): 2 1 3 6 12 22 43 83 160 308 594 1145 2207 4254 8200
lucas(5): 2 1 3 6 12 24 46 91 179 352 692 1360 2674 5257 10335
lucas(6): 2 1 3 6 12 24 48 94 187 371 736 1460 2896 5744 11394
lucas(7): 2 1 3 6 12 24 48 96 190 379 755 1504 2996 5968 11888
lucas(8): 2 1 3 6 12 24 48 96 192 382 763 1523 3040 6068 12112
lucas(9): 2 1 3 6 12 24 48 96 192 384 766 1531 3059 6112 12212
lucas(10): 2 1 3 6 12 24 48 96 192 384 768 1534 3067 6131 12256
```




## JavaScript


### ES5


```JavaScript
function fib(arity, len) {
    return nacci(nacci([1,1], arity, arity), arity, len);
}

function lucas(arity, len) {
    return nacci(nacci([2,1], arity, arity), arity, len);
}

function nacci(a, arity, len) {
    while (a.length < len) {
        var sum = 0;
        for (var i = Math.max(0, a.length - arity); i < a.length; i++)
            sum += a[i];
        a.push(sum);
    }
    return a;
}

function main() {
    for (var arity = 2; arity <= 10; arity++)
        console.log("fib(" + arity + "): " + fib(arity, 15));
    for (var arity = 2; arity <= 10; arity++)
        console.log("lucas(" + arity + "): " + lucas(arity, 15));
}

main();
```

{{out}}

```txt
fib(2): 1,1,2,3,5,8,13,21,34,55,89,144,233,377,610
fib(3): 1,1,2,4,7,13,24,44,81,149,274,504,927,1705,3136
fib(4): 1,1,2,4,8,15,29,56,108,208,401,773,1490,2872,5536
fib(5): 1,1,2,4,8,16,31,61,120,236,464,912,1793,3525,6930
fib(6): 1,1,2,4,8,16,32,63,125,248,492,976,1936,3840,7617
fib(7): 1,1,2,4,8,16,32,64,127,253,504,1004,2000,3984,7936
fib(8): 1,1,2,4,8,16,32,64,128,255,509,1016,2028,4048,8080
fib(9): 1,1,2,4,8,16,32,64,128,256,511,1021,2040,4076,8144
fib(10): 1,1,2,4,8,16,32,64,128,256,512,1023,2045,4088,8172
lucas(2): 2,1,3,4,7,11,18,29,47,76,123,199,322,521,843
lucas(3): 2,1,3,6,10,19,35,64,118,217,399,734,1350,2483,4567
lucas(4): 2,1,3,6,12,22,43,83,160,308,594,1145,2207,4254,8200
lucas(5): 2,1,3,6,12,24,46,91,179,352,692,1360,2674,5257,10335
lucas(6): 2,1,3,6,12,24,48,94,187,371,736,1460,2896,5744,11394
lucas(7): 2,1,3,6,12,24,48,96,190,379,755,1504,2996,5968,11888
lucas(8): 2,1,3,6,12,24,48,96,192,382,763,1523,3040,6068,12112
lucas(9): 2,1,3,6,12,24,48,96,192,384,766,1531,3059,6112,12212
lucas(10): 2,1,3,6,12,24,48,96,192,384,768,1534,3067,6131,12256
```



### ES6


```javascript
(() => {
    'use strict';

    // Start sequence -> Number of terms -> terms

    // takeNFibs :: [Int] -> Int -> [Int]
    const takeNFibs = (xs, n) => {
        const go = (xs, n) =>
            0 < n && 0 < xs.length ? (
                cons(
                    head(xs),
                    go(
                        append(tail(xs), [sum(xs)]),
                        n - 1
                    )
                )
            ) : [];
        return go(xs, n);
    };

    // fibInit :: Int -> [Int]
    const fibInit = n =>
        cons(
            1,
            map(x => Math.pow(2, x),
                enumFromToInt(0, n - 2)
            )
        );

    // TEST -----------------------------------------------------------------
    const main = () => {
        const
            intTerms = 15,
            strTable = unlines(
                zipWith(
                    (s, n) =>
                    justifyLeft(12, ' ', s + 'nacci') + ' -> ' +
                    showJSON(
                        takeNFibs(fibInit(n), intTerms)
                    ),
                    words('fibo tribo tetra penta hexa hepta octo nona deca'),
                    enumFromToInt(2, 10)
                )
            );

        return justifyLeft(12, ' ', 'Lucas ') + ' -> ' +
            showJSON(takeNFibs([2, 1], intTerms)) + '\n' +
            strTable;
    };

    // GENERIC FUNCTIONS ----------------------------

    // append (++) :: [a] -> [a] -> [a]
    // append (++) :: String -> String -> String
    const append = (xs, ys) => xs.concat(ys);

    // cons :: a -> [a] -> [a]
    const cons = (x, xs) =>
        Array.isArray(xs) ? (
            [x].concat(xs)
        ) : (x + xs);

    // enumFromToInt :: Int -> Int -> [Int]
    const enumFromToInt = (m, n) =>
        m <= n ? iterateUntil(
            x => n <= x,
            x => 1 + x,
            m
        ) : [];

    // head :: [a] -> a
    const head = xs => xs.length ? xs[0] : undefined;

    // iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
    const iterateUntil = (p, f, x) => {
        const vs = [x];
        let h = x;
        while (!p(h))(h = f(h), vs.push(h));
        return vs;
    };

    // justifyLeft :: Int -> Char -> String -> String
    const justifyLeft = (n, cFiller, s) =>
        n > s.length ? (
            s.padEnd(n, cFiller)
        ) : s;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // showJSON :: a -> String
    const showJSON = x => JSON.stringify(x);

    // sum :: [Num] -> Num
    const sum = xs => xs.reduce((a, x) => a + x, 0);

    // tail :: [a] -> [a]
    const tail = xs => 0 < xs.length ? xs.slice(1) : [];

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // words :: String -> [String]
    const words = s => s.split(/\s+/);

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) =>
        Array.from({
            length: Math.min(xs.length, ys.length)
        }, (_, i) => f(xs[i], ys[i], i));

    // MAIN ---
    return main();
})();
```


```txt
Lucas        -> [2,1,3,4,7,11,18,29,47,76,123,199,322,521,843]
fibonacci    -> [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]
tribonacci   -> [1,1,2,4,7,13,24,44,81,149,274,504,927,1705,3136]
tetranacci   -> [1,1,2,4,8,15,29,56,108,208,401,773,1490,2872,5536]
pentanacci   -> [1,1,2,4,8,16,31,61,120,236,464,912,1793,3525,6930]
hexanacci    -> [1,1,2,4,8,16,32,63,125,248,492,976,1936,3840,7617]
heptanacci   -> [1,1,2,4,8,16,32,64,127,253,504,1004,2000,3984,7936]
octonacci    -> [1,1,2,4,8,16,32,64,128,255,509,1016,2028,4048,8080]
nonanacci    -> [1,1,2,4,8,16,32,64,128,256,511,1021,2040,4076,8144]
decanacci    -> [1,1,2,4,8,16,32,64,128,256,512,1023,2045,4088,8172]
```



## jq

{{Works with|jq|1.4}}

```jq
# Input: the initial array
def nacci(arity; len):
  arity as $arity | len as $len
  | reduce range(length; $len) as $i
      (.;
       ([0, (length - $arity)] | max ) as $lower
       | . + [ .[ ($lower) : length] | add] ) ;

def fib(arity; len):
  arity as $arity | len as $len
  | [1,1] | nacci($arity; $arity) | nacci($arity; $len) ;

def lucas(arity; len):
  arity as $arity | len as $len
  | [2,1] | nacci($arity; $arity) | nacci($arity; $len) ;
```

'''Example''':

```jq
def main:
    (range(2; 11) | "fib(\(.)): \(fib(.; 15))"),
    (range(2; 11) | "lucas(\(.)): \(lucas(.; 15))")
;

main
```

{{Out}}
 $ jq -M -r -n -f fibonacci_n-step.jq
 ... [as for JavaScript] ...


## Julia

This solution provides a generalized Fibonacci iterator that is then made specific to particular sorts of series by setting its parameters.  <tt>NFib</tt> is the type that holds the series parameters.  <tt>FState</tt> contains the iteration state.  The methods <tt>start</tt>, <tt>end</tt> and <tt>next</tt>, provided for these new types, enable Julia's iteration mechanics upon them.

This iterator is implemented using an n-element circular list that contains the previous values of the sequence that are needed to calculate the current value.  To do this without clumsy initialization logic, the "seed" sequence consists of the <math>n</math> values prior to <math>k=1</math> rather than the first <math>n</math> values.  For example the (2 step) Fibonacci sequence is <math>F_{k+1}=F_{k}+F_{k-1}</math> with <math>F_{-1}=1</math> and <math>F_{0}=0</math> rather than <math>F_{1}=1</math> and <math>F_{2}=1</math>.  See [https://cs.uwaterloo.ca/journals/JIS/VOL8/Noe/noe5.html Primes in Fibonacci n-step and Lucas n-step Sequences] for further details.

'''Generalized Fibonacci Iterator Definition'''

```Julia

type NFib{T<:Integer}
    n::T
    klim::T
    seeder::Function
end

type FState
    a::Array{BigInt,1}
    adex::Integer
    k::Integer
end

function Base.start{T<:Integer}(nf::NFib{T})
    a = nf.seeder(nf.n)
    adex = 1
    k = 1
    return FState(a, adex, k)
end

function Base.done{T<:Integer}(nf::NFib{T}, fs::FState)
    fs.k > nf.klim
end

function Base.next{T<:Integer}(nf::NFib{T}, fs::FState)
    f = sum(fs.a)
    fs.a[fs.adex] = f
    fs.adex = rem1(fs.adex+1, nf.n)
    fs.k += 1
    return (f, fs)
end

```


'''Specification of the n-step Fibonacci Iterator'''

The seeding for this series of sequences is <math>F_{1-n} = 1</math> and <math>F_{2-n} \ldots F_{0}=0</math>.

```Julia

function fib_seeder{T<:Integer}(n::T)
    a = zeros(BigInt, n)
    a[1] = one(BigInt)
    return a
end

function fib{T<:Integer}(n::T, k::T)
    NFib(n, k, fib_seeder)
end

```


'''Specification of the Rosetta Code n-step Lucas Iterator'''

This iterator produces the task description's version of the Lucas Sequence ([https://oeis.org/A000032 OEIS A000032]) and its generalization to n-steps as was done by some of the other solutions to this task.  The seeding for this series of sequences is <math>F_{1-n} = 3</math>, <math>F_{2-n} = -1</math> and, for <math>n > 2</math>, <math>F_{3-n} \ldots F_{0}=0</math>.

```Julia

function luc_rc_seeder{T<:Integer}(n::T)
    a = zeros(BigInt, n)
    a[1] = 3
    a[2] = -1
    return a
end

function luc_rc{T<:Integer}(n::T, k::T)
    NFib(n, k, luc_rc_seeder)
end

```


'''Specification of the MathWorld n-step Lucas Iterator'''

This iterator produces the Mathworld version of the Lucas Sequence ([http://mathworld.wolfram.com/LucasNumber.html Lucas Number] and [https://oeis.org/A000204 OEIS A000204]) and its generalization to n-steps according to Mathworld ([http://mathworld.wolfram.com/Lucasn-StepNumber.html Lucas n-Step Number] and [https://cs.uwaterloo.ca/journals/JIS/VOL8/Noe/noe5.html Primes in Fibonacci n-step and Lucas n-step Sequences]).  The seeding for this series of sequences is <math>F_{0} = n</math> and <math>F_{1-n} \ldots F_{-1}=-1</math>.

```Julia

function luc_seeder{T<:Integer}(n::T)
    a = -ones(BigInt, n)
    a[end] = big(n)
    return a
end

function luc{T<:Integer}(n::T, k::T)
    NFib(n, k, luc_seeder)
end

```


'''Main'''

```Julia

lo = 2
hi = 10
klim = 16

print("n-step Fibonacci for n = (", lo, ",", hi)
println(") up to k = ", klim, ":")
for i in 2:10
    print(@sprintf("%5d => ", i))
    for j in fib(i, klim)
        print(j, " ")
    end
    println()
end

println()
print("n-step Rosetta Code Lucas for n = (", lo, ",", hi)
println(") up to k = ", klim, ":")
for i in 2:10
    print(@sprintf("%5d => ", i))
    for j in luc_rc(i, klim)
        print(j, " ")
    end
    println()
end

println()
print("n-step MathWorld Lucas for n = (", lo, ",", hi)
println(") up to k = ", klim, ":")
for i in 2:10
    print(@sprintf("%5d => ", i))
    for j in luc(i, klim)
        print(j, " ")
    end
    println()
end

```


{{out}}

```txt

n-step Fibonacci for n = (2,10) up to k = 16:
    2 => 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987
    3 => 1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136 5768
    4 => 1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536 10671
    5 => 1 1 2 4 8 16 31 61 120 236 464 912 1793 3525 6930 13624
    6 => 1 1 2 4 8 16 32 63 125 248 492 976 1936 3840 7617 15109
    7 => 1 1 2 4 8 16 32 64 127 253 504 1004 2000 3984 7936 15808
    8 => 1 1 2 4 8 16 32 64 128 255 509 1016 2028 4048 8080 16128
    9 => 1 1 2 4 8 16 32 64 128 256 511 1021 2040 4076 8144 16272
   10 => 1 1 2 4 8 16 32 64 128 256 512 1023 2045 4088 8172 16336

n-step Rosetta Code Lucas for n = (2,10) up to k = 16:
    2 => 2 1 3 4 7 11 18 29 47 76 123 199 322 521 843 1364
    3 => 2 1 3 6 10 19 35 64 118 217 399 734 1350 2483 4567 8400
    4 => 2 1 3 6 12 22 43 83 160 308 594 1145 2207 4254 8200 15806
    5 => 2 1 3 6 12 24 46 91 179 352 692 1360 2674 5257 10335 20318
    6 => 2 1 3 6 12 24 48 94 187 371 736 1460 2896 5744 11394 22601
    7 => 2 1 3 6 12 24 48 96 190 379 755 1504 2996 5968 11888 23680
    8 => 2 1 3 6 12 24 48 96 192 382 763 1523 3040 6068 12112 24176
    9 => 2 1 3 6 12 24 48 96 192 384 766 1531 3059 6112 12212 24400
   10 => 2 1 3 6 12 24 48 96 192 384 768 1534 3067 6131 12256 24500

n-step MathWorld Lucas for n = (2,10) up to k = 16:
    2 => 1 3 4 7 11 18 29 47 76 123 199 322 521 843 1364 2207
    3 => 1 3 7 11 21 39 71 131 241 443 815 1499 2757 5071 9327 17155
    4 => 1 3 7 15 26 51 99 191 367 708 1365 2631 5071 9775 18842 36319
    5 => 1 3 7 15 31 57 113 223 439 863 1695 3333 6553 12883 25327 49791
    6 => 1 3 7 15 31 63 120 239 475 943 1871 3711 7359 14598 28957 57439
    7 => 1 3 7 15 31 63 127 247 493 983 1959 3903 7775 15487 30847 61447
    8 => 1 3 7 15 31 63 127 255 502 1003 2003 3999 7983 15935 31807 63487
    9 => 1 3 7 15 31 63 127 255 511 1013 2025 4047 8087 16159 32287 64511
   10 => 1 3 7 15 31 63 127 255 511 1023 2036 4071 8139 16271 32527 65023

```



## Kotlin


```scala
// version 1.1.2

fun fibN(initial: IntArray, numTerms: Int) : IntArray {
    val n = initial.size
    require(n >= 2 && numTerms >= 0)
    val fibs = initial.copyOf(numTerms)
    if (numTerms <= n) return fibs
    for (i in n until numTerms) {
        var sum = 0
        for (j in i - n until i) sum += fibs[j]
        fibs[i] = sum
    }
    return fibs
}

fun main(args: Array<String>) {
    val names = arrayOf("fibonacci",  "tribonacci", "tetranacci", "pentanacci", "hexanacci",
                        "heptanacci", "octonacci",  "nonanacci",  "decanacci")
    val initial = intArrayOf(1, 1, 2, 4, 8, 16, 32, 64, 128, 256)
    println(" n  name        values")
    var values = fibN(intArrayOf(2, 1), 15).joinToString(", ")
    println("%2d  %-10s  %s".format(2, "lucas", values))
    for (i in 0..8) {
        values = fibN(initial.sliceArray(0 until i + 2), 15).joinToString(", ")
        println("%2d  %-10s  %s".format(i + 2, names[i], values))
    }
}
```


{{out}}

```txt

 n  name        values
 2  lucas       2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123, 199, 322, 521, 843
 2  fibonacci   1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610
 3  tribonacci  1, 1, 2, 4, 7, 13, 24, 44, 81, 149, 274, 504, 927, 1705, 3136
 4  tetranacci  1, 1, 2, 4, 8, 15, 29, 56, 108, 208, 401, 773, 1490, 2872, 5536
 5  pentanacci  1, 1, 2, 4, 8, 16, 31, 61, 120, 236, 464, 912, 1793, 3525, 6930
 6  hexanacci   1, 1, 2, 4, 8, 16, 32, 63, 125, 248, 492, 976, 1936, 3840, 7617
 7  heptanacci  1, 1, 2, 4, 8, 16, 32, 64, 127, 253, 504, 1004, 2000, 3984, 7936
 8  octonacci   1, 1, 2, 4, 8, 16, 32, 64, 128, 255, 509, 1016, 2028, 4048, 8080
 9  nonanacci   1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 511, 1021, 2040, 4076, 8144
10  decanacci   1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1023, 2045, 4088, 8172

```



## Lua


```Lua
function nStepFibs (seq, limit)
    local iMax, sum = #seq - 1
    while #seq < limit do
        sum = 0
        for i = 0, iMax do sum = sum + seq[#seq - i] end
        table.insert(seq, sum)
    end
    return seq
end

local fibSeqs = {
    {name = "Fibonacci",  values = {1, 1}      },
    {name = "Tribonacci", values = {1, 1, 2}   },
    {name = "Tetranacci", values = {1, 1, 2, 4}},
    {name = "Lucas",      values = {2, 1}      }
}
for _, sequence in pairs(fibSeqs) do
    io.write(sequence.name .. ": ")
    print(table.concat(nStepFibs(sequence.values, 10), " "))
end
```

{{out}}

```txt
Fibonacci: 1 1 2 3 5 8 13 21 34 55
Tribonacci: 1 1 2 4 7 13 24 44 81 149
Tetranacci: 1 1 2 4 8 15 29 56 108 208
Lucas: 2 1 3 4 7 11 18 29 47 76
```



## Maple


```maple
numSequence := proc(initValues :: Array)
	local n, i, values;
	n := numelems(initValues);
	values := copy(initValues);
	for i from (n+1) to 15 do
		values(i) := add(values[i-n..i-1]);
	end do;
	return values;
end proc:

initValues := Array([1]):
for i from 2 to 10 do
	initValues(i) := add(initValues):
	printf ("nacci(%d): %a\n", i, convert(numSequence(initValues), list));
end do:
printf ("lucas: %a\n", convert(numSequence(Array([2, 1])), list));
```

{{out}}

```txt

nacci(2): [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610]
nacci(3): [1, 1, 2, 4, 7, 13, 24, 44, 81, 149, 274, 504, 927, 1705, 3136]
nacci(4): [1, 1, 2, 4, 8, 15, 29, 56, 108, 208, 401, 773, 1490, 2872, 5536]
nacci(5): [1, 1, 2, 4, 8, 16, 31, 61, 120, 236, 464, 912, 1793, 3525, 6930]
nacci(6): [1, 1, 2, 4, 8, 16, 32, 63, 125, 248, 492, 976, 1936, 3840, 7617]
nacci(7): [1, 1, 2, 4, 8, 16, 32, 64, 127, 253, 504, 1004, 2000, 3984, 7936]
nacci(8): [1, 1, 2, 4, 8, 16, 32, 64, 128, 255, 509, 1016, 2028, 4048, 8080]
nacci(9): [1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 511, 1021, 2040, 4076, 8144]
nacci(10): [1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1023, 2045, 4088, 8172]
lucas: [2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123, 199, 322, 521, 843]

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica

f2=Function[{l,k},
  Module[{n=Length@l,m},
  m=SparseArray[{{i_,j_}/;i==1||i==j+1->1},{n,n}];
  NestList[m.#&,l,k]]];
Table[Last/@f2[{1,1}~Join~Table[0,{n-2}],15+n][[-18;;]],{n,2,10}]//TableForm
Table[Last/@f2[{1,2}~Join~Table[0,{n-2}],15+n][[-18;;]],{n,2,10}]//TableForm

```

Output:

```txt

1	1	2	3	5	8	13	21	34	55	89	144	233	377	610	987	1597	2584
1	1	2	4	7	13	24	44	81	149	274	504	927	1705	3136	5768	10609	19513
1	1	2	4	8	15	29	56	108	208	401	773	1490	2872	5536	10671	20569	39648
1	1	2	4	8	16	31	61	120	236	464	912	1793	3525	6930	13624	26784	52656
1	1	2	4	8	16	32	63	125	248	492	976	1936	3840	7617	15109	29970	59448
1	1	2	4	8	16	32	64	127	253	504	1004	2000	3984	7936	15808	31489	62725
1	1	2	4	8	16	32	64	128	255	509	1016	2028	4048	8080	16128	32192	64256
1	1	2	4	8	16	32	64	128	256	511	1021	2040	4076	8144	16272	32512	64960
1	1	2	4	8	16	32	64	128	256	512	1023	2045	4088	8172	16336	32656	65280

2	1	3	4	7	11	18	29	47	76	123	199	322	521	843	1364	2207	3571
2	1	3	6	10	19	35	64	118	217	399	734	1350	2483	4567	8400	15450	28417
2	1	3	6	12	22	43	83	160	308	594	1145	2207	4254	8200	15806	30467	58727
2	1	3	6	12	24	46	91	179	352	692	1360	2674	5257	10335	20318	39944	78528
2	1	3	6	12	24	48	94	187	371	736	1460	2896	5744	11394	22601	44831	88926
2	1	3	6	12	24	48	96	190	379	755	1504	2996	5968	11888	23680	47170	93961
2	1	3	6	12	24	48	96	192	382	763	1523	3040	6068	12112	24176	48256	96320
2	1	3	6	12	24	48	96	192	384	766	1531	3059	6112	12212	24400	48752	97408
2	1	3	6	12	24	48	96	192	384	768	1534	3067	6131	12256	24500	48976	97904


```



## Nim

{{trans|Python}}

```nim
import sequtils, strutils

proc fiblike(start: seq[int]): auto =
  var memo = start
  proc fibber(n: int): int =
    if n < memo.len:
      return memo[n]
    else:
      var ans = 0
      for i in n-start.len .. <n:
        ans += fibber(i)
      memo.add ans
      return ans
  return fibber

let fibo = fiblike(@[1,1])
echo toSeq(0..9).map(fibo)
let lucas = fiblike(@[2,1])
echo toSeq(0..9).map(lucas)

for n, name in items({2: "fibo", 3: "tribo", 4: "tetra", 5: "penta", 6: "hexa",
                      7: "hepta", 8: "octo", 9: "nona", 10: "deca"}):
  var se = @[1]
  for i in 0..n-2:
    se.add(1 shl i)
  let fibber = fiblike(se)
  echo "n = ", align($n,2), ", ", align(name, 5), "nacci ->
    ", toSeq(0..14).mapIt(string, $fibber(it)).join(" "), " ..."
```

Output:

```txt
@[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
@[2, 1, 3, 4, 7, 11, 18, 29, 47, 76]
n =  2,  fibonacci -> 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 ...
n =  3, tribonacci -> 1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136 ...
n =  4, tetranacci -> 1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536 ...
n =  5, pentanacci -> 1 1 2 4 8 16 31 61 120 236 464 912 1793 3525 6930 ...
n =  6,  hexanacci -> 1 1 2 4 8 16 32 63 125 248 492 976 1936 3840 7617 ...
n =  7, heptanacci -> 1 1 2 4 8 16 32 64 127 253 504 1004 2000 3984 7936 ...
n =  8,  octonacci -> 1 1 2 4 8 16 32 64 128 255 509 1016 2028 4048 8080 ...
n =  9,  nonanacci -> 1 1 2 4 8 16 32 64 128 256 511 1021 2040 4076 8144 ...
n = 10,  decanacci -> 1 1 2 4 8 16 32 64 128 256 512 1023 2045 4088 8172 ...
```



## Ol

We will use lazy lists, so can get any amount of n-nacci numbers.


```scheme

(define (n-fib-iterator ll)
   (cons (car ll)
         (lambda ()
            (n-fib-iterator (append (cdr ll) (list (fold + 0 ll)))))))

```


Testing:

```scheme

(print "2, fibonacci : " (ltake (n-fib-iterator '(1 1)) 15))
(print "3, tribonacci: " (ltake (n-fib-iterator '(1 1 2)) 15))
(print "4, tetranacci: " (ltake (n-fib-iterator '(1 1 2 4)) 15))
(print "5, pentanacci: " (ltake (n-fib-iterator '(1 1 2 4 8)) 15))
(print "2, lucas : " (ltake (n-fib-iterator '(2 1)) 15))

; ==>
2, fibonacci : (1 1 2 3 5 8 13 21 34 55 89 144 233 377 610)
3, tribonacci: (1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136)
4, tetranacci: (1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536)
5, pentanacci: (1 1 2 4 8 16 31 61 120 236 464 912 1793 3525 6930)
2, lucas : (2 1 3 4 7 11 18 29 47 76 123 199 322 521 843)

```



## PARI/GP

The function <code>gen</code> generates code to generate a given number of terms of the k-th sequence. Of course there are other approaches.

Use genV if you prefer to supply a different starting vector.

```parigp
gen(n)=k->my(v=vector(k,i,1));for(i=3,min(k,n),v[i]=2^(i-2));for(i=n+1,k,v[i]=sum(j=i-n,i-1,v[j]));v
genV(n)=v->for(i=3,min(#v,n),v[i]=2^(i-2));for(i=n+1,#v,v[i]=sum(j=i-n,i-1,v[j]));v
for(n=2,10,print(n"\t"gen(n)(10)))
```

{{out}}

```txt
2       [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
3       [1, 1, 2, 4, 7, 13, 24, 44, 81, 149]
4       [1, 1, 2, 4, 8, 15, 29, 56, 108, 208]
5       [1, 1, 2, 4, 8, 16, 31, 61, 120, 236]
6       [1, 1, 2, 4, 8, 16, 32, 63, 125, 248]
7       [1, 1, 2, 4, 8, 16, 32, 64, 127, 253]
8       [1, 1, 2, 4, 8, 16, 32, 64, 128, 255]
9       [1, 1, 2, 4, 8, 16, 32, 64, 128, 256]
10      [1, 1, 2, 4, 8, 16, 32, 64, 128, 256]
```



## Pascal

{{works with|Free_Pascal}}

```pascal
program FibbonacciN (output);

type
  TintArray = array of integer;
const
  Name: array[2..11] of string = ('Fibonacci:  ',
                                  'Tribonacci: ',
                                  'Tetranacci: ',
                                  'Pentanacci: ',
                                  'Hexanacci:  ',
                                  'Heptanacci: ',
                                  'Octonacci:  ',
                                  'Nonanacci:  ',
                                  'Decanacci:  ',
                                  'Lucas:      '
                                 );
var
  sequence: TintArray;
  j, k: integer;

function CreateFibbo(n: integer): TintArray;
  var
    i: integer;
  begin
    setlength(CreateFibbo, n);
    CreateFibbo[0] := 1;
    CreateFibbo[1] := 1;
    i := 2;
    while i < n do
    begin
      CreateFibbo[i] := CreateFibbo[i-1] * 2;
      inc(i);
    end;
  end;

procedure Fibbonacci(var start: TintArray);
  const
    No_of_examples = 11;
  var
    n, i, j: integer;
  begin
    n := length(start);
    setlength(start, No_of_examples);
    for i := n to high(start) do
    begin
      start[i] := 0;
      for j := 1 to n do
        start[i] := start[i] + start[i-j]
    end;
  end;

begin
  for j := 2 to 10 do
  begin
    sequence := CreateFibbo(j);
    Fibbonacci(sequence);
    write (Name[j]);
    for k := low(sequence) to high(sequence) do
      write(sequence[k], ' ');
    writeln;
  end;
  setlength(sequence, 2);
  sequence[0] := 2;
  sequence[1] := 1;
  Fibbonacci(sequence);
  write (Name[11]);
  for k := low(sequence) to high(sequence) do
    write(sequence[k], ' ');
  writeln;
end.
```

Output:

```txt
% ./Fibbonacci
Fibonacci:  1 1 2 3 5 8 13 21 34 55 89
Tribonacci: 1 1 2 4 7 13 24 44 81 149 274
Tetranacci: 1 1 2 4 8 15 29 56 108 208 401
Pentanacci: 1 1 2 4 8 16 31 61 120 236 464
Hexanacci:  1 1 2 4 8 16 32 63 125 248 492
Heptanacci: 1 1 2 4 8 16 32 64 127 253 504
Octonacci:  1 1 2 4 8 16 32 64 128 255 509
Nonanacci:  1 1 2 4 8 16 32 64 128 256 511
Decanacci:  1 1 2 4 8 16 32 64 128 256 512
Lucas:      2 1 3 4 7 11 18 29 47 76 123
```



### Alternative

With the same output like above.
A little bit like C++ alternative, but using only one idx and the observation,
 that Sum[n] = 2*Sum[n-1]- Sum[n-stepSize].
There is no need to do so in Terms of speed, since fib(100) is out of reach using Uint64.
Fib(n)/Fib(n-1) tends to the golden ratio = 1.618... 1.618^100 > 2^64
{{works with|Free_Pascal}}

```pascal

program FibbonacciN (output);
{$IFNDEF FPC}
   {$APPTYPE CONSOLE}
{$ENDIF}
const
  MAX_Nacci = 10;

  No_of_examples = 11;// max 90; (golden ratio)^No < 2^64
  Name: array[2..11] of string = ('Fibonacci:  ',
                                  'Tribonacci: ',
                                  'Tetranacci: ',
                                  'Pentanacci: ',
                                  'Hexanacci:  ',
                                  'Heptanacci: ',
                                  'Octonacci:  ',
                                  'Nonanacci:  ',
                                  'Decanacci:  ',
                                  'Lucas:      '
                                 );

type
  tfibIdx = 0..MAX_Nacci;
  tNacVal = Uint64;// longWord
  tNacci = record
             ncSum      : tNacVal;
             ncLastFib  : array[tFibIdx] of tNacVal;
             ncNextIdx  : array[tFibIdx] of tFibIdx;
             ncIdx      : tFibIdx;
             ncValue    : tFibIdx;
           end;


function CreateNacci(n: tFibIdx): TNacci;
var
  i : tFibIdx;
  sum :tNacVal;
begin
  //With result do
  with CreateNacci do
  begin
     ncLastFib[0] := 1;
     ncLastFib[1] := 1;
     For i := 2 to n-1 do
       ncLastFib[i] := ncLastFib[i-1] * 2;

     Sum := 0;
     For i := 0 to n-1 do
       sum := sum +ncLastFib[i];
     ncSum := Sum;
     //No need to do a compare
     //inc(idx);
     //if idx>= n then
     //  idx := 0;
     //idx := nextIdx[idx]
     For i := 0 to n-2 do
       ncNextIdx[i] := i+1;
     ncNextIdx[n-1] := 0;
     ncIdx   := 0;
  end;
end;

function LehmerCreate:TNacci;
begin
  with LehmerCreate do
  begin
     ncLastFib[0] := 2;
     ncLastFib[1] := 1;
     ncSum := 3;
     ncNextIdx[0] := 1;
     ncNextIdx[1] := 0;
     ncIdx   := 0;
  end;
end;

function NextNacci(var Nacci:tNacci):tNacVal;
var
  NewSum :tNacVal;
begin
  with Nacci do
  begin
    NewSum := 2*ncSum- ncLastFib[ncIdx];
    ncLastFib[ncIdx] := ncSum;
    ncIdx := ncNextIdx[ncIdx];
    NextNacci := ncSum;
    ncSum := NewSum;
  end;
end;

var
  Nacci : tNacci;
  j, k: integer;

BEGIN
  for j := 2 to 10 do
  begin
    Nacci := CreateNacci(j);
    write (Name[j]);
    For k := 0 to j-1 do
      write(Nacci.ncLastFib[k],' ');
    For k := j to No_of_examples-1 do
      write(NextNacci(Nacci),' ');
    writeln;
  end;

  write (Name[11]);
  j := 2;
  Nacci := LehmerCreate;
  For k := 0 to j-1 do
    write(Nacci.ncLastFib[k],' ');
  For k := j to No_of_examples-1 do
    write(NextNacci(Nacci),' ');
  writeln;
END.
```



## Perl


```perl
use 5.010;

use List::Util qw/max sum/;

sub fib {
    my $n = shift;
    my $xs = shift // [1];
    my @xs = @{$xs};

    while (my $len = scalar @xs) {
        last if $len >= 20;
        push(
            @xs,
            sum(@xs[max($len - $n, 0)..$len-1])
        );
    }

    return @xs;
}

for (2..10) {
    say join(' ', fib($_));
}
say join(' ', fib(2, [2,1]));
```


{{out}}

```txt
1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765
1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136 5768 10609 19513 35890 66012
1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536 10671 20569 39648 76424 147312
1 1 2 4 8 16 31 61 120 236 464 912 1793 3525 6930 13624 26784 52656 103519 203513
1 1 2 4 8 16 32 63 125 248 492 976 1936 3840 7617 15109 29970 59448 117920 233904
1 1 2 4 8 16 32 64 127 253 504 1004 2000 3984 7936 15808 31489 62725 124946 248888
1 1 2 4 8 16 32 64 128 255 509 1016 2028 4048 8080 16128 32192 64256 128257 256005
1 1 2 4 8 16 32 64 128 256 511 1021 2040 4076 8144 16272 32512 64960 129792 259328
1 1 2 4 8 16 32 64 128 256 512 1023 2045 4088 8172 16336 32656 65280 130496 260864
2 1 3 4 7 11 18 29 47 76 123 199 322 521 843 1364 2207 3571 5778 9349
```



## Perl 6



### Lazy List with Closure


```perl6
use MONKEY-SEE-NO-EVAL;

sub fibo ($n) {
    constant @starters = 1,1,2,4 ... *;
    nacci @starters[^$n];
}

sub nacci (*@starter) {
    EVAL "|@starter, { join '+', '*' xx @starter } ... *";
}

for 2..10 -> $n { say fibo($n)[^20] }
say nacci(2,1)[^20];
```

{{out}}

```txt
1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765
1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136 5768 10609 19513 35890 66012
1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536 10671 20569 39648 76424 147312
1 1 2 4 8 16 31 61 120 236 464 912 1793 3525 6930 13624 26784 52656 103519 203513
1 1 2 4 8 16 32 63 125 248 492 976 1936 3840 7617 15109 29970 59448 117920 233904
1 1 2 4 8 16 32 64 127 253 504 1004 2000 3984 7936 15808 31489 62725 124946 248888
1 1 2 4 8 16 32 64 128 255 509 1016 2028 4048 8080 16128 32192 64256 128257 256005
1 1 2 4 8 16 32 64 128 256 511 1021 2040 4076 8144 16272 32512 64960 129792 259328
1 1 2 4 8 16 32 64 128 256 512 1023 2045 4088 8172 16336 32656 65280 130496 260864
2 1 3 4 7 11 18 29 47 76 123 199 322 521 843 1364 2207 3571 5778 9349
```



### Generative

A slightly more straight forward way of constructing a lazy list.
{{works with|Rakudo|2015.12}}

```perl6
sub fib ($n, @xs is copy = [1]) {
    flat gather {
        take @xs[*];
        loop {
            take my $x = [+] @xs;
            @xs.push: $x;
            @xs.shift if @xs > $n;
        }
    }
}

for 2..10 -> $n {
    say fib($n, [1])[^20];
}
say fib(2, [2,1])[^20];
```



## Phix


```Phix
function nacci_noo(integer n, s, l)
    if n<2 then return n+n*l end if
    if n=2 then return 1 end if
    atom res = nacci_noo(n-1,s,l)
    for i=2 to min(s,n-1) do
        res += nacci_noo(n-i,s,l)
    end for
    return res
end function

constant names = split("lucas fibo tribo tetra penta hexa hepta octo nona deca")
sequence f = repeat(0,10)
for i=1 to 4 do
    for j=1 to 10 do
        f[j] = nacci_noo(j,i+(i=1),i=1)
    end for
    printf(1,"%snacci: %s\n",{names[i],sprint(f)})
end for
```

{{out}}

```txt

lucasnacci: {2,1,3,4,7,11,18,29,47,76}
fibonacci: {1,1,2,3,5,8,13,21,34,55}
tribonacci: {1,1,2,4,7,13,24,44,81,149}
tetranacci: {1,1,2,4,8,15,29,56,108,208}

```



## PHP


```php
<?php
/**
 * @author Elad Yosifon
 */

/**
 * @param int $x
 * @param array $series
 * @param int $n
 * @return array
 */
function fib_n_step($x, &$series = array(1, 1), $n = 15)
{
	$count = count($series);

	if($count > $x && $count == $n) // exit point
	{
		return $series;
	}

	if($count < $n)
	{
		if($count >= $x) // 4 or less
		{
			fib($series, $x, $count);
			return fib_n_step($x, $series, $n);
		}
		else // 5 or more
		{
			while(count($series) < $x )
			{
				$count = count($series);
				fib($series, $count, $count);
			}
			return fib_n_step($x, $series, $n);
		}
	}

	return $series;
}

/**
 * @param array $series
 * @param int $n
 * @param int $i
 */
function fib(&$series, $n, $i)
{
	$end = 0;
	for($j = $n; $j > 0; $j--)
	{
		$end += $series[$i-$j];
	}
	$series[$i] = $end;
}


/*
### ================  OUTPUT =========================
*/

header('Content-Type: text/plain');
$steps = array(
	'LUCAS' => 		array(2, 	array(2, 1)),
	'FIBONACCI' => 	array(2, 	array(1, 1)),
	'TRIBONACCI' =>	array(3, 	array(1, 1, 2)),
	'TETRANACCI' =>	array(4, 	array(1, 1, 2, 4)),
	'PENTANACCI' =>	array(5,	array(1, 1, 2, 4)),
	'HEXANACCI' =>	array(6, 	array(1, 1, 2, 4)),
	'HEPTANACCI' =>	array(7,	array(1, 1, 2, 4)),
	'OCTONACCI' =>	array(8, 	array(1, 1, 2, 4)),
	'NONANACCI' =>	array(9, 	array(1, 1, 2, 4)),
	'DECANACCI' =>	array(10, 	array(1, 1, 2, 4)),
);

foreach($steps as $name=>$pair)
{
	$ser = fib_n_step($pair[0],$pair[1]);
	$n = count($ser)-1;

	echo $name." => ".implode(',', $ser) . "\n";
}


```

{{out}}

```txt

LUCAS => 2,1,3,4,7,11,18,29,47,76,123,199,322,521,843
FIBONACCI => 1,1,2,3,5,8,13,21,34,55,89,144,233,377,610
TRIBONACCI => 1,1,2,4,7,13,24,44,81,149,274,504,927,1705,3136
TETRANACCI => 1,1,2,4,8,15,29,56,108,208,401,773,1490,2872,5536
PENTANACCI => 1,1,2,4,8,16,31,61,120,236,464,912,1793,3525,6930
HEXANACCI => 1,1,2,4,8,16,32,63,125,248,492,976,1936,3840,7617
HEPTANACCI => 1,1,2,4,8,16,32,64,127,253,504,1004,2000,3984,7936
OCTONACCI => 1,1,2,4,8,16,32,64,128,255,509,1016,2028,4048,8080
NONANACCI => 1,1,2,4,8,16,32,64,128,256,511,1021,2040,4076,8144
DECANACCI => 1,1,2,4,8,16,32,64,128,256,512,1023,2045,4088,8172

```



## PicoLisp


```PicoLisp
(de nacci (Init Cnt)
   (let N (length Init)
      (make
         (made Init)
         (do (- Cnt N)
            (link (apply + (tail N (made)))) ) ) ) )
```

Test:

```PicoLisp
# Fibonacci
: (nacci (1 1) 10)
-> (1 1 2 3 5 8 13 21 34 55)

# Tribonacci
: (nacci (1 1 2) 10)
-> (1 1 2 4 7 13 24 44 81 149)

# Tetranacci
: (nacci (1 1 2 4) 10)
-> (1 1 2 4 8 15 29 56 108 208)

# Lucas
: (nacci (2 1) 10)
-> (2 1 3 4 7 11 18 29 47 76)

# Decanacci
: (nacci (1 1 2 4 8 16 32 64 128 256) 15)
-> (1 1 2 4 8 16 32 64 128 256 512 1023 2045 4088 8172)
```



## PL/I


```PL/I
(subscriptrange, fixedoverflow, size):
n_step_Fibonacci: procedure options (main);
   declare line character (100) varying;
   declare (i, j, k) fixed binary;

   put ('n-step Fibonacci series: Please type the initial values on one line:');
   get edit (line) (L);
   line = trim(line);
   k = tally(line, ' ') - tally(line, '  ') + 1; /* count values */

   begin;
      declare (n(k), s) fixed decimal (15);
      get string (line || ' ') list ( n );

      if n(1) = 2 then put ('We have a Lucan series');
      else put ('We have a ' || trim(k) || '-step Fibonacci series.');

      put skip edit ( (trim(n(i)) do i = 1 to k) ) (a, x(1));
      do j = k+1 to 20; /* In toto, generate 20 values in the series. */
         s = sum(n); /* the next value in the series */
         put edit (trim(s)) (x(1), a);
         do i = lbound(n,1)+1 to k; /* Discard the oldest value */
            n(i-1) = n(i);
         end;
         n(k) = s; /* and insert the new value */
      end;
   end;
end n_step_Fibonacci;
```

Output:

```txt

We have a Lucan series.
2 1 3 4 7 11 18 29 47 76 123 199 322 521 843 1364 2207 3571 5778 9349

We have a 2-step Fibonacci series.
1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765

We have a 3-step Fibonacci series.
1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136 5768 10609 19513 35890 66012

We have a 4-step Fibonacci series.
1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536 10671 20569 39648 76424 147312

We have a 5-step Fibonacci series.
1 1 2 4 8 16 31 61 120 236 464 912 1793 3525 6930 13624 26784 52656 103519 203513

```



## Powershell


```Powershell
#Create generator of extended fibonaci
Function Get-ExtendedFibonaciGenerator($InitialValues ){
    $Values = $InitialValues
    {
        #exhaust initial values first before calculating next values by summation
        if ($InitialValues.Length -gt 0) {
            $NextValue = $InitialValues[0]
            $Script:InitialValues = $InitialValues | Select -Skip 1
            return $NextValue
        }

        $NextValue = $Values | Measure-Object -Sum | Select -ExpandProperty Sum
        $Script:Values = @($Values | Select-Object -Skip 1) + @($NextValue)

        $NextValue
    }.GetNewClosure()
}

```


Example of invocation to generate up to decanaci


```Powershell
$Name = 'fibo tribo tetra penta hexa hepta octo nona deca'.Split()
0..($Name.Length-1) | foreach { $Index = $_
    $InitialValues = @(1) + @(foreach ($I In 0..$Index) { [Math]::Pow(2,$I) })
    $Generator = Get-ExtendedFibonaciGenerator $InitialValues
    [PSCustomObject] @{
        n        = $InitialValues.Length;
        Name     = "$($Name[$Index])naci";
        Sequence = 1..15 | foreach { & $Generator } | Join-String -Separator ','
    }
} | Format-Table -AutoSize

```

Sample output

```txt

 n Name      Sequence
 - ----      --------
 2 fibonaci  1,1,2,3,5,8,13,21,34,55,89,144,233,377,610
 3 tribonaci 1,1,2,4,7,13,24,44,81,149,274,504,927,1705,3136
 4 tetranaci 1,1,2,4,8,15,29,56,108,208,401,773,1490,2872,5536
 5 pentanaci 1,1,2,4,8,16,31,61,120,236,464,912,1793,3525,6930
 6 hexanaci  1,1,2,4,8,16,32,63,125,248,492,976,1936,3840,7617
 7 heptanaci 1,1,2,4,8,16,32,64,127,253,504,1004,2000,3984,7936
 8 octonaci  1,1,2,4,8,16,32,64,128,255,509,1016,2028,4048,8080
 9 nonanaci  1,1,2,4,8,16,32,64,128,256,511,1021,2040,4076,8144
10 decanaci  1,1,2,4,8,16,32,64,128,256,512,1023,2045,4088,8172

```



## PureBasic


```PureBasic


Procedure.i FibonacciLike(k,n=2,p.s="",d.s=".")
Protected i,r
if k<0:ProcedureReturn 0:endif
if p.s
n=CountString(p.s,d.s)+1
for i=0 to n-1
if k=i:ProcedureReturn val(StringField(p.s,i+1,d.s)):endif
next
else
if k=0:ProcedureReturn 1:endif
if k=1:ProcedureReturn 1:endif
endif
for i=1 to n
r+FibonacciLike(k-i,n,p.s,d.s)
next
ProcedureReturn r
EndProcedure

; The fact that PureBasic supports default values for procedure parameters
; is very useful in a case such as this.
; Since:
; k=4
; Debug FibonacciLike(k)               ;good old Fibonacci

; Debug FibonacciLike(k,3)             ;here we specified n=3 [Tribonacci]
; Debug FibonacciLike(k,3,"1.1.2")     ;using the default delimiter "."
; Debug FibonacciLike(k,3,"1,1,2",",") ;using a different delimiter ","
; the last three all produce the same result.

; as do the following two for the Lucas series:
; Debug FibonacciLike(k,2,"2.1")     ;using the default delimiter "."
; Debug FibonacciLike(k,2,"2,1",",") ;using a different delimiter ","

m=10
t.s=lset("n",5)
for k=0 to m
  t.s+lset(str(k),5)
  next
Debug t.s
for n=2 to 10
  t.s=lset(str(n),5)
  for k=0 to m
    t.s+lset(str(FibonacciLike(k,n)),5)
    next
Debug t.s
next
Debug ""
p.s="2.1"
t.s=lset(p.s,5)
for k=0 to m
  t.s+lset(str(FibonacciLike(k,n,p.s)),5)
  next
Debug t.s
Debug ""


```


'''Sample Output'''

```txt

n    0    1    2    3    4    5    6    7    8    9    10
2    1    1    2    3    5    8    13   21   34   55   89
3    1    1    2    4    7    13   24   44   81   149  274
4    1    1    2    4    8    15   29   56   108  208  401
5    1    1    2    4    8    16   31   61   120  236  464
6    1    1    2    4    8    16   32   63   125  248  492
7    1    1    2    4    8    16   32   64   127  253  504
8    1    1    2    4    8    16   32   64   128  255  509
9    1    1    2    4    8    16   32   64   128  256  511
10   1    1    2    4    8    16   32   64   128  256  512

2.1  2    1    3    4    7    11   18   29   47   76   123



```



## Python


### Python: function returning a function


```python>>>
 def fiblike(start):
	addnum = len(start)
	memo = start[:]
	def fibber(n):
		try:
			return memo[n]
		except IndexError:
			ans = sum(fibber(i) for i in range(n-addnum, n))
			memo.append(ans)
			return ans
	return fibber

>>> fibo = fiblike([1,1])
>>> [fibo(i) for i in range(10)]
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
>>> lucas = fiblike([2,1])
>>> [lucas(i) for i in range(10)]
[2, 1, 3, 4, 7, 11, 18, 29, 47, 76]
>>> for n, name in zip(range(2,11), 'fibo tribo tetra penta hexa hepta octo nona deca'.split()) :
	fibber = fiblike([1] + [2**i for i in range(n-1)])
	print('n=%2i, %5snacci -> %s ...' % (n, name, ' '.join(str(fibber(i)) for i in range(15))))


n= 2,  fibonacci -> 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 ...
n= 3, tribonacci -> 1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136 ...
n= 4, tetranacci -> 1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536 ...
n= 5, pentanacci -> 1 1 2 4 8 16 31 61 120 236 464 912 1793 3525 6930 ...
n= 6,  hexanacci -> 1 1 2 4 8 16 32 63 125 248 492 976 1936 3840 7617 ...
n= 7, heptanacci -> 1 1 2 4 8 16 32 64 127 253 504 1004 2000 3984 7936 ...
n= 8,  octonacci -> 1 1 2 4 8 16 32 64 128 255 509 1016 2028 4048 8080 ...
n= 9,  nonanacci -> 1 1 2 4 8 16 32 64 128 256 511 1021 2040 4076 8144 ...
n=10,  decanacci -> 1 1 2 4 8 16 32 64 128 256 512 1023 2045 4088 8172 ...
>>>
```



### Python: Callable class


```python>>>
 class Fiblike():
	def __init__(self, start):
		self.addnum = len(start)
		self.memo = start[:]
	def __call__(self, n):
		try:
			return self.memo[n]
		except IndexError:
			ans = sum(self(i) for i in range(n-self.addnum, n))
			self.memo.append(ans)
			return ans


>>> fibo = Fiblike([1,1])
>>> [fibo(i) for i in range(10)]
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
>>> lucas = Fiblike([2,1])
>>> [lucas(i) for i in range(10)]
[2, 1, 3, 4, 7, 11, 18, 29, 47, 76]
>>> for n, name in zip(range(2,11), 'fibo tribo tetra penta hexa hepta octo nona deca'.split()) :
	fibber = Fiblike([1] + [2**i for i in range(n-1)])
	print('n=%2i, %5snacci -> %s ...' % (n, name, ' '.join(str(fibber(i)) for i in range(15))))


n= 2,  fibonacci -> 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 ...
n= 3, tribonacci -> 1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136 ...
n= 4, tetranacci -> 1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536 ...
n= 5, pentanacci -> 1 1 2 4 8 16 31 61 120 236 464 912 1793 3525 6930 ...
n= 6,  hexanacci -> 1 1 2 4 8 16 32 63 125 248 492 976 1936 3840 7617 ...
n= 7, heptanacci -> 1 1 2 4 8 16 32 64 127 253 504 1004 2000 3984 7936 ...
n= 8,  octonacci -> 1 1 2 4 8 16 32 64 128 255 509 1016 2028 4048 8080 ...
n= 9,  nonanacci -> 1 1 2 4 8 16 32 64 128 256 511 1021 2040 4076 8144 ...
n=10,  decanacci -> 1 1 2 4 8 16 32 64 128 256 512 1023 2045 4088 8172 ...
>>>
```



### Python: Generator


```python
from itertools import islice, cycle

def fiblike(tail):
    for x in tail:
        yield x
    for i in cycle(xrange(len(tail))):
        tail[i] = x = sum(tail)
        yield x

fibo = fiblike([1, 1])
print list(islice(fibo, 10))
lucas = fiblike([2, 1])
print list(islice(lucas, 10))

suffixes = "fibo tribo tetra penta hexa hepta octo nona deca"
for n, name in zip(xrange(2, 11), suffixes.split()):
    fib = fiblike([1] + [2 ** i for i in xrange(n - 1)])
    items = list(islice(fib, 15))
    print "n=%2i, %5snacci -> %s ..." % (n, name, items)
```

{{out}}

```txt
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
[2, 1, 3, 4, 7, 11, 18, 29, 47, 76]
n= 2,  fibonacci -> [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610] ...
n= 3, tribonacci -> [1, 1, 2, 4, 7, 13, 24, 44, 81, 149, 274, 504, 927, 1705, 3136] ...
n= 4, tetranacci -> [1, 1, 2, 4, 8, 15, 29, 56, 108, 208, 401, 773, 1490, 2872, 5536] ...
n= 5, pentanacci -> [1, 1, 2, 4, 8, 16, 31, 61, 120, 236, 464, 912, 1793, 3525, 6930] ...
n= 6,  hexanacci -> [1, 1, 2, 4, 8, 16, 32, 63, 125, 248, 492, 976, 1936, 3840, 7617] ...
n= 7, heptanacci -> [1, 1, 2, 4, 8, 16, 32, 64, 127, 253, 504, 1004, 2000, 3984, 7936] ...
n= 8,  octonacci -> [1, 1, 2, 4, 8, 16, 32, 64, 128, 255, 509, 1016, 2028, 4048, 8080] ...
n= 9,  nonanacci -> [1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 511, 1021, 2040, 4076, 8144] ...
n=10,  decanacci -> [1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1023, 2045, 4088, 8172] ...
```



### Python: Curried

A recipe quickly assembled from generic ingredients in the cupboard.
{{Trans|Haskell}}
{{Works with|Python|3.7}}

```python
'''Fibonacci n-step number sequences'''


# fibInit :: Int -> [Int]
def fibInit(n):
    '''Initial values for a
       Fibonacci n-step number sequence
       of order n.
    '''
    return [1] + [
        pow(2, x) for x
        in enumFromTo(0)(n - 2)
    ]


# takeNFibs :: [Int] -> Int -> [Int]
def takeNFibs(xs):
    '''Given the initial members, the
       continuation to the nth member of
       a Fibonacci n-step number sequence.
    '''
    def go(xs, n):
        h, *t = xs
        return [h] + (
            go(t + [sum(xs)], n - 1)
        ) if 0 < n and xs else []
    return lambda n: go(xs, n)


# TESTS ----------------------------------------------------
# main :: IO ()
def main():
    '''Various n-step sequences'''

    print(
        fTable(__doc__ + ':\n')(fst)(showList)(
            compose(flip(takeNFibs)(15))(snd)
        )([('Lucas', [2, 1])] + list(zip(
            [k + 'nacci' for k in words(
                'fibo tribo tetra penta hexa hepta octo nona deca'
            )],
            [fibInit(n) for n in enumFromTo(2)(10)]
        )))
    )


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# flip :: (a -> b -> c) -> b -> a -> c
def flip(f):
    '''The curried function f with its
       arguments reversed.'''
    return lambda a: lambda b: f(b)(a)


# fst :: (a, b) -> a
def fst(tpl):
    '''First member of a pair.'''
    return tpl[0]


# showList :: [a] -> String
def showList(xs):
    '''Stringification of a list.'''
    return '[' + ','.join(str(x) for x in xs) + ']'


# words :: String -> [String]
def words(s):
    '''A list of words delimited by characters
       representing white space.'''
    return s.split()


# fTable :: String -> (a -> String) ->
#                     (b -> String) ->
#        (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
          f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join([
            xShow(x).rjust(w, ' ') + ' -> ' + fxShow(f(x)) for x in xs
        ])
    return lambda xShow: lambda fxShow: (
        lambda f: lambda xs: go(
            xShow, fxShow, f, xs
        )
    )


# snd :: (a, b) -> b
def snd(tpl):
    '''Second member of a pair.'''
    return tpl[1]


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Fibonacci n-step number sequences:

     Lucas -> [2,1,3,4,7,11,18,29,47,76,123,199,322,521,843]
 fibonacci -> [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]
tribonacci -> [1,1,2,4,7,13,24,44,81,149,274,504,927,1705,3136]
tetranacci -> [1,1,2,4,8,15,29,56,108,208,401,773,1490,2872,5536]
pentanacci -> [1,1,2,4,8,16,31,61,120,236,464,912,1793,3525,6930]
 hexanacci -> [1,1,2,4,8,16,32,63,125,248,492,976,1936,3840,7617]
heptanacci -> [1,1,2,4,8,16,32,64,127,253,504,1004,2000,3984,7936]
 octonacci -> [1,1,2,4,8,16,32,64,128,255,509,1016,2028,4048,8080]
 nonanacci -> [1,1,2,4,8,16,32,64,128,256,511,1021,2040,4076,8144]
 decanacci -> [1,1,2,4,8,16,32,64,128,256,512,1023,2045,4088,8172]
```



## Racket


```Racket
#lang racket

;; fib-list : [Listof Nat] x Nat -> [Listof Nat]
;; Given a non-empty list of natural numbers, the length of the list
;; becomes the size of the step; return the first n numbers of the
;; sequence; assume n >= (length lon)
(define (fib-list lon n)
  (define len (length lon))
  (reverse (for/fold ([lon (reverse lon)]) ([_ (in-range (- n len))])
             (cons (apply + (take lon len)) lon))))

;; Show the series ...
(define (show-fibs name l)
  (printf "~a: " name)
  (for ([n (in-list (fib-list l 20))]) (printf "~a, " n))
  (printf "...\n"))

;; ... with initial 2-powers lists
(for ([n (in-range 2 11)])
  (show-fibs (format "~anacci" (case n [(2) 'fibo] [(3) 'tribo] [(4) 'tetra]
                                     [(5) 'penta] [(6) 'hexa] [(7) 'hepta]
                                     [(8) 'octo] [(9) 'nona] [(10) 'deca]))
             (cons 1 (build-list (sub1 n) (curry expt 2)))))
;; and with an initial (2 1)
(show-fibs "lucas" '(2 1))
```


{{out}}

```txt
fibonacci: 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, ...
tribonacci: 1, 1, 2, 4, 7, 13, 24, 44, 81, 149, 274, 504, 927, 1705, 3136, 5768, 10609, 19513, 35890, 66012, ...
tetranacci: 1, 1, 2, 4, 8, 15, 29, 56, 108, 208, 401, 773, 1490, 2872, 5536, 10671, 20569, 39648, 76424, 147312, ...
pentanacci: 1, 1, 2, 4, 8, 16, 31, 61, 120, 236, 464, 912, 1793, 3525, 6930, 13624, 26784, 52656, 103519, 203513, ...
hexanacci: 1, 1, 2, 4, 8, 16, 32, 63, 125, 248, 492, 976, 1936, 3840, 7617, 15109, 29970, 59448, 117920, 233904, ...
heptanacci: 1, 1, 2, 4, 8, 16, 32, 64, 127, 253, 504, 1004, 2000, 3984, 7936, 15808, 31489, 62725, 124946, 248888, ...
octonacci: 1, 1, 2, 4, 8, 16, 32, 64, 128, 255, 509, 1016, 2028, 4048, 8080, 16128, 32192, 64256, 128257, 256005, ...
nonanacci: 1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 511, 1021, 2040, 4076, 8144, 16272, 32512, 64960, 129792, 259328, ...
decanacci: 1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1023, 2045, 4088, 8172, 16336, 32656, 65280, 130496, 260864, ...
lucas: 2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123, 199, 322, 521, 843, 1364, 2207, 3571, 5778, 9349, ...
```



## REXX


```rexx
/*REXX program  calculates and displays a   N-step   Fibonacci   sequence(s). */
parse arg FibName values               /*allows a Fibonacci name, starter vals*/
if FibName\=''  then do;  call nStepFib  FibName,values;    signal done;    end
                                       /* [↓]  no args specified, show a bunch*/
call  nStepFib  'Lucas'       ,   2 1
call  nStepFib  'fibonacci'   ,   1 1
call  nStepFib  'tribonacci'  ,   1 1 2
call  nStepFib  'tetranacci'  ,   1 1 2 4
call  nStepFib  'pentanacci'  ,   1 1 2 4 8
call  nStepFib  'hexanacci'   ,   1 1 2 4 8 16
call  nStepFib  'heptanacci'  ,   1 1 2 4 8 16 32
call  nStepFib  'octonacci'   ,   1 1 2 4 8 16 32 64
call  nStepFib  'nonanacci'   ,   1 1 2 4 8 16 32 64 128
call  nStepFib  'decanacci'   ,   1 1 2 4 8 16 32 64 128 256
call  nStepFib  'undecanacci' ,   1 1 2 4 8 16 32 64 128 256 512
call  nStepFib  'dodecanacci' ,   1 1 2 4 8 16 32 64 128 256 512 1024
call  nStepFib  '13th-order'  ,   1 1 2 4 8 16 32 64 128 256 512 1024 2048
done:  exit                            /*stick a fork in it,  we're all done. */
/*────────────────────────────────────────────────────────────────────────────*/
nStepFib:  procedure;  parse arg Fname,vals,m;    if m==''  then m=30;      L=
N=words(vals)
                             do pop=1  for N        /*use  N  initial values. */
                             @.pop=word(vals,pop)   /*populate initial numbers*/
                             end   /*pop*/
        do j=1  for m                               /*calculate M Fib numbers.*/
        if j>N  then do;  @.j=0                     /*initialize the sum to 0.*/
                                 do k=j-N  for N    /*sum the last  N numbers.*/
                                 @.j=@.j+@.k        /*add the  [N-j]th number.*/
                                 end   /*k*/
                     end
        L=L  @.j                                    /*append Fib number──►list*/
        end   /*j*/

say right(Fname,11)'[sum'right(N,3)    "terms]:"     strip(L)    '···'
return
```

'''output'''   when using the default input:

```txt

      Lucas[sum  2 terms]: 2 1 3 4 7 11 18 29 47 76 123 199 322 521 843 1364 2207 3571 5778 9349 15127 24476 39603 64079 103682 167761 271443 439204 710647 1149851 ···
  fibonacci[sum  2 terms]: 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657 46368 75025 121393 196418 317811 514229 832040 ···
 tribonacci[sum  3 terms]: 1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136 5768 10609 19513 35890 66012 121415 223317 410744 755476 1389537 2555757 4700770 8646064 15902591 29249425 ···
 tetranacci[sum  4 terms]: 1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536 10671 20569 39648 76424 147312 283953 547337 1055026 2033628 3919944 7555935 14564533 28074040 54114452 104308960 ···
 pentanacci[sum  5 terms]: 1 1 2 4 8 16 31 61 120 236 464 912 1793 3525 6930 13624 26784 52656 103519 203513 400096 786568 1546352 3040048 5976577 11749641 23099186 45411804 89277256 175514464 ···
  hexanacci[sum  6 terms]: 1 1 2 4 8 16 32 63 125 248 492 976 1936 3840 7617 15109 29970 59448 117920 233904 463968 920319 1825529 3621088 7182728 14247536 28261168 56058368 111196417 220567305 ···
 heptanacci[sum  7 terms]: 1 1 2 4 8 16 32 64 127 253 504 1004 2000 3984 7936 15808 31489 62725 124946 248888 495776 987568 1967200 3918592 7805695 15548665 30972384 61695880 122895984 244804400 ···
  octonacci[sum  8 terms]: 1 1 2 4 8 16 32 64 128 255 509 1016 2028 4048 8080 16128 32192 64256 128257 256005 510994 1019960 2035872 4063664 8111200 16190208 32316160 64504063 128752121 256993248 ···
  nonanacci[sum  9 terms]: 1 1 2 4 8 16 32 64 128 256 511 1021 2040 4076 8144 16272 32512 64960 129792 259328 518145 1035269 2068498 4132920 8257696 16499120 32965728 65866496 131603200 262947072 ···
  decanacci[sum 10 terms]: 1 1 2 4 8 16 32 64 128 256 512 1023 2045 4088 8172 16336 32656 65280 130496 260864 521472 1042432 2083841 4165637 8327186 16646200 33276064 66519472 132973664 265816832 ···
undecanacci[sum 11 terms]: 1 1 2 4 8 16 32 64 128 256 512 1024 2047 4093 8184 16364 32720 65424 130816 261568 523008 1045760 2091008 4180992 8359937 16715781 33423378 66830392 133628064 267190704 ···
dodecanacci[sum 12 terms]: 1 1 2 4 8 16 32 64 128 256 512 1024 2048 4095 8189 16376 32748 65488 130960 261888 523712 1047296 2094336 4188160 8375296 16748544 33492993 66977797 133939218 267845688 ···
 13th-order[sum 13 terms]: 1 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8191 16381 32760 65516 131024 262032 524032 1048000 2095872 4191488 8382464 16763904 33525760 67047424 134086657 268156933 ···

```



## Ring


```ring

# Project : Fibonacci n-step number sequences

f = list(12)

see "Fibonacci:" + nl
f2  = [1,1]
for nr2 = 1 to 10
    see "" + f2[1] + " "
    fibn(f2)
next
showarray(f2)
see " ..." + nl + nl

see "Tribonacci:" + nl
f3 = [1,1,2]
for nr3 = 1 to 9
    see "" + f3[1] + " "
    fibn(f3)
next
showarray(f3)
see " ..." + nl + nl

see "Tetranacci:" + nl
f4 = [1,1,2,4]
for nr4 = 1 to 8
    see "" + f4[1] + " "
    fibn(f4)
next
showarray(f4)
see " ..." + nl + nl

see "Lucas:" + nl
f5 = [2,1]
for nr5 = 1 to 10
    see "" + f5[1] + " "
    fibn(f5)
next
showarray(f5)
see " ..." + nl + nl

func fibn(fs)
     s = sum(fs)
     for i = 2 to len(fs)
         fs[i-1] = fs[i]
     next
     fs[i-1] = s
     return fs

func sum(arr)
     sm = 0
     for sn = 1 to len(arr)
         sm = sm + arr[sn]
     next
     return sm

func showarray(fn)
     svect = ""
     for p = 1 to len(fn)
         svect = svect + fn[p] + " "
     next
     see svect

```

Output:

```txt

Fibonacci:
1 1 2 3 5 8 13 21 34 55 89 144  ...

Tribonacci:
1 1 2 4 7 13 24 44 81 149 274 504  ...

Tetranacci:
1 1 2 4 8 15 29 56 108 208 401 773  ...

Lucas:
2 1 3 4 7 11 18 29 47 76 123 199  ...

```



## Ruby

{{works with|Ruby|1.9}}

```ruby
def anynacci(start_sequence, count)
  n = start_sequence.length    # Get the n-step for the type of fibonacci sequence
  result = start_sequence.dup  # Create a new result array with the values copied from the array that was passed by reference

  (n+1..count).each do         # Loop for the remaining results up to count
    result << result.last(n).reduce(:+)    # Get the last n element from result and append its total to Array
  end

  result                       # Return result
end

naccis = { lucus:      [2,1],
           fibonacci:  [1,1],
           tribonacci: [1,1,2],
           tetranacci: [1,1,2,4],
           pentanacci: [1,1,2,4,8],
           hexanacci:  [1,1,2,4,8,16],
           heptanacci: [1,1,2,4,8,16,32],
           octonacci:  [1,1,2,4,8,16,32,64],
           nonanacci:  [1,1,2,4,8,16,32,64,128],
           decanacci:  [1,1,2,4,8,16,32,64,128,256] }

def print_nacci(naccis, count=15)
  puts naccis.map {|name, seq| "%12s : %p" % [name, anynacci(seq, count)]}
end

print_nacci(naccis)
```


{{out}}

```ruby
       lucus : [2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123, 199, 322, 521, 843]
   fibonacci : [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610]
  tribonacci : [1, 1, 2, 4, 7, 13, 24, 44, 81, 149, 274, 504, 927, 1705, 3136]
  tetranacci : [1, 1, 2, 4, 8, 15, 29, 56, 108, 208, 401, 773, 1490, 2872, 5536]
  pentanacci : [1, 1, 2, 4, 8, 16, 31, 61, 120, 236, 464, 912, 1793, 3525, 6930]
   hexanacci : [1, 1, 2, 4, 8, 16, 32, 63, 125, 248, 492, 976, 1936, 3840, 7617]
  heptanacci : [1, 1, 2, 4, 8, 16, 32, 64, 127, 253, 504, 1004, 2000, 3984, 7936]
   octonacci : [1, 1, 2, 4, 8, 16, 32, 64, 128, 255, 509, 1016, 2028, 4048, 8080]
   nonanacci : [1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 511, 1021, 2040, 4076, 8144]
   decanacci : [1, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1023, 2045, 4088, 8172]

```



## Run BASIC

{{incomplete|Run BASIC|Lucas?}}

```runbasic
a = fib("1,1")
a = fib("1,1,2")
a = fib("1,1,2,4")
a = fib("1,1,2,4,8")
a = fib("1,1,2,4,8,16")

function fib(f$)
dim f(20)
while word$(f$,b+1,",") <> ""
 b	= b + 1
 f(b)	= val(word$(f$,b,","))
wend
PRINT "Fibonacci:";b;"=>";
for i = b to 13 + b
  print f(i-b+1);",";
  for j = (i - b) + 1 to i
    f(i+1) = f(i+1) + f(j)
  next j
next i
print
end function
```


```txt
Fibonacci:2=>1,1,2,3,5,8,13,21,34,55,89,144,233,377,
Fibonacci:3=>1,1,2,4,7,13,24,44,81,149,274,504,927,1705,
Fibonacci:4=>1,1,2,4,8,15,29,56,108,208,401,773,1490,2872,
Fibonacci:5=>1,1,2,4,8,16,31,61,120,236,464,912,1793,3525,
Fibonacci:6=>1,1,2,4,8,16,32,63,125,248,492,976,1936,3840,
```



## Rust


```rust

struct GenFibonacci {
    buf:    Vec<u64>,
    sum:    u64,
    idx:    usize,
}

impl Iterator for GenFibonacci {
    type Item = u64;
    fn next(&mut self) -> Option<u64> {
        let result = Some(self.sum);
        self.sum -= self.buf[self.idx];
        self.buf[self.idx] += self.sum;
        self.sum += self.buf[self.idx];
        self.idx = (self.idx + 1) % self.buf.len();
        result
    }
}

fn print(buf: Vec<u64>, len: usize) {
    let mut sum = 0;
    for &elt in buf.iter() { sum += elt; print!("\t{}", elt); }
    let iter = GenFibonacci { buf: buf, sum: sum, idx: 0 };
    for x in iter.take(len) {
        print!("\t{}", x);
    }
}


fn main() {
    print!("Fib2:");
    print(vec![1,1], 10 - 2);

    print!("\nFib3:");
    print(vec![1,1,2], 10 - 3);

    print!("\nFib4:");
    print(vec![1,1,2,4], 10 - 4);

    print!("\nLucas:");
    print(vec![2,1], 10 - 2);
}

```


<lang>
Fib2:	1	1	2	3	5	8	13	21	34	55
Fib3:	1	1	2	4	7	13	24	44	81	149
Fib4:	1	1	2	4	8	15	29	56	108	208
Lucas:	2	1	3	4	7	11	18	29	47	76

```



## Scala


### Simple Solution


```scala

//we rely on implicit conversion from Int to BigInt.
//BigInt is preferable since the numbers get very big, very fast.
//(though for a small example of the first few numbers it's not needed)
def fibStream(init: BigInt*): Stream[BigInt] = {
  def inner(prev: Vector[BigInt]): Stream[BigInt] = prev.head #:: inner(prev.tail :+ prev.sum)
  inner(init.toVector)
}

```



### Optimizing


```scala

//in the optimized version we don't compute values until it's needed.
//the unoptimized version, computed k elements ahead, where k being
//the number of elements to sum (fibonacci: k=2, tribonacci: k=3, ...).
def fibStream(init: BigInt*): Stream[BigInt] = {
  def inner(prev: Vector[BigInt]): Stream[BigInt] = {
    val sum = prev.sum
    sum #:: inner(prev.tail :+ sum)
  }
  init.toStream #::: inner(init.toVector)
}

```


### Optimizing Further


```scala

//instead of summing k elements each phase, we exploit the fact
//that the last element is already the sum of all k preceding elements
def fib2Stream(init: BigInt*): Stream[BigInt] = {
  def inner(prev: Vector[BigInt]): Stream[BigInt] = {
    val n = prev.last * 2 - prev.head
    n #:: inner(prev.tail :+ n)
  }
  //last element must be the sum of k preceding elements, vector size should be k+1
  val v = init.toVector :+ init.sum
  v.toStream #::: inner(v)
}

```


### Printing


```scala

println(s"Fibonacci:  ${fibStream(1,1).take(10).mkString(",")}")
println(s"Tribonacci: ${fibStream(1,1,2).take(10).mkString(",")}")
println(s"Tetranacci: ${fibStream(1,1,2,4).take(10).mkString(",")}")
println(s"Lucas:      ${fibStream(2,1).take(10).mkString(",")}")

```

{{out}}

```txt

Fibonacci:  1,1,2,3,5,8,13,21,34,55
Tribonacci: 1,1,2,4,7,13,24,44,81,149
Tetranacci: 1,1,2,4,8,15,29,56,108,208
Lucas:      2,1,3,4,7,11,18,29,47,76

```

'''Note:''' In Scala, ''Stream'' is a lazy list. if you don't need the sequence saved in memory, just to iterate over members, you may convert the logic to use ''Iterator'' instead of ''Stream''.


## Scheme



```scheme

(import (scheme base)
        (scheme write)
        (srfi 1))

;; uses n-step sequence formula to
;; continue lst until of length num
(define (n-fib lst num)
  (let ((n (length lst)))
    (do ((result (reverse lst)
                 (cons (fold + 0 (take result n))
                       result)))
      ((= num (length result)) (reverse result)))))

;; display examples
(do ((i 2 (+ 1 i)))
  ((> i 4) )
  (display (string-append "n = "
                          (number->string i)
                          ": "))
  (display (n-fib (cons 1 (list-tabulate (- i 1) (lambda (n) (expt 2 n))))
                  15))
  (newline))

(display "Lucas: ")
(display (n-fib '(2 1) 15))
(newline)

```


{{out}}

```txt

n = 2: (1 1 2 3 5 8 13 21 34 55 89 144 233 377 610)
n = 3: (1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136)
n = 4: (1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536)
Lucas: (2 1 3 4 7 11 18 29 47 76 123 199 322 521 843)

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func array integer: bonacci (in array integer: start, in integer: arity, in integer: length) is func
  result
    var array integer: bonacciSequence is 0 times 0;
  local
    var integer: sum is 0;
    var integer: index is 0;
  begin
    bonacciSequence := start[.. length];
    while length(bonacciSequence) < length do
      sum := 0;
      for index range max(1, length(bonacciSequence) - arity + 1) to length(bonacciSequence) do
        sum +:= bonacciSequence[index];
      end for;
      bonacciSequence &:= [] (sum);
    end while;
  end func;

const proc: print (in string: name, in array integer: sequence) is func
  local
    var integer: index is 0;
  begin
    write((name <& ":") rpad 12);
    for index range 1 to pred(length(sequence)) do
      write(sequence[index] <& ", ");
    end for;
    writeln(sequence[length(sequence)]);
  end func;

const proc: main is func
  begin
    print("Fibonacci",  bonacci([] (1, 1), 2, 10));
    print("Tribonacci", bonacci([] (1, 1), 3, 10));
    print("Tetranacci", bonacci([] (1, 1), 4, 10));
    print("Lucas",      bonacci([] (2, 1), 2, 10));
  end func;
```


{{out}}

```txt

Fibonacci:  1, 1, 2, 3, 5, 8, 13, 21, 34, 55
Tribonacci: 1, 1, 2, 4, 7, 13, 24, 44, 81, 149
Tetranacci: 1, 1, 2, 4, 8, 15, 29, 56, 108, 208
Lucas:      2, 1, 3, 4, 7, 11, 18, 29, 47, 76

```



## Sidef

{{trans|Perl}}

```ruby
func fib(n, xs=[1]) {
    loop {
        var len = xs.len
        len >= 20 && break
        xs.append(xs.ft(0.max(len - n)).sum)
    }
    return xs
}

for i in (2..10) {
    say fib(i).join(' ')
}
say fib(2, [2, 1]).join(' ')
```

{{out}}

```txt

1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765
1 1 2 4 7 13 24 44 81 149 274 504 927 1705 3136 5768 10609 19513 35890 66012
1 1 2 4 8 15 29 56 108 208 401 773 1490 2872 5536 10671 20569 39648 76424 147312
1 1 2 4 8 16 31 61 120 236 464 912 1793 3525 6930 13624 26784 52656 103519 203513
1 1 2 4 8 16 32 63 125 248 492 976 1936 3840 7617 15109 29970 59448 117920 233904
1 1 2 4 8 16 32 64 127 253 504 1004 2000 3984 7936 15808 31489 62725 124946 248888
1 1 2 4 8 16 32 64 128 255 509 1016 2028 4048 8080 16128 32192 64256 128257 256005
1 1 2 4 8 16 32 64 128 256 511 1021 2040 4076 8144 16272 32512 64960 129792 259328
1 1 2 4 8 16 32 64 128 256 512 1023 2045 4088 8172 16336 32656 65280 130496 260864
2 1 3 4 7 11 18 29 47 76 123 199 322 521 843 1364 2207 3571 5778 9349

```



## Tailspin


```tailspin

templates fibonacciNstep@{N:}
  templates next
    @: $(1);
    $(2..-1)... -> @: $ + $@;
    [ $(2..-1)..., $@ ] !
  end next

  @: $;
  1..$N -> #
  <>
    $@(1) !
    @: $@ -> next;
end fibonacciNstep

[1,1] -> fibonacciNstep@{N:10} -> '$; ' -> !OUT::write
'
' -> !OUT::write

[1,1,2] -> fibonacciNstep@{N:10} -> '$; ' -> !OUT::write
'
' -> !OUT::write

[1,1,2,4] -> fibonacciNstep@{N:10} -> '$; ' -> !OUT::write
'
' -> !OUT::write

[2,1] -> fibonacciNstep@{N:10} -> '$; ' -> !OUT::write
'
' -> !OUT::write

```

{{out}}

```txt

1 1 2 3 5 8 13 21 34 55
1 1 2 4 7 13 24 44 81 149
1 1 2 4 8 15 29 56 108 208
2 1 3 4 7 11 18 29 47 76

```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

proc fibber {args} {
    coroutine fib[incr ::fibs]=[join $args ","] apply {fn {
	set n [info coroutine]
	foreach f $fn {
	    if {![yield $n]} return
	    set n $f
	}
	while {[yield $n]} {
	    set fn [linsert [lreplace $fn 0 0] end [set n [+ {*}$fn]]]
	}
    } ::tcl::mathop} $args
}

proc print10 cr {
    for {set i 1} {$i <= 10} {incr i} {
	lappend out [$cr true]
    }
    puts \[[join [lappend out ...] ", "]\]
    $cr false
}
puts "FIBONACCI"
print10 [fibber 1 1]
puts "TRIBONACCI"
print10 [fibber 1 1 2]
puts "TETRANACCI"
print10 [fibber 1 1 2 4]
puts "LUCAS"
print10 [fibber 2 1]
```

{{out}}

```txt

FIBONACCI
[1, 1, 2, 3, 5, 8, 13, 21, 34, 55, ...]
TRIBONACCI
[1, 1, 2, 4, 7, 13, 24, 44, 81, 149, ...]
TETRANACCI
[1, 1, 2, 4, 8, 15, 29, 56, 108, 208, ...]
LUCAS
[2, 1, 3, 4, 7, 11, 18, 29, 47, 76, ...]

```



## VBA


```vb
Option Explicit

Sub Main()
Dim temp$, T() As Long, i&
    'Fibonacci:
    T = Fibonacci_Step(1, 15, 1)
    For i = LBound(T) To UBound(T)
        temp = temp & ", " & T(i)
    Next
    Debug.Print "Fibonacci: " & Mid(temp, 3)
    temp = ""

    'Tribonacci:
    T = Fibonacci_Step(1, 15, 2)
    For i = LBound(T) To UBound(T)
        temp = temp & ", " & T(i)
    Next
    Debug.Print "Tribonacci: " & Mid(temp, 3)
    temp = ""

    'Tetranacci:
    T = Fibonacci_Step(1, 15, 3)
    For i = LBound(T) To UBound(T)
        temp = temp & ", " & T(i)
    Next
    Debug.Print "Tetranacci: " & Mid(temp, 3)
    temp = ""

    'Lucas:
    T = Fibonacci_Step(1, 15, 1, 2)
    For i = LBound(T) To UBound(T)
        temp = temp & ", " & T(i)
    Next
    Debug.Print "Lucas: " & Mid(temp, 3)
    temp = ""
End Sub

Private Function Fibonacci_Step(First As Long, Count As Long, S As Long, Optional Second As Long) As Long()
Dim T() As Long, R() As Long, i As Long, Su As Long, C As Long

    If Second <> 0 Then S = 1
    ReDim T(1 - S To Count)
    For i = LBound(T) To 0
        T(i) = 0
    Next i
    T(1) = IIf(Second <> 0, Second, 1)
    T(2) = 1
    For i = 3 To Count
        Su = 0
        C = S + 1
        Do While C >= 0
            Su = Su + T(i - C)
            C = C - 1
        Loop
        T(i) = Su
    Next
    ReDim R(1 To Count)
    For i = 1 To Count
        R(i) = T(i)
    Next
    Fibonacci_Step = R
End Function
```


{{Out}}

```txt
Fibonacci: 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610
Tribonacci: 1, 1, 2, 4, 7, 13, 24, 44, 81, 149, 274, 504, 927, 1705, 3136
Tetranacci: 1, 1, 2, 4, 8, 15, 29, 56, 108, 208, 401, 773, 1490, 2872, 5536
Lucas: 2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123, 199, 322, 521, 843
```




## VBScript


```vb

'function arguments:
'init - initial series of the sequence(e.g. "1,1")
'rep - how many times the sequence repeats - init
Function generate_seq(init,rep)
	token = Split(init,",")
	step_count = UBound(token)
	rep = rep - (UBound(token) + 1)
	out = init
	For i = 1 To rep
		sum = 0
		n = step_count
		Do While n >= 0
			sum = sum + token(UBound(token)-n)
			n = n - 1
		Loop
		'add the next number to the sequence
		ReDim Preserve token(UBound(token) + 1)
		token(UBound(token)) = sum
		out = out & "," & sum
	Next
	generate_seq = out
End Function

WScript.StdOut.Write "fibonacci: " & generate_seq("1,1",15)
WScript.StdOut.WriteLine
WScript.StdOut.Write "tribonacci: " & generate_seq("1,1,2",15)
WScript.StdOut.WriteLine
WScript.StdOut.Write "tetranacci: " & generate_seq("1,1,2,4",15)
WScript.StdOut.WriteLine
WScript.StdOut.Write "lucas: " & generate_seq("2,1",15)
WScript.StdOut.WriteLine

```


{{Out}}

```txt

fibonacci: 1,1,2,3,5,8,13,21,34,55,89,144,233,377,610
tribonacci: 1,1,2,4,7,13,24,44,81,149,274,504,927,1705,3136
tetranacci: 1,1,2,4,8,15,29,56,108,208,401,773,1490,2872,5536
lucas: 2,1,3,4,7,11,18,29,47,76,123,199,322,521,843

```



## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations

proc Nacci(N, F0);              \Generate Fibonacci N-step sequence
int N,                          \step size
    F0;                         \array of first N values
int I, J;
def M = 10;                     \number of members in the sequence
int F(M);                       \Fibonacci sequence
[for I:= 0 to M-1 do            \for all the members of the sequence...
    [if I < N then F(I):= F0(I) \initialize sequence
    else [F(I):= 0;             \sum previous members to get member I
         for J:= 1 to N do F(I):= F(I) + F(I-J);
         ];
    IntOut(0, F(I)); ChOut(0, ^ );
    ];
CrLf(0);
];

[Text(0, " Fibonacci: ");  Nacci(2, [1, 1]);
 Text(0, "Tribonacci: ");  Nacci(3, [1, 1, 2]);
 Text(0, "Tetranacci: ");  Nacci(4, [1, 1, 2, 4]);
 Text(0, "     Lucas: ");  Nacci(2, [2, 1]);
]
```


Output:

```txt

 Fibonacci: 1 1 2 3 5 8 13 21 34 55
Tribonacci: 1 1 2 4 7 13 24 44 81 149
Tetranacci: 1 1 2 4 8 15 29 56 108 208
     Lucas: 2 1 3 4 7 11 18 29 47 76

```



## Yabasic

{{trans|Lua}}

```Yabasic
sub nStepFibs$(seq$, limit)
    local iMax, sum, numb$(1), lim, i

    lim = token(seq$, numb$(), ",")
    redim numb$(limit)
    seq$ = ""
    iMax = lim - 1
    while(lim < limit)
        sum = 0
        for i = 0 to iMax : sum = sum + val(numb$(lim - i)) : next
        lim = lim + 1
        numb$(lim) = str$(sum)
    wend
    for i = 0 to lim : seq$ = seq$ + " " + numb$(i) : next
    return seq$
end sub

print "Fibonacci:", nStepFibs$("1,1", 10)
print "Tribonacci:", nStepFibs$("1,1,2", 10)
print "Tetranacci:", nStepFibs$("1,1,2,4", 10)
print "Lucas:", nStepFibs$("2,1", 10)
```



## zkl


```zkl
fcn fibN(ns){ fcn(ns){ ns.append(ns.sum()).pop(0) }.fp(vm.arglist.copy()); }
```

This stores the initial n terms of the sequence and returns a function that, at each call, appends the sum of the terms to the sequence then pops the leading value and returns it.

```zkl
N:=15;
lucas:=fibN(2,1); do(N){ lucas().print(","); } println();  // Lucas
ns:=L(1); foreach _ in ([ns.len()+1..10]){ // Fibonacci n-step for 2 .. 10
   ns.append(ns.sum());  // the inital values for the series
   "%2d: ".fmt(ns.len()).print();
   (N).pump(List,fibN(ns.xplode())).println();
}
```

{{out}}

```txt

2,1,3,4,7,11,18,29,47,76,123,199,322,521,843,
 2: L(1,1,2,3,5,8,13,21,34,55,89,144,233,377,610)
 3: L(1,1,2,4,7,13,24,44,81,149,274,504,927,1705,3136)
 4: L(1,1,2,4,8,15,29,56,108,208,401,773,1490,2872,5536)
 5: L(1,1,2,4,8,16,31,61,120,236,464,912,1793,3525,6930)
 6: L(1,1,2,4,8,16,32,63,125,248,492,976,1936,3840,7617)
 7: L(1,1,2,4,8,16,32,64,127,253,504,1004,2000,3984,7936)
 8: L(1,1,2,4,8,16,32,64,128,255,509,1016,2028,4048,8080)
 9: L(1,1,2,4,8,16,32,64,128,256,511,1021,2040,4076,8144)
10: L(1,1,2,4,8,16,32,64,128,256,512,1023,2045,4088,8172)

```

