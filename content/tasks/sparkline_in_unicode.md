+++
title = "Sparkline in unicode"
description = ""
date = 2019-03-01T11:35:59Z
aliases = []
[extra]
id = 14267
[taxonomies]
categories = ["task"]
tags = []
+++

A [[wp:Sparkline|sparkline]] is a graph of successive values laid out horizontally 
where the height of the line is proportional to the values in succession.


## Task

Use the following series of Unicode characters to create a program 
that takes a series of numbers separated by one or more whitespace or comma characters 
and generates a sparkline-type bar graph of the values on a single line of output.

The eight characters: <code>'▁▂▃▄▅▆▇█'</code>

(Unicode values U+2581 through U+2588).

Use your program to show sparklines for the following input, 
here on this page:
# 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
# 1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5 
:(note the mix of separators in this second case)!

;Notes: 
* A space is not part of the generated sparkline.
* The sparkline may be accompanied by simple statistics of the data such as its range.
* A suggestion emerging in later discussion (see [[Talk:Sparkline_in_unicode|Discussion]] page) is that the bounds between bins should ideally be set to yield the following results for two particular edge cases:

:: <code>"0, 1, 19, 20" -> ▁▁██</code>
:: (Aiming to use just two spark levels)

:: <code>"0, 999, 4000, 4999, 7000, 7999" -> ▁▁▅▅██</code>
:: (Aiming to use just three spark levels)

:: It may be helpful to include these cases in output tests.
* You may find that the unicode sparklines on this page are rendered less noisily by Google Chrome than by Firefox or Safari.





## APL

Note → this is in a 0-indexed version of APL
'''Solution''':
```APL
   sparkln←{'▁▂▃▄▅▆▇█'[⌊0.5+7×⍵÷⌈/⍵]}
```

'''Example''':
```APL
      sparkln 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
▂▃▄▅▅▆▇█▇▆▅▅▄▃▂
      sparkln 1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5
▂▁▄▃▆▅█▇

```

Note → APL accepts the input with commas and spaces naturally.  If one wanted to read input as a string they could use ⍎⍞ to do so.



## AppleScript


```AppleScript
use AppleScript version "2.4"
use framework "Foundation"
use scripting additions

on run
    unlines(map(¬
        compose(compose(unlines, sparkLine), readFloats), ¬
        {"0, 1, 19, 20", "0, 999, 4000, 4999, 7000, 7999", ¬
            "0, 1000, 4000, 5000, 7000, 8000", ¬
            "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1", ¬
            "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5"}))
end run


-- sparkLine :: [Float] -> [String]
on sparkLine(xs)
    set ys to sort(xs)
    set mn to item 1 of ys
    set mx to item -1 of ys
    set n to length of xs
    set mid to (n div 2)
    set w to (mx - mn) / 8
    
    script bound
        on |λ|(x)
            mn + (w * x)
        end |λ|
    end script
    set lbounds to map(bound, enumFromTo(1, 7))
    
    script spark
        on |λ|(x)
            script flipGT
                on |λ|(b)
                    b > x
                end |λ|
            end script
            script indexedBlock
                on |λ|(i)
                    item i of "▁▂▃▄▅▆▇"
                end |λ|
            end script
            maybe("█", indexedBlock, findIndex(flipGT, lbounds))
        end |λ|
    end script
    
    script str
        on |λ|(x)
            x as string
        end |λ|
    end script
    
    {concat(map(spark, xs)), ¬
        unwords(map(str, xs)), ¬
        "Min " & mn as string, ¬
        "Mean " & roundTo(mean(xs), 2) as string, ¬
        "Median " & bool(item mid of xs, ((item mid of xs) + ¬
        (item (mid + 1) of xs)) / 2, even(n)), ¬
        "Max " & mx as string, ""}
end sparkLine


-- GENERIC -------------------------------------------------

-- Just :: a -> Maybe a
on Just(x)
    {type:"Maybe", Nothing:false, Just:x}
end Just

-- Nothing :: Maybe a
on Nothing()
    {type:"Maybe", Nothing:true}
end Nothing

-- bool :: a -> a -> Bool -> a
on bool(f, t, p)
    if p then
        t
    else
        f
    end if
end bool

-- compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
on compose(f, g)
    script
        property mf : mReturn(f)
        property mg : mReturn(g)
        on |λ|(x)
            mf's |λ|(mg's |λ|(x))
        end |λ|
    end script
end compose

-- concat :: [[a]] -> [a]
-- concat :: [String] -> String
on concat(xs)
    set lng to length of xs
    if 0 < lng and string is class of (item 1 of xs) then
        set acc to ""
    else
        set acc to {}
    end if
    repeat with i from 1 to lng
        set acc to acc & item i of xs
    end repeat
    acc
end concat

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m ≤ n then
        set lst to {}
        repeat with i from m to n
            set end of lst to i
        end repeat
        return lst
    else
        return {}
    end if
end enumFromTo

-- even :: Int -> Bool
on even(x)
    0 = x mod 2
end even

-- Takes a predicate function and a list and
-- returns Just( the 1-based index of the first
-- element ) in the list satisfying the predicate
-- or Nothing if there is no such element.
-- findIndex(isSpace, "hello world")
--> {type:"Maybe", Nothing:false, Just:6}

-- findIndex(even, [3, 5, 7, 8, 9])
--> {type:"Maybe", Nothing:false, Just:4}

-- findIndex(isUpper, "all lower case")
--> {type:"Maybe", Nothing:true}
-- findIndex :: (a -> Bool) -> [a] -> Maybe Int
on findIndex(p, xs)
    tell mReturn(p)
        set lng to length of xs
        repeat with i from 1 to lng
            if |λ|(item i of xs) then return Just(i)
        end repeat
        return Nothing()
    end tell
end findIndex

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

-- mean :: [Num] -> Num
on mean(xs)
    script
        on |λ|(a, x)
            a + x
        end |λ|
    end script
    foldl(result, 0, xs) / (length of xs)
end mean

-- | The 'maybe' function takes a default value, a function, and a 'Maybe'
-- value.  If the 'Maybe' value is 'Nothing', the function returns the
-- default value.  Otherwise, it applies the function to the value inside
-- the 'Just' and returns the result.
-- maybe :: b -> (a -> b) -> Maybe a -> b
on maybe(v, f, mb)
    if Nothing of mb then
        v
    else
        tell mReturn(f) to |λ|(Just of mb)
    end if
end maybe

-- Lift 2nd class handler function into 1s class script wrapper 
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if script is class of f then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- readFloats :: String -> [Float]
on readFloats(s)
    script asReal
        on |λ|(n)
            n as real
        end |λ|
    end script
    map(asReal, splitRegex("[\\s,]+", s))
end readFloats

-- regexMatches :: String -> String -> [[String]]
on regexMatches(strRegex, strHay)
    set ca to current application
    -- NSNotFound handling and and High Sierra workaround due to @sl1974
    set NSNotFound to a reference to 9.22337203685477E+18 + 5807
    set oRgx to ca's NSRegularExpression's regularExpressionWithPattern:strRegex ¬
        options:((ca's NSRegularExpressionAnchorsMatchLines as integer)) ¬
        |error|:(missing value)
    set oString to ca's NSString's stringWithString:strHay
    
    script matchString
        on |λ|(m)
            script rangeMatched
                on |λ|(i)
                    tell (m's rangeAtIndex:i)
                        set intFrom to its location
                        if NSNotFound ≠ intFrom then
                            text (intFrom + 1) thru (intFrom + (its |length|)) of strHay
                        else
                            missing value
                        end if
                    end tell
                end |λ|
            end script
        end |λ|
    end script
    
    script asRange
        on |λ|(x)
            range() of x
        end |λ|
    end script
    map(asRange, (oRgx's matchesInString:oString ¬
        options:0 range:{location:0, |length|:oString's |length|()}) as list)
end regexMatches


-- roundTo :: Float -> Int -> Float
on roundTo(x, n)
    set d to 10 ^ n
    (round (x * d)) / d
end roundTo

-- sort :: Ord a => [a] -> [a]
on sort(xs)
    ((current application's NSArray's arrayWithArray:xs)'s ¬
        sortedArrayUsingSelector:"compare:") as list
end sort

-- splitRegex :: Regex -> String -> [String]
on splitRegex(strRegex, str)
    set lstMatches to regexMatches(strRegex, str)
    if length of lstMatches > 0 then
        script preceding
            on |λ|(a, x)
                set iFrom to start of a
                set iLocn to (location of x)
                
                if iLocn > iFrom then
                    set strPart to text (iFrom + 1) thru iLocn of str
                else
                    set strPart to ""
                end if
                {parts:parts of a & strPart, start:iLocn + (length of x) - 1}
            end |λ|
        end script
        
        set recLast to foldl(preceding, {parts:[], start:0}, lstMatches)
        
        set iFinal to start of recLast
        if iFinal < length of str then
            parts of recLast & text (iFinal + 1) thru -1 of str
        else
            parts of recLast & ""
        end if
    else
        {str}
    end if
end splitRegex

-- unlines :: [String] -> String
on unlines(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, linefeed}
    set str to xs as text
    set my text item delimiters to dlm
    str
end unlines

-- unwords :: [String] -> String
on unwords(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, space}
    set s to xs as text
    set my text item delimiters to dlm
    return s
end unwords
```

```txt
▁▁██
0.0 1.0 19.0 20.0
Min 0.0
Mean 10.0
Median 10.0
Max 20.0

▁▁▅▅██
0.0 999.0 4000.0 4999.0 7000.0 7999.0
Min 0.0
Mean 4166.17
Median 4499.5
Max 7999.0

▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0 1.0
Min 1.0
Mean 4.27
Median 7.0
Max 8.0

▂▁▄▃▆▅█▇
1.5 0.5 3.5 2.5 5.5 4.5 7.5 6.5
Min 0.5
Mean 4.0
Median 4.0
Max 7.5
```



## AutoHotkey

```AutoHotkey
SetFormat, FloatFast, 0.1
strings := ["1 2 3 4 5 6 7 8 7 6 5 4 3 2 1"
         ,  "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5"]

Loop, % strings.MaxIndex()
{
    SL := Sparklines(strings[A_Index])
    MsgBox, % "Min: " SL["Min"] ", Max: " SL["Max"] ", Range: " SL["Rng"] "`n" SL["Chars"]
}

Sparklines(s)
{
    s := RegexReplace(s, "[^\d\.]+", ",")
    Loop, Parse, s, `,
    {
        Max := A_LoopField > Max ? A_LoopField : Max
        Min := !Min ? Max : A_LoopField < Min ? A_LoopField : Min
    }
    Rng := Max - Min
    Loop, Parse, s, `,
        Chars .= Chr(0x2581 + Round(7 * (A_LoopField - Min) / Rng))
    return, {"Min": Min, "Max": Max, "Rng": Rng, "Chars": Chars}
}
```

```txt
Min: 1, Max: 8, Range: 7
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁

Min: 0.5, Max: 7.5, Range: 7.0
▂▁▄▃▆▅█▇
```



## C

This seemingly simple task turns out to be very complicated for languages like C. As the characters to be printed are Unicode, there is no platform independent way to do so. The implementation below works on Linux but not on Windows, detailed investigations show that there is no way, at least no reliable and/or easily reproducible way, to accomplish this task via C on Windows. This is not due to any lacunae in C but due to the vagaries of the Windows Command shell.

### Linux version

Accepts data via command line, prints out usage on incorrect invocation.

```C

#include<string.h>
#include<stdlib.h>
#include<locale.h>
#include<stdio.h>
#include<wchar.h>
#include<math.h>

int main(int argC,char* argV[])
{
	double* arr,min,max;
	char* str;
	int i,len;
	if(argC == 1)
		printf("Usage : %s <data points separated by spaces or commas>",argV[0]);
	else{
		arr = (double*)malloc((argC-1)*sizeof(double));
		for(i=1;i<argC;i++){
			len = strlen(argV[i]);
			
			if(argV[i][len-1]==','){
				str = (char*)malloc(len*sizeof(char));
				strncpy(str,argV[i],len-1);
				arr[i-1] = atof(str);
				free(str);
			}
			else
				arr[i-1] = atof(argV[i]);
			if(i==1){
				min = arr[i-1];
				max = arr[i-1];
			}
			else{
				min=(min<arr[i-1]?min:arr[i-1]);
				max=(max>arr[i-1]?max:arr[i-1]);
			}
		}
		
		printf("\n%Max : %lf,Min : %lf,Range : %lf\n",max,min,max-min);
		
		setlocale(LC_ALL, "");
		
		for(i=1;i<argC;i++){
			printf("%lc", (wint_t)(9601 + (int)ceil((arr[i-1]-min)/(max-min)*7)));
		}
	}
	return 0;
}

```

Invocation and output :

```txt

/home/aamrun/rosettaCode>./sparkLine 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
Max : 8.000000,Min : 1.000000,Range : 7.000000
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
/home/aamrun/rosettaCode>./sparkLine 1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5
Max : 7.500000,Min : 0.500000,Range : 7.000000
▂▁▄▃▆▅█▇

```



## C++



```cpp

#include <iostream>
#include <sstream>
#include <vector>
#include <cmath>
#include <algorithm>
#include <locale>

class Sparkline {
    public:
        Sparkline(std::wstring &cs) : charset( cs ){
        }
        virtual ~Sparkline(){
        }

        void print(std::string spark){
            const char *delim = ", ";
            std::vector<float> data;
            // Get first non-delimiter
            std::string::size_type last = spark.find_first_not_of(delim, 0);
            // Get end of token
            std::string::size_type pos = spark.find_first_of(delim, last);

            while( pos != std::string::npos || last != std::string::npos ){
                std::string tok = spark.substr(last, pos-last);
                // Convert to float:
                std::stringstream ss(tok);
                float entry;
                ss >> entry;

                data.push_back( entry );

                last = spark.find_first_not_of(delim, pos);
                pos = spark.find_first_of(delim, last);
            }

            // Get range of dataset
            float min = *std::min_element( data.begin(), data.end() );
            float max = *std::max_element( data.begin(), data.end() );

            float skip = (charset.length()-1) / (max - min);

            std::wcout<<L"Min: "<<min<<L"; Max: "<<max<<L"; Range: "<<(max-min)<<std::endl;
            
            std::vector<float>::const_iterator it;
            for(it = data.begin(); it != data.end(); it++){
                float v = ( (*it) - min ) * skip; 
                std::wcout<<charset[ (int)floor( v ) ];
            }
            std::wcout<<std::endl;
            
        }
    private:
        std::wstring &charset;
};

int main( int argc, char **argv ){
    std::wstring charset = L"\u2581\u2582\u2583\u2584\u2585\u2586\u2587\u2588";

    // Mainly just set up utf-8, so wcout won't narrow our characters.
    std::locale::global(std::locale("en_US.utf8"));

    Sparkline sl(charset);

    sl.print("1 2 3 4 5 6 7 8 7 6 5 4 3 2 1");
    sl.print("1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5");

    return 0;
}
```

```txt
Min: 1; Max: 8; Range: 7
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
Min: 0.5; Max: 7.5; Range: 7
▂▁▄▃▆▅█▇
```



## Clojure


```Clojure
(defn sparkline [nums]
  (let [sparks   "▁▂▃▄▅▆▇█"
        high     (apply max nums)
        low      (apply min nums)
        spread   (- high low)
        quantize #(Math/round (* 7.0 (/ (- % low) spread)))]
        (apply str (map #(nth sparks (quantize %)) nums))))

(defn spark [line]
  (if line
    (let [nums (read-string (str "[" line "]"))] 
      (println (sparkline nums))
      (recur (read-line)))))

(spark (read-line))
```


```txt
$ clj sparkline.clj <<<$'1 2 3 4 5 6 7 8 7 6 5 4 3 2 1\n1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5'
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
▂▁▄▃▆▅█▇

```



## Common Lisp


```lisp
(defun buckets (numbers)
  (loop with min = (apply #'min numbers)
        with max = (apply #'max numbers)
        with width = (/ (- max min) 7)
        for base from (- min (/ width 2)) by width
        repeat 8
        collect (cons base (+ base width))))

(defun bucket-for-number (number buckets)
  (loop for i from 0
        for (min . max) in buckets
        when (and (<= min number) (< number max))
          return i))

(defun sparkline (numbers)
  (loop with buckets = (buckets numbers)
        with sparks = "▁▂▃▄▅▆▇█"
        with sparkline = (make-array (length numbers) :element-type 'character)
        with min = (apply #'min numbers)
        with max = (apply #'max numbers)
        for number in numbers
        for i from 0
        for bucket = (bucket-for-number number buckets)
        do (setf (aref sparkline i) (char sparks bucket))
        finally (format t "Min: ~A, Max: ~A, Range: ~A~%" min max (- max min))
                (write-line sparkline)))

(defun string->numbers (string)
  (flet ((delimiterp (c)
           (or (char= c #\Space) (char= c #\,))))
    (loop for prev-end = 0 then end
          while prev-end
          for start = (position-if-not #'delimiterp string :start prev-end)
          for end = (position-if #'delimiterp string :start start)
          for number = (read-from-string string t nil :start start :end end)
          do (assert (numberp number))
          collect number)))

(defun string->sparkline (string)
  (sparkline (string->numbers string)))

(string->sparkline "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1")
(string->sparkline "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5")
```

```txt
Min: 1, Max: 8, Range: 7
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
Min: 0.5, Max: 7.5, Range: 7.0
▂▁▄▃▆▅█▇
```



## D

```d
void main() {
    import std.stdio, std.range, std.algorithm, std.conv,
           std.string, std.regex;

    "Numbers please separated by space/commas: ".write;
    immutable numbers = readln
                        .strip
                        .splitter(r"[\s,]+".regex)
                        .array /**/
                        .to!(real[]);
    immutable mm = numbers.reduce!(min, max);
    writefln("min: %4f, max: %4f", mm[]);
    immutable bars = iota(9601, 9609).map!(i => i.to!dchar).dtext;
    immutable div = (mm[1] - mm[0]) / (bars.length - 1);
    numbers.map!(n => bars[cast(int)((n - mm[0]) / div)]).writeln;
}
```

The output is the same as the Python entry 
(but it only accepts one series of values at a time).


## Elixir


```elixir
defmodule RC do
  def sparkline(str) do
    values = str |> String.split(~r/(,| )+/)
                 |> Enum.map(&elem(Float.parse(&1), 0))
    {min, max} = Enum.min_max(values)
    IO.puts Enum.map(values, &(round((&1 - min) / (max - min) * 7 + 0x2581)))
  end
end
```

Usage:
<lang>str1 = "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1"
str2 = "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5"

RC.sparkline(str1)
IO.puts "" # newline
RC.sparkline(str2)
```

```txt

▁▂▃▄▅▆▇█▇▆▅▄▃▂▁

▂▁▄▃▆▅█▇

```


=={{header|F_Sharp|F#}}==

```fsharp
open System
open System.Globalization
open System.Text.RegularExpressions

let bars = Array.map Char.ToString ("▁▂▃▄▅▆▇█".ToCharArray())

while true do
    printf "Numbers separated by anything: "
    let numbers =
        [for x in Regex.Matches(Console.ReadLine(), @"-?\d+(?:\.\d*)?") do yield x.Value]
        |> List.map (fun x -> Double.Parse(x, CultureInfo.InvariantCulture))
    if numbers.Length = 0 then System.Environment.Exit(0)
    if numbers.Length = 1 then
        printfn "A sparkline for 1 value is not very useful... ignoring entry"
    else
        let min, max = List.min numbers, List.max numbers
        printfn "min: %5f; max: %5f" min max
        let barsCount = float (bars.GetUpperBound(0))
        numbers
        |> List.map (fun x -> bars.[int ((x - min)/(max - min) * barsCount)])
        |> String.Concat
        |> printfn "%s"
```

```txt
Numbers separated by anything: 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1 
min: 1.000000; max: 8.000000
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
Numbers separated by anything: 1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5
min: 0.500000; max: 7.500000
▂▁▄▃▆▅█▇
Numbers separated by anything: 

```



## FALSE


```false
{
  variables:
  s: sign (1 or -1)
  u: current number
  f: current number fraction length
  v: current number is valid
  t: number of numbers read
  x: biggest fraction
  y: smallest number (without fraction)
  z: biggest number (without fraction)
}

{function a: test if top is 0-9, without popping the value, codes 48-57 are in range}
[$$47>\57>~&]a:

{function b: test if top is ',' or ' ', without popping the value}
[$$',=\' =|]b:

{function c: read a number from the input, given that the first character of the input is already on the stack}
[
  1s:0u:0f:0v: {reset values}

  $'-=[1_s:%^]? {if (it is negative) set the sign value to -1 move to next}
  [a;!][48-u;10*+u:1_v:^]# {while (isnumber) do number = number * 10 + decimal and set valid number and move to next}
  $'.=[ {if (it is a decimal) move forward and read fraction}
    %^
    [a;!][48-u;10*+u:f;1+f:1_v:^]# {while (isnumber) do number = number * 10 + decimal and increase fraction length and set valid number and move to next}
  ]?
  $$'-=\'.=|[0v:]? {if next charachter is a '-' or a '.', set invalid}
]c:

{function d: normalize number/fraction from stack to max fraction and push that number}
[
  [$x;=~][1+\10*\]# {while (fraction != max) fraction + 1, value * 10}
  % {pop fraction}
]d:

0t:
0x:
1_v: { nothing read, so we are still valid }
^[b;!][%^]# {read away any initial separators}
[$1_=~v;&][ {while input != -1 and valid input, leaving input on the stack}
  c;! {read a number}
  t;1+t:u;s;*f;@ {increase count, push number * sign and fraction length onto the stack and bring input back up}
  f;x;>[f;x:]? {set fraction to biggest of current and previous biggest}
  [b;!][%^]# {while (isseparator) move forward}
]#
v;~["error at charachter ",]? {if invalid number, tell them when}
v;[ {if last number also valid, do the math}
  %        {pop the -1}
  t;2*1-q: {var q: points to next value}
  0p:      {var p: whether min/max have been set}
  [q;1+t;>][ {while q + 1 > t}
    q;ø        {current number}
    q;ø        {current fraction}
    d;!        {normalize}
    p;[$y;\>[$y:]? $z;>[$z:]?]?      {compare min/max}
    p;~[1_p:$y:$z:]?     {if (first)) set min/max}
    q;1-q:     {move pointer}
  ]#

  t;q: {point q to first value}
  [q;0>][ {while q > 0}
    q;1-øy;-7*z;y;-/ {(number - minvalue) * 7 / (maxvalue - minvalue), should result in 0..7}
    9601+,           {print character}
    q;1-q:           {move pointer}
  ]#
]?
```

This implementation can only accept one series of numbers at a time.
```txt

▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
▂▁▄▃▆▅█▇

```



## Go


```go
package main

import (
    "bufio"
    "errors"
    "fmt"
    "math"
    "os"
    "regexp"
    "strconv"
    "strings"
)

func main() {
    fmt.Println("Numbers please separated by space/commas:")
    sc := bufio.NewScanner(os.Stdin)
    sc.Scan()
    s, n, min, max, err := spark(sc.Text())
    if err != nil {
        fmt.Println(err)
        return
    }
    if n == 1 {
        fmt.Println("1 value =", min)
    } else {
        fmt.Println(n, "values.  Min:", min, "Max:", max)
    }
    fmt.Println(s)
}

var sep = regexp.MustCompile(`[\s,]+`)

func spark(s0 string) (sp string, n int, min, max float64, err error) {
    ss := sep.Split(s0, -1)
    n = len(ss)
    vs := make([]float64, n)
    var v float64
    min = math.Inf(1)
    max = math.Inf(-1)
    for i, s := range ss {
        switch v, err = strconv.ParseFloat(s, 64); {
        case err != nil:
        case math.IsNaN(v):
            err = errors.New("NaN not supported.")
        case math.IsInf(v, 0):
            err = errors.New("Inf not supported.")
        default:
            if v < min {
                min = v
            }
            if v > max {
                max = v
            }
            vs[i] = v
            continue
        }
        return
    }
    if min == max {
        sp = strings.Repeat("▄", n)
    } else {
        rs := make([]rune, n)
        f := 8 / (max - min)
        for j, v := range vs {
            i := rune(f * (v - min))
            if i > 7 {
                i = 7
            }
            rs[j] = '▁' + i
        }
        sp = string(rs)
    }
    return
}
```

```txt

Numbers please separated by space/commas:
1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
15 values.  Min: 1 Max: 8
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
Numbers please separated by space/commas:
1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5 
8 values.  Min: 0.5 Max: 7.5
▂▁▄▃▆▅█▇

Numbers please separated by space/commas:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
24 values.  Min: 1 Max: 24
▁▁▁▂▂▂▃▃▃▄▄▄▅▅▅▆▆▆▇▇▇███
Numbers please separated by space/commas:
0 99 101 699 701 800
6 values.  Min: 0 Max: 800
▁▁▂▇██
Numbers please separated by space/commas:
0 -.09 -.11 -.69 -.71 -.8
6 values.  Min: -0.8 Max: 0
██▇▂▁▁
Numbers please separated by space/commas:
3 3 3
3 values.  Min: 3 Max: 3
▄▄▄
Numbers please separated by space/commas:
1e99
1 value = 1e+99
▄
Numbers please separated by space/commas:

strconv.ParseFloat: parsing "": invalid syntax

```



## Groovy


```groovy
def sparkline(List<Number> list) {
    def (min, max) = [list.min(), list.max()]
    def div = (max - min) / 7
    list.collect { (char)(0x2581 + (it-min) * div) }.join()
}
def sparkline(String text) { sparkline(text.split(/[ ,]+/).collect { it as Double }) }
```

Test Code

```groovy
["1 2 3 4 5 6 7 8 7 6 5 4 3 2 1", "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5"].each { dataset ->
    println "  Dataset: $dataset"
    println "Sparkline: ${sparkline(dataset)}"
}
```

```txt

  Dataset: 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
Sparkline: ▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
  Dataset: 1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5
Sparkline: ▂▁▄▃▆▅█▇

```



## Haskell


```haskell
import Data.List.Split (splitOneOf)
import Data.Char (chr)


toSparkLine :: [Double] -> String
toSparkLine xs = map cl xs
  where
    top = maximum xs
    bot = minimum xs
    range = top - bot
    cl x = chr $ 0x2581 + floor (min 7 ((x - bot) / range * 8))

makeSparkLine :: String -> (String, Stats)
makeSparkLine xs = (toSparkLine parsed, stats parsed)
  where
    parsed = map read $ filter (not . null) $ splitOneOf " ," xs

data Stats = Stats
  { minValue, maxValue, rangeOfValues :: Double
  , numberOfValues :: Int
  }

instance Show Stats where
  show (Stats mn mx r n) =
    "min: " ++
    show mn ++
    "; max: " ++
    show mx ++ "; range: " ++ show r ++ "; no. of values: " ++ show n

stats :: [Double] -> Stats
stats xs =
  Stats
  { minValue = mn
  , maxValue = mx
  , rangeOfValues = mx - mn
  , numberOfValues = length xs
  }
  where
    mn = minimum xs
    mx = maximum xs

drawSparkLineWithStats :: String -> IO ()
drawSparkLineWithStats xs = putStrLn sp >> print st
  where
    (sp, st) = makeSparkLine xs

main :: IO ()
main =
  mapM_
    drawSparkLineWithStats
    [ "0, 1, 19, 20"
    , "0, 999, 4000, 4999, 7000, 7999"
    , "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1"
    , "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5"
    , "3 2 1 0 -1 -2 -3 -4 -3 -2 -1 0 1 2 3"
    , "-1000 100 1000 500 200 -400 -700 621 -189 3"
    ]
```

```txt
▁▁██
min: 0.0; max: 20.0; range: 20.0; no. of values: 4
▁▁▅▅██
min: 0.0; max: 7999.0; range: 7999.0; no. of values: 6
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
min: 1.0; max: 8.0; range: 7.0; no. of values: 15
▂▁▄▃▆▅█▇
min: 0.5; max: 7.5; range: 7.0; no. of values: 8
█▇▆▅▄▃▂▁▂▃▄▅▆▇█
min: -4.0; max: 3.0; range: 7.0; no. of values: 15
▁▅█▇▅▃▂▇▄▅
min: -1000.0; max: 1000.0; range: 2000.0; no. of values: 10
```



Or, stripping back a little:
```haskell
import Data.List.Split (splitOneOf)
import Data.List (findIndex)
import Data.Maybe (maybe)
import Control.Arrow ((&&&))

sparkLine :: [Float] -> String
sparkLine xs =
  let (mn, mx) = (minimum &&& maximum) xs
      w = (mx - mn) / 8
      lbounds = ((mn +) . (w *)) <$> [1 .. 7]
  in fmap (maybe '█' ("▁▂▃▄▅▆▇" !!) . flip findIndex lbounds . flip (>)) xs

parseFloats :: String -> [Float]
parseFloats = fmap read . filter (not . null) . splitOneOf " ,"

main :: IO ()
main =
  mapM_
    putStrLn
    ((sparkLine . parseFloats) <$>
     [ "0, 1, 19, 20"
     , "0, 999, 4000, 4999, 7000, 7999"
     , "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1"
     , "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5"
     ])
```

```txt
▁▁██
▁▁▅▅██
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
▂▁▄▃▆▅█▇
```



## J

'''Solution''' (''explicit''):
```j
   spkln =: verb define
   	y spkln~ 4 u:16b2581+i.8  NB.  ▁▂▃▄▅▆▇█
   :
   	'MIN MAX' =. (<./ , >./) y
   	N         =. # x
   	x {~ <. (N-1) * (y-MIN) % MAX-MIN
   )
```

'''Solution''' (''tacit''):
```j
   spkln =: (4 u:16b2581+i.8)&$: : ([ {~ <:@#@[ <.@* ] (- % >./@[ - ]) <./@])
```

'''Solution''' (''look Ma, no hands!''):
```j
   spkln =: (u:9601+i.8)&$: : ([ {~ ((<.@* <:@#)~ ((- % (- >./))~ <./)))
```

'''Examples''':
```txt
   spkln 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
   spkln 1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5
▁▅█▆▅▃▂▇▄▅
```

Notes: J's grammar automatically normalizes numeric vector inputs which use a mix of whitespace and commas.  If we wanted to normalize input ourselves, i.e. take string rather than numeric input (e.g. to make apples-for-apples comparisons with the languages easier) we could simply use <tt>".</tt> ("eval") as a preprocessor, as in <tt>spkln@".</tt> (which is simple because it's still taking advantage of J's grammar to normalize numeric vectors).

Note also: the font my browser uses to render these sparkline characters looks awful. Other fonts look better.


## Java


```java

public class Sparkline 
{
	String bars="▁▂▃▄▅▆▇█";
	public static void main(String[] args)
	{
		Sparkline now=new Sparkline();
		float[] arr={1, 2, 3, 4, 5, 6, 7, 8, 7, 6, 5, 4, 3, 2, 1};
		now.display1D(arr);
		System.out.println(now.getSparkline(arr));
		float[] arr1={1.5f, 0.5f, 3.5f, 2.5f, 5.5f, 4.5f, 7.5f, 6.5f};
		now.display1D(arr1);
		System.out.println(now.getSparkline(arr1));
	}
	public void display1D(float[] arr)
	{
		for(int i=0;i<arr.length;i++)
			System.out.print(arr[i]+" ");
		System.out.println();
	}
	public String getSparkline(float[] arr)
	{
		float min=Integer.MAX_VALUE;
		float max=Integer.MIN_VALUE;
		for(int i=0;i<arr.length;i++)
		{
			if(arr[i]<min)
				min=arr[i];
			if(arr[i]>max)
				max=arr[i];
		}
		float range=max-min;
		int num=bars.length()-1;
		String line="";
		for(int i=0;i<arr.length;i++)
		{
			
			line+=bars.charAt((int)Math.ceil(((arr[i]-min)/range*num)));
		}
		return line;
	}
}

```

```txt

1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0 1.0 
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
1.5 0.5 3.5 2.5 5.5 4.5 7.5 6.5 
▂▁▄▃▆▅█▇


```




## JavaScript



### ES6



```JavaScript
(() => {
    'use strict';

    const main = () => {

        // sparkLine :: [Num] -> String
        const sparkLine = xs => {
            const hist = dataBins(8)(xs);
            return unlines([
                concat(map(
                    i => '▁▂▃▄▅▆▇█' [i],
                    hist.indexes
                )),
                unwords(xs),
                [
                    'Min: ' + hist.min,
                    'Mean: ' + hist.mean.toFixed(2),
                    'Median: ' + hist.median,
                    'Max: ' + hist.max,
                ].join('\t'),
                ''
            ]);
        };


        // dataBins :: Int -> [Num] ->
        //      {indexes:: [Int], min:: Float, max:: Float,
        //        range :: Float, lbounds :: [Float]}
        const dataBins = intBins => xs => {
            const
                iLast = intBins - 1,
                ys = sort(xs),
                mn = ys[0],
                mx = last(ys),
                rng = mx - mn,
                w = rng / intBins,
                lng = xs.length,
                mid = lng / 2,
                lbounds = map(
                    i => mn + (w * i),
                    enumFromTo(1, iLast)
                );
            return {
                indexes: map(
                    x => {
                        const mb = findIndex(b => b > x, lbounds);
                        return mb.Nothing ? (
                            iLast
                        ) : mb.Just;
                    },
                    xs
                ),
                lbounds: lbounds,
                min: mn,
                median: even(lng) ? (
                    sum([ys[mid - 1], ys[mid]]) / 2
                ) : ys[Math.floor(mid)],
                mean: sum(xs) / lng,
                max: mx,
                range: rng
            };
        };

        // numbersFromString :: String -> [Float]
        const numbersFromString = s =>
            map(x => parseFloat(x, 10),
                s.split(/[,\s]+/)
            );

        return unlines(map(
            compose(sparkLine, numbersFromString),
            [
                '0, 1, 19, 20',
                '0, 999, 4000, 4999, 7000, 7999',
                '1 2 3 4 5 6 7 8 7 6 5 4 3 2 1',
                '1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5'
            ]
        ));
    };

    // GENERIC FUNCTIONS ----------------------------

    // Just :: a -> Maybe a
    const Just = x => ({
        type: 'Maybe',
        Nothing: false,
        Just: x
    });

    // Nothing :: Maybe a
    const Nothing = () => ({
        type: 'Maybe',
        Nothing: true,
    });

    // compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
    const compose = (f, g) => x => f(g(x));

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];


    // enumFromTo :: (Int, Int) -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: 1 + n - m
        }, (_, i) => m + i);

    // even :: Int -> Bool
    const even = n => 0 === n % 2;

    // last :: [a] -> a
    const last = xs =>
        0 < xs.length ? xs.slice(-1)[0] : undefined;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // sort :: Ord a => [a] -> [a]
    const sort = xs => xs.slice()
        .sort((a, b) => a < b ? -1 : (a > b ? 1 : 0));


    // findIndex :: (a -> Bool) -> [a] -> Maybe Int
    const findIndex = (p, xs) => {
        const
            i = (
                'string' !== typeof xs ? (
                    xs
                ) : xs.split('')
            ).findIndex(p);
        return -1 !== i ? (
            Just(i)
        ) : Nothing();
    };

    // sum :: [Num] -> Num
    const sum = xs => xs.reduce((a, x) => a + x, 0);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // unwords :: [String] -> String
    const unwords = xs => xs.join(' ');

    // MAIN ---
    return main();
})();
```

```txt
▁▁██
0 1 19 20
Min: 0    Mean: 10.00    Median: 10    Max: 20

▁▁▅▅██
0 999 4000 4999 7000 7999
Min: 0    Mean: 4166.17    Median: 4499.5    Max: 7999

▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
Min: 1    Mean: 4.27    Median: 4    Max: 8

▂▁▄▃▆▅█▇
1.5 0.5 3.5 2.5 5.5 4.5 7.5 6.5
Min: 0.5    Mean: 4.00    Median: 4    Max: 7.5
```



## jq


```jq
def sparkline:
  min as $min
  | ( (max - $min) / 7 ) as $div
  | map( 9601 +  (. - $min) * $div )
  | implode ;

def string2array:
  def tidy: select( length > 0 );
  [split(" ") | .[] | split(",") | .[] | tidy | tonumber];
```

'''Task'''
 ( "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1",
   "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5"
 )  | string2array | sparkline
 $ jq -n -f -r sparkline.jq
 ▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
 ▂▁▄▃▆▅█▇


## Julia

```julia
function sparklineit(arr::Vector{<:Integer})
    sparkchars = '\u2581':'\u2588'
    dyn = length(sparkchars)
    lo, hi = extrema(arr)
    b = @. max(ceil(Int, dyn * (arr - lo) / (hi - lo)), 1)
    return join(sparkchars[b])
end

v = rand(0:10, 10)
println("$v → ", sparklineit(v))
v = 10rand(10)
println("$(round.(v, 2)) → ", sparklineit(v))
```


```txt
[6, 3, 9, 5, 1, 10, 0, 1, 3, 6] → ▅▃█▄▁█▁▁▃▅
[0.57, 0.14, 4.73, 6.61, 6.9, 0.8, 9.71, 7.39, 2.75, 5.7] → ▁▁▄▆▆▁█▇▃▅
```



## Kotlin

```scala
internal const val bars = "▁▂▃▄▅▆▇█"
internal const val n = bars.length - 1

fun <T: Number> Iterable<T>.toSparkline(): String {
    var min = Double.MAX_VALUE
    var max = Double.MIN_VALUE
    val doubles = map { it.toDouble() }
    doubles.forEach { i -> when { i < min -> min = i; i > max -> max = i } }
    val range = max - min
    return doubles.fold("") { line, d -> line + bars[Math.ceil((d - min) / range * n).toInt()] }
}

fun String.toSparkline() = replace(",", "").split(" ").map { it.toFloat() }.toSparkline()

fun main(args: Array<String>) {
    val s1 = "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1"
    println(s1)
    println(s1.toSparkline())
    val s2 = "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5"
    println(s2)
    println(s2.toSparkline())
}
```

```txt
1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5
▂▁▄▃▆▅█▇
```



## LiveCode


```LiveCode
command sparklines listOfNums
    local utfbase=0x2581
    local tStats, utfp, tmin,tmax,trange
    put listOfNums into tStats
    replace ", " with space in tStats
    replace space with comma in tStats
    put min(tStats) into tmin
    put max(tStats) into tmax
    put tmax - tmin into trange
    put "Min:" && tmin && tab into plot
    put "Max:" && tmax && tab after plot
    put "Range:" && trange && tab after plot
    put "Mean" && average(tStats) && tab after plot
    put "Stdev:" && standardDeviation(tStats) && tab after plot
    put "Variance:" && variance(tStats) && return after plot
    
    repeat for each item i in tStats
        put  (round(i - tmin/trange * 7)) + utfbase into utfp 
        put numToCodepoint(utfp) after plot
    end repeat
    put plot
end sparklines
```


Test

```txt
sparklines("1 2 3 4 5 6 7 8 7 6 5 4 3 2 1")
Min: 1 	Max: 8 	Range: 7 	Mean 4.266667 	Stdev: 2.250926 	Variance: 5.066667 
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁

sparklines("1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5")
Min: 0.5 	Max: 7.5 	Range: 7 	Mean 4 	Stdev: 2.44949 	Variance: 6 
▂▁▄▃▆▅█▇
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method sparkline(spark) private static
  spark = spark.changestr(',', ' ')
  bars = '\u2581 \u2582 \u2583 \u2584 \u2585 \u2586 \u2587 \u2588'
  barK = bars.words()
  nmin = spark.word(1)
  nmax = nmin
  -- get min & max values
  loop iw = 1 to spark.words()
    nval = spark.word(iw)
    nmin = nval.min(nmin)
    nmax = nval.max(nmax)
    end iw
  range = nmax - nmin + 1
  slope = ''
  loop iw = 1 to spark.words()    
    point = Math.ceil((spark.word(iw) - nmin + 1) / range * barK)
    slope = slope || bars.word(point)
    end iw
  return slope nmin nmax range

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  -- sample data setup
  parse arg vals
  sparks = 0
  sparks[0] = 0
  if vals = '' then do
    si = sparks[0] + 1; sparks[0] = si; sparks[si] = 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
    si = sparks[0] + 1; sparks[0] = si; sparks[si] = '1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5'
    end
  else do
    loop until vals = ''
      -- split input on a ! character
      parse vals lst '!' vals
      si = sparks[0] + 1; sparks[0] = si; sparks[si] = lst
      end
    end
  -- run the samples
  loop si = 1 to sparks[0]
    vals = sparks[si]
    parse sparkline(vals) slope .
    say 'Input:        ' vals
    say 'Sparkline:    ' slope
    say
    end si
  
  return

```

```txt

Input:         1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
Sparkline:     ▁▂▃▄▅▆▇█▇▆▅▄▃▂▁

Input:         1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5
Sparkline:     ▂▁▄▃▆▅█▇

```


## M2000 Interpreter

This statement a=cdr(a) make a new array from a excluding the first item, so can be return the empty array (,). In Row$(a) a is passing by value, but is a pointer to array, so we get a pointer to point to same array. When we do the a=cdr(a) we change pointer so variable (a) point to a new array.


The first program use auto arrays (or tuple), so we have to convert to string for output to clipboard (Print dat can be used to print the array as is to screen), and the second program get the input as string, so we have to make it an array at execution time, using then param() for inline parameters in expressions. A Random(param("1,10")) is the same as Random(1,10) (we can use only literal values in param(string_arg)). Because dat get from dat$ only the data, we can use Eval$("("+dat$+")"). Eval$() get all string as an expression for evaluation.




```M2000 Interpreter

Module CheckIt {
      Function Row$(a) {
          def item$(a)=str$(car(a)#val(0),0)
          rep$=item$(a)
          while len(a)
          rep$+=", "+item$(a)
          a=cdr(a)
          End While
          =rep$
      }
      Font "Dejavu Sans Mono"
      Cls
      Const bar$="▁▂▃▄▅▆▇█"
      Document doc$
      data1=(1, 2, 3, 4, 5, 6, 7, 8, 7, 6, 5, 4, 3, 2, 1)
      data2=(1.5, 0.5, 3.5, 2.5, 5.5, 4.5, 7.5, 6.5)
      SparkLine(data1)
      SparkLine(data2)
      Clipboard doc$
      Sub SparkLine(dat as array)
            Local min=dat#min(), range=(dat#max()-dat#min()), range1=7/range
            Local item, rep$="Input:"+Row$(dat)+{
            }
            item=each(dat)
            While item
            rep$+=Mid$(bar$,(array(item)-min)*range1+1 ,1)
            End While
            rep$+=" ("+str$(range,1033)+")"
            doc$=rep$+{
            }
            Report rep$
      End Sub
}
Checkit

```


Function Param() get a string an put it as inline code in expressions


```M2000 Interpreter

Module CheckIt {
      Font "Dejavu Sans Mono"
      Cls
      Const bar$="▁▂▃▄▅▆▇█"
      Document doc$
      data1$="1, 2, 3, 4, 5, 6, 7, 8, 7, 6, 5, 4, 3, 2, 1"
      data2$="1.5, 0.5, 3.5, 2.5, 5.5, 4.5, 7.5, 6.5"
      SparkLine(data1$)
      SparkLine(data2$)
      Clipboard doc$
      Sub SparkLine(dat$)
            dat=(param(dat$))
            Rem dat=eval$("("+dat$+")")
            Local min=dat#min(), range=(dat#max()-dat#min()), range1=7/range
            Local item, rep$="Input:"+dat$+{
            }
            item=each(dat)
            While item
            rep$+=Mid$(bar$,(array(item)-min)*range1+1 ,1)
            End While
            rep$+=" ("+str$(range,1033)+")"
            doc$=rep$+{
            }
            Report rep$
      End Sub
}
Checkit

```


Third program use strings for stack input, which skip coma as white space. We have to pass to current stack, here calling a function (which always have a new stack for values). First we flush the stack (make it empty), then we use Stack Dat$ to parse the dat$ (also empty the string). Last we rtuern an array using array() using as argument a stack object. Read only function [] do two things replace the current stack object with an empty one, and return the old stack.



```M2000 Interpreter

Module CheckIt {
      Function ExtractDat(dat$) {
            Flush
            Stack dat$
            =array([])
      }
      Font "Dejavu Sans Mono"
      Cls
      Const bar$="▁▂▃▄▅▆▇█"
      Document doc$
      data1$="1, 2, 3, 4, 5, 6, 7, 8, 7, 6, 5, 4, 3, 2, 1"
      data2$="1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5 "
      SparkLine(data1$)
      SparkLine(data2$)
      Clipboard doc$
      Sub SparkLine(dat$)
            dat=ExtractDat(dat$)
            Rem dat=eval$("("+dat$+")")
            Local min=dat#min(), range=(dat#max()-dat#min()), range1=7/range
            Local item, rep$="Input:"+dat$+{
            }
            item=each(dat)
            While item
            rep$+=Mid$(bar$,(array(item)-min)*range1+1 ,1)
            End While
            rep$+=" ("+str$(range,1033)+")"
            doc$=rep$+{
            }
            Report rep$
      End Sub
}
Checkit

```

```txt

Input:1, 2, 3, 4, 5, 6, 7, 8, 7, 6, 5, 4, 3, 2, 1
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁ (7)
Input:1.5, 0.5, 3.5, 2.5, 5.5, 4.5, 7.5, 6.5
▂▁▄▃▆▅█▇ (7)
</pre >


## Mathematica


```Mathematica
toSparkline[data_String] := FromCharacterCode[Round[7 Rescale@Flatten@ImportString[data, "Table", "FieldSeparators" -> {" ", ","}]] + 16^^2581];
```


```txt

toSparkline["1 2 3 4 5 6 7 8 7 6 5,4 3 2 1 "]
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁

toSparkline[" 1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5 "]
▂▁▄▃▆▅█▇

```



## Nim

```nim
import rdstdin, strutils, unicode

const bar = [9601, 9602, 9603, 9604, 9605, 9606, 9607, 9608]
const barcount = float(bar.high)

while True:
  let
    line = readLineFromStdin "Numbers please separated by space/commas: "
    numbers = line.split({' ',','}).map(parseFloat)
    mn = min(numbers)
    mx = max(numbers)
    extent = mx - mn
  var sparkline = ""
  for n in numbers:
    let i = int((n-mn) / extent * barcount)
    sparkline.add($TRune(bar[i]))
  echo "min: ", mn.formatFloat(precision = 0), "; max: ", mx.formatFloat(precision = 0)
  echo sparkline
```

```txt
Numbers please separated by space/commas: 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
min: 1; max: 8
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
Numbers please separated by space/commas: 1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5
min: 0.5; max: 7.5
▂▁▄▃▆▅█▇
```



## Perl


```perl
binmode(STDOUT, ":utf8");
our @sparks=map {chr} 0x2581 .. 0x2588;
sub sparkline(@) {
    my @n=map {0+$_} grep {length} @_ or return "";
    my($min,$max)=($n[0])x2;
    if (@n>1) {
        for (@n[1..$#n]) {
            if    ($_<$min) { $min=$_ }
            elsif ($_>$max) { $max=$_ }
        }
    }
    my $sparkline="";
    for(@n) {
        my $height=int( $max==$min ? @sparks/2 : ($_-$min)/($max-$min)*@sparks );
        $height=$#sparks if $height>$#sparks;
        $sparkline.=$sparks[$height];
    }
    my $summary=sprintf "%d values; range %s..%s", scalar(@n), $min, $max;
    return wantarray ? ($summary, "\n", $sparkline, "\n") : $sparkline;
}

# one number per line
# print sparkline( <> );

# in scalar context, get just the sparkline without summary or trailing newline
# my $sl=sparkline( <> ); print $sl;

# one sparkline per line
print sparkline( split /[\s,]+/ ) while <>;

```

```txt

$ echo 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1 | perl -Mstrict -w spark.pl # strict and warn not needed but it passes both
15 values; range 1..8
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁

$ echo 0 1 19 20 | perl spark.pl
4 values; range 0..20
▁▁██

$ echo 0 999 4000 4999 7000 7999  | perl spark.pl
6 values; range 0..7999
▁▁▅▅██

$ echo 1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5 | perl spark.pl
8 values; range 0.5..7.5
▂▁▄▃▆▅█▇

$ echo -9e9 1.2345 6e5  | perl spark.pl
3 values; range -9000000000..600000
▁██

$ echo 12 12 12 12 | perl spark.pl
4 values; range 12..12
▅▅▅▅

```

Most Perl builds have [https://en.wikipedia.org/wiki/IEEE_754-2008_revision IEEE-754 floats]:

```txt

$ echo {1..8}e307  | perl spark.pl
8 values; range 1e+307..8e+307
▁▂▃▄▅▆▇█

$ echo {1..8}e308  | perl spark.pl  
8 values; range 1e+308..Inf
▁▁▁▁▁▁▁▁

$ echo -{1..8}e307 | perl spark.pl  
8 values; range -8e+307..-1e+307
█▇▆▅▄▃▂▁
```



## Perl 6


```perl6
constant @bars = '▁' ... '█';
while prompt 'Numbers separated by anything: ' -> $_ {
    my @numbers = map +*, .comb(/ '-'? [[\d+ ['.' \d*]?] | ['.' \d+]] /);
    my ($mn,$mx) = @numbers.minmax.bounds;
    say "min: $mn.fmt('%5f'); max: $mx.fmt('%5f')";
    say @bars[ @numbers.map: { @bars * ($_ - $mn) / ($mx - $mn) min @bars - 1 } ].join;
}
```

```txt
Numbers separated by anything: 9 18 27 36 45 54 63 72 63 54 45 36 27 18 9
9 18 27 36 45 54 63 72 63 54 45 36 27 18 9
min: 9.000000; max: 72.000000
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
Numbers separated by anything: 1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5
1.5 0.5 3.5 2.5 5.5 4.5 7.5 6.5
min: 0.500000; max: 7.500000
▂▁▄▃▆▅█▇
Numbers separated by anything: 3 2 1 0 -1 -2 -3 -4 -3 -2 -1 0 1 2 3  
min: -4.000000; max: 3.000000
█▇▆▅▄▃▂▁▂▃▄▅▆▇█
Numbers separated by anything: ^D
```



## Phix

Works fine on Linux, with or without unicode_console.e as that does little apart from check environment settings for clues, but on my windows box, on which it invokes kernel32/SetConsoleOutputCP(CP_UTF8), unicode_console.e improves it only slightly, getting just two of the eight characters right. Changing the font on the windows(10) console to "NSimSum" or "SimSun-ExtB" (manually, by right clicking on the title bar and selecting properties/fonts) improved things considerably.

```Phix
include builtins\unicode_console.e   -- (0.8.0+)

constant tests = {"0 1 19 20", "0 0 1 1",
                  "0 999 4000 4999 7000 7999", "1 1 2 2 3 3",
                  "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1",
                  "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5"}

for i=1 to length(tests) do
    sequence ti = split_any(tests[i]," ,",no_empty:=true)
    for j=1 to length(ti) do
        {{ti[j]}} = scanf(ti[j],"%f")
    end for
    atom mn = min(ti),
         mx = max(ti),
         range = mx-mn
    printf(1,"Min :%g, Max :%g, Range :%g\n",{mn,mx,range})
 
    if unicode_console() then
        for j=1 to length(ti) do
            ti[j] = #2581 + min(7,floor((ti[j]-mn)/range*8))
        end for
        printf(1,"%s\n",{utf32_to_utf8(ti)})
    else
        puts(1,"unicode is not supported\n")
    end if
end for
```

```txt

Min :0, Max :20, Range :20
▁▁██
Min :0, Max :1, Range :1
▁▁██
Min :0, Max :7999, Range :7999
▁▁▅▅██
Min :1, Max :3, Range :2
▁▁▅▅██
Min :1, Max :8, Range :7
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
Min :0.5, Max :7.5, Range :7
▂▁▄▃▆▅█▇

```



## PicoLisp


```PicoLisp
(de sparkLine (Lst)
   (let (Min (apply min Lst)  Max (apply max Lst)  Rng (- Max Min))
      (for N Lst
         (prin
            (char (+ 9601 (*/ (- N Min) 7 Rng)) ) ) )
      (prinl) ) )
```

Test:

```PicoLisp
(sparkLine (str "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1"))
(sparkLine (scl 1 (str "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5")))
```

Output:

```txt
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
▂▁▄▃▆▅█▇
```



## Python


```python
# -*- coding: utf-8 -*-

# Unicode: 9601, 9602, 9603, 9604, 9605, 9606, 9607, 9608
bar = '▁▂▃▄▅▆▇█'
barcount = len(bar)

def sparkline(numbers):
    mn, mx = min(numbers), max(numbers)
    extent = mx - mn
    sparkline = ''.join(bar[min([barcount - 1,
                                 int((n - mn) / extent * barcount)])]
                        for n in numbers)
    return mn, mx, sparkline

if __name__ == '__main__':
    import re
    
    for line in ("0 0 1 1; 0 1 19 20; 0 999 4000 4999 7000 7999;"
                 "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1;"
                 "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5 ").split(';'):
        print("\nNumbers:", line)
        numbers = [float(n) for n in re.split(r'[\s,]+', line.strip())]
        mn, mx, sp = sparkline(numbers)
        print('  min: %5f; max: %5f' % (mn, mx))
        print("  " + sp)
```


```txt

Numbers: 0 0 1 1
  min: 0.000000; max: 1.000000
  ▁▁██

Numbers:  0 1 19 20
  min: 0.000000; max: 20.000000
  ▁▁██

Numbers:  0 999 4000 4999 7000 7999
  min: 0.000000; max: 7999.000000
  ▁▁▅▅██

Numbers: 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
  min: 1.000000; max: 8.000000
  ▁▂▃▄▅▆▇█▇▆▅▄▃▂▁

Numbers: 1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5 
  min: 0.500000; max: 7.500000
  ▂▁▄▃▆▅█▇
```




Or, by composition of pure functions, with type comments for the reader, rather than the compiler.

To vary the approach, the block used is derived from the option type (<code>Just x | Nothing</code>) result of a search over 7 zero-indexed lower bounds [0..6].

If a lower bound whose value exceeds that of the given data point can be found, we just use the index of that bound.

If the search result is 'not found' (Nothing), then we use the highest (nominally 8th, or index 7) block.


```python
import re


# sparkLine :: [Float] -> [String]
def sparkLine(xs):
    def go(xs):
        ys = sorted(xs)
        mn, mx = ys[0], ys[-1]
        n = len(xs)
        mid = n // 2
        w = (mx - mn) / 8
        lbounds = list(map(lambda i: mn + (w * i), range(1, 8)))
        return [
            ''.join(map(
                lambda x: maybe('█')(
                    lambda i: '▁▂▃▄▅▆▇'[i]
                )(findIndex(lambda b: b > x)(lbounds)),
                xs
            )),
            ' '.join(map(str, xs)),
            '\t'.join([
                'Min ' + str(mn),
                'Mean ' + str(round(mean(xs), 2)),
                'Median ' + str(
                    (ys[mid - 1] + ys[mid]) / 2 if even(n) else (
                        ys[mid]
                    )
                ),
                'Max ' + str(mx)
            ]),
            ''
        ]
    return go(xs) if xs else []


# main :: IO ()
def main():
    print(
        unlines(map(
            compose(compose(unlines)(sparkLine))(readFloats),
            [
                "0, 1, 19, 20",
                "0, 999, 4000, 4999, 7000, 7999",
                "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1",
                "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5"
            ]
        ))
    )


# GENERIC -------------------------------------------------


# Just :: a -> Maybe a
def Just(x):
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    return {'type': 'Maybe', 'Nothing': True}


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    return lambda f: lambda x: g(f(x))


# even :: Int -> Bool
def even(x):
    return 0 == x % 2


# findIndex :: (a -> Bool) -> [a] -> Maybe Int
def findIndex(p):
    def go(xs):
        try:
            return Just(next(
                i for i, v in enumerate(xs) if p(v)
            ))
        except StopIteration:
            return Nothing()
    return lambda xs: go(xs)


# maybe :: b -> (a -> b) -> Maybe a -> b
def maybe(v):
    return lambda f: lambda m: v if m.get('Nothing') else (
        f(m.get('Just'))
    )


# mean :: [Num] -> Float
def mean(xs):
    return sum(xs) / float(len(xs))


# readFloats :: String -> [Float]
def readFloats(s):
    return list(map(
        float,
        re.split('[\s,]+', s)
    ))


# unlines :: [String] -> String
def unlines(xs):
    return '\n'.join(xs)


# TEST -------------------------------------------------
if __name__ == '__main__':
    main()
```

```txt
▁▁██
0.0 1.0 19.0 20.0
Min 0.0    Mean 10.0    Median 10.0    Max 20.0

▁▁▅▅██
0.0 999.0 4000.0 4999.0 7000.0 7999.0
Min 0.0    Mean 4166.17    Median 4499.5    Max 7999.0

▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0 1.0
Min 1.0    Mean 4.27    Median 4.0    Max 8.0

▂▁▄▃▆▅█▇
1.5 0.5 3.5 2.5 5.5 4.5 7.5 6.5
Min 0.5    Mean 4.0    Median 4.0    Max 7.5
```



## REXX


### version 1

```REXX
/* Rexx */

parse arg aaa
call runSample aaa
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sparkline:
  procedure
  parse arg spark
  spark = changestr(',', spark, ' ')
  bars = '▁ ▂ ▃ ▄ ▅ ▆ ▇ █'
  barK = words(bars)
  nmin = word(spark, 1)
  nmax = nmin
  -- get min & max values
  do iw = 1 to words(spark)
    nval = word(spark, iw)
    nmin = min(nval, nmin)
    nmax = max(nval, nmax)
    end iw
  range = nmax - nmin + 1
  slope = ''
  do iw = 1 to words(spark)
    point = ceiling((word(spark, iw) - nmin + 1) / range * barK)
    slope = slope || word(bars, point)
    end iw
  return slope nmin nmax range

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ceiling:
procedure 
  parse arg ceil
  return trunc(ceil) + (ceil > 0) * (ceil \= trunc(ceil))
floor:
procedure 
  parse arg flor
  return trunc(flor) - (flor < 0) * (flor \= trunc(flor))

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
runSample:
procedure
  -- sample data setup
  parse arg vals
  sparks = 0
  sparks.0 = 0
  if vals = '' then do
    si = sparks.0 + 1; sparks.0 = si; sparks.si = 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
    si = sparks.0 + 1; sparks.0 = si; sparks.si = '1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5'
    end
  else do
    do until vals = ''
      -- split input on a ! character
      parse var vals lst '!' vals
      si = sparks.0 + 1; sparks.0 = si; sparks.si = lst
      end
    end
  -- run the samples
  do si = 1 to sparks.0
    vals = sparks.si
    parse value sparkline(vals) with slope .
    say 'Input:        ' vals
    say 'Sparkline:    ' slope
    say
    end si
  
  return

```

```txt

Input:         1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
Sparkline:     ▁▂▃▄▅▆▇█▇▆▅▄▃▂▁

Input:         1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5
Sparkline:     ▂▁▄▃▆▅█▇

```



### version 2

(A re-work of REXX version 1)

This version works on:
::* all versions of Regina (which may or may not support ''single line comments'')
::* R4 and ROO  (which don't support ''single line comments'')
::* older versions of REXX such as PC/REXX and Personal REXX which don't support the '''changestr''' BIF

This version also removed some dead code, simplified the program structure and subroutines, added comments.

Single line comments'' were introduced in Regina 3.4.

Regina 3.6 introduced the options:   ''single_line_comments'' and ''noSingle_line_comments''.

It should also be noted that the CMS and TSO versions of REXX (and practically all others) don't support ''single line comments''.

```rexx
/*REXX program displays a  sparkline  (spark graph)  for a group of values.             */
if arg()==0  then do                             /*Optional arguments? Then use defaults*/
                  call sparkGraph  1  2  3  4  5  6  7  8  7  6  5  4  3  2  1
                  call sparkGraph '1.5,  0.5  3.5,  2.5  5.5,  4.5  7.5,  6.5'
                  end
             else call sparkGraph arg(1)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ceil:       procedure;  parse arg ?;         _=trunc(?);          return _+(?>0)*(?\=_)
/*──────────────────────────────────────────────────────────────────────────────────────*/
sparkGraph: procedure;  parse arg x;   say ' input: '  x              /*echo the values.*/
            x= translate(x, ' ', ",")                  /*remove any superfluous commas. */
            $= '▁▂▃▄▅▆▇█';         L= length($)    /*chars to be used for the graph.*/
            xmin= word(x, 1);          xmax= xmin      /*assume a minimum and a maximum.*/

                do n=2  to words(x);   _= word(x, n)   /*examine successive words in  X.*/
                xmin= min(_, xmin)                     /*find the minimum value in  X.  */
                xmax= max(_, xmax)                     /*  "   "  maximum   "    "  "   */
                end   /*n*/
            z=
                   do j=1  for words(x)                /*build the output spark graph.  */
                   z= z || substr($, ceil( ( word(x, j) -xmin+1) / (xmax -xmin+1) * L), 1)
                   end   /*j*/

            say 'output: '  z;   say;  return          /*show the output, + a blank line*/
```

```txt

 input:  1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
output:  ▁▂▃▄▅▆▇█▇▆▅▄▃▂▁

 input:  1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5
output:  ▂▁▄▃▆▅█▇

```



## Racket


```racket

#lang racket (require syntax/parse)

(define bars "▁▂▃▄▅▆▇█")
(define bar-count (string-length bars))

(define (sparks str)
  (define ns (map string->number (string-split str #rx"[ ,]" #:repeat? #t)))
  (define mn (apply min ns))
  (define bar-width (/ (- (apply max ns) mn) (- bar-count 1)))
  (apply string (for/list ([n ns]) (string-ref bars (exact-floor (/ (- n mn) bar-width))))))

(sparks "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1")
(sparks "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5")

```

```racket

"▁▂▃▄▅▆▇█▇▆▅▄▃▂▁"
"▂▁▄▃▆▅█▇"

```



## Ruby

{{trans|Perl 6}} with added protection for input like "0 0 0 0".

```ruby
bar = ('▁'..'█').to_a 
loop {print 'Numbers please separated by space/commas: '
  numbers = gets.split(/[\s,]+/).map(&:to_f)
  min, max = numbers.minmax
  puts "min: %5f; max: %5f"% [min, max]
  div = (max - min) / (bar.size - 1)
  puts min == max ? bar.last*numbers.size : numbers.map{|num| bar[((num - min) / div).to_i]}.join
}
```


{{out}} Used Go testcases

```txt
Numbers please separated by space/commas: 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
min: 1.000000; max: 8.000000
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
Numbers please separated by space/commas: 1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5 
min: 0.500000; max: 7.500000
▂▁▄▃▆▅█▇
Numbers please separated by space/commas: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
min: 1.000000; max: 24.000000
▁▁▁▁▂▂▂▃▃▃▄▄▄▄▅▅▅▆▆▆▇▇▇█
Numbers please separated by space/commas: 0 99 101 699 701 800
min: 0.000000; max: 800.000000
▁▁▁▇▇█
Numbers please separated by space/commas: 0 -.09 -.11 -.69 -.71 -.8
min: -0.800000; max: 0.000000
█▇▇▁▁▁
Numbers please separated by space/commas: 3 3 3
min: 3.000000; max: 3.000000
███
Numbers please separated by space/commas: 1e99
min: 999999999999999967336168804116691273849533185806555472917961779471295845921727862608739868455469056.000000; max: 999999999999999967336168804116691273849533185806555472917961779471295845921727862608739868455469056.000000
█

```



## Rust


```rust

const BARS: &'static str = "▁▂▃▄▅▆▇█";

fn print_sparkline(s: &str){
    let v = BARS.chars().collect::<Vec<char>>();
    let line: String = s.replace(",", " ").split(" ")
                            .filter(|x| !x.is_empty())
                            .map(|x| v[x.parse::<f64>().unwrap().ceil() as usize - 1])
                            .collect();
    println!("{:?}", line);
}

fn main(){
    let s1 = "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1";
    print_sparkline(s1);
    let s2 = "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5";
    print_sparkline(s2);
}

```

"▁▂▃▄▅▆▇█▇▆▅▄▃▂▁"

"▂▁▄▃▆▅█▇"

=={{header|S-lang}}==
<lang S-lang>
% Just to demonstrate alternate ways of defining unicode:
private variable spchrs = "\u{2581}\u{2582}\u{2583}\u{2584}\u{2585}\u{2586}\u{2587}\u{2588}";
private variable spchrs_alt = "▁▂▃▄▅▆▇█";

define sparkline(arrstr) 
{
    variable a = strtok(arrstr, " \t,"), alen = length(a), out = "";
    a = atof(a);
    variable amin = min(a), amax = max(a), span = amax - amin, i, d;

    _for i (0, alen-1, 1)
        if (span != 0) {
            % int() truncates; adding .5 here to round:
            d = int((a[i] - amin) * 7.0 / span + 0.5);
            out += substr(spchrs, d+1, 1);
        }
        else
            out += substr(spchrs, 4, 1);

    print(out);
}

if (not _slang_utf8_ok) error("Sorry, UTF8 mode is not on.");
sparkline("1 2 3 4 5 6 7 8 7 6 5 4 3 2 1");
sparkline("1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5 ");

```

"▁▂▃▄▅▆▇█▇▆▅▄▃▂▁"

"▂▁▄▃▆▅█▇"


## Scala


```scala
def mkSparks( numStr:String ) : String =
  numStr.split( "[\\s,]+" ).map(_.toFloat) match {
    case v if v.isEmpty => ""
    case v if v.length == 1 => "\u2581"
    case v =>
      (for( i <- v;
            s = "\u2581\u2582\u2583\u2584\u2585\u2586\u2587\u2588".toCharArray;
            d = (v.max - v.min) / (s.length - 1)
       ) yield s( ((i - v.min) / d).toInt)).mkString
  }

println( mkSparks( "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1" ) )
println( mkSparks( "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5" ) )

// A random test...
println( mkSparks( Stream.continually( math.abs(util.Random.nextInt % 8)).take(64).mkString(" ") ))
```

```txt
 ▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
 ▂▁▄▃▆▅█▇
 ▆▁▅▄▁▅▅▇▂▅▆▄▆▃█▄▃▅▇▂▅▆▂▃▇▆▅▇▅█▂▅▄▂▃▇▁▃▇▇▃▁▆▆▂▄▁▄▂▁▁▃▇▆▃▂▆▂▆▇▁▁▆▃
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "scanfile.s7i";
  include "float.s7i";
  include "utf8.s7i";

const func array float: readDataLine is func
  result
    var array float: data is 0 times 0.0;
  begin
    write("Numbers separated by anything: ");
    IN.bufferChar := getc(IN);
    skipSpace(IN);
    while IN.bufferChar <> '\n' do
      data &:= float parse getNumber(IN);
      skipSpace(IN);
      if IN.bufferChar = ',' then
        IN.bufferChar := getc(IN);
      end if;
      skipSpace(IN);
    end while;
  end func;


const proc: main is func
  local
    const string: bars is "▁▂▃▄▅▆▇█";
    var array float: data is 0 times 0.0;
    var float: min is 0.0;
    var float: max is 0.0;
    var float: number is 0.0;
    var integer: index is 0;
  begin
    OUT := STD_UTF8_OUT;
    data  := readDataLine;
    while length(data) >= 1 do
      min := data[1];
      max := data[1];
      for number range data do
        if number < min then
          min := number;
        end if;
        if number > max then
          max := number;
        end if;
      end for;
      for number range data do
        index := succ(min(trunc((number - min) * 8.0 / max), 7));
        write(bars[index]);
      end for;
      writeln;
      data  := readDataLine;
    end while;
  end func;
```


```txt

Numbers separated by anything: 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1 
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
Numbers separated by anything: 1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5
▂▁▄▃▆▅█▇
Numbers separated by anything: 

```



## Sidef

```ruby
var bar = @('▁'..'█');
loop {
    print 'Numbers, please, separated by space/commas: ';
    var numbers = read(String).trim.split(/[\s,]+/).map{.to_n};
    var (min, max) = numbers.minmax;
    say "min: %5f; max: %5f"%(min, max);
    var div = ((max - min) / bar.end);
    say (min == max ? bar.last*numbers.len : numbers.map{|num| bar[(num - min) / div]}.join);
}
```

```txt
Numbers, please, separated by space/commas: 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
min: 1.000000; max: 8.000000
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
Numbers, please, separated by space/commas: 1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5
min: 0.500000; max: 7.500000
▂▁▄▃▆▅█▇
```



## Tcl

```tcl
package require Tcl 8.6

proc extractValues {series} {
    return [regexp -all -inline {\d+(?:\.\d*)?|\.\d+} $series]
}
proc renderValue {min max value} {
    set band [expr {int(8*($value-$min)/(($max-$min)*1.01))}]
    return [format "%c" [expr {0x2581 + $band}]]
}
proc sparkline {series} {
    set values [extractValues $series]
    set min [tcl::mathfunc::min {*}$values]
    set max [tcl::mathfunc::max {*}$values]
    return [join [lmap v $values {renderValue $min $max $v}] ""]
}
```

Demonstrating:

```tcl
set data {
    "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1"
    "1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5"
}
foreach series $data {
    puts "Series: $series"
    puts "Sparkline: [sparkline $series]"
}
```

```txt

Series: 1 2 3 4 5 6 7 8 7 6 5 4 3 2 1
Sparkline: ▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
Series: 1.5, 0.5 3.5, 2.5 5.5, 4.5 7.5, 6.5
Sparkline: ▂▁▄▃▆▅█▇

```



## zkl


```zkl
var sparks=[0x2581..0x2588].apply("toString",-8); // int.toString(-8)-->UTF-8
var sl=(sparks.len()-1);
 
fcn sparkLine(xs){
   min:=(0.0).min(xs); max:=(0.0).max(xs);  // min/max are float reguardless of xs
   range:=max-min;  // float
   println("Range [",min,"-",max,"]", xs);
   xs.pump(String,'wrap(x){ sparks[(x - min)*sl/range] }).println();
}
```


```zkl
one:="1 2 3 4 5 6 7 8 7 6 5 4 3 2 1".split(" ").apply("toInt");
two:=("1.5, 0.5 3.5, 2.5 5.5 4.5 7.5, 6.5" - ",").split(" ").apply("toFloat");
sparkLine(one); sparkLine(two);
```

```txt

Range [1-8]: L(1,2,3,4,5,6,7,8,7,6,5,4,3,2,1)
▁▂▃▄▅▆▇█▇▆▅▄▃▂▁
Range [0.5-7.5]: L(1.5,0.5,3.5,2.5,5.5,4.5,7.5,6.5)
▂▁▄▃▆▅█▇

```

