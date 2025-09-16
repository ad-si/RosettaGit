+++
title = "Entropy"
description = ""
date = 2019-06-21T06:28:11Z
aliases = []
[extra]
id = 12957
[taxonomies]
categories = ["Mathematics", "Information theory", "task"]
tags = []
languages = [
  "11l",
  "ada",
  "aime",
  "algol_68",
  "algol_w",
  "apl",
  "autohotkey",
  "awk",
  "basic",
  "bbc_basic",
  "burlesque",
  "c",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "elena",
  "elixir",
  "emacs_lisp",
  "erlang",
  "euler_math_toolbox",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "friendly_interactive_shell",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "lang5",
  "liberty_basic",
  "lua",
  "miniscript",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oorexx",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "pl_i",
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
  "scala",
  "scheme",
  "scilab",
  "seed7",
  "sidef",
  "sinclair_zx81_basic",
  "swift",
  "tcl",
  "xpl0",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task
Calculate the Shannon entropy   H   of a given input string.

Given the discrete random variable <math>X</math> that is a string of <math>N</math> "symbols" (total characters) consisting of <math>n</math> different characters (n=2 for binary), the Shannon entropy of X in '''bits/symbol''' is :
:<math>H_2(X) = -\sum_{i=1}^n \frac{count_i}{N} \log_2 \left(\frac{count_i}{N}\right)</math>

where <math>count_i</math> is the count of character <math>n_i</math>.

For this task, use X="<tt>1223334444</tt>" as an example. The result should be 1.84644... bits/symbol.  This assumes X was a random variable, which may not be the case, or it may depend on the observer.

This coding problem calculates the "specific" or "[[wp:Intensive_and_extensive_properties|intensive]]" entropy that finds its parallel in physics with "specific entropy" S<sup>0</sup> which is entropy per kg or per mole, not like physical entropy S and therefore not the "information" content of a file. It comes from Boltzmann's H-theorem where <math>S=k_B  N  H</math> where N=number of molecules. Boltzmann's H is the same equation as Shannon's H, and it gives the specific entropy H on a "per molecule" basis.

The "total", "absolute", or "[[wp:Intensive_and_extensive_properties|extensive]]" information entropy is
:<math>S=H_2 N</math> bits
This is not the entropy being coded here, but it is the closest to physical entropy and a measure of the information content of a string. But it does not look for any patterns that might be available for compression, so it is a very restricted, basic, and certain measure of "information". Every binary file with an equal number of 1's and 0's will have S=N bits. All hex files with equal symbol frequencies will have <math>S=N \log_2(16)</math> bits of entropy. The total entropy in bits of the example above is S= 10*18.4644 = 18.4644  bits.

The H function does not look for any patterns in data or check if X was a random variable.  For example, X=000000111111 gives the same calculated entropy in all senses as Y=010011100101. For most purposes it is usually more relevant to divide the gzip length by the length of the original data to get an informal measure of how much "order" was in the data.

Two other "entropies" are useful:

Normalized specific entropy:
:<math>H_n=\frac{H_2 * \log(2)}{\log(n)}</math>
which varies from 0 to 1 and it has units of "entropy/symbol" or just 1/symbol. For this example, H<sub>n<\sub>= 0.923.

Normalized total (extensive) entropy:
:<math>S_n = \frac{H_2 N * \log(2)}{\log(n)}</math>
which varies from 0 to N and does not have units. It is simply the "entropy", but it needs to be called "total normalized extensive entropy" so that it is not confused with Shannon's (specific) entropy or physical entropy. For this example, S<sub>n<\sub>= 9.23.

Shannon himself is the reason his "entropy/symbol" H function is very confusingly called "entropy". That's like calling a function that returns a speed a "meter". See section 1.7 of his classic [http://worrydream.com/refs/Shannon%20-%20A%20Mathematical%20Theory%20of%20Communication.pdf  A Mathematical Theory of Communication] and search on "per symbol" and "units" to see he always stated his entropy H has units of "bits/symbol" or "entropy/symbol" or "information/symbol".  So it is legitimate to say entropy NH is "information".

In keeping with Landauer's limit, the physics entropy generated from erasing N bits is <math>S = H_2 N k_B \ln(2)</math> if the bit storage device is perfectly efficient.  This can be solved for H<sub>2</sub>*N to (arguably) get the number of bits of information that a physical entropy represents.

;Related tasks:
:* [[Fibonacci_word]]
:* [[Entropy/Narcissist]]





## 11l


```11l
F entropy(source)
   DefaultDict[Char, Int] hist
   L(c) source
      hist[c]++
   V r = 0.0
   L(v) hist.values()
      V c = Float(v) / source.len
      r -= c * log2(c)
   R r

print(entropy(‘1223334444’))
```

{{out}}

```txt

1.84644

```



## Ada

Uses Ada 2012.

```Ada
with Ada.Text_IO, Ada.Float_Text_IO, Ada.Numerics.Elementary_Functions;

procedure Count_Entropy is

   package TIO renames Ada.Text_IO;

   Count: array(Character) of Natural := (others => 0);
   Sum:   Natural := 0;
   Line: String := "1223334444";

begin
   for I in Line'Range loop   -- count the characters
      Count(Line(I)) := Count(Line(I))+1;
      Sum := Sum + 1;
   end loop;

   declare   -- compute the entropy and print it
      function P(C: Character) return Float is (Float(Count(C)) / Float(Sum));
      use Ada.Numerics.Elementary_Functions, Ada.Float_Text_IO;
      Result: Float := 0.0;
   begin
      for Ch in Character loop
         Result := Result -
          (if P(Ch)=0.0 then 0.0 else P(Ch) * Log(P(Ch), Base => 2.0));
      end loop;
      Put(Result, Fore => 1, Aft => 5, Exp => 0);
   end;
end Count_Entropy;
```



## Aime


```aime
integer c;
real h, v;
index x;
data s;

for (, c in (s = argv(1))) {
    x[c] += 1r;
}

h = 0;
for (, v in x) {
    v /= ~s;
    h -= v * log2(v);
}

o_form("/d6/\n", h);
```

Examples:

```txt
$ aime -a tmp/entr 1223334444
1.846439
$ aime -a tmp/entr 'Rosetta Code is the best site in the world!'
3.646513
$ aime -a tmp/entr 1234567890abcdefghijklmnopqrstuvwxyz
5.169925
```



## ALGOL 68


```algol68
BEGIN
    # calculate the shannon entropy of a string                                #
    PROC shannon entropy = ( STRING s )REAL:
    BEGIN
        INT string length = ( UPB s - LWB s ) + 1;
        # count the occurences of each character #
        [ 0 : max abs char ]INT char count;
        FOR char pos FROM LWB char count TO UPB char count DO
            char count[ char pos ] := 0
        OD;
        FOR char pos FROM LWB s TO UPB s DO
            char count[ ABS s[ char pos ] ] +:= 1
        OD;
        # calculate the entropy, we use log base 10 and then convert #
        # to log base 2 after calculating the sum                    #
        REAL entropy := 0;
        FOR char pos FROM LWB char count TO UPB char count DO
            IF char count[ char pos ] /= 0
            THEN
                # have a character that occurs in the string #
                REAL probability = char count[ char pos ] / string length;
                entropy -:= probability * log( probability )
            FI
        OD;
        entropy / log( 2 )
    END; # shannon entropy #

    # test the shannon entropy routine #
    print( ( shannon entropy( "1223334444" ), newline ) )

END
```

{{out}}

```txt

+1.84643934467102e  +0

```



## ALGOL W

{{trans|ALGOL 68}}

```algolw
begin
    % calculates the shannon entropy of a string                          %
    % strings are fixed length in algol W and the length is part of the   %
    % type, so we declare the string parameter to be the longest possible %
    % string length (256 characters) and have a second parameter to       %
    % specify how much is actually used                                   %
    real procedure shannon_entropy ( string(256) value s
                                   ; integer     value stringLength
                                   );
    begin

        real    probability, entropy;

        % algol W assumes there are 256 possible characters %
        integer MAX_CHAR;
                MAX_CHAR := 256;

        % declarations must preceed statements, so we start a new         %
        % block here so we can use MAX_CHAR as an array bound             %
        begin

            % increment an integer variable                               %
            procedure incI ( integer value result a ) ; a := a + 1;

            integer array charCount( 1 :: MAX_CHAR );

            % count the occurances of each character in s                 %
            for charPos := 1 until MAX_CHAR do charCount( charPos ) := 0;
            for sPos := 0 until stringLength - 1 do incI( charCount( decode( s( sPos | 1 ) ) ) );

            % calculate the entropy, we use log base 10 and then convert  %
            % to log base 2 after calculating the sum                     %

            entropy := 0.0;
            for charPos := 1 until MAX_CHAR do
            begin
                if charCount( charPos ) not = 0
                then begin
                    % have a character that occurs in the string          %
                    probability := charCount( charPos ) / stringLength;
                    entropy     := entropy - ( probability * log( probability ) )
                end
            end charPos

        end;

        entropy / log( 2 )
    end shannon_entropy ;

    % test the shannon entropy routine                                    %
    r_format := "A"; r_w := 12; r_d := 6; % set output to fixed format    %
    write( shannon_entropy( "1223334444", 10 ) )

end.
```

{{out}}

```txt

    1.846439

```



## APL


```apl

      ENTROPY←{-+/R×2⍟R←(+⌿⍵∘.=∪⍵)÷⍴⍵}

      ⍝ How it works:
      ⎕←UNIQUE←∪X←'1223334444'
1234
      ⎕←TABLE_OF_OCCURENCES←X∘.=UNIQUE
1 0 0 0
0 1 0 0
0 1 0 0
0 0 1 0
0 0 1 0
0 0 1 0
0 0 0 1
0 0 0 1
0 0 0 1
0 0 0 1
      ⎕←COUNT←+⌿TABLE_OF_OCCURENCES
1 2 3 4
      ⎕←N←⍴X
10
      ⎕←RATIO←COUNT÷N
0.1 0.2 0.3 0.4
      -+/RATIO×2⍟RATIO
1.846439345

```

{{out}}

```txt

      ENTROPY X
1.846439345

```



## AutoHotkey


```AutoHotkey
MsgBox, % Entropy(1223334444)

Entropy(n)
{
    a := [], len := StrLen(n), m := n
    while StrLen(m)
    {
        s := SubStr(m, 1, 1)
        m := RegExReplace(m, s, "", c)
        a[s] := c
    }
    for key, val in a
    {
        m := Log(p := val / len)
        e -= p * m / Log(2)
    }
    return, e
}
```

{{out}}

```txt
1.846440
```



## AWK


```awk
#!/usr/bin/awk -f
{
	for (i=1; i<= length($0); i++) {
		H[substr($0,i,1)]++;
		N++;
	}
}

END {
	for (i in H) {
		p = H[i]/N;
		E -=  p * log(p);
	}
	print E/log(2);
}
```

{{out|Usage}}

```bash
 echo 1223334444 |./entropy.awk
1.84644
```



## BASIC

Works with older (unstructured) Microsoft-style BASIC.

```basic
10 DEF FN L(X)=LOG(X)/LOG(2)
20 S$="1223334444"
30 U$=""
40 FOR I=1 TO LEN(S$)
50 K=0
60 FOR J=1 TO LEN(U$)
70 IF MID$(U$,J,1)=MID$(S$,I,1) THEN K=1
80 NEXT J
90 IF K=0 THEN U$=U$+MID$(S$,I,1)
100 NEXT I
110 DIM R(LEN(U$)-1)
120 FOR I=1 TO LEN(U$)
130 C=0
140 FOR J=1 TO LEN(S$)
150 IF MID$(U$,I,1)=MID$(S$,J,1) THEN C=C+1
160 NEXT J
170 R(I-1)=(C/LEN(S$))*FN L(C/LEN(S$))
180 NEXT I
190 E=0
200 FOR I=0 TO LEN(U$)-1
210 E=E-R(I)
220 NEXT I
230 PRINT E
```

{{out}}

```txt
1.84643935
```


=
## Sinclair ZX81 BASIC
=
Works with 1k of RAM.

```basic
 10 LET X$="1223334444"
 20 LET U$=""
 30 FOR I=1 TO LEN X$
 40 LET K=0
 50 FOR J=1 TO LEN U$
 60 IF U$(J)=X$(I) THEN LET K=K+1
 70 NEXT J
 80 IF K=0 THEN LET U$=U$+X$(I)
 90 NEXT I
100 DIM R(LEN U$)
110 FOR I=1 TO LEN U$
120 LET C=0
130 FOR J=1 TO LEN X$
140 IF U$(I)=X$(J) THEN LET C=C+1
150 NEXT J
160 LET R(I)=C/LEN X$*LN (C/LEN X$)/LN 2
170 NEXT I
180 LET E=0
190 FOR I=1 TO LEN U$
200 LET E=E-R(I)
210 NEXT I
220 PRINT E
```

{{out}}

```txt
1.8464393
```



## BBC BASIC

{{trans|APL}}

```bbcbasic
REM>
entropy
PRINT FNentropy("1223334444")
END
:
DEF FNentropy(x$)
LOCAL unique$, count%, n%, ratio(), u%, i%, j%
unique$ = ""
n% = LEN x$
FOR i% = 1 TO n%
  IF INSTR(unique$, MID$(x$, i%, 1)) = 0 THEN unique$ += MID$(x$, i%, 1)
NEXT
u% = LEN unique$
DIM ratio(u% - 1)
FOR i% = 1 TO u%
  count% = 0
  FOR j% = 1 TO n%
    IF MID$(unique$, i%, 1) = MID$(x$, j%, 1) THEN count% += 1
  NEXT
  ratio(i% - 1) = (count% / n%) * FNlogtwo(count% / n%)
NEXT
= -SUM(ratio())
:
DEF FNlogtwo(n)
= LN n / LN 2
```

{{out}}

```txt
1.84643934
```



## Burlesque


```burlesque
blsq ) "1223334444"F:u[vv^^{1\/?/2\/LG}m[?*++
1.8464393446710157
```



## C



```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>

#define MAXLEN 100 //maximum string length

int makehist(char *S,int *hist,int len){
	int wherechar[256];
	int i,histlen;
	histlen=0;
	for(i=0;i<256;i++)wherechar[i]=-1;
	for(i=0;i<len;i++){
		if(wherechar[(int)S[i]]==-1){
			wherechar[(int)S[i]]=histlen;
			histlen++;
		}
		hist[wherechar[(int)S[i]]]++;
	}
	return histlen;
}

double entropy(int *hist,int histlen,int len){
	int i;
	double H;
	H=0;
	for(i=0;i<histlen;i++){
		H-=(double)hist[i]/len*log2((double)hist[i]/len);
	}
	return H;
}

int main(void){
	char S[MAXLEN];
	int len,*hist,histlen;
	double H;
	scanf("%[^\n]",S);
	len=strlen(S);
	hist=(int*)calloc(len,sizeof(int));
	histlen=makehist(S,hist,len);
	//hist now has no order (known to the program) but that doesn't matter
	H=entropy(hist,histlen,len);
	printf("%lf\n",H);
	return 0;
}
```

Examples:
<lang>$ ./entropy
1223334444
1.846439
$ ./entropy
Rosetta Code is the best site in the world!
3.646513
```



## C++


```cpp
#include <string>
#include <map>
#include <iostream>
#include <algorithm>
#include <cmath>

double log2( double number ) {
   return log( number ) / log( 2 ) ;
}

int main( int argc , char *argv[ ] ) {
   std::string teststring( argv[ 1 ] ) ;
   std::map<char , int> frequencies ;
   for ( char c : teststring )
     frequencies[ c ] ++ ;
   int numlen = teststring.length( ) ;
   double infocontent = 0 ;
   for ( std::pair<char , int> p : frequencies ) {
      double freq = static_cast<double>( p.second ) / numlen ;
      infocontent -= freq * log2( freq ) ;
   }

   std::cout << "The information content of " << teststring
      << " is " << infocontent << std::endl ;
   return 0 ;
}
```

{{out}}

```txt
(entropy "1223334444")
The information content of 1223334444 is 1.84644
```



## Clojure


```Clojure
(defn entropy [s]
  (let [len (count s), log-2 (Math/log 2)]
    (->> (frequencies s)
         (map (fn [[_ v]]
                (let [rf (/ v len)]
                  (-> (Math/log rf) (/ log-2) (* rf) Math/abs))))
         (reduce +))))
```

{{out}}

```Clojure
(entropy "1223334444")
1.8464393446710154
```


## C#

Translation of C++.

```c#

using System;
using System.Collections.Generic;
namespace Entropy
{
	class Program
	{
		public static double logtwo(double num)
		{
			return Math.Log(num)/Math.Log(2);
		}
		public static void Main(string[] args)
		{
		label1:
			string input = Console.ReadLine();
			double infoC=0;
			Dictionary<char,double> table = new Dictionary<char, double>();


			foreach (char c in input)
			{
				if (table.ContainsKey(c))
					table[c]++;
				    else
				    	table.Add(c,1);

			}
			double freq;
			foreach (KeyValuePair<char,double> letter in table)
			{
				freq=letter.Value/input.Length;
				infoC+=freq*logtwo(freq);
			}
			infoC*=-1;
			Console.WriteLine("The Entropy of {0} is {1}",input,infoC);
			goto label1;

		}
	}
}

```

{{out}}

```txt
The Entropy of 1223334444 is 1.84643934467102
```

Without using Hashtables or Dictionaries:

```c#
using System;
namespace Entropy
{
	 class Program
	{
		public static double logtwo(double num)
		{
			return Math.Log(num)/Math.Log(2);
		}
		static double Contain(string x,char k)
		{
			double count=0;
			foreach (char Y in x)
			{
				if(Y.Equals(k))
					count++;
			}
			return count;
		}
		public static void Main(string[] args)
		{
		label1:
			string input = Console.ReadLine();
			double infoC=0;
			double freq;
			string k="";
			foreach (char c1 in input)
			{
				if (!(k.Contains(c1.ToString())))
					k+=c1;
			}
			foreach (char c in k)
			{
				freq=Contain(input,c)/(double)input.Length;
				infoC+=freq*logtwo(freq);
			}
			infoC/=-1;
			Console.WriteLine("The Entropy of {0} is {1}",input,infoC);
			goto label1;

		}
	}
}
```



## CoffeeScript


```coffeescript
entropy = (s) ->
    freq = (s) ->
        result = {}
        for ch in s.split ""
            result[ch] ?= 0
            result[ch]++
        return result

    frq = freq s
    n = s.length
    ((frq[f]/n for f of frq).reduce ((e, p) -> e - p * Math.log(p)), 0) * Math.LOG2E

console.log "The entropy of the string '1223334444' is #{entropy '1223334444'}"
```

{{out}}

```txt
The entropy of the string '1223334444' is 1.8464393446710157
```



## Common Lisp


Not very Common Lisp-y version:


```lisp
(defun entropy (string)
  (let ((table (make-hash-table :test 'equal))
        (entropy 0))
    (mapc (lambda (c) (setf (gethash c table) (+ (gethash c table 0) 1)))
          (coerce string 'list))
    (maphash (lambda (k v)
               (decf entropy (* (/ v (length input-string))
                                (log (/ v (length input-string)) 2))))
             table)
    entropy))
```


More like Common Lisp version:


```lisp
(defun entropy (string &aux (length (length string)))
  (declare (type string string))
  (let ((table (make-hash-table)))
    (loop for char across string
          do (incf (gethash char table 0)))
    (- (loop for freq being each hash-value in table
             for freq/length = (/ freq length)
             sum (* freq/length (log freq/length 2))))))
```



## D


```d
import std.stdio, std.algorithm, std.math;

double entropy(T)(T[] s)
pure nothrow if (__traits(compiles, s.sort())) {
    immutable sLen = s.length;
    return s
           .sort()
           .group
           .map!(g => g[1] / double(sLen))
           .map!(p => -p * p.log2)
           .sum;
}

void main() {
    "1223334444"d.dup.entropy.writeln;
}
```

{{out}}

```txt
1.84644
```



## EchoLisp


```scheme

(lib 'hash)
;; counter: hash-table[key]++
(define (count++ ht k )
(hash-set ht k (1+ (hash-ref! ht k 0))))

(define (hi count n )
	(define pi (// count n))
	(- (* pi (log2 pi))))

;; (H [string|list]) → entropy (bits)
(define (H info)
	(define S (if(string? info) (string->list info) info))
	(define ht (make-hash))
	(define n (length S))

	(for ((s S)) (count++ ht s))
	(for/sum ((s (make-set S)))  (hi (hash-ref ht s) n)))


```

{{out}}

```scheme

;; by increasing entropy

(H "🔴")  → 0
(H "🔵🔴") → 1
(H "1223334444") → 1.8464393446710154
(H "♖♘♗♕♔♗♘♖♙♙♙♙♙♙♙♙♙") → 2.05632607578088
(H "EchoLisp")  → 3
(H "Longtemps je me suis couché de bonne heure") → 3.860828877124944
(H "azertyuiopmlkjhgfdsqwxcvbn") → 4.700439718141092
(H (for/list ((i 1000)) (random 1000)))  → 9.13772704467521
(H (for/list ((i 100_000)) (random 100_000))) → 15.777516877140766
(H (for/list ((i 1000_000)) (random 1000_000))) → 19.104028424596976


```


## Elena

{{trans|C#}}
ELENA 4.x :

```elena
import system'math;
import system'collections;
import system'routines;
import extensions;

extension op
{
    logTwo()
        = self.ln() / 2.ln();
}

public program()
{
    var input := console.readLine();
    var infoC := 0.0r;
    var table := new Dictionary();

    input.forEach:(ch)
    {
        var n := table[ch];
        if (nil == n)
        {
            table[ch] := 1
        }
        else
        {
            table[ch] := n + 1
        }
    };

    var freq := 0;
    table.forEach:(letter)
    {
        freq := letter.toInt().realDiv(input.Length);

        infoC += (freq * freq.logTwo())
    };

    infoC *= -1;

    console.printLine("The Entropy of ", input, " is ", infoC)
}
```

{{out}}

```txt

The Entropy of 1223334444 is 1.846439344671

```



## Elixir

{{works with|Erlang/OTP|18}}
<code>:math.log2</code> was added in OTP 18.

```elixir
defmodule RC do
  def entropy(str) do
    leng = String.length(str)
    String.graphemes(str)
    |> Enum.group_by(&(&1))
    |> Enum.map(fn{_,value} -> length(value) end)
    |> Enum.reduce(0, fn count, entropy ->
         freq = count / leng
         entropy - freq * :math.log2(freq)
       end)
  end
end

IO.inspect RC.entropy("1223334444")
```


{{out}}

```txt

1.8464393446710154

```



## Emacs Lisp


```lisp
(defun shannon-entropy (input)
  (let ((freq-table (make-hash-table))
	(entropy 0)
	(length (+ (length input) 0.0)))
    (mapcar (lambda (x)
	      (puthash x
		       (+ 1 (gethash x freq-table 0))
		       freq-table))
	    input)
    (maphash (lambda (k v)
	       (set 'entropy (+ entropy
			     (* (/ v length)
				(log (/ v length) 2)))))
	     freq-table)
  (- entropy)))
```


{{out}}
After adding the above to the emacs runtime, you can run
the function interactively in the scratch buffer
as shown below (type ctrl-j at the end of the first line
and the output will be placed by emacs on the second line).

```lisp
(shannon-entropy "1223334444")
1.8464393446710154
```



## Erlang


```Erlang

-module( entropy ).

-export( [shannon/1, task/0] ).

shannon( String ) -> shannon_information_content( lists:foldl(fun count/2, dict:new(), String), erlang:length(String) ).

task() -> shannon( "1223334444" ).



count( Character, Dict ) -> dict:update_counter( Character, 1, Dict ).

shannon_information_content( Dict, String_length ) ->
	{_String_length, Acc} = dict:fold( fun shannon_information_content/3, {String_length, 0.0}, Dict ),
	Acc / math:log( 2 ).

shannon_information_content( _Character, How_many, {String_length, Acc} ) ->
        Frequency = How_many / String_length,
	{String_length, Acc - (Frequency * math:log(Frequency))}.

```


{{out}}

```txt

24> entropy:task().
1.8464393446710157

```



## Euler Math Toolbox


```EulerMathToolbox>
function entropy (s) ...
$  v=strtochar(s);
$  m=getmultiplicities(unique(v),v);
$  m=m/sum(m);
$  return sum(-m*logbase(m,2))
$endfunction
>entropy("1223334444")
 1.84643934467
```



=={{header|F_Sharp|F#}}==

```fsharp
open System

let ld x = Math.Log x / Math.Log 2.

let entropy (s : string) =
    let n = float s.Length
    Seq.groupBy id s
    |> Seq.map (fun (_, vals) -> float (Seq.length vals) / n)
    |> Seq.fold (fun e p -> e - p * ld p) 0.

printfn "%f" (entropy "1223334444")
```

{{out}}

```txt
1.846439
```



## Factor


```factor
USING: assocs kernel math math.functions math.statistics
prettyprint sequences ;
IN: rosetta-code.entropy

: shannon-entropy ( str -- entropy )
    [ length ] [ histogram >alist [ second ] map ] bi
    [ swap / ] with map
    [ dup log 2 log / * ] map-sum neg ;

"1223334444" shannon-entropy .
"Factor is my favorite programming language." shannon-entropy .
```

{{out}}

```txt

1.846439344671015
4.04291723248433

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Entropy this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth
: flog2 ( f -- f ) fln 2e fln f/ ;

create freq 256 cells allot

: entropy ( str len -- f )
  freq 256 cells erase
  tuck
  bounds do
    i c@ cells freq +
    1 swap +!
  loop
  0e
  256 0 do
    i cells freq + @ ?dup if
      s>f dup s>f f/
      fdup flog2 f* f-
    then
  loop
  drop ;

s" 1223334444" entropy f.     \ 1.84643934467102  ok

```



## Fortran


Please find the GNU/linux compilation instructions along with sample run among the comments at the start of the FORTRAN 2008 source.  This program acquires input from the command line argument, thereby demonstrating the fairly new get_command_argument intrinsic subroutine.  The expression of the algorithm is a rough translated of the j solution.  Thank you.

```FORTRAN

!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Tue May 21 21:43:12
!
!a=./f && make $a && OMP_NUM_THREADS=2 $a 1223334444
!gfortran -std=f2008 -Wall -ffree-form -fall-intrinsics f.f08 -o f
! Shannon entropy of 1223334444 is    1.84643936
!
!Compilation finished at Tue May 21 21:43:12

program shannonEntropy
  implicit none
  integer :: num, L, status
  character(len=2048) :: s
  num = 1
  call get_command_argument(num, s, L, status)
  if ((0 /= status) .or. (L .eq. 0)) then
    write(0,*)'Expected a command line argument with some length.'
  else
    write(6,*)'Shannon entropy of '//(s(1:L))//' is ', se(s(1:L))
  endif

contains
  !     algebra
  !
  ! 2**x = y
  ! x*log(2) = log(y)
  ! x = log(y)/log(2)

  !   NB. The j solution
  !   entropy=:  +/@:-@(* 2&^.)@(#/.~ % #)
  !   entropy '1223334444'
  !1.84644

  real function se(s)
    implicit none
    character(len=*), intent(in) :: s
    integer, dimension(256) :: tallies
    real, dimension(256) :: norm
    tallies = 0
    call TallyKey(s, tallies)
    ! J's #/. works with the set of items in the input.
    ! TallyKey is sufficiently close that, with the merge, gets the correct result.
    norm = tallies / real(len(s))
    se = sum(-(norm*log(merge(1.0, norm, norm .eq. 0))/log(2.0)))
  end function se

  subroutine TallyKey(s, counts)
    character(len=*), intent(in) :: s
    integer, dimension(256), intent(out) :: counts
    integer :: i, j
    counts = 0
    do i=1,len(s)
      j = iachar(s(i:i))
      counts(j) = counts(j) + 1
    end do
  end subroutine TallyKey

end program shannonEntropy

```


## FreeBASIC


```FreeBASIC
' version 25-06-2015
' compile with: fbc -s console

Sub calc_entropy(source As String, base_ As Integer)

    Dim As Integer i, sourcelen = Len(source), totalchar(255)
    Dim As Double prop, entropy

    For i = 0 To sourcelen -1
        totalchar(source[i]) += 1
    Next

    Print "Char    count"
    For i = 0 To 255
        If totalchar(i) = 0 Then Continue For
        Print "   "; Chr(i); Using "   ######"; totalchar(i)
        prop = totalchar(i) / sourcelen
        entropy = entropy - (prop * Log (prop) / Log(base_))
    Next

    Print : Print "The Entropy of "; Chr(34); source; Chr(34); " is"; entropy

End Sub

' ------=< MAIN >=------

calc_entropy("1223334444", 2)
Print

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
Char    count
   1        1
   2        2
   3        3
   4        4

The Entropy of "1223334444" is 1.846439344671015
```



## friendly interactive shell

Sort of hacky, but friendly interactive shell isn't really optimized for mathematic tasks (in fact, it doesn't even have associative arrays).


```fishshell
function entropy
    for arg in $argv
        set name count_$arg
        if not count $$name > /dev/null
            set $name 0
            set values $values $arg
        end
        set $name (math $$name + 1)
    end
    set entropy 0
    for value in $values
        set name count_$value
        set entropy (echo "
            scale = 50
            p = "$$name" / "(count $argv)"
            $entropy - p * l(p)
        " | bc -l)
    end
    echo "$entropy / l(2)" | bc -l
end
entropy (echo 1223334444 | fold -w1)
```

{{out}}

```txt
1.84643934467101549345
```


## Go


### Go: Slice version


```go
package main

import (
    "fmt"
    "math"
    "strings"
)

func main(){
    fmt.Println(H("1223334444"))
}

func H(data string) (entropy float64) {
    if data == "" {
        return 0
    }
    for i := 0; i < 256; i++ {
        px := float64(strings.Count(data, string(byte(i)))) / float64(len(data))
        if px > 0 {
	    entropy += -px * math.Log2(px)
	}
    }
    return entropy
}
```

{{out}}

```txt

1.8464393446710154

```



### Go: Map version


```go
package main

import (
    "fmt"
    "math"
)

func main() {
    const s = "1223334444"

    m := map[rune]float64{}
    for _, r := range s {
        m[r]++
    }
    var fm float64
    for _, c := range m {
        hm += c * math.Log2(c)
    }
    const l = float64(len(s))
    fmt.Println(math.Log2(l) - hm/l)
}
```

{{out}}

```txt

1.8464393446710152

```



## Groovy


```groovy
String.metaClass.getShannonEntrophy = {
    -delegate.inject([:]) { map, v -> map[v] = (map[v] ?: 0) + 1; map }.values().inject(0.0) { sum, v ->
        def p = (BigDecimal)v / delegate.size()
        sum + p * Math.log(p) / Math.log(2)
    }
}
```

Testing

```groovy
[ '1223334444': '1.846439344671',
  '1223334444555555555': '1.969811065121',
  '122333': '1.459147917061',
  '1227774444': '1.846439344671',
  aaBBcccDDDD: '1.936260027482',
  '1234567890abcdefghijklmnopqrstuvwxyz': '5.169925004424',
  'Rosetta Code': '3.084962500407' ].each { s, expected ->

    println "Checking $s has a shannon entrophy of $expected"
    assert sprintf('%.12f', s.shannonEntrophy) == expected
}
```

{{out}}

```txt
Checking 1223334444 has a shannon entrophy of 1.846439344671
Checking 1223334444555555555 has a shannon entrophy of 1.969811065121
Checking 122333 has a shannon entrophy of 1.459147917061
Checking 1227774444 has a shannon entrophy of 1.846439344671
Checking aaBBcccDDDD has a shannon entrophy of 1.936260027482
Checking 1234567890abcdefghijklmnopqrstuvwxyz has a shannon entrophy of 5.169925004424
Checking Rosetta Code has a shannon entrophy of 3.084962500407
```



## Haskell


```haskell
import Data.List

main = print $ entropy "1223334444"

entropy :: (Ord a, Floating c) => [a] -> c
entropy = sum . map lg . fq . map genericLength . group . sort
  where lg c = -c * logBase 2 c
        fq c = let sc = sum c in map (/ sc) c
```


{{out}}

```txt
1.8464393446710154
```


=={{header|Icon}} and {{header|Unicon}}==

Hmmm, the 2nd equation sums across the length of the string (for the
example, that would be the sum of 10 terms).  However, the answer cited
is for summing across the <i>different</i> characters in the string
(sum of 4 terms).  The code shown here assumes the latter and works
in Icon and Unicon.  This assumption is consistent with the Wikipedia
description.


```unicon
procedure main(a)
    s := !a | "1223334444"
    write(H(s))
end

procedure H(s)
    P := table(0.0)
    every P[!s] +:= 1.0/*s
    every (h := 0.0) -:= P[c := key(P)] * log(P[c],2)
    return h
end
```


{{out}}

```txt

->en
1.846439344671015
->

```



## J

'''Solution''':
```j
   entropy=:  +/@(-@* 2&^.)@(#/.~ % #)
```

{{out|Example}}

```j
   entropy '1223334444'
1.84644
   entropy i.256
8
   entropy 256$9
0
   entropy 256$0 1
1
   entropy 256$0 1 2 3
2
```


So it looks like entropy is roughly the number of bits which would be needed to ''distinguish between'' each item in the argument (for example, with perfect compression). Note that in some contexts this might not be the same thing as information because the choice of the items themselves might matter. But it's good enough in contexts with a fixed set of symbols.


## Java

{{trans|NetRexx}}
{{trans|REXX}}
{{works with|Java|7+}}

```java5
import java.lang.Math;
import java.util.Map;
import java.util.HashMap;

public class REntropy {

  @SuppressWarnings("boxing")
  public static double getShannonEntropy(String s) {
    int n = 0;
    Map<Character, Integer> occ = new HashMap<>();

    for (int c_ = 0; c_ < s.length(); ++c_) {
      char cx = s.charAt(c_);
      if (occ.containsKey(cx)) {
        occ.put(cx, occ.get(cx) + 1);
      } else {
        occ.put(cx, 1);
      }
      ++n;
    }

    double e = 0.0;
    for (Map.Entry<Character, Integer> entry : occ.entrySet()) {
      char cx = entry.getKey();
      double p = (double) entry.getValue() / n;
      e += p * log2(p);
    }
    return -e;
  }

  private static double log2(double a) {
    return Math.log(a) / Math.log(2);
  }
  public static void main(String[] args) {
    String[] sstr = {
      "1223334444",
      "1223334444555555555",
      "122333",
      "1227774444",
      "aaBBcccDDDD",
      "1234567890abcdefghijklmnopqrstuvwxyz",
      "Rosetta Code",
    };

    for (String ss : sstr) {
      double entropy = REntropy.getShannonEntropy(ss);
      System.out.printf("Shannon entropy of %40s: %.12f%n", "\"" + ss + "\"", entropy);
    }
    return;
  }
}
```

{{out}}

```txt

Shannon entropy of                             "1223334444": 1.846439344671
Shannon entropy of                    "1223334444555555555": 1.969811065278
Shannon entropy of                                 "122333": 1.459147917027
Shannon entropy of                             "1227774444": 1.846439344671
Shannon entropy of                            "aaBBcccDDDD": 1.936260027532
Shannon entropy of   "1234567890abcdefghijklmnopqrstuvwxyz": 5.169925001442
Shannon entropy of                           "Rosetta Code": 3.084962500721

```



## JavaScript

{{works with|ECMAScript 2015}}
Calculate the entropy of a string by determining the frequency of each character, then summing each character's probability multiplied by the log base 2 of that same probability, taking the negative of the sum.

```JavaScript
// Shannon entropy in bits per symbol.
function entropy(str) {
  const len = str.length

  // Build a frequency map from the string.
  const frequencies = Array.from(str)
    .reduce((freq, c) => (freq[c] = (freq[c] || 0) + 1) && freq, {})

  // Sum the frequency of each character.
  return Object.values(frequencies)
    .reduce((sum, f) => sum - f/len * Math.log2(f/len), 0)
}

console.log(entropy('1223334444'))        // 1.8464393446710154
console.log(entropy('0'))                 // 0
console.log(entropy('01'))                // 1
console.log(entropy('0123'))              // 2
console.log(entropy('01234567'))          // 3
console.log(entropy('0123456789abcdef'))  // 4
```

{{out}}

```txt
1.8464393446710154
0
1
2
3
4
```



## JavaScript


```JavaScript
const entropy = (s) => {
  const split = s.split('');
  const counter = {};
  split.forEach(ch => {
    if (!counter[ch]) counter[ch] = 1;
    else counter[ch]++;
  });


  const lengthf = s.length * 1.0;
  const counts = Object.values(counter);
  return -1 * counts
    .map(count => count / lengthf * Math.log2(count / lengthf))
    .reduce((a, b) => a + b);
};
```

{{out}}

```txt
console.log(entropy("1223334444")); // 1.8464393446710154
```



## jq

For efficiency with long strings, we use a hash (a JSON object)
to compute the frequencies.

The helper function, ''counter'', could be defined as an inner function of ''entropy'', but for the sake of clarity and because it is independently useful,
it is defined separately.

```jq
# Input: an array of strings.
# Output: an object with the strings as keys, the values of which are the corresponding frequencies.
def counter:
  reduce .[] as $item ( {}; .[$item] += 1 ) ;

# entropy in bits of the input string
def entropy:
  (explode | map( [.] | implode ) | counter
    | [ .[] | . * log ] | add) as $sum
  | ((length|log) - ($sum / length)) / (2|log) ;
```


{{out|Example}}

```jq
"1223334444" | entropy # => 1.8464393446710154
```



## Jsish

From Javascript entry.

```javascript
/* Shannon entropy, in Jsish */

function values(obj:object):array {
    var vals = [];
        for (var key in obj) vals.push(obj[key]);
    return vals;
}

function entropy(s) {
    var split = s.split('');
    var counter = {};
    split.forEach(function(ch) {
        if (!counter[ch]) counter[ch] = 1;
        else counter[ch]++;
    });

    var lengthf = s.length * 1.0;
    var counts = values(counter);
    return -1 * counts.map(function(count) {
        return count / lengthf * (Math.log(count / lengthf) / Math.log(2));
        })
        .reduce(function(a, b) { return a + b; }
    );
};

if (Interp.conf('unitTest')) {
;    entropy('1223334444');
;    entropy('Rosetta Code');
;    entropy('password');
}
```


{{out}}

```txt
prompt$ jsish --U entropy.jsi
entropy('1223334444') ==> 1.84643934467102
entropy('Rosetta Code') ==> 3.08496250072116
entropy('password') ==> 2.75
```



## Julia

{{works with|Julia|0.6}}


```julia
entropy(s) = -sum(x -> x * log(2, x), count(x -> x == c, s) / length(s) for c in unique(s))
@show entropy("1223334444")
@show entropy([1, 2, 3, 1, 2, 1, 2, 3, 1, 2, 3, 4, 5])
```


{{out}}

```txt
entropy("1223334444") = 1.8464393446710154
entropy([1, 2, 3, 1, 2, 1, 2, 3, 1, 2, 3, 4, 5]) = 2.103909910282364
```



## Kotlin


```scala
// version 1.0.6

fun log2(d: Double) = Math.log(d) / Math.log(2.0)

fun shannon(s: String): Double {
    val counters = mutableMapOf<Char, Int>()
    for (c in s) {
        if (counters.containsKey(c)) counters[c] = counters[c]!! + 1
        else counters.put(c, 1)
    }
    val nn = s.length.toDouble()
    var sum = 0.0
    for (key in counters.keys) {
       val term = counters[key]!! / nn
       sum += term * log2(term)
    }
    return -sum
}

fun main(args: Array<String>) {
    val samples = arrayOf(
        "1223334444",
        "1223334444555555555",
        "122333",
        "1227774444",
        "aaBBcccDDDD",
        "1234567890abcdefghijklmnopqrstuvwxyz",
        "Rosetta Code"
    )
    println("            String                             Entropy")
    println("------------------------------------      ------------------")
    for (sample in samples) println("${sample.padEnd(36)}  ->  ${"%18.16f".format(shannon(sample))}")
}
```


{{out}}

```txt

            String                             Entropy
------------------------------------      ------------------
1223334444                            ->  1.8464393446710154
1223334444555555555                   ->  1.9698110652780971
122333                                ->  1.4591479170272448
1227774444                            ->  1.8464393446710154
aaBBcccDDDD                           ->  1.9362600275315274
1234567890abcdefghijklmnopqrstuvwxyz  ->  5.1699250014423095
Rosetta Code                          ->  3.0849625007211556

```



## Liberty BASIC


```lb

dim countOfChar( 255) ' all possible one-byte ASCII chars

    source$    ="1223334444"
    charCount  =len( source$)
    usedChar$  =""

    for i =1 to len( source$)   '   count which chars are used in source
        ch$             =mid$( source$, i, 1)
        if not( instr( usedChar$, ch$)) then usedChar$ =usedChar$ +ch$
        'currentCh$      =mid$(
        j               =instr( usedChar$, ch$)
        countOfChar( j) =countOfChar( j) +1
    next i

    l =len( usedChar$)
    for i =1 to l
        probability =countOfChar( i) /charCount
        entropy     =entropy -( probability *logBase( probability, 2))
    next i

    print " Characters used and the number of occurrences of each "
    for i =1 to l
        print " '"; mid$( usedChar$, i, 1); "'", countOfChar( i)
    next i

    print " Entropy of '"; source$; "' is  "; entropy; " bits."
    print " The result should be around 1.84644 bits."

    end
    function logBase( x, b) '   in LB log() is base 'e'.
        logBase =log( x) /log( 2)
    end function

```

{{Out}}

```txt
 Characters used and the number of occurrences of each
 '1'          1
 '2'          2
 '3'          3
 '4'          4
 Entropy of '1223334444' is  1.84643934 bits.
 The result should be around 1.84644 bits.
```





## Lang5


```lang5
: -rot rot rot ; [] '__A set : dip swap __A swap 1 compress append '__A
set execute __A -1 extract nip ; : nip swap drop ; : sum '+ reduce ;
: 2array 2 compress ; : comb "" split ; : lensize length nip ;
: <group> #( a -- 'a )
    grade subscript dup 's dress distinct strip
    length 1 2array reshape swap
    'A set
    : `filter(*)  A in A swap select ;
    '`filter apply
    ;

: elements(*)  lensize ;
: entropy #( s -- n )
    length "<group> 'elements apply" dip /
    dup neg swap log * 2 log / sum ;

"1223334444" comb entropy . # 1.84643934467102
```



## Lua


```Lua
function log2 (x) return math.log(x) / math.log(2) end

function entropy (X)
    local N, count, sum, i = X:len(), {}, 0
    for char = 1, N do
        i = X:sub(char, char)
        if count[i] then
            count[i] = count[i] + 1
        else
            count[i] = 1
        end
    end
    for n_i, count_i in pairs(count) do
        sum = sum + count_i / N * log2(count_i / N)
    end
    return -sum
end

print(entropy("1223334444"))
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
shE[s_String] := -Plus @@ ((# Log[2., #]) & /@ ((Length /@ Gather[#])/
         Length[#]) &[Characters[s]])
```

{{out|Example}}
```Mathematica
 shE["1223334444"]
1.84644
shE["Rosetta Code"]
3.08496
```


=={{header|MATLAB}} / {{header|Octave}}==
This version allows for any input vectors,
including strings, floats, negative integers, etc.

```MATLAB
function E = entropy(d)
	if ischar(d), d=abs(d); end;
        [Y,I,J] = unique(d);
	H = sparse(J,1,1);
	p = full(H(H>0))/length(d);
	E = -sum(p.*log2(p));
end;
```

{{out|Usage}}

```matlab

 entropy('1223334444')
ans =  1.8464
```



## MiniScript


```MiniScript
entropy = function(s)
    count = {}
    for c in s
        if count.hasIndex(c) then count[c] = count[c]+1 else count[c] = 1
    end for
    sum = 0
    for x in count.values
        countOverN = x / s.len
        sum = sum + countOverN * log(countOverN, 2)
    end for
    return -sum
end function

print entropy("1223334444")
```


{{out}}

```txt
1.846439
```



## NetRexx

{{trans|REXX}}

```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols

runSample(Arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/* REXX ***************************************************************
 * 28.02.2013 Walter Pachl
 **********************************************************************/
method getShannonEntropy(s = "1223334444") public static
--trace var occ c chars n cn i e p pl
  Numeric Digits 30
  occ = 0
  chars = ''
  n = 0
  cn = 0
  Loop i = 1 To s.length()
    c = s.substr(i, 1)
    If chars.pos(c) = 0 Then Do
      cn = cn + 1
      chars = chars || c
      End
    occ[c] = occ[c] + 1
    n = n + 1
    End i
  p = ''
  Loop ci = 1 To cn
    c = chars.substr(ci, 1)
    p[c] = occ[c] / n
    End ci
  e = 0
  Loop ci = 1 To cn
    c = chars.substr(ci, 1)
    pl = log2(p[c])
    e = e + p[c] * pl
    End ci
  Return -e

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method log2(a = double) public static binary returns double
  return Math.log(a) / Math.log(2)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(Arg) public static
  parse Arg sstr
  if sstr = '' then
    sstr = '1223334444' -
           '1223334444555555555' -
           '122333' -
           '1227774444' -
           'aaBBcccDDDD' -
           '1234567890abcdefghijklmnopqrstuvwxyz' -
           'Rosetta_Code'
  say 'Calculating Shannon''s entropy for the following list:'
  say '['(sstr.space(1, ',')).changestr(',', ', ')']'
  say
  entropies = 0
  ssMax = 0
  -- This crude sample substitutes a '_' character for a space in the input strings
  loop w_ = 1 to sstr.words()
    ss = sstr.word(w_)
    ssMax = ssMax.max(ss.length())
    ss_ = ss.changestr('_', ' ')
    entropy = getShannonEntropy(ss_)
    entropies[ss] = entropy
    end w_
  loop report = 1 to sstr.words()
    ss = sstr.word(report)
    ss_ = ss.changestr('_', ' ')
    Say 'Shannon entropy of' ('"'ss_'"').right(ssMax + 2)':' entropies[ss].format(null, 12)
    end report
  return

```

{{out}}

```txt

Calculating Shannon's entropy for the following list:
[1223334444, 1223334444555555555, 122333, 1227774444, aaBBcccDDDD, 1234567890abcdefghijklmnopqrstuvwxyz, Rosetta_Code]

Shannon entropy of                           "1223334444": 1.846439344671
Shannon entropy of                  "1223334444555555555": 1.969811065278
Shannon entropy of                               "122333": 1.459147917027
Shannon entropy of                           "1227774444": 1.846439344671
Shannon entropy of                          "aaBBcccDDDD": 1.936260027532
Shannon entropy of "1234567890abcdefghijklmnopqrstuvwxyz": 5.169925001442
Shannon entropy of                         "Rosetta Code": 3.084962500721

```



## Nim


```nim
import tables, math

proc entropy(s): float =
  var t = initCountTable[char]()
  for c in s: t.inc(c)
  for x in t.values: result -= x/s.len * log2(x/s.len)

echo entropy("1223334444")
```















## Objeck



```objeck
use Collection;

class Entropy {
  function : native : GetShannonEntropy(result : String) ~ Float {
    frequencies := IntMap->New();

    each(i : result) {
      c := result->Get(i);

      if(frequencies->Has(c)) {
        count := frequencies->Find(c)->As(IntHolder);
        count->Set(count->Get() + 1);
      }
      else {
        frequencies->Insert(c, IntHolder->New(1));
      };
    };

    length := result->Size();
    entropy := 0.0;

    counts := frequencies->GetValues();
    each(i : counts) {
      count := counts->Get(i)->As(IntHolder)->Get();
      freq := count->As(Float) / length;
      entropy += freq * (freq->Log() / 2.0->Log());
    };

    return -1 * entropy;
  }

  function : Main(args : String[]) ~ Nil {
    inputs := [
      "1223334444",
      "1223334444555555555",
      "122333",
      "1227774444",
      "aaBBcccDDDD",
      "1234567890abcdefghijklmnopqrstuvwxyz",
      "Rosetta Code"];

    each(i : inputs) {
      input := inputs[i];
      "Shannon entropy of '{$input}': "->Print();
      GetShannonEntropy(inputs[i])->PrintLine();
    };
  }
}
```


Output:

```txt

Shannon entropy of '1223334444': 1.84644
Shannon entropy of '1223334444555555555': 1.96981
Shannon entropy of '122333': 1.45915
Shannon entropy of '1227774444': 1.84644
Shannon entropy of 'aaBBcccDDDD': 1.93626
Shannon entropy of '1234567890abcdefghijklmnopqrstuvwxyz': 5.16993
Shannon entropy of 'Rosetta Code': 3.08496

```



## OCaml


```ocaml
(* generic OCaml, using a mutable Hashtbl *)

(* pre-bake & return an inner-loop function to bin & assemble a character frequency map *)
let get_fproc (m: (char, int) Hashtbl.t) :(char -> unit)  =
  (fun (c:char) -> try
                     Hashtbl.replace m c ( (Hashtbl.find m c) + 1)
                   with Not_found -> Hashtbl.add m c 1)


(* pre-bake and return an inner-loop function to do the actual entropy calculation *)
let get_calc (slen:int) :(float -> float) =
  let slen_float = float_of_int slen in
  let log_2 = log 2.0 in

  (fun v -> let pt = v /. slen_float in
                pt *. ((log pt) /. log_2) )


(* main function, given a string argument it:
       builds a (mutable) frequency map (initial alphabet size of 255, but it's auto-expanding),
       extracts the relative probability values into a list,
       folds-in the basic entropy calculation and returns the result. *)
let shannon (s:string) :float  =
  let freq_hash = Hashtbl.create 255 in
  String.iter (get_fproc freq_hash) s;

  let relative_probs = Hashtbl.fold (fun k v b -> (float v)::b) freq_hash [] in
  let calc = get_calc (String.length s) in

   -1.0 *. List.fold_left (fun b x -> b +. calc x) 0.0 relative_probs

```


'''output:'''

  1.84643934467


## Oforth



```Oforth
: entropy(s) -- f
| freq sz |
   s size dup ifZero: [ return ] asFloat ->sz
   ListBuffer initValue(255, 0) ->freq
   s apply( #[ dup freq at 1+ freq put ] )
   0.0 freq applyIf( #[ 0 <> ], #[ sz / dup ln * - ] ) Ln2 / ;

entropy("1223334444") .
```


{{out}}

```txt
1.84643934467102
```



## ooRexx

{{trans|REXX}}

```oorexx
/* REXX */
Numeric Digits 16
Parse Arg s
If s='' Then
  s="1223334444"
occ.=0
chars=''
n=0
cn=0
Do i=1 To length(s)
  c=substr(s,i,1)
  If pos(c,chars)=0 Then Do
    cn=cn+1
    chars=chars||c
    End
  occ.c=occ.c+1
  n=n+1
  End
do ci=1 To cn
  c=substr(chars,ci,1)
  p.c=occ.c/n
  /* say c p.c */
  End
e=0
Do ci=1 To cn
  c=substr(chars,ci,1)
  e=e+p.c*rxcalclog(p.c)/rxcalclog(2)
  End
Say s 'Entropy' format(-e,,12)
Exit

::requires 'rxmath' LIBRARY
```

{{out}}

```txt
1223334444 Entropy 1.846439344671
```


## Pascal


Free Pascal (http://freepascal.org).


```Pascal

PROGRAM entropytest;

USES StrUtils, Math;

TYPE FArray = ARRAY of CARDINAL;

VAR	 strng: STRING = '1223334444';

// list unique characters in a string
FUNCTION uniquechars(str: STRING): STRING;
	VAR n: CARDINAL;
	BEGIN
		uniquechars := '';
		FOR n := 1 TO length(str) DO
			IF (PosEx(str[n],str,n)>0)
				AND (PosEx(str[n],uniquechars,1)=0)
					THEN uniquechars += str[n];
	END;

// obtain a list of character-frequencies for a string
//  given a string containing its unique characters
FUNCTION frequencies(str,ustr: STRING): FArray;
	VAR u,s,p,o: CARDINAL;
	BEGIN
		SetLength(frequencies, Length(ustr)+1);
		p := 0;
		FOR u := 1 TO length(ustr) DO
			FOR s := 1 TO length(str) DO BEGIN
				o := p;	p := PosEx(ustr[u],str,s);
				IF (p>o) THEN INC(frequencies[u]);
			END;
	END;

// Obtain the Shannon entropy of a string
FUNCTION entropy(s: STRING): EXTENDED;
	VAR pf : FArray;
		us : STRING;
		i,l: CARDINAL;
	BEGIN
		us := uniquechars(s);
		pf := frequencies(s,us);
		l  := length(s);
		entropy := 0.0;
		FOR i := 1 TO length(us) DO
			entropy -= pf[i]/l * log2(pf[i]/l);
	END;

BEGIN
	Writeln('Entropy of "',strng,'" is ',entropy(strng):2:5, ' bits.');
END.

```


{{out}}

```txt

Entropy of "1223334444" is 1.84644 bits.

```




## PARI/GP


```parigp
entropy(s)=s=Vec(s);my(v=vecsort(s,,8));-sum(i=1,#v,(x->x*log(x))(sum(j=1,#s,v[i]==s[j])/#s))/log(2)
```


```txt
>entropy("1223334444")
%1 = 1.8464393446710154934341977463050452232
```



## Perl


```Perl
sub entropy {
    my %count; $count{$_}++ for @_;
    my $entropy = 0;
    for (values %count) {
        my $p = $_/@_;
        $entropy -= $p * log $p;
    }
    $entropy / log 2
}

print entropy split //, "1223334444";
```



## Perl 6

{{works with|rakudo|2015-09-09}}

```perl6
sub entropy(@a) {
    [+] map -> \p { p * -log p }, bag(@a).values »/» +@a;
}

say log(2) R/ entropy '1223334444'.comb;
```

{{out}}

```txt
1.84643934467102
```


In case we would like to add this function to Perl 6's core, here is one way it could be done:


```perl6
use MONKEY-TYPING;
augment class Bag {
    method entropy {
	[+] map -> \p { - p * log p },
	self.values »/» +self;
    }
}

say '1223334444'.comb.Bag.entropy / log 2;
```



## Phix


```Phix
function log2(atom v)
    return log(v)/log(2)
end function

function entropy(sequence s)
sequence symbols = {},
         counts = {}
    integer N = length(s)
    for i=1 to N do
        object si = s[i]
        integer k = find(si,symbols)
        if k=0 then
            symbols = append(symbols,si)
            counts = append(counts,1)
        else
            counts[k] += 1
        end if
    end for
    atom H = 0
    integer n = length(counts)
    for i=1 to n do
        atom ci = counts[i]/N
        H -= ci*log2(ci)
    end for
    return H
end function

?entropy("1223334444")
```

{{out}}

```txt

1.846439345

```



## PL/I


```pli
*process source xref attributes or(!);
 /*--------------------------------------------------------------------
 * 08.08.2014 Walter Pachl  translated from REXX version 1
 *-------------------------------------------------------------------*/
 ent: Proc Options(main);
 Dcl (index,length,log2,substr) Builtin;
 Dcl sysprint Print;
 Dcl occ(100) Bin fixed(31) Init((100)0);
 Dcl (n,cn,ci,i,pos) Bin fixed(31) Init(0);
 Dcl chars Char(100) Var Init('');
 Dcl s Char(100) Var Init('1223334444');
 Dcl c Char(1);
 Dcl (occf,p(100)) Dec Float(18);
 Dcl e Dec Float(18) Init(0);
 Do i=1 To length(s);
   c=substr(s,i,1);
   pos=index(chars,c);
   If pos=0 Then Do;
     pos=length(chars)+1;
     cn+=1;
     chars=chars!!c;
     End;
   occ(pos)+=1;
   n+=1;
   End;
  do ci=1 To cn;
    occf=occ(ci);
    p(ci)=occf/n;
    End;
  Do ci=1 To cn;
    e=e+p(ci)*log2(p(ci));
    End;
  Put Edit('s='''!!s!!''' Entropy=',-e)(Skip,a,f(15,12));
  End;
```

{{out}}

```txt
s='1223334444' Entropy= 1.846439344671
```



## PowerShell


```PowerShell

function entropy ($string) {
    $n = $string.Length
    $string.ToCharArray() | group | foreach{
        $p = $_.Count/$n
        $i = [Math]::Log($p,2)
        -$p*$i
    } | measure -Sum | foreach Sum
}
entropy "1223334444"

```

<b>Output:</b>

```txt

1.84643934467102

```



## Prolog


{{works with|Swi-Prolog|7.3.3}}

This solution calculates the run-length encoding of the input string to get the relative frequencies of its characters.


```Prolog
:-module(shannon_entropy, [shannon_entropy/2]).

%!	shannon_entropy(+String, -Entropy) is det.
%
%	Calculate the Shannon Entropy of String.
%
%	Example query:
%	==
%	?- shannon_entropy(1223334444, H).
%	H = 1.8464393446710154.
%	==
%
shannon_entropy(String, Entropy):-
	atom_chars(String, Cs)
	,relative_frequencies(Cs, Frequencies)
	,findall(CI
		,(member(_C-F, Frequencies)
		 ,log2(F, L)
		 ,CI is F * L
		 )
		,CIs)
	,foldl(sum, CIs, 0, E)
	,Entropy is -E.

%!	frequencies(+Characters,-Frequencies) is det.
%
%	Calculates the relative frequencies of elements in the list of
%	Characters.
%
%	Frequencies is a key-value list with elements of the form:
%	C-F, where C a character in the list and F its relative
%	frequency in the list.
%
%	Example query:
%	==
%	?- relative_frequencies([a,a,a,b,b,b,b,b,b,c,c,c,a,a,f], Fs).
%	Fs = [a-0.3333333333333333, b-0.4, c-0.2,f-0.06666666666666667].
%	==
%
relative_frequencies(List, Frequencies):-
	run_length_encoding(List, Rle)
        % Sort Run-length encoded list and aggregate lengths by element
	,keysort(Rle, Sorted_Rle)
	,group_pairs_by_key(Sorted_Rle, Elements_Run_lengths)
	,length(List, Elements_in_list)
	,findall(E-Frequency_of_E
		,(member(E-RLs, Elements_Run_lengths)
                 % Sum the list of lengths of runs of E
		 ,foldl(plus, RLs, 0, Occurences_of_E)
		 ,Frequency_of_E is Occurences_of_E / Elements_in_list
		 )
		,Frequencies).


%!	run_length_encoding(+List, -Run_length_encoding) is det.
%
%	Converts a list to its run-length encoded form where each "run"
%	of contiguous repeats of the same element is replaced by that
%	element and the length of the run.
%
%	Run_length_encoding is a key-value list, where each element is a
%	term:
%
%	Element:term-Repetitions:number.
%
%	Example query:
%	==
%       ?- run_length_encoding([a,a,a,b,b,b,b,b,b,c,c,c,a,a,f], RLE).
%	RLE = [a-3, b-6, c-3, a-2, f-1].
%	==
%
run_length_encoding([], []-0):-
	!. % No more results needed.

run_length_encoding([Head|List], Run_length_encoded_list):-
	run_length_encoding(List, [Head-1], Reversed_list)
	% The resulting list is in reverse order due to the head-to-tail processing
	,reverse(Reversed_list, Run_length_encoded_list).

%!	run_length_encoding(+List,+Initialiser,-Accumulator) is det.
%
%	Business end of run_length_encoding/3. Calculates the run-length
%	encoded form of a list and binds the result to the Accumulator.
%	Initialiser is a list [H-1] where H is the first element of the
%	input list.
%
run_length_encoding([], Fs, Fs).

% Run of F consecutive occurrences of C
run_length_encoding([C|Cs],[C-F|Fs], Acc):-
        % Backtracking would produce successive counts
	% of runs of C at different indices in the list.
	!
	,F_ is F + 1
	,run_length_encoding(Cs, [C-F_| Fs], Acc).

% End of a run of consecutive identical elements.
run_length_encoding([C|Cs], Fs, Acc):-
	run_length_encoding(Cs,[C-1|Fs], Acc).


/* Arithmetic helper predicates */

%!	log2(N, L2_N) is det.
%
%	L2_N is the logarithm with base 2 of N.
%
log2(N, L2_N):-
	L_10 is log10(N)
	,L_2 is log10(2)
	,L2_N is L_10 / L_2.

%!	sum(+A,+B,?Sum) is det.
%
%	True when Sum is the sum of numbers A and B.
%
%	Helper predicate to allow foldl/4 to do addition. The following
%	call will raise an error (because there is no predicate +/3):
%	==
%	foldl(+, [1,2,3], 0, Result).
%	==
%
%	This will not raise an error:
%	==
%	foldl(sum, [1,2,3], 0, Result).
%	==
%
sum(A, B, Sum):-
	must_be(number, A)
	,must_be(number, B)
	,Sum is A + B.

```


Example query:


```txt

?- shannon_entropy(1223334444, H).
H = 1.8464393446710154.

```



## PureBasic


```purebasic
#TESTSTR="1223334444"
NewMap uchar.i() : Define.d e

Procedure.d nlog2(x.d) : ProcedureReturn Log(x)/Log(2) : EndProcedure

Procedure countchar(s$, Map uchar())
  If Len(s$)
    uchar(Left(s$,1))=CountString(s$,Left(s$,1))
    s$=RemoveString(s$,Left(s$,1))
    ProcedureReturn countchar(s$, uchar())
  EndIf
EndProcedure

countchar(#TESTSTR,uchar())

ForEach uchar()
  e-uchar()/Len(#TESTSTR)*nlog2(uchar()/Len(#TESTSTR))
Next

OpenConsole()
Print("Entropy of ["+#TESTSTR+"] = "+StrD(e,15))
Input()
```

{{out}}

```txt
Entropy of [1223334444] = 1.846439344671015
```



## Python


### Python: Longer version


```python
from __future__ import division
import math

def hist(source):
    hist = {}; l = 0;
    for e in source:
        l += 1
        if e not in hist:
            hist[e] = 0
        hist[e] += 1
    return (l,hist)

def entropy(hist,l):
    elist = []
    for v in hist.values():
        c = v / l
        elist.append(-c * math.log(c ,2))
    return sum(elist)

def printHist(h):
    flip = lambda (k,v) : (v,k)
    h = sorted(h.iteritems(), key = flip)
    print 'Sym\thi\tfi\tInf'
    for (k,v) in h:
        print '%s\t%f\t%f\t%f'%(k,v,v/l,-math.log(v/l, 2))



source = "1223334444"
(l,h) = hist(source);
print '.[Results].'
print 'Length',l
print 'Entropy:', entropy(h, l)
printHist(h)
```


{{out}}

```txt

.[Results].
Length 10
Entropy: 1.84643934467
Sym	hi	fi	Inf
1	1.000000	0.100000	3.321928
2	2.000000	0.200000	2.321928
3	3.000000	0.300000	1.736966
4	4.000000	0.400000	1.321928

```



### Python: More succinct version


The <tt>Counter</tt> module is only available in Python >= 2.7.


```python
>>
 import math
>>> from collections import Counter
>>>
>>> def entropy(s):
...     p, lns = Counter(s), float(len(s))
...     return -sum( count/lns * math.log(count/lns, 2) for count in p.values())
...
>>> entropy("1223334444")
1.8464393446710154
>>>
```



### Uses Python 2


```python
def Entropy(text):
    import math
    log2=lambda x:math.log(x)/math.log(2)
    exr={}
    infoc=0
    for each in text:
        try:
            exr[each]+=1
        except:
            exr[each]=1
    textlen=len(text)
    for k,v in exr.items():
        freq  =  1.0*v/textlen
        infoc+=freq*log2(freq)
    infoc*=-1
    return infoc

while True:
    print Entropy(raw_input('>>>'))
```



## R


```r
entropy = function(s)
   {freq = prop.table(table(strsplit(s, '')[1]))
    -sum(freq * log(freq, base = 2))}

print(entropy("1223334444"))   # 1.846439
```



## Racket


```racket
#lang racket
(require math)
(provide entropy hash-entropy list-entropy digital-entropy)

(define (hash-entropy h)
  (define (log2 x) (/ (log x) (log 2)))
  (define n (for/sum [(c (in-hash-values h))] c))
  (- (for/sum ([c (in-hash-values h)] #:unless (zero? c))
       (* (/ c n) (log2 (/ c n))))))

(define (list-entropy x) (hash-entropy (samples->hash x)))

(define entropy         (compose list-entropy string->list))
(define digital-entropy (compose entropy number->string))

(module+ test
  (require rackunit)
  (check-= (entropy "1223334444") 1.8464393446710154 1E-8)
  (check-= (digital-entropy 1223334444) (entropy "1223334444") 1E-8)
  (check-= (digital-entropy 1223334444) 1.8464393446710154 1E-8)
  (check-= (entropy "xggooopppp") 1.8464393446710154 1E-8))

(module+ main (entropy "1223334444"))
```

{{out}}

```txt
 1.8464393446710154
```



## REXX


### version 1



```rexx
/* REXX ***************************************************************
* 28.02.2013 Walter Pachl
* 12.03.2013 Walter Pachl  typo in log corrected. thanx for testing
* 22.05.2013 -"- extended the logic to accept other strings
* 25.05.2013 -"- 'my' log routine is apparently incorrect
* 25.05.2013 -"- problem identified & corrected
**********************************************************************/
Numeric Digits 30
Parse Arg s
If s='' Then
  s="1223334444"
occ.=0
chars=''
n=0
cn=0
Do i=1 To length(s)
  c=substr(s,i,1)
  If pos(c,chars)=0 Then Do
    cn=cn+1
    chars=chars||c
    End
  occ.c=occ.c+1
  n=n+1
  End
do ci=1 To cn
  c=substr(chars,ci,1)
  p.c=occ.c/n
  /* say c p.c */
  End
e=0
Do ci=1 To cn
  c=substr(chars,ci,1)
  e=e+p.c*log(p.c,30,2)
  End
Say 'Version 1:' s 'Entropy' format(-e,,12)
Exit

log: Procedure
/***********************************************************************
* Return log(x) -- with specified precision and a specified base
* Three different series are used for the ranges  0 to 0.5
*                                                 0.5 to 1.5
*                                                 1.5 to infinity
* 03.09.1992 Walter Pachl
* 25.05.2013 -"- 'my' log routine is apparently incorrect
* 25.05.2013 -"- problem identified & corrected
***********************************************************************/
  Parse Arg x,prec,b
  If prec='' Then prec=9
  Numeric Digits (2*prec)
  Numeric Fuzz   3
  Select
    When x<=0 Then r='*** invalid argument ***'
    When x<0.5 Then Do
      z=(x-1)/(x+1)
      o=z
      r=z
      k=1
      Do i=3 By 2
        ra=r
        k=k+1
        o=o*z*z
        r=r+o/i
        If r=ra Then Leave
        End
      r=2*r
      End
    When x<1.5 Then Do
      z=(x-1)
      o=z
      r=z
      k=1
      Do i=2 By 1
        ra=r
        k=k+1
        o=-o*z
        r=r+o/i
        If r=ra Then Leave
        End
      End
    Otherwise /* 1.5<=x */ Do
      z=(x+1)/(x-1)
      o=1/z
      r=o
      k=1
      Do i=3 By 2
        ra=r
        k=k+1
        o=o/(z*z)
        r=r+o/i
        If r=ra Then Leave
        End
      r=2*r
      End
    End
  If b<>'' Then
    r=r/log(b,prec)
  Numeric Digits (prec)
  r=r+0
  Return r
```



```rexx
/* REXX ***************************************************************
* Test program to compare Versions 1 and 2
* (the latter tweaked to be acceptable by my (oo)Rexx
* and to give the same output.)
* version 1 was extended to accept the strings of the incorrect flag
* 22.05.2013 Walter Pachl (I won't analyze the minor differences)
* 25.05.2013 I did now analyze and had to discover that
*            'my' log routine is apparently incorrect
* 25.05.2013 problem identified & corrected
*********************************************************************/
Call both '1223334444'
Call both '1223334444555555555'
Call both '122333'
Call both '1227774444'
Call both 'aaBBcccDDDD'
Call both '1234567890abcdefghijklmnopqrstuvwxyz'
Exit
both:
  Parse Arg s
  Call entropy  s
  Call entropy2 s
  Say ' '
  Return

```

{{out}}

```txt
Version 1: 1223334444 Entropy 1.846439344671
Version 2: 1223334444 Entropy 1.846439344671

Version 1: 1223334444555555555 Entropy 1.969811065278
Version 2: 1223334444555555555 Entropy 1.969811065278

Version 1: 122333 Entropy 1.459147917027
Version 2: 122333 Entropy 1.459147917027

Version 1: 1227774444 Entropy 1.846439344671
Version 2: 1227774444 Entropy 1.846439344671

Version 1: 1234567890abcdefghijklmnopqrstuvwxyz Entropy 5.169925001442
Version 2: 1234567890abcdefghijklmnopqrstuvwxyz Entropy 5.169925001442
```



### version 2

REXX doesn't have a BIF for   '''LOG'''   or   '''LN''',   so the subroutine (function)   '''LOG2'''   is included herein.

The   '''LOG2'''   subroutine is only included here for functionality, not to document how to calculate   LOG<sub>2</sub>   using REXX.

```rexx
/*REXX program calculates the   information entropy   for a given character string.     */
numeric digits 50                                /*use 50 decimal digits for precision. */
parse arg $;   if $=''  then $=1223334444        /*obtain the optional input from the CL*/
#=0;           @.=0;         L=length($)         /*define handy-dandy REXX variables.   */
$$=                                              /*initialize the   $$   list.          */
       do j=1  for L;        _=substr($, j, 1)   /*process each character in  $  string.*/
       if @._==0  then do;   #=# + 1             /*Unique?  Yes, bump character counter.*/
                             $$=$$ || _          /*add this character to the  $$  list. */
                       end
       @._=@._ + 1                               /*keep track of this character's count.*/
       end   /*j*/
sum=0                                            /*calculate info entropy for each char.*/
       do i=1  for #;        _=substr($$, i, 1)  /*obtain a character from unique list. */
       sum=sum  -   @._/L * log2(@._/L)          /*add (negatively) the char entropies. */
       end   /*i*/

say ' input string: '   $
say 'string length: '   L
say ' unique chars: '   # ;     say
say 'the information entropy of the string ──► '         format(sum,,12)          " bits."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
log2: procedure;  parse arg x 1 ox;     ig= x>1.5;     ii=0;          is=1 -  2 * (ig\==1)
      numeric digits digits()+5                  /* [↓]  precision of E must be≥digits()*/
      e=2.71828182845904523536028747135266249775724709369995957496696762772407663035354759
           do  while  ig & ox>1.5 | \ig&ox<.5;       _=e;       do j=-1;  iz=ox * _ ** -is
           if j>=0 & (ig & iz<1 | \ig&iz>.5)  then leave;     _=_*_;   izz=iz;  end  /*j*/
           ox=izz;  ii=ii+is*2**j;  end /*while*/;   x=x * e** -ii -1;  z=0;   _=-1;   p=z
               do k=1;   _=-_*x;   z=z+_/k;        if z=p  then leave;  p=z;    end  /*k*/
           r=z+ii;  if arg()==2  then return r;    return r/log2(2,.)
```

'''output'''   when using the default input of:   <tt> 1223334444 </tt>

```txt

 input string:  1223334444
string length:  10
 unique chars:  4

the information entropy of the string ──►  1.846439344671  bits.

```

'''output'''   when using the input of:   <tt> Rosetta Code </tt>

```txt

 input string:  Rosetta Code
string length:  12
 unique chars:  9

the information entropy of the string ──►  3.084962500721  bits.

```



## Ring


```ring

decimals(8)
entropy = 0
countOfChar = list(255)

source="1223334444"
charCount  =len( source)
usedChar  =""

for i =1 to len( source)
     ch =substr(source, i, 1)
     if not(substr( usedChar, ch)) usedChar =usedChar +ch ok
     j  =substr( usedChar, ch)
    countOfChar[j] =countOfChar[j] +1
next

l =len(usedChar)
for i =1 to l
     probability =countOfChar[i] /charCount
     entropy =entropy - (probability *logBase(probability, 2))
next

see "Characters used and the number of occurrences of each " + nl
for i =1 to l
      see "'" + substr(usedChar, i, 1) + "' " + countOfChar[i] + nl
next

see " Entropy of " + source + " is  " + entropy + " bits." + nl
see " The result should be around 1.84644 bits." + nl

func logBase (x, b)
        logBase =log( x) /log( 2)
        return logBase

```

Output:

```txt

Characters used and the number of occurrences of each
'1' 1
'2' 2
'3' 3
'4' 4
Entropy of 1223334444 is  1.84643934 bits.
The result should be around 1.84644 bits.

```



## Ruby

{{works with|Ruby|1.9}}

```ruby
def entropy(s)
  counts = Hash.new(0.0)
  s.each_char { |c| counts[c] += 1 }
  leng = s.length

  counts.values.reduce(0) do |entropy, count|
    freq = count / leng
    entropy - freq * Math.log2(freq)
  end
end

p entropy("1223334444")
```

{{out}}

```txt

1.8464393446710154

```

One-liner, same performance (or better):

```ruby
def entropy2(s)
  s.each_char.group_by(&:to_s).values.map { |x| x.length / s.length.to_f }.reduce(0) { |e, x| e - x*Math.log2(x) }
end
```



## Run BASIC


```runbasic
dim chrCnt( 255) 			' possible ASCII chars

source$		= "1223334444"
numChar		= len(source$)

for i = 1 to len(source$)   		' count which chars are used in source
	ch$	= mid$(source$,i,1)
	if not( instr(chrUsed$, ch$)) then chrUsed$ = chrUsed$ + ch$
	j	= instr(chrUsed$, ch$)
	chrCnt(j) =chrCnt(j) +1
next i

lc	= len(chrUsed$)
for i = 1 to lc
	odds	= chrCnt(i) /numChar
	entropy	= entropy - (odds * (log(odds) / log(2)))
next i

print " Characters used and times used of each "
for i = 1 to lc
	print " '"; mid$(chrUsed$,i,1); "'";chr$(9);chrCnt(i)
next i

print " Entropy of '"; source$; "' is  "; entropy; " bits."

end
```

```txt

Characters used and times used of each
 '1'	1
 '2'	2
 '3'	3
 '4'	4
 Entropy of '1223334444' is  1.84643939 bits.

```



## Rust


```rust
fn entropy(s: &[u8]) -> f32 {
    let mut histogram = [0u64; 256];

    for &b in s {
        histogram[b as usize] += 1;
    }

    histogram
        .iter()
        .cloned()
        .filter(|&h| h != 0)
        .map(|h| h as f32 / s.len() as f32)
        .map(|ratio| -ratio * ratio.log2())
        .sum()
}

fn main() {
    let arg = std::env::args().nth(1).expect("Need a string.");
    println!("Entropy of {} is {}.", arg, entropy(arg.as_bytes()));
}
```

{{out}}

```txt
$ ./entropy 1223334444
Entropy of 1223334444 is 1.8464394.

```



## Scala


```scala
import scala.math._

def entropy( v:String ) = { v
  .groupBy (a => a)
  .values
  .map( i => i.length.toDouble / v.length )
  .map( p => -p * log10(p) / log10(2))
  .sum
}

// Confirm that "1223334444" has an entropy of about 1.84644
assert( math.round( entropy("1223334444") * 100000 ) * 0.00001 == 1.84644 )
```



## scheme

A version capable of calculating multidimensional entropy.

```scheme

(define (entropy input)
  (define (close? a b)
    (define (norm x y)
      (define (infinite_norm m n)
        (define (absminus p q)
             (cond ((null? p) '())
                (else (cons (abs (- (car p) (car q))) (absminus (cdr p) (cdr q))))))
        (define (mm l)
             (cond ((null? (cdr l)) (car l))
                   ((> (car l) (cadr l)) (mm (cons (car l) (cddr l))))
                   (else (mm (cdr l)))))
        (mm (absminus m n)))
      (if (pair? x) (infinite_norm x y) (abs (- x y))))
    (let ((epsilon 0.2))
      (< (norm a b) epsilon)))
  (define (freq-list x)
    (define (f x)
      (define (count a b)
        (cond ((null? b) 1)
              (else (+ (if (close? a (car b)) 1 0) (count a (cdr b))))))
      (let ((t (car x)) (tt (cdr x)))
        (count t tt)))
    (define (g x)
      (define (filter a b)
        (cond ((null? b) '())
              ((close? a (car b)) (filter a (cdr b)))
              (else (cons (car b) (filter a (cdr b))))))
      (let ((t (car x)) (tt (cdr x)))
        (filter t tt)))
    (cond ((null? x) '())
          (else (cons (f x) (freq-list (g x))))))
  (define (scale x)
    (define (sum x)
      (if (null? x) 0.0 (+ (car x) (sum (cdr x)))))
    (let ((z (sum x)))
      (map (lambda(m) (/ m z)) x)))
  (define (cal x)
    (if (null? x) 0 (+ (* (car x) (/ (log (car x)) (log 2))) (cal (cdr x)))))
  (- (cal (scale (freq-list input)))))

(entropy (list 1 2 2 3 3 3 4 4 4 4))
(entropy (list (list 1 1) (list 1.1 1.1) (list 1.2 1.2) (list 1.3 1.3) (list 1.5 1.5) (list 1.6 1.6)))

```


{{out}}

```txt

1.8464393446710154 bits

1.4591479170272448 bits

```



## Scilab

<lang>function E = entropy(d)
    d=strsplit(d);
    n=unique(string(d));
    N=size(d,'r');

    count=zeros(n);
    n_size = size(n,'r');
    for i = 1:n_size
       count(i) = sum ( d == n(i) );
    end

    E=0;
    for i=1:length(count)
        E = E - count(i)/N * log(count(i)/N) / log(2);
    end
endfunction

word ='1223334444';
E = entropy(word);
disp('The entropy of '+word+' is '+string(E)+'.');
```


{{out}}

```txt
 The entropy of 1223334444 is 1.8464393.
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "math.s7i";

const func float: entropy (in string: stri) is func
  result
    var float: entropy is 0.0;
  local
    var hash [char] integer: count is (hash [char] integer).value;
    var char: ch is ' ';
    var float: p is 0.0;
  begin
    for ch range stri do
      if ch in count then
        incr(count[ch]);
      else
        count @:= [ch] 1;
      end if;
    end for;
    for key ch range count do
      p := flt(count[ch]) / flt(length(stri));
      entropy -:= p * log(p) / log(2.0);
    end for;
  end func ;

const proc: main is func
  begin
    writeln(entropy("1223334444") digits 5);
  end func;
```


{{out}}

```txt

1.84644

```



## Sidef


```ruby
func entropy(s) {
  var counts = Hash.new;
  s.each { |c| counts{c} := 0 ++ };
  var len = s.len;
  [0, counts.values.map {|count|
    var freq = count/len; freq * freq.log2 }...
  ]«-»;
}
 
say entropy("1223334444");
```

{{out}}

```txt
1.846439344671015493434197746305045223237
```



## Swift


```swift
import Foundation

func entropy(of x: String) -> Double {
  return x
    .reduce(into: [String: Int](), {cur, char in
      cur[String(char), default: 0] += 1
    })
    .values
    .map({i in Double(i) / Double(x.count) } as (Int) -> Double)
    .map({p in -p * log2(p) } as (Double) -> Double)
    .reduce(0.0, +)
}

print(entropy(of: "1223334444"))
```


{{out}}

```txt
1.8464393446710154
```



## Tcl


```tcl
proc entropy {str} {
    set log2 [expr log(2)]
    foreach char [split $str ""] {dict incr counts $char}
    set entropy 0.0
    foreach count [dict values $counts] {
	set freq [expr {$count / double([string length $str])}]
	set entropy [expr {$entropy - $freq * log($freq)/$log2}]
    }
    return $entropy
}
```

Demonstration:

```tcl
puts [format "entropy = %.5f" [entropy "1223334444"]]
puts [format "entropy = %.5f" [entropy "Rosetta Code"]]
```

{{out}}

```txt

entropy = 1.84644
entropy = 3.08496

```



## XPL0


```XPL0
code real RlOut=48, Ln=54;      \intrinsic routines
string 0;                       \use zero-terminated strings

func StrLen(A);                 \Return number of characters in an ASCIIZ string
char A;
int  I;
for I:= 0, -1>>1-1 do
    if A(I) = 0 then return I;

func real Entropy(Str);         \Return Shannon entropy of string
char Str;
int  Len, I, Count(128);
real Sum, Prob;
[Len:= StrLen(Str);
for I:= 0 to 127 do Count(I):= 0;
for I:= 0 to Len-1 do           \count number of each character in string
    Count(Str(I)):= Count(Str(I)) + 1;
Sum:= 0.0;
for I:= 0 to 127 do
    if Count(I) # 0 then        \(avoid Ln(0.0) error)
        [Prob:= float(Count(I)) / float(Len);   \probability of char in string
        Sum:= Sum + Prob*Ln(Prob);
        ];
return -Sum/Ln(2.0);
];

RlOut(0, Entropy("1223334444"))
```

{{out}}

```txt

    1.84644

```



## zkl

{{trans|D}}

```zkl
fcn entropy(text){
   text.pump(Void,fcn(c,freq){ c=c.toAsc(); freq[c]+=1; freq }
       .fp1( (0).pump(256,List,0.0).copy() )) // array[256] of 0.0
   .filter()		      // remove all zero entries from array
   .apply('/(text.len()))     // (num of char)/len
   .apply(fcn(p){-p*p.log()}) // |p*ln(p)|
   .sum(0.0)/(2.0).log();     // sum * ln(e)/ln(2) to convert to log2
}

entropy("1223334444").println(" bits");
```

{{out}}

```txt

1.84644 bits

```



## ZX Spectrum Basic

{{trans|FreeBASIC}}

```zxbasic
10 LET s$="1223334444": LET base=2: LET entropy=0
20 LET sourcelen=LEN s$
30 DIM t(255)
40 FOR i=1 TO sourcelen
50 LET number= CODE s$(i)
60 LET t(number)=t(number)+1
70 NEXT i
80 PRINT "Char";TAB (6);"Count"
90 FOR i=1 TO 255
100 IF t(i)<>0 THEN PRINT CHR$ i;TAB (6);t(i): LET prop=t(i)/sourcelen: LET entropy=entropy-(prop*(LN prop)/(LN base))
110 NEXT i
120 PRINT '"The Entropy of """;s$;""" is ";entropy
```

