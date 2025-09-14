+++
title = "Permutations with repetitions"
description = ""
date = 2019-05-07T11:19:39Z
aliases = []
[extra]
id = 13370
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_68",
  "applescript",
  "autohotkey",
  "c",
  "cpp",
  "d",
  "echolisp",
  "elixir",
  "erlang",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "k",
  "kotlin",
  "m2000_interpreter",
  "mathematica",
  "maxima",
  "pascal",
  "perl",
  "perl_6",
  "permelemlist_candidate_void",
  "phix",
  "php",
  "picolisp",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "sidef",
  "tcl",
]
+++

## Task

'''Generate''' a sequence of permutations of n elements drawn from choice of k values.  

This sequence will have   <big><math>n^k</math></big>   elements, unless the program decides to terminate early.

Do not store all the intermediate values of the sequence, rather generate them as required, and pass the intermediate result to a deciding routine for combinations selection and/or early generator termination.

For example: When "cracking" a ''"combination"'' lock a sequence is required, but the sequence is terminated once a successful ''"combination"'' is found.  This case is a good example of where it is not required to store all the intermediate '''permutations'''.

'''See Also:'''
## AppleScript


### Strict evaluation of the whole set

Permutations with repetitions, using strict evaluation, generating the entire set (where system constraints permit) with some degree of efficiency. For lazy or interruptible evaluation, see the second example below.


```AppleScript
-- e.g. replicateM(3, {1, 2})) -> 
-- {{1, 1, 1}, {1, 1, 2}, {1, 2, 1}, {1, 2, 2}, {2, 1, 1}, 
--  {2, 1, 2}, {2, 2, 1}, {2, 2, 2}}

-- replicateM :: Int -> [a] -> [[a]]
on replicateM(n, xs)
    script go
        script cons
            on |λ|(a, bs)
                {a} & bs
            end |λ|
        end script
        on |λ|(x)
            if x ≤ 0 then
                {{}}
            else
                liftA2List(cons, xs, |λ|(x - 1))
            end if
        end |λ|
    end script
    
    go's |λ|(n)
end replicateM


-- TEST ------------------------------------------------------------
on run
    
    replicateM(2, {1, 2, 3})
    
    -- {{1, 1}, {1, 2}, {1, 3}, {2, 1}, {2, 2}, {2, 3}, {3, 1}, {3, 2}, {3, 3}}
end run


-- GENERIC FUNCTIONS -----------------------------------------------

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lng to length of xs
    set acc to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set acc to acc & |λ|(item i of xs, i, xs)
        end repeat
    end tell
    return acc
end concatMap

-- liftA2List :: (a -> b -> c) -> [a] -> [b] -> [c]
on liftA2List(f, xs, ys)
    script
        property g : mReturn(f)'s |λ|
        on |λ|(x)
            script
                on |λ|(y)
                    {g(x, y)}
                end |λ|
            end script
            concatMap(result, ys)
        end |λ|
    end script
    concatMap(result, xs)
end liftA2List

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
```

```AppleScript
```



### Lazy evaluation with a generator

Permutations with repetition by treating the <math>n^k</math> elements as an ordered set, and writing a function from a zero-based index to the nth permutation. This allows us terminate a repeated generation on some condition, or explore a sub-set without needing to generate the whole set:

```AppleScript
use AppleScript version "2.4"
use framework "Foundation"
use scripting additions

-- permutesWithRepns :: [a] -> Int -> Generator [[a]]
on permutesWithRepns(xs, n)
    script
        property f : curry3(my nthPermutationWithRepn)'s |λ|(xs)'s |λ|(n)
        property limit : (length of xs) ^ n
        property i : -1
        on |λ|()
            set i to 1 + i
            if i < limit then
                return f's |λ|(i)
            else
                missing value
            end if
        end |λ|
    end script
end permutesWithRepns


-- nthPermutationWithRepn :: [a] -> Int -> Int -> [a]
on nthPermutationWithRepn(xs, intGroup, intIndex)
    set intBase to length of xs
    if intIndex < (intBase ^ intGroup) then
        set ds to baseDigits(intBase, xs, intIndex)
        
        -- With any 'leading zeros' required by length
        replicate(intGroup - (length of ds), item 1 of xs) & ds
    else
        missing value
    end if
end nthPermutationWithRepn


-- baseDigits :: Int -> [a] -> [a]
on baseDigits(intBase, digits, n)
    script
        on |λ|(v)
            if 0 = v then
                Nothing()
            else
                Just(Tuple(item (1 + (v mod intBase)) of digits, ¬
                    v div intBase))
            end if
        end |λ|
    end script
    unfoldr(result, n)
end baseDigits


-- TEST ------------------------------------------------------------------
on run
    set cs to "ACKR"
    set wordLength to 5
    set gen to permutesWithRepns(cs, wordLength)
    
    set i to 0
    set v to gen's |λ|() -- First permutation drawn from series
    set alpha to v
    set psi to alpha
    
    repeat while missing value is not v
        set s to concat(v)
        if "crack" = toLower(s) then
            return ("Permutation " & (i as text) & " of " & ¬
                (((length of cs) ^ wordLength) as integer) as text) & ¬
                ": " & s & linefeed & ¬
                "Found after searching from " & alpha & " thru " & psi
        else
            set i to 1 + i
            set psi to v
        end if
        set v to gen's |λ|()
    end repeat
end run


-- GENERIC ----------------------------------------------------------

-- Just :: a -> Maybe a
on Just(x)
    {type:"Maybe", Nothing:false, Just:x}
end Just

-- Nothing :: Maybe a
on Nothing()
    {type:"Maybe", Nothing:true}
end Nothing

-- Tuple (,) :: a -> b -> (a, b)
on Tuple(a, b)
    {type:"Tuple", |1|:a, |2|:b, length:2}
end Tuple

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

-- curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
on curry3(f)
    script
        on |λ|(a)
            script
                on |λ|(b)
                    script
                        on |λ|(c)
                            |λ|(a, b, c) of mReturn(f)
                        end |λ|
                    end script
                end |λ|
            end script
        end |λ|
    end script
end curry3

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

-- toLower :: String -> String
on toLower(str)
    set ca to current application
    ((ca's NSString's stringWithString:(str))'s ¬
        lowercaseStringWithLocale:(ca's NSLocale's currentLocale())) as text
end toLower

-- > unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
-- > [10,9,8,7,6,5,4,3,2,1] 
-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
on unfoldr(f, v)
    set xr to Tuple(v, v) -- (value, remainder)
    set xs to {}
    tell mReturn(f)
        repeat -- Function applied to remainder.
            set mb to |λ|(|2| of xr)
            if Nothing of mb then
                exit repeat
            else -- New (value, remainder) tuple,
                set xr to Just of mb
                -- and value appended to output list.
                set end of xs to |1| of xr
            end if
        end repeat
    end tell
    return xs
end unfoldr
```

```txt
Permutation 589 of 1024: CRACK
Found after searching from AAAAA thru ARACK
```



## ALGOL 68

'''File: prelude_permutations_with_repetitions.a68'''
```algol68
# -*- coding: utf-8 -*- #

MODE PERMELEMLIST = FLEX[0]PERMELEM;
MODE PERMELEMLISTYIELD = PROC(PERMELEMLIST)VOID;

PROC perm gen elemlist = (FLEX[]PERMELEMLIST master, PERMELEMLISTYIELD yield)VOID:(
  [LWB master:UPB master]INT counter;
  [LWB master:UPB master]PERMELEM out;
  FOR i FROM LWB counter TO UPB counter DO
    INT c = counter[i] := LWB master[i];
    out[i] := master[i][c]
  OD;
  yield(out);
  WHILE TRUE DO
    INT next i := LWB counter;
    counter[next i] +:= 1;
    FOR i FROM LWB counter TO UPB counter WHILE counter[i]>UPB master[i] DO
      INT c = counter[i] := LWB master[i];
      out[i] := master[i][c];
      next i := i + 1;
      IF next i > UPB counter THEN done FI;
      counter[next i] +:= 1
    OD;
    INT c = counter[next i];
    out[next i] := master[next i][c];
    yield(out)
  OD;
  done: SKIP
);

SKIP
```
'''File: test_permutations_with_repetitions.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

MODE PERMELEM = STRING;
PR READ "prelude_permutations_with_repetitions.a68" PR;

INT lead actor = 1, co star = 2;
PERMELEMLIST actors list = ("Chris Ciaffa", "Keith Urban","Tom Cruise",
                            "Katie Holmes","Mimi Rogers","Nicole Kidman");

FLEX[0]PERMELEMLIST combination := (actors list, actors list, actors list, actors list);

FORMAT partner fmt = $g"; "$;
test:(
# FOR PERMELEMELEM candidate in # perm gen elemlist(combination #) DO (#,
##   (PERMELEMLIST candidate)VOID: (
    printf((partner fmt,candidate));
    IF candidate[lead actor] = "Keith Urban" AND candidate[co star]="Nicole Kidman" OR
       candidate[co star] = "Keith Urban" AND candidate[lead actor]="Nicole Kidman" THEN
      print((" => Sunday + Faith as extras", new line)); # children #
      done
    FI;
    print(new line)
# OD #));
  done: SKIP
)
```
'''Output:'''

```txt

Chris Ciaffa; Chris Ciaffa; Chris Ciaffa; Chris Ciaffa; 
Keith Urban; Chris Ciaffa; Chris Ciaffa; Chris Ciaffa; 
Tom Cruise; Chris Ciaffa; Chris Ciaffa; Chris Ciaffa; 
Katie Holmes; Chris Ciaffa; Chris Ciaffa; Chris Ciaffa; 
Mimi Rogers; Chris Ciaffa; Chris Ciaffa; Chris Ciaffa; 
Nicole Kidman; Chris Ciaffa; Chris Ciaffa; Chris Ciaffa; 
Chris Ciaffa; Keith Urban; Chris Ciaffa; Chris Ciaffa; 
Keith Urban; Keith Urban; Chris Ciaffa; Chris Ciaffa; 
Tom Cruise; Keith Urban; Chris Ciaffa; Chris Ciaffa; 
Katie Holmes; Keith Urban; Chris Ciaffa; Chris Ciaffa; 
Mimi Rogers; Keith Urban; Chris Ciaffa; Chris Ciaffa; 
Nicole Kidman; Keith Urban; Chris Ciaffa; Chris Ciaffa;  => Sunday + Faith as extras

```



## AutoHotkey

Use the function from http://rosettacode.org/wiki/Permutations#Alternate_Version with opt=1

```ahk
P(n,k="",opt=0,delim="",str="") { ; generate all n choose k permutations lexicographically
	;1..n = range, or delimited list, or string to parse
	;	to process with a different min index, pass a delimited list, e.g. "0`n1`n2"
	;k = length of result
	;opt 0 = no repetitions
	;opt 1 = with repetitions
	;opt 2 = run for 1..k
	;opt 3 = run for 1..k with repetitions
	;str = string to prepend (used internally)
	;returns delimited string, error message, or (if k > n) a blank string
	i:=0
	If !InStr(n,"`n")
		If n in 2,3,4,5,6,7,8,9
			Loop, %n%
				n := A_Index = 1 ? A_Index : n "`n" A_Index
		Else
			Loop, Parse, n, %delim%
				n := A_Index = 1 ? A_LoopField : n "`n" A_LoopField
	If (k = "")
		RegExReplace(n,"`n","",k), k++
	If k is not Digit
		Return "k must be a digit."
	If opt not in 0,1,2,3
		Return "opt invalid."
	If k = 0
		Return str
	Else
		Loop, Parse, n, `n
			If (!InStr(str,A_LoopField) || opt & 1)
				s .= (!i++ ? (opt & 2 ? str "`n" : "") : "`n" )
					. P(n,k-1,opt,delim,str . A_LoopField . delim)
		Return s
}
```



## C


```d>#include <stdio.h

#include <stdlib.h>

int main(){	
	int temp;
	int numbers=3;
	int a[numbers+1], upto = 4, temp2;
	for( temp2 = 1 ; temp2 <= numbers; temp2++){
		a[temp2]=1;
	}
	a[numbers]=0;
	temp=numbers;
	while(1){
		if(a[temp]==upto){
			temp--;
			if(temp==0)
				break;
		}
		else{
			a[temp]++;
			while(temp<numbers){
				temp++;
				a[temp]=1;
			}
			
			printf("(");
			for( temp2 = 1 ; temp2 <= numbers; temp2++){
				printf("%d", a[temp2]);
			}
			printf(")");
		}
	}
	return 0;
}
```

```txt
(111)(112)(113)(114)(121)(122)(123)(124)(131)(132)(133)(134)(141)(142)(143)(144)(211)(212)(213)(214)(221)(222)(223)(224)(231)(232)(233)(234)(241)(242)(243)(244)(311)(312)(313)(314)(321)(322)(323)(324)(331)(332)(333)(334)(341)(342)(343)(344)(411)(412)(413)(414)(421)(422)(423)(424)(431)(432)(433)(434)(441)(442)(443)(444)
```



## C++


```d

#include <stdio.h>
#include <stdlib.h>

struct Generator
{
    public:
        Generator(int s, int v)
            : cSlots(s)
            , cValues(v)
        {
            a = new int[s];

            for (int i = 0; i < cSlots - 1; i++) {
                a[i] = 1;
            }
            a[cSlots - 1] = 0;

            nextInd = cSlots;
        }

        ~Generator()
        {
            delete a;
        }

        bool doNext()
        {
            for (;;)
            {
                if (a[nextInd - 1] == cValues) {
                    nextInd--;
                    if (nextInd == 0)
                        return false;
                }
                else {
                    a[nextInd - 1]++;
                    while (nextInd < cSlots) {
                        nextInd++;
                        a[nextInd - 1] = 1;
                    }

                    return true;
                }
            }
        }

        void doPrint()
        {
            printf("(");
            for (int i = 0; i < cSlots; i++) {
                printf("%d", a[i]);
            }
            printf(")");
        }

    private:
        int *a;
        int cSlots;
        int cValues;
        int nextInd;
};


int main() 
{
    Generator g(3, 4);

    while (g.doNext()) {
        g.doPrint();
    }

    return 0;
}


```

```txt

(111)(112)(113)(114)(121)(122)(123)(124)(131)(132)(133)(134)(141)(142)(143)(144)(211)(212)(213)(214)(221)(222)(223)(224)(231)(232)(233)(234)(241)(242)(243)(244)(311)(312)(313)(314)(321)(322)(323)(324)(331)(332)(333)(334)(341)(342)(343)(344)(411)(412)(413)(414)(421)(422)(423)(424)(431)(432)(433)(434)(441)(442)(443)(444)

```



## D


### opApply Version

```d
import std.array;

struct PermutationsWithRepetitions(T) {
    const T[] data;
    const int n;

    int opApply(int delegate(ref T[]) dg) {
        int result;
        T[] aux;

        if (n == 1) {
            foreach (el; data) {
                aux = [el];
                result = dg(aux);
                if (result) goto END;
            }
        } else {
            foreach (el; data) {
                foreach (p; PermutationsWithRepetitions(data, n - 1)) {
                    aux = el ~ p;
                    result = dg(aux);
                    if (result) goto END;
                }
            }
        }

        END:
        return result;
    }
}

auto permutationsWithRepetitions(T)(T[] data, in int n) pure nothrow
in {
    assert(!data.empty && n > 0);
} body {
    return PermutationsWithRepetitions!T(data, n);
}

void main() {
    import std.stdio, std.array;
    [1, 2, 3].permutationsWithRepetitions(2).array.writeln;
}
```

```txt
[[1, 1], [1, 2], [1, 3], [2, 1], [2, 2], [2, 3], [3, 1], [3, 2], [3, 3]]
```



### Generator Range Version

```d
import std.stdio, std.array, std.concurrency;

Generator!(T[]) permutationsWithRepetitions(T)(T[] data, in uint n)
in {
    assert(!data.empty && n > 0);
} body {
    return new typeof(return)({
        if (n == 1) {
            foreach (el; data)
                yield([el]);
        } else {
            foreach (el; data)
                foreach (perm; permutationsWithRepetitions(data, n - 1))
                    yield(el ~ perm);
        }
    });
}

void main() {
    [1, 2, 3].permutationsWithRepetitions(2).writeln;
}
```

The output is the same.


## EchoLisp


```scheme

(lib 'sequences) ;; (indices   ..) 
(lib 'list) ;; (list-permute ..)

;; (indices range_1 ..range_k) returns a  procrastinator (lazy sequence)
;; which gives all combinations of indices_i in range_i.
;;
;; If  all  k ranges are equal to (0 ...n-1)
;; (indices (make-vector k n))
;; will give the n^k permutations with repetitions of the integers (0 ... n-1).


(define perms (indices (make-vector 2 3)))
(take perms #:all)
    → (#(0 0) #(0 1) #(0 2) #(1 0) #(1 1) #(1 2) #(2 0) #(2 1) #(2 2))
(length perms) → 9
    
;; 6-permute the numbers (0 ....9)
(define perms (indices (make-vector 6 10)))
(length perms) → 1000000

;; passing the procrastinator to a routine
;; which stops when sum = 22
(for ((p perms))
    #:break (= (apply + (vector->list p)) 22) => p )
     →  #( 0 0 0 4 9 9)
     
;; to permute any objects, use (list-permute list permutation-vector/list)
(list-permute '(a b c d e) '(1 0 1 0 3 2 1))
    → (b a b a d c b)
(list-permute '(a b c d e) #(1 0 1 0 3 2 1))
    → (b a b a d c b)

```



## Elixir

```elixir
defmodule RC do
  def perm_rep(list), do: perm_rep(list, length(list))
  
  def perm_rep([], _), do: [[]]
  def perm_rep(_,  0), do: [[]]
  def perm_rep(list, i) do
    for x <- list, y <- perm_rep(list, i-1), do: [x|y]
  end
end

list = [1, 2, 3]
Enum.each(1..3, fn n ->
  IO.inspect RC.perm_rep(list,n)
end)
```


```txt

[[1], [2], [3]]
[[1, 1], [1, 2], [1, 3], [2, 1], [2, 2], [2, 3], [3, 1], [3, 2], [3, 3]]
[[1, 1, 1], [1, 1, 2], [1, 1, 3], [1, 2, 1], [1, 2, 2], [1, 2, 3], [1, 3, 1],
 [1, 3, 2], [1, 3, 3], [2, 1, 1], [2, 1, 2], [2, 1, 3], [2, 2, 1], [2, 2, 2],
 [2, 2, 3], [2, 3, 1], [2, 3, 2], [2, 3, 3], [3, 1, 1], [3, 1, 2], [3, 1, 3],
 [3, 2, 1], [3, 2, 2], [3, 2, 3], [3, 3, 1], [3, 3, 2], [3, 3, 3]]

```



## Erlang


```Erlang
-module(permute).
-export([permute/1]).

permute(L) -> permute(L,length(L)).
permute([],_) -> [[]];
permute(_,0) -> [[]];
permute(L,I) -> [[X|Y] || X<-L, Y<-permute(L,I-1)].
```



## Go


```go
package main

import "fmt"

var (
    n      = 3
    values = []string{"A", "B", "C", "D"}
    k      = len(values)
    decide = func(p []string) bool {
        return p[0] == "B" && p[1] == "C"
    }
)

func main() {
    pn := make([]int, n)
    p := make([]string, n)
    for {
        // generate permutaton
        for i, x := range pn {
            p[i] = values[x]
        }
        // show progress
        fmt.Println(p)
        // pass to deciding function
        if decide(p) {
            return // terminate early
        }
        // increment permutation number
        for i := 0; ; {
            pn[i]++
            if pn[i] < k {
                break
            }
            pn[i] = 0
            i++
            if i == n {
                return // all permutations generated
            }
        }
    }
}
```

```txt

[A A A]
[B A A]
[C A A]
[D A A]
[A B A]
[B B A]
[C B A]
[D B A]
[A C A]
[B C A]

```



## Haskell


```haskell
import Control.Monad (replicateM)

main = mapM_ print (replicateM 2 [1,2,3])
```

```txt

[1,1]
[1,2]
[1,3]
[2,1]
[2,2]
[2,3]
[3,1]
[3,2]
[3,3]

```



## J


Position in the sequence is an integer from <code>i.n^k</code>, for example:


```j
   i.3^2
0 1 2 3 4 5 6 7 8
```


The sequence itself is expressed using <code>(k#n)#: position</code>, for example:


```j
   (2#3)#:i.3^2
0 0
0 1
0 2
1 0
1 1
1 2
2 0
2 1
2 2
```


Partial sequences belong in a context where they are relevant and the sheer number of such possibilities make it inadvisable to generalize outside of those contexts.  But anything that can generate integers will do.  For example:


```j
   (2#3)#:3 4 5
1 0
1 1
1 2
```


We might express this as a verb


```j
perm=: # #: i.@^~
```


with example use:


```j
   2 perm 3
0 0
0 1
0 2
1 0
...
```


but the structural requirements of this task (passing intermediate results "when needed") mean that we are not looking for a word that does it all, but are instead looking for components that we can assemble in other contexts. This means that the language primitives are what's needed here.


## Java

```java
import java.util.function.Predicate;

public class PermutationsWithRepetitions {

    public static void main(String[] args) {
        char[] chars = {'a', 'b', 'c', 'd'};
        // looking for bba
        permute(chars, 3, i -> i[0] == 1 && i[1] == 1 && i[2] == 0);
    }

    static void permute(char[] a, int k, Predicate<int[]> decider) {
        int n = a.length;
        if (k < 1 || k > n)
            throw new IllegalArgumentException("Illegal number of positions.");

        int[] indexes = new int[n];
        int total = (int) Math.pow(n, k);

        while (total-- > 0) {
            for (int i = 0; i < n - (n - k); i++)
                System.out.print(a[indexes[i]]);
            System.out.println();

            if (decider.test(indexes))
                break;

            for (int i = 0; i < n; i++) {
                if (indexes[i] >= n - 1) {
                    indexes[i] = 0;
                } else {
                    indexes[i]++;
                    break;
                }
            }
        }
    }
}
```


Output:


```txt
aaa
baa
caa
daa
aba
bba
```



## JavaScript


### ES5


Permutations with repetitions, using strict evaluation, generating the entire set (where system constraints permit) with some degree of efficiency. For lazy or interruptible evaluation, see the second example below.


```JavaScript
(function () {
    'use strict';

    // permutationsWithRepetition :: Int -> [a] -> [[a]]
    var permutationsWithRepetition = function (n, as) {
        return as.length > 0 ? (
            foldl1(curry(cartesianProduct)(as), replicate(n, as))
        ) : [];
    };


    // GENERIC FUNCTIONS -----------------------------------------------------

    // cartesianProduct :: [a] -> [b] -> [[a, b]]
    var cartesianProduct = function (xs, ys) {
        return [].concat.apply([], xs.map(function (x) {
            return [].concat.apply([], ys.map(function (y) {
                return [
                    [x].concat(y)
                ];
            }));
        }));
    };

    // foldl1 :: (a -> a -> a) -> [a] -> a
    var foldl1 = function (f, xs) {
        return xs.length > 0 ? xs.slice(1)
            .reduce(f, xs[0]) : [];
    };

    // replicate :: Int -> a -> [a]
    var replicate = function (n, a) {
        var v = [a],
            o = [];
        if (n < 1) return o;
        while (n > 1) {
            if (n & 1) o = o.concat(v);
            n >>= 1;
            v = v.concat(v);
        }
        return o.concat(v);
    };

    // curry :: ((a, b) -> c) -> a -> b -> c
    var curry = function (f) {
        return function (a) {
            return function (b) {
                return f(a, b);
            };
        };
    };

    // TEST -----------------------------------------------------------------
    // show :: a -> String
    var show = function (x) {
        return JSON.stringify(x);
    }; //, null, 2);

    return show(permutationsWithRepetition(2, [1, 2, 3]));

    //--> [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
})();
```


```JavaScript
[[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
```


Permutations with repetition by treating the <math>n^k</math> elements as an ordered set, and writing a function from a zero-based index to the nth permutation. This allows us terminate a repeated generation on some condition, or explore a sub-set without needing to generate the whole set:


```JavaScript
(function () {
    'use strict';

    // nthPermutationWithRepn :: [a] -> Int -> Int -> [a]
    var nthPermutationWithRepn = function (xs, groupSize, index) {
        var intBase = xs.length,
            intSetSize = Math.pow(intBase, groupSize),
            lastIndex = intSetSize - 1; // zero-based

        if (intBase < 1 || index > lastIndex) return undefined;

        var baseElements = unfoldr(function (m) {
                var v = m.new,
                    d = Math.floor(v / intBase);
                return {
                    valid: d > 0,
                    value: xs[v % intBase],
                    new: d
                };
            }, index),
            intZeros = groupSize - baseElements.length;

        return intZeros > 0 ? replicate(intZeros, xs[0])
            .concat(baseElements) : baseElements;
    };

    // GENERIC FUNCTIONS

    // unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    var unfoldr = function (mf, v) {
        var xs = [];
        return [until(function (m) {
                return !m.valid;
            }, function (m) {
                var m2 = mf(m);
                return m2.valid && (xs = [m2.value].concat(xs)), m2;
            }, {
                valid: true,
                value: v,
                new: v
            })
            .value
        ].concat(xs);
    };

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    var until = function (p, f, x) {
        var v = x;
        while (!p(v)) {
            v = f(v);
        }
        return v;
    };

    // replicate :: Int -> a -> [a]
    var replicate = function (n, a) {
        var v = [a],
            o = [];
        if (n < 1) return o;
        while (n > 1) {
            if (n & 1) o = o.concat(v);
            n >>= 1;
            v = v.concat(v);
        }
        return o.concat(v);
    };

    // show :: a -> String
    var show = function (x) {
        return JSON.stringify(x);
    }; //, null, 2);

    // curry :: Function -> Function
    var curry = function (f) {
        for (var lng = arguments.length,
                args = Array(lng > 1 ? lng - 1 : 0),
                iArg = 1; iArg < lng; iArg++) {
            args[iArg - 1] = arguments[iArg];
        }

        var intArgs = f.length,
            go = function (xs) {
                return xs.length >= intArgs ? f.apply(null, xs) : function () {
                    return go(xs.concat([].slice.apply(arguments)));
                };
            };
        return go([].slice.call(args, 1));
    };

    // range :: Int -> Int -> [Int]
    var range = function (m, n) {
        return Array.from({
            length: Math.floor(n - m) + 1
        }, function (_, i) {
            return m + i;
        });
    };

    // TEST
    // Just items 30 to 35 in the (zero-indexed) series:
    return show(range(30, 35)
        .map(curry(nthPermutationWithRepn)(['X', 'Y', 'Z'], 4)));
})();
```


```txt
["Y","X","Y","X"], ["Y","X","Y","Y"], ["Y","X","Y","Z"], ["Y","X","Z","X"], ["Y","X","Z","Y"], ["Y","X","Z","Z"]
```



### ES6


### =Strict evaluation of the whole set=

Permutations with repetitions, using strict evaluation, generating the entire set. 
For partial or interruptible evaluation, see the second example below.

A (strict) analogue of the (lazy) replicateM in Haskell.


```JavaScript
(() => {
    'use strict';

    // GENERIC FUNCTIONS

    // replicateM n act performs the action n times, gathering the results.
    // replicateM :: (Applicative m) => Int -> m a -> m [a]
    const replicateM = (n, f) => {
        const loop = x => x <= 0 ? [
            []
        ] : liftA2(cons, f, loop(x - 1));
        return loop(n);
    };

    // Lift a binary function to actions.
    // liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
    const liftA2 = (f, a, b) =>
        listApply(a.map(curry(f)), b);

    // <*>
    // listApply :: [(a -> b)] -> [a] -> [b]
    const listApply = (fs, xs) =>
        [].concat.apply([], fs.map(f =>
        [].concat.apply([], xs.map(x => [f(x)]))));

    // curry :: ((a, b) -> c) -> a -> b -> c
    const curry = f => a => b => f(a, b);

    // cons :: a -> [a] -> [a]
    const cons = (x, xs) => [x].concat(xs);

    // show :: a -> String;
    const show = JSON.stringify;

    // TEST
    return show(
        replicateM(2, [1, 2, 3])
    );
    // -> [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
})();
```

```JavaScript
[[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
```




### =Lazy evaluation with a generator =

Permutations with repetition by treating the <math>n^k</math> elements as an ordered set, and writing a function from a zero-based index to the nth permutation. Wrapping this function in a generator allows us terminate a repeated generation on some condition, or explore a sub-set without needing to generate the whole set:


```JavaScript
(() => {
    'use strict';

    const main = () => {

        // Generator object
        const gen = permsWithRepn('ACKR', 5);

        // Search without needing to generate whole set:
        let
            nxt = gen.next(),
            i = 0,
            alpha = nxt.value,
            psi = alpha;
        while (!nxt.done && 'crack' !== toLower(concat(nxt.value))) {
            psi = nxt.value;
            console.log(psi)
            nxt = gen.next()
            i++
        }
        console.log(nxt.value)
        return (
            'Generated ' + i + ' of ' + Math.pow(4, 5) +
            ' possible permutations,\n' +
            'searching from: ' + show(alpha) + ' thru: ' + show(psi) +
            '\nbefore finding: ' + show(nxt.value)
        );
    };

    // PERMUTATION GENERATOR ------------------------------

    // permsWithRepn :: [a] -> Int -> Generator [a]
    function* permsWithRepn(xs, intGroup) {
        const
            vs = Array.from(xs),
            intBase = vs.length,
            intSet = Math.pow(intBase, intGroup);
        if (0 < intBase) {
            let index = 0;
            while (index < intSet) {
                const
                    ds = unfoldr(
                        v => 0 < v ? (() => {
                            const rd = quotRem(v, intBase);
                            return Just(Tuple(vs[rd[1]], rd[0]))
                        })() : Nothing(),
                        index++
                    );
                yield replicate(
                    intGroup - ds.length,
                    vs[0]
                ).concat(ds);
            };
        }
    };

    // GENERIC FUNCTIONS ----------------------------------

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

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // index (!!) :: [a] -> Int -> a
    // index (!!) :: String -> Int -> Char
    const index = (xs, i) => xs[i];

    // quotRem :: Int -> Int -> (Int, Int)
    const quotRem = (m, n) =>
        Tuple(Math.floor(m / n), m % n);

        // replicate :: Int -> a -> [a]
    const replicate = (n, x) =>
        Array.from({
            length: n
        }, () => x);

    // show :: a -> String
    const show = x => JSON.stringify(x);

    // toLower :: String -> String
    const toLower = s => s.toLocaleLowerCase();

    // unfoldr(x => 0 !== x ? Just([x, x - 1]) : Nothing(), 10);
    // --> [10,9,8,7,6,5,4,3,2,1]

    // unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    const unfoldr = (f, v) => {
        let
            xr = [v, v],
            xs = [];
        while (true) {
            const mb = f(xr[1]);
            if (mb.Nothing) {
                return xs
            } else {
                xr = mb.Just;
                xs.push(xr[0])
            }
        }
    };

    // MAIN ---
    return main();
})();
```

```txt
Generated 589 of 1024 possible permutations,
searching from: ["A","A","A","A","A"] thru: ["A","R","A","C","K"]
before finding: ["C","R","A","C","K"]
```



## jq

We first present a definition of permutations_with_replacement(n) that is compatible with jq 1.4.
To interrupt the stream that it produces, however, requires a version of jq with break, which was introduced after the release of jq 1.4.

'''Definitions'''

We shall define permutations_with_replacements(n) in terms of a more general filter, combinations/0, defined as follows:


```jq
# Input: an array, $in, of 0 or more arrays
# Output: a stream of arrays, c, with c[i] drawn from $in[i].
def combinations:
  if length == 0 then []
  else
  .[0][] as $x
  | (.[1:] | combinations) as $y
  | [$x] +  $y
  end ;

# Input: an array of the k values from which to choose.
# Output: a stream of arrays of length n with elements drawn from the input array.
def permutations_with_replacements(n):
  . as $in | [range(0; n) | $in] | combinations;
```

  
'''Example 1: Enumeration''':

Count the number of 4-combinations of [0,1,2] by enumerating them, i.e., without creating a data structure to store them all.

```jq
def count(stream): reduce stream as $i (0; .+1);

count([0,1,2] | permutations_with_replacements(4))
# output: 81
```



'''Example 2: Early termination of the generator''':

Counting from 1, and terminating the generator when the item is found, what is the sequence number of ["c", "a", "b"] in the stream
of 3-combinations of ["a","b","c"]?

```jq
# Input: the item to be matched
# Output: the index of the item in the stream (counting from 1);
# emit null if the item is not found
def sequence_number(stream):
  . as $in
  | (label $top
     | foreach stream as $i (0; .+1; if $in == $i then ., break $top else empty end))
    // null;  # NOTE: "//" here is an operator

["c", "a", "b"] | sequence_number( ["a","b","c"] | permutations_with_replacements(3))

# output: 20
```



## Julia

Implements a simil-Combinatorics.jl API.


```julia
struct WithRepetitionsPermutations{T}
    a::T
    t::Int
end

with_repetitions_permutations(elements::T, len::Integer) where T =
    WithRepetitionsPermutations{T}(unique(elements), len)

Base.iteratorsize(::WithRepetitionsPermutations) = Base.HasLength()
Base.length(p::WithRepetitionsPermutations) = length(p.a) ^ p.t
Base.iteratoreltype(::WithRepetitionsPermutations) = Base.HasEltype()
Base.eltype(::WithRepetitionsPermutations{T}) where T = T
Base.start(p::WithRepetitionsPermutations) = ones(Int, p.t)
Base.done(p::WithRepetitionsPermutations, s::Vector{Int}) = s[end] > endof(p.a)
function Base.next(p::WithRepetitionsPermutations, s::Vector{Int})
    cur = p.a[s]
    s[1] += 1
    local i = 1
    while i < endof(s) && s[i] > length(p.a)
        s[i] = 1
        s[i+1] += 1
        i += 1
    end
    return cur, s
end

println("Permutations of [4, 5, 6] in 3:")
foreach(println, collect(with_repetitions_permutations([4, 5, 6], 3)))
```


```txt
Permutations of [4, 5, 6] in 3:
[4, 4, 4]
[5, 4, 4]
[6, 4, 4]
[4, 5, 4]
[5, 5, 4]
[6, 5, 4]
[4, 6, 4]
[5, 6, 4]
[6, 6, 4]
[4, 4, 5]
[5, 4, 5]
[6, 4, 5]
[4, 5, 5]
[5, 5, 5]
[6, 5, 5]
[4, 6, 5]
[5, 6, 5]
[6, 6, 5]
[4, 4, 6]
[5, 4, 6]
[6, 4, 6]
[4, 5, 6]
[5, 5, 6]
[6, 5, 6]
[4, 6, 6]
[5, 6, 6]
[6, 6, 6]
```


## K

enlist each from x on the left and each from x on the right where x is range 10

```k
 
,/x/:\:x:!10

```



## Kotlin

```scala
// version 1.1.2

fun main(args: Array<String>) {
    val n  = 3
    val values = charArrayOf('A', 'B', 'C', 'D')
    val k = values.size
    // terminate when first two characters of the permutation are 'B' and 'C' respectively
    val decide = fun(pc: CharArray) = pc[0] == 'B' && pc[1] == 'C'
    val pn = IntArray(n)
    val pc = CharArray(n)
    while (true) {
        // generate permutation
        for ((i, x) in pn.withIndex()) pc[i] = values[x]
        // show progress
        println(pc.contentToString())
        // pass to deciding function
        if (decide(pc)) return  // terminate early
        // increment permutation number
        var i = 0
        while (true) {
            pn[i]++
            if (pn[i] < k) break
            pn[i++] = 0
            if (i == n) return  // all permutations generated
        }
    }
}
```


```txt

[A, A, A]
[B, A, A]
[C, A, A]
[D, A, A]
[A, B, A]
[B, B, A]
[C, B, A]
[D, B, A]
[A, C, A]
[B, C, A]

```



## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      a=("A","B","C","D")
      n=len(a)
      c1=lambda a, n, c (&f) ->{
            =(array(a, c),)
            c++
            if c=n then c=0: f=true
      }
      m=n-2
      While m >0 {
            c3=lambda c2=c1, a, n, c (&f) -> {
                  f=false
                  =Cons((array(a, c),), c2(&f))
                  if f then {
                         c++
                         f=false
                        if c=n then c=0: f=true    
                  }
            }
            c1=c3
            m--
      }
      k=false
      While not k {
           r=c3(&k)
           rr=each(r end to start)
           While rr {
                  Print array$(rr),
            }
            Print
            if array$(r, 2)="B" and array$(r,1)="C" then exit
      }
}
Checkit

```

<pre style="height:30ex;overflow:scroll">
A   A   A
B   A   A
C   A   A
D   A   A
A   B   A
B   B   A
C   B   A
D   B   A
A   C   A
B   C   A
</pre >


## Mathematica


```mathematica
Tuples[{1, 2, 3}, 2]
```

```txt
```



## Maxima


```maxima
apply(cartesian_product,makelist({1,2,3}, 2));
```

```txt
{[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]}
```



## Perl


```perl
use Algorithm::Combinatorics qw/tuples_with_repetition/;
print join(" ", map { "[@$_]" } tuples_with_repetition([qw/A B C/],2)), "\n";
```

```txt
[A A] [A B] [A C] [B A] [B B] [B C] [C A] [C B] [C C]
```


Solving the crack problem:

```perl
use Algorithm::Combinatorics qw/tuples_with_repetition/;
my $iter = tuples_with_repetition([qw/A C K R/], 5);
my $tries = 0;
while (my $p = $iter->next) {
  $tries++;
  die "Found the combination after $tries tries!\n" if join("",@$p) eq "CRACK";
}
```

```txt
Found the combination after 455 tries!
```



## Perl 6


We can use the <tt>X</tt> operator ("cartesian product") to cross the list with itself.

For <math>n=2</math>:

```perl6>my @k = <a b c
;

.say for @k X @k;
```


For arbitrary <math>n</math>:

```perl6>my @k = <a b c
;
my $n = 2;

.say for [X] @k xx $n;
```


```txt
a a
a b
a c
b a
b b
b c
c a
c b
c c
```


Here is an other approach, counting all <math>k^n</math> possibilities in base <math>k</math>:

```perl6>my @k = <a b c
;
my $n = 2;

say @k[.polymod: +@k xx $n-1] for ^@k**$n
```


```txt
a a
b a
c a
a b
b b
c b
a c
b c
c c
```


## Pascal

Create a list of indices into what ever you want, one by one.
Doing it by addig one to a number with k-positions to base n.

```pascal
program PermuWithRep;
//permutations with repetitions
//http://rosettacode.org/wiki/Permutations_with_repetitions
{$IFDEF FPC}
  {$Mode Delphi}{$Optimization ON}{$Align 16}{$Codealign proc=16,loop=4}
{$ELSE}
  {$APPTYPE CONSOLE}// for Delphi
{$ENDIF}
uses
  sysutils;
type
  tPermData =  record
               mdTup_n,           //number of positions
               mdTup_k:NativeInt; //number of different elements
               mdTup :array of integer;
             end;

function InitTuple(k,n:nativeInt):tPermData;
begin
  with result do
  Begin
    IF k> 0 then
    Begin
      mdTup_k:= k;
      setlength(mdTup,k);
      IF (n<0) then
        mdTup_n := 0
      else
        mdTup_n := n;
    end
    else
    Begin
      mdTup_k := 1;
      mdTup_n := k;
    end;
  end;
end;

procedure PermOut(const p:tPermData);
var
  i : nativeInt;
Begin
  with p do
  Begin
    For i := 0 to mdTup_k-1 do
      write(mdTup[i]:4);
  end;
  writeln;
end;

function NextPermWithRep(var perm:tPermData): boolean;
// create next permutation by adding 1 and correct "carry"
// returns false if finished
var
  pDg :^Integer;
  dg,le :nativeInt;
begin
  WIth perm do
  Begin
    pDg := @mdTup[0];
    le := mdTup_k;
    repeat
      dg := pDg^+1;
      IF (dg<mdTup_n) then
      Begin
        pDg^ := dg;
        BREAK;
      end
      else
        pDg^  := 0;
     dec(le);
     inc(pDg);
    until  le<=0;
    result := (dg<mdTup_n);
  end;
end;

var
  p: tPermData;
  cnt,k,n: nativeInt;
Begin
  cnt := 0;
  //k := 2;n := 3;
  k := 10;n := 8;
  p:= InitTuple(k,n);
  IF (n<= 6) then
    repeat
      inc(cnt);
      PermOut(p);
    until Not(NextPermWithRep(p))
  else
    repeat
      inc(cnt);
    until Not(NextPermWithRep(p));
  writeln('k: ',k,' n: ',n,'  count ',cnt);
end.
```

```txt

   0   0
   1   0
   2   0
   0   1
   1   1
   2   1
   0   2
   1   2
   2   2
k: 2 n: 3  count 9
..
//speedtest Compiler /fpc/3.1.1/ppc386 "%f" -al -Xs -XX -O3
// i4330 3.5 Ghz
k: 10 n: 8  count 1073741824 => 8^10

real  0m2.556s // without inc(cnt); real  0m2.288s-> 7,5 cycles per call
//"old" compiler-version
//real  0m3.465s  /fpc/2.6.4/ppc386 "%f" -al -Xs -XX -O3
```



## Phix

The task is equivalent to simply counting in base=length(set), from 1 to power(base,n).

Asking for the 0th permutation just returns the total number of permutations (ie "").

Results can be generated in any order, hence early termination is quite simply a non-issue.

```Phix
function permrep(sequence set, integer n, idx=0)
    integer base = length(set),
            nperm = power(base,n)
    if idx=0 then
        -- return the number of permutations
        return nperm
    end if
    -- return the idx'th [1-based] permutation
    if idx<1 or idx>nperm then ?9/0 end if
    idx -= 1    -- make it 0-based
    sequence res = ""
    for i=1 to n do
        res = prepend(res,set[mod(idx,base)+1])
        idx = floor(idx/base)   
    end for
    if idx!=0 then ?9/0 end if -- sanity check
    return res
end function
```

Some slightly excessive testing:

```Phix
procedure show_all(sequence set, integer n)
    integer l = permrep(set,n)
    sequence s = repeat(0,l)
    for i=1 to l do
        s[i] = permrep(set,n,i)
    end for
    ?s
end procedure

show_all("123",1)
show_all("123",2)
show_all("123",3)
show_all("456",3)
show_all({1,2,3},3)
show_all({"bat","fox","cow"},2)

sequence s = {}
for i=31 to 36 do
    s = append(s,permrep("XYZ",4,i))
end for
?s

integer l = permrep("ACKR",5)
for i=1 to l do
    if permrep("ACKR",5,i)="CRACK" then -- 455
        printf(1,"Permutation %d of %d: CRACK\n",{i,l})
        exit
    end if
end for
--The 590th (one-based) permrep is KCARC, ie reverse(CRACK), matching the 589 result of 0-based idx solutions
printf(1,"reverse(permrep(\"ACKR\",5,589+1):%s\n",{reverse(permrep("ACKR",5,590))})
```

```txt

{"1","2","3"}
{"11","12","13","21","22","23","31","32","33"}
{"111","112","113","121","122","123","131","132","133","211","212","213","221","222","223","231","232","233","311","312","313","321","322","323","331","332","333"}
{"444","445","446","454","455","456","464","465","466","544","545","546","554","555","556","564","565","566","644","645","646","654","655","656","664","665","666"}
{"YXYX","YXYY","YXYZ","YXZX","YXZY","YXZZ"}
Permutation 455 of 1024: CRACK
reverse(permrep("ACKR",5,589+1):CRACK

```



## PHP


```PHP
<?php
function permutate($values, $size, $offset) {
    $count = count($values);
    $array = array();
    for ($i = 0; $i < $size; $i++) {
        $selector = ($offset / pow($count,$i)) % $count;
        $array[$i] = $values[$selector];
    }
    return $array;
}

function permutations($values, $size) {
    $a = array();
    $c = pow(count($values), $size);
    for ($i = 0; $i<$c; $i++) {
        $a[$i] = permutate($values, $size, $i);        
    }
    return $a;
}

$permutations = permutations(['bat','fox','cow'], 2);
foreach ($permutations as $permutation) {
    echo join(',', $permutation)."\n";
}

```


```txt
bat,bat
fox,bat
cow,bat
bat,fox
fox,fox
cow,fox
bat,cow
fox,cow
cow,cow

```



## PicoLisp


```PicoLisp
(de permrep (N Lst)
   (if (=0 N)
      (cons NIL)
      (mapcan
         '((X)
            (mapcar '((Y) (cons Y X)) Lst) )
         (permrep (dec N) Lst) ) ) )
```



## Python



### Strict evaluation of the whole set


To evaluate the whole set of permutations, without the option to make complete evaluation conditional, we can reach for a generic replicateM function for lists:
```python
'''Permutations of n elements drawn from k values'''

from itertools import product


# replicateM :: Applicative m => Int -> m a -> m [a]
def replicateM(n):
    '''A functor collecting values accumulated by
       n repetitions of m. (List instance only here).
    '''
    def rep(m):
        def go(x):
            return [[]] if 1 > x else (
                liftA2List(lambda a, b: [a] + b)(m)(go(x - 1))
            )
        return go(n)
    return lambda m: rep(m)


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Permutations of two elements, drawn from three values'''
    print(
        fTable(main.__doc__ + ':\n')(repr)(showList)(

            replicateM(2)

        )([[1, 2, 3], 'abc'])
    )


# GENERIC FUNCTIONS ---------------------------------------

# liftA2List :: (a -> b -> c) -> [a] -> [b] -> [c]
def liftA2List(f):
    '''The binary operator f lifted to a function over two
       lists. f applied to each pair of arguments in the
       cartesian product of xs and ys.
    '''
    return lambda xs: lambda ys: [
        f(*xy) for xy in product(xs, ys)
    ]


# DISPLAY -------------------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# showList :: [a] -> String
def showList(xs):
    '''Stringification of a list.'''
    return '[' + ','.join(
        showList(x) if isinstance(x, list) else repr(x) for x in xs
    ) + ']'


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Permutations of two elements, drawn from three values:

[1, 2, 3] -> [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
    'abc' -> [['a','a'],['a','b'],['a','c'],['b','a'],['b','b'],['b','c'],['c','a'],['c','b'],['c','c']]
```



### Lazy evaluation with a generator


### =Applying itertools.product=



```python
from itertools import product

# check permutations until we find the word 'crack'
for x in product('ACRK', repeat=5):
    w = ''.join(x)
    print w
    if w.lower() == 'crack': break
```



### =Writing a generator=


Or, composing our own generator, by wrapping a function '''from''' an index in the range ''0 .. ((distinct items to the power of groupSize) - 1)''  '''to''' a unique permutation. (Each permutation is equivalent to a 'number' in the base of the size of the set of distinct items, in which each distinct item functions as a 'digit'):
```Python
'''Generator-based permutations with repetition'''

from itertools import (chain, repeat)


# permsWithRepns :: [a] -> Int -> Generator [[a]]
def permsWithRepns(xs):
    '''Generator of permutations of length n, with
       elements drawn from the values in xs.
    '''
    def groupsOfSize(n):
        f = nthPermWithRepn(xs)(n)
        limit = len(xs)**n
        i = 0
        while i < limit:
            yield f(i)
            i = 1 + i
    return lambda n: groupsOfSize(n)


# Index as a 'number' in the base of the
# size of the set (of distinct values to be permuted),
# using each value as a 'digit'
# (leftmost value used as the 'zero')

# nthPermWithRepn :: [a] -> Int -> Int -> [a]
def nthPermWithRepn(xs):
    '''Indexed permutation of n values drawn from xs'''
    def go(intGroup, index):
        vs = list(xs)
        intBase = len(vs)
        intSet = intBase ** intGroup
        return (
            lambda ds=unfoldr(
                lambda v: (
                    lambda qr=divmod(v, intBase): Just(
                        (qr[0], vs[qr[1]])
                    )
                )() if 0 < v else Nothing()
            )(index): (
                list(repeat(vs[0], intGroup - len(ds))) + ds
            )
        )() if 0 < intBase and index < intSet else None
    return lambda intGroup: lambda index: go(
        intGroup, index
    )


# MAIN ----------------------------------------------------
# main :: IO ()
def main():
    '''Search for a 5 char permutation drawn from 'ACKR' matching "crack"'''

    cs = 'ACKR'
    wordLength = 5
    target = 'crack'

    gen = permsWithRepns(cs)(wordLength)
    mb = Nothing()
    for idx, xs in enumerate(gen):
        s = ''.join(xs)
        if target == s.lower():
            mb = Just((s, idx))
            break

    print(main.__doc__ + ':\n')
    print(
        maybe('No match found for "{k}"'.format(k=target))(
            lambda m: 'Permutation {idx} of {total}: {pm}'.format(
                idx=m[1], total=len(cs)**wordLength, pm=s
            )
        )(mb)
    )


# GENERIC FUNCTIONS -------------------------------------

# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe(option type) value.'''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe(option type) value.'''
    return {'type': 'Maybe', 'Nothing': True}


# concat :: [[a]] -> [a]
# concat :: [String] -> String
def concat(xs):
    '''The concatenation of all the elements
       in a list or iterable.'''

    def f(ys):
        zs = list(chain(*ys))
        return ''.join(zs) if isinstance(ys[0], str) else zs

    return (
        f(xs) if isinstance(xs, list) else (
            chain.from_iterable(xs)
        )
    ) if xs else []


# fst :: (a, b) -> a
def fst(tpl):
    '''First member of a pair.'''
    return tpl[0]


# maybe :: b -> (a -> b) -> Maybe a -> b
def maybe(v):
    '''Either the default value v, if m is Nothing,
       or the application of f to x,
       where m is Just(x).
    '''
    return lambda f: lambda m: v if None is m or m.get('Nothing') else (
        f(m.get('Just'))
    )


# snd :: (a, b) -> b
def snd(tpl):
    '''Second member of a pair.'''
    return tpl[1]


# unfoldr(lambda x: Just((x, x - 1)) if 0 != x else Nothing())(10)
# -> [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
# unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
def unfoldr(f):
    '''Dual to reduce or foldr.
       Where catamorphism reduces a list to a summary value,
       the anamorphic unfoldr builds a list from a seed value.
       As long as f returns Just(a, b), a is prepended to the list,
       and the residual b is used as the argument for the next
       application of f.
       When f returns Nothing, the completed list is returned.
    '''
    def go(v):
        xr = v, v
        xs = []
        while True:
            mb = f(xr[0])
            if mb.get('Nothing'):
                return xs
            else:
                xr = mb.get('Just')
                xs.append(xr[1])
        return xs
    return lambda x: go(x)


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Search for a 5 char permutation drawn from 'ACKR' matching "crack":

Permutation 589 of 1024: CRACK
```



## Racket


### As a sequence

First we define a procedure that defines the sequence of the permutations.

```Racket
#lang racket
(define (permutations-with-repetitions/proc size items)
  (define items-vector (list->vector items))
  (define num (length items))
  (define (pos->element pos)
    (reverse
     (for/list ([p (in-vector pos)])
      (vector-ref items-vector p))))
  (define (next-pos pos) 
    (let ([ret (make-vector size #f)])
      (for/fold ([carry 1]) ((i (in-range size)))
        (let ([tmp (+ (vector-ref pos i) carry)])
          (if (= tmp num)
            (begin 
              (vector-set! ret i 0)
              #;carry 1)
            (begin 
              (vector-set! ret i tmp)
              #;carry 0))))
      ret))
  (define initial-pos (vector->immutable-vector (make-vector size 0)))
  (define last-pos (vector->immutable-vector (make-vector size (sub1 num))))
  (define (continue-after-pos+val? pos val)
    (not (equal? pos last-pos)))
  
  (make-do-sequence (lambda () 
                      (values pos->element
                              next-pos
                              initial-pos
                              #f
                              #f
                              continue-after-pos+val?))))
                              
(sequence->list (permutations-with-repetitions/proc 2 '(1 2 3)))
```

```txt
'((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))
```



### As a sequence with for clause support

Now we define a more general version that can be used efficiently in as a for clause. In other uses it falls back to the sequence implementation.

```Racket
(require (for-syntax racket))
 
(define-sequence-syntax in-permutations-with-repetitions 
  (lambda () #'permutations-with-repetitions/proc) 
  (lambda (stx) 
    (syntax-case stx () 
      [[(element) (_  size/ex items/ex)] 
       #'[(element) 
          (:do-in ([(size) size/ex]
                   [(items) items/ex]
                   [(items-vector) (list->vector items/ex)]
                   [(num) (length items/ex)]
                   [(last-pos) (make-vector size/ex (sub1 (length items/ex)))]) 
                  (void)
                  ([pos (make-vector size 0)]) 
                  #t
                  ([(element) (reverse
                               (for/list ([p (in-vector pos)])
                                (vector-ref items-vector p)))]) 
                  #t
                  (not (equal? pos last-pos)) 
                  [(let ([ret (make-vector size #f)])
                     (for/fold ([carry 1]) ((i (in-range size)))
                       (let ([tmp (+ (vector-ref pos i) carry)])
                         (if (= tmp num)
                           (begin 
                             (vector-set! ret i 0)
                             #;carry 1)
                           (begin 
                             (vector-set! ret i tmp)
                             #;carry 0))))
                     ret)])]])))


(for/list ([element (in-permutations-with-repetitions 2 '(1 2 3))])
  element)
(sequence->list (in-permutations-with-repetitions 2 '(1 2 3)))
```

```txt
'((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))
'((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))
```



## REXX


### version 1


```rexx
/*REXX pgm generates/displays all permutations of N different objects taken M at a time.*/
parse arg things bunch inbetweenChars names
                  /* ╔════════════════════════════════════════════════════════════════╗ */
                  /* ║  inBetweenChars  (optional)   defaults to a  [null].           ║ */
                  /* ║           names  (optional)   defaults to digits (and letters).║ */
                  /* ╚════════════════════════════════════════════════════════════════╝ */
call permSets things, bunch, inBetweenChars, names
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
p:        return word( arg(1), 1)                /*P  function (Pick first arg of many).*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
permSets: procedure; parse arg x,y,between,uSyms /*X    things taken    Y    at a time. */
          @.=;   sep=                            /*X  can't be  >  length(@0abcs).      */
          @abc  = 'abcdefghijklmnopqrstuvwxyz';     @abcU=  @abc;        upper @abcU
          @abcS = @abcU || @abc;                    @0abcS= 123456789 || @abcS

            do k=1  for x                        /*build a list of permutation symbols. */
            _= p( word(uSyms, k)  p( substr(@0abcS, k, 1) k) )  /*get/generate a symbol.*/
            if length(_)\==1  then sep= '_'      /*if not 1st character,  then use sep. */
            $.k= _                               /*append the character to symbol list. */
            end   /*k*/

          if between==''  then between= sep      /*use the appropriate separator chars. */
          call .permSet 1                        /*start with the  first  permutation.  */
          return                                 /* [↓]  this is a recursive subroutine.*/
.permSet: procedure expose $. @. between x y;     parse arg ?
          if ?>y then do; _=@.1;   do j=2  for y-1;  _=_ || between || @.j;   end;   say _
                      end
                 else do q=1  for x              /*build the  permutation  recursively. */
                      @.?= $.q;             call .permSet ?+1
                      end   /*q*/
          return                                 /*this is meant to be an anonymous sub.*/
```

```txt

11
12
13
21
22
23
31
32
33

```

```txt

bat,bat
bat,fox
bat,cow
fox,bat
fox,fox
fox,cow
cow,bat
cow,fox
cow,cow

```


===version 2 (using Interpret)===
Note: this REXX version will cause Regina REXX to fail (crash) if the expression to be INTERPRETed is too large (byte-wise).

PC/REXX and Personal REXX also fail, but for a smaller expression.

Please specify limitations. One could add:
If length(a)>implementation_dependent_limit Then 

  Say 'too large for this Rexx version'

Also note that the output isn't the same as REXX version 1 when the 1st argument is two digits or more, i.e.:   '''11   2''' 

```rexx
/* REXX ***************************************************************
* Arguments and output as in REXX version 1 (for the samples shown there)
* For other elements (such as 11 2), please specify a separator 
* Translating 10, 11, etc. to A, B etc. is left to the reader
* 12.05.2013 Walter Pachl
* 12-05-2013 Walter Pachl take care of bunch<=0 and other oddities
**********************************************************************/
Parse Arg things bunch sep names
If datatype(things,'W') & datatype(bunch,'W') Then 
  Nop
Else 
  Call exit 'First two arguments must be integers >0'
If things='' Then n=3; Else n=things
If bunch=''  Then m=2; Else m=bunch
If things<=0 Then Call exit 'specify a positive number of things'
If bunch<=0 Then Call exit 'no permutations with' bunch 'elements!'

Select
  When sep='' Then ss=''''''
  When datatype(sep)='NUM' Then ss=''''copies(' ',sep)''''
  Otherwise ss=''''sep''''
  End
Do i=1 To n
  If names<>'' Then
    Parse Var names e.i names
  Else
    e.i=i
  End
a='p=0;'; Do i=1 To m; a=a||'Do p'i'=1 To n;'; End
a=a||'ol=e.p1'
          Do i=2 To m; a=a||'||'ss'||e.p'i; End
a=a||'; say ol; p=p+1;'
          Do i=1 To m; a=a||'end;'; End
a=a||'Say' p 'permutations'
/* Say a */
Interpret a
```



### version 3

This is a very simplistic version that is limited to nine things ('''N''').  

It essentially just executes a   '''do'''   loop and ignores any permutation out of range,

this is very wasteful of CPU processing time when using a larger   '''N'''.

This version could easily be extended to '''N''' up to 15   (using hexadecimal arithmetic). 

```rexx
/*REXX pgm gens all permutations with repeats of  N  objects (<10) taken  M  at a time. */
parse arg N M .
z= N**M
$= left(1234567890, N)
t= 0
          do j=copies(1, M)  until t==z
          if verify(j, $)\==0  then iterate
          t= t+1
          say j
          end   /*j*/                            /*stick a fork in it,  we're all done. */
```

```txt

11
12
13
21
22
23
31
32
33

```



## Ring


```ring

# Project : Permutations with repetitions
 
list1 = [["a", "b", "c"], ["a", "b", "c"]]
list2 = [["1", "2", "3"], ["1", "2", "3"]]
permutation(list1)
permutation(list2)
 
func permutation(list1)
     for n = 1 to len(list1[1])
         for m = 1 to len(list1[2])
             see list1[1][n] + " " + list1[2][m] + nl
         next
     next
     see nl

```

Output:

```txt

a a
a b
a c
b a
b b
b c
c a
c b
c c

1 1
1 2
1 3
2 1
2 2
2 3
3 1
3 2
3 3

```



## Ruby

This is built in　(Array#repeated_permutation):

```ruby
rp = [1,2,3].repeated_permutation(2) # an enumerator (generator)
p rp.to_a #=>[[1, 1], [1, 2], [1, 3], [2, 1], [2, 2], [2, 3], [3, 1], [3, 2], [3, 3]]

#yield permutations until their sum happens to exceed 4, then quit:
p rp.take_while{|(a, b)| a + b < 5}  #=>[[1, 1], [1, 2], [1, 3], [2, 1], [2, 2]]
```



## Scala


```scala
package permutationsRep

object PermutationsRepTest extends Application {
  /**
   * Calculates all permutations taking n elements of the input List, 
   * with repetitions. 
   * Precondition: input.length > 0 && n > 0
   */
  def permutationsWithRepetitions[T](input : List[T], n : Int) : List[List[T]] = {
    require(input.length > 0 && n > 0)
    n match {
      case 1 => for (el <- input) yield List(el)
      case _ => for (el <- input; perm <- permutationsWithRepetitions(input, n - 1)) yield el :: perm
    }
  }   
  println(permutationsWithRepetitions(List(1, 2, 3), 2))
}
```

```txt

List(List(1, 1), List(1, 2), List(1, 3), List(2, 1), List(2, 2), List(2, 3), List(3, 1), List(3, 2), List(3, 3))

```



## Sidef


```ruby
var k = %w(a b c)
var n = 2

cartesian([k] * n, {|*a| say a.join(' ') })
```

```txt

a a
a b
a c
b a
b b
b c
c a
c b
c c

```



## Tcl


### Iterative version

```tcl

proc permutate {values size offset} {
    set count [llength $values]
    set arr [list]
    for {set i 0} {$i < $size} {incr i} {
        set selector [expr [round [expr $offset / [pow $count $i]]] % $count];
        lappend arr [lindex $values $selector]
        
    }
    return $arr
}

proc permutations {values size} {
    set a [list]
    set c [pow [llength $values] $size]
    for {set i 0} {$i < $c} {incr i} {
        set permutation [permutate $values $size $i]
        lappend a $permutation
    }
    return $a
}
# Usage
permutations [list 1 2 3 4] 3

```



### Version without additional libraries

```tcl
package require Tcl 8.6

# Utility function to make procedures that define generators
proc generator {name arguments body} {
    set body [list try $body on ok {} {return -code break}]
    set lambda [list $arguments "yield \[info coroutine\];$body"]
    proc $name args "tailcall \
	coroutine gen_\[incr ::generate_ctr\] apply [list $lambda] {*}\$args"
}

# How to generate permutations with repetitions
generator permutationsWithRepetitions {input n} {
    if {[llength $input] == 0 || $n < 1} {error "bad arguments"}
    if {![incr n -1]} {
	foreach el $input {
	    yield [list $el]
	}
    } else {
	foreach el $input {
	    set g [permutationsWithRepetitions $input $n]
	    while 1 {
		yield [list $el {*}[$g]]
	    }
	}
    }
}

# Demonstrate usage
set g [permutationsWithRepetitions {1 2 3} 2]
while 1 {puts [$g]}
```


### Alternate version with extra library package

```tcl
package require Tcl 8.6
package require generator

# How to generate permutations with repetitions
generator define permutationsWithRepetitions {input n} {
    if {[llength $input] == 0 || $n < 1} {error "bad arguments"}
    if {![incr n -1]} {
	foreach el $input {
	    generator yield [list $el]
	}
    } else {
	foreach el $input {
	    set g [permutationsWithRepetitions $input $n]
	    while 1 {
		generator yield [list $el {*}[$g]]
	    }
	}
    }
}

# Demonstrate usage
generator foreach val [permutationsWithRepetitions {1 2 3} 2] {
    puts $val
}
```

