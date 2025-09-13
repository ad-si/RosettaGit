+++
title = "Self-describing numbers"
description = ""
date = 2019-10-13T05:53:14Z
aliases = []
[extra]
id = 9565
[taxonomies]
categories = ["task"]
tags = []
+++

There are several so-called "self-describing" or "[[wp:Self-descriptive number|self-descriptive]]" integers.

An integer is said to be "self-describing" if it has the property that, when digit positions are labeled 0 to N-1, the digit in each position is equal to the number of times that that digit appears in the number.

For example,   '''2020'''   is a four-digit self describing number:

*   position   0   has value   2   and there are two 0s in the number;
*   position   1   has value   0   and there are no 1s in the number;
*   position   2   has value   2   and there are two 2s;
*   position   3   has value   0   and there are zero 3s.



Self-describing numbers < 100.000.000  are:     1210,   2020,   21200,   3211000,   42101000.


;Task Description
# Write a function/routine/method/... that will check whether a given positive integer is self-describing.
# As an optional stretch goal - generate and display the set of self-describing numbers.


## Related tasks

*   [[Fours is the number of letters in the ...]]
*   [[Look-and-say sequence]]
*   [[Number names]]
*   [[Self-referential sequence]]
*   [[Spelling of ordinal numbers]]





## Ada


```Ada
with Ada.Text_IO; use Ada.Text_IO;
procedure SelfDesc is
   subtype Desc_Int is Long_Integer range 0 .. 10**10-1;

   function isDesc (innum : Desc_Int) return Boolean is
      subtype S_Int is Natural range 0 .. 10;
      type S_Int_Arr is array (0 .. 9) of S_Int;
      ref, cnt : S_Int_Arr := (others => 0);
      n, digit : S_Int := 0;  num : Desc_Int := innum;
   begin
      loop
         digit := S_Int (num mod 10);
         ref (9 - n) := digit;  cnt (digit) := cnt (digit) + 1;
         num := num / 10; exit when num = 0; n := n + 1;
      end loop;
      return ref (9 - n .. 9) = cnt (0 .. n);
   end isDesc;

begin
   for i in Desc_Int range 1 .. 100_000_000 loop
      if isDesc (i) then
         Put_Line (Desc_Int'Image (i));
      end if;
   end loop;
end SelfDesc;
```

```txt
1210
2020
21200
3211000
42101000
```



## ALGOL 68

```algol68
BEGIN

    # return TRUE if number is self describing, FALSE otherwise #
    OP SELFDESCRIBING = ( INT number )BOOL:
       BEGIN

           [10]INT counts := ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 );
           INT n          := number;
           INT digits     := 0;

           # count the occurances of each digit #
           WHILE
               n /= 0
           DO
               digits +:= 1;
               counts[ ( n MOD 10 ) + 1 ] +:= 1;
               n OVERAB 10
           OD;

           # construct the number that the counts would describe, #
           # if the number was self describing                    #

           INT described number := 0;
           FOR i TO digits
           DO
               described number *:= 10;
               described number +:= counts[ i ]
           OD;

           # if the described number is the input number, #
           # it is self describing #
           ( number = described number )
       END; # SELFDESCRIBING #

main: (

    FOR i TO 100 000 000
    DO
        IF SELFDESCRIBING i
        THEN
            print( ( i, " is self describing", newline ) )
        FI
    OD
)

END
```

```txt

      +1210 is self describing
      +2020 is self describing
     +21200 is self describing
   +3211000 is self describing
  +42101000 is self describing

```



## AutoHotkey

Uses CountSubString: [[Count occurrences of a substring#AutoHotkey]]

```AutoHotkey
; The following directives and commands speed up execution:
#NoEnv
SetBatchlines -1
ListLines Off
Process, Priority,, high

MsgBox % 2020 ": " IsSelfDescribing(2020) "`n" 1337 ": " IsSelfDescribing(1337) "`n" 1210 ": " IsSelfDescribing(1210)
Loop 100000000
   If IsSelfDescribing(A_Index)
      list .= A_Index "`n"
MsgBox % "Self-describing numbers < 100000000 :`n" . list

CountSubstring(fullstring, substring){
   StringReplace, junk, fullstring, %substring%, , UseErrorLevel
   return errorlevel
}

IsSelfDescribing(number){
   Loop Parse, number
      If Not CountSubString(number, A_Index-1) = A_LoopField
         return false
   return true
}
```

Output:

```txt
---------------------------
Self.ahk
---------------------------
Self-describing numbers < 100000000 :
1210
2020
21200
3211000
42101000

---------------------------
OK
---------------------------
```



## AWK


```AWK
# syntax: GAWK -f SELF-DESCRIBING_NUMBERS.AWK
BEGIN {
    for (n=1; n<=100000000; n++) {
      if (is_self_describing(n)) {
        print(n)
      }
    }
    exit(0)
}
function is_self_describing(n,  i) {
    for (i=1; i<=length(n); i++) {
      if (substr(n,i,1) != gsub(i-1,"&",n)) {
        return(0)
      }
    }
    return(1)
}
```

<p>output:</p>

```txt

1210
2020
21200
3211000
42101000

```



## BASIC


```qbasic
Dim x, r, b, c, n, m As Integer
Dim a, d As String
Dim v(10), w(10) As Integer
Cls
For x = 1 To 5000000
   a$ = ltrim$(Str$(x))
   b = Len(a$)
   For c = 1 To b
      d$ = Mid$(a$, c, 1)
      v(Val(d$)) = v(Val(d$)) + 1
      w(c - 1) = Val(d$)
   Next c
   r = 0
   For n = 0 To 10
      If v(n) = w(n) Then r = r + 1
      v(n) = 0
      w(n) = 0
   Next n
   If r = 11 Then Print x; " Yes,is autodescriptive number"
Next x
Print
Print "End"
sleep
end
```



## BBC BASIC

```bbcbasic
      FOR N = 1 TO 5E7
        IF FNselfdescribing(N) PRINT N
      NEXT
      END

      DEF FNselfdescribing(N%)
      LOCAL D%(), I%, L%, O%
      DIM D%(9)
      O% = N%
      L% = LOG(N%)
      WHILE N%
        I% = N% MOD 10
        D%(I%) += 10^(L%-I%)
        N% DIV=10
      ENDWHILE
      = O% = SUM(D%())
```

Output:

```txt

      1210
      2020
     21200
   3211000
  42101000
```



## Befunge

Although we simply list the conforming numbers - nothing more.

Be aware, though, that even with a fast interpreter, it's going to be a very long time before you see the full set of results.


```befunge>>1+9:0
\#06#:p#-:#1_$v
?v6:%+55:\+1\<<<\0:::<
#>g1+\6p55+/:#^_001p\v
^_@#!`<<v\+g6g10*+55\<
>:*:*:*^>>:01g1+:01p`|
^_\#\:#+.#5\#5,#$:<-$<
```


```txt
1210
2020
21200
3211000
42101000
```



## C

Using integers instead of strings.

```c
#include <stdio.h>

inline int self_desc(unsigned long long xx)
{
	register unsigned int d, x;
	unsigned char cnt[10] = {0}, dig[10] = {0};

	for (d = 0; xx > ~0U; xx /= 10)
		cnt[ dig[d++] = xx % 10 ]++;

	for (x = xx; x; x /= 10)
		cnt[ dig[d++] = x % 10 ]++;

	while(d-- && dig[x++] == cnt[d]);

	return d == -1;
}

int main()
{
	int i;
	for (i = 1; i < 100000000; i++) /* don't handle 0 */
		if (self_desc(i)) printf("%d\n", i);

	return 0;
}
```
output<lang>1210
2020
21200
3211000
42101000
```



### Backtracking version

Backtracks on each digit from right to left, takes advantage of constraints "sum of digit values = number of digits" and "sum of (digit index * digit value) = number of digits". It is using as argument the list of allowed digits (example 012345789 to run the program in standard base 10).

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BASE_MIN 2
#define BASE_MAX 94

void selfdesc(unsigned long);

const char *ref = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
char *digs;
unsigned long *nums, *inds, inds_sum, inds_val, base;

int main(int argc, char *argv[]) {
int used[BASE_MAX];
unsigned long digs_n, i;
	if (argc != 2) {
		fprintf(stderr, "Usage is %s <digits>\n", argv[0]);
		return EXIT_FAILURE;
	}
	digs = argv[1];
	digs_n = strlen(digs);
	if (digs_n < BASE_MIN || digs_n > BASE_MAX) {
		fprintf(stderr, "Invalid number of digits\n");
		return EXIT_FAILURE;
	}
	for (i = 0; i < BASE_MAX; i++) {
		used[i] = 0;
	}
	for (i = 0; i < digs_n && strchr(ref, digs[i]) && !used[digs[i]-*ref]; i++) {
		used[digs[i]-*ref] = 1;
	}
	if (i < digs_n) {
		fprintf(stderr, "Invalid digits\n");
		return EXIT_FAILURE;
	}
	nums = calloc(digs_n, sizeof(unsigned long));
	if (!nums) {
		fprintf(stderr, "Could not allocate memory for nums\n");
		return EXIT_FAILURE;
	}
	inds = malloc(sizeof(unsigned long)*digs_n);
	if (!inds) {
		fprintf(stderr, "Could not allocate memory for inds\n");
		free(nums);
		return EXIT_FAILURE;
	}
	inds_sum = 0;
	inds_val = 0;
	for (base = BASE_MIN; base <= digs_n; base++) {
		selfdesc(base);
	}
	free(inds);
	free(nums);
	return EXIT_SUCCESS;
}

void selfdesc(unsigned long i) {
unsigned long diff_sum, upper_min, j, lower, upper, k;
	if (i) {
		diff_sum = base-inds_sum;
		upper_min = inds_sum ? diff_sum:base-1;
		j = i-1;
		if (j) {
			lower = 0;
			upper = (base-inds_val)/j;
		}
		else {
			lower = diff_sum;
			upper = diff_sum;
		}
		if (upper < upper_min) {
			upper_min = upper;
		}
		for (inds[j] = lower; inds[j] <= upper_min; inds[j]++) {
			nums[inds[j]]++;
			inds_sum += inds[j];
			inds_val += inds[j]*j;
			for (k = base-1; k > j && nums[k] <= inds[k] && inds[k]-nums[k] <= i; k--);
			if (k == j) {
				selfdesc(i-1);
			}
			inds_val -= inds[j]*j;
			inds_sum -= inds[j];
			nums[inds[j]]--;
		}
	}
	else {
		for (j = 0; j < base; j++) {
			putchar(digs[inds[j]]);
		}
		puts("");
	}
}
```


Output for base 36
<lang>$ time ./selfdesc.exe 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
1210
2020
21200
3211000
42101000
521001000
6210001000
72100001000
821000001000
9210000001000
A2100000001000
B21000000001000
C210000000001000
D2100000000001000
E21000000000001000
F210000000000001000
G2100000000000001000
H21000000000000001000
I210000000000000001000
J2100000000000000001000
K21000000000000000001000
L210000000000000000001000
M2100000000000000000001000
N21000000000000000000001000
O210000000000000000000001000
P2100000000000000000000001000
Q21000000000000000000000001000
R210000000000000000000000001000
S2100000000000000000000000001000
T21000000000000000000000000001000
U210000000000000000000000000001000
V2100000000000000000000000000001000
W21000000000000000000000000000001000

real    0m0.094s
user    0m0.046s
sys     0m0.030s
```



## C++


```cpp

#include <iostream>

//--------------------------------------------------------------------------------------------------
typedef unsigned long long bigint;

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
class sdn
{
public:
    bool check( bigint n )
    {
	int cc = digitsCount( n );
	return compare( n, cc );
    }

    void displayAll( bigint s )
    {
	for( bigint y = 1; y < s; y++ )
	    if( check( y ) )
		cout << y << " is a Self-Describing Number." << endl;
    }

private:
    bool compare( bigint n, int cc )
    {
	bigint a;
	while( cc )
	{
	    cc--; a = n % 10;
	    if( dig[cc] != a ) return false;
	    n -= a; n /= 10;
	}
	return true;
    }

    int digitsCount( bigint n )
    {
	int cc = 0; bigint a;
	memset( dig, 0, sizeof( dig ) );
	while( n )
	{
	    a = n % 10; dig[a]++;
	    cc++ ; n -= a; n /= 10;
	}
	return cc;
    }

    int dig[10];
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    sdn s;
    s. displayAll( 1000000000000 );
    cout << endl << endl; system( "pause" );

    bigint n;
    while( true )
    {
	system( "cls" );
	cout << "Enter a positive whole number ( 0 to QUIT ): "; cin >> n;
	if( !n ) return 0;
	if( s.check( n ) ) cout << n << " is";
	else cout << n << " is NOT";
	cout << " a Self-Describing Number!" << endl << endl;
	system( "pause" );
    }

    return 0;
}

```

```txt

1210 is a Self-Describing Number.
2020 is a Self-Describing Number.
21200 is a Self-Describing Number.
3211000 is a Self-Describing Number.
42101000 is a Self-Describing Number.
521001000 is a Self-Describing Number.
[...]

```



###  Alternate version

Uses C++11. Build with
 g++ -std=c++11 sdn.cpp

```cpp
#include <algorithm>
#include <array>
#include <iostream>

bool is_self_describing(unsigned long long int n) noexcept {
  if (n == 0) {
    return false;
  }

  std::array<char, 10> digits = {0}, counts = {0};
  std::size_t i = digits.size();

  do {
    counts[digits[--i] = n % 10]++;
  } while ((n /= 10) > 0 && i < digits.size());

  return n == 0 && std::equal(begin(digits) + i, end(digits), begin(counts));
}

int main() {
  for (unsigned long long int i = 0; i < 10000000000; ++i) {
    if (is_self_describing(i)) {
      std::cout << i << "\n";
    }
  }
}
```

Output:

```txt

1210
2020
21200
3211000
42101000
521001000
6210001000

```



## Common Lisp

Not terribly speedy brute force.  I played around with "counting" the digits
directly into a number by adding in appropriate powers of 10 for each digit I
see but trailing zeroes kind of gum up the works.  I still think it's possible and
probably much faster because it wouldn't have to allocate an array and then
turn around and "interpret" it back out but I didn't really pursue it.

```lisp
(defun to-ascii (str) (mapcar #'char-code (coerce str 'list)))

(defun to-digits (n)
  (mapcar #'(lambda(v) (- v 48)) (to-ascii  (princ-to-string n))))

(defun count-digits (n)
  (do
      ((counts (make-array '(10) :initial-contents '(0 0 0 0 0 0 0 0 0 0)))
       (curlist (to-digits n) (cdr curlist)))
      ((null curlist) counts)
    (setf (aref counts (car curlist)) (+ 1 (aref counts (car curlist)))))))

(defun self-described-p (n)
  (if (not (numberp n))
      nil
  (do ((counts (count-digits n))
       (ipos 0 (+ 1 ipos))
       (digits (to-digits n) (cdr digits)))
      ((null digits) t)
    (if (not (eql (car digits) (aref counts ipos))) (return nil)))))
```


Output:
<lang>(loop for i from 1 to 4000000 do (if (self-described-p i) (print i)))

1210
2020
21200
3211000
NIL
```



## D


### Functional Version


```d
import std.stdio, std.algorithm, std.range, std.conv, std.string;

bool isSelfDescribing(in long n) pure nothrow @safe {
    auto nu = n.text.representation.map!q{ a - '0' };
    return nu.length.iota.map!(a => nu.count(a)).equal(nu);
}

void main() {
    4_000_000.iota.filter!isSelfDescribing.writeln;
}
```

```txt
[1210, 2020, 21200, 3211000]
```



### A Faster Version


```d
bool isSelfDescribing2(ulong n) nothrow @nogc {
  if (n <= 0)
    return false;

  __gshared static uint[10] digits, d;
  digits[] = 0;
  d[] = 0;
  int i;

  if (n < uint.max) {
    uint nu = cast(uint)n;
    for (i = 0; nu > 0 && i < digits.length; nu /= 10, i++) {
      d[i] = nu % 10;
      digits[d[i]]++;
    }
    if (nu > 0)
      return false;
  } else {
    for (i = 0; n > 0 && i < digits.length; n /= 10, i++) {
      d[i] = n % 10;
      digits[d[i]]++;
    }
    if (n > 0)
      return false;
  }

  foreach (immutable k; 0 .. i)
    if (d[k] != digits[i - k - 1])
      return false;
  return true;
}

void main() {
  import std.stdio;

  foreach (immutable x; [1210, 2020, 21200, 3211000,
                         42101000, 521001000, 6210001000])
    assert(x.isSelfDescribing2);

  foreach (immutable i; 0 .. 4_000_000)
    if (i.isSelfDescribing2)
      i.writeln;
}
```

```txt
1210
2020
21200
3211000
```

(About 0.29 seconds run time for 4 million tests.)

Output with foreach(i;0..600_000_000):

```txt
1210
2020
21200
3211000
42101000
521001000
```



## Elixir


```elixir
defmodule Self_describing do
  def number(n) do
    digits = Integer.digits(n)
    Enum.map(0..length(digits)-1, fn s ->
      length(Enum.filter(digits, fn c -> c==s end))
    end) == digits
  end
end

m = 3300000
Enum.filter(0..m, fn n -> Self_describing.number(n) end)
```


```txt

[1210, 2020, 21200, 3211000]

```



## Erlang



```erlang


sdn(N) -> lists:map(fun(S)->length(lists:filter(fun(C)->C-$0==S end,N))+$0 end,lists:seq(0,length(N)-1))==N.
gen(M) -> lists:filter(fun(N)->sdn(integer_to_list(N)) end,lists:seq(0,M)).


```



## Factor


```factor
USING: kernel math.parser prettyprint sequences ;
IN: rosetta-code.self-describing-numbers

: digits ( n -- seq ) number>string string>digits ;

: digit-count ( seq n -- m ) [ = ] curry count ;

: self-describing-number? ( n -- ? )
    digits dup [ digit-count = ] with map-index [ t = ] all? ;

100,000,000 <iota> [ self-describing-number? ] filter .
```

```txt

V{ 1210 2020 21200 3211000 42101000 }

```



## Forth



```forth
\ where unavailable.
: third ( A b c -- A b c A )  >r over r> swap ;
: (.) ( u -- c-addr u )  0 <# #s #> ;

\ COUNT is a standard word with a very different meaning, so this
\ would typically be beheaded, or given another name, or otherwise
\ given a short lifespan, so to speak.
: count ( c-addr1 u1 c -- c-addr1 u1 c+1 u )
  0 2over bounds do
    over i c@ = if 1+ then
  loop swap 1+ swap ;

: self-descriptive? ( u -- f )
  (.) [char] 0 third third bounds ?do
    count i c@ [char] 0 - <> if drop 2drop false unloop exit then
  loop drop 2drop true ;
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function selfDescribing (n As UInteger) As Boolean
   If n = 0 Then Return False
   Dim ns As String = Str(n)
   Dim count(0 To 9) As Integer '' all elements zero by default
   While n > 0
     count(n Mod 10) += 1
     n \= 10
   Wend
   For i As Integer = 0 To Len(ns) - 1
     If ns[i] - 48 <> count(i) Then Return False '' numerals have ascii values from 48 to 57
   Next
   Return True
End Function

Print "The self-describing numbers less than 100 million are:"
For i As Integer = 0 To 99999999
  If selfDescribing(i) Then Print i; " ";
Next
Print
Print "Press any key to quit"
Sleep
```


```txt

The self-describing numbers less than 100 million are:
 1210  2020  21200  3211000  42101000

```



## Go


```go
package main

import (
    "fmt"
    "strconv"
    "strings"
)

// task 1 requirement
func sdn(n int64) bool {
    if n >= 1e10 {
        return false
    }
    s := strconv.FormatInt(n, 10)
    for d, p := range s {
        if int(p)-'0' != strings.Count(s, strconv.Itoa(d)) {
            return false
        }
    }
    return true
}

// task 2 code (takes a while to run)
func main() {
    for n := int64(0); n < 1e10; n++ {
        if sdn(n) {
            fmt.Println(n)
        }
    }
}
```

Output produced by above program:

```txt

1210
2020
21200
3211000
42101000
521001000
6210001000

```



## Haskell


```Haskell
import Data.Char

count :: Int -> [Int] -> Int
count x = length . filter (x ==)

isSelfDescribing :: Integer -> Bool
isSelfDescribing n = nu == f
  where
    nu = digitToInt <$> show n
    f = (`count` nu) <$> [0 .. length nu - 1]

main :: IO ()
main = do
  print $
    isSelfDescribing <$>
    [1210, 2020, 21200, 3211000, 42101000, 521001000, 6210001000]
  print $ filter isSelfDescribing [0 .. 4000000]
```

Output:

```txt
[True,True,True,True,True,True,True]
[1210,2020,21200,3211000]
```


Here are functions for generating all the self-describing numbers of a certain length. We capitalize on the fact (from Wikipedia) that a self-describing number of length n is a base-n number (i.e. all digits are 0..n-1).

```haskell
import Data.Char (intToDigit)
import Control.Monad (replicateM, forM_)

count :: Int -> [Int] -> Int
count x = length . filter (x ==)

-- all the combinations of n digits of base n
-- a base-n number are represented as a list of ints, one per digit
allBaseNNumsOfLength :: Int -> [[Int]]
allBaseNNumsOfLength = replicateM <*> (enumFromTo 0 . subtract 1)

isSelfDescribing :: [Int] -> Bool
isSelfDescribing num = all (\(i, x) -> x == count i num) $ zip [0 ..] num

-- translate it back into an integer in base-10
decimalize :: [Int] -> Int
decimalize = read . map intToDigit

main :: IO ()
main =
  (print . concat) $
  map decimalize . filter isSelfDescribing . allBaseNNumsOfLength <$> [1 .. 8]
```

```txt
[1210,2020,21200,3211000,42101000]
```


=={{header|Icon}} and {{header|Unicon}}==

The following program contains the procedure <code>is_self_describing</code> to test if a number is a self-describing number, and the procedure <code>self_describing_numbers</code> to generate them.


```Icon

procedure count (test_item, str)
  result := 0
  every item := !str do
    if test_item == item then result +:= 1
  return result
end

procedure is_self_describing (n)
  ns := string (n) # convert to a string
  every i := 1 to *ns do {
    if count (string(i-1), ns) ~= ns[i] then fail
  }
  return 1 # success
end

# generator for creating self_describing_numbers
procedure self_describing_numbers ()
  n := 1
  repeat {
    if is_self_describing(n) then suspend n
    n +:= 1
  }
end

procedure main ()
  # write the first 4 self-describing numbers
  every write (self_describing_numbers ()\4)
end

```

A slightly more concise solution can be derived from the above by taking
more advantage of Icon's (and Unicon's) automatic goal-directed
evaluation:

```unicon

procedure is_self_describing (n)
  ns := string (n) # convert to a string
  every i := 1 to *ns do {
      if count (string(i-1), ns) ~= ns[i] then fail
      }
  return n # on success, return the self-described number
end

procedure self_describing_numbers ()
  suspend is_self_describing(seq())
end
```



## J


'''Solution''':
```j
   digits   =: 10&#.^:_1
   counts   =: _1 + [: #/.~ i.@:# , ]
   selfdesc =: = counts&.digits"0       NB.  Note use of "under"
```

'''Example''':
```j
   selfdesc 2020 1210 21200 3211000 43101000 42101000
1 1 1 1 0 1
```

'''Extra credit''':
```j
   I.@:selfdesc i. 1e6
1210 2020 21200
```

'''Discussion''': The use of <tt>&.</tt> here is a great example of its surprisingly broad applicability, and the elegance it can produce.

The use of <tt>"0</tt> is less satisfying, expressing an essentially scalar solution, and that such an approach runs against the grain of J becomes quite evident when executing the extra credit sentence.

It would not be difficult to rephrase the verb in a way that would take advantage of J's array mastery, but it would cost us of some of the simplicity and elegance of the existing solution.  More gratifying would be  some kind of closed-form, algebraic formula that could identify the SDNs directly, without test-and-filter.

That said, note that this is an incomplete implementation of the extra-credit problem -- and, hypothetically speaking, numbers longer than 9 digits could be valid results in the extra-credit problem (we just have to be sure that digit positions which are not occupied by digits we can represent have 0 for their count).  This might allow us to treat numbers up to just under 19 digits as self describing numbers.  This is a slightly larger range of numbers than we get for positive integers from signed 64 bit representation.  So a proper solution to this problem on currently available hardware (one that finds the complete result in some useful span of time) probably should use a non-brute-force solution.


## Java


```java
public class SelfDescribingNumbers{
    public static boolean isSelfDescribing(int a){
        String s = Integer.toString(a);
        for(int i = 0; i < s.length(); i++){
            String s0 = s.charAt(i) + "";
            int b = Integer.parseInt(s0); // number of times i-th digit must occur for it to be a self describing number
            int count = 0;
            for(int j = 0; j < s.length(); j++){
                int temp = Integer.parseInt(s.charAt(j) + "");
                if(temp == i){
                    count++;
                }
                if (count > b) return false;
            }
            if(count != b) return false;
        }
        return true;
    }

    public static void main(String[] args){
        for(int i = 0; i < 100000000; i++){
            if(isSelfDescribing(i)){
                System.out.println(i);
             }
        }
    }
}
```



## JavaScript

```javascript
function is_self_describing(n) {
    var digits = Number(n).toString().split("").map(function(elem) {return Number(elem)});
    var len = digits.length;
    var count = digits.map(function(x){return 0});

    digits.forEach(function(digit, idx, ary) {
        if (digit >= count.length)
            return false
        count[digit] ++;
    });

    return digits.equals(count);
}

Array.prototype.equals = function(other) {
    if (this === other)
        return true;  // same object
    if (this.length != other.length)
        return false;
    for (idx in this)
        if (this[idx] !== other[idx])
            return false;
    return true;
}

for (var i=1; i<=3300000; i++)
    if (is_self_describing(i))
        print(i);
```


outputs

```txt
1210
2020
21200
3211000
```



## jq

```jq
# If your jq includes all/2 then comment out the following definition,
# which is slightly less efficient:
def all(generator; condition):
  reduce generator as $i (true; if . then $i | condition else . end);
```


```jq
def selfie:
  def count(value): reduce .[] as $i (0; if $i == value then . + 1 else . end);
  def digits: tostring | explode | map(. - 48);

  digits
  | if  add != length then false
    else . as $digits
    | all ( range(0; length); . as $i | $digits | (.[$i] == count($i)) )
    end;
```

'''The task:'''

```jq
range(0; 100000001) | select(selfie)
```

```sh
$ jq -n -f Self-describing_numbers.jq
1210
2020
21200
3211000
42101000
```



## Julia

```julia
function selfie(x::Integer)
	ds = reverse(digits(x))
	if sum(ds) != length(ds) return false end
	for (i, d) in enumerate(ds)
		if d != sum(ds .== i - 1) return false end
	end
	return true
end

@show selfie(2020)
@show selfie(2021)

selfies(x) = for i in 1:x selfie(i) && println(i) end
@time selfies(4000000)
```


```txt
1210
2020
21200
3211000
  1.398922 seconds (8.01 M allocations: 1.049 GiB, 6.91% gc time)
```



## K


```k
  sdn: {n~+/'n=/:!#n:0$'$x}'
  sdn 1210 2020 2121 21200 3211000 42101000
1 1 0 1 1 1

  &sdn@!:1e6
1210 2020 21200
```



## Kotlin


```scala
// version 1.0.6

fun selfDescribing(n: Int): Boolean {
    if (n <= 0) return false
    val ns = n.toString()
    val count = IntArray(10)
    var nn = n
    while (nn > 0) {
        count[nn % 10] += 1
        nn /= 10
    }
    for (i in 0 until ns.length)
        if( ns[i] - '0' != count[i]) return false
    return true
}

fun main(args: Array<String>) {
    println("The self-describing numbers less than 100 million are:")
    for (i in 0..99999999) if (selfDescribing(i)) print("$i ")
    println()
}
```


```txt

The self-describing numbers less than 100 million are:
1210 2020 21200 3211000 42101000

```


=={{Header|Liberty BASIC}}==

```lb
'adapted from BASIC solution
FOR x = 1 TO 5000000
   a$ = TRIM$(STR$(x))
   b = LEN(a$)
   FOR c = 1 TO b
      d$ = MID$(a$, c, 1)
      v(VAL(d$)) = v(VAL(d$)) + 1
      w(c - 1) = VAL(d$)
   NEXT c
   r = 0
   FOR n = 0 TO 10
      IF v(n) = w(n) THEN r = r + 1
      v(n) = 0
      w(n) = 0
   NEXT n
   IF r = 11 THEN PRINT x; " is a self-describing number"
NEXT x
PRINT
PRINT "End"
```


=={{Header|LiveCode}}==

```LiveCode
function selfDescNumber n
    local tSelfD, tLen
    put len(n) into tLen
    repeat with x = 0 to (tLen - 1)
        put n into nCopy
        replace x with empty in nCopy
        put char (x + 1) of n = (tLen - len(nCopy)) into tSelfD
        if not tSelfD then exit repeat
    end repeat
    return tSelfD
end selfDescNumber
```

To list the self-describing numbers to 10 million
```LiveCode
on mouseUp
    repeat with n = 0 to 10000000
        if selfDescNumber(n) then
            put n into selfNum[n]
        end if
    end repeat
    combine selfNum using comma
    put selfNum
end mouseUp
```

Output
```LiveCode
1210,2020,21200,3211000
```



## Logo


```logo
TO XX
BT
MAKE "AA (ARRAY 10 0)
MAKE "BB (ARRAY 10 0)
FOR [Z 0 9][SETITEM :Z :AA "0 SETITEM :Z :BB "0 ]
   FOR [A 1 50000][
      MAKE "B COUNT :A
      MAKE "Y 0
      MAKE "X 0
      MAKE "R 0
      MAKE "J 0
      MAKE "K 0

   FOR [C 1 :B][MAKE "D ITEM :C :A
      SETITEM :C - 1 :AA :D
      MAKE "X ITEM :D :BB
      MAKE "Y :X + 1
      SETITEM :D :BB :Y
      MAKE "R 0]
   FOR [Z 0 9][MAKE "J ITEM :Z :AA
      MAKE "K ITEM :Z :BB
      IF :J = :K [MAKE "R :R + 1]]
IF :R = 10 [PR :A]
FOR [Z 0 9][SETITEM :Z :AA "0 SETITEM :Z :BB "0 ]]
PR [END]
END
```



## Lua


```lua
function Is_self_describing( n )
    local s = tostring( n )

    local t = {}
    for i = 0, 9 do t[i] = 0 end

    for i = 1, s:len() do
	local idx = tonumber( s:sub(i,i) )
        t[idx] = t[idx] + 1
    end

    for i = 1, s:len() do
        if t[i-1] ~= tonumber( s:sub(i,i) ) then return false end
    end

    return true
end

for i = 1, 999999999 do
    print( Is_self_describing( i ) )
end
```



## Mathematica


```Mathematica
isSelfDescribing[n_Integer] := (RotateRight[DigitCount[n]] == PadRight[IntegerDigits[n], 10])
```



```txt
Select[Range[10^10 - 1], isSelfDescribing]
-> {1210,2020,21200,3211000,42101000,521001000,6210001000}
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
function z = isSelfDescribing(n)
  s = int2str(n)-'0';    % convert to vector of digits
  y = hist(s,0:9);
  z = all(y(1:length(s))==s);
end;
```


Test function:


```Matlab
for k = 1:1e10,
   if isSelfDescribing(k),
      printf('%i\n',k);
   end
end;
```


Output:

```txt
  1210
  2020
  21200
  ...
```



## MiniScript


```MiniScript
numbers = [12, 1210, 1300, 2020, 21200, 5]

occurrences = function(test, values)
    count = 0
    for i in values
        if i.val == test then count = count + 1
    end for
    return count
end function

for number in numbers
    check = "" + number
    digits = check.values
    describing = true
    for digit in digits.indexes
        if digits[digit].val != occurrences(digit, digits) then
            describing = false
        end if
    end for
    if describing then
        print number + " is self describing"
    else
        print number + " is not self describing"
    end if
end for

```

```txt

12 is not self describing
1210 is self describing
1300 is not self describing
2020 is self describing
21200 is self describing
5 is not self describing

```


=={{header|Modula-2}}==
```modula2

MODULE SelfDescribingNumber;

FROM WholeStr IMPORT
  CardToStr;
FROM STextIO IMPORT
  WriteString, WriteLn;
FROM SWholeIO IMPORT
  WriteCard;

PROCEDURE Check(Number: CARDINAL): BOOLEAN;
VAR
  I, D: CARDINAL;
  A: ARRAY [0 .. 9] OF CHAR;
  Count, W: ARRAY [0 .. 9] OF CARDINAL;
  Result: BOOLEAN;
BEGIN
  CardToStr(Number, A);
  FOR I := 0 TO 9 DO
    Count[I] := 0;
    W[I] := 0;
  END;
  FOR I := 0 TO LENGTH(A) - 1 DO
    D := ORD(A[I]) - ORD("0");
    INC(Count[D]);
    W[I] := D;
  END;
  Result := TRUE;
  I := 0;
  WHILE Result AND (I <= 9) DO
    Result := (Count[I] = W[I]);
    INC(I);
  END;
  RETURN Result;
END Check;

VAR
  X: CARDINAL;

BEGIN
  WriteString("Autodescriptive numbers from 1 to 100000000:");
  WriteLn;
  FOR X := 1 TO 100000000 DO
    IF Check(X) THEN
      WriteString(" ");
      WriteCard(X, 1);
      WriteLn;
    END;
  END;
  WriteString("Job done.");
  WriteLn;
END SelfDescribingNumber.

```

```txt

Autodescriptive numbers from 1 to 100000000:
 1210
 2020
 21200
 3211000
 42101000
Job done.

```



## Nim


```nim
import strutils

proc count(s, sub): int =
  var i = 0
  while true:
    i = s.find(sub, i)
    if i < 0:
      break
    inc i
    inc result

proc isSelfDescribing(n): bool =
  let s = $n
  for i, ch in s:
    if s.count($i) != parseInt("" & ch):
      return false
  return true

for x in 0 .. 4_000_000:
  if isSelfDescribing(x): echo x
```

Output:

```txt
1210
2020
21200
321100
```



## ooRexx


```ooRexx

-- REXX program to check if a number (base 10) is self-describing.
parse arg x y .
if x=='' then exit
if y=='' then y=x
-- 10 digits is the maximum size number that works here, so cap it
numeric digits 10
y=min(y, 9999999999)

loop number = x to y
  loop i = 1 to number~length
      digit = number~subchar(i)
      -- return on first failure
      if digit \= number~countstr(i - 1) then iterate number
   end
   say number "is a self describing number"
end

```

'''output''' when using the input of: <tt> 0 999999999 </tt>
<pre style="overflow:scroll">
1210 is a self-describing number.
2020 is a self-describing number.
21200 is a self-describing number.
3211000 is a self-describing number.
42101000 is a self-describing number.
521001000 is a self-describing number.
6210001000 is a self-describing number.

```



## PARI/GP

This is a finite set...

```parigp
S=[1210, 2020, 21200, 3211000, 42101000, 521001000, 6210001000];
isself(n)=vecsearch(S,n)
```



## Pascal


```pascal
Program SelfDescribingNumber;

uses
  SysUtils;

function check(number: longint): boolean;
  var
    i, d: integer;
    a: string;
    count, w : array [0..9] of integer;

  begin
    a := intToStr(number);
    for i := 0 to 9 do
    begin
      count[i] := 0;
      w[i] := 0;
    end;
    for i := 1 to length(a) do
    begin
      d := ord(a[i]) - ord('0');
      inc(count[d]);
      w[i - 1] := d;
    end;
    check := true;
    i := 0;
    while check and (i <= 9) do
    begin
      check := count[i] = w[i];
      inc(i);
    end;
  end;

var
  x: longint;

begin
  writeln ('Autodescriptive numbers from 1 to 100000000:');
  for x := 1 to 100000000 do
    if check(x) then
      writeln (' ', x);
  writeln('Job done.');
end.
```

Output:

```txt

:> ./SelfDescribingNumber
Autodescriptive numbers from 1 to 100000000:
 1210
 2020
 21200
 3211000
 42101000
Job done.

```



## Perl

The idea is to make two arrays: the first one contains the digits at their positions and the second one contains the digits counts.

The number is self-descriptive If the arrays are equal.

```perl
sub is_selfdesc
{
	local $_ = shift;
	my @b = (0) x length;
	$b[$_]++ for my @a = split //;
	return "@a" eq "@b";
}

# check all numbers from 0 to 100k plus two 'big' ones
for (0 .. 100000, 3211000, 42101000) {
	print "$_\n" if is_selfdesc($_);
}
```

Output:

```txt
1210
2020
21200
3211000
42101000
```



## Perl 6


```perl6
my @values = <1210 2020 21200 3211000
42101000 521001000 6210001000 27 115508>;

for @values -> $test {
    say "$test is {sdn($test) ?? '' !! 'NOT ' }a self describing number.";
}

sub sdn($n) {
    my $s = $n.Str;
    my $chars = $s.chars;
    my @a = +«$s.comb;
    my @b;
    for @a -> $i {
        return False if $i >= $chars;
        ++@b[$i];
    }
    @b[$_] //= 0 for ^$chars;
    @a eqv @b;
}

.say if .&sdn for ^9999999;
```

Output:

```txt

1210 is a self describing number.
2020 is a self describing number.
21200 is a self describing number.
3211000 is a self describing number.
42101000 is a self describing number.
521001000 is a self describing number.
6210001000 is a self describing number.
27 is NOT a self describing number.
115508 is NOT a self describing number.
1210
2020
21200
3211000

```



## Phix

```Phix
function self_desc(integer i)
sequence digits = repeat(0,10), counts = repeat(0,10)
integer n = 0, digit
    while 1 do
        digit := mod(i,10)
        digits[10-n] := digit
        counts[digit+1] += 1
        i = floor(i/10)
        if i=0 then exit end if
        n += 1
    end while
    return digits[10-n..10] = counts[1..n+1]
end function

atom t0 = time()
for i=10 to 100_000_000 by 10 do
    if self_desc(i) then ?i end if
end for
printf(1,"done (%3.2fs)",time()-t0)
```

```txt

1210
2020
21200
3211000
42101000
done (21.78s)

```



## PHP

Works with: PHP 5.


```PHP
<?php

function is_describing($number) {
    foreach (str_split((int) $number) as $place => $value) {
        if (substr_count($number, $place) != $value) {
            return false;
        }
    }
    return true;
}

for ($i = 0; $i <= 50000000; $i += 10) {
    if (is_describing($i)) {
        echo $i . PHP_EOL;
    }
}

?>
```


Output:

```txt
1210
2020
21200
3211000
42101000
```



## PicoLisp


```PicoLisp
(de selfDescribing (N)
   (fully '((D I) (= D (cnt = N (circ I))))
      (setq N (mapcar format (chop N)))
      (range 0 (length N)) ) )
```

Output:

```txt
: (filter selfDescribing (range 1 4000000))
-> (1210 2020 21200 3211000)
```



## PowerShell

According to the Wiki definition, the sum of the products of the index and the
digit contained at the index should equal the number of digits in the number:

```PowerShell

function Test-SelfDescribing ([int]$Number)
{
    [int[]]$digits = $Number.ToString().ToCharArray() | ForEach-Object {[Char]::GetNumericValue($_)}
    [int]$sum = 0

    for ($i = 0; $i -lt $digits.Count; $i++)
    {
        $sum += $i * $digits[$i]
    }

    $sum -eq $digits.Count
}

```


```PowerShell

Test-SelfDescribing -Number 2020

```

```txt

True

```

It takes a very long while to test 100,000,000 numbers, and since they are already known just test a few:

```PowerShell

11,2020,21200,321100 | ForEach-Object {
    [PSCustomObject]@{
        Number = $_
        IsSelfDescribing = Test-SelfDescribing -Number $_
    }
} | Format-Table -AutoSize

```

```txt

Number IsSelfDescribing
------ ----------------
    11            False
  2020             True
 21200             True
321100            False

```



## Prolog

Works with SWI-Prolog and library clpfd written by <b>Markus Triska</b>.

```Prolog
:- use_module(library(clpfd)).

self_describling :-
	forall(between(1, 10, I),
	       (findall(N, self_describling(I,N), L),
		format('Len ~w, Numbers ~w~n', [I, L]))).

% search of the self_describling numbers of a given len
self_describling(Len, N) :-
	length(L, Len),
	Len1 is Len - 1,
	L = [H|T],

	% the first figure is greater than 0
	H in 1..Len1,

	% there is a least to figures so the number of these figures
	% is at most Len - 2
	Len2 is Len - 2,
	T ins 0..Len2,

	% the sum of the figures is equal to the len of the number
	sum(L, #=, Len),

	% There is at least one figure corresponding to the number of zeros
	H1 #= H+1,
	element(H1, L, V),
	V #> 0,

	% create the list
	label(L),

	% test the list
	msort(L, LNS),
	packList(LNS,LNP),
	numlist(0, Len1, NumList),
	verif(LNP,NumList, L),

	% list is OK, create the number
	maplist(atom_number, LA, L),
	number_chars(N, LA).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% testing a number (not use in this program)
self_describling(N) :-
	number_chars(N, L),
	maplist(atom_number, L, LN),
	msort(LN, LNS),
	packList(LNS,LNP), !,
	length(L, Len),
	Len1 is Len - 1,
	numlist(0, Len1, NumList),
	verif(LNP,NumList, LN).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% verif(PackList, Order_of_Numeral, Numeral_of_the_nuber_to_test)
%  Packlist is of the form [[Number_of_Numeral, Order_of_Numeral]|_]
%  Test succeed when

%  All lists are empty
verif([], [], []).

% Packlist is empty and all lasting numerals are 0
verif([], [_N|S], [0|T]) :-
	verif([], S, T).

% Number of numerals N is V
verif([[V, N]|R], [N|S], [V|T]) :-
	verif(R, S, T).

% Number of numerals N is 0
verif([[V, N1]|R], [N|S], [0|T]) :-
	N #< N1,
	verif([[V,N1]|R], S, T).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ?- packList([a,a,a,b,c,c,c,d,d,e], L).
%  L = [[3,a],[1,b],[3,c],[2,d],[1,e]] .
% ?- packList(R,  [[3,a],[1,b],[3,c],[2,d],[1,e]]).
% R = [a,a,a,b,c,c,c,d,d,e] .
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
packList([],[]).

packList([X],[[1,X]]) :- !.


packList([X|Rest],[XRun|Packed]):-
    run(X,Rest, XRun,RRest),
    packList(RRest,Packed).


run(Var,[],[1, Var],[]).

run(Var,[Var|LRest],[N1, Var],RRest):-
    N #> 0,
    N1 #= N + 1,
    run(Var,LRest,[N, Var],RRest).


run(Var,[Other|RRest], [1, Var],[Other|RRest]):-
    dif(Var,Other).
```


Output

```txt
 ?- self_describling.
Len 1, Numbers []
Len 2, Numbers []
Len 3, Numbers []
Len 4, Numbers [1210,2020]
Len 5, Numbers [21200]
Len 6, Numbers []
Len 7, Numbers [3211000]
Len 8, Numbers [42101000]
Len 9, Numbers [521001000]
Len 10, Numbers [6210001000]
true.

```



## PureBasic


```PureBasic
Procedure isSelfDescribing(x.q)
  ;returns 1 if number is self-describing, otherwise it returns 0
  Protected digitCount, digit, i, digitSum
  Dim digitTally(10)
  Dim digitprediction(10)

  If x <= 0
    ProcedureReturn 0 ;number must be positive and non-zero
  EndIf

  While x > 0 And i < 10
    digit = x % 10
    digitSum + digit
    If digitSum > 10
      ProcedureReturn 0 ;sum of digits' values exceeds maximum possible
    EndIf
    digitprediction(i) = digit
    digitTally(digit) + 1
    x / 10
    i + 1
  Wend
  digitCount = i - 1

  If digitSum < digitCount Or x > 0
    ProcedureReturn 0  ;sum of digits' values is too small or number has more than 10 digits
  EndIf

  For i = 0 To digitCount
    If digitTally(i) <> digitprediction(digitCount - i)
      ProcedureReturn 0 ;number is not self-describing
    EndIf
  Next
  ProcedureReturn 1 ;number is self-describing
EndProcedure

Procedure displayAll()
  Protected i, j, t
  PrintN("Starting search for all self-describing numbers..." + #CRLF$)
  For j = 0 To 9
    PrintN(#CRLF$ + "Searching possibilites " + Str(j * 1000000000) + " -> " + Str((j + 1) * 1000000000 - 1)+ "...")
    t = ElapsedMilliseconds()
    For i = 0 To 999999999
      If isSelfDescribing(j * 1000000000 + i)
        PrintN(Str(j * 1000000000 + i))
      EndIf
    Next
    PrintN("Time to search this range of possibilities: " + Str((ElapsedMilliseconds() - t) / 1000) + "s.")
  Next
  PrintN(#CRLF$ + "Search complete.")
EndProcedure

If OpenConsole()

  DataSection
    Data.q 1210, 2020, 21200, 3211000, 42101000, 521001000, 6210001000, 3214314
  EndDataSection

  Define i, x.q
  For i = 1 To 8
    Read.q x
    Print(Str(x) + " is ")
    If Not isSelfDescribing(x)
      Print("not ")
    EndIf
    PrintN("selfdescribing.")
  Next
  PrintN(#CRLF$)

  displayAll()

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
1210 is selfdescribing.
2020 is selfdescribing.
21200 is selfdescribing.
3211000 is selfdescribing.
42101000 is selfdescribing.
521001000 is selfdescribing.
6210001000 is selfdescribing.
3214314 is not selfdescribing.


Starting search for all self-describing numbers...


Searching possibilites 0 -> 999999999...
1210
2020
21200
3211000
42101000
521001000
Time to search this range of possibilities: 615s.

Searching possibilites 1000000000 -> 1999999999...
Time to search this range of possibilities: 614s.

Searching possibilites 2000000000 -> 2999999999...
Time to search this range of possibilities: 628s.

Searching possibilites 3000000000 -> 3999999999...
Time to search this range of possibilities: 631s.

Searching possibilites 4000000000 -> 4999999999...
Time to search this range of possibilities: 630s.

Searching possibilites 5000000000 -> 5999999999...
Time to search this range of possibilities: 628s.

Searching possibilites 6000000000 -> 6999999999...
6210001000
Time to search this range of possibilities: 629s.

Searching possibilites 7000000000 -> 7999999999...
Time to search this range of possibilities: 631s.

Searching possibilites 8000000000 -> 8999999999...
Time to search this range of possibilities: 629s.

Searching possibilites 9000000000 -> 9999999999...
Time to search this range of possibilities: 629s.

Search complete.
```



## Python


```python>>>
 def isSelfDescribing(n):
	s = str(n)
	return all(s.count(str(i)) == int(ch) for i, ch in enumerate(s))

>>> [x for x in range(4000000) if isSelfDescribing(x)]
[1210, 2020, 21200, 3211000]
>>> [(x, isSelfDescribing(x)) for x in (1210, 2020, 21200, 3211000, 42101000, 521001000, 6210001000)]
[(1210, True), (2020, True), (21200, True), (3211000, True), (42101000, True), (521001000, True), (6210001000, True)]
```


### Generator

From [http://leetm.mingpao.com/cfm/Forum3.cfm?CategoryID=1&TopicID=1545&TopicOrder=Desc&TopicPage=1 here].

```python
def impl(d, c, m):
    if m < 0: return
    if d == c[:len(d)]: print d
    for i in range(c[len(d)],m+1):
        dd = d+[i]
        if i<len(dd) and c[i]==dd[i]: continue
        impl(dd,c[:i]+[c[i]+1]+c[i+1:],m-i)

def self(n): impl([], [0]*(n+1), n)

self(10)
```

Output:

```txt
[]
[1, 2, 1, 0]
[2, 0, 2, 0]
[2, 1, 2, 0, 0]
[3, 2, 1, 1, 0, 0, 0]
[4, 2, 1, 0, 1, 0, 0, 0]
[5, 2, 1, 0, 0, 1, 0, 0, 0]
[6, 2, 1, 0, 0, 0, 1, 0, 0, 0]
```



## Racket


```Racket
#lang racket
(define (get-digits number (lst null))
  (if (zero? number)
      lst
      (get-digits (quotient number 10) (cons (remainder number 10) lst))))

(define (self-describing? number)
  (if (= number 0) #f
      (let ((digits (get-digits number)))
        (for/fold ((bool #t))
          ((i (in-range (length digits))))
          (and bool
               (= (count (lambda (x) (= x i)) digits)
                  (list-ref digits i)))))))
```


Sadly, the implementation is too slow for the optional task, taking somewhere around 3 minutes to check all numbers below 100.000.000

## Red


```Red
Red []

;;-------------------------------------
count-dig: func ["count occurence of digit in number"
;;-------------------------------------
  s [string!] "number as string"
  sdig [char!] "search number as char"
][
cnt: #"0" ;; counter as char for performance optimization

while [s: find/tail s sdig][cnt: cnt + 1]
return cnt
]

;;-------------------------------------
isSDN?: func ["test if number is self describing number"
  s [string!] "number to test as string "
  ][
;;-------------------------------------

ind: #"0" ;; use digit as char for performance optimization

foreach ele s [
  if ele <> count-dig s ind [return false]
  ind: ind + 1
]
return true
]

repeat i 4000000 [  if isSDN? to-string i [print i] ]

```

'''output'''

```txt
1210
2020
21200
3211000
>>

```



## REXX

Also see:    [http://oeis.org/A046043 OEIS A46043]
  and   [http://oeis.org/A138480 OEIS A138480].

### digit by digit test


```rexx
/*REXX program determines if a number (in base 10)  is a  self─describing,              */
/*──────────────────────────────────────────────────────  self─descriptive,             */
/*──────────────────────────────────────────────────────  autobiographical,   or a      */
/*──────────────────────────────────────────────────────  curious  number.              */
parse arg x y .                                  /*obtain optional arguments from the CL*/
if x=='' | x==","  then exit                     /*Not specified?  Then get out of Dodge*/
if y=='' | y==","  then y=x                      /* "      "       Then use the X value.*/
w=length(y)                                      /*use  Y's  width for aligned output.  */
numeric digits max(9, w)                         /*ensure we can handle larger numbers. */
if x==y  then do                                 /*handle the case of a single number.  */
              noYes=test_SDN(y)                  /*is it  or  ain't it?                 */
              say y  word("is isn't", noYes+1)  'a self-describing number.'
              exit
              end

         do n=x  to  y
         if test_SDN(n)  then iterate            /*if not self─describing,  try again.  */
         say  right(n,w)  'is a self-describing number.'                       /*is it? */
         end   /*n*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
test_SDN: procedure; parse arg ?;    L=length(?) /*obtain the argument  and  its length.*/
                    do j=L  to 1  by -1          /*parsing backwards is slightly faster.*/
                    if substr(?,j,1)\==L-length(space(translate(?,,j-1),0))  then return 1
                    end   /*j*/
          return 0                               /*faster if used inverted truth table. */
```


```txt

        ╔══════════════════════════════════════════════════════════════════╗
        ║ The method used above is to TRANSLATE the digit being queried to ║
        ║ blanks,  then use the  SPACE  BIF function to remove all blanks, ║
        ║ and then compare the new number's length to the original length. ║
        ║                                                                  ║
        ║ The difference in  length  is the  number of digits  translated. ║
        ╚══════════════════════════════════════════════════════════════════╝

```

'''output'''   when using the input of:   <tt> 0   9999999999 </tt>

```txt

      1210 is a self-describing number.
      2020 is a self-describing number.
     21200 is a self-describing number.
   3211000 is a self-describing number.
  42101000 is a self-describing number.
 521001000 is a self-describing number.
6210001000 is a self-describing number.

```



### faster method

(Uses table lookup.)

```rexx
/*REXX program  determines  if a  number  (in base 10)   is  a  self-describing  number.*/
parse arg x y .                                  /*obtain optional arguments from the CL*/
if x=='' | x==","  then exit                     /*Not specified?  Then get out of Dodge*/
if y=='' | y==","  then y=x                      /*Not specified?  Then use the X value.*/
w=length(y)                                      /*use  Y's  width for aligned output.  */
numeric digits max(9, w)                         /*handle the possibility of larger #'s.*/
$= '1210 2020 21200 3211000 42101000 521001000 6210001000'        /*the list of numbers.*/
                                                 /*test for a  single  integer.         */
if x==y  then do                                 /*handle the case of a single number.  */
              say word("isn't is",  wordpos(x, $) + 1)     'a self-describing number.'
              exit
              end
                                                 /* [↓]  test for a  range  of integers.*/
         do n=x  to y;  parse var  n  ''  -1  _  /*obtain the last decimal digit of  N. */
         if _\==0              then iterate
         if wordpos(n, $)==0   then iterate
         say  right(n,w)  'is a self-describing number.'
         end   /*n*/
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''   is the same as the 1<sup>st</sup> REXX example.


### fastest method

(Uses a table look-up.)

(Results are instantaneous.)

```rexx
/*REXX program  determines  if a  number  (in base 10)   is  a  self-describing  number.*/
parse arg x y .                                  /*obtain optional arguments from the CL*/
if x=='' | x==","  then exit                     /*Not specified?  Then get out of Dodge*/
if y=='' | y==","  then y=x                      /*Not specified?  Then use the X value.*/
w=length(y)                                      /*use  Y's  width for aligned output.  */
numeric digits max(9, w)                         /*handle the possibility of larger #'s.*/
$= '1210 2020 21200 3211000 42101000 521001000 6210001000'        /*the list of numbers.*/
                                                 /*test for a  single  integer.         */
if x==y  then do                                 /*handle the case of a single number.  */
              say word("isn't is",  wordpos(x, $) + 1)     'a self-describing number.'
              exit
              end
                                                 /* [↓]  test for a  range  of integers.*/
         do n=1  for words($);     _=word($, n)  /*look for integers that are in range. */
         if _<x | _>y  then iterate              /*if not self-describing, try again.   */
         say  right(_, w)       'is a self-describing number.'
         end   /*n*/                             /*stick a fork in it,  we're all done. */
```

'''output'''   is the same as the 1<sup>st</sup> REXX example.





## Ring


```ring

# Project : Self-describing numbers

for num = 1 to 45000000
     res = 0
     for n=1 to len(string(num))
          temp = string(num)
          pos = number(temp[n])
          cnt = count(temp,string(n-1))
          if cnt = pos
             res = res + 1
          ok
      next
      if res = len(string(num))
         see num + nl
      ok
next

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

1210
2020
21200
3211000
42101000

```



## Ruby


```ruby
def self_describing?(n)
  digits = n.digits.reverse
  digits.each_with_index.all?{|digit, idx| digits.count(idx) == digit}
end

3_300_000.times {|n| puts n if self_describing?(n)}
```

outputs

```txt
1210
2020
21200
3211000
```



## Run BASIC


```Runbasic
for i = 0 to 50000000 step 10
   a$ = str$(i)
   for c = 1 TO len(a$)
      d      = val(mid$(a$, c, 1))
      j(d)   = j(d) + 1
      k(c-1) = d
   next c
   r = 0
   for n = 0 to 10
      r    = r + (j(n) = k(n))
      j(n) = 0
      k(n) = 0
   next n
   if r = 11 then print i
next i
print "== End =="
end
```



## Rust


```rust

fn is_self_desc(xx: u64) -> bool
{
    let s: String = xx.to_string();
    let mut count_vec = vec![0; 10];
    for c in s.chars() {
        count_vec[c.to_digit(10).unwrap() as usize] += 1;
    }
    for (i, c) in s.chars().enumerate() {
        if count_vec[i] != c.to_digit(10).unwrap() as usize {
            return false;
        }
    }
    return true;
}

fn main() {
    for i in 1..100000000 {
        if is_self_desc(i) {
            println!("{}", i)
        }
    }
}

```



## Scala


### Functional Programming


```Scala
object SelfDescribingNumbers extends App {
  def isSelfDescribing(a: Int): Boolean = {
    val s = Integer.toString(a)

    (0 until s.length).forall(i => s.count(_.toString.toInt == i) == s(i).toString.toInt)
  }

  println("Curious numbers n = x0 x1 x2...x9 such that xi is the number of digits equal to i in n.")

  for (i <- 0 to 42101000 by 10
       if isSelfDescribing(i)) println(i)

  println("Successfully completed without errors.")
}
```


See it running in your browser by [https://scastie.scala-lang.org/vQv61PpoSLeWwyVipLUevQ Scastie (JVM)].


## Seed7


```seed
$ include "seed7_05.s7i";

const func boolean: selfDescr (in string: stri) is func
  result
    var boolean: check is TRUE;
  local
    var integer: idx is 0;
    var array integer: count is [0 .. 9] times 0;
  begin
    for idx range 1 to length(stri) do
      incr(count[ord(stri[idx]) - ord('0')]);
    end for;
    idx := 1;
    while check and idx <= length(stri) do
      check := count[pred(idx)] = ord(stri[idx]) - ord('0');
      incr(idx);
    end while;
  end func;

const proc: gen (in integer: n) is func
  local
    var array integer : digits is 0 times 0;
    var string: stri is "";
    var integer: numberOfOneDigits is 0;
    var integer: idx is 0;
  begin
    while numberOfOneDigits <= 2 and numberOfOneDigits < n - 2 do
      digits := n times 0;
      digits[1] := n - 2 - numberOfOneDigits;
      if digits[1] <> 2 then
        digits[digits[1] + 1] := 1;
        digits[2] := 2;
        digits[3] := 1;
      else
        digits[2] := ord(numberOfOneDigits <> 0);
        digits[3] := 2;
      end if;
      stri := "";
      for idx range 1 to n do
        stri &:= chr(ord(digits[idx]) + ord('0'));
      end for;
      if selfDescr(stri) then
        writeln(stri);
      end if;
      incr(numberOfOneDigits);
    end while;
  end func;

const proc: main is func
  local
    const array integer: nums is [] (1210, 1337, 2020, 21200, 3211000, 42101000);
    var integer: number is 0;
  begin
    for number range nums do
      write(number <& " is ");
      if not selfDescr(str(number)) then
        write("not ");
      end if;
      writeln("self describing");
    end for;
    writeln;
    writeln("All autobiograph numbers:");
    for number range 1 to 10 do
      gen(number);
    end for;
  end func;
```


Output:

```txt

1210 is self describing
1337 is not self describing
2020 is self describing
21200 is self describing
3211000 is self describing
42101000 is self describing

All autobiograph numbers:
2020
1210
21200
3211000
42101000
521001000
6210001000

```



## Sidef

```ruby
func sdn(Number n) {
    var b = [0]*n.len
    var a = n.digits.flip
    a.each { |i| b[i] := 0 ++ }
    a == b
}

var values = [1210, 2020, 21200, 3211000,
42101000, 521001000, 6210001000, 27, 115508]

values.each { |test|
    say "#{test} is #{sdn(test) ? '' : 'NOT ' }a self describing number."
}

say "\nSelf-descriptive numbers less than 1e5 (in base 10):"
^1e5 -> each { |i| say i if sdn(i) }
```

```txt

1210 is a self describing number.
2020 is a self describing number.
21200 is a self describing number.
3211000 is a self describing number.
42101000 is a self describing number.
521001000 is a self describing number.
6210001000 is a self describing number.
27 is NOT a self describing number.
115508 is NOT a self describing number.

Self-descriptive numbers less than 1e5 (in base 10):
1210
2020
21200

```


'''Extra credit:''' this will generate all the self-describing numbers in bases 7 to 36:

```ruby
for b in (7 .. 36) {
    var n = ((b-4) * b**(b-1) + 2*(b**(b-2)) + b**(b-3) + b**3 -> base(b))
    say "base #{'%2d' % b}: #{n}"
}
```

```txt

base  7: 3211000
base  8: 42101000
base  9: 521001000
base 10: 6210001000
base 11: 72100001000
base 12: 821000001000
base 13: 9210000001000
base 14: a2100000001000
base 15: b21000000001000
base 16: c210000000001000
base 17: d2100000000001000
base 18: e21000000000001000
base 19: f210000000000001000
base 20: g2100000000000001000
base 21: h21000000000000001000
base 22: i210000000000000001000
base 23: j2100000000000000001000
base 24: k21000000000000000001000
base 25: l210000000000000000001000
base 26: m2100000000000000000001000
base 27: n21000000000000000000001000
base 28: o210000000000000000000001000
base 29: p2100000000000000000000001000
base 30: q21000000000000000000000001000
base 31: r210000000000000000000000001000
base 32: s2100000000000000000000000001000
base 33: t21000000000000000000000000001000
base 34: u210000000000000000000000000001000
base 35: v2100000000000000000000000000001000
base 36: w21000000000000000000000000000001000

```



## Tcl


```tcl
package require Tcl 8.5
proc isSelfDescribing num {
    set digits [split $num ""]
    set len [llength $digits]
    set count [lrepeat $len 0]
    foreach d $digits {
	if {$d >= $len} {return false}
	lset count $d [expr {[lindex $count $d] + 1}]
    }
    foreach d $digits c $count {if {$c != $d} {return false}}
    return true
}

for {set i 0} {$i < 100000000} {incr i} {
    if {[isSelfDescribing $i]} {puts $i}
}
```



## UNIX Shell

Seeking self-describing numbers up to 100,000,000 is very time consuming, so we'll just verify a few numbers.

```bash
selfdescribing() {
    local n=$1
    local count=()
    local i
    for ((i=0; i<${#n}; i++)); do
        ((count[${n:i:1}]++))
    done
    for ((i=0; i<${#n}; i++)); do
        (( ${n:i:1} == ${count[i]:-0} )) || return 1
    done
    return 0
}

for n in 0 1 10 11 1210 2020 21200 3211000 42101000; do
    if selfdescribing $n; then
        printf "%d\t%s\n" $n yes
    else
        printf "%d\t%s\n" $n no
    fi
done
```

```txt
0	no
1	no
10	no
11	no
1210	yes
2020	yes
21200	yes
3211000	yes
42101000	yes
```



## VBScript

Takes a very, very long time to check 100M numbers that I have to terminate the script.  But the function
works.

```vb

Function IsSelfDescribing(n)
	IsSelfDescribing = False
	Set digit = CreateObject("Scripting.Dictionary")
	For i = 1 To Len(n)
		k = Mid(n,i,1)
		If digit.Exists(k) Then
			digit.Item(k) = digit.Item(k) + 1
		Else
			digit.Add k,1
		End If
	Next
	c = 0
	For j = 0 To Len(n)-1
		l = Mid(n,j+1,1)
		If digit.Exists(CStr(j)) Then
			If digit.Item(CStr(j)) = CInt(l) Then
				c = c + 1
			End If
		ElseIf l = 0 Then
			c = c + 1
		Else
			Exit For
		End If
	Next
	If c = Len(n) Then
		IsSelfDescribing = True
	End If
End Function

'testing
start_time = Now
s = ""
For m = 1 To 100000000
	If 	IsSelfDescribing(m) Then
		WScript.StdOut.WriteLine m
	End If
Next
end_time = Now
WScript.StdOut.WriteLine "Elapse Time: " & DateDiff("s",start_time,end_time) & " seconds"

```



## XPL0


```XPL0
code ChOut=8, IntOut=11;

func SelfDesc(N);               \Returns 'true' if N is self-describing
int N;
int Len,        \length = number of digits in N
    I, D;
char Digit(10), Count(10);

        proc Num2Str(N);        \Convert integer N to string in Digit
        int N;
        int R;
        [N:= N/10;
        R:= rem(0);
        if N then Num2Str(N);
        Digit(Len):= R;
        Len:= Len+1;
        ];

[Len:= 0;
Num2Str(N);
for I:= 0 to Len-1 do Count(I):= 0;
for I:= 0 to Len-1 do
        [D:= Digit(I);
        if D >= Len then return false;
        Count(D):= Count(D)+1;
        ];
for I:= 0 to Len-1 do
        if Count(I) # Digit(I) then return false;
return true;
]; \SelfDesc


int N;
for N:= 0 to 100_000_000-1 do
        if SelfDesc(N) then [IntOut(0, N);  ChOut(0, ^ )]
```


Output:

```txt

1210 2020 21200 3211000 42101000

```



## Yabasic

```Yabasic
FOR N = 1 TO 5E7
    IF FNselfdescribing(N) PRINT N
NEXT


sub FNselfdescribing(N)
    LOCAL D(9), I, L, O

    O = N
    L = INT(LOG(N, 10))
    WHILE(N)
        I = MOD(N, 10)
        D(I) = D(I) + 10^(L-I)
        N = INT(N / 10)
    WEND

    L = 0
    FOR I = 0 TO 8 : L = L + D(I) : NEXT
    RETURN O = L
END SUB
```



## zkl


```zkl
fcn isSelfDescribing(n){
   if (n.bitAnd(1)) return(False); // Wikipedia: last digit must be zero
   nu:= n.toString();
   ns:=["0".."9"].pump(String,nu.inCommon,"len"); //"12233".inCommon("2")-->"22"
   (nu+"0000000000")[0,10] == ns;  //"2020","2020000000"
}
```

Since testing a humongous number of numbers is slow, chunk the task into a bunch of threads. Even so, it pegged my 8 way Ivy Bridge Linux box for quite some time (eg the Python & AWK solutions crush this one).

```zkl
//[1..0x4_000_000].filter(isSelfDescribing).println();
const N=0d500_000;
[1..0d100_000_000, N] // chunk and thread, 200 in this case
   .apply(fcn(n){ n.filter(N,isSelfDescribing) }.future)
   .filter().apply("noop").println();
```

A future is a thread returning a [delayed] result, future.filter/future.noop will block until the future coughs up the result. Since the results are really sparse for the bigger numbers, filter out the empty results.
```txt

L(L(1210,2020,21200),L(3211000),L(42101000))

```

