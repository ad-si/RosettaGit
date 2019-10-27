+++
title = "Count the coins"
description = ""
date = 2019-10-11T15:47:07Z
aliases = []
[extra]
id = 10731
[taxonomies]
categories = []
tags = []
+++

{{task}}

There are four types of common coins in   [https://en.wikipedia.org/wiki/United_States US]   currency: 
:::#   quarters   (25 cents)
:::#   dimes   (10 cents)
:::#   nickels   (5 cents),   and 
:::#   pennies   (1 cent)  


There are six ways to make change for 15 cents:
:::#   A dime and a nickel 
:::#   A dime and 5 pennies
:::#   3 nickels
:::#   2 nickels and 5 pennies
:::#   A nickel and 10 pennies
:::#   15 pennies



;Task:
How many ways are there to make change for a dollar using these common coins?     (1 dollar = 100 cents).


;Optional:
Less common are dollar coins (100 cents);   and very rare are half dollars (50 cents).   With the addition of these two coins, how many ways are there to make change for $1000? 

(Note:   the answer is larger than   2<sup>32</sup>).


;Reference:
*   [http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_Temp_52 an algorithm from MIT Press]. 





## 11l

{{trans|Python}}

```11l
F changes(amount, coins)
   V ways = [Int64(0)] * (amount + 1)
   ways[0] = 1
   L(coin) coins
      L(j) coin .. amount
         ways[j] += ways[j - coin]
   R ways[amount]

print(changes(100, [1, 5, 10, 25]))
print(changes(100000, [1, 5, 10, 25, 50, 100]))
```


Output:

```txt

242
13398445413854501

```



## 360 Assembly

{{trans|AWK}}

```360asm
*        count the coins           04/09/2015
COINS    CSECT
         USING  COINS,R12
         LR     R12,R15
         L      R8,AMOUNT          npenny=amount
         L      R4,AMOUNT
         SRDA   R4,32
         D      R4,=F'5'
         LR     R9,R5              nnickle=amount/5
         L      R4,AMOUNT
         SRDA   R4,32
         D      R4,=F'10'
         LR     R10,R5             ndime=amount/10
         L      R4,AMOUNT
         SRDA   R4,32
         D      R4,=F'25'
         LR     R11,R5             nquarter=amount/25
         SR     R1,R1              count=0
         SR     R4,R4              p=0
LOOPP    CR     R4,R8              do p=0 to npenny
         BH     ELOOPP
         SR     R5,R5              n=0
LOOPN    CR     R5,R9              do n=0 to nnickle
         BH     ELOOPN
         SR     R6,R6
LOOPD    CR     R6,R10             do d=0 to ndime
         BH     ELOOPD
         SR     R7,R7              q=0
LOOPQ    CR     R7,R11             do q=0 to nquarter
         BH     ELOOPQ
         LR     R3,R5              n
         MH     R3,=H'5'
         LR     R2,R4              p
         AR     R2,R3
         LR     R3,R6              d
         MH     R3,=H'10'
         AR     R2,R3
         LR     R3,R7              q
         MH     R3,=H'25'
         AR     R2,R3              s=p+n*5+d*10+q*25
         C      R2,=F'100'         if s=100
         BNE    NOTOK
         LA     R1,1(R1)           count=count+1
NOTOK    LA     R7,1(R7)           q=q+1
         B      LOOPQ
ELOOPQ   LA     R6,1(R6)           d=d+1
         B      LOOPD
ELOOPD   LA     R5,1(R5)           n=n+1
         B      LOOPN
ELOOPN   LA     R4,1(R4)           p=p+1
         B      LOOPP
ELOOPP   XDECO  R1,PG+0            edit count
         XPRNT  PG,12              print count
         XR     R15,R15
         BR     R14
AMOUNT   DC     F'100'             start value in cents
PG       DS     CL12
         YREGS
         END    COINS
```

{{out}}

```txt

         242

```



## Ada


{{Works with|gnat/gcc}}


```Ada
with Ada.Text_IO;

procedure Count_The_Coins is

   type Counter_Type is range 0 .. 2**63-1; -- works with gnat
   type Coin_List is array(Positive range <>) of Positive;

   function Count(Goal: Natural; Coins: Coin_List) return Counter_Type is
      Cnt: array(0 .. Goal) of Counter_Type := (0 => 1, others => 0);
      -- 0 => we already know one way to choose (no) coins that sum up to zero
      -- 1 .. Goal => we do not (yet) other ways to choose coins
   begin
      for C in Coins'Range loop
         for Amount in 1 .. Cnt'Last loop
            if Coins(C) <= Amount then
               Cnt(Amount) := Cnt(Amount) + Cnt(Amount-Coins(C));
               -- Amount-Coins(C) plus Coins(C) sums up to Amount;
            end if;
         end loop;
      end loop;
      return Cnt(Goal);
   end Count;

   procedure Print(C: Counter_Type) is
   begin
      Ada.Text_IO.Put_Line(Counter_Type'Image(C));
   end Print;

begin
   Print(Count(   1_00,          (25, 10, 5, 1)));
   Print(Count(1000_00, (100, 50, 25, 10, 5, 1)));
end Count_The_Coins;
```


Output:
```txt
 242
 13398445413854501
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.4.1}}
{{trans|Haskell}}
This corresponds to a "naive" Haskell version; to do the larger problem will require a better approach.


```Algol68

#
  Rosetta Code "Count the coins"
  This is a direct translation of a Haskell version, using an array rather than
  a list. LWB, UPB, and array slicing makes the mapping very simple:
 
  LWB > UPB     <=> []
  LWB = UPB     <=> [x]
  a[LWB a]      <=> head xs
  a[LWB a + 1:] <=> tail xs
#

BEGIN
  PROC ways to make change = ([] INT denoms, INT amount) INT :
  BEGIN
    IF amount = 0 THEN
      1
    ELIF LWB denoms > UPB denoms THEN
      0
    ELIF LWB denoms = UPB denoms THEN
      (amount MOD denoms[LWB denoms] = 0 | 1 | 0)
    ELSE
      INT sum := 0;
      FOR i FROM 0 BY denoms[LWB denoms] TO amount DO
        sum +:= ways to make change(denoms[LWB denoms + 1:], amount - i)
      OD;
      sum
    FI
  END;
  [] INT denoms = (25, 10, 5, 1);
  print((ways to make change(denoms, 100), newline))
END 

```

Output:
```txt

       +242

```



## Arturo

{{trans|Python}}

```arturo
changes [amount,coins]{
	ways #(0)*(amount+1)
	ways.0 1

	loop coins [coin]{
		loop $(range coin amount) [j]{
			ways.[j] ways.[j]+ways.[j-coin]
		}
	}
	return ways.[amount]
}

print $(changes 100 #(1 5 10 25))
print $(changes 100000 #(1 5 10 25 50 100))
```


{{out}}


```txt
242
13398445413854501

```



## AutoHotkey

{{trans|Go}}
{{Works with|AutoHotkey_L}}

```AHK
countChange(amount){
	return cc(amount, 4)
}

cc(amount, kindsOfCoins){
	if ( amount == 0 )
		return 1
	if ( amount < 0 ) || ( kindsOfCoins == 0 )
		return 0
	return cc(amount, kindsOfCoins-1)
	    +  cc(amount - firstDenomination(kindsOfCoins), kindsOfCoins)
}

firstDenomination(kindsOfCoins){
	return [1, 5, 10, 25][kindsOfCoins]
}
MsgBox % countChange(100)
```



## AWK


Iterative implementation, derived from Run BASIC:


```awk
#!/usr/bin/awk -f

BEGIN {
    print cc(100)
    exit
}

function cc(amount, coins,    numPennies, numNickles, numQuarters, p, n, d, q, s, count) {
    numPennies = amount
    numNickles = int(amount / 5)
    numDimes = int(amount / 10)
    numQuarters = int(amount / 25)
    
    count = 0
    for (p = 0; p <= numPennies; p++) {
        for (n = 0; n <= numNickles; n++) {
            for (d = 0; d <= numDimes; d++) {
                for (q = 0; q <= numQuarters; q++) {
                    s = p + n * 5 + d * 10 + q * 25;
                    if (s == 100) count++;
                }
            }
        }
    }
    return count;
}

```


Run time:
 time ./change-itr.awk
 242
 
 real	0m0.065s
 user	0m0.063s
 sys	0m0.002s

Recursive implementation (derived from Scheme example):


```awk
#!/usr/bin/awk -f

BEGIN {
    COINSEP = ", "
    coins = 1 COINSEP 5 COINSEP 10 COINSEP 25
    print cc(100, coins)
    exit
}

function cc(amt, coins) {
    if (length(coins) == 0) return 0
    if (amt < 0) return 0
    if (amt == 0) return 1
    return cc(amt, tail(coins)) + cc(amt - head(coins), coins)
}

function tail(coins,    koins, s, c) {
    split(coins, koins, COINSEP)
    s = ""
    for (c = 2; c <= length(koins); c++) s = s (s == "" ? "" : COINSEP) koins[c]
    return s;
}

function head(coins,    koins) {
    split(coins, koins, COINSEP)
    return koins[1]
}

```


Run time:
 time ./change-rec.awk 
 242
 
 real	0m0.081s 
 user	0m0.079s
 sys	0m0.002s

While the recursive version is slower for small amounts, about 2 bucks it gets faster than the iterative version, at least until is segfaults from exhausting the stack.


## BBC BASIC

Non-recursive solution:

```bbcbasic
      DIM uscoins%(3)
      uscoins%() = 1, 5, 10, 25
      PRINT FNchange(100, uscoins%()) " ways of making $1"
      PRINT FNchange(1000, uscoins%()) " ways of making $10"
      
      DIM ukcoins%(7)
      ukcoins%() = 1, 2, 5, 10, 20, 50, 100, 200
      PRINT FNchange(100, ukcoins%()) " ways of making £1"
      PRINT FNchange(1000, ukcoins%()) " ways of making £10"
      END
      
      DEF FNchange(sum%, coins%())
      LOCAL C%, D%, I%, N%, P%, Q%, S%, table()
      C% = 0
      N% = DIM(coins%(),1) + 1
      FOR I% = 0 TO N% - 1
        D% = coins%(I%)
        IF D% <= sum% IF D% >= C% C% = D% + 1
      NEXT
      C% *= N%
      DIM table(C%-1)
      FOR I% = 0 TO N%-1 : table(I%) = 1 : NEXT
      
      P% = N%
      FOR S% = 1 TO sum%
        FOR I% = 0 TO N% - 1
          IF I% = 0 IF P% >= C% P% = 0
          IF coins%(I%) <= S% THEN
            Q% = P% - coins%(I%) * N%
            IF Q% >= 0 table(P%) = table(Q%) ELSE table(P%) = table(Q% + C%)
          ENDIF
          IF I% table(P%) += table(P% - 1)
          P% += 1
        NEXT
      NEXT
      = table(P%-1)

```

Output (BBC BASIC does not have large enough integers for the optional task):

```txt
       242 ways of making $1
    142511 ways of making $10
      4563 ways of making £1
 321335886 ways of making £10
```



## C

Using some crude 128-bit integer type.

```c>#include <stdio.h

#include <stdlib.h>
#include <stdint.h>

// ad hoc 128 bit integer type; faster than using GMP because of low
// overhead
typedef struct { uint64_t x[2]; } i128;

// display in decimal
void show(i128 v) {
	uint32_t x[4] = {v.x[0], v.x[0] >> 32, v.x[1], v.x[1] >> 32};
	int i, j = 0, len = 4;
	char buf[100];
	do {
		uint64_t c = 0;
		for (i = len; i--; ) {
			c = (c << 32) + x[i];
			x[i] = c / 10, c %= 10;
		}

		buf[j++] = c + '0';
		for (len = 4; !x[len - 1]; len--);
	} while (len);

	while (j--) putchar(buf[j]);
	putchar('\n');
}

i128 count(int sum, int *coins)
{
	int n, i, k;
	for (n = 0; coins[n]; n++);

	i128 **v = malloc(sizeof(int*) * n);
	int *idx = malloc(sizeof(int) * n);

	for (i = 0; i < n; i++) {
		idx[i] = coins[i];
		// each v[i] is a cyclic buffer
		v[i] = calloc(sizeof(i128), coins[i]);
	}

	v[0][coins[0] - 1] = (i128) {{1, 0}};

	for (k = 0; k <= sum; k++) {
		for (i = 0; i < n; i++)
			if (!idx[i]--) idx[i] = coins[i] - 1;

		i128 c = v[0][ idx[0] ];

		for (i = 1; i < n; i++) {
			i128 *p = v[i] + idx[i];

			// 128 bit addition
			p->x[0] += c.x[0];
			p->x[1] += c.x[1];
			if (p->x[0] < c.x[0]) // carry
				p->x[1] ++;
			c = *p;
		}
	}

	i128 r = v[n - 1][idx[n-1]];

	for (i = 0; i < n; i++) free(v[i]);
	free(v);
	free(idx);

	return r;
}

// simple recursive method; slow
int count2(int sum, int *coins)
{
	if (!*coins || sum < 0) return 0;
	if (!sum) return 1;
	return count2(sum - *coins, coins) + count2(sum, coins + 1);
}

int main(void)
{
	int us_coins[] = { 100, 50, 25, 10, 5, 1, 0 };
	int eu_coins[] = { 200, 100, 50, 20, 10, 5, 2, 1, 0 };

	show(count(   100, us_coins + 2));
	show(count(  1000, us_coins));

	show(count(  1000 * 100, us_coins));
	show(count( 10000 * 100, us_coins));
	show(count(100000 * 100, us_coins));

	putchar('\n');

	show(count(     1 * 100, eu_coins));
	show(count(  1000 * 100, eu_coins));
	show(count( 10000 * 100, eu_coins));
	show(count(100000 * 100, eu_coins));

	return 0;
}
```
output (only the first two lines are required by task):<lang>242
13398445413854501
1333983445341383545001
133339833445334138335450001

4563
10056050940818192726001
99341140660285639188927260001
992198221207406412424859964272600001
```



## C++


```cpp

#include <iostream>
#include <stack>
#include <vector>

struct DataFrame {
  int sum;
  std::vector<int> coins;
  std::vector<int> avail_coins;
};

int main() {
  std::stack<DataFrame> s;
  s.push({ 100, {}, { 25, 10, 5, 1 } });
  int ways = 0;
  while (!s.empty()) {
    DataFrame top = s.top();
    s.pop();
    if (top.sum < 0) continue;
    if (top.sum == 0) {
      ++ways;
      continue;
    }
    if (top.avail_coins.empty()) continue;
    DataFrame d = top;
    d.sum -= top.avail_coins[0];
    d.coins.push_back(top.avail_coins[0]);
    s.push(d);
    d = top;
    d.avail_coins.erase(std::begin(d.avail_coins));
    s.push(d);
  }
  std::cout << ways << std::endl;
  return 0;
}
```


{{out}}

```txt
242
```



## C sharp


```csharp

    // Adapted from http://www.geeksforgeeks.org/dynamic-programming-set-7-coin-change/
    class Program
    {
        static long Count(int[] C, int m, int n)
        {
            var table = new long[n + 1];
            table[0] = 1;
            for (int i = 0; i < m; i++)
                for (int j = C[i]; j <= n; j++)
                    table[j] += table[j - C[i]];
            return table[n];
        }
        static void Main(string[] args)
        {
            var C = new int[] { 1, 5, 10, 25 };
            int m = C.Length;
            int n = 100;
            Console.WriteLine(Count(C, m, n));  //242
            Console.ReadLine();
        }
    }

```



## Clojure


```lisp
(def denomination-kind [1 5 10 25])

(defn- cc [amount denominations]
  (cond (= amount 0) 1
        (or (< amount 0) (empty? denominations)) 0
        :else (+ (cc amount (rest denominations))
                 (cc (- amount (first denominations)) denominations))))

(defn count-change
  "Calculates the number of times you can give change with the given denominations."
  [amount denominations]
  (cc amount denominations))

(count-change 15 denomination-kind) ; = 6 
```



## COBOL

{{trans|C#}}

```cobol

       identification division.
       program-id. CountCoins.

       data division.
       working-storage section.
       77  i                      pic 9(3).
       77  j                      pic 9(3).  
       77  m                      pic 9(3) value 4.
       77  n                      pic 9(3) value 100.  
       77  edited-value           pic z(18).
       01  coins-table            value "01051025".
           05 coin                pic 9(2) occurs 4.
       01  ways-table.
           05 way                 pic 9(18) occurs 100.

       procedure division.
       main.
           perform calc-count
           move way(n) to edited-value
           display function trim(edited-value)
           stop run
           .
       calc-count.
           initialize ways-table
           move 1 to way(1)
           perform varying i from 1 by 1 until i > m
              perform varying j from coin(i) by 1 until j > n
                 add way(j - coin(i)) to way(j)
              end-perform
           end-perform
           .           

```

{{out}}

```txt
242
```



## Coco


{{trans|Python}}


```coco
changes = (amount, coins) ->
    ways = [1].concat [0] * amount
    for coin of coins
        for j from coin to amount
            ways[j] += ways[j - coin]
    ways[amount]
 
console.log changes 100, [1 5 10 25]
```



## Commodore BASIC


'''Example 1:''' Base example in Commodore BASIC (works on PET, C64, VIC20, etc.)

This example is based on the Spectrum ZX BASIC example found below. Direct copy of that algorithm and executed on an emulated Commodore 64 in VICE resulted in a timed performance of 46 minutes and 37 seconds (46:37) as measured by the C64 BASIC system clock (TIME$ or TI$, times are approximate within a few seconds). Some improvements were made as follows:

# Reversed the order of the loops to start counting with the largest denomination > smallest denomination. Result: 44:45
# It makes no sense to check with anything other than a multiple of 5 pennies, since the other denominations value a multiple of 5. Adding "step 5" to the penny for loop skips over a good portion of useless iteration. Result: about 9:44.
# Not printing any of the individual results speeds up total time to 9:30.
# Removing the specific variables used in the NEXT statements helps the interpreter speed up. Result: 9:10.
# Now that the denominations were reordered, it makes sense that each sub-loop with the next lower denomination should loop only through the remaining money not accounted for by the larger denomination. Result: 2:12. 



```gwbasic
5 m=100:rem money = $1.00 or 100 pennies.
10 print chr$(147);chr$(14);"This program will calculate the number"
11 print "of combinations of 'change' that can be"
12 print "given for a $1 bill."
13 print:print "The coin values are:"
14 print "0.01 = Penny":print "0.05 = Nickle"
15 print "0.10 = Dime":print "0.25 = Quarter"
16 print
20 print "Would you like to see each combination?"
25 get k$:yn=(k$="y"):if k$="" then 25
100 p=m:ti$="000000"
130 q=int(m/25)
140 count=0:ps=1
147 if yn then print "Count  P    N    D    Q"
150 for qc=0 to q:d=int((m-qc*25)/10)
160 for dc=0 to d:n=int((m-dc*10)/5)
170 for nc=0 to n:p=m-nc*5
180 for pc=0 to p step 5
190 s=pc+nc*5+dc*10+qc*25
200 if s=m then count=count+1:if yn then gosub 1000
210 next:next:next:next
245 en$=ti$
250 print:print count;"different combinations found in"
260 print tab(len(str$(count))+1);
265 print left$(en$,2);":";mid$(en$,3,2);":";right$(en$,2);"."
270 end
1000 print count;tab(6);pc;tab(11);nc;tab(16);dc;tab(21);qc:return
```


'''Example 2:''' Commodore 64 with Screen Blanking

Make the following changes on a Commodore 64 to enable screen blanking. This will give the CPU a few extra cycles normally held by the VIC-II. Add line 145 and change line 245 as shown.

Enabling screen blanking (and therefore not printing each result) results in a total time of 1:44.


```gwbasic
145 if not yn then poke 53265,peek(53265) and 239
245 en$=ti$:if not yn then poke 53265,peek(53265) or 16
```


'''Example 3:''' Commodore 128 with VIC-II blanking, 2MHz fast mode.

Similar to above, however the Commodore 128 is capable of using a faster clock speed at the expense of any VIC-II graphics display. Timed result is 1:18. Add/change the following lines on the Commodore 128:


```gwbasic
145 if not yn then fast
245 en$=ti$:if not yn then slow
```



## Common Lisp


### Recursive Version With Cache


```lisp
(defun count-change (amount coins
                    &optional
                    (length (1- (length coins)))
                    (cache  (make-array (list (1+ amount) (length coins))
                                        :initial-element nil)))
  (cond ((< length 0) 0)
        ((< amount 0) 0)
        ((= amount 0) 1)
        (t (or (aref cache amount length)
               (setf (aref cache amount length)
                     (+ (count-change (- amount (first coins)) coins length cache)
                        (count-change amount (rest coins) (1- length) cache)))))))

; (compile 'count-change) ; for CLISP

(print (count-change 100 '(25 10 5 1)))		   ; = 242
(print (count-change 100000 '(100 50 25 10 5 1)))  ; = 13398445413854501
(terpri)
```



### Iterative Version


```lisp
(defun count-change (amount coins &aux (ways (make-array (1+ amount) :initial-element 0)))
  (setf (aref ways 0) 1)
  (loop for coin in coins do
        (loop for j from coin upto amount
              do (incf (aref ways j) (aref ways (- j coin)))))
  (aref ways amount))
```



## D


### Basic Version

{{trans|Go}}

```d
import std.stdio, std.bigint;

auto changes(int amount, int[] coins) {
    auto ways = new BigInt[amount + 1];
    ways[0] = 1;
    foreach (coin; coins)
        foreach (j; coin .. amount + 1)
            ways[j] += ways[j - coin];
    return ways[$ - 1];
}

void main() {
    changes(   1_00, [25, 10, 5, 1]).writeln;
    changes(1000_00, [100, 50, 25, 10, 5, 1]).writeln;
}
```

{{out}}

```txt
242
13398445413854501
```



### Safe Ulong Version

This version is very similar to the precedent, but it uses a faster ulong type, and performs a checked sum to detect overflows at run-time.

```d
import std.stdio, core.checkedint;

auto changes(int amount, int[] coins, ref bool overflow) {
    auto ways = new ulong[amount + 1];
    ways[0] = 1;
    foreach (coin; coins)
        foreach (j; coin .. amount + 1)
            ways[j] = ways[j].addu(ways[j - coin], overflow);
    return ways[amount];
}

void main() {
    bool overflow = false;
    changes(    1_00, [25, 10, 5, 1], overflow).writeln;
    if (overflow)
        "Overflow".puts;
    overflow = false;
    changes( 1000_00, [100, 50, 25, 10, 5, 1], overflow).writeln;
    if (overflow)
        "Overflow".puts;
}
```

The output is the same.


### Faster Version

{{trans|C}}

```d
import std.stdio, std.bigint;

BigInt countChanges(in int amount, in int[] coins) pure /*nothrow*/ {
    immutable n = coins.length;
    int cycle;
    foreach (immutable c; coins)
        if (c <= amount && c >= cycle)
            cycle = c + 1;
    cycle *= n;
    auto table = new BigInt[cycle];
    table[0 .. n] = 1.BigInt;

    int pos = n;
    foreach (immutable s; 1 .. amount + 1) {
        foreach (immutable i; 0 .. n) {
            if (i == 0 && pos >= cycle)
                pos = 0;
            if (coins[i] <= s) {
                immutable int q = pos - (coins[i] * n);
                table[pos] = (q >= 0) ? table[q] : table[q + cycle];
            }
            if (i)
                table[pos] += table[pos - 1];
            pos++;
        }
    }

    return table[pos - 1];
}

void main() {
    immutable usCoins = [100, 50, 25, 10, 5, 1];
    immutable euCoins = [200, 100, 50, 20, 10, 5, 2, 1];

    foreach (immutable coins; [usCoins, euCoins]) {
        countChanges(     1_00, coins[2 .. $]).writeln;
        countChanges(  1000_00, coins).writeln;
        countChanges( 10000_00, coins).writeln;
        countChanges(100000_00, coins).writeln;
        writeln;
    }
}
```

{{out}}

```txt
242
13398445413854501
1333983445341383545001
133339833445334138335450001

4562
10056050940818192726001
99341140660285639188927260001
992198221207406412424859964272600001
```


===128-bit Version===
A much faster version that mixes high-level and low-level style programming. This version uses basic 128-bit unsigned integers, like the C version. The output is the same as the second D version.
{{trans|C}}

```d
import std.stdio, std.bigint, std.algorithm, std.conv, std.functional;

struct Ucent { /// Simplified 128-bit integer (like ucent).
    ulong hi, lo;
    static immutable one = Ucent(0, 1);

    void opOpAssign(string op="+")(in ref Ucent y) pure nothrow @nogc @safe {
        this.hi += y.hi;
        if (this.lo >= ~y.lo)
            this.hi++;
        this.lo += y.lo;
    }

    string toString() const /*pure nothrow @safe*/ {
        return text((this.hi.BigInt << 64) + this.lo);
    }
}

Ucent countChanges(in int amount, in int[] coins) pure nothrow {
    immutable n = coins.length;

    // Points to a cyclic buffer of length coins[i]
    auto p = new Ucent*[n];
    auto q = new Ucent*[n]; // iterates it.
    auto buf = new Ucent[coins.sum];

    p[0] = buf.ptr;
    foreach (immutable i; 0 .. n) {
        if (i)
            p[i] = coins[i - 1] + p[i - 1];
        *p[i] = Ucent.one;
        q[i] = p[i];
    }

    Ucent prev;
    foreach (immutable j; 1 .. amount + 1)
        foreach (immutable i; 0 .. n) {
            q[i]--;
            if (q[i] < p[i])
                q[i] = p[i] + coins[i] - 1;
            if (i)
                *q[i] += prev;
            prev = *q[i];
        }

    return prev;
}

void main() {
    immutable usCoins = [100, 50, 25, 10, 5, 1];
    immutable euCoins = [200, 100, 50, 20, 10, 5, 2, 1];

    foreach (immutable coins; [usCoins, euCoins]) {
        countChanges(     1_00, coins[2 .. $]).writeln;
        countChanges(  1000_00, coins).writeln;
        countChanges( 10000_00, coins).writeln;
        countChanges(100000_00, coins).writeln;
        writeln;
    }
}
```



### Printing Version

This version prints all the solutions (so it can be used on the smaller input):

```d
import std.stdio, std.conv, std.string, std.algorithm, std.range;

void printChange(in uint tot, in uint[] coins)
in {
    assert(coins.isSorted);
} body {
    auto freqs = new uint[coins.length];

    void inner(in uint curTot, in size_t start) {
        if (curTot == tot)
            return writefln("%-(%s %)",
                            zip(coins, freqs)
                            .filter!(cf => cf[1] != 0)
                            .map!(cf => format("%u:%u", cf[])));

        foreach (immutable i; start .. coins.length) {
            immutable ci = coins[i];
            for (auto v = (freqs[i] + 1) * ci; v <= tot; v += ci)
                if (curTot + v <= tot) {
                    freqs[i] += v / ci;
                    inner(curTot + v, i + 1);
                    freqs[i] -= v / ci;
                }
        }
    }

    inner(0, 0);
}

void main() {
    printChange(1_00, [1, 5, 10, 25]);
}
```

{{out}}

```txt
1:5 5:1 10:4 25:2
1:5 5:1 10:9
1:5 5:2 10:1 25:3
1:5 5:2 10:6 25:1
1:5 5:3 10:3 25:2
1:5 5:3 10:8
1:5 5:4 10:5 25:1
1:5 5:4 25:3
1:5 5:5 10:2 25:2
1:5 5:5 10:7
1:5 5:6 10:4 25:1
1:5 5:7 10:1 25:2
...
5:11 10:2 25:1
5:12 10:4
5:13 10:1 25:1
5:14 10:3
5:15 25:1
5:16 10:2
5:18 10:1
5:20
10:5 25:2
10:10
25:4

```





## Dart

Simple recursive version plus cached version using a map.  

```Dart

var cache = new Map();

main() {
    var stopwatch = new Stopwatch()..start();

    // use the brute-force recursion for the small problem
    int amount = 100;
    list coinTypes = [25,10,5,1];
    print (coins(amount,coinTypes).toString() + " ways for $amount using $coinTypes coins.");

    // use the cache version for the big problem
    amount = 100000;
    coinTypes = [100,50,25,10,5,1];
    print (cachedCoins(amount,coinTypes).toString() + " ways for $amount using $coinTypes coins.");

    stopwatch.stop();
    print ("... completed in " + (stopwatch.elapsedMilliseconds/1000).toString() + " seconds");
}


coins(int amount, list coinTypes) {
    int count = 0;

    if(coinTypes.length == 1) return (1);   // just pennies available, so only one way to make change

    for(int i=0; i<=(amount/coinTypes[0]).toInt(); i++){                // brute force recursion
      count += coins(amount-(i*coinTypes[0]),coinTypes.sublist(1));     // sublist(1) is like lisp's '(rest ...)'
    }

    // uncomment if you want to see intermediate steps
    //print("there are " + count.toString() +" ways to count change for ${amount.toString()} using ${coinTypes} coins.");
    return(count);
  }


  cachedCoins(int amount, list coinTypes) {
      int count = 0;

      // this is more efficient, looks at last two coins.  but not fast enough for the optional exercise.
      if(coinTypes.length == 2) return ((amount/coinTypes[0]).toInt() + 1);

      var key = "$amount.$coinTypes";         // lookes like "100.[25,10,5,1]"
      var cacheValue = cache[key];            // check whether we have seen this before

      if(cacheValue != null) return(cacheValue);

      count = 0;
      // same recursion as simple method, but caches all subqueries too
      for(int i=0; i<=(amount/coinTypes[0]).toInt(); i++){
        count += cachedCoins(amount-(i*coinTypes[0]),coinTypes.sublist(1));     // sublist(1) is like lisp's '(rest ...)'
      }

      cache[key] = count;                     // add this to the cache
      return(count);
    }

```

{{out}}

```txt

242 ways for 100 using [25, 10, 5, 1] coins.
13398445413854501 ways for 100000 using [100, 50, 25, 10, 5, 1] coins.
... completed in 3.604 seconds

```





## Dyalect



```dyalect
func countCoins(coins, n) {
    var xs = Array.empty(n + 1, 0)
    xs[0] = 1
    for c in coins {
        var cj = c
        while cj <= n {
            xs[cj] += xs[cj - c]
            cj += 1
        }
    }
    return xs[n]
}

var coins = [1, 5, 10, 25]
print(countCoins(coins, 100))
```


{{out}}


```txt
242
```



## EchoLisp

Recursive solution using memoization, adapted from CommonLisp and Racket.

```scheme

(lib 'compile) ;; for (compile)
(lib 'bigint)  ;; integer results > 32 bits
(lib 'hash)    ;; hash table

;; h-table
(define Hcoins (make-hash))

;; the function to memoize
(define (sumways cents coins)
	(+ (ways cents (cdr coins)) (ways (- cents (car coins)) coins)))
	
;; accelerator : ways (cents, coins) = ways ((cents  - cents % 5) , coins)
(define (ways cents coins)
  (cond ((null? coins) 0)
        ((negative? cents) 0)
        ((zero? cents) 1)
        ((eq? coins c-1) 1) ;; if coins = (1) --> 1
        (else (hash-ref! Hcoins (list (- cents (modulo cents 5)) coins) sumways))))

(compile 'ways) ;; speed-up things

```

{{out}}

```scheme

(define change '(25 10 5 1))
(define c-1 (list-tail change -1)) ;; pointer to (1)
(ways 100 change)
    → 242

(define change '(100 50 25 10 5 1))
(define c-1 (list-tail change -1))
(for ((i (in-range 0 200001 20000))) 
    (writeln i (time (ways i change)) (hash-count Hcoins)))


;; iterate cents = 20000, 40000, ..
;; cents ((time (msec) number-of-ways) number-of-entries-in-h-table

20000      (350 4371565890901)         9398    
40000      (245 138204514221801)       18798    
60000      (230 1045248220992701)      28198    
80000      (255 4395748062203601)      37598    
100000     (234 13398445413854501)     46998    
120000     (230 33312577651945401)     56398    
140000     (292 71959878152476301)     65798    
160000     (736 140236576291447201)     75198    
180000     (237 252625397444858101)     84598    
200000     (240 427707562988709001)     93998    

;; One can see that the time is linear, and the h-table size reasonably small

change 
    → (100 50 25 10 5 1)
(ways 100000 change)
    → 13398445413854501 


```



## Elixir

Recursive Dynamic Programming solution in Elixir

```Elixir
defmodule Coins do
  def find(coins,lim) do
    vals = Map.new(0..lim,&{&1,0}) |> Map.put(0,1)
    count(coins,lim,vals)
      |> Map.values
      |> Enum.max
      |> IO.inspect
  end
  
  defp count([],_,vals), do: vals
  defp count([coin|coins],lim,vals) do
    count(coins,lim,ways(coin,coin,lim,vals))
  end
  
  defp ways(num,_coin,lim,vals) when num > lim, do: vals
  defp ways(num, coin,lim,vals) do
    ways(num+1,coin,lim,ad(coin,num,vals))
  end
  
  defp ad(a,b,c), do: Map.put(c,b,c[b]+c[b-a])
end

Coins.find([1,5,10,25],100)
Coins.find([1,5,10,25,50,100],100_000)
```


{{out}}

```txt

242
13398445413854501

```



## Erlang


```erlang

-module(coins).
-compile(export_all).

count(Amount, Coins) ->
    {N,_C} = count(Amount, Coins, dict:new()),
    N.

count(0,_,Cache) ->
    {1,Cache};
count(N,_,Cache) when N < 0 ->
    {0,Cache};
count(_N,[],Cache) ->
    {0,Cache};
count(N,[C|Cs]=Coins,Cache) ->
    case dict:is_key({N,length(Coins)},Cache) of
        true -> 
            {dict:fetch({N,length(Coins)},Cache), Cache};
        false ->
            {N1,C1} = count(N-C,Coins,Cache),
            {N2,C2} = count(N,Cs,C1),
            {N1+N2,dict:store({N,length(Coins)},N1+N2,C2)}
    end.

print(Amount, Coins) ->
    io:format("~b ways to make change for ~b cents with ~p coins~n",[count(Amount,Coins),Amount,Coins]).

test() ->
    A1 = 100, C1 = [25,10,5,1],
    print(A1,C1),
    A2 = 100000, C2 = [100, 50, 25, 10, 5, 1],
    print(A2,C2).

```


{{out}}
 42> coins:test().
 242 ways to make change for 100 cents with [25,10,5,1] coins
 13398445413854501 ways to make change for 100000 cents with [100,50,25,10,5,1] coins
 ok

=={{header|F_Sharp|F#}}==
{{trans|OCaml}}
<p>Forward iteration, which can also be seen in Scala.</p>

```fsharp
let changes amount coins =
    let ways = Array.zeroCreate (amount + 1)
    ways.[0] <- 1L
    List.iter (fun coin ->
        for j = coin to amount do ways.[j] <- ways.[j] + ways.[j - coin]
    ) coins
    ways.[amount]
 
[<EntryPoint>]
let main argv = 
    printfn "%d" (changes    100 [25; 10; 5; 1]);
    printfn "%d" (changes 100000 [100; 50; 25; 10; 5; 1]);
    0
```

{{out}}

```txt
242
13398445413854501
```



## Factor


```factor
USING: combinators kernel locals math math.ranges sequences sets sorting ;
IN: rosetta.coins

<PRIVATE
! recursive-count uses memoization and local variables.
! coins must be a sequence.
MEMO:: recursive-count ( cents coins -- ways )
    coins length :> types
    {
        ! End condition: 1 way to make 0 cents.
        { [ cents zero? ] [ 1 ] }
        ! End condition: 0 ways to make money without any coins.
        { [ types zero? ] [ 0 ] }
        ! Optimization: At most 1 way to use 1 type of coin.
        { [ types 1 number= ] [
            cents coins first mod zero? [ 1 ] [ 0 ] if
        ] }
        ! Find all ways to use the first type of coin.
        [
            ! f = first type, r = other types of coins.
            coins unclip-slice :> f :> r
            ! Loop for 0, f, 2*f, 3*f, ..., cents.
            0 cents f <range> [
                ! Recursively count how many ways to make remaining cents
                ! with other types of coins.
                cents swap - r recursive-count
            ] [ + ] map-reduce          ! Sum the counts.
        ]
    } cond ;
PRIVATE>

! How many ways can we make the given amount of cents
! with the given set of coins?
: make-change ( cents coins -- ways )
    members [ ] inv-sort-with   ! Sort coins in descending order.
    recursive-count ;
```


From the listener:

 '''USE: rosetta.coins'''
 ( scratchpad ) '''100 { 25 10 5 1 } make-change .'''
 242
 ( scratchpad ) '''100000 { 100 50 25 10 5 1 } make-change .'''
 13398445413854501

This algorithm is '''slow'''. A test machine needed '''1 minute''' to run ''100000 { 100 50 25 10 5 1 } make-change .'' and get 13398445413854501. The same machine needed less than 1 second to run the Common Lisp ([[SBCL]]), Ruby ([[MRI]]) or Tcl ([[tclsh]]) programs and get the same answer.

One might make use of the rosetta-code.count-the-coins vocabulary as shown:
<lang>
IN: scratchpad [ 100000 { 1 5 10 25 50 100 } make-change . ] time
13398445413854501
Running time: 0.020869274 seconds

```

For reference, the implementation is shown next.
<lang>
USING: arrays locals math math.ranges sequences sets sorting ;
IN: rosetta-code.count-the-coins

<PRIVATE

:: (make-change) ( cents coins -- ways )
    cents 1 + 0 <array> :> ways
    1 ways set-first
    coins [| coin |
        coin cents [a,b] [| j |
            j coin - ways nth j ways [ + ] change-nth
        ] each
    ] each ways last ;

PRIVATE>

! How many ways can we make the given amount of cents
! with the given set of coins?
: make-change ( cents coins -- ways )
    members [ ] inv-sort-with (make-change) ;

```

Or one could implement the algorithm like described in http://www.cdn.geeksforgeeks.org/dynamic-programming-set-7-coin-change.

```factor

USE: math.ranges 

:: exchange-count ( seq val -- cnt )
  val 1 + 0 <array> :> tab
  0 :> old!
  1 0 tab set-nth
  seq length iota [
    seq nth old!
    old val [a,b] [| j |
      j old - tab nth
      j tab nth + 
      j tab set-nth
    ] each
  ] each
  val tab nth
;

[ { 1 5 10 25 50 100 } 100000 exchange-count . ] time
13398445413854501
Running time: 0.029163549 seconds

```



## Forth


```forth
\ counting change (SICP section 1.2.2)

: table create does> swap cells + @ ;
table coin-value 0 , 1 , 5 , 10 , 25 , 50 ,

: count-change ( total coin -- n )
  over 0= if
    2drop 1
  else over 0< over 0= or if
    2drop 0
  else
    2dup coin-value - over recurse
    >r 1- recurse r> +
  then then ;

100 5 count-change .
```


## FreeBASIC

Translation from "Dynamic Programming Solution: Python version" on this webside [http://www.geeksforgeeks.org/dynamic-programming-set-7-coin-change/]

```freebasic
' version 09-10-2016
' compile with: fbc -s console


Function count(S() As UInteger, n As UInteger) As ULongInt

  Dim As Integer i, j
  ' calculate m from array S()
  Dim As UInteger m = UBound(S) - LBound(S) +1
  Dim As ULongInt x, y

  '' We need n+1 rows as the table is consturcted in bottom up manner using
  '' the base case 0 value case (n = 0)
  Dim As ULongInt table(n +1, m)

  '' Fill the enteries for 0 value case (n = 0)
  For i = 0 To m -1
    table(0, i) = 1
  Next

  '' Fill rest of the table enteries in bottom up manner
  For i = 1 To n
    For j = 0 To m -1
      '' Count of solutions including S[j]
      x = IIf (i >= S(j), table(i - S(j), j), 0)
      '' Count of solutions excluding S[j]
      y = IIf (j >= 1, table(i, j -1), 0)
      ''total count
      table(i, j) = x + y
    Next
  Next

  Return table(n, m -1)

End Function

' ------=< MAIN >=------

Dim As UInteger n
Dim As UInteger value()

ReDim value(3)
value(0) = 1 : value(1) = 5 : value(2) = 10 : value(3) = 25

n = 100
print
Print " There are "; count(value(), n); " ways to make change for $";n/100;" with 4 coins"
Print

n = 100000
Print " There are "; count(value(), n); " ways to make change for $";n/100;" with 4 coins"
Print

ReDim value(5)
value(0) =  1 : value(1) =  5 : value(2) =  10
value(3) = 25 : value(4) = 50 : value(5) = 100

n = 100000
Print " There are "; count(value(), n); " ways to make change for $";n/100;" with 6 coins"
Print

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt

 There are 242 ways to make change for $ 1 with 4 coins

 There are 133423351001 ways to make change for $ 1000 with 4 coins

 There are 13398445413854501 ways to make change for $ 1000 with 6 coins
```



## FutureBasic


```futurebasic

include "ConsoleWindow"

dim as long penny, nickel, dime, quarter , count

penny = 1 : nickel = 1
dime = 1  : quarter = 1

for penny = 0 to 100
   for nickel = 0 to 20
      for dime = 0 to 10
         for quarter = 0 to 4
            if penny + nickel * 5 + dime * 10 + quarter * 25 == 100
               print penny; " pennies "; nickel;" nickels "; dime; " dimes "; quarter; " quarters"
               count++ 
            end if
         next quarter
      next dime
   next nickel
next penny
print count;" ways to make a dollar"


```


Output:

```txt

0 pennies 0 nickels 0 dimes 4 quarters
0 pennies 0 nickels 5 dimes 2 quarters
0 pennies 0 nickels 10 dimes 0 quarters
0 pennies 1 nickels 2 dimes 3 quarters
......
65 pennies 5 nickels 1 dimes 0 quarters
65 pennies 7 nickels 0 dimes 0 quarters
70 pennies 0 nickels 3 dimes 0 quarters
70 pennies 1 nickels 0 dimes 1 quarters

242 ways to make a dollar



```



## Go

A translation of the Lisp code referenced by the task description:

```go
package main

import "fmt"

func main() {
    amount := 100
    fmt.Println("amount, ways to make change:", amount, countChange(amount))
}

func countChange(amount int) int64 {
    return cc(amount, 4)
}

func cc(amount, kindsOfCoins int) int64 {
    switch {
    case amount == 0:
        return 1
    case amount < 0 || kindsOfCoins == 0:
        return 0
    }
    return cc(amount, kindsOfCoins-1) +
        cc(amount - firstDenomination(kindsOfCoins), kindsOfCoins)
}

func firstDenomination(kindsOfCoins int) int {
    switch kindsOfCoins {
    case 1:
        return 1
    case 2:
        return 5
    case 3:
        return 10
    case 4:
        return 25
    }
    panic(kindsOfCoins)
}
```

Output:

```txt

amount, ways to make change: 100 242

```

Alternative algorithm, practical for the optional task.

```go
package main

import "fmt"

func main() {
    amount := 1000 * 100
    fmt.Println("amount, ways to make change:", amount, countChange(amount))
}

func countChange(amount int) int64 {
    ways := make([]int64, amount+1)
    ways[0] = 1
    for _, coin := range []int{100, 50, 25, 10, 5, 1} {
        for j := coin; j <= amount; j++ {
            ways[j] += ways[j-coin]
        }
    }
    return ways[amount]
}
```

Output:

```txt

amount, ways to make change: 100000 13398445413854501

```


## Groovy

{{trans|Go}}
Intuitive Recursive Solution:

```groovy
def ccR
ccR = { BigInteger tot, List<BigInteger> coins ->
    BigInteger n = coins.size()
    switch ([tot:tot, coins:coins]) {
        case { it.tot == 0 } :
            return 1g
        case { it.tot < 0 || coins == [] } :
            return 0g
        default:
            return ccR(tot, coins[1..<n]) +
                ccR(tot - coins[0], coins)
    }
}
```


Fast Iterative Solution:

```groovy
def ccI = { BigInteger tot, List<BigInteger> coins ->
    List<BigInteger> ways = [0g] * (tot+1)
    ways[0] = 1g
    coins.each { BigInteger coin ->
        (coin..tot).each { j ->
            ways[j] += ways[j-coin]
        }
    }
    ways[tot]
}
```


Test:

```groovy
println '\nBase:'
[iterative: ccI, recursive: ccR].each { label, cc ->
    print "${label} "
    def start = System.currentTimeMillis()
    def ways = cc(100g, [25g, 10g, 5g, 1g])
    def elapsed = System.currentTimeMillis() - start
    println ("answer: ${ways}   elapsed: ${elapsed}ms")
}

print '\nExtra Credit:\niterative '
def start = System.currentTimeMillis()
def ways = ccI(1000g * 100, [100g, 50g, 25g, 10g, 5g, 1g])
def elapsed = System.currentTimeMillis() - start
println ("answer: ${ways}   elapsed: ${elapsed}ms")
```


Output:

```txt
Base:
iterative answer: 242   elapsed: 5ms
recursive answer: 242   elapsed: 220ms

Extra Credit:
iterative answer: 13398445413854501   elapsed: 1077ms
```



## Haskell

Naive implementation:

```haskell
count :: (Integral t, Integral a) => t -> [t] -> a
count 0 _ = 1
count _ [] = 0
count x (c:coins) =
  sum
    [ count (x - (n * c)) coins
    | n <- [0 .. (quot x c)] ]

main :: IO ()
main = print (count 100 [1, 5, 10, 25])
```


Much faster, probably harder to read, is to update results from bottom up:

```haskell>count :: Integral a =
 [Int] -> [a]
count = foldr addCoin (1 : repeat 0)
  where
    addCoin c oldlist = newlist
      where
        newlist = take c oldlist ++ zipWith (+) newlist (drop c oldlist)
 
main :: IO ()
main = do
  print (count [25, 10, 5, 1] !! 100)
  print (count [100, 50, 25, 10, 5, 1] !! 10000)
```


Or equivalently, (reformulating slightly, and adding a further test):


```haskell
import Data.Function (fix)

count :: Integral a => [Int] -> [a]
count =
  foldr
    (\x a ->
        let (l, r) = splitAt x a
        in fix (mappend l . flip (zipWith (+)) r))
    (1 : repeat 0)
    

-- TEST -----------------------------------------------------------------------
main :: IO ()
main =
  mapM_
    (print . uncurry ((!!) . count))
    [ ([25, 10, 5, 1], 100)
    , ([100, 50, 25, 10, 5, 1], 10000)
    , ([100, 50, 25, 10, 5, 1], 1000000)
    ]
```

{{Out}}

```txt
242
139946140451
1333983445341383545001
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()

   US_coins       := [1, 5, 10, 25]
   US_allcoins    := [1,5,10,25,50,100]
   EU_coins       := [1, 2, 5, 10, 20, 50, 100, 200]
   CDN_coins      := [1,5,10,25,100,200]
   CDN_allcoins   := [1,5,10,25,50,100,200]

   every trans := ![ [15,US_coins], 
                     [100,US_coins], 
                     [1000*100,US_allcoins] 
                  ] do 
      printf("There are %i ways to count change for %i using %s coins.\n",CountCoins!trans,trans[1],ShowList(trans[2]))
end

procedure ShowList(L)            # helper list to string 
every (s := "[ ") ||:= !L || " "
return s || "]"
end
```


This is a naive implementation and very slow.
{{improve|Icon|Needs a better algorithm.}}

```Icon
procedure CountCoins(amt,coins)  # very slow, recurse by coin value
local count
static S

if type(coins) == "list" then {
   S := sort(set(coins))
   if *S < 1 then runerr(205,coins)
   return  CountCoins(amt)
   }
else {
   /coins := 1
   if value := S[coins] then {
      every (count := 0) +:= CountCoins(amt - (0 to amt by value), coins + 1) 
      return count
      }   
   else    
      return (amt ~= 0) | 1
   }
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting] 

Output:
```txt
There are 6 ways to count change for 15 using [ 1 5 10 25 ] coins.
There are 242 ways to count change for 100 using [ 1 5 10 25 ] coins.
^c
```


Another one:

```Icon

# coin.icn
# usage: coin value
procedure count(coinlist, value)
	if value = 0 then return 1
	if value < 0 then return 0
	if (*coinlist <= 0) & (value >= 1) then return 0
	return count(coinlist[1:*coinlist], value) + count(coinlist, value - coinlist[*coinlist])
end


procedure main(params)
	money := params[1]
	coins := [1,5,10,25]
	
	writes("Value of ", money, " can be changed by using a set of ")
	every writes(coins[1 to *coins], " ")
	write(" coins in ", count(coins, money), " different ways.")
end

```

Output:

```txt

Value of 15 can be changed by using a set of 1 5 10 25  coins in 6 different ways.
Value of 100 can be changed by using a set of 1 5 10 25  coins in 242 different ways.

```



## J


In this draft intermediate results are a two column array.  The first column is tallies -- the number of ways we have for reaching the total represented in the second column, which is unallocated value (which we will assume are pennies).  We will have one row for each different in-range value which can be represented using only nickles (0, 5, 10, ... 95, 100).


```j
merge=: ({:"1 (+/@:({."1),{:@{:)/. ])@;
count=: {.@] <@,. {:@] - [ * [ i.@>:@<.@%~ {:@]
init=: (1 ,. ,.)^:(0=#@$)
nsplits=: 0 { [: +/ [: (merge@:(count"1) init)/ }.@/:~@~.@,
```


This implementation special cases the handling of pennies and assumes that the lowest coin value in the argument is 1.  If I needed additional performance, I would next special case the handling of nickles/penny combinations...

Thus:


```j
   100 nsplits 1 5 10 25
242
```


And, on a 64 bit machine with sufficient memory:


```j
   100000 nsplits 1 5 10 25 50 100
13398445413854501
```


Warning: the above version can miss one when the largest coin is equal to the total value.

For British viewers change from £10 using £10 £5 £2 £1 50p 20p 10p 5p 2p and 1p


```j
   init =: 4 : '(1+x)$1'
length1 =: 4 : '1=#y'
      f =: 4 : ',/ +/\ (-x) ]\ y'

      1000 {  f ` init @. length1 / 1000 500 200 100 50 20 10 5 2 , 1000 0
327631322

NB. this is a foldLeft once initialised the intermediate right arguments are arrays
 1000 f 500 f 200 f 100 f 50 f 20 f 10 f 5 f 2 f (1000 init 0)
```



## Java

{{trans|D}}
{{works with|Java|1.5+}}

```java5
import java.util.Arrays;
import java.math.BigInteger;

class CountTheCoins {
    private static BigInteger countChanges(int amount, int[] coins){
        final int n = coins.length;
        int cycle = 0;
        for (int c : coins)
            if (c <= amount && c >= cycle)
                cycle = c + 1;
        cycle *= n;
        BigInteger[] table = new BigInteger[cycle];
        Arrays.fill(table, 0, n, BigInteger.ONE);
        Arrays.fill(table, n, cycle, BigInteger.ZERO);

        int pos = n;
        for (int s = 1; s <= amount; s++) {
            for (int i = 0; i < n; i++) {
                if (i == 0 && pos >= cycle)
                    pos = 0;
                if (coins[i] <= s) {
                    final int q = pos - (coins[i] * n);
                    table[pos] = (q >= 0) ? table[q] : table[q + cycle];
                }
                if (i != 0)
                    table[pos] = table[pos].add(table[pos - 1]);
                pos++;
            }
        }

        return table[pos - 1];
    }

    public static void main(String[] args) {
        final int[][] coinsUsEu = {{100, 50, 25, 10, 5, 1},
                                   {200, 100, 50, 20, 10, 5, 2, 1}};

        for (int[] coins : coinsUsEu) {
            System.out.println(countChanges(     100,
                Arrays.copyOfRange(coins, 2, coins.length)));
            System.out.println(countChanges(  100000, coins));
            System.out.println(countChanges( 1000000, coins));
            System.out.println(countChanges(10000000, coins) + "\n");
        }
    }
}
```

Output:

```txt
242
13398445413854501
1333983445341383545001
133339833445334138335450001

4562
10056050940818192726001
99341140660285639188927260001
992198221207406412424859964272600001
```



## JavaScript



### Iterative


Efficient iterative algorithm (cleverly calculates number of combinations without permuting them)


```Javascript
function countcoins(t, o) {
    'use strict';
    var targetsLength = t + 1;
    var operandsLength = o.length;
    t = [1];

    for (var a = 0; a < operandsLength; a++) {
        for (var b = 1; b < targetsLength; b++) {

            // initialise undefined target
            t[b] = t[b] ? t[b] : 0;

            // accumulate target + operand ways
            t[b] += (b < o[a]) ? 0 : t[b - o[a]];
        }
    }

    return t[targetsLength - 1];
}
```


{{out}}
JavaScript hits integer limit for optional task

```JavaScript
countcoins(100, [1,5,10,25]);
242
```



### Recursive


Inefficient recursive algorithm (naively calculates number of combinations by actually permuting them)


```Javascript
function countcoins(t, o) {
    'use strict';
    var operandsLength = o.length;
    var solutions = 0;

    function permutate(a, x) {

        // base case
        if (a === t) {
            solutions++;
        }

        // recursive case
        else if (a < t) {
            for (var i = 0; i < operandsLength; i++) {
                if (i >= x) {
                    permutate(o[i] + a, i);
                }
            }
        }
    }

    permutate(0, 0);
    return solutions;
}
```

{{Out}}
Too slow for optional task


```JavaScript
countcoins(100, [1,5,10,25]);
242
```



### Iterative again


{{Trans|C#}}

```javascript
var amount = 100,
    coin = [1, 5, 10, 25]
var t = [1];
for (t[amount] = 0, a = 1; a < amount; a++) t[a] = 0 // initialise t[0..amount]=[1,0,...,0]
for (var i = 0, e = coin.length; i < e; i++)
    for (var ci = coin[i], a = ci; a <= amount; a++)
        t[a] += t[a - ci]
document.write(t[amount])
```

{{Out}}

```txt
242
```



## jq

Currently jq uses IEEE 754 64-bit numbers.  Large integers are approximated by floats, and therefore the answer that the following program provides for the optional task is only correct for the first 15 digits.

```jq
# How many ways are there to make "target" cents, given a list of coin
# denominations as input.
# The strategy is to record at total[n] the number of ways to make n cents.
def countcoins(target):
  . as $coin
  | reduce range(0; length) as $a
      ( [1];   # there is 1 way to make 0 cents
        reduce range(1; target + 1) as $b
          (.;                                      # total[]
           if $b < $coin[$a] then .
           else  .[$b - $coin[$a]] as $count
           | if $count == 0 then .
             else .[$b] += $count
             end
           end ) ) 
  | .[target] ;
```

'''Example''':
 [1,5,10,25] | countcoins(100)
{{Out}}
 242


## Julia

{{trans|Python}}

```julia
function changes(amount::Int, coins::Array{Int})::Int128
    ways = zeros(Int128, amount + 1)
    ways[1] = 1
    for coin in coins, j in coin+1:amount+1
        ways[j] += ways[j - coin]
    end
    return ways[amount + 1]
end

@show changes(100, [1, 5, 10, 25])
@show changes(100000, [1, 5, 10, 25, 50, 100])
```


{{out}}

```txt
changes(100, [1, 5, 10, 25]) = 242
changes(100000, [1, 5, 10, 25, 50, 100]) = 13398445413854501
```



## Kotlin

{{trans|C#}}

```scala
// version 1.0.6

fun countCoins(c: IntArray, m: Int, n: Int): Long {
    val table = LongArray(n + 1)
    table[0] = 1
    for (i in 0 until m) 
        for (j in c[i]..n) table[j] += table[j - c[i]]
    return table[n]
}

fun main(args: Array<String>) {
    val c = intArrayOf(1, 5, 10, 25, 50, 100)
    println(countCoins(c, 4, 100))
    println(countCoins(c, 6, 1000 * 100)) 
}
```


{{out}}

```txt

242
13398445413854501

```



## Lasso

Inspired by the javascript iterative example for the same task

```Lasso
define cointcoins(
	target::integer,
	operands::array
) => {

	local(
		targetlength	= #target + 1,
		operandlength	= #operands -> size,
		output			= staticarray_join(#targetlength,0),
		outerloopcount
	)

	#output -> get(1) = 1

	loop(#operandlength) => {
		#outerloopcount = loop_count
		loop(#targetlength) => {

			if(loop_count >= #operands -> get(#outerloopcount) and loop_count - #operands -> get(#outerloopcount) > 0) => {
				#output -> get(loop_count) += #output -> get(loop_count - #operands -> get(#outerloopcount))
			}
		}
	}

	return #output -> get(#targetlength)
}

cointcoins(100, array(1,5,10,25,))
'<br />'
cointcoins(100000, array(1, 5, 10, 25, 50, 100))
```

Output:

```txt
242
13398445413854501
```



## Lua

Lua uses one-based indexes but table keys can be any value so you can define an element 0 just as easily as you can define an element "foo"...

```Lua
function countSums (amount, values)
    local t = {}
    for i = 1, amount do t[i] = 0 end
    t[0] = 1
    for k, val in pairs(values) do
        for i = val, amount do t[i] = t[i] + t[i - val] end
    end
    return t[amount]
end

print(countSums(100, {1, 5, 10, 25}))
print(countSums(100000, {1, 5, 10, 25, 50, 100}))
```

{{out}}

```txt
242
1.3398445413855e+16
```



## M2000 Interpreter

===Fast O(n*m)===
Works with decimals in table()

```M2000 Interpreter

Module FindCoins {
      Function count(c(), n)  {
            dim table(n+1)=0@ :  table(0)=1@
            for c=0 to len(c())-1 {
                 if c(c)>n then exit
            }
            if c else exit
            for i=0 to c-1 {for j=c(i) to n {table(j)+=table(j-c(i))}}
            =table(n)
      }
      Print "For 1$ ways to change:";count((1,5,10,25),100)
      Print "For 100$ (optional task ways to change):";count((1,5,10,25,50,100),100000)
}
FindCoins

```

{{out}}

```txt

For 1$ ways to change:242
For 100$ (optional task) ways to change:13398445413854501

```


### With Recursion with saving partial results

Using an inventory (a kind of vector) to save first search (but is slower than previous one)


```M2000 Interpreter

Module CheckThisToo {
      inventory c=" 0 0":=1@
      make_change=lambda c (amount, coins()) ->{
            m=lambda c,coins() (n,m)->{if n<0 or m<0 then =0@:exit
            if exist(c,str$(n)+str$(m)) then =eval(c):exit
            append c,str$(n)+str$(m):=lambda(n-coins(m), m)+lambda(n, m-1):=c(str$(n)+str$(m))}
           =m(amount,len(coins())-1)
      }
      Print make_change(100, (1,5,10,25,50,100))=293
      Print make_change(100, (1,5,10,25))=242
      Print make_change(15, (1,5,10,25))=6
      Print make_change(5, (1,5,10,25))=2
}
CheckThisToo

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
{{trans|Go}}

```Mathematica
CountCoins[amount_, coinlist_] := ( ways = ConstantArray[1, amount];
Do[For[j = coin, j <= amount, j++,
  If[ j - coin == 0,
    ways[[j]] ++,
    ways[[j]] += ways[[j - coin]]
]]
, {coin, coinlist}];
ways[[amount]])
```

Example usage:

```txt
CountCoins[100, {25, 10, 5}]
-> 242

CountCoins[100000, {100, 50, 25, 10, 5}]
-> 13398445413854501
```


=={{header|MATLAB}} / {{header|Octave}}==

```Matlab

%% Count_The_Coins
clear;close all;clc;
tic
 
for i = 1:2 % 1st loop is main challenge 2nd loop is optional challenge
    if (i == 1)
        amount = 100;                       % Matlab indexes from 1 not 0, so we need to add 1 to our target value                        
        amount = amount + 1;                    
        coins = [1 5 10 25];                % Value of coins we can use
    else
        amount = 100*1000;                  % Matlab indexes from 1 not 0, so we need to add 1 to our target value                        
        amount = amount + 1; 
        coins = [1 5 10 25 50 100];         % Value of coins we can use
    end % End if
    ways = zeros(1,amount);                 % Preallocating for speed
    ways(1) = 1;                            % First solution is 1
 
    % Solves from smallest sub problem to largest (bottom up approach of dynamic programming).
    for j = 1:length(coins)                 
        for K = coins(j)+1:amount           
            ways(K) = ways(K) + ways(K-coins(j));   
        end % End for
    end % End for
        if (i == 1)
            fprintf(‘Main Challenge: %d \n', ways(amount));
        else
            fprintf(‘Bonus Challenge: %d \n', ways(amount));
        end % End if 
end % End for
toc

```

Example Output:

```txt
Main Challenge: 242

Bonus Challenge: 13398445413854501
```



## Mercury


```Mercury
:- module coins.
:- interface.
:- import_module int, io.
:- type coin ---> quarter; dime; nickel; penny.
:- type purse ---> purse(int, int, int, int).

:- pred sum_to(int::in, purse::out) is nondet.

:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module solutions, list, string.

:- func value(coin) = int.
value(quarter) = 25.
value(dime) = 10.
value(nickel) = 5.
value(penny) = 1.

:- pred supply(coin::in, int::in, int::out) is multi.
supply(C, Target, N) :- upto(Target div value(C), N).

:- pred upto(int::in, int::out) is multi.
upto(N, R) :- ( nondet_int_in_range(0, N, R0) -> R = R0 ; R = 0 ).

sum_to(To, Purse) :-
	Purse = purse(Q, D, N, P),
	sum(Purse) = To,
	supply(quarter, To, Q),
	supply(dime, To, D),
	supply(nickel, To, N),
	supply(penny, To, P).

:- func sum(purse) = int.
sum(purse(Q, D, N, P)) =
	value(quarter) * Q + value(dime) * D +
	value(nickel) * N + value(penny) * P.

main(!IO) :-
	solutions(sum_to(100), L),
	show(L, !IO),
	io.format("There are %d ways to make change for a dollar.\n",
                  [i(length(L))], !IO).

:- pred show(list(purse)::in, io::di, io::uo) is det.
show([], !IO).
show([P|T], !IO) :-
	io.write(P, !IO), io.nl(!IO),
	show(T, !IO).
```



## Nim

{{trans|Python}}

```nim
proc changes(amount: int, coins: openArray[int]): int =
  var ways = @[1]
  ways.setLen(amount+1)
  for coin in coins:
    for j in coin..amount:
      ways[j] += ways[j-coin]
  ways[amount]

echo changes(100, [1, 5, 10, 25])
echo changes(100000, [1, 5, 10, 25, 50, 100])
```

Output:

```txt
242
13398445413854501
```



## OCaml


Translation of the D minimal version:


```ocaml
let changes amount coins =
  let ways = Array.make (amount + 1) 0L in
  ways.(0) <- 1L;
  List.iter (fun coin ->
    for j = coin to amount do
      ways.(j) <- Int64.add ways.(j) ways.(j - coin)
    done
  ) coins;
  ways.(amount)

let () =
  Printf.printf "%Ld\n" (changes    1_00 [25; 10; 5; 1]);
  Printf.printf "%Ld\n" (changes 1000_00 [100; 50; 25; 10; 5; 1]);
;;
```


Output:


```txt
$ ocaml coins.ml 
242
13398445413854501
```



## PARI/GP


```parigp
coins(v)=prod(i=1,#v,1/(1-'x^v[i]));
ways(v,n)=polcoeff(coins(v)+O('x^(n+1)),n);
ways([1,5,10,25],100)
ways([1,5,10,25,50,100],100000)
```

Output:

```txt
%1 = 242
%2 = 13398445413854501
```


## Perl


```perl
use 5.01;
use Memoize;

sub cc {
    my $amount = shift;
    return 0 if !@_ || $amount < 0;
    return 1 if $amount == 0;
    my $first = shift;
    cc( $amount, @_ ) + cc( $amount - $first, $first, @_ );
}
memoize 'cc';

# Make recursive algorithm run faster by sorting coins descending by value:
sub cc_optimized {
    my $amount = shift;
    cc( $amount, sort { $b <=> $a } @_ );
}

say 'Ways to change $ 1 with common coins: ',
    cc_optimized( 100, 1, 5, 10, 25 );
say 'Ways to change $ 1000 with addition of less common coins: ',
    cc_optimized( 1000 * 100, 1, 5, 10, 25, 50, 100 );

```

{{out}}
 Ways to change $ 1 with common coins: 242
 Ways to change $ 1000 with addition of less common coins: 13398445413854501


## Perl 6

{{works with|rakudo|2018.10}}
{{trans|Ruby}}


```perl6
# Recursive (cached)
sub change-r($amount, @coins) {
    my @cache = [1 xx @coins], |([] xx $amount);

    multi ways($n where $n >= 0, @now [$coin,*@later]) {
        @cache[$n;+@later] //= ways($n - $coin, @now) + ways($n, @later);
    }
    multi ways($,@) { 0 }

    # more efficient to start with coins sorted in descending order
    ways($amount, @coins.sort(-*).list);
}

# Iterative
sub change-i(\n, @coins) {
    my @table = [1 xx @coins], [0 xx @coins] xx n;
    (1..n).map: -> \i {
        for ^@coins -> \j {
        my \c = @coins[j];
        @table[i;j] = [+]
            @table[i - c;j] // 0,
            @table[i;j - 1] // 0;
        }
    }
    @table[*-1][*-1];
}

say "Iterative:";
say change-i    1_00, [1,5,10,25];
say change-i 1000_00, [1,5,10,25,50,100];

say "\nRecursive:";
say change-r    1_00, [1,5,10,25];
say change-r 1000_00, [1,5,10,25,50,100];
```

{{out}}

```txt
Iterative:
242
13398445413854501

Recursive:
242
13398445413854501
```



## Phix

Very fast, from http://www.geeksforgeeks.org/dynamic-programming-set-7-coin-change

```Phix
function coin_count(sequence coins, integer amount)
    sequence s = repeat(0,amount+1)
    s[1] = 1
    for c=1 to length(coins) do
        for n=coins[c] to amount do
            s[n+1] += s[n-coins[c]+1]
        end for
    end for
    return s[amount+1]
end function
```

An attempt to explain this algorithm further seems worthwhile:

```Phix
function coin_count(sequence coins, integer amount)
    -- start with 1 known way to achieve 0 (being no coins)
    --  (nb: s[1] holds the solution for 0, s[n+1] for n)
    sequence s = repeat(0,amount+1)
    s[1] = 1
    -- then for every coin that we can use, increase number of 
    --  solutions by that previously found for the remainder.
    for c=1 to length(coins) do
        -- this inner loop is essentially behaving as if we had
        -- called this routine with 1..amount, but skipping any 
        -- less than the coin's value, hence coins[c]..amount.
        for n=coins[c] to amount do
            s[n+1] += s[n-coins[c]+1]
        end for
    end for
    return s[amount+1]
end function

-- The key to understanding the above is to try a dry run of this:
printf(1,"%d\n",coin_count({2,3},5))    -- (prints 1)
-- You'll need 4 2p coins, 3 3p coins, and 5 spaces marked 1..5.
-- Place 2p wherever it fits: 1:0 2:1 3:1 4:1 5:1
-- Add previously found solns: +0  +1  +0  +1  +0   [1]
-- Place 3p wherever it fits: 1:0 2:0 3:1 4:1 5:1
-- Add previously found solns: +0  +0  +1  +0  +1   [2]
-- [1] obviously at 2: we added the base soln for amount=0,
--     and at 4: we added the previously found soln for 2.
--     also note that we added nothing for 2p+3p, yet, that
--     fact is central to understanding why this works. [3]
-- [2] obviously at 3: we added the base soln for amount=0,
--     at 4: we added the zero solutions yet found for 1p,
--     and at 5: we added the previously found soln for 2.
--     you can imagine at 6,9,12 etc all add in soln for 3,
--     albeit by adding that as just added to the precessor.
-- [3] since we add no 3p solns when processing 2p, we do 
--     not count 2p+3p and 3p+2p as two solutions.

--For N = 4 and S = {1,2,3}, there are four solutions: {1,1,1,1},{1,1,2},{2,2},{1,3}.
printf(1,"%d\n",coin_count({1,2,3},4))
--For N = 10 and S = {2, 5, 3, 6}, there are five solutions: {2,2,2,2,2}, {2,2,3,3}, {2,2,6}, {2,3,5} and {5,5}.
printf(1,"%d\n\n",coin_count({2,3,5,6},10))

printf(1,"%d\n",coin_count({25, 10, 5, 1},1_00))
printf(1,"%,d\n",coin_count({100, 50, 25, 10, 5, 1},1000_00))
```

{{out}}

```txt

1
4
5

242
13,398,445,413,854,501

```

Note that a slightly wrong value is printed when running this on 32 bits:

```txt

13,398,445,413,854,501      -- 64 bit (exact)
13,398,445,413,854,496      -- 32 bit (5 out)
 9,007,199,254,740,992      -- max precision (53 bits) of a 64-bit float

```



## PicoLisp

{{trans|C}}

```PicoLisp
(de coins (Sum Coins)
   (let (Buf (mapcar '((N) (cons 1 (need (dec N) 0))) Coins)  Prev)
      (do Sum
         (zero Prev)
         (for L Buf
            (inc (rot L) Prev)
            (setq Prev (car L)) ) )
      Prev ) )
```

Test:

```PicoLisp
(for Coins '((100 50 25 10 5 1) (200 100 50 20 10 5 2 1))
   (println (coins 100 (cddr Coins)))
   (println (coins (* 1000 100) Coins))
   (println (coins (* 10000 100) Coins))
   (println (coins (* 100000 100) Coins))
   (prinl) )
```

Output:

```txt
242
13398445413854501
1333983445341383545001
133339833445334138335450001

4562
10056050940818192726001
99341140660285639188927260001
992198221207406412424859964272600001
```



## Python


### Simple version

{{trans|Go}}

```python
def changes(amount, coins):
    ways = [0] * (amount + 1)
    ways[0] = 1
    for coin in coins:
        for j in xrange(coin, amount + 1):
            ways[j] += ways[j - coin]
    return ways[amount]

print changes(100, [1, 5, 10, 25])
print changes(100000, [1, 5, 10, 25, 50, 100])
```

Output:

```txt
242
13398445413854501
```


### Fast version

{{trans|C}}

```python
try:
    import psyco
    psyco.full()
except ImportError:
    pass

def count_changes(amount_cents, coins):
    n = len(coins)
    # max([]) instead of max() for Psyco
    cycle = max([c+1 for c in coins if c <= amount_cents]) * n
    table = [0] * cycle
    for i in xrange(n):
        table[i] = 1

    pos = n
    for s in xrange(1, amount_cents + 1):
        for i in xrange(n):
            if i == 0 and pos >= cycle:
                pos = 0
            if coins[i] <= s:
                q = pos - coins[i] * n
                table[pos]= table[q] if (q >= 0) else table[q + cycle]
            if i:
                table[pos] += table[pos - 1]
            pos += 1
    return table[pos - 1]

def main():
    us_coins = [100, 50, 25, 10, 5, 1]
    eu_coins = [200, 100, 50, 20, 10, 5, 2, 1]

    for coins in (us_coins, eu_coins):
        print count_changes(     100, coins[2:])
        print count_changes(  100000, coins)
        print count_changes( 1000000, coins)
        print count_changes(10000000, coins), "\n"

main()
```

Output:

```txt
242
13398445413854501
1333983445341383545001
133339833445334138335450001

4562
10056050940818192726001
99341140660285639188927260001
992198221207406412424859964272600001
```



## Racket

This is the basic recursive way:

```Racket
#lang racket
(define (ways-to-make-change cents coins)
  (cond ((null? coins) 0)
        ((negative? cents) 0)
        ((zero? cents) 1)
        (else
         (+ (ways-to-make-change cents (cdr coins))
            (ways-to-make-change (- cents (car coins)) coins)))))

(ways-to-make-change 100 '(25 10 5 1)) ; -> 242

```

This works for the small numbers, but the optional task is just too slow with this solution, so with little change to the code we can use memoization:

```Racket
#lang racket
 
(define memos (make-hash))
(define (ways-to-make-change cents coins)
  (cond [(or (empty? coins) (negative? cents)) 0]
        [(zero? cents) 1]
        [else (define (answerer-for-new-arguments)
                (+ (ways-to-make-change cents (rest coins))
                   (ways-to-make-change (- cents (first coins)) coins)))
              (hash-ref! memos (cons cents coins) answerer-for-new-arguments)]))

(time (ways-to-make-change 100 '(25 10 5 1)))
(time (ways-to-make-change 100000 '(100 50 25 10 5 1)))
(time (ways-to-make-change 1000000 '(200 100 50 20 10 5 2 1)))

#| Times in milliseconds, and results:

     cpu time: 1 real time: 1 gc time: 0
     242

     cpu time: 524 real time: 553 gc time: 163
     13398445413854501

     cpu time: 20223 real time: 20673 gc time: 10233
     99341140660285639188927260001 |#
```



## REXX


### recursive

The recursive calls to the subroutine have been unrolled somewhat, this reduces the number of recursive calls substantially.

These REXX versions also support fractional cents  (as in a   <big>½</big>-cent   and   <big>¼</big>-cent coins).   Any fractional coin can be 

specified as a decimal fraction    (.5,    .25,   <b>···</b>).

Support was included to allow specification of half-cent and quarter-cent coins as   '''1/2''' 
  and   '''1/4'''.

The amount can be specified in cents (as a number), or in dollars (as for instance,   $1000).

```rexx
/*REXX program counts the number of ways to make change with coins from an given amount.*/
numeric digits 20                                /*be able to handle large amounts of $.*/
parse arg N $                                    /*obtain optional arguments from the CL*/
if N='' | N=","    then N=100                    /*Not specified?  Then Use $1  (≡100¢).*/
if $='' | $=","    then $=1 5 10 25              /*Use penny/nickel/dime/quarter default*/
if left(N,1)=='$'  then N=100*substr(N,2)        /*the amount was specified in  dollars.*/
coins=words($)                                   /*the number of coins specified.       */
NN=N;              do j=1  for coins             /*create a fast way of accessing specie*/
                   _=word($,j)                   /*define an array element for the coin.*/
                   if _=='1/2'  then _=.5        /*an alternate spelling of a half-cent.*/
                   if _=='1/4'  then _=.25       /* "     "         "     " " quarter-¢.*/
                   $.j=_                         /*assign the value to a particular coin*/
                   end   /*j*/
_=n//100;                          cnt=' cents'  /* [↓]  is the amount in whole dollars?*/
if _=0  then do; NN='$' || (NN%100);  cnt=;  end /*show the amount in dollars, not cents*/
say 'with an amount of '      commas(NN)cnt",  there are "       commas( MKchg(N, coins) )
say 'ways to make change with coins of the following denominations: '    $
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: procedure;  parse arg _;           n=_'.9';     #=123456789;     b=verify(n,#,"M")
        e=verify(n,#'0',,verify(n,#"0.",'M'))-4
                    do j=e  to b  by -3;   _=insert(',',_,j);    end  /*j*/;      return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
MKchg: procedure expose $.;       parse arg a,k  /*this function is invoked recursively.*/
         if a==0    then return 1                /*unroll for a special case of  zero.  */
         if k==1    then return 1                /*   "    "  "    "      "   "  unity. */
         if k==2    then f=1                     /*handle this special case   of  two.  */
                    else f=MKchg(a, k-1)         /*count,  and then recurse the amount. */
         if a==$.k  then return f+1              /*handle this special case of A=a coin.*/
         if a <$.k  then return f                /*   "     "     "      "   " A<a coin.*/
                         return f+MKchg(a-$.k,k) /*use diminished amount ($) for change.*/
```

'''output'''   when using the default input:

```txt

with an amount of  $1,  there are  242
ways to make change with coins of the following denominations:  1 5 10 25

```

'''output'''   when using the following input:   <tt> $1   1/4   1/2   1   2   3   5   10   20   25   50   100 </tt>

```txt

with an amount of  $1,  there are  29,034,171
ways to make change with coins of the following denominations:  1/4 1/2 1 2 3 5 10 20 25 50 100

```



### with memoization

This REXX version is more than a couple of orders of magnitude faster than the 1<sup>st</sup> version when using larger amounts.

```rexx
/*REXX program counts the number of ways to make change with coins from an given amount.*/
numeric digits 20                                /*be able to handle large amounts of $.*/
parse arg N $                                    /*obtain optional arguments from the CL*/
if N='' | N=","    then N=100                    /*Not specified?  Then Use $1  (≡100¢).*/
if $='' | $=","    then $=1 5 10 25              /*Use penny/nickel/dime/quarter default*/
if left(N,1)=='$'  then N=100*substr(N,2)        /*the amount was specified in  dollars.*/
coins=words($)                                   /*the number of coins specified.       */
!.=.;  NN=N;       do j=1  for coins             /*create a fast way of accessing specie*/
                   _=word($,j);      ?=_ ' coin' /*define an array element for the coin.*/
                   if _=='1/2'  then _=.5        /*an alternate spelling of a half-cent.*/
                   if _=='1/4'  then _=.25       /* "     "         "     " " quarter-¢.*/
                   $.j=_                         /*assign the value to a particular coin*/
                   end   /*j*/
_=n//100;                          cnt=' cents'  /* [↓]  is the amount in whole dollars?*/
if _=0  then do; NN='$' || (NN%100);  cnt=;  end /*show the amount in dollars, not cents*/
say 'with an amount of '      commas(NN)cnt",  there are "       commas( MKchg(N, coins) )
say 'ways to make change with coins of the following denominations: '    $
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: procedure;  parse arg _;           n=_'.9';     #=123456789;     b=verify(n,#,"M")
        e=verify(n,#'0',,verify(n,#"0.",'M'))-4
                    do j=e  to b  by -3;   _=insert(',',_,j);    end  /*j*/;      return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
MKchg:  procedure expose $. !.;  parse arg a,k               /*function is recursive.   */
        if !.a.k\==. then return !.a.k                       /*found this A & K before? */
        if a==0      then return 1                           /*unroll for a special case*/
        if k==1      then return 1                           /*   "    "  "    "      " */
        if k==2  then f=1                                    /*handle this special case.*/
                 else f=MKchg(a, k-1)                        /*count, recurse the amount*/
        if a==$.k    then do; !.a.k=f+1; return !.a.k; end   /*handle this special case.*/
        if a <$.k    then do; !.a.k=f  ; return f    ; end   /*   "     "     "      "  */
        !.a.k=f + MKchg(a-$.k, k);       return !.a.k        /*compute, define, return. */
```

'''output'''   when using the following input for the optional test case:  
<tt> $1000   1   5   10   25   50   100 </tt>

```txt

with an amount of  $1,000,  there are  13,398,445,413,854,501
ways to make change with coins of the following denominations:  1 5 10 25 50 100

```



### with error checking

This REXX version is identical to the previous REXX version, but has error checking for the amount and the coins specified. 

```rexx
/*REXX program counts the number of ways to make change with coins from an given amount.*/
numeric digits 20                                /*be able to handle large amounts of $.*/
parse arg N $                                    /*obtain optional arguments from the CL*/
if N='' | N=","    then N=100                    /*Not specified?  Then Use $1  (≡100¢).*/
if $='' | $=","    then $=1 5 10 25              /*Use penny/nickel/dime/quarter default*/
X=N                                              /*save original for possible error msgs*/
if left(N,1)=='$'  then do                       /*the amount has a leading dollar sign.*/
                        _=substr(N,2)            /*the amount was specified in  dollars.*/
                        if \isNum(_)  then call ser  "amount isn't numeric: "   N
                        N=100*_                  /*change amount (in $) ───►  cents (¢).*/
                        end
max$=10**digits()                                /*the maximum amount this pgm can have.*/
if \isNum(N)  then call  ser  X   " amount isn't numeric."
if N=0        then call  ser  X   " amount can't be zero."
if N<0        then call  ser  X   " amount can't be negative."
if N>max$     then call  ser  X   " amount can't be greater than " max$'.'
coins=words($);  !.=.;   NN=N;   p=0             /*#coins specified; coins; amount; prev*/
@.=0                                             /*verify a coin was only specified once*/
          do j=1  for coins                      /*create a fast way of accessing specie*/
          _=word($,j);     ?=_ ' coin'           /*define an array element for the coin.*/
          if _=='1/2'  then _=.5                 /*an alternate spelling of a half-cent.*/
          if _=='1/4'  then _=.25                /* "     "         "     " " quarter-¢.*/
          if \isNum(_) then call ser ? "coin value isn't numeric."
          if _<0       then call ser ? "coin value can't be negative."
          if _<=0      then call ser ? "coin value can't be zero."
          if @._       then call ser ? "coin was already specified."
          if _<p       then call ser ? "coin must be greater than previous:"    p
          if _>N       then call ser ? "coin must be less or equal to amount:"  X
          @._=1;  p=_                            /*signify coin was specified; set prev.*/
          $.j=_                                  /*assign the value to a particular coin*/
          end   /*j*/
_=n//100;                          cnt=' cents'  /* [↓]  is the amount in whole dollars?*/
if _=0  then do; NN='$' || (NN%100);  cnt=;  end /*show the amount in dollars, not cents*/
say 'with an amount of '      commas(NN)cnt",  there are "       commas( MKchg(N, coins) )
say 'ways to make change with coins of the following denominations: '    $
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isNum:  return datatype(arg(1), 'N')             /*return 1 if arg is numeric, 0 if not.*/
ser:    say;   say '***error***';   say;   say arg(1);   say;   exit 13     /*error msg.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: procedure;  parse arg _;           n=_'.9';     #=123456789;     b=verify(n,#,"M")
        e=verify(n,#'0',,verify(n,#"0.",'M'))-4
                    do j=e  to b  by -3;   _=insert(',',_,j);    end  /*j*/;      return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
MKchg:  procedure expose $. !.;  parse arg a,k               /*function is recursive.   */
        if !.a.k\==. then return !.a.k                       /*found this A & K before? */
        if a==0      then return 1                           /*unroll for a special case*/
        if k==1      then return 1                           /*   "    "  "    "      " */
        if k==2  then f=1                                    /*handle this special case.*/
                 else f=MKchg(a, k-1)                        /*count, recurse the amount*/
        if a==$.k    then do; !.a.k=f+1; return !.a.k; end   /*handle this special case.*/
        if a <$.k    then do; !.a.k=f  ; return f    ; end   /*   "     "     "      "  */
        !.a.k=f + MKchg(a-$.k, k);       return !.a.k        /*compute, define, return. */
```

'''output'''   is the same as the previous REXX versions. 




## Ring


```ring

penny = 1
nickel = 1
dime = 1 
quarter = 1
count = 0
 
for penny = 0 to 100
    for nickel = 0 to 20
        for dime = 0 to 10
            for quarter = 0 to 4
                if (penny + nickel * 5 + dime * 10 + quarter * 25) = 100
                   see "" + penny + " pennies " + nickel + " nickels " + dime + " dimes " + quarter + " quarters" + nl
                   count = count + 1 
                ok
            next
        next
    next
next
see  count + " ways to make a dollar" + nl

```

Output:

```txt

0 pennies 0 nickels 0 dimes 4 quarters
0 pennies 0 nickels 5 dimes 2 quarters
0 pennies 0 nickels 10 dimes 0 quarters
0 pennies 1 nickels 2 dimes 3 quarters
......
65 pennies 5 nickels 1 dimes 0 quarters
65 pennies 7 nickels 0 dimes 0 quarters
70 pennies 0 nickels 3 dimes 0 quarters
70 pennies 1 nickels 0 dimes 1 quarters

242 ways to make a dollar

```



## Ruby

The algorithm also appears [http://www.algorithmist.com/index.php/Coin_Change here]

'''Recursive, with caching'''


```ruby
def make_change(amount, coins)
  @cache = Array.new(amount+1){|i| Array.new(coins.size, i.zero? ? 1 : nil)}
  @coins = coins
  do_count(amount, @coins.length - 1)
end

def do_count(n, m)
  if n < 0 || m < 0
    0
  elsif @cache[n][m]
    @cache[n][m]
  else
    @cache[n][m] = do_count(n-@coins[m], m) + do_count(n, m-1)
  end
end

p make_change(   1_00, [1,5,10,25])
p make_change(1000_00, [1,5,10,25,50,100])
```


outputs

```txt
242
13398445413854501
```


'''Iterative'''


```ruby
def make_change2(amount, coins)
  n, m = amount, coins.size
  table = Array.new(n+1){|i| Array.new(m, i.zero? ? 1 : nil)}
  for i in 1..n
    for j in 0...m
      table[i][j] = (i<coins[j] ? 0 : table[i-coins[j]][j]) +
                    (j<1        ? 0 : table[i][j-1])
    end
  end
  table[-1][-1]
end

p make_change2(   1_00, [1,5,10,25])
p make_change2(1000_00, [1,5,10,25,50,100])
```

outputs

```txt
242
13398445413854501
```




## Run BASIC


```runbasic
for penny         = 0 to 100
  for nickel      = 0 to 20
    for dime      = 0 to 10
      for quarter = 0 to 4
       if penny + nickel * 5 + dime * 10 + quarter * 25 = 100 then
        print penny;" pennies ";nickel;" nickels "; dime;" dimes ";quarter;" quarters"
        count = count + 1 
      end if
      next quarter
    next dime
  next nickel
next penny
print count;" ways to make a buck"
```
Output:

```txt
0 pennies 0 nickels 0 dimes 4 quarters
0 pennies 0 nickels 5 dimes 2 quarters
0 pennies 0 nickels 10 dimes 0 quarters
0 pennies 1 nickels 2 dimes 3 quarters
......
65 pennies 5 nickels 1 dimes 0 quarters
65 pennies 7 nickels 0 dimes 0 quarters
70 pennies 0 nickels 3 dimes 0 quarters
70 pennies 1 nickels 0 dimes 1 quarters
.....
242 ways to make a buck
```


## Rust


```rust
fn make_change(coins: &[usize], cents: usize) -> usize {
    let size = cents + 1;
    let mut ways = vec![0; size];
    ways[0] = 1;
    for &coin in coins {
        for amount in coin..size {
            ways[amount] += ways[amount - coin];
        }
    }
    ways[cents]
}

fn main() {
    println!("{}", make_change(&[1,5,10,25], 100));
    println!("{}", make_change(&[1,5,10,25,50,100], 100_000));
}
```

{{output}}

```txt
242
13398445413854501
```



## SAS

Generate the solutions using CLP solver in SAS/OR:

```sas
/* call OPTMODEL procedure in SAS/OR */
proc optmodel;
   /* declare set and names of coins */
   set COINS = {1,5,10,25};
   str name {COINS} = ['penny','nickel','dime','quarter'];

   /* declare variables and constraint */
   var NumCoins {COINS} >= 0 integer;
   con Dollar:
      sum {i in COINS} i * NumCoins[i] = 100;

   /* call CLP solver */
   solve with CLP / findallsolns;

   /* write solutions to SAS data set */
   create data sols(drop=s) from [s]=(1.._NSOL_) {i in COINS} <col(name[i])=NumCoins[i].sol[s]>;
quit;

/* print all solutions */
proc print data=sols;
run;
```


Output:

```txt

Obs penny nickel dime quarter 
1 100 0 0 0 
2 95 1 0 0 
3 90 2 0 0 
4 85 3 0 0 
5 80 4 0 0 
...
238 5 2 1 3 
239 0 3 1 3 
240 5 0 2 3 
241 0 1 2 3 
242 0 0 0 4 

```



## Scala


```scala
def countChange(amount: Int, coins:List[Int]) = {
	  val ways = Array.fill(amount + 1)(0)
	  ways(0) = 1
	  coins.foreach (coin =>
	  for (j<-coin to amount)
		  ways(j) =  ways(j) + ways(j - coin)
		  )
	ways(amount)
  }       

countChange (15, List(1, 5, 10, 25)) 

```

Output:

```txt
res0: Int = 6

```


Recursive implementation:

```scala
def count(target: Int, coins: List[Int]): Int = {
  if (target == 0) 1
  else if (coins.isEmpty || target < 0) 0
  else count(target, coins.tail) + count(target - coins.head, coins)
}


count(100, List(25, 10, 5, 1))

```



## Scheme

A simple recursive implementation:

```scheme
(define ways-to-make-change
  (lambda (x coins)
    (cond
      [(null? coins) 0]
      [(< x 0) 0]
      [(zero? x) 1]
      [else (+ (ways-to-make-change x (cdr coins)) (ways-to-make-change (- x (car coins)) coins))])))

(ways-to-make-change 100)
```

Output:

```txt
242
```



## Scilab



### Straightforward solution

Fairly simple solution for the task. Expanding it to the optional task is not recommend, for Scilab will spend a lot of time processing the nested <code>for</code> loops.
<lang>amount=100;
coins=[25 10 5 1];
n_coins=zeros(coins);
ways=0;

for a=0:4
    for b=0:10
        for c=0:20
            for d=0:100
                n_coins=[a b c d];
                change=sum(n_coins.*coins);
                if change==amount then
                    ways=ways+1;
                elseif change>amount
                    break
                end
            end
        end
    end
end

disp(ways);
```


{{out}}

```txt
   242.
```



### Faster approach


{{trans|Python}}

<lang>function varargout=changes(amount, coins)
    ways = zeros(1,amount + 2);
    ways(1) = 1;
    for coin=coins
        for j=coin:(amount+1)
            ways(j+1) = ways(j+1) + ways(j + 1 - coin);
        end
    end
    
    varargout=list(ways(length(ways)))
endfunction

a=changes(100, [1, 5, 10, 25]);
b=changes(100000, [1, 5, 10, 25, 50, 100]);
mprintf("%.0f, %.0f", a, b);
```


{{out}}


```txt
242, 13398445413854540
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";
 
const func bigInteger: changeCount (in integer: amountCents, in array integer: coins) is func
  result
    var bigInteger: waysToChange is 0_;
  local
    var array bigInteger: t is 0 times 0_;
    var integer: pos is 0;
    var integer: s is 0;
    var integer: i is 0;
  begin
    t := length(coins) times 1_ & (length(coins) * amountCents) times 0_;
    pos := length(coins) + 1;
    for s range 1 to amountCents do
      if coins[1] <= s then
        t[pos] := t[pos - (length(coins) * coins[1])];
      end if;
      incr(pos);
      for i range 2 to length(coins) do
        if coins[i] <= s then
          t[pos] := t[pos - (length(coins) * coins[i])];
        end if;
        t[pos] +:= t[pos - 1];
        incr(pos);
      end for;
    end for;
    waysToChange := t[pos - 1];
  end func;
 
const proc: main is func
  local
    const array integer: usCoins is [] (1, 5, 10, 25, 50, 100);
    const array integer: euCoins is [] (1, 2, 5, 10, 20, 50, 100, 200);
  begin
    writeln(changeCount(    100, usCoins[.. 4]));
    writeln(changeCount( 100000, usCoins));
    writeln(changeCount(1000000, usCoins));
    writeln(changeCount( 100000, euCoins));
    writeln(changeCount(1000000, euCoins));
  end func;
```


Output:

```txt

242
13398445413854501
1333983445341383545001
10056050940818192726001
99341140660285639188927260001

```



## Sidef

{{trans|Perl}}

```ruby
func cc(_)                { 0 }
func cc({ .is_neg  }, *_) { 0 }
func cc({ .is_zero }, *_) { 1 }

func cc(amount, first, *rest) is cached {
    cc(amount, rest...) + cc(amount - first, first, rest...);
}

func cc_optimized(amount, *rest) {
    cc(amount, rest.sort_by{|v| -v }...);
}

var x = cc_optimized(100, 1, 5, 10, 25);
say "Ways to change $1 with common coins: #{x}";

var y = cc_optimized(1000 * 100, 1, 5, 10, 25, 50, 100);
say "Ways to change $1000 with addition of less common coins: #{y}";
```

{{out}}

```txt

Ways to change $1 with common coins: 242
Ways to change $1000 with addition of less common coins: 13398445413854501

```



## Swift

{{trans|Python}}
{{libheader|Attaswift BigInt}}

```swift
import BigInt

func countCoins(amountCents cents: Int, coins: [Int]) -> BigInt {
  let cycle = coins.filter({ $0 <= cents }).map({ $0 + 1 }).max()! * coins.count
  var table = [BigInt](repeating: 0, count: cycle)

  for x in 0..<coins.count {
    table[x] = 1
  }

  var pos = coins.count

  for s in 1..<cents+1 {
    for i in 0..<coins.count {
      if i == 0 && pos >= cycle {
        pos = 0
      }

      if coins[i] <= s {
        let q = pos - coins[i] * coins.count
        table[pos] = q >= 0 ? table[q] : table[q + cycle]
      }

      if i != 0 {
        table[pos] += table[pos - 1]
      }

      pos += 1
    }
  }

  return table[pos - 1]
}

let usCoins = [100, 50, 25, 10, 5, 1]
let euCoins = [200, 100, 50, 20, 10, 5, 2, 1]

for set in [usCoins, euCoins] {
  print(countCoins(amountCents: 100, coins: Array(set.dropFirst(2))))
  print(countCoins(amountCents: 100000, coins: set))
  print(countCoins(amountCents: 1000000, coins: set))
  print(countCoins(amountCents: 10000000, coins: set))
  print()
}
```


{{out}}

```txt
242
13398445413854501
1333983445341383545001
133339833445334138335450001

4562
10056050940818192726001
99341140660285639188927260001
992198221207406412424859964272600001
```



## Tcl

{{trans|Ruby}}

```tcl
package require Tcl 8.5

proc makeChange {amount coins} {
    set table [lrepeat [expr {$amount+1}] [lrepeat [llength $coins] {}]]
    lset table 0 [lrepeat [llength $coins] 1]
    for {set i 1} {$i <= $amount} {incr i} {
	for {set j 0} {$j < [llength $coins]} {incr j} {
	    set k [expr {$i - [lindex $coins $j]}]
	    lset table $i $j [expr {
		($k < 0 ? 0 : [lindex $table $k $j]) +
		($j < 1 ? 0 : [lindex $table $i [expr {$j-1}]])
	    }]
	}
    }
    return [lindex $table end end]
}

puts [makeChange 100 {1 5 10 25}]
puts [makeChange 100000 {1 5 10 25 50 100}]
# Making change with the EU coin set:
puts [makeChange 100 {1 2 5 10 20 50 100 200}]
puts [makeChange 100000 {1 2 5 10 20 50 100 200}]
```

Output:

```txt

242
13398445413854501
4563
10056050940818192726001

```



## uBasic/4tH

{{trans|Run BASIC}}
<lang>c = 0
for p       = 0 to 100
  for n     = 0 to 20
    for d   = 0 to 10
      for q = 0 to 4
       if p + n * 5 + d * 10 + q * 25 = 100 then
         print p;" pennies ";n;" nickels "; d;" dimes ";q;" quarters"
         c = c + 1
       endif
      next q
    next d
  next n
next p
print c;" ways to make a buck"
```

{{out}}

```txt
0 pennies 0 nickels 0 dimes 4 quarters
0 pennies 0 nickels 5 dimes 2 quarters
0 pennies 0 nickels 10 dimes 0 quarters
...
90 pennies 2 nickels 0 dimes 0 quarters
95 pennies 1 nickels 0 dimes 0 quarters
100 pennies 0 nickels 0 dimes 0 quarters
242 ways to make a buck

0 OK, 0:312
```


## UNIX Shell


{{trans|Common Lisp}}
{{works with|bash}}

```bash
function count_change {
  local -i amount=$1 coin j
  local ways=(1)
  shift
  for coin; do
    for (( j=coin; j <= amount; j++ )); do
      let ways[j]=${ways[j]:-0}+${ways[j-coin]:-0}
    done
  done
  echo "${ways[amount]}"
}
count_change 100 25 10 5 1
count_change 100000 100 50 25 10 5 1
```


{{works with|ksh|93}}

```bash
function count_change {
  typeset -i amount=$1 coin j
  typeset ways
  set -A ways 1
  shift
  for coin; do
    for (( j=coin; j <= amount; j++ )); do
      let ways[j]=${ways[j]:-0}+${ways[j-coin]:-0}
    done
  done
  echo "${ways[amount]}"
}
count_change 100 25 10 5 1
count_change 100000 100 50 25 10 5 1
```


{{works with|ksh|88}}

```bash
function count_change {
  typeset -i amount=$1 coin j
  typeset ways
  set -A ways 1
  shift
  for coin; do
    let j=coin
    while (( j <= amount )); do
      let ways[j]=${ways[j]:-0}+${ways[j-coin]:-0}
      let j+=1
    done
  done
  echo "${ways[amount]}"
}
count_change 100 25 10 5 1
# (optional task exceeds a subscript limit in ksh88)
```


And just for fun, here's one that works even with the original V7 shell:

{{works with|sh|v7}}

```bash
if [ $# -lt 2 ]; then
  set ${1-100} 25 10 5 1
fi
amount=$1
shift
ways_0=1
for coin in "$@"; do
  j=$coin
  while [ $j -le $amount ]; do
    d=`expr $j - $coin`
    eval "ways_$j=\`expr \${ways_$j-0} + \${ways_$d-0}\`"
    j=`expr $j + 1`
  done
done
eval "echo \$ways_$amount"
```


{{Out}}

```txt
242
13398445413854501
```



## VBA

{{trans|Phix}}
```vb
Private Function coin_count(coins As Variant, amount As Long) As Variant 'return type will be Decimal
    'sequence s = Repeat(0, amount + 1)
    Dim s As Variant
    ReDim s(amount + 1)
    Dim c As Integer
    s(1) = CDec(1)
    For c = 1 To UBound(coins)
        For n = coins(c) To amount
            s(n + 1) = CDec(s(n + 1) + s(n - coins(c) + 1))
        Next n
    Next c
    coin_count = s(amount + 1)
End Function
Public Sub main2()
    Dim us_commons_coins As Variant
    'The next line creates a base 1 array
    us_common_coins = [{25, 10, 5, 1}]
    Debug.Print coin_count(us_common_coins, 100)
    Dim us_coins As Variant
    us_coins = [{100,50,25, 10, 5, 1}]
    Debug.Print coin_count(us_coins, 100000)
End Sub
```
{{out}}

```txt
 242 
 13398445413854501 
```



## VBScript

{{trans|C#}}

```vb

Function count(coins,m,n)
	ReDim table(n+1)
	table(0) = 1
	i = 0
	Do While i < m
		j = coins(i)
		Do While j <= n
			table(j) = table(j) + table(j - coins(i))
			j = j + 1
		Loop
		i = i + 1
	Loop
	count = table(n)
End Function

'testing
arr = Array(1,5,10,25)
m = UBound(arr) + 1
n = 100
WScript.StdOut.WriteLine count(arr,m,n)

```


{{Out}}

```txt

242

```



## Visual Basic

{{trans|VBA}}
{{works with|Visual Basic|6}}

```vb
Option Explicit
'----------------------------------------------------------------------
Private Function coin_count(coins As Variant, amount As Long) As Variant
'return type will be Decimal
Dim s() As Variant
Dim n As Long, c As Long
    
  ReDim s(amount + 1)
  s(1) = CDec(1)
  For c = LBound(coins) To UBound(coins)
    For n = coins(c) To amount
      s(n + 1) = CDec(s(n + 1) + s(n - coins(c) + 1))
    Next n
  Next c
  coin_count = s(amount + 1)
End Function
'----------------------------------------------------------------------
Sub Main()
Dim us_common_coins As Variant
Dim us_coins As Variant
    
  'The next line creates 0-based array
  us_common_coins = Array(25, 10, 5, 1)
  Debug.Print coin_count(us_common_coins, 100)
  
  us_coins = Array(100, 50, 25, 10, 5, 1)
  Debug.Print coin_count(us_coins, 100000)
  
End Sub
```

{{out}}

```txt
 242 
 13398445413854501
```



## zkl

{{trans|Scheme}}

```zkl
fcn ways_to_make_change(x, coins=T(25,10,5,1)){
   if(not coins) return(0);
   if(x<0)  return(0);
   if(x==0) return(1);
   ways_to_make_change(x, coins[1,*]) + ways_to_make_change(x - coins[0], coins)
}
ways_to_make_change(100).println();
```

{{out}}

```txt
242
```

Blows the stack on the optional part, so try this:

{{trans|Ruby}}

```zkl
fcn make_change2(amount, coins){
  n, m  := amount, coins.len();
  table := (0).pump(n+1,List, (0).pump(m,List().write,1).copy);
  foreach i,j in ([1..n],[0..m-1]){
     table[i][j] = (if(i<coins[j]) 0 else table[i-coins[j]][j]) +
                   (if(j<1)        0 else table[i][j-1])
  }
  table[-1][-1]
}

println(make_change2(   100, T(1,5,10,25)));
make_change2(0d1000_00, T(1,5,10,25,50,100)) : "%,d".fmt(_).println();
```

{{out}}

```txt

242
13,398,445,413,854,501

```



## ZX Spectrum Basic

{{trans|AWK}}
Test with emulator at full speed for reasonable performance.

```zxbasic
10 LET amount=100
20 GO SUB 1000
30 STOP 
1000 LET nPennies=amount
1010 LET nNickles=INT (amount/5)
1020 LET nDimes=INT (amount/10)
1030 LET nQuarters=INT (amount/25)
1040 LET count=0
1050 FOR p=0 TO nPennies
1060 FOR n=0 TO nNickles
1070 FOR d=0 TO nDimes
1080 FOR q=0 TO nQuarters
1090 LET s=p+n*5+d*10+q*25
1100 IF s=100 THEN LET count=count+1
1110 NEXT q
1120 NEXT d
1130 NEXT n
1140 NEXT p
1150 PRINT count
1160 RETURN 
```

