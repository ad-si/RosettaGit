+++
title = "Truncatable primes"
description = ""
date = 2019-10-14T11:51:32Z
aliases = []
[extra]
id = 8274
[taxonomies]
categories = ["task", "Prime Numbers"]
tags = []
languages = [
  "ada",
  "algol_68",
  "arturo",
  "autohotkey",
  "awk",
  "bracmat",
  "c",
  "clojure",
  "coffeescript",
  "common_lisp",
  "csharp",
  "d",
  "echolisp",
  "eiffel",
  "elena",
  "elixir",
  "factor",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "java",
  "julia",
  "kotlin",
  "lua",
  "maple",
  "mathematica",
  "matlab",
  "nim",
  "oorexx",
  "openedge_progress",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "sidef",
  "tcl",
  "vbscript",
  "xpl0",
  "zkl",
]
+++

A truncatable prime is a prime number that when you successively remove digits from one end of the prime, you are left with a new prime number.


;Examples:
The number '''997''' is called a ''left-truncatable prime'' as the numbers '''997''', '''97''', and '''7''' are all prime.

The number '''7393''' is a ''right-truncatable prime'' as the numbers '''7393''', '''739''', '''73''', and '''7''' formed by removing digits from its right are also prime.

No zeroes are allowed in truncatable primes.


## Task

The task is to find the largest left-truncatable and right-truncatable primes less than one million (base 10 is implied).


## Related tasks

* [[Find largest left truncatable prime in a given base]]
* [[Sieve of Eratosthenes]]


## See also

* [http://mathworld.wolfram.com/TruncatablePrime.html Truncatable Prime] from MathWorld.]





## Ada


```Ada

with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Containers.Ordered_Sets;

procedure Truncatable_Primes is
   package Natural_Set is new Ada.Containers.Ordered_Sets (Natural);
   use Natural_Set;

   Primes : Set;

   function Is_Prime (N : Natural) return Boolean is
      Position : Cursor := First (Primes);
   begin
      while Has_Element (Position) loop
         if N mod Element (Position) = 0 then
            return False;
         end if;
         Position := Next (Position);
      end loop;
      return True;
   end Is_Prime;

   function Is_Left_Trucatable_Prime (N : Positive) return Boolean is
      M : Natural := 1;
   begin
      while Contains (Primes, N mod (M * 10)) and (N / M) mod 10 > 0 loop
         M := M * 10;
         if N <= M then
            return True;
         end if;
      end loop;
      return False;
   end Is_Left_Trucatable_Prime;

   function Is_Right_Trucatable_Prime (N : Positive) return Boolean is
      M : Natural := N;
   begin
      while Contains (Primes, M) and M mod 10 > 0 loop
         M := M / 10;
         if M <= 1 then
            return True;
         end if;
      end loop;
      return False;
   end Is_Right_Trucatable_Prime;

   Position : Cursor;
begin
   for N in 2..1_000_000 loop
      if Is_Prime (N) then
         Insert (Primes, N);
      end if;
   end loop;
   Position := Last (Primes);
   while Has_Element (Position) loop
      if Is_Left_Trucatable_Prime (Element (Position)) then
         Put_Line ("Largest LTP from 1..1000000:" & Integer'Image (Element (Position)));
         exit;
      end if;
      Previous (Position);
   end loop;
   Position := Last (Primes);
   while Has_Element (Position) loop
      if Is_Right_Trucatable_Prime (Element (Position)) then
         Put_Line ("Largest RTP from 1..1000000:" & Integer'Image (Element (Position)));
         exit;
      end if;
      Previous (Position);
   end loop;
end Truncatable_Primes;

```

Sample output:

```txt

Largest LTP from 1..1000000: 998443
Largest RTP from 1..1000000: 739399

```



## ALGOL 68

{{trans|C}} Note: This specimen retains the original [[#C|C]] coding style.
```algol68
#!/usr/local/bin/a68g --script #

PROC is prime = (INT n)BOOL:(
  []BOOL is short prime=(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE);
  IF n<=UPB is short prime THEN is short prime[n] # EXIT # ELSE
    IF ( NOT ODD n | TRUE | n MOD 3 = 0 ) THEN FALSE # EXIT # ELSE
      INT h := ENTIER sqrt(n)+3;
      FOR a FROM 7 BY 6 WHILE a<h DO
        IF ( n MOD a = 0 | TRUE |  n MOD (a-2) = 0 ) THEN false exit FI
      OD;
      TRUE # EXIT #
    FI
  FI EXIT
  false exit: FALSE
);

PROC string to int = (STRING in a)INT:(
  FILE f; STRING a := in a; associate(f, a);
  INT i; get(f, i); close(f);
  i
);

PROC is trunc prime = (INT in n, PROC(REF STRING)VOID trunc)BOOL: (
  INT n := in n;
  STRING s := whole(n, 0);
  IF char in string("0", NIL, s) THEN FALSE # EXIT #
  ELSE
    WHILE is prime(n) DO
      s := whole(n, 0);
      trunc(s);
      IF UPB s = 0 THEN true exit FI;
      n := string to int(s)
    OD;
    FALSE EXIT
    true exit: TRUE
  FI
);

PROC get trunc prime = (INT in n, PROC(REF STRING)VOID trunc)VOID:(
  FOR n FROM in n BY -1 TO 1 DO
    IF is trunc prime(n, trunc) THEN
      printf(($g(0)l$, n));
      break
    FI
  OD;
  break: ~
);

main:(
  INT limit = 1000000;
  printf(($g g(0) gl$,"Highest left- and right-truncatable primes under ",limit,":"));
  get trunc prime(limit, (REF STRING s)VOID: s := s[LWB s+1:]);
  get trunc prime(limit, (REF STRING s)VOID: s := s[:UPB s-1]);
  write("Press Enter");
  read(newline)
)
```

Output:

```txt

Highest left- and right-truncatable primes under 1000000:
998443
739399
Press Enter

```



## Arturo



```arturo
leftTrunc [num]{
	str $(toString num)
	ret #()
	loop $(range 0 $(size str)-1) [x]{
		ret ret + $(slice str x $(size str))
	}
	return $(map ret { toNumber & })
}

rightTrunc [num]{
	str $(toString num)
	ret #()
	loop $(range $(size str) 1) [x]{
		ret ret + $(slice str 0 x)
	}
	return $(map ret { toNumber & })
}

isTruncatablePrime [nums]{ all nums { isPrime & } }

MaxLeft 0
MaxRight 0

loop $(range 3 1000000 2) [n]{
	if $(isTruncatablePrime $(leftTrunc n)) { MaxLeft n }
	if $(isTruncatablePrime $(rightTrunc n)) { MaxRight n }
}

print "Max Left-Truncatable Prime found (<1000000): " + MaxLeft
print "Max Right-Truncatable Prime found (<1000000): " + MaxRight
```


```txt
Max Left-Truncatable Prime found (<1000000): 998443
Max Right-Truncatable Prime found (<1000000): 739399
```



## AutoHotkey


```AutoHotkey
SetBatchLines, -1
MsgBox, % "Largest left-truncatable and right-truncatable primes less than one million:`n"
	. "Left:`t" LTP(10 ** 6) "`nRight:`t" RTP(10 ** 6)

LTP(n) {
	while n {
		n--
		if (!Instr(n, "0") && IsPrime(n)) {
			Loop, % StrLen(n)
				if (!IsPrime(SubStr(n, A_Index)))
					continue, 2
			break
		}
	}
	return, n
}

RTP(n) {
	while n {
		n--
		if (!IsPrime(SubStr(n, 1, 1)))
			n -= 10 ** (StrLen(n) - 1)
		if (!Instr(n, "0") && IsPrime(n)) {
			Loop, % StrLen(n)
				if (!IsPrime(SubStr(n, 1, A_Index)))
					continue, 2
			break
		}
	}
	return, n
}

IsPrime(n) {
	if (n < 2)
		return, 0
	else if (n < 4)
		return, 1
	else if (!Mod(n, 2))
		return, 0
	else if (n < 9)
		return 1
	else if (!Mod(n, 3))
		return, 0
	else {
		r := Floor(Sqrt(n))
		f := 5
		while (f <= r) {
			if (!Mod(n, f))
				return, 0
			if (!Mod(n, (f + 2)))
				return, 0
			f += 6
		}
		return, 1
	}
}
```

'''Output:'''

```txt
Largest left-truncatable and right-truncatable primes less than one million:
Left:	998443
Right:	739399
```


## AWK


```AWK

# syntax: GAWK -f TRUNCATABLE_PRIMES.AWK
BEGIN {
    limit = 1000000
    for (i=1; i<=limit; i++) {
      if (is_prime(i)) {
        prime_count++
        arr[i] = ""
        if (truncate_left(i) == 1) {
          max_left = max(max_left,i)
        }
        if (truncate_right(i) == 1) {
          max_right = max(max_right,i)
        }
      }
    }
    printf("1-%d: %d primes\n",limit,prime_count)
    printf("largest L truncatable: %d\n",max_left)
    printf("largest R truncatable: %d\n",max_right)
    exit(0)
}
function is_prime(x,  i) {
    if (x <= 1) {
      return(0)
    }
    for (i=2; i<=int(sqrt(x)); i++) {
      if (x % i == 0) {
        return(0)
      }
    }
    return(1)
}
function truncate_left(n) {
    while (n != "") {
      if (!(n in arr)) {
        return(0)
      }
      n = substr(n,2)
    }
    return(1)
}
function truncate_right(n) {
    while (n != "") {
      if (!(n in arr)) {
        return(0)
      }
      n = substr(n,1,length(n)-1)
    }
    return(1)
}
function max(x,y) { return((x > y) ? x : y) }

```

```txt

1-1000000: 78498 primes
largest L truncatable: 998443
largest R truncatable: 739399

```



## Bracmat

Primality test: In an attempt to compute the result of taking a (not too big, 2^32 or 2^64, depending on word size) number to a fractional power, Bracmat computes the prime factors of the number and checks whether the powers of prime factors make the fractional power go away. If the number is prime, the output of the computation is the same as the input.

```bracmat
( 1000001:?i
&   whl
  ' ( !i+-2:>0:?i
    & !i:?L
    & whl'(!L^1/2:#?^1/2&@(!L:% ?L))
    & !L:~
    )
& out$("left:" !i)
& 1000001:?i
&   whl
  ' ( !i+-2:>0:?i
    & !i:?R
    & whl'(!R^1/2:#?^1/2&@(!R:?R %@))
    & !R:~
    )
& out$("right:" !i)
)
```

Output:

```txt
left: 998443
right: 739399
```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_PRIME 1000000
char *primes;
int n_primes;

/*  Sieve. If we were to handle 10^9 range, use bit field. Regardless,
 *  if a large amount of prime numbers need to be tested, sieve is fast.
 */
void init_primes()
{
	int j;
	primes = malloc(sizeof(char) * MAX_PRIME);
	memset(primes, 1, MAX_PRIME);
	primes[0] = primes[1] = 0;
	int i = 2;
	while (i * i < MAX_PRIME) {
		for (j = i * 2; j < MAX_PRIME; j += i)
			primes[j] = 0;
		while (++i < MAX_PRIME && !primes[i]);
	}
}

int left_trunc(int n)
{
	int tens = 1;
	while (tens < n) tens *= 10;

	while (n) {
		if (!primes[n]) return 0;
		tens /= 10;
		if (n < tens) return 0;
		n %= tens;
	}
	return 1;
}

int right_trunc(int n)
{
	while (n) {
		if (!primes[n]) return 0;
		n /= 10;
	}
	return 1;
}

int main()
{
	int n;
	int max_left = 0, max_right = 0;
	init_primes();

	for (n = MAX_PRIME - 1; !max_left;  n -= 2)
		if (left_trunc(n)) max_left = n;

	for (n = MAX_PRIME - 1; !max_right; n -= 2)
		if (right_trunc(n)) max_right = n;

	printf("Left: %d; right: %d\n", max_left, max_right);
	return 0;
}
```
output<lang>Left: 998443; right: 739399
```


Faster way of doing primality test for small numbers (1000000 isn't big), and generating truncatable primes bottom-up:

```c
#include <stdio.h>

#define MAXN 1000000
int maxl, maxr;

int is_prime(int n)
{
	int p;
	if (n % 3 == 0) return 0;

	for (p = 6; p * p <= n; p += 6)
		if (!(n % (p + 1) && n % (p + 5)))
			return 0;
	return 1;
}

void left(int n, int tens)
{
	int i, nn;

	if (n > maxl) maxl = n;
	if (n < MAXN / 10)
		for (tens *= 10, i = 1; i < 10; i++)
			if (is_prime(nn = i * tens + n))
				left(nn, tens);
}

void right(int n)
{
	int i, nn;
	static int d[] = {1,3,7,9};

	if (n > maxr) maxr = n;
	if (n < MAXN / 10)
		for (i = 1; i < 4; i++)
			if (is_prime(nn = n * 10 + d[i])) right(nn);
}

int main(void)
{
	left(3, 1); left(7, 1);
	right(3); right(5); right(7);

	printf("%d %d\n", maxl, maxr);

	return 0;
}
```

```txt

998443 739399

```


## C#

```c#
using System;  // 4790@3.6
using System.Collections.Generic;
class truncatable_primes
{
    static void Main()
    {
        uint m = 1000000;
        Console.Write("L " + L(m) + " R " + R(m) + "  ");
        var sw = System.Diagnostics.Stopwatch.StartNew();
        for (int i = 1000; i > 0; i--) { L(m); R(m); }
        Console.Write(sw.Elapsed); Console.Read();
    }

    static uint L(uint n)
    {
        n -= n & 1; n--;
        for (uint d, d1 = 100; ; n -= 2)
        {
            while (n % 3 == 0 || n % 5 == 0 || n % 7 == 0) n -= 2;
            if ((d = n % 10) == 3 || d == 7)
            {
                while (d1 < n && d < (d = n % d1) && isP(d)) d1 *= 10;
                if (d1 > n && isP(n)) return n; d1 = 100;
            }
        }
    }

    static uint R(uint m)
    {
        var p = new List<uint>() { 2, 3, 5, 7 }; uint n = 20, np;
        for (int i = 1; i < p.Count; n = 10 * p[i++])
        {
            if ((np = n + 1) >= m) break; if (isP(np)) p.Add(np);
            if ((np = n + 3) >= m) break; if (isP(np)) p.Add(np);
            if ((np = n + 7) >= m) break; if (isP(np)) p.Add(np);
            if ((np = n + 9) >= m) break; if (isP(np)) p.Add(np);
        }
        return p[p.Count - 1];
    }

    static bool isP(uint n)
    {
        if (n < 7) return n == 2 || n == 3 || n == 5;
        if ((n & 1) == 0 || n % 3 == 0 || n % 5 == 0) return false;
        for (uint r = (uint)Math.Sqrt(n), d = 7; d <= r; d += 30)
            if (n % (d + 00) == 0 || n % (d + 04) == 0 ||
                n % (d + 06) == 0 || n % (d + 10) == 0 ||
                n % (d + 12) == 0 || n % (d + 16) == 0 ||
                n % (d + 22) == 0 || n % (d + 24) == 0) return false;
        return true;
    }
}
```


```txt
Output:  L 998443 R 739399   24 μs
```



## Clojure


```Clojure
(use '[clojure.contrib.lazy-seqs :only [primes]])

(def prime?
  (let [mem (ref #{})
	primes (ref primes)]
    (fn [n]
      (dosync
       (if (< n (first @primes))
	 (@mem n)
	 (let [[mems ss] (split-with #(<= % n) @primes)]
	   (ref-set primes ss)
	   ((commute mem into mems) n)))))))

(defn drop-lefts [n]
  (let [dropl #(if (< % 10) 0 (Integer. (subs (str %) 1)))]
    (->> (iterate dropl n)
	 (take-while pos? ,)
	 next)))

(defn drop-rights [n]
  (->> (iterate #(quot % 10) n)
       next
       (take-while pos? ,)))

(defn truncatable-left? [n]
  (every? prime? (drop-lefts n)))

(defn truncatable-right? [n]
  (every? prime? (drop-rights n)))

user> (->> (for [p primes
	   :while (< p 1000000)
	   :when (not-any? #{\0} (str p))
	   :let [l? (if (truncatable-left? p) p 0)
		 r? (if (truncatable-right? p) p 0)]
	    :when (or l? r?)]
       [l? r?])
     ((juxt #(apply max-key first %) #(apply max-key second %)) ,)
     ((juxt ffirst (comp second second)) ,)
     (map vector ["left truncatable: " "right truncatable: "] ,))
(["left truncatable: " 998443] ["right truncatable: " 739399])
```



## CoffeeScript


```coffeescript
# You could have symmetric algorithms for max right and left
# truncatable numbers, but they lend themselves to slightly
# different optimizations.

max_right_truncatable_number = (n, f) ->
  # This algorithm only evaluates 37 numbers for primeness to
  # get the max right truncatable prime < 1000000.  Its
  # optimization is that it prunes candidates for
  # the first n-1 digits before having to iterate through
  # the 10 possibilities for the last digit.
  if n < 10
    candidate = n
    while candidate > 0
      return candidate if f(candidate)
      candidate -= 1
  else
    left = Math.floor n / 10
    while left > 0
      left = max_right_truncatable_number left, f
      right = 9
      while right > 0
        candidate = left * 10 + right
        return candidate if candidate <= n and f(candidate)
        right -= 1
      left -= 1
  throw Error "none found"

max_left_truncatable_number = (max, f) ->
  # This is a pretty straightforward countdown.  The first
  # optimization here would probably be to cache results of
  # calling f on small numbers.
  is_left_truncatable = (n) ->
    candidate = 0
    power_of_ten = 1
    while n > 0
      r = n  % 10
      return false if r == 0
      n = Math.floor n / 10
      candidate = r * power_of_ten + candidate
      power_of_ten *= 10
      return false unless f(candidate)
    true
  do ->
    n = max
    while n > 0
      return n if is_left_truncatable n, f
      n -= 1
    throw Error "none found"

is_prime = (n) ->
  return false if n == 1
  return true if n == 2
  for d in [2..n]
    return false if n % d == 0
    return true if d * d >= n


console.log "right", max_right_truncatable_number(999999, is_prime)
console.log "left", max_left_truncatable_number(999999, is_prime)

```

output
<lang>
> coffee truncatable_prime.coffee
right 739399
left 998443

```



## Common Lisp


```lsip

(defun start ()
  (format t "Largest right-truncatable ~a~%" (max-right-truncatable))
  (format t "Largest left-truncatable  ~a~%" (max-left-truncatable)))

(defun max-right-truncatable ()
  (loop for el in (6-digits-R-truncatables)
        maximizing el into max
        finally (return max)))

(defun 6-digits-R-truncatables (&optional (lst '(2 3 5 7)) (n 5))
  (if (zerop n)
    lst
    (6-digits-R-truncatables (R-trunc lst) (- n 1))))

(defun R-trunc (lst)
  (remove-if (lambda (x) (not (primep x)))
	     (loop for el in lst
		   append (mapcar (lambda (x) (+ (* 10 el) x)) '(1 3 7 9)))))

(defun max-left-truncatable ()
  (loop for el in (6-digits-L-truncatables)
        maximizing el into max
        finally (return max)))

(defun 6-digits-L-truncatables (&optional (lst '(3 7)) (n 5))
  (if (zerop n)
    lst
    (6-digits-L-truncatables (L-trunc lst (- 6 n)) (- n 1))))

(defun L-trunc (lst n)
  (remove-if (lambda (x) (not (primep x)))
	     (loop for el in lst
		   append (mapcar (lambda (x) (+ (* (expt 10 n) x) el)) '(1 2 3 4 5 6 7 8 9)))))

(defun primep (n)
  (primep-aux n 2))

(defun primep-aux (n d)
  (cond ((> d (sqrt n)) t)
        ((zerop (rem n d)) nil)
        (t (primep-aux n (+ d 1)))))

```

```txt
Largest right-truncatable 739399
Largest left-truncatable  998443
```



## D


```d
import std.stdio, std.math, std.string, std.conv, std.algorithm,
       std.range;

bool isPrime(in int n) pure nothrow {
    if (n <= 1)
        return false;
    foreach (immutable i; 2 .. cast(int)sqrt(real(n)) + 1)
        if (!(n % i))
            return false;
    return true;
}

bool isTruncatablePrime(bool left)(in int n) pure {
    immutable s = n.text;
    if (s.canFind('0'))
        return false;
    foreach (immutable i; 0 .. s.length)
        static if (left) {
            if (!s[i .. $].to!int.isPrime)
                return false;
        } else {
            if (!s[0 .. i + 1].to!int.isPrime)
                return false;
        }
    return true;
}

void main() {
    enum n = 1_000_000;
    writeln("Largest left-truncatable prime in 2 .. ", n, ": ",
            iota(n, 1, -1).filter!(isTruncatablePrime!true).front);
    writeln("Largest right-truncatable prime in 2 .. ", n, ": ",
            iota(n, 1, -1).filter!(isTruncatablePrime!false).front);
}
```

```txt
Largest left-truncatable prime in 2 .. 1000000: 998443
Largest right-truncatable prime in 2 .. 1000000: 739399
```



## EchoLisp


```lisp

;; does p include a 0 in its decimal representation ?
(define (nozero? n) (= -1 (string-index (number->string n) "0")))

;; right truncate : p and successive quotients by 10 (integer division) must be primes
(define (right-trunc p) (unless (zero? p)
	(and (prime? p) (right-trunc (quotient p 10)))))
(remember 'right-trunc)

;; left truncate : p and successive modulo by 10, 100, .. must be prime
(define (left-trunc p (mod 1000000))
	(unless (< mod 1)
	(and (prime? p) (nozero? p) (left-trunc (modulo p mod) (/ mod 10)))))

;; start from 999999. stop on first found
(define (fact-trunc trunc)
(for ((p (in-range 999999 100000 -1))) #:break (when (trunc p) (writeln p) #t)))

```

Output:

```lisp

(fact-trunc left-trunc)
998443
(fact-trunc right-trunc)
739399

```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
		do
			io.put_string ("Largest right truncatable prime: " + find_right_truncatable_primes.out)
			io.new_line
			io.put_string ("Largest left truncatable prime: " + find_left_truncatable_primes.out)
		end

	find_right_truncatable_primes: INTEGER
			-- Largest right truncatable prime below 1000000.
		local
			i, maybe_prime: INTEGER
			found, is_one: BOOLEAN
		do
			from
				i := 999999
			until
				found
			loop
				is_one := True
				from
					maybe_prime := i
				until
					not is_one  or maybe_prime.out.count = 1
				loop
					if maybe_prime.out.has ('0') or maybe_prime.out.has ('2') or maybe_prime.out.has ('4') or maybe_prime.out.has ('6') or maybe_prime.out.has ('8') then
						is_one := False
					else
						if not is_prime (maybe_prime)  then
							is_one := False
						elseif is_prime (maybe_prime) and maybe_prime.out.count > 1 then
							maybe_prime := truncate_right (maybe_prime)
						end
					end
				end
				if is_one then
					found := True
					Result := i
				end
				i := i - 2
			end
		ensure
			Result_is_smaller: Result < 1000000
		end

	find_left_truncatable_primes: INTEGER
			-- Largest left truncatable prime below 1000000.
		local
			i, maybe_prime: INTEGER
			found, is_one: BOOLEAN
		do
			from
				i := 999999
			until
				found
			loop
				is_one := True
				from
					maybe_prime := i
				until
					not is_one or maybe_prime.out.count = 1
				loop
					if not is_prime (maybe_prime) then
						is_one := False
					elseif is_prime (maybe_prime) and maybe_prime.out.count > 1 then
						if maybe_prime.out.at (2) = '0' then
							is_one := False
						else
							maybe_prime := truncate_left (maybe_prime)
						end
					end
				end
				if is_one then
					found := True
					Result := i
				end
				i := i - 2
			end
		ensure
			Result_is_smaller: Result < 1000000
		end

feature {NONE}

	is_prime (n: INTEGER): BOOLEAN
			--Is 'n' a prime number?
		require
			positiv_input: n > 0
		local
			i: INTEGER
			max: REAL_64
			math: DOUBLE_MATH
		do
			create math
			if n = 2 then
				Result := True
			elseif n <= 1 or n \\ 2 = 0 then
				Result := False
			else
				Result := True
				max := math.sqrt (n)
				from
					i := 3
				until
					i > max
				loop
					if n \\ i = 0 then
						Result := False
					end
					i := i + 2
				end
			end
		end

	truncate_left (n: INTEGER): INTEGER
			-- 'n' truncated by one digit from the left side.
		require
			truncatable: n.out.count > 1
		local
			st: STRING
		do
			st := n.out
			st.remove_head (1)
			Result := st.to_integer
		ensure
			Result_truncated: Result.out.count = n.out.count - 1
		end

	truncate_right (n: INTEGER): INTEGER
			-- 'n' truncated by one digit from the right side.
		require
			truncatable: n.out.count > 1
		local
			st: STRING
		do
			st := n.out
			st.remove_tail (1)
			Result := st.to_integer
		ensure
			Result_truncated: Result.out.count = n.out.count - 1
		end

end

```

```txt

Largest right truncatable prime: 739399
Largest left truncatable prime: 999431

```



## Elena

ELENA 4.x :

```elena
import system'calendar;
import extensions;

const MAXN = 1000000;

extension mathOp
{
    isPrime()
    {
        int n := cast int(self);

        if (n < 2) { ^ false };
        if (n < 4) { ^ true };
        if (n mod:2 == 0) { ^ false };
        if (n < 9) { ^ true };
        if (n mod:3 == 0) { ^ false };

        int r := n.sqrt();
        int f := 5;
        while (f <= r)
        {
            if ((n.mod(f) == 0) || (n.mod(f + 2) == 0))
                { ^ false };

            f := f + 6
        };

        ^ true
    }

    isRightTruncatable()
    {
        int n := self;

        while (n != 0)
        {
            ifnot (n.isPrime())
                { ^ false };

            n := n / 10
        };

        ^ true
    }

    isLeftTruncatable()
    {
        int n := self;
        int tens := 1;

        while (tens < n)
            { tens := tens * 10 };

        while (n != 0)
        {
            ifnot (n.isPrime())
                { ^ false };

            tens := tens / 10;
            n := n - (n / tens * tens)
        };

        ^ true
    }
}

program()
{
    var n := MAXN;
    var max_lt := 0;
    var max_rt := 0;

    while (max_lt == 0 || max_rt == 0)
    {
        if(n.toString().indexOf("0") == -1)
        {
            if ((max_lt == 0) && (n.isLeftTruncatable()))
                {
                    max_lt := n
                };

            if ((max_rt == 0) && (n.isRightTruncatable()))
                {
                    max_rt := n
                }
        };

        n := n - 1
    };

    console.printLine("Largest truncable left is ",max_lt);
    console.printLine("Largest truncable right is ",max_rt);
}
```

```txt

Largest truncable left is 998443
Largest truncable right is 739399

```



## Elixir

```elixir
defmodule Prime do
  defp left_truncatable?(n, prime) do
    func = fn i when i<=9 -> 0
              i           -> to_string(i) |> String.slice(1..-1) |> String.to_integer end
    truncatable?(n, prime, func)
  end

  defp right_truncatable?(n, prime) do
    truncatable?(n, prime, fn i -> div(i, 10) end)
  end

  defp truncatable?(n, prime, trunc_func) do
    if to_string(n) |> String.match?(~r/0/),
      do:   false,
      else: trunc_loop(trunc_func.(n), prime, trunc_func)
  end

  defp trunc_loop(0, _prime, _trunc_func), do: true
  defp trunc_loop(n, prime, trunc_func) do
    if elem(prime,n), do: trunc_loop(trunc_func.(n), prime, trunc_func), else: false
  end

  def eratosthenes(limit) do            # descending order
    Enum.to_list(2..limit) |> sieve(:math.sqrt(limit), [])
  end

  defp sieve([h|_]=list, max, sieved) when h>max, do: Enum.reverse(list, sieved)
  defp sieve([h | t], max, sieved) do
    list = for x <- t, rem(x,h)>0, do: x
    sieve(list, max, [h | sieved])
  end

  defp prime_table(_, [], list), do: [false, false | list]
  defp prime_table(n, [n|t], list), do: prime_table(n-1, t,      [true|list])
  defp prime_table(n, prime, list), do: prime_table(n-1, prime, [false|list])

  def task(limit \\ 1000000) do
    prime = eratosthenes(limit)
    prime_tuple = prime_table(limit, prime, []) |> List.to_tuple
    left = Enum.find(prime, fn n -> left_truncatable?(n, prime_tuple) end)
    IO.puts "Largest left-truncatable prime : #{left}"
    right = Enum.find(prime, fn n -> right_truncatable?(n, prime_tuple) end)
    IO.puts "Largest right-truncatable prime: #{right}"
  end
end

Prime.task
```


```txt

Largest left-truncatable prime : 998443
Largest right-truncatable prime: 739399

```



## Factor

<lang>USING: formatting fry grouping.extras kernel literals math
math.parser math.primes sequences ;
IN: rosetta-code.truncatable-primes

CONSTANT: primes $[ 1,000,000 primes-upto reverse ]

: number>digits ( n -- B{} ) number>string string>digits ;

: no-zeros? ( seq -- ? ) [ zero? not ] all? ;

: all-prime? ( seq -- ? ) [ prime? ] all? ;

: truncate ( seq quot -- seq' ) call( seq -- seq' )
    [ 10 digits>integer ] map ;

: truncate-right ( seq -- seq' ) [ head-clump ] truncate ;

: truncate-left ( seq -- seq' ) [ tail-clump ] truncate ;

: truncatable-prime? ( n quot -- ? ) [ number>digits ] dip
    '[ @ all-prime? ] [ no-zeros? ] bi and ; inline

: right-truncatable-prime? ( n -- ? ) [ truncate-right ]
    truncatable-prime? ;

: left-truncatable-prime? ( n -- ? ) [ truncate-left ]
    truncatable-prime? ;

: find-truncatable-primes ( -- ltp rtp )
    primes [ [ left-truncatable-prime?  ] find nip ]
           [ [ right-truncatable-prime? ] find nip ] bi ;

: main ( -- ) find-truncatable-primes
    "Left: %d\nRight: %d\n" printf ;

MAIN: main
```

```txt

Left: 998443
Right: 739399

```



## Fortran

```fortran
module primes_mod
  implicit none

  logical, allocatable :: primes(:)

contains

subroutine Genprimes(parr)
  logical, intent(in out) :: parr(:)
  integer :: i
! Prime sieve
  parr = .true.
  parr (1) = .false.
  parr (4 : size(parr) : 2) = .false.
  do i = 3, int (sqrt (real (size(parr)))), 2
    if (parr(i)) parr(i * i : size(parr) : i) = .false.
  end do

end subroutine

function is_rtp(candidate)
  logical :: is_rtp
  integer, intent(in) :: candidate
  integer :: n

  is_rtp = .true.
  n = candidate / 10
  do while(n > 0)
    if(.not. primes(n)) then
      is_rtp = .false.
      return
    end if
    n = n / 10
  end do

end function

function is_ltp(candidate)
  logical :: is_ltp
  integer, intent(in) :: candidate
  integer :: i, n
  character(10) :: nstr

  write(nstr, "(i10)") candidate
  is_ltp = .true.
  do i = len_trim(nstr)-1, 1, -1
    n = mod(candidate, 10**i)
    if(.not. primes(n)) then
      is_ltp = .false.
      return
    end if
  end do
end function

end module primes_mod

program Truncatable_Primes
  use primes_mod
  implicit none

  integer, parameter :: limit = 999999
  integer :: i
  character(10) :: nstr

! Generate an array of prime flags up to limit of search
  allocate(primes(limit))
  call Genprimes(primes)

! Find left truncatable prime
  do i = limit, 1, -1
    write(nstr, "(i10)") i
    if(index(trim(nstr), "0") /= 0) cycle      ! check for 0 in number
    if(is_ltp(i)) then
      write(*, "(a, i0)") "Largest left truncatable prime below 1000000 is ", i
      exit
    end if
  end do

! Find right truncatable prime
  do i = limit, 1, -1
    write(nstr, "(i10)") i
    if(index(trim(nstr), "0") /= 0) cycle      ! check for 0 in number
    if(is_rtp(i)) then
      write(*, "(a, i0)") "Largest right truncatable prime below 1000000 is ", i
      exit
    end if
  end do
end program
```

Output

```txt
Largest left truncatable prime below 1000000 is 998443
Largest right truncatable prime below 1000000 is 739399
```



## FreeBASIC


### Version 1


```freebasic
' FB 1.05.0 Win64

Function isPrime(n As Integer) As Boolean
  If n Mod 2 = 0 Then Return n = 2
  If n Mod 3 = 0 Then Return n = 3
  Dim d As Integer = 5
  While d * d <= n
    If n Mod d = 0 Then Return False
    d += 2
    If n Mod d = 0 Then Return False
    d += 4
  Wend
  Return True
End Function

Dim As UInteger i, j, p, pow, lMax = 2, rMax = 2
Dim s As String

' largest left truncatable prime less than 1000000
' It can't end with 1, 4, 6, 8 or 9 as these numbers are not prime
' Nor can it end in 2 if it has more than one digit as such a number would divide by 2
For i = 3 To 999997 Step 2
  s = Str(i)
  If Instr(s, "0") > 1 Then Continue For '' cannot contain 0
  j = s[Len(s) - 1] - 48
  If j = 1 OrElse j = 9 Then Continue For
  p = i
  pow = 10 ^ (Len(s) - 1)
  While pow > 1
    If Not isPrime(p) Then Continue For
    p Mod= pow
    pow \= 10
  Wend
  lMax = i
Next

' largest right truncatable prime less than 1000000
' It can't begin with 1, 4, 6, 8 or 9 as these numbers are not prime
For i = 3 To 799999 Step 2
  s = Str(i)
  If Instr(s, "0") > 1 Then Continue For '' cannot contain 0
  j = s[0] - 48
  If j = 1 OrElse j = 4 OrElse j = 6 Then Continue For
  p = i
  While p > 0
    If Not isPrime(p) Then Continue For
    p \= 10
  Wend
  rMax = i
Next

Print "Largest left  truncatable prime : "; lMax
Print "Largest right truncatable prime : "; rMax
Print
Print "Press any key to quit"
Sleep
```


```txt

Largest left  truncatable prime : 998443
Largest right truncatable prime : 739399

```


### Version 2

Construct primes using previous found primes.

```freebasic
' version 10-12-2016
' compile with: fbc -s console

Dim Shared As Byte isPrime()

Sub sieve(m As UInteger)

    Dim As Integer i, j
    ReDim isPrime(m)

    For i = 4 To m Step 2
        isPrime(i) = 1
    Next

    For i = 3 To Sqr(m) Step 2
        If isPrime(i) = 0 Then
            For j = i * i To m Step i * 2
                isPrime(j) = 1
            Next
        End If
    Next

End Sub

' ------=< MAIN >=------

#Define max 1000000 'upto 2^30 max for 32bit OS

Dim As UInteger a(), lt_prime(5000), rt_prime(100)
Dim As UInteger i, j, j1, p1, p2, left_max, right_max

sieve(max)

' left truncatable primes
' if odd and ends with 3 or 7, never ends 1 or 9 (no prime
' never ends on a 2 or 5 and starts with 1 to 9
lt_prime(1) = 3 : lt_prime(2) = 7
p1 = 1 : p2 = 2

Do
    For i = 1 To 9
        j = Val( Str(i) + Str(lt_prime(p1)) )
        If j > max Then Exit Do
        If isPrime(j) = 0 Then ' if prime then add to the list
            p2 += 1
            lt_prime(p2) = j
            If Left_max < j Then left_max = j
        End If
    Next
    p1 += 1
Loop Until p1 > p2   ' no more numbers to process

' right truncatable prime
' start with 2, 3, 5 or 7 and end with 1, 3, 7 or 9
rt_prime(1) = 2 : rt_prime(2) = 3 : rt_prime(3) = 5 : rt_prime(4) = 7
p1 = 1 : p2 = 4
Dim As UInteger end_num(1 To 4) => {1, 3, 7, 9}

Do
    j1 = rt_prime(p1) * 10
    If j1 > max Then Exit Do
    For i = 1 To 4
        j = j1 + End_num(i)
        If isprime(j) = 0 Then  ' if prime then add to the list
            p2 += 1
            rt_prime(p2) = j
           ' If right_max < j Then right_max = j
        End If
    Next
    p1 += 1
Loop Until p1 > p2   ' no more numbers to process
' the last one added is the biggest
right_max = rt_prime(p2)

Print
Print "The biggest  left truncatable prime below"; max; " is "; left_max
Print "The biggest right truncatable prime below"; max; " is "; right_max

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt

The biggest  left truncatable prime below 1000000 is 998443
The biggest right truncatable prime below 1000000 is 739399
```



## Go


```go
package main

import "fmt"

func main() {
    sieve(1e6)
    if !search(6, 1e6, "left", func(n, pot int) int { return n % pot }) {
        panic("997?")
    }
    if !search(6, 1e6, "right", func(n, _ int) int { return n / 10 }) {
        panic("7393?")
    }
}

var c []bool

func sieve(ss int) {
    c = make([]bool, ss)
    c[1] = true
    for p := 2; ; {
        p2 := p * p
        if p2 >= ss {
            break
        }
        for i := p2; i < ss; i += p {
            c[i] = true
        }
        for {
            p++
            if !c[p] {
                break
            }
        }
    }
}

func search(digits, pot int, s string, truncFunc func(n, pot int) int) bool {
    n := pot - 1
    pot /= 10
smaller:
    for ; n >= pot; n -= 2 {
        for tn, tp := n, pot; tp > 0; tp /= 10 {
            if tn < tp || c[tn] {
                continue smaller
            }
            tn = truncFunc(tn, tp)
        }
        fmt.Println("max", s, "truncatable:", n)
        return true
    }
    if digits > 1 {
        return search(digits-1, pot, s, truncFunc)
    }
    return false
}
```

Output:

```txt

max left truncatable: 998443
max right truncatable: 739399

```



## Haskell

Using {{libheader|Primes}} from [http://hackage.haskell.org/packages/hackage.html HackageDB]

```haskell
import Data.Numbers.Primes(primes, isPrime)
import Data.List
import Control.Arrow

primes1e6 = reverse. filter (notElem '0'. show) $ takeWhile(<=1000000) primes

rightT, leftT :: Int -> Bool
rightT = all isPrime. takeWhile(>0). drop 1. iterate (`div`10)
leftT x = all isPrime. takeWhile(<x).map (x`mod`) $ iterate (*10) 10

main = do
  let (ltp, rtp) = (head. filter leftT &&& head. filter rightT) primes1e6
  putStrLn $ "Left truncatable  " ++ show ltp
  putStrLn $ "Right truncatable " ++ show rtp
```

Output:

```haskell
*Main> main
Left truncatable  998443
Right truncatable 739399
```


Interpretation of the J contribution:

```haskell
digits = [1..9] :: [Integer]
smallPrimes = filter isPrime digits
pow10 = iterate (*10) 1
mul10 = (pow10!!). length. show
righT = (+) . (10 *)
lefT = liftM2 (.) (+) ((*) . mul10)

primesTruncatable f = iterate (concatMap (filter isPrime.flip map digits. f)) smallPrimes
```

Output:

```haskell
*Main> maximum $ primesTruncatable righT !! 5
739399

*Main> maximum $ primesTruncatable lefT !! 5
998443
```


== {{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main(arglist)
   N := 0 < integer(\arglist[1]) | 1000000              # primes to generator 1 to ... (1M or 1st arglist)
   D := (0 < integer(\arglist[2]) | 10) / 2             # primes to display (10 or 2nd arglist)
   P := sieve(N)                                        # from sieve task (modified)
   write("There are ",*P," prime numbers in the range 1 to ",N)
   if *P <= 2*D then
      every writes( "Primes: "|!sort(P)||" "|"\n" )
   else
      every writes( "Primes: "|(L := sort(P))[1 to D]||" "|"... "|L[*L-D+1 to *L]||" "|"\n" )
   largesttruncateable(P)
end

procedure largesttruncateable(P)            #: find the largest left and right trucatable numbers in P
local ltp,rtp

   every x  := sort(P)[*P to 1 by -1] do    # largest to smallest
      if not find('0',x) then {
         /ltp  := islefttrunc(P,x)
         /rtp  := isrighttrunc(P,x)
         if \ltp & \rtp then break          # until both found
         }
   write("Largest left truncatable prime  = ", ltp)
   write("Largest right truncatable prime = ", rtp)
   return
end

procedure isrighttrunc(P,x) #: return integer x if x and all right truncations of x are in P or fails
if x = 0 | (member(P,x) & isrighttrunc(P,x / 10)) then return x
end

procedure islefttrunc(P,x) #: return integer x if x and all left truncations of x are in P or fails
if *x = 0 | ( (x := integer(x)) & member(P,x) & islefttrunc(P,x[2:0]) ) then return x
end
```


Sample output:
```txt
There are 78498 prime numbers in the range 1 to 1000000
Primes: 2 3 5 7 11 ... 999953 999959 999961 999979 999983
Largest left truncatable prime  = 998443
Largest right truncatable prime = 739399
```


== {{header|J}} ==

Truncatable primes may be constructed by starting with a set of one digit prime numbers and then repeatedly adding a non-zero digit (combine all possibilities of a truncatable prime digit sequence with each digit) and, at each step, selecting the prime numbers which result.

In other words, given:


```j
selPrime=: #~ 1&p:
seed=: selPrime digits=: 1+i.9
step=: selPrime@,@:(,&.":/&>)@{@;
```


Here, selPrime discards non-prime numbers from a list, so seed is the list 2 3 5 7.

The largest truncatable primes less than a million can be obtained by adding five digits to the prime seeds, then finding the largest value from the result.


```j
>
./ digits&step^:5 seed  NB. left truncatable
998443
   >./ step&digits^:5 seed  NB. right truncatable
739399
```


Note that we are using the same combining function and same basic procedure in both cases. The difference is which side of the number we add arbitrary digits to, for each step.


## Java


```Java
import java.util.BitSet;

public class Main {

	public static void main(String[] args){

		final int MAX = 1000000;

		//Sieve of Eratosthenes (using BitSet only for odd numbers)
		BitSet primeList = new BitSet(MAX>>1);
		primeList.set(0,primeList.size(),true);

		int sqroot = (int) Math.sqrt(MAX);
		primeList.clear(0);
		for(int num = 3; num <= sqroot; num+=2)
		{
			if( primeList.get(num >> 1) )
			{
				int inc = num << 1;
				for(int factor = num * num; factor < MAX; factor += inc)
				{
					//if( ((factor) & 1) == 1)
					//{
					primeList.clear(factor >> 1);
					//}
				}
			}
		}
		//Sieve ends...

		//Find Largest Truncatable Prime. (so we start from 1000000 - 1
		int rightTrunc = -1, leftTrunc = -1;
		for(int prime = (MAX - 1) | 1; prime >= 3; prime -= 2)
		{
			if(primeList.get(prime>>1))
			{
				//Already found Right Truncatable Prime?
				if(rightTrunc == -1)
				{
					int right = prime;
					while(right > 0 && right % 2 != 0 && primeList.get(right >> 1)) right /= 10;
					if(right == 0) rightTrunc = prime;
				}

				//Already found Left Truncatable Prime?
				if(leftTrunc == -1 )
				{
					//Left Truncation
					String left = Integer.toString(prime);
					if(!left.contains("0"))
					{
						while( left.length() > 0 ){
							int iLeft = Integer.parseInt(left);
							if(!primeList.get( iLeft >> 1)) break;
							left = left.substring(1);
						}
						if(left.length() == 0) leftTrunc = prime;
					}
				}
				if(leftTrunc != -1 && rightTrunc != -1) //Found both? then Stop loop
				{
					break;
				}
			}
		}
		System.out.println("Left  Truncatable : " + leftTrunc);
		System.out.println("Right Truncatable : " + rightTrunc);
	}
}

```

Output :

```txt

Left  Truncatable : 998443
Right Truncatable : 739399

```



## Julia

There are several features of Julia that make solving this task easy.  Julia has excellent built-in support for prime generation and testing.  The built-in mathematical functions <tt>prevpow</tt> and <tt>divrem</tt> are quite handy for implementing <tt>isltruncprime</tt>.

```Julia

function isltruncprime{T<:Integer}(n::T, base::T=10)
    isprime(n) || return false
    p = n
    f = prevpow(base, p)
    while 1 < f
        (d, p) = divrem(p, f)
        isprime(p) || return false
        d != 0 || return false
        f = div(f, base)
    end
    return true
end

function isrtruncprime{T<:Integer}(n::T, base::T=10)
    isprime(n) || return false
    p = n
    while base < p
        p = div(p, base)
        isprime(p) || return false
    end
    return true
end

hi = 10^6

for i in reverse(primes(hi))
    isltruncprime(i) || continue
    println("The largest  left truncatable prime ≤ ", hi, " is ", i, ".")
    break
end

for i in reverse(primes(hi))
    isrtruncprime(i) || continue
    println("The largest right truncatable prime ≤ ", hi, " is ", i, ".")
    break
end

```


```txt

The largest  left truncatable prime ≤ 1000000 is 998443.
The largest right truncatable prime ≤ 1000000 is 739399.

```



## Kotlin

```scala
//  version 1.0.5-2

fun isPrime(n: Int) : Boolean {
    if (n < 2) return false
    if (n % 2 == 0) return n == 2
    if (n % 3 == 0) return n == 3
    var d : Int = 5
    while (d * d <= n) {
        if (n % d == 0) return false
        d += 2
        if (n % d == 0) return false
        d += 4
    }
    return true
}

fun main(args: Array<String>) {
    var j: Char
    var p: Int
    var pow: Int
    var lMax: Int = 2
    var rMax: Int = 2
    var s: String

    // calculate maximum left truncatable prime less than 1 million
    loop@ for( i in 3..999997 step 2) {
        s = i.toString()
        if ('0' in s) continue
        j = s[s.length - 1]
        if (j == '1' || j == '9') continue
        p = i
        pow = 1
        for (k in 1..s.length - 1) pow *= 10
        while(pow > 1) {
            if (!isPrime(p)) continue@loop
            p %= pow
            pow /= 10
        }
        lMax = i
    }

    // calculate maximum right truncatable prime less than 1 million
    loop@ for( i in 3..799999 step 2) {
        s = i.toString()
        if ('0' in s) continue
        j = s[0]
        if (j == '1' || j == '4' || j == '6') continue
        p = i
        while(p > 0) {
            if (!isPrime(p)) continue@loop
            p /= 10
        }
        rMax = i
    }

    println("Largest left  truncatable prime : " + lMax.toString())
    println("Largest right truncatable prime : " + rMax.toString())
}
```


```txt

Largest left  truncatable prime : 998443
Largest right truncatable prime : 739399

```



## Lua


```lua
max_number = 1000000

numbers = {}
for i = 2, max_number do
    numbers[i] = i;
end

for i = 2, max_number do
    for j = i+1, max_number do
        if numbers[j] ~= 0 and j % i == 0 then numbers[j] = 0 end
    end
end

max_prime_left, max_prime_right = 2, 2
for i = 2, max_number do
    if numbers[i] ~= 0 then
        local is_prime = true

        local l = math.floor( i / 10 )
        while l > 1 do
            if numbers[l] == 0 then
                is_prime = false
                break
            end
            l = math.floor( l / 10 )
        end
        if is_prime then
            max_prime_left = i
        end

        is_prime = true
        local n = 10;
        while math.floor( i % 10 ) ~= 0 and n < max_number do
            if numbers[ math.floor( i % 10 ) ] ~= 0 then
                is_prime = false
                break
            end
            n = n * 10
        end
        if is_prime then
            max_prime_right = i
        end
    end
end

print( "max_prime_left = ", max_prime_left )
print( "max_prime_right = ", max_prime_right )
```




## Maple


```Maple

MaxTruncatablePrime := proc({left::truefalse:=FAIL, right::truefalse:=FAIL}, $)
local i, j, c, p, b, n, sdprimes, dir;
local tprimes := table();
    if left = true and right = true then
        error "invalid input";
    elif right = true then
        dir := "right";
    else
        dir := "left";
    end if;
    b := 10;
    n := 6;
    sdprimes := select(isprime, [seq(1..b-1)]);
    for p in sdprimes do
        if assigned(tprimes[p]) then
            next;
        end if;
        i := ilog[b](p)+1;
        j := 1;
        while p < b^n do
            if dir = "left" then
                c := j*b^i + p;
            else
                c := p*b + j;
            end if;
            if j >= b or c > b^n then # we have tried all 1 digit extensions of p, add p to tprimes and move back 1 digit
                tprimes[p] := p;
                if i = 1 then # if we are at the first digit,  go to the next 1 digit prime
                    break;
                end if;
                i := i - 1;
                j := 1;
                if dir = "left" then
                    p := p - iquo(p, b^i)*b^i;
                else
                    p := iquo(p, b);
                end if;
            elif assigned(tprimes[c]) then
                j := j + 1;
            elif isprime(c) then
                p := c;
                i := i + 1;
                j := 1;
            else
                j := j+1;
            end if;
        end do;
    end do;
    return max(indices(tprimes, 'nolist'));
end proc;
```


```txt


> MaxTruncatablePrime(right); MaxTruncatablePrime(left);
                             739399
                             998443

```





## Mathematica


```Mathematica
LeftTruncatablePrimeQ[n_] := Times @@ IntegerDigits[n] > 0 &&
  And @@ PrimeQ /@ ToExpression /@ StringJoin /@
      Rest[Most[NestList[Rest, #, Length[#]] &[Characters[ToString[n]]]]]
RightTruncatablePrimeQ[n_] := Times @@ IntegerDigits[n] > 0 &&
  And @@ PrimeQ /@ ToExpression /@ StringJoin /@
      Rest[Most[NestList[Most, #, Length[#]] &[Characters[ToString[n]]]]]
```

Example usage:

```txt
n = PrimePi[1000000]; While[Not[LeftTruncatablePrimeQ[Prime[n]]], n--]; Prime[n]
-> 998443

n = PrimePi[1000000]; While[Not[RightTruncatablePrimeQ[Prime[n]]], n--]; Prime[n]
-> 739399
```



## MATLAB

largestTruncatablePrimes.m:

```MATLAB
function largestTruncatablePrimes(boundary)

    %Helper function for checking if a prime is left of right truncatable
    function [leftTruncatable,rightTruncatable] = isTruncatable(prime,checkLeftTruncatable,checkRightTruncatable)

        numDigits = ceil(log10(prime)); %calculate the number of digits in the prime less one
        powersOfTen = 10.^(0:numDigits); %cache the needed powers of ten

        leftTruncated = mod(prime,powersOfTen); %generate a list of numbers by repeatedly left truncating the prime

        %leading zeros will cause duplicate entries thus it is possible to
        %detect leading zeros if we rotate the list to the left or right
        %and check for any equivalences with the original list
        hasLeadingZeros = any( circshift(leftTruncated,[0 1]) == leftTruncated );

        if( hasLeadingZeros || not(checkLeftTruncatable) )
            leftTruncatable = false;
        else
            %check if all of the left truncated numbers are prime
            leftTruncatable = all(isprime(leftTruncated(2:end)));
        end

        if( checkRightTruncatable )
            rightTruncated = (prime - leftTruncated) ./ powersOfTen; %generate a list of right truncated numbers
            rightTruncatable = all(isprime(rightTruncated(1:end-1))); %check if all the right truncated numbers are prime
        else
            rightTruncatable = false;
        end

    end %isTruncatable()

    nums = primes(boundary); %generate all primes <= boundary

    %Flags that indicate if the largest left or right truncatable prime has not
    %been found
    leftTruncateNotFound = true;
    rightTruncateNotFound = true;

    for prime = nums(end:-1:1) %Search through primes in reverse order

        %Get if the prime is left and/or right truncatable, ignoring
        %checking for right truncatable if it has already been found
        [leftTruncatable,rightTruncatable] = isTruncatable(prime,leftTruncateNotFound,rightTruncateNotFound);

        if( leftTruncateNotFound && leftTruncatable ) %print out largest left truncatable prime
            display([num2str(prime) ' is the largest left truncatable prime <= ' num2str(boundary) '.']);
            leftTruncateNotFound = false;
        end

        if( rightTruncateNotFound && rightTruncatable ) %print out largest right truncatable prime
            display([num2str(prime) ' is the largest right truncatable prime <= ' num2str(boundary) '.']);
            rightTruncateNotFound = false;
        end

        %Terminate loop when the largest left and right truncatable primes have
        %been found
        if( not(leftTruncateNotFound || rightTruncateNotFound) )
            break;
        end
    end
end

```

Solution for n = 1,000,000:

```MATLAB

>> largestTruncatablePrimes(1e6)
998443 is the largest left truncatable prime <= 1000000.
739399 is the largest right truncatable prime <= 1000000.

```



## Nim


```nim
import sets, strutils, algorithm

proc primes(n: int64): seq[int64] =
  result = @[]
  var multiples = initSet[int64]()
  for i in 2..n:
    if i notin multiples:
      result.add i
      for j in countup(i*i, n, i.int):
        multiples.incl j

proc truncatablePrime(n: int64): tuple[left: int64, right: int64] =
  var
    primelist: seq[string] = @[]
  for x in primes(n):
    primelist.add($x)
  reverse primelist
  var primeset = toSet primelist
  for n in primelist:
    var alltruncs = initSet[string]()
    for i in 0..n.len-1:
      alltruncs.incl n[i..n.high]
    if alltruncs <= primeset:
      result.left = parseInt(n)
      break
  for n in primelist:
    var alltruncs = initSet[string]()
    for i in 0..n.len-1:
      alltruncs.incl n[0..i]
    if alltruncs <= primeset:
      result.right = parseInt(n)
      break

echo truncatablePrime(1000000'i64)
```

Output:

```txt
(left: 998443, right: 739399)
```



## ooRexx


```ooRexx

-- find largest left- & right-truncatable primes < 1 million.
-- an initial set of primes (not, at this time, we leave out 2 because
-- we'll automatically skip the even numbers.  No point in doing a needless
-- test each time through
primes = .array~of(3, 5, 7, 11)

-- check all of the odd numbers up to 1,000,000
loop j = 13 by 2 to 1000000
  loop i = 1 to primes~size
      prime = primes[i]
      -- found an even prime divisor
      if j // prime == 0 then iterate j
      -- only check up to the square root
      if prime*prime > j then leave
  end
  -- we only get here if we don't find a divisor
  primes~append(j)
end

-- get a set of the primes that we can test more efficiently
primeSet = .set~of(2)
primeSet~putall(primes)


say 'The last prime is' primes[primes~last] "("primeSet~items 'primes under one million).'
say copies('-',66)

lastLeft = 0

-- we're going to use the array version to do these in order.  We're still
-- missing "2", but that's not going to be the largest
loop prime over primes

    -- values containing 0 can never work
    if prime~pos(0) \= 0 then iterate
    -- now start the truncations, checking against our set of
    -- known primes
    loop i = 1 for prime~length - 1
        subprime = prime~right(i)
        -- not in our known set, this can't work
        if \primeset~hasIndex(subprime) then iterate prime
    end
    -- this, by definition, with be the largest left-trunc prime
    lastLeft = prime
end
-- now look for right-trunc primes
lastRight = 0
loop prime over primes

    -- values containing 0 can never work
    if prime~pos(0) \= 0 then iterate
    -- now start the truncations, checking against our set of
    -- known primes
    loop i = 1 for prime~length - 1
        subprime = prime~left(i)
        -- not in our known set, this can't work
        if \primeset~hasIndex(subprime) then iterate prime
    end
    -- this, by definition, with be the largest left-trunc prime
    lastRight = prime
end

say 'The largest  left-truncatable prime is' lastLeft '(under one million).'
say 'The largest right-truncatable prime is' lastRight '(under one million).'


```

Output:

```txt

The last prime is 999983 (78498 primes under one million).
------------------------------------------------------------------
The largest  left-truncatable prime is 998443 (under one million).
The largest right-truncatable prime is 739399 (under one million).

```



## OpenEdge/Progress


```progress
FUNCTION isPrime RETURNS LOGICAL (
   i_i AS INT
):

   DEF VAR ii AS INT.

   DO ii = 2 TO SQRT( i_i ):

      IF i_i MODULO ii = 0 THEN
         RETURN FALSE.

   END.

   RETURN TRUE AND i_i > 1.

END FUNCTION. /* isPrime */

FUNCTION isLeftTruncatablePrime RETURNS LOGICAL (
   i_i AS INT
):

   DEF VAR ii        AS INT.
   DEF VAR cc        AS CHAR.
   DEF VAR lresult   AS LOGICAL INITIAL TRUE.

   cc = STRING( i_i ).

   DO WHILE cc > "":
      lresult = lresult AND isPrime( INTEGER( cc ) ).
      cc = SUBSTRING( cc, 2 ).
   END.

   RETURN lresult.

END FUNCTION. /* isLeftTruncatablePrime */

FUNCTION isRightTruncatablePrime RETURNS LOGICAL (
   i_i AS INT
):

   DEF VAR ii        AS INT.
   DEF VAR cc        AS CHAR.
   DEF VAR lresult   AS LOGICAL INITIAL TRUE.

   cc = STRING( i_i ).

   DO WHILE cc > "":
      lresult = lresult AND isPrime( INTEGER( cc ) ).
      cc = SUBSTRING( cc, 1, LENGTH( cc ) - 1 ).
   END.

   RETURN lresult.

END FUNCTION. /* isRightTruncatablePrime */

FUNCTION getHighestTruncatablePrimes RETURNS CHARACTER (
   i_imax AS INTEGER
):

   DEF VAR ii        AS INT.
   DEF VAR ileft     AS INT.
   DEF VAR iright    AS INT.

   DO ii = i_imax TO 1 BY -1 WHILE ileft = 0 OR iright = 0:

      IF INDEX( STRING( ii ), "0" ) = 0 THEN DO:
         IF ileft = 0 AND isLeftTruncatablePrime( ii ) THEN
            ileft = ii.
         IF iright = 0 AND isRightTruncatablePrime( ii ) THEN
            iright = ii.
      END.

   END.

   RETURN SUBSTITUTE("Left: &1~nRight: &2", ileft, iright ).

END FUNCTION. /* getHighestTruncatablePrimes */

MESSAGE
   getHighestTruncatablePrimes( 1000000 )
VIEW-AS ALERT-BOX.

```

Output:

```txt
---------------------------
Message
---------------------------
Left: 998443
Right: 739399
---------------------------
OK
---------------------------
```



## PARI/GP

This version builds the truncatable primes with up to k digits in a straightforward fashion. Run time is about 15 milliseconds, almost all of which is I/O.

```parigp
left(n)={
	my(v=[2,3,5,7],u,t=1,out=0);
	for(i=1,n,
		t*=10;
		u=[];
		for(j=1,#v,
			forstep(a=t,t*9,t,
				if(isprime(a+v[j]),u=concat(u,a+v[j]))
			)
		);
		out=v[#v];
		v=vecsort(u)
	);
	out
};
right(n)={
	my(v=[2,3,5,7],u,out=0);
	for(i=1,n,
		u=[];
		for(j=1,#v,
			forstep(a=1,9,[2,4],
				if(isprime(10*v[j]+a),u=concat(u,10*v[j]+a))
			)
		);
		out=v[#v];
		v=u
	);
	out
};
[left(6),right(6)]
```



## Perl

Typically with Perl we'll look for a CPAN module to make our life easier.  This basically just follows the task rules:
```perl
use ntheory ":all";
sub isltrunc {
  my $n = shift;
  return (is_prime($n) && $n !~ /0/ && ($n < 10 || isltrunc(substr($n,1))));
}
sub isrtrunc {
  my $n = shift;
  return (is_prime($n) && $n !~ /0/ && ($n < 10 || isrtrunc(substr($n,0,-1))));
}
for (reverse @{primes(1e6)}) {
  if (isltrunc($_)) { print "ltrunc: $_\n"; last; }
}
for (reverse @{primes(1e6)}) {
  if (isrtrunc($_)) { print "rtrunc: $_\n"; last; }
}
```

```txt
ltrunc: 998443
rtrunc: 739399
```

We can be a little more Perlish and build up n-digit lists then select the last one:

```perl
use ntheory ":all";

my @lprimes = my @rprimes = (2,3,5,7);

@lprimes = sort { $a <=> $b }
           map { my $p=$_; map { is_prime($_.$p) ? $_.$p : () } 1..9 } @lprimes
  for 2..6;

@rprimes = sort { $a <=> $b }
           map { my $p=$_; map { is_prime($p.$_) ? $p.$_ : () } 1..9 } @rprimes
  for 2..6;

print "ltrunc: $lprimes[-1]\nrtrunc: $rprimes[-1]\n";
```


Or we can do everything ourselves:

```perl
#!/usr/bin/perl
use warnings;
use strict;

use constant {
    LEFT  => 0,
    RIGHT => 1,
};

{   my @primes = (2, 3);

    sub is_prime {
        my $n = shift;
        return if $n < 2;

        for my $prime (@primes) {
            last if $prime >= $n;
            return unless $n % $prime;
        }

        my $sqrt = sqrt $n;
        while ($primes[-1] < $sqrt) {
            my $new = 2 + $primes[-1];
            $new += 2 until is_prime($new);
            push @primes, $new;
            return unless $n % $new;
        }

        return 1;
    }
}


sub trunc {
    my ($n, $side) = @_;
    substr $n, $side == LEFT ? 0 : -1, 1, q();
    return $n;
}


sub is_tprime { # Absence of zeroes is tested outside the sub.
    my ($n, $side) = @_;
    return (is_prime($n)
            and (1 == length $n or is_tprime(trunc($n, $side), $side)));
}


my $length = 6;
my @tprimes = ('9' x $length) x 2;
for my $side (LEFT, RIGHT) {
    $tprimes[$side] -= 2 until -1 == index $tprimes[$side], '0'
                               and is_tprime($tprimes[$side], $side);
}

print 'left ', join(', right ', @tprimes), "\n";
```

```txt
left 998443, right 739399
```



## Perl 6

```perl6
constant ltp = $[2, 3, 5, 7], -> @ltp {
    $[ grep { .&is-prime }, ((1..9) X~ @ltp) ]
} ... *;

constant rtp = $[2, 3, 5, 7], -> @rtp {
    $[ grep { .&is-prime }, (@rtp X~ (1..9)) ]
} ... *;

say "Highest ltp = ", ltp[5][*-1];
say "Highest rtp = ", rtp[5][*-1];
```

```txt
Highest ltp: 998443
Highest rtp: 739399
```



## Phix

A slightly different approach. Works up to N=8, quite fast - 10^8 in 5s with ~90% of time spent creating the basic sieve and ~10% propagation and final scan.

```Phix
constant N = 6, limit = power(10,N)
-- standard sieve:
enum L,R  -- (with primes[i] as mini bit-field)
sequence primes = repeat(L+R, limit)
primes[1] = 0
for i=2 to floor(sqrt(limit)) do
    if primes[i] then
        for k=i*i to limit by i do
            primes[k] = 0
        end for
    end if
end for

-- propagate non-truncateables up the prime table:
for p=1 to N-1 do
    integer p10 = power(10,p)       -- ie 10, 100, .. 100_000
    for i=p10+1 to p10*10-1 by 2 do -- to 99, 999, .. 999_999
        if primes[i] then
            integer l = remainder(i,p10),
                    r = floor(i/10)
            integer pi = and_bits(primes[l],L)+and_bits(primes[r],R)
            if pi and find('0',sprint(i)) then pi = 0 end if
            primes[i] = pi
        end if
    end for
end for

integer maxl=0, maxr=0

for i=limit-1 to 1 by -2 do
    integer pi = primes[i]
    if pi then
        if maxl=0 and and_bits(pi,L) then maxl = i end if
        if maxr=0 and and_bits(pi,R) then maxr = i end if
        if maxl!=0 and maxr!=0 then exit end if
    end if
end for
?{maxl,maxr}
```

```txt

{998443,739399}

```



## PicoLisp


```PicoLisp
(load "@lib/rsa.l")  # Use the 'prime?' function from RSA package

(de truncatablePrime? (N Fun)
   (for (L (chop N) L (Fun L))
      (T (= "0" (car L)))
      (NIL (prime? (format L)))
      T ) )

(let (Left 1000000  Right 1000000)
   (until (truncatablePrime? (dec 'Left) cdr))
   (until (truncatablePrime? (dec 'Right) '((L) (cdr (rot L)))))
   (cons Left Right) )
```

Output:

```txt
-> (998443 . 739399)
```



## PL/I


```PL/I

tp: procedure options (main);
    declare primes(1000000) bit (1);
    declare max_primes fixed binary (31);
    declare (i, k) fixed binary (31);

    max_primes = hbound(primes, 1);
    call sieve;

   /* Now search for primes that are right-truncatable. */
   call right_truncatable;

   /* Now search for primes that are left-truncatable. */
   call left_truncatable;

right_truncatable: procedure;
   declare direction bit (1);
   declare (i, k) fixed binary (31);

test_truncatable:
   do i = max_primes to 2 by -1;
      if primes(i) then /* it's a prime */
         do;
            k = i/10;
            do while (k > 0);
               if ^primes(k) then iterate test_truncatable;
               k = k/10;
            end;
            put skip list (i || ' is right-truncatable');
            return;
         end;
   end;
end right_truncatable;

left_truncatable: procedure;
   declare direction bit (1);
   declare (i, k, d, e) fixed binary (31);

test_truncatable:
   do i = max_primes to 2 by -1;
      if primes(i) then /* it's a prime */
         do;
            k = i;
            do d = 100000 repeat d/10 until (d = 10);
               e = k/d;
               k = k - e*d;
               if e = 0 then iterate test_truncatable;
               if ^primes(k) then iterate test_truncatable;
            end;
            put skip list (i || ' is left-truncatable');
            return;
         end;
   end;
end left_truncatable;

sieve: procedure;
   declare (i, j) fixed binary (31);

   primes = '1'b; primes(1) = '0'b;

   do i = 2 to sqrt(max_primes);
      do j = i+i to max_primes by i;
         primes(j) = '0'b;
      end;
   end;
end sieve;

end tp;

```


```txt

        739399 is right-truncatable
        998443 is left-truncatable

```



## PowerShell


```PowerShell
function IsPrime ( [int] $num )
{
    $isprime = @{}
    2..[math]::sqrt($num) | Where-Object {
        $isprime[$_] -eq $null } | ForEach-Object {
        $_
        $isprime[$_] = $true
        for ( $i=$_*$_ ; $i -le $num; $i += $_ )
        { $isprime[$i] = $false }
    }
    2..$num | Where-Object { $isprime[$_] -eq $null }
}

function Truncatable ( [int] $num )
{
    $declen = [math]::abs($num).ToString().Length
    $primes = @()
    $ltprimes = @{}
    $rtprimes = @{}
    1..$declen | ForEach-Object { $ltprimes[$_]=@{}; $rtprimes[$_]=@{} }
    IsPrime $num | ForEach-Object {
        $lastltprime = 2
        $lastrtprime = 2
    } {
        $curprim = $_
        $curdeclen = $curprim.ToString().Length
        $primes += $curprim
        if( $curdeclen -eq 1 ) {
            $ltprimes[1][$curprim] = $true
            $rtprimes[1][$curprim] = $true
            $lastltprime = $curprim
            $lastrtprime = $curprim
        } else {
            $curmod = $curprim % [math]::pow(10,$curdeclen - 1)
            $curdiv = [math]::floor($curprim / 10)
            if( $ltprimes[$curdeclen - 1][[int]$curmod] ) {
                $ltprimes[$curdeclen][$curprim] = $true
                $lastltprime = $curprim
            }
            if( $rtprimes[$curdeclen - 1][[int]$curdiv] ) {
                $rtprimes[$curdeclen][$curprim] = $true
                $lastrtprime = $curprim
            }
        }
        if( ( $ltprimes[$curdeclen - 2].Keys.count -gt 0 ) -and ( $ltprimes[$curdeclen - 1].Keys.count -gt 0 ) ) { $ltprimes[$curdeclen -2] = @{} }
        if( ( $rtprimes[$curdeclen - 2].Keys.count -gt 0 ) -and ( $rtprimes[$curdeclen - 1].Keys.count -gt 0 ) ) { $rtprimes[$curdeclen -2] = @{} }
    } {
        "Largest Left Truncatable Prime: $lastltprime"
        "Largest Right Truncatable Prime: $lastrtprime"
    }
}
```



## PureBasic


```PureBasic
#MaxLim = 999999

Procedure is_Prime(n)
  If     n<=1 : ProcedureReturn #False
  ElseIf n<4  : ProcedureReturn #True
  ElseIf n%2=0: ProcedureReturn #False
  ElseIf n<9  : ProcedureReturn #True
  ElseIf n%3=0: ProcedureReturn #False
  Else
    Protected r=Round(Sqr(n),#PB_Round_Down)
    Protected f=5
    While f<=r
      If n%f=0 Or n%(f+2)=0
        ProcedureReturn #False
      EndIf
      f+6
    Wend
  EndIf
  ProcedureReturn #True
EndProcedure

Procedure TruncateLeft(n)
  Protected s.s=Str(n), l=Len(s)-1
  If Not FindString(s,"0",1)
    While l>0
      s=Right(s,l)
      If Not is_Prime(Val(s))
        ProcedureReturn #False
      EndIf
      l-1
    Wend
    ProcedureReturn #True
  EndIf
EndProcedure

Procedure TruncateRight(a)
  Repeat
    a/10
    If Not a
      Break
    ElseIf Not is_Prime(a) Or a%10=0
      ProcedureReturn #False
    EndIf
  ForEver
  ProcedureReturn #True
EndProcedure

i=#MaxLim
Repeat
  If is_Prime(i)
    If Not truncateleft And TruncateLeft(i)
      truncateleft=i
    EndIf
    If Not truncateright And TruncateRight(i)
      truncateright=i
    EndIf
  EndIf
  If truncateleft And truncateright
    Break
  Else
    i-2
  EndIf
Until i<=0

x.s="Largest TruncateLeft= "+Str(truncateleft)
y.s="Largest TruncateRight= "+Str(truncateright)

MessageRequester("Truncatable primes",x+#CRLF$+y)
```


== {{header|Python}} ==

```python
maxprime = 1000000

def primes(n):
    multiples = set()
    prime = []
    for i in range(2, n+1):
        if i not in multiples:
            prime.append(i)
            multiples.update(set(range(i*i, n+1, i)))
    return prime

def truncatableprime(n):
    'Return a longest left and right truncatable primes below n'
    primelist = [str(x) for x in primes(n)[::-1]]
    primeset = set(primelist)
    for n in primelist:
        # n = 'abc'; [n[i:] for i in range(len(n))] -> ['abc', 'bc', 'c']
        alltruncs = set(n[i:] for i in range(len(n)))
        if alltruncs.issubset(primeset):
            truncateleft = int(n)
            break
    for n in primelist:
        # n = 'abc'; [n[:i+1] for i in range(len(n))] -> ['a', 'ab', 'abc']
        alltruncs = set([n[:i+1] for i in range(len(n))])
        if alltruncs.issubset(primeset):
            truncateright = int(n)
            break
    return truncateleft, truncateright

print(truncatableprime(maxprime))
```


'''Sample Output'''

```txt
(998443, 739399)
```



## Racket



```racket

#lang racket
(require math/number-theory)

(define (truncate-right n)
  (quotient n 10))

(define (truncate-left n)
  (define s (number->string n))
  (string->number (substring s 1 (string-length s))))

(define (contains-zero? n)
  (member #\0 (string->list (number->string n))))

(define (truncatable? truncate n)
  (and (prime? n)
       (not (contains-zero? n))
       (or (< n 10)
           (truncatable? truncate (truncate n)))))

; largest left truncatable prime
(for/first ([n (in-range 1000000 1 -1)]
            #:when (truncatable? truncate-left n))
  n)

; largest right truncatable prime
(for/first ([n (in-range 1000000 1 -1)]
            #:when (truncatable? truncate-right n))
  n)

; Output:
998443
739399

```



## REXX

Extra code was added to the prime number generator as this is the section of the REXX program that consumes the vast majority of the computation time.

```REXX
/*REXX program finds largest  left─ and right─truncatable  primes ≤ 1m  (or argument 1).*/
parse arg high .;    if high==''  then high=1000000        /*Not specified?  Then use 1m*/
!.=0;   w=length(high)                           /*placeholders for primes;  max width. */
@.1=2;  @.2=3;  @.3=5;  @.4=7;  @.5=11;  @.6=13;  @.7=17   /*define some low primes.    */
!.2=1;  !.3=1;  !.5=1;  !.7=1;  !.11=1;  !.13=1;  !.17=1   /*set some low prime flags.  */
#=7;    s.#=@.#**2                               /*number of primes so far;     prime². */
                                                 /* [↓]  generate more  primes  ≤  high.*/
   do j=@.#+2  by 2  for max(0, high%2-@.#%2-1)  /*only find odd primes from here on out*/
                        if j// 3==0 then iterate /*is J divisible by three?             */
   parse var j '' -1 _; if     _==5 then iterate /* " "     "      " five? (right digit)*/
                        if j// 7==0 then iterate /* " "     "      " seven?             */
                        if j//11==0 then iterate /* " "     "      " eleven?            */
                        if j//13==0 then iterate /* " "     "      " thirteen?          */
                                                 /* [↑]  the above five lines saves time*/
          do k=7  while s.k<=j                   /* [↓]  divide by the known odd primes.*/
          if j//@.k==0  then iterate j           /*Is J ÷  X?  Then not prime.    ___   */
          end   /*k*/                            /* [↑]  only process up to the  √ J    */
   #=#+1                                         /*bump the number of primes found.     */
   @.#=j;      s.#=j*j;     !.j=1                /*assign next prime;  prime²;  prime #.*/
   end         /*j*/
                                                 /* [↓]  find largest left truncatable P*/
  do L=#  by -1  for #;    digs=length(@.L)      /*search from top end;  get the length.*/
        do k=1  for digs;  _=right(@.L, k)       /*validate all left truncatable primes.*/
        if \!._  then iterate L                  /*Truncated number not prime?  Skip it.*/
        end   /*k*/
  leave                                          /*egress, found left truncatable prime.*/
  end         /*L*/
                                                 /* [↓]  find largest right truncated P.*/
  do R=#  by -1  for #;    digs=length(@.R)      /*search from top end;  get the length.*/
        do k=1  for digs;  _=left(@.R, k)        /*validate all right truncatable primes*/
        if \!._  then iterate R                  /*Truncated number not prime?  Skip it.*/
        end   /*k*/
  leave                                          /*egress, found right truncatable prime*/
  end         /*R*/
                                                 /* [↓]  show largest left/right trunc P*/
say 'The last prime found is '   @.#    " (there are"   #   'primes ≤'  high")."
say copies('─', 70)                              /*show a separator line for the output.*/
say 'The largest  left─truncatable prime ≤'        high        " is "       right(@.L, w)
say 'The largest right─truncatable prime ≤'        high        " is "       right(@.R, w)
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''   when using the default input:

```txt

The last prime found is  999983  (there are 78498 primes ≤ 1000000).
──────────────────────────────────────────────────────────────────────
The largest  left─truncatable prime ≤ 1000000  is   998443
The largest right─truncatable prime ≤ 1000000  is   739399

```



## Ring


```ring

# Project : Truncatable primes

for n = 1000000 to 1 step -1
    flag = 1
    flag2 = 1
    strn = string(n)
    for nr = 1 to len(strn)
        if strn[nr] = "0"
           flag2 = 0
        ok
    next
    if flag2 = 1
       for m = 1 to len(strn)
           strp = right(strn, m)
           if isprime(number(strp))
           else
              flag = 0
              exit
           ok
       next
       if flag = 1
          nend = n
          exit
       ok
    ok
next
see "Largest left truncatable prime : " + nend + nl

for n = 1000000 to 1 step -1
    flag = 1
    strn = string(n)
    for m = 1 to len(strn)
        strp = left(strn, len(strn) - m + 1)
        if isprime(number(strp))
        else
           flag = 0
           exit
        ok
    next
    if flag = 1
       nend = n
       exit
    ok
next
see "Largest right truncatable prime : " + nend + nl

func isprime num
     if (num <= 1) return 0 ok
     if (num % 2 = 0 and num != 2) return 0 ok
     for i = 3 to floor(num / 2) -1 step 2
         if (num % i = 0) return 0 ok
     next
     return 1

```

Output:

```txt

Largest left  truncatable prime : 998443
Largest right truncatable prime : 739399

```



## Ruby


```ruby
def left_truncatable?(n)
  truncatable?(n) {|i| i.to_s[1..-1].to_i}
end


def right_truncatable?(n)
  truncatable?(n) {|i| i/10}
end

def truncatable?(n, &trunc_func)
  return false if n.to_s.include? "0"
  loop do
    n = trunc_func.call(n)
    return true if n.zero?
    return false unless Prime.prime?(n)
  end
end

require 'prime'
primes = Prime.each(1_000_000).to_a.reverse

p primes.detect {|p| left_truncatable? p}
p primes.detect {|p| right_truncatable? p}
```


returns

```txt
998443
739399
```



### An Alternative Approach

Setting BASE to 10 and MAX to 6 in the Ruby example [[Find largest left truncatable prime in a given base|here]] Produces:

```txt

The largest left truncatable prime less than 1000000 in base 10 is 998443

```



## Scala

This example uses lazily evaluated lists. The functions to determine if a number is a truncatable prime construct a list of truncated numbers and check that all the elements in the list are prime.

```scala
object TruncatablePrimes {
  def main(args: Array[String]): Unit = {
    val max = 1000000

    println(
      s"""|ltPrime: ${ltPrimes.takeWhile(_ <= max).last}
          |rtPrime: ${rtPrimes.takeWhile(_ <= max).last}
          |""".stripMargin)
  }

  def ltPrimes: LazyList[Int] = 2 #:: LazyList.from(3, 2).filter(isLeftTruncPrime)
  def rtPrimes: LazyList[Int] = 2 #:: LazyList.from(3, 2).filter(isRightTruncPrime)

  def isPrime(num: Int): Boolean = (num > 1) && !LazyList.range(3, math.sqrt(num).toInt + 1, 2).exists(num%_ == 0)
  def isLeftTruncPrime(num: Int): Boolean = !num.toString.contains('0') && Iterator.unfold(num.toString){str => if(str.nonEmpty) Some((str.toInt, str.tail)) else None}.forall(isPrime)
  def isRightTruncPrime(num: Int): Boolean = !num.toString.exists(_.asDigit%2 == 0) && Iterator.unfold(num.toString){str => if(str.nonEmpty) Some((str.toInt, str.init)) else None}.forall(isPrime)
}
```


```txt
ltPrime: 998443
rtPrime: 739399
```



## Sidef


```ruby
func t_prime(n, left=true) {
    var p = %w(2 3 5 7);
    var f = (
        left ? { '1'..'9' ~X+ p }
             : { p ~X+ '1'..'9' }
    )
    n.times {
        p = f().grep{ .to_i.is_prime }
    }
    p.map{.to_i}.max
}

say t_prime(5, left: true)
say t_prime(5, left: false)
```

```txt

998443
739399

```



## Tcl


```tcl
package require Tcl 8.5

# Optimized version of the Sieve-of-Eratosthenes task solution
proc sieve n {
    set primes [list]
    if {$n < 2} {return $primes}
    set nums [dict create]
    for {set i 2} {$i <= $n} {incr i} {
        dict set nums $i ""
    }
    set next 2
    set limit [expr {sqrt($n)}]
    while {$next <= $limit} {
        for {set i $next} {$i <= $n} {incr i $next} {dict unset nums $i}
        lappend primes $next
	dict for {next -} $nums break
    }
    return [concat $primes [dict keys $nums]]
}

proc isLeftTruncatable n {
    global isPrime
    while {[string length $n] > 0} {
	if {![info exist isPrime($n)]} {
	    return false
	}
	set n [string range $n 1 end]
    }
    return true
}
proc isRightTruncatable n {
    global isPrime
    while {[string length $n] > 0} {
	if {![info exist isPrime($n)]} {
	    return false
	}
	set n [string range $n 0 end-1]
    }
    return true
}

# Demo code
set limit 1000000
puts "calculating primes up to $limit"
set primes [sieve $limit]
puts "search space contains [llength $primes] members"
foreach p $primes {
    set isPrime($p) "yes"
}
set primes [lreverse $primes]

puts "searching for largest left-truncatable prime"
foreach p $primes {
    if {[isLeftTruncatable $p]} {
	puts FOUND:$p
	break
    }
}

puts "searching for largest right-truncatable prime"
foreach p $primes {
    if {[isRightTruncatable $p]} {
	puts FOUND:$p
	break
    }
}
```

Output:

```txt

calculating primes up to 1000000
search space contains 78498 members
searching for largest left-truncatable prime
FOUND:998443
searching for largest right-truncatable prime
FOUND:739399

```



## VBScript


```vb

start_time = Now

lt = 0
rt = 0

For h = 1 To 1000000
	If IsLeftTruncatable(h) And h > lt Then
		lt = h
	End If
	If IsRightTruncatable(h) And h > rt Then
		rt = h
	End If
Next

end_time = now

WScript.StdOut.WriteLine "Largest LTP from 1..1000000: " & lt
WScript.StdOut.WriteLine "Largest RTP from 1..1000000: " & rt
WScript.StdOut.WriteLine "Elapse Time(seconds)       : " & DateDiff("s",start_time,end_time)

'------------
Function IsLeftTruncatable(n)
	IsLeftTruncatable = False
	c = 0
	For i = Len(n) To 1 Step -1
		If InStr(1,n,"0") > 0 Then
			Exit For
		End If
		If IsPrime(Right(n,i)) Then
			c = c + 1
		End If
	Next
	If c = Len(n) Then
		IsLeftTruncatable = True
	End If
End Function

Function IsRightTruncatable(n)
	IsRightTruncatable = False
	c = 0
	For i = Len(n) To 1 Step -1
		If InStr(1,n,"0") > 0 Then
			Exit For
		End If
		If IsPrime(Left(n,i)) Then
			c = c + 1
		End If
	Next
	If c = Len(n) Then
		IsRightTruncatable = True
	End If
End Function

Function IsPrime(n)
	If n = 2 Then
		IsPrime = True
	ElseIf n <= 1 Or n Mod 2 = 0 Then
		IsPrime = False
	Else
		IsPrime = True
		For i = 3 To Int(Sqr(n)) Step 2
			If n Mod i = 0 Then
				IsPrime = False
				Exit For
			End If
		Next
	End If
End Function

```


```txt

Largest LTP from 1..1000000: 998443
Largest RTP from 1..1000000: 739399
Elapse Time(seconds)       : 49

```



## XPL0


```XPL0
code CrLf=9, IntOut=11;

func Prime(P);          \Return true if P is a prime number
int  P;                 \(1 is not prime, but 2 is, etc.)
int  I;
[if P<=1 then return false;     \negative numbers are not prime
for I:= 2 to sqrt(P) do
        if rem(P/I) = 0 then return false;
return true;
];

func RightTrunc(N);     \Return largest right-truncatable prime < one million
int N;
int M;
[for N:= 1_000_000-1 downto 2 do
        [M:= N;
        loop    [if not Prime(M) then quit;
                M:= M/10;
                if rem(0) = 0 then quit;        \no zeros allowed
                if M=0 then return N;
                ];
        ];
];

func LeftTrunc(N);      \Return largest left-truncatable prime < one million
int N;
int M, P;
[for N:= 1_000_000-1 downto 2 do
        [M:= N;
        P:=100_000;
        loop    [if not Prime(M) then quit;
                M:= rem(M/P);
                P:= P/10;
                if M<P then quit;               \no zeros allowed
                if M=0 then return N;
                ];
        ];
];

[IntOut(0, LeftTrunc);  CrLf(0);
 IntOut(0, RightTrunc); CrLf(0);
]
```


Output:

```txt

998443
739399

```



## zkl

Using [[Extensible prime generator#zkl]] and a one meg bucket of bytes, construct a yes/no lookup table for all primes <= one million (<80,000).

```zkl
const million=0d1_000_000;

var pTable=Data(million+1,Int).fill(0);	// actually bytes, all zero
primes:=Utils.Generator(Import("sieve").postponed_sieve);
while((p:=primes.next())<million){ pTable[p]=1; }

fcn rightTrunc(n){
   while(n){ if(not pTable[n]) return(False); n/=10; }
   True
}
fcn leftTrunc(n){  // 999,907 is not allowed
   ns:=n.toString(); if (ns.holds("0")) return(False);
   while(ns){ if(not pTable[ns]) return(False); ns=ns[1,*]; }
   True
}
```


```zkl
[million..0,-1].filter1(rightTrunc):
   "%,d is a right truncatable prime".fmt(_).println();
[million..0,-1].filter1(leftTrunc):
   "%,d is a left truncatable prime".fmt(_).println();
```

```txt

739,399 is a right truncatable prime
998,443 is a left truncatable prime

```

