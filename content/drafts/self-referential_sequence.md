+++
title = "Self-referential sequence"
description = ""
date = 2019-10-21T03:52:34Z
aliases = []
[extra]
id = 10366
[taxonomies]
categories = []
tags = []
+++

{{task}}
There are several ways to generate a self-referential sequence. One very common one (the [[Look-and-say sequence]]) is to start with a positive integer, then generate the next term by concatenating enumerated groups of adjacent alike digits:
        0, 10, 1110, 3110, 132110, 1113122110, 311311222110 ...
The terms generated grow in length geometrically and never converge.

Another way to generate a self-referential sequence is to summarize the previous term.

Count how many of each alike digit there is, then concatenate the sum and digit for each of the sorted enumerated digits. Note that the first five terms are the same as for the previous sequence.
        0, 10, 1110, 3110, 132110, 13123110, 23124110 ... 
Sort the digits largest to smallest. Do not include counts of digits that do not appear in the previous term.

Depending on the seed value, series generated this way always either converge to a stable value or to a short cyclical pattern. (For our purposes, I'll use converge to mean an element matches a previously seen element.) The sequence shown, with a seed value of 0, converges to a stable value of 1433223110 after 11 iterations. The seed value that converges most quickly is 22. It goes stable after the first element. (The next element is 22, which has been seen before.)


;Task:
Find all the positive integer seed values under 1000000, for the above convergent self-referential sequence, that takes the largest number of iterations before converging. Then print out the number of iterations and the sequence they return. Note that different permutations of the digits of the seed will yield the same sequence. For this task, assume leading zeros are not permitted.  


```txt
Seed Value(s): 9009 9090 9900

Iterations: 21 

Sequence: (same for all three seeds except for first element)
9009
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110

```


;Related tasks:
*   [[Fours is the number of letters in the ...]]
*   [[Look-and-say sequence]]
*   [[Number names]]
*   [[Self-describing numbers]]
*   [[Spelling of ordinal numbers]]


;Also see:
*   [[oeis:A036058|The On-Line Encyclopedia of Integer Sequences]].





## Ada


```Ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
procedure SelfRef is
   subtype Seed is Natural range 0 .. 1_000_000;
   subtype Num is Natural range 0 .. 10;
   type NumList is array (0 .. 10) of Num;
   package IO is new Ada.Text_IO.Integer_IO (Natural);
   package DVect is new Ada.Containers.Vectors (Positive, NumList);

   function Init (innum : Seed) return NumList is
      list : NumList := (others => 0);
      number : Seed := innum;  d : Num;
   begin
      loop
         d := Num (number mod 10);
         list (d) :=  list (d) + 1;
         number := number / 10; exit when number = 0;
      end loop; return list;
   end Init;

   procedure Next (inoutlist : in out NumList) is
      list : NumList := (others => 0);
   begin
      for i in list'Range loop
         if inoutlist (i) /= 0 then
            list (i) := list (i) + 1;
            list (inoutlist (i)) := list (inoutlist (i)) + 1;
         end if;
      end loop; inoutlist := list;
   end Next;

   procedure Show (list : NumList) is begin
      for i in reverse list'Range loop
         if list (i) > 0 then
            IO.Put (list (i), Width => 1); IO.Put (i, Width => 1);
         end if;
      end loop; New_Line;
   end Show;

   function Iterate (theseed : Seed; p : Boolean) return Natural is
      list : NumList := Init (theseed);
      vect : DVect.Vector;
   begin
      vect.Append (list);
      loop
         if p then Show (list); end if;
         Next (list); exit when vect.Contains (list); vect.Append (list);
      end loop;
      return Integer (DVect.Length (vect)) + 1;
   end Iterate;

   mseed : Seed;
   len, maxlen : Natural := 0;
begin
   for i in Seed'Range loop
      len := Iterate (i, False);
      if len > maxlen then mseed := i; maxlen := len; end if;
   end loop;
   IO.Put (maxlen, Width => 1); Put_Line (" Iterations:");
   IO.Put (mseed, Width => 1); New_Line;
   len := Iterate (mseed, True);
end SelfRef;
```

{{out}}

```txt
21 Iterations:
9009
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110
```



## Aime

{{trans|C}}

```aime
text
next(text s)
{
    integer c, e, l;
    index v;
    data d;

    l = ~s;
    while (l) {
        v[-s[l -= 1]] += 1;
    }

    for (c, e in v) {
        b_form(d, "%d%c", e, -c);
    }

    return d;
}

integer
depth(text s, integer i, record r)
{
    integer d;

    d = 0;
    r_j_integer(d, r, s);
    if (d <= 0) {
        i += 1;
        d += d ? i : -i;
        r[s] = d;
        i = depth(next(s), i, r);
        d = r[s];
        if (d <= 0) {
            r[s] = d = i + 1;
        }
    }

    return d;
}

integer
main(void)
{
    integer d, e, i;
    record r;
    list l;

    d = 0;
    i = 1000000;
    while (i) {
        i -= 1;
        e = depth(itoa(i), 0, r);
        if (e == d) {
            lb_p_integer(l, i);
        } elif (d < e) {
            d = e;
            l_clear(l);
            lb_p_integer(l, i);
        }
    }

    o_("longest length is ", d, "\n");
    while (l_o_integer(i, l, 0)) {
        text s;

        o_("\n", i, "\n");
        e = d - 1;
        s = itoa(i);
        while (e) {
            o_(s = next(s), "\n");
            e -= 1;
        }
    }

    return 0;
}
```

{{out}}

```txt
longest length is 21

9900
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110

9090
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110

9009
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110

```



## AutoHotkey

Not optimized in the slightest.

```AutoHotkey

; The following directives and commands speed up execution:
#NoEnv
SetBatchlines -1
ListLines Off
Process, Priority,, high

iterations := 0, seed := "Seeds: "

Loop 1000000
	If (newIterations := CountSubString(list := ListSequence(A_Index), "`n")) > iterations
		iterations := newiterations
		,final := "`nIterations: " iterations+1 "`nSequence:`n`n" A_Index "`n" list
		,seed := A_Index " "
	else if (newIterations = iterations)
		seed .= A_Index " "
MsgBox % "Seeds: " . seed . final
ListSequence(seed){
	While !InStr("`n" . out, "`n" (d:=Describe(seed)) "`n")
		out .= d "`n", seed := d
	return out
}

Describe(n){
	Loop 10
		If (t:=CountSubString(n, 10-A_Index))
			out .= t . (10-A_Index)
	return out
}

CountSubstring(fullstring, substring){
   StringReplace, junk, fullstring, %substring%, , UseErrorLevel
   return errorlevel
}

```

Output:

```txt
Seeds: 9009 9090 9900 
Iterations: 21
Sequence:

9009
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110
```


## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      *FLOAT64
      DIM list$(30)
      maxiter% = 0
      maxseed% = 0
      FOR seed% = 0 TO 999999
        list$(0) = STR$(seed%)
        iter% = 0
        REPEAT
          list$(iter%+1) = FNseq(list$(iter%))
          IF VALlist$(iter%+1) <= VALlist$(iter%) THEN
            FOR try% = iter% TO 0 STEP -1
              IF list$(iter%+1) = list$(try%) EXIT REPEAT
            NEXT
          ENDIF
          iter% += 1
        UNTIL FALSE
        IF iter% >= maxiter% THEN
          IF iter% > maxiter% CLS
          maxiter% = iter%
          maxseed% = seed%
          PRINT "Seed " ;seed% " has "; iter% " iterations"
        ENDIF
      NEXT
      PRINT '"Sequence:"
      number$ = STR$(maxseed%)
      FOR i% = 1 TO maxiter%
        PRINT number$
        number$ = FNseq(number$)
      NEXT
      END
      
      DEF FNseq(n$)
      LOCAL I%, o$, d%()
      DIM d%(9)
      FOR I% = 1 TO LEN(n$)
        d%(ASCMID$(n$,I%)-&30) += 1
      NEXT
      FOR I% = 9 TO 0 STEP -1
        IF d%(I%) o$ += STR$d%(I%) + STR$I%
      NEXT
      = o$
```

'''Output:'''

```txt

Seed 9009 has 21 iterations
Seed 9090 has 21 iterations
Seed 9900 has 21 iterations

Sequence:
9900
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110

```



## Bracmat


```bracmat
( ( self-referential
  =   seq N next
    .   ( next
        =   R S d f
          .   0:?S
            &   whl
              ' (@(!arg:%@?d ?arg)&(.!d)+!S:?S)
            & :?R
            &   whl
              ' ( !S:#?f*(.?d)+?S
                & !f !d !R:?R
                )
            & str$!R
        )
      & 1:?N
      & !arg:?seq
      &   whl
        ' ( next$!arg:?arg
          & ~(!seq:? !arg ?)
          & !arg !seq:?seq
          & 1+!N:?N
          )
      & (!seq.!N)
  )
& ( Perm
  =   permutations S p
    .   :?permutations
      & ( perm
        =   prefix List original A Z p
          .     !arg:(?prefix.)
              & str$!prefix:?p
              & (!S:?+(.!p)+?|(.!p)+!S:?S)
            | !arg:(0 ?.?)&
            |   !arg:(?prefix.?List:?original)
              &   whl
                ' ( @(!List:%?A ?Z)
                  & perm$(!prefix !A.!Z)
                  & str$(!Z !A):~!original:?List
                  )
        )
      & 0:?S
      & perm$(.!arg)
      & :?permutations
      &   whl
        ' ( !S:?*(.?p)+?S
          & !p !permutations:?permutations
          )
      & !permutations
  )
& -1:?i:?max
& :?seqs
&   whl
  ' ( 1+!i:<1000000:?i
    & ( @(!i:? %@?a >%@!a ?)
      |   self-referential$!i
        : ( ?seq
          .   ( >!max:?max&:?seqs
              | !max
              )
            &     ( "Seed Value(s):" Perm$!i
                  .   "Sequence: (same for all three seeds except for first element)
"
                      !seq
                  )
                  !seqs
              : ?seqs
          )
      | 
      )
    )
& out$("Iterations:" !max !seqs)
);
```

Output:

```txt
  Iterations:
  21
  ( Seed Value(s): 9900 9090 9009
  .   Sequence: (same for all three seeds except for first element)

      19182716152413228110
      19281716151413427110
      19281716151423228110
      29181716151413328110
      19182716151423129110
      19181716151413327110
      191726151423128110
      191716151413326110
      1916251423127110
      1916151413325110
      19251413226110
      19151423125110
      191433125110
      191413323110
      1923224110
      1923123110
      19323110
      19222110
      192210
      2920
      9900
  )
```



## C


```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>

typedef struct rec_t rec_t;
struct rec_t {
	int depth;
	rec_t * p[10];
};

rec_t root = {0, {0}};

#define USE_POOL_ALLOC
#ifdef USE_POOL_ALLOC /* not all that big a deal */
rec_t *tail = 0, *head = 0;
#define POOL_SIZE (1 << 20)
inline rec_t *new_rec()
{
	if (head == tail) {
		head = calloc(sizeof(rec_t), POOL_SIZE);
		tail = head + POOL_SIZE;
	}
	return head++;
}
#else
#define new_rec() calloc(sizeof(rec_t), 1)
#endif

rec_t *find_rec(char *s)
{
	int i;
	rec_t *r = &root;
	while (*s) {
		i = *s++ - '0';
		if (!r->p[i]) r->p[i] = new_rec();
		r = r->p[i];
	}
	return r;
}

/* speed up number to string conversion */
char number[100][4];
void init()
{
	int i;
	for (i = 0; i < 100; i++)
		sprintf(number[i], "%d", i);
}

void count(char *buf)
{
	int i, c[10] = {0};
	char *s;

	for (s = buf; *s; c[*s++ - '0']++);

	for (i = 9; i >= 0; i--) {
		if (!c[i]) continue;
		s = number[c[i]];

		*buf++ = s[0];
		if ((*buf = s[1])) buf++;

		*buf++ = i + '0';
	}

	*buf = '\0';
}

int depth(char *in, int d)
{
	rec_t *r = find_rec(in);

	if (r->depth > 0)
		return r->depth;

	d++;
	if (!r->depth)	r->depth = -d;
	else		r->depth += d;

	count(in);
	d = depth(in, d);

	if (r->depth <= 0) r->depth = d + 1;
	return r->depth;
}

int main(void)
{
	char a[100];
	int i, d, best_len = 0, n_best = 0;
	int best_ints[32];
	rec_t *r;

	init();

	for (i = 0; i < 1000000; i++) {
		sprintf(a, "%d", i);
		d = depth(a, 0);

		if (d < best_len) continue;
		if (d > best_len) {
			n_best = 0;
			best_len = d;
		}
		if (d == best_len)
			best_ints[n_best++] = i;
	}

	printf("longest length: %d\n", best_len);
	for (i = 0; i < n_best; i++) {
		printf("%d\n", best_ints[i]);
		sprintf(a, "%d", best_ints[i]);
		for (d = 0; d <= best_len; d++) {
			r = find_rec(a);
			printf("%3d: %s\n", r->depth, a);
			count(a);
		}
		putchar('\n');
	}

	return 0;
}
```

{{out}}

```txt
longest length: 21
9009
 21: 9009
 20: 2920
 19: 192210
 18: 19222110
 17: 19323110
 16: 1923123110
 15: 1923224110
 14: 191413323110
 13: 191433125110
 12: 19151423125110
 11: 19251413226110
 10: 1916151413325110
  9: 1916251423127110
  8: 191716151413326110
  7: 191726151423128110
  6: 19181716151413327110
  5: 19182716151423129110
  4: 29181716151413328110
  3: 19281716151423228110
  2: 19281716151413427110
  2: 19182716152413228110
  2: 19281716151413427110

9090
 21: 9090
 20: 2920
 19: 192210
 18: 19222110
 17: 19323110
 16: 1923123110
 15: 1923224110
 14: 191413323110
 13: 191433125110
 12: 19151423125110
 11: 19251413226110
 10: 1916151413325110
  9: 1916251423127110
  8: 191716151413326110
  7: 191726151423128110
  6: 19181716151413327110
  5: 19182716151423129110
  4: 29181716151413328110
  3: 19281716151423228110
  2: 19281716151413427110
  2: 19182716152413228110
  2: 19281716151413427110

9900
 21: 9900
 20: 2920
 19: 192210
 18: 19222110
 17: 19323110
 16: 1923123110
 15: 1923224110
 14: 191413323110
 13: 191433125110
 12: 19151423125110
 11: 19251413226110
 10: 1916151413325110
  9: 1916251423127110
  8: 191716151413326110
  7: 191726151423128110
  6: 19181716151413327110
  5: 19182716151423129110
  4: 29181716151413328110
  3: 19281716151423228110
  2: 19281716151413427110
  2: 19182716152413228110
  2: 19281716151413427110

```



## C++


```cpp

#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <algorithm>

std::map<char, int> _map;
std::vector<std::string> _result;
size_t longest = 0;

void make_sequence( std::string n ) {
    _map.clear();
    for( std::string::iterator i = n.begin(); i != n.end(); i++ )
        _map.insert( std::make_pair( *i, _map[*i]++ ) );

    std::string z;
    for( std::map<char, int>::reverse_iterator i = _map.rbegin(); i != _map.rend(); i++ ) {
        char c = ( *i ).second + 48;
        z.append( 1, c );
        z.append( 1, i->first );
    }

    if( longest <= z.length() ) {
        longest = z.length();
        if( std::find( _result.begin(), _result.end(), z ) == _result.end() ) {
            _result.push_back( z );
            make_sequence( z );
        }
    }
}
int main( int argc, char* argv[] ) {
    std::vector<std::string> tests;
    tests.push_back( "9900" ); tests.push_back( "9090" ); tests.push_back( "9009" );
    for( std::vector<std::string>::iterator i = tests.begin(); i != tests.end(); i++ ) {
        make_sequence( *i );
        std::cout  << "[" << *i << "] Iterations: " << _result.size() + 1 << "\n";
        for( std::vector<std::string>::iterator j = _result.begin(); j != _result.end(); j++ ) {
            std::cout << *j << "\n";
        }
        std::cout << "\n\n";
    }
    return 0;
}

```

{{out}}

```txt

[9900] Iterations: 21
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110

[9090] Iterations: 21
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110

[9009] Iterations: 21
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110



```



## CoffeeScript

{{incomplete|CoffeeScript|This code only produces one of the seeds, not all of them.}}
This takes less than a second to run, even though the only real optimization is to exclude integers that don't have their digits descending.


```coffeescript

sequence = (n) ->
  cnts = {}
  for c in n.toString()
    d = parseInt(c)
    incr cnts, d

  seq = []
  while true
    s = ''
    for i in [9..0]
      s += "#{cnts[i]}#{i}" if cnts[i]
    if s in seq
      break
    seq.push s
  
    new_cnts = {}
    for digit, cnt of cnts
      incr new_cnts, cnt
      incr new_cnts, digit
    cnts = new_cnts
  seq

incr = (h, k) ->
  h[k] ?= 0
  h[k] += 1
  
descending = (n) ->
  return true if n < 10
  tens = n / 10
  return false if n % 10 > tens % 10
  descending(tens)
  
max_len = 0
for i in [1..1000000]
  if descending(i)
    seq = sequence(i)
    if seq.length > max_len
      max_len = seq.length
      max_seq = seq
      max_i = i

console.log max_i, max_seq


```



```txt
 9900 ["2920", "192210", "19222110", "19323110", "1923123110", "1923224110", "191413323110", 
"191433125110", "19151423125110", "19251413226110", "1916151413325110", "1916251423127110", "1
91716151413326110", "191726151423128110", "19181716151413327110", "19182716151423129110", 
"29181716151413328110", "19281716151423228110", "19281716151413427110", "19182716152413228110"]
```



## Clojure


```clojure
(defmacro reduce-with
  "simplifies form of reduce calls"
  [bindings & body]
  (assert (and (vector? bindings) (= 4 (count bindings))))
  (let [[acc init, item sequence] bindings]
    `(reduce (fn [~acc ~item] ~@body) ~init ~sequence)))

(defn digits
  "maps e.g. 2345 => [2 3 4 5]"
  [n] (->> n str seq (map #(- (int %) (int \0))) vec))

(defn dcount
  "handles case (probably impossible in this range) of digit count > 9"
  [ds] (let [c (count ds)] (if (< c 10) c (digits c))))

(defn summarize-prev
  "produces the summary sequence for a digit sequence"
  [ds]
  (->> ds (sort >) (partition-by identity) (map (juxt dcount first)) flatten vec)

(defn convergent-sequence
  "iterates summarize-prev until a duplicate is found; returns summary step sequence"
  [ds]
  (reduce-with [cur-seq [], ds (iterate summarize-prev ds)]
    (if (some #{ds} cur-seq)
      (reduced cur-seq)
      (conj cur-seq ds))))

(defn candidate-seq
  "only try an already sorted digit sequence, so we only try equivalent seeds once; 
   e.g. 23 => []; 32 => (convergent-sequence [3 2])"
  [n]
  (let [ds (digits n)]
    (if (apply >= ds) (convergent-sequence ds) [])))

(defn find-longest
  "the meat of the task; returns summary step sequence(s) of max length within the range"
  [limit]
  (reduce-with [max-seqs [[]], new-seq (map candidate-seq (range 1 limit))]
    (let [cmp (compare (-> max-seqs first count) (count new-seq))]
      (cond
        (pos? cmp) max-seqs
        (neg? cmp) [new-seq]
        (zero? cmp) (conj max-seqs new-seq)))))

(def results (find-longest 1000000))
```


The above code saves a lot of time by only calculating summary step sequences for one
of a set of equivalent seeds: e.g. it only calculates for 4321, not for all 24 digit
permutations 1234, 1243, 1324,.... So for output this creates a some extra work to
reconstitute the permuted digit sequences for the result seed(s).
Clojure doesn't have a standard permutations
function (though there's one in the contributed library ''clojure.math.combinations''),
but the one here will serve.


```clojure
(defn perms
  "produce all the permutations of a finite sequence"
  [ds]
  (if (empty? ds)
    []
    (let [rotseq (for [n (range (count ds))] (concat (drop n ds) (take n ds)))]
      (reduce-with [rs [], [[d & ds]] rotseq]
        (concat rs (if (empty? ds) [[d]] (map #(cons d %) (perms ds))))))))

(doseq [result results]
  (let [seed (first result)
        seeds (->> seed perms (map vec) set sort (remove (comp zero? first)))]
    (apply println "Seed value(s):" (map #(apply str %) seeds)))))
  (println)
  (println "Iterations:" (count result))
  (println)
  (println "Sequence:")
  (doseq [ds result]
    (println (apply str ds))))
```



## Common Lisp

Doesn't do cache, and takes forever.

```lisp
(defun count-and-say (str)
   (let* ((s (sort (map 'list #'identity str) #'char>))
	  (out (list (first s) 0)))
     (loop for x in s do
	   (if (char= x (first out))
	     (incf (second out))
	     (setf out (nconc (list x 1) out))))
     (format nil "~{~a~^~}" (nreverse out))))

(defun ref-seq-len (n &optional doprint)
  (let ((s (format nil "~d" n)) hist)
    (loop (push s hist)
	  (if doprint (format t "~a~%" s))
	  (setf s (count-and-say s))
	  (loop for item in hist
		for i from 0 to 2 do
		(if (string= s item) (return-from ref-seq-len (length hist)))))))

(defun find-longest (top)
  (let (nums (len 0))
  (dotimes (x top)
    (let ((l (ref-seq-len x)))
      (if (> l len) (setf len l nums nil))
      (if (= l len) (push x nums))))
  (list nums len)))

(let ((r (find-longest 1000000)))
  (format t "Longest: ~a~%" r)
  (ref-seq-len (first (first r)) t))
```
output<lang>Longest: ((9900 9090 9009) 21)
9900
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110
```



## D

===Slow High-level Version===
{{trans|Ruby}}

```d
import std.stdio, std.algorithm, std.conv;

string[] selfReferentialSeq(string n, string[] seen=[]) nothrow {
    __gshared static string[][string] cache;
    if (n in cache)
        return cache[n];
    if (seen.canFind(n))
        return [];

    int[10] digit_count;
    foreach (immutable d; n)
        digit_count[d - '0']++;
    string term;
    foreach_reverse (immutable d; 0 .. 10)
        if (digit_count[d] > 0)
            term ~= text(digit_count[d], d);
    return cache[n] = [n] ~ selfReferentialSeq(term, [n] ~ seen);
}

void main() {
    enum int limit = 1_000_000;
    int max_len;
    int[] max_vals;

    foreach (immutable n; 1 .. limit) {
        const seq = n.text().selfReferentialSeq();
        if (seq.length > max_len) {
            max_len = seq.length;
            max_vals = [n];
        } else if (seq.length == max_len)
            max_vals ~= n;
    }

    writeln("values: ", max_vals);
    writeln("iterations: ", max_len);
    writeln("sequence:");
    foreach (const idx, const val; max_vals[0].text.selfReferentialSeq)
        writefln("%2d %s", idx + 1, val);
}
```

{{out}}

```txt
values: [9009, 9090, 9900]
iterations: 21
sequence:
 1 9009
 2 2920
 3 192210
 4 19222110
 5 19323110
 6 1923123110
 7 1923224110
 8 191413323110
 9 191433125110
10 19151423125110
11 19251413226110
12 1916151413325110
13 1916251423127110
14 191716151413326110
15 191726151423128110
16 19181716151413327110
17 19182716151423129110
18 29181716151413328110
19 19281716151423228110
20 19281716151413427110
21 19182716152413228110
```


### More Efficient Version

{{trans|Python}}

```d
import std.range, std.algorithm;

struct Permutations(bool doCopy=true, T) {
    T[] items;
    int r;
    bool stopped;
    int[] indices, cycles;
    static if (!doCopy)
        T[] result;

    this(T)(T[] items, int r=-1) pure nothrow @safe {
        this.items = items;
        immutable int n = items.length;
        if (r < 0)
            r = n;
        this.r = r;
        immutable n_minus_r = n - r;
        if (n_minus_r < 0) {
            this.stopped = true;
        } else {
            this.stopped = false;
            this.indices = n.iota.array;
            //this.cycles = iota(n, n_minus_r, -1).array; // Not nothrow.
            this.cycles = iota(n_minus_r + 1, n + 1).retro.array;
        }

        static if (!doCopy)
            result = new T[r];
    }

    @property bool empty() const pure nothrow @safe @nogc {
        return this.stopped;
    }

    static if (doCopy) {
        @property T[] front() const pure nothrow @safe {
            assert(!this.stopped);
            auto result = new T[r];
            foreach (immutable i, ref re; result)
                re = items[indices[i]];
            return result;
        }
    } else {
        @property T[] front() pure nothrow @safe @nogc {
            assert(!this.stopped);
            foreach (immutable i, ref re; this.result)
                re = items[indices[i]];
            return this.result;
        }
    }

    void popFront() pure nothrow /*@safe*/ @nogc {
        assert(!this.stopped);
        int i = r - 1;
        while (i >= 0) {
            immutable int j = cycles[i] - 1;
            if (j > 0) {
                cycles[i] = j;
                indices[i].swap(indices[$ - j]);
                return;
            }
            cycles[i] = indices.length - i;
            immutable int n1 = indices.length - 1;
            assert(n1 >= 0);
            immutable int num = indices[i];

            // copy isn't @safe.
            indices[i + 1 .. n1 + 1].copy(indices[i .. n1]);
            indices[n1] = num;
            i--;
        }

        this.stopped = true;
    }
}

Permutations!(doCopy, T) permutations(bool doCopy=true, T)
                                     (T[] items, int r=-1)
pure nothrow @safe {
    return Permutations!(doCopy, T)(items, r);
}

// ---------------------------------

import std.stdio, std.typecons, std.conv, std.algorithm, std.array,
       std.exception, std.string;

enum maxIters = 1_000_000;

string A036058(in string ns) pure nothrow @safe {
    return ns.representation.group.map!(t => t[1].text ~ char(t[0])).join;
}

int A036058_length(bool doPrint=false)(string numberString="0") {
    int iterations = 1;
    int queueIndex;
    string[3] lastThree;

    while (true) {
        static if (doPrint)
            writefln("  %2d %s", iterations, numberString);

        numberString = numberString
                       .dup
                       .representation
                       .sort()
                       .release
                       .assumeUTF;

        if (lastThree[].canFind(numberString))
            break;
        assert(iterations < maxIters);
        lastThree[queueIndex] = numberString;
        numberString = numberString.A036058;
        iterations++;
        queueIndex++;
        queueIndex %= 3;
    }

    return iterations;
}

Tuple!(int,int[]) max_A036058_length(R)(R startRange = 11.iota) {
    bool[string] alreadyDone;
    auto max_len = tuple(-1, (int[]).init);

    foreach (n; startRange) {
        immutable sns = n
                        .to!(char[])
                        .representation
                        .sort()
                        .release
                        .assumeUTF;

        if (sns !in alreadyDone) {
            alreadyDone[sns] = true;
            const size = sns.A036058_length;
            if (size > max_len[0])
                max_len = tuple(size, [n]);
            else if (size == max_len[0])
                max_len[1] ~= n;
        }
    }
    return max_len;
}

void main() {
    //const (lenMax, starts) = maxIters.iota.max_A036058_length;
    const lenMax_starts = maxIters.iota.max_A036058_length;
    immutable lenMax = lenMax_starts[0];
    const starts = lenMax_starts[1];

    // Expand:
    int[] allStarts;
    foreach (immutable n; starts) {
        bool[string] set;
        foreach (const k; permutations!false(n.to!(char[]), 4))
            if (k[0] != '0')
                set[k.idup] = true;
        //allStarts ~= set.byKey.to!(int[]);
        allStarts ~= set.byKey.map!(to!int).array;
    }

    allStarts = allStarts.sort().filter!(x => x < maxIters).array;

    writefln("The longest length, followed by the number(s) with the
longest sequence length for starting sequence numbers below maxIters
are:
Iterations = %d and sequence-starts = %s.", lenMax, allStarts);

    writeln("Note that only the first of any sequences with the same
digits is printed below. (The others will differ only in their first
term).");

    foreach (immutable n; starts) {
        writeln;
        A036058_length!true(n.text);
    }
}
```

The output is similar to the Python entry.

===Faster Low-level Version===
{{trans|C}}
From the C version, with a memory pool for a faster tree allocation.

```d
import core.stdc.stdio, core.stdc.stdlib;

struct MemoryPool(T, int MAX_BLOCK_BYTES = 1 << 17) {
    static assert(!is(T == class),
                  "MemoryPool is designed for native data.");
    static assert(MAX_BLOCK_BYTES >= 1,
                  "MemoryPool: MAX_BLOCK_BYTES must be >= 1 bytes.");
    static assert(MAX_BLOCK_BYTES >= T.sizeof,
                  "MemoryPool: MAX_BLOCK_BYTES must be" ~
                  " bigger than a T.");
    static if (T.sizeof * 5 > MAX_BLOCK_BYTES)
        pragma(msg, "MemoryPool: Block is very small.");

    alias Block = T[MAX_BLOCK_BYTES / T.sizeof];
    static __gshared Block*[] blocks;
    static __gshared T* nextFree, lastFree;

    static T* newItem() nothrow {
        if (nextFree >= lastFree) {
            blocks ~= cast(Block*)calloc(1, Block.sizeof);
            if (blocks[$ - 1] == null)
                exit(1);
            nextFree = blocks[$ - 1].ptr;
            lastFree = nextFree + Block.length;
        }

        return nextFree++;
    }

//    static void freeAll() nothrow {
//        foreach (blockPtr; blocks)
//            free(blockPtr);
//        blocks.length = 0;
//        nextFree = null;
//        lastFree = null;
//    }
}

struct Rec { // Tree node
    int length;
    Rec*[10] p;
}

__gshared int nNodes;
__gshared Rec* rec_root;
__gshared MemoryPool!Rec recPool;

Rec* findRec(char* s, Rec* root) nothrow {
    int c;
    Rec* next;

    while (true) {
        c = *s;
        s++;
        if (!c)
            break;
        c -= '0';
        next = root.p[c];
        if (!next) {
            nNodes++;
            next = recPool.newItem;
            root.p[c] = next;
        }
        root = next;
    }
    return root;
}

void nextNum(char* s) nothrow @nogc {
    int[10] cnt;
    for (int i = 0; s[i]; i++)
        cnt[s[i] - '0']++;

    foreach_reverse (i; 0 .. 10) {
        if (!cnt[i])
            continue;
        s += sprintf(s, "%d%c", cnt[i], i + '0');
    }
}

int getLen(char* s, int depth) nothrow {
    auto r = findRec(s, rec_root);
    if (r.length > 0)
        return r.length;

    depth++;
    if (!r.length)
        r.length = -depth;
    else
        r.length += depth;

    nextNum(s);
    depth = 1 + getLen(s, depth);

    if (r.length <= 0)
        r.length = depth;
    return r.length;
}

void main() nothrow {
    enum MAXN = 1_000_000;

    int[100] longest;
    int nLongest, ml;
    char[32] buf;
    rec_root = recPool.newItem();

    foreach (immutable i; 0 .. MAXN) {
        sprintf(buf.ptr, "%d", i);
        int l = getLen(buf.ptr, 0);
        if (l < ml)
            continue;
        if (l > ml) {
            nLongest = 0;
            ml = l;
        }
        longest[nLongest] = i;
        nLongest++;
    }

    printf("seq leng: %d\n\n", ml);
    foreach (immutable i; 0 .. nLongest) {
        sprintf(buf.ptr, "%d", longest[i]);
        // print len+1 so we know repeating starts from when
        foreach (immutable l; 0 .. ml + 1) {
            printf("%2d: %s\n", getLen(buf.ptr, 0), buf.ptr);
            nextNum(buf.ptr);
        }
        printf("\n");
    }

    printf("Allocated %d Rec tree nodes.\n", nNodes);
    //recPool.freeAll;
}
```

Faster than the C entry, run-time is about 1.16 seconds using the dmd compiler (about 1.5 without memory pool). Same output as the C entry.


## EchoLisp

Extra credit: searching up to 1e+10 does not find a longer sequence.


```scheme

(lib 'hash)
(lib 'list) ;; permutations

(define H (make-hash))

;; G R A P H
;; generate 'normalized' starter vectors  D[i] = number of digits 'i' (0 <=i < 10)
;; reduce graph size : 9009, 9900 ..  will be generated once : vector #(2 0 0 0 0 0 0 0 0 2)

(define (generate D dstart ndigits (sd 0)) 
(when (> ndigits 0)
	(set! sd (vector-ref D dstart)) ;; save before recurse
	(for ((i (in-range 0 (1+ ndigits))))
		#:continue (and ( = i 0) (> dstart 0))
		(vector-set! D dstart i)
		(sequence D) ;; sequence length from D
		(for ((j (in-range (1+ dstart) 10)))
		(generate D j  (- ndigits i))))
	(vector-set! D dstart sd))) ;; restore
	

;; compute follower of D (at most 99 same digits)
(define (dnext D (dd 0))
	(define N (make-vector 10))
	(for ((d D) (i 10))
		#:continue (zero? d)
		(vector-set! N i (1+ (vector-ref N i)))
		(if (< d 10)
		(vector-set! N d (1+ (vector-ref N d))) ;; d < 9
		(begin
		(set! dd (modulo d 10))
		(vector-set! N dd (1+ (vector-ref N dd))) 
		(set! dd (quotient d 10))
		(vector-set! N dd (1+ (vector-ref N dd))))))  
		N)
		
		
;; update all nodes in same sequence
;; seq-length (next D) = 1 - seq-length(D)
(define (sequence D)
(define (dists D d)
	(unless (hash-ref H D)
		  (hash-set H  D d)
		  (dists (dnext D ) (1- d))))
		  
		(unless  (hash-ref H D)
				 (dists D (sequence-length D))))		
		
;; sequence length from D
;; stops on loop found (node N)
(define (sequence-length D )
(define (looper N looplg depth) ;; looper 2 : a b a 
	(when ( > depth 0)
			(hash-set H N looplg)
			(looper (dnext N)  looplg (1- depth))))
			
		(define followers (make-hash))
		(define N (dnext D))
		(define seqlg 0)
		(define looplg 0)
		
		(hash-set followers D 0)
		
	(set! seqlg
		(for ((lg (in-naturals 1 )))
			 #:break (hash-ref H N) =>  (+ lg (hash-ref H N)) ;; already known
			 #:break (hash-ref followers N) => lg ;; loop found
			 (hash-set followers N lg)
			 (set! N (dnext N))))
			 
;; update nodes in loop : same seq-length
		(when (hash-ref followers N) ;; loop found
			(set! looplg  ( - seqlg  (hash-ref followers N)))
			(looper N looplg looplg))
		
		seqlg )
		
;; O U T P U T
;; backwards from D - normalized vector - to numbers (as strings) 
(define (starters D)
(define (not-leading-zero list) (!zero? (first list)))
	(map list->string 
	(filter not-leading-zero (make-set (permutations (for/fold (acc null) ((d D) (i 10))
		#:continue (zero? d)
		(append acc   (for/list ((j d)) i))))))))
	
		
;; printing one node
(define (D-print D)
	 (set! D (reverse (vector->list D)))
	 (for/string ( (d D) (i (in-range 9 -1 -1)))
		#:continue (zero? d)
		(string-append d i)))
			
;; print graph contents
(define (print-sequence D)
		(writeln 1 (starters D))
		(writeln 2 (D-print D ))
		(for ((i (in-range 1 (hash-ref H D))))
			(writeln (+ i 2)  (D-print (setv! D (dnext D))))))
	

;; TA S K
(define (task (n 6)  (verbose #t))
	(generate (make-vector 10) 0 n)
	(define seqmax (apply max (hash-values H)))
	(when verbose (for ((kv H))
		#:continue (!= (rest kv ) seqmax)
		(print-sequence (first kv))))

	(writeln (expt 10 n) '--> 'max-sequence= (1+ seqmax)  'nodes= (length (hash-values H))))
	

```

{{out}}

```scheme

(task 6)
1     (9009 9090 9900)    
2     2920    
3     192210    
4     19222110    
5     19323110    
6     1923123110    
7     1923224110    
8     191413323110    
9     191433125110    
10     19151423125110    
11     19251413226110    
12     1916151413325110    
13     1916251423127110    
14     191716151413326110    
15     191726151423128110    
16     19181716151413327110    
17     19182716151423129110    
18     29181716151413328110    
19     19281716151423228110    
20     19281716151413427110    
21     19182716152413228110    
1000000     -->     max-sequence=     21     nodes=     10926    

(task 7 #f)
10000000     -->     max-sequence=     21     nodes=     23432    
(task 8 #f)
100000000     -->     max-sequence=     21     nodes=     47359    
(task 9 #f)
1000000000     -->     max-sequence=     21     nodes=     97455    
(task 10 #f)
10000000000     -->     max-sequence=     21     nodes=     188493    


```



## Eiffel

Only checks numbers where digits are in ascending order. Digits with trailing zeros have to be treated as ascending numbers (special case). Calculates all the permutations in the end.

```Eiffel

class
	SELF_REFERENTIAL_SEQUENCE

create
	make

feature

	make
		local
			i: INTEGER
			length, max: INTEGER_64
		do
			create seed_value.make
			create sequence.make (25)
			create permuted_values.make
			from
				i := 1
			until
				i > 1000000
			loop
				length := check_length (i.out)
				if length > max then
					max := length
					seed_value.wipe_out
					seed_value.extend (i)
				elseif length = max then
					seed_value.extend (i)
				end
				sequence.wipe_out
				i := next_ascending (i).to_integer
			end
			io.put_string ("Maximal length: " + max.out)
			io.put_string ("%NSeed Value: %N")
			across
				seed_value as s
			loop
				permute (s.item.out, 1)
			end
			across
				permuted_values as p
			loop
				io.put_string (p.item + "%N")
			end
			io.put_string ("Sequence:%N")
			max := check_length (seed_value [1].out)
			across
				sequence as s
			loop
				io.put_string (s.item)
				io.new_line
			end
		end

	next_ascending (n: INTEGER_64): STRING
			-- Next number with ascending digits after 'n'.
			-- Numbers with trailing zeros are treated as ascending numbers.
		local
			st: STRING
			first, should_be, zero: STRING
			i: INTEGER
		do
			create Result.make_empty
			create zero.make_empty
			st := (n + 1).out
			from
			until
				st.count < 2
			loop
				first := st.at (1).out
				if st [2] ~ '0' then
					from
						i := 3
					until
						i > st.count
					loop
						zero.append ("0")
						i := i + 1
					end
					Result.append (first + first + zero)
					st := ""
				else
					should_be := st.at (2).out
					if first > should_be then
						should_be := first
					end
					st.remove_head (2)
					st.prepend (should_be)
					Result.append (first)
				end
			end
			if st.count > 0 then
				Result.append (st [st.count].out)
			end
		end

feature {NONE}

	seed_value: SORTED_TWO_WAY_LIST [INTEGER]

	permuted_values: SORTED_TWO_WAY_LIST [STRING]

	sequence: ARRAYED_LIST [STRING]

	permute (a: STRING; k: INTEGER)
			-- All permutations of 'a'.
		require
			count_positive: a.count > 0
			k_valid_index: k > 0
		local
			t: CHARACTER
			b: STRING
			found: BOOLEAN
		do
			across
				permuted_values as p
			loop
				if p.item ~ a then
					found := True
				end
			end
			if k = a.count and a [1] /= '0' and not found then
				create b.make_empty
				b.deep_copy (a)
				permuted_values.extend (b)
			else
				across
					k |..| a.count as c
				loop
					t := a [k]
					a [k] := a [c.item]
					a [c.item] := t
					permute (a, k + 1)
					t := a [k]
					a [k] := a [c.item]
					a [c.item] := t
				end
			end
		end

	check_length (i: STRING): INTEGER_64
			-- Length of the self referential sequence starting with 'i'.
		local
			found: BOOLEAN
			j: INTEGER
			s: STRING
		do
			create s.make_from_string (i)
			from
			until
				found
			loop
				sequence.extend (s)
				s := next (s)
				from
					j := sequence.count - 1
				until
					j < 1
				loop
					if sequence [j] ~ s then
						found := True
					end
					j := j - 1
				end
			end
			Result := sequence.count
		end

	next (n: STRING): STRING
			-- Next item after 'n' in a self referential sequence.
		local
			i, count: INTEGER
			counter: ARRAY [INTEGER]
		do
			create counter.make_filled (0, 0, 9)
			create Result.make_empty
			from
				i := 1
			until
				i > n.count
			loop
				count := n [i].out.to_integer
				counter [count] := counter [count] + 1
				i := i + 1
			end
			from
				i := 9
			until
				i < 0
			loop
				if counter [i] > 0 then
					Result.append (counter [i].out + i.out)
				end
				i := i - 1
			end
		end

end

```

{{out}}

```txt

Maximal length: 21
Seed Value:
9009
9090
9900 
Sequence:
9009
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110

```



## Factor

Like the Eiffel example, this program saves time by considering only seed numbers whose digits are in increasing order (zeros are exempt). This ensures that extra permutations of a number are not searched, as they produce equivalent sequences (aside from the first element). For instance,   <tt>21</tt>   is the first number to be skipped because it's a permutation of   <tt>12</tt>.

```factor
USING: assocs grouping io kernel math math.combinatorics
math.functions math.ranges math.statistics math.text.utils
prettyprint sequences sets ;
IN: rosetta-code.self-referential-sequence

: next-term ( seq -- seq ) histogram >alist concat ;

! Output the self-referential sequence, given a seed value.
: srs ( seq -- seq n )
    V{ } clone [ 2dup member? ] [ 2dup push [ next-term ] dip ]
    until nip dup length ;

: digit-before? ( m n -- ? ) dup zero? [ 2drop t ] [ <= ] if ;

! The numbers from 1 to n sans permutations.
: candidates ( n -- seq )
    [1,b] [ 1 digit-groups reverse ] map
    [ [ digit-before? ] monotonic? ] filter ;

: max-seed ( n -- seq ) candidates [ srs nip ] supremum-by ;

: max-seeds ( n -- seq )
    max-seed <permutations> members [ first zero? ] reject ;

: digits>number ( seq -- n ) [ 10 swap ^ * ] map-index sum ;

: >numbers ( seq -- seq ) [ digits>number ] map ;

: main ( -- )
    "Seed value(s): " write
    1,000,000 max-seeds
    [ [ reverse ] map >numbers . ]
    [ first srs ] bi
    "Iterations: " write .
    "Sequence:" print >numbers . ;

MAIN: main
```

{{out}}

```txt

Seed value(s): V{ 9009 9090 9900 }
Iterations: 21
Sequence:
V{
    9009
    2920
    221910
    22192110
    19323110
    1923123110
    1923224110
    191413323110
    191433125110
    19151423125110
    19251413226110
    1916151413325110
    1916251423127110
    191716151413326110
    191726151423128110
    19181716151413327110
    19182716151423129110
    29181716151413328110
    19281716151423228110
    19281716151413427110
    19182716152413228110
}

```



## Go

Brute force

```go
package main

import (
    "fmt"
    "strconv"
)

func main() {
    var maxLen int
    var seqMaxLen [][]string
    for n := 1; n < 1e6; n++ {
        switch s := seq(n); {
        case len(s) == maxLen:
            seqMaxLen = append(seqMaxLen, s)
        case len(s) > maxLen:
            maxLen = len(s)
            seqMaxLen = [][]string{s}
        }
    }
    fmt.Println("Max sequence length:", maxLen)
    fmt.Println("Sequences:", len(seqMaxLen))
    for _, seq := range seqMaxLen {
        fmt.Println("Sequence:")
        for _, t := range seq {
            fmt.Println(t)
        }
    }
}

func seq(n int) []string {
    s := strconv.Itoa(n)
    ss := []string{s}

    for {
        dSeq := sortD(s)
        d := dSeq[0]
        nd := 1
        s = ""
        for i := 1; ; i++ {
            if i == len(dSeq) {
                s = fmt.Sprintf("%s%d%c", s, nd, d)
                break
            }
            if dSeq[i] == d {
                nd++
            } else {
                s = fmt.Sprintf("%s%d%c", s, nd, d)
                d = dSeq[i]
                nd = 1
            }
        }
        for _, s0 := range ss {
            if s == s0 {
                return ss
            }
        }
        ss = append(ss, s)
    }
    panic("unreachable")
}

func sortD(s string) []rune {
    r := make([]rune, len(s))
    for i, d := range s {
        j := 0
        for ; j < i; j++ {
            if d > r[j] {
                copy(r[j+1:], r[j:i])
                break
            }
        }
        r[j] = d
    }
    return r
}
```

Output:

```txt

Max sequence length: 21
Sequences: 3
Sequence:
9009
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110
Sequence:
9090
2920
...
19182716152413228110
Sequence:
9900
2920
...
19182716152413228110

```



## Groovy

Solution:

```groovy
Number.metaClass.getSelfReferentialSequence = {
  def number = delegate as String; def sequence = []

  while (!sequence.contains(number)) {
    sequence << number
    def encoded = new StringBuilder()
    ((number as List).sort().join('').reverse() =~ /(([0-9])\2*)/).each { matcher, text, digit ->
      encoded.append(text.size()).append(digit)
    }
    number = encoded.toString()
  }
  sequence
}

def maxSeqSize = { List values ->
  values.inject([seqSize: 0, seeds: []]) { max, n ->
    if (n % 100000 == 99999) println 'HT'
    else if (n % 10000 == 9999) print '.'
    def seqSize = n.selfReferentialSequence.size()
    switch (seqSize) {
      case max.seqSize: max.seeds << n
      case { it < max.seqSize }: return max
      default: return [seqSize: seqSize, seeds: [n]]
    }
  }
}
```

Test:

```groovy
def max = maxSeqSize(0..<1000000)

println "\nLargest sequence size among seeds < 1,000,000\n"
println "Seeds: ${max.seeds}\n"
println "Size: ${max.seqSize}\n"
println "Sample sequence:"
max.seeds[0].selfReferentialSequence.each { println it }
```

Output:

```txt
Largest sequence size among seeds < 1,000,000

Seeds: [9009, 9090, 9900]

Size: 21

Sample sequence:
9009
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110
```



## Haskell

Brute force and quite slow:

```haskell
import Data.Set (Set, member, insert, empty)
import Data.List (group, sort)

step :: String -> String
step = concatMap (\list -> show (length list) ++ [head list]) . group . sort

findCycle :: (Ord a) => [a] -> [a]
findCycle = aux empty where
	aux set (x : xs)
		| x `member` set = []
		| otherwise = x : aux (insert x set) xs

select :: [[a]] -> [[a]]
select = snd . foldl (\(len, acc) xs -> case len `compare` length xs of
		LT -> (length xs, [xs])
		EQ -> (len, xs : acc)
		GT -> (len, acc)) (0, [])

main :: IO ()
main = mapM_ (mapM_ print) $ -- Print out all the numbers
	select $ -- find the longest ones
	map findCycle $ -- run the sequences until there is a repeat
	map (iterate step) $ -- produce the sequence
	map show -- turn the numbers into digits
	[1..1000000] -- The input seeds

```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
link printf 

procedure main()
every L := !longestselfrefseq(1000000) do 
   every printf(" %i : %i\n",i := 1 to *L,L[i])
end


procedure longestselfrefseq(N)    #: find longest sequences from 1 to N

mlen := 0 
every L := selfrefseq(n := 1 to N) do {
   if mlen <:= *L then 
      ML := [L] 
   else if mlen = *L then 
      put(ML,L)
   }

return ML
end

procedure selfrefseq(n) #: return list of sequence oeis:A036058 for seed n
S := set()
L := []
every p := seq(1) do 
   if member(S,n) then return L   # ends at a repeat 
   else {
      insert(S,n)
      put(L,n)
      n := nextselfrefseq(n)
      }
end

procedure nextselfrefseq(n)  #: return next element of sequence oeis:A036058
every (Counts := table(0))[integer(!n)] +:= 1              # count digits
every (n := "") ||:= (0 < Counts[i := 9 to 0 by -1]) || i  # assemble counts
return integer(n)                                           
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides printf, sprintf, fprintf, etc.] 

Sample of Output:
```txt
 1 : 9009
 2 : 2920
 3 : 192210
 4 : 19222110
 5 : 19323110
 6 : 1923123110
 7 : 1923224110
 8 : 191413323110
 9 : 191433125110
 10 : 19151423125110
 11 : 19251413226110
 12 : 1916151413325110
 13 : 1916251423127110
 14 : 191716151413326110
 15 : 191726151423128110
 16 : 19181716151413327110
 17 : 19182716151423129110
 18 : 29181716151413328110
 19 : 19281716151423228110
 20 : 19281716151413427110
 21 : 19182716152413228110
 1 : 9090
 2 : 2920
 ... (manually removed, same as above)
 21 : 19182716152413228110
 1 : 9900
 2 : 2920
 ... (manually removed, same as above)
 21 : 19182716152413228110

```

The following (admittedly overdense) version produces output matching the
problem statement and avoids repeating sequences that arise from 'similar'
seeds.  It does not assume that only one equivalence class of similar seeds
exists at the maximum sequence length.  As with the first example, it works
in both Icon and Unicon.

```Unicon
 link strings   # to get csort()

procedure main(A)
    limit := A[1] | 1000000             # Allow alternate limit
    mSq := 0
    # May have multiple 'unique' sequence sets (unrelated seeds) so use table
    every s := [n := 1 to limit, sequence(n)] do {
        if mSq <:= *s[2] then mT := table()   # new max, start over
        if mSq  == *s[2] then insert((/mT[n := csort(n)] := set()) | mT[n],s)
        }
    dumpSequences(mT)
end
    
procedure sequence(n)                   # produce sequence of SDS with seed n
    every (repeats := [], iter := seq(), put(repeats, n)) do
        if (n := nElem(n)) == !repeats then return repeats   # Converged
end

procedure nElem(n)	                # given n, produce its self-description
    every (n1 := "", c := !cset(n)) do 
        (every (d := 0) +:= (upto(c, n),1)) | (n1 := d||c||n1)
    return n1
end

procedure dumpSequences(seqTab)		# Show each 'unique' sequence in table
    every writes("Seeds:" | (!!seqTab)[1], " ")
    write("\n\nIterations: ",*(!!seqTab)[2])
    every s := !seqTab do (write() & every write(!(!s\1)[2]))
end

```

Output with <tt>limit = 1000000</tt>:

```txt

Seeds: 9009 9090 9900 

Iterations: 21

9009
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110

```



## J

Given:

```j
require'stats'
digits=: 10&#.inv"0 :. ([: ".@; (<'x'),~":&.>)
summar=: (#/.~ ,@,. ~.)@\:~&.digits
sequen=: ~.@(, summar@{:)^:_
values=: ~. \:~&.digits i.1e6
allvar=: [:(#~(=&<.&(10&^.) >./))@~.({~ perm@#)&.(digits"1) 
```


The values with the longest sequence are:


```j
   ;allvar&.> values #~ (= >./) #@sequen"0 values
9900 9090 9009
   # sequen 9900
21
   ,.sequen 9900
                9900
                2920
              192210
            19222110
            19323110
          1923123110
          1923224110
        191413323110
        191433125110
      19151423125110
      19251413226110
    1916151413325110
    1916251423127110
  191716151413326110
  191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110
```


Notes:

<code>digits</code> is an invertible function that maps from a number to a sequence of digits and back where the inverse transform converts numbers to strings, concatenates them, and then back to a number.


```j
   digits 321
3 2 1
   digits inv 34 5
345
```


<code>summar</code> computes the summary successor.


```j
   summar 0 1 2
10 11 12
```


<code>sequen</code> computes the complete non-repeating sequence of summary successors

The computation for <code>values</code> could have been made much more efficient.  Instead, though, all one million integers have their digits sorted in decreasing order, and then the unique set of them is found.

Finally, <code>allvar</code> finds all variations of a number which would have the same summary sequence based on the permutations of that number's digits.


## Java

{{works with|Java|8}}

```java
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.IntStream;

public class SelfReferentialSequence {

    static Map<String, Integer> cache = new ConcurrentHashMap<>(10_000);

    public static void main(String[] args) {
        Seeds res = IntStream.range(0, 1000_000)
                .parallel()
                .mapToObj(n -> summarize(n, false))
                .collect(Seeds::new, Seeds::accept, Seeds::combine);

        System.out.println("Seeds:");
        res.seeds.forEach(e -> System.out.println(Arrays.toString(e)));

        System.out.println("\nSequence:");
        summarize(res.seeds.get(0)[0], true);
    }

    static int[] summarize(int seed, boolean display) {
        String n = String.valueOf(seed);

        String k = Arrays.toString(n.chars().sorted().toArray());
        if (!display && cache.get(k) != null)
            return new int[]{seed, cache.get(k)};

        Set<String> seen = new HashSet<>();
        StringBuilder sb = new StringBuilder();

        int[] freq = new int[10];

        while (!seen.contains(n)) {
            seen.add(n);

            int len = n.length();
            for (int i = 0; i < len; i++)
                freq[n.charAt(i) - '0']++;

            sb.setLength(0);
            for (int i = 9; i >= 0; i--) {
                if (freq[i] != 0) {
                    sb.append(freq[i]).append(i);
                    freq[i] = 0;
                }
            }
            if (display)
                System.out.println(n);
            n = sb.toString();
        }

        cache.put(k, seen.size());

        return new int[]{seed, seen.size()};
    }

    static class Seeds {
        int largest = Integer.MIN_VALUE;
        List<int[]> seeds = new ArrayList<>();

        void accept(int[] s) {
            int size = s[1];
            if (size >= largest) {
                if (size > largest) {
                    largest = size;
                    seeds.clear();
                }
                seeds.add(s);
            }
        }

        void combine(Seeds acc) {
            acc.seeds.forEach(this::accept);
        }
    }
}
```



```txt
Seeds:
[9009, 21]
[9090, 21]
[9900, 21]

Sequence:
9009
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110
```



## jq


```jq
# Given any array, produce an array of [item, count] pairs for each run.
def runs:
  reduce .[] as $item
    ( [];
      if . == [] then [ [ $item, 1] ] 
      else  .[length-1] as $last
            | if $last[0] == $item
              then (.[0:length-1] + [ [$item, $last[1] + 1] ] )
              else . + [[$item, 1]]
              end
      end ) ;

# string to string
def next_self_referential: 
  def runs2integer: # input is an array as produced by runs, 
    # i.e. an array of [count, n] pairs, where count is an int,
    # and n is an "exploded" digit
    reduce .[] as $pair
      (""; . + ($pair[1] | tostring) + ([$pair[0]]|implode) ) ;
    
  explode | sort | reverse | runs | runs2integer;

# Given an integer as a string, 
# compute the entire sequence (of strings) to convergence:
def sequence_of_self_referentials:
  def seq:
     . as $ary
     | (.[length-1] | next_self_referential) as $next
     | if ($ary|index($next)) then $ary
       else $ary + [$next] | seq
       end;
  [.] | seq;

def maximals(n):
  def interesting:
    tostring | (. == (explode | sort | reverse | implode));
  
  reduce range(0;n) as $i
    ([[], 0];  # maximalseeds, length
      if ($i | interesting) then
        ($i|tostring|sequence_of_self_referentials|length) as $length
        | if .[1] == $length then [ .[0] + [$i], $length]
          elif .[1] < $length then [ [$i], $length]
          else .
          end
      else .
      end );

def task(n):
  maximals(n) as $answer
  | "The maximal length to convergence for seeds up to \(n) is \($answer[1]).",
    "The corresponding seeds are the allowed permutations",
    "of the representative number(s): \($answer[0][])",
    "For each representative seed, the self-referential sequence is as follows:",
    ($answer[0][] | tostring
     | ("Representative: \(.)",
        "Self-referential sequence:",
	(sequence_of_self_referentials | map(tonumber))))
    ;

task(1000000)
```

{{out}}
<div style="overflow:scroll; height:400px;">

```sh
$ jq -n -r -f Self_referential_sequence.jq
The maximal length to convergence for seeds up to 1000000 is 21.
The corresponding seeds are the allowed permutations
of the representative number(s): 9900
For each representative seed, the self-referential sequence is as follows:
Representative: 9900
Self-referential sequence:
[
  9900,
  2920,
  192210,
  19222110,
  19323110,
  1923123110,
  1923224110,
  191413323110,
  191433125110,
  19151423125110,
  19251413226110,
  1916151413325110,
  1916251423127110,
  191716151413326100,
  191726151423128100,
  19181716151413326000,
  19182716151423128000,
  29181716151413330000,
  19281716151423230000,
  19281716151413430000,
  19182716152413230000
]
```
</div>


## Julia


```julia
const seen = Dict{Vector{Char}, Vector{Char}}()

function findnextterm(prevterm)
    counts = Dict{Char, Int}()
    reversed = Vector{Char}()
    for c in prevterm
        if !haskey(counts, c)
            counts[c] = 0
        end
        counts[c] += 1
    end
    for c in sort(collect(keys(counts)))
        if counts[c] > 0
            push!(reversed, c)
            if counts[c] == 10
                push!(reversed, '0'); push!(reversed, '1')
            else
                push!(reversed, Char(UInt8(counts[c]) + UInt8('0')))
            end 
        end
    end
    reverse(reversed)
end
 
function findsequence(seedterm)
    term = seedterm
    sequence = Vector{Vector{Char}}()
    while !(term in sequence)
        push!(sequence, term)
        if !haskey(seen, term)
            nextterm = findnextterm(term)
            seen[term] = nextterm
        end
        term = seen[term]
    end
    return sequence
end

function selfseq(maxseed)
    maxseqlen = -1
    maxsequences = Vector{Pair{Int, Vector{Char}}}()
    for i in 1:maxseed
        seq = findsequence([s[1] for s in split(string(i), "")])
        seqlen = length(seq)
        if seqlen > maxseqlen
            maxsequences = [Pair(i, seq)]
            maxseqlen = seqlen
        elseif seqlen == maxseqlen
            push!(maxsequences, Pair(i, seq))
        end
    end
    println("The longest sequence length is $maxseqlen.")
    for p in maxsequences
        println("\n Seed: $(p[1])")
        for seq in p[2]
            println("     ", join(seq, ""))
        end
    end
end

selfseq(1000000)

```
 {{output}} 
```txt

The longest sequence length is 21.

 Seed: 9009
     9009
     2920
     192210
     19222110
     19323110
     1923123110
     1923224110
     191413323110
     191433125110
     19151423125110
     19251413226110
     1916151413325110
     1916251423127110
     191716151413326110
     191726151423128110
     19181716151413327110
     19182716151423129110
     29181716151413328110
     19281716151423228110
     19281716151413427110
     19182716152413228110

 Seed: 9090
     9090
     2920
     192210
     19222110
     19323110
     1923123110
     1923224110
     191413323110
     191433125110
     19151423125110
     19251413226110
     1916151413325110
     1916251423127110
     191716151413326110
     191726151423128110
     19181716151413327110
     19182716151423129110
     29181716151413328110
     19281716151423228110
     19281716151413427110
     19182716152413228110

 Seed: 9900
     9900
     2920
     192210
     19222110
     19323110
     1923123110
     1923224110
     191413323110
     191433125110
     19151423125110
     19251413226110
     1916151413325110
     1916251423127110
     191716151413326110
     191726151423128110
     19181716151413327110
     19182716151423129110
     29181716151413328110
     19281716151423228110
     19281716151413427110
     19182716152413228110

```



## Kotlin


```scala
// version 1.1.2

const val LIMIT = 1_000_000

val sb = StringBuilder()

fun selfRefSeq(s: String): String {
    sb.setLength(0)  // faster than using a local StringBuilder object
    for (d in '9' downTo '0') {
        if (d !in s) continue
        val count = s.count { it == d }      
        sb.append("$count$d")
    } 
    return sb.toString()
}

fun permute(input: List<Char>): List<List<Char>> {
    if (input.size == 1) return listOf(input)
    val perms = mutableListOf<List<Char>>()
    val toInsert = input[0]
    for (perm in permute(input.drop(1))) {
        for (i in 0..perm.size) {
            val newPerm = perm.toMutableList()
            newPerm.add(i, toInsert)
            perms.add(newPerm)
        }
    }
    return perms
}

fun main(args: Array<String>) {
    val sieve = IntArray(LIMIT) // all zero by default
    val elements = mutableListOf<String>()
    for (n in 1 until LIMIT) {
        if (sieve[n] > 0) continue
        elements.clear()  
        var next = n.toString()
        elements.add(next)
        while (true) {
            next = selfRefSeq(next)
            if (next in elements) {
                val size = elements.size
                sieve[n] = size
                if (n > 9) {
                    val perms = permute(n.toString().toList()).distinct()
                    for (perm in perms) {
                        if (perm[0] == '0') continue
                        val k = perm.joinToString("").toInt()
                        sieve[k] = size
                    }
                }    
                break
            }
            elements.add(next)
        }
    }
    val maxIterations = sieve.max()!!
    for (n in 1 until LIMIT) {
        if (sieve[n] < maxIterations) continue
        println("$n -> Iterations = $maxIterations")
        var next = n.toString()
        for (i in 1..maxIterations) {
            println(next)
            next = selfRefSeq(next)
        }
        println()
    } 
}
```


{{out}}

```txt

9009 -> Iterations = 21
9009
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110

9090 -> Iterations = 21
9090
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110

9900 -> Iterations = 21
9900
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110

```



## Lua

Runs in about nine seconds under LuaJIT.  Uses memoisation via the global table 'nextTerm'.

```lua
-- Return the next term in the self-referential sequence
function findNext (nStr)
    local nTab, outStr, pos, count = {}, "", 1, 1
    for i = 1, #nStr do nTab[i] = nStr:sub(i, i) end
    table.sort(nTab, function (a, b) return a > b end)
    while pos <= #nTab do
        if nTab[pos] == nTab[pos + 1] then
            count = count + 1
        else
            outStr = outStr .. count .. nTab[pos]
            count = 1
        end
        pos = pos + 1
    end
    return outStr
end

-- Return boolean indicating whether table t contains string s
function contains (t, s)
    for k, v in pairs(t) do
        if v == s then return true end
    end
    return false
end

-- Return the sequence generated by the given seed term
function buildSeq (term)
    local sequence = {}
    repeat
        table.insert(sequence, term)
        if not nextTerm[term] then nextTerm[term] = findNext(term) end
        term = nextTerm[term]
    until contains(sequence, term)
    return sequence
end

-- Main procedure
nextTerm = {}
local highest, seq, hiSeq = 0
for i = 1, 10^6 do
    seq = buildSeq(tostring(i))
    if #seq > highest then
        highest = #seq
        hiSeq = {seq}
    elseif #seq == highest then
        table.insert(hiSeq, seq)
    end
end
io.write("Seed values: ")
for _, v in pairs(hiSeq) do io.write(v[1] .. " ") end
print("\n\nIterations: " .. highest)
print("\nSample sequence:")
for _, v in pairs(hiSeq[1]) do print(v) end
```

{{out}}

```txt
Seed values: 9009 9090 9900

Iterations: 21

Sample sequence:
9009
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110
```



## Mathematica


```Mathematica
selfRefSequence[ x_ ] := FromDigits@Flatten@Reverse@Cases[Transpose@{RotateRight[DigitCount@x,1], Range[0,9]},Except[{0,_}]]
DisplaySequence[ x_ ] := NestWhileList[selfRefSequence,x,UnsameQ[##]&,4] 
data= {#, Length@DisplaySequence[#]}&/@Range[1000000];
Print["Values: ", Select[data  ,#[[2]] == Max@data[[;;,2]]&][[1,;;]]]
Print["Iterations: ", Length@DisplaySequence[#]&/@Select[data  ,#[[2]] == Max@data[[;;,2]]&][[1,;;]]]
DisplaySequence@Select[data, #[[2]] == Max@data[[;;,2]]&][[1]]//Column
```



```txt
Values: {9009, 9090, 9900}
Iterations: 21
9009
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110
19281716151413427110
```



## Perl


```perl
sub next_num {
	my @a;
	$a[$_]++ for split '', shift;
	join('', map(exists $a[$_] ? $a[$_].$_ : "", reverse 0 .. 9));
}

my %cache;
sub seq {
	my $a = shift;
	my (%seen, @s);
	until ($seen{$a}) {
		$seen{$a} = 1;
		push(@s, $a);
		last if !wantarray && $cache{$a};
		$a = next_num($a);
	}
	return (@s) if wantarray;

	my $l = $cache{$a};
	if ($l) { $cache{$s[$_]} = $#s - $_ + $l for (0 .. $#s); }
	else {
		$l++ while ($s[-$l] != $a);
		$cache{pop @s} = $l 	for (1 .. $l);
		$cache{pop @s} = ++$l	while @s;
	}
	$cache{$s[0]}
}

my (@mlist, $mlen);
for (1 .. 100_000) { # 1_000_000 takes very, very long
	my $l = seq($_);
	next if $l < $mlen;

	if ($l > $mlen) { $mlen = $l; @mlist = (); }
	push @mlist, $_;
}

print "longest ($mlen): @mlist\n";
print join("\n", seq($_)), "\n\n"	for @mlist;
```



## Perl 6

{{Works with|rakudo|2018.03}}


```perl6
my @list;
my $longest = 0;
my %seen;

for 1 .. 1000000 -> $m {
    next unless $m ~~ /0/;         # seed must have a zero
    my $j = join '', $m.comb.sort;
    next if %seen{$j}:exists;      # already tested a permutation
    %seen{$j} = '';
    my @seq = converging($m);
    my %elems;
    my $count;
    for @seq[] -> $value { last if ++%elems{$value} == 2; $count++; };
    if $longest == $count {
        @list.push($m);
    }
    elsif $longest < $count {
        $longest = $count;
        @list = $m;
        print "\b" x 20, "$count, $m"; # monitor progress
    }
};

for @list -> $m {
    say "\nSeed Value(s): ", my $seeds = ~permutations($m).unique.grep( { .substr(0,1) != 0 } );
    my @seq = converging($m);
    my %elems;
    my $count;
    for @seq[] -> $value { last if ++%elems{$value} == 2; $count++; };
    say "\nIterations: ", $count;
    say "\nSequence: (Only one shown per permutation group.)";
   .say for |@seq[^$count], "\n";
}

sub converging ($seed) { return $seed, -> $l { join '', map { $_.value.elems~$_.key }, $l.comb.classify({$^b}).sort: {-$^c.key} } ... * }

sub permutations ($string, $sofar? = '' ) {
    return $sofar unless $string.chars;
    my @perms;
    for ^$string.chars -> $idx {
        my $this = $string.substr(0,$idx)~$string.substr($idx+1);
        my $char = substr($string, $idx,1);
        @perms.push( |permutations( $this, join '', $sofar, $char ) );
    }
    return @perms;
}
```


{{out}}

```txt

Seed Value(s): 9009 9090 9900

Iterations: 21

Sequence: (Only one shown per permutation group.)
9009
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110

```



## Phix

Optimisation idea taken from CoffeeScript, completes in under a second.

```Phix
string n = "000000"

function incn()
    for i=length(n) to 1 by -1 do
        if n[i]='9' then
            if i=1 then return false end if
            n[i]='0'
        else
            n[i] += 1
            exit
        end if
    end for
    return true
end function

sequence res = {}, bestseen
integer maxcycle = 0

procedure srs()
sequence seen, this = n
integer cycle = 1
    while length(this)>1 and this[1]='0' do
        this = this[2..$]
    end while
    integer ch = this[1]
    for i=2 to length(this) do
        if this[i]>ch then return end if
        ch = this[i]
    end for
    seen = {this}
    while 1 do
        sequence digits = repeat(0,10)
        for i=1 to length(this) do
            digits[this[i]-'0'+1] += 1
        end for
        string next = ""
        for i=length(digits) to 1 by -1 do
            if digits[i]!=0 then
                next &= sprint(digits[i])
                next &= i+'0'-1
            end if
        end for
        if find(next,seen) then exit end if
        seen = append(seen,next)
        this = next             
        cycle += 1
    end while
    if cycle>maxcycle then
        res = {seen[1]}
        maxcycle = cycle
        bestseen = seen
    elsif cycle=maxcycle then
        res = append(res,seen[1])
    end if
end procedure

while 1 do
    srs()
    if not incn() then exit end if
end while

-- add non-leading-0 perms:
for i=length(res) to 1 by -1 do
    string ri = res[i]
    for p=1 to factorial(length(ri)) do
        string pri = permute(p,ri)
        if pri[1]!='0' and not find(pri,res) then
            res = append(res,pri)
        end if
    end for
end for
?res
puts(1,"cycle length is ") ?maxcycle
pp(bestseen,{pp_Nest,1})
```


```txt

{"9900","9009","9090"}
cycle length is 21
{"9900",
 "2920",
 "192210",
 "19222110",
 "19323110",
 "1923123110",
 "1923224110",
 "191413323110",
 "191433125110",
 "19151423125110",
 "19251413226110",
 "1916151413325110",
 "1916251423127110",
 "191716151413326110",
 "191726151423128110",
 "19181716151413327110",
 "19182716151423129110",
 "29181716151413328110",
 "19281716151423228110",
 "19281716151413427110",
 "19182716152413228110"}

```



## PicoLisp

Using 'las' from [[Look-and-say sequence#PicoLisp]]:

```PicoLisp
(de selfRefSequence (Seed)
   (let L (mapcar format (chop Seed))
      (make
         (for (Cache NIL  (not (idx 'Cache L T)))
            (setq L
               (las (flip (sort (copy (link L))))) ) ) ) ) )

(let Res NIL
   (for Seed 1000000
      (let N (length (selfRefSequence Seed))
         (cond
            ((> N (car Res)) (setq Res (list N Seed)))
            ((= N (car Res)) (queue 'Res Seed)) ) ) )
   (println 'Values: (cdr Res))
   (println 'Iterations: (car Res))
   (mapc prinl (selfRefSequence (cadr Res))) )
```

Output:

```txt
Values: (9009 9090 9900)
Iterations: 21
9009
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110
```



## Python

The number generation function follows that of Look-and-say with a sort. only the first of any set of numbers with the same digits has the length of its sequence calculated in function max_A036058_length, although no timings were taken to check if the optimisation was of value.


```python
from itertools import groupby, permutations

def A036058(number):
    return ''.join( str(len(list(g))) + k
                    for k,g in groupby(sorted(str(number), reverse=True)) )

def A036058_length(numberstring='0', printit=False):
    iterations, last_three, queue_index = 1, ([None] * 3), 0

    def A036058(number):
        # rely on external reverse-sort of digits of number
        return ''.join( str(len(list(g))) + k
                        for k,g in groupby(number) )

    while True:
        if printit:
            print("  %2i %s" % (iterations, numberstring))
        numberstring = ''.join(sorted(numberstring, reverse=True))
        if numberstring in last_three:
            break
        assert iterations < 1000000
        last_three[queue_index], numberstring = numberstring, A036058(numberstring)
        iterations += 1
        queue_index +=1
        queue_index %=3
    return iterations
    
def max_A036058_length( start_range=range(11) ):
    already_done = set()
    max_len = (-1, [])
    for n in start_range:
        sn = str(n)
        sns = tuple(sorted(sn, reverse=True))
        if sns not in already_done:
            already_done.add(sns)
            size = A036058_length(sns)
            if size > max_len[0]:
                max_len = (size, [n])
            elif size == max_len[0]:
                max_len[1].append(n)
    return max_len

lenmax, starts = max_A036058_length( range(1000000) )

# Expand
allstarts = []
for n in starts:
    allstarts += [int(''.join(x))
                  for x in set(k
                               for k in permutations(str(n), 4)
                               if k[0] != '0')]
allstarts = [x for x in sorted(allstarts) if x < 1000000]

print ( '''\
The longest length, followed by the number(s) with the longest sequence length
for starting sequence numbers below 1000000 are:
  Iterations = %i and sequence-starts = %s.''' % (lenmax, allstarts)   )

print ( '''
Note that only the first of any sequences with the same digits is printed below.
(The others will differ only in their first term)''' )

for n in starts:
    print()
    A036058_length(str(n), printit=True)
```


;Output:

```txt

The longest length, followed by the number(s) with the longest sequence length
for starting sequence numbers below 1000000 are:
  Iterations = 21 and sequence-starts = [9009, 9090, 9900].

Note that only the first of any sequences with the same digits is printed below.
(The others will differ only in their first term)

   1 9009
   2 2920
   3 192210
   4 19222110
   5 19323110
   6 1923123110
   7 1923224110
   8 191413323110
   9 191433125110
  10 19151423125110
  11 19251413226110
  12 1916151413325110
  13 1916251423127110
  14 191716151413326110
  15 191726151423128110
  16 19181716151413327110
  17 19182716151423129110
  18 29181716151413328110
  19 19281716151423228110
  20 19281716151413427110
  21 19182716152413228110
```



## Racket


```racket

#lang racket

(define (next s)
  (define v (make-vector 10 0))
  (for ([c s])
    (define d (- (char->integer #\9) (char->integer c)))
    (vector-set! v d (add1 (vector-ref v d))))
  (string-append* (for/list ([x v] [i (in-range 9 -1 -1)] #:when (> x 0))
                    (format "~a~a" x i))))

(define (seq-of s)
  (reverse (let loop ([ns (list s)])
             (define n (next (car ns)))
             (if (member n ns) ns (loop (cons n ns))))))

(define (sort-string s)
  (list->string (sort (string->list s) char>?)))

(define-values [len nums seq]
  (for/fold ([*len #f] [*nums #f] [*seq #f])
            ([n (in-range 1000000 -1 -1)]) ; start at the high end
    (define s (number->string n))
    (define sorted (sort-string s))
    (cond [(equal? s sorted)
           (define seq (seq-of s))
           (define len (length seq))
           (cond [(or (not *len) (> len *len)) (values len (list s) seq)]
                 [(= len *len) (values len (cons s *nums) seq)]
                 [else (values *len *nums *seq)])]
          ;; not sorted: see if it's a permutation of the best
          [else (values
                 *len
                 (if (and *nums (member sorted *nums)) (cons s *nums) *nums)
                 *seq)])))
(printf "Numbers: ~a\nLength: ~a\n" (string-join nums ", ") len)
(for ([n seq]) (printf "  ~a\n" n))

```

{{out}}

```txt

Numbers: 9009, 9090, 9900
Length: 21
  9900
  2920
  192210
  19222110
  19323110
  1923123110
  1923224110
  191413323110
  191433125110
  19151423125110
  19251413226110
  1916151413325110
  1916251423127110
  191716151413326110
  191726151423128110
  19181716151413327110
  19182716151423129110
  29181716151413328110
  19281716151423228110
  19281716151413427110
  19182716152413228110

```



## REXX

The REXX language supports   '''sparse'''   (stemmed) arrays, so this program utilizes REXX's hashing of 

array elements to speed up the checking to see if a sequence has been generated before.

```rexx
/*REXX pgm generates a self─referential sequence and displays sequences with max length.*/
parse arg LO HI .                                /*obtain optional arguments from the CL*/
if LO=='' | LO==","  then LO=      1             /*Not specified?  Then use the default.*/
if HI=='' | HI==","  then HI=1000000 - 1         /* "      "         "   "   "     "    */
max$=;      seeds=;    maxL=0                    /*inialize some defaults and counters. */

  do #=LO  to HI;      n=#;     @.=0;     @.#=1  /*loop thru seed; define some defaults.*/
  $=n
      do c=1  until x==n;       x=n              /*generate a self─referential sequence.*/
      n=;           do k=9  by -1  for 10        /*generate a new sequence (downwards). */
                    _=countstr(k, x)             /*obtain the number of sequence counts.*/
                    if _\==0  then n=n || _ || k /*is count > zero?  Then append it to N*/
                    end   /*k*/
      if @.n  then leave                         /*has sequence been generated before ? */
      $=$'-'n;     @.n=1                         /*add the number to sequence and roster*/
      end   /*c*/

  if c==maxL then do;  seeds=seeds #             /*is the sequence equal to max so far ?*/
                       max$=max$   $             /*append this self─referential # to  $ */
                  end
             else if c>maxL  then do;  seeds=#   /*use the new number as the new seed.  */
                                  maxL=c; max$=$ /*also, set the new maximum L; max seq.*/
                                  end            /* [↑]  have we found a new best seq ? */
  end   /*#*/

say  ' seeds that had the most iterations: '     seeds
say  'the maximum self─referential length: '     maxL

  do j=1  for words(max$) ;      say
  say copies('─',30)  "iteration sequence for: "   word(seeds,j)  '  ('maxL  "iterations)"
  q=translate( word( max$, j), ,'-')
                                     do k=1  for words(q);     say  word(q, k)
                                     end   /*k*/
  end   /*j*/                                    /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default inputs:}}

(Shown at five-sixths size.)

<pre style="font-size:84%;height:85ex">
 seeds that had the most iterations:  9009 9090 9900
the maximum self─referential length:  21

────────────────────────────── iteration sequence for:  9009   (21 iterations)
9009
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110

────────────────────────────── iteration sequence for:  9090   (21 iterations)
9090
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110

────────────────────────────── iteration sequence for:  9900   (21 iterations)
9900
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110

```



## Ruby

Cached for performance

```ruby
$cache = {}
def selfReferentialSequence_cached(n, seen = [])
  return $cache[n] if $cache.include? n
  return [] if seen.include? n

  digit_count = Array.new(10, 0)
  n.to_s.chars.collect {|char| digit_count[char.to_i] += 1}
  term = ''
  9.downto(0).each do |d|
    if digit_count[d] > 0
      term += digit_count[d].to_s + d.to_s
    end
  end
  term = term.to_i
  $cache[n] = [n] + selfReferentialSequence_cached(term, [n] + seen)
end

limit = 1_000_000
max_len = 0
max_vals = []

1.upto(limit - 1) do |n| 
  seq = selfReferentialSequence_cached(n)
  if seq.length > max_len
    max_len = seq.length
    max_vals = [n]
  elsif seq.length == max_len
    max_vals << n
  end
end

puts "values: #{max_vals.inspect}"
puts "iterations: #{max_len}"
puts "sequence:"
selfReferentialSequence_cached(max_vals[0]).each_with_index do |val, idx| 
  puts "%2d %d" % [idx + 1, val]
end
```

output

```txt
values: [9009, 9090, 9900]
iterations: 21
sequence:
 1 9009
 2 2920
 3 192210
 4 19222110
 5 19323110
 6 1923123110
 7 1923224110
 8 191413323110
 9 191433125110
10 19151423125110
11 19251413226110
12 1916151413325110
13 1916251423127110
14 191716151413326110
15 191726151423128110
16 19181716151413327110
17 19182716151423129110
18 29181716151413328110
19 19281716151423228110
20 19281716151413427110
21 19182716152413228110
```



## Scala


This example creates a ParVector, which is a collection type that inherently uses parallel processing, of all seeds within the range, maps each seed to a tuple containing the seed, the sequence, and the number of iterations, sorts the collection by decreasing sequence length, then shows the relevant information for the maximal sequences at the head of the collection.


```scala
import spire.math.SafeLong

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParVector

object SelfReferentialSequence {
  def main(args: Array[String]): Unit = {
    val nums = ParVector.range(1, 1000001).map(SafeLong(_))
    val seqs = nums.map{ n => val seq = genSeq(n); (n, seq, seq.length) }.toVector.sortWith((a, b) => a._3 > b._3)
    val maxes = seqs.takeWhile(t => t._3 == seqs.head._3)
    
    println(s"Seeds: ${maxes.map(_._1).mkString(", ")}\nIterations: ${maxes.head._3}")
    for(e <- maxes.distinctBy(a => nextTerm(a._1.toString))){
      println(s"\nSeed: ${e._1}\n${e._2.mkString("\n")}")
    }
  }
  
  def genSeq(seed: SafeLong): Vector[String] = {
    @tailrec
    def gTrec(seq: Vector[String], n: String): Vector[String] = {
      if(seq.contains(n)) seq
      else gTrec(seq :+ n, nextTerm(n))
    }
    gTrec(Vector[String](), seed.toString)
  }
  
  def nextTerm(num: String): String = {
    @tailrec
    def dTrec(digits: Vector[(Int, Int)], src: String): String = src.headOption match{
      case Some(n) => dTrec(digits :+ ((n.asDigit, src.count(_ == n))), src.filter(_ != n))
      case None => digits.sortWith((a, b) => a._1 > b._1).map(p => p._2.toString + p._1.toString).mkString
    }
    dTrec(Vector[(Int, Int)](), num)
  }
}
```


{{out}}


```txt
Seeds: 9009, 9090, 9900
Iterations: 21

Seed: 9009
9009
2920
192210
19222110
19323110
1923123110
1923224110
191413323110
191433125110
19151423125110
19251413226110
1916151413325110
1916251423127110
191716151413326110
191726151423128110
19181716151413327110
19182716151423129110
29181716151413328110
19281716151423228110
19281716151413427110
19182716152413228110
```



## TXR


==={{trans|Clojure}}===

This is a close, almost expression-by-expression transliteration of the Clojure version.


```txrlisp
;; Syntactic sugar for calling reduce-left
(defmacro reduce-with ((acc init item sequence) . body)
  ^(reduce-left (lambda (,acc ,item) ,*body) ,sequence ,init))

 ;; Macro similar to clojure's ->> and ->
(defmacro opchain (val . ops)
  ^[[chain ,*[mapcar [iffi consp (op cons 'op)] ops]] ,val])

;; Reduce integer to a list of integers representing its decimal digits.
(defun digits (n)
  (if (< n 10)
    (list n)
    (opchain n tostring list-str (mapcar (op - @1 #\0)))))

(defun dcount (ds)
  (digits (length ds)))

;; Perform a look-say step like (1 2 2) --"one 1, two 2's"-> (1 1 2 2).
(defun summarize-prev (ds)
  (opchain ds copy (sort @1 >) (partition-by identity)
           (mapcar [juxt dcount first]) flatten))

;; Take a starting digit string and iterate the look-say steps,
;; to generate the whole sequence, which ends when convergence is reached.
(defun convergent-sequence (ds)
  (reduce-with (cur-seq nil ds [giterate true summarize-prev ds])
    (if (member ds cur-seq)
      (return-from convergent-sequence cur-seq)
      (nconc cur-seq (list ds)))))

;; A candidate sequence is one which begins with montonically
;; decreasing digits. We don't bother with (9 0 9 0) or (9 0 0 9);
;; which yield identical sequences to (9 9 0 0).
(defun candidate-seq (n)
  (let ((ds (digits n)))
    (if [apply >= ds]
      (convergent-sequence ds))))

;; Discover the set of longest sequences.
(defun find-longest (limit)
  (reduce-with (max-seqs nil new-seq [mapcar candidate-seq (range 1 limit)])
    (let ((cmp (- (opchain max-seqs first length) (length new-seq))))
      (cond ((> cmp 0) max-seqs)
            ((< cmp 0) (list new-seq))
            (t (nconc max-seqs (list new-seq)))))))

(defvar *results* (find-longest 1000000))

(each ((result *results*))
  (flet ((strfy (list) ;; (strfy '((1 2 3 4) (5 6 7 8))) -> ("1234" "5678")
           (mapcar [chain (op mapcar tostring) cat-str] list)))
    (let* ((seed (first result))
           (seeds (opchain seed perm uniq (remove-if zerop @1 first))))
      (put-line `Seed value(s): @(strfy seeds)`)
      (put-line)
      (put-line `Iterations: @(length result)`)
      (put-line)
      (put-line `Sequence: @(strfy result)`))))
```


{{out}}

```txt
$ txr self-ref-seq.tl

Seed value(s): 9900 9090 9009

Iterations: 21

Sequence: 9900 2920 192210 19222110 19323110 1923123110 1923224110 191413323110 191433125110 19151423125110 19251413226110 1916151413325110 1916251423127110 191716151413326110 191726151423128110 19181716151413327110 19182716151423129110 29181716151413328110 19281716151423228110 19281716151413427110 19182716152413228110
```


==={{trans|Common Lisp}}===

Mostly the same logic. The <code>count-and-say</code> function is based on the same steps, but stays in the string domain instead of converting the input to a list, and then the output back to a string. It also avoids building the output backwards and reversing it, so <code>out</code> must be accessed on the right side inside the loop. This is easy due to Python-inspired array indexing semantics: -1 means last element, -2 second last
and so on.

Like in Common Lisp, TXR's <code>sort</code> is destructive, so we take care to use <code>copy-str</code>.


```txrlisp
(defun count-and-say (str)
  (let* ((s [sort (copy-str str) <])
         (out `@[s 0]0`))
    (each ((x s))
      (if (eql x [out -1])
        (inc [out -2])
        (set out `@{out}1@x`)))
    out))

(defun ref-seq-len (n : doprint)
  (let ((s (tostring n)) hist)
    (while t
      (push s hist)
      (if doprint (pprinl s))
      (set s (count-and-say s))
      (each ((item hist)
             (i (range 0 2)))
        (when (equal s item)
          (return-from ref-seq-len (length hist)))))))

(defun find-longest (top)
  (let (nums (len 0))
    (each ((x (range 0 top)))
      (let ((l (ref-seq-len x)))
        (when (> l len) (set len l) (set nums nil))
        (when (= l len) (push x nums))))
    (list nums len)))
```


{{out}}


```txt
Longest: ((9900 9090 9009 99) 21)
9900
2029
102219
10212219
10313219
1031122319
1041222319
103132131419
105112331419
10511223141519
10612213142519
1051321314151619
1071122314251619
106132131415161719
108112231415261719
10713213141516171819
10911223141516271819
10813213141516171829
10812223141516172819
10714213141516172819
10812213241516271819
```


==={{trans|Racket}}===


```txrlisp
;; Macro very similar to Racket's for/fold
(defmacro for-accum (accum-var-inits each-vars . body)
  (let ((accum-vars [mapcar first accum-var-inits])
        (block-sym (gensym))
        (next-args [mapcar (ret (progn @rest (gensym))) accum-var-inits])
        (nvars (length accum-var-inits)))
    ^(let ,accum-var-inits
       (flet ((iter (,*next-args)
                ,*[mapcar (ret ^(set ,@1 ,@2)) accum-vars next-args]))
         (each ,each-vars
           ,*body)
         (list ,*accum-vars)))))

(defun next (s)
  (let ((v (vector 10 0)))
    (each ((c s))
      (inc [v (- #\9 c)]))
    (cat-str
      (collect-each ((x v)
                     (i (range 9 0 -1)))
        (when (> x 0)
          `@x@i`)))))

(defun seq-of (s)
  (for* ((ns ()))
        ((not (member s ns)) (reverse ns))
        ((push s ns) (set s (next s)))))

(defun sort-string (s)
  [sort (copy s) >])

(tree-bind (len nums seq)
  (for-accum ((*len nil) (*nums nil) (*seq nil))
             ((n (range 1000000 0 -1))) ;; start at the high end
    (let* ((s (tostring n))
           (sorted (sort-string s)))
      (if (equal s sorted)
        (let* ((seq (seq-of s))
               (len (length seq)))
          (cond ((or (not *len) (> len *len)) (iter len (list s) seq))
                ((= len *len) (iter len (cons s *nums) seq))))
        (iter *len
              (if (and *nums (member sorted *nums)) (cons s *nums) *nums)
              *seq))))
  (put-line `Numbers: @{nums ", "}\nLength: @len`)
  (each ((n seq)) (put-line `  @n`)))
```


{{out}}


```txt
Numbers: 9009, 9090, 9900
Length: 21
  9900
  2920
  192210
  19222110
  19323110
  1923123110
  1923224110
  191413323110
  191433125110
  19151423125110
  19251413226110
  1916151413325110
  1916251423127110
  191716151413326110
  191726151423128110
  19181716151413327110
  19182716151423129110
  29181716151413328110
  19281716151423228110
  19281716151413427110
  19182716152413228110
```



## Tcl

<!-- The first version of this code had a neat trick with sorting the strings characters and using a counting regexp, but it was very slow -->

```tcl
proc nextterm n {
    foreach c [split $n ""] {incr t($c)}
    foreach c {9 8 7 6 5 4 3 2 1 0} {
	if {[info exist t($c)]} {append r $t($c) $c}
    }
    return $r
}
# Local context of lambda term is just for speed
apply {limit {
    #  Build a digit cache; this adds quite a bit of speed
    set done [lrepeat [set l2 [expr {$limit * 100}]] 0]
    # Iterate over search space
    set maxlen 0
    set maxes {}
    for {set i 0} {$i < $limit} {incr i} {
	if {[lindex $done $i]} continue
	# Compute the sequence length for this value (with help from cache)
	set seq {}
	for {set seed $i} {$seed ni $seq} {set seed [nextterm $seed]} {
	    if {$seed < $l2 && [lindex $done $seed]} {
		set len [expr {[llength $seq] + [lindex $done $seed]}]
		break
	    }
	    set len [llength [lappend seq $seed]]
	}
	# What are we going to do about it?
	if {$len > $maxlen} {
	    set maxlen $len
	    set maxes [list $i]
	} elseif {$len == $maxlen} {
	    lappend maxes $i
	}
	# Update the cache with what we have learned
	foreach n $seq {
	    if {$n < $l2} {lset done $n $len}
	    incr len -1
	}
    }
    # Output code
    puts "max length: $maxlen"
    foreach c $maxes {puts $c}
    puts "Sample max-len sequence:"
    set seq {}
    # Rerun the sequence generator for printing; faster for large limits
    for {set seed [lindex $c 0]} {$seed ni $seq} {set seed [nextterm $seed]} {
	lappend seq $seed
        puts "\t$seed"
    }
}} 1000000
```

Output:

```txt

max length: 21
9009
9090
9900
Sample max-len sequence:
	9900
	2920
	192210
	19222110
	19323110
	1923123110
	1923224110
	191413323110
	191433125110
	19151423125110
	19251413226110
	1916151413325110
	1916251423127110
	191716151413326110
	191726151423128110
	19181716151413327110
	19182716151423129110
	29181716151413328110
	19281716151423228110
	19281716151413427110
	19182716152413228110

```



## zkl


```zkl
N:=0d1_000_001;

fcn lookAndJustSaying(seed){ // numeric String --> numeric String
   "9876543210".pump(String,'wrap(n){
      (s:=seed.inCommon(n)) and String(s.len(),n) or ""
   });
}
fcn sequence(seed){ // numeric string --> sequence until it repeats
   seq:=L();
   while(not seq.holds(seed)){ seq.append(seed); seed=lookAndJustSaying(seed); }
   seq
}
fcn decending(str) //--> True if digits are in descending (or equal) order
   { (not str.walker().zipWith('<,str[1,*]).filter1()) }

szs:=List.createLong(25); max:=0;
foreach seed in (N){ 
   z:=seed.toString(); 
   if(decending(z)){ // 321 generates same sequence as 312,132,123,213
      len:=sequence(z).len();
      if(len>max) szs.clear();
      if(len>=max){ szs.append(seed.toString()); max=len; }
   }
}

// List permutations of longest seeds
// ("9900"-->(((9,0,0,9),...))-->((9,0,0,9),...)-->("9009"...)
//        -->remove numbers w/leading zeros-->remove dups
zs:=szs.apply(Utils.Helpers.permute).flatten().apply("concat")
   .filter(fcn(s){ s[0]!="0" }) : Utils.Helpers.listUnique(_);
println(max," iterations for ",zs.concat(", "));
zs.pump(Console.println,sequence,T("concat",", "));
```

Ignoring permutations cut run time from 4 min to 9 sec.
{{out}}

```txt

21 iterations for 9900, 9090, 9009
9900, 2920, 192210, 19222110, 19323110, 1923123110, 1923224110, 191413323110, 191433125110, 19151423125110, 19251413226110, 1916151413325110, 1916251423127110, 191716151413326110, 191726151423128110, 19181716151413327110, 19182716151423129110, 29181716151413328110, 19281716151423228110, 19281716151413427110, 19182716152413228110
9090, 2920, 192210, 19222110, 19323110, 1923123110, 1923224110, 191413323110, 191433125110, 19151423125110, 19251413226110, 1916151413325110, 1916251423127110, 191716151413326110, 191726151423128110, 19181716151413327110, 19182716151423129110, 29181716151413328110, 19281716151423228110, 19281716151413427110, 19182716152413228110
9009, 2920, 192210, 19222110, 19323110, 1923123110, 1923224110, 191413323110, 191433125110, 19151423125110, 19251413226110, 1916151413325110, 1916251423127110, 191716151413326110, 191726151423128110, 19181716151413327110, 19182716151423129110, 29181716151413328110, 19281716151423228110, 19281716151413427110, 19182716152413228110

```

