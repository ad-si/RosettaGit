+++
title = "Hofstadter Figure-Figure sequences"
description = ""
date = 2019-03-09T21:38:35Z
aliases = []
[extra]
id = 10707
[taxonomies]
categories = []
tags = []
+++

{{task}}

These two sequences of positive integers are defined as:
:::: <big><math>\begin{align}
R(1)&=1\ ;\ S(1)=2 \\
R(n)&=R(n-1)+S(n-1), \quad n>1.
\end{align}</math></big>



The sequence <big><math>S(n)</math></big> is further defined as the sequence of positive integers '''''not''''' present in <big><math>R(n)</math></big>.

Sequence <big><math>R</math></big> starts:
    1, 3, 7, 12, 18, ...
Sequence <big><math>S</math></big> starts:
    2, 4, 5, 6, 8, ...


;Task:
# Create two functions named '''ffr''' and '''ffs''' that when given '''n''' return '''R(n)''' or '''S(n)''' respectively.
(Note that R(1) = 1 and S(1) = 2 to avoid off-by-one errors).
# No maximum value for '''n''' should be assumed.
# Calculate and show that the first ten values of '''R''' are:
 1, 3, 7, 12, 18, 26, 35, 45, 56, and 69
# Calculate and show that the first 40 values of '''ffr''' plus the first 960 values of '''ffs''' include all the integers from 1 to 1000 exactly once.


;References:
* Sloane's [http://oeis.org/A005228 A005228] and [http://oeis.org/A030124 A030124].
* [http://mathworld.wolfram.com/HofstadterFigure-FigureSequence.html Wolfram MathWorld]
* Wikipedia: [[wp:Hofstadter_sequence#Hofstadter_Figure-Figure_sequences|Hofstadter Figure-Figure sequences]].





## Ada

Specifying a package providing the functions FFR and FFS:

```Ada
package Hofstadter_Figure_Figure is

   function FFR(P: Positive) return Positive;

   function FFS(P: Positive) return Positive;

end Hofstadter_Figure_Figure;
```


The implementation of the package internally uses functions which generate an array of Figures or Spaces:

```Ada
package body Hofstadter_Figure_Figure is

   type Positive_Array is array (Positive range <>) of Positive;

   function FFR(P: Positive) return Positive_Array is
      Figures: Positive_Array(1 .. P+1);
      Space: Positive := 2;
      Space_Index: Positive := 2;
   begin
      Figures(1) := 1;
      for I in 2 .. P loop
         Figures(I) := Figures(I-1) + Space;
         Space := Space+1;
         while Space = Figures(Space_Index) loop
            Space := Space + 1;
            Space_Index := Space_Index + 1;
         end loop;
      end loop;
      return Figures(1 .. P);
   end FFR;

   function FFR(P: Positive) return Positive is
      Figures: Positive_Array(1 .. P) := FFR(P);
   begin
      return Figures(P);
   end FFR;

   function FFS(P: Positive) return Positive_Array is
      Spaces:  Positive_Array(1 .. P);
      Figures: Positive_Array := FFR(P+1);
      J: Positive := 1;
      K: Positive := 1;
   begin
      for I in Spaces'Range loop
         while J = Figures(K) loop
            J := J + 1;
            K := K + 1;
         end loop;
         Spaces(I) := J;
         J := J + 1;
      end loop;
      return Spaces;
   end FFS;

   function FFS(P: Positive) return Positive is
      Spaces: Positive_Array := FFS(P);
   begin
      return Spaces(P);
   end FFS;

end Hofstadter_Figure_Figure;
```


Finally, a test program for the package, solving the task at hand:

```Ada
with Ada.Text_IO, Hofstadter_Figure_Figure;

procedure Test_HSS is

   use Hofstadter_Figure_Figure;

   A: array(1 .. 1000) of Boolean := (others => False);
   J: Positive;

begin
   for I in 1 .. 10 loop
      Ada.Text_IO.Put(Integer'Image(FFR(I)));
   end loop;
   Ada.Text_IO.New_Line;

   for I in 1 .. 40 loop
      J := FFR(I);
      if A(J) then
         raise Program_Error with Positive'Image(J) & " used twice";
      end if;
      A(J) := True;
   end loop;

   for I in 1 .. 960 loop
      J := FFS(I);
      if A(J) then
         raise Program_Error with Positive'Image(J) & " used twice";
      end if;
      A(J) := True;
   end loop;

   for I in A'Range loop
      if not A(I) then raise Program_Error with Positive'Image(I) & " unused";
      end if;
   end loop;
   Ada.Text_IO.Put_Line("Test Passed: No overlap between FFR(I) and FFS(J)");

exception
   when Program_Error => Ada.Text_IO.Put_Line("Test Failed"); raise;
end Test_HSS;
```


The output of the test program:
<lang> 1 3 7 12 18 26 35 45 56 69
Test Passed: No overlap between FFR(I) and FFS(J)
```



## AutoHotkey


```AutoHotkey
R(n){
	if n=1
		return 1
	return R(n-1) + S(n-1)
}

S(n){
	static ObjR:=[]
	if n=1
		return 2
	ObjS:=[]
	loop, % n
		ObjR[R(A_Index)] := true
	loop, % n-1
		ObjS[S(A_Index)] := true
	Loop
		if !(ObjR[A_Index]||ObjS[A_Index])
			return A_index
}
```

Examples:
```AutoHotkey
Loop
	MsgBox, 262144, , % "R(" A_Index ") = " R(A_Index) "`nS(" A_Index ") = " S(A_Index)
```

Outputs:
```txt
R(1) = 1, 3, 7, 12, 18, 26, 35,...
S(1) = 2, 4, 5,  6,  8,  9, 10,...
```



## AWK


```awk
# Hofstadter Figure-Figure sequences
#
#    R(1) = 1; S(1) = 2;
#    R(n) = R(n-1) + S(n-1), n > 1
#    S(n) is the values not in R(n)

BEGIN {

    # start with the first two values of R and S to simplify finding S[n]:
    R[ 1 ] = 1;
    R[ 2 ] = 3;
    S[ 1 ] = 2;
    S[ 2 ] = 4;
    # maximum n we currently have of R and S
    rMax   = 2;
    sMax   = 2;

    # calculate and show the first 10 values of R:
    printf( "R[1..10]:" );
    for( n = 1; n < 11; n ++ )
    {
        printf( " %d", ffr( n ) );
    }
    printf( "\n" );
    # check that R[1..40] and S[1..960] contain the numbers 1..1000 once each
    # add the values of R[ 1..40 ] to the set V
    for( n = 1; n <= 40; n ++ )
    {
        V[ ffr( n ) ] ++;
    }
    # add the values of S[ 1..960 ] to the set V
    for( n = 1; n <= 960; n ++ )
    {
        V[ ffs( n ) ] ++;
    }
    # check all numbers are present and not duplicated
    ok = 1;
    for( n = 1; n <= 1000; n ++ )
    {
        if( ! ( n in V ) )
        {
            printf( "%d not present in R[1..40], S[1..960]\n", n );
            ok = 0;
        }
        else if( V[ n ] != 1 )
        {
            printf( "%d occurs %d times in R[1..40], S[1..960]\n", n, V[ n ] );
            ok = 0;
        }
    }
    if( ok )
    {
        printf( "R[1..40] and S[1..960] uniquely contain all 1..1000\n" );
    }

} # BEGIN

function ffr( n )
{
    # calculate R[n]
    if( ! ( n in R ) )
    {
        # we haven't calculated R[ n ] yet
        R[ n ]  = ffs( n - 1 );
        R[ n ] += ffr( n - 1 );
    }
return R[ n ];
} # ffr

function ffs( n )
{
    # calculate S[n]
    if( ! ( n in S ) )
    {
        # starting at the highest known R, calculate the next one and fill in the S values
        # continuing until we have enough S values
        do
        {
            R[ rMax + 1 ] = R[ rMax ] + S[ rMax ];
            for( sValue = R[ rMax ] + 1; sValue < R[ rMax + 1 ]; sValue ++ )
            {
                S[ sMax ++ ] = sValue;
            }
            rMax ++;
        }
        while( sMax < n );
    }
return S[ n ];
} # ffs
```

{{out}}

```txt

R[1..10]: 1 3 7 12 18 26 35 45 56 69
R[1..40] and S[1..960] uniquely contain all 1..1000

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      PRINT "First 10 values of R:"
      FOR i% = 1 TO 10 : PRINT ;FNffr(i%) " "; : NEXT : PRINT
      PRINT "First 10 values of S:"
      FOR i% = 1 TO 10 : PRINT ;FNffs(i%) " "; : NEXT : PRINT
      PRINT "Checking for first 1000 integers:"
      r% = 1 : s% = 1
      ffr% = FNffr(r%)
      ffs% = FNffs(s%)
      FOR wanted% = 1 TO 1000
        CASE TRUE OF
          WHEN wanted% = ffr% : r% += 1 : ffr% = FNffr(r%)
          WHEN wanted% = ffs% : s% += 1 : ffs% = FNffs(s%)
          OTHERWISE: EXIT FOR
        ENDCASE
      NEXT
      IF r% = 41 AND s% = 961 PRINT "Test passed" ELSE PRINT "Test failed"
      END

      DEF FNffr(N%)
      LOCAL I%, J%, R%, S%, V%
      DIM V% LOCAL 2*N%+1
      V%?1 = 1
      IF N% = 1 THEN = 1
      R% = 1
      S% = 2
      FOR I% = 2 TO N%
        FOR J% = S% TO 2*N%
          IF V%?J% = 0 EXIT FOR
        NEXT
        V%?J% = 1
        S% = J%
        R% += S%
        IF R% <= 2*N% V%?R% = 1
      NEXT I%
      = R%

      DEF FNffs(N%)
      LOCAL I%, J%, R%, S%, V%
      DIM V% LOCAL 2*N%+1
      V%?1 = 1
      IF N% = 1 THEN = 2
      R% = 1
      S% = 2
      FOR I% = 1 TO N%
        FOR J% = S% TO 2*N%
          IF V%?J% = 0 EXIT FOR
        NEXT
        V%?J% = 1
        S% = J%
        R% += S%
        IF R% <= 2*N% V%?R% = 1
      NEXT I%
      = S%
```


```txt

First 10 values of R:
1 3 7 12 18 26 35 45 56 69
First 10 values of S:
2 4 5 6 8 9 10 11 13 14
Checking for first 1000 integers:
Test passed

```



## C


```c
#include <stdio.h>
#include <stdlib.h>

// simple extensible array stuff
typedef unsigned long long xint;

typedef struct {
	size_t len, alloc;
	xint *buf;
} xarray;

xarray rs, ss;

void setsize(xarray *a, size_t size)
{
	size_t n = a->alloc;
	if (!n) n = 1;

	while (n < size) n <<= 1;
	if (a->alloc < n) {
		a->buf = realloc(a->buf, sizeof(xint) * n);
		if (!a->buf) abort();
		a->alloc = n;
	}
}

void push(xarray *a, xint v)
{
	while (a->alloc <= a->len)
		setsize(a, a->alloc * 2);

	a->buf[a->len++] = v;
}


// sequence stuff
void RS_append(void);

xint R(int n)
{
	while (n > rs.len) RS_append();
	return rs.buf[n - 1];
}

xint S(int n)
{
	while (n > ss.len) RS_append();
	return ss.buf[n - 1];
}

void RS_append()
{
	int n = rs.len;
	xint r = R(n) + S(n);
	xint s = S(ss.len);

	push(&rs, r);
	while (++s < r) push(&ss, s);
	push(&ss, r + 1); // pesky 3
}

int main(void)
{
	push(&rs, 1);
	push(&ss, 2);

	int i;
	printf("R(1 .. 10):");
	for (i = 1; i <= 10; i++)
		printf(" %llu", R(i));

	char seen[1001] = { 0 };
	for (i = 1; i <=  40; i++) seen[ R(i) ] = 1;
	for (i = 1; i <= 960; i++) seen[ S(i) ] = 1;
	for (i = 1; i <= 1000 && seen[i]; i++);

	if (i <= 1000) {
		fprintf(stderr, "%d not seen\n", i);
		abort();
	}

	puts("\nfirst 1000 ok");
	return 0;
}
```



## C++

{{works with|gcc}}
{{works with|C++|11, 14, 17}}

```cpp
#include <iomanip>
#include <iostream>
#include <set>
#include <vector>

using namespace std;

unsigned hofstadter(unsigned rlistSize, unsigned slistSize)
{
    auto n = rlistSize > slistSize ? rlistSize : slistSize;
    auto rlist = new vector<unsigned> { 1, 3, 7 };
    auto slist = new vector<unsigned> { 2, 4, 5, 6 };
    auto list = rlistSize > 0 ? rlist : slist;
    auto target_size = rlistSize > 0 ? rlistSize : slistSize;

    while (list->size() > target_size) list->pop_back();

    while (list->size() < target_size)
    {
        auto lastIndex = rlist->size() - 1;
        auto lastr = (*rlist)[lastIndex];
        auto r = lastr + (*slist)[lastIndex];
        rlist->push_back(r);
        for (auto s = lastr + 1; s < r && list->size() < target_size;)
            slist->push_back(s++);
    }

    auto v = (*list)[n - 1];
    delete rlist;
    delete slist;
    return v;
}

ostream& operator<<(ostream& os, const set<unsigned>& s)
{
    cout << '(' << s.size() << "):";
    auto i = 0;
    for (auto c = s.begin(); c != s.end();)
    {
        if (i++ % 20 == 0) os << endl;
        os << setw(5) << *c++;
    }
    return os;
}

int main(int argc, const char* argv[])
{
    const auto v1 = atoi(argv[1]);
    const auto v2 = atoi(argv[2]);
    set<unsigned> r, s;
    for (auto n = 1; n <= v2; n++)
    {
        if (n <= v1)
            r.insert(hofstadter(n, 0));
        s.insert(hofstadter(0, n));
    }
    cout << "R" << r << endl;
    cout << "S" << s << endl;

    int m = max(*r.rbegin(), *s.rbegin());
    for (auto n = 1; n <= m; n++)
        if (r.count(n) == s.count(n))
            clog << "integer " << n << " either in both or neither set" << endl;

    return 0;
}
```

{{out}}


```sh
% ./hofstadter 40 100 2> /dev/null
R(40):
    1    3    7   12   18   26   35   45   56   69   83   98  114  131  150  170  191  213  236  260
  285  312  340  369  399  430  462  495  529  565  602  640  679  719  760  802  845  889  935  982
S(100):
    2    4    5    6    8    9   10   11   13   14   15   16   17   19   20   21   22   23   24   25
   27   28   29   30   31   32   33   34   36   37   38   39   40   41   42   43   44   46   47   48
   49   50   51   52   53   54   55   57   58   59   60   61   62   63   64   65   66   67   68   70
   71   72   73   74   75   76   77   78   79   80   81   82   84   85   86   87   88   89   90   91
   92   93   94   95   96   97   99  100  101  102  103  104  105  106  107  108  109  110  111  112
```


=={{header|C sharp|C#}}==
Creates an IEnumerable for R and S and uses those to complete the task

```Csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace HofstadterFigureFigure
{
	class HofstadterFigureFigure
	{
		readonly List<int> _r = new List<int>() {1};
		readonly List<int> _s = new List<int>();

		public IEnumerable<int> R()
		{
			int iR = 0;
			while (true)
			{
				if (iR >= _r.Count)
				{
					Advance();
				}
				yield return _r[iR++];
			}
		}

		public IEnumerable<int> S()
		{
			int iS = 0;
			while (true)
			{
				if (iS >= _s.Count)
				{
					Advance();
				}
				yield return _s[iS++];
			}
		}

		private void Advance()
		{
			int rCount = _r.Count;
			int oldR = _r[rCount - 1];
			int sVal;

			// Take care of first two cases specially since S won't be larger than R at that point
			switch (rCount)
			{
				case 1:
					sVal = 2;
					break;
				case 2:
					sVal = 4;
					break;
				default:
					sVal = _s[rCount - 1];
					break;
			}
			_r.Add(_r[rCount - 1] + sVal);
			int newR = _r[rCount];
			for (int iS = oldR + 1; iS < newR; iS++)
			{
				_s.Add(iS);
			}
		}
	}

	class Program
	{
		static void Main()
		{
			var hff = new HofstadterFigureFigure();
			var rs = hff.R();
			var arr = rs.Take(40).ToList();

			foreach(var v in arr.Take(10))
			{
				Console.WriteLine("{0}", v);
			}

			var hs = new HashSet<int>(arr);
			hs.UnionWith(hff.S().Take(960));
			Console.WriteLine(hs.Count == 1000 ? "Verified" : "Oops!  Something's wrong!");
		}
	}
}

```

Output:

```txt
1
3
7
12
18
26
35
45
56
69
Verified
```



## CoffeeScript

{{trans|Ruby}}

```coffeescript
R = [ null, 1 ]
S = [ null, 2 ]

extend_sequences = (n) ->
  current = Math.max(R[R.length - 1], S[S.length - 1])
  i = undefined
  while R.length <= n or S.length <= n
    i = Math.min(R.length, S.length) - 1
    current += 1
    if current == R[i] + S[i]
      R.push current
    else
      S.push current

ff = (X, n) ->
    extend_sequences n
    X[n]

console.log 'R(' + i + ') = ' + ff(R, i) for i in [1..10]
int_array = ([1..40].map (i) -> ff(R, i)).concat [1..960].map (i) -> ff(S, i)
int_array.sort (a, b) -> a - b

for i in [1..1000]
  if int_array[i - 1] != i
    throw 'Something\'s wrong!'
console.log '1000 integer check ok.'
```

{{out}}
As JavaScript.


## Common Lisp


```lisp
;;; equally doable with a list
(flet ((seq (i) (make-array 1 :element-type 'integer
			      :initial-element i
			      :fill-pointer 1
			      :adjustable t)))
  (let ((rr (seq 1)) (ss (seq 2)))
    (labels ((extend-r ()
		       (let* ((l (1- (length rr)))
			      (r (+ (aref rr l) (aref ss l)))
			      (s (elt ss (1- (length ss)))))
			 (vector-push-extend r rr)
			 (loop while (<= s r) do
			       (if (/= (incf s) r)
				 (vector-push-extend s ss))))))
      (defun seq-r (n)
	(loop while (> n (length rr)) do (extend-r))
	(elt rr (1- n)))

      (defun seq-s (n)
	(loop while (> n (length ss)) do (extend-r))
	(elt ss (1- n))))))

(defun take (f n)
  (loop for x from 1 to n collect (funcall f x)))

(format t "First of R: ~a~%" (take #'seq-r 10))

(mapl (lambda (l) (if (and (cdr l)
			   (/= (1+ (car l)) (cadr l)))
		    (error "not in sequence")))
      (sort (append (take #'seq-r 40)
		    (take #'seq-s 960))
	    #'<))
(princ "Ok")
```

{{out}}

```txt
First of R: (1 3 7 12 18 26 35 45 56 69)
Ok
```



## D

{{trans|Go}}

```d
int delegate(in int) nothrow ffr, ffs;

nothrow static this() {
    auto r = [0, 1], s = [0, 2];

    ffr = (in int n) nothrow {
        while (r.length <= n) {
            immutable int nrk = r.length - 1;
            immutable int rNext = r[nrk] + s[nrk];
            r ~= rNext;
            foreach (immutable sn; r[nrk] + 2 .. rNext)
                s ~= sn;
            s ~= rNext + 1;
        }
        return r[n];
    };

    ffs = (in int n) nothrow {
        while (s.length <= n)
            ffr(r.length);
        return s[n];
    };
}

void main() {
    import std.stdio, std.array, std.range, std.algorithm;

    iota(1, 11).map!ffr.writeln;
    auto t = iota(1, 41).map!ffr.chain(iota(1, 961).map!ffs);
    t.array.sort().equal(iota(1, 1001)).writeln;
}
```

{{out}}

```txt
[1, 3, 7, 12, 18, 26, 35, 45, 56, 69]
true
```


### Alternative version

{{trans|Python}}
(Same output)

```d
import std.stdio, std.array, std.range, std.algorithm;

struct ffr {
    static r = [int.min, 1];

    static int opCall(in int n) nothrow {
        assert(n > 0);
        if (n < r.length) {
            return r[n];
        } else {
            immutable int ffr_n_1 = ffr(n - 1);
            immutable int lastr = r[$ - 1];
            // Extend s up to, and one past, last r.
            ffs.s ~= iota(ffs.s[$ - 1] + 1, lastr).array;
            if (ffs.s[$ - 1] < lastr)
                ffs.s ~= lastr + 1;
            // Access s[n - 1] temporarily extending s if necessary.
            immutable size_t len_s = ffs.s.length;
            immutable int ffs_n_1 = (len_s > n) ?
                                    ffs.s[n - 1] :
                                    (n - len_s) + ffs.s[$ - 1];
            immutable int ans = ffr_n_1 + ffs_n_1;
            r ~= ans;
            return ans;
        }
    }
}

struct ffs {
    static s = [int.min, 2];

    static int opCall(in int n) nothrow {
        assert(n > 0);
        if (n < s.length) {
            return s[n];
        } else {
            foreach (immutable i; ffr.r.length .. n + 2) {
                ffr(i);
                if (s.length > n)
                    return s[n];
            }
            assert(false, "Whoops!");
        }
    }
}

void main() {
    iota(1, 11).map!ffr.writeln;
    auto t = iota(1, 41).map!ffr.chain(iota(1, 961).map!ffs);
    t.array.sort().equal(iota(1, 1001)).writeln;
}
```



## EchoLisp


```scheme
(define (FFR n)
	(+ (FFR (1- n)) (FFS (1- n))))

(define (FFS n)
	(define next (1+ (FFS (1- n))))
	(for ((k (in-naturals next)))
		  #:break (not (vector-search* k (cache 'FFR))) => k
		  ))

(remember 'FFR #(0 1)) ;; init cache
(remember 'FFS #(0 2))

```

{{out}}

```scheme

(define-macro m-range [a .. b] (range a (1+ b)))

(map FFR [1 .. 10])
    → (1 3 7 12 18 26 35 45 56 69)

;; checking
(equal? [1 .. 1000] (list-sort < (append (map FFR [1 .. 40]) (map FFS [1 .. 960]))))
    → #t
```



## Euler Math Toolbox



```Euler Math Toolbox

>function RSstep (r,s) ...
$  n=cols(r);
$  r=r|(r[n]+s[n]);
$  s=s|(max(s[n]+1,r[n]+1):r[n+1]-1);
$  return {r,s};
$  endfunction
>function RS (n) ...
$  if n==1 then return {[1],[2]}; endif;
$  if n==2 then return {[1,3],[2]}; endif;
$  r=[1,3]; s=[2,4];
$  loop 3 to n; {r,s}=RSstep(r,s); end;
$  return {r,s};
$  endfunction
>{r,s}=RS(10);
>r
 [ 1  3  7  12  18  26  35  45  56  69 ]
>{r,s}=RS(50);
>all(sort(r[1:40]|s[1:960])==(1:1000))
 1

```



## Factor

We keep lists S and R, and increment them when necessary.

```factor
SYMBOL: S  V{ 2 } S set
SYMBOL: R  V{ 1 } R set

: next ( s r -- news newr )
2dup [ last ] bi@ + suffix
dup [
  [ dup last 1 + dup ] dip member? [ 1 + ] when suffix
] dip ;

: inc-SR ( n -- )
dup 0 <=
[ drop ]
[ [ S get R get ] dip  [ next ] times  R set S set ]
if ;

: ffs ( n -- S(n) )
dup S get length - inc-SR
1 - S get nth ;
: ffr ( n -- R(n) )
dup R get length - inc-SR
1 - R get nth ;
```



```factor
( scratchpad ) 10 iota [ 1 + ffr ] map .
{ 1 3 7 12 18 26 35 45 56 69 }
( scratchpad ) 40 iota [ 1 + ffr ] map  960 iota [ 1 + ffs ] map append  1000 iota 1 v+n set= .
t
```



## Go


```go
package main

import "fmt"

var ffr, ffs func(int) int

// The point of the init function is to encapsulate r and s.  If you are
// not concerned about that or do not want that, r and s can be variables at
// package level and ffr and ffs can be ordinary functions at package level.
func init() {
    // task 1, 2
    r := []int{0, 1}
    s := []int{0, 2}

    ffr = func(n int) int {
        for len(r) <= n {
            nrk := len(r) - 1       // last n for which r(n) is known
            rNxt := r[nrk] + s[nrk] // next value of r:  r(nrk+1)
            r = append(r, rNxt)     // extend sequence r by one element
            for sn := r[nrk] + 2; sn < rNxt; sn++ {
                s = append(s, sn)   // extend sequence s up to rNext
            }
            s = append(s, rNxt+1)   // extend sequence s one past rNext
        }
        return r[n]
    }

    ffs = func(n int) int {
        for len(s) <= n {
            ffr(len(r))
        }
        return s[n]
    }
}

func main() {
    // task 3
    for n := 1; n <= 10; n++ {
        fmt.Printf("r(%d): %d\n", n, ffr(n))
    }
    // task 4
    var found [1001]int
    for n := 1; n <= 40; n++ {
        found[ffr(n)]++
    }
    for n := 1; n <= 960; n++ {
        found[ffs(n)]++
    }
    for i := 1; i <= 1000; i++ {
        if found[i] != 1 {
            fmt.Println("task 4: FAIL")
            return
        }
    }
    fmt.Println("task 4: PASS")
}
```

{{out}}

```txt

r(1): 1
r(2): 3
r(3): 7
r(4): 12
r(5): 18
r(6): 26
r(7): 35
r(8): 45
r(9): 56
r(10): 69
task 4: PASS
```


The following defines two mutually recursive generators without caching results.  Each generator will end up dragging a tree of closures behind it, but due to the odd nature of the two series' growth pattern, it's still a heck of a lot faster than the above method when producing either series in sequence.

```go
package main
import "fmt"

type xint int64
func R() (func() (xint)) {
	r, s := xint(0), func() (xint) (nil)
	return func() (xint) {
		switch {
		case r < 1: r = 1
		case r < 3: r = 3
		default:
			if s == nil {
				s = S()
				s()
			}
			r += s()
		}
		if r < 0 { panic("r overflow") }
		return r
	}
}

func S() (func() (xint)) {
	s, r1, r := xint(0), xint(0), func() (xint) (nil)
	return func() (xint) {
		if s < 2 {
			s = 2
		} else {
			if r == nil {
				r = R()
				r()
				r1 = r()
			}
			s++
			if s >  r1 { r1 = r() }
			if s == r1 { s++ }
		}
		if s < 0 { panic("s overflow") }
		return s
	}
}

func main() {
	r, sum := R(), xint(0)
	for i := 0; i < 10000000; i++ {
		sum += r()
	}
	fmt.Println(sum)
}
```



## Haskell


```haskell
import Data.List (delete, sort)

-- Functions by Reinhard Zumkeller
ffr :: Int -> Int
ffr n = rl !! (n - 1)
  where
    rl = 1 : fig 1 [2 ..]
    fig n (x:xs) = n_ : fig n_ (delete n_ xs)
      where
        n_ = n + x

ffs :: Int -> Int
ffs n = rl !! n
  where
    rl = 2 : figDiff 1 [2 ..]
    figDiff n (x:xs) = x : figDiff n_ (delete n_ xs)
      where
        n_ = n + x

main :: IO ()
main = do
  print $ ffr <$> [1 .. 10]
  let i1000 = sort (fmap ffr [1 .. 40] ++ fmap ffs [1 .. 960])
  print (i1000 == [1 .. 1000])
```

Output:

```txt
[1,3,7,12,18,26,35,45,56,69]
True
```


Defining R and S literally:

```haskell
import Data.List (sort)

r :: [Int]
r = scanl (+) 1 s

s :: [Int]
s = 2 : 4 : tail (complement (tail r))
  where
    complement = concat . interval
    interval x = zipWith (\x y -> [succ x .. pred y]) x (tail x)

main :: IO ()
main = do
  putStr "R: "
  print (take 10 r)
  putStr "S: "
  print (take 10 s)
  putStr "test 1000: "
  print $ [1 .. 1000] == sort (take 40 r ++ take 960 s)
```

output:

```txt

R: [1,3,7,12,18,26,35,45,56,69]
S: [2,4,5,6,8,9,10,11,13,14]
test 1000: True

```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
link printf,ximage

procedure main()
   printf("Hofstader ff sequences R(n:= 1 to %d)\n",N := 10)
   every printf("R(%d)=%d\n",n := 1 to N,ffr(n))

   L := list(N := 1000,0)
   zero := dup := oob := 0
   every n := 1 to (RN := 40) do
      if not L[ffr(n)] +:= 1 then    # count R occurrence
         oob +:= 1                   # count out of bounds

   every n := 1 to (N-RN) do
      if not L[ffs(n)] +:= 1 then    # count S occurrence
         oob +:= 1                   # count out of bounds

   every zero +:= (!L = 0)           # count zeros / misses
   every dup  +:= (!L > 1)           # count > 1's / duplicates

   printf("Results of R(1 to %d) and S(1 to %d) coverage is ",RN,(N-RN))
   if oob+zero+dup=0 then
      printf("complete.\n")
   else
      printf("flawed\noob=%i,zero=%i,dup=%i\nL:\n%s\nR:\n%s\nS:\n%s\n",
             oob,zero,dup,ximage(L),ximage(ffr(ffr)),ximage(ffs(ffs)))
end

procedure ffr(n)
static R,S
initial {
   R := [1]
   S := ffs(ffs)               # get access to S in ffs
   }

   if n === ffr then return R  # secret handshake to avoid globals :)

   if integer(n) > 0 then
      return R[n] | put(R,ffr(n-1) + ffs(n-1))[n]
end

procedure ffs(n)
static R,S
initial {
   S := [2]
   R := ffr(ffr)               # get access to R in ffr
   }

   if n === ffs then return S  # secret handshake to avoid globals :)

   if integer(n) > 0 then {
      if S[n] then return S[n]
      else {
         t := S[*S]
         until *S = n do
            if (t +:= 1) = !R then next # could be optimized with more code
            else return put(S,t)[*S]    # extend S
         }
   }
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]
[http://www.cs.arizona.edu/icon/library/src/procs/ximage.icn ximage.icn allows formatting entire structures]

Output:
```txt
Hofstader ff sequences R(n:= 1 to 10)
R(1)=1
R(2)=3
R(3)=7
R(4)=12
R(5)=18
R(6)=26
R(7)=35
R(8)=45
R(9)=56
R(10)=69
Results of R(1 to 40) and S(1 to 960) coverage is complete.
```



## J



```j
R=: 1 1 3
S=: 0 2 4
FF=: 3 :0
  while. +./y>:R,&#S do.
    R=: R,({:R)+(<:#R){S
    S=: (i.<:+/_2{.R)-.R
  end.
  R;S
)
ffr=: { 0 {:: FF@(>./@,)
ffs=: { 1 {:: FF@(0,>./@,)
```


Required examples:


```j
   ffr 1+i.10
1 3 7 12 18 26 35 45 56 69
   (1+i.1000) -: /:~ (ffr 1+i.40), ffs 1+i.960
1
```



## Java


'''Code:'''


```java
import java.util.*;

class Hofstadter
{
  private static List<Integer> getSequence(int rlistSize, int slistSize)
  {
    List<Integer> rlist = new ArrayList<Integer>();
    List<Integer> slist = new ArrayList<Integer>();
    Collections.addAll(rlist, 1, 3, 7);
    Collections.addAll(slist, 2, 4, 5, 6);
    List<Integer> list = (rlistSize > 0) ? rlist : slist;
    int targetSize = (rlistSize > 0) ? rlistSize : slistSize;
    while (list.size() > targetSize)
      list.remove(list.size() - 1);
    while (list.size() < targetSize)
    {
      int lastIndex = rlist.size() - 1;
      int lastr = rlist.get(lastIndex).intValue();
      int r = lastr + slist.get(lastIndex).intValue();
      rlist.add(Integer.valueOf(r));
      for (int s = lastr + 1; (s < r) && (list.size() < targetSize); s++)
        slist.add(Integer.valueOf(s));
    }
    return list;
  }

  public static int ffr(int n)
  {  return getSequence(n, 0).get(n - 1).intValue();  }

  public static int ffs(int n)
  {  return getSequence(0, n).get(n - 1).intValue();  }

  public static void main(String[] args)
  {
    System.out.print("R():");
    for (int n = 1; n <= 10; n++)
      System.out.print(" " + ffr(n));
    System.out.println();

    Set<Integer> first40R = new HashSet<Integer>();
    for (int n = 1; n <= 40; n++)
      first40R.add(Integer.valueOf(ffr(n)));

    Set<Integer> first960S = new HashSet<Integer>();
    for (int n = 1; n <= 960; n++)
      first960S.add(Integer.valueOf(ffs(n)));

    for (int i = 1; i <= 1000; i++)
    {
      Integer n = Integer.valueOf(i);
      if (first40R.contains(n) == first960S.contains(n))
        System.out.println("Integer " + i + " either in both or neither set");
    }
    System.out.println("Done");
  }
}
```


'''Output:'''


```txt
R(): 1 3 7 12 18 26 35 45 56 69
Done
```



## JavaScript

{{trans|Ruby}}

```JavaScript
var R = [null, 1];
var S = [null, 2];

var extend_sequences = function (n) {
	var current = Math.max(R[R.length-1],S[S.length-1]);
	var i;
	while (R.length <= n || S.length <= n) {
		i = Math.min(R.length, S.length) - 1;
		current += 1;
		if (current === R[i] + S[i]) {
			R.push(current);
		} else {
			S.push(current);
		}
	}
}

var ffr = function(n) {
	extend_sequences(n);
	return R[n];
};

var ffs = function(n) {
	extend_sequences(n);
	return S[n];
};

for (var i = 1; i <=10; i += 1) {
   console.log('R('+ i +') = ' + ffr(i));
}

var int_array = [];

for (var i = 1; i <= 40; i += 1) {
	int_array.push(ffr(i));
}
for (var i = 1; i <= 960; i += 1) {
	int_array.push(ffs(i));
}

int_array.sort(function(a,b){return a-b;});

for (var i = 1; i <= 1000; i += 1) {
	if (int_array[i-1] !== i) {
		throw "Something's wrong!"
	} else { console.log("1000 integer check ok."); }
}
```

Output:

```txt
R(1) = 1
R(2) = 3
R(3) = 7
R(4) = 12
R(5) = 18
R(6) = 26
R(7) = 35
R(8) = 45
R(9) = 56
R(10) = 69
1000 integer check ok.
```



## Julia

Much of this task would seem to lend itself to an iterator based solution.  However, the first step calls for <tt>ffr(n)</tt> and <tt>ffs(n)</tt>, which imply that the series values are to be "randomly" rather than "sequentially" accessed.  Given this implied requirement, I chose to implement <tt>ffr</tt> and <tt>ffs</tt> as closures containing the type (data structure) <tt>FigureFigure</tt>, which are used to calculate their values as required.  I address task requirement 2 (no maximum n) by having these functions extend this data structure as needed to accommodate values of n larger than those used for their creation.

'''Functions'''

```Julia

type FigureFigure{T<:Integer}
    r::Array{T,1}
    rnmax::T
    snmax::T
    snext::T
end

function grow!{T<:Integer}(ff::FigureFigure{T}, rnmax::T=100)
    ff.rnmax < rnmax || return nothing
    append!(ff.r, zeros(T, (rnmax-ff.rnmax)))
    snext = ff.snext
    for i in (ff.rnmax+1):rnmax
        ff.r[i] = ff.r[i-1] + snext
        snext += 1
        while snext in ff.r
            snext += 1
        end
    end
    ff.rnmax = rnmax
    ff.snmax = ff.r[end] - rnmax
    ff.snext = snext
    return nothing
end

function FigureFigure{T<:Integer}(rnmax::T=10)
    ff = FigureFigure([1], 1, 0, 2)
    grow!(ff, rnmax)
    return ff
end

function FigureFigure{T<:Integer}(rnmax::T, snmax::T)
    ff = FigureFigure(rnmax)
    while ff.snmax < snmax
        grow!(ff, 2ff.rnmax)
    end
    return ff
end

function make_ffr{T<:Integer}(nmax::T=10)
    ff = FigureFigure(nmax)
    function ffr{T<:Integer}(n::T)
        if n > ff.rnmax
            grow!(ff, 2n)
        end
        ff.r[n]
    end
end

function make_ffs{T<:Integer}(nmax::T=100)
    ff = FigureFigure(13, nmax)
    function ffs{T<:Integer}(n::T)
        while ff.snmax < n
            grow!(ff, 2ff.rnmax)
        end
        s = n
        for r in ff.r
            r <= s || return s
            s += 1
        end
    end
end

```


'''Main'''

```Julia

NR = 40
NS = 960
ffr = make_ffr(NR)
ffs = make_ffs(NS)

hi = 10
print("The first ", hi, " values of R are:\n    ")
for i in 1:hi
    print(ffr(i), "  ")
end
println()

tally = falses(NR+NS)
iscontained = true
for i in 1:NR
    try
        tally[ffr(i)] = true
    catch
        iscontained = false
    end
end
for i in 1:NS
    try
        tally[ffs(i)] = true
    catch
        iscontained = false
    end
end

println()
print("The first ", NR, " values of R and ", NS, " of S are ")
if !iscontained
    print("not ")
end
println("contained in the interval 1:", NR+NS, ".")
print("These values ")
if !all(tally)
    print("do not ")
end
println("cover the entire interval.")

```


{{out}}

```txt

The first 10 values of R are:
    1  3  7  12  18  26  35  45  56  69

The first 40 values of R and 960 of S are contained in the interval 1:1000.
These values cover the entire interval.

```


=={{header|Kotlin}} ==
Translated from Java.

```scala
fun ffr(n: Int) = get(n, 0)[n - 1]

fun ffs(n: Int) = get(0, n)[n - 1]

internal fun get(rSize: Int, sSize: Int): List<Int> {
    val rlist = arrayListOf(1, 3, 7)
    val slist = arrayListOf(2, 4, 5, 6)
    val list = if (rSize > 0) rlist else slist
    val targetSize = if (rSize > 0) rSize else sSize

    while (list.size > targetSize)
        list.removeAt(list.size - 1)
    while (list.size < targetSize) {
        val lastIndex = rlist.lastIndex
        val lastr = rlist[lastIndex]
        val r = lastr + slist[lastIndex]
        rlist += r
        var s = lastr + 1
        while (s < r && list.size < targetSize)
            slist += s++
    }
    return list
}

fun main(args: Array<String>) {
    print("R():")
    (1..10).forEach { print(" " + ffr(it)) }
    println()

    val first40R = (1..40).map { ffr(it) }
    val first960S = (1..960).map { ffs(it) }
    val indices = (1..1000).filter  { it in first40R == it in first960S }
    indices.forEach { println("Integer $it either in both or neither set") }
    println("Done")
}
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

1. Create two functions named ffr and ffs that when given n return R(n) or S(n) respectively.
    The instructions call for two functions.
    Because S[n] is generated while computing R[n], one would normally avoid redundancy by combining
    R and S into a single function that returns both sequences.

2. No maximum value for n should be assumed.


```Mathematica

 ffr[j_] := Module[{R = {1}, S = 2, k = 1},
    Do[While[Position[R, S] != {}, S++]; k = k + S; S++;
    R = Append[R, k], {n, 1, j - 1}]; R]

 ffs[j_] := Differences[ffr[j + 1]]

```


3. Calculate and show that the first ten values of R are: 1, 3, 7, 12, 18, 26, 35, 45, 56, and 69


```Mathematica

 ffr[10]

 (* out *)
 {1, 3, 7, 12, 18, 26, 35, 45, 56, 69}

```


4. Calculate and show that the first 40 values of ffr plus the first 960 values of ffs include all the integers from 1 to 1000 exactly once.


```Mathematica

 t = Sort[Join[ffr[40], ffs[960]]];

 t == Range[1000]

 (* out *)
 True

```


=={{header|MATLAB}} / {{header|Octave}}==

1. Create two functions named ffr and ffs that when given n return R(n) or S(n) respectively.
2. No maximum value for n should be assumed.


```MATLAB
  function [R,S] = ffr_ffs(N)
    t = [1,0];
    T = 1;
    n = 1;
    %while T<=1000,
    while n<=N,
        R = find(t,n);
        S = find(~t,n);
        T = R(n)+S(n);

        % pre-allocate memory, this improves performance
	if T > length(t), t = [t,zeros(size(t))]; end;

        t(T) = 1;
        n = n + 1;
    end;
    if nargout>0,
      r = max(R);
      s = max(S);
    else
      printf('Sequence R:\n'); disp(R);
      printf('Sequence S:\n'); disp(S);
    end;
  end;
```


3. Calculate and show that the first ten values of R are: 1, 3, 7, 12, 18, 26, 35, 45, 56, and 69


```txt
>>ffr_ffs(10)
Sequence R:
    1    3    7   12   18   26   35   45   56   69
Sequence S:
    2    4    5    6    8    9   10   11   13   14

```


4. This is self-evident from the function definition, but also because R and S are complementary in t and ~t. However, one can also
Calculate and show that the first 40 values of ffr plus the first 960 values of ffs include all the integers from 1 to 1000 exactly once.
Modify the function above in such a way that, instead of r and s, R and S are returned, and run

```txt
  [R1,S1] = ffr_ffs(40);
  [R2,S2] = ffr_ffs(960);
  all(sort([R1,S2])==1:1000)
ans =  1

```



## Nim


```nim
var cr = @[1]
var cs = @[2]

proc extendRS =
  let x = cr[cr.high] + cs[cr.high]
  cr.add x
  for y in cs[cs.high] + 1 .. <x: cs.add y
  cs.add x + 1

proc ffr(n): int =
  assert n > 0
  while n > cr.len: extendRS()
  cr[n - 1]

proc ffs(n): int =
  assert n > 0
  while n > cs.len: extendRS()
  cs[n - 1]

for i in 1..10: stdout.write ffr i," "
echo ""

var bin: array[1..1000, int]
for i in 1..40: inc bin[ffr i]
for i in 1..960: inc bin[ffs i]
var all = true
for x in bin:
  if x != 1:
    all = false
    break

if all: echo "All Integers 1..1000 found OK"
else: echo "All Integers 1..1000 NOT found only once: ERROR"
```

Output:

```txt
/home/deen/git/nim-unsorted/hofstadter
1 3 7 12 18 26 35 45 56 69
All Integers 1..1000 found OK
```



## Oforth


```oforth
tvar: R
ListBuffer new 1 over add R put

tvar: S
ListBuffer new 2 over add S put

: buildnext
| r s current i |
   R at ->r
   S at ->s
   r last  r size s at  + dup ->current  r add
   s last 1+  current 1-  for: i [ i s add ]
   current 1+ s add ;

: ffr(n)
   while ( R at size n < ) [ buildnext ]
   n R at at ;

: ffs(n)
   while ( S at size n < ) [ buildnext ]
   n S at at ;
```


Output :

```txt

>#[ ffr . ] 10 seqEach
1 3 7 12 18 26 35 45 56 69
ok
>#ffr 40 seq map  #ffs 960 seq map  + sort 1000 seq == .
1 ok

```



## Perl

The program produces a table with the first 10 values of R and S.  It also calculates
R(40) which is 982, S(960) which is 1000, and R(41) which is 1030.

Then we go through the first 1000 outputs, mark those which are seen, then check if all values in the range one through one thousand were seen.


```perl
#!perl
use strict;
use warnings;

my @r = ( undef, 1 );
my @s = ( undef, 2 );

sub ffsr {
  my $n = shift;
  while( $#r < $n ) {
    push @r, $s[$#r]+$r[-1];
    push @s, grep { $s[-1]<$_ } $s[-1]+1..$r[-1]-1, $r[-1]+1;
  }
  return $n;
}

sub ffr { $r[ffsr shift] }
sub ffs { $s[ffsr shift] }

printf "  i: R(i) S(i)\n";
printf "
### ========
\n";
printf "%3d:  %3d  %3d\n", $_, ffr($_), ffs($_) for 1..10;
printf "\nR(40)=%3d S(960)=%3d R(41)=%3d\n", ffr(40), ffs(960), ffr(41);

my %seen;
$seen{ffr($_)}++ for 1 .. 40;
$seen{ffs($_)}++ for 1 .. 960;
if( 1000 == keys %seen and grep $seen{$_}, 1 .. 1000 ) {
	print "All occured exactly once.\n";
} else {
	my @missed = grep !$seen{$_}, 1 .. 1000;
	my @dupped = sort { $a <=> $b} grep $seen{$_}>1, keys %seen;
	print "These were missed: @missed\n";
	print "These were duplicated: @dupped\n";
}

```



## Perl 6

{{works with|Rakudo|2018.03}}

```perl6
my %r = 1 => 1;
my %s = 1 => 2;

sub ffr ($n) { %r{$n} //= ffr($n - 1) + ffs($n - 1) }
sub ffs ($n) { %s{$n} //= (grep none(map &ffr, 1..$n), max(%s.values)+1..*)[0] }

my @ffr = map &ffr, 1..*;
my @ffs = map &ffs, 1..*;

say @ffr[^10];
say "Rawks!" if 1...1000 eqv sort |@ffr[^40], |@ffs[^960];
```

Output:

```txt

1 3 7 12 18 26 35 45 56 69
Rawks!
```



## Phix

Initialising such that length(S)>length(F) simplified things significantly.

```Phix
sequence F = {1,3,7},
         S = {2,4,5,6}
integer fmax = 3 -- (ie F[3], ==7, already in S)

forward function ffs(integer n)

function ffr(integer n)
    integer l = length(F)
    while n>l do
        F &= F[l]+ffs(l)
        l += 1
    end while
    return F[n]
end function

function ffs(integer n)
    while n>length(S) do
        fmax += 1
        if fmax>length(F) then {} = ffr(fmax) end if
        S &= tagset(lim:=F[fmax]-1,start:=F[fmax-1]+1)
        -- ie/eg if fmax was 3, then F[2..3] being {3,7}
        --       ==> tagset(lim:=6,start:=4), ie {4,5,6}.
    end while
    return S[n]
end function

{} = ffr(10)    -- (or collect one by one)
?{"The first ten values of R",F[1..10]}
{} = ffr(40)    -- (not actually needed)
{} = ffs(960)
if sort(F[1..40]&S[1..960])=tagset(1000) then
    puts(1,"test passed\n")
else
    puts(1,"some error!\n")
end if
```

{{out}}

```txt

{"The first ten values of R",{1,3,7,12,18,26,35,45,56,69}}
test passed

```



## PicoLisp


```PicoLisp
(setq *RNext 2)

(de ffr (N)
   (cache '(NIL) N
      (if (= 1 N)
         1
         (+ (ffr (dec N)) (ffs (dec N))) ) ) )

(de ffs (N)
   (cache '(NIL) N
      (if (= 1 N)
         2
         (let S (inc (ffs (dec N)))
            (when (= S (ffr *RNext))
               (inc 'S)
               (inc '*RNext) )
            S ) ) ) )
```

Test:

```PicoLisp
: (mapcar ffr (range 1 10))
-> (1 3 7 12 18 26 35 45 56 69)

: (=
   (range 1 1000)
   (sort (conc (mapcar ffr (range 1 40)) (mapcar ffs (range 1 960)))) )
-> T
```



## PL/I


```pli
ffr: procedure (n) returns (fixed binary(31));
   declare n fixed binary (31);
   declare v(2*n+1) bit(1);
   declare (i, j) fixed binary (31);
   declare (r, s) fixed binary (31);

   v = '0'b;
   v(1) = '1'b;

   if n = 1 then return (1);

   r = 1;
   do i = 2 to n;
      do j = 2 to 2*n;
         if v(j) = '0'b then leave;
      end;
      v(j) = '1'b;
      s = j;
      r = r + s;
      if r <= 2*n then v(r) = '1'b;
   end;
   return (r);
end ffr;
```

Output:

```txt

Please type a value for n:
    1    3    7   12   18   26   35   45   56   69   83   98  114  131  150
  170  191  213  236  260  285  312  340  369  399  430  462  495  529  565
  602  640  679  719  760  802  845  889  935  982

```


```pli
ffs: procedure (n) returns (fixed binary (31));
   declare n fixed binary (31);
   declare v(2*n+1) bit(1);
   declare (i, j) fixed binary (31);
   declare (r, s) fixed binary (31);

   v = '0'b;
   v(1) = '1'b;

   if n = 1 then return (2);

   r = 1;
   do i = 1 to n;
      do j = 2 to 2*n;
         if v(j) = '0'b then leave;
      end;
      v(j) = '1'b;
      s = j;
      r = r + s;
      if r <= 2*n then v(r) = '1'b;
   end;
   return (s);
end ffs;
```

Output of first 960 values:

```txt

Please type a value for n:
    2    4    5    6    8    9   10   11   13   14   15   16   17   19   20
   21   22   23   24   25   27   28   29   30   31   32   33   34   36   37
  ...
  986  987  988  989  990  991  992  993  994  995  996  997  998  999 1000

```

Verification using the above procedures:

```pli

   Dcl t(1000) Bit(1) Init((1000)(1)'0'b);
   put skip list ('Verification that the first 40 FFR numbers and the first');
   put skip list ('960 FFS numbers result in the integers 1 to 1000 only.');
   do i = 1 to 40;
      j = ffr(i);
      if t(j) then put skip list ('error, duplicate value at ' || i);
      else t(j) = '1'b;
   end;
   do i = 1 to 960;
      j = ffs(i);
      if t(j) then put skip list ('error, duplicate value at ' || i);
      else t(j) = '1'b;
   end;
   if all(t = '1'b) then put skip list ('passed test');

```

Output:

```txt

Verification that the first 40 FFR numbers and the first
960 FFS numbers result in the integers 1 to 1000 only.
passed test

```



## Prolog


### Constraint Handling Rules

CHR is a programming language created by '''Professor Thom Frühwirth'''.

Works with SWI-Prolog and module '''chr''' written by '''Tom Schrijvers''' and '''Jan Wielemaker'''

```Prolog
:- use_module(library(chr)).

:- chr_constraint ffr/2, ffs/2, hofstadter/1,hofstadter/2.
:- chr_option(debug, off).
:- chr_option(optimize, full).

% to remove duplicates
ffr(N, R1) \ ffr(N, R2) <=> R1 = R2 | true.
ffs(N, R1) \ ffs(N, R2) <=> R1 = R2 | true.

% compute ffr
ffr(N, R), ffr(N1, R1), ffs(N1,S1) ==>
         N > 1, N1 is N - 1 |
	 R is R1 + S1.

% compute ffs
ffs(N, S), ffs(N1,S1) ==>
         N > 1, N1 is N - 1 |
	 V is S1 + 1,
	 (   find_chr_constraint(ffr(_, V)) ->  S is V+1; S = V).

% init
hofstadter(N) ==>  ffr(1,1), ffs(1,2).
% loop
hofstadter(N), ffr(N1, _R), ffs(N1, _S) ==> N1 < N, N2 is N1 +1 |  ffr(N2,_), ffs(N2,_).


```

Output for first task :

```txt
 ?- hofstadter(10), bagof(ffr(X,Y), find_chr_constraint(ffr(X,Y)), L).
ffr(10,69)
ffr(9,56)
ffr(8,45)
ffr(7,35)
ffr(6,26)
ffr(5,18)
ffr(4,12)
ffr(3,7)
ffr(2,3)
ffr(1,1)
ffs(10,14)
ffs(9,13)
ffs(8,11)
ffs(7,10)
ffs(6,9)
ffs(5,8)
ffs(4,6)
ffs(3,5)
ffs(2,4)
ffs(1,2)
hofstadter(10)
L = [ffr(10,69),ffr(9,56),ffr(8,45),ffr(7,35),ffr(6,26),ffr(5,18),ffr(4,12),ffr(3,7),ffr(2,3),ffr(1,1)].

```


Code for the second task

```Prolog
hofstadter :-
	hofstadter(960),
	% fetch the values of ffr
	bagof(Y, X^find_chr_constraint(ffs(X,Y)), L1),
	% fetch the values of ffs
	bagof(Y, X^(find_chr_constraint(ffr(X,Y)), X < 41), L2),
	% concatenate then
	append(L1, L2, L3),
	% sort removing duplicates
	sort(L3, L4),
	% check the correctness of the list
	(   (L4 = [1|_], last(L4, 1000), length(L4, 1000)) -> writeln(ok); writeln(ko)),
	% to remove all pending constraints
	fail.

```

Output for second task

```txt
 ?- hofstadter.
ok
false.

```



## Python


```python
def ffr(n):
    if n < 1 or type(n) != int: raise ValueError("n must be an int >= 1")
    try:
        return ffr.r[n]
    except IndexError:
        r, s = ffr.r, ffs.s
        ffr_n_1 = ffr(n-1)
        lastr = r[-1]
        # extend s up to, and one past, last r
        s += list(range(s[-1] + 1, lastr))
        if s[-1] < lastr: s += [lastr + 1]
        # access s[n-1] temporarily extending s if necessary
        len_s = len(s)
        ffs_n_1 = s[n-1] if len_s > n else (n - len_s) + s[-1]
        ans = ffr_n_1 + ffs_n_1
        r.append(ans)
        return ans
ffr.r = [None, 1]

def ffs(n):
    if n < 1 or type(n) != int: raise ValueError("n must be an int >= 1")
    try:
        return ffs.s[n]
    except IndexError:
        r, s = ffr.r, ffs.s
        for i in range(len(r), n+2):
            ffr(i)
            if len(s) > n:
                return s[n]
        raise Exception("Whoops!")
ffs.s = [None, 2]

if __name__ == '__main__':
    first10 = [ffr(i) for i in range(1,11)]
    assert first10 == [1, 3, 7, 12, 18, 26, 35, 45, 56, 69], "ffr() value error(s)"
    print("ffr(n) for n = [1..10] is", first10)
    #
    bin = [None] + [0]*1000
    for i in range(40, 0, -1):
        bin[ffr(i)] += 1
    for i in range(960, 0, -1):
        bin[ffs(i)] += 1
    if all(b == 1 for b in bin[1:1000]):
        print("All Integers 1..1000 found OK")
    else:
        print("All Integers 1..1000 NOT found only once: ERROR")
```


;Output:

```txt
ffr(n) for n = [1..10] is [1, 3, 7, 12, 18, 26, 35, 45, 56, 69]
All Integers 1..1000 found OK
```



### Alternative


```python
cR = [1]
cS = [2]

def extend_RS():
	x = cR[len(cR) - 1] + cS[len(cR) - 1]
	cR.append(x)
	cS += range(cS[-1] + 1, x)
	cS.append(x + 1)

def ff_R(n):
	assert(n > 0)
	while n > len(cR): extend_RS()
	return cR[n - 1]

def ff_S(n):
	assert(n > 0)
	while n > len(cS): extend_RS()
	return cS[n - 1]

# tests
print([ ff_R(i) for i in range(1, 11) ])

s = {}
for i in range(1, 1001): s[i] = 0
for i in range(1, 41):  del s[ff_R(i)]
for i in range(1, 961): del s[ff_S(i)]

# the fact that we got here without a key error
print("Ok")
```
output<lang>[1, 3, 7, 12, 18, 26, 35, 45, 56, 69]
Ok
```



### Using cyclic iterators

{{trans|Haskell}}
Defining R and S as mutually recursive generators. Follows directly from the definition of the R and S sequences.

```python
from itertools import islice

def R():
	n = 1
	yield n
	for s in S():
		n += s
		yield n;

def S():
	yield 2
	yield 4
	u = 5
	for r in R():
		if r <= u: continue;
		for x in range(u, r): yield x
		u = r + 1

def lst(s, n): return list(islice(s(), n))

print "R:", lst(R, 10)
print "S:", lst(S, 10)
print sorted(lst(R, 40) + lst(S, 960)) == list(range(1,1001))

# perf test case
# print sum(lst(R, 10000000))
```

{{out}}

```txt
R: [1, 3, 7, 12, 18, 26, 35, 45, 56, 69]
S: [2, 4, 5, 6, 8, 9, 10, 11, 13, 14]
True

```



## Racket

{{trans|Java}}

We store the values of r and s in hash-tables. The first values are added by hand. The procedure extend-r-s! adds more values.


```Racket
#lang racket/base

(define r-cache (make-hash '((1 . 1) (2 . 3) (3 . 7))))
(define s-cache (make-hash '((1 . 2) (2 . 4) (3 . 5) (4 . 6))))

(define (extend-r-s!)
  (define r-count (hash-count r-cache))
  (define s-count (hash-count s-cache))
  (define last-r (ffr r-count))
  (define new-r (+ (ffr r-count) (ffs r-count)))
  (hash-set! r-cache (add1 r-count) new-r)
  (define offset (- s-count last-r))
  (for ([val (in-range (add1 last-r) new-r)])
    (hash-set! s-cache (+ val offset) val)))
```


The functions ffr and ffs simply retrieve the value from the hash table if it exist, or call extend-r-s until they are long enought.


```Racket
(define (ffr n)
  (hash-ref r-cache n (lambda () (extend-r-s!) (ffr n))))

(define (ffs n)
  (hash-ref s-cache n (lambda () (extend-r-s!) (ffs n))))
```


Tests:

```Racket
(displayln (map ffr (list 1 2 3 4 5 6 7 8 9 10)))
(displayln (map ffs (list 1 2 3 4 5 6 7 8 9 10)))

(displayln "Checking for first 1000 integers:")
(displayln (if (equal? (sort (append (for/list ([i (in-range 1 41)])
                                       (ffr i))
                                     (for/list ([i (in-range 1 961)])
                                       (ffs i)))
                             <)
                       (for/list ([i (in-range 1 1001)])
                         i))
               "Test passed"
               "Test failed"))
```


'''Sample Output:'''

```txt
(1 3 7 12 18 26 35 45 56 69)
(2 4 5 6 8 9 10 11 13 14)
Checking for first 1000 integers: Test passed
```



## REXX



### version 1

This REXX example makes use of sparse arrays.

Over a third of the program was for verification of the first thousand numbers in the Hofstadter Figure-Figure sequences.

```rexx
/*REXX program  calculates and verifies  the  Hofstadter Figure─Figure sequences.       */
parse arg x top bot .                            /*obtain optional arguments from the CL*/
if   x=='' |   x==","  then   x=  10             /*Not specified?  Then use the default.*/
if top=='' | top==","  then top=1000             /* "      "         "   "   "      "   */
if bot=='' | bot==","  then bot=  40             /* "      "         "   "   "      "   */
low=1;         if x<0  then low=abs(x)           /*only display a  single   │X│  value? */
r.=0;  r.1=1;  rr.=r.;  rr.1=1;   s.=r.;  s.1=2  /*initialize the  R, RR, and S  arrays.*/
errs=0                                           /*the number of errors found  (so far).*/
             do i=low  to abs(x)                 /*display the 1st  X  values of  R & S.*/
             say right('R('i") =",20) right(FFR(i),7) right('S('i") =",20) right(FFS(i),7)
             end   /*i*/
                                                 /* [↑]  list the 1st X Fig─Fig numbers.*/
if x<1  then exit                                /*if X isn't positive, then we're done.*/
$.=0                                             /*initialize the memoization ($) array.*/
             do m=1  for  bot;  r=FFR(m);  $.r=1 /*calculate the first forty  R  values.*/
             end   /*m*/                         /* [↑]  ($.)  is used for memoization. */
                                                 /* [↓]  check for duplicate #s in R & S*/
             do n=1  for top-bot;     s=FFS(n)   /*calculate the value of  FFS(n).      */
             if $.s  then call ser 'duplicate number in R and S lists:' s;   $.s=1
             end   /*n*/                         /* [↑]  calculate the 1st 960 S values.*/
                                                 /* [↓]  check for missing values in R│S*/
             do v=1  for top;  if \$.v  then  call ser     'missing R │ S:'    v
             end   /*v*/                         /* [↑]  are all 1≤ numbers ≤1k present?*/
say
if errs==0  then say 'verification completed for all numbers from  1 ──►' top "  [inclusive]."
            else say 'verification failed with'      errs      "errors."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
FFR: procedure expose r. rr. s.; parse arg n     /*obtain the number from the arguments.*/
     if r.n\==0  then return r.n                 /*R.n  defined?  Then return the value.*/
     _=FFR(n-1) + FFS(n-1)                       /*calculate the  FFR  and  FFS  values.*/
     r.n=_;       rr._=1;        return _        /*assign the value to R & RR;   return.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
FFS: procedure expose r. s. rr.; parse arg n     /*search for not null  R or S  number. */
     if s.n==0  then do k=1  for n               /* [↓]  1st  IF  is a  SHORT CIRCUIT.  */
                     if s.k\==0  then if r.k\==0  then iterate       /*are both defined?*/
                     call FFR k                  /*define  R.k  via the  FFR  subroutine*/
                     km=k-1;     _=s.km+1        /*calc. the next  S  number,  possibly.*/
                     _=_+rr._;   s.k=_           /*define an element of  the  S  array. */
                     end   /*k*/
     return s.n                                  /*return   S.n   value to the invoker. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ser: errs=errs+1;    say  '***error***'  arg(1);                  return
```

To see the talk section about this REXX program's timings, click here:     [http://rosettacode.org/wiki/Talk:Hofstadter_Figure-Figure_sequences#timings_for_the_REXX_solutions timings for the REXX solutions].


'''output'''   when using the default inputs:

```txt

              R(1) =       1               S(1) =       2
              R(2) =       3               S(2) =       4
              R(3) =       7               S(3) =       5
              R(4) =      12               S(4) =       6
              R(5) =      18               S(5) =       8
              R(6) =      26               S(6) =       9
              R(7) =      35               S(7) =      10
              R(8) =      45               S(8) =      11
              R(9) =      56               S(9) =      13
             R(10) =      69              S(10) =      14

verification completed for all numbers from  1 ──► 1000   [inclusive].

```



### Version 2  from PL/I


```rexx
/* REXX **************************************************************
* 21.11.2012 Walter Pachl transcribed from PL/I
**********************************************************************/
  Call time 'R'
  Say 'Verification that the first 40 FFR numbers and the first'
  Say '960 FFS numbers result in the integers 1 to 1000 only.'
  t.=0
  num.=''
  do i = 1 to 40
    j = ffr(i)
    if t.j then Say 'error, duplicate value at ' || i
    else t.j = 1
    num.i=j
    end
  nn=0
  Say time('E') 'seconds elapsed'
  Do i=1 To 3
    ol=''
    Do j=1 To 15
      nn=nn+1
      ol=ol right(num.nn,3)
      End
    Say ol
    End
  do i = 1 to 960
    j = ffs(i)
    if t.j then
      Say 'error, duplicate value at ' || i
    else t.j = 1
    end
  Do i=1 To 1000
    if t.i=0 Then
      Say i 'was not set'
    End
  If i>1000 Then
    Say 'passed test'
  Say time('E') 'seconds elapsed'
  Exit

 ffr: procedure Expose v.
   Parse Arg n
   v.= 0
   v.1 = 1
   if n = 1 then return 1
   r = 1
   do i = 2 to n
     do j = 2 to 2*n
       if v.j = 0 then leave
       end
     v.j = 1
     s = j
     r = r + s
     if r <= 2*n then v.r = 1
     end
   return r

 ffs: procedure Expose v.
   Parse Arg n
   v.= 0
   v.1 = 1
   if n = 1 then return 2
   r = 1
   do i = 1 to n
     do j = 2 to 2*n
       if v.j = 0 then leave
       end
     v.j = 1
     s = j
     r = r + s
     if r <= 2*n then v.r = 1
     end
   return s
```

{{out}}

```txt
Verification that the first 40 FFR numbers and the first
960 FFS numbers result in the integers 1 to 1000 only.
0.011000 seconds elapsed
   1   3   7  12  18  26  35  45  56  69  83  98 114 131 150
 170 191 213 236 260 285 312 340 369 399 430 462 495 529 565
 602 640 679 719 760 802 845 889 935 982
passed test
Windows (ooRexx)  33.183000 seconds elapsed
Windows (Regina)  22.627000 seconds elapsed
TSO interpreted: 139.699246 seconds elapsed
TSO compiled:      9.749457 seconds elapsed
```



## Ring


```ring

# Project : Hofstadter Figure-Figure sequences

hofr = list(20)
hofr[1] = 1
hofs = []
add(hofs,2)
for n = 1 to 10
      hofr[n+1] = hofr[n] + hofs[n]
      if n = 1
         add(hofs,4)
      else
         for p = hofr[n] + 1 to hofr[n+1] - 1
               if p != hofs[n]
                  add(hofs,p)
               ok
         next
      ok
next
see "First 10 values of R:" + nl
showarray(hofr)
see "First 10 values of S:" + nl
showarray(hofs)

func showarray(vect)
         svect = ""
        for n = 1 to 10
              svect = svect + vect[n] + " "
        next
        svect = left(svect, len(svect) - 1)
        see svect + nl

```

Output:

```txt

First 10 values of R:
1 3 7 12 18 26 35 45 56 69
First 10 values of S:
2 4 5 6 8 9 10 11 13 14

```



## Ruby

{{trans|Tcl}}

```ruby
$r = [nil, 1]
$s = [nil, 2]

def buildSeq(n)
  current = [ $r[-1], $s[-1] ].max
  while $r.length <= n || $s.length <= n
    idx = [ $r.length, $s.length ].min - 1
    current += 1
    if current == $r[idx] + $s[idx]
      $r << current
    else
      $s << current
    end
  end
end

def ffr(n)
  buildSeq(n)
  $r[n]
end

def ffs(n)
  buildSeq(n)
  $s[n]
end

require 'set'
require 'test/unit'

class TestHofstadterFigureFigure < Test::Unit::TestCase
  def test_first_ten_R_values
    r10 = 1.upto(10).map {|n| ffr(n)}
    assert_equal(r10, [1, 3, 7, 12, 18, 26, 35, 45, 56, 69])
  end

  def test_40_R_and_960_S_are_1_to_1000
    rs_values = Set.new
    rs_values.merge( 1.upto(40).collect  {|n| ffr(n)} )
    rs_values.merge( 1.upto(960).collect {|n| ffs(n)} )
    assert_equal(rs_values, Set.new( 1..1000 ))
  end
end
```


outputs

```txt
Loaded suite hofstadter.figurefigure
Started
..
Finished in 0.511000 seconds.

2 tests, 2 assertions, 0 failures, 0 errors, 0 skips
```


### Using cyclic iterators

{{trans|Python}}

```ruby
R = Enumerator.new do |y|
  y << n = 1
  S.each{|s_val| y << n += s_val}
end

S = Enumerator.new do |y|
  y << 2
  y << 4
  u = 5
  R.each do |r_val|
    next if u > r_val
    (u...r_val).each{|r| y << r}
    u = r_val+1
  end
end

p R.take(10)
p S.take(10)
p (R.take(40)+ S.take(960)).sort == (1..1000).to_a

```

{{out}}

```txt

[1, 3, 7, 12, 18, 26, 35, 45, 56, 69]
[2, 4, 5, 6, 8, 9, 10, 11, 13, 14]
true

```



## Scala

{{trans|Go}}

```Scala
object HofstadterFigFigSeq extends App {
  import scala.collection.mutable.ListBuffer

  val r = ListBuffer(0, 1)
  val s = ListBuffer(0, 2)

  def ffr(n: Int): Int = {
    val ffri: Int => Unit = i => {
      val nrk = r.size - 1
      val rNext = r(nrk)+s(nrk)
      r += rNext
      (r(nrk)+2 to rNext-1).foreach{s += _}
      s += rNext+1
    }

    (r.size to n).foreach(ffri(_))
    r(n)
  }

  def ffs(n:Int): Int = {
    while (s.size <= n) ffr(r.size)
    s(n)
  }

  (1 to 10).map(i=>(i,ffr(i))).foreach(t=>println("r("+t._1+"): "+t._2))
  println((1 to 1000).toList.filterNot(((1 to 40).map(ffr(_))++(1 to 960).map(ffs(_))).contains)==List())
}
```

Output:

```txt
r(1): 1
r(2): 3
r(3): 7
r(4): 12
r(5): 18
r(6): 26
r(7): 35
r(8): 45
r(9): 56
r(10): 69
true
```



## Sidef

{{trans|Perl}}

```ruby
var r = [nil, 1]
var s = [nil, 2]

func ffsr(n) {
  while(r.end < n) {
    r << s[r.end]+r[-1]
    s << [(s[-1]+1 .. r[-1]-1)..., r[-1]+1].grep{ s[-1] < _ }...
  }
  return n
}

func ffr(n) { r[ffsr(n)] }
func ffs(n) { s[ffsr(n)] }

printf("  i: R(i) S(i)\n")
printf("
### ========
\n")
{ |i|
    printf("%3d:  %3d  %3d\n", i, ffr(i), ffs(i))
} << 1..10
printf("\nR(40)=%3d S(960)=%3d R(41)=%3d\n", ffr(40), ffs(960), ffr(41))

var seen = Hash()

{|i| seen{ffr(i)} := 0 ++ } << 1..40
{|i| seen{ffs(i)} := 0 ++ } << 1..960

if (seen.count {|k,v| (k.to_i >= 1) && (k.to_i <= 1000) && (v == 1) } == 1000) {
    say "All occured exactly once."
}
else {
    var missed = { !seen.has_key(_) }.grep(1..1000)
    var dupped = seen.grep { |_, v| v > 1 }.keys.sort
    say "These were missed: #{missed}"
    say "These were duplicated: #{dupped}"
}
```

{{out}}

```txt

  i: R(i) S(i)

### ========

  1:    1    2
  2:    3    4
  3:    7    5
  4:   12    6
  5:   18    8
  6:   26    9
  7:   35   10
  8:   45   11
  9:   56   13
 10:   69   14

R(40)=982 S(960)=1000 R(41)=1030
All occured exactly once.

```



## Tcl

{{tcllib|struct::set}}

```tcl
package require Tcl 8.5
package require struct::set

# Core sequence generator engine; stores in $R and $S globals
set R {R:-> 1}
set S {S:-> 2}
proc buildSeq {n} {
    global R S
    set ctr [expr {max([lindex $R end],[lindex $S end])}]
    while {[llength $R] <= $n || [llength $S] <= $n} {
	set idx [expr {min([llength $R],[llength $S]) - 1}]
	if {[incr ctr] == [lindex $R $idx]+[lindex $S $idx]} {
	    lappend R $ctr
	} else {
	    lappend S $ctr
	}
    }
}

# Accessor procedures
proc ffr {n} {
    buildSeq $n
    lindex $::R $n
}
proc ffs {n} {
    buildSeq $n
    lindex $::S $n
}

# Show some things about the sequence
for {set i 1} {$i <= 10} {incr i} {
    puts "R($i) = [ffr $i]"
}
puts "Considering {1..1000} vs {R(i)|i\u2208\[1,40\]}\u222a{S(i)|i\u2208\[1,960\]}"
for {set i 1} {$i <= 1000} {incr i} {lappend numsInSeq $i}
for {set i 1} {$i <= 40} {incr i} {
    lappend numsRS [ffr $i]
}
for {set i 1} {$i <= 960} {incr i} {
    lappend numsRS [ffs $i]
}
puts "set sizes: [struct::set size $numsInSeq] vs [struct::set size $numsRS]"
puts "set equality: [expr {[struct::set equal $numsInSeq $numsRS]?{yes}:{no}}]"
```

Output:

```txt

R(1) = 1
R(2) = 3
R(3) = 7
R(4) = 12
R(5) = 18
R(6) = 26
R(7) = 35
R(8) = 45
R(9) = 56
R(10) = 69
Considering {1..1000} vs {R(i)|i∈[1,40]}∪{S(i)|i∈[1,960]}
set sizes: 1000 vs 1000
set equality: yes

```



## uBasic/4tH

Note that uBasic/4tH has ''no'' dynamic memory facilities and only ''one single array'' of 256 elements. So the only way to cram over a 1000 values there is to use a bitmap. This bitmap consists of an '''R''' range and an '''S''' range. In each range, a bit represents a positional value (bit 0 = "1", bit 1 = "2", etc.). The '''R'''(x) and '''S'''(x) functions simply count the number of bits set they encountered. To determine whether all integers between 1 and 1000 are complementary, both ranges are ''XOR''ed, which would result in a value other than 2<sup>31</sup>-1 if there were any discrepancies present. An extra check determines if there are exactly 40 '''R''' values.
<lang>Proc _SetBitR(1)                       ' Set the first R value
Proc _SetBitS(2)                       ' Set the first S value

Print "Creating bitmap, wait.."        ' Create the bitmap
Proc _MakeBitMap
Print

Print "R(1 .. 10):";                   ' Print first 10 R-values

For x = 1 To 10
  Print " ";FUNC(_Rx(x));
Next

Print : Print "S(1 .. 10):";           ' Print first 10 S-values

For x = 1 To 10
  Print " ";FUNC(_Sx(x));
Next

Print : Print                          ' Terminate and skip line

For x = 0 To (1000/31)                 ' Check the first 1000 values
  Print "Checking ";(x*31)+1;" to ";(x*31)+31;":\t";
  If XOR(@(x), @(x+64)) = 2147483647 Then
     Print "OK"                        ' XOR R() and S() ranges
  Else                                 ' should deliver MAX-N
     Print "Fail!"                     ' or we did have an error
  EndIf
Next

For x = 1 to 40                        ' Prove there are only 40 R(x) values
  If FUNC(_Rx(x)) > 1000 Then Print "R(";x;") value greater than 1000"
Next                                   ' below 1000

If FUNC(_Rx(x)) < 1001 Then Print "R(";x;") value also below 1000"
End


_MakeBitMap                            ' Create the bitmap
  Local (4)

  a@ = 1                               ' Previous R(x) level
  b@ = 1                               ' Previous R(x) value

  Do Until b@ > (1000/31)*32           ' Fill up an entire array element
                                       ' calculate R(x+1) level
    c@ = FUNC(_Rx(a@)) + FUNC(_Sx(a@))
    Proc _SetBitR (c@)                 ' Set R(x+1) in the bitmap

    For d@ = b@ + 1 To c@ - 1          ' Set all intermediate S() values
      Proc _SetBitS (d@)               ' between R(x) and R(x+1)
    Next

    Proc _SetBitS (c@+1)               ' Number after R(x) is always S()
    b@ = c@                            ' R(x+1) now becomes R(x)
    a@ = a@ + 1                        ' Increment level
  Loop                                 ' Now do it again
Return


_Rx Param(1)                           ' Return value R(x)
  Local(2)

  b@ = 0                               ' No value found so far

  For c@ = 1 To (64*31)-1              ' Check the entire bitmap
    If (FUNC(_GetBitR(c@))) Then b@ = b@ + 1
    Until b@ = a@                      ' If a value found, increment counter
  Next                                 ' Until the required level is reached
Return (c@)                            ' Return position in bitmap


_Sx Param(1)                           ' Return value S(x)
  Local(2)

  b@ = 0                               ' No value found so far

  For c@ = 1 To (64*31)-1              ' Check the entire bitmap
    If (FUNC(_GetBitS(c@))) Then b@ = b@ + 1
    Until b@ = a@                      ' If a value found, increment counter
  Next                                 ' Until the required level is reached
Return (c@)                            ' Return position in bitmap


_SetBitR Param(1)                      ' Set bit n-1 in R-bitmap
  a@ = a@ - 1
  @(a@/31) = OR(@(a@/31), SHL(1,a@%31))
Return

_GetBitR Param(1)                      ' Return bit n-1 in R-bitmap
  a@ = a@ - 1
Return (AND(@(a@/31), SHL(1,a@%31))#0)

_SetBitS Param(1)                      ' Set bit n-1 in S-bitmap
  a@ = a@ - 1
  @(64+a@/31) = OR(@(64+a@/31), SHL(1,a@%31))
Return

_GetBitS Param(1)                      ' Return bit n-1 in S-bitmap
  a@ = a@ - 1
Return (AND(@(64+a@/31), SHL(1,a@%31))#0)
```

{{out}}

```txt
Creating bitmap, wait..

R(1 .. 10): 1 3 7 12 18 26 35 45 56 69
S(1 .. 10): 2 4 5 6 8 9 10 11 13 14

Checking 1 to 31:       OK
Checking 32 to 62:      OK
Checking 63 to 93:      OK
Checking 94 to 124:     OK
Checking 125 to 155:    OK
Checking 156 to 186:    OK
Checking 187 to 217:    OK
Checking 218 to 248:    OK
Checking 249 to 279:    OK
Checking 280 to 310:    OK
Checking 311 to 341:    OK
Checking 342 to 372:    OK
Checking 373 to 403:    OK
Checking 404 to 434:    OK
Checking 435 to 465:    OK
Checking 466 to 496:    OK
Checking 497 to 527:    OK
Checking 528 to 558:    OK
Checking 559 to 589:    OK
Checking 590 to 620:    OK
Checking 621 to 651:    OK
Checking 652 to 682:    OK
Checking 683 to 713:    OK
Checking 714 to 744:    OK
Checking 745 to 775:    OK
Checking 776 to 806:    OK
Checking 807 to 837:    OK
Checking 838 to 868:    OK
Checking 869 to 899:    OK
Checking 900 to 930:    OK
Checking 931 to 961:    OK
Checking 962 to 992:    OK
Checking 993 to 1023:   OK

0 OK, 0:875
```



## VBA


```vb
Private Function ffr(n As Long) As Long
    Dim R As New Collection
    Dim S As New Collection
    R.Add 1
    S.Add 2
    'return R(n)
    For i = 2 To n
        R.Add R(i - 1) + S(i - 1)
        For j = S(S.Count) + 1 To R(i) - 1
            S.Add j
        Next j
        For j = R(i) + 1 To R(i) + S(i - 1)
            S.Add j
        Next j
    Next i
    ffr = R(n)
    Set R = Nothing
    Set S = Nothing
End Function
Private Function ffs(n As Long) As Long
    Dim R As New Collection
    Dim S As New Collection
    R.Add 1
    S.Add 2
    'return S(n)
    For i = 2 To n
        R.Add R(i - 1) + S(i - 1)
        For j = S(S.Count) + 1 To R(i) - 1
            S.Add j
        Next j
        For j = R(i) + 1 To R(i) + S(i - 1)
            S.Add j
        Next j
        If S.Count >= n Then Exit For
    Next i
    ffs = S(n)
    Set R = Nothing
    Set S = Nothing
End Function
Public Sub main()
    Dim i As Long
    Debug.Print "The first ten values of R are:"
    For i = 1 To 10
        Debug.Print ffr(i);
    Next i
    Debug.Print
    Dim x As New Collection
    For i = 1 To 1000
        x.Add i, CStr(i)
    Next i
    For i = 1 To 40
        x.Remove CStr(ffr(i))
    Next i
    For i = 1 To 960
        x.Remove CStr(ffs(i))
    Next i
    Debug.Print "The first 40 values of ffr plus the first 960 values of ffs "
    Debug.Print "include all the integers from 1 to 1000 exactly once is "; Format(x.Count = 0)
End Sub
```
{{out}}

```txt
The first ten values of R are:
 1  3  7  12  18  26  35  45  56  69
The first 40 values of ffr plus the first 960 values of ffs
include all the integers from 1 to 1000 exactly once is True
```


## VBScript


```vb

'Initialize the r and the s arrays.
Set r = CreateObject("System.Collections.ArrayList")
Set s = CreateObject("System.Collections.ArrayList")

'Set initial values of r.
r.Add ""  : r.Add 1

'Set initial values of s.
s.Add "" : s.Add 2

'Populate the r and the s arrays.
For i = 2 To 1000
	ffr(i)
	ffs(i)
Next

'r function
Function ffr(n)
	r.Add r(n-1)+s(n-1)
End Function

's function
Function ffs(n)
	'index is the value of the last element of the s array.
	index = s(n-1)+1
	Do
                'Add to s if the current index is not in the r array.
		If r.IndexOf(index,0) = -1 Then
			s.Add index
			Exit Do
		Else
			index = index + 1
		End If
	Loop
End Function

'Display the first 10 values of r.
WScript.StdOut.Write "First 10 Values of R:"
WScript.StdOut.WriteLine
For j = 1 To 10
	If j = 10 Then
		WScript.StdOut.Write "and " & r(j)
	Else
		WScript.StdOut.Write r(j) & ", "
	End If
Next
WScript.StdOut.WriteBlankLines(2)

'Show that the first 40 values of r plus the first 960 values of s include all the integers from 1 to 1000 exactly once.
'The idea here is to create another array(integer) with 1000 elements valuing from 1 to 1000. Go through the first 40 values
'of the r array and remove the corresponding element in the integer array.  Do the same thing with the first 960 values of
'the s array.  If the resultant count of the integer array is 0 then it is a pass.
Set integers = CreateObject("System.Collections.ArrayList")
For k = 1 To 1000
	integers.Add k
Next
For l = 1 To 960
	If l <= 40 Then
		integers.Remove(r(l))
	End If
	integers.Remove(s(l))
Next
WScript.StdOut.Write "Test for the first 1000 integers: "
If integers.Count = 0 Then
	WScript.StdOut.Write "Passed!!!"
	WScript.StdOut.WriteLine
Else
	WScript.StdOut.Write "Miserably Failed!!!"
	WScript.StdOut.WriteLine
End If

```


{{Out}}

```txt

First 10 Values of R:
1, 3, 7, 12, 18, 26, 35, 45, 56, and 69

Test for the first 1000 integers: Passed!!!

```



## zkl


```zkl
fcn genRS(reset=False){ //-->(n,R,S)
  var n=0, Rs=L(0,1), S=2;
  if(True==reset){ n=0; Rs=L(0,1); S=2; return(); }

  if (n==0) return(n=1,1,2);
  R:=Rs[-1] + S; Rs.append(R);
  foreach s in ([S+1..]){
     if(not Rs.holds(s)) { S=s; break; } // trimming Rs doesn't save space
  }
  return(n+=1,R,S);
}
fcn ffrs(n) { genRS(True); do(n){ n=genRS() } n[1,2] }  //-->( R(n),S(n) )
```

{{out}}

```txt

(0).pump(10,List,genRS).apply("get",1).println();
L(1,3,7,12,18,26,35,45,56,69)

```


```zkl
genRS(True);  // reset
sink:=(0).pump(40,List,    'wrap(ns){ T(Void.Write,Void.Write,genRS()[1,*]) });
sink= (0).pump(960-40,sink,'wrap(ns){ T(Void.Write,genRS()[2]) });
(sink.sort()==[1..1000].pump(List))  // [1..n].pump(List)-->(1,2,3...)
   .println("<-- should be True");
```

{{out}}

```txt

True<-- should be True

```

