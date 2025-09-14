+++
title = "Levenshtein distance"
description = ""
date = 2019-09-26T23:08:51Z
aliases = []
[extra]
id = 9131
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "aime",
  "apple_script",
  "arc",
  "auto_hotkey",
  "awk",
  "bbc_basic",
  "bracmat",
  "c",
  "c_plus_plus",
  "c_sharp",
  "coffee_script",
  "common_lisp",
  "clojure",
  "crystal",
  "d",
  "dw_script",
  "dyalect",
  "echo_lisp",
  "ela",
  "elixir",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "forth",
  "free_basic",
  "frink",
  "future_basic",
  "go",
  "groovy",
  "haskell",
  "io",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "lfe",
  "liberty_basic",
  "limbo",
  "live_code",
  "lua",
  "m2000_interpreter",
  "maple",
  "mathematica",
  "matlab",
  "net_rexx",
  "nim",
  "objeck",
  "ocaml",
  "oo_rexx",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "power_shell",
  "processing",
  "prolog",
  "pure_basic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sequence_l",
  "sidef",
  "simula",
  "smalltalk",
  "swift",
  "tcl",
  "tse_sal",
  "tuscript",
  "vala",
  "vba",
  "visual_basic",
  "visual_basic_dot_net",
  "zkl"
]
+++

## Task



In information theory and computer science, the '''Levenshtein distance''' is a [[wp:string metric|metric]] for measuring the amount of difference between two sequences (i.e. an [[wp:edit distance|edit distance]]). The Levenshtein distance between two strings is defined as the minimum number of edits needed to transform one string into the other, with the allowable edit operations being insertion, deletion, or substitution of a single character.


## Example
The Levenshtein distance between "'''kitten'''" and "'''sitting'''" is 3, since the following three edits change one into the other, and there isn't a way to do it with fewer than three edits:
1.   k itten    s itten       (substitution of 'k' with 's')
2.   sitt e n    sitt i n       (substitution of 'e' with 'i')
3.   sittin          sittin g       (insert 'g' at the end).



''The Levenshtein distance between   "'''rosettacode'''",   "'''raisethysword'''"   is   '''8'''.

''The distance between two strings is same as that when both strings are reversed.''


## Task
Implements a Levenshtein distance function, or uses a library function, to show the Levenshtein distance between   "kitten"   and   "sitting".


## Related task
*   [[Longest common subsequence]]





## Ada


```Ada
with Ada.Text_IO;

procedure Main is
   function Levenshtein_Distance (S, T : String) return Natural is
      D : array (0 .. S'Length, 0 .. T'Length) of Natural;
   begin
      for I in D'Range (1) loop
         D (I, 0) := I;
      end loop;
      for I in D'Range (2) loop
         D (0, I) := I;
      end loop;
      for J in T'Range loop
         for I in S'Range loop
            if S (I) = T (J) then
               D (I, J) := D (I - 1, J - 1);
            else
               D (I, J) :=
                  Natural'Min
                    (Natural'Min (D (I - 1, J) + 1, D (I, J - 1) + 1),
                     D (I - 1, J - 1) + 1);
            end if;
         end loop;
      end loop;
      return D (S'Length, T'Length);
   end Levenshtein_Distance;
begin
   Ada.Text_IO.Put_Line
     ("kitten -> sitting:" &
      Integer'Image (Levenshtein_Distance ("kitten", "sitting")));
   Ada.Text_IO.Put_Line
     ("rosettacode -> raisethysword:" &
      Integer'Image (Levenshtein_Distance ("rosettacode", "raisethysword")));
end Main;
```

{{out}}

```txt
kitten -> sitting: 3
rosettacode -> raisethysword: 8
```



## Aime

{{trans|C}}

```aime
integer
dist(data s, t, integer i, j, list d)
{
    integer x;

    x = d[i * (~t + 1) + j];
    if (x == -1) {
        if (i == ~s) {
            x = ~t - j;
        } elif (j == ~t) {
            x = ~s - i;
        } elif (s[i] == t[j]) {
            x = dist(s, t, i + 1, j + 1, d);
        } else {
            x = dist(s, t, i + 1, j + 1, d)
                .min(dist(s, t, i, j + 1, d))
                .min(dist(s, t, i + 1, j, d));
            x += 1;
        }

        d[i * (~t + 1) + j] = x;
    }

    x;
}

levenshtein(data s, t)
{
    list d;

    d.pn_integer(0, (~s + 1) * (~t + 1), -1);
    dist(s, t, 0, 0, d);
}

main(void)
{
    text s1, s2;

    o_form("`~' to `~' is ~\n", s1 = "rosettacode", s2 = "raisethysword",
           levenshtein(s1, s2));
    o_form("`~' to `~' is ~\n", s1 = "kitten", s2 = "sitting",
           levenshtein(s1, s2));

    0;
}
```

{{Out}}

```txt
`rosettacode' to `raisethysword' is 8
`kitten' to `sitting' is 3
```



## AppleScript


### Iteration

Translation of the "fast" C-version

```AppleScript
set dist to findLevenshteinDistance for "sunday" against "saturday"
to findLevenshteinDistance for s1 against s2
    script o
        property l : s1
        property m : s2
    end script
    if s1 = s2 then return 0
    set ll to length of s1
    set lm to length of s2
    if ll = 0 then return lm
    if lm = 0 then return ll

    set v0 to {}

    repeat with i from 1 to (lm + 1)
        set end of v0 to (i - 1)
    end repeat
    set item -1 of v0 to 0
    copy v0 to v1

    repeat with i from 1 to ll
        -- calculate v1 (current row distances) from the previous row v0

        -- first element of v1 is A[i+1][0]
        --   edit distance is delete (i+1) chars from s to match empty t
        set item 1 of v1 to i
        --  use formula to fill in the rest of the row
        repeat with j from 1 to lm
            if item i of o's l = item j of o's m then
                set cost to 0
            else
                set cost to 1
            end if
            set item (j + 1) of v1 to min3 for ((item j of v1) + 1) against ((item (j + 1) of v0) + 1) by ((item j of v0) + cost)
        end repeat
        copy v1 to v0
    end repeat
    return item (lm + 1) of v1
end findLevenshteinDistance

to min3 for anInt against anOther by theThird
    if anInt < anOther then
        if theThird < anInt then
            return theThird
        else
            return anInt
        end if
    else
        if theThird < anOther then
            return theThird
        else
            return anOther
        end if
    end if
end min3
```



### Composition of generic functions

{{Trans|JavaScript}}
(ES6 version)

```AppleScript
-- levenshtein :: String -> String -> Int
on levenshtein(sa, sb)
    set {s1, s2} to {characters of sa, characters of sb}

    script
        on |λ|(ns, c)
            script minPath
                on |λ|(z, c1xy)
                    set {c1, x, y} to c1xy
                    minimum({y + 1, z + 1, x + fromEnum(c1 is not c)})
                end |λ|
            end script

            set {n, ns1} to uncons(ns)
            scanl(minPath, n + 1, zip3(s1, ns, ns1))
        end |λ|
    end script

    |last|(foldl(result, enumFromTo(0, length of s1), s2))
end levenshtein

-- TEST -----------------------------------------------------------------------
on run
    script test
        on |λ|(xs)
            levenshtein(item 1 of xs, item 2 of xs)
        end |λ|
    end script

    map(test, [["kitten", "sitting"], ["sitting", "kitten"], ¬
        ["rosettacode", "raisethysword"], ["raisethysword", "rosettacode"]])

    --> {3, 3, 8, 8}
end run


-- GENERIC FUNCTIONS -----------------------------------------------------------

-- enumFromTo :: Enum a => a -> a -> [a]
on enumFromTo(m, n)
    set {intM, intN} to {fromEnum(m), fromEnum(n)}

    if intM > intN then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    if class of m is text then
        repeat with i from intM to intN by d
            set end of lst to chr(i)
        end repeat
    else
        repeat with i from intM to intN by d
            set end of lst to i
        end repeat
    end if
    return lst
end enumFromTo

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

-- fromEnum :: Enum a => a -> Int
on fromEnum(x)
    set c to class of x
    if c is boolean then
        if x then
            1
        else
            0
        end if
    else if c is text then
        if x ≠ "" then
            id of x
        else
            missing value
        end if
    else
        x as integer
    end if
end fromEnum

-- last :: [a] -> a
on |last|(xs)
    if length of xs > 0 then
        item -1 of xs
    else
        missing value
    end if
end |last|

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

-- minimum :: [a] -> a
on minimum(xs)
    script min
        on |λ|(a, x)
            if x < a or a is missing value then
                x
            else
                a
            end if
        end |λ|
    end script

    foldl(min, missing value, xs)
end minimum

-- scanl :: (b -> a -> b) -> b -> [a] -> [b]
on scanl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        set lst to {startValue}
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
            set end of lst to v
        end repeat
        return lst
    end tell
end scanl

-- uncons :: [a] -> Maybe (a, [a])
on uncons(xs)
    if length of xs > 0 then
        {item 1 of xs, rest of xs}
    else
        missing value
    end if
end uncons

-- zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
on zip3(xs, ys, zs)
    script
        on |λ|(x, i)
            [x, item i of ys, item i of zs]
        end |λ|
    end script

    map(result, items 1 thru ¬
        minimum({length of xs, length of ys, length of zs}) of xs)
end zip3
```

{{Out}}

```AppleScript
{3, 3, 8, 8}
```



## Arc


### Waterhouse Arc

O(n * m) time, linear space, using lists instead of vectors


```lisp
(def levenshtein (str1 str2)
  (withs l1  len.str1
         l2  len.str2
         row range0:inc.l1

    (times j l2
      (let next list.j
        (times i l1
          (push
            (inc:min
              car.next
              ((if (is str1.i str2.j) dec id) car.row)
              (car:zap cdr row))
            next))
        (= row nrev.next)))
    row.l1))
```



## AutoHotkey

{{trans|Go}}

```AutoHotkey
levenshtein(s, t){
	If s =
		return StrLen(t)
	If t =
		return strLen(s)
	If SubStr(s, 1, 1) = SubStr(t, 1, 1)
		return levenshtein(SubStr(s, 2), SubStr(t, 2))
	a := Levenshtein(SubStr(s, 2), SubStr(t, 2))
	b := Levenshtein(s,            SubStr(t, 2))
	c := Levenshtein(SubStr(s, 2), t           )
	If (a > b)
		a := b
	if (a > c)
		a := c
	return a + 1
}
s1 := "kitten"
s2 := "sitting"
MsgBox % "distance between " s1 " and " s2 ": " levenshtein(s1, s2)
```
It correctly outputs '3'


## AWK


Slavishly copied from the very clear AutoHotKey example.


```awk
#!/usr/bin/awk -f

BEGIN {
    a = "kitten";
    b = "sitting";
    d = levenshteinDistance(a, b);
    p = d == 1 ? "" : "s";
    printf("%s -> %s after %d edit%s\n", a, b, d, p);
    exit;
}

function levenshteinDistance(s1, s2,
    s1First, s2First, s1Rest, s2Rest,
    distA, distB, distC, minDist) {

    # If either string is empty,
    # then distance is insertion of the other's characters.
    if (length(s1) == 0) return length(s2);
    if (length(s2) == 0) return length(s1);

    # Rest of process uses first characters
    # and remainder of each string.
    s1First = substr(s1, 1, 1);
    s2First = substr(s2, 1, 1);
    s1Rest = substr(s1, 2, length(s1));
    s2Rest = substr(s2, 2, length(s2));

    # If leading characters are the same,
    # then distance is that between the rest of the strings.
    if (s1First == s2First) {
        return levenshteinDistance(s1Rest, s2Rest);
    }

    # Find the distances between sub strings.
    distA = levenshteinDistance(s1Rest, s2);
    distB = levenshteinDistance(s1, s2Rest);
    distC = levenshteinDistance(s1Rest, s2Rest);

    # Return the minimum distance between substrings.
    minDist = distA;
    if (distB < minDist) minDist = distB;
    if (distC < minDist) minDist = distC;
    return minDist + 1; # Include change for the first character.
}


```


Example output:

```txt

kitten -> sitting after 3 edits

```



Alternative, much faster but also less readable lazy-evaluation version from http://awk.freeshell.org/LevenshteinEditDistance
(where the above takes e.g. 0m44.904s in gawk 4.1.3 for 5 edits (length 10 and 14 strings), this takes user    0m0.004s):

```awk
#!/usr/bin/awk -f

function levdist(str1, str2,	l1, l2, tog, arr, i, j, a, b, c) {
	if (str1 == str2) {
		return 0
	} else if (str1 == "" || str2 == "") {
		return length(str1 str2)
	} else if (substr(str1, 1, 1) == substr(str2, 1, 1)) {
		a = 2
		while (substr(str1, a, 1) == substr(str2, a, 1)) a++
		return levdist(substr(str1, a), substr(str2, a))
	} else if (substr(str1, l1=length(str1), 1) == substr(str2, l2=length(str2), 1)) {
		b = 1
		while (substr(str1, l1-b, 1) == substr(str2, l2-b, 1)) b++
		return levdist(substr(str1, 1, l1-b), substr(str2, 1, l2-b))
	}
	for (i = 0; i <= l2; i++) arr[0, i] = i
	for (i = 1; i <= l1; i++) {
		arr[tog = ! tog, 0] = i
		for (j = 1; j <= l2; j++) {
			a = arr[! tog, j  ] + 1
			b = arr[  tog, j-1] + 1
			c = arr[! tog, j-1] + (substr(str1, i, 1) != substr(str2, j, 1))
			arr[tog, j] = (((a<=b)&&(a<=c)) ? a : ((b<=a)&&(b<=c)) ? b : c)
		}
	}
	return arr[tog, j-1]
}

```



## BBC BASIC


```bbcbasic
      PRINT "'kitten' -> 'sitting' has distance " ;
      PRINT ; FNlevenshtein("kitten", "sitting")
      PRINT "'rosettacode' -> 'raisethysword' has distance " ;
      PRINT ; FNlevenshtein("rosettacode", "raisethysword")
      END

      DEF FNlevenshtein(s$, t$)
      LOCAL i%, j%, m%, d%()
      DIM d%(LENs$, LENt$)
      FOR i% = 0 TO DIM(d%(),1)
        d%(i%,0) = i%
      NEXT
      FOR j% = 0 TO DIM(d%(),2)
        d%(0,j%) = j%
      NEXT
      FOR j% = 1 TO DIM(d%(),2)
        FOR i% = 1 TO DIM(d%(),1)
          IF MID$(s$,i%,1) = MID$(t$,j%,1) THEN
            d%(i%,j%) = d%(i%-1,j%-1)
          ELSE
            m% = d%(i%-1,j%-1)
            IF d%(i%,j%-1) < m% m% = d%(i%,j%-1)
            IF d%(i%-1,j%) < m% m% = d%(i%-1,j%)
            d%(i%,j%) = m% + 1
          ENDIF
        NEXT
      NEXT j%
      = d%(i%-1,j%-1)
```

'''Output:'''

```txt

'kitten' -> 'sitting' has distance 3
'rosettacode' -> 'raisethysword' has distance 8

```



## Bracmat

{{trans|C}}
Recursive method, but with memoization.

```bracmat
(levenshtein=
  lev cache
.   ( lev
    =   s s0 s1 t t0 t1 L a b c val key
      .     (cache..find)$(str$!arg:?key):(?.?val)
          & !val
        |   !arg:(?s,?t)
          & ( !s:&@(!t:? [?L)
            | !t:&@(!s:? [?L)
            )
          & (cache..insert)$(!key.!L)
          & !L
        |   !arg:(@(?:%?s0 ?s1),@(?:%?t0 ?t1))
          & !s0:!t0
          & lev$(!s1,!t1)
        |   lev$(!s1,!t1):?a
          & lev$(!s,!t1):?b
          & lev$(!s1,!t):?c
          & (!b:<!a:?a|)
          & (!c:<!a:?a|)
          & (cache..insert)$(!key.1+!a)
          & 1+!a
    )
  & new$hash:?cache
  & lev$!arg);
```

{{out|Demonstrating}}

```txt
 levenshtein$(kitten,sitting)
 3
 levenshtein$(rosettacode,raisethysword)
 8
```



## C

Recursive method. Deliberately left in an inefficient state to show the recursive nature of the algorithm; notice how it would have become the Wikipedia algorithm if we memoized the function against parameters <code>ls</code> and <code>lt</code>.

```c
#include <stdio.h>
#include <string.h>

/* s, t: two strings; ls, lt: their respective length */
int levenshtein(const char *s, int ls, const char *t, int lt)
{
        int a, b, c;

        /* if either string is empty, difference is inserting all chars
         * from the other
         */
        if (!ls) return lt;
        if (!lt) return ls;

        /* if last letters are the same, the difference is whatever is
         * required to edit the rest of the strings
         */
        if (s[ls - 1] == t[lt - 1])
                return levenshtein(s, ls - 1, t, lt - 1);

        /* else try:
         *      changing last letter of s to that of t; or
         *      remove last letter of s; or
         *      remove last letter of t,
         * any of which is 1 edit plus editing the rest of the strings
         */
        a = levenshtein(s, ls - 1, t, lt - 1);
        b = levenshtein(s, ls,     t, lt - 1);
        c = levenshtein(s, ls - 1, t, lt    );

        if (a > b) a = b;
        if (a > c) a = c;

        return a + 1;
}

int main()
{
        const char *s1 = "rosettacode";
        const char *s2 = "raisethysword";
        printf("distance between `%s' and `%s': %d\n", s1, s2,
                levenshtein(s1, strlen(s1), s2, strlen(s2)));

        return 0;
}
```

Take the above and add caching, we get (in [[C99]]):

```c
#include <stdio.h>
#include <string.h>

int levenshtein(const char *s, const char *t)
{
	int ls = strlen(s), lt = strlen(t);
	int d[ls + 1][lt + 1];

	for (int i = 0; i <= ls; i++)
		for (int j = 0; j <= lt; j++)
			d[i][j] = -1;

	int dist(int i, int j) {
		if (d[i][j] >= 0) return d[i][j];

		int x;
		if (i == ls)
			x = lt - j;
		else if (j == lt)
			x = ls - i;
		else if (s[i] == t[j])
			x = dist(i + 1, j + 1);
		else {
			x = dist(i + 1, j + 1);

			int y;
			if ((y = dist(i, j + 1)) < x) x = y;
			if ((y = dist(i + 1, j)) < x) x = y;
			x++;
		}
		return d[i][j] = x;
	}
	return dist(0, 0);
}

int main(void)
{
	const char *s1 = "rosettacode";
	const char *s2 = "raisethysword";
	printf("distance between `%s' and `%s': %d\n", s1, s2,
		levenshtein(s1, s2));

        return 0;
}
```



## C++


```c
#include <string>
#include <iostream>
using namespace std;

// Compute Levenshtein Distance
// Martin Ettl, 2012-10-05

size_t uiLevenshteinDistance(const std::string &s1, const std::string &s2)
{
  const size_t m(s1.size());
  const size_t n(s2.size());

  if( m==0 ) return n;
  if( n==0 ) return m;

  size_t *costs = new size_t[n + 1];

  for( size_t k=0; k<=n; k++ ) costs[k] = k;

  size_t i = 0;
  for ( std::string::const_iterator it1 = s1.begin(); it1 != s1.end(); ++it1, ++i )
  {
    costs[0] = i+1;
    size_t corner = i;

    size_t j = 0;
    for ( std::string::const_iterator it2 = s2.begin(); it2 != s2.end(); ++it2, ++j )
    {
      size_t upper = costs[j+1];
      if( *it1 == *it2 )
      {
		  costs[j+1] = corner;
	  }
      else
	  {
		size_t t(upper<corner?upper:corner);
        costs[j+1] = (costs[j]<t?costs[j]:t)+1;
	  }

      corner = upper;
    }
  }

  size_t result = costs[n];
  delete [] costs;

  return result;
}

int main()
{
	string s0 = "rosettacode";
        string s1 = "raisethysword";
	cout << "distance between " << s0 << " and " << s1 << " : "
	     << uiLevenshteinDistance(s0,s1) << std::endl;

        return 0;
}

```

{{out|Example output}}

```txt

$ ./a.out
distance between rosettacode and raisethysword : 8

```


## C#
This is a straightforward translation of the Wikipedia pseudocode.

```c#
using System;

namespace LevenshteinDistance
{
    class Program
    {
        static int LevenshteinDistance(string s, string t)
        {
            int n = s.Length;
            int m = t.Length;
            int[,] d = new int[n + 1, m + 1];

	    if (n == 0)
	    {
		return m;
	    }

	    if (m == 0)
	    {
		return n;
	    }

            for (int i = 0; i <= n; i++)
                d[i, 0] = i;
            for (int j = 0; j <= m; j++)
                d[0, j] = j;

            for (int j = 1; j <= m; j++)
                for (int i = 1; i <= n; i++)
                    if (s[i - 1] == t[j - 1])
                        d[i, j] = d[i - 1, j - 1];  //no operation
                    else
                        d[i, j] = Math.Min(Math.Min(
                            d[i - 1, j] + 1,    //a deletion
                            d[i, j - 1] + 1),   //an insertion
                            d[i - 1, j - 1] + 1 //a substitution
                            );
            return d[n, m];
        }

        static void Main(string[] args)
        {
            if (args.Length == 2)
                Console.WriteLine("{0} -> {1} = {2}",
                    args[0], args[1], LevenshteinDistance(args[0], args[1]));
            else
                Console.WriteLine("Usage:-\n\nLevenshteinDistance <string1> <string2>");
        }
    }
}
```

{{out|Example output}}

```txt

> LevenshteinDistance kitten sitting
kitten -> sitting = 3

> LevenshteinDistance rosettacode raisethysword
rosettacode -> raisethysword = 8

```


=={{header|COBOL|COBOL}}==
GnuCobol 2.2

```cobol

       identification division.
       program-id. Levenshtein.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       data division.
       working-storage section.
       77  string-a               pic x(255).
       77  string-b               pic x(255).
       77  length-a               pic 9(3).
       77  length-b               pic 9(3).
       77  distance               pic z(3).
       77  i                      pic 9(3).
       77  j                      pic 9(3).
       01  tab.
           05 filler              occurs 256.
              10 filler           occurs 256.
                 15 costs         pic 9(3).

       procedure division.
       main.
           move "kitten" to string-a
           move "sitting" to string-b
           perform levenshtein-dist

           move "rosettacode" to string-a
           move "raisethysword" to string-b
           perform levenshtein-dist
           stop run
           .
       levenshtein-dist.
           move length(trim(string-a)) to length-a
           move length(trim(string-b)) to length-b

           initialize tab

           perform varying i from 0 by 1 until i > length-a
              move i to costs(i + 1, 1)
           end-perform

           perform varying j from 0 by 1 until j > length-b
              move j to costs(1, j + 1)
           end-perform

           perform with test after varying i from 2 by 1 until i > length-a
              perform with test after varying j from 2 by 1 until j > length-b
                 if string-a(i - 1:1) = string-b(j - 1:1)
                    move costs(i - 1, j - 1) to costs(i, j)
                 else
                    move min(min(costs(i - 1, j) + 1,     *> a deletion
                                 costs(i, j - 1) + 1),    *> an insertion
                             costs(i - 1, j - 1) + 1)     *> a substitution
                       to costs(i, j)
                 end-if
              end-perform
           end-perform
           move costs(length-a + 1, length-b + 1) to distance
           display trim(string-a) " -> " trim(string-b) " = " trim(distance)
           .

```

{{out|Output}}

```txt

> ./Levenshtein
kitten -> sitting = 3
rosettacode -> raisethysword = 8

```



## CoffeeScript


```coffeescript
levenshtein = (str1, str2) ->
  # more of less ported simple algorithm from JS
  m = str1.length
  n = str2.length
  d = []

  return n  unless m
  return m  unless n

  d[i] = [i] for i in [0..m]
  d[0][j] = j for j in [1..n]

  for i in [1..m]
    for j in [1..n]
      if str1[i-1] is str2[j-1]
        d[i][j] = d[i-1][j-1]
      else
        d[i][j] = Math.min(
          d[i-1][j]
          d[i][j-1]
          d[i-1][j-1]
        ) + 1

  d[m][n]

console.log levenshtein("kitten", "sitting")
console.log levenshtein("rosettacode", "raisethysword")
console.log levenshtein("stop", "tops")
console.log levenshtein("yo", "")
console.log levenshtein("", "yo")
```



## Common Lisp


```lisp
(defun levenshtein (a b)
  (let* ((la  (length a))
	 (lb  (length b))
	 (rec (make-array (list (1+ la) (1+ lb)) :initial-element nil)))

    (defun leven (x y)
      (cond
	((zerop x) y)
	((zerop y) x)
	((aref rec x y) (aref rec x y))
	(t (setf (aref rec x y)
		 (+ (if (char= (char a (- la x)) (char b (- lb y))) 0 1)
		    (min (leven (1- x) y)
			 (leven x (1- y))
			 (leven (1- x) (1- y))))))))
    (leven la lb)))

(print (levenshtein "rosettacode" "raisethysword"))
```

{{out}}

```txt
8
```


## Clojure



### Recursive Version


```lisp
(defn levenshtein [str1 str2]
  (let [len1 (count str1)
        len2 (count str2)]
    (cond (zero? len1) len2
          (zero? len2) len1
          :else
          (let [cost (if (= (first str1) (first str2)) 0 1)]
            (min (inc (levenshtein (rest str1) str2))
                 (inc (levenshtein str1 (rest str2)))
                 (+ cost
                    (levenshtein (rest str1) (rest str2))))))))

(println (levenshtein "rosettacode" "raisethysword"))
```

{{out}}

```txt
8
```



### Iterative version


```lisp
(defn levenshtein [w1 w2]
  (letfn [(cell-value [same-char? prev-row cur-row col-idx]
            (min (inc (nth prev-row col-idx))
                 (inc (last cur-row))
                 (+ (nth prev-row (dec col-idx)) (if same-char?
                                                   0
                                                   1))))]
    (loop [row-idx  1
           max-rows (inc (count w2))
           prev-row (range (inc (count w1)))]
      (if (= row-idx max-rows)
        (last prev-row)
        (let [ch2           (nth w2 (dec row-idx))
              next-prev-row (reduce (fn [cur-row i]
                                      (let [same-char? (= (nth w1 (dec i)) ch2)]
                                        (conj cur-row (cell-value same-char?
                                                                  prev-row
                                                                  cur-row
                                                                  i))))
                                    [row-idx] (range 1 (count prev-row)))]
          (recur (inc row-idx) max-rows next-prev-row))))))
```



## Crystal

The standard library includes [https://crystal-lang.org/api/0.19.2/Levenshtein.html levenshtein] module

```ruby
require "levenshtein"
puts Levenshtein.distance("kitten", "sitting")
puts Levenshtein.distance("rosettacode", "raisethysword")

```

{{out}}

```txt
3
8
```


{{trans|Ruby 1st version}}

```ruby
module Levenshtein

  def self.distance(a, b)
    a, b = a.downcase, b.downcase
    costs = (0..b.size).to_a
    (1..a.size).each do |i|
      costs[0], nw = i, i - 1  # j == 0; nw is lev(i-1, j)
      (1..b.size).each do |j|
        costs[j], nw = [costs[j] + 1, costs[j-1] + 1, a[i-1] == b[j-1] ? nw : nw + 1].min, costs[j]
      end
    end
    costs[b.size]
  end

  def self.test
    %w{kitten sitting saturday sunday rosettacode raisethysword}.each_slice(2) do |(a, b)| #or do |pair| a, b = pair
      puts "distance(#{a}, #{b}) = #{distance(a, b)}"
    end
  end

end

Levenshtein.test

```

{{out}}

```txt

distance(kitten, sitting) = 3
distance(saturday, sunday) = 3
distance(rosettacode, raisethysword) = 8

```


{{trans|Ruby 2nd version}}

```ruby
def levenshtein_distance(str1, str2)
  n, m = str1.size, str2.size
  max = n / 2

  return 0 if n == 0 || m == 0
  return n if (n - m).abs > max

  d = (0..m).to_a
  x = 0

  str1.each_char_with_index do |char1, i|
    e = i + 1

    str2.each_char_with_index do |char2, j|
      cost = (char1 == char2) ? 0 : 1
      x = [ d[j+1] + 1, # insertion
            e + 1,      # deletion
            d[j] + cost # substitution
          ].min
      d[j] = e
      e = x
    end

    d[m] = x
  end
  x
end

%w{kitten sitting saturday sunday rosettacode raisethysword}.each_slice(2) do |(a, b)| #or do |pair| a, b = pair
  puts "distance(#{a}, #{b}) = #{levenshtein_distance(a, b)}"
end

```

{{out}}

```txt

distance(kitten, sitting) = 3
distance(saturday, sunday) = 3
distance(rosettacode, raisethysword) = 8

```



## D


### Standard Version

The standard library [http://www.digitalmars.com/d/2.0/phobos/std_algorithm.html#levenshteinDistance std.algorithm] module includes a Levenshtein distance function:

```d
void main() {
    import std.stdio, std.algorithm;

    levenshteinDistance("kitten", "sitting").writeln;
}
```

{{out}}

```txt
3
```



### Iterative Version

{{trans|Java}}

```d
import std.stdio, std.algorithm;

int distance(in string s1, in string s2) pure nothrow {
  auto costs = new int[s2.length + 1];

  foreach (immutable i; 0 .. s1.length + 1) {
    int lastValue = i;
    foreach (immutable j; 0 .. s2.length + 1) {
      if (i == 0)
        costs[j] = j;
      else {
        if (j > 0) {
          int newValue = costs[j - 1];
          if (s1[i - 1] != s2[j - 1])
            newValue = min(newValue, lastValue, costs[j]) + 1;
          costs[j - 1] = lastValue;
          lastValue = newValue;
        }
      }
    }

    if (i > 0)
      costs[$ - 1] = lastValue;
  }

  return costs[$ - 1];
}

void main() {
  foreach (p; [["kitten", "sitting"], ["rosettacode", "raisethysword"]])
    writefln("distance(%s, %s): %d", p[0], p[1], distance(p[0], p[1]));
}
```



### Memoized Recursive Version

{{trans|Python}}

```d
import std.stdio, std.array, std.algorithm, std.functional;

uint lDist(T)(in const(T)[] s, in const(T)[] t) nothrow {
    alias mlDist = memoize!lDist;
    if (s.empty || t.empty) return max(t.length, s.length);
    if (s[0] == t[0]) return mlDist(s[1 .. $], t[1 .. $]);
    return min(mlDist(s, t[1 .. $]),
               mlDist(s[1 .. $], t),
               mlDist(s[1 .. $], t[1 .. $])) + 1;
}

void main() {
    lDist("kitten", "sitting").writeln;
    lDist("rosettacode", "raisethysword").writeln;
}
```



## DWScript

Based on Wikipedia version

```delphi
function LevenshteinDistance(s, t : String) : Integer;
var
   i, j : Integer;
begin
   var d:=new Integer[Length(s)+1, Length(t)+1];
   for i:=0 to Length(s) do
      d[i, 0] := i;
   for j:=0 to Length(t) do
      d[0, j] := j;

   for j:=1 to Length(t) do
      for i:=1 to Length(s) do
         if s[i]=t[j] then
            d[i, j] := d[i-1, j-1] // no operation
         else d[i,j]:=MinInt(MinInt(
               d[i-1, j] +1 ,    // a deletion
               d[i, j-1] +1 ),   // an insertion
               d[i- 1,j-1] +1    // a substitution
               );
   Result:=d[Length(s), Length(t)];
end;

PrintLn(LevenshteinDistance('kitten', 'sitting'));
```



## Dyalect



```dyalect
func min(x, y) {
    if x < y {
        x
    } else {
        y
    }
}

func levenshtein(s, t) {
    var n = s.len()
    var m = t.len()
    var d = Array.empty(n + 1, () => Array.empty(m + 1))

    if n == 0 {
        return m
    }

    if (m == 0) {
        return n
    }

    for i in 0..n {
        d[i][0] = i
    }

    for j in 0..m {
        d[0][j] = j
    }

    for j in 1..m {
        for i in 1..n {
            if s[i - 1] == t[j - 1] {
                d[i][j] = d[i - 1][j - 1] //no operation
            }
            else {
                d[i][j] = min(min(
                    d[i - 1][j] + 1,    //a deletion
                    d[i][j - 1] + 1),   //an insertion
                    d[i - 1][j - 1] + 1 //a substitution
                    )
            }
        }
    }

    d[n][m]
}

func run(x, y) {
    print("\(x) -> \(y) = \(levenshtein(x, y))")
}

run("rosettacode", "raisethysword")
```


{{out}}


```txt
rosettacode -> raisethysword = 8
```



## EchoLisp


```lisp

;; Recursive version adapted from Clojure
;; Added necessary memoization

(define (levenshtein str1 str2 (cost 0) (rest1 0) (rest2 0) (key null))
(set! key (string-append str1 "|" str2))
(if (get 'mem key) ;; memoized ?
    (get 'mem key)
;; else memoize
(putprop 'mem
  (let [(len1 (string-length str1))
        (len2 (string-length str2))]
    (cond ((zero? len1) len2)
          ((zero? len2) len1)
          (else
          (set! cost (if (= (string-first str1) (string-first str2)) 0 1))
          (set! rest1 (string-rest str1))
          (set! rest2 (string-rest str2))
            (min (1+ (levenshtein rest1 str2))
                 (1+ (levenshtein str1 rest2))
                 (+ cost
                    (levenshtein rest1 rest2 ))))))
    key)))

;; 😛 127 calls with memoization
;; 😰 29737 calls without memoization
(levenshtein "kitten" "sitting") → 3

(levenshtein "rosettacode" "raisethysword") → 8

```



## Ela

This code is translated from Haskell version.


```ela
open list

levenshtein s1 s2 = last <| foldl transform [0 .. length s1] s2
            where transform (n::ns')@ns c = scanl calc (n+1) <| zip3 s1 ns ns'
                        where calc z (c', x, y) = minimum [y+1, z+1, x + toInt (c' <> c)]
```


Executing:


```ela
(levenshtein "kitten" "sitting", levenshtein "rosettacode" "raisethysword")
```

{{out}}

```txt
(3, 8)
```



## Elixir

{{trans|Ruby}}

```elixir
defmodule Levenshtein do
  def distance(a, b) do
    ta = String.downcase(a) |> to_char_list |> List.to_tuple
    tb = String.downcase(b) |> to_char_list |> List.to_tuple
    m = tuple_size(ta)
    n = tuple_size(tb)
    costs = Enum.reduce(0..m, %{},   fn i,acc -> Map.put(acc, {i,0}, i) end)
    costs = Enum.reduce(0..n, costs, fn j,acc -> Map.put(acc, {0,j}, j) end)
    Enum.reduce(0..n-1, costs, fn j, acc ->
      Enum.reduce(0..m-1, acc, fn i, map ->
        d = if elem(ta, i) == elem(tb, j) do
              map[ {i,j} ]
            else
              Enum.min([ map[ {i  , j+1} ] + 1,         # deletion
                         map[ {i+1, j  } ] + 1,         # insertion
                         map[ {i  , j  } ] + 1 ])       # substitution
            end
        Map.put(map, {i+1, j+1}, d)
      end)
    end)
    |> Map.get({m,n})
  end
end

words = ~w(kitten sitting saturday sunday rosettacode raisethysword)
Enum.each(Enum.chunk(words, 2), fn [a,b] ->
  IO.puts "distance(#{a}, #{b}) = #{Levenshtein.distance(a,b)}"
end)
```


{{out}}

```txt

distance(kitten, sitting) = 3
distance(saturday, sunday) = 3
distance(rosettacode, raisethysword) = 8

```



## Erlang

Here are two implementations. The first is the naive version, the second is a memoized version using Erlang's dictionary datatype.

```erlang

-module(levenshtein).
-compile(export_all).

distance_cached(S,T) ->
    {L,_} = ld(S,T,dict:new()),
    L.

distance(S,T) ->
    ld(S,T).

ld([],T) ->
    length(T);
ld(S,[]) ->
    length(S);
ld([X|S],[X|T]) ->
    ld(S,T);
ld([_SH|ST]=S,[_TH|TT]=T) ->
    1 + lists:min([ld(S,TT),ld(ST,T),ld(ST,TT)]).

ld([]=S,T,Cache) ->
    {length(T),dict:store({S,T},length(T),Cache)};
ld(S,[]=T,Cache) ->
    {length(S),dict:store({S,T},length(S),Cache)};
ld([X|S],[X|T],Cache) ->
    ld(S,T,Cache);
ld([_SH|ST]=S,[_TH|TT]=T,Cache) ->
    case dict:is_key({S,T},Cache) of
        true -> {dict:fetch({S,T},Cache),Cache};
        false ->
            {L1,C1} = ld(S,TT,Cache),
            {L2,C2} = ld(ST,T,C1),
            {L3,C3} = ld(ST,TT,C2),
            L = 1+lists:min([L1,L2,L3]),
            {L,dict:store({S,T},L,C3)}
    end.

```

Below is a comparison of the runtimes, measured in microseconds, between the two implementations.

```erlang

68> timer:tc(levenshtein,distance,["rosettacode","raisethysword"]).
{774799,8} % {Time, Result}
69> timer:tc(levenshtein,distance_cached,["rosettacode","raisethysword"]).
{659,8}
70> timer:tc(levenshtein,distance,["kitten","sitting"]).
{216,3}
71> timer:tc(levenshtein,distance_cached,["kitten","sitting"]).
{213,3}

```



## ERRE


```ERRE

PROGRAM LEVENSHTEIN

!$DYNAMIC
  DIM D%[0,0]

PROCEDURE LEVENSHTEIN(S$,T$->RES%)
      LOCAL I%,J%,M%
      FOR I%=0 TO LEN(S$) DO
        D%[I%,0]=I%
      END FOR
      FOR J%=0 TO LEN(T$) DO
        D%[0,J%]=J%
      END FOR
      FOR J%=1 TO LEN(T$) DO
        FOR I%=1 TO LEN(S$) DO
          IF MID$(S$,I%,1)=MID$(T$,J%,1) THEN
            D%[I%,J%]=D%[I%-1,J%-1]
          ELSE
            M%=D%[I%-1,J%-1]
            IF D%[I%,J%-1]<M% THEN M%=D%[I%,J%-1] END IF
            IF D%[I%-1,J%]<M% THEN M%=D%[I%-1,J%] END IF
            D%[I%,J%]=M%+1
          END IF
        END FOR
      END FOR
      RES%=D%[I%-1,J%-1]
END PROCEDURE

BEGIN
   S$="kitten"  T$="sitting"
   PRINT("'";S$;"' -> '";T$;"' has distance ";)
   !$DIM D%[LEN(S$),LEN(T$)]
   LEVENSHTEIN(S$,T$->RES%)
   PRINT(RES%)
   !$ERASE D%

   S$="rosettacode" T$="raisethysword"
   PRINT("'";S$;"' -> '";T$;"' has distance ";)
   !$DIM D%[LEN(S$),LEN(T$)]
   LEVENSHTEIN(S$,T$->RES%)
   PRINT(RES%)
   !$ERASE D%
END PROGRAM

```

{{out}}
```txt

'kitten' -> 'sitting' has distance  3
'rosettacode' -> 'raisethysword' has distance  8

```



## Euphoria

Code by Jeremy Cowgar from the [http://www.rapideuphoria.com/cgi-bin/asearch.exu?gen=on&keywords=Levenshtein Euphoria File Archive].

```euphoria
function min(sequence s)
    atom m
    m = s[1]
    for i = 2 to length(s) do
        if s[i] < m then
            m = s[i]
        end if
    end for
    return m
end function

function levenshtein(sequence s1, sequence s2)
    integer n, m
    sequence d
    n = length(s1) + 1
    m = length(s2) + 1

    if n = 1  then
        return m-1
    elsif m = 1 then
        return n-1
    end if

    d = repeat(repeat(0, m), n)
    for i = 1 to n do
        d[i][1] = i-1
    end for

    for j = 1 to m do
        d[1][j] = j-1
    end for

    for i = 2 to n do
        for j = 2 to m do
            d[i][j] = min({
                d[i-1][j] + 1,
                d[i][j-1] + 1,
                d[i-1][j-1] + (s1[i-1] != s2[j-1])
            })
        end for
    end for

    return d[n][m]
end function

? levenshtein("kitten", "sitting")
? levenshtein("rosettacode", "raisethysword")
```

{{out}}

```txt
3
8

```


=={{header|F Sharp|F#}}==

```FSharp

open System

let getInput (name : string) =
    Console.Write ("String {0}: ", name)
    Console.ReadLine ()

let levDist (strOne : string) (strTwo : string) =
    let strOne = strOne.ToCharArray ()
    let strTwo = strTwo.ToCharArray ()

    let (distArray : int[,]) = Array2D.zeroCreate (strOne.Length + 1) (strTwo.Length + 1)

    for i = 0 to strOne.Length do distArray.[i, 0] <- i
    for j = 0 to strTwo.Length do distArray.[0, j] <- j

    for j = 1 to strTwo.Length do
        for i = 1 to strOne.Length do
            if strOne.[i - 1] = strTwo.[j - 1] then distArray.[i, j] <- distArray.[i - 1, j - 1]
            else
                distArray.[i, j] <- List.min (
                    [distArray.[i-1, j] + 1;
                    distArray.[i, j-1] + 1;
                    distArray.[i-1, j-1] + 1]
                )
    distArray.[strOne.Length, strTwo.Length]


let stringOne = getInput "One"
let stringTwo = getInput "Two"
printf "%A" (levDist stringOne stringTwo)

Console.ReadKey () |> ignore

```



## Factor


```factor
USING: lcs prettyprint ;
"kitten" "sitting" levenshtein .
```

{{out}}

```txt

3

```



## Forth

{{trans|C}}

```forth
: levenshtein                          ( a1 n1 a2 n2 -- n3)
  dup                                  \ if either string is empty, difference
  if                                   \ is inserting all chars from the other
    2>r dup
    if
      2dup 1- chars + c@ 2r@ 1- chars + c@ =
      if
        1- 2r> 1- recurse exit
      else                             \ else try:
        2dup 1- 2r@ 1- recurse -rot    \   changing first letter of s to t;
        2dup    2r@ 1- recurse -rot    \   remove first letter of s;
        1- 2r> recurse min min 1+      \   remove first letter of t,
      then                             \ any of which is 1 edit plus
    else                               \ editing the rest of the strings
      2drop 2r> nip
    then
  else
    2drop nip
  then
;

s" kitten"      s" sitting"       levenshtein . cr
s" rosettacode" s" raisethysword" levenshtein . cr
```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' Uses the "iterative with two matrix rows" algorithm
' referred to in the Wikipedia article.

Function min(x As Integer, y As Integer) As Integer
   Return IIf(x < y, x, y)
End Function

Function levenshtein(s As String, t As String) As Integer
    ' degenerate cases
    If s = t Then Return 0
    If s = "" Then Return Len(t)
    If t = "" Then Return Len(s)

    ' create two integer arrays of distances
    Dim v0(0 To Len(t)) As Integer  '' previous
    Dim v1(0 To Len(t)) As Integer  '' current

    ' initialize v0
    For i As Integer = 0 To Len(t)
      v0(i) = i
    Next

    Dim cost As Integer
    For i As Integer = 0 To Len(s) - 1
      ' calculate v1 from v0
      v1(0) = i + 1

      For j As Integer = 0 To Len(t) - 1
        cost = IIf(s[i] = t[j], 0, 1)
        v1(j + 1) = min(v1(j) + 1, min(v0(j + 1) + 1, v0(j) + cost))
      Next j

      ' copy v1 to v0 for next iteration
      For j As Integer = 0 To Len(t)
        v0(j) = v1(j)
      Next  j
    Next i

    Return v1(Len(t))
End Function

Print "'kitten' to 'sitting'            => "; levenshtein("kitten", "sitting")
Print "'rosettacode' to 'raisethysword' => "; levenshtein("rosettacode", "raisethysword")
Print "'sleep' to 'fleeting'            => "; levenshtein("sleep", "fleeting")
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

'kitten' to 'sitting'            =>  3
'rosettacode' to 'raisethysword' =>  8
'sleep' to 'fleeting'            =>  5

```



## Frink

Frink has a built-in function to calculate the Levenshtein edit distance between two strings:

```frink
println[editDistance["kitten","sitting"]]
```



## FutureBasic

Based on Wikipedia algorithm. Suitable for Pascal strings.

```futurebasic

include "ConsoleWindow"

local fn LevenshteinDistance( aStr as Str255, bStr as Str255 ) as long
dim as long m, n, i, j, min, k, l
dim as long distance( 255, 255 )

m = len(aStr)
n = len(bStr)

for i = 0 to m
   distance( i, 0 ) = i
next

for j = 0 to n
   distance( 0, j ) = j
next

for j = 1 to n
   for i = 1 to m
      if mid$( aStr, i, 1 ) == mid$( bStr, j, 1 )
         distance( i, j ) = distance( i-1, j-1 )
      else
         min = distance( i-1, j   ) + 1
         k   = distance( i, j - 1 ) + 1
         l   = distance( i-1, j-1 ) + 1
         if k < min then min = k
         if l < min then min = l
         distance( i, j ) = min
     end if
   next
next
end fn = distance( m, n )

dim as long i
dim as Str255 testStr( 5, 2 )

testStr( 0, 0 ) = "kitten"      : testStr( 0, 1 ) = "sitting"
testStr( 1, 0 ) = "rosettacode" : testStr( 1, 1 ) = "raisethysword"
testStr( 2, 0 ) = "Saturday"    : testStr( 2, 1 ) = "Sunday"
testStr( 3, 0 ) = "FutureBasic" : testStr( 3, 1 ) = "FutureBasic"
testStr( 4, 0 ) = "here's a bunch of words"
testStr( 4, 1 ) = "to wring out this code"

for i = 0 to 4
   print "1st string = "; testStr( i, 0 )
   print "2nd string = "; testStr( i, 1 )
   print "Levenshtein distance ="; fn LevenshteinDistance( testStr( i, 0 ), testStr( i, 1 ) )
   print
next

```


Output:

```txt

1st string = kitten
2nd string = sitting
Levenshtein distance = 3

1st string = rosettacode
2nd string = raisethysword
Levenshtein distance = 8

1st string = Saturday
2nd string = Sunday
Levenshtein distance = 3

1st string = FutureBasic
2nd string = FutureBasic
Levenshtein distance = 0

1st string = here's a bunch of words
2nd string = to wring out this code
Levenshtein distance = 18

```



## Go

WP algorithm:

```go
package main

import "fmt"

func main() {
    fmt.Println(ld("kitten", "sitting"))
}

func ld(s, t string) int {
    d := make([][]int, len(s)+1)
    for i := range d {
        d[i] = make([]int, len(t)+1)
    }
    for i := range d {
        d[i][0] = i
    }
    for j := range d[0] {
        d[0][j] = j
    }
    for j := 1; j <= len(t); j++ {
        for i := 1; i <= len(s); i++ {
            if s[i-1] == t[j-1] {
                d[i][j] = d[i-1][j-1]
            } else {
                min := d[i-1][j]
                if d[i][j-1] < min {
                    min = d[i][j-1]
                }
                if d[i-1][j-1] < min {
                    min = d[i-1][j-1]
                }
                d[i][j] = min + 1
            }
        }

    }
    return d[len(s)][len(t)]
}
```

{{out}}

```txt

3

```

{{trans|C}}

```go
package main

import "fmt"

func levenshtein(s, t string) int {
    if s == "" { return len(t) }
    if t == "" { return len(s) }
    if s[0] == t[0] {
        return levenshtein(s[1:], t[1:])
    }
    a := levenshtein(s[1:], t[1:])
    b := levenshtein(s,     t[1:])
    c := levenshtein(s[1:], t)
    if a > b { a = b }
    if a > c { a = c }
    return a + 1
}

func main() {
    s1 := "rosettacode"
    s2 := "raisethysword"
    fmt.Printf("distance between %s and %s: %d\n", s1, s2,
        levenshtein(s1, s2))
}
```

{{out}}

```txt

distance between rosettacode and raisethysword: 8

```



## Groovy


```groovy
def distance(String str1, String str2) {
    def dist = new int[str1.size() + 1][str2.size() + 1]
    (0..str1.size()).each { dist[it][0] = it }
    (0..str2.size()).each { dist[0][it] = it }

    (1..str1.size()).each { i ->
        (1..str2.size()).each { j ->
            dist[i][j] = [dist[i - 1][j] + 1, dist[i][j - 1] + 1, dist[i - 1][j - 1] + ((str1[i - 1] == str2[j - 1]) ? 0 : 1)].min()
        }
    }
    return dist[str1.size()][str2.size()]
}

[ ['kitten', 'sitting']: 3,
  ['rosettacode', 'raisethysword']: 8,
  ['edocattesor', 'drowsyhtesiar']: 8 ].each { key, dist ->
    println "Checking distance(${key[0]}, ${key[1]}) == $dist"
    assert distance(key[0], key[1]) == dist
}
```

{{out}}

```txt

Checking distance(kitten, sitting) == 3
Checking distance(rosettacode, raisethysword) == 8
Checking distance(edocattesor, drowsyhtesiar) == 8

```



## Haskell


```haskell>levenshtein :: Eq a =
 [a] -> [a] -> Int
levenshtein s1 s2 = last $ foldl transform [0 .. length s1] s2
  where
    transform ns@(n:ns1) c = scanl calc (n + 1) $ zip3 s1 ns ns1
      where
        calc z (c1, x, y) = minimum [y + 1, z + 1, x + fromEnum (c1 /= c)]

main :: IO ()
main = print (levenshtein "kitten" "sitting")
```

{{Out}}

```txt
3
```


=={{header|Icon}} and {{header|Unicon}}==

```unicon
procedure main()
    every process(!&input)
end

procedure process(s)
    s ? (s1 := tab(upto(' \t')), s2 := (tab(many(' \t')), tab(0))) | fail
    write("'",s1,"' -> '",s2,"' = ", levenshtein(s1,s2))
end

procedure levenshtein(s, t)
    if (n := *s+1) = 1 then return *t
    if (m := *t+1) = 1 then return *s

    every !(d := list(n,0)) := list(m, 0)
    every i := 1 to max(n,m) do d[i,1] := d[1,i] := i
    every d[1(i := 2 to n, s_i := s[iM1 := i-1]), j := 2 to m] :=
             min(d[iM1,j], d[i,jM1:=j-1],
                 d[iM1,jM1] + if s_i == t[jM1] then -1 else 0) + 1

    return d[n,m]-1
end
```

{{out|Example}}

```txt

->leven
kitten  sitting
'kitten' -> 'sitting' = 3
->

```



## Io

A <code>levenshtein</code> method is available on strings when the standard <code>Range</code> addon is loaded.
<lang>Io 20110905
Io> Range ; "kitten" levenshtein("sitting")
==> 3
Io> "rosettacode" levenshtein("raisethysword")
==> 8
Io>
```



## J

One approach would be a literal transcription of the [[wp:Levenshtein_distance#Computing_Levenshtein_distance|wikipedia implementation]]:

```j
levenshtein=:4 :0
  D=. x +/&i.&>:&# y
  for_i.1+i.#x do.
    for_j.1+i.#y do.
      if. ((<:i){x)=(<:j){y do.
        D=.(D {~<<:i,j) (<i,j)} D
      else.
        min=. 1+<./D{~(i,j) <@:-"1#:1 2 3
        D=. min (<i,j)} D
      end.
    end.
  end.
  {:{:D
)
```

However, this is a rather slow and bulky approach.  Another alternative would be:

```j
levD=: i.@-@>:@#@] ,~ >:@i.@-@#@[ ,.~ ~:/
lev=: [: {. {."1@((1&{ ,~ (1 1 , {.) <./@:+ }.)@,/\.)@,./@levD
```

First, we setup up an matrix of costs, with 0 or 1 for unexplored cells (1 being where the character pair corresponding to that cell position has two different characters).  Note that the "cost to reach the empty string" cells go on the bottom and the right, instead of the top and the left, because this works better with J's "[http://www.jsoftware.com/help/dictionary/d420.htm insert]" operation (which we will call "reduce" in the next paragraph here. It could also be thought of as a right fold which has been constrained such the initial value is the identity value for the operation. Or, just think of it as inserting its operation between each item of its argument...).

Then we reduce the rows of that matrix using an operation that treats those two rows as columns and reduces the rows of this derived matrix with an operation that gives the (unexplored cell + the minumum of the other cells) followed by (the cell adjacent to the previously unexplored cell.
{{out|Example use}}

```j
   'kitten' levenshtein 'sitting'
3
   'kitten' lev 'sitting'
3
```

Time and space use:

```j
   ts=: 6!:2,7!:2
   ts '''kitten'' levenshtein ''sitting'''
0.00153132 12800
   ts '''kitten'' lev ''sitting'''
0.000132101 5376
```

(The J flavored variant winds up being about 10 times faster, in J, for this test case, than the explicit version.)

See the [[j:Essays/Levenshtein Distance|Levenshtein distance essay]] on the Jwiki for additional solutions.


## Java

Based on the definition for Levenshtein distance given in the Wikipedia article:

```java
public class Levenshtein {

    public static int distance(String a, String b) {
        a = a.toLowerCase();
        b = b.toLowerCase();
        // i == 0
        int [] costs = new int [b.length() + 1];
        for (int j = 0; j < costs.length; j++)
            costs[j] = j;
        for (int i = 1; i <= a.length(); i++) {
            // j == 0; nw = lev(i - 1, j)
            costs[0] = i;
            int nw = i - 1;
            for (int j = 1; j <= b.length(); j++) {
                int cj = Math.min(1 + Math.min(costs[j], costs[j - 1]), a.charAt(i - 1) == b.charAt(j - 1) ? nw : nw + 1);
                nw = costs[j];
                costs[j] = cj;
            }
        }
        return costs[b.length()];
    }

    public static void main(String [] args) {
        String [] data = { "kitten", "sitting", "saturday", "sunday", "rosettacode", "raisethysword" };
        for (int i = 0; i < data.length; i += 2)
            System.out.println("distance(" + data[i] + ", " + data[i+1] + ") = " + distance(data[i], data[i+1]));
    }
}
```

{{out}}

```txt
distance(kitten, sitting) = 3
distance(saturday, sunday) = 3
distance(rosettacode, raisethysword) = 8

```

{{trans|C}}

```java
public class Levenshtein{
    public static int levenshtein(String s, String t){
        /* if either string is empty, difference is inserting all chars
         * from the other
         */
        if(s.length() == 0) return t.length();
        if(t.length() == 0) return s.length();

        /* if first letters are the same, the difference is whatever is
         * required to edit the rest of the strings
         */
        if(s.charAt(0) == t.charAt(0))
            return levenshtein(s.substring(1), t.substring(1));

        /* else try:
         *      changing first letter of s to that of t,
         *      remove first letter of s, or
         *      remove first letter of t
         */
        int a = levenshtein(s.substring(1), t.substring(1));
        int b = levenshtein(s, t.substring(1));
        int c = levenshtein(s.substring(1), t);

        if(a > b) a = b;
        if(a > c) a = c;

        //any of which is 1 edit plus editing the rest of the strings
        return a + 1;
    }

    public static void main(String[] args) {
        String s1 = "kitten";
        String s2 = "sitting";
        System.out.println("distance between '" + s1 + "' and '"
                + s2 + "': " + levenshtein(s1, s2));
        s1 = "rosettacode";
        s2 = "raisethysword";
        System.out.println("distance between '" + s1 + "' and '"
                + s2 + "': " + levenshtein(s1, s2));
        StringBuilder sb1 = new StringBuilder(s1);
        StringBuilder sb2 = new StringBuilder(s2);
        System.out.println("distance between '" + sb1.reverse() + "' and '"
                + sb2.reverse() + "': "
                + levenshtein(sb1.reverse().toString(), sb2.reverse().toString()));
    }
}
```

{{out}}

```txt
distance between 'kitten' and 'sitting': 3
distance between 'rosettacode' and 'raisethysword': 8
distance between 'edocattesor' and 'drowsyhtesiar': 8
```


===Iterative space optimized (even bounded) ===
{{trans|Python}}

```java

import static java.lang.Math.abs;
import static java.lang.Math.max;

public class Levenshtein {

	public static int ld(String a, String b) {
		return distance(a, b, -1);
	}
	public static boolean ld(String a, String b, int max) {
		return distance(a, b, max) <= max;
	}

	private static int distance(String a, String b, int max) {
		if (a == b) return 0;
		int la = a.length();
		int lb = b.length();
		if (max >= 0 && abs(la - lb) > max) return max+1;
		if (la == 0) return lb;
		if (lb == 0) return la;
		if (la < lb) {
			int tl = la; la = lb; lb = tl;
			String ts = a;  a = b; b = ts;
		}

		int[] cost = new int[lb+1];
		for (int i=1; i<=la; i+=1) {
			cost[0] = i;
			int prv = i-1;
			int min = prv;
			for (int j=1; j<=lb; j+=1) {
				int act = prv + (a.charAt(i-1) == b.charAt(j-1) ? 0 : 1);
				cost[j] = min(1+(prv=cost[j]), 1+cost[j-1], act);
				if (prv < min) min = prv;
			}
			if (max >= 0 && min > max) return max+1;
		}
		if (max >= 0 && cost[lb] > max) return max+1;
		return cost[lb];
	}

	private static int min(int ... a) {
		int min = Integer.MAX_VALUE;
		for (int i: a) if (i<min) min = i;
		return min;
	}

	public static void main(String[] args) {
		System.out.println(
			ld("kitten","kitten") + " " + // 0
			ld("kitten","sitten") + " " + // 1
			ld("kitten","sittes") + " " + // 2
			ld("kitten","sityteng") + " " + // 3
			ld("kitten","sittYing") + " " + // 4
			ld("rosettacode","raisethysword") + " " + // 8
			ld("kitten","kittenaaaaaaaaaaaaaaaaa") + " " + // 17
			ld("kittenaaaaaaaaaaaaaaaaa","kitten") // 17
		);
		System.out.println(
			ld("kitten","kitten", 3) + " " + // true
			ld("kitten","sitten", 3) + " " + // true
			ld("kitten","sittes", 3) + " " + // true
			ld("kitten","sityteng", 3) + " " + // true
			ld("kitten","sittYing", 3) + " " + // false
			ld("rosettacode","raisethysword", 3) + " " + // false
			ld("kitten","kittenaaaaaaaaaaaaaaaaa", 3) + " " + // false
			ld("kittenaaaaaaaaaaaaaaaaa","kitten", 3) // false
		);
	}
}

```

{{out}}

```txt

0 1 2 3 4 8 17 17
true true true true false false false false

```



## JavaScript


### ES5

Iterative:

```javascript
function levenshtein(a, b) {
  var t = [], u, i, j, m = a.length, n = b.length;
  if (!m) { return n; }
  if (!n) { return m; }
  for (j = 0; j <= n; j++) { t[j] = j; }
  for (i = 1; i <= m; i++) {
    for (u = [i], j = 1; j <= n; j++) {
      u[j] = a[i - 1] === b[j - 1] ? t[j - 1] : Math.min(t[j - 1], t[j], u[j - 1]) + 1;
    } t = u;
  } return u[n];
}

// tests
[ ['', '', 0],
  ['yo', '', 2],
  ['', 'yo', 2],
  ['yo', 'yo', 0],
  ['tier', 'tor', 2],
  ['saturday', 'sunday', 3],
  ['mist', 'dist', 1],
  ['tier', 'tor', 2],
  ['kitten', 'sitting', 3],
  ['stop', 'tops', 2],
  ['rosettacode', 'raisethysword', 8],
  ['mississippi', 'swiss miss', 8]
].forEach(function(v) {
  var a = v[0], b = v[1], t = v[2], d = levenshtein(a, b);
  if (d !== t) {
    console.log('levenstein("' + a + '","' + b + '") was ' + d + ' should be ' + t);
  }
});
```



### ES6

{{Trans|Haskell}}

By composition of generic functions:

```JavaScript
(() => {
    'use strict';

    // levenshtein :: String -> String -> Int
    const levenshtein = (sa, sb) => {
        const [s1, s2] = [sa.split(''), sb.split('')];

        return last(s2.reduce((ns, c) => {
            const [n, ns1] = uncons(ns);

            return scanl(
                (z, [c1, x, y]) =>
                minimum(
                    [y + 1, z + 1, x + fromEnum(c1 != c)]
                ),
                n + 1,
                zip3(s1, ns, ns1)
            );
        }, range(0, s1.length)));
    };


    /*********************************************************************/
    // GENERIC FUNCTIONS

    // minimum :: [a] -> a
    const minimum = xs =>
        xs.reduce((a, x) => (x < a || a === undefined ? x : a), undefined);

    // fromEnum :: Enum a => a -> Int
    const fromEnum = x => {
        const type = typeof x;
        return type === 'boolean' ? (
            x ? 1 : 0
        ) : (type === 'string' ? x.charCodeAt(0) : undefined);
    };

    // uncons :: [a] -> Maybe (a, [a])
    const uncons = xs => xs.length ? [xs[0], xs.slice(1)] : undefined;

    // scanl :: (b -> a -> b) -> b -> [a] -> [b]
    const scanl = (f, a, xs) => {
        for (var lst = [a], lng = xs.length, i = 0; i < lng; i++) {
            a = f(a, xs[i], i, xs), lst.push(a);
        }
        return lst;
    };

    // zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
    const zip3 = (xs, ys, zs) =>
        xs.slice(0, Math.min(xs.length, ys.length, zs.length))
        .map((x, i) => [x, ys[i], zs[i]]);

    // last :: [a] -> a
    const last = xs => xs.length ? xs.slice(-1) : undefined;

    // range :: Int -> Int -> [Int]
    const range = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    /*********************************************************************/
    // TEST
    return [
        ["kitten", "sitting"],
        ["sitting", "kitten"],
        ["rosettacode", "raisethysword"],
        ["raisethysword", "rosettacode"]
    ].map(pair => levenshtein.apply(null, pair));

    // -> [3, 3, 8, 8]
})();
```


{{Out}}

```JavaScript
[3, 3, 8, 8]
```



## jq

The main point of interest about the following implementation is that it shows how the naive recursive algorithm can be tweaked within a completely functional framework to yield an efficient implementation.

'''Performance''':
Here is a breakdown of the run-times on a 2.53GHz machine:

   9ms overhead (invoking jq and compiling the program)
  17ms for kitten/sitting
  67ms for rosettacode/raisethysword
  71ms for edocattesor/drowsyhtesiar

```jq

# lookup the distance between s and t in the nested cache,
# which uses basic properties of the Levenshtein distance to save space:
def lookup(s;t):
  if (s == t) then 0
  elif (s|length) == 0 then (t|length)
  elif (t|length) == 0 then (s|length)
  elif (s|length) > (t|length) then
       .[t] as $t | if $t then $t[s] else null end
  else .[s] as $s | if $s then $s[t] else null end
  end ;

# output is the updated cache;
# basic properties of the Levenshtein distance are used to save space:
def store(s;t;value):
  if (s == t) then .
  else (s|length) as $s | (t|length) as $t
    | if $s == 0 or $t == 0 then .
      elif $s < $t then .[s][t] = value
      elif $t < $s then .[t][s] = value
      else (.[s][t] = value) | (.[t][s] = value)
      end
  end ;

# Input is a cache of nested objects; output is [distance, cache]
def ld(s1; s2):

  # emit [distance, cache]
  # input: cache
  def cached_ld(s;t):
    lookup(s;t) as $check
    | if $check then [ $check, . ]
      else ld(s;t)
      end
  ;

  # If either string is empty,
  # then distance is insertion of the other's characters.
  if   (s1|length) == 0 then [(s2|length), .]
  elif (s2|length) == 0 then [(s1|length), .]
  elif (s1[0:1] == s2[0:1]) then
    cached_ld(s1[1:]; s2[1:])
  else
    cached_ld(s1[1:]; s2) as $a
    | ($a[1] | cached_ld(s1; s2[1:])) as $b
    | ($b[1] | cached_ld(s1[1:]; s2[1:])) as $c
    | [$a[0], $b[0], $c[0]] | (min + 1) as $d
    | [$d, ($c[1] | store(s1;s2;$d)) ]
  end ;

def levenshteinDistance(s;t):
  s as $s | t as $t | {} | ld($s;$t) | .[0];
```

'''Task'''

```jq
def demo:
 "levenshteinDistance between \(.[0]) and \(.[1]) is \( levenshteinDistance(.[0]; .[1]) )";

(["kitten", "sitting"] | demo),
(["rosettacode","raisethysword"] | demo),
(["edocattesor", "drowsyhtesiar"] | demo),
(["this_algorithm_is_similar_to",
  "Damerau-Levenshtein_distance"] | demo)
```

{{Out}}
 levenshteinDistance between kitten and sitting is 3
 levenshteinDistance between rosettacode and raisethysword is 8
 levenshteinDistance between edocattesor and drowsyhtesiar is 8


## Jsish

From Javascript, ES5 entry.

```javascript
/* Levenshtein Distance, in Jsish */

function levenshtein(a, b) {
  var t = [], u, i, j, m = a.length, n = b.length;
  if (!m) { return n; }
  if (!n) { return m; }
  for (j = 0; j <= n; j++) { t[j] = j; }
  for (i = 1; i <= m; i++) {
    for (u = [i], j = 1; j <= n; j++) {
      u[j] = a[i - 1] === b[j - 1] ? t[j - 1] : Math.min(t[j - 1], t[j], u[j - 1]) + 1;
    } t = u;
  } return u[n];
}

provide('levenshtein', 1);

;levenshtein('', '');
;levenshtein('yo', '');
;levenshtein('', 'yo');
;levenshtein('yo', 'yo');
;levenshtein('tier', 'tor');
;levenshtein('saturday', 'sunday');
;levenshtein('mist', 'dist');
;levenshtein('tier', 'tor');
;levenshtein('kitten', 'sitting');
;levenshtein('stop', 'tops');
;levenshtein('rosettacode', 'raisethysword');
;levenshtein('mississippi', 'swiss miss');

/*
=!EXPECTSTART!=
levenshtein('', '') ==> 0
levenshtein('yo', '') ==> 2
levenshtein('', 'yo') ==> 2
levenshtein('yo', 'yo') ==> 0
levenshtein('tier', 'tor') ==> 2
levenshtein('saturday', 'sunday') ==> 3
levenshtein('mist', 'dist') ==> 1
levenshtein('tier', 'tor') ==> 2
levenshtein('kitten', 'sitting') ==> 3
levenshtein('stop', 'tops') ==> 2
levenshtein('rosettacode', 'raisethysword') ==> 8
levenshtein('mississippi', 'swiss miss') ==> 8
=!EXPECTEND!=
*/
```

{{out}}

```txt
prompt$ jsish -u levenshtein.jsi
[PASS] levenshtein.jsi
```



## Julia


'''Recursive''':
{{works with|Julia|1.0}}

```julia
function levendist(s::AbstractString, t::AbstractString)
    ls, lt = length.((s, t))
    ls == 0 && return lt
    lt == 0 && return ls

    s₁, t₁ = s[2:end], t[2:end]
    ld₁ = levendist(s₁, t₁)
    s[1] == t[1] ? ld₁ : 1 + min(ld₁, levendist(s, t₁), levendist(s₁, t))
end

@show levendist("kitten", "sitting") # 3
@show levendist("rosettacode", "raisethysword") # 8
```


'''Iterative''':
{{works with|Julia|0.6}}

```julia
function levendist1(s::AbstractString, t::AbstractString)
    ls, lt = length(s), length(t)
    if ls > lt
        s, t = t, s
        ls, lt = lt, ls
    end
    dist = collect(0:ls)
    for (ind2, chr2) in enumerate(t)
        newdist = Vector{Int}(ls+1)
        newdist[1] = ind2
        for (ind1, chr1) in enumerate(s)
            if chr1 == chr2
                newdist[ind1+1] = dist[ind1]
            else
                newdist[ind1+1] = 1 + min(dist[ind1], dist[ind1+1], newdist[end])
            end
        end
        dist = newdist
    end
    return dist[end]
end
```


Let's see some benchmark:

```julia
using BenchmarkTools
println("\n# levendist(kitten, sitting)")
s, t = "kitten", "sitting"
println(" - Recursive:")
@btime levendist(s, t)
println(" - Iterative:")
@btime levendist1(s, t)
println("\n# levendist(rosettacode, raisethysword)")
s, t = "rosettacode", "raisethysword"
println(" - Recursive:")
@btime levendist(s, t)
println(" - Iterative:")
@btime levendist1(s, t)
```


{{out}}

```txt

# levendist(kitten, sitting)
 - Recursive:
  78.788 μs (1103 allocations: 34.47 KiB)
 - Iterative:
  494.376 ns (9 allocations: 1.16 KiB)

# levendist(rosettacode, raisethysword)
 - Recursive:
  317.817 ms (3468524 allocations: 105.85 MiB)
 - Iterative:
  1.168 μs (15 allocations: 2.44 KiB)
```



## Kotlin


### Standard Version


```scala
// version 1.0.6

// Uses the "iterative with two matrix rows" algorithm referred to in the Wikipedia article.

fun levenshtein(s: String, t: String): Int {
    // degenerate cases
    if (s == t)  return 0
    if (s == "") return t.length
    if (t == "") return s.length

    // create two integer arrays of distances and initialize the first one
    val v0 = IntArray(t.length + 1) { it }  // previous
    val v1 = IntArray(t.length + 1)         // current

    var cost: Int
    for (i in 0 until s.length) {
        // calculate v1 from v0
        v1[0] = i + 1
        for (j in 0 until t.length) {
            cost = if (s[i] == t[j]) 0 else 1
            v1[j + 1] = Math.min(v1[j] + 1, Math.min(v0[j + 1] + 1, v0[j] + cost))
        }
        // copy v1 to v0 for next iteration
        for (j in 0 .. t.length) v0[j] = v1[j]
    }
    return v1[t.length]
}

fun main(args: Array<String>) {
    println("'kitten' to 'sitting'            => ${levenshtein("kitten", "sitting")}")
    println("'rosettacode' to 'raisethysword' => ${levenshtein("rosettacode", "raisethysword")}")
    println("'sleep' to 'fleeting'            => ${levenshtein("sleep", "fleeting")}")
}
```


{{out}}

```txt

'kitten' to 'sitting'            => 3
'rosettacode' to 'raisethysword' => 8
'sleep' to 'fleeting'            => 5

```



### Functional/Folding Version


```scala

fun levenshtein(s: String, t: String,
                charScore : (Char, Char) -> Int = { c1, c2 -> if (c1 == c2) 0 else 1}) : Int {

    // Special cases
    if (s == t)  return 0
    if (s == "") return t.length
    if (t == "") return s.length

    val initialRow : List<Int> = (0 until t.length + 1).map { it }.toList()
    return (0 until s.length).fold(initialRow, { previous, u ->
        (0 until t.length).fold( mutableListOf(u+1), {
            row, v -> row.add(minOf(row.last() + 1,
                    previous[v+1] + 1,
                    previous[v] + charScore(s[u],t[v])))
            row
        })
    }).last()

}

```

{{out}}

```txt

'kitten' to 'sitting'            => 3
'rosettacode' to 'raisethysword' => 8
'sleep' to 'fleeting'            => 5

```



## LFE



###  Simple Implementation


Suitable for short strings:


```lisp

(defun levenshtein-simple
  (('() str)
    (length str))
  ((str '())
    (length str))
  (((cons a str1) (cons b str2)) (when (== a b))
    (levenshtein-simple str1 str2))
  (((= (cons _ str1-tail) str1) (= (cons _ str2-tail) str2))
    (+ 1 (lists:min
          (list
           (levenshtein-simple str1 str2-tail)
           (levenshtein-simple str1-tail str2)
           (levenshtein-simple str1-tail str2-tail))))))

```


You can copy and paste that function into an LFE REPL and run it like so:


```lisp

> (levenshtein-simple "a" "a")
0
> (levenshtein-simple "a" "")
1
> (levenshtein-simple "" "a")
1
> (levenshtein-simple "kitten" "sitting")
3

```


It is not recommended to test strings longer than the last example using this implementation, as performance quickly degrades.


###  Cached Implementation



```lisp

(defun levenshtein-distance (str1 str2)
  (let (((tuple distance _) (levenshtein-distance
                               str1 str2 (dict:new))))
    distance))

(defun levenshtein-distance
  (((= '() str1) str2 cache)
    (tuple (length str2)
           (dict:store (tuple str1 str2)
                       (length str2)
                       cache)))
  ((str1 (= '() str2) cache)
    (tuple (length str1)
           (dict:store (tuple str1 str2)
                       (length str1)
                       cache)))
  (((cons a str1) (cons b str2) cache) (when (== a b))
    (levenshtein-distance str1 str2 cache))
  (((= (cons _ str1-tail) str1) (= (cons _ str2-tail) str2) cache)
     (case (dict:is_key (tuple str1 str2) cache)
       ('true (tuple (dict:fetch (tuple str1 str2) cache) cache))
       ('false (let* (((tuple l1 c1) (levenshtein-distance str1 str2-tail cache))
                      ((tuple l2 c2) (levenshtein-distance str1-tail str2 c1))
                      ((tuple l3 c3) (levenshtein-distance str1-tail str2-tail c2))
                      (len (+ 1 (lists:min (list l1 l2 l3)))))
                 (tuple len (dict:store (tuple str1 str2) len c3)))))))

```


As before, here's some usage in the REPL. Note that longer strings are now possible to compare without incurring long execution times:


```lisp

> (levenshtein-distance "a" "a")
0
> (levenshtein-distance "a" "")
1
> (levenshtein-distance "" "a")
1
> (levenshtein-distance "kitten" "sitting")
3
> (levenshtein-distance "rosettacode" "raisethysword")
8

```



## Liberty BASIC


```lb
'Levenshtein Distance translated by Brandon Parker
'08/19/10
'from http://www.merriampark.com/ld.htm#VB
'No credit was given to the Visual Basic Author on the site :-(

Print LevenshteinDistance("kitten", "sitting")
End

'Get the minum of three values
Function Minimum(a, b, c)
    Minimum = Min(a, Min(b, c))
End Function

'Compute the Levenshtein Distance
Function LevenshteinDistance(string1$, string2$)
    n = Len(string1$)
    m = Len(string2$)
    If n = 0 Then
        LevenshteinDistance = m
        Exit Function
    End If
    If m = 0 Then
        LevenshteinDistance = n
        Exit Function
    End If
    Dim d(n, m)
    For i = 0 To n
        d(i, 0) = i
    Next i
    For i = 0 To m
        d(0, i) = i
    Next i
    For i = 1 To n
        si$ = Mid$(string1$, i, 1)
        For ii = 1 To m
            tj$ = Mid$(string2$, ii, 1)
            If si$ = tj$ Then
                cost = 0
            Else
                cost = 1
            End If
            d(i, ii) = Minimum((d(i - 1, ii) + 1), (d(i, ii - 1) + 1), (d(i - 1, ii - 1) + cost))
        Next ii
    Next i
    LevenshteinDistance = d(n, m)
End Function
```



## Limbo

{{trans|Go}}

```limbo
implement Levenshtein;

include "sys.m"; sys: Sys;
	print: import sys;
include "draw.m";


Levenshtein: module {
	init: fn(nil: ref Draw->Context, args: list of string);
	# Export distance so that this module can be used as either a
	# standalone program or as a library:
	distance: fn(s, t: string): int;
};

init(nil: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	if(!(len args % 2)) {
		sys->fprint(sys->fildes(2), "Provide an even number of arguments!\n");
		raise "fail:usage";
	}
	args = tl args;

	while(args != nil) {
		(s, t) := (hd args, hd tl args);
		args = tl tl args;
		print("%s <-> %s => %d\n", s, t, distance(s, t));
	}
}

distance(s, t: string): int
{
	if(s == "")
		return len t;
	if(t == "")
		return len s;
	if(s[0] == t[0])
		return distance(s[1:], t[1:]);
	a := distance(s[1:], t);
	b := distance(s, t[1:]);
	c := distance(s[1:], t[1:]);
	if(a > b)
		a = b;
	if(a > c)
		a = c;
	return a + 1;
}

```


{{output}}
<code>
 % levenshtein kitten sitting rosettacode raisethysword
 kitten <-> sitting => 3
 rosettacode <-> raisethysword => 8
</code>


## LiveCode

{{trans|Go}}

```LiveCode

//Code By Neurox66
function Levenshtein pString1 pString2
   put 0 into tPosChar1
   repeat for each char tChar1 in pString1
      add 1 to tPosChar1
      put tPosChar1 into tDistance[tPosChar1][0]
      put 0 into tPosChar2
      repeat for each char tChar2 in pString2
         add 1 to tPosChar2
         put tPosChar2 into tDistance[0][tPosChar2]
         put 1 into tCost
         if tChar1 = tChar2 then
            put 0 into tCost
         end if
         put min((tDistance[tPosChar1-1][tPosChar2] + 1),(tDistance[tPosChar1][tPosChar2-1] + 1),(tDistance[tPosChar1-1][tPosChar2-1] + tCost)) into tDistance[tPosChar1][tPosChar2]
      end repeat
   end repeat
   return tDistance[tPosChar1][tPosChar2]
end Levenshtein


put Levenshtein("kitten","sitting")
put Levenshtein("rosettacode","raisethysword")

```

{{out}}

```txt
3
8
```



## Lua


```lua
function leven(s,t)
    if s == '' then return t:len() end
    if t == '' then return s:len() end

    local s1 = s:sub(2, -1)
    local t1 = t:sub(2, -1)

    if s:sub(0, 1) == t:sub(0, 1) then
        return leven(s1, t1)
    end

    return 1 + math.min(
        leven(s1, t1),
        leven(s,  t1),
        leven(s1, t )
      )
end

print(leven("kitten", "sitting"))
print(leven("rosettacode", "raisethysword"))
```

{{out}}

```txt
3
8
```


## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
	\\ Iterative with two matrix rows
	function LevenshteinDistance(s$,t$) {
		if len(s$)<len(t$) then swap s$, t$
		n=len(t$)
		m=len(s$)
		dim base 0, v0(n+1), v1(n+1)
Rem		dim sw()  ' we can use stack of values to make the swap.
		for i=0 to n : v0(i)=i:next
		for i=0 to m-1
			v1(0)=i+1
			for j=0 to n-1
				deletioncost=v0(j+1)+1
				insertioncost=v1(j)+1
				if mid$(s$,i+1,1)=mid$(t$,j+1,1) then
					substitutionCost=v0(j)
				else
					substitutionCost=v0(j)+1
				end if
				v1(j+1)=min.data(deletionCost, insertionCost, substitutionCost)
			next
Rem			sw()=v0():v0()=v1():v1()=sw()
			\\ when we push arrays, we only push a pointer to
			\\ when we read array (identifier with parenthesis) then we get a copy
			\\ between Push and Read any change on arrays included in copies
			Push v0(),v1(): Read v0(),v1()
		next
		=v0(n)
	}
	Print LevenshteinDistance("kitten","sitting")=3  ' true
	Print LevenshteinDistance("Sunday","Saturday")=3  ' true
	Print LevenshteinDistance("rosettacode","raisethysword")=8  ' true
}
Checkit

Module Checkit2 {
	\\ Iterative with two matrix rows, using pointers to arrays
	function LevenshteinDistance(s$,t$) {
		if len(s$)<len(t$) then swap s$, t$
		n=len(t$)
		m=len(s$)
		dim base 0, v0(n+1), v1(n+1)
		v0=v0()  ' v0 is pointer to v0()
		v1=v1() ' v1 is pointer to v1()
		for i=0 to n : v0(i)=i:next
		for i=0 to m-1
			return v1, 0:=i+1
			for j=0 to n-1
				deletioncost=Array(v0,j+1)+1
				insertioncost=Array(v1,j)+1
				if mid$(s$,i+1,1)=mid$(t$,j+1,1) then
					substitutionCost=Array(v0,j)
				else
					substitutionCost=Array(v0,j)+1
				end if
				return v1, j+1:=min.data(deletionCost, insertionCost, substitutionCost)
			next
			swap v0, v1  ' just swap pointers
		next
		=Array(v0,n)
	}
	Print LevenshteinDistance("kitten","sitting")=3
	Print LevenshteinDistance("Sunday","Saturday")=3
	Print LevenshteinDistance("rosettacode","raisethysword")=8
}
Checkit2

```



## Maple


```Maple

> with(StringTools):
> Levenshtein("kitten","sitting");
                                   3

> Levenshtein("rosettacode","raisethysword");
                                   8

```




## Mathematica


```Mathematica
EditDistance["kitten","sitting"]
->3
EditDistance["rosettacode","raisethysword"]
->8
```


## MATLAB


```MATLAB

function score = levenshtein(s1, s2)
% score = levenshtein(s1, s2)
%
% Calculates the area under the ROC for a given set
% of posterior predictions and labels. Currently limited to two classes.
%
% s1: string
% s2: string
% score: levenshtein distance
%
% Author: Ben Hamner (ben@benhamner.com)
if length(s1) < length(s2)
score = levenshtein(s2, s1);
elseif isempty(s2)
score = length(s1);
else
previous_row = 0:length(s2);
for i=1:length(s1)
current_row = 0*previous_row;
current_row(1) = i;
for j=1:length(s2)
insertions = previous_row(j+1) + 1;
deletions = current_row(j) + 1;
substitutions = previous_row(j) + (s1(i) ~= s2(j));
current_row(j+1) = min([insertions, deletions, substitutions]);
end
previous_row = current_row;
end
score = current_row(end);
end

```

Source : [https://github.com/benhamner/Metrics/blob/master/MATLAB/metrics/levenshtein.m]


## NetRexx

{{trans|ooRexx}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

n = 0
w = ''
n = n + 1; w[0] = n; w[n] = "kitten sitting"
n = n + 1; w[0] = n; w[n] = "rosettacode raisethysword"

loop n = 1 to w[0]
  say w[n].word(1) "->" w[n].word(2)":" levenshteinDistance(w[n].word(1), w[n].word(2))
  end n
return

method levenshteinDistance(s, t) private static
  s = s.lower
  t = t.lower

  m = s.length
  n = t.length

  -- for all i and j, d[i,j] will hold the Levenshtein distance between
  -- the first i characters of s and the first j characters of t;
  -- note that d has (m+1)x(n+1) values
  d = 0

  -- source prefixes can be transformed into empty string by
  -- dropping all characters (Note, ooRexx arrays are 1-based)
  loop i = 2 to m + 1
    d[i, 1] = 1
  end i

  -- target prefixes can be reached from empty source prefix
  -- by inserting every characters
  loop j = 2 to n + 1
    d[1, j] = 1
  end j

  loop j = 2 to n + 1
    loop i = 2 to m + 1
      if s.substr(i - 1, 1) == t.substr(j - 1, 1) then do
        d[i, j] = d[i - 1, j - 1]   -- no operation required
        end
      else do
        d[i, j] =                 -
          (d[i - 1, j] + 1).min(  - -- a deletion
          (d[i, j - 1] + 1)).min( - -- an insertion
          (d[i - 1, j - 1] + 1))    -- a substitution
        end
    end i
  end j

  return d[m + 1, n + 1]

```

'''Output:'''

```txt

kitten -> sitting: 3
rosettacode -> raisethysword: 8

```



## Nim


```nim
import strutils

echo editDistance("kitten","sitting")
echo editDistance("rosettacode","raisethysword")
```


{{out}}

```txt
3
8
```


{{trans|Python}}

```nim
import sequtils

proc levenshteinDistance(s1, s2): int =
  var (s1, s2) = (s1, s2)

  if s1.len > s2.len:
    swap s1, s2

  var distances = toSeq(0..s1.len)

  for i2, c2 in s2:
    var newDistances = @[i2+1]
    for i1, c1 in s1:
      if c1 == c2:
        newDistances.add(distances[i1])
      else:
        newDistances.add(1 + min(distances[i1], distances[i1+1],
                                 newDistances[newDistances.high]))

    distances = newDistances
  result = distances[distances.high]

echo levenshteinDistance("kitten","sitting")
echo levenshteinDistance("rosettacode","raisethysword")
```



## Objeck

{{trans|C#}}

```objeck
class Levenshtein {
  function : Main(args : String[]) ~ Nil {
    if(args->Size() = 2) {
      s := args[0]; t := args[1]; d := Distance(s,t);
      "{$s} -> {$t} = {$d}"->PrintLine();
    };
  }

  function : native : Distance(s : String,t : String) ~ Int {
    d := Int->New[s->Size() + 1, t->Size() + 1];
    for(i := 0; i <= s->Size(); i += 1;) {
      d[i,0] := i;
    };

    for(j := 0; j <= t->Size(); j += 1;) {
      d[0,j] := j;
    };

    for(j := 1; j <= t->Size(); j += 1;) {
      for(i := 1; i <= s->Size(); i += 1;) {
        if(s->Get(i - 1) = t->Get(j - 1)) {
          d[i,j] := d[i - 1, j - 1];
        }
        else {
          d[i,j] := (d[i - 1, j] + 1)
            ->Min(d[i, j - 1] + 1)
            ->Min(d[i - 1, j - 1] + 1);
        };
      };
    };

    return d[s->Size(), t->Size()];
  }
}
```


=={{header|Objective-C}}==
Translation of the C# code into a NSString category

```objc
@interface NSString (levenshteinDistance)
- (NSUInteger)levenshteinDistanceToString:(NSString *)string;
@end

@implementation NSString (levenshteinDistance)
- (NSUInteger)levenshteinDistanceToString:(NSString *)string {
    NSUInteger sl = [self length];
    NSUInteger tl = [string length];
    NSUInteger *d = calloc(sizeof(*d), (sl+1) * (tl+1));

#define d(i, j) d[((j) * sl) + (i)]
    for (NSUInteger i = 0; i <= sl; i++) {
        d(i, 0) = i;
    }
    for (NSUInteger j = 0; j <= tl; j++) {
        d(0, j) = j;
    }
    for (NSUInteger j = 1; j <= tl; j++) {
        for (NSUInteger i = 1; i <= sl; i++) {
            if ([self characterAtIndex:i-1] == [string characterAtIndex:j-1]) {
                d(i, j) = d(i-1, j-1);
            } else {
                d(i, j) = MIN(d(i-1, j), MIN(d(i, j-1), d(i-1, j-1))) + 1;
            }
        }
    }

    NSUInteger r = d(sl, tl);
#undef d

    free(d);

    return r;
}
@end
```



## OCaml

Translation of the pseudo-code of the Wikipedia article:

```ocaml
let minimum a b c =
  min a (min b c)

let levenshtein_distance s t =
  let m = String.length s
  and n = String.length t in
  (* for all i and j, d.(i).(j) will hold the Levenshtein distance between
     the first i characters of s and the first j characters of t *)
  let d = Array.make_matrix (m+1) (n+1) 0 in

  for i = 0 to m do
    d.(i).(0) <- i  (* the distance of any first string to an empty second string *)
  done;
  for j = 0 to n do
    d.(0).(j) <- j  (* the distance of any second string to an empty first string *)
  done;

  for j = 1 to n do
    for i = 1 to m do

      if s.[i-1] = t.[j-1] then
        d.(i).(j) <- d.(i-1).(j-1)  (* no operation required *)
      else
        d.(i).(j) <- minimum
                       (d.(i-1).(j) + 1)   (* a deletion *)
                       (d.(i).(j-1) + 1)   (* an insertion *)
                       (d.(i-1).(j-1) + 1) (* a substitution *)
    done;
  done;

  d.(m).(n)
;;

let test s t =
  Printf.printf " %s -> %s = %d\n" s t (levenshtein_distance s t);
;;

let () =
  test "kitten" "sitting";
  test "rosettacode" "raisethysword";
;;
```


###  A recursive functional version

This could be made faster with memoization

```OCaml
let levenshtein s t =
   let rec dist i j = match (i,j) with
      | (i,0) -> i
      | (0,j) -> j
      | (i,j) ->
         if s.[i-1] = t.[j-1] then dist (i-1) (j-1)
         else let d1, d2, d3 = dist (i-1) j, dist i (j-1), dist (i-1) (j-1) in
         1 + min d1 (min d2 d3)
   in
   dist (String.length s) (String.length t)

let test s t =
  Printf.printf " %s -> %s = %d\n" s t (levenshtein s t)

let () =
  test "kitten" "sitting";
  test "rosettacode" "raisethysword";
```

{{out}}

```txt

 kitten -> sitting = 3
 rosettacode -> raisethysword = 8

```



## ooRexx


```ooRexx

say "kitten -> sitting:" levenshteinDistance("kitten", "sitting")
say "rosettacode -> raisethysword:" levenshteinDistance("rosettacode", "raisethysword")

::routine levenshteinDistance
  use arg s, t
  s = s~lower
  t = t~lower

  m = s~length
  n = t~length

  -- for all i and j, d[i,j] will hold the Levenshtein distance between
  -- the first i characters of s and the first j characters of t;
  -- note that d has (m+1)x(n+1) values
  d = .array~new(m + 1, n + 1)

  -- clear all elements in d
  loop i = 1 to d~dimension(1)
      loop j = 1 to d~dimension(2)
          d[i, j] = 0
      end
  end

  -- source prefixes can be transformed into empty string by
  -- dropping all characters (Note, ooRexx arrays are 1-based)
  loop i = 2 to m + 1
      d[i, 1] = 1
  end

  -- target prefixes can be reached from empty source prefix
  -- by inserting every characters
  loop j = 2 to n + 1
      d[1, j] = 1
  end

  loop j = 2 to n + 1
      loop i = 2 to m + 1
          if s~subchar(i - 1) == t~subchar(j - 1) then
              d[i, j] = d[i - 1, j - 1]   -- no operation required
          else d[i, j] = min(d[i - 1, j] + 1,    - -- a deletion
                             d[i, j-1] + 1,      - -- an insertion
                             d[i - 1, j - 1] + 1)  -- a substitution
      end
  end

  return d[m + 1, n + 1 ]

```

Output:

```txt

kitten -> sitting: 3
rosettacode -> raisethysword: 8

```



## PARI/GP

{{trans|JavaScript}}
{{Works with|PARI/GP|2.7.4 and above}}

```parigp

\\ Levenshtein distance between two words
\\ 6/21/16 aev
levensDist(s1,s2)={
my(n1=#s1,n2=#s2,v1=Vecsmall(s1),v2=Vecsmall(s2),c,
   n11=n1+1,n21=n2+1,t=vector(n21,z,z-1),u0=vector(n21),u=u0);
if(s1==s2, return(0)); if(!n1, return(n2)); if(!n2, return(n1));
for(i=2,n11, u=u0; u[1]=i-1;
  for(j=2,n21,
    if(v1[i-1]==v2[j-1], c=t[j-1], c=vecmin([t[j-1],t[j],u[j-1]])+1);
    u[j]=c;
  );\\fend j
  t=u;
);\\fend i
print(" *** Levenshtein distance = ",t[n21]," for strings: ",s1,", ",s2);
return(t[n21]);
}
{ \\ Testing:
levensDist("kitten","sitting");
levensDist("rosettacode","raisethysword");
levensDist("Saturday","Sunday");
levensDist("oX","X");
levensDist("X","oX");
}

```


{{Output}}


```txt

 *** Levenshtein distance = 3 for strings: kitten, sitting
 *** Levenshtein distance = 8 for strings: rosettacode, raisethysword
 *** Levenshtein distance = 3 for strings: Saturday, Sunday
 *** Levenshtein distance = 1 for strings: oX, X
 *** Levenshtein distance = 1 for strings: X, oX

```



## Pascal

A fairly direct translation of the wikipedia pseudo code:

```pascal
Program LevenshteinDistanceDemo(output);

uses
  Math;

function LevenshteinDistance(s, t: string): longint;
  var
    d: array of array of integer;
    i, j, n, m: integer;
  begin
    n := length(t);
    m := length(s);
    setlength(d, m+1, n+1);

    for i := 0 to m do
      d[i,0] := i;
    for j := 0 to n do
      d[0,j] := j;
    for j := 1 to n do
      for i := 1 to m do
        if s[i] = t[j] then
          d[i,j] := d[i-1,j-1]
        else
          d[i,j] := min(d[i-1,j] + 1, min(d[i,j-1] + 1, d[i-1,j-1] + 1));
    LevenshteinDistance := d[m,n];
  end;

var
  s1, s2: string;

begin
  s1 := 'kitten';
  s2 := 'sitting';
  writeln('The Levenshtein distance between "', s1, '" and "', s2, '" is: ', LevenshteinDistance(s1, s2));
  s1 := 'rosettacode';
  s2 := 'raisethysword';
  writeln('The Levenshtein distance between "', s1, '" and "', s2, '" is: ', LevenshteinDistance(s1, s2));
end.
```

{{out}}

```txt

The Levenshtein distance between "kitten" and "sitting" is: 3
The Levenshtein distance between "rosettacode" and "raisethysword" is: 8

```



## Perl

Recursive algorithm, as in the C sample. You are invited to comment out the line where it says so, and see the speed difference. By the way, there's the <code>Memoize</code> standard module, but it requires setting quite a few parameters to work right for this example, so I'm just showing the simple minded caching scheme here.

```Perl
use List::Util qw(min);

my %cache;

sub leven {
    my ($s, $t) = @_;

    return length($t) if $s eq '';
    return length($s) if $t eq '';

    $cache{$s}{$t} //=    # try commenting out this line
      do {
        my ($s1, $t1) = (substr($s, 1), substr($t, 1));

        (substr($s, 0, 1) eq substr($t, 0, 1))
          ? leven($s1, $t1)
          : 1 + min(
                    leven($s1, $t1),
                    leven($s,  $t1),
                    leven($s1, $t ),
            );
      };
}

print leven('rosettacode', 'raisethysword'), "\n";
```


Iterative solution:


```perl
use List::Util qw(min);

sub leven {
    my ($s, $t) = @_;

    my $tl = length($t);
    my $sl = length($s);

    my @d = ([0 .. $tl], map { [$_] } 1 .. $sl);

    foreach my $i (0 .. $sl - 1) {
        foreach my $j (0 .. $tl - 1) {
            $d[$i + 1][$j + 1] =
              substr($s, $i, 1) eq substr($t, $j, 1)
              ? $d[$i][$j]
              : 1 + min($d[$i][$j + 1], $d[$i + 1][$j], $d[$i][$j]);
        }
    }

    $d[-1][-1];
}

print leven('rosettacode', 'raisethysword'), "\n";
```



## Perl 6

{{works with|rakudo|2015-09-16}}
Implementation of the wikipedia algorithm. Since column 0 and row 0 are used for base distances, the original algorithm would require us to compare "@s[$i-1] eq @t[$j-1]", and reference the $m and $n separately. Prepending an unused value (undef) onto @s and @t makes their indices align with the $i,$j numbering of @d, and lets us use .end instead of $m,$n.

```perl6
sub levenshtein_distance ( Str $s, Str $t --> Int ) {
    my @s = *, |$s.comb;
    my @t = *, |$t.comb;

    my @d;
    @d[$_;  0] = $_ for ^@s.end;
    @d[ 0; $_] = $_ for ^@t.end;

    for 1..@s.end X 1..@t.end -> ($i, $j) {
        @d[$i; $j] = @s[$i] eq @t[$j]
            ??   @d[$i-1; $j-1]    # No operation required when eq
            !! ( @d[$i-1; $j  ],   # Deletion
                 @d[$i  ; $j-1],   # Insertion
                 @d[$i-1; $j-1],   # Substitution
               ).min + 1;
    }

    return @d[*-1][*-1];
}

my @a = [<kitten sitting>], [<saturday sunday>], [<rosettacode raisethysword>];

for @a -> [$s, $t] {
    say "levenshtein_distance('$s', '$t') == ", levenshtein_distance($s, $t);
}
```

{{out}}

```txt
levenshtein_distance('kitten', 'sitting') == 3
levenshtein_distance('saturday', 'sunday') == 3
levenshtein_distance('rosettacode', 'raisethysword') == 8
```



## Phix

Copy of [[Levenshtein_distance#Euphoria|Euphoria]]

```Phix
function levenshtein(sequence s1, sequence s2)
integer n = length(s1)+1,
        m = length(s2)+1
sequence d

    if n=1  then
        return m-1
    elsif m=1 then
        return n-1
    end if

    d = repeat(repeat(0, m), n)
    for i=1 to n do
        d[i][1] = i-1
    end for

    for j=1 to m do
        d[1][j] = j-1
    end for

    for i=2 to n do
        for j=2 to m do
            d[i][j] = min({
                           d[i-1][j]+1,
                           d[i][j-1]+1,
                           d[i-1][j-1]+(s1[i-1]!=s2[j-1])
                          })
        end for
    end for

    return d[n][m]
end function

?levenshtein("kitten", "sitting")
?levenshtein("rosettacode", "raisethysword")
```

{{out}}

```txt

3
8

```



## PHP


```PHP

echo levenshtein('kitten','sitting');
echo levenshtein('rosettacode', 'raisethysword');

```


{{out}}

```txt
3
8
```



## PicoLisp

Translation of the pseudo-code in the Wikipedia article:

```PicoLisp
(de levenshtein (A B)
   (let D
      (cons
         (range 0 (length A))
         (mapcar
            '((I) (cons I (copy A)))
            (range 1 (length B)) ) )
      (for (J . Y) B
         (for (I . X) A
            (set
               (nth D (inc J) (inc I))
               (if (= X Y)
                  (get D J I)
                  (inc
                     (min
                        (get D J (inc I))
                        (get D (inc J) I)
                        (get D J I) ) ) ) ) ) ) ) )
```

or, using 'map' to avoid list indexing:

```PicoLisp
(de levenshtein (A B)
   (let D
      (cons
         (range 0 (length A))
         (mapcar
            '((I) (cons I (copy A)))
            (range 1 (length B)) ) )
      (map
         '((B Y)
            (map
               '((A X P)
                  (set (cdr P)
                     (if (= (car A) (car B))
                        (car X)
                        (inc (min (cadr X) (car P) (car X))) ) ) )
               A
               (car Y)
               (cadr Y) ) )
         B
         D ) ) )
```

{{out|Output (both cases)}}

```txt
: (levenshtein (chop "kitten") (chop "sitting"))
-> 3
```



## PL/I


### version 1


```pli
*process source xref attributes or(!);
 lsht: Proc Options(main);
 Call test('kitten'      ,'sitting');
 Call test('rosettacode' ,'raisethysword');
 Call test('Sunday'      ,'Saturday');
 Call test('Vladimir_Levenshtein[1965]',
           'Vladimir_Levenshtein[1965]');
 Call test('this_algorithm_is_similar_to',
            'Damerau-Levenshtein_distance');
 Call test('','abc');

 test: Proc(s,t);
 Dcl (s,t) Char(*) Var;
 Put Edit('          1st string  = >'!!s!!'<')(Skip,a);
 Put Edit('          2nd string  = >'!!t!!'<')(Skip,a);
 Put Edit('Levenshtein distance  =',LevenshteinDistance(s,t))
         (Skip,a,f(3));
 Put Edit('')(Skip,a);
 End;

 LevenshteinDistance: Proc(s,t) Returns(Bin Fixed(31));
 Dcl (s,t) Char(*) Var;
 Dcl (sl,tl) Bin Fixed(31);
 Dcl ld      Bin Fixed(31);
 /* for all i and j, d[i,j] will hold the Levenshtein distance between
 *  the first i characters of s and the first j characters of t;
 *  note that d has (m+1)*(n+1) values                               */
 sl=length(s);
 tl=length(t);
 Begin;
   Dcl d(0:sl,0:tl) Bin Fixed(31);
   Dcl (i,j,ii,jj)  Bin Fixed(31);
   d=0;
   Do i=1 To sl;  /* source prefixes can be transformed into         */
     d(i,0)=i;    /* empty string by dropping all characters         */
     End;
   Do j=1 To tl;  /* target prefixes can be reached from             */
     d(0,j)=j;    /* empty source prefix by inserting every character*/
     End;
   Do j=1 To tl;
     jj=j-1;
     Do i=1 To sl;
       ii=i-1;
       If substr(s,i,1)=substr(t,j,1) Then
         d(i,j)=d(ii,jj);                  /* no operation required  */
       Else
         d(i,j)=1+min(d(ii,j),             /* a deletion             */
                      d(i,jj),             /* an insertion           */
                      d(ii,jj));           /* a substitution         */
       End;
     End;
   ld=d(sl,tl);
   End;
 Return(ld);
 End;
 End;
```

{{out}}

```txt
          1st string  = >kitten<
          2nd string  = >sitting<
Levenshtein distance  =  3

          1st string  = >rosettacode<
          2nd string  = >raisethysword<
Levenshtein distance  =  8

          1st string  = >Sunday<
          2nd string  = >Saturday<
Levenshtein distance  =  3

          1st string  = >Vladimir_Levenshtein[1965]<
          2nd string  = >Vladimir_Levenshtein[1965]<
Levenshtein distance  =  0

          1st string  = >this_algorithm_is_similar_to<
          2nd string  = >Damerau-Levenshtein_distance<
Levenshtein distance  = 24

          1st string  = ><
          2nd string  = >abc<
Levenshtein distance  =  3
```


### version 2 recursive with memoization


```pli
*process source attributes xref or(!);
 ld3: Proc Options(main);
 Dcl ld(0:30,0:30) Bin Fixed(31);
 call test('kitten'      ,'sitting');
 call test('rosettacode' ,'raisethysword');
 call test('Sunday'      ,'Saturday');
 call test('Vladimir_Levenshtein[1965]',
           'Vladimir_Levenshtein[1965]');
 call test('this_algorithm_is_similar_to',
           'Damerau-Levenshtein_distance');
 call test('','abc');

 test: Proc(s,t);
 Dcl (s,t) Char(*);
 ld=-1;
 Put Edit('          1st string  = >'!!s!!'<')(Skip,a);
 Put Edit('          2nd string  = >'!!t!!'<')(Skip,a);
 Put Edit('Levenshtein distance  =',
          LevenshteinDistance(s,length(s),t,length(t)))
         (Skip,a,f(3));
 Put Edit('')(Skip,a);
 End;

 LevenshteinDistance: Proc(s,sl,t,tl) Recursive Returns(Bin Fixed(31));
 Dcl (s,t) Char(*);
 Dcl (sl,tl) Bin Fixed(31);
 Dcl cost    Bin Fixed(31);
 If ld(sl,tl)^=-1 Then
   Return(ld(sl,tl));
 Select;
   When(sl=0) ld(sl,tl)=tl;
   When(tl=0) ld(sl,tl)=sl;
   Otherwise Do;
     /* test if last characters of the strings match */
     cost=(substr(s,sl,1)^=substr(t,tl,1));
     /* return minimum of delete char from s, delete char from t,
        and delete char from both */
     ld(sl,tl)=min(LevenshteinDistance(s,sl-1,t,tl  )+1,
                   LevenshteinDistance(s,sl  ,t,tl-1)+1,
                   LevenshteinDistance(s,sl-1,t,tl-1)+cost));
     End;
   End;
 Return(ld(sl,tl));
 End;
 End;
```

Output is the same as for version 1.


## PowerShell

This version does not allow empty strings.

```PowerShell

function Get-LevenshteinDistance
{
    [CmdletBinding()]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$true, Position=0)]
        [ValidateNotNullOrEmpty()]
        [Alias("s")]
        [string]
        $ReferenceObject,

        [Parameter(Mandatory=$true, Position=1)]
        [ValidateNotNullOrEmpty()]
        [Alias("t")]
        [string]
        $DifferenceObject
    )

    [int]$n = $ReferenceObject.Length
    [int]$m = $DifferenceObject.Length

    $d = New-Object -TypeName 'System.Object[,]' -ArgumentList ($n + 1),($m + 1)

    $outputObject = [PSCustomObject]@{
        ReferenceObject  = $ReferenceObject
        DifferenceObject = $DifferenceObject
        Distance         = $null
    }

    for ($i = 0; $i -le $n; $i++)
    {
        $d[$i, 0] = $i
    }

    for ($i = 0; $i -le $m; $i++)
    {
        $d[0, $i] = $i
    }

    for ($i = 1; $i -le $m; $i++)
    {
        for ($j = 1; $j -le $n; $j++)
        {
            if ($ReferenceObject[$j - 1] -eq $DifferenceObject[$i - 1])
            {
                $d[$j, $i] = $d[($j - 1), ($i - 1)]
            }
            else
            {
                $d[$j, $i] = [Math]::Min([Math]::Min(($d[($j - 1), $i] + 1), ($d[$j, ($i - 1)] + 1)), ($d[($j - 1), ($i - 1)] + 1))
            }
        }
    }

    $outputObject.Distance = $d[$n, $m]

    $outputObject
}

```


```PowerShell

Get-LevenshteinDistance "kitten" "sitting"
Get-LevenshteinDistance rosettacode raisethysword

```

{{Out}}

```txt

ReferenceObject DifferenceObject Distance
--------------- ---------------- --------
kitten          sitting                 3
rosettacode     raisethysword           8

```




## Processing


```processing
void setup() {
  println(distance("kitten", "sitting"));
}
int distance(String a, String b) {
  int [] costs = new int [b.length() + 1];
  for (int j = 0; j < costs.length; j++)
    costs[j] = j;
  for (int i = 1; i <= a.length(); i++) {
    costs[0] = i;
    int nw = i - 1;
    for (int j = 1; j <= b.length(); j++) {
      int cj = min(1 + min(costs[j], costs[j - 1]), a.charAt(i - 1) == b.charAt(j - 1) ? nw : nw + 1);
      nw = costs[j];
      costs[j] = cj;
    }
  }
  return costs[b.length()];
}
```




## Prolog

Works with SWI-Prolog.

Based on Wikipedia's pseudocode.

```Prolog
levenshtein(S, T, R) :-
	length(S, M),
	M1 is M+1,
	length(T, N),
	N1 is N+1,
	length(Lev, N1),
	maplist(init(M1), Lev),
	numlist(0, N, LN),
	maplist(init_n, LN, Lev),
	nth0(0, Lev, Lev0),
	numlist(0, M, Lev0),

	% compute_levenshtein distance
	numlist(1, N, LN1),
	maplist(work_on_T(Lev, S), LN1, T),
	last(Lev, LevLast),
	last(LevLast, R).


work_on_T(Lev, S, J, TJ) :-
	length(S, M),
	numlist(1, M, LM),
	maplist(work_on_S(Lev, J, TJ), LM, S).

work_on_S(Lev, J, C, I, C) :-
	% same char
	!,
	I1 is I-1, J1 is J-1,
	nth0(J1, Lev, LevJ1),
	nth0(I1, LevJ1, V),
	nth0(J, Lev, LevJ),
	nth0(I, LevJ, V).


work_on_S(Lev, J, _C1, I, _C2) :-
	I1 is I-1, J1 is J - 1,
	% compute the value for deletion
	nth0(J, Lev, LevJ),
	nth0(I1, LevJ, VD0),
	VD is VD0 + 1,

	% compute the value for insertion
	nth0(J1, Lev, LevJ1),
	nth0(I, LevJ1, VI0),
	VI is VI0 + 1,

	% compute the value for substitution
	nth0(I1, LevJ1, VS0),
	VS is VS0 + 1,

	% set the minimum value to cell(I,J)
	sort([VD, VI, VS], [V|_]),

	nth0(I, LevJ, V).


init(Len, C) :-
	length(C, Len).

init_n(N, L) :-
	nth0(0, L, N).
```

{{out|Output examples}}

```txt
 ?- levenshtein("kitten", "sitting", R).
R = 3.

 ?- levenshtein("saturday", "sunday", R).
R = 3.

 ?- levenshtein("rosettacode", "raisethysword", R).
R = 8.

```



## PureBasic

Based on Wikipedia's pseudocode.

```PureBasic
Procedure LevenshteinDistance(A_string$, B_String$)
  Protected m, n, i, j, min, k, l
  m = Len(A_string$)
  n = Len(B_String$)
  Dim D(m, n)

  For i=0 To m: D(i,0)=i: Next
  For j=0 To n: D(0,j)=j: Next

  For j=1 To n
    For i=1 To m
      If Mid(A_string$,i,1) = Mid(B_String$,j,1)
        D(i,j) = D(i-1, j-1); no operation required
      Else
        min = D(i-1, j)+1   ; a deletion
        k   = D(i, j-1)+1   ; an insertion
        l   = D(i-1, j-1)+1 ; a substitution
        If k < min: min=k: EndIf
        If l < min: min=l: EndIf
        D(i,j) = min
      EndIf
    Next
  Next
  ProcedureReturn D(m,n)
EndProcedure

;- Testing
n = LevenshteinDistance("kitten", "sitting")
MessageRequester("Info","Levenshtein Distance= "+Str(n))
```



## Python


### Iterative

Implementation of the wikipedia algorithm, optimized for memory

```python
def minimumEditDistance(s1,s2):
    if len(s1) > len(s2):
        s1,s2 = s2,s1
    distances = range(len(s1) + 1)
    for index2,char2 in enumerate(s2):
        newDistances = [index2+1]
        for index1,char1 in enumerate(s1):
            if char1 == char2:
                newDistances.append(distances[index1])
            else:
                newDistances.append(1 + min((distances[index1],
                                             distances[index1+1],
                                             newDistances[-1])))
        distances = newDistances
    return distances[-1]

print(minimumEditDistance("kitten","sitting"))
print(minimumEditDistance("rosettacode","raisethysword"))

```


{{out}}

```txt
3
8
```



```python
def levenshteinDistance(str1, str2):
    m = len(str1)
    n = len(str2)
    lensum = float(m + n)
    d = []
    for i in range(m+1):
        d.append([i])
    del d[0][0]
    for j in range(n+1):
        d[0].append(j)
    for j in range(1,n+1):
        for i in range(1,m+1):
            if str1[i-1] == str2[j-1]:
                d[i].insert(j,d[i-1][j-1])
            else:
                minimum = min(d[i-1][j]+1, d[i][j-1]+1, d[i-1][j-1]+2)
                d[i].insert(j, minimum)
    ldist = d[-1][-1]
    ratio = (lensum - ldist)/lensum
    return {'distance':ldist, 'ratio':ratio}

print(levenshteinDistance("kitten","sitting"))
print(levenshteinDistance("rosettacode","raisethysword"))


```


{{out}}

```txt
{'distance': 5, 'ratio': 0.6153846153846154}
{'distance': 12, 'ratio': 0.5}
```


===Iterative space optimized (even bounded) ===

```python

def ld(a, b, mx=-1):
    def result(d): return d if mx < 0 else False if d > mx else True

    if a == b: return result(0)
    la, lb = len(a), len(b)
    if mx >= 0 and abs(la - lb) > mx: return result(mx+1)
    if la == 0: return result(lb)
    if lb == 0: return result(la)
    if lb > la: a, b, la, lb = b, a, lb, la

    cost = array('i', range(lb + 1))
    for i in range(1, la + 1):
        cost[0] = i; ls = i-1; mn = ls
        for j in range(1, lb + 1):
            ls, act = cost[j], ls + int(a[i-1] != b[j-1])
            cost[j] = min(ls+1, cost[j-1]+1, act)
            if (ls < mn): mn = ls
        if mx >= 0 and mn > mx: return result(mx+1)
    if mx >= 0 and cost[lb] > mx: return result(mx+1)
    return result(cost[lb])

print(
    ld('kitten','kitten'), # 0
    ld('kitten','sitten'), # 1
    ld('kitten','sittes'), # 2
    ld('kitten','sityteng'), # 3
    ld('kitten','sittYing'), # 4
    ld('rosettacode','raisethysword'), # 8
    ld('kitten','kittenaaaaaaaaaaaaaaaaa'), # 17
    ld('kittenaaaaaaaaaaaaaaaaa','kitten') # 17
)

print(
    ld('kitten','kitten',3), # True
    ld('kitten','sitten',3), # True
    ld('kitten','sittes',3), # True
    ld('kitten','sityteng',3), # True
    ld('kitten','sittYing',3), # False
    ld('rosettacode','raisethysword',3), # False
    ld('kitten','kittenaaaaaaaaaaaaaaaaa',3), # False
    ld('kittenaaaaaaaaaaaaaaaaa','kitten',3) # False
)

```

{{out}}

```txt

0 1 2 3 4 8 17 17
True True True True False False False False

```



### Functional


### =Memoized recursion=

(Uses [http://docs.python.org/dev/library/functools.html?highlight=functools.lru_cache#functools.lru_cache this] cache from the standard library).

```python>>>
 from functools import lru_cache
>>> @lru_cache(maxsize=4095)
def ld(s, t):
	if not s: return len(t)
	if not t: return len(s)
	if s[0] == t[0]: return ld(s[1:], t[1:])
	l1 = ld(s, t[1:])
	l2 = ld(s[1:], t)
	l3 = ld(s[1:], t[1:])
	return 1 + min(l1, l2, l3)

>>> print( ld("kitten","sitting"),ld("rosettacode","raisethysword") )
3 8
```


====Non-recursive: reduce and scanl====
{{Works with|Python|3.7}}

```python
'''Levenshtein distance'''

from itertools import (accumulate, chain, islice)
from functools import reduce


# levenshtein :: String -> String -> Int
def levenshtein(sa):
    '''Levenshtein distance between
       two strings.'''
    s1 = list(sa)

    # go :: [Int] -> Char -> [Int]
    def go(ns, c):
        n, ns1 = ns[0], ns[1:]

        # gap :: Int -> (Char, Int, Int) -> Int
        def gap(z, c1xy):
            c1, x, y = c1xy
            return min(
                succ(y),
                succ(z),
                succ(x) if c != c1 else x
            )
        return scanl(gap)(succ(n))(
            zip(s1, ns, ns1)
        )
    return lambda sb: reduce(
        go, list(sb), enumFromTo(0)(len(s1))
    )[-1]


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Tests'''

    pairs = [
        ('rosettacode', 'raisethysword'),
        ('kitten', 'sitting'),
        ('saturday', 'sunday')
    ]

    print(
        tabulated(
            'Levenshtein minimum edit distances:\n'
        )(str)(str)(
            uncurry(levenshtein)
        )(concat(map(
            list,
            zip(pairs, map(swap, pairs))
        )))
    )


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# concat :: [[a]] -> [a]
# concat :: [String] -> String
def concat(xxs):
    '''The concatenation of all the elements in a list.'''
    xs = list(chain.from_iterable(xxs))
    unit = '' if isinstance(xs, str) else []
    return unit if not xs else (
        ''.join(xs) if isinstance(xs[0], str) else xs
    )


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# scanl :: (b -> a -> b) -> b -> [a] -> [b]
def scanl(f):
    '''scanl is like reduce, but returns a succession of
       intermediate values, building from the left.'''
    return lambda a: lambda xs: (
        list(accumulate(chain([a], xs), f))
    )


# swap :: (a, b) -> (b, a)
def swap(tpl):
    '''The swapped components of a pair.'''
    return (tpl[1], tpl[0])


# succ :: Int => Int -> Int
def succ(x):
    '''The successor of a value.
       For numeric types, (1 +).'''
    return 1 + x


# tabulated :: String -> (a -> String) ->
#                        (b -> String) ->
#                        (a -> b) -> [a] -> String
def tabulated(s):
    '''Heading -> x display function ->
                 fx display function ->
                 f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join([
            xShow(x).rjust(w, ' ') + ' -> ' + fxShow(f(x))
            for x in xs

        ])
    return lambda xShow: lambda fxShow: (
        lambda f: lambda xs: go(
            xShow, fxShow, f, xs
        )
    )


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


# uncurry :: (a -> b -> c) -> ((a, b) -> c)
def uncurry(f):
    '''A function over a tuple
       derived from a curried function.'''
    return lambda xy: f(xy[0])(
        xy[1]
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Levenshtein minimum edit distances:

('rosettacode', 'raisethysword') -> 8
('raisethysword', 'rosettacode') -> 8
           ('kitten', 'sitting') -> 3
           ('sitting', 'kitten') -> 3
          ('saturday', 'sunday') -> 3
          ('sunday', 'saturday') -> 3
```



## Racket

A memoized recursive implementation.

```racket
#lang racket

(define (levenshtein a b)
  (define (ls0 a-index b-index)
    (cond [(or (= a-index -1) (= b-index -1)) (abs (- a-index b-index))]
          [else
           (define a-char (string-ref a a-index))
           (define b-char (string-ref b b-index))
           (if (equal? a-char b-char)
               (ls (sub1 a-index) (sub1 b-index))
               (min (add1 (ls (sub1 a-index) b-index))
                    (add1 (ls a-index (sub1 b-index)))
                    (add1 (ls (sub1 a-index) (sub1 b-index)))))]))
  (define memo (make-hash))
  (define (ls a-i b-i)
    (hash-ref! memo (cons a-i b-i) (λ() (ls0 a-i b-i))))
  (ls (sub1 (string-length a)) (sub1 (string-length b))))

(levenshtein "kitten" "sitting")
(levenshtein "rosettacode" "raisethysword")
```

{{out}}

```txt
3
8
```



## REXX


### version 1

As per the task's requirements, this version includes a driver to display the results.

```rexx
/*REXX program  calculates and displays the  Levenshtein distance  between two strings. */
call Levenshtein   'kitten'                        ,     "sitting"
call Levenshtein   'rosettacode'                   ,     "raisethysword"
call Levenshtein   'Sunday'                        ,     "Saturday"
call Levenshtein   'Vladimir Levenshtein[1965]'    ,     "Vladimir Levenshtein[1965]"
call Levenshtein   'this algorithm is similar to'  ,     "Damerau─Levenshtein distance"
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Levenshtein: procedure; parse arg o,t;  oL= length(o);   tL= length(t);     @.= 0
    say '        original string  = '    o                          /*show   old  string*/
    say '          target string  = '    t                          /*  "   target   "  */
                     do #=1  for tL;   @.0.#= #;   end  /*#*/       /*the   drop  array.*/
                     do #=1  for oL;   @.#.0= #;   end  /*#*/       /* "   insert   "   */
         do    j=1  for tL;   jm= j-1;    q= substr(t, j, 1)        /*obtain character. */
            do k=1  for oL;   km= k-1
            if q==substr(o, k, 1)  then @.k.j= @.km.jm              /*use previous char.*/
                                   else @.k.j= 1   +   min(@.km.j,  @.k.jm,  @.km.jm)
            end   /*k*/
         end      /*j*/                                             /* [↑]  best choice.*/
    say '   Levenshtein distance  = '  @.oL.tL;    say;      return
```

{{out|output|text=  when using the internal default inputs:}}

```txt

        original string  =  kitten
          target string  =  sitting
   Levenshtein distance  =  3

        original string  =  rosettacode
          target string  =  raisethysword
   Levenshtein distance  =  8

        original string  =  Sunday
          target string  =  Saturday
   Levenshtein distance  =  3

        original string  =  Vladimir Levenshtein[1965]
          target string  =  Vladimir Levenshtein[1965]
   Levenshtein distance  =  0

        original string  =  this algorithm is similar to
          target string  =  Damerau─Levenshtein distance
   Levenshtein distance  =  24

```



### version 2

same as version 1  (but does not include a driver for testing),  reformatted and commented

```rexx
Levenshtein: Procedure
Parse Arg s,t
/* for all i and j, d[i,j] will hold the Levenshtein distance between     */
/* the first i characters of s and the first j characters of t;           */
/* note that d has (m+1)*(n+1) values                                     */
  m=length(s)
  n=length(t)
  d.=0
  Do i=1 To m  /* source prefixes can be transformed into empty string by */
    d.i.0=i    /* dropping all characters                                 */
    End
  Do j=1 To n  /* target prefixes can be reached from empty source prefix */
    d.0.j=j    /* by inserting every character                            */
    End
  Do j=1 To n
    jj=j-1
    Do i=1 To m
      ii=i-1
      If substr(s,i,1)=substr(t,j,1) Then
        d.i.j=d.ii.jj          /* no operation required                   */
      else
        d.i.j=min(d.ii.j+1,,   /* a deletion                              */
                  d.i.jj+1,,   /* an insertion                            */
                  d.ii.jj+1)   /* a substitution                          */
      End
    End
  Say '          1st string  = '    s
  Say '          2nd string  = '    t
  say 'Levenshtein distance  = ' d.m.n;   say ''
  Return d.m.n
```



### version 3

Alternate algorithm from Wikipedia (but does not include a driver for testing).

```rexx
LevenshteinDistance: Procedure
Parse Arg s,t
If s==t Then Return 0;
sl=length(s)
tl=length(t)
If sl=0 Then Return tl
If tl=0 Then Return sl
Do i=0 To tl
  v0.i=i
  End
Do i=0 To sl-1
  v1.0=i+1
  Do j=0 To tl-1
    jj=j+1
    cost=substr(s,i+1,1)<>substr(t,j+1,1)
    v1.jj=min(v1.j+1,v0.jj+1,v0.j+cost)
    End
  Do j=0 to tl-1
    v0.j=v1.j
    End
  End
return v1.tl
```


===version 4 (recursive)===
Recursive algorithm from Wikipedia with memoization

```rexx
call test 'kitten'      ,'sitting'
call test 'rosettacode' ,'raisethysword'
call test 'Sunday'      ,'Saturday'
call test 'Vladimir_Levenshtein[1965]',,
          'Vladimir_Levenshtein[1965]'
call test 'this_algorithm_is_similar_to',,
          'Damerau-Levenshtein_distance'
call test '','abc'
Exit

test: Procedure
  Parse Arg s,t
  ld.=''
  Say '          1st string  = >'s'<'
  Say '          2nd string  = >'t'<'
  Say 'Levenshtein distance  =' LevenshteinDistance(s,length(s),t,length(t))
  Say ''
  Return

LevenshteinDistance: Procedure Expose ld.
/* sl and tl are the number of characters in string s and t respectively */
  Parse Arg s,sl,t,tl
  If ld.sl.tl<>'' Then
    Return ld.sl.tl
  Select
    When sl=0 Then ld.sl.tl=tl
    When tl=0 Then ld.sl.tl=sl
    Otherwise Do
      /* test if last characters of the strings match */
      cost=substr(s,sl,1)<>substr(t,tl,1)
      /* return minimum of delete char from s, delete char from t,
         and delete char from both */
      ld.sl.tl=min(LevenshteinDistance(s,sl-1,t,tl  )+1,,
                   LevenshteinDistance(s,sl  ,t,tl-1)+1,,
                   LevenshteinDistance(s,sl-1,t,tl-1)+cost)
      End
    End
  Return ld.sl.tl
```



## Ring


```ring

# Project : Levenshtein distance

load "stdlib.ring"
see "" + "distance(kitten, sitting) = " + levenshteindistance("kitten", "sitting") + nl
see "" + "distance(saturday, sunday) = " + levenshteindistance("saturday", "sunday") + nl
see "" + "distance(rosettacode, raisethysword) = " + levenshteindistance("rosettacode", "raisethysword") + nl

func levenshteindistance(s1, s2)
        n = len(s1)
        m = len(s2)
        if n = 0
            levenshteindistance = m
            return
        ok
        if m = 0
            levenshteindistance = n
            return
        ok
        d = newlist(n, m)
        for i = 1 to n
             d[i][1] = i
        next i
        for i = 1 to m
             d[1][i] = i
        next
        for i = 2 to n
             si = substr(s1, i, 1)
             for j = 2 to m
                  tj = substr(s2, j, 1)
                  if si = tj
                     cost = 0
                  else
                     cost = 1
                  ok
                  d[i][ j] = min((d[i - 1][ j]), min((d[i][j - 1] + 1), (d[i - 1][j - 1] + cost)))
             next
        next
        levenshteindistance = d[n][m]
        return levenshteindistance

```

Output:

```txt

distance(kitten, sitting) = 3
distance(saturday, sunday) = 3
distance(rosettacode, raisethysword) = 8

```



## Ruby

Implementation of the wikipedia algorithm. Invariant is that for current loop indices <code>i</code>
and <code>j</code>, <code>costs[k]</code> for <code>k < j</code> contains ''lev(i, k)''
and for <code>k >= j</code> contains ''lev(i-1, k)''.  The inner loop body restores the invariant for the
new value of <code>j</code>.

```ruby
module Levenshtein

  def self.distance(a, b)
    a, b = a.downcase, b.downcase
    costs = Array(0..b.length) # i == 0
    (1..a.length).each do |i|
      costs[0], nw = i, i - 1  # j == 0; nw is lev(i-1, j)
      (1..b.length).each do |j|
        costs[j], nw = [costs[j] + 1, costs[j-1] + 1, a[i-1] == b[j-1] ? nw : nw + 1].min, costs[j]
      end
    end
    costs[b.length]
  end

  def self.test
    %w{kitten sitting saturday sunday rosettacode raisethysword}.each_slice(2) do |a, b|
      puts "distance(#{a}, #{b}) = #{distance(a, b)}"
    end
  end

end

Levenshtein.test
```

{{out}}

```txt

distance(kitten, sitting) = 3
distance(saturday, sunday) = 3
distance(rosettacode, raisethysword) = 8
```


A variant can be found used in Rubygems [https://github.com/rubygems/rubygems/blob/master/lib/rubygems/text.rb]


```ruby
def levenshtein_distance(str1, str2)
  n = str1.length
  m = str2.length
  max = n/2

  return m if 0 == n
  return n if 0 == m
  return n if (n - m).abs > max

  d = (0..m).to_a
  x = nil

  str1.each_char.with_index do |char1,i|
    e = i+1

    str2.each_char.with_index do |char2,j|
      cost = (char1 == char2) ? 0 : 1
      x = [ d[j+1] + 1, # insertion
            e + 1,      # deletion
            d[j] + cost # substitution
          ].min
      d[j] = e
      e = x
    end

    d[m] = x
  end

  x
end

%w{kitten sitting saturday sunday rosettacode raisethysword}.each_slice(2) do |a, b|
  puts "distance(#{a}, #{b}) = #{levenshtein_distance(a, b)}"
end
```

same output


## Run BASIC


```runbasic
print levenshteinDistance("kitten", "sitting")
print levenshteinDistance("rosettacode", "raisethysword")
end
function levenshteinDistance(s1$, s2$)
    n = len(s1$)
    m = len(s2$)
    if n = 0 then
        levenshteinDistance = m
        goto [ex]
    end if
    if m = 0 then
        levenshteinDistance = n
        goto [ex]
    end if
    dim d(n, m)
    for i = 0 to n
        d(i, 0) = i
    next i
    for i = 0 to m
        d(0, i) = i
    next i
    for i = 1 to n
        si$ = mid$(s1$, i, 1)
        for j = 1 to m
            tj$ = mid$(s2$, j, 1)
            if si$ = tj$ then cost = 0 else cost = 1
            d(i, j) = min((d(i - 1, j) + 1),min((d(i, j - 1) + 1),(d(i - 1, j - 1) + cost)))
        next j
    next i
    levenshteinDistance = d(n, m)
[ex]
end function
```
Output:
```txt
3
8
```



## Rust

Implementation of the wikipedia algorithm.
{{works with|Rust|1.1}}

```rust
fn main() {
    println!("{}", levenshtein_distance("kitten", "sitting"));
    println!("{}", levenshtein_distance("saturday", "sunday"));
    println!("{}", levenshtein_distance("rosettacode", "raisethysword"));
}

fn levenshtein_distance(word1: &str, word2: &str) -> usize {
    let w1 = word1.chars().collect::<Vec<_>>();
    let w2 = word2.chars().collect::<Vec<_>>();

    let word1_length = w1.len() + 1;
    let word2_length = w2.len() + 1;

    let mut matrix = vec![vec![0]];

    for i in 1..word1_length { matrix[0].push(i); }
    for j in 1..word2_length { matrix.push(vec![j]); }

    for j in 1..word2_length {
        for i in 1..word1_length {
            let x: usize = if w1[i-1] == w2[j-1] {
                matrix[j-1][i-1]
            } else {
                1 + std::cmp::min(
                        std::cmp::min(matrix[j][i-1], matrix[j-1][i])
                        , matrix[j-1][i-1])
            };
            matrix[j].push(x);
        }
    }
    matrix[word2_length-1][word1_length-1]
}
```

{{out}}

```txt
3
3
8
```



## Scala


### Translated Wikipedia algorithm.


```scala
object Levenshtein0 extends App {

  def distance(s1: String, s2: String): Int = {
    val dist = Array.tabulate(s2.length + 1, s1.length + 1) { (j, i) => if (j == 0) i else if (i == 0) j else 0 }

    @inline
    def minimum(i: Int*): Int = i.min

    for {j <- dist.indices.tail
         i <- dist(0).indices.tail} dist(j)(i) =
        if (s2(j - 1) == s1(i - 1)) dist(j - 1)(i - 1)
        else minimum(dist(j - 1)(i) + 1, dist(j)(i - 1) + 1, dist(j - 1)(i - 1) + 1)

    dist(s2.length)(s1.length)
  }

  def printDistance(s1: String, s2: String) {
    println("%s -> %s : %d".format(s1, s2, distance(s1, s2)))
  }

  printDistance("kitten", "sitting")
  printDistance("rosettacode", "raisethysword")

}
```

{{out}}

```txt
kitten -> sitting : 3
rosettacode -> raisethysword : 8
```

===Functional programmed, memoized===
{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/zj7bHC7/0 (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/qHhDWl68QgWv1uwOYzzNqw Scastie (remote JVM)].

```Scala
import scala.collection.mutable
import scala.collection.parallel.ParSeq

object Levenshtein extends App {

  def printDistance(s1: String, s2: String) =
    println(f"$s1%s -> $s2%s : ${levenshtein(s1, s2)(s1.length, s2.length)}%d")

  def levenshtein(s1: String, s2: String): mutable.Map[(Int, Int), Int] = {
    val memoizedCosts = mutable.Map[(Int, Int), Int]()

    def lev: ((Int, Int)) => Int = {
      case (k1, k2) =>
        memoizedCosts.getOrElseUpdate((k1, k2), (k1, k2) match {
          case (i, 0) => i
          case (0, j) => j
          case (i, j) =>
            ParSeq(1 + lev((i - 1, j)),
              1 + lev((i, j - 1)),
              lev((i - 1, j - 1))
                + (if (s1(i - 1) != s2(j - 1)) 1 else 0)).min
        })
    }

    lev((s1.length, s2.length))
    memoizedCosts
  }

  printDistance("kitten", "sitting")
  printDistance("rosettacode", "raisethysword")
  printDistance("Here's a bunch of words", "to wring out this code")
  printDistance("sleep", "fleeting")

}
```



## Scheme


Recursive version from wikipedia article.


```scheme

(define (levenshtein s t)
  (define (%levenshtein s sl t tl)
    (cond ((zero? sl) tl)
          ((zero? tl) sl)
          (else
	    (min (+ (%levenshtein (cdr s) (- sl 1) t tl) 1)
                 (+ (%levenshtein s sl (cdr t) (- tl 1)) 1)
                 (+ (%levenshtein (cdr s) (- sl 1) (cdr t) (- tl 1))
		    (if (char=? (car s) (car t)) 0 1))))))
  (%levenshtein (string->list s)
		(string-length s)
		(string->list t)
		(string-length t)))

```


{{out}}

```txt

> (levenshtein "kitten" "sitting")
3
> (levenshtein "rosettacode" "raisethysword")
8

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func integer: levenshteinDistance (in string: s, in string: t) is func
  result
    var integer: distance is 0;
  local
    var array array integer: d is 0 times 0 times 0;
    var integer: i is 0;
    var integer: j is 0;
  begin
    d := [0 .. length(s)] times [0 .. length(t)] times 0;
    for key i range s do
      d[i][0] := i;
    end for;
    for key j range t do
      d[0][j] := j;
      for key i range s do
        if s[i] = t[j] then
          d[i][j] := d[pred(i)][pred(j)];
        else
          d[i][j] := min(min(succ(d[pred(i)][j]), succ(d[i][pred(j)])), succ(d[pred(i)][pred(j)]));
        end if;
      end for;
    end for;
    distance := d[length(s)][length(t)];
  end func;

const proc: main is func
  begin
    writeln("kitten -> sitting: " <& levenshteinDistance("kitten", "sitting"));
    writeln("rosettacode -> raisethysword: " <& levenshteinDistance("rosettacode", "raisethysword"));
  end func;
```


{{out}}

```txt

kitten -> sitting: 3
rosettacode -> raisethysword: 8

```



## SequenceL

This implementation is based on the "Iterative with two matrix rows" version on Wikipedia.

```sequenceL

import <Utilities/Sequence.sl>;
import <Utilities/Math.sl>;

main(args(2)) := LenenshteinDistance(args[1], args[2]);

LenenshteinDistance(s(1), t(1)) :=
	0 when equalList(s,t) else
	size(t) when size(s) = 0 else
	size(s) when size(t) = 0 else
	LenenshteinDistanceIterative(s, t, 0 ... size(t), duplicate(0, size(t) + 1), 1);

LenenshteinDistanceIterative(s(1), t(1), v0(1), v1(1), n) :=
	v0[size(t) + 1] when n > size(s) else
	LenenshteinDistanceIterative(s, t, iterate(s[n], t, v0, setElementAt(v1, 1, n + 0), 1), v0, n + 1);

iterate(s, t(1), v0(1), v1(1), n) :=
	v1 when n > size(t) else
	iterate(s, t, v0,
		setElementAt(v1, n + 1,
			min(min(v1[n] + 1, v0[n + 1] + 1), v0[n] + (0 when s = t[n] else 1))),
		n + 1);

```


## Sidef


### Recursive


```ruby
func lev(s, t) is cached {
 
    s || return t.len
    t || return s.len
 
    var s1 = s.ft(1)
    var t1 = t.ft(1)
 
    s[0] == t[0] ? __FUNC__(s1, t1)
                 : 1+Math.min(
                        __FUNC__(s1, t1),
                        __FUNC__(s,  t1),
                        __FUNC__(s1, t )
                     )
}
```



### Iterative


```ruby
func lev(s, t) {
    var d = [@(0 .. t.len), s.len.of {[_]}...]
    for i,j in (^s ~X ^t) {
        d[i+1][j+1] = (
            s[i] == t[j]
                ? d[i][j]
                : 1+Math.min(d[i][j+1], d[i+1][j], d[i][j])
        )
    }
    d[-1][-1]
}
```


Calling the function:

```ruby
say lev(%c'kitten', %c'sitting');               # prints: 3
say lev(%c'rosettacode', %c'raisethysword');    # prints: 8
```


## Simula


```simula
BEGIN

    INTEGER PROCEDURE LEVENSHTEINDISTANCE(S1, S2); TEXT S1, S2;
    BEGIN
        INTEGER N, M;
        N := S1.LENGTH;
        M := S2.LENGTH;
        IF N = 0 THEN LEVENSHTEINDISTANCE := M ELSE
        IF M = 0 THEN LEVENSHTEINDISTANCE := N ELSE
        BEGIN
            INTEGER ARRAY D(0:N, 0:M);
            INTEGER I, J;
            FOR I := 0 STEP 1 UNTIL N DO D(I, 0) := I;
            FOR I := 0 STEP 1 UNTIL M DO D(0, I) := I;
            S1.SETPOS(1);
            FOR I := 1 STEP 1 UNTIL N DO
            BEGIN
                CHARACTER SI, TJ;
                SI := S1.GETCHAR;
                S2.SETPOS(1);
                FOR J := 1 STEP 1 UNTIL M DO
                BEGIN
                    INTEGER COST;
                    TJ := S2.GETCHAR;
                    COST := IF SI = TJ THEN 0 ELSE 1;
                    D(I, J) := MIN(D(I - 1, J) + 1, MIN(D(I, J - 1) + 1, D(I - 1, J - 1) + COST));
                END;
            END;
            LEVENSHTEINDISTANCE := D(N, M);
        END;
    END LEVENSHTEINDISTANCE;

    OUTINT(LEVENSHTEINDISTANCE("kitten", "sitting"), 0); OUTIMAGE;
    OUTINT(LEVENSHTEINDISTANCE("rosettacode", "raisethysword"), 0); OUTIMAGE;

END

```

{{out}}

```txt

3
8


```



## Smalltalk


{{works with|Smalltalk/X}}
ST/X provides a customizable levenshtein method in the String class (weights for individual operations can be passed in):

```smalltalk
'kitten' levenshteinTo: 'sitting' s:1 k:1 c:1 i:1 d:1 -> 3
'rosettacode' levenshteinTo: 'raisethysword' s:1 k:1 c:1 i:1 d:1 -> 8
```



## Swift


Version using entire matrix:


```swift
func levDis(w1: String, w2: String) -> Int {

  let (t, s) = (w1.characters, w2.characters)

  let empty = Repeat(count: s.count, repeatedValue: 0)
  var mat = [[Int](0...s.count)] + (1...t.count).map{[$0] + empty}

  for (i, tLett) in t.enumerate() {
    for (j, sLett) in s.enumerate() {
      mat[i + 1][j + 1] = tLett == sLett ?
        mat[i][j] : min(mat[i][j], mat[i][j + 1], mat[i + 1][j]).successor()
    }
  }
  return mat.last!.last!
}
```


Version using only two rows at a time:


```swift
func levDis(w1: String, w2: String) -> Int {

  let (t, s) = (w1.characters, w2.characters)

  let empty = Repeat(count: s.count, repeatedValue: 0)
  var last = [Int](0...s.count)

  for (i, tLett) in t.enumerate() {
    var cur = [i + 1] + empty
    for (j, sLett) in s.enumerate() {
      cur[j + 1] = tLett == sLett ? last[j] : min(last[j], last[j + 1], cur[j]).successor()
    }
    last = cur
  }
  return last.last!
}
```



## Tcl


```tcl
proc levenshteinDistance {s t} {
    # Edge cases
    if {![set n [string length $t]]} {
	return [string length $s]
    } elseif {![set m [string length $s]]} {
	return $n
    }
    # Fastest way to initialize
    for {set i 0} {$i <= $m} {incr i} {
	lappend d 0
	lappend p $i
    }
    # Loop, computing the distance table (well, a moving section)
    for {set j 0} {$j < $n} {} {
	set tj [string index $t $j]
	lset d 0 [incr j]
	for {set i 0} {$i < $m} {} {
	    set a [expr {[lindex $d $i]+1}]
	    set b [expr {[lindex $p $i]+([string index $s $i] ne $tj)}]
	    set c [expr {[lindex $p [incr i]]+1}]
	    # Faster than min($a,$b,$c)
	    lset d $i [expr {$a<$b ? $c<$a ? $c : $a : $c<$b ? $c : $b}]
	}
	# Swap
	set nd $p; set p $d; set d $nd
    }
    # The score is at the end of the last-computed row
    return [lindex $p end]
}
```

{{out|Usage}}

```tcl
puts [levenshteinDistance "kitten" "sitting"];   # Prints 3
```



## TSE SAL


```TSESAL>// library: math: get: damerau: levenshtein <description></description> <version>1.0.0.0.23</version> <version control></version control
 (filenamemacro=getmadle.s) [kn, ri, th, 08-09-2011 23:04:55]
INTEGER PROC FNMathGetDamerauLevenshteinDistanceI( STRING s1, STRING s2 )
 INTEGER L1 = Length( s1 )
 INTEGER L2 = Length( s2 )
 INTEGER substitutionCostI = 0
 STRING h1[255] = ""
 STRING h2[255] = ""
 IF ( ( L1 == 0 ) OR ( L2 == 0 ) )
  // Trivial case: one string is 0-length
  RETURN( Max( L1, L2 ) )
 ELSE
  // The cost of substituting the last character
  IF   ( ( s1[ L1 ] ) == ( s2[ L2 ] ) )
   substitutionCostI = 0
  ELSE
   substitutionCostI = 1
  ENDIF
  // h1 and h2 are s1 and s2 with the last character chopped off
  h1 = SubStr( s1, 1,  L1 - 1 )
  h2 = SubStr( s2, 1,  L2 - 1 )
  IF ( ( L1 > 1 ) AND  ( L2 > 1 ) AND  ( s1[ L1 - 0 ] == s2[ L2 - 1 ] ) AND ( s1[ L1 - 1 ] == s2[ L2 - 0 ] ) )
   RETURN( Min( Min( FNMathGetDamerauLevenshteinDistanceI( h1, s2 ) + 1, FNMathGetDamerauLevenshteinDistanceI( s1, h2 ) + 1 ), Min( FNMathGetDamerauLevenshteinDistanceI( h1 , h2 ) + substitutionCostI, FNMathGetDamerauLevenshteinDistanceI( SubStr( s1, 1,  L1 - 2 ), SubStr( s2, 1, L2 - 2 ) ) + 1 ) ) )
  ENDIF
  RETURN( Min( Min( FNMathGetDamerauLevenshteinDistanceI( h1, s2 ) + 1, FNMathGetDamerauLevenshteinDistanceI( s1, h2 ) + 1 ), FNMathGetDamerauLevenshteinDistanceI( h1 ,  h2 ) + substitutionCostI ) )
 ENDIF
END

PROC Main()
STRING s1[255] = "arcain"
STRING s2[255] = "arcane"
Warn( "Minimum amount of steps to convert ", s1, " to ", s2, " = ", FNMathGetDamerauLevenshteinDistanceI( s1, s2 ) ) // gives e.g. 2
s1 = "algorithm"
s2 = "altruistic"
Warn( "Minimum amount of steps to convert ", s1, " to ", s2, " = ", FNMathGetDamerauLevenshteinDistanceI( s1, s2 ) ) // gives e.g. 6
END
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
distance=DISTANCE ("kitten", "sitting")
PRINT distance

```

Output:

```txt

3

```


## Vala


```vala
class LevenshteinDistance : Object {
    public static int compute (owned string s, owned string t, bool case_sensitive = false) {
        var n = s.length;
        var m = t.length;
        var d = new int[n + 1, m + 1];
        if (case_sensitive == false) {
            s = s.down ();
            t = t.down ();
        }
        if (n == 0) {
            return m;
        }
        if (m == 0) {
            return n;
        }
        for (var i = 0; i <= n; d[i, 0] = i++) {}
        for (var j = 0; j <= m; d[0, j] = j++) {}
        for (var i = 1; i <= n; i++) {
            for (var j = 1; j <= m; j++) {
                var cost = (t[j - 1] == s[i - 1]) ? 0 : 1;
                d[i, j] = int.min (int.min (d[i - 1, j] + 1, d[i, j - 1] + 1), d[i - 1, j - 1] + cost);
            }
        }
        return d[n, m];
    }
}

```



## VBA

{{trans|Phix}}
```vb
Option Base 1
Function levenshtein(s1 As String, s2 As String) As Integer
    Dim n As Integer: n = Len(s1) + 1
    Dim m As Integer: m = Len(s2) + 1
    Dim d() As Integer, i As Integer, j As Integer
    ReDim d(n, m)

    If n = 1 Then
        levenshtein = m - 1
        Exit Function
    Else
        If m = 1 Then
            levenshtein = n - 1
            Exit Function
        End If
    End If

    For i = 1 To n
        d(i, 1) = i - 1
    Next i

    For j = 1 To m
        d(1, j) = j - 1
    Next j

    For i = 2 To n
        For j = 2 To m
            d(i, j) = WorksheetFunction.Min( _
                           d(i - 1, j) + 1, _
                           d(i, j - 1) + 1, _
                           (d(i - 1, j - 1) - (Mid(s1, i - 1, 1) <> Mid(s2, j - 1, 1))) _
                           )
        Next j
    Next i

    levenshtein = d(n, m)
End Function
Public Sub main()
    Debug.Print levenshtein("kitten", "sitting")
    Debug.Print levenshtein("rosettacode", "raisethysword")
End Sub
```
{{out}}

```txt
 3
 8
```



## Visual Basic

{{Trans|FreeBASIC}}
{{works with|Visual Basic|5}}
{{works with|Visual Basic|6}}
{{works with|VBA|Access 97}}
{{works with|VBA|6.5}}
{{works with|VBA|7.1}}

```vb
Function min(x As Integer, y As Integer) As Integer
    If x < y Then
        min = x
    Else
        min = y
    End If
End Function

Function levenshtein(s As String, t As String) As Integer
Dim ls As Integer, lt As Integer
Dim i As Integer, j As Integer, cost As Integer
    ' degenerate cases
    ls = Len(s)
    lt = Len(t)
    If ls = lt Then
        If s = t Then
            Exit Function ' return 0
        End If
    ElseIf ls = 0 Then
        levenshtein = lt
        Exit Function
    ElseIf lt = 0 Then
        levenshtein = ls
        Exit Function
    End If

    ' create two integer arrays of distances
    ReDim v0(0 To lt) As Integer  '' previous
    ReDim v1(0 To lt) As Integer  '' current

    ' initialize v0
    For i = 0 To lt
        v0(i) = i
    Next i

    For i = 0 To ls - 1
       ' calculate v1 from v0
       v1(0) = i + 1

       For j = 0 To lt - 1
           cost = Abs(CInt(Mid$(s, i + 1, 1) <> Mid$(t, j + 1, 1)))
           v1(j + 1) = min(v1(j) + 1, min(v0(j + 1) + 1, v0(j) + cost))
       Next j

       ' copy v1 to v0 for next iteration
       For j = 0 To lt
           v0(j) = v1(j)
       Next j
    Next i

    levenshtein = v1(lt)
End Function

Sub Main()
' tests
    Debug.Print "'kitten' to 'sitting'            => "; levenshtein("kitten", "sitting")
    Debug.Print "'sitting' to 'kitten'            => "; levenshtein("sitting", "kitten")
    Debug.Print "'rosettacode' to 'raisethysword' => "; levenshtein("rosettacode", "raisethysword")
    Debug.Print "'sleep' to 'fleeting'            => "; levenshtein("sleep", "fleeting")
End Sub

```

{{out}}

```txt
'kitten' to 'sitting'            =>  3
'sitting' to 'kitten'            =>  3
'rosettacode' to 'raisethysword' =>  8
'sleep' to 'fleeting'            =>  5
```



## Visual Basic .NET


```vbnet
 Function LevenshteinDistance(ByVal String1 As String, ByVal String2 As String) As Integer
        Dim Matrix(String1.Length, String2.Length) As Integer
        Dim Key As Integer
        For Key = 0 To String1.Length
            Matrix(Key, 0) = Key
        Next
        For Key = 0 To String2.Length
            Matrix(0, Key) = Key
        Next
        For Key1 As Integer = 1 To String2.Length
            For Key2 As Integer = 1 To String1.Length
                If String1(Key2 - 1) = String2(Key1 - 1) Then
                    Matrix(Key2, Key1) = Matrix(Key2 - 1, Key1 - 1)
                Else
                    Matrix(Key2, Key1) = Math.Min(Matrix(Key2 - 1, Key1) + 1, Math.Min(Matrix(Key2, Key1 - 1) + 1, Matrix(Key2 - 1, Key1 - 1) + 1))
                End If
            Next
        Next
        Return Matrix(String1.Length - 1, String2.Length - 1)
    End Function
```



## zkl

{{trans|D}}

```zkl
fcn levenshtein(s1,s2){
   sz2,costs:=s2.len() + 1, List.createLong(sz2,0);  // -->zero filled List
   foreach i in (s1.len() + 1){
      lastValue:=i;
      foreach j in (sz2){
         if (i==0) costs[j]=j;
	 else if (j>0){
	    newValue:=costs[j-1];
	    if (s1[i-1]!=s2[j-1])
	       newValue=newValue.min(lastValue, costs[j]) + 1;
	    costs[j-1]=lastValue;
	    lastValue =newValue;
	 }
      }
      if (i>0) costs[-1]=lastValue;
   }
   costs[-1]
}
```


```zkl
foreach a,b in (T(T("kitten","sitting"), T("rosettacode","raisethysword"),
	T("yo",""), T("","yo"), T("abc","abc")) ){
   println(a," --> ",b,": ",levenshtein(a,b));
}
```

{{out}}

```txt

kitten --> sitting: 3
rosettacode --> raisethysword: 8
yo --> : 2
 --> yo: 2
abc --> abc: 0

```
