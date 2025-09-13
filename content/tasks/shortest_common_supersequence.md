+++
title = "Shortest common supersequence"
description = ""
date = 2019-10-08T06:19:22Z
aliases = []
[extra]
id = 12961
[taxonomies]
categories = ["task"]
tags = []
+++

The   '''[[wp:shortest common supersequence|shortest common supersequence]]'''   is a problem closely related to the   [[longest common subsequence]],   which you can use as an external function for this task.


;;Task:
Given two strings <math>u</math> and <math>v</math>, find the shortest possible sequence <math>s</math>, which is the shortest common super-sequence of <math>u</math> and <math>v</math> where both <math>u</math> and <math>v</math> are a subsequence of <math>s</math>.  Defined as such, <math>s</math> is not necessarily unique.

Demonstrate this by printing <math>s</math> where <math>u = </math>“<tt>abcbdab</tt>” and <math>v = </math>“<tt>bdcaba</tt>”.
<!-- This example is taken from the Wikipedia page. -->


;Also see:
*   The Wikipedia article:   [http://wikipedia.org/wiki/Shortest_common_supersequence_problem shortest common supersequence].





## C

The C99 code here isn't all that different from Levenstein distance calculation.

```c
#include <stdio.h>
#include <string.h>

typedef struct link link_t;
struct link {
	int len;
	char letter;
	link_t *next;
};

// Stores a copy of a SCS of x and y in out.  Caller needs to make sure out is long enough.
int scs(char *x, char *y, char *out)
{
	int lx = strlen(x), ly = strlen(y);
	link_t lnk[ly + 1][lx + 1];

	for (int i = 0; i < ly; i++)
		lnk[i][lx] = (link_t) {ly - i, y[i], &lnk[i + 1][lx]};

	for (int j = 0; j < lx; j++)
		lnk[ly][j] = (link_t) {lx - j, x[j], &lnk[ly][j + 1]};

	lnk[ly][lx] = (link_t) {0};

	for (int i = ly; i--; ) {
		for (int j = lx; j--; ) {
			link_t *lp = &lnk[i][j];
			if (y[i] == x[j]) {
				lp->next = &lnk[i+1][j+1];
				lp->letter = x[j];
			} else if (lnk[i][j+1].len < lnk[i+1][j].len) {
				lp->next = &lnk[i][j+1];
				lp->letter = x[j];
			} else {
				lp->next = &lnk[i+1][j];
				lp->letter = y[i];
			}
			lp->len = lp->next->len + 1;
		}
	}

	for (link_t *lp = &lnk[0][0]; lp; lp = lp->next)
		*out++ = lp->letter;

	return 0;
}

int main(void)
{
	char x[] = "abcbdab", y[] = "bdcaba", res[128];
	scs(x, y, res);
	printf("SCS(%s, %s) -> %s\n", x, y, res);
	return 0;
}
```

```txt

SCS(abcbdab, bdcaba) -> abdcabdab

```



## D

```d
import std.stdio, std.functional, std.array, std.range;

dstring scs(in dstring x, in dstring y) nothrow @safe {
    alias mScs = memoize!scs;
    if (x.empty) return y;
    if (y.empty) return x;
    if (x.front == y.front)
        return x.front ~ mScs(x.dropOne, y.dropOne);
    if (mScs(x, y.dropOne).length <= mScs(x.dropOne, y).length)
        return y.front ~ mScs(x, y.dropOne);
    else
        return x.front ~ mScs(x.dropOne, y);
}

void main() @safe {
    scs("abcbdab", "bdcaba").writeln;
}
```

```txt
abdcabdab
```



## Go

```go
package main

import (
    "fmt"
    "strings"
)

func lcs(x, y string) string {
    xl, yl := len(x), len(y)
    if xl == 0 || yl == 0 {
        return ""
    }
    x1, y1 := x[:xl-1], y[:yl-1]
    if x[xl-1] == y[yl-1] {
        return fmt.Sprintf("%s%c", lcs(x1, y1), x[xl-1])
    }
    x2, y2 := lcs(x, y1), lcs(x1, y)
    if len(x2) > len(y2) {
        return x2
    } else {
        return y2
    }
}

func scs(u, v string) string {
    ul, vl := len(u), len(v)
    lcs := lcs(u, v)
    ui, vi := 0, 0
    var sb strings.Builder
    for i := 0; i < len(lcs); i++ {
        for ui < ul && u[ui] != lcs[i] {
            sb.WriteByte(u[ui])
            ui++
        }
        for vi < vl && v[vi] != lcs[i] {
            sb.WriteByte(v[vi])
            vi++
        }
        sb.WriteByte(lcs[i])
        ui++
        vi++
    }
    if ui < ul {
        sb.WriteString(u[ui:])
    }
    if vi < vl {
        sb.WriteString(v[vi:])
    }
    return sb.String()
}

func main() {
    u := "abcbdab"
    v := "bdcaba"
    fmt.Println(scs(u, v))
}
```


```txt

abdcabdab

```



## Elixir

uses 'LCS' from [[Longest common subsequence#Elixir|here]]

```elixir
defmodule SCS do
  def scs(u, v) do
    lcs = LCS.lcs(u, v) |> to_charlist
    scs(to_charlist(u), to_charlist(v), lcs, []) |> to_string
  end

  defp scs(u, v, [], res), do: Enum.reverse(res) ++ u ++ v
  defp scs([h|ut], [h|vt], [h|lt], res),      do: scs(ut, vt, lt, [h|res])
  defp scs([h|_]=u, [vh|vt], [h|_]=lcs, res), do: scs(u, vt, lcs, [vh|res])
  defp scs([uh|ut], v, lcs, res),             do: scs(ut, v, lcs, [uh|res])
end

u = "abcbdab"
v = "bdcaba"
IO.puts "SCS(#{u}, #{v}) = #{SCS.scs(u, v)}"
```


```txt

SCS(abcbdab, bdcaba) = abdcabdab

```



## Factor

```factor
USING: combinators io kernel locals math memoize sequences ;

MEMO:: scs ( x y -- seq )
    {
        { [ x empty? ] [ y ] }
        { [ y empty? ] [ x ] }
        { [ x first y first = ]
          [ x rest y rest scs x first prefix ] }
        { [ x y rest scs length x rest y scs length <= ]
          [ x y rest scs y first prefix ] }
        [ x rest y scs x first prefix ]
    } cond ;

"abcbdab" "bdcaba" scs print
```

```txt

abdcabdab

```



## Julia

```Julia
using Memoize

@memoize function scs(x, y)
    if x == ""
        return y
    elseif y == ""
        return x
    elseif x[1] == y[1]
        return "$(x[1])$(scs(x[2:end], y[2:end]))"
    elseif length(scs(x, y[2:end])) <= length(scs(x[2:end], y))
        return "$(y[1])$(scs(x, y[2:end]))"
    else
        return "$(x[1])$(scs(x[2:end], y))"
    end
end

println(scs("abcbdab", "bdcaba"))

```

```txt

abdcabdab

```




## Kotlin

Uses 'lcs' function from [[Longest common subsequence#Kotlin]]:

```scala
// version 1.1.2

fun lcs(x: String, y: String): String {
    if (x.length == 0 || y.length == 0) return ""
    val x1 = x.dropLast(1)
    val y1 = y.dropLast(1)
    if (x.last() == y.last()) return lcs(x1, y1) + x.last()
    val x2 = lcs(x, y1)
    val y2 = lcs(x1, y)
    return if (x2.length > y2.length) x2 else y2
}

fun scs(u: String, v: String): String{
    val lcs = lcs(u, v)
    var ui = 0
    var vi = 0
    val sb = StringBuilder()
    for (i in 0 until lcs.length) {
        while (ui < u.length && u[ui] != lcs[i]) sb.append(u[ui++])
        while (vi < v.length && v[vi] != lcs[i]) sb.append(v[vi++])
        sb.append(lcs[i])
        ui++; vi++
    }
    if (ui < u.length) sb.append(u.substring(ui))
    if (vi < v.length) sb.append(v.substring(vi))
    return sb.toString()
}

fun main(args: Array<String>) {
    val u = "abcbdab"
    val v = "bdcaba"
    println(scs(u, v))
}
```


```txt

abdcabdab

```



## Perl


```perl
sub lcs { # longest common subsequence
    my( $u, $v ) = @_;
    return '' unless length($u) and length($v);
    my $longest = '';
    for my $first ( 0..length($u)-1 ) {
        my $char = substr $u, $first, 1;
        my $i = index( $v, $char );
        next if -1==$i;
        my $next = $char;
        $next .= lcs( substr( $u, $first+1), substr( $v, $i+1 ) ) unless $i==length($v)-1;
        $longest = $next if length($next) > length($longest);
    }
    return $longest;
}

sub scs { # shortest common supersequence
    my( $u, $v ) = @_;
    my @lcs = split //, lcs $u, $v;
    my $pat = "(.*)".join("(.*)",@lcs)."(.*)";
    my @u = $u =~ /$pat/;
    my @v = $v =~ /$pat/;
    my $scs = shift(@u).shift(@v);
    $scs .= $_.shift(@u).shift(@v) for @lcs;
    return $scs;
}

my $u = "abcbdab";
my $v = "bdcaba";
printf "Strings %s %s\n", $u, $v;
printf "Longest common subsequence:   %s\n", lcs $u, $v;
printf "Shortest common supersquence: %s\n", scs $u, $v;

```

```txt
Strings abcbdab bdcaba
Longest common subsequence:   bcba
Shortest common supersquence: abdcabdab

```



## Perl 6

Using 'lcs' routine from [[Longest_common_subsequence#Perl_6 |Longest common subsequence task]]

```perl6
sub lcs(Str $xstr, Str $ystr) { # longest common subsequence
    return "" unless $xstr && $ystr;
    my ($x, $xs, $y, $ys) = $xstr.substr(0, 1), $xstr.substr(1), $ystr.substr(0, 1), $ystr.substr(1);
    return $x eq $y
        ?? $x ~ lcs($xs, $ys)
        !! max(:by{ $^a.chars }, lcs($xstr, $ys), lcs($xs, $ystr) );
}

sub scs ($u, $v) { # shortest common supersequence
    my @lcs = (lcs $u, $v).comb;
    my $pat = '(.*)' ~ join('(.*)',@lcs) ~ '(.*)';
    my $regex = "rx/$pat/".EVAL;
    my @u = ($u ~~ $regex).list;
    my @v = ($v ~~ $regex).list;
    my $scs = shift(@u) ~ shift(@v);
    $scs ~= $_ ~ shift(@u) ~ shift(@v) for @lcs;
    return $scs;
}

my $u = 'abcbdab';
my $v = 'bdcaba';
printf "Strings: %s %s\n", $u, $v;
printf "Longest common subsequence:   %s\n", lcs $u, $v;
printf "Shortest common supersquence: %s\n", scs $u, $v;
```

```txt
Strings: abcbdab bdcaba
Longest common subsequence:   bcba
Shortest common supersquence: abdcabdab
```



## Phix

```Phix
function longest_common_subsequence(sequence a, b)
sequence res = ""
    if length(a) and length(b) then
        if a[$]=b[$] then
            res = longest_common_subsequence(a[1..-2],b[1..-2])&a[$]
        else
            sequence l = longest_common_subsequence(a,b[1..-2]),
                     r = longest_common_subsequence(a[1..-2],b)
            res = iff(length(l)>length(r)?l:r)
        end if
    end if
    return res
end function

function shortest_common_supersequence(string a, b)
    string lcs = longest_common_subsequence(a, b),
           scs = ""
    -- Consume lcs
    while length(lcs) do
        integer c = lcs[1]
        if a[1]==c and b[1]==c then
            -- Part of the lcs, so consume from all strings
            scs &= c
            lcs = lcs[2..$]
            a = a[2..$]
            b = b[2..$]
        elsif a[1]==c then
            scs &= b[1]
            b = b[2..$]
        else
            scs &= a[1]
            a = a[2..$]
        end if
    end while
    -- append remaining characters
    return scs & a & b
end function

?shortest_common_supersequence("abcbdab", "bdcaba")
?shortest_common_supersequence("WEASELS", "WARDANCE")
```

```txt

"abdcabdab"
"WEASRDANCELS"

```



## Python


```Python

# Use the Longest Common Subsequence algorithm

def shortest_common_supersequence(a, b):
    lcs = longest_common_subsequence(a, b)
    scs = ""
    # Consume lcs
    while len(lcs) > 0:
        if a[0]==lcs[0] and b[0]==lcs[0]:
        # Part of the LCS, so consume from all strings
            scs += lcs[0]
            lcs = lcs[1:]
            a = a[1:]
            b = b[1:]
        elif a[0]==lcs[0]:
            scs += b[0]
            b = b[1:]
        else:
            scs += a[0]
            a = a[1:]
    # append remaining characters
    return scs + a + b

```

```txt

Seq1: WEASELS
Seq2: WARDANCE
SCS:  WEASRDANCELS

```



## Racket

{{trans|C}}This program is based on the C implementation, but use memorization instead of dynamic programming. More explanations about the memorization part in http://blog.racket-lang.org/2012/08/dynamic-programming-versus-memoization.html .

```Racket
#lang racket

(struct link (len letters))

(define (link-add li n letter)
  (link (+ n (link-len li))
        (cons letter (link-letters li))))

(define (memoize f)
  (local ([define table (make-hash)])
    (lambda args
      (dict-ref! table args (λ () (apply f args))))))

(define scs/list
  (memoize
   (lambda (x y)
     (cond
       [(null? x)
        (link (length y) y)]
       [(null? y)
        (link (length x) x)]
       [(eq? (car x) (car y))
        (link-add (scs/list (cdr x) (cdr y)) 1 (car x))]
       [(<= (link-len (scs/list x (cdr y)))
            (link-len (scs/list (cdr x) y)))
        (link-add (scs/list x (cdr y)) 1 (car y))]
       [else
        (link-add (scs/list (cdr x) y) 1 (car x))]))))

(define (scs x y)
  (list->string (link-letters (scs/list (string->list x) (string->list y)))))

(scs "abcbdab" "bdcaba")
```

```txt
"abdcabdab"
```



## REXX

```rexx
/*REXX program finds the  Shortest common supersequence (SCS)  of two character strings.*/
parse arg u v .                                  /*obtain optional arguments from the CL*/
if u=='' | u==","  then u= 'abcbdab'             /*Not specified?  Then use the default.*/
if v=='' | v==","  then v= 'bdcaba'              /* "      "         "   "   "     "    */
say '                     string u='  u          /*echo the value of string  U  to term.*/
say '                     string v='  v          /*  "   "    "    "    "    V   "   "  */
$= u                                             /*define initial value for the output. */
      do n=1    to length(u)                     /*process the whole length of string U.*/
        do m=n  to length(v) - 1                 /*   "    right─ish  part   "    "   V.*/
        p= pos( substr(v, m, 1), $)              /*position of mTH  V  char in $ string.*/
        _= substr(v, m+1, 1)                     /*obtain a single character of string V*/
        if p\==0  &  _\==substr($, p+1, 1)  then $= insert(_, $, p)
        end   /*m*/                              /* [↑]  insert _ in $ after position P.*/
      end     /*n*/
say
say 'shortest common supersequence='  $          /*stick a fork in it,  we're all done. */
```

```txt

                     string u= abcbdab
                     string v= bdcaba

shortest common supersequence= abdcabdab

```

```txt

                     string u= ab
                     string v= ac

shortest common supersequence= acb

```

```txt

                     string u= ac
                     string v= ab

shortest common supersequence= abc

```



## Ring


```ring

# Project : Shortest common supersequence

str1 = "a b c b d a b"
str2 = "bdcaba"
str3 = str2list(substr(str1, " ", nl))
for n = 1 to len(str3)
     for m = n to len(str2)-1
          pos = find(str3, str2[m])
          if pos > 0 and str2[m+1] != str3[pos+1]
             insert(str3, pos, str2[m+1])
          ok
     next
next
showarray(str3)

func showarray(vect)
       svect = ""
       for n = 1 to len(vect)
             svect = svect + vect[n]
       next
       see svect

```

Output:

```txt

Shortest common supersequence: abdcabdab

```



## Ruby

uses 'lcs' from [[Longest common subsequence#Ruby|here]]

```ruby
require 'lcs'

def scs(u, v)
  lcs = lcs(u, v)
  u, v = u.dup, v.dup
  scs = ""
  # Iterate over the characters until LCS processed
  until lcs.empty?
    if u[0]==lcs[0] and v[0]==lcs[0]
      # Part of the LCS, so consume from all strings
      scs << lcs.slice!(0)
      u.slice!(0)
      v.slice!(0)
    elsif u[0]==lcs[0]
      # char of u = char of LCS, but char of LCS v doesn't so consume just that
      scs << v.slice!(0)
    else
      # char of u != char of LCS, so consume just that
      scs << u.slice!(0)
    end
  end
  # append remaining characters, which are not in common
  scs + u + v
end

u = "abcbdab"
v = "bdcaba"
puts "SCS(#{u}, #{v}) = #{scs(u, v)}"
```


```txt

SCS(abcbdab, bdcaba) = abcbdcaba

```



## Sidef

Uses the ''lcs'' function defined [http://rosettacode.org/wiki/Longest_common_subsequence#Sidef here].

```ruby
func scs(u, v) {
    var ls = lcs(u, v).chars
    var pat = Regex('(.*)'+ls.join('(.*)')+'(.*)')
    u.scan!(pat)
    v.scan!(pat)
    var ss = (u.shift + v.shift)
    ls.each { |c| ss += (c + u.shift + v.shift) }
    return ss
}

say scs("abcbdab", "bdcaba")
```

```txt

abdcabdab

```



## Tcl

This example uses either of the <code>lcs</code> implementations from [[longest common subsequence#Tcl|here]], assumed renamed to <tt>lcs</tt>…

```tcl
proc scs {u v} {
    set lcs [lcs $u $v]
    set scs ""

    # Iterate over the characters until LCS processed
    for {set ui [set vi [set li 0]]} {$li<[string length $lcs]} {} {
	set uc [string index $u $ui]
	set vc [string index $v $vi]
	set lc [string index $lcs $li]
	if {$uc eq $lc} {
	    if {$vc eq $lc} {
		# Part of the LCS, so consume from all strings
		append scs $lc
		incr ui
		incr li
	    } else {
		# char of u = char of LCS, but char of LCS v doesn't so consume just that
		append scs $vc
	    }
	    incr vi
	} else {
	    # char of u != char of LCS, so consume just that
	    append scs $uc
	    incr ui
	}
    }

    # append remaining characters, which are not in common
    append scs [string range $u $ui end] [string range $v $vi end]
    return $scs
}
```

Demonstrating:

```tcl
set u "abcbdab"
set v "bdcaba"
puts "SCS($u,$v) = [scs $u $v]"
```

```txt
SCS(abcbdab,bdcaba) = abdcabdab
```



## zkl

```zkl
class Link{ var len,letter,next;
   fcn init(l=0,c="",lnk=Void){ len,letter,next=l,c,lnk; }
}
fcn scs(x,y,out){
   lx,ly:=x.len(),y.len();
   lnk:=(ly+1).pump(List,'wrap(_){ (lx+1).pump(List(),Link.create) });

   foreach i in (ly){ lnk[i][lx]=Link(ly-i, y[i]) }
   foreach j in (lx){ lnk[ly][j]=Link(lx-j, x[j]) }

   foreach i,j in ([ly-1..0,-1],[lx-1..0,-1]){
      lp:=lnk[i][j];
      if (y[i]==x[j]){
	 lp.next  =lnk[i+1][j+1];
	 lp.letter=x[j];
      }else if(lnk[i][j+1].len < lnk[i+1][j].len){
	 lp.next  =lnk[i][j+1];
	 lp.letter=x[j];
      }else{
	 lp.next  =lnk[i+1][j];
	 lp.letter=y[i];
      }
      lp.len=lp.next.len + 1;
   }

   lp:=lnk[0][0]; while(lp){ out.write(lp.letter); lp=lp.next; }
   out.close()
}
```


```zkl
scs("abcbdab","bdcaba", Sink(String)).println();
```

```txt

abdcabdab

```


