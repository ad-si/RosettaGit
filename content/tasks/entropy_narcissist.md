+++
title = "Entropy/Narcissist"
description = ""
date = 2019-03-26T21:56:57Z
aliases = []
[extra]
id = 12977
[taxonomies]
categories = ["task"]
tags = []
+++

[[File:ENTROPY.JPG|800px||right]]

## Task

Write a computer program that computes and shows its own   [[entropy]].


;Related Tasks:
:*   [[Fibonacci_word]]
:*   [[Entropy]]





## ALGOL 68

Assumes the source file is in the current directory and called "entropyNarcissist.a68".


Note that the source here uses spaces, not tabs, hence the low entropy, replacing all runs of four spaces with a single space
results in an entropy of +4.64524532762062e  +0.

```algol68
BEGIN
    # calculate the shannon entropy of a string                                #
    PROC shannon entropy = ( STRING s )REAL:
    BEGIN
        INT string length = ( UPB s - LWB s ) + 1;
        # count the occurances of each character #
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

    IF  FILE input file;
        STRING file name = "entropyNarcissist.a68";
        open( input file, file name, stand in channel ) /= 0
    THEN
        # failed to open the file #
        print( ( "Unable to open """ + file name + """", newline ) )
    ELSE
        # file opened OK #
        BOOL at eof := FALSE;
        # set the EOF handler for the file #
        on logical file end( input file
                           , ( REF FILE f )BOOL:
                             BEGIN
                                 # note that we reached EOF on the latest read #
                                 at eof := TRUE;
                                 # return TRUE so processing can continue #
                                 TRUE
                             END
                           );
        # construct a string containing the whole file #
        STRING file contents := "";
        WHILE STRING line;
              get( input file, ( line, newline ) );
              NOT at eof
        DO
            file contents +:= line + REPR 12
        OD;
        close( input file );
        # show the entropy of the file cotents #
        print( ( shannon entropy( file contents ), newline ) )
    FI
END
```

```txt

+3.93440186690189e  +0

```



## AutoHotkey

```AutoHotkey
FileRead, var, *C %A_ScriptFullPath%
MsgBox, % Entropy(var)

Entropy(n) {
    a := [], len := StrLen(n), m := n
    while StrLen(m) {
        s := SubStr(m, 1, 1)
        m := RegExReplace(m, s, "", c)
        a[s] := c
    }
    for key, val in a {
        m := Log(p := val / len)
        e -= p * m / Log(2)
    }
    return, e
}
```

```txt
5.942956
```



## C


Minor edit to the [[Entropy#C|Entropy]] answer.

Assumes that the source file is stored in the working directory as "entropy.c".

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>

#define MAXLEN 961 //maximum string length

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
	FILE *f;
	f=fopen("entropy.c","r");
	for(len=0;!feof(f);len++)S[len]=fgetc(f);
	S[--len]='\0';
	hist=(int*)calloc(len,sizeof(int));
	histlen=makehist(S,hist,len);
	//hist now has no order (known to the program) but that doesn't matter
	H=entropy(hist,histlen,len);
	printf("%lf\n",H);
	return 0;
}
```

<lang>5.195143
```



## Crystal

```ruby
def entropy(s)
  counts = s.chars.each_with_object(Hash(Char, Float64).new(0.0)) { |c, h| h[c] += 1 }
  counts.values.sum do |count|
    freq = count / s.size
    -freq * Math.log2(freq)
  end
end

puts entropy File.read(__FILE__)
```

```txt

4.963709090807145

```



## D


```d
void main(in string[] args) {
    import std.stdio, std.algorithm, std.math, std.file;

    auto data = sort(cast(ubyte[])args[0].read);
    return data
           .group
           .map!(g => g[1] / double(data.length))
           .map!(p => -p * p.log2)
           .sum
           .writeln;
}
```

```txt
6.29803
```



## Elixir


```elixir
File.open(__ENV__.file, [:read], fn(file) ->
  text = IO.read(file, :all)
  leng = String.length(text)
  String.codepoints(text)
  |> Enum.group_by(&(&1))
  |> Enum.map(fn{_,value} -> length(value) end)
  |> Enum.reduce(0, fn count, entropy ->
       freq = count / leng
       entropy - freq * :math.log2(freq)
     end)
  |> IO.puts
end)
```


```txt

4.848342673395324

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

(defun narcissist ()
  (shannon-entropy (with-temp-buffer
		     (insert-file-contents "U:/rosetta/narcissist.el")
		     (buffer-string))))
```

```lisp
(narcissist)
4.5129548515535785
```



## Erlang


```erlang
#! /usr/bin/escript

-define(LOG2E, 1.44269504088896340735992).

main(_) ->
    Self = escript:script_name(),
    {ok, Contents} = file:read_file(Self),
    io:format("My entropy is ~p~n", [entropy(Contents)]).

entropy(Data) ->
    Frq = count(Data),
    maps:fold(fun(_, C, E) ->
                  P = C / byte_size(Data),
                  E - P*math:log(P)
              end, 0, Frq) * ?LOG2E.

count(Data) -> count(Data, 0, #{}).
count(Data, I, Frq) when I =:= byte_size(Data) -> Frq;
count(Data, I, Frq) ->
    Chr = binary:at(Data, I),
    case Frq of
        #{Chr := K} -> count(Data, I+1, Frq #{Chr := K+1});
        _ -> count(Data, I+1, Frq #{Chr => 1})
    end.

```

```txt

My entropy is 5.00988934931771

```



## Factor


```factor
USING: assocs io io.encodings.utf8 io.files kernel math
math.functions math.statistics prettyprint sequences ;
IN: rosetta-code.entropy-narcissist

: entropy ( seq -- entropy )
    [ length ] [ histogram >alist [ second ] map ] bi
    [ swap / ] with map
    [ dup log 2 log / * ] map-sum neg ;

"entropy-narcissist.factor" utf8 [
    contents entropy .
] with-file-reader
```

```txt

4.591946214804276

```



## FreeBASIC


```FreeBASIC
' version 01-06-2016
' compile with: fbc -s console
' modified code from ENTROPY entry

Dim As Integer i, count, totalchar(255)
Dim As UByte buffer
Dim As Double prop, entropy
' command (0) returns the name of this program (including the path)
Dim As String slash, filename = Command(0)
Dim As Integer ff = FreeFile   ' find first free filenumber
Open filename For Binary As #ff

If Err > 0 Then ' should not happen
    Print "Error opening the file"
    Beep : Sleep 5000, 1
    End
End If

' will read 1 UByte from the file until it reaches the end of the file
For i = 1 To Lof(ff)
    Get #ff, ,buffer
    totalchar(buffer) += 1
    count = count + 1
Next

For i = 0  To 255
    If totalchar(i) = 0 Then Continue For
    prop = totalchar(i) / count
    entropy = entropy - (prop * Log (prop) / Log(2))
Next

' next lines are only compiled when compiling for Windows OS (32/64)
#Ifdef __FB_WIN32__
    slash = chr(92)
    print "Windows version"
#endif
#Ifdef __FB_LINUX__
   slash = chr(47)
   print "LINUX version"
#EndIf

    i = InStrRev(filename, slash)
    If i <> 0 Then filename = Right(filename, Len(filename)-i)

Print "My name is "; filename
Print : Print "The Entropy of myself is"; entropy
Print

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
Windows version
My name is entropy_narcissist.exe

The Entropy of myself is 6.142286625408597

LINUX version
My name is entropy_narcissist

The Entropy of myself is 5.450343613062795
```



## Go


```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "math"
    "os"
    "runtime"
)

func main() {
    _, src, _, _ := runtime.Caller(0)
    fmt.Println("Source file entropy:", entropy(src))
    fmt.Println("Binary file entropy:", entropy(os.Args[0]))
}

func entropy(file string) float64 {
    d, err := ioutil.ReadFile(file)
    if err != nil {
        log.Fatal(err)
    }
    var f [256]float64
    for _, b := range d {
        f[b]++
    }
    hm := 0.
    for _, c := range f {
        if c > 0 {
            hm += c * math.Log2(c)
        }
    }
    l := float64(len(d))
    return math.Log2(l) - hm/l
}
```

```txt

Source file entropy: 5.038501725029859
Binary file entropy: 5.388171194771937

```



## Haskell


```haskell
import qualified Data.ByteString as BS
import Data.List
import System.Environment

(>>>) = flip (.)

main = getArgs >>= head >>> BS.readFile >>= BS.unpack >>> entropy >>> print

entropy = sort >>> group >>> map genericLength >>> normalize >>> map lg >>> sum
  where lg c = -c * logBase 2 c
        normalize c = let sc = sum c in map (/ sc) c
```


{{out}} In a shell

 $ ghc --make -O3 Narcissist.hs

Entropy of the source

 $ ./Narcissist Narcissist.hs
 4.452645183154108

Entropy of the binary

 $ ./Narcissist Narcissist
 5.525417236346172


## J

'''Solution''':
```j
   entropy=:  +/@:-@(* 2&^.)@(#/.~ % #)
   1!:2&2 entropy 1!:1 (4!:4 <'entropy') { 4!:3''
```

'''Example''':
```j
   load 'entropy.ijs'
4.73307
```



## Julia

Entropy function copied from [[Entropy#Julia]].


```julia
using DataStructures
entropy(s) = -sum(x -> x / length(s) * log2(x / length(s)), values(counter(s)))
println("self-entropy: ", entropy(readstring(Base.source_path())))
```


```txt
self-entropy: 4.8684243451359706
```



## Kotlin


```scala
// version 1.1.0 (entropy_narc.kt)

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
   val prog = java.io.File("entropy_narc.kt").readText()
   println("This program's entropy is ${"%18.16f".format(shannon(prog))}")
}
```


```txt

This program's entropy is 4.8471803665906705

```



## Lua

arg[0] gives the path of the script currently being executed

```Lua
function getFile (filename)
    local inFile = io.open(filename, "r")
    local fileContent = inFile:read("*all")
    inFile:close()
    return fileContent
end

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

print(entropy(getFile(arg[0])))
```

```txt
4.3591214356783
```



## PARI/GP


```parigp
entropy(s)=s=Vec(s);my(v=vecsort(s,,8));-sum(i=1,#v,(x->x*log(x))(sum(j=1,#s,v[i]==s[j])/#s))/log(2);
entropy(Str(entropy))
```

```txt
%1 = 4.54978213
```



## Perl


```Perl
#!/usr/bin/perl
use strict ;
use warnings ;
use feature 'say' ;

sub log2 {
   my $number = shift ;
   return log( $number ) / log( 2 ) ;
}

open my $fh , "<" , $ARGV[ 0 ] or die "Can't open $ARGV[ 0 ]$!\n" ;
my %frequencies ;
my $totallength = 0 ;
while ( my $line = <$fh> ) {
   chomp $line ;
   next if $line =~ /^$/ ;
   map { $frequencies{ $_ }++ } split( // , $line ) ;
   $totallength += length ( $line ) ;
}
close $fh ;
my $infocontent = 0 ;
for my $letter ( keys %frequencies ) {
   my $content = $frequencies{ $letter } / $totallength ;
   $infocontent += $content * log2( $content ) ;
}
$infocontent *= -1 ;
say "The information content of the source file is $infocontent !" ;
```

```txt
The information content of the source file is 4.6487923749222 !
```



## Perl 6

```perl6
say log(2) R/ [+] map -> \p { p * -log p }, $_.comb.Bag.values >>/>> +$_
    given slurp($*PROGRAM-NAME).comb
```

Result should be in the neighborhood of 4.9
```txt
4.89351613053006
```



## Phix

Minor edit to the [[Entropy#Phix|Entropy]] answer, if compiled assumes source code is in the same directory.

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
            symbols  = append(symbols,si)
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

?entropy(get_text(open(substitute(command_line()[2],".exe",".exw")),"rb"))
```

```txt

4.993666233

```



## Python

Minor edit to the [[Entropy#Python|Entropy]] answer.


```Python
import math
from collections import Counter

def entropy(s):
    p, lns = Counter(s), float(len(s))
    return -sum( count/lns * math.log(count/lns, 2) for count in p.values())

with open(__file__) as f:
    b=f.read()

print(entropy(b))
```

```txt
4.575438063744619
```



## Racket

The entropy of the program below is 4.512678555350348.

```racket

#lang racket
(require math)
(define (log2 x) (/ (log x) (log 2)))
(define ds (string->list (file->string "entropy.rkt")))
(define n (length ds))
(- (for/sum ([(d c) (in-hash (samples->hash ds))])
     (* (/ c n) (log2 (/ c n)))))

```



## REXX

REXX doesn't have a BIF (built-in function) for   '''log'''   or   '''ln''',   so the subroutine (function)   '''log2'''   is included herein.

```rexx
/*REXX program calculates the   "information entropy"   for  ~this~  REXX program.      */
numeric digits 50                                /*use 50 decimal digits for precision. */
#=0; @.=0; $=; $$=; recs=sourceline()            /*define some handy─dandy REXX vars.   */

              do m=1  for recs                   /* [↓]  obtain program source and ──► $*/
              $=$ || sourceline(m)               /*get a sourceLine of this REXX program*/
              end   /*m*/                        /* [↑]  $ str won't have any meta chars*/
L=length($)                                      /*the byte length of this REXX program.*/

      do j=1  for L;  _=substr($,j,1)            /*process each character in  $  string.*/
      if @._==0  then do;  #=#+1                 /*¿Character unique?  Bump char counter*/
                           $$=$$ || _            /*add this character to the  $$  list. */
                      end
      @._=@._+1                                  /*keep track of this character's count.*/
      end   /*j*/                                /* [↑]  characters are all 8─bit bytes.*/
sum=0                                            /*calculate info entropy for each char.*/
      do i=1  for #;  _=substr($$,i,1)           /*obtain a character from unique list. */
      sum=sum  -  @._/L  * log2(@._/L)           /*add {negatively} the char entropies. */
      end   /*i*/

say '    program length: '   L                   /*pgm length doesn't include meta chars*/
say 'program statements: '   recs                /*pgm statements are actually pgm lines*/
say ' unique characters: '   #;   say            /*characters are 8─bit bytes of the pgm*/
say 'The information entropy of this REXX program ──► '       format(sum,,12)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
log2: procedure;  parse arg x 1 ox;     ig= x>1.5;     ii=0;          is=1 -  2 * (ig\==1)
      numeric digits digits()+5                  /* [↓]  precision of E must be≥digits()*/
      e=2.71828182845904523536028747135266249775724709369995957496696762772407663035354759
           do  while  ig & ox>1.5 | \ig&ox<.5;      _=e;         do j=-1;  iz=ox* _**-is
           if j>=0 & (ig & iz<1 | \ig&iz>.5)  then leave;     _=_*_;   izz=iz;  end  /*j*/
           ox=izz;  ii=ii+is*2**j;  end;           x=x* e**-ii-1;  z=0;   _=-1;    p=z
               do k=1;   _=-_*x;   z=z+_/k;        if z=p  then leave;  p=z;    end  /*k*/
           r=z+ii;  if arg()==2  then return r;    return r/log2(2,.)
```

'''output'''

```txt

    program length:  2612
program statements:  34
 unique characters:  78

The information entropy of this REXX program ──►  4.284631866395

```



## Ruby


```ruby
def entropy(s)
  counts = s.each_char.with_object(Hash.new(0.0)) {|c,h| h[c] += 1}
  counts.values.reduce(0) do |entropy, count|
    freq = count / s.size
    entropy - freq * Math.log2(freq)
  end
end
s = File.read(__FILE__)
p entropy(s)
```


```txt

4.885234973253878

```



## Rust


```Rust
use std::fs::File;
use std::io::{Read, BufReader};

fn entropy<I: IntoIterator<Item = u8>>(iter: I) -> f32 {
    let mut histogram = [0u64; 256];
    let mut len = 0u64;

    for b in iter {
        histogram[b as usize] += 1;
        len += 1;
    }

    histogram
        .iter()
        .cloned()
        .filter(|&h| h > 0)
        .map(|h| h as f32 / len as f32)
        .map(|ratio| -ratio * ratio.log2())
        .sum()
}

fn main() {
    let name = std::env::args().nth(0).expect("Could not get program name.");
    let file = BufReader::new(File::open(name).expect("Could not read file."));
    println!("Entropy is {}.", entropy(file.bytes().flatten()));
}
```

```txt
Entropy is 5.7108583.
```



## Sidef


```ruby
func entropy(s) {
    [0,
        s.chars.freq.values.map {|c|
            var f = c/s.len
            f * f.log2
        }...
    ]«-»
}

say entropy(File(__FILE__).open_r.slurp)
```

```txt

4.27307750866434915713432109186549

```



## Tcl

Note that this code doesn't bother to close the open handle on the script; it is only suitable as a demonstration program.

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

puts [format "entropy = %.5f" [entropy [read [open [info script]]]]]
```

```txt

entropy = 4.59099

```



## zkl

Minor edit to the [[Entropy#zkl|Entropy]] answer.

```zkl
fcn entropy(text){
   text.pump(Void,fcn(c,freq){ c=c.toAsc(); freq[c]=freq[c]+1; freq }
       .fp1((0).pump(256,List,(0.0).create.fp(0)).copy()))
   .filter()		      // remove all zero entries
   .apply('/(text.len()))     // (num of char)/len
   .apply(fcn(p){-p*p.log()}) // |p*ln(p)|
   .sum(0.0)/(2.0).log();     // sum * ln(e)/ln(2) to convert to log2
}

entropy(File("entropy.zkl").read().text).println();
```

```txt

4.8422

```

