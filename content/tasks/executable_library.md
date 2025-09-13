+++
title = "Executable library"
description = ""
date = 2019-10-18T15:35:16Z
aliases = []
[extra]
id = 9371
[taxonomies]
categories = ["task"]
tags = []
+++

The general idea behind an executable library is to create a library
that when used as a library does one thing;
but has the ability to be run directly via command line.
Thus the API comes with a CLI in the very same source code file.

'''Task detail'''

* Create a library/module/dll/shared object/... for a programming language that contains a function/method called hailstone that is a function taking a positive integer and returns the [[Hailstone sequence]] for that number.

* The library, when executed directly should satisfy the remaining requirements of the [[Hailstone sequence]] task:
:: 2.  Use the routine to show that the hailstone sequence for the number 27 has 112 elements starting with 27, 82, 41, 124 and ending with 8, 4, 2, 1
:: 3.  Show the number less than 100,000 which has the longest hailstone sequence together with that sequence's length.

* Create a second executable to calculate the following:
** Use the library's hailstone function, in the standard manner, (or document how this use deviates from standard use of a library), together with extra code in this executable, to find the hailstone length returned most often for 1 ≤ n < 100,000.

* Explain any extra setup/run steps needed to complete the task.

'''Notes:'''
* It is assumed that for a language that overwhelmingly ships in a compiled form, such as C, the library must also be an executable and the compiled user of that library is to do so without changing the compiled library. I.e. the compile tool-chain is assumed ''not'' to be present in the runtime environment.
* Interpreters are present in the runtime environment.


## Ada


In Ada, '''any parameterless procedure''' can either '''run as a (stand-alone) main program''', or can '''be called from another program''' like a library function.
For the task at hand, this appears useful -- except for the following two obstacles:

1. There are neither ingoing parameters into a parameterless procedure, nor is there a return value.

2. The procedure does not know how it has been called: is it running as a main program, or has it been called from another program?

To overcome the first obstacle, we implement a very simplistic parameter passing mechanism in a package Parameter (''parameter.ads''): The global variable Parameter.X will hold the ingoing parameter, the other global variable Parameter.Y will take the return value. To overcome the second obstacle, we ensure that Parameter.X is 0 by default.


```Ada
package Parameter is
   X: Natural := 0;
   Y: Natural;
end Parameter;
```


Now comes our parameterless procedure Hailstone (''hailstone.adb''). Note that we are
using the the package Hailstones (''hailstones.adb/hailstones.ads'') from
[[Hailstone sequence#Alternative method]] to perform the real computation.


```Ada
with Ada.Text_IO, Parameter, Hailstones;

procedure Hailstone is
   -- if Parameter.X > 0, the length of Hailstone(Parameter.X)
   -- is computed and written into Parameter.Y

   -- if Parameter.X = 0, Hailstone(27) and N <= 100_000 with maximal
   -- Hailstone(N) are computed and printed.

   procedure Show_Sequence(N: Natural) is
      Seq: Hailstones.Integer_Sequence := Hailstones.Create_Sequence(N);
   begin
      Ada.Text_IO.Put("Hailstone(" & Integer'Image(N) & " ) = (");
      if Seq'Length < 8 then
         for I in Seq'First .. Seq'Last-1 loop
            Ada.Text_IO.Put(Integer'Image(Seq(I)) & ",");
         end loop;
      else
         for I in Seq'First .. Seq'First+3 loop
            Ada.Text_IO.Put(Integer'Image(Seq(I)) & ",");
         end loop;
         Ada.Text_IO.Put(" ...,");
         for I in Seq'Last-3 .. Seq'Last-1 loop
            Ada.Text_IO.Put(Integer'Image(Seq(I)) &",");
         end loop;
      end if;
      Ada.Text_IO.Put_Line(Integer'Image(Seq(Seq'Last)) & " ); Length: " &
                             Integer'Image(seq'Length));
   end Show_Sequence;
begin
   if Parameter.X>0 then
      Parameter.Y := Hailstones.Create_Sequence(Parameter.X)'Length;
   else
      Show_Sequence(27);
      declare
         Longest: Natural := 0;
         Longest_Length: Natural := 0;
      begin
         for I in 2 .. 100_000 loop
            if Hailstones.Create_Sequence(I)'Length > Longest_Length then
               Longest := I;
               Longest_Length :=  Hailstones.Create_Sequence(I)'Length;
            end if;
         end loop;
         Ada.Text_IO.Put("Longest<=100_000: ");
         Show_Sequence(Longest);
      end;
   end if;
end Hailstone;
```


If we compile this and run it, we get the following output:

```txt
> ./hailstone
Hailstone( 27 ) = ( 27, 82, 41, 124, ..., 8, 4, 2, 1 ); Length:  112
Longest<=100_000: Hailstone( 77031 ) = ( 77031, 231094, 115547, 346642, ..., 8, 4, 2, 1 ); Length:  351
```


To use the same procedure like a library function, we need a specification (file ''hailstone.ads''),
that essentially repeats the parameter profile. As our procedure is actually parameterless, this specification is more than trivial.


```Ada>procedure Hailstone;</lang


Finally, we write another parameterless procedure (''hailstone_test.adb''), that will call the procedure Hailstone. Note that we '''must''' change the Parameter.X to a value > 0 before calling Hailstone, otherwise, Hailstone would act as if it where the main program.


```Ada
with Hailstone, Parameter, Ada.Text_IO;

procedure Hailstone_Test is
   Counts: array (1 .. 100_000) of Natural := (others => 0);
   Max_Count: Natural := 0;
   Most_Common: Positive := Counts'First;
   Length: Natural renames Parameter.Y;
   Sample: Natural := 0;
begin
   for I in Counts'Range loop
      Parameter.X := I;
      Hailstone; -- compute the length of Hailstone(I)
      Counts(Length) := Counts(Length)+1;
   end loop;
   for I in Counts'Range loop
      if Counts(I) > Max_Count then
         Max_Count := Counts(I);
         Most_Common := I;
      end if;
   end loop;
   Ada.Text_IO.Put_Line("Most frequent length:"
                          & Integer'Image(Most_Common)
                          & ";" & Integer'Image(Max_Count)
                          & " sequences of that length.");
   for I in Counts'Range loop
       Parameter.X := I;
       Hailstone; -- compute the length of Hailstone(I)
       if Length = Most_Common then
          Sample := I;
          exit;
       end if;
   end loop;
   Ada.Text_IO.Put_Line("The first such sequence: Hailstone("
                          & Integer'Image(Sample) & " ).");
end Hailstone_Test;
```
.

Compiling and running this gives the following output:

```txt
> ./hailstone_test
Most frequent length: 72; 1467 sequences of that length.
The first such sequence: Hailstone( 444 ).
```


Note that using global variables for parameter and return value passing works here, but is bad programming practice. Ada is a compiled language, and it is not clear how useful an executable library written in a compiled language is, anyway.

In fact, except for the constraints imposed by this task, there is no reason to ask the procedure Hailstone for the length of a Hailstone sequence -- solid software engineering practice would require to directly call the parameterized function Hailstones.Create_Sequence.


## AutoHotkey

First we create the library, hailstone.ahk:

```AHK
#NoEnv
SetBatchLines, -1

; Check if we're executed directly:
If (A_LineFile = A_ScriptFullPath){
	h27 := hailstone(27)
	MsgBox % "Length of hailstone 27: " (m := h27.MaxIndex()) "`nStarts with "
		. h27[1] ", " h27[2] ", " h27[3] ", " h27[4]
		. "`nEnds with "
		. h27[m-3] ", " h27[m-2] ", " h27[m-1] ", " h27[m]

	Loop 100000
	{
		h := hailstone(A_Index)
		If (h.MaxIndex() > m)
			m := h.MaxIndex(), longest := A_Index
	}
	MsgBox % "Longest hailstone is that of " longest " with a length of " m "!"
}


hailstone(n){
	out := [n]
	Loop
		n := n & 1 ? n*3+1 : n//2, out.insert(n)
	until n=1
	return out
}
```

Running this directly gives the output:

```txt
Length of hailstone 27: 112
Starts with 27, 82, 41, 124
Ends with 8, 4, 2, 1

Longest hailstone is that of 77031 with a length of 351!
```


Then we can create a file (test.ahk) that uses the library (note the #Include line):

```AHK
#NoEnv
#Include %A_ScriptDir%\hailstone.ahk
SetBatchLines -1

col := Object(), highestCount := 0

Loop 100000
{
	length := hailstone(A_Index).MaxIndex()
	if not col[length]
		col[length] := 0
	col[length]++
}
for length, count in col
	if (count > highestCount)
		highestCount := count, highestN := length
MsgBox % "the most common length was " highestN "; it occurred " highestCount " times."
```

Running this '''does not''' trigger the output of the hailstone.ahk,
instead it outputs this:

```txt
the most common length was 72; it occurred 1467 times.
```

[[Link title]]


## BBC BASIC

To meet the terms of this task the BBC BASIC run-time engine '''bbcwrun.exe''' must be installed on the target PC and the file extension '''.bbc''' must be associated with this executable.
This is normally the case when ''BBC BASIC for Windows'' has been installed.


### Library

This must be saved as the file HAILSTONE.BBC.
It may be used as a library (see below) or executed directly.

```bbcbasic
      seqlen% = FNhailstone(27)
      PRINT "Sequence length for 27 is "; seqlen%
      maxlen% = 0
      FOR number% = 2 TO 100000
        seqlen% = FNhailstone(number%)
        IF seqlen% > maxlen% THEN
          maxlen% = seqlen%
          maxnum% = number%
        ENDIF
      NEXT
      PRINT "The number with the longest hailstone sequence is " ; maxnum%
      PRINT "Its sequence length is " ; maxlen%
      END

      DEF FNhailstone(N%)
      LOCAL L%
      WHILE N% <> 1
        IF N% AND 1 THEN N% = 3 * N% + 1 ELSE N% DIV= 2
        L% += 1
      ENDWHILE
      = L% + 1
```

```txt

Sequence length for 27 is 112
The number with the longest hailstone sequence is 77031
Its sequence length is 351

```



### Client

This uses the above program as a library:

```bbcbasic
      INSTALL "HAILSTONE"

      DIM freq%(351)
      FOR number% = 2 TO 100000
        seqlen% = FNhailstone(number%)
        freq%(seqlen%) += 1
      NEXT
      max% = 0
      FOR i% = 0 TO 351
        IF freq%(i%) > max% THEN
          max% = freq%(i%)
          mostcommon% = i%
        ENDIF
      NEXT

      PRINT "The most common sequence length is " ; mostcommon%
      PRINT "It occurs " ; max% " times"
      END
```

```txt

The most common sequence length is 72
It occurs 1467 times

```



## C

Solution for Linux/GCC. First, header file hailstone.h:

```C
#ifndef HAILSTONE
#define HAILSTONE

long hailstone(long, long**);
void free_sequence(long *);

#endif/*HAILSTONE*/
```

Then the lib source code hailstone.c (actual name doesn't matter):

```c
#include <stdio.h>
#include <stdlib.h>

long hailstone(long n, long **seq)
{
        long len = 0, buf_len = 4;
        if (seq)
                *seq = malloc(sizeof(long) * buf_len);

        while (1) {
                if (seq) {
                        if (len >= buf_len) {
                                buf_len *= 2;
                                *seq = realloc(*seq, sizeof(long) * buf_len);
                        }
                        (*seq)[len] = n;
                }
                len ++;
                if (n == 1) break;
                if (n & 1) n = 3 * n + 1;
                else n >>= 1;
        }
        return len;
}

void free_sequence(long * s) { free(s); }

const char my_interp[] __attribute__((section(".interp"))) = "/lib/ld-linux.so.2";
/* "ld-linux.so.2" should be whatever you use on your platform */

int hail_main() /* entry point when running along, see compiler command line */
{
        long i, *seq;

        long len = hailstone(27, &seq);
        printf("27 has %ld numbers in sequence:\n", len);
        for (i = 0; i < len; i++) {
                printf("%ld ", seq[i]);
        }
        printf("\n");
        free_sequence(seq);

        exit(0);
}
```

A program to use the lib (I call it test.c):

```c
#include <stdio.h>
#include "hailstone.h"

int main()
{
        long i, longest, longest_i, len;

        longest = 0;
        for (i = 1; i < 100000; i++) {
                len = hailstone(i, 0);
                if (len > longest) {
                        longest_i = i;
                        longest = len;
                }
        }

        printf("Longest sequence at %ld, length %ld\n", longest_i, longest);

        return 0;
}
```


Building the lib: <code>gcc -Wall -W -fPIC -shared -o libhail.so hailstone.c -lc -Wl,-e,hail_main</code>

Building the test.c code: <code>gcc -Wall -L. -lhail test.c -o hailtest</code>

Running the lib:
```txt
% ./libhail.so
27 has 112 numbers in sequence:
27 82 41 124 62 31 94 47 142 71 214 107 322 161 484 242 121 364 182 91 274....

```


Running the program:
```txt
% LD_LIBRARY_PATH=. ./hailtest
Longest sequence at 77031, length 351

```


For a serious library the <code>libhail.so</code> would have been put into a system lib dir, but for now we'll just leave it in the same directory, so to run the program, we need to give additional hints to tell it where to find the lib: <code>LD_LIBRARY_PATH=. ./hailtest</code>


## Clojure

In a project's source directory, you might have a subfolder named <code>rosetta_code</code> with two Clojure source files:

```txt

.
├── clojure.jar
└── rosetta_code
    ├── frequent_hailstone_lengths.clj
    └── hailstone_sequence.clj

```

The file <code>hailstone_sequence.clj</code> is an executable library:

```clojure
(ns rosetta-code.hailstone-sequence)

(defn next-in-hailstone
  "Returns the next number in the Hailstone sequence that starts with x.
  If x is less than 2, returns nil."
  [x]
  (when (> x 1)
    (if (even? x)
      (/ x 2)
      (inc (* 3 x)))))

(defn hailstone-seq
  "Returns a lazy Hailstone sequence starting with the number x."
  [x]
  (take-while some?
              (iterate next-in-hailstone x)))

(defn -main [& args]
  (let [h27 (hailstone-seq 27)]
    (printf "The Hailstone sequence starting at 27 contains %s elements:\n%s ... %s.\n"
            (count h27)
            (vec (take 4 h27))
            (vec (take-last 4 h27)))
    (let [[number length] (apply max-key second
                                 (map (fn [x] [x (count (hailstone-seq x))])
                                      (range 100000)))]
      (printf "The number %s has the longest Hailstone sequence under 100000, of length %s.\n"
              number length))))
```

You can run it from the command line (<code>clojure.jar</code> is [https://clojure.org/community/downloads the jar that contains the Clojure language]):

```txt
$ java -cp clojure.jar clojure.main -m rosetta-code.hailstone-sequence
The Hailstone sequence starting at 27 contains 112 elements:
[27 82 41 124] ... [8 4 2 1].
The number 77031 has the longest Hailstone sequence under 100000, of length 351.

```

You can also use its functions from other programs. The file <code>frequent_hailstone_lengths.clj</code>:

```clojure
(ns rosetta-code.frequent-hailstone-lengths
  (:require [rosetta-code.hailstone-sequence
             :refer [hailstone-seq]]))

(defn -main [& args]
  (let [frequencies (apply merge-with +
                           (for [x (range 1 100000)]
                             {(count (hailstone-seq x)) 1}))
        [most-frequent-length frequency]
        (apply max-key val (seq frequencies))]
    (printf (str "The most frequent Hailstone sequence length for numbers under 100000 is %s,"
                 " with a frequency of %s.\n")
            most-frequent-length frequency)))
```

You can run it from the command line:

```txt
$ java -cp clojure.jar clojure.main -m rosetta-code.frequent-hailstone-lengths
The most frequent Hailstone sequence length for numbers under 100000 is 72, with a frequency of 1467.
```

Clojure also supports ahead-of-time compilation to class files, and the standard packaging tools [https://leiningen.org/ Leiningen] and [http://boot-clj.com/ Boot] provide meta-commands to compile and package Clojure programs into stand-alone runnable jars. At this point, Clojure has more or less the same executable library story as Java (class files can be named as the main entry point, and they can be included by other classes that import them).

=={{header|Déjà Vu}}==

The library, named <code>hailstone.deja</code>:

```dejavu
local hailstone:
	swap [ over ]
	while < 1 dup:
		if % over 2:
			#odd
			++ * 3
		else:
			#even
			/ swap 2
		swap push-through rot dup
	drop

if = (name) :(main):
	local :h27 hailstone 27
	!. = 112 len h27
	!. = 27 h27! 0
	!. = 82 h27! 1
	!. = 41 h27! 2
	!. = 124 h27! 3
	!. = 8 h27! 108
	!. = 4 h27! 109
	!. = 2 h27! 110
	!. = 1 h27! 111

	local :max 0
	local :maxlen 0
	for i range 1 99999:
		dup len hailstone i
		if < maxlen:
			set :maxlen
			set :max i
		else:
			drop
	!print( "number: " to-str max ", length: " to-str maxlen )
else:
	@hailstone
```


The client:

```dejavu
!import!hailstone

local :counts {}
set-default counts 0
for i range 1 99999:
	set-to counts swap ++ counts! dup len hailstone i

local :maxlen 0
for k in keys counts:
	if < maxlen counts! k:
		set :maxlen counts! k
!print( "Maximum length: " to-str maxlen )

```



## Factor

An ''executable library'' is a vocabulary with a main entry point.

This vocabulary, ''rosetta.hailstone'', exports the word ''hailstone'', but also uses ''MAIN:'' to declare a main entry point.


```factor
! rosetta/hailstone/hailstone.factor
USING: arrays io kernel math math.ranges prettyprint sequences vectors ;
IN: rosetta.hailstone

: hailstone ( n -- seq )
    [ 1vector ] keep
    [ dup 1 number= ]
    [
        dup even? [ 2 / ] [ 3 * 1 + ] if
        2dup swap push
    ] until
    drop ;

<PRIVATE
: main ( -- )
    27 hailstone dup dup
    "The hailstone sequence from 27:" print
    "  has length " write length .
    "  starts with " write 4 head [ unparse ] map ", " join print
    "  ends with " write 4 tail* [ unparse ] map ", " join print

    ! Maps n => { length n }, and reduces to longest Hailstone sequence.
    1 100000 [a,b)
    [ [ hailstone length ] keep 2array ]
    [ [ [ first ] bi@ > ] most ] map-reduce
    first2
    "The hailstone sequence from " write pprint
    " has length " write pprint "." print ;
PRIVATE>

MAIN: main
```


There are two ways to run this program:

* Inside Factor, from its listener: <code>"rosetta.hailstone" run</code>
* Outside Factor, from some shell: <code>./factor -run=rosetta.hailstone</code>


```txt
$ ./factor -run=rosetta.hailstone
Loading resource:work/rosetta/hailstone/hailstone.factor
The hailstone sequence from 27:
  has length 112
  starts with 27, 82, 41, 124
  ends with 8, 4, 2, 1
The hailstone sequence from 77031 has length 351.
```


Any other Factor program can also use ''rosetta.hailstone'' as a regular vocabulary. This program only uses the word ''hailstone'' from that vocabulary, and never calls the main entry point of ''rosetta.hailstone''.


```factor
! rosetta/hailstone/length/length.factor
USING: assocs kernel io math math.ranges prettyprint
       rosetta.hailstone sequences ;
IN: rosetta.hailstone.length

<PRIVATE
: f>0 ( object/f -- object/0 )
    dup [ drop 0 ] unless ;

: max-value ( pair1 pair2 -- pair )
    [ [ second ] bi@ > ] most ;

: main ( -- )
    H{ } clone      ! Maps sequence length => count.
    1 100000 [a,b) [
        hailstone length                ! Find sequence length.
        over [ f>0 1 + ] change-at      ! Add 1 to count.
    ] each
    ! Find the length-count pair with the highest count.
    >alist unclip-slice [ max-value ] reduce
    first2 swap
    "Among Hailstone sequences from 1 <= n < 100000," print
    "there are " write pprint
    " sequences of length " write pprint "." print ;
PRIVATE>

MAIN: main
```



```txt
$ ./factor -run=rosetta.hailstone.length
Loading resource:work/rosetta/hailstone/length/length.factor
Loading resource:work/rosetta/hailstone/hailstone.factor
Among Hailstone sequences from 1 <= n < 100000,
there are 72 sequences of length 1467.
```



## Go

To be directly executable, a Go application must contain a package called 'main' which includes a main() function from which execution begins automatically. Any other application is regarded as a library.

However, a package can consist of not just one but several source code files contained in the same directory.

Consequently, Go can emulate an executable library by splitting package 'main' into two files:

1. A file including the library code which includes (say) a libMain() function but no main() function as such.

2. A file including a main() function which calls libMain().

If these two files are 'run' as a package, then they can mimic executing the library code directly.

To use the library from another executable application, all that is needed is to copy the first file to the application's directory and then 'run' the files as a package. Care would be needed to ensure that there are no name clashes in the 'combined' package.

Note that, although the 'go run' command may look like it's launching an interpreter, it does in fact build an application from the package files in a temporary directory and then executes it in a single step.

To complete the task, first create these two files in the 'modulino' directory:


```go
// modulino.go
package main

import "fmt"

// Function borrowed from Hailstone sequence task.
// 1st arg is the number to generate the sequence for.
// 2nd arg is a slice to recycle, to reduce garbage.
func hailstone(n int, recycle []int) []int {
    s := append(recycle[:0], n)
    for n > 1 {
        if n&1 == 0 {
            n = n / 2
        } else {
            n = 3*n + 1
        }
        s = append(s, n)
    }
    return s
}

func libMain() {
    seq := hailstone(27, nil)
    fmt.Println("\nHailstone sequence for the number 27:")
    fmt.Println("  has", len(seq), "elements")
    fmt.Println("  starts with", seq[0:4])
    fmt.Println("  ends with", seq[len(seq)-4:])

    var longest, length int
    for i := 1; i < 100000; i++ {
        if le := len(hailstone(i, nil)); le > length {
            longest = i
            length = le
        }
    }
    fmt.Printf("\n%d has the longest Hailstone sequence, its length being %d.\n", longest, length)
}
```



```go
// modulino_main.go
package main

func main() {
    libMain()
}
```


To emulate an executable library:
```txt

$ go run modulino

Hailstone sequence for the number 27:
  has 112 elements
  starts with [27 82 41 124]
  ends with [8 4 2 1]

77031 has the longest Hailstone sequence, its length being 351.

```


Now create this file in the 'hailstone' directory:


```go
// hailstone.go
package main

import "fmt"

func main() {
    freq := make(map[int]int)
    for i := 1; i < 100000; i++ {
        freq[len(hailstone(i, nil))]++
    }
    var mk, mv int
    for k, v := range freq {
        if v > mv {
            mk = k
            mv = v
        }
    }
    fmt.Printf("\nThe Hailstone length returned most is %d, which occurs %d times.\n", mk, mv)
}
```


and copy modulino.go to the 'hailstone' directory. The library can then be used in the 'normal' way:
```txt

$ go run hailstone

The Hailstone length returned most is 72, which occurs 1467 times.

```



## Io


'''HailStone.io'''

```io
HailStone := Object clone
HailStone sequence := method(n,
    if(n < 1, Exception raise("hailstone: expect n >= 1 not #{n}" interpolate))
    n = n floor         // make sure integer value
    stones := list(n)
    while (n != 1,
        n = if(n isEven, n/2, 3*n + 1)
        stones append(n)
    )
    stones
)

if( isLaunchScript,
    out := HailStone sequence(27)
    writeln("hailstone(27) has length ",out size,": ",
        out slice(0,4) join(" ")," ... ",out slice(-4) join(" "))

    maxSize := 0
    maxN := 0
    for(n, 1, 100000-1,
        out = HailStone sequence(n)
        if(out size > maxSize,
            maxSize = out size
            maxN = n
        )
    )

    writeln("For numbers < 100,000, ", maxN,
    " has the longest sequence of ", maxSize, " elements.")
)
```

'''client.io'''

```io
counts := Map clone
for(n, 1, 100000-1,
    out := HailStone sequence(n)
    key := out size asCharacter
    counts atPut(key, counts atIfAbsentPut(key, 0) + 1)
)

maxCount := counts values max
lengths := list()
counts foreach(k,v,
    if(v == maxCount, lengths append(k at(0)))
)

if(lengths size == 1,
    writeln("The most frequent sequence length for n < 100,000 is ",lengths at(0),
        " occurring ",maxCount," times.")
,
    writeln("The most frequent sequence lengths for n < 100,000 are:\n",
        lengths join(",")," occurring ",maxCount," times each.")
)
```

'''Terminal Session'''

```txt
$ io HailStone.io
hailstone(27) has length 112: 27 82 41 124 ... 8 4 2 1
For numbers < 100,000, 77031 has the longest sequence of 351 elements.
$ io client.io
The most frequent sequence length for n < 100,000 is 72 occurring 1467 times.
$
```

Io has a built-in auto importer that will attempt to automatically
import a file with the same name (plus "io" extension) as an undefined
prototype object when it is first referenced.  To be imported the first letter of the
prototype name must also be upper case which by convention indicates a
prototype that has the same role as a class in class based object-oriented
languages. The Importer's default search path is the current working
directory, but you can add search paths using <code>Importer addSearchPath()</code>.


## J


This is the executable library:


```j
hailseq=: -:`(1 3&p.)@.(2&|) ^:(1 ~: ]) ^:a:"0
9!:29]1
9!:27'main 0'
main=:3 :0
  smoutput 'Hailstone sequence for the number 27'
  smoutput hailseq 27
  smoutput ''
  smoutput 'Finding number with longest hailstone sequence which is'
  smoutput 'less than 100000 (and finding that sequence length):'
  smoutput (I.@(= >./),>./) #@hailseq i.1e5
)
```


Running it might look like this:


```j
   load jpath '~temp/hailseq.ijs'
Hailstone sequence for the number 27
27 82 41 124 62 31 94 47 142 71 214 107 322 161 484 242 121 364 182 91 274 137 412 206 103 310 155 466 233 700 350 175 526 263 790 395 1186 593 1780 890 445 1336 668 334 167 502 251 754 377 1132 566 283 850 425 1276 638 319 958 479 1438 719 2158 1079 3238 ...
Finding number with longest hailstone sequence which is
less than 100000 (and finding that sequence length):
77031 351
```


This is the program which uses the library part of that executable library:


```j
require '~temp/hailseq.ijs'
9!:29]1
9!:27'main 0'
main=:3 :0
  smoutput 'Finding most frequent hailstone sequence length for'
  smoutput 'Hailstone sequences for whole numbers less than 100000:'
  smoutput  {:{.\:~ (#/.~,.~.) #@hailseq }.i.1e5
)

```


Running it might look like this:

```j
   load jpath '~temp/66.ijs'
Finding most frequent hailstone sequence length for
Hailstone sequences for whole numbers less than 100000
72
```


Notes:  <code>9!:29]1</code>  tells the interpeter to run a phrase.  <code>9!:27'phrase'</code>  tells the interpeter the phrase to execute.  (<code>9!:</code> means, in essence: standard library number 9, and <code>9!:29</code> identifies a specific entry point in that library.)
In 66.ijs we can not use the presence of <code>9!:29]1</code> from <code>hailseq.ijs</code> because hailseq.ijs was loaded with require which means that if it had already been loaded it will not be loaded again.
(And, <code>66</code> here is just an arbitrary temporary file name.)


## Julia

A Julia module can check to see if it is also the file run from the command lime by checking the PROGRAM_NAME variable.

```julia

############### in file hailstone.jl ###############
module Hailstone

function hailstone(n)
    ret = [n]
    while n > 1
        if n & 1 > 0
            n = 3n + 1
        else
            n = Int(n//2)
        end
        append!(ret, n)
    end
    return ret
end

export hailstone

end

if PROGRAM_FILE == "hailstone.jl"
    using Hailstone
    h = hailstone(27)
    n = length(h)
    println("The sequence of hailstone(27) is:\n $h.\nThis sequence is of length $n. It starts with $(h[1:4]) and ends with $(h[n-3:end]).")
end
############ in file moduletest.jl ####################
include("hailstone.jl")
using Hailstone
function countstones(mi, mx)
    lengths2occurences = Dict()
    mostfreq = mi
    maxcount = 1
    for i in mi:mx
        h = hailstone(i)
        n = length(h)
        if haskey(lengths2occurences, n)
            newoccurences = lengths2occurences[n] + 1
            if newoccurences > maxcount
                maxcount = newoccurences
                mostfreq = n
            end
            lengths2occurences[n] = newoccurences
        else
            lengths2occurences[n] = 1
        end
    end
    mostfreq, maxcount
end

nlen, cnt = countstones(1,99999)

print("The most common hailstone sequence length for hailstone(n) for 1 <= n < 100000 is $nlen, which occurs $cnt times.")

```

```txt

Running hailstone.jl standalone:
The sequence of hailstone(27) is:
 [27, 82, 41, 124, 62, 31, 94, 47, 142, 71, 214, 107, 322, 161, 484, 242, 121, 364, 182, 91, 274, 137, 412, 206, 103, 310, 155, 466, 233, 700, 350, 175, 526, 263, 790, 395, 1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502, 251, 754, 377, 1132, 566, 283, 850, 425, 1276, 638, 319, 958, 479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429, 7288, 3644, 1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232, 4616, 2308, 1154, 577, 1732, 866, 433, 1300, 650, 325, 976, 488, 244, 122, 61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1].
This sequence is of length 112. It starts with [27, 82, 41, 124] and ends with [8, 4, 2, 1].
Running the program that uses the Hailstone module:
The most common hailstone sequence length for hailstone(n) for 1 <= n < 100000 is 72, which occurs 1467 times.

```



## Limbo


There's no real difference in compilation or output
for libraries versus commands in Inferno;
commands (by convention) are expected to define an <code>init()</code> function that accepts a reference to a graphical context and a list of strings (i.e., the argument list) in order to satisy the type-checker.
So this task is fairly simple.
First, <code>execlib.b</code> looks like this:


```Limbo
implement Execlib;

include "sys.m"; sys: Sys;
include "draw.m";

Execlib: module {
	init: fn(ctxt: ref Draw->Context, args: list of string);
	hailstone: fn(i: big): list of big;
};

init(nil: ref Draw->Context, nil: list of string)
{
	sys = load Sys Sys->PATH;

	seq := hailstone(big 27);
	l := len seq;

	sys->print("hailstone(27):  ");
	for(i := 0; i < 4; i++) {
		sys->print("%bd, ", hd seq);
		seq = tl seq;
	}
	sys->print("⋯");

	while(len seq > 4)
		seq = tl seq;

	while(seq != nil) {
		sys->print(", %bd", hd seq);
		seq = tl seq;
	}
	sys->print(" (length %d)\n", l);

	max := 1;
	maxn := big 1;
	for(n := big 2; n < big 100000; n++) {
		cur := len hailstone(n);
		if(cur > max) {
			max = cur;
			maxn = n;
		}
	}
	sys->print("hailstone(%bd) has length %d\n", maxn, max);
}

hailstone(i: big): list of big
{
	if(i == big 1)
		return big 1 :: nil;
	if(i % big 2 == big 0)
		return i :: hailstone(i / big 2);
	return i :: hailstone(big 3 * i + big 1);
}

```


And <code>execsexeclib.b</code> (which executes <code>execlib</code>) looks like this:


```Limbo
implement ExecsExeclib;

include "sys.m"; sys: Sys;
include "draw.m";

ExecsExeclib: module {
	init: fn(ctxt: ref Draw->Context, args: list of string);
};

# Usually, this would be placed into something like "execlib.m",
# but it's fine here.
Execlib: module {
	hailstone: fn(i: big): list of big;
};

init(nil: ref Draw->Context, nil: list of string)
{
	sys = load Sys Sys->PATH;
	# This program expects that the result of compiling Execlib is execlib.dis,
	# so you'll need to adjust this line if you used a different filename.
	lib := load Execlib "execlib.dis";
	if(lib == nil)
		die("Couldn't load execlib.dis");

	counts := array[352] of { * => 0 };
	for(i := 1; i < 100000; i++) {
		counts[len lib->hailstone(big i)]++;
	}

	max := 0;
	maxi := 0;
	for(i = 1; i < len counts; i++) {
		if(counts[i] > max) {
			max = counts[i];
			maxi = i;
		}
	}
	sys->print("The most common sequence length is %d (encountered %d times)\n", maxi, max);
}

die(s: string)
{
	sys->fprint(sys->fildes(2), "runls: %s: %r", s);
	raise "fail:errors";
}

```


```txt

% apply {limbo $1} *execlib.b
% ./execlib
hailstone(27):  27, 82, 41, 124, ⋯, 8, 4, 2, 1 (length 112)
hailstone(77031) has length 351
% ./execsexeclib
The most common sequence length is 72 (encountered 1467 times)

```



## Mathematica


### Library


```Mathematica
hailstone[1] = {1};
hailstone[n_] :=
  hailstone[n] = Prepend[hailstone[If[EvenQ[n], n/2, 3 n + 1]], n];
If[$ScriptCommandLine[[1]] == $Input,
  val = hailstone[27];
  Print["hailstone(27) starts with ", val[[;; 4]], ", ends with ",
   val[[-4 ;;]], ", and has length ", Length[val], "."];
  val = MaximalBy[Range[99999], Length@*hailstone][[1]];
  Print[val, " has the longest hailstone sequence with length ",
   Length[hailstone[val]], "."]];
```

```txt
hailstone(27) starts with {27, 82, 41, 124}, ends with {8, 4, 2, 1}, and has length 112.
77031 has the longest hailstone sequence with length 351.
```


### Client

This assumes that the library is named <tt>hailstone.m</tt>.

```Mathematica
<< hailstone.m;
Print["The most common hailstone length is ",
  Commonest[Length@*hailstone /@ Range[99999]][[1]], "."];
```

```txt
The most common hailstone length is 72.
```



## NetRexx

The NetRexx compiler can generate Java classes and in common with all Java classes, public methods within each class are available for use by other programs.  Packaging a class in a JAR file effectively crates a library that can be used by any other Java program.  If this file is constructed correctly it can also by delivered as an &quot;executable JAR file&quot; which can be launched via the <tt>-jar</tt> switch of the <tt>java</tt> command.  The following command can be used to package the [[Hailstone sequence#NetRexx|NetRexx Hailstone Sequence]] sample as an executable JAR file:

```txt
$ jar cvfe RHailstoneSequence.jar RHailstoneSequence RHailstoneSequence.class
added manifest
adding: RHailstoneSequence.class(in = 2921) (out= 1567)(deflated 46%)
```


Here, the <tt>e</tt> switch causes the <tt>jar</tt> program to add a <tt>Main-Class</tt> property to the generated jar manifest which now contains the following:

```txt

Manifest-Version: 1.0
Created-By: 1.7.0_15 (Oracle Corporation)
Main-Class: RHailstoneSequence

```


With this <tt>Main-Class</tt> property present, launching the program via <tt>java -jar</tt> will cause Java to attempt to execute the <tt>main()</tt> method of the program specified above (<tt>RHailstoneSequence</tt>):


```txt
$ java -jar RHailstoneSequence.jar
The number 27 has a hailstone sequence comprising 112 elements
  its first four elements are: 27 82 41 124
   and last four elements are: 8 4 2 1
The number 77031 has the longest hailstone sequence in the range 1 to 99999 with a sequence length of 351
```

Using this JAR file as a library, the following program can use the <tt>hailstone(N)</tt> method to complete the task:


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import RHailstoneSequence

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) private static
  parse arg beginNum endNum .
  if beginNum = '' | beginNum = '.' then beginNum = 1
  if endNum   = '' | endNum   = '.' then endNum   = 100000
  if beginNum > endNum then signal IllegalArgumentException('Gronk!')

  -- collect sequences
  hailstones = 0
  loop hn = beginNum while hn < endNum
    hslist = RHailstoneSequence.hailstone(hn)
    hscount = hslist.words()
    hailstones[hscount] = hailstones[hscount] + 1
    end hn

  -- locate most common
  mostOftenNum = 0
  mostOftenCount = 0
  loop hn = beginNum while hn < endNum
    if hailstones[hn] > mostOftenCount then do
      mostOftenCount = hailstones[hn]
      mostOftenNum   = hn
      end
    end hn

  say 'The length of hailstone sequence that is most common in the range' beginNum '<= N <' endNum 'is' mostOftenNum'. It occurs' mostOftenCount 'times.'
  return

```


The program can then be launched with the <tt>java</tt> command.  In this sample the JAR file is included via the <tt>-cp</tt> switch:
```txt
$ java -cp .:RHailstoneSequence.jar RHailstoneSequenceUser
The length of hailstone sequence that is most common in the range 1 <= N < 100000 is 72. It occurs 1467 times.
```



## Nim


```nim
proc hailstone*(n): auto =
  result = @[n]
  var n = n
  while n > 1:
    if (n and 1) == 1:
      n = 3 * n + 1
    else:
      n = n div 2
    result.add n

when isMainModule:
  let h = hailstone 27
  assert h.len == 112 and h[0..3] == @[27,82,41,124] and h[h.high-3..h.high] == @[8,4,2,1]
  var m, mi = 0
  for i in 1 .. <100_000:
    let n = hailstone(i).len
    if n > m:
      m = n
      mi = i
  echo "Maximum length ", m, " was found for hailstone(", mi, ") for numbers <100,000"
```


In Nim the value <code>isMainModule</code> is set at compiletime, and we can use it with <code>when</code> (a compiletime if).

'''Library Importing Example'''

```nim
import hailstone, tables

var t = initCountTable[int]()

for i in 1 .. <100_000:
  t.inc(hailstone(i).len)

let (val, cnt) = t.largest()
echo "The length of hailstone sequence that is most common for"
echo "hailstone(n) where 1<=n<100000, is ", val, ". It occurs ", cnt, " times."
```


```txt
The length of hailstone sequence that is most common for
hailstone(n) where 1<=n<100000, is 72. It occurs 1467 times.
```



## PARI/GP

Linux solution (i386 and x86_64).

Library file hailstone.c:

```C>#include <pari/pari.h


#define HAILSTONE1      "n=1;print1(%d,\": \");apply(x->while(x!=1,if(x/2==x\\2,x/=2,x=x*3+1);n++;print1(x,\", \")),%d);print(\"(\",n,\")\n\")"
#define HAILSTONE2      "m=n=0;for(i=2,%d,h=1;apply(x->while(x!=1,if(x/2==x\\2,x/=2,x=x*3+1);h++),i);if(m<h,m=h;n=i));print(n,\": \",m)"

void hailstone1(int x)
{
  char buf[1024];

  snprintf(buf, sizeof(buf), HAILSTONE1, x, x);

  pari_init(1000000, 2);

  geval(strtoGENstr(buf));

  pari_close();
}

void hailstone2(int range)
{
  char buf[1024];

  snprintf(buf, sizeof(buf), HAILSTONE2, range);

  pari_init(1000000, 2);

  geval(strtoGENstr(buf));

  pari_close();
}

#if __i386__
const char _hail[] __attribute__((section(".interp"))) = "/lib/ld-linux.so.2";
#else // __x86_64__
const char _hail[] __attribute__((section(".interp"))) = "/lib64/ld-linux-x86-64.so.2";
#endif

int main(void)
{
  hailstone1(27);
  hailstone2(100000);

  exit(0);
}
```

Compile hailstone.c: ''gcc -O2 -Wall hailstone.c -fPIC -shared -o libhailstone.so -lpari -Wl,-e,main''

Execute library: ''./libhailstone.so'':

```txt
27: 82, 41, 124, 62, 31, 94, 47, 142, 71, 214, 107, 322, 161, 484, 242, 121, 364, 182, 91, 274, 137, 412, 206, 103, 310, 155, 466, 233, 700, 350, 175, 526, 263, 790, 395, 1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502, 251, 754, 377, 1132, 566, 283, 850, 425, 1276, 638, 319, 958, 479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429, 7288, 3644, 1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232, 4616, 2308, 1154, 577, 1732, 866, 433, 1300, 650, 325, 976, 488, 244, 122, 61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1, (112)

77031: 351
```

----
Main program linking libhailstone.so... main.c:

```C
void hailstone1(int);

int main(void)
{
  hailstone1(27);

  return 0;
}
```

Compile main.c: ''gcc -O2 -Wall main.c -o main -L. libhailstone.so''

Execute main program: ''LD_LIBRARY_PATH=. ./main'' :

```txt
27: 82, 41, 124, 62, 31, 94, 47, 142, 71, 214, 107, 322, 161, 484, 242, 121, 364, 182, 91, 274, 137, 412, 206, 103, 310, 155, 466, 233, 700, 350, 175, 526, 263, 790, 395, 1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502, 251, 754, 377, 1132, 566, 283, 850, 425, 1276, 638, 319, 958, 479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429, 7288, 3644, 1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232, 4616, 2308, 1154, 577, 1732, 866, 433, 1300, 650, 325, 976, 488, 244, 122, 61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1, (112)
```



## Perl

Lib package in file <code>Hailstone.pm</code>:

```perl
package Hailstone;

sub seq {
	my $x = shift;
	$x == 1	? (1) : ($x & 1)? ($x, seq($x * 3 + 1))
				: ($x, seq($x / 2))
}

my %cache = (1 => 1);
sub len {
	my $x = shift;
	$cache{$x} //= 1 + (
		$x & 1	? len($x * 3 + 1)
			: len($x / 2))
}

unless (caller) {
    for (1 .. 100_000) {
        my $l = len($_);
        ($m, $len) = ($_, $l) if $l > $len;
    }
    print "seq of 27 - $cache{27} elements: @{[seq(27)]}\n";
    print "Longest sequence is for $m: $len\n";
}

1;
```

Main program in file <code>test.pl</code>:
```perl
use Hailstone;
use strict;
use warnings;

my %seqs;
for (1 .. 100_000) {
    $seqs{Hailstone::len($_)}++;
}

my ($most_frequent) = sort {$seqs{$b} <=> $seqs{$a}} keys %seqs;
print "Most frequent length: $most_frequent ($seqs{$most_frequent} occurrences)\n";
```

Running the lib:
```txt
% perl Hailstone.pm
seq of 27 - 112 elements: 27 82 41 124 62 31 94 47 142 ... 10 5 16 8 4 2 1
Longest sequence is for 77031: 351
```

Running the main program:
```txt
% perl test.pl
Most frequent length: 72 (1467 occurrences)
```



## Perl 6

The library can be written as a module:

```perl6
module Hailstone {
    our sub hailstone($n) is export {
	$n, { $_ %% 2 ?? $_ div 2 !! $_ * 3 + 1 } ... 1
    }
}

sub MAIN {
    say "hailstone(27) = {.[^4]} [...] {.[*-4 .. *-1]}" given Hailstone::hailstone 27;
}
```


It can be run with:

```shell
$ perl6 Hailstone.pm
```

```txt
hailstone(27) = 27 82 41 124 [...] 8 4 2 1
```


It can then be used with a program such as:

```perl6
use Hailstone;
my %score;
(1 .. 100_000).race.map: { %score{hailstone($_).elems}++ };
say "Most common length is {.key}, occurring {.value} times." given max :by(*.value), %score;
```


Called with a command line as:

```txt
$ PERL6LIB=. perl6 test-hailstone.p6
```


The environment variable PERL6LIB might be necessary if the file Hailstone.pm is not in the standard  library path for Perl 6.


## Phix

hail.exw:

```Phix
function isMainOrInclude()
-- returns 1 if called from the main file, 0 if from an include
integer res
    #ilASM{
        [32]
            mov eax,[ebp+20]    -- prev_ebp
            mov eax,[eax+8]     -- rtn
            mov [res],eax
        [64]
            mov rax,[rbp+40]    -- prev_ebp
            mov rax,[rax+16]    -- rtn
            mov [res],rax
        []
          }
    return res=21 -- (21=T_maintls)
end function

--global (if you want to be able to call this from test.exw)
function hailstone(atom n)
sequence s = {n}
    while n!=1 do
        if remainder(n,2)=0 then
            n /= 2
        else
            n = 3*n+1
        end if
        s &= n
    end while
    return s
end function

global function hailstone_count(atom n)
integer count = 1
    while n!=1 do
        if remainder(n,2)=0 then
            n /= 2
        else
            n = 3*n+1
        end if
        count += 1
    end while
    return count
end function

if isMainOrInclude() then

    sequence s = hailstone(27)
    integer ls = length(s)
    s[5..-5] = {".."}
    puts(1,"hailstone(27) = ")
    ? s
    printf(1,"length = %d\n\n",ls)

    integer hmax = 1, imax = 1,count
    for i=2 to 1e5-1 do
        count = hailstone_count(i)
        if count>hmax then
            hmax = count
            imax = i
        end if
    end for

    printf(1,"The longest hailstone sequence under 100,000 is %d with %d elements.\n",{imax,hmax})
end if
```

```txt

hailstone(27) = {27,82,41,124,"..",8,4,2,1}
length = 112

The longest hailstone sequence under 100,000 is 77031 with 351 elements.

```

test.exw:

```Phix
include hail.exw

sequence counts = {}
for i=1 to 100000 do
    integer l = hailstone_count(i)
    if length(counts)<l then
        counts &= repeat(0,l-length(counts))
    end if
    counts[l] += 1
end for
printf(1,"The hailstone length returned most often between 1 and 100,000 is %d.\n",{largest(counts,1)})
```

```txt

The hailstone length returned most often between 1 and 100,000 is 72.

```



## Pike

any Pike source file is a class and can be instantiated as an object.
to executable a Pike file it needs a <code>main()</code> function.

Pike modules are classes instantiated at compile time. below we demonstrate both forms:

to use the library as a class, save it as HailStone.pike
to use it as a module, save it as Hailstone.pmod

both can be used as an executable.

```Pike
#!/usr/bin/env pike

int next(int n)
{
    if (n==1)
        return 0;
    if (n%2)
        return 3*n+1;
    else
        return n/2;
}

array(int) hailstone(int n)
{
    array seq = ({ n });
    while (n=next(n))
        seq += ({ n });
    return seq;
}

void main()
{
    array(int) two = hailstone(27);
    if (equal(two[0..3], ({ 27, 82, 41, 124 })) && equal(two[<3..], ({ 8,4,2,1 })))
        write("sizeof(({ %{%d, %}, ... %{%d, %} }) == %d\n", two[0..3], two[<3..], sizeof(two));

    mapping longest = ([ "length":0, "start":0 ]);

    foreach(allocate(100000); int start; )
    {
        int length = sizeof(hailstone(start));
        if (length > longest->length)
        {
            longest->length = length;
            longest->start = start;
        }
    }
    write("longest sequence starting at %d has %d elements\n", longest->start, longest->length);
}
```


if run directly we get:
 $ pike hailstone.pike
 sizeof(({ 27, 82, 41, 124, , ... 8, 4, 2, 1,  }) == 112
 longest sequence starting at 77031 has 351 elements

to use it as a class we need to instantiate an object.
note that the . in .HailStone only signifies calling a class or module from the current directory.
the analyze function is identical in both examples:

```Pike
void main()
{
    .HailStone HailStone = .HailStone();

    mapping long = ([]);

    foreach (allocate(100000); int start; )
        long[sizeof(HailStone->hailstone(start))]++;

    analyze(long);
}

void analyze(mapping long)
{
    mapping max = ([ "count":0, "length":0 ]);
    foreach (long; int length; int count)
    {
        if (count > max->count)
        {
            max->length = length;
            max->count = count;
        }
    }
    write("most common length %d appears %d times\n", max->length, max->count);
}
```


a module is already instantiated so we can use it directly.
like above the initial . in .Hailstone.hailstone only signifies the current directory, the second . is a member reference resolved at compile time.

```Pike
void main()
{
    mapping long = ([]);

    foreach (allocate(100000); int start; )
        long[sizeof(.Hailstone.hailstone(start))]++;

    analyze(long);
}

void analyze(mapping long)
{
    mapping max = ([ "count":0, "length":0 ]);
    foreach (long; int length; int count)
    {
        if (count > max->count)
        {
            max->length = length;
            max->count = count;
        }
    }
    write("most common length %d appears %d times\n", max->length, max->count);
}
```


{{out}} for both examples:

```txt

 most common length 72 appears 1467 times

```



## PicoLisp

There is no formal difference between libraries and other executable files
in PicoLisp.
Any function in a library can be called from the command line
by prefixing it with '-'.
Create an executable file (chmod +x) "hailstone.l":

```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(de hailstone (N)
   (make
      (until (= 1 (link N))
         (setq N
            (if (bit? 1 N)
               (inc (* N 3))
               (/ N 2) ) ) ) ) )

(de hailtest ()
   (let L (hailstone 27)
      (test 112 (length L))
      (test (27 82 41 124) (head 4 L))
      (test (8 4 2 1) (tail 4 L)) )
   (let N (maxi '((N) (length (hailstone N))) (range 1 100000))
      (test 77031 N)
      (test 351 (length (hailstone N))) )
   (println 'OK)
   (bye) )
```

and an executable file (chmod +x) "test.l":

```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(load "hailstone.l")

(let Len NIL
   (for N 100000
      (accu 'Len (length (hailstone N)) 1) )
   (let M (maxi cdr Len)
      (prinl "The hailstone length returned most often is " (car M))
      (prinl "It is returned " (cdr M) " times") ) )
(bye)
```

Test:

```txt
$ ./hailstone.l -hailtest
OK

$ ./test.l
The hailstone length returned most often is 72
It is returned 1467 times
```



## Python

Executable libraries are common in Python. The [[Hailstone sequence#Python|Python]] entry for Hailstone sequence is already written in the correct manner.

The entry is copied below and, for this task needs to be in a file called <code>hailstone.py</code>:

```python
def hailstone(n):
    seq = [n]
    while n>1:
        n = 3*n + 1 if n & 1 else n//2
        seq.append(n)
    return seq

if __name__ == '__main__':
    h = hailstone(27)
    assert len(h)==112 and h[:4]==[27, 82, 41, 124] and h[-4:]==[8, 4, 2, 1]
    print("Maximum length %i was found for hailstone(%i) for numbers <100,000" %
          max((len(hailstone(i)), i) for i in range(1,100000)))
```


In the case of the Python language the interpreter maintains a module level variable called __name__. If the file hailstone.py is ''imported'' (as <code>import hailstone</code>), then the __name__ variable is set to the import name of 'hailstone' and the <code>if __name__ == '__main__'</code> expression would then be false, and only the hailstone function is available to the importer.

If the same file hailstone.py is ''run'', (as maybe <code>python hailstone.py</code>; or maybe double-clicking the hailstone.py file), then the __name__ variable is set to the special name of '__main__' and the <code>if __name__ == '__main__'</code> expression would then be true causing its block of code to be executed.

'''Library importing executable'''

The second executable is the file <code>common_hailstone_length.py</code> with this content:

```python
from collections import Counter

def function_length_frequency(func, hrange):
    return Counter(len(func(n)) for n in hrange).most_common()

if __name__ == '__main__':
    from executable_hailstone_library import hailstone

    upto = 100000
    hlen, freq = function_length_frequency(hailstone, range(1, upto))[0]
    print("The length of hailstone sequence that is most common for\n"
          "hailstone(n) where 1<=n<%i, is %i. It occurs %i times."
          % (upto, hlen, freq))
```


Both files could be in the same directory. (That is the easiest way to make the library known to its importer for this example)

On executing the file common_hailstone_length.py it loads the library and produces the following result:

```txt
The length of hailstone sequence that is most common for
hailstone(n) where 1<=n<100000, is 72. It occurs 1467 times
```


Note that the file common_hailstone_length.py is itself written as an executable library. When imported it makes function_length_frequency available to the importer.


### Other examples

* The Python Prime decomposition entry of [[Least common multiple]] employs [[Prime decomposition#Python]] as an executable library.
* [[Names_to_numbers#Python]] uses [[Number_names#Python]] as an executable library.


## Racket


When Racket runs a file (with <tt>racket some-file</tt>) it executes its
toplevel expressions, and then it runs a submodule named <tt>main</tt> if there
is one.  When a file is used as a library (with <tt>require</tt>), the toplevel
expressions are executed as well, but the <tt>main</tt> is <em>not</em>
executed.
The idea is that toplevel expressions might be used to initialize state
that the library needs -- a good example here is the initialization
of the memoization hash table.
(Note that this is better than the common hacks of check-the-loaded-script-name, since it is robust against failures due to symlinks, case normalization, etc etc.)

We start with a "<tt>hs.rkt</tt>" file that has the exact code from the
[[Hailstone sequence#Racket]] solution, except that the <tt>hailstone</tt>
function is now provided, and the demonstration printout is pushed into a
<tt>main</tt> submodule:

```Racket

#lang racket

(provide hailstone)
(define hailstone
  (let ([t (make-hasheq)])
    (hash-set! t 1 '(1))
    (λ(n) (hash-ref! t n
            (λ() (cons n (hailstone (if (even? n) (/ n 2) (+ (* 3 n) 1)))))))))

(module+ main
  (define h27 (hailstone 27))
  (printf "h(27) = ~s, ~s items\n"
          `(,@(take h27 4) ... ,@(take-right h27 4))
          (length h27))
  (define N 100000)
  (define longest
    (for/fold ([m #f]) ([i (in-range 1 (add1 N))])
      (define h (hailstone i))
      (if (and m (> (cdr m) (length h))) m (cons i (length h)))))
  (printf "for x<=~s, ~s has the longest sequence with ~s items\n"
          N (car longest) (cdr longest)))

```


Running it directly produces the same output as [[Hailstone sequence#Racket]]:

```txt

$ racket hs.rkt
first 4 elements of h(27): '(27 82 41 124)
last  4 elements of h(27): '(8 4 2 1)
x < 10000 such that h(x) gives the longest sequence: 351

```


And now this can be used from a second source file, "<tt>hsfreq.rkt</tt>" as a
library:

```Racket

#lang racket
(require "hs.rkt")
(define N 100000)
(define t (make-hasheq))
(define best
  (for/fold ([best #f]) ([i (in-range 1 (add1 N))])
    (define len (length (hailstone i)))
    (define freq (add1 (hash-ref t len 0)))
    (hash-set! t len freq)
    (if (and best (> (car best) freq)) best (cons freq len))))
(printf "Most frequent sequence length for x<=~s: ~s, appearing ~s times\n" N
        (cdr best) (car best))

```



```txt

$ racket hsfreq.rkt
Most frequent sequence length for x<=100000: 72, appearing 1467 times

```



## REXX


### task 1

The following REXX subroutine (or function, as it returns a value)
is normally stored in a folder that the REXX interpreter
searches first for subroutine/function call/invokes.

If not there, the REXX interpreter normally checks the
current drive (or default disk), and then through some sort of heirarchy --- depending upon the particular REXX interpreter and operating system.


On Microsoft Windows systems using Regina, PC/REXX, Personal REXX, R4, or ROO, the program name is normally the function name with a file extension of '''REX'''   (but that isn't a strict requirement or rule, each REXX interpreter has multiple file extensions that are supported).

On VM/CMS systems, the filetype (the file extension) is normally   '''EXEC'''.   If however, the REXX program was previously '''EXECLOAD'''ed, it may have a different name (identity) assigned to it.

The following program (function) is named:   '''HAILSTONE.REX'''   (the case doesn't matter for Microsoft Windows systems).

All REXX interpreters support subroutines/functions being on the current drive ('''CD'''), default disk (or MDISK in the case of CMS), or the equivalent.

```rexx
/*REXX program returns the hailstone (Collatz) sequence for any integer.*/
numeric digits 20                      /*ensure enough digits for mult. */
parse arg n 1 s                        /*N & S assigned to the first arg*/
                do  while n\==1        /*loop while  N  isn't  unity.   */
                if n//2  then n=n*3+1  /*if  N  is odd,  calc:   3*n +1 */
                         else n=n%2    /* "  "   " even, perform fast ÷ */
                s=s n                  /*build a sequence list (append).*/
                end   /*while*/
return s
```


===task 2, 3===
The following program is named:  : '''HAIL_PGM.REX'''   and is stored in the current directory.

```rexx
/*REXX pgm tests a number and a range for hailstone (Collatz) sequences.*/
parse arg x .;  if x=='' then x=27     /*get the optional first argument*/

$=hailstone(x)                         /*═════════════task 2════════════*/
#=words($)                             /*number of numbers in sequence. */
say x 'has a hailstone sequence of'  #  'and starts with: ' subword($,1,4),
                                       ' and ends with:'    subword($,#-3)
say
w=0;       do j=1  for 99999           /*═════════════task 3════════════*/
           $=hailstone(j);  #=words($) /*obtain the hailstone sequence. */
           if #<=w  then iterate       /*Not big 'nuff? Then keep going.*/
           bigJ=j;    w=#              /*remember what # has biggest HS.*/
           end   /*j*/

say '(between 1──►99,999) '  bigJ  'has the longest hailstone sequence:' w
                                       /*stick a fork in it, we're done.*/
```

```txt

27 has a hailstone sequence of 112 and starts with:  27 82 41 124  and ends with: 8 4 2 1

(between 1──►99,999)  77031 has the longest hailstone sequence: 351

```



### task 4

The following program is named:   '''HAIL_POP.REX'''   and is stored in the current directory.

```rexx
/*REXX pgm finds the most common (popular) hailstone sequence length.   */
parse arg z .;  if z=='' then z=99999  /*get the optional first argument*/
!.=0
w=0;          do j=1  for z            /*═════════════task 4════════════*/
              #=words(hailstone(j))    /*obtain hailstone sequence count*/
              !.# = !.# + 1            /*add unity to popularity count. */
              end   /*j*/
occ=0;  p=0
              do k=1  for z
              if !.k>occ  then do;  occ=!.k;  p=k;  end
              end   /*p*/

say '(between 1──►'z") "  p,
' is the most common hailstone sequence length  (with' occ "occurrences)."
                                       /*stick a fork in it, we're done.*/
```

```txt

(between 1──►99999)  72  is the most common hailstone sequence length  (with 1467 occurrences).

```



### task 5

To run a REXX program, it depends on the REXX interpretor and which operating system is being used   (and what options where used when the REXX interpreter was installed/set up).


On a VM/CMS system, you could enter either of:
*             HAILSTONE
* EXEC HAILSTONE
to execute the   '''HAILSTONE EXEC A'''   program   (there are also other ways to invoke it).


On a Microsoft Windows system, you could enter any of:
*       HAILSTONE.REX
*       HAILSTONE
* xxx HAILSTONE.REX
* xxx HAILSTONE
where   '''xxx'''   is the name of the REXX interpreter, and if installed under a Microsoft Windows (Next family), the file extension and/or the REXX interpreter can be omitted.





## Ruby

An executable library checks ''__FILE__ == $0''. Here, ''__FILE__'' is the path
of the current source file, and ''$0'' is the path of the current executable.
If ''__FILE__ == $0'', then the current source file is the executable,
else the current source file is a library for some other executable.

* ''__FILE__ == $0'' also works with older versions of Ruby, but this Hailstone example calls new methods in Ruby 1.8.7.

This is ''hailstone.rb'', a modification of [[Hailstone sequence#Ruby]] as an executable library.

```ruby
# hailstone.rb
module Hailstone
  module_function
  def hailstone n
    seq = [n]
    until n == 1
      n = (n.even?) ? (n / 2) : (3 * n + 1)
      seq << n
    end
    seq
  end
end

if __FILE__ == $0
  include Hailstone

  # for n = 27, show sequence length and first and last 4 elements
  hs27 = hailstone 27
  p [hs27.length, hs27[0..3], hs27[-4..-1]]

  # find the longest sequence among n less than 100,000
  n, len = (1 ... 100_000) .collect {|n|
    [n, hailstone(n).length]} .max_by {|n, len| len}
  puts "#{n} has a hailstone sequence length of #{len}"
  puts "the largest number in that sequence is #{hailstone(n).max}"
end
```


```txt
$ ruby scratch.rb
[112, [27, 82, 41, 124], [8, 4, 2, 1]]
77031 has a hailstone sequence length of 351
the largest number in that sequence is 21933016
```


This is ''hsfreq.rb'', which requires ''hailstone.rb'' as a library.


```ruby
# hsfreq.rb
require 'hailstone'

h = Hash.new(0)
last = 99_999
(1..last).each {|n| h[Hailstone.hailstone(n).length] += 1}
length, count = h.max_by {|length, count| count}

puts "Given the hailstone sequences from 1 to #{last},"
puts "the most common sequence length is #{length},"
puts "with #{count} such sequences."
```


As with any library, ''hailstone.rb'' must be in <code>$:</code>, the search path for libraries.
One way is to leave ''hailstone.rb'' in the current directory and run <code>ruby -I. hsfreq.rb</code>.
(Ruby older than 1.9.2 also searches the current directory by default.)


```txt
$ ruby -I. hsfreq.rb
Given the hailstone sequences from 1 to 99999,
the most common sequence length is 72,
with 1467 such sequences.
```



## Scala

In Scala it is possible to combine several "main"s
(mixed-in by the App trait) in one file (e.g. HailstoneSequence.scala):

```Scala
object HailstoneSequence extends App { // Show it all, default number is 27.
  def hailstone(n: Int): Stream[Int] =
       n #:: (if (n == 1) Stream.empty else hailstone(if (n % 2 == 0) n / 2 else n * 3 + 1))

  Hailstone.details(args.headOption.map(_.toInt).getOrElse(27))
  HailTest.main(Array())
}

object Hailstone extends App { // Compute a given or default number to Hailstone sequence
  def details(nr: Int) = {
    val collatz = HailstoneSequence.hailstone(nr)

    println(s"Use the routine to show that the hailstone sequence for the number: $nr.")
    println(collatz.toList)
    println(s"It has ${collatz.length} elements.")
  }
  details(args.headOption.map(_.toInt).getOrElse(27))
}

object HailTest extends App { // Compute only the < 100000 test
  println(
    "Compute the number < 100,000, which has the longest hailstone sequence with that sequence's length.")
  val (n, len) = (1 until 100000).map(n => (n, HailstoneSequence.hailstone(n).length)).maxBy(_._2)
  println(s"Longest hailstone sequence length= $len occurring with number $n.")
}
```


Steps:

1. First let the compiler process the source file:

```txt
C:\Users\FransAdm\Documents>scalac HailstoneSequence.scala

```

2. Run the Hailstone function with a parameter:

```txt
C:\Users\FransAdm\Documents>scala Hailstone 42
Use the routine to show that the hailstone sequence for the number: 42.
List(42, 21, 64, 32, 16, 8, 4, 2, 1)
It has 9 elements.

```
  3. Run the combined function and < 100000 test:

```txt
C:\Users\FransAdm\Documents>scala HailstoneSequence 27
Use the routine to show that the hailstone sequence for the number: 27.
List(27, 82, 41, 124, 62, 31, 94, 47, 142, 71, 214, 107, 322, 161, 484, 242, 121, 364, 182, 91, 274, 137, 412, 206, 103, 310, 155,
466, 233, 700, 350, 175, 526, 263, 790, 395, 1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502, 251, 754, 377, 1132, 566, 283, 850,
 425, 1276, 638, 319, 958, 479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429, 7288, 3644, 1822, 911, 2734, 1367, 4102, 2051, 6154,
3077, 9232, 4616, 2308, 1154, 577, 1732, 866, 433, 1300, 650, 325, 976, 488, 244, 122, 61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 80
, 40, 20, 10, 5, 16, 8, 4, 2, 1)
It has 112 elements.
Compute the number < 100,000, which has the longest hailstone sequence with that sequence's length.
Longest hailstone sequence length= 351 occurring with number 77031.
```

4. Finally do only the callable < 100000 test

```txt
C:\Users\FransAdm\Documents>scala HailTest
Compute the number < 100,000, which has the longest hailstone sequence with that sequence's length.
Longest hailstone sequence length= 351 occurring with number 77031.

C:\Users\FransAdm\Documents>
```



## Sidef

Library saved as '''Hailstone.sm'''

```ruby
func hailstone(n) {
    gather {
        while (n > 1) {
            take(n)
            n = (n.is_even ? n/2 : (3*n + 1))
        }
        take(1)
    }
}
 
if (__FILE__ == __MAIN__) {             # true when not imported
    var seq = hailstone(27)
    say "hailstone(27) - #{seq.len} elements: #{seq.ft(0, 3)} [...] #{seq.ft(-4)}"
 
    var n = 0
    var max = 0
    100_000.times { |i|
        var seq = hailstone(i)
        if (seq.len > max) {
            max = seq.len
            n = i
        }
    }
 
    say "Longest sequence is for #{n}: #{max}"
}
```


It can be run with:

```shell
$ sidef Hailstone.sm
```


It can then be used with a program such as:

```ruby
include Hailstone
 
var score = Hash()
100_000.times { |i| score{ Hailstone::hailstone(i).len } := 0 ++ }
 
var k = score.keys.max_by {|k| score{k} }
say "Most common length is #{k}, occurring #{score{k}} times"
```


Called with a command line as:

```shell
$ sidef test_hailstone.sf
```


The library is searched in the directories specified in the '''SIDEF_INC''' environment variable, which defaults to the current directory.


## Tcl

The standard idiom for detecting whether a script is being loaded as a library or run directly is to compare the result of <code>info script</code> (which describes the name of the currently sourced script file) and the global <code>argv0</code> variable (which holds the name of the main script).

```tcl
### In the file hailstone.tcl ###
package provide hailstone 1.0

proc hailstone n {
    while 1 {
	lappend seq $n
	if {$n == 1} {return $seq}
	set n [expr {$n & 1 ? $n*3+1 : $n/2}]
    }
}

# If directly executed, run demo code
if {[info script] eq $::argv0} {
    set h27 [hailstone 27]
    puts "h27 len=[llength $h27]"
    puts "head4 = [lrange $h27 0 3]"
    puts "tail4 = [lrange $h27 end-3 end]"

    set maxlen [set max 0]
    for {set i 1} {$i<100000} {incr i} {
	set l [llength [hailstone $i]]
	if {$l>$maxlen} {set maxlen $l;set max $i}
    }
    puts "max is $max, with length $maxlen"
}
```


To make the package locatable, run this Tcl script
in the same directory which builds the index file:

```tcl
pkg_mkIndex .
```


Using the above code as a library then just requires that we tell the script the location of the additional library directory by adding it to the global <code>auto_path</code> variable; it is unnecessary if the script is installed in one of the standard locations (a fairly long list that depends on the installation):

```tcl
#!/usr/bin/tclsh8.6
package require Tcl 8.6	;# For [lsort -stride] option
lappend auto_path .		;# Or wherever it is located
package require hailstone 1.0

# Construct a histogram of length frequencies
set histogram {}
for {set n 1} {$n < 100000} {incr n} {
    dict incr histogram [llength [hailstone $n]]
}

# Identify the most common length by sorting...
set sortedHist [lsort -decreasing -stride 2 -index 1 $histogram]
lassign $sortedHist mostCommonLength freq

puts "most common length is $mostCommonLength, with frequency $freq"
```



## zkl

zkl is a system of VM, compiler, etc (ala Python) so no need for compiling/linking etc.

File hailstone.zkl:

```zkl
fcn collatz(n,z=L()){ z.append(n); if(n==1) return(z);
   if(n.isEven) return(self.fcn(n/2,z)); return(self.fcn(n*3+1,z)) }

h27:=collatz(27);
println("Hailstone(27)-->",h27[0,4].concat(","),"...",
	h27[-4,*].concat(",")," length ",h27.len());

[2..0d100_000].pump(Void,  // loop n from 2 to 100,000
   collatz,              // generate Collatz sequence(n)
   fcn(c,n){           // if new longest sequence, save length/C, return longest
      if(c.len()>n[0]) n.clear(c.len(),c[0]); n}.fp1(L(0,0)))
.println();
```

File hailstone2.zkl:

```zkl
#!/home/craigd/Bin/zkl
collatz:=Import("hailstone",False,False,False).collatz; // don't run constructor
d:=Dictionary();
[2..0d100_000].pump(Void,  // loop n from 2 to 100,000
   collatz,       // generate Collatz sequence(n)
   'wrap(c){ d.incV(c.len()) }      // save length
);
println("Number of different lengths: ",d.len());
longest:=(0).max(d.values);
mostFreqLen:=d.filter1('wrap([(k,v)]){ v==longest })[0];
println("Most frequent length: %d; %d sequences of that length."
        .fmt(mostFreqLen,longest));
```

```txt

 $ zkl hailstone
Hailstone(27)-->27,82,41,124...8,4,2,1 length 112
L(351,77031)

 $ ./hailstone2.zkl
Number of different lengths: 314
Most frequent length: 72; 1467 sequences of that length.

 $

```

