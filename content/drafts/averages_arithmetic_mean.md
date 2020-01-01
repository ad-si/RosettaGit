+++
title = "Averages/Arithmetic mean"
description = ""
date = 2019-10-10T12:35:36Z
aliases = []
[extra]
id = 2412
[taxonomies]
categories = []
tags = []
+++

{{task|Probability and statistics}}

{{task heading}}

Write a program to find the [[wp:arithmetic mean|mean]] (arithmetic average) of a numeric vector.

In case of a zero-length input, since the mean of an empty set of numbers is ill-defined, the program may choose to behave in any way it deems appropriate, though if the programming language has an established convention for conveying math errors or undefined values, it's preferable to follow it.

{{task heading|See also}}

{{Related tasks/Statistical measures}}


<hr>


## 0815


```0815

{x{+=<:2:x/%<:d:~$<:01:~><:02:~><:03:~><:04:~><:05:~><:06:~><:07:~><:08:
~><:09:~><:0a:~><:0b:~><:0c:~><:0d:~><:0e:~><:0f:~><:10:~><:11:~><:12:~>
<:13:~><:14:~><:15:~><:16:~><:17:~><:18:~><:19:~><:ffffffffffffffff:~>{x
{+>}:8f:{&={+>{~>&=x<:ffffffffffffffff:/#:8f:{{=<:19:x/%

```

{{out}}

```txt

0
D

```



## 11l

{{trans|Python}}

```11l
F average(x)
   R sum(x) / Float(x.len)

print(average([0, 0, 3, 1, 4, 1, 5, 9, 0, 0]))
```

{{out}}

```txt

2.3

```



## 360 Assembly

Compact and functional.

```360asm
AVGP     CSECT
         USING  AVGP,12
         LR     12,15
         SR     3,3                i=0
         SR     6,6                sum=0
LOOP     CH     3,=AL2(NN-T-1)     for i=1 to nn
         BH     ENDLOOP
         L      2,T(3)             t(i)
         MH     2,=H'100'          scaling factor=2
         AR     6,2                sum=sum+t(i)
         LA     3,4(3)             next i
         B      LOOP
ENDLOOP  LR     5,6                sum
         LA     4,0
         D      4,NN               sum/nn
         XDECO  5,Z                edit binary
         MVC    U,Z+10             descale
         MVI    Z+10,C'.'
         MVC    Z+11(2),U
         XPRNT  Z,80               output
         XR     15,15
         BR     14
T        DC     F'10',F'9',F'8',F'7',F'6',F'5',F'4',F'3',F'2',F'1'
NN       DC     A((NN-T)/4)
Z        DC     CL80' '
U        DS     CL2
         END    AVGP
```

{{out}}

```txt
         5.50
```



## 6502 Assembly

Called as a subroutine (i.e., JSR ArithmeticMean), this calculates the integer average of up to 255 8-bit unsigned integers.  The address of the beginning of the list of integers is in the memory location ArrayPtr and the number of integers is in the memory location NumberInts.  The arithmetic mean is returned in the memory location ArithMean.


```6502asm
ArithmeticMean:		PHA
			TYA
			PHA		;push accumulator and Y register onto stack


			LDA #0
			STA Temp
			STA Temp+1	;temporary 16-bit storage for total

			LDY NumberInts
			BEQ Done	;if NumberInts = 0 then return an average of zero

			DEY		;start with NumberInts-1
AddLoop:		LDA (ArrayPtr),Y
			CLC
			ADC Temp
			STA Temp
			LDA Temp+1
			ADC #0
			STA Temp+1
			DEY
			CPY #255
			BNE AddLoop

			LDY #-1
DivideLoop:		LDA Temp
			SEC
			SBC NumberInts
			STA Temp
			LDA Temp+1
			SBC #0
			STA Temp+1
			INY
			BCS DivideLoop

Done:			STY ArithMean	;store result here
			PLA		;restore accumulator and Y register from stack
			TAY
			PLA
			RTS		;return from routine
```



## 8th


```forth

: avg \ a -- avg(a)
  dup ' n:+ 0 a:reduce
  swap a:len nip n:/ ;

\ test:
[ 1.0, 2.3, 1.1, 5.0, 3, 2.8, 2.01, 3.14159 ] avg . cr
[ ] avg . cr
[ 10 ] avg . cr
bye

```

Output is:

2.54395

NaN

10.00000


## ACL2


```Lisp
(defun mean-r (xs)
   (if (endp xs)
       (mv 0 0)
       (mv-let (m j)
               (mean-r (rest xs))
          (mv (+ (first xs) m) (+ j 1)))))

(defun mean (xs)
   (if (endp xs)
       0
       (mv-let (n d)
               (mean-r xs)
          (/ n d))))
```



## ActionScript


```ActionScript
function mean(vector:Vector.<Number>):Number
{
	var sum:Number = 0;
	for(var i:uint = 0; i < vector.length; i++)
		sum += vector[i];
	return vector.length == 0 ? 0 : sum / vector.length;
}
```



## Ada

This example shows how to pass a zero length vector as well as a larger vector. With Ada 2012 it is possible to check that pre conditions are satisfied (otherwise an exception is thrown). So we check that the length is not zero.

```ada
with Ada.Float_Text_Io; use Ada.Float_Text_Io;
with Ada.Text_IO; use Ada.Text_IO;

procedure Mean_Main is
   type Vector is array (Positive range <>) of Float;
   function Mean (Item : Vector) return float with pre => Item'length > 0;
   function Mean (Item : Vector) return Float is
      Sum : Float := 0.0;
   begin
      for I in Item'range loop
         Sum := Sum + Item(I);
      end loop;
	  return Sum / Float(Item'Length);
   end Mean;
   A : Vector := (3.0, 1.0, 4.0, 1.0, 5.0, 9.0);
begin
    Put(Item => Mean (A), Fore => 1, Exp => 0);
   New_Line;
   -- test for zero length vector
   Put(Item => Mean(A (1..0)), Fore => 1, Exp => 0);
   New_Line;
end Mean_Main;
```

Output:
3.83333

raised SYSTEM.ASSERTIONS.ASSERT_FAILURE : failed precondition from mean_main.adb:6


## Aime


```aime
real
mean(list l)
{
    real sum, x;

    sum = 0;
    for (, x in l) {
        sum += x;
    }

    sum / ~l;
}

integer
main(void)
{
    o_form("%f\n", mean(list(4.5, 7.25, 5r, 5.75)));

    0;
}
```



## ALGOL 68

{{trans|C}}

{{works with|ALGOL 68|Standard - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386 - note that some necessary LONG REAL operators are missing from ELLA's library.}}

```algol68
PROC mean = (REF[]REAL p)REAL:
# Calculates the mean of qty REALs beginning at p. #
  IF LWB p > UPB p THEN 0.0
  ELSE
    REAL total := 0.0;
    FOR i FROM LWB p TO UPB p DO total +:= p[i] OD;
    total / (UPB p - LWB p + 1)
  FI;

main:(
  [6]REAL test := (1.0, 2.0, 5.0, -5.0, 9.5, 3.14159);
  print((mean(test),new line))
)
```



## ALGOL W


```algolw
begin
    % procedure to find the mean of the elements of a vector.                %
    % As the procedure can't find the bounds of the array for itself,        %
    % we pass them in lb and ub          %
    real procedure mean ( real    array vector ( * )
                        ; integer value lb
                        ; integer value ub
                        ) ;
    begin
        real sum;
        assert( ub > lb ); % terminate the program if there are no elements  %
        sum := 0;
        for i := lb until ub do sum := sum + vector( i );
        sum / ( ( ub + 1 ) - lb )
    end mean ;

    % test the mean procedure by finding the mean of 1.1, 2.2, 3.3, 4.4, 5.5 %
    real array numbers ( 1 :: 5 );
    for i := 1 until 5 do numbers( i ) := i + ( i / 10 );
    r_format := "A"; r_w := 10; r_d := 2; % set fixed point output           %
    write( mean( numbers, 1, 5 ) );
end.
```



## AmigaE

Because of the way Amiga E handles floating point numbers, the passed list/vector must contain
all explicitly floating point values (e.g., you need to write "1.0", not "1")

```amigae
PROC mean(l:PTR TO LONG)
  DEF m, i, ll
  ll := ListLen(l)
  IF ll = 0 THEN RETURN 0.0
  m := 0.0
  FOR i := 0 TO ll-1 DO m := !m + l[i]
  m := !m / (ll!)
ENDPROC m

PROC main()
  DEF s[20] : STRING
  WriteF('mean \s\n',
         RealF(s,mean([1.0, 2.0, 3.0, 4.0, 5.0]), 2))
ENDPROC
```



## Applesoft BASIC


```ApplesoftBasic
REM COLLECTION IN DATA STATEMENTS, EMPTY DATA IS THE END OF THE COLLECTION
    0 READ V$
    1 IF LEN(V$) = 0 THEN END
    2 N = 0
    3 S = 0
    4 FOR I = 0 TO 1 STEP 0
    5     S = S + VAL(V$)
    6     N = N + 1
    7     READ V$
    8     IF LEN(V$) THEN NEXT
    9 PRINT S / N
10000 DATA1,2,2.718,3,3.142
63999 DATA

REM COLLECTION IN AN ARRAY, ITEM 0 IS THE SIZE OF THE COLLECTION
A(0) = 5 : A(1) = 1 : A(2) = 2 : A(3) = 2.718 : A(4) = 3 : A(5) = 3.142
N = A(0) : IF N THEN S = 0 : FOR I = 1 TO N : S = S + A(I) : NEXT : ? S / N

```


## AntLang

AntLang has a built-in avg function.

```AntLang
avg[list]
```



## Arturo



```arturo
arr #(1 2 3 4 5 6 7)

print $(avg arr)
```


{{out}}


```txt
4
```



## Astro


```astro
mean([1, 2, 3])
mean(1..10)
mean([])

```



## AutoHotkey


```autohotkey
i = 10
Loop, % i {
  Random, v, -3.141592, 3.141592
  list .= v "`n"
  sum += v
}
MsgBox, % i ? list "`nmean: " sum/i:0
```



## AWK


```awk
cat mean.awk
#!/usr/local/bin/gawk -f

# User defined function
function mean(v,      i,n,sum) {
  for (i in v) {
    n++
    sum += v[i]
  }
  if (n>0) {
    return(sum/n)
  } else {
    return("zero-length input !")
  }
}

BEGIN {
  # fill a vector with random numbers
  for(i=0; i < 10; i++) {
    vett[i] = rand()*10
  }
  print mean(vett)
  print mean(nothing)
}

```


{{out}}

```txt

$ awk -f mean.awk
3.92689
zero-length input !

```



## APL

{{works with|APL2}}

```apl
      X←3 1 4 1 5 9
      (+/X)÷⍴X
3.833333333
```



## Babel



```babel
(3 24 18 427 483 49 14 4294 2 41) dup len <- sum ! -> / itod <<
```


{{Out}}

```txt
535
```



## BASIC

{{works with|QBasic}}

Assume the numbers are in an array named "nums".

```qbasic
mean = 0
sum = 0;
FOR i = LBOUND(nums) TO UBOUND(nums)
   sum = sum + nums(i);
NEXT i
size = UBOUND(nums) - LBOUND(nums) + 1
PRINT "The mean is: ";
IF size <> 0 THEN
   PRINT (sum / size)
ELSE
   PRINT 0
END IF
```


=
## BBC BASIC
=
{{works with|BBC BASIC for Windows}}

To calculate the mean of an array:

```BBC BASIC

      REM specific functions for the array/vector types

      REM Byte Array
      DEF FN_Mean_Arithmetic&(n&())
      = SUM(n&()) / (DIM(n&(),1)+1)

      REM Integer Array
      DEF FN_Mean_Arithmetic%(n%())
      = SUM(n%()) / (DIM(n%(),1)+1)

      REM Float 40 array
      DEF FN_Mean_Arithmetic(n())
      = SUM(n()) / (DIM(n(),1)+1)

      REM A String array
      DEF FN_Mean_Arithmetic$(n$())
      LOCAL I%, S%, sum, Q%
      S% = DIM(n$(),1)
      FOR I% = 0 TO S%
        Q% = TRUE
        ON ERROR LOCAL Q% = FALSE
        IF Q% sum += EVAL(n$(I%))
      NEXT
      = sum / (S%+1)

      REM Float 64 array
      DEF FN_Mean_Arithmetic#(n#())
      = SUM(n#()) / (DIM(n#(),1)+1)

```

[[User:MichaelHutton|Michael Hutton]] 14:02, 29 May 2011 (UTC)

==={{header|IS-BASIC}}===
<lang IS-BASIC>100 NUMERIC ARR(3 TO 8)
110 LET ARR(3)=3:LET ARR(4)=1:LET ARR(5)=4:LET ARR(6)=1:LET ARR(7)=5:LET ARR(8)=9
120 PRINT AM(ARR)
130 DEF AM(REF A)
140   LET T=0
150   FOR I=LBOUND(A) TO UBOUND(A)
160     LET T=T+A(I)
170   NEXT
180   LET AM=T/SIZE(A)
190 END DEF
```



## bc

Uses the current scale for calculating the mean.

```bc
define m(a[], n) {
    auto i, s

    for (i = 0; i < n; i++) {
        s += a[i]
    }
    return(s / n)
}
```



## Befunge

The first input is the length of the vector. If a length of 0 is entered, the result is equal to <code>0/0</code>.

```befunge
&:0\:!v!:-1<
 @./\$_\&+\^
```



## blz


```blz

:mean(vec)
    vec.fold_left(0, (x, y -> x + y)) / vec.length()
end
```



## Bracmat

Here are two solutions. The first uses a while loop, the second scans the input by backtracking.

```bracmat

(mean1=
  sum length n
.   0:?sum:?length
  &   whl
    ' ( !arg:%?n ?arg
      & 1+!length:?length
      & !n+!sum:?sum
      )
  & !sum*!length^-1
);

(mean2=
  sum length n
.     0:?sum:?length
    &   !arg
      :   ?
          ( #%@?n
          & 1+!length:?length
          & !n+!sum:?sum
          & ~
          )
          ?
  | !sum*!length^-1
);

```

To test with a list of all numbers 1 .. 999999:

```bracmat

( :?test
& 1000000:?Length
& whl'(!Length+-1:?Length:>0&!Length !test:?test)
& out$mean1$!test
& out$mean2$!test
)
```



## Brat


```brat
mean = { list |
  true? list.empty?, 0, { list.reduce(0, :+) / list.length }
}

p mean 1.to 10  #Prints 5.5
```



## Burlesque



```burlesque

blsq ) {1 2 2.718 3 3.142}av
2.372
blsq ) {}av
NaN

```



## C

Compute mean of a <code>double</code> array of given length.  If length is zero, does whatever <code>0.0/0</code> does (usually means returning <code>NaN</code>).


```c
#include <stdio.h>

double mean(double *v, int len)
{
	double sum = 0;
	int i;
	for (i = 0; i < len; i++)
		sum += v[i];
	return sum / len;
}

int main(void)
{
	double v[] = {1, 2, 2.718, 3, 3.142};
	int i, len;
	for (len = 5; len >= 0; len--) {
		printf("mean[");
		for (i = 0; i < len; i++)
			printf(i ? ", %g" : "%g", v[i]);
		printf("] = %g\n", mean(v, len));
	}

	return 0;
}
```
{{out}}
```txt

mean[1, 2, 2.718, 3, 3.142] = 2.372
mean[1, 2, 2.718, 3] = 2.1795
mean[1, 2, 2.718] = 1.906
mean[1, 2] = 1.5
mean[1] = 1
mean[] = -nan

```



## C sharp


```csharp
using System;
using System.Linq;

class Program
{
    static void Main()
    {
        Console.WriteLine(new[] { 1, 2, 3 }.Average());
    }
}
```


Alternative version (not using the built-in function):

```csharp
using System;

class Program
{
    static void Main(string[] args)
    {
        double average = 0;

        double[] numArray = { 1, 2, 3, 4, 5 };
        average = Average(numArray);

        Console.WriteLine(average); // Output is 3

        // Alternative use
        average = Average(1, 2, 3, 4, 5);

        Console.WriteLine(average); // Output is still 3
        Console.ReadLine();
    }

    static double Average(params double[] nums)
    {
        double d = 0;

        foreach (double num in nums)
            d += num;
        return d / nums.Length;
    }
}
```



## C++

{{libheader|STL}}

```cpp
#include <vector>

double mean(const std::vector<double>& numbers)
{
     if (numbers.size() == 0)
          return 0;

     double sum = 0;
     for (std::vector<double>::iterator i = numbers.begin(); i != numbers.end(); i++)
          sum += *i;
     return sum / numbers.size();
}
```


Shorter (and more idiomatic) version:


```cpp
#include <vector>
#include <algorithm>

double mean(const std::vector<double>& numbers)
{
    if (numbers.empty())
        return 0;
    return std::accumulate(numbers.begin(), numbers.end(), 0.0) / numbers.size();
}
```


Idiomatic version templated on any kind of iterator:


```cpp
#include <iterator>
#include <algorithm>

template <typename Iterator>
double mean(Iterator begin, Iterator end)
{
    if (begin == end)
        return 0;
    return std::accumulate(begin, end, 0.0) / std::distance(begin, end);
}
```



## Chef



```Chef
Mean.

Chef has no way to detect EOF, so rather than interpreting
some arbitrary number as meaning "end of input", this program
expects the first input to be the sample size. Pass in the samples
themselves as the other inputs. For example, if you wanted to
compute the mean of 10, 100, 47, you could pass in 3, 10, 100, and
47. To test the "zero-length vector" case, you need to pass in 0.

Ingredients.
0 g Sample Size
0 g Counter
0 g Current Sample

Method.
Take Sample Size from refrigerator.
Put Sample Size into mixing bowl.
Fold Counter into mixing bowl.
Put Current Sample into mixing bowl.
Loop Counter.
Take Current Sample from refrigerator.
Add Current Sample into mixing bowl.
Endloop Counter until looped.
If Sample Size.
Divide Sample Size into mixing bowl.
Put Counter into 2nd mixing bowl.
Fold Sample Size into 2nd mixing bowl.
Endif until ifed.
Pour contents of mixing bowl into baking dish.

Serves 1.
```



## Clojure


Returns a [http://clojure.org/data_structures ratio]:

```lisp
(defn mean [sq]
  (if (empty? sq)
      0
      (/ (reduce + sq) (count sq))))
```


Returns a float:

```lisp
(defn mean [sq]
  (if (empty? sq)
      0
      (float (/ (reduce + sq) (count sq)))))
```



## COBOL

Intrinsic function:

```cobol
FUNCTION MEAN(some-table (ALL))
```


Sample implementation:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. find-mean.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  i                       PIC 9(4).

       01  summ                    USAGE FLOAT-LONG.

       LINKAGE SECTION.
       01  nums-area.
           03  nums-len            PIC 9(4).
           03  nums                USAGE FLOAT-LONG
                                   OCCURS 0 TO 1000 TIMES
                                   DEPENDING ON nums-len.

       01  result                  USAGE FLOAT-LONG.

       PROCEDURE DIVISION USING nums-area, result.
           IF nums-len = 0
               MOVE 0 TO result
               GOBACK
           END-IF

           DIVIDE FUNCTION SUM(nums (ALL)) BY nums-len GIVING result

           GOBACK
           .
```



## Cobra



```cobra

class Rosetta
	def mean(ns as List<of number>) as number
		if ns.count == 0
			return 0
		else
			sum = 0.0
			for n in ns
				sum += n
			return sum / ns.count

	def main
		print "mean of [[]] is [.mean(List<of number>())]"
		print "mean of [[1,2,3,4]] is [.mean([1.0,2.0,3.0,4.0])]"

```


Output:

```txt

mean of [] is 0
mean of [1, 2, 3, 4] is 2.5

```



## CoffeeScript


```coffeescript

mean = (array) ->
 return 0 if array.length is 0
 sum = array.reduce (s,i,0) -> s += i
 sum / array.length


alert mean [1]

```



## Common Lisp

'''With Reduce'''


```lisp
(defun mean (&rest sequence)
  (if (null sequence)
      nil
      (/ (reduce #'+ sequence) (length sequence))))
```


'''With Loop'''

```lisp
(defun mean (list)
  (unless (null list)
    (/ (loop for i in list sum i)
       (length list))))
```



## Crystal


```ruby
# Crystal will return NaN if an empty array is passed
def mean(arr) : Float64
  arr.sum / arr.size.to_f
end
```



## D


### Imperative Version


```d
real mean(Range)(Range r) pure nothrow @nogc {
    real sum = 0.0;
    int count;

    foreach (item; r) {
        sum += item;
        count++;
    }

    if (count == 0)
        return 0.0;
    else
        return sum / count;
}

void main() {
    import std.stdio;

    int[] data;
    writeln("Mean: ", data.mean);
    data = [3, 1, 4, 1, 5, 9];
    writeln("Mean: ", data.mean);
}
```

{{out}}

```txt
mean: 0
mean: 3.83333
```


### More Functional Version


```d
import std.stdio, std.algorithm, std.range;

real mean(Range)(Range r) pure nothrow @nogc {
    return r.sum / max(1.0L, r.count);
}

void main() {
    writeln("Mean: ", (int[]).init.mean);
    writeln("Mean: ", [3, 1, 4, 1, 5, 9].mean);
}
```

{{out}}

```txt
Mean: 0
Mean: 3.83333
```



### More Precise Version

A (naive?) version that tries to minimize precision loss (but already the sum algorithm applied to a random access range of floating point values uses a more precise summing strategy):

```d
import std.stdio, std.conv, std.algorithm, std.math, std.traits;

CommonType!(T, real) mean(T)(T[] n ...) if (isNumeric!T) {
    alias E = CommonType!(T, real);
    auto num = n.dup;
    num.schwartzSort!(abs, "a > b");
    return num.map!(to!E).sum(0.0L) / max(1, num.length);
}

void main() {
    writefln("%8.5f", mean((int[]).init));
    writefln("%8.5f", mean(     0, 3, 1, 4, 1, 5, 9, 0));
    writefln("%8.5f", mean([-1e20, 3, 1, 4, 1, 5, 9, 1e20]));
}
```

{{out}}

```txt
 0.00000
 2.87500
 2.87500
```



## Dart


```d
num mean(List<num> l) => l.reduce((num p, num n) => p + n) / l.length;

void main(){
  print(mean([1,2,3,4,5,6,7]));
}
```

{{out}}

```txt
4.0
```



## dc

This is not a translation of the bc solution. Array handling would add some complexity. This one-liner is similar to the K solution.


```dc
1 2 3 5 7 zsn1k[+z1<+]ds+xln/p
3.6
```


An expanded example, identifying an empty sample set, could be created as a file, e.g., amean.cd:


```dc
[[Nada Mean: ]Ppq]sq
zd0=qsn [stack length = n]sz
1k [precision can be altered]sz
[+z1<+]ds+x[Sum: ]Pp
ln/[Mean: ]Pp
[Sample size: ]Plnp
```


By saving the sample set "1 2 3 5 7" in a file (sample.dc), the routine, listing summary information, could be called in a command line:


```dc
$ dc sample.dc amean.cd
Sum: 18
Mean: 3.6
Sample size: 5
$
```



## Delphi


```Delphi
program AveragesArithmeticMean;

{$APPTYPE CONSOLE}

uses Types;

function ArithmeticMean(aArray: TDoubleDynArray): Double;
var
  lValue: Double;
begin
  Result := 0;

  for lValue in aArray do
    Result := Result + lValue;
  if Result > 0 then
    Result := Result / Length(aArray);
end;

begin
  Writeln(Mean(TDoubleDynArray.Create()));
  Writeln(Mean(TDoubleDynArray.Create(1,2,3,4,5)));
end.
```



## Dyalect



```dyalect
func avg(args...) {
    var acc = .0
    var len = 0
    for x in args {
        len += 1
        acc += x
    }
    acc / len
}

avg(1, 2, 3, 4, 5, 6)
```



## E


Slightly generalized to support any object that allows iteration.


```e
def meanOrZero(numbers) {
    var count := 0
    var sum := 0
    for x in numbers {
        sum += x
        count += 1
    }
    return sum / 1.max(count)
}
```



## EasyLang

<lang>floatvars
func mean . f[] r .
  for i% range len f[]
    s += f[i%]
  .
  r = s / len f[]
.
f[] = [ 1 2 3 4 5 6 7 8 ]
call mean f[] r
print r
```



## EchoLisp

'''(mean values)''' is included in math.lib. values may be a list, vector, sequence, or any kind of procrastinator.

```scheme

(lib 'math)
(mean '(1 2 3 4)) ;; mean of a list
    → 2.5
(mean #(1 2 3 4)) ;; mean of a vector
    → 2.5

(lib 'sequences)
(mean [1 3 .. 10]) ;; mean of a sequence
    → 5

;; error handling
(mean 'elvis)
    ⛔ error: mean : expected sequence : elvis
(mean ())
    💣 error: mean : null is not an object
(mean #())
    😐 warning: mean : zero-divide : empty-vector
    → 0
(mean [2 2 .. 2])
    😁 warning: mean : zero-divide : empty-sequence
    → 0

```



## ECL


```ecl

AveVal(SET OF INTEGER s) := AVE(s);

//example usage

SetVals := [14,9,16,20,91];
AveVal(SetVals) //returns 30.0 ;

```


## Elena

ELENA 4.1:

```elena
import extensions;

extension op
{
    average()
    {
        real sum := 0;
        int count := 0;

        var enumerator := self.enumerator();

        while (enumerator.next())
        {
            sum += enumerator.get();
            count += 1;
        };

        ^ sum / count
    }
}

public program()
{
    var array := new int[]::(1, 2, 3, 4, 5, 6, 7, 8);
    console.printLine(
        "Arithmetic mean of {",array.asEnumerable(),"} is ",
        array.average()).readChar()
}
```



## Elixir


```elixir
defmodule Average do
  def mean(list), do: Enum.sum(list) / length(list)
end
```



## Emacs Lisp


```lisp
  (defun mean (lst)
    (/ (float (apply '+ lst)) (length lst)))
  (mean '(1 2 3 4))
```


Calculate mean by Emacs Lisp and built-in Emacs Calc


```lisp
(setq x '[1 2 3 4])
(string-to-number (calc-eval (format "vmean(%s)" x)))
```



## Erlang


```erlang
mean([]) -> 0;
mean(L)  -> lists:sum(L)/erlang:length(L).
```



## Euphoria


```Euphoria
function mean(sequence s)
  atom sum
  if length(s) = 0 then
    return 0
  else
    sum = 0
    for i = 1 to length(s) do
      sum += s[i]
    end for
    return sum/length(s)
  end if
end function

sequence test
test = {1.0, 2.0, 5.0, -5.0, 9.5, 3.14159}
? mean(test)
```



## Excel

Assuming the values are entered in the A column, type into any cell which will not be part of the list:


```excel
=AVERAGE(A1:A10)
```


Assuming 10 values will be entered, alternatively, you can just type:


```excel
=AVERAGE(
```


and then select the start and end cells, not necessarily in the same row or column.

The output for the first expression, for the set {x | 1 <= x <= 10, x E N} is


```txt

1	5,5
2
3
4
5
6
7
8
9
10

```



## Factor


```factor
USING: math math.statistics ;

: arithmetic-mean ( seq -- n )
    [ 0 ] [ mean ] if-empty ;
```


Tests:


```factor
( scratchpad ) { 2 3 5 } arithmetic-mean >float
3.333333333333333
```



## Fantom



```fantom

class Main
{
  static Float average (Float[] nums)
  {
    if (nums.size == 0) return 0.0f
    Float sum := 0f
    nums.each |num| { sum += num }
    return sum / nums.size.toFloat
  }

  public static Void main ()
  {
    [[,], [1f], [1f,2f,3f,4f]].each |Float[] i|
    {
      echo ("Average of $i is: " + average(i))
    }
  }
}

```



## Fish


```Fish
!vl0=?vl1=?vl&!
v<  +<>0n; >n;
>l1)?^&,n;
```

Must be called with the values pre-populated on the stack, which can be done in the <tt>fish.py</tt> interpreter with the <tt>-v</tt> switch:

```txt
fish.py mean.fish -v 10 100 47 207.4
```

which generates:

```txt
91.1
```



## Forth


```forth
: fmean ( addr n -- f )
  0e
  dup 0= if 2drop exit then
  tuck floats bounds do
    i f@ f+
  1 floats +loop
  0 d>f f/ ;

create test 3e f, 1e f, 4e f, 1e f, 5e f, 9e f,
test 6 fmean f.     \ 3.83333333333333
```



## Fortran

In ISO Fortran 90 or later, use the SUM intrinsic, the SIZE intrinsic and the MAX intrinsic (to avoid divide by zero):

```fortran
real, target, dimension(100) :: a = (/ (i, i=1, 100) /)
real, dimension(5,20) :: b = reshape( a, (/ 5,20 /) )
real, pointer, dimension(:) :: p => a(2:1)       ! pointer to zero-length array
real :: mean, zmean, bmean
real, dimension(20) :: colmeans
real, dimension(5) :: rowmeans

mean = sum(a)/size(a)                ! SUM of A's elements divided by SIZE of A
mean = sum(a)/max(size(a),1)         ! Same result, but safer code
                                     ! MAX of SIZE and 1 prevents divide by zero if SIZE == 0 (zero-length array)

zmean = sum(p)/max(size(p),1)        ! Here the safety check pays off. Since P is a zero-length array,
                                     ! expression becomes "0 / MAX( 0, 1 ) -> 0 / 1 -> 0", rather than "0 / 0 -> NaN"

bmean = sum(b)/max(size(b),1)        ! multidimensional SUM over multidimensional SIZE

rowmeans = sum(b,1)/max(size(b,2),1) ! SUM elements in each row (dimension 1)
                                     ! dividing by the length of the row, which is the number of columns (SIZE of dimension 2)
colmeans = sum(b,2)/max(size(b,1),1) ! SUM elements in each column (dimension 2)
                                     ! dividing by the length of the column, which is the number of rows (SIZE of dimension 1)
```



## FreeBASIC


```freebasic

' FB 1.05.0 Win64

Function Mean(array() As Double) As Double
  Dim length As Integer = Ubound(array) - Lbound(array) + 1
  If length = 0 Then
    Return 0.0/0.0 'NaN
  End If
  Dim As Double sum = 0.0
  For i As Integer = LBound(array) To UBound(array)
    sum += array(i)
  Next
  Return sum/length
End Function

Function IsNaN(number As Double) As Boolean
  Return Str(number) = "-1.#IND" ' NaN as a string in FB
End Function

Dim As Integer n, i
Dim As Double num
Print "Sample input and output"
Print
Do
  Input "How many numbers are to be input ? : ", n
Loop Until n > 0
Dim vector(1 To N) As Double
Print
For i = 1 to n
  Print "  Number #"; i; " : ";
  Input "", vector(i)
Next
Print
Print "Mean is"; Mean(vector())
Print
Erase vector
num = Mean(vector())
If IsNaN(num) Then
  Print "After clearing the vector, the mean is 'NaN'"
End If
Print
Print "Press any key to quit the program"
Sleep

```


{{out}}

```txt

Sample input and output

How many numbers are to be input ? : 6

  Number # 1 : 12
  Number # 2 : 18
  Number # 3 : 5.6
  Number # 4 : 6
  Number # 5 : 23
  Number # 6 : 17

Mean is 13.6

After clearing the vector, the mean is 'NaN'

```



## Frink

The following works on arrays or sets.  If the collection is empty, this returns the special value <CODE>undef</CODE>.

```frink

mean[x] := length[x] > 0 ? sum[x] / length[x] : undef

```


=={{header|F_Sharp|F#}}==
The following computes the running mean using a tail-recursive approach. If we just sum all the values then divide by the number of values then we will suffer from overflow problems for large lists. See [[wp:Moving_average|wikipedia]] about the moving average computation.

```fsharp
let avg (a:float) (v:float) n =
    a + (1. / ((float n) + 1.)) * (v - a)

let mean_series list =
    let a, _ = List.fold_left (fun (a, n) h -> avg a (float h) n, n + 1) (0., 0) list in
    a
```


Checking this:

```fsharp>
 mean_series [1; 8; 2; 8; 1; 7; 1; 8; 2; 7; 3; 6; 1; 8; 100] ;;
 val it : float = 10.86666667
 > mean_series [] ;;
 val it : float = 0.0
```


We can also make do with the built-in ''List.average'' function:

```fsharp
List.average [4;1;7;5;8;4;5;2;1;5;2;5]
```



## GAP


```gap
Mean := function(v)
  local n;
  n := Length(v);
  if n = 0 then
    return 0;
  else
    return Sum(v)/n;
  fi;
end;

Mean([3, 1, 4, 1, 5, 9]);
# 23/6
```



## GEORGE


```GEORGE
R (n) P ;
0
1, n rep (i)
   R P +
]
n div
P
```

Output:

```txt

 7.000000000000000
 1.500000000000000E+0001
 1.300000000000000E+0001
 8.000000000000000
 2.500000000000000E+0001
 7.400000000000000E+0001
 3.100000000000000E+0001
 2.900000000000000E+0001
 1.700000000000000E+0001
 4.300000000000000E+0001
 2.620000000000000E+0001

```



## GFA Basic


This works for arrays of integers.

<lang>
DIM a%(10)
FOR i%=0 TO 10
  a%(i%)=i%*2
  PRINT "element ";i%;" is ";a%(i%)
NEXT i%
PRINT "mean is ";@mean(a%)
'
FUNCTION mean(a%)
  LOCAL i%,size%,sum
  ' find size of array,
  size%=DIM?(a%())
  ' return 0 for empty arrays
  IF size%<=0
    RETURN 0
  ENDIF
  ' find sum of all elements
  sum=0
  FOR i%=0 TO size%-1
    sum=sum+a%(i%)
  NEXT i%
  ' mean is sum over size
  RETURN sum/size%
ENDFUNC

```



## Go

A little more elaborate that the task requires.  The function "mean" fulfills the task of "a program to find the mean."  As a Go idiom, it returns an ok value of true if result m is valid.  An ok value of false means the input "vector" (a Go slice) was empty.  The fancy accuracy preserving algorithm is a little more than was called more.  The program main is a test program demonstrating the ok idiom and several data cases.


```go
package main

import (
    "fmt"
    "math"
)

func mean(v []float64) (m float64, ok bool) {
    if len(v) == 0 {
        return
    }
    // an algorithm that attempts to retain accuracy
    // with widely different values.
    var parts []float64
    for _, x := range v {
        var i int
        for _, p := range parts {
            sum := p + x
            var err float64
            switch ax, ap := math.Abs(x), math.Abs(p); {
            case ax < ap:
                err = x - (sum - p)
            case ap < ax:
                err = p - (sum - x)
            }
            if err != 0 {
                parts[i] = err
                i++
            }
            x = sum
        }
        parts = append(parts[:i], x)
    }
    var sum float64
    for _, x := range parts {
        sum += x
    }
    return sum / float64(len(v)), true
}

func main() {
    for _, v := range [][]float64{
        []float64{},                         // mean returns ok = false
        []float64{math.Inf(1), math.Inf(1)}, // answer is +Inf

        // answer is NaN, and mean returns ok = true, indicating NaN
        // is the correct result
        []float64{math.Inf(1), math.Inf(-1)},

        []float64{3, 1, 4, 1, 5, 9},

        // large magnitude numbers cancel. answer is mean of small numbers.
        []float64{1e20, 3, 1, 4, 1, 5, 9, -1e20},

        []float64{10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, .11},
        []float64{10, 20, 30, 40, 50, -100, 4.7, -11e2},
    } {
        fmt.Println("Vector:", v)
        if m, ok := mean(v); ok {
            fmt.Printf("Mean of %d numbers is %g\n\n", len(v), m)
        } else {
            fmt.Println("Mean undefined\n")
        }
    }
}
```

{{out}}

```txt

Vector: []
Mean undefined

Vector: [+Inf +Inf]
Mean of 2 numbers is +Inf

Vector: [+Inf -Inf]
Mean of 2 numbers is NaN

Vector: [3 1 4 1 5 9]
Mean of 6 numbers is 3.8333333333333335

Vector: [1e+20 3 1 4 1 5 9 -1e+20]
Mean of 8 numbers is 2.875

Vector: [10 9 8 7 6 5 4 3 2 1 0 0 0 0 0.11]
Mean of 15 numbers is 3.674

Vector: [10 20 30 40 50 -100 4.7 -1100]
Mean of 8 numbers is -130.6625

```



## Groovy


```groovy
def avg = { list -> list == [] ? 0 : list.sum() / list.size() }
```


Test Program:

```groovy
println avg(0..9)
println avg([2,2,2,4,2])
println avg ([])
```


Output:

```txt
4.5
2.4
0
```



## Haskell

This function works if the element type is an instance of Fractional:

```haskell
mean :: (Fractional a) => [a] -> a
mean [] = 0
mean xs = sum xs / Data.List.genericLength xs
```


But some types, e.g. integers, are not Fractional; the following function works for all Real types:

```haskell
meanReals :: (Real a, Fractional b) => [a] -> b
meanReals = mean . map realToFrac
```


If you want to avoid keeping the list in memory and traversing it twice:


```haskell
{-# LANGUAGE BangPatterns #-}

import Data.List (foldl') --'

mean
  :: (Real n, Fractional m)
  => [n] -> m
mean xs =
  let (s, l) =
        foldl' --'
          f
          (0, 0)
          xs
  in realToFrac s / l
  where
    f (!s, !l) x = (s + x, l + 1)

main :: IO ()
main = print $ mean [1 .. 100]
```



## HicEst


```hicest
REAL :: vec(100)               ! no zero-length arrays in HicEst

   vec = $ - 1/2               ! 0.5 ... 99.5
   mean = SUM(vec) / LEN(vec)  ! 50
END
```



## Hy

Returns <tt>None</tt> if the input is of length zero.

```clojure
(defn arithmetic-mean [xs]
    (if xs
        (/ (sum xs) (len xs))))
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main(args)
    every (s := 0) +:= !args
    write((real(s)/(0 ~= *args)) | 0)
end
```


Sample outputs:

```txt
->am 1 2 3 4 5 6 7
4.0
->am
0
->
```



## IDL


If truly only the mean is wanted, one could use


```idl
x = [3,1,4,1,5,9]
print,mean(x)
```


But <tt>mean()</tt> is just a thin wrapper returning the zeroth element of <tt>moment()</tt> :


```idl
print,moment(x)
; ==>
  3.83333      8.96667     0.580037     -1.25081
```


which are mean, variance, skewness and kurtosis.

There are no zero-length vectors in IDL. Every variable has at least one value or otherwise it is <tt><Undefined></tt>.


## J



```j
mean=: +/ % #
```


That is, sum divided by the number of items. The verb also works on higher-ranked arrays. For example:


```j
   mean 3 1 4 1 5 9
3.83333
   mean $0         NB. $0 is a zero-length vector
0
   x=: 20 4 ?@$ 0  NB. a 20-by-4 table of random (0,1) numbers
   mean x
0.58243 0.402948 0.477066 0.511155
```


The computation can also be written as a loop. It is shown here for comparison only and is highly non-preferred compared to the version above.


```j
mean1=: 3 : 0
 z=. 0
 for_i. i.#y do. z=. z+i{y end.
 z % #y
)
   mean1 3 1 4 1 5 9
3.83333
   mean1 $0
0
   mean1 x
0.58243 0.402948 0.477066 0.511155
```



## Java

{{works with|Java|1.5+}}


```java5
public static double avg(double... arr) {
    double sum = 0.0;
    for (double x : arr) {
        sum += x;
    }
    return sum / arr.length;
}
```



## JavaScript



### ES5



```javascript
function mean(array)
{
 var sum = 0, i;
 for (i = 0; i < array.length; i++)
 {
  sum += array[i];
 }
  return array.length ? sum / array.length : 0;
}

alert( mean( [1,2,3,4,5] ) );   // 3
alert( mean( [] ) );            // 0
```


Using the native function `.forEach()`:

```javascript
function mean(array) {
    var sum = 0;
    array.forEach(function(value){
        sum += value;
        });
    return array.length ? sum / array.length : 0;
    }

alert( mean( [1,2,3,4,5] ) );   // 3
```


Using the native function `.reduce()`:

```javascript
function mean(array) {
    return !array.length ? 0
        : array.reduce(function(pre, cur, i) {
            return (pre * i + cur) / (i + 1);
            });
    }

alert( mean( [1,2,3,4,5] ) );   // 3
alert( mean( [] ) );            // 0

```


Extending the `Array` prototype:

```javascript
Array.prototype.mean = function() {
    return !this.length ? 0
        : this.reduce(function(pre, cur, i) {
            return (pre * i + cur) / (i + 1);
            });
    }

alert( [1,2,3,4,5].mean() );   // 3
alert( [].mean() );            // 0

```



{{libheader|Functional}}

```javascript
function mean(a)
{
 return a.length ? Functional.reduce('+', 0, a) / a.length : 0;
}
```




### ES6



```JavaScript
(sample => {

    // mean :: [Num] => (Num | NaN)
    let mean = lst => {
        let lng = lst.length;

        return lng ? (
            lst.reduce((a, b) => a + b, 0) / lng
        ) : NaN;
    };

    return mean(sample);

})([1, 2, 3, 4, 5, 6, 7, 8, 9]);
```


{{Out}}

```JavaScript>5</lang



## jq

The mean of an array of numbers can be computed by simply writing

```jq>add/length</lang


This definition raises an error condition if the array is empty, so it may make sense to define '''mean''' as follows, '''null''' being jq's null value:

```jq
def mean: if length == 0 then null
  else add/length
  end;
```



## Julia

Julia's built-in mean function accepts AbstractArrays (vector, matrix, etc.)

```julia>julia
 using Statistics; mean([1,2,3])
2.0
julia> mean(1:10)
5.5
julia> mean([])
ERROR: mean of empty collection undefined: []
```



## K


```k
  mean: {(+/x)%#x}
  mean 1 2 3 5 7
3.6
  mean@!0    / empty array
0.0
```



## Kotlin

Kotlin has builtin functions for some collection types.
Example:

```scala
fun main(args: Array<String>) {
    val nums = doubleArrayOf(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
    println("average = %f".format(nums.average()))
}
```



## LabVIEW

{{VI solution|LabVIEW_Averages_Arithmetic_mean.png}}


## Lasso


```Lasso
define average(a::array) => {
	not #a->size ? return 0
	local(x = 0.0)
	with i in #a do => { #x += #i }
	return #x / #a->size
}

average(array(1,2,5,17,7.4)) //6.48
```



## LFE


=== 1-Arity ===


```lisp

(defun mean (data)
  (/ (lists:sum data)
     (length data)))

```


Usage:

```lisp>
 (mean '(1 1))
1.0
> (mean '(1 2))
1.5
> (mean '(2 10))
6.0
> (mean '(6 12 18 24 30 36 42 48 54 60 66 72 78))
42.0
```


=== n-Arity ===

Functions in LFE (and Erlang) have set arity, but macros can be used to provide the same use as n-arity functions:


```lisp
(defmacro mean args
  `(/ (lists:sum ,args)
      ,(length args)))
```


Usage:


```lisp>
 (mean 42)
42.0
> (mean 18 66)
42.0
> (mean 6 12 18 24 30 36 42 48 54 60 66 72 78)
42.0
```



## Liberty BASIC


```lb
total=17
dim nums(total)
for i = 1 to total
    nums(i)=i-1
next

for j = 1 to total
    sum=sum+nums(j)
next
if total=0 then mean=0 else mean=sum/total
print "Arithmetic mean: ";mean

```


## Limbo


```Limbo
implement Command;

include "sys.m";
sys: Sys;

include "draw.m";

include "sh.m";

init(nil: ref Draw->Context, nil: list of string)
{
	sys = load Sys Sys->PATH;

	a := array[] of {1.0, 2.0, 500.0, 257.0};
	sys->print("mean of a: %f\n", getmean(a));
}

getmean(a: array of real): real
{
	n: real = 0.0;
	for (i := 0; i < len a; i++)
		n += a[i];
	return n / (real len a);
}
```



## Lingo


```Lingo
-- v can be (2D) point, (3D) vector or list of integers/floats
on mean (v)
    case ilk(v) of
        #point: cnt = 2
        #vector: cnt = 3
        #list: cnt = v.count
        otherwise: return
    end case
    sum = 0
    repeat with i = 1 to cnt
        sum = sum + v[i]
    end repeat
    return float(sum)/cnt
end
```



```Lingo
put mean(point(1, 2.5))
-- 1.7500
put mean(vector(1.2, 4.7, 5.6))
-- 3.8333
put mean([6,12,18,24,30,36,42,48,54,60,66,72,78])
-- 42.0000
```



## LiveCode

Livecode provides arithmeticMean (avg, average) built-in.

```LiveCode
average(1,2,3,4,5)  -- 3
average(empty)  -- 0
```



## Logo


```logo
to average :l
  if empty? :l [output 0]
  output quotient apply "sum :l count :l
end
print average [1 2 3 4]    ; 2.5
```



## Logtalk

Logtalk's standard library provides an arithmetic average predicate but we ignore it here. Representing a vector using a list:

```logtalk

:- object(averages).

    :- public(arithmetic/2).

    % fails for empty vectors
    arithmetic([X| Xs], Mean) :-
        sum_and_count([X| Xs], 0, Sum, 0, Count),
        Mean is Sum / Count.

    % use accumulators to make the predicate tail-recursive
    sum_and_count([], Sum, Sum, Count, Count).
    sum_and_count([X| Xs], Sum0, Sum, Count0, Count) :-
        Sum1 is Sum0 + X,
        Count1 is Count0 + 1,
        sum_and_count(Xs, Sum1, Sum, Count1, Count).

:- end_object.

```

Sample output:

```text

| ?- averages::arithmetic([1,2,3,4,5,6,7,8,9,10], Mean).
Mean = 5.5
yes

```



## LSL


```LSL
integer MAX_ELEMENTS = 10;
integer MAX_VALUE = 100;
default {
    state_entry() {
        list lst = [];
        integer x = 0;
        for(x=0 ; x<MAX_ELEMENTS ; x++) {
            lst += llFrand(MAX_VALUE);
        }
        llOwnerSay("lst=["+llList2CSV(lst)+"]");
        llOwnerSay("Geometric Mean: "+(string)llListStatistics(LIST_STAT_GEOMETRIC_MEAN, lst));
        llOwnerSay("           Max: "+(string)llListStatistics(LIST_STAT_MAX, lst));
        llOwnerSay("          Mean: "+(string)llListStatistics(LIST_STAT_MEAN, lst));
        llOwnerSay("        Median: "+(string)llListStatistics(LIST_STAT_MEDIAN, lst));
        llOwnerSay("           Min: "+(string)llListStatistics(LIST_STAT_MIN, lst));
        llOwnerSay("     Num Count: "+(string)llListStatistics(LIST_STAT_NUM_COUNT, lst));
        llOwnerSay("         Range: "+(string)llListStatistics(LIST_STAT_RANGE, lst));
        llOwnerSay("       Std Dev: "+(string)llListStatistics(LIST_STAT_STD_DEV, lst));
        llOwnerSay("           Sum: "+(string)llListStatistics(LIST_STAT_SUM, lst));
        llOwnerSay("   Sum Squares: "+(string)llListStatistics(LIST_STAT_SUM_SQUARES, lst));
    }
}
```

Output:

```txt

lst=[23.815209, 85.890704, 10.811144, 31.522696, 54.619416, 12.211729, 42.964463, 87.367889, 7.106129, 18.711078]
Geometric Mean:    27.325070
           Max:    87.367889
          Mean:    37.502046
        Median:    27.668953
           Min:     7.106129
     Num Count:    10.000000
         Range:    80.261761
       Std Dev:    29.819840
           Sum:   375.020458
   Sum Squares: 22067.040048

```



## Lua


```lua
function mean (numlist)
    if type(numlist) ~= 'table' then return numlist end
    num = 0
    table.foreach(numlist,function(i,v) num=num+v end)
    return num / #numlist
end

print (mean({3,1,4,1,5,9}))
```



## Lucid



```lucid
avg(x)
 where
    sum = first(x) fby sum + next(x);
    n = 1 fby n + 1;
    avg = sum / n;
 end
```



## M4

M4 handle only integers, so in order to have a slightly better math for the mean, we
must pass to the <tt>mean</tt> macro integers multiplied by 100. The macro
<tt>rmean</tt> could embed the macro <tt>fmean</tt> and <tt>extractdec</tt>
directly, but it is a little bit clearer to keep them separated.


```m4
define(`extractdec', `ifelse(eval(`$1%100 < 10'),1,`0',`')eval($1%100)')dnl
define(`fmean', `eval(`($2/$1)/100').extractdec(eval(`$2/$1'))')dnl
define(`mean', `rmean(`$#', $@)')dnl
define(`rmean', `ifelse(`$3', `', `fmean($1,$2)',dnl
`rmean($1, eval($2+$3), shift(shift(shift($@))))')')dnl
```


```m4
mean(0,100,200,300,400,500,600,700,800,900,1000)
```



## Maple

This version accepts any indexable structure, including numeric arrays.  We use a call to the "environment variable" (dynamically scoped global) "Normalizer" to provide normalization of symbolic expressions.  This can be set by the caller to adjust the strength of normalization desired.

```Maple

mean := proc( a :: indexable )
        local   i;
        Normalizer( add( i, i in a ) / numelems( a ) )
end proc:

```

For example:

```Maple

> mean( { 1/2, 2/3, 3/4, 4/5, 5/6 } ); # set
                                  71
                                  ---
                                  100

> mean( [ a, 2, c, 2.3, e ] ); # list
                     0.8600000000 + a/5 + c/5 + e/5

> mean( Array( [ 1, sin( s ), 3, exp( I*t ), 5 ] ) ); # array
                    9/5 + 1/5 sin(s) + 1/5 exp(t I)

> mean( [ sin(s)^2, cos(s)^2 ] );
                                 2             2
                       1/2 sin(s)  + 1/2 cos(s)

> Normalizer := simplify: # use a stronger normalizer than the default
> mean( [ sin(s)^2, cos(s)^2 ] );
                                  1/2

> mean([]); # empty argument causes an exception to be raised.
Error, (in mean) numeric exception: division by zero

```

A slightly different design computes the mean of all its arguments, instead of requiring a single container argument.  This seems a little more Maple-like for a general purpose utility.

```Maple
mean := () -> Normalizer( `+`( args ) / nargs ):
```

This can be called as in the following examples.

```Maple

> mean( 1, 2, 3, 4, 5 );
                                   3

> mean( a + b, b + c, c + d, d + e, e + a );
                      2 a   2 b   2 c   2 d   2 e
                      --- + --- + --- + --- + ---
                       5     5     5     5     5

> mean(); # again, an exception is raised
Error, (in mean) numeric exception: division by zero

```

If desired, we can add argument type-checking as follows.

```Maple
mean := ( s :: seq(algebraic) ) -> Normalizer( `+`( args ) / nargs ):
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Modify the built-in Mean function to give 0 for empty vectors (lists in Mathematica):

```mathematica
Unprotect[Mean];
Mean[{}] := 0
```

Examples:

```mathematica
Mean[{3,4,5}]
Mean[{3.2,4.5,5.9}]
Mean[{-4, 1.233}]
Mean[{}]
Mean[{1/2,1/3,1/4,1/5}]
Mean[{a,c,Pi,-3,a}]
```

gives (a set of integers gives back an integer or a rational, a set of floats gives back a float, a set of rationals gives a rational back, a list of symbols and numbers keeps the symbols exact and a mix of exact and approximate numbers gives back an approximate number):

```mathematica
4
4.53333
-1.3835
0
77/240
1/5 (-3+2 a+c+Pi)
```



## Mathprog


Summing the vector and then dividing the sum by the vector's length is slightly less boring than calling a builtin function Mean or similar.

Mathprog is never boring so this program finds a number M such that when M is subtracted from each value in the vector a second vector is formed with the property that the sum of the elements in the second vector is zero. In this case M is the Arithmetic Mean.

Euclid proved that for any vector there is only one such number and from this derived the Division Theorem.

To make it more interesting I find the Arithmectic Mean of more than a million Integers.

<lang>
/*Arithmetic Mean of a large number of Integers
  - or - solve a very large constraint matrix
         over 1 million rows and columns
  Nigel_Galloway
  March 18th., 2008.
*/

param e := 20;
set Sample := {1..2**e-1};

var Mean;
var E{z in Sample};

/* sum of variances is zero */
zumVariance: sum{z in Sample} E[z] = 0;

/* Mean + variance[n] = Sample[n] */
variances{z in Sample}: Mean + E[z] = z;

solve;

printf "The arithmetic mean of the integers from 1 to %d is %f\n", 2**e-1, Mean;

end;

```


When run this produces:

<lang>
GLPSOL: GLPK LP/MIP Solver, v4.47
Parameter(s) specified in the command line:
 --nopresol --math AM.mprog
Reading model section from AM.mprog...
24 lines were read
Generating zumVariance...
Generating variances...
Model has been successfully generated
Scaling...
 A: min|aij| = 1.000e+000  max|aij| = 1.000e+000  ratio = 1.000e+000
Problem data seem to be well scaled
Constructing initial basis...
Size of triangular part = 1048575
GLPK Simplex Optimizer, v4.47
1048576 rows, 1048576 columns, 3145725 non-zeros
      0: obj =  0.000000000e+000  infeas = 5.498e+011 (1)
*     1: obj =  0.000000000e+000  infeas = 0.000e+000 (0)
OPTIMAL SOLUTION FOUND
Time used:   2.0 secs
Memory used: 1393.8 Mb (1461484590 bytes)
The arithmetic mean of the integers from 1 to 1048575 is 524288.000000
Model has been successfully processed

```



## MATLAB


```Matlab
function meanValue = findmean(setOfValues)
   meanValue = mean(setOfValues);
end
```



## Maxima


```maxima
load("descriptive");
mean([2, 7, 11, 17]);
```



## MAXScript


```maxscript
fn mean data =
(
    total = 0
    for i in data do
    (
        total += i
    )
    if data.count == 0 then 0 else total as float/data.count
)

print (mean #(3, 1, 4, 1, 5, 9))
```



## Mercury


```mercury
:- module arithmetic_mean.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module float, list, require.

main(!IO) :-
    io.print_line(mean([1.0, 2.0, 3.0, 4.0, 5.0]), !IO).

:- func mean(list(float)) = float.

mean([]) = func_error("mean: emtpy list").
mean(Ns @ [_ | _]) = foldl((+), Ns, 0.0) / float(length(Ns)).

:- end_module arithmetic_mean.
```


Alternatively, we could use inst subtyping to ensure we get a compilation error if the
mean function is called with an empty list.


```mercury
:- func mean(list(float)::in(non_empty_list)) = (float::out).

mean(Ns) = foldl((+), Ns, 0.0) / float(length(Ns)).
```



## min

Returns <code>nan</code> for an empty quotation.
{{works with|min|0.19.3}}

```min
(((0 (+) reduce) (size /)) cleave) :mean
(2 3 5) mean print
```

{{out}}

```txt

3.333333333333334

```



## MiniScript



```MiniScript
arr = [ 1, 3, 7, 8, 9, 1 ]

avg = function(arr)
    avgNum = 0
    for num in arr
        avgNum = avgNum + num
    end for
    return avgNum / arr.len
end function

print avg(arr)
```


=={{header|МК-61/52}}==
<lang>0	П0	П1	С/П	ИП0	ИП1	*	+	ИП1	1
+	П1	/	П0	БП	03
```


''Instruction:'' В/О С/П Number С/П Number ...

Each time you press the С/П on the indicator would mean already entered numbers.

=={{header|Modula-2}}==

```modula2
PROCEDURE  Avg;

VAR     avg             : REAL;

BEGIN
   avg := sx / n;
   InOut.WriteString ("Average = ");
   InOut.WriteReal (avg, 8, 2);
   InOut.WriteLn
END Avg;
```

OR

```modula2
PROCEDURE Average (Data  : ARRAY OF REAL;   Samples : CARDINAL) : REAL;

(*  Calculate the average over 'Samples' values, stored in array 'Data'.     *)

VAR     sum         : REAL;
        n           : CARDINAL;

BEGIN
  sum := 0.0;
  FOR n := 0 TO Samples - 1 DO
    sum := sum + Data [n]
  END;
  RETURN sum / FLOAT(Samples)
END Average;
```


## MUMPS


```MUMPS
MEAN(X)
 ;X is assumed to be a list of numbers separated by "^"
 QUIT:'$DATA(X) "No data"
 QUIT:X="" "Empty Set"
 NEW S,I
 SET S=0,I=1
 FOR  QUIT:I>$L(X,"^")  SET S=S+$P(X,"^",I),I=I+1
 QUIT (S/$L(X,"^"))
```


```txt
USER>W $$MEAN^ROSETTA
No data
USER>W $$MEAN^ROSETTA("")
Empty Set
USER>

USER>W $$MEAN^ROSETTA("1^6^12^4")
5.75

```



## Nemerle


```Nemerle
using System;
using System.Console;
using Nemerle.Collections;

module Mean
{
    ArithmeticMean(x : list[int]) : double
    {
        |[] => 0.0
        |_  =>(x.FoldLeft(0, _+_) :> double) / x.Length
    }

    Main() : void
    {
        WriteLine("Mean of [1 .. 10]: {0}", ArithmeticMean($[1 .. 10]));
    }
}
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

launchSample()
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method arithmeticMean(vv = Vector) public static signals DivideException returns Rexx
  sum = 0
  n_ = Rexx
  loop n_ over vv
    sum = sum + n_
    end n_
  mean = sum / vv.size()

  return mean

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method launchSample() public static
  TRUE_  = 1 == 1
  FALSE_ = \TRUE_
  tracing = FALSE_
  vectors = getSampleData()
  loop v_ = 0 to vectors.length - 1
    say 'Average of:' vectors[v_].toString()
    do
      say '          =' arithmeticMean(vectors[v_])
    catch dex = DivideException
      say 'Caught "Divide By Zero"; bypassing...'
      if tracing then dex.printStackTrace()
    catch xex = RuntimeException
      say 'Caught unspecified run-time exception; bypassing...'
      if tracing then xex.printStackTrace()
    end
    say
    end v_
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method getSampleData() private static returns Vector[]
  seed = 1066
  rng = Random(seed)
  vectors =[ -
    Vector(Arrays.asList([Rexx 1, 2, 3, 4, 5, 6, 7, 8, 9, 10])), -
    Vector(), -
    Vector(Arrays.asList([Rexx rng.nextInt(seed), rng.nextInt(seed), rng.nextInt(seed), rng.nextInt(seed), rng.nextInt(seed), rng.nextInt(seed)])), -
    Vector(Arrays.asList([Rexx rng.nextDouble(), rng.nextDouble(), rng.nextDouble(), rng.nextDouble(), rng.nextDouble(), rng.nextDouble(), rng.nextDouble()])), -
    Vector(Arrays.asList([Rexx '1.0', '2.0', 3.0])), -
    Vector(Arrays.asList([Rexx '1.0', 'not a number', 3.0])) -
    ]
  return vectors

```

'''Output:'''

```txt

Average of: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
          = 5.5

Average of: []
Caught "Divide By Zero"; bypassing...

Average of: [294, 726, 945, 828, 1031, 825]
          = 774.833333

Average of: [0.3318379308729921, 0.7612271993941618, 0.9517290891755477, 0.7687823629521795, 0.2201768257213939, 0.1083471020993242, 0.5158554699332363]
          = 0.52256514

Average of: [1.0, 2.0, 3.0]
          = 2

Average of: [1.0, not a number, 3.0]
Caught unspecified run-time exception; bypassing...


```



## NewLISP


```NewLISP
(define (Mean Lst)
   (if (empty? Lst)
      0
      (/ (apply + Lst) (length Lst))))

 (Mean (sequence 1 1000))-> 500
 (Mean '()) -> 0
```



## Nial

in the standard way, mean is

```nial
mean is / [sum, tally]

mean 6 2 4
= 4
```

but it fails with 0 length vectors. so using a tally with a minimum value 1


```nial
dtally is recur [ empty rest, 1 first, 1 first, plus, rest ]
mean is / [sum, dtally]

mean []
=0
```



## Nim

{{trans|C}}

```nim
import strutils

proc mean(xs: openArray[float]): float =
  for x in xs:
    result += x
  result = result / float(xs.len)

var v = @[1.0, 2.0, 2.718, 3.0, 3.142]
for i in 0..5:
  echo "mean of first ", v.len, " = ", formatFloat(mean(v), precision = 0)
  if v.len > 0: v.setLen(v.high)
```

Output:

```txt
mean of first 5 = 2.372
mean of first 4 = 2.1795
mean of first 3 = 1.906
mean of first 2 = 1.5
mean of first 1 = 1
mean of first 0 = -1.#IND
```



## Niue


```Niue

[ [ , len 1 - at ! ] len 3 - times swap , ] 'map ; ( a Lisp like map, to sum the stack )
[ len 'n ; [ + ] 0 n swap-at map n / ] 'avg ;

1 2 3 4 5 avg .
=> 3
3.4 2.3 .01 2.0 2.1 avg .
=> 1.9619999999999997

```

=={{header|Oberon-2}}==
Oxford Oberon-2

```oberon2

MODULE AvgMean;
IMPORT Out;
CONST MAXSIZE = 10;
PROCEDURE Avg(a: ARRAY OF REAL; items: INTEGER): REAL;
VAR
	i: INTEGER;
	total: REAL;
BEGIN
	total := 0.0;
	FOR i := 0 TO LEN(a) -  1 DO
		total := total + a[i]
	END;
	RETURN total/LEN(a)
END Avg;
VAR
	ary: ARRAY MAXSIZE OF REAL;
BEGIN
	ary[0] := 10.0;
	ary[1] := 11.01;
	ary[2] := 12.02;
	ary[3] := 13.03;
	ary[4] := 14.04;
	ary[5] := 15.05;
	ary[6] := 16.06;
	ary[7] := 17.07;
	ary[8] := 18.08;
	ary[9] := 19.09;
	Out.Fixed(Avg(ary),4,2);Out.Ln
END AvgMean.

```

Output:

```txt

14.55

```


## Objeck


```objeck

function : native : PrintAverage(values : FloatVector) ~ Nil {
  values->Average()->PrintLine();
}

```



## OCaml

These functions return a float:


```ocaml
let mean_floats = function
  | [] -> 0.
  | xs -> List.fold_left (+.) 0. xs /. float_of_int (List.length xs)

let mean_ints xs = mean_floats (List.map float_of_int xs)
```


the previous code is easier to read and understand, though if you wish
the fastest implementation to use in production code notice several points:
it is possible to save a call to List.length computing the length through
the List.fold_left, and for mean_ints it is possible to save calling
float_of_int on every numbers, converting only the result of the addition.
(also when using List.map and when the order is not important, you can use
List.rev_map instead to save an internal call to List.rev).
Also the task asks to return 0 on empty lists, but in OCaml this case
would rather be handled by an exception.


```ocaml
let mean_floats xs =
  if xs = [] then
    invalid_arg "empty list"
  else
    let total, length =
      List.fold_left
        (fun (tot,len) x -> (x +. tot), len +. 1.)
        (0., 0.) xs
    in
    (total /. length)
;;


let mean_ints xs =
  if xs = [] then
    invalid_arg "empty list"
  else
    let total, length =
      List.fold_left
        (fun (tot,len) x -> (x + tot), len +. 1.)
        (0, 0.) xs
    in
    (float total /. length)
;;
```



## Octave


GNU Octave has a <tt>mean</tt> function (from statistics package), but it does not handle an empty vector; an implementation that allows that is:


```octave
function m = omean(l)
  if ( numel(l) == 0 )
    m = 0;
  else
    m = mean(l);
  endif
endfunction

disp(omean([]));
disp(omean([1,2,3]));
```


If the data contains missing value, encoded as non-a-number:


```octave
function m = omean(l)
     n = sum(~isnan(l));
     l(isnan(l))=0;
     s = sum(l);
     m = s./n;
end;
```



## Oforth



```Oforth
: avg ( x -- avg )
   x sum
   x size dup ifZero: [ 2drop null ] else: [ >float / ]
;
```


{{out}}

```txt

[1, 2, 2.718, 3, 3.142] avg .
2.372 ok
[ ] avg .
null ok

```



## ooRexx


```ooRexx

call testAverage .array~of(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
call testAverage .array~of(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, .11)
call testAverage .array~of(10, 20, 30, 40, 50, -100, 4.7, -11e2)
call testAverage .array~new

::routine testAverage
  use arg numbers
  say "numbers =" numbers~toString("l", ", ")
  say "average =" average(numbers)
  say

::routine average
  use arg numbers
  -- return zero for an empty list
  if numbers~isempty then return 0

  sum = 0
  do number over numbers
      sum += number
  end
  return sum/numbers~items

```

Output:

```txt

numbers = 10, 9, 8, 7, 6, 5, 4, 3, 2, 1
average = 5.5

numbers = 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, .11
average = 3.674

numbers = 10, 20, 30, 40, 50, -100, 4.7, -1100
average = -130.6625

numbers =
average = 0

```



## Oz

A version working on floats:

```oz
declare
  fun {Mean Xs}
     {FoldL Xs Number.'+' 0.0} / {Int.toFloat {Length Xs}}
  end
in
  {Show {Mean [3. 1. 4. 1. 5. 9.]}}
```



## PARI/GP


```parigp
avg(v)={
  if(#v,vecsum(v)/#v)
};
```



## Pascal


```pascal
Program Mean;

  function DoMean(vector: array of double): double;
  var
    sum: double;
    i, len: integer;
  begin
    sum := 0;
    len := length(vector);
    if len > 0 then
      begin
      for i := low(vector) to high(vector) do
	sum := sum + vector[i];
      sum := sum / len;
      end;
     DoMean := sum;
  end;

const
  vector: array [3..8] of double = (3.0, 1.0, 4.0, 1.0, 5.0, 9.0);
var
  i: integer;
begin
  writeln('Calculating the arithmetic mean of a series of numbers:');
  write('Numbers: [ ');
  for i := low(vector) to high(vector) do
    write (vector[i]:3:1, ' ');
  writeln (']');
  writeln('Mean: ', DoMean(vector):10:8);
end.
```


Output:

```txt

Calculating the arithmetic mean of a series of numbers:
Numbers: [ 3.0 1.0 4.0 1.0 5.0 9.0 ]
Mean: 3.83333333

```


Alternative version using the Math unit:


```pascal
Program DoMean;
uses math;
const
  vector: array [3..8] of double = (3.0, 1.0, 4.0, 1.0, 5.0, 9.0);
var
  i: integer;
  mean: double;
begin
  writeln('Calculating the arithmetic mean of a series of numbers:');
  write('Numbers: [ ');
  for i := low(vector) to high(vector) do
    write (vector[i]:3:1, ' ');
  writeln (']');
  mean := 0;
  if length(vector) > 0 then
    mean := sum(vector)/length(vector);
  writeln('Mean: ', mean:10:8);
end.
```



## Perl


```perl
sub avg {
  @_ or return 0;
  my $sum = 0;
  $sum += $_ foreach @_;
  return $sum/@_;
}

print avg(qw(3 1 4 1 5 9)), "\n";
```



## Perl 6

{{works with|Rakudo|2015.10-11}}


```perl6
multi mean([]){ Failure.new('mean on empty list is not defined') }; # Failure-objects are lazy exceptions
multi mean (@a) { ([+] @a) / @a }
```



## Phix


```Phix
function mean(sequence s)
    if length(s)=0 then return 0 end if
    return sum(s)/length(s)
end function

? mean({1, 2, 5, -5, -9.5, 3.14159})
```



## PHP


```php
$nums = array(3, 1, 4, 1, 5, 9);
if ($nums)
    echo array_sum($nums) / count($nums), "\n";
else
    echo "0\n";
```



## PL/I


```pli
arithmetic_mean = sum(A)/dimension(A,1);
```



## PicoLisp


```PicoLisp
(de mean (Lst)
   (if (atom Lst)
      0
      (/ (apply + Lst) (length Lst)) ) )
```

Output:

```txt
: (mean (range 1 1000))
-> 500
```



## Pop11



```pop11
define mean(v);
    lvars n = length(v), i, s = 0;
    if n = 0 then
        return(0);
    else
        for i from 1 to n do
            s + v(i) -> s;
        endfor;
    endif;
    return(s/n);
enddefine;
```



## PostScript

<lang>
/findmean{
/x exch def
/sum 0 def
/i 0 def
x length 0 eq
{}
{
x length{
/sum sum x i get add def
/i i 1 add def
}repeat
/sum sum x length div def
}ifelse
sum ==
}def

```


{{libheader|initlib}}
{{works with|Ghostscript}}

```postscript

/avg {
    dup length
    {0 gt} {
       exch 0 {add} fold exch div
    } {
        exch pop
    } ifte
}.

```



## PowerShell

The hard way by calculating a sum and dividing:

```powershell
function mean ($x) {
    if ($x.Count -eq 0) {
        return 0
    } else {
        $sum = 0
        foreach ($i in $x) {
            $sum += $i
        }
        return $sum / $x.Count
    }
}
```

or, shorter, by using the <code>Measure-Object</code> cmdlet which already knows how to compute an average:

```powershell
function mean ($x) {
    if ($x.Count -eq 0) {
        return 0
    } else {
        return ($x | Measure-Object -Average).Average
    }
}
```



## Prolog


{{works with|SWI-Prolog|6.6}}


```prolog

mean(List, Mean) :-
    length(List, Length),
    sumlist(List, Sum),
    Mean is Sum / Length.

```



## PureBasic


```PureBasic
Procedure.d mean(List number())
  Protected sum=0

  ForEach number()
    sum + number()
  Next
  ProcedureReturn sum / ListSize(number())
  ; Depends on programm if zero check needed, returns nan on division by zero
EndProcedure
```



## Python

{{works with|Python|3.0}}.
{{works with|Python|2.6}}

Uses [http://docs.python.org/3.3/library/math.html?highlight=fsum#math.fsum fsum] which tracks multiple partial sums to avoid losing precision

```python
from math import fsum
def average(x):
    return fsum(x)/float(len(x)) if x else 0
print (average([0,0,3,1,4,1,5,9,0,0]))
print (average([1e20,-1e-20,3,1,4,1,5,9,-1e20,1e-20]))
```


{{out}}

```python
2.3
2.3
```



{{works with|Python|2.5}}

```python
def average(x):
    return sum(x)/float(len(x)) if x else 0
print (average([0,0,3,1,4,1,5,9,0,0]))
print (average([1e20,-1e-20,3,1,4,1,5,9,-1e20,1e-20]))
```


{{out}}
(Notice how the second call gave the wrong result)

```python
2.3
1e-21
```



{{works with|Python|2.4}}

```python
def avg(data):
    if len(data)==0:
        return 0
    else:
        return sum(data)/float(len(data))
print avg([0,0,3,1,4,1,5,9,0,0])
```


{{out}}

```python>2.3</lang


{{works with|Python|3.4}}
Since 3.4, Python has a [[http://docs.python.org/3/library/statistics.html statistics] library in the stdlib, which takes care of these precision overflow issues in a way that works for all standard types, not just float, even with values way too big or small to fit in a float. (For Python 2.6-2.7, there's a backport available on PyPI.)

```python>>>
 from statistics import mean
>>> mean([1e20,-1e-20,3,1,4,1,5,9,-1e20,1e-20])
2.3
>>> mean([10**10000, -10**10000, 3, 1, 4, 1, 5, 9, 0, 0])
2.3
>>> mean([10**10000, -10**10000, 3, 1, 4, 1, 5, 9, Fraction(1, 10**10000), Fraction(-1, 10**10000)])
Fraction(23, 10)
>>> big = 10**10000
>>> mean([Decimal(big), Decimal(-big), 3, 1, 4, 1, 5, 9, 1/Decimal(big), -1/Decimal(big)])
Decimal('2.3')
```



## Q

A built-in solution is <tt>avg</tt>. An implementation of it could be:

```q
mean:{(sum x)%count x}
```



## R

R has its <tt>mean</tt> function but it does not allow for NULL (void vectors or whatever) as argument: in this case it raises a warning and the result is NA. An implementation that does not suppress the warning could be:


```rsplus
omean <- function(v) {
  m <- mean(v)
  ifelse(is.na(m), 0, m)
}
```



## Racket


Racket's math library (available in v5.3.2 and newer) comes with a <tt>mean</tt> function that works on arbitrary sequences.


```racket

#lang racket
(require math)

(mean (in-range 0 1000)) ; -> 499 1/2
(mean '(2 2 4 4))        ; -> 3
(mean #(3 4 5 8))        ; -> 5

```



## REBOL


```REBOL
rebol [
    Title: "Arithmetic Mean (Average)"
    URL: http://rosettacode.org/wiki/Average/Arithmetic_mean
]

average: func [v /local sum][
	if empty? v [return 0]

	sum: 0
	forall v [sum: sum + v/1]
	sum / length? v
]

; Note precision loss as spread increased.

print [mold x: [] "->" average x]
print [mold x: [3 1 4 1 5 9] "->" average x]
print [mold x: [1000 3 1 4 1 5 9 -1000] "->" average x]
print [mold x: [1e20 3 1 4 1 5 9 -1e20] "->" average x]
```


Output:


```txt
[] -> 0
[3 1 4 1 5 9] -> 3.83333333333333
[1000 3 1 4 1 5 9 -1000] -> 2.875
[1E+20 3 1 4 1 5 9 -1E+20] -> 0.0
```



## REXX

The vectors (list) can contain any valid (REXX) numbers.

A check is made to validate if the numbers in the list are all numeric.

```rexx
/*REXX program finds the averages/arithmetic mean of several lists (vectors) or CL input*/
parse arg @.1; if @.1=''  then do;   #=6                         /*vector from the C.L.?*/
                               @.1 =   10 9 8 7 6 5 4 3 2 1
                               @.2 =   10 9 8 7 6 5 4 3 2 1 0 0 0 0  .11
                               @.3 =  '10 20 30 40 50  -100  4.7  -11e2'
                               @.4 =  '1 2 3 4  five  6 7 8 9  10.1.  ±2'
                               @.5 =  'World War I  &  World War II'
                               @.6 =                             /*  ◄─── a null value. */
                               end
                          else #=1                               /*number of CL vectors.*/
     do j=1  for #
     say '       numbers = '   @.j
     say '       average = '   avg(@.j)
     say copies('═', 79)
     end   /*t*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
avg: procedure;  parse arg x;     #=words(x)                      /*#:  number of items.*/
     if #==0  then return  'N/A: ───[null vector.]'               /*No words? Return N/A*/
     $=0
          do k=1  for #;      _=word(x,k)                         /*obtain a number.    */
          if datatype(_,'N')  then do;  $=$+_;  iterate;   end    /*if numeric, then add*/
          say left('',40) "***error***  non-numeric: " _;  #=#-1  /*error; adjust number*/
          end   /*k*/

     if #==0  then return  'N/A: ───[no numeric values.]'         /*No nums?  Return N/A*/
     return $ / #                                                 /*return the average. */
```

'''output'''   when using the (internal) lists:

```txt

       numbers =  10 9 8 7 6 5 4 3 2 1
       average =  5.5
═══════════════════════════════════════════════════════════════════════════════
       numbers =  10 9 8 7 6 5 4 3 2 1 0 0 0 0 .11
       average =  3.674
═══════════════════════════════════════════════════════════════════════════════
       numbers =  10 20 30 40 50  -100  4.7  -11e2
       average =  -130.6625
═══════════════════════════════════════════════════════════════════════════════
       numbers =  1 2 3 4  five  6 7 8 9  10.1.  ±2
                                         ***error***  non-numeric:  five
                                         ***error***  non-numeric:  10.1.
                                         ***error***  non-numeric:  ±2
       average =  5
═══════════════════════════════════════════════════════════════════════════════
       numbers =  World War I  &  World War II
                                         ***error***  non-numeric:  World
                                         ***error***  non-numeric:  War
                                         ***error***  non-numeric:  I
                                         ***error***  non-numeric:  &
                                         ***error***  non-numeric:  World
                                         ***error***  non-numeric:  War
                                         ***error***  non-numeric:  II
       average =  N/A: ───[no numeric values.]
═══════════════════════════════════════════════════════════════════════════════
       numbers =
       average =  N/A: ───[null vector.]
═══════════════════════════════════════════════════════════════════════════════


```



## Ring


```ring

nums = [1,2,3,4,5,6,7,8,9,10]
sum = 0
see "Average = " + average(nums) + nl

func average numbers
     for i = 1 to len(numbers)
         sum = sum + nums[i]
     next
     return sum/len(numbers)

```



## RPL/2


This is a simple rewrite of the dc version above. This works on an HP 48. "->" is a single right arrow character on the 48. Feel free to alter this code as necessary to work on RPL/2.


```rpl/2
1 2 3 5 7
AMEAN
   << DEPTH DUP 'N' STO ->LIST ΣLIST N / >>
3.6
```



## Ruby


```ruby
def mean(nums)
  nums.sum(0.0) / nums.size
end

nums = [3, 1, 4, 1, 5, 9]
nums.size.downto(0) do |i|
  ary = nums[0,i]
  puts "array size #{ary.size} : #{mean(ary)}"
end
```

{{out}}

```txt

array size 6 : 3.8333333333333335
array size 5 : 2.8
array size 4 : 2.25
array size 3 : 2.6666666666666665
array size 2 : 2.0
array size 1 : 3.0
array size 0 : NaN

```



## Run BASIC


```runbasic
print "Gimme the number in the array:";input numArray
dim value(numArray)
for i = 1 to numArray
    value(i) = i * 1.5
next

for i = 1 to total
    totValue = totValue +value(numArray)
next
if totValue <> 0 then mean = totValue/numArray
print "The mean is: ";mean
```



## Rust


```rust
fn sum(arr: &[f64]) -> f64 {
    arr.iter().fold(0.0, |p,&q| p + q)
}

fn mean(arr: &[f64]) -> f64 {
    sum(arr) / arr.len() as f64
}

fn main() {
    let v = &[2.0, 3.0, 5.0, 7.0, 13.0, 21.0, 33.0, 54.0];
    println!("mean of {:?}: {:?}", v, mean(v));

    let w = &[];
    println!("mean of {:?}: {:?}", w, mean(w));
}
```

Output:

```txt
mean of [2, 3, 5, 7, 13, 21, 33, 54]: 17.25
mean of []: NaN
```



## Sather

Built to work with VEC, ("geometric" vectors), whose elements must be floats. A 0-dimension vector yields "nan".

```sather
class VECOPS is
  mean(v:VEC):FLT is
    m ::= 0.0;
    loop m := m + v.aelt!; end;
    return m / v.dim.flt;
  end;
end;

class MAIN is
  main is
    v ::= #VEC(|1.0, 5.0, 7.0|);
    #OUT + VECOPS::mean(v) + "\n";
  end;
end;
```



## Scala

Using Scala 2.7, this has to be defined for each numeric type:


```scala
def mean(s: Seq[Int]) = s.foldLeft(0)(_+_) / s.size
```


However, Scala 2.8 gives much more flexibility, but you still have to opt
between integral types and fractional types. For example:


```scala
def mean[T](s: Seq[T])(implicit n: Integral[T]) = {
  import n._
  s.foldLeft(zero)(_+_) / fromInt(s.size)
}
```


This can be used with any subclass of <tt>Sequence</tt> on integral types, up
to and including BigInt. One can also create singletons extending <tt>Integral</tt>
for user-defined numeric classes. Likewise, <tt>Integral</tt> can be replaced by
<tt>Fractional</tt> in the code to support fractional types, such as <tt>Float</tt>
and <tt>Double</tt>.

Alas, Scala 2.8 also simplifies the task in another way:


```scala
def mean[T](s: Seq[T])(implicit n: Fractional[T]) = n.div(s.sum, n.fromInt(s.size))
```


Here we show a function that supports fractional types. Instead of importing the definitions
from <tt>n</tt>, we are calling them on <tt>n</tt> itself. And because we did not import them,
the implicit definitions that would allow us to use <tt>/</tt> were not imported as well.
Finally, we use <tt>sum</tt> instead of <tt>foldLeft</tt>.


## Scheme


```scheme
(define (mean l)
  (if (null? l)
      0
      (/ (apply + l) (length l))))
```


 > (mean (list 3 1 4 1 5 9))
 3 5/6


## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const array float: numVector is [] (1.0, 2.0, 3.0, 4.0, 5.0);

const func float: mean (in array float: numbers) is func
  result
    var float: result is 0.0;
  local
    var float: total is 0.0;
    var float: num is 0.0;
  begin
    if length(numbers) <> 0 then
      for num range numbers do
        total +:= num;
      end for;
      result := total / flt(length(numbers));
    end if;
  end func;

const proc: main is func
  begin
    writeln(mean(numVector));
  end func;
```



## Sidef


```ruby
func avg(Array list) {
    list.len > 0 || return 0;
    list.sum / list.len;
}

say avg([Math.inf, Math.inf]);
say avg([3,1,4,1,5,9]);
say avg([1e+20, 3, 1, 4, 1, 5, 9, -1e+20]);
say avg([10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, 0.11]);
say avg([10, 20, 30, 40, 50, -100, 4.7, -1100]);
```

{{out}}

```txt
inf
3.833333333333333333333333333333333333333
2.875
3.674
-130.6625
```



## Slate



```slate
[|:list| (list reduce: #+ `er ifEmpty: [0]) / (list isEmpty ifTrue: [1] ifFalse: [list size])] applyWith: #(3 1 4 1 5 9).
[|:list| (list reduce: #+ `er ifEmpty: [0]) / (list isEmpty ifTrue: [1] ifFalse: [list size])] applyWith: {}.
```



## Smalltalk


```smalltalk

| numbers |

numbers := #(1 2 3 4 5 6 7 8).
(numbers isEmpty
    ifTrue:[0]
    ifFalse: [
         (numbers inject: 0 into: [:sumSoFar :eachElement | sumSoFar + eachElement]) / numbers size ]
) displayNl.

```

However, the empty check can be omitted, as inject returns the injected value for empty collections, and we probably do not care for the average of nothing (i.e. the division by zero exception):

```smalltalk

| numbers |

numbers := #(1 2 3 4 5 6 7 8).
( numbers inject: 0 into: [:sumSoFar :eachElement | sumSoFar + eachElement]) / numbers size] ) displayNl.

```

also, most Smalltalk's collection classes already provide sum and average methods, which makes it:
{{works with|Pharo}}
{{works with|Smalltalk/X}}

```smalltalk

| numbers |

numbers := #(1 2 3 4 5 6 7 8).
(numbers sum / numbers size) displayNl.

```

or

```smalltalk

| numbers |

numbers := #(1 2 3 4 5 6 7 8).
numbers average displayNl.

```



## SNOBOL4


{{works with|Macro Spitbol}}
{{works with|Snobol4+}}
{{works with|CSnobol}}


```SNOBOL4
        define('avg(a)i,sum') :(avg_end)
avg     i = i + 1; sum = sum + a<i> :s(avg)
        avg = 1.0 * sum / prototype(a) :(return)
avg_end

*       # Fill arrays
        str = '1 2 3 4 5 6 7 8 9 10'; arr = array(10)
loop    i = i + 1; str len(p) span('0123456789') . arr<i> @p :s(loop)
        empty = array(1) ;* Null vector

*       # Test and display
        output = '[' str '] -> ' avg(arr)
        output = '[ ] -> ' avg(empty)
end
```


Output:

```txt
[1 2 3 4 5 6 7 8 9 10] -> 5.5
[ ] -> 0.
```



## SQL

Tested on Oracle 11gR2, the more limited the tool, the more resourceful one becomes :)

```SQL

create table "numbers" ("datapoint" integer);

insert into "numbers" select rownum from tab;

select sum("datapoint")/count(*)  from "numbers";

```

...or...

```SQL
select avg("datapoint") from "numbers";
```



## Standard ML

These functions return a real:


```sml
fun mean_reals [] = 0.0
  | mean_reals xs = foldl op+ 0.0 xs / real (length xs);

val mean_ints = mean_reals o (map real);
```


The previous code is easier to read and understand, though if you want
the fastest implementation to use in production code notice several points:
it is possible to save a call to <code>length</code> computing the length through
the <code>foldl</code>, and for mean_ints it is possible to save calling
<code>real</code> on every numbers, converting only the result of the addition.
Also the task asks to return 0 on empty lists, but in Standard ML this case
would rather be handled by an exception.


```sml
fun mean_reals [] = raise Empty
  | mean_reals xs = let
    val (total, length) =
      foldl
        (fn (x, (tot,len)) => (x + tot, len + 1.0))
        (0.0, 0.0) xs
    in
      (total / length)
    end;


fun mean_ints [] = raise Empty
  | mean_ints xs = let
    val (total, length) =
      foldl
        (fn (x, (tot,len)) => (x + tot, len + 1.0))
        (0, 0.0) xs
    in
      (real total / length)
    end;
```



## Stata


###  Mean of a dataset variable

Illustration of the mean on the population (in millions) in january 2016 of a few european countries (source [http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_gind&lang=fr Eurostat]).
<lang>clear all
input str20 country population
Belgium 11311.1
Bulgaria 7153.8
"Czech Republic" 10553.8
Denmark 5707.3
Germany 82175.7
Estonia 1315.9
Ireland 4724.7
Greece 10783.7
end

. mean population

Mean estimation                   Number of obs   =          8

--------------------------------------------------------------
             |       Mean   Std. Err.     [95% Conf. Interval]
-------------+------------------------------------------------
  population |   16715.75   9431.077     -5585.203     39016.7
--------------------------------------------------------------

. tabstat population, statistic(mean)
    variable |      mean
-------------+----------
  population |  16715.75
------------------------

. quietly summarize population
. display r(mean)
16715.75
```



###  Mean in Mata


```stata
mata
a=11311.1\7153.8\10553.8\5707.3\
82175.7\1315.9\4724.7\10783.7

mean(a)
16715.75
```


## Swift


```swift
func meanDoubles(s: [Double]) -> Double {
  return s.reduce(0, +) / Double(s.count)
}
func meanInts(s: [Int]) -> Double {
  return meanDoubles(s.map{Double($0)})
}
```



## Tcl


```tcl
package require Tcl 8.5
proc mean args {
    if {[set num [llength $args]] == 0} {return 0}
    expr {[tcl::mathop::+ {*}$args] / double($num)}
}
mean 3 1 4 1 5 9 ;# ==> 3.8333333333333335
```


=={{header|TI-83 BASIC}}==

```ti83b
Mean(Ans
```


=={{header|TI-89 BASIC}}==


```ti89b
Define rcmean(nums) = when(dim(nums) = 0, 0, mean(nums))
```



## Trith


```trith
: mean dup empty? [drop 0] [dup [+] foldl1 swap length /] branch ;

[3 1 4 1 5 9] mean
```



## TypeScript


```typescript

function mean(numbersArr)
{
    let arrLen = numbersArr.length;
    if (arrLen > 0) {
        let sum: number = 0;
        for (let i of numbersArr) {
            sum += i;
        }
        return sum/arrLen;
    }
    else return "Not defined";
}

alert( mean( [1,2,3,4,5] ) );
alert( mean( [] ) );

```



## UnixPipes

{{incorrect|UnixPipes|There is a race between parallel commands. <code>cat count</code> might try to read the file before <code>wc -l >count</code> writes it. This may cause an error like ''cat: count: No such file or directory'', then ''bc: stdin:1: syntax error: ) unexpected''.}}

Uses [[ksh93]]-style process substitution. Also overwrites the file named <tt>count</tt> in the current directory.
{{works with|bash}}

```bash
term() {
   b=$1;res=$2
   echo "scale=5;$res+$b" | bc
}

sum() {
  (read B; res=$1;
  test -n "$B" && (term $B $res) || (term 0 $res))
}

fold() {
  func=$1
  (while read a ; do
      fold $func | $func $a
  done)
}

mean() {
  tee >(wc -l > count) | fold sum | xargs echo "scale=5;(1/" $(cat count) ") * " | bc
}

(echo 3; echo 1; echo 4) | mean
```



## UNIX Shell

This example uses <tt>expr</tt>, so it only works with integers. It checks that each string in the list is an integer.


```bash
mean() {
	if expr $# >/dev/null; then
		(count=0
		 sum=0
		 while expr $# \> 0 >/dev/null; do
			sum=`expr $sum + "$1"`
			result=$?
			expr $result \> 1 >/dev/null && exit $result

			count=`expr $count + 1`
			shift
		 done
		 expr $sum / $count)
	else
		echo 0
	fi
}

printf "test 1: "; mean				# 0
printf "test 2: "; mean 300			# 300
printf "test 3: "; mean 300 100 400		# 266
printf "test 4: "; mean -400 400 -1300 200	# -275
printf "test 5: "; mean -			# expr: syntax error
printf "test 6: "; mean 1 2 A 3			# expr: non-numeric argument
```



## Ursa


```ursa
#
# arithmetic mean
#

decl int<> input
decl int i
for (set i 1) (< i (size args)) (inc i)
        append (int args<i>) input
end for

out (/ (+ input) (size input)) endl console
```



## Ursala

There is a library function for means already, although it doesn't cope with
empty vectors. A mean function could be defined as shown for this task.

```Ursala
#import nat
#import flo

mean = ~&?\0.! div^/plus:-0. float+ length

#cast %e

example = mean <5.,3.,-2.,6.,-4.>
```

output:

```txt
1.600000e+00
```




## V


```v
[mean
   [sum 0 [+] fold].
   dup sum
   swap size [[1 <] [1]] when /
].
```



## Vala

Using array to hold the numbers of the list:

```vala

double arithmetic(double[] list){
	double mean;
	double sum = 0;

	if (list.length == 0)
		return 0.0;
	foreach(double number in list){
		sum += number;
	} // foreach

	mean = sum / list.length;

	return mean;
} // end arithmetic mean

public static void main(){
	double[] test = {1.0, 2.0, 5.0, -5.0, 9.5, 3.14159};
	double[] zero_len = {};

	double mean = arithmetic(test);
	double mean_zero = arithmetic(zero_len);

	stdout.printf("%s\n", mean.to_string());
	stdout.printf("%s\n", mean_zero.to_string());
}

```


Output:

```txt

2.6069316666666666
0

```



## VBA


```vb
Private Function mean(v() As Double, ByVal leng As Integer) As Variant
    Dim sum As Double, i As Integer
    sum = 0: i = 0
    For i = 0 To leng - 1
        sum = sum + vv
    Next i
    If leng = 0 Then
        mean = CVErr(xlErrDiv0)
    Else
        mean = sum / leng
    End If
End Function
Public Sub main()
    Dim v(4) As Double
    Dim i As Integer, leng As Integer
    v(0) = 1#
    v(1) = 2#
    v(2) = 2.178
    v(3) = 3#
    v(4) = 3.142
    For leng = 5 To 0 Step -1
        Debug.Print "mean[";
        For i = 0 To leng - 1
            Debug.Print IIf(i, "; " & v(i), "" & v(i));
        Next i
        Debug.Print "] = "; mean(v, leng)
    Next leng
End Sub
```
{{out}}

```txt
mean[1; 2; 2,178; 3; 3,142] =  0
mean[1; 2; 2,178; 3] =  0
mean[1; 2; 2,178] =  0
mean[1; 2] =  0
mean[1] =  0
mean[] = Fout 2007
```


## VBScript


```vb

Function mean(arr)
	size = UBound(arr) + 1
	mean = 0
	For i = 0 To UBound(arr)
		mean = mean + arr(i)
	Next
	mean = mean/size
End Function

'Example
WScript.Echo mean(Array(3,1,4,1,5,9))

```


{{Out}}

```txt
3.83333333333333
```



## Vedit macro language

The numeric data is stored in current edit buffer as ASCII strings, one value per line.

```vedit
#1 = 0			// Sum
#2 = 0			// Count
BOF
While(!At_EOF) {
    #1 += Num_Eval(SIMPLE)
    #2++
    Line(1, ERRBREAK)
}
if (#2) { #1 /= #2 }
Num_Type(#1)
```



## Vim Script

Throws an exception if the list is empty.

```vim
function Mean(lst)
    if empty(a:lst)
        throw "Empty"
    endif
    let sum = 0.0
    for i in a:lst
        let sum += i
    endfor
    return sum / len(a:lst)
endfunction
```



## Wart


```python
def (mean l)
  sum.l / len.l
```


Example run:

```txt
mean '(1 2 3)
=> 2
```



## WDTE


```WDTE>let s =
 import 'stream';
let a => import 'arrays';

let mean nums =>
  a.stream nums
  -> s.reduce [0; 0] (@ s p n => [+ (a.at p 0) 1; + (a.at p 1) n])
  -> (@ s p => / (a.at p 1) (a.at p 0));
```


This is a tad messier than it has to be due to a lack of a way to get the length of an array in WDTE currently.

Usage:

```WDTE
mean [1; 2; 3] -- io.writeln io.stdout;
```


Output:

```txt
2
```



## Wren


```wren
class Arithmetic {
    static mean(arr) {
        if (arr.count == 0) Fiber.abort("Length must be greater than zero")
        return arr.reduce(Fn.new{ |x,y| x+y }) / arr.count
    }
}
Arithmetic.mean([1,2,3,4,5]) // 3

```



## Wortel


```wortel
@let {
  ; using a fork (sum divided-by length)
  mean1 @(@sum / #)

  ; using a function with a named argument
  mean2 &a / @sum a #a

  [[
    !mean1 [3 1 4 1 5 9 2]
    !mean2 [3 1 4 1 5 9 2]
  ]]
}
```

Returns:

```txt
[3.5714285714285716 3.5714285714285716]
```



## XLISP

The specification calls for a function that takes a vector; for convenience, we convert this vector internally to a list. The mean of a zero-length vector is returned as <tt>nil</tt>, equivalent to the empty list or logical <tt>false</tt>.

```lisp
(defun mean (v)
    (if (= (vector-length v) 0)
        nil
        (let ((l (vector->list v)))
            (/ (apply + l) (length l)))))
```



## XPL0


```XPL0
code CrLf=9;
code real RlOut=48;

func real Mean(A, N);
real A;  int N;
real S;  int I;
[if N=0 then return 0.0;
S:= 0.0;
for I:= 0 to N-1 do
        S:= S+A(I);
return S/float(N);
]; \Mean

real Test;
[Test:= [1.0, 2.0, 5.0, -5.0, 9.5, 3.14159];
RlOut(0, Mean(Test, 6));  CrLf(0);
]
```


Output:

```txt

    2.60693

```



## XSLT


Where <code>$values</code> is some variable indicating a set of nodes containing numbers, the average is given by the XPath expression:


```xpath
sum($values) div count($values)
```



### Runnable example



```xml
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
	<xsl:output method="text"/>

	<xsl:template match="/">
		<xsl:variable name="values" select="/*/*"/>
		<xsl:value-of select="sum($values) div count($values)"/>
	</xsl:template>
</xsl:stylesheet>
```


Sample input:


```xml><numbers

	<!-- Average is 2.4 -->
	<number>1</number>
	<number>1</number>
	<number>2</number>
	<number>3</number>
	<number>5</number>
</numbers>
```



## Yorick


```yorick
func mean(x) {
    if(is_void(x)) return 0;
    return x(*)(avg);
}
```



## zkl

Converts int to floats (implicitly):

```zkl
fcn mean(a,b,c,etc){ z:=vm.arglist; z.reduce('+,0.0)/z.len() }
mean(3,1,4,1,5,9);  //-->3.83333
mean(); //-->Exception thrown: MathError(NaN (Not a number))
```

To pass in a vector/list:

```zkl
fcn meanV(z){ z.reduce('+,0.0)/z.len() }
meanV(T(3,1,4,1,5,9)); // --> 3.83333
```



## zonnon


```zonnon

module Averages;
type
	Vector = array {math} * of real;

	procedure ArithmeticMean(x: Vector): real;
	begin
		(* sum is a predefined function for mathematical arrays *)
		return sum(x)
	end ArithmeticMean;
var
	x: Vector;

begin
	x := new Vector(4);
	x := [1.0, 2.3, 3.2, 2.1, 5.3];
	write("arithmetic mean: ");writeln(ArithmeticMean(x):10:2)
end Averages.

```

{{out}}

```txt

arithmetic mean:       13,9

```

