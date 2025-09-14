+++
title = "Loops/with multiple ranges"
description = ""
date = 2019-10-15T10:52:20Z
aliases = []
[extra]
id = 21990
[taxonomies]
categories = ["task", "Iteration"]
tags = []
languages = [
  "algol_60",
  "algol_68",
  "algol_w",
  "c",
  "factor",
  "go",
  "groovy",
  "j",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "prolog",
  "purebasic",
  "python",
  "related_tasks",
  "rexx",
  "ruby",
  "vba",
  "visual_basic_dotnet",
  "zkl",
]
+++

Some languages allow multiple '''loop''' ranges, such as the '''PL/I''' example (snippet) below.


```pli
                                       /* all variables are DECLARED as integers. */
          prod=  1;                    /*start with a product of unity.           */
           sum=  0;                    /*  "     "  "   sum    " zero.            */
             x= +5;
             y= -5;
             z= -2;
           one=  1;
         three=  3;
         seven=  7;
                                       /*(below)  **  is exponentiation:  4**3=64 */
           do j=   -three  to     3**3        by three   ,
                   -seven  to   +seven        by   x     ,
                      555  to      550 - y               ,
                       22  to      -28        by -three  ,
                     1927  to     1939                   ,
                        x  to        y        by   z     ,
                    11**x  to    11**x + one;
                                                        /* ABS(n) = absolute value*/
           sum= sum + abs(j);                           /*add absolute value of J.*/
           if abs(prod)<2**27 & j¬=0  then prod=prod*j; /*PROD is small enough & J*/
           end;                                         /*not 0, then multiply it.*/
                     /*SUM and PROD are used for verification of J incrementation.*/
         display (' sum= ' ||  sum);                    /*display strings to term.*/
         display ('prod= ' || prod);                    /*   "       "     "   "  */
```



## Task

Simulate/translate the above '''PL/I''' program snippet as best as possible in your
language,   with particular emphasis on the   '''do'''   loop construct.

The   '''do'''   index must be incremented/decremented in the same order shown.

If feasible, add commas to the two output numbers (being displayed).

Show all output here.
<lang>      A simple PL/I   DO  loop  (incrementing or decrementing)  has the construct of:

            DO variable = start_expression    {TO ending_expression]       {BY increment_expression} ;
                 ---or---
            DO variable = start_expression    {BY increment_expression}    {TO ending_expression]    ;

      where it is understood that all expressions will have a value.  The  variable  is normally a
      scaler variable,  but need not be  (but for this task, all variables and expressions are declared
      to be scaler integers).   If the   BY   expression is omitted,  a   BY   value of unity is used.
      All expressions are evaluated before the   DO   loop is executed,  and those values are used
      throughout the   DO   loop execution   (even though, for instance,  the value of   Z   may be
      changed within the   DO   loop.    This isn't the case here for this task.

      A multiple-range   DO   loop can be constructed by using a comma (,) to separate additional ranges
      (the use of multiple   TO   and/or   BY   keywords).     This is the construct used in this task.

      There are other forms of   DO   loops in PL/I involving the  WHILE  clause,  but those won't be
      needed here.    DO  loops without a   TO   clause might need a   WHILE   clause  or some other
      means of exiting the loop  (such as  LEAVE,  RETURN,  SIGNAL,  GOTO,  or  STOP),  or some other
      (possible error) condition that causes transfer of control outside the  DO  loop.

      Also, in PL/I, the check if the   DO   loop index value is outside the range is made at the
      "head"  (start)  of the   DO  loop,  so it's possible that the   DO   loop isn't executed,  but
      that isn't the case for any of the ranges used in this task.

      In the example above, the clause:                    x    to y       by z
      will cause the variable   J   to have to following values  (in this order):  5  3  1  -1  -3  -5

      In the example above, the clause:                 -seven  to +seven  by x
      will cause the variable   J   to have to following values  (in this order):  -7  -2   3
```



## Related tasks

*   [[Loop over multiple arrays simultaneously]]
*   [[Loops/Break]]
*   [[Loops/Continue]]
*   [[Loops/Do-while]]
*   [[Loops/Downward for]]
*   [[Loops/For]]
*   [[Loops/For with a specified step]]
*   [[Loops/Foreach]]
*   [[Loops/Increment loop index within loop body]]
*   [[Loops/Infinite]]
*   [[Loops/N plus one half]]
*   [[Loops/Nested]]
*   [[Loops/While]]
*   [[Loops/with multiple ranges]]
*   [[Loops/Wrong ranges]]





## ALGOL 60

```algol60
begin
  integer prod, sum, x, y, z, one, three, seven;
  integer j;
  prod := 1;
  sum := 0;
  x := 5; y := -5; z := -2;
  one := 1;
  three := 3;
  seven := 7;

  for j := -three  step  three  until 3^3    ,
           -seven  step      x  until seven  ,
              555  step      1  until 550 - y,
               22  step -three  until -28    ,
             1927  step      1  until 1939   ,
                x  step      z  until y      ,
             11^x  step      1  until 11^x + one
  do begin
    sum := sum + iabs(j);
    if iabs(prod) < 2^27 & j != 0 then prod := prod*j
  end;

  outstring(1, " sum= "); outinteger(1, sum);  outstring(1, "\n");
  outstring(1, "prod= "); outinteger(1, prod); outstring(1, "\n")
end

```

```txt
 sum= 348173
prod= -793618560

```



## ALGOL 68

As with most of the other languages, Algol 68 doesn't support multiple loop ranges, so a sequence pf loops is used instead.

```algol68
BEGIN
    # translation of task PL/1 code, with minimal changes, semicolons required by      #
    # PL/1 but not allowed in Algol 68 removed, unecessary rounding removed            #
    # Note that in Algol 68, the loop counter is a local variable to the loop and      #
    # the value of j is not available outside the loops                                #
    PROC loop body = ( INT j )VOID:          #(below)  **  is exponentiation:  4**3=64 #
    BEGIN sum +:= ABS j;                                      #add absolute value of J.#
          IF ABS prod<2**27 AND j /= 0 THEN prod *:= j FI     #PROD is small enough & J#
                                                              # ABS(n) = absolute value#
          END;                                                #not 0, then multiply it.#
                           #SUM and PROD are used for verification of J incrementation.#
     INT  prod :=  1;                        #start with a product of unity.           #
     INT   sum :=  0;                        #  "     "  "   sum    " zero.            #
     INT     x := +5;
     INT     y := -5;
     INT     z := -2;
     INT   one :=  1;
     INT three :=  3;
     INT seven :=  7;
         FOR j FROM -three  BY  three TO      ( 3**3 )        DO loop body( j ) OD;
         FOR j FROM -seven  BY  x     TO    +seven            DO loop body( j ) OD;
         FOR j FROM    555            TO    550 - y           DO loop body( j ) OD;
         FOR j FROM     22  BY -three TO   -28                DO loop body( j ) OD;
         FOR j FROM   1927            TO  1939                DO loop body( j ) OD;
         FOR j FROM      x  BY  z     TO     y                DO loop body( j ) OD;
         FOR j FROM      ( 11**x )    TO      ( 11**x ) + one DO loop body( j ) OD;
         print((" sum= ", whole( sum,0), newline));           #display strings to term.#
         print(("prod= ", whole(prod,0), newline))            #   "       "     "   "  #
END

```

```txt

 sum= 348173
prod= -793618560

```



## ALGOL W

As with most of the other languages, Algol W doesn't support multiple loop ranges, so a sequence pf loops is used instead.

```algolw
begin
    % translation of task PL/1 code, with minimal changes, semicolons required by      %
    % PL/1 but redundant in Algol W retained ( technically they introduce empty        %
    % statements after the "if" in the loop body and before the final "end" )          %
    % Note that in Algol W, the loop counter is a local variable to the loop and       %
    % the value of j is not available outside the loops                                %
    procedure loopBody ( integer value j );  %(below)  **  is exponentiation:  4**3=64 %
    begin sum := sum + abs(j);                                %add absolute value of J.%
          if abs(prod)<2**27 and j not = 0 then prod := prod*j; %PROD is small enough & J%
                                                              % ABS(n) = absolute value%
          end;                                                %not 0, then multiply it.%
                           %SUM and PROD are used for verification of J incrementation.%
      integer prod, sum, x, y, z, one, three, seven;
          prod :=  1;                        %start with a product of unity.           %
           sum :=  0;                        %  "     "  "   sum    " zero.            %
             x := +5;
             y := -5;
             z := -2;
           one :=  1;
         three :=  3;
         seven :=  7;
         for j :=   -three  step  three until round( 3**3 )        do loopBody( j );
         for j :=   -seven  step  x     until    +seven            do loopBody( j );
         for j :=      555              until    550 - y           do loopBody( j );
         for j :=       22  step -three until   -28                do loopBody( j );
         for j :=     1927              until  1939                do loopBody( j );
         for j :=        x  step  z     until     y                do loopBody( j );
         for j := round( 11**x )        until round( 11**x ) + one do loopBody( j );
         write(s_w := 0, " sum= ",  sum);                    %display strings to term.%
         write(s_w := 0, "prod= ", prod);                    %   "       "     "   "  %
end.
```

```txt

 sum=         348173
prod=     -793618560

```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <locale.h>

long prod = 1L, sum = 0L;

void process(int j) {
    sum += abs(j);
    if (labs(prod) < (1 << 27) && j) prod *= j;
}

long ipow(int n, uint e) {
    long pr = n;
    int i;
    if (e == 0) return 1L;
    for (i = 2; i <= e; ++i) pr *= n;
    return pr;
}

int main() {
    int j;
    const int x = 5, y = -5, z = -2;
    const int one = 1, three = 3, seven = 7;
    long p = ipow(11, x);
    for (j = -three; j <= ipow(3, 3); j += three) process(j);
    for (j = -seven; j <= seven; j += x) process(j);
    for (j = 555; j <= 550 - y; ++j) process(j);
    for (j = 22; j >= -28; j -= three) process(j);
    for (j = 1927; j <= 1939; ++j) process(j);
    for (j = x; j >= y; j -= -z) process(j);
    for (j = p; j <= p + one; ++j) process(j);
    setlocale(LC_NUMERIC, "");
    printf("sum  = % 'ld\n", sum);
    printf("prod = % 'ld\n", prod);
    return 0;
}
```


```txt

sum  =  348,173
prod = -793,618,560

```



## Factor

Factor doesn't have any special support for this sort of thing, but we can store iterable <code>range</code> objects in a collection and loop over them.

```factor
USING: formatting kernel locals math math.functions math.ranges
sequences sequences.generalizations tools.memory.private ;

[let                            ! Allow lexical variables.
     1 :> prod!                 ! Start with a product of unity.
     0 :> sum!                  !   "     "  "   sum    " zero.
     5 :> x
    -5 :> y
    -2 :> z
     1 :> one
     3 :> three
     7 :> seven

    three neg 3 3 ^ three <range>              ! Create array
    seven neg seven x     <range>              ! of 7 ranges.
    555 550 y -             [a,b]
    22 -28 three neg      <range>
    1927 1939               [a,b]
    x y z                 <range>
    11 x ^ 11 x ^ 1 +       [a,b] 7 narray

    [
        [
            :> j j abs sum + sum!
            prod abs 2 27 ^ < j zero? not and
            [ prod j * prod! ] when
        ] each                      ! Loop over range.
    ] each                          ! Loop over array of ranges.

    ! SUM and PROD are used for verification of J incrementation.
    sum prod [ commas ] bi@ " sum=  %s\nprod= %s\n" printf
]
```

```txt

 sum=  348,173
prod= -793,618,560

```



## Go

Nothing fancy from Go here (is there ever?), just a series of individual for loops.

```go
package main

import "fmt"

func pow(n int, e uint) int {
    if e == 0 {
        return 1
    }
    prod := n
    for i := uint(2); i <= e; i++ {
        prod *= n
    }
    return prod
}

func abs(n int) int {
    if n >= 0 {
        return n
    }
    return -n
}

func commatize(n int) string {
    s := fmt.Sprintf("%d", n)
    if n < 0 {
        s = s[1:]
    }
    le := len(s)
    for i := le - 3; i >= 1; i -= 3 {
        s = s[0:i] + "," + s[i:]
    }
    if n >= 0 {
        return " " + s
    }
    return "-" + s
}

func main() {
    prod := 1
    sum := 0
    const (
        x     = 5
        y     = -5
        z     = -2
        one   = 1
        three = 3
        seven = 7
    )
    p := pow(11, x)
    var j int

    process := func() {
        sum += abs(j)
        if abs(prod) < (1<<27) && j != 0 {
            prod *= j
        }
    }

    for j = -three; j <= pow(3, 3); j += three {
        process()
    }
    for j = -seven; j <= seven; j += x {
        process()
    }
    for j = 555; j <= 550-y; j++ {
        process()
    }
    for j = 22; j >= -28; j -= three {
        process()
    }
    for j = 1927; j <= 1939; j++ {
        process()
    }
    for j = x; j >= y; j -= -z {
        process()
    }
    for j = p; j <= p+one; j++ {
        process()
    }
    fmt.Println("sum  = ", commatize(sum))
    fmt.Println("prod = ", commatize(prod))
}
```


```txt

sum  =   348,173
prod =  -793,618,560

```



## Groovy

Solution:

```groovy
def (prod, sum, x, y, z, one, three, seven) = [1, 0, +5, -5, -2, 1, 3, 7]

for (
    j in (
        ((-three) .. (3**3)       ).step(three)
      + ((-seven) .. (+seven)     ).step(x)
      + (555      .. (550-y)      )
      + (22       .. (-28)        ).step(three)    // This is correct!
      // Groovy interprets positive step size as stride through the LIST ELEMENTS as ordered
      // and negative step size as stride through the REVERSED LIST ELEMENTS as ordered
      //   so step(-3) gives:   -28, -25, -22, ... ,  20
      //   while step(3) gives:  22,  19,  16, ... , -26
      + (1927     .. 1939         )
      + (x        .. y            ).step(z)
      + (11**x    .. (11**x + one))
    )
) {

    sum = sum + j.abs()
    if ( prod.abs() < 2**27 && j != 0) prod *= j
}

println " sum= ${sum}"
println "prod= ${prod}"
```


Output:

```txt
 sum= 348177
prod= -793618560
```



## J

J uses the names x, y, m, n, u, v to pass arguments into explicit definitions.  Treating these as reserved names is reasonable practice.  Originally these had been x. , y. etceteras however the dots must have been deemed "noisy".

We've passed the range list argument literally for evaluation in local scope.  Verb f evaluates and concatenates the ranges, then perhaps the ensuing for. loop looks somewhat like familiar code.


```j

NB. http://rosettacode.org/wiki/Loops/Wrong_ranges#J
NB. define range as a linear polynomial
start =: 0&{
stop =: 1&{
increment =: 2&{ :: 1:  NB. on error use 1
range =: (start , increment) p. [: i. [: >: [: <. (stop - start) % increment

f =: 3 :0
 input =. y
 'prod sum x y z one three seven' =. 1 0 5 _5 _2 1 3 7
 J =. ([: ; range&.>) ". input
 for_j. J do.
  sum =. sum + | j
  if. ((|prod)<2^27) *. (0 ~: j) do.
   prod =. prod * j
  end.
 end.
 sum , prod
)

```



```txt

   ] A =: f '((-three), (3^3), three); ((-seven),seven,x); (555 , 550-y); (22 _28, -three); 1927 1939; (x,y,z); (0 1 + 11^x)'
348173 _7.93619e8

   20j0 ": A
              348173          _793618560

```



## Julia

Julia allows concatenation of iterators with the ; iterator within a vector. An attempt was made to preserve the shape of the PL/1 code.

```julia
using Formatting

function PL1example()

                                    # all variables are DECLARED as integers.
    prod  =  1;                     # start with a product of unity.
    sum   =  0;                     #   "     "  "   sum    " zero.
    x     = +5;
    y     = -5;
    z     = -2;
    one   =  1;
    three =  3;
    seven =  7;
                                    # (below)  **  is exponentiation:  4**3=64
    for j in [           -three   :  three :  3^3           ;
                         -seven   :   x    :  +seven        ;
                            555            :  550 - y       ;
                             22   : -three :  -28           ;
                           1927            :  1939          ;
                              x   :  z     :  y             ;
                           11^x            :   11^x + one   ]
                                                        # ABS(n) = absolute value
        sum = sum + abs(j);                             # add absolute value of J.
        if abs(prod) < 2^27 && j !=0 prod = prod*j      # PROD is small enough & J
        end;                                            # not 0, then multiply it.
    end             # SUM and PROD are used for verification of J incrementation.
    println(" sum = $(format(sum, commas=true))");      # display strings to term.
    println("prod = $(format(prod, commas=true))");     #   "       "     "   "
end

PL1example()

```
 {{output}}
```txt

     sum = 348,173
    prod = -793,618,560

```



## Kotlin

Nothing special here, just a series of individual for loops.

```scala
// Version 1.2.70

import kotlin.math.abs

infix fun Int.pow(e: Int): Int {
    if (e == 0) return 1
    var prod = this
    for (i in 2..e) {
        prod *= this
    }
    return prod
}

fun main(args: Array<String>) {
    var prod = 1
    var sum = 0
    val x = 5
    val y = -5
    val z = -2
    val one = 1
    val three = 3
    val seven = 7
    val p = 11 pow x
    fun process(j: Int) {
        sum += abs(j)
        if (abs(prod) < (1 shl 27) && j != 0) prod *= j
    }

    for (j in -three..(3 pow 3) step three) process(j)
    for (j in -seven..seven step x) process(j)
    for (j in 555..550-y) process(j)
    for (j in 22 downTo -28 step three) process(j)
    for (j in 1927..1939) process(j)
    for (j in x downTo y step -z) process(j)
    for (j in p..p + one) process(j)
    System.out.printf("sum  = % ,d\n", sum)
    System.out.printf("prod = % ,d\n", prod)
}
```


```txt

sum  =  348,173
prod = -793,618,560

```



## Perl


```perl>use constant   one =
  1;
use constant three =>  3;
use constant seven =>  7;
use constant     x =>  5;
use constant    yy => -5; # 'y' conflicts with use as equivalent to 'tr' operator (a carry-over from 'sed')
use constant     z => -2;

my $prod = 1;

sub from_to_by {
    my($begin,$end,$skip) = @_;
    my $n = 0;
    grep{ !($n++ % abs $skip) } $begin <= $end ? $begin..$end : reverse $end..$begin;
}

sub commatize {
    (my $s = reverse shift) =~ s/(.{3})/$1,/g;
    $s =~ s/,(-?)$/$1/;
    $s = reverse $s;
}

for my $j (
    from_to_by(-three,3**3,three),
    from_to_by(-seven,seven,x),
    555 .. 550 - yy,
    from_to_by(22,-28,-three),
    1927 .. 1939,
    from_to_by(x,yy,z),
    11**x .. 11**x+one,
   ) {
     $sum  += abs($j);
     $prod *= $j if $j and abs($prod) < 2**27;
}

printf "%-8s %12s\n", 'Sum:',     commatize $sum;
printf "%-8s %12s\n", 'Product:', commatize $prod;
```

```txt
Sum:          348,173
Product: -793,618,560
```



## Perl 6


This task is really conflating two separate things, (at least in Perl 6). Sequences and loops are two different concepts and may be considered / implemented separately from each other.

Yes, you can generate a sequence with a loop, and a loop can use a sequence for an iteration value, but the two are somewhat orthogonal and don't necessarily overlap.

Sequences are first class objects in Perl 6. You can (and typically do) generate a sequence using the (appropriately enough) sequence operator and can assign it to a variable and/or pass it as a parameter; the entire sequence, not just it's individual values. It ''may'' be used in a looping construct, but it is not necessary to do so.

Various looping constructs often do use sequences as their iterator but not exclusively, possibly not even in the majority.


Displaying the j sequence as well since it isn't very large.


```perl6
sub comma { ($^i < 0 ?? '-' !! '') ~ $i.abs.flip.comb(3).join(',').flip }

my \x     =  5;
my \y     = -5;
my \z     = -2;
my \one   =  1;
my \three =  3;
my \seven =  7;

my $j = flat
  ( -three, *+three … 3³         ),
  ( -seven, *+x     …^ * > seven ),
  ( 555   .. 550 - y             ),
  ( 22,     *-three …^ * < -28   ),
  ( 1927  .. 1939                ),
  ( x,      *+z     …^ * < y     ),
  ( 11**x .. 11**x + one         );

put 'j sequence: ', $j;
put '       Sum: ', comma [+] $j».abs;
put '   Product: ', comma ([\*] $j.grep: so +*).first: *.abs > 2²⁷;

# Or, an alternate method for generating the 'j' sequence, employing user-defined
# operators to preserve the 'X to Y by Z' layout of the example code.
# Note that these operators will only work for monotonic sequences.

sub infix:<to> { $^a ... $^b }
sub infix:<by> { $^a[0, $^b.abs ... *] }

$j = cache flat
    -three  to          3**3  by  three ,
    -seven  to         seven  by      x ,
       555  to     (550 - y)            ,
        22  to           -28  by -three ,
      1927  to          1939  by    one ,
         x  to             y  by      z ,
     11**x  to (11**x + one)            ;

put "\nLiteral minded variant:";
put '       Sum: ', comma [+] $j».abs;
put '   Product: ', comma ([\*] $j.grep: so +*).first: *.abs > 2²⁷;
```

```txt
j sequence: -3 0 3 6 9 12 15 18 21 24 27 -7 -2 3 555 22 19 16 13 10 7 4 1 -2 -5 -8 -11 -14 -17 -20 -23 -26 1927 1928 1929 1930 1931 1932 1933 1934 1935 1936 1937 1938 1939 5 3 1 -1 -3 -5 161051 161052
       Sum: 348,173
   Product: -793,618,560

Literal minded variant:
       Sum: 348,173
   Product: -793,618,560

```



## Phix


```Phix
integer prod =  1,
       total =  0,  -- (renamed as sum is a Phix builtin)
           x = +5,
           y = -5,
           z = -2,
         one =  1,
       three =  3,
       seven =  7

sequence loopset = {{     -three,        power(3,3), three },
                    {     -seven,            +seven,   x   },
                    {        555,           550 - y,   1   },
                    {         22,               -28, -three},
                    {       1927,              1939,   1   },
                    {          x,                 y,   z   },
                    {power(11,x), power(11,x) + one,   1   }}

for i=1 to length(loopset) do
    integer {f,t,s} = loopset[i]
    for j=f to t by s do
        total += abs(j)
        if abs(prod)<power(2,27) and j!=0 then
            prod *= j
        end if
    end for
end for
printf(1," sum = %,d\n",total)
printf(1,"prod = %,d\n",prod)
```

```txt

 sum = 348,173
prod = -793,618,560

```



## Prolog

Prolog does not have the richness of some other languages where it comes to loops, variables and the like, but does have some rather interesting features such as difference lists and backtracking for generating solutions.

```prolog
for(Lo,Hi,Step,Lo)  :- Step>0, Lo=<Hi.
for(Lo,Hi,Step,Val) :- Step>0, plus(Lo,Step,V), V=<Hi, !, for(V,Hi,Step,Val).
for(Hi,Lo,Step,Hi)  :- Step<0, Lo=<Hi.
for(Hi,Lo,Step,Val) :- Step<0, plus(Hi,Step,V), Lo=<V, !, for(V,Lo,Step,Val).

sym(x,5).                 % symbolic lookups for values
sym(y,-5).
sym(z,-2).
sym(one,1).
sym(three,3).
sym(seven,7).

range(-three,3^3,three).  % as close as we can syntactically get
range(-seven,seven,x).
range(555,550-y,1).
range(22,-28, -three).
range(1927,1939,1).
range(x,y,z).
range(11^x,11^x+one,1).

translate(V, V)   :- number(V), !.    % difference list based parser
translate(S, V)   :- sym(S,V), !.
translate(-S, V)  :- translate(S,V0), !, V is -V0.
translate(A+B, V) :- translate(A,A0), translate(B, B0), !, V is A0+B0.
translate(A-B, V) :- translate(A,A0), translate(B, B0), !, V is A0-B0.
translate(A^B, V) :- translate(A,A0), translate(B, B0), !, V is A0^B0.

range_value(Val) :-             % enumerate values for all ranges in order
	range(From,To,Step),
	translate(From,F), translate(To,T), translate(Step,S),
	for(F,T,S,Val).

calc_values([], S, P, S, P).    % calculate all values in generated order
calc_values([J|Js], S, P, Sum, Product) :-
  S0 is S + abs(J), ((abs(P)< 2^27, J \= 0) -> P0 is P * J; P0=P),
  !, calc_values(Js, S0, P0, Sum, Product).

calc_values(Sum, Product) :-    % Find the sum and product
	findall(V, range_value(V), Values),
	calc_values(Values, 0, 1, Sum, Product).
```



```txt
?- calc_values(Sum, Product).
Sum = 348173,
Product = -793618560.
```



## PureBasic


```purebasic
#X = 5 : #Y = -5 : #Z = -2
#ONE   = 1 : #THREE = 3 : #SEVEN = 7
Define j.i
Global prod.i = 1, sum.i = 0

Macro ipow(n, e)
  Int(Pow(n, e))
EndMacro

Macro ifn(x)
  FormatNumber(x,0,".",",")
EndMacro

Macro loop_for(start, stop, step_for=1)
  For j = start To stop Step step_for
    proc(j)
  Next
EndMacro

Procedure proc(j.i)
  sum + Abs(j)
  If (Abs(prod) < ipow(2 , 27)) And (j<>0)
    prod * j
  EndIf
EndProcedure

loop_for(-#THREE, ipow(3, 3), #THREE)
loop_for(-#SEVEN, #SEVEN, #X)
loop_for(555, 550 - #Y)
loop_for(22, -28, -#THREE)
loop_for(1927, 1939)
loop_for(#X, #Y, #Z)
loop_for(ipow(11, #X), ipow(11, #X) + 1)

If OpenConsole("Loops/with multiple ranges")
  PrintN("sum  = " + ifn(sum))
  PrintN("prod = " + ifn(prod))
  Input()
EndIf
```

```txt
sum  = 348,173
prod = -793,618,560
```



## Python

Pythons range function does not include the second argument hence the definition of _range()

```python
from itertools import chain

prod, sum_, x, y, z, one,three,seven = 1, 0, 5, -5, -2, 1, 3, 7

def _range(x, y, z=1):
    return range(x, y + (1 if z > 0 else -1), z)

print(f'list(_range(x, y, z)) = {list(_range(x, y, z))}')
print(f'list(_range(-seven, seven, x)) = {list(_range(-seven, seven, x))}')

for j in chain(_range(-three, 3**3, three), _range(-seven, seven, x),
               _range(555, 550 - y), _range(22, -28, -three),
               _range(1927, 1939), _range(x, y, z),
               _range(11**x, 11**x + 1)):
    sum_ += abs(j)
    if abs(prod) < 2**27 and (j != 0):
        prod *= j
print(f' sum= {sum_}\nprod= {prod}')
```


```txt
list(_range(x, y, z)) = [5, 3, 1, -1, -3, -5]
list(_range(-seven, seven, x)) = [-7, -2, 3]
 sum= 348173
prod= -793618560
```



## REXX

Programming note:   the (sympathetic) trailing semicolons (''';''') after each REXX
statement are optional,   they are only there to mimic what the '''PL/I''' language
requires after each statement.

The technique used by this REXX version is to "break up" the
various   '''do'''   iterating clauses (ranges) into
separate   '''do'''   loops,   and have them invoke a subroutine to perform the actual computations.

```rexx
/*REXX program emulates a multiple─range  DO  loop  (all variables can be any numbers). */
 prod=  1;
  sum=  0;
    x= +5;
    y= -5;
    z= -2;
  one=  1;
three=  3;
seven=  7;

      do j=   -three  to      3**3      by three  ;      call meat;      end;
      do j=   -seven  to    seven       by   x    ;      call meat;      end;
      do j=      555  to      550 - y             ;      call meat;      end;
      do j=       22  to      -28       by -three ;      call meat;      end;
      do j=     1927  to     1939                 ;      call meat;      end;
      do j=        x  to        y       by   z    ;      call meat;      end;
      do j=    11**x  to    11**x + one           ;      call meat;      end;

say ' sum= ' || commas( sum);                          /*display   SUM   with commas.   */
say 'prod= ' || commas(prod);                          /*   "     PROD     "     "      */
exit;                                                  /*stick a fork in it, we're done.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: procedure; parse arg _;     n= _'.9';     #= 123456789;     b= verify(n, #, "M")
                                    e= verify(n, #'0', , verify(n, #"0.", 'M') )  - 4
          do j=e  to b  by -3;      _= insert(',', _, j);   end;                  return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
meat:  sum= sum + abs(j);
       if abs(prod)<2**27 & j\==0  then prod= prod * j;
       return;
```

```txt

 sum= 348,173
prod= -793,618,560

```


## Ruby

Uses chaining of enumerables, which was introduced with Ruby 2.6

```Ruby
x, y, z, one, three, seven = 5, -5, -2, 1, 3, 7

enums = (-three).step(3**3, three) +
        (-seven).step(seven, x) +
        555     .step(550-y, -1) +
        22      .step(-28, -three) +
        (1927..1939) +                # just toying, 1927.step(1939) is fine too
        x       .step(y, z) +
        (11**x) .step(11**x + one)
# enums is an enumerator, consisting of a bunch of chained enumerators,
# none of which has actually produced a value.

puts "Sum of absolute numbers:  #{enums.sum(&:abs)}"
prod = enums.inject(1){|prod, j| ((prod.abs < 2**27) && j!=0) ? prod*j : prod}
puts "Product (but not really): #{prod}"

```

```txt
Sum of absolute numbers:  348173
Product (but not really): -793618560

```



## VBA


```VB
Dim prod As Long, sum As Long
Public Sub LoopsWithMultipleRanges()
    Dim x As Integer, y As Integer, z As Integer, one As Integer, three As Integer, seven As Integer, j As Long
    prod = 1
    sum = 0
    x = 5
    y = -5
    z = -2
    one = 1
    three = 3
    seven = 7
    For j = -three To pow(3, 3) Step three: Call process(j): Next j
    For j = -seven To seven Step x: Call process(j): Next j
    For j = 555 To 550 - y: Call process(j): Next j
    For j = 22 To -28 Step -three: Call process(j): Next j
    For j = 1927 To 1939: Call process(j): Next j
    For j = x To y Step z: Call process(j): Next j
    For j = pow(11, x) To pow(11, x) + one: Call process(j): Next j
    Debug.Print " sum= " & Format(sum, "#,##0")
    Debug.Print "prod= " & Format(prod, "#,##0")
End Sub
Private Function pow(x As Long, y As Integer) As Long
    pow = WorksheetFunction.Power(x, y)
End Function
Private Sub process(x As Long)
    sum = sum + Abs(x)
    If Abs(prod) < pow(2, 27) And x <> 0 Then prod = prod * x
End Sub
```

```txt
 sum= 348.173
prod= -793.618.560
```



## Visual Basic .NET

VB.NET loops can't have multiple ranges, so this implementation will use the For Each loop and demonstrate various functions that produce concatenated ranges.

Composite formatting is used to add digit separators.

Using the following to provide the functionality of the For loop as a function,

```vbnet
Partial Module Program
    ' Stop and Step are language keywords and must be escaped with brackets.
    Iterator Function Range(start As Integer, [stop] As Integer, Optional [step] As Integer = 1) As IEnumerable(Of Integer)
        For i = start To [stop] Step [step]
            Yield i
        Next
    End Function
End Module
```


and Enumerable.Concat (along with extension method syntax) to splice the ranges, the program ends up looking like this:


```vbnet
Imports System.Globalization

Partial Module Program
    Sub Main()
        ' All variables are inferred to be of type Integer.
        Dim prod = 1,
            sum = 0,
            x = +5,
            y = -5,
            z = -2,
            one = 1,
            three = 3,
            seven = 7

        ' The exponent operator compiles to a call to Math.Pow, which returns Double, and so must be converted back to Integer.
        For Each j In Range(-three,       CInt(3 ^ 3),        3     ).
               Concat(Range(-seven,       +seven,             x     )).
               Concat(Range(555,          550 - y                   )).
               Concat(Range(22,           -28,                -three)).
               Concat(Range(1927,         1939                      )).
               Concat(Range(x,            y,                  z     )).
               Concat(Range(CInt(11 ^ x), CInt(11 ^ x) + one        ))

            sum = sum + Math.Abs(j)
            If Math.Abs(prod) < 2 ^ 27 AndAlso j <> 0 Then prod = prod * j
        Next

        ' The invariant format info by default has two decimal places.
        Dim format As New NumberFormatInfo() With {
            .NumberDecimalDigits = 0
        }

        Console.WriteLine(String.Format(format, " sum= {0:N}", sum))
        Console.WriteLine(String.Format(format, "prod= {0:N}", prod))
    End Sub
End Module
```


To improve the program's appearance, a ConcatRange method can be defined to combine the two method calls,

```vbnet>    <Runtime.CompilerServices.Extension

    Function ConcatRange(source As IEnumerable(Of Integer), start As Integer, [stop] As Integer, Optional [step] As Integer = 1) As IEnumerable(Of Integer)
        Return source.Concat(Range(start, [stop], [step]))
    End Function
```


which results in a loop that looks like this:

```vbnet
        For Each j In Range(-three,       CInt(3 ^ 3),        3     ).
                ConcatRange(-seven,       +seven,             x     ).
                ConcatRange(555,          550 - y                   ).
                ConcatRange(22,           -28,                -three).
                ConcatRange(1927,         1939                      ).
                ConcatRange(x, y,         z                         ).
                ConcatRange(CInt(11 ^ x), CInt(11 ^ x) + one        )
        Next
```


An alternative to avoid the repeated method calls would be to make a Range function that accepts multiple ranges, in this case as a parameter array of tuples.

```vbnet
    Function Range(ParamArray ranges() As (start As Integer, [stop] As Integer, [step] As Integer)) As IEnumerable(Of Integer)
        ' Note: SelectMany is equivalent to bind, flatMap, etc.
        Return ranges.SelectMany(Function(r) Range(r.start, r.stop, r.step))
    End Function
```


resulting in:

```vbnet
        For Each j In Range((-three,       CInt(3 ^ 3),        3        ),
                            (-seven,       +seven,             x        ),
                            (555,          550 - y,            1        ),
                            (22,           -28,                -three   ),
                            (1927,         1939,               1        ),
                            (x,            y,                  z        ),
                            (CInt(11 ^ x), CInt(11 ^ x) + one, 1        ))
        Next
```


Note, however, that the inability to have a heterogenous array means that specifying the step is now mandatory. Using a parameter array of arrays is slightly less clear but results in the tersest loop.

```vbnet
    Function Range(ParamArray ranges As Integer()()) As IEnumerable(Of Integer)
        Return ranges.SelectMany(Function(r) Range(r(0), r(1), If(r.Length < 3, 1, r(2))))
    End Function
```



```vbnet
        For Each j In Range({-three,       CInt(3 ^ 3),        3        },
                            {-seven,       +seven,             x        },
                            {555,          550 - y                      },
                            {22,           -28,                -three   },
                            {1927,         1939                         },
                            {x,            y,                  z        },
                            {CInt(11 ^ x), CInt(11 ^ x) + one           })
        Next
```


```txt
 sum= 348,173
prod= -793,618,560
```



## zkl


```zkl
prod,sum := 1,0;  /* start with a product of unity, sum of 0 */
x,y,z := 5, -5, -2;
one,three,seven := 1,3,7;
foreach j in (Walker.chain([-three..(3).pow(3),three], // do these sequentially
               [-seven..seven,x], [555..550 - y], [22..-28,-three], #[start..last,step]
               [1927..1939], [x..y,z], [(11).pow(x)..(11).pow(x) + one])){
   sum+=j.abs();	/* add absolute value of J */
   if(prod.abs()<(2).pow(27) and j!=0) prod*=j; /* PROD is small enough & J */
}
/* SUM and PROD are used for verification of J incrementation */
println("sum  = %,d\nprod = %,d".fmt(sum,prod));
```

```txt

sum  = 348,173
prod = -793,618,560

```

