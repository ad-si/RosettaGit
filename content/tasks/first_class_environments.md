+++
title = "First class environments"
description = ""
date = 2019-01-13T22:55:40Z
aliases = []
[extra]
id = 9996
[taxonomies]
categories = ["task", "Classic CS problems and programs"]
tags = []
+++

## Task

According to [[wp:First-class_object|Wikipedia]], "In computing, a first-class object ... is an entity that can be constructed at run-time, passed as a parameter, returned from a subroutine, or assigned into a variable".

Often this term is used in the context of "first class functions". In an analogous way, a programming language may support "first class environments".

The environment is minimally, the set of variables accessable to a statement being executed. Change the environments and the same statement could produce different results when executed.

Often an environment is captured in a [[wp:Closure_(computer_science)|closure]], which encapsulates a function together with an environment. That environment, however, is '''not''' first-class, as it cannot be created, passed etc. independently from the function's code.

Therefore, a first class environment is a set of variable bindings which can be constructed at run-time, passed as a parameter, returned from a subroutine, or assigned into a variable. It is like a closure without code. A statement must be able to be executed within a stored first class environment and act according to the environment variable values stored within.

The task: Build a dozen environments, and a single piece of code to be run repeatedly in each of these envionments.

Each environment contains the bindings for two variables: A value in the [[Hailstone sequence]], and a count which is incremented until the value drops to 1. The initial hailstone values are 1 through 12, and the count in each environment is zero.

When the code runs, it calculates the next hailstone step in the current environment (unless the value is already 1) and counts the steps. Then it prints the current value in a tabular form.

When all hailstone values dropped to 1, processing stops, and the total number of hailstone steps for each environment is printed.

## BBC BASIC

Here the 'environment' consists of all the dynamic variables; the static integer variables (A%-Z%) are not affected.

```bbcbasic
      DIM @environ$(12)
      @% = 4 : REM Column width

      REM Initialise:
      FOR E% = 1 TO 12
        PROCsetenvironment(@environ$(E%))
        seq% = E%
        cnt% = 0
        @environ$(E%) = FNgetenvironment
      NEXT

      REM Run hailstone sequences:
      REPEAT
        T% = 0
        FOR E% = 1 TO 12
          PROCsetenvironment(@environ$(E%))
          PRINT seq% ;
          IF seq% <> 1 THEN
            T% += 1
            cnt% += 1
            IF seq% AND 1 seq% = 3 * seq% + 1 ELSE seq% DIV= 2
          ENDIF
          @environ$(E%) = FNgetenvironment
        NEXT
        PRINT
      UNTIL T% = 0

      REM Print counts:
      PRINT "Counts:"
      FOR E% = 1 TO 12
        PROCsetenvironment(@environ$(E%))
        PRINT cnt% ;
        @environ$(E%) = FNgetenvironment
      NEXT
      PRINT
      END

      DEF FNgetenvironment
      LOCAL e$ : e$ = STRING$(216, CHR$0)
      SYS "RtlMoveMemory", !^e$, ^@%+108, 216
      = e$

      DEF PROCsetenvironment(e$)
      IF LEN(e$) < 216 e$ = STRING$(216, CHR$0)
      SYS "RtlMoveMemory", ^@%+108, !^e$, 216
      ENDPROC
```

'''Output:'''

```txt

   1   2   3   4   5   6   7   8   9  10  11  12
   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1
   1   1   1   1   1   1   1   1   1   1   1   1
Counts:
   0   1   7   2   5   8  16   3  19   6  14   9

```



## Bracmat


```bracmat
(  (environment=(cnt=0) (seq=))
& :?environments
& 13:?seq
&   whl
  ' ( !seq+-1:>0:?seq
    & new$environment:?env
    & !seq:?(env..seq)
    & !env !environments:?environments
    )
& out$(Before !environments)
&   whl
  ' ( !environments:? (=? (seq=>1) ?) ?
    & !environments:?envs
    &   whl
      ' ( !envs:(=?env) ?envs
        &   (
              ' ( $env
                  (
                  =
                    .     put$(!(its.seq) \t)
                        & !(its.seq):1
                      |   1+!(its.cnt):?(its.cnt)
                        & 1/2*!(its.seq):~/?(its.seq)
                      | 3*!(its.seq)+1:?(its.seq)
                  )
                )
            .
            )
          $
        )
    & out$
    )
& out$(After !environments)
)
```

Output:

```txt
  Before
  (=(cnt=0) (seq=1))
  (=(cnt=0) (seq=2))
  (=(cnt=0) (seq=3))
  (=(cnt=0) (seq=4))
  (=(cnt=0) (seq=5))
  (=(cnt=0) (seq=6))
  (=(cnt=0) (seq=7))
  (=(cnt=0) (seq=8))
  (=(cnt=0) (seq=9))
  (=(cnt=0) (seq=10))
  (=(cnt=0) (seq=11))
  (=(cnt=0) (seq=12))
1       2       3       4       5       6       7       8       9       10      11      12
1       1       10      2       16      3       22      4       28      5       34      6
1       1       5       1       8       10      11      2       14      16      17      3
1       1       16      1       4       5       34      1       7       8       52      10
1       1       8       1       2       16      17      1       22      4       26      5
1       1       4       1       1       8       52      1       11      2       13      16
1       1       2       1       1       4       26      1       34      1       40      8
1       1       1       1       1       2       13      1       17      1       20      4
1       1       1       1       1       1       40      1       52      1       10      2
1       1       1       1       1       1       20      1       26      1       5       1
1       1       1       1       1       1       10      1       13      1       16      1
1       1       1       1       1       1       5       1       40      1       8       1
1       1       1       1       1       1       16      1       20      1       4       1
1       1       1       1       1       1       8       1       10      1       2       1
1       1       1       1       1       1       4       1       5       1       1       1
1       1       1       1       1       1       2       1       16      1       1       1
1       1       1       1       1       1       1       1       8       1       1       1
1       1       1       1       1       1       1       1       4       1       1       1
1       1       1       1       1       1       1       1       2       1       1       1
  After
  (=(cnt=0) (seq=1))
  (=(cnt=1) (seq=1))
  (=(cnt=7) (seq=1))
  (=(cnt=2) (seq=1))
  (=(cnt=5) (seq=1))
  (=(cnt=8) (seq=1))
  (=(cnt=16) (seq=1))
  (=(cnt=3) (seq=1))
  (=(cnt=19) (seq=1))
  (=(cnt=6) (seq=1))
  (=(cnt=14) (seq=1))
  (=(cnt=9) (seq=1))
```



## C

Well, this fits the semantics, not sure about the spirit…

```c
#include <stdio.h>

#define JOBS 12
#define jobs(a) for (switch_to(a = 0); a < JOBS || !printf("\n"); switch_to(++a))
typedef struct { int seq, cnt; } env_t;

env_t env[JOBS] = {{0, 0}};
int *seq, *cnt;

void hail()
{
	printf("% 4d", *seq);
	if (*seq == 1) return;
	++*cnt;
	*seq = (*seq & 1) ? 3 * *seq + 1 : *seq / 2;
}

void switch_to(int id)
{
	seq = &env[id].seq;
	cnt = &env[id].cnt;
}

int main()
{
	int i;
	jobs(i) { env[i].seq = i + 1; }

again:	jobs(i) { hail(); }
	jobs(i) { if (1 != *seq) goto again; }

	printf("COUNTS:\n");
	jobs(i) { printf("% 4d", *cnt); }

	return 0;
}
```

```txt

   1   2   3   4   5   6   7   8   9  10  11  12
   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1

COUNTS:
   0   1   7   2   5   8  16   3  19   6  14   9

```



## D

D doesn't have first class environments, this is an approximation.
```d
import std.stdio, std.algorithm, std.range, std.array;

struct Prop {
    int[string] data;
    ref opDispatch(string s)() pure nothrow {
        return data[s];
    }
}

immutable code = `
writef("% 4d", e.seq);
if (e.seq != 1) {
    e.cnt++;
    e.seq = (e.seq & 1) ? 3 * e.seq + 1 : e.seq / 2;
}`;

void main() {
    auto envs = 12.iota.map!(i => Prop(["cnt": 0, "seq": i+1])).array;

    while (envs.any!(env => env.seq > 1)) {
        foreach (e; envs) {
            mixin(code);
        }
        writeln;
    }

    writefln("Counts:\n%(% 4d%)", envs.map!(env => env.cnt));
}
```

```txt
   1   2   3   4   5   6   7   8   9  10  11  12
   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1
Counts:
   0   1   7   2   5   8  16   3  19   6  14   9
```



## EchoLisp

'''(environment-new ((name value) ..) ''' is used to create a new envrionment. '''(eval form env)''' is used to evaluate a form in a specified environment.

```scheme

(define (bump-value)
	(when (> value 1)
	(set! count (1+ count))
	(set! value (if (even? value) (/ value 2) (1+ (* 3 value))))))

(define (env-show name envs )
(write name)
(for ((env envs)) (write (format "%4a" (eval name env))))
(writeln))

(define (task (envnum 12))
(define envs (for/list ((i envnum)) (environment-new `((value ,(1+ i)) (count 0)))))
(env-show 'value envs)
(while
	(any  (curry  (lambda ( n env) (!= 1 (eval n env))) 'value) envs)
	(for/list ((env envs)) (eval '(bump-value) env))
	(env-show 'value envs))
(env-show 'count envs))

```

```txt

(task)
value    1    2    3    4    5    6    7    8    9   10   11   12
value    1    1   10    2   16    3   22    4   28    5   34    6
value    1    1    5    1    8   10   11    2   14   16   17    3
value    1    1   16    1    4    5   34    1    7    8   52   10
value    1    1    8    1    2   16   17    1   22    4   26    5
value    1    1    4    1    1    8   52    1   11    2   13   16
value    1    1    2    1    1    4   26    1   34    1   40    8
value    1    1    1    1    1    2   13    1   17    1   20    4
value    1    1    1    1    1    1   40    1   52    1   10    2
value    1    1    1    1    1    1   20    1   26    1    5    1
value    1    1    1    1    1    1   10    1   13    1   16    1
value    1    1    1    1    1    1    5    1   40    1    8    1
value    1    1    1    1    1    1   16    1   20    1    4    1
value    1    1    1    1    1    1    8    1   10    1    2    1
value    1    1    1    1    1    1    4    1    5    1    1    1
value    1    1    1    1    1    1    2    1   16    1    1    1
value    1    1    1    1    1    1    1    1    8    1    1    1
value    1    1    1    1    1    1    1    1    4    1    1    1
value    1    1    1    1    1    1    1    1    2    1    1    1
value    1    1    1    1    1    1    1    1    1    1    1    1
count    0    1    7    2    5    8   16    3   19    6   14    9

```



## Erlang

The Erlang modifiable environment, aka process dictionary, has the following warning in the documentation:

```txt

Note that using the Process Dictionary:
Destroys referencial transparency
Makes debugging difficult
Survives Catch/Throw

```

There is a lot of code below to manage the tabular printout. Otherwise the task is simple.

```Erlang

-module( first_class_environments ).

-export( [task/0] ).

task() ->
	Print_pid = erlang:spawn( fun() -> print_loop() end ),
	Environments = lists:seq( 1, 12 ),
	Print_pid ! "Environment:   Sequence",
	Pids = [erlang:spawn(fun() -> hailstone_in_environment(Print_pid, X) end) || X <- Environments],
	Counts = counts( Pids ),
	Print_pid ! "{Environment, Step count}",
	Print_pid ! lists:flatten( io_lib:format("~p", [Counts]) ),
	ok.



counts( Pids ) ->
	My_pid = erlang:self(),
	[X ! {count, My_pid} || X <- Pids],
	counts( Pids, [] ).

counts( [], Acc ) -> Acc;
counts( Pids, Acc ) ->
	receive
	{count, N, Count, Pid} -> counts( lists:delete(Pid, Pids), [{N, Count} | Acc] )
	end.

hailstone_in_environment( Print_pid, N ) ->
	erlang:put( hailstone_value, N ),
	erlang:put( count, 0 ),
	hailstone_loop( hailstone_loop_done(N), Print_pid, N, [N] ).

hailstone_loop( stop, Print_pid, N, Acc ) ->
	Environment = lists:flatten( io_lib:format("~11B:", [N]) ),
	Sequence = lists:flatten( [io_lib:format("~4B", [X]) || X <- lists:reverse(Acc)] ),
	Print_pid ! Environment ++ Sequence,
	Count= erlang:get( count ),
	receive
	{count, Pid} -> Pid ! {count, N, Count, erlang:self()}
	end;
hailstone_loop( keep_going, Print_pid, N, Acc ) ->
	Next = hailstone_next( erlang:get(hailstone_value) ),
	erlang:put( hailstone_value, Next ),
	Count = erlang:get( count ),
	erlang:put( count, Count + 1 ),
	hailstone_loop( hailstone_loop_done(Next), Print_pid, N, [Next | Acc]  ).

hailstone_loop_done( 1 ) -> stop;
hailstone_loop_done( _N ) -> keep_going.

hailstone_next( 1 ) -> 1;
hailstone_next( Even ) when (Even rem 2) =:= 0 -> Even div 2;
hailstone_next( Odd ) -> (3 * Odd) + 1.

print_loop() ->
	receive
	String -> io:fwrite("~s~n", [String] )
	end,
	print_loop().

```

```txt

13>  first_class_environments:task().
Environment:  Sequence
          1:   1
          2:   2   1
          3:   3  10   5  16   8   4   2   1
          4:   4   2   1
          5:   5  16   8   4   2   1
          6:   6   3  10   5  16   8   4   2   1
          7:   7  22  11  34  17  52  26  13  40  20  10   5  16   8   4   2   1
          8:   8   4   2   1
          9:   9  28  14   7  22  11  34  17  52  26  13  40  20  10   5  16   8   4   2   1
         10:  10   5  16   8   4   2   1
         11:  11  34  17  52  26  13  40  20  10   5  16   8   4   2   1
         12:  12   6   3  10   5  16   8   4   2   1
{Environment, Step count}
[{12,9}, {11,14}, {10,6}, {9,19}, {8,3}, {7,16}, {6,8}, {5,5}, {4,2}, {3,7}, {2,1}, {1,0}]

```



## Go

```go
package main

import "fmt"

const jobs = 12

type environment struct{ seq, cnt int }

var (
    env      [jobs]environment
    seq, cnt *int
)

func hail() {
    fmt.Printf("% 4d", *seq)
    if *seq == 1 {
        return
    }
    (*cnt)++
    if *seq&1 != 0 {
        *seq = 3*(*seq) + 1
    } else {
        *seq /= 2
    }
}

func switchTo(id int) {
    seq = &env[id].seq
    cnt = &env[id].cnt
}

func main() {
    for i := 0; i < jobs; i++ {
        switchTo(i)
        env[i].seq = i + 1
    }

again:
    for i := 0; i < jobs; i++ {
        switchTo(i)
        hail()
    }
    fmt.Println()

    for j := 0; j < jobs; j++ {
        switchTo(j)
        if *seq != 1 {
            goto again
        }
    }
    fmt.Println()

    fmt.Println("COUNTS:")
    for i := 0; i < jobs; i++ {
        switchTo(i)
        fmt.Printf("% 4d", *cnt)
    }
    fmt.Println()
}
```


```txt

   1   2   3   4   5   6   7   8   9  10  11  12
   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1

COUNTS:
   0   1   7   2   5   8  16   3  19   6  14   9

```


=={{header|Icon}} and {{header|Unicon}}==
The simplest way to create an environment with variables isolated from code in Icon/Unicon is to create instances of records or class objects.

```Icon
link printf

procedure main()
   every put(environment := [], hailenv(1 to 12,0))  # setup environments
   printf("Sequences:\n")
   while (e := !environment).sequence > 1 do {
      every hailstep(!environment)
      printf("\n")
      }
   printf("\nCounts:\n")
   every printf("%4d ",(!environment).count)
   printf("\n")
end

record hailenv(sequence,count)

procedure hailstep(env)
   printf("%4d ",env.sequence)
    if env.sequence ~= 1 then {
        env.count +:= 1
        if env.sequence % 2 = 0 then env.sequence /:= 2
        else env.sequence := 3 * env.sequence + 1
        }
end
```

[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]
```txt
Sequences:
   1    2    3    4    5    6    7    8    9   10   11   12
   1    1   10    2   16    3   22    4   28    5   34    6
   1    1    5    1    8   10   11    2   14   16   17    3
   1    1   16    1    4    5   34    1    7    8   52   10
   1    1    8    1    2   16   17    1   22    4   26    5
   1    1    4    1    1    8   52    1   11    2   13   16
   1    1    2    1    1    4   26    1   34    1   40    8
   1    1    1    1    1    2   13    1   17    1   20    4
   1    1    1    1    1    1   40    1   52    1   10    2
   1    1    1    1    1    1   20    1   26    1    5    1
   1    1    1    1    1    1   10    1   13    1   16    1
   1    1    1    1    1    1    5    1   40    1    8    1
   1    1    1    1    1    1   16    1   20    1    4    1
   1    1    1    1    1    1    8    1   10    1    2    1
   1    1    1    1    1    1    4    1    5    1    1    1
   1    1    1    1    1    1    2    1   16    1    1    1
   1    1    1    1    1    1    1    1    8    1    1    1
   1    1    1    1    1    1    1    1    4    1    1    1
   1    1    1    1    1    1    1    1    2    1    1    1

Counts:
   0    1    7    2    5    8   16    3   19    6   14    9
```



## Haskell


First let's implement the algorithm of calculating Hailstone series:

```haskell
hailstone n
  | n == 1 = 1
  | even n = n `div` 2
  | odd n  = 3*n + 1
```


and a data structure representing the environment


```haskell
data Environment = Environment { count :: Int, value :: Int }
                 deriving Eq
```


In Haskell operations with first class environments could be implemented using several approaches:

1. Using any data structure <code>S</code> which is passed by a chain of functions, having type <code>S -> S</code>.

2. Using the <code>Reader</code> monad, which emulates access to imutable environment.

3. Using the <code>State</code> monad, which emulates access to an environment, that coud be changed.

For given task approaches 1 and 3 are suitable.

Let's define a collection of environments:


```haskell
environments = [ Environment 0 n | n <- [1..12] ]
```


and a <code>process</code>, which changes an environment according to a task.

Approach 1.


```haskell
process (Environment c 1) = Environment c 1
process (Environment c n) = Environment (c+1) (hailstone n)
```


Approach 3. (needs <code>import Control.Monad.State</code>)


```haskell
process = execState $ do
  n <- gets value
  c <- gets count
  when (n > 1) $ modify $ \env -> env { count = c + 1 }
  modify $ \env -> env { value = hailstone n }
```


Repetitive batch processing of a collection we implement as following:


```haskell
fixedPoint f x
  | fx == x = [x]
  | otherwise = x : fixedPoint f fx
  where fx = f x

prettyPrint field = putStrLn . foldMap (format.field)
  where format n = (if n < 10 then " " else "") ++ show n ++ " "

main = do
  let result = fixedPoint (map process) environments
  mapM_ (prettyPrint value) result
  putStrLn (replicate 36 '-')
  prettyPrint count (last result)
```


```txt
1  2  3  4  5  6  7  8  9 10 11 12
1  1 10  2 16  3 22  4 28  5 34  6
1  1  5  1  8 10 11  2 14 16 17  3
1  1 16  1  4  5 34  1  7  8 52 10
1  1  8  1  2 16 17  1 22  4 26  5
1  1  4  1  1  8 52  1 11  2 13 16
1  1  2  1  1  4 26  1 34  1 40  8
1  1  1  1  1  2 13  1 17  1 20  4
1  1  1  1  1  1 40  1 52  1 10  2
1  1  1  1  1  1 20  1 26  1  5  1
1  1  1  1  1  1 10  1 13  1 16  1
1  1  1  1  1  1  5  1 40  1  8  1
1  1  1  1  1  1 16  1 20  1  4  1
1  1  1  1  1  1  8  1 10  1  2  1
1  1  1  1  1  1  4  1  5  1  1  1
1  1  1  1  1  1  2  1 16  1  1  1
1  1  1  1  1  1  1  1  8  1  1  1
1  1  1  1  1  1  1  1  4  1  1  1
1  1  1  1  1  1  1  1  2  1  1  1
1  1  1  1  1  1  1  1  1  1  1  1
-----------------------------------
0  1  7  2  5  8 16  3 19  6 14  9
```


Or in "transposed" way


```haskell
main = do
  let result = map (fixedPoint process) environments
  mapM_ (prettyPrint value) result
  putStrLn (replicate 36 '-')
  putStrLn "Counts: "
  prettyPrint (count . last) result
```


```txt
 1
 2  1
 3 10  5 16  8  4  2  1
 4  2  1
 5 16  8  4  2  1
 6  3 10  5 16  8  4  2  1
 7 22 11 34 17 52 26 13 40 20 10  5 16  8  4  2  1
 8  4  2  1
 9 28 14  7 22 11 34 17 52 26 13 40 20 10  5 16  8  4  2  1
10  5 16  8  4  2  1
11 34 17 52 26 13 40 20 10  5 16  8  4  2  1
12  6  3 10  5 16  8  4  2  1
------------------------------------
Counts:
 0  1  7  2  5  8 16  3 19  6 14  9
```




## J

I have tried to determine what makes this task interesting (see talk page), but I am still confused.

Here is my current interpretation of the task requirements:

```j
coclass 'hailstone'

step=:3 :0
  NB. and determine next element in hailstone sequence
  if.1=N do. N return.end.
    NB. count how many times this has run when N was not 1
    STEP=:STEP+1
  if.0=2|N do.
    N=: N%2
  else.
    N=: 1 + 3*N
  end.
)

create=:3 :0
  STEP=: 0
  N=: y
)

current=:3 :0
  N__y
)

run1=:3 :0
  step__y''
  STEP__y
)

run=:3 :0
  old=: ''
  while. -. old -: state=: run1"0 y do.
    smoutput 4j0 ": current"0 y
    old=: state
  end.
)
```

```j
   environments=: conew&'hailstone'"0 (1+i.12)
   run_hailstone_ environments
   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1
   1   1   1   1   1   1   1   1   1   1   1   1
0 1 7 2 5 8 16 3 19 6 14 9
```

In essence: run is a static method of the class <code>hailstone</code> which, given a list of objects of the class runs all of them until their hailstone sequence number stops changing.  It also displays the hailstone sequence number from each of the objects at each step.  Its result is the step count from each object.


## jq


In the context of jq, a JSON object is an environment in the sense of this article,
because an object, E, serves as an execution environment for a program of the form E | P
where P is a jq program.

For the task at hand, the environment can be taken to be an object of the form:

     { "value": <HAILSTONE>, "count": <COUNT> }

The required jq "code" is simply:

    if .value > 1 then (.value |= hail) | .count += 1 else . end

where the filter "hail", when given an integer, computes the next hailstone value.

Let us therefore define a function named "code" accordingly:

```jq
def code:
     # Given an integer as input, compute the corresponding hailstone value:
     def hail: if . % 2 == 0 then ./2|floor else 3*. + 1 end;

     if .value > 1 then (.value |= hail) | .count += 1 else . end;

```


To generate the n-th environment, it is useful to define a function:

    def environment: . as $n | { value: $n, count:0 };


Now the program for creating the 12 environments and applying "code" to them until quiesence can be written as follows:

    def generate:
      [range(1;13) | environment]   # create 12 environments
      | recurse( if (any(.[] | .value; . != 1)) then map(code) else empty end);


Finally, to present the results in tabular form, the following helper function will be useful:

   # Apply a filter to a stream,
   # and ALSO emit the last item in the stream filtered through "final"
   def filter_and_last(s; filter; final):
     [s] as $array
     | ($array[] | filter), ($array[-1] | final);


Putting it all together:

```jq
filter_and_last( generate;
                 map(.value) | @tsv;
                 "", "Counts:", (map(.count) | @tsv ))
```



The following invocation produces the result shown below, assuming the above code is in a file named "program.jq":

    $ jq -nr -f program.jq


```txt

1	2	3	4	5	6	7	8	9	10	11	12
1	1	10	2	16	3	22	4	28	5	34	6
1	1	5	1	8	10	11	2	14	16	17	3
1	1	16	1	4	5	34	1	7	8	52	10
1	1	8	1	2	16	17	1	22	4	26	5
1	1	4	1	1	8	52	1	11	2	13	16
1	1	2	1	1	4	26	1	34	1	40	8
1	1	1	1	1	2	13	1	17	1	20	4
1	1	1	1	1	1	40	1	52	1	10	2
1	1	1	1	1	1	20	1	26	1	5	1
1	1	1	1	1	1	10	1	13	1	16	1
1	1	1	1	1	1	5	1	40	1	8	1
1	1	1	1	1	1	16	1	20	1	4	1
1	1	1	1	1	1	8	1	10	1	2	1
1	1	1	1	1	1	4	1	5	1	1	1
1	1	1	1	1	1	2	1	16	1	1	1
1	1	1	1	1	1	1	1	8	1	1	1
1	1	1	1	1	1	1	1	4	1	1	1
1	1	1	1	1	1	1	1	2	1	1	1
1	1	1	1	1	1	1	1	1	1	1	1

Counts:
0	1	7	2	5	8	16	3	19	6	14	9

```



## Julia

```julia
const jobs = 12

mutable struct Environment
    seq::Int
    cnt::Int
    Environment() = new(0, 0)
end

const env = [Environment() for i in 1:jobs]
const currentjob = [1]

seq() = env[currentjob[1]].seq
cnt() = env[currentjob[1]].cnt
seq(n) = (env[currentjob[1]].seq = n)
cnt(n) = (env[currentjob[1]].cnt = n)

function hail()
    print(lpad(seq(), 4))
    if seq() == 1
        return
    end
    cnt(cnt() + 1)
    seq(isodd(seq()) ? 3 * seq() + 1 : div(seq(), 2))
end

function runtest()
    for i in 1:jobs
        currentjob[1] = i
        env[i].seq = i
    end
    computing = true
    while computing
        for i in 1:jobs
            currentjob[1] = i
            hail()
        end
        println()
        for j in 1:jobs
            currentjob[1] = j
            if seq() != 1
                break
            elseif j == jobs
                computing = false
            end
        end
    end
    println("\nCOUNTS:")
    for i in 1:jobs
        currentjob[1] = i
        print(lpad(cnt(), 4))
    end
    println()
end

runtest()

```
```txt

   1   2   3   4   5   6   7   8   9  10  11  12
   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1

 COUNTS:
   0   1   7   2   5   8  16   3  19   6  14   9

```




## Kotlin

This is based on the C entry except that, instead of using object references (Kotlin/JVM doesn't support explicit pointers) to switch between environments, it saves and restores the actual values of the two variables on each job switch. I see no reason why objects shouldn't be used to represent the environments as long as they have no member functions.

```scala
// version 1.1.3

class Environment(var seq: Int, var count: Int)

const val JOBS = 12
val envs = List(JOBS) { Environment(it + 1, 0) }
var seq = 0     // 'seq' for current environment
var count = 0   // 'count' for current environment
var currId = 0  // index of current environment

fun switchTo(id: Int) {
    if (id != currId) {
        envs[currId].seq = seq
        envs[currId].count = count
        currId = id
    }
    seq = envs[id].seq
    count = envs[id].count
}

fun hailstone() {
    print("%4d".format(seq))
    if (seq == 1) return
    count++
    seq = if (seq % 2 == 1) 3 * seq + 1 else seq / 2
}

val allDone get(): Boolean {
    for (a in 0 until JOBS) {
        switchTo(a)
        if (seq != 1) return false
    }
    return true
}

fun code() {
    do {
        for (a in 0 until JOBS) {
            switchTo(a)
            hailstone()
        }
        println()
    }
    while (!allDone)

    println("\nCOUNTS:")
    for (a in 0 until JOBS) {
        switchTo(a)
        print("%4d".format(count))
    }
    println()
}

fun main(args: Array<String>) {
    code()
}
```


```txt

   1   2   3   4   5   6   7   8   9  10  11  12
   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1

COUNTS:
   0   1   7   2   5   8  16   3  19   6  14   9

```



## Lua

In Lua, environments capture reads and writes to "global" variables (the environment for locals is static). Functions can have their own environments, or multiple functions can share an environment. Functions inherit the environment of their enclosing function when they are instantiated.

The way in which environments are manipulated depends on the Lua version:
* Lua 5.1 and before: the <code>setfenv</code> function
* Lua 5.2: an upvalue called <code>_ENV</code>


```lua

local envs = { }
for i = 1, 12 do
    -- fallback to the global environment for io and math
    envs[i] = setmetatable({ count = 0, n = i }, { __index = _G })
end

local code = [[
io.write(("% 4d"):format(n))
if n ~= 1 then
    count = count + 1
    n = (n % 2 == 1) and 3 * n + 1 or math.floor(n / 2)
end
]]

while true do
    local finished = 0
    for _, env in ipairs(envs) do
        if env.n == 1 then finished = finished + 1 end
    end

    if finished == #envs then break end

    for _, env in ipairs(envs) do
        -- 5.1; in 5.2, use load(code, nil, nil, env)() instead
        setfenv(loadstring(code), env)()
    end
    io.write "\n"
end

print "counts:"
for _, env in ipairs(envs) do
    io.write(("% 4d"):format(env.count))
end

```


```txt

   1   2   3   4   5   6   7   8   9  10  11  12
   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1
counts:
   0   1   7   2   5   8  16   3  19   6  14   9

```



## Order

Order supports environments as a first-class type, but since all values are immutable, updating a value means using one environment to update the next in a chain (nothing unusual for languages with immutable data structures):

```c
#include <order/interpreter.h>

#define ORDER_PP_DEF_8hail ORDER_PP_FN(                \
8fn(8N, 8cond((8equal(8N, 1), 1)                       \
          (8is_0(8remainder(8N, 2)), 8quotient(8N, 2)) \
          (8else, 8inc(8times(8N, 3))))) )

#define ORDER_PP_DEF_8h_loop ORDER_PP_FN(                           \
8fn(8S,                                                             \
    8let((8F, 8fn(8E, 8env_ref(8(8H), 8E))),                        \
         8do(                                                       \
           8print(8seq_to_tuple(8seq_map(8F, 8S)) 8space),          \
           8let((8S, 8h_once(8S)),                                  \
                8if(8equal(1,                                       \
                           8seq_fold(8times, 1, 8seq_map(8F, 8S))), \
                    8print_counts(8S),                              \
                    8h_loop(8S)))))) )

#define ORDER_PP_DEF_8h_once ORDER_PP_FN(                          \
8fn(8S,                                                            \
    8seq_map(                                                      \
      8fn(8E,                                                      \
          8eval(8E,                                                \
                8quote(                                            \
                  8env_bind(8(8C),                                 \
                            8env_bind(8(8H),                       \
                                      8env_bind(8(8E), 8E, 8E),    \
                                      8hail(8H)),                  \
                            8if(8equal(8H, 1), 8C, 8inc(8C))) ))), \
      8S)) )

#define ORDER_PP_DEF_8print_counts ORDER_PP_FN( \
8fn(8S,                                         \
    8print(8space 8(Counts:)                    \
           8seq_to_tuple(8seq_map(8fn(8E, 8env_ref(8(8C), 8E)), 8S)))) )

ORDER_PP(
  8let((8S,    // Build a list of environments
        8seq_map(8fn(8N, 8seq_of_pairs_to_env(
                           8seq(8pair(8(8H), 8N), 8pair(8(8C), 0),
                                8pair(8(8E), 8env_nil)))),
                 8seq_iota(1, 13))),
       8h_loop(8S))
)
```

<lang>(1,2,3,4,5,6,7,8,9,10,11,12) (1,1,10,2,16,3,22,4,28,5,34,6) (1,1,5,1,8,10,11,2,14,16,17,3) (1,1,16,1,4,5,34,1,7,8,52,10) (1,1,8,1,2,16,17,1,22,4,26,5) (1,1,4,1,1,8,52,1,11,2,13,16) (1,1,2,1,1,4,26,1,34,1,40,8) (1,1,1,1,1,2,13,1,17,1,20,4) (1,1,1,1,1,1,40,1,52,1,10,2) (1,1,1,1,1,1,20,1,26,1,5,1) (1,1,1,1,1,1,10,1,13,1,16,1) (1,1,1,1,1,1,5,1,40,1,8,1) (1,1,1,1,1,1,16,1,20,1,4,1) (1,1,1,1,1,1,8,1,10,1,2,1) (1,1,1,1,1,1,4,1,5,1,1,1) (1,1,1,1,1,1,2,1,16,1,1,1) (1,1,1,1,1,1,1,1,8,1,1,1) (1,1,1,1,1,1,1,1,4,1,1,1) (1,1,1,1,1,1,1,1,2,1,1,1) Counts:(0,1,7,2,5,8,16,3,19,6,14,9)
```

The C preprocessor cannot output newlines, so the output is all on one line, but easily parsable.


## Perl

The Safe module (which is part of perl's standard distribution) is everything that one might want in a First Class Environment, and a bit more.  The module's primary purpose is to provide a safe execution environment for untrustworthy code, and by doing so, it lends itself very well to this task's goal.

My use of the module is relatively straightforward.

Create a Safe object.  Within that Safe, set up $value and $count variables, with appropriate initial values.  Tell the Safe what "external" functions (from outside of the Safe object) it may run.  Add a function, inside of the Safe, which will be run in the Safe, using the Safe's $value and $count variables.  Add the Safe to an array.  Repeat eleven more times.

Notice that to the function in the safe, $value and $count look like (and are!) perfectly ordinary variables.

Next, repeatedly perform the task, until the required conditions are met, and print the counts.


```perl

use strict;
use warnings;

use Safe;

sub hail_next {
    my $n = shift;
    return 1 if $n == 1;
    return $n * 3 + 1 if $n % 2;
    $n / 2;
};

my @enviornments;
for my $initial ( 1..12 ) {
   my $env = Safe->new;
   ${ $env->varglob('value') } = $initial;
   ${ $env->varglob('count') } = 0;
   $env->share('&hail_next');
   $env->reval(q{
      sub task {
         return if $value == 1;
         $value = hail_next( $value );
         ++$count;
      }
   });
   push @enviornments, $env;
}

my @value_refs = map $_->varglob('value'), @enviornments;
my @tasks = map $_->varglob('task'), @enviornments;
while( grep { $$_ != 1 } @value_refs ) {
    printf "%4s", $$_ for @value_refs;
    print "\n";
    $_->() for @tasks;
}

print "Counts\n";

printf "%4s", ${$_->varglob('count')} for @enviornments;
print "\n";

```

```txt

   1   2   3   4   5   6   7   8   9  10  11  12
   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1
Counts
   0   1   7   2   5   8  16   3  19   6  14   9

```



## Perl 6

Fairly straightforward. Set up an array of hashes containing the current values and iteration counts then pass each hash in turn with a code reference to a routine to calculate the next iteration.


```perl6
my $calculator = sub ($n is rw) {
    return ($n == 1) ?? 1 !! $n %% 2 ?? $n div 2 !! $n * 3 + 1
};

sub next (%this, &get_next) {
    return %this if %this.<value> == 1;
    %this.<value>.=&get_next;
    %this.<count>++;
    return %this;
};

my @hailstones = map { %(value => $_, count => 0) }, 1 .. 12;

while not all( map { $_.<value> }, @hailstones ) == 1 {
    say [~] map { $_.<value>.fmt("%4s") }, @hailstones;
    @hailstones[$_].=&next($calculator) for ^@hailstones;
}

say 'Counts';

say [~] map { $_.<count>.fmt("%4s") }, @hailstones;
```


```txt
   1   2   3   4   5   6   7   8   9  10  11  12
   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1
Counts
   0   1   7   2   5   8  16   3  19   6  14   9

```



## Phix

Emulation using edx as an "enviroment index" into static sequences. (You could of course nest the three sequences inside a single "environments" sequence, if you prefer.)

```Phix
function hail(integer n)
    if remainder(n,2)=0 then
        n /= 2
    else
        n = 3*n+1
    end if
    return n
end function

sequence hails = tagset(12),
         counts = repeat(0,12),
         results = columnize({hails})

function step(integer edx)
    integer n = hails[edx]
    if n=1 then return 0 end if
    n = hail(n)
    hails[edx] = n
    counts[edx] += 1
    results[edx] &= n
    return 1
end function

procedure main()
    bool done = false
    while not done do
        done = true
        for i=1 to 12 do
            if step(i) then
                done = false
            end if
        end for
    end while

    for i=1 to max(counts)+1 do
        for j=1 to 12 do
            puts(1,iff(i<=length(results[j])?sprintf("%4d",{results[j][i]}):"    "))
        end for
        puts(1,"\n")
    end for
    printf(1," %s\n",{join(repeat("===",12))})
    for j=1 to 12 do
        printf(1,"%4d",{counts[j]})
    end for
    puts(1,"\n")
end procedure

main()
```

Emulation using edx as a dictionary_id (creating a separate dictionary for each environment):

```Phix
function hail(integer n)
    if remainder(n,2)=0 then
        n /= 2
    else
        n = 3*n+1
    end if
    return n
end function

function step(integer edx)
    integer n = getd("hail",edx)
    if n=1 then return 0 end if
    n = hail(n)
    setd("hail",n,edx)
    setd("count",getd("count",edx)+1,edx)
    setd("results",getd("results",edx)&n,edx)
    return 1
end function

sequence dicts = {}

procedure main()
    for i=1 to 12 do
        integer d = new_dict()
        setd("hail",i,d)
        setd("count",0,d)
        setd("results",{i},d)
        dicts &= d
    end for

    bool done = false
    while not done do
        done = true
        for i=1 to 12 do
            if step(dicts[i]) then
                done = false
            end if
        end for
    end while

    done = false
    integer i = 1
    while not done do
        done = true
        for j=1 to 12 do
            sequence res = getd("results",dicts[j])
            if i<length(res) then done = false end if
            puts(1,iff(i<=length(res)?sprintf("%4d",{res[i]}):"    "))
        end for
        puts(1,"\n")
        i += 1
    end while
    printf(1," %s\n",{join(repeat("===",12))})
    for j=1 to 12 do
        integer count = getd("count",dicts[j])
        printf(1,"%4d",{count})
    end for
    puts(1,"\n")
end procedure

main()
```

(same for both)

```txt

   1   2   3   4   5   6   7   8   9  10  11  12
       1  10   2  16   3  22   4  28   5  34   6
           5   1   8  10  11   2  14  16  17   3
          16       4   5  34   1   7   8  52  10
           8       2  16  17      22   4  26   5
           4       1   8  52      11   2  13  16
           2           4  26      34   1  40   8
           1           2  13      17      20   4
                       1  40      52      10   2
                          20      26       5   1
                          10      13      16
                           5      40       8
                          16      20       4
                           8      10       2
                           4       5       1
                           2      16
                           1       8
                                   4
                                   2
                                   1

###  === === === === === === === === === ===

   0   1   7   2   5   8  16   3  19   6  14   9

```



## PicoLisp

Runtime environments can be controlled with the '[http://software-lab.de/doc/refJ.html#job job]' function:

```PicoLisp
(let Envs
   (mapcar
      '((N) (list (cons 'N N) (cons 'Cnt 0)))  # Build environments
      (range 1 12) )
   (while (find '((E) (job E (> N 1))) Envs)   # Until all values are 1:
      (for E Envs
         (job E                                # Use environment 'E'
            (prin (align 4 N))
            (unless (= 1 N)
               (inc 'Cnt)                      # Increment step count
               (setq N
                  (if (bit? 1 N)               # Calculate next hailstone value
                     (inc (* N 3))
                     (/ N 2) ) ) ) ) )
      (prinl) )
   (prinl (need 48 '=))
   (for E Envs                                 # For each environment 'E'
      (job E
         (prin (align 4 Cnt)) ) )              # print the step count
   (prinl) )
```

```txt
   1   2   3   4   5   6   7   8   9  10  11  12
   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1

### ==========================================

   0   1   7   2   5   8  16   3  19   6  14   9
```



## Python

In Python, name bindings are held in dicts, one for global scope and another for local scope. When [http://docs.python.org/release/3.1.3/library/functions.html#exec exec]'ing code, you are allowed to give your own dictionaries for these scopes. In this example, two names are held in dictionaries that are used as the local scope for the evaluation of source.

```python
environments = [{'cnt':0, 'seq':i+1} for i in range(12)]

code = '''
print('% 4d' % seq, end='')
if seq != 1:
    cnt += 1
    seq = 3 * seq + 1 if seq & 1 else seq // 2
'''

while any(env['seq'] > 1 for env in environments):
    for env in environments:
        exec(code, globals(), env)
    print()

print('Counts')
for env in environments:
    print('% 4d' % env['cnt'], end='')
print()
```

```txt
   1   2   3   4   5   6   7   8   9  10  11  12
   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1
Counts
   0   1   7   2   5   8  16   3  19   6  14   9

```



## R



```R
code <- quote(
          if (n == 1) n else {
            count <- count + 1;
            n <- if (n %% 2 == 1) 3 * n + 1 else n/2
          })

eprint <- function(envs, var="n")
  cat(paste(sprintf("%4d", sapply(envs, `[[`, var)), collapse=" "), "\n")

envs <- mapply(function(...) list2env(list(...)), n=1:12, count=0)

while (any(sapply(envs, eval, expr=code) > 1)) {eprint(envs)}
eprint(envs)

cat("\nCounts:\n")
eprint(envs, "count")
```


```txt
   1    2    3    4    5    6    7    8    9   10   11   12
   1    1   10    2   16    3   22    4   28    5   34    6
   1    1    5    1    8   10   11    2   14   16   17    3
   1    1   16    1    4    5   34    1    7    8   52   10
   1    1    8    1    2   16   17    1   22    4   26    5
   1    1    4    1    1    8   52    1   11    2   13   16
   1    1    2    1    1    4   26    1   34    1   40    8
   1    1    1    1    1    2   13    1   17    1   20    4
   1    1    1    1    1    1   40    1   52    1   10    2
   1    1    1    1    1    1   20    1   26    1    5    1
   1    1    1    1    1    1   10    1   13    1   16    1
   1    1    1    1    1    1    5    1   40    1    8    1
   1    1    1    1    1    1   16    1   20    1    4    1
   1    1    1    1    1    1    8    1   10    1    2    1
   1    1    1    1    1    1    4    1    5    1    1    1
   1    1    1    1    1    1    2    1   16    1    1    1
   1    1    1    1    1    1    1    1    8    1    1    1
   1    1    1    1    1    1    1    1    4    1    1    1
   1    1    1    1    1    1    1    1    2    1    1    1

Counts:
   0    1    7    2    5    8   16    3   19    6   14    9

```



## Racket


```Racket

#lang racket

(define namespaces
  (for/list ([i (in-range 1 13)])
    (define ns (make-base-namespace))
    (eval `(begin (define N ,i) (define count 0)) ns)
    ns))

(define (get-var-values name)
  (map (curry namespace-variable-value name #t #f) namespaces))

(define code
  '(when (> N 1)
     (set! N (if (even? N) (/ N 2) (+ 1 (* N 3))))
     (set! count (add1 count))))

(define (show-nums nums)
  (for ([n nums]) (display (~a n #:width 4 #:align 'right)))
  (newline))

(let loop ()
  (define Ns (get-var-values 'N))
  (show-nums Ns)
  (unless (andmap (λ(n) (= n 1)) Ns)
    (for ([ns namespaces]) (eval code ns))
    (loop)))
(displayln (make-string (* 4 12) #\=))
(show-nums (get-var-values 'count))

```


Output:

```txt

   1   2   3   4   5   6   7   8   9  10  11  12
   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1
   1   1   1   1   1   1   1   1   1   1   1   1

### ==========================================

   0   1   7   2   5   8  16   3  19   6  14   9

```



## REXX

The formatting is sensitive to a terminating Collatz sequence and is shown as blanks   (that is,

once a   '''1'''   (unity)   is found, no more numbers are displayed in that column).

Column widths are automatically adjusted for their width (maximum decimal digits displayed in a column).

The '''hailstone''' function (subroutine) could be coded in-line to further comply with the task's requirement that

the solution have a   ''single piece of code to be run repeatedly in each of these environments''.

```rexx
/*REXX program illustrates 1st─class environments (using the numbers from hailstone seq)*/
parse arg N .                                    /*obtain optional argument from the CL.*/
if N=='' | N==","  then N=12                     /*Was N defined?  No, then use default.*/
w=length(N)                                      /*width  (so far)  for columnar output.*/
@.=
      do i=1  for N;   @.i=i;   end  /*i*/       /*initialize all the environments.     */

      do forever  until @.0;    @.0=1            /* ◄─── process all the environments.  */
          do k=1  for N;        x=hailstone(k)   /*obtain next hailstone number in seq. */
          w=max(w, length(x) )                   /*determine the maximum width needed.  */
          @.k=@.k  x                             /* ◄─── where the rubber meets the road*/
          end   /*k*/
      end       /*forever*/
#=0                                              /* [↓]   display the tabular results.  */
      do lines=-1  until _='';     _=            /*process a line for each environment. */
          do j=1  for N                          /*process each of the environments.    */
              select                             /*determine how to process the line.   */
              when #== 1      then _=_ right(words(@.j) - 1, w)
              when lines==-1  then _=_ right(j,  w)                    /*the header.    */
              when lines== 0  then _=_ right('', w, "─")               /*the separator. */
              otherwise            _=_ right(word(@.j, lines), w)
              end   /*select*/
          end       /*j*/
      if #==1   then #=2
      if _=''   then #=# + 1                                           /*Null?  Bump #. */
      if #==1   then _=copies(" "left('', w, "═"), N)                  /*foot separator.*/
      if _\=''  then say strip( substr(_, 2), "T")                     /*display counts.*/
      end   /*lines*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
hailstone:  procedure expose @.;  parse arg y;      _=word(@.y, words(@.y) )
            if _==1  then return '';    @.0=0;   if _//2  then return _*3+1;    return _%2
```

```txt

 1  2  3  4  5  6  7  8  9 10 11 12
── ── ── ── ── ── ── ── ── ── ── ──
 1  2  3  4  5  6  7  8  9 10 11 12
    1 10  2 16  3 22  4 28  5 34  6
       5  1  8 10 11  2 14 16 17  3
      16     4  5 34  1  7  8 52 10
       8     2 16 17    22  4 26  5
       4     1  8 52    11  2 13 16
       2        4 26    34  1 40  8
       1        2 13    17    20  4
                1 40    52    10  2
                  20    26     5  1
                  10    13    16
                   5    40     8
                  16    20     4
                   8    10     2
                   4     5     1
                   2    16
                   1     8
                         4
                         2
                         1
══ ══ ══ ══ ══ ══ ══ ══ ══ ══ ══ ══
 0  1  7  2  5  8 16  3 19  6 14  9

```

(Shown at three-quarter size.)
<pre style="font-size:75%;height:115ex">
   1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   25   26   27   28   29   30   31   32   33   34   35   36   37   38   39   40   41   42   43   44   45   46   47   48   49   50   51   52   53   54   55   56   57   58   59   60
──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ──── ────
   1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   25   26   27   28   29   30   31   32   33   34   35   36   37   38   39   40   41   42   43   44   45   46   47   48   49   50   51   52   53   54   55   56   57   58   59   60
        1   10    2   16    3   22    4   28    5   34    6   40    7   46    8   52    9   58   10   64   11   70   12   76   13   82   14   88   15   94   16  100   17  106   18  112   19  118   20  124   21  130   22  136   23  142   24  148   25  154   26  160   27  166   28  172   29  178   30
             5    1    8   10   11    2   14   16   17    3   20   22   23    4   26   28   29    5   32   34   35    6   38   40   41    7   44   46   47    8   50   52   53    9   56   58   59   10   62   64   65   11   68   70   71   12   74   76   77   13   80   82   83   14   86   88   89   15
            16         4    5   34    1    7    8   52   10   10   11   70    2   13   14   88   16   16   17  106    3   19   20  124   22   22   23  142    4   25   26  160   28   28   29  178    5   31   32  196   34   34   35  214    6   37   38  232   40   40   41  250    7   43   44  268   46
             8         2   16   17        22    4   26    5    5   34   35    1   40    7   44    8    8   52   53   10   58   10   62   11   11   70   71    2   76   13   80   14   14   88   89   16   94   16   98   17   17  106  107    3  112   19  116   20   20  124  125   22  130   22  134   23
             4         1    8   52        11    2   13   16   16   17  106        20   22   22    4    4   26  160    5   29    5   31   34   34   35  214    1   38   40   40    7    7   44  268    8   47    8   49   52   52   53  322   10   56   58   58   10   10   62  376   11   65   11   67   70
             2              4   26        34    1   40    8    8   52   53        10   11   11    2    2   13   80   16   88   16   94   17   17  106  107        19   20   20   22   22   22  134    4  142    4  148   26   26  160  161    5   28   29   29    5    5   31  188   34  196   34  202   35
             1              2   13        17        20    4    4   26  160         5   34   34    1    1   40   40    8   44    8   47   52   52   53  322        58   10   10   11   11   11   67    2   71    2   74   13   13   80  484   16   14   88   88   16   16   94   94   17   98   17  101  106
                            1   40        52        10    2    2   13   80        16   17   17             20   20    4   22    4  142   26   26  160  161        29    5    5   34   34   34  202    1  214    1   37   40   40   40  242    8    7   44   44    8    8   47   47   52   49   52  304   53
                                20        26         5    1    1   40   40         8   52   52             10   10    2   11    2   71   13   13   80  484        88   16   16   17   17   17  101       107       112   20   20   20  121    4   22   22   22    4    4  142  142   26  148   26  152  160
                                10        13        16             20   20         4   26   26              5    5    1   34    1  214   40   40   40  242        44    8    8   52   52   52  304       322        56   10   10   10  364    2   11   11   11    2    2   71   71   13   74   13   76   80
                                 5        40         8             10   10         2   13   13             16   16        17       107   20   20   20  121        22    4    4   26   26   26  152       161        28    5    5    5  182    1   34   34   34    1    1  214  214   40   37   40   38   40
                                16        20         4              5    5         1   40   40              8    8        52       322   10   10   10  364        11    2    2   13   13   13   76       484        14   16   16   16   91        17   17   17            107  107   20  112   20   19   20
                                 8        10         2             16   16             20   20              4    4        26       161    5    5    5  182        34    1    1   40   40   40   38       242         7    8    8    8  274        52   52   52            322  322   10   56   10   58   10
                                 4         5         1              8    8             10   10              2    2        13       484   16   16   16   91        17             20   20   20   19       121        22    4    4    4  137        26   26   26            161  161    5   28    5   29    5
                                 2        16                        4    4              5    5              1    1        40       242    8    8    8  274        52             10   10   10   58       364        11    2    2    2  412        13   13   13            484  484   16   14   16   88   16
                                 1         8                        2    2             16   16                            20       121    4    4    4  137        26              5    5    5   29       182        34    1    1    1  206        40   40   40            242  242    8    7    8   44    8
                                           4                        1    1              8    8                            10       364    2    2    2  412        13             16   16   16   88        91        17                 103        20   20   20            121  121    4   22    4   22    4
                                           2                                            4    4                             5       182    1    1    1  206        40              8    8    8   44       274        52                 310        10   10   10            364  364    2   11    2   11    2
                                           1                                            2    2                            16        91                 103        20              4    4    4   22       137        26                 155         5    5    5            182  182    1   34    1   34    1
                                                                                        1    1                             8       274                 310        10              2    2    2   11       412        13                 466        16   16   16             91   91        17        17
                                                                                                                           4       137                 155         5              1    1    1   34       206        40                 233         8    8    8            274  274        52        52
                                                                                                                           2       412                 466        16                            17       103        20                 700         4    4    4            137  137        26        26
                                                                                                                           1       206                 233         8                            52       310        10                 350         2    2    2            412  412        13        13
                                                                                                                                   103                 700         4                            26       155         5                 175         1    1    1            206  206        40        40
                                                                                                                                   310                 350         2                            13       466        16                 526                                103  103        20        20
                                                                                                                                   155                 175         1                            40       233         8                 263                                310  310        10        10
                                                                                                                                   466                 526                                      20       700         4                 790                                155  155         5         5
                                                                                                                                   233                 263                                      10       350         2                 395                                466  466        16        16
                                                                                                                                   700                 790                                       5       175         1                1186                                233  233         8         8
                                                                                                                                   350                 395                                      16       526                           593                                700  700         4         4
                                                                                                                                   175                1186                                       8       263                          1780                                350  350         2         2
                                                                                                                                   526                 593                                       4       790                           890                                175  175         1         1
                                                                                                                                   263                1780                                       2       395                           445                                526  526
                                                                                                                                   790                 890                                       1      1186                          1336                                263  263
                                                                                                                                   395                 445                                               593                           668                                790  790
                                                                                                                                  1186                1336                                              1780                           334                                395  395
                                                                                                                                   593                 668                                               890                           167                               1186 1186
                                                                                                                                  1780                 334                                               445                           502                                593  593
                                                                                                                                   890                 167                                              1336                           251                               1780 1780
                                                                                                                                   445                 502                                               668                           754                                890  890
                                                                                                                                  1336                 251                                               334                           377                                445  445
                                                                                                                                   668                 754                                               167                          1132                               1336 1336
                                                                                                                                   334                 377                                               502                           566                                668  668
                                                                                                                                   167                1132                                               251                           283                                334  334
                                                                                                                                   502                 566                                               754                           850                                167  167
                                                                                                                                   251                 283                                               377                           425                                502  502
                                                                                                                                   754                 850                                              1132                          1276                                251  251
                                                                                                                                   377                 425                                               566                           638                                754  754
                                                                                                                                  1132                1276                                               283                           319                                377  377
                                                                                                                                   566                 638                                               850                           958                               1132 1132
                                                                                                                                   283                 319                                               425                           479                                566  566
                                                                                                                                   850                 958                                              1276                          1438                                283  283
                                                                                                                                   425                 479                                               638                           719                                850  850
                                                                                                                                  1276                1438                                               319                          2158                                425  425
                                                                                                                                   638                 719                                               958                          1079                               1276 1276
                                                                                                                                   319                2158                                               479                          3238                                638  638
                                                                                                                                   958                1079                                              1438                          1619                                319  319
                                                                                                                                   479                3238                                               719                          4858                                958  958
                                                                                                                                  1438                1619                                              2158                          2429                                479  479
                                                                                                                                   719                4858                                              1079                          7288                               1438 1438
                                                                                                                                  2158                2429                                              3238                          3644                                719  719
                                                                                                                                  1079                7288                                              1619                          1822                               2158 2158
                                                                                                                                  3238                3644                                              4858                           911                               1079 1079
                                                                                                                                  1619                1822                                              2429                          2734                               3238 3238
                                                                                                                                  4858                 911                                              7288                          1367                               1619 1619
                                                                                                                                  2429                2734                                              3644                          4102                               4858 4858
                                                                                                                                  7288                1367                                              1822                          2051                               2429 2429
                                                                                                                                  3644                4102                                               911                          6154                               7288 7288
                                                                                                                                  1822                2051                                              2734                          3077                               3644 3644
                                                                                                                                   911                6154                                              1367                          9232                               1822 1822
                                                                                                                                  2734                3077                                              4102                          4616                                911  911
                                                                                                                                  1367                9232                                              2051                          2308                               2734 2734
                                                                                                                                  4102                4616                                              6154                          1154                               1367 1367
                                                                                                                                  2051                2308                                              3077                           577                               4102 4102
                                                                                                                                  6154                1154                                              9232                          1732                               2051 2051
                                                                                                                                  3077                 577                                              4616                           866                               6154 6154
                                                                                                                                  9232                1732                                              2308                           433                               3077 3077
                                                                                                                                  4616                 866                                              1154                          1300                               9232 9232
                                                                                                                                  2308                 433                                               577                           650                               4616 4616
                                                                                                                                  1154                1300                                              1732                           325                               2308 2308
                                                                                                                                   577                 650                                               866                           976                               1154 1154
                                                                                                                                  1732                 325                                               433                           488                                577  577
                                                                                                                                   866                 976                                              1300                           244                               1732 1732
                                                                                                                                   433                 488                                               650                           122                                866  866
                                                                                                                                  1300                 244                                               325                            61                                433  433
                                                                                                                                   650                 122                                               976                           184                               1300 1300
                                                                                                                                   325                  61                                               488                            92                                650  650
                                                                                                                                   976                 184                                               244                            46                                325  325
                                                                                                                                   488                  92                                               122                            23                                976  976
                                                                                                                                   244                  46                                                61                            70                                488  488
                                                                                                                                   122                  23                                               184                            35                                244  244
                                                                                                                                    61                  70                                                92                           106                                122  122
                                                                                                                                   184                  35                                                46                            53                                 61   61
                                                                                                                                    92                 106                                                23                           160                                184  184
                                                                                                                                    46                  53                                                70                            80                                 92   92
                                                                                                                                    23                 160                                                35                            40                                 46   46
                                                                                                                                    70                  80                                               106                            20                                 23   23
                                                                                                                                    35                  40                                                53                            10                                 70   70
                                                                                                                                   106                  20                                               160                             5                                 35   35
                                                                                                                                    53                  10                                                80                            16                                106  106
                                                                                                                                   160                   5                                                40                             8                                 53   53
                                                                                                                                    80                  16                                                20                             4                                160  160
                                                                                                                                    40                   8                                                10                             2                                 80   80
                                                                                                                                    20                   4                                                 5                             1                                 40   40
                                                                                                                                    10                   2                                                16                                                               20   20
                                                                                                                                     5                   1                                                 8                                                               10   10
                                                                                                                                    16                                                                     4                                                                5    5
                                                                                                                                     8                                                                     2                                                               16   16
                                                                                                                                     4                                                                     1                                                                8    8
                                                                                                                                     2                                                                                                                                      4    4
                                                                                                                                     1                                                                                                                                      2    2
                                                                                                                                                                                                                                                                            1    1
════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════ ════
   0    1    7    2    5    8   16    3   19    6   14    9    9   17   17    4   12   20   20    7    7   15   15   10   23   10  111   18   18   18  106    5   26   13   13   21   21   21   34    8  109    8   29   16   16   16  104   11   24   24   24   11   11  112  112   19   32   19   32   19

```



## Ruby

The ''object'' is an environment for instance variables. These variables use the <code>@</code> sigil. We create 12 objects, and put <code>@n</code> and <code>@cnt</code> inside these objects. We use <code>Object#instance_eval</code> to switch the current object and bring those instance variables into scope.

```ruby
# Build environments
envs = (1..12).map do |n|
  Object.new.instance_eval {@n = n; @cnt = 0; self}
end

# Until all values are 1:
until envs.all? {|e| e.instance_eval{@n} == 1}
  envs.each do |e|
    e.instance_eval do          # Use environment _e_
      printf "%4s", @n
      if @n > 1
        @cnt += 1               # Increment step count
        @n = if @n.odd?         # Calculate next hailstone value
               @n * 3 + 1
             else
               @n / 2
             end
      end
    end
  end
  puts
end
puts '=' * 48
envs.each do |e|                # For each environment _e_
  e.instance_eval do
    printf "%4s", @cnt          # print the step count
  end
end
puts
```

Ruby also provides the ''binding'', an environment for local variables. The problem is that local variables have lexical scope. Ruby needs the lexical scope to parse Ruby code. So, the only way to use a binding is to evaluate a string of Ruby code. We use <code>Kernel#binding</code> to create the bindings, and <code>Kernel#eval</code> to evaluate strings in these bindings. The lines between <code><<-'eos'</code> and <code>eos</code> are multi-line string literals.

```ruby
# Build environments
envs = (1..12).map do |n|
  e = class Object
        # This is a new lexical scope with no local variables.
        # Create a new binding here.
        binding
      end
  eval(<<-EOS, e).call(n)
    n, cnt = nil, 0
    proc {|arg| n = arg}
  EOS
  e
end

# Until all values are 1:
until envs.all? {|e| eval('n == 1', e)}
  envs.each do |e|
    eval(<<-EOS, e)           # Use environment _e_
      printf "%4s", n
      if n > 1
        cnt += 1              # Increment step count
        n = if n.odd?         # Calculate next hailstone value
              n * 3 + 1
            else
              n / 2
            end
      end
    EOS
  end
  puts
end
puts '=' * 48
envs.each do |e|                # For each environment _e_
  eval('printf "%4s", cnt', e)  # print the step count
end
puts
```

```txt

   1   2   3   4   5   6   7   8   9  10  11  12
   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1

### ==========================================

   0   1   7   2   5   8  16   3  19   6  14   9

```



## Sidef

```ruby
func calculator({.is_one}   ) { 1 }
func calculator(n {.is_even}) { n / 2 }
func calculator(n           ) { 3*n + 1 }

func succ(this {_{:value}.is_one}, _) {
    return this
}

func succ(this, get_next) {
    this{:value} = get_next(this{:value})
    this{:count}++
    return this
}

var enviornments = (1..12 -> map {|i| Hash(value => i, count => 0) });

while (!enviornments.map{ _{:value} }.all { .is_one }) {
    say enviornments.map {|h| "%4s" % h{:value} }.join;
    enviornments.range.each { |i|
        enviornments[i] = succ(enviornments[i], calculator);
    }
}

say 'Counts';
say enviornments.map{ |h| "%4s" % h{:count} }.join;
```

```txt

   1   2   3   4   5   6   7   8   9  10  11  12
   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1
Counts
   0   1   7   2   5   8  16   3  19   6  14   9

```



## Tcl

The simplest way to make a first-class environment in Tcl is to use a dictionary; the <code>dict with</code> command (and <code>dict update</code>; not shown here) will expand a dictionary and bind it to variables for the duration of its body script.

```tcl
package require Tcl 8.5

for {set i 1} {$i <= 12} {incr i} {
    dict set hailenv hail$i [dict create num $i steps 0]
}
while 1 {
    set loopagain false
    foreach k [dict keys $hailenv] {
	dict with hailenv $k {
	    puts -nonewline [format %4d $num]
	    if {$num == 1} {
		continue
	    } elseif {$num & 1} {
		set num [expr {3*$num + 1}]
	    } else {
		set num [expr {$num / 2}]
	    }
	    set loopagain true
	    incr steps
	}
    }
    puts ""
    if {!$loopagain} break
}
puts "Counts..."
foreach k [dict keys $hailenv] {
    dict with hailenv $k {
	puts -nonewline [format %4d $steps]
    }
}
puts ""
```

```txt

   1   2   3   4   5   6   7   8   9  10  11  12
   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1
   1   1   1   1   1   1   1   1   1   1   1   1
Counts...
   0   1   7   2   5   8  16   3  19   6  14   9

```



## zkl

In zkl, classes wrap state. All instances of a class share code but each instance binds code to itself. In this task, class creation, instead of returning a new class instance, it returns a bound function. Calling this function calculates the next hailstone in the environment the function is bound to. To get the counts from the class/environment, we ask the function for its container and then pull the count.

```zkl
class Env{
   var n,cnt=0;
   fcn init(_n){n=_n; returnClass(self.f)}
   fcn f{
      if(n!=1){
         cnt += 1;
         if(n.isEven) n=n/2; else n=n*3+1;
      }
      n
   }
}
```


```zkl
var es=(1).pump(12,List,Env);
while(1){
   ns:=es.run(True);
   ns.pump(String,"%4d".fmt).println();
   if (not ns.filter('!=(1))) break;
}
println("Counts:");
es.pump(String,fcn(e){"%4d".fmt(e.container.cnt)}).println();
```

```txt

   1   1  10   2  16   3  22   4  28   5  34   6
   1   1   5   1   8  10  11   2  14  16  17   3
   1   1  16   1   4   5  34   1   7   8  52  10
   1   1   8   1   2  16  17   1  22   4  26   5
   1   1   4   1   1   8  52   1  11   2  13  16
   1   1   2   1   1   4  26   1  34   1  40   8
   1   1   1   1   1   2  13   1  17   1  20   4
   1   1   1   1   1   1  40   1  52   1  10   2
   1   1   1   1   1   1  20   1  26   1   5   1
   1   1   1   1   1   1  10   1  13   1  16   1
   1   1   1   1   1   1   5   1  40   1   8   1
   1   1   1   1   1   1  16   1  20   1   4   1
   1   1   1   1   1   1   8   1  10   1   2   1
   1   1   1   1   1   1   4   1   5   1   1   1
   1   1   1   1   1   1   2   1  16   1   1   1
   1   1   1   1   1   1   1   1   8   1   1   1
   1   1   1   1   1   1   1   1   4   1   1   1
   1   1   1   1   1   1   1   1   2   1   1   1
   1   1   1   1   1   1   1   1   1   1   1   1
Counts:
   0   1   7   2   5   8  16   3  19   6  14   9

```


