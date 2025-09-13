+++
title = "Stair-climbing puzzle"
description = ""
date = 2019-07-01T14:04:53Z
aliases = []
[extra]
id = 4924
[taxonomies]
categories = ["task"]
tags = []
+++

From [http://lambda-the-ultimate.org/node/1872 Chung-Chieh Shan] (LtU):

Your stair-climbing robot has a very simple low-level API: the "step" function takes no argument and attempts to climb one step as a side effect. Unfortunately, sometimes the attempt fails and the robot clumsily falls one step instead. The "step" function detects what happens and returns a boolean flag: true on success, false on failure.

Write a function "step_up" that climbs one step up [from the initial position] (by repeating "step" attempts if necessary). Assume that the robot is not already at the top of the stairs, and neither does it ever reach the bottom of the stairs. How small can you make "step_up"? Can you avoid using variables (even immutable ones) and numbers?

Here's a pseudo-code of a simple recursive solution without using variables:

```txt

func step_up()
{
    if not step() {
        step_up();
        step_up();
    }
}

```

Inductive proof that step_up() steps up one step, if it terminates:
* Base case (if the step() call returns true): it stepped up one step. QED
* Inductive case (if the step() call returns false): Assume that recursive calls to step_up() step up one step. It stepped down one step (because step() returned false), but now we step up two steps using two step_up() calls. QED



The second (tail) recursion above can be turned into an iteration, as follows:

```txt

func step_up()
{
    while not step() {
        step_up();
    }
}

```



## ActionScript


### Iterative


```ActionScript
function stepUp()
{
	var i:int = 0;
	while(i < 1)
		if(step())i++;
		else i--;
}
```


### Recursive


```ActionScript
function stepUp()
{
	if(!step())
	{
		stepUp();
		stepUp();
	}
}
```



## Ada


```Ada
procedure Step_Up is
begin
   while not Step loop
      Step_Up;
   end loop;
end Step_Up;
```

The following is a test program simulating Step:

```Ada
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;

procedure Scaffolding is
   package Try is new Ada.Numerics.Discrete_Random (Boolean);
   use Try;
   Dice  : Generator;
   Level : Integer := 0;

   function Step return Boolean is
   begin
      if Random (Dice) then
         Level := Level + 1;
         Ada.Text_IO.Put_Line ("Climbed up to" & Integer'Image (Level));
         return True;
      else
         Level := Level - 1;
         Ada.Text_IO.Put_Line ("Fell down to" & Integer'Image (Level));
         return False;
      end if;
   end Step;

   procedure Step_Up is
   begin
      while not Step loop
         Step_Up;
      end loop;
   end Step_Up;
begin
   Reset (Dice);
   Step_Up;
end Scaffolding;
```

Sample output:

```txt

Fell down to-1
Climbed up to 0
Fell down to-1
Climbed up to 0
Fell down to-1
Fell down to-2
Climbed up to-1
Climbed up to 0
Climbed up to 1

```



## Aime

```aime
void step_up(void)
{
    while (!step()) {
        step_up();
    }
}
```



## ALGOL 68

```Algol68
   PROC step up = VOID:
   BEGIN
      WHILE NOT step DO
         step up
      OD
   END # step up #;
```
The following is a test program simulating step:
```Algol68

PROC scaffolding = VOID:
BEGIN
   INT level := 0;

   PROC step = BOOL:
   BEGIN
      IF random > 0.5 THEN
         level +:= 1;
         print(("Climbed up to",level, new line));
         TRUE
      ELSE
         level -:= 1;
         print(("Fell down to",level, new line));
         FALSE
      FI
   END # step #;

   PROC step up = VOID:
   BEGIN
      WHILE NOT step DO
         step up
      OD
   END # step up #;

   step up
END # scaffolding #;

scaffolding
```

Sample output:

```txt

Fell down to         -1
Fell down to         -2
Climbed up to         -1
Climbed up to         +0
Climbed up to         +1

```



## AutoHotkey

Recursive solution:

```AutoHotkey
step_up()
{
    While !step()
       step_up()
}
```


## AWK


```AWK

function step_up() {
    while (!step()) { step_up() }
}

```



## BASIC

For many (most?) BASICs, <code>STEP</code> is a (case-insensitive) keyword, therefore the "step" function would need a different name -- in this example, "step1". (Also, for some BASICs -- notably those influenced by [[Microsoft]]'s [[QuickBASIC]] -- the underscore character ("'''_'''") is invalid inside subroutine names.)


```qbasic
SUB stepup
    IF NOT step1 THEN stepup: stepup
END SUB
```


See also: [[#BBC BASIC|BBC BASIC]], [[#Liberty BASIC|Liberty BASIC]], [[#PureBasic|PureBasic]], [[#TI-83 BASIC|TI-83 BASIC]]


## BBC BASIC

Recursive solution:

```bbcbasic
      DEF PROCstepup
      IF NOT FNstep PROCstepup : PROCstepup
      ENDPROC
```



## C


```c
void step_up(void)
{
    while (!step()) {
        step_up();
    }
}
```


The following uses a variable and is a bit longer, but avoids a possible stack overflow by risking a probably less likely integer overflow instead:

```c
void step_up(void)
{
    int i = 0;

    while (i < 1) {
        if (step()) {
            ++i;
        } else {
            --i;
        }
    }
}
```



## C++


```cpp
void step_up()
{
  while (!step()) step_up();
}
```


The following uses a variable and is a bit longer, but avoids a possible stack overflow:

```cpp
void step_up()
{
  for (int i = 0; i < 1; step()? ++i : --i);
}
```


## C#

```c#
void step_up() {
    while (!step()) step_up();
}
```



## Clojure

First, some boilerplate.


```lisp
;; the initial level
(def level (atom 41))

;; the probability of success
(def prob 0.5001)


(defn step
  []
  (let [success (< (rand) prob)]
    (swap! level (if success inc dec))
    success) )
```


=== Tail-recursive ===
The internal recursion uses a counter; see the function documentation.


```lisp
(defn step-up1
  "Straightforward implementation: keep track of how many level we
   need to ascend, and stop when this count is zero."
  []
  (loop [deficit 1]
    (or (zero? deficit)
	(recur (if (step) (dec deficit)
		   (inc deficit)))) ) )
```



###  Recursive

This satisfies Chung-chieh's challenge to avoid using numbers. Might blow the stack as
''p'' approaches 0.5.


```lisp
(defn step-up2
  "Non-tail-recursive. No numbers."
  []
  (if (not (step))
    (do (step-up2) ;; undo the fall
	(step-up2) ;; try again
	)
    true))
```


## Common Lisp


```lisp
(defun step-up ()
  (unless (step) (step-up) (step-up)))
```



## D

The recursive version (note that "step_up" is equivalent to "step_up()" in D):

```d
void step_up()
{
    while(!step)
        step_up;
}
```

The non-recursive version, using 1 variable:

```d
void step_up_nr()
{
    for(uint i = 0; i < 1; step ? ++i : --i) {};
}
```

Test program:

```d
import std.stdio;
import std.random;

int position;
bool step()
{
    bool r = rand() > (uint.max / 2);
    if(r)
        writefln("Climbed up to %d", ++position);
    else
        writefln("Fell down to %d", --position);
    return r;
}

void step_up()
{
    while(!step)
        step_up;
}

void main()
{
    rand_seed(0, 0); // to make it somewhat repeatable
    step_up;
}
```

Sample output:

```txt
Fell down to -1
Fell down to -2
Fell down to -3
Fell down to -4
Climbed up to -3
Fell down to -4
Climbed up to -3
Climbed up to -2
Climbed up to -1
Climbed up to 0
Climbed up to 1
```



## E


This is at first a very direct {{trans|Clojure}}

The problem framework:


```e
var level := 41
var prob := 0.5001

def step() {
    def success := entropy.nextDouble() < prob
    level += success.pick(1, -1)
    return success
}
```


Counting solution:


```e
def stepUpCounting() {
    var deficit := 1
    while (deficit > 0) {
        deficit += step().pick(-1, 1)
    }
}
```


Ordinary recursive solution:

```e
def stepUpRecur() {
    if (!step()) {
        stepUpRecur()
        stepUpRecur()
    }
}
```


Eventual-recursive solution. This executes on the vat ''queue'' rather than the stack, so while it has the same space usage properties as the stack-recursive version it does not use the stack which is often significantly smaller than the heap. Its return value resolves when it has completed its task.


```e
def stepUpEventualRecur() {
    if (!step()) {
        return when (stepUpEventualRecur <- (),
                     stepUpEventualRecur <- ()) -> {}
    }
}
```


Fully eventual counting solution. This would be appropriate for controlling an actual robot, where the climb operation is non-instantaneous (and therefore eventual):

```e
def stepUpEventual() {
    # This general structure (tail-recursive def{if{when}}) is rather common
    # and probably ought to be defined in a library.

    def loop(deficit) {
        if (deficit > 0) {
            return when (def success := step()) -> {
                loop(deficit + success.pick(-1, 1))
            }
        }
    }
    return loop(1)
}
```



## EchoLisp


```scheme

(define (step-up) (while (not (step)) (step-up)))
;; checking this is tail-recusive :
step-up
   → (#λ null (#while (#not (step)) (#lambda-tail-call)))

;; Experimentation (not part of the task)
;; How much step calls to climb  1000 stairs ?
;; success is the robot success probability
(define (step)
 (set! STEPS (1+ STEPS)) ;; count
 (< (random) SUCCESS)) ;; ->#t or #f

(define (climb stairs)
	(when (> stairs 0)  (step-up) (climb (1- stairs))))

(define (task  (stairs 1000))
	(for ((success (in-range 1 0 -5/100)))
	(set! SUCCESS success)
	(set! STEPS 0)
	(climb stairs)
	(writeln 'stairs stairs 'probability success 'steps STEPS)))

```


```txt

stairs     1000     probability     1         steps     1000
stairs     1000     probability     19/20     steps     1062
stairs     1000     probability     9/10      steps     1115
stairs     1000     probability     17/20     steps     1207
stairs     1000     probability     4/5       steps     1254
stairs     1000     probability     3/4       steps     1305
stairs     1000     probability     7/10      steps     1440
stairs     1000     probability     13/20     steps     1542
stairs     1000     probability     3/5       steps     1641
stairs     1000     probability     11/20     steps     1865
stairs     1000     probability     1/2       steps     2045
stairs     1000     probability     9/20      steps     2177
stairs     1000     probability     2/5       steps     2615
stairs     1000     probability     7/20      steps     2769
stairs     1000     probability     3/10      steps     3312
stairs     1000     probability     1/4       steps     3963
stairs     1000     probability     1/5       steps     5054
stairs     1000     probability     3/20      steps     6573
stairs     1000     probability     1/10      steps     9840
stairs     1000     probability     1/20      steps     18689

;; looks as if steps = stairs / success-probability

```



## Elixir

```elixir
defmodule Stair_climbing do
  defp step, do: 1 == :rand.uniform(2)

  defp step_up(true), do: :ok
  defp step_up(false) do
    step_up(step)
    step_up(step)
  end

  def step_up, do: step_up(step)
end

IO.inspect Stair_climbing.step_up
```



## Erlang


```erlang

-module(stair).
-compile(export_all).

step() ->
    1 == random:uniform(2).

step_up(true) ->
    ok;
step_up(false) ->
    step_up(step()),
    step_up(step()).

step_up() ->
    step_up(step()).

```



## Euphoria


```euphoria
procedure step_up()
    if not step() then
        step_up()
        step_up()
    end if
end procedure
```



## Factor


```factor
: step-up ( -- ) step [ step-up step-up ] unless ;
```



## Forth

Recursive. May exceed return stack unless compiler optimizes tail calls.

```forth
: step-up   begin step 0= while recurse repeat ;
```

Counting. Avoids using a named variable.

```forth
: step-up   -1 begin step if 1+ else 1- then ?dup 0= until ;
```



## Fortran

```fortran
module StairRobot
  implicit none

contains

  logical function step()
    ! try to climb up and return true or false
    step = .true.     ! to avoid compiler warning
  end function step

  recursive subroutine step_up_rec
    do while ( .not. step() )
       call step_up_rec
    end do
  end subroutine step_up_rec

  subroutine step_up_iter
    integer :: i = 0
    do while ( i < 1 )
       if ( step() ) then
          i = i + 1
       else
          i = i - 1
       end if
    end do
  end subroutine step_up_iter

end module StairRobot
```



## Go

38 bytes, no variables, no numbers.

```go
func step_up(){for !step(){step_up()}}
```



## Groovy


```Grovy

class Stair_climbing{
static void main(String[] args){
}
static def step_up(){
    while not step(){
            step_up();
            }
}

}

```



## Haskell


In Haskell, stateful computation is only allowed in a monad. Then suppose we have a monad <code>Robot</code> with an action <code>step :: Robot Bool</code>. We can implement <code>stepUp</code> like this:


```haskell
stepUp :: Robot ()
stepUp = untilM step stepUp

untilM :: Monad m => m Bool -> m () -> m ()
untilM test action = do
    result <- test
    if result then return () else action >> untilM test action
```


Here's an example implementation of <code>Robot</code> and <code>step</code>, as well as a <code>main</code> with which to test <code>stepUp</code>.


```haskell
import Control.Monad.State
import System.Random (StdGen, getStdGen, random)

type Robot = State (Int, StdGen)

step :: Robot Bool
step = do
    (i, g) <- get
    let (succeeded, g') = random g
    put (if succeeded then i + 1 else i - 1, g')
    return succeeded

startingPos = 20 :: Int

main = do
    g <- getStdGen
    putStrLn $ "The robot is at step #" ++ show startingPos ++ "."
    let (endingPos, _) = execState stepUp (startingPos, g)
    putStrLn $ "The robot is at step #" ++ show endingPos ++ "."
```


=={{header|Icon}} and {{header|Unicon}}==

Icon (and Unicon) don't have boolean values.  Instead expressions (<i>all
expressions</i>) either <i>succeed</i> or they <i>fail</i>.  Control
structures respond to this success or failure.  Assuming that <tt>step()</tt>
is implemented in Icon (or Unicon) and fails only when the implementation in
another language returns <tt>false</tt>, then:

```unicon
procedure step_up()
    return step() | (step_up(),step_up())
end
```


You can subtract a few more characters (and multiply the difficulty
of understanding) with:

```unicon
procedure step_up()
    (|not step(), step_up())
end
```



## J

'''Solution (Tacit):'''

```j>step         =: 0.6
 ?@0:
attemptClimb =: [: <:`>:@.step 0:
isNotUpOne   =: -.@(+/@])

step_up=: (] , attemptClimb)^:isNotUpOne^:_
```

Note that <code>0:</code> is not a number but a verb (function) that returns the number zero irrespective of its argument(s). And, arguably, infinity is not any specific number. And, finally, <code>step</code> is presumed to pre-exist in the task description. Therefore the above solution for <code>step_up</code> could validly be said to meet the restrictions of no variables or numbers.

J's verbs (functions) always take an argument. J programmers use verbs which ignore their arguments (e.g. <code>step</code> and <code>attemptClimb</code>) to serve as verbs which "do not take an argument".

'''Solution (Explicit):'''

```j
step_upX=: monad define           NB. iterative
  while. -. +/y do. y=. y , _1 1 {~ step 0 end.
)

step_upR=: monad define           NB. recursive (stack overflow possible!)
   while. -. step'' do. step_upR'' end.
)
```


'''Example usage:'''

```j
   step_up ''            NB. output is sequence of falls & climbs required to climb one step.
_1 1 _1 _1 1 1 1
   +/\ _1 1 _1 _1 1 1 1  NB. running sum of output (current step relative to start)
_1 0 _1 _2 _1 0 1
   +/\ step_up ''        NB. another example
_1 _2 _3 _2 _3 _2 _1 _2 _3 _4 _3 _2 _3 _2 _3 _2 _3 _2 _1 _2 _1 _2 _1 0 1
```



Another approach might be:


```J
keepTrying=: (, {: - _1 ^ step)^:({. >: {:)^:_
```


Here, the argument is the number of the starting step and the result is a list of the numbers of each visited step including the initial and final steps.  For example:


```J
   keepTrying 2
2 1 0 1 2 3
   keepTrying 3
3 2 3 2 3 2 3 4
   keepTrying 4
4 5
```



## Java

```java
public void stepUp() {
  while (!step()) stepUp();
}
```

The following uses a variable and is a bit longer, but avoids a possible stack overflow:

```java
public void stepUp(){
  for (int i = 0; i < 1; step() ? ++i : --i);
}
```



## jq

Since jq is a purely functional language, we need to keep track of time explicitly. This can be done using a clock that ticks the time:

```jq>def tick: .+1;</lang

To model the robot's success and failure, we shall assume a sufficiently large array of 0/1 values is available.
To avoid problems with modeling infinite time, we will pad the array with 1s if necessary.

```jq
def random: [0, 0, 0, 1, 0, 1, 1, 0];
```


"step" returns true or false based on the current time (the input) and the value of "random":

```jq
def step:
  random as $r
  | if . >= ($r|length) then true else ($r[.] == 1) end ;
```


We can now define step_up:

```jq
def step_up:
  if step then tick
  else tick | step_up | step_up
  end;
```

Now we can start the simulation at time 0; step_up will then emit the number of "step" attempts that have been made to achieve success:

```jq
0 | step_up
```

```sh
$ jq -n -f stair-climbing_puzzle.jq
11
```


### Tail Call Optimization

To take advantage of jq's TCO (available in versions of jq after the release of Version 1.4), the step_up
function must be tail-recursive and have arity 0.  This can be
achieved by providing [time, goal] as the input as follows:

```jq
def tco_step_up:
  .[0] as $time | .[1] as $goal
  | if $goal == 0 then $time
    else
       if $time|step then $goal - 1 else $goal + 1 end
       | [ ($time|tick), .] | tco_step_up
    end ;
```

The simulation can then be started as follows:

```jq
[0,1] | tco_step_up
```



## Julia

As specified, shorter and fewer numbers preferred. It may be supposed that the robot would reach the bottom of any steps well before blowing the stack to reboot.

```julia

step_up() = while !step() step_up() end

```

Here is an example to test the code with a step that has a 1/3 failure rate:

```julia

step() = (b = rand([true,true,false]); println(b); b)
step_up()

```

```txt

julia>  step_up()
true

julia>  step_up()
true

julia>  step_up()
false
true
true

```



## Kotlin

```scala
// version 1.2.0

import java.util.Random

val rand = Random(6321L) // generates short repeatable sequence
var position = 0

fun step(): Boolean {
    val r = rand.nextBoolean()
    if (r)
        println("Climbed up to ${++position}")
    else
        println("Fell down to ${--position}")
    return r
}

fun stepUp() {
    while (!step()) stepUp()
}

fun main(args: Array<String>) {
    stepUp()
}
```


```txt

Fell down to -1
Fell down to -2
Climbed up to -1
Climbed up to 0
Fell down to -1
Climbed up to 0
Fell down to -1
Climbed up to 0
Climbed up to 1

```



## Liberty BASIC


```lb
'This demo will try to get the robot to step up
'Run it several times to see the differences; sometimes the robot falls
'quite a ways before making it to the next step up, but sometimes he makes it
'on the first try

result = Stepp.Up()

Function Stepp.Up()
    While Not(Stepp())
        result = Stepp.Up()
    Wend
End Function

Function Stepp()
    Stepp = Int((Rnd(1) * 2))
    If Stepp Then
        Print "Robot stepped up"
    Else
        Print "Robot fell down"
    End If
End Function
```



## Logo

Recursive.

```logo
to step.up
  if not step [step.up step.up]
end
```

Constant space (fully tail-recursive).

```logo
to step.up [:n 1]
  if :n=0 [stop]
  (step.up ifelse step [:n-1] [:n+1])
end
```



## Lua



```Lua

function step_up()
    while not step() do step_up() end
end

```



## Mathematica



```Mathematica
StepUp[] := If[!Step[], StepUp[]; StepUp[]]
```



## MATLAB


```MATLAB
function step_up()
    while ~step()
        step_up();
    end
```



## Nim


```nim
proc stepUp1 =
  var deficit = 1
  while deficit > 0:
    if step():
      dec deficit
    else:
      inc deficit

proc stepUp2 =
  while not step():
    stepUp2()
```



## OCaml


```ocaml
let rec step_up() =
  while not(step()) do
    step_up()
  done
;;
```



## Oz

Recursive solution:

```oz
proc {StepUp}
   if {Not {Step}} then
      {StepUp}  %% make up for the fall
      {StepUp}  %% repeat original attempt
   end
end
```

Might lead to a stack overflow because the first call to <code>StepUp</code> is not in tail position.

Iterative solution:

```oz
proc {StepUp}
   Level = {NewCell 0}
in
   for until:@Level == 1 do
      if {Step} then Level := @Level + 1
      else Level := @Level - 1
      end
   end
end

```

Oz has arbitrary large integers. So if the robot is very unlucky, the contents of the <code>Level</code> variable will fill up all the memory and the program will fail. I believe this problem needs infinite memory to be solved for all cases.


## PARI/GP


```parigp
step_up()=while(!step(),step_up())
```



## Pascal

Recursive solution:

```pascal
procedure stepUp;
begin
  while not step do
    stepUp;
end;
```



## Perl


```perl
sub step_up { step_up until step; }
```



## Perl 6


```perl6
sub step_up { step_up until step; }
```



## Phix


```Phix
procedure step_up()
    while not step() do step_up() end while
end procedure
```



## PicoLisp


```PicoLisp
(de stepUp ()
   (until (step1)  # ('step1', because 'step' is a system function)
      (stepUp) ) )
```



## PowerShell


```powershell
function StepUp
    {
    If ( -not ( Step ) )
        {
        StepUp
        StepUp
        }
    }

#  Step simulator for testing
function Step
    {
    If ( Get-Random 0,1 )
        {
        $Success = $True
        Write-Verbose "Up one step"
        }
    Else
        {
        $Success = $False
        Write-Verbose "Fell one step"
        }
    return $Success
    }

#  Test
$VerbosePreference = 'Continue'
StepUp
```

```txt
VERBOSE: Fell one step
VERBOSE: Fell one step
VERBOSE: Up one step
VERBOSE: Fell one step
VERBOSE: Fell one step
VERBOSE: Up one step
VERBOSE: Up one step
VERBOSE: Up one step
VERBOSE: Fell one step
VERBOSE: Up one step
VERBOSE: Up one step
```



## PureBasic

Iterative version using one variable.

```PureBasic
Procedure step_up()
  Protected i
  Repeat: If _step(): i + 1: Else: i - 1: EndIf: Until i = 1
EndProcedure
```

Recursive version. Stack may overflow as probability of a fall approaches or exceeds 50%.

```PureBasic
Procedure step_up()
  While Not _step()
    step_up()
  Wend
EndProcedure
```

Demonstration program.

```PureBasic
Global level

Procedure _step()
  If Random(1) ;equal chance of stepping up or falling down
    level + 1
    PrintN("Climbed up to " + Str(level))
    ProcedureReturn #True
  Else
    level - 1
    PrintN("Fell down to " + Str(level))
    ProcedureReturn #False
  EndIf
EndProcedure

;recursive
Procedure step_up()
  While Not _step()
    step_up()
  Wend
EndProcedure

If OpenConsole()
  PrintN("Begin at level: " + Str(level))
  step_up()
  PrintN("*** Now at level: " + Str(level))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
Begin at level: 0
Fell down to -1
Climbed up to 0
Fell down to -1
Climbed up to 0
Climbed up to 1
*** Now at level: 1
```



## Python



###  Iterative


```python
def step_up1()
  "Straightforward implementation: keep track of how many level we
   need to ascend, and stop when this count is zero."
  deficit = 1
  while deficit > 0:
    if step():
      deficit -= 1
    else:
      deficit += 1
```



###  Recursive

This satisfies Chung-chieh's challenge to avoid using numbers. Might blow the stack as
''p'' approaches 0.5.


```python
def step_up2():
  "No numbers."
  while not step():
    step_up2() # undo the fall
```



## R


The step() function described would not be idiomatic R, since it would
require using the global assignment operator to get the side effect.

```R
step <- function() {
    success <- runif(1) > p
    ## Requires that the "robot" is a variable named "level"
    level <<- level - 1 + (2 * success)
    success
}
```



### Recursive Solution



```R
stepUp <- function() {
    while(! step()) {
        stepUp()
    }
}
```



### Iterative Solution



```R
stepUpIter <- function() {
    i <- 0
    while ( ! i) {
        i <- i - 1 + (2 * step())
    }
}
```


Example output:

```txt

> p <- 0.25
> level <- 1
> print(level)
[1] 1
> stepUp()
> print(level)
[1] 2
> stepUpIter()
> print(level)
[1] 3

```



## Racket


```Racket
#lang racket
(define p 0.5001)
(define (step)
  (> p (random)))

(define (step-up n)
  (cond ((zero? n) 'done)
        ((step) (step-up (sub1 n)))
        (else (step-up (add1 n)))))

(step-up 1)
```



## REBOL


```REBOL
REBOL [
    Title: "Stair Climber"
    URL: http://rosettacode.org/wiki/Stair_Climbing
]

random/seed now

step: does [random/only reduce [yes no]]

; Iterative solution with symbol stack. No numbers, draws a nifty
; diagram of number of steps to go. This is intended more to
; demonstrate a correct solution:

step_up: func [/steps s] [
	either not steps [
		print "Starting up..."
		step_up/steps copy [|]
	][
		while [not empty? s][
			print ["    Steps left:" s]
			either step [remove s][append s '|]
		]
	]
]

step_up  print ["Success!" crlf]

; Recursive solution. No numbers, no variables. "R" means a recover
; step, "+" means a step up.

step_upr: does [if not step [prin "R " step_upr  prin "+ " step_upr]]

step_upr  print ["Success!" crlf]

; Small recursive solution, no monitoring:

step_upt: does [if not step [step_upt step_upt]]

step_upt  print "Success!"
```


Output:


```txt
Starting up...
    Steps left: |
    Steps left: | |
    Steps left: |
Success!

R R + R R R R R R R + + R + + + + + + R + R R + R + R + R + + + Success!

Success!
```



## REXX


```rexx
step_up:           do  while \step();   call step_up
                   end
          return
```



## Ring


```ring

stepup()

func stepup
     n = 0
     while n < 1
           if stp() n=n+1 else n= n-1 ok
           see n + nl
     end

func stp
     return 0

```



## Run BASIC


```runbasic

result = stepUp()

Function stepUp()
    While Not(stepp())
        result = stepUp()
    Wend
End Function

Function stepp()
	stepp = int((Rnd(1) * 2))
	print "Robot stepped "+word$("up down",stepp+1)
End Function

```



## Ruby


```ruby
def step_up
  start_position = $position
  step until ($position == start_position + 1)
end

# assumptions about the step function:
# - it maintains the current position of the robot "as a side effect"
# - the robot is equally likely to step back as to step up
def step
  if rand < 0.5
    $position -= 1
    p "fall (#$position)" if $DEBUG
    return false
  else
    $position += 1
    p "rise (#$position)" if $DEBUG
    return true
  end
end

$position = 0
step_up
```

Sample run:

```txt
$ ruby -d stair.climbing.rb
"fall (-1)"
"rise (0)"
"fall (-1)"
"rise (0)"
"fall (-1)"
"fall (-2)"
"rise (-1)"
"rise (0)"
"rise (1)"
```



## SAS



```SAS

%macro step();
	%sysfunc(round(%sysfunc(ranuni(0))))
	%mend step;

```



### Recursive


```SAS

%macro step_up();

	%if not %step %then %do;
		%put Step Down;
		%step_up;
		%step_up;
		%end;
	%else %put Step Up;

	%mend step_up;

%step_up;

```



### Iterative


```SAS

%macro step_up();

	%do %while (not %step);
		%put Step Down;
		%step_up;
		%end;
	%put Step Up;

	%mend step_up;

```




Sample Output:

```txt

Step Down
Step Down
Step Down
Step Up
Step Down
Step Down
Step Up
Step Up
Step Up
Step Up
Step Up

```



## Scala

Simple recursive solution:


```scala
def stepUp { while (! step) stepUp }
```


Non-recursive solution which almost gets away with not having named variables:


```scala
def stepUp {
  def rec: List[Boolean] => Boolean = step :: (_: List[Boolean]) match {
    case true :: Nil => true
    case true :: false :: rest => rec(rest)
    case other => rec(other)
  }
  rec(Nil)
}
```



## Scheme


```scheme
(define (step-up n-steps)
  (cond ((zero? n-steps) 'done)
        ((step) (step-up (- n-steps 1)))
        (else (step-up (+ n-steps 1)))))
```



## Seed7


```seed7
const proc: step_up is func
  begin
    while not doStep do
      step_up;
    end while;
  end func;
```



## Sidef


```ruby
func step_up() {
    while (!step()) {
        step_up();
    }
}
```



## Smalltalk

The following uses a block closure and the recursive solution which consumes stack until successful.

```smalltalk
Smalltalk at: #stepUp put: 0.
stepUp := [ [ step value ] whileFalse: [ stepUp value ] ].
```



## Standard ML


```Standard ML

(*
 * val step : unit -> bool
 * This is a stub for a function which returns true if successfully climb a step or false otherwise.
 *)
fun step() = true

(*
 * val step_up : unit -> bool
 *)
fun step_up() = step() orelse (step_up() andalso step_up())

```



## Swift


```swift
func step_up() {
  while !step() {
    step_up()
  }
}
```


The following uses a variable and is a bit longer, but avoids a possible stack overflow:

```swift
func step_up() {
  for var i = 0; i < 1; step()? ++i : --i { }
}
```



## Tcl

The setup (note that <code>level</code> and <code>steps</code> are not used elsewhere, but are great for testing…)

```tcl
set level 41
set prob 0.5001
proc step {} {
    global level prob steps
    incr steps
    if {rand() < $prob} {
	incr level 1
	return 1
    } else {
	incr level -1
	return 0
    }
}
```


### Iterative Solution

All iterative solutions require a counter variable, but at least we can avoid any literal digits...

```tcl
proc step-up-iter {} {
    for {incr d} {$d} {incr d} {
	incr d [set s -[step]]; incr d $s
    }
}
```


### Recursive Solution

This is the simplest possible recursive solution:

```tcl
proc step-up-rec {} {
    while {![step]} step-up-rec
}
```


=={{header|TI-83 BASIC}}==
TI-83 BASIC doesn't have functions (only subroutines), so a variable must be used as the return value for <code>prgmSTEP</code>. Set <code>A</code> to <code>1</code> before calling to display the offset from the stair it was called from (store in D). Set <code>B</code> to <code>1</code> to pause after each attempt. <code>C</code> is the return variable for <code>prgmSTEP</code>. <code>D</code> is the stair it is on (only used for display, and not used if <code>A</code> isn't <code>1</code>).

<code>prgmSTEP</code>:

```ti83b>If rand
.5:Then
0→C
Disp "FALL"
If A=1:Then
D-1→D
Disp D
End
Else
1→C
Disp "CLIMB"
If A=1:Then
D+1→D
Disp D
End
End
If B=1
Pause
```


<code>prgmSTEPUP</code>:

```ti83b
prgmSTEP
While C=0
prgmSTEPUP
prgmSTEP
End
```



## XPL0


```XPL0
proc Step_up;           \Iterative version
int I;
[I:= 0;
while I<1 do
        if Step then I:= I+1
        else I:= I-1;
];

proc Step_up;           \Recursive version
while not Step do Step_up;
```



## zkl

According to Leon P Smith in the referenced LTU thread:

This version consumes stack space proportional to the number of steps it needs to go up, as opposed to the number of steps it attempts. This is a substantial difference if the probability of success is only somewhat greater than or equal to 1/2.

```zkl
fcn step{  } // add code to return Bool
fcn stepUp{ while(not step()){ self.fcn() } }
```

You could also use "stepUp" instead of self.fcn, self.fcn seems a little clearer and makes it easier to refactor.

An example step function:
```zkl
var position=0;
fcn step(){  //-->0|1
   r:=(0).random(2);	// 0 or 1
   if(r) println("Climbed up to ",position+=1);
   else  println("Fell down to ", position-=1);
   r
}
stepUp();
```

```txt

Fell down to -1
Climbed up to 0
Fell down to -1
Fell down to -2
Climbed up to -1
Climbed up to 0
Climbed up to 1

```

