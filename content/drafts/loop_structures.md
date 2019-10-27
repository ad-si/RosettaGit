+++
title = "Loop Structures"
description = ""
date = 2019-06-29T07:35:57Z
aliases = []
[extra]
id = 1786
[taxonomies]
categories = []
tags = []
+++

{{DeprecatedTask}}
[[Category:Maintenance]]In this former task, we document loop structures offered by different languages.

;What are loops?:

Loops are control structures that allow sections of code to be executed repeatedly according to the controlling conditions of the loop.

There are two types of loops:

;Repetition:
Additionally, there is the overly simple repetitive loop: [[repeat|repetition]]. The simplistic construct executes a block of code, or a procedure, a given number of times, without explicitly exposing any state change to the looped procedure.

;Iterative loops:

An [[:Category:Iteration|iterative loop]] repeatedly executes a set of instructions as the iterator steps through a series of values. Types of iterative loops include [[for loop]]s and [[foreach]] loops. An iterative loop is a repetition but with a variable dependent on the current iteration. This allows the looped procedure to vary slightly between iterations. For example, the same operation can be carried out on each iteration, but each time on a different object.

;Conditional loops:
A [[conditional loop]] tests for a condition around the loop, and repeatedly executes a block of [[instruction]]s whilst the [[condition]] is true. Types of [[conditional loop]]s include [[while loop]]s and [[do-while loop]]s. 

'''Examples here should be migrated to an appropriate [[:Category:Iteration|Iteration]] page and removed from here. If a page does not exist demonstrating a particular loop structure, discuss it [[:Category talk:Iteration|here]].'''





## AmbientTalk



### doTimes


```ambienttalk

 // print 1 2 3 ... 20
20.doTimes { |i| system.print(" "+i); }

```



###  each 


Iterate over a collection:


```ambienttalk

[ "foo", "bar", "baz" ].each: { |e|
  system.print(" "+e);
}
// prints: foo bar baz

```


==[[AppleScript]]==
'''NOT COVERED IN LOOP PAGES'''
===repeat-until===
 set i to 5
 repeat until i is less than 0
        set i to i - 1
 end repeat

 repeat
        --endless loop
 end repeat

===repeat-with===
        repeat with i from 1 to 20
                --do something
        end repeat

        set array to {1,2,3,4,5}
        repeat with i in array
                display dialog i
        end repeat


==[[AssemblyScript]]==
'''NOT COVERING ALL POSSIBLE LOOP OPTIONS'''


### while

  let isTrue: boolean = true;
  while(isTrue) {
    isTrue = false;
  }


### for

  for (let i: i32 = 0; i++) {
    i += i;
  }

==[[Brainf***]]==
'''NOT EXPLAINED THIS MUCH IN LOOP PAGES'''
BF's only control flow construct of any kind is a loop. Two of the eight commands define the start and end of a conditional loop.
* '''[''' - branch forward past matching ']' if the current cell is zero
* ''']''' - branch back to the matching '[' if the current cell is non-zero
In practice this is equivalent to a "while not zero" loop in other languages.
 [-]  set current cell to zero

 [->+>+<<]>>[-<<+>>] copy cell 0 to cell 1, using cell 2 as temporary storage

==[[C++]]==
=== Compile-Time Control Structures ===
'''Necessary?'''
A compile-time for loop can be generated with template metaprogramming. Example:

 // the loop
 template<int start, int finish, template<int n, typename T> class X> struct loop
 {
   typedef typename X<start, typename loop<start+1, finish, X>::type>::type type;
 };
 
 // the termination of the loop
 template<int finish, template<int n, typename T> class X> struct loop<finish, finish, X>
 {
   typedef typename X<finish, void>::type type;
 };

 // example usage: This implements just a very complicated way of building a multi-dimensional array
 
 // the loop body
 template<int n, typename T> struct build_array
 {
   typedef T type[n];
 };
 
 template<int n> struct build_array<n, void>
 {
   typedef double type;
 };
 
 // the loop execution: This is equivalent to
 // typedef double array_type[2][3][4][5];
 typedef loop<2,6,build_array>::type array_type;

==[[Clojure]]==
'''NOT COVERED IN LOOP PAGES'''

### loop


```clojure

 ;; loop/recur is the most general looping construct
 (loop [lst [1 3 5 7]
        ret []]
    (if lst
        (recur (rest lst) (conj ret (* (first lst) (first lst))))
        ret))
 ==> [1 9 25 49]

```



## Crack


### For


```crack

for( i=0; i<9; i++) 
  cout ` $i\n`;

```



## Dafny


```dafny

var i: int := 0;
while i < n
   invariant 0 <= i <= n
   decreases n - i
{
   i := i + 1;
}
assert i == n;

```



## Dao


### For


```java
for( i=0; i<9; ++i) io.writeln( i );
for( i = 0 : 8 ) io.writeln( i );
```



### For In


```java
items = { 1, 2, 3 }
for( item in items ) io.writeln( item )
```



### While


```java
i = 0
while( i < 5 ) { i += 1 }
```



### Do While


```java
i = 0
do { i += 1 } while( i < 9 )
```


=={{header|Déjà Vu}}==

### For

Déjà Vu has a for-loop protocol, so you can write your own iterators. The most commonly used iterators are <code>in</code> and <code>range</code>. The first iterates over a list, the second takes two arguments and goes from the first to the second, like a classic for-loop.

```dejavu
for i range 1 3:
    !print i # prints 1, 2 and 3
```


### While


```dejavu
while true:
    !print "This is the song that never ends..."
```


### Repeat


```dejavu
repeat 3:
    !print "This sentence is printed three times."
```


==[[Factor]]==
'''NOT COVERED IN LOOP PAGES'''

### Looping

Most looping is done with recursion. Tail recursion is properly optimized.
    : forever ( quot -- ) dup slip forever ; inline
    [ "A hungry raptor stalks you..." print flush 2000 random sleep ] forever


### Iteration

Most indices are implicit or not present at all.
    3 [ "pint" drink ] times
    { "high" "level" "language" } [ print ] each
        high
        level
        language
    10 [ sq ] map
        { 0 1 4 9 16 25 36 49 64 81 }
    { 1 2 3 } { 4 5 6 } [ * ] 2map .
        { 4 10 18 }
    10 [ even? ] subset .
        V{ 0 2 4 6 8 }
    0 10 3 <range> >array .
        { 0 3 6 9 }
    10 1 -2 <range> >array .
        { 10 8 6 4 2 }
    2222 [ dup 0 > ] [ 2/ dup ] [ ] unfold nip .
        { 1111 555 277 138 69 34 17 8 4 2 1 0 }
Iterating with an index:
    : indexed-alphabet. ( -- )
        "abcdefghijklmnopqrstuvwxyz"
        [ [ 1string ] [ number>string ] bi* ": " glue print ] each-index ;
==[[Forth]]==
===DO-LOOP===
 ( limit start ) DO ( iterated statements ) LOOP
 ( limit start ) DO ( iterated statements ) ( increment ) +LOOP
 LEAVE \ exits a DO loop
 UNLOOP EXIT \ cleans up loop counters from return stack before returning from the current word
example: Two standard iterations
 10 0 DO I . LOOP      \ Prints the numbers from 0 to 9
 10 0 DO I . 2 +LOOP   \ Prints the even numbers from 0 to 8

===BEGIN-UNTIL===
 BEGIN ( iterated statements ) ( conditional ) UNTIL
example: Counts down from a given number to zero
 : COUNTDOWN ( n -- )  BEGIN  DUP CR .  1- DUP 0< UNTIL  DROP ;

===BEGIN-AGAIN===
 BEGIN ( iterated statements ) AGAIN
example: echo user's input
 : FOREVER ( -- )   BEGIN  KEY EMIT  AGAIN ;

===BEGIN-WHILE-REPEAT===
 BEGIN ( unconditional iterated statements ) ( conditional ) WHILE  ( conditional iterated statements )  REPEAT
example: counts down from a given number to one
 : COUNTDOWN ( n -- )  BEGIN  DUP WHILE  CR DUP . 1-  REPEAT  DROP ;
Additional WHILE clauses may be added to a loop, but each extra WHILE requires a matching THEN after the REPEAT.


### Mixed Structures

Because Forth's compiler is laid bare to the programmer, it is quite easy to both define your own looping structures or combine existing structures in interesting ways. The rules for such combining are somewhat involved, though discussions can be found in the gforth user's manual, among other places. These more complex looping constructs can make up for Forth's lack of a "break" word, and can allow expressing complex loops without resorting to boolean variables. A practical example is also found in the [[Binary search]] task.

A good example of a useful combination is this complex loop:
 BEGIN
   ( condition 1 )
 WHILE
   ( condition 2 )
 UNTIL
   ( condition 2 succeeded )
 ELSE
   ( condition 1 failed )
 THEN
An example of using this idiom in practice might be this pseudo-Forth
 BEGIN
   read-next-record
 WHILE
   found-record
 UNTIL
   process-record
 ELSE
   error" Ran out of records looking for the right one!"
 THEN


## Frink

In all of the loops below, the curly braces can be omitted if the body is a single statement.


### For Loop

A <CODE>for</CODE> loop is really a <CODE>foreach</CODE> loop that can work with range operators or iterate through various data structures.  The <CODE>to</CODE> operator creates an enumerating expression that lazily steps through its range.

```frink

for i = 1 to 1000000
{
   println[i]
} 

```


The <CODE>to</CODE> operator can be combined with a <CODE>step</CODE> statement:

```frink

for i = 1 to 1000000 step 3
   println[i]

```


As a <CODE>foreach</CODE> statement.  The <CODE>for</CODE> construct can iterate over the elements of an array, set, dictionary, or enumerating expression.

```frink

for i = [2,3,7,9]
   println[i]

```



### Do...While Loop


```frink

i=0
do
{
   i = i+1
} while i<1000 

```



==[[Groovy]]==


### While Loops

 while (true) {
   println 'groovy'
 }


### For Loops


```txt


 // iterate over a range
 x = 0
 for (i in 1..3) { x += i }
 assert x == 6

 // iterate over a list
 x = 0
 for (i in [1, 2, 3]) { x += i }
 assert x == 6

 // iterate over an array
 x = 0
 for (i in (1..3).toArray()) { x += i }
 assert x == 6

 // iterate over a map's key/value pairs
 x = 0
 for (i in map) { x += i.value }
 assert x = 6

 // iterate over a map's values
 x = 0
 for (i in map.values()) { x += i }
 assert x == 6

 // iterate over the characters in a string
 text = 'abc'
 list = []
 for (c in text) { list.add(c) }
 assert list == ['a', 'b', 'c']

```



### Each


```txt

 def stringList = [ "java", "perl", "python", "ruby" ];

 def stringMap = [ "Su" : "Sunday", "Mo" : "Monday", "Tu" : "Tuesday",
                  "We" : "Wednesday", "Th" : "Thursday", "Fr" : "Friday",
                  "Sa" : "Saturday" ];

 stringList.each() { print " ${it}" }; println "";
 // java perl python ruby

 stringMap.each() { key, value -> println "${key} == ${value}" };
 // Su == Sunday
 // We == Wednesday
 // Mo == Monday
 // Sa == Saturday
 // Th == Thursday
 // Tu == Tuesday
 // Fr == Friday  

 stringList.eachWithIndex() { obj, i -> println " ${i}: ${obj}" };
 // 0: java
 // 1: perl
 // 2: python
 // 3: ruby

 stringMap.eachWithIndex() { obj, i -> println " ${i}: ${obj}" };
 // 0: Su=Sunday
 // 1: We=Wednesday
 // 2: Mo=Monday
 // 3: Sa=Saturday
 // 4: Th=Thursday
 // 5: Tu=Tuesday
 // 6: Fr=Friday

```


==[[Haskell]]==

Most of the usual applications for loops are realized in Haskell by operations on (lazy) lists, like '''map''', '''fold''' or '''filter'''. It's unusual to use loops in an imperative style. However, if one insists on it, it's easy to make your own implementation of any loop variant.

Here are a few examples:

===Pre-checked loop (while)===

 whileM :: Monad m => m Bool -> m a -> m ()
 whileM cond body = 
   cond >>= \b -> if b then body >> untilM cond body else return ()

===Post-checked loop (repeat-until)===

 untilM :: Monad m => m Bool -> m a -> m ()
 untilM cond body = 
   body >> cond >>= \b -> if b then return () else untilM cond body

===For-style loop===

Simplest done by iterating over a list:

 forM :: Monad m => [a] -> (a -> m ()) -> m ()
 forM []     f = return ()
 forM (x:xs) f = f x >> forM xs f

==[[IDL]]==

It should be noted that IDL programmers tend to avoid loops -- most of the time loops are used to access the elements of arrays or vectors, and since IDL is an array language the same purpose can almost always be served in a faster, more elegant and more readable way though any of the array operations.


### for


  for i=0,50,2 do print,i

prints out every second number starting at 0 stopping at 50. Wherever a single command can go in IDL, there can always go a <tt>begin...end</tt> pair with arbitrary amount of code in between. Thus the above can also read 

  for variable = start, stop [,increment] do begin
    ;some code here
    ;some more code
  endfor

It is allowed but not required to use the appropriate "type of end" - i.e. it would be allowed to just say "end" instead of "endfor". However "endfor" (and "endwhile", "endif" etc) will throw an error if the wrong one is encountered at compile time and thus it is recommended to always use the more descriptive form since it makes debugging a lot easier.


### while


  while condition do command

Same extensions as above:

  while running do begin
    ; various snippets of code that might change the variable 
    ; "running" from something true to something false
  end[while]


### repeat


  repeat command until condition

etc


### goto


'<tt>Goto</tt>' exists and can in principle be forced to make a loop:

  label: 
    ;some code
  [if condition then $]
  goto label


### break


The <tt>break</tt> statement will immediately terminate the current innermost <tt>for</tt>, <tt>while</tt>, <tt>repeat</tt>, <tt>if</tt>, <tt>case</tt> or <tt>switch</tt> without having to resort to a <tt>goto</tt>.


==[[Kabap]]==

There is no native loop command in Kabap, but labels, variables, jumps and conditional execution are supported which is enough to create a basic loop structure.  Support for native loops is being prepared for the next major release.


### Basic loop


```Kabap

  $i = 0;
  :start;
    // Your loop code here
    $i = $i + 1;
    if $i < 20;
      goto start;

```


==[[Logo]]==
 forever [right random 360 forward 10]

 repeat 5 [right 180-36 forward 100]

Repeat and forever also have access to a loop counter, starting at 1.
 repeat 10 [print repcount]

 while [:x > 0] [make "x :x / 2]
 do.while [make "x :x / 2] [:x > 0]

 until [:x = 0] [make "x :x / 2]
 do.until [make "x :x / 2] [:x = 0]

==[[LSE64]]==
LSE's loop words all work via tail recursion, like [[Scheme]], by re-executing the current word. If used interactively, "repeat" works upon the current line. Exiting words, like "&&", "||", and "exit" can be used to exit an otherwise infinite loop (see other tasks).
 infinite : " again and " ,t repeat
 limited : continue? &repeat

Counted loops execute a specified word ''n'' times. Within that word, "count" accesses a loop counter, counted down to zero.
 body : count , sp
 10 body iterate    # 10 9 8 7 6 5 4 3 2 1 
 
 body? : count , sp  count 5 >
 10 body? &iterate   # 10 9 8 7 6 5

==[[Make]]==
 Make does looping through recursion.
 SUCC=`expr $* + 1`
 MAX=10
 
 all: 0-n;
 
 %-n: %-echo
    @-make -f loop.mk $(SUCC)-n MAX=$(MAX)
 
 %-echo:
    @echo $*
 
 $(MAX)-n: $(MAX)-echo;

Invoking it
 |make -f loop.mk MAX=2
 0
 1
 2

==[[newLISP]]==
{{works with|newLISP|v.9.0}}

### dotimes


 (dotimes (x 10) (println (+ x 1)))

===do-until===

 (set 'x 1)
 (do-until (= x 10) (inc 'x) (println x))

===do-while===

 (set 'x 1)
 (do-while (< x 10) (inc 'x) (println x))


### for

 (for (x 1 10) (println x))
==[[OCaml]]==

### let rec

The for and while loops are imperative features of OCaml, and most often it is rather recommended to prefer using functional loop designed with recursive functions, or better when iterating over a list or an array using a built-in iterator from the standard library or the extLib library.

```ocaml
let rec loop i =
  Printf.printf "%d\n" i;
  if i <= 4 then loop (i + 1)
in
loop 0
```


===Built-in Iterators===

```ocaml
List.iter
List.fold_left
Array.iter
Array.iteri
```



## Prolog


There are three primitive methods of looping in Prolog: recursion, fail-driven loops, and repeat-driven loops.


```Prolog
% recursion as loop
print_each_element([]).
print_each_element([E|T]) :- writeln(E), print_each_element(T).

% fail-driven loop
fact(foo).
fact(bar).
fact(baz).
print_each_fact :-
    (   fact(X), writeln(X), fail
    ;   true ).

% equivalently
%print_each_fact :- fact(X), writeln(X), fail.
%print_each_fact.

% repeat-driven loop
print_each_fact_again :-
    repeat,
        fact(X),
        writeln(X),
        X = baz,
    !.

go :-
    print_each_element([foo, bar, baz]),
    print_each_fact,
    print_each_fact_again.
```


Of the three recursion is the favoured approach as it requires no non-logical predicates and is thus easy to read in its declarative form.

The fail-driven loop form is a(n ab)use of the built-in backtracking mechanism of Prolog's reasoning engine.  In the specific example provided, fact(X) will first succeed, binding "foo" to X.  It will then write "foo" to the output (as a side effect of the writeln/1 predicate).  It then hits the call to fail/0 which is a non-logical predicate which always fails and thus always triggers backtracking.  On backtracking, the runtime will try fact(X) again and will find that it is true when X is bound to "bar".  This will then print and backtrack again.  A third time binds to and prints "baz".  A fourth time will fail because there is no more solution to the goal "fact(X)".  This triggers a further backtrack and a try on the second branch of the disjunction.  That second branch invokes the true/0 predicate which always succeeds.  This exits the query with an overall success.

The repeat-driven loop uses similar (ab)use of the backtracking mechanism.  Instead of employing a predicate that always fails, however, it employs one that will always succeed: repeat/0.  Thus, in this sample, fact(X) works as before, as does writeln(X), but the attempt to unify X with "baz" will fail for the first two attempts, causing the system to backtrack until it hits repeat.  Since repeat always succeeds it drives the engine forward again, testing each fact in succession.  Once X is unified with "baz" (which is to say once X contains the value "baz") the predicate carries on.  The cut operator !/0, guarantees that the predicate won't be re-entered later.

As with any language permitting higher-order invocations, using the looping primitives directly as above is often not a desirable thing.  Instead higher-order features would be used.


```Prolog
% using maplist/2 to replace explicit recursion on a list
print_each_element(L) :- maplist(writeln, L).

% using forall/2 to replace an explicit fail-driven loop
fact(foo).
fact(bar).
fact(baz).
print_each_fact() :- forall(fact(X), writeln(X)).
```


There are a myriad of such predicates available in a useful Prolog implementation (SWI-Prolog provides, non-exhaustively: include/3, exclude/3, partition/4-5, maplist/2-5, foldl/4-7, scanl/4-6, aggregate/3-4, aggregate_all/3-4, forall/2, findall/3-4, findnsols/4-5, bagof/3, setof/3, … just as the more fundamental wrappings.)  If the provided predicates do not permit the kinds of functionality desired for common patterns, it is trivial to make a new one.  As an illustration, this is the source code for forall/2:


```Prolog
:- meta_predicate forall(0,0).
forall(A, B) :- \+ (call(A), \+ call(B)).
```



## Pop11



###  until 

Variant of while is until loop:

 until condition do /* Action */ enduntil;

is equivalent to:

 while not(condition) do /* Action */ endwhile;


###  for 

One can process all elements of a list:

  for x in [a b c] do x => endfor;

It is possible to simultaneously process multiple lists:

 for x y in [a b c], [1 2 3] do [^x ^y] => endfor;

in first iteration sets x to "a" and y to 1, in the second x is "b" and y is 2, in the third (last) iteration x is "c" and y is 3. The iteration ends when the shortest list is exhausted.

Sometimes one wants to process tails of the list, to do this use on keyword instead of in keyword:

 for x on [a b c] do x => endfor;

in first iteration sets x to [a b c], in the second to [b c], etc...

There is also "counting" version of for loop:

 for x from 2 by 2 to 7 do x => endfor;

goes trough 2, 4 and 6.  Ommited by frase means by 1.

There is alse a C-like for loop:

 for action1 step  action2 till condition do /* Actions */ endfor;

is equivalent to

 action1
 while not(condition) do
    /* Actions */
    action2
 endwhile;

There are more specialized kinds of loops, but we skip them here.


###  quitloop quitif quitunless 

Inside loops one can use control transfers to prematurely exit the loop or end current iteration and start the next one:

 while true do n - 1 -> n; quitif(n=0); endwhile;

quits loop when n=0.  quitloop unconditionally quits loop,
quitunless(x) is equivalent to quitif(not(x)).


###  nextloop nextif nextunless 

Similarely to quitloop nextloop unconditionally ends current iteration and starts the new one, nextif(x) ends current iteration when x is true, nextunless(x) is equivalent to nextif(not(x)). The loop control transfers can be also used inside for (and until) loops.

Finally, it is frequently possible to avoid explicit iteration using higher order map functions (like appdata and mapdata).

==[[PostScript]]==

The "<tt>for</tt>" operator expects three numbers and a procedure on the stack. It will consume all four arguments then it will push the first number on the stack, execute the procedure, increase the first number by the second number, push it on the stack, execute the procedure etc until the third number is reached. For example

 10 12 200 
   {dup moveto 100 0 lineto} 
   for 
 stroke

will add lines to the <tt>currentpath</tt> that start at coordinates {10,10}; {22,22}; {34,34} ... and all end at {100,0}. The "stroke" operator then renders these lines on the current output device (usually a screen or a piece of paper).

==[[Python]]==


### for

Frequently one wants to both iterate over a list and increment a counter:


```python

 mylist = ["foo", "bar", "baz"]
 for i, x in enumerate(mylist):
     print "Element no.", i, " is", x

```



Iterating over more than one list + incrementing a counter:


```python

 for counter, [x, y, z] in enumerate(zip(lst1, lst2, lst3)):
     print counter, x, y, z

```



###  list comprehension expressions 


Typically used when you want to create a list and there is little logic involved. Faster than for loop:

 positives = [n for n in numbers if n > 0]

A list comprehension is an expression rather than a statement.  This allows them to be used as arguments to a function:


```python

   def square_each(n):
       results = []
       for each in n:
          results.append(each * each)
       return results
   squares_3x5 = square_each([x for x in range(100) if (x%3)==0 and (x%5)==0])
   # Return a list of all the squares of numbers from 1 up to 100 those numbers are
   # multiples of both 3 and 5.

```



### while


Typical use:


```python

 while True:
     # Do stuff...
     if condition:
         break

```


You can add optional ''else'', which is executed only if the expression tested was false. Typically used for searches.


```python

 while True:
     # Do stuff...
     if found:
         results = ...
         break
 else:
    print 'Not found'

```


Since Python has no "bottom-tested" loop construct (such as "do ... until") ... it's common Python practice to either rethink the design in terms of iteration or to use an ''while 1'' (infinite loop) and ''break'' out of it as appropriate.

==[[Raven]]==


### each


 10 each as i                                     # counted loop
     "index is %(i)d\n" print
 
 'hello world' each as c                          # string characters
     "%(c)c" print
 
 [ 'a' 'b' 'c' ] each as item                     # list items
     "%(item)s" print
 
 { 'a' 1 'b' 2 } each pair as k, v                # hash key/val pairs
     "%(k)s => %(v)d\n" print
 
 'SELECT * FROM table' mysql query each as row    # iterable resource 
     row print


### repeat while/until


 repeat <some_condition> while
     <some_process>
 
 repeat <come_condition> until
     <some_process>


## REXX


### repeat

This example shows how to perform a loop for one million times.

```rexx
n= 1000000
x=       1
y=      12
z=       0
              do n
              z=someFunction(z, x, y)
              end   /*n*/
```
 



==[[Seed7]]==
In Seed7 new statements can be declared easily. This feature is not explained here. Here are examples of the predefined loops:


### while


 while TRUE do
   foo;
 end while;


### repeat


 repeat
   foo;
 until FALSE;


### for


 for i range 0 to 4 do
   foo;
 end for;

 for i range 4 downto 0 do
   foo;
 end for;

 for stri range []("foo", "bar", "baz") do
   writeln(stri);
 end for;

==SETL4==

```setl4

    define('prime(n)set.this')                 :(prime.end)

*   Tests if _n_ is a prime integer.

prime

    n = integer(n) +n
    eq(n,2)                                     :s(return)
    even(n)                                     :s(freturn)
    exists(new('iter 3 ' square.root(n) ' 2'), 'multiple(n,this)') :s(freturn)f(return)

prime.end

    define('primes(n)set.this')                :(primes.end)

*   Returns set of primes less than _n_.

primes

    primes = filter(new('iter 2 ' (n - 1)),'prime(this)') :(return)
    primes = new('set')
    iter = new('iter 2 ' (n - 1))
    loop(iter)

primes.loop
   
    this = next(iter)                           :f(return)
    prime(this) add(primes,this)                :(primes.loop)

primes.end

```


==[[SIMPOL]]==
In [[SIMPOL]] there is only one looping construct. It is very flexible and can be used as a while, a repeat, or a combination of both.


### while



```simpol
while [condition]
  // Actions taken here
end while [condition]
```


Either condition or both can be present. Not setting either condition will result in an endless loop. The first condition will continue the loop if the result is true, the second will exit the loop if it is true. In [[SIMPOL]] code it is common to see both present, with the initial condition being the standard method of exiting the loop, and the end while condition used for exiting because of an error.

In this abbreviated example, the code will read records from a database table in a loop, placing each in record object in an array element, until it reaches the end of the table.


```simpol
function readrecs(sbme1table table)
  sbme1record r
  array recs
  integer e, i

  e = 0
  i = 0
  recs =@ array.new()
  r =@ table.select(error=e)
  while r !@= .nul
    i = i + 1
    recs[i] =@ r
    r =@ r.select(error=e)
  end while e != 0
end function recs
```


The final condition should best be read as: "end the while, if this is true".

Another typical use of the loop is the traversing of rings that are commonly used in [[SIMPOL]].


```simpol
function getfieldnames(sbme1table table)
  sbme1field field
  integer i
  array fieldnames

  fieldnames =@ array.new()
  i = 0
  field =@ table.firstfield
  while field !@= .nul
    i = i + 1
    fieldnames[i] = field.name
    field =@ field.next
  end while field =@= table.firstfield
end function fieldnames
```


==[[SNUSP]]==
 ==in==!/==body==?\==out==
        \==ydob===/

The basic loop structure can be modified in many ways. You can have a pre-loop test by including '''?''' before the loop.

A unique feature of SNUSP is the ability to write ''bi-directional'' loops, designed for different effect depending on which direction the flow of control is running around the loop. The two entry points into this subroutine will move a value either up or down one cell, determined by the direction the loop is circulating.
          #    #
 up1====?!/->+<\
          ?    ?
 down1==?!\<+>-/
          #    #


==[[Sparkling]]==

While loop (test-first loop):


```sparkling
var n = -1;
print("Enter 0 to exit");

while n != 0 {
    n = toint(getline());
    print(n);
}
```


For loop (alternate form of test-first loop, idiomatic approach to counting):


```sparkling
var i;
for i = 0; i < 10; i++ {
    print(i);
}
```


Do-while loop (test-last loop):


```sparkling
print("enter empty line to exit");

var s;
do {
    s = getline();
    print(s);
} while s != "";
```


==[[Toka]]==


###  countedLoop 

Counts up or down until the boundaries are met.

 10 0 [ i . ] countedLoop
 0 10 [ i . ] countedLoop


###  whileTrue 

Repeatedly executes a quote until the quote returns true.

 100 [ dup . 1 - dup 0 <> ] whileTrue


###  whileFalse 

Repeatedly executes a quote until the quote returns true.

 0 [ dup . 1 + dup 101 = ] whileFalse

==[[Tern]]==

Tern has several distinct loop statements.


### Infinite Loop



```tern
let v = 0;
loop {
   println(v++);
}
```



### While Loop



```tern
let v = 0;
while(v < 100) {
   println(v++);
}
```



### For Loop



```tern
for(let v = 0; v < 100; v++) {
   println(v);
}
```



### For In Loop



```tern
for(v in 0 to 99) {
   println(v);
}
```

