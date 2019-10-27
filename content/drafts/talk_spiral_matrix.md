+++
title = "Talk:Spiral matrix"
description = ""
date = 2015-09-30T12:46:34Z
aliases = []
[extra]
id = 2968
[taxonomies]
categories = []
tags = []
+++

== Explanation of Python code ==
See [http://paddy3118.blogspot.com/2008/08/spiral.html Spiral]. --[[User:Paddy3118|Paddy3118]] 06:30, 5 August 2008 (UTC)
:At least for the iterative solution. --[[User:Paddy3118|Paddy3118]] 10:48, 5 August 2008 (UTC)

== J ==

Unlike the concise solution to [[Talk:Zig Zag#epilogue|ZigZag]], this [[Spiral#J|J function]] works entirely on a list, until the very end, when it reshapes the 1D list into the 2D array.

So if <tt>SPIRAL</tt>:

    SPIRAL=:spiral 5

is our spiral array, then <tt>,SPIRAL</tt> is the rows catenated together:

    ,SPIRAL
 0 1 2 3 4 15 16 17 18 5 14 23 24 19 6 13 22 21 20 7 12 11 10 9 8


###  insight no. 1 


Nothing about this list pops out at me, but if you read the [http://www.jsoftware.com/papers/play132.htm original article], you'll see a smart J guy (Joey Tuttle) noticed that this list looks like the ''grade'' of another list.


###  aside:  grading 


As I said in my [[Talk:Zig Zag#reading the J examples|earlier exposition]], a grade represents the ''permutation vector'' required to sort the list.

Let's say in some non-J language, you had a list of chars:

   ['E','C','D','F','A','B' ]

Well, if you take that list, and make it a 2D array where the chars are the first column, and the index is the second column:

   [ ['E',0],
     ['C',1],
     ['D',2],
     ['F',3],
     ['A',4],
     ['B',5] ]

And then sort that 2D array (either by the first column, or by both, which is equivalent), you'd get this:

  [ ['A',4],
    ['B',5],
    ['C',1],
    ['D',2],
    ['E',0],
    ['F',3] ]

Pulling out the second column from the matrix (the integers), you'd get this:

   [4,5,1,2,0,3]

Which is the grade.  

Well, in J, you don't use sort to get the grade, you use grade to get the sort.  For example, get the grade:

    /:'ECDFAB'
 4 5 1 2 0 3

then use that grade to index into the array:

    4 5 1 2 0 3 { 'ECDFAB'
 ABCDEF

(in J indexing is a function, not part of the syntax.  So, for example, <tt>x[4]</tt> in C becomes <tt>4{x</tt> in J).


###  chasing Joey 


Ok, back to the spiral.  When we left off, Joey had begun to suspect that <tt>,SPIRAL</tt> was actually a permutation vector.  That is, that <tt>,SPIRAL</tt> was the result of grading some other list. More formally, he suspected:

   (,SPIRAL) = (/: something_else)

But how to find <tt>something_else</tt>?  Well, it may not be obvious, but <tt>/:</tt> is self-inverse.  That is, if you apply it twice, it "undoes" itself. 
     /: 4 5 1 2 0 3
 4 2 3 5 0 1
    
    /: /: 4 5 1 2 0 3
 4 5 1 2 0 3
    
    4 5 1 2 0 3 = (/: /: 4 5 1 2 0 3)
 1 1 1 1 1 1
So, since we know 
    something_else = (/: /: something_else)
and  
    (,SPIRAL) = (/: something_else)
then, by substitution, we know 
    something_else = (/: , SPIRAL)
If you're still iffy on function inverse, don't worry, we'll return to it later, and find out a more formal way to ask J to invert a function for us.

In any case, let's see what Joey saw:
    /: , SPIRAL
 0 1 2 3 4 9 14 19 24 23 22 21 20 15 10 5 6 7 8 13 18 17 16 11 12


###  insight no. 2 


I don't see obvious patterns when I look at the result of <tt>/: , SPIRAL</tt> above.  But then, I'm not as smart as Joey.  When he looked at it, he had a second insight:  this array looks like a ''running-sum''.

Here's what I mean by running-sum:

    +/\ 1 2 3 4     NB. running sum
 1 3 6 10
   
    (1),(1+2),(1+2+3),(1+2+3+4)
 1 3 6 10

Formally, Joey suspected:
  (/: , SPIRAL) = (+/\ another_thing)
But obviously <tt>+/\</tt> is not self-inverse.  So how can we discover <tt>another_thing</tt>?  

<b>''We ask J to do it for us''</b>.


###  aside: calculus of functions 


J provides for a calculus of functions.  What is a "calculus of functions"?  Well, just like functions act on data to produce data, we can have meta-functions that operate on functions to produce functions.

Think back to math notation <small><sup>[[#notes|1]]</sup></small>:

   log <b>x</b>   # (1) log of x
   log <b>x</b><sup>2</sup>  # (2) log of x * x
   log<sup>2</sup> <b>x</b>  # (3) log of log of x

Read the (3) again.  How is it different from (2)?  In (2), <tt><b>x</b></tt> has been manipulated, but in (3) ''<tt>log</tt>'' has been manipulated!

That is, in (2), there are two instances of <tt><b>x</b></tt> (in <tt><b>x</b>*<b>x</b></tt>), but in (3) there are two instances of <tt>log</tt> (in <tt>log(log(<b>x</b>))</tt>).

Analogously in J:
   log =: 10&^.  NB.  "^." is the log function in J
   x   =: 100
   log x         NB. (1) log of x
 2
   log x^2       NB. (2) log of x*x
 4
   log^:2 x      NB. (3) log of log of x
 0.30103
Now let's take this one step further.  You'll remember the notation 
   <b>x</b><sup>-1</sup>  # (4)
meant <tt>1/x</tt>, that is, the inverse of the number <tt>x</tt>.  But what did 
   log<sup>-1</sup> # (5)
mean?  By analogy to (4), (5) meant ''the inverse of the log function''.

Analogously in J:

    x^_1                   NB.  (4)
 0.01
    log x   
 2
    log_inverse =: log^:_1 NB.  (5)
 
    log_inverse log x  
 100

Neat, huh?  

=== caught him! ===

So what does all this have to do with our hero, Joey?  Well, if you remember, he had the insight that <tt>(/: , SPIRAL) =(+/\ another_thing)</tt>.  But he was stumped: <tt>+/\</tt> is not self-inverse, so how could he recover <tt>another_thing</tt>?

Well, now we know how he proceeded:  <tt>+/\^:_1</tt>.  Let's follow him:

    +/\^:_1 /: ,SPIRAL
 0 1 1 1 1 5 5 5 5 _1 _1 _1 _1 _5 _5 _5 1 1 1 5 5 _1 _1 _5 1

<b>Eureka</b>!  This list clearly shows a pattern.  We can simply replicate the pattern, then undo (invert) the operations that generated it from <tt>SPIRAL</tt>, and we'll have our <tt>SPIRAL</tt> back.  


###  home again 


Put another way: if we can generate these ones-and-fives, we can generate <tt>SPIRAL</tt>.  How?  Well, since we generated the ones-and-fives from <tt>SPIRAL</tt> using:

    ones_and_fives =. +/\^:_1 /: , SPIRAL

Then we need to do the opposite of that to recover <tt>SPIRAL</tt>.  To break this procedure down, we have:
  
    func3 =. +/\^:_1
    func2 =. /:
    func1 =. ,
 
    ones_and_fives =. func3 func2 func1 SPIRAL

Obviously if you're doing the "opposite" of something you proceed LIFO.  That is, given <tt><b>y</b> = func3(func2(func1(<b>x</b>)))</tt>, then <tt><b>x</b> =  func1<sup>-1</sup>(func2<sup>-1</sup>func3<sup>-1</sup>(<b>y</b>)))</tt> <sup><small>[[#notes|2]]</small></sup>.

That is:

    SPIRAL =. func1^:_1 func2^:_1 func3^:_1 ones_and_fives

and we know:

# The inverse of an inverse is the thing itself, so <tt>func3^:_1</tt> is merely <tt>+/\</tt>.  
#  The function <tt>/:</tt> is self-inverse, so <tt>func2^:_1</tt> is merely <tt>/:</tt> (though <tt>/:^:_1</tt> would work as well).  
# The function <tt>,</tt> (''ravel'') is not invertible: it loses information (it is possible that <tt>(,x)=(,y)</tt> is true but <tt>x=y</tt> is not). But that's ok, we have the information that it lost: it's the original shape of <tt>SPIRAL</tt>, which is the input to our function!  So we can undo the ravel.


###  conclusion 


Putting that all together, we conclude:

    SPIRAL =. (input) reshape /: +/\ ones_and_fives 

In English:

:''Joey discovered a very simple pattern underlying the spiral arrays.  The spiral itself is merely the grade of the running-sum of this pattern, reshaped into a matrix.''

Generating the underlying pattern (<tt>ones_and_fives</tt>) is left as an exercise for the reader.  But, if you get stuck, it's spelled out in [http://www.jsoftware.com/papers/play132.htm the original article].


###  notes 

# For simplicity, here <tt>log</tt> means log-base-10, or <tt>log<sub>10</sub></tt>.
# In fact, LIFO of inverses is exactly how J inverts  composite functions.

[[User:DanBron|DanBron]] 19:40, 5 August 2008 (UTC)

== original J exposition ==
The [[Spiral#J|J solution]] was:


```txt

   spiral =. ,~ $ [: /: }.@(2 # >:@i.@-) +/\@# <:@+: $ (, -)@(1&,)

```


Here are some hints that will allow you to reimplement it in your language:


```txt

   counts   =:  }.@(2 # >:@i.@-)
   counts 5
5 4 4 3 3 2 2 1 1
   
   values   =:  <:@:+: $ (, -)@(1&,)
   values 5
1 5 _1 _5 1 5 _1 _5 1
   
   copy     =:  #
   3 copy 9
9 9 9
   
   (counts copy values) 5
1 1 1 1 1 5 5 5 5 _1 _1 _1 _1 _5 _5 _5 1 1 1 5 5 _1 _1 _5 1
   
   sumscan  =:  +/\       NB.  Cumulative sum
   sumscan 0 1 2 3 4
0 1 3 6 10
   
   (counts sumscan@copy values) 5
1 2 3 4 5 10 15 20 25 24 23 22 21 16 11 6 7 8 9 14 19 18 17 12 13
   
   grade    =:  /:  NB.  Permutation which tells us how to sort
   grade 5 2 3 1 0 4
4 3 1 2 5 0
   
   (counts grade@sumscan@copy values) 5
0 1 2 3 4 15 16 17 18 5 14 23 24 19 6 13 22 21 20 7 12 11 10 9 8
   
   dup      =:  ,~
   dup 5
5 5
   
   reshape  =:  $   NB. Reshape an array
   3 4 reshape 'hello'
hell
ohel
lohe
   
   (dup reshape counts grade@sumscan@copy values) 5
 0  1  2  3 4
15 16 17 18 5
14 23 24 19 6
13 22 21 20 7
12 11 10  9 8

```


For a fuller explanation, see [http://www.jsoftware.com/papers/play132.htm the original source].

: Yet another J solution that looks both interesting and impenetrable to me. at least for [[Zig Zag]] a Haskel person had reimplemented the J solution and left me the clue that it involved a sort :-) 
:--[[User:Paddy3118|Paddy3118]] 16:56, 5 August 2008 (UTC)
::This one includes a sort, too :)  That's one of the neatest parts!  Alright, let me write out the algo real quick (I'll gloss over details and may fib a bit, to get the idea across quickly).


== Python array initialisation edit problem ==
Hi Spoon, your edit to Spiral of substituting <python>array = [[None]*n]*n</python> for <python>array = [[None]*n for j in range(n)]</python> [[http://www.rosettacode.org/w/index.php?title=Spiral&curid=2967&diff=16906&oldid=16746 here]] doesn't work because of: 

```python>>>
 n=2
>>> a = [[None]*n]*n
>>> a
[[None, None], [None, None]]
>>> a[0][0] = 1
>>> a
[[1, None], [1, None]]
>>>
```

You are referencing the inner list multiple times instead of creating new copies. --[[User:Paddy3118|Paddy3118]] 06:17, 14 August 2008 (UTC)
:Oh yeah, sorry. You're right. --[[User:Spoon!|Spoon!]] 18:58, 14 August 2008 (UTC)

==Lua spiralling error?==
Task says:
  "where the numbers increase sequentially as you go around the edges of the array spiralling inwards"
Lua says:
  "returns the value at (x, y) in a spiral that starts at 1 and goes outwards"

I haven't run the program to see its output, but could someone check that the spiral is as in the task description? Thanks. --[[User:Paddy3118|Paddy3118]] 08:24, 29 January 2010 (UTC)

: Lua output is

```txt

14   15   16   17   18   19   20   21   
13   38   39   40   41   42   43   22   
12   37   54   55   56   57   44   23   
11   36   53   62   63   58   45   24   
10   35   52   61   60   59   46   25   
9   34   51   50   49   48   47   26   
8   33   32   31   30   29   28   27   
7   6   5   4   3   2   1   0
```


: which seems correct (even though starting from lower-rightmost corner). &mdash; [[User:ShinTakezou|ShinTakezou]] 10:35, 21 May 2010 (UTC)

== Spiral Matrix implementation in VBA ==

hii
i came across this page during a search for a mechanism to implement the spiral matrix in VBA bt unfortunately it was not listed here. But afrterwards by translating the java code i manage to obtain the result. I beleive a lot f peopla have manage to do this but didnt come across anyone who has shared it
so here it goes....

:Howdy, and welcome to RC (if appropriate). Might I suggest creating an account?
:Having said that, code submissions really should go on [[Spiral matrix|the task's page]], and not its talk page. I've moved it for you: [[Spiral matrix#VBA]]. I made some changes so that it is more generalized; your use of <code>Init</code> (which I assume sets up certain starting conditions) doesn't help anyone without access to your code.
:(As a minor aside, I have to wonder at your use of cells way down in the 400+ range. For something like this, I'd think that just starting at A1 would be acceptable.)
:Not trying to be a jerk, just trying to help. -- [[User:Eriksiers|Erik Siers]] 18:28, 21 September 2010 (UTC)

==Really long line in C#==
It doesn't look like best coding practice to have that really long line. Wouldn't it be better with some newlines and spacing to better show its structure and cut the line length? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:58, 30 September 2015 (UTC)

: I wrapped it. Hopefully that makes it ok... --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 12:28, 30 September 2015 (UTC)

:: It's a lot better, thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:46, 30 September 2015 (UTC)
