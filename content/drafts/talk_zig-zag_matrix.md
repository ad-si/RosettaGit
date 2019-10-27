+++
title = "Talk:Zig-zag matrix"
description = ""
date = 2016-09-02T10:12:36Z
aliases = []
[extra]
id = 2963
[taxonomies]
categories = []
tags = []
+++

== reading the J examples ==


I noticed this in the Haskell example:
 Computing the array: 
 ...
 -- (The author of this example thinks the algorithm could be better, but <b>can't read the J examples.</b>)

I can spell out the J examples in more detail.  I'm not sure that it will help much, because J has a rich set of primitives (built in operations), so recreating the example in another language will entail recreating the primitives, too.  So you might not save much work.  


###  exposition 


None the less, let's start with the solution which is probably easiest to rewrite in another language.  

    zz4 =: $ [: /:@:; ] (+/@#: <@(A.~_2|#)/. ]) [: i. */


###  list of integers 


To break this down, the first major construct is:

    integers =: [: i. */

This simply produces the first N non-negative integers, where N is the product of its argument.  For example, if we want a 3x3 matrix, then:

    integers 3 3
 0 1 2 3 4 5 6 7 8

So  <tt>integers</tt> is basically the input vector we want to rearrange into the zigzag matrix.

=== anti-diagonals ===

The next major part of the function is 

    antid =: +/@#: <@(A.~_2|#)/. ]

which generates the anti-diagonals (and reverses them if appropriate).  This itself is composed of 3 parts, <tt>+/@#:</tt>, <tt><@(A.~_2|#)/.</tt>, and <tt>]</tt>.  The first of these, <tt>+/@#:</tt> gives the sum of the coordinates of each element of the matrix.  Let's ignore the details of why for now, and just look at an example.

If we have a 3 by 3 matrix, like this:

   i. 3 3
 0 1 2
 3 4 5
 6 7 8

Then these are the coordinates of each number in that matrix:

    3 3 <@#: i. 3 3
 +---+---+---+
 |0 0|0 1|0 2|
 +---+---+---+
 |1 0|1 1|1 2|
 +---+---+---+
 |2 0|2 1|2 2|
 +---+---+---+

And these are the sums of those coordinates:

   3 3 +/@#: i. 3 3
 0 1 2
 1 2 3
 2 3 4

Notice a regularity there?  Look at the antidiagonals:

    </. 3 3 +/@#: i. 3 3
 +-+---+-----+---+-+
 |0|1 1|2 2 2|3 3|4|
 +-+---+-----+---+-+

Along each antidiagonal, the sum of the coordinates is constant (this isn't surprising, but it may not be obvious).  We will use this observation in a moment.


### = classification =


For now, let's get back to <tt>antid</tt>.  The second component of that function is <tt><@(A.~_2|#)/.</tt> .  The most important feature of that component is that it ''classifies'' its input.  That is, it breaks it into buckets.  Given a list of buckets on the left, and data on the right, for each item of the data (on the right), it will look at the corresponding bucket number (on the left) and put the data-item into that bucket. An example:

    NB.  Classify by even and odd
    0 1 0 1 0 1 </. 0 1 2 3 4 5
 +-----+-----+
 |0 2 4|1 3 5|
 +-----+-----+
 
    NB.  Classify by vowel and consonant
    0 1 0 0 1 0 0 1 0 0 0 </. 'hello world' 
 +--------+---+
 |hll wrld|eoo|
 +--------+---+
      
    NB.  Classify by sign (positive, negative, or zero)
    2 1 0 1 1 2 0 0 2 2 1 0 2 2 </. 1 _2 0 _3 _4 5 0 0 6 7 _8 0 9 10
 +------------+-----------+-------+
 |1 5 6 7 9 10|_2 _3 _4 _8|0 0 0 0|
 +------------+-----------+-------+

In this case, we're going to classify <tt>integers</tt> by the sum-of-the-coordinates, calculated earlier.  As we observed, that sum is constant along the antidiagonals of the matrix, so we're effectively putting each antidiagonal into its own bucket.  

    input_list         =.  integers 3 3
    sum_of_coordinates =.  3 3 +/@#: input_list
  
    sum_of_coordinates </. input_list
 +-+---+-----+---+-+
 |0|1 3|2 4 6|5 7|8|
 +-+---+-----+---+-+


### = conditional reversal =


The final transformation <tt>antid</tt> applies is that it reverses each antidiagonal as appropriate.  Basically the primitive <tt>A.</tt> gets an integer on the left, and a list on the right.  The integer is an index into the table of all possible permutations of the data.  For example, there are only 6 ways to permute a list of 3 items:

    (A.~ i.@:!@:#) 'ABC'
 ABC
 ACB
 BAC
 BCA
 CAB
 CBA

Whereas there are 24 ways to permute a list of 4 items, 120 for a list of 5, etc.  Another thing to notice is that the table of permutations is in lexographic order.  That is, it's sorted.  In particular, the identity permutation is always first, and the permutation that reverses the data is always last (see the first and last rows of the example output, above).  So given the index -1, <tt>A.</tt> will reverse the data, and given the index 0, it will leave the data alone.  Example:

    _1 A. 'ABC'
 CBA
     0 A. 'ABC'
 ABC

We can use this fact to reverse a list based on a condition.  We want to reverse every other antidiagonal, which is the same as saying we want to reverse the odd antidiagonals.  We observe that odd antidiagonals have odd lengths, and even antidiagonals have even lengths.  Which is to say, the length of the antidiagonal, mod 2, tells us whether we want to reverse that antidiagonal or not.  Combining this observation with the property of <tt>A.</tt> highlighted above, we simply take the ''negation of its length, mod 2'' as the index into its permutation table.  If the antidiagonal is odd (every other antidiagonal), it is reversed, otherwise left alone (it is worth noting that J treats negative arguments to modulous consistently, so that we can say <tt>_2 | x</tt> to get 0 or _1, rather than having negate-the-mod in two steps).

Therefore <tt>antid</tt> classifies the input into its antidiagonals, reversed appropriately.  OK, almost home.


###  reordering 


So now we have:

    zz4 =: ($ [: /:@:; ] antid integers)

The  <tt>] antid integers</tt> generates the input list and uses that to calculate the antidiagonals, as we have seen:

    (] antid integers) 3 3 
 +-+---+-----+---+-+
 |0|1 3|6 4 2|5 7|8|
 +-+---+-----+---+-+

The penultimate part,

    reorder =: /:@:;

returns the antidiagonals to a list of integers, and reorders it.  Example:

    ; (] antid integers) 3 3
 0 1 3 6 4 2 5 7 8

Turning a list-of-lists into a flat list isn't interesting, but the reordering is.  We now have a list of integers in the order they'd appear if you walked along the antidiagonals.  But no one walks along a matrix's antidiagonals -- we walk along the rows, left-to-right, top-to-bottom.  Here's how the list would look if you tried to walk that way along them, without reordering first:

   3 3 $ 0 1 3 6 4 2 5 7 8 
 0 1 3
 6 4 2
 5 7 8

Which doesn't make sense, because we've ordered them one way, but we're trying to read them another way.  What we want to do is read them in order.  We want to visit 0 first, 1 second, 2 third, etc.  That is, we want to know ''what we'd have to do to sort the array''.  More precisely, we want to know how to permute the array to make it sorted.

Many languages have built-in sort functions, and J is no different in that regard.  It is, however, different in how far it generalizes the concept.  In J, more than just sorting an array, you can (must) ''grade'' it.  That is, "this comes first, this comes second, this comes third".  That is the function of the primitive <tt>/:</tt> (read "grade down", and its counterpart, <tt>\:</tt> "grade up").

For example, if we grade a list, it is like grading a school class; it tells us who's first, who's second, etc:
 
    /: 'ECDFAB' 
 4 5 1 2 0 3

Then, if we use those numbers to index into the list:

    4 5 1 2 0 3 { 'ECDFAB' 
 ABCDEF

Voila, the list is sorted (we could've done this in one step, with <tt>/:~'ECDFAB'</tt>).  So, taking our anti-diagonals:

    AD =: (] antid integers) 3 3 
    AD
 +-+---+-----+---+-+
 |0|1 3|6 4 2|5 7|8|
 +-+---+-----+---+-+

We want to know what we'd have to do to walk along them in row order (that is: "first go to 0 (in the first position), then step to one (in the second position), then jump all the way to 2 (which is in the sixth position), then jump BACK to 3 (which is in the third position), then to 4 (in the fifth position), ...":

    reorder AD
 0 1 5 2 4 6 3 7 8

So now, if we read these in row-order we see:

    3 3 $ reorder AD
 0 1 5
 2 4 6
 3 7 8

Which is exactly what we wanted.


###  reshaping 


Incidentally, the <tt>$</tt> in the expression above serves exactly the same purpose as it does in the original expression: it ''reshapes'' the list into a matrix.  

    reshape =: $


###  succinctness is power 


Putting all the above together, we can read the original expression as:

    zz4 =: reshape [: reorder ] antid integers

Which is exactly what it does.

So now you can see why "J has no words".  It took me over '''2000 <u>words</u>''' to describe what the J expression did in less than '''40 <u>characters</u>''', and I left out much detail (both logical and technical; for example, all the edge cases are handled transparently).  That is a very good compression ratio; we get a lot for a little.  If you were as fluent in J as you are in English, you could've read and understood the J in maybe a tenth the time it took you to read this exposition.

Didn't Paul Graham say "[http://www.jsoftware.com/jwiki/Essays/Notation_as_a_Tool_of_Thought succinctness is power]"?


###  epilogue 


Incidentally, I chose the expression <tt>zz4</tt> not because it is the best, but because I thought it would be most easily translated to another language (given an exposition).  In fact, you'll notice that except for the last transformation (<tt>reshape</tt>) the entire function works on a list of numbers.

But there is no reason this should be so; J is a master of multidimensional arrays, and there is no reason would should constrain the function by making it work on a one-dimensional list.  For example, there is no need for the defined function <tt>integers</tt>; the built in primitve <tt>i.</tt> will more than suffice:

    i. 3 3
 0 1 2
 3 4 5
 6 7 8
 
And there is no reason to calculate the sum-of-the-coordinates; we can access the anti-diagonals directly:
   
   </. i. 3 3
 +-+---+-----+---+-+
 |0|1 3|2 4 6|5 7|8|
 +-+---+-----+---+-+
 
And there's no reason to muck about with permutation tables or modulous, J can loop over a list of functions just as easily as other languages can loop over lists of data:

   <@|.`</. i. 3 3     NB.  Reverse or don't, cyclically.
 +-+---+-----+---+-+
 |0|1 3|6 4 2|5 7|8|
 +-+---+-----+---+-+

(I did say J has a rich set of primitives.) 

So, in fact, if we stop working against the grain of J and start out with a multi-dimensional array in the first place, the expression becomes much shorter and sweeter:
 
    zz4 =: $ [: /:@:; ] (+/@#: <@(A.~_2|#)/. ]) [: i. */
 
    zz0 =: $ [: /:@; [: <@|.`</. i.
 
    zz4 3 3
 0 1 5
 2 4 6
 3 7 8
   
    zz0 3 3
 0 1 5
 2 4 6
 3 7 8
   
And that's why I like J.

== further J exposition ==

One thing that may not be clear about the J code, and therefore might render it confusing, is that is has no variables.  It is completely functional, and the arguments are implicit (the arragement of the expression determines how and where inputs and results are passed).

A verbose version of this functional expression might be written:

```txt

   zigZag            =:  rearrange (antidiagonals integerList)
     rearrange       =:  reshape reorder
       reshape       =:  $
       reorder       =:  grade@flatten
         grade       =:  /:
         flatten     =:  ;
     antidiagonals   =:  sumOfCoords rClass identity
       sumOfCoords   =:  sum@antibase
         sum         =:  +/
         antibase    =:  #:    
       rClass        =:  <@maybeRevers classify
         maybeRevers =:  permute~ evenOrOdd
           permute   =:  A.
           evenOrOdd =:  _2 mod len
             mod     =:  |
             len     =:  #
         classify    =:  /.
       identity      =:  ]  NB.  Copy of argument
     integerList     =:  zeroToN_1@product
       zeroToN_1     =:  i.
       product       =:  */  NB.  Note resemblance to sum ('/' is zipWith)

```


To explicitly recreate the function calls that occur when the function is invoked (with assignments being what the interpreter actually does, and everything else just to show you what's going on):

```txt

    INPUT       =: 3 3

    PRODUCT     =: product INPUT
    PRODUCT
 9
    INTEGERLIST =: zeroToN_1 PRODUCT
    INTEGERLIST 
 0 1 2 3 4 5 6 7 8

    INPUT <@antibase INTEGERLIST
 +---+---+---+---+---+---+---+---+---+
 |0 0|0 1|0 2|1 0|1 1|1 2|2 0|2 1|2 2|
 +---+---+---+---+---+---+---+---+---+
    INPUT sum@antibase INTEGERLIST
 0 1 2 1 2 3 2 3 4
    SUMOFCOORDS=:INPUT sumOfCoords INTEGERLIST 
    SUMOFCOORDS
 0 1 2 1 2 3 2 3 4

    SUMOFCOORDS <classify INTEGERLIST   NB.  Anti-diagonals
 +-+---+-----+---+-+
 |0|1 3|2 4 6|5 7|8|
 +-+---+-----+---+-+
    SUMOFCOORDS evenOrOdd classify INTEGERLIST   NB.  Anti-diagonal lengths alternate, even and odd
 _1 0 _1 0 _1
    0 permute 1 2 3  NB.  0 permute X is just X
 1 2 3
    _1 permute 1 2 3 NB. _1 permute X is X reversed
 3 2 1
    maybeRevers 1 2 3 NB.  Odd lengths reversed
 3 2 1
    maybeRevers 1 2 3 4 NB.  Even lengths left alone
 1 2 3 4

    ANTID =: SUMOFCOORDS rClass INTEGERLIST  
    ANTID   NB. Anti-diagonals, appropriately reversed
 +-+---+-----+---+-+
 |0|1 3|6 4 2|5 7|8|
 +-+---+-----+---+-+

    flatten ANTID    NB.  Not exciting
 0 1 3 6 4 2 5 7 8
    grade FLAT_ANTID NB.  How to permute, in order to sort
 0 1 5 2 4 6 3 7 8
    REORDERED=:reorder ANTID
    REORDERED
 0 1 5 2 4 6 3 7 8

    INPUT reshape REORDERED NB.  Make it a matrix
 0 1 5
 2 4 6
 3 7 8

    ZIGZAG=:INPUT reshape REORDERED   NB.  Done
    ZIGZAG
 0 1 5
 2 4 6
 3 7 8  

```


So, to transliterate this to another (inconsistent) pseudo-language:

```txt


function zigZag(int-list INPUT) as matrix
{
   return rearrange(INPUT, antidiagonals(INPUT,integerList(INPUT))
}

function rearrange(int-list SHAPE, int-list-list ANTIDIAGONALS) as matrix
{
   return reshape(SHAPE,reorder(ANTIDIAGONALS))
}

function reorder(int-list-list ANTIDIAGONALS) as int-list
{
   RESULTS = []
   for each ANTIDIAGONAL in ANTIDIAGONALS
   {
      RESULTS.append(ANTIDIAGONAL)  //  Flatten the list-list to a list
   }

   return grade(RESULTS)
}

function antidiagonals(int-list INPUT, int-list INTEGERS) as int-list-list
{ 
   // Note classify has a function-argument
  CLASSES = classify(sumOfCoords(INPUT, INTEGERS), INTEGERS)
  RESULTS = []
  for each CLASS in CLASSES
  {
    RESULTS.append(maybeReverse(CLASS))
  }

  return RESULTS
}

function sumOfCoords(int-list INPUT, int-list INTEGERS) as int-list
{
   COORDINATES = antibase(INPUT, INTEGERS)
   RESULTS = []
   for each COORDINATE in COORDINATES
   {  
     RESULTS.append(sum(COORDINATE))
   }
   return RESULTS
}

function maybeReverse(int-list LIST) as int-list
{
  if ( len(LIST) mod 2 ) 
  {
     return reverse(LIST)  // FIXME: Fully implement A. (permutation table)
  }
  else
  {
     return LIST
  }
}

function integerList(int-list INPUT) as int-list
{
    PROD = product(INPUT)
    return [0 .. (PROD-1)]
}

```


The uninmplemented functions (e.g. <tt>antibase</tt>) are primitives, or simple combinations of primitives, in J.  You can get the formal specifications at [http://www.jsoftware.com/help/dictionary/vocabul.htm The J Vocabulary].  For example, see the [http://www.jsoftware.com/help/dictionary/d402.htm definition of antibase].


==Reply ==

Thanks for the info! I've updated the Haskell example with a better algorithm based on your techniques. (By the way, I think this explanation is worth keeping around; perhaps it should be moved to something like [[Zig Zag/J explanation]] and linked from the example?) --[[User:Kevin Reid|Kevin Reid]] 17:49, 4 August 2008 (UTC)

:I took the fact that you sorted in the Haskel example and worked it out for myself for the Python example, before thinking of reading this talk page. So I have my own [http://paddy3118.blogspot.com/2008/08/zig-zag.html explanation]. --[[User:Paddy3118|Paddy3118]] 04:12, 5 August 2008 (UTC)

Even though J looks so strange and hard to me, this algo so explained and encoded gave me the idea that J deserves a look and more:) --[[User:ShinTakezou|ShinTakezou]] 01:00, 16 December 2008 (UTC)

== Error in Pascal example ==

Pascal example works only for odd matrix dimensions.

--[[User:Blentabler|Blentabler]]

Now fixed --[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 10:11, 2 September 2016 (UTC)
