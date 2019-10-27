+++
title = "Array"
description = ""
date = 2017-06-18T07:02:35Z
aliases = []
[extra]
id = 1561
[taxonomies]
categories = []
tags = []
+++

An '''array''' is a composite data type -- it can store multiple values, and so is in the [[Collections|collection]] category. The stored values are called '''elements''' of the array, and are accessed by a sequence of indices. In a program, indices may be a mixture of constant values or variables that allow the elements accessed to vary under program control. The indices of an array are [http://en.wikipedia.org/wiki/Total_order totally ordered].

The implementation details of an array give rise to an important distinction between '''arrays''' and '''associative arrays'''.

:The implementation of '''arrays''' is based on setting the bounds of indices of the array, the ''size'' of the array, normally by allocating a contiguous region of memory to hold the elements of the array, and using simple offset calculations on the indices from the origin of the memory to access memory elements. Some languages support extensions to allow such arrays to be resized, or re-shaped, in which the memory area is adjusted, but extent elements are retained.

:By contrast, an '''[[associative array]]''' maps the association between index "keys" and their associated values, generally using more complex [http://en.wikipedia.org/wiki/Hash_function hash functions] on the keys of the array to map them to their corresponding elements (by pointers, references or memory addresses of some sort).  Associative arrays are referred to variously as "hashes" ([[Perl]]), "maps" or "mappings" ([[Lua]]), or "dictionaries" ([[Python]]), as well as "associative arrays" ([[AWK]], [[ksh]], and others).  The keys into associative arrays are normally not constrained to be integers, unlike arrays, which generally required contiguous integer ranges.  Different languages may impose various constraints on these keys.  For example, in [[Perl]], keys must always be strings, so 1, "1", and 1.0, each of which stringifies to "1", are the same key, but "1.0" is distinct from all of these. In [[PHP]], keys must be strings or integers; floats and booleans get implicitly converted to an integer. Other languages (such as Python) may treat each type of object as distinct. (See [[associative array]] for further discussion.)

:Non-associative arrays may have speed and memory consumption advantages. Associative arrays have greater flexibility in types used for keys, and generally obviate the need to implement searches through the collection. (Each component on which one would search can be implemented as a different associative array of references to their corresponding values or records.)

Arrays with more than one index are called '''multidimensional''' arrays. For example, a matrix is a two-dimensional array.

Some languages (such as [[AWK]]) do not support true arrays; they merely emulate them through their associative arrays.  Similarly, some languages emulate multi-dimensional arrays by concatenation of dimensional indices into keys (perhaps a peculiarity of [[AWK]]).

Common operations defined on arrays include:

* Indexing: accessing an array element by its indices. (There is a one to one mapping between an index and its corresponding element).  While the cost of indexing is O(1) on typical current machines, the cost of indexing an element adjacent to a recently accessed element is much faster than indexing an arbitrary random element of a large array.
* Slicing: producing a subarray by putting some constraint on the indices. For example, [[PL/1]] provides extracting of a row or a column of an array. In [[Ada]] any range of the index can be used in order to extract a subarray from a single-dimensional array.  In [[Python]] slices can extract any contiguous subset of an array and extended slice notation can extract elements in reversed order and/or by traversing in a given "stride" --- for example ''a[100:0:-2]'' would return every odd element from 100 to the beginning of the list: a[99], a[97], ... a[1].
* Iteration over the array's elements. Some languages make this a universal or implicit operation, some languages have a [[Loop/Foreach|foreach loop]] construct for array iteration, in others this must be done with conventional looping and arithmetic.
* Iteration over the indices of an associative or sparse array.
* Querying the bounds of array indices (determining the maximum element index of offset)
* Querying the indices of an associative array (determining if the collection contains a value for any given key).
* Operations on indices (next, previous, range etc)
* Sorting the contents of the array to satisfy some relationship.
* Searching an array for the location(s) of some value(s)

Array programming languages provide operations applied to entire arrays, so programs in such languages often do not bother using index references (for example [[APL]]).

Multidimensional arrays in which the valid range of one index depends on the value of another are called '''ragged''' (also '''jagged'''). This term comes from a typical example of a ragged array, when a two-dimensional array is used to store strings of different length in its rows. When put on paper the right margin of the output become ''ragged''.

The lower bound of non-associative arrays in many [[:Category:Programming Languages|programming languages]] is commonly fixed at either 0 ([[C]] and relatives) or 1 (Old [[Fortran]] and relatives); or an arbitrary integer ([[Pascal]] and relatives, modern Fortran). In [[Ada]] any discrete type can used as an index.  Zero-based indexing is best thought of in terms of the index being an offset from the beginning of the array.  Thus the first element is located zero elements from this starting point.  The alternative can be thought of as ordinal indexes referring to the first, second, ... and ''n''th elements of the array.

In most programming languages, arrays are accessed by using the array brackets <tt>[</tt> and <tt>]</tt>, e.g. in <tt>A[i]</tt>. However, exceptions exist, including [[REXX]] which instead uses the dot operator <tt>.</tt>, such as in <tt>A.i</tt>; [[Fortran]], [[Ada]] and [[BASIC]] which use round parentheses <tt>A(i)</tt>, and in [[LISP|lisp]] dialects which use constructions like <tt>(ELT A n)</tt> for accessing and <tt>(SETA A n new_val)</tt> for setting (Interlisp) or <tt>(vector-ref A n)</tt> for accessing and <tt>(vector-set! A n new_val)</tt> for setting (Scheme). No bracket indexing occurs in [[J]], an array language; instead, the normal syntax of function creation and function calling applies.

{{Template:See also lists}}

==Computational metrics==
Access is O(1), appending is O(1), and insertion is O(n) for a single item.

==Examples==
* [[letter frequency]]

===[[Fortran]]===
Arrays have been available from the start, for each of the allowed standard types of variables: integer, real, complex, logical, and character, and their different precisions. They can be indexed only with integer values, and definitely not with text strings as in say Snobol - though one could place a short text into a small CHARACTER variable that is equivalenced to an INTEGER variable and use that integer as in a "hash table" scheme, possibly to good effect. Indexing starts with one, and the size is fixed by the declaration at compile time, thus 
```Fortran
      REAL A(66)    !Declares an array, elements one to sixty-six.
      A(3) = 1.1    !Assigns a value to the third element of A.
      A(2) = 1.1
      A(1) = 1.1
```

Higher dimensonality is available, with additional indices, thus given the declaration <code>INTEGER B(12,9)</code> individual elements would be accessed via the likes of <code>B(12,6) = 7</code> The type includes the dimensionality so array B can only be referenced with two indices and A only with one, although it is possible to specify something like <code>EQUIVALENCE (A,B)</code> which means that the two names refer to the same storage; the arrays are overlaid, even if of different types. With careful organisation and a good plan, this can have positive benefits. Multi-dimensional arrays are stored in "column-major" order, which is to say that for consecutive elements in storage, the ''left''most subscript varies most rapidly, so the order would be B(1,1), B(2,1), B(3,1), ... B(12,1), B(1,2), B(2,2), ''etc''. This is important when explicit indexing is not used, as when only its name is given in a READ, WRITE, or DATA statement: values are processed in storage order. Thus, element B(12,6) is the sixth element of row twelve, and the next along in storage would be B(1,7), which is not the seventh element of row twelve.

There is no "array subscript" type or usage, whereby, given <code>INTEGER XLAT(2)</code> element B(i,j) might be accessed via B(XLAT) where XLAT(1) = i and XLAT(2) = j. One must use <code>B(XLAT(1),XLAT(2))</code>

An array is identical to a function of integer arguments, so in <code>result = 3*F(X) + 7</code>, the F might be a function (of one parameter, an integer), or, it might be an array of one dimension - unlike with Pascal where F[...] is definitely an array, and F(...) is definitely a function. If a declaration such as <code>COMPLEX F(33)</code> to make it an array does ''not'' appear in that program unit, F will be taken as being a function, and the declaration <code>COMPLEX F</code> should appear since otherwise F would be REAL, not COMPLEX because undeclared names have a type of INTEGER if they start with the letters I, J, K, L, M, or N, otherwise REAL. This could be useful in debugging, where function F will provide checking and trace output, etc. as well as supplying the correct value. Alas, Fortran does not offer the ability to write "palindromic" functions so although <code>N = DAYNUM(Year,Month,Day)</code> is perfectly valid, <code>DAYNUM(Year,Month,Day) = N</code> is not possible with functions, though standard with arrays.

There is no associative access facility, of the form "what indices of A satisfy this condition", except almost: the <code>INDEX</code> function, which however is allowed only for CHARACTER variables. <code>L = INDEX("sometexts","text")</code> will return 5, the location of the first occasion (counting characters from one) where the second parameter exactly matches a portion of the first.

With F90 came the standardisation of many expanded facilities. Arrays can be defined with any lower bound instead of just one, as in <code>REAL A(1951:2017)</code>, and with it now possible to define compound types, there can be arrays of such aggregates. Type matching remains strict, however. There is also much greater flexibility in manipulating arrays without explicit looping. The assignments above could be done via <code>A(3:1:-1) = 1.1</code> or, more normally, by <code>A(1:3) = 1.1</code> The scheme is ''start:stop:step'' with the '':step'' part unnecessary if it is one, as with DO-loops. Simplicity of syntax can however lead to surprises. If a 4 x 4 portion of array B was selected as a parameter (to some matrix-manipulation routine, say) as in <code>CALL TRANSPOSE(B(5:8,6:9))</code>, then the scattered elements of B would be copied into a 4x4 work area first, because the subroutine deals with array elements in contiguous storage: this is copy-in, copy-out rather than the normal pass-by-reference, and if the arrays are large, this will be very slow as well as producing some subtle differences in behaviour.

Similarly, F90 provides additional functions applicable to arrays; there is a <code>TRANSPOSE</code> function, and <code>MAXLOC(A)</code> returns the index of the (first encountered?) maximum value of the array, but there is still no extension for <code>INDEX</code> to other types. However, new statements provide some more associative processing, as in <code>WHERE(A > 0) A = 1/A</code> for a simple case, and similarly, <code>FOR ALL (I = 1:N, A(I) > 0) A(I) = 1/A(I)</code> potentially executes every assignment in parallel. Even so, this is still not an associative memory usage whereby the memory storage device identifies matching indices by its own mysterious inner workings, as with on-chip L1 "cache" memory and the like.

Array sizes are no longer always fixed at compile time: on entry to a subroutine or function it can declare an array of a size determined by that occasion (as in Algol since the 1960s), and arrays can be explicitly allocated and de-allocated storage according to program logic. But, their type and dimensionality remain fixed.

Arrays remain resolutely rectilinear in shape. There is no direct facility to enable "triangular" arrays, still less ragged arrays. Triangular (and potentially other shape) arrays can be attained with a little effort: most simply, use a rectilinear array and waste the unused portion, otherwise, use a one-dimensional array and calculate offsets into it. But it may be possible to employ ''two'' triangular arrays, that can be fitted into a rectilinear array, possibly with special treatment of the diagonal elements; if so, one must be rigorous about keeping track of which has what subscripts! Escalating to three (or more) dimensions is quite possible, but, a calm and resolute mind is needed. As for ragged arrays, F90 facilities are required: consider 
```Fortran
 TYPE AROW
  REAL, ALLOCATABLE:: COL(:)
 END TYPE AROW
 TYPE(AROW) A(66)
```

This declares an array of sixty-six rows, where A(r,c) would be accessed via <code>A(r).COL(c)</code>, but first the code would have to execute <code>ALLOCATE(A(r).COL(first:last))</code> for every row that is to be used to acquire storage for it. Such usage will not be as swift as with plain arrays.

===[[Pascal]]===
This defines an array suitable to hold a 64x64 truecolor image (i.e. red, green and blue RGB values all can go from 0 to 255) and then sets the color of a single pixel

```pascal

type
  color = red, green, blue;
  rgbvalue = 0 .. 255;

var
  picture: array[0 .. 63, 0 .. 63, color] of rgbvalue

begin
  { set pixel (4,7) to yellow }
  picture[4, 7, red]   := 255;
  picture[4, 7, green] := 255;
  picture[4, 7, blue]  := 0
end.

```


===[[jq]]===
jq's data types are the same as JSON's data types, and thus jq's arrays can hold any combination of JSON entities. jq arrays are indexed beginning with 0, so ["hello"][0] evaluates to "hello".


### =Immutability=

jq offers a comprehensive collection of operators and functions for array processing, but to understand them it must be appreciated that all data values in jq are immutable.  In particular, if a is an array, a jq expression such as "a[0] = 1" may give the appearance of updating the array, but it simply returns an array identical to <tt>a</tt> except for the first element.

In fact, an expression such as "a[m] = 1" (where m is some non-negative integer) does not require that the length of the array, a, be at least (m+1). The expression should instead be interpreted to mean: produce an array derived from <tt>a</tt> such that a[m] == 1.  If necessary, jq will add "null" elements to achieve this requirement.  Thus, one way to create an array of m+1 nulls is to write:
```jq
[][m] = null
```

It might seem that jq must be horrendously inefficient because of immutability, but in fact, jq is fast because of compiler optimizations.


### =Basic Array Operations=

Assuming a and b are arrays, and if m and n are non-negative integers:

```jq

[][n] = null # => an array with (1+n) nulls
[range(0;n)] # => [0,1, ... (n-1)]
a | length   # => the length of a
a + b        # => the concatenation of a followed by b
a[n]         # => the element at offset n (or null)
a[-1]        # => the last element (or null)
a[m:n]       # => the subarray [a[m], ..., a[n-1]]

```



### =Streams and Arrays=

jq provides support for streams of JSON entities, and thus the syntax for converting between an array and the stream of entities that an array contains is fundamental to jq.  In brief:

```jq

# If a is an array, then the expression
 a[]
# will produce the stream of a's entities.

# Conversely, if S is a stream of entities,
# then they can be gathered together using the syntax:
[ S ]

```

For example, if S is the stream (1,2,3), then [(1,2,3)] == [1,2,3]. 

This highlights an important point about "," in jq: the comma is not merely a syntactic marker but an operator.


### =map/reduce=

jq's "map" filter is quite conventional, e.g. 
```jq
[1,2,3] | map(-(.))   # => [-1, -2, -3]
```
The <tt>reduce</tt> filter, however, has a special syntax that makes it very powerful. In brief, the syntax is:
```jq
reduce S as $var (init; update)

```
where S is an expression producing a stream of values, and init and update are jq expressions. The reduction takes place by setting a hidden variable to the value of "init" initially, and then performing the update on the hidden variable as specified by "update" for each value, $var, in the stream.  The result of the reduction is the final value of the hidden variable.

For example, here is a toy example followed by an efficient computation of n! (factorial n):
```jq
reduce [1,2,3][] as $i (0; . + $i) # => 6

reduce range(2;n+1) as $i (1; . * $i) # => n!
```


===[[REXX]]===

```rexx
/*REXX program snippets to show a variety (types) of "array" indexing.  */

/*REXX arrays aren't true arrays, but can be used (and thought) as such.*/
/*REXX never needs to allocate an array  (indeed, there is no mechanism */
/*                                        in the language to do so.)    */

  do j=0 to 100                        /*this "array" starts at zero.   */
  quad.j=j**2                          /*assign J's square-->QUAD array.*/
  end

say 'middle number=' quad.50           /*display a use an array element.*/

  do k=-99 to +99                      /*this "array" starts at minus 99*/
  box.k=k**3                           /*assign K's  cube --> BOX array.*/
  end

                                       /*an example of a sparse array:  */
  do m=0 to 10000 by 10                /*"array" only has odd elements. */
  wierd.m=m**3 - m**2                  /*assign something--> WIERD array*/
  end


/*How to assign a value (such as 0)  to all indexes of an array.        */
g.=1-1                                 /*trying to be fancy, but failing*/

    do n=-1e6 to 1e6 by 100
    g.n=sign(n)                        /*assigns -1 to negative elements*/
    end                                /*         1 to positive elements*/
                                       /*         0 to the 0th  element.*/

googol=10**100
say g.googol                           /*will show a value of 0 (zero). */


/*Indexes (or keys) need not be numeric (as in all the examples above). */

quad.kk = 'Diplomacy is the art of saying "nice doggy" until you can',
          'find a rock.'               /*a character 2-liner assignment.*/
qwot=quad.kk;  say  qwot        /*show a quote, make user scratch head. */

                                /*since   kk   hasn't been defined yet, */
                                /*then the index is  KK  (uppercase, but*/
                                /*it doesn't matter if  KK, kk,  kK,  Kk*/
                                /*is used in the program to refer to it)*/


veryStrange='box cañon or [canyon]¿'          /*note special characters.*/
                                /*illustrates a very quarky index (key).*/
                                /*this type of index must be the same   */
                                /*case as the "original"  (lowercase).  */
quad.veryStrange=box.10         /*quad's "strange" index <--one-thousand*/



/*For all intents and purposes, stemmed arrays look like arrays in other*/
/*   languages, except that a dot/period (.) is used.                   */
/*   I.E.:     x.2    instead of  (say, the usual):    x(2)             */
/*Although, if you wanted to use that format, you could.  To illustrate:*/

x.1=1;   x.2='two';   x.3="3.0 ± 1";      do i=1 to 3;  say x.i;  end

  /*another way of using arrays.*/     say x(2)
                                       exit

                                       /*further on down in the program.*/
                                            .
                                            .
                                            .
                                       x: procedure expose x.; parse arg i
                                          return x.i

/*Note that REXX knows that  x(2) is a function, and  x.2  is a variable*/

/*Since all arrays are sparse (or can be),  there doesn't need to be a  */
/*declaritive on the indexes limits (bounds).  Also, there is no limit  */
/*on the number of dimensions of an array.    I.E.:                     */

ht=14411;   z='feet';     mt.rainier.12.9.4.6.44.z.12.7.1.0.6.2.2.2.1 = ht
```


[[Category:Data Structures]]
[[Category:Encyclopedia]]
