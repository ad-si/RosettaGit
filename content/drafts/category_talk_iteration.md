+++
title = "Category talk:Iteration"
description = ""
date = 2014-05-18T06:10:59Z
aliases = []
[extra]
id = 2825
[taxonomies]
categories = []
tags = []
+++

I'm not sure about [[Loop/Map]] and [[Loop/Fold]]. I've never heard of them before. Can someone else fill in tasks for them? --[[User:Mwn3d|Mwn3d]] 12:33, 14 April 2008 (MDT)

: Loop/Map is already there, as [[Apply a callback to an Array]] (see the Common Lisp solution!) I don't know what Loop/Fold means, but maybe it's also already there under another name.  --[[User:Ce|Ce]] 13:28, 18 April 2008 (MDT)
::The articles [[Sum and product of array]] contains some examples of '''Fold'''(or reduce). '''Fold''' apply a binary function(ZxZ->Z) to each elements of a collection with an accumulator, the result value is set to the accumulator, and the final value of accumulator after iterated all elements is the result of '''Fold''' (the accumulator initialized to 1.0 in Product case, and 0.0 in Sum case). '''Map''' is similar, but with an unary function Z->Z. There is also '''Filter/Select'''(compare [[Select from Array]]) similar to '''Map''', with an unary boolean function, but don't keep elements if result value is false. -- [[User:Badmadevil|badmadevil]] 05:58, 19 April 2008 (MDT)
:: FWIW, [[Apply a callback to an Array]] isn't Loop/Map because there's no requirement to reassemble the values into another array (or sequence/list, etc.) In [[SML]]-like type-theoretic terms, Map is
::: <math>(\alpha \rightarrow \beta) \rightarrow \alpha\, \mathrm{list} \rightarrow \beta\, \mathrm{list}</math>
:: whereas that task allows
::: <math>(\alpha \rightarrow \mathrm{unit}) \rightarrow \alpha \, \mathrm{list} \rightarrow \mathrm{unit}</math>
:: These are quite different type signatures, though Map can be used for the other by throwing away the final result. –[[User:Dkf|Donal Fellows]] 12:40, 4 January 2010 (UTC)

What about "implied loops" like the one Fortran can use to initialize arrays or in the where construct? (Maybe implied is not correct... but the final operation in other languages like C is a ''for'' loop, but in these languages it is not written explicitly); e.g.

```c
for(i=0; i<10; i++) {
  if ( ( arr[i] % 5 ) == 0 ) {
     arr[i] = 9
  } else {
     arr[i] = 3
  }
}
```


in Fortran(90/95) would be


```fortran
where ( mod(arr, 5 ) == 0 ) 
  arr = 9
elsewhere
  arr = 3
end where
```


And to initialize an array with numbers from 0 to 100 one can write


```fortran
integer, dimension(101) :: a = (/ (j,j=0,100) /)
```


Is there any possibile classification that makes sense to other languages too? --[[User:ShinTakezou|ShinTakezou]] 00:17, 18 February 2009 (UTC)

== OpenMP looping? ==

It would be nice to have some examples of OpenMP looping constructs in here (probably as subsections of the C and Fortran language examples) since they're interesting in their own right and are fairly common extensions. —[[User:Dkf|Dkf]] 11:38, 23 May 2009 (UTC)

== Um... ==

J is all about structured iteration. So much so that early versions of the language (APL) did not incorporate conditionals. Then again, mechanisms like recursion (or at least tail recursion) could also be thought of as a kind of iteration.

But I'm not certain I want to be building out pages for every single J control primitive. Most languages just give you a few basic tools and then are unstructured after that and most languages don't have the semantic constructs that drive and control J iteration mechanisms

These range from simple things, like prefix and suffix scans:


```J
   <\ 'abcdef'
┌─┬──┬───┬────┬─────┬──────┐
│a│ab│abc│abcd│abcde│abcdef│
└─┴──┴───┴────┴─────┴──────┘
   <\. 'abcdef'
┌──────┬─────┬────┬───┬──┬─┐
│abcdef│bcdef│cdef│def│ef│f│
└──────┴─────┴────┴───┴──┴─┘
```


..to more complex things such as inner product builders, diagonals, and tessellations which have are only meaningful for tables or higher ranked arrays (think of SQL if you want a language with slightly similar constructs).

And then there's stuff like [wp:Rank_(J_programming_language)|rank] - I'm not even sure how to write a rosettacode task for a general mechanism like that.

Anyways, are we really sure we want an exhaustive treatment of iteration techniques? If so, I guess maybe we should start with "Emulate J Rank", with task examples involving arrays with shapes like 2 2 2 2 2 (the sort of thing that might be used in an fft)? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 06:10, 18 May 2014 (UTC)
