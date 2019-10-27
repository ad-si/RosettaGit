+++
title = "Talk:Sorting algorithms/Quicksort"
description = ""
date = 2018-04-24T17:17:09Z
aliases = []
[extra]
id = 2149
[taxonomies]
categories = []
tags = []
+++

==Task statement is buggy==
If array elements are all equal then pseudocode's version of quicksort never returns. [[User:A|A]] 14:52, 14 July 2008 (UTC)

----

Details I'm uncertain on:
* Should it be the in-place form of quicksort, or is it unspecified?
* What is a discrete type?
* Should the program as given work for all arrays with indexes "of any discrete type", or should I pick one "discrete type" and give an example with that?
--[[User:Kevin Reid|Kevin Reid]] 15:42, 3 October 2007 (MDT)
:I'd say follow example of the [[Bubble Sort]] page (from which I stole the text). In other words, do whatever is most comfortable in your language, perhaps pointing out the design choices made. --[[User:IanOsgood|IanOsgood]] 19:02, 3 October 2007 (MDT)


Yeah, the problem statement is completely wrong. The algorithm described is not the real quicksort but a bastardized version that was promoted by the Haskell community that avoids mutation at the cost of asymptotically more space consumption and, consequently, it is practically worthless because it runs hundreds of times slower than a real quicksort (but it can be written concisely in Haskell!). The problem statement should be changed to a real in-place quicksort that actually works. --[[User:Jdh30|Jon Harrop]]
: I'm glad to see you finally created an account (I've been following your FFF blog for a while), but I should mention that vitriol is unwelcome, and vitriolic arguments against particular languages fall directly outside my [[User:Short Circuit#Language Promotion|language advocacy]] policy.  I've tried finding the original definition of Quicksort a few times, but the original paper is hidden behind a paywall. If the task description requires cleanup, then so be it, but access to an authoritative source is lacking. --[[User:Short Circuit|Michael Mol]] 19:45, 19 April 2010 (UTC)
:: The British Computer Society host [http://comjnl.oxfordjournals.org/cgi/content/short/5/1/10 Hoare's original 1962 paper]. In the paper, Hoare emphasizes the speed and memory efficiency of his quicksort algorithm and describes the algorithm explicitly in terms of pointers converging to the middle of the array and "exchanges" of pairs of elements that are in the wrong order and he uses the word "overwrites". In the Conclusion, he says "the data are sorted in situ". I'm sure variations on the theme are welcome but I would expect them to preserve the essence of the original and this bastardized out-of-place "quicksort" does not: it is asymptotically slower in theory (you're supposed to choose the pivot randomly!), hundreds of times slower in practice and consumes asymptotically more memory to the extent that it is practically useless (even the Haskell stdlibs do not use it!). At the very least, I recommend marking the fake quicksorts to avoid confusion. Ideally, demand real quicksorts (preferably parallel and generic) and [http://www.haskell.org/haskellwiki/Introduction/Direct_Translation watch the Haskellers squirm]! --[[User:Jdh30|Jon Harrop]]
::: I already warned you once about vitriol. this site functions as well as it does because of cooperative behavior among participants, regardless of what they think of each others' preferred languages. If you have no intention of non-inflammatory and non-malicious participation, you'll find yourself unwelcome to the point of my exercising admin powers. Last warning on that front. As for the issues with the Quicksort task description, I'll leave that to others to discuss and debate. --[[User:Short Circuit|Michael Mol]] 09:47, 25 May 2010 (UTC)
::I made some measurements for the F# version which physically splits the list to be sorted into 2 new lists for one thousand to ten million elements as follows (x is a random number generator):

```txt

> let n = List.init 1000 (fun _->x.Next());;
> qsort n;;
Real: 00:00:00.012, CPU: 00:00:00.020, GC gen0: 0, gen1: 0
> let n = List.init 10000 (fun _->x.Next());;
> qsort n;;                                  
Real: 00:00:00.047, CPU: 00:00:00.040, GC gen0: 5, gen1: 0
> let n = List.init 100000 (fun _->x.Next());;
> qsort n;;                                   
Real: 00:00:00.485, CPU: 00:00:00.660, GC gen0: 53, gen1: 2
> let n = List.init 1000000 (fun _->x.Next());;
> qsort n;;                                    
Real: 00:00:05.200, CPU: 00:00:06.990, GC gen0: 602, gen1: 10
> let n = List.init 1000000 (fun _->x.Next());;
> qsort n;;                                     
Real: 00:01:34.239, CPU: 00:02:01.510, GC gen0: 7331, gen1: 20

```

which is a little worse than linear but not too bad, so the situation for Haskell may not be as bad as you think. I have 8 cores available to me and if I wanted to write a parallel version I'd prefer the copied list to the single array with pointers. Even using a single core swapping in place requires three read write operations: read i and write to temp; read j and write to i; read temp and write to i. Whereas copying to new lists can be done in one read and write.  
--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 17:17, 24 April 2018 (UTC)

==Algorithm description==
Would someone care to add some text describing the Quicksort algorithm?  I.e., its worst-case completion time, a pseudocode implementation, what the pivot does, etc. --[[User:Short Circuit|Short Circuit]] 03:28, 7 October 2007 (MDT)

: Quicksort is an elegant and powerful algorithm. If you want to learn about it, read the [http://comjnl.oxfordjournals.org/cgi/content/short/5/1/10 Hoare's original 1962 paper on Quicksort]. Note that the task description and code here on Rosetta Code and the Wikipedia article are wrong. --[[User:Jdh30|Jon Harrop]]
::If it wouldn't be too much trouble (since you seem to understand it yourself) could you maybe add a summary to this task then? It would be more convenient to have the proper sort on-site. --[[User:Mwn3d|Mwn3d]] 03:17, 26 May 2010 (UTC)
::: I'm not sure what you mean by a summary but I can certainly fix the task description if you like. The quicksort algorithm is very simple: you randomly choose a pivot and swap it with the last element in the current slice of the array, then you start pointers at either end of the slice and move them towards the middle swapping any pairs that are out of place until the pointers meet and, finally, recursively quicksort the lower and upper slices separated by the point the pointers converged on. Sedgewick gave a nice C++ implementation (without randomization) [http://www.cs.princeton.edu/~rs/talks/QuicksortIsOptimal.pdf here] except for a bug where the array element should only be loaded within the 'if' statement. Here's the fixed code: --[[User:Jdh30|Jon Harrop]]

    void quicksort(Item a[], int l, int r) {
      if (r > l) {
        int i = l-1, j = r;
        Item v = a[r];
        for (;;) {
          while (a[++i] < v) ;
          while (v < a[--j]) if (j == l) break;
          if (i >= j) break;
          exch(a[i], a[j]);
        }
        exch(a[i], a[r]);
        quicksort(a, l, i-1);
        quicksort(a, i+1, r);
      }
    }

== UnixPipes solution ==

It seems to me that the UnixPipes implementation violates the conditions given on [[:Category:UnixPipes]]: There you find:
:''It does not include programming problems solved with normal shell by using the control structures available as a part of shell outside of pipelines.''
However the quicksort implementation here uses the following shell constructs:
* named function
* while loop
* case statement
So either the example, or the UnixPipes description is wrong. --[[User:Ce|Ce]] 15:41, 7 April 2008 (MDT)

The intension in UnixPipes description is to include only those solutions that use the pipelines as computational paradigm.
(So as to exclude the imperative solutions in normal shell)
Since I do not seem to have captured the intension correctly, I will remove that line from UnixPipelines until I can think of a better way to put it. Thanks for the attention.
[[User:Rahul|Rahul]] 02:36, 8 April 2008 (MDT)

== MATLAB... works? ==

Does MATLAB work? It defines a function named quicksort, but then calls qsort; and the return after the function declaration... if it is like Octave, it makes the program terminate so that the test is never run (maybe the file where you stored the code was called qsort? this could fix something... but doesn't MATLAB complain about the fact that your file is called qsort while your function is quicksort? --[[User:ShinTakezou|ShinTakezou]] 23:04, 25 June 2009 (UTC)

I've fixed that; I'm used to short names; after entering the code and looking at the whole page, it occurred to me that I should use the same name that everyone else was using. --[[User:mecej4|mecej4]] 11:53, 26 June 2009 (CDT)

== Fortran-90 code ... pivot choice can cause it to go O(N^2) ==
If the input numbers have a distribution which plots as a symmetric triangle, the first and last elements will be equal and their mean is a bad choice for pivot.
--[[User:mecej4|mecej4]] 12:21, 26 June 2009 (CDT)

Note that the problem isn't that it's a bad pivot choice. It's that the comments say it's a good pivot choice. Other examples have bad pivot choices (Java just picks the first element), but the task doesn't require a good pivot choice. --[[User:Mwn3d|Mwn3d]] 17:25, 26 June 2009 (UTC)

: The comments say just it's a good pivot choice for already sorted data or for reverse sorted data; I haven't checked it, but I can believe it, as I can believe it's not a so good choice for "symmetric triangle" distribution. --[[User:ShinTakezou|ShinTakezou]] 18:44, 26 June 2009 (UTC)

== IDL ==

"IDL has a powerful optimized sort() built-in. The following is thus merely for demonstration."

This is true for most languages represented here, either as a built-in or in included standard libraries.
