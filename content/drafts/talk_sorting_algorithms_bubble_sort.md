+++
title = "Talk:Sorting algorithms/Bubble sort"
description = ""
date = 2011-09-24T22:40:04Z
aliases = []
[extra]
id = 1877
[taxonomies]
categories = []
tags = []
+++

== Algorithm link ==

Seems like it would be better for algorithm tasks to include their natural-language description, infobox, and perhaps pseudocode in a top-level section before the examples rather than on a separate page.  --[[User:Bob9000|Bob9000]] 07:41, 31 January 2007 (EST)

:Removed.  Now for someone to fill in the description of the algorithm... --[[User:Short Circuit|Short Circuit]] 10:53, 31 January 2007 (EST)

== This is bubble sort? ==

This isn't the bubble sort I've learned. Where did you get this algorithm? This is my bubble sort:
 
 void sort(int *a, int size)
 {
   int i,j;
   for (j=size-1; j>0; j--)
     for (i=0; i<j; i++)
       if (a[i+1] < a[i])
         swap(a+i);
 }

--IanOsgood

:That may be "your bubble short", but it is definitely not Bubble Sort. The basic feature of Bubble Sort is that it finishes sorting when there were no more swaps needed. Because of this, the best case execution time of Bubble Sort is O(N), i.e. linear time, which is significantly better than that of QuickSort, O(N*Log(N)). --[[User:PauliKL|PauliKL]] 18:51, 24 November 2008 (UTC)
::I also learned the bubble sort the way without checking for swaps (twice). I think we can all agree that the bubble sort is a comparison sort. If so, it would ONLY compare and swap items in the collection AND its best possible complexity would be O(n*log(n)). Adding this check for swaps seems like an optimization that takes it out of the comparison sort category, so I don't think it's part of the original algorithm. It may be the way that people implement it in practice (though I don't know why you'd use it in practice), and it may also be a welcome optimization (at least worth noting that it exists), but it doesn't seem like it's part of the idea of the sort. The "basic feature" of the bubble sort is the one it gets its name from: smaller elements bubbling to the top (front) of the collection. --[[User:Mwn3d|Mwn3d]] 19:38, 24 November 2008 (UTC)
:::No, it is exactly the opposite. Testing if swaps were made in inner loop was '''the only''' exit criteria in the original algorithm (as I learned it back in the early eighties).  Bubble Sort goes through all the values, swapping any two values that are not in correct order, and repeats from beginning if any swaps were made. That is where the name ''Bubble Sort'' cames from. The small values bubble towards the top. The sorting needs to continue as long as there are more bubbles going up. The fact that largest value falls to bottom is a side effect that can be used to optimize the outer loop. But that only reduces the worst case execution time to half.
:::The code above is not Bubble Sort, it is an inefficient implementation of insertion sort. It does not care about the bubbles, only  about the largest value falling to bottom. --[[User:PauliKL|PauliKL]] 13:51, 9 December 2008 (UTC)

:I think the difference is splitting hairs.  One can unconditionally implement the nested loops for a guaranteed run-time of O(n**2) or one can track whether any swaps were performed in the most recent (inner loop) pass.  One can consider that test to be an "optimization" which improves the best case while having negligible effect on typical and worst case performance.

:Given that the  bubble sort is only useful for pedagogical (educational/instructional) purposes it's worth discussing these differences in an academic coverage.  Their the focus would be on the trade-offs between code complexity (an extra assignment in the innermost conditional and a couple of extra statements in the out loop to reset and check the flag) vs. the performance benefit (in this case only for best case or near best cases).

:However, the different is trivial for purposes of the code examples here.  The use of bubble sort examples on RosettaCode is to present each language's syntax and features as applied to an extremely familiar algorithm, one which is widely studied and understood.  Having the conditional "optimizations" shows more of each language's fundamental syntax and features. [[User:JimD|JimD]] 23:30, 24 November 2008 (UTC)

::That is not splitting hairs, it is a fundamental difference. We are talking about two different algorithms, poor implementation of insertion sort (the code above) vs. Bubble Sort. And there is huge difference in the best case execution time: O(n**2) vs. O(n). The best case is when the data is nearly sorted, which is a very common case.

::Bubble Sort is definitely not "only useful for pedagogical purposes". It is the simplest sorting algorithm. In cases where the data set is small, there is no point using more complex algorithm. Especially in cases when the data is expected to be nearly sorted. In fact, for small data sets, a more complex algorithm is probably slower.

::The difference is not insignificant in RosettaCode either. In order to be able to compare languages, it is important that all the implementations use the same algorithm. And it seems that Bubble Sort is not so familiar algorithm after all, since so many people seem to mix Bubble Sort and the poor implementation of Insertion Sort.
:: --[[User:PauliKL|PauliKL]] 16:49, 9 December 2008 (UTC)

:::I'd like to say mine: the pseudocode proposed is a BubbleSort; as far as I remember, BubbleSorting can be implementend in both the way, and still is BubbleSort. The name BubbleSort comes since it let the smaller elements get on the top like bubbles (in water?), letting the lighter bubble pass beyond the near heavier bubble. The principle is the same, but the pseudocode is more efficient since there's a test that avoid looping without swapping (while in the previous code extern loop must be executed even if the inner loop has not swapped anything) --[[User:ShinTakezou|ShinTakezou]] 17:31, 9 December 2008 (UTC)

: As the person who originally asked the question, I acknowledge that I was mistaken about the definition of "Bubble Sort". The given algorithm was the first sorting algorithm I learned, but I am happy to implement the better one for this task. The unoptimized bubble sort is the one analyzed (and derided) by Knuth. I mean...  '''KNUTH!'''  /discussion  --[[User:IanOsgood|IanOsgood]] 18:04, 9 December 2008 (UTC)

::Ok, then let's call it Optimized Bubble Sort, since it is that: a Bubble Sort that does not waste times executing outmost loop when the inner loop '''always''' won't swap anything. This condition becomes true since for the first time the inner loop makes no change.


```txt

 void sort(int *a, int size)
 {
   int i,j, swapped=0;
   for (j=size-1; j>0; j--) {
     swapped=0;
     for (i=0; i<j; i++) {
       if (a[i+1] < a[i]) {
          swap(a[i], a[i+1]); swapped=1
       }
     }
     if ( swapped == 0 ) break;
   }
 }

```


::I wrote it fastly, but hopely it works; would you call it still BubbleSort or no? Do you agree with the fact that if the inner loop makes no swap, it will make no swap even in every other iteration? So it is still bubble sort in principle, but the code is optimised; but let us have the implementation A of the algo X. If the Code Optimizer (a person) optimize A generating the implementation B, B is still an implementation of X? Even though Knuth would be more precise in word using, I believe it would say yes. On internet you can find also interesting resources, like [http://www.cs.duke.edu/~ola/bubble/bubble.pdf this pdf file] where we can read:

<blockquote style="background-color:#DDF; padding:5px">
Nearly every description of bubble sort describes how
to terminate the sort early if the vector becomes sorted.
This optimization requires checking if any swaps are
made and terminating if no swaps are made after j it-
erations of the inner loop.
</blockquote>

::and presents a code very close to yours. Even the wikipedian explanation of how the BubbleSort operates tell the fact that they are just two different implementation of the same thing, the first being an optimisation of the more rough way of doing it. --[[User:ShinTakezou|ShinTakezou]] 18:37, 9 December 2008 (UTC)

:::Yes, that is an optimized version of Bubble Sort which reduces the worst case execution time to half. (I fixed the swap command in your code.) The correct un-optimized version of Bubble Sort is given in the pseudo code in the article. The first code in this thread is a poor implementation of Selection sort (not Insertion Sort as I said earlier). --[[User:PauliKL|PauliKL]] 16:08, 10 December 2008 (UTC)

::::About swap, I've just copy-pasted the code on top of this section, it was not my intention to give working code here, I was just showing a point. It does not matter, but... the first code in this thread is no more the first :) --[[User:ShinTakezou|ShinTakezou]] 00:28, 11 December 2008 (UTC)

: How about a compromise? We implement both optimized and unoptimized versions on some other tasks. Why not here? (From that paper on the history of Bubble Sort, it appears that both forms have been named Bubble Sort over time.  Earlier names include ''sorting by exchange'', "exchange sorting", and ''shuttle sort''.  It appears Ken Iverson of [[APL]] and [[J]] fame first coined ''Bubble Sort''.) --[[User:IanOsgood|IanOsgood]] 17:22, 10 December 2008 (UTC)

::It could be done, but I suppose it is up to implementors; I've not done the C implementation, but if I had, I would have done it using the optimized "form", disregarding the ''expensive'' "form". --[[User:ShinTakezou|ShinTakezou]] 00:28, 11 December 2008 (UTC)

:::I don't know if it is necessary to have two implementations that has so little difference. In practice, the optimized version only requires decrementing the upper limit of the inner loop on each iteration and setting the initial value at the beginning. However, I notice both versions have been used in the implementations. So it might be good idea at least to mention on each implementation whether it uses optimized or un-optimized algorithm. --[[User:PauliKL|PauliKL]] 15:42, 11 December 2008 (UTC)

== Never used? ==

In the algorithm description, there is sentence ''"Because of its abysmal O(n2) performance, it is never used anywhere else"''.

That is, of course, totally untrue. Since bubble sort is the simplest sorting algorithm, it is the best choice for situations where speed is not critical (e.g. when sorting only small datasets), and/or when code size is important. In addition, it is stable and requires no additional space for temporary storage.

Further, there is nothing abysmal about O(n<sup>2</sup>) performance. Many commonly used algorithms have the same performance (e.g. insertion sort, selection sort). Worst case performance of Bubble Sort is the same as that of the best implementations of QuickSort, i.e. O(n<sup>2</sup>), and much better than naive implementation of QuickSort (infinite time). And best case performance for Bubble sort is much better than that of QuickSort.

Of course, Insertion Sort is only slightly more complex than Bubble Sort, and it is somewhat faster, so it could be preferred.
--[[User:PauliKL|PauliKL]] 16:22, 11 December 2008 (UTC)

== CMake ==

My original [[CMake]] code was too slow, approaching [[O|O(n<sup>3</sup>)]]. A good bubble sort should be [[O|O(n<sup>2</sup>)]]. I benchmarked my CMake code with this [[Ruby]] script:


```ruby
require 'benchmark'
require 'tempfile'

def string(n)
  (1..n).collect { rand(32000) }.join " "
end

def trial(string)
  src = Tempfile.new 'cmake'
  src.print <<EOF
# bubble_sort(var [value1 value2...]) sorts a list of integers.
function(bubble_sort var)
  # !!!!! !!!!! !!!!! insert CMake code here !!!!! !!!!! !!!!!
endfunction(bubble_sort)

bubble_sort(result #{string})
EOF
  src.flush
  system "cmake", "-P", src.path
  src.close
end

Benchmark.bm do |x|
  a = string(100)
  b = string(200)
  c = string(300)
  d = string(400)
  x.report("100 integers") { trial(a) }
  x.report("200 integers") { trial(b) }
  x.report("300 integers") { trial(c) }
  x.report("400 integers") { trial(d) }
end
```


[http://rosettacode.org/mw/index.php?title=Sorting_algorithms/Bubble_sort&oldid=121250#CMake The old code] used list operations against ''ARGN''. Because a list is a semicolon-delimited string (like "11;22;33;44;55;66"), these list operations might be O(n). Contrast other languages, where array operations are O(1).

Benchmark results for [http://rosettacode.org/mw/index.php?title=Sorting_algorithms/Bubble_sort&oldid=121250#CMake the old code]:


```txt
$ ruby bubble-old.rb  
       user     system      total        real
100 integers  0.000000   0.000000   0.760000 (  0.764968)
200 integers  0.000000   0.010000   4.860000 (  4.875269)
300 integers  0.000000   0.000000  14.290000 ( 14.370998)
400 integers  0.000000   0.000000  31.700000 ( 31.647215)
```


* 100 integers: ''1 time''
* 200 integers: ''6 times slow''
* 300 integers: ''19 times slow''
* 400 integers: ''42 times slow'' '''(32 seconds)'''

The old code was slightly better than O(n<sup>3</sup>), but not much better. 6 was almost 8, 19 was almost 27, 42 was almost 64.

[http://rosettacode.org/mw/index.php?title=Sorting_algorithms/Bubble_sort&oldid=121403#CMake The new code] uses associative array operations against ''ARG1'' to ''ARG${last}''. The switch from a list to an associative array improves performance, but requires extra code at the end of bubble_sort() to convert the sorted associative array to a regular list.

Benchmark results for [http://rosettacode.org/mw/index.php?title=Sorting_algorithms/Bubble_sort&oldid=121403#CMake the new code]:


```txt
$ ruby bubble-new.rb  
       user     system      total        real
100 integers  0.000000   0.000000   0.330000 (  0.343380)
200 integers  0.000000   0.000000   1.220000 (  1.220903)
300 integers  0.000000   0.000000   2.700000 (  2.803828)
400 integers  0.000000   0.000000   4.910000 (  5.047400)
```


* 100 integers: ''1 time''
* 200 integers: ''4 times slow''
* 300 integers: ''8 times slow''
* 400 integers: ''15 times slow'' '''(5 seconds)'''

The new code is very close to O(n<sup>2</sup>). The new bubble_sort() for CMake runs as fast as a bubble sort should run. --[[User:Kernigh|Kernigh]] 22:40, 24 September 2011 (UTC)
