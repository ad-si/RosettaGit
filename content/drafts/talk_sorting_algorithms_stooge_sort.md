+++
title = "Talk:Sorting algorithms/Stooge sort"
description = ""
date = 2016-04-04T21:44:55Z
aliases = []
[extra]
id = 7809
[taxonomies]
categories = []
tags = []
+++

Im having a bit of an issue with syntax apparently. I have the latest code here:

```Newlisp


(define (stoogesort L left right) 
  (println "list : " L " left: " left " right: " right " calc: " (- right (/ (- right (+ left 1))3)))
  (if (> ( L left ) ( L right )) 
    (swap  (L left)  (L right) 
      (if (< (- left right) 1) (print "hello")
        (stoogesort
          (L left (- right (/ (- right (+ left 1))3))
            (stoogesort
              (L (+ left (/ (- right (+ left 1))3)) right
                (stoogesort(L left (- right (/ (- right (+ left 1))3)))))))))))
  L)






(stoogesort (list 7 3 2 3 4 0) 0 5)


```

It seems to never get to the recursive step. I'm wondering why as I did some other tests to see whats going on. I feel extremely dumb right now with such an algorithm....
--Michael Chrisco

:I do not know enough about Newlisp to be very helpful, but I formatted your code to better represent matching parenthesis.  (I am stuck on: How does L work in the function position of a list when composing arguments for nested stoogesorts?  Also, what modifies the L which you return from your defined function -- or was L supposed to be the else clause on your if statement -- if so I think it needs to be moved over inside the parenthesis to its left?)  --[[User:Rdm|Rdm]] 15:19, 5 August 2010 (UTC)

== WP markup reference problem ==
The wikipedia markup tag creates an article reference that creates a new article and doesn't reference the actual article.  Not sure how to fix. --[[User:Dgamey|Dgamey]] 14:36, 12 September 2010 (UTC)

----
== ooRexx / NetRexx Questions ==
What is the reason to use the underscores in the variables (in NetRexx and taken over to ooRexx?
--[[User:Walterpachl|Walterpachl]] 11:24, 5 July 2012 (UTC)
: Just personal style; I strongly disapprove of single character variables and the pseudo-code on which the solution was based used single characters.  Adding underscore to the names was a less invasive change than renaming the variables (but would perhaps have made the code more self-explanatory: mea culpa).  However using underscore in iterator names helps me find them easily and with iterators, concise is nice.  Another plus is that it's easier to   <strike>but</strike> put   the cursor between two characters when double-clicking than to try to grab a single character in some IDEs. --[[User:Alansam|Alansam]] 15:12, 5 July 2012 (UTC)
----
ooRexx solution:
 This comment incorrectly taken over?
 Helper class.  Map get and set methods for easier conversion from java.util.List
I guess there is no relationship to java.util.List
 --[[User:Walterpachl|Walterpachl]] 13:22, 5 July 2012 (UTC)
: In this case the solution is translated from NetRexx which uses the <tt>java.util.List</tt> collection.  Java's <tt>List</tt> class uses <tt>get()</tt> and <tt>set()</tt> where ooRexx uses <tt>at()</tt> and <tt>put()</tt>.  The <tt>NList</tt> class implements a clean way to add these methods to ooRexx's <tt>List</tt> as an aid to translation. --[[User:Alansam|Alansam]] 15:12, 5 July 2012 (UTC)
---
Thanks for the answers
I'd rather read ii instead of i_ but at least you told me why.

But the comment on java should be removed or rephrased.
You don't USE the java here, do you?
--[[User:Walterpachl|Walterpachl]] 15:59, 5 July 2012 (UTC)

== task requirement clarification ==
Just to be clear(er):   Is the task requirement to show the '''output''' of a Stooge Sort, or just show the algorithm?   Most (but not all) include output, but some examples only show a subroutine without an invoking (main) program.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:44, 4 April 2016 (UTC)
