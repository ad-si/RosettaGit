+++
title = "Talk:Topological sort"
description = ""
date = 2016-02-23T10:33:51Z
aliases = []
[extra]
id = 4810
[taxonomies]
categories = []
tags = []
+++

==J implementation==


### first implementation


These are brief notes, and do not attempt to document the language itself.


```J
dependencySort=: monad define
  parsed=. <@;:;._2 y
  names=. {.&>parsed
  depends=. (> =@i.@#) names e.S:1 parsed
  depends=. (+. +./ .*.~)^:_ depends
  assert.-.1 e. (<0 1)|:depends
  (-.&names ~.;parsed),names /: +/"1 depends
)
```


<code>parsed</code> is a list of lines.  Each line is a boxed list of words.  Each word is a boxed list of characters.

<code>names</code> is the first word extracted from each line.

<code>depends</code> is a connection matrix -- rows and columns correspond to names, and the values are bits -- 1 for names which depend on other names, and 0 for names which do not depend on other names.

The phrase <code>(> =@i.@#)</code> means that names are not allowed to depend on themselves (since the specification said self dependencies should be ignored, and this makes detecting circular dependencies trivial).

The phrase <code>(+. +./ .*.~)^:_</code> performs transitive closure on a connection matrix.

The assert statement checks for names which depend on themselves.  Since we no know names depended on themselves before we did our transitive closure, we know we have a problem if we have any such dependencies.

Finally, we sort the names so that names with fewer dependencies are followed by names with more dependencies.  And, we prepend any names which we depend on which would otherwise have no dependencies.

For the example data, the temporary variable <code>names</code> gets the value:

 ┌──────────────┬────┬────┬────┬────┬────┬────┬────┬─────┬─────┬──────┬────────────┬────────┐
 │des_system_lib│dw01│dw02│dw03│dw04│dw05│dw06│dw07│dware│gtech│ramlib│std_cell_lib│synopsys│
 └──────────────┴────┴────┴────┴────┴────┴────┴────┴─────┴─────┴──────┴────────────┴────────┘

(Note: this is meant to be viewed in a fixed width font, and the non-alphabetic decorating characters are meant to be line drawing characters.  If you are not seeing this, and you want to, you might try using a different browser.  Or, if the text is fixed width but the line drawing characters are mis-aligned, you might try using a different fixed width font in your browser.)
:(There are other problems with using the box characters too, such as the extra space between lines. If it's just illustrative, try a mediawiki table instead. –[[User:Dkf|Donal Fellows]] 22:45, 1 March 2010 (UTC))

The result of <code>names e.S:1 parsed</code> is then:

 1 1 1 0 0 0 0 0 0 0 1 1 1
 0 1 0 0 0 0 0 0 1 1 0 0 0
 0 0 1 0 0 0 0 0 1 0 0 0 0
 0 1 1 1 0 0 0 0 1 1 0 0 1
 0 1 0 0 1 0 0 0 1 1 0 0 0
 0 0 0 0 0 1 0 0 1 0 0 0 0
 0 0 0 0 0 0 1 0 1 0 0 0 0
 0 0 0 0 0 0 0 1 1 0 0 0 0
 0 0 0 0 0 0 0 0 1 0 0 0 0
 0 0 0 0 0 0 0 0 0 1 0 0 0
 0 0 0 0 0 0 0 0 0 0 1 0 0
 0 0 0 0 0 0 0 0 0 0 0 1 0
 0 0 0 0 0 0 0 0 0 0 0 0 1

In other words, rows and columns both correspond to names, and a value is 1 if the name for that row depends on the name for that column.  We next clean up the diagonal, using the phrase <code>(> =@i.@#)</code>, yielding:

 0 1 1 0 0 0 0 0 0 0 1 1 1
 0 0 0 0 0 0 0 0 1 1 0 0 0
 0 0 0 0 0 0 0 0 1 0 0 0 0
 0 1 1 0 0 0 0 0 1 1 0 0 1
 0 1 0 0 0 0 0 0 1 1 0 0 0
 0 0 0 0 0 0 0 0 1 0 0 0 0
 0 0 0 0 0 0 0 0 1 0 0 0 0
 0 0 0 0 0 0 0 0 1 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0

And, we then perform a transitive closure (if a name1 depends on name2 and name2 depends on name3, then name1 depends on name3), using the phrase <code>(+. +./ .*.~)^:_</code>, which yields:

 0 1 1 0 0 0 0 0 1 1 1 1 1
 0 0 0 0 0 0 0 0 1 1 0 0 0
 0 0 0 0 0 0 0 0 1 0 0 0 0
 0 1 1 0 0 0 0 0 1 1 0 0 1
 0 1 0 0 0 0 0 0 1 1 0 0 0
 0 0 0 0 0 0 0 0 1 0 0 0 0
 0 0 0 0 0 0 0 0 1 0 0 0 0
 0 0 0 0 0 0 0 0 1 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0

Finally, we sum each row, and sort the names in order by their dependency count.


### alternate implementation


The above implementation is a bit naive, since a connection matrix is O(n^2) in space and O(n^3) in time for n dependencies.  If this matters, I should probably rewrite the code (and these comments) to use the tree structure mentioned at http://www.jsoftware.com/jwiki/Essays/Tree%20Sum#Descendants

: I look forward to when you distill this into the J solution. (Maybe provide it twice, once in expanded form with annotations?) —[[User:Dkf|Donal Fellows]] 22:18, 1 September 2009 (UTC)

Here's that alternate implementation.  The algorithm remains unchanged -- I have just represented the dependencies using a different data structure.


```J
depSort=: monad define
  parsed=. <@;:;._2 y
  names=. {.&>parsed
  depends=. (-.L:0"_1 #,.i.@#) names i.L:1 parsed
  depends=. (~.@,&.> ;@:{L:0 1~)^:_ depends
  assert.-.1 e. (i.@# e.S:0"0 ])depends
  (-.&names ~.;parsed),names /: #@> depends
)
```


In other words, instead of using a connection matrix, we use lists of name indices.  In other words, the result of <code>names i.L:1 parsed</code> is

 ┌──────────────────────┬──────────┬────────┬────────────────────┬────────────┬────────┬────────┬──────┬──────┬──────┬────────┬────────┬──┐
 │0 13 12 11 0 2 1 10 13│1 13 1 8 9│2 13 2 8│3 13 12 8 3 2 1 13 9│4 4 13 1 8 9│5 5 13 8│6 6 13 8│7 13 8│8 13 8│9 13 9│10 13 13│11 13 11│12│
 └──────────────────────┴──────────┴────────┴────────────────────┴────────────┴────────┴────────┴──────┴──────┴──────┴────────┴────────┴──┘

(and, once again, my apologies if your browser does not render this properly.)

As before, we need to remove cases where a name depends on itself.  But, here, we also need to remove dependencies on names which are not in our names list.  After we use the phrase <code>(-.L:0"_1 #,.i.@#)</code> our cleaned up dependency list looks like this:

 ┌────────────┬───┬─┬──────────┬─────┬─┬─┬─┬┬┬┬┬┐
 │12 11 2 1 10│8 9│8│12 8 2 1 9│1 8 9│8│8│8││││││
 └────────────┴───┴─┴──────────┴─────┴─┴─┴─┴┴┴┴┴┘

We then use the phrase <code>(~.@,&.> ;@:{L:0 1~)^:_</code> to get our transitive closure:

 ┌────────────────┬───┬─┬──────────┬─────┬─┬─┬─┬┬┬┬┬┐
 │12 11 2 1 10 8 9│8 9│8│12 8 2 1 9│1 8 9│8│8│8││││││
 └────────────────┴───┴─┴──────────┴─────┴─┴─┴─┴┴┴┴┴┘

If I have been too brief on some subject, please feel free to ask questions.  (I could, hypothetically, expand this discussion out into a tutorial on the J language, but I have already done something like that on a few rosetta code pages and that sort of thing gets tiring after a while -- especially when I do not get any feedback from the audience about their interests.  Also I do not want to be spending too much time boring people who do not care at all about J.)

[[User:Rdm|Rdm]] 15:48, 2 September 2009 (UTC)

: [http://paddy3118.blogspot.com/2009/09/j-for-py-guy.html J for a Py Guy] --[[User:Paddy3118|Paddy3118]] 17:37, 2 September 2009 (UTC)

Note: Roger Hui has pointed out that, in the version of J I am using (version 6.02a), the expression <code>;@:{L:0 1~</code> is slow on large data sets (for example: trees with 2e5 nodes).  For now, a faster (but more obscure) implementation of the same algorithm is <code>3 : '(*c) #^:_1 (I.c) <@;/. (;y){y [ c=. #&> y'</code>.  In both cases, we are finding the grandchildren which correspond to the current children. [[User:Rdm|Rdm]] 18:21, 2 September 2009 (UTC)

== Clojure example ==

I don't know much about Clojure, but I did notice that most of its section's leader description describes the role of code in the example. Could someone migrate that description appropriately into comments within the code sample? --[[User:Short Circuit|Michael Mol]] 18:02, 25 March 2010 (UTC)

== Task Name? ==

As far as I see, all the Sorting tasks with the exception of the Topological Sort and Topological sort/Extracted top item start with "Sort ...". or "Sorting algorithm".  Would it not be better for consistency to rename these two.--[[User:Dgamey|Dgamey]] 11:35, 10 August 2011 (UTC)

"Sort topologically"? Yeuch! I would hate to put the word topologically in a title. Maybe "Sort/Topological sort"? --[[User:Paddy3118|Paddy3118]] 12:27, 10 August 2011 (UTC)
: [[Sort/Topological]]? --[[User:Short Circuit|Michael Mol]] 14:08, 10 August 2011 (UTC)

: All of the "Sorting algorithms", like [[Sorting algorithms/Insertion sort]], can sort a list of numbers. "Topological sort" cannot sort a list of numbers, therefore it belongs not with the "Sorting algorithms", and retains its page name "Topological sort". --[[User:Kernigh|Kernigh]] 15:20, 10 August 2011 (UTC)
:: You can sort numbers using topological sort.  A trivial example of course would be to sort them based on a "less than" or "greater than" relationship.  However, you can use other relationships, such as "is a factor of" or "is a product of" or "is a hailstone predecessor of" or whatever else...  That said, this task is not just about sorting, but also about putting things into buckets.  And that, I think, is a good reason for keeping this task separate from the other [[Sorting algorithms]] tasks. --[[User:Rdm|Rdm]] 15:32, 10 August 2011 (UTC)
::Also this doesn't seem to have an algorithm. It's more like a class of sorts (like [[wp:comparison sort|comparison sorts]]). The class of items that it can sort shouldn't matter for including it under [[Sorting algorithms]], but the fact that it's not an algorithm should. --[[User:Mwn3d|Mwn3d]] 15:25, 10 August 2011 (UTC)

:I guess we could re-name all the 'Sorting algorithms/*' to 'Sort/Magnitude/Algorithms/*' then this could slot in as 'Sort/Topological' (as we don't specify any algorithms). See [[wp:Sorting]]. (I used the word magnitude rather than their use of the word intensity). Then again, we could just leave the 'Sorting algorithms/*' as they are. --[[User:Paddy3118|Paddy3118]] 17:46, 10 August 2011 (UTC)
: All other sorts require full ordering, that is for any two elements a and b, the comparison a < b or a <= b is meaningful.  Topological sort has only partial ordering: if a depends on c and b depends on d, and there are no other dependencies, a < b or a < d doesn't have to be defined: dbca, dcba, cadb, etc are all valid result.  It's really quite different. --[[User:Ledrug|Ledrug]] 18:55, 10 August 2011 (UTC)
::+1 on leaving as-is. --[[User:Paddy3118|Paddy3118]] 06:48, 11 August 2011 (UTC)

:: What about leaving it here, making sure some of the good notes above (i.e it's about more than just sorting) are in the task description as background (excuse me if they already are), and giving a redirect?  Or perhaps just wait till the new SMW code is in and we can find it through sorting if we like.  --[[User:Dgamey|Dgamey]] 03:02, 30 August 2011 (UTC)

== External link and Captcha problem ==

I am trying to add a Forth entry to the Topological sort page.  Even if I just add the header for the Forth section to the Erlang section, the system tells me that I have added a new external link and I have to solve a captcha.

I first tried without JavaScript: After solving the picture puzzle, the captcha tells me to copy something to an empty box, but there is no empty box, only a filled box; and I also don't see a way to tell the system that I am done.

Then I tried with JavaScript enabled: I have to click on a box and it gives me the green check mark (in one case I had to solve a picture puzzle first), but the page is not saved yet.  When I press "Save Page" again, I get another message about a new link and again a captcha.

I am not sure that this is the right place for such user-interface issues, but I did not find anything that looked more appropriate, and the problem about reporting new links that are not there seems to be specific to this page (it has not occured on other pages I edited).

: I have added a stub Forth entry to the page for you to fill in. (Thanks) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:44, 22 February 2016 (UTC)

Thanks, I have now added the content to the entry.
