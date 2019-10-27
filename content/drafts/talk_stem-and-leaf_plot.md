+++
title = "Talk:Stem-and-leaf plot"
description = ""
date = 2016-06-04T00:58:56Z
aliases = []
[extra]
id = 5221
[taxonomies]
categories = []
tags = []
+++

==WP Confusing==
After trying to implement something, I thought the WP article to be less than helpful. How about basing the task on [http://mathforum.org/library/drmath/view/54392.html this]? --[[User:Paddy3118|Paddy3118]] 07:32, 14 December 2009 (UTC)

: Agreed that the WP article is confusing (perhaps one of us ought to improve it!) but I think I've extracted the key part to make the Tcl solution. It even handles signed fractional data, should that be presented. I ''think'' it is easier to convert the data to stems and leaves and store in a map (from stems to a sequence of leaves) before sorting any of it. Or at least that was what made sense to me. –[[User:Dkf|Donal Fellows]] 09:37, 14 December 2009 (UTC)

: Short question aside: How should missing stems be handled? One example on your page lists stems without leaves in between the stems that have leaves. For An overview of the data distribution this is certainly of value but it can make the plot needlessly long. Maybe it should be clarified whether stems without leaves should be represented in the output or not. —[[User:Hypftier|Johannes Rössel]] 14:12, 14 December 2009 (UTC)

::As WP says: "It is important that each stem is listed only once and that no numbers are skipped, even if it means that some stems have no leaves." This ensures that the vertical direction is always a linear scale. This is not just a table: it is an information graphic. Distances matter. —[[User:Kevin Reid|Kevin Reid]] 14:42, 14 December 2009 (UTC)

==Less Useful for negative numbers?==
For data: 15 14 3 2 1 0 -1 -2 -3 -14 -15
Would you generate either graph1:
  -2 | 5 6
  -1 | 7 8 9
   0 | 0 1 2 3
  10 | 4 5
Which satisfies X | Y where 10*X + Y is a datum, but the digit for Y when the datum<0 is not necessarily the right-most digit of the datum.

Or graph 2:
  -1 | -4 -5
   0 | -3 -2 -1  0  1  2  3
  10 |  4  5
Which also satisfies X | Y where 10*X + Y is a datum, the last digit is preserved, but I don't like the negative leaf numbers. 

What thinketh though? --[[User:Paddy3118|Paddy3118]] 13:38, 16 December 2009 (UTC)

Per Wikipedia, you generate a -0 stem, not a -1 stem, before the 0 stem, and use the actual digits -- that is, for your data
 -1 | 4 5
 -0 | 1 2 3
  0 | 0 1 2 3
  1 | 4 5
However, in general the choice of what goes in the stems and the leaves is a choice for whatever best illustrates the particular data set. In ''this task'', there are no negative numbers. 

Also, your second example is problematic because the 0 stem contains a wider span (19 values, -9..9) than the 10 of every other stem, so it distorts the data. On the other hand, the -0 strategy means that the -0 stem has a range of -9..-1 with only 9 elements whereas everything else has 10. I like your first example for uniformity but it seems confusing to read which sort of defeats the point. —[[User:Kevin Reid|Kevin Reid]] 13:50, 16 December 2009 (UTC)

:Hi Kevin, I am unhappy with all forms. The WP -0 stem means that the stems don't form a linear progression between values. They all seem to jar in some way. --[[User:Paddy3118|Paddy3118]] 14:50, 16 December 2009 (UTC)

:I generally agree, but that's a problem with the stem-and-leaf concept, not a problem with this task. The point of this task, as I designed it, is an exercise in producing a visual layout of information -- which I have arbitrarily chosen to be stem-and-leaf ''with positive values''. So I hereby declare this discussion to be irrelevant to the task! There! All settled! <tt>:-)</tt> —[[User:Kevin Reid|Kevin Reid]] 15:03, 16 December 2009 (UTC)

:: Agreed, and it's notable that this is a style of plotting that's fallen out of favour. From the perspective of RC tasks though, the main problem was that it was very poorly described in the first instance. We could do with a test dataset that forces correct handling of negative numbers too. –[[User:Dkf|Donal Fellows]] 15:07, 16 December 2009 (UTC)

:: I would prefer we leave out negatives. Stem plots look crap for negative numbers. --[[User:Paddy3118|Paddy3118]] 08:18, 18 December 2009 (UTC)

::: I rather agree about leaving out negatives, it does complicate the programming.   It took a few extra REXX statements to handle the case of   '''-0'''   and   '''0'''   to be handled as separate indices.   It would be nice to have a companion Rosetta Code task to handle negative numbers   (and also zeroes).   An '''extra credit''' at this late point would be a day late and a dollar short.   '''But''', I disagree about the stem plots looking like crap for negative numbers.   See the output for the 2<sup>nd</sup> REXX version.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:57, 4 June 2016 (UTC)

:: Should I appear smug at this point? ;) --[[User:Glennj|glennj]] 16:09, 16 December 2009 (UTC)

==Data set==
Kevin, is this task still under development? From your comments here, it sounds like it's pretty much fixed. --[[User:Glennj|glennj]] 16:09, 16 December 2009 (UTC)

: It needs a data set which is less sparse -- more obvious humps, and less unnecessary completely-blank stems. That's all. —[[User:Kevin Reid|Kevin Reid]] 17:05, 16 December 2009 (UTC)
:: Now that I'm back at work again, I'm again short on time for everything. See the data generator linked to in the class. Adding additional calls to genhump will add more humps. Tweaking the parameters will adjust the width, location, height and shape of the humps. Run it until a suitable dataset is found.  Or provide several data sets, testing and highlighting different behaviors. --[[User:Short Circuit|Michael Mol]] 19:26, 16 December 2009 (UTC)
::: Here is an alternative data set matching those specs:

```txt
176  86 160 210  76 205 186  67 154 192  96 187
 83 110 112  90 247  91 108  93  88 133 159 104
109 173 167  52 225  73  82 198 114  69 183 166
 59 209  74 267  74 211 118  68 105 221  67 233
243 109 226  66 179 201  61  84  93  83 181  88
```

::: Generated using:

```j
   require 'stats/distribs/normal'
   randomize=: ] /: 0 ?@$~ #
   showStemLeaf dat=: (84 20,:184 40) ([: randomize ,@:(<.@:rnorm"1 0)) 30 30
┌──┬─────────────┐
│ 5│2 9          │
│ 6│1 6 7 7 8 9  │
│ 7│3 4 4 6      │
│ 8│2 3 3 4 6 8 8│
│ 9│0 1 3 3 6    │
│10│4 5 8 9 9    │
│11│0 2 4 8      │
│12│             │
│13│3            │
│14│             │
│15│4 9          │
│16│0 6 7        │
│17│3 6 9        │
│18│1 3 6 7      │
│19│2 8          │
│20│1 5 9        │
│21│0 1          │
│22│1 5 6        │
│23│3            │
│24│3 7          │
│25│             │
│26│7            │
└──┴─────────────┘
```

--[[User:Tikkanz|Tikkanz]] 01:22, 18 December 2009 (UTC)

== "do better if you can"??? ==

What on earth does that mean? Can the originator of that part please explain what they had in mind so that we can determine how best to provide output to satisfy the task? –[[User:Dkf|Donal Fellows]] 00:08, 17 December 2009 (UTC)

:The task is an exercise in producing a visual layout of information. Think about what wouldn't look out of place  in a book or article. If you choose monospaced text, it might work, but you lose a lot of options about particular spacing and vertical rule style (and you may not even be able to make an unbroken line, depending on the text renderer). —[[User:Kevin Reid|Kevin Reid]] 00:45, 17 December 2009 (UTC)

:: But if you've read the WP article on these things, you'll know that they were only ever popular when created using monospaced fonts. Going for anything else... it's not really in the spirit of the things. –[[User:Dkf|Donal Fellows]] 10:10, 18 December 2009 (UTC)

I too have problems with this. You need mono-spaced text to get the height  (width) of each line right. If rendered in, say, HTML then I guess you could use a highlight colour to create solid bars of colour. --[[User:Paddy3118|Paddy3118]] 11:33, 25 December 2009 (UTC)
