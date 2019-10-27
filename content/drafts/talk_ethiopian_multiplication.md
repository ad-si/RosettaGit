+++
title = "Talk:Ethiopian multiplication"
description = ""
date = 2014-02-21T07:01:38Z
aliases = []
[extra]
id = 4901
[taxonomies]
categories = []
tags = []
+++


### Flagged tasks?

It should be noted that definitions (elsewhere and here) refer to this method as multiplying of two integers, other

definitions (referred to here) use the phrase "multiplication of two numbers" where "integers" is meant.   

The method here (may) work for some negative integers, but as generally stated, will work for only positive integers,

depending on whether the multiplier or multipicand is positive.  Almost all of the examples (except for REXX) won't

handle most cases of negative integers.  Some won't work for zeros, but I didn't want to throw stones, as this is

easily fixed. [[User:Gerard Schildberger|Gerard Schildberger]] 22:15, 18 January 2011 (UTC)

==Flagged tasks without separate named routines for half, double, even==
I am sure that most languages could dispense with those requirements of the task spec and create a smaller program, but I think people should refrain and stick to the spec so examples can be easily compared, and the page doesn't get to long. I flagged a number of examples accordingly. It was hard doing this for the x86 assembly - the routine might have been similarly coded in a multitude of assembly languages for machines without multiplication, but in the end - it doesn't answer the task. Sorry. --[[User:Paddy3118|Paddy3118]] 06:34, 19 October 2009 (UTC)

Technically, the task doesn't stipulate that the routines have to be named. Some languages allow the creation of anonymous functions, so the concept of defining a function might not be entirely clear cut. 
-- [[User:Sluggo|Sluggo]] 04:31, 14 November 2009 (UTC)
:You do have to create a function. the intent is for it to be a named function where the name should indicate the functions purpose. I am hoping that most contributors will go with the intent. --[[User:Paddy3118|Paddy3118]] 18:33, 14 November 2009 (UTC)
:: You can clarify the task, and then mark the tasks which don't quite comply. --[[User:Short Circuit|Michael Mol]] 23:51, 14 November 2009 (UTC)

So languages which don't have the notion of functions do what then?  (Consider CHR as an example of such.) [[User:Ttmrichter|Michael T. Richter]] ([[User talk:Ttmrichter|talk]])
:Try your best to work in the spirit of what is being asked for. 
:If you have named macros but not functions then mention that up-front then try using them. If you have include files then include files with appropriate names.
:In short: Do as much as you can and state the reasons for any discrepancies prominently at the beginning. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:01, 21 February 2014 (UTC)

==X86 Smaller version==
I propose rewording this as something like "More representative of assembler languages without the use of explicit multiplication ops", as it already doesn't follow the letter of the task description w.r.t. separate functions, and I would not want it to become some contest in minimalism. How say you? --[[User:Paddy3118|Paddy3118]] 07:03, 11 May 2010 (UTC)


== Similar entries for Unix Shell but also Bash ==
How to amalgamate? --[[User:Paddy3118|Paddy3118]] 12:53, 9 June 2010 (UTC)
:[[:Category:Bash]] should be a [[UNIX Shell]] implementation, not a separate language. Move the Bash examples to the [[UNIX Shell]] section with a {works with|bash} template. --[[User:IanOsgood|IanOsgood]] 15:56, 9 June 2010 (UTC)
::Sorry about that.  Managed to miss the [[UNIX Shell]] cat altogether.  Since the implementation that I posted used proper bit shifts rather than multiplication and division, however, I am going to retrieve it and drop it in as an alternate [[UNIX Shell]] implementation. [[User:Bendingoutward|Bendingoutward]] 07:43, 12 June 2010 (UTC)
:: Strictly, bash is ''mostly'' an implementation of Unix shell, but it does have some peculiarities of its own. If the example is using any of those peculiarities, it should be on its own (but perhaps as a level-3 entry under the L2 Unix shell one). If not, {{tmpl|works with}} is the right thing; having multiple idiomatic implementations of an example is fine, though rarely done because it's a lot of work. :-) –[[User:Dkf|Donal Fellows]] 10:54, 5 January 2012 (UTC)

==OpenOffice Calc==
i did made a version in openoffice calc. but since there is no "sourcecode", i would like to upload the .ods file. how should i do this since this filetype is not allowed as  upload in this wiki? [[User:Elvis|Elvis]] 05:46, 18 June 2010 (UTC)
:Try showing the equations then the values that are in the grid instead. A .ods file would be much harder for someone to compare with other solutions as it is not meant to be readable. See [[Knapsack problem/Unbounded#OOCalc]]. --[[User:Paddy3118|Paddy3118]] 06:53, 18 June 2010 (UTC)
::There is source code if you've used any formulas. Change the cell format to show the formulas, then take a screen-shot as a .PNG and upload that. That's what I hope to do when I get to demonstrating [http://www.mytongue.biz MyTongue].
: CSV export? --[[User:Short Circuit|Michael Mol]] 12:45, 18 June 2010 (UTC)
:: Won't export formulaes. --[[User:Paddy3118|Paddy3118]] 13:05, 18 June 2010 (UTC)
==Unexplained Popularity!?==
Has anyone checked out the popularity of this page at [[Special:PopularPages]]?  It's far and above the most popular single task and second only to the main page.  I wonder why?--[[User:Dgamey|Dgamey]] 11:16, 22 July 2010 (UTC)

:I imagine it is linked from some popular page(s).  --[[User:Rdm|Rdm]] 12:20, 22 July 2010 (UTC)
: According to the Google Analytics data, most of those hits come from [http://www.stumbleupon.com/url/rosettacode.org/wiki/Ethiopian_Multiplication StumbleUpon]. Actually, the traffic surge that hit when SU started sending people to this task forced me to upgrade servers last year. [[Quine]] is the other task that's currently seeing high relative inflow from SU. --[[User:Short Circuit|Michael Mol]] 13:46, 22 July 2010 (UTC)
:SU seems to be a great way to increase traffic. I know of a number of sites and blogs that have seen this. Far better than most similar.--[[User:Dgamey|Dgamey]] 13:56, 22 July 2010 (UTC)
:: It's finicky. They obviously don't like spamming submissions to a site, so people will thumb submissions down for that. Apart from that, it takes a novel page to catch the interest of CS-types on SU. --[[User:Short Circuit|Michael Mol]] 14:35, 22 July 2010 (UTC)
:: Agreed. The occasional self submission is usually fine.  I should probably thumbs up the main page and then lay low for a while.  I'm not consistently on SU so it's time. --[[User:Dgamey|Dgamey]] 21:49, 22 July 2010 (UTC)

::: Michael and I discussed it at the time [http://rosettacode.org/wiki/User_talk:Paddy3118#Ethiopian_Multiplication here]. --[[User:Paddy3118|Paddy3118]] 22:11, 22 July 2010 (UTC)

==Ethiopian??? My foot!==

This is just a straightforward long multiplication algorithm using the binary number system!

If you need a video to explain this, you need to go back to CS for a refresher.

Say you want to multiply  100101 by 1101  okay?   This is the same thing as:

 (100000 * 1101) + (100 * 1101) + (1 * 1101).

See, we have "struck out" the entries that are even (corresponding to the zero digits in the left hand operand).

       1 * 1101 == 1101      //        100101 & 1 == 1 -> keep!
      10 * 1101 == 1101 << 1 // (100101 >> 1) & 1 == 0
     100 * 1101 == 1101 << 2 // (100101 >> 2) & 1 == 1 -> keep!
    1000 * 1101 == 1101 << 3 // (100101 >> 3) & 1 == 0
   10000 * 1101 == 1101 << 4 // (100101 >> 4) & 1 == 0
  100000 * 1101 == 1101 << 5 // (100101 >> 5) & 1 == 1 -> keep!

The algorithm determines which rows to discard by dividing one of the operands repeatedly by by two and testing for even, which is the same thing as shifting right and testing for a 1 bit: i.e. iterating over the binary representation to find the 1's, from the LSB upward. The other operand is multiplied by two to get it into the corresponding bit position.

This is the most basic multiplication algorithm that has been widely implemented in hardware and is also equivalent to long multiplication:

        1101
    * 100101
      ------
        1101
      110100
 + 110100000
  ----------
       <sum>

Same "shift", different pile.[[Special:Contributions/24.85.131.247|24.85.131.247]] 03:50, 9 November 2011 (UTC)
 
: Of course it works out the same, but it's not the usual formulation.  And it's interesting that humans developed it independently of any notion of binary arithmetic (or numerical bases period).  And it is indeed Ethiopian, if not in origin, at least in terms of where this version of the algorithm was "discovered" by the modern West.   [[User:Markjreed|Markjreed]] ([[User talk:Markjreed|talk]]) 20:45, 6 October 2013 (UTC)


== python lambda ==

what is the point of doing:

```Python
halve  = lambda x: x // 2
double = lambda x: x*2
even   = lambda x: not x % 2
```

instead of:

```Python
def halve(x):
    return x//2
 
def double(x):
    return x*2
 
def even(x):
    return not x % 2
```

if a named  function is needed, it doesn't seem to make much sense to use lambda to create it.
lambda is useful for cases where an unnamed function is preferred.--[[User:EMBee|eMBee]] 07:35, 5 January 2012 (UTC)
:shorter? --[[Special:Contributions/76.21.41.59|76.21.41.59]] 09:19, 5 January 2012 (UTC)
