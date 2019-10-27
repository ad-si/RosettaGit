+++
title = "Talk:Price fraction"
description = ""
date = 2010-11-06T17:58:13Z
aliases = []
[extra]
id = 6444
[taxonomies]
categories = []
tags = []
+++

== Table extension ==
What do you get for an input price of 0.99 or 1.00? --[[User:Paddy3118|Paddy3118]] 09:56, 15 March 2010 (UTC)
:That's covered by the final case: <code>&gt;=  0.96  &lt;  1.01  :=  1.00</code> which is handled in the Clipper code with 
<code>ELSEIF npQuantDispensed >= .96
    nResult = 1
</code>
:--[[User:Axtens|Axtens]] 02:36, 16 March 2010 (UTC)

Ah, I get it. You just forgot to add it to the table in the task descrition [http://rosettacode.org/mw/index.php?title=Price_Fraction&diff=77456&oldid=77453 here], and I just looked and saw that the Clipper table had the same entries as the original table and assumed that what was missing was what I then added. (Which in this case just happened to match the Clipper code).

<small>Damn! I would have liked to save that luck for the lottery :-)</small>
 --[[User:Paddy3118|Paddy3118]] 06:35, 16 March 2010 (UTC)

:Actually, the case was always there and the redundant bit of code as well. All you've done is to point out that there's pudding between my ears. --[[User:Axtens|Axtens]] 08:10, 16 March 2010 (UTC)

== What about invalid values? ==

What's supposed to happen if you get a negative number, or a number greater than 1.01? -- [[User:Eriksiers|Eriksiers]] 19:13, 15 March 2010 (UTC)
:The calling routine takes care of those details --[[User:Axtens|Axtens]] 02:30, 16 March 2010 (UTC)
::Okay, but if an illegal value gets passed in anyway, what should we do? Return the number as-is? Zero? Error? -- [[User:Eriksiers|Eriksiers]] 20:04, 19 March 2010 (UTC)

:::Hi Eriksiers, the task description begins: "Task: Given a floating point value between 0.00 and 1.00 ...", so I would assume we don't have to handle it. --[[User:Paddy3118|Paddy3118]] 11:52, 20 March 2010 (UTC)
::::Okay, then I'll leave the BASIC example the way it is and not worry about it. -- [[User:Eriksiers|Eriksiers]] 00:33, 22 March 2010 (UTC)

== Floating point for money?! ==

Floating point for money? Don't they teach kids anything these days? --[[User:IanOsgood|IanOsgood]] 20:15, 15 March 2010 (UTC)
:I've had that exact conversation with a friend in the past few weeks. All I can say is... I (at least) never think of the problems, or the alternatives. -- [[User:Eriksiers|Eriksiers]] 20:44, 15 March 2010 (UTC)
:: Two topics on the original wiki, [http://c2.com/cgi/wiki?FloatingPointCurrency Floating Point Currency] and [http://c2.com/cgi/wiki?MoneyObject Money Object], contain many anecdotes, cautionary tales, and useful links. For a toy problem like this, using an integer in units of cents should suffice. --[[User:IanOsgood|IanOsgood]] 01:29, 16 March 2010 (UTC)
:::All good points. However, (1) I'm the maintenance programmer, and (2) to work in integers would require a significant rewrite, something I have neither the time nor the patience for (I don't get paid for the work, and have to fit it in wherever and whenever possible.) --[[User:Axtens|Axtens]] 02:30, 16 March 2010 (UTC)
::::Note that once you have a function which works with integers it should be trivial in most languages to wrap it with something which multiplies floating point numbers by 100 and gets the nearest integer and divides the result by 100. Some languages also let you mark the floating point variant as "obsolete" with a message that points at the other approach.  --[[User:Rdm|Rdm]] 19:20, 18 March 2010 (UTC)
:::::That said, there are some efficiency problems with that approach.  There are several ways of dealing with these problems.  Probably the best would be to coerce numbers to integer values before doing arithmetic on them and convert them back after.  If you implement this approach the storage system should probably complain if it is ever given values which deviate too much from values which could plausibly result from this approach -- this would be run-time detection of invalid code.  --[[User:Rdm|Rdm]] 14:25, 17 May 2010 (UTC)

== Representing the data ==

One issue, with this kind of problem, has to do with the form that potential future changes might take.  This touches on some perhaps interesting topics.

For example, consider the J implementation:


```J
le=: -0.96 0.91 0.86 0.81 0.76 0.71 0.66 0.61 0.56 0.51 0.46 0.41 0.36 0.31 0.26 0.21 0.16 0.11 0.06 0
out=: 1 0.98 0.94 0.9 0.86 0.82 0.78 0.74 0.7 0.66 0.62 0.58 0.54 0.5 0.44 0.38 0.32 0.26 0.18 0.1
```


This, of course, works.  But perhaps a better approach might be to simply incorporate the text of the original table in the program.  The TCL implementation did this.  A J version of this approach might be:


```J
'g1 out1'=:|:|.(1 5 { 0 ". ]);._2]0 :0
>=  0.00  <  0.06  :=  0.10
>=  0.06  <  0.11  :=  0.18
>=  0.11  <  0.16  :=  0.26
>=  0.16  <  0.21  :=  0.32
>=  0.21  <  0.26  :=  0.38
>=  0.26  <  0.31  :=  0.44
>=  0.31  <  0.36  :=  0.50
>=  0.36  <  0.41  :=  0.54
>=  0.41  <  0.46  :=  0.58
>=  0.46  <  0.51  :=  0.62
>=  0.51  <  0.56  :=  0.66
>=  0.56  <  0.61  :=  0.70
>=  0.61  <  0.66  :=  0.74
>=  0.66  <  0.71  :=  0.78
>=  0.71  <  0.76  :=  0.82
>=  0.76  <  0.81  :=  0.86
>=  0.81  <  0.86  :=  0.90
>=  0.86  <  0.91  :=  0.94
>=  0.91  <  0.96  :=  0.98
>=  0.96  <  1.01  :=  1.00
)
priceFraction=: out {~ (-g) I. -

```


The problem with this approach, though, is that it invites the casual reader to think the code is doing more than it is.  Are we prepared to deal with different sorts of expressions in this table?  The answer should be no, unless the table was designed for use by our language or unless we have designed that facility to work properly.

Another problem with this approach is bulk.  This table is somewhat regular and that regularity could be used to provide compact definitions of these required constants:


```J
out=: |.+/\1 2 4 12 1 0#0.02*i.-6
le=: -0.01+0.05*i.-#out
priceFraction=: out {~ le I. -
```


The problem, here, is that a future "casual maintainer" might not be able to figure out how to modify this table.  This might be seen as an advantage or a disadvantage, depending on the likely value and role of casual maintainers.

In some contexts, the specification would not be set in stone but would be a part of a discussion between the person (or people) developing the code and the people that need it to do something useful.  In this sort of environment, both parties would have valuable insight to offer.

In an "strongly specified" sort of problem, the specification would be set in stone and the developers' insights would not be relevant to the specification.
: I'd be open to standardizing on CSV as a format for tabular data, and hold a strong recommendation that tasks that make use of tabular data present it in CSV, and that examples that make use of it import it from such.  It depends on it being fairly portable; one would be increasing the complexity of every task to including either processing stream input or file input. --[[User:Short Circuit|Michael Mol]] 21:36, 18 March 2010 (UTC)
:: Real code parses it in whatever idiotic form the local legislature decides to publish it in. These days, that means whatever format one gets when one cuts-n-pastes it out of some PDF… –[[User:Dkf|Donal Fellows]] 22:27, 18 March 2010 (UTC)
: The issues raised are certainly interesting. However after the discussion of the merits of two alternative forms for specifying the table data, it seems to me that the data format chosen by the J implementation (and others) is a nice compromise between them - easily understandable/editable by the "casual maintainer" (especially when the values are lined up) and not bulky or misleading. 
:While I think the idea of standardizing on CSV for presenting tablular (and list?) data to be used in a task has some merit, I'm not sure that adding routines to all such tasks for reading/parsing it from a CSV file is desirable. I'm pretty sure there are already tasks for reading and writing files in CSV format.--[[User:Tikkanz|Tikkanz]] 22:48, 18 March 2010 (UTC)

:If the government supplied data then I would copy their description into the program and add a link to the original source too. When things change, the maintainer would have both before and after sources to decide on what has changed and then to implement that change. on how the routine is coded: after learning about bisect it seems right for the job and leaves the raw range and output price data in a nicely displayed way. A competent maintainer who didn't know the bisect model should be able to learn to use it from the Python documentation and a little experimentation as its use in the Python example is straight-forward. 

:CSV has its uses but this table of regions I find OK for this task, but see no need to parse it as part of performing the task. --[[User:Paddy3118|Paddy3118]] 09:42, 19 March 2010 (UTC)
::Given the regularity of the data, I'd go for TSV (tab-separated values) rather than CSV (comma-separated values). TSV's whitespace makes things clearer than a comma. --[[User:Axtens|Axtens]] 13:48, 20 March 2010 (UTC)
:::I thought about that, but realized that CSV has broader existing support in spreadsheet tools and the like. --[[User:Short Circuit|Michael Mol]] 14:40, 20 March 2010 (UTC)
::::Excel reads tab-delimited data. IIRC, give it a .TXT file with TSVs and it parses them into columns automagically. --[[User:Axtens|Axtens]] 14:50, 20 March 2010 (UTC)

The goal of the task description is to communicate to human programmers, not programs. CSV/TSV are textual, which is good; but they don't convey as much information as the explicit depiction of the ranges that we have now. Yes it may be harder to cut-n-paste values into the source for your program, but that use is secondary. When all the examples are completed, you want a task desccription that reads well. --[[User:Paddy3118|Paddy3118]] 11:26, 21 March 2010 (UTC)
:Good point and well made. --[[User:Axtens|Axtens]] 14:25, 21 March 2010 (UTC)
::<blush>I try.</blush> --[[User:Paddy3118|Paddy3118]] 16:16, 21 March 2010 (UTC)

== Rounding? ==
How conversion [0.0,0.06[->0.1, [0.06,0.11[->0.18 can be named ''rounding''? It is certainly not, because any rounding function must be idempotent, i.e. ''f''(''f''(''x''))=''f''(''x''). The function defined by the table violates this requirement. Consequent "rounding" yields: 0.05->0.1->0.18->0.32 and so on. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 18:22, 6 August 2010 (UTC)

: I changed the task description to remove the word "rounding".  --[[User:Rdm|Rdm]] 18:33, 6 August 2010 (UTC)

== The task cannot be implemented ==
Another issue with this task is that all provided implementations are broken. The problem is that some of the values from the table simply cannot be represented in the floating-point format based on either 2 or 16 radix. This covers basically all known modern machines. Since these values are non-existent the task cannot be implemented. The moral: don't use floating-point numbers in place of the fixed point ones. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 18:22, 6 August 2010 (UTC)

: These values certainly can be implemented.  If your language's floating point implementation does not include any provisions for representing decimal fractions using floating point you can multiply the numbers by 100 and use integers, internally, in your code.  (Multiplying by 100 and rounding would be good practice, also, if you were performing sums on these numbers.)  --[[User:Rdm|Rdm]] 18:28, 6 August 2010 (UTC)

::That would not help, because you still cannot compare non-existent numbers. The problem will persist so long the input is required to be floating-point. You cannot input 0.6, or 0.600000000000001, or 0.599999999999 for that matter. The technique you described is in essence what ''fixed-point'' numbers are. Does the task require floating-point numbers or permits any numeric representations? What is the required accuracy? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 18:50, 6 August 2010 (UTC)

:::Then I am afraid I do not understand your claim.  But if you could give me a specific example which you believe fails, I can try it in the [[Price_Fraction#J|implementation I wrote]] and see if it works or not. --[[User:Rdm|Rdm]] 19:07, 6 August 2010 (UTC)
::::Since your implementation seem to use floating-point numbers, it fails to return the value 0.26 as the task requires. Float "0.26f" is not 0.26, because the latter cannot be represented as an IEEE 754 float. The problem would disappear if the task stated the accuracy of the result and the comparisons. E.g. 0.26 +/- 0.0005. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 19:23, 6 August 2010 (UTC)
:::::Well, if you look close enough, we are not returning numbers at all -- we are dealing with bit patterns, and it is up to us to determine what they represent.  Personally, I would claim that the result I am returning for this case represents 0.26.  <lang>   priceFraction 0.15
0.26
```
--[[User:Rdm|Rdm]] 19:27, 6 August 2010 (UTC)

: Not quite Dmitry
:# You would have to demonstrate that floating point math would fail for the particular case of this range of values and "cut points", remember, we are dealing with whole cents or their representation as fractions of a dollar up to less than two dollars.
:# I mentioned this point and provided an integer Python solution that scaled input to be in cents.
:--[[User:Paddy3118|Paddy3118]] 18:35, 6 August 2010 (UTC)

::This is what I meant. It is not floating point, it is fixed point, and the required accuracy is 0.01. So? --[[User:Dmitry-kazakov|Dmitry-kazakov]] 18:50, 6 August 2010 (UTC)

I have changed my mind. If the task is about money, and its says use floating point, then I would rather it be changed to not mention floating point at all as the phrase "floating point is the wrong type to use with money" is a very good phrase to follow. --[[User:Paddy3118|Paddy3118]] 19:34, 6 August 2010 (UTC)
