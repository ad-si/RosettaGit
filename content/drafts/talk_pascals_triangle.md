+++
title = "Talk:Pascal's triangle"
description = ""
date = 2018-02-24T12:22:39Z
aliases = []
[extra]
id = 3836
[taxonomies]
categories = []
tags = []
+++

== Right Output Format? ==
The task description says the triangle looks like this:

```txt
   1
  1 1
 1 2 1
1 3 3 1
```


And yet, some examples are showing the easier to construct:

```txt
1
1 1
1 2 1
1 3 3 1
```


I think that maybe all example output should follow the task description format of an isosceles triangle. --[[User:Paddy3118|Paddy3118]] 08:59, 27 December 2009 (UTC)
:That's not always easy to do. I think the important part of the task is the generation of each row. We don't need to complicate it with output formatting that isn't important to the theory involved. --[[User:Mwn3d|Mwn3d]] 18:37, 27 December 2009 (UTC)

::: It seems easy enough to do, just indent:   '''Total Rows-Current Row'''.   -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:12, 2 November 2012 (UTC)

:::: (For further edification and clarification):   indent each displayed row with:     value('''totalRows   ''minus''   currentRow).     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:33, 18 July 2016 (UTC)

::Need it or not, the Python~2 code as available strays from theory by virtue of the return statement. I submit that completeness and conformity trumps raw theory in the case of code examples and would serve an additional segment of the public with formatting applied. I further submit the following as alternative and addition to present code under the potential heading of "Traditionally Formatted, Python~3":
::
```python
def pascal(n):
   # Prints n rows of Pascal's triangle, traditionally formatted.
   # Good for 1 <= n <= 20 rows.
   # Beyond n = 20, digits exceed allotted character space
   # and overlap occurs.
   row = [1]
   k = [0]
   z = n-1
   for x in range(n):
      tabs = '   '*z
      for i in range(len(row)):
         row[i] = str(format(row[i], '6d'))
      print(tabs+''.join( row[i] for i in range(len(row)) ) )
      for i in range(len(row)):
         row[i] = int(row[i])
      row=[l+r for l,r in zip(row+k,k+row)]
      z -= 1
```
Likely, there are more elegant formatting methods. This is to my ability. --[[User:Jnever1|Jnever1]] 04:35, 14 March 2012 (UTC)

::Easy or not, it's what makes Pascal's triangle a ... well, a triangle.  Having the output in a triangle is what shows (easlily) what the relationship is between any number and the two above it, assuming that the "above" numbers in the line are formatted properly. Next thing you know, Latin squares will be acceptable as all the numbers on one line. It should be an easy thing to insert some blanks before/between the numbers. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:07, 14 May 2012 (UTC)

::FWIW my two cents on this is that it's easy enough to tilt your head left 45&deg; :). It's not stated in the problem text, but I saw this task as an exercise in creating the triangle itself, and the extra code to format it properly can obfuscate the more important underlying logic. I agree it's not hard, but even so, for a beginning programmer to sift through which logic applies to which issue can be difficult. I think a separate formatting task would be a great way to address this. --[[User:MikeLorenz|Mike Lorenz]] 13:28, 3 November 2012 (UTC)

== J Explanation ==


```j
!/~@i. N
```


The triangle itself is simply the table of number-of-combinations, for the first N non-negative integers.


```J
   !/~i.5
1 1 1 1 1
0 1 2 3 4
0 0 1 3 6
0 0 0 1 4
0 0 0 0 1
```


That is, [[wp:Combination|C(n,k)]] for all <tt>n,k in [0 .. n)</tt>.  J's notation for <tt>C(n,k)</tt> is <code>k ! n</code> (mnemonic: combinations are closely related to factorials, which are denoted by <tt>!</tt> in math).

So, for example, the number of ways to choose a poker hand (5 cards from the deck of 52):

```j
   5!52
2598960
```


So <code>!</code> is the mathematical choose function.  As for <code>/~@i.</code>, the <code>/~</code> would be "table of" and <code>i.</code> "the first N non-negative integers (i.e. 0 .. N-1)".  (And <code>@</code> is just glue.)

But, formatting the thing takes a bit more effort:


```j
   (-@|. |."_1 [: ;:inv [: ":@-.&0&.>@|: !/~)@i. 5
     1
    1 1
   1 2 1
  1 3 3 1
 1 4 6 4 1
```


Here, we have refactored the operation slightly:


```j
(stuff)@i.5
```
 makes the sequence 0 1 2 3 4 available every odd verb (counting from the right) in stuff

The "stuff" are (from right to left)

```j
!/~
```
 build our table of combinations

```j
":@-.&0&.>@|:
```
 transpose that array, and for each number remove it if it is zero, format it as a string and put it in a box

```j
[:
```
 placeholder meaning "no verb here" the verb to the right does not get a left argument

```j
;:inv
```
 combine the strings in rows of boxes as word (with spaces between them

```j
[:
```
 placeholder meaning "no verb here" the verb to the right does not get a left argument

```j
|."_1
```
 rotate each item (each row of characters) by the indicated amount (rotation would be moving contents left except that the amounts are negative).

```j
-@|.
```
 reverse and negate the argument (resulting in the negative sequence _4 _3 _2 _1 0)


== first imagined by Pascal? are you sure? ==

In the very first line of the description it's said:

```txt

Pascal's triangle   is an arithmetic and geometric figure first imagined by   Blaise Pascal.

```


In addition Pascal drown it as a square triangle while all former representations were isosceles: in the task is shown as isosceles so it is no way the Pascal's triangle (see also the discussion above).

The wikipedia page linked shows a chinese document dated 1303 and the text:

```txt

 The title reads "The Old Method Chart of the Seven Multiplying Squares"

```



here an excerpt from the introduction as can be found in my [https://github.com/LorenzoTa/Tartaglia-s-triangle program about the triangle]


```txt

In Italy, the arithmetic triangle is called Tartglia's triangle, because exposed in the "General trattato di numeri et misure" written in 1556 by Niccolò Fontana (1499 ca, Brescia 13 December 1557, Venice), known also as Tartaglia.
...
Known as Pascal's triangle (but Pascal drown it as right triangle) in many other countries was known by Halayuda, an Indian commentator, in 10th century, studied around 1100 by Omar Khayyam, a Persian mathematician, known in China as early as 1261 and so studied in India, Greece, Iran, China, Germany and Italy before Pascal.

```


So I definitively vote to change the first sentence from:

```txt

Pascal's triangle   is an arithmetic and geometric figure first imagined by   Blaise Pascal.

```

to

```txt

Pascal's triangle is an arithmetic and geometric figure named, in most of the western countries, after Blaise Pascal.

```

:Why "in most of the western countries"? Are there countries where this is another Pascal? No. It's named after Blaise Pascal, period. That does not imply Pascal was the inventor, or that there are no other names. Likewise, the "triangolo di Tartaglia" is named after Niccolò Tartaglia, and no other Tartaglia (and not Pascal, obviously). [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 01:06, 23 February 2018 (UTC)

::you are absolutely right [[User:Eoraptor|Eoraptor]] I just reported a 'neutral' sentence from [https://en.wikipedia.org/wiki/Pascal%27s_triangle wikipedia]:

```txt

In much of the Western world, it is named after French mathematician Blaise Pascal, although other mathematicians studied it centuries before him in India,[1] Persia (Iran), China, Germany, and Italy.

```


::I just mean with my sentence that there are also western country as Italy for example where it's called otherwise. The triangle has many names: Staircase of Mount Meru, Khayyam triangle, Yang Hui's triangle, Tartaglia's triangle and obviously Pascal's triangle.

::I'm not able to reproduce a geographical distribution of different names: what i proposed was for sure an approximation but byfar better than the original, wrong, one. --[[User:LorenzoTa|LorenzoTa]] ([[User talk:LorenzoTa|talk]]) 08:20, 23 February 2018 (UTC)
:::You are right, and I perfectly agree with the edit. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 12:21, 24 February 2018 (UTC)
