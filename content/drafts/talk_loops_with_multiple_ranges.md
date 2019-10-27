+++
title = "Talk:Loops/with multiple ranges"
description = ""
date = 2018-09-16T10:51:55Z
aliases = []
[extra]
id = 21992
[taxonomies]
categories = []
tags = []
+++

==Further explanation needed==
You can't expect RC coders to understand your programming language. You shold convert to some form of pseudo-code and explanation in English. 

It would also help if you went step-by-step through a single, two and maybe three ranges should produce - in a mix of pseudo-code and English. Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 19:05, 15 September 2018 (UTC)

: I thought that a   '''do'''   loop would be self explanatory   (even with such an old language as PL/I, conceived in 1963 or earlier as a possible extension to FORTRAN, announced in 1964, delivered in 1966)   with the same degree that when a   '''for'''   loop is used in pseudo-code,   but is never described.   An astonishing number of pseudo-code on Wikipedia and Rosetta Code uses (a)   '''C'''   (or '''C'''-type)   language,   but I have yet to see a   '''for'''   loop explained/described in the form of pseudo-code.   One reason that multiple ranges for the   '''do'''   loop were chosen for this task (along with expressions in the various ranges) was to ensure that short-cuts weren't chosen to bypass the logic and expressiveness of the original code   (for maintainability and range expanding, for example),   and also to have some method of verification that the various ranges were indeed, executed correctly.   One such concern was:  

         do j=1  to 100,   200  to 300;
         say j;
         end;

: ─── being executed as:

         do j=1  to 300
         if j>100  &  j<200  then iterate
         say j
         end

: ─── or:

         do j=1  to 300
         if j<=100 | j>=200  then say j
         end

: (both of which are slightly harder to comprehend as compared to the original case code, and harder to modify if the original ranges are changed or their "step" (incrementation) values are either modified or added.)   Also of note is that in the later two cases, the variable   '''j'''   will have more values assigned to it over the execution of the   '''do'''   loop.
 
: The main intent was to show how various languages would or could utilize multiple ranges of   '''do'''   loops,   not to show how a   '''do'''   loop works.   I had assumed that almost everyone knew how   '''do'''   loops worked,   it was the use of multiple ranges that was the point of this task.     I had assumed that once other popular languages were entered (most of which would probably use a   '''for'''   construction or something similar),   that other people would understand how a   '''do'''   loop worked.   I had carefully chosen the ranges to   ''not''   use a range that some languages would behave differently   (which would make a nice task addition to Rosetta Code, by the way ─── this is one area that is seldom explored when comparing languages here on Rosetta Code). 

: For instance:
         n=75
              do j=100  to n
              say j   
              end
: ─── where some languages will not produce any output   (not testing the range before execution of the '''do''' loop at least once),   other languages (such as PL/I and REXX) will test at the start (head) of the   '''do'''   loop.

: However, I'll take a stab at adding pseudo-code to explain   '''do'''   loops as used by the PL/I language.   By the way, PL/I isn't my language, it's been years since I even had access to a PL/I compiler, that is the only reason that I didn't enter a PL/I example (here and in other Rosetta Code tasks).   If I had access to a PL/I compiler, I'd be entering numerous PL/I examples;   PL/I syntax is very similar to REXX's, and indeed, REXX borrowed a lot of PL/I's syntax and PL/I's BIFs.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:26, 15 September 2018 (UTC)

Hi Gerard, I would first like to say that after first reading the task description:
* I really liked what seemed to me the central idea of the task: looping over multiple ranges with only the single loop body.
* I didn't like the initial reliance on the PL/I code; although PL/I served the crucial requirement of making you come up with the idea, I thought that its variable initialisation and explicit language syntax didn't add to the description.

I was also lazy and didn't try and give an example for discusion of my ideas. 
;Here's more of what I was thinking of:

Some languages allow for the easy looping around a single block of code using a variable that iterates through several ranges  of values in succession. In pseudocode:

```txt

LOOP variable J over range(-3 to 9 by  3) then range(30 to 24 by -2) then range(startfunc(x) to stopfunc(x) by stepfunc(x))
BEGIN
    /* do_loop_body */
    print_variable_followed_by_space(J)
END; /* LOOP */
print_newline()
```


The PL/I code could be rewritten in such pseudocode, or the task itself altered to ask for not much more than what is stated above. (I would need to add that ranges are inclusive of endpoints and what the three funcs do when computing a ranges limits).

What do you think Gerard? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:07, 16 September 2018 (UTC)


: First, I used the initializations of several variables to show that values (and expressions) should be able to be used within the constructs shown.   This by-passed the static or hard-coding of values within the iteration ranges.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:37, 16 September 2018 (UTC)


: I definitely wasn't thinking of an easy way to loop around a block (or blocks) of code, if fact, this is exactly what I was trying to avoid, that is, avoiding logic which avoids going around code (to avoid execution of some code ─── this seems/appears to an anathema to structured programming)   by some kind of artificial construct,   or even using logic to chose which block to execute (or not execute).   Specifically, I wanted a "pure"   '''do'''   construct   (or   '''for'''   construct)   if possible.   I was only interested in methods that directly stated what was intended  (that is, if it was possible to write code in that manner).   I had, in fact, originally wrote the REXX version to do exactly what the PL/I language used, but it involved writing a function to essentially parse (scan) for the appropriate clauses   ('''to''', '''by''', '''which''', '''for''', '''until''', ...   in any order)   and execute the   '''do'''   index variable accordingly ─── but the amount of code seemed to be a tad bit bulky and had to deal with too many (error/omission) issues, and it made the program hard to follow and understand, defeating the purpose of a (clean-looking)   '''do'''   loop with multiple (iteration) ranges.   I may end up including it as an alternative solution with the usual "your mileage may vary" caveat.   I have often come across the need for a multiple-range   '''do'''   loop structure, and once you've used it, it comes very natural to think (and code) in that manner,   --- but in any case, it's easy to program around any deficiency or short-coming of a language, but I wanted to see how other programming languages dealt with such an animal, or failing that, how programmers dealt with that type of logic to perform the same function/need.   One thing that I definitely wanted was to use expressions instead of hard coded start/stops/increments   (or however one wanted to express such a range construct).   I think using functions   (as one form of expressions or within expressions)   really muddies up the waters   (but it covers the ground!)   when writing pseudo-code, even in a general manner such as this.   I had assumed that programmers could understand the use of functions instead or embedded within an expressions, but I didn't think that would make it any clearer.   Also note that I didn't assume that many languages supported such a structure, so part of the Rosetta Code's task requirement stated that other "work-arounds" may be used if   '''do'''   loops with multiple ranges weren't possible.   I also had to deal with several types (kinds) of   '''do'''   loop (incrementing/decrementing) ranges, and also the "step" or   '''by'''   values, and to make sure that expressions could be used instead of hard-coded values, and to also ensure that every computer programming solution did the exact index enumerations, so I had to Jerry-rig a method to validate the   '''do'''   index    (in this case,   '''j''').   I had to add a somewhat complicated   '''if'''   statement so that the product wasn't zero, and also so that the product didn't exceed the limit(s) of an integer for most programming languages.   I was going to have the programming examples just display all the values of the   '''j'''   index, but it takes a lot of eyeballing to verify a list of numbers   (even a horizontal list),   whereas observing just two values makes it easy to verify that the   '''do'''   loop index was incremented and/or decremented correctly.   I threw in the commatizing the two (output) results as a more-or-less optional requirement for programmers that wanted to walk an extra few paces ─── I always appreciated those Rosetta Code tasks that had an optional or "extra" credit thingy.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:37, 16 September 2018 (UTC)

==other DO loop cases==
Another good tasks for Rosetta Code would be the case of how a language treats the following code snippets:
<lang>k= 5
L= 2    
       do j=k  to  L 
       say  j
       end
```

and show what values  (if any) are displayed.

Or, another case:
<lang>k= 5
L= 2    
       do j=L  to k  by -2 
       say  j
       end
```


Or, yet another case:
<lang>k= 5
L= 2    
       do j=L  to k  by -2 
       say  j
       k= 9
       end
```

But I don't want to go through explaining those in pseudo-code.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:56, 16 September 2018 (UTC)

:You've got me going now ... :-)
 [[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 08:12, 16 September 2018 (UTC)

::See [[Loops/Wrong ranges]] (A bit of a tongue twister ;-)
[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:32, 16 September 2018 (UTC)

::: You could add several more.  
             a  DO  with no  TO
             a  DO  that increments not one variable,  but a list of variables
             a  DO  with no  BY
             a  DO  that stops and stops on the same value  (it is within range),  but when is the index checked?      
::: -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 10:50, 16 September 2018 (UTC)
