+++
title = "Talk:Menu"
description = ""
date = 2018-09-08T17:59:18Z
aliases = []
[extra]
id = 4316
[taxonomies]
categories = []
tags = []
+++

==Naming==
And there I was thinking it was named after the select() syscall... —[[User:Dkf|Donal Fellows]] 20:17, 3 June 2009 (UTC)

I am so glad I added the note on where the name is from :-)

--[[User:Paddy3118|Paddy3118]] 21:22, 3 June 2009 (UTC)
:Good task BTW. Nice mix of data and I/O to do something user-focussed. —[[User:Dkf|Donal Fellows]] 22:36, 3 June 2009 (UTC)

::<blushes/>. --[[User:Paddy3118|Paddy3118]] 08:49, 4 June 2009 (UTC)

==J implementation==


### Code removed

because it didn't work in Beta-3: commercial/2017-04-10T17:51:14 and was flagged for improvement.

```j
require 'general/misc/prompt' NB. in older versions of J this was: require'misc'
showMenu =: i.@# smoutput@,&":&> ' '&,&.>
makeMsg  =: 'Choose a number 0..' , ': ',~ ":@<:@#
errorMsg =: [ smoutput bind 'Please choose a valid number!'
 
select=: ({::~ _&".@prompt@(makeMsg [ showMenu)) :: ($:@errorMsg)
```

See [[Talk:Select#J_implementation|Talk page]] for explanation of code.

'''Example use:'''

```j
   select 'fee fie'; 'huff and puff'; 'mirror mirror'; 'tick tock'
```


This would display:

 0 fee fie
 1 huff and puff
 2 mirror mirror
 3 tick tock
 choose a number 0..3:

And, if the user responded with 2, would return: <tt>mirror mirror</tt>

--LambertDW 17:59, 8 September 2018 (UTC)


Note: this description of the code is based on an older version (the line below) and is now out of sync with the copy on the main page.

Here are some introductory notes for people not familiar with J, who might care about the J implementation.


```J
select=: ({::~ 'choose a number 0..' 0&".@prompt@, ': ',~ ":@<:@# [ i.@# smoutput@,&":&> ' '&,&.>) :: (select@([ smoutput bind 'please choose a valid number'))
```


This statement consists of several parts:

   select ({::~ ....prompt user... [ ...provide reference...) :: (...error handler...)

And, as written, it expects the user is displaying the code in a short, wide window (or that they can resize their window that way).


### Provide reference table


The first part of the code to be executed is:

   i.@# smoutput@,&":&> ' '&,&.>

This expression consists of three parts:
    ' '&,&.>
prepends a space on each of the strings from the argument:

    ' '&,&.> 'fee fie'; 'huff and puff'; 'mirror mirror'; 'tick tock'
 ┌────────┬──────────────┬──────────────┬──────────┐
 │ fee fie│ huff and puff│ mirror mirror│ tick tock│
 └────────┴──────────────┴──────────────┴──────────┘

Specifically, ' ' represents the space character, and a comma is J's concatenation operator.  ' '&, curries the concatenation operator forming a new function which always prepends a space to its argument.  The two character token &. combines two functions:  f&.g y is equivalent to G(f(g(y))) where G is the inverse of g.  The function > in a monadic context extracts the contents of a box (dereferences a pointer), and its inverse puts its argument in a box (gets a pointer to its argument).

   i.@#
counts the number of strings in the argument and generates a list of indices for those strings (0 1 2 3 in this example case)

    i.@# 'fee fie'; 'huff and puff'; 'mirror mirror'; 'tick tock'
 0 1 2 3

Here, # counts the number of items in a list, and the two character token i. generates that many indices.  @ combines two functions:  f@g y is equivalent to f(g(y))

Next, smoutput@,&":&> combines these two results.

   0 1 2 3 smoutput@,&":&> ' fee fie'; ' huff and puff'; ' mirror mirror'; ' tick tock'

will display

 0 fee fie
 1 huff and puff
 2 mirror mirror
 3 tick tock

and returns an irrelevant result.

The composite verb smoutput@,&":&> combines several functions, one after the other.  A dyadic verb of the form x f&g y is equivalent to ((g x) f (g y)) -- and note also that this is different from expressions like x&f because x is a noun, and not a verb and the grammar associated with nouns is different from the grammar associated with verbs.  A dyadic verb of the form x f@g y is equivalent to (g (x f y)).  Here, f and g are arbitrary verbs (functions) and x and y are arbitrary nouns (data).

[Note for LISP programmers: J is an infix language (1 + 1) gives 2.]

The progress of execution can be hinted at by replacing some of the verbs in the expression:

    0 1 2 3 ;&> ' fee fie'; ' huff and puff'; ' mirror mirror'; ' tick tock'
 ┌─┬──────────────┐
 │0│ fee fie      │
 ├─┼──────────────┤
 │1│ huff and puff│
 ├─┼──────────────┤
 │2│ mirror mirror│
 ├─┼──────────────┤
 │3│ tick tock    │
 └─┴──────────────┘

Using &> means we are working with atoms from each side of the list and we are unpacking boxes.  (The ; verb constructs a boxed list of its arguments.)  A subtle distinction here is that everything on the left side of &> gets applied to each element of the list, instead of to the list as a whole.

    0 1 2 3 ;&":&> ' fee fie'; ' huff and puff'; ' mirror mirror'; ' tick tock'
 ┌─┬──────────────┐
 │0│ fee fie      │
 ├─┼──────────────┤
 │1│ huff and puff│
 ├─┼──────────────┤
 │2│ mirror mirror│
 ├─┼──────────────┤
 │3│ tick tock    │
 └─┴──────────────┘

The verb ": formats its argument.  In other words, the numbers 0, 1, 2 and 3 have been replaced by their equivalent character lists.  This would be more apparent if I were serializing each of the arguments, but this bit of documentation is already getting a bit too big.

    0 1 2 3 <@,&":&> ' fee fie'; ' huff and puff'; ' mirror mirror'; ' tick tock'
 ┌─────────┬───────────────┬───────────────┬───────────┐
 │0 fee fie│1 huff and puff│2 mirror mirror│3 tick tock│
 └─────────┴───────────────┴───────────────┴───────────┘

The , operator concatenates two lists.  (And < puts the result of the operation back in a box.)

    0 1 2 3 smoutput@,&":&> ' fee fie'; ' huff and puff'; ' mirror mirror'; ' tick tock'
 0 fee fie
 1 huff and puff
 2 mirror mirror
 3 tick tock

Here, we have printed each of the composed strings.  A subtle distinction here is that they will be displayed before evaluation of the rest of the sentence.


### List indexing

In all my years, I have never seen:

You have two choices:

 0 go
 1 don't go

Everybody (except "C" people) are used to choices starting with 1, not zero.  Like a book, real people expect
the first page to be 1 (one).  Ask anyone for a top ten list, and you won't see 0 --> 9. 

[[User:Gerard Schildberger|Gerard Schildberger]] 17:38, 21 January 2011 (UTC)

:But given the topic of this site, it is ''very'' common to have to consider lists with a minimum index of zero as well as one. --[[User:Paddy3118|Paddy3118]] 21:22, 21 January 2011 (UTC)


### prompt user



```J
'choose a number 0..' 0&".@prompt@, ': ',~ ":@<:@#
```


The first part of this phrase to be executed is

   ":@<:@#

<code>#</code> counts the number of elements in its argument

<: is a two element token which when used with one argument subtracts one from its argument.  I could have instead used (-&1) but that's more than twice as long.

": gives the string representation of numbers (and of strings, but that's trivial).

In other words:

    ":@<:@# 'fee fie'; 'huff and puff'; 'mirror mirror'; 'tick tock'
 3

This is a literal string, and not a number.

   (': ',~ ":@<:@#) 'fee fie'; 'huff and puff'; 'mirror mirror'; 'tick tock'

An expression of the form (f g h) y is equivalent to the expression ((f y) g (h y)), if f g and h are verbs and y is a noun.  As a special (and useful) case, an expression of the form (m g h) y is equivalent to an expression (m g (h y)).  In other words, if the word on the far left is a noun, it is treated as a constant function which always returns itself.  

An expression of the form x g~ y is equivalent to the expression y g x, if g is a verb and x and y are nouns.

Thus:

    (': ',~ ":@<:@#) 'fee fie'; 'huff and puff'; 'mirror mirror'; 'tick tock'
 3: 

And,

    ('choose a number 0..' , ': ',~ ":@<:@#) 'fee fie'; 'huff and puff'; 'mirror mirror'; 'tick tock'
 choose a number 0..3: 

The verb prompt takes a string argument, displays that string to the user, and waits for the user to type something and its result is the list of characters typed by the user.

  _&".

converts a list of characters to numbers.  Invalid numbers are represented using _ (infinity).

==={::===

The J verb {:: indexes from an array and unboxes its result.

    3 {:: 'fee fie'; 'huff and puff'; 'mirror mirror'; 'tick tock'
 tick tock

Moreover an expression of the form (g h) y is equivalent to the expression y g (h y).  (And, as was explained earlier, ~ swaps left and right arguments.)


### error handler


An expression of the form f ::g y is equivalent to f y if y is in the domain of f (if the evaluation of f y does not raise an error), and is equivalent to g y if y is not in the domain of f (if the evaluation of f y does raise an error).  So if something bad happens, we run the error handler:

    select@([ smoutput bind 'please choose a valid number')

The phrase
   smoutput bind 'please choose a valid number'
always ignores its argument, and instead always passes the string 'please choose a valid number' to the verb smoutput.

Once that has happened, we have ([ PHRASE) which always ignores the result of phrase and instead returns its original argument, which in our example was 'fee fie'; 'huff and puff'; 'mirror mirror'; 'tick tock'

Finally, our error handler calls the function we have defined, recursively, which repeats this whole process.


### going further


This writeup is at once too short, and too long.  For a novice to the language, I have brushed over a variety of issues which might be puzzling.  (Meanwhile, for both the uninterested reader and people with some familiarity with the language, I have probably provided far too much text.)

If people have questions, I would encourage them to first try a few experiments with the language (it's a free download and runs on a variety of platforms), and perhaps read some [http://www.jsoftware.com/help/dictionary/intro.htm documentation].  And, if some issue remains puzzling, I will try and answer them here.

Finally, hypothetically speaking, both this writeup and the original code might be changed.  For example, perhaps, instead of a single long line, a user would be more comfortable with a series of short definitions of (throwaway) words.    [In my experience, this sort of thing can be a handy learning tool.  But -- unless particularly relevant words are used -- it also introduces distracting complexities, instead of illuminating anything.]

:I'd prefer something like the following as being more readily parseable:

```j

load 'misc'
displayMenu      =: i.@# smoutput@,&":&> ' '&,&.>
makeMsg          =: 'Choose a number 0..' , ': ',~ ":@<:@#
displayBadNumMsg =: [ smoutput bind 'Please choose a valid number!'

select=: ({::~ _&".@prompt@(makeMsg [ displayMenu)) :: ($:@displayBadNumMsg)

```

:--[[User:Tikkanz|Tikkanz]] 02:46, 15 October 2009 (UTC)
::I have adopted this version, with some minor name changes (and sticking with require instead of load) --[[User:Rdm|Rdm]] 12:43, 26 January 2010 (UTC)
