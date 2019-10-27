+++
title = "Talk:Natural sorting"
description = ""
date = 2017-04-07T19:19:55Z
aliases = []
[extra]
id = 9455
[taxonomies]
categories = []
tags = []
+++

==Why draft?==
I'm never sure about Unicode, and this is my first "choose any four from eight" type task description.

I think I will change it to ask that at least the first four are done then leave the others as "extra credit" - that way language examples will be more comparable. --[[User:Paddy3118|Paddy3118]] 11:16, 23 April 2011 (UTC)

:Note that, unless constrained by their language, a good programmer will implement this as a sort routine that takes a text normalization function, and after the first two examples this task devolves down to writing the normalization function and testing it.  (Some languages provide a sort routine which performs suboptimally with this approach but I think we can neglect that issue here.)  --[[User:Rdm|Rdm]] 12:10, 23 April 2011 (UTC)
::Like function sortkeygen of the python example ;-)
:: --[[User:Paddy3118|Paddy3118]] 12:20, 23 April 2011 (UTC)

== Numeric sub-fields ==
For the fourth one (numeric sub-fields), are we to ignore the non-numeric parts or to assume they're the same? –[[User:Dkf|Donal Fellows]] 17:28, 23 April 2011 (UTC)

No field is ignored Donal.
# Split the string into fields of alternating numeric/non-numeric runs of characters (or non-numeric/numeric).
# when comparing two strings s1 and s2 say, now split as fields, start from the rightmost fields and compare them - if the first fields are numeric then compare them as integers; if the fields are both non-numeric then compare them as character strings; if they differ in type then assume any integer field is always less than any non-integer field (the two strings shouldn't really be compared, but this gives them a defined order).
# If the two rightmost fields are equal then compare the next two fields, and so on.
# If the fields of the two strings compare equal so far, but one string has no more fields but the other has more fields, then the string with least fields is the smaller.
Using the above comparison rules you should be able to sort/order strings. --[[User:Paddy3118|Paddy3118]] 19:11, 23 April 2011 (UTC)
: Cool. That corresponds to one of Tcl's built-in sorting modes. :-) –[[User:Dkf|Donal Fellows]] 20:01, 23 April 2011 (UTC)

:: Wait! I'm gobsmacked! Do you have a link to the documentation? --[[User:Paddy3118|Paddy3118]] 21:25, 23 April 2011 (UTC)

::: That's what <code>[http://www.tcl.tk/man/tcl8.5/TclCmd/lsort.htm#M6 lsort -dictionary]</code> does. (Well, it also handles case a bit differently, treating it as a second-order difference rather than the usual first-order difference.) It was added because it is the mode which “puts filenames in the order that the user expects”, making it much easier to produce a nice GUI to use. (FWIW, I'm not doing the ligature normalization stuff because that's a lot of work to do right and it's an area where Tcl needs more work; I forget the Request For Enhancement number. :-)) –[[User:Dkf|Donal Fellows]] 11:17, 2 May 2011 (UTC)
:::: Thanks. --[[User:Paddy3118|Paddy3118]] 23:26, 2 May 2011 (UTC)

I'm not understanding this algorithm.  Copied from the Python results,

```txt

Naturally sorted:
['Equiv.\x0bspaces: 3+0',
 'Equiv.\nspaces: 3+1',
 'Equiv.\tspaces: 3+2',
 'Equiv.\x0cspaces: 3-1',
 'Equiv.\rspaces: 3-2',
 'Equiv. spaces: 3-3']

```

Each of these strings ends in a sub-field of digits, actually a single digit, and the sub-field before that is either "+" or "-".  Starting with the right-most subfields, they will be ordered by their integer values, 0, 1, 2, or 3.  Then for cases with the same value, "+" comes before "-" by usual string comparison.  That's enough to discriminate these strings.  The remaining left-most sub-fields don't need to be considered.  So, shouldn't the correct order be,

```txt

 'Equiv.\x0bspaces: 3+0',
 'Equiv.\nspaces: 3+1',
 'Equiv.\x0cspaces: 3-1',
 'Equiv.\tspaces: 3+2',
 'Equiv.\rspaces: 3-2',
 'Equiv. spaces: 3-3'

```

I'm missing something &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 22:32, 25 July 2014 (UTC)

:(I see this. I won't answer now as I am (pleasantly), hung over after the works summer party yesterday). (Hic) --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:27, 26 July 2014 (UTC)

::Looking at this more, it seems examples are interpreting "rightmost" as "leftmost", or perhaps "most significant" as "least significant."  Maybe the task description could be clarified? &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 21:40, 29 July 2014 (UTC)

I agree with [[User:Sonia|Sonia]], the Python treatment of numeric fields does not agree with the algorithm above, which makes the above example come out wrong.  Consider this example:

```txt

# TEST Numeric fields as numerics
Text strings:
['foo3bar99baz2.txt',
 'foo2bar99baz3.txt',
 'foo1bar99baz4.txt',
 'foo4bar99baz1.txt']
Normally sorted :
['foo1bar99baz4.txt',
 'foo2bar99baz3.txt',
 'foo3bar99baz2.txt',
 'foo4bar99baz1.txt']
Naturally sorted:
['foo1bar99baz4.txt',
 'foo2bar99baz3.txt',
 'foo3bar99baz2.txt',
 'foo4bar99baz1.txt']

```


I would expect the natural sort to give the reverse order, based on evaluating the groups from the right. -- [[User:Peter|Peter]]  00:20, 17 Feb 2017 (UTC)

Task point four is self-contradictory. It says "with the rightmost fields being the most significant" and later on "x9y99 before x9y100, before x10y0", which is the leftmost fields being the most significant. [[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 19:18, 7 April 2017 (UTC)

==Criticisms please==
I note that it has been several days without another language implementation added to the initial Python so I would like to stick my chin out, (figuratively), and ask for criticisms. 

Is the task too long? Does it seem like too much work? Is it hard to understand? ...

Oh; and this is the first time I think, that I have tried to save space by linking the task test-suite in this way - what do you think?
--[[User:Paddy3118|Paddy3118]] 06:27, 27 April 2011 (UTC)

:For me: It involves aspects of unicode where I do not have complete knowledge nor do I know how to get complete knowledge (this seems to apply for orderings 2 3 6 7 8 and maybe 4), so I am waiting until I am inspired to try to discover these structures in unicode, or for someone else to pick it up.  (Also, the examples look necessarily bulky.) --[[User:Rdm|Rdm]] 14:43, 27 April 2011 (UTC)

:I may be blinded by the only way I can think of to do it in Java, but it seems to me that the task is a super-complex version of [[Sort using a custom comparator]]. I'm not sure that linking to a particular example's output is the best way to define the task. We ran into problems with that in [[Multisplit]]. Beyond all of that it does seem like a lot of work. Do umlauts count as accents? Should they be sorted as their expansion like scharfes? What about circumflexes (circumfleces? circumfli?)? There is a whole list of marks [[wp:Diacritic|here]]. Some of them represent condensations of letters (like ss condensed to a scharfe), some represent accents, and some represent different pronunciations (like a cedilla in French or tilde in Spanish). As you can see, this can get pretty complicated quickly. --[[User:Mwn3d|Mwn3d]] 15:25, 27 April 2011 (UTC)
:: I'll only answer the point about accents at the moment. Unicode is a pig for me too, so you only ''need'' to handle the accents mentioned in the particular test for that section if you don't have a convenient unicode class to make it more generic. You might implement parts via expandable means like using a table for example - expand the table to handle more than what task examples require. --[[User:Paddy3118|Paddy3118]] 18:01, 27 April 2011 (UTC)

:To me it seems that it is not so much the task, it is the test cases. Can't you simply provide a file with lines to be sorted?--[[User:Abu|Abu]] 08:05, 1 September 2011 (UTC)
::Hi Abu, I have extracted the strings used for testing from the original Python and created a sample inputs section. --[[User:Paddy3118|Paddy3118]] 05:19, 2 September 2011 (UTC)
:::Great! Thanks Paddy3118, I give it a try --[[User:Abu|Abu]] 19:06, 3 September 2011 (UTC)


###  Lexical ordering system 


I always place symbols before numbers and letters, caps after lowercase and symbols are sorted in a different order to which they appear in the ascii table:

* foo bar
* foo_bar
* foo-bar
* foo9bar
* foobar
* Foo bar
* FOO bar

[[User:Markhobley|Markhobley]] 19:46, 27 April 2011 (UTC)

== Ʒ ==

Wasn't 'Ʒ' derived from 'Z' rather than 'S'? And IIRC 'ß' may be interpreted as either 'ss' or 'sz', but I admit that may be incorrect since I don't speak German. --[[User:Ledrug|Ledrug]] 23:39, 31 July 2011 (UTC)
:All I can find is [[wp:Ʒ]] and [[wp:ß]]. --[[User:Paddy3118|Paddy3118]] 04:57, 2 August 2011 (UTC)
