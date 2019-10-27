+++
title = "Talk:99 Bottles of Beer"
description = ""
date = 2018-12-22T22:29:32Z
aliases = []
[extra]
id = 2664
[taxonomies]
categories = []
tags = []
+++

==CPP Recursive ==
Hope you dont mind but i put in a 99 beer solution i had to do for a class a while back. I have my origional reference here: http://csinsider.homeip.net/index.php/CPP_Pro#Program_.234_Recursion_on_99_bottles_of_beer
I figure that you guys helped me so much, i can at least contribute a bit.

== CPP ==

What about taking the two "with preprocessor" examples from C and C++ -- is there any difference between them? --
and making them an example of the language CPP, the C preprocessor?  Don't C and C++ share the same preprocessor?
Note that CPP can be run all by itself, as well.

== Syntax highlighting ==

What's the story with syntax highlighting? I kind of like it, but are we going to use it on tasks from now on? --[[User:Mwn3d|Mwn3d]] 12:23, 28 February 2008 (MST)
:I'm ambivalent until we have more/better language support.  However, if other folks don't find having some languages supported while others aren't, then I wouldn't mind seeing its use expanded.  NevilleDNZ was going to work on on a means of importing Vim syntax highlighting scripts so that the syntax highlighter could be extended.  I'd be curious how that's coming. --[[User:Short Circuit|Short Circuit]] 21:02, 28 February 2008 (MST)

== Common Lisp ==

The Common Lisp solution is ... not very good!
* there's some wiki quoting/escaping/formatting issue with "defun"
* it uses (- x 1), where (1- x) is shorter and probably more idiomatic
* it uses (> .. 0) instead of zerop
* it uses a 1-form if instead of when
* it's recursive when it really doesn't need to be
* it uses massive indentation
* it says "1 bottles of beer", when it's trivial to say "1 bottle of beer"
* it uses ~a (any text) instead of ~d (integer) for printing integers
* it doesn't leave a line between verses

  (defun bottle (n)
    (format t "~D bottle~:P of beer on the wall~%" n)
    (format t "~D bottle~:P of beer~%" n)
    (format t "Take one down, pass it around,~%")
    (format t "~D bottle~:P of beer on the wall~%~%" (1- n) (1- n)))
  (defun bottles (x)
    (loop for i from x downto 1 do (bottle i)))
  (bottles 99)

: Be bold! It's a wiki, feel free to fix any problems yourself. (Many of the examples were contributed by undergraduates who were just learning the languages themselves.) --[[User:IanOsgood|IanOsgood]] 18:33, 9 December 2008 (UTC)
::The formatting issue is known also. We use an open source syntax highlighter that isn't always perfect. We can't figure out how to fix it ourselves and there have been more important issues since we saw those problems. --[[User:Mwn3d|Mwn3d]] 18:52, 9 December 2008 (UTC)

== Page Size ==

This page is getting VERY big. It probably should be broken int three or more smaller pages. I'm not sure what the best way to do that would be.

One possibility that comes to mind is to partition the languages alphabetically: A-C: D-J; K-P:  and Q-Z.

== Review and cleanup ==

This page needs a ''lot'' of review and cleanup. Issues I can see so far:
* At least one code example is stated as copied from another site. Is this compliant with [[Rosetta Code:Copyrights]]?
* "Go" is in there twice, as "Go" and "Go!"
:: Those are two different languages.  The language "Go!" was the first, but Google came 10 years later with the similarly-named language "Go".   Please read more about it at [http://code.google.com/p/go/issues/detail?id=9 Issue 9] --[[User:Eliasen|Eliasen]] 07:39, 15 March 2012 (UTC)
* Someone just added a golf'd C version, and called it  the "correct" version...How does one even define a correct version for 99 Bottles of Beer that isn't simply "Meets the requirements of the task and of RC at large?" I'm not a fan of golfing on RC; expressing something as succinctly as possible has its place in showing the expressiveness of a language and/or the breadth of accessible libraries, but dropping mundane readability factors like newlines and whitespace makes it more like an amateur IOCCC exercise.
So, yeah, this page needs a lot of review and cleanup. --[[User:Short Circuit|Michael Mol]] 14:09, 15 November 2010 (UTC)

:The copyright issue is probably a non-issue, at least under u.s.a. copyright law (fair use allows for excerpting, and the law has other aspects which are designed to draw a line dividing simplistic stuff and real creativity).  --[[User:Rdm|Rdm]] 19:36, 16 November 2010 (UTC)

: I assume the copyright item was Clips. I went ahead and replaced it with a shorter, simpler, original version. Voila, no more potential copyright concerns. -- [[User:Sekoia|Sekoia]] 16:28, 20 November 2010 (UTC)

== here an ogg ==
There are screenshots on rosettacode yet, but is there an .ogg yet?
I think we should find someone to sing this song (but maybe not from 99 downto 1 of course). -- [[User:Blue Prawn|Blue Prawn]]
: (Remember to sign your conversation posts,) I can do it. In fact, I'll do one better, and record "90" "80" "70" "60" "50" "40" "30" "20" "10", 1-9, "bottles", "bottle", "of beer on the wall", "of beer", "take one down, pass it around" and "no more". It'd be up to you assemble it. :) --[[User:Short Circuit|Michael Mol]] 12:34, 17 November 2010 (UTC)
:: You would actually need two sets of the numbers as well as "bottles", "bottle", and "of beer on the wall", since the end of the verse is not the same notes as the other instances. --[[User:Coderjoe|Coderjoe]] 00:13, 18 November 2010 (UTC)
::: Three sets, I think. The first, second, and fourth lines of each verse all use different notes. You could just use one set with a pitch shifter, though. --[[User:Mr2001|Mr2001]] 02:32, 18 November 2010 (UTC)
:::: Indeed. and also 11-19. --[[User:Coderjoe|Coderjoe]] 09:07, 19 November 2010 (UTC)
:::You can look at [[99_Bottles_of_Beer#Sound]] to see what notes you need. "e-" is E-flat, "+" means sharp, ">" means up an octave (octaves start on C), and "<" means down and octave. That's also assuming I got the tune correct by ear :p. --[[User:Mwn3d|Mwn3d]] 14:38, 19 November 2010 (UTC)

==musings and complaints==
If the task is to print (display) the lyrics to the song, then the lyrics should have correct punctuation.
:: The listing should also have correct spacing;  each verse should be considered like a chapter, and should have a blank line after each verse.
:: Also, nobody sings "zero bottles of beer on the wall", it should be "no bottles of beer on the wall". 
:: Nor do they sing "1 bottles of beer on the wall".  
Pluralization is a simple thing to program. 

I had thought about spelling out the numbers, that is another task by itself. 

 -- [[User:Gerard Schildberger|Gerard Schildberger]] 07:16, 16 March 2012 (UTC)

== plurals in non-english languages ==

as the GNU Gettext manual points out, plurals in non-English languages can be somewhat complex, and would essentially change the entire nature of the code. For example see http://www.russianlessons.net/lessons/lesson11_main.php Russian. ... interesting problem. [[User:Donbright|Donbright]] ([[User talk:Donbright|talk]]) 20:10, 14 April 2013 (UTC)
