+++
title = "Talk:Farey sequence"
description = ""
date = 2019-08-08T20:17:55Z
aliases = []
[extra]
id = 17471
[taxonomies]
categories = []
tags = []
+++

== REXX program encoding ==

What language encoding should the file be saved in, and what should the LC_* env vars be before invoking Regina on a Unixy OS?  Currently under UTF-8 everything, Regina (-v: REXX-Regina_3.6(MT) 5.00 31 Dec 2011) complains:
```txt

    10 +++    say center('Farey sequence for order '   n   " has"   #   'fractions.', 150, '═')
Error 40 running "/tmp/test.rex", line 10: Incorrect call to routine
Error 40.23: CENTER argument 3 must be a single character; found "═"

```

It goes away after changing that "═" to any ASCII char. The same problem is present in many Rexx examples on this site. --[[User:Ledrug|Ledrug]] ([[User talk:Ledrug|talk]]) 21:30, 1 April 2014 (UTC)

-----

I suspect the mechanism that is being used in saving (or downloading) the REXX program (file) from Rosetta Code is storing the character(s) of the extended ASCII (ASCII-8) as some flavor of Unicode, which Regina REXX is seeing as a two- or three-byte character.   The extended-ASCII character that was being used is the 'cd'x (═) character (the horizontal double-line boxing character) from the Microsoft (DOS) code page 437 (which, I think, is probably the default code page in the USA).   What you are showing (in the line with 10 +++ message) looks like a one-character (the 3<sup>rd</sup> argument for the '''center''' <strike>BUF</strike> BIF), but the REXX program source file probably has a two-character field stored there, thereby incurring Regina REXX's wrath (as witnessed by it issuing the error message(s). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:44, 1 April 2014 (UTC)

To address the 1<sup>st</sup> question:   all Classic REXXes that I know of only support bytes, not Unicode characters.   I'm not very familiar with Unixy thingys, so I don't know what can be specified (if anything) for Regina REXX regarding this issue.   As far as I know, there isn't anything as far as REXX switches or environmental variables that will "fix" the above-mentioned problem; the only fix is to put in a ''single-byte'' character (which that 3<sup>rd</sup> argument is just essentially used for fodder for a line separator, a dash would work as well, ··· or a minus sign will do in a pinch --- but then it looks chintzy, and we can't have that.) -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:44, 1 April 2014 (UTC)

-----

== Grumble about "marked as incorrect" ==

I got a "marked as incorrect" flag:

{{incorrect|J| 
 First and last Farey terms aren't expressed as fractions as per the definition, 
 no terms shown are expressed as fractions, 
 (see the '''C''' programming example's output as an example). 
 }}

Here's an example:


```J
   Farey 1
0 1
   Farey 2
0 1r2 1
   Farey 3
0 1r3 1r2 2r3 1
```


And yet, 

(a) the definition currently places no specification on output format, and
(b) this is the native J format for fractions.

I could "change the output format to match C" but that's not a part of the task description. It would be trivial to match the C display, but there's actually nothing in the task description (other than "completely reduced") that seems relevant here. (And a person could easily argue that 0 and 1 are completely reduced while 0/1 and 1/1 are not.)

As a first pass, I'm going to address this "incorrect" with documentation. If the task description itself gets changed to document the "completely reduced format" I will of course comply, but I wanted to put my reasoning here, before I did that.

-----

The definition doesn't specifically mention an ''exact'' format, but every example shown (anywhere) are displayed as what people would easily recognize as fractions.  

Indeed, the first and last terms are specifically called fractions, and even mentions HOW they are to be expressed (that is, as fractions, specifically as 0/0 and 1/1), not zero and unity.   This is part of the definition of a Farey sequence (that is, how it is displayed, otherwise 0 and 1 would be used).   So is the definition of the terms in-between the first and last entries, these are fractions (which are completely reduced).   Again, note that the end fractions are ''defined'' to be 0/1 and 1/1, all others between 0 and 1 are shown in lowest terms (or in other words, reduced, ... or to be more specific, completely reduced).       I.E.:   28/48   could be reduced to   14/24,   but completely reduced is   7/12. 

As far as the ''internal'' format for any language, that doesn't matter.   Although it mentions elsewhere that they are positive integers, I won't quibble if they are (internally) stored in some other format.   If a language stores integers internally as binary, I still expect the output to be shown in base ten (decimal), and without a leading plus sign, and without a trailing decimal point, and with superfluous leading zeroes suppressed, and without superfluous zeros elsewhere, and have the sequence shown horizontally instead of a vertically format, and without superfluous blanks (i.e.:   7 / 9 ) for that matter.   I wouldn't complain if the horizontal/vertical format wasn't thus, but everybody saw the practicality of using a horizontal format.   I think that the obvious shouldn't have to be stated.   In doing so, it would really, really clutter up the requirements, not to mention make it tedious bore to read.   What matters is how the Farey sequences are displayed:   as fractions as per the examples (showing the orders of 1 to 5).   Please try to show the Farey sequences as close as possible to those examples.   If it's a trivial thing to use a solidus instead of an '''r''', then please transform the Farey sequences (output).   All examples shown in the programming entries use the common form of fractions:   the use of a solidus ('''/''') instead of a horizontal bar (as do the examples of orders 1 to 5 in the preamble, a nicety that HTML provides).   I don't know anybody who doesn't recognize that format as an example of a fraction. 

It wasn't my intent to have you ''match'' a particular output from another programming example's to order to conform to that language's output, I just chose '''C''' because it was the first one in the language list and reduced any scrolling to locate it.   The perfect (output) model of efficiency and minimalism.   When I said, '''see''' that example, I meant just look at it to get the general idea; its a good example of what all other programming examples have done.   This isn't about making any particular language's output look like another's output.   It's about having the output reflect the definition (or example, if you will) of the Farey sequences (order 1 to 5) as exemplified in the Rosetta Code task's preamble.

As an aside, I chose to mark the output as incorrect (as opposed to have this discussion in the talk page) as it would've been so easy to comply, and once done, the flag would be removed, and there would be no fuss, no mess.   Whereas these comments will be around forever. 

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:13, 2 April 2014 (UTC)

-----

: Acts of civil disobedience (such as I am engaged in here) probably deserve some discussion. My stance - that what I have provided is mathematically equivalent to what is being displayed. The order is equivalent, the values are equivalent. The only thing that is not equivalent is the display. And yet, none are truly equivalent to the task display (which is not plain ascii).

: Anyways, If you are comfortable adding a format requirement to the task description, I'll be comfortable complying with that format requirement. It's only a short line of code (but that pretty much doubles the amount of code I'd need for this task, and removes the motivation to document the associated language features). And, I will also remove the documentation explaining the current state of affairs (because they would no longer be the current state of affairs).

: If you are not comfortable adding a format requirement to the task description, if the only motivation is style, I'd prefer to leave it like it is.

: Thanks! --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 17:54, 2 April 2014 (UTC)

:: The motivation was never style, let alone the ''only'' motivation, it's form and content which implies meaning.   Please re-read the definition of the starting and ending values of a Farey sequence:   they are the fraction '''0/1''' and the fraction '''1/1'''.   This isn't style, this is the definition.   '''0''' and '''1''' aren't fractions.   The Farey sequences are all fractions, including the border terms.   I don't see a need to add verbage that the programming examples need to conform to that definition, it 'is' the definition, and that should be enough to cause conformity from the examples' outputs.   But, if you feel the need for a special requirement (or more verbage) for a particular language (or just in general), then please feel free to add it if that's what it takes to make your example correct.   I don't see your adding a special requirement (as a tidying up thingy) is a stumbling block.   If that's what it takes to clarify the issue, then I wish you'd add whatever language makes it clearer. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:50, 2 April 2014 (UTC)

::: Indeed, when I look up the definition of "fraction" I see "a numerical quantity that is not a whole number (e.g., 1/2, 0.5)" -- but this would mean that 0/1 and 1/1 are not fractions, because these represent whole numbers.
::: Possibly the concept you are trying to describe needs a few qualifying words to distinguish it from this definition? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 06:54, 16 February 2015 (UTC)

:::: "The" definition is not the only definition of a fraction, indeed, the common usage is:   a fraction if any part of a unit.   That ''any part'' may, I believe, be the ''whole part''.   Also, another point is that an ''improper fraction'' is still a fraction.   I certainly don't want to start a mathematical debate on the one true definition of a fraction --- I defer to what others have defined what a Farey sequence is (and, apparently, what it looks like) --- or, at least, how the terms are depicted --- and most important, how the end terms are depicted (defined by fiat).   Perhaps we are getting hung up on what a fraction is, rather than what a fraction looks like (or is depicted) --- at least, as far as a Farey sequence is often depicted.   Farey sequences are terms that look/appear/resemble fractions, in that there is what appears to be a numerator and a denominator, and furthermore, their equivalent values aren't used --- you don't see   '''0 .25 1/3 .5 2/3 .75 1'''   as being depicted in (or as) a Farey sequence.   It doesn't matter to me if a solidus is used or some kind of horizontal bar is between the numerator and denominator, ... as long as the Farey term ''looks'' like they are part of the Farey sequence.   Since HTML code (or plain text) is commonly used on Rosetta Code, there is nothing wrong with "plain" ASCII characters being used.   The beginning and ending terms are defined to be the value   '''0'''   and   '''1''',   denoted by the fractions (or glyphs)   '''0/1'''   and   '''1/1'''   (this is the wording the Wikipedia entry uses in its definition of a ''Farey sequence''.   Another definition that I like is the Wolfram MathWorld (TM) entry. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:45, 16 February 2015 (UTC)



:::: There's a fine line between a numerator and a denominator.       -- Anonymous 
:::::::::::::::::::::::::::: -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:17, 8 August 2019 (UTC)

==Parts of formulae here now invisible to many browsers==

Parts of various formulae on this task page have become invisible to many browsers (all those - the majority - which display the graphic file rather than processing the MathML directly), as a result of 'tidying and spacing' edits made on 00:37, 22 May 2016. One of the issues may be an attempt at cosmetic introduction of redundant white space around Latex expressions inside &lt;math&gt; tags. This white space is not currently expected by the MediaWiki processor, and leads to the generation of syntactically ill-formed HTML. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:16, 19 September 2016 (UTC)

: This one doesn't respond to immediate First Aid. I can see why an attempt was made to improve its appearance, though the edits that were made unfortunately hid the Farey sequence itself from most browsers. 
: Even before the 22 May 2016 edits, the formulae, though visible, were oddly prefixed by a fragment of undigested LaTeX code ('''\textit'''). Perhaps we should just revert this, for the moment, to its 21 May 2016 condition (at least the task description formulae were visible at that stage), and then aim to fix the undigested \textit tag (if possible) next ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 18:45, 18 November 2016 (UTC)

:: For the moment I have gone ahead and restored the visibility of the sequence formulae to Chrome IE/Edge and Safari, at the expense of also reintroducing an earlier glitch - a visible LaTeX fragment.
:: My guess is that restored visibility at the price of a a bit of visual noise, is better than no noise at the price of complete invisibility to most browsers. Please advise if you take a different view. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 22:36, 20 November 2016 (UTC)
