+++
title = "Talk:Mayan numerals"
description = ""
date = 2019-01-18T00:21:06Z
aliases = []
[extra]
id = 22135
[taxonomies]
categories = []
tags = []
+++

==Cartouche display is messy for Chrome and Safari==

It looks as if the the testing may only have been done with Firefox - the cartouches are ragged for Chrome and Safari. Particularly where their integer value is shown to the left with an arrow, which disrupts the vertical alignment for the rest of the line. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 18:12, 14 January 2019 (UTC)

PS the Perl 6 cartouche-rendering has already achieved a much better signal-to-noise ratio – perhaps worth standardising on something like that ? (Especially if there is a way of tweaking it for continuous (rather than dashed) horizontal 5-bars) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:51, 14 January 2019 (UTC)


==possible a better rendering for the Mayan numerals==
How does this rendering look versus the one on the task page?     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:50, 14 January 2019 (UTC)


;The Mayan numerals   (and some random numbers)   shown in the   ''vertical''   format would be shown as:

```txt

       ╔════╗                      ╔════╗                            ╔════╦════╗
       ║    ║                      ║    ║                            ║    ║    ║
       ║    ║                      ║ ∙  ║                            ║    ║    ║
  1──► ║    ║                11──► ║────║                      21──► ║    ║    ║
       ║ ∙  ║                      ║────║                            ║ ∙  ║ ∙  ║
       ╚════╝                      ╚════╝                            ╚════╩════╝
       ╔════╗                      ╔════╗                            ╔════╦════╗
       ║    ║                      ║    ║                            ║    ║    ║
       ║    ║                      ║ ∙∙ ║                            ║    ║    ║
  2──► ║    ║                12──► ║────║                      22──► ║    ║    ║
       ║ ∙∙ ║                      ║────║                            ║ ∙  ║ ∙∙ ║
       ╚════╝                      ╚════╝                            ╚════╩════╝
       ╔════╗                      ╔════╗                            ╔════╦════╗
       ║    ║                      ║    ║                            ║    ║    ║
       ║    ║                      ║∙∙∙ ║                            ║    ║    ║
  3──► ║    ║                13──► ║────║                      40──► ║    ║    ║
       ║∙∙∙ ║                      ║────║                            ║ ∙∙ ║ Θ  ║
       ╚════╝                      ╚════╝                            ╚════╩════╝
       ╔════╗                      ╔════╗                            ╔════╦════╗
       ║    ║                      ║    ║                            ║    ║    ║
       ║    ║                      ║∙∙∙∙║                            ║    ║    ║
  4──► ║    ║                14──► ║────║                      80──► ║    ║    ║
       ║∙∙∙∙║                      ║────║                            ║∙∙∙∙║ Θ  ║
       ╚════╝                      ╚════╝                            ╚════╩════╝
       ╔════╗                      ╔════╗                            ╔════╦════╗
       ║    ║                      ║    ║                            ║    ║    ║
       ║    ║                      ║────║                            ║    ║    ║
  5──► ║    ║                15──► ║────║                      90──► ║    ║────║
       ║────║                      ║────║                            ║∙∙∙∙║────║
       ╚════╝                      ╚════╝                            ╚════╩════╝
       ╔════╗                      ╔════╗                            ╔════╦════╗
       ║    ║                      ║ ∙  ║                            ║    ║    ║
       ║    ║                      ║────║                            ║    ║    ║
  6──► ║ ∙  ║                16──► ║────║                     100──► ║    ║    ║
       ║────║                      ║────║                            ║────║ Θ  ║
       ╚════╝                      ╚════╝                            ╚════╩════╝
       ╔════╗                      ╔════╗                            ╔════╦════╗
       ║    ║                      ║ ∙∙ ║                            ║    ║    ║
       ║    ║                      ║────║                            ║    ║    ║
  7──► ║ ∙∙ ║                17──► ║────║                     200──► ║────║    ║
       ║────║                      ║────║                            ║────║ Θ  ║
       ╚════╝                      ╚════╝                            ╚════╩════╝
       ╔════╗                      ╔════╗                            ╔════╦════╗
       ║    ║                      ║∙∙∙ ║                            ║    ║    ║
       ║    ║                      ║────║                     300──► ║────║    ║
  8──► ║∙∙∙ ║                18──► ║────║                            ║────║    ║
       ║────║                      ║────║                            ║────║ Θ  ║
       ╚════╝                      ╚════╝                            ╚════╩════╝
       ╔════╗                      ╔════╗                            ╔════╦════╦════╗
       ║    ║                      ║∙∙∙∙║                            ║    ║    ║    ║
       ║    ║                      ║────║                     400──► ║    ║    ║    ║
  9──► ║∙∙∙∙║                19──► ║────║                            ║    ║    ║    ║
       ║────║                      ║────║                            ║ ∙  ║ Θ  ║ Θ  ║
       ╚════╝                      ╚════╝                            ╚════╩════╩════╝
       ╔════╗                      ╔════╦════╗                       ╔════╦════╦════╦════╗
       ║    ║                      ║    ║    ║                       ║    ║    ║    ║    ║
       ║    ║                      ║    ║    ║                       ║    ║    ║    ║    ║
 10──► ║────║                20──► ║    ║    ║             16,000──► ║    ║    ║    ║    ║
       ║────║                      ║ ∙  ║ Θ  ║                       ║ ∙∙ ║ Θ  ║ Θ  ║ Θ  ║
       ╚════╝                      ╚════╩════╝                       ╚════╩════╩════╩════╝

```

 

Note that the Mayan numeral   '''13'''   in   ''horizontal''   format would be shown as:

```txt


                                   ╔════╗
                                   ║  ││║
                                   ║ ∙││║
                             13──► ║ ∙││║        ◄─── this glyph form won't be used in this Rosetta Code task.
                                   ║ ∙││║
                                   ╚════╝

```

:::::::::: -----   (end of rendering)   -----

: Just the same, alas – once you venture out of Firefox the left-hand numbers and arrows render a char or two wider, and break the vertical alignments of the character boxes to the right [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 23:20, 14 January 2019 (UTC)

:: Well, that's a bummer.     I never expected a       <big> < pre > </big>       HTML tag to do that.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:29, 14 January 2019 (UTC)

:::I wonder if pinning down the font used for the numbers and arrows would help ? Defaults may differ [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 23:47, 14 January 2019 (UTC)

:::: All characters used in the task preamble (and above), and for the REXX solution are from the Microsoft DOS (prompt) screen,   the code page used is   '''437'''.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:32, 15 January 2019 (UTC)

==The 'unique' keyword==

In the change log I notice that you have written 'added underlining to the unique keyword, most people are missing that point'. I wonder if there is another way of expressing what you are after ? Readers might be forgiven for thinking that all numbers are equally 'unique'. Or are some a little less unique than others ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 23:47, 14 January 2019 (UTC)

: I was attempting to use the word '''unique''' as in to provide/display a unique Mayan number   different   from any other number chosen by other people when displaying an example number.   Saying it this way seemed a bit too wordy.   I don't disagree that all numbers are unique   (but the Mayan numbers displayed are (from other people's numbers).   I was thinking that in the context,   the number used for their computer programming language example would be distinct from any other example used for this Rosetta Code task.   But, I won't complain or flag that some people choose to copy/re-use another person's unique number.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:07, 15 January 2019 (UTC)

:: Got it – a number not shown in any of the other language contributions. 
:: (A reward for early birds ? The processing cost will progressively rise for late-comers, as each of them checks all existing contributions :-) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 13:25, 15 January 2019 (UTC)

::: Not necessarily.   I could've added more unique contributions without having to check other entries, but it wouldn't take much scrolling.   It's "thinking outside of the box", whether it be Morse code, palindromes, step structures, a Mayan number resembling a (simple) fractal, eggs and dots, something that looks rather like a sine wave, ...   the sky's the limit.   I thought it would be a fun requirement.   Cheap thrills and/or entertainment.     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:20, 18 January 2019 (UTC)
