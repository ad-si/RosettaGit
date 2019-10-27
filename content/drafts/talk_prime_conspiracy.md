+++
title = "Talk:Prime conspiracy"
description = ""
date = 2016-09-16T20:51:23Z
aliases = []
[extra]
id = 20656
[taxonomies]
categories = []
tags = []
+++

===numbers in the example for the task (deprecated)===

<strike>

For 10,000 primes   (as shown by the example in the Rosetta Code task), my numbers (using the REXX program)   don't match those shown:

```txt

For  10000  primes used in this study:

H= 80000
prime  10001  is:  84327

digit  1 ──► 1  has a count of:   281,  frequency of:   2.81%.
digit  1 ──► 3  has a count of:  1094,  frequency of:  10.94%.
digit  1 ──► 7  has a count of:   697,  frequency of:   6.97%.
digit  1 ──► 9  has a count of:   308,  frequency of:   3.08%.

digit  3 ──► 1  has a count of:   417,  frequency of:   4.17%.
digit  3 ──► 3  has a count of:   236,  frequency of:   2.36%.
digit  3 ──► 7  has a count of:   588,  frequency of:   5.88%.
digit  3 ──► 9  has a count of:   719,  frequency of:   7.19%.

digit  7 ──► 1  has a count of:   512,  frequency of:   5.12%.
digit  7 ──► 3  has a count of:   578,  frequency of:   5.78%.
digit  7 ──► 7  has a count of:   254,  frequency of:   2.54%.
digit  7 ──► 9  has a count of:  1059,  frequency of:  10.59%.

digit  9 ──► 1  has a count of:  1170,  frequency of:  11.70%.
digit  9 ──► 3  has a count of:   485,  frequency of:   4.85%.
digit  9 ──► 7  has a count of:   431,  frequency of:   4.31%.
digit  9 ──► 9  has a count of:   302,  frequency of:   3.02%.

```

Does anyone else match either set of numbers for 10,000 primes?   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:32, 21 March 2016 (UTC)

</strike>


Never mind, I found the problem   (had to do with the calculation of the upper bound for the sieve).   I did think it strange that my calculations for 1,000,000 primes was correct, but not for 10,000. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:42, 21 March 2016 (UTC)

-----

I get a different result. Also, prime 10001 is 104759. Prime 8220 is 84327. Prime 0 is 2. So that's probably your issue. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:42, 21 March 2016 (UTC)

: The REXX language (usually) starts out an index with unity, not zero.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:49, 21 March 2016 (UTC)

Pascal results also match the numbers given in the example.
--[[User:G.Brougnard|G.Brougnard]] ([[User talk:G.Brougnard|talk]]) 22:48, 21 March 2016 (UTC)



### primes ending in 2 or 5


[The below was disunioned from the previous (deprecated) talk section.]   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:37, 22 March 2016 (UTC)



:: Ok. Of course, the Pascal result also does not show the transitions involving the prime numbers 2 and 5. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 00:03, 22 March 2016 (UTC)

::: And also, it doesn't show the transitions involving the prime number 3 and the number 5.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:30, 22 March 2016 (UTC)

::: True. I didn't mention 3 because (a) its transitions are already covered in the other two, and (2) the digit 3 shows up in the pascal and rexx tables, so it's not as obvious of a statement. (If you make 2 and 5 show up, you'd have to fix the omission of 3 as well). Anyways, I guess the point is that there's an error there that should probably be fixed sooner or later? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:29, 22 March 2016 (UTC)
::::  I changed my original pascal result, including the transitions 2->3,3->5,5->7, because of the example in front of the task, where they aren't mentioned ;-)

```pascal
      res := Trs[0].CTR_CntTrans[i,j];
      //not counting 2->3,3->5,5->7
      IF res > 1 then
```


:::: I also (regarding REXX) didn't show the transitions for primes ending in '''2''' or '''5''', as the task (apparently) didn't require it   (as it didn't reflect those in the example output).    Since there are exactly three of those (total), it didn't seem that it should/would be beneficial just to add those particular counts to the output as they'll just appear as constants   (for any list showing more than three or more primes).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:37, 22 March 2016 (UTC)

-----

... Although, this task   ''did''   say:       ...   ''for '''any''' pair of successive primes, ... '' 

(the bold   '''any'''   was my highlighting, er, ... enbolding).   I think the   '''any'''   should be reenforced, or a statement should be added that any primes ending in '''2''' or '''5''' should (or could) be excluded from the output   (to save clutter and also not have existing programs changed).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:51, 22 March 2016 (UTC)

::::: I have updated the task's example's numbers to match their description. This seemed to make more sense than changing the description to match the example. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:45, 22 March 2016 (UTC)

::::: After rereading the task description, I have also removed '''Rem : i and j are in (1,3,7,9) .''' from the task description since that doesn't make sense. The problem is that the task asks for frequencies, but the frequencies indicated are only valid frequencies if the remaining i and j values are considered. (Though I suppose it could be argued that the task example rounds those numbers to too few decimal points for that to be noticeable.)  --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 22:49, 22 March 2016 (UTC)

-----

Am I correct in assuming that there is   no   prime that ends in the decimal digit '''1''' that transits to the next prime which ends in a decimal digit '''2'''?     '''Pascal''' has this as part of its output;   would this be considered an error (incorrect output)?.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:03, 23 March 2016 (UTC)

: Yes: 2 is the only prime that ends in decimal digit 2, and it's the first prime, so 2 can never be a successor. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 02:31, 23 March 2016 (UTC)

== Propose change in output format ==

Only primes above 6 are to be included. This simplifies things. We're only left with 1,3,7,9, so the output can be made much more compact, like so:

```txt

10000 first primes. Transitions prime % 10 → next-prime % 10.

              counts                     frequencies, %
          1    3    7    9           1      3      7      9          total
1 →      365, 833, 889, 397        14.69, 33.53, 35.79, 15.98        24.85
3 →      529, 324, 754, 907        21.04, 12.89, 29.99, 36.08        25.15
7 →      655, 722, 323, 808        26.12, 28.79, 12.88, 32.22        25.09
9 →      935, 635, 541, 379        37.55, 25.50, 21.73, 15.22        24.91

```

And frequencies should be calculated separately for each starting digit, so the 4 frequencies for each digit  should sum up to 100%. Also add the total percentage for the four transitions together for each digit.

:: The phrase   "10000 first primes"   now isn't quite true, as some (low) primes are ignored to "simplify" things.   I find the output (above) less intuitive than a straight/simple vertical list (and with no lower limits on what primes are chosen).   And the comment (below) about magnifying the code size and complexity shouldn't to be taken lightly.   I see the requirement to use a horizontal format that will be mucking up the task's clarity and brevity.   People can read and comprehend a vertical list a lot better than a horizontal list, even though the horizontal list is shorter (as far as vertical spacing goes).   I vote '''no''', as readability is more important (in my eyen).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:11, 16 September 2016 (UTC)

::: '''no''' vote recorded. About low primes, we could reword it as "10000 first primes above 5 (or 10)".
::: Personally, I'm lost in the vertical output; with the table I can easily follow the diagonal. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 20:30, 16 September 2016 (UTC)

This way the results are much more visually apparent. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 13:02, 4 September 2016 (UTC)

:::: If you're referring to the vertical list in the task's preamble, I agree with you.   A little whitespace between the changes of (each changed) last-digit would help immensely   (as I did for the REXX example).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:50, 16 September 2016 (UTC)

: I am not seeing anything notable visually jump out at me with that presentation. And while this would make the results more vertically compact, it would also significantly increase the code size (perhaps by an order of magnitude, in some cases). So while it is a cute idea, I am not sure it's worth having everyone redo their entries for this.

:: Ditto that last remark   (as far as being visually apparent).   A vertical format is much easier to peruse.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:11, 16 September 2016 (UTC)

: Plus, of course, this presentation hides the quirk that originally motivated this task. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:18, 4 September 2016 (UTC)

:: If anything it ''highlights'' the evidence that "primes seem to avoid being followed by another prime with the same final digit" (citing from the motivational article) -- because we now can just glance at the diagonal in the frequencies table and see it right away, while normalizing each row separately to the 100% helps to accentuate the difference. Funny how perceptions can be totally different for different people. And of course including the one-off cases for primes below 10 seems to make very little sense because there's no repeated appearances for them at all, as there are for the other digits among the millions - or billions - of primes. So yeah, transition 2 -> 3 is ''''extremely'''' rare, so what? It's one-off anyway; it' uninteresting. Just my opinion. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 17:13, 12 September 2016 (UTC)

::: For whatever reason, I was not seeing that "Frequencies" table when I wrote my "4 Sept" response. I see it there, now, and so I withdraw that objection. (That said, my memory is also that I wrote that response on a different date - so, what do I know?) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 20:09, 12 September 2016 (UTC)

:::: Great, so I take it you too '''support that the task should be amended along these lines'''. For now, the vote is '''''2'' YESes''' and  '''''0'' NOs'''.  I think we should try to draw more attention to this from the participants on this page by posting messages on their talk pages. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 18:29, 16 September 2016 (UTC)

::::: Put me down for "abstaining from the vote", actually. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 18:53, 16 September 2016 (UTC)

:::::: Updated votes: YES: 1, NO: 0, ABSTAIN: 1. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 19:49, 16 September 2016 (UTC)

:::::: Updated votes: YES: 1, NO: 1, ABSTAIN: 1. -- [[User:WillNess|WillNess]] ([[User talk:WillNess|talk]]) 20:30, 16 September 2016 (UTC)
