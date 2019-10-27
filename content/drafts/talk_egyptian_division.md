+++
title = "Talk:Egyptian division"
description = ""
date = 2017-08-24T22:18:50Z
aliases = []
[extra]
id = 21559
[taxonomies]
categories = []
tags = []
+++

==Perhaps no need to ask for more than one array ?==

The description seems attached to the use of two distinct arrays – does that seem necessary ?
( One list/array of tuples/pairs/records might seem more natural in some languages) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 14:49, 10 August 2017 (UTC)
:Indeed. In the Perl6 example, I use an array of pairs rather than two separate arrays. Seems to me that _how_ the values are stored is an implementation detail that isn't critical to the task. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 15:18, 10 August 2017 (UTC)

: Two arrays, one table of "pairs" would be fine. I would like a such a semblance of the description to be used to aid in example comparison . Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:24, 10 August 2017 (UTC)

:: Fair enough – it's essentially a hylomorphism [https://en.wikipedia.org/wiki/Hylomorphism_(computer_science)], and it seems reasonable / sufficient to ask people not to fuse the building up and the stripping down to the extent that the intermediate data structure is concealed. In the Haskell, JS, and AppleScript versions I've pulled that out and named it 'rows' rather than fusing it out of sight. I hope that seems enough to you :-) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:09, 10 August 2017 (UTC)
::: A fused composition of the unfold and the fold, in which the rows were implicit but not directly visible, might look something like:
::::
```Haskell
import Data.List (unfoldr)

egyptianQuotRem
  :: Integral a
  => a -> a -> (a, a)
egyptianQuotRem m n =
  foldr
    (\(i, x) (q, r) ->
        if x < r
          then (q + i, r - x)
          else (q, r))
    (0, m) $
  unfoldr
    (\(i, x) ->
        if x > m
          then Nothing
          else Just ((i, x), (i + i, x + x)))
    (1, n)

main :: IO ()
main = print $ egyptianQuotRem 580 34
```
 [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 19:35, 10 August 2017 (UTC)
::::Is this the solution you are going with? On the page you write 'In Haskell we could lazily take the rows we need from an infinite list'. The task requires that you work backwards through the list, a clever trick on an infinite list. The list has been fully and non lazily realized in rows before the calculation begins. Is (2 ^) exponentiation? see below--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:21, 11 August 2017 (UTC)
:::::Not quite the solution I am going with – I've pulled the composition of foldr and unfoldr apart, to expose a value named 'rows'. I take your point about using exponentiation, and it gives me an excuse to remove the first of my 3 versions – the one which started by taking a finite number of rows from an infinite list. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 14:22, 11 August 2017 (UTC)

==Number Systems==
The Egyptian number system essentially precludes multiplication and division, but they required both. They therefore designed methods that required only addition. I am willing to accept that n*2 is essentially equivalent to n+n but I must take exception to 'dbls.append(pwrs[-1] * divisor)' in the Python solution. They could not have multiplied by a variable like (say 34). As for exponentiation, well let's just say the Perl 6 is not Egyptian--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:01, 11 August 2017 (UTC)
: This makes a nice point, and perhaps suggests a simpler and deeper formulation for the task description ? I've used it as a welcome excuse to delete the first of my 3 Haskell versions, which included an exponentiation operator.
: Given that the 'Egyptian' algorithm is specifically a derivation of division from addition and subtraction, and assumes a dearth of more ambitious arithmetic operators, perhaps an edit (to that effect) to the task description – requiring parsimony in the use of arithmetic operators – no multiplication, division or exponentiation operators ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 14:33, 11 August 2017 (UTC)

: So what's your point? Ancient Egyptians didn't use multiplication an exponentiation? I somehow doubt they used digital computers, Latin letters or Arabic numerals either; yet every example posted so far uses all of these. Horrors. The algorithm given in the task description ''specifically'' states to use multiplication and exponentiation to generate the table. In addition, the task isn't "Generate a table using methods an ancient Egyptian would and then use that table to do Egyptian division." If we're going to get into petty, pedantic nit-picking, I could point out that the F# example is incorrect because it doesn't display the specific example solution required by the task. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 15:15, 11 August 2017 (UTC)
:: The Perl and Python versions are absolutely consistent with the task description, and of course there's not much to be gained from antiquarianism or reconstructive archaeology here :-)
:: The kernel of Nigel's point does, however, seem interesting and fruitful to me – the heart, motivation, and logic of the algorithm is precisely to derive division from more primitive operators – and I think we might get a simpler (and perhaps more interesting ?) task description by foregrounding that, even tho it would cost us some tweaks to some of our first drafts. Asking people to show how one could formalise this algorithm, in a given language, '''purely in (arithmetic) terms of addition/subtraction''', seems like a more interesting task constraint, for example, than over-specifying the intermediate data structure.  [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 17:19, 11 August 2017 (UTC)

:: The point is that if you use exponentiation and multiplication you can generate each row of the table as needed, so the requirement to first create the table and store it is just a pointless restriction. The beauty is that the table can be produced just by adding the previous row to itself.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:49, 12 August 2017 (UTC)

That is an interesting point by Nigel, and some nice replies. I originally was debating adding extra information on how the table could be created by bit shifting, and the summations gave the binary result, but left all that guff out for a simpler task. I had not thought of restricting mathematical operators in Nigel's manner - now I know, it would be a valid but different task. I don't think it would be good to change the current task however.  --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:06, 11 August 2017 (UTC)

:P.S. Nigel, we tend not to sign examples on the task page, relying on the page history to track authorship. This also encourages others to correct errors they may spot etc. It's the wiki way on RC :-) --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:13, 11 August 2017 (UTC)
:: How about just adding a simple 'extra credit' clause – 'Show that Egyptian division can be performed in your language without making any use of multiplication, division or exponentiation operators' [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:55, 11 August 2017 (UTC)

==Haskell 'lazy lists' example==

Two thoughts about the current draft of this:
First, to avoid using (*) (in this exercise in deriving division from addition and subtraction), the function which you '''iterate''' for 'doubling' could either be <code Haskell>(+) >>= id</code> or the equivalent <code Haskell>join (+)</code> (where join is imported from Control.Monad).
Second, perhaps the task really needs a formulation which returns a remainder as well as a quotient ?
