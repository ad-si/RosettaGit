+++
title = "Talk:Entropy"
description = ""
date = 2016-10-16T23:09:58Z
aliases = []
[extra]
id = 12958
[taxonomies]
categories = []
tags = []
+++

== What about a less dull example? ==


What about using "Rosetta code" as an example instead of the dull "1223334444"?--[[User:Grondilu|Grondilu]] 17:53, 22 February 2013 (UTC)
: Or even funnier:  write a program who computes its own entropy :) --[[User:Grondilu|Grondilu]] 12:40, 25 February 2013 (UTC)
::Better yet, a bonus task of computing the entropy of each solution on the page. :) --[[User:TimToady|TimToady]] 19:23, 25 February 2013 (UTC)
::: I like computing the entropy of “<tt>Rosetta Code</tt>” (it's about 3.08496, assuming my code is right); a more self-referential one is fine too, except it involves features that might block some languages from participating. (The draft [[Entropy/Narcissist|child task]] is a better place for that.) –[[User:Dkf|Donal Fellows]] 09:31, 26 February 2013 (UTC)
:::: I have added a task [[Fibonacci_word]] which I think is an interesting application of Entropy--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 15:53, 28 December 2013 (UTC)

== Alternate form ==

Not sure this is very useful, but I was wondering if one could not find a more concise way of writing this.

If we call <math>N</math> the length of the string and <math>n_c</math> the number of occurrences of the character c, we have:

<math>H = \sum_c -p_c \ln p_c = \sum_c -\frac{n_c}{N} \ln \frac{n_c}{N} = \ln N - \frac{1}{N}\sum_c n_c\ln n_c </math>

In perl6, this allows a slightly simpler formula, i.e. not using hyperoperators:


```Perl 6
sub entropy(@a) {
    log(@a) - @a R/ [+] map -> \n { n * log n }, @a.bag.values
}
```


For what it's worth.--[[User:Grondilu|Grondilu]] 18:14, 4 March 2013 (UTC)

== Description ==

I think the task is not described correctly. It calculates the average entropy ''per character'', not the entropy of the message as a whole.

For example, I generated this 100-digit string with 0 through 7 from random.org. It has 300 bits of entropy (more conservatively, 299 to 300 bits of entropy, in case the generation is slightly flawed). The task gives it just under 3:

```parigp
entropy("1652410507230105455225274644011734652475143261241037401534561367707447375435416503021223072520334062")
%1 = 2.9758111700170733830744745234131842224
```


[[User:CRGreathouse|CRGreathouse]] ([[User talk:CRGreathouse|talk]]) 02:08, 17 April 2013 (UTC)

:I don't know.  It's not simple.  I was a bit embarrassed with this task as I struggled to understand what the entropy of a string is.  Normally entropy is defined for a system whose internal state is only known probabilisticly.  A string is known exactly so stricto sensu, the entropy of any string is zero.  As I understood it, the task defines the entropy of a string as the entropy of whatever system "generated" the string.  The string is thus seen as a representative samples of the probabilities of each possible character, seen as a random variable.
:In your example, you say the entropy is 300 bits, but I'm not sure it makes much sense to say that.  300 bits is the amount of information in this string, if you consider it as the result of picking a random number from 0 to 10^300.  Yet the entropy is still zero, for your string is exactly defined.
:The entropy as currently defined in the task does not directly depend on the "size" of the string in memory, because it merely represents the probability table of a stochastic process.
:--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 15:42, 17 April 2013 (UTC)

:: What I mean when I say 299-300 bits is that if we knew I was going to pick 100 digits from random.org as described we wouldn't be able to design a scheme that would average fewer than 299 bits to tell you which one I picked, but we could design one which would do it in 300. This task takes a slightly different model (if I was going to generate a 100-character string with a given number of each digit) but the two are similar. Either way the string has about 300 and each character has about 3.
:: [[User:CRGreathouse|CRGreathouse]] ([[User talk:CRGreathouse|talk]]) 00:55, 18 April 2013 (UTC)

: Yes, it was supposed to be bits/character and I've corrected it. Shannon called N the number of "symbols" and I call the number of different symbols "characters" (your number of c) to distinguish n "different symbols" from N symbols.  The entropy in bits (technically called shannons to indicate not only base 2 was used but that the function N*H was applied) is S<sub>2</sub>=NH<sub>2</sub> = 100*2.9758 = '''297 shannons'''.  H is intensive or specific entropy, S is extensive or total entropy.  The normalized specific entropy/symbol (the degree to which the symbols were equally frequently used, from 0 to 1) is H<sub>8</sub> = H<sub>n</sub> = H<sub>2</sub>*ln(2)/ln(8) = 0.992 in "octals/symbol" or "normalized entropy/symbol".  ln(2)/ln(8) changes from base 2 to base 8. The same distribution of characters with a different number n of characters would give the same H<sub>n</sub>.  The most objective "total entropy" is then S<sub>n</sub> = H<sub>n</sub> N = 99.2.  The are no units because this is a pure statistical measure of the distribution of the ratios times the number of symbols.  99.2 shows that only 1 of the 100 symbols was not perfectly "random" from an H-function perspective (which is VERY limited and basic because it does not try to do any compression, i.e. it does not try to look for patterns except for character frequency). But the last digit had to be chosen randomly without trying to make H=1. S<sub>n</sub> is the total "information" in the data if the observer of the data was not able to compress the data more. Compression has no perfect universal function so information depends on the observer's view, or his access to compression tools like gzip. So for general computer work, the compressed file is the amount of information in it.  See also my comments in a section below. 

:Physics is our best known (most efficient) set of non-lossy compression functions for the data observed in the world, which is a way of saying Occam's razor was applied.  Other sets of compression functions like religion + democracy + free markets are more intelligent than physics's functions if they result in more profit for the computing device implementing the functions (algorithms), in keeping with Marcus Hutter's definition of intelligence.  Deciding the most efficient way to move the symbols around with electrons in CPUs, ions in brains, votes in government, reputation in open source, or money in markets allows the computing device (CPUs implementing real A.I., brains, government, open source, markets) to most efficiently move the much larger real-world objects that the symbols represent so that the computing device can more efficiently make copies of itself (seek lower entropy). This is how entropy relates to information and how information relates to intelligence and evolution which are the result of least action dynamics seeking lower physical entropy in the distribution of N atoms on Earth, as the excess entropy is sloughed off to space with 17 low-energy un-directed photons per incoming sun-directed photon, so that the Universe can expand (entropy and energy per comoving volume of the universe is constant). The result is higher energy bonds (lower entropy due to smaller volume) of the N atoms on Earth of n types, which is why machines that utilize metal and metalloid (e.g. silicon) atoms with their previously attached oxygen atoms removed (resulting in carbon-carbon, metal-metal, silicon-silicon) are replacing biology: they are more efficient due to the higher energy low entropy bonds. This is why entropy is important. [[User:Zawy|Zawy]] ([[User talk:Zawy|talk]]) 10:33, 23 January 2016 (UTC)

== REXX (log2) ==

"The LOG2 subroutine in only included here for functionality, not to document how to calculate LOG2 using REXX"
:: What's the difference and where is it / should it be documented?

::: Difference between what?

::: The LOG2 function should/could be documented in a Rosetta Code task that asks for a user-written version of LN/LOG2/LOG (as opposed to one that is a BIF).   Documenting a subroutine that is a BIF (built-in function) for most languages would just make the LOG2 function much too bulky and detract from the task's requirement:   ''solve the entropy problem''.   The REXX LOG2 function (which is a modified LN function) was originally written to be compact and written as a ''one-liner'' function, intended to be placed at the end of the program that it was needed (it, and another hundred or so subroutines/functions --- that's no exaggeration).   Strictly speaking, the   '''E'''   value was originally a function, but it was abbreviated here (about 80 decimal digits as a constant rather than a function call) to save even more space and complexity.   Also omitted were error checks that were in the original LN function, as well as   SIGNAL ON NOVALUE;   SIGNAL ON SYNTAX,   SIGNAL ON HALT, and the mechanism of issuing/displaying the error message(s) and the offending REXX statements.   At lot of crafting went into the writing of the original LN function (this was under CMS REXX and then ported to PC/REXX in 1989, as I recall).   PC/REXX has some restrictions on the number of symbols, the aggregate size of the values of the variables, and the size of the REXX program. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:47, 28 May 2013 (UTC)

:: Take 'my' (now corrected - thanks) log as an example --[[User:Walterpachl|Walterpachl]]

::: If this Rosetta Code task was to explain how LN (or LOG2) was calculated, that would be appropriate. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:47, 28 May 2013 (UTC)

::: Here's a version of the (REXX) LOG2 function, unrolled and expatiated: -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:47, 28 May 2013 (UTC)

```rexx
/*──────────────────────────────────LOG2 subroutine───────────────────────────*/
log2: procedure;  parse arg x 1 xx
ig= x>1.5
is= 1 - 2*(ig\==1)
numeric digits digits()+5      /* [↓] precision of E must be > digits().*/
e=2.7182818284590452353602874713526624977572470936999595749669676277240766303535
ii=0
                  do  while  ig & xx>1.5  |  \ig & xx<.5
                  _=e
                         do j=-1
                         iz=xx* _**-is
                         if j>=0  then  if  ig & iz<1  |  \ig&iz>.5  then leave
                         _=_*_
                         izz=iz
                         end   /*j*/
                  xx=izz
                  ii=ii + is* 2**j
                  end   /*while*/
 x=x * e**-ii -1
 z=0
 _=-1
 p=z
                  do k=1
                  _=-_*x
                  z=z+_/k
                  if z=p  then leave
                  p=z
                  end   /*k*/
r=z+ii
if arg()==2  then return r
                  return r / log2(2,0)
```


==  This is not exactly "entropy". H is in bits/symbol. ==
This article is confusing Shannon entropy with information entropy and incorrectly states Shannon entropy H has units of bits.

There are many problems in applying H= -1*sum(p*log(p)) to a string and calling it the entropy of that string. H is called entropy but its units are bits/symbol, or entropy/symbol if the correct log base is chosen. For example, H of 01 and 011100101010101000011110 are exactly the same "entropy", H=1 bit/symbol, even though the 2nd one obviously carries more information entropy than the 1st.  Another problem is that if you simply re-express the same data in hexadecimal, H gives a different answer for the same information entropy. The best and real information entropy of a string is 4) below. Applying 4) to binary data gives the entropy in "bits", but since the data was binary, its units are also a true statistical "entropy" without having to specify "bits" as a unit.

Total entropy (in an arbitrarily chosen log base, which is not the best type of "entropy") for a file is S=N*H where N is the length of the file. Many times in [http://worrydream.com/refs/Shannon%20-%20A%20Mathematical%20Theory%20of%20Communication.pdf Shannon's book] he says H is in units of "bits/symbol", "entopy/symbol", and "information/symbol".  Some people don't believe Shannon, so [https://schneider.ncifcrf.gov/ here's a modern respected researcher's home page] that tries to clear the confusion by stating the units out in the open.

Shannon called H "entropy" when he should have said "specific entropy" which is analogous to physics' S<sup>0</sup> that is on a per kg or per mole basis instead of S.  On page 13 of [http://worrydream.com/refs/Shannon%20-%20A%20Mathematical%20Theory%20of%20Communication.pdf Shannon's book], you easily can see Shannon's horrendous error that has resulted in so much confusion.  On that page he says H, his "entropy", is in units of "entropy per symbol". This is like saying some function "s" is called "meters" and its results are in "meters/second". He named H after Boltzmann's H-theorem where H is a specific entropy on a per molecule basis.  Boltzmann's entropy S = k*N*H = k*ln(states).  

There 4 types of entropy of a file of N symbols long with n unique types of symbols:

1) Shannon (specific) entropy '''H = sum(count_i / N * log(N / count_i))''' 
where count_i is the number of times symbol i occured in N.  
Units are bits/symbol if log is base 2, nats/symbol if natural log.

2) Normalized specific entropy: '''H<sub>n</sub> = H / log(n).'''
The division converts the logarithm base of H to n. Units are entropy/symbol or 1/symbol. Ranges from 0 to 1. When it is 1 it means each symbol occurred equally often, n/N times. Near 0 means all symbols except 1 occurred only once, and the rest of a very long file was the other symbol. "Log" is in same base as H.

3) Total entropy '''S' = N * H.''' 
Units are bits if log is base 2, nats if ln()). 

4) Normalized total entropy '''S<sub>n</sub>' = N * H / log(n).'''  See "gotcha" below in choosing n.
No units in the same way a ratio does not have units. Notice the formula uses a ratio and the data itself instead of a person chooses the logarithm base. This is the total entropy of the symbols. It varies from 0 to N

5) Physical entropy S of a binary file when the data is stored perfectly efficiently (using Landauer's limit): '''S = S' * k<sub>B</sub> / log(e)'''

6) Macroscopic information entropy of an ideal gas of N identical molecules in its most likely random state (n=1 and N is known a priori):  '''S' = S / k<sub>B</sub> / ln(1)''' = k<sub>B</sub>*[ln(states^N/N!)] = k<sub>B</sub>*N* [ln(states/N)+1].

*Gotcha: a data generator may have the option of using say 256 symbols but only use 200 of those symbols for a set of data. So it becomes a matter of semantics if you chose n=256 or n=200, and neither may work (giving the same entropy when expressed in a different symbol set) because an implicit compression has been applied.

== possible typo in an equation ==
About in the middle of the task's preamble, there is a formula:

  The total entropy in bits of the example above is   S= 10*18.4644 = 18.4644   bits. 

Should the last number be    '''184.644'''   (instead)?   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:29, 16 August 2016 (UTC)

==Several formulae on task page now invisible to many browsers==

Various formulae on this page became invisible to many browsers (all those - the majority - which display the graphic file rather than processing the MathML directly), as a result of the 'tidying and spacing' edits at 19:40 Jul 14 2016. One of the issues is the editor's introduction of redundant white space around Latex expressions inside &lt;math&gt; tags. This white space is not currently supported by the MediaWiki processor. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 12:52, 19 September 2016 (UTC)

: Visibility of formulae to Chrome, IE/Edge, Safari etc restored by reverting task description to state before under-tested cosmetic edits of  19:40, 14 July 2016‎.  The editor may wish to restore some of these cosmetic edits, testing their real effects in Google, Microsoft and Apple's browsers, but the the most practical immediate solution is simply to return to the state before visibility of formulae was lost. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 23:09, 16 October 2016 (UTC)
