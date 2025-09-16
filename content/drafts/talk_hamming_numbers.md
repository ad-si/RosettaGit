+++
title = "Talk:Hamming numbers"
description = ""
date = 2016-05-23T06:41:07Z
aliases = []
[extra]
id = 5031
[taxonomies]
categories = []
tags = []
+++

==a list of references to add ...==
I have a list of references to add, plus another one or two Python algorithms. --[[User:Paddy3118|Paddy3118]] 18:40, 2 December 2009 (UTC)
:Done. --[[User:Paddy3118|Paddy3118]] 21:55, 2 December 2009 (UTC)

== Off-by-one error? ==

Since hamming(1692) = 2^31, the last one before 2^31 is hamming(1691). I changed it in the problem description. --[[User:Dsnouck|Dsnouck]] 08:56, 3 December 2009 (UTC)
: Originally I had the 1691. Tcl had 1690 so I stored all values in an array and found that 1690 was correct. Since I have checked twice, I have reverted your edit, but please check again (as I will tonight). --[[User:Paddy3118|Paddy3118]] 09:09, 3 December 2009 (UTC)
:: I still believe my original remark was correct. I am not going to re-revert. I changed my Scheme program to show some extra output. Maybe some people that submitted other implementations can also check this. We actually do agree on the value of hamming(1690). It's just that this is not the last one before 2^31. Maybe there is something wrong with my implementation. At least we agree on the first 20 :). --[[User:Dsnouck|Dsnouck]] 09:22, 3 December 2009 (UTC)
::: FWIW, calculating with the Tcl impl...
```txt

:::hamming{1690} = 2123366400
:::hamming{1691} = 2125764000
:::hamming{1692} = 2147483648
:::hamming{1693} = 2149908480
:::
```
My only concern is whether I had an off-by-one error from counting indices from zero or one (i.e., is it ''H''<sub>0</sub> or ''H''<sub>1</sub> that is 1? My impl assumes it is ''H''<sub>1</sub>...) –[[User:Dkf|Donal Fellows]] 10:20, 3 December 2009 (UTC)
:::: So I think it is safe to say that we agree on the value of the 1690th Hamming number. Here it doesn't matter wheter indexing is zero-based or one-based. If we agree that the first Hamming number is 1, it is clear what we mean by the 1690th Hamming number. The only difference between zero-based indexing compared to one-based is that the first Hamming number is called hamming(0) in the former case and hamming(1) in the latter. Similarly for the 1690th Hamming number: with zero-based indexing it is called hamming(1689) as compared to hamming(1690) with one-based indexing. Anyway, to me it still looks like the last Hamming number before 2^31 is the 1691th, since the 1692th Hamming number is equal to 2^31. --[[User:Dsnouck|Dsnouck]] 10:48, 3 December 2009 (UTC)
::::: Hence the error is in the task itself. But it's not serious; any implementation that can compute one ought to be able to do the other (and if it can't... well, that's pretty poor). Doing the millionth though, that takes a bit more computation.  Especially given that it has 84 digits in decimal notation. –[[User:Dkf|Donal Fellows]] 11:15, 3 December 2009 (UTC)


Calculations using the Python implementation give:

```python
>>>
 # Create a zero-based list of the Hamming numbers
>>> h = list(islice(raymonds_hamming(), 1695))
>>> # Show some of the vaules in one-based, (as well as zero based) indexing
>>> for i in chain(range(20), range(1689,1693)):
	print "(i=%4i), n=%4i, h(n)=%10i, (h(n)<2**31) = %s" % (i, i+1, h[i], h[i]<2**31)

	
(i=   0), n=   1, h(n)=         1, (h(n)<2**31) = True
(i=   1), n=   2, h(n)=         2, (h(n)<2**31) = True
(i=   2), n=   3, h(n)=         3, (h(n)<2**31) = True
(i=   3), n=   4, h(n)=         4, (h(n)<2**31) = True
(i=   4), n=   5, h(n)=         5, (h(n)<2**31) = True
(i=   5), n=   6, h(n)=         6, (h(n)<2**31) = True
(i=   6), n=   7, h(n)=         8, (h(n)<2**31) = True
(i=   7), n=   8, h(n)=         9, (h(n)<2**31) = True
(i=   8), n=   9, h(n)=        10, (h(n)<2**31) = True
(i=   9), n=  10, h(n)=        12, (h(n)<2**31) = True
(i=  10), n=  11, h(n)=        15, (h(n)<2**31) = True
(i=  11), n=  12, h(n)=        16, (h(n)<2**31) = True
(i=  12), n=  13, h(n)=        18, (h(n)<2**31) = True
(i=  13), n=  14, h(n)=        20, (h(n)<2**31) = True
(i=  14), n=  15, h(n)=        24, (h(n)<2**31) = True
(i=  15), n=  16, h(n)=        25, (h(n)<2**31) = True
(i=  16), n=  17, h(n)=        27, (h(n)<2**31) = True
(i=  17), n=  18, h(n)=        30, (h(n)<2**31) = True
(i=  18), n=  19, h(n)=        32, (h(n)<2**31) = True
(i=  19), n=  20, h(n)=        36, (h(n)<2**31) = True
(i=1689), n=1690, h(n)=2123366400, (h(n)<2**31) = True
(i=1690), n=1691, h(n)=2125764000, (h(n)<2**31) = True
(i=1691), n=1692, h(n)=2147483648, (h(n)<2**31) = False
(i=1692), n=1693, h(n)=2149908480, (h(n)<2**31) = False
```


Since we are using 1 based indices for h(n) then the task should use '''1691'''. --[[User:Paddy3118|Paddy3118]] 16:39, 3 December 2009 (UTC)

==J Solution limitations==
There is no indication of how the J solution is used to produce the 1691'st Hamming number or the millionth. (Or if the algorithm used will work for larger values). --[[User:Paddy3118|Paddy3118]] 15:05, 5 January 2010 (UTC)
: O, I didn't realize this. But the millionth will be a problem. So, should I retreat my solution? --[[User:Gaaijz|Gaaijz]]
:: Hi, No, keep it in. You might want to mention if the millionth is not achieved due to lack of multi-precision integer arithmetic or excessive run-time though. Thanks. --[[User:Paddy3118|Paddy3118]] 05:43, 6 January 2010 (UTC)

==Changes to the Java algorithm==
Originally the Java algorithm kept the whole sequence in memory. While that could be useful, it ran into out-of-memory errors quite easily.
My contribution was to split the Hamming sequence memory into three buffers: one for Hamming numbers to multiply by two, one for Hamming numbers to multiply by three and another for Hamming numbers to multiply by five. Hamming numbers which were already multiplied can be forgotten (resetting them to null frees memory occupied by BigInteger).

The first buffer contains only numbers with factor two (they are powers of two) since that is always the smallest unique Hamming number that can appear in that buffer e.g. 6 = 2*3 multiplied by two is 12 = 2*2*3, but 4 = 2*2 is a smaller Hamming number which appears in the "multiply by three" buffer so we don't need 6 in the "multiply by two" buffer.
The "multiply by three" buffer contains only numbers with factors two and three for the same reasons. The "multiply by five" buffer contains numbers with factors two, three and five. That division in three buffers prevents duplicates when merging. Note that it can be generalised to 7-smooth numbers, 11-smooth etc.

Looking at the algorithm, the "multiply by two" buffer never grows -- it always contains exactly one element. So it can be eliminated. The "multiply by three" buffer never receives more than half the limit numbers, which is what I use as buffer size. Since this buffer only grows with powers of two, we could use a circular buffer sized after log2(limit) but that would complicate the code.

I don't know if the above comments were in reference to the previous version, but the version on the page would NPE when I ran it with a parameter of 20. I replaced it with a new version. I think what I came up with is kinda like a lazy merge. --[[User:Paul.miner|Paul.miner]] 22:46, 29 December 2010 (UTC)

Removed all the coefficients stuff (values of i, j, and k are not needed), and updated code to not save the entire list of one million Hamming numbers so it could be run with the client VM. --[[User:Paul.miner|Paul.miner]] 23:19, 29 December 2010 (UTC)

== Original DrDobbs blog discussion ==

Just for the record, I would like to reclaim authorship of that snippet of pseudocode from the DDJ discussion back then, quoted in the Python section that apparently started this whole page. :) No consequences other than to state it here for the record - that link is broken now, gone dead after DDJ moved their blog system to some "new implementation with new and exciting features" which included losing all the old contents apparently.

Two interesting related observations. One, very minor, I can now explain the spaces in <code>x2=2*h[ i ];</code> in the quoted pseudo-code: the old blog system at DDJ would interpret [i] ... [/i] as markers for ''italics''. Another - for me, somewhat major - is that while I came out with that pseudo-code trying to translate the classic ''Haskell'' merging-the-mappings code back into something C-like in my mind, as it turns out, it is in ''almost exact verbatim correspondence'' with the original Edsger Dijkstra's code in his book (IIRC), which I stumbled upon much later, by chance. (I had a link to it somewhere, will add later.) Amazing how it came back in an almost exact loop, this idea, back to where it started - "to Haskell and back!". Interesting... :) [[User:WillNess|WillNess]] 21:06, 20 August 2012 (UTC)

:Thanks for the original inspiration :-)
--[[User:Paddy3118|Paddy3118]] 21:41, 20 August 2012 (UTC)

::Thank ''you'' for getting inspired and making this page happen! :) The book in question is "A Discipline of Programming", Prentice-Hall, [http://web.cecs.pdx.edu/~cs410aph/Lectures/Smalltalk%20II/Dijkstra%20on%20Hamming's%20Problem.pdf chap. 17 "An Exercise Attributed to R.W.Hamming"], [http://i.imgur.com/tPygG.gif pg. 132]. -- [[User:WillNess|WillNess]] 17:40, 29 August 2012 (UTC)

== "Cyclic generator variant #1" not efficient ==

The Python [[Hamming numbers#Cyclic generator variant #1.|Cyclic generator variant #1.]] that is said to be much simpler, is not really equivalent to the other solutions. I compared it to another version, measuring how many times the <code>merge</code> iterator is advanced:


```python
from itertools import tee, chain, groupby, islice
import heapq

count = 0

def merge(*args):
    global count
    for x in heapq.merge(*args):
        count += 1
        yield x

def raymonds_hamming():
    # Generate "5-smooth" numbers, also called "Hamming numbers"
    # or "Regular numbers".  See: http://en.wikipedia.org/wiki/Regular_number
    # Finds solutions to 2**i * 3**j * 5**k  for some integers i, j, and k.
 
    def deferred_output():
        for i in output:
            yield i
 
    result, p2, p3, p5 = tee(deferred_output(), 4)
    m2 = (2*x for x in p2)                          # multiples of 2
    m3 = (3*x for x in p3)                          # multiples of 3
    m5 = (5*x for x in p5)                          # multiples of 5
    merged = merge(m2, m3, m5)
    combined = chain([1], merged)                   # prepend a starting point
    output = (k for k,g in groupby(combined))       # eliminate duplicates
 
    return result
 
print list(islice(raymonds_hamming(), 100))
print count
```



```python
from itertools import tee, chain, groupby, islice
import heapq

count = 0

def merge(*args):
    global count
    for x in heapq.merge(*args):
        count += 1
        yield x

def hamming_numbers():
    last = 1
    yield last
 
    a,b,c = tee(hamming_numbers(), 3)
 
    for n in merge((2*i for i in a), (3*i for i in b), (5*i for i in c)):
        if n != last:
            yield n
            last = n
 
print list(islice(hamming_numbers(), 100))
print count
```


The results I get:
{| class="wikitable"
|-
! number of items printed !! raymonds_hamming !! hamming_numbers
|-
| 20 || 28 || 59
|-
| 100 || 201 || 670
|-
| 1,000 || 2,506 || 17,822
|-
| 10,000 || 27,634 || 420,168
|-
| 100,000 || 288,851 || 9,429,734
|}
The new "Cyclic generator variant #1" version seems to be much, much worse. --[[User:Spoon!|Spoon!]] 20:52, 12 October 2012 (UTC)

: The '#1' variant is indeed doing something completely different.  Original code's <code>tee()</code> creates multiple copies of a generator, but does this exactly once per <code>raymonds_hamming()</code> call.  The '#1' variant is actually recursive: caller calls <code>hamming_number()</code>, which in turn calls <code>tee()</code> and creates copies ... which unfortunately calls <code>hamming_number()</code> again with a new closure, which calles <code>tee()</code> and makes new sets of copies, recursively.  Either the author on programmingpraxis site misunderstood how <code>tee()</code> works, or he simply wanted brevity (much) more than efficiency. --[[User:Ledrug|Ledrug]] 03:19, 13 October 2012 (UTC)

== Captha unusable ==

I've been spending over ten attempts to correct an error at the Fortran module, but neither listening nor reading helped a bit.

So I can't add this link again at Fortran, sorry: //fortran.com/big_integer_module.f95

This way the whole darn thing is pretty much counterproductive.

: It's not just you. I've been having problems with captha also. One of the manifestations is that captha arrives on the scene, but it doesn't show me the two fuzzy words.  So I click on the audio clues, which are totally worthless (or should I say  maaaaarrr cwooooorrrrthleeeesssss...), but then I click on the big '''T'''  (for text), and sometimes it then gives me two fuzzy words.  If not, I ask for two more (different) fuzzy words, and repeat the cycle over (and over and over and over) again until captha thinks I get it right, either that, captha gets tired of me ... I don't know. Captha acts like an imp or a gremlin really interested in frustrating programmers. Also, if you get a couple of fuzzy words that you correctly answered previously, it will now fail, which is really, really frustrating. I think captha has a mean wicked sense of humor, and it's just screwing with ya. If you get a couple of fuzzy words that you're responded to (correctly or "incorrectly"), ignore them and ask for two other (new) fuzzy words.  Don't waste your time if you  KNOW  the answer (from a previous attempt).  It doesn't matter, the answer(s) will be rejected and most of the time, it presents a new pair of fuzzy words (or maybe not).  If I were high on something, this could be construed as entertainment, playing Russian roulette with fuzzy words. Hitting your head with a hammer is much more fun, plus it feels soooooo good when you stop. If I sound too sarcastic, it's because I wasted over an hour today ... probably my fault, I entered a whole mess of stuff, and I didn't want all my editing to go down the toilet without a fight. I won, but a lot of hair got pulled out, and I don't have that much to spare. Perhaps someone should start a Village Pump dialogue? -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:02, 18 March 2014 (UTC)

: I forgot to say that this happens using Microsoft Windows Internet Explorer, or my main squeeze, Firefox Aurora. This is under Windows/XP with a fibre-optic internet connection. Also, I found out that it helps to shut almost everything else down that can be shut down (just applications, not services). Crossing your fingers and toes doesn't help. Time of day doesn't seem to matter. Root canal is more enjoyable. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:09, 18 March 2014 (UTC)

:: I mostly ignore the audio, and just request another image until I find one with low enough ambiguity I am comfortable trying it. I have not tried root canal. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 00:03, 19 March 2014 (UTC)
