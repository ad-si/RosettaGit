+++
title = "Talk:Prime decomposition"
description = ""
date = 2019-06-18T12:19:06Z
aliases = []
[extra]
id = 2530
[taxonomies]
categories = []
tags = []
+++

==C==
Could someone explain the C example a bit more (either in text around it or in comments)? It's using some things I think may be a bit unconventional. Also, does it actually return some sort of collection which contains the factors? The task says it should. --[[User:Mwn3d|Mwn3d]] 09:17, 5 February 2008 (MST)

It prints out the factors seperated by * to stdout, In the context of unix, where everything is a text stream this counts as a collection.
Why do you think it's unconventional, If you haven't used libgmp it may look strange.

==C==
This method seems to be incorrect. E.g. it does not find the decomposition for 2^41 - 1 == 13367 * 164511353. Can someone confirm this?
--[[User:Renfield|Renfield]] ([[User talk:Renfield|talk]]) 12:18, 18 June 2019 (UTC)

==Java==
Also the java example doesn't work for all integers > 1, maybe it could be fixed using the java bignum lib.
:I added a BigDecimal example, though I don't think anyone will ever need to go beyond Double.MAX_VALUE. If they want to, they shouldn't be using Java. Also, sign your talk page posts please. See [[Help:Formatting]] for tips. --[[User:Mwn3d|Mwn3d]] 11:08, 5 February 2008 (MST)

==J==
I note the J example simply calls a built-in - is that allowed? The task is kinda vague: "write a function that..." which could well include access to some language builtin. Or is the intent to show how one would solve the actual problem in that language? [[User:Sgeier|Sgeier]] 11:32, 6 February 2008 (MST)
:The vagueness is fine. Do it as simply as you see fit. If your language has prime decomposition built-in then that just makes it easier. --[[User:Mwn3d|Mwn3d]] 11:44, 6 February 2008 (MST)

==Big Numbers==
The OCaml and Python versions are incorrect, wrt big numbers?
:Python integers automagically extend. For example:
    >>> 2**1234
    295811224608098629060044695716103590786339687135372992239556207050657350796238924261053837248378050186443647759070955993120820899330381760937027212482840944941362110665443775183495726811929203861182015218323892077355983393191208928867652655993602487903113708549402668624521100611794270340232766099317098048887493809023127398253860618772619035009883272941129544640111837184L
    >>>
:--[[User:Paddy3118|Paddy3118]] 06:36, 14 August 2008 (UTC)

The Python code has comparisons with sys.maxint in one of the functions, but the
comparisons are really not necessary.  You'ld probably not want to use the algorithm
in real code, though, as it's not the fastest. Pythonic, yes. Quick, no.
--[[User:64.238.49.65|64.238.49.65]] 15:04, 17 August 2008 (UTC)
: Oh yea. I missed that! (I didn't write the original, and should have looked closer). --[[User:Paddy3118|Paddy3118]] 17:32, 17 August 2008 (UTC)

==How to run==
Could some please explain how to run the haskell implementation? (I am also curious how the 'primes' are used
in the factorize method.) [[User:Rahul|Rahul]] 11:09, 23 September 2008 (UTC)

== Notes ==

I believe two task requirements put focus out of the "prime decomposition" aim:
* returning an array or collection: stress on how to handle a growing/dynamic array/collection for those language that do not handle arrays/collections as easily as Python, Perl, Octave, J... and any other having an "evoluted" array type... or on how to determine previously the number of prime factors...
* big nums: stress on the usage of an extern lib for big nums if the language does not handle them ''innerly''; easy if they exist widely available bindings e.g. to the GMP or similar; harder if not...

C, Fortran (and likely more) can't accomplish the array-requirement easily (of course they can... but self-made array handling code will be needed...); for Fortran, I've failed (for now) using GMP bindings...

To me, both requirements should be dropped (bignums and the use of "growing arrays" should be other tasks) --[[User:ShinTakezou|ShinTakezou]] 14:09, 7 April 2009 (UTC)

I also think that bignum/growing array requirements should be dropped.  At the very least, the title of the page should indicate that big numbers are part of the challenge. --[[User:Showell|Showell]] 07:33, 5 January 2012 (UTC)
:Agreed. The task is asking far too much. Integer factorization is one of those basic programs one learns early, in any language. But if bignums are required, it's entirely another matter: first one needs a library (relatively hard), then one needs a good factorization algorithm (very hard). [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 23:28, 17 December 2017 (UTC)

== NZMATH ==

here's a Python 3 example using NZMATH modules--[[User:Billymac00|Billymac00]] 03:11, 3 January 2011 (UTC)

```python

#  snippet.py Python 3 to demo NZMATH ops ref: http://tnt.math.se.tmu.ac.jp/nzmath/
import sys
sys.path.append(r'C:\Python31\Lib')                  
sys.path.append(r'C:\Python31\Lib\site-packages')    

from nzmath import prime 
from nzmath import arith1    

print("factors of 64 2 ways: ")
print(prime._factor(64))    # returns [ (2 , 6) ]
print("")
print(prime.properDivisors(64))    # returns [2, 4, 8, 16, 32]

```


: You'd need to convert their format for the task, though: 64 needs to return <code>[2, 2, 2, 2, 2, 2]</code>.
: I did the same thing in [[Prime decomposition#PARI/GP|my PARI/GP solution]].
: [[User:CRGreathouse|CRGreathouse]] 19:57, 14 June 2011 (UTC)

== Propose to remove C GMP code ==

The GMP version of the C code is ''bad''.  It can't realistically handle any number with a prime factor that's larger than 64 bit (so it's pretty pointless to use GMP to begin with), is written in a convoluted way, and leaks memory.  Keeping it here only serves as a bad influence.  If there are no objections soon, I'll delete it.  --[[User:Ledrug|Ledrug]] 03:39, 4 August 2011 (UTC)

== C edits: can't be more than 8 ==

I'm reverting the last change of array size from 8 to 30, since it's mathematically impossible to have more (or fewer than) 8 numbers after each run.  If you disagree because some diagnostic software says other wise, show me where it will fail. --[[User:Ledrug|Ledrug]] 20:59, 7 September 2011 (UTC)

: The program fails because it crashes and dumps core! However, I fixed it wrong and made the size too large: it only needs to be 9, not 30.

: 
```c
	for (i = 1, q = p; i <= 30; i++, q += p) {
		if (!(b[n] = bit_pos[q % 30])) continue;
		b[n] = ~b[n];
		shift[n++] = q / 30;
	}
```


: I missed that bit_pos[] has only 8 nonzero elements. After this loop fills b[0] to b[7], it continues to assign b[8] = 0, because the assignment is before the <code>continue</code> statement. Therefore, program must declare array b[9] to hold elements b[0] to b[8]. --[[User:Kernigh|Kernigh]] 22:29, 7 September 2011 (UTC)
:: Ok.  Don't edit it yet though, I'll modify that part of the logic soon. --[[User:Ledrug|Ledrug]] 22:32, 7 September 2011 (UTC)

== Factor missing from 15 November 2001 to 23 March 2012 ==

An anonymous user [http://rosettacode.org/mw/index.php?title=Prime_decomposition&diff=next&oldid=125923 accidentally deleted the Factor example] at '''15 November 2001'''. Rosetta Code never caught this mistake and never restored the deleted code. Nickolas [http://rosettacode.org/mw/index.php?title=Prime_decomposition&diff=134165&oldid=132999 contributed a new Factor example] at '''23 March 2012'''. --[[User:Kernigh|Kernigh]] 15:03, 23 March 2012 (UTC)

: '''2001'''?   I thought ''Rosetta Code'' was inaugurated in '''2007'''. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 13:37, 16 October 2013 (UTC)

== Javascript ==

Javascript implementation without libraries fails with 100 as an argument. It decomposes to 2,2,25
