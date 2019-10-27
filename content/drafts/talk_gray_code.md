+++
title = "Talk:Gray code"
description = ""
date = 2015-05-30T14:13:04Z
aliases = []
[extra]
id = 9377
[taxonomies]
categories = []
tags = []
+++

== D encoding ==

Am I reading the D encoding right? Gray code is just the number xor'd with its right shifted value? If so, my mind is blown. --[[User:Mwn3d|Mwn3d]] 04:41, 18 March 2011 (UTC)
:It has been stated in wikipedia :) [[User:Dingowolf|dingowolf]] 11:17, 18 March 2011 (UTC)
::Wow. Reading is hard. I'm definitely changing the Java version. --[[User:Mwn3d|Mwn3d]] 13:04, 18 March 2011 (UTC)
:::The only tricky bit in decoding is that you need the previously decoded bit to decode the next one. That makes everything just that ''little'' bit messier (as evidenced by the many strategies shown for decoding!) though it does mean that everything admits a rather nice decode loop if you're cunning enough. (Decoding tables? O RLY? SRSLY?) –[[User:Dkf|Donal Fellows]] 14:47, 18 March 2011 (UTC)

== Decoding a bitstream ==

Decoding individual symbols looks trivial; you could do it with a lookup table. What would decoding a Gray code bitstream look like? Finding the the beginning of each symbol might be a tad trickier. --[[User:Short Circuit|Michael Mol]] 13:13, 18 March 2011 (UTC)

==Python bin<>int translations==
Someone has just modified the Python bin2int function to use intermediate 'string' functions. (The analogous dual of the int2bin function would be to parse and split the output of bin(n) then turn the split bits into a list of integers i.e: <code>[int(b) for b in bin(n)[2:]]</code>)

* I had thought of doing that myself and rejected it as I didn't want to introduce string types as well, so keeping the focus more on the gray code conversions. 

* Using the 'string' versions does make the functions smaller however.

I did time the two versions of bin2int and the new version (bintint2 below) is 3 times slower so there is a cost.

```python>>>
 Timer('for b in n: bin2int(b)', setup).repeat(1)
[6.267737098708267]
>>> setup = '''
def bin2int(bits):
	'From binary bits, msb at index 0 to integer'
	i = 0
	for bit in bits:
		i = i * 2 + bit
	return i

def bin2int2(bits):
	'From binary bits, msb at index 0 to integer'
	return int(''.join(str(b) for b in bits), 2)

import random
n = [[1] + [random.choice((0,1)) for i in range(31)] for j in range(10)]

'''
>>> Timer('for b in n: bin2int(b)', setup).repeat(1)
[60.964307340517905]
>>> Timer('for b in n: bin2int2(b)', setup).repeat(1)
[188.2841723752312]
```


Personally I would prefer to use the none-string versions, but I can't decide if the string versions are easier to read/maintain. There is no speed penalty when considering the needs of the RC task. I do think that the original versions might be readable by a larger % of the people that are trying to understand Gray coding through the referenced animation though. --[[User:Paddy3118|Paddy3118]] 09:26, 23 March 2011 (UTC)

== Formatting ==

Why revert my edit?  I wouldn't have made it if it didn't change anything.  Before the edit,  the beginning of the document reads like this:


```txt
Gray code is a form of binary encoding

where transitions between consecutive numbers differ by only one bit. 
```


After my edit, it looks like this:


```txt
Gray code is a form of binary encoding where transitions between consecutive numbers differ by only one bit.
```


... that seems like a difference to me. --[[User:Markjreed|Markjreed]] ([[User talk:Markjreed|talk]]) 21:39, 29 May 2015 (UTC)

:My apologies. I carefully looked at the before and after but must have my browser width set at a size that masked the difference or something. Sorry. I've reverted my reversion--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:50, 30 May 2015 (UTC)

::No worries, I was just confused.  Thanks!  --[[User:Markjreed|Markjreed]] ([[User talk:Markjreed|talk]]) 14:13, 30 May 2015 (UTC)
