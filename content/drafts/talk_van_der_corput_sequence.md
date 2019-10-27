+++
title = "Talk:Van der Corput sequence"
description = ""
date = 2012-08-07T21:59:58Z
aliases = []
[extra]
id = 9351
[taxonomies]
categories = []
tags = []
+++

== Python output ==
Looks to me like the base 2 sample output for the Python example is actually base 3?--[[User:Tikkanz|Tikkanz]] 08:34, 11 March 2011 (UTC)
: Umm, my (very good) maths teachers defence in such situations was to say "Excellent lad, you've found the deliberate mistake"! :-)
: I'm at work at the moment but will correct the copy/paste error this evening. Thanks, --[[User:Paddy3118|Paddy3118]] 09:53, 11 March 2011 (UTC)
: Hmm? The text says base 3, numbers do look like base 2. Edit conflict? --[[User:Ledrug|Ledrug]] 07:03, 10 June 2011 (UTC)

== displaying of terms ==

In every reference I've looked at, the 2nd term of the van der Corput sequenct (for base two) is 

 .1

(not) .10000000


I suggest that trailing zeroes illegitimize the terms. Mathematically, of course,  .1 is equal to .100 (except to an engineer, where trailing zeroes signify more precision). -- [[User:Gerard Schildberger|Gerard Schildberger]] 03:28, 26 March 2012 (UTC)

==Generation of the image in the task description==
My windows machine has packed up so I am using Ipython on Ubuntu. I did the following to create the image:


```python
In [211]: from __future__ import division

In [212]: def vdc(n, base=2):
     ...:     vdc, denom = 0,1
     ...:     while n:
     ...:         denom *= base
     ...:         n, remainder = divmod(n, base)
     ...:         vdc += remainder / denom
     ...:     return vdc

In [213]: plt.plot([(random.random()*0.5, 0.5+vdc(i)*0.5) for i in range(2500)], '.')
Out[213]: 
[<matplotlib.lines.Line2D at 0x12c73f2c>,
 <matplotlib.lines.Line2D at 0x1311fe4c>]

In [214]: plt.title('Distribution: Van der Corput (top) vs pseudorandom')
Out[214]: <matplotlib.text.Text at 0x12ed6fcc>

In [215]: 
```

--[[User:Paddy3118|Paddy3118]] 21:59, 7 August 2012 (UTC)
