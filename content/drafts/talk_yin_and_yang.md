+++
title = "Talk:Yin and yang"
description = ""
date = 2011-04-19T05:19:36Z
aliases = []
[extra]
id = 9418
[taxonomies]
categories = []
tags = []
+++

==Standard symbol==
Hi, Donald, I made the previous d demo (I made a new one :). If i didn't miss anything, previous d's output is different to python one only by counts of yan/yin of center line , with scale 2, D's is yan:yin = 12:13, python's is 13:12. I think it is sufficiently resemble a Yin-yang symbol =)  [[User:Dingowolf|dingowolf]] 16:36, 4 April 2011 (UTC)

: Thanks dingowolf for doing the Yin thing. And thanks for re-doing it too!
: I still thought that even by disregarding the rectangular rather than square aspect ratio of the fixed width font used in pre blocks on my browser, there still seemed to be a something not quite right about the figure (too me anyway).

: I cut and pasted the figure from my browser and printed the line number, the line length (including spaces), and the line length without leading and trailing spaces. in the last table we have 25 lines making up the outer circle so the longest line should be half way - at line 13, which is 25 non-blank characters long. From lines 1 to 13, I expect the non-blank line length to ''not'' decrease. From line 13 to 25 I expect the line length to ''not increase''. Lines 11 and 15 however are shorter than they should be having 23 characters when the lines around them have 24.
:
```python>>>
 d = ''' 　　　　　　　　　　　　　．　　　　　　　　　　　　　
　　　　　　　　　．．．．．．．．＃＃　　　　　　　　
 　　　　　　　．．．．．．．．．．．＃＃　　　　　　　
　　　　　　．．．．．．．．．．．．．＃＃＃　　　　　
 　　　　　．．．．．．．．＃．．．．．＃＃＃　　　　　
　　　　．．．．．．．．＃＃＃＃．．．．＃＃＃＃　　　
 　　　．．．．．．．．＃＃＃＃＃．．．．＃＃＃＃　　　
　　　．．．．．．．．．＃＃＃＃．．．．＃＃＃＃＃　　
 　　．．．．．．．．．．．＃．．．．．＃＃＃＃＃＃　　
　　．．．．．．．．．．．．．．．．．＃＃＃＃＃＃＃　
 　　．．．．．．．．．．．．．．．．＃＃＃＃＃＃＃　　
　　．．．．．．．．．．．．．．．＃＃＃＃＃＃＃＃＃　
 　．．．．．．．．．．．．＃＃＃＃＃＃＃＃＃＃＃＃＃　
　　．．．．．．．．．＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　
 　　．．．．．．．＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　　
　　．．．．．．．＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　
 　　．．．．．．＃＃＃＃＃．＃＃＃＃＃＃＃＃＃＃＃　　
　　　．．．．．＃＃＃＃．．．．＃＃＃＃＃＃＃＃＃　　
 　　　．．．．＃＃＃＃．．．．．＃＃＃＃＃＃＃＃　　　
　　　　．．．．＃＃＃＃．．．．＃＃＃＃＃＃＃＃　　　
 　　　　　．．．＃＃＃＃＃．＃＃＃＃＃＃＃＃　　　　　
　　　　　　．．．＃＃＃＃＃＃＃＃＃＃＃＃＃　　　　　
 　　　　　　　．．＃＃＃＃＃＃＃＃＃＃＃　　　　　　　
　　　　　　　　　．．＃＃＃＃＃＃＃＃　　　　　　　　
 　　　　　　　　　　　　　＃　　　　　　　　　　　　　'''
>>> # (INDEX, LINELENGTH, NONBLANKLENGTH): LINE
>>> for n,l in enumerate(d.split('\n'), 1):
	print('(%2i, %2i, %2i): "%s"' % (n, len(l), len(l.strip()), l))

	
( 1, 28,  1): " 　　　　　　　　　　　　　．　　　　　　　　　　　　　"
( 2, 27, 10): "　　　　　　　　　．．．．．．．．＃＃　　　　　　　　"
( 3, 28, 13): " 　　　　　　　．．．．．．．．．．．＃＃　　　　　　　"
( 4, 27, 16): "　　　　　　．．．．．．．．．．．．．＃＃＃　　　　　"
( 5, 28, 17): " 　　　　　．．．．．．．．＃．．．．．＃＃＃　　　　　"
( 6, 27, 20): "　　　　．．．．．．．．＃＃＃＃．．．．＃＃＃＃　　　"
( 7, 28, 21): " 　　　．．．．．．．．＃＃＃＃＃．．．．＃＃＃＃　　　"
( 8, 27, 22): "　　　．．．．．．．．．＃＃＃＃．．．．＃＃＃＃＃　　"
( 9, 28, 23): " 　　．．．．．．．．．．．＃．．．．．＃＃＃＃＃＃　　"
(10, 27, 24): "　　．．．．．．．．．．．．．．．．．＃＃＃＃＃＃＃　"
(11, 28, 23): " 　　．．．．．．．．．．．．．．．．＃＃＃＃＃＃＃　　"
(12, 27, 24): "　　．．．．．．．．．．．．．．．＃＃＃＃＃＃＃＃＃　"
(13, 28, 25): " 　．．．．．．．．．．．．＃＃＃＃＃＃＃＃＃＃＃＃＃　"
(14, 27, 24): "　　．．．．．．．．．＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　"
(15, 28, 23): " 　　．．．．．．．＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　　"
(16, 27, 24): "　　．．．．．．．＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　"
(17, 28, 23): " 　　．．．．．．＃＃＃＃＃．＃＃＃＃＃＃＃＃＃＃＃　　"
(18, 27, 22): "　　　．．．．．＃＃＃＃．．．．＃＃＃＃＃＃＃＃＃　　"
(19, 28, 21): " 　　　．．．．＃＃＃＃．．．．．＃＃＃＃＃＃＃＃　　　"
(20, 27, 20): "　　　　．．．．＃＃＃＃．．．．＃＃＃＃＃＃＃＃　　　"
(21, 28, 17): " 　　　　　．．．＃＃＃＃＃．＃＃＃＃＃＃＃＃　　　　　"
(22, 27, 16): "　　　　　　．．．＃＃＃＃＃＃＃＃＃＃＃＃＃　　　　　"
(23, 28, 13): " 　　　　　　　．．＃＃＃＃＃＃＃＃＃＃＃　　　　　　　"
(24, 27, 10): "　　　　　　　　　．．＃＃＃＃＃＃＃＃　　　　　　　　"
(25, 28,  1): " 　　　　　　　　　　　　　＃　　　　　　　　　　　　　"
>>> 
```


:I also looked at what different characters where being used and noticed the use of what may be none-ASCII characters:
:
```python>>>
 # Types of characters in d
>>> set(d)
{'\n', ' ', '＃', '．', '\u3000'}
>>> 
```

:I decided to check to make sure that the visual 'kink' wasn't due to my browser (firefox 4), so also used IE-8, Opera 11.01, and Chrome 10.0.648.204. They all showed the short lines too. 

: I was careful to '''not''' specify that the symbol should be displayed as only ASCII art, as I wanted to leave room for people to post any other renditions they chose . If your symbol works for you in some other display medium then it would be nice to maybe show a screen grab. A note about the circumstances that makes your output appear the best would be nice, but at the moment, in a web browser for the task as it stands, I see these slight issues with the output. (could it be off-by-one or a rounding type error in the Hex grid calcs)? --[[User:Paddy3118|Paddy3118]] 21:32, 4 April 2011 (UTC)

: P.S. I wear spectacles, and am due an eye test later this year so feel free to question my eyesight :-)
:: Thanks for the detailed analysis. Yes, it is incorrect =( and I can't fix it for now (don't know how). I'll reverse the code and output to previous incorrect(?) version, at least it look a bit less incorrect :) [[User:Dingowolf|dingowolf]] 14:47, 5 April 2011 (UTC)

== Graphical version ==

Drawing it in text is all very well, but I found this easier to do by using graphics. (I suppose that's a testament to a good toolkit integration.) So far as I can see, this is still permitted by the task. –[[User:Dkf|Donal Fellows]] 23:20, 18 April 2011 (UTC)
:Yep. I thought Tcl might when I wrote it like that :-)
: --[[User:Paddy3118|Paddy3118]] 05:19, 19 April 2011 (UTC)
