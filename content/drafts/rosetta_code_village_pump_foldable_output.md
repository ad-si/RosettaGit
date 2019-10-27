+++
title = "Rosetta Code:Village Pump/Foldable output"
description = ""
date = 2010-11-10T01:43:52Z
aliases = []
[extra]
id = 3093
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Foldable output
|summary=Question on folding large data segments to avoid bloating page length
}}
The example out of any task that generates ASCII graphs or ASCII art output, such as [[Life in two dimensions]], leads to a loooong page. it would be good if you could implement some tag to surround text that would initially render that text as a single line stating that the real text is hidden, and that clicking on the line would expand to show the hidden text. --[[User:Paddy3118|Paddy3118]] 06:42, 25 October 2008 (UTC)
: That would require JavaScript.  We've avoided using Javascript thus far, because it's a PITA do do right without adopting a framework of some sort.  For it to happen...someone else would have to do it.  I'm finding myself with less and less time of late. --[[User:Short Circuit|Short Circuit]] 03:35, 28 October 2008 (UTC)
:: If it is done with a MediaWiki extension, the JavaScript part should be trivial: Basically it just sets the CSS display attribute of the corresponding div either to "block" (show) or to "none" (hide). The MediaWiki extension would of course have to generate unique ids for those blocks, and generate the links to show/hide the navigation. I don't know how complicated such a MediaWiki extension would be (maybe it even already exists).
:: Alternatively, a pure JavaScript solution would be possible. It wouldn't be that hard either. Basically, in the page you'd write something like

```txt

<div class="hideable" display="none">
(Text which should be hidden)
</div>

```

:: and on page load have the JavaScript scan for all class="hideable" and prepend them with a link to the show/hide function (which would be basically the same as before, except now the index could be used instead of an explicit ID). I'd try that out, but User-Javascript seems to be disabled here.
:: Probably there should be a convention that the hideable part would be surrounded by an always divisible div which just tells you that there's a hidden output area. This is, of course, not part of the JavaScript.
:: Also note that all the boilerplate could easily be hidden in a MediaWiki template. --[[User:Ce|Ce]] 09:08, 28 October 2008 (UTC)

:I just tried using the textarea tag and that failed too. I had hoped that something like: '<nowiki><textarea cols="80" rows="20" readonly="readonly" wrap="off"></nowiki>' would confine the output to a scrollable box, but it did not work. --[[User:Paddy3118|Paddy3118]] 05:59, 29 October 2008 (UTC)

:: A good idea. The following works:
:: <nowiki><pre style="height:30ex;overflow:scroll"></nowiki>
:: Example:
<pre style="height:30ex;overflow:scroll">
Test
Another line
Yet another line, this time one which clearly is much longer than the width of the browser window, at least assuming normal screen dimensions and font settings, and assuming the browser window is not wider than the screen (which would be impractical anyway).
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21

```



Yippee! I just tried it on the Fortran & Python examples of [[Life in two dimensions]]. It works admirably. --[[User:Paddy3118|Paddy3118]] 14:39, 29 October 2008 (UTC)

:Next step is to make it into a template like {{scroll|30}}. --[[User:IanOsgood|IanOsgood]] 16:52, 29 October 2008 (UTC)
::It should probably be {{scrollbegin|30}} and {{scrollend}} so that the output can be pasted in between. Then it's more like a tag, but in a wiki way. --[[User:Mwn3d|Mwn3d]] 18:29, 29 October 2008 (UTC)
:::If it's just a CSS attribute, add it to [[Mediawiki:Common.css]]. --[[User:Short Circuit|Short Circuit]] 02:20, 30 October 2008 (UTC)



###  Template:Hidden 


The "overflow:scroll" is nice but I think that it could be usefull to also have foldable boxes like we can see on other mediawiki's. So I have tryed to import the "Template:Hidden" from Wikipedia ([http://en.wikipedia.org/wiki/Template:Hidden there]) to rosettacode ([http://rosettacode.org/wiki/Template:Hidden here]). Then I have just tryed to use it on [http://rosettacode.org/wiki/Ackermann_Function#Tail-Recursive this page] to provide a complete program, and instead of the foldable box appears a <nowiki>{{{2}}}</nowiki>. Maybe I have missed something? [[User:Blue Prawn|Blue Prawn]] 12:20, 30 December 2008 (UTC)
:I think we need to steal stuff from their [[wp:MediaWiki:Common.css|common.css]] file to put in our [[MediaWiki:common.css|common.css]] file. I don't know a thing about CSS though. --[[User:Mwn3d|Mwn3d]] 17:49, 30 December 2008 (UTC)
:: I think CSS alone won't help here. While you can hide something with CSS (attribute <tt>display:none</tt>), I think for interactivity you need JavaScript) --[[User:Ce|Ce]] 12:03, 31 January 2009 (UTC)

== How can you scroll in a Python> tag ==
Colourised scrollable regions anyone? Thanks.  --[[User:Paddy3118|Paddy3118]] 05:24, 31 January 2009 (UTC)
: The following is the best I manage to get:
<div style="height:30ex;overflow:scroll">
```python

# Your code here

a = 1
b = 2
c = 3
d = 4
e = 5
f = 6
g = 7
h = 8
i = 9
j = 10
k = 11
l = 12
m = 13
n = 14
o = 15
p = 16
q = 17
r = 18
s = 19
t = 20

```
</div>
: I guess to get anything better, one would have to change the GeSHi MediaWiki extension, so it takes anything after the language name and adds it to the generated pre tag. Then you could use <tt><nowiki><lang python style="height:30ex;overflow:scroll"></nowiki></tt> --[[User:Ce|Ce]] 12:26, 31 January 2009 (UTC)

:Thanks. I used the div. --[[User:Paddy3118|Paddy3118]] 18:39, 31 January 2009 (UTC)
