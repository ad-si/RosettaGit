+++
title = "Rosetta Code:Village Pump/Line length"
description = ""
date = 2010-11-10T01:28:43Z
aliases = []
[extra]
id = 4721
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Line length
|summary=Discussion of appropriate line length
}}
In my web browser there are often long lines of codes that end out of the right border of the window.
I see that this occurs when the line of code is made of more than 85 characters.
When programming it is recommended to limit the line length to some bound, it is even sometimes set in some vim syntax configuration files.

Maybe we could define the maximum number of characters a line of code can contain on Rosettacode ?
:I had put a few output examples in a div box using:
:
```txt
<div style="width:full;overflow:scroll">
```

:That just puts in a scrollbar. I don't think that's what you want though is it? --[[User:Mwn3d|Mwn3d]] 20:19, 11 August 2009 (UTC)
::I don't know if more than that would be such a good idea. If a language or compiler has a max line length, then sure, but, for example, [[QBasic]]'s limit is 255 characters (I think). (Granted, I don't know what a person would do that ''requires'' such a long line, but that's not the point.) If such a limit were to be set, then all the code would have to be reviewed by '''someone''' for lines over the limit, and then edited (one way or another).
::For that matter, I can imagine puzzles and/or tasks where long lines would be a Good Thing<sup>TM</sup> (e.g. I could do a [[quine]] in a very few lines, if I were to cram several instructions onto each line... it wouldn't be pretty). -- [[User:Eriksiers|Eriksiers]] 20:25, 11 August 2009 (UTC)

::: What I propose is to create a template like this [[Template:Lines_too_long]], and to add it to examples with lines too long like here: [[Echo_Server#AutoHotkey]]
::: If you don't like this way to do, just remove the template
::: [[User:Blue Prawn|Blue Prawn]] 13:38, 12 August 2009 (UTC)
