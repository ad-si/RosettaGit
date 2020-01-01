+++
title = "Rosetta Code:Village Pump/Pre to Lang Tag Fixer"
description = ""
date = 2010-11-10T02:07:16Z
aliases = []
[extra]
id = 3835
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Pre to Lang Tag Fixer
|summary=Planning GugaTagFixer
}}
I'm thinking, if we did Code Tag Fixer, why not make a Pre Tag Fixer?
I already did a bot, but i need a hash/dict, like this:

[[Python]]= python

[[C Sharp]] = csharp

[[Java]] = java5

[[Common Lisp]] = lisp


I need this, because just lowercasing and removing spaces isn't very secure.

----

'''Input:'''
<lang>

## Ada

testeblabla


```txt

lalal

```


lululu


## Python


```txt

aiaiai

```



## C#


```txt

aiai
uiui

```

```


'''Output:'''


```txt


## Ada

testeblabla


```ada

lalal

```


lululu


## Python


```python

aiaiai

```



## C#


```c#

aiai
uiui

```


```


--[[User:Guga360|Guga360]] 23:27, 20 February 2009 (UTC)
: I thought about it, but not all instances of <nowiki>
```txt
</nowiki> are code snippets.  Several are just output.  There's also a bunch of examples out there that use leading whitespace for the same effect.  I think these will need to be done by hand. --[[User:Short Circuit|Short Circuit]] 23:44, 20 February 2009 (UTC)
::I agree. This task is just a bit too human. --[[User:Mwn3d|Mwn3d]] 03:28, 21 February 2009 (UTC)
: This particular discussion is long dead, I know, but for completeness, I figure I ought to put here a link to [[Rosetta Code:Village Pump/Lang-tag bot|my bot that does this (among other things)]]. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 23:05, 17 November 2009 (UTC)
