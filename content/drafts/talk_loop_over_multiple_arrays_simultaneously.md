+++
title = "Talk:Loop over multiple arrays simultaneously"
description = ""
date = 2014-08-27T23:06:40Z
aliases = []
[extra]
id = 4986
[taxonomies]
categories = []
tags = []
+++

== Task clarification required? ==
It seems that some of the solutions have assumed that each of the arrays are of the same type (string) while others have assumed that the array (1,2,3) is a list/array of integers.  The result is that solutions provided by any two languages do not necessarily solve the same task and are not necessarily directly comparable. 

It might help readers compare the languages more easily, if the task be clarified to either specify that the code should handle arrays of different types, or that all arrays are the same type. --[[User:Tikkanz|Tikkanz]] 00:21, 19 November 2009 (UTC)

:True, but which to choose? --[[User:Paddy3118|Paddy3118]] 03:27, 19 November 2009 (UTC)
: I would suggest making a "same type" a primary goal for languages where it's convenient, and a second, separate "generic" example a secondary goal, and note which example goes with which goal.  This keeps it simple for languages which support strong typing, allowing a little more showing off for languages with generics, and doesn't add a nonsensical requirement for languages with weak typing. --[[User:Short Circuit|Michael Mol]] 06:58, 19 November 2009 (UTC)
:For some languages, type declarations just aren't present (either because of type inference or because of the use of value systems which make it unnecessary). More importantly, '''''that's not the point of this task'''''. It's the looping construct that is significant, not the types of the data participating. (Well, assuming that the language can loop over arrays/lists of arbitrary element type; I can't think why anyone would restrict it, but someone somewhere might be silly enough.) –[[User:Dkf|Donal Fellows]] 09:09, 19 November 2009 (UTC)
:: I agree that the point of this task is to highlight the looping construct. My point is that because of the different interpretations of the example task, the differences in the looping mechanisms between languages is, to some extent, obfuscated. I have no problem with the arrays being of different types, I just think it would be better if the solutions were all solving the same task. I certainly wasn't suggesting that type must be explicitly specified by each language <shudder>.--[[User:Tikkanz|Tikkanz]] 20:42, 19 November 2009 (UTC)

== ? ==

I am removing from the C entry a comment that read:

:THIS CANNOT BE RIGHT. PLEASE FIX.

The code compiles and runs, and loops over multiple arrays simultaneously, so in that sense, it is right.  If anything, it's probably doing too much (there's about 5 times the amount of code this task should need).  But I think that that kind of code critique belongs here on the talk page.  Alternate versions could go on the main page, if anyone wants to write them.  --[[User:Rdm]] 18:31, 22 July 2011 (UTC)

I've added an “ordinary” C example and some explanation of what the fancy one is doing. —[[User:Kevin Reid|Kevin Reid]] 21:56, 22 July 2011 (UTC)

: I'd vote for getting rid of the "fancy" C code entirely: it really feels like a C++ programmer's idea of C.  But I'm probably alone on this. --[[User:Ledrug|Ledrug]] 22:02, 22 July 2011 (UTC)
:: You're not alone. I don't think it is a useful (or reasonable) example for this page. --[[User:Dchapes|Dchapes]] ([[User talk:Dchapes|talk]]) 20:21, 27 August 2014 (UTC)
::: I've extracted the elaborate entry onto a sub-page. Good enough? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:06, 27 August 2014 (UTC)

== UNIX Shell ==
I wrote this very ugly code for [[Bourne Shell]]...


```bash
a=a:b:c
b=A:B:C
c=1:2:3

IFS=:
paste -d ':' /dev/fd/3 /dev/fd/4 /dev/fd/5 3<<ENDA 4<<ENDB 5<<ENDC |
`printf '%s\n' $a`
ENDA
`printf '%s\n' $b`
ENDB
`printf '%s\n' $c`
ENDC
	while read -r e3 e4 e5; do
		printf '%s%s%s\n' $e3 $e4 $e5
	done
```


...and then I remembered about <code>shift $i</code>, so I never put this code on our task page. --[[User:Kernigh|Kernigh]] 21:03, 28 July 2011 (UTC)
