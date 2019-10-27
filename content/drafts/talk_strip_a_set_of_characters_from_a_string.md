+++
title = "Talk:Strip a set of characters from a string"
description = ""
date = 2016-12-27T00:41:31Z
aliases = []
[extra]
id = 11849
[taxonomies]
categories = []
tags = []
+++

==Task clarification==

I assume from the example, that the case is be be preserved, and the characters to be removed are also case sensative.  It wouldn't hurt to mention that uppercase characters for the AEI are to be left intact. -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:44, 9 June 2012 (UTC)

: It seems to be implied by omission. Why change case unless told to? --[[User:Paddy3118|Paddy3118]] 20:51, 9 June 2012 (UTC)

:: I didn't change case, I preserved it.  But some people think that a character may be the same as a letter, and that's ambigous, letters can be upper or lowercase.  It still doesn't hurt to state that. I realize that one can assume a lot from omissions, but not necessarily correct assumptions. It's only obvious to the person writing (er, not writing) the omissions.  If the example wasn't shown, I would've assume ''strip'' meant to remove any ''outer'' characters, not ''inner'' characters. A lot of people have many assumptions about the words used, especially those languages which have a   '''strip'''   BIF.   I would've used ''Remove a set of characters from a string''. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:56, 9 June 2012 (UTC)

==REXX version 3==
::: ''I have noted the restriction. What's the problem with CR and LF and tab ???'' --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 22:33, 7 June 2013 (UTC)

:::: You've noted '''a''' restriction (which isn't checked for; if the any of  the restrictions exist, the program produces the wrong result --- if this version is inferior to a previous version, it should be corrected or removed, in my opinion).   The problem with other whitespace is that the '''space''' BIF removes them (because they are considered blanks).   Change the period (within the string) to a period followed by six tab characters and note the result(s). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:01, 7 June 2013 (UTC)

:::::: Which REXX would do that? Mine (ooRexx) does not! s='a'||'05'x||b;t=space(s,0);say t c2x(t) And there should be room for simpler, albeit restricted, solutions. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 05:49, 8 June 2013 (UTC)

::::::: It seems all the "IBM" REXXes (CMS and TSO REXX, ooRexx, and the REXX compiler) use the definition of ''blanks''' to just mean ''spaces''. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 14:45, 10 June 2013 (UTC)

::::::: Regina REXX's PDF documentation says it uses blanks (whitespace) for most BIFs, but doesn't (it just uses spaces).   I've considered submitting a bug (or multiple bugs), but I don't believe that's the correct havior as it may break older programs --- this was a topic of heated debate a decade earlier or so); ROO and R4 do treat blanks as whitespace, at least more consistently   (I haven't test a multitude of others).   Note that (in some REXXes) whitespace is/are blanks, and blanks are spaces and several other characters such as tabs, VT, HT, LF, CR, FF, etc.   The list of which characters are considered blanks is rather vague and not comprehensive.   Older REXX interpreters only use the space character for a blank.   This most likely includes CMS REXX and TSO REXX, oRexx, the CMS and TSO REXX compiler, ooRexx, PC/REXX, and Personal REXX.

::::::::: I'd say the language is rather strict about the SPACE builtin-function. White space in Rexx source code is a different matter. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 09:35, 8 June 2013 (UTC). Afterthought: How does Strip behave in REXXes that I do not have? You mention blank in your version 1 of this task's solutions.

:::::::::: You say ''the language'', but that is ambiguous.   It depends what definition is being used for the implementation.   I always preferred IBM's documents for Classic REXX (CMS and TSO REXX reference) as they are clean, concise, and consistent).   Regina REXX's documentation is incomplete, has conflicting information, and many terms aren't defined. 

:::::::::: The Regina PDF documentation states that:

:::::::::: the document is to provide an overview of the REXX Language and the Regina implementation of the REXX Language.   It is not intended as a definitive reference to REXX; you should really have a copy of the REXX "bible"; ''The REXX Language,'' by ''Mike Colwishaw'' [TRL2]. 

:::::::::: There seems to be a lot of conflicts between the two documents. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:09, 9 June 2013 (UTC)

::::::::::: There is an ANSI Standard that you can refer to. Yes I know that many REXXes don't implement that exactly (think of --, $, #, a=) but there's no doubt, I THINK, about SPACE(string,n,CHAR) and the like. You did not respond to my question regarding the STRIP bif. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 05:31, 10 June 2013 (UTC)

:::::::::::: I know there is a standard out there on the web somewhere, all I have is the ''ANSI® X3J18-199X   American National Standard for Information Systems -- Programming Language REXX'', with the caution:   "This is a proposed American National Standard of Accredited Standards Comittee X3. As such, this is not a complete standard." -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 14:45, 10 June 2013 (UTC)

:::::::::::: Maybe not doubt, ''per se'', but there ARE differences, that is, the various REXXes implement SPACE and STRIP BIFs differently.   So if there are differences, I suppose there would be doubt. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 14:45, 10 June 2013 (UTC)

:::::::::::: What the ANSI Standard says and what's implemented (with regarding to the various REXX interpreters) are almost always different.   I have yet to see an exact definition of what a ''blank'' is (as opposed to a ''space'').   It (as I recall) mentions TAB, HT, RT, FF, LF, ... but never includes an exhaustive list (either for ASCII or EBCDIC), nor do they list the hexadecimal values. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 

:::::::::::: I found the ANSI Standard very hard to read (and understand).   There doesn't seem to be a good definition if the   '''¬'''   (not) symbol is supported (or not), or even what a blank (whitespace?) is.   I found the definitions too esoteric and obtuse.   There are too many references to ''the configuration''.   It is noted that the ''configuration'' varies system to system.   A configuration may have a category of characters in source programs called extra_letters.   Extra_letters are determined by the configuration.   A configuration may have a category of characters in source programs called other_blank_characters.   Other_blank_characters are determined by the configuration.   A configuration may have a category of characters in source programs called other_negators.   Other_negators are determined by the configuration.   A configuration may have a category of characters in source programs called other_characters.   Other_characters are determined by the configuration.   And so it goes. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 14:45, 10 June 2013 (UTC)

:::::::::::: As far as the '''strip''' BIF, that function is also dependent on what blanks are (whitespace).   The '''parse''' statement also depends on what the definition of whitespace is, as well as the '''space''' BIF.   There seems to be a lot of BIFs in REXX that use the term ''blanks'' (which means whitespace); some of which are:   WORD, WORDPOS, DATATYPE, X2C, X2B, X2D, B2C, B2X, JUSTIFY, WORDINDEX, WORDLENGTH, SUBWORD, DELWORD, FIND, WORDS, TRIM, and possibly others.   Note that some of the BIFs are "extensions" or legacy BIFs.   Because REXX numbers can have leading/imbedded/trailing ''blanks'' in them, whitespace can be more insidious, causing numeric comparisons instead of character comparisons for '''IF''' and/or '''WHEN''' statements. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Ges,ard Schildberger|talk]]) 14:45, 10 June 2013 (UTC)
::::::::::::: Apology: I stand corrected: space(s,0) removes whitespce (not only blanks). Thanks. ..[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 08:01, 19 June 2013 (UTC)


The following is a simplier albeit restricted version of a REXX program elsewhere on Rosetta Code:

```rexx
/*REXX program gets two numbers and show their sum.*/ 
/*If y is zero, this works.*/
pull x y
say x
```

Just because it's simpler (with restrictions) doesn't means it warrants inclusion. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:50, 8 June 2013 (UTC)



:: I beg to differ and let's leave it at that, please! --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 09:35, 8 June 2013 (UTC)

If the solution doesn't solve the task as per the requirements, then at the least, it should be fixed.   Just noting the restriction (as noted above in the three line example) doesn't make it correct.


Since the task asks for character stripping we can do without '''changestr'''.   (taken from the REXX version 3 section header.)


(Only if you accept that it only works under certain conditions.) -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 14:45, 10 June 2013 (UTC)
