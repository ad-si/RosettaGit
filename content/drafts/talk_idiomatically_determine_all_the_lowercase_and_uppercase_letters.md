+++
title = "Talk:Idiomatically determine all the lowercase and uppercase letters"
description = ""
date = 2019-06-11T02:45:05Z
aliases = []
[extra]
id = 19069
[taxonomies]
categories = []
tags = []
+++

This task needs clarification: is the distinction between lower case and upper case meant to represent a language feature, or is it meant to be a property of the character set? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 04:40, 28 April 2015 (UTC)
 
: If I understand your meaning, the determination of what is a valid (legal) upper or lower case letter (character, ... not a glyph) would represent a language feature, as it is the computer programming language that determines what is (and is not) a valid upper or lower case letter for that language. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:34, 1 May 2015 (UTC)

:: So, for example, if the the language itself is limited to ascii, but it supports unicode as a data type (but only provides lower/upper case mappings for the ascii subset of unicode - anything beyond that is the programmer's responsibility), for this task we would only be interested in the ascii characters? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 23:57, 1 May 2015 (UTC)

::: No, not strictly so.   If the computer programming language is restrictive to ASCII, then I think the answer to your question would be/could be a qualified   ''yes''.   I'm not aware of any computer programming languages that are restricted to ASCII, but I'm only familiar with some major legacy languages and a handful of others.   Could you enlighten me with some computer programming languages that are restrictive to ASCII?   By ''restricted'', I mean restricted, not limited to, as possibly not yet written/implemented. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:44, 2 May 2015 (UTC)

:::: What I meant was that valid code in that language would need to be ascii (variable names, for example). While I understand that some languages allow unicode identifiers, I do not think we can assume that this is characteristic of all languages. Beyond that, let's say that uppercase/lowercase translation is not built into the language, but is implemented as "library code". Does this make sense? --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 06:40, 2 May 2015 (UTC)

::::: Yes, it makes sense.   However, if a computer language   (via some intrinsic feature and/or a BIF)   can't determine if a character is a upper- or lowercase version of a character (letter),   then it probably should be omitted from this task.   Saying that, I have no qualms if someone were to write an ''ad hoc'' function to perform the test in an idiomatic way so that it achieves the same purpose, albeit that it isn't part of the language description.   I feel that a language shouldn't be excluded from this Rosetta Code task just because there is no built-in language feature or BIF to perform the test idiomatically.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:35, 16 September 2018 (UTC)


This task may need further clarification, for while the description clearly defines "letter" as only the English (ASCII) letters, many of the solutions are displaying large numbers of non-ASCII Unicode letters.  And one can see why they might, given both the title of the task (which says "all letters") and the mention of such ASCII extensions as EBCDIC. [[User:JoeStrout|JoeStrout]] ([[User talk:JoeStrout|talk]]) 02:45, 11 June 2019 (UTC)
