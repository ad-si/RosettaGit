+++
title = "Talk:Here document"
description = ""
date = 2012-06-20T22:08:23Z
aliases = []
[extra]
id = 9635
[taxonomies]
categories = []
tags = []
+++

==Quackquack?==
Aren't they usually called delimiters? --[[User:Paddy3118|Paddy3118]] 04:56, 15 May 2011 (UTC)
: I think he's talking about the <tt>&lt;&lt;</tt> itself, rather than the delimiter that follows it. –[[User:Dkf|Donal Fellows]] 22:26, 15 May 2011 (UTC)
:: Definitions or a reference are in order.  I never heard of quackquack or here documents, and found nothing when I tried googling for these terms.  Some languages I've worked with have a way of directing a file read function to treat some immediate data within the program as if it were an external file.  Is that what we're talking about here, or is the task simply to express string literals with embedded whitespace? &mdash;[[User:Sonia|Sonia]] 23:33, 15 May 2011 (UTC)
:::"quackquack" is slang for "<<" (sort of like "bang" is slang for "!"). Here documents have an article [[wp:Here document|on wikipedia]]. Looks like it's just for literals. I had never heard either of the terms either. --[[User:Mwn3d|Mwn3d]] 01:47, 16 May 2011 (UTC)

:: I am well aware of the term "here document", but have never heard of the term quackquack. [[http://www.google.co.uk/search?q=quackquack&ie=utf-8&oe=utf-8&aq=t&rls=org.mozilla:en-GB:official&client=firefox-a#hl=en&pq=%2B%22quackquack%22%20%2B%22%3C%3C%22&xhr=t&q=%2B%22quackquack%22+%2B%22here+document%22&cp=29&pf=p&sclient=psy&client=firefox-a&hs=zpB&rls=org.mozilla:en-GB%3Aofficial&biw=1920&bih=906&source=hp&aq=f&aqi=&aql=&oq=%2B%22quackquack%22+%2B%22here+document%22&pbx=1&fp=ba67f0d5f6cca76c this search] on google for both terms provided no hits. Might the original author consider a more mainstream phrase?
:::I have never heard the phrase used before for this purpose, and I could not find anything either, in google.  That said, I imagine it's a visual pun, with &lt; representing a duck's beak? --[[User:Rdm|Rdm]] 11:26, 16 May 2011 (UTC)
::::It is a visual pun. In any case, I got rid of the phrase in the description. Don't worry about it anymore. --[[User:Mwn3d|Mwn3d]] 12:20, 16 May 2011 (UTC)

== Difference from multi-line string literals ==

Some examples here seem like basically multi-line string literals. So I want to ask: Are any multi-line string literals allowed? (I'm guessing the answer is probably no?) And if not, what is the dividing line that separates "here documents" from other multi-line string literals? Is it that it allows you to use a custom word of your choosing as a delimiter? (Python triple-quotes and many others do not satisfy this.) Is it that it allows you to use a delimiter that is more than one character? What is the criterion? --[[User:Spoon!|Spoon!]] 07:58, 11 July 2011 (UTC)

:I think the distinctions are:
:1  end delimiter takes a line by itself.
:2  the text can get inserted in the middle of the line that invokes it.
:--[[User:Rdm|Rdm]] 08:14, 11 July 2011 (UTC)

Some languages support multi-line literals, but those languages are from an age of fixed-length records, when programmers still used [punched] cards. [[User:Gerard Schildberger|Gerard Schildberger]] 08:46, 16 March 2012 (UTC)
:: Since apparently [[wp:Punched card#Standards|punched cards]] have had standards issued for them as recently as 1990, "when programmers still used [punched] cards" must include the origin time for many languages.  Meanwhile, Perl [which admittedly appeared in 1987] supports both multiline string literals and hereis documents.  So, I guess I am not really sure what point you are trying to make.  --[[User:Rdm|Rdm]] 19:53, 21 March 2012 (UTC)

::: I wasn't making that profound a point.  I was just pointing out from a historical reference on why (or how) some literals were or weren't continued, and those that did stemmed from the use of fixed length records, and those records were almost always from 80-byte (fixed) records, er ..., [punched] cards.   (Yes, I know, some cards weren't 80 columns.)  One computer, I believe some IBM 70x or 70xx series, could read in punched cards in column binary (which was much faster than reading the punched card in character mode), but since the wordsize (er, make that a double word) was only 72 bits, the last eight columns were ignored, so columns 73-->80 were used for placing (punching) sequencial numbers in them to aid the hapless programmer who dropped a deck in reassembling the schmuck's program in the proper order.  It was not a rare occurance, as this schmuck can attest to.  If the programmer was being rude to the computer operator (and I was both a student and a computer operator), sometimes the deck got dropped when the deck was being handed back to the short tempered programmer.  Oops.  I never did such a thing, because it was a lot of work to reassemble the beast, and most everybody (that is, students) didn't bother keypunching the sequence numbers onto the cards). But it happened, and nobody gave any lip to those computer operators (at least, to those who returned the programs at the computer (dutch) door. -- [[User:Gerard Schildberger|Gerard Schildberger]] 22:08, 20 June 2012 (UTC)
