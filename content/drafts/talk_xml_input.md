+++
title = "Talk:XML/Input"
description = ""
date = 2013-12-08T21:36:14Z
aliases = []
[extra]
id = 4294
[taxonomies]
categories = []
tags = []
+++

== Interpreting XML? ==

The name of this task is XML Reading. Are we supposed to interpret the XML structure, or just extract the names in this particular example?

The AWK implementation only extracts any text between double quotes. That would not be useful in any practical purpose. I think the task should at least require to extract only the contents of the fields named "Name". Maybe the example input file should contain some other fields that are not to be extracted. --[[User:PauliKL|PauliKL]] 13:00, 1 June 2009 (UTC)
: I'm tempted to say let the AWK example stand with comments about how it is scraping the XML and not properly parsing it; disappointingly many languages have to do it that way anyway and it is a common (if nasty) technique. —[[User:Dkf|Donal Fellows]] 13:25, 1 June 2009 (UTC)
::This task should definitely require stuctured XML parsing.  We already have [[Web Scraping]] for more ad-hoc methods. To aid this, I would change the XML to something less trivial. --[[User:IanOsgood|IanOsgood]] 19:04, 1 June 2009 (UTC)
:::I added a <del>entity</del> numeric character reference, since XML processors in general need to be able to handle &amp; and the full character set. --[[User:Kevin Reid|Kevin Reid]] 00:44, 2 June 2009 (UTC)
::::Are you suggesting that the program should convert HTML entities and numeric references into some character encoding? I think that should be a separate task. And, AFAIK, it is HTML specific, not XML. --[[User:PauliKL|PauliKL]] 09:03, 2 June 2009 (UTC)
:::::No. Numeric references, a small set of predefined entities, and the permitted character set, are [http://www.w3.org/TR/xml11/ part of the XML specification]. All XML parsers must support them. Practically, I think it is better for Rosetta Code if our examples show ''robust'', fully-general solutions rather than just-enough-for-the-example-at-hand. Don't spread code that will break when someone with an accent in their name comes along. --[[User:Kevin Reid|Kevin Reid]] 12:23, 2 June 2009 (UTC)
::::::The purpose of Rosetta Code is ''not'' to provide robust, full applications such as XML parsers. Such an application would require thousands of lines of code. Nobody would write them. We should have simple, clearly defined tasks that solve some specific problem, or can be used as a (small) part of an application. The task has to be specified clearly enough so that the implementations will actually solve the problem instead of using shortcut to a known answer. I think this task should be about extracting information from XML file. Character conversion is an entirely different task. Often you would not even want the conversion to be done. And the conversion has nothing to do with code breaking. I now added Vedit example that extracts the data but does not do the conversion. And it does not break because of this, it just extracts the data as expected. --[[User:PauliKL|PauliKL]] 16:33, 2 June 2009 (UTC)
::::::: Certainly no RC example should contain a full XML parser. This one, however, should ''use'' a conformant XML parser library. In a task that is about processing XML, it is misleading to demonstrate half-baked solutions. This is not a matter of doing some "translation" -- it is an inherent part of ''parsing XML at all''. The XML specification [http://www.w3.org/TR/xml11/#entproc states] that "REQUIRED" behavior of an XML processor is that "the indicated character is processed in place of the reference itself" when a character reference occurs in attribute values. --[[User:Kevin Reid|Kevin Reid]] 17:18, 2 June 2009 (UTC)
:::::::: ... then the task should be something like: show how to use a full featured XML parser to parse this doc, rather than <cite>extract the list of student names using whatever means desired</cite>. Only brand new AWK and Vedit macro language would be broken, as far as I can understand other codes. --[[User:ShinTakezou|ShinTakezou]] 23:18, 2 June 2009 (UTC)

::::::::: ... But who decides what one of those is? I think it best to leave the task as given and leave it up to the languages on how they parse it. I have used XML pretty-printers followed by an AWK script to extract quite detailed info from XML - it can work, why bother? --[[User:Paddy3118|Paddy3118]] 07:18, 3 June 2009 (UTC)
:::::::: Kevin, why not simply add a requirement that everybody must use your favorite language? No, there is no point creating tasks that require using some specific language or library. The tasks should require solving some specific problem. That is what programming languages are used for in the real world. The requirements of "conformant XML parser" have nothing to do with Rosetta Code tasks, and not with real world problems either. It is the '''customer''' who sets the requirements. If the task is to extract student names from an XML file, then a program that extracts students names is the correct solution. There is nothing "misleading" or "half baked" in a correct solution that performs the required tasks. "Processing" character references has nothing to do with this task. When the task is to extract the data, the character references have to be extracted just like any other characters, there is nothing to process. --[[User:PauliKL|PauliKL]] 07:54, 3 June 2009 (UTC)
::::::::: If your job required you to extract names from XML, and your solution didn't handle the gamut of XML possibilities (for instance, translating either "&amp;#x00C9" or "&amp;Eacute;" to É), you'd eventually have your code sent back with a bug. Let's not have buggy code on this site; folks are coming here to see best-practice in programming languages, not ad-hoc hackery. --[[User:IanOsgood|IanOsgood]] 15:01, 3 June 2009 (UTC)
:::::::::: '''Wrong'''. If the job requires to extract names from XML, a program that extracts names from XML is exactly the correct solution. It does ''not'' have a bug. In fact, if you ''do'' the conversion when that is not requested, ''then'' your program has a bug and the customer will complain. Why on earth should the characters be converted? And converted to what? UTF-8? UTF-16? ISO 8859-1? If numerical references are used, there is a reason for that, so they must not be converted to something else when such conversion is not requested. --[[User:PauliKL|PauliKL]] 16:27, 3 June 2009 (UTC)
::Donal, the problem is that AWK implementation does not interpret the structure at all. It is quite possible to do some parsing even if there are no ready-made library routines for that. But that does not mean that we should implement a full XML parser. The task should be kept relatively simple.
::I notice that the XML input file has now been changed. But the the task description needs to be changed, too. --[[User:PauliKL|PauliKL]] 09:14, 2 June 2009 (UTC)
::Being the poster of the AWK solution, I have to admit it was a bit tongue-in-cheek - but also true to the XP rule "do the simplest thing that might possibly work", which the original code did for the original task. But rather than implement an XML parser in AWK, I'm rather ok with withdrawing the AWK code. --[[User:Suchenwi|Suchenwi]] 10:17, 2 June 2009 (UTC)
:Please '''don't''' withdraw the AWK code. People who cut-n-paste code without a thought for its applicability deserve what they get. We write tasks, with give solutions to those tasks. Their is no way that we can cater for all unwritten problems of this type that a potential cut-n-paste user may have and we shouldn't pander to them. For some users with the awk solution might be fine - why penalise them? --[[User:Paddy3118|Paddy3118]] 15:33, 3 June 2009 (UTC)

:I changed the task description slightly so that it now requires list of ''student names''. Maybe this is enough to specify the task? --[[User:PauliKL|PauliKL]] 14:32, 2 June 2009 (UTC)

== Still about AWK, and task definition ==

Unluckly the task specifications do not talk about full XML parser or what... it says: <cite>using whatever means desired</cite>... this means that a proper (set of) regular expression(s) in AWK can extract the list of the student names properly... still without knowing too much of the structure of an XML document, and without pretending to be a full featured XML parser... --[[User:ShinTakezou|ShinTakezou]] 22:05, 2 June 2009 (UTC)
: Yeah, fix that task description. Examples should be parsing XML. --[[User:IanOsgood|IanOsgood]] 15:03, 3 June 2009 (UTC)

== Character encoding ==
The Emily encoding, &#x00C9;mily, is it iso-10646-utf-1 ? My code works when I assume that this is the case, but it is not mentioned anywhere.
[[User:bengt]] Sun Dec  8 22:35:02 CET 2013
