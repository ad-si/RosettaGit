+++
title = "Talk:Web scraping"
description = ""
date = 2011-11-22T22:48:13Z
aliases = []
[extra]
id = 2995
[taxonomies]
categories = []
tags = []
+++

== Choice of page to scrape ==

I hope I've chosen a page that will be around for some time and in the same form. If not I could switch to extracting the last modified time of a Rosetta page I suppose, but we wouldn't be guaranteed a page that is simple to parse and that you can extract slightly changing answers from. --[[User:Paddy3118|Paddy3118]] 20:53, 20 August 2008 (UTC)

== Criticism ==

The task, as described and the examples so far are extremely weak by comparison to one's normal expectations of what "web scraping" means.  The examples just pull a page and extract a line of text using simple regular expressions.

When developers talk about "web scraping" they are usually talking about much more than simply fetching the page and doing trivial extraction of a simple regular expression.  Usually the task implies more sophisticated parsing of the page's HTML and frequently involves encoding the request into a query string (ReSTful sites) or an HTTP POST-able form.

Thus I would expect the task to describe the fetching and parsing of a web page, in HTML form ... with a subsequent encoding of selected results into a new query (and/or posted form).  This would give a far more realistic example of what "web scraping" means to most people who would employ the phrase. [[User:JimD|JimD]] 23:00, 8 September 2008 (UTC)

:Hi Jim, 
:I have read your criticism, and looked at the introduction of the definition of web scraping [http://en.wikipedia.org/wiki/Web_scraping here]. It seems we disagree on the size of example appropriate to R.C. but not really on what web scraping is. If you have a larger example then the central idea of extracting data from a live web page may be lost in the details of how the data is extracted from HTML, or what is done subsequently with that data. A lot of the tasks on R.C. are small and I thought this would fit that mould.
:You could always add a separate task involving extracting data from HTML files? --[[User:Paddy3118|Paddy3118]] 12:56, 9 September 2008 (UTC)

:: Whatever the task calls for, It's perfectly acceptable to break a particular language's code example out into a separate page (e.g. [[Web Scraping/x86 Macro Assembler]]) if the code size gets too large.  If the task itself calls for something beyond a function or two, the task falls under a "project" scope as in [[RCBF]] and [[RCSNUSP]], where the task itself becomes a category, and each example gets its own page there.
::
:: If the task's intent is tightly scoped, but the task's examples grow too far, then consider breaking up the task into multiple pieces that each illustrate some different function. --[[User:Short Circuit|Short Circuit]] 05:59, 10 September 2008 (UTC)

:I think it is worthwhile for the HTML parsing to be a separate task, because different languages are optimized for parsing vs. networking. For instance, [[XSLT]] is fine for parsing complex HTML, but incapable of pulling a page by itself. --[[User:IanOsgood|IanOsgood]] 18:10, 10 September 2008 (UTC)
::Agreed. More complex HTML parsing should be its own task. The focus of this one should be getting the code from the remote site and then doing a basic operation with it. If an HTML task is created, it should also go in the Networking and Web Interaction category and should probably be linked to from here. --[[User:Mwn3d|Mwn3d]] 20:10, 10 September 2008 (UTC)

== The downside of web scraping ==

I noticed that several solutions anchored UTC to the end of line, and the page now outputs "UTC Universal Time" instead. --[[User:Glennj|glennj]] 15:32, 12 August 2009 (UTC)

== “Just the UTC time” – clarification ==

I've noticed that some examples disagree on what exactly should be printed. Some use, literally, just the timestamp without any date. Some include the date, some include the time zone name, some the complete line. I think it should be a little clarified what exactly should be returned. I read the tasks as if just the time (e. g. 01:22:25) should be returned. The complete line isn't of much use anyway since it lacks the current year. —[[User:Hypftier|Johannes Rössel]] 00:22, 21 December 2009 (UTC)
: If you're going to start making the task more exact, don't forget to correct each language's implementation or mark it as needing attention. Or let sleeping dogs lie; it's just a web-scraping task and all of the implementations achieve the basic requirement. (After all, disagreement over what should be scraped is normal for programmers doing web scraping…) –[[User:Dkf|Donal Fellows]] 07:38, 21 December 2009 (UTC)

:A big aim of the task is showing what can be done with the libraries easily available with the language. I would not want to be more exact on what should be scraped if that would merely add more code to parse text. Obviously returning the whole page would be wrong, as would returning time from another zone, or time that was not from the web page, or the use of some obscure library. (I'm with the sleeping dogs, AAaaoooooowwww...) --[[User:Paddy3118|Paddy3118]] 09:08, 21 December 2009 (UTC)

== xPath and HTML web scraping ==
You should have a chance with xPath working on XHTML, but HTML in general can be notoriously badly formed and contains [http://www.w3schools.com/tags/tag_br.asp tags without terminators] such as <nowiki>
</nowiki>
I can't remember how well formed this site is though. --[[User:Paddy3118|Paddy3118]] 16:49, 13 May 2011 (UTC)
