+++
title = "Talk:Rosetta Code/Rank languages by popularity"
description = ""
date = 2019-10-20T21:31:27Z
aliases = []
[extra]
id = 3316
[taxonomies]
categories = []
tags = []
+++

==More task clarification requested==
My example (zkl) has been marked as incorrect. It uses the members field as the count. As does UnixPipes example (if I read it correctly). I checked my answer against the UnixPipes one and got the exact same results. I also note other examples yield the same results. What am I missing?

: For one thing, ranking usually means it starts with #1 (the highest ranking), not #783. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:49, 12 March 2014 (UTC)
::This seems somewhat pedantic as the order is correct (#1 Tcl is listed first), sorted by most members (783) sorted in a decreasing order.

: Some of the ealier programming examples used the ''category'' file as the "true file", and then filtered out the categories that weren't a language   (usually by keyword and/or phrase filters).   This worked fine for the top ten (or twenty, or fifty "languages" ...), but it became problematic when more programing languages where shown in the ranking.   My program (the REXX example) also read the ''languages'' file and used that for a true filter to verify that a category was indeed a programming language. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:34, 12 March 2014 (UTC)
:::The task description gave a url to use as the source data. It also states that "filtering wrong results is optional" (however wrong is defined). I looked through the source data and didn't see any indication as what was a programming language or not. The task description also doesn't list criteria or list heuristics. If any solution is to used as canonical solution, is should be marked as such in the task description.
:::: Oops, I somehow missed that filtering is optional. I've reverted my edits. (But IMHO that makes the task pretty useless and uninteresting). --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 17:46, 12 March 2014 (UTC)

:: As explained by Gerard, I've marked the [[zkl]] solution as incorrect because it includes non-language categories. In addition I've just marked several more solutions (including [[UnixPipes]]) as incorrect too. (I haven't noticed before that more solutions don't filter the categories list). --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 13:03, 12 March 2014 (UTC)
:::As I noted above, doesn't that fall into the "optional" part of the task?

: Other pitfalls are that some programming languages are in different (letter) cases (Maple, MAPLE;   Gdl, GDL;   NewLISP, NewLisp;   etc.);   several languages use unicode character(s), others use different names, and still others aren't "registered" in Rosetta Code properly (such that they aren't recognized as a programming language).   Another big stumbling block is that most examples don't properly handle the ranking of tied languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:49, 12 March 2014 (UTC)
::It really does seem like this task needs additional verbiage such that it can be solved/implemented against the description rather than other implementation and "hidden" knowledge.

==task clarification==

Is it to be assummed that part of the task requirments are to list a certain number of the top ranked Rosetta Code languages? -- [[User:Gerard Schildberger|Gerard Schildberger]] 00:02, 23 July 2012 (UTC)

Would it be a good idea to show the last 10 languages, too? Then it would become obvious if a solution is missing out on giving the same position to languages with the same number of entries.

: Note:   this is done by REXX. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:03, 30 September 2013 (UTC)

==incorrect sample==

The sample output (as part of the task description) is incorrect  (as shown on March 31<sup>st</sup>, 2013).

It shows that the 6<sup>th</sup> and 7<sup>th</sup> entries as having the same number of entries (i.e., are tied), but one is ranked 6<sup>th</sup>, the other is ranked 7<sup>th</sup>.   There shouldn't be a 7<sup>th</sup> (place) entry, instead there should be two 6<sup>th</sup> place entries, and both should be marked as ''tied'' or somesuch indicator.  


Similarly, all examples from the various languages (except one) are also incorrect in this regard.


This is exactly like a foot race, where there are two (tied) 1<sup>st</sup>-place winners (gold), and no 2<sup>nd</sup>-place winner (silver).   First place is shared.    Next to cross the finish line is 3<sup>rd</sup> place (bronze).  

 -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:53, 1 April 2013 (UTC)

-----

Regarding the sample output (as part of the task description) appears to be incorrect (as shown on March 24<sup>th</sup>, 2014.

It doesn't show the   PARI/GP   language entry (it's currently ranked 28th at 394 entries) and it was ranked approximately the same at that time by the REXX entry that was also executed at around the same time frame.   Could it be that the solidus is goofing things up in the program? -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:14, 24 March 2014 (UTC)

==Incorrect examples==
ALL examples, except Python "Working Solution" are not working. Please fix. --[[User:Guga360|Guga360]] 03:49, 29 July 2009 (UTC)
:How, exactly, are all other solutions "not working"?  Please elaborate.   --[[User:Glennj|glennj]] 10:12, 2 August 2009 (UTC)
:Attempting the Python "Working Solution", I'm getting this error using python 2.5.2 --[[User:Glennj|glennj]] 10:14, 2 August 2009 (UTC)

```txt
 $ python rosetta_popular.py
Traceback (most recent call last):
  File "rosetta_popular.py", line 18, in <module>
    for n, i in enumerate(sorted(result,key=lambda x: x[1],reverse=True),start=1):
TypeError: 'start' is an invalid keyword argument for this function
```

:You need Python 2.6. "Not working" examples are only grabbing 500 categories, programming languages like Tcl or Visual Basic don't get in top 10. --[[User:Guga360|Guga360]] 16:48, 2 August 2009 (UTC)
:Actually, the Python implementation isn't correct, either. It omits, for example, AutoHotkey and LotusScript. Compare it with the Perl output. —[[User:Underscore|Underscore]] 16:16, 30 October 2009 (UTC)
::I tried the following:
::AutoHotkey is working.  
::Python is not working
::Ruby is not working
::TCL webscraping is working   
::perl wikipedia api has a lot of dependencies, I couldn't get URI to build on my macbook  --[[User:Tinku99|Tinku99]] 21:01, 16 May 2010 (UTC)

-----

I noticed about a handful of programming examples that "only" grabbed 500 languages.   Now, when they were first entered (for the most part), there weren't 500 languages, but now there are over 500 languages.   At least two examples were added after there were 500 languages.   Are these (or should these) examples be considered as incorrect? -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:16, 15 January 2014 (UTC)

==ALGOL 68==

I tested [[ALGOL 68]] (I'm using release algol68g-mk16.win32), but it did not worked.
Do I need a library? --[[User:Guga360|Guga360]] 22:19, 18 April 2009 (UTC)

PS: I'm currently on Windows, i'll try to restart in Ubuntu later.

Error:

<code algol68>39 http content (reply, "www.rosettacode.org", "http://www.rosettacode.org/w
/index.php?title=Special:Categories&limit=500", 0);
1 a68g16.exe: error: 1: tag "httpcontent" has not been declared properly (detected in c
onditional-clause starting at "IF" in line 37).</code>

I think it's really a library. But i never programmed in ALGOL, what should i do?

Guga360  08:24, 19 April 2009

Hi Guya360,

This ALGOL 68 implementation is an interpretor, and it would require the library being 
linked into the a68g.exe binary.  Hence the .exe you have definitely does not have
"http content" linked in.

I ran it on Fedora9 OK.  I just checked a68g-manual.pdf and it says: 

Mark 8, July 2005
# Adds procedure ''http content'' for fetching web page contents (UNIX)
# Adds procedure ''tcp request'' for sending requests via TCP (UNIX)
# Adds procedure ''grep in string'' for matching regular expressions in a string (UNIX)

The key point being the (UNIX) at the end of the line.  I am guessing that the tcp library 
for windows was enough different from the tcp library for Linux/Unix that the "''http content''"
routine remains both un-ported and broken.

Now I have not tried Algol68g on Ubuntu.  I hack around on Fedora.  So you MAY be able 
to use the pre-compiled algol68g RPM for ubuntu (Does Ubuntu support RPMs?)  

BUT the algol68g '''source''' tar ball will definitely work on ubuntu.  (Maybe not on a 
64bit Ubuntu as the libraries have moved to /lib64 - if so let me know and I'll 
sort/hack you up a 64bit update)

The compile should be as easy as installing gcc and configure (and if you need them 
then install postgres or curses or ... ) then:

```txt

tar -xvf /tmp/download/algol68g-mk16.tgz
./configure --threads
make 
# as root
make install
# as user
a68g Sort_most_popular_programming_languages.a68

```


BTW: you are the first person to feed back on running the ALGOL 68 
rosettacode code snippets... I am rather impressed.  ThanX - (blush)

Is this snippet the first ALGOL 68 that you have tried?  

[[User:NevilleDNZ|NevilleDNZ]] 08:40, 19 April 2009 (UTC)
:Yes, it's my first try.
:I just found that example intersting because it does not use [[http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Languages&cmlimit=500]].

:I compiled algol68g right now. (./configure && make && sudo make install)
:It compiled perfectly.
:But i runned that example, and nothing happened. It's just "loading".
:Any suggestions? --[[User:Guga360|Guga360]] 16:08, 19 April 2009 (UTC)

I am guessing that you program is running, just really slow, give it 2 minutes to run.  
Basically the routine ''re split'' used to parse the HTML is really slow.   ''re split'''s 
performance is the order of O<sup>2</sup>.

I just recoded the ALGOL 68 version to use a linked list, it is a huge improvement:

```txt

[nevilled@november rosettacode]$ time  a68g Sort_most_popular_programming_languages_slow.a68 
1. 233 - Python
2. 222 - Ada
3. 204 - C
4. 203 - OCaml
5. 201 - Perl
6. 193 - Haskell
7. 182 - Java
8. 179 - D
9. 178 - ALGOL 68
10. 160 - Ruby

real	0m47.950s
user	0m44.363s
sys	0m0.080s

[nevilled@november rosettacode]$ time  a68g Sort_most_popular_programming_languages.a68 
1. 233 - Python
2. 222 - Ada
3. 204 - C
4. 203 - OCaml
5. 201 - Perl
6. 193 - Haskell
7. 182 - Java
8. 179 - D
9. 178 - ALGOL 68
10. 160 - Ruby

real	0m11.504s
user	0m3.228s
sys	0m0.068s

```


Sort_most_popular_programming_languages_slow - the original - would have been issuing '''thousands''' of calls to malloc.

re: [[http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Languages&cmlimit=500]]
I should/could use this link.  I hacked out a solution, c.f. the actual code for the ''re ignore'' values.

```txt

# hack: needs to be manually maintained #
  STRING re ignore ="Programming Tasks|WikiStubs|Maintenance/OmitCategoriesCreated|"+

```

Yes... it is a hack.  I'll try to stitch this in shortly.

[[User:NevilleDNZ|NevilleDNZ]] 20:04, 19 April 2009 (UTC)
----

The example does not work on algol68 genie anymore due to a bug in http content.
I contributed a new version using the api. It show all entries, and have a work around for the algol68g bug.

--[[User:Prisni|Prisni]] ([[User talk:Prisni|talk]]) 06:23, 29 August 2015 (UTC)

Even if the work-around is applied to the web-scrapping example, there is an additional problem in that it does not find all the languages because the request specifies too small a limit:


```txt

http content (reply, "www.rosettacode.org", "http://www.rosettacode.org/w/index.php?title=Special:Categories&limit=500", 0);

```


The limit needs to be much larger (probably 5000 would work at the moment). I'm not sure the "manually maintained" list of things to filter out the non-programming languages entries is sufficient. Other web-scrapping examples filter them out using a second query to find the categories that are programming languages. 

--[[User:Tigerofdarkness|Tigerofdarkness]] 


### =Note: ALGOL 68 for Ubuntu now available=

* Announcement: [http://sourceforge.net/mailarchive/forum.php?thread_name=1255746666.15125.166.camel%40zod.sgr-a.net&forum_name=algol68-user sourceforge algol68 forum]
* Download: http://sourceforge.net/projects/algol68/files/

[[User:NevilleDNZ|NevilleDNZ]] 09:19, 17 October 2009 (UTC)


### Problem description insufficient?


I updated the Ruby solution today when I noticed there were more than 500 categories.  Popular (top 10%) languages like Visual Basic .NET and Vedit macro language are left off most lists. --[[User:Glennj|glennj]] 20:02, 15 June 2009 (UTC)
:I found a better way to do this task.
:"http://www.rosettacode.org/w/api.php?action=query&prop=categoryinfo&titles=Category:Python|Category:Tcl|...", property "pages" should return category members. --[[User:Guga360|Guga360]] 21:31, 15 June 2009 (UTC)
:: I completely missed the Perl/Python solutions:  first filter on the Programming_Languages category, not all categories. --[[User:Glennj|glennj]]
::: Actually, this does not solve the problem. We're still reading Special:Categories, if there is a language starting with Z, probably it will not get in the list. --[[User:Guga360|Guga360]] 01:30, 16 June 2009 (UTC)

:::: As of January 2013, there are five   '''Z'''   entries:
:::::::* ZX Spectrum Basic
:::::::* Z80 Assembly
:::::::* ZPL
:::::::* Zonnon
:::::::* ZED
:::::::::: -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:19, 7 January 2013 (UTC)

== Ruby example question ==

I'm trying to run the Ruby example on Ruby 1.8.6 and it says that the "each_slice" method isn't defined. Is that part of 1.8.7? --[[User:Mwn3d|Mwn3d]] 19:29, 26 June 2009 (UTC)
:I don't know, probably it's for Ruby 1.9, but you can try replace 
```ruby
langs.each_slice(50) do
```
 with 
```ruby
langs[0..50].each do
```
 --[[User:Guga360|Guga360]] 22:54, 26 June 2009 (UTC)
::It's 1.8.7. I'll mark the example. The indexing solution didn't work, but I used a computer that has 1.8.7 and it works. Thanks for the suggestion, though. --[[User:Mwn3d|Mwn3d]] 23:18, 26 June 2009 (UTC)

:: [[User:Glennj|glennj]] 18:43, 27 June 2009 (UTC) -- Here's what the 1.8.7 docs say about <code>Enumerable#each_slice</code>
```txt
Iterates the given block for each slice of <n> elements
```
It could be implemented like
```ruby
def each_slice(ary, n)
  (ary.length/n + 1).times {|i| yield ary[i*n,n]}
end
```


== Redundant task? ==

I actually asked that on this page on [http://rosettacode.org/mw/index.php?title=Talk:Sort_most_popular_programming_languages&oldid=21099 Jan 26], however it was [http://rosettacode.org/mw/index.php?title=Talk:Sort_most_popular_programming_languages&diff=29733&oldid=21099 replaced without comment] by Guga360. Therefore I'll ask again:

Isn't this task basically a combination of [[HTTP Request]], [[Regular expression matching]] and [[Sorting Using a Custom Comparator]]? --[[User:Ce|Ce]] 11:55, 1 November 2009 (UTC)

: '''Not Redundant''' — Don't be ridiculous. You might as well argue (by reduction to fundamentals) that it's a combination of basic operations and TCP socket handling. Which it is, but that's missing the whole point. This is a composite task that is focussed on the end goal rather than the specific technique used to achieve it, which is absolutely allowed in the RC rules. –[[User:Dkf|Donal Fellows]] 13:32, 1 November 2009 (UTC)
: BTW, I don't know why Guga360 replaced what you wrote. It seems rather rude to me to do so. –[[User:Dkf|Donal Fellows]] 13:37, 1 November 2009 (UTC)

::There are similarities in the question and answer given [[Talk:24_game#Purpose|here]]. --[[User:Paddy3118|Paddy3118]] 14:47, 1 November 2009 (UTC)

: I'll copy the relevant portion of my reply from over there:
:''As for "redundant" tasks, it doesn't bother me. In fact, I can think of ways to take advantage of it, such as category tagging individual examples with techniques, features or principles they may illustrate, to offer another way to browse and search the site, and to be more illustrative of alternate approaches.'' (end quote) --[[User:Short Circuit|Michael Mol]] 17:16, 1 November 2009 (UTC)


== J ==

The neat thing about the [[Sort most popular programming languages#J|J solution]] is that it's completely functional/declarative.  That is, it contains no imperative code; no instructions on how to process data.  It merely describes the result it wants, using four separate domain-specific functional/declarative syntaxes: J, XPath, Regular Expressions, and [[j:Addons/xml/sax/x2j%20Examples|x2j]].

Note: the J solution specifically uses XPath to precisely address the data it wants, rather than relying on a the textual format of the XML (e.g. by assuming the &lt;li&gt; will not break across lines), which is not guaranteed, and defeats the purpose of XML to an extent.  The regular expressions are not strictly required; they are included simply to increase legibility (self-documentation) and to identify the data of interest (e.g. by excluding uninteresting &lt;li&gt; in the language list, based on their content).

But this doesn't mean the code is specific to this task (e.g. it doesn't implement a MediaWiki-specific interface).  The great value of separating the concerns of addressing (XPath), identifying (regex), and transforming (J) the data is precisely that it is general and adaptable.

This declarative nature also explains why the solution is so concise (relatively).  But also it omits the optional join on MostLinkedCategories and washes the list through pattern exclusion.

--[[User:DanBron|DanBron]] 05:52, 26 November 2009 (UTC)

: And the downside is that the current XPath implementation relies on [http://sax.sourceforge.net/ SAX], and maintenance has gone so bad there that you can basically only use this J implementation on 32 bit J6.02. The underlying problems of standards decay and implementation decay has a lot of causes, but the bottom line might be that any technical solution relies partially on popularity for support and thus will fail sooner or later. I guess that means I (or, ok, someone else - but if you want something done right you need to take responsibility for accomplishing it) should come up with another J implementation. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 00:11, 10 April 2016 (UTC)

==wanted (no longer): a complete list==
I would like to see at least one (and probably only one) list to be complete.   I also would like it updated, say, every month or quarter so we all can see the current state of all languages, not just the top ten or twenty languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:17, 21 March 2012 (UTC)

I tried to execute the Ruby example (with what little I knew of that language), but it didn't execute (got an error) with my version of the Ruby language.   I know so little that I don't know if the example depended on a certain version, or maybe the libraries that I have don't contain the necessary routines.   In any case, I was hoping that someone could run ''any'' of the examples and provide a complete list. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:29, 29 June 2012 (UTC)

One reason to have a complete list is to ensure that the example programs are processing all of the languages (or ''for'' the languages).    That isn't the main reason, but it would/could point out deficiencies in an example's process/algorithm. -- [[User:Gerard Schildberger|Gerard Schildberger]] 21:35, 29 June 2012 (UTC)



-----



Since nobody re-ran their examples, I decided to write my own (using REXX) and included an almost full list of the rank of languages on Rosetta Code.

I'll try to update it once a month or so. -- [[User:Gerard Schildberger|Gerard Schildberger]] 23:53, 22 July 2012 (UTC)

If somebody else creates a more complete ranking, better filtering program, or an automated version (or more timely), I'll reduce the number of Rosetta Code languages ranked in the REXX '''output''' section. -- [[User:Gerard Schildberger|Gerard Schildberger]] 00:11, 23 July 2012 (UTC)

Currently, the REXX example reports on all   <strike>'''471'''</strike>   (see below)   programming languages, but there is code to support the skipping of languages that have less than a certain (specified) number of examples. 

However, listing them all enabled me to find some languages that are "misspelled" as far as case goes (inconsistent upper/lower/mixed spellings, use of different glyphs, etc.). -- [[User:Gerard Schildberger|Gerard Schildberger]] 04:51, 5 September 2012 (UTC)

:: The REXX program has been updated (sometime ago now) that takes in account if programming languages are in a different case (lower/upper/mixed) and also those that have (glyph) aliases. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:31, 13 February 2016 (UTC)


<strike>

Now, there're '''473''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:23, 5 September 2012 (UTC)

Now, there're '''475''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:45, 26 January 2013 (UTC)

Now, there're '''481''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:41, 30 March 2013 (UTC)

Now, there're '''500''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] 07:01, 2 November 2013 (UTC)

Now, there're '''501''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 09:40, 18 November 2013 (UTC)

Now, there're '''502''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:05, 16 December 2013 (UTC)

Now, there're '''503''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 11:10, 20 December 2013 (UTC)

Now, there're '''504''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 13:39, 28 December 2013 (UTC)

Now, there're '''505''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:18, 4 January 2014 (UTC)

Now, there're '''510''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:18, 28 January 2014 (UTC)

Now, there're '''511''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:52, 4 February 2014 (UTC) 

Now, there're '''515''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:59, 1 March 2014 (UTC) 

Now, there're '''516''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:05, 12 March 2014 (UTC)

Now, there're '''514''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:45, 25 March 2014 (UTC)

Now, there're '''512''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:20, 7 April 2014 (UTC)

Now, there're '''521''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:33, 2 May 2014 (UTC)

Now, there're '''523''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:58, 1 June 2014 (UTC)

Now, there're '''527''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:17, 1 July 2014 (UTC) 

Now, there're '''528''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:26, 21 July 2014 (UTC)

Now, there're '''529''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:31, 11 August 2014 (UTC)

Now, there're '''535''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:28, 1 September 2014 (UTC)

Now, there're '''539''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:07, 19 October 2014 (UTC)

Now, there're '''545''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:09, 3 December 2014 (UTC)

Now, there're '''548''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:45, 12 January 2015 (UTC)

Now, there're '''549''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:46, 28 February 2015 (UTC)

Now, there're '''555''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:10, 4 May 2015 (UTC) 

Now, there're '''556''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:32, 22 May 2015 (UTC)

Now, there're '''562''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 14:48, 24 June 2015 (UTC)

Now, there're '''563''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:57, 22 July 2015 (UTC)

Now, there're '''569''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:03, 2 September 2015 (UTC)

Now, there're '''570''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:42, 13 September 2015 (UTC)

Now, there're '''571''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:18, 13 September 2015 (UTC) 

Now, there're '''573''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:48, 12 October 2015 (UTC)

Now, there're '''575''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:28, 29 November 2015 (UTC)

Now, there're '''575''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:28, 29 November 2015 (UTC)

Now, there're '''576''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:53, 13 December 2015 (UTC)

Now, there're '''577''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:04, 1 January 2016 (UTC)

Now, there're '''580''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:15, 31 January 2016 (UTC) 

Now, there're '''581''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:21, 13 February 2016 (UTC)

Now, there're '''593''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:50, 9 April 2016 (UTC)

Now, there're '''597''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:48, 23 April 2016 (UTC)

Now, there're '''598''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:34, 29 April 2016 (UTC)

Now, there're '''599''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:05, 11 May 2016 (UTC)

Now, there're '''601''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 15:15, 22 June 2016 (UTC) 

Now, there're '''602''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:03, 21 July 2016 (UTC)

Now, there're '''622''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:28, 18 December 2016 (UTC)

Now, there're '''624''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:35, 30 January 2017 (UTC)

Now, there're '''626''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:51, 4 March 2017 (UTC)

Now, there're '''627''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 13:03, 28 April 2017 (UTC)

Now, there're '''628''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:33, 14 May 2017 (UTC)

Now, there're '''635''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:54, 6 June 2017 (UTC)

Now, there're '''637''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:26, 7 July 2017 (UTC)

Now, there're '''639''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:21, 15 July 2017 (UTC)

Now, there're '''645''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:56, 20 August 2017 (UTC)

Now, there're '''646''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:32, 2 September 2017 (UTC)

Now, there're '''649''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:06, 28 September 2017 (UTC)

Now, there're '''650''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:53, 29 October 2017 (UTC) 

Now, there're '''651''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:49, 17 November 2017 (UTC) 

Now, there're '''652''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:16, 10 December 2017 (UTC)

Now, there're '''654''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:16, 10 December 2017 (UTC)

Now, there're '''656''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:02, 24 February 2018 (UTC)

Now, there're '''658''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:22, 7 April 2018 (UTC)

Now, there're '''663''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:50, 15 May 2018 (UTC)

Now, there're '''677''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:51, 8 August 2018 (UTC) 

Now, there're '''679''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:02, 16 August 2018 (UTC)

Now, there're '''680''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:05, 26 August 2018 (UTC)

Now, there're '''681''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:40, 4 September 2018 (UTC)

Now, there're '''682''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:12, 25 September 2018 (UTC) 

Now, there're '''683''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:57, 15 October 2018 (UTC)

Now, there're '''684''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:23, 26 October 2018 (UTC)

Now, there're '''686''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:46, 19 November 2018 (UTC)

Now, there're '''687''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:23, 6 December 2018 (UTC)

Now, there're '''686''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:14, 31 December 2018 (UTC) 

Now, there're '''687''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:17, 2 January 2019 (UTC) 

Now, there're '''690''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:21, 6 January 2019 (UTC) 

Now, there're '''692''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:52, 15 February 2019 (UTC)

Now, there're '''694''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 14:05, 27 March 2019 (UTC)

Now, there're '''696''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:47, 28 March 2019 (UTC)

Now, there're '''697''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:53, 29 March 2019 (UTC)

Now, there're '''698''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:15, 31 March 2019 (UTC)

Now, there're '''699''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:17, 9 April 2019 (UTC)

Now, there're '''698''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:46, 10 April 2019 (UTC)

Now, there're '''699''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:41, 12 April 2019 (UTC)

Now, there're '''700''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:02, 14 April 2019 (UTC)

Now, there're '''701''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:29, 26 April 2019 (UTC)

Now, there're '''703''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:18, 5 May 2019 (UTC)

Now, there're '''705''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:35, 17 May 2019 (UTC)

Now, there're '''706''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:03, 17 May 2019 (UTC)

Now, there're '''707''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 12:58, 19 May 2019 (UTC)

Now, there're '''708''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 13:54, 1 June 2019 (UTC)

Now, there're '''712''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:33, 13 June 2019 (UTC)

Now, there're '''713''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:53, 19 June 2019 (UTC)

Now, there're '''714''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:43, 28 June 2019 (UTC)

Now, there're '''716''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 10:56, 5 July 2019 (UTC)

Now, there're '''715''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 10:58, 20 July 2019 (UTC)

Now, there're '''716''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:49, 1 August 2019 (UTC)

Now, there're '''717''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:01, 15 August 2019 (UTC)

Now, there're '''719''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:54, 21 August 2019 (UTC)

Now, there're '''720''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 16:30, 14 September 2019 (UTC)

Now, there're '''721''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:20, 16 September 2019 (UTC)

Now, there're '''722''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:15, 16 September 2019 (UTC)

Now, there're '''723''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:59, 20 September 2019 (UTC)

</strike>






Now, there're '''727''' programming languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:31, 20 October 2019 (UTC)






<strike>

The apparent decrease in the number of programming languages (at one point) was due to combining the
:::*   <big> ''' UC++  ''' </big>
:::*   <big> ''' µC++  ''' </big>
:::*   <big> ''' ╬£C++ ''' </big>     (unicode version)
languages into one:   '''µC++'''.   -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:45, 26 January 2013 (UTC) 
</strike>

(The above was struck as it no longer applies.) -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 05:28, 1 September 2014 (UTC)



-----


 
Hi Gerard - I noticed your recent update to this page didn't agree with my count (from the Tcl API example, which currently counts 582 languages).  I'm not sure exactly where our discrepancy comes from, but looking at the data:

  * Absolute difference:  Tcl 582 - REXX 563 = 19
  * Counted by Tcl but not REXX (25): <tt>AngelScript {Caché ObjectScript} Ceylon {Déjà Vu} EhBASIC Epigram FLORA-2 Florid Forth Golo Haxe LLP Lolli Lygon ObjectIcon RPGIV Reduce Rubylog Star {True BASIC} UC++ X10 XS ΜC++ МК-61/52</tt>
  * Counted by REXX but not by Tcl (6):  <tt>{Dëjá Vu} FORTH HaXe MK-61/52 {MK-61/52 ObjectScript} µC++</tt>

All of the differences can be accounted for by Unicode - I see that the REXX script tries to bitbang non-ASCII entities into place, while Tcl is generally quite good with encodings, so this is to be expected.  The entries found by Tcl but not by REXX are less clear to me .. the ones I've examined look like legitimate languages.  Is this a bug in the REXX?

If it helps, I can give you a (Tcl) script to perform the comparison above, for which I used the <tt>RC_POP.OUT</tt> file you so helpfully attached.
--[[User:Aspectcl|Aspectcl]] ([[User talk:Aspectcl|talk]]) 03:37, 23 July 2015 (UTC)



-----



First, regarding the Unicode characters:   there are several Unicode versions (or entries) of the same(?) language (all spelled different and/or use different glyphs), and it was somewhat obvious to me that they all referred to a common language, albeit spelled differently.   I chose to combine them as I thought they referred to the same language.   There are probably other languages that have multiple spellings that I haven't observed.   This was   ''one''   of the reasons that I include a complete list of all languages (as REXX finds them in the categories file), which are specified/verified in the languages file. 

:::*   <big> ''' UC++  ''' </big>
:::*   <big> ''' µC++  ''' </big>
:::*   <big> ''' ╬£C++ ''' </big>     (unicode version)
(to my eyes) appear to be referring to the same language.   Note that REXX doesn't support Unicode, so I see two bytes (characters) of what appears to be gibberish, and, as you put it, I bit-banged the gibberish bytes into something that is recognizable (at least, by my eyes).  

REXX did find the   '''Déjà Vu'''   language, but the various entries were entered (via my translations) as   '''Dëjá Vu''',   one of it's alternate spellings.   You can find it ranked 
102<sup>nd</sup> with 105 entries.   I had to choose a primary (version) spelling, and the one that REXX uses is the one I chose.   If there is a preferred glyph, I'll change the REXX program to list all the entries under the preferred (or correct) spelling.   I'll let somebody else be the spelling police.

REXX did find the '''UC++''' language, but it's translated to (spelled as) '''µC++''', and it's ranked 346<sup>th</sup> (tied) with 6 entries.

REXX did find the '''MK-61/52 ObjectScript''' language, but it's translated to (spelled as) '''MK-61/52 ObjectScript''', and it's ranked 191<sup>st</sup> (tied) with 31 entries. 
 
Since REXX doesn't do Unicodes, I chose to have REXX use a Latin (Roman) letter <big><big>'''K'''</big></big> instead of the Cyrillic <big><big>'''К'''</big></big> glyph.   You may have to squint (or enlarge your font) to see the difference.   Oy veh!

REXX did find the '''Forth''' language, but it's spelled '''FORTH''', and it's ranked
44<sup>th</sup> (tied) with 31 entries. 

REXX did find the '''Haxe''' language, but it's spelled '''HaXe''', and it's ranked
209<sup>th</sup> (tied) with 25 entries.

Note that the REXX program ignores the case (upper/lower/mixed) for computer programming languages.   All pertinent comparisons in the REXX program are case insensitive (caseless), so '''Haxe''' compares equal (the same) as '''HaXe'''.   The REXX program uses the first encounter of a language's spelling as the preferred spelling;   added a popularity count for the various spellings would place a large (but not difficult) burden on the REXX program and detract from its purpose and make it harder to follow it's logic   (well, even more so). 

Quite a few computer programming languages have similar problems:
::::*   PL/I   or   PL/1
::::*   REXX   or   Rexx   or   R<small>EXX</small>

just to mention a couple of the simple ones.

I took a look at the raw file (which the REXX program uses), the category file.   There is no entry for '''AngelScript''' in that file, but there is an entry for that language in the (raw) language file.   The REXX program uses the category file to find entries, and when it finds an entry, it verifies that the language exists (that is, it is indeed a language instead of just a special category), it only then recognizes it as a language.   Languages which have no entries in the category file aren't counted.   However, the category file does have an entry for '''AngelScript User''' (1 member), so noting that '''AngelScript User''' is not a language, it is just a special category. 

Stated another way, entries in the languages file, but have an entry in the category file aren't counted as being a language that has a computer programming language on Rosetta Code. 

::I took this Rosetta Code task's requirement quite literally:

::: ''Sort most popular programming languages '''based in number of members''' in Rosetta Code '''categories'''.''

:::: (Bold highlighting added by me.)

So what the REXX program did was use the category file as its reference file (or base), and used the language file to verify that entries in the category file are indeed languages (and not just a special category entry). 

If the REXX program did the reading/verification the other way 'round, then it would list that language, but with 0 (zero) members.   Or more precisely, '''null''' entries.   When I tried to do a Rosetta Code search on that language, it doesn't find an entry. 

Now, as it is, I can't/don't see what Tcl's (program language example/entry) count is for the '''AngleScript''' language, is it one, zero, or what?

And, yes, I would like to see (or receive) Tcl's complete (output) list with counts.

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:04, 23 July 2015 (UTC)

::: I've enhanced the REXX program to now include the   ''total number of languages detected in the languages file''   (as well as previously noting the total number of languages detected in the category file.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:08, 4 August 2015 (UTC)



-----



Thanks!  I've updated the Tcl script to produce a similar report to [[RC_POP.OUT]], saved at [[Rosetta_Code/Rank_languages_by_popularity/Tcl_API_full_output]].  The Tcl script hits the API, first asking for all members of "Category:Programming Languages", then getting a count for each "Category:$language" in a separate call, without attempting to filter empty categories.  I don't have the attention to do any deeper analysis just now, but having uniform output to compare should help!

--[[User:Aspectcl|Aspectcl]] ([[User talk:Aspectcl|talk]]) 11:03, 23 July 2015 (UTC)

: Thank you for the complete ranking list produced by Tcl.   I noticed the number of zero entries (at the bottom), which aren't present in the category file that the REXX program reads.   Kudos to your proper handling of the tied entries!   I think only five programming entries got it correct (for tied rankings) as far as I could tell, some programming entries only listed just a few (top ten languages or whatever) where there weren't any tied entries.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:55, 23 July 2015 (UTC)



-----



In the complete list [http://rosettacode.org/wiki/Category:A%2B A+] is ranked 431 although (unfairly) it has no worked tasks. [http://rosettacode.org/wiki/Category:Xanadu Xanadu], a language with which I am unfamiliar, has one worked [http://rosettacode.org/wiki/N-queens_problem task] but is ranked 463. --[[User:Nigel Galloway|Nigel Galloway]] 12:29, 21 December 2012 (UTC)

: The way identically ranked languages (identical in the sense that they have the same number of entries) is sorted in the order in which they appear first in the Rosetta Code list.   Thus, some languages aren't ranked fairly because of a (weak) sorting artifact of having the same number (of entries).   Strictly speaking, if the following were true:
:::* hog   97
:::* dog   72 
:::* auk   72
:::* ape     4
:::* cow   72
:::* gnu   72
The ranking should be:
:::* 1           hog
:::* 2 (tied)   dog
:::* 2 (tied)   auk
:::* 2 (tied)   cow
:::* 2 (tied)   gnu
:::* 6           ape
with all 2<sup>nd</sup> place names marked as ''tied'' for 2<sup>nd</sup>, and nothing marked as 3<sup>rd</sup>, 4<sup>th</sup> or 5<sup>th</sup>. 

These duplicates (tied for placement) would make a good addition to this task (to rank languages ''correctly'') -- or lacking that, a good Rosetta Code task that can stand by itself.

Note that the chicken was disqualified as it wouldn't cross the road. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:48, 7 January 2013 (UTC)


Update notice:   the above (the ranking of tied languages) has been implemented by the REXX program example, and also the Icon and Unicon languages. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:41, 30 March 2013 (UTC)

:According to [http://rosettacode.org/wiki/Category:A%2B its task page] A+ has no tasks implemented. It seems as if a language with no tasks implemented is treated as if it has three
:: rank: 441         (3 entries)  A+
:--[[User:Nigel Galloway|Nigel Galloway]] 14:51, 26 January 2013 (UTC)

:: Using the number of members in a category is biased because most language categories have three subcategories ("X examples needing attention", "X Implementations" and "X User") which are also included in the count. Many (all?) languages that are listed with 3 members (like [[A+]] or [[B]]) in the ranking actually don't have any task implemented. --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 13:18, 12 March 2014 (UTC)



-----



I took the task's requirements quite literally:


''Sort most popular programming languages based in number of '''members''' in Rosetta Code categories''

 (from http://www.rosettacode.org/mw/index.php?title=Special:Categories&limit=5000)


(The bold font was added by me.)   Note that it didn't say   '''implementations''',   but   '''members'''.

I think that's what most people (most likely) thought that's what was wanted, but there ya have it. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:45, 26 January 2013 (UTC)

==case of names of programming languages==

In producing a complete list (for the REXX example), I found several examples (errors?) in the case (upper/lower/mixed) of the names of some programming languages that somebody may want to correct (at least, make them consistent as far as their ''case'').
* ANT, ant
* AutoIt, AutoIT
* BASIC, Basic
* Bc, BC
* C sharp, C Sharp
* F Sharp, F sharp
* Gdl, GDL
* HaXe, Haxe
* Maple, MAPLE
* MATLAB, Matlab
* NewLISP, Newlisp
* OoRexx, OOREXX
* OpenEdge/Progress, Openedge/Progress
* Run BASIC, Run Basic
* UC++, µC++
* (unicode µC++)     [previously displayed as   '''╬£'''].
All the above names (both cases) appear as different entries in the categories section and have their own count.


Below are the specific counts for each ''case'':
*          rank: 449         (3 entries)  ANT
*          rank: 476         (1 entries)  Ant
*
*          rank: 126        (39 entries)  AutoIt
*          rank: 455         (2 entries)  AutoIT
*
*          rank:  53       (186 entries)  BASIC
*          rank: 424         (3 entries)  Basic
*
*          rank: 117        (42 entries)  Bc
*          rank: 463         (1 entries)  BC
*
*          rank:  22       (412 entries)  C sharp
*          rank: 482         (1 entries)  C Sharp
*
*          rank:  50       (206 entries)  F Sharp
*          rank: 470         (1 entries)  F sharp
*
*          rank: 377         (3 entries)  Gdl
*          rank: 475         (1 entries)  GDL
*
*          rank: 178        (19 entries)  Haxe
*          rank: 416         (3 entries)  HaXe
*
*          rank: 266         (6 entries)  Maple
*          rank: 480         (1 entries)  MAPLE
*
*          rank:  43       (253 entries)  MATLAB
*          rank: 468         (1 entries)  Matlab
*
*          rank:  94        (67 entries)  NewLISP
*          rank: 316         (4 entries)  NewLisp
*
*          rank:  71       (128 entries)  OoRexx
*          rank: 457         (2 entries)  OOREXX
*
*          rank: 111        (46 entries)  OpenEdge/Progress
*          rank: 454         (2 entries)  Openedge/Progress
*
*          rank:  59       (178 entries)  Run BASIC
*          rank: 471         (1 entries)  Run Basic

(updated) -- [[User:Gerard Schildberger|Gerard Schildberger]] 01:25, 17 November 2012 (UTC)

(updated)--- [[User:Gerard Schildberger|Gerard Schildberger]] 17:30, 5 September 2012 (UTC)

:Thanks Gerard. I think this kind of info is useful for people writing other site scraping tools. --[[User:Paddy3118|Paddy3118]] 07:09, 5 September 2012 (UTC)

:: I was hoping for people to step up to the plate and fix the various ''cases'' as I lack the tools to locate the inconsistencies (or hell's bells, even the knowledge of which case it ''should'' be).  I looked at each of the four '''ANT''' (or '''Ant'''?) examples, but within the comments of the programs, various ''cases'' were used, so I couldn't tell which one is correct (or even preferred), and I certainly don't want to be the one correcting something like that.  I thought if I pointed out the problems, they would get magically get fixed (by the ''case'' fairy or something). Anybody with a magic wand to do a "presto, chango!" ? -- [[User:Gerard Schildberger|Gerard Schildberger]] 16:26, 5 September 2012 (UTC)
::: :-) but no magic wand, nor even pixie dust. --[[User:Paddy3118|Paddy3118]] 16:47, 5 September 2012 (UTC)

:You missed one: C Sharp/C sharp.  There are 14 such pairs, which is why the BBC BASIC solution (which merges them) lists a total of 470 languages compared with REXX's 484. [[User:RichardRussell|RichardRussell]] 00:59, 17 November 2012 (UTC)

:: That particular entry was most likely added after I ran the above run in August of 2012.  I haven't re-did it to find more ''mixed-case'' ... er, cases. -- [[User:Gerard Schildberger|Gerard Schildberger]] 01:13, 17 November 2012 (UTC)

::: I have updated the (above) list. -- [[User:Gerard Schildberger|Gerard Schildberger]] 01:54, 17 November 2012 (UTC)

-----

By the way, as the above get fixed/corrected (even partially), I'll whittle down the list (the list of incorrect ''cases'' of languages) so eventually (hopefully), there won't be a list anymore.  I hope that changing the "incorrect case" list won't goof up the ''history'' of this section, but I hate to leave ''errors'' hanging around long after they get fixed/corrected. -- [[User:Gerard Schildberger|Gerard Schildberger]] 16:35, 5 September 2012 (UTC)

-----

I've since changed the way the REXX program handles the above problems (concerning mixed case).   The REXX program now ignores the case of the language name, and now uses the first encounter as the name of the language as it appears in the REXX program's output.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:46, 4 December 2017 (UTC)

==unicode characters in languages== 

The REXX program translates the unicode   '''µC++'''   into the ASCII-8   '''µC++'''.


Along with that, the REXX program also translates   '''UC++'''   into ASCII-8   '''µC++'''   to be consistant. 


This reduces the language count by one. 


Previously, the REXX program was displaying the unicode   '''µC++'''   as   '''╬£'''. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:42, 24 January 2013 (UTC)


Also added the unicode translation of   '''╨£╨Ü-61/52'''   (Cyrillic   '''МК-61/52)'''   into   '''MK-61/52'''. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:24, 15 February 2013 (UTC)


REXX code for other unicode versions of programming languages have been added since then. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:45, 30 March 2013 (UTC)

== Task suggestion for update ==

Most of the code here is web scrapping exercises that will break at 500 (or 5000) names. Better to use the API and step through page by page the results (500 at a time to be polite). This requires an algo but will handle 500, 5000 or 5 million language names - future proof. See the Awk example for how to use &gcmcontinue= and &gcmlimit= .. also, many of the languages are using external programs for networking even though it's possible to use native language code to get a web page. As a demonstration of the languages we should be using native code when possible. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 05:12, 26 May 2015 (UTC)

:Hi. If you name the languages and their standard library features that they are not using then this might make it easier for them to update and/or enter into a dialogue about there non-use. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:17, 26 May 2015 (UTC)

::Hi [[User:Paddy3118|Paddy3118]] - I added some [[Template:improve]] tags to a few popular languages (C, Python, Perl 6, Haskell) but it appears other than Awk, every one makes the same "mistake" of calling Special:Categories&limit=5000 and then doing web scraping. I don't want to spam the page with nag templates. Maybe there could be a different task that required the API in blocks no larger than 500 with format=txt or json or whatever is available. It would demonstrate how to use an API's continue feature, which is very handy, I've used it often at Wikipedia when retrieving 10s of thousands of results.  -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 00:31, 27 May 2015 (UTC)

:::Yep. I saw. I see. I'll try and address the Python but having different Python examples that use the API and scraping -couldn't that be seen as a good way to show contrast (so long as the contrasting methods of solution are explained)? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:56, 27 May 2015 (UTC)
::::Yes good idea. There can be two versions. The [[Rosetta_Code/Rank_languages_by_popularity#Tcl| TCL section]] does just that. I'll reword the "improve" template along those lines. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 18:35, 27 May 2015 (UTC)

:::Hi [[User:Paddy3118|Paddy3118]] - regarding Python I tried the current code but Rosetta Code returns "Access Denied" based on my "browser signature". It must see it as a bot. It's not my IP as Awk works using the same API call. Also had a "Segmentation Fault" when running the C code. Just posting in case you want to try if different results. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 21:43, 27 May 2015 (UTC)

==limit=5000==
The jq example uses limit=5000:

```sh
'http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000'
```

This (together with the single-page query Category:Programming_Languages) seems like the simplest approach.  Is there a problem with that? --[[User:Peak|Peak]] ([[User talk:Peak|talk]]) 21:09, 27 May 2015 (UTC)

:[[User:Peak|Peak]]: I installed jq and ran it and it reports [[:Category:80386 Assembly]] has 4 pages but in reality it only has 1 page. It's best to use the API as web scrapping is fraught with problems, this is one of a couple ways things can go wrong. The MediaWiki API is very powerful and designed for this sort of thing. And you can get the results in json format and demonstrate the natural abilities of jq, it could even end up being less code.  -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 00:02, 28 May 2015 (UTC)

[[User:craigd|CraigD]] Wed, 29 Jul 2015 06:51:52 GMT: I noticed a problem while looking at the Nim example; there is a request limit of 500, while there are over 500 languages. Should this be bumped?
:{"warnings":{"categorymembers":{"*":"cmlimit may not be over 500 (set to 5000) for users"}

==why task change==

Why was this Rosetta Code task definition/requirements changed?

The following text was deleted:

```txt

Sort most popular programming languages based in number of members in Rosetta Code categories.
(from http://www.rosettacode.org/mw/index.php?title=Special:Categories&limit=5000)

```


This change/deletion changes the very definition of what's being reported. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:58, 28 May 2015 (UTC)

:It wasn't changed it was moved into the Notes section. There are two valid ways to retrieve the data, either via Special:Categories or via the API. This reflects how people are coding, they are doing one, the other, or both. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 01:51, 28 May 2015 (UTC)

:: The original requirement (see above box, ''Sort most popular ··· (from http://www.rossetta code.org/ ··· '') specifically mentions which web page to be used.   The new note section that was created which had removed/changed that requirement.   I'm not arguing that the change is wrong, but that it   ''was''   a change.   The original requirement ('''as worded''') was not moved to the new notes section. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:33, 28 May 2015 (UTC)

:::Many are coding using the API to retrieve the data as it accurately reflects language popularity which is the title of the task and obvious intention of the creator who has not posted on RC since 2010. That doesn't mean those who code using Special:Category are wrong thus there is room to do it both ways should you choose to follow a literal interpretation. -- [[User:3havj7t3nps8z8wij3g9|3havj7t3nps8z8wij3g9]] ([[User talk:3havj7t3nps8z8wij3g9|talk]]) 04:26, 28 May 2015 (UTC)

==Accessing RC via Java==

I'm trying to solve this in Java - the "obvious" Java code:


```txt

       String path = "http://www.rosettacode.org"
                   + "/mw/api.php"
                   + "?action=query"
                   + "&generator=categorymembers"
                   + "&gcmtitle=Category:Programming%20Languages"
                   + "&gcmlimit=500"
                   + ( gcmcontinue ? ( "&gcmcontinue=" + gcmcontinue ) : "" )
                   + "&prop=categoryinfo"
                   + "&format=txt"
                   ;
        URL url = new URL( path );

        Object content = url.getContent();


```


gets a 403 (Forbidden) response.
I found a question on Stack Overflow (related to another RC task) that suggested setting the http Agent property of the connection and it seems that is correct - by default, Java sets the user agent which stops the above code working. Interestingly, the .NET default is not to set the user agent which is presumably why the C# sample works.

Setting the user agent to "" appears to work, as does pretending to be a browser...


```txt

URL            url = new URL( ... );
URLConnection  uc  = url.openConnection();

uc.setRequestProperty( "User-Agent", "" );


BufferedReader bfr = new BufferedReader( new InputStreamReader( uc.getInputStream() ) );

String line = bfr.readLine();

etc...


```


== Racket code breaking ==

When I try to run the Racket code locally I get:


```txt

substring: starting index is out of range
  starting index: 9
  valid range: [0, 4]
  string: "Xojo"

```


.. which seems to be due to [[Xojo]] appearing in [[Category:ProgrammingLanguages]] without the "Category" prefix.  I'm not sure if this should be corrected in the page, in the Racket code or elsewhere.

FWIW, the Tcl solution uses <tt>regsub {^Category:} $name {}</tt> to get rid of the prefix, which seems safer than dropping the first 9 chars.  I suggest Racket either do the same, or skip entries it cannot parse.
