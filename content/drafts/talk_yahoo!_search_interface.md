+++
title = "Talk:Yahoo! search interface"
description = ""
date = 2011-09-20T16:29:29Z
aliases = []
[extra]
id = 4125
[taxonomies]
categories = []
tags = []
+++

==TOS violation==

:Note -- the following applies to an earlier version of the task but not the current task.

This task violates [http://www.google.com/accounts/TOS Google's TOS].

<blockquote>
'''5.	Use of the Services by you'''

: 5.3	You agree not to access (or attempt to access) any of the Services by any means other than through the interface that is provided by Google, unless you have been specifically allowed to do so in a separate agreement with Google. You specifically agree not to access (or attempt to access) any of the Services through any automated means (including use of scripts or web crawlers) and shall ensure that you comply with the instructions set out in any robots.txt file present on the Services.
</blockquote>

--[[User:Kevin Reid|Kevin Reid]] 10:51, 3 May 2009 (UTC)
: I'd say just put a warning at the top of the page that says something along the lines of '''Usage of this code violates section 5.3 of Google's Terms of Service, unless you have special arrangements with Google''', but I'm pretty sure that folks who have such special arrangements also get access to a different API, and probly an authentication key or other token.
: So what's the purpose of the task?  Is it to call an API and perform some sort of scraping operation on the result?  Can someone whip up a low-footprint web script I can put on Rosetta Code's server as an alternative?  Or is the task's value specifically in that it aids automate Google searches, and redirecting to a different server and API makes it useless?  If the latter is the case, then I would suggest we delete it.  Not because I believe it's necessarily inappropriate to describe how to do something that a TOS or other rule says one isn't allowed to do, but because if we piss Google off, that's 68% of our traffic that gets lost if Google decides to remove us from their search index.
: Again, if the problem is the need to interact with an API on a remote server, and do some web scraping on the results, I'll put up any suitable and lightweight script provided on the server for people to test against.
: In the mean time, I'm erasing the code, and modifying robots.txt to not index anything but /wiki/*. (I should have done that ages ago, anyway; Google indexes all the results of clicking on the Edit links, as it stands.) --[[User:Short Circuit|Short Circuit]] 12:34, 3 May 2009 (UTC)
:: I could turn my [http://paddy3118.blogspot.com/2009/02/extended-vanity-search-on-rosetta-code.html Vanity Search] blog entry into an RC task, but don't we already have a task that extracts info from RC stats pages? Maybe we should just let this other "search a web site that needs user input and gives answers on multiple pages" type task just die? --[[User:Paddy3118|Paddy3118]] 12:56, 3 May 2009 (UTC)
::: Actually, I'd prefer to avoid too many more tasks that pull from MediaWiki-supplied content; They're unkind to the DB backend, and my slice doesn't have enough RAM for memcached or squid to be particularly helpful when dealing with a high request-per-minute rate; apache processes fill 256MB pretty quick when they pile up waiting for a response from MySQL, which gets slower as it gets starved for RAM. (I can't afford to spend more on RC server performance unless RC can pay for itself.)  If scraping and list navigation is the ultimate goal, something much more lightweight can be provided, that doesn't even touch MySQL. --[[User:Short Circuit|Short Circuit]] 15:40, 3 May 2009 (UTC)

Google's <cite>Use of services by you</cite> seems clear about the ''usage''; of course they can't prohibit to write such scripts. Nonetheless, if we drop here a code, likely it will be tested (I like to know if a code work or not... not just by reading at it, but executing it), and this violates that paragraph. Which sounds rather odiosus (I've realized now I've violated it a lot of times with ''ad hoc'' LWP perl scripts, mechanize libraries, wget and curl... after all the HTTP headers are all they can use to "know" if it's a "real browser" or not), but after all the task maybe is not worth the risk for RC.

I've not read the task requirements. But I believe there can exist search engines that have not a paragraph like that and allow to be used even not with their interface; and after all, if the point was just to show how to use a search engine and analyze its results, we should not be interested particularly in using the Google indexes... if someone else is, it can use the knowledge here gained at his/her own risk. --[[User:ShinTakezou|ShinTakezou]] 15:47, 3 May 2009 (UTC)

Google API token isn't a thing easy to get. But some languagaes (Like .NET) have a real browser User-Agent, such as "Mozilla/5.0 (Firefox 3.0; X11)", other languges, like Python, you need to "hack" because user-agent is "Python/urllib2.0". Searching Google, sometimes maybe very util for some people. I think that putting a disclaimer in top of page is a nice solution. --[[User:Guga360|Guga360]]
: Normally, I'd be fine with hosting code that when used violates someone's TOS; The existence of the code isn't illegal in the country where RC is hosted (with the exception of copyright protection, DRM and the DMCA).  The worst most entities can do is send me a DMCA notice (Which is easily attainable via WHOIS), I take down the code, and that's that.  If ''Google'' gets upset with Rosetta Code, they can strike RC from their search index, and we lose 68% of our traffic and exposure.  And, AFAIK, there's no recourse short of getting something like the Slashdot community up in arms.  A DMCA notice is one thing; I can fight or fold.  Getting dropped from Google's search index is rather like being cut off at the knees. --[[User:Short Circuit|Short Circuit]] 19:44, 3 May 2009 (UTC)
::The task title can be changed, and disallowed by robots.txt, and a disclaimer. But i think that this is insufficient too. Feel free to remove that task. --[[User:Guga360|Guga360]]
: I've done a little bit of experiments; using directly a wget will result in a 403 http answer. Changing the user-agent (even a void string, or a non existing like MikeyMouse/1.0!) worked... '''But'''... I've read Yahoo! TOS, and tried wget on Yahoo!, and it works even without changing User-Agent. So, maybe, this same task can be changed in order to use Yahoo! instead? (Anyone with a better english could check the TOS, if it really does not disallow what Google disallows) --[[User:ShinTakezou|ShinTakezou]] 21:50, 3 May 2009 (UTC)
::I would think that most search providers would not like programmatic searches as it is so easily abused. What would we loose by just dropping this task? Against what could be lost if we include it? --[[User:Paddy3118|Paddy3118]] 22:29, 3 May 2009 (UTC)
::: I've not read the task specs (just the title). If the task was interesting, we loose an interesting task; if it was not, ... But if the task's creator would like to keep the idea, then s/he could take a look at Yahoo. Of course, even though Yahoo's TOS allows for automated use of their ''service'', this does not mean we (or anybody else) can flood it with search requests! And what world would it be if I take a perfectly legal and TOS-compliant code from RC, then ''abuse it'' and for this reason RC has trouble?! (I am thinking about the TOS of Yahoo... it seems to me an automated script using Yahoo search engine wouldn't violate their TOS, so that they have no reason to consider RC responsible of any ''abuse'' of the code... if it is possible such a relationship between an ''abuser'' and the source for the code which made the abuse possible, sites like [http://www.w3.org/Library/ this] or [http://pavuk.sourceforge.net/ this] shouldn't be indexed at all!) --[[User:ShinTakezou|ShinTakezou]] 00:10, 4 May 2009 (UTC)
::::OK, i'm converting to Yahoo. And i'll add Python after this. --[[User:Guga360|Guga360]] 02:10, 4 May 2009 (UTC)
:::::Wow, faster than any other answer to my points:D ! Have you checked Yahoo's TOS? I would like to have confirmation about my interpretation of the text... I've not seen a section similar to Google's "section five", but ... those legal texts always bring me headaches. --[[User:ShinTakezou|ShinTakezou]] 10:00, 4 May 2009 (UTC)

== Enchanced results ==

What do you mean by "enchanced results" not working? (I suppose it should be "enhanced"; anyway the question is the same: what do you mean by "enhanced results", and is it a task requirements?) --[[User:ShinTakezou|ShinTakezou]] 22:34, 4 May 2009 (UTC)
:This is a enchanced result:
:[[Image:YahooEnch.jpg]]
:This is a normal result:
:[[Image:Yahoo.jpg]]

:A simple Regex change should fix it.
:Or just change your Yahoo settings to not display enchanced results. And use Yahoo Cookies in a HTTP Request.

:: Ok, this means it is not a task requirement handling them properly in the code... Right? --[[User:ShinTakezou|ShinTakezou]] 09:43, 5 May 2009 (UTC)

No, it isn't a task requeriment, but is VERY RECOMMENDED, because some results title will be like "Test - Wikipedia<nowiki><div id="ench-smb">...</div></nowiki>", but are 3 methods for fixing this:

1. Change regular expressions to match enchanced results correctly.

2. Change Yahoo settings to don't show enchanced results, and use your Cookie (as Guest) in a HTTP Request.

3. Remove anything after a "<div" in a result title.

--[[User:Guga360|Guga360]] 13:16, 5 May 2009 (GMT -3)

I don't know regular expressions too much.
Can someone convert 
```python
i[:i.index("</a></h3></div>")]
```
 in a regular expression? --[[User:Guga360|Guga360]] 17:02, 5 May 2009 (UTC)

== Haha! ==

Alarms are going off at Yahoo! "Look, look! Our search engine is being used!"
