+++
title = "MediaWiki talk:Common.css"
description = ""
date = 2010-06-17T12:26:51Z
aliases = []
[extra]
id = 2792
[taxonomies]
categories = []
tags = []
+++

By the way...if anyone wants to see some CSS changes, feel free to leave a comment. --[[User:Short Circuit|Short Circuit]] 21:59, 1 April 2008 (MDT)

Could you please add
 .re2 { background-color: #ccccff; }
 .re3 { background-color: #ffcccc; }
for Whitespace highlighting? --[[User:Ce|Ce]] 12:03, 26 January 2009 (UTC)
:They're in. And they work. I thought newlines got highlighted too. Maybe that was only on some other websites. --[[User:Mwn3d|Mwn3d]] 13:40, 26 January 2009 (UTC)
:: Thanks. Newlines don't seem to get any special markup in the HTML. There probably is a way to add that to GeSHi, but I don't know how. --[[User:Ce|Ce]] 15:37, 26 January 2009 (UTC)
::: What, specifically, are you looking for? --[[User:Short Circuit|Short Circuit]] 19:23, 29 January 2009 (UTC)
::::What I've seen is that a newline is highlighted as having a light green background (something like #66FF99). It looks like it highlighted a space at the end of the line. --[[User:Mwn3d|Mwn3d]] 19:40, 29 January 2009 (UTC)
:::::There's two things going on there.  First, GeshiCodeTag converts \ns in GeSHi's output to <nowiki><br/></nowiki> tags, regardless of language, because HTML ignores \n as far as formatting is concerned.  Second, I'm pretty sure <nowiki>
</nowiki> isn't supposed to have any logical width, so actually seeing a background color there is most likely a rendering bug in your browser.  The best thing I can think of for visibility is to leave the existing behavior there, but remove the background color for that element so as not to confuse it with a space.  Or insert an image, but that could make copying and pasting difficult. --[[User:Short Circuit|Short Circuit]] 15:37, 30 January 2009 (UTC)
::::::I can't seem to find any examples that highlight newlines. Maybe I'm crazy. It's not that important anyway. Don't worry about it. --[[User:Mwn3d|Mwn3d]] 15:44, 30 January 2009 (UTC)

== Adsense ==

What's the story on this Adsense stuff? I haven't seen any ads come up yet. --[[User:Mwn3d|Mwn3d]] 21:07, 10 January 2010 (UTC)
:Only anonymous users will see ads.  See [[Rosetta_Code:Village_Pump/Ideas_for_2010#advertisements.3F|advertisements in 2010]] and the comment on [http://rosettacode.org/mw/index.php?title=MediaWiki:Common.css&curid=4&diff=71369&oldid=22099 this diff].
::Even when I'm signed out I don't see ads. I only see a box for them. --[[User:Mwn3d|Mwn3d]] 21:48, 10 January 2010 (UTC)
::: Two things are at play here.  First, I have Adsense set up to ''only'' show relevant links, and not to fall back on more general "PSAs".  Second, changes to Adsense configuration won't take effect until the site has been re-spidered, which they say may take up to two weeks.

::: The short version? The box will be empty until Adsense has something that might be relevant. Otherwise, it'll be empty. (I'd prefer the portlet to disappear if it were empty, but I don't know that there's an implementation out there that would do that for me.  It would likely require hiding the div after crawling its contents to see if there's anything relevant.) --[[User:Short Circuit|Michael Mol]] 14:52, 11 January 2010 (UTC)

== Example Scrollbars ==

Is there a way to make the scrollbars on the examples optional? Right now they show up ''everywhere'', and that's kind of ugly. It's particularly an issue on pages with short examples! Is there a way to make the scrollbars optional so they only show up when things get long or wide? (I'm no CSS hacker...) –[[User:Dkf|Donal Fellows]] 10:50, 26 January 2010 (UTC)
: I'm not a CSS hacker, either; I expected browsers to be a little better behaved. I could tweak the highlight extension to add another  CSS class based on how wide (in char count) an example is, but that would only work based on assuming a particular window width. I'm open to suggestions from anyone who reads this. --[[User:Short Circuit|Michael Mol]] 14:08, 26 January 2010 (UTC)
:: I suggest for width you can probably use 80 columns as a switch point. Classic terminal width! For length, pick a value that's a page or so in your browser (or maybe 66; the classic length of a page of typewritten text). –[[User:Dkf|Donal Fellows]] 14:12, 26 January 2010 (UTC)
:: Try <code>overflow: auto;</code>. In Firefox, this creates scroll bars only as necessary (I tried it out with Firebug, and it works great). [http://www.w3.org/TR/CSS21/visufx.html#propdef-overflow The standard] is vague as to exactly what should happen, but my guess is that most browsers will do what you want. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 15:32, 26 January 2010 (UTC)

== Hard Quoted strings ==

Some languages (e.g. [[J]]) now use the GeSHi setting HARDQUOTE rather than QUOTEMARKS to specify strings (AFAIU = less processing overhead). However the resulting string gets marked with a different CSS class (<tt>st_h</tt>) to normal strings (<tt>st0</tt>) and that class isn't currently handled by the RC style sheet.
The following change to the style sheet would display both types of strings the same way.


```txt
<nowiki>
.st0, .st_h {
     color: #ff0000;
}</nowiki>
```


:Could you add <tt>.st1</tt> there too? Languages that use both single- and double-quote strings (php for one) will show single-quote strings as plain text. --[[User:MizardX|MizardX]] 21:54, 19 May 2010 (UTC)

:Is there a reason not to make this change? AFAICS it won't break anything, and if per-language style sheets are introduced at some stage then they will override this default setting. --[[User:Tikkanz|Tikkanz]] 22:00, 16 June 2010 (UTC)
:: Seeing as I haven't changed it since January, I'd guess I've alternated between being busy or simply forgetting. Changed. --[[User:Short Circuit|Michael Mol]] 12:26, 17 June 2010 (UTC)
