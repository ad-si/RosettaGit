+++
title = "Rosetta Code talk:Village Pump/Suggest a language"
description = ""
date = 2015-10-05T06:09:43Z
aliases = []
[extra]
id = 9979
[taxonomies]
categories = []
tags = []
+++

== Notice of merge ==
{{alertbox|yellow|'''These pages were merged:'''

[[Help:Request a new programming language]] &rarr; [[Rosetta Code:Village Pump/Suggest a language]]
}}

''The other talk page was at [[Help talk:Request a new programming language]].''

== Navigation ==
Just wanted to note that "Cucumber" is not a language per/se.  It is more of an IDE. Like Eclipse is not a language.
:I assume that's also the case with Qt?  It's an IDE and set of libraries (and compiler?), but the basic language is C++ (or a version of UML & Javascript)  [[User:Nerfer|Nerfer]] ([[User talk:Nerfer|talk]]) 00:37, 17 October 2014 (UTC)

About the navigation.  It is (most?) common to want to see how X task is done in Y language. For Example I 
wanted to see how "Associative Arrays" were done in JavaScript.  I went to Languages, then JavaScript, looked down the list and clicked "Associative Arrays". I expected to go right to the JavaScript of "Associative Arrays" but instead landed at the TOP of "Associative Arrays". I had to again list my JavaScript preference to get where I wanted. If there was a good search facility, I suppose I could have entered:
"JavaScript Associative" and get right there. Could your people spend some time on this issue to save us all some time?
Thanks,
--[[User:rlrandallx|Robin Randall]]22:21, 13 July 2011 (UTC)
:That is a limitation of MediaWiki. It has been discussed [[Rosetta_Code:Village_Pump/Features_Wanted#Simple_features|here]], but we have not had success. If you want to sign up and help us that would be great. As we are now (and for the forseeable future), we rely on volunteers, so it's tough to do much more than "default" with our setup. --[[User:Mwn3d|Mwn3d]] 19:43, 27 June 2011 (UTC)

Thanks for the quick response. I was also looking for "Create an Object" for JavaScript and realized "Object-Oriented" Category was a bit hard to find. Until a "Google-like" search mechanism becomes available, a good organization is needed. I was thinking maybe splitting Languages into 1. Legacy Languages 2. Object-Oriented Languages 3. Scripting Languages, etc. Yes, there might be some overlap, but major categories would not be "hidden". I have been trying to help since meeting Michael Mol over the phone. I've been in the industry for 30 years so have about 20 languages under my belt. I'll look into MediaWiki to see what they have.
Thanks,--[[User:rlrandallx|Robin Randall]]22:21, 13 July 2011 (UTC)

:That would be great! I would suggest planning your categorization publicly here first. We had a few attempts at categorizing tasks and at least one attempt at categorizing languages (based on features) before. In my opinion the task categorization is an overlapping mess now. At the very least you might find out that some categories are intuitive to you and not others (and maybe the other way). See [[Rosetta Code:Village Pump]] for directions on creating a topic -- I would suggest [[Rosetta Code:Village Pump/Language categories]] or something. Also you can come and chat about things in [[Special:WebChat|the IRC channel]] if you want, though not all users are active there. Thanks in advance for your help! --[[User:Mwn3d|Mwn3d]] 20:36, 27 June 2011 (UTC)

I just noticed your Search box in the lower left of the page.  I used it and found exactly what I wanted right away! I did have to do an extra click. If only we could go directly to: [[http://rosettacode.org/wiki/Associative_arrays/Creation#JavaScript]|Javascript x Assoc Array] Lower left is the last place your eyes look when scanning. I suggest you make it bigger and put it in the upper right of the page like Media Wiki does. Finding that Search box just made your site about three times friendlier!  What can I do to help make that happen? Any HTML wiz can move a field. I'll think about what you said and my language categorization idea. With space free on the left I can add some bullets (cubes?) --[[User:rlrandallx|Robin Randall]]22:21, 13 July 2011 (UTC)
: Heh. This ''is'' MediaWiki. Wikipedia switched to a fancy new theme that wasn't available to general MediaWiki last time I upgraded the server software. I'll get the server software updated again, it's just taking a while to hit the right opportunity. --[[User:Short Circuit|Michael Mol]] 11:25, 14 July 2011 (UTC)

I think if you take the below code and change the classes to "firstheading" or something like that, it should place it in upper right of page.  No harm in leaving the "sidebar" search where it is.


```HTML

	<div id="p-search" class="portlet">
		<h5><label for="searchInput">Search</label></h5>

		<div id="searchBody" class="pBody">
			<form action="/mw/index.php" id="searchform">
				<input type='hidden' name="title" value="Special:Search"/>
				<input id="searchInput" title="Search Rosetta Code" accesskey="f" type="search" name="search" />
				<input type='submit' name="go" class="searchButton" id="searchGoButton"	value="Go" title="Go to a page with this exact name if exists" /> 
				<input type='submit' name="fulltext" class="searchButton" id="mw-searchButton" value="Search" title="Search the pages for this text" />
			</form>
		</div>
	</div>

```

--[[User:rlrandallx|Robin Randall]] 5:10, 18 July 2011 (UTC)

== Syntax highlighting is impossible ==
Some programming language cannot be made syntax highlighting, such as Forth and TeX. (Actually it is possible to make syntax highlighting with Forth but it would be necessary to execute the code in order to do so, and therefore the syntax highlighter must be written in Forth; I believe such an implementation exists.) --[[User:Zzo38|Zzo38]] ([[User talk:Zzo38|talk]]) 06:09, 5 October 2015 (UTC)
