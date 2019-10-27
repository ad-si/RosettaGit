+++
title = "Rosetta Code talk:Village Pump/Javascript Functionality Add"
description = ""
date = 2010-07-18T22:28:36Z
aliases = []
[extra]
id = 7375
[taxonomies]
categories = []
tags = []
+++

== Language comparison script ==

[[Rosetta Code:Language comparison script]]

This script puts checkboxes next to each language in the ToC which can be checked to compare a number of languages.  I chose to leave them underneath the ToC rather than floating them next to it due to the observation that many of the examples have fairly long line lengths, making side-by-side comparison rather difficult space-wise.

--[[User:Tyrok1|Tyrok1]] 00:16, 20 May 2010 (UTC)

: I took the liberty of playing around a bit with the DOM after your script did its job: [[:File:Compare Languages Concept.png]]. Depending on the display resolution a side-by-side comparison of at least two languages should be possible in most cases, I think. The ToC in its smaller form there might need work; that doesn't look too compelling yet, but it's at least much shorter (vertically) than before. But that was just a rough idea what could be done. Overall I think a side-by-side view would be quite nice to have. —[[User:Hypftier|Johannes Rössel]] 06:22, 20 May 2010 (UTC)

: Jeepers, have you got a large screen.  :)  I'll see what I can do.  --[[User:Tyrok1|Tyrok1]] 14:07, 20 May 2010 (UTC)

: New version's up at the same link - [[Rosetta Code:Language comparison script]]  Please let me know what you think.  --[[User:Tyrok1|Tyrok1]] 21:01, 21 May 2010 (UTC)

: The script is on its own page now.  The links are updated to reflect this.  --[[User:Tyrok1|Tyrok1]] 03:19, 23 May 2010 (UTC)

: Updated links after moving page to Rosetta Code namespace.  --[[User:Tyrok1|Tyrok1]] 03:18, 25 May 2010 (UTC)

Could this maybe be changed to that it doesn't show the boxes on talk pages? Sometimes they get enough sections to get ToC's, but they don't need to be compared. Unless it would be advantageous to only show some sections of talk pages...? --[[User:Mwn3d|Mwn3d]] 03:18, 26 May 2010 (UTC)

: The new version is supposed to only activate if it's on a page that's in the Programming Tasks category, so in theory it shouldn't do that anymore.  However, it's certainly possible you found a bug, and if the new version's still doing that, can you post an example link?  --[[User:Tyrok1|Tyrok1]] 23:17, 26 May 2010 (UTC)

== Direct links to language examples when coming from a language page ==

Just a random thought/idea: Since languages are categories they simply aggregate all pages that are tagged with that category. For languages hwoever, there is also a pretty rigid structure on this site concerning ''where'' the interesting part (concerning a particular language) is on a task page. Currently the links to the tasks from a language page simlpy link to the task page (which for a normal Wiki is fine since it's the ''page'' that's in the category, not a section). But on RC those links could – theoretically – link directly to the appropriate section (which is probably what a user might expect when clicking on a task link on a language page).

For most languages this is rather simple, I think, since the anchor name on the task page is identical to the language's name (which is also the category name for that language). However, for a few languages, including C# and F#, this doesn't quite work, as their categories are named »C sharp« and »F sharp« and the anchors use »C.23« and »F.23« (apparently url-encoded and <code>%</code> replaced by <code>.</code>). The name used in the anchors appears to be nowhere to grab for JS on the language page, although this might be solvable with converting the infobox into a template which makes the intended/canonical name accessible via a <code>&lt;span class='intended-name'&gt;</code> or so. –[[User:Hypftier|Johannes Rössel]] 21:18, 21 May 2010 (UTC)

== Per-code example buttonbar ==

Buttonbar set per-code example. Including features like:

* "Select all" -- for easy copy
* "Try on Codepad" -- for languages which support Codepad
* "Collapse/Shrink" -- to optionally shrink/expand code examples.
* "Show/Hide tags" -- Adding to arbitrary categories like [[:Category:Examples/Tags/Functional]], [[:Category:Examples/Tags/pure code]] or any other arbitrary tag


: Didn't add Collapse/Shrink, as I'm not sure this is necessary with the language comparison script.  Didn't add Show/Hide Tags as I don't believe there are tags yet to show or hide.  However, this script implements Select All, Copy to Clipboard (in IE), and Try on Codepad:
: [[Rosetta Code:Per-Code Example Buttonbar]]
: --[[User:Tyrok1|Tyrok1]] 01:14, 6 July 2010 (UTC)


:: Just made a change that should fix it.  If you could try it out again and let me know if it works, that'd be great.  --[[User:Tyrok1|Tyrok1]] 12:08, 13 July 2010 (UTC)

== Syntax highlighting selector color picker ==

Color picker for GeSHi highlighting CSS selectors. Save to cookies or HTML5 local datastore. (There's currently no serverside datastore for RC) --[[User:Short Circuit|Michael Mol]] 20:36, 26 June 2010 (UTC)


== Live version of all JS + HTML examples ==

Implement a button (possibly in the corner button bar) where you could try out any JS + HTML example on the site.  You could easily do this by detecting which &lt;pre&gt; tag in the example was HTML and assuming the other (if it exists) is JS, popping open a new window, and inserting this code into it.  If there's a separate &lt;pre&gt; tag for JS, it could be inserted into the HTML before being placed in the new window.
--[[User:Tyrok1|Tyrok1]] 00:24, 13 July 2010 (UTC)
: I've been thinking about that, but that's leaving things way, ''way'' open for arbitrary code execution in an unsafe environment. I was thinking about crypto-signing code examples, but that's too complicated. The next best option I can think of is dumping the JS code into an HTML page, allowing the user to save the option to save it to disk, and then let them open it there. --[[User:Short Circuit|Michael Mol]] 00:32, 13 July 2010 (UTC)
:: I would think having the user save out the code to a file to run it could potentially be even more problematic, as the domain's local to the computer.  If you've got code that's visible to the user and they click the "Try it" button, I would think the window should be sandboxed to the RC domain, preventing cross-domain requests to other sites.  There are a couple potential risks I can see, but I don't think the likelyhood of them happening is great and if they did happen, the access they have would be very limited due to browser security constraints.  Could you clarify your main concerns a bit more so we can see if we can work around them?  --[[User:Tyrok1|Tyrok1]] 00:56, 13 July 2010 (UTC)
::: Local to the RC domain is worse, from a wiki integrity standpoint. If the scripts were run at (subdomain).rosettacode.org, can the get access up to rosettacode.org? Or would I need a sideways domain to limit it? --[[User:Short Circuit|Michael Mol]] 01:56, 13 July 2010 (UTC)
:::: IIRC it's generally limited to the subdomain, and in this case I believe it would be the subdomain of the task page.  Unless you had some kind of a server-side script on a separate subdomain that would just echo back anything it got from a GET request.  Otherwise you wouldn't be able to access it from the rosettacode.org-based task pages.  This would sandbox it into its own subdomain, but I would suspect it would be infinitely worse by being exploitable from outside the site for phishing attacks against users unless you were to limit it to only requests that come from the rosettacode.org domain name.  Another solution would be to have someone evaluate the code and place it on a page where it's locked down permission-wise and have the script check to see if one of th10ese is available.  As it is, the solution I've seen used is that people are hosting examples on personal websites and linking to them, which is fine from a wiki integrity point of view, but from a convenience and user security point of view, I'd consider it a lesser option.  Any other thoughts/ideas?  --[[User:Tyrok1|Tyrok1]] 02:28, 13 July 2010 (UTC)
::::: Running it on a separate domain was what I was thinking about, using signed requests. I was thinking of running the code+salt(server-only-known secret key)+salt(timestamp mod (sitewide cache expiry time)) through a hash, and verifying that signature on the echo side. The problem is whether I would need to register a separate something.org, or if something.rosettacode.org would be secure, from a JS standpoint. --[[User:Short Circuit|Michael Mol]] 13:40, 13 July 2010 (UTC)
:::::: From my experience, a subdomain should be sufficient.  --[[User:Tyrok1|Tyrok1]] 13:45, 13 July 2010 (UTC)
::::::: Any browser with a debugger (chrome, firefox with firebug or venkman, and probably others) lets you do this already with a copy and paste.  --[[User:Rdm|Rdm]] 14:55, 13 July 2010 (UTC)
:::::::: Not everyone has access to browsers with debuggers (most notably those in locked-down environments), so it still strikes me as a worthwhile pursuit. --[[User:Short Circuit|Michael Mol]] 14:57, 13 July 2010 (UTC)
::::::::: Hmm, ok... but a locked down environment which prevents the kind of javascript inspection allowed by a debugger and which allows or requires execution of javascript from arbitrary web sites is just asking for problems.  --[[User:Rdm|Rdm]] 15:05, 13 July 2010 (UTC)
:::::::::: The implementation I had in mind would only echo back code+execution if it said code were signed using a timestamp(the server has a cache timeout, and I was thinking of using that as the basis for the timestamp precision.) and  server-secret key (only known to my MW install and to the echoing script). That way, debuggers aren't disabled by the echo'd page, and the only code sent by the server is known to come from the RC wiki. As for client-side environments that prevent users from making changes, but don't apply additional restrictions on server-provided content, well, those environments are pretty common. I've worked in one or two, and cleaned up a half dozen more. --[[User:Short Circuit|Michael Mol]] 15:13, 13 July 2010 (UTC)


== "My Scripts" Menu ==
[[Rosetta Code:My Scripts Menu]]

This script adds a "My scripts" menu next to the "My preferences" button which allows users to choose which JS modules they want to use.  It stores the state of these choices in a cookie so it can automatically load the same modules whenever they navigate to a new page.  --[[User:Tyrok1|Tyrok1]] 22:28, 18 July 2010 (UTC)
