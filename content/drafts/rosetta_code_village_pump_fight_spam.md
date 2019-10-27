+++
title = "Rosetta Code:Village Pump/Fight spam"
description = ""
date = 2015-04-28T02:43:24Z
aliases = []
[extra]
id = 4454
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Fight spam
|summary=On dealing with spam
}}
__NEWSECTIONLINK__
How can we fight this spam which is attacking RC nowadays? I don't like too much black listing of netblocks as suggested maybe somewhere, since you could block also "common" people. I've seen a brand new spam user after posting to Named Argument, and I've discovered I can't do too much more than saying I've seen it. I've also noticed the name of these spammers follow a pattern which could be identified (but likely will change...) ... --[[User:ShinTakezou|ShinTakezou]] 11:17, 29 June 2009 (UTC)
: On the Tcler's Wiki, we block problem netblocks from making updates (well, we actually show the spammer a preview page but never commit it to the database, which is a nicer solution as they ''think'' they've spammed you successfully) but without seeing the logs for addresses where those spam users are being created from, it's hard to tell whether that will work. It's a fairly stupid spammer though, since external links are all <tt>nofollow</tt>-marked. Maybe simple techniques will work for now. Plus visibly blocking that netblock from creating a new user too. —[[User:Dkf|Donal Fellows]] 13:51, 29 June 2009 (UTC)
::I didn't want to block the IPs because we had previously had a problem with an IP collision with a legitimate user. I'm not really sure what else I can do. We do have a [http://www.mediawiki.org/wiki/Extension:ConfirmEdit CAPTCHA], but maybe it's not good enough. --[[User:Mwn3d|Mwn3d]] 13:57, 29 June 2009 (UTC)
:::Since I think it's not robotic spam, I can't see that a CAPTCHA would help. —[[User:Dkf|Donal Fellows]] 14:13, 29 June 2009 (UTC)
::::Yeah and I wouldn't suggest turning off anonymous edits because we've had a recent surge of legitimate anonymous editors (and some people would probably think that was inconvenient). We may just have to keep up the old fashioned delete and block strategy. --[[User:Mwn3d|Mwn3d]] 14:25, 29 June 2009 (UTC)
:::::Gah.  Drop off the face of the planet for a weekend and come back to another spam influx.  It could very well be robotic spam if they have a human being sign up the account; CAPTCHAs are only presented to anonymous edits, account creation and login failures.  Those settings have worked well for us for the better part of two years.  Roboticizing after account creation was an eventuality, but it depended on someone deciding that RC was a big enough target to go the extra steps. (And extra steps are something that the spam economic model tends to avoid; They'd rather hit more weak targets than fewer higher profile ones.)  I'm not going to have time to tweak the server settings for a few days, at least.  In the mean time, let's watch to see if the problem is going to be bad enough to warrant significant attention. (Unless they've broken reCAPTCHA, it's roughly 1:1 manual labor, which is uneconomic for spammers.)  If need be, it might be possible to do a halfway-block; Rather than an outright ban on a user or IP, force all edits from them to go through reCAPTCHA.  But that will likely require modding an extension, which I don't have time for right now. --[[User:Short Circuit|Short Circuit]] 16:17, 29 June 2009 (UTC)
::::::I don't know if it's possible, but we can deny accounts containing "buy" in their names. --[[User:Guga360|Guga360]] 16:35, 29 June 2009 (UTC)
::::::: If the accounts are manually created, this will not give you much. As soon as the spammer gets the error message, he'll just change the account name to something which works. A better idea would be to special-case edits adding hyperlinks, and demand a captcha for those even for logged-in users. That would stop bots adding links, while not affecting normal users too much (few legitimate edits contain external links, therefore having to solve a captcha in those cases would not be too much of a burden). You could also maintain a whitelist for URLs not protected by captchas (e.g. everything on wikipedia.org), in order to minimize the impact for legitimate edits. --[[User:Ce|Ce]] 09:10, 30 June 2009 (UTC)
:::::::: For Wikipedia there's the special <tt>wp:</tt> link domain. —[[User:Dkf|Donal Fellows]] 11:01, 30 June 2009 (UTC)
:::::::: Don't give an error message to the spammy accounts. Just silently fail to commit any changes they make. (Better would be giving them their own view of the world, but that's more work.) —[[User:Dkf|Donal Fellows]] 11:13, 30 June 2009 (UTC)
:::::::::Then diagnosing and resolving false positives would be a PITA. --[[User:Short Circuit|Short Circuit]] 14:55, 30 June 2009 (UTC)
:::::::: If links trigger captchas, then the bots will just post raw URLs.  I've seen that one before... --[[User:Short Circuit|Short Circuit]] 14:55, 30 June 2009 (UTC)
::::::::: A raw URL is a link in the wiki, so naturally it should trigger captha, too. --[[User:PauliKL|PauliKL]] 09:14, 2 July 2009 (UTC)
:::::::::: Test: http://m-w.com/ http://news.google.com/ http://slashdot.org --[[User:Short Circuit|Short Circuit]] 16:02, 2 July 2009 (UTC)
::::::::::: Odd.  I that must have been added during some upgrade since the site started; MW didn't used to do that.  Another test: ht tp://broken.com http:// broken.com  http://odd-fish . com --[[User:Short Circuit|Short Circuit]] 16:02, 2 July 2009 (UTC)

=Upgrades=
I may be about time for another MW upgrade too while we're messing around under the hood. We're on 1.13.3 and they're up to 1.15.0. --[[User:Mwn3d|Mwn3d]] 17:23, 2 July 2009 (UTC)
: I was going to do it last weekend, along with ImplSearchBot and fixing my desktop machine, but I instead spent the whole weekend cleaning in search of a missing $500 MSRP phone...  --[[User:Short Circuit|Short Circuit]] 19:03, 2 July 2009 (UTC)
::Updating WP would be good too. That's just hitting a button right? --[[User:Mwn3d|Mwn3d]] 19:05, 2 July 2009 (UTC)
:::Yes, if the FTP port was open, and if I was running an ftpd.  The WP upgrade mechanism is stupid that way.  July 4th is going to get in the way, but I'll see what I can do about it this weekend. --[[User:Short Circuit|Short Circuit]] 22:21, 2 July 2009 (UTC)
=Timing=

The creation of each of those accounts requires some form of manual attention.  Keep an eye out for a schedule on when they seem to be appearing.  For someone to go to that much work to spam a site like this is rather odd. --[[User:Short Circuit|Short Circuit]] 00:26, 1 July 2009 (UTC)
:We're seeing the same sort of spam at the [http://wiki.erights.org/wiki/Special:Recentchanges erights.org wiki], also a MediaWiki; if you want to do analysis looking there as well might be useful. (Feel free to help with the deleting, of course :-) ) --[[User:Kevin Reid|Kevin Reid]] 00:42, 1 July 2009 (UTC)

==Looks like CAPTCHAs don't work==
We're still getting spammed even with annoying levels of CAPTCHAs. Looks like this is some ass doing it manually or they've broken reCAPTCHA, though the fairly low rate of spamming indicates that this is probably manual. Time to ban some netblocks from doing updates to the database, given that nuking from orbit isn't an option. (When spam is a problem, there's no point trying half-measures first. ''They won't work.'' Spammers are the scum of the earth and have a financial incentive to boot.) —[[User:Dkf|Donal Fellows]] 11:18, 1 July 2009 (UTC)
: [http://groups.google.com/group/recaptcha/browse_thread/thread/4b60d4ff6601fe29?hl=en Read this.]  Seems that it's likely a manual effort in an attempt to create landing pages.  Banning netblocks isn't really going to help, as Tor makes for an easy workaround.  At this point, I'm thinking either utilizing an RBL blacklist, or come up with a Bayes-based edit filter based on the ConfirmEdit extension. (Ham gets marked via MediaWiki's patrol mechanism, while spam gets marked by page deletion.)
: The other thought is that spammers are putting manual effort into creating landing pages for email campaigns and the like.  We could conceivably #REDIRECT the spam pages to a common target page for the time being. --[[User:Short Circuit|Short Circuit]] 18:01, 1 July 2009 (UTC)
:: Blacklists or Bayesian filters are not very effective ways to filter spam, and they create false positives. A good spam filtering is based on what the spammers are actually selling: their contact information (e-mail address, web address etc.). I would think there is only one or just a few spammers that bother to manually create pages here in Rosetta Code, so it should be possible to add their contact information to spam filter manually. --[[User:PauliKL|PauliKL]] 09:59, 2 July 2009 (UTC)
::: You could also try simply turning off the creation of new accounts for a while (e.g., a couple of weeks) to encourage the spammers to go elsewhere. The number of new genuine users turned off by this is probably going to be quite small, and the problem does at least seem to be confined to user pages. —[[User:Dkf|Donal Fellows]] 13:23, 2 July 2009 (UTC)
:::: They're still using identifiable names that could be caught by RE like "[0-9]+\s*buy"; if stopped this way, they can for sure change approach, but if they are landing-pages, likely the username must follow a pattern an airplain can identify, and then they must change their OLS signals, so I believe it is not a so bad approach to fight them. I like the idea of silent failure and honeypot page, even though I've not the slightest idea on how it could be done on mediawiki. --[[User:ShinTakezou|ShinTakezou]] 13:42, 2 July 2009 (UTC)
::::: That was the approach I had in mind.  Need time to implement it.  Shouldn't take too long, but it'll require learning how to extend the ConfirmEdit extension. --[[User:Short Circuit|Short Circuit]] 15:47, 2 July 2009 (UTC)

==Couldn't?==
Couldn't CAPTCHA be disabled for already registered and "tested" users? --[[User:ShinTakezou|ShinTakezou]] 15:03, 2 July 2009 (UTC)
: That's where we were earlier this week.  I might reduce the captcha conditions this weekend if the spam doesn't drop off.  At any rate, I'm going to have to mod things a bit to deal with spammers, and it might be possible to drop the CAPTCHAs altogether at that point. --[[User:Short Circuit|Short Circuit]] 15:46, 2 July 2009 (UTC)

:: Could the ''captha'' issue be re-addressed (as far as being more friendly to tried-and-true bona fide Rosetta Code contributors)?   I have a bunch of updates/fixes to do (more like a ton of "renaming" to clean up orphan files) and at this point, it's just a massive waste of my time trying to fight the captha "who's on first" game.   (Could it be changed to rock-paper-scissors-Spock-lizard? --- just kidding.)   How about what MathWorld (TM) uses for it's "bug" reports?   MathWorld has never failed me yet.   If it's good enough for the gander, it's good enough for the goose.   It's bad enough just adding links or worse yet, creating a new page, and ''then'' adding a link that you forgot. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:40, 18 March 2014 (UTC) 

==Done?==
It looks like it's done for now. Did anyone do anything or did they just give up? --[[User:Mwn3d|Mwn3d]] 18:25, 9 July 2009 (UTC)

== Block anon users from uploading images? ==

We've just had another influx of spam, this time from (a) non-logged-in user(s?) with many IP addresses, all within the USA. (Yay for http://ip-lookup.net/ which makes confirming this sort of thing much easier.) Analyzing the nature of the spammer's technique leads to the conclusion that they're putting the bulk of their spam in an image and then wrapping a page around that.  While there are many parts that should be blocked, I'd suggest that the big one is to block uploading of new images so that only logged in users can do it; hardly any legit images (well, any at all?) are ever uploaded by anonymous users — unlike contributions to solving tasks — so blocking won't hurt site growth much. It will also make it easier to clean things up; only a single page will need clearing, instead of multiple. Blocking non-logged-in from all page creation might be a nice extension, if possible, but it's more intrusive (and more likely to encourage irritating changes of tactics on the part of the <s>scumbag gits</s> spammers). –[[User:Dkf|Donal Fellows]] 10:50, 8 February 2011 (UTC)

:I know how you feel about the <s>scumbag gits</s> spammers, and support your call for upload restrictions. ''However'', these particular scumbag gits then went on to create a user account which originally left me confused until I saw that Mwn3d had been cleaning their crap earlier this morning. -[[User:Paddy3118|Paddy3118]] 13:39, 8 February 2011 (UTC)
:: FWIW, I did change the settings on uploads. MW now requires registered, autoconfirmed credentials. --[[User:Short Circuit|Michael Mol]] 13:42, 16 February 2011 (UTC)

== Possible spam ==

* [[User talk:RepairToolbox]]
* [[File:Dog_hiking_backpacks_4877.jpg]] irrelevant image, submitter was blocked some time ago
* [[File:AP_Specialties_-_CPP-3086_-_Lowe_Promotions_4662.jpg]] irrelevant image, submitter was blocked some time ago

== What about a programming-related Turing test? ==

Long time ago I registered to a [http://random.irb.hr/ quantum random bit generator] online service.  I remember they had a qualifying question for registering new users:  http://random.irb.hr/signup.php

I suppose they have a pool of questions that are randomly picked.

We could use something similar, but with programming-related questions.  It could even be the subject of a Rosetta-code task.
--[[User:Grondilu|Grondilu]] 21:50, 20 November 2012 (UTC)

PS.  A new user candidate could chose a question about his preferred language.  Here is a simple example, assuming the user is a Perl6 adept:

"Which virtual machine hosts a very famous implementation of Perl6, and has the same name as a very talkative kind of bird?"
--[[User:Grondilu|Grondilu]] 22:04, 20 November 2012 (UTC)

: RC uses an extension based off of the SimpleCaptcha MediaWiki extension. Show me some options that build on SimpleCaptcha, and we can look at changing up the captchas. Keep in mind many of RC's users are not very good with English. --[[User:Short Circuit|Michael Mol]] 22:32, 20 November 2012 (UTC)
::Ahh I failed to realise that the Captcha system is part of the mediawiki software, and not some custom code that we might tweak easily to fit our needs.  My bad--[[User:Grondilu|Grondilu]] 02:26, 21 November 2012 (UTC)

: Isn't QuestyCaptcha a standard feature of the MediaWiki capthca system? I think it would be much better alternative than an image based based captcha. It is both easier for normal user and more effective against spammers. There are captcha-breaking services in India, where thousands of workers routinely provide answers to captchas. It is very easy for them to type in the text they see in the image (especially since they have lots of practice). But even a simple question such as "what is the name of this forum" may be impossible for them to answer, if they only see the captcha. --[[User:PauliKL|PauliKL]]

== Persistent spammer ==

Wouldn't it be better if we turn <code>rel="nofollow"</code> on for external links? [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 08:30, 19 May 2013 (UTC)
: The problem is that we have someone actively targeting Rosetta Code. I believe he simply desires to make work for spamfighting out of malice, not profit. The glut of spam is helping in one way, though...I can pull the blocked IPs out of the log and block common subnets. That said, I intend to write a MediaWiki extension to tie the block action with a "add to IP blocks at Cloudflare". This will result in each IP we block here getting a mark against it at Cloudflare. CF issues captcha challenges to users they suspect are hostile. I can see the list of challenges and which have passed...so far, none have passed, so the challenge must be effective against whichever bots they challenge. Now, CF uses heatmaps to figure out which IPs are likely malicious, and which aren't. However, those heat maps are only as good as the data fed into them...whatever mechanism this spammer uses to hide his source IP (be it zombie or proxy) will get blocked by Cloudflare for CF customers, putting a pretty strong pressure against its use. (And if sites like Wikimedia and Wikia pick up the plugin, so much the better.) --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 12:40, 19 May 2013 (UTC)
:: I don't have an opinion on the ip blocking strategy. The problem with the current setup is that you need to delete active links as soon as possible to avoid them being spidered. With rel=nofollow in place there's less time pressure on the admins. If the guy is truly malicious then, in my experience, only post screening of new users would be effective anyway. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 19 May 2013 (UTC)
Would it perhaps be an idea to disable account creation, and in stead invite new contributors to drop us a line, and tell us a bit about their interests / background in programming (without demanding sensitive information like real name and such)? [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 15:57, 29 May 2013 (UTC)
: nofollow has been added. If someone with MediaWiki coding chops could write up an extension to tie MW's blocking logic with Cloudflare's API, that would be most excellent. I could do it...if I had the time. I very much don't right now. The key thing needed is described in section 4.7 of CF's [https://www.cloudflare.com/docs/client-api.html API docs]. IPs should be able to be blocked when IP blocks are added to the site, and should be removed from CF when removed/expired from the site. (There's also some excellent possibilities for proper cache purging logic, but that's another issue...) --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 03:14, 2 June 2013 (UTC)
:: We're getting absolutely inundated with spammers here. Maybe a hundred new account registrations a day. What's worse, the block-by-IP appears to be catching about half the spam attempts. Is it possible to turn off registration without manual approval? Or failing that turn it off entirely?
::FWIW, there appears to be two spammers/spamming teams; one puts fairly small amounts of material in their User page (one or two links), and the other (''much'' more prolific) does two edits to their UserTalk page to put in a large slab of text with variable links. I'm pretty sure that both have people involved at some point; it's not just a bot.
:: But whatever you do, please do something to stem the tide. I don't want to spend my time dealing with this sort of thing instead of contributing… –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 14:25, 11 July 2013 (UTC)
:::The same thing happens on Hitchwiki. Until it crashed a few weeks ago (it's nearly restored), new posters could not add external links until after having made at least 10(?) normal edits. For the rest we just block everyone with the user names that you are also getting here. Also, we have an extension, "Recent Changes Cleanup" that basically flips the bot bit, allowing us to hide all activity of spammers, and a little bit of custom js that fills in the block page with all the right tick-boxes, so that all we have to do is click the "Block" button. FWIW, my username on Hitchwiki is also [http://hitchwiki.org/en/User:Prino Prino] and I am an admin on the site, and I'd be quite happy to give a hand here. [[User:Prino|Prino]] ([[User talk:Prino|talk]]) 12:03, 8 September 2013 (UTC)

:When I get the RSS updates, I see the deleted material, and on a whim I followed an older one. Even with "nofollow" the material is still there. If what this "person" is trying to do is notice to some site, even the "nofollow" might not be enough to dissuade "it" and the mechanism of the usual deletion but still retaining the archived edit is possibly aiding "it". Is there a method to do a more destructive kill of the entered text without destroying the historical record completely? [[User:Stormneedle|Stormneedle]] ([[User talk:Stormneedle|talk]])

::: I am thinking we should do several things here:

::::1) turn off registration entirely, we can live with it turned off for a week. 
::::2) Live with email registration for a time (email has highly developed spam filters)
::::3) Since people are behind this, I think we should try and engage them in conversation. We need to understand whether they are willing to talk about their interest (or hostility :/ - but if it goes there, I do not think that anyone here is going to have time for them) or we need to find whether they are too afraid to do so. I cannot say anything further about them without that conversation.
::::4) We should probably also set up another (slower, difficult) mechanism so that we can catch complaints and problems. Perhaps a dedicated post office box would be a good choice here? Does anyone feel like affording that?
::::5) Eventually, it would be good to re-enable on-site signup?
:::: --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 15:30, 11 July 2013 (UTC)

== Random ideas - viability hypothetical. ==

It seems to me that the RC is just collateral damage of a spammers spambot. i.e. The Spammer has a spamscript, and they merely entered "rosettacode.org" as a target and whamo... RC has screeds of fake spam-users...

Is it possible to hook into "wikipedia" or "stackoverflow" or "linkedin" some other collaborative web site, and ensure they candidate RC contributors are somehow bona-fide there? (e.g. a +1000 rating) Or have a "staging" RC where their contributions are "supervised" somehow for 10 contributions.

Or maybe charge them a small fee for their first 10 contributions, eg $10 each.  Which is refundable after the tenth contribution by "royal decree" ( TimToady, Mwn3d, Paddy3118  or ShortCircuit etc) OR via a RC member plebiscite?

Just putting it out there... [[User:NevilleDNZ|NevilleDNZ]] ([[User talk:NevilleDNZ|talk]]) 08:09, 1 June 2013 (UTC)

: I notice that Cloudflare supports reporting the "threat level" of an IP address...it should thus be possible to write a MW plugin that blocks activities (such as account creation, uploads or link addition) to addresses above a particular threat level unless the user is in a blessed group. --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 03:14, 2 June 2013 (UTC)

Here's a new task, perhaps. Write something to take the first three letters and the seventh letter of the username and see they match the first four letters (minus spaces) of the posting. If there's a match, flag for examination by an admin. For example, in the last two accounts where I marked the posting for removal, the elements of the username (marked in capitals) matched the posting's first four characters, viz:
   WEBdaeHsqgzios -> Web Hosting 21
   ORGwnhYgtqqffo‎ -> Orgy Oil Naturally
There may be other patterns here. For example, how long are the usernames? [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 09:54, 2 June 2013 (UTC)

### Span Avoidance by Parsing Code

* Basically if you eliminate URLs by running a regexp filter on user input you can hopefully eliminate some spam
* Also parsing the language code against the given interpreter/compiler is a more expensive way to ensure the submission to RC is valid code and not some spam.
Both of these options may be expensive to implement.

==Recent Changes==
Is it possible to not show user account creation/deletion in thae recent changes? Is this spam also the reason I can no longer find the link to upload files?--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 13:03, 2 June 2013 (UTC)
:: There's an option in your preferences/recent changes that allows you to group changes together. That also helps a lot. [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 14:24, 2 June 2013 (UTC)
::: Yup. Though it does require enabling Javascript. --[[User:Short Circuit|Michael Mol]] ([[User talk:Short Circuit|talk]]) 15:09, 2 June 2013 (UTC)

=Anti-spam 'bot=
I am currently trying out a script that:
# Trawls [[Special:RecentChanges]] looking for some types of new pages
# Displays the new page
# Gives you the option of deleting the page and blocking the creator by entering [n]/y
# Appends to a macro file of deletion commands.
# After quizzing you on each new page, you run the generated macro on the browser and sit and watch all the pages and users you selected disappear.
Cathartic!
(Plus it cuts down on the RSI whilst keeping human control of the 'bot).
 --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 02:29, 24 June 2013 (UTC)

==Bot registration==
Anyone know how to get the edits of the 'bot recognised as 'bot edits so that the 'Show/Hide Bots' links on the [[Special:RecentChanges]] page works appropriately for them? That way they could then be easily filtered from view. Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 02:37, 24 June 2013 (UTC)

==Captcha is completely stopping me==
I sympathize with spam issues. Unfortunately, for some reason the captcha doesn't like me at all. I'm looking at clear text, no ambiguous letters, and I type it in and get rejected. :-/  [[User:Gcanyon|Gcanyon]] ([[User talk:Gcanyon|talk]]) 22:42, 6 July 2013 (UTC)
:I completely agree with [[User:Gcanyon|Gcanyon]]. The captcha is pretty annoying. I just get illegible text and it doesn't get any better if I try to reload it to get another one. Here are some screenshots (I've added whitespace because otherwise I would need to solve a captcha which I'm just not able to do):
:* http: //www.freeimagehosting.net /newuploads /z6fqb.png
:* http: //www.freeimagehosting.net /newuploads /39e1d.png
:--[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 22:38, 16 January 2014 (UTC)

== New captcha ==

Google announced [http://googleonlinesecurity.blogspot.de/2014/12/are-you-robot-introducing-no-captcha.html new captcha].

(Seeing fresh batches of "new users" daily, RC would make a good beta-tester :)  --[[User:Hajo|Hajo]] ([[User talk:Hajo|talk]]) 18:32, 3 December 2014 (UTC)

==Spam as Task==

We could set as a task the challenge of writing a tool to post spam to RosettaCode. With all of us writing one, we might gain some insights into what's being used against us and perhaps think of new ways of stemming the tide. [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 02:13, 28 April 2015 (UTC)

: Meh... if we do that we should definitely be doing it against a scratch copy of the site. But I doubt the insights would be that useful unless we were actively engaged not against this specific site but against a large collection of sites (and presumably with a misguided political or economic motive, so that the activity would be sustainable). Meanwhile note also that whatever is being used today will not necessarily be used later.

: No, I think that if we want to deal with this, we'll need to design some sort of "apprentice-track" mechanism where people need to invest some effort showing their worth before we let them use the site as a launching point for things directed elsewhere. And we'll also need volunteers to help mentor that intro mechanism. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 02:43, 28 April 2015 (UTC)
