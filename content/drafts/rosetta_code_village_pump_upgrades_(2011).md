+++
title = "Rosetta Code:Village Pump/Upgrades (2011)"
description = ""
date = 2011-10-07T03:16:44Z
aliases = []
[extra]
id = 10627
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Upgrades for 2011
|summary=System upgrades for 2011. Servers. Software and protocols
}}
I've been working on upgrading things. I've been working on it for quite some time, with a large degree of frustration. That tells me it's time to ask for help...and I probably should describe the things that I *know* are going to change. If there are some upgrades you'd like to see, be sure to tack them on in a list somewhere below. --[[User:Short Circuit|Michael Mol]] 00:11, 7 October 2011 (UTC)

Right now, Rosetta Code is sitting on a VM whose public location can be found at prgmr1.rosettacode.org. That domain name will continue to be associated with this VM for as long as this VM exists. Because prgmr1 is running Debian 5, and I want to upgrade to Debian 6 at the same time as a slew of other upgrades, I've got prgmr2 running Debian 6, with squid and a LAMP stack on top of it. I've even got prgmr1's MySQL database replicating to prgmr2. Once prgmr2 is ready. Pretty much all Rosetta Code domains will be pointed at that VM. --[[User:Short Circuit|Michael Mol]] 00:11, 7 October 2011 (UTC)

'''Changes coming:'''
* IPv6. If you look, you'll notice prgmr2.rosettacode.org has a AAAA record as well as an A record. While prgmr1 has a public, global-scope IPv6 address, squid2 doesn't support IPv6, so I can't simply flip a switch and turn it on--if I add the AAAA record, RC will break for clients with native IPv6. prgmr2 is running squid3, which does support IPv6. The IPv6 stuff is already in place and working on prgmr2 to the same extent IPv4 stuff works on prgmr2. --[[User:Short Circuit|Michael Mol]] 00:11, 7 October 2011 (UTC)
* TLS. If you haven't heard of Firesheep, well, you should go read up on it. If you haven't heard of TLS, think of it as 'SSL version 3 and up.' Session hijacking has become child's play, and the techniques have been packaged up enough for script kiddies. It's easy to say that it's on users to not use open wifi networks and the like, but it's really not practical. I've now got a class 2 identity cert and a class 1 server cert. I'll be getting a class 2 server cert just as soon as I get things working with the class 1 cert. I'll be able to set up a *.rosettacode.org cert, and that'll be good. ''This is the part that's currently blocking me.'' Not getting the class 2 cert, but getting Apache and squid to play nice while putting a TLS front on things. --[[User:Short Circuit|Michael Mol]] 00:11, 7 October 2011 (UTC)
::I assume you've heard of [http://www.theregister.co.uk/2011/09/19/beast_exploits_paypal_ssl/ the BEAST]? Short version: TLS 1.0 broken; TLS 1.1+ ok but not yet widely-supported. -- [[User:Eriksiers|Erik Siers]] 03:12, 7 October 2011 (UTC)
:::I heard about it. It doesn't change the need to begin deployment. --[[User:Short Circuit|Michael Mol]] 03:16, 7 October 2011 (UTC)

* Apache server upgrades. Nothing special here. --[[User:Short Circuit|Michael Mol]] 00:11, 7 October 2011 (UTC)
* MySQL server upgrades. Nothing special here. --[[User:Short Circuit|Michael Mol]] 00:11, 7 October 2011 (UTC)
* PHP upgrades. Nothing special here. --[[User:Short Circuit|Michael Mol]] 00:11, 7 October 2011 (UTC)
* MediaWiki upgrades. Hopefully, this will fix some of the upload issues people have been having. --[[User:Short Circuit|Michael Mol]] 00:11, 7 October 2011 (UTC)
* Upgrades to all extensions which have upgrades available. I could use some help putting together a list of things which can be upgrades. (You should be able to get a good overview at [[Special:Version]]). --[[User:Short Circuit|Michael Mol]] 00:11, 7 October 2011 (UTC)
* [[User:tyrok1]] wrote a few useful JS bits I'll be adding to the global JS load. --[[User:Short Circuit|Michael Mol]] 02:34, 7 October 2011 (UTC)

Anyway, I can use assistance with testing, planning (i.e. checklist prep) and guidance in getting things working.
: Also, whaatever folks want added, infrastructure-wise, I'll add it if possible.--[[User:Short Circuit|Michael Mol]] 02:34, 7 October 2011 (UTC)

'''Additional requests:'''
*I'd like [http://www.mediawiki.org/wiki/Extension:CategoryTree Category Tree] if you don't think it will break things. SMW may be able to replace most categories, but that has been going slow. Since we have categories inside categories inside categories right now, I think it would speed things up for new users. --[[User:Mwn3d|Mwn3d]] 00:48, 7 October 2011 (UTC)
::I second this. -- [[User:Eriksiers|Erik Siers]] 03:12, 7 October 2011 (UTC)
