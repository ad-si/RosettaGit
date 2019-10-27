+++
title = "Rosetta Code:IPv6"
description = ""
date = 2012-01-23T16:01:56Z
aliases = []
[extra]
id = 11289
[taxonomies]
categories = []
tags = []
+++

[[File:World IPv6 launch logo 256.png|frame|left|See [http://www.worldipv6launch.org/ World IPv6 Launch] ]] Rosetta Code is dual-stacked, with native IPv6 connectivity through its [http://prgmr.com VPS host provider], and operates just fine over IPv6. My regular access to Rosetta Code has been via IPv6 for some time, now.

The IPv6 address is 2605:2700:0:3::4713:91bf, and there is already a AAAA record in DNS for 'rosettacode.org'. All content-holding subdomains of 'rosettacode.org' 301-redirect to a path under http://rosettacode.org/, so accessing any Rosetta code site will ultimately connect you to Rosetta Code, if you have suitable (i.e. non-Teredo, non-6to4) IPv6 network connectivity.

Rosetta Code's [http://dns.he.net DNS servers] are also dual-stacked: ns2.he.net, ns3.he.net, ns4.he.net, ns5.he.net.

The only known problem with Rosetta Code's IPv6 access, currently, is that the route between AS41075 and AS18779 appears to break at AS8928.
