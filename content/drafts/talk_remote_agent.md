+++
title = "Talk:Remote agent"
description = ""
date = 2014-12-10T07:51:23Z
aliases = []
[extra]
id = 8704
[taxonomies]
categories = []
tags = []
+++

== Event parameters? ==

The ''color'' and ''ball'' events need to describe the color of their respective subjects; how is this done? –[[User:Dkf|Donal Fellows]] 16:34, 10 November 2010 (UTC)
: Wow, I wrote this a while ago. I'll have to look at it again and get back to you. I vaguely recall trying to write an implementation, then discovering a flaw in the spec. --[[User:Short Circuit|Michael Mol]] 19:49, 10 November 2010 (UTC)
: Hm. At a quick glance, I'd guess ORing the relevant events with one of four values situated in the high nibble would do it. However, it might make more sense to create four unique events each for 'color' and 'ball', to keep them in the printable range. Adjust to suit; nobody's written any code. --[[User:Short Circuit|Michael Mol]] 19:53, 10 November 2010 (UTC)

Is this a multi-user game? I mean, is the world shared globally among all users, or is there only a single agent per world? In the latter case, a new world would be created whenever a client connects, and this world would last only for that client's session, right? --[[User:Abu|Abu]] 11:04, 16 December 2010 (UTC)
: It's not yet clearly enough described, so… “maybe” has got to be the answer. –[[User:Dkf|Donal Fellows]] 13:24, 16 December 2010 (UTC)
:: For this one, single-user makes the most sense, I think. I'll have to get back later today and fix some problems with the task description. --[[User:Short Circuit|Michael Mol]] 13:28, 16 December 2010 (UTC)
::: Single-user, for sure; otherwise, clients would have to be forced to be in sync with each other, and there would be a race condition if two agents picked up a ball at the same time. The intent of the task was to have as a precisely defined protocol and virtual machine. --[[User:Short Circuit|Michael Mol]] 22:31, 16 December 2010 (UTC)

"There may be 'wall' sectors within the area": Are there any specific rules for that? A percentage, like for the balls? How should the walls be arranged? The spec says that closed areas are prohibited, but in the extreme case this may still end up as a maze. --[[User:Abu|Abu]] 11:30, 16 December 2010 (UTC)
: Maze circumstances are fine; the intent in description was to prevent a ball from existing in a place where it couldn't be reached. --[[User:Short Circuit|Michael Mol]] 22:31, 16 December 2010 (UTC)
:: Great. Then I propose to pass another argument for the number of walls. For consistency, I would prefer to also pass the ''number'' of balls (instead of a percentage). What do you think?
: The specification for "Map Generation" requires that "no non-wall sector may be surrounded on four cardinal sides with wall sectors." This isn't strong enough to ensure that all (non-wall) cells are reachable. Was that the intention? --[[User:Tim-brown|Tim-brown]] ([[User talk:Tim-brown|talk]]) 12:30, 1 December 2014 (UTC)

Now I have a working server implementation (see the task). Concerning the color events, I propose to use upper- and lowercase characters: R G Y B for the fields (sectors), and r g y b for the ball. In an analog way, if we say that uppercase characters stand for sector attributes, and lowercase for ball attributes, and use some punctuation characters for special operations, the following makes sense to me:

{|border=1
|-
|name||type||character||decimal||hex
|-
|forward||command||^||94||0x5E
|-
|turn right||command||>||62||0x3E
|-
|turn left||command||<||60||0x3C
|-
|get||command|||@||64||0x40
|-
|drop||command||!||33||0x21
|-
|game over||event||+||43||0x2B
|-
|stop||event||.||46||0x2E
|-
|color red||event||R||82||0x52
|-
|color green||event||G||71||0x47
|-
|color yellow||event||Y||89||0x59
|-
|color blue||event||B||66||0x42
|-
|ball red||event||r||114||0x72
|-
|ball green||event||g||103||0x67
|-
|ball yellow||event||y||121||0x79
|-
|ball blue||event||b||98||0x62
|-
|bump||event||<nowiki>|</nowiki>||124||0x7C
|-
|sector full||event||S||83||0x53
|-
|agent full||event||A||65||0x41
|-
|no ball in sector||event||s||115||0x73
|-
|no ball in agent||event||a||97||0x61
|}
: Epic timing. I'd just rewritten the commands myself. I like your command set better, though.--[[User:Short Circuit|Michael Mol]] 22:27, 16 December 2010 (UTC)

== Client implementations ==

There's now a client implementation in [[Tcl]], split into two parts (as I believe you had in mind). OK, the agent logic is mind-numbingly stupid as it has virtually no memory at all, but that's how it goes. –[[User:Dkf|Donal Fellows]] 11:31, 7 January 2011 (UTC)

== game ==

maybe this should be called "simulation" instead of "game". --[[User:Oenone|Oenone]] 08:54, 19 April 2011 (UTC)
: If you give it a goal, such as 'fewest steps possible', then it becomes a game, does it not? I'm not saying it's a conventional or particularly fancy game, though. --[[User:Short Circuit|Michael Mol]] 12:16, 19 April 2011 (UTC)

== Stream? ==

The Task talks about a "stream". Does this mean a TCP stream? Or is it just a stream without defining what medium to use? --[[User:Oenone|Oenone]] 09:17, 19 April 2011 (UTC)
: Any bidirectional stream is probably fine; some outside-program glue can be used as an adapter. For example, if there's a TCP-based server, but a STDIN/STDOUT-based client, one can use tcpwrappers to get the client and server talking to each other. --[[User:Short Circuit|Michael Mol]] 12:14, 19 April 2011 (UTC)

== First Turn ==

Once the handshake is exchanged, do I understand correctly that the RA has no idea about the cell it stands on until it successfully moves into another cell... bump won't describe your current cell. Could we have a stay "_" command, which although not awesomely useful for getting things done would at least show the contents of the first cell without changing the initial exchanges in the stream protocol. --[[User:Tim-brown|Tim-brown]] ([[User talk:Tim-brown|talk]]) 07:51, 10 December 2014 (UTC)
