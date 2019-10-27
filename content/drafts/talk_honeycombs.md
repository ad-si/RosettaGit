+++
title = "Talk:Honeycombs"
description = ""
date = 2012-10-27T22:40:47Z
aliases = []
[extra]
id = 9783
[taxonomies]
categories = []
tags = []
+++

Just some observations (not criticisms):
* The hexagonal shape of the widget will be interesting, as GUIs normally use rectangular bounding boxes. A rectangular box used in drawing will require one of three solutions to avoid overdraw:
** The use of a drawing mask
** Transparency in the widget's drawn image
** A tileable rectangular pattern not 1:1 associated with the widgets themselves (i.e. drawn as a background, with transparent widgets on top)
* The rectangle/hexagonal dissociation will be interesting for pointer hit checks, too; a rectangular widget would need to detect that the pointer check is outside its hexagonal region, and then it would need to be able to have that check passed on to the tiles below it.
--[[User:Short Circuit|Michael Mol]] 18:10, 25 May 2011 (UTC)

Question: Must the widgets be represented as distinct objects in the code examples? If so, that would preclude the use of things like the [[wp:Flyweight pattern]], which strikes me as a particularly appropriate tool for this problem. --[[User:Short Circuit|Michael Mol]] 18:10, 25 May 2011 (UTC)

:Personally, I would imagine that a rectangular background should be permitted.  I would also imagine that any implementation of "widget" should be permitted.  Finally, I would imagine that the task's slippery use of "honeycomb" (initially it's the collection of widgets and later on it's a single widget) and color (initially all colors are the same and later that becomes a non-specification) should be ignored.  --[[User:Rdm|Rdm]] 20:04, 25 May 2011 (UTC)
:: Ok, there were some changes made. On the selection of a hexagon, should the previous hexagon's 'selected' appearance/state be cleared? (i.e. should any previously-selected hexagon return to its original color?) --[[User:Short Circuit|Michael Mol]] 20:12, 25 May 2011 (UTC)

Mark, you might consider uploading a simple image (a low-filesize PNG would be perfectly appropriate) to illustrate. (An animated GIF to demonstrate behavior wouldn't be bad, if you can swing that) --[[User:Short Circuit|Michael Mol]] 20:20, 25 May 2011 (UTC)

It was looking good, but I think the requirement for the message output and the each-hex-only-once made it more complicated than the gains in illustrating the concepts involved. The user will know that X was selected because the hex changed color, and I don't see the illustrative benefit of each-hex-can-change-only-once. --[[User:Short Circuit|Michael Mol]] 02:56, 26 May 2011 (UTC)

This reminds me of a [[wp:Blockbusters (UK game show)|gameshow that used to be on UK TV]]. More on topic, would it be an acceptable solution of the task if there was a single containing widget containing a grid of hexagonal active regions that can be colored/highlighted appropriately? That's much easier to implement… –[[User:Dkf|Donal Fellows]] 09:44, 26 May 2011 (UTC)

This task seems oddly specific. --[[User:Paul.miner|Paul.miner]] 22:40, 27 October 2012 (UTC)
