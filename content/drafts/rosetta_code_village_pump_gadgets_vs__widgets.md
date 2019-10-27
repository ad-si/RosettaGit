+++
title = "Rosetta Code:Village Pump/Gadgets vs. Widgets"
description = ""
date = 2010-11-28T03:07:36Z
aliases = []
[extra]
id = 8860
[taxonomies]
categories = []
tags = []
+++

Had a discussion with [[User:Short_Circuit]] today about MediaWiki extensions for JavaScript functionality.  Namely using the Gadgets extension vs. using the Widgets extension for JavaScript functionality.  For some comparison:


### Gadgets

Allows users to enable or disable various JavaScript functionality, very similar to the "My Scripts" menu on [[Rosetta_Code:Village_Pump/Javascript_Functionality_Add]]

* Not currently installed
* Configurable on a user-by-user basis
* Site-wide - scripts apply to all pages
* Scripts have to check the page they're on to make sure they should apply


### Widgets

Allows page authors to add a chunk of HTML code (including JavaScript) to a page using template-like syntax

* Already installed
* Not configurable on a user-by-user basis - they're either enabled site-wide or disabled entirely
* Is page- or template-specific - for example, we can add the Language Comparison script to only the task pages
* Scripts would not have to detect if they're on the correct type of page to activate themselves

--[[User:Tyrok1|Tyrok1]] 01:04, 28 November 2010 (UTC)


### Additional considerations


AFAIK, Gadgets requires me to bestow Sysop-level access to the wiki. Considering that the Gadgets extension doesn't suppport per-user configuration, I can understand why it would require a high level of access. I expect the requirement comes from the individual gadgets being contained in the MediaWiki namespace. However, it ''should'' alow me to at least move those privs to an explicit privelage group, and use a different namespace.  The more work has to be done at Sysop level, the less inclined I am to use it. --[[User:Short Circuit|Michael Mol]] 03:07, 28 November 2010 (UTC)
