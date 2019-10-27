+++
title = "Talk:GUI/Maximum window dimensions"
description = ""
date = 2012-01-23T11:38:11Z
aliases = []
[extra]
id = 8845
[taxonomies]
categories = []
tags = []
+++

As titled, this task is impossible. Window size can have restrictions placed on it by the system's window manager. (This includes tiling window managers, a class of WMs I use as often as possible.)  If intent is to query the system about physical display parameters, then that's awesome, but the task needs to be renamed. Also, it needs to properly be aware of  multimonitor environments, and make a distinction between a monitor's display area and the total desktop. --[[User:Short Circuit|Michael Mol]] 13:53, 23 November 2010 (UTC)
: That's why I knocked it back to draft… –[[User:Dkf|Donal Fellows]] 16:32, 23 November 2010 (UTC)

Ok. You are right that the idea is to determine the physical display parameters for the maximum height and width of the usable display area in pixels (without scrolling).

We are talking about the display area here (not the total desktop area, which could be bigger than the screen). The values that are calculated represent the usable desktop area of a window maximized to fit the the screen.

For multiple monitors, the value returned should be the display area on the monitor which is related to the task (ie the monitor which would display a window if such instructions were given).

For a tiling window manager, the parameters should represent the maximum height and width of the display area of the maximum size a window can be created without scrolling. This would typically be a full screen window (minus any areas occupied by desktop bars), unless the window manager has restrictions that does not allow creation of a full screen window, in which case the values represent the maximum usable area (without scrolling).

I'll go head and paste this information into the task description

[[User:Markhobley|Markhobley]] 22:41, 23 November 2010 (UTC)
: Good improvements. To be honest, I think explicitly asking for "the display size of the current display area, where display area would be the current monitor" is fine. It's probably simpler and more straightforward. (By the way, have you read [[Rosetta Code:Add a Task]]? I like to see folks creating tasks, and that's a draft set of task creation guidelines. It'd be good to get more input.)--[[User:Short Circuit|Michael Mol]] 02:33, 24 November 2010 (UTC)

Would this task be better titled as "Determine the maximum height and width of a nonoversized window"?

[[User:Markhobley|Markhobley]] 19:29, 7 December 2010 (UTC)
