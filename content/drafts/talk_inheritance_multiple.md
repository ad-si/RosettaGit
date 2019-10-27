+++
title = "Talk:Inheritance/Multiple"
description = ""
date = 2010-04-08T02:38:45Z
aliases = []
[extra]
id = 6809
[taxonomies]
categories = []
tags = []
+++

I have to question the point

''There is no need to implement any functions for those classes.''

Surely this example should require the user to make (empty) functions

* Photo() in Camera
* Call() in MobilePhone
* Action() in both Camera and MobilePhone just to throw a spanner in the works

and then end up with a functioning CameraPhone class which also resolves the conflict between Action in both parent classes.

As an added bonus, I'm thinking it might be useful to have a function Power(on: boolean) in both parent classes that could profitably be merged in the CameraPhone class - let's assume they both fire off a pulse to the battery circuit. This function could possibly be provided by another class which parents both Camera and MobilePhone, resulting in a diamond inheritance graph. 

The difficulty of merging, selecting or renaming the various functions would be very interesting to see. Further, it would show the need to write bridging methods in all the languages that only offer Interfaces without defined code.

Even more interesting would be to require some member variables in both parent classes, probably two variables that also illustrate the difficulty or ease of merging, selecting or renaming the members in the CameraPhone class.

There's a lot of subtlety in multiple inheritance, and a lot of hot air blown both for and against it. There's also a lot of hot air blown about the different features allowed in interfaces (as a work-around for full MI) which should be illustrated with clear examples.
--[[User:Andrew Clarke|Andrew Clarke]] 02:38, 8 April 2010 (UTC)
