+++
title = "Talk:Animate a pendulum"
description = ""
date = 2015-07-14T21:53:28Z
aliases = []
[extra]
id = 4672
[taxonomies]
categories = []
tags = []
+++

It's moderately tricky to make this work perfectly, especially for large amplitudes. The issue is that there is no neat analytic solution of the equation of motion (not without getting into fairly hairy maths). Since this is to demonstrate simulation and not mathematical prowess, I suggest hacking it. (I have some code that simultaneously plots a phase diagram when running the simulation, which is very nice but detracts from the basic requirements of the task so I omitted it.) —[[User:Dkf|Donal Fellows]] 15:42, 7 August 2009 (UTC)
:Assuming you're referring to the introduction to my example: Sure. I just figured a description of the principle would be useful, to help understand without studying any particular example's code details (such as your refined approximation, and my "k" constant). Feel free to edit that info into the general task description. (Another way to do the pendulum might be to simulate position/velocity vectors and the pendulum arm as a constraint on distance from the center; this might be done if you have a general physics engine handy.)
:By the way, I think it would be good to have an explanation of the principle of the approximation procedure used in the Tcl example. --[[User:Kevin Reid|Kevin Reid]] 17:16, 7 August 2009 (UTC)

:: I'm not very proud of it. I wanted to use [[wp:Simpson's Rule]] for the integration since I knew the differential equation for pendulums from way back, but couldn't get that to work without solving it first. Chicken-and-egg! So solve it once using the guess that the velocity is constant over the time period to get approximate locations for the other points of the integration, and solve it again to get a better estimate of the velocity and, consequently, position. I've tested it and it works well enough for a few minutes of simulation at the initial angle I selected, which is all I think anyone will actually bother with. (I suppose I could do better if I really drilled into the math, but whatever...) —[[User:Dkf|Donal Fellows]] 21:44, 7 August 2009 (UTC)

There has been a mistake in the MatLab code (now commented in the code), but occasionally in other languages as well.
