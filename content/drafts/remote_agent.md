+++
title = "Remote agent"
description = ""
date = 2010-12-16T22:27:14Z
aliases = []
[extra]
id = 6038
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Remote agent
|summary=A set of three tasks revolving around the concept of a remote agent
}}
This is a set of three tasks revolving around the concept of a 'remote agent.' The overarching theme of the task set is to have a simple, stateful world with an agent that operates on the world's state until the end conditions are met. The world is a grid of colored squares, and is populated to some percentage by colored balls. The ending condition is when all balls are in a square of the correct color. The perspective of the agent is limited; it is not given an overview of the world. It only knows its surroundings by responses to its actions. Read on below for a complete description of the virtual world, interface commands and stream protocol.

Each of the three tasks satisfies a different goal:
# [[Remote agent/Simulation|Implement the world simulation, accept and respond to agent commands via a stream interface]].
# [[Remote agent/Agent interface|Implement the agent side of the stream interface in a manner idiomatic to the language]].
# [[Remote agent/Agent logic|Use the agent stream interface to satisfy the end conditions]].

Here is a description of the world and commands.  The stream protocol below should be common among all implementations; any agent interface implementation should work equally well with any of the world simulation implementations.

= A description of the world =
== Physics==
* time is quantized. There is only a turn count. No time elapses if no commands are received.
* distance is quantized on a grid of squares. No coordinate data is provided. Any adjustment in location takes one unit time per unit distance.
* Orientation is quantized, with four units making a complete rotation. (i.e. each unit is 90 degrees.) adjustments occur at one-unit intervals; two turns in the same direction result in a reversal of bearing with two turns spent, four turns in the same direction result in no ultimate change in bearing, though with four turns spent.
* A sector may posess no more than one ball at a time. (A ball posessed by an agent in the sector does not count.)
* An agent may posess no more than one ball at a time.

== Grid sectors == 
Each grid sector has one of the following type properties:
* Space - an agent may exist here, and may move in and out without impedance.
* Wall - an agent will not exist here. Any attempt to move into this sector will fail, and a "bump" event will be triggered.


###  Sector color 

Each 'space' grid sector has one of the following color properties (no 'wall' sector may have these properties):
* Red
* Green
* Yellow
* Blue


###  Ball color 

Each 'space' grid sector may have one and only one of the following properties (no 'wall' sector may have these properties):
* Red ball
* Green ball
* Yellow ball
* Blue ball

== Map generation ==
* The grid is always bounded. The outermost rim of sectors must have the 'wall' property.
* While the distribution and location of the color properties is undefined, there must not be more balls of a color than there are sectors with that color property.
* There must be at least one 'space' sector without a ball in it. (The agent does not initialize while holding a ball.)
* There may be 'wall' sectors within the area (including on the interior side of the outermost rim), but the no non-wall sector may be surrounded on four cardinal sides with wall sectors.

== Goal ==
* The game ends when the agent holds no ball, and there are no balls in a sector whose color property does not match that of the ball.

= Agent activity rules =
== Movement ==
* With the exception of attempting to move from a 'space' sector to a 'wall' sector, which is denied, nothing impedes movement.
* Movement always occurs at one unit distance per unit time.

== Ball manipulation ==
* An agent may hold one and only one ball at a time.
* An agent may only drop a ball if it has one.
* An agent may only drop a ball while in a sector that does not have a ball in it. The ball the agent held is transferred to that sector.
* An agent may only pick up a ball while in a sector that has a ball in it. The ball that was in that sector is transferred to being held by the agent.

= Agent interface =

== Commands ==

###  forward 

* Moves ahead one space. Costs one turn if successful.
* Will respond with a 'color' event indicating the color property of the now-current sector.
* Will respond with a 'ball' event if there is a ball in the now-current sector.
* Will be responded to with 'bump' event if unsuccessful, with no turn spent, meaning there is a 'wall' sector "ahead".
* Will respond with a 'stop' event, once all other events have been sent.


###  clockwise 

* Orients the agent one unit clockwise, costing one turn. (Recall that four units make a full rotation.)
* Will respond with a 'stop' event, once all other events have been sent.


###  counterclockwise 

* Orients the agent one unit counterclockwise, costing one turn. (Recall that four units make a full rotation.)
* Will respond with a 'stop' event, once all other events have been sent.


###  get 

* Transfers the ball in the current sector to the agent, if successful. Costs one turn, regardless of success.
* Will respond with a 'no ball in sector' event, if there is no ball in the sector.
* Will respond with a 'agent full' event, if the agent already has a ball.
* Will respond with a 'stop' event, once all other events have been met.


###  drop 

* Transfers the ball in the agent to the current sector, if successful. Cost one turn, regardless of success.
* Will respond with a 'no ball in agent' event, if the agent has no ball.
* Will respond with a 'sector full' event, if the sector already has a ball.
* Will respond with a 'game over' event, if successful, and if all balls are in sectors matching their color.
* Will respond with a 'stop' event, once all other events have been met.

== Events ==


###  game over 

* Sent when the winning condition has been reached.


###  stop 

* Sent when all other events for the current command have been sent.


###  color 

* When sent, indicates the color of the current sector.


###  ball 

* When sent, indicates the color of the ball in the current sector.


###  bump 

* When sent, indicates the ahead command failed due to obstruction, no turn spent.

=== 'no ball in sector' ===
* When sent, indicates that there is no ball in the current sector, and that a 'get' command failed.

=== 'no ball in agent' ===
* When sent, indicates that there is no ball in the agent, and that a 'drop' command failed.

=== 'sector full' ===
* When sent, indicates that the sector already has a ball, and that a 'drop' command failed.

=== 'agent full' ===
* When sent, indicates that the agent already has a ball, and that a 'get' command failed.

= Stream Protocol =
The stream protocol is simple. Every command is a single byte sent from the agent to the simulator, no line feed, no carriage return. Every event is a single byte sent from the simulator to the agent, no line feed, no carriage return.  For the sake of simplicity, all bytes fall within the range of [65,90], which corresponds to [A,Z] in ASCII, as well as in UTF-8.

== Greeting ==
The first byte is sent by the server to the agent, indicating that it's ready: '''A''' (65)

The second byte is sent by the agent to the server, indicating that it's ready: '''A''' (65)

The purpose of the handshake is to prevent the agent from sending commands before the server can service them, and to allow the server to identify which command set the agent intends to use. The former is important because I would like to eventually run the server via inetd on Rosetta Code's server, and I don't want to open myself up to an accidental DDOS if the server is under heavy load. The latter is important because I'd like to be able to do other things with the same port in the future, and the agent's reply byte differentiates this simulation game from others.

== Order of events ==
While all commands will trigger an event, the order that events are sent is undefined, with the exception that the '''stop''' event will signify that processing has finished; the '''stop''' event is the last event sent in response to ''any'' command, including any command that triggers the '''game over''' event.

== Commands and event codes ==


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
