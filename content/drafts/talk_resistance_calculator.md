+++
title = "Talk:Resistance Calculator"
description = ""
date = 2019-03-23T12:58:07Z
aliases = []
[extra]
id = 22220
[taxonomies]
categories = []
tags = []
+++

--[[User:ChristerNilsson|ChristerNilsson]] ([[User talk:ChristerNilsson|talk]]) 11:02, 14 March 2019 (UTC)
I need help with the picture. File Upload seems to be broken since 2016.
I also would like to set tab size to two spaces, if possible

==Headings==
Hi, you also need to '''not''' use subheadings in the task description - they are reserved for Language examples exclusively.

You can get that kind of effect using:

;Not a heading

<nowiki>;What is typed for the above type of bold subheading starts with a semicolon</nowiki>

All the best :-)

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 13:45, 14 March 2019 (UTC)

==Tab Size==
Although I'd be delighted to be proved wrong, there doesn't appear to be a way to change the tab size on this site which is fixed at 8.

If you want to use anything else (personally I prefer 4), then use spaces rather than tabs.
--[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 10:09, 15 March 2019 (UTC)

--[[User:ChristerNilsson|ChristerNilsson]] ([[User talk:ChristerNilsson|talk]]) 15:53, 15 March 2019 (UTC) In Github I just add a file .editorconfig where tab width is stated. I find tabs has a lot of advantages. Smaller files, fewer keystrokes, and each individual might use his/her own tab width. But, this is religion and not even Nim allows tabs. I must use the following directive as the first line in every .nim file: #? replace(sub = "\t", by = "  ")

== Task Name ==
Two things: 

:1 In general, Rosettacode task names should only have the first word in a title capitalized. (Excluding proper nouns) It isn't a hard rule, and has been broken before, but is the preferred style. 

:2 Is there any particular reason this is named '''Parallel''' resistor calculator? Seems like it is doing both parallel and series resistances. I would propose to rename it to "'''Resistance equivalence calculator'''" to frontload what the primary point is: "Resistance", more accurately describe what it does, and bring it in line with site naming conventions. (Or even just '''Resistance calculator''' ) --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 12:23, 15 March 2019 (UTC)

--[[User:ChristerNilsson|ChristerNilsson]] ([[User talk:ChristerNilsson|talk]]) 15:44, 15 March 2019 (UTC)
I agree, name changed to Resistance Calculator

==Picture==
--[[User:ChristerNilsson|ChristerNilsson]] ([[User talk:ChristerNilsson|talk]]) 15:46, 15 March 2019 (UTC) I would like the picture to be displayed directly, without being a link.

== Shunting Yard ==

--[[User:ChristerNilsson|ChristerNilsson]] ([[User talk:ChristerNilsson|talk]]) 16:02, 15 March 2019 (UTC)
I added a Nim solution for a better handling of infix input.
Ideally, the source code should not have to be recompiled for every new input.
Also, most of the code in Infix and RPN are identical. So, they might be merged.

Input change proposal:
* Infix: 2 + 3
* RPN: 2 3 +

This proposal unfortunately has the implication that operator overloading is no longer needed.

== Task extensions ==

Serial/parallel circuits are a simple case, there is a more general case where you can model the resistor network with a sparse symmetric matrix and solve a linear system to get the resistance between any two points of the network. See [[Resistor mesh#Maxima]] for instance. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 23:51, 19 March 2019 (UTC)

The Mesh is very interesting. I haven't figured out how to enter such a beast into a solver using either infix or RPN. The mesh example is very symmetric and easy. How would you propose dealing with a spaghetti mesh? --[[User:ChristerNilsson|ChristerNilsson]] ([[User talk:ChristerNilsson|talk]]) 01:12, 20 March 2019 (UTC)

Ok, there is a solution, but I think it should stand on it's own feet. Using the picture and labelling the nodes we get: 
AC6 CH8 CD4 DH8 GH4 DG6 DE10 FG8 DF6 EF10. Then we can use the Mesh code. --[[User:ChristerNilsson|ChristerNilsson]] ([[User talk:ChristerNilsson|talk]]) 02:23, 20 March 2019 (UTC)

The Mesh example has 180 resistors and 100 nodes. The description might be A1 A2 1|A1 B1 1| .. |J9 J10 1. Then the iteration might start.
Should be able to solve my problems as well. --[[User:ChristerNilsson|ChristerNilsson]] ([[User talk:ChristerNilsson|talk]]) 02:32, 20 March 2019 (UTC)
:A "spaghetti" mesh would still lead to a symmetric matrix: this does not come from the symmetry of the circuit, but from Kirchhoff's laws. If there are N nodes the matrix A has dimensions NxN, and there is a nonzero element A(i,j) for each nodes i,j linked by a resistor. To describe an arbitrary circuit, you need basically to be able to decribe an arbitrary graph: a (usually sparse) matrix, or a list of nodes together with a list of edges, for instance. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 20:17, 21 March 2019 (UTC)

Eoraptor: [https://www.rosettacode.org/wiki/Resistance_Network_Calculator Mission accomplished] --[[User:ChristerNilsson|ChristerNilsson]] ([[User talk:ChristerNilsson|talk]]) 11:08, 22 March 2019 (UTC)
