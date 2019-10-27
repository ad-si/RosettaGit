+++
title = "Talk:Banker's algorithm"
description = ""
date = 2019-04-17T18:27:14Z
aliases = []
[extra]
id = 19610
[taxonomies]
categories = []
tags = []
+++

== What is being tested here? ==

The wikipedia [[wp:Banker%27s_algorithm|Banker's algorithm]] page is currently a bit messed up (see the wikipedia talk page), and the examples here do not quite match the task description (there's no testing against the numbers listed as "Total resources in system" in the task description).

So I think the task needs a better description of what exactly is being tested here.

If I read the code right, each process is run sequentially. But we do not have a description of what each process does, here. I think we need a description of these "processes". --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 09:29, 30 September 2015 (UTC)

== EWD108 transcription error ==
If you're going all the way back to EWD108, be careful of your source.  WP links both a scanned document and a transcription.  If you look closely at the scan you can see some ≤ signs were transcribed as <.  --[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 17:01, 18 August 2017 (UTC)

== More reading material ==
WP mentions EWD-108 and EWD-623, but I just saw that EWD-123 describes the Banker's Algorithm as well.  -- [[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 03:49, 31 October 2017 (UTC)

== Code in Java ==

Here's a link from where you can refer the code for Banker's algorithm in java
[https://github.com/rajat81/SPOS/blob/master/Bankers/src/com/rajatswnt/Bankers.java]


== Algorithm ==

Safety Algorithm


1) Let Work and Finish be vectors of length ‘m’ and ‘n’ respectively.
Initialize: Work = Available
Finish[i] = false; for i=1, 2, 3, 4….n

2) Find an i such that both
a) Finish[i] = false
b) Needi <= Work
if no such i exists goto step (4)

3) Work = Work + Allocation[i]
Finish[i] = true
goto step (2)



 

4) if Finish [i] = true for all i
then the system is in a safe state
