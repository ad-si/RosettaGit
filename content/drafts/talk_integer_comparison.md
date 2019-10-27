+++
title = "Talk:Integer comparison"
description = ""
date = 2019-10-01T07:19:02Z
aliases = []
[extra]
id = 2060
[taxonomies]
categories = []
tags = []
+++

== Pop11 & Standard ML examples ==

The Pop11 and Standard ML examples don't implement the task properly. Please note that the task explicitly contains "Test the condition for each case separately, so that all three comparison operators are used in the code." As is, those examples don't use the equality comparison operator. That is, they don't demonstrate how to compare for equality directly (in case those languages indeed only allow to test for integer equality by testing that one is neither larger nor smaller than the other &mdash; which I strongly doubt &mdash; it should be explicitly stated). --[[User:Ce|Ce]] 13:21, 11 May 2007 (EDT)


== Perl ==

Does the Perl example output the relation between the two numbers? I don't know Perl, but it doesn't look like it prints anything. --[[User:Mwn3d|Mwn3d]] 07:26, 25 April 2008 (MDT)
: An old comment, I know. But yes, the Perl function needs to be wrapped in code to get user input and display the result. Fixed. --[[User:Snoman|Snoman]] 11:06, 12 July 2010 (UTC)


== when would two *integers* fail to compare? ==

You might as well ask what is the point of the task?  If the point is to exercise the three comparison operators and observe that only one of them evaluates to true, then all of the posted solutions that use else if, switch, cond, or similar short circuit forms are incorrect.  The reference solutions originally posted by the task author did exercise all three operators for any valid input without short circuiting.  My Go solution was anomalously sophisticated, I know.  It not only exercised all three operators, but went on to programmatically validate that exactly one operator returned true and that the other two returned false.  &mdash;[[User:Sonia|Sonia]] 04:09, 28 June 2011 (UTC)
:Task says "test all three separately", not "use all three on all inputs".  All three are used in the code right now, which fills the bill.  If it were short-circuiting, the third test doesn't even need to be done.  Even if you do leave all redundant tests in, there's no garantee an optimizer will keep them there anyway.  --[[User:Ledrug|Ledrug]] 04:17, 28 June 2011 (UTC)
