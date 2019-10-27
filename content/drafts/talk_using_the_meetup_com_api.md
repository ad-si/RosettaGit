+++
title = "Talk:Using the Meetup.com API"
description = ""
date = 2014-12-30T16:33:46Z
aliases = []
[extra]
id = 18445
[taxonomies]
categories = []
tags = []
+++

== Output? ==

I think the task should specify an example set of parameters for the event query and ask for some sample output. E.g. a city and topic and the number of events and partial information for the first n events. &mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 00:30, 29 December 2014 (UTC)

== Submitting events ==

I think asking for submitting of events will be problematic.
Perhaps it would be better for the task author to create a single example/test RosettaCode "event"
and then have a ''bonus'' task requirement to use
[http://www.meetup.com/meetup_api/docs/2/event_comment/#create POST 2/event_comment]
to add a comment to it.
An example use of any POST API should be close enough to any other POST API for RosettaCode purposes.
&mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 00:41, 29 December 2014 (UTC)

: Note also the comment "Needs permission to post, which is paid." in http://rosettacode.org/mw/index.php?title=Using_the_Meetup.com_API&diff=next&oldid=195957 

: I think that any task which requires people pay money in order to exercise the code (another example might be credit card handling) should be treated as dubious. There's definitely a place for that kind of code, but I think that that place should be on a rigorously regulated paid access site. It's advertising for a commercial endeavor, which is fine as long as the commercial endeavor (a) contributes back, and (b) does so in an honest and clear fashion, and (c) does not whine and complain about other activities. I do not think that the person asking this task be posted here was doing the right thing.  --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 13:12, 29 December 2014 (UTC)

::Ick, I wasn't aware of that. IMO then this whole task should be replaced by a generic "use a REST API" task against some kind of test server. If no such freely available API for testing exists then either one could be part of a separate (more involved) task or there isn't much place for this on Rosettacode.org. &mdash;[[User:dchapes|dchapes]] ([[User talk:dchapes|talk]] | [[Special:Contributions/dchapes|contribs]]) 18:31, 29 December 2014 (UTC)
:::you are right, i didn't think about that. sure, i didn't expect anyone to go out and pay, it was more like about exploring how that particular API works. yes, we should find a better example. anyone have a good suggestion? i can only offer my own API (which is not documented, and rather obscure (it is about events though :-). it would probably better to pick something popular--[[User:EMBee|eMBee]] ([[User talk:EMBee|talk]]) 16:25, 30 December 2014 (UTC)


### Does cost Kill this task?

If, as Rdm has pointed out, "Posting requires a valid group which is a paid feature", and posting is an important part of the API, then the draft task should '''not''' be accepted as RC'ers don't have to pay external parties to complete tasks so far and I don't think that should change. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:28, 29 December 2014 (UTC)

== Uh... ==

This seems a bit iffy, in my opinion. The whole "talk to an API" thing seems marginal.

First, if the task itself is so complicated that the definition of what the task is doing has to be hosted elsewhere, that seems bad. Why isn't the API documentation included in the task description?

Second, what is an "event", in the context of this task? Why isn't that documented? 

Third, what is the success criteria, for talking to an external API like this? Is it acceptable for an implementation to be an utter failure? If so, what's the point? If not, how does a reader verify that the code is working?

Finally, [apparently, currently] none of the implementations are acceptable. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:05, 29 December 2014 (UTC)

== Changes == 

I have made changes to the task so as to make it more generic. The task now requires creating functions to make a post and get request to the meetup API. The task now no longer requires the paid events part to finish, it is merely optional now. Thank you for the idea, dchapes.

Rdm, the [Javascript Implementation http://rosettacode.org/wiki/Using_the_Meetup.com_API#JavaScript] is complete.

--[[User:Namanyayg|Namanyay Goel, designer and developer.]] ([[User talk:Namanyayg|talk]]) 16:33, 30 December 2014 (UTC)
