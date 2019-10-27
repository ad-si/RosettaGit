+++
title = "Talk:Holidays related to Easter"
description = ""
date = 2012-03-10T22:10:20Z
aliases = []
[extra]
id = 7627
[taxonomies]
categories = []
tags = []
+++

==Whose Easter?==
Whose Easter are we calculating here?  Or, more specifically, which calendars are we using?  (And, when do we switch from Gregorian to Julian?)  According to Wikipedia, depending on the answers to these questions we can have easter in March, April or May.  The sample output from the Algol 68 program makes me think that we are supposed to calculate what wikipedia calls the "Western Christianity"'s Easter, but that was just one of the issues which cause different people to assign different dates to easter in the same year.  I think a full specification would be appropriate here.  (Easter is apparently the first sunday after the 14th of the month where it occurs, but I am confused about how you unambiguously determine which month it occurs in.  But I am also having problems reconciling this "first sunday after the 14th of the month" rule with easters which occur in the first week of April.) --[[User:Rdm|Rdm]] 14:40, 29 June 2010 (UTC)

: It's the first Sunday after the 14th day of the ''lunar'' month. That is, the first Sunday after the full moon, specifically, the full moon after March 21st. --[[User:Markjreed|Markjreed]] 04:22, 4 June 2011 (UTC)

: It's horribly complicated as it's ''usually'' the Sunday of the weekend after Passover, but is calculated using different rules to those specified in the Torah. And it's been the subject of a number of major fights in the Christian church (e.g., the Council of Nicaea). So I searched around and ended up using the code at the end of http://www.assa.org.au/edm.html (which I ''think'' is based on doing the switch to Gregorian rules using the British date; you should be able to confirm from the tables higher up that page) as that at least gives the same days/months as that deeply impenetrable ALGOL code for the dates in the exact challenge. –[[User:Dkf|Donal Fellows]] 10:34, 30 June 2010 (UTC)
:: http://www.assa.org.au/edm.html describes several different (and apparently conflicting) methods for determining when easter is.  Although the page itself claims to describe how to do this for dates from 326 onward, the code at the bottom of the page is claimed to be accurate for years 1583..4099.  (Many -- but not all --european countries [[wp:Gregorian calendar#Adoption|switched]] from the julian calendar to the gregorian calendar between easter in 1582 and easter in 1583.)  Given the historical issues here, I think the task should explicitly specify how they are handled (or, perhaps, specify a date range which excludes the conflicts).  As it stands now, implementations which do not specify multiple easters in some years necessarily fail to recognize some easters and this is complicated by the fact that the same "date" in different calendar systems will necessarily represent different physical dates.  --[[User:Rdm|Rdm]] 18:34, 30 June 2010 (UTC)


::: I just changed ''brief'' operators to '''bold''' operator for readability.  I hope it helps. 
::: Not sure what to do about dates before Gregorian.  Esp [[wp:Calendar (New Style) Act 1750|Sep 1752]].  We might standardise on [[wp:Common Era|Common Era (CE)]] for the program, however even CE is vague about whether either early [[wp:Gregorian calendar|Gregorian calendar]] Or [[wp:Julian calendar|Julian calendar]] dates should be used.
::: [OT: Time.... to be the [[wp:Time Lord|Time Lord]] and just play with time...  I heard [an urban myth(?)] that [[wp:Julius Caesar|Julius Caesar]] changed the first month of the year from March to the next January so as to increase his term in the Senate.  Hence to this day Sep/Oct/Nov/December are no longer the 7th/8th/9th/10th months.  It seems when you are the [[wp:Roman consul|Consul]]/[[wp:Roman dictator|Dictator]] of the [[wp:Roman Republic|Roman Republic]] you then have enough authority to bend time!  EYHO Albert!  :-) ]
::: [[User:NevilleDNZ|NevilleDNZ]] 00:33, 1 July 2010 (UTC)

:::: You're right about that being an urban myth, FYI. The numbering of the months dates from an older Roman calendar, older even than the one Julius replaced.  The short-changing of February is just as old; the story of Julius and Augustus stealing days from it to make their months 31 days each is also a myth. --[[User:Markjreed|Markjreed]] 04:37, 4 June 2011 (UTC)

:::: For example: http://www.merlyn.demon.co.uk/estrdate.htm#MEDC -- for the year 2011, easter falls on April 11 and April 24 where in the year 2000, it fell on April 17th, April 23 and April 30.  And things are much simpler now than they were in the past...  --[[User:Rdm|Rdm]] 02:20, 5 July 2010 (UTC)
::: Well, all that I can really say for sure is that the code is probably wrong, but both the Algol and Tcl examples are wrong in the same way. (I know they're wrong because when I put in the day of the week, things are all over the place before the switch to the Gregorian calendar; that's an area where I'm pretty sure that the Tcl time formatting engine is correct; the guy who wrote the modern implementation is a ''serious'' time geek.) Given the massive uncertainty over calendars, I suggest not worrying about it. As noted, it's been a point of contention for the church for a long time even without programmers involved. –[[User:Dkf|Donal Fellows]] 09:33, 5 July 2010 (UTC)


:: Not only is it horribly complicated (or rather not clear), arguments over how to do it split the church (the excommunication of the Quartodecimans which sounds like something out of Ghostbusters) and almost started conflicts.  See [http://mangsbatpage.433rd.com/2008/03/calculating-easter.html] --[[User:Dgamey|Dgamey]] 22:27, 8 July 2010 (UTC)

::: To be fair, Easter calculation is a bit complex, but not "horribly" so, nor is it all that controversial at this point.  Most of the complexity in descriptions is left over from the traditional, pre-computer methods used to calculate the date; in straightforward code, it's not that bad.  As for the controversy, that was mostly settled in 325 AD.  There is one remaining schism - between the Western and Eastern Orthodox congregations - and even they agree on the basic rule: Easter is the first Sunday after the first (approximated) full moon on or after March 21st.  They only disagree in the choice of calendar, which leads to two discrepancies: the two calendars call different days "March 21st" (for years between 1900 and 2099, Eastern "March 21st" is the same day as Western "April 3rd"), and have slightly different rules for approximating the moon phases, so the calculated "full moon" falls on different dates between them. In 2012, Western churches will observe Easter Sunday on April 8th, but the Julian calculation places it on April 2nd, and because of the 13-day difference between the calendars, the actual date of Orthodox Easter is April 15th. --[[User:Markjreed|Markjreed]] 04:22, 4 June 2011 (UTC)


I was thinking of adding a Python calculator for future easters that relied on the random module :-)
--[[User:Paddy3118|Paddy3118]] 13:07, 5 July 2010 (UTC)

==Suggestion==
Take away the uncertainty. Find a routine that is easy to follow and has little controversy in calculating Easter for a small period of time around ''now''. Quote the routine in the task description, and have the task be to calculate a less controversial number of Easters around now. You could point to external arguments about dates of Easter for the interested, but meaanwhile you would get a task that aids programming [[wp:chrestomathy|chrestomathy]].
:Good idea, especially if you specify the Gregorian Calendar and Catholic rules (i.e., no ambiguity from calendars or religious denominations). –[[User:Dkf|Donal Fellows]] 22:30, 8 July 2010 (UTC)
::All of the easter holiday rules are catholic rules.  But some of the dates required by the current task are dates when older versions of the rules were the "officially accepted versions".  I would recommend, in addition to specifying the current catholic rules and the gregorian calendar, that we also change the task's example to use years from 1990 through 2020.  --[[User:Rdm|Rdm]] 17:00, 9 July 2010 (UTC)

To take away the uncertainty, you only have to specify which set of rules.  As I said above, different churches use different rules (Gregorian vs Julian), so you have to specify at least which of those you want.  But having specified Gregorian, there's no need to limit it to such a narrow range of years; the Gregorian rules are valid in the Western Church as far back as the Gregorian calendar has been in use, which is of course locale-dependent, but goes back at least as far as 1929 everywhere (see [[wp:Gregorian Calendar#Adoption]]), and those rules are valid for the foreseeable future as well (though there have been proposals to abandon them in favor of a wholly astronomical calculation.) --[[User:Markjreed|Markjreed]] 03:58, 4 June 2011 (UTC)

== Inconsistency for negative years ==

There is an inconsistency in the results of the solutions on this page for negative years. (Yes, I know that Easter didn't exist back then, but this is just from an algorithmic perspective.) For example, for the year -1 (i.e. 2 B.C.), the C code produces an Easter date of March 27, while the Ruby code produces April 18. I believe this is due to the different behavior of the modulo operator for negative numbers. --[[User:Spoon!|Spoon!]] 23:43, 18 December 2011 (UTC)
