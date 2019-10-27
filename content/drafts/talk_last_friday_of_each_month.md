+++
title = "Talk:Last Friday of each month"
description = ""
date = 2016-07-15T17:28:27Z
aliases = []
[extra]
id = 10820
[taxonomies]
categories = []
tags = []
+++

== Java ==

The Java example gives me this result (the problem is at the last line):
 java LastFridays 2012
 2012 Jan 27
 2012 Feb 24
 2012 Mar 30
 2012 Apr 27
 2012 May 25
 2012 Jun 29
 2012 Jul 27
 2012 Aug 31
 2012 Sep 28
 2012 Oct 26
 2012 Nov 30
 2012 Dec 28
 2012  25
[[User:Blue Prawn|Blue Prawn]] 07:30, 31 December 2012 (UTC)

: I've had the same problem. getShortMonths() returns 13 elements. I assume this is a bug in certain Java versions. [[User:Fwend|Fwend]] 12:30, 31 December 2012 (UTC)
::Yeah it's happening for me now too (on the Windows and Linux implementations). It must have been changed in a recent update. I hope I would have noticed that when I wrote the example. Do you think we need to fix the example around that? --[[User:Mwn3d|Mwn3d]] 14:11, 31 December 2012 (UTC)
::: When I posted the code, I thought it was a bug in the Java implementation, but according to the documentation it isn't: ''Short month strings. For example: "Jan", "Feb", etc. An array of 13 strings (some calendars have 13 months)...''. [http://www.docjar.com/docs/api/java/text/DateFormatSymbols.html]. I'll fix the example. [[User:Fwend|Fwend]] 14:26, 31 December 2012 (UTC)

== command line ==
Why does the task specify that the year is supplied from the command line?  This seems to conflate two totally separate language capabilities--date arithmetic and command line handling.  For the PHP example, I'm just ignoring the command line requirement.  --[[User:Showell|Showell]] 17:37, 20 January 2012 (UTC)

: Specifying the year on the command-line makes it easy to check any specified year (or multiple years) without having to change a hard-coded year inside the computer program.   This should be a trivial requirement for most languages.   It also makes it easier to compare how a particular language handles command-line arguments.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:28, 15 July 2016 (UTC)

== task name ==
of year? not of month?--[[User:EMBee|eMBee]] 13:43, 7 November 2011 (UTC)
:It looks like it's all of the last fridays of the months of a given year? --[[User:Rdm|Rdm]] 14:52, 7 November 2011 (UTC)
:: yes, exactly, which is why i find the current title confusing. something like ''Last Friday of every month'' would be more to the point. otherwise, i thought the original title was sufficient. something about fridays. it is clear enough that this is about the calendar. is more details really needed?--[[User:EMBee|eMBee]] 14:59, 7 November 2011 (UTC)
::: English is not my born language, but I also find the title "Last Fridays of year" confusing. I understand it as 'really' the last fridays of a given year, for example the last three Fridays of 2012 are 2012-12-14, 2012-12-21 and 2012-12-28. [[User:Blue Prawn|Blue Prawn]] 21:10, 9 November 2011 (UTC)
:::: exactly my thoughts too. so what's a better title? "last friday of each month?" (i thin kthe fact that it is for a year is less interesting, the same solution would work for any period of time).--[[User:EMBee|eMBee]] 04:11, 10 November 2011 (UTC)
::::: I'm not particularly attached to the current name; I just thought that the original (“Last Fridays”) wasn't good enough either. Suggested changes are welcome. –[[User:Dkf|Donal Fellows]] 08:49, 10 November 2011 (UTC)

== C version ==
<blockquote>''This discussion is about the program shown here:''

```c
#define _XOPEN_SOURCE
#include <stdio.h>
#include <time.h>

int main(int c, char *v[])
{
	int days[] = {31,29,31,30,31,30,31,31,30,31,30,31};
	int m, y, w;
	struct tm tm;
	char buf[32];

	if (c < 2 || !sscanf(v[1], "%d", &y)) return 1;

	days[1] -= y % 4 || (y % 100 && ! (y % 400));
	sprintf(buf, "%d-1-1", y);
	strptime(buf, "%Y-%m-%d", &tm);
	w = tm.tm_wday - 1; /* day of week for zeroth of Jan */

	for(m = 0; m < 12; m++) {
		w = (w + days[m]) % 7;
		printf("%d-%02d-%d\n", y, m + 1,
			days[m] + (w < 5 ? -2 : 5) - w);
	}

	return 0;
}
```
</blockquote>

Building the C version under cygwin (gcc 3.4.4), I get:


```bash
$ make last_fridays
cc     last_fridays.c   -o last_fridays

$ ./last_fridays 2011
2011-01-27
2011-02-24
2011-03-31
2011-04-28
2011-05-26
2011-06-30
2011-07-28
2011-08-25
2011-09-29
2011-10-27
2011-11-24
2011-12-29

$ ./last_fridays 2012
2012-01-27
2012-02-24
2012-03-30
2012-04-27
2012-05-25
2012-06-29
2012-07-27
2012-08-31
2012-09-28
2012-10-26
2012-11-30
2012-12-28

$ cc --version
cc (GCC) 3.4.4 (cygming special, gdc 0.12, using dmd 0.125)
Copyright (C) 2004 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```


The C entry currently shows different results for 2011, but I cannot reproduce them. --[[User:Rdm|Rdm]] 23:25, 8 November 2011 (UTC)
: I'll assume it's a [http://www.cygwin.com/ml/newlib/2011/msg00180.html cygwin bug] for now.  For the record, can you change two lines to
:<lang>sprintf(buf, "%d-01-01 09:01:01", y);
strptime(buf, "%Y-%m-%d %H:%M:%S", &tm);
```
 and see how it does? --unsigned
:: I replaced the sprintf an strptime lines with these two lines and got the same result.  Note also that might results are consistent (but this does not prove that the issue is not uninitialized memory). --[[User:Rdm|Rdm]] 11:17, 9 November 2011 (UTC)

:: I have a similar problem with [[OpenBSD]] 4.9. The output changes from run to run, and might show the last Mondays, Thursdays, Fridays or Saturdays. After the call to strptime(), <code>tm.tm_wday</code> contains junk values like 50237728 or 239567136. (Legal values are 0 to 6.) Change to "%d-01-01 09:01:01" and "%Y-%m-%d %H:%M:%S" is no help. Checking my manuals, [http://www.openbsd.org/cgi-bin/man.cgi?query=mktime&sektion=3&arch=amd64&apropos=0&manpath=OpenBSD+4.9 mktime(3)] describes the fields of ''struct tm'', but [http://www.openbsd.org/cgi-bin/man.cgi?query=strptime&apropos=0&sektion=0&manpath=OpenBSD+4.9&arch=amd64&format=html strptime(3)] is vague; I assume that strptime() never fills ''tm_wday'' unless the string contains the weekday. --[[User:Kernigh|Kernigh]] 01:05, 9 November 2011 (UTC)

== 1582 ==

4 Oct 1582 is followed by 15 Oct 1582 in the Gregorian Calendar.
Java seems to take care of that. Rexx results differ:

```txt

25.09.1582
26.09.1582
27.09.1582
28.09.1582 Fri
29.09.1582
30.09.1582
01.10.1582
02.10.1582
03.10.1582
04.10.1582
15.10.1582 Fri
16.10.1582
17.10.1582
18.10.1582
19.10.1582
20.10.1582
21.10.1582
22.10.1582 Fri
23.10.1582
24.10.1582
25.10.1582
26.10.1582
27.10.1582
28.10.1582
29.10.1582 Fri
30.10.1582
31.10.1582
Java       Rexx
Jän 26     Friday 29 Jan 1582
Feb 23     Friday 26 Feb 1582
Mär 30     Friday 26 Mar 1582
Apr 27     Friday 30 Apr 1582
Mai 25     Friday 28 May 1582
Jun 29     Friday 25 Jun 1582
Jul 27     Friday 30 Jul 1582
Aug 31     Friday 27 Aug 1582
Sep 28     Friday 24 Sep 1582  <--- different
Okt 29     Friday 29 Oct 1582
Nov 26     Friday 26 Nov 1582
Dez 31     Friday 31 Dec 1582

```

--[[User:Walterpachl|Walterpachl]] 06:05, 11 August 2012 (UTC)

-----

No, there are no missing days in the Gregorian calendar.  

October 4th, 1582, in the Gregorian calendar is followed by October 5th in the Gregorian Calendar.  

October 4th, 1582, in the Julian calendar is followed by October 15th in the Gregorian calendar.  

October 4th, 1582, in the Julian calendar is followed by October 5th in the Julian calendar for those countries (and places) that didn't convert to the Gregorian calendar.

October 4th, 1582, in the Julian calendar is followed by October 5th in the Julian calendar, even though it wasn't the "legal" (in use) calendar anymore.

It's like when someone leaves San Francisco, California and flies to New York, and adjusts (or adopts) their wristwatch to local time by jumping three hours ahead.  There's no missing three hours.  The traveler just adopted the "time" of a different time zone.    

See:  http://rosettacode.org/wiki/Talk:Calendar 

The only "missing days" are when one switches from one calendar to another.  The Gregorian calendar is proleptic.  October 5th follows 4 Oct in EVERY year in the Gregorian calendar (starting in January 1, year 1). -- [[User:Gerard Schildberger|Gerard Schildberger]] 06:18, 11 August 2012 (UTC)

October 4th in year 1582 was the date in the Julian calendar (the day before the switchover in some countries).  The next day, the Gregorian calendar was adopted (or put into effect) in various countries, but not everywhere.  The new Gregorian calendar says it was October 15th.  If we switched to a Mayan calendar, would we be missing ''years'' (or whatever)?  Of course not.  Once the Gregorian calendar was adopted, it was like the calendar was in effect all along, with a continous calendar (proleptic) backward to January 1st, year 1.  No missing days.  That is why there are references to ''O.S.'' (''o''ld ''s''tyle) for years previous to the adoption of the new Gregorian calendar for those people who were born before the switchover.  If the Gregorian calendar wasn't proleptic, there would be no need for ''O.S.'' type of dates. 

See: http://en.wikipedia.org/wiki/George_Washington 

-- [[User:Gerard Schildberger|Gerard Schildberger]] 07:02, 11 August 2012 (UTC)

:One learns something every other day. Thanks!
:Does that mean that Java is wrong here?--[[User:Walterpachl|Walterpachl]] 17:16, 11 August 2012 (UTC)
:::proletic should be proleptic?!? --[[User:Walterpachl|Walterpachl]] 17:34, 11 August 2012 (UTC)

::::Yes, three misspellings of proleptic corrected. -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:45, 11 August 2012 (UTC)

:: I don't know what the Java language says (as per its documentation) concerning ''what'' it returns from its date/time functions;  does it reportedly return a Gregorian date, or does it switch to a Julian date at a certain point (date), and if so, why assume ''any'' date as the chosen switchover date would be, more-or-less, capricious as different countries adopted the Gregorian calendar at different times.  But if it's for a Gregorian calendar, then yes, it's wrong.  There's a lot (time, ego, emotions, reputations, ...) invested in the existing Java code (and others), and the resistance in changing/correcting it will be great. -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:45, 11 August 2012 (UTC)

:::I think this line from the documentation about the Object that the Java code is using is what you're looking for:
 GregorianCalendar is a hybrid calendar that supports both the Julian and Gregorian calendar systems with the support of a single discontinuity, which corresponds by default to the Gregorian date when the Gregorian calendar was instituted (October 15, 1582 in some countries, later in others).

:::I think that means it's Julian before 10/15/1582 and Gregorian thereafter (and you can set the cutover date if you want). Honestly I have no idea what's going on with any of this. I think it's kind of a waste since the calendar change was so long ago. Seems sort of impractical to worry about it now. --[[User:Mwn3d|Mwn3d]] 17:57, 11 August 2012 (UTC)

:::: The Gregorian Calendar isn't a hyrid calendar --- which is contrary to (I assume) some Java document --- one should refer to the (I can't believe I'm saying this) Vatican papers about it's description, rules, and implementation).  Failing that, one could just check the bureau of weights and measurements, division of measurement standards, or whatever ... for the country you're in. It is proleptic (both in its inception and use) and there is no discontinuity.  [Nothing is really simple, the more you know about something, the more detail there is that you don't want to necessarily know or care about].  The only point of interest is when states/countries switched over from the (whatever) older calendar(s) [in most cases, a Julian calendar] to the Gregorian calendar, and that switchover caused "missing days" when the new calendar was adopted and the old calendar was dropped.  The pratical side to this is when referring to specific dates, such as (USA president) George Washington's birthday (some of old fogies still remember when GW's birthday was celebrated on the ''O.S.'' birthdate ... and then later came President's Day.  The practical side of the changeover didn't really affect many people (or contracts), there was no driver's licenses to worry about, no retirements kicking in (Social Security was a long, long way off), I suppose there were a handful of people who suddenly become over the age of consent for marriage, property ownership, inheritance, impressment, and other ... stuff, the most important was the ability to buy beer, of course, of course.  The adoption of a new/different calendar (with different leapyear rules, and now, leapsecond rules) isn't a simple thing, otherwise it wouldn't have been resisted for so long by many states/countries, and, not surprising, politics/religion/posturing played a big role in this. We still squabble (a very polite term) over daylight savings times, and you won't believe the (USA) laws around the use (or not) of it --- on how they affect labor (time worked vs. time passed), 24/7 type of contracts, end-of-use clauses, a whole host of incidentals. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:10, 11 August 2012 (UTC)

:::::Ignoring pretty much all of that....do you have a problem with the Java code? What do you want to know about it? I don't want to know anything about the calendar systems yet. I'm just trying to figure out if you guys think the program is wrong or not. --[[User:Mwn3d|Mwn3d]] 20:30, 11 August 2012 (UTC)

::::::I noticed that 2 programs produce different results and brought that up
::::::Thanks to GS for his elaborations!
::::::I couldn't care less about Java's calendar apart from watching out 
::::::for NetRexx' solution:-)--[[User:Walterpachl|Walterpachl]] 07:52, 13 August 2012 (UTC)  
 
I don't have a dog in that fight (as pertaining to Java code and how it interprets/presents/discombobulates/converts Gregorian and Julian dates). But, as far as I can interpret from the quoted text from the documentation about the Object that the Java code is using, it states that the Gregorian calendar is a hybrid calendar.  It isn't. (Could it mean the ''way'' Java treats dates is a hybrid system?)  Possibly, the Java code treats dates after a "switchover" (there were many switchovers, depending on the state/country) as Gregorian, and Julian before that.

If the Java code supports Julian '''and''' Gregorian as an option (as separate function/subroutine options), that would seem to be OK. But the quoted statement says it supports a single discontinuity with an assumed date of when the Gregorian calendar was adopted by most of the (Holy Roman) Catholic world back in 1582, then it appears that the Java code writer(s) think that the Gregorian calendar isn't proleptic [proleptic essentially means that it extends backwards in time with no discontinuity back to January 1st, year 1], and the Java code then (it appears to me) switches back to the Julian year method. 

If that is true, then the Java code would have to use the Julian leapyear rules, and also that ''some'' Julian new years start on March 25th (Lady Day) [in ''some'' countries], not January 1st. To check this, see what year George Washington (USA president) was born in the O.S.); if Java says 1731, then that's the correct year for the Old Style (Julian) format. 

Note that according to the Gregorian calendar, George Washington was born in the year 1732. 

Also note that Russia's October Revolution (being Eastern Orthodox Catholic, didn't adopt the Gregorian calendar until 1924) actually happened in November (in the Gregorian calendar). I just love trivia.

This subject is, for the most part, pretty much out of my league, and needs a scholar's attention. I ain't that. I hoped I paraphrased and stated accurately some of the differences between Gregorian and Julian calendars (N.S. vs. O.S.). 

Most people think that the leap year rule for the Julian calendar was simple: years divisible by 4 are leap years. Not so. Julian year 4 wasn't a leap year (see if the Java code supports that). Some scholars think that Julian year 8 wasn't a leap year as well, but I can't find that reference on the "google-net" anymore. 

If we don't read and understand history, we're doomed to repeat it's mistakes. -- [[User:Gerard Schildberger|Gerard Schildberger]] 19:27, 12 August 2012 (UTC)

-----

:While I found this discussion of 1582 most enlightening I am thrilled by the 'real' calendar of 1582 shown under Calendar for Scala.
:( http://rosettacode.org/mw/index.php?title=Calendar&action=edit&section=17 )
:I wonder if such a calendar page was ever printed.

```txt

Snippet from there:
1582
     September              October       
Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
                1  2   1  2  3  4 15 16 17
 3  4  5  6  7  8  9  18 19 20 21 22 23 24
10 11 12 13 14 15 16  25 26 27 28 29 30 31
17 18 19 20 21 22 23                      
24 25 26 27 28 29 30                      

```


:--[[User:Walterpachl|Walterpachl]] 05:12, 22 August 2012 (UTC)

No, I don't think so.  The October "month" shown above is a mixture of two types of calendars: 

Julian (O.S.) and Gregorian (N.S.);  furthermore, neither the old or new style is indicated. 

The Julian calendar continues after any certain date, and is still used today for various purposes.

The Gregorian calendar is proleptic (so there're no "missing" days in it, no matter when it was adopted).

:Nevertheless, the Gregorian calendar is discontinuous.

:First, it's discontinuous based on where the results of the program are being understood.   Second, it's discontinuous because it was not used before certain dates (which depend on location).

:: No, both calendars are continuous.  Just because one was adopted at a certain date, doesn't mean the calendar is discontinous. Both calendars have current day usage, and the Gregorian calendar is proleptic, that is, dates previous to its ''inception'' are "present" down to January 1st, year 1 --- no matter when that calendar was adopted. The only discontinuity is when showing a specific calendar, and then a different calendar, during the switchover (when one calendar was "dropped", another adopted for common use). 
The problem would be solved if, when showing that "split" calendar, which dates are Julian, which dates are Gregorian. -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:55, 22 August 2012 (UTC)
-- [[User:Gerard Schildberger|Gerard Schildberger]] 18:55, 22 August 2012 (UTC)

::: Except, it's not really "two calendars".  It's "two classes of calendar standards".  Another way of looking at this is that each country has a calendar and at some point they changed standards.  So, if you look at this geographically for any single day during the transition periods (or, even now), you have discontinuities because of partial adoption of the Gregorian calendar.  Alternatively, if you look at this as a stretch of time at one specific location you have discontinuity because at one point in time the Julian standard was used and at a later time the Gregorian standard was used (there's an exception here, for institutions which never adopted the Gregorian standard -- and of course that means that the calendar has drifted from having much of anything to do with the usual meaning in the context of seasons). 

::: Your point, I think, is that you can take the Gregorian calendar and project it backwards -- assigning dates to events which took place before the calendar was invented.  And, from this point of view, the dating system is self consistent.  But it achieves this by assigning dates to events that have nothing to do with any dates which would have been expressed at the time represented by those dates.  And, yes, this point of view is internally self-consistent.  But that internal self consistency does not mean that people talking about historical calendar gaps are incorrect.  It's more like nit-picking.  --[[User:Rdm|Rdm]] 14:34, 23 August 2012 (UTC)

:It's true that some of these discontinuities can be interpreted as being an artifact of the transition from Julian calendar to Gregorian calendar, but that doesn't eliminate the discontinuities, it only labels them.  (And, also, we can legitimately say that all days before its adoption are "missing" from it.)
:Also, "where the results of the program are being understood" can be awkward to implement.  --[[User:Rdm|Rdm]] 18:26, 22 August 2012 (UTC)

:: Again, there are no missing days, no discontinuites.  It only appears missing (or discontinuous) because a calendar is shown in one format (O.S. or old style or Julian) and then another format (N.S. or new style or Gregorian) during the switchover.  If we switched over to a Mayan calendar, there won't be 12,000 missing years (or whatever). Both (er, all three) calendars are continuous (up to the present day). I don't want to go into the exception of the Mayan calendar when it "starts over" later this year (2012). -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:55, 22 August 2012 (UTC)

:::I still like the calendar page shown which is about the same as the full 31 October days with 5 to 14 October rossed out with a felt pen which wasn't available then. Neither did they know the word 'proleptic'. However, it's a pity that they (well, Gregor) dropped my birthday :-( --[[User:Walterpachl|Walterpachl]] 19:21, 22 August 2012 (UTC)

:::: Felt pen was, of course, no good because days of the week would have been wrong. Better: Snowpake and correct the numbers on the days! --[[User:Walterpachl|Walterpachl]] 09:26, 23 August 2012 (UTC)

:::: You were born in 1752?  Sheesh, you're almost as old as dirt.  Aside from that, Pope Gregory XIII (and his [or a] scientific advisor and primary author of the reformed calendar: Aloysius Lilius, AKA: Luigi Lilio, or Liuigi Giglio, or Aluise Baldassar Lilio) didn't know about the word ''proleptic'', but it was proclaimed (via a ''papal bull'') that the reformed calendar was as if it was in effect (back) to January 1st, year 1. Note that the reformed calendar (later called the Gregorian calendar) was made effective the 24th of February in 1582 for the Catholic clergy, and it furthermore exhorted Catholic sovereigns to adopt the reformed calendar. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:29, 22 August 2012 (UTC)

-----
::::: So this calender page was printed in February and everyone knew that there would not be these days. Fine with me. What's your point in misspelling ''proleptic''? --[[User:Walterpachl|Walterpachl]] 06:25, 23 August 2012 (UTC) And is it correct that 5 October 1582 was a Friday?

:::::: I never said that any calendar was printed (in February or at any time).  Not ''everyone'' knew; the papal bull was issued primarily to the (Catholic) clergy (the bishops).  At the time of the issuance of the papal bull, it wasn't known when the reformed calendar would be adopted (or even ''if'' it would be adopted) by the various states/countries/principalities.  Because of the origin of the reformed calendar, there was great resistance to the new calendar.  Certainly the Catholic church knew that when the reformed calender would be adopted (the switchover), there would be a gap between the Julian calender date (O.S.) and the reformed calendar date (N.S.), later called the Gregorian calendar.  The Catholic Church didn't observe the gap between the two calendars (in October) since they were already using the reformed calendar much earlier that year.  Actually, according to the Julian calender, it was the "previous year" (due to the start of the new year in March 25th).  You can see a good explanation of this at the Wikipedia page of George Washington [http://en.wikipedia.org/wiki/George_Washington], read '''Note 1'''.  Note that George Washington was born February 22nd, 1732 (N.S.), which was February 11th, 1731 (O.S.).  The year difference has caused so many incorrect "corrections" that the Wikipedia page has to be locked (put into semi-protected mode which can only be edited by established registered users) so misinformed people wouldn't "correct" the O.S. year. Also, read (view the source): [http://en.wikipedia.org/w/index.php?title=George_Washington&action=edit], particurily the extra-large bold print. -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:22, 23 August 2012 (UTC)

:::::: As for your question about misspelling a word ... there was no point, it was a typo. -- [[User:Gerard Schildberger|Gerard Schildberger]] 18:22, 23 August 2012 (UTC)

-----
It is really amazing how much misinformation (that's a code word for bad or incorrect information) concerning the subject of the Gregorian calendar, the various switchovers (adoptions), and the subject of the translations of the two styles of dates ('''O.S.''' vs. '''N.S.''').  One website states that Pope Gregory ''ordered'' ten days to be dropped ... and whatnot.  As for the Wikipedia's article, it is suprisingly factual and well-formed.  Kudos to those authors. -- [[User:Gerard Schildberger|Gerard Schildberger]] 20:36, 22 August 2012 (UTC)


-----
May I recommen a fantastic url showing all those things brilliantly?
 http://emr.cs.iit.edu/home/reingold/calendar-book/Calendrica.html
It's an offspring of a book I own: 
Calendrical Calculations by Edward M. Reingold and Nachum Dershowitz

--[[User:Walterpachl|Walterpachl]] 11:49, 29 August 2012 (UTC)

== Proposed amendment & rename: [[Find a first or last day of week in a month]] ==

Wots say we rename [[Last Friday of each month]] to [[Find a first or last day of week in a month]](?)...
Then use Tuesday and Thursday January 2038 (Vis-à-vis [wp:Year 2038 problem]) as test cases...

e.g.

```algol68
PROC find day of week in month year = (INT wday, month, year)INT: (
   ¢ Where wday=1,2,3,4... is first Mon,Tue,Wed... ¢
   ¢ Where wday=-1,-2,-3,... is last Mon,Tue,Wed... ¢
   code...
   ¢ found ¢ day of month EXIT
);
```


Actually: I'm curious... is this first/last day of week issue specifically addressed in any library, or "[[wp:ICalendar#Events_.28VEVENT.29|icalendar]]" standard?

On another topic: We have "Last Friday" snacks every month, and need to order the food 2 days before.  AFAIK no calendar program out there automatically caters for a recurrent order like this.  (I've failed in google calendar and cron)  Any hints.

[[User:NevilleDNZ|NevilleDNZ]] ([[User talk:NevilleDNZ|talk]]) 05:53, 17 April 2013 (UTC)
