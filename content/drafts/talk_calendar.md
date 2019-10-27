+++
title = "Talk:Calendar"
description = ""
date = 2017-03-16T04:15:14Z
aliases = []
[extra]
id = 9839
[taxonomies]
categories = []
tags = []
+++

==Switch-over Complexity==
Surely the date of switchover between Julian and Gregorian calendars is locale specific? For example, getting the rules right for Sweden is… “interesting” given that they tried to do the switch slowly, got distracted by a war, and then decided to switch the rest of the way at once. –[[User:Dkf|Donal Fellows]] 13:53, 31 May 2011 (UTC)
: Yes.  And this reminds me of [[Talk:Holidays_related_to_Easter|easter]]. --[[User:Rdm|Rdm]] 18:22, 31 May 2011 (UTC)
Maybe a the switch over date should be an argument, defaulting to the British date of switch?  I am also a bit curious as to what dating system the [[wp:International Astronomical Union|International Astronomical Union]] use.  Maybe there is standard for year/date definitions in [[wp:Common_Era|BCE and CE]]?
:: The IAU uses the Gregorian calendar proleptically even for dates long before its inception.  (Many of the dates they deal with are so far in the past that the difference between the introductions of the Julian and Gregorian calendars is a tiny fraction of time, relatively.)  They also generally don't use BCE dates, instead continuing the CE era backward into 0 and negative numbers (so 1 BCE is 0 CE, 2 BCE is -1 CE, etc.)  --~~

:Why would the British date be the default one? At least one device, the HP48 calculator, used the initial transition date, in 1582. That would be a rather logical default date. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 04:09, 16 March 2017 (UTC)

Or... Another task could be to calculate the locale date and time, based on latitude, longitude, (altitude?) AND date.  Ouch... to tough for me. 

[[User:NevilleDNZ|NevilleDNZ]] 22:45, 4 June 2011 (UTC)

:Conceptually, this would require a data structure which may be conceptualized as database table where the primary key was a polygon (a country's polygon) and a time interval (a time interval where that polygon defines that country's boundary).  The table would also have to have a bit which indicates whether that time interval and polygon corresponds to the Julian or Gregorian calendar.  All that you have to do to populate this table is find a list of all country boundaries as latitude/longitude polygons as well as how that has changed with each war or treaty settlement that has changed a country's boundary.  And note that the algorithm which uses this data structure might have to return multiple values (when a border was in dispute during a region of time where one side of the border was Gregorian and the other side was Julian).  An issue, of course, is that historical country boundaries might not always exactly correspond to anything we can currently unambiguously assign a latitude and longitude to (for example: when a river was the boundary -- rivers are wide and can move somewhat over the course of a century).  On the other hand, this data structure can be considerably simplified where both sides of a border were using the same calendar.  Nevertheless, I am not aware of any such list, and I think that this sounds like a huge historical research project. But the programming itself sounds relatively trivial (at least in comparison).  --[[User:Rdm|Rdm]] 18:44, 6 June 2011 (UTC)

== Improving the task description ==

The task description includes these statements:
:Test the calendar by generating a calendar for the year 1969 on a line printer of the time with a width of 132 characters.
:Ideally the program will generate well formatted calendars for any page width from 20 characters up.
:Kudos (κῦδος) for routines that also correctly transition from Julian to Gregorian calendar in September 1752.
While generating a particular year is fine, a Real Programmer would not have written their calendar generator to handle arbitrary widths of display. Instead, it would have either produced for 132-char wide line printers or 80-char wide terminals. In this day and age, nobody's got line printers any more so formatting for an 80-char terminal is what we must put up with. Given that, I propose that the task should state that the calendar should be tested by generating a calendar for the current year suitable for an 80-column device. Adaptation to other widths, other years and other languages are all to be extra-credit items. –[[User:Dkf|Donal Fellows]] 08:36, 4 June 2011 (UTC)
: Note also that the 132-column requirement would fit entirely within the scope of the dual [[CALENDAR]] task. :-) –[[User:Dkf|Donal Fellows]] 08:47, 4 June 2011 (UTC)

:: I disagree about a "Real Programmer" wouldn't have written code to handle arbitrary widths of display terminals.  I consider myself a real programmer, and had exposure to different terminals and/or printers.  Even if I didn't have that exposure, I still would've written the code to handle various sizes of display and/or different printer widths.  Almost all my programs have that type of logic from the get-go, so it makes it much easier to enhance the program when thinking of solving the task problematically. Also, whether line printers are used (printing a line at a time) or those that print a "page" at a time (my laser printer does a "line" at a time even though it may only print part of a line), the printed output still has "lines" (at least, for normal-sized text). Since I wrote the CALENDAR program from ground up, so to speak, the days on the calendar were thought of as a cell with "stuff" in it, such as the day-of-month of course, and optional day-of-year, optional moon phase, and optional highlighting of the "today"'s date, it was easy to code the program to adjust for different size cells, and things like spacing between months and other such niceties, including re-sizing of the cells.  [As an aside, the company that I worked for was small enough (about 30 programmers plus support staff), so everyone's birthday was also shown, letting people know that there is cake and ice cream in the breakroom --- but I digress.] Originally, it's primary purpose was to just show the current month's calendar when users logged on (but only when using a display terminal).  You couldn't believe the requests for modifications to add this & that to the program by the company's programmers.  The CALENDAR program is in actuality, just a part of my general-purpose "DATE" routine, with the CALendar option as, ... well, an option. 

:: I wish the original task had stated the obvious (but didn't) that instead of "a" calendar, it would've said a Gregorian calendar.  Almost everyone has shown a Gregorian calendar, except for those that mixed a Julian calendar with a Gregorian calendar [in a failed attempt to show the "missing days" when (whoever) switched from the Julian calendar to a Gregorian calendar].  There are no missing days.  When the Gregorian calendar was implemented (wherever the locality), it was designed as being proleptic.  That is, when the Gregorian calendar was adopted by a country, the previous day wasn't missing, it just was the previous day, way back to day one of year one (as if the Gregorian calendar had been in effect all the time). -- [[User:Gerard Schildberger|Gerard Schildberger]] 01:46, 14 April 2012 (UTC)

::: It was always the case that the previous day was just the previous day. Days are things that exist independent of calendars; dates and calendars are just a labeling of reality. The problems that occurred at calendar changes were virtually all actually to do with landlords (who else!) demanding a full month's rent despite the month in question being thoroughly foreshortened. ''Plus ça change…'' –[[User:Dkf|Donal Fellows]] 14:54, 14 April 2012 (UTC)

:::: I should've said what I meant (not what I wrote):  ... the previous calendar day ... --- We have the same problem (legally, rent-wise, but more importantly pay-wise).  When we lose (or gain) an hour whenever daylight savings time goes into effect (or drops), nobody is to be penalized because they didn't work their full eight hours, and if it appears that they worked nine hours (according to the wall clock), they get paid for that hour, even if not really worked.  There are a lot of contracts that specify so many hours of service (or whatever), and, in general, nobody is to be penalized because of a clock "bookkeeping" change. This was a big deal (a handful of decades ago) with the American unions (who, for the most part, deal with hourly waged workers). -- [[User:Gerard Schildberger|Gerard Schildberger]] 15:36, 14 April 2012 (UTC)  
 
Below is an extract from the newsgroup <tt>bit.listserv.ibm-main</tt> (unfortunately, I have lost the author and date it was posted)  which addresses the 10 or 11 missing days holy war:

```txt

Newsgroups: bit.listserv.ibm-main
Subject: Re: date formats
> Had I thought much about it, I would have hoped, devoutly, that the 'ten
> missing days' had been laid to rest along with discussiom of the Y2K
> problem.
>
> There are no missing days.  There are two calendars, the Julian one and the
> Gregorian one.
>
> The Julian one continued in use after the promulgation of the Gregorian
> calendar.  It indeed continues to be used today, chiefly by astronomers, who
> measure time in JDs (Julian Days), but also for such specialized purposes as
> calculating the date upon which the Orthodox Christian Churches celebrate
> Easter.
>
> At the point in time at which Pope Gregory XIII's new 'Gregorian' calendar
> came into use in Counter-Reformation Europe the difference between that
> day's Gregorian date and that day's Julian date was indeed ten days.  Later,
> it grew larger; and when the British and their then American colonies
> finally abandoned the Julian calendar in 1752 (Julian) for 1752 (Gregorian)
> this gap had widened to 13 days.
>
> To repeat myself now: there is no gap; there are two calendars, the
> difference between which is not constant because of different leap-year
> determination rules.
>
> Moreover, just as there are Julian dates for each day on and after the the
> Gregorian calendar came into use, there are Gregorian dates for each day
> before the Gregorian calendar came into use.  (Astronomers sometimes call
> such dates proleptic ones, but they also sometimes reserve the adjective
> 'proleptic' for dates before the epoch origin of a calendar.)

A good exposition of the changeover from Julian to Gregorian calendar
can be found at http://www.tondering.dk/claus/cal/calendar26.html

This shows the conversion datea for a large number of countries and
territories.

Conversions done in 1582 "lost" ten days, those done in 1752 "lost"
eleven days.

```

The  URL  still works (albeit it re-directs you to the current URL of:
: http://www.tondering.dk/claus/cal/calendar.html
Also, if a Julian calendar is going to be mixed with the showing of a Gregorian calendar, it should be so stated. The usual nomanclature is to mark the {Gregorian calendar] date with "OS" [old style]. Some of us older gizzers might remember some calendars that marked George Washington's (1st president of the USA) birthday as one date and another with OS — this was when it was a legal holiday in some states way back when (also, Thomas Jefferson) — this was all changed when congress created a "President's day") — but I digress once again. I was thinking of showing a Mayan calendar mixed with a Gregorian calendar just to show the obvious.  Also, if a Julian calendar is going to be shown, then I would like to see year 4 (as in 4 C.E., for you old gizzers: 4 A.D.) and see if February is a leap year or not.  It wasn't. – [[User:Gerard Schildberger|Gerard Schildberger]] 01:46, 14 April 2012 (UTC)

== Calendar versus CALENDAR ==

This task had the sentence: "For further Kudos see task [[Calendar - for "real" programmers|CALENDAR]], for printing on a 132 character width [[wp:IBM 1403|IBM 1403]] printer.  Where all code and output is to be in UPPERCASE."

This sentence was wrong, because CALENDAR only requires uppercase code, not uppercase output. Also, CALENDAR is identical to Calendar (except uppercase), so if Calendar requires 80x43 output, then CALENDAR also requires 80x43 output. Before 12 June 2011, Calendar specified "a width of 132 characters". At 12 June 2011, Calendar began to require 80x43 output. This new 80x43 requirement broke the relationship between Calendar and CALENDAR, squeezed Snoopy, and ignored the line-printer Snoopy calender that inspired the task. So, I am now changing Calendar to accept either 132-wide output or 80x43 output. --[[User:Kernigh|Kernigh]] 02:00, 20 August 2011 (UTC)
:All of this is really crazy. Tasks should be kept simple: what do Snoopy, uppercase and IBM 1403 do here? Nothing. It's all about printing a calendar, the rest is just some strange humor. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 04:14, 16 March 2017 (UTC)

== 31 March 1969 ==

Most programs say it was a Monday.

Run Basic shows it as Sunday ?!? --[[User:Walterpachl|Walterpachl]] 19:41, 21 August 2012 (UTC)
:Every day is one off in the Run Basic example. It looks like it has a mislabeled calendar. If it were marked as a "Monday first" calendar then it would be all right. --[[User:Mwn3d|Mwn3d]] 20:39, 21 August 2012 (UTC)

== REXX ==

Three questions:

Calput: seems to be missing

ly: should return 1 for leapyears??

Shouldn't be a note about which Calender is used (the proleptic Gregorian)

--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:19, 6 July 2013 (UTC)

There were several chunks of the original REXX code missing (not whole statements, but pieces of the interior of the REXX statements).

I re-pasted the whole of the REXX program. 

The   '''ly'''   function was one of those REXX statements (lines) that was somehow mangled, also the   '''calPut'''   function.


As for which calendar is being used, I assumed the calendar that (almost) everybody uses in everyday life, the (proleptic) Gregorian calendar.

I have yet to see a calendar in use that says "this is a Gregorian calendar). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:37, 6 July 2013 (UTC)

:: The task description has a line about this, I think:

:: "Kudos (κῦδος) for routines that also correctly transition from Julian to Gregorian calendar in September 1752. "

:: I haven't found a calendar yet that claimed to show this :-)
:: --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 03:58, 7 July 2013 (UTC)

:::KUDOS to Scala and TCL which show the Calendar of 1582 respecting the transition from Julian to Gregorian! --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 08:52, 8 July 2013 (UTC)

== 1752 ==

Wikipedia and Scala Version 2 show

```txt

      September              October       
Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa 
       1  2 14 15 16   1  2  3  4  5  6  7 
17 18 19 20 21 22 23   8  9 10 11 12 13 14 
24 25 26 27 28 29 30  15 16 17 18 19 20 21 
                      22 23 24 25 26 27 28 
                      29 30 31             

```

whereas Scala Version 1 shows

```txt

      September              October       
 Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su
                 1  2   1  2  3  4 15 16 17
  3  4  5  6  7  8  9  18 19 20 21 22 23 24
 10 11 12 13 14 15 16  25 26 27 28 29 30 31
 17 18 19 20 21 22 23                      
 24 25 26 27 28 29 30  

```

which is in my opinion correct.
Any opinions/explanations??
--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 15:06, 23 December 2014 (UTC)
:See [https://en.wikipedia.org/wiki/Gregorian_calendar Gregorian calendar] on Wikipedia. However, 1752 is not really correct: the switch from the Julian to the Gregorian calendar was initially done in october 1582. Some countries switched later. There is also a [https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar Proleptic Gregorian calendar], which "continues" the Gregorian calendar in both directions, past and future, without any reference to the Julian calendar. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 03:54, 16 March 2017 (UTC)
