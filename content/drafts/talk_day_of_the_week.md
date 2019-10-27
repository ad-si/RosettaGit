+++
title = "Talk:Day of the week"
description = ""
date = 2012-09-22T12:54:31Z
aliases = []
[extra]
id = 3252
[taxonomies]
categories = []
tags = []
+++

==Same prog on different machine==

Yes, and I see it is a 64bit machine, isn't it? I am still on plain 32 bit! --[[User:ShinTakezou|ShinTakezou]] 14:55, 12 December 2008 (UTC)

:True. But it is all worth knowing :-)  --[[User:Paddy3118|Paddy3118]] 14:56, 12 December 2008 (UTC)

Hey - why not make it interesting and have the program print the years when
Dec. 25 falls on Sunday through 2121. Let's see if any get this wrong. --[[User:64.238.49.65|64.238.49.65]] 15:11, 12 December 2008 (UTC)

:The date is internally stored as seconds from the year 1970 (more or less). I've computed (2038-1970)*365*24*3600, obtaining 2144448000; now, we are near the limit for a 32bit signed integer... but not to the limit of a 64bit signed integer. I take a note for the day I will ask myself is it is worth buying a 64bit hardware :) About the 2121, 64bit systems still can :) With a 64bit signed int, you can really go a lot further (I wonder if you thought about a limit in the way the day is found) --[[User:ShinTakezou|ShinTakezou]] 15:22, 12 December 2008 (UTC)

::What I'm referring to is that 2100 is not a leap year. There may be software out there that miscalculates this, and therefore gets the dates wrong. --[[User:Rldrenth|Rldrenth]] 16:17, 12 December 2008 (UTC)

:::I hope it goes ok, since how to compute correctly leap years is on the first page of every good programming book &mdash;well at least it was on the books I've seen :D --[[User:ShinTakezou|ShinTakezou]] 16:29, 12 December 2008 (UTC)

==Ada program==
I've installed GNAT too, and tried the Ada code; it says:


```txt

yuletide.adb:1:06: "Ada.Calendar.Formatting" is not a predefined library unit

```


Should the Ada coders add a libheader or such an info? Or is it a GNAT problem? Would like to know. --[[User:ShinTakezou|ShinTakezou]] 15:28, 12 December 2008 (UTC)

: Did you used the -gnat05 switch? Ada.Calendar.Formatting is a library unit of [[Ada 2005]]. However, I presume you have some very outdated version of GNAT, because even without the switch, the compiler should tell:
: 
```txt
yuletide.adb:1:18: warning: "Ada.Calendar.Formatting" is an Ada 2005 unit
```

: My version is 20081029-43. OK, it is GNAT Pro. GNAT GPL should be xxxxxxxx-41, I guess. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 16:46, 12 December 2008 (UTC)

::Tried... I've GNAT 4.2.3, GPL of course... maybe it's a Pro only ''unit'' :( --[[User:ShinTakezou|ShinTakezou]] 18:05, 12 December 2008 (UTC)

<blockquote><blockquote>No, it is a part of the standard library. I have tested it at home on my Linux box with GNAT GPL:

```txt
>gcc -v
Using built-in specs.
Target: i686-pc-linux-gnu
Configured with: /cardhu.b/gnatmail/release-gpl/build-cardhu/src/configure --prefix=/usr/gnat --target=i686-pc-linux-gnu --host=i686-pc-linux-gnu --build=i686-pc-linux-gnu --enable-languages=c,ada --disable-nls --disable-libada --enable-checking=release --enable-threads=posix
Thread model: posix
gcc version 4.1.3 20080522 for GNAT GPL 2008 (20080521)

```

Then compiled as:

```txt
>gnatmake -gnat05 yuletide.adb
```

I've got:

```txt
>./yuletide.adb
2011-12-25 00:00:00
2016-12-25 00:00:00
2022-12-25 00:00:00
2033-12-25 00:00:00
2039-12-25 00:00:00
2044-12-25 00:00:00
2050-12-25 00:00:00
2061-12-25 00:00:00
2067-12-25 00:00:00
2072-12-25 00:00:00
2078-12-25 00:00:00
2089-12-25 00:00:00
2095-12-25 00:00:00
2101-12-25 00:00:00
2107-12-25 00:00:00
2112-12-25 00:00:00
2118-12-25 00:00:00

```

I think you have some installation problem. Check the system path. It seems that the compiler cannot find the include directory, where the library files are stored. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 19:08, 12 December 2008 (UTC)
</blockquote></blockquote>

Thanks for your time, I will check all the env. --[[User:ShinTakezou|ShinTakezou]] 19:26, 12 December 2008 (UTC)


### Ada hard time...

I did all the kind of tests I could do. At the end, taken a look at the *.ads file (ugly naming conventions!), and at the end I successfulling compiled the following code:


```ada
with GNAT.Calendar;          use GNAT.Calendar;
with GNAT.Calendar.Time_IO;  use GNAT.Calendar.Time_IO;
with Ada.Text_IO;            use Ada.Text_IO;
 
procedure Yuletide is
begin
   for Year in 2008..2121 loop
      if Day_Of_Week (Time_Of (Year, 12, 25, 0, 0, 0)) = Sunday then
         Put_Line (Image (Time_Of (Year, 12, 25, 0,0,0), ISO_Date));
      end if;
   end loop;
end Yuletide;
```


Which by the way on my machine outputs (only last lines)


```txt
2089-12-25
2095-12-25

raised CONSTRAINT_ERROR : yuletide.adb:8 range check failed
```


Anyway, at least I ''discovered'' how to ''arrange'' the code on my system with my poor GNAT :) --[[User:ShinTakezou|ShinTakezou]] 20:52, 12 December 2008 (UTC)

: I see, now it seems to be clear. First of all, GNAT.Calendar is not a part of the standard. But that is not the problem, Ada.Calendar is. The standard ([[Ada 2005]]) requires Year_Number to be at least in the range 1901..2399. See [http://www.adaic.org/standards/1zrm/html/RM-9-6.html LRM 9.6 11/2]. So the code given in the Ada solution '''must''' work, or else your compiler is not Ada 2005. From your attempts I deduce that it is indeed not. You have an [[Ada 95]] compiler. In Ada 95 the range of Number was narrower, 1901 .. 2099. This is why your code produces an exception. You can slightly modify your code:
<blockquote>

```ada

with GNAT.Calendar;          use GNAT.Calendar;
with GNAT.Calendar.Time_IO;  use GNAT.Calendar.Time_IO;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Calendar;           use Ada.Calendar;

procedure Yuletide is
begin
   for Year in Year_Number range 2008..2121 loop -- Explicitly specify it as Year_Number
      if Day_Of_Week (Time_Of (Year, 12, 25, 0, 0, 0)) = Sunday then
         Put_Line (Image (Time_Of (Year, 12, 25, 0,0,0), ISO_Date));
      end if;
   end loop;
end Yuletide;
```

</blockquote>
:Now the compiler will warn you that Year is not in the range of Year_Number and you will get Constraint_Error at run time. So the problem is that your GNAT is not [[Ada 2005]]. Remove it and install a new one. The official distributor is [https://libre.adacore.com here]. Register yourself (it is free) and download GNAT GPL for your platform. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 21:48, 12 December 2008 (UTC)

::Thanks again. I imagined that GNAT.* is not standard, but with Ada.Calendar nothing worked :( Going to update out of the repositories :D --[[User:ShinTakezou|ShinTakezou]] 23:27, 12 December 2008 (UTC)

==When failing and when not on 32bit machine==
I've written (and tested) the C and UNIX Shell code, showing that they stop in the year 2033, depending on the fact that both uses the (g)libc on a 32bit system; in fact a run on a 64bit machine was successful. But this must not make one think the same apply on e.g. Python and Java code: in fact I've test them too, and they give the '''right results''' even on my machine! So, they don't use the underlying libc to compute dates and time. Luckly! So they proved to be really system independent! --[[User:ShinTakezou|ShinTakezou]] 16:07, 12 December 2008 (UTC)

'''Note''': UNIX Shell example uses standard tools like date, which in turn uses (g)libc library, that in its ''standard'' POSIX implementation is limited by the underlying ''hardware'' max integer. So on 64bit machines it will give the right answers until 2121; on 32bit machines, no, no matter how the code is changed (it should use some other program and avoid <tt>date</tt>... of course we could use any of the interpreter installed on a system, but it would be almost cheating:D) --[[User:ShinTakezou|ShinTakezou]] 18:10, 12 December 2008 (UTC)

==Yuletide Wish==
I thought I would write up the task at lunchtime when at work, and have time enugh to do a Python implementation when I got home. No such luck, you beat me too it :-)

Well, let me be the first to wish you fellow RC'ers happy holidays then. --[[User:Paddy3118|Paddy3118]] 17:12, 12 December 2008 (UTC)

:Thanks and the same to you all. --[[User:ShinTakezou|ShinTakezou]] 18:06, 12 December 2008 (UTC)


==UNIX Shell==
As modified by 71.106.183.17. I suggest to give back my implementation, it should be a little bit better, since this one now relies on the execution of another program (seq) while the for((..)) version is all executed into the bash interpreter. Maybe this last version works also with other shells, but I've put the Works With with a reason then! Let's think about. --[[User:ShinTakezou|ShinTakezou]] 00:28, 14 December 2008 (UTC)

I am happy to use your original, although I did check and ''seq'' is available on Cygwin. --[[User:Paddy3118|Paddy3118]] 06:44, 14 December 2008 (UTC)

:It's not so important... The seq is into the GNU coreutils, so 99/100 should exist. It's a memory and elegance matter. The backtilde say to execute the extern command, which generate a list for ''for''. The calling to another extern tool can be avoided, and we don't need to create the list which is like: 2008 2009 2010 2011 2012 2013 ..... 2121 ... not so short. Bash allows to use a counter in a rather common way, so I've thought it was much better, and we have not to worry about how long the list generated as output by ''seq'' is. Anyway, it does not matter. --[[User:ShinTakezou|ShinTakezou]] 15:04, 14 December 2008 (UTC)

==Zune Problem==
It seems owners of the Microsoft Zune MP3 player had date related issues [[http://74.125.77.132/search?q=cache:6Q7xpDmGz-8J:www.zuneboards.com/forums/zune-news/38143-cause-zune-30-leapyear-problem-isolated.html+Zune+leapyear&hl=en&client=firefox-a&gl=uk&strip=1 recently]].

(The link is to a Google cache as the page was not accessible when I tried).

--[[User:Paddy3118|Paddy3118]] 13:27, 2 January 2009 (UTC)

== Zeller's congruence ==

I just fixed a bug in the Vedit macro language implementation.
The modulo calculation for negative numbers does not work as it should (it is actually remainder, not modulo).
See [[wp:Zeller's_congruence]].
It does not cause problem in this task, but try to check the Sundays on March 1st!

I changed the equation so that the numerator is never negative.
Some other language implementations are using Zeller's congruence, too, so those may have the same problem.

--[[User:PauliKL|PauliKL]] 16:30, 5 May 2009 (UTC)


###  TeX formula 

<math>\begin{align}
  adjustment = & \lfloor (14 - month) \div 12 \rfloor            \\
          mm = & month + 12 \times adjustment                    \\
          yy = & year - adjustment                               \\
weekday \equiv & day + \lfloor (13 \times mm + 8) \div 5 \rfloor \\
               & + yy + \lfloor yy \div 4 \rfloor
                 - \lfloor yy \div 100 \rfloor
                 + \lfloor yy \div 400 \rfloor \mod 7            \\
\end{align}</math>

I wrote this [[metawikipedia:Help:Formula|TeX formula]] for Zeller's congruence, but decided not to put it on the page, so I saved it here. --[[User:Kernigh|Kernigh]] 21:43, 11 November 2011 (UTC)

== Deleting examples ==
I'm not sure there's a compelling reason to remove examples - e.g. Perl here - only because they're less concise or stylish, unless they're wrong or don't meet the task description. A big point of RC is to show different ways to solve the same problem, and I'm sure many people appreciate seeing different approaches. Especially in Perl, TMTOWTDI. :-) --[[User:Snoman|Snoman]] 22:27, 30 July 2010 (UTC)

:Do you advocate more than what happens for Perl in other tasks? I think there is a code of practice that is emerging from considering a number of tasks, where there is usually only one language example for a particular task, unless a different algorithm is used in each example - such as one using a regex and another not; but even then, it is very rare to show any more than two ways of fulfilling the task. This is probably why examples get replaced. Personally I have been known to put back prior Python examples if I think there is no improvement shown by a replacement and use the talk page to discuss reasons for change. (Especially if the reason is just to replace a small example done at the Python shell showing shell prompts by the same thing done in a file). 

: I wrote the above before looking at the change. On looking at the long list of four Perl examples as it was in Revision as of 11:17, 23 July 2010, there seems to have been one that used Time::Local, and three using DateTime. It could be shortened to maybe one example from what Perl users would consider the best module to use and then just a mention that the other module is available. You could use the talk page to try and work out the details with the other Perl users as the discussion might prove insightful to non-Perl users. (When constrained, how do Perl users wish their language to be seen when there are differing levels of concise-ness available)? --[[User:Paddy3118|Paddy3118]] 02:04, 31 July 2010 (UTC)

:: The thing that bothered me about the removal was that it went from several examples showing at least a couple different approaches and coding styles to just one examples. That rather defeats Perl's TMTOWTDI philosophy, IMO. --[[User:Short Circuit|Michael Mol]] 02:57, 31 July 2010 (UTC)
::: Maybe a mention of TMTOWTDI  on RC's main Perl page and mention of its implications w.r.t. RC? --[[User:Paddy3118|Paddy3118]] 04:26, 31 July 2010 (UTC)

Also, DateTime is not a core module, so is inferior in many ways compared to the deleted example which uses Time::Local, which ''is'' a core module (i.e., included with Perl). --[[Special:Contributions/71.141.117.11|71.141.117.11]] 04:00, 31 July 2010 (UTC)
: Seems like you have a reason to leave the Time::Local version in fully and just maybe mention the existence of  DateTime?
 P.S. I should mention that I am a reluctant Perl programmer --[[User:Paddy3118|Paddy3118]] 04:26, 31 July 2010 (UTC)

Thanks, guys. I'm still a RC n00b, so I flagged my undo here and I leave it up to your wiser heads to decide. BTW, the examples were not my code, so I have no vested interest. I think ShinTakezou was the original poster.

> When constrained, how do Perl users wish their language to be seen when there are differing levels of concise-ness available

That's a very interesting question. I've been out of Perl culture for a while, so I'm not sure how it has evolved. Perl is famous for "golf" solutions - tight, elegant, and unreadable. But stylistically, Perl is less constrained than some other languages and open to more conventional approaches as well. My impression is that RC is supposed to be pedagogically useful, as well as a place to show off one's code-fu. So allowing less concise solutions to stand may sometimes be helpful.

I've already seen a few instances where the Perl Patrol have come in and re-written code for what seem to be mostly personal style preferences, e.g. my $foo = shift vs. my $foo = $_[0]. So I agree that this might be a useful discussion on the Perl page, and maybe some kind of consensus might emerge. Especially with Perl 6 looming, and the first official non-dev release of Perl 6 "Rakudo Star" only yesterday.

Fortunately, given the number of Snobol programmers out there, I'd guess my main Snobol4 contributions so far are less likely to be rewritten. :-) --[[User:Snoman|Snoman]] 05:58, 31 July 2010 (UTC)

I removed the <code>Time::Local</code> version because it doesn't work because on 32-bit machines due to the year 2038 problem (look at the sample output). I removed another two examples because they varied from the last example only in ways that had nothing to do with the task. I'm a big believer in TIMTOWTDI, but I see the idea behind that principle being that you can pick whatever method you like, not that you should pick multiple redundant methods. It seems obvious to me that when multiple solutions are provided for a single language, they should vary in ways that are ''relevant to the task''—see, for example, how so many languages have multiple solutions for [[Fibonacci sequence]], since (a) the task is understood to be a demonstration of looping, so showing how your language does iteration and recursion is wholly appropriate, and (b) there are a number of drastically different alternative approaches. If we allow those three different versions of essentially the same program here, I see no argument why every Perl program in Rosetta Code that has a while loop couldn't be accompanied by a nearly identical version that uses an until loop. Let's try to keep the signal-to-noise ratio high. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 22:27, 31 July 2010 (UTC)

:Thanks, I've learned something more today. So maybe keep '''a''' DateTime version, together with a note that it is on CPAN and that built-in Time::Local is not being used due to these problems on 32 bit machines? --[[User:Paddy3118|Paddy3118]] 23:50, 31 July 2010 (UTC)
:: Sounds good to me. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 18:20, 1 August 2010 (UTC)

:I don't at all like examples and snippets being removed from RC, as long as the code is legal (copyright caveat, etc). That said, with the site's current organization, I can understand why it would be a problem if there were thirty examples for Ayrch for each task, but only a relatively small number for other languages.

:Rosetta Code's core mission approach is helping people learn tool X by looking at tool X applied alongside more familiar tools Y and Z.  A tool can be a language, a library, a programming paradigm or a language features (consider the scenario of showing how to do memory-management relevant things in a language with ''optional'' garbage collection). To that end, it's practical to apply learning-by-comparison within a language, such as demonstrating '''for''' against '''foreach''' against '''map'''.

:"Picking whatever method you like" is a matter for the individual contributor and the individual user of the language, but when Rosetta Code is being used as a learning resource, it becomes important to avoid hiding language capabilities.

:Again, I can understand why that could be a problem, given the way the site is currently structured with monolithic task pages.  Migrating toward the Semantic MediaWiki feature set should allow us to clean and organize things in a more fine-grained fashion, such as by allowing individual code examples to be tagged with the language features they exploit. --[[User:Short Circuit|Michael Mol]] 13:54, 1 August 2010 (UTC)
:: Don't you fear that a don't-delete-stuff-so-long-as-it's-legal strategy could lead to good code getting lost in crufty, mediocre code, and to people adding trivial variations just for ego's sake? This isn't the first time that I deleted something for the sake of improving the site. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 18:20, 1 August 2010 (UTC)
:::# Crufty, mediocre code -- We already have that, even in areas without apparent redundancy. ''Every'' time I see RC linked to from Reddit, ycombinator or some other geek link aggregate-and-comment place, I see complaints about the code quality not being that great. That's why we have [[Template:incorrect]] and other ENAs. I've been investigating ways of automating some components of code review to help maintain and improve quality.
:::# Code getting lost -- Not especially, particularly if a language's multiple tacks on the same task can be differentiated by approach, illustrating different ways the language can be used. I ''especially'' expect to see multiple-tacks taken as more third-party libraries are demonstrated. DirectX vs OpenGL vs SDL, DirectComputer versus CUDA vs OGL4, Linux's ALSA vs SDL vs POSIX's OSS vs Win32's WaveOut. Language/Task isn't the only pair of intersections I want or expect to see Rosetta Code implement.
:::# Trivial variations just for ego's sake -- I don't know. I can't define what "trivial" means within a language I don't know, and I know (through personal experience) that the word "trivial" is tied very closely to individual experience and competence. As long as the difference between two separate code examples can be described in meaningful terms, then I think there's likely to be value there--I can use those described differences in structuring the site and ''aiding'' browsability.
::: By and large, the installation of Semantic MediaWiki is intended to help address these problems, by making the wiki software aware of the differences between examples(what task, what language, what library, what paradigm), and allowing us to create navigation pages and shortcuts. --[[User:Short Circuit|Michael Mol]] 19:34, 1 August 2010 (UTC)

::Maybe we always have to take the complexity of the task being solved into account. For a simple task such as "Output the numbers 1 to five in order", you might expect an example using a for loop and another using a foreach loop, for example; as the way you loop is a large part of the task.
::For a larger task such as [[Range expansion]] you might prefer to work on the one example and edit it to come to a consensus on what looping structure to use. But an example picking and testing characters from a string might go ''alongside'' another example that parses using a regexp as the two methods are sufficiently different to be noteworthy.
::I don't know how to write the rules of noteworthiness other than by reading a lot of RC edits and looking at what happens. This will tend to perpetuate the status quo, however discussions like this pop up to help :-) (P.S. Edited simultaneousely with Michael's edit above. Written to follow the comment of Underscore)
--[[User:Paddy3118|Paddy3118]] 19:47, 1 August 2010 (UTC)

== Month contribution ==
Although date calculations don't usually need much optimization, it may be interesting to note that the division by 10 in Zeller's congruence (useful at Zeller time for hand calculations), can be replaced by a shift. Keeping only the interesting part, all it does is this :

Count days for 1st of each month, from 1st march, modulo 7 (in the following list, the third column is the number of days of the month, and the fourth is the cumulated number of days, mod 7, with a 1 month lag since march starts at day 0)
<lang>3 march      31   0
4 april      30   3 = 31 mod 7
5 may        31   5 = (31 + 30) mod 7
...
12 december  31   2 = (31 + 30 + ... + 30) mod 7  // the last in the sum is november, remember the lag
13 january   31   5 = ...
14 february  --   1 = ...
```


For february, one does not need to know the number of days, and it's precisely because it can change that we use a trick (year-1, month+12) to push it at the end of the list.

So, all one really needs is a mapping from (3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14) to (0, 3, 5, 1, 3, 6, 2, 4, 0, 2, 5, 1), possibly up to a constant, mod 7.
As we know from Zeller's congruence, '''floor((26*(m+1))/10)''' works perfectly well.
But so does '''floor((83*m+24)/32)''', and division by 32 is simply a shift.
Of course, there are many other values of A, B, C such that floor((A*m+B)/C) does the job, but 32 is the least power of 2 that works.

[[User:Dejvicka|Dejvicka]] 12:54, 22 September 2012 (UTC)
