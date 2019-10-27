+++
title = "Talk:French Republican calendar"
description = ""
date = 2019-02-25T22:07:30Z
aliases = []
[extra]
id = 22150
[taxonomies]
categories = []
tags = []
+++


###  bugfix in Go 

Found when translating to phix, bug occurred for first day of leap years >=1996 only (according to my sainty test).
Running either Go example gave me:

    > 22 September 2016
    Fete de la Revolution 224
    > 23 September 2016
    2 Vendemiaire 225
    > 2 Vendemiaire 225
    23 September 2016
    > 1 Vendemiaire 225
    22 September 2016

I fixed this with a couple of tweaks to dayToRep (see edit history), that may need applying to BBC BASIC and possibly others.

This is what I now get (which matches the Perl test set):

    > 22 September 2016
    1 Vendemiaire 225
    > 21 September 2016
    Fete des recompenses 224
    > 23 September 2016
    2 Vendemiaire 225
    > 1 Vendemiaire 225
    22 September 2016
    > 2 Vendemiaire 225
    23 September 2016

--[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 21:18, 24 January 2019 (UTC)

I'm not the author of the BBC BASIC entry. (I'm using FREEBasic nowadays), but it is old school basic and the bug fix was very easy. 

The BBC BASIC (for Windows) is not broken. 

Tested it with following programs.

BBC BASIC for Windows 5.94a(trial) - 8Kb
BBC BASIC for Windows 6.12a(trial) - 32Kb
BBC BASIC for SDL 2.0 Win32 Version 0.29a

The trial versions have limit on the amount of memory that can be used for the user’s program + data + stack.
The size of the program is about 4180 bytes. A trial version that has only 4Kb of memory available can’t run this program.

Bugfix:

In the <b>DEF PROC_day_to_rep</b>, <b>sansculottides%</b> is set to 6 if <b>y%</b> = is a leap year. If <b>y%</b> is increased in the While loop <b>sansculottides%</b> is not set to 5 as it should.

Solution: copy <b>IF FN_rep_leap(y%) THEN sansculottides% = 6 ELSE sansculottides% = 5</b> before <b>While</b> and paste it under the line with <b>y% += 1</b> in the While loop.


```qbasic
DEF PROC_day_to_rep(day%, RETURN d%, RETURN m%, RETURN y%)
LOCAL sansculottides%
y% = INT(day% / 365.25)
IF FN_rep_leap(y%) THEN y% -= 1
d% = day% - INT(365.25 * y%) + INT((y% + 1) / 100) - INT((y% + 1) / 400)
y% += 1
m% = 1
rem copy the next line
IF FN_rep_leap(y%) THEN sansculottides% = 6 ELSE sansculottides% = 5
WHILE d% > 30
  d% -= 30
  m% += 1
  IF m% = 13 THEN
    IF d% > sansculottides% THEN
      d% -= sansculottides%
      m% = 1
      y% += 1
      rem paste the line here
    ENDIF
  ENDIF
ENDWHILE
ENDPROC
```

--[[User:Frisian|Frisian]] ([[User talk:Frisian|talk]]) 21:20, 6 February 2019 (UTC)
:Thanks, that all looks good. Erm, can I ask though, is there some difference between "broken" and "needs bugfix" that I should be aware of? I suspect the "not broken, see talk page" comment on the BBC entry is unlikely to be helpful and thus should probably be removed. --[[User:Petelomax|Pete Lomax]] ([[User talk:Petelomax|talk]]) 04:48, 7 February 2019 (UTC)
removed it.--[[User:Frisian|Frisian]] ([[User talk:Frisian|talk]]) 22:07, 25 February 2019 (UTC)
