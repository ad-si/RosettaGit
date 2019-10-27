+++
title = "SOUND.REX"
description = ""
date = 2017-09-07T00:04:09Z
aliases = []
[extra]
id = 12880
[taxonomies]
categories = []
tags = []
+++

This is the   '''SOUND.REX'''   (REXX) program.

It is used by other REXX programs to issue sounds through the PC speaker.

```rexx
/**/trace o;      parse arg !;         if !all(arg()) then exit
if !cms then address ''
signal on halt;   signal on novalue;   signal on syntax

if \!dos     then return ''                     /*if not   DOS,   exit. */
if \!regina  then return ''                     /*if not Regina,  exit. */

if arg()==0  then call beep 400
if arg()==1  then call beep arg(1)
if arg()==2  then call beep arg(1),arg(2)*1000  /*arg2 is in millisecs. */

return ''

/*═════════════════════════════general 1-line subs════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════*/
!all:!!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=='NT';!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,'? ?SAMPLES ?AUTHOR ?FLOW')==0 then return 0;!call=']$H';call '$H' !fn !;!call=;return 1
!cal: if symbol('!CALL')\=="VAR" then !call=;return !call
!env: !env='ENVIRONMENT';if !sys=='MSDOS'|!brexx|!r4|!roo then !env='SYSTEM';if !os2 then !env='OS2'!env;!ebcdic=1=='f0'x;return
!fid: parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .;call !sys;if !dos then do;_=lastpos('\',!fn);!fm=left(!fn,_);!fn=substr(!fn,_+1);parse var !fn !fn '.' !ft;end;return word(0 !fn !ft !fm,1+('0'arg(1)))
!rex: parse upper version !ver !vernum !verdate .;!brexx='BY'==!vernum;!kexx='KEXX'==!ver;!pcrexx='REXX/PERSONAL'==!ver|'REXX/PC'==!ver;!r4='REXX-R4'==!ver;!regina='REXX-REGINA'==left(!ver,11);!roo='REXX-ROO'==!ver;call !env;return
!sys: !cms=!sys=='CMS';!os2=!sys=='OS2';!tso=!sys=='TSO'|!sys=='MVS';!vse=!sys=='VSE';!dos=pos('DOS',!sys)\==0|pos('WIN',!sys)\==0|!sys=='CMD';call !rex;return
!var: call !fid;if !kexx then return space(dosenv(arg(1)));return space(value(arg(1),,!env))
er:   parse arg _1,_2;call '$ERR' "14"p(_1) p(word(_1,2) !fid(1)) _2;if _1<0 then return _1;exit result
p:    return word(arg(1),1)
halt: call er .1
novalue:!sigl=sigl;call er 17,!fid(2) !fid(3) !sigl condition('D') sourceline(!sigl)
syntax:!sigl=sigl;call er 13,!fid(2) !fid(3) !sigl !cal() condition('D') sourceline(!sigl)
```


[[Category:REXX library routines]]
