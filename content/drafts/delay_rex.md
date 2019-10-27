+++
title = "DELAY.REX"
description = ""
date = 2018-10-28T21:18:16Z
aliases = []
[extra]
id = 12936
[taxonomies]
categories = []
tags = []
+++

This the   '''DELAY.REX'''   REXX program which emulates the   '''delay'''   BIF function as implemented by the PC/REXX and Personal REXX interpreters. 

This REXX program (below) will work with most REXXes:
:::* CMS REXX
:::* PC/REXX   (see note)
:::* Personal REXX   (see note)
:::* REGINA REXX
:::* ROO REXX
:::* R4 REXX
:::* TSO REXX
:::* (Microsoft) DOS
:::* (Microsoft) Windows
:::* any system that supports the PING command
in conjunction with the following program (either external or imbedded).


Note:   when PC/REXX or Personal REXX are used, those REXXes already have a built-in function (BIF), so the   '''delay'''    subroutine (below) will never be executed, but the REXX   '''DELAY'''   BIF will be used instead.

This REXX program only uses whole seconds   (fractional seconds are ignored).


## REXX


```rexx
/*REXX program  delays  (or SLEEPS)  a number of  whole  number of seconds.             */
trace off                                        /*suppress any REXX error messages.    */
parse arg !                                      /*obtain all the arguments from the CL.*/
if !all( arg() )  then exit                      /*Any kind of documentation requested? */
if !cms  then address ''                         /*Is this CMS?  Then use fast cmd path.*/
signal on halt                                   /*handle  (user)  HALT  gracefully.    */
signal on noValue                                /*handle REXX   noValue error/mishap.  */
signal on syntax                                 /*handle REXX    syntax errors.        */

               /*┌────────────────────────────────────────────────────────────────────┐
               ┌─┘ The  DELAY  function is used to delay (wait) a specific amount of  └─┐
               │ (wall─clock)  time specified in seconds.  Any fraction part is ignored │
               │ for most REXX interpreters, but it is supported for Regina REXX,       │
               │ even though some operating systems  (or REXXes)  support that option.  │
               │                                                                        │
               │ If the REXX program invoking  DELAY  function is running under PC/REXX │
               │ or  Personal REXX,  this REXX program should never be invoked as those │
               └─┐ REXXes have their own built─in function (BIF)  named   "DELAY".    ┌─┘
                 └────────────────────────────────────────────────────────────────────┘*/

parse var !  n  _  .                             /*parse args from command line or parms*/
if _\=='' | arg()>1  then call er 59             /*are there too many arguments? Err msg*/
if n==''  | n==","   then n= 1                   /*Mo arguments?    Then assume one sec.*/
if \isNum(n)  then call er 53,n 'delay-seconds'  /*Is   n   not numeric?  Say error msg.*/
nFrac= n                                         /*retain the original delay time.      */
n= n % 1                                         /*elide any fractional part of the time*/
if nFrac<=0  then return 0                       /*Negative or zero time?  No sleep then*/

@cpsleep  = 'CP SLEEP'                           /*point to the  (CP)  SLEEP   command. */
@ping     = 'PING'                               /*point to the   DOS  PING    command. */
@pingArgs = '-n'   n   "127.0.0.1  >  NUL"       /*arguments used with the DOS PING cmd.*/


                              /* ┌────────────────────┐ */
                              /* │ delay  n  seconds. │ */
                              /* │ or fractional secs.│ */
                              /* └────────────────────┘ */
  select
  when !cms     then @cpsleep   n    "SEC"        /*Is this  CMS?      Use   CP SLEEP.  */
  when !tso     then call sleep n                 /*Is this  TSO?      Use   SLEEP cmd. */
  when !regina  then do                           /*Is this Regina?                     */
                     if nFrac=n  then call sleep n                     /*whole seconds? */
                                 else call beep 32000, nFrac * 1000    /*uses fraction. */
                                                  /* [↑]  sound MAY be heard, faint tic.*/
                     end
  when !dos     then @ping  @pingArgs             /*Is this  DOS?      Use   PING  cms. */
  otherwise          nop                          /*don't know what this environment is.*/
  end   /*select*/

return 0                                          /*return a zero value  (if a fuction).*/


/*══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════*/
!all:  !!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=='NT';!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,'? ?SAMPLES ?AUTHOR ?FLOW')==0 then return 0;!call=']$H';call '$H' !fn !;!call=;return 1
!cal:    if symbol('!CALL')\=="VAR"  then !call=;     return !call
!env:    !env='ENVIRONMENT'; if !sys=='MSDOS' | !brexx | !r4 | !roo  then !env='SYSTEM'; if !os2  then !env='OS2'!env; !ebcdic=3=='f3'x; if !crx  then !env='DOS';  return
!fid:    parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .; call !sys; if !dos  then do; _=lastpos('\',!fn); !fm=left(!fn,_); !fn=substr(!fn,_+1); parse var !fn !fn '.' !ft; end;   return word(0 !fn !ft !fm, 1+('0'arg(1)))
!rex:    parse upper version !ver !vernum !verdate .; !brexx='BY'==!vernum; !kexx='KEXX'==!ver; !pcrexx='REXX/PERSONAL'==!ver | 'REXX/PC'==!ver; !r4='REXX-R4'==!ver; !regina='REXX-REGINA'==left(!ver,11); !roo='REXX-ROO'==!ver; call !env; return
!sys:    !cms=!sys=='CMS'; !os2=!sys=='OS2'; !tso=!sys=='TSO' | !sys=='MVS'; !vse=!sys=='VSE'; !dos=pos('DOS',!sys)\==0 | pos('WIN',!sys)\==0 | !sys=='CMD'; !crx=left(!sys,6)=='DOSCRX'; call !rex; return
!var:    call !fid;  if !kexx  then return space( dosenv( arg(1) ) );        return space( value( arg(1), , !env) )
er:      parse arg _1,_2;  call '$ERR' "14"p(_1) p(word(_1, 2) !fid(1)) _2;  if _1<0  then return _1;        exit result
p:       return word( arg(1), 1)
halt:    call er .1
isNum:   return datatype( arg(1), 'N')
noValue: !sigl=sigl;  call er 17, !fid(2) !fid(3) !sigl        condition('D') sourceline(!sigl)
syntax:  !sigl=sigl;  call er 13, !fid(2) !fid(3) !sigl !cal() condition('D') sourceline(!sigl)
```


Coding note:   the   '''!<small>xxx</small>'''   subroutines  (above) deal mostly with determining what version of REXX is being invoked and what operating system is being used;   and based on that information, appropriate flags (variables) are set.   This is an example of a robust boilerplate code checking for various versions of REXX and operating systems, and it also defines additional flags not used within this particular program. 

Programming note:   The subroutine   '''$ERR'''   isn't included here;   so here is the gist of the error messages:
::*   '''er 59'''       too many arguments specified for the ─── DELAY ─── command.
::*   '''er 53'''       argument ─── xxx ─── isn't numeric for the option ─── delay-seconds ─── for the ─── DELAY ─── command.
