+++
title = "LINESIZE.REX"
description = ""
date = 2017-09-07T00:02:50Z
aliases = []
[extra]
id = 12933
[taxonomies]
categories = []
tags = []
+++

The   '''LINESIZE.REX'''   is a REXX program to emulate the   '''linesize'''   BIF for some REXX programs.



The help for the   '''LINESIZE'''   REXX program is included here ──► [[LINESIZE.HEL]].

```rexx
/*REXX*/ trace off

/*┌────────────────────────────────────────────────────────────────────┐
┌─┘                                                                    └─┐
│ The  LINESIZE  function exists for those REXX interpreters that don't  │
│ support the  LINESIZE  built-in function.                              │
│                                                                        │
│ The following REXXes (and others) support the  LINESIZE  as a BIF:     │
│                                                                        │
│         CMS                                                            │
│         PC/REXX                                                        │
│         Personal REXX                                                  │
│         R4                                                             │
│         ROO                                                            │
│         TSO                                                            │
│                                                                        │
│ Method:  to save time, this program first attempts to find the DOS     │
│ environmental variable LINES.  Failing that, it then parses the result │
│ from the   MODE CON   (DOS) command and scans for the  COLUMNS  text.  │
└─┐                                                                    ┌─┘
  └────────────────────────────────────────────────────────────────────┘*/

parse arg !
if !all(arg())  then exit
if !cms then address ''

signal on halt
signal on novalue
signal on syntax


if \!dos  then return 24 80                  /*not DOS?  Return default.*/

@abc='abcdefghijklmnopqrstuvwxyz'
@erase    = 'ERASE'                          /*point to dos ERASE    cmd*/
@find     = 'FIND'                           /*point to the dos FIND cmd*/
@MODE     = 'MODE'                           /*point to the dos MODE cmd*/
tfid=                                        /*name of a temporary FID. */

                                             /*Note:   /i = ignore case.*/
@find_col = '/i "column"'                    /*find line with    COLUMN.*/

findLines=1
sw=0


parse var !! _ . '(' ops ')' __
if _\=='' | __\==''  then call er 59
ops=space(ops)

  do  while  ops\==''
  parse  var  ops  _1 2 1 _ . 1 _o ops
  upper _
    select
    when _==','                    then nop
    when _1=='.' & pos("=",_)\==0  then tops=tops _o
    when abbn('SCRWIDths' )|,
         abbn('WIDths'    )|,
         abbn('WIDes'     )|,
         abbn('WIDs'      )|,
         abbn('COLumns'   )|,
         abbn('COLs'      )        then findLines=no()
    otherwise                      call er 55,_o
    end   /*select*/
  end     /*while*/


/*──────────────attempt to use the DOS environmental variable: LINES.   */
if findLines  then do
                   sd=word(!var('LINES'),1)  /*pick off the first word. */
                   if \isInt(sd)  then sd=0  /*if not whole #,  then 0. */
                   sd=sd/1                   /*decimal point ?   remove.*/
                   end


/*──────────────if not defined, then use (DOS) MODE (writes to a file). */
if sw==0  then                               /*not defined?  use MODE.  */
   do                                        /*first, find temp. disk.  */
   call gettfid ,'$$$'                       /*get a TEMP id:  !fn $$$  */

   @mode 'con |' @find @find_col '>' tfid    /*issue MODE CON|filter>fid*/

   call linein tfid,1,0                      /*point to record 1.       */

     do  while  sw==0                        /*read file while sw|sw =0.*/
     if lines(tfid)==0  then leave           /*No lines left? We're done*/
     _=translate(linein(tfid),,'=:')         /*translate = : --> blanks.*/
     parse  upper  var  _  yname yval .      /*parse with name value.   */
     if yname=='COLUMNS'  then sw=yval       /*if COLUMNS, it's width.*/
     end    /*while*/

   @erase tfid                               /*erase the temp. file.    */
   end


if sw==0  then sw=80                         /*just in case MODE failed.*/
                                             /*use positive values.     */
return abs(sw)                               /*return width.            */


/*═════════════════════════════general 1-line subs════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════*/
!all:!!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=='NT';!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,'? ?SAMPLES ?AUTHOR ?FLOW')==0 then return 0;!call=']$H';call '$H' !fn !;!call=;return 1
!cal:if symbol('!CALL')\=="VAR" then !call=;return !call
!env:!env='ENVIRONMENT';if !sys=='MSDOS'|!brexx|!r4|!roo then !env='SYSTEM';if !os2 then !env='OS2'!env;!ebcdic=1=='f0'x;if !crx then !env='DOS';return
!fid:parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .;call !sys;if !dos then do;_=lastpos('\',!fn);!fm=left(!fn,_);!fn=substr(!fn,_+1);parse var !fn !fn '.' !ft;end;return word(0 !fn !ft !fm,1+('0'arg(1)))
!rex:parse upper version !ver !vernum !verdate .;!brexx='BY'==!vernum;!kexx='KEXX'==!ver;!pcrexx='REXX/PERSONAL'==!ver|'REXX/PC'==!ver;!r4='REXX-R4'==!ver;!regina='REXX-REGINA'==left(!ver,11);!roo='REXX-ROO'==!ver;call !env;return
!sys:!cms=!sys=='CMS';!os2=!sys=='OS2';!tso=!sys=='TSO'|!sys=='MVS';!vse=!sys=='VSE';!dos=pos('DOS',!sys)\==0|pos('WIN',!sys)\==0|!sys=='CMD';!crx=left(!sys,6)=='DOSCRX';call !rex;return
!var:call !fid;if !kexx then return space(dosenv(arg(1)));return space(value(arg(1),,!env))
$fact!:procedure;parse arg x _ .;l=length(x);n=l-length(strip(x,'T',"!"));if n<=-n|_\==''|arg()\==1 then return x;z=left(x,l-n);if z<0|\isInt(z) then return x;return $fact(z,n)
$fact:procedure;parse arg x _ .;arg ,n ! .;n=p(n 1);if \isInt(n) then n=0;if x<-n|\isInt(x)|n<1|_||!\==''|arg()>2 then return x||copies("!",max(1,n));!=1;s=x//n;if s==0 then s=n;do j=s to x by n;!=!*j;end;return !
$sfxa:parse arg ,s,m;arg u,c;if pos(left(s,2),u)\==0 then do j=length(s) to compare(s,c)-1 by -1;if right(u,j)\==left(c,j) then iterate;_=left(u,length(u)-j);if isNum(_) then return m*_;leave;end;return arg(1)
$sfxf:parse arg y;if right(y,1)=='!' then y=$fact!(y);if \isNum(y) then y=$sfxz();if isNum(y) then return y;return $sfxm(y)
$sfxm:parse arg z;arg w;b=1000;if right(w,1)=='I' then do;z=shorten(z);w=z;upper w;b=1024;end;p=pos(right(w,1),'KMGTPEZYXWVU');if p==0 then return arg(1);n=shorten(z);r=num(n,f,1);if isNum(r) then return r*b**p;return arg(1)
$sfxz:return $sfxa($sfxa($sfxa($sfxa($sfxa($sfxa(y,'PAIRs',2),'DOZens',12),'SCore',20),'GREATGRoss',1728),'GRoss',144),'GOOGOLs',1e100)
abb:arg abbu;parse arg abb;return abbrev(abbu,_,abbl(abb))
abbl:return verify(arg(1)'a',@abc,'M')-1
abbn:parse arg abbn;return abb(abbn)|abb('NO'abbn)
er:parse arg _1,_2;call '$ERR' "14"p(_1) p(word(_1,2) !fid(1)) _2;if _1<0 then return _1;exit result
err:call er '-'arg(1),arg(2);return ''
erx:call er '-'arg(1),arg(2);exit ''
getdtfid:tfid=p(!var("TMP") !var('TEMP') homedrive()"\");if substr(tfid,2,1)==':'&substr(tfid,3,1)\=="\" then tfid=insert('\',t,2);return strip(tfid,'T',"\")'\'arg(1)'.'arg(2)
getTFID:if symbol('TFID')=='LIT' then tfid=;if tfid\=='' then return tfid;gfn=word(arg(1) !fn,1);gft=word(arg(2) 'ANS',1);tfid='TEMP';if !tso then tfid=gfn'.'gft;if !cms then tfid=gfn','gft",A4";if !dos then tfid=getdTFID(gfn,gft);return tfid
halt:call er .1
homedrive:if symbol('HOMEDRIVE')\=="VAR" then homedrive=p(!var('HOMEDRIVE') 'C:');return homedrive
int:int=num(arg(1),arg(2));if \isInt(int) then call er 92,arg(1) arg(2);return int/1
isInt:return datatype(arg(1),'W')
isNum:return datatype(arg(1),'N')
na:if arg(1)\=='' then call er 01,arg(2);parse var ops na ops;if na=='' then call er 35,_o;return na
nai:return int(na(),_o)
nan:return num(na(),_o)
no:if arg(1)\=='' then call er 01,arg(2);return left(_,2)\=='NO'
novalue:!sigl=sigl;call er 17,!fid(2) !fid(3) !sigl condition('D') sourceline(!sigl)
num:procedure;parse arg x .,f,q;if x=='' then return x;if isNum(x) then return x/1;x=space(translate(x,,','),0);if \isNum(x) then x=$sfxf(x);if isNum(x) then return x/1;if q==1 then return x;if q=='' then call er 53,x f;call erx 53,x f
p:return word(arg(1),1)
s:if arg(1)==1 then return arg(3);return word(arg(2) 's',1)
shorten:procedure;parse arg a,n;return left(a,max(0,length(a)-p(n 1)))
syntax:!sigl=sigl;call er 13,!fid(2) !fid(3) !sigl !cal() condition('D') sourceline(!sigl)
```


[[Category:REXX library routines]]
