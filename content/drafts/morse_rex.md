+++
title = "$MORSE.REX"
description = ""
date = 2017-09-06T23:59:43Z
aliases = []
[extra]
id = 12878
[taxonomies]
categories = []
tags = []
+++

The following is the   '''$MORSE.REX'''   (REXX) program.

The help for the   '''$MORSE.REX'''   REXX program is included here   ──►   [[$MORSE.HEL]].


This program supports the ''International Morse code'' as well as the ''USA Morse code''   (the later being primarily used by the North American Railroads). 

Some translation is done for unsupported characters such as braces   '''{'''   '''}''', brackets   '''['''   ''']'''   and the like.  

This REXX program normally shows Morse code words one word to a line before sounding.


This REXX programs only works for Regina and PC/REXX, but other REXXes (specifically R4) will only display the Morse code, but not sound it.


The   '''$MORSE.REX'''   REXX program makes use of   '''LINESIZE'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console). 

The   '''LINESIZE.REX'''   REXX program is included here   ──►   [[LINESIZE.REX]].


The   '''$MORSE.REX'''   REXX program makes use of   '''$T.REX'''   REXX program which is used to display text and/or write the text to a file. 

The   '''$T.REX'''   REXX program is included here   ──►   [[$T.REX]].


The   '''$MORSE.REX'''   REXX program makes use of   '''$ERR.REX'''   REXX program which is used to display error messages (via   '''$T.REX'''). 

The   '''$ERR.REX '''   REXX program is included here   ──►   [[$ERR.REX]].


The   '''$MORSE.REX'''   REXX program makes use of   '''SOUND.REX'''   REXX program which is used to express sound (via the internal speaker). 

The   '''SOUND.REX'''   REXX program is included here   ──►   [[SOUND.REX]].


Some older REXXes don't have a   '''changestr'''   BIF, so one is included here   ──►   [[CHANGESTR.REX]].

```rexx
/*REXX program sounds out  (on the PC speaker)  Morse code  for (almost) any given text.*/
trace off                                        /*suppress non-zero return code message*/
parse arg !                                      /*obtain required arguments from the CL*/
if !all( arg() )  then exit                      /*if any help requested, show it & exit*/
if !cms           then address ''                /*assist CMS in handling commands fast.*/
signal on halt                                   /*be able to handle the halting of pgm.*/
signal on noValue                                /* "   "   "    "   undefined variables*/
signal on syntax                                 /* "   "   "    "   pgm  syntax errors.*/

if !==''  then exit                              /*No text to convert──►Morse code? exit*/

@abc= 'abcdefghijklmnopqrstuvwxyz'               /*define a  lowercase (Latin) alphabet.*/
@abcU=@abc;         upper @abcU                  /*   "   "  uppercase    "        "    */
parse var  !  ops  '('  plainText                /*obtain text to convert ──► Morse code*/
ops=space(ops)                                   /*elide any superfluous spaces in text.*/

              dah= '-'
              dit=  .
if !cms  then dit= 'af'x
if !dos  then dit= 'f9'x

                                    /* _b1,  _v2,  and  _b3  are pseudo-blanks and they */
_b1= 'b0'x                          /* should be a character that can't be entered from */
_b2= 'b1'x                          /* the keyboard (easily).    They are translated to */
_b3= 'b2'x                          /* true blanks by the   TT   internal subroutine.   */

colors     = !cms | !pcrexx | !r4 | !roo         /*the REXXes that support color.       */
betweens   = 0                                   /*blanks between the Morse symbols.    */
bf         = 400                                 /*the beat frequency (internal speaker)*/
clear      = 0                                   /*clear the terminal screen  (or not). */
code       =                                     /*Morse code characters  (so far).     */
delaySpace = 1
emsg       = 1
logs       = 0
long_      = '='
longer_    = '~'
long       = '__'                                /*a long   dash.                       */
longer     = '____'                              /*a longer dash.                       */
morseType  = 'I'
pause_     = '¬'
quiet      = 0
show       = 0
slice      = 0
sound      = 1
spaces     = 3
split      = 1
spread     = 1
tfid       =                                     /*the temporary file identifier.       */
 TimeDelay =  .2
   ditTime =  .1
   dahTime =  .2
  longTime =  .4
longerTime =  .8
 pauseTime =  .15
      tops =

sw=linesize()                                    /*get the width of the terminal screen.*/

  do  while ops\==''                             /*keep parsing until no more options.  */
  parse var ops _1 2 1 _ . 1 _o ops              /*pick apart various pieces of an opt. */
  upper _                                        /*convert a value to uppercase (Latin).*/

    select
    when _==','                    then nop
    when _1=='.' & pos("=",_)\==0  then tops      = tops _o
    when abbn('CLearscreen')       then clear     =  no()
    when abbn('COLORs')            then colors    =  no()
    when abbn('LOGs')              then logs      =  no()
    when abbn('EMSGs')             then emsg      =  no()
    when abbn('SPLit')             then split     =  no()
    when abbn('Quiet')             then quiet     =  no()
    when abbn('SHOWcodes')         then show      =  no()
    when abbn('SOUnds')|,
         abbn('BEEPs')             then sound     =  no()
    when abbn('SLIce')             then slice     =  no()
    when  abb('BETWEENs')          then betweens  = nai()
    when  abb('DAHs')|,
          abb('DASHs')|,
          abb('DASHes')            then dah       =  na()
    when  abb('DITs')|,
          abb('DOTs')              then dit       =  na()
    when  abb('LONGs')             then long      =  na()
    when  abb('LONGERs')           then longer    =  na()
    when  abb('SPAces')            then spaces    = nai()
    when  abb('SPREADs')           then spread    = nai()
    when  abb('AMERican')      |,
          abb('INTERnational') |,
          abb('RAILroads')     |,
          abb('RAILways')      |,
          abb('RRs')           |,
          abb('USa')               then morseType = _1
    otherwise                      call er 55,_o
    end     /*select*/
  end       /*while ops\==''*/

if betweens<0 | betweens>sw  then call er 81,0 sw betweens 'betweens'
if spaces<0   | spaces>sw    then call er 81,0 sw spaces 'spaces'
if spread<0   | spread>sw    then call er 81,0 sw spread 'spread'

w=words(plainText)
if w==0 & emsg & \show  then call er 35,'( plain-text'
dah= verchar(dah, 'DAH')                         /*verify   dah   Morse code character. */
dit= verchar(dit, 'DIT')                         /*   "     dit     "     "      "      */
if morseType\=='I'  then morseType="R"           /*Not internatioal?   Assume railroad. */
morseType= '0'morseType                          /*construct the  "type"  of Morse code.*/
between_ = copies(' ',betweens)                  /*construct number of "between" blanks.*/
if logs    then tops = '.F='gettfid(,"ANS") tops
if colors  then tops = '.C=green' tops           /*add color  (if any)  to  TOPS.       */
tops= space(tops)                                /*get rid of extraneous blanks.        */
_ = dah
@ = dit
if clear & \quiet  then !cls                     /*should we clear the terminal screen? */

                 /*┌───────────────────────────────────────────────────────────────────┐
                   │              1         2         3         4         5            │
                   │     123456789012345678901234567890123456789012345678901234        │
                   │     ABCDEFGHIJKLMNOPQRSTUVWZYZ0123456789':,-(.?;/_$!)=@&"+        │
                   └───────────────────────────────────────────────────────────────────┘*/
@chars = @abcu || "0123456789':,-(.?;/_$!)=@&""+"  /*penultimate char is quotation char.*/
$.=                                              /*default value  (null)  for all chars.*/
$.0i.1  = mc(@ _)                                /* A  Latin letter                     */
$.0i.2  = mc(_ @ @ @)                            /* B  Latin letter                     */
$.0i.3  = mc(_ @ _ @)                            /* C  Latin letter                     */
$.0i.4  = mc(_ @ @)                              /* D  Latin letter                     */
$.0i.5  = mc(@)                                  /* E  Latin letter                     */
$.0i.6  = mc(@ @ _ @)                            /* F  Latin letter                     */
$.0i.7  = mc(_ _ @)                              /* G  Latin letter                     */
$.0i.8  = mc(@ @ @ @)                            /* H  Latin letter                     */
$.0i.9  = mc(@ @)                                /* I  Latin letter                     */
$.0i.10 = mc(@ _ _ _)                            /* J  Latin letter                     */
$.0i.11 = mc(_ @ _)                              /* K  Latin letter                     */
$.0i.12 = mc(@ _ @ @)                            /* L  Latin letter                     */
$.0i.13 = mc(_ _)                                /* M  Latin letter                     */
$.0i.14 = mc(_ @)                                /* N  Latin letter                     */
$.0i.15 = mc(_ _ _)                              /* O  Latin letter                     */
$.0i.16 = mc(@ _ _ @)                            /* P  Latin letter                     */
$.0i.17 = mc(_ _ @ _)                            /* Q  Latin letter                     */
$.0i.18 = mc(@ _ @)                              /* R  Latin letter                     */
$.0i.19 = mc(@ @ @)                              /* S  Latin letter                     */
$.0i.20 = mc(_)                                  /* T  Latin letter                     */
$.0i.21 = mc(@ @ _)                              /* U  Latin letter                     */
$.0i.22 = mc(@ @ @ _)                            /* V  Latin letter                     */
$.0i.23 = mc(@ _ _)                              /* W  Latin letter                     */
$.0i.24 = mc(_ @ @ _)                            /* X  Latin letter                     */
$.0i.25 = mc(_ @ _ _)                            /* Y  Latin letter                     */
$.0i.26 = mc(_ _ @ @)                            /* Z  Latin letter                     */
$.0i.27 = mc(_ _ _ _ _)                          /* 0  decimal digit                    */
$.0i.28 = mc(@ _ _ _ _)                          /* 1  decimal digit                    */
$.0i.29 = mc(@ @ _ _ _)                          /* 2  decimal digit                    */
$.0i.30 = mc(@ @ @ _ _)                          /* 3  decimal digit                    */
$.0i.31 = mc(@ @ @ @ _)                          /* 4  decimal digit                    */
$.0i.32 = mc(@ @ @ @ @)                          /* 5  decimal digit                    */
$.0i.33 = mc(_ @ @ @ @)                          /* 6  decimal digit                    */
$.0i.34 = mc(_ _ @ @ @)                          /* 7  decimal digit                    */
$.0i.35 = mc(_ _ _ @ @)                          /* 8  decimal digit                    */
$.0i.36 = mc(_ _ _ _ @)                          /* 9  decimal digit                    */
$.0i.37 = mc(@ _ _ _ _ @)                        /* ' apostrophe                        */
$.0i.38 = mc(_ _ _ @ @ @)                        /* : colon                             */
$.0i.39 = mc(_ _ @ @ _ _)                        /* , comma                             */
$.0i.40 = mc(_ @ @ @ @ _)                        /* - minus or hyphen                   */
$.0i.41 = mc(_ @ _ _ @ _)                        /* ( left parenthesis                  */
$.0i.42 = mc(@ _ @ _ @ _)                        /* . period                            */
$.0i.43 = mc(@ @ _ _ @ @)                        /* ? question mark                     */
$.0i.44 = mc(_ @ _ @ _ @)                        /* ; semi-colon                        */
$.0i.45 = mc(_ @ @ _ @)                          /* / slash                             */
$.0i.46 = mc(@ @ _ _ @ _)                        /* _ underscrore                       */
$.0i.47 = mc(@ @ @ _ @ @ _)                      /* $ dollar sign                       */
$.0i.48 = mc(@ _ @ _ @ @)                        /* ! exclamation mark                  */
$.0i.49 = mc(_ _ _ @ @)                          /* ) right parenthesis                 */
$.0i.50 = mc(_ @ @ @ _)                          /* = equal sign                        */
$.0i.51 = mc(@ _ _ @ _ @)                        /* @ comercial at                      */
$.0i.52 = mc(_ _ _ _ @)                          /* & ampersand                         */
$.0i.53 = mc(@ _ @ @ _ @)                        /* " double-quote                      */
$.0i.54 = mc(@ _ @ _ @)                          /* + plus sign                         */

    do j=1  for length(@chars)
    $.0r.j=$.0i.j
    end   /*j*/                                  /*use International code for most chars*/

$.0r.3  = mc(@ @ pause_ @)                       /* C  Latin letter,       railroad code*/
$.0r.6  = mc(@ long_ @)                          /* F  Latin letter,       railroad code*/
$.0r.10 = mc(_ @ _ @)                            /* J  Latin letter,       railroad code*/
$.0r.12 = mc(long_)                              /* L  Latin letter,       railroad code*/
$.0r.15 = mc(@ pause_ @)                         /* O  Latin letter,       railroad code*/
$.0r.16 = mc(@ @ @ @ @)                          /* P  Latin letter,       railroad code*/
$.0r.17 = mc(@ @ long_ @)                        /* Q  Latin letter,       railroad code*/
$.0r.18 = mc(@ pause_ @ @)                       /* R  Latin letter,       railroad code*/
$.0r.24 = mc(@ long_ @ @)                        /* X  Latin letter,       railroad code*/
$.0r.25 = mc(@ @ pause_ @ @)                     /* Y  Latin letter,       railroad code*/
$.0r.26 = mc(@ @ @ pause_ @)                     /* Z  Latin letter,       railroad code*/
$.0r.27 = mc(longer_)                            /* 0  decimal digit,      railroad code*/
$.0r.28 = mc(@ _ _ @)                            /* 1  decimal digit,      railroad code*/
$.0r.29 = mc(@ @ long_ @ @)                      /* 2  decimal digit,      railroad code*/
$.0r.30 = mc(@ @ @ long_ @)                      /* 3  decimal digit,      railroad code*/
$.0r.32 = mc(_ _ _)                              /* 5  decimal digit,      railroad code*/
$.0r.33 = mc(@ @ @ @ @ @)                        /* 6  decimal digit,      railroad code*/
$.0r.34 = mc(_ _ @ @)                            /* 7  decimal digit,      railroad code*/
$.0r.35 = mc(_ @ @ @ @)                          /* 8  decimal digit,      railroad code*/
$.0r.36 = mc(_ @ @ _)                            /* 9  decimal digit,      railroad code*/
$.0r.39 = mc(@ _ @ _)                            /* ,  a comma,            railroad code*/
$.0r.42 = mc(@ @ _ _ @ @)                        /* .  a period,           railroad code*/
$.0r.43 = mc(_ @ @ _ @)                          /* ?  a question mark,    railroad code*/
$.0r.48 = mc(_ _ _ @)                            /* ! an exclamation mark, railroad code*/
$.0r.52 = mc(@ _ @ @ @)                          /* & an ampersand,        railroad code*/

if show  then do jshow=1  for length(@chars)
              call tt  substr(@chars, jshow,1)  $.0i.jshow
              end   /*jshow*/

from= '{}[]<>`\─'                                /*some characters not in Morse code.   */
into= "()()()'/─"                                /*translate characters ──> Morse code. */

newText= translate(plainText, into, from)        /*translate some other characters.     */
newText= space(newText, spaces)                  /*elide extraneous spaces  (blanks)    */
spread_= copies(_b2,    spread)                  /*construct the spread.                */
spaces_= copies(_b3,    spaces)                  /*    "      "  spaces.                */

  do jw=1  for w
  aword=word(newText, jw)
  if code\==''  then  code= code || spaces_
  pb=
       do jc=1  for length(aword)
       ?= substr(aword, jc, 1)
       idx= pos(?,@chars)
       if idx==0  then code=code || pb || ?
                  else code=code || pb || $.morseType.idx
       pb=spread_
       end   /*jc*/
  end        /*jw*/

if split  then code= translate(code, , _b3)
if slice  then code= translate(code, , _b2)

                        do jg=1  for words(code)
                        if jg\==1 & sound  then call $tq  ".Z="delaySpace
                        glyph = word(code,jg)
                        cglyph= changestr(long_  ,  glyph, long)
                        cglyph= changestr(longer_, cglyph, longer)
                        call tt cglyph
                        call ss  glyph
                        end   /*jg*/
return unpsu(code)

/*──────────────────────────────────────────────────────────────────────────────────────*/
ss: if \sound  then return
    _s= unpsu( arg(1) )
                            do js=1  for length(_s)
                            _c=substr(_s, js, 1)
                            if _c==' '      then call $tq  ".Z="delaySpace
                            if _c==dit      then call $tq  ".B=1 .BF="bf ".BD="ditTime
                            if _c==dah      then call $tq  ".B=1 .BF="bf ".BD="dahTime
                            if _c==long_    then call $tq  ".B=1 .BF="bf ".BD="longTime
                            if _c==longer_  then call $tq  ".B=1 .BF="bf ".BD="longerTime
                                                 call $tq  ".Z="TimeDelay
                            end   /*js*/
    return

/*─────────────────────────────general 1─line subs──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
!all:  !!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=='NT';!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,'? ?SAMPLES ?AUTHOR ?FLOW')==0 then return 0;!call=']$H';call '$H' !fn !;!call=;return 1
!cal:  if symbol('!CALL')\=="VAR" then !call=; return !call
!env:  !env='ENVIRONMENT'; if !sys=='MSDOS'|!brexx|!r4|!roo then !env='SYSTEM'; if !os2 then !env='OS2'!env; !ebcdic=3=='f3'x; if !crx then !env='DOS'; return
!fid:  parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .; call !sys; if !dos then do; _=lastpos('\',!fn); !fm=left(!fn,_); !fn=substr(!fn,_+1); parse var !fn !fn '.' !ft; end; return word(0 !fn !ft !fm,1+('0'arg(1)))
!rex:  parse upper version !ver !vernum !verdate .; !brexx='BY'==!vernum; !kexx='KEXX'==!ver; !pcrexx='REXX/PERSONAL'==!ver|'REXX/PC'==!ver; !r4='REXX-R4'==!ver; !regina='REXX-REGINA'==left(!ver,11); !roo='REXX-ROO'==!ver; call !env; return
!sys:  !cms=!sys=='CMS'; !os2=!sys=='OS2'; !tso=!sys=='TSO'|!sys=='MVS'; !vse=!sys=='VSE'; !dos=pos('DOS',!sys)\==0|pos('WIN',!sys)\==0|!sys=='CMD'; !crx=left(!sys,6)=='DOSCRX'; call !rex; return
!var:  call !fid; if !kexx then return space(dosenv(arg(1))); return space(value(arg(1),,!env))
$fact!: procedure; parse arg x _ .; l=length(x); n=l-length(strip(x,'T',"!")); if n<=-n|_\==''|arg()\==1 then return x; z=left(x,l-n); if z<0|\isint(z) then return x; return $fact(z,n)
$fact!: procedure; parse arg x; l=length(x); n=l-length(strip(x,'T',"!")); z=left(x,l-n); if z<0|\datatype(z,'W') then return x; !=1; if n==1 then return $fact(z); do j=z to 2 by -n; !=!*j; end; return !
$fact:  procedure; parse arg x _ .; arg ,n ! .; n=p(n 1); if \isint(n) then n=0; if x<-n|\isint(x)|n<1|_||!\==''|arg()>2 then return x||copies("!",max(1,n)); !=1; s=x//n; if s==0 then s=n; do j=s to x by n; !=!*j; end; return !
$sfxa:  parse arg ,s,m; arg u,c; if pos(left(s,2),u)\==0 then do j=length(s) to compare(s,c)-1 by -1; if right(u,j)\==left(c,j) then iterate; _=left(u,length(u)-j); if isnum(_) then return m*_; leave; end; return arg(1)
$sfxf:  parse arg y; if right(y,1)=='!' then y=$fact!(y); if \isnum(y) then y=$sfxz(); if isnum(y) then return y; return $sfxm(y)
$sfxm:  parse arg z; arg w; b=1000; if right(w,1)=='I' then do; z=shorten(z); w=z; upper w; b=1024; end; p=pos(right(w,1),'KMGTPEZYXWVU'); if p==0 then return arg(1); n=shorten(z); r=num(n,f,1); if isnum(r) then return r*b**p; return arg(1)
$sfxz:  return $sfxa($sfxa($sfxa($sfxa($sfxa($sfxa(y,'PAIRs',2),'DOZens',12),'SCore',20),'GREATGRoss',1728),'GRoss',144),'GOOGOLs',1e100)
$t:     !call=']$T';   call "$T" tops arg(1);   !call=;   return
$tq:    call $t '.Q=1' arg(1);                          return
abb:    arg abbu; parse arg abb; return abbrev(abbu,_,abbl(abb))
abbl:   return verify(arg(1)'a',@abc,'M')-1
abbn:   parse arg abbn; return abb(abbn)|abb('NO'abbn)
er:     parse arg _1,_2; call '$ERR' "14"p(_1) p(word(_1,2) !fid(1)) _2; if _1<0 then return _1; exit result
err:    call er '-'arg(1),arg(2); return ''
erx:    call er '-'arg(1),arg(2); exit ''
getdtfid:tfid=p(!var("TMP") !var('TEMP') homedrive()"\"); if substr(tfid,2,1)==':' & substr(tfid,3,1)\=="\" then tfid=insert('\',t,2); return strip(tfid,'T',"\")'\'arg(1)'.'arg(2)
getTFID:if symbol('TFID')=='LIT' then tfid=; if tfid\=='' then return tfid; gfn=word(arg(1) !fn,1); gft=word(arg(2) 'ANS',1); tfid='TEMP'; if !tso then tfid=gfn'.'gft; if !cms then tfid=gfn','gft",A4"; if !dos then tfid=getdTFID(gfn,gft); return tfid
halt:   call er .1
homedrive:if symbol('HOMEDRIVE')\=="VAR" then homedrive=p(!var('HOMEDRIVE') 'C:'); return homedrive
int:    int=num(arg(1),arg(2)); if \isint(int) then call er 92,arg(1) arg(2); return int/1
ishex:  return datatype(arg(1),'X')
isint:  return datatype(arg(1),'W')
isnum:  return datatype(arg(1),'N')
mc:     return translate(space(arg(1),betweens),_b1,' ')
na:     if arg(1)\=='' then call er 01,arg(2); parse var ops na ops; if na=='' then call er 35,_o; return na
nai:    return int(na(),_o)
nan:    return num(na(),_o)
no:     if arg(1)\=='' then call er 01,arg(2); return left(_,2)\=='NO'
noValue:!sigl=sigl; call er 17,!fid(2) !fid(3) !sigl condition('D') sourceline(!sigl)
num:    procedure; parse arg x .,f,q; if x=='' then return x; if isnum(x) then return x/1; x=space(translate(x,,','),0); if \isnum(x) then x=$sfxf(x); if isnum(x) then return x/1; if q==1 then return x; if q=='' then call er 53,x f; call erx 53,x f
p:      return word(arg(1),1)
shorten:procedure; parse arg a,n; return left(a,max(0,length(a)-p(n 1)))
syntax: !sigl=sigl; call er 13,!fid(2) !fid(3) !sigl !cal() condition('D') sourceline(!sigl)
tell:   if tops==''  then say arg(1);  else call $t tops arg(1);      return
tt:     if \quiet then call tell unpsu(arg(1)); return
unpsu:  return translate(arg(1), , _b1 || _b2 || _b3)
verchar:procedure; parse arg y,w; _=length(y); if _==1 then return y; if _==2 then do; if \ishex(y) then call er 40,y w; return x2c(y); end; if _==3 then do; if \int(y) then call er 92,y ',' w; return d2c(y); end; call er 55,y w
```


[[Category:REXX library routines]]
