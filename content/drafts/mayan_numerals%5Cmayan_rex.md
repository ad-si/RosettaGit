+++
title = "Mayan numerals $MAYAN.REX"
description = ""
date = 2019-01-14T04:55:51Z
aliases = []
[extra]
id = 22134
[taxonomies]
categories = []
tags = []
+++

This is the full-blown REXX program that is used by the '''REXX''' program entry for the Rosetta Code task:   '''Mayan numerals'''.


## REXX


```rexx
/*REXX program converts decimal numbers to the  Mayan system  (with/without cartouches).*/

trace off                                        /*turn   off   all REXX tracing.       */

parse arg !                                      /*obtain arguments from the C.L.       */
if !all(arg())  then exit                        /*if help/flow/sample/author then exit.*/
if !cms         then address ''                  /*Running under CMS?  Then set ADDRESS.*/

signal on halt                                   /*be able to handle  HALT signal.      */
signal on noValue                                /*trap  REXX  NOVALUE  errors.         */
signal on syntax                                 /*  "     "   SYNTAX      "            */

numeric digits 1000                              /*allow use of gihugeic numbers.       */
@abc= 'abcdefghijklmnopqrstuvwxyz'               /*used for option abbreviations.       */
@abcU= @abc;     upper @abcU                     /*  "   "     "        "               */
bar= '-'                                         /*default character for a "bar".       */
boxed= 1                                         /*placeholder:  CARTOUCHE option.      */
cartouche= boxed                                 /*    "              "       "         */
colors= !cms | !pcrexx | !r4 | !roo              /*placeholder:  COLORS    option.      */
dot= .                                           /*default glyph for a   "dot"          */
egg= 'o'                                         /*    "     "    "  an  "egg"          */
mayan= 1                                         /*placeholder:  MAYAN     option.      */
quiet= 0                                         /*placeholder:  QUIET     option.      */
simple= 0                                        /*placeholder:  SIMPLE    option.      */
single= 0                                        /*placeholder:  SINGLE    option.      */
spacing= 1                                       /*placeholder:  SPACING   option.      */
overlap= 0                                       /*placeholder:  OVERLAP   option.      */
tfid=                                            /*temporary file identity, so far.     */
tops=                                            /*options for the  $T  program.        */

if !dos  then do                                 /*DOS?  Then use chars for CP 437.     */
              dot= 'f9'x                         /*utilize the glyph for a bullet.      */
              egg= 'e9'x                         /*   "     "    "    "  an  egg.       */
              end

if !cms  then dot= 'af'x                         /*CMS?  Then use a bullet for dot.     */

parse var  !!   yyy   _   .   '('   ops   ")"    /*separate options from arguments.     */
if _\==''  then call er 59                       /*too many arguments specified?        */
ops= space(ops)                                  /*remove superfluous OPS blanks.       */

  do  while  ops\==''                            /*process all specified options.       */
  parse var  ops  _1  2  1  _  .  1  _o  ops     /*obtain various parts of options.     */
  upper _                                        /*uppercase the   _   variable.        */
    select
    when _==','                     then nop
    when _1==. & pos("=", _)\==0    then tops= tops _o
    when abbn('ARABic'      )       then mayan= \no()
    when abbn('BOXed'       )       then boxed= no()
    when abbn('CARTouches'  )       then cartouche= no()
    when abbn('COLORs'      )       then colors= no()
    when abbn('DOUBle'      )       then single= \no()
    when abbn('MAYAn'       )       then mayan= no()
    when abbn('OVerlappings') |,
         abbn('OVerlapped'  ) |,
         abbn('OVerlaps'    )       then overlap= no()
    when abbn('Quiet'       )       then quiet= no()
    when abbn('SIMPle'      )       then simple= no()
    when abbn('SINgle'      )       then single= no()
    when  abb('SPacings'    ) |,
          abb('SPaces'      )       then spacing= nai()
    when  abb('UNBOXed'     ) |,
          abb('NOTBOXed'    )       then boxed= \no()
    otherwise                  call er 55,_o
    end   /*select*/
  end     /*while ops¬==''*/

if yyy==''     then call er 54                   /*No number specified? Oops.           */
if spacing<0   then call er 02,spacing 'SPACING' /*SPACING was negative.                */
if overlap     then spacing= 0                   /*force spacing=0  if  overlap  option.*/

oyyy= yyy                                        /*save original number.                */
yyy= num(yyy)                                    /*perform any conversions.             */

if colors  then tops= '.C=green' tops            /*if COLORS, then use green.           */

tops= space(tops)                                /*remove superfluous blanks.*/
_= left(yyy, 1)                                  /*process (any) leading sign*/
sig=                                             /*placeholder for the sign. */

if _=='-'  then sig= "minus "                    /*Leading  '-'?    Then use a   minus. */
if _=="+"  then sig= "plus "                     /*   "     '+'?      "   "  "   plus.  */

if sig\==''  then yyy=substr(yyy,2)              /*Has a sign?  Use  ABS  of the #.     */
#=

  do pow=0  until  20**(pow + 1) > yyy           /*find highest power of twenty.        */
  end   /*pow*/                                  /*  ··· only go up to  YYY.            */

  do pow=pow  by -1  to 0                        /*convert the number (#) to base twenty*/
  _= 20 ** pow                                   /*essentially, use a power of twenty.  */
  #= # yyy % _                                   /*build one numeral at a time.         */
  yyy= yyy // _                                  /*now, process the rest of number (#). */
  end   /*pow*/                                  /*Mayan ≡ essentially base twenty.     */

if pow==0  then #=# yyy                          /*add residual numeral if zero.        */
#= substr(#,2)                                   /*pick off the number (#) sans the sign*/
$.=                                              /*placeholder for the output.          */
$= #                                             /*placeholder for cartouche.           */
if mayan   then call $mayan                      /*MAYAN option on?  Show it.           */
#= sig || $                                      /*prefix the sign  (if any).           */

if cartouche & mayan  then do j=1  for 6         /*show cartouche if required.          */
                           call tell  substr($.j,  2 - boxed)
                           end   /*j*/
                      else call tell $           /*display Arabic numbers.              */
return #


/*──────────────────────────────────────────────────────────────────────────────────────*/
$mayan: $=                                       /*nullify the dollar string.           */
y= #                                             /*Y:  decimal number to Mayan numerals.*/
                    do  until  y=''              /*parse each dig for a Mayan numeral.  */
                    parse var  y  _  y           /*get the next Mayan digit (numeral).  */
                    $= $  $maydig(_)             /*append glyph to the  $  string.      */
                    end   /*until*/              /*now, $ has a leading blank.          */
$   = strip($)                                   /*remove any extra blanks.             */
b.1 = '╔════╗'                                   /*define the  cartouche  top.          */
b.6 = '╚════╝'                                   /*   "    "       "     bottom.        */
aa  = '║'                                        /*   "    "       "      sides.        */
dble= '═║╔╗╚╝╦╩'                                 /*use these for double box.            */
sing= '─│┌┐└┘@@'                                 /* "    "    "  single  "              */
simp= '-|++++++'                                 /* "    "    "  simple  "              */
sbar= substr(dble, 2, 1)                         /*define  single   side characters.    */
dbar= sbar || sbar                               /*   "    double     "      "          */
ttee= substr(dble, 7, 1)                         /*   "    top tee           "          */
btee= substr(dble, 8, 1)                         /*   "    bot tee           "          */

  do jw=1  for words($)  while  cartouche        /*enclose the Mayan # in a cartouche.  */
  call $maybox word($, jw)                       /* [↑]  if option is "on".             */
  end   /*jw*/

    do j=1  for 6  while overlap                 /*handle  OVERLAP  option.             */
    $.j= left($.j, 1)  ||  changestr( left(b.1, 1) , substr($.j, 2),  '')
    $.j= left($.j, 1)  ||  changestr( left(b.6, 1) , substr($.j, 2),  '')

    $.j= changestr( dbar         , $.j, sbar)    /*alter if using OVERLAP.              */
    $.j= changestr( right(b.1, 1), $.j, ttee)    /*  "    "   "      "                  */
    $.j= changestr( right(b.6, 1), $.j, btee)    /*  "    "   "      "                  */

    $.j= reverse($.j)                            /*backwards for simplicity.            */
    $.j= changestr( ttee, left($.j, 1), right(b.1, 1))  ||  substr($.j, 2)
    $.j= changestr( btee, left($.j, 1), right(b.6, 1))  ||  substr($.j, 2)
    $.j= reverse($.j)                            /*OK now,  let's  reverse  the reverse.*/
    end   /*j*/
                                                 /* [↓]  choose type of display for #.  */
  do j=1  for 6  while \boxed;   $.j=translate($.j,             ,         dble); end
  do j=1  for 6  while simple;   $.j=translate($.j, simp || simp, sing || dble); end
  do j=1  for 6  while single;   $.j=translate($.j, sing        ,         dble); end

return $                                         /*return the  $  string to the invoker.*/


/*──────────────────────────────────────────────────────────────────────────────────────*/
$maybox:  parse arg q
            do j=6  to 1  by -1
            if j>5 | j<2  then do
                               $.j= $.j || left('', spacing)b.j
                               iterate
                               end

            if right(q,1)=='-'  then do
                                     $.j= $.j || left('', spacing)aa"────"aa
                                     q= left(q, length(q) - 1)
                                     end
                                else do
                                     $.j= $.j || left('', spacing)aa || centre(q,4)aa
                                     q=
                                     end
            end   /*j*/

         return


/*══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════*/
!all:  !!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=="NT";!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,"? ?SAMPLES ?AUTHOR ?FLOW")==0 then return 0;!call=']$H';call "$H" !fn !;!call=;return 1
!cal:  if symbol('!CALL')\=="VAR"  then !call=;  return !call
!env:  !env='ENVIRONMENT';  if !sys=="MSDOS" | !brexx | !r4 | !roo  then !env='SYSTEM';  if !os2  then !env="OS2"!env;  !ebcdic=3=='f3'x;  if !crx  then !env="DOS";  return
!fid:  parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .;  call !sys;  if !dos  then do;  _=lastpos('\', !fn);  !fm=left(!fn, _);  !fn=substr(!fn, _ + 1);  parse var !fn !fn "." !ft;  end;           return word(0 !fn !ft !fm, 1 + ('0'arg(1) ) )
!rex:  parse upper version !ver !vernum !verdate .;  !brexx='BY'==!vernum;  !kexx="KEXX"==!ver;  !pcrexx='REXX/PERSONAL'==!ver | "REXX/PC"==!ver;  !r4='REXX-R4'==!ver;  !regina="REXX-REGINA"==left(!ver,11);  !roo='REXX-ROO'==!ver;  call !env;  return
!sys:  !cms=!sys=='CMS';  !os2=!sys=="OS2";  !tso=!sys=='TSO' | !sys=="MVS";  !vse=!sys=='VSE';  !dos=pos("DOS", !sys)\==0 | pos('WIN', !sys)\==0 | !sys=="CMD";  !crx=left(!sys, 6)=='DOSCRX';  call !rex;      return
!var:  call !fid;  if !kexx  then return space(dosenv(arg(1)));  return space(value(arg(1),,!env))
$fact!:   procedure; parse arg x _ .; l=length(x); n=l - length( strip(x, 'T', "!"));  if n<=-n | _\=='' | arg()\==1  then return x;  z=left(x, l - n);  if z<0 | \isInt(z)  then return x;                      return $fact(z, n)
$fact:    procedure; parse arg x _ .; arg ,n ! .; n=p(n 1); if \isInt(n)  then n=0; if x<-n | \isInt(x) | n<1 | _ || !\=='' | arg()>2  then return x || copies("!", max(1, n)); !=1; s=x//n; if s==0  then s=n;  do j=s  to x  by n; !=!*j; end;  return !
$maydig:  parse arg q;  return copies(dot,q//5)copies(bar,q%5)substr(egg,q+1)
$sfxa:    parse arg ou,s,m;  arg u,c;  if pos( left(s, 2), u)\==0  then do j=length(s)  to compare(s, c) - 1  by -1;  if right(u, j)\==left(c, j)  then iterate;  _=left(u, length(u) - j);  if isNum(_)  then return m * _;  leave;  end;       return ou
$sfxf:    parse arg y;  if right(y,1)=='!'  then y=$fact!(y);  if \isNum(y)  then y=$sfxz();  if isNum(y)  then return y;  return $sfxm(y)
$sfxm:    parse arg z 1 oz; arg w; b=1000; if right(w, 1)=='I'  then do; z=shorten(z); w=z; upper w; b=1024; end;  p=pos(right(w,1), "KMGTPEZYXWVU"); if p==0  then return arg(1); n=shorten(z); r=num(n,f,1); if isNum(r)  then return r*b**p;  return oz
$sfxz:    return $sfxa( $sfxa( $sfxa( $sfxa( $sfxa( $sfxa(y, 'PAIRs', 2), "DOZens", 12), 'SCore', 20), "GREATGRoss", 1728), 'GRoss', 144), "GOOGOLs", 1e100)
$t:       !call=']$T';  call "$T" arg(1);  !call=;                         return
abb:      arg abbu;  parse arg abb;  return abbrev( abbu, _, abbl(abb) )
abbl:     return verify( arg(1)'a', @abc, "M") - 1
abbn:     parse arg abbn;  return abb(abbn) | abb('NO'abbn)
er:       parse arg _1,_2;  call '$ERR' "14"p(_1) p(word(_1,2) !fid(1)) _2;  if _1<0  then return _1;  exit result
err:      call er '-'arg(1), arg(2);     return ''
erx:      call er '-'arg(1), arg(2);     exit ''
halt:     call er .1
int:      int=num( arg(1), arg(2) );  if \isInt(int)  then call er 92,arg(1) arg(2);   return int/1
isInt:    return datatype( arg(1), 'W')
isNum:    return datatype( arg(1), 'N')
na:       if arg(1)\==''  then call er 01,arg(2);  parse var ops na ops;  if na==""  then call er 35,_o;  return na
nai:      return int(na(),_o)
nan:      return num(na(),_o)
no:       if arg(1)\==''  then call er 01,arg(2);            return left(_, 2) \== "NO"
noValue:  !sigl=sigl;  call er 17, !fid(2) !fid(3) !sigl condition('D') sourceline(!sigl)
num:      procedure; parse arg x .,f,q; if x=='' then return x; if isNum(x) then return x/1; x=space(translate(x,,","),0); if \isNum(x) then x=$sfxf(x); if isNum(x) then return x/1; if q==1 then return x; if q=='' then call er 53,x f; call erx 53,x f
p:        return word( arg(1), 1)
s:        if arg(1)==1  then return arg(3);  return word( arg(2) 's', 1)
shorten:  procedure;  parse arg a,n;  return left(a, max(0, length(a) - p(n 1) ) )
syntax:   !sigl=sigl;  call er 13, !fid(2) !fid(3) !sigl !cal() condition('D') sourceline(!sigl)
tell:     if quiet  then return;  if tops==''  then say arg(1);  else call $t tops arg(1);  return
```
.





The following is the help file for the   '''$MAYAN.REX'''   REXX program:

```txt

The  $MAYAN  command will display an integer  in Mayan numerals (symbols),  or
in Arabic numbers,  without or in a cartouche,  with/without boxes.

The Pre-Columbia Mayan civilization used a base twenty system (also known as a
vigesimal numbering system), and are usually depicted in Maya codices.  Because
codices are hard to draw in ASCII text, they are shown within a box to identify
significant digits (positions of the numerals).  The three Mayan numerals are:

         zero   ( Θ    an egg shape and sometimes likes more like an eye)
         one    ( ∙    a dot)
         five   (────  a bar)

Here are some numbers represented in Mayan numerals with their Arabic #s on top:


  0        1        2        3        4        5        6        7        8
╔════╗   ╔════╗   ╔════╗   ╔════╗   ╔════╗   ╔════╗   ╔════╗   ╔════╗   ╔════╗
║    ║   ║    ║   ║    ║   ║    ║   ║    ║   ║    ║   ║    ║   ║    ║   ║    ║
║    ║   ║    ║   ║    ║   ║    ║   ║    ║   ║    ║   ║    ║   ║    ║   ║    ║
║    ║   ║    ║   ║    ║   ║    ║   ║    ║   ║    ║   ║ ∙  ║   ║ ∙∙ ║   ║∙∙∙ ║
║ Θ  ║   ║ ∙  ║   ║ ∙∙ ║   ║∙∙∙ ║   ║∙∙∙∙║   ║────║   ║────║   ║────║   ║────║
╚════╝   ╚════╝   ╚════╝   ╚════╝   ╚════╝   ╚════╝   ╚════╝   ╚════╝   ╚════╝



  9        10       11       12       13       14       15       16       17
╔════╗   ╔════╗   ╔════╗   ╔════╗   ╔════╗   ╔════╗   ╔════╗   ╔════╗   ╔════╗
║    ║   ║    ║   ║    ║   ║    ║   ║    ║   ║    ║   ║    ║   ║ ∙  ║   ║ ∙∙ ║
║    ║   ║    ║   ║ ∙  ║   ║ ∙∙ ║   ║∙∙∙ ║   ║∙∙∙∙║   ║────║   ║────║   ║────║
║∙∙∙∙║   ║────║   ║────║   ║────║   ║────║   ║────║   ║────║   ║────║   ║────║
║────║   ║────║   ║────║   ║────║   ║────║   ║────║   ║────║   ║────║   ║────║
╚════╝   ╚════╝   ╚════╝   ╚════╝   ╚════╝   ╚════╝   ╚════╝   ╚════╝   ╚════╝



  18       19                 2 0                          4 0 0
╔════╗   ╔════╗          ╔════╗ ╔════╗              ╔════╗ ╔════╗ ╔════╗
║∙∙∙ ║   ║∙∙∙∙║          ║    ║ ║    ║              ║    ║ ║    ║ ║    ║
║────║   ║────║          ║    ║ ║    ║              ║    ║ ║    ║ ║    ║
║────║   ║────║          ║    ║ ║    ║              ║    ║ ║    ║ ║    ║
║────║   ║────║          ║ ∙  ║ ║ Θ  ║              ║ ∙  ║ ║ Θ  ║ ║ Θ  ║
╚════╝   ╚════╝          ╚════╝ ╚════╝              ╚════╝ ╚════╝ ╚════╝



          8 0 0 0                                        1 6 0 0 0 0
╔════╗ ╔════╗ ╔════╗ ╔════╗                   ╔════╗ ╔════╗ ╔════╗ ╔════╗ ╔════╗
║    ║ ║    ║ ║    ║ ║    ║                   ║    ║ ║    ║ ║    ║ ║    ║ ║    ║
║    ║ ║    ║ ║    ║ ║    ║                   ║    ║ ║    ║ ║    ║ ║    ║ ║    ║
║    ║ ║    ║ ║    ║ ║    ║                   ║    ║ ║    ║ ║    ║ ║    ║ ║    ║
║ ∙  ║ ║ Θ  ║ ║ Θ  ║ ║ Θ  ║                   ║ ∙  ║ ║ Θ  ║ ║ Θ  ║ ║ Θ  ║ ║ Θ  ║
╚════╝ ╚════╝ ╚════╝ ╚════╝                   ╚════╝ ╚════╝ ╚════╝ ╚════╝ ╚════╝



Other than the bar and dot notation, Maya numerals can be illustrated by a
human/diety face type glyphs.  The face glyph for a number represents the diety
associated with the number.   These face number glyphs were rarely used, and
are mostly seen only on some of the most elaborate monumental carvings.

╔══════════════════════════════════════════════════════════════════════════════╗
║                                                                              ║
║                         {COLORs | NOCOLORs}                                  ║
║  $MAYAN    nnn      (   {MAYAn | ARABic}                            {)}      ║
║                         {CARTouche | NOCARTouche}                            ║
║                         {BOXed | NOBOXed | UNBOXed | NOTBOXed}               ║
║                         {OVerlapped | NOOVerlapped}                          ║
║            ?            {SIMPle | NOSIMPle}                                  ║
║            ?AUTHOR      {SINGle | DOUBle | NOSINGle | NODOUBle}              ║
║            ?FLOW        {SPacings nnn}                                       ║
║            ?SAMPLES     {Quiet}                                              ║
║                         {tops}                                               ║
║                                                                              ║
╚══════════════════════════════════════════════════════════════════════════════╝

───where:

?          shows this help file              (press  ESC  to quit when viewing).

?AUTHOR    shows the author of this program.

?FLOW      shows the external execution flow of this program.

?SAMPLES   shows some sample uses            (press  ESC  to quit when viewing).

nnn        is the integer in Arabic numerals that is to be converted to Mayan
           numerals.   The number must be a whole number,  and  it may have
           imbedded commas for readability.  Although Mayan numbers didn't have
           (leading) minus or plus signs, they are supported.

COLORs     shows the output in color(s).
           The default is:     (for CMS or DOS):      COLORS
                               (for all others):    NOCOLORS

           The default TOPS is:   .P=1  .A=1  .C=green
           This options can be overridden by specifying  TOPS    (see below).

NOCOLORs   won't show the output in color(s).

MAYAn      converts the integer to Mayan numerals (symbols).
                                                  The default is:  MAYAN

ARABic     converts the integer to Mayan numerals, but uses ARABIC numerals
           instead of Mayan symbols.              The default is:  MAYAN
           The (REXX)  RESULT  is always set to the Mayan numerals in  Arabic.

CARTouche
CARTouch   displays the Mayan numerals (symbols) in a cartouche.
                                                  The default is:  CARTOUCHE

NOCARTouch displays the Mayan numerals as a series of symbols on one line.
                                                  The default is:  CARTOUCHE

BOXed      boxes the Mayan numerals (symbols),  this has only an effect
           if the  CARTOUCHE  option is on.       The default is:  BOXED

NOBOX      displays the Mayan numerals (symbols) horizontaly, without
           boxes.                                 The default is:  BOXED

OVerlapped displays the cartouched Mayan numerals (symbols) with overlapping
           boxes.   The   OVERLAPPED   option is only effective when the
           CARTOUCHE  option is in effect.        The default is:  NOOVERLAPPED

NOOVerlapped   displays the cartouched Mayan numerals (symbols) as seperate
           and distinct cartouches.               The default is:  NOOVERLAPPED

                ╔═══╗
DOUBle     uses ║   ║ for the boxing characters.  The default is:  DOUBLE
                ╚═══╝

                ┌───┐
SINGle     uses │   │ for the boxing characters.  The default is:  DOUBLE
                └───┘

                +---+
SIMPle     uses |   | for the boxing characters.  The default is:  NOSIMPle
                +---+
           SIMPLE overrides  SINGLE and/or DOUBLE.

SPacing nnn    the number of spaces (blanks) between cartouches.   The number
               can be any non-negative integer.   The default is:  1

Quiet      suppresses the showing of any results.   However, the REXX  variable
           RESULT    is always set  (unless there's an error).
           Error messages  (if any)  are always shown.
                                                     The default is:  NOQUIET

NOQuiet    shows the results.

tops       are any or all of the following  $T  .X=xxx options.


────────────────────────────────────────────────────────────────────────────────

Some (but not all) of the  $T  options are:   (issue    $T ?    for more help)

────────  ──────────────────────────────────────────────────────────────────────

.BOX=     draws a box (as shown above) around the message, the default is *NONE*

.A=nnn    types   nnn   blank lines   after  the  message   (in addition to .E=)
          The default is  0.

.P=nnn    types   nnn   blank lines previous to the message (in addition to .E=)
          The default is  0.

.E=nnn    types   nnn   blank lines before and after the message,  they are
          within the box  (if any),   the default is  1.

.I=nnn    indents the message  nnn  spaces,   the default is 0.

.X=nnn    appends   nnn   blanks to the message,   or,   if   nnn   is negative,
          appends  and  prefixes   nnn   blanks to the message.
          The default is  0.

.B=nnn    sets the number of beeps (alarms) before typing the text.
          nnn  can be  0  or a whole number,   the default is  0   (a negative
          number indicates to sound before and after the message).

.C=color  sets the  color  of the message,  the default is  GREEN.

.H=color  sets the highlight color of any parenthesized text.  The default color
          is  YELLOW.

.D=ddd    controls the disposition of the highlighting  (if any),  it can be
          NONE  if no highlighting is wanted,   the default is  HIGHL.

.F=fff    writes the information (in addition to typing it) to the file   fff

.J=kind   justifies  (Left, Right, Center, or Justify)  the text on the screen,
          the default is   None.

.K=ccc    chops the output in several lines, each seperated by the character
          string    ccc    (which is kept at the end of each line of output).

.KD=ccc   same as the  .K=  option, but the character string is deleted from the
          output lines.   The   ccc   can also be specified in hexadecimal with:
          .K='hhhh'X   or   .KD="HHHH"x    where   hh   are hexadecimal pairs of
          hexadecimal digits  (0──►9, a──►f, A──►F).

                                       Ω

```

