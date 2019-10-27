+++
title = "$T.REX"
description = ""
date = 2018-10-15T20:08:13Z
aliases = []
[extra]
id = 12876
[taxonomies]
categories = []
tags = []
+++

==$T.REX==
This is the   '''$T.REX'''   (REXX) program which is used by many other REXX programs to display error or informational message(s),

some of the options are:
::*   in color(s)     (if supported)
::*   highlights (in color) parts (up to 8 unique parts) of the text     (if supported)
::*   write text to a file
::*   breaks the text into multiple lines
::*   adds indentation
::*   justifies the text: left/right/center/justify   (auto-fill)
::*   add blank lines before and/or after the displaying of text
::*   boxing (around) the text
::*   add spacing around the text inside the box
::*   only showing specific lines of the text messages
::*   suppressing specific lines of the text messages
::*   translation of certain characters in the text
::*   allowing other characters to be used for blanks
::*   repeating a text
::*   allows remarks in the text
::*   writes the message, waits for a confirmation to proceed 
::*   delaying (waiting) after the text is displayed 
::*   showing a scale and/or a ruler above/below the text message(s)
::*   supports hex/dec/bit strings
::*   changing the case of the text
::*   reverses the text
::*   inverts the bits for certain characters
::*   sounds alarm (beeps) after the text is displayed     (if supported)
::*   displays the text in reverse video (if supported) 
::*   displays the text in (big) block letters
::*   clear the screen after or before the displaying of text
::*   allows user-define option character, the default is   '''.'''     (period)
::*   and many other options


The help for the   '''$T'''   REXX program is included here   ──►   [[$T.HEL]].


The   '''$T'''   REXX program makes use of   '''$ERR'''   REXX program which is used to display error messages (via   '''$T'''). 

The   '''$ERR'''   REXX program is included here   ──►   [[$ERR.REX]].


The   '''$T'''   REXX program makes use of   '''$BLOCK'''   REXX program which is used to generate text to display text in (big) blocked letters (via   '''$T'''). 

The   '''$BLOCK'''   REXX program is included here   ──►   [[$BLOCK.REX]].


The   '''$T'''   REXX program makes use of   '''LINESIZE'''   BIF   which returns the terminals width (linesize). 

Some REXXes don't have a   '''LINESIZE'''   BIF, so one is included here   ──►   [[LINESIZE.HEL]]. 


The    '''$T'''   REXX program makes use of   '''SCRSIZE'''   BIF which returns the terminals width (linesize) and depth. 

Some REXXes don't have a   '''SCRSIZE'''   BIF, so one is included here   ──►   [[SCRSIZE.HEL]]. 


The   '''$T'''   REXX program makes use of   '''DELAY'''   BIF which delays (sleeps) for a specified amount of seconds. 

Some REXXes don't have a   '''DELAY'''   BIF, so one is included here   ──►   [[DELAY.REX]]. 


The   '''$T'''   REXX program makes use of   '''SOUND'''   BIF which produces sounds via the PC speaker. 

Some REXXes don't have a   '''SOUND'''   BIF, so one is included here   ──►   [[SOUND.REX]]. 


REXX programs not included are   '''$H'''   which shows '''help''' and other documentation.

```rexx
/*REXX*/  trace off                    /* There be many dragons below.  */
parse arg !
if !all(0)  then exit 0                /*help options  and  boilerplate.*/

zz = !!                                /*save a copy of original args.  */
if !cms  then address ''
signal on halt                         /*be able to handle a  HALT.     */
signal on noValue                      /*catch REXX vars with  noValue. */
signal on syntax                       /*catch REXX syntax errors.      */
numeric digits 300                     /*be able to handle some big 'uns*/

hues=space( 'BLACK     0;30',          /*define some colors for DOS.    */
            'BROWN     0;33',
            'DEFAULT   1;37',
            'GRAY      1;37',
            'BLUE      1;34',
            'GREEN     1;32',
            'TURQUOISE 1;36',
            'RED       1;31',
            'PINK      1;35',
            'YELLOW    1;33',
            'WHITE     1;37',
            'BRITE     1;37')          /*colors for  DOS  via  ANSI.SYS */

_=                                     /*(below) set some vars ──> NULL */
parse var _ ?. @ color. colorC. ahics ehics hold lz more onlyo onlys,
            scr0 shics VMout VScolor VSdisp x1 x2

@abc     = 'abcdefghijklmnopqrstuvwxyz'
@abcU    = @abc;   upper @abcU

#ms      = 0
?.a      = 0
?.b      = 0
?.block  = 0
?.e      = 0
?.end    = 0
?.i      = 0
?.ks     = 0
?.L      = 0
?.p      = 0
?.q      = 0
?.r      = 0
?.ruler  = 0
?.s      = 0
?.scale  = 0
?.ts     = 0
?.x      = 0
?.z      = 0
boxing   = 0
highL    = 0
LLd      = 0
LLk      = 0
LLx      = 0
maxhic   = 0

##       = 1
hue#     = 1
minhic   = 1
?.t      = 1

?.bd     = .2
?.bf     = 800
?.bs     = 2
?.o      = 9999
?.rulerb = ' '
?.scaleb = ' '
?.scaled = '.'
?.scalep = '+'
?.use    = '.'
esc      = '1b'x"["

his='H() H{} H[] H<> H≤≥ H«» H/\'
#his=words(his)
                                       do jh=1  for #his
                                       hh.jh=substr(word(his,jh),2)
                                       end   /*jh*/

colorSupport=!pcrexx | !r4 | !roo      /*colors are supported by these. */

                 boxCH = '+-+|+-+|'    /*define some boxing characters. */
if !ebcdic  then boxCH = 'acbfbcfabbbfabfa'x  /*¼┐╝·╗┐½·  <──single box.*/
if !dos     then boxCH = 'c9cdbbbabccdc8ba'x  /*╔═╗║╝═╚║  <──double box.*/

if colorSupport  then do               /*use pre-saved color values.    */
                      _=translate(!var('SCREEN'), ,";,")       /*envVar.*/
                      if \datatype(space(_,0), "W")  then _='36 40'
                      scr0=esc || translate(0 _, ';', " ")'m'
                      colorC.0=scr0
                      colorC.1=esc"1;33m"
                      end

  do jz=1  while  zz\==''
  if ?.end==1 | pos('=',zz)==0 | pos(" "?.use,' 'zz)==0  then do
                                                              @=@ zz
                                                              leave
                                                              end
  if left(zz,1)==' '  then lz=lz" "
  parse  var  zz  yy1 2 yy2 3 1 yy ' ' zz

  if yy1==?.use & pos('=',yy)\==0 & datatype(yy2,"U")  then
     do 1
     parse  var  yy  2 _ "=" dotv 2 _1 3
     if datatype(_,'U')  then
       do
       L1=length(_)==1
       if L1  then do
                   if _=='H'  then ?.hi.1=dotv
                              else ?._=dotv
                   iterate jz
                   end
              else select
                   when _=='BIN'  then yy=valn("'"dotv"'B",'BIN',"B")
                   when _=='BOX'  then do
                                       if dotv==""  then ?.BOX=boxCH
                                                    else ?.BOX=dotv
                                       iterate jz
                                       end
                   when _=='DEC'  then yy=valn("'"dotv"'D",'DEC',"D")
                   when _=='INV'  then yy=.inv(dotv)
                   when _=='HEX'  then yy=valn("'"dotv"'X",'HEX',"X")
                   when _=='USE'  then do
                                       dotv=tb(dotv,"USE",'.')
                                       iterate jz
                                       end
                   otherwise      ?._=dotv;    iterate jz
                   end   /*select*/
       end

     if _1=='H'  then do
                      _=wordpos(_,his)
                      if _\==0  then do
                                     ?.hi._=dotv
                                     iterate jz
                                     end
                      end
     end  /*do 1*/

  if @==''  then @=lz || yy
            else @=@ yy
  lz=
  end     /*jz*/

if left(@,1)==' '  then @=substr(@,2)  /*handle this special case.      */

if ?.a\==0       then call .a
if ?.a\==0       then call .b
if ?.block\==0   then call .block
if ?.c\==''      then call .c
hue.1=colorC.0
if ?.d\==''      then call .d
if ?.e\==0       then call wn 'E',0,99,sd()
?.eb=tb(?.eb,'EB')
if ?.ef\==''     then call .ef
if ?.f\==''      then call .f

                      do _j=1  for #his
                      _=?.hi._j
                      if _\=='' & \!regina  then do
                                                 call colors _,"H"hh._j,_j
                                                 highL=1
                                                 end
                      end   /*_j*/

if ?.i\==0       then do
                      call wn 'I',0,sw()
                      ?.ib=tb(?.ib,'IB')
                      end
if ?.j\==''      then call .j
if ?.k\==''      then ?.k =valn(?.k,"K")
if ?.kd\==''     then ?.kd=valn(?.kd,"KD")
if ?.k\==''      then if ?.kd\==""  then call er 61, '.K= .KD='
if ?.ks\==0      then call .ks
if ?.L\==0       then call .L
if ?.o\==9999    then call .o
if ?.p\==0       then do;  call wn 'P',-99,99;   ?.pb=tb(?.pb,'PB');   end
if ?.q\==0       then call wn 'Q',0,1
if ?.r\==0       then call wn "R",0,99;   ?.r=?.r+1
if ?.ruler\==0   then call .ruler
if ?.s\==0       then call .s;            ?.s=?.s+1
if ?.scale\==0   then call .scale
if ?.t\==1       then call .t
if ?.u\==''      then call .u
?.ub=tb(?.ub,'UB')
if ?.ut\==''     then call .ut
if ?.v\==''      then call .v
?.xb=tb(?.xb,'XB')
if ?.z\==0       then call wn 'Z',0,99,,"N"
if ?.box\==''    then call .box
if highL         then call highLight
indent=copies(?.ib,?.i)
if ?.x\==0       then call .x
@=copies(@,?.r)
ll=length(@)
if ?.ub\==' '    then @=translate(@,?.ub," ")
_=length(?.ut)%2
if _\==0         then @=translate(@,right(?.ut,_),left(?.ut,_))
tx.1=@
xk=?.k || ?.kd
if xk\==''       then call .xk
if LLk\==0       then LL=LLk

if ?.block\==0   then tLL=12+max(LL-1,0)*(12+?.bs)
                 else tLL=LL

bline=strip(indent || x1 || copies(?.ab, tLL+4*boxing)x2, 'T')

if boxing        then call ms bx.1 || copies(bx.2, LLx+tLL+2)bx.3
caLL VEReb ?.e,?.eb

  do jt=1  for ?.t
  if jt\==1  then  if jt\==?.t  then  call VEReb ?.ts,?.tsb

    do jj=1  for ##
    if jj\==1     then call VEReb ?.ks,?.ksb

    if boxing     then _=left(tx.jj,tLL)
                  else _=tx.jj

    if ?.v=='R'   then _=reverse(_)

    if ?.u\==''   then select
                       when ?.u=='A'  then nop
                       when ?.u=='U'  then upper _
                       when ?.u=='L'  then _=lower(_)
                       when ?.u=='F'  then _=proper(_)
                       when ?.u=='W'  then do
                                         __=
                                                  do jw=1  for words(_)
                                                  __=__ proper(word(_,jw))
                                                  end   /*jw*/

                                         _=strip(__)
                                         end
                       end   /*select*/

    if ?.block==0  then call tellIt _
                   else call blocker
    end   /*jj*/
  end     /*jt*/

call VEReb ?.e,?.eb
if boxing  then call ms bx.7 || copies(bx.6,LLx+tLL+2)bx.5
call beeps ?.b
call .p
if ?.ruler<0  then call inches ?.ruler,0
if ?.scale<0  then call inches ?.scale,1

  select  /* <══════════════════════════where the rubber meets the road.*/
  when highL                                    then call sayHighlight
  when \highL & (?.c=='BRITE' | ?.c=="BRIGHT")  then call sayBright
  when ?.L\==0                                  then call sayAline
  otherwise                                          call sayNline
  end   /*select*/

if ?.c\==''       then call VMcolor VMout,space(VScolor VSdisp)
if ?.b<0          then call call beeps ?.b
if ?.z\==0        then call .z
if ?.ruler>0      then call inches ?.ruler,0
if ?.scale>0      then call inches ?.scale,1
_=abs(?.a)

if _==99 & \?.q   then !cls
                  else do  min(99,_)
                       call wit bline
                       end   /*min(···*/

if ?.w\==''       then call .w

if !pcrexx  then  if ?.q & LLd>79  then  if LLd>sw()  then say

                              /*(above)  PC-REXX bug:  wrapped lines are*/
                              /*            overwritten during cleanup. */
return 0

/*──────────────────────────────────.B subroutine───────────────────────*/
.b: call wn 'B',-99,99,sd()            /*B  is for  beeps  (sounds).    */

if ?.bd\==.2   then do
                    _=translate(?.bd,,',')
                    __=_
                                   do  while  __\==''
                                   parse  var  __  ?.bd __
                                   call wn 'BD', .1, 9, ,"N"
                                   end   /*while*/
                    ?.bd=_
                    end

if ?.bf\==800  then do
                    _=translate(?.bf,,',')
                    __=_
                                   do  while  __\==''
                                   parse  var  __  ?.bf __
                                   call wn 'BF', 1, 20000
                                   end   /*while*/
                    ?.bf=_
                    end
return

/*──────────────────────────────────.BLOCK subroutine───────────────────*/
.block: call wn 'BLOCK',-12,12
                                if ?.bs\==2   then call wn 'BS', -12, sw()
                                if ?.bc\==''  then ?.bc = tb(?.bc, "BC")
?.bb=tb(?.bb,'BB')
return

/*──────────────────────────────────.BOX subroutine─────────────────────*/
.box:  _=?.box;      upper _
if _=='*NONE*'  then ?.box=
boxing= ?.box\==''
if \boxing  then return

if _=='SINGLELINE'     then _=boxCH
if length(_)>8         then call er 30, '.BOX='_ "boxcharacters 1 8"
?.box=left(_,8,right(_,1))
                                     do _=1  for 8
                                     bx._=substr(?.box,_,1)
                                     end   /*_*/
_=verify(@,' ')-1
if _>0  then @=@ || copies(" ",_)
return

/*──────────────────────────────────.C subroutine───────────────────────*/
.c: call colors ?.c,'C',0

if !cms  then do
              call cp 'QUERY SCREEN',1
              parse var cp.1 "VMOUT" VMout
              'QUERY VSCREEN CMS ALL (LIFO'
              if rc==0  then pull "(" . . VScolor VSdisp .
              if ?.c=='BRITE'  then call VMcolor "DEFAULT NONE"
                               else call VMcolor color.0 ?.d, color.0  ?.d
              end

if \colorSupport  then ?.c=            /*Most REXXes don't support color*/
return

/*──────────────────────────────────.D subroutine───────────────────────*/
.d:  upper ?.d
     _ =   ?.d

if \(abbrev('BRITE',_,3)    |,
     abbrev("BRIGHT",_,3)   |,
     abbrev('HIGHLIGHT',_)  |,
     abbrev("NONE",_,3)     |,
     abbrev('REVVIDEO',_,3) |,
     abbrev("UNDERLINE",_,3))      then call er 55, _ ".D="

if !regina  then ?.d=                   /*Regina can't handle DISP's.   */
            else  if  left(_,1)=='H'      then  highL=1
return

/*──────────────────────────────────.EF subroutine──────────────────────*/
ef:  if ?.f\==''  then call er 61, '.F= .EF='     /*conflicting options.*/
?.f = ?.ef
return

/*──────────────────────────────────.F subroutine───────────────────────*/
.f:  _=?.f                             /*File where the text is written.*/
if !cms  then do
              _=translate(_, , '/,')   /*try to translate to CMS format.*/
              if words(_)>3  then call er 10, ?.f
              ?.f = _ word(subword(_,2)  !fn,1)  word(subword(_,3) 'A1',1)
              end

__=lastpos("\",_)
if !dos  &  ?.ef==''  &  __\==0   then call $mkdir left(_,__)
return

/*──────────────────────────────────.INV subroutine─────────────────────*/
.inv:  return  x2c( b2x( translate( x2b( c2x( arg(1) ) ),   01,   10) ) )

/*──────────────────────────────────.J subroutine───────────────────────*/
.j:  upper ?.j                         /*Justify  (or not)  the text.   */
     if ?.j==''  then ?.j= 'N'         /*Justify  (or not)  the text.   */
                 else ?.j= left(?.j,1) /*just use the first letter of .J*/

if pos(?.j,"ACJLNR")==0  then call er 55, ?.j '.J='
if ?.j=='A'              then ?.j= substr(copies('LRC',30),random(1,90),1)

?.jb=tb(?.jb,'JB')                     /*while we're here, handle  JB.  */
return

/*──────────────────────────────────.KS subroutine──────────────────────*/
.ks: call wn 'KS', 0, 99, sw()
     ?.ksb = tb(?.ksb, 'KSB')          /*blank lines between karate chop*/
return

/*──────────────────────────────────.L subroutine───────────────────────*/
.L:  upper ?.L                         /*Line(s) for the text is shown. */
     if !cms  then do
                   '$QWHAT DSC'
                   if rc==4  then ?.L=0
                   end

if ?.L=='CMSG'     then ?.L="*"
call wn 'L',-sd(),sd()
if ?.L<0           then ?.L=sd()-?.L
return

/*──────────────────────────────────.O subroutine───────────────────────*/
.o:   call wn 'O',-999,999,9999

if ?.o<0  then do
               onlyo=-?.o
               ?.o=9999
               end
return

/*──────────────────────────────────.P subroutine───────────────────────*/
.p:  if ?.q  then return               /*Post (writting) blank lines.   */
_=?.p

if _>98 |,
   _<0  then do 1
             if !cms       & _>9998     then call CPmore
             !cls
             if \!cms                   then leave  /*1*/

             if _>9998     & more\==''  then call CP 'TERMINAL MORE' more
             if _>99999998 & hold\==''  then call CP 'TERMINAL HOLD' hold
             if _>99999998 & hold\==''  then call CP 'TERMINAL HOLD' hold
             end   /*1*/

   do  abs(_)  while _<99
   call wit bline
   end   /*abs*/
                         do _=1  to -?.a
                         call wit  bline
                         end  /*_*/
return

/*──────────────────────────────────.RULER subroutine───────────────────*/
.ruler:  call wn 'RULER', -sw(), sw()  /*RULER draws a  "ruler"  line.  */
?.rulerb = tb(?.rulerb, 'RULERB')
return

/*──────────────────────────────────.S subroutine───────────────────────*/
.s:      call wn "S", -999, 999, 999   /*Skip (or suppress)  line(s).   */

if ?.s<0  then do
               if left(?.o,1)=='-'  then /*check for conflicting options*/
                     call er 61,"O="?.o 'S='?.s "(both can't be negative)"
               onlys = -?.s
               ?.s   = 0
               end

if left(?.o,1)=="-" & left(?.s,1)=='-'  then
                     call er 61,"O="?.o 'S='?.s "(both can't be negative)"
return

/*──────────────────────────────────.SCALE subroutine───────────────────*/
.scale:  call wn 'SCALE', -sw(), sw()  /*SCALE draws a  "scale"  line. */
         ?.scaleb = tb(?.scaleb, 'SCALEB')
         ?.scaled = tb(?.scaled, 'SCALED', ".")
         ?.scalep = tb(?.scalep, 'SCALEP', "+")
return

/*──────────────────────────────────.T subroutine───────────────────────*/
.t:               call wn 'T',  0, 99  /*Times the text is written.     */
if ?.ts\==0  then call wn 'TS', 0, 99
                  ?.tsb = tb(?.tsb, 'TSB')
return

/*──────────────────────────────────.U subroutine───────────────────────*/
.u:       upper ?.u                   /*handle uppercasing text parts.  */
          ?.u = left(?.u, 1)
          if pos(?.u, " AFLUW")==0  then call er 55, ?.u  '.U='
          if ?.u==' ' | ?.u=='A'    then ?.u=
return

/*──────────────────────────────────.UT subroutine──────────────────────*/
.ut:      call wn 'T', 0, 99           /*Times the text is written.     */
          ?.ut=valn(?.ut, "UT")

          if length(?.ut)//2==1  then
                  call er 30,?.ut 'translate-characters an-even-number-of'
return

/*──────────────────────────────────.V subroutine───────────────────────*/
.v:       upper ?.v                    /*video mode, Normal -or- Reverse*/
          ?.v=left(?.v, 1)
          if pos(?.v, " NR")==0   then call er 55, ?.v  '.V='
          if ?.v==' ' | ?.v=='N'  then ?.v=
return

/*──────────────────────────────────.W subroutine───────────────────────*/
.w:       if ?.q        then return
          if ?.wb\==''  then ?.wb=tb(?.wb, 'WB')

          ww=translate(?.w,,"_")
          if ww='dd'x   then ww = "press any key to continue ..."
          if ww='de'x   then ww = "press the  ENTER  key to continue ..."
          call '$T' ".C=yel" translate(ww,?.wb,' ')
          if ww='dd'x   then call inkey
          if ww='de'x   then pull external
return

/*──────────────────────────────────.X subroutine───────────────────────*/
.x:       call wn 'X', -sw(), sw()
          x2 = copies(?.xb, abs(?.x))
          if ?.x<0  then x1=x2
          LLx = length(x1 || x2)
return

/*──────────────────────────────────.XK subroutine──────────────────────*/
.xk:      do ##=1
          parse  var  @  _ (xk) @
          if _==''  &  @==""  then leave
          tx.## = _
          if @\==''           then tx.## = tx.## || ?.k
          tx.## = strip(tx.##)
          LLk = max(LLk, length(tx.##))
          end    /*##*/
##=##-1
return

/*──────────────────────────────────.Z subroutine───────────────────────*/
.z:       _z=word(arg(1) ?.z, 1)       /*snore subroutine:  zzzzzz...   */
          if _z=0  then return
          if !cms  then call cp 'SLEEP' _z "SEC"
          if !dos  then call delay _z
return

/*──────────────────────────────────BEEPS subroutine────────────────────*/
beeps: if \!dos & !pcrexx  then return /*can this OS handle sounds?     */

          do jb=1  for abs(arg(1))
          if jb\==1  then call delay .1

                   do jb_=1  for words(?.bf)
                   call sound word(?.bf, jb_),  word(word(?.bd,jb_) .2,1)
                   end  /*jb_*/
          end           /*jb */
return

/*──────────────────────────────────BLOCKER subroutine──────────────────*/
blocker:           do jc=1  for LL     /*process some blocked characters*/
                   chbit.jc = $block(substr(_, jc, 1))
                   end   /*jc*/
bcl = ?.block
bcs = 1

if bcl<0    then do
                 bcl=-bcl
                 bcs=3*bcl-2
                 end

if _==''    then _=' '
tbc = ?.bc
if tbc==''  then tbc=_
tbc = left(copies(tbc,1+sw()%length(tbc)),sw())

  do jl=bcs  to 3*bcl  by 3
  _ = copies(?.bb, max(1, 12*LL+?.bs*LL-?.bs))
  bix = 1
                   do jo=1  for LL
                   _ = overlay(translate(x2b(substr(chbit.jo, jl, 3)),,
                                      substr(tbc, jo, 1)?.bb, 10), _, bix)
                   bix = max(1, bix+?.bs+12)
                   end   /*jo*/
  call tellIt _
  end     /*jl*/

return

/*──────────────────────────────────COLORS subroutine───────────────────*/
colors:  arg hue,__,cc#,cc             /*verify/handle synonymous colors*/
dark = left(hue,4)=='DARK'
if dark                          then hue = substr(hue,5)
if hue=='BRITE' | hue=="BRIGHT"  then hue = 'WHITE'
if left(hue,5)=='BRITE'          then hue = substr(hue,6)
if left(hue,6)=="BRIGHT"         then hue = substr(hue,7)
if abbrev('MAGENTA',hue,3)       then hue = "PINK"
if abbrev('CYAN'   ,hue,3)       then hue = "TURQUOIS"
if hue=='GREY'                   then hue = "GRAY"

  do jj=1  to words(hues)  by 2
  ahue=word(hues,jj)
  if abbrev(ahue,hue,3)  then do
                              cc=word(hues,jj+1)
                              hue=ahue
                              leave
                              end
   end   /*jj*/

if cc==''                   then call er 50, "color" '.'__"="hue
if dark & left(cc,2)=='1;'  then cc="0"substr(cc,2)

if !cms  then do
              if hue='GRAY' | hue=="BLACK"  then hue='WHITE'
              if hue="BROWN"                then hue='YELLOW'
              end

color.cc#  = hue
colorC.cc# = esc || cc'm'
return

/*──────────────────────────────────CPMORE subroutine───────────────────*/
cpMore:      call cp 'QUERY TERM', 9   /*parse CP TERMINAL for MORE,HOLD*/
             __=
                      do jj=1  for cp.0
                      __=__ cp.jj
                      end   /*jj*/

             parse upper var __  'MORE'  more  ','  1  'HOLD'  hold  ','
             if _>9998     & more\==''  then call  cp  'TERMINAL MORE 0 0'
             if _>99999998 & hold\==''  then call  cp  'TERMINAL HOLD OFF'
return

/*──────────────────────────────────DSAY subroutine─────────────────────*/
dsay:        if ?.q  then return       /*do SAY subroutine, write to scr*/
             dsay_ = strip(translate(arg(1), , '0'x), 'T')
             say  dsay_
             LLd = length(dsay_)       /*length of last line displayed. */
return

/*──────────────────────────────────HIGHLIGHT subroutine────────────────*/
highLight:   do _=1  for 7
             hhl._  = color._\==''
             hics._ = left(hh._,1)
             hice._ = right(hh._,1)

             if hhl._  then do
                            minhic= min(_,minhic);  shics= shics || hics._
                            maxhic= max(_,maxhic);  ehics= ehics || hice._
                            end
             end   /*_*/

ahics=shics || ehics
return

/*──────────────────────────────────HUE subroutine──────────────────────*/
hue:         hue#=max(1, hue#+arg(1))
             __=arg(2)
             if __\==''  then hue.hue#=__
             _=
return

/*──────────────────────────────────INCHES Subroutine───────────────────*/
inches:                                /*handle  RULER and SCALE  stuff.*/
_ = kw('RULERB')   kw('SCALEB')   kw('SCALEP')   kw('SCALED')

if arg(2)  then _=$scale(?.scale _ 'Q')
           else _=$scale(?.ruler 'RULE' _ 'Q')

parse  var  _  _.1 '9'x _.2 '9'x _.3

             do jk=1  for 3
             _=_.jk
             if _\==''  then call wit _
             end   /*jk*/
return

/*──────────────────────────────────MS subroutine───────────────────────*/
ms: #ms=#ms+1                          /*justification and indentation. */
parse  arg  _i

  select
  when ?.j==''             then nop
  when ?.N=='N'            then nop
  when length(_i)>=sw()-1  then nop
  when ?.j=='C'            then _i = centre(_i, sw()-1, ?.jb)
  when ?.j=='L'            then _i = strip(_i)
  when ?.j=='R'            then _i = right(strip(_i, "T"), sw()-1)
  when ?.j=='J'            then _i = justify(_i, sw()-1, ?.jb)
  end   /*select*/

mm.#ms=strip(indent || _i,'T')
return

/*──────────────────────────────────SAYALINE subroutine──────────────────*/
sayAline:

  do jj=?.s  to #ms  for ?.o
  if skp()  then iterate

  if \?.q   then do
                 if !cms  then '$CLEAR .WL='?.L _mm
                 if !dos  then call dsay,
                            esc || (?.L-1) || ";0f"colorC.0 || _mm || scr0
                 end
  call wr _mm
  ?.L=?.L+1
  if ?.L>sd()  then ?.L=1
  end   /*jj*/

return

/*──────────────────────────────────SAYBRITE subroutine─────────────────*/
sayBrite:  do jj=?.s  to #ms  for ?.o
           if skp()  then iterate
           call wr _mm
           if ?.q    then iterate

           if !cms   then '$CLEAR .C=BRITE' _mm
                     else if  !dos  then call dsay colorC.0 || _mm || scr0
           end   /*jj*/
return

/*──────────────────────────────────SAYNLINE subroutine─────────────────*/
sayNline:  do jj=?.s  to #ms  for ?.o
           if skp()  then iterate

           if !dos  then do
                         if ?.c==''  then call dsay _mm
                                     else call dsay colorC.0 || _mm || scr0
                         call wr _mm
                         end
                    else call wit _mm
           end   /*jj*/
return

/*──────────────────────────────────SAYHIGHLIGHT subroutine─────────────*/
sayHighlight:

  do jj=?.s  to #ms  for ?.o
  if skp()   then iterate

  if !cms    then do
                  if \?.q  then '$CLEAR .C=HIGHL' _mm
                  iterate
                  end

  lenmm=length(_mm)
  __=verify(_mm,ahics,'M')

  if __==0   then hc=lenmm+1
             else hc=__
  _xx=hue.1
  if hc>1    then _xx=_xx || left(_mm, hc-1)

    do jl=hc  to lenmm
    _=substr(_mm,jl,1)

      do jc=minhic  to maxhic
      if hhl.jc  then  if _==hics.jc  then call hue 1, colorC.jc
                                      else if _==hice.jc  then call hue -1
      end  /*jc*/

    if _==''  then _xx=_xx" "
    __=verify(substr(_mm, jl+1), ahics, 'M')

    if __==0  then pl=lenmm-jl+1
              else pl=__

    if pl==1  then iterate
    _xx=_xx || hue.hue# || substr(_mm, jl+1, pl-1)
    jl=jl+pl-1
    end   /*jl*/

  if length(_xx)>sw()  then if lenmm<=sw()  then _xx = esc's'_xx || esc"u"
  call dsay _xx || scr0
  call wr _mm
  end   /*jj*/

return

/*──────────────────────────────────SKP subroutine──────────────────────*/
skp:  if (onlyo\==''  &  onlyo\==jj) |,
         (onlys\==""  &  onlys ==jj)   then return 1
_mm = mm.jj
return 0

/*──────────────────────────────────TB subroutine───────────────────────*/
tb: tb=arg(1)                          /*test|verify Blank specification*/
if tb==''         then return left(arg(3), 1)
if length(tb)==2  then return valn("'"tb"'X", arg(2), 'X')
if length(tb)>1   then call er 30, tb "."arg(2)'=' 1
return tb

/*──────────────────────────────────TELLIT subroutine───────────────────*/
tellIt:         ___=arg(1)             /*tell it to the display terminal*/
                ___ = x1 || ___ || x2
if boxing  then ___=bx.8 || ?.eb || ___ || ?.eb || bx.4
        call ms ___
return

/*──────────────────────────────────VALN subroutine─────────────────────*/
valn: procedure;  parse arg x,n,k      /*validate number (dec,bin,hex). */
_ = left(x, 1)
v = "."n'='
if (_\=='"' & _\=="'")  |  ((right(x,2)\==_||k)  &  k\=='')  then return x
arg ' ' -1 t
x = substr(x,2,length(x)-3)
_ = length(x)

if t=='X'            then do
                          if \datatype(x, t)     then call er 40, x v
                          return x2c(x)
                          end

if t=='B'            then do
                          if \datatype(x, t)     then call er 91, x v
                          return x2c(b2x(x))
                          end

if \datatype(x, 'W')      then call er 53, x v
return d2c(x)

/*──────────────────────────────────VEREB subroutine────────────────────*/
VEReb: if arg(1)==0  then return       /*character for  Extra Blank(s). */
eb_ = x1 || copies(?.eb,tLL)x2
if boxing  then eb_ = bx.8 || ?.eb || eb_ || ?.eb || bx.4

                                    do jeb=1  for arg(1)
                                    call  ms  eb_
                                    end   /*jeb*/
return

/*──────────────────────────────────VMCOLOR subroutine──────────────────*/
VMcolor:         if \!cms    then return
parse  arg  c1,c2
                 if c1\==''  then call cp "SCREEN VMOUT" c1
                 if c2\==''  then "SET VSCREEN CMS" c2
return

/*──────────────────────────────────WN subroutine───────────────────────*/
wn:  procedure expose ?.               /*normalize, validate N in range.*/
arg  z, L, H, d, t
_ = ?.z
parse  upper  var  _  f 2
m = pos(f,'MH')\==0

if m | f=='*'  then do
                    _ = (word(d H L sw(),1)) / word(1 2,m+1)substr(_,2)
                    if \datatype(_,"N")  then interpret '_='translate(_,"%",'/')
                    ?.z = _
                    end

if datatype(_,"N")             then ?.z = _/1
if \datatype(_,left(t"W",1))   then call er 53, _ '.'z"="
if L\==''  then  if _<L | _>H  then call er 81,L H _ "value for option ."z'='
return _

/*──────────────────────────────────WR subroutine───────────────────────*/
wr: parse  arg  wr                     /*write [argument 1] ───> disk.  */
if ?.f==''             then return     /*Nothing to write? Then skip it.*/
if highL & ahics\==''  then wr=translate(wr,, ahics) /*has highlighting?*/

if !cms | !tso         then 'EXECIO 1 DISKW'  ?.f  "(FINIS STRING"  wr
                       else call lineout  ?.f, translate(wr, '10'x, "1a"x)
                                       /*(above) Handle E-O-F character.*/

call lineout ?.f                       /*close the file.                */
return 0

/*═════════════════════════════general 1-line subs═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════*/
!all: !!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=='NT';!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,'? ?SAMPLES ?AUTHOR ?FLOW')==0 then return 0;!call=']$H';call '$H' !fn !;!call=;return 1
!cal:   if symbol('!CALL')\=="VAR"  then !call=; return !call
!env:    !env='ENVIRONMENT'; if !sys=='MSDOS' | !brexx | !r4 | !roo  then !env='SYSTEM'; if !os2 then !env='OS2'!env; !ebcdic=1=='f0'x; if !crx then !env='DOS'; return
!fid:    parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .; call !sys; if !dos  then do; _=lastpos('\',!fn); !fm=left(!fn,_); !fn=substr(!fn,_+1); parse var !fn !fn '.' !ft; end;  return word(0 !fn !ft !fm,1+('0'arg(1)))
!rex:    parse upper version !ver !vernum !verdate .; !brexx='BY'==!vernum; !kexx='KEXX'==!ver; !pcrexx='REXX/PERSONAL'==!ver | 'REXX/PC'==!ver; !r4='REXX-R4'==!ver; !regina='REXX-REGINA'==left(!ver,11); !roo='REXX-ROO'==!ver; call !env; return
!sys:    !cms=!sys=='CMS'; !os2=!sys=='OS2'; !tso=!sys=='TSO' | !sys=='MVS'; !vse=!sys=='VSE'; !dos=pos('DOS',!sys)\==0 | pos('WIN',!sys)\==0 | !sys=='CMD'; !crx=left(!sys,6)=='DOSCRX'; call !rex; return
!var:    call !fid; if !kexx  then return space(dosenv(arg(1))); return space(value(arg(1),,!env))
.a:      call wn 'A',-99,99,sd();   ?.ab=tb(?.ab,'AB');  return
$block:  !call='$BLOCK';  call '$BLOCK' arg(1); !call=;  return result
$mkdir:  !call='$MKDIR';  call '$MKDIR' arg(1); !call=;  return result
$scale:  !call='$SCALE';  call '$SCALE' arg(1); !call=;  return result
cp:      "EXECIO" '0'arg(2) "CP(STEM CP. STRING" arg(1); return rc
er:      parse arg _1,_2; call '$ERR' "14"p(_1) p(word(_1,2) !fid(1)) _2; if _1<0  then return _1; exit result
p:       return word(arg(1),1)
halt:    call er .1
kw:      parse arg kw; return kw c2x(?.kw)
lower:   return translate(arg(1),@abc,@abcu)
noValue: !sigl=sigl; call er 17,!fid(2) !fid(3) !sigl condition('D') sourceline(!sigl)
proper:  procedure; arg f 2; parse arg 2 r; return f || r
sd:      if ?.scrdepth==''  then parse value scrsize()  with  ?.scrdepth ?.linesize .; return ?.scrdepth
sw:      if ?.linesize==''  then ?.linesize=linesize(); return ?.linesize
syntax:  !sigl=sigl; call er 13,!fid(2) !fid(3) !sigl !cal() condition('D') sourceline(!sigl)
wit:     call dsay arg(1);   call wr arg(1);   return
```


[[Category:REXX library routines]]
