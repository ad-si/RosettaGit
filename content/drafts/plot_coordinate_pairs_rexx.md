+++
title = "Plot coordinate pairs/REXX"
description = ""
date = 2019-01-12T04:20:42Z
aliases = []
[extra]
id = 17366
[taxonomies]
categories = []
tags = []
+++

This is a REXX program that satisfies the Rosetta Code task ''Plot coordinate pairs''.

This is a general purpose REXX program that supports a variety of options and data formats and produces output in text format.

See the   HELP   and SAMPLE   documentation below.



The   '''$PLOT.REX'''   REXX program makes use of   '''$T.REX'''   REXX program which is used to display text and/or write the text to a file. 

The   '''$T.REX'''   REXX program is included here ──► [[$T.REX]].


The   '''$PLOT.REX'''   REXX program makes use of '''$ERR''' REXX program which is used to display error messages (via '''$T'''). 

The   '''$ERR.REX'''   REXX program is included here ──► [[$ERR.REX]].


The   '''$PLOT.REX'''   REXX program makes use of '''SCRSIZE''' REXX program which is used to determine the screen size (via '''SCRSIZE'''). 

The   '''SCRSIZE.REX'''   REXX program is included here ──► [[SCRSIZE.REX]].


Some older REXXes don't have a   '''changestr'''   BIF, so one is included here ──► [[CHANGESTR.REX]].


REXX programs not included are   '''$H'''   which shows '''help''' and other documentation.

==$PLOT.REX (REXX) program==

```rexx
/*REXX program plots    X    or    X,Y    points   (with/without labels).               */

trace off
parse arg !
if !all(arg())  then exit                        /*if there was a doc request, then exit*/
if !cms  then address ''

signal on halt
signal on noValue
signal on syntax

@abc= 'abcdefghijklmnopqrstuvwxyz'               /*the lowercase Latin alphabet.  */
@abcU= @abc
upper @abcU                                      /*the uppercase Latin alphabet.  */
bgchar= ' '                                      /*character used for BackGround. */
colors= !cms | !pcrexx | !r4 | !roo              /*REXXes that support term color.*/
digs= 80                                         /*digits used for numeric digits.*/
fuzz= 0                                          /*use  0 digits for numeric fuzz.*/
showdigs= 10                                     /*show this many decimal digits. */
labelTag= '()'                                   /*chars used are for label tags. */
                                                 /* [↓] zero these REXX variables.*/
_= 0
parse var _ $calc 1 kws 1 labelD 1 labelP 1 logs 1 lowcas 1,
                  nDups 1 Oints 1 Onums 1 plotMult 1 1 plotSeq 1,
                  plotSim 1 pnts 1 quiet 1 sd 1 shownXlab. 1 showVal 1,
                  simple 1 sin 1 sortA 1 sortD 1 sortDu 1 sortU 1 sw 1,
                  swapAx 1 uppcas 1 xSin 1 ySin

                                                 /* [↓]  set these REXX vars to 1.*/
_= 1
parse var _ clear 1 commas 1 ixs 1 labelEv 1 norm 1 showLab 1,
                  showOrg 1 ixi 1 scaling

parse var _ . !. $$ gFID graf labelEnd labelSta labelX labelY tFID ,
                    tops xmaxuse ymaxuse xminuse yminuse xmnval ymnval ,
                    xmxval ymxval xy             /* [↑]  nullify these REXX vars. */

labelDatadef= 9          /*default for LABELP if LABELDATA is specified.*/

numeric digits 500                               /*now, use 500 for numeric digits*/
                                                 /*(could be more, see DIGs below)*/
_= space(!!)                                     /*remove superfluous blanks.     */
parse var   _   numbs  '('  ops  ")"             /*get numbers to be plotted, opts*/
ops= space(ops)                                  /*remove superfluous blanks.     */

if !ebcdic then do                               /*axis characters for EBCDIC mach*/
                xaxchar = 'bf'x                  /* hyphen symbol.                */
                yaxchar = 'fa'x                  /*  bar   symbol.                */
                orgchar = 'abbbacbcebeccccb8f'x                    /*  ½╗¼╝δ∞╠╦Å  */
                pntChars= '8eafd6f05c6c7b7c5b9c9f2b'x              /* Ä»O0*%#@$£+ */
                end
           else do                               /*axis characters for ASCII mach.*/
                xaxchar = 'c4'x                  /* hyphen symbol.                */
                yaxchar = 'b3'x                  /*   bar  symbol.                */
                orgchar = 'c0d9dabfc3b4c1c2c5'x                    /*  └┘┌┐├┤┴┬┼  */
                pntChars= 'faf94fe97ff0feb1b2b3db2b'x              /* ·∙OΘ≡■▒▓█+ */
                end

numbs= translate( numbs, , ',')                  /*remove commas from numbers.    */

  do kws=1  while numbs\==''                     /*verify that thingys are numeric*/
  parse var numbs _ numbs                        /*pick off the first "number".   */
  if \isnum(_)  then call er 55,_                /*¬ numeric?  Then show error msg*/
  !.kws= _                                       /*build a number stemmed array.  */
  end   /*kws*/                                  /* [↑]  traipse through all #'s. */

kws= kws - 1                                     /*adjust #data points 'cause of DO loop*/


  do  while  ops\==''                            /*process all options  (or none).*/
  parse var ops _1 2 1 _ . 1 _o ops              /*get an option, first character.*/
  upper _                                        /*uppercase a version of option. */


    select                                       /*traipse through possible opts. */
    when _1=.  &  pos("=", _)\==0    then tops=     tops  _o
    when  abb('$CALC')               then $calc=    1
    when abbn('CLearscreen')         then clear=    no()
    when abbn('COMMAs')              then commas=   no()
    when abbn('COLORs')              then colors=   no()
    when  abb('DIGits') |,
          abb('DIGs')                then digs=     nai()
    when abbn('FUZZ')                then fuzz=     nai()
    when  abb('GETfile')             then gfid=     na()
    when  abb('INDEXIncrement') |,
          abb('IIncrement')          then ixi=      nai()
    when  abb('INDEXStart') |,
          abb('IStart')              then ixs=      nai()
    when  abb('LABELDatapoints')     then labelD=   no()
    when  abb('LABELEVery')          then labelEv=  nai()
    when  abb('LABELPOints')         then labelP=   nai()
    when  abb('LABELStart')          then labelSta= nai()
    when  abb('LABELTags')           then labelTag= nai()
    when  abb('LABELXpoints')        then labelX=   nai()
    when  abb('LABELYpoints')        then labely=   nai()
    when abbn('LOGs')                then logs=     no()
    when abbn('LOWercased')          then lowcas=   no()
    when abbn('Quiet')               then quiet=    no()
    when  abb('NODUPlicates') |,
          abb('NODUPs')              then nDups=    1
    when  abb('NOLABELTags')         then labelTag=
    when abbn('NORMalized')          then norm=     left(_, 3)=='NOR'
    when  abb('ONLYINTegers') |,
          abb('ONLYINTs')            then Oints=    1
    when  abb('ONLYNUMbers') |,
          abb('ONLYNUMs')            then Onums=    1
    when abbn('PLOTMULtiples')       then plotMult= no()
    when abbn('PLOTSEQuencial')      then plotSeq=  no()
    when abbn('PLOTSIMples')         then plotSim=  no()
    when abbn('SCALing')             then scaling=  no()
    when  abb('SCREENDepth') |,
          abb('SCRDepth') |,
          abb('SDepth') |,
          abb('DEPth')               then sd=       nai()
    when  abb('SCREENWidth') |,
          abb('SCRWidth') |,
          abb('SWidth') |,
          abb('WIDth')               then sw=       nai()
    when  abb('SHOWDIGits') |,
          abb('SHOWDIGs')            then showdigs= nai()
    when abbn('SHOWLABels') |,
         abbn('LABels')              then showLab=  no()
    when abbn('SHOWORGins') |,
         abbn('ORGins')              then showOrg=  no()
    when abbn('SHOWVALues')          then showVal=  no()
    when abbn('SIMplecharacters') |,
         abbn('SIMplechars')         then simple=   no()
    when abbn('SINglevalues') |,
         abbn('SINgles')             then sin=      no()
    when abbn('SORTascending')       then sortA=    no()
    when abbn('SORTDescending')      then sortD=    no()
    when abbn('SORTDUnique')         then sortDu=   no()
    when abbn('SORTUnique')          then sortU=    no()
    when abbn('SWAPAXises') |,
         abbn('SWAPXYs') |,
         abbn('SWAPYXs')             then swapAx=   no()
    when abbn('UPPercased')          then uppcas=   no()
    when  abb('XMAXUSE')             then xmaxuse=  nan()
    when  abb('XMINUSE')             then xminuse=  nan()
    when abbn('XSINgles')            then xSin=     no()
    when  abb('YMAXUSE')             then ymaxuse=  nan()
    when  abb('YMINUSE')             then yminuse=  nan()
    when abbn('YSINgles')            then ySin=     no()
    otherwise      if \$calc then call er 55,_o  /*oop-say, not kosher opt.*/
    end   /*select*/                             /* [↑]   process options.*/
  end     /*while  ops\=='' */


if digs<1               then call er 81,'1 ,'  digs 'DIGITS'
if showdigs<1           then call er 81,'1 ,'  showdigs 'SHOWDIGITS'
if fuzz<0 | fuzz>=digs  then call er 81,0  digs-1  fuzz  'FUZZ'
if fuzz\==0             then numeric fuzz fuzz   /*user wants FUZZ, by gum*/

numeric digits max(digs, showdigs, ixs, ixs + ixi * length(kws) )      /*adjust digits. */

_= length(labelTag)                              /*get length of label tags*/

if _\==0  then do                                /*get LENs of start & end.*/
               if labelSta==''  then labelSta =  left( labelTag, round(_ / 2) )
               if labelEnd==''  then labelEnd = right( labelTag,       _ % 2)
               end

labelSta= translate( labelSta, , '_')            /*trans underbars──►blanks*/
labelEnd= translate( labelEnd, , '_')            /*trans underbars──►blanks*/

if sortA   & sortD        then call er 61,'SORTA SORTD'      /*conflict.*/
if sortA   & sortDu       then call er 61,'SORTA SORTDU'     /*conflict.*/
if plotSim & plotSeq      then call er 61,'PLOTSIMple PLOTSEQuencial'
if plotSim & plotMult     then call er 61,'PLOTSIMple PLOTMultiple'
if plotSeq & plotMult     then call er 61,'PLOTSEQuential PLOTMultiple'

labelP= int(labelP, 'LABELPOINTS')                /*insure LABELP is numeric*/

if labelP<0               then call er 81,0  ','  labelP  'LABELPOINTS'

labelEv= int(labelEv, 'LABELEVERY')               /*insure LABELV is numeric*/

if labelD & labelP==0     then labelP= labelDatadef    /*maybe use default.*/

labelX= int( p( labelX labelP), 'LABELXPOINTS')   /*insure labelX is numeric*/
labely= int( p( labely labelP), 'LABELYPOINTS')   /*insure labelY is numeric*/

if labelX<0               then call er 81,0  ','  labelX  'LABELXPOINTS'
if labely<0               then call er 81,0  ','  labely  'LABELYPOINTS'

ixi= int(ixi, 'INDEXINCEMENT')                   /*insure IXI    is numeric*/
ixs= int(ixs, 'INDEXSTART')                      /*insure IXS    is numeric*/

if  xSin &  ySin          then call er 61,'XSINGle YSINGle'     /*conflict.*/
if \xSin & \ySin          then xSin= 1           /*use  X as single points.*/
if \(plotSim & plotSeq & plotMult)  then plotSim= 1      /*use simple plot?*/
if xminuse\==''           then xminuse= num( xminuse, "XMINUSE")   /*min X.*/
if xmaxuse\==''           then xmaxuse= num( xmaxuse, "XMAXUSE")   /*max X.*/
if yminuse\==''           then yminuse= num( yminuse, "YMINUSE")   /*min Y.*/
if ymaxuse\==''           then ymaxuse= num( ymaxuse, "YMAXUSE")   /*max Y.*/
if logs                   then tops= '.F='gettFID(, "ANS")  tops   /*$T ops*/
if colors                 then tops= '.C=green'  tops              /*colors*/

tops= space(tops)                                /* [↓]    get screen size.*/
if sd==0 | sw==0          then parse value   scrsize()    with    _sd  _sw  .
if sd==0                  then sd= _sd           /*No scr depth?  Use true.*/
if sw==0                  then sw= _sw           /*No scr width?  Use true.*/

_= 0                                             /*set all vars below to 0.*/

if showOrg        then parse var _ xmnval 1 xmxval 1 ymnval 1 ymxval

if gfid\==''  then do                            /*there a gFID for input? */
                   call lineout gfid             /*close the gFID file.    */
                   gfide= 0

                     do  while lines(gfid)\==0   /*read all lines in gFIF. */
                     gfide= 1                    /*indicated there is data.*/
                     _= translate( linein( gfid), , ',')   /*remove commas.*/

                          do  while  _\==''
                          parse var   _    z  _
                          kws= kws + 1
                          !.kws= z
                          end   /*while _==*/

                     end        /*while lines*/  /* [↑] put each #──► array*/

                   if \gfide  then call er 38,gfid   /*No data? Tell errmsg*/
                  end                            /* [↑] process file plot#s*/

if sortDu  then do                               /*sort descending, unique?*/
                sortD= 1                         /*indicate sort descending*/
                sortU= 1                         /*indicate sort unique.   */
                end
@.0= kws                                         /*number of points to plot*/

if sin | (sortA | sortD | sortU)  then           /*single, any sort?       */
  do
             do j=1  for kws                     /*prepare for the SORT sub*/
             @.j= !.j                            /*assign data points──►@. */
             end

  if sortA | sortD | sortU  then call qSort      /*use qSort to sort nums. */
  ep= kws + 1                                    /*set the End-Point for @.*/
  if sortD  then do j=1  for kws%2               /*if descending, backward.*/
                 _= ep - j                       /*do it bottom-to-top.    */
                 parse value  @.j  @._  with  @._  @.j    /*assign values. */
                 end                             /* [↑]  order low-to=high.*/

  if sortU  then do                              /*is this a sort unique?  */
                 _= @.1                          /*first sort number.      */
                 k= 1                            /*first sort number index.*/
                       do j=2  for kws-1         /*is there a duplicate ?  */
                       if @.j=_  then iterate    /*Dup?  Then ignore the #.*/
                       k= k + 1                  /*No dup, then bump # ctr.*/
                       parse var  @.j  @.k  1  _ /*assign unique number.   */
                       end    /*j*/

                 kws= k                          /*keep track of # of nums.*/
                 end
  end

if sin  then do                                  /*if SINGLE, then handle. */
             sino= ixs                           /*start with this number. */

                      do j=1  for kws            /*process each number in @*/
                      _= @.j                     /*get a plot point number.*/
                      if xSin  then @.j= sino _  /*handle X single data pt.*/
                               else @.j= _ sino  /*   "   Y    "     "   " */
                      sino= sino + ixi           /*bump the SINGLE counter.*/
                      end   /*j*/


                                    do j=1  for kws
                                    !.j= @.j
                                    end   /*j*/
             end

  do j=1  for kws                                /*process the data points.*/
  _= !.j                                         /*get a data point number.*/
  xy= xy  _                                      /*add it to list of nums. */
  _w= words(xy)                                  /*number of numbers so far*/
  if _w==1   then iterate                        /*1st #?   Then get next. */
  if _w\==2  then call er 55,'XY-coordinate XY-plot-point' xy     /*2 nums?*/
                                                 /* [↑]  if ¬ 2#'s, err msg*/
  if swapAx  then parse var xy y x               /*swap X,Y numbers ··· or */
             else parse var xy x y               /*    ··· use #'s  as is. */
  xy=
  if xminuse\==''  then if x<xminuse  then iterate /*X value too small? */
  if xmaxuse\==''  then if x>xmaxuse  then iterate /*X value too large? */
  if yminuse\==''  then if y<yminuse  then iterate /*Y value too small? */
  if ymaxuse\==''  then if y>ymaxuse  then iterate /*Y value too large? */

  is#= isnum(_)                                  /*is data point a number? */
  if is# & Onums  then iterate                   /*plot only numbers?      */

  isi= isint(_)                                  /*is data point an int?   */
  if isi & Oints  then iterate                   /*plot only integers?     */

  if nDups  then do                              /*plot only non-dups?     */
                 _p= _                           /*set up a temporary value*/
                 if sin                  then _p= word(_, 2)
                 if wordpos(_p, $$)\==0  then iterate
                 $$= $$  _p                      /*add data point to string*/
                 end

  if norm  then do                               /*normalize the numbers?   */
                x= $norm(x)                      /*normalize  "  X number.  */
                y= $norm(y)                      /*    "      "  Y    "     */
                end

  pnts= pnts + 1                                 /*bump the POINTS counter. */
  @.pnts= x y                                    /*assign pnts to array @   */

  if xmnval==''  then do                         /*Not set?  Use this value.*/
                      xmnval= x                  /*set the minimum X value. */
                      ymnval= y                  /* "   "     "    Y   "    */
                      xmxval= x                  /* "   "  maximum X   "    */
                      ymxval= y                  /* "   "     "    Y   "    */
                      end

  if x>xmxval  then xmxval= x                    /*set the maximum  X  value*/
  if x<xmnval  then xmnval= x                    /* "   "  minimum  "    "  */
  if y>ymxval  then ymxval= y                    /* "   "  maximum  Y    "  */
  if y<ymnval  then ymnval= y                    /* "   "  minimum  "    "  */
  end   /*j=1  for kws*/

                                                 /* [↓] error if only 1 num.*/
if _w==1  then call er 55,'XY-coordinate XY-plot-point'  xy   /*odd data pt.*/

$$=                                              /*nullify the unique vals. */
oxmnval= xmnval                                  /*used for scaling minimum.*/
oymnval= ymnval                                  /*  "   "     "       "    */
oxmxval= xmxval                                  /*  "   "     "    maximum.*/
oymxval= ymxval                                  /*  "   "     "       "    */

if \scaling  then do                             /*Are we not scaling?  Get MIN and MAX.*/
                  _= min(xmnval, ymnval)
                  xmnval= _
                  ymnval= _                      /*minimum for   X   and   Y.     */
                  _= max(xmxval, ymxval)
                  xmxval= _
                  ymxval= _                      /*maximum for   X   and   Y.     */
                  end

if clear  then !cls                              /*should the screen be cleared?  */

xspread= max(1, xmxval - xmnval)                 /*calculate the spread of X vals.*/
yspread= max(1, ymxval - ymnval)                 /*    "      "     "    " Y   "  */

sd= int(sd, 'SCREENDEPTH')
if sd<1  then call er 27,sd 'SCREENDEPTH'

sw= int(sw, 'SCREENWIDTH')
if sw<1  then call er 27,sw 'SCREENWIDTH'

if pnts==0  then call er 54,'data─points'        /*if no points to plot,  issue error. */

promptlen= length( !var('PROMPT') )              /*length of the  PROMPT  string. */

tsw=   sw - 1                                    /*calculate the usuable scr width*/
tswu= tsw - 1                                    /*calculate the true screen width*/

tsd=   sd - 3  -  promptlen % sw
tsdu= tsd - 1                                    /*    "      "    "     "   depth*/

if plotSeq  then tsw= tsw - length(pnts)         /*Plot sequential?  Make smaller.*/

#.= copies(bgchar, tsw)                          /*characters used for background.*/
minxx=                                           /*actual minimum value for  X.   */
maxxx=                                           /*   "   maximum   "    "   "    */
minyy=                                           /*   "   minimum   "    "   Y.   */
maxyy=                                           /*   "      "      "   "    "    */

  xx0= round( -oxmnval / xspread * tswu)         /*round the value for  X  origin.*/
  yy0= round( -oymnval / yspread * tsdu)         /*  "    "    "    "   Y     "   */

if showOrg  then                                 /*construct X & Y axis.*/
  do
  minxx= xx0                                     /*initialize the  minimum  X's.  */
  maxxx= xx0                                     /*     "      "   maximum  X's.  */
  minyy= yy0                                     /*     "      "   minimum  Y's.  */
  maxyy= yy0                                     /*     "      "   maximum  Y's.  */

    do 1                                         /*handle the  0,0  origin of plot*/
    if substr(#.yy0, xx0 + 1, 1)\==bgchar  then leave  /*Bit the background?  Then skip.*/

    xn= oxmnval<0                                /*find the quadrant for  X  point. */
    xp= oxmxval>0                                /*  "   "      "     "   X    "    */
    yn= oymnval<0                                /*  "   "      "     "   Y    "    */
    yp= oymxval>0                                /*  "   "      "     "   Y    "    */

    _= 1                                         /*assume the 1st origin cross chr*/

    if  xn & \xp & \yn &  yp  then _= 2          /*  use   "  2nd    "     "    " */
    if \xn &  xp &  yn & \yp  then _= 3          /*  use   "  3rd    "     "    " */
    if  xn & \xp &  yn & \yp  then _= 4          /*  use   "  4th    "     "    " */
    if \xn &  xp &  yn &  yp  then _= 5          /*  use   "  5th    "     "    " */
    if  xn & \xp &  yn &  yp  then _= 6          /*  use   "  6th    "     "    " */
    if  xn &  xp & \yn &  yp  then _= 7          /*  use   "  7th    "     "    " */
    if  xn &  xp &  yn & \yp  then _= 8          /*  use   "  8th    "     "    " */
    if  xn &  xp &  yn &  yp  then _= 9          /*  use   "  9th    "     "    " */

    #.yy0= overlay( substr( orgchar, _, 1), #. yy0, xx0 + 1)    /*plot origin character.*/

    end   /*do 1*/                               /* [↑]  origin char:  axis cross.*/

  #.yy0= translate( #.yy0, xaxchar, bgchar)      /*change the background to   X   axis. */
  end

pntChar1= substr(pntChars, 2, 1)                 /*use this for a point character.*/
pntChar= pntChar1                                /*make a copy of   "       "     */
labelP= labelX + labelY                          /*indicate to label X or Y point.*/


  do j=1  for pnts                               /*plot the data points, ya betcha*/
  parse var   @.j   x  y                         /*break apart the X & Y data pt. */
  xx= round( (x - oxmnval) / xspread *tswu)      /*for this terminal, round X val.*/
  yy= round( (y - oymnval) / yspread *tsdu)      /* "    "      "       "   Y  "  */

  if minxx==''  then do
                     minxx= xx                   /*set initial minimum value for  X.   */
                     maxxx= xx                   /* "     "    maximum   "    "   X    */

                     minyy= yy                   /* "     "    minimum   "    "   Y.   */
                     maxyy= yy                   /* "     "    maximum   "    "   Y.   */
                     end

  minxx= min(minxx, xx)                          /*set the minimum value of  X.   */
  maxxx= max(maxxx, xx)                          /* "   "  maximum    "   "  X.   */

  minyy= min(minyy, yy)                          /*set the minimum value of  Y.   */
  maxyy= max(maxyy, yy)                          /* "   "  maximum   "    "  Y.   */

                                                 /* [↓] if plot multiple points···*/
  if plotMult  then pntChar= word( substr( pntChars, pos( substr( #.yy, xx + 1, 1),  ,
                                           pntChars) + 1, 1)  '+', 1)

  if plotSeq   then pntChar= pntChar1 || j       /*if plotting sequentially ···   */

  #.yy= overlay( pntChar, #.yy, xx + 1)          /*plot the  Y  data point.       */

  if labelP\==0  then @.j= x  y  xx  yy          /*if show point labels, pre-pend.*/
  end   /*j=1 for pnts*/



 do j=1  for pnts  while  labelP\==0             /*attach data point labels.      */
 parse  var    @.j    x  y  xx  yy               /*obtain x&y values from orig pt.*/
 _=                                              /* [↓]  use labels if existing.  */

 if labelX\==0  then do
                     xl= strip(left(x, labelX) )
                     _ = xl
                     end

 if labely\==0  then do
                     yl= strip(left(y, labely) )
                     _ = _','yl
                     end

 _= strip(_, , ',')                              /*remove commas from data pt. #. */

 if _==''           then iterate                 /*if nothing in data point, skip.*/
 if labelEv<1       then iterate                 /*don't label any data points.   */
 if j//labelEv\==0  then iterate                 /*only  label every N data points*/

 _= labelSta || _ || labelEnd                    /*add a label to the  data point.*/
 ll= length(_)                                   /*LL=length of label & data point*/
 old= #.yy                                       /*set an older copy of data point*/

 if xx+1+ll<=tsw & ,                             /*can lab+data point fit on term?*/
    substr(#.yy, xx + 2, ll)=' '  then #.yy = strip( overlay(_, #.yy, xx + 2), ' T')
                                  else do 1
                                       __= xx + 1 -ll
                                       if __<1   then leave
                                       if substr(#.yy, __, ll)\=' '  then leave
                                       #.yy = overlay(_, #.yy, __)
                                       end   /*do 1*/

 if length(#.yy)>tsw  then #.yy= old             /*if lab +data pt ¬ fit, use old.*/
 end   /*j=1 for pnts while labelP···*/


                                                 /* [↓]   now, display the graph. */
 do j=maxyy  to minyy  by -1                     /* only show data points that fit*/
 if showOrg  then                                /*Show origin?  Then place orgin.*/
   if substr(#.j, xx0 + 1, 1)==bgchar  then #.j = overlay( yaxchar, #.j, xx0 + 1)

 if showLab  then                                /*Show the plot labels?          */
    do                                           /*handle the min and max values. */
    if j==maxyy  then call labY oymxval          /*top*/
    if j==minyy  then call labY oymnval          /*bottom*/

      do jp=1  to -1  by -2                      /*show labels top──►bot.*/
      if j==yy0 + jp  then do                    /*place min & max values*/
                           call labX oxmnval 0   /*left  (minimum value).*/
                           call labX oxmxval 1   /*right (maximum value).*/
                           end
      end   /*jp*/
    end

 call plotL strip(#.j, 'T')                      /*display a particular plot line.*/
 end   /*j=maxyy to minyy by -1 */
                                                 /* [↓]   GRAF:  non-simple plots.*/
if graf\==''  then call $t '.KD=÷' tops substr( translate( graf, 'ff'x, " "), 2)
exit 0                                           /*stick a fork in it,  we're all done. */


/*═════════════════════════════general 1-line subs══════════════════════*/
!all:!!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=='NT';!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,'? ?SAMPLES ?AUTHOR ?FLOW')==0 then return 0;!call=']$H';call '$H' !fn !;!call=;return 1
!cal:if symbol('!CALL')\=="VAR" then !call=;return !call
!env:!env='ENVIRONMENT';if !sys=='MSDOS'|!brexx|!r4|!roo then !env='SYSTEM';if !os2 then !env='OS2'!env;!ebcdic=3=='f3'x;if !crx then !env='DOS';return
!fid:parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .;call !sys;if !dos then do;_=lastpos('\',!fn);!fm=left(!fn,_);!fn=substr(!fn,_+1);parse var !fn !fn '.' !ft;end;return word(0 !fn !ft !fm,1+('0'arg(1)))
!rex:parse upper version !ver !vernum !verdate .;!brexx='BY'==!vernum;!kexx='KEXX'==!ver;!pcrexx='REXX/PERSONAL'==!ver|'REXX/PC'==!ver;!r4='REXX-R4'==!ver;!regina='REXX-REGINA'==left(!ver,11);!roo='REXX-ROO'==!ver;call !env;return
!sys:!cms=!sys=='CMS';!os2=!sys=='OS2';!tso=!sys=='TSO'|!sys=='MVS';!vse=!sys=='VSE';!dos=pos('DOS',!sys)\==0|pos('WIN',!sys)\==0|!sys=='CMD';!crx=left(!sys,6)=='DOSCRX';call !rex;return
!var:call !fid;if !kexx then return space(dosenv(arg(1)));return space(value(arg(1),,!env))
$fact!: procedure;parse arg x _ .;l=length(x);n=l-length(strip(x,'T',"!"));if n<=-n|_\==''|arg()\==1 then return x;z=left(x,l-n);if z<0|\isint(z) then return x;return $fact(z,n)
$fact:  procedure;parse arg x _ .;arg ,n ! .;n=p(n 1);if \isint(n) then n=0;if x<-n|\isint(x)|n<1|_||!\==''|arg()>2 then return x||copies("!",max(1,n));!=1;s=x//n;if s==0 then s=n;do j=s to x by n;!=!*j;end;return !
$norm:  procedure expose showdigs !regina;parse arg x;if \datatype(x,'N') then return x;x=x/1;if pos('.',x)\==0 then x=format(x,,showdigs)/1;if !regina then do;_=x 'E0';parse var _ 'E' e .;if e<0&-e<showdigs then x=format(x,,showdigs,0);end;return x
$sfxa:  parse arg ,s,m;arg u,c;if pos(left(s,2),u)\==0 then do j=length(s) to compare(s,c)-1 by -1;if right(u,j)\==left(c,j) then iterate;_=left(u,length(u)-j);if isnum(_) then return m*_;leave;end;return arg(1)
$sfxf:  parse arg y;if right(y,1)=='!' then y=$fact!(y);if \isnum(y) then y=$sfxz();if isnum(y) then return y;return $sfxm(y)
$sfxm:  parse arg z;arg w;b=1000;if right(w,1)=='I' then do;z=shorten(z);w=z;upper w;b=1024;end;p=pos(right(w,1),'KMGTPEZYXWVU');if p==0 then return arg(1);n=shorten(z);r=num(n,f,1);if isnum(r) then return r*b**p;return arg(1)
$sfxz:  return $sfxa($sfxa($sfxa($sfxa($sfxa($sfxa(y,'PAIRs',2),'DOZens',12),'SCore',20),'GREATGRoss',1728),'GRoss',144),'GOOGOLs',1e100)
$t:        !call=']$T'; call "$T" arg(1); !call=;return
abb:       arg abbu; parse arg abb; return abbrev(abbu,_,abbl(abb))
abbl:      return verify(arg(1)'a',@abc,'M')-1
abbn:      parse arg abbn; return abb(abbn) | abb('NO'abbn)
er:        parse arg _1,_2; call '$ERR' "14"p(_1) p(word(_1,2) !fid(1)) _2; if _1<0 then return _1; exit result
err:       call er '-'arg(1),arg(2); return ''
erx:       call er '-'arg(1),arg(2); exit ''
getdtFID:  tFID=p(!var("TMP") !var('TEMP') homedrive()"\"); if substr(tFID,2,1)==':' & substr(tFID,3,1)\=="\" then tFID=insert('\',t,2); return strip(tFID,'T',"\")'\'arg(1)'.'arg(2)
getTFID:   if symbol('TFID')=='LIT'  then tFID=;if tFID\=='' then return tFID;gfn=word(arg(1) !fn,1);gft=word(arg(2) 'ANS',1);tFID='TEMP';if !tso then tFID=gfn'.'gft;if !cms then tFID=gfn','gft",A4";if !dos then tFID=getdTFID(gfn,gft);return tFID
halt:      call er .1
homedrive: if symbol('HOMEDRIVE')\=="VAR"  then homedrive=p(!var('HOMEDRIVE') 'C:'); return homedrive
int:       int=num(arg(1),arg(2)); if \isint(int)  then call er 92,arg(1) arg(2); return int/1
isint:     return datatype(arg(1),'W')
isnum:     return datatype(arg(1),'N')
labX:  parse arg n r;if (shownXlab.0&\r)|(shownXlab.1&r) then return;z=copies(bgchar,length(n));_=#.j;if r then do;_=reverse(_);n=reverse(n);end;_p=pos(z,_);if _p>2|_p==0 then return;_=overlay(n,_,_p);shownXlab.r=1;if r then _=reverse(_);#.j=_;return
labY:      parse arg n; l=length(n); z=copies(bgchar,l); do k=2 to 5; _=xx0+k; if substr(#.j,_,l)==z then do; #.j=overlay(n,#.j,_); return; end; _=max(1,xx0-k+1); if substr(#.j,_,l)==z then do; #.j=overlay(n,#.j,_);return;end;end;return
na:        if arg(1)\=='' then call er 01,arg(2);parse var ops na ops;if na=='' then call er 35,_o;return na
nai:       return int(na(),_o)
nan:       return num(na(),_o)
no:        if arg(1)\==''  then call er 01,arg(2); return left(_,2)\=='NO'
novalue:   !sigl=sigl; call er 17,!fid(2) !fid(3) !sigl condition('D') sourceline(!sigl)
num:       procedure;parse arg x .,f,q;if x=='' then return x;if isnum(x) then return x/1;x=space(translate(x,,','),0);if \isnum(x) then x=$sfxf(x);if isnum(x) then return x/1;if q==1 then return x;if q=='' then call er 53,x f;call erx 53,x f
p:         return word(arg(1),1)
plotL:     if quiet then return; parse arg plotL; if simple  then plotL=simple(plotL); if tops==''  then say arg(1); else graf=graf'÷'arg(1); return
qSort:     procedure expose @.; h=@.0; do while h>1; h=h%2; do i=1 for @.0-h; j=i; k=h+i; do while @.k<@.j; t=@.j; @.j=@.k; @.k=t; if h>=j then leave; j=j-h; k=k-h; end; end; end; return
round:     return format(arg(1),,p(arg(2) 0))
s:         if arg(1)==1 then return arg(3); return word(arg(2) 's',1)
shorten:   procedure; parse arg a,n; return left(a,max(0,length(a)-p(n 1)))
simple:    return translate(arg(1),'.||--%<>AV'copies('+',25),"·│║─═☼◄►↑↓┤┐└┴┬├┼┘┌╔╗╚╝╟╢╞╡╫╪╤╧╥╨╠╣")
syntax:    !sigl=sigl; call er 13,!fid(2) !fid(3) !sigl !cal() condition('D') sourceline(!sigl)
```


==$PLOT.REX (REXX) program HELP==

```txt


    ┌───────┐          ┌┐                    ┌──────────┐         ┌────────────┐
   ┌┘ ┌───┐ └┐         ││                   ┌┘ ┌──────┐ └┐        │┌────┐┌────┐│
   │  │   │  │         ││                   │  │      │  │        └┘    ││    └┘
   │  │   │  │         ││                   │  │      │  │              ││
   │  │   │  │         ││                   │  │      │  │              ││
   │  └───┘ ┌┘         ││                   │  │      │  │              ││
   │┌───────┘          ││                   │  │      │  │              ││
   ││                  ││                   │  │      │  │              ││
   ││                  ││                   │  │      │  │              ││
   ││                  ││       ┌┐          │  │      │  │              ││
   ││                  ││       ││          │  │      │  │              ││
   ││                  │└───────┘│          └┐ └──────┘ ┌┘              ││
   ││                  │┌────────┘           └────┐┌────┘               ││
───┘└──────────────────┘└─────────────────────────┘└────────────────────┘└──────


The  $PLOT  command/subroutine is used to display a plot of specified pairs of
numbers (X,Y coordinates), or plot a list of numbers (singularly), with either
the  X or Y  cordinates being implied, the default is 0 ───► n─1, the origin and
increment can be overridden.

╔══════════════════════════════════════════════════════════════════════════════╗
║                                                                              ║
║         ┌        ┐         ┌             ┐       ┌        ┐      ┌        ┐  ║
║         │numbers │         │  CLearscreen│       │  COLORs│      │  COMMAs│  ║
║ $PLOT   │        │   (     │NOCLearscreen│       │NOCOLORs│      │NOCOMMAs│  ║
║         │?       │         └             ┘       └        ┘      └        ┘  ║
║         │?AUTHOR │                                                           ║
║         │?FLOW   │              ┌              ┐     ┌                  ┐    ║
║         │?SAMPLES│              │            1 │     │                1 │    ║
║         └        ┘              │INDEXStart sss│     │INDEXIncrement iii│    ║
║                                 │IStart     sss│     │IIncrement     iii│    ║
║                                 └              ┘     └                  ┘    ║
║                                                                              ║
║          ┌                ┐                ┌                 ┐               ║
║          │              0 │                │NOLABELdatapoints│               ║
║          │LABELPoints  iii│                │  LABELDatapoints│               ║
║          └                ┘                └                 ┘               ║
║          ┌                ┐                                                  ║
║          │              0 │      ┌                 ┐     ┌               ┐   ║
║          │LABELXpoints iii│      │NOLABELTags      │     │LABELStart cccc│   ║
║          └                ┘      │                 │     └               ┘   ║
║          ┌                ┐      │             ()  │     ┌               ┐   ║
║          │              0 │      │  LABELTags cccc │     │LABELEnd   cccc│   ║
║          │LABELYpoints iii│      └                 ┘     └               ┘   ║
║          └                ┘                                                  ║
║                                     ┌          ┐                             ║
║                                     │MINUSE nnn│   ┌           ┐   ┌       ┐ ║
║    ┌              ┐     ┌      ┐    └          ┘   │  NORMalize│   │NOQuiet│ ║
║    │            1 │     │  LOGs│    ┌          ┐   │NONORMalize│   │  Quiet│ ║
║    │LABELEVery nnn│     │NOLOGs│    │MAXUSE nnn│   └           ┘   └       ┘ ║
║    └              ┘     └      ┘    └          ┘                             ║
║                                                                              ║
║                                                                              ║
║                      ┌            ┐              ┌            ┐              ║
║                      │NODUPlicates│              │ONLYINTegers│              ║
║                      │NODUPs      │              │ONLYINTs    │              ║
║                      └            ┘              └            ┘              ║
║                                                                              ║
║                                                                              ║
║                                                                              ║
║    ┌              ┐    ┌         ┐    ┌               ┐    ┌               ┐ ║
║    │PLOTSEQuential│    │  SCALing│    │SCREENWidth www│    │SCREENDepth ddd│ ║
║    │PLOTSIMple    │    │NOSCALing│    │SCRWidth    www│    │SCRDepth    ddd│ ║
║    │PLOTMULTiple  │    └         ┘    │SWidth      www│    │SDepth      ddd│ ║
║    └              ┘                   │WIDth       www│    │DEPth       ddd│ ║
║                                       └               ┘    └               ┘ ║
║                                                                              ║
║                                 ┌                  ┐                         ║
║              ┌            ┐     │  SIMplechars     │      ┌              ┐   ║
║              │  SHOWLABels│     │  SIMplecharacters│      │  SINGlevalues│   ║
║              │NOSHOWLABels│     │NOSIMplechars     │      │NOSINGlevalues│   ║
║              └            ┘     │NOSIMplecharacters│      └              ┘   ║
║                                 └                  ┘                         ║
║   ┌              ┐                                                           ║
║   │SORTascending │      ┌            ┐        ┌        ┐        ┌        ┐   ║
║   │SORTDescending│      │NOSWAPAXises│        │XSINgles│        │.XXX=yyy│   ║
║   │SORTDUnique   │      │  SWAPAXises│        │YSINgles│        └        ┘   ║
║   │SORTUnique    │      └            ┘        └        ┘                     ║
║   └              ┘                                                           ║
║                                                                              ║
╚══════════════════════════════════════════════════════════════════════════════╝

───where:    (all arguments are optional can be specified in any order,  and
             all options and/or values can be in lower/upper/mixed case.)

?            shows this help file             (press ESC to quit when viewing).

?AUTHOR      shows the author of this program.

?FLOW        shows the external execution flow of this program.

?SAMPLES     shows some sample uses           (press ESC to quit when viewing).


numbers      are any number of valid REXX numbers seperated by blanks  and  may
             be seperated by  commas  to help  pairing  of  X,Y  coordinates.
             The numbers are assumed to be  pairs  of   X,Y   coordinates  (the
             default),  or  single values  of the   X  or  Y   axis if the
             SINglevalues  option is specified.    The  SINX  or  SINY  option
             is used to specify  which axis  is be used for the single values.

(            signifies that  $PLOT  options follow.

CLearscreen  will clear the terminal screen before any output is
             shown on the terminal.                The default is   CLEARSCREEN

COLORs       uses colors when displaying the plot.

NOCOLOrs     doesn't use colors when displaying the plot.
             The default is:    COLORS   (for DOS or CMS),
                              NOCOLORS   (for all other systems).

COMMAs       specifies to commas are to be inserted into the numbers shown
             for the labels (if shown).                The default is:   COMMAS

NOCOMMAs     doesn't insert commas into numbers being shown for labels.

INDEXStart sss
IStart     sss      where   sss   is a number that is used to set (start) the
                    initial value of   X   (the default),  or   Y   if
                    SINGLEVALUES  is specified.   The default is:  INDEXSTART 1

INDEXIncrement iii
IIcrement      iii  where   iii   is a number that is used to increment either
                    the  X  or  Y  value  (starting at  INDEXSTART)   if
                    SINGLEVALUES  is specified.
                                              The default is:  INDEXINCREMENT 1

LABELPoints  iii    causes labeling of the data points with the left-most  nnn
                    characters of the  X  and  Y  numeric values, seperated by
                    a comma.   See the  LABELXPOINTS and LABELYPOINTS options
                    below.   This option, in effect,  sets both  LABELXPOINTS
                    and  LABELYPOINTS  to   nnn.       The data point label is
                    prefixed with the characters specified in LABELSTART  and
                    suffixed   "   "       "         "      " LABELEND, if any.
                    If LABELSTART or LABELEND aren't specified, they get their
                    values from  LABELTAGS  (unless  NOLABELTAGS is specified).
                                              The default is:  0

LABELXpoints iii    causes labeling of the data points with the left-most  nnn
                    characters of the  X  numeric value.   If the value of  X
                    is  3.45678  and  LABELXPOINTS 4  is specified, the label
                    3.456  will be used.      The default is:  0   (zero)
                    but takes its value from  LABELPOINTS  if not specified.

LABELYpoints iii    causes labeling of the data points with the left-most  nnn
                    characters of the  Y  numeric value.   If the value of  Y
                    is  -3.45678  and  LABELYPOINTS 4  is specified, the label
                    -3.4  will be used.       The default is:  0   (zero)
                    but takes its value from  LABELPOINTS  if not specified.

NOLABELDatapoints   doesn't label the data points.
                                            The default is:  NOLABELDATAPOINTS

LABELDatapoints     turns on the   LABELPOINTS  9    option  if  the
                                   LABELPOINTS    option hasn't been specified.
                                            The default is:  NOLABELDATAPOINTS

NOLABELTags         causes no   LABELTAGS   to be used (set to null),  but
                    LABELSTART  and  LABELEND  can still be specified.

LABELTags cccc   is any arbitary character string to be used for LABELSTART and
                 LABELEND,  the first half is used for LABELSTART, the latter
                 half is used for LABELEND, the greater half (if an odd # of
                 characters is specified) is used for LABELSTART.  Blanks can
                 be specified by using the underscore (_) character.
                                              The default is:  ()

LABELStart cccc  is any arbitary character string to be used for the start of a
                 data point label.  If not specified, the first (larger) half
                 of  LABELTAGS  is used.  Blanks can be specified by using the
                 underscore (_) character.    The default is:  (       {if any}

LABELEnd   cccc  is any arbitary character string to be used for the end of a
                 data point label.  If not specified, the last (smaller) half
                 of  LABELTAGS  is used.  Blanks can be specified by using the
                 underscore (_) character.    The default is:  )       {if any}

LABELEVery  iii  will label every  IIIth  data point (if data points are being
                 labeled.                             The default is:  1

LOGs         causes all normal (non-error) output  (i.e., the plot) to be
             logged  (written to, or appended) to a file.
             On CMS, the fileid is:       $PLOT  ANS  A5
             On TSO, the fileid is:       $PLOT.ANS
             On DOS, the fileid is: \temp\$PLOT.ANS    -where  temp  is the
               value of the  TEMP environment var, if not found,  TMP  is used.
             The default is:     NOLOGs

MINUSE nnn   is the minimum number to be used in the plot.   Any number below
             this is ignored.      There is no default.

MAXUSE nnn   is the maximum number to be used in the plot.   Any number above
             this is ignored.      There is no default.

NORMalize    will normalize all the numbers specified.   Any leading plus
             signs are removed, including leading or trailing trivial zeros,
             superflous decimal points, and exponentation.
             The default is:    NORMALIZE

Quiet        indicates that no output is shown.       The default is:   NOQUIET

DUPs
DUPlicates     will  show duplicate results.  The default is:   DUPLICATES

NODUPs
NODUPlicates   won't show duplicate results.  The default is:   DUPLICATES
               If any result (to be shown) is a duplicate, it isn't shown.  The
               first result is shown, however, any subsequent duplicates aren't.
               The comparison is performed after adding any commas, ajusting
               the case, normalizing the number, or adjusting the precision.

ONLYINTegers
ONLYINTs       won't plot numbers that aren't an integer.
                                            The default is to allow all numbers.

PLOTSIMple   does a simple plot, with a point (∙)  to show the datapoint.  Any
             plot point overlaying an another plot point simple shows one point.
                                                    The default is:  PLOTSIMPLE

PLOTSEQuential  each plot is plotted with a point (∙) followed by the number of
                the data point, starting with 1.    The default is:  PLOTSIMPLE

PLOTMULTiple    each plot is plotted with a point, unless another point has been
                already plotted, in that case, the next point in the list is
                used to show the data point.   The point list is:  ·∙OΘ≡■▒▓█+
                If the list is exausted, a plus sign (+) is used.

SCALing      will  scale the  X  and  Y  axis data points to the best fit on the
             terminal screen.                       The default is:  SCALing

NOSCALing    won't scale the  X and Y  axis data points to the best fit on the
             terminal screen.   In effect, the  NOSCALING  option will preserve
             the aspect of the  X and Y  values.    The default is:  SCALing

SCREENWidth www  specifies the width of the terminal screen.   If not specified
SCRWidth www           or a value of 0 (zero) is specified, the screen width is
SWidth www             obtained from the operating system.
WIDth www              If not obtainable, a value of 80 is assumed.
                       Because of some terminal's behavior, the actual
                       width used is    SCREENWIDTH - 1

SCREENDepth ddd  specifies the depth of the terminal screen.   If not specified
SCRDepth ddd           or a value of 0 (zero) is specified, the screen depth is
SDepth ddd             obtained from the operating system.
DEPth ddd              If not obtainable, a value of 24 is assumed.
                       Because of certain operating systems, three lines
                       are assummed to be used for prompts, etc, so the
                       actual depth used is    SCREENDEPTH - 3

SHOWLABels   shows the values of the minimum and maximum values plotted,
             including  0 (if it's on the plot).  The default is:    SHOWLABels

SIMplechars
SIMPLEcharacters     translates extended characters to "simple" characters.
           This option affects boxing characters and foreign non-Latin alphabet
           characters in the month and day-of-week names.
                                           The default is:   NOSIMPLEcharacters

SINglevalues    will cause the numbers specified to be plotted as single values
                (instead of being plotted as  X and Y  coordinates).
                                           The default is:   NOSINGLEVALUES

NOSINglevalues  will cause the numbers specified to be plotted as  X and Y
                coordinates.               The default is:   NOSINGLEVALUES

SWAPAXises      will swap the data points so that:
                    all  X  points will plot on the  Y  axis, and
                    all  Y  points will plot on the  X  axis.
                The default is:  NOSWAPAXISIS

NOSWAPAXises    won't swap the data points on each axises..

SORTascending   sorts the data points in ascending order.
                This option only can be in effect for  SINGLE  values.
                The default is to plot the data points in the order specified.

SORTDescending  sorts the data points in descending order.
                This option only can be in effect for  SINGLE  values.
                The default is to plot the data points in the order specified.

SORTDUnique     sorts the data points in descending order, and any duplicate
                data points are not plotted.
                This option only can be in effect for  SINGLE  values.
                The default is to plot the data points in the order specified.

SORTUnique      sorts the data points in ascending order, and any duplicate
                data points are not plotted.
                This option only can be in effect for  SINGLE  values.
                The default is to plot the data points in the order specified.

XSINgles        will cause a sequence of generated numbers to be used for the X
                coordinates starting at  INDEXStart     (normally 0) and
                to be incremented by    INDEXIcrement  (normally 1)  if the
                SINGLEvalues  option is specified.   The default is:  NOSINX
                The  Y  axis numbers are taken from the specified list.

YSINgles        will cause a sequence of generated numbers to be used for the Y
                coordinates starting at  INDEXStart     (normally 0) and
                to be incremented by    INDEXIcrement  (normally 1)  if the
                SINGLEvalues  option is specified.   The default is:  NOSINY
                The  X  axis numbers are taken from the specified list.

.XXX=yyy     are any options to be used by the  $T  program  (which is used to
             display the histogram.   Possible uses are to specify a color
             (.C=color)  for the histogram.   For more info, issue:     $H  $T

                                       Ω

```

==$PLOT.REX (REXX) program SAMPLE usages==

```txt

                  ╔══════════════════════════════════════════╗
                  ║ sample uses of the  $PLOT   REXX command ║
                  ╚══════════════════════════════════════════╝


████████████████████████████████████████████████████████████████████████████████

─── how to get help information for the  $PLOT  command  (two methods are shown)
    (press  ESC  to quit viewing the help file)

$H $PLOT
$PLOT ?

████████████████████████████████████████████████████████████████████████████████

─── how to invoke  $PLOT  to show a plot of the some Fibonocci numbers

$PLOT  1,0 2,0 3,1 4,2 5,3 6,5 7,8 8,13 9,21 10,34 11,55 12,89

│89                                                                           ·
│
│
│
│
│
│
│
│
│
│
│
│
│
│
│
│
│
│                                                                       ·
│
│
│
│
│
│
│
│
│
│                                                                ·
│
│
│
│
│
│
│                                                          ·
│
│
│
│                                                   ·
│
│
│                                             ·
│                                      ·
│                                ·
│0                  ·     ·                                                 12
└──────·─────·────────────────────────────────────────────────────────────────


████████████████████████████████████████████████████████████████████████████████

─── how to invoke  $PLOT  (as above), but the data points are in the (DOS)
    file:   FIB.DAT      (plot is not shown as it is a duplicate of above)

$PLOT  getfile fib.dat

████████████████████████████████████████████████████████████████████████████████

─── how to invoke  $PLOT  (as above), but the data are expressed as singlur
    points

$PLOT  0 1 2 3 5 8 13 21 34 55 89   singlevalues

│89                                                                           ·
│
│
│
│
│
│
│
│
│
│
│
│
│
│
│
│
│
│                                                                      ·
│
│
│
│
│
│
│
│
│
│                                                               ·
│
│
│
│
│
│
│                                                        ·
│
│
│
│                                                 ·
│
│
│                                          ·
│                                  ·
│                           ·
│0            ·      ·                                                      11
└──────·──────────────────────────────────────────────────────────────────────

████████████████████████████████████████████████████████████████████████████████

─── how to invoke  $PLOT  but limit the screen (plot) depth to 25, and the
    screen (plot) width to 40

$PLOT  -4,-2  -3,-1  -2,0  -1,1  0,1  1,1  2,2  3,3   sd 25  sw 40

                      │3              ·
                      │
                      │
                      │
                      │          ·
                      │
                      │
                      │
                ·     ·    ·
                      │
                      │
                      │
-4                    │              3
───────────·──────────┼───────────────
                      │
                      │
                      │
     ·                │
                      │
                      │
                      │
·                     │-2

████████████████████████████████████████████████████████████████████████████████


                                      ♦

```

==$PLOT.REX (REXX) program logic FLOW==

```txt

     ┌───────────────┐       ╔═══════════╗
     │ $H  for ?opts ├────◄──╢           ║
     └───────────────┘       ║   $PLOT   ║
                             ║           ║                           ┌────────┐
                             ║           ║     ┌─────────────────┐   │ $T  for│
                             ║     errors╟───►─┤ $ERR for err msg├─►─┤  color │
                             ║           ║     └─────────────────┘   │   msgs │
                             ║           ║                           └────────┘
                             ║           ║
                             ║           ║
                             ║           ║
                             ║           ║
                             ║           ║
                             ║           ║
                             ║           ║
                             ║           ║
                             ║           ║      ┌─────────┐
  ┌───────────────────┐      ║           ╟──►───┤ SCRSIZE │
  │ $T  for color,log ├───◄──╢output     ║      └──┬──────┘
  └────────────────┬──┘ ┌──►─╢           ╟────◄────┘
                   └──►─┘    ╚═══════════╝

```

