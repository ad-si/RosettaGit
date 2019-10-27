+++
title = "$SPELL.REX"
description = ""
date = 2019-01-01T11:55:01Z
aliases = []
[extra]
id = 12874
[taxonomies]
categories = []
tags = []
+++

==$SPELL#.REX==
This is the   '''$SPELL#.REX'''   (REXX) program.

It is invoked (called) from other REXX programs to spell numbers with various options, including spelling the number as an ordinal number.


The help for the   '''$SPELL#.REX'''   REXX program is included here   ──►   [[$SPELL.HEL|$SPELL#.HEL]].


The   '''$SPELL#.REX'''   REXX program makes use of   '''$ERR.REX'''   REXX program which is used to display error messages (via   '''$T.REX''');   error messages are displayed in red if the terminal supports color. 

The   '''$ERR.REX'''   REXX program is included here   ──►   [[$ERR.REX]].


The   '''$SPELL#.REX'''   REXX program makes use of   '''$T.REX'''   REXX program which is used to display text messages. 

The   '''$T.REX'''   REXX program is included here   ──►   [[$T.REX]].

```rexx
/*REXX program converts a numeric string into English words, with support for ordinals, */
/*─────────────── some national currency symbols, decimal fractions, and other options. */

trace off                                        /*suppress superfluous return codes.   */

parse arg !                                      /*save the original arguments in ! var.*/

if !all( arg() )  then exit                      /*exit pgm if user wanted documentation*/
if !cms           then address ''

signal on halt                                   /*establish a method to handle  HALTs. */
signal on noValue                                /*handle uninitialized REXX variables. */
signal on syntax                                 /*   "   REXX syntax errors in this pgm*/
                                                 /* [↓]  set some REXX variables & such.*/
@abc     = 'abcdefghijklmnopqrstuvwxyz'
@abcU    = @abc
                   upper @abcU
american = 0                                     /*express numbers in American notation.*/
english  = 0                                     /*   "       "     " English      "    */
asayear  = 0                                     /*   "       "    as a year.           */
ordinal  = 0                                     /*   "       "    as an ordinal.      */
leadingz =                                       /*   "    leading zeros in numbers.    */

asis     = 0
blanks   =
british  = 0
clear    = 0
colors   = !cms  |  !pcrexx  |  !r4  |  !roo     /*this terminal supports colors.       */
dot      =
exponents=
fractions=
leading  =
logs     = 0
n        =
ops      = space(!!)
quiet    = 0
sep      = 'none'                                /*a decimal number period separator.   */
single   = 0
tfid     =                                       /*a temporary file id  (if needed).    */
tops     =                                       /*options used for the   $T  REXX pgm. */
xcurr    =

andcent  = 'and'                                 /*English phrase for:  "and xxx cents" */

minus    = 'minus '                              /*   "    word for a  minus sign.      */
nothing  = 'zero'                                /*   "      "   "  "  zero digit.      */
plus     = 'plus '                               /*   "      "   "  "  plus sign.       */
point    = 'point'                               /*   "      "   "  "  decimal point.   */
zero     = 'oh'                                  /*   "      "   "  "  zero.            */

power    = 'power'                               /*phrase when #'s are raised to a power*/
raised   = 'times ten raised to the'             /*   "     "   "   "     "    " "   "  */

cent     = 'cent'                                /*English word for a  ¢ currency symbol*/
dollar   = 'dollar'                              /*   "      "   "  "  $     "       "  */
euro     = 'euro'                                /*   "      "   "  "  ε     "       "  */
franc    = 'franc'                               /*   "      "   "  "  ƒ     "       "  */
piseta   = 'piseta'                              /*   "      "   "  "  ₧     "       "  */
pound    = 'pound'                               /*   "      "   "  "  £     "       "  */
yen      = 'yen'                                 /*   "      "   "  "  ¥     "       "  */

  do  while  ops\==''                            /*process user arguments and options.  */
  parse var  ops  _1  2  1  _  .  1  _o  ops     /*pull assunder some parts of an option*/
  upper _                                        /*uppercase the   _    variable.       */

    select
    when isnum(_)                  then n=n || _ /*if numeric,  then append this digit. */
    when _==',' | _=="(" | _==')'  then nop      /*ignore any commas  and  parentheses. */
    when _1=='.' & pos("=",_)\==0  then tops=tops _o
    when abbn('AMERican')          then american= no()
    when abbn('ASAYEAR') |,
         abbn('ASYEAR') |,
         abbn('YEAR')              then asayear=  no()
    when  abb('BEINGRAISed') |,
          abb('RAISEd')            then raised=   na()
    when  abb('BLANKs')            then blanks=   na()
    when abbn('BRITish')           then british=  no()
    when  abb('CENTs')             then cent=     na()
    when abbn('CLearscreen')       then clear=    no()
    when abbn('COLORs')            then colors=   no()
    when  abb('DECIMALpoints') |,
          abb('DOLLARs')           then dollar=   na()
    when abbn('ENGlish')           then english=  no()
    when  abb('EUROs')             then euro=     na()
    when  abb('FRANCs')            then franc=    na()
    when  abb('LEADINGzeroes') |,
          abb('LEADINGzeros')      then leading=  na()
    when abbn('LOGs')              then logs=     no()
    when  abb('MINUSsigns') |,
          abb('MINUSes')           then minus=    na()' '
    when  abb('NOTHINGs')          then nothing=  na()
    when abbn('ORDinal')           then ordinal=  no()
    when  abb('PISETAs')           then piseta=   na()
    when  abb('POWer')             then power=    na()
    when  abb('PLUSsigns') |,
          abb('PLUSes')            then plus=     na()' '
    when  abb('POINTs')            then point=    na()
    when  abb('POUNDs')            then pound=    na()
    when abbn('Quiet')             then quiet=    no()
    when  abb('SEPerators')        then sep=      na()
    when abbn('SINGley')           then single=   no()
    when  abb('YENs')              then yen=      na()
    when  abb('ZEROs') |,
          abb('ZEROes')            then zero=     na()
    otherwise                           n=n || _o
    end   /*select*/

  end     /*while  ops···*/

if sep=='none'  then if pos(',', n)\==0  then sep=','
if sep=='none'  then sep=
ogn=n                                           /*the original number that was entered. */
_=blanks                                        /*validate the    BLANKS=   option.     */
l=length(_)

if l >3  then call er 55,_

if l==3  then do                                /*it's a decimal value for  BLANKS.     */
              if \isint(_)    then call er 92, _ "BLANK="
              if _<0 | _>255  then call er 81, 0 255 _ "BLANKS="
              blanks=d2c(_)
              end

if l==2  then do                                /*it's a hexadecimal value for  BLANKS. */
              if \ishex(_)  then call er 40,_
              blanks=x2c(_)
              end

if colors  then tops= '.P=1 .A=1 .C=green'  tops    /*colors  used by the  $T  program. */
if logs    then tops= '.F='gettfid(,"ANS")  tops    /*logfile   "   "  "   $T     "     */
tops=space(tops)                                    /*options   "   "  "   $T     "     */

if n==''  then call er 54                           /*oops─ay,  no arguments entered.   */

if asayear   & ordinal              then call er 61, 'ASAYEAR ORDINAL'
if single    & ordinal              then call er 61, 'SINGLEY ORDINAL'
if american  & english              then call er 61, 'AMERICAN ENGLISH'
if american  & british              then call er 61, 'AMERICAN BRITISH'
if \american & \english & \british  then american=1
if english                          then british =1

if clear  then !cls                                 /*the terminal screen to be cleared?*/

dig.  =
dig.0 = zero
dig.1 = 'one'
dig.2 = 'two'
dig.3 = 'three'
dig.4 = 'four'
dig.5 = 'five'
dig.6 = 'six'
dig.7 = 'seven'
dig.8 = 'eight'
dig.9 = 'nine'

_='0 thousand m b tr quadr quint sext sept oct non dec undec duodec tredec quattuordec quinquadec sedec septendec octodec novendec vigin unvigin duovigin tresvigin quattuorvigin quinquavigin sesvigin septemvigin octovigin novemvigin trigin'
_=_ 'untrigin duotrigin trestrigin quattuortrigin quinquatrigin sestrigin septentrigin octotrigin noventrigin quadragin unquadragin duoquadragin tresquadragin quattuorquadragin quinquaquadragin sesquadragin septenquadragin octoquadragin'
_=_ 'novenquadragin quinquagin unquinquagin duoquinquagin tresquinquagin quattuorquinquagin quinquaquinquagin sesquinquagin septenquinquagin octoquinquagin novenquinquagin sexagin unsexagin duosexagin tresexagin quattuorsexagin quinquasexagin'
_=_ 'sesexagin septensexagin octosexagin novensexagin septuagin unseptuagin duoseptuagin treseptuagin quattuorseptuagin quinquaseptuagin seseptuagin septenseptuagin octoseptuagin novenseptuagin octogin unoctogin duooctogin tresoctogin'
_=_ 'quattuoroctogin quinquaoctogin sexoctogin septemoctogin octooctogin novemoctogin nonagin unnonagin duononagin trenonagin quattuornonagin quinquanonagin senonagin septenonagin octononagin novenonagin cen uncen duocen trescen quattuorcen'
_=_ 'quinquacen sexcen septencen octocen novencen decicen undecicen duodecicen tredecicen quattuordecicen quinquadecicen sedecicen septendecicen octodecicen novendecicen viginticen unviginticen duoviginticen tresviginticen quattuorviginticen'
_=_ 'quinquaviginticen sesviginticen septemviginticen octoviginticen novemviginticen trigintacen untrigintacen duotrigintacen trestrigintacen quattuortrigintacen quinquatrigintacen sestrigintacen septentrigintacen octotrigintacen'
_=_ 'noventrigintacen quadragintacen unquadragintacen duoquadragintacen tresquadragintacen quattuorquadragintacen quinquaquadragintacen sesquadragintacen septenquadragintacen octoquadragintacen novenquadragintacen quinquagintacen'
_=_ 'unquinquagintacen duoquinquagintacen tresquinquagintacen quattuorquinquagintacen quinquaquinquagintacen sesquinquagintacen septenquinquagintacen octoquinquagintacen novenquinquagintacen sexagintacen unsexagintacen duosexagintacen'
_=_ 'tresexagintacen quattuorsexagintacen quinquasexagintacen sesexagintacen septensexagintacen octosexagintacen novensexagintacen septuagintacen unseptuagintacen duoseptuagintacen treseptuagintacen quattuorseptuagintacen quinquaseptuagintacen'
_=_ 'seseptuagintacen septenseptuagintacen octoseptuagintacen novenseptuagintacen octogintacen unoctogintacen duooctogintacen tresoctogintacen quattuoroctogintacen quinquaoctogintacen sexoctogintacen septemoctogintacen octooctogintacen'
_=_ 'novemoctogintacen nonagintacen unnonagintacen duononagintacen trenonagintacen quattuornonagintacen quinquanonagintacen senonagintacen septenonagintacen octononagintacen novenonagintacen ducen unducen duoducen treducen quattuorducen'
_=_ 'quinquaducen seducen septenducen octoducen novenducen deciducen undeciducen duodeciducen tredeciducen quattuordeciducen quinquadeciducen sedeciducen septendeciducen octodeciducen novendeciducen vigintiducen unvigintiducen duovigintiducen'
_=_ 'tresvigintiducen quattuorvigintiducen quinquavigintiducen sesvigintiducen septemvigintiducen octovigintiducen novemvigintiducen trigintaducen untrigintaducen duotrigintaducen trestrigintaducen quattuortrigintaducen quinquatrigintaducen'
_=_ 'sestrigintaducen septentrigintaducen octotrigintaducen noventrigintaducen quadragintaducen unquadragintaducen duoquadragintaducen tresquadragintaducen quattuorquadragintaducen quinquaquadragintaducen sesquadragintaducen'
_=_ 'septenquadragintaducen octoquadragintaducen novenquadragintaducen quinquagintaducen unquinquagintaducen duoquinquagintaducen tresquinquagintaducen quattuorquinquagintaducen quinquaquinquagintaducen sesquinquagintaducen'
_=_ 'septenquinquagintaducen octoquinquagintaducen novenquinquagintaducen sexagintaducen unsexagintaducen duosexagintaducen tresexagintaducen quattuorsexagintaducen quinquasexagintaducen sesexagintaducen septensexagintaducen octosexagintaducen'
_=_ 'novensexagintaducen septuagintaducen unseptuagintaducen duoseptuagintaducen treseptuagintaducen quattuorseptuagintaducen quinquaseptuagintaducen seseptuagintaducen septenseptuagintaducen octoseptuagintaducen novenseptuagintaducen'
_=_ 'octogintaducen unoctogintaducen duooctogintaducen tresoctogintaducen quattuoroctogintaducen quinquaoctogintaducen sexoctogintaducen septemoctogintaducen octooctogintaducen novemoctogintaducen nonagintaducen unnonagintaducen'
_=_ 'duononagintaducen trenonagintaducen quattuornonagintaducen quinquanonagintaducen senonagintaducen septenonagintaducen octononagintaducen novenonagintaducen trecen untrecen duotrecen trestrecen quattuortrecen quinquatrecen sestrecen'
_=_ 'septentrecen octotrecen noventrecen decitrecen undecitrecen duodecitrecen tredecitrecen quattuordecitrecen quinquadecitrecen sedecitrecen septendecitrecen octodecitrecen novendecitrecen vigintitrecen unvigintitrecen duovigintitrecen'
_=_ 'tresvigintitrecen quattuorvigintitrecen quinquavigintitrecen sesvigintitrecen septemvigintitrecen octovigintitrecen novemvigintitrecen trigintatrecen untrigintatrecen duotrigintatrecen trestrigintatrecen quattuortrigintatrecen'
_=_ 'quinquatrigintatrecen sestrigintatrecen septentrigintatrecen octotrigintatrecen noventrigintatrecen quadragintatrecen unquadragintatrecen duoquadragintatrecen tresquadragintatrecen quattuorquadragintatrecen quinquaquadragintatrecen'
_=_ 'sesquadragintatrecen septenquadragintatrecen octoquadragintatrecen novenquadragintatrecen quinquagintatrecen unquinquagintatrecen duoquinquagintatrecen tresquinquagintatrecen quattuorquinquagintatrecen quinquaquinquagintatrecen'
_=_ 'sesquinquagintatrecen septenquinquagintatrecen octoquinquagintatrecen novenquinquagintatrecen sexagintatrecen unsexagintatrecen duosexagintatrecen tresexagintatrecen quattuorsexagintatrecen quinquasexagintatrecen sesexagintatrecen'
_=_ 'septensexagintatrecen octosexagintatrecen novensexagintatrecen septuagintatrecen unseptuagintatrecen duoseptuagintatrecen treseptuagintatrecen quattuorseptuagintatrecen quinquaseptuagintatrecen seseptuagintatrecen septenseptuagintatrecen'
_=_ 'octoseptuagintatrecen novenseptuagintatrecen octogintatrecen unoctogintatrecen duooctogintatrecen tresoctogintatrecen quattuoroctogintatrecen quinquaoctogintatrecen sexoctogintatrecen septemoctogintatrecen octooctogintatrecen'
_=_ 'novemoctogintatrecen nonagintatrecen unnonagintatrecen duononagintatrecen trenonagintatrecen quattuornonagintatrecen quinquanonagintatrecen senonagintatrecen septenonagintatrecen octononagintatrecen novenonagintatrecen quadringen'
_=_ 'unquadringen duoquadringen tresquadringen quattuorquadringen quinquaquadringen sesquadringen septenquadringen octoquadringen novenquadringen deciquadringen undeciquadringen duodeciquadringen tredeciquadringen quattuordeciquadringen'
_=_ 'quinquadeciquadringen sedeciquadringen septendeciquadringen octodeciquadringen novendeciquadringen vigintiquadringen unvigintiquadringen duovigintiquadringen tresvigintiquadringen quattuorvigintiquadringen quinquavigintiquadringen'
_=_ 'sesvigintiquadringen septemvigintiquadringen octovigintiquadringen novemvigintiquadringen trigintaquadringen untrigintaquadringen duotrigintaquadringen trestrigintaquadringen quattuortrigintaquadringen quinquatrigintaquadringen'
_=_ 'sestrigintaquadringen septentrigintaquadringen octotrigintaquadringen noventrigintaquadringen quadragintaquadringen unquadragintaquadringen duoquadragintaquadringen tresquadragintaquadringen quattuorquadragintaquadringen'
_=_ 'quinquaquadragintaquadringen sesquadragintaquadringen septenquadragintaquadringen octoquadragintaquadringen novenquadragintaquadringen quinquagintaquadringen unquinquagintaquadringen duoquinquagintaquadringen tresquinquagintaquadringen'
_=_ 'quattuorquinquagintaquadringen quinquaquinquagintaquadringen sesquinquagintaquadringen septenquinquagintaquadringen octoquinquagintaquadringen novenquinquagintaquadringen sexagintaquadringen unsexagintaquadringen duosexagintaquadringen'
_=_ 'tresexagintaquadringen quattuorsexagintaquadringen quinquasexagintaquadringen sesexagintaquadringen septensexagintaquadringen octosexagintaquadringen novensexagintaquadringen septuagintaquadringen unseptuagintaquadringen'
_=_ 'duoseptuagintaquadringen treseptuagintaquadringen quattuorseptuagintaquadringen quinquaseptuagintaquadringen seseptuagintaquadringen septenseptuagintaquadringen octoseptuagintaquadringen novenseptuagintaquadringen octogintaquadringen'
_=_ 'unoctogintaquadringen duooctogintaquadringen tresoctogintaquadringen quattuoroctogintaquadringen quinquaoctogintaquadringen sexoctogintaquadringen septemoctogintaquadringen octooctogintaquadringen novemoctogintaquadringen'
_=_ 'nonagintaquadringen unnonagintaquadringen duononagintaquadringen trenonagintaquadringen quattuornonagintaquadringen quinquanonagintaquadringen senonagintaquadringen septenonagintaquadringen octononagintaquadringen novenonagintaquadringen'
_=_ 'quingen unquingen duoquingen tresquingen quattuorquingen quinquaquingen sesquingen septenquingen octoquingen novenquingen deciquingen undeciquingen duodeciquingen tredeciquingen quattuordeciquingen quinquadeciquingen sedeciquingen'
_=_ 'septendeciquingen octodeciquingen novendeciquingen vigintiquingen unvigintiquingen duovigintiquingen tresvigintiquingen quattuorvigintiquingen quinquavigintiquingen sesvigintiquingen septemvigintiquingen octovigintiquingen'
_=_ 'novemvigintiquingen trigintaquingen untrigintaquingen duotrigintaquingen trestrigintaquingen quattuortrigintaquingen quinquatrigintaquingen sestrigintaquingen septentrigintaquingen octotrigintaquingen noventrigintaquingen quadragintaquinge'
_=_ 'unquadragintaquingen duoquadragintaquingen tresquadragintaquingen quattuorquadragintaquingen quinquaquadragintaquingen sesquadragintaquingen septenquadragintaquingen octoquadragintaquingen novenquadragintaquingen quinquagintaquingen'
_=_ 'unquinquagintaquingen duoquinquagintaquingen tresquinquagintaquingen quattuorquinquagintaquingen quinquaquinquagintaquingen sesquinquagintaquingen septenquinquagintaquingen octoquinquagintaquingen novenquinquagintaquingen sexagintaquingen'
_=_ 'unsexagintaquingen duosexagintaquingen tresexagintaquingen quattuorsexagintaquingen quinquasexagintaquingen sesexagintaquingen septensexagintaquingen octosexagintaquingen novensexagintaquingen septuagintaquingen unseptuagintaquingen'
_=_ 'duoseptuagintaquingen treseptuagintaquingen quattuorseptuagintaquingen quinquaseptuagintaquingen seseptuagintaquingen septenseptuagintaquingen octoseptuagintaquingen novenseptuagintaquingen octogintaquingen unoctogintaquingen'
_=_ 'duooctogintaquingen tresoctogintaquingen quattuoroctogintaquingen quinquaoctogintaquingen sexoctogintaquingen septemoctogintaquingen octooctogintaquingen novemoctogintaquingen nonagintaquingen unnonagintaquingen duononagintaquingen'
_=_ 'trenonagintaquingen quattuornonagintaquingen quinquanonagintaquingen senonagintaquingen septenonagintaquingen octononagintaquingen novenonagintaquingen sescen unsescen duosescen tresescen quattuorsescen quinquasescen sesescen septensescen'
_=_ 'octosescen novensescen decisescen undecisescen duodecisescen tredecisescen quattuordecisescen quinquadecisescen sedecisescen septendecisescen octodecisescen novendecisescen vigintisescen unvigintisescen duovigintisescen tresvigintisescen'
_=_ 'quattuorvigintisescen quinquavigintisescen sesvigintisescen septemvigintisescen octovigintisescen novemvigintisescen trigintasescen untrigintasescen duotrigintasescen trestrigintasescen quattuortrigintasescen quinquatrigintasescen'
_=_ 'sestrigintasescen septentrigintasescen octotrigintasescen noventrigintasescen quadragintasescen unquadragintasescen duoquadragintasescen tresquadragintasescen quattuorquadragintasescen quinquaquadragintasescen sesquadragintasescen'
_=_ 'septenquadragintasescen octoquadragintasescen novenquadragintasescen quinquagintasescen unquinquagintasescen duoquinquagintasescen tresquinquagintasescen quattuorquinquagintasescen quinquaquinquagintasescen sesquinquagintasescen'
_=_ 'septenquinquagintasescen octoquinquagintasescen novenquinquagintasescen sexagintasescen unsexagintasescen duosexagintasescen tresexagintasescen quattuorsexagintasescen quinquasexagintasescen sesexagintasescen septensexagintasescen'
_=_ 'octosexagintasescen novensexagintasescen septuagintasescen unseptuagintasescen duoseptuagintasescen treseptuagintasescen quattuorseptuagintasescen quinquaseptuagintasescen seseptuagintasescen septenseptuagintasescen octoseptuagintasescen'
_=_ 'novenseptuagintasescen octogintasescen unoctogintasescen duooctogintasescen tresoctogintasescen quattuoroctogintasescen quinquaoctogintasescen sexoctogintasescen septemoctogintasescen octooctogintasescen novemoctogintasescen nonagintasesce'
_=_ 'unnonagintasescen duononagintasescen trenonagintasescen quattuornonagintasescen quinquanonagintasescen senonagintasescen septenonagintasescen octononagintasescen novenonagintasescen septingen unseptingen duoseptingen treseptingen'
_=_ 'quattuorseptingen quinquaseptingen seseptingen septenseptingen octoseptingen novenseptingen deciseptingen undeciseptingen duodeciseptingen tredeciseptingen quattuordeciseptingen quinquadeciseptingen sedeciseptingen septendeciseptingen'
_=_ 'octodeciseptingen novendeciseptingen vigintiseptingen unvigintiseptingen duovigintiseptingen tresvigintiseptingen quattuorvigintiseptingen quinquavigintiseptingen sesvigintiseptingen septemvigintiseptingen octovigintiseptingen'
_=_ 'novemvigintiseptingen trigintaseptingen untrigintaseptingen duotrigintaseptingen trestrigintaseptingen quattuortrigintaseptingen quinquatrigintaseptingen sestrigintaseptingen septentrigintaseptingen octotrigintaseptingen'
_=_ 'noventrigintaseptingen quadragintaseptingen unquadragintaseptingen duoquadragintaseptingen tresquadragintaseptingen quattuorquadragintaseptingen quinquaquadragintaseptingen sesquadragintaseptingen septenquadragintaseptingen'
_=_ 'octoquadragintaseptingen novenquadragintaseptingen quinquagintaseptingen unquinquagintaseptingen duoquinquagintaseptingen tresquinquagintaseptingen quattuorquinquagintaseptingen quinquaquinquagintaseptingen sesquinquagintaseptingen'
_=_ 'septenquinquagintaseptingen octoquinquagintaseptingen novenquinquagintaseptingen sexagintaseptingen unsexagintaseptingen duosexagintaseptingen tresexagintaseptingen quattuorsexagintaseptingen quinquasexagintaseptingen sesexagintaseptingen'
_=_ 'septensexagintaseptingen octosexagintaseptingen novensexagintaseptingen septuagintaseptingen unseptuagintaseptingen duoseptuagintaseptingen treseptuagintaseptingen quattuorseptuagintaseptingen quinquaseptuagintaseptingen'
_=_ 'seseptuagintaseptingen septenseptuagintaseptingen octoseptuagintaseptingen novenseptuagintaseptingen octogintaseptingen unoctogintaseptingen duooctogintaseptingen tresoctogintaseptingen quattuoroctogintaseptingen quinquaoctogintaseptingen'
_=_ 'sexoctogintaseptingen septemoctogintaseptingen octooctogintaseptingen novemoctogintaseptingen nonagintaseptingen unnonagintaseptingen duononagintaseptingen trenonagintaseptingen quattuornonagintaseptingen quinquanonagintaseptingen'
_=_ 'senonagintaseptingen septenonagintaseptingen octononagintaseptingen novenonagintaseptingen octingen unoctingen duooctingen tresoctingen quattuoroctingen quinquaoctingen sexoctingen septemoctingen octooctingen novemoctingen decioctingen'
_=_ 'undecioctingen duodecioctingen tredecioctingen quattuordecioctingen quinquadecioctingen sedecioctingen septendecioctingen octodecioctingen novendecioctingen vigintioctingen unvigintioctingen duovigintioctingen tresvigintioctingen'
_=_ 'quattuorvigintioctingen quinquavigintioctingen sesvigintioctingen septemvigintioctingen octovigintioctingen novemvigintioctingen trigintaoctingen untrigintaoctingen duotrigintaoctingen trestrigintaoctingen quattuortrigintaoctingen'
_=_ 'quinquatrigintaoctingen sestrigintaoctingen septentrigintaoctingen octotrigintaoctingen noventrigintaoctingen quadragintaoctingen unquadragintaoctingen duoquadragintaoctingen tresquadragintaoctingen quattuorquadragintaoctingen'
_=_ 'quinquaquadragintaoctingen sesquadragintaoctingen septenquadragintaoctingen octoquadragintaoctingen novenquadragintaoctingen quinquagintaoctingen unquinquagintaoctingen duoquinquagintaoctingen tresquinquagintaoctingen'
_=_ 'quattuorquinquagintaoctingen quinquaquinquagintaoctingen sesquinquagintaoctingen septenquinquagintaoctingen octoquinquagintaoctingen novenquinquagintaoctingen sexagintaoctingen unsexagintaoctingen duosexagintaoctingen tresexagintaoctingen'
_=_ 'quattuorsexagintaoctingen quinquasexagintaoctingen sesexagintaoctingen septensexagintaoctingen octosexagintaoctingen novensexagintaoctingen septuagintaoctingen unseptuagintaoctingen duoseptuagintaoctingen treseptuagintaoctingen'
_=_ 'quattuorseptuagintaoctingen quinquaseptuagintaoctingen seseptuagintaoctingen septenseptuagintaoctingen octoseptuagintaoctingen novenseptuagintaoctingen octogintaoctingen unoctogintaoctingen duooctogintaoctingen tresoctogintaoctingen'
_=_ 'quattuoroctogintaoctingen quinquaoctogintaoctingen sexoctogintaoctingen septemoctogintaoctingen octooctogintaoctingen novemoctogintaoctingen nonagintaoctingen unnonagintaoctingen duononagintaoctingen trenonagintaoctingen'
_=_ 'quattuornonagintaoctingen quinquanonagintaoctingen senonagintaoctingen septenonagintaoctingen octononagintaoctingen novenonagintaoctingen nongen unnongen duonongen trenongen quattuornongen quinquanongen senongen septenongen octonongen'
_=_ 'novenongen decinongen undecinongen duodecinongen tredecinongen quattuordecinongen quinquadecinongen sedecinongen septendecinongen octodecinongen novendecinongen vigintinongen unvigintinongen duovigintinongen tresvigintinongen'
_=_ 'quattuorvigintinongen quinquavigintinongen sesvigintinongen septemvigintinongen octovigintinongen novemvigintinongen trigintanongen untrigintanongen duotrigintanongen trestrigintanongen quattuortrigintanongen quinquatrigintanongen'
_=_ 'sestrigintanongen septentrigintanongen octotrigintanongen noventrigintanongen quadragintanongen unquadragintanongen duoquadragintanongen tresquadragintanongen quattuorquadragintanongen quinquaquadragintanongen sesquadragintanongen'
_=_ 'septenquadragintanongen octoquadragintanongen novenquadragintanongen quinquagintanongen unquinquagintanongen duoquinquagintanongen tresquinquagintanongen quattuorquinquagintanongen quinquaquinquagintanongen sesquinquagintanongen'
_=_ 'septenquinquagintanongen octoquinquagintanongen novenquinquagintanongen sexagintanongen unsexagintanongen duosexagintanongen tresexagintanongen quattuorsexagintanongen quinquasexagintanongen sesexagintanongen septensexagintanongen'
_=_ 'octosexagintanongen novensexagintanongen septuagintanongen unseptuagintanongen duoseptuagintanongen treseptuagintanongen quattuorseptuagintanongen quinquaseptuagintanongen seseptuagintanongen septenseptuagintanongen octoseptuagintanongen'
_=_ 'novenseptuagintanongen octogintanongen unoctogintanongen duooctogintanongen tresoctogintanongen quattuoroctogintanongen quinquaoctogintanongen sexoctogintanongen septemoctogintanongen octooctogintanongen novemoctogintanongen nonagintanonge'
_=_ 'unnonagintanongen duononagintanongen trenonagintanongen quattuornonagintanongen quinquanonagintanongen senonagintanongen septenonagintanongen novenonagintanongen octononagintanongen'

amers=words(_)
                         do j=1  for amers
                         a.j=word(_, j)
                         end   /*j*/
maxzlen=amers *3

if british  then do
                 engs   =amers*2 - 2
                 maxzlen=engs *3
                                            do k=1  for 2
                                            b.k=a.k
                                            end   /*k*/
                         do j=3  to amers
                         _=j*2 - 3
                         b._=a.j
                         _n=_ + 1
                         b._n=a.j
                         end   /*j*/
                 drop a.
                 end

n=space(n)
_=left(n, 1)
sig=

if _=='-' | _=="+"  then do                               /*handle leading sign (+ -).  */
                         if _=='+'  then sig= plus
                                    else sig=minus
                         n=substr(n, 2)                   /*remove the leading sign.    */
                         end

numeric digits 80 + maxzlen

zpoints =countstr('.',n); zpoint =zpoints \==0; if zpoints >1 then call er 59,'decimal-points(.)'
zdollars=countstr('$',n); zdollar=zdollars\==0; if zdollars>1 then call er 59,'dollar-signs($)'
zeuros  =countstr('ε',n); zeuro  =zeuros  \==0; if zeuros  >1 then call er 59,'euro-signs(ε)'
zfrancs =countstr('ƒ',n); zfranc =zfrancs \==0; if zfrancs >1 then call er 59,'franc-signs(ƒ)'
zpounds =countstr('£',n); zpound =zpounds \==0; if zpounds >1 then call er 59,'pound-signs(£)'
zyens   =countstr('¥',n); zyen   =zyens   \==0; if zyens   >1 then call er 59,'yen-signs(¥)'
zpisetas=countstr('₧',n); zpiseta=zpisetas\==0; if zpisetas>1 then call er 59,'piseta-signs(₧)'
zcents  =countstr('¢',n); zcent  =zcents  \==0; if zcents  >1 then call er 59,'cent-signs(¢)'

zcurrs  =zdollars + zeuros + zpounds + zyens + zpisetas + zfrancs + zcents
zcurr   =zcurrs\==0

if zcurrs>2  then call er 59, 'currency symbols'

if zdollar   then do;    xcurr=dollar;    n=changestr("$", n, '');    end
if zeuro     then do;    xcurr=euro;      n=changestr("ε", n, '');    end
if zpound    then do;    xcurr=pound;     n=changestr("£", n, '');    end
if zyen      then do;    xcurr=yen;       n=changestr("¥", n, '');    end
if zpiseta   then do;    xcurr=piseta;    n=changestr("₧", n, '');    end
if zfranc    then do;    xcurr=franc;     n=changestr("ƒ", n, '');    end
if zcent     then do;    xcurr=cent;      n=changestr("¢", n, '');    end

if zpoint    then dot=point                      /*the number has a decimal point.      */

_=right(n, 1)                                    /*pick off right─most character of num.*/
if ismix(_) | _=='!'  then n=num(n)              /*if number has a suffix,  convert it. */

parse upper var  n    n  'E'  exponent           /*parse the exponent, if present.      */
parse       var  n    n  '.'  fraction

leadzs=compare(n, copies(0, digits()) ) - 1      /*count the leading zeroes in number.  */
n=changestr(',', n, "")                          /*change commas to nulls  (delete 'em).*/

if isnum(n)  then do                             /*this DO structure must be presered···*/
                  if \asis  then n=n/1
                  end                            /*elsewise the THEN/ELSEs aren't paired.*/

             else do 1
                  if n==''  then leave           /*this LEAVE is why there is a  DO  1  */
                  expression=n
                  interpret  'number='expression
                  n=number % 1                   /*reduce  NUMBER  to an integer.       */
                  end   /*1*/                    /* [↑]   same as  TRUNC(number)        */

if xcurr\==''  &  xcurr\==zyen  &  n\==1  then xcurr=xcurr's'      /*need to ba plural? */

max#=10**maxzlen * 1000  -  1
if \asis  & isnum(n)   then n=n/1

if n\=='' & isnum(n)   then if n>max#  then call er 81, -max#  max#  ogn   /*in range ? */
if n\=='' & \isint(n)  then call er 53, n                                  /*numeric ?  */
if abs(n)>max#         then call er 81, -max# max# ogn                     /*in range ? */

if leading\==''        then leadingz=copies(leading' ',leadzs)

if asayear & right(n, 3)\==000  then do
                                     #=spnte( left(n, max(0, length(n) - 2) ) )
                                     if #==zero       then #=
                                     _2=right(n, 2)
                                     _=spnte(_2)
                                     if _==zero       then _= "hundred"
                                     if _2<10 & _2>0  then _= zero _
                                     #=sig || # _
                                     end

                                else #=sig || leadingz || spnte(n)

if ordinal  then do
                 sx=
                 w=words(#)
                 p=word(#, w)
                 oldp=p
                 pp=
                 if pos('-', p)\==0  then parse var  p   pp "-" p
                 if pp\==''          then pp=pp"-"

                      select                                  /* [↓]  adjust some words.*/
                      when p=='one'          then p= "first"
                      when p=='two'          then p= "second"
                      when p=='three'        then p= "third"
                      when p=='five'         then p= "fifth"
                      when p=='eight'        then p= "eighth"
                      when p=='nine'         then p= "ninth"
                      when p=='twelve'       then p= "twelfth"
                      when right(p, 1)=='y'  then p= left(p, length(p) - 1)"ieth"
                      otherwise                  sx= 'th'
                      end   /*select*/

                 if p\==oldp  then if w==1  then #=pp || p
                                            else #=subword(#, 1, w-1)  pp  ||  p
                 #=# || sx
                 end

Lf=length(fraction)
if zdollar & (Lf==1 | Lf==2)  then do
                                   if fraction\=1  then cent=cent's'
                                   fractions= sp(fraction)  cent
                                   dot=xcurr  andcent
                                   xcurr=
                                   end

                              else do j=1  for Lf
                                   _=substr(fraction, j, 1)
                                   fractions= fractions  dig._
                                   end   /*j*/

if exponent\==''  then do
                       if \isint(exponent)  then call er 53, exponent 'exponent'
                       _=sp(exponent  "ORDINAL")
                       if _\==''            then exponents=raised _ power
                       end

after=space(dot fractions exponents xcurr)
   if after\==''       then after=" "after
#=translate(# || after, , "_")
   if blanks\==''      then #=translate(#, blanks, " ")
   if \quiet           then call $t tops #
return #

/*──────────────────────────────────────────────────────────────────────────────────────*/
spnte: parse arg zz
       if zz==0 | zz=' '  then return nothing
       en=
       bzz=reverse(zz)
                        has_t=22
       if british  then has_t=41

       if single then do j=1  for length(zz)
                      _=substr(zz, j, 1)
                      en=en dig._
                      end   /*j*/

                 else do j=1  to maxzlen  by 3
                      _=(j+2) % 3
                      if american    then zillion=a._
                                     else zillion=b._
                      if zillion==0  then zillion=
                      if _>=has_t    then zillion=zillion't'

                      if _>2 then if american  then zillion=zillion'illion'
                                               else if _//2  then zillion=zillion'illion'
                                                             else zillion=zillion'illiard'

                      ttt=reverse( substr(bzz, j, 3))
                            if ttt=='   '  then leave
                      ttt=right( strip(ttt), 3, 0)
                            if ttt== 000   then iterate
                      x=sphtu(ttt)  zillion
                            if en\==''     then en=sep || en
                      en=x  en
                      end   /*j*/

       en=strip( translate(en, , "_") )

       if en==''  then if zcurr  then do           /*this  DO  structure must be intact.*/
                                      if \zcent  then en="no"
                                      end

                                 else en=zero
       return en

/*──────────────────────────────────────────────────────────────────────────────────────*/
sphtu: procedure; parse arg z                      /*SPell Hundred Tens Units.          */
       @987= 'nine eight seven six five four three two one'

       zm= substr(z, 2, 1)
       zr= right(z, 1)
       zh= word(@987, 10 - left(z, 1))

       if zh\==''  then zh=zh  "hundred"

       zt= word('ninety eighty seventy sixty fifty forty thirty twenty',  10 - zm)
       zu= word(@987, 10 - zr)

       @teens= 'ten eleven twelve thir four fif six seven eigh nine'

       if zm==1 then  do
                      zu=
                      zt=word(@teens, zr+1)
                      if zr>2 then zt=zt'teen'
                      end

       if zt\==''  &  zu\==""  then do
                                    zt=zt'-'zu
                                    zu=
                                    end
       return space(zh zt zu)

/*═════════════════════════════general 1─line subs══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════*/
!all:  !!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=='NT';!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,'? ?SAMPLES ?AUTHOR ?FLOW')==0 then return 0;!call=']$H';call '$H' !fn !;!call=;return 1
!cal:      if symbol('!CALL')\=="VAR" then !call=;  return !call
!env:      !env='ENVIRONMENT';  if !sys=="MSDOS" | !brexx | !r4 | !roo  then !env='SYSTEM';   if !os2  then !env="OS"!env;   !ebcdic=6=='f6'x;   if !crx  then !env="DOS";                                return
!fid:      parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .;  call !sys; if !dos then do; _=lastpos('\', !fn);  !fm=left(!fn, _); !fn=substr(!fn, _+1);  parse var !fn !fn "." !ft;  end;          return word(0  !fn  !ft  !fm, 1 + ("0"arg(1) ) )
!rex:      parse upper version !ver !vernum !verdate .; !brexx='BY'==!vernum; !kexx="KEXX"==!ver; !pcrexx='REXX/PERSONAL'==!ver | "REXX/PC"==!ver; !r4='REXX-R4'==!ver; !regina="REXX-REGINA"==left(!ver, 11);  !roo='REXX-ROO'==!ver;  call !env;  return
!sys:      !cms=!sys=='CMS';  !os2=!sys=="OS2";  !tso=!sys=='TSO' | !sys=="MVS";  !vse=!sys=='VSE';  !dos=pos("DOS", !sys)\==0 | pos('WIN', !sys)\==0 | !sys=="CMD";  !crx=left(!sys, 6)=='DOSCRX';   call !rex;                          return
!var:      call !fid;  if !kexx then return space(dosenv(arg(1)));  return space(value(arg(1),,!env))
$fact!:    procedure; parse arg x _ .;   L=length(x);   n=L - length( strip(x, 'T', "!"));   if n<=-n | _\=='' | arg()\==1  then return x;   z=left(x, L-n);   if z<0 | \isint(z)  then return x;   return $fact(z, n)
$fact:     procedure; parse arg x _ .; arg ,n ! .; n=p(n 1); if \isint(n) then n=0; if x<-n | \isint(x) | n<1 | _ || !\=='' | arg()>2  then return x || copies("!", max(1, n)); !=1; s=x//n; if s==0  then s=n; do j=s  to x  by n; !=!*j; end;   return !
$sfxa:     parse arg ,s,m;  arg u,c;  if pos( left(s, 2), u)\==0  then do j=length(s)  to compare(s,c) -1  by -1;  if right(u, j)\==left(c, j)  then iterate;  _=left(u, length(u) - j);  if isnum(_)  then return m*_;  leave;  end;        return arg(1)
$sfxf:     parse arg y;  if right(y, 1)=='!' then y=$fact!(y);  if \isnum(y)  then y=$sfxz();  if isnum(y)  then return y;                    return $sfxm(y)
$sfxm:     parse arg z;  arg w;  b=1000;  if right(w,1)=='I' then do; z=shorten(z); w=z; upper w; b=1024; end;  p=pos(right(w,1),'KMGTPEZYXWVU');  if p==0 then return arg(1); n=shorten(z); r=num(n,f,1);  if isnum(r) then return r*b**p;  return arg(1)
$sfxz:     return $sfxa($sfxa($sfxa($sfxa($sfxa($sfxa(y,'PAIRs',2),'DOZens',12),'SCore',20),'GREATGRoss',1728),'GRoss',144),'GOOGOLs',1e100)
$t:        !call=']$T';  call "$T" arg(1);  !call=;      return
abb:       arg abbu;  parse arg abb;                     return abbrev(abbu,_,abbl(abb))
abbl:      return verify(arg(1)'a',@abc,'M')-1
abbn:      parse arg abbn;                               return abb(abbn) | abb('NO'abbn)
countstr:  procedure;  parse arg n,h,s;  if s=='' then s=1;  w=length(n);  do r=0  until _==0;  _=pos(n, h, s);  s=_+w;  end;                return r
er:        parse arg _1,_2;  call '$ERR' "14"p(_1) p(word(_1,2) !fid(1)) _2;  if _1<0  then return _1;       exit result
err:       call er '-'arg(1),arg(2);                     return ''
erx:       call er '-'arg(1),arg(2);                     exit ''
getdtfid:  tfid=p( !var("TMP") !var('TEMP') homedrive()"\");  if substr(tfid, 2, 1)==':' & substr(tfid, 3, 1)\=="\" then tfid=insert('\', t, 2);  return strip( tfid, "T", '\')"\"arg(1)'.'arg(2)
getTFID:   if symbol('TFID')=='LIT' then tfid=; if tfid\=='' then return tfid; gfn=word(arg(1) !fn,1); gft=word(arg(2) 'ANS',1);tfid='TEMP'; if !tso then tfid=gfn'.'gft; if !cms then tfid=gfn','gft",A4";if !dos then tfid=getdTFID(gfn,gft);return tfid
halt:      call er .1
homedrive: if symbol('HOMEDRIVE')\=="VAR"  then homedrive=p( !var('HOMEDRIVE') 'C:');                      return homedrive
int:       int=num( arg(1), arg(2) );  if \isint(int)  then call er 92, arg(1)  arg(2);                      return int/1
ishex:     return datatype( arg(1), 'X')
isint:     return datatype( arg(1), 'W')
ismix:     return datatype( arg(1), 'M')
isnum:     return datatype( arg(1), 'N')
na:        if arg(1)\==''  then call er 01, arg(2);  parse var ops  na ops;  if na=='' then call er 35, _o;   return na
nai:       return int(na(), _o)
nan:       return num(na(), _o)
no:        if arg(1)\==''  then call er 01,arg(2);        return left(_, 2)\=='NO'
noValue:   !sigl=sigl;  call er 17, !fid(2)  !fid(3)  !sigl  condition('D')  sourceline(!sigl)
num:       procedure; parse arg x .,f,q; if x=='' then return x; if isnum(x) then return x/1; x=space( translate(x,,','),0); if \isnum(x) then x=$sfxf(x); if isnum(x) then return x/1;if q==1 then return x; if q=='' then call er 53,x f;call erx 53,x f
p:         return word(arg(1),1)
shorten:   procedure;  parse arg a,n;  return left(a, max(0, length(a) - p(n 1)))
sp:        !call=']'!fn;  sp="$SPELL#"(arg(1)  'Q');  !call=;            return sp
syntax:    !sigl=sigl;  call er 13, !fid(2) !fid(3) !sigl !cal() condition('D') sourceline(!sigl)
```


Note:   The subroutines and functions in the     '''══general 1-line subs══'''     section were kept to one line, elsewise, the program would be exceedingly long.   These functions and subroutines were meant to be brief and not clutter up the main program.

[[Category:REXX library routines]]
