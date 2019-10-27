+++
title = "Temperature conversion/REXX"
description = ""
date = 2019-01-01T11:53:59Z
aliases = []
[extra]
id = 18357
[taxonomies]
categories = []
tags = []
+++

This is the unabridged version of the REXX program to solve the Rosetta Code task of   ''Temperature conversion''.


Note that conversion from/to the   '''Dalton'''   temperature scale requires natural logarithms   ('''ln'''),   and the '''pow''' function. 


No commenting/explaining was affixed to the higher math functions because it would detract from the logic of the main program.

## REXX


```rexx
/*REXX program converts  temperatures  for  fifty─eight  different  temperature scales. */

/*
≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡

  tt                                                        tt
  tt                                                        tt
tttttt                                                    tttttt
tttttt    eeee   mmm  mm   ppppp    eeee   rr rr   aaaa   tttttt   uu  uu   rr rr    eee
  tt     eeeeee  mmmmmmmm  pppppp  eeeeee  rrrrrr  aaaaa    tt     uu  uu   rrrrrr  eeeeee
  tt     ee  ee  mm mm mm  pp  pp  ee  ee  rrr rr     aa    tt     uu  uu   rrr rr  ee  ee
  tt     eeeeee  mm mm mm  pp  pp  eeeeee  rr      aaaaaa   tt     uu  uu   rr      eeeeee
  tt     eeeeee  mm mm mm  pppppp  eeeeee  rr     aaaaaaa   tt     uu  uu   rr      eeeeee
  tt tt  ee      mm mm mm  ppppp   ee      rr     aa   aa   tt tt  uu  uu   rr      ee
  ttttt  eeeee   mm mm mm  pp      eeeee   rr     aaaaaaa   ttttt  uuuuuu   rr      eeeee
   ttt    eee    mm mm mm  pp       eee    rr      aaaaaaa   ttt    uuu uu  rr       eee
                           pp
                           pp

≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡
*/


call e                                           /*let's see the precision we can use.  */
numeric digits  length(e) - 1                    /*big digits for Planck & Dalton scale.*/
                                                 /*subtract  one  for the decimal point.*/
parse arg tList                                  /*get the specified temperature lists. */
tList= space(tList)                              /*elide any and all superfluous blanks.*/

  do  until  tList=''                            /*process the list of temperatures.    */
  parse  var   tList    x  ','  tList            /*temperatures are separated by commas.*/
  x= translate(x, '(((', "[{«")                  /*support other grouping symbols.      */
  x= space(x)                                    /*elide any and all superfluous blanks.*/
  parse        var  x   z  '('                   /*handle any comments (if there're any)*/
  parse upper  var  z   z  ' TO '    ! .         /*separate the  TO  option from number.*/
  parse upper  var  z   z 'NOT' not . , noS noE  /*separate  NOT  option from number.   */
                                                 /*for the  NOT  keyword,  see  (below).*/
                               /*   *xxx    ◄───don't show scales that  end  in    xxx  */
                               /*    xxx*   ◄───  "     "    "      "  begin with  xxx  */
  if not\==''  then do
                    if  left(not, 1) == '*'  then noE= substr(not, 2)
                    if right(not, 1) == '*'  then noS= left(not, length(not) - 1)
                    noL= length(noE || noS)
                    if noL==0  then  call serr ,
                                "illegal NOT keyword, no leading or trailing * specified."
                    end

  if !==''  then != "ALL"                        /*nothing specific, so choose  "ALL".  */

  all= (!=='ALL')                                /*allows specification of the "TO" opt.*/

  if z==''  then call serr  'no arguments were specified.'

  _= verify(z, '+-.0123456789')                  /*list of valid decimal digs & thingys.*/
  n= z                                           /*obtain a "backup" copy of Z (number).*/

  if _\==0  then do
                 if _==1  then call serr  'illegal temperature:'  z
                 n= left(z, _-1)                 /*obtain the  number  (hopefully).     */
                 u= strip( substr(z, _) )        /*obtain the  temperature unit.        */
                 end
            else u= 'k'                          /*assume kelvin as per task requirement*/
                                                 /*       ──────                        */
  if \datatype(n, 'N')  then call serr  "illegal number:"   n

  if \all  then do                               /*there is a    TO  ααα   temp. scale. */
                call scaleName !                 /*process the   TO  temp. abbreviation.*/
                != sn                            /*assign the full temperature name to !*/
                end                              /*!  now contains temp scale full name.*/

  call scaleName  u                              /*allow alternate temp. scale spellings*/
  call convert2Fahrenheit                        /*convert a temperature ──► Fahrenheit.*/

  say right(' ' x, 79, "─")                      /*show original value & scale (for sep)*/

  call convert2specific                          /*convert Fahrenheit ──► specific temp,*/
  end   /*until tlist*/                          /*this is a biggish  DO  loop.         */


exit                                             /*stick a fork in it,  we're all done. */


/*──────────────────────────────────────────────────────────────────────────────────────*/
$: procedure;  showDig= 8                        /*only show 8 significant decimal digs.*/
   _= commas( format( arg(1), , showDig ) / 1 )  /*format# 8 digits past . and add comma*/
   p= pos(., _)                                  /*find position of the decimal point.  */
                                                 /* [↓]  align integers with FP numbers.*/
   if p==0  then _= _ || left('',  5 + showDig + 1)                 /*no  decimal point.*/
            else _= _ || left('',  5 + showDig - length(_) + p)     /*has    "      "   */

   return right(_, max(60, length(_) ) )         /*return the re─formatted argument (#).*/


/*────────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
convert2Fahrenheit:                              /*convert   N  ──►  ºF   temperatures. */

                                                 /* [↓]  fifty-eight temperature scales.*/
      select
      when sn=='ABSOLUTE'           then F= n *   9/5       -  459.67
      when sn=='AMONTON'            then F= n * 8.37209     -  399.163
      when sn=='BARNSDORF'          then F= n * 6.85714     +    6.85714
      when sn=='BEAUMUIR'           then F= n * 2.22951     +   32
      when sn=='BENART'             then F= n * 1.43391     +   31.2831
      when sn=='BERGEN'             then F=(n + 23.8667)    *   15/14
      when sn=='BRISSEN'            then F= n *  32/15      +   32
      when sn=='CELSIUS'            then F= n *   9/5       +   32             /*a single  C  is taken as Celsius.*/
      when sn=='CIMENTO'            then F= n * 2.70677     -    4.54135
      when sn=='CRUQUIUS'           then F= n * 0.409266    -  405.992
      when sn=='DALENCE'            then F= n * 2.7         +   59
      when sn=='DALTON'             then F=     273.15      *  pow(273.15 / 273.15, n / 100)   *   1.8   -   459.67
      when sn=='DANIELL'            then F= n * 7.27194     +   55.9994
      when sn=='DE LA HIRE'         then F=(n - 3)          /    0.549057
      when sn=='DE LA VILLE'        then F=(n + 6.48011)    /    0.985568
      when sn=='DELISLE'            then F=212              -    n * 6/5
      when sn=='DELISLE OLD'        then F=212              -    n * 1.58590197
      when sn=='DE LUC'             then F=(n + 14)         *   16/7
      when sn=='DE LYON'            then F=(n + 17.5)       *   64/35
      when sn=='DE REVILLAS'        then F=212              -    n * 97/80
      when sn=='DERHAM'             then F= n / 0.38444386  -  188.578
      when sn=='DERHAM OLD'         then F= n * 3           +    4.5
      when sn=='DE SUEDE'           then F=(n + 17.6666)    *  150/83
      when sn=='DE VILLENEUVE'      then F=(n + 23.7037)    /    0.740741
      when sn=='DU CREST'           then F=(n + 37.9202)    /    0.650656
      when sn=='EDINBURGH'          then F= n * 4.6546      -    6.40048
      when sn=='ELECTRON VOLTS'     then F= n * 20888.1     -  459.67
      when sn=='FAHRENHEIT'         then F= n
      when sn=='FAHRENHEIT OLD'     then F= n * 20/11       -   89.2727
      when sn=='FLORENTINE LARGE'   then F=(n +  7.42857)   /    0.857143
      when sn=='FLORENTINE MAGNUM'  then F=(n + 73.9736 )   /    1.50659
      when sn=='FLORENTINE SMALL'   then F=(n -  1.38571)   /    0.378571
      when sn=='FOWLER'             then F= n * 0.640321    +   53.7709
      when sn=='FRICK'              then F= n * 200/251     +   58.5339
      when sn=='GASMARK'            then F= n * 25          +  250
      when sn=='GOUBERT'            then F= n * 2           +   32
      when sn=='HALES'              then F= n * 1.2         +   32
      when sn=='HANOW'              then F= n * 1.06668     -   10.6672
      when sn=='HAUKSBEE'           then F= n *  18/25      +   88.16
      when sn=='JACOBS-HOLBORN'     then F= n *  18/71      -   53.4366
      when sn=='KELVIN'             then F= n *   9/5       -  459.67
      when sn=='LEIDEN'             then F= n * 1.8         -  423.4
      when sn=='NEWTON'             then F= n *  60/11      +   32
      when sn=='OERTEL'             then F= n + n           -   32
      when sn=='PLANCK'             then F= n * 1.416833e32 *    9/5  -  459.67
      when sn=='RANKINE'            then F= n               -  459.67          /*a single  R  is taken as Rankine.*/
      when sn=='REAUMUR'            then F= n *   9/4       +   32
      when sn=='RICHTER'            then F= n * 160/73      -    7.45205
      when sn=='RINALDINI'          then F= n * 15          +   32
      when sn=='ROMER'              then F=(n - 7.5) * 27/4 +   32
      when sn=='ROSENTHAL'          then F= n *  45/86      -  453.581
      when sn=='ROYAL SOCIETY'      then F=(n -122.82)      *  -50/69
      when sn=='SAGREDO'            then F= n * 0.3798      -    5.98
      when sn=='SAINT-PATRICE'      then F= n * 2.62123     +  115.879
      when sn=='STUFE'              then F= n * 45          +  257
      when sn=='SULZER'             then F= n * 1.14595     +   33.2334
      when sn=='THERMOSTAT'         then F= n * 54          +   32
      when sn=='WEDGWOOD'           then F= n * 44.7429295  +  516.2
      otherwise          call serr  'invalid temperature scale: '    u
      end   /*select*/

return


/*─────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
convert2specific:                                /*convert  ºF  ──►  xxx   temperatures.*/

  K = (F + 459.67)  *  5/9                       /*compute temperature in kelvin scale. */
  a = (1e || (-digits() % 2)  -  digits() % 20)  /*minimum number for Dalton temperature*/
 eV = (F + 459.67)  /  20888.1                   /*compute the number of electron volts.*/


if ?('ABSOLUTE')           then say $(     k                                         )  "Absolute"
if ?('AMONTON')            then say $( ( F   + 399.163  ) /   8.37209                )  "Amonton"
if ?('BARNSDORF')          then say $( ( F   -   6.85715) /   6.85715                )  "Barnsdorf"
if ?('BEAUMUIR')           then say $( ( F   -  32      ) /   2.22951                )  "Beaumuir"
if ?('BENART')             then say $( ( F   -  31.2831 ) /   1.43391                )  "Benart"
if ?('BERGEN')             then say $( ( F   * 14/15    ) -  23.8667                 )  "Bergen"
if ?('BRISSON')            then say $( ( F   -  32      ) *  15/32                   )  "Brisson"
if ?('CELSIUS')            then say $( ( F   -  32      ) *   5/9                    )  "Celsius"
if ?('CIMENTO')            then say $( ( F   +   4.54135) /   2.70677                )  "Cimento"
if ?('CRUQUIUS')           then say $( ( F   + 405.992  ) /   0.409266               )  "Cruquius"
if ?('DALENCE')            then say $( ( F   -  59      ) /   2.7                    )  "Dalence"

if ?('DALTON')             then if K>a then say $(100*ln(k/273.15)/ln(373.15/273.15) )  "Dalton"
                                       else say right("-infinity        ", 60)          "Dalton"

if ?('DANIELL')            then say $( ( F   -  55.9994 ) /   7.27194                )  "Daniell"
if ?('DE LA HIRE')         then say $(   F   *   0.549057 +   3                      )  "De la Hire"
if ?('DE LA VILLE')        then say $(   F   *   0.985568 -   6.48011                )  "De la Ville"
if ?('DELISLE')            then say $( ( 212 - F        ) *   5/6                    )  "Delisle"
if ?('DELISLE OLD')        then say $( ( 212 - F        ) /   1.58590197             )  "Delisle OLD"
if ?('DE LUC')             then say $(   F   *  7/16      -  14                      )  "De Luc"
if ?('DE LYON')            then say $(   F   * 35/64      -  17.5                    )  "De Lyon"
if ?('DE REVILLAS')        then say $( ( 212 - F        ) *  80/97                   )  "De Revillas"
if ?('DERHAM')             then say $(   F   * 0.38444386 +  72.4978                 )  "Derham"
if ?('DERHAM OLD')         then say $( ( F   - 4.5      ) /   3                      )  "Derham OLD"
if ?('DE VILLENEUVE')      then say $(   F   * 0.740741   -  23.7037                 )  "De Villeneuve"
if ?('DE SUEDE')           then say $(   F   * 83/150     -  17.6666                 )  "De Suede"
if ?('DU CREST')           then say $(   F   * 0.650656   -  37.9202                 )  "Du Crest"
if ?('EDINBURGH')          then say $( ( F   +   6.40048) /   4.6546                 )  "Edinburgh"
if ?('ELECTRON VOLTS')     then say $(     eV                                        )  "electron volt"s(eV)
if ?('FAHRENHEIT')         then say $(   F                                           )  "Fahrenheit"
if ?('FAHRENHEIT OLD')     then say $(   F   * 20/11      -  89.2727                 )  "Fahrenheit OLD"
if ?('FLORENTINE LARGE')   then say $(   F   * 0.857143   -   7.42857                )  "Florentine large"
if ?('FLORENTINE MAGNUM')  then say $(   F   * 1.50659    -  73.9736                 )  "Florentine Magnum"
if ?('FLORENTINE SMALL')   then say $(   F   * 0.378571   +   1.38571                )  "Florentine small"
if ?('FOWLER')             then say $( ( F   -  53.7709 ) /   0.640321               )  "Fowler"
if ?('FRICK')              then say $( ( F   -  58.5338 ) * 251/200                  )  "Frick"
if ?('GAS MARK')           then say $( ( F   - 250      ) *   0.04                   )  "gas mark"
if ?('GOUBERT')            then say $( ( F   +  32      ) *   0.5                    )  "Goubert"
if ?('HALES')              then say $( ( F   -  32      ) /   1.2                    )  "Hales"
if ?('HANOW')              then say $( ( F   +  10.6672 ) /   1.06668                )  "Hanow"
if ?('HAUKSBEE')           then say $( ( F   -  88.16   ) *  25/18                   )  "Hauksbee"
if ?('JACOBS-HOLBORN')     then say $( ( F   +  53.4366 ) *  71/18                   )  "Jacobs-Holborn"
if ?('KELVIN')             then say $(     k                                         )  "kelvin"s(k)
if ?('LEIDEN')             then say $(   F   /   1.8      + 235.222                  )  "Leiden"
if ?('NEWTON')             then say $( ( F   -  32      ) *  11/60                   )  "Newton"
if ?('OERTEL')             then say $( ( F   +  32      ) *   0.5                    )  "Oertel"
if ?('PLANCK')             then say $( ( F   + 459.67   ) *   5/9 / 1.416833e32      )  "Planck"
if ?('RANKINE')            then say $(   F   + 459.67                                )  "Rankine"
if ?('REAUMUR')            then say $( ( F   -  32      ) *   4/9                    )  "Reaumur"
if ?('RICHTER')            then say $( ( F   +   7.45205) *  73/160                  )  "Richter"
if ?('RINALDINI')          then say $( ( F   -  32      ) /  15                      )  "Rinaldini"
if ?('ROMER')              then say $( ( F   -  32      ) *   4/27  +  7.5           )  "Romer"
if ?('ROSENTHAL')          then say $( ( F   + 453.581  ) *  86/45                   )  "Rosenthal"
if ?('ROYAL SOCIETY')      then say $(   F   * -69/50     + 122.82                   )  "Royal Society of London"
if ?('SAGREDO')            then say $( ( F   +   5.98   ) /   0.3798                 )  "Segredo"
if ?('SAINT-PATRICE')      then say $( ( F   - 115.879  ) /   2.62123                )  "Saint-Patrice"
if ?('STUFE')              then say $( ( F   - 257      ) /  45                      )  "Stufe"
if ?('SULZER')             then say $( ( F   -  33.2334 ) /   1.14595                )  "Sulzer"
if ?('THERMOSTAT')         then say $( ( F   -  32      ) /  54                      )  "Thermostat"
if ?('WEDGWOOD')           then say $( ( F   - 516.2    ) /  44.7429295              )  "Wedgwood"

return
-- More  --


/*──────────────────────────────────────────────────────────────────────────────────────────────────*/
scaleName:  parse arg y                          /*abbreviations  ──►  temp. short name.*/
yU= translate(y, '-eE', "_éÉ")                   /*translate some accented characters.  */
upper yU                                         /*uppercase version of temperature unit*/

if left(yU, 7)=='DEGREES'  then yU=substr(yU, 8) /*is this a redundant  "degrees"  ?    */
if left(yU, 6)=='DEGREE'   then yU=substr(yU, 7) /* "   "  "     "      "degree"   ?    */

yU=  strip(yU)                                   /*elide all leading & trailing blanks. */
 _= length(yU)                                   /*obtain the length of the  yU  value. */

if right(yU,1)=='S' & _>1  then yU=left(yU, _-1) /*remove any trailing plural(s).       */


      select                                     /*change abbreviations  ──►  shortname.*/
      when abbrev('ABSOLUTE'                , yU, 1)    then sn= "ABSOLUTE"
      when abbrev('AMONTON'                 , yU)       then sn= "AMONTON"
      when abbrev('BARNDORF'                , yU,2)  |,
           abbrev('BARNSDORF'               , yU,2)     then sn= "BARNSDORF"
      when abbrev('BEAUMIUR'                , yU,3)  |,
           abbrev('BEAUMUIR'                , yU,3)     then sn= "BEAUMUIR"
      when abbrev('BENERT'                  , yU,3)  |,
           abbrev('BENART'                  , yU,3)     then sn= "BENART"
      when abbrev('BRISSEN'                 , yU,3)  |,
           abbrev('BRISSON'                 , yU,3)     then sn= "BRISSEN"
      when abbrev('BURGEN'                  , yU,3)  |,
           abbrev('BURGAN'                  , yU,3)  |,
           abbrev('BERGAN'                  , yU,3)  |,
           abbrev('BERGEN'                  , yU,3)     then sn= "BERGEN"
      when abbrev('CENTIGRADE'              , yU)    |,
           abbrev('CENTRIGRADE'             , yU)    |,                           /* 50% misspelled.*/
           abbrev('CETIGRADE'               , yU)    |,                           /* 50% misspelled.*/
           abbrev('CENTINGRADE'             , yU)    |,
           abbrev('CENTESIMAL'              , yU)    |,
           abbrev('CELCIU'                  , yU)    |,                           /* 82% misspelled.*/
           abbrev('CELCIOU'                 , yU)    |,                           /*  4% misspelled.*/
           abbrev('CELCUI'                  , yU)    |,                           /*  4% misspelled.*/
           abbrev('CELSUI'                  , yU)    |,                           /*  2% misspelled.*/
           abbrev('CELCEU'                  , yU)    |,                           /*  2% misspelled.*/
           abbrev('CELCU'                   , yU)    |,                           /*  2% misspelled.*/
           abbrev('CELISU'                  , yU)    |,                           /*  1% misspelled.*/
           abbrev('CELSU'                   , yU)    |,                           /*  1% misspelled.*/
           abbrev('CELSIU'                  , yU)       then sn= "CELSIUS"
      when abbrev('CIMANTO'                 , yU,2)  |,
           abbrev('CIMENTO'                 , yU,2)     then sn= "CIMENTO"
      when abbrev('CRUQUIOU'                , yU,2)  |,
           abbrev('CRUQUIO'                 , yU,2)  |,
           abbrev('CRUQUIU'                 , yU,2)     then sn= "CRUQUIU"
      when abbrev('DALANCE'                 , yU,4)  |,
           abbrev('DALENCE'                 , yU,4)     then sn= "DALENCE"
      when abbrev('DANELLE'                 , yU,3)  |,
           abbrev('DANEAL'                  , yU,3)  |,
           abbrev('DANIAL'                  , yU,3)  |,
           abbrev('DANIELE'                 , yU,3)  |,
           abbrev('DANNEL'                  , yU,3)  |,
           abbrev('DANYAL'                  , yU,3)  |,
           abbrev('DANYEL'                  , yU,3)  |,
           abbrev('DANIELL'                 , yU,3)     then sn= "DANIELL"
      when abbrev('DALTON'                  , yU,3)     then sn= "DALTON"
      when abbrev('DELAHIRE'                , yU,7)  |,
           abbrev('LAHIRE'                  , yU,4)  |,
           abbrev('HIRE'                    , yU,2)  |,
           abbrev('DE-LA-HIRE'              , yU,7)     then sn= "DE LA HIRE"
      when abbrev('DELAVILLE'               , yU,7)  |,
           abbrev('LAVILLE'                 , yU,3)  |,
           abbrev('VILLE'                   , yU,1)  |,
           abbrev('VILLA'                   , yU,1)  |,
           abbrev('DE-LA-VILLE'             , yU,7)     then sn= "DE LA VILLE"
      when abbrev('DELISLE'                 , yU,3)     then sn= "DELISLE"
      when abbrev('DELISLE-OLD'             , yU,8)  |,
           abbrev('OLDDELISLE'              , yU,6)  |,
           abbrev('DELISLEOLD'              , yU,8)     then sn= "DELISLE OLD"
      when abbrev('DELUC'                   , yU,4)  |,
           abbrev('LUC'                     , yU,2)  |,
           abbrev('DE-LUC'                  , yU,5)     then sn= "DE LUC"
      when abbrev('DELYON'                  , yU,4)  |,
           abbrev('LYON'                    , yU,2)  |,
           abbrev('DE-LYON'                 , yU,5)     then sn= "DE LYON"
      when abbrev('DEREVILLA'               , yU,3)  |,
           abbrev('DEREVILA'                , yU,3)  |,
           abbrev('REVILLA'                 , yU,3)  |,
           abbrev('DE-REVILLA'              , yU,4)  |,
           abbrev('DE-REVILLA'              , yU,5)     then sn= "DE REVILLAS"
      when abbrev('DEVILLENEUVE'            , yU,3)  |,
           abbrev('DE-VILLENEUVE'           , yU,4)     then sn= "DE VILLENEUVE"
      when abbrev('DURHAM'                  , yU,3)  |,
           abbrev('DERHAM'                  , yU,4)     then sn= "DERHAM"
      when abbrev('OLDDURHAM'               , yU,5)  |,
           abbrev('OLDDERHAM'               , yU,6)  |,
           abbrev('DERHAM-OLD'              , yU,4)  |,
           abbrev('DERHAMOLD'               , yU,4)     then sn= "DERHAM OLD"
      when abbrev('DE-SUEDE'                , yU,4)  |,
           abbrev('DESUEDE'                 , yU,4)     then sn= "DE SUEDE"
      when abbrev('DU-CREST'                , yU,2)  |,
           abbrev('DUCREST'                 , yU,2)     then sn= "DU CREST"
      when abbrev('EDENBURGH'               , yU,2)  |,
           abbrev('EDINBURGH'               , yU,2)     then sn= "EDINBURGH"
      when abbrev('EVOLT'                   , yU,2)  |,
           abbrev('ELECTRONVOLT'            , yU,2)     then sn= "ELECTRON VOLTS"
      when abbrev('FARENHEIT'               , yU)    |,                           /* 39% misspelled.*/
           abbrev('FARENHEIGHT'             , yU)    |,                           /* 15% misspelled.*/
           abbrev('FARENHITE'               , yU)    |,                           /*  6% misspelled.*/
           abbrev('FARENHIET'               , yU)    |,                           /*  3% misspelled.*/
           abbrev('FARHENHEIT'              , yU)    |,                           /*  3% misspelled.*/
           abbrev('FARINHEIGHT'             , yU)    |,                           /*  2% misspelled.*/
           abbrev('FARENHIGHT'              , yU)    |,                           /*  2% misspelled.*/
           abbrev('FAHRENHIET'              , yU)    |,                           /*  2% misspelled.*/
           abbrev('FERENHEIGHT'             , yU)    |,                           /*  2% misspelled.*/
           abbrev('FEHRENHEIT'              , yU)    |,                           /*  2% misspelled.*/
           abbrev('FERENHEIT'               , yU)    |,                           /*  2% misspelled.*/
           abbrev('FERINHEIGHT'             , yU)    |,                           /*  1% misspelled.*/
           abbrev('FARIENHEIT'              , yU)    |,                           /*  1% misspelled.*/
           abbrev('FARINHEIT'               , yU)    |,                           /*  1% misspelled.*/
           abbrev('FARANHITE'               , yU)    |,                           /*  1% misspelled.*/
           abbrev('FAHRENHEIT'              , yU)       then sn= "FAHRENHEIT"
      when abbrev('OLDFAHRENHEIT'           , yU,4)  |,
           abbrev('FAHRENHEIT-OLD'          , yU,13) |,
           abbrev('FAHRENHEITOLD'           , yU,13)    then sn= "FARHENHEIT OLD"
      when abbrev('FLORENTINE-LARGE'        , yU,12) |,
           abbrev('LARGE-FLORENTINE'        , yU,7)  |,
           abbrev('LARGEFLORENTINE'         , yU,6)  |,
           abbrev('FLORENTINELARGE'         , yU,12)    then sn= "FLORENTINE LARGE"
      when abbrev('FLORENTINE-MAGNUM'       , yU,2)  |,
           abbrev('MAGNUM-FLORENTINE'       , yU,3)  |,
           abbrev('MAGNUMFLORENTINE'        , yU,3)  |,
           abbrev('FLORENTINEMAGNUM'        , yU,2)     then sn= "FLORENTINE MAGNUM"
      when abbrev('FLORENTINE-SMALL'        , yU,13) |,
           abbrev('SMALL-FLORENTINE'        , yU,7)  |,
           abbrev('SMALLFLORENTINE'         , yU,6)  |,
           abbrev('FLORENTINESMALL'         , yU,13)    then sn= "FLORENTINE SMALL"
      when abbrev('FOULER'                  , yU,2)  |,
           abbrev('FOWLOR'                  , yU,2)  |,
           abbrev('FOWLER'                  , yU,2)     then sn= "FOWLER"
      when abbrev('FRICK'                   , yU,2)     then sn= "FRICK"
      when abbrev('GAS-MARK'                , yU,2)  |,
           abbrev('GASMARK'                 , yU,2)     then sn= "GAS MARK"
      when abbrev('GOUBERT'                 , yU,2)     then sn= "GOUBERT"
      when abbrev('HAIL'                    , yU,3)  |,
           abbrev('HALE'                    , yU,3)     then sn= "HALES"
      when abbrev('HANOW'                   , yU,3)     then sn= "HANOW"
      when abbrev('HUCKSBEE'                , yU,3)  |,
           abbrev('HAWKSBEE'                , yU,3)  |,
           abbrev('HAUKSBEE'                , yU,3)     then sn= "HAUKSBEE"
      when abbrev('JACOBSHOLBORN'           , yU,2)  |,
           abbrev('JACOBS-HOLBORN'          , yU,2)     then sn= "JACOBS-HOLBORN"
      when abbrev('KALVIN'                  , yU)    |,                           /* 27% misspelled.*/
           abbrev('KERLIN'                  , yU)    |,                           /* 18% misspelled.*/
           abbrev('KEVEN'                   , yU)    |,                           /*  9% misspelled.*/
           abbrev('KELVIN'                  , yU)       then sn= "KELVIN"
      when abbrev('LAYDEN'                  , yU)    |,
           abbrev('LEIDEN'                  , yU)       then sn= "LEIDEN"
      when abbrev('NEUTON'                  , yU)    |,                           /*100% misspelled.*/
           abbrev('NEWTON'                  , yU)       then sn= "NEWTON"
      when abbrev('ORTEL'                   , yU)    |,
           abbrev('OERTEL'                  , yU)       then sn= "OERTEL"
      when abbrev('PLACK'                   , yU)    |,                           /*100% misspelled.*/
           abbrev('PLANC'                   , yU)    |,                           /*     misspelled.*/
           abbrev('PLANK'                   , yU)    |,                           /*     misspelled.*/
           abbrev('PLANCK'                  , yU)       then sn= "PLANCK"
      when abbrev('RANKINE'                 , yU, 1)    then sn= "RANKINE"
      when abbrev('REAUMUR'                 , yU, 2)    then sn= "REAUMUR"
      when abbrev('RICKTER'                 , yU, 3) |,
           abbrev('RICHTER'                 , yU, 3)    then sn= "RICHTER"
      when abbrev('RINALDINI'               , yU, 3)    then sn= "RINALDINI"
      when abbrev('ROEMER'                  , yU, 3) |,
           abbrev('ROMER'                   , yU, 3)    then sn= "ROMER"
      when abbrev('ROSANTHAL'               , yU, 3) |,
           abbrev('ROSENTHAL'               , yU, 3)    then sn= "ROSENTHAL"
      when abbrev('RSOL'                    , yU, 2) |,
           abbrev('RSL'                     , yU, 2) |,
           abbrev('ROYALSOCIETYOFLONDON'    , yU, 3) |,
           abbrev('ROYAL-SOCIETY-OF-LONDON' , yU, 3)    then sn= "ROYAL SOCIETY"
      when abbrev('SAGREDO'                 , yU, 3)    then sn= "SAGREDO"
      when abbrev('ST.-PATRICE'             , yU, 3) |,
           abbrev('ST.PATRICE'              , yU, 3) |,
           abbrev('SAINTPATRICE'            , yU, 3) |,
           abbrev('SAINT-PATRICE'           , yU, 3)    then sn= "SAINT-PATRICE"
      when abbrev('STUFFE'                  , yU, 3) |,
           abbrev('STUFE'                   , yU, 3)    then sn= "STUFE"
      when abbrev('SULTZER'                 , yU, 2) |,
           abbrev('SULZER'                  , yU, 2)    then sn= "SULZER"
      when abbrev('WEDGEWOOD'               , yU)    |,
           abbrev('WEDGWOOD'                , yU)       then sn= "WEDGWOOD"

      otherwise           call serr  'illegal temperature scale:'  y
      end   /*select*/


return


/*──────────────────────────────────────────────────────────────────────────────────────*/
?:      parse arg y 1 yu
        upper yu
        if not\==''  then do
                          if noS\==""  then if  left(yu, noL)==noS  then return 0
                          if noE\==''  then if right(yu, noL)==noE  then return 0
                          end

        if all | y==!  then return 1
                            return 0


/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: procedure; parse arg _                            /*insert commas in a number.  */
        n= _'.9'                                          /*added suffix for VERIFY BIF.*/
        #= 123456789                                      /*a nifty handy-dandy literal.*/
        b= verify(n, #, "M")                              /*find beginning of a number. */
        e= verify(n, #'0', , verify(n, #"0.", 'M') ) - 4  /*  "      end    " "    "    */

            do j=e  to b  by -3                           /*insert commas right─to─left.*/
            _= insert(",", _, j)                          /*insert a comma every period.*/
            end  /*j*/

        return _


/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt:  procedure; parse arg x                             /*obtain the target of SQRT.  */
       if x=0  then return 0                              /*Argument is zero?  Return 0.*/
                                                          /*This function work for zero.*/
       d= digits()                                        /*get # of dec. digs, current.*/
       m.= 9                                              /*set "  "  "     "  at start.*/
       h= d+6                                             /*add 6 for rounding concerns.*/
       numeric form                                       /*right form of exponentiation*/
       numeric digits                                     /*start with nine numeric digs*/

                                                          /*a way of getting the expon. */
                                                          /*No exponent?  Then add one. */
       parse value format(x, 2, 1, , 0)  'E0'     with     g  "E"  _  .
                                                          /* [↓]  halve the exponent.   */
       g=g  *  .5'e'_  %  2                               /*a first best guess for sqrt,*/
                                                          /*which is 1/2 of the exponent*/
                                                          /* [↓]  use min number of dec.*/
                                                          /*      digs for early SQRTs. */
                        do j=0  while h>9
                        m.j= h                            /*calculate # of digits to use*/
                        h= h % 2   +   1                  /*halving the exponent means  */
                        end  /*j*/                        /*   that it'll be doubled as */
                                                          /*   the   M.   array will be */
                                                          /*   processed backwards. [↓] */
                        do k=j+5  to 0  by -1             /*calculate higher precision. */
                        numeric digits m.k                /*bump the decimal digits.    */
                        g= (g  +  x / g)   *   .5         /*calculate SQRT approximation*/
                        end  /*k*/
       return g / 1                                       /*this normalizes the sqrt #. */


/*──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
?:      parse arg y; if not\==''  then do; if noS\==""  then if left(y, noL)==noS  then return 0;  if noE\==''  then if right(y, noL)==noE  then return 0; end;    if all | y==!  then return 1;   return 0
e:      e = 2.718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427427466391932;         return e                /*112 useful decimal digits. */
isInt:  return datatype(arg(1), 'W')            /*is the argument a whole number  (integer)?  */
exp:    procedure; parse arg x; ix=x%1; if abs(x-ix)>.5 then ix=ix+sign(x); x=x-ix; z=1; _=1; w=z; do j=1; _=_*x/j; z=(z+_)/1; if z==w  then leave; w=z; end;  if z\==0  then z= z * e()**ix;      return z/1
ln:     procedure; parse arg x; call e; ig=x>1.5; is=1-2*(ig\==1); ii=0; xx=x;             return ln..()
ln..:   do while ig & xx>1.5 | \ig & xx<.5;_=e;do k=-1;iz=xx*_**-is;if k>=0 & (ig & iz<1 | \ig & iz>.5) then leave;_=_*_;izz=iz;end;xx=izz; ii=ii+is*2**k; end; x=x*e**-ii-1; z=0;_=-1;p=z;do k=1; _=-_*x;z=z+_/k; if z=p then leave;p=z; end; return z+ii
pow:    procedure; parse arg x,y; if y=0  then return 1; if x=0  then return 0; if isInt(y)  then return x**y; if isInt(1/y)  then return root(x,1/y,f);   return pow.()
pow.:   if abs(y//1)=.5  then return sqrt(x)**sign(y)*x**(y%1);  return exp(y*ln(x))
root:   procedure; parse arg x 1 ox,y 1 oy; if x=0 | y=1  then return x; if isInt(y)  then return rooti(x,y);  _=sqrt(x);  if y<0  then _=1/_;   return _
rooti:  x=abs(x); y=abs(y); a= digits() + 5; g=rootIg(); m= y-1; d=5;   do  until d==a; d=min(d+d, a); numeric digits d; o=0;   do until o=g; o=g; g=format( (m*g**y+x) /y/g**m, , d-2); end; end;   _= g * sign(ox); if oy<0  then _= 1/_;    return _
rootIg: numeric form;parse value format(x,2,1,,0) 'E0'  with  ? 'E' _ .;                  return (? / y'E'_ % y) + (x>1)
s:      if arg(1)==1  then return arg(3);     return word(arg(2) 's',1)          /*pluralizer.*/
serr:   say;       say '***error***';       say;        say arg(1);        say;          exit 13

```

