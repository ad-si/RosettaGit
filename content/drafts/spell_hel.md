+++
title = "$SPELL.HEL"
description = ""
date = 2017-09-07T00:00:10Z
aliases = []
[extra]
id = 12875
[taxonomies]
categories = []
tags = []
+++

==$SPELL#.HEL==
The   '''$SPELL#.HEL'''   is the HELp (documentation) for the   [[$SPELL.REX|$SPELL#.REX]]   (REXX) program.

```txt

The    $SPELL#    command is used to display a number spelled out (in English
words), either as a cardinal number (the default), or as an ordinal number.  The
number may have leading/imbedded/trailing blanks, a leading sign, a decimal
point, an exponent, a suffix multiplier (see below), and/or a leading currency
symbol ($ ¥ £ ₧ ƒ ε)  or a trailing cent sign (¢).

The Conway-Wechsler system is used for expression the numbers.

Cardinal  #s are: one two three four five six seven eight nine ten eleven ...
Ordinal   #s are: first second third fourth fifth sixth seventh eighth ninth ...

╔══════════════════════════════════════════════════════════════════════════════╗
║                                                                              ║
║                                                           {CLearscreen}      ║
║           +nnn                    {ORDinal │ ASAYEar}     {LEADING zzz}      ║
║          -nnn                               {SINGley}     {POINTs ddd}       ║
║ $SPELL#   nnn                         {Quiet|NOQuiet}     {MINUSsigns mmm}   ║
║           nnnSUFFIX                            {tops}     {PLUSsigns ppp}    ║
║          expression    {AMERican | BRITish | ENGlish}     {SEPerators sss}   ║
║                                         {LOGs|NOLOGs}     {ZEROes ooo}       ║
║                                     {COLORs|NOCOLORs}     {NOTHING bbb}      ║
║            ?                                              {BLANKs c}         ║
║            ?AUTHOR                                        {RAISED rrr}       ║
║            ?FLOW                                          {POWER www}        ║
║            ?SAMPLES                                       {DOLLARs xxx}      ║
║                                                           {POUNDs xxx}       ║
║                                                           {PISETAs xxx}      ║
║                                                           {FRANCs xxx}       ║
║                                                           {EUROs xxx}        ║
║                                                           {YEN xxx}          ║
║                                                           {CENTs xxx}        ║
║                                                           {ANDCENTs aaa}     ║
║                                                                              ║
╚══════════════════════════════════════════════════════════════════════════════╝

───where:

?          shows this help file              (press  ESC  to quit when viewing).

?AUTHOR    shows the author of this program.

?FLOW      shows the external execution flow of this program.

?SAMPLES   shows some sample uses            (press  ESC  to quit when viewing).

nnn        is the number that is to be spelled out in English words,  and it may
           have imbedded commas for for readability   (see the table of names of
           numbers below.)    No superflous zeros are spelled out.

           The current limitation is  3,003 digits for "American" numbers,  and
                                      6,000 digits for "British"  numbers.

           If the number has a (leading) sign  (+ or -), it's also spelled out.

           If the number has a trailing decimal point, it's also spelled out.

           Any fractional part of the number is spelled out by their individual
           digits     (i.e.:    20.678     is:   twenty point six seven eight).

           If the number has an exponent, it is expressed as
              nnn times ten raised to the eee power

           The word  "power"  can be changed by use of the   POWER=www   option,
           and the phrase   'times ten raised to the'    can be changed by the
           use of the      RAISED=rrr     option.


nnnSUFFIX  is a  number  suffixed  with one of the following suffix codes.
           (Note: the  suffix  is upper/lowercase sensative,
            and the "i" suffix may be in either lower or uppercase):

       suffix name  multiplier              mulitiplier
       ────── ───── ────────── ─────────────────────────────────────────────────
       ki Ki  kibi     2**10   1,024
          Mi  mebi     2**20   1,048,576
          Gi  gibi     2**30   1,073,741,824
          Ti  tebi     2**40   1,099,571,627,776
          Pi  pebi     2**50   1,125,899,906,884,629
          Ei  exbi     2**60   1,152,921,504,606,846,976
          Zi  zebi     2**70   1,180,591,620,717,411,303,424
          Yi  yobi     2**80   1,208,925,819,614,629,174,706,176
          Xi  xebi     2**90   1,237,940,039,285,380,274,899,124,224
          Wi  webi     2**100  1,267,650,600,228,229,401,496,703,205,376
          Vi  vebi     2**110  1,298,074,214,633,706,907,132,624,082,305,024
          Ui  uebi     2**120  1,329,227,995,784,915,872,903,807,060,280,344,576

       k  K   kilo     10**3   1,000
          M   mega     10**6   1,000,000
          G   giga     10**9   1,000,000,000
          T   tera     10**12  1,000,000,000,000
          P   peta     10**15  1,000,000,000,000,000
          E   exa      10**18  1,000,000,000,000,000,000
          Z   zetta    10**21  1,000,000,000,000,000,000,000
          Y   yotta    10**24  1,000,000,000,000,000,000,000,000
          X   xenta    10**27  1,000,000,000,000,000,000,000,000,000
          W   wekta    10**30  1,000,000,000,000,000,000,000,000,000,000
          V   vendeka  10**33  1,000,000,000,000,000,000,000,000,000,000,000
          U   udekta   10**36  1,000,000,000,000,000,000,000,000,000,000,000,000
       ────── ───── ────────── ─────────────────────────────────────────────────

       (See derivation for the some of the suffixes below.)

       Also, the suffixes  DOZen  and  GRoss  (in mixed case) are also supported
       and can be combined with any of the above;  i.e.:   17Kdozen  ──or──  5gr

expression is a mathematical expression (as supported by REXX) and should result
           in a whole number  (if not, the result is rounded).

ORDINAL    spells out the number as an ordinal number  (instead of as a cardinal
           number).

ASAYEAR    spells the number as if it were a year  (1867  is spelled out as
                                                    eighteen sixty seven).

SINGley    expresses the number singley (per each digit).
           I.E.   2046    would be  expressed as:  two zero four six
           The default is  NOSINGley.

Quiet      suppresses the showing of any results.   However, the REXX  variable
           RESULT    is always set  (unless there's an error).
           Error messages  (if any)  are always shown.
                                                     The default is:  NOQUIET

NOQuiet    shows the results.

tops       are any of the  $T  options   (all start with a dot [.]   followed by
           the specific option).   Also, see the documenatatin below (at the end
           of this help file) for more information.

AMERican   indicates to use American type numbers (1 billion=1,000,000,000).
           AMERICAN is the default.

BRITish    indicates to use British  type numbers (1 billion=1,000,000,000,000).
           AMERICAN is the default.

ENGlish    (same as BRITISH).

LOGs       will write the answer to a file.             The default is:  NOLOGS
           The name of the log file is one of the following:

             (DOS)     tmp\$CALC.ANS      (if TMP  environmental var is defined)
             (DOS)    temp\$CALC.ANS      (if TEMP environmental var is defined)
             (DOS)      C:\$CALC.ANS      (depending if the ENVVARs are defined)

             (CMS)      $CALC ANS A4
             (TSO)      $CALC.ANS

COLORs     will use  $T  to display the answer in color.
           For  CMS  and  DOS,    the default is:  COLORS
           For all other systems, the default is:  NOCOLORS

           When COLORS is in effect, the (TOPS) default is:  .P=1 .A=1 .C=green
           These options can be overridden by specifing any  TOPS  option  (see
           the  TOPS  option above).

CLearscreen   clears the terminal screen before any output is displayed.
              The default is:   NOCLEARSCREEN.

LEADINGzeros zzz   where zzz is a character or character string (with no blanks)
           that is to be used to indicate leading zeros.   The default is a null
           string  (in effect, to ignore leading zeros).  If a blank (or blanks)
           are wanted, the underscore character (_) should be used.
           I.E.:    leading=_zero_(     will use a blank, the word   "zero"  and
           another blank  and  will display as      (blank)zero(blank).
           The     'LEADING='    may be in upper/lower/mixed case, but the   zzz
           string is used in the case specified.

MINUSsigns mmm    where  mmm is a character or character string (with no blanks)
           that is to be used to indicate a minus sign.    The default string is
           the word "minus".   If a blank (or blanks) are wanted, the underscore
           character (_) should be used.    I.E.:   minus=(minus)___    will use
           the  parenthised word   "minus"    and  three  extra  blanks  will be
           shown  when  the number specified (entred)  has a leading minus sign.
           The     'MINUS='    may be in upper/lower/mixed case,  but the    mmm
           string is used in the case specified.

POINTs ddd where    ddd     is a character or character string  (with no blanks)
           that is to be used to indicate a  decimal point.   The default string
           is the  word  "point".     If a  blank  (or blanks)  are wanted,  the
           underscore character (_) should be used.       I.E.:   point=(dot)___
           will use  the parenthised word  "dot"  and  three  extra  blanks will
           be shown  when  the number  specified (entred)  has  a decimal point.
           The     'POINT='    may be in upper/lower/mixed case,  but the    ddd
           string specified is case sensative.

PLUSsigns ppp   where  ppp  is a character or character string  (with no blanks)
           that is to be used to indicate a  plus sign.    The default string is
           the word  "plus".   If a blank (or blanks) are wanted, the underscore
           character should be used.   I.E.:   plus=a_positive    will use a the
           word  "a",  a blank,  and  the word   "positive"   will be shown when
           the number specified has a leading minus sign.
           The     'PLUS='    may be in upper/lower/mixed case,  but the     ppp
           string specified is case sensative.

SEPerators sss    where  sss  are characters (with no blanks) that are used as a
           seperator between all the  billions,  millions, and thousands  (these
           are also called  "periods").   This character string is  concatenated
           to the  right  of each period.   If blanks are wanted, the underscore
           character (_) should be used.            I.E.:     SEP=_and_
           will use extra blanks before  and after  each  "and"  to seperate the
           the (spelled-out) periods.  The  "SEP="   may be in upper/lower/mixed
           case, but the        sss        string specified is case sensative.

           If the original number contains a comma, then SEP defaultsto:  ,
           To override this, SEP can be specified as a _ (underscore), or:  none

ZEROes ooo where     ooo     are characters (with no blanks)  that are used when
           spelling zero digits.   Note that most imbedded zeroes aren't
           spelled, as in 1906 would be "nineteen hundred six", but when the
           number is expressed as a year (the ASAYEAR option), the same number
           is spelled as "nineteen oh six".   Any underscore character in the
           OOO  field will be treated as a blank,  and the case of the  OOO
           field will be honored  (upper/lower/mixed case).   Also, see the
           NOTHING    option below.        The default for  ZERO  is:     oh

NOTHING bbb   where   bbb   are characters (with no blanks)  that are used when
              spelling a zero number.   The default of a zero number is:   zero

BLANKs hh  where   hh     is a hexadecimal character.
BLANKs c   where    c     is a character that are used in place of blanks in
           the output (result).
           The default for  BLANKS  is a blank.

RAISEd rrr where   rrr   are the words used to express that a number is being
           raised to a power of ten  (because of the presense of an exponent).
           The default is:    times_ten_raised_to_the
           Note that any underbars are translated to blanks for the output.

POWer www  where   www   are the suffix words used to express that a number is
           being raised to a power of ten  (because of the presense of an
           exponent.
           The default is:    power
           Note that any underbars are translated to blanks for the output.

DOLLARs xxx   is the text string to be used when a dollar sign ($) is
              encountered (as a leading character in the number).
              The default is:   dollar
              and is pluralized by adding an 's'.

POUNDs xxx    is the text string to be used when a Britsh pound sign (£) is
              encountered (as a leading character in the number).
              The default is:   pound
              and is pluralized by adding an 's'.

PISETAs xxx   is the text string to be used when a piseta sign (₧) is
              encountered (as a leading character in the number).
              The default is:   piseta
              and is pluralized by adding an 's'.

FRANCs xxx    is the text string to be used when a franc sign (ƒ) is
              encountered (as a leading character in the number).
              The default is:   franc
              and is pluralized by adding an 's'.

EUROs xxx     is the text string to be used when a euro sign (ε) is
              encountered (as a leading character in the number).
              The default is:   euro
              and is pluralized by adding an 's'.

YEN xxx       is the text string to be used when a yen sign (¥) is
              encountered (as a leading character in the number).
              The default is:   yen
              and is pluralized by adding nothing.

CENTs xxx     is the text string to be used when a cent sign (¢) is
              encountered (as a trailing character in the number).
              The default is:   cent
              and is pluralized by adding an 's'.

ANDCENT=xxx   is the text string to be used when a dollar sign ($) is
              encountered (as a trailing character in the number) and
              there is a one or two digit fraction of a dollar (cents).
              The default is:   and


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

.C=color  sets the  color  of the message,  the default is  BLUE.

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



┌──────────────────────────────────────────────────────────────────────────────┐
│                                                                              │
│ prefix                             derivation                                │
├────────┬─────────────────────────────────────────────────────────────────────┤
│  kilo  │ from the Greek for 'thousand'                                       │
│  mega  │ from the Greek for 'large; great'                                   │
│  giga  │ from the Greek for 'giant'.                                         │
│  tera  │ from the Greek for 'monster'.                                       │
│  peta  │ seemingly an invented word based on the Greek prefix  penta  (this  │
│        │ being the fifth prefix in the series).                              │
│  exa   │ apparently derived from the Greek prefix  hexa  by deleting the     │
│        │ first letter.                                                       │
│  zetta │ possibly derived from the Greek word for seven.  However, it is     │
│        │ also said to be adapted from the Italian  setta,  'seven',          │
│        │ which seems more likely.                                            │
│  yotta │ similarly either derived from the Greek  octa,  or from the         │
│        │ Italian  otto,  'eight',  with the last letter changed to match     │
│        │ that of the other prefixes.                                         │
└────────┴─────────────────────────────────────────────────────────────────────┘



                ┌──────────────────────────────────────────────┐
                │      names of some                           │
                │ "American" style numbers               10**n │
                ├───────────────────────────────────────┬──────┤
                │ one                               (1) │    0 │
                │ ten                              (10) │    1 │
                │ hundred                         (100) │    2 │
                │ thousand                      (1,000) │    3 │
                │ million                   (1,000,000) │    6 │
                │ billion               (1,000.000.000) │    9 │
                │ trillion          (1,000,000,000,000) │   12 │
                │ quadrillion                           │   15 │
                │ quintillion                           │   18 │
                │ sextillion                            │   21 │
                │ septillion                            │   24 │
                │ octillion                             │   27 │
                │ nonillion                             │   30 │
                │ decillion                             │   33 │
                │ undecillion                           │   36 │
                │ duodecillion                          │   39 │
                │ tredecillion                          │   42 │
                │ quattuordecillion                     │   45 │
                │ quinquadecillion                      │   48 │
                │ sedecillion                           │   51 │
                │ septendecillion                       │   54 │
                │ octodecillion                         │   57 │
                │ novendecillion                        │   60 │
                │ vigintillion                          │   63 │
                │ unvigintillion                        │   66 │
                │ duovigintillion                       │   69 │
                │ tresvigintillion                      │   72 │
                │ quattuorvigintillion                  │   75 │
                │ quinquavigintillion                   │   78 │
                │ sesvigintillion                       │   81 │
                │ septemvigintillion                    │   84 │
                │ octovigintillion                      │   87 │
                │ novemvigintillion                     │   90 │
                │ trigintillion                         │   93 │
                │ untrigintillion                       │   96 │
                │ duotrigintillion                      │   99 │
                │ trestrigintillion                     │  102 │
                │ quattuortrigintillion                 │  105 │
                │ quinquatrigintillion                  │  108 │
                │ sestrigintillion                      │  111 │
                │ septentrigintillion                   │  114 │
                │ octotrigintillion                     │  117 │
                │ noventrigintillion                    │  120 │
                │ quadragintillion                      │  123 │
                │ unquadragintillion                    │  126 │
                │ duoquadragintillion                   │  129 │
                │ tresquadragintillion                  │  132 │
                │ quattuorquadragintillion              │  135 │
                │ quinquaquadragintillion               │  138 │
                │ sesquadragintillion                   │  141 │
                │ septenquadragintillion                │  144 │
                │ octoquadragintillion                  │  147 │
                │ novenquadragintillion                 │  150 │
                │ quinquagintillion                     │  153 │
                │ unquinquagintillion                   │  156 │
                │ duoquinquagintillion                  │  159 │
                │ tresquinquagintillion                 │  162 │
                │ quattuorquinquagintillion             │  165 │
                │ quinquaquinquagintillion              │  168 │
                │ sesquinquagintillion                  │  171 │
                │ septenquinquagintillion               │  174 │
                │ octoquinquagintillion                 │  177 │
                │ novenquinquagintillion                │  180 │
                │ sexagintillion                        │  183 │
                │ unsexagintillion                      │  186 │
                │ duosexagintillion                     │  189 │
                │ tresexagintillion                     │  192 │
                │ quattuorsexagintillion                │  195 │
                │ quinquasexagintillion                 │  198 │
                │ sesexagintillion                      │  201 │
                │ septensexagintillion                  │  204 │
                │ octosexagintillion                    │  207 │
                │ novensexagintillion                   │  210 │
                │ septuagintillion                      │  213 │
                │ unseptuagintillion                    │  216 │
                │ duoseptuagintillion                   │  219 │
                │ treseptuagintillion                   │  222 │
                │ quattuorseptuagintillion              │  225 │
                │ quinquaseptuagintillion               │  228 │
                │ seseptuagintillion                    │  231 │
                │ septenseptuagintillion                │  234 │
                │ octoseptuagintillion                  │  237 │
                │ novenseptuagintillion                 │  240 │
                │ octogintillion                        │  243 │
                │ unoctogintillion                      │  246 │
                │ duooctogintillion                     │  249 │
                │ tresoctogintillion                    │  252 │
                │ quattuoroctogintillion                │  255 │
                │ quinquaoctogintillion                 │  258 │
                │ sexoctogintillion                     │  261 │
                │ septemoctogintillion                  │  264 │
                │ octooctogintillion                    │  267 │
                │ novemoctogintillion                   │  270 │
                │ nonagintillion                        │  273 │
                │ unnonagintillion                      │  276 │
                │ duononagintillion                     │  279 │
                │ trenonagintillion                     │  282 │
                │ quattuornonagintillion                │  285 │
                │ quinquanonagintillion                 │  288 │
                │ senonagintillion                      │  291 │
                │ septenonagintillion                   │  294 │
                │ octononagintillion                    │  297 │
                │ novenonagintillion                    │  300 │
                │ centillion                            │  303 │
                │ uncentillion                          │  306 │
                │ duocentillion                         │  309 │
                │ trescentillion                        │  312 │
                │ quattuorcentillion                    │  315 │
                │ quinquacentillion                     │  318 │
                │ sexcentillion                         │  321 │
                │ septencentillion                      │  324 │
                │ octocentillion                        │  327 │
                │ novencentillion                       │  330 │
                │ decicentillion                        │  333 │
                │ undecicentillion                      │  336 │
                │ duodecicentillion                     │  339 │
                │ tredecicentillion                     │  342 │
                │ quattuordecicentillion                │  345 │
                │ quinquadecicentillion                 │  348 │
                │ sedecicentillion                      │  351 │
                │ septendecicentillion                  │  354 │
                │ octodecicentillion                    │  357 │
                │ novendecicentillion                   │  360 │
                │ viginticentillion                     │  363 │
                │ unviginticentillion                   │  366 │
                │ duoviginticentillion                  │  369 │
                │ tresviginticentillion                 │  372 │
                │ quattuorviginticentillion             │  375 │
                │ quinquaviginticentillion              │  378 │
                │ sesviginticentillion                  │  381 │
                │ septemviginticentillion               │  384 │
                │ octoviginticentillion                 │  387 │
                │ novemviginticentillion                │  390 │
                │ trigintacentillion                    │  393 │
                │ untrigintacentillion                  │  396 │
                │ duotrigintacentillion                 │  399 │
                │ trestrigintacentillion                │  402 │
                │ quattuortrigintacentillion            │  405 │
                │ quinquatrigintacentillion             │  408 │
                │ sestrigintacentillion                 │  411 │
                │ septentrigintacentillion              │  414 │
                │ octotrigintacentillion                │  417 │
                │ noventrigintacentillion               │  420 │
                │ quadragintacentillion                 │  423 │
                │ unquadragintacentillion               │  426 │
                │ duoquadragintacentillion              │  429 │
                │ tresquadragintacentillion             │  432 │
                │ quattuorquadragintacentillion         │  435 │
                │ quinquaquadragintacentillion          │  438 │
                │ sesquadragintacentillion              │  441 │
                │ septenquadragintacentillion           │  444 │
                │ octoquadragintacentillion             │  447 │
                │ novenquadragintacentillion            │  450 │
                │ quinquagintacentillion                │  453 │
                │ unquinquagintacentillion              │  456 │
                │ duoquinquagintacentillion             │  459 │
                │ tresquinquagintacentillion            │  462 │
                │ quattuorquinquagintacentillion        │  465 │
                │ quinquaquinquagintacentillion         │  468 │
                │ sesquinquagintacentillion             │  471 │
                │ septenquinquagintacentillion          │  474 │
                │ octoquinquagintacentillion            │  477 │
                │ novenquinquagintacentillion           │  480 │
                │ sexagintacentillion                   │  483 │
                │ unsexagintacentillion                 │  486 │
                │ duosexagintacentillion                │  489 │
                │ tresexagintacentillion                │  492 │
                │ quattuorsexagintacentillion           │  495 │
                │ quinquasexagintacentillion            │  498 │
                │ sesexagintacentillion                 │  501 │
                │ septensexagintacentillion             │  504 │
                │ octosexagintacentillion               │  507 │
                │ novensexagintacentillion              │  510 │
                │ septuagintacentillion                 │  513 │
                │ unseptuagintacentillion               │  516 │
                │ duoseptuagintacentillion              │  519 │
                │ treseptuagintacentillion              │  522 │
                │ quattuorseptuagintacentillion         │  525 │
                │ quinquaseptuagintacentillion          │  528 │
                │ seseptuagintacentillion               │  531 │
                │ septenseptuagintacentillion           │  534 │
                │ octoseptuagintacentillion             │  537 │
                │ novenseptuagintacentillion            │  540 │
                │ octogintacentillion                   │  543 │
                │ unoctogintacentillion                 │  546 │
                │ duooctogintacentillion                │  549 │
                │ tresoctogintacentillion               │  552 │
                │ quattuoroctogintacentillion           │  555 │
                │ quinquaoctogintacentillion            │  558 │
                │ sexoctogintacentillion                │  561 │
                │ septemoctogintacentillion             │  564 │
                │ octooctogintacentillion               │  567 │
                │ novemoctogintacentillion              │  570 │
                │ nonagintacentillion                   │  573 │
                │ unnonagintacentillion                 │  576 │
                │ duononagintacentillion                │  579 │
                │ trenonagintacentillion                │  582 │
                │ quattuornonagintacentillion           │  585 │
                │ quinquanonagintacentillion            │  588 │
                │ senonagintacentillion                 │  591 │
                │ septenonagintacentillion              │  594 │
                │ octononagintacentillion               │  597 │
                │ novenonagintacentillion               │  600 │
                │ ducentillion                          │  603 │
                │ unducentillion                        │  606 │
                │ duoducentillion                       │  609 │
                │ treducentillion                       │  612 │
                │ quattuorducentillion                  │  615 │
                │ quinquaducentillion                   │  618 │
                │ seducentillion                        │  621 │
                │ septenducentillion                    │  624 │
                │ octoducentillion                      │  627 │
                │ novenducentillion                     │  630 │
                │ deciducentillion                      │  633 │
                │ undeciducentillion                    │  636 │
                │ duodeciducentillion                   │  639 │
                │ tredeciducentillion                   │  642 │
                │ quattuordeciducentillion              │  645 │
                │ quinquadeciducentillion               │  648 │
                │ sedeciducentillion                    │  651 │
                │ septendeciducentillion                │  654 │
                │ octodeciducentillion                  │  657 │
                │ novendeciducentillion                 │  660 │
                │ vigintiducentillion                   │  663 │
                │ unvigintiducentillion                 │  666 │
                │ duovigintiducentillion                │  669 │
                │ tresvigintiducentillion               │  672 │
                │ quattuorvigintiducentillion           │  675 │
                │ quinquavigintiducentillion            │  678 │
                │ sesvigintiducentillion                │  681 │
                │ septemvigintiducentillion             │  684 │
                │ octovigintiducentillion               │  687 │
                │ novemvigintiducentillion              │  690 │
                │ trigintaducentillion                  │  693 │
                │ untrigintaducentillion                │  696 │
                │ duotrigintaducentillion               │  699 │
                │ trestrigintaducentillion              │  702 │
                │ quattuortrigintaducentillion          │  705 │
                │ quinquatrigintaducentillion           │  708 │
                │ sestrigintaducentillion               │  711 │
                │ septentrigintaducentillion            │  714 │
                │ octotrigintaducentillion              │  717 │
                │ noventrigintaducentillion             │  720 │
                │ quadragintaducentillion               │  723 │
                │ unquadragintaducentillion             │  726 │
                │ duoquadragintaducentillion            │  729 │
                │ tresquadragintaducentillion           │  732 │
                │ quattuorquadragintaducentillion       │  735 │
                │ quinquaquadragintaducentillion        │  738 │
                │ sesquadragintaducentillion            │  741 │
                │ septenquadragintaducentillion         │  744 │
                │ octoquadragintaducentillion           │  747 │
                │ novenquadragintaducentillion          │  750 │
                │ quinquagintaducentillion              │  753 │
                │ unquinquagintaducentillion            │  756 │
                │ duoquinquagintaducentillion           │  759 │
                │ tresquinquagintaducentillion          │  762 │
                │ quattuorquinquagintaducentillion      │  765 │
                │ quinquaquinquagintaducentillion       │  768 │
                │ sesquinquagintaducentillion           │  771 │
                │ septenquinquagintaducentillion        │  774 │
                │ octoquinquagintaducentillion          │  777 │
                │ novenquinquagintaducentillion         │  780 │
                │ sexagintaducentillion                 │  783 │
                │ unsexagintaducentillion               │  786 │
                │ duosexagintaducentillion              │  789 │
                │ tresexagintaducentillion              │  792 │
                │ quattuorsexagintaducentillion         │  795 │
                │ quinquasexagintaducentillion          │  798 │
                │ sesexagintaducentillion               │  801 │
                │ septensexagintaducentillion           │  804 │
                │ octosexagintaducentillion             │  807 │
                │ novensexagintaducentillion            │  810 │
                │ septuagintaducentillion               │  813 │
                │ unseptuagintaducentillion             │  816 │
                │ duoseptuagintaducentillion            │  819 │
                │ treseptuagintaducentillion            │  822 │
                │ quattuorseptuagintaducentillion       │  825 │
                │ quinquaseptuagintaducentillion        │  828 │
                │ seseptuagintaducentillion             │  831 │
                │ septenseptuagintaducentillion         │  834 │
                │ octoseptuagintaducentillion           │  837 │
                │ novenseptuagintaducentillion          │  840 │
                │ octogintaducentillion                 │  843 │
                │ unoctogintaducentillion               │  846 │
                │ duooctogintaducentillion              │  849 │
                │ tresoctogintaducentillion             │  852 │
                │ quattuoroctogintaducentillion         │  855 │
                │ quinquaoctogintaducentillion          │  858 │
                │ sexoctogintaducentillion              │  861 │
                │ septemoctogintaducentillion           │  864 │
                │ octooctogintaducentillion             │  867 │
                │ novemoctogintaducentillion            │  870 │
                │ nonagintaducentillion                 │  873 │
                │ unnonagintaducentillion               │  876 │
                │ duononagintaducentillion              │  879 │
                │ trenonagintaducentillion              │  882 │
                │ quattuornonagintaducentillion         │  885 │
                │ quinquanonagintaducentillion          │  888 │
                │ senonagintaducentillion               │  891 │
                │ septenonagintaducentillion            │  894 │
                │ octononagintaducentillion             │  897 │
                │ novenonagintaducentillion             │  900 │
                │ trecentillion                         │  903 │
                │ untrecentillion                       │  906 │
                │ duotrecentillion                      │  909 │
                │ trestrecentillion                     │  912 │
                │ quattuortrecentillion                 │  915 │
                │ quinquatrecentillion                  │  918 │
                │ sestrecentillion                      │  921 │
                │ septentrecentillion                   │  924 │
                │ octotrecentillion                     │  927 │
                │ noventrecentillion                    │  930 │
                │ decitrecentillion                     │  933 │
                │ undecitrecentillion                   │  936 │
                │ duodecitrecentillion                  │  939 │
                │ tredecitrecentillion                  │  942 │
                │ quattuordecitrecentillion             │  945 │
                │ quinquadecitrecentillion              │  948 │
                │ sedecitrecentillion                   │  951 │
                │ septendecitrecentillion               │  954 │
                │ octodecitrecentillion                 │  957 │
                │ novendecitrecentillion                │  960 │
                │ vigintitrecentillion                  │  963 │
                │ unvigintitrecentillion                │  966 │
                │ duovigintitrecentillion               │  969 │
                │ tresvigintitrecentillion              │  972 │
                │ quattuorvigintitrecentillion          │  975 │
                │ quinquavigintitrecentillion           │  978 │
                │ sesvigintitrecentillion               │  981 │
                │ septemvigintitrecentillion            │  984 │
                │ octovigintitrecentillion              │  987 │
                │ novemvigintitrecentillion             │  990 │
                │ trigintatrecentillion                 │  993 │
                │ untrigintatrecentillion               │  996 │
                │ duotrigintatrecentillion              │  999 │
                │ trestrigintatrecentillion             │ 1002 │
                │ quattuortrigintatrecentillion         │ 1005 │
                │ quinquatrigintatrecentillion          │ 1008 │
                │ sestrigintatrecentillion              │ 1011 │
                │ septentrigintatrecentillion           │ 1014 │
                │ octotrigintatrecentillion             │ 1017 │
                │ noventrigintatrecentillion            │ 1020 │
                │ quadragintatrecentillion              │ 1023 │
                │ unquadragintatrecentillion            │ 1026 │
                │ duoquadragintatrecentillion           │ 1029 │
                │ tresquadragintatrecentillion          │ 1032 │
                │ quattuorquadragintatrecentillion      │ 1035 │
                │ quinquaquadragintatrecentillion       │ 1038 │
                │ sesquadragintatrecentillion           │ 1041 │
                │ septenquadragintatrecentillion        │ 1044 │
                │ octoquadragintatrecentillion          │ 1047 │
                │ novenquadragintatrecentillion         │ 1050 │
                │ quinquagintatrecentillion             │ 1053 │
                │ unquinquagintatrecentillion           │ 1056 │
                │ duoquinquagintatrecentillion          │ 1059 │
                │ tresquinquagintatrecentillion         │ 1062 │
                │ quattuorquinquagintatrecentillion     │ 1065 │
                │ quinquaquinquagintatrecentillion      │ 1068 │
                │ sesquinquagintatrecentillion          │ 1071 │
                │ septenquinquagintatrecentillion       │ 1074 │
                │ octoquinquagintatrecentillion         │ 1077 │
                │ novenquinquagintatrecentillion        │ 1080 │
                │ sexagintatrecentillion                │ 1083 │
                │ unsexagintatrecentillion              │ 1086 │
                │ duosexagintatrecentillion             │ 1089 │
                │ tresexagintatrecentillion             │ 1092 │
                │ quattuorsexagintatrecentillion        │ 1095 │
                │ quinquasexagintatrecentillion         │ 1098 │
                │ sesexagintatrecentillion              │ 1101 │
                │ septensexagintatrecentillion          │ 1104 │
                │ octosexagintatrecentillion            │ 1107 │
                │ novensexagintatrecentillion           │ 1110 │
                │ septuagintatrecentillion              │ 1113 │
                │ unseptuagintatrecentillion            │ 1116 │
                │ duoseptuagintatrecentillion           │ 1119 │
                │ treseptuagintatrecentillion           │ 1122 │
                │ quattuorseptuagintatrecentillion      │ 1125 │
                │ quinquaseptuagintatrecentillion       │ 1128 │
                │ seseptuagintatrecentillion            │ 1131 │
                │ septenseptuagintatrecentillion        │ 1134 │
                │ octoseptuagintatrecentillion          │ 1137 │
                │ novenseptuagintatrecentillion         │ 1140 │
                │ octogintatrecentillion                │ 1143 │
                │ unoctogintatrecentillion              │ 1146 │
                │ duooctogintatrecentillion             │ 1149 │
                │ tresoctogintatrecentillion            │ 1152 │
                │ quattuoroctogintatrecentillion        │ 1155 │
                │ quinquaoctogintatrecentillion         │ 1158 │
                │ sexoctogintatrecentillion             │ 1161 │
                │ septemoctogintatrecentillion          │ 1164 │
                │ octooctogintatrecentillion            │ 1167 │
                │ novemoctogintatrecentillion           │ 1170 │
                │ nonagintatrecentillion                │ 1173 │
                │ unnonagintatrecentillion              │ 1176 │
                │ duononagintatrecentillion             │ 1179 │
                │ trenonagintatrecentillion             │ 1182 │
                │ quattuornonagintatrecentillion        │ 1185 │
                │ quinquanonagintatrecentillion         │ 1188 │
                │ senonagintatrecentillion              │ 1191 │
                │ septenonagintatrecentillion           │ 1194 │
                │ octononagintatrecentillion            │ 1197 │
                │ novenonagintatrecentillion            │ 1200 │
                │ quadringentillion                     │ 1203 │
                │ unquadringentillion                   │ 1206 │
                │ duoquadringentillion                  │ 1209 │
                │ tresquadringentillion                 │ 1212 │
                │ quattuorquadringentillion             │ 1215 │
                │ quinquaquadringentillion              │ 1218 │
                │ sesquadringentillion                  │ 1221 │
                │ septenquadringentillion               │ 1224 │
                │ octoquadringentillion                 │ 1227 │
                │ novenquadringentillion                │ 1230 │
                │ deciquadringentillion                 │ 1233 │
                │ undeciquadringentillion               │ 1236 │
                │ duodeciquadringentillion              │ 1239 │
                │ tredeciquadringentillion              │ 1242 │
                │ quattuordeciquadringentillion         │ 1245 │
                │ quinquadeciquadringentillion          │ 1248 │
                │ sedeciquadringentillion               │ 1251 │
                │ septendeciquadringentillion           │ 1254 │
                │ octodeciquadringentillion             │ 1257 │
                │ novendeciquadringentillion            │ 1260 │
                │ vigintiquadringentillion              │ 1263 │
                │ unvigintiquadringentillion            │ 1266 │
                │ duovigintiquadringentillion           │ 1269 │
                │ tresvigintiquadringentillion          │ 1272 │
                │ quattuorvigintiquadringentillion      │ 1275 │
                │ quinquavigintiquadringentillion       │ 1278 │
                │ sesvigintiquadringentillion           │ 1281 │
                │ septemvigintiquadringentillion        │ 1284 │
                │ octovigintiquadringentillion          │ 1287 │
                │ novemvigintiquadringentillion         │ 1290 │
                │ trigintaquadringentillion             │ 1293 │
                │ untrigintaquadringentillion           │ 1296 │
                │ duotrigintaquadringentillion          │ 1299 │
                │ trestrigintaquadringentillion         │ 1302 │
                │ quattuortrigintaquadringentillion     │ 1305 │
                │ quinquatrigintaquadringentillion      │ 1308 │
                │ sestrigintaquadringentillion          │ 1311 │
                │ septentrigintaquadringentillion       │ 1314 │
                │ octotrigintaquadringentillion         │ 1317 │
                │ noventrigintaquadringentillion        │ 1320 │
                │ quadragintaquadringentillion          │ 1323 │
                │ unquadragintaquadringentillion        │ 1326 │
                │ duoquadragintaquadringentillion       │ 1329 │
                │ tresquadragintaquadringentillion      │ 1332 │
                │ quattuorquadragintaquadringentillion  │ 1335 │
                │ quinquaquadragintaquadringentillion   │ 1338 │
                │ sesquadragintaquadringentillion       │ 1341 │
                │ septenquadragintaquadringentillion    │ 1344 │
                │ octoquadragintaquadringentillion      │ 1347 │
                │ novenquadragintaquadringentillion     │ 1350 │
                │ quinquagintaquadringentillion         │ 1353 │
                │ unquinquagintaquadringentillion       │ 1356 │
                │ duoquinquagintaquadringentillion      │ 1359 │
                │ tresquinquagintaquadringentillion     │ 1362 │
                │ quattuorquinquagintaquadringentillion │ 1365 │
                │ quinquaquinquagintaquadringentillion  │ 1368 │
                │ sesquinquagintaquadringentillion      │ 1371 │
                │ septenquinquagintaquadringentillion   │ 1374 │
                │ octoquinquagintaquadringentillion     │ 1377 │
                │ novenquinquagintaquadringentillion    │ 1380 │
                │ sexagintaquadringentillion            │ 1383 │
                │ unsexagintaquadringentillion          │ 1386 │
                │ duosexagintaquadringentillion         │ 1389 │
                │ tresexagintaquadringentillion         │ 1392 │
                │ quattuorsexagintaquadringentillion    │ 1395 │
                │ quinquasexagintaquadringentillion     │ 1398 │
                │ sesexagintaquadringentillion          │ 1401 │
                │ septensexagintaquadringentillion      │ 1404 │
                │ octosexagintaquadringentillion        │ 1407 │
                │ novensexagintaquadringentillion       │ 1410 │
                │ septuagintaquadringentillion          │ 1413 │
                │ unseptuagintaquadringentillion        │ 1416 │
                │ duoseptuagintaquadringentillion       │ 1419 │
                │ treseptuagintaquadringentillion       │ 1422 │
                │ quattuorseptuagintaquadringentillion  │ 1425 │
                │ quinquaseptuagintaquadringentillion   │ 1428 │
                │ seseptuagintaquadringentillion        │ 1431 │
                │ septenseptuagintaquadringentillion    │ 1434 │
                │ octoseptuagintaquadringentillion      │ 1437 │
                │ novenseptuagintaquadringentillion     │ 1440 │
                │ octogintaquadringentillion            │ 1443 │
                │ unoctogintaquadringentillion          │ 1446 │
                │ duooctogintaquadringentillion         │ 1449 │
                │ tresoctogintaquadringentillion        │ 1452 │
                │ quattuoroctogintaquadringentillion    │ 1455 │
                │ quinquaoctogintaquadringentillion     │ 1458 │
                │ sexoctogintaquadringentillion         │ 1461 │
                │ septemoctogintaquadringentillion      │ 1464 │
                │ octooctogintaquadringentillion        │ 1467 │
                │ novemoctogintaquadringentillion       │ 1470 │
                │ nonagintaquadringentillion            │ 1473 │
                │ unnonagintaquadringentillion          │ 1476 │
                │ duononagintaquadringentillion         │ 1479 │
                │ trenonagintaquadringentillion         │ 1482 │
                │ quattuornonagintaquadringentillion    │ 1485 │
                │ quinquanonagintaquadringentillion     │ 1488 │
                │ senonagintaquadringentillion          │ 1491 │
                │ septenonagintaquadringentillion       │ 1494 │
                │ octononagintaquadringentillion        │ 1497 │
                │ novenonagintaquadringentillion        │ 1500 │
                │ quingentillion                        │ 1503 │
                │ unquingentillion                      │ 1506 │
                │ duoquingentillion                     │ 1509 │
                │ tresquingentillion                    │ 1512 │
                │ quattuorquingentillion                │ 1515 │
                │ quinquaquingentillion                 │ 1518 │
                │ sesquingentillion                     │ 1521 │
                │ septenquingentillion                  │ 1524 │
                │ octoquingentillion                    │ 1527 │
                │ novenquingentillion                   │ 1530 │
                │ deciquingentillion                    │ 1533 │
                │ undeciquingentillion                  │ 1536 │
                │ duodeciquingentillion                 │ 1539 │
                │ tredeciquingentillion                 │ 1542 │
                │ quattuordeciquingentillion            │ 1545 │
                │ quinquadeciquingentillion             │ 1548 │
                │ sedeciquingentillion                  │ 1551 │
                │ septendeciquingentillion              │ 1554 │
                │ octodeciquingentillion                │ 1557 │
                │ novendeciquingentillion               │ 1560 │
                │ vigintiquingentillion                 │ 1563 │
                │ unvigintiquingentillion               │ 1566 │
                │ duovigintiquingentillion              │ 1569 │
                │ tresvigintiquingentillion             │ 1572 │
                │ quattuorvigintiquingentillion         │ 1575 │
                │ quinquavigintiquingentillion          │ 1578 │
                │ sesvigintiquingentillion              │ 1581 │
                │ septemvigintiquingentillion           │ 1584 │
                │ octovigintiquingentillion             │ 1587 │
                │ novemvigintiquingentillion            │ 1590 │
                │ trigintaquingentillion                │ 1593 │
                │ untrigintaquingentillion              │ 1596 │
                │ duotrigintaquingentillion             │ 1599 │
                │ trestrigintaquingentillion            │ 1602 │
                │ quattuortrigintaquingentillion        │ 1605 │
                │ quinquatrigintaquingentillion         │ 1608 │
                │ sestrigintaquingentillion             │ 1611 │
                │ septentrigintaquingentillion          │ 1614 │
                │ octotrigintaquingentillion            │ 1617 │
                │ noventrigintaquingentillion           │ 1620 │
                │ quadragintaquingetillion              │ 1623 │
                │ unquadragintaquingentillion           │ 1626 │
                │ duoquadragintaquingentillion          │ 1629 │
                │ tresquadragintaquingentillion         │ 1632 │
                │ quattuorquadragintaquingentillion     │ 1635 │
                │ quinquaquadragintaquingentillion      │ 1638 │
                │ sesquadragintaquingentillion          │ 1641 │
                │ septenquadragintaquingentillion       │ 1644 │
                │ octoquadragintaquingentillion         │ 1647 │
                │ novenquadragintaquingentillion        │ 1650 │
                │ quinquagintaquingentillion            │ 1653 │
                │ unquinquagintaquingentillion          │ 1656 │
                │ duoquinquagintaquingentillion         │ 1659 │
                │ tresquinquagintaquingentillion        │ 1662 │
                │ quattuorquinquagintaquingentillion    │ 1665 │
                │ quinquaquinquagintaquingentillion     │ 1668 │
                │ sesquinquagintaquingentillion         │ 1671 │
                │ septenquinquagintaquingentillion      │ 1674 │
                │ octoquinquagintaquingentillion        │ 1677 │
                │ novenquinquagintaquingentillion       │ 1680 │
                │ sexagintaquingentillion               │ 1683 │
                │ unsexagintaquingentillion             │ 1686 │
                │ duosexagintaquingentillion            │ 1689 │
                │ tresexagintaquingentillion            │ 1692 │
                │ quattuorsexagintaquingentillion       │ 1695 │
                │ quinquasexagintaquingentillion        │ 1698 │
                │ sesexagintaquingentillion             │ 1701 │
                │ septensexagintaquingentillion         │ 1704 │
                │ octosexagintaquingentillion           │ 1707 │
                │ novensexagintaquingentillion          │ 1710 │
                │ septuagintaquingentillion             │ 1713 │
                │ unseptuagintaquingentillion           │ 1716 │
                │ duoseptuagintaquingentillion          │ 1719 │
                │ treseptuagintaquingentillion          │ 1722 │
                │ quattuorseptuagintaquingentillion     │ 1725 │
                │ quinquaseptuagintaquingentillion      │ 1728 │
                │ seseptuagintaquingentillion           │ 1731 │
                │ septenseptuagintaquingentillion       │ 1734 │
                │ octoseptuagintaquingentillion         │ 1737 │
                │ novenseptuagintaquingentillion        │ 1740 │
                │ octogintaquingentillion               │ 1743 │
                │ unoctogintaquingentillion             │ 1746 │
                │ duooctogintaquingentillion            │ 1749 │
                │ tresoctogintaquingentillion           │ 1752 │
                │ quattuoroctogintaquingentillion       │ 1755 │
                │ quinquaoctogintaquingentillion        │ 1758 │
                │ sexoctogintaquingentillion            │ 1761 │
                │ septemoctogintaquingentillion         │ 1764 │
                │ octooctogintaquingentillion           │ 1767 │
                │ novemoctogintaquingentillion          │ 1770 │
                │ nonagintaquingentillion               │ 1773 │
                │ unnonagintaquingentillion             │ 1776 │
                │ duononagintaquingentillion            │ 1779 │
                │ trenonagintaquingentillion            │ 1782 │
                │ quattuornonagintaquingentillion       │ 1785 │
                │ quinquanonagintaquingentillion        │ 1788 │
                │ senonagintaquingentillion             │ 1791 │
                │ septenonagintaquingentillion          │ 1794 │
                │ octononagintaquingentillion           │ 1797 │
                │ novenonagintaquingentillion           │ 1800 │
                │ sescentillion                         │ 1803 │
                │ unsescentillion                       │ 1806 │
                │ duosescentillion                      │ 1809 │
                │ tresescentillion                      │ 1812 │
                │ quattuorsescentillion                 │ 1815 │
                │ quinquasescentillion                  │ 1818 │
                │ sesescentillion                       │ 1821 │
                │ septensescentillion                   │ 1824 │
                │ octosescentillion                     │ 1827 │
                │ novensescentillion                    │ 1830 │
                │ decisescentillion                     │ 1833 │
                │ undecisescentillion                   │ 1836 │
                │ duodecisescentillion                  │ 1839 │
                │ tredecisescentillion                  │ 1842 │
                │ quattuordecisescentillion             │ 1845 │
                │ quinquadecisescentillion              │ 1848 │
                │ sedecisescentillion                   │ 1851 │
                │ septendecisescentillion               │ 1854 │
                │ octodecisescentillion                 │ 1857 │
                │ novendecisescentillion                │ 1860 │
                │ vigintisescentillion                  │ 1863 │
                │ unvigintisescentillion                │ 1866 │
                │ duovigintisescentillion               │ 1869 │
                │ tresvigintisescentillion              │ 1872 │
                │ quattuorvigintisescentillion          │ 1875 │
                │ quinquavigintisescentillion           │ 1878 │
                │ sesvigintisescentillion               │ 1881 │
                │ septemvigintisescentillion            │ 1884 │
                │ octovigintisescentillion              │ 1887 │
                │ novemvigintisescentillion             │ 1890 │
                │ trigintasescentillion                 │ 1893 │
                │ untrigintasescentillion               │ 1896 │
                │ duotrigintasescentillion              │ 1899 │
                │ trestrigintasescentillion             │ 1902 │
                │ quattuortrigintasescentillion         │ 1905 │
                │ quinquatrigintasescentillion          │ 1908 │
                │ sestrigintasescentillion              │ 1911 │
                │ septentrigintasescentillion           │ 1914 │
                │ octotrigintasescentillion             │ 1917 │
                │ noventrigintasescentillion            │ 1920 │
                │ quadragintasescentillion              │ 1923 │
                │ unquadragintasescentillion            │ 1926 │
                │ duoquadragintasescentillion           │ 1929 │
                │ tresquadragintasescentillion          │ 1932 │
                │ quattuorquadragintasescentillion      │ 1935 │
                │ quinquaquadragintasescentillion       │ 1938 │
                │ sesquadragintasescentillion           │ 1941 │
                │ septenquadragintasescentillion        │ 1944 │
                │ octoquadragintasescentillion          │ 1947 │
                │ novenquadragintasescentillion         │ 1950 │
                │ quinquagintasescentillion             │ 1953 │
                │ unquinquagintasescentillion           │ 1956 │
                │ duoquinquagintasescentillion          │ 1959 │
                │ tresquinquagintasescentillion         │ 1962 │
                │ quattuorquinquagintasescentillion     │ 1965 │
                │ quinquaquinquagintasescentillion      │ 1968 │
                │ sesquinquagintasescentillion          │ 1971 │
                │ septenquinquagintasescentillion       │ 1974 │
                │ octoquinquagintasescentillion         │ 1977 │
                │ novenquinquagintasescentillion        │ 1980 │
                │ sexagintasescentillion                │ 1983 │
                │ unsexagintasescentillion              │ 1986 │
                │ duosexagintasescentillion             │ 1989 │
                │ tresexagintasescentillion             │ 1992 │
                │ quattuorsexagintasescentillion        │ 1995 │
                │ quinquasexagintasescentillion         │ 1998 │
                │ sesexagintasescentillion              │ 2001 │
                │ septensexagintasescentillion          │ 2004 │
                │ octosexagintasescentillion            │ 2007 │
                │ novensexagintasescentillion           │ 2010 │
                │ septuagintasescentillion              │ 2013 │
                │ unseptuagintasescentillion            │ 2016 │
                │ duoseptuagintasescentillion           │ 2019 │
                │ treseptuagintasescentillion           │ 2022 │
                │ quattuorseptuagintasescentillion      │ 2025 │
                │ quinquaseptuagintasescentillion       │ 2028 │
                │ seseptuagintasescentillion            │ 2031 │
                │ septenseptuagintasescentillion        │ 2034 │
                │ octoseptuagintasescentillion          │ 2037 │
                │ novenseptuagintasescentillion         │ 2040 │
                │ octogintasescentillion                │ 2043 │
                │ unoctogintasescentillion              │ 2046 │
                │ duooctogintasescentillion             │ 2049 │
                │ tresoctogintasescentillion            │ 2052 │
                │ quattuoroctogintasescentillion        │ 2055 │
                │ quinquaoctogintasescentillion         │ 2058 │
                │ sexoctogintasescentillion             │ 2061 │
                │ septemoctogintasescentillion          │ 2064 │
                │ octooctogintasescentillion            │ 2067 │
                │ novemoctogintasescentillion           │ 2070 │
                │ nonagintasescetillion                 │ 2073 │
                │ unnonagintasescentillion              │ 2076 │
                │ duononagintasescentillion             │ 2079 │
                │ trenonagintasescentillion             │ 2082 │
                │ quattuornonagintasescentillion        │ 2085 │
                │ quinquanonagintasescentillion         │ 2088 │
                │ senonagintasescentillion              │ 2091 │
                │ septenonagintasescentillion           │ 2094 │
                │ octononagintasescentillion            │ 2097 │
                │ novenonagintasescentillion            │ 2100 │
                │ septingentillion                      │ 2103 │
                │ unseptingentillion                    │ 2106 │
                │ duoseptingentillion                   │ 2109 │
                │ treseptingentillion                   │ 2112 │
                │ quattuorseptingentillion              │ 2115 │
                │ quinquaseptingentillion               │ 2118 │
                │ seseptingentillion                    │ 2121 │
                │ septenseptingentillion                │ 2124 │
                │ octoseptingentillion                  │ 2127 │
                │ novenseptingentillion                 │ 2130 │
                │ deciseptingentillion                  │ 2133 │
                │ undeciseptingentillion                │ 2136 │
                │ duodeciseptingentillion               │ 2139 │
                │ tredeciseptingentillion               │ 2142 │
                │ quattuordeciseptingentillion          │ 2145 │
                │ quinquadeciseptingentillion           │ 2148 │
                │ sedeciseptingentillion                │ 2151 │
                │ septendeciseptingentillion            │ 2154 │
                │ octodeciseptingentillion              │ 2157 │
                │ novendeciseptingentillion             │ 2160 │
                │ vigintiseptingentillion               │ 2163 │
                │ unvigintiseptingentillion             │ 2166 │
                │ duovigintiseptingentillion            │ 2169 │
                │ tresvigintiseptingentillion           │ 2172 │
                │ quattuorvigintiseptingentillion       │ 2175 │
                │ quinquavigintiseptingentillion        │ 2178 │
                │ sesvigintiseptingentillion            │ 2181 │
                │ septemvigintiseptingentillion         │ 2184 │
                │ octovigintiseptingentillion           │ 2187 │
                │ novemvigintiseptingentillion          │ 2190 │
                │ trigintaseptingentillion              │ 2193 │
                │ untrigintaseptingentillion            │ 2196 │
                │ duotrigintaseptingentillion           │ 2199 │
                │ trestrigintaseptingentillion          │ 2202 │
                │ quattuortrigintaseptingentillion      │ 2205 │
                │ quinquatrigintaseptingentillion       │ 2208 │
                │ sestrigintaseptingentillion           │ 2211 │
                │ septentrigintaseptingentillion        │ 2214 │
                │ octotrigintaseptingentillion          │ 2217 │
                │ noventrigintaseptingentillion         │ 2220 │
                │ quadragintaseptingentillion           │ 2223 │
                │ unquadragintaseptingentillion         │ 2226 │
                │ duoquadragintaseptingentillion        │ 2229 │
                │ tresquadragintaseptingentillion       │ 2232 │
                │ quattuorquadragintaseptingentillion   │ 2235 │
                │ quinquaquadragintaseptingentillion    │ 2238 │
                │ sesquadragintaseptingentillion        │ 2241 │
                │ septenquadragintaseptingentillion     │ 2244 │
                │ octoquadragintaseptingentillion       │ 2247 │
                │ novenquadragintaseptingentillion      │ 2250 │
                │ quinquagintaseptingentillion          │ 2253 │
                │ unquinquagintaseptingentillion        │ 2256 │
                │ duoquinquagintaseptingentillion       │ 2259 │
                │ tresquinquagintaseptingentillion      │ 2262 │
                │ quattuorquinquagintaseptingentillion  │ 2265 │
                │ quinquaquinquagintaseptingentillion   │ 2268 │
                │ sesquinquagintaseptingentillion       │ 2271 │
                │ septenquinquagintaseptingentillion    │ 2274 │
                │ octoquinquagintaseptingentillion      │ 2277 │
                │ novenquinquagintaseptingentillion     │ 2280 │
                │ sexagintaseptingentillion             │ 2283 │
                │ unsexagintaseptingentillion           │ 2286 │
                │ duosexagintaseptingentillion          │ 2289 │
                │ tresexagintaseptingentillion          │ 2292 │
                │ quattuorsexagintaseptingentillion     │ 2295 │
                │ quinquasexagintaseptingentillion      │ 2298 │
                │ sesexagintaseptingentillion           │ 2301 │
                │ septensexagintaseptingentillion       │ 2304 │
                │ octosexagintaseptingentillion         │ 2307 │
                │ novensexagintaseptingentillion        │ 2310 │
                │ septuagintaseptingentillion           │ 2313 │
                │ unseptuagintaseptingentillion         │ 2316 │
                │ duoseptuagintaseptingentillion        │ 2319 │
                │ treseptuagintaseptingentillion        │ 2322 │
                │ quattuorseptuagintaseptingentillion   │ 2325 │
                │ quinquaseptuagintaseptingentillion    │ 2328 │
                │ seseptuagintaseptingentillion         │ 2331 │
                │ septenseptuagintaseptingentillion     │ 2334 │
                │ octoseptuagintaseptingentillion       │ 2337 │
                │ novenseptuagintaseptingentillion      │ 2340 │
                │ octogintaseptingentillion             │ 2343 │
                │ unoctogintaseptingentillion           │ 2346 │
                │ duooctogintaseptingentillion          │ 2349 │
                │ tresoctogintaseptingentillion         │ 2352 │
                │ quattuoroctogintaseptingentillion     │ 2355 │
                │ quinquaoctogintaseptingentillion      │ 2358 │
                │ sexoctogintaseptingentillion          │ 2361 │
                │ septemoctogintaseptingentillion       │ 2364 │
                │ octooctogintaseptingentillion         │ 2367 │
                │ novemoctogintaseptingentillion        │ 2370 │
                │ nonagintaseptingentillion             │ 2373 │
                │ unnonagintaseptingentillion           │ 2376 │
                │ duononagintaseptingentillion          │ 2379 │
                │ trenonagintaseptingentillion          │ 2382 │
                │ quattuornonagintaseptingentillion     │ 2385 │
                │ quinquanonagintaseptingentillion      │ 2388 │
                │ senonagintaseptingentillion           │ 2391 │
                │ septenonagintaseptingentillion        │ 2394 │
                │ octononagintaseptingentillion         │ 2397 │
                │ novenonagintaseptingentillion         │ 2400 │
                │ octingentillion                       │ 2403 │
                │ unoctingentillion                     │ 2406 │
                │ duooctingentillion                    │ 2409 │
                │ tresoctingentillion                   │ 2412 │
                │ quattuoroctingentillion               │ 2415 │
                │ quinquaoctingentillion                │ 2418 │
                │ sexoctingentillion                    │ 2421 │
                │ septemoctingentillion                 │ 2424 │
                │ octooctingentillion                   │ 2427 │
                │ novemoctingentillion                  │ 2430 │
                │ decioctingentillion                   │ 2433 │
                │ undecioctingentillion                 │ 2436 │
                │ duodecioctingentillion                │ 2439 │
                │ tredecioctingentillion                │ 2442 │
                │ quattuordecioctingentillion           │ 2445 │
                │ quinquadecioctingentillion            │ 2448 │
                │ sedecioctingentillion                 │ 2451 │
                │ septendecioctingentillion             │ 2454 │
                │ octodecioctingentillion               │ 2457 │
                │ novendecioctingentillion              │ 2460 │
                │ vigintioctingentillion                │ 2463 │
                │ unvigintioctingentillion              │ 2466 │
                │ duovigintioctingentillion             │ 2469 │
                │ tresvigintioctingentillion            │ 2472 │
                │ quattuorvigintioctingentillion        │ 2475 │
                │ quinquavigintioctingentillion         │ 2478 │
                │ sesvigintioctingentillion             │ 2481 │
                │ septemvigintioctingentillion          │ 2484 │
                │ octovigintioctingentillion            │ 2487 │
                │ novemvigintioctingentillion           │ 2490 │
                │ trigintaoctingentillion               │ 2493 │
                │ untrigintaoctingentillion             │ 2496 │
                │ duotrigintaoctingentillion            │ 2499 │
                │ trestrigintaoctingentillion           │ 2502 │
                │ quattuortrigintaoctingentillion       │ 2505 │
                │ quinquatrigintaoctingentillion        │ 2508 │
                │ sestrigintaoctingentillion            │ 2511 │
                │ septentrigintaoctingentillion         │ 2514 │
                │ octotrigintaoctingentillion           │ 2517 │
                │ noventrigintaoctingentillion          │ 2520 │
                │ quadragintaoctingentillion            │ 2523 │
                │ unquadragintaoctingentillion          │ 2526 │
                │ duoquadragintaoctingentillion         │ 2529 │
                │ tresquadragintaoctingentillion        │ 2532 │
                │ quattuorquadragintaoctingentillion    │ 2535 │
                │ quinquaquadragintaoctingentillion     │ 2538 │
                │ sesquadragintaoctingentillion         │ 2541 │
                │ septenquadragintaoctingentillion      │ 2544 │
                │ octoquadragintaoctingentillion        │ 2547 │
                │ novenquadragintaoctingentillion       │ 2550 │
                │ quinquagintaoctingentillion           │ 2553 │
                │ unquinquagintaoctingentillion         │ 2556 │
                │ duoquinquagintaoctingentillion        │ 2559 │
                │ tresquinquagintaoctingentillion       │ 2562 │
                │ quattuorquinquagintaoctingentillion   │ 2565 │
                │ quinquaquinquagintaoctingentillion    │ 2568 │
                │ sesquinquagintaoctingentillion        │ 2571 │
                │ septenquinquagintaoctingentillion     │ 2574 │
                │ octoquinquagintaoctingentillion       │ 2577 │
                │ novenquinquagintaoctingentillion      │ 2580 │
                │ sexagintaoctingentillion              │ 2583 │
                │ unsexagintaoctingentillion            │ 2586 │
                │ duosexagintaoctingentillion           │ 2589 │
                │ tresexagintaoctingentillion           │ 2592 │
                │ quattuorsexagintaoctingentillion      │ 2595 │
                │ quinquasexagintaoctingentillion       │ 2598 │
                │ sesexagintaoctingentillion            │ 2601 │
                │ septensexagintaoctingentillion        │ 2604 │
                │ octosexagintaoctingentillion          │ 2607 │
                │ novensexagintaoctingentillion         │ 2610 │
                │ septuagintaoctingentillion            │ 2613 │
                │ unseptuagintaoctingentillion          │ 2616 │
                │ duoseptuagintaoctingentillion         │ 2619 │
                │ treseptuagintaoctingentillion         │ 2622 │
                │ quattuorseptuagintaoctingentillion    │ 2625 │
                │ quinquaseptuagintaoctingentillion     │ 2628 │
                │ seseptuagintaoctingentillion          │ 2631 │
                │ septenseptuagintaoctingentillion      │ 2634 │
                │ octoseptuagintaoctingentillion        │ 2637 │
                │ novenseptuagintaoctingentillion       │ 2640 │
                │ octogintaoctingentillion              │ 2643 │
                │ unoctogintaoctingentillion            │ 2646 │
                │ duooctogintaoctingentillion           │ 2649 │
                │ tresoctogintaoctingentillion          │ 2652 │
                │ quattuoroctogintaoctingentillion      │ 2655 │
                │ quinquaoctogintaoctingentillion       │ 2658 │
                │ sexoctogintaoctingentillion           │ 2661 │
                │ septemoctogintaoctingentillion        │ 2664 │
                │ octooctogintaoctingentillion          │ 2667 │
                │ novemoctogintaoctingentillion         │ 2670 │
                │ nonagintaoctingentillion              │ 2673 │
                │ unnonagintaoctingentillion            │ 2676 │
                │ duononagintaoctingentillion           │ 2679 │
                │ trenonagintaoctingentillion           │ 2682 │
                │ quattuornonagintaoctingentillion      │ 2685 │
                │ quinquanonagintaoctingentillion       │ 2688 │
                │ senonagintaoctingentillion            │ 2691 │
                │ septenonagintaoctingentillion         │ 2694 │
                │ octononagintaoctingentillion          │ 2697 │
                │ novenonagintaoctingentillion          │ 2700 │
                │ nongentillion                         │ 2703 │
                │ unnongentillion                       │ 2706 │
                │ duonongentillion                      │ 2709 │
                │ trenongentillion                      │ 2712 │
                │ quattuornongentillion                 │ 2715 │
                │ quinquanongentillion                  │ 2718 │
                │ senongentillion                       │ 2721 │
                │ septenongentillion                    │ 2724 │
                │ octonongentillion                     │ 2727 │
                │ novenongentillion                     │ 2730 │
                │ decinongentillion                     │ 2733 │
                │ undecinongentillion                   │ 2736 │
                │ duodecinongentillion                  │ 2739 │
                │ tredecinongentillion                  │ 2742 │
                │ quattuordecinongentillion             │ 2745 │
                │ quinquadecinongentillion              │ 2748 │
                │ sedecinongentillion                   │ 2751 │
                │ septendecinongentillion               │ 2754 │
                │ octodecinongentillion                 │ 2757 │
                │ novendecinongentillion                │ 2760 │
                │ vigintinongentillion                  │ 2763 │
                │ unvigintinongentillion                │ 2766 │
                │ duovigintinongentillion               │ 2769 │
                │ tresvigintinongentillion              │ 2772 │
                │ quattuorvigintinongentillion          │ 2775 │
                │ quinquavigintinongentillion           │ 2778 │
                │ sesvigintinongentillion               │ 2781 │
                │ septemvigintinongentillion            │ 2784 │
                │ octovigintinongentillion              │ 2787 │
                │ novemvigintinongentillion             │ 2790 │
                │ trigintanongentillion                 │ 2793 │
                │ untrigintanongentillion               │ 2796 │
                │ duotrigintanongentillion              │ 2799 │
                │ trestrigintanongentillion             │ 2802 │
                │ quattuortrigintanongentillion         │ 2805 │
                │ quinquatrigintanongentillion          │ 2808 │
                │ sestrigintanongentillion              │ 2811 │
                │ septentrigintanongentillion           │ 2814 │
                │ octotrigintanongentillion             │ 2817 │
                │ noventrigintanongentillion            │ 2820 │
                │ quadragintanongentillion              │ 2823 │
                │ unquadragintanongentillion            │ 2826 │
                │ duoquadragintanongentillion           │ 2829 │
                │ tresquadragintanongentillion          │ 2832 │
                │ quattuorquadragintanongentillion      │ 2835 │
                │ quinquaquadragintanongentillion       │ 2838 │
                │ sesquadragintanongentillion           │ 2841 │
                │ septenquadragintanongentillion        │ 2844 │
                │ octoquadragintanongentillion          │ 2847 │
                │ novenquadragintanongentillion         │ 2850 │
                │ quinquagintanongentillion             │ 2853 │
                │ unquinquagintanongentillion           │ 2856 │
                │ duoquinquagintanongentillion          │ 2859 │
                │ tresquinquagintanongentillion         │ 2862 │
                │ quattuorquinquagintanongentillion     │ 2865 │
                │ quinquaquinquagintanongentillion      │ 2868 │
                │ sesquinquagintanongentillion          │ 2871 │
                │ septenquinquagintanongentillion       │ 2874 │
                │ octoquinquagintanongentillion         │ 2877 │
                │ novenquinquagintanongentillion        │ 2880 │
                │ sexagintanongentillion                │ 2883 │
                │ unsexagintanongentillion              │ 2886 │
                │ duosexagintanongentillion             │ 2889 │
                │ tresexagintanongentillion             │ 2892 │
                │ quattuorsexagintanongentillion        │ 2895 │
                │ quinquasexagintanongentillion         │ 2898 │
                │ sesexagintanongentillion              │ 2901 │
                │ septensexagintanongentillion          │ 2904 │
                │ octosexagintanongentillion            │ 2907 │
                │ novensexagintanongentillion           │ 2910 │
                │ septuagintanongentillion              │ 2913 │
                │ unseptuagintanongentillion            │ 2916 │
                │ duoseptuagintanongentillion           │ 2919 │
                │ treseptuagintanongentillion           │ 2922 │
                │ quattuorseptuagintanongentillion      │ 2925 │
                │ quinquaseptuagintanongentillion       │ 2928 │
                │ seseptuagintanongentillion            │ 2931 │
                │ septenseptuagintanongentillion        │ 2934 │
                │ octoseptuagintanongentillion          │ 2937 │
                │ novenseptuagintanongentillion         │ 2940 │
                │ octogintanongentillion                │ 2943 │
                │ unoctogintanongentillion              │ 2946 │
                │ duooctogintanongentillion             │ 2949 │
                │ tresoctogintanongentillion            │ 2952 │
                │ quattuoroctogintanongentillion        │ 2955 │
                │ quinquaoctogintanongentillion         │ 2958 │
                │ sexoctogintanongentillion             │ 2961 │
                │ septemoctogintanongentillion          │ 2964 │
                │ octooctogintanongentillion            │ 2967 │
                │ novemoctogintanongentillion           │ 2970 │
                │ nonagintanongetillion                 │ 2973 │
                │ unnonagintanongentillion              │ 2976 │
                │ duononagintanongentillion             │ 2979 │
                │ trenonagintanongentillion             │ 2982 │
                │ quattuornonagintanongentillion        │ 2985 │
                │ quinquanonagintanongentillion         │ 2988 │
                │ senonagintanongentillion              │ 2991 │
                │ septenonagintanongentillion           │ 2994 │
                │ novenonagintanongentillion            │ 2997 │
                │ octononagintanongentillion            │ 3000 │
                ├───────────────────────────────────────┴──────┤
                │ nonillion    is also named  noventillion     │
                │ noven*      are also named  novem*           │
                │ septen*     are also named  septem*          │
                │ ses*        are also named  sex*             │
                │ tre*        are also named  tres*            │
                └──────────────────────────────────────────────┘



                               ┌──────────────────────────────────────────────┐
                               │      names of some                           │
                               │ "British" style numbers                10**n │
                               ├───────────────────────────────────────┬──────┤
                               │ one                               (1) │    0 │
                               │ ten                              (10) │    1 │
                               │ hundred                         (100) │    2 │
                               │ thousand                      (1,000) │    3 │
                               │ million                   (1,000,000) │    6 │
                               │ millard               (1,000.000.000) │    9 │
                               │ billion           (1,000,000,000,000) │   12 │
                               │ billiard                              │   15 │
                               │ trillion                              │   18 │
                               │ trilliard                             │   21 │
                               │ quadrillion                           │   24 │
                               │ quadrilliard                          │   27 │
                               │ quintillion                           │   30 │
                               │ quintilliard                          │   33 │
                               │ sextillion                            │   36 │
                               │ sextilliard                           │   39 │
                               │ septillion                            │   42 │
                               │ septilliard                           │   45 │
                               │ octillion                             │   48 │
                               │ octilliard                            │   51 │
                               │ nonillion                             │   54 │
                               │ nonilliard                            │   57 │
                               │ decillion                             │   60 │
                               │ decilliard                            │   63 │
                               │ undecillion                           │   66 │
                               │ undecilliard                          │   69 │
                               │ duodecillion                          │   72 │
                               │ duodecilliard                         │   75 │
                               │ tredecillion                          │   78 │
                               │ tredecilliard                         │   81 │
                               │ quattuordecillion                     │   84 │
                               │ quattuordecilliard                    │   87 │
                               │ quinquadecillion                      │   90 │
                               │ quinquadecilliard                     │   93 │
                               │ sedecillion                           │   96 │
                               │ sedecilliard                          │   99 │
                               │ septendecillion                       │  102 │
                               │ septendecilliard                      │  105 │
                               │ octodecillion                         │  108 │
                               │ octodecilliard                        │  111 │
                               │ novendecillion                        │  114 │
                               │ novendecilliard                       │  117 │
                               │ vigintillion                          │  120 │
                               │ vigintilliard                         │  123 │
                               │ unvigintillion                        │  126 │
                               │ unvigintilliard                       │  129 │
                               │ duovigintillion                       │  132 │
                               │ duovigintilliard                      │  135 │
                               │ tresvigintillion                      │  138 │
                               │ tresvigintilliard                     │  141 │
                               │ quattuorvigintillion                  │  144 │
                               │ quattuorvigintilliard                 │  147 │
                               │ quinquavigintillion                   │  150 │
                               │ quinquavigintilliard                  │  153 │
                               │ sesvigintillion                       │  156 │
                               │ sesvigintilliard                      │  159 │
                               │ septemvigintillion                    │  162 │
                               │ septemvigintilliard                   │  165 │
                               │ octovigintillion                      │  168 │
                               │ octovigintilliard                     │  171 │
                               │ novemvigintillion                     │  174 │
                               │ novemvigintilliard                    │  177 │
                               │ trigintillion                         │  180 │
                               │ trigintilliard                        │  183 │
                               │ untrigintillion                       │  186 │
                               │ untrigintilliard                      │  189 │
                               │ duotrigintillion                      │  192 │
                               │ duotrigintilliard                     │  195 │
                               │ trestrigintillion                     │  198 │
                               │ trestrigintilliard                    │  201 │
                               │ quattuortrigintillion                 │  204 │
                               │ quattuortrigintilliard                │  207 │
                               │ quinquatrigintillion                  │  210 │
                               │ quinquatrigintilliard                 │  213 │
                               │ sestrigintillion                      │  216 │
                               │ sestrigintilliard                     │  219 │
                               │ septentrigintillion                   │  222 │
                               │ septentrigintilliard                  │  225 │
                               │ octotrigintillion                     │  228 │
                               │ octotrigintilliard                    │  231 │
                               │ noventrigintillion                    │  234 │
                               │ noventrigintilliard                   │  237 │
                               │ quadragintillion                      │  240 │
                               │ quadragintilliard                     │  243 │
                               │ unquadragintillion                    │  246 │
                               │ unquadragintilliard                   │  249 │
                               │ duoquadragintillion                   │  252 │
                               │ duoquadragintilliard                  │  255 │
                               │ tresquadragintillion                  │  258 │
                               │ tresquadragintilliard                 │  261 │
                               │ quattuorquadragintillion              │  264 │
                               │ quattuorquadragintilliard             │  267 │
                               │ quinquaquadragintillion               │  270 │
                               │ quinquaquadragintilliard              │  273 │
                               │ sesquadragintillion                   │  276 │
                               │ sesquadragintilliard                  │  279 │
                               │ septenquadragintillion                │  282 │
                               │ septenquadragintilliard               │  285 │
                               │ octoquadragintillion                  │  288 │
                               │ octoquadragintilliard                 │  291 │
                               │ novenquadragintillion                 │  294 │
                               │ novenquadragintilliard                │  297 │
                               │ quinquagintillion                     │  300 │
                               │ quinquagintilliard                    │  303 │
                               │ unquinquagintillion                   │  306 │
                               │ unquinquagintilliard                  │  309 │
                               │ duoquinquagintillion                  │  312 │
                               │ duoquinquagintilliard                 │  315 │
                               │ tresquinquagintillion                 │  318 │
                               │ tresquinquagintilliard                │  321 │
                               │ quattuorquinquagintillion             │  324 │
                               │ quattuorquinquagintilliard            │  327 │
                               │ quinquaquinquagintillion              │  330 │
                               │ quinquaquinquagintilliard             │  333 │
                               │ sesquinquagintillion                  │  336 │
                               │ sesquinquagintilliard                 │  339 │
                               │ septenquinquagintillion               │  342 │
                               │ septenquinquagintilliard              │  345 │
                               │ octoquinquagintillion                 │  348 │
                               │ octoquinquagintilliard                │  351 │
                               │ novenquinquagintillion                │  354 │
                               │ novenquinquagintilliard               │  357 │
                               │ sexagintillion                        │  360 │
                               │ sexagintilliard                       │  363 │
                               │ unsexagintillion                      │  366 │
                               │ unsexagintilliard                     │  369 │
                               │ duosexagintillion                     │  372 │
                               │ duosexagintilliard                    │  375 │
                               │ tresexagintillion                     │  378 │
                               │ tresexagintilliard                    │  381 │
                               │ quattuorsexagintillion                │  384 │
                               │ quattuorsexagintilliard               │  387 │
                               │ quinquasexagintillion                 │  390 │
                               │ quinquasexagintilliard                │  393 │
                               │ sesexagintillion                      │  396 │
                               │ sesexagintilliard                     │  399 │
                               │ septensexagintillion                  │  402 │
                               │ septensexagintilliard                 │  405 │
                               │ octosexagintillion                    │  408 │
                               │ octosexagintilliard                   │  411 │
                               │ novensexagintillion                   │  414 │
                               │ novensexagintilliard                  │  417 │
                               │ septuagintillion                      │  420 │
                               │ septuagintilliard                     │  423 │
                               │ unseptuagintillion                    │  426 │
                               │ unseptuagintilliard                   │  429 │
                               │ duoseptuagintillion                   │  432 │
                               │ duoseptuagintilliard                  │  435 │
                               │ treseptuagintillion                   │  438 │
                               │ treseptuagintilliard                  │  441 │
                               │ quattuorseptuagintillion              │  444 │
                               │ quattuorseptuagintilliard             │  447 │
                               │ quinquaseptuagintillion               │  450 │
                               │ quinquaseptuagintilliard              │  453 │
                               │ seseptuagintillion                    │  456 │
                               │ seseptuagintilliard                   │  459 │
                               │ septenseptuagintillion                │  462 │
                               │ septenseptuagintilliard               │  465 │
                               │ octoseptuagintillion                  │  468 │
                               │ octoseptuagintilliard                 │  471 │
                               │ novenseptuagintillion                 │  474 │
                               │ novenseptuagintilliard                │  477 │
                               │ octogintillion                        │  480 │
                               │ octogintilliard                       │  483 │
                               │ unoctogintillion                      │  486 │
                               │ unoctogintilliard                     │  489 │
                               │ duooctogintillion                     │  492 │
                               │ duooctogintilliard                    │  495 │
                               │ tresoctogintillion                    │  498 │
                               │ tresoctogintilliard                   │  501 │
                               │ quattuoroctogintillion                │  504 │
                               │ quattuoroctogintilliard               │  507 │
                               │ quinquaoctogintillion                 │  510 │
                               │ quinquaoctogintilliard                │  513 │
                               │ sexoctogintillion                     │  516 │
                               │ sexoctogintilliard                    │  519 │
                               │ septemoctogintillion                  │  522 │
                               │ septemoctogintilliard                 │  525 │
                               │ octooctogintillion                    │  528 │
                               │ octooctogintilliard                   │  531 │
                               │ novemoctogintillion                   │  534 │
                               │ novemoctogintilliard                  │  537 │
                               │ nonagintillion                        │  540 │
                               │ nonagintilliard                       │  543 │
                               │ unnonagintillion                      │  546 │
                               │ unnonagintilliard                     │  549 │
                               │ duononagintillion                     │  552 │
                               │ duononagintilliard                    │  555 │
                               │ trenonagintillion                     │  558 │
                               │ trenonagintilliard                    │  561 │
                               │ quattuornonagintillion                │  564 │
                               │ quattuornonagintilliard               │  567 │
                               │ quinquanonagintillion                 │  570 │
                               │ quinquanonagintilliard                │  573 │
                               │ senonagintillion                      │  576 │
                               │ senonagintilliard                     │  579 │
                               │ septenonagintillion                   │  582 │
                               │ septenonagintilliard                  │  585 │
                               │ octononagintillion                    │  588 │
                               │ octononagintilliard                   │  591 │
                               │ novenonagintillion                    │  594 │
                               │ novenonagintilliard                   │  597 │
                               │ centillion                            │  600 │
                               │ centilliard                           │  603 │
                               │ uncentillion                          │  606 │
                               │ uncentilliard                         │  609 │
                               │ duocentillion                         │  612 │
                               │ duocentilliard                        │  615 │
                               │ trescentillion                        │  618 │
                               │ trescentilliard                       │  621 │
                               │ quattuorcentillion                    │  624 │
                               │ quattuorcentilliard                   │  627 │
                               │ quinquacentillion                     │  630 │
                               │ quinquacentilliard                    │  633 │
                               │ sexcentillion                         │  636 │
                               │ sexcentilliard                        │  639 │
                               │ septencentillion                      │  642 │
                               │ septencentilliard                     │  645 │
                               │ octocentillion                        │  648 │
                               │ octocentilliard                       │  651 │
                               │ novencentillion                       │  654 │
                               │ novencentilliard                      │  657 │
                               │ decicentillion                        │  660 │
                               │ decicentilliard                       │  663 │
                               │ undecicentillion                      │  666 │
                               │ undecicentilliard                     │  669 │
                               │ duodecicentillion                     │  672 │
                               │ duodecicentilliard                    │  675 │
                               │ tredecicentillion                     │  678 │
                               │ tredecicentilliard                    │  681 │
                               │ quattuordecicentillion                │  684 │
                               │ quattuordecicentilliard               │  687 │
                               │ quinquadecicentillion                 │  690 │
                               │ quinquadecicentilliard                │  693 │
                               │ sedecicentillion                      │  696 │
                               │ sedecicentilliard                     │  699 │
                               │ septendecicentillion                  │  702 │
                               │ septendecicentilliard                 │  705 │
                               │ octodecicentillion                    │  708 │
                               │ octodecicentilliard                   │  711 │
                               │ novendecicentillion                   │  714 │
                               │ novendecicentilliard                  │  717 │
                               │ viginticentillion                     │  720 │
                               │ viginticentilliard                    │  723 │
                               │ unviginticentillion                   │  726 │
                               │ unviginticentilliard                  │  729 │
                               │ duoviginticentillion                  │  732 │
                               │ duoviginticentilliard                 │  735 │
                               │ tresviginticentillion                 │  738 │
                               │ tresviginticentilliard                │  741 │
                               │ quattuorviginticentillion             │  744 │
                               │ quattuorviginticentilliard            │  747 │
                               │ quinquaviginticentillion              │  750 │
                               │ quinquaviginticentilliard             │  753 │
                               │ sesviginticentillion                  │  756 │
                               │ sesviginticentilliard                 │  759 │
                               │ septemviginticentillion               │  762 │
                               │ septemviginticentilliard              │  765 │
                               │ octoviginticentillion                 │  768 │
                               │ octoviginticentilliard                │  771 │
                               │ novemviginticentillion                │  774 │
                               │ novemviginticentilliard               │  777 │
                               │ trigintacentillion                    │  780 │
                               │ trigintacentilliard                   │  783 │
                               │ untrigintacentillion                  │  786 │
                               │ untrigintacentilliard                 │  789 │
                               │ duotrigintacentillion                 │  792 │
                               │ duotrigintacentilliard                │  795 │
                               │ trestrigintacentillion                │  798 │
                               │ trestrigintacentilliard               │  801 │
                               │ quattuortrigintacentillion            │  804 │
                               │ quattuortrigintacentilliard           │  807 │
                               │ quinquatrigintacentillion             │  810 │
                               │ quinquatrigintacentilliard            │  813 │
                               │ sestrigintacentillion                 │  816 │
                               │ sestrigintacentilliard                │  819 │
                               │ septentrigintacentillion              │  822 │
                               │ septentrigintacentilliard             │  825 │
                               │ octotrigintacentillion                │  828 │
                               │ octotrigintacentilliard               │  831 │
                               │ noventrigintacentillion               │  834 │
                               │ noventrigintacentilliard              │  837 │
                               │ quadragintacentillion                 │  840 │
                               │ quadragintacentilliard                │  843 │
                               │ unquadragintacentillion               │  846 │
                               │ unquadragintacentilliard              │  849 │
                               │ duoquadragintacentillion              │  852 │
                               │ duoquadragintacentilliard             │  855 │
                               │ tresquadragintacentillion             │  858 │
                               │ tresquadragintacentilliard            │  861 │
                               │ quattuorquadragintacentillion         │  864 │
                               │ quattuorquadragintacentilliard        │  867 │
                               │ quinquaquadragintacentillion          │  870 │
                               │ quinquaquadragintacentilliard         │  873 │
                               │ sesquadragintacentillion              │  876 │
                               │ sesquadragintacentilliard             │  879 │
                               │ septenquadragintacentillion           │  882 │
                               │ septenquadragintacentilliard          │  885 │
                               │ octoquadragintacentillion             │  888 │
                               │ octoquadragintacentilliard            │  891 │
                               │ novenquadragintacentillion            │  894 │
                               │ novenquadragintacentilliard           │  897 │
                               │ quinquagintacentillion                │  900 │
                               │ quinquagintacentilliard               │  903 │
                               │ unquinquagintacentillion              │  906 │
                               │ unquinquagintacentilliard             │  909 │
                               │ duoquinquagintacentillion             │  912 │
                               │ duoquinquagintacentilliard            │  915 │
                               │ tresquinquagintacentillion            │  918 │
                               │ tresquinquagintacentilliard           │  921 │
                               │ quattuorquinquagintacentillion        │  924 │
                               │ quattuorquinquagintacentilliard       │  927 │
                               │ quinquaquinquagintacentillion         │  930 │
                               │ quinquaquinquagintacentilliard        │  933 │
                               │ sesquinquagintacentillion             │  936 │
                               │ sesquinquagintacentilliard            │  939 │
                               │ septenquinquagintacentillion          │  942 │
                               │ septenquinquagintacentilliard         │  945 │
                               │ octoquinquagintacentillion            │  948 │
                               │ octoquinquagintacentilliard           │  951 │
                               │ novenquinquagintacentillion           │  954 │
                               │ novenquinquagintacentilliard          │  957 │
                               │ sexagintacentillion                   │  960 │
                               │ sexagintacentilliard                  │  963 │
                               │ unsexagintacentillion                 │  966 │
                               │ unsexagintacentilliard                │  969 │
                               │ duosexagintacentillion                │  972 │
                               │ duosexagintacentilliard               │  975 │
                               │ tresexagintacentillion                │  978 │
                               │ tresexagintacentilliard               │  981 │
                               │ quattuorsexagintacentillion           │  984 │
                               │ quattuorsexagintacentilliard          │  987 │
                               │ quinquasexagintacentillion            │  990 │
                               │ quinquasexagintacentilliard           │  993 │
                               │ sesexagintacentillion                 │  996 │
                               │ sesexagintacentilliard                │  999 │
                               │ septensexagintacentillion             │ 1002 │
                               │ septensexagintacentilliard            │ 1005 │
                               │ octosexagintacentillion               │ 1008 │
                               │ octosexagintacentilliard              │ 1011 │
                               │ novensexagintacentillion              │ 1014 │
                               │ novensexagintacentilliard             │ 1017 │
                               │ septuagintacentillion                 │ 1020 │
                               │ septuagintacentilliard                │ 1023 │
                               │ unseptuagintacentillion               │ 1026 │
                               │ unseptuagintacentilliard              │ 1029 │
                               │ duoseptuagintacentillion              │ 1032 │
                               │ duoseptuagintacentilliard             │ 1035 │
                               │ treseptuagintacentillion              │ 1038 │
                               │ treseptuagintacentilliard             │ 1041 │
                               │ quattuorseptuagintacentillion         │ 1044 │
                               │ quattuorseptuagintacentilliard        │ 1047 │
                               │ quinquaseptuagintacentillion          │ 1050 │
                               │ quinquaseptuagintacentilliard         │ 1053 │
                               │ seseptuagintacentillion               │ 1056 │
                               │ seseptuagintacentilliard              │ 1059 │
                               │ septenseptuagintacentillion           │ 1062 │
                               │ septenseptuagintacentilliard          │ 1065 │
                               │ octoseptuagintacentillion             │ 1068 │
                               │ octoseptuagintacentilliard            │ 1071 │
                               │ novenseptuagintacentillion            │ 1074 │
                               │ novenseptuagintacentilliard           │ 1077 │
                               │ octogintacentillion                   │ 1080 │
                               │ octogintacentilliard                  │ 1083 │
                               │ unoctogintacentillion                 │ 1086 │
                               │ unoctogintacentilliard                │ 1089 │
                               │ duooctogintacentillion                │ 1092 │
                               │ duooctogintacentilliard               │ 1095 │
                               │ tresoctogintacentillion               │ 1098 │
                               │ tresoctogintacentilliard              │ 1101 │
                               │ quattuoroctogintacentillion           │ 1104 │
                               │ quattuoroctogintacentilliard          │ 1107 │
                               │ quinquaoctogintacentillion            │ 1110 │
                               │ quinquaoctogintacentilliard           │ 1113 │
                               │ sexoctogintacentillion                │ 1116 │
                               │ sexoctogintacentilliard               │ 1119 │
                               │ septemoctogintacentillion             │ 1122 │
                               │ septemoctogintacentilliard            │ 1125 │
                               │ octooctogintacentillion               │ 1128 │
                               │ octooctogintacentilliard              │ 1131 │
                               │ novemoctogintacentillion              │ 1134 │
                               │ novemoctogintacentilliard             │ 1137 │
                               │ nonagintacentillion                   │ 1140 │
                               │ nonagintacentilliard                  │ 1143 │
                               │ unnonagintacentillion                 │ 1146 │
                               │ unnonagintacentilliard                │ 1149 │
                               │ duononagintacentillion                │ 1152 │
                               │ duononagintacentilliard               │ 1155 │
                               │ trenonagintacentillion                │ 1158 │
                               │ trenonagintacentilliard               │ 1161 │
                               │ quattuornonagintacentillion           │ 1164 │
                               │ quattuornonagintacentilliard          │ 1167 │
                               │ quinquanonagintacentillion            │ 1170 │
                               │ quinquanonagintacentilliard           │ 1173 │
                               │ senonagintacentillion                 │ 1176 │
                               │ senonagintacentilliard                │ 1179 │
                               │ septenonagintacentillion              │ 1182 │
                               │ septenonagintacentilliard             │ 1185 │
                               │ octononagintacentillion               │ 1188 │
                               │ octononagintacentilliard              │ 1191 │
                               │ novenonagintacentillion               │ 1194 │
                               │ novenonagintacentilliard              │ 1197 │
                               │ ducentillion                          │ 1200 │
                               │ ducentilliard                         │ 1203 │
                               │ unducentillion                        │ 1206 │
                               │ unducentilliard                       │ 1209 │
                               │ duoducentillion                       │ 1212 │
                               │ duoducentilliard                      │ 1215 │
                               │ treducentillion                       │ 1218 │
                               │ treducentilliard                      │ 1221 │
                               │ quattuorducentillion                  │ 1224 │
                               │ quattuorducentilliard                 │ 1227 │
                               │ quinquaducentillion                   │ 1230 │
                               │ quinquaducentilliard                  │ 1233 │
                               │ seducentillion                        │ 1236 │
                               │ seducentilliard                       │ 1239 │
                               │ septenducentillion                    │ 1242 │
                               │ septenducentilliard                   │ 1245 │
                               │ octoducentillion                      │ 1248 │
                               │ octoducentilliard                     │ 1251 │
                               │ novenducentillion                     │ 1254 │
                               │ novenducentilliard                    │ 1257 │
                               │ deciducentillion                      │ 1260 │
                               │ deciducentilliard                     │ 1263 │
                               │ undeciducentillion                    │ 1266 │
                               │ undeciducentilliard                   │ 1269 │
                               │ duodeciducentillion                   │ 1272 │
                               │ duodeciducentilliard                  │ 1275 │
                               │ tredeciducentillion                   │ 1278 │
                               │ tredeciducentilliard                  │ 1281 │
                               │ quattuordeciducentillion              │ 1284 │
                               │ quattuordeciducentilliard             │ 1287 │
                               │ quinquadeciducentillion               │ 1290 │
                               │ quinquadeciducentilliard              │ 1293 │
                               │ sedeciducentillion                    │ 1296 │
                               │ sedeciducentilliard                   │ 1299 │
                               │ septendeciducentillion                │ 1302 │
                               │ septendeciducentilliard               │ 1305 │
                               │ octodeciducentillion                  │ 1308 │
                               │ octodeciducentilliard                 │ 1311 │
                               │ novendeciducentillion                 │ 1314 │
                               │ novendeciducentilliard                │ 1317 │
                               │ vigintiducentillion                   │ 1320 │
                               │ vigintiducentilliard                  │ 1323 │
                               │ unvigintiducentillion                 │ 1326 │
                               │ unvigintiducentilliard                │ 1329 │
                               │ duovigintiducentillion                │ 1332 │
                               │ duovigintiducentilliard               │ 1335 │
                               │ tresvigintiducentillion               │ 1338 │
                               │ tresvigintiducentilliard              │ 1341 │
                               │ quattuorvigintiducentillion           │ 1344 │
                               │ quattuorvigintiducentilliard          │ 1347 │
                               │ quinquavigintiducentillion            │ 1350 │
                               │ quinquavigintiducentilliard           │ 1353 │
                               │ sesvigintiducentillion                │ 1356 │
                               │ sesvigintiducentilliard               │ 1359 │
                               │ septemvigintiducentillion             │ 1362 │
                               │ septemvigintiducentilliard            │ 1365 │
                               │ octovigintiducentillion               │ 1368 │
                               │ octovigintiducentilliard              │ 1371 │
                               │ novemvigintiducentillion              │ 1374 │
                               │ novemvigintiducentilliard             │ 1377 │
                               │ trigintaducentillion                  │ 1380 │
                               │ trigintaducentilliard                 │ 1383 │
                               │ untrigintaducentillion                │ 1386 │
                               │ untrigintaducentilliard               │ 1389 │
                               │ duotrigintaducentillion               │ 1392 │
                               │ duotrigintaducentilliard              │ 1395 │
                               │ trestrigintaducentillion              │ 1398 │
                               │ trestrigintaducentilliard             │ 1401 │
                               │ quattuortrigintaducentillion          │ 1404 │
                               │ quattuortrigintaducentilliard         │ 1407 │
                               │ quinquatrigintaducentillion           │ 1410 │
                               │ quinquatrigintaducentilliard          │ 1413 │
                               │ sestrigintaducentillion               │ 1416 │
                               │ sestrigintaducentilliard              │ 1419 │
                               │ septentrigintaducentillion            │ 1422 │
                               │ septentrigintaducentilliard           │ 1425 │
                               │ octotrigintaducentillion              │ 1428 │
                               │ octotrigintaducentilliard             │ 1431 │
                               │ noventrigintaducentillion             │ 1434 │
                               │ noventrigintaducentilliard            │ 1437 │
                               │ quadragintaducentillion               │ 1440 │
                               │ quadragintaducentilliard              │ 1443 │
                               │ unquadragintaducentillion             │ 1446 │
                               │ unquadragintaducentilliard            │ 1449 │
                               │ duoquadragintaducentillion            │ 1452 │
                               │ duoquadragintaducentilliard           │ 1455 │
                               │ tresquadragintaducentillion           │ 1458 │
                               │ tresquadragintaducentilliard          │ 1461 │
                               │ quattuorquadragintaducentillion       │ 1464 │
                               │ quattuorquadragintaducentilliard      │ 1467 │
                               │ quinquaquadragintaducentillion        │ 1470 │
                               │ quinquaquadragintaducentilliard       │ 1473 │
                               │ sesquadragintaducentillion            │ 1476 │
                               │ sesquadragintaducentilliard           │ 1479 │
                               │ septenquadragintaducentillion         │ 1482 │
                               │ septenquadragintaducentilliard        │ 1485 │
                               │ octoquadragintaducentillion           │ 1488 │
                               │ octoquadragintaducentilliard          │ 1491 │
                               │ novenquadragintaducentillion          │ 1494 │
                               │ novenquadragintaducentilliard         │ 1497 │
                               │ quinquagintaducentillion              │ 1500 │
                               │ quinquagintaducentilliard             │ 1503 │
                               │ unquinquagintaducentillion            │ 1506 │
                               │ unquinquagintaducentilliard           │ 1509 │
                               │ duoquinquagintaducentillion           │ 1512 │
                               │ duoquinquagintaducentilliard          │ 1515 │
                               │ tresquinquagintaducentillion          │ 1518 │
                               │ tresquinquagintaducentilliard         │ 1521 │
                               │ quattuorquinquagintaducentillion      │ 1524 │
                               │ quattuorquinquagintaducentilliard     │ 1527 │
                               │ quinquaquinquagintaducentillion       │ 1530 │
                               │ quinquaquinquagintaducentilliard      │ 1533 │
                               │ sesquinquagintaducentillion           │ 1536 │
                               │ sesquinquagintaducentilliard          │ 1539 │
                               │ septenquinquagintaducentillion        │ 1542 │
                               │ septenquinquagintaducentilliard       │ 1545 │
                               │ octoquinquagintaducentillion          │ 1548 │
                               │ octoquinquagintaducentilliard         │ 1551 │
                               │ novenquinquagintaducentillion         │ 1554 │
                               │ novenquinquagintaducentilliard        │ 1557 │
                               │ sexagintaducentillion                 │ 1560 │
                               │ sexagintaducentilliard                │ 1563 │
                               │ unsexagintaducentillion               │ 1566 │
                               │ unsexagintaducentilliard              │ 1569 │
                               │ duosexagintaducentillion              │ 1572 │
                               │ duosexagintaducentilliard             │ 1575 │
                               │ tresexagintaducentillion              │ 1578 │
                               │ tresexagintaducentilliard             │ 1581 │
                               │ quattuorsexagintaducentillion         │ 1584 │
                               │ quattuorsexagintaducentilliard        │ 1587 │
                               │ quinquasexagintaducentillion          │ 1590 │
                               │ quinquasexagintaducentilliard         │ 1593 │
                               │ sesexagintaducentillion               │ 1596 │
                               │ sesexagintaducentilliard              │ 1599 │
                               │ septensexagintaducentillion           │ 1602 │
                               │ septensexagintaducentilliard          │ 1605 │
                               │ octosexagintaducentillion             │ 1608 │
                               │ octosexagintaducentilliard            │ 1611 │
                               │ novensexagintaducentillion            │ 1614 │
                               │ novensexagintaducentilliard           │ 1617 │
                               │ septuagintaducentillion               │ 1620 │
                               │ septuagintaducentilliard              │ 1623 │
                               │ unseptuagintaducentillion             │ 1626 │
                               │ unseptuagintaducentilliard            │ 1629 │
                               │ duoseptuagintaducentillion            │ 1632 │
                               │ duoseptuagintaducentilliard           │ 1635 │
                               │ treseptuagintaducentillion            │ 1638 │
                               │ treseptuagintaducentilliard           │ 1641 │
                               │ quattuorseptuagintaducentillion       │ 1644 │
                               │ quattuorseptuagintaducentilliard      │ 1647 │
                               │ quinquaseptuagintaducentillion        │ 1650 │
                               │ quinquaseptuagintaducentilliard       │ 1653 │
                               │ seseptuagintaducentillion             │ 1656 │
                               │ seseptuagintaducentilliard            │ 1659 │
                               │ septenseptuagintaducentillion         │ 1662 │
                               │ septenseptuagintaducentilliard        │ 1665 │
                               │ octoseptuagintaducentillion           │ 1668 │
                               │ octoseptuagintaducentilliard          │ 1671 │
                               │ novenseptuagintaducentillion          │ 1674 │
                               │ novenseptuagintaducentilliard         │ 1677 │
                               │ octogintaducentillion                 │ 1680 │
                               │ octogintaducentilliard                │ 1683 │
                               │ unoctogintaducentillion               │ 1686 │
                               │ unoctogintaducentilliard              │ 1689 │
                               │ duooctogintaducentillion              │ 1692 │
                               │ duooctogintaducentilliard             │ 1695 │
                               │ tresoctogintaducentillion             │ 1698 │
                               │ tresoctogintaducentilliard            │ 1701 │
                               │ quattuoroctogintaducentillion         │ 1704 │
                               │ quattuoroctogintaducentilliard        │ 1707 │
                               │ quinquaoctogintaducentillion          │ 1710 │
                               │ quinquaoctogintaducentilliard         │ 1713 │
                               │ sexoctogintaducentillion              │ 1716 │
                               │ sexoctogintaducentilliard             │ 1719 │
                               │ septemoctogintaducentillion           │ 1722 │
                               │ septemoctogintaducentilliard          │ 1725 │
                               │ octooctogintaducentillion             │ 1728 │
                               │ octooctogintaducentilliard            │ 1731 │
                               │ novemoctogintaducentillion            │ 1734 │
                               │ novemoctogintaducentilliard           │ 1737 │
                               │ nonagintaducentillion                 │ 1740 │
                               │ nonagintaducentilliard                │ 1743 │
                               │ unnonagintaducentillion               │ 1746 │
                               │ unnonagintaducentilliard              │ 1749 │
                               │ duononagintaducentillion              │ 1752 │
                               │ duononagintaducentilliard             │ 1755 │
                               │ trenonagintaducentillion              │ 1758 │
                               │ trenonagintaducentilliard             │ 1761 │
                               │ quattuornonagintaducentillion         │ 1764 │
                               │ quattuornonagintaducentilliard        │ 1767 │
                               │ quinquanonagintaducentillion          │ 1770 │
                               │ quinquanonagintaducentilliard         │ 1773 │
                               │ senonagintaducentillion               │ 1776 │
                               │ senonagintaducentilliard              │ 1779 │
                               │ septenonagintaducentillion            │ 1782 │
                               │ septenonagintaducentilliard           │ 1785 │
                               │ octononagintaducentillion             │ 1788 │
                               │ octononagintaducentilliard            │ 1791 │
                               │ novenonagintaducentillion             │ 1794 │
                               │ novenonagintaducentilliard            │ 1797 │
                               │ trecentillion                         │ 1800 │
                               │ trecentilliard                        │ 1803 │
                               │ untrecentillion                       │ 1806 │
                               │ untrecentilliard                      │ 1809 │
                               │ duotrecentillion                      │ 1812 │
                               │ duotrecentilliard                     │ 1815 │
                               │ trestrecentillion                     │ 1818 │
                               │ trestrecentilliard                    │ 1821 │
                               │ quattuortrecentillion                 │ 1824 │
                               │ quattuortrecentilliard                │ 1827 │
                               │ quinquatrecentillion                  │ 1830 │
                               │ quinquatrecentilliard                 │ 1833 │
                               │ sestrecentillion                      │ 1836 │
                               │ sestrecentilliard                     │ 1839 │
                               │ septentrecentillion                   │ 1842 │
                               │ septentrecentilliard                  │ 1845 │
                               │ octotrecentillion                     │ 1848 │
                               │ octotrecentilliard                    │ 1851 │
                               │ noventrecentillion                    │ 1854 │
                               │ noventrecentilliard                   │ 1857 │
                               │ decitrecentillion                     │ 1860 │
                               │ decitrecentilliard                    │ 1863 │
                               │ undecitrecentillion                   │ 1866 │
                               │ undecitrecentilliard                  │ 1869 │
                               │ duodecitrecentillion                  │ 1872 │
                               │ duodecitrecentilliard                 │ 1875 │
                               │ tredecitrecentillion                  │ 1878 │
                               │ tredecitrecentilliard                 │ 1881 │
                               │ quattuordecitrecentillion             │ 1884 │
                               │ quattuordecitrecentilliard            │ 1887 │
                               │ quinquadecitrecentillion              │ 1890 │
                               │ quinquadecitrecentilliard             │ 1893 │
                               │ sedecitrecentillion                   │ 1896 │
                               │ sedecitrecentilliard                  │ 1899 │
                               │ septendecitrecentillion               │ 1902 │
                               │ septendecitrecentilliard              │ 1905 │
                               │ octodecitrecentillion                 │ 1908 │
                               │ octodecitrecentilliard                │ 1911 │
                               │ novendecitrecentillion                │ 1914 │
                               │ novendecitrecentilliard               │ 1917 │
                               │ vigintitrecentillion                  │ 1920 │
                               │ vigintitrecentilliard                 │ 1923 │
                               │ unvigintitrecentillion                │ 1926 │
                               │ unvigintitrecentilliard               │ 1929 │
                               │ duovigintitrecentillion               │ 1932 │
                               │ duovigintitrecentilliard              │ 1935 │
                               │ tresvigintitrecentillion              │ 1938 │
                               │ tresvigintitrecentilliard             │ 1941 │
                               │ quattuorvigintitrecentillion          │ 1944 │
                               │ quattuorvigintitrecentilliard         │ 1947 │
                               │ quinquavigintitrecentillion           │ 1950 │
                               │ quinquavigintitrecentilliard          │ 1953 │
                               │ sesvigintitrecentillion               │ 1956 │
                               │ sesvigintitrecentilliard              │ 1959 │
                               │ septemvigintitrecentillion            │ 1962 │
                               │ septemvigintitrecentilliard           │ 1965 │
                               │ octovigintitrecentillion              │ 1968 │
                               │ octovigintitrecentilliard             │ 1971 │
                               │ novemvigintitrecentillion             │ 1974 │
                               │ novemvigintitrecentilliard            │ 1977 │
                               │ trigintatrecentillion                 │ 1980 │
                               │ trigintatrecentilliard                │ 1983 │
                               │ untrigintatrecentillion               │ 1986 │
                               │ untrigintatrecentilliard              │ 1989 │
                               │ duotrigintatrecentillion              │ 1992 │
                               │ duotrigintatrecentilliard             │ 1995 │
                               │ trestrigintatrecentillion             │ 1998 │
                               │ trestrigintatrecentilliard            │ 2001 │
                               │ quattuortrigintatrecentillion         │ 2004 │
                               │ quattuortrigintatrecentilliard        │ 2007 │
                               │ quinquatrigintatrecentillion          │ 2010 │
                               │ quinquatrigintatrecentilliard         │ 2013 │
                               │ sestrigintatrecentillion              │ 2016 │
                               │ sestrigintatrecentilliard             │ 2019 │
                               │ septentrigintatrecentillion           │ 2022 │
                               │ septentrigintatrecentilliard          │ 2025 │
                               │ octotrigintatrecentillion             │ 2028 │
                               │ octotrigintatrecentilliard            │ 2031 │
                               │ noventrigintatrecentillion            │ 2034 │
                               │ noventrigintatrecentilliard           │ 2037 │
                               │ quadragintatrecentillion              │ 2040 │
                               │ quadragintatrecentilliard             │ 2043 │
                               │ unquadragintatrecentillion            │ 2046 │
                               │ unquadragintatrecentilliard           │ 2049 │
                               │ duoquadragintatrecentillion           │ 2052 │
                               │ duoquadragintatrecentilliard          │ 2055 │
                               │ tresquadragintatrecentillion          │ 2058 │
                               │ tresquadragintatrecentilliard         │ 2061 │
                               │ quattuorquadragintatrecentillion      │ 2064 │
                               │ quattuorquadragintatrecentilliard     │ 2067 │
                               │ quinquaquadragintatrecentillion       │ 2070 │
                               │ quinquaquadragintatrecentilliard      │ 2073 │
                               │ sesquadragintatrecentillion           │ 2076 │
                               │ sesquadragintatrecentilliard          │ 2079 │
                               │ septenquadragintatrecentillion        │ 2082 │
                               │ septenquadragintatrecentilliard       │ 2085 │
                               │ octoquadragintatrecentillion          │ 2088 │
                               │ octoquadragintatrecentilliard         │ 2091 │
                               │ novenquadragintatrecentillion         │ 2094 │
                               │ novenquadragintatrecentilliard        │ 2097 │
                               │ quinquagintatrecentillion             │ 2100 │
                               │ quinquagintatrecentilliard            │ 2103 │
                               │ unquinquagintatrecentillion           │ 2106 │
                               │ unquinquagintatrecentilliard          │ 2109 │
                               │ duoquinquagintatrecentillion          │ 2112 │
                               │ duoquinquagintatrecentilliard         │ 2115 │
                               │ tresquinquagintatrecentillion         │ 2118 │
                               │ tresquinquagintatrecentilliard        │ 2121 │
                               │ quattuorquinquagintatrecentillion     │ 2124 │
                               │ quattuorquinquagintatrecentilliard    │ 2127 │
                               │ quinquaquinquagintatrecentillion      │ 2130 │
                               │ quinquaquinquagintatrecentilliard     │ 2133 │
                               │ sesquinquagintatrecentillion          │ 2136 │
                               │ sesquinquagintatrecentilliard         │ 2139 │
                               │ septenquinquagintatrecentillion       │ 2142 │
                               │ septenquinquagintatrecentilliard      │ 2145 │
                               │ octoquinquagintatrecentillion         │ 2148 │
                               │ octoquinquagintatrecentilliard        │ 2151 │
                               │ novenquinquagintatrecentillion        │ 2154 │
                               │ novenquinquagintatrecentilliard       │ 2157 │
                               │ sexagintatrecentillion                │ 2160 │
                               │ sexagintatrecentilliard               │ 2163 │
                               │ unsexagintatrecentillion              │ 2166 │
                               │ unsexagintatrecentilliard             │ 2169 │
                               │ duosexagintatrecentillion             │ 2172 │
                               │ duosexagintatrecentilliard            │ 2175 │
                               │ tresexagintatrecentillion             │ 2178 │
                               │ tresexagintatrecentilliard            │ 2181 │
                               │ quattuorsexagintatrecentillion        │ 2184 │
                               │ quattuorsexagintatrecentilliard       │ 2187 │
                               │ quinquasexagintatrecentillion         │ 2190 │
                               │ quinquasexagintatrecentilliard        │ 2193 │
                               │ sesexagintatrecentillion              │ 2196 │
                               │ sesexagintatrecentilliard             │ 2199 │
                               │ septensexagintatrecentillion          │ 2202 │
                               │ septensexagintatrecentilliard         │ 2205 │
                               │ octosexagintatrecentillion            │ 2208 │
                               │ octosexagintatrecentilliard           │ 2211 │
                               │ novensexagintatrecentillion           │ 2214 │
                               │ novensexagintatrecentilliard          │ 2217 │
                               │ septuagintatrecentillion              │ 2220 │
                               │ septuagintatrecentilliard             │ 2223 │
                               │ unseptuagintatrecentillion            │ 2226 │
                               │ unseptuagintatrecentilliard           │ 2229 │
                               │ duoseptuagintatrecentillion           │ 2232 │
                               │ duoseptuagintatrecentilliard          │ 2235 │
                               │ treseptuagintatrecentillion           │ 2238 │
                               │ treseptuagintatrecentilliard          │ 2241 │
                               │ quattuorseptuagintatrecentillion      │ 2244 │
                               │ quattuorseptuagintatrecentilliard     │ 2247 │
                               │ quinquaseptuagintatrecentillion       │ 2250 │
                               │ quinquaseptuagintatrecentilliard      │ 2253 │
                               │ seseptuagintatrecentillion            │ 2256 │
                               │ seseptuagintatrecentilliard           │ 2259 │
                               │ septenseptuagintatrecentillion        │ 2262 │
                               │ septenseptuagintatrecentilliard       │ 2265 │
                               │ octoseptuagintatrecentillion          │ 2268 │
                               │ octoseptuagintatrecentilliard         │ 2271 │
                               │ novenseptuagintatrecentillion         │ 2274 │
                               │ novenseptuagintatrecentilliard        │ 2277 │
                               │ octogintatrecentillion                │ 2280 │
                               │ octogintatrecentilliard               │ 2283 │
                               │ unoctogintatrecentillion              │ 2286 │
                               │ unoctogintatrecentilliard             │ 2289 │
                               │ duooctogintatrecentillion             │ 2292 │
                               │ duooctogintatrecentilliard            │ 2295 │
                               │ tresoctogintatrecentillion            │ 2298 │
                               │ tresoctogintatrecentilliard           │ 2301 │
                               │ quattuoroctogintatrecentillion        │ 2304 │
                               │ quattuoroctogintatrecentilliard       │ 2307 │
                               │ quinquaoctogintatrecentillion         │ 2310 │
                               │ quinquaoctogintatrecentilliard        │ 2313 │
                               │ sexoctogintatrecentillion             │ 2316 │
                               │ sexoctogintatrecentilliard            │ 2319 │
                               │ septemoctogintatrecentillion          │ 2322 │
                               │ septemoctogintatrecentilliard         │ 2325 │
                               │ octooctogintatrecentillion            │ 2328 │
                               │ octooctogintatrecentilliard           │ 2331 │
                               │ novemoctogintatrecentillion           │ 2334 │
                               │ novemoctogintatrecentilliard          │ 2337 │
                               │ nonagintatrecentillion                │ 2340 │
                               │ nonagintatrecentilliard               │ 2343 │
                               │ unnonagintatrecentillion              │ 2346 │
                               │ unnonagintatrecentilliard             │ 2349 │
                               │ duononagintatrecentillion             │ 2352 │
                               │ duononagintatrecentilliard            │ 2355 │
                               │ trenonagintatrecentillion             │ 2358 │
                               │ trenonagintatrecentilliard            │ 2361 │
                               │ quattuornonagintatrecentillion        │ 2364 │
                               │ quattuornonagintatrecentilliard       │ 2367 │
                               │ quinquanonagintatrecentillion         │ 2370 │
                               │ quinquanonagintatrecentilliard        │ 2373 │
                               │ senonagintatrecentillion              │ 2376 │
                               │ senonagintatrecentilliard             │ 2379 │
                               │ septenonagintatrecentillion           │ 2382 │
                               │ septenonagintatrecentilliard          │ 2385 │
                               │ octononagintatrecentillion            │ 2388 │
                               │ octononagintatrecentilliard           │ 2391 │
                               │ novenonagintatrecentillion            │ 2394 │
                               │ novenonagintatrecentilliard           │ 2397 │
                               │ quadringentillion                     │ 2400 │
                               │ quadringentilliard                    │ 2403 │
                               │ unquadringentillion                   │ 2406 │
                               │ unquadringentilliard                  │ 2409 │
                               │ duoquadringentillion                  │ 2412 │
                               │ duoquadringentilliard                 │ 2415 │
                               │ tresquadringentillion                 │ 2418 │
                               │ tresquadringentilliard                │ 2421 │
                               │ quattuorquadringentillion             │ 2424 │
                               │ quattuorquadringentilliard            │ 2427 │
                               │ quinquaquadringentillion              │ 2430 │
                               │ quinquaquadringentilliard             │ 2433 │
                               │ sesquadringentillion                  │ 2436 │
                               │ sesquadringentilliard                 │ 2439 │
                               │ septenquadringentillion               │ 2442 │
                               │ septenquadringentilliard              │ 2445 │
                               │ octoquadringentillion                 │ 2448 │
                               │ octoquadringentilliard                │ 2451 │
                               │ novenquadringentillion                │ 2454 │
                               │ novenquadringentilliard               │ 2457 │
                               │ deciquadringentillion                 │ 2460 │
                               │ deciquadringentilliard                │ 2463 │
                               │ undeciquadringentillion               │ 2466 │
                               │ undeciquadringentilliard              │ 2469 │
                               │ duodeciquadringentillion              │ 2472 │
                               │ duodeciquadringentilliard             │ 2475 │
                               │ tredeciquadringentillion              │ 2478 │
                               │ tredeciquadringentilliard             │ 2481 │
                               │ quattuordeciquadringentillion         │ 2484 │
                               │ quattuordeciquadringentilliard        │ 2487 │
                               │ quinquadeciquadringentillion          │ 2490 │
                               │ quinquadeciquadringentilliard         │ 2493 │
                               │ sedeciquadringentillion               │ 2496 │
                               │ sedeciquadringentilliard              │ 2499 │
                               │ septendeciquadringentillion           │ 2502 │
                               │ septendeciquadringentilliard          │ 2505 │
                               │ octodeciquadringentillion             │ 2508 │
                               │ octodeciquadringentilliard            │ 2511 │
                               │ novendeciquadringentillion            │ 2514 │
                               │ novendeciquadringentilliard           │ 2517 │
                               │ vigintiquadringentillion              │ 2520 │
                               │ vigintiquadringentilliard             │ 2523 │
                               │ unvigintiquadringentillion            │ 2526 │
                               │ unvigintiquadringentilliard           │ 2529 │
                               │ duovigintiquadringentillion           │ 2532 │
                               │ duovigintiquadringentilliard          │ 2535 │
                               │ tresvigintiquadringentillion          │ 2538 │
                               │ tresvigintiquadringentilliard         │ 2541 │
                               │ quattuorvigintiquadringentillion      │ 2544 │
                               │ quattuorvigintiquadringentilliard     │ 2547 │
                               │ quinquavigintiquadringentillion       │ 2550 │
                               │ quinquavigintiquadringentilliard      │ 2553 │
                               │ sesvigintiquadringentillion           │ 2556 │
                               │ sesvigintiquadringentilliard          │ 2559 │
                               │ septemvigintiquadringentillion        │ 2562 │
                               │ septemvigintiquadringentilliard       │ 2565 │
                               │ octovigintiquadringentillion          │ 2568 │
                               │ octovigintiquadringentilliard         │ 2571 │
                               │ novemvigintiquadringentillion         │ 2574 │
                               │ novemvigintiquadringentilliard        │ 2577 │
                               │ trigintaquadringentillion             │ 2580 │
                               │ trigintaquadringentilliard            │ 2583 │
                               │ untrigintaquadringentillion           │ 2586 │
                               │ untrigintaquadringentilliard          │ 2589 │
                               │ duotrigintaquadringentillion          │ 2592 │
                               │ duotrigintaquadringentilliard         │ 2595 │
                               │ trestrigintaquadringentillion         │ 2598 │
                               │ trestrigintaquadringentilliard        │ 2601 │
                               │ quattuortrigintaquadringentillion     │ 2604 │
                               │ quattuortrigintaquadringentilliard    │ 2607 │
                               │ quinquatrigintaquadringentillion      │ 2610 │
                               │ quinquatrigintaquadringentilliard     │ 2613 │
                               │ sestrigintaquadringentillion          │ 2616 │
                               │ sestrigintaquadringentilliard         │ 2619 │
                               │ septentrigintaquadringentillion       │ 2622 │
                               │ septentrigintaquadringentilliard      │ 2625 │
                               │ octotrigintaquadringentillion         │ 2628 │
                               │ octotrigintaquadringentilliard        │ 2631 │
                               │ noventrigintaquadringentillion        │ 2634 │
                               │ noventrigintaquadringentilliard       │ 2637 │
                               │ quadragintaquadringentillion          │ 2640 │
                               │ quadragintaquadringentilliard         │ 2643 │
                               │ unquadragintaquadringentillion        │ 2646 │
                               │ unquadragintaquadringentilliard       │ 2649 │
                               │ duoquadragintaquadringentillion       │ 2652 │
                               │ duoquadragintaquadringentilliard      │ 2655 │
                               │ tresquadragintaquadringentillion      │ 2658 │
                               │ tresquadragintaquadringentilliard     │ 2661 │
                               │ quattuorquadragintaquadringentillion  │ 2664 │
                               │ quattuorquadragintaquadringentilliard │ 2667 │
                               │ quinquaquadragintaquadringentillion   │ 2670 │
                               │ quinquaquadragintaquadringentilliard  │ 2673 │
                               │ sesquadragintaquadringentillion       │ 2676 │
                               │ sesquadragintaquadringentilliard      │ 2679 │
                               │ septenquadragintaquadringentillion    │ 2682 │
                               │ septenquadragintaquadringentilliard   │ 2685 │
                               │ octoquadragintaquadringentillion      │ 2688 │
                               │ octoquadragintaquadringentilliard     │ 2691 │
                               │ novenquadragintaquadringentillion     │ 2694 │
                               │ novenquadragintaquadringentilliard    │ 2697 │
                               │ quinquagintaquadringentillion         │ 2700 │
                               │ quinquagintaquadringentilliard        │ 2703 │
                               │ unquinquagintaquadringentillion       │ 2706 │
                               │ unquinquagintaquadringentilliard      │ 2709 │
                               │ duoquinquagintaquadringentillion      │ 2712 │
                               │ duoquinquagintaquadringentilliard     │ 2715 │
                               │ tresquinquagintaquadringentillion     │ 2718 │
                               │ tresquinquagintaquadringentilliard    │ 2721 │
                               │ quattuorquinquagintaquadringentillion │ 2724 │
                               │ quattuorquinquagintaquadringentilliard│ 2727 │
                               │ quinquaquinquagintaquadringentillion  │ 2730 │
                               │ quinquaquinquagintaquadringentilliard │ 2733 │
                               │ sesquinquagintaquadringentillion      │ 2736 │
                               │ sesquinquagintaquadringentilliard     │ 2739 │
                               │ septenquinquagintaquadringentillion   │ 2742 │
                               │ septenquinquagintaquadringentilliard  │ 2745 │
                               │ octoquinquagintaquadringentillion     │ 2748 │
                               │ octoquinquagintaquadringentilliard    │ 2751 │
                               │ novenquinquagintaquadringentillion    │ 2754 │
                               │ novenquinquagintaquadringentilliard   │ 2757 │
                               │ sexagintaquadringentillion            │ 2760 │
                               │ sexagintaquadringentilliard           │ 2763 │
                               │ unsexagintaquadringentillion          │ 2766 │
                               │ unsexagintaquadringentilliard         │ 2769 │
                               │ duosexagintaquadringentillion         │ 2772 │
                               │ duosexagintaquadringentilliard        │ 2775 │
                               │ tresexagintaquadringentillion         │ 2778 │
                               │ tresexagintaquadringentilliard        │ 2781 │
                               │ quattuorsexagintaquadringentillion    │ 2784 │
                               │ quattuorsexagintaquadringentilliard   │ 2787 │
                               │ quinquasexagintaquadringentillion     │ 2790 │
                               │ quinquasexagintaquadringentilliard    │ 2793 │
                               │ sesexagintaquadringentillion          │ 2796 │
                               │ sesexagintaquadringentilliard         │ 2799 │
                               │ septensexagintaquadringentillion      │ 2802 │
                               │ septensexagintaquadringentilliard     │ 2805 │
                               │ octosexagintaquadringentillion        │ 2808 │
                               │ octosexagintaquadringentilliard       │ 2811 │
                               │ novensexagintaquadringentillion       │ 2814 │
                               │ novensexagintaquadringentilliard      │ 2817 │
                               │ septuagintaquadringentillion          │ 2820 │
                               │ septuagintaquadringentilliard         │ 2823 │
                               │ unseptuagintaquadringentillion        │ 2826 │
                               │ unseptuagintaquadringentilliard       │ 2829 │
                               │ duoseptuagintaquadringentillion       │ 2832 │
                               │ duoseptuagintaquadringentilliard      │ 2835 │
                               │ treseptuagintaquadringentillion       │ 2838 │
                               │ treseptuagintaquadringentilliard      │ 2841 │
                               │ quattuorseptuagintaquadringentillion  │ 2844 │
                               │ quattuorseptuagintaquadringentilliard │ 2847 │
                               │ quinquaseptuagintaquadringentillion   │ 2850 │
                               │ quinquaseptuagintaquadringentilliard  │ 2853 │
                               │ seseptuagintaquadringentillion        │ 2856 │
                               │ seseptuagintaquadringentilliard       │ 2859 │
                               │ septenseptuagintaquadringentillion    │ 2862 │
                               │ septenseptuagintaquadringentilliard   │ 2865 │
                               │ octoseptuagintaquadringentillion      │ 2868 │
                               │ octoseptuagintaquadringentilliard     │ 2871 │
                               │ novenseptuagintaquadringentillion     │ 2874 │
                               │ novenseptuagintaquadringentilliard    │ 2877 │
                               │ octogintaquadringentillion            │ 2880 │
                               │ octogintaquadringentilliard           │ 2883 │
                               │ unoctogintaquadringentillion          │ 2886 │
                               │ unoctogintaquadringentilliard         │ 2889 │
                               │ duooctogintaquadringentillion         │ 2892 │
                               │ duooctogintaquadringentilliard        │ 2895 │
                               │ tresoctogintaquadringentillion        │ 2898 │
                               │ tresoctogintaquadringentilliard       │ 2901 │
                               │ quattuoroctogintaquadringentillion    │ 2904 │
                               │ quattuoroctogintaquadringentilliard   │ 2907 │
                               │ quinquaoctogintaquadringentillion     │ 2910 │
                               │ quinquaoctogintaquadringentilliard    │ 2913 │
                               │ sexoctogintaquadringentillion         │ 2916 │
                               │ sexoctogintaquadringentilliard        │ 2919 │
                               │ septemoctogintaquadringentillion      │ 2922 │
                               │ septemoctogintaquadringentilliard     │ 2925 │
                               │ octooctogintaquadringentillion        │ 2928 │
                               │ octooctogintaquadringentilliard       │ 2931 │
                               │ novemoctogintaquadringentillion       │ 2934 │
                               │ novemoctogintaquadringentilliard      │ 2937 │
                               │ nonagintaquadringentillion            │ 2940 │
                               │ nonagintaquadringentilliard           │ 2943 │
                               │ unnonagintaquadringentillion          │ 2946 │
                               │ unnonagintaquadringentilliard         │ 2949 │
                               │ duononagintaquadringentillion         │ 2952 │
                               │ duononagintaquadringentilliard        │ 2955 │
                               │ trenonagintaquadringentillion         │ 2958 │
                               │ trenonagintaquadringentilliard        │ 2961 │
                               │ quattuornonagintaquadringentillion    │ 2964 │
                               │ quattuornonagintaquadringentilliard   │ 2967 │
                               │ quinquanonagintaquadringentillion     │ 2970 │
                               │ quinquanonagintaquadringentilliard    │ 2973 │
                               │ senonagintaquadringentillion          │ 2976 │
                               │ senonagintaquadringentilliard         │ 2979 │
                               │ septenonagintaquadringentillion       │ 2982 │
                               │ septenonagintaquadringentilliard      │ 2985 │
                               │ octononagintaquadringentillion        │ 2988 │
                               │ octononagintaquadringentilliard       │ 2991 │
                               │ novenonagintaquadringentillion        │ 2994 │
                               │ novenonagintaquadringentilliard       │ 2997 │
                               │ quingentillion                        │ 3000 │
                               │ quingentilliard                       │ 3003 │
                               │ unquingentillion                      │ 3006 │
                               │ unquingentilliard                     │ 3009 │
                               │ duoquingentillion                     │ 3012 │
                               │ duoquingentilliard                    │ 3015 │
                               │ tresquingentillion                    │ 3018 │
                               │ tresquingentilliard                   │ 3021 │
                               │ quattuorquingentillion                │ 3024 │
                               │ quattuorquingentilliard               │ 3027 │
                               │ quinquaquingentillion                 │ 3030 │
                               │ quinquaquingentilliard                │ 3033 │
                               │ sesquingentillion                     │ 3036 │
                               │ sesquingentilliard                    │ 3039 │
                               │ septenquingentillion                  │ 3042 │
                               │ septenquingentilliard                 │ 3045 │
                               │ octoquingentillion                    │ 3048 │
                               │ octoquingentilliard                   │ 3051 │
                               │ novenquingentillion                   │ 3054 │
                               │ novenquingentilliard                  │ 3057 │
                               │ deciquingentillion                    │ 3060 │
                               │ deciquingentilliard                   │ 3063 │
                               │ undeciquingentillion                  │ 3066 │
                               │ undeciquingentilliard                 │ 3069 │
                               │ duodeciquingentillion                 │ 3072 │
                               │ duodeciquingentilliard                │ 3075 │
                               │ tredeciquingentillion                 │ 3078 │
                               │ tredeciquingentilliard                │ 3081 │
                               │ quattuordeciquingentillion            │ 3084 │
                               │ quattuordeciquingentilliard           │ 3087 │
                               │ quinquadeciquingentillion             │ 3090 │
                               │ quinquadeciquingentilliard            │ 3093 │
                               │ sedeciquingentillion                  │ 3096 │
                               │ sedeciquingentilliard                 │ 3099 │
                               │ septendeciquingentillion              │ 3102 │
                               │ septendeciquingentilliard             │ 3105 │
                               │ octodeciquingentillion                │ 3108 │
                               │ octodeciquingentilliard               │ 3111 │
                               │ novendeciquingentillion               │ 3114 │
                               │ novendeciquingentilliard              │ 3117 │
                               │ vigintiquingentillion                 │ 3120 │
                               │ vigintiquingentilliard                │ 3123 │
                               │ unvigintiquingentillion               │ 3126 │
                               │ unvigintiquingentilliard              │ 3129 │
                               │ duovigintiquingentillion              │ 3132 │
                               │ duovigintiquingentilliard             │ 3135 │
                               │ tresvigintiquingentillion             │ 3138 │
                               │ tresvigintiquingentilliard            │ 3141 │
                               │ quattuorvigintiquingentillion         │ 3144 │
                               │ quattuorvigintiquingentilliard        │ 3147 │
                               │ quinquavigintiquingentillion          │ 3150 │
                               │ quinquavigintiquingentilliard         │ 3153 │
                               │ sesvigintiquingentillion              │ 3156 │
                               │ sesvigintiquingentilliard             │ 3159 │
                               │ septemvigintiquingentillion           │ 3162 │
                               │ septemvigintiquingentilliard          │ 3165 │
                               │ octovigintiquingentillion             │ 3168 │
                               │ octovigintiquingentilliard            │ 3171 │
                               │ novemvigintiquingentillion            │ 3174 │
                               │ novemvigintiquingentilliard           │ 3177 │
                               │ trigintaquingentillion                │ 3180 │
                               │ trigintaquingentilliard               │ 3183 │
                               │ untrigintaquingentillion              │ 3186 │
                               │ untrigintaquingentilliard             │ 3189 │
                               │ duotrigintaquingentillion             │ 3192 │
                               │ duotrigintaquingentilliard            │ 3195 │
                               │ trestrigintaquingentillion            │ 3198 │
                               │ trestrigintaquingentilliard           │ 3201 │
                               │ quattuortrigintaquingentillion        │ 3204 │
                               │ quattuortrigintaquingentilliard       │ 3207 │
                               │ quinquatrigintaquingentillion         │ 3210 │
                               │ quinquatrigintaquingentilliard        │ 3213 │
                               │ sestrigintaquingentillion             │ 3216 │
                               │ sestrigintaquingentilliard            │ 3219 │
                               │ septentrigintaquingentillion          │ 3222 │
                               │ septentrigintaquingentilliard         │ 3225 │
                               │ octotrigintaquingentillion            │ 3228 │
                               │ octotrigintaquingentilliard           │ 3231 │
                               │ noventrigintaquingentillion           │ 3234 │
                               │ noventrigintaquingentilliard          │ 3237 │
                               │ quadragintaquingetillion              │ 3240 │
                               │ quadragintaquingetilliard             │ 3243 │
                               │ unquadragintaquingentillion           │ 3246 │
                               │ unquadragintaquingentilliard          │ 3249 │
                               │ duoquadragintaquingentillion          │ 3252 │
                               │ duoquadragintaquingentilliard         │ 3255 │
                               │ tresquadragintaquingentillion         │ 3258 │
                               │ tresquadragintaquingentilliard        │ 3261 │
                               │ quattuorquadragintaquingentillion     │ 3264 │
                               │ quattuorquadragintaquingentilliard    │ 3267 │
                               │ quinquaquadragintaquingentillion      │ 3270 │
                               │ quinquaquadragintaquingentilliard     │ 3273 │
                               │ sesquadragintaquingentillion          │ 3276 │
                               │ sesquadragintaquingentilliard         │ 3279 │
                               │ septenquadragintaquingentillion       │ 3282 │
                               │ septenquadragintaquingentilliard      │ 3285 │
                               │ octoquadragintaquingentillion         │ 3288 │
                               │ octoquadragintaquingentilliard        │ 3291 │
                               │ novenquadragintaquingentillion        │ 3294 │
                               │ novenquadragintaquingentilliard       │ 3297 │
                               │ quinquagintaquingentillion            │ 3300 │
                               │ quinquagintaquingentilliard           │ 3303 │
                               │ unquinquagintaquingentillion          │ 3306 │
                               │ unquinquagintaquingentilliard         │ 3309 │
                               │ duoquinquagintaquingentillion         │ 3312 │
                               │ duoquinquagintaquingentilliard        │ 3315 │
                               │ tresquinquagintaquingentillion        │ 3318 │
                               │ tresquinquagintaquingentilliard       │ 3321 │
                               │ quattuorquinquagintaquingentillion    │ 3324 │
                               │ quattuorquinquagintaquingentilliard   │ 3327 │
                               │ quinquaquinquagintaquingentillion     │ 3330 │
                               │ quinquaquinquagintaquingentilliard    │ 3333 │
                               │ sesquinquagintaquingentillion         │ 3336 │
                               │ sesquinquagintaquingentilliard        │ 3339 │
                               │ septenquinquagintaquingentillion      │ 3342 │
                               │ septenquinquagintaquingentilliard     │ 3345 │
                               │ octoquinquagintaquingentillion        │ 3348 │
                               │ octoquinquagintaquingentilliard       │ 3351 │
                               │ novenquinquagintaquingentillion       │ 3354 │
                               │ novenquinquagintaquingentilliard      │ 3357 │
                               │ sexagintaquingentillion               │ 3360 │
                               │ sexagintaquingentilliard              │ 3363 │
                               │ unsexagintaquingentillion             │ 3366 │
                               │ unsexagintaquingentilliard            │ 3369 │
                               │ duosexagintaquingentillion            │ 3372 │
                               │ duosexagintaquingentilliard           │ 3375 │
                               │ tresexagintaquingentillion            │ 3378 │
                               │ tresexagintaquingentilliard           │ 3381 │
                               │ quattuorsexagintaquingentillion       │ 3384 │
                               │ quattuorsexagintaquingentilliard      │ 3387 │
                               │ quinquasexagintaquingentillion        │ 3390 │
                               │ quinquasexagintaquingentilliard       │ 3393 │
                               │ sesexagintaquingentillion             │ 3396 │
                               │ sesexagintaquingentilliard            │ 3399 │
                               │ septensexagintaquingentillion         │ 3402 │
                               │ septensexagintaquingentilliard        │ 3405 │
                               │ octosexagintaquingentillion           │ 3408 │
                               │ octosexagintaquingentilliard          │ 3411 │
                               │ novensexagintaquingentillion          │ 3414 │
                               │ novensexagintaquingentilliard         │ 3417 │
                               │ septuagintaquingentillion             │ 3420 │
                               │ septuagintaquingentilliard            │ 3423 │
                               │ unseptuagintaquingentillion           │ 3426 │
                               │ unseptuagintaquingentilliard          │ 3429 │
                               │ duoseptuagintaquingentillion          │ 3432 │
                               │ duoseptuagintaquingentilliard         │ 3435 │
                               │ treseptuagintaquingentillion          │ 3438 │
                               │ treseptuagintaquingentilliard         │ 3441 │
                               │ quattuorseptuagintaquingentillion     │ 3444 │
                               │ quattuorseptuagintaquingentilliard    │ 3447 │
                               │ quinquaseptuagintaquingentillion      │ 3450 │
                               │ quinquaseptuagintaquingentilliard     │ 3453 │
                               │ seseptuagintaquingentillion           │ 3456 │
                               │ seseptuagintaquingentilliard          │ 3459 │
                               │ septenseptuagintaquingentillion       │ 3462 │
                               │ septenseptuagintaquingentilliard      │ 3465 │
                               │ octoseptuagintaquingentillion         │ 3468 │
                               │ octoseptuagintaquingentilliard        │ 3471 │
                               │ novenseptuagintaquingentillion        │ 3474 │
                               │ novenseptuagintaquingentilliard       │ 3477 │
                               │ octogintaquingentillion               │ 3480 │
                               │ octogintaquingentilliard              │ 3483 │
                               │ unoctogintaquingentillion             │ 3486 │
                               │ unoctogintaquingentilliard            │ 3489 │
                               │ duooctogintaquingentillion            │ 3492 │
                               │ duooctogintaquingentilliard           │ 3495 │
                               │ tresoctogintaquingentillion           │ 3498 │
                               │ tresoctogintaquingentilliard          │ 3501 │
                               │ quattuoroctogintaquingentillion       │ 3504 │
                               │ quattuoroctogintaquingentilliard      │ 3507 │
                               │ quinquaoctogintaquingentillion        │ 3510 │
                               │ quinquaoctogintaquingentilliard       │ 3513 │
                               │ sexoctogintaquingentillion            │ 3516 │
                               │ sexoctogintaquingentilliard           │ 3519 │
                               │ septemoctogintaquingentillion         │ 3522 │
                               │ septemoctogintaquingentilliard        │ 3525 │
                               │ octooctogintaquingentillion           │ 3528 │
                               │ octooctogintaquingentilliard          │ 3531 │
                               │ novemoctogintaquingentillion          │ 3534 │
                               │ novemoctogintaquingentilliard         │ 3537 │
                               │ nonagintaquingentillion               │ 3540 │
                               │ nonagintaquingentilliard              │ 3543 │
                               │ unnonagintaquingentillion             │ 3546 │
                               │ unnonagintaquingentilliard            │ 3549 │
                               │ duononagintaquingentillion            │ 3552 │
                               │ duononagintaquingentilliard           │ 3555 │
                               │ trenonagintaquingentillion            │ 3558 │
                               │ trenonagintaquingentilliard           │ 3561 │
                               │ quattuornonagintaquingentillion       │ 3564 │
                               │ quattuornonagintaquingentilliard      │ 3567 │
                               │ quinquanonagintaquingentillion        │ 3570 │
                               │ quinquanonagintaquingentilliard       │ 3573 │
                               │ senonagintaquingentillion             │ 3576 │
                               │ senonagintaquingentilliard            │ 3579 │
                               │ septenonagintaquingentillion          │ 3582 │
                               │ septenonagintaquingentilliard         │ 3585 │
                               │ octononagintaquingentillion           │ 3588 │
                               │ octononagintaquingentilliard          │ 3591 │
                               │ novenonagintaquingentillion           │ 3594 │
                               │ novenonagintaquingentilliard          │ 3597 │
                               │ sescentillion                         │ 3600 │
                               │ sescentilliard                        │ 3603 │
                               │ unsescentillion                       │ 3606 │
                               │ unsescentilliard                      │ 3609 │
                               │ duosescentillion                      │ 3612 │
                               │ duosescentilliard                     │ 3615 │
                               │ tresescentillion                      │ 3618 │
                               │ tresescentilliard                     │ 3621 │
                               │ quattuorsescentillion                 │ 3624 │
                               │ quattuorsescentilliard                │ 3627 │
                               │ quinquasescentillion                  │ 3630 │
                               │ quinquasescentilliard                 │ 3633 │
                               │ sesescentillion                       │ 3636 │
                               │ sesescentilliard                      │ 3639 │
                               │ septensescentillion                   │ 3642 │
                               │ septensescentilliard                  │ 3645 │
                               │ octosescentillion                     │ 3648 │
                               │ octosescentilliard                    │ 3651 │
                               │ novensescentillion                    │ 3654 │
                               │ novensescentilliard                   │ 3657 │
                               │ decisescentillion                     │ 3660 │
                               │ decisescentilliard                    │ 3663 │
                               │ undecisescentillion                   │ 3666 │
                               │ undecisescentilliard                  │ 3669 │
                               │ duodecisescentillion                  │ 3672 │
                               │ duodecisescentilliard                 │ 3675 │
                               │ tredecisescentillion                  │ 3678 │
                               │ tredecisescentilliard                 │ 3681 │
                               │ quattuordecisescentillion             │ 3684 │
                               │ quattuordecisescentilliard            │ 3687 │
                               │ quinquadecisescentillion              │ 3690 │
                               │ quinquadecisescentilliard             │ 3693 │
                               │ sedecisescentillion                   │ 3696 │
                               │ sedecisescentilliard                  │ 3699 │
                               │ septendecisescentillion               │ 3702 │
                               │ septendecisescentilliard              │ 3705 │
                               │ octodecisescentillion                 │ 3708 │
                               │ octodecisescentilliard                │ 3711 │
                               │ novendecisescentillion                │ 3714 │
                               │ novendecisescentilliard               │ 3717 │
                               │ vigintisescentillion                  │ 3720 │
                               │ vigintisescentilliard                 │ 3723 │
                               │ unvigintisescentillion                │ 3726 │
                               │ unvigintisescentilliard               │ 3729 │
                               │ duovigintisescentillion               │ 3732 │
                               │ duovigintisescentilliard              │ 3735 │
                               │ tresvigintisescentillion              │ 3738 │
                               │ tresvigintisescentilliard             │ 3741 │
                               │ quattuorvigintisescentillion          │ 3744 │
                               │ quattuorvigintisescentilliard         │ 3747 │
                               │ quinquavigintisescentillion           │ 3750 │
                               │ quinquavigintisescentilliard          │ 3753 │
                               │ sesvigintisescentillion               │ 3756 │
                               │ sesvigintisescentilliard              │ 3759 │
                               │ septemvigintisescentillion            │ 3762 │
                               │ septemvigintisescentilliard           │ 3765 │
                               │ octovigintisescentillion              │ 3768 │
                               │ octovigintisescentilliard             │ 3771 │
                               │ novemvigintisescentillion             │ 3774 │
                               │ novemvigintisescentilliard            │ 3777 │
                               │ trigintasescentillion                 │ 3780 │
                               │ trigintasescentilliard                │ 3783 │
                               │ untrigintasescentillion               │ 3786 │
                               │ untrigintasescentilliard              │ 3789 │
                               │ duotrigintasescentillion              │ 3792 │
                               │ duotrigintasescentilliard             │ 3795 │
                               │ trestrigintasescentillion             │ 3798 │
                               │ trestrigintasescentilliard            │ 3801 │
                               │ quattuortrigintasescentillion         │ 3804 │
                               │ quattuortrigintasescentilliard        │ 3807 │
                               │ quinquatrigintasescentillion          │ 3810 │
                               │ quinquatrigintasescentilliard         │ 3813 │
                               │ sestrigintasescentillion              │ 3816 │
                               │ sestrigintasescentilliard             │ 3819 │
                               │ septentrigintasescentillion           │ 3822 │
                               │ septentrigintasescentilliard          │ 3825 │
                               │ octotrigintasescentillion             │ 3828 │
                               │ octotrigintasescentilliard            │ 3831 │
                               │ noventrigintasescentillion            │ 3834 │
                               │ noventrigintasescentilliard           │ 3837 │
                               │ quadragintasescentillion              │ 3840 │
                               │ quadragintasescentilliard             │ 3843 │
                               │ unquadragintasescentillion            │ 3846 │
                               │ unquadragintasescentilliard           │ 3849 │
                               │ duoquadragintasescentillion           │ 3852 │
                               │ duoquadragintasescentilliard          │ 3855 │
                               │ tresquadragintasescentillion          │ 3858 │
                               │ tresquadragintasescentilliard         │ 3861 │
                               │ quattuorquadragintasescentillion      │ 3864 │
                               │ quattuorquadragintasescentilliard     │ 3867 │
                               │ quinquaquadragintasescentillion       │ 3870 │
                               │ quinquaquadragintasescentilliard      │ 3873 │
                               │ sesquadragintasescentillion           │ 3876 │
                               │ sesquadragintasescentilliard          │ 3879 │
                               │ septenquadragintasescentillion        │ 3882 │
                               │ septenquadragintasescentilliard       │ 3885 │
                               │ octoquadragintasescentillion          │ 3888 │
                               │ octoquadragintasescentilliard         │ 3891 │
                               │ novenquadragintasescentillion         │ 3894 │
                               │ novenquadragintasescentilliard        │ 3897 │
                               │ quinquagintasescentillion             │ 3900 │
                               │ quinquagintasescentilliard            │ 3903 │
                               │ unquinquagintasescentillion           │ 3906 │
                               │ unquinquagintasescentilliard          │ 3909 │
                               │ duoquinquagintasescentillion          │ 3912 │
                               │ duoquinquagintasescentilliard         │ 3915 │
                               │ tresquinquagintasescentillion         │ 3918 │
                               │ tresquinquagintasescentilliard        │ 3921 │
                               │ quattuorquinquagintasescentillion     │ 3924 │
                               │ quattuorquinquagintasescentilliard    │ 3927 │
                               │ quinquaquinquagintasescentillion      │ 3930 │
                               │ quinquaquinquagintasescentilliard     │ 3933 │
                               │ sesquinquagintasescentillion          │ 3936 │
                               │ sesquinquagintasescentilliard         │ 3939 │
                               │ septenquinquagintasescentillion       │ 3942 │
                               │ septenquinquagintasescentilliard      │ 3945 │
                               │ octoquinquagintasescentillion         │ 3948 │
                               │ octoquinquagintasescentilliard        │ 3951 │
                               │ novenquinquagintasescentillion        │ 3954 │
                               │ novenquinquagintasescentilliard       │ 3957 │
                               │ sexagintasescentillion                │ 3960 │
                               │ sexagintasescentilliard               │ 3963 │
                               │ unsexagintasescentillion              │ 3966 │
                               │ unsexagintasescentilliard             │ 3969 │
                               │ duosexagintasescentillion             │ 3972 │
                               │ duosexagintasescentilliard            │ 3975 │
                               │ tresexagintasescentillion             │ 3978 │
                               │ tresexagintasescentilliard            │ 3981 │
                               │ quattuorsexagintasescentillion        │ 3984 │
                               │ quattuorsexagintasescentilliard       │ 3987 │
                               │ quinquasexagintasescentillion         │ 3990 │
                               │ quinquasexagintasescentilliard        │ 3993 │
                               │ sesexagintasescentillion              │ 3996 │
                               │ sesexagintasescentilliard             │ 3999 │
                               │ septensexagintasescentillion          │ 4002 │
                               │ septensexagintasescentilliard         │ 4005 │
                               │ octosexagintasescentillion            │ 4008 │
                               │ octosexagintasescentilliard           │ 4011 │
                               │ novensexagintasescentillion           │ 4014 │
                               │ novensexagintasescentilliard          │ 4017 │
                               │ septuagintasescentillion              │ 4020 │
                               │ septuagintasescentilliard             │ 4023 │
                               │ unseptuagintasescentillion            │ 4026 │
                               │ unseptuagintasescentilliard           │ 4029 │
                               │ duoseptuagintasescentillion           │ 4032 │
                               │ duoseptuagintasescentilliard          │ 4035 │
                               │ treseptuagintasescentillion           │ 4038 │
                               │ treseptuagintasescentilliard          │ 4041 │
                               │ quattuorseptuagintasescentillion      │ 4044 │
                               │ quattuorseptuagintasescentilliard     │ 4047 │
                               │ quinquaseptuagintasescentillion       │ 4050 │
                               │ quinquaseptuagintasescentilliard      │ 4053 │
                               │ seseptuagintasescentillion            │ 4056 │
                               │ seseptuagintasescentilliard           │ 4059 │
                               │ septenseptuagintasescentillion        │ 4062 │
                               │ septenseptuagintasescentilliard       │ 4065 │
                               │ octoseptuagintasescentillion          │ 4068 │
                               │ octoseptuagintasescentilliard         │ 4071 │
                               │ novenseptuagintasescentillion         │ 4074 │
                               │ novenseptuagintasescentilliard        │ 4077 │
                               │ octogintasescentillion                │ 4080 │
                               │ octogintasescentilliard               │ 4083 │
                               │ unoctogintasescentillion              │ 4086 │
                               │ unoctogintasescentilliard             │ 4089 │
                               │ duooctogintasescentillion             │ 4092 │
                               │ duooctogintasescentilliard            │ 4095 │
                               │ tresoctogintasescentillion            │ 4098 │
                               │ tresoctogintasescentilliard           │ 4101 │
                               │ quattuoroctogintasescentillion        │ 4104 │
                               │ quattuoroctogintasescentilliard       │ 4107 │
                               │ quinquaoctogintasescentillion         │ 4110 │
                               │ quinquaoctogintasescentilliard        │ 4113 │
                               │ sexoctogintasescentillion             │ 4116 │
                               │ sexoctogintasescentilliard            │ 4119 │
                               │ septemoctogintasescentillion          │ 4122 │
                               │ septemoctogintasescentilliard         │ 4125 │
                               │ octooctogintasescentillion            │ 4128 │
                               │ octooctogintasescentilliard           │ 4131 │
                               │ novemoctogintasescentillion           │ 4134 │
                               │ novemoctogintasescentilliard          │ 4137 │
                               │ nonagintasescetillion                 │ 4140 │
                               │ nonagintasescetilliard                │ 4143 │
                               │ unnonagintasescentillion              │ 4146 │
                               │ unnonagintasescentilliard             │ 4149 │
                               │ duononagintasescentillion             │ 4152 │
                               │ duononagintasescentilliard            │ 4155 │
                               │ trenonagintasescentillion             │ 4158 │
                               │ trenonagintasescentilliard            │ 4161 │
                               │ quattuornonagintasescentillion        │ 4164 │
                               │ quattuornonagintasescentilliard       │ 4167 │
                               │ quinquanonagintasescentillion         │ 4170 │
                               │ quinquanonagintasescentilliard        │ 4173 │
                               │ senonagintasescentillion              │ 4176 │
                               │ senonagintasescentilliard             │ 4179 │
                               │ septenonagintasescentillion           │ 4182 │
                               │ septenonagintasescentilliard          │ 4185 │
                               │ octononagintasescentillion            │ 4188 │
                               │ octononagintasescentilliard           │ 4191 │
                               │ novenonagintasescentillion            │ 4194 │
                               │ novenonagintasescentilliard           │ 4197 │
                               │ septingentillion                      │ 4200 │
                               │ septingentilliard                     │ 4203 │
                               │ unseptingentillion                    │ 4206 │
                               │ unseptingentilliard                   │ 4209 │
                               │ duoseptingentillion                   │ 4212 │
                               │ duoseptingentilliard                  │ 4215 │
                               │ treseptingentillion                   │ 4218 │
                               │ treseptingentilliard                  │ 4221 │
                               │ quattuorseptingentillion              │ 4224 │
                               │ quattuorseptingentilliard             │ 4227 │
                               │ quinquaseptingentillion               │ 4230 │
                               │ quinquaseptingentilliard              │ 4233 │
                               │ seseptingentillion                    │ 4236 │
                               │ seseptingentilliard                   │ 4239 │
                               │ septenseptingentillion                │ 4242 │
                               │ septenseptingentilliard               │ 4245 │
                               │ octoseptingentillion                  │ 4248 │
                               │ octoseptingentilliard                 │ 4251 │
                               │ novenseptingentillion                 │ 4254 │
                               │ novenseptingentilliard                │ 4257 │
                               │ deciseptingentillion                  │ 4260 │
                               │ deciseptingentilliard                 │ 4263 │
                               │ undeciseptingentillion                │ 4266 │
                               │ undeciseptingentilliard               │ 4269 │
                               │ duodeciseptingentillion               │ 4272 │
                               │ duodeciseptingentilliard              │ 4275 │
                               │ tredeciseptingentillion               │ 4278 │
                               │ tredeciseptingentilliard              │ 4281 │
                               │ quattuordeciseptingentillion          │ 4284 │
                               │ quattuordeciseptingentilliard         │ 4287 │
                               │ quinquadeciseptingentillion           │ 4290 │
                               │ quinquadeciseptingentilliard          │ 4293 │
                               │ sedeciseptingentillion                │ 4296 │
                               │ sedeciseptingentilliard               │ 4299 │
                               │ septendeciseptingentillion            │ 4302 │
                               │ septendeciseptingentilliard           │ 4305 │
                               │ octodeciseptingentillion              │ 4308 │
                               │ octodeciseptingentilliard             │ 4311 │
                               │ novendeciseptingentillion             │ 4314 │
                               │ novendeciseptingentilliard            │ 4317 │
                               │ vigintiseptingentillion               │ 4320 │
                               │ vigintiseptingentilliard              │ 4323 │
                               │ unvigintiseptingentillion             │ 4326 │
                               │ unvigintiseptingentilliard            │ 4329 │
                               │ duovigintiseptingentillion            │ 4332 │
                               │ duovigintiseptingentilliard           │ 4335 │
                               │ tresvigintiseptingentillion           │ 4338 │
                               │ tresvigintiseptingentilliard          │ 4341 │
                               │ quattuorvigintiseptingentillion       │ 4344 │
                               │ quattuorvigintiseptingentilliard      │ 4347 │
                               │ quinquavigintiseptingentillion        │ 4350 │
                               │ quinquavigintiseptingentilliard       │ 4353 │
                               │ sesvigintiseptingentillion            │ 4356 │
                               │ sesvigintiseptingentilliard           │ 4359 │
                               │ septemvigintiseptingentillion         │ 4362 │
                               │ septemvigintiseptingentilliard        │ 4365 │
                               │ octovigintiseptingentillion           │ 4368 │
                               │ octovigintiseptingentilliard          │ 4371 │
                               │ novemvigintiseptingentillion          │ 4374 │
                               │ novemvigintiseptingentilliard         │ 4377 │
                               │ trigintaseptingentillion              │ 4380 │
                               │ trigintaseptingentilliard             │ 4383 │
                               │ untrigintaseptingentillion            │ 4386 │
                               │ untrigintaseptingentilliard           │ 4389 │
                               │ duotrigintaseptingentillion           │ 4392 │
                               │ duotrigintaseptingentilliard          │ 4395 │
                               │ trestrigintaseptingentillion          │ 4398 │
                               │ trestrigintaseptingentilliard         │ 4401 │
                               │ quattuortrigintaseptingentillion      │ 4404 │
                               │ quattuortrigintaseptingentilliard     │ 4407 │
                               │ quinquatrigintaseptingentillion       │ 4410 │
                               │ quinquatrigintaseptingentilliard      │ 4413 │
                               │ sestrigintaseptingentillion           │ 4416 │
                               │ sestrigintaseptingentilliard          │ 4419 │
                               │ septentrigintaseptingentillion        │ 4422 │
                               │ septentrigintaseptingentilliard       │ 4425 │
                               │ octotrigintaseptingentillion          │ 4428 │
                               │ octotrigintaseptingentilliard         │ 4431 │
                               │ noventrigintaseptingentillion         │ 4434 │
                               │ noventrigintaseptingentilliard        │ 4437 │
                               │ quadragintaseptingentillion           │ 4440 │
                               │ quadragintaseptingentilliard          │ 4443 │
                               │ unquadragintaseptingentillion         │ 4446 │
                               │ unquadragintaseptingentilliard        │ 4449 │
                               │ duoquadragintaseptingentillion        │ 4452 │
                               │ duoquadragintaseptingentilliard       │ 4455 │
                               │ tresquadragintaseptingentillion       │ 4458 │
                               │ tresquadragintaseptingentilliard      │ 4461 │
                               │ quattuorquadragintaseptingentillion   │ 4464 │
                               │ quattuorquadragintaseptingentilliard  │ 4467 │
                               │ quinquaquadragintaseptingentillion    │ 4470 │
                               │ quinquaquadragintaseptingentilliard   │ 4473 │
                               │ sesquadragintaseptingentillion        │ 4476 │
                               │ sesquadragintaseptingentilliard       │ 4479 │
                               │ septenquadragintaseptingentillion     │ 4482 │
                               │ septenquadragintaseptingentilliard    │ 4485 │
                               │ octoquadragintaseptingentillion       │ 4488 │
                               │ octoquadragintaseptingentilliard      │ 4491 │
                               │ novenquadragintaseptingentillion      │ 4494 │
                               │ novenquadragintaseptingentilliard     │ 4497 │
                               │ quinquagintaseptingentillion          │ 4500 │
                               │ quinquagintaseptingentilliard         │ 4503 │
                               │ unquinquagintaseptingentillion        │ 4506 │
                               │ unquinquagintaseptingentilliard       │ 4509 │
                               │ duoquinquagintaseptingentillion       │ 4512 │
                               │ duoquinquagintaseptingentilliard      │ 4515 │
                               │ tresquinquagintaseptingentillion      │ 4518 │
                               │ tresquinquagintaseptingentilliard     │ 4521 │
                               │ quattuorquinquagintaseptingentillion  │ 4524 │
                               │ quattuorquinquagintaseptingentilliard │ 4527 │
                               │ quinquaquinquagintaseptingentillion   │ 4530 │
                               │ quinquaquinquagintaseptingentilliard  │ 4533 │
                               │ sesquinquagintaseptingentillion       │ 4536 │
                               │ sesquinquagintaseptingentilliard      │ 4539 │
                               │ septenquinquagintaseptingentillion    │ 4542 │
                               │ septenquinquagintaseptingentilliard   │ 4545 │
                               │ octoquinquagintaseptingentillion      │ 4548 │
                               │ octoquinquagintaseptingentilliard     │ 4551 │
                               │ novenquinquagintaseptingentillion     │ 4554 │
                               │ novenquinquagintaseptingentilliard    │ 4557 │
                               │ sexagintaseptingentillion             │ 4560 │
                               │ sexagintaseptingentilliard            │ 4563 │
                               │ unsexagintaseptingentillion           │ 4566 │
                               │ unsexagintaseptingentilliard          │ 4569 │
                               │ duosexagintaseptingentillion          │ 4572 │
                               │ duosexagintaseptingentilliard         │ 4575 │
                               │ tresexagintaseptingentillion          │ 4578 │
                               │ tresexagintaseptingentilliard         │ 4581 │
                               │ quattuorsexagintaseptingentillion     │ 4584 │
                               │ quattuorsexagintaseptingentilliard    │ 4587 │
                               │ quinquasexagintaseptingentillion      │ 4590 │
                               │ quinquasexagintaseptingentilliard     │ 4593 │
                               │ sesexagintaseptingentillion           │ 4596 │
                               │ sesexagintaseptingentilliard          │ 4599 │
                               │ septensexagintaseptingentillion       │ 4602 │
                               │ septensexagintaseptingentilliard      │ 4605 │
                               │ octosexagintaseptingentillion         │ 4608 │
                               │ octosexagintaseptingentilliard        │ 4611 │
                               │ novensexagintaseptingentillion        │ 4614 │
                               │ novensexagintaseptingentilliard       │ 4617 │
                               │ septuagintaseptingentillion           │ 4620 │
                               │ septuagintaseptingentilliard          │ 4623 │
                               │ unseptuagintaseptingentillion         │ 4626 │
                               │ unseptuagintaseptingentilliard        │ 4629 │
                               │ duoseptuagintaseptingentillion        │ 4632 │
                               │ duoseptuagintaseptingentilliard       │ 4635 │
                               │ treseptuagintaseptingentillion        │ 4638 │
                               │ treseptuagintaseptingentilliard       │ 4641 │
                               │ quattuorseptuagintaseptingentillion   │ 4644 │
                               │ quattuorseptuagintaseptingentilliard  │ 4647 │
                               │ quinquaseptuagintaseptingentillion    │ 4650 │
                               │ quinquaseptuagintaseptingentilliard   │ 4653 │
                               │ seseptuagintaseptingentillion         │ 4656 │
                               │ seseptuagintaseptingentilliard        │ 4659 │
                               │ septenseptuagintaseptingentillion     │ 4662 │
                               │ septenseptuagintaseptingentilliard    │ 4665 │
                               │ octoseptuagintaseptingentillion       │ 4668 │
                               │ octoseptuagintaseptingentilliard      │ 4671 │
                               │ novenseptuagintaseptingentillion      │ 4674 │
                               │ novenseptuagintaseptingentilliard     │ 4677 │
                               │ octogintaseptingentillion             │ 4680 │
                               │ octogintaseptingentilliard            │ 4683 │
                               │ unoctogintaseptingentillion           │ 4686 │
                               │ unoctogintaseptingentilliard          │ 4689 │
                               │ duooctogintaseptingentillion          │ 4692 │
                               │ duooctogintaseptingentilliard         │ 4695 │
                               │ tresoctogintaseptingentillion         │ 4698 │
                               │ tresoctogintaseptingentilliard        │ 4701 │
                               │ quattuoroctogintaseptingentillion     │ 4704 │
                               │ quattuoroctogintaseptingentilliard    │ 4707 │
                               │ quinquaoctogintaseptingentillion      │ 4710 │
                               │ quinquaoctogintaseptingentilliard     │ 4713 │
                               │ sexoctogintaseptingentillion          │ 4716 │
                               │ sexoctogintaseptingentilliard         │ 4719 │
                               │ septemoctogintaseptingentillion       │ 4722 │
                               │ septemoctogintaseptingentilliard      │ 4725 │
                               │ octooctogintaseptingentillion         │ 4728 │
                               │ octooctogintaseptingentilliard        │ 4731 │
                               │ novemoctogintaseptingentillion        │ 4734 │
                               │ novemoctogintaseptingentilliard       │ 4737 │
                               │ nonagintaseptingentillion             │ 4740 │
                               │ nonagintaseptingentilliard            │ 4743 │
                               │ unnonagintaseptingentillion           │ 4746 │
                               │ unnonagintaseptingentilliard          │ 4749 │
                               │ duononagintaseptingentillion          │ 4752 │
                               │ duononagintaseptingentilliard         │ 4755 │
                               │ trenonagintaseptingentillion          │ 4758 │
                               │ trenonagintaseptingentilliard         │ 4761 │
                               │ quattuornonagintaseptingentillion     │ 4764 │
                               │ quattuornonagintaseptingentilliard    │ 4767 │
                               │ quinquanonagintaseptingentillion      │ 4770 │
                               │ quinquanonagintaseptingentilliard     │ 4773 │
                               │ senonagintaseptingentillion           │ 4776 │
                               │ senonagintaseptingentilliard          │ 4779 │
                               │ septenonagintaseptingentillion        │ 4782 │
                               │ septenonagintaseptingentilliard       │ 4785 │
                               │ octononagintaseptingentillion         │ 4788 │
                               │ octononagintaseptingentilliard        │ 4791 │
                               │ novenonagintaseptingentillion         │ 4794 │
                               │ novenonagintaseptingentilliard        │ 4797 │
                               │ octingentillion                       │ 4800 │
                               │ octingentilliard                      │ 4803 │
                               │ unoctingentillion                     │ 4806 │
                               │ unoctingentilliard                    │ 4809 │
                               │ duooctingentillion                    │ 4812 │
                               │ duooctingentilliard                   │ 4815 │
                               │ tresoctingentillion                   │ 4818 │
                               │ tresoctingentilliard                  │ 4821 │
                               │ quattuoroctingentillion               │ 4824 │
                               │ quattuoroctingentilliard              │ 4827 │
                               │ quinquaoctingentillion                │ 4830 │
                               │ quinquaoctingentilliard               │ 4833 │
                               │ sexoctingentillion                    │ 4836 │
                               │ sexoctingentilliard                   │ 4839 │
                               │ septemoctingentillion                 │ 4842 │
                               │ septemoctingentilliard                │ 4845 │
                               │ octooctingentillion                   │ 4848 │
                               │ octooctingentilliard                  │ 4851 │
                               │ novemoctingentillion                  │ 4854 │
                               │ novemoctingentilliard                 │ 4857 │
                               │ decioctingentillion                   │ 4860 │
                               │ decioctingentilliard                  │ 4863 │
                               │ undecioctingentillion                 │ 4866 │
                               │ undecioctingentilliard                │ 4869 │
                               │ duodecioctingentillion                │ 4872 │
                               │ duodecioctingentilliard               │ 4875 │
                               │ tredecioctingentillion                │ 4878 │
                               │ tredecioctingentilliard               │ 4881 │
                               │ quattuordecioctingentillion           │ 4884 │
                               │ quattuordecioctingentilliard          │ 4887 │
                               │ quinquadecioctingentillion            │ 4890 │
                               │ quinquadecioctingentilliard           │ 4893 │
                               │ sedecioctingentillion                 │ 4896 │
                               │ sedecioctingentilliard                │ 4899 │
                               │ septendecioctingentillion             │ 4902 │
                               │ septendecioctingentilliard            │ 4905 │
                               │ octodecioctingentillion               │ 4908 │
                               │ octodecioctingentilliard              │ 4911 │
                               │ novendecioctingentillion              │ 4914 │
                               │ novendecioctingentilliard             │ 4917 │
                               │ vigintioctingentillion                │ 4920 │
                               │ vigintioctingentilliard               │ 4923 │
                               │ unvigintioctingentillion              │ 4926 │
                               │ unvigintioctingentilliard             │ 4929 │
                               │ duovigintioctingentillion             │ 4932 │
                               │ duovigintioctingentilliard            │ 4935 │
                               │ tresvigintioctingentillion            │ 4938 │
                               │ tresvigintioctingentilliard           │ 4941 │
                               │ quattuorvigintioctingentillion        │ 4944 │
                               │ quattuorvigintioctingentilliard       │ 4947 │
                               │ quinquavigintioctingentillion         │ 4950 │
                               │ quinquavigintioctingentilliard        │ 4953 │
                               │ sesvigintioctingentillion             │ 4956 │
                               │ sesvigintioctingentilliard            │ 4959 │
                               │ septemvigintioctingentillion          │ 4962 │
                               │ septemvigintioctingentilliard         │ 4965 │
                               │ octovigintioctingentillion            │ 4968 │
                               │ octovigintioctingentilliard           │ 4971 │
                               │ novemvigintioctingentillion           │ 4974 │
                               │ novemvigintioctingentilliard          │ 4977 │
                               │ trigintaoctingentillion               │ 4980 │
                               │ trigintaoctingentilliard              │ 4983 │
                               │ untrigintaoctingentillion             │ 4986 │
                               │ untrigintaoctingentilliard            │ 4989 │
                               │ duotrigintaoctingentillion            │ 4992 │
                               │ duotrigintaoctingentilliard           │ 4995 │
                               │ trestrigintaoctingentillion           │ 4998 │
                               │ trestrigintaoctingentilliard          │ 5001 │
                               │ quattuortrigintaoctingentillion       │ 5004 │
                               │ quattuortrigintaoctingentilliard      │ 5007 │
                               │ quinquatrigintaoctingentillion        │ 5010 │
                               │ quinquatrigintaoctingentilliard       │ 5013 │
                               │ sestrigintaoctingentillion            │ 5016 │
                               │ sestrigintaoctingentilliard           │ 5019 │
                               │ septentrigintaoctingentillion         │ 5022 │
                               │ septentrigintaoctingentilliard        │ 5025 │
                               │ octotrigintaoctingentillion           │ 5028 │
                               │ octotrigintaoctingentilliard          │ 5031 │
                               │ noventrigintaoctingentillion          │ 5034 │
                               │ noventrigintaoctingentilliard         │ 5037 │
                               │ quadragintaoctingentillion            │ 5040 │
                               │ quadragintaoctingentilliard           │ 5043 │
                               │ unquadragintaoctingentillion          │ 5046 │
                               │ unquadragintaoctingentilliard         │ 5049 │
                               │ duoquadragintaoctingentillion         │ 5052 │
                               │ duoquadragintaoctingentilliard        │ 5055 │
                               │ tresquadragintaoctingentillion        │ 5058 │
                               │ tresquadragintaoctingentilliard       │ 5061 │
                               │ quattuorquadragintaoctingentillion    │ 5064 │
                               │ quattuorquadragintaoctingentilliard   │ 5067 │
                               │ quinquaquadragintaoctingentillion     │ 5070 │
                               │ quinquaquadragintaoctingentilliard    │ 5073 │
                               │ sesquadragintaoctingentillion         │ 5076 │
                               │ sesquadragintaoctingentilliard        │ 5079 │
                               │ septenquadragintaoctingentillion      │ 5082 │
                               │ septenquadragintaoctingentilliard     │ 5085 │
                               │ octoquadragintaoctingentillion        │ 5088 │
                               │ octoquadragintaoctingentilliard       │ 5091 │
                               │ novenquadragintaoctingentillion       │ 5094 │
                               │ novenquadragintaoctingentilliard      │ 5097 │
                               │ quinquagintaoctingentillion           │ 5100 │
                               │ quinquagintaoctingentilliard          │ 5103 │
                               │ unquinquagintaoctingentillion         │ 5106 │
                               │ unquinquagintaoctingentilliard        │ 5109 │
                               │ duoquinquagintaoctingentillion        │ 5112 │
                               │ duoquinquagintaoctingentilliard       │ 5115 │
                               │ tresquinquagintaoctingentillion       │ 5118 │
                               │ tresquinquagintaoctingentilliard      │ 5121 │
                               │ quattuorquinquagintaoctingentillion   │ 5124 │
                               │ quattuorquinquagintaoctingentilliard  │ 5127 │
                               │ quinquaquinquagintaoctingentillion    │ 5130 │
                               │ quinquaquinquagintaoctingentilliard   │ 5133 │
                               │ sesquinquagintaoctingentillion        │ 5136 │
                               │ sesquinquagintaoctingentilliard       │ 5139 │
                               │ septenquinquagintaoctingentillion     │ 5142 │
                               │ septenquinquagintaoctingentilliard    │ 5145 │
                               │ octoquinquagintaoctingentillion       │ 5148 │
                               │ octoquinquagintaoctingentilliard      │ 5151 │
                               │ novenquinquagintaoctingentillion      │ 5154 │
                               │ novenquinquagintaoctingentilliard     │ 5157 │
                               │ sexagintaoctingentillion              │ 5160 │
                               │ sexagintaoctingentilliard             │ 5163 │
                               │ unsexagintaoctingentillion            │ 5166 │
                               │ unsexagintaoctingentilliard           │ 5169 │
                               │ duosexagintaoctingentillion           │ 5172 │
                               │ duosexagintaoctingentilliard          │ 5175 │
                               │ tresexagintaoctingentillion           │ 5178 │
                               │ tresexagintaoctingentilliard          │ 5181 │
                               │ quattuorsexagintaoctingentillion      │ 5184 │
                               │ quattuorsexagintaoctingentilliard     │ 5187 │
                               │ quinquasexagintaoctingentillion       │ 5190 │
                               │ quinquasexagintaoctingentilliard      │ 5193 │
                               │ sesexagintaoctingentillion            │ 5196 │
                               │ sesexagintaoctingentilliard           │ 5199 │
                               │ septensexagintaoctingentillion        │ 5202 │
                               │ septensexagintaoctingentilliard       │ 5205 │
                               │ octosexagintaoctingentillion          │ 5208 │
                               │ octosexagintaoctingentilliard         │ 5211 │
                               │ novensexagintaoctingentillion         │ 5214 │
                               │ novensexagintaoctingentilliard        │ 5217 │
                               │ septuagintaoctingentillion            │ 5220 │
                               │ septuagintaoctingentilliard           │ 5223 │
                               │ unseptuagintaoctingentillion          │ 5226 │
                               │ unseptuagintaoctingentilliard         │ 5229 │
                               │ duoseptuagintaoctingentillion         │ 5232 │
                               │ duoseptuagintaoctingentilliard        │ 5235 │
                               │ treseptuagintaoctingentillion         │ 5238 │
                               │ treseptuagintaoctingentilliard        │ 5241 │
                               │ quattuorseptuagintaoctingentillion    │ 5244 │
                               │ quattuorseptuagintaoctingentilliard   │ 5247 │
                               │ quinquaseptuagintaoctingentillion     │ 5250 │
                               │ quinquaseptuagintaoctingentilliard    │ 5253 │
                               │ seseptuagintaoctingentillion          │ 5256 │
                               │ seseptuagintaoctingentilliard         │ 5259 │
                               │ septenseptuagintaoctingentillion      │ 5262 │
                               │ septenseptuagintaoctingentilliard     │ 5265 │
                               │ octoseptuagintaoctingentillion        │ 5268 │
                               │ octoseptuagintaoctingentilliard       │ 5271 │
                               │ novenseptuagintaoctingentillion       │ 5274 │
                               │ novenseptuagintaoctingentilliard      │ 5277 │
                               │ octogintaoctingentillion              │ 5280 │
                               │ octogintaoctingentilliard             │ 5283 │
                               │ unoctogintaoctingentillion            │ 5286 │
                               │ unoctogintaoctingentilliard           │ 5289 │
                               │ duooctogintaoctingentillion           │ 5292 │
                               │ duooctogintaoctingentilliard          │ 5295 │
                               │ tresoctogintaoctingentillion          │ 5298 │
                               │ tresoctogintaoctingentilliard         │ 5301 │
                               │ quattuoroctogintaoctingentillion      │ 5304 │
                               │ quattuoroctogintaoctingentilliard     │ 5307 │
                               │ quinquaoctogintaoctingentillion       │ 5310 │
                               │ quinquaoctogintaoctingentilliard      │ 5313 │
                               │ sexoctogintaoctingentillion           │ 5316 │
                               │ sexoctogintaoctingentilliard          │ 5319 │
                               │ septemoctogintaoctingentillion        │ 5322 │
                               │ septemoctogintaoctingentilliard       │ 5325 │
                               │ octooctogintaoctingentillion          │ 5328 │
                               │ octooctogintaoctingentilliard         │ 5331 │
                               │ novemoctogintaoctingentillion         │ 5334 │
                               │ novemoctogintaoctingentilliard        │ 5337 │
                               │ nonagintaoctingentillion              │ 5340 │
                               │ nonagintaoctingentilliard             │ 5343 │
                               │ unnonagintaoctingentillion            │ 5346 │
                               │ unnonagintaoctingentilliard           │ 5349 │
                               │ duononagintaoctingentillion           │ 5352 │
                               │ duononagintaoctingentilliard          │ 5355 │
                               │ trenonagintaoctingentillion           │ 5358 │
                               │ trenonagintaoctingentilliard          │ 5361 │
                               │ quattuornonagintaoctingentillion      │ 5364 │
                               │ quattuornonagintaoctingentilliard     │ 5367 │
                               │ quinquanonagintaoctingentillion       │ 5370 │
                               │ quinquanonagintaoctingentilliard      │ 5373 │
                               │ senonagintaoctingentillion            │ 5376 │
                               │ senonagintaoctingentilliard           │ 5379 │
                               │ septenonagintaoctingentillion         │ 5382 │
                               │ septenonagintaoctingentilliard        │ 5385 │
                               │ octononagintaoctingentillion          │ 5388 │
                               │ octononagintaoctingentilliard         │ 5391 │
                               │ novenonagintaoctingentillion          │ 5394 │
                               │ novenonagintaoctingentilliard         │ 5397 │
                               │ nongentillion                         │ 5400 │
                               │ nongentilliard                        │ 5403 │
                               │ unnongentillion                       │ 5406 │
                               │ unnongentilliard                      │ 5409 │
                               │ duonongentillion                      │ 5412 │
                               │ duonongentilliard                     │ 5415 │
                               │ trenongentillion                      │ 5418 │
                               │ trenongentilliard                     │ 5421 │
                               │ quattuornongentillion                 │ 5424 │
                               │ quattuornongentilliard                │ 5427 │
                               │ quinquanongentillion                  │ 5430 │
                               │ quinquanongentilliard                 │ 5433 │
                               │ senongentillion                       │ 5436 │
                               │ senongentilliard                      │ 5439 │
                               │ septenongentillion                    │ 5442 │
                               │ septenongentilliard                   │ 5445 │
                               │ octonongentillion                     │ 5448 │
                               │ octonongentilliard                    │ 5451 │
                               │ novenongentillion                     │ 5454 │
                               │ novenongentilliard                    │ 5457 │
                               │ decinongentillion                     │ 5460 │
                               │ decinongentilliard                    │ 5463 │
                               │ undecinongentillion                   │ 5466 │
                               │ undecinongentilliard                  │ 5469 │
                               │ duodecinongentillion                  │ 5472 │
                               │ duodecinongentilliard                 │ 5475 │
                               │ tredecinongentillion                  │ 5478 │
                               │ tredecinongentilliard                 │ 5481 │
                               │ quattuordecinongentillion             │ 5484 │
                               │ quattuordecinongentilliard            │ 5487 │
                               │ quinquadecinongentillion              │ 5490 │
                               │ quinquadecinongentilliard             │ 5493 │
                               │ sedecinongentillion                   │ 5496 │
                               │ sedecinongentilliard                  │ 5499 │
                               │ septendecinongentillion               │ 5502 │
                               │ septendecinongentilliard              │ 5505 │
                               │ octodecinongentillion                 │ 5508 │
                               │ octodecinongentilliard                │ 5511 │
                               │ novendecinongentillion                │ 5514 │
                               │ novendecinongentilliard               │ 5517 │
                               │ vigintinongentillion                  │ 5520 │
                               │ vigintinongentilliard                 │ 5523 │
                               │ unvigintinongentillion                │ 5526 │
                               │ unvigintinongentilliard               │ 5529 │
                               │ duovigintinongentillion               │ 5532 │
                               │ duovigintinongentilliard              │ 5535 │
                               │ tresvigintinongentillion              │ 5538 │
                               │ tresvigintinongentilliard             │ 5541 │
                               │ quattuorvigintinongentillion          │ 5544 │
                               │ quattuorvigintinongentilliard         │ 5547 │
                               │ quinquavigintinongentillion           │ 5550 │
                               │ quinquavigintinongentilliard          │ 5553 │
                               │ sesvigintinongentillion               │ 5556 │
                               │ sesvigintinongentilliard              │ 5559 │
                               │ septemvigintinongentillion            │ 5562 │
                               │ septemvigintinongentilliard           │ 5565 │
                               │ octovigintinongentillion              │ 5568 │
                               │ octovigintinongentilliard             │ 5571 │
                               │ novemvigintinongentillion             │ 5574 │
                               │ novemvigintinongentilliard            │ 5577 │
                               │ trigintanongentillion                 │ 5580 │
                               │ trigintanongentilliard                │ 5583 │
                               │ untrigintanongentillion               │ 5586 │
                               │ untrigintanongentilliard              │ 5589 │
                               │ duotrigintanongentillion              │ 5592 │
                               │ duotrigintanongentilliard             │ 5595 │
                               │ trestrigintanongentillion             │ 5598 │
                               │ trestrigintanongentilliard            │ 5601 │
                               │ quattuortrigintanongentillion         │ 5604 │
                               │ quattuortrigintanongentilliard        │ 5607 │
                               │ quinquatrigintanongentillion          │ 5610 │
                               │ quinquatrigintanongentilliard         │ 5613 │
                               │ sestrigintanongentillion              │ 5616 │
                               │ sestrigintanongentilliard             │ 5619 │
                               │ septentrigintanongentillion           │ 5622 │
                               │ septentrigintanongentilliard          │ 5625 │
                               │ octotrigintanongentillion             │ 5628 │
                               │ octotrigintanongentilliard            │ 5631 │
                               │ noventrigintanongentillion            │ 5634 │
                               │ noventrigintanongentilliard           │ 5637 │
                               │ quadragintanongentillion              │ 5640 │
                               │ quadragintanongentilliard             │ 5643 │
                               │ unquadragintanongentillion            │ 5646 │
                               │ unquadragintanongentilliard           │ 5649 │
                               │ duoquadragintanongentillion           │ 5652 │
                               │ duoquadragintanongentilliard          │ 5655 │
                               │ tresquadragintanongentillion          │ 5658 │
                               │ tresquadragintanongentilliard         │ 5661 │
                               │ quattuorquadragintanongentillion      │ 5664 │
                               │ quattuorquadragintanongentilliard     │ 5667 │
                               │ quinquaquadragintanongentillion       │ 5670 │
                               │ quinquaquadragintanongentilliard      │ 5673 │
                               │ sesquadragintanongentillion           │ 5676 │
                               │ sesquadragintanongentilliard          │ 5679 │
                               │ septenquadragintanongentillion        │ 5682 │
                               │ septenquadragintanongentilliard       │ 5685 │
                               │ octoquadragintanongentillion          │ 5688 │
                               │ octoquadragintanongentilliard         │ 5691 │
                               │ novenquadragintanongentillion         │ 5694 │
                               │ novenquadragintanongentilliard        │ 5697 │
                               │ quinquagintanongentillion             │ 5700 │
                               │ quinquagintanongentilliard            │ 5703 │
                               │ unquinquagintanongentillion           │ 5706 │
                               │ unquinquagintanongentilliard          │ 5709 │
                               │ duoquinquagintanongentillion          │ 5712 │
                               │ duoquinquagintanongentilliard         │ 5715 │
                               │ tresquinquagintanongentillion         │ 5718 │
                               │ tresquinquagintanongentilliard        │ 5721 │
                               │ quattuorquinquagintanongentillion     │ 5724 │
                               │ quattuorquinquagintanongentilliard    │ 5727 │
                               │ quinquaquinquagintanongentillion      │ 5730 │
                               │ quinquaquinquagintanongentilliard     │ 5733 │
                               │ sesquinquagintanongentillion          │ 5736 │
                               │ sesquinquagintanongentilliard         │ 5739 │
                               │ septenquinquagintanongentillion       │ 5742 │
                               │ septenquinquagintanongentilliard      │ 5745 │
                               │ octoquinquagintanongentillion         │ 5748 │
                               │ octoquinquagintanongentilliard        │ 5751 │
                               │ novenquinquagintanongentillion        │ 5754 │
                               │ novenquinquagintanongentilliard       │ 5757 │
                               │ sexagintanongentillion                │ 5760 │
                               │ sexagintanongentilliard               │ 5763 │
                               │ unsexagintanongentillion              │ 5766 │
                               │ unsexagintanongentilliard             │ 5769 │
                               │ duosexagintanongentillion             │ 5772 │
                               │ duosexagintanongentilliard            │ 5775 │
                               │ tresexagintanongentillion             │ 5778 │
                               │ tresexagintanongentilliard            │ 5781 │
                               │ quattuorsexagintanongentillion        │ 5784 │
                               │ quattuorsexagintanongentilliard       │ 5787 │
                               │ quinquasexagintanongentillion         │ 5790 │
                               │ quinquasexagintanongentilliard        │ 5793 │
                               │ sesexagintanongentillion              │ 5796 │
                               │ sesexagintanongentilliard             │ 5799 │
                               │ septensexagintanongentillion          │ 5802 │
                               │ septensexagintanongentilliard         │ 5805 │
                               │ octosexagintanongentillion            │ 5808 │
                               │ octosexagintanongentilliard           │ 5811 │
                               │ novensexagintanongentillion           │ 5814 │
                               │ novensexagintanongentilliard          │ 5817 │
                               │ septuagintanongentillion              │ 5820 │
                               │ septuagintanongentilliard             │ 5823 │
                               │ unseptuagintanongentillion            │ 5826 │
                               │ unseptuagintanongentilliard           │ 5829 │
                               │ duoseptuagintanongentillion           │ 5832 │
                               │ duoseptuagintanongentilliard          │ 5835 │
                               │ treseptuagintanongentillion           │ 5838 │
                               │ treseptuagintanongentilliard          │ 5841 │
                               │ quattuorseptuagintanongentillion      │ 5844 │
                               │ quattuorseptuagintanongentilliard     │ 5847 │
                               │ quinquaseptuagintanongentillion       │ 5850 │
                               │ quinquaseptuagintanongentilliard      │ 5853 │
                               │ seseptuagintanongentillion            │ 5856 │
                               │ seseptuagintanongentilliard           │ 5859 │
                               │ septenseptuagintanongentillion        │ 5862 │
                               │ septenseptuagintanongentilliard       │ 5865 │
                               │ octoseptuagintanongentillion          │ 5868 │
                               │ octoseptuagintanongentilliard         │ 5871 │
                               │ novenseptuagintanongentillion         │ 5874 │
                               │ novenseptuagintanongentilliard        │ 5877 │
                               │ octogintanongentillion                │ 5880 │
                               │ octogintanongentilliard               │ 5883 │
                               │ unoctogintanongentillion              │ 5886 │
                               │ unoctogintanongentilliard             │ 5889 │
                               │ duooctogintanongentillion             │ 5892 │
                               │ duooctogintanongentilliard            │ 5895 │
                               │ tresoctogintanongentillion            │ 5898 │
                               │ tresoctogintanongentilliard           │ 5901 │
                               │ quattuoroctogintanongentillion        │ 5904 │
                               │ quattuoroctogintanongentilliard       │ 5907 │
                               │ quinquaoctogintanongentillion         │ 5910 │
                               │ quinquaoctogintanongentilliard        │ 5913 │
                               │ sexoctogintanongentillion             │ 5916 │
                               │ sexoctogintanongentilliard            │ 5919 │
                               │ septemoctogintanongentillion          │ 5922 │
                               │ septemoctogintanongentilliard         │ 5925 │
                               │ octooctogintanongentillion            │ 5928 │
                               │ octooctogintanongentilliard           │ 5931 │
                               │ novemoctogintanongentillion           │ 5934 │
                               │ novemoctogintanongentilliard          │ 5937 │
                               │ nonagintanongetillion                 │ 5940 │
                               │ nonagintanongetilliard                │ 5943 │
                               │ unnonagintanongentillion              │ 5946 │
                               │ unnonagintanongentilliard             │ 5949 │
                               │ duononagintanongentillion             │ 5952 │
                               │ duononagintanongentilliard            │ 5955 │
                               │ trenonagintanongentillion             │ 5958 │
                               │ trenonagintanongentilliard            │ 5961 │
                               │ quattuornonagintanongentillion        │ 5964 │
                               │ quattuornonagintanongentilliard       │ 5967 │
                               │ quinquaquadragintanongentillion       │ 5670 │
                               │ quinquaquadragintanongentilliard      │ 5673 │
                               │ sesquadragintanongentillion           │ 5676 │
                               │ sesquadragintanongentilliard          │ 5679 │
                               │ septenquadragintanongentillion        │ 5682 │
                               │ septenquadragintanongentilliard       │ 5685 │
                               │ octoquadragintanongentillion          │ 5688 │
                               │ octoquadragintanongentilliard         │ 5691 │
                               │ novenquadragintanongentillion         │ 5694 │
                               │ novenquadragintanongentilliard        │ 5697 │
                               │ quinquagintanongentillion             │ 5700 │
                               │ quinquagintanongentilliard            │ 5703 │
                               │ unquinquagintanongentillion           │ 5706 │
                               │ unquinquagintanongentilliard          │ 5709 │
                               │ duoquinquagintanongentillion          │ 5712 │
                               │ duoquinquagintanongentilliard         │ 5715 │
                               │ tresquinquagintanongentillion         │ 5718 │
                               │ tresquinquagintanongentilliard        │ 5721 │
                               │ quattuorquinquagintanongentillion     │ 5724 │
                               │ quattuorquinquagintanongentilliard    │ 5727 │
                               │ quinquaquinquagintanongentillion      │ 5730 │
                               │ quinquaquinquagintanongentilliard     │ 5733 │
                               │ sesquinquagintanongentillion          │ 5736 │
                               │ sesquinquagintanongentilliard         │ 5739 │
                               │ septenquinquagintanongentillion       │ 5742 │
                               │ septenquinquagintanongentilliard      │ 5745 │
                               │ octoquinquagintanongentillion         │ 5748 │
                               │ octoquinquagintanongentilliard        │ 5751 │
                               │ novenquinquagintanongentillion        │ 5754 │
                               │ novenquinquagintanongentilliard       │ 5757 │
                               │ sexagintanongentillion                │ 5760 │
                               │ sexagintanongentilliard               │ 5763 │
                               │ unsexagintanongentillion              │ 5766 │
                               │ unsexagintanongentilliard             │ 5769 │
                               │ duosexagintanongentillion             │ 5772 │
                               │ duosexagintanongentilliard            │ 5775 │
                               │ tresexagintanongentillion             │ 5778 │
                               │ tresexagintanongentilliard            │ 5781 │
                               │ quattuorsexagintanongentillion        │ 5784 │
                               │ quattuorsexagintanongentilliard       │ 5787 │
                               │ quinquasexagintanongentillion         │ 5790 │
                               │ quinquasexagintanongentilliard        │ 5793 │
                               │ sesexagintanongentillion              │ 5796 │
                               │ sesexagintanongentilliard             │ 5799 │
                               │ septensexagintanongentillion          │ 5802 │
                               │ septensexagintanongentilliard         │ 5805 │
                               │ octosexagintanongentillion            │ 5808 │
                               │ octosexagintanongentilliard           │ 5811 │
                               │ novensexagintanongentillion           │ 5814 │
                               │ novensexagintanongentilliard          │ 5817 │
                               │ septuagintanongentillion              │ 5820 │
                               │ septuagintanongentilliard             │ 5823 │
                               │ unseptuagintanongentillion            │ 5826 │
                               │ unseptuagintanongentilliard           │ 5829 │
                               │ duoseptuagintanongentillion           │ 5832 │
                               │ duoseptuagintanongentilliard          │ 5835 │
                               │ treseptuagintanongentillion           │ 5838 │
                               │ treseptuagintanongentilliard          │ 5841 │
                               │ quattuorseptuagintanongentillion      │ 5844 │
                               │ quattuorseptuagintanongentilliard     │ 5847 │
                               │ quinquaseptuagintanongentillion       │ 5850 │
                               │ quinquaseptuagintanongentilliard      │ 5853 │
                               │ seseptuagintanongentillion            │ 5856 │
                               │ seseptuagintanongentilliard           │ 5859 │
                               │ septenseptuagintanongentillion        │ 5862 │
                               │ septenseptuagintanongentilliard       │ 5865 │
                               │ octoseptuagintanongentillion          │ 5868 │
                               │ octoseptuagintanongentilliard         │ 5871 │
                               │ novenseptuagintanongentillion         │ 5874 │
                               │ novenseptuagintanongentilliard        │ 5877 │
                               │ octogintanongentillion                │ 5880 │
                               │ octogintanongentilliard               │ 5883 │
                               │ unoctogintanongentillion              │ 5886 │
                               │ unoctogintanongentilliard             │ 5889 │
                               │ duooctogintanongentillion             │ 5892 │
                               │ duooctogintanongentilliard            │ 5895 │
                               │ tresoctogintanongentillion            │ 5898 │
                               │ tresoctogintanongentilliard           │ 5901 │
                               │ quattuoroctogintanongentillion        │ 5904 │
                               │ quattuoroctogintanongentilliard       │ 5907 │
                               │ quinquaoctogintanongentillion         │ 5910 │
                               │ quinquaoctogintanongentilliard        │ 5913 │
                               │ sexoctogintanongentillion             │ 5916 │
                               │ sexoctogintanongentilliard            │ 5919 │
                               │ septemoctogintanongentillion          │ 5922 │
                               │ septemoctogintanongentilliard         │ 5925 │
                               │ octooctogintanongentillion            │ 5928 │
                               │ octooctogintanongentilliard           │ 5931 │
                               │ novemoctogintanongentillion           │ 5934 │
                               │ novemoctogintanongentilliard          │ 5937 │
                               │ nonagintanongetillion                 │ 5940 │
                               │ nonagintanongetilliard                │ 5943 │
                               │ unnonagintanongentillion              │ 5946 │
                               │ unnonagintanongentilliard             │ 5949 │
                               │ duononagintanongentillion             │ 5952 │
                               │ duononagintanongentilliard            │ 5955 │
                               │ trenonagintanongentillion             │ 5958 │
                               │ trenonagintanongentilliard            │ 5961 │
                               │ quattuornonagintanongentillion        │ 5964 │
                               │ quattuornonagintanongentilliard       │ 5967 │
                               │ quinquanonagintanongentillion         │ 5970 │
                               │ quinquanonagintanongentilliard        │ 5973 │
                               │ senonagintanongentillion              │ 5976 │
                               │ senonagintanongentilliard             │ 5979 │
                               │ septenonagintanongentillion           │ 5982 │
                               │ septenonagintanongentilliard          │ 5985 │
                               │ novenonagintanongentillion            │ 5988 │
                               │ novenonagintanongentilliard           │ 5991 │
                               │ octononagintanongentillion            │ 5994 │
                               │ octononagintanongentilliard           │ 5997 │
                               └───────────────────────────────────────┴──────┘



┌──────────────────────────────────────────────────────────────────────────────┐
│   names of some larger numbers that  aren't  used in the  $SPELL#  program.  │
├────────────────────┬────────────────┬────────────────────────────────────────┤
│ googol             │ 10**100        │ 1 followed by a hundred zeroes         │
│ asankhyeyu         │ 10**140        │ 1 followed by a hundred forty zeroes   │
│ millimillimillion  │ 10**(6billion) │ 1 followed by 6,000,000,000 zeroes     │
│ milli─millillion   │ 10**3000003    │                                        │
│ skew   (also known │ 10**10**10**34 │                                        │
│  as Skewe's number)│                │                                        │
│ googolplex         │ 10**googol     │ 1 followed by googol of zeroes         │
│                    │                │                                        │
│ prime─vigesimo─    │ 10**366        │                                        │
│         centillion │                │                                        │
│                    │                │                                        │
│ Graham's number    │ (see below)    │                                        │
└────────────────────┴────────────────┴────────────────────────────────────────┘



Graham's number is so large that it needs a special notation to express:

     3################################################################3

where:

3#3  = 3**3 = 27
3##3 = 3#(3#3) = 3#27 = 7,625,597,484,987
3###3= 3##(3##3) = 3##7,625,597,484,987 =3#(7,625,597,484,987#7,625,597,484,987)
        (this expression is too large to express as a number)



┌──────────────────────────────────────────────────────────────────────────────┐
│                                                                              │
│ The largest number─word used by the early Romans was  "mille",  meaning      │
│ thousand.                                                                    │
│                                                                              │
│ A distance of one thousand paces was "milia";  which we used to call a mile. │
│ This Roman mile was eactly 5,000 feet  (a pace being 2 1/2 feet.   Later,    │
│ the English mile was used because 5,280 feet better related to the length of │
│ an English furlong.   An furlong  (1/8 of a mile or 660 feet) was the length │
│ of a furrow on a square field of ten acres.                                  │
│                                                                              │
│ The suffix  "ione"  implies an unusually large size.  So when a number       │
│ bigger than one thousand (mille) was needed,  the word chosen was:           │
│                                                                              │
│                        "mille ione"  ────►  "million"                        │
│                                                                              │
│ The Latin prefix  "bi"  means  twice,  so the word with twice as many zeros  │
│ as  milli  leads use to  "billion".                                          │
│                                                                              │
│ For bigger numbers, the Latin prefixes are used:                             │
│                         tri, quad, quint, sext, sept, etc.                   │
│                                                                              │
└──────────────────────────────────────────────────────────────────────────────┘

                                       Ω

```


[[Category:REXX library routines]]
