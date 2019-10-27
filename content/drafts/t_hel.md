+++
title = "$T.HEL"
description = ""
date = 2017-09-07T00:01:11Z
aliases = []
[extra]
id = 12873
[taxonomies]
categories = []
tags = []
+++

==$T.HEL==
The   '''$T.HEL'''   is the HELp (documentation) for the   '''$T.REX'''   (REXX) program.

```txt

The   $T   command is used to display  text to the console with various options,
some of which are: clearing the screen, specifing all (or parts of) text in hex,
showing the text in a box,  writing the text to any line on the screen, sounding
the bell (alarm),  typing blank lines  before  and/or  after  the text,  showing
parenthesized portions in a brite white (or any color),  or showing the text and
waiting for any key to be pressed    (in CMS, the  ENTER  key must be used).
Note that colors and highlighting are supported under DOS (and/or under the
various Windows)  if  and  only if  the  ANSI.SYS  driver is installed.

╔══════════════════════════════════════════════════════════════════════════════╗
║                                                                              ║
║    $T    yyy  yyy  yyy   {.OOO=yyy}   yyy  yyy  yyy  ...                     ║
║                                                                              ║
║           ?                                                                  ║
║           ?AUTHOR                                                            ║
║           ?FLOW R                                                            ║
║           ?SAMPLES                                                           ║
║                                                                              ║
╟──────────────── where  .OOO  is any or all of  (note the uppercase): ────────╢
║                                                                              ║
║# blank lines typed  after message   .A=nn                                    ║
║character used as a blank for   .A   .AB=c │ hh                               ║
║# of beeps  (using the PC speaker)   .B=nn                                    ║
║char used for blockchr  background   .BB=c │ hh                               ║
║character(s) to use when  blocking   .BC=ccc │ 'hhhh'x                        ║
║duration in seconds for the  beeps   .BD=nnn │ nnn,nnn,nnn,...                ║
║bin strings ───> char  used in msg   .BIN=bbbbbbbb │ bbbbbbbb,bbbbbbbb, ...   ║
║frequency of the  sound  for beeps   .BF=nnn │ nnn,nnn,nnn,...                ║
║# lines  used  for  blocked  chars   .BLOCK=nn                                ║
║draws a  box  around  the  message   .BOX= │ cccccccc │ *NONE*                ║
║# blanks between each blocked char   .BS=nn                                   ║
║specifies  the  color  of the  msg   .C=color                                 ║
║highlights, reverses, blinks,  ...   .D=disposition                           ║
║dec strings ───> char  used in msg   .DEC=nnn │ nnn,nnn,nnn,...               ║
║# empty lines  (also inside boxes)   .E=nn                                    ║
║character used as a blank for   .E   .EB=c │ hh                               ║
║like .F but doesn't build DIR tree   .EF=fff │ fn,ft │ fn,ft,fm               ║
║following  .OOO opts  to be in msg   .END=1                                   ║
║also  writes msg to a  (disk) file   .F=fff │ fn,ft │ fn,ft,fm                ║
║highlight color of text inside  ()   .H=color                                 ║
║highlight color of text inside  []   .H[]=color                               ║
║highlight color of text inside  {}   .H{}=color                               ║
║hex strings ───> char  used in msg   .HEX=hh │ hhhh │ hh,hh,hh,,,,            ║
║indentation  used by  all messages   .I=nn                                    ║
║character used as a blank for   .I   .IB=c │ hh                               ║
║invert  bits for  ccc   characters   .INV=ccc                                 ║
║left/center/right    justification   .J=A | L │ R │ C │ J │ N                 ║
║karate chop  msg  into smaller mgs   .K=ccc  │ 'hhhh'x                        ║
║destructive karate chops  (delete)   .KD=ccc │ 'hhhh'x                        ║
║# blank lines between chopped msgs   .KS=nn                                   ║
║character used as a blank for  .KS   .KSB=c │ hh                              ║
║which line on  screen to write msg   .L=nn │ M │ M+nn │ M-nn │ * │ *+nn │ *-nn║
║only  write some lines  or  a line   .O=nnn                                   ║
║# blank lines  preceeding  message   .P=nn                                    ║
║character used as a blank for   .P   .PB=c │ hh                               ║
║supress typing,  but  not  to disk   .Q=1                                     ║
║a method of commenting  $T options   .REM=ccc                                 ║
║# of times  the msg is  replicated   .R=nnn                                   ║
║show 1,2,3─line ruler  bef│aft msg   .RULER=* │ nnn │ -nnn │ +nnn             ║
║char used as a blank in the  ruler   .RULERB=c │ hh                           ║
║supress writing of  some/a line(s)   .S=nnn                                   ║
║show 1,2,3─line scale  bef│aft msg   .SCALE=* │ nnn │ -nnn │ +nnn             ║
║char used as a blank in the  scale   .SCALEB=c │ hh                           ║
║char used as a  dot  in the  scale   .SCALED=c │ hh                           ║
║char used as a plus  in tge  scale   .SCALEP=c │ hh                           ║
║# of times  the message is written   .T=nnn                                   ║
║# blank lines between  .T= writing   .TS=nn                                   ║
║character used as a blank for  .TS   .TSB=c │ hh                              ║
║uses the  case  specified  for msg   .U=A │ L │ M │ U                         ║
║character used as a blank in   msg   .UB =c │ hh                              ║
║use char as the dot for  .OOO opts   .USE=c │ hh                              ║
║translate all chars cc─>CC  in msg   .UT=ccCC  │ 'hhhhhhhh'x                  ║
║how  the msg is  viewed  (written)   .V=Normal │ Reverse                      ║
║writes waitmsg and waits for input   .W=waitmsg                               ║
║character used as a blank for   .W   .WB=c │ hh                               ║
║add eXtra  blanks  to the msg line   .X=nn                                    ║
║character used as a blank for   .X   .XB=c │ hh                               ║
║# seconds delay  after writing msg   .Z=nn                                    ║
╚══════════════════════════════════════════════════════════════════════════════╝

───where all arguments are optional, and  the      .OPTION    options must be
   capitalized  and  prefixed by a period (.)

?          shows this help file              (press  ESC  to quit when viewing).

?AUTHOR    shows the author of this program.

           The author is shown in a yellow box (if color is supported),  along
           with the author's phone number  and  E-mail address.

?FLOW      shows the external execution flow of this program.

?SAMPLES   shows some sample uses            (press  ESC  to quit when viewing).

yyy        is a character string or strings that are to be displayed.  It may be
           ommitted (blank),  in which case nothing is displayed.   Imbedded
           blanks are retained, but leading and trailing blanks are stripped  (a
           function of the operating system).    Leading blanks can be "changed"
           into  imbedded blanks by prefixing the text with an superfous option.
           I.E.:   $T .A=0      yowsa!
           Another way to  specify leading  blanks  is to use the  indent  (.I=)
           option,  and trailing blanks can be specified via the   .X=   option.
           Any characters of the value '00'x are translated to blanks.


───note that any  .OOO  letters before the equal sign (=) must be in uppercase.


.A=nn      specifies the number of blank lines written after the message text is
           displayed.
           If  nn   is negative, the positive value of it is used to cause blank
           lines to be written  before the message also  (this is in addition to
           the  .P  value).      nn   can be from  -99 ──> 99    (default is 0).

           nn   can also be specified as: *         (screen depth)
                                          *+nn      (screen depth + nn)
                                          *-nn      (screen depth - nn)
                                          M+nn      (half screen +nn)
                                          M-nn      (half screen -nn)
                                          */nn      (screen depth / nn), rounded
                                                                            down
.AB=c
.AB=hh     the character used as a blank for the  .A   option  (displaying blank
           lines after the text).   It can be specified as a single character or
           as a hexadecimal pair.     The default is a blank.

.B=nn      sounds the  bell (beep)    nn    times    before   the text is shown.
           There is a one─tenth second delay between multiple beeps.
           nnn  can be from   -99 ──> 99,   the default is  0.
           If   nn    is negative, the bell is sounded   nn   times  before  and
           after the message   (the positive value of  nn   is used).   There is
           a one second delay after the text is displayed and before the "after"
           beeps are sounded.

.BB=c
.BB=hh     the character used for the background when the text is  blocked  (via
           the   .BLOCK=   option).   The default is a blank.

.BC=ccc
.BC='hhhh'x  the character(s) to be used if the text is blocked (via the .BLOCK=
           option).   The default is to use the character that is being blocked.
           The  .BC   character(s)  are concatenated to itself to yield a string
           long enough to match the (message) text specified.

.BD=nnn,nnn,nnn,...
.BD=nnn    specifies the duration in seconds for the beep (sound).
           nnn  can be from   .1 ──> 9            The default is  .2  (seconds).
           Multiple durations may be specified (for multiple frequencies.)

.BF=nnn,nnn,nnn,...
.BF=nnn    specifies the frequency (of the sound) of the beep.
           nnn  can be from   1 ──> 20000         The default is   800  (hertz).
           Multiple frequencies may be specified (with multiple durations).

.BIN=bbbbbbbb
.BIN=bbbbbbbb,bbbbbbbb,bbbbbbbb,...
.BIN=bbbbbbbbbbbbbbbb        is a binary string  and  bbbbbbbb  should be binary
           digits (only zeroes and ones).    No leading,  trailing,  or imbedded
           blanks are allowed within the binary string(s).     Commas (,) may be
           used for readability.   The binary string is converted to a character
           string.
           Any number of   .BIN=   options may be specified.

.BLOCK=-nnn
.BLOCK=nnn if  nnn  is positive,  it indicates the number of blocked lines to be
           generated of the blocked (message) text.
           If  nnn  is negative,  it indicates to only display the (positive)
           nth  block text line.              nnn     can range from  -1  to  12
           The default is  0   (don't block the text).

.BOX=(none)
.BOX=(NONE)   indicates that no box is to be used.

.BOX='HHHHHHHHHHHHHHHH'X
.BOX="hhhhhhhhhhhhhhhh"x
.BOX=cccccccc
.BOX=      indicates the text is to be boxed  (the text will have a  leading and
           trailing blank prefixed and appended to it to make the text easier to
           read within the box).   If the boxchars  (cccccccc)  are omitted, the
           boxchars used  are the ones used in this documention   (see the first
           part of this file).       The number of boxchars can be from  one  to
           eight   (or  sixteen,  if specified in hexadecimal).

           'hhhh...'x   or   "HHHH..."X   are the boxcharacters specified in hex
           where   hh   are pairs of hexadecimal digits (0──>9, a──>f, or A──>F)
           the  X  suffix may be upper/lower case,    hh   can be in mixed case.
           Note that either a paired single (') or double quote (") may be used.

           If  boxchars (cccccccc)  are specified,  the  order  of selection is:

                 1st character is the   top   left corner
                 2nd     "     "   "    top   side
                 3rd     "     "   "    top  right corner
                 4th     "     "   "         right side
                 5th     "     "   "  bottom right corner
                 6th     "     "   "  bottom  side
                 7th     "     "   "  bottom right corner
                 8th     "     "   "          left side

           If there are less than eight  boxchars (cccccccc) specified, the last
           character specified is used for the remainder of the boxchars.  Thus,
           a single plus sign (+)  would be used for all eight sides of the box.

           If there are leading blanks specified  before  the actual body of the
           text, these blanks are paired on the right (end) of the text so as to
           make the text in the box balanced.

.BS=nn     spacing between each blocked character  (of the message text,  if the
           text is blocked via the   .BLOCK   option).        The default is  2.
           nn    can be from  -11  to  screenwidth,  a negative value will cause
           overlapping of blocked characters.

           nn   can also be specified as: *         (screen width)
                                          *+nn      (screen width + nn)
                                          *-nn      (screen width - nn)
                                          M+nn      (half of width +nn)
                                          M-nn      (half of width -nn)
                                          */nn      (screen width / nn), rounded
                                                                            down

.C=color   shows the text in color;  the color specified must be capitalized and
           be one of:

                 RED     PINK     GREEN     TURQUOISE     BLUE WHITE YELLOW GREY
             DARKRED DARKPINK DARKGREEN DARKTURQUOISE DARKBLUE BRITE BROWN  GRAY

           CYAN can be used instead of TURQUOISE,  and  MAGENTA instead of PINK.

           Note that some DOS color drivers don't support GRAY (shown in black).

           If under CMS, all dark colors are shown as  regular colors,   and the
           following colors are translated:    brown ──> yellow
                                               black ──> white
                                               grey  ──> white

           Only the first 3 characters of any color  need be  specified,  except
           for the  DARK  colors, which need seven,   and   GREY  which can't be
           abbreviated    (it would conflict with  GREen).

           Under DOS,  $T expects the DOS EnvVar (environmental variable) SCREEN
           to be set with  up to three values  (with blanks between the values):
             foreground─color─code   background─color─code   text─attribute─code
           These values are used to restore the screen colors after  $T finishes
           executing.    If no  SCREEN  variables are set, then    37 40 0    is
           assummed.       Under CMS,  screen colors are automatically restored.

          ┌────────────────────────────────────────────────────────────────────┐
          │ fore─  back─                                                       │
          │ground ground     color                    text attributes          │
          ├──────────────────────────────────┬─────────────────────────────────┤
          │   30     40       black          │                                 │
          │   31     41  dark red            │                                 │
          │   32     42  dark green          │                                 │
          │   33     43  dark yellow (brown) │ 0 reset   (all attributes off)  │
          │   34     44  dark blue           │                                 │
          │   35     45  dark magenta        │ 1 bold (or high─intensity color)│
          │   36     46  dark cyan           │                                 │
          │   37     47  dark white  (grey)  │ 4 underscore (monochrome only)  │
          │ 1,31   1,41       red            │                                 │
          │ 1,32   1,42       green          │ 5 blink                         │
          │ 1,33   1,43       yellow         │                                 │
          │ 1,34   1,44       blue           │ 7 reverse video                 │
          │ 1,35   1,45       magenta        │                                 │
          │ 1,36   1,46       cyan           │ 8 concealed  (invisible)        │
          │ 1,37   1,47       white          │                                 │
          └──────────────────────────────────┴─────────────────────────────────┘

.D=disposition   where disposition is one of the capitalized following:
           NONE     ─── shows normal text.
           REVVIDEO ─── shows reverse vidio text.
           UNDERLIN ─── shows underlined text.
           BLINK    ─── shows blinking text.
           BRITE    ─── shows text in high intensity white.
           HIGHL    ─── shows parenthesized text in highlight (default is
                        BRITE  (or high intensity white), or whatever
                        color is specified via the  .H=color  option).  The
                        parenthesis used () are translated to blanks via this
                        process.  Specifying any of the   .H   .H[]   .H{}
                        options enables the   .D=HIGHL   option by default.

.DEC=ddd,ddd,ddd
.DEC=ddd         is a decimal string (number) and   ddd   should be only decimal
           digits.   No leading, trailing, or imbedded blanks are allowed within
           within the decimal number(s).              Commas (,) may be used for
           readability.   The decimal string is converted to a character string.
           Any number of   .DEC=   options may be specified.

.E=nn      number of blank lines to be added before and after the text.   The
           lines will be "inside" the box  if  a box is generated.   These blank
           lines are in addition to (if any) the     .A     and     .P    lines.
           nn   can be from  0 ──> 99     (default is 0).
           /hex=
           nn   can also be specified as: *         (screen depth)
                                          *+nn      (screen depth + nn)
                                          *-nn      (screen depth - nn)
                                          M+nn      (half screen +nn)
                                          M-nn      (half screen -nn)
                                          */nn      (screen depth / nn), rounded
                                                                            down
.EB=c
.EB=hh     the character used as a blank for the  .E  option  (displaying blank
           lines before and after the text.    It can be specified as a single
           character  or  as a hexadecimal pair.     The default is a blank.

.EF=fff    same as the  .F=fff   option, but if running under DOS, doesn't build
           the directory structure (tree) if it doesn't exist.

.END=1     any     .OOO=      options following the .END=1 option are to be
           treated as data, that is, they will appear as is in the message text.

.F=fff     writes the text to the file specified (fff),  in addition to typing.

           Under CMS, the filename, filetype, and filemode  (the ft and fm are
           optional)  should be seperated by commas (,).

           The default for  filetype  is   $T        (for CMS).
           The default for  filemode  is   A1        (for CMS).

           Error messages (if any) aren't written to the specified file.

           Under DOS, the directory structure (tree) for the file is built if it
           doesn't exist.   All levels are created in need be.

.H=color   specifies the color to be used when () highlighting is in effect (via
           the   .D=HIGHL   option).   If not specified,  the default is  BRITE.
           Color for this highlighting is effected  by placing  some text within
           parenthesis  ().    The  .H=  option enables the   .D=HIGHL   option.

.H[]=color specifies the color to be used when [] highlighting is in effect (via
           the  .D=HIGHL  option).     If not specified,  the default is  BRITE.
           Color for this highlighting is effected  by placing  some text within
           brackets  [].     The  .H[]=  option enables the   .D=HIGHL   option.

.H{}=color specifies the color to be used when {} highlighting is in effect (via
           the  .D=HIGHL  option).     If not specified,  the default is  BRITE.
           Color for this highlighting is effected  by placing  some text within
           braces  {}.       The  .H{}=  option enables the   .D=HIGHL   option.

.HEX=hh
.HEX=hh,hh,hh ...
.HEX=hhhh  is a hexadecimal string  and  hh  should be hexadecimal pair(s).  The
           hexadecimal characters may be in mixed case.    No leading, trailing,
           or imbedded blanks  are allowed within the hexstring.  Commas (,) may
           be used for readability.     The hexadecimal string is converted to a
           character string.
           Any number of   .HEX=   options may be specified.

.I=nn      number of spaces all text is to be  indented   (this includes the box
           (if a box is generated).
           nn   can be from  0 ──> 99,  the default is  0.

           nn   can also be specified as: *         (screen width)
                                          *+nn      (screen width + nn)
                                          *-nn      (screen width - nn)
                                          M+nn      (half of width +nn)
                                          M-nn      (half of width -nn)
                                          */nn      (screen width / nn), rounded
                                                                            down

.IB=c
.IB=hh     the character used as a blank for the  .I  option  (indentation).  It
           can be  specified as a  single character  or  as a  hexadecimal pair.
           The default is a blank.

.INV=ccc   where  ccc  is any number of characters  whose  bit values  are to be
           inverted (that is, turn all  1's  into  0's, and all  0's  into  1's.
           Any number of   .INV=   options may be specified.

.J=Any
.J=Left
.J=Right
.J=Center
.J=Justify
.J=None    specifies the kind of justification.     The default is  .J=N  (none)

           In all cases of justification, indentation is performed (added) after
           the justification process  (this may cause truncation or croping).
           All justifications are made within the screen width.  Any text longer
           or equal to the screen width isn't justified and left intact (asis).
           In call cases below, the actual screen width (or length) is the
           screen width minus one.   The value may be in any case.

                what blanks
           .J=  are stripped           what the justification does
           ───  ──────────── ──────────────────────────────────────────────────
            A     depends     randomly chooses one of    L,   R,   or   C.

            L     leading     abuts the text against the  leftmost column

            R     trailing    abuts the text against the rightmost column

            C       both      places the text in the center of the line

            J       both      inserts blanks between words so they fill the line

            N       none      nothing, no justification is performed


.K='HHHH'X
.K='hhhh'x
.K=ccc     where  ccc  is a character or characters which are used as chop the
           text into smaller (multiple) text lines.   The character(s) can be
           thought of as line seperators or line feeds.   The character string
           is kept as part of the previous string.   If a blank is desired
           as the line sepeator, it must be specified in hexadecimal.

           The string can also be specified as a hexadecimal character string.

           This option can't be specified if the   .KD=   option is used.

.KD='HHHH'X
.KD='hhhh'x
.KD=ccc    same as the   .K=   option, but the character(s) are removed from the
           text.

           This option can't be specified if the   .KD=   option is used.

.KS=nn     number of "blank" lines displayed between lines of chopped message
           text(s)   (via the   .K    or   .KD    option).
           nn   can be from  0 ──> 99     (default is 0).

           nn   can also be specified as: *         (screen depth)
                                          *+nn      (screen depth + nn)
                                          *-nn      (screen depth - nn)
                                          M+nn      (half screen +nn)
                                          M-nn      (half screen -nn)
                                          */nn      (screen depth / nn), rounded
                                                                            down
.KSB=c
.KSB=hh    the character used as a blank for the  .KS  option  (displaying blank
           lines between chopped message texts.  It can be specified as a single
           character or as a hexadecimal pair.     The default is a blank.

.L=nn      where  nn   is the line on the screen that the text is to be shown,
           and must be one of  (capitalized if alphabetic):
             nn     line on screen, from 1 to the screen depth size.
             *      (same as LAST)
             LAST   indicates the last line on the screen.
             CMSG   indicates the CMS command line.  The  text  is written to
                    the command line and then can be modified by the user and
                    the ENTER key can be pressed to enter the text as a command.
             M      indicates the middle of the screen  (rounded down).
             M+nn       "      "    "    "   "    "    plus    nn   lines.
             M-nn       "      "    "    "   "    "    minus    "     "
             *-nn       "      "  last line  "    "    minus    "     "
             */nn       "      "  last line  "    "    divided by  nn (rounded
                                                                       down)
           A value of zero indicates to write the message normally.

.O=nnn     type only  nnn  lines of text, even if more lines exist (or have been
           generated), including blank lines.   A value of   0   suppresses all

           If  nnn  is negative,  only the positive  nnnTH  line is typed.
           Writting to disk (if any)  is similarly affected.
           nnn  can be from  -999 ──> 999, the default is  *   (same as 999).

.P=nn      specifies the  number  of  blank  lines  written previous to the
           message text being displayed.
           nn   can be from  -99 ──> 99     (default is 0).

           nn   can also be specified as: *         (screen depth)
                                          *+nn      (screen depth + nn)
                                          *-nn      (screen depth - nn)
                                          M+nn      (half screen +nn)
                                          M-nn      (half screen -nn)
                                          */nn      (screen depth / nn), rounded
                                                                            down

.Q=1       suppress all typing  (writing to the screen), but doesn't suppress
           writing to a disk file    (via the  .F=fff  option, see above).
           The default is    0       (don't suppress typing).

.R=nn      number of times the text is repeated  (on one line).  Each
           nn   can be from  0 ──> 99,  the default is  0.

.REM=ccc   string   ccc  is treated as a comment (remark).  The  string can't
           have any blanks.

.RULER=*
.RULER=-nnn
.RULER=+nnn
.RULER=nnn width of a ruler line to be shown (typed) after the message.
           A leading minus sign indicates the ruler is shown before the message.
           A leading  plus sign indicates the ruler is shown before  and  after
           the message.

           nnn  can be from  0 ──> screenwidth,  the default is  0,  or it can
                be specified as an asterisk (*) which indicates the screenwidth.

           A value of  0  indicates no ruler is to be shown  (the default is 0).
           The ruler can be  1,  2, or  3  lines, depending upon how many digits
           there are in the length of the ruler.   The ruler generated isn't
           indented, boxed, nor justified as with the other text messages.
           The ruler may look like (say, for a width of 57):

                    1         2         3         4         5
           123456789012345678901234567890123456789012345678901234567

.RULERB=c
.RULERB=hh the character used as a blank for the  ruler.  It can be specified as
           single character  or  as a  hexadecimal pair.
           The default is a blank.

.S=nnn     number of lines that  $T  will suppress typing to the screen,
           nnn  can be from  0  or any positive or negative integer.  The
           default is zero.

           If  nnn  is negative, it indicates that  $T  will only suppress
           (positive)  nnnTH  line,  all others aren't suppressed.
           Writting to disk (if any)  is similarly affected.

.SCALE=*
.SCALE=-nnn
.SCALE=+nnn
.SCALE=nnn width of a scale line to be shown (typed) after the message.
           A leading minus sign indicates the scale is shown before the message.
           A leading  plus sign indicates the scale is shown before  and  after
           the message.

           nnn  can be from  0 ──> screenwidth,  the default is  0,  or it can
                be specified as an asterisk (*) which indicates the screenwidth.

           A value of  0  indicates no scale is to be shown  (the default is 0).
           The scale can be  1,  2, or  3  lines, depending upon how many digits
           there are in the length of the scale.   The scale generated isn't
           indented, boxed, nor justified as with the other text messages.
           The scale may look like (say, for a width of 57):

                    1         2         3         4         5
           ....+....0....+....0....+....0....+....0....+....0....+..

.SCALEB=c
.SCALEB=hh the character used as a blank for the  scale.  It can be specified as
           single character  or  as a  hexadecimal pair.
           The default is a blank.

.SCALED=c
.SCALED=hh the character used as a  dot  for the  scale.  It can be specified as
           single character  or  as a  hexadecimal pair.
           The default is a dot (.)  [period or decimal point].

.SCALEP=c
.SCALEP=hh the character used as a plus  for the  scale.  It can be specified as
           single character  or  as a  hexadecimal pair.
           The default is a plus sign (+)  [period or decimal point].

.T=nn      number of times the text is shown  (typed).
           nnn  can be from  0 ──> screenwidth,  the default is  1.

.TS=nn     number of "blank" lines displayed between lines of text when  .T  is
           greater than 1.   The  .EB  character is used for a blank.
           These lines are in addition to (if any) the    .A    .P    and   .E
           blank lines.
           nn   can be from  0 ──> 99     (default is 0).

           nn   can also be specified as: *         (screen depth)
                                          *+nn      (screen depth + nn)
                                          *-nn      (screen depth - nn)
                                          M+nn      (half screen +nn)
                                          M-nn      (half screen -nn)
                                          */nn      (screen depth / nn), rounded
                                                                            down

.TSB=c
.TSB=hh    the character used as a blank for the  .TS  option  (displaying blank
           lines between "times" message texts.  It can be specified as a single
           character or as a hexadecimal pair.     The default is a blank.

.V=N
.V=R       views the msg (but not the box if one is drawn) in Normal or Reverse.
           The default is  .V=n                               ─         ─

.U=A       uses the  msg  asis   (no translation).   The default is  .U=A
.U=L       uses the  msg  after translating it to lowercase.
.U=U       uses the  msg  after translating it to uppercase.
.U=F       uses the  msg  after translating the 1st character to uppercase, and
           all other characters to lowercase.
.U=W       uses the  msg  after translating the 1st character of every word to
           uppercase, all other characters to lowercase.

.UB=c
.UB=hh     the character used as a blank in the text.    All blanks in the text
           are translated to this character.    It can be specified as a single
           character or as a hexadecimal pair.        The default is a blank.
.USE=c
.USE=hh    the character to be used in subseqent     .OOO=     options.
           It can be specified as a single character or as a hexadecimal pair.
           The default is a decimal point (.) or dot.

.UT='hhhhhhh'x
.UT='HHHHHHH'x
.UT=ccCC   are pairs of hexadecimal pairs or pairs of characters, the first part
           of the pair  is a character of the text  that is to be  translated to
           the 2nd part of the pair.    If  any part  of the pair(s)  is a blank
           (character), all characters must be specified in hex.   Also, in DOS,
           any console redirection characters  should also be  specified in hex,
           such as the  <,  >,  and |  characters, as well as the usual terminal
           control characters such as backspace,  rub,  line feed, tab, carriage
           return, bell, escape, etc.   (These are characters below '20'x).

.W=        where   waitmsg  is the text to be shown and then waits for:

           ──>on DOS: any key to be pressed  (cxcept for SHIFT, CTRL, ALT)
           ──>on CMS: the  ENTER  key to be pressed, or any PF or PA key pressed

           If   waitmsg   is the character  ▌  ('DD'x)  then the following
           text is shown:  please press any key to continue ...

           If   waitmsg   is the character  ▐  ('DE'x)  then the following
           text is shown:  please press the  ENTER  key to continue ...

.WB=c
.WB=hh     the character used as a blank for the  .W  option  (displaying a wait
           message after the text is displayed.    It can be specified as a
           single character or as a hexadecimal pair.   The default is a blank.

.X=nn      number of extra blanks to append to the message (if positive), or
           if negative, extra blanks are  prefixed  and  appended  to the text.
           nn   can be from   -screenwidth ──> screenwidth,  the default is  0.

           nn   can also be specified as: *         (screen width)
                                          *+nn      (screen width + nn)
                                          *-nn      (screen width - nn)
                                          M+nn      (half width + nn)
                                          M-nn      (half width - nn)
                                          */nn      (screen width / nn), rounded
                                                                            down

.XB=c
.XB=hh     the character used as a blank for the  .X  option  (displaying blanks
           after  and/or  before the text.    It can be specified as a single
           character  or  as a hexadecimal pair.     The default is a blank.

.Z=nn      number of seconds delay after the message text is typed.
           nn   can be from  0.0 ──> 99,  the default is  0.

────────────────────────────────────────────────────────────────────────────────

One quirk with REXX/PC: if the last line written to the screen from a REXX
                        program is greater then the screen width, the 2nd part
                        of the text {i.e., the wrapped line(s)}  is overwritten
                        by the REXX manager as part of its clean-up.  Therefore,
                        to insure visual integrity of  $T  displayed text, if
                        the last line of  $T's  screen output is greater then
                        the screen width, an extra blank line is displayed.

                                       Ω

```


[[Category:REXX library routines]]
