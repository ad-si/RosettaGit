+++
title = "$MORSE.HEL"
description = ""
date = 2017-09-06T23:59:17Z
aliases = []
[extra]
id = 12879
[taxonomies]
categories = []
tags = []
+++

The   '''$MORSE.HEL'''   file is the HELp (documentation) file for the   '''$MORSE.REX'''   (REXX) program.

```txt

The   $MORSE   command is used to display the  Morse code  equivalent of text
(words or letters of the English (Latin) alphabet,  digits,  and some special
characters,  mostly punctuation).

╔══════════════════════════════════════════════════════════════════════════════╗
║                                                                              ║
║                 ┌         ┐            ┌            ┐                        ║
║    $MORSE       │ options │      (     │ words ...  │                        ║
║                 └         ┘            └            ┘                        ║
║                                                                              ║
║                 ┌          ┐                                                 ║
║                 │ ?        │                                                 ║
║                 │ ?AUTHOR  │                                                 ║
║                 │ ?FLOW    │                                                 ║
║                 │ ?SAMPLES │                                                 ║
║                 └          ┘                                                 ║
║                                                                              ║
╚══════════════════════════════════════════════════════════════════════════════╝

───where:

?              shows this help file          (press  ESC  to quit when viewing).

?AUTHOR        shows the author of this program.

?FLOW          shows the external execution flow of this program.

?SAMPLES       shows some sample uses        (press  ESC  to quit when viewing).

(              indicates the start of the words (text) to be translated to Morse
               code.    Any leading, trailing, or extra blanks are removed.

words ...      are the words (or letters, digits, symbols) to be translated to
               code.  Any characters that can't be translated (i.e., have no
               Morse code equivalent) are left untranslated and are shown as is.
               Some characters are translated to their closest Morse code
               equivalent, i.e.,  all brackets and braces are translated to the
               Morse code parenthesis.

               Letters supported (lower/uppercase):  ABCDEFGHIJKLMNOPQRSTUVWXYZ
               Digits  supported:                    0123456789

               Punctuation supported:    .    period
                                         ,    comma
                                         ;    semicolon
                                         :    colon
                                         '    apostrophe
                                         "    quotation mark
                                         _    underscore
                                         -    minus sign
                                         +    plus sign
                                         =    equal sign
                                         (    left  parenthesis
                                         )    right parenthesis
                                         /    slash or fraction bar
                                         ?    question mark
                                         &    ampersand
                                         !    exclaimation mark
                                         $    dollar sign
                                         @    commercial at

              Note that the American (or railroad) Morse code doesn't support
             some of the special (puntuation) characters.  In that case, the
             International Morse code is used.

┌───────────────────────┬──────────────────────────┬──────────────────────────┐
│       character       │ International Morse Code │ American (RR) Morse Code │
├───────────────────────┼──────────────────────────┼──────────────────────────┤
│              letter A │         ∙ ─              │           (same)         │
│              letter B │         ─ ∙ ∙ ∙          │           (same)         │
│              letter C │         ─ ∙ ─ ∙          │        . . _ .           │
│              letter D │         ─ ∙ ∙            │           (same)         │
│              letter E │         ∙                │           (same)         │
│              letter F │         ∙ ∙ ─ ∙          │        . __ .            │
│              letter G │         ─ ─ ∙            │           (same)         │
│              letter H │         ∙ ∙ ∙ ∙          │           (same)         │
│              letter I │         ∙ ∙              │           (same)         │
│              letter J │         ∙ ─ ─ ─          │        _ . _ .           │
│              letter K │         ─ ∙ ─            │           (same)         │
│              letter L │         ∙ ─ ∙ ∙          │        __                │
│              letter M │         ─ ─              │           (same)         │
│              letter N │         ─ ∙              │           (same)         │
│              letter O │         ─ ─ ─            │        . _ .             │
│              letter P │         ∙ ─ ─ ∙          │        . . . .  .        │
│              letter Q │         ─ ─ ∙ ─          │        . . __  .         │
│              letter R │         ∙ ─ ∙            │        . _ . .           │
│              letter S │         ∙ ∙ ∙            │           (same)         │
│              letter T │         ─                │           (same)         │
│              letter U │         ∙ ∙ ─            │           (same)         │
│              letter V │         ∙ ∙ ∙ ─          │           (same)         │
│              letter W │         ∙ ─ ─            │           (same)         │
│              letter X │         ─ ∙ ∙ ─          │        . __ .  .         │
│              letter Y │         ─ ∙ ─ ─          │        . . _ .  .        │
│              letter Z │         ─ ─ ∙ ∙          │        . . . _  .        │
│                       │                          │                          │
│               digit 0 │         ─ ─ ─ ─ ─        │        ____              │
│               digit 1 │         ∙ ─ ─ ─ ─        │        . _ _ .           │
│               digit 2 │         ∙ ∙ ─ ─ ─        │        . . __  . .       │
│               digit 3 │         ∙ ∙ ∙ ─ ─        │        . . . __  .       │
│               digit 4 │         ∙ ∙ ∙ ∙ ─        │           (same)         │
│               digit 5 │         ∙ ∙ ∙ ∙ ∙        │        _ _ _             │
│               digit 6 │         ─ ∙ ∙ ∙ ∙        │        . . . .  . .      │
│               digit 7 │         ─ ─ ∙ ∙ ∙        │        _ _ . .           │
│               digit 8 │         ─ ─ ─ ∙ ∙        │        _ . . .  .        │
│               digit 9 │         ─ ─ ─ ─ ∙        │        _ . . _           │
│                       │                          │                          │
│   & ampersand         │         ─ ─ ─ ─ ∙        │        . _ . . .         │
│   ' apostrophe        │         ∙ ─ ─ ─ ─ ∙      │          (not supported) │
│   : colon             │         ─ ─ ─ ∙ ∙ ∙      │          (not supported) │
│   @ commercial at     │         ∙ ─ ─ ∙ ─ ∙      │          (not supported) │
│   , comma             │         ─ ─ ∙ ∙ ─ ─      │        . _ . _           │
│   $ dollar sign       │         ∙ ∙ ∙ ─ ∙ ∙ ─    │          (not supported) │
│   " double quote      │         ∙ ─ ∙ ∙ ─ ∙      │          (not supported) │
│   = equal sign        │         ─ ∙ ∙ ∙ ─        │          (not supported) │
│   ! exclamation mark  │         ∙ ─ ∙ ─ ∙ ∙      │        _ _ _ .           │
│   ( left parenthesis  │         ─ ∙ ─ ─ ∙ ─      │          (not supported) │
│   - minus sign        │         ─ ∙ ∙ ∙ ∙ ─      │          (not supported) │
│   . period            │         ∙ ─ ∙ ─ ∙ ─      │        . . _ _ . .       │
│   + plus sign         │         ∙ ─ ∙ ─ ∙        │          (not supported) │
│   ? question mark     │         ∙ ∙ ─ ─ ∙ ∙      │        _ . . _ .         │
│   ) right parenthesis │         ─ ─ ─ ∙ ∙        │          (not supported) │
│   ; semi-colon        │         ─ ∙ ─ ∙ ─ ∙      │          (not supported) │
│   / slash             │         ─ ∙ ∙ ─ ∙        │          (not supported) │
│   ─ underscore        │         ∙ ∙ ─ ─ ∙ ─      │          (not supported) │
│                       │                          │                          │
└───────────────────────┴──────────────────────────┴──────────────────────────┘


───where options can be any or all of the following:

DASHs  x       (same as DAHs)
DASHes x       (same as DAHs)
DAHs   x       is the character to be used when displaying a DAH.
                                  For CMS and DOS, the default is: ─
                                  For all others,  the default is: -

DOts x         (same as DITs)
DITs x         is the character to be used when displaying a DIT.
                                  For CMS and DOS, the default is: ∙
                                  For all others,  the default is: .

LONGs x        is the character to be used when displaying a LONG dash,
               LONGs  are only used for the  RAILROAD  Morse code.
                                                   The default is: __

LONGers x      is the character to be used when displaying a LONGer dash,
               LONGERs  are only used for the  RAILROAD  Morse code.
                                                   The default is: ____

CLearscreen    clears the screen before typing.    The default is: NOCLEARSCREEN

Quiet          suppresses the showing of any results.   However, the REXX  variable
               RESULT    is always set  (unless there's an error).
               Error messages  (if any)  are always shown.
                                                     The default is:  NOQUIET

NOQuiet        shows the results.

NOEMSG         suppresses the error message   "no words were specified after (".
               The default is:   EMSG

SHOWcodes      shows (types) all known Morse characters and their Morse code
               equivalents  (one per line).   There are about 60 or so.
               The default is   NOSHOWcodes.

SPACEs   nnn   where   nnn    is the number of blanks to be inserted between the
               Morse code words.                         The default is 3.

BETWEENS nnn   where   nnn    is the number of blanks to be inserted between the
               dits and dahs of the Morse code.          The default is 0.

SOUNDs         causes sound to be generated for each morse "dit" or "dah".

NOSOUNDs       causes sound to be not generated.         The default is:  SOUND

SPREADs  nnn   where   nnn    is the number of blanks to be inserted between the
               Morse code "letters".                     The default is 1.

SLIce          causes the output to be split up into multiple lines,  one line
               per Morse code letter.                    The default is NOSLICE.

NOSLIce        doesn't split the output on letter boundries.

SPLit          causes the output to be split up into multiple lines,  one line
               per Morse code word.                      The default is SPLIT.

NOSPLit        doesn't split the output on word boundries.

INTERnational  sets the type of Morse code to the one used internationaly.
                                                    The default is INTERNATIONAL

AMERican       (same as RAILROAD)
USA            (same as RAILROAD)
RAILways       (same as RAILROAD)
RAILROADs      sets the type of Morse code to the one used by US railways of
               olde.

COLORs         shows the output in color.
               The default is:     (for CMS or DOS):      COLORS
                                   (for all others):    NOCOLORS

               The default TOPS is:   .C=green
               This options can be overridden by specifying  TOPS    (see below).

NOCOLORs       won't show the output in color.

LOGs           logs the answer to a log file.   The default is:   NOLOGS
               The name of the log file is one of the following:

                (DOS)   tmp\$MORSE.ANS    (if TMP  environmental var is defined)
                (DOS)  temp\$MORSE.ANS    (if TEMP environmental var is defined)
                (DOS)    C:\$MORSE.ANS    (depending if the ENVVARs are defined)

                (CMS)       $MORSE ANS A4
                (TSO)       $MORSE.ANS

NOLOGs         won't log the answer to a log file.

tops           are any of the  "dot"  options of the  $T  command   (see below).


────────────────────────────────────────────────────────────────────────────────

Some (but not all) of the  $T  options are:   (issue    $T ?    for more help)

────────  ──────────────────────────────────────────────────────────────────────

.BOX=     draws a box (as shown above) around the message, this is the default.
          (Use   .BOX=*NONE*   to override boxing)

.A=nnn    types   nnn   blank lines   after  the  message   (in addition to .E=)
          The default is  0.

.P=nnn    types   nnn   blank lines previous to the message (in addition to .E=)
          The default is  0.

.E=nnn    types   nnn   blank lines before and after the message,  they are
          within the box  (if any),   the default is  0.

.I=nnn    indents the message  nnn  spaces,   the default is 0.

.X=nnn    appends   nnn   blanks to the message,   or,   if   nnn   is negative,
          appends  and  prefixes   nnn   blanks to the message.
          The default is  0.

.B=nnn    sets the number of beeps (alarms) before typing of the text.
          nnn  can be  0  or a whole number,   the default is  0.
          (A negative number indicates to beep before and after the message).

.C=color  sets the  color  of the message,  the default is  GREEN.

.H=color  sets the highlight color of any parenthesized text.  The default color
          is  YELLOW.

.F=fff    writes the information (in addition to typing it) to the file   fff

.J=kind   justifies  (Left, Right, Center, or Justify)  the text on the screen,
          the default is   Left

.K=ccc    chops the output in several lines, each seperated by the character
          string    ccc    (which is kept at the end of each line of output).
          The default is no  .K

.KD=ccc   same as the  .K=  option, but the character string is deleted from the
          output lines.   The   ccc   can also be specified in hexadecimal with:
          .K='hhhh'X   or   .KD="HHHH"x    where   hh   are hexadecimal pairs of
          hexadecimal digits  (0──►9, a──►f, A──►F).
          The default is  no  .KD

.Q=1      suppress the typing of the message.     .Q=0   is the default.

                                       Ω

```


[[Category:REXX library routines]]
