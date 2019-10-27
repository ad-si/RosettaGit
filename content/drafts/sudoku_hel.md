+++
title = "$SUDOKU.HEL"
description = ""
date = 2018-10-19T23:20:49Z
aliases = []
[extra]
id = 22031
[taxonomies]
categories = []
tags = []
+++

[[Category:REXX_library_routines]]

This   HELp   page is the documentation for the   $SUDOKU   REXX program which can be used to solve (or help solve) a sudoku puzzle. 

A note about the filetype.   This documentation file was written before Windows/95 and was limited to an 8.3 naming convention.


The following text file is the documentation (HELp) for the   '''$SUDOKU.REX'''   program.

```txt

The  $SUDOKU  command will display a sudoku puzzle, cells/rows/columns of which
may be specified.     A sudoku puzzle is a grid of nine 3x3 cells  (for a total
of 9x9 cells)  that can contain the digits 1──►9.    The object is to fill in
the puzzle so that every row, column,  and  3x3 box  has every (unique) digit.

To show several supplied sudoku puzzles, the  $SUDOKU#   program can be used to
display over 12,600 different puzzles.   To see that help, issue:   $H $SUDOKU#

╔══════════════════════════════════════════════════════════════════════════════╗
║                         {CLearscneen | NOCLearscreen}                        ║
║                         {HIGHLightsingles | NOHIGHLightsingles}              ║
║                         {PUZZle .d..dd..d.......d..dddd.ddd...ddd.dddd....}  ║
║                         {COLumn n .d..dd..d.}                                ║
║                         {ROW    n ...d..d.dd}                                ║
║  $SUDOKU                {CELL  rc d}                                         ║
║                         {PRUNEEXCLusives}   {PRUNELINEs}                     ║
║            ?            {PRUNEMATches}      {PRUNEONLYs}      {PRUNESINGLes} ║
║            ?AUTHOR      {PRUNEALL}                                           ║
║            ?FLOW        {SHORTgrid}                                          ║
║            ?SAMPLES     {SHOWCELL rc,xy,ab,...}                              ║
║                         {SHOWBOXes bbb}    {SHOWCOLs ccc}    {SHOWROWs rrr}  ║
║                         {SHOWCOMBinations}                                   ║
║                         {SHOWGrid | NOSHOWGrid}                              ║
║                         {SHOWINFOmation | NOSHOWINFOmation}                  ║
║                         {SHOWPOSSibles}                                      ║
║                         {SHOWONELINE}                                        ║
║                         {SIMPLE}                                             ║
║                         {tops}                                               ║
╚══════════════════════════════════════════════════════════════════════════════╝

───where:

?          shows this help file              (press  ESC  to quit when viewing).

?AUTHOR    shows the author of this program.

?FLOW      shows the external execution flow of this program.

?SAMPLES   shows some sample uses            (press  ESC  to quit when viewing).

CLearscreen     clears the screen before any grid is shown.
                                                    The default is:  CLEARSCREEN

NOCLearscreen   doen't clear the screen before any grid is show.
                                                    The default is:  CLEARSCREEN

HIGHLightsingles  highlights all specified digits  (if the grid is shown).
                  A highlighted digits is prefixed and suffixed with a
                  minus sign (-), or shown in yellow if running on CMS or
                  with PC/REXX.              The default is:  NOHIGHLIGHTSINGLES

NOHIGHLightsingles  doesn't highlight specified digits  (if the grid is
                    shown).                  The default is:  NOHIGHLIGHTSINGLES

PUZZle .d..dd..d.......d..dddd.ddd...ddd.dddd....    (for example)
                The character string that follows  are the digits  to be placed
                into the puzzle  (going from left to right).  row by row.   Any
                position  that has a  period (.)  is skipped over.    The  10th
                character would be the start of row 2, the 19th character would
                be the start of row 3, etc.  The character string is considered
                to "wrap around",  row to row.  Up to 81 chars may be specified.

COL n .d..dd..d.    (for example)
                D  is the column to be specified and must be  1 ───► 9.    The
                character string that follows  are the digits  to be placed  in
                that column  (going from top to bottom),  and any position that
                has a period (.) is skipped over.  I.E., to set  column 9  (the
                rightmost column) to    blank 3 blank blank 4 7 blank 8,    the
                following could be specified:   col 9 .3..47.8     (the rest of
                the column is left blank).   Up to 9 digits  (or chars)  may be
                specified.  Any number of   COL   keywords may be specified and
                they may be given in any order.

ROW n ...d..d.dd    (for example)
                D  is the  row   to be specified and must be  1 ───► 9.     The
                character string that follows  are the digits  to be placed  in
                that row (going from left to right),  and any position that has
                a period (.) is skipped over.     I.E.,  to set row 5      (the
                middle row) to      blank blank 6 9 blank 5 blank 2,        the
                following could be specified:    row 5 ..69.5.2       (the rest
                of the row is left blank).    Up to nine digits  (or chars) may
                be specified.   Any number of   ROW  keywords  may be specified
                and they may be given in any order.

CELL rc d       R  is the  row   to be specified and must be  1 ───► 9,
                C  is the  col   to be specified and must be  1 ───► 9,
                D  is the  digit to be  placed   and must be  1 ───► 9   or  "."

                I.E.,  to set the 4th cell in the grid (row 1, col 4)  to the
                digit 7, the following could be specified:   CELL 14 7
                Any number of  CELL  keywords my be specified and they may be
                in any order.

PRUNEEXCLusives will prune any possible values that are the only value (digit)
                for a box.  If  PRUNESINGLE  is in effect,  than this digit is
                made into a specified digit  (solves that cell).
                                              The default is:  NOPRUNEEXCLUSIVES

PRUNEMATches    will prune any possible values that are matched up (two pairs,
                three triplets, ...) and then removes them from any other
                possible on the same row and/or column.   If  PRUNESINGLE  is
                in effect, any possible values that have now become one digit
                are made into a specified digit.
                                                 The default is:  NOPRUNEMATCHES

PRUNEONLYs      will prune any possible values  that are the  only  digit  in a
                row or column,  and then then  removes all other digits in that
                cell, and if just a single digit remains,  makes it a specified
                digit (solves that cell).          The default is:  NOPRUNEONLYS

PRUNESINGles    will prune any possible values that have a single value (one
                digit) to be as if it were a specified digit.    This is the
                simplest form of pruning.        The default is:  NOPRUNESINGles

PRUNELINEs      will prune any possible values that exist in any row or column
                that can only can exist in a particular row or column in a
                box.                               The default is:  NOPRUNELINEs

PRUNEALL        will prune all of the above  PRUNExxx.
                                                     The default is:  NOPRUNEALL

SHORTgrid       shows a shortened versin of the grid.
                                                    The default is:  NOSHORTGRID

NOSHORTgrid     shows a   full    versin of the grid.
                                                    The default is:  NOSHORTGRID

SHOWBOXes bbb   when showing POSSibles,  only those boxes   (BBB...)  specified
                have their possible digits shown,  where  B  is the box
                number(s)   and must be  1 ───► 9.
                The boxes are numbered  left to right, top to bottom,  with the
                top left-most box is 1, the middle box is 5, and the 1st box in
                the middle row is box 4,  the 1st box on the last row is box 7.
                                                     The default is:  all boxes.

SHOWCOLs ccc    when showing POSSibles, only those columns  (CCC...)  specified
                have  their possible  digits  shown,  where   C   is the column
                number(s)   and must be  1 ───► 9.
                The columns are numbered left to right.
                                                    The default is:  all columns

SHOWROWs ccc    when showing POSSibles,   only those rows  (CCC...)  specified
                have their possible digits shown,   where    R    is the row
                number(s)   and must be  1 ───► 9.
                The rows are numbered top to bottom.
                                                       The default is:  all rows

SHOWCOMBinations    shows the number of combinations of all the possible
                    values.                  The default is:  NOSHOWCOMBinations

NOSHOWCOMBinations  doesn't show the number of combinations of all the
                    possible values.         The default is:  NOSHOWCOMBinations

SHOWGrid        shows the sudoku puzzle  in a grid  after  the digits are
                specified,  after  computing  the  possible values  (if wanted),
                after each pruning (if any).           The default is:  SHOWGrid

NOSHOWGrid      doesn't show the grid.                 The default is:  SHOWGrid

SHOWINFOmation    shows various information messages such as screen titles,
                  action being taken, etc.      The default is:  SHOWINFOrmation

NOSHOWINFOmation  doesn't show the informational messages.
                                                The default is:  SHOWINFOrmation

SHOWPOSSibles   shows what digits are possible for each empty cell.
                The   SHOWGrid   option  must be  ON,  and  the cells shown are
                restricted (if given) by   SHOWCELL,  SHOWCOLs,  and  SHOWROWs.
                                               The default is:  NOSHOWPOSSibles

SHORTgrid       shows a shortened versin of the grid.
                                                    The default is:  NOSHORTGRID

NOSHORTgrid     shows a   full    versin of the grid.
                                                    The default is:  NOSHORTGRID

SHOWONELINE     shows a the puzzle as speiified as line line of:
                ....dd....d.d.d..d.....d....d.dd...d.....d....d   (for example).
                Up to  81  characters  may be shown,  and  any trailing periods
                aren't shown.                     The default is:  NOSHOWONELINE

                +---+
SIMPle     uses |   | for the boxing characters.       The default is:  NOSIMPle
                +---+

                ┌───┐
NOSIMPle   uses │   │ for the boxing characters.       The default is:  NOSIMPle
                └───┘

tops       are any or all of the following  $T  .X=xxx options.


────────────────────────────────────────────────────────────────────────────────

Some (but not all) of the  $T  options are:   (issue    $T ?    for more help)

────────  ──────────────────────────────────────────────────────────────────────

.I=nnn    indents the messages   nnn   spaces,   the default is 0.

.C=color  sets the  color  of the messages,  there is no default.

.H=color  sets the highlight color of any parenthesized text,  there is
          no default.

.F=fff    writes the information (in addition to typing it) to the file,  fff
          there is no default.

                                       Ω

```

