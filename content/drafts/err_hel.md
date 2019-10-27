+++
title = "$ERR.HEL"
description = ""
date = 2017-09-06T23:58:30Z
aliases = []
[extra]
id = 12872
[taxonomies]
categories = []
tags = []
+++

This   '''$ERR.HEL'''   page is the contents of the (help) information displayed when the   '''$ERR.REX'''   (REXX) program 

invokes the   '''$H.REX'''   program to display the   '''help'''   documentation for the   '''$ERR.REX'''   program.  


```txt

The  $ERR  command is used to display an error message (or messages) from a
predefined list of over 100 messages.

Usually,  five lines are displayed  (all in red if  color is supported):

            {a blank line}
            ($$$14nn) *error*:
            an error message
            maybe error message2
            possible error message3
            for the ──────── ccc ──────── command.
            {a blank line}

The format of the  $ERR  command is:

╔═════════════════════════════════════════════════════════════════════════════╗
║                                                                             ║
║   $ERR    errNum   EXECname    {xxx yyy zzz}    {..F=fff}                   ║
║                        ,                                                    ║
║           ?                                                                 ║
║           ?AUTHOR                                                           ║
║           ?FLOW                                                             ║
║           ?SAMPLES                                                          ║
║                                                                             ║
╚═════════════════════════════════════════════════════════════════════════════╝

───where:

?            shows this help file           (press  ESC  to quit when viewing).

?AUTHOR      shows the author of this program.

?FLOW        shows the external execution flow of this program.

?SAMPLES     shows some sample uses         (press  ESC  to quit when viewing).

errNum       is the number of the error message(s) (numbered from 1400──►1499),
             or  14.1  (which indicates that the REXX program has been HALTed).

                  0  exits $ERR.
                                                                     '
EXECname     is the name of the invoking EXEC,  and  if not a comma, it's shown
             in the last error message (see above).

xxx yyy zzz  are optional arguments that the error message text(s) may require.

fff          is an optional filename that the error message text(s) are written
             to.    Any commas (,) in the filename are translated to blanks
             before it's used.


The return code is set to the last two digits of the errNum,  except for error
1468,  which uses the return code  (error code)  from the 3rd argument  which
is an code code received from another command that failed.

Another special case is error  14.1  which signifies that the REXX program has
been   HALTed   and the return code is set to  14000.

                                      Ω

```


[[Category:REXX library routines]]
